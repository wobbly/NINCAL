...............................................................................
.MODUSERS - PROGRAM TO MODIFY 'NINUSERS' FILE                                 .
...............................................................................
.CREATED 13 JUNE 90
...................
PC       EQU       0                .fixed 24mar95 DLH
         INC       COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NUSEDD.INC
Release	Init	"1.3"	DLH 13March 2007  Pacfic Lists
.release  init      "1.2"          DLH 24Feb98 file structure change
.RELEASE  INIT      "1.1"
.
Temp	File
FUNCBR   FORM      1
.
.
         clock     date to today
         MOVE      "NUSE0001" TO PROGRAM
         MOVE      "PORT/USER MAINTENANCE" TO STITLE
         MOVE      "Names in the News Ca" TO COMPNME
         CALL      PAINT
         move      "Exit" to pf5
.         call      funcdisp
         trap      stop if f5
         MOVE      C1 TO NUSEPATH
         open 	Temp,"\\nts1\e\data\text\ninuser.dat.new"
         Loop
         Read	Temp,seq;Nusevars
         until 	over
         call	start
         repeat 
	goto stop         
         
.
START   
         MOVE      NUSENUM TO HOLDNUM
         MOVE      NUSENUM TO NUSEFLD
         MOVE      C1 TO FUNCBR
         CALL      NUSEtst
         GOTO      stop IF OVER
         DISPLAY   *P24:10,NUSENUM,"     IP: ",nusernid,"-000-000-",nusenum:
                   *P10:12,"NAME         : ",NUSEUSER:
                   *P10:14,"INITIALS     : ",NUSEINIT:
                   *P10:16,"IP Ring ID   :",NUSERNID:
                   *P10:18,"Lan Card Man :",NUSEADPT:
                   *P10:20,"Company Code :",NuseComp

KEY
UPD      CALL      NUSEUPD
	Return
	
WRITE    CALL      NUSEWRT
         GOTO      START
ADD      DISPLAY   *P24:10,NUSENUM:
                   *P10:12,"NAME        : ",NUSEUSER:
                   *P10:14,"INITIALS    : ",NUSEINIT:
                   *P10:16,"IP Ring ID  : ",NUSERNId:
                   *P10:18,"Type (HP)   : ",NUSEADpt:
                   *P10:20,"Company Code :",NuseComp
         MOVE      NUSEFLD TO NUSENUM
         KEYIN     *P24:10,*DV,NUSENUM:
                   *P24:23,*EF,"ADD THIS RECORD ? ",STR1
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
         MOVE      C2 TO FUNCBR
         GOTO      stop
delete
         KEYIN     *P24:10,*DV,NUSENUM:
                   *P24:23,*EF,"Delete THIS RECORD ? ",STR1
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
         call      nusedel
         goto      start
STOP     STOP
         INCLUDE   NUSEIO.INC
         INCLUDE   COMLOGIC.INC
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
.
START    KEYIN     *P10:10,*El,"PORT NUMBER : ",*JR,*ZF,NUSENUM
         SCAN      STAR IN NUSENUM
         GOTO      STOP IF EQUAL
         MOVE      NUSENUM TO HOLDNUM
         MOVE      NUSENUM TO NUSEFLD
         MOVE      C1 TO FUNCBR
         CALL      NUSEKEY
         GOTO      ADD IF OVER
         DISPLAY   *P24:10,NUSENUM,"     IP: ",nusernid,"-000-000-",nusenum:
                   *P10:12,"NAME         : ",NUSEUSER:
                   *P10:14,"INITIALS     : ",NUSEINIT:
                   *P10:16,"IP Ring ID   :",NUSERNID:
                   *P10:18,"Lan Card Man :",NUSEADPT:
                   *P10:20,"Company Code :",NuseComp

         KEYIN     *P24:23,*EL,"MODIFY THIS RECORD ? ",STR1
         match     "D" to str1
         goto      delete if equal
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
KEY
         KEYIN     *P24:10,*RV,*ZF,*JR,NUSENUM
         SCAN      STAR IN NUSENUM
         GOTO      START IF EQUAL
         MOVE      NUSENUM TO NUSEFLD
         TYPE      NUSENUM
         GOTO      KEY1 IF EQUAL
         DISPLAY   *P10:23,*EL,*HON,"MUST BE NUMERIC!!!!!!",*B:
                   *W,*B,*W,*B,*W,*HOFF,*P10:23,*EL
         MOVE      HOLDNUM TO NUSEFLD
         GOTO      KEY
KEY1     KEYIN     *P24:12,*JL,*RV,NUSEUSER
         SCAN      STAR IN NUSEUSER
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSEUSER
         GOTO      KEY IF EQUAL
KEY2     KEYIN     *P24:14,*RV,*JL,NUSEINIT
         SCAN      STAR IN NUSEINIT
         GOTO      START  IF EQUAL
         SCAN      "<" IN NUSEINIT
         GOTO      KEY1 IF EQUAL
KEY3     KEYIN     *P24:16,*RV,*JR,NUSERNID
         SCAN      STAR IN NUSERNID
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSERNID
         GOTO      KEY2 IF EQUAL
KEY4     display   *p35:18,"j2577a(EISA),J2573a(ISA),J2585A(pci),J2585B "
         KEYIN     *P24:18,*RV,*JR,NUSEadpt
         SCAN      STAR IN NUSEadpt
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSEadpt
         GOTO      KEY3 IF EQUAL
KEY5     KEYIN     *P24:20,*RV,*JR,NuseComp
         SCAN      STAR IN NuseComp
         GOTO      START IF EQUAL
         SCAN      "<" IN NuseComp
         GOTO      KEY4 IF EQUAL
KEYDONE  KEYIN     *P24:23,*EL,"EVERYTHING OK ? ",STR1
         CMATCH    "N" TO STR1
         GOTO      KEY IF EQUAL
         BRANCH    FUNCBR OF UPD,WRITE
         GOTO      START
UPD      CALL      NUSEUPD
         GOTO      START
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
         GOTO      KEY1
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
* ****************************************************************************
* TO MARK AN UNBILLED REC BILLED CHANGE SECOND BYTE OF NININV OR CMPINV FROM
* 0 TO B
* X TO Q
* TO MARK AN UNPAID PAID
* 0 TO P
* ****************************************************************************
* TO REINSTATE AN ORDER CHANGE SECOND BYTE OF NINORD OR CMPORD FROM
* X TO 0
* Q TO B
* *****************************************************************************
* *****************************************************************************
PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
RELEASE  INIT      "1.0"         DLH   dump record that was HOTPRINTED back into file for processing
          INclude   Norddd.inc
IFILE    IFILE     KEYLEN=6,VAR=566,UNCOMP
DATA     DIM       1
CODE     DIM       1
LR       DIM       6
INAME    DIM       8
CompID    DIm       1                mailer excluiive bytte
CompID2   Dim       1              LIst exclusive byte
OK       DIM       5
ORDPRINT ifile      keylen=6,fixed=696
ORDPRNTA afile      fixed=696
         TRAP      STOP IF F5
           MOVE      "Names In The News Ca Inc" TO COMPNME
           MOVE        "MARK" TO PROGRAM
           MOVE      "END" TO PF5
           CALL       PAINT
           CALL       FUNCDISP
.         DISPLAY    *P01:23,*ES
.         DISPLAY   *P01:23,*EL,"HIT F3 TO END"
COSMO
         KEYIN     *P01:01,*EL,*EOFF,"ENTER PASSWORD",OK
         MATCH     "COSMO",OK
         STOP      IF NOT EQUAL

         DISPLAY   *P1:24,"THIS PROGRAM UPDATES THE SECOND (STATUS) BYTE";
          open      ORDPRINT,"NINPRINT.isi|NINS1:502"
          move      "OPEN-ninprint.aam",Location
          open      ordprnta,"ninprint.aam|NINS1:502"
          MOVE      C1,NORDPATH

START    KEYIN     *P1:6,*EL,"ENTER LR TO BE MARKED: ",*ZF,*JR,LR
         CMATCH    " ",LR
         GOTO      START IF EOS
         MATCH     "00000*",LR
         GOTO      STOP IF EQUAL
          packkey   Nordfld,lr
          call      Nordkey
          goto      start if over

          read      ORDPRINT,nordFLD;;
          if over
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL          
.begin patch 3.79.2   OcompID Ocompid2
                    write     ORDPRINT,nordFLD;oRDVARS
                    insert    ORDPRNTA
                    TRAPCLR   IO
          else                    
                    Update    Ordprint;ordvars
          endif
         DISPLAY   *P1:24,"WRITTEN";
          GOTO      START                    











         READ      IFILE,LR;DATA,CODE,*tab=335,compid,compid2
         GOTO      NOHIT IF OVER
         DISPLAY   *P1:4,*EL,"PRESENT CODE IN THIS BYTE IS: ",CODE
         DISPLAY   *P1:6,*EL,"PRESENT CompID IS           : ",COmpId
         DISPLAY   *P1:8,*EL,"PRESENT COmpID2 IS          : ",COmpid2
         KEYIN     *P1:5,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CODE
         KEYIN     *P1:7,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID
         KEYIN     *P1:9,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID2
         FILEPI    1;IFILE
         UPDATAB   IFILE;DATA,CODE,*tab=335,compid,compid2
         GOTO      START
NOHIT    DISPLAY   *P1:24,"LR NUMBER NOT IN FILE !!!!",*W,*W,*W;
         GOTO      START
STOP     STOP
          INclude   NordIO.inc
           INCLUDE   COMLOGIC.inc


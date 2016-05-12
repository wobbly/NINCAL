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
RELEASE  INIT      "1.2"         DMB 18JUN2005 Changed IP address of File Manager
.RELEASE  INIT      "1.1"         DLH 30SEP92  CONVERT TO PCBUS.
IFILE    IFILE     KEYLEN=6,VAR=566,UNCOMP
DATA     DIM       1
CODE     DIM       1
LR       DIM       6
INAME    DIM       10
CompID    DIm       1                mailer excluiive bytte
CompID2   Dim       1              LIst exclusive byte
OK       DIM       5
         TRAP      STOP IF F5
           MOVE      "Names In The News" TO COMPNME
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
         KEYIN     *P1:4,*EF,"ENTER INPUT FILE: ",INAME
         DISPLAY   *P01:12,"TO MARK A UNBILLED ORDER BILLED CHANGE:":
                   *P04:13,"0 TO B   OR   X TO Q ":
                   *P01:14,"TO REINSTATE AN ORDER CHANGE:":
                   *P04:15,"X TO 0   OR   Q TO B ":
                   *P01:14,"TO Change Exclusivity of AN ORDER CHANGE:":
                   *P04:15,"P TO ' '   OR  ' ' TO P "
                   
         DISPLAY   *P1:24,"THIS PROGRAM UPDATES THE SECOND (STATUS) BYTE";
         rep       lowup in iname
         if        (iname = "NINORD")
.Patch 1.2 Begin
.         OPEN      IFILE,"NINORD.ISI|20.20.30.103:502"
.         OPEN      IFILE,"NINORD.ISI|NINS1:502"
         OPEN      IFILE,"ninvoice.ISI|NINS1:502"
.Patch 1.2 End         
         else
          pack      str55 from Iname,"|NINS1:502"
         OPEN      IFILE,str55
         endif
START    KEYIN     *P1:6,*EL,"ENTER LR TO BE MARKED: ",*ZF,*JR,LR
         CMATCH    " ",LR
         GOTO      START IF EOS
         MATCH     "00000*",LR
         GOTO      STOP IF EQUAL
.         READ      IFILE,LR;DATA,CODE,*tab=335,compid,compid2
         READ      IFILE,LR;DATA,CODE
         GOTO      NOHIT IF OVER
         DISPLAY   *P1:4,*EL,"PRESENT CODE IN THIS BYTE IS: ",CODE
         DISPLAY   *P1:6,*EL,"PRESENT CompID IS           : ",COmpId
         DISPLAY   *P1:8,*EL,"PRESENT COmpID2 IS          : ",COmpid2
         KEYIN     *P1:5,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CODE
         KEYIN     *P1:7,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID
         KEYIN     *P1:9,*EL,"ENTER CODE TO BE UPDATED    : ",*RV,CompID2
         FILEPI    1;IFILE
         UPDATAB   IFILE;DATA,CODE

.         UPDATAB   IFILE;DATA,CODE,*tab=335,compid,compid2
         GOTO      START
NOHIT    DISPLAY   *P1:24,"LR NUMBER NOT IN FILE !!!!",*W,*W,*W;
         GOTO      START
STOP     STOP
           INCLUDE   COMLOGIC.inc


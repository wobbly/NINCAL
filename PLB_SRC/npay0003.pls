.Npay0003 - GLOBAL BILL TO FILE ADDRESS/NAME CHANGE.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NpayDD.inc
release  init      "1.1"        ASH 05OCT98 NINPAY File expansion
.RELEASE  INIT      "1.0"        JD10NOV95
. .............................................................................
NEWBILL   INIT     "The Polk Company"
NEWADD INIT        "P.O. Box 77000/Dept 771265"
NEWCITY  INIT      "Detroit"
NEWSTAT  INIT      "MI"
NEWZIP   INIT      "48277-1265"
PASSER   INIT      "Info Services"
.
.
DATE     DIM       8                   USED WITH CLOCK VERB.
.
. ..MISCELLANEOUS
.
ANS      DIM       1                   ANSWER
MODE     DIM       1                   INDICATES IF PASSWORD WAS VALID
LIN      FORM      2                   NUMBER USED TO BRANCH ON UPDATES
CHANGE   DIM       1                   IF A FIELD WAS MODIFIED IT HAS A 'C'
BILKEY   DIM       8                   HOLDS APPENDED MLR/CNT/BILCD FOR READ
BROKER   DIM       21                  HOLDS NIN BROKER COMPANY NAME
DMM      DIM       2                   DISPLAY
DDD      DIM       2                          OF
DYY      DIM       2                            REVISION DATE.
RED      FORM      4
UPD      FORM      4
.
.
.
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO TODAY
         XIF
         MOVE      C1 TO NpayPATH
START    MOVE      "Npay0003" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "GLOBAL pay-TO UPDATE" TO STITLE
         MOVE      "ABORT" TO PF1
         MOVE      "EXIT" TO PF5
         call       paint
         call       funcdisp
         TRAP      ABORT IF F1
         TRAP      EXIT IF F5
READ
         CALL      NpayKS
         GOTO      STOP IF OVER
         ADD       "1" TO RED
         DISPLAY   *ES,*P10:14,"NUMBER OF RECORDS READ = ",RED
         SCAN      "Polk" IN pcomp
         CALL      UPDATE IF EQUAL
         GOTO      READ
UPDATE
         MOVE      NEWBILL TO PCOMP
         MOVE      NEWADD TO pstreet
         MOVE      NEWCITY TO pCITY
         MOVE      NEWSTAT TO pSTATE
         MOVE      NEWZIP TO pZIP
         MOVE      PASSER TO ppass
.Start Patch #1.1 - field expanded with Y2K
.         PACK      pdate FROM MM,DD,YY
         PACK      pdate FROM CC,YY,MM,DD
.End Patch #1.1 - field expanded with Y2K
         PACK      NpayFLD FROM powner,paynum
         CALL      NpayUPD
         ADD       "1" TO UPD
         DISPLAY   *P10:15,"NUMBER OF payTO'S UPDATED = ",UPD
         RETURN
ABORT
.
         TRAPCLR   F1
         NORETURN
         TRAP      ABORT IF F1
         DISPLAY   *P1:24,*EL,"NOT ALL RECORDS UPDATED",*B,*W3;

EXIT

STOP
         STOP
.
         INCLUDE   NpayIO.inc
         INCLUDE   COMLOGIC.inc


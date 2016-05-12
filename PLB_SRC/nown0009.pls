.Nown????? - GLOBAL BILL TO FILE ADDRESS/NAME CHANGE.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NownDD.inc
RELEASE  INIT      "1.0"        JD03jul99
. .............................................................................
NEWBILL   INIT     "Craver, Mathews & Smith"
NEWADD INIT        "4121 Wilson Blvd"
NEWCITY  INIT      "Arlington"
NEWSTAT  INIT      "VA"
NEWZIP   INIT      "22203"
NEWphon  init      "7032580000"
newfax   init      "7032580001"
PASSER   INIT      "Info Srvs"
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
         MOVE      C1 TO NownPATH
START    MOVE      "Nown????" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "GLOBAL OWNER UPDATE" TO STITLE
         MOVE      "ABORT" TO PF1
         MOVE      "EXIT" TO PF5
         call       paint
         call       funcdisp
         TRAP      ABORT IF F1
         TRAP      EXIT IF F5
READ
         CALL      Nownks
         GOTO      STOP IF OVER
         ADD       "1" TO RED
         DISPLAY   *ES,*P10:14,"NUMBER OF RECORDS READ = ",RED
         SCAN      "Craver, Mathews" IN ownocpy
         CALL      UPDATE IF EQUAL
         GOTO      READ
UPDATE
.         MOVE      NEWBILL TO PCOMP
         MOVE      NEWADD TO ownlosa
         MOVE      NEWCITY TO ownlocty
         MOVE      NEWSTAT TO ownlos
         MOVE      NEWZIP TO ownlozc
         move      newphon to owntele
         move      newfax to ownfax
         MOVE      PASSER TO ownpass
.Start Patch #1.1 - field expanded with Y2K
.         PACK      pdate FROM MM,DD,YY
         PACK      ownrdte FROM mm,dd,cc,yy
.End Patch #1.1 - field expanded with Y2K
         PACK      NownflD FROM ownlon
         CALL      Nownupd
         ADD       "1" TO UPD
         DISPLAY   *P10:15,"NUMBER OF OWNER's UPDATED = ",UPD
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
         INCLUDE   NownIO.inc
         INCLUDE   COMLOGIC.inc


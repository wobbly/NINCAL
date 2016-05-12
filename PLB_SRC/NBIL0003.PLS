.NBIL0003 - GLOBAL BILL TO FILE ADDRESS/NAME CHANGE.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NBILDD.inc
RELEASE  INIT      "1.2"       DLH   20MAR92
. .............................................................................
NEWBILL   INIT     "OMMUNICATIONS"
NEWADD INIT        "4001 TAMIAMI TR N. ## 370"
NEWCITY  INIT      "NAPLES"
NEWSTAT  INIT      "FL"
NEWZIP   INIT      "33940"
PASSER   INIT      "INFO SERVICES"
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
         MOVE      C1 TO NBILPATH
START    MOVE      "NBIL0003" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "GLOBAL BILL-TO UPDATE" TO STITLE
         MOVE      "ABORT" TO PF1
         MOVE      "EXIT" TO PF5
         call       paint
         call       funcdisp
         TRAP      ABORT IF F1
         TRAP      EXIT IF F5
READ
         CALL      NBILKS
         GOTO      STOP IF OVER
         ADD       "1" TO RED
         DISPLAY   *ES,*P10:14,"NUMBER OF RECORDS READ = ",RED
.         SCAN      "ENERTEX" IN NAME
.         SCAN      "WHITNEY" IN BILNAME
.         SCAN      "MARGOT STEIN" IN BILNAME 
.         SCAN      "ANACAPA" IN BILNAME 
.         SCAN      "GRANGER" IN BILNAME 
.         CALL      UPDATE IF EQUAL
.         SCAN      "ED BURNETT" IN BILNAME 
.         SCAN      "MPG LIST" IN BILNAME 
.         SCAN      "OLD DOMINION" IN BILNAME 
.         SCAN      "NAMES UNLIMITED" IN BILNAME 
.         SCAN      "NAME-FINDERS" IN BILNAME 
.         SCAN      "NAME FINDERS" IN BILNAME 
.         SCAN      "BEEKMAN" IN BILNAME 
.         SCAN      "COPLON" IN BILNAME 
.         SCAN      "LISTCO" IN BILNAME
.         SCAN      "GOLD COMM" IN BILNAME
.         SCAN      "GREENFIELD" IN BILNAME
.         SCAN      "BEV GREENFIELD" IN BILNAME
.         SCAN      "KROLL" IN BILNAME
.         SCAN      "MARKETRY,INC." IN BILNAME
.         SCAN      "ALAN DREY" IN BILNAME
         SCAN      "DIRECT MEDIA/FL" IN BILNAME
         CALL      UPDATE IF EQUAL
         GOTO      READ
UPDATE
.         MOVE      NEWBILL TO BILNAME
         MOVE      NEWADD TO BILADDR
         MOVE      NEWCITY TO BILCITY
         MOVE      NEWSTAT TO BILSTATE
         MOVE      NEWZIP TO BILZIP
         MOVE      PASSER TO BILUSER
         PACK      BILRVDTE FROM MM,DD,YY
         PACK      NBILFLD FROM BILMLR,BILCNT,BILCODE
         CALL      NBILUPD
         ADD       "1" TO UPD
         DISPLAY   *P10:15,"NUMBER OF BILTO'S UPDATED = ",UPD
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
         INCLUDE   NBILIO.inc
         INCLUDE   COMLOGIC.inc


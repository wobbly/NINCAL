PC	 EQU	    0
         INC       COMMON.inc
         INCLUDE   CONS.inc
RELEASE  INIT      "1.2"       DLH 18MAR92
CINVNO  FORM  6
DINVNO  FORM  6
NINVNO  FORM  6
ZERO     FORM      "0"
LASTLR FORM       6
NEWLR    FORM      6
CHKLR    DIM       6
LASTLCR FORM       6
NEWLCR   FORM      6
CHKLCR   DIM       6
CMPLR    FORM      6
NINLR    FORM      6
DATALR   FORM      6
FILLER   DIM       12
NINLCR   FORM      6
CMSLCR   FORM      6
DIMLR    DIM       6
DIMLCR   DIM       6
ANS      DIM       1
NUM      FORM      1
LRNUM    FILE  FIXED=42
         MOVE      "NORD0018" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "LR/LCR## MODIFICATION PROGRAM" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
         TRAP      EXIT IF F5
         OPEN      LRNUM,"SAVELR"
         FILEPI    1;LRNUM
         READTAB   LRNUM,ZERO;*7,NINLR,DATALR,FILLER,NINLCR,CMSLCR
         GOTO      EOJ IF OVER
START    DISPLAY   *P1:1,*ES,"USE GNXT0001",*B,*B
	 PAUSE     "5"
	 STOP
	 DISPLAY   *P15:10,*EF,"(1) CHANGE LR ##"
         DISPLAY   *P15:11,"(2) CHANGE LCR ##"
         DISPLAY   *P15:12,"(3) END"
KEYIN    KEYIN     *P15:14,"ENTER 1 OR 2 OR 3   ",NUM
         GOTO      KEYIN IF EOS
         COMPARE   "3",NUM
         GOTO      EOJ IF EQUAL
         COMPARE   "2",NUM
         GOTO      KEYLCR IF EQUAL
         MOVE      NINLR TO LASTLR
         DISPLAY   *P15:10,*EF,"CURRENT LR ## IS     = ",LASTLR
KEYLR   KEYIN     *P15:14,*EL,*ZF,*JR,"ENTER NEW LR ## ",NEWLR:
                   *P50:14,"OK ? ",ANS
         MOVE      NEWLR TO CHKLR
         GOTO      KEYLR IF EOS
         TYPE      CHKLR
         GOTO      KEYLR IF NOT EQUAL
         CMATCH    "Y" TO ANS
         GOTO      WRITLR IF EQUAL
         GOTO      KEYLR
WRITLR    MOVE      NEWLR TO DIMLR
         REP       " 0" IN DIMLR
         MOVE      DIMLR TO NINLR
         FILEPI    1;LRNUM
         WRITAB   LRNUM,ZERO;*7,NINLR
         GOTO      START
.
.
KEYLCR   MOVE      NINLCR,LASTLCR
         DISPLAY   *P15:10,*EF,"CURRENT LCR  ## IS     = ",LASTLCR
        KEYIN     *P15:14,*EL,*ZF,*JR,"ENTER NEW LCR ## ",NEWLCR:
                   *P50:14,"OK ? ",ANS
         MOVE      NEWLCR TO CHKLCR
         GOTO      KEYLCR IF EOS
         TYPE      CHKLCR
         GOTO      KEYLCR IF NOT EQUAL
         CMATCH    "Y" TO ANS
         GOTO      WRITLCR IF EQUAL
         GOTO      KEYLCR
WRITLCR    MOVE      NEWLCR TO DIMLCR
         REP       " 0" IN DIMLCR
         MOVE      DIMLCR,NINLCR
         FILEPI    1;LRNUM
         WRITAB   LRNUM,ZERO;*30,NINLCR
         GOTO      START
.
.
EOJ      DISPLAY   *P10:24,*HON,*B,"ALL DONE ! BYE ",*W4
         CLOSE     LRNUM
         STOP
.
EXIT     STOP
         INCLUDE   COMLOGIC.inc


* ***************************************************************************
* FIXADJ PROGRAM TO BLANK OUT THE ASTERIK IN BYTE 58
* *****************************************************************************
. FIXADJ  12/06/82
.
. INVOICE FILE...
PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
;begin patch 2.2
;         INCLUDE   NINVDD.inc
         INCLUDE   ninvdd.inc
;end patch 2.2
release init       "2.2"        DLH   2005March02  invoice Conversion
.release init       "2.1"        DLH   26Apr99  invoice y2k
.RELEASE INIT	   "2.0"        DLH   20MAR92    CONS, NINVXX,
.
PASSWORD DIM       5
. ...........
         MOVE      "NADJ0004" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "DELETE ADJUSTMENT BYTE FROM INV" TO STITLE
	 MOVE	   C1 TO NINVPATH
         CALL      PAINT
	 KEYIN     *P16:10,"Enter Password to Add, Hit Enter for Inquiry":
                   *EOFF,PASSWORD,*EON;
         MATCH     "COSMO",PASSWORD
         GOTO      WHAT2DO IF EQUAL
         STOP
WHAT2DO MOVE      " ",STR1
        KEYIN     *P31:4,"  (A)DD '*' TO INVOICE REC ":
                   *P31:5,"  (R)EMOVE '*'   ":
                   STR1;
         CMATCH    "*" TO STR1
         STOP      IF EQUAL
	REP       "A1R2" IN STR1
	MOVE	  C0 TO N1
	MOVE      STR1 TO N1
	BRANCH    N1 OF KEYLR,KEYLR,WHAT2DO
.
.  OBTAIN LR#
KEYLR   KEYIN     *P31:4," FIX ADJUSTMENTS",*P1:12,"PLEASE ENTER 'LR ##' ":
                  "OR '*' TO END.",*P56:12,*JR,*ZF,NINVFLD;
         SCAN      "*" IN NINVFLD
         STOP      IF EQUAL
        TYPE NINVFLD
        GOTO READINV IF EQUAL
        GOTO KEYLR IF EOS
         GOTO      KEYLR IF NOT EQUAL
.
*  READ INVOICE,"NININV",TO GET INVNO VALUE
READINV CALL      NINVKEY
        GOTO      BUSYINV IF NOT OVER
        BEEP
        DISPLAY   *P1:24,*EL,"INVOICE RECORD OF 'LR ##' ",NINVFLD:
                  " NOT FOUND",*W;
        GOTO      KEYLR
BUSYINV  CMATCH    "*",CODE
         GOTO      RECBUSY IF EQUAL
	 BRANCH    N1 OF BUSY,REMOVE
	 DISPLAY   *P1:24,*EL,"YOU FORGOT TO TELL ME WHAT 2 DO ",*B;
	 GOTO      WHAT2DO
BUSY	 MOVE      "*" TO ADJC
	 GOTO      UPD
REMOVE   MOVE	   B1 TO ADJC
UPD	 CALL      NINVUPD
         TRAPCLR   IO
MORINPT KEYIN     *P1:24,*EL,"MORE  INPUT ? ",STR1;
        CMATCH    YES,STR1
        GOTO      WHAT2DO IF EQUAL
        CMATCH    NO,STR1
        GOTO      MORINPT IF NOT EQUAL
.
        STOP
. ....
RECBUSY BEEP
        DISPLAY   *P1:24,*EL,"THE REQUESTED RECORD IS IN USE. PL":
                  "EASE TRY LATER",*W;
        GOTO      WHAT2DO
;begin patch 2.2
;	 INCLUDE   NINVIO.inc
	 INCLUDE   ninvio.inc
;end patch 2.2
	 INCLUDE   COMLOGIC.inc


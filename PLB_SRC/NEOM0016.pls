PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.START PATCH 1.1 REPLACED LOGIC
.         INCLUDE   NbrkDD.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.1 REPLACED LOGIC

RELEASE  INIT      "1.1"       27MAY2004  ASH	MAILER CONVERSION
.RELEASE  INIT      "1.0"       DLH 19oact93
.
.
. ..................
.
.
. PROGRAM VARIABLES
. .................
READNUM  FORM      5       NUMBER OF RECORDS READ.
UPDNUM   FORM      5       NUMBER OF RECORD UPDATED.
DATE     DIM       8
.
.
         TRAP      EXIT IF F5
	 MOVE      "ABORT " TO PF5
         MOVE      "NEOM0016" TO PROGRAM
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "CLEAR Brk/Consultant CREDIT" TO STITLE
         MOVE      "99/99/99" TO TODAY
         CLOCK     DATE TO DATE
	 IFNZ	   PC
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
	 XIF
	 IFZ	   PC
	 MOVE      DATE TO TODAY
	 XIF
	 CALL	   PAINT
	 CALL      FUNCDISP
         DISPLAY   *P15:12,"NUMBER OF Brokers READ    : ":
                   *P15:14,"NUMBER OF Brokers UPDATED : ";
.
...............................................................................
READbrk  MOVE      C3 TO NbrkPATH
         CALL      NbrkSEQ
         GOTO      DONE IF OVER
         ADD       C1 TO READNUM
         DISPLAY   *P44:12,READNUM;
         CMATCH    "*" TO brcredit
         GOTO      READbrk IF NOT EQUAL
         GOTO      READbrk IF EOS
. ABOVE CHECK SHOULD DO IT BUT LETS BE SURE
         CMATCH    " " TO brcredit
         GOTO      READbrk IF EQUAL
         CMATCH    "N" TO brcredit
         GOTO      READbrk IF EQUAL          *NEW NO CREDIT
         CMATCH    "I" TO brcredit
         GOTO      READbrk IF EQUAL          *INACTIVE  NO CREDIT.
         CMATCH    "B" IN brcredit
         GOTO      READbrk IF EQUAL          *BAD CREDIT.
         PACK      nbrkfld FROM brknum,brkcnt
         REP       ZFILL,nbrkfld
.
UPDATE   ADD       C1 TO UPDNUM
         DISPLAY   *P44:14,UPDNUM;
.
         MOVE      C1 TO NbrkPATH
         CALL      NbrkKEY
         GOTO      READbrk IF OVER
         MOVE      " " TO brcredit
         CALL      NbrkUPD
         GOTO      READbrk
DONE
         DISPLAY   *P1:24,"JOB DONE / RETURNING TO CHAIN",*W5;
         STOP
EXIT
         TRAPCLR   F5
         STOP
.START PATCH 1.1 REPLACED LOGIC
.         INCLUDE   NbrkIO.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.1 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc

.CREDITRESET -  CREATED 22 September 2003 .
.
.PURPOSE - AUTOMATICALLY TURN OFF CREDIT FOR COMPANY AFTER
.          THEIR CREDIT WAS TEMP. CLEARED.
.
PC       EQU       0
	INCLUDE	COMMON.inc
	INCLUDE	CONS.inc
;patch1.01
	INCLUDE	COMPDD.INC
	include  cntdd.inc

.	INCLUDE	NBRKDD.INC
;patch1.01
RELEASE  INIT      "1.01"  01JUN2004   DMB Added Code for Mailer Conversion
.RELEASE  INIT      "1.00"	22SEP2003	DMB CHANGED FILE AND CODE TO REFLECT USE IN NEW COMPANY FILE    

CREDIT   IFILE     KEYLEN=6

CSTAT    DIM       1
.  
         MOVE      "COMP0001" TO PROGRAM
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "Company Credit Reset" TO STITLE
         CALL      PAINT
         IFZ       PC
         OPEN      CREDIT,"COMPCREDIT"
         XIF
.
READ     READ      CREDIT,SEQ;COMPFLD,CSTAT
         GOTO      EOJ IF OVER
         ADD       C1 TO N5
         DISPLAY   *P10:12,"RECORDS PROCESSED: ",N5
.
         CALL      COMPKEY
.  
         MOVE      CSTAT TO COMPCREDIT
         CALL      COMPUPD
         GOTO      READ
EOJ      CLOSE     CREDIT,DELETE
         display   *p1:24,*el,"DONE",*b,*w5;
         shutdown  "cls"
         STOP
;patch1.01
			include	compio.inc
         include  cntio.inc

.         INCLUDE   NBRKIO.INC
;patch1.01
         INCLUDE   COMLOGIC.inc


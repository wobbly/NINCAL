PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.START PATCH 1.3 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.3 REPLACED LOGIC
         INCLUDE   NORDDD.inc
         include   ndatdd.inc

release  init      "1.3"        ASH 27MAY2004  MAILER CONVERSION
.release  init      "1.2"        ASH 01OCT2000 NEW SERVER ADDED
.release  init      "1.1"        ASH 05JAN99 NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"
.
.Start patch #1.1 - increased file size
.File size was never increase when Broker & Mailer files were converted and so
.following increase reflects +40 bytes for Broker/Mailer expansion and
.+114 bytes for NINORD expansion
.INPUT1    FILE     VAR=312
.INPUT2   IFILE     VAR=312,KEYLEN=6
INPUT1    FILE     VAR=498
INPUT2   IFILE     VAR=498,KEYLEN=6          .
.end Patch #1.1 - increased file size
RECMST   IFILE     VAR=3002,KEYLEN=6,COMP
.
ORDCT    FORM      5
LKEY     DIM       6
NAMES    FORM      10
.START PATCH 1.2 REPLACED LOGIC
.RECNAME  INIT       "g:\data\datadupe"             FIELD USED IN CREATION OF OUTPUT FILE.
RECNAME  DIM        25
         PACK       RECNAME,NTWKPATH1,"DATADUPE"            FIELD USED IN CREATION OF OUTPUT FILE.
.END PATCH 1.2 REPLACED LOGIC
ANS      DIM       1
FILENUM  FORM      2              NUMERIC WORK FIELD USED IN CREATION OF OUTPUT
FILE1    DIM       9
FILE2    DIM       9
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
ZERO     FORM      "0"
ONE      FORM      "1"
EIGHT    FORM      "8"
HOLDM    INIT      "0001"
OQTY1    FORM      7
DIM3     INIT      "000"
SIX      FORM      "6"
BLNK3    DIM       3
COUNT    FORM      5
USED     FORM      5
NOTUSED  FORM      5
. 
         TRAP      EOJ IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NDAT0025" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "DUPLICATED LISTS USAGE",STITLE
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NMLRPATH
. OPEN INPUT FILES
FILE1    KEYIN     *P10:10,"FILE1 : ",FILE1,*P70:10,"OK? ",ANS,*P70:10,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE1 IF NOT EQUAL
         GOTO      FILE1 IF EOS
         TRAP      FILE1NG IF IO
         OPEN      INPUT1,FILE1,READ
.
FILE2    KEYIN     *P10:12,"FILE2 : ",FILE2,*P70:12,"OK? ",ANS,*P70:12,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE2 IF NOT EQUAL
         GOTO      FILE2 IF EOS
         TRAP      FILE2NG IF IO
         OPEN      INPUT2,FILE2,READ
         MOVE      "01" TO FILENUM
.
. 
. PREP OUTPUT FILE
. 
. 
         PREPARE  RECMST,RECNAME,recname,"6","3002"
         DISPLAY   *P1:22,*EL,"The output file name for this search is: ":
                   RECNAME,*P1:24,*EL;
READ
         READ      INPUT1,SEQ;ORDVARS
         GOTO      EOJ IF OVER
         ADD       C1 TO ORDCT
         DISPLAY   *P10:15,"ORDERS READ = ",ORDCT
         MOVE      OLNUM TO LKEY
         REP       ZFILL IN LKEY        
         READ      INPUT2,LKEY;;
         GOTO      READ IF OVER
         GOTO      READOUT
READOUT 
         READ      RECMST,LKEY;;
         GOTO      READ IF NOT OVER
WRITE
         MOVE      LKEY TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         WRITE     RECMST,NDATFLD;DATVARS
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF DUPED LISTS ",USED
         GOTO      READ
.
FILE1NG  BEEP
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:24,*EL,"NO SUCH FILE",*W2,*P1:24,*EL;
         GOTO      FILE1
FILE2NG  BEEP
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:24,*EL,"NO SUCH FILE",*W2,*P1:24,*EL;
         GOTO      FILE2
         INCLUDE   COMLOGIC.inc
.START PATCH 1.3 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.3 REPLACED LOGIC
         INCLUDE   NDATIO.inc
.
EOJ
.         FLUSH     RECMST
         CLOSE     RECMST
         CLOSE     INPUT1
         CLOSE     INPUT2
. 
EXIT     STOP


PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.START PATCH 1.3 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.3 REPLACED LOGIC
         INCLUDE   NORDDD.inc

release  init      "1.3" ASH 27MAY2004 MAILER CONVERSION
.release  init      "1.2" ASH 05JAN99 NINORD Y2K, File expansion
.release  init      "1.1" ASH 21Sep98 NINMLR Y2K file expansion
.RELEASE  INIT      "1.0"
.
.Start patch #1.2 - increased file size
.INPUT1    FILE     VAR=344
.INPUT2   IFILE     var=344,KEYLEN=4
INPUT1    FILE     VAR=498
INPUT2   IFILE     var=498,KEYLEN=4
.End patch #1.2 - increased file size
.Start Patch #1.1 - remmed and replaced line
.RECMST   IFILE     VAR=165,KEYLEN=4
RECMST   IFILE     VAR=207,KEYLEN=4
.Endtch #1.1 - remmed and replaced line
.
ORDCT    FORM      5
DKEY     DIM       4
KEY      DIM       4
NAMES    FORM      10
RECNAME  INIT      "MLRSACT"             FIELD USED IN CREATION OF OUTPUT FILE.
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
         MOVE      "NDAT0024" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "MAILERS USED/NOT USED FOR LIST'S",STITLE
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
         OPEN      INPUT2,FILE2
         MOVE      "01" TO FILENUM
.
. 
. PREP OUTPUT FILE
. 
. 
.Start Patch #1.1 - remmed and replaced line
.         PREPARE  RECMST,RECNAME,recname,"4","165"
         PREPARE  RECMST,RECNAME,recname,"4","207"
.End Patch #1.1 - remmed and replaced line
         DISPLAY   *P1:22,*EL,"The output file name for this search is: ":
                   RECNAME,*P1:24,*EL;
READ
         READ      INPUT1,SEQ;ORDVARS
         GOTO      EOJ IF OVER
         scan      ostat in cancodes
         goto      read if equal
         CMATCH    "p" TO OSTAT           .PENDING DON'T TAKE
         GOTO      READ IF EQUAL
         CMATCH    "l" TO OSTAT           .lcr DON'T TAKE
         GOTO      READ IF EQUAL
         CMATCH    "z" TO OSTAT           .PENDING DON'T TAKE
         GOTO      READ IF EQUAL
         CMATCH    "x" TO OSTAT           .lcr DON'T TAKE
         GOTO      READ IF EQUAL
         ADD       C1 TO ORDCT
         DISPLAY   *P10:15,"ORDERS READ = ",ORDCT
         MOVE      OMLRNUM TO DKEY
         READ      INPUT2,DKEY;;
         GOTO      READOUT IF OVER
         GOTO      READ
READOUT  PACK      KEY FROM DKEY
         REP       " 0" IN KEY
         READ      RECMST,KEY;;
         GOTO      READ IF NOT OVER
         PACK      MKEY FROM KEY,DIM3
         CALL      NMLRKEY
         GOTO      WRITE IF NOT OVER
         GOTO      READ
WRITE
         CMATCH    "I"  TO MSTAT
         GOTO      READ IF  EQUAL
         WRITE     RECMST,KEY;MCODE:
                                    MRCODE:     001-002   FILLER
                                    MNUM:     003-006   MAILER COMPANY NUMBER
                                    MCONTCT:      007-009   MAILER CONTACT NUMB
                                    MNAME:     010-034   CONTACT NAME
                                    MCOMP:     035-059   COMPANY NAME
                                    MADDR:     060-084   ADDRESS
                                    MCITY:     085-099   CITY
                                    MSTATE:    100-101   STATE
                                    MZIP:      102-111   ZIP CODE
                                    MCOPIES:       112-113   NUMBER OF CARBON C
                                    MCCTO:     114-129   CARBON COPY TO
                                    MPASS:     130-139   LAST USER TO MODIFY
                                    MSLSPER:   137-138   SALES PERSON
                                    MREVDATE:    140-145   REVISED DATE
                                    MSTAT:
                                    MTELE:
                                    MFAX
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF DE-DUPED CLIENTS ",USED
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
         DISPLAY   *P1:24,*EL,"NO SUCH FILE",file2,*W4,*P1:24,*EL;
         GOTO      FILE2
         INCLUDE   COMLOGIC.inc
.START PATCH 1.3 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.3 REPLACED LOGIC
.
EOJ
.         FLUSH     RECMST
         CLOSE     RECMST
         CLOSE     INPUT1
         CLOSE     INPUT2
. 
EXIT     STOP


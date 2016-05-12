PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
         INCLUDE   NORDDD.inc
RELEASE  INIT      "1.2"        ASH 29OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.1"        ASH 05JAN99 NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"
.
.Start patch #1.1 - increased file size
.File size was never increase when Broker & Mailer files were converted and so
.following increase reflects +40 bytes for Broker/Mailer expansion and
.+114 bytes for NINORD expansion
.INPUT1    FILE     VAR=344
.INPUT2   IFILE     VAR=344,KEYLEN=6
INPUT1    FILE     VAR=498
INPUT2   IFILE     VAR=498,KEYLEN=6
.End patch #1.1 - increased file size
RECMST   IFILE     VAR=3002,KEYLEN=6
.
ORDCT    FORM      5
DKEY     DIM       6
KEY      DIM       6
NAMES    FORM      10
RECNAME  DIM       20             FIELD USED IN CREATION OF OUTPUT FILE.
ANS      DIM       1
FILENUM  FORM      2              NUMERIC WORK FIELD USED IN CREATION OF OUTPUT
FILE1    DIM       9
FILE2    DIM       9
NEWNAME  DIM       25              SEE RECNAME
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
         MOVE      "NDAT0011" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "DATACARDS USED/NOT USED BY MLR'S",STITLE
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NDATPATH
. OPEN INPUT FILES
FILE1    KEYIN     *P10:10,"DISKIN #1 : ",FILE1,*P70:10,"OK? ",ANS,*P70:10,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE1 IF NOT EQUAL
         GOTO      FILE1 IF EOS
         TRAP      FILE1NG IF IO
         OPEN      INPUT1,FILE1,READ
.
FILE2    KEYIN     *P10:12,"SUPRESSING DISKIN : ",FILE2,*P70:12,"OK? ",ANS:
                   *P70:12,*EL;
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
FILENAME CLEAR     NEWNAME
         APPEND    "DATA2",NEWNAME
         MOVE      FILENUM,STR2
         REP       ZFILL,STR2
         APPEND    STR2,NEWNAME
         RESET     NEWNAME TO 7
         RESET     NEWNAME
         TRAP      GOODFILE GIVING ERROR IF IO
         OPEN      RECMST,NEWNAME
         CLOSE     RECMST
ADDFILE  ADD       "1" TO FILENUM
         GOTO      FILENAME
.
GOODFILE TRAPCLR   IO
         IFNZ       PC
         SCAN      "0030-0031" IN ERROR
.         DISPLAY    *P1:24,*EL,ERROR,*W4
         GOTO      ADDFILE IF EQUAL
         XIF
         IFZ       PC
         SCAN      "I03" IN ERROR
.         DISPLAY    *P1:24,*EL,ERROR,*W4
         GOTO      ADDFILE IF EQUAL
         XIF
         TRAP      EXIT IF F5
         CLEAR     RECNAME
         IFNZ      PC
         APPEND    NEWNAME,RECNAME
         APPEND    ":PRINT",RECNAME
         XIF
         IFZ       PC
.START PATCH 1.2 REPLACED LOGIC
.         APPEND    "g:\DATA\",RECNAME
         APPEND    NTWKPATH1,RECNAME
.END PATCH 1.2 REPLACED LOGIC
         APPEND    NEWNAME,RECNAME
         XIF
         RESET     RECNAME
         PREPARE  RECMST,RECNAME,recname,"6","3002"
         DISPLAY   *P1:22,*EL,"The output file name for this search is: ":
                   NEWNAME,*P1:24,*EL;
READ
         READ      INPUT1,SEQ;ORDVARS
         GOTO      EOJ IF OVER
         ADD       C1 TO ORDCT
         DISPLAY   *P10:15,"ORDERS READ = ",ORDCT
         MOVE      OLNUM TO DKEY
         READ      INPUT2,DKEY;;
         GOTO      READOUT IF OVER
         GOTO      READ
READOUT  PACK      KEY FROM DKEY
         REP       " 0" IN KEY
         READ      RECMST,KEY;;
         GOTO      READ IF NOT OVER
         MOVE      KEY TO NDATFLD
         CALL      NDATKEY
         GOTO      WRITE IF NOT OVER
         GOTO      READ
WRITE
         CMATCH    "W" TO STATUS
         GOTO      READ IF EQUAL
         CMATCH    "T" TO STATUS
         GOTO      READ IF EQUAL
         WRITE     RECMST,KEY;DATVARS
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF DE-DUPED LISTS ",USED
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
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc
.
EOJ
         IFNZ      PC
         FLUSH     RECMST
         XIF
         CLOSE     RECMST
         CLOSE     INPUT1
         CLOSE     INPUT2
. 
EXIT     STOP


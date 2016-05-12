PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc

RELEASE  INIT      "1.2"         DB  21Aug2001 Expanded Var Recname used for
.                                              creation of output file
.RELEASE  INIT      "1.1"        ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.0"
.
INPUT1    FILE     VAR=600
INPUT2   IFILE     VAR=600,KEYLEN=6
RECMST   IFILE     VAR=600,KEYLEN=6
.
ORDCT    FORM      5
DKEY     DIM       6
KEY      DIM       6
NAMES    FORM      10
.RECNAME  DIM       20             FIELD USED IN CREATION OF OUTPUT FILE.
RECNAME  DIM       25             FIELD USED IN CREATION OF OUTPUT FILE.
ANS      DIM       1
FILENUM  FORM      2              NUMERIC WORK FIELD USED IN CREATION OF OUTPUT
FILE1    DIM       9
FILE2    DIM       9
NEWNAME  DIM       30              SEE RECNAME
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
         MOVE      "NDAT0029" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "DATACARD SUPPRESSION PROGRAM",STITLE
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NDATPATH
. OPEN INPUT FILES
FILE1    KEYIN     *P10:10,"DATA #1 : ",FILE1,*P70:10,"OK? ",ANS,*P70:10,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE1 IF NOT EQUAL
         GOTO      FILE1 IF EOS
         TRAP      FILE1NG IF IO
         OPEN      INPUT1,FILE1
.
FILE2    KEYIN     *P10:12,"SUPRESSING DATA FILE : ",FILE2,*P70:12,"OK? ",ANS:
                   *P70:12,*EL;
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
         SCAN      "I * Y" IN ERROR
         GOTO      ADDFILE IF EQUAL
         reset     error
         SCAN      "I03" IN ERROR
         GOTO      ADDFILE IF EQUAL
         reset     error
         XIF
         TRAP      EXIT IF F5
         CLEAR     RECNAME
         IFNZ      PC
         APPEND    NEWNAME,RECNAME
         APPEND    ":PRINT",RECNAME
         XIF
         IFZ       PC
.START PATCH 1.1 REPLACED LOGIC
.         APPEND    "g:\DATA\",RECNAME
         APPEND    NTWKPATH1,RECNAME
.END PATCH 1.1 REPLACED LOGIC
         APPEND    NEWNAME,RECNAME
         XIF
         RESET     RECNAME
         PREPARE  RECMST,RECNAME,recname,"6","600"
         DISPLAY   *P1:22,*EL,"The output file name for this search is: ":
                   NEWNAME,*P1:24,*EL;
READ
         filepi    1;input1
         READ      INPUT1,SEQ;datvars
         GOTO      EOJ IF OVER
         ADD       C1 TO ORDCT
         DISPLAY   *P10:15,"DATACARDS READ = ",ORDCT
         MOVE      LSTNUM TO DKEY
         filepi    1;input2
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
         INCLUDE   COMLOGIC.inc
         INCLUDE   NDATIO.inc
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

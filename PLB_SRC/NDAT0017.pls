
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
RELEASE  INIT      "1.2"        ASH 29OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.1"
.
.FILES
. .......................................
INPUT    FILE
OUTPUT   IFILE      keylen=6,VAR=3002,COMP       output file (DATA??)
.
.
ANS      DIM       1
FILENAM2 DIM       12
COUNT    FORM      4
COUNT1   FORM      4
CHCK     FORM      1
NUM      FORM      1
.TASKNAME INIT      "DELETE DATA100/ISAM:PRINT"
DELETE   INIT      "DELETE "
ISAM     INIT      "/ISAM:PRINT"
PRINT    INIT      ":PRINT"
.START PATCH 1.2 REPLACED LOGIC
.DATA     INIT      "g:\DATA\"
.END PATCH 1.2 REPLACED LOGIC
ISI      INIT      "/ISI:DATA"
DEL      INIT      "DEL "
.START PATCH 1.2 REPLACED LOGIC
.TASKNME2 INIT      "DEL g:\DATA\DATA100.ISI"
TASKNME2 DIM       30
         PACK      TASKNME2,"DEL ",NTWKPATH1,"DATA100.ISI"
.END PATCH 1.2 REPLACED LOGIC
WORKNAME DIM       25
TIME     DIM       8
STARTIME DIM       8
.
START    TRAP      EXIT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NDAT0017" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "SUPERPIC" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NDATPATH       SET ACCESS TO ISAM KEY
         KEYIN     *P10:12,"DO YOU WANT WITHDRAWNS ?? ",ANS
         REPLACE   "Y1N2" IN ANS
         MOVE      ANS,NUM
         BRANCH    NUM OF FILENAME,FILENAME
         GOTO      START
FILENAME KEYIN     *P10:12,"INPUT FILENAME :",FILENAME,*EL,*P70:12,"OK?",ANS:
                   *P70:12,*EL;
         CMATCH    "Y" TO ANS
         GOTO      START IF NOT EQUAL
         KEYIN     *P10:14,"DEFAULT OUTPUT NAME ? ",ANS
         CMATCH    "N" TO ANS
         GOTO      CREATE IF EQUAL
         MOVE      "DATA100" TO FILENAM2
PREP     DISPLAY   *P10:14,"OUTPUT FILENAME IS **",FILENAM2,"/TEXT:PRINT";
         IFNZ      PC
         PACK      WORKNAME FROM FILENAM2,PRINT
         XIF
         IFZ      PC
.START PATCH 1.2 REPLACED LOGIC
.         PACK      WORKNAME FROM DATA,FILENAM2
         PACK      WORKNAME FROM NTWKPATH1,FILENAM2
.END PATCH 1.2 REPLACED LOGIC
         XIF
.         PREPARE   INPUT,WORKNAME
.         CLOSE     INPUT
         PREPARE   OUTPUT,WORKNAME,workname,"6","3002"
         CLOCK     TIME TO TIME
         MOVE      TIME TO STARTIME
         TRAP      FILE1 IF IO
         OPEN      INPUT,FILENAME
         TRAP      FILE2 IF IO
.
READ
         READ      INPUT,SEQ;LSTNUM
         GOTO      EOJ IF OVER
         ADD       "1" TO COUNT
         DISPLAY   *P10:16,"NUMBER OF LIST NUMBERS READ ",COUNT;
         REP       " 0" IN LSTNUM
         READ      OUTPUT,LSTNUM;;
         GOTO      WRITE IF OVER
         GOTO      READ
.
WRITE
         MOVE      LSTNUM TO NDATFLD
         rep       zfill in ndatfld
         CALL      NDATKEY
         GOTO      READ IF OVER
         BRANCH    NUM OF COUNT,CHCK
CHCK     CMATCH    "W",STATUS
         GOTO      READ IF EQUAL
         CMATCH    "T" TO STATUS
         GOTO      READ IF EQUAL
COUNT    ADD       "1" TO COUNT1
         DISPLAY   *P10:18,"NUMBER OF DATACARDS WRITTEN ",COUNT1;
         WRITE    OUTPUT,LSTNUM;DATVARS
         GOTO      READ
EXIT      DISPLAY   *B,*B,"JOB ABORTED!!!!!!!!",*W2;
         GOTO      EOJ
CREATE   KEYIN     *P10:14,*EL,"OUTPUT FILENAME ",FILENAM2
         CMATCH    " " TO FILENAM2
         GOTO      EXIT IF EQUAL
         GOTO      EXIT IF EOS
         IFNZ      PC
         PACK      TASKNAME FROM DELETE,FILENAM2,ISAM
         XIF
         IFZ      PC
         PACK      TASKNME2 FROM DEL,FILENAM2,ISI
         XIF
         GOTO      PREP
EOJ
         DISPLAY   *B,*P1:24,"JOB DONE";
         CLOSE     INPUT
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         CLOSE     OUTPUT,EOFSIZE
         CLOCK     TIME TO TIME
         DISPLAY   *P1:23,*EL,"STARTIME = ",STARTIME,"   FINISHED : ",TIME;
         KEYIN     *P1:24,*R,*R,*R,*R,*R,*P1:24,*EL:
                   "DELETE ",*DV,FILENAM2,"/ISAM ?":
                   "              *****   ",ANS
         CMATCH    "Y" TO ANS
         STOP      IF NOT EQUAL
         IFNZ      PC
         EXECUTE   TASKNAME
         XIF
         IFZ      PC
         EXECUTE   TASKNME2
         XIF
         STOP
FILE1    TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"INPUT FILE MISSING",*W2,*B;
         STOP
FILE2    TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"INPUT FILE MISSING",*W2,*B;
         STOP
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


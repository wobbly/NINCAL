. .............................................................................
. DATAMRGE - PROGRAM TO MERGE TWO DATACARD PICK OFF FILES WITHOUT DUPLICATION.
. 11/26/84
. .............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
RELEASE  INIT      "1.1"          ASH 29OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "PRE"          DLH 26FEB92 CONVERSION TO PCBUS
. FILES
. ......
         IFNZ      PC
INPUTA   FILE      VAR=3002,COMP  
INPUTB   FILE      VAR=3002,COMP 
OUTPUT   IFILE     KEYLEN=6,VAR=3002,COMP
         XIF
         IFZ       PC
INPUTA   FILE      VAR=3002,COMP    READ SEQUENCIALLY
INPUTB   FILE      VAR=3002,COMP    READ SEQUENCIALLY
OUTPUT   IFILE     KEYLEN=6,VAR=3002,COMP
         XIF
. NOTE ALSO CHANGE DRIVE SPEC'S TO ENV.
.
. VARIABLES.
ANS      DIM       1
FILE1    DIM       12
FILE2    DIM       12
FILE3    DIM       17
FILE     FORM      1
COUNT1   FORM      5
COUNT2   FORM      5
COUNT3   FORM      5
COUNT4   FORM      5
.
START    TRAP      EXIT IF F5
         MOVE      "ABORT" TO PF5
         MOVE      "NDAT0006" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "DATA MERGE" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NDATPATH       .SET ACCESS TO ISI BY LIST#.
         MOVE      C1 TO FILE
FILE1    KEYIN     *P10:10,"FILE1 : ",FILE1,*P70:10,"OK? ",ANS,*P70:10,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE1 IF NOT EQUAL
         GOTO      FILE1 IF EOS
         TRAP      FILE1NG IF IO
         OPEN      INPUTA,FILE1,READ
.
FILE2    KEYIN     *P10:12,"FILE2 : ",FILE2,*P70:12,"OK? ",ANS,*P70:12,*EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE2 IF NOT EQUAL
         GOTO      FILE2 IF EOS
         TRAP      FILE2NG IF IO
         OPEN      INPUTB,FILE2,READ
.
FILE3    KEYIN     *P10:14,"OUTPUT FILE : NAME    :ENV",*P24:14,FILE3
         KEYIN     *P24:14,*EL,*DV,FILE3,*P70:14,"OK? ",ANS,*P70:14:
                   *EL;
         CMATCH    "Y" TO ANS
         GOTO      FILE3 IF NOT EQUAL
         GOTO      FILE3 IF EOS
         SCAN      "SYS" IN FILE3
         GOTO      FILE3 IF EQUAL      *INVALID FILE NAME.
         SCAN      "." IN FILE3
         GOTO      FILE3 IF EQUAL      *EXT NOT ALLOWED FOR IFILE PREP.
         TRAP      FILE3NG IF IO
.         OPEN      OUTPUT,FILE3,EXCLUSIVE
         OPEN      OUTPUT,FILE3
         TRAPCLR   IO
.
.         GOTO      READa
         GOTO      READb
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
FILE3NG  BEEP
         TRAPCLR   IO
         NORETURN
         TRAP      FILE4NG IF IO
         DISPLAY   *P1:24,*EL,"NO SUCH FILE FOUND",*W2,*P1:24,*EL;
         DISPLAY   *P1:24,*EL,"I WILL BUILD FILE - ",FILE3,*B;
         CLOSE     OUTPUT
         pack      prtfile from pdrive,file3
         PREPARE   OUTPUT,prtfile,prtfile,"6","3002"
         GOTO      READA
FILE4NG
         BEEP
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,"FILE COULD NOT BE CREATED",*W2,*P1:24,*EL;
         DISPLAY   *P1:24,*EL,"I WILL BUILD FILE - DATAMRG",*B;
.START PATCH 1.1 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\data\DATAMRG","g:\data\DATAMRG","6","3002"
        PACK    STR35,NTWKPATH1,"DATAMRG"
        PACK    STR45,NTWKPATH1,"DATAMRG"
         PREPARE   OUTPUT,STR35,STR45,"6","3002"
.END PATCH 1.1 REPLACED LOGIC
.
. READA - READ FIRST INPUT FILE.
.
READA
.
         goto     readb
         READ     INPUTA,SEQ;DATVARS
         GOTO      READB IF OVER
         ADD       "1" TO COUNT1
         DISPLAY   *P40:10,"NUMBER OF READS : ",COUNT1;
         GOTO      WRITE
.
. READB - READ SECOND INPUT FILE
.
READB
.
         MOVE      "2" TO FILE
         READ     INPUTB,SEQ;DATVARS
         GOTO      EOJ IF OVER
         ADD       "1" TO COUNT2
         DISPLAY   *P40:12,"NUMBER OF READS : ",COUNT2;
         GOTO      WRITE
.
WRITE
         filepi    1;output
         READ      OUTPUT,LSTNUM;;
         GOTO      WRITE1 IF OVER
         move      lstnum to ndatfld
         rep       zfill in ndatfld
         move     mlstname to ndatfld3
         move      c1 to ndatpath
         call      ndatdel
         call      ndatwrt
         ADD       "1" TO COUNT4
         DISPLAY   *P40:14,"NUMBER OF update : ",COUNT4;
         BRANCH    FILE OF READA,READB
         STOP
WRITE1
         MOVE      LSTNUM TO NDATFLD
         move     mlstname to ndatfld3
.         CALL      NDATTST
.         CALL      NDATDEL
         CALL      NDATWRT
.
         ADD       "1" TO COUNT3
         DISPLAY   *P40:14,"NUMBER OF WRITES : ",COUNT3;
         BRANCH    FILE OF READA,READB
         STOP
.
EOJ      BEEP
         CLOSE     INPUTA
         CLOSE     INPUTB
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         CLOSE     OUTPUT,EOFSIZE
         DISPLAY   *P1:1,*HON,*ES,*P10:12,"JOB DONE!!!!",*W2;
         KEYIN     *P1:24,"TAP ENTER TO EXIT";
         STOP
EXIT     BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED!!!!!!",*B,*W;
         GOTO      EOJ
         inc       ndatio.inc
         INCLUDE   COMLOGIC.inc

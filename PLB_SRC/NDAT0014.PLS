PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc

RELEASE  INIT      "1.6"        DLH Use Data manager, remove locks on nindat reads
Reldate   Init      "29 April 2011"
.RELEASE  INIT      "1.5"        ASH 07APR2005 COMMPER CONVERSION
.RELEASE  INIT      "1.4"        ASH 02FEB2004 DATACARD CONVERSION
.RELEASE  INIT      "1.3"        ASH 290CT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.2"        JOSE D. 05MAR92
.RELEASE  INIT      "1.1"        D. HERRICK 02AUG91

OUTFILE  FILE      VAR=200
IN       FORM      5
OUT      FORM      5
ONE      FORM      "1"
TIME     DIM       8
ANS      DIM       1
NUM      FORM      1
WITH     FORM      1
EXSW     DIM       1
FIELD46  DIM       46
.
. I/O STATEMENTS
. ..............
.
         MOVE      "EXIT" TO PF5
         MOVE      "NDAT0014" TO PROGRAM
         MOVE      "COPYDATACARD" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         TRAP      DONE IF F5
         CALL      PAINT
         CALL      FUNCDISP
.
         move       c3,ndatlock         .no file ord record locking
         MOVE      C1 TO NDATPATH
         TRAP      DONE IF F5
         CLOCK     TIME TO TIME
         DISPLAY   *P1:2,"START TIME : ",TIME
         move      c2 to num
ENTER    KEYIN     *P10:15,"ENTER (1) FOR RH, (2) FOR STANDARD",*t120,*RV,NUM
         BRANCH    NUM OF PREP1,PREP2
         GOTO      ENTER
PREP1
         IFNZ      PC
         PREPARE   OUTFILE,"DATARH:PRINT"
         XIF
         IFZ       PC
.START PATCH 1.3 REPLACED LOGIC
.         PREPARE   OUTFILE,"g:\DATA\DATARH"
         PACK      TASKNAME,NTWKPATH1,"DATARH|NINS1:502"
         PREPARE   OUTFILE,TASKNAME
.END PATCH 1.3 REPLACED LOGIC
         XIF
         GOTO      QUES
PREP2
         IFNZ      PC
         PREPARE   OUTFILE,"DATAST:PRINT"
         XIF
         IFZ       PC
.START PATCH 1.3 REPLACED LOGIC
.         PREPARE   OUTFILE,"g:\DATA\DATAST"
         PACK      TASKNAME,NTWKPATH1,"DATAST|NINS1:502"
         PREPARE   OUTFILE,TASKNAME
.END PATCH 1.3 REPLACED LOGIC
         XIF
QUES     move      yes to ans
         KEYIN     *P10:12,*EL,"Do you want to exclude Withdrawn cards? ":
                   " OK? ",*t120,ANS;
         REP       "Y1N2" IN ANS
         MOVE      ANS TO WITH
         DISPLAY   *P1:3,*EF
.
READA
         CALL      NDATSEQ
         GOTO      DONE IF OVER
         ADD       ONE TO IN
         DISPLAY   *P20:12,"INCOUNT : ",IN
         BRANCH    WITH OF CHECK1,WRIT1
CHECK1   CMATCH    "W",STATUS
         GOTO      READA IF EQUAL
         CMATCH    "T" TO STATUS
         GOTO      READA IF EQUAL
WRIT1    ADD       ONE TO OUT
         DISPLAY   *P20:14,"OUTCOUNT: ",OUT
         BRANCH    NUM OF RHWRIT,STWRIT
STWRIT
.START PATCH 1.4 REPLACED LOGIC
.   WRITE     OUTFILE,SEQ;STATUS:
.                LSTNUM:
.                OWNNUM:
.                CATCDE1:
.                CATCDE2:
.                CATCDE3:
.                CATCDE4:
.                CATCDE5:
.                CATCDE6:
.                CATCDE7:
.                CATCDE8:
.                CATCDE9:
.                CATCDE10:
.                NLSTCDE:
.                ELSTCDE:
.                COMMPER:
.                HOTLINE:
.                NEWDATE:
.                REVDATE:
.                PASSWORD:
.                MLSTNAME
.START PATCH 1.5 REPLACED LOGIC
.   WRITE     OUTFILE,SEQ;STATUS:
.                LSTNUM:
.                OWNNUM:
.                NLSTCDE:
.                ELSTCDE:
.                COMMPER:
.                HOTLINE:
.                NEWDATE:
.                REVDATE:
.                PASSWORD:
.                MLSTNAME
          move      COMMPER,str3
   WRITE     OUTFILE,SEQ;STATUS:
                LSTNUM:
                OWNNUM:
                NLSTCDE:
                ELSTCDE:
                str3:
                HOTLINE:
                NEWDATE:
                REVDATE:
                PASSWORD:
                MLSTNAME
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
         GOTO      READA
RHWRIT
.START PATCH 1.4 REPLACED LOGIC
.         parse     textdata into text1 using " ~09",noskip,blankfill
.        MOVE      TEXT1 TO FIELD46
.         WRITE     OUTFILE,SEQ;STATUS:
.                LSTNUM:
.                OWNNUM:
.                CATCDE1:
.                CATCDE2:
.                CATCDE3:
.                CATCDE4:
.                CATCDE5:
.                CATCDE6:
.                CATCDE7:
.                CATCDE8:
.                CATCDE9:
.                CATCDE10:
.                NLSTCDE:
.                ELSTCDE:
.                COMMPER:
.                HOTLINE:
.                NEWDATE:
.                REVDATE:
.                PASSWORD:
.                MLSTNAME:
.                UNIVERSE:
.                FIELD46
.START PATCH 1.5 REPLACED LOGIC
.         WRITE     OUTFILE,SEQ;STATUS:
.                LSTNUM:
.                OWNNUM:
.                NLSTCDE:
.                ELSTCDE:
.                COMMPER:
.                HOTLINE:
.                NEWDATE:
.                REVDATE:
.                PASSWORD:
.                MLSTNAME:
.                UNIVERSE
          move      COMMPER,str3
         WRITE     OUTFILE,SEQ;STATUS:
                LSTNUM:
                OWNNUM:
                NLSTCDE:
                ELSTCDE:
                str3:
                HOTLINE:
                NEWDATE:
                REVDATE:
                PASSWORD:
                MLSTNAME:
                UNIVERSE
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
         GOTO      READA
.
DONE     WEOF      OUTFILE,SEQ
         IFNZ      PC
         FLUSH     OUTFILE
         XIF
         CLOSE     OUTFILE
         CLOCK     TIME TO TIME
         DISPLAY   *P1:3,"END TIME : ",TIME
         STOP
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


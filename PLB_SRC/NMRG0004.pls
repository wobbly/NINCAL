pC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.
RELEASE  INIT      "1.1"       JD  15FEB96   INCLUDES, PCBUS
TDMCmrg  IFILE     KEYLEN=6
INFILE   FILE      
mrgout   IFILE     KEYLEN=6
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
LR       DIM       6
.
         MOVE      "NMRG0004" TO PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
         MOVE      "COPY NINMRGE LR'S -> DAILYMRG" TO STITLE
         CALL      PAINT
         OPEN      INFILE,"NINMRGE|NINS1:502",SHARE
         OPEN      tdmcmrg,"NINMRGE|NINS1:502"
         OPEN      mrgOUT,"mrgout|NINS1:502"
.
READ     FILEPI    1;INFILE
         READ      INFILE,SEQ;LR,STR3
         GOTO      EOJ IF OVER
         ADD       ONE TO READCNT
         DISPLAY   *P10:12,"NUMBER OF MERGES  READ : ",READCNT
WRITE    FILEPI    3;mrgOUT
         read      mrgout,lr;;
         goto      del if not over
           WRITE     mrgOUT,LR;LR,STR3
del      MOVE      LR TO KEY
         REP       " 0" IN KEY
         FILEPI    2;TDMCmrg
         READ      TDMCmrg,KEY;;
         DELETE    TDMCmrg,KEY
         GOTO      READ
EOJ      WEOF      mrgOUT,SEQ
           IFNZ        PC
         FLUSH     mrgOUT
           XIF
         CLOSE     mrgOUT,EOFSIZE
         CLOSE     TDMCmrg
         CLOSE     INFILE
         STOP
         INCLUDE   COMLOGIC.inc


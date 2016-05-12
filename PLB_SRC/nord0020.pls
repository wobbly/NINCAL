pC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.
RELEASE  INIT      "1.5"        DMB     18JUN2005 FM IP CHG
.RELEASE  INIT      "1.4"        JD     08JAN2004 changed inputs to fixed and saving lr field.
;RELEASE  INIT      "1.3"        ASH    01FEB2002 ADDED TDMCORD.DAT TO FILE MANAGER
.RELEASE  INIT      "1.2"        JD    22SEP99   copy ?
.RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
TDMCORD  IFILE     KEYLEN=6,FIXED=7
INFILE   IFILE     keylen=6,FIXED=7
.INFILE   FILE      VAR=7
TDMCOUT  IFILE     KEYLEN=6,VAR=7
KEY      DIM       6
KEYSAVE  DIM       6
ONE      FORM      "1"
READCNT  FORM      4
LR       DIM       6
reject   file
.
         MOVE      "NORD0020" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "COPY TDMC LR'S -> TDMCORD1" TO STITLE
         CALL      PAINT
         move      yes to str1
         keyin     *P1:12,*blinkon,*red,"This will copy TDMC lr's for TC  OK???   ",*RV,*t3,STR1;
         cmatch    yes to str1
         stop      if not equal
.START PATCH 1.3 REPLACED LOGIC
.         OPEN      INFILE,"TDMCORD"
.         OPEN      TDMCORD,"TDMCORD"
.>Patch 1.5 Begin
.         OPEN      INFILE,"TDMCORD.isi|20.20.30.103:502"
         OPEN      INFILE,"TDMCORD.isi|NINS1:502"
.         OPEN      TDMCORD,"TDMCORD.isi|20.20.30.103:502"
         OPEN      TDMCORD,"TDMCORD.isi|NINS1:502"
.>Patch 1.5 End         
.END PATCH 1.3 REPLACED LOGIC
         OPEN      TDMCOUT,"TDMCORD1"
         open       reject,"tdmcreject"
.
READ     FILEPI    1;INFILE
         READ      INFILE,SEQ;LR,STR1
         GOTO      EOJ IF OVER
                              move      lr to keysave
         ADD       ONE TO READCNT
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
         cmatch    "p" to str1
         if         equal
         write      reject,seqeof;lr,str1
         beep
         goto      del
         endif
WRITE    FILEPI    3;TDMCOUT
         read      tdmcout,lr;;
         goto      del if not over
         WRITE     TDMCOUT,LR;LR,STR1
del      MOVE      keysave TO KEY
         REP       " 0" IN KEY
         FILEPI    2;TDMCORD
         READ      TDMCORD,KEY;;
         DELETE    TDMCORD,KEY
         GOTO      READ
EOJ
         IFNZ      PC
         FLUSH     TDMCOUT
         XIF
         CLOSE     TDMCOUT,EOFSIZE
         CLOSE     TDMCORD
         CLOSE     INFILE
         weof      reject,seqeof
         STOP
         INCLUDE   COMLOGIC.inc


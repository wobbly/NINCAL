pC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.
RELEASE  INIT      "1.0"       DLH   
TDMCORD  IFILE     KEYLEN=6,FIXED=7
KEY      DIM       6
KEYSAVE  DIM       6
ONE      FORM      "1"
READCNT  FORM      4
LR       DIM       6
.
         MOVE      "NORD0020A" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "add MIA TDMC LR'S -> TDMCORD" TO STITLE
         CALL      PAINT
         OPEN      TDMCORD,"TDMCORD.isi|NINS1:502"
.
Key
         keyin     *P1:12,*blinkon,*red,"Keyin TDMC lr's, * to exut   ",*ZF,*JR,STR6;
         Scan       "*",str6
         stop       if  equal
         reset     str6          

WRITE    FILEPI    1;TDMCORD
         WRITE     TDMCORD,STR6;STR6,"D"
         GOTO      Key
EOJ
         CLOSE     TDMCORD
         STOP
         INCLUDE   COMLOGIC.inc


pc       equ       0
         INCLUDE   COMMON.inc
         INCLUDE   ndatdd.inc
         include   ndat3dd.inc
         INCLUDE   nowndd.inc
         include   cons.inc
NUM      FORM      1

RELEASE  INIT      "1.3"       DLH run Butil locally
Reldate   Init      "2013 May 7"
.RELEASE  INIT      "1.2"       ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.1"       DLH 21MAR94 DO NOT SUPPRESS WITHDRAWNS OR NON TDMC.
.release  init      "1.0"
copy     dim       2
OUTFILE  FILE      VAR=3002
.
         DISPLAY   *P1:1,*ES
         move     c1 to ndatpath
.START PATCH 1.2 REPLACED LOGIC
.PREP     PREPARE    OUTFILE,"g:\data\datarc"
PREP     PACK       TASKNAME,NTWKPATH1,"datarc"
         PREPARE    OUTFILE,TASKNAME
.END PATCH 1.2 REPLACED LOGIC
         
INPUT    call      ndat3seq
         GOTO      EOJ IF OVER
         add       c1 to n6
         display   *p10:15,"records read = ",n6
check    unpack    ndat3key into ndatfld
         rep       zfill in ndatfld
         call      ndatkey
.         cmatch    "W" to status
.         goto      input if equal
.         MOVE      Ownnum TO nownfld
.         rep       zfill in nownfld
.         call     nownkey
.         SCAN      "TDMC" IN OWNCTN
.         GOTO      write if EQUAL
.         reset     ownctn
.         SCAN      "TRIPLEX" IN OWNCTN
.         GOTO      write IF EQUAL
.         GOTO      input
. 
         
WRITE   
         WRITE      OUTFILE,SEQ;datvars
         add       c1 to n5
         display   *p10:16,"records written = ",n5
         GOTO      INPUT
EOJ
         WEOF      OUTFILE,SEQ
         CLOSE     OUTFILE
RH       clear     taskname
         move      c1 to copy
.         IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.         append    "\\Nins1\Lanbatch\batch32 -X -N -P\\nins1\winbatch\BUTIL job=NCARDRH INfile=datarc",TASKNAME
.          else
         append    "!\\nins1\winbatch\BUTIL job=NCARDRH INfile=datarc",TASKNAME
.          endif
.        APPEND    SHORTNME TO TASKNAME
         APPEND    " F=default C=",TASKNAME
         APPEND    COPY,TASKNAME
         APPEND    " B=",TASKNAME
         APPEND    user TO TASKNAME
         RESET     TASKNAME
         EXECUTE   TASKNAME
         STOP
         include   ndatio.inc
         include   nownio.inc
         include   ndat3io.inc
         include   comlogic.inc


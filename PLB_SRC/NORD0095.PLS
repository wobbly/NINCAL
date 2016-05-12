PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NORDDD.inc
         inc       ninvdd.inc
         inc       ndatdd.inc
         INCLUDE   NSPEDD.INC
.
RELEASE  INIT      "1.2"       ASH   13JUL99   NINSPE FILE CONVERSION
.RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
TDMCORD  FILE      
JDout    file
INFILE   FILE      VAR=7
TDMCOUT  IFILE     KEYLEN=6,VAR=288
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
writupd  FORM      4
writCNT  FORM      4
spec1    form      1
LR       DIM       6
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
.START PATCH 1.2 - ADDED VARS
DESC0L1  DIM       47       7-53      LINE 1 OF 00 INSTRUCTION
DESC0L2  DIM       47      54-100     LINE 2 OF 00 INSTRUCTION
DESC991  DIM       47     101-147     LINE 1 OF 99 INSTRUCTION
DESC992  DIM       47     148-194     LINE 2 OF 99 INSTRUCTION
DESC981  DIM       47     195-241     LINE 1 OF 98 INSTRUCTION
DESC982  DIM       47     242-288     LINE 2 OF 98 INSTRUCTION
.END PATCH 1.2 - ADDED VARS
.
.
         MOVE      "NORD0095" TO PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
         MOVE      "Fix mlrs./inv's #'s" TO STITLE
         move      c1 to nordpath
         move      c1 to ninvpath
         move      c1 to ndatpath
         CALL      PAINT
         prepare   jdout,"c:\work\jdinv.dat"
         OPEN      TDMCORD,"c:\work\diskinjd.dat"
.
READ
MainLoop read      tdmcord,seq;ordvars
         goto      eoj if over
.         if        (Ostat <> "0" or OSTAT <> "B")
.;         CMATCH    "p" TO OSTAT       Pending order ?
.;         GOTO      read IF EQUAL     YES, skip.
.;         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.;         GOTO      read IF EQUAL     YES, skip.
.;         CMATCH    "l" TO OSTAT      lcr order ?
.;         GOTO      read IF EQUAL     YES, skip.
.;         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
.;         GOTO      read IF EQUAL     YES, skip.
         ADD       ONE TO READCNT
         DISPLAY   *P10:20,"NUMBER OF ORDERS READ : ",READCNT
               clear          taskname
               append         "!\\nins1\winbatch\butil job=HOTORD",TASKNAME
               append         " infile=",taskname
               append         olrn,taskname
               append         "M",taskname
               append         "LD",taskname
               append         " F=default C=1",TASKNAME
               append         " B=",TASKNAME
               append         "JDUENAS",TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
               append         C1,taskname
               reset          TASKNAME
               execute        TASKNAME
               pause     "4"

         GOTO      READ
.
eoj         CLOSE     TDMCORD
         shutdown   "cls"
         INCLUDE   COMLOGIC.inc
         include   ninvio.inc
	 include   ndatio.inc
        include    nordio.inc
         INCLUDE   NSPEIO.INC


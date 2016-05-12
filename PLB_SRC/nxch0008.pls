PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   norddd.inc
.START PATCH 2.6 ADDED LOGIC
	INCLUDE	NXNGDD.INC
	INCLUDE	NXCHDD.INC
.END PATCH 2.6 ADDED LOGIC
release  init      "2.7"         18JUN2005 DMB FM IP CHG
.release  init      "2.6"         12MAY2005 ASH NINXNUM/NINXCHNG CONVERSION
.release  init      "2.5"         11APR2001 ASH FILE ADDED TO FILE MANAGER
.release  init      "2.4"         03OCT2000 ASH NEW SERVER ADDED
.release  init      "2.3"         19OCT95 dlh remove update use delete add.
.RELEASE  INIT      "2.2"         05aug94 added order inc for olon move.
.RELEASE  INIT      "2.1"        03AUG93 TYPIST INITIALS TO 3 BYTES.
.
.RELEASE  INIT      "2.0"        DLH  23MAR92. CONS,COMLOGIC, NINVDD.
.START PATCH 2.6 REPLACED LOGIC
.LRXNG    IFILE     KEYLEN=6,FIXED=90
.XCHNG    IFILE     KEYLEN=13,FIXED=90
LRXNG    IFILE     KEYLEN=6,FIXED=200
XCHNG    IFILE     KEYLEN=17,FIXED=200
.END PATCH 2.6 REPLACED LOGIC
ninx     file
ninx2     file
ninx99   file
rEADCNT  FORM      6
writcnt  form      6
TYPIST   DIM       3
DUPCODE  DIM       1       USED IF DUPE RECORD
DUPSTATB DIM       1         "   "  "      "
lstnum   dim       6
SYSDAT   DIM       6                        SYSTEM DATE
.START PATCH 2.6 REMOVED LOGIC
.EXKEY    DIM       13           ISAM KEY MLR1/MLR2/ENTRY
.STAT     DIM       1            HOLDS (C) IF ORDER CANCELLED
.LR       DIM       6            LR NUMBER 14-19
.USAGE1   FORM      9            TOTAL USAGE TO DATE MLR1 33-41
.USAGE2   FORM      9            TOTAL USAGE TO DATE MLR2 42-50
.MLRSW    DIM       1            HOLDS (1) OR (2) FOR MAILER PLACING ORDER
.TYPE     DIM       2            59-60  YPISTS INITIALS
.END PATCH 2.6 REMOVED LOGIC
.COMMENT  DIM       30           61-90  COMMENTS.
ACNUM    DIM       6            USED AS LR NUMBER ON BEGINNING BALANCES
NACNUM   FORM      5            NUMERIC WORK
exqty    dim       7
.
.START PATCH 2.5 REPLACED LOGIC
.         OPEN      XCHNG,"NINXCHNG"
.         OPEN      LRXNG,"NINLRXNG"
.>patch 2.7
.         OPEN      XCHNG,"NINXCHNG.ISI|20.20.30.103:502"
         OPEN      XCHNG,"NINXCHNG.ISI|NINS1:502"
.         OPEN      LRXNG,"NINLRXNG.ISI|20.20.30.103:502"
         OPEN      LRXNG,"NINLRXNG.ISI|NINS1:502"
.>patch 2.7         
.END PATCH 2.5 REPLACED LOGIC
.START PATCH 2.4 REPLACED LOGIC
.         open      NINX,"g:\data\badxchg.dat"
         PACK      STR35,NTWKPATH1,"BADXCHG.DAT"
         open      NINX,STR35
.END PATCH 2.4 REPLACED LOGIC
.         open      NINX2,"g:\data\xchng2.dat"
.         open      ninx99,"g:\data\xchng99.dat"
         MOVE      "NINx0099" TO PROGRAM
         MOVE      "Names in the News Ca" TO COMPNME
         move      c1 to nordpath
         CALL      PAINT
.
READ     FILEPI    1;NINX
.START PATCH 2.6 REPLACED LOGIC
.         READ      LRXNG,SEQ;EXKEY,lr,USAGE1,USAGE2:
.                   EXQTY,LSTNUM,SYSDAT,STAT,MLRSW,TYPE,COMMENT
         READ      LRXNG,SEQ;EXKEY,LR2,lr,USAGE1,USAGE2:
                   QTYFILL,EXQTY,LSTNUM,SYSDAT,STAT,MLRSW,TYPE,COMMENT
.END PATCH 2.6 REPLACED LOGIC
         GOTO      EOJ IF OVER
         ADD       C1 TO READCNT
         DISPLAY   *P10:12,"NUMBER OF X's read : ",READCNT
         cmatch    "R" to stat
         goto      read if equal
         rep       zfill in exqty
         unpack    sysdat into mm,dd,yy
         match     "96" to yy
         goto      read if not equal
         move      lr to nordfld
         rep       zfill in nordfld
         call      nordkey
         goto      read if over
         rep       zfill in oexqty
         match     "0000000" to oexqty
         if        equal
         match     oqty to exqty
         else
         match     oexqty to exqty
         endif
         goto      read if equal
.         WRITE     ninx,seq;EXKEY,lr,USAGE1,USAGE2:
.                   EXQTY,LSTNUM,SYSDAT,STAT,MLRSW,TYPE,COMMENT
         WRITE     ninx,seq;ordvars
         ADD       C1 TO writCNT
         DISPLAY   *P10:13,"NUMBER OF X's WRITTEN : ",writCNT
         goto       read
EOJ
         CLOSE     XCHNG
         CLOSE     LRXNG
         STOP
.START PATCH 2.6 REPLACED LOGIC
	INCLUDE	NXNGIO.INC
	INCLUDE	NXCHIO.INC
.END PATCH 2.6 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc
         include   nordio.inc
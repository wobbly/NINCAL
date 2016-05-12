PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
.START PATCH 1.31 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.31 REPLACED LOGIC
         INCLUDE   nord2dd.inc
         XIF

RELEASE  INIT      "1.31"  ASH 27MAY2004  MAILER CONVERSION
.RELEASE  INIT      "1.3"  ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.2"  ASH 15JAN99 NINORD Y2K, File expansion
.RELEASE  INIT      "1.1"  ASH 21Sep98 NINMLR Y2K File expansion
.RELEASE  INIT      "1.0"
.

.Start Patch #1.2 - increased file size
.DISKIN   IFILE     VAR=328,KEYLEN=4
DISKIN   IFILE     VAR=498,KEYLEN=4
.end Patch #1.2 - increased file size
.Start Patch #1.1 - remmed and replaced line
.OUTPUT   IFILE     FIXED=165,KEYLEN=7
OUTPUT   IFILE     FIXED=207,KEYLEN=7
.End Patch #1.1 - remmed and replaced line
.
ORDCT    FORM      5
DKEY     DIM       4
KEY      DIM       6
NAMES    FORM      10
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
         MOVE      "Abort" TO PF5
         MOVE      "ndat0033" TO PROGRAM
         MOVE      "Names in the News CA" TO COMPNME
         MOVE      "ACTIVE CLIENTS NOT USED A LIST" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
. OPEN INPUT FILES
. 
         OPEN      DISKIN,"DISKIN99",READ
. 
. PREP OUTPUT FILE
. 
.Start Patch #1.1 - remmed and replaced line
.         PREPARE      OUTPUT,"g:\data\MLRS","g:\data\MLRS","7","165"
.START PATCH 1.3 REPLACED LOGIC
.         PREPARE      OUTPUT,"g:\data\MLRS","g:\data\MLRS","7","207"
         PACK         STR35,NTWKPATH1,"MLRS"
         PACK         STR45,NTWKPATH1,"MLRS"
         PREPARE      OUTPUT,STR35,STR45,"7","207"
.END PATCH 1.3 REPLACED LOGIC
.End Patch #1.1 - remmed and replaced line
.
         move         c1 to nmlrpath          
. ORDER READ
. FIRST LR OF 1/1/91
...............................................................................
MAIN     call      nmlrks
         GOTO      EOJ IF OVER
         cmatch    "I" to mstat
         goto      main if equal
         ADD       C1 TO ORDCT
         DISPLAY   *P10:15,"ORDERS READ = ",ORDCT
         MOVE      MNUM TO DKEY
         READ      DISKIN,DKEY;;
         GOTO      READOUT IF OVER
         GOTO      main
READOUT  PACK      MKEY FROM DKEY,DIM3
         REP       " 0" IN MKEY
         READ      OUTPUT,MKEY;;
         GOTO      main IF NOT OVER
WRITE
         WRITE     OUTPUT,MKEY;MCODE:
                   MRCODE:
                   MNUM:
                   MCONTCT:
                   MNAME:
                   MCOMP:
                   MADDR:
                   MCITY:
                   MSTATE:
                   MZIP:
                   MCOPIES:
                   MCCTO:
                   MPASS:
                   MSLSPER:
                   MREVDATE:
                   MSTAT:
                   mtele:
                   mfax
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF MLRS WITH USAGE ",USED
         GOTO      main
.
         INCLUDE   COMLOGIC.INC
.START PATCH 1.31 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.31 REPLACED LOGIC
         include   nord2io.inc
.
EOJ
         CLOSE     OUTPUT
         CLOSE     DISKIN
         STOP
. 

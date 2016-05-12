*******************************************************************************
. THIS IS THE EXCHANGE DETAIL RECORD REINSTATEMENT PROGRAM DEC 1982
*******************************************************************************
. Written for Names in the News California By David Herrick
*******************************************************************************
*******************************************************************************
.
* VARIABLES
.
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.START PATCH 2.3 REPLACED LOGIC
	INCLUDE	NXNGDD.INC
	INCLUDE	NXCHDD.INC
.END PATCH 2.3 REPLACED LOGIC
.
.
RELEASE  INIT      "2.4"      DMB 18JUN2005 File Manager IP Change
.RELEASE  INIT      "2.3"      ASH 10MAY2005 NINXNUM/NINXCHNG CONVERSION
.RELEASE  INIT      "2.2"      ASH 11APR2001 FILE ADDED TO FILE MANAGER
.RELEASE  INIT      "2.1"      ASH 08JUN99 NINXCHNG.DAT Y2K FILE EXPANSION
.RELEASE  INIT      "2.0"      DLH 13MAR92
.START PATCH 2.1 - REPLACED LOGIC
.LRXNG    IFILE     KEYLEN=6,FIXED=90
.START PATCH 2.3 REPLACED LOGIC
.LRXNG    IFILE     KEYLEN=6,FIXED=96
LRXNG    IFILE     KEYLEN=6,FIXED=200
.END PATCH 2.3 REPLACED LOGIC
.END PATCH 2.1 - REPLACED LOGIC
.
+ ..EXCHANGE MASTER
.
.START PATCH 2.3 REPLACED LOGIC
.EXKEY    DIM       13             ISAM KEY MAILER#1,MAILER#2,ENTRY
.KEY      DIM       6              ISAM KEY LR NUMBER
.LIST     DIM       6              LIST NUMBER
.LR       DIM       6              LR NUMBER 14-19
..START PATCH 2.1 - REPLACED LOGIC
..QTY      FORM      7              QUANTITY OF EXCHANGE
..DAT      DIM       6              DATE OF EXCHANGE
..STAT     DIM       1              STATUS OF EXCHANGE (C) IF CANCELLED
..USAGE1   FORM      9              TOTAL USAGE TO DATE MAILER 1
..USAGE2   FORM      9              TOTAL USAGE TO DATE MAILER 2
.QTY      FORM      9              QUANTITY OF EXCHANGE
.DAT      DIM       8              DATE OF EXCHANGE
.STAT     DIM       1              STATUS OF EXCHANGE (C) IF CANCELLED
.USAGE1   FORM      10             TOTAL USAGE TO DATE MAILER 1
.USAGE2   FORM      10             TOTAL USAGE TO DATE MAILER 2
..ENDTCH 2.1 - REPLACED LOGIC
.TYPE     DIM       2              HOLDS TYPIST INITIALS 72-73
.MLRSW    DIM       1              ORDER PLACED BY MLR1 HOLDS "1"
KEY      DIM       6              ISAM KEY LR NUMBER
.END PATCH 2.3 REPLACED LOGIC
.                                 PLACED BY MLR2 SWITCH HOLDS "2"
BLANK1   DIM       1              NOT USED
ACNUM    DIM       6              HOLDS LR ON BEG. BAL ENTRIES
NACNUM   FORM      6              NUMERIC WORK FIELD
ZERO     FORM      "0"
.START PATCH 2.3 REPLACED LOGIC
.XCHCOMNT DIM       30
.MALER1   DIM       4
.MALER2   DIM       4
MALER1   DIM       6
MALER2   DIM       6
.END PATCH 2.3 REPLACED LOGIC
ANTRY    DIM       5
ANS      DIM       1
.
+******************************************************************************
. PROGRAM
*******************************************************************************
.
PREP     MOVE      "NXCH0004" TO PROGRAM
         MOVE      "RE-INSTATE EXCHANGES" TO STITLE
         MOVE      "NINCAL" TO COMPNME
         MOVE      "ABORT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
.
         MOVE      " ",STAT              CLEAR FIELD SO CORREST STATUS WRITTEN
         TRAP      ERROR IF IO
         TRAP      STOP IF F5
.START PATCH 2.2 REPLACED LOGIC
.         OPEN      LRXNG,"NINLRXNG"
.>Patch 2.4 Begin
.         OPEN      LRXNG,"NINLRXNG.ISI|20.20.30.103:502"
         OPEN      LRXNG,"NINLRXNG.ISI|NINS1:502"
.>Patch 2.4 End
.END PATCH 2.2 REPLACED LOGIC
IN1
         DISPLAY   *P01:04,*P10:4,"*** NOTE QUANTITIES WILL NOT BE":
                   " ADJUSTED!!!!!";
         DISPLAY   *P20:24,*EL,"Enter (*) to exit";
         KEYIN     *P30:15,*EL,"ENTER LR##:",*ZF,*JR,LR;
         MATCH     "00000*",LR
         GOTO      STOP IF EQUAL
         CMATCH    " ",LR
         GOTO      IN1 IF EOS
         MOVE      LR,KEY
.
INREAD
.START PATCH 2.3 REPLACED LOGIC
.         FILEPI    4;LRXNG
.         READ      LRXNG,KEY;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         FILEPI    4;LRXNG
         READ      LRXNG,KEY;NXCHVARS
.END PATCH 2.3 REPLACED LOGIC
         GOTO      NOREC IF OVER
NEXT     UNPACK    EXKEY INTO MALER1,MALER2,ANTRY
         DISPLAY   *EL,*P10:10,"MAILER ONE=",MALER1,"/MAILER TWO=",MALER2
         CMATCH    "C" TO STAT
         IF        EQUAL
         DISPLAY   *P10:12,"CANCELLED/ADJUSTED"
         ENDIF
         CMATCH    "R" TO STAT
         IF        EQUAL
         DISPLAY   *P10:12,"RENTAL"
         ENDIF
         CMATCH    "X" TO STAT
         IF        EQUAL
         DISPLAY   *P10:12,"CANCELLED/NOT ADJUSTED"
         ENDIF
AGAIN    KEYIN     *EL,*P1:22,"IS THIS RECORD YOU WANT ??",ANS
         CMATCH    "*" TO ANS
         GOTO      STOP IF EQUAL
         CMATCH    "Y" TO ANS
         GOTO      UPDATE IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      READLRX IF EQUAL
         GOTO      AGAIN IF EOS
READLRX  MOVE      LR TO KEY
         TRAP      TRYAGAIN GIVING ERROR IF IO
.START PATCH 2.3 REPLACED LOGIC
.         READKS    LRXNG;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         READKS    LRXNG;NXCHVARS
.END PATCH 2.3 REPLACED LOGIC
         GOTO      NOREC IF OVER
         MATCH     LR TO KEY
         GOTO      NOREC IF NOT EQUAL
         GOTO      NEXT
.
TRYAGAIN KEYIN     *P1:24,*EL,"ERROR ",*DV,ERROR," TRY AGAIN? ",ANS,*P1:24,*EL;
         CMATCH    "Y" TO ANS
         GOTO      READLRX IF EQUAL
         GOTO      STOP
.
UPDATE   MOVE      " ",STAT
.DOS COMPRESSION OFF REMOVED.
         keyin     *p12:18,"change status to what ? C,R,X, or ' ' ",STR1
         MOVE      STR1 TO STAT
.START PATCH 2.3 REPLACED LOGIC
.         FILEPI    1;LRXNG
.         UPDATE    LRXNG;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         move      C0,N3
         FILEPI    1;LRXNG
         UPDATE    LRXNG;NXCHVARS
.END PATCH 2.3 REPLACED LOGIC
         GOTO      IN1
NOREC
         DISPLAY   *P20:23,*EL,"NO Record found!";
         GOTO      IN1
STOP     STOP
.
ERROR
         DISPLAY   *P20:23,*HON,*EL,"IO ERROR !";
         BEEP
         STOP
.START PATCH 2.3 REPLACED LOGIC
	INCLUDE	NXNGIO.INC
	INCLUDE	NXCHIO.INC
.END PATCH 2.3 REPLACED LOGIC
	INCLUDE   COMLOGIC.inc
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   NMOADD.inc
         include   hp.inc
.
         INCLUDE   CONS.inc
         INCLUDE   CONSACCT.inc
+
+
+
.begin patch 1.3
         INCLUDE   NCSHDD.inc
         INCLUDE   NADJDD.inc
+
.end patch 1.3
         INCLUDE   NADJCLDD.inc
+
         INCLUDE   NORDDD.inc
+
.begin patch 1.2
;begin patch 1.5
;         INCLUDE   NINVDD.inc
          INCLUDE             ninvdd.inc
          Include   NinvAcddd.inc
;end patch 1.5
         include   ndatdd.inc
         include   nacddd.inc
         include   nshpdd.inc
.end patch 1.2
.START PATCH 1.4 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.4 REPLACED LOGIC
         INCLUDE   NDAT3DD.INC
         include   nescdd.inc
         inc       nmrgdd.inc
         INCLUDE   NOWNDD.INC
.
release init      "1.6"   2007July11 DLH PLI CONVERSION ---- ever used anymore????????????
.release init      "1.5"   2005March02 DLH Invoice CONVERSION
.release init      "1.4"   27MAY2004 ASH MAILER CONVERSION
.release init      "1.3"   99Apr26 DLH NINadj nadjust Y2K
.release init      "1.2"   99Apr26 DLH NININV Y2K
.release init      "1.1"   98Nov06 ASH DAT25N Y2K, File expansion
.RELEASE INIT      "1.0"   96mar30jd write out tmp eom reconn.
.                           INITIAL RELEASE
.
............................................................................
cashfile file      fixed=71
output   file      fixed=71
.
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
PAYAMT   FORM      8.2
MLRITOT  FORM      8.2       .Mlr total in (internal)
MLRETOT  FORM      8.2       .Mlr total in (external)
MLRMATOT  FORM      8.2       .Mlr total MOA applied
MLRAP    FORM      8.2
CHKITOT  FORM      8.2       .check total in (internal)
CHKETOT  FORM      8.2       .check total in (external)
CHKAP    FORM      8.2
FINAP    FORM      8.2
TOTAP2   FORM      8.2
GRDITOT  FORM      8.2        .grand total internal rec'd
GRDETOT  FORM      8.2        .grand total external rec'd
GRDMATOT FORM      8.2        .grand total MOA applied
GRDAP    FORM      8.2
GRDDIFF  FORM      8.2
PREAMT   FORM      8.2
GRDMLR   FORM      8.2
DIFF     FORM      8.2
DATEMASK INIT      "XX/XX/XX"
CSHDATE  DIM       8
SYSDATE  DIM       8
ARAMT    FORM      8.2
INVCHK1  DIM       6
APAMT    FORM      7.2
AP2AMT   FORM      9.2
DATEPRT1 DIM       8
DATEPRT2 DIM       8
HOLDMLR  INIT      "    "
holdbrk  dim       4
HOLDCHK  DIM       6
PRTCHK   DIM       6
CASH     INIT      "CSH"
EXT      INIT      "EXT"
CNTNUMB  DIM       3
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
FORM11   FORM      11
ACOUNT    FORM      5
CO       FORM      1
AMNTRECD DIM       14
TRANDTE   DIM       8
DINVDTE  DIM       8
DETAIL$  FORM      8.2
DOLLAR   INIT      "$$,$$$,$$$.99-"
MOAFLAG  FORM      2
escFLAG  init      "N"
+........................................................................
.
         MOVE      "NCSH0004 " TO PROGRAM
         MOVE      "Cash Receipts" TO STITLE
.         MOVE      "NINCSH" TO INPNAME
         MOVE      "Names In The News" TO COMPNME
.         MOVE      LOCAL TO PRTNAME
         CLEAR     HOLDCHK
         MOVE      "NINCaSH.all" TO NCSHNAME
         MOVE      C1 TO NCSHPATH
         MOVE      C4 TO Nmoapath
         MOVE      C1 TO NOWNPATH
         CLOCK     DATE TO SYSDATE
         MOVE      SYSDATE TO TODAY
         IFNZ      PC
         UNPACK    SYSDATE INTO MM,DD,YY
         PACK      SYSDATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         CALL      PAINT
         TRAP      END IF F5
         MOVE      "Exit" TO PF5
         CALL      FUNCDISP
         MOVE      c59 TO PRTLINES
.
         DISPLAY   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         open      cashfile,"nincash.dat"
         prepare   output,"\\NINS2\d\data\cashfile.dat"
         GOTO      START
.
+........................................................................
.
START    MOVE      C0 TO PAYAMT
         MOVE      C0 TO ARAMT
         MOVE      C0 TO APAMT
         MOVE      C0 TO AP2AMT
         MOVE      C1 TO MOAFLAG
.         CALL      NCSHSEQ
         FILEPI    1;CaSHFILE
         READ      CaSHFILE,SEQ;Cashvars
         GOTO      EOJ IF OVER
         ADD       C1 TO N6
         DISPLAY   *P15:09,N6
.
.begin patch 1.3
.         MOVE      CAMOUNT TO CVTFLD
.         CALL      CVT
.         MOVE      NUM102 TO PAYAMT
          move      camount to payamt
.end patch 1.3
         move      cmo to mm
         move      cdy to dd
         move      cyr to yy
.Start patch #1.1 - added line to cover eventual change of logic in cvtjults 
         move      cce to cc
.End patch #1.1 - added line to cover eventual change of logic in cvtjults
         call      cvtjults
.begin patch 1.6
.         write     output,seq;cnum,b1,clr,b1,juldays,b1,payamt,b2,ncshchk,b1,cextcd
         write     output,seq;cnum,b1,clr,b1,juldays,b1,payamt,b2,ncshchk,b1,cextcd,b1,CCOMPID
.end patch 1.6
         GOTO      START

eoj
         
         weof      output,seq
         close     output
         close     cashfile
         stop
+........................................................................
.
         INCLUDE   NCSHIO.inc
+
;begin patch 1.5
;         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
          Include   NInvAcdio.inc
;end patch 1.5
         INCLUDE   NADJIO.inc
+
.begin patch 1.3
.         INCLUDE   COMPUTE.inc
.end patch 1.3
.
         INCLUDE   NADJCLIO.inc
+
         INCLUDE   NORDIO.inc
+
.START PATCH 1.4 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.4 REPLACED LOGIC
+
.begin patch 1.2
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
.end patch 1.2
         INCLUDE   NMOAIO.INC
         INCLUDE   NDAT3IO.INC
         INCLUDE   NOWNIO.INC
         INCLUDE   NESCIO.INC
         include   nmrgio.inc
         INCLUDE   COMLOGIC.inc


............................................................................
.
. PROGRAM    : NCSH005
. DATE       : 03/17/97
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         include   hp.inc
.
         INCLUDE   CONS.inc

         INCLUDE   CONSACCT.inc
+
.begin patch 1.2
         INCLUDE   NCSHDD.inc
.end patch 1.2
+
;begin patch 1.3
;         INCLUDE   NINVDD.inc
          INCLUDE             ninvdd.inc
          Include   NinvAcddd.inc
;end patch 1.3
+
.
release   init     "1.3"        2005March02 DLH Invoice conversion
;release   init     "1.2"        26APR99 DLH NINadj nadjust Y2k
.release   init     "1.1"        26APR99 DLH NININV Y2k
.release   init     "1.0"        17mar97 JD reopen invoices re-run checks
............................................................................
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
lasrflag init      "T"            generally true unless a/p clerk has req
IDPASS   INIT      "PASS"
ID       DIM       4
COUNT    FORM      5
+........................................................................
.
         MOVE      "NCSH0005 " TO PROGRAM
         MOVE      "Cash RE-OPENING" TO STITLE
.         MOVE      "NINCSH" TO INPNAME
           MOVE        "Names In The News" TO COMPNME
.         MOVE      LOCAL TO PRTNAME
         MOVE      "cash99" TO NCSHNAME
         MOVE      C1 TO NCSHPATH
         MOVE      C1 TO Ninvpath
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
.
         DISPLAY   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
.
         keyin     *P10:15,"THIS WILL REOPEN ALL INVOICES IN FILE (CASH99.DAT)!!!",str1;
ID       KEYIN     *Ef,*p10:15,*P1:24,*EOFF,"ENTER PASSWORD ",ID;
         MATCH     ID TO IDPASS
         GOTO      INPUT IF EQUAL
         GOTO      nopass
.
+........................................................................
.
INPUT    CALL       NCSHSEQ
         GOTO      FINISH IF OVER
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"NUMBER OF CASH ENTRIES PROCESSED: ",COUNT
CHKEXT
..................................................

         MOVE      CLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         GOTO      INPUT IF OVER
UPDPRE
;begin patch 1.3
          Move      C0 to statb
          call      Ninvupd
;         FILEPI    1;NINVFILE
;         UPDATAB   NINVFILE;*2,"0"
;end patch 1.3
         GOTO      INPUT

NOPASS    
         DISPLAY   *P1:24,"PASSWORD REJECTED NO RECORDS UPDATED! ",*w3
FINISH  
         STOP
+........................................................................
.
         INCLUDE   NCSHIO.inc
+
;begin patch 1.3
;         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
          Include   NInvAcdio.inc
;end patch 1.3
         INCLUDE   COMLOGIC.inc

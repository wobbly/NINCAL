
.
. NMLR0008 CATEGORY/ ALPHA PRINT OF MAILERS SHOWING MAILER#
. AND MAILER CATEGORY.
. QUICK REFERENCE GUIDE PRINTED ON BLANK 8 1/2 * 11 STOCK
. 
. 
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NMTXDD.inc
         INCLUDE   NMLCDD.inc
         INCLUDE   NMLGDD.inc
;begin patch 1.6
;         INCLUDE   NMLRDD.inc
	INCLUDE 	COMPDD.INC
	INCLUDE 	CNTDD.INC
;end patch 1.6
         include   hp.inc
.
Release	Init	"1.6"	DLH 29July2005 Mailer conversion
;RELEASE  INIT      "1.5"         ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.4"         DLH 18MAR92   CLEANUP, PCBUS, NEW INCLUDES:
.                                NMLCXX, NMLGXX
.RELEASE  INIT      "1.3"        DLH 07JAN92   NEW MAILER INCLUDES.
.
.RELEASE  INIT      "1.2"       D.L. HERRICK 17OCT91
.                              ADDED MAILER TAX INCLUDES.
.
.RELEASE  INIT      "1.1"       D.L. HERRICK 12AUG91
.                              ADDED OPTION TO SUPPRESS INACTIVE MLRS.
.
;RELEASE  INIT      "1.0"       D. HERRICK 05AUG91

.
. ..MISCELLANEOUS FIELDS
.
PGNO     FORM      "   1"    PAGE NUMBER
LINCTR   FORM      2         LINE COUNTER
HLDMLR   DIM       6         CHECKS FOR NEW CLIENT NUMBER
CONTLINE DIM       55        CONTACT INFO LINE
HOLDCAT  DIM       45
CATTAB   FORM      3
HLDCOMP  DIM       25        HOLDS COMPANY NAME FOR PAGE OVERFLOW
COHEAD   DIM       5         COMPANY HEADING
DOT30    INIT      ".............................."
CONTD    DIM       1
TEMPKEY  DIM       58
SPOOL    DIM       1
INMLRSW  DIM       1         INCLUDE INACTIVE MLRS, 'Y=YES, DEFAULT
RECOUNT  FORM      4         NUMBER OF RECORDS READ.
DATE     DIM       8
STAT501  DIM       2
CATDES2  DIM       45
CATDES3  DIM       45
PRTSW    DIM       1          'Y' = PRINT ONLY
;begin patch 1.6
TfILE 	IFILE    	KEYLEN=58,var=509
;TfILE 	IFILE    	KEYLEN=28,var=154
;end patch 1.6
+
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         CMATCH    B1 TO TODAY
         IF        EOS
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         ENDIF
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         CMATCH    B1 TO TODAY
         IF        EOS
         MOVE      DATE TO TODAY
         ENDIF
         XIF
         MOVE      "ABORT" TO PF5
         TRAP      ABORT IF F5
         MOVE      "NMLR0008" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "MAILER CATEGORY PRINT" TO STITLE
BEGIN    CALL      PAINT
         CALL      FUNCDISP
         KEYIN     *P1:24,*EL,"Default is No":
                   *P22:14,*EL,"Print only ? ",*T20,PRTSW
         KEYIN     *P1:24,*EL,"Default is yes":
                   *P22:14,*EL,"Do you want spooling ? ",*T30,SPOOL
         KEYIN     *P1:24,*EL,"Default is yes":
                   *P22:14,*EL,"Do you want Inactives ? ",*T30,INMLRSW
         CMATCH    NO TO SPOOL
         GOTO      PRINTOK IF EQUAL
         MOVE      YES TO SPOOL
         IFNZ      PC
         SPLOPEN   "MLRCATPRT/PRT:PRINT"
         XIF
         IFZ       PC
.START PATCH 1.5 REPLACED LOGIC
.         SPLOPEN   "e:\data\MLRCAT"
         PACK      STR35,NTWKPATH1,"MLRCAT"
         SPLOPEN   STR35
.END PATCH 1.5 REPLACED LOGIC
         print      hpreset,hpport,hpdupl,*f
         XIF
PRINTOK
         DISPLAY   *P1:24,*EL;
         CMATCH    YES TO PRTSW
         GOTO      PRTONLY IF EQUAL
         IFNZ      PC
         TRAP      NOFILE IF IO
         PREPARE   TFILE,"TMPmcat:PRINT"
         XIF
         IFZ       PC
         TRAP      NOFILE IF IO
.START PATCH 1.5 REPLACED LOGIC
;begin patch 1.6
.         PREPARE   TFILE,"e:\data\TMPmcat","e:\data\TMPmcat","28","154"
         PACK      STR35,NTWKPATH1,"TMPmCAT"
         PACK      STR45,NTWKPATH1,"TMPmCAT"
         PREPARE   TFILE,STR35,STR45,"58","509"
;end patch 1.6
.END PATCH 1.5 REPLACED LOGIC
         XIF
         TRAPCLR   IO
LOOP1    DISPLAY   *P10:10,"PASS ONE PROCESSING MAILERS : ",N5
         CALL      compSeq
;begin patch 1.6
          if	     (compmlrflg = "F")
          goto	     loop1
          elseif   (CompNum = "") 
          goto		     passtwo
          endif
;         GOTO      PASSTWO  IF OVER
         ADD       C1 TO N5
         MATCH     CompNum TO HLDMLR
         GOTO      LOOP1 IF EQUAL
         if        (inmlrsw=no & mstat="I")
         goto      loop1
         endif
.         CMATCH    NO TO INMLRSW
.         IF        EQUAL
.         CMATCH    "I" TO MSTAT
.         GOTO      LOOP1 IF EQUAL
.         ENDIF
         MOVE      CompNum TO NMLCFLD
         MOVE      CompNum TO HLDMLR
;end patch 1.6
         CALL      NMLCKEY
         IF        OVER
         MOVE      B3 TO NMLCCAT1
         CLEAR     NMLCCAT2
         CLEAR     NMLCCAT3
         GOTO      LOOP1
         ENDIF
         CALL      WRITEMP
         GOTO      LOOP1
PASSTWO  CLOSE     TFILE
PRTONLY  MOVE      "N I N",COHEAD
         OPEN      TFILE,"\\nins1\e\data\TMPmcat",read
         GOTO      READTEMP
HEADING
         PRINT     *F,*19,hpdtch10,hpfixed,"***  ",COHEAD,"   M A S ":
                   "T E R   C L I E N T   F I L E  ***":
                   *L,*05,"DATE ",TODAY,*70,"PAGE ",PGNO,*L:
                   *L,*05,"COMPANY NAME",*66,"NUMBER":
                   *L,*05,"------------",*66,"------",*L:
                   *CATTAB,HOLDCAT,*L
         MOVE      C8,LINCTR
         ADD       C1,PGNO
         RETURN
.
HEADCAT 
.         reset    holdcat
         CLEAR     HOLDCAT
         MOVE      NMLGCAT TO HOLDCAT
         RESET     HOLDCAT
         SETLPTR   HOLDCAT
         ENDSET    HOLDCAT
CHKCHEAD CMATCH    B1 TO HOLDCAT
         GOTO      SETCHEAD IF EQUAL
         BUMP      HOLDCAT BY -1
         GOTO      CHKCHEAD IF NOT EOS
SETCHEAD MOVEFPTR  HOLDCAT TO N3
         MOVE      "132" TO CATTAB
         SUB       N3 FROM CATTAB
         DIVIDE    C2 INTO CATTAB
         RESET     HOLDCAT
         SETLPTR   HOLDCAT
         CALL      HEADING
         RETURN
READTEMP
.  
;begin patch 1.6

         READKS    TFILE;COMPVARS:
                   NMLCCAT1,NMLCCAT2,NMLCCAT3
.
         GOTO      EXIT IF OVER
         MOVE      CompNum TO HLDMLR
         MOVE      NMLCCAT1 TO NMLGFLD
         CALL      NMLGKEY
         CALL      NOCAT IF OVER
         match     b2 to nmlgcat
         call      nocat if equal
         call      nocat if eos
         CLEAR     STAT501
         MOVE      CompNum TO NMTXFLD
         CALL      NMTXKEY
         IF         NOT OVER
         MOVE      HLDMLR TO CompNum
         ELSE
         MATCH     "3" TO MTXC501
         CALL      C3 IF EQUAL
         MATCH     "4" TO MTXC501
         CALL      C4 IF EQUAL
         MATCH     "5" TO MTXC501
         CALL      C5 IF EQUAL
         MATCH     "6" TO MTXC501
         CALL      C6 IF EQUAL
         ENDIF
         ADD       C1 TO RECOUNT
         DISPLAY   *P10:12,"Number of records printed: ",RECOUNT
         MOVE      CompCOMP,HLDCOMP
         GOTO      PRNTCOMP
;end patch 1.6
.
C3       MOVE      "C3" TO STAT501
         RETURN
C4       MOVE      "C4" TO STAT501
         RETURN
C5       MOVE      "C5" TO STAT501
         RETURN
C6       MOVE      "C6" TO STAT501
         RETURN
PRNTCOMP
         COMPARE   C1 TO RECOUNT
         CALL      HEADCAT IF EQUAL
         COMPARE   "57",LINCTR
         CALL      HEADING IF NOT LESS
         MATCH     NMLGCAT TO HOLDCAT
         CALL      HEADCAT IF NOT EQUAL
         MOVE      NMLCCAT2 TO NMLGFLD
         CLEAR     NMLGCAT
         CALL      NMLGKEY
         MOVE      NMLGCAT TO CATDES2
         MOVE      NMLCCAT3 TO NMLGFLD
         CLEAR     NMLGCAT
         CALL      NMLGKEY
         MOVE      NMLGCAT TO CATDES3
         PRINT     *5,hpdtch10,hpfixed,CompCOMP,*35,DOT30,*66,CompNum," ",STAT501:
                   *N,*8,hpdtch85,CATDES2,CATDES3,hpdtch10,hpfixed
         ADD       C2 TO LINCTR
         GOTO      READTEMP
. 
ABORT    TRAPCLR   F3
         NORETURN
         PRINT     *L,*1,"*****************JOB ABORTED BY OPERATOR";
         PRINT      "*****************";
         PRINT      *1,"*****************JOB ABORTED BY OPERATOR";
         PRINT      "*****************"
         DISPLAY   *P1:24,*HON,*EL,"JOB ABORTED",*B,*B,*W;
EXIT
         DISPLAY   *P1:24,*EL,"JOB IS DONE",*W9;
         BEEP
         PRINT     *F
         RELEASE
         SPLCLOSE
         CMATCH    YES TO SPOOL
         GOTO      RELEASE IF NOT EQUAL
         GOTO      SHUTDOWN
RELEASE
         RELEASE
SHUTDOWN
         STOP
NOFILE   DISPLAY   *P1:24,*EL,"FILE NOT FOUND",*B,*B,*W3;
         STOP
.
;begin patch 1.6

WRITEMP  PACK      TEMPKEY FROM NMLCCAT1,CompCOMP
         READ      TFILE,TEMPKEY;;
         RETURN    IF NOT OVER
         
         WRITE     TFILE,TEMPKEY;CompVars:
                   NMLCCAT1,NMLCCAT2,NMLCCAT3
.
         return
..........................................temp return.         
         MATCH     B3 TO NMLCCAT2
         RETURN    IF EQUAL
         RETURN    IF EOS
         PACK      TEMPKEY FROM NMLCCAT2,compCOMP
         READ      TFILE,TEMPKEY;;
         RETURN    IF NOT OVER
         MOVE      NMLCCAT2 TO NMLGFLD
         CALL      NMLGKEY
         return    if over
         WRITE     TFILE,TEMPKEY;CompVars:
                   NMLCCAT1,NMLCCAT2,NMLCCAT3
.
         MATCH     B3 TO NMLCCAT3
         RETURN    IF EQUAL
         RETURN    IF EOS
         PACK      TEMPKEY FROM NMLCCAT3,CompCOMP
         READ      TFILE,TEMPKEY;;
         RETURN    IF NOT OVER
         MOVE      NMLCCAT3 TO NMLGFLD
         CALL      NMLGKEY
         return    if over
         WRITE     TFILE,TEMPKEY;CompVars:
                   NMLCCAT1,NMLCCAT2,NMLCCAT3
;end patch 1.6
.
         RETURN
.
NOCAT    MOVE      "No Category" TO NMLGCAT
         display   *p1:24,*el,"no cat";
         RETURN

         ;INCLUDE   NMLRIO.inc
         INCLUDE   NMLGIO.inc
         INCLUDE   NMLCIO.inc
         INCLUDE   NMTXIO.inc
	INCLUDE 	COMPIO.INC
	INCLUDE 	CNTIO.INC
         INCLUDE   COMLOGIC.inc


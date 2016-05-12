............................................................................
.
. PROGRAM    : Neom0010
. DATE       : 03/28/88
. AUTHOR     : E.W. LAKE
. DESCRIPTION: PRODUCES NIN ADJUSTMENT out of balance REGISTER.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         include   hp.inc
.
RELEASE  INIT      "1.13"    DMB  31MAY2005 NJSTDD CONV
.RELEASE  INIT      "1.12"    ASH 03AUG04 LOGO CONVERSION
.RELEASE  INIT      "1.11"    ASH 27MAY2004 MAILER CONVERSION
.RELEASE  INIT      "1.10"    ASH 14SEP98 NINMLR.DAT Y2K file expansion
.RELEASE  INIT      "1.00"    DLH 06apr95 
.                           INITIAL RELEASE
.
DATE     DIM       8
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
xfoot     form      10.2
TAR      FORM      8.2
TAP1     FORM      8.2
TAP2     FORM      8.2
TAP      FORM      8.2        TOTAL AP1+AP2
TNININC  FORM      8.2
TLRINC   FORM      8.2
TLRINC1  FORM      8.2        BROKERAGE EXCHANGE
TLRINC2  FORM      8.2        LIST MANAGEMENT EXCHANGE
TLRINC3  FORM      8.2        BROKERAGE RENTAL
TLRINC4  FORM      8.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
TSTAX    FORM      7.2
TCTAX    FORM      7.2
TPOST    FORM      7.2
R11      FORM      7.2
R12      FORM      7.2
R14      FORM      7.2
R16      FORM      7.2
R20      FORM      7.2
R21      FORM      7.2
R22      FORM      7.2
R23      FORM      7.2
R24      FORM      7.2
R25AR    FORM      7.2
R25AP    FORM      7.2
R25LR    FORM      7.2
R26      FORM      7.2
R27      form      7.2
R29      form      7.2
R30      FORM      7.2
R31      FORM      7.2
hund     form      "100"
form82   form      8.2
TINVQTY  FORM      9
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
M1       INIT      " JANUARY 19?? "
M2       INIT      "FEBRUARY 19?? "
M3       INIT      "  MARCH 19??  "
M4       INIT      "  APRIL 19??  "
M5       INIT      "   MAY 19??   "
M6       INIT      "  JUNE 19??   "
M7       INIT      "  JULY 19??   "
M8       INIT      " AUGUST 19??  "
M9       INIT      "SEPTEMBER 19??"
M10      INIT      " OCTOBER 19?? "
M11      INIT      "NOVEMBER 19?? "
M12      INIT      "DECEMBER 19?? "
MONTH    DIM       14
PREFLAG  DIM       1
.Start patch #1.10 - NINMLR file expansion
.NAME1    DIM       19
.NAME2    DIM       19
NAME1    DIM       45
NAME2    DIM       45
.end patch #1.10 - NINMLR file expansion
.
AR       FORM      9.2       A/R
AP1      FORM      7.2       A/P1
AP2      FORM      7.2       A/P2
AP       FORM      7.2       A/P1 + A/P2
LRINC    FORM      7.2       LR INCOME
NINC     Form	     7.2
GROSS    FORM      7.2       GROSS BILLING
CTAX     FORM      7.2       CITY TAX
STAX     FORM      7.2       STATE TAX
TAXES    FORM      7.2       TOTAL TAXES
ARWOPP   FORM      7.2       A/R WITHOUT PRE-PAYMENT
SHIP     FORM      7.2       TOTAL SHIPPING
SELECT   FORM      7.2       TOTAL SELECTIONS
POST     FORM      7.2
............................................................................
+
         INCLUDE   CONS.inc
+
.START PATCH 1.11 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc    MAILER
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.11 REPLACED LOGIC
+
         INCLUDE   NBILDD.inc    BILL-TO
+
         INCLUDE   NORDDD.inc
+
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
+
         INCLUDE   NJSTCLDD.inc  ADJUSTMENT APPLY
.
+...........................................................................
.
         MATCH     B8 TO TODAY
         IF        EQUAL
         CALL      GETDATE
                   ELSE
         IF        EOS
         CALL      GETDATE
         ENDIF
         ENDIF
         MOVE      "Adjustment x-foot Register" TO STITLE
         MATCH     "NADJ0008" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "NADJ0008" TO PROGRAM
         IFNZ      PC
         MOVE      "ADJUST/tmp" TO INPNAME
         XIF
         IFZ       PC
         MOVE      "ADJUST.tmp" TO INPNAME
         XIF
         MOVE      "adjxfoot" TO PRTNAME
         MOVE      "Names in the News Ca" TO COMPNME
         ENDIF
         CALL      PAINT
.
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
DATEOK   KEYIN     *P10:10,"DATE OK? ",*T60,STR1
         CMATCH    NO TO STR1
         IF        EQUAL
         KEYIN     *P10:12,*EL,"MM/DD/YY":
                   *P10:12,*+,*ZF,*JR,MM,*DV,SLASH,*ZF,*JR,DD:
                   *DV,SLASH,*ZF,*JR,YY,*-
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         DISPLAY   *P10:12,TODAY,*EL
         GOTO      DATEOK
         ENDIF
         DISPLAY   *P10:10,*EL,*P10:12,*EL
         GOTO      BEGIN
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         RETURN
.
BEGIN    UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
         SCAN      "??" IN MONTH
         GOTO      PRTGET IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
.
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Adjust Date : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START    
.>Patch 1.13 Modified Code
	MOVE      INPNAME TO NJSTNAMESEQ
.	MOVE      INPNAME TO NJSTNAME
.>Patch 1.13 End 
         CALL      HEADING
.
RDADJ    CALL      NJSTSEQ
         GOTO      EOJ IF OVER
         ADD       C1 TO N5
         DISPLAY   *P15:08,N5:
                   *P15:09,JSTDATE
.
         move      c0 to xfoot
         move      c0 to cvtfld
         move      jstar to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         add       form82 to xfoot
.
         move      c0 to cvtfld
         move      jstap1 to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         move      c0 to cvtfld
         move      jstap2 to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         move      c0 to cvtfld
         move      jstlrinc to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         move      c0 to cvtfld
         move      jststax to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         move      c0 to cvtfld
         move      jstctax to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         move      c0 to cvtfld
         move      jstpost to cvtfld
         call      cvt
         move      cvtfld to form82
         div       hund into form82
         sub       form82 to xfoot
.
         compare   c0 to xfoot
         goto      rdadj if equal
.         
         PACK      STR8 WITH JSTMLR,JSTCNT,JSTBILTO
         MATCH     STR8 TO NBILFLD
         GOTO      DTLPRT IF EQUAL
         MOVE      STR8 TO NBILFLD
         CALL      NBILKEY
         GOTO      GETMLR IF OVER
         MOVE      BILNAME TO NAME1
         MOVE      BILCOMP TO NAME2
         GOTO      DTLPRT
GETMLR   PACK      MKEY FROM NBILFLD
         CALL      NMLRKEY
         MOVE      MNAME TO NAME1
         MOVE      MCOMP TO NAME2
.
DTLPRT   MOVE      DATEMASK TO DATEPRT1
         EDIT      JSTDATE  TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         EDIT      JSTINVDT TO DATEPRT2
         REPLACE   "0 " IN JSTISTAT
.
         CALL      NJSTCALC
.
         MOVE      C1 TO NORDPATH
         PACK      NORDFLD WITH JSTLR
         CALL      NORDKEY
         DISPLAY   *P01:19,OLRN,B1,O1DES
.
CHK11    MATCH     "11" TO JSTREASN
         GOTO      CHK12 IF NOT EQUAL
         ADD       Ap TO R11
         GOTO      DTLPRT1
.
CHK12    MATCH     "12" TO JSTREASN
         GOTO      CHK14 IF NOT EQUAL
         ADD       lrinc TO R12
         GOTO      DTLPRT1
.
chk14    MATCH     "14" TO JSTREASN
         GOTO      CHK16 IF NOT EQUAL
         ADD       AP TO R14
         GOTO      DTLPRT1
.
CHK16    MATCH     "16" TO JSTREASN
         GOTO      CHK20 IF NOT EQUAL
         ADD       AR TO R16
         GOTO      DTLPRT1
.
CHK20    MATCH     "20" TO JSTREASN
         GOTO      CHK21 IF NOT EQUAL
         ADD       POST TO R20
         GOTO      DTLPRT1
.
CHK21    MATCH     "21" TO JSTREASN
         GOTO      CHK22 IF NOT EQUAL
         ADD       AP TO R21
         GOTO      DTLPRT1
.
CHK22    MATCH     "22" TO JSTREASN
         GOTO      CHK23 IF NOT EQUAL
         ADD       AR TO R22
         GOTO      DTLPRT1
.
CHK23    MATCH     "23" TO JSTREASN
         GOTO      CHK24 IF NOT EQUAL
         ADD       AP TO R23
         GOTO      DTLPRT1
.
CHK24    MATCH     "24" TO JSTREASN
         GOTO      CHK25 IF NOT EQUAL
         ADD       AP TO R24
         GOTO      DTLPRT1
.
CHK25    MATCH     "25" TO JSTREASN
         GOTO      CHK26 IF NOT EQUAL
         ADD       LRINC TO R25LR
         ADD       AP TO R25AP
         ADD       AR TO R25AR
         GOTO      DTLPRT1
.
CHK26    MATCH     "26" TO JSTREASN
         GOTO      CHK27 IF NOT EQUAL
         ADD       LRINC TO R26
         GOTO      DTLPRT1
.
CHK27    MATCH     "27" TO JSTREASN
         GOTO      CHK29 IF NOT EQUAL
         ADD       Ar TO R27
         GOTO      DTLPRT1
.
CHK29    MATCH     "29" TO JSTREASN
         GOTO      CHK30 IF NOT EQUAL
         ADD       AP TO R29
         GOTO      DTLPRT1
.
CHK30    MATCH     "30" TO JSTREASN
         GOTO      CHK31 IF NOT EQUAL
         ADD       LRINC TO R30
         GOTO      DTLPRT1
.
CHK31    MATCH     "31" TO JSTREASN
         GOTO      DTLPRT1 IF NOT EQUAL
         ADD       LRINC TO R31
DTLPRT1  RESET     EXCODES
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      C0 TO N2
         MOVE      STR2 TO N2
         COMPARE   C0 TO N2
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      C1 TO N2
         MOVE      C2 TO OELCODE
         ELSE     
         MOVE      C6 TO N2
         GOTO      EXQUES
         ENDIF
         ENDIF
.         GOTO      EXQUES IF NOT EQUAL
.         MOVE      C1 TO N2
.         MOVE      C2 TO OELCODE
.         ENDIF
EXQUES   RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         IF        EQUAL                   *EXCHANGE
         BRANCH    N2 OF BRK,BRK,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:
                  BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,LSTM,BRK:
                   BRK,BRK,BRK,BRK,BRK
LSTM     ADD       LRINC TO TLRINC2
         GOTO      DTLPRT2
BRK      ADD       LRINC TO TLRINC1
                ELSE                       *RENTAL
         BRANCH    N2 OF BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1
LSTM1    ADD       LRINC TO TLRINC4
         GOTO      DTLPRT2
BRK1     ADD       LRINC TO TLRINC3
         ENDIF
.
DTLPRT2  COMPARE   C60 TO PRTLINES
         CALL      HEADING IF NOT LESS
         PRINT     *N,*01,JSTMLR,SLASH,JSTCNT:
                      *10,NAME1:
                      *30,JSTLR:
                      *37,DATEPRT1:
                      *46,JSTINVNO,DASH,JSTSUBNO:
                      *55,DATEPRT2:
                      *64,AR:
                      *79,AP1:
                      *89,LRINC:
                      *99,CTAX:
                      *108,STAX:
                      *118,POST:
                      *129,JSTREASN,JSTISTAT:
                   *N,*10,NAME2:
                      *79,AP2:
                      *129,JSTCD:
                   *N,*10,O1DES
         ADD       C4 TO PRTLINES
.
         ADD       AR     TO TAR
         ADD       AP1    TO TAP1
         ADD       AP2    TO TAP2
         ADD       LRINC  TO TLRINC
         ADD       STAX   TO TSTAX
         ADD       CTAX   TO TCTAX
         ADD       POST   TO TPOST
         MOVE      C0     TO AR
         MOVE      C0     TO AP
         MOVE      C0     TO AP1
         MOVE      C0     TO AP2
         MOVE      C0     TO LRINC
         MOVE      C0     TO STAX
         MOVE      C0     TO CTAX
         MOVE      C0     TO POST
.
         GOTO      RDADJ
.
EOJ     
.          COMPARE   C45 TO PRTLINES
.         CALL      HEADING IF NOT LESS
         CALL      HEADING 
         SUB       TAP FROM TAP
         ADD       TAP1 TO TAP
         ADD       TAP2 TO TAP
         ADD       TLRINC2 TO TLRINC4
         PRINT     *N:
                   *N,*01,MONTH,":":
                      *65,TAR:
                      *78,TAP1:
                      *89,TLRINC:
                      *101,TCTAX:
                      *110,TSTAX:
                      *120,TPOST:
                   *N,*78,TAP2:
                   *N,*78,"-----------":
                   *N,*01,N5,B1,"ADJUSTMENTS",*78,TAP:
                   *N:
                   *N,*01,"TOTAL LRINC BROKERAGE RENTAL: ",*88,TLRINC3:
                   *N,*01,"TOTAL LRINC BROKERAGE XCHNGE: ",*88,TLRINC1:
                   *N,*01,"TOTAL LRINC LIST MANAGEMENT : ",*88,TLRINC4:
                   *N,*01,"     AP REASON CODE 11 TOTAL: ",R11:
                   *N,*01,"  LRINC REASON CODE 12 TOTAL: ",R12:
                   *N,*01,"     AP REASON CODE 14 TOTAL: ",R14:
                   *N,*01,"     AR REASON CODE 16 TOTAL: ",R16:
                   *N,*01,"POSTAGE REASON CODE 20 TOTAL: ",R20:
                   *N,*01,"     AP REASON CODE 21 TOTAL: ",R21:
                   *N,*01,"     AR REASON CODE 22 TOTAL: ",R22:
                   *N,*01,"     AP REASON CODE 23 TOTAL: ",R23:
                   *N,*01,"     AP REASON CODE 24 TOTAL: ",R24:
                   *N,*01,"     AR REASON CODE 25 TOTAL: ",R25AR:
                   *N,*01,"     AP REASON CODE 25 TOTAL: ",R25AP:
                   *N,*01,"  LRINC REASON CODE 25 TOTAL: ",R25LR:
                   *N,*01,"  LRINC REASON CODE 26 TOTAL: ",R26:
                   *N,*01,"Prepay  REASON CODE 27 TOTAL: ",R27:
                   *N,*01,"CND tax REASON CODE 29 TOTAL: ",r27:
                   *N,*01,"  LRINC REASON CODE 30 TOTAL: ",R30:
                   *N,*01,"  LRINC REASON CODE 31 TOTAL: ",R31:
                   *F
.                   *N,*01,"TOTAL LRINC LIST MAN. RENTAL: ",*88,TLRINC4:
.                   *N,*01,"TOTAL LRINC LIST MAN. XCHNGE: ",*88,TLRINC2:
         BRANCH    PRTFLAG TO END
         print     hpreset
         release
         SPLCLOSE
         CALL      REMVTOF
         GOTO      END
+............................................................................
.
HEADING  ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch,hpdupl,hptop,*f
         endif
.START PATCH 1.12 REPLACED LOGIC
.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*55,"ADJUSTMENT REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*01,"CLIENT##":
.                      *10,"BILL-TO/LIST":
.                      *32,"LR":
.                      *38,"CREDIT":
.                      *47,"INVOICE":
.                      *56,"INVOICE":
.                      *67,"ACCOUNTS":
.                      *81,"ACCOUNTS":
.                      *95,"LR":
.                      *105,"CITY":
.                      *113,"STATE":
.                      *122,"OUR":
.                   *N,*30,"NUMBER":
.                      *39,"DATE":
.                      *48,"NUMBER":
.                      *58,"DATE":
.                      *66,"RECEIVABLE":
.                      *82,"PAYABLE":
.                      *93,"INCOME":
.                      *105,"TAX":
.                      *114,"TAX":
.                      *121,"POSTAGE"
         PRINT        *f,*n,"CONFIDENTIAL":
                      *54,"NAMES IN THE NEWS":
                      *119,"DATE: ",TODAY:
                   *N,*55,"ADJUSTMENT REGISTER":
                      *119,"PAGE:    ",PAGE:
                   *N,*58,MONTH:
                   *N:
                   *N,*01,"CLIENT##":
                      *10,"BILL-TO/LIST":
                      *32,"LR":
                      *38,"CREDIT":
                      *47,"INVOICE":
                      *56,"INVOICE":
                      *67,"ACCOUNTS":
                      *81,"ACCOUNTS":
                      *95,"LR":
                      *105,"CITY":
                      *113,"STATE":
                      *122,"OUR":
                   *N,*30,"NUMBER":
                      *39,"DATE":
                      *48,"NUMBER":
                      *58,"DATE":
                      *66,"RECEIVABLE":
                      *82,"PAYABLE":
                      *93,"INCOME":
                      *105,"TAX":
                      *114,"TAX":
                      *121,"POSTAGE"
.END PATCH 1.12 REPLACED LOGIC
         MOVE      C8 TO PRTLINES
         RETURN
+...........................................................................
.
.START PATCH 1.11 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc    MAILER
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.11 REPLACED LOGIC
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NORDIO.inc
+
         INCLUDE   NJSTIO.inc    ADJUSTMENT DETAIL
+
         INCLUDE   NJSTCLIO.inc  ADJUSTMENT DETAIL
+
         INCLUDE   COMLOGIC.inc


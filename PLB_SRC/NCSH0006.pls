.
. PROGRAM    : Ncsh0006
. DATE       : 03/20/95
. AUTHOR     : D.L. Herrick
. DESCRIPTION: PRODUCES NIN Cash control ADJUSTMENT REGISTER.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
.begin patch 1.2
         INCLUDE   NCSHDD.inc
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
.end patch 1.2
         include   hp.inc
+
         INCLUDE   CONS.inc
+
.START PATCH 1.41 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.41 REPLACED LOGIC
+
         INCLUDE   NBILDD.inc    BILL-TO
+
         INCLUDE   NORDDD.inc
+
;begin patch 1.5
          include             ninvdd.inc
;end patch 1.5
         INCLUDE   NJSTCLDD.inc  ADJUSTMENT APPLY
.
         liston
.
RELEASE  INIT      "1.7"       2007August16 DLH  PLI
.RELEASE  INIT      "1.6"       2007March05 DLH  oslspern
.RELEASE  INIT      "1.5"       2005March02 DLH  Invoice Conversion
.RELEASE  INIT      "1.42"       05AUG2004 ASH  LOGO CONVERSION
.RELEASE  INIT      "1.41"       27MAY2004 ASH  MAILER CONVERSION
.RELEASE  INIT      "1.4"       18July01 DLH Change date check evidently contrary to previous info sometimes
.                               adjustments are done with a date prior to the control date
.RELEASE  INIT      "1.3"       14Sep00 DLH tighten up date check to only get pertinate adjustment records
.RELEASE  INIT      "1.2"       99AAug24 DLH NINadj nadjust Y2k
.RELEASE  INIT      "1.1"       99APR26 DLH NININV Y2k
.RELEASE  INIT      "1.0"       05Mar98
.
DATE     DIM       8
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
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
R30      FORM      7.2
R31      FORM      7.2
TINVQTY  FORM      9
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
M1       INIT      " JANUARY "
M2       INIT      "FEBRUARY "
M3       INIT      "  MARCH "
M4       INIT      "  APRIL "
M5       INIT      "   MAY "
M6       INIT      "  JUNE "
M7       INIT      "  JULY "
M8       INIT      " AUGUST "
M9       INIT      "SEPTEMBER "
M10      INIT      " OCTOBER "
M11      INIT      "NOVEMBER "
M12      INIT      "DECEMBER "
MONTH    DIM       18
PREFLAG  DIM       1
NAME1    DIM       19
NAME2    DIM       19
.       

AP       FORM      7.2       A/P1 + A/P2
LRINC    FORM      7.2       LR INCOME
NINC     FORM      7.2       NIN INCOME
GROSS    FORM      7.2       GROSS BILLING
CTAX     FORM      7.2       CITY TAX
STAX     FORM      7.2       STATE TAX
TAXES    FORM      7.2       TOTAL TAXES
ARWOPP   FORM      7.2       A/R WITHOUT PRE-PAYMENT
SHIP     FORM      7.2       TOTAL SHIPPING
SELECT   FORM      7.2       TOTAL SELECTIONS
POST     FORM      7.2
.begin release 1.3
.datechck dim       4
datechck form       5
.end release 1.3
holdc    dim       3
count    form      4
adjcnt   form      4 
;begin patch 1.5
holdinvNum  dim        6
;end patch 1.5
.begin patch 1.7
CmpPrtNme Dim       18
.end patch 1.7
............................................................................
+...........................................................................
.
         liston
         MOVE      "Cash Adjustment Register" TO STITLE
         MOVE      "Ncsh0006 " TO PROGRAM
         MOVE      "Names in the News Ca" TO COMPNME
         CALL      PAINT
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         move      c1 to ncshpath
         CALL      FUNCDISP
.begin patch 1.7
          IF        (COMPANY = C2)
          Move      "Pacific Lists Inc",CmpPrtNme
          else
          MOVe      "Names in the News",CmpPRtNme
          endif
.end patch 1.7
          

         DISPLAY   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.

.
         clock     date to today
.
BEGIN    UNPACK    TODAY TO MM,STR1,DD,STR1,YY
.begin release 1.3
.         pack      datechck from mm,yy
.end release 1.3
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
         endset    month
         append    dd to month
         append    ", " to month
         append    cc to month
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
         move      inpname to ncshname
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
.         SPLOPEN   PRTFILE
         SPLOPEN   "\\nins2\laser2","R"
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START    
.
RDADJ    CALL      NCSHSEQ
         GOTO      EOJ IF OVER

         ADD       C1 TO count

         DISPLAY   *P15:08,count,b1,clr,b1,cnum

         compare   c1 to count
         if        equal
         move      cnum to holdc
         call      heading 
.begin release 1.3
         move      cce to cc
         move      cyr to yy
         move      cmo to mm
         move      cdy to dd
         call      cvtjul
         move      juldays to datechck
.end release 1.3
         endif
         type      clr 
         goto      rdadj if not equal
         packkey   ninvfld from clr
.         Move      clr to ninvfld
         rep       zfill in ninvfld
         move      c1 to ninvpath
         CaLL      Ninvkey
         goto      rdadj if over                .no adjustments
           
           move      "01" to str2
         pack      njstfld from invnum,str2
           rep       zfill in njstfld
         call      njstkey
         GOTO      rdadj IF OVER
;begin patch 1.5
;         move      invnum to holdinv
         move      invnum to holdinvNum
         goto      chkadj
looper   call      njstks         
         goto     rdadj if over
;         match     jstinvno to holdinv
         match     jstinvno to holdinvNum
;end patch 1.5
         goto      rdadj if not equal
         
chkadj   unpack    jstdate into cc,yy,mm,dd
.begin release 1.3
.         pack      str4 from mm,yy
.         match     datechck to str4
.         goto      rdadj if not equal
         call       cvtjul
.begin patch 1.4
.         move       datechck to n5
.         add        c3 to n5
.         sub        juldays from n5
.         if         (n5 >= 0 & n5 <= 3)
.         if         (juldays >= datechck-4 and juldays <= datechck+4)
         if         (juldays >= datechck-7 and juldays <= datechck+7)
.end patch 1.4
         goto       takeit
         else
         goto       looper               .try again
         endif
Takeit
.end release 1.3
         add        c1 to adjcnt
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
           unpack    jstdate into cc,yy,mm,dd,cc
           pack      str6 from mm,dd,yy
         EDIT      str6  TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
           unpack    jstinvdt into cc,yy,mm,dd
           pack      str6 from mm,dd,yy
         EDIT      str6  TO DATEPRT1
         EDIT      str6 TO DATEPRT2
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
         ADD       FAR TO R16
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
         ADD       FAR TO R22
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
         ADD       FAR TO R25AR
         GOTO      DTLPRT1
.
CHK26    MATCH     "26" TO JSTREASN
         GOTO      CHK27 IF NOT EQUAL
         ADD       LRINC TO R26
         GOTO      DTLPRT1
.
CHK27    MATCH     "27" TO JSTREASN
         GOTO      CHK30 IF NOT EQUAL
         ADD       FAr TO R27
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
.begin patch 1.6
         BRANCH    N2 OF BRK,LstM,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:
                  BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,LSTM,BRK:
                   BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK:
                   BRK,BRK,BRK,BRK,BRK
.         BRANCH    N2 OF BRK,BRK,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:
.                  BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,LSTM,BRK:
.                   BRK,BRK,BRK,BRK,BRK
.end patch 1.6
LSTM     ADD       LRINC TO TLRINC2
         GOTO      DTLPRT2
BRK      ADD       LRINC TO TLRINC1
                ELSE                       *RENTAL
.begin patch 1.6
         BRANCH    N2 OF BRK1,Lstm1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1
.         BRANCH    N2 OF BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
.                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1:
.                   BRK1,BRK1,BRK1,BRK1,BRK1
.end patch 1.6
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
                      *64,FAR:
                      *79,FAP1:
                      *89,LRINC:
                      *99,CTAX:
                      *108,STAX:
                      *118,POST:
                      *129,JSTREASN,JSTISTAT:
                   *N,*10,NAME2:
                      *79,FAP2:
                      *129,JSTCD:
                   *N,*10,O1DES
         ADD       C4 TO PRTLINES
.
         ADD       FAR     TO TAR
         ADD       FAP1    TO TAP1
         ADD       FAP2    TO TAP2
         ADD       LRINC  TO TLRINC
         ADD       STAX   TO TSTAX
         ADD       CTAX   TO TCTAX
         ADD       POST   TO TPOST
         MOVE      C0     TO FAR
         MOVE      C0     TO AP
         MOVE      C0     TO FAP1
         MOVE      C0     TO FAP2
         MOVE      C0     TO LRINC
         MOVE      C0     TO STAX
         MOVE      C0     TO CTAX
         MOVE      C0     TO POST
.
         GOTO      looper
.
EOJ     
         COMPARE   C45 TO PRTLINES
         CALL      HEADING IF NOT LESS
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
                   *N,*01,adjcnt,B1,"ADJUSTMENTS",*78,TAP:
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
                   *N,*01,"  LRINC REASON CODE 30 TOTAL: ",R30:
                   *N,*01,"  LRINC REASON CODE 31 TOTAL: ",R31:
                   *F
.                   *N,*01,"TOTAL LRINC LIST MAN. RENTAL: ",*88,TLRINC4:
.                   *N,*01,"TOTAL LRINC LIST MAN. XCHNGE: ",*88,TLRINC2:
         BRANCH    PRTFLAG TO END
         print     hpreset
         release
         SPLCLOSE
         shutdown
         stop
+............................................................................
.
HEADING  ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch,hpdupl,hptop,*f
         endif
.START PATCH 1.42 REPLACED LOGIC
.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Control Adjustment Register":
.                      *1,"Control ## ",holdc,*119,"PAGE:    ",PAGE:
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
.begin patch 1.7
                      *54,CmpPrtNme:
.                      *54,"NAMES IN THE NEWS":
.end patch 1.7
                      *119,"DATE: ",TODAY:
                   *N,*52,"Control Adjustment Register":
                      *1,"Control ## ",holdc,*119,"PAGE:    ",PAGE:
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
.END PATCH 1.42 REPLACED LOGIC
         MOVE      C8 TO PRTLINES
         RETURN
+............................................................................
.START PATCH 1.41 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.41 REPLACED LOGIC
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NORDIO.inc
         INCLUDE   NCSHio.inc

+
+
.begin patch 1.2
         INCLUDE   NJSTIO.inc    ADJUSTMENT DETAIL
.end patch 1.2
         INCLUDE   NJSTCLIO.inc  ADJUSTMENT DETAIL
+
;begin patch 1.5
;         include   ninvio.inc
          include             ninvio.inc
;end patch 1.5

         INCLUDE   COMLOGIC.inc


............................................................................
.
. PROGRAM    : NEOM0013
. DATE       : 11/01/91
. AUTHOR     : JOSE DUENAS
. DESCRIPTION: PRODUCES NIN PAYABLE STATEMENTS, PAID REPORTS.
.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NSLSDD.inc    SALES FILE
.begin patch 3.3
               INCLUDE        NCNTDD.inc
                    include   winapi.inc
.end patch 3.3
.patch3.23
                                        include   compdd.inc
                                        include   cntdd.inc
.         INC       NMLRDD.inc
.patch3.23
          INCLUDE             ninvdd.inc
.         INCLUDE   NINVDD.inc    INVOICE
         INCLUDE   NMTXDD.inc
         INCLUDE   NPAYDD.inc
.begin patch 3.0
         INCLUDE   NJSTDD.inc
.end patch 3.0
         INCLUDE   NOWNDD.inc    OWNER
         INCLUDE   NPGEDD.inc
         INCLUDE   NORDDD.INC
         include   hp.inc
.begin patch 3.3
          Include   PrtPagedd.inc
.end patch 3.3
.begin patch 3.6
          include   consacct.inc
         include   nmrgdd.inc
          Include   NInvAcddd.inc
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
          include ndat3dd.inc
mrgsw    dim       1
shipsw   dim       1
.end patch 3.6



OUTPUT   FILE      UNCOMP
PARFILE  FILE
POWN     DIM       4
SKEY     DIM       4
OWNBR    DIM       1
PAYCHK   FORM      1
STR25b   dim       25
Release   Init      "3.6"  DLH  .A/R version never accounted for prepayments or A/R adjustments - argh
.                                Note lots of code that could be cleaned up as it is redundant or wrong quite a hash :(      
Reldate   Init      "2013 August 07"
.Release   Init      "3.5"  DLH  Sunbelt PDF
.Reldate   Init      "2013 April 23"
.Release             Init      "3.45"  DMB  15August06  Modified Read Var for PDF995 ini to accomodate longer strings using taskname
.Release            Init      "3.44"  ASH  05June06  Adjusted Report titles
.Release            Init      "3.43"  DMS  25April06  Replaced reference to g:\ drive
.Release  Init      "3.43"    DMB  14APR06 Bug Fix during file open
.Release  Init      "3.42"    JD  01AUG05  Multiple owners per list. Owner break not printing correct owner company.
.Release  Init      "3.41"    ASH 13JUN2005 Work Order 782
.Release  Init      "3.40"    JD17May2005 Updated Manual Check processing.
.Release  Init      "3.31"    JD09May2005 Updated Hd3 to print payto name if exists
.Release  Init      "3.3"     DLH 14March2005 Convert to prtpage
.release  init      "3.25"         JD   11MAR2005 Patched up code for FUNC 5 to work. Statement style pay with AR.
.release  init      "3.24"        ASH   21DEC2004 PAGE.SRT Conversion
.release  init      "3.23"        DMB   26MAY2004 Mailer Conversion
.release  init      "3.22"          JD24Oct03 added manuals to combined total paid.
.release  init      "3.21"          JD28Dec01 Fixed  exch total.
.release  init      "3.20"          JD28Dec01 Fixed  exch total/ap2 correct printing
.release  init      "3.12"          JD  26Sep00  If ap2 move owner info headings.
.release  init      "3.1"          DLH 26Oct99  Create pcl form on the fly
.release  init      "3.0"          DLH 23Aug99  NINadj nadjust Y2K, File expansion
.release  init      "2.9"          ASH 27APR99  NININV Y2K, File expansion
.release  init      "2.8"          ASH 20JAN99  NINORD Y2K, File expansion
.RELEASE  INIT      "2.7"           JD 25NOV98 MAILER TAX STATUS NOTE.
.RELEASE  INIT      "2.6"          ASH05Oct98 NINPAY File expansion
.RELEASE  INIT      "2.5"          jd22apr98 forced page 1 on list break loinc, font chng payables.
.RELEASE  INIT      "2.4"         jd11nov97 page break fixed with list break.
.RELEASE  INIT      "2.3"         jd06dec96 break new page on list break
.RELEASE  INIT      "2.2"         jd07nov96 combined paid/payable report.
.RELEASE  INIT      "2.1"         jd23mar added if ap1 zero dont print on manual.
.RELEASE  INIT      "2.0"         DLH  23JUN94  Laser.
.RELEASE  INIT      "1.5"         JD  29JUL93  PRINTS MANUAL CHECKS.
.RELEASE  INIT      "1.4"        DLH 9MAR93   501C STATUS VAR CLEAR.
.RELEASE         INIT      "1.3"    DLH 20JAN93 TOTAL OWNER VARIABLE SIZE AND MASK FIX.
.
.RELEASE         INIT      "1.2"    30DEC92 ADDED PRINTING OF ALL OWNERS STATEMENT
.                           STYLE, PRINT POSITION ON AP'S MADE TO LINE UP.
.
.RELEASE         INIT      "1.1"    12/19/92 JDUENAS . ALL OPTION ADDED, MINOR PRINT
.                           POSITION FIXES.
.RELEASE INIT      "1.0"    JOSE DUENAS 11/1/91.
.                           INITIAL RELEASE
.
.begin patch 3.3
CountIn   FOrm      6
.Reset995Flag       Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
PDFFlag   Dim       1
PRTFLAG   Dim       1
HOTFLAG   FORM                1                 "1=regular print, 2=hot print"
userinfo  dim                 500
userlogn  dim       7
userlogw  dim                 7
userlogn2      dim            7
.timestamp1          dim       16
timestamp2          dim       16
.time1     form      16
.time2     form      16
.time3     form      16
BEGIN     FORM                2
LAST      FORM                2
.hexeight           integer 4,"4294967295"
.end patch 3.3
APCHECK   FORM      "000000001"
APSW    DIM       1
AP2SW    DIM       1
FINISH   DIM       1
DIM1     DIM       1
TAX501   FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
DATE     DIM       8
LISTCHG  DIM       1
.HUND     FORM      "100"
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
R1       INIT      "STATEMENT STYLE PAYABLES"
R2       INIT      "   INHOUSE PAYABLE REPORT  "
R3       INIT      "NIN PAID REPORT"
R4       INIT      "   STATEMENT STYLE ALL  "
R5       INIT      "Nin Payables Report WITH AR    "
R6       INIT      "Nin Paid Report WITH AR        "
R7       INIT      "COMBINED STYLE        "
RPTTYPE  DIM       26
MON1     INIT      " JANUARY 20?? "
MON2     INIT      "FEBRUARY 20?? "
MON3     INIT      "  MARCH 20??  "
MON4     INIT      "  APRIL 20??  "
MON5     INIT      "   MAY 20??   "
MON6     INIT      "  JUNE 20??   "
MON7     INIT      "  JULY 20??   "
MON8     INIT      " AUGUST 20??  "
MON9     INIT      "SEPTEMBER 20??"
MON10    INIT      " OCTOBER 20?? "
MON11    INIT      "NOVEMBER 20?? "
MON12    INIT      "DECEMBER 20?? "
MONTH    DIM       14
SYSDAYS  FORM      5              AGEING DATE CONVERTED TO JULIAN
HOLDMLR  INIT      "    "
HOLDCNT  INIT      "   "
PAYKEY   DIM       5
MTXKEY   DIM       4
CHKDATE  DIM       10
CBLSTNUM FORM      6
CBOWNNUM FORM      4
APTOTDET FORM      10.2
TOTACPAY FORM      10.2
APTOTLST FORM      10.2
APTOTLSE FORM      10.2
APTOTLSR FORM      10.2
ARTOTLST form      10.2
APTOTOWN FORM      10.2      DLH
appaid   FORM      10.2       DLH
APopen   FORM      10.2       DLH
APOWNER  FORM      10.2
APGRAND  FORM      10.2
APONETOT FORM      10.2
GRAP     FORM      10.2
GRAP1    FORM      10.2
GRAP2    FORM      10.2
GRAPMSK  DIM       17
GRAP1MSK DIM       17
GRAP2MSK DIM       17
AP1OUT   DIM        17
adjap    form      10.2
form92a   form      10.2
form72a   form      10.2
manpay   form      10.2
aplist   form      10.2
JSTN     DIM       2
AP1FORM  FORM      10.2
arform   form      10.2
arout    dim       17
aplotus  dim       17
AP2OUT   DIM       17
TOTOUT   DIM       17
LSTMASK  DIM       17
manMASK  DIM       17
LSTMASKE DIM       17
LSTMASKR DIM       17
ARMASK   dim       17
OWNERMSK DIM       17
APTOTMSK DIM       17
TOTACMSK DIM       17
APTWOTOT FORM      10.2
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
.Start patch #2.8 - increased var
.DATEMASK INIT      "99/99/99"
.DATEPRT1 DIM       8
.DATEPRT2 DIM       8
DATEMASK INIT      "99/99/9999"         .Won't affect DATEPRT2 or DATEPRT3!!
DATEPRT1 DIM       10
DATEPRT2 DIM       10
.End patch #2.8 - increased var
DATEPRT3 DIM       10
INVDATE  FORM      5
C37      FORM      "37"
C38      FORM      "38"
c46      FORM      "46"
C48      FORM      "48"
C52      FORM      "52"
C56      FORM      "56"
C54      FORM      "54"
C12      FORM      "12"
TAXPRT   INIT      "       "
FIRST    INIT      "Y"
PAYTWO   DIM       25
.Start patch #2.6 - var expanded to reflect NINPAY expansion
.PAYONE   DIM       25
PAYONE   DIM       45
.End patch #2.6 - var expanded to reflect NINPAY expansion
APMASK   INIT      "$,$$$,$$$,$$9.99-"
TOTMASK  INIT      "$,$$$,$$$,$$9.99-"
TOTOMSK  INIT      "$,$$$,$$$,$$9.99-"
manmsk  INIT      "$,$$$,$$$,$$9.99-"
GRDMASK  INIT      "$,$$$,$$$,$$9.99-"
LNAME17  DIM       17
ltot     dim       1
TIPE     DIM       1
.START PATCH 3.24 ADDED LOGIC
HoldMlrKey          dim       6
HoldBrkKey          dim       25
.END PATCH 3.24 ADDED LOGIC
.START PATCH 3.44 REMOVED LOGIC
.NINLogo  PICT
.         CREATE    NINLogo=3:13:30:50:
.                   "\\nts0\c\netutils\NIN logo black outline.jpg"
.END PATCH 3.44 REMOVED LOGIC

............................................................................
+...........................................................................
.
.
         MOVE      FUNC TO FUNCBR
         MATCH     "        " TO TODAY
         IF        EQUAL
         GOTO      GETDATE
         ELSE
         IF        EOS
         GOTO      GETDATE
         ENDIF
         ENDIF
         GOTO      BEGIN
.
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "99/99/99" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
BEGIN    CMATCH    B1 TO PROGRAM
         IF        EOS
          MOVE      "NEOM0013 " TO PROGRAM
          MOVE      LOCAL TO PRTNAME
.begin patch 3.3
          MOVE      C1 TO hotflag
.end patch 3.3

DATEGET  DISPLAY   *P15:06,MM,SLASH,DD,SLASH,YY
         KEYIN     *P1:24,*EL,"DATE OK ? ",*T10,STR1;
         CMATCH    NO TO STR1
         GOTO      DATEBAD IF EQUAL
         ENDIF
          move      user,userlogn

         MOVE      "Payables/Paid" TO STITLE
         MOVE      INPNAME to nslsname
         CALL      PAINT
         MOVE      C1 TO NINVPATH
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
.
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
.
         DISPLAY   *P01:06,"Ageing Date : ":
                   *P01:07,"Report Type : ":
                   *P01:08,"Input File  : ":
                   *P01:10,"Print File  : ":
                   *P01:11,"Input Count : ":
                   *P01:12,"Invoice Date: ":
                   *P01:13,"Index File  : "
.
         UNPACK    TODAY TO MM,STR1,DD,STR1,YY
.
         CALL      DATETEST
         BRANCH    DATEFLAG TO DATENG
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSDAYS
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM MON1,MON2,MON3,MON4,MON5,MON6:
                   MON7,MON8,MON9,MON10,MON11,MON12
         SCAN      "??" IN MONTH
         GOTO      PRTGET IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
         GOTO      TYPEGET
DATEBAD  KEYIN     *P01:24,*EL,"The ageing date is invalid.":
                   *P15:06,*DV,*HON,MM,*DV,SLASH,*DV,DD,*DV,SLASH,*DV,YY:
                   *P15:06,*RV,*+,*JR,MM:
                   *P18:06,*RV,*+,*JR,DD:
                   *P21:06,*RV,*-,*JR,YY,*HOFF;
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         GOTO      DATEGET
.
TYPEGET  COMPARE   C0 TO FUNCBR
         GOTO      TYPENG IF LESS
         GOTO      TYPENG IF EQUAL
         COMPARE   FUNCBR TO C7
         GOTO      TYPENG IF LESS
         LOAD      RPTTYPE USING FUNCBR FROM R1,R2,R3,R4,R5,r6,r7
         DISPLAY   *P15:07,FUNCBR,SLASH,RPTTYPE
         COMPARE   C8 TO FUNCBR
         IF        EQUAL
         DISPLAY   *P15:13,"UNKOWN REPORT TYPE",*B,*B,*B,*B,*W4,*B,*B
         STOP
         ENDIF
.begin patch 3.3

          clock               port to str3           .plb note
          unpack              str3 into str2,str1
          pack                str3 from str1,str2
          move                str3 to portN
          move      PORTN,NCNTFLD1
          rep       zfill,NCNTFLD1
          move      C3,NCNTPATH
          move      "NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD1
          call      NCNTKEY
          if        over
                    move      C2,CNTPRINT    .Laser 3
          endif
          move      CNTPRINT,PRTFLAG
          call      Trim using CNTNAME
          if        (CNTNAME <> "")
                    move      CNTNAME,str1
                    scan      B1,CNTNAME
                    if equal
                              bump      CNTNAME,1
                              call      RemoveChar using CNTNAME,B1
                    endif
                    pack      str6,CNTNAME
                    clear     userlogn2
                    pack      userlogn2,str1,str6
          endif

          reset     CNTNAME
          move      C1,NCNTPATH
                Call           GetWinVer
               move           "USER" to userinfo
               clock          env,userinfo
               scan           "LOGIN" in  userinfo
               if             equal
                    bump           userinfo by 6
                    clear          userlogw
                    move           userinfo,userlogw
                    MOVEFPTR       userlogw TO BEGIN
                    SCAN           "," IN userlogw
                              if             equal
                                        MOVEFPTR       userlogw TO LAST
                                        SUB            C3 FROM LAST
                                        RESET          userlogw
                                        SETLPTR        userlogw TO LAST
                                        clear          userlogn
                                        APPEND         userlogw TO userlogn
                                        reset          userlogn
                              endif
               endif
          Move      c1 to hotflag
          scan      "PDF" in comment
          If        equal
          MOve      c2 to hotflag
          MOve      "P" to prtflag
          endif
.end patch 3.3

.
           COMPARE   FUNCBR TO C2
           IF        EQUAL
           MOVE      C2 TO INDNUM
           ENDIF
.
         CLEAR     NPGENAME
         APPEND    "PAGE" TO NPGENAME
         APPEND    INDNUM TO NPGENAME
         RESET     NPGENAME
         REP       ZFILL IN NPGENAME
         DISPLAY   *P15:13,NPGENAME
.
         GOTO      INPGET
TYPENG   KEYIN     *P01:24,*EL,"The Report Type is invalid.":
                   *P15:07,FUNCBR;
         GOTO      TYPEGET
.
INPGET
         COMPARE   C3 TO FUNCBR
         IF        EQUAL
         MOVE      INPNAME to nslsname
         MOVE      C1 TO NSLSPATH
         ENDIF

         COMPARE   C6 TO FUNCBR
         IF        EQUAL
         MOVE      INPNAME to nslsname
         MOVE      C1 TO NSLSPATH
         ENDIF

          IF        (Funcbr = c1 or funcbr = c5)
          MOVE                C2 TO NSLSPATH
          ELSE
          MOVE                C1 TO NSLSPATH
          ENDIF

         COMPARE    C5 TO FUNCBR
         IF         EQUAL
.         MOVE      "NINSLS" TO INPNAME    turned off JD. 03/30/05.
.PATCH 3.25
         MOVE      INPNAME to nslsname
.PATCH 3.25
.         MOVE       C1 TO NSLSPATH
.START PATCH        3.43      REPLACED LOGIC
.         prepare    output,"g:\data\ninsls.tmp"
          pack taskname, NTWKPATH1,"ninsls.tmp"
          prepare   output, taskname
.END PATCH          3.43      REPLACED LOGIC
         ENDIF
         DISPLAY   *P15:08,INPNAME
         BRANCH    FUNCBR OF PRTGET,PAGE,PRTGET,PRTGET,PRTGET,prtget,prtget
Page
.START PATCH 3.24 REPLACED LOGIC
.     PREPARE   NPGEFILE,NPGENAME
.         MOVE      C1 TO NPGEFLAG
          PREPARE   NPGEFLE2,NPGENAME,"\\nins1\E\DATA\INDEX\page","3-8","66"
          MOVE      C1 TO NPGEFLG2
.END PATCH 3.24 REPLACED LOGIC
         GOTO      PRTGET
INPNG    NORETURN
         KEYIN     *P01:24,*EL,"The Input File is not on-line. ":
                   *P15:08,INPNAME;
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
.begin patch 3.3
         MATCH     LOCAL TO PRTNAME
         GOTO      OMIT IF EQUAL
         PACK      PRTFILE FROM PRTNAME
         PACK      str8 FROM "PAY05"
                              clear     str25b
                              move      prtname to str25B
          call      PrtOpenPrep
prtok    DISPLAY   *P15:10,PRTFILE
         GOTO      OMIT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:10,PRTNAME;
         GOTO      PRTGET
.
. THE 'NNOSTMNT' ENTRY IN THE TABLE FILE CONTAINS MAILER NUMBERS THAT DO NOT
. NEED STATEMENT CARDS TO BE PRINTED.
.

OMIT     DISPLAY   *P1:24,*EL;
         OPEN      PARFILE,"PAREOM"
.DH testing 2015 June 29
.START    BRANCH    FUNCBR TO PARFILE,INHOUSE,INHOUSE,Parfile,Parfile,inhouse:
START    BRANCH    FUNCBR TO PARFILE,INHOUSE,INHOUSE,InHouse,Parfile,inhouse:
                   inhouse
.
INHOUSE
         CALL      NSLSSEQ
         GOTO     TOTALS IF OVER
.         ADD       C1 TO Countin
         if         (slslr = "559262" or slslr = "546426" or slslr = "568034" or slslr = "565427")
         Move       "P" to slscode
         call       debug
         endif
          if        (slslr = "768225")
          call      debug
          endif

DETAIL
           clear     tipe
         BRANCH    FUNCBR OF PROCES1,PROCES2,PROCES2,PROCES1,PROCES1,proces2:
                   proces2
PROCES2

         ADD       C1 TO Countin
         MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         MOVE      OWNLONM TO INDNAME
         MOVE      OWNOCPY TO INDCOMP
         MATCH    YES TO FIRST
         CALL      BREAK IF EQUAL
PROCES1
.         ADD       C1 TO Countin
         DISPLAY   *P15:11,"RECORDS PROCESSED ",Countin

.         compare    "484150" to slslr
.         call       debug if equal
         COMPARE    SLSOWN TO CBOWNNUM
.         CALL      TOTALOWN IF NOT EQUAL
.Start patch #3.42
         if         not equal
         MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         CALL      READPAY
                              call      totalown
                              endif
.End patch #3.42
         COMPARE   SLSLIST TO CBLSTNUM
         CALL      TOTLIST IF NOT EQUAL
         COMPARE   C0 TO SLSAP2
        IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE
         MOVE     YES TO AP2SW
         ENDIF
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
.begin patch 3.6
          IF        (FUNCBR = C6  or funcbr = c5)               .a/r INCLUDED
          if        (slslr = "768225")
          call      debug
          endif
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         packkey    nownfld from slsown
          rep       zfill,nownfld
         CALL      NOWNKEY
.
.
         MOVE      NO TO SUBPPSW
         MOVE      SLSLR to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         MOVE      SLSLR to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
         call      wipecvars
         move      c1 to ndatpath
         move      SLSLIST to ndatfld
         call      ndatkey
         move      SLSLR to nshpfld
         call      nshpkey
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
          Move      Yes,FrcCompFlag
         CALL      COMPUTE
          if        (slslr = "768225")
          call      debug
          endif

         move      c0 to slsar
         move      formar to slsar
          move      c1,n2
ARADJ    move      n2 to jstn
         clear     njstfld
         rep       zfill in jstn
          PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      ARADJOUT IF OVER
.begin patch 3.6
          if        (JSTREASN <> "16")           .dont sub out shortpay
          Add       JstAr to slsar
          endif
         ADD       C1 TO n2
          goto      ARADJ
         endif
ARADJOUT
.end patch 3.6




         CALL      READPAY
         PACK      MKEY FROM SLSMLR,Z3
         REP       " 0" IN MKEY
         CALL      NMLRKEY
         IF        OVER
         PACK      MKEY FROM Z3,Z3,Z3
         CALL      NMLRKEY
         ENDIF
         CALL      READTAX

.
         MOVE      DATEMASK TO DATEPRT1
.Start patch #2.8 - increased var
.         EDIT      SLSMDTE  TO DATEPRT1
.         MOVE      DATEMASK TO DATEPRT2
.         EDIT      SLSIDTE TO DATEPRT2
         CLEAR     STR8
         UNPACK    SLSMDTE TO STR4,STR4
         APPEND    STR4,STR8
         UNPACK    SLSMDTE TO STR4
         APPEND    STR4,STR8
         RESET     STR8
         EDIT      STR8  TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         CLEAR     STR8
         UNPACK    SLSIDTE TO STR4,STR4
         APPEND    STR4,STR8
         UNPACK    SLSIDTE TO STR4
         APPEND    STR4,STR8
         RESET     STR8
         EDIT      STR8  TO DATEPRT2
.End patch #2.8 - increased var
.begin patch 2.9
.         PACK      CHKDATE FROM CHKDTEM,SLASH,CHKDTED,SLASH,CHKDTEY
         PACK      CHKDATE FROM CHK1DTEM,SLASH,CHK1DTED,SLASH,CHK1DTEY
.end patch 2.9
         move      c0  to invdate
.Start patch #2.8 - increased var
.         unpack    slsidte into mm,dd,yy
         unpack    slsidte into STR2,YY,mm,dd
.END patch #2.8 - increased var
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO INVDATE
.

         BRANCH    FUNCBR OF DT1,DT1,DT1,DT1,DT1,dt1,dt1
DT1
         MOVE      SLSAP1 TO AP1FORM
         MOVE      YES TO APSW
         COMPARE   APCHECK TO AP1form
         IF          NOT GREATER
         MOVE      NO TO APSW
         ENDIF
        MOVE    APMASK TO AP1OUT
        EDIT    AP1FORM TO AP1OUT
        ADD     SLSAP1 TO APTOTDET
         BRANCH    FUNCBR OF ADDAPA,ADDAPB,ADDAPA,ADDAPA,ADDAPA,addapa,addapa
ADDAPB
        CLEAR    TIPE
        ADD     SLSAP1 TO APTOTLST
        ADD     SLSAP1 TO TOTACPAY
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         IF        EQUAL
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9
.         IF        EQUAL
         GOTO      SPLITER IF NOT EQUAL
.         add       slsap1 to aptotlse
         MOVE      "E" TO TIPE
         goto      addapa
         endif
.         endif
.Start patch 3.20
SPLITER  match     yes to ap2sw
         if        not equal
         add       slsap1 to aptotlsR
         add       slsap1 to aptotown
         goto      funchk7
         else
         add       slsap2 to aptotlsr
         add       slsap2 to aptotown
         MOVE      b1 TO TIPE
         goto      funchk7
         endif
ADDAPA MATCH   YES TO AP2SW
        IF      EQUAL
        ADD     SLSAP2 TO APTOTOWN
.        move    c0 to aptotlse
        add     slsap2 to aptotlse
        ELSE
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
        ADD     SLSAP1 TO APTOTOWN
.        move    c0 to aptotlse
        add     slsap1 to aptotlse
         ENDIF
        ENDIF
funchk7
.End patch 3.20
        compare   c7 to funcbr
        if      equal
        match   "P" to slscode
        if      equal
        add     slsap1 to appaid
        add     slsap2 to appaid
        else
        add     slsap1 to apopen
        add     slsap2 to apopen
        endif
        endif
        MATCH   NO TO AP2SW
        IF     EQUAL
        MOVE    TOTMASK TO AP2OUT
        EDIT    SLSAP2 TO AP2OUT
        ELSE
        MOVE    TOTMASK TO AP2OUT
        EDIT    SLSAP2 TO AP2OUT
        MOVE    YES TO AP2SW
        ENDIF
         BRANCH    FUNCBR OF ADDAPC,ADDAPD,ADDAPC,ADDAPC,ADDAPC,addapc,addapc
ADDAPD  ADD     SLSAP2 TO TOTACPAY
        ADD     SLSAP2 TO APTOTDET
        ADD     SLSAP2 TO APTOTLST
         GOTO      GRAND
ADDAPC
         MATCH     YES TO AP2SW
         IF        EQUAL
         ADD       SLSAP2 TO APTOTLST
         ELSE
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         ADD       SLSAP1 TO APTOTLST
         ENDIF
         ENDIF
GRAND    ADD       SLSAP1       TO GRAP1
        ADD       SLSAP1       TO GRAP
        ADD       SLSAP2      TO GRAP2
        ADD       SLSAP2      TO GRAP
         MOVE      TOTMASK TO TOTOUT
         EDIT      APTOTDET TO TOTOUT
         CMATCH    YES TO LISTCHG
         IF        EQUAL
         GOTO      MODE
         ELSE
         MOVE      "                                   " TO SLSLST1
         ENDIF
MODE     BRANCH    FUNCBR OF PRT1,PRT2,PRT3,PRT1,PRT5,prt3,prt3
PRT2
INHPRT
.begin patch 3.3
          If        (Row > 7100)
          call      Heading
          endif
        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg85,mcomp:
                    *p=2125:row,slslr:
                              *p=2725:row,Invnum:
                              *p=3305:row,DatePrt2:
                              *p=3930:row,DatePrt1:
                              *p=4625:row,loinvn:
                    *p=6550:row,taxprt;
         MATCH     YES TO AP2SW
         IF        EQUAL
         PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
         move      ap2out to aplotus
         ELSE
          PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
         move      ap1out to aplotus
         ENDIF
.PrtPage  Laser;*p=125:row,*font=prtpg85,Mcomp:
.         *p=2125:row,slslr:
.         *p=2725:row,INVnum:
.         *p=3375:row,dateprt1:
.         *p=4625:row,Loinvn:
.         *p=9950:row,taxprt
.        add            SixLpi to row
         add            SixLpi to row
.end patch 3.3
         MOVE      C1 TO n2
         MOVE      SLSLR TO NINVFLD
         CALL      NINVKEY

DETADJIN move      n2 to jstn
         clear     njstfld
         rep       zfill in jstn
          PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      NOMAN2 IF OVER
         MATCH     "14" TO JSTREASN
         ADD       C1 TO n2
         GOTO      DETADJIN


        move       c0 to adjap
         add       jstap1 to adjap

        move       apmask to ap1out
        edit       adjap to ap1out
        move       c0 to form92a
         move       jstap2 to form92a
        move       c0 to form72a
       add        form92a to form72a
        move       apmask to ap2out
        edit       form92a to ap2out
         MOVE      DATEMASK TO DATEPRT3
         CLEAR     STR8
         UNPACK    jstdate TO STR4,STR4
         APPEND    STR4,STR8
         UNPACK    jstdate TO STR4
         APPEND    STR4,STR8
         RESET     STR8
         MOVE      DATEMASK TO DATEPRT3
         EDIT      STR8  TO DATEPRT3
.begin patch 3.3
          IF        (row > 7100)
          call      Heading
          endif
          PrtPage   Laser;*p=125:row,*font=prtpg85,Mcomp:
                    *p=2125:row,slslr:
                    *p=2725:row,INVnum:
                    *p=3375:row,dateprt1:
                    *p=4625:row,Loinvn:
                    *p=6550:row,taxprt

        COMPARE     c0 to form72a
         IF        NOT EQUAL
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left:
                              *p=6725:row,Dateprt3,*p=7725:row,"*Manual"
         add       form72a TO manpay
         ELSE
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left:
                              *p=6725:row,Dateprt3,*p=7725:row,"*Manual"
         add       adjap to manpay
         ENDIF
.end patch 3.3
NOMAN2
         MOVE      C0 TO APTOTDET
.begin patch 3.3
.         ADD       C2 TO PRTLINES
.         add            SixLpi to row            JD 5/17/05.
         add            SixLpi to row
.end patch 3.3
         GOTO      INHOUSE
.
PRT1
         cmatch    yes to apsw
         goto      readks if not equal
         MATCH     YES TO AP2SW
         IF         EQUAL
         GOTO      GOODONE
         ELSE
         COMPARE    C0 TO PAYCHK
         IF        NOT EQUAL
         GOTO      READKS
         ENDIF
         ENDIF
GOODONE
.begin patch 3.3

          if        (row > 7100)
          call                Heading
          endif
          clear               str40
          move                mcomp to str40

.
.
        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg85,str40:
                    *p=2125:row,slslr:
                              *p=2725:row,Invnum:
                              *p=3305:row,DatePrt2:
                              *p=3930:row,DatePrt1:
                              *p=4625:row,loinvn:
.                              *p=4000:row,loinvn:
                    *p=6550:row,taxprt;
         MATCH     YES TO AP2SW
         IF        EQUAL
         PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
         move      ap2out to aplotus
         ELSE
          PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
         move      ap1out to aplotus
         ENDIF
.end patch 3.3
         COMPARE     C5 TO FUNCBR
         IF          EQUAL
         MOVE        SLSLST1 TO STR25
         move        c0 to arform
         move        slsar to arform
         move        apmask to arout
         add         arform to artotlst
         EDIT        arform TO AROUT
         WRITE     OUTPUT,SEQ;B6,mcomp:
                      b1,slslr:
                      b1,invnum:
                      b1,dateprt2:
                      b1,loinvn:
                      b1,aplotus:
                      b1,str25:
                      b1:
                      arout:
                      taxprt
         ENDIF
          add       sixlpi to row
         GOTO      READKS
.
PRT5
         cmatch    yes to apsw
         goto      readks if not equal
         MATCH     YES TO AP2SW
         IF         EQUAL
         GOTO      GOODONE5
         ELSE
         COMPARE    C0 TO PAYCHK
         IF        NOT EQUAL
         GOTO      READKS
         ENDIF
         ENDIF
GOODONE5

          if        (row > 7100)
          call                Heading
          endif
          clear               str40
          move                mcomp to str40

.
.
        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg85,str40:
                    *p=2125:row,slslr:
                              *p=2725:row,Invnum:
                              *p=3305:row,DatePrt2:
                              *p=3930:row,DatePrt1:
.                              *p=4000:row,loinvn:
                              *p=4625:row,loinvn:
                    *p=10400:row,taxprt;
         MATCH     YES TO AP2SW
         IF        EQUAL
         PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
         move      ap2out to aplotus
         ELSE
          PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
         move      ap1out to aplotus
         ENDIF
          if        (chkdate = "  /  /    " or chkdate = "  /  /  ")
          clear     chkdate
          endif
          move                c0 to arform
          move                slsar to arform
          move                apmask to arout
          add                 arform to artotlst
          EDIT                arform TO AROUT
          PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1,*p=8725:row,arout
         MOVE        SLSLST1 TO STR25
         move        c0 to arform
         move        slsar to arform
         move        apmask to arout
.begin patch            - why you just added above!!!!!!!!!!!
.         add         arform to artotlst
         EDIT        arform TO AROUT
         WRITE     OUTPUT,SEQ;B6,mcomp:
                      b1,slslr:
                      b1,invnum:
                      b1,dateprt2:
                      b1,loinvn:
                      b1,aplotus:
                      b1,str25:
                      b1:
                      arout:
                      taxprt
          add       sixlpi to row
         GOTO      READKS



PRT3
          MATCH     YES TO AP2SW
         IF         EQUAL
         GOTO      GOODONE2
         ELSE
         COMPARE    C0 TO PAYCHK
         IF         NOT EQUAL
         GOTO      INHOUSE                     .sales read
         ENDIF
         ENDIF
GOODONE2
          IF        (apsw <> no)
.         match      no to apsw
.         goto       prt3a2 if equal
.begin patch 3.3
                              IF        (row > 7100)
                    call      Heading
                    endif
           PrtPage  Laser;*p=125:row,*font=prtpg85,Mcomp:
                    *p=2125:row,slslr:
                    *p=2725:row,INVnum:
                    *p=3305:row,dateprt2:
                    *p=3930:row,dateprt1:
.                   *p=4625:row,Loinvn:
                    *p=4625:row,Loinvn:
                    *ALIGNMENT=*RIGHT:
                    *p=10400:row,taxprt,*ALIGNMENT=*Left
                    MATCH     YES TO AP2SW
                              IF        EQUAL
                              PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
                              MOVE      CHKN2 TO CHKN1
                              ELSE
                              PrtPage               Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
.end patch 3.3
                              endif
          MATCH     YES TO AP2SW
                              IF        EQUAL
                              move      ap2out to ap1out
                              endif
.
                              if        (chkdate = "  /  /    " or chkdate = "  /  /  ")
                              clear     chkdate
                              endif
.
                              IF        (funcbr = c6)
                              move                c0 to arform
                              move                slsar to arform
                              move                apmask to arout
                              add                 arform to artotlst
                              EDIT                arform TO AROUT
                              PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1,*p=8725:row,arout
                              Else
                              PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1
                              Endif
          endif
          add       sixlpi to row
          move      "01" to n2
          MOVE      SLSLR TO NINVFLD
          CALL      NINVKEY


DETADJ2
         move      n2 to jstn
         rep       zfill in jstn
         CLEAR     NJSTFLD
         PACK      NJSTFLD FROM INVNUM,JSTN
         rep        zfill in njstfld
         CALL      NJSTKEY
         GOTO      INHOUSE IF OVER
adjhit
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO n2
         GOTO      DETADJ2
         ENDIF
         IF        NOT EQUAL
         ADD       C1 TO n2
         GOTO      DETADJ2
         ENDIF
         move      jstap1 to form92a
        move       c0 to adjap
        add        form92a to adjap
        mult       seq by adjap
        match     "O" to slscode
        if         equal
        add        adjap to apopen
        endif
        move       apmask to ap1out
        edit       adjap to ap1out
        CMATCH     YES TO AP2SW
        IF          EQUAL
        GOTO       CONVAP2
        ENDIF
        COMPARE    C0 TO ADJAP
        IF         NOT EQUAL
        ADD        ADJAP TO MANPAY
        move       adjap to aplist
        add        aplist to aptotlst
.begin patch 3.4
        add        aplist to appaid                  added 5/17/05.
.begin patch 3.4
        ENDIF
CONVAP2
        move        jstap2 to form92a
        move       c0 to form72a
        add        form92a to form72a
        move       apmask to ap2out
        edit       form72a to ap2out
        cmatch     yes to ap2sw
        if         equal
        compare    c0 to form72a
        goto       inhouse if equal
        endif
        ADD        form72a TO MANPAY
        move       c0 to adjap
        add        form72a to adjap
        COMPARE    C0 TO ADJAP
        IF         NOT EQUAL
        mult       seq by adjap
        move       adjap to aplist
        add        aplist to aptotlst
.begin patch 3.4
        add        aplist to appaid                                                added 5/17/05.
.begin patch 3.4
        ENDIF
         match     "O" to slscode
         goto      paypart if equal
.begin patch 3.4
           PrtPage  Laser;*p=125:row,*font=prtpg85,Mcomp:
                    *p=2125:row,slslr:
                    *p=2725:row,INVnum:
                    *p=3305:row,dateprt2:
                    *p=3930:row,dateprt1:
.                   *p=4625:row,Loinvn:
                    *p=4625:row,Loinvn;
.                   *ALIGNMENT=*RIGHT:
.                   *p=10400:row,taxprt,*ALIGNMENT=*Left
.begin patch 3.4
         COMPARE   C0 TO form72a
         IF        NOT EQUAL
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
         MOVE      "MANUAL" TO CHKN1
         ELSE
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
         ENDIF
         MOVE      "MANUAL" TO CHKN1
         MOVE      DATEMASK TO DATEPRT3
         CLEAR     STR8
         UNPACK    jstdate TO STR4,STR4
         APPEND    STR4,STR8
         UNPACK    jstdate TO STR4
         APPEND    STR4,STR8
         RESET     STR8
         MOVE      DATEMASK TO DATEPRT3
         EDIT      STR8  TO DATEPRT3
         clear     chkdate
         move      dateprt3 to chkdate
         COMPARE     C6 TO FUNCBR
         goto        its6b if equal
         goto        not6b
.         IF          EQUAL
its6b
         move        c0 to arform
         move        slsar to arform
         move        apmask to arout
         add         arform to artotlst
         EDIT        arform TO AROUT
          PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1,*p=8725:row,arout
         goto        prt3b
not6b
          PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1
prt3b
          add       sixlpi to row
         GOTO      INHOUSE
paypart
                              IF        (row > 7100)
                    call      Heading
                    endif
           PrtPage  Laser;*p=125:row,*font=prtpg85,Mcomp:
                    *p=2125:row,slslr:
                    *p=2725:row,INVnum:
                    *p=3305:row,dateprt2:
                    *p=3930:row,dateprt1:
                    *p=4625:row,Loinvn:
                    *ALIGNMENT=*RIGHT:
                    *p=10400:row,taxprt,*ALIGNMENT=*Left
         move      "OPEN" to chkn1
         MATCH     YES TO AP2SW
         IF        EQUAL
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP2Out,*ALIGNMENT=*Left
         ELSE
          PrtPage   Laser;*p=6200:row,*ALIGNMENT=*RIGHT,AP1Out,*ALIGNMENT=*Left
         endif
          PRtPage   Laser;*p=6725:row,chkdate,*p=7725:row,chkn1
          add       sixlpi to row
         GOTO      INHOUSE
.
DTL2      IF        (row > 7100)
          call      Heading
          endif
          add       sixlpi to row
          GOTO      START
.
DTL3      IF        (row > 7100)
          call      Heading
          endif
          add       sixlpi to row
          GOTO      START

READPAY  MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      PAYTN TO DIM1
         MOVE      PAYTN TO PAYCHK
         PACK      PAYKEY FROM NOWNFLD,DIM1
         REP       ZFILL IN PAYKEY
         MOVE      PAYKEY TO NPAYFLD
         REP       ZFILL IN NPAYFLD
         CLEAR     PCOMP                   *PCBUS DOES NOT CLEAR ON OVER.
         CLEAR      PNAME
         CLEAR      PSTREET
         CLEAR      PCITY
         CLEAR      PSTATE
         CLEAR      PZIP
         CALL      NPAYKEY
         CALL      NOWNKEY
APQUES   MATCH    NO TO AP2SW
         IF        EQUAL
         MOVE      "                   " TO PAYTWO
         ELSE
         MOVE      OWNOCPY TO PAYTWO
         ENDIF
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        NOT EQUAL
         BRANCH    FUNCBR OF STETYPE
         ENDIF
         MOVE      PCOMP TO PAYONE
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ELSE
STETYPE  MOVE      OWNOCPY TO PAYONE
         ENDIF
         RETURN
READTAX
.         MOVE      SLSMLR TO MTXKEY
         CLEAR     TAXPRT
         MOVE      C0 TO MTXC501
         MOVE      compnum TO nmtxfld
         REP       ZFILL IN nmtxfld
         CALL      NMTXKEY
         MOVE      C0 TO TAX501
         MOVE      MTXC501,TAX501
         BRANCH    TAX501 OF C0,C0,C3,C4,C5,C6
C0
         RETURN
C3       MOVE      "501C-3" TO TAXPRT
         RETURN
C4       MOVE      "501C-4" TO TAXPRT
         RETURN
C5       MOVE      "501C-5" TO TAXPRT
         RETURN
C6       MOVE      "501C-6" TO TAXPRT
         RETURN
.
.
. OWNER TOTAL.
.
.
.
.
EXIT
.         SPLCLOSE
          prtclose   laser
          if        (PRTFLAG = "p")     .PDF
.         pause   "5"
.         call      CreatePDFFile
          endif

          shutdown  "cls"
..         STOP
exit1
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
.         SPLCLOSE
         shutdown   "cls"
.         STOP
.

EOJ     BRANCH     funcbr TO Exit,exit,exit,exit,EXIT,exit,exit
inhous  prtclose     laser
         CALL      REMVTOF
.START PATCH 3.24 REMOVED LOGIC
.         CALL      NPGEEOF
.END PATCH 3.24 REMOVED LOGIC
         GOTO      END
.
+...........................................................................
.
HEADING  ADD       C1 TO PAGE
.START PATCH 3.41 ADDED LOGIC
          if (SLSLIST > 0)
                    move      SLSLIST,str6
                    rep       zfill,str6
                    pack      str9,str6," - "
          else
                    clear     str9
          endif
.END PATCH 3.41 ADDED LOGIC
.         BRANCH    FUNCBR TO HD1,HD2,HD3,HD1,HD1,hd6,hd3
.DH test
         BRANCH    FUNCBR TO HD1,HD2,HD3,HD1,HD6,hd6,hd3
         RETURN
HD1
.Start patch 3.12
         cmatch    yes to ap2sw
         if        equal
         MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         call      nownkey
         move      ownocpy to payone
         endif
.End patch 3.12
.
         MATCH     YES TO OWNBR
         IF        EQUAL
         MOVE      C1 TO PAGE
         ELSE
         MATCH    YES TO ltot
         IF       EQUAL
         MOVE     C1 TO PAGE
         ENDIF
         ENDIF
         move     no to ltot
          IF        (Countin <> c1)     not first record
          prtPage        Laser;*newpage
          endif
          call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p8000:500,Page:
                              *p=8000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
.                              *p=500:725,Ownocpy:
                              *p=500:725,payone:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
          MOVE      NO TO OWNBR

          RETURN
HD2
.                IF (Countin <> c1 or ownbr = YEs)     not first record
.                prtPage        Laser;*newpage;
.                endif
                    call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p8000:500,Page:
                              *p=8000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
                              *p=500:725,Ownocpy:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
.                      prtclose         Laser
         RETURN
HD3
         MATCH     YES TO OWNBR
         IF        EQUAL
         MOVE      C1 TO PAGE
         ELSE
         MATCH    YES TO ltot
         IF       EQUAL
         MOVE     C1 TO PAGE
         ENDIF
         ENDIF
         move     no to ltot
          IF        (Countin <> c1)     not first record
          prtPage        Laser;*newpage
          endif
          call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p10500:500,Page:
                              *p=10500:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
.                              *p=500:725,Ownocpy:
                              *p=500:725,payone:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
         MOVE      NO TO OWNBR
         RETURN
HD4
         ADD       C1 TO PAGE
          IF        (Countin <> c1)     not first record
          prtPage        Laser;*newpage
          endif
          call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p8000:500,Page:
                              *p=8000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
                              *p=500:725,Ownocpy:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
         RETURN
HD5
         ADD       C1 TO PAGE
          IF        (Countin <> c1)     not first record
          prtPage        Laser;*newpage
          endif
          call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p8000:500,Page:
                              *p=8000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
                              *p=500:725,Ownocpy:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
         RETURN
HD6
         MATCH     YES TO OWNBR
         IF        EQUAL
         MOVE      C1 TO PAGE
         ELSE
         ENDIF
          IF        (Countin <> c1)     not first record
          prtPage        Laser;*newpage
          endif
          call                prtpayform
                call          trim using Ownlocty
                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p8000:500,Page:
                              *p=8000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
                              *p=500:725,Ownocpy:
                              *p=500:875,Ownlosa:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc:
.START PATCH 3.41 REPLACED LOGIC
.                              *p=3000:1025,Slslst1
                              *p=3000:1025,str9,Slslst1
.END PATCH 3.41 REPLACED LOGIC
         MOVE      NO TO OWNBR
         RETURN
TOTALOWN
.        branch     funcbr to totland,totport,totport,totport,totland,totport,totport
        branch     funcbr to totland,totport,totport,totLand,totland,totport,totport
totland
          if        (row > 7100)
          call                Heading
          endif

          goto                totdoit
.
totport
          if        (row > 9500)
          call                Heading
          endif
totdoit
         MOVE      TOTmask TO LSTMASK
         EDIT      APTOTLST TO LSTMASK
         MOVE      TOTmask TO LSTMASKE
         EDIT      APTOTLSe TO LSTMASKe
         MOVE      TOTmask TO LSTMASKR
         EDIT      APTOTLSr TO LSTMASKr
         MOVE      TOTOMSK TO OWNERMSK
         COMPARE   C3 TO FUNCBR
         IF        EQUAL
         ADD       MANPAY TO APTOTOWN
         ADD       manpay to grap
.         ADD       MANPAY TO APTOTlst
         ENDIF
         COMPARE   C6 TO FUNCBR
         IF        EQUAL
         ADD       MANPAY TO APTOTOWN
.         ADD       MANPAY TO APTOTlst
         ENDIF
.Start patch #3.22
         COMPARE   C7 TO FUNCBR
         IF        EQUAL
         ADD       MANPAY TO APtotlst
         endif
..End patch #3.22
         EDIT      APTOTOWN TO OWNERMSK
         MOVE      TOTMASK TO TOTACMSK
         EDIT      TOTACPAY TO TOTACMSK
         move      totmask to armask
         edit      artotlst to armask
         compare   c2 to funcbr
         goto      totc2 if equal
        compare    c5 to funcbr
        goto       totc5 if equal
        compare    c6 to funcbr
        goto       totc6 if equal
        goto       totnot5
.        if         equal
totc2
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
          add       Sixlpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Exchange Total",*Alignment=*right,*p=6200:row,lstMaske:
                              *Alignment=*Left
          add       Sixlpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Rent Total",*Alignment=*right,*p=6200:row,lstMaskR:
                              *Alignment=*Left
          add       Sixlpi to row
          goto       totx
totc5
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"A/R Total",*Alignment=*right,*p=8725:row,Armask
         add            SixLpi to row
         goto      totx
totc6
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"A/R Total",*Alignment=*right,*p=8725:row,Armask
         add            SixLpi to row
         goto      totx
totnot5
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
          add       Sixlpi to row
          add       Sixlpi to row
totx
         COMPARE    C2 TO FUNCBR
         IF        EQUAL
         GOTO      TOTBR
         ENDIF
         COMPARE    C3 TO FUNCBR
         IF        EQUAL
         GOTO      TOTBR
         ENDIF
         COMPARE    C6 TO FUNCBR
         IF        EQUAL
         GOTO      TOTBR
         ENDIF
         COMPARE    C7 TO FUNCBR
         IF        EQUAL
         GOTO      TOTBR
         ENDIF
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total Due Owner",*Alignment=*right,*p=6200:row,OwnerMsk:
                              *Alignment=*Left
mtaxprt2
               PrtPage        Laser;*p=1:10425,"Mailer's tax status is provided as a service though":
                              " its accuracy cannot be guaranteed."
          CALL                HEADING
          goto      notot
TOTBR
        BRANCH    FUNCBR OF NOTOT,TOTAC,TOTPAYD,NOTOT,notot,totpayd,totcomb
TOTAC
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total Payable",*Alignment=*right,*p=6200:row,TotAcMsk:
                              *Alignment=*Left
          add       Sixlpi to row
         GOTO      NOTOT
TOTPAYD
mtaxpaid
          if        (row > 7600)
          call                Heading
          endif
               PrtPage        Laser;*p=1:7725,"Mailer's tax status is provided as a service though":
                              " its accuracy cannot be guaranteed."
        compare    c0 to manpay
        goto       noman if equal
        MOVE        manmsk TO MANMASK
         EDIT       MANPAY TO MANMASK
         if        (row > 7100)
         call       Heading
         endif
.begin patch 3.4
.                   PrtPage             laser;*p=125:row,*font=prtpg9b,"Total Manual",*Alignment=*right,*p=6200:row,manMask:
.                              *Alignment=*Left
.begin patch 3.4
NOMAN
         if        (row > 7100)
         call       Heading
         endif
          PrtPage             laser;*p=125:row,*font=prtpg9b,"Owner Total",*Alignment=*right,*p=6200:row,OwnerMsk:
                              *Alignment=*Left
        goto       notot

totcomb
         if        (row > 7100)
         call       Heading
         endif
.begin patch 3.4
.                   PrtPage             laser;*p=125:row,*font=prtpg9b,"Total Manual",*Alignment=*right,*p=6200:row,OwnerMsk:
.                              *Alignment=*Left
.        compare    c0 to manpay
.        goto       totp if equal
.        MOVE        manmsk TO MANMASK
.         EDIT       MANPAY TO MANMASK
.                   PrtPage             laser;*p=125:row,*font=prtpg9b,"Total Manual",*Alignment=*right,*p=6200:row,manMask:
.                              *Alignment=*Left
.begin patch 3.4

totp
                              add       Sixlpi to row
        clear       ownermsk
        MOVE        totomsk TO ownermsk
        edit        appaid  to ownermsk
         if        (row > 7100)
         call       Heading
         endif
          PrtPage             laser;*p=125:row,*font=prtpg9b,"Total Paid",*Alignment=*right,*p=6200:row,OwnerMsk:
                              *Alignment=*Left
          add       Sixlpi to row
        clear       ownermsk
        MOVE        totomsk TO ownermsk
        edit        apopen  to ownermsk
         if        (row > 7100)
         call       Heading
         endif
          PrtPage             laser;*p=125:row,*font=prtpg9b,"Total Open",*Alignment=*right,*p=6200:row,OwnerMsk:
                              *Alignment=*Left
Mtaxcomb
               PrtPage        Laser;*p=1:7725,"Mailer's tax status is provided as a service though":
                              " its accuracy cannot be guaranteed."

NOTOT  MOVE        C0 TO MANPAY
        MOVE        C0 TO APTOTLST
        MOVE        C0 TO APTOTLSE
        MOVE        C0 TO APTOTLSR
        move        c0 to aplist
        move        c0 to artotlst
        MOVE        C0 TO APpaid
        MOVE        C0 TO APopen
        MOVE        C0 TO APTOTOWN
        MOVE        C0 TO TOTACPAY
         MOVE      C0 TO APTOTDET
         MATCH     YES TO FINISH
         GOTO      FINALTOT IF EQUAL
          If        (funcbr = c2 or Funcbr = c3 or Funcbr = c6 or Funcbr = c7)
          MOVE        SLSOWN TO CBOWNNUM
          MOVE        SLSLIST TO CBLSTNUM
          MOVE      YES TO OWNBR
          CALL      HEADING
          endif
          BRANCH    FUNCBR OF RET1,REt2,RET3,RET4,ret1,ret3,ret3
RET1
         NORETURN
         MOVE      YES TO OWNBR
         GOTO      PARFILE
REt2     CALL      WRTPAGE
         RETURN
RET3
         RETURN
RET4
         NORETURN
         mATCH     YES TO FINISH
         GOTO      EOJ IF EQUAL
         MOVE      YES TO OWNBR
         call      MOVEOWN
         GOTO      DETAIL

TOTLIST
.        branch     funcbr to totland1,totport1,totport1,totport1,totland1,totport1,totport1
        branch     funcbr to totland1,totport1,totport1,totland1,totland1,totport1,totport1
totland1
         if        (row > 7100)
         call       Heading
         endif
.         COMPARE   c46 TO PRTLINES
.         CALL      HEADING IF NOT LESS
         goto      totdoit1
totport1
          if        (row > 9500)
          call                Heading
          endif
.COMPARE   C56 TO PRTLINES
.         CALL      HEADING IF NOT LESS
totdoit1 MOVE      TOTMASK TO LSTMASK
         MOVE      TOTMASK TO LSTMASK
         EDIT      APTOTLST TO LSTMASK
        MOVE      TOTmask TO LSTMASKE
         EDIT      APTOTLSe TO LSTMASKe
         MOVE      TOTmask TO LSTMASKR
         EDIT      APTOTLSr TO LSTMASKr
          move      totmask to armask
         edit      artotlst to armask
         compare   c5 to funcbr
         if        equal
                    if        (row > 7100)
                    call       Heading
                    endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
         add            SixLpi to row
                    if        (row > 7100)
                    call       Heading
                    endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"A/R Total",*Alignment=*right,*p=8725:row,Armask
         add            SixLpi to row
         endif
         
         compare   c6 to funcbr
         if        equal
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"A/R Total",*Alignment=*right,*p=8725:row,Armask
         add            SixLpi to row
         endif
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Exchange Total",*Alignment=*right,*p=6200:row,lstMaskE:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Rental Total",*Alignment=*right,*p=6200:row,lstMaskR:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         add            SixLpi to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"List Total",*Alignment=*right,*p=6200:row,lstMask:
                              *Alignment=*Left
         add            SixLpi to row
           COMPARE   FUNCBR TO C2
           IF       not  EQUAL
         move      yes to ltot
         endif
         BRANCH    FUNCBR OF NOLST,TOTLST,TOTLST,NOLST,nolst,totlst,totlst
TOTLST
           COMPARE   FUNCBR TO C2
           IF        EQUAL
          if        (row > 7100)
          call                Heading
          endif
        endif
NOLST
        MOVE       C0 TO APTOTLST
        MOVE        C0 TO APTOTLSE
        MOVE        C0 TO APTOTLSR
        move       c0 to aplist
        move       c0 to artotlst
         MOVE      C0 TO APTOTDET
        MOVE       SLSLIST TO CBLSTNUM
          add       sixlpi to row
          add       sixlpi to row
          add       sixlpi to row
          COMPARE             FUNCBR TO C2
                    IF                  EQUAL
                    if                  (row > 7100)
                    call                Heading
                    endif
          else
          call                HEADING
                    ENDIF
         MOVE      YES TO LISTCHG
        RETURN
.
FINALTOT
         COMPARE   C3 TO FUNCBR
         IF        EQUAL
         CALL      HD5
         GOTO      ADDGRD
         ENDIF
         COMPARE   C6 TO FUNCBR
         IF        EQUAL
         CALL      HD5
         GOTO      ADDGRD
         ENDIF
         COMPARE   C7 TO FUNCBR
         IF        EQUAL
          call      Heading
.         CALL      HD7
         GOTO      ADDGRD
         ENDIF
         CALL      HD4
ADDGRD   MOVE      GRDMASK TO GRAPMSK
         EDIT      GRAP TO GRAPMSK
         MOVE      GRDMASK TO GRAP1MSK
         EDIT      GRAP1 TO GRAP1MSK
         MOVE      TOTMASK TO GRAP2MSK
         EDIT      GRAP2 TO GRAP2MSK
         COMPARE   C3 TO FUNCBR
         IF        EQUAL
         GOTO      PAIDTOT
         ENDIF
         COMPARE   C6 TO FUNCBR
         IF        EQUAL
         GOTO      PAIDTOT
         ENDIF
         COMPARE   C7 TO FUNCBR
         IF        EQUAL
         GOTO      PAIDTOT
         ENDIF
         if        (row > 7100)
         call       Heading
         endif
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total A/P ONE",*Alignment=*right,*p=6200:row,Grap1msk:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total A/P TWO",*Alignment=*right,*p=6200:row,Grap2msk:
                              *Alignment=*Left
         add            SixLpi to row
         if        (row > 7100)
         call       Heading
         endif
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total ACCOUNTS PAYABLE",*Alignment=*right,*p=6200:row,Grapmsk:
                              *Alignment=*Left
         add            SixLpi to row
         GOTO      EOJ
PAIDTOT
.         if        (row > 7100)
.         call       Heading
.         endif
.         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total ACCOUNTS PAID",*Alignment=*right,*p=6200:row,Grapmsk:
.                              *Alignment=*Left
.         add            SixLpi to row
         GOTO      EOJ
.
.
BREAK
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         CALL      READPAY
         CALL      HEADING
        MOVE      SLSOWN TO CBOWNNUM
        MOVE      SLSLIST TO CBLSTNUM
         MOVE      YES TO LISTCHG
         BRANCH    FUNCBR OF RET,PAGEW2,RET,RET,ret,ret,ret
RET      MOVE      NO TO FIRST
        RETURN
PAGEW2
         MOVE      NO TO FIRST
         MOVE      YES TO OWNBR
         CALL      WRTPAGE
         RETURN
.
.

.
.PAGES    BRANCH    FUNCBR OF CHKAGE,WRTPAGE,WRTPAGE
.PAGES    BRANCH    FUNCBR OF WRTPAGE,WRTPAGE,WRTPAGE,WRTPAGE,wrtpage,wrtpage
PAGES    BRANCH    FUNCBR OF WRTPAGE,WRTPAGE,WRTPAGE
         RETURN
.
.
WRTPAGE  MOVE      PAGE TO N4
         MOVE      N4 TO INDPAGE
.START PATCH 3.24 REPLACED LOGIC
.         MOVE      CBOWNNUM TO INDMNUM
.         MOVE      OWNLONM TO INDNAME
.         MOVE      OWNOCPY TO INDCOMP
.         CALL      NPGEWRT
.Temporary patch
          pack      INDMNUM,"00",CBOWNNUM
.         pack      INDMNUM,CBOWNNUM
.
          pack      NPGEFLD,INDMNUM
          MOVE      OWNLONM TO INDNAME
          MOVE      OWNOCPY TO INDCOMP
          move      "NPGEWRT",Location
          pack      KeyLocation,"Key: ",NPGEFLD
          CALL      NPGEWRT
          call      PageUpdate
.END PATCH 3.24 REPLACED LOGIC
         RETURN
+...........................................................................
TOTALS
         MOVE      YES TO FINISH
         CALL      TOTALOWN
.
PARFILE
          IF        (Funcbr = c1 or Funcbr = c5)
.         COMPARE   C1 TO FUNCBR
.         IF        EQUAL
         READ      PARFILE,SEQ;POWN
         GOTO      EOJ IF OVER
         MOVE      POWN TO nslsfld
         REP       ZFILL IN nslsfld
         CALL      NSLSKEY
         GOTO      PARFILE IF OVER
JDTest1
         call      MOVEOWN
         GOTO      DETAIL
         ENDIF
. new patch added. payables getting U01 3/22/05 JD Func=4
                              call      moveown
                              goto      detail
.;         COMPARE   C5 TO FUNCBR
.         IF        EQUAL
..PATCH 3.25
.;                            move      c2 to nslspath
.;                            move      c0 to nslsflag
..PATCH 3.25
.         READ      PARFILE,SEQ;POWN
.         GOTO      EOJ IF OVER
.         MOVE      POWN TO nslsfld
.         REP       ZFILL IN nslsfld
.         CALL      NSLSKEY
.         GOTO      PARFILE IF OVER
.         call      MOVEOWN
.         GOTO      DETAIL
.         ENDIF
.         CALL     NSLSSEQ
.         GOTO     EOJ IF OVER
.         call     moveown
.         GOTO      DETAIL
.
MOVEOWN MOVE      SLSOWN TO CBOWNNUM
        MOVE      SLSLIST TO CBLSTNUM
         MOVE      YES TO LISTCHG
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         CALL      READPAY
         CALL      HEADING
         return
.         GOTO      DETAIL
.
READKS
          IF        (Funcbr = c1 or funcbr =c5)
.         COMPARE   C1 TO FUNCBR
.         IF        EQUAL
         CALL      NSLSKS
.        GOTO      EOJ IF OVER          .12FEB93 DLH ADDED.
         GOTO      TOTALOWN IF OVER     .08APRJD ADDED.
         GOTO      DETAIL
         ENDIF
.         COMPARE   C5 TO FUNCBR
.         IF        EQUAL
.         CALL      NSLSKS
.         GOTO      TOTALOWN IF OVER     .08APRJD ADDED.
.         GOTO      DETAIL
.         ENDIF
         CALL      NSLSSEQ
         GOTO      totals IF OVER
         GOTO      DETAIL
.begin patch 3.1
................................................................................................
.prtform
PrtPayForm          Branch    Funcbr of prtpayform1,prtpayform4,prtpayform3,prtpayform4,prtpayform5,prtpayform3:
                              PrtPayform3

Prtpayform1         return


................................................................................................
prtpayform3
.
       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                   *MarginL=0,*MarginT=0:
                   *Alignment=*Left:
                   *Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
                   *p=1:1,*font=prtpg12B,"Confidential":
                   *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
                   *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
                   *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
                   *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
                   *PENSIZE=10,*p=2625:1250,*Line=2625:8000:             ;3rd
                   *PENSIZE=10,*p=3250:1250,*Line=3250:8000:             ;4th
                   *PENSIZE=10,*p3875:1250,*Line=3875:8000:              ;5th
                   *PENSIZE=10,*p4500:1250,*Line=4500:8000:              ;6th
                   *PENSIZE=10,*p5250:1250,*Line=5250:8000:              ;7th
                   *PENSIZE=10,*p6250:1250,*Line=6250:8000:              ;8th
                   *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;9th
                   *PENSIZE=10,*p=1:8000,*Line=1900:8000:                    ;bottom lines   1
                   *PENSIZE=10,*p=2000:8000,*Line=2525:8000:                    ;bottom lines 2
                   *PENSIZE=10,*p=2625:8000,*Line=3150:8000:                    ;bottom lines 2
                   *PENSIZE=10,*p=3250:8000,*Line=3775:8000:                    ;bottom lines   3
                   *PENSIZE=10,*p=3875:8000,*Line=4400:8000:                    ;bottom lines   4
                   *PENSIZE=10,*p=4500:8000,*Line=5150:8000:                    ;bottom lines   5
                   *PENSIZE=10,*p=5250:8000,*Line=6150:8000:                    ;bottom lines   5
                   *PENSIZE=10,*p=6250:8000,*Line=9800:8000:                    ;bottom lines   6
                   *PENSIZE=10,*p=9875:8000,*Line=10400:8000:                    ;bottom lines   7
                   *p=583:1300,*font=prtpg9bi,"Mailer":
                   *p=2125:1300,*font=prtpg9bi,"LR##":
                   *p=2825:1300,*font=prtpg9bi,"Inv##":
                   *p=3350:1300,*font=prtpg9bi,"Inv Date":
                   *p=3955:1300,*font=prtpg9bi,"Mail Date":
                   *p=4600:1300,*font=prtpg9bi,"Owner Inv":
                   *p=5300:1300,*font=prtpg9bi,"Income Amount":
              *p6725:1300,*font=prtpg9bi,"Check Date":
              *p7725:1300,*font=prtpg9bi,"Check Number":
                   *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
....Begin patch
                    if        (funcbr = c6)
                    PRTPAGE     Laser;*p8725:1300,*font=prtpg9bi,"Gross A/R"
                    endif
                    
.End patch
          move      "1600",row
          call      PrtPrepHead
          return
................................................................................................
.prtform   - portrait
prtpayform4
       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                   *MarginL=0,*MarginT=0:
                   *Alignment=*Left:
                   *Pictrect=*off,*PICT=1:800:3000:8000:NINLogo:
                   *p=1:1,*font=prtpg12B,"Confidential":
                   *PENSIZE=10,*p=1:1250,*Line=8000:1250:               ;top line
                   *PENSIZE=10,*p=1:1500,*Line=8000:1500:               ;top line
                   *PENSIZE=10,*p=1:1250,*Line=1:10750:                   ;first vert line
                   *PENSIZE=10,*p=2000:1250,*Line=2000:10750:             ;2nd
                   *PENSIZE=10,*p=2625:1250,*Line=2625:10750:             ;3rd
                   *PENSIZE=10,*p=3250:1250,*Line=3250:10750:             ;4th
                   *PENSIZE=10,*p3875:1250,*Line=3875:10750:              ;5th
                   *PENSIZE=10,*p4500:1250,*Line=4500:10750:              ;6th
                   *PENSIZE=10,*p5250:1250,*Line=5250:10750:              ;7th
                   *PENSIZE=10,*p6250:1250,*Line=6250:10750:              ;8th
.                   *PENSIZE=10,*p9875:1250,*Line=9875:10750:              ;9th
                   *PENSIZE=10,*p=1:10575,*Line=1900:10575:                    ;bottom lines   1
                   *PENSIZE=10,*p=2000:10575,*Line=2525:10575:                    ;bottom lines 2
                   *PENSIZE=10,*p=2625:10575,*Line=3150:10575:                    ;bottom lines 3
                   *PENSIZE=10,*p=3250:10575,*Line=3775:10575:                    ;bottom lines  4
                   *PENSIZE=10,*p=3875:10575,*Line=4400:10575:                    ;bottom lines  5
                   *PENSIZE=10,*p=4500:10575,*Line=5150:10575:                    ;bottom lines  6
                   *PENSIZE=10,*p=5250:10575,*Line=6150:10575:                    ;bottom lines  7
                   *PENSIZE=10,*p=6250:10750,*Line=9800:10750:                    ;bottom lines  8
.                   *PENSIZE=10,*p=9875:10750,*Line=10400:10750:                    ;bottom lines 9
                   *p=583:1300,*font=prtpg9bi,"Mailer":
                   *p=2125:1300,*font=prtpg9bi,"LR##":
                   *p=2825:1300,*font=prtpg9bi,"Inv##":
                   *p=3350:1300,*font=prtpg9bi,"Inv Date":
                   *p=3955:1300,*font=prtpg9bi,"Mail Date":
                   *p=4600:1300,*font=prtpg9bi,"Owner Inv":
                   *p=5300:1300,*font=prtpg9bi,"Income Amount":
                   *p=6550:1300,*font=prtpg9bi,"Mlr Tax Status"
          move      "1600",row
          call      PrtPrepHead
          return
................................................................................................
prtpayform5
.
       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                   *MarginL=0,*MarginT=0:
                   *Alignment=*Left:
                   *Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
                   *p=1:1,*font=prtpg12B,"Confidential":
                   *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
                   *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
                   *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
                   *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
                   *PENSIZE=10,*p=2625:1250,*Line=2625:8000:             ;3rd
                   *PENSIZE=10,*p=3250:1250,*Line=3250:8000:             ;4th
                   *PENSIZE=10,*p3875:1250,*Line=3875:8000:              ;5th
                   *PENSIZE=10,*p4500:1250,*Line=4500:8000:              ;6th
                   *PENSIZE=10,*p5250:1250,*Line=5250:8000:              ;7th
                   *PENSIZE=10,*p6250:1250,*Line=6250:8000:              ;8th
                   *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;9th
                   *PENSIZE=10,*p=1:8000,*Line=1900:8000:                    ;bottom lines   1
                   *PENSIZE=10,*p=2000:8000,*Line=2525:8000:                    ;bottom lines 2
                   *PENSIZE=10,*p=2625:8000,*Line=3150:8000:                    ;bottom lines 2
                   *PENSIZE=10,*p=3250:8000,*Line=3775:8000:                    ;bottom lines   3
                   *PENSIZE=10,*p=3875:8000,*Line=4400:8000:                    ;bottom lines   4
                   *PENSIZE=10,*p=4500:8000,*Line=5150:8000:                    ;bottom lines   5
                   *PENSIZE=10,*p=5250:8000,*Line=6150:8000:                    ;bottom lines   5
                   *PENSIZE=10,*p=6250:8000,*Line=9800:8000:                    ;bottom lines   6
                   *PENSIZE=10,*p=9875:8000,*Line=10400:8000:                    ;bottom lines   7
                   *p=583:1300,*font=prtpg9bi,"Mailer":
                   *p=2125:1300,*font=prtpg9bi,"LR##":
                   *p=2825:1300,*font=prtpg9bi,"Inv##":
                   *p=3350:1300,*font=prtpg9bi,"Inv Date":
                   *p=3955:1300,*font=prtpg9bi,"Mail Date":
                   *p=4600:1300,*font=prtpg9bi,"Owner Inv":
                   *p=5300:1300,*font=prtpg9bi,"Income Amount":
              *p6725:1300,*font=prtpg9bi,"Check Date":
              *p7725:1300,*font=prtpg9bi,"Check Number":
              *p8850:1300,*font=prtpg9bi," Gross A/R":
                   *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
          move      "1600",row
          call      PrtPrepHead
          return

PrtPrepHead
          Branch    result of reph1,reph2,reph3,reph4,reph5,reph6,reph7
reph1     return
reph2
              PrtPage         Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
                    *p=5550:1,*font=prtpg12B,"EOM-Payable By Owner",*font=prtpg9bi
          Return
reph3
.START PATCH 3.44 REPLACED LOGIC
.              PrtPage        Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
.                             *p=5550:1,*font=prtpg12B,"Paid Report By Owner",*font=prtpg9bi
              PrtPage         Laser;*PENSIZE=10,*RECT=1:250:5500:7750:
                    *p=5550:1,*font=prtpg12B,"Owner Income Report - Paid",*font=prtpg9bi
.END PATCH 3.44 REPLACED LOGIC
          Return
reph4
.START PATCH 3.44 REPLACED LOGIC
.              PrtPage        Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
.                             *p=5550:1,*font=prtpg12B,"Payable Report By Owner",*font=prtpg9bi
              PrtPage         Laser;*PENSIZE=10,*RECT=1:450:5500:7400:
                    *p=5550:1,*font=prtpg12B,"Owner Income Report - "
              PrtPage         Laser;*p=5650:200,"Open Invoices only",*font=prtpg9bi
          Return
.END PATCH 3.44 REPLACED LOGIC
reph5
.START PATCH 3.44 REPLACED LOGIC
.              PrtPage        Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
.                             *p=5550:1,*font=prtpg12B,"Payable Report By Owner",*font=prtpg9bi
              PrtPage         Laser;*PENSIZE=10,*RECT=1:250:5500:8700:
                    *p=5550:1,*font=prtpg12B,"Owner Income Report - Open Invoices only",*font=prtpg9bi
          Return
.END PATCH 3.44 REPLACED LOGIC
reph6
.START PATCH 3.44 REPLACED LOGIC
.              PrtPage        Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
.                             *p=5550:1,*font=prtpg12B,"Payable Report By Owner",*font=prtpg9bi
              PrtPage         Laser;*PENSIZE=10,*RECT=1:250:5500:8700:
                    *p=5550:1,*font=prtpg12B,"Owner Income Report - Open Invoices only",*font=prtpg9bi
          Return
.END PATCH 3.44 REPLACED LOGIC
reph7
.START PATCH 3.44 REPLACED LOGIC
.              PrtPage        Laser;*PENSIZE=10,*RECT=1:250:5500:7500:
.                             *p=5550:1,*font=prtpg12B,"Income Report By Owner",*font=prtpg9bi
              PrtPage         Laser;*PENSIZE=10,*RECT=1:250:5500:8400:
                    *p=5550:1,*font=prtpg12B,"Owner Income Report (Open and Paid)",*font=prtpg9bi
          Return
.END PATCH 3.44 REPLACED LOGIC
................................................................................................
.end patch 3.1
.START PATCH 3.24 ADDED LOGIC
PageUpdate
.Update previous record!!
          call      Trim using HoldMlrKey
          if (HoldMlrKey <> "")
                    pack      NPGEFLD,HoldMlrKey
.Re-pack immediately!!!
                    move      INDMNUM,HoldMlrKey
.
                    move      HoldBrkKey,str35
                    call      Trim using str35
                    move      INDNAME,HoldBrkKey
.
                    move      "NPGEKEY",Location
                    pack      KeyLocation,"Key: ",NPGEFLD
                    CALL      NPGEKEY
                    loop
                              until over
                              until (INDMNUM <> NPGEFLD)
                              call      Trim using INDNAME
                              if (str35 = INDNAME)
                                        move      PAGE,N4
                                        MOVE      N4 TO INDPAGE2
                                        move      "NPGEUPD",Location
                                        pack      KeyLocation,"Key: ",NPGEFLD
                                        CALL      NPGEUPD
                                        break
                              endif
                              move      "NPGEKS",Location
                              pack      KeyLocation,"Key: ",NPGEFLD
                              CALL      NPGEKS
                    repeat
          else
                    move      INDMNUM,HoldMlrKey
                    move      INDNAME,HoldBrkKey
          endif
          return
.END PATCH 3.24 ADDED LOGIC
PRTOPENPREP
               Branch          hotflag of ToPrinter,ToFIle
.ToPRinter - send directly to a printer
ToPRinter
.>Patch 3.43 Commented Out
.                        PRTOPEN       Laser,"",str45
.                        return
.**************TEMP
.               if             (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.               PRTOPEN        Laser,"\\NINs2\Laser8",str45
.                              ELSEIF (OSFLAG = "3" or OSFLAG = "4")            .95/98
.                              PRTOPEN       Laser,"Laser8",str45
.                              else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN       Laser,"-",str45
.               endif
.               Return
.>Patch 3.43 Commented Out
.>Patch 3.43 Code Added
                    if (PRTFLAG = "2")  .Sales
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   Laser,"\\NINs2\Laser3 Blankstock",str45
                              if (OSFLAG >= c6)
                                        PRTOPEN   Laser,"\\nins2\Laser3 Blankstock",str45
.                              elseif (osflag = "9" or osflag = "8")         .win 7  or vista
.                                        PRTOPEN   Laser,"\\NINs2\Laser3 Blankstock",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",str45
                              endif
.turned off 3/5/08 jd Spooling errors on GB/JL machine, checking.
                    elseif (PRTFLAG = "3")                            .All others
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   Laser,"\\NINs2\Laser2",str45
                              if (OSFLAG >= c6)
                                        PRTOPEN   Laser,"\\nins2\Laser2",str45
.                              elseif (osflag = "9" or osflag = "8")         .win 7  or vista
.                                        PRTOPEN   Laser,"\\NINs2\Laser2",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",str45
                              endif
                    else
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   Laser,"\\NINs2\Laser8",str45
                              if (OSFLAG >= c6)
                                        PRTOPEN   Laser,"\\nins2\Laser8",str45
.                              elseif (osflag = "9" or osflag = "8")         .win 7  or vista
.                                        PRTOPEN   Laser,"\\NINs2\Laser8",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",str45
                              endif
                    endif
.>Patch 3.43 Code Added
.ToFile - Spool to file
ToFile
..........................................................
                    if (PRTFLAG = "P")            .PDF
.First check 995 autolaunch settings
.begin patch 3.5
.                        move  "PDF995 Copy",Filename
.                        pack  KeyLocation,"Prepping FIle "
.                              call      PDF995Auto    
..                              Open           TestFile,"c:\progra~1\\pdf995\res\pdf995.ini"
..                              Prepare        TempFile,"c:\progra~1\\pdf995\res\pdf995.out"
..                              Loop
..                         move "PDF995 Copy",Filename
.                         pack KeyLocation,"Reading FIle "
.//Patch 3.45 Code Modification                                                       
..                              Read           TestFile,seq;STr35
..                              Read           TestFile,seq;taskname
..                              Until          Over
...                              Scan           "Autolaunch=1" in str35
..                              Scan           "Autolaunch=1" in taskname
..                              If             equal
..                              move           Yes to Reset995flag
...                              reset          str35
...                              clear          str35
..                                    reset         taskname
..                                    clear         taskname                              
...                              Move           "Autolaunch=0" to str35
..                              Move           "Autolaunch=0" to taskname
..//Patch 3.45 Code Modification End                              
..                              endif
..                         move "PDF995 Copy",Filename
..                         pack KeyLocation,"Writing FIle "
..//Patch 3.45 Code Modification                                                       
..                              write          tempfile,Seq;str35
..                              write          tempfile,Seq;taskname
.//Patch 3.45 Code Modification End
..                              Repeat
..                         move "PDF995 Copy",Filename
..                         pack KeyLocation,"WEOF FIle "
..                              weof           tempfile,seq
..                         move "PDF995 Copy",Filename
..                         pack KeyLocation,"Closing Output FIle "
..                              close          tempfile
..                         move "PDF995 Copy",Filename
..                         pack KeyLocation,"Closing Input FIle "
..                              close          testfile
..                         move "PDF995 Copy",Filename
..                         pack KeyLocation,"Erasing Save FIle "
..                              Erase          "c:\progra~1\\pdf995\res\pdf995.Sav"
..                         move "PDF995 Rename",Filename
..                         pack KeyLocation,"Ren Ini to Sav "
..                              Rename         "c:\progra~1\\pdf995\res\pdf995.ini","c:\progra~1\\pdf995\res\pdf995.Sav"
..                         move "PDF995 Rename",Filename
..                         pack KeyLocation,"Ren out to Ini"
..                              Rename         "c:\progra~1\\pdf995\res\pdf995.out","c:\progra~1\\pdf995\res\pdf995.ini"
.
Createjd
.;                             PRTOPEN Laser,"Acrobat Distiller",str25b
                              pack      str55 from "c:\work\pdf\",str25b
.                                             PRTOPEN Laser,"PDF995",str25b
                              PRTOPEN Laser,"PDF:",str55
..                             pack    str55,str25b,".pdf"
.                              pack    str55,str25b
.end patch 3.5
                              move      "p",PRTFLAG
                    elseif (PRTFLAG = "2")        .Sales
.                   if (PRTFLAG = "2")  .Sales
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   Laser,"\\NINs2\Laser3 Blankstock",str45
                              if (OSFLAG >= c6)
                                        PRTOPEN   Laser,"\\nins2\Laser3 Blankstock",str45
.                              elseif (osflag = "9" or osflag = "8")         .win 7  or vista
.                                        PRTOPEN   Laser,"\\NINs2\Laser3 Blankstock",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",str45
                              endif
                    else                          .All others
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   Laser,"\\NINs2\Laser2",str45
                              if (OSFLAG >= c6)
                                        PRTOPEN   Laser,"\\nins2\Laser2",str45
.                              elseif (osflag = "9" or osflag = "8")         .win 7  or vista
.                                        PRTOPEN   Laser,"\\NINs2\Laser2",str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",str45
                              endif
                    endif
               Return
CreatePDFFile
.It takes some time for the file to be created, so we must check
.begin patch 3.5
.          move    C0,N9
.          move    "                                        ",APIFileName
..                             pack    str55,str25b,".pdf"
.                              pack    str55,str25b
.          clear   APIFileName
.          pack    APIFileName,"C:\WORK\PDF\",str55,hexzero               ."//
.          clock   timestamp,timestamp1
.          move    timestamp1,time1
.          loop
.                    clock   timestamp,timestamp2
.                    move    timestamp2,time2
.                    sub     time1,time2,time3
.                    if (time3 > 1000) .10 Seconds Maximum
.                              break
.                    endif
.          repeat
.end patch 3.5

.         move      "Here is your PDF File",SmtpSubject Subject
          move      "Here is your PDF File",MailSubjct
.   Set the text message that is send with the attachments
          CLear     Mailbody
          append    str55,mailbody
          append    crlf,mailbody
          reset     mailbody
.         move      str55,SmtpTextMessage(1)   Array <Text message >
.         move      "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.         move      "NTS4",SmtpEmailServer                   Address of email serverc
.         clear     SmtpUserName
          pack      Mailfrom from userlogn2,"@nincal.com"
          pack      MailTo from userlogn2,"@nincal.com"
.               Move           Userlogn to SmtpUserFullName
.         clear     smtpemailaddress
.         append    userlogn2,SmtpEmailAddress
.         append    "@nincal.com",SmtpEmailAddress
.         reset     smtpemailaddress
.         move      userlogn2,SmtpUserName                                User name
..   Set the destinations of the email. Max 100 (Mime spec)
.         move      smtpemailaddress,SmtpDestinations(1,1)
.         move      userlogn2,SmtpDestinations(1,2)
.         move      "1",SmtpDestIndexLast                          originators UserName
.begin patch 3.5
.          pack      mailattach,"c:\work\pdf\",str55         
          pack      mailattach,str55         
.end patch 3.5

.         move      str55,SmtpAttachments(1,1)                     Attached file name
.         move      "C:\WORK\PDF\",SmtpAttachments(1,2)           Path to attached file name
.         move      "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.         move      "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.         clear     SmtpLogFile                                         'Clear' disables the LogFile
.         move      "1",SmtpProgress                                    Enable progress bars
.         call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.         if not equal
.                   pack      Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                             "Status Code ",SmtpStatus," - ",SmtpStatusText
.                   move      "PDF File not found",SmtpSubject Subject
.                   move      "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                   call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..                  Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
.         else
..                  Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
.         endif
          call      SendMail
.Clean up afterwards
               if             (Reset995Flag = yes)
.begin patch 3.5
.                    call      PDF995Auto0
.end patch 3.5
.               move "PDF995 Restore",Filename
.               pack KeyLocation,"Erase temp ini FIle "
.               Erase          "c:\progra~1\\pdf995\res\pdf995.ini"
.               move "PDF995 Restore",Filename
.               pack KeyLocation,"Ren ini  to Save "
.               Rename         "c:\progra~1\\pdf995\res\pdf995.Sav","c:\progra~1\\pdf995\res\pdf995.ini"
               endif
          move      "P",PRTFLAG
          return
...............................................................................

.
.         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
.patch3.23
                                        include   compio.inc
                                        include   cntio.inc
.        INCLUDE   NMLRIO.inc    MAILER
.patch3.23
.begin patch 3.6
         include   nmrgIO.inc
          Include   NInvAcdIO.inc
         include   nshpIO.inc
         include   ndatIO.inc
         include   nacdIO.inc
          include   ndat3io.inc
          include compute.inc
.end patch 3.6
                INCLUDE        NCNTIO.inc
        INCLUDE   NPAYIO.inc
         INCLUDE  NMTXIO.inc
         INCLUDE   NPGEIO.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NJSTIO.inc
         INCLUDE   NSLSIO.inc
         INCLUDE   NORDIO.INC
         INCLUDE   COMLOGIC.inc


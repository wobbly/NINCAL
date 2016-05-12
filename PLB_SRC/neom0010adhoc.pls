............................................................................
.
. PROGRAM    : Neom0010
. DATE       : 03/28/88
. AUTHOR     : E.W. LAKE
. DESCRIPTION: PRODUCES NIN ADJUSTMENT REGISTER.
.
............................................................................
.
PC       EQU       0
.
         INCLUDE   COMMON.inc
         include   hp.inc
         include   cons.inc
............................................................................
+
.patch3.62
                                             include        compdd.inc
                                             include        cntdd.inc
.         INCLUDE   NMLRDD.inc    MAILER
.patch3.62

+
         INCLUDE   NBILDD.inc   
+
         INCLUDE   NORDDD.inc
+
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
+
         INCLUDE   NJSTCLDD.inc  ADJUSTMENT APPLY
.begin patch 3.7
.         include   ninvdd.inc
              include         ninvdd.inc
.end patch 3.7

.begin patch 2.3
         include   ncshdd.inc
.end patch 2.3
.>Patch 3.35
        include winapi.inc
.>Patch 3.35
        
ClnAdj   FILE
INVFLAG  DIM       1         
.
Release  Init      "1.01"     DLH include NIN income
RelDate  INit      "2015 February 17"
.Release  Init      "1.00"     DLH 
.RelDate  INit      "2014 July 16"
.Release  Init      "4.62"     DLH - add additional logic around code 14 (adv payments) for monday #'s
..                                          also fixed old error on sales branch  N2 was being wiped on on exfee orders
.RelDate  INit      "2014 January 27"
.Release  Init      "4.61"     DLH - add additional logic around code 16 (short payments) for monday #'s
.RelDate  INit      "2013 November 18"
.Release  Init      "4.60"     DLH Sunbelt PDF
.RelDate  INit      "2013 April 23"
.Release  Init      "4.53"     DLH Produce A/R & A/P details for excel reporting
.RelDate  INit      "19 Jan 2011"
.release   init      "4.52"        DLH         NINinc code 36
.Reldate   Init      "16 July 2010"
.release   init      "4.51"        DLH         New Year
.Reldate   Init      "04 Jan 2010"
.release   init      "4.5"        DLH         All to NIN 
.Reldate   Init      "09 Dec 2009"
.release   init      "4.4"        DLH         Tag details written to flag file as List Management
.Reldate   Init      "05 Oct 2009"
.release       init            "4.3"        DLH          09 Jan 2009 code 38 pay ar
.release       init            "4.2"        DLH          24 June 2008 more PL  make totals tie to breakout
.release       init            "4.1"        DLH          28January2008 more PL  make totals tie to breakout
.release       init            "4.0"        DLH          11January2008 more PL
.release       init            "3.9"        DLH          12April2007 adj code 29
.release       init            "3.82"        DLH          12April2007 PL
.release       init            "3.81"        DLH          13Feb2006 dump data to excel
.release  init      "3.8"        DMB         23JAN2006 File Folder Restructure for nins1
.release  init      "3.7"        DLH         02March2005    Invoice Conversion
.release  init      "3.65"        DMB        22JUL2005 Converted to prtpage
.release  init      "3.64"        DMB        31MAY2005       NJST dd mod
.release  init      "3.63"        ASH        05AUG2004      Logo Conversion
.release  init      "3.62"        DMB        26MAY2004      Mailer Conversion
.RELEASE  INIT      "2.61"     JD  20Feb2002 - added reason code to download
.RELEASE  INIT      "2.6"     DLH  14Feb2002 - New subotals Void #37
.RELEASE  INIT      "2.5"     DLH  04Oct2001 - New subotals
.RELEASE  INIT      "2.4"     DLH  18June2001 - Email some output data to DH and fix from
.                            cash file conversion.
.RELEASE  INIT      "2.3"     DLH  08April2001 - attempt to add some control info.
.                                 and fixed record in counter was using N5 now using Recsin.
.RELEASE  INIT      "2.2"     DLH  23Aug99 NINadj nadjust Y2K file expansion
.RELEASE  INIT      "2.1"     DLH  27APR99 NININV.DAT Y2K file expansion
.RELEASE  INIT      "2.02"     ASH  14SEP98 NINMLR.DAT Y2K file expansion
.RELEASE  INIT      "2.01"     JD  16apr97 skip date ok? sub.
.RELEASE  INIT      "2.00"    dlh 13SEP96 ADDED FLAT FILE DUMP
.                            ADDED INVOICE VERIFICATION
.RELEASE  INIT      "1.91"    DLH 18AUG94 ADDED CODE TO HANDLE SUMMARY OF
.                            REASON CODE 25.
.RELEASE  INIT      "1.9"     DLH 10MAy94  fix bub (header missing on laser).
.RELEASE  INIT      "1.8"     JD 14MAR94  print to laser.
.RELEASE  INIT      "1.7"     DLH  04JAN94  - ADDED BREAKOUT FOR REASON 27.
.RELEASE  INIT      "1.6"    DLH  01/31/92  - COMBINED LM TOTALS.
.RELEASE  INIT      "1.5"    D.L. HERRICK 12AUG91
.                           ADDED PRINT OF JSTCD 'C OR D'.
.
.RELEASE   INIT      "1.4"   D.L. HERRICK 08AUG91
.                           CONVERSION FROM NADJ002(NINNY) TO CAL. FORMAT.
.
.RELEASE  INIT      "1.3"    E.W. LAKE  08/02/91
.                           ISOLATE CODE '31' LRINC.
.
.RELEASE INIT      "1.2"    E.W. LAKE  07/31/91
.                           ISOLATE CODE '30' LRINC.
.
.RELEASE INIT      "1.1"    E.W. LAKE  07/14/88
.                           PCBUS CONVERSION
.
.RELEASE INIT      "1.0"    E.W. LAKE   03/28/88
.                           INITIAL RELEASE
.
DATE     DIM       8
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
.begin patch 4.0
SLS       Dim       2         Salesnumber
OrderID   DIm       1
.end patch 4.0
.begin patch 4.4
DivFlag   Dim       5         .BR,BE,LMr,LMe,PBR,PBE,PLMr,Plme
.end patch 4.4
.................................................................
.Begin patch 3.82
TAP3     FORM      8.2
TxNININC  FORM      8.2
.end patch 3.82
.begin patch 2.3
.totals related to a control
TCAR      FORM      9.2
TCAP1     FORM      9.2
TCAP2     FORM      9.2
TCAP      FORM      9.2        TOTAL AP1+AP2
TCNININC  FORM      9.2
TCLRINC   FORM      9.2
TCSTAX    FORM      7.2
TCCTAX    FORM      7.2
TCPOST    FORM      7.2
.Begin patch 4.2
TCAP3     FORM      8.2
TCxNININC  FORM      8.2
.end patch 4.2
.totals Not related to a control
TNCAR      FORM      9.2
TNCAP1     FORM      9.2
TNCAP2     FORM      9.2
TNCAP      FORM      9.2        TOTAL AP1+AP2
TNCNININC  FORM      9.2
TNCLRINC   FORM      9.2
TNCSTAX    FORM      7.2
TNCCTAX    FORM      7.2
TNCPOST    FORM      7.2
.Begin patch 4.2
TNCAP3     FORM      8.2
TNCxNININC  FORM      8.2
.end patch 4.2
............................................................
TLRINC1  FORM      9.2        BROKERAGE EXCHANGE
TLRINC2  FORM      9.2        LIST MANAGEMENT EXCHANGE
TLRINC3  FORM      9.2        BROKERAGE RENTAL
TLRINC4  FORM      9.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
TAR      FORM      9.2
TAP1     FORM      9.2
TAP2     FORM      9.2
TAP      FORM      9.2        TOTAL AP1+AP2
.begin Patch 4.53
TBAR      FORM      9.2       .total Brokerage
TBAP      FORM      9.2       .total Brokerage
TLMAR     FORM      9.2       .total Management
TLMAP     FORM      9.2       .total Management

.end Patch 4.53
TNININC  FORM      9.2
TLRINC   FORM      9.2
TSTAX    FORM      7.2
TCTAX    FORM      7.2
TPOST    FORM      7.2
R11      FORM      9.2
R12      FORM      9.2
R14      FORM      9.2
R16      FORM      9.2
.begin patch 4.61
R16LM     form      9.2                           .short payment List Management
R16BR     form      9.2
.end patch 4.61
.begin patch xxx
TBNININC  FORM      9.2                     .total adj to Brokerage NIN
TMNININC  FORM      9.2                     .total adj to Management NIN
.end patch xxx
.begin patch 4.62
R14LM     form      9.2                           .ADV LO payment List Management
R14BR     form      9.2
.end patch 4.62
R20      FORM      9.2
R21      FORM      9.2
R22      FORM      9.2
R23      FORM      9.2
R24      FORM      9.2
R25AR    FORM      9.2
R25AP    FORM      9.2
R25LR    FORM      9.2
R26      FORM      9.2
R27      form      9.2
R29      form      9.2
R30      FORM      9.2
R31      FORM      9.2
.begin patch 2.6
r37AR         form          9.2
r37AP         form          9.2
r37LR         form          9.2
.begin patch 2.6
.begin patch 2.5
R35AR         form          9.2
R35AP         form          9.2
R35LR         form          9.2
r36AR         form          9.2
r36AP         form          9.2
r36LR         form          9.2
.begin patch 4.52
r36NIN         form          9.2
.end patch 4.52
.begin patch 4.3
R38       form      9.2                 pay ar
.end patch 4.3

.begin patch 3.82
.totals related to a control
PLTCAR      FORM      9.2
PLTCAP1     FORM      9.2
PLTCAP2     FORM      9.2
PLTCAP      FORM      9.2        TOTAL AP1+AP2
PLTCNININC  FORM      9.2
PLTCLRINC   FORM      9.2
PLTCSTAX    FORM      7.2
PLTCCTAX    FORM      7.2
PLTCPOST    FORM      7.2
.begin patch 4.2
PLTCAP3     FORM      9.2
PLTCXNININC  FORM      9.2
.end patch 4.2
.totals Not related to a control
PLTNCAR      FORM      9.2
PLTNCAP1     FORM      9.2
PLTNCAP2     FORM      9.2
PLTNCAP      FORM      9.2        TOTAL AP1+AP2
PLTNCNININC  FORM      9.2
PLTNCLRINC   FORM      9.2
PLTNCSTAX    FORM      7.2
PLTNCCTAX    FORM      7.2
PLTNCPOST    FORM      7.2
.begin patch 4.2
PLTNCAP3     FORM      9.2
PLTNCXNININC  FORM      9.2
.end patch 4.2
............................................................
PLTLRINC1  FORM      9.2        BROKERAGE EXCHANGE
PLTLRINC2  FORM      9.2        LIST MANAGEMENT EXCHANGE
PLTLRINC3  FORM      9.2        BROKERAGE RENTAL
PLTLRINC4  FORM      9.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
PLTAR      FORM      9.2
PLTAP1     FORM      9.2
PLTAP2     FORM      9.2
PLTAP      FORM      9.2        TOTAL AP1+AP2
PLTNININC  FORM      9.2
PLTLRINC   FORM      9.2
PLTSTAX    FORM      7.2
PLTCTAX    FORM      7.2
PLTPOST    FORM      7.2
PLR11      FORM      9.2
PLR12      FORM      9.2
PLR14      FORM      9.2
PLR16      FORM      9.2
PLR20      FORM      9.2
PLR21      FORM      9.2
PLR22      FORM      9.2
PLR23      FORM      9.2
PLR24      FORM      9.2
PLR25AR    FORM      9.2
PLR25AP    FORM      9.2
PLR25LR    FORM      9.2
PLR26      FORM      9.2
PLR27      form      9.2
PLR29      form      9.2
PLR30      FORM      9.2
PLR31      FORM      9.2
PLr37AR         form          9.2
PLr37AP         form          9.2
PLr37LR         form          9.2
PLR35AR         form          9.2
PLR35AP         form          9.2
PLR35LR         form          9.2
PLr36AR         form          9.2
PLr36AP         form          9.2
PLr36LR         form          9.2
plTAP3     FORM      8.2
plTxNININC  FORM      8.2

.end patch 3.82
.begin patch 4.3
PR38                form      9.2                 pay ar
.end patch 4.3
.begin patch 2.5

TINVQTY  FORM      9
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
M1       INIT      " JANUARY 20?? "
M2       INIT      "FEBRUARY 20?? "
M3       INIT      "  MARCH 20??  "
M4       INIT      "  APRIL 20??  "
M5       INIT      "   MAY 20??   "
M6       INIT      "  JUNE 20??   "
M7       INIT      "  JULY 20??   "
M8       INIT      " AUGUST 20??  "
M9       INIT      "SEPTEMBER 20??"
M10      INIT      " OCTOBER 20?? "
M11      INIT      "NOVEMBER 20?? "
M12      INIT      "DECEMBER 20?? "
MONTH    DIM       14
PREFLAG  DIM       1
.Start Patch #2.02 - file expansion
.NAME1    DIM       19
.NAME2    DIM       19
NAME1    DIM       45
NAME2    DIM       45
.End Patch #2.02 - file expansion
.
FART       FORM      9.2       A/R
.FAP1T      FORM      7.2       A/P1
.FAP2T      FORM      7.2       A/P2
.FAPT      FORM      7.2       A/P1 + A/P2
.LRINC    FORM      7.2       LR INCOME
.GROSS    FORM      7.2       GROSS BILLING
FAP1T      FORM      9.2       A/P1
FAP2T      FORM      9.2       A/P2
.BEGIN PATCH 3.82
FAP3T      FORM      9.2       Inter comp transfer of LR
.END PATCH 3.82
FAPT      FORM      9.2       A/P1 + A/P2
LRINC    FORM      9.2       LR INCOME
NINC    FORM      9.2       NIN INCOME
.BEGIN PATCH 3.82
xNINC1    FORM      9.2       NIN INCOME
.END PATCH 3.82
GROSS    FORM      9.2       GROSS BILLING
CTAX     FORM      7.2       CITY TAX
STAX     FORM      7.2       STATE TAX
TAXES    FORM      7.2       TOTAL TAXES
ARWOPP   FORM      9.2       A/R WITHOUT PRE-PAYMENT
SHIP     FORM      7.2       TOTAL SHIPPING
SELECT   FORM      7.2       TOTAL SELECTIONS
POST     FORM      7.2
.begin patch 2.3
datechk  form       5
controlnum  dim     3
holdStr4    dim     4
RecsIn      Form    5
incontrol   dim     1
.end patch 2.3
.>Patch 3.65
PRFILE        pfile
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
.Column8  form    9
.Column9  form    9
.Column10 form    9
.Column11 form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
.............................................................................................
.begin patch 3.81
.some excel goodies
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
time          Dim             8
.end patch 3.81

.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
              move    "4250" to Header1
        move    "250",column
        move    "675",column1
        move    "3100",column2
        move    "3600",column3
.        move    "3200",column4
.        move    "3600",column5
.        move    "4400",column6
.        move    "5400",column7
.        move    "6250",column8
.        move    "6800",column9
.        move    "7300",column10
.        move    "8000",column11
.        move    "4000",column4R
        move    "4700",column4R
        move    "5400",column5R
        move    "6100",column6R
        move    "6600",column7R
        move    "7100",column8R
        move    "7700",column9R
        move    "7900",column10R        
        
.        move    "1000",column2
.        move    "1500",column3
..        move    "3200",column4
.        move    "3600",column4
.        move    "4400",column5
.        move    "5400",column6
.        move    "6250",column7
.        move    "6800",column8
.        move    "7300",column9
..        move    "8000",column11
..        move    "4000",column4R
.        move    "4700",column4R
.        move    "5400",column5R
.        move    "6100",column6R
.        move    "6600",column7R
.        move    "7100",column8R
.        move    "7700",column9R        
        
.>Patch 3.65
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
         MOVE      "Adjustment Register" TO STITLE
           MATCH     "NEOM0010" TO PROGRAM        *CHAINED FROM DSINIT?
           IF          NOT EQUAL                        *NO
         MOVE      "NEOM0010 " TO PROGRAM
           IFNZ        PC
         MOVE      "Nadjust.new" TO INPNAME
           XIF
           IFZ         PC
          MOVE      "Nadjust.new" TO INPNAME
           XIF
         MOVE      "LOCAL" TO PRTNAME
         MOVE      "NINCAL" TO COMPNME
           ENDIF
         CALL      PAINT
.
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>change date year<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         MOve         "02/29/16",Today
         move         "adjreg",prtname
DATEOK   goto      begin
         KEYIN     *P10:10,"DATE OK? ",*T60,STR1
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
BEGIN 
         UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
         SCAN      "??" IN MONTH
         GOTO      PRTGET IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
          Move      c3,Nordlock
          Move      c3,Ninvlock
.

.begin patch 2.3
.begin patch 2.4
.         move      "NINCASH" to ncshname
.end patch 2.3
         move      c3 to ncshpath
.end patch 2.3
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Adjust Date : "
.
INPGET   TRAP      FILENG IF IO
.         OPEN      TESTFILE,INPNAME,read
.         TRAPCLR   IO
.         DISPLAY   *P15:06,INPNAME
         GOTO      PRTGET
FILENG   NORETURN
.         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
.                   *P15:06,INPNAME
.         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
.         GOTO      PRTNG IF EQUAL
.         MOVE      C1 TO PRTFLAG
.         MATCH     LOCAL TO PRTNAME
.         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         
.>Patch 3.65 Comment Out                 
.         PACK      PRTFILE WITH pdrive,PRTNAME
.         SPLOPEN   PRTFILE
.>Patch 3.65 Comment Out         
.>Patch 3.38 Logic Addition for PDF Quality Control
.begin patch 4.6                    
.          Call      PDF995Auto
.          call      SetPDFFlag

.>Patch 3.38 Logic Addition for PDF Quality Control
.>Patch 3.65 Replace Logic
.         PACK      PRTFILE WITH "c:\work\pdf\",PRTNAME
         PACK      PRTFILE WITH "c:\work\pdf\",PRTNAME,".pdf"
.               PRTOPEN prfile,"PDF995",PRTFILE
               PRTOPEN prfile,"PDF:",PRTFILE
.end patch 4.6                    
               PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                                              *Duplex=2;                                                 
.>Patch 3.65
         
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START    
              
.             MOVE      INPNAME TO NJSTNAME
.              MOVE      INPNAME TO NJSTNAMESEQ    
.sort 'NEW' adjustments
         DISPLAY   *P10:11,"Sorting 'new' adjustments: "
          pack      str4 from yy,MM
          rep       zfill in str4
.          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127=#'",sysYr,"#'&129=#'",sysmo,"#'#""
          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127='",str4,"'#""
          Sort      Taskname,SunDM="NINS1:502"

          pack      NJSTNAMESEQ from "Nadjust.new|NINS1:502"
              

         CALL      HEADING
         CLEAR     STR55
.>Patch 3.8 Code Modified         
.         APPEND    "\\nins1\d\users\Accounti\gl\clnadj." TO STR55
         APPEND    "\\nins1\d\Accounting\gl\clnadj" TO STR55
.>Patch 3.8 Code Modified         
         APPEND    str4,STR55
         append    ".csv|NINS1:502",str55
         RESET     STR55
         PREPARE   ClnAdj,STR55,exclusive
         MOVE      C1 TO NINVPATH
.
RDADJ    CALL      NJSTSEQ
         GOTO      EOJ IF OVER
         ADD       C1 TO REcsin
         DISPLAY   *P15:08,Recsin:
                   *P15:09,JSTDATE
.*********************TEMPORARY**************************************                   
.         if        (jstap3 <> c0 | JSTXNINC <>c0)
.         goto      rdadj
.         endif
.*********************TEMPORARY**************************************                   

.
.begin patch 2.3
         call      getcontrol
.end patch 2.3
         PACK      STR8 WITH JSTMLR,JSTCNT,JSTBILTO
         MATCH     STR8 TO NBILFLD
         GOTO      DTLPRT IF EQUAL
         MOVE      STR8 TO NBILFLD
         REP       ZFILL IN NBILFLD
         MOVE      C1 TO NBILPATH
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
         unpack    jstdate  into str2,yy,mm,dd
         pack      str6 from mm,dd,yy
.         EDIT      JSTDATE  TO DATEPRT1
         EDIT      str6  TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         unpack    jstinvdt  into str2,yy,mm,dd
         pack      str6 from mm,dd,yy
         EDIT      str6  TO DATEPRT2
.         EDIT      JSTINVDT TO DATEPRT2
         REPLACE   "0 " IN JSTISTAT
.
         CALL      NJSTCALC
.
         MOVE      C1 TO NORDPATH
         PACK      NORDFLD WITH JSTLR
         CALL      NORDKEY
         DISPLAY   *P01:19,OLRN,B1,O1DES
         PACK      NINVFLD FROM JSTLR
         CALL      NINVKEY
         IF        OVER
         MOVE      "F" TO INVFLAG
         ELSE
         MOVE      TRUE TO INVFLAG
         ENDIF
          PACK                Sls FROM OSALES10,OSALES
          unpack    Olrn,OrderID,str5
.
CHK11    MATCH     "11" TO JSTREASN                 .AP
         GOTO      CHK12 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.          ADD       FAPT TO PLR11
.          Else
          ADD       FAPT TO R11
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK12    MATCH     "12" TO JSTREASN         .AR
         GOTO      CHK14 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.            ADD       lrinc TO PLR12
.          else
          ADD       lrinc TO R12
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
chk14    MATCH     "14" TO JSTREASN           .AP
         GOTO      CHK16 IF NOT EQUAL
.begin patch 3.82
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FAPT TO PLR14
.          Else
         ADD       FAPT TO R14
.          endif
.begin patch 4.62
          If        (Sls = "02" or Sls = "06" or Sls = "19" or Sls = "27" or Sls = "28") .list man
          add       FaPt to R14LM
          else
          add       Fapt to R14BR
          endif
.end patch 4.62

.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK16    MATCH     "16" TO JSTREASN                    .AR
         GOTO      CHK20 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FART TO PLR16
.         else
         ADD       FART TO R16
.begin patch 4.61
          If        (Sls = "02" or Sls = "06" or Sls = "19" or Sls = "27" or Sls = "28" or Sls = "30") .list man
          add       Fart to R16LM
          else
          add       Fart to R16BR
          endif
.end patch 4.61
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK20    MATCH     "20" TO JSTREASN                  .Postage
         GOTO      CHK21 IF NOT EQUAL
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       POST TO PLR20
.          Else
         ADD       POST TO R20
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK21    MATCH     "21" TO JSTREASN               .AP
         GOTO      CHK22 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FAPT TO PLR21
.          else
         ADD       FAPT TO R21
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK22    MATCH     "22" TO JSTREASN               .AR
         GOTO      CHK23 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FART TO PLR22
.          else
         ADD       FART TO R22
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK23    MATCH     "23" TO JSTREASN               .AP
         GOTO      CHK24 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FAPT TO PLR23
.          else
         ADD       FAPT TO R23
.end patch 3.82
.          endif
.end patch 4.5
         GOTO      DTLPRT1
.
CHK24    MATCH     "24" TO JSTREASN               ..ap
         GOTO      CHK25 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FAPT TO PLR24
.          else
         ADD       FAPT TO R24
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK25    MATCH     "25" TO JSTREASN               .LR
         GOTO      CHK26 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR25LR
.         ADD       FAPT TO PLR25AP
.         ADD       FART TO PLR25AR
.          else
         ADD       LRINC TO R25LR
         ADD       FAPT TO R25AP
         ADD       FART TO R25AR
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK26    MATCH     "26" TO JSTREASN               ..LR
         GOTO      CHK27 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR26
.          else
         ADD       LRINC TO R26
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK27    MATCH     "27" TO JSTREASN               .prepay AR
         GOTO      CHK30 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FART TO PLR27
.          else
         ADD       FART TO R27
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
.begin patch 3.9
CHK29    MATCH     "29" TO JSTREASN               ..Canadian 10%
         GOTO      CHK30 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       FART TO PLR29
.          else
         ADD       FART TO R29
.          endif
.end patch 4.5
         GOTO      DTLPRT1
.end patch 3.9
.
CHK30    MATCH     "30" TO JSTREASN               .LR
         GOTO      CHK31 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR30
.          else
         ADD       LRINC TO R30
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
CHK31    MATCH     "31" TO JSTREASN               .LR
         GOTO      CHK35 IF NOT EQUAL
.begin patch 3.82
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid = "P")                            
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR31
.          else
         ADD       LRINC TO R31
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.
.begin patch 2.5
CHK35    MATCH     "35" TO JSTREASN
         GOTO      CHK36 IF NOT EQUAL
.begin patch 3.82
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
.end patch 4.0
.         ADD       LRINC TO PLR35LR
.         ADD       FAPT TO PLR35AP
.         ADD       FART TO PLR35AR
.          else
         ADD       LRINC TO R35LR
         ADD       FAPT TO R35AP
         ADD       FART TO R35AR
.          endif
.end patch 4.5
.end patch 3.82
         
         GOTO      DTLPRT1

.
CHK36    MATCH     "36" TO JSTREASN
.begin patch 2.6
.         GOTO      DTLPRT1 IF NOT EQUAL
         GOTO      CHK37 IF NOT EQUAL
.end patch 2.6
.begin patch 3.82
.begin patch 4.0
.         if        (OcompId = "P")                         
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR36LR
.         ADD       FAPT TO PLR36AP
.         ADD       FART TO PLR36AR
.          else
         ADD       LRINC TO R36LR
         ADD       FAPT TO R36AP
         ADD       FART TO R36AR
.begin patch 4.52
         ADD       NINc TO R36NIN
.end patch 4.52
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.end patch 2.5
.begin patch 2.6
CHK37    MATCH     "37" TO JSTREASN
.begin patch 4.3
         GOTO      CHK38 IF NOT EQUAL
.         GOTO      DTLPRT1 IF NOT EQUAL
.end patch 4.3
.begin patch 3.82
.begin patch 4.0
.         if        (OcompId2 = "P")                        
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
..end patch 4.0
.         ADD       LRINC TO PLR37LR
.         ADD       FAPT TO PLR37AP
.         ADD       FART TO R37AR
.          Else
         ADD       LRINC TO R37LR
         ADD       FAPT TO R37AP
         ADD       FART TO R37AR
.          endif
.end patch 4.5
.end patch 3.82
         GOTO      DTLPRT1
.end patch 2.6
.begin patch 4.3
CHK38    MATCH     "38" TO JSTREASN               .pay AR
         GOTO      DTLPRT1 IF NOT EQUAL
.begin patch 4.5
.          if        (OcompId = "P" or SLS = "27" or SLS = "28" or OrderID = "M" or OrderID = "B")                       
.         ADD       FART TO PR38
.          else
         ADD       FART TO R38
.          endif
.end patch 4.5
         GOTO      DTLPRT1
.end patch 4.3

DTLPRT1  RESET     EXCODES
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      C0 TO N2
         MOVE      STR2 TO N2
           if         (JSTLR = "793924")
           call       debug
           endif
.begin patch 2013 july 16
.this is wrong contact will be blank or zero not salesperson
.         COMPARE   C0 TO N2
.         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      C1 TO N2
         MOVE      C2 TO OELCODE
         ELSE     
.new code
          reset     exfeelst
          SCAN      OLNUM IN exfeelst
                    IF        EQUAL
                      MOVE      C6 TO N2
                      clear      osales10
                      move       c6,osales
.                     GOTO      EXQUES
                    endif 
.set flag for exfee, why giving to LM if no salesperson?
.new code
         ENDIF
.         ENDIF
.end patch 2013 july 16

.         GOTO      EXQUES IF NOT EQUAL
.         MOVE      C1 TO N2
.         MOVE      C2 TO OELCODE
.         ENDIF
EXQUES   RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         IF        EQUAL                   *EXCHANGE
.         BRANCH    N2 OF BRK,BRK,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:
         BRANCH    N2 OF BRK,LSTM,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:              .1-10
                  BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,LSTM,BRK:                       .11-20
                   BRK,BRK,BRK,BRK,BRK,brk,lstm,lstm                             .21-28  
.default if overflow the branch - brokerage
          goto      BRK
.begin patch 3.82
LSTM     
.begin patch 4.1
.         if        (Ocompid2 = "P" or ((SLS = "27" or SLS = "28") & Ocompid = "P") or OrderID = "M")                             
.begin patch 4.5
.          if        (Ocompid2 = "P" or SLS = "27" or SLS = "28" or OrderID = "M")                             
..         if        (Ocompid2 = "P")                        
..end patch 4.1
.          ADD       LRINC TO PLTLRINC2
..begin patch 4.4
.          move      "PLME",DivFlag
..end patch 4.4
.          else
          ADD       LRINC TO TLRINC2
.begin patch xxx
           add        NINC,TMNININC
.end patch xxx
.begin patch 4.4
          move      "LME",DivFlag
.end patch 4.4
.          endif
.end patch 4.5
.Begin Patch 4.53
          ADD                 FART     TO TLMAR
          ADD                 FAP1T    TO TLMAP
          ADD                 FAP2T    TO TLMAP

.end Patch 4.53
          GOTO      DTLPRT2

BRK
           if         (olrn = "793924")
           call       debug
           endif
.begin patch 4.5
.          if        (Ocompid = "P" or OrderID = "B")                            
.                    ADD       LRINC TO PLTLRINC1
..begin patch 4.4
.          move      "PBE",DivFlag
..end patch 4.4
.                    Else
                    ADD       LRINC TO TLRINC1
.begin patch xxx
           add        NINC,TBNININC
.end patch xxx
..begin patch 4.4
          move      "PBE",DivFlag
..end patch 4.4
.                    endif               
.end patch 4.5
.Begin Patch 4.53
          ADD                 FART     TO TBAR
          ADD                 FAP1T    TO TBAP
          ADD                 FAP2T    TO TBAP
.end Patch 4.53

                ELSE                       *RENTAL                                    .from check of oelcode
.         BRANCH    N2 OF BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
         BRANCH    N2 OF BRK1,Lstm1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,LSTM1
.default if overflow the branch - brokerage
          goto      BRK1
LSTM1
.begin patch 4.1
.begin patch 4.5
.          if        (Ocompid2 = "P" or SLS = "27" or SLS = "28" or OrderID = "M")                             
..         if        (Ocompid2 = "P")                        
..end patch 4.1
.          ADD       LRINC TO PLTLRINC4
..begin patch 4.4
.          move      "PLMR",DivFlag
..end patch 4.4
.          Else
          ADD       LRINC TO TLRINC4
.begin patch 4.4
          move      "LMR",DivFlag
.begin patch xxx
           add        NINC,TMNININC
.end patch xxx
.end patch 4.4
.          endif
.end patch 4.5
.Begin Patch 4.53
          ADD                 FART     TO TLMAR
          ADD                 FAP1T    TO TLMAP
          ADD                 FAP2T    TO TLMAP
.end Patch 4.53
         GOTO      DTLPRT2
.begin patch 4.1
BRK1
.begin patch 4.5
.          if        (Ocompid = "P" or OrderID = "B")                                 
..BRK1               if        (Ocompid = "P")                              
..begin patch 4.1
.                    ADD       LRINC TO PLTLRINC3
..begin patch 4.4
.          move      "PBR",DivFlag
..end patch 4.4
.                    else
                    ADD       LRINC TO TLRINC3
.begin patch xxx
           add        NINC,TBNININC
.end patch xxx
.begin patch 4.4
          move      "BR",DivFlag
.end patch 4.4
.                    endif               
.end patch 4.5
.Begin Patch 4.53
          ADD                 FART     TO TBAR
          ADD                 FAP1T    TO TBAP
          ADD                 FAP2T    TO TBAP
.end Patch 4.53
         ENDIF
.end patch 3.82
.
DTLPRT2  
.>Patch  3.65 Comment Out
.              COMPARE   C60 TO PRTLINES
.>Patch  3.65 Comment Out
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
         CALL      HEADING IF NOT LESS
.>Patch  3.65 Comment Out         
.         PRINT     *N,*01,JSTMLR,SLASH,JSTCNT:
.                      *10,NAME1:
.                      *37,DATEPRT1:
.                      *46,JSTINVNO,DASH,JSTSUBNO:
.                      *64,FART:
.                      *79,FAP1T:
.                      *89,LRINC:
.                      *99,CTAX:
.                      *108,STAX:
.                      *118,POST:
.                      *129,JSTREASN,JSTISTAT:
.                   *N:
.                      *1,JSTLR:
.                   *10,NAME2:
.                      *79,FAP2T:
.                      *89,NINC:
.                      *129,JSTCD:
.                   *N,*10,O1DES,*128,HPbon,controlnum,hpboff
.>Patch 3.65 Comment Out          
.>Patch 3.65 Code Added
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,JSTMLR,SLASH,JSTCNT;               
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,NAME1;                    
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,DATEPRT1;                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,JSTINVNO,DASH,JSTSUBNO;                                 
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,FART;                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,FAP1T;                  
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,LRINC;                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,CTAX;                   
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,STAX;            
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,POST;    
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Left,*ll,JSTREASN,JSTISTAT;
        add     eightlpi,row   
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,JSTLR;              
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,NAME2;                                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,FAP2T;         
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,NINC;                                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"< NIN";                   
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Left,*ll,JSTCD;
        add     eightlpi,row                 
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,O1DES;           
.BEGIN PATCH 3.82
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,"LR >";                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,FAP3t;         
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,xNINC1;                                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"< NIN";                   
.END PATCH 3.82
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Left,*boldon,*ll,controlnum,*boldoff;           
        add     eightlpi,row  
        add     eightlpi,row          
              
        
.>Patch 3.65 Code Added

.         PRINT     *N,*01,JSTMLR,SLASH,JSTCNT:
.                      *10,NAME1:
.                      *37,DATEPRT1:
.                      *46,JSTINVNO,DASH,JSTSUBNO:
.                      *55,DATEPRT2:
.                      *64,FART:
.                      *79,FAP1T:
.                      *89,LRINC:
.                      *99,CTAX:
.                      *108,STAX:
.                      *118,POST:
.                      *129,JSTREASN,JSTISTAT:
.                   *N:
.                      *1,JSTLR:
.                   *10,NAME2:
.                      *79,FAP2T:
.                      *129,JSTCD:
.                   *N,*10,O1DES

         ADD       C4 TO PRTLINES
         CMATCH    "F" TO INVFLAG       .MISSING INVOICE?
         IF        EQUAL
.>Patch 3.65 Comment Out         
.         PRINT     *1,*RPTCHAR "*":80,HPBON,"MISSING INVOICE",HPBOFF
.>Patch 3.65 Comment OUt
.>Patch 3.65 Code Added
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,"*******************************************MISSING INVOICE*********************************",*boldoff;                       
          add     eightlpi,row  
          add     eightlpi,row                 
.>Patch 3.65 Code Added
         
         ADD       C1 TO PRTLINES
         ENDIF
.        compare    c1 to n5
.begin patch 4.4
.         if        equal
          if        (recsin = c1)
         write     ClnAdj,seq;*cdfon,"Mailer","LR","Invoice","adj number":
                   "A/R","A/P1","A/P2","LRInc","Adj Date","adj code","List","Reason","Division","sales"
.                   "A/R","A/P1","A/P2","LRInc","NINC","Adj Date","adj code","List","Reason"
         endif           
           pack       str2 from Osales10,osales
         write     clnAdj,seq;*cdfon,jstmlr,jstlr,jstinvno,jstsubno:
                   FART,FAP1T,FAP2T,LRINC,JSTNININC,jstdate,JSTCD,O1DES,jstreasn,DivFlag,str2
.                   FART,FAP1T,FAP2T,LRINC,jstdate,JSTCD,O1DES,jstreasn
.end patch 4.4
.Begin patch 4.2
.         ADD       FART     TO TAR
.         ADD       FAP1T    TO TAP1
.         ADD       FAP2T    TO TAP2
.Begin patch 4.2
.Begin patch 3.82
.         ADD       FAP3T    TO TAP3
.         ADD       XNINC1   TO TXNININC
.end patch 3.82
.end patch 4.2
         
.Need to add code here if neither side PL add to NIN  who to divi up the rest - sigh
.begin patch 3.82
.Begin patch 4.2
.         if        (OcompId <> "P" and Ocompid2 <> "P")              .all NIN  
.begin patch 4.5
.          if        (OcompId <> "P" and Ocompid2 <> "P" & OrderID <> "M" & OrderID <> "B")                    .all NIN  
.          ADD                 LRINC  TO TLRINC
.          ADD                 NINC  TO TNININC
.          ADD                 FART     TO TAR
.          ADD                 FAP1T    TO TAP1
.          ADD                 FAP2T    TO TAP2
..inter comp
.          ADD                 FAP3T    TO PLTAP3
.          ADD                 XNINC1   TO PLTXNININC
.          
.          Elseif      (OcompId = "P" and Ocompid2 = "P")                        .all PLI
.
.          ADD                 LRINC  TO PLTLRINC
.          ADD                 NINC  TO PLTNININC
.          ADD                 FART     TO PLTAR
.          ADD                 FAP1T    TO PLTAP1
.          ADD                 FAP2T    TO PLTAP2
..inter comp
.          ADD                 FAP3T    TO TAP3
.          ADD                 XNINC1   TO TXNININC
.
.          Elseif    (OcompId = "P")
.
.          ADD                 LRINC  TO PLTLRINC
.          ADD                 NINC   TO PLTNININC
.          ADD                 FART     TO PLTAR
.          ADD                 FAP1T    TO PLTAP1
.          ADD                 FAP2T    TO PLTAP2
..inter comp
.          ADD                 FAP3T    TO TAP3
.          ADD                 XNINC1   TO TXNININC
.
.          Elseif    (sls = "27" or SLS = "28" or ORderID = "M" or OrderId = "B")    --- PLI
.
.          ADD                 LRINC  TO PLTLRINC
.          ADD                 NINC  TO PLTNININC
.          ADD                 FART     TO PLTAR
.          ADD                 FAP1T    TO PLTAP1
.          ADD                 FAP2T    TO PLTAP2
..inter comp
.          ADD                 FAP3T    TO TAP3
.          ADD                 XNINC1   TO TXNININC
.          
.          Else

          ADD                 NINC  TO TNININC
          ADD                 LRINC  TO TLRINC                    .for now dump to nin
          ADD                 FART     TO TAR
          ADD                 FAP1T    TO TAP1
          ADD                 FAP2T    TO TAP2
.inter comp
          ADD                 FAP3T    TO PLTAP3
          ADD                 XNINC1   TO PLTXNININC
.          endif
.end patch 4.5
.end patch 3.82
.end patch 4.2
          
.Begin patch 4.2
.         ADD       STAX   TO TSTAX
.         ADD       CTAX   TO TCTAX
.         ADD       POST   TO TPOST
.         if        (incontrol = yes)
.         ADD       FART     TO TCAR
.         ADD       FAP1T    TO TCAP1
.         ADD       FAP2T    TO TCAP2
.         ADD       LRINC  TO TCLRINC
.         ADD       NINC  TO TCNININC
.         ADD       STAX   TO TCSTAX
.         ADD       CTAX   TO TCCTAX
.         ADD       POST   TO TCPOST
.begin patch 4.5
.          if        (OcompId <> "P" and Ocompid2 <> "P" & OrderID <> "M" & OrderID <> "B")                    .all NIN       
.          ADD       STAX   TO TSTAX
.          ADD       CTAX   TO TCTAX
.          ADD       POST   TO TPOST
.                    if        (incontrol = yes)
.                    ADD       FART     TO TCAR
.                    ADD       FAP1T    TO TCAP1
.                    ADD       FAP2T    TO TCAP2
.                    ADD       LRINC  TO TCLRINC
.                    ADD       NINC  TO TCNININC
.                    ADD       STAX   TO TCSTAX
.                    ADD       CTAX   TO TCCTAX
.                    ADD       POST   TO TCPOST
..inter comp
.                    ADD       FAP3T    TO PLTCAP3
.                    ADD       XNINC1   TO PLTCXNININC
.          else
.                    ADD       FART     TO TNCAR
.                    ADD       FAP1T    TO TNCAP1
.                    ADD       FAP2T    TO TNCAP2
.                    ADD       LRINC  TO TNCLRINC
.                    ADD       NINC  TO TNCNININC
.                    ADD       STAX   TO TNCSTAX
.                    ADD       CTAX   TO TNCCTAX
.                    ADD       POST   TO TNCPOST
..inter comp
.                    ADD       FAP3T    TO PLTNCAP3
.                    ADD       XNINC1   TO PLTNCXNININC
.                    endif
.          Elseif      (OcompId = "P" and Ocompid2 = "P")                        .all PLI
.          ADD       STAX   TO PLTSTAX
.          ADD       CTAX   TO PLTCTAX
.          ADD       POST   TO PLTPOST
.                    if        (incontrol = yes)
.                    ADD       FART     TO PLTCAR
.                    ADD       FAP1T    TO PLTCAP1
.                    ADD       FAP2T    TO PLTCAP2
.                    ADD       LRINC    TO PLTCLRINC
.                    ADD       NINC     TO PLTCNININC
.                    ADD       STAX     TO PLTCSTAX
.                    ADD       CTAX   TO PLTCCTAX
.                    ADD       POST   TO PLTCPOST
..inter comp
.                    ADD       FAP3T    TO TCAP3
.                    ADD       XNINC1   TO TCXNININC
.
.          else
.                    ADD       FART     TO PLTNCAR
.                    ADD       FAP1T    TO PLTNCAP1
.                    ADD       FAP2T    TO PLTNCAP2
.                    ADD       LRINC  TO PLTNCLRINC
.                    ADD       NINC  TO PLTNCNININC
.                    ADD       STAX   TO PLTNCSTAX
.                    ADD       CTAX   TO PLTNCCTAX
.                    ADD       POST   TO PLTNCPOST
..inter comp
.                    ADD       FAP3T    TO TNCAP3
.                    ADD       XNINC1   TO TNCXNININC
.                    endif
.          Elseif    (OcompId = "P")
.          ADD       STAX   TO PLTSTAX
.          ADD       CTAX   TO PLTCTAX
.          ADD       POST   TO PLTPOST
.                    if        (incontrol = yes)
.                    ADD       FART     TO PLTCAR
.                    ADD       FAP1T    TO PLTCAP1
.                    ADD       FAP2T    TO PLTCAP2
.                    ADD       LRINC    TO PLTCLRINC
.                    ADD       NINC     TO PLTCNININC
.                    ADD       STAX     TO PLTCSTAX
.                    ADD       CTAX   TO PLTCCTAX
.                    ADD       POST   TO PLTCPOST
..inter comp
.                    ADD       FAP3T    TO TCAP3
.                    ADD       XNINC1   TO TCXNININC
.          else
.                    ADD       FART     TO PLTNCAR
.                    ADD       FAP1T    TO PLTNCAP1
.                    ADD       FAP2T    TO PLTNCAP2
.                    ADD       LRINC  TO PLTNCLRINC
.                    ADD       NINC  TO PLTNCNININC
.                    ADD       STAX   TO PLTNCSTAX
.                    ADD       CTAX   TO PLTNCCTAX
.                    ADD       POST   TO PLTNCPOST
..inter comp
.                    ADD       FAP3T    TO TNCAP3
.                    ADD       XNINC1   TO TNCXNININC
.                    endif
.
.          Elseif    (sls = "27" or SLS = "28" or ORderID = "M" or OrderId = "B")    --- PLI
.          ADD       STAX   TO PLTSTAX
.          ADD       CTAX   TO PLTCTAX
.          ADD       POST   TO PLTPOST
.                    if        (incontrol = yes)
.                    ADD       FART     TO PLTCAR
.                    ADD       FAP1T    TO PLTCAP1
.                    ADD       FAP2T    TO PLTCAP2
.                    ADD       LRINC    TO PLTCLRINC
.                    ADD       NINC     TO PLTCNININC
.                    ADD       STAX     TO PLTCSTAX
.                    ADD       CTAX   TO PLTCCTAX
.                    ADD       POST   TO PLTCPOST
..inter comp
.                    ADD       FAP3T    TO TCAP3
.                    ADD       XNINC1   TO TCXNININC
.          else
.                    ADD       FART     TO PLTNCAR
.                    ADD       FAP1T    TO PLTNCAP1
.                    ADD       FAP2T    TO PLTNCAP2
.                    ADD       LRINC  TO PLTNCLRINC
                    ADD       NINC  TO PLTNCNININC
.                    ADD       STAX   TO PLTNCSTAX
.                    ADD       CTAX   TO PLTNCCTAX
.                    ADD       POST   TO PLTNCPOST
..inter comp
.                    ADD       FAP3T    TO TNCAP3
.                    ADD       XNINC1   TO TNCXNININC
.                    endif
.
.          Else                                    .must be NIN
          ADD       STAX   TO TSTAX
          ADD       CTAX   TO TCTAX
          ADD       POST   TO TPOST
                    if        (incontrol = yes)
                    ADD       FART     TO TCAR
                    ADD       FAP1T    TO TCAP1
                    ADD       FAP2T    TO TCAP2
                    ADD       LRINC  TO TCLRINC
                    ADD       NINC  TO TCNININC
                    ADD       STAX   TO TCSTAX
                    ADD       CTAX   TO TCCTAX
                    ADD       POST   TO TCPOST
.inter comp
                    ADD       FAP3T    TO PLTCAP3            .???????
                    ADD       XNINC1   TO PLTCXNININC

                              else

                    ADD       FART     TO TNCAR
                    ADD       FAP1T    TO TNCAP1
                    ADD       FAP2T    TO TNCAP2
                    ADD       LRINC  TO TNCLRINC
                    ADD       NINC  TO TNCNININC
                    ADD       STAX   TO TNCSTAX
                    ADD       CTAX   TO TNCCTAX
                    ADD       POST   TO TNCPOST
.inter comp
                    ADD       FAP3T    TO PLTNCAP3
                    ADD       XNINC1   TO PLTNCXNININC
                    endif
.end patch 4.2         
.         endif
.end patch 4.5
         MOVE      C0     TO FART
         MOVE      C0     TO FAPT
         MOVE      C0     TO FAP1T
         MOVE      C0     TO FAP2T
         MOVE      C0     TO FAP3T
         MOVE      C0     TO LRINC
         MOVE      C0     TO NINC
         MOVE      C0     TO xNINC1
         MOVE      C0     TO STAX
         MOVE      C0     TO CTAX
         MOVE      C0     TO POST
.
          if        (tlrinc <> c0)
.          call      Debug
          endif
         GOTO      RDADJ
.
EOJ     
.          COMPARE   C45 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.>Patch 3.65
              COMPARE  "2000" to ROW
.>Patch 3.65
         CALL      HEADING 
         SUB       TAP FROM TAP
         ADD       TAP1 TO TAP
         ADD       TAP2 TO TAP
         ADD       TLRINC2 TO TLRINC4
.begin patch 2.3
         SUB       TCAP FROM TCAP
         ADD       TCAP1 TO TCAP
         ADD       TCAP2 TO TCAP
         SUB       TNCAP FROM TNCAP
         ADD       TNCAP1 TO TNCAP
         ADD       TNCAP2 TO TNCAP
.end patch 2.3
.>Patch 3.65 Comment Out
.         PRINT     *N:
.                   *N,*01,MONTH,":":
.                      *65,TAR:
.                      *78,TAP1:
.                      *89,TLRINC:
.                      *101,TCTAX:
.                      *110,TSTAX:
.                      *120,TPOST:
.                   *N,*78,TAP2:
.                   *89,Tnininc:
.                   *N,*78,"------------":
.begin patch 2.3
.                   *N,*01,Recsin,B1,"ADJUSTMENTS",*78,TAP:
.                   *N,*01,"------------------------------------------------------------------------------------------":
.                          "------------------------------------------":
.end patch 2.3
.                   *N,*01,"TOTAL LRINC BROKERAGE RENTAL: ",*88,TLRINC3:
.                   *N,*01,"TOTAL LRINC BROKERAGE XCHNGE: ",*88,TLRINC1:
.                   *N,*01,"TOTAL LRINC LIST MANAGEMENT : ",*88,TLRINC4:
.                   *N,*01,"     AP REASON CODE 11 TOTAL: ",R11:
.                   *N,*01,"  LRINC REASON CODE 12 TOTAL: ",R12:
.                   *N,*01,"     AP REASON CODE 14 TOTAL: ",R14:
.                   *N,*01,"     AR REASON CODE 16 TOTAL: ",R16:
.                   *N,*01,"POSTAGE REASON CODE 20 TOTAL: ",R20:
.                   *N,*01,"     AP REASON CODE 21 TOTAL: ",R21:
.                   *N,*01,"     AR REASON CODE 22 TOTAL: ",R22:
.                   *N,*01,"     AP REASON CODE 23 TOTAL: ",R23:
.                   *N,*01,"     AP REASON CODE 24 TOTAL: ",R24:
.                   *N,*01,"     AR REASON CODE 25 TOTAL: ",R25AR:
.                   *N,*01,"     AP REASON CODE 25 TOTAL: ",R25AP:
.                   *N,*01,"  LRINC REASON CODE 25 TOTAL: ",R25LR:
.                   *N,*01,"  LRINC REASON CODE 26 TOTAL: ",R26:
.                   *N,*01,"Prepay  REASON CODE 27 TOTAL: ",R27:
.                   *N,*01,"  LRINC REASON CODE 30 TOTAL: ",R30:
.                   *N,*01,"  LRINC REASON CODE 31 TOTAL: ",R31:
.begin patch 2.5
.                   *N,*01,"     AR REASON CODE 35 TOTAL: ",R35AR:
.                   *N,*01,"     AP REASON CODE 35 TOTAL: ",R35AP:
.                   *N,*01,"  LRINC REASON CODE 35 TOTAL: ",R35LR:
.                   *N,*01,"     AR REASON CODE 36 TOTAL: ",R36AR:
.                   *N,*01,"     AP REASON CODE 36 TOTAL: ",R36AP:
.                   *N,*01,"  LRINC REASON CODE 36 TOTAL: ",R36LR:
.end patch 2.5
.begin patch 2.6
.                   *N,*01,"     AR REASON CODE 37 TOTAL: ",R37AR:
.                   *N,*01,"     AP REASON CODE 37 TOTAL: ",R37AP:
.                   *N,*01,"  LRINC REASON CODE 37 TOTAL: ",R37LR:
.end patch 2.6
.begin patch 2.3
.                   *N,*01,"------------------------------------------------------------------------------------------":
.                          "------------------------------------------":
.                   *N,*01,"Control Related Totals":
.                   *N,*65,TCAR:
.                      *78,TCAP1:
.                      *89,TCLRINC:
.                      *101,TCCTAX:
.                      *110,TCSTAX:
.                      *120,TCPOST:
.                   *N,*78,TCAP2:
.                   *89,TCnininc:
.                   *N,*78,"------------":
.                   *N,*01,*78,TCAP:
.                   *N,*01,"------------------------------------------------------------------------------------------":
.                          "------------------------------------------":
.                   *N,*01,"Non Control Related Totals":
.                   *N,*65,TNCAR:
.                      *78,TNCAP1:
.                      *89,TNCLRINC:
.                      *101,TNCCTAX:
.                      *110,TNCSTAX:
.                      *120,TNCPOST:
.                   *N,*78,TNCAP2:
.                   *89,TNCnininc:
.                   *N,*78,"------------":
.                   *N,*01,*78,TNCAP:
.                   *N,*01,"------------------------------------------------------------------------------------------":
.                          "------------------------------------------":
.end patch 2.3
.                   *F
.                   *N,*01,"TOTAL LRINC LIST MAN. RENTAL: ",*88,TLRINC4:
.                   *N,*01,"TOTAL LRINC LIST MAN. XCHNGE: ",*88,TLRINC2:
.         BRANCH    PRTFLAG TO END
.         print     hpreset
.>Patch 3.65 Comment Out     
.>Patch 3.65 Code Added
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,MONTH,":";                   
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,TAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP1;                                                     
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC;                                                                  
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,TCTAX;                                                                                  
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,TSTAX;                                                                                                 
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,TPOST;                                                                                                 
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP2;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
.BEGIN PATCH 
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,"LR >";                                                     
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP3;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TXNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
          add     eightlpi,row                                
.END PATCH 

          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,"------------";          
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,Recsin,B1,"ADJUSTMENTS";                              
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP;                    
          add     eightlpi,row                 
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;          
          add     eightlpi,row  
          add     eightlpi,row            
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE RENTAL: ";
.begin Patch 4.53
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,TBAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TBAP;                                                     
.end Patch 4.53
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC3;     
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE XCHNGE: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC1;   
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC LIST MANAGEMENT : ";
.begin Patch 4.53
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,TLMAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TLMAP;                                                     
.end Patch 4.53
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC4;                    
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 11 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R11;                   
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 12 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R12;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 14 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R14;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 16 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R16;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"POSTAGE REASON CODE 20 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R20;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 21 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R21;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 22 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R22;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 23 TOTAL: ";                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R23;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 24 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R24;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 26 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R26;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Prepay  REASON CODE 27 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R27;                   
.begin patch 3.9
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"REASON CODE 29 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R29;                   
.end patch 3.9
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 30 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R30;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 31 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R31;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 36 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36LR;                 
.begin patch 4.52
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  NINC REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36NIN;                 
.end patch 4.52
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37AR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37AP;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37LR;                 
.begin patch 4.3
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 38 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R38;                 
.end patch 4.3
          add     eightlpi,row                                                         
          add     eightlpi,row  
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"Control Related Totals";                   
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,TCAR;                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TCAP1;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TCLRINC;                             
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,TCCTAX;                              
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,TCSTAX;                       
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,TCPOST;                                
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TCAP2;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TCNININC;                              
.begin patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TCAP3;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TCXNININC;                              
.end patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"------------";                                       
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TCAP;    
          add     eightlpi,row            
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row                           
          add     eightlpi,row  
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"NonControl Related Totals";                
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,TNCAR;                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TNCAP1;                              
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TNCLRINC;                            
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,TNCCTAX;                             
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,TNCSTAX;                      
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,TNCPOST;                               
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TNCAP2;                              
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TNCnininc;                             
.begin patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TNCAP3;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,TNCXNININC;                              
.end patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"------------";                                       
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TNCAP;                  
          
                
.begin patch 3.82  
          call      Heading   
.begin Patch 4.2
         SUB       PLTAP FROM PLTAP
         ADD       PLTAP1 TO PLTAP
         ADD       PLTAP2 TO PLTAP
         ADD       PLTLRINC2 TO PLTLRINC4
         SUB       PLTCAP FROM PLTCAP
         ADD       PLTCAP1 TO PLTCAP
         ADD       PLTCAP2 TO PLTCAP
         SUB       PLTNCAP FROM PLTNCAP
         ADD       PLTNCAP1 TO PLTNCAP
         ADD       PLTNCAP2 TO PLTNCAP
.end Patch 4.2


                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"Pacific Lists",":";                   
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,PLTAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP1;                                                     
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC;                                                                  
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,PLTCTAX;                                                                                  
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,PLTSTAX;                                                                                                 
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,PLTPOST;                                                                                                 
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP2;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
.BEGIN PATCH 4.2
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,"LR >";                                                     
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP3;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTXNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
          add     eightlpi,row                                
.END PATCH 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,"------------";          
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,Recsin,B1,"ADJUSTMENTS";                              
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP;                    
          add     eightlpi,row                 
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;          
          add     eightlpi,row  
          add     eightlpi,row            
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE RENTAL: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC3;     
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE XCHNGE: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC1;   
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC LIST MANAGEMENT : ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC4;                    
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 11 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR11;                   
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 12 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR12;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 14 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR14;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 16 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR16;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"POSTAGE REASON CODE 20 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR20;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 21 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR21;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 22 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR22;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 23 TOTAL: ";                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR23;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 24 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR24;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 26 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR26;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Prepay  REASON CODE 27 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR27;                   
.begin patch 3.9
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  REASON CODE 29 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR29;                   
.end patch 3.9
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 30 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR30;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 31 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR31;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 36 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36LR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37AR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37AP;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37LR;                 
.begin patch 4.3
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 38 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PR38;                 
.end patch 4.3
          add     eightlpi,row                                                         
          add     eightlpi,row  
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"Control Related Totals";                   
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,PLTCAR;                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTCAP1;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTCLRINC;                             
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,PLTCCTAX;                              
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,PLTCSTAX;                       
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,PLTCPOST;                                
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTCAP2;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTCNININC;                              
.begin patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTCAP3;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTCXNININC;                              
.end patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"------------";                                       
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTCAP;    
          add     eightlpi,row            
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row                           
          add     eightlpi,row  
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"NonControl Related Totals";                
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,PLTNCAR;                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTNCAP1;                              
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTNCLRINC;                            
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,PLTNCCTAX;                             
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,PLTNCSTAX;                      
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,PLTNCPOST;                               
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTNCAP2;                              
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTNCnininc;                             
.begin patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTNCAP3;                
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,PLTNCXNININC;                              
.end patch 4.2
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"------------";                                       
          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PLTNCAP;                  
.end patch 3.82                
                
                
          
.>Patch 3.65 Code Added
         release
.Patch 3.65 Comment Out         
.         SPLCLOSE
.Patch 3.65 Comment Out         
.Patch 3.65 Code Added
              prtclose prfile 
.Give the email a chance of rendering itself before updating the INI file.
.begin patch 4.6                    
.          Call      PDF995Auto0
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.              pack            APIFileName,STR45,hexzero
.              loop
.               call           FindFirstFile
.               until (APIResult = 0 | APIResult = hexeight)
.               pause          "1"
.              repeat
.end patch 4.6                    
              pause           "5"
.              erase           "c:\progra~1\pdf995\flag.dat"                
              
.Patch 3.65 Code Added
         WEOF      ClnAdj,SEQ
         CLOSE     ClnAdj
         CALL      REMVTOF
.begin patch 2.4
.........................................................................................................................
.begin patch 2.14
          Clear     Mailbody
          append    "NEOM0010 - output",Mailbody
          Append    CRLF,MailBody                                               
          append    REcsin,Mailbody
          Append    " Adjustments",Mailbody
          Append    CRLF,MailBody                                               
.begin patch 4.53
          append    "AR TOTALS       ",Mailbody
          append    TAR,Mailbody
          Append    CRLF,MailBody                                               

          append    "AP TOTALS       ",Mailbody
          append    TAP,Mailbody
          Append    CRLF,MailBody                                               

          append    "AR Brokerage TOTALS       ",Mailbody
          append    TBAR,Mailbody
          Append    CRLF,MailBody                                               

.begin patch 4.61          
          append    "AR Brokerage Short Pay    ",Mailbody
          append    R16BR,Mailbody
          Append    CRLF,MailBody                                               
.end patch 4.61          
.begin patch 4.62         
          append    "AP Brokerage ADV Pay    ",Mailbody
          append    R14BR,Mailbody
          Append    CRLF,MailBody                                               
.end patch 4.62          

          append    "AP Brokerage TOTALS       ",Mailbody
          append    TBAP,Mailbody
          Append    CRLF,MailBody                                                      

          append    "AR Management TOTALS      ",Mailbody
          append    TLMAR,Mailbody
          Append    CRLF,MailBody                                               

.begin patch 4.61          
          append    "AR Management Short Pay    ",Mailbody
          append    R16LM,Mailbody
          Append    CRLF,MailBody                                               
.end patch 4.61          
.begin patch 4.62          
          append    "AP Management ADV Pay    ",Mailbody
          append    R14LM,Mailbody
          Append    CRLF,MailBody                                               
.end patch 4.62
          
          append    "AP Management TOTALS      ",Mailbody
          append    TLMAP,Mailbody
          Append    CRLF,MailBody                                                      

.end patch 4.53
          append    "LR INCOME TOTALS          ",Mailbody
          append    TLRinc,Mailbody
          Append    CRLF,MailBody                                               
          
          append    "NIN INCOME TOTALS         ",Mailbody
          append    TNININC,Mailbody
          Append    CRLF,MailBody                                               

          append    "LR INCOME rental          ",mailbody
          append    TLRINC3,Mailbody              
          Append    CRLF,MailBody                                               

          append    "LR INCOME Exchange        ",mailbody
          append    TLRINC1,Mailbody              
          Append    CRLF,MailBody                                               
          
          append    "LR INCOME List Management ",mailbody
          append    TLRINC4,Mailbody              
          Append    CRLF,MailBody                                               
.begin cleanup
          append    "LR INterCOMp Transfer     ",mailbody
          append    TAP3,Mailbody       
          Append    CRLF,MailBody                                               
          
          append    "NIN INterCOMp Transfer    ",mailbody
          append    TXNININC,Mailbody             
          Append    CRLF,MailBody                                               

.begin cleanup
          append    "PL LR INCOME TOTALS          ",Mailbody
          append    PLTLRinc,Mailbody
          Append    CRLF,MailBody                                               
          
          append    "PL NIN INCOME TOTALS         ",Mailbody
          append    PLTNININC,Mailbody
          Append    CRLF,MailBody                                               

          append    "PL LR INCOME rental          ",mailbody
          append    PLTLRINC3,Mailbody            
          Append    CRLF,MailBody                                               

          append    "PL LR INCOME Exchange        ",mailbody
          append    PLTLRINC1,Mailbody            
          Append    CRLF,MailBody                                               
          
          append    "PL LR INCOME List Management ",mailbody
          append    PLTLRINC4,Mailbody            
          Append    CRLF,MailBody                                               
.begin cleanup
          append    "PL LR INterCOMp Transfer     ",mailbody
          append    PLTAP3,Mailbody               
          Append    CRLF,MailBody                                               
          
          append    "PL NIN INterCOMp Transfer    ",mailbody
          append    PLTXNININC,Mailbody           
          Append    CRLF,MailBody                                               

.begin cleanup
          
          Reset     MailBody
          MOve      "NEOM0010 - output",Mailsubjct
          pack      Mailto,"DavidHerrick@nincal.com"
          pack      Mailfrom,"DavidHerrick@nincal.com"
          clear     mailattach
          move      c3,MailType
          call      sendmail

.              move          "NEOM0010 - output" to SmtpSubject Subject
.              append         recsin,SmtpTextMessage(1)   Array <Text message >
.              append         b1,SmtpTextMessage(1)   Array <Text message >
.              append         "Adjustments",SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)
..
.              append         "LR INCOME TOTALS          ",SmtpTextMessage(2)   Array <Text message >
.              append         b1,SmtpTextMessage(2)   Array <Text message >
.              append         TLRinc,SmtpTextMessage(2)   Array <Text message >
.              reset          smtpTextMessage(2)
..
.              append         "NIN INCOME TOTALS         ",SmtpTextMessage(3)   Array <Text message >
.              append         b1,SmtpTextMessage(3)   Array <Text message >
.              append         TNININC,SmtpTextMessage(3)   Array <Text message >
.              reset          smtpTextMessage(3)
..
.              append         "LR INCOME rental          ",SmtpTextMessage(4)   Array <Text message >
.              append         b1,SmtpTextMessage(4)   Array <Text message >
.              append         TLRINC3,SmtpTextMessage(4)   Array <Text message >
.              reset          smtpTextMessage(4)
..
.              append         "LR INCOME Exchange        ",SmtpTextMessage(5)   Array <Text message >
.              append         b1,SmtpTextMessage(5)   Array <Text message >
.              append         TLRINC1,SmtpTextMessage(5)   Array <Text message >
.              reset          smtpTextMessage(5)
..
.              append         "LR INCOME List Management ",SmtpTextMessage(6)   Array <Text message >
.              append         b1,SmtpTextMessage(6)   Array <Text message >
.              append         TLRinc4,SmtpTextMessage(6)   Array <Text message >
.              reset          smtpTextMessage(6)
..

.              Move       "6",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
.end patch 2.4
.begin patch 3.81
.Open Excel application
OhMy
              Create  ex
.              pack            Taskname,"\\nins1\d\users\dherric\monday2009.xlsx"
.              pack            Taskname,"\\nins1\d\users\dherric\monday2009.xls"       
              pack            Taskname,"\\nins1\d\accounting\monday2016.xlsx"       
              getprop ex,*Workbooks=books
              Books.Open using *Filename=taskname
              books.item giving book using 1
              getprop book,*Sheets=sheets
.              getprop book,*workSheets=sheets
.Reset Default of Worksheets found in a Workbook         .eight
.should try getting the property here and reseting it when done.
        getprop ex,*SheetsInNewWorkbook=NumberofSheets
.Create Workbooks collection
.        clock   timestamp,str8
.        unpack  str8,str2,yy,mm,dd
           unpack     today into mm,str1,dd,str1,yy
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        clock   time to time
        move      mm to sheetno
              if              (sheetno = c1)
              sheets.item giving sheet using 1
              Elseif          (sheetno = c2)
              sheets.item giving sheet using 2
              Elseif          (sheetno = c3)
              sheets.item giving sheet using 3
              Elseif          (sheetno = c4)
              sheets.item giving sheet using 4
              Elseif          (sheetno = c5)
              sheets.item giving sheet using 5
              Elseif          (sheetno = c6)
              sheets.item giving sheet using 6
              Elseif          (sheetno = c7)
              sheets.item giving sheet using 7
              Elseif          (sheetno = c8)
              sheets.item giving sheet using 8
              Elseif          (sheetno = c9)
              sheets.item giving sheet using 9
              Elseif          (sheetno = 10)
              sheets.item giving sheet using 10
              Elseif          (sheetno = 11)
              sheets.item giving sheet using 11
              Elseif          (sheetno = 12)
              sheets.item giving sheet using 12
              endif
        pack    RowNumber,"C","9"
        setprop sheet.range(RowNumber),*Value=str10
        pack    RowNumber,"D","9"
        setprop sheet.range(RowNumber),*Value=Time
        pack    RowNumber,"A","9"
        setprop sheet.range(RowNumber),*Value="Brokerage Total Adj"
        pack    RowNumber,"B","9"
        setprop sheet.range(RowNumber),*Value=Tlrinc
        pack    RowNumber,"A","10"
        setprop sheet.range(RowNumber),*Value="NIN Total Adj"
        pack    RowNumber,"B","10"
        setprop sheet.range(RowNumber),*Value=TNININC
        pack    RowNumber,"A","11"
        setprop sheet.range(RowNumber),*Value="Brokerage Rent Adj"
        pack    RowNumber,"B","11"
        setprop sheet.range(RowNumber),*Value=Tlrinc3
        pack    RowNumber,"A","12"
        setprop sheet.range(RowNumber),*Value="Brokerage Exch Adj"
        pack    RowNumber,"B","12"
        setprop sheet.range(RowNumber),*Value=Tlrinc1
        pack    RowNumber,"A","13"
        setprop sheet.range(RowNumber),*Value="List Management Adj"
        pack    RowNumber,"B","13"
        setprop sheet.range(RowNumber),*Value=Tlrinc4
        pack    RowNumber,"F","9"
        setprop sheet.range(RowNumber),*Value=PLTlrinc
        pack    RowNumber,"F","10"
        setprop sheet.range(RowNumber),*Value=PLTNININC
        pack    RowNumber,"F","11"
        setprop sheet.range(RowNumber),*Value=PLTlrinc3
        pack    RowNumber,"F","12"
        setprop sheet.range(RowNumber),*Value=PLTlrinc1
        pack    RowNumber,"F","13"
        setprop sheet.range(RowNumber),*Value=PLTlrinc4
        pack    RowNumber,"A","41"
        setprop sheet.range(RowNumber),*Value="Inter Comp LR Adj"
        pack    RowNumber,"A","42"
        setprop sheet.range(RowNumber),*Value="Inter Comp NIN Adj"
        pack    RowNumber,"B","41"
        setprop sheet.range(RowNumber),*Value=TAP3
        pack    RowNumber,"B","42"
        setprop sheet.range(RowNumber),*Value=TXNININC
        pack    RowNumber,"F","41"
        setprop sheet.range(RowNumber),*Value=PLTAP3
        pack    RowNumber,"F","42"
        setprop sheet.range(RowNumber),*Value=PLTXNININC
.begin patch 4.53
        pack    RowNumber,"A","57"
        setprop sheet.range(RowNumber),*Value="Total Adj AR"
        pack    RowNumber,"A","58"
        setprop sheet.range(RowNumber),*Value="Total Adj AP"
        pack    RowNumber,"A","59"
        setprop sheet.range(RowNumber),*Value="Brokerage Adj AR"
        pack    RowNumber,"A","60"
        setprop sheet.range(RowNumber),*Value="Brokerage Adj AP"
        pack    RowNumber,"A","61"
        setprop sheet.range(RowNumber),*Value="Management Adj AR"
        pack    RowNumber,"A","62"
        setprop sheet.range(RowNumber),*Value="Management Adj AP"

        pack    RowNumber,"B","57"
        setprop sheet.range(RowNumber),*Value=TAR
        pack    RowNumber,"B","58"
        setprop sheet.range(RowNumber),*Value=TAP
        pack    RowNumber,"B","59"
        setprop sheet.range(RowNumber),*Value=TBAR
        pack    RowNumber,"B","60"
        setprop sheet.range(RowNumber),*Value=TBAP
        pack    RowNumber,"B","61"
        setprop sheet.range(RowNumber),*Value=TLMAR
        pack    RowNumber,"B","62"
        setprop sheet.range(RowNumber),*Value=TLMAP

.end patch 4.53

.begin patch 4.61          
        pack    RowNumber,"A","75"
        setprop sheet.range(RowNumber),*Value="Brokerage Short Pay AR"
        pack    RowNumber,"A","76"
        setprop sheet.range(RowNumber),*Value="Management Short Pay AR"
        pack    RowNumber,"B","75"
        setprop sheet.range(RowNumber),*Value=R16BR
        pack    RowNumber,"B","76"
        setprop sheet.range(RowNumber),*Value=R16LM
.end patch 4.61          
.begin patch 4.62
        pack    RowNumber,"A","77"
        setprop sheet.range(RowNumber),*Value="Brokerage ADV Pay AP"
        pack    RowNumber,"A","78"
        setprop sheet.range(RowNumber),*Value="Management ADV Pay AP"
        pack    RowNumber,"B","77"
        setprop sheet.range(RowNumber),*Value=R14BR
        pack    RowNumber,"B","78"
        setprop sheet.range(RowNumber),*Value=R14LM
.end patch 4.62          
.begin patch xxx
        pack    RowNumber,"A","83"
        setprop sheet.range(RowNumber),*Value="NIN MAN Adj"
        pack    RowNumber,"B","83"
        setprop sheet.range(RowNumber),*Value=TMNININC
        pack    RowNumber,"A","84"
        setprop sheet.range(RowNumber),*Value="NIN BRK Adj"
        pack    RowNumber,"B","84"
        setprop sheet.range(RowNumber),*Value=TBNININC
.end patch xxx




.        trap    TrapObject if Object
        book.save giving N9 
        trapclr Object
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
.end patch 3.81
         shutdown  "cls"
.                 GOTO      END
+............................................................................
.
HEADING  ADD       C1 TO PAGE
         compare    c1 to page
.>Patch 3.65         Comment Out
.         if         equal
.         print      hp17ptch,hpdupl,hptop,*f
.         endif
              
.>Patch 3.65  Comment Out
.>Patch 3.65 Code Added
              if not equal
               PRTPAGE prfile;*NEWPAGE:
               *UNITS=*HIENGLISH:
                       *ORIENT=*PORTRAIT:
               *Duplex=2                                                   
              endif
.>Patch 3.65 Code Added
.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*55,"ADJUSTMENT REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*01,"CLIENT##":
.                      *10,"BILL-TO/LIST":
.                      *38,"CREDIT":
.                      *47,"INVOICE":
.                      *56,"INVOICE":
.                      *67,"ACCOUNTS":
.                      *81,"ACCOUNTS":
.                      *95,"LR":
.                      *105,"CITY":
.                      *113,"STATE":
.                      *122,"OUR":
.                   *N:
.                      *3,"LR##":
.                      *39,"DATE":
.                      *48,"NUMBER":
.                      *58,"DATE":
.                      *66,"RECEIVABLE":
.                      *82,"PAYABLE":
.                      *93,"INCOME":
.                      *105,"TAX":
.                      *114,"TAX":
.                      *121,"POSTAGE"
.START PATCH 3.63 REPLACED LOGIC
.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*55,"ADJUSTMENT REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*01,"CLIENT##":
.                      *10,"BILL-TO/LIST":
.                      *38,"CREDIT":
.                      *47,"INVOICE":
.                      *67,"ACCOUNTS":
.                      *81,"ACCOUNTS":
.                      *95,"LR/NIN":
.                      *105,"CITY":
.                      *113,"STATE":
.                      *122,"OUR":
.                   *N:
.                      *3,"LR##":
.                      *39,"DATE":
.                      *48,"NUMBER":
.                      *66,"RECEIVABLE":
.                      *82,"PAYABLE":
.                      *95,"INCOME":
.                      *105,"TAX":
.                      *114,"TAX":
.                      *121,"POSTAGE"
.>Patch 3.65 Comment Out

.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS":
.                      *119,"DATE: ",TODAY:
.                   *N,*55,"ADJUSTMENT REGISTER":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*01,"CLIENT##":
.                      *10,"BILL-TO/LIST":
.                      *38,"CREDIT":
.                      *47,"INVOICE":
.                      *67,"ACCOUNTS":
.                      *81,"ACCOUNTS":
.                      *95,"LR/NIN":
..                      *105,"CITY":
.                      *113,"STATE":
.                      *122,"OUR":
.                   *N:
.                      *3,"LR##":
.                      *39,"DATE":
.                      *48,"NUMBER":
.                      *66,"RECEIVABLE":
..                      *82,"PAYABLE":
.                      *95,"INCOME":
.                      *105,"TAX":
.                      *114,"TAX":
.                      *121,"POSTAGE"
.>Patch 3.65 Comment Out                      
.>Patch 3.65 Code Added
              clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Names in the News",*boldoff;           
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Adjustment Register",*boldoff;         
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,MONTH,*boldoff;          
        add     eightlpi,row                
        add     eightlpi,row        
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Client",*boldoff;         
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*boldon,*ll,"Credit",*boldoff;                              
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Invoice",*boldoff;                                            
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,"Accounts",*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,"Accounts",*boldoff;                          
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,"LR/NIN",*boldoff;                            
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,"City",*boldoff;                              
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*boldon,*ll,"State",*boldoff;                      
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*boldon,*ll,"Our",*boldoff;         
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"LR##",*ULOFF,*boldoff;             
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Bill-To/List",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Date",*ULOFF,*boldoff;                   
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Number",*ULOFF,*boldoff;                                
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Receivable",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Payable",*ULOFF,*boldoff;                             
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Income",*ULOFF,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Tax",*ULOFF,*boldoff;                  
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Tax",*ULOFF,*boldoff;           
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Postage",*ULOFF,*boldoff;                      
        add     eightlpi,row  
        add     eightlpi,row  
.>Patch 3.65 Code Added
.END PATCH 3.63 REPLACED LOGIC
.>Patch 3.65 Comment Out 
.         MOVE      C8 TO PRTLINES
.>Patch 3.65 Comment Out          
         RETURN
+...........................................................................
.
.patch3.62
                                             include        compio.inc
                                             include        cntio.inc
.         INCLUDE   NMLRIO.inc    MAILER
.patch3.62
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NORDIO.inc
+
         INCLUDE   NJSTIO.inc    ADJUSTMENT DETAIL
+
.         INCLUDE   NJSTCLIO.inc  ADJUSTMENT DETAIL
..............................................................................
.
. NJSTCLIO INCLUSION
. NIN DETAIL ADJUSTMENT APPLICATION ROUTINE
.
. APPLIES ADJUSTMENT AMOUNTS TO COMPUTED AMOUNTS FROM THE NINVCALC
. ROUTINE IN THE NINVCLIO INCLUSION.
.
..............................................................................
.
NJSTCALC
.              MOVE      JSTAR TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO FART
.         ADD       NUM102 TO ARWOPP
         add        jstar to fart
         add        jstar to arwopp
.
.         MOVE      JSTAP1 TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO FAP1T
.         ADD       NUM102 TO FAPT
         add       jstap1 to fapt
         add       jstap1 to fap1t
.
.         MOVE      JSTAP2 TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO FAP2T
.         ADD       NUM102 TO FAPT
         add       jstap2 to fapt
         add       jstap2 to fap2t
.BEGIN PATCH 3.82         
         add       jstap3 to fap3t
         add       jstxNinc to xNinc1
.END PATCH 3.82         
.
.         MOVE      JSTLRINC TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO LRINC
         add       jstlrinc to lrinc
         add       jstNiNinc to Ninc
.
.         MOVE      JSTSTAX TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO STAX
.         ADD       NUM102 TO TAXES
          add       jststax to taxes
          add       jststax to stax
.
.         MOVE      JSTCTAX TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO CTAX
.         ADD       NUM102 TO TAXES
          add       jstctax to taxes
          add       jstctax to ctax
.
.         MOVE      JSTPOST TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO POST
         add        jstpost to post
.
         RETURN
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         GOTO      CVTOK IF EQUAL                ITS OK.
.FORMERR  DISPLAY   *P01:24,*EL,*B,"Format error in NJSTCALC. ",*W9
.         MOVE      B10 TO CVTFLD
. .        RETURN
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
..         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.CVTOK    MOVE      CVTFLD TO NUM102
.         MULTIPLY  ".01"  BY NUM102
.         RETURN
.begin patch 2.3
GetControl
         move        no to incontrol
         if        (jstlr = "374510")
         call      debug
         endif
         unpack    jstdate into cc,yy,mm,dd
         pack      HoldStr4 from yy,mm
         rep       zfill in Holdstr4
         call       cvtjul
               move      juldays to datechk
         packkey   ncshfld3 from jstlr
         call      ncshkey
         if        not over
               move      cyr to yy
         move      cmo to mm
         move      cdy to dd
         call       cvtjul
         clear     str4
         pack      str4 from cyr,cmo
         rep       zfill in str4
         move       juldays to n5
         add        c3 to n5
         sub        datechk from n5
.                      if         (n5 >= 0 & n5 <= 3)
                 if         (HoldStr4 = str4)
                       move        cnum to Controlnum
                 move        yes to incontrol
                       return
                       else
                       clear      controlnum
                 goto       getcontrolloop
                       endif
        else
        clear      controlnum
        return
        endif
getcontrolloop
         call      ncshks
         if        not over
               move      cyr to yy
         move      cmo to mm
         move      cdy to dd
         call       cvtjul
         clear     str4
         pack      str4 from cyr,cmo
         rep       zfill in str4
         move       juldays to n5
         add        c3 to n5
         sub        datechk from n5
.                      if         (n5 >= 0 & n5 <= 3)
                 if         (HoldStr4 = str4)
                       move        cnum to Controlnum
                 move        yes to incontrol
                       return
                       else
                       clear      controlnum
                 goto       getcontrolloop
                       endif
        else
        clear      controlnum
        return
        endif
+
.begin patch 3.7
.         include   ninvio.inc
              include         ninvio.inc
.end patch 3.7
.begin patch 2.3
         include   ncshio.inc
.end patch 2.3
         INCLUDE   COMLOGIC.inc


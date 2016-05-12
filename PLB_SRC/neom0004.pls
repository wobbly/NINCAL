...............................................................................
.NEOM0004 - sales register
...............................................................................
.
PC        EQU       0
          INC       COMMON.inc
          INCLUDE   CONS.inc
          include   consacct.inc
          include   hp.inc
          INC       ninvdd.inc
          Include   NInvAcddd.inc
          include   compdd.inc
          include   cntdd.inc
          INCLUDE   NBILDD.inc
          INCLUDE   NORDDD.INC
          include   nowndd.inc
          INCLUDE   GNXTDD.inc
          INCLUDE   NDAT3DD.inc
          include   winapi.inc
          inc       nmrgdd.inc
          include   nslsdd.inc
          include   nmoadd.inc
          include   nxcgdd.inc
          include   tinvdd.inc
          include   nshpdd.inc
          include   ndatdd.inc
          include   nacddd.inc
          include   nprjdd.inc
          include   nrevdd.inc
          
.
...........................................
Release  Init      "3.93"     DLH office365 email server change email type to HTML
RelDate  INit      "2015 December 30"
.Release  Init      "3.92"     DLH Break out new biz details in email
.RelDate  INit      "2015 February 4"
.Release  Init      "3.91"     DLH clarification for "Monday" numbers
.RelDate  INit      "2013 July 21"
.Release  Init      "3.90"     DLH Sunbelt PDF
.RelDate  INit      "2013 April 23"
.Release  Init      "3.84"     DLH Produce A/R & A/P details for excel reporting, use Data Manager
.RelDate  INit      "14 Jan 2011"
.Release  Init      "3.83"     DLH All back under NIN
.RelDate  INit      "04 Jan 2010"
.Release  Init      "3.82"     DLH All back under NIN
.RelDate  INit      "09 Dec 2009"
.Release  Init      "3.81"     DLH FIX new projection/revenue previous yr/newbus
.RelDate  INit      "24 April 2009"
.Release   Init      "3.80"    DLH       Cleanup printed totals PL Lm & LM exchange fee
.RelDate   INit      "01 July 2008"      use olnum as IDentifier
.Release  Init      "3.79"    DLH       Cleanup printed totals PL Lm & LM exchange fee
.RelDate  INit      "09 June 2008"
.Release  Init      "3.78"     JD Check for new projection/revenue previous yr/newbus?
.RelDate  INit      "????? 2008"
.Release  Init      "3.77"    DLH       REconcile LR subtotals do not equal total
.RelDate  INit      "06 March 2008"
.RelDate  INit      "05 December 2007"
.Release  Init      "3.76"              JD        30March2007    Updated projection read

.Release  Init      "3.75"    DLH       12March2007    Pacific Lists - brokerage side
.see release 3.74 for previous changes
.release        init           "3.74"        DLH              02October2006     skip monday update of Zero records
.release        init           "3.73"        DLH              24August2006     MailSend
.release        init           "3.72"        DLH              13JUne2006     error checking  
.release       init            "3.71"        JD           2006May05 New business patch.
.release       init            "3.7"        JD/DH         2006Mar16 Break out new business lr inc
.release       init            "3.6"        DLH          2006Feb13 Excel
.release  init      "3.5"        DMB          2005 JAN Update Directory Locations for File Folder Restructure
.release  init      "3.4"        DLH         2005March02    Invoice Conversion
.>Patch 3.38
.release  init      "3.38"        DMB        13JUL2005 Update print to prtpage
.release  init      "3.37"        DMB        26MAY2004      Mailer Conversion
.release   init      "3.36"           JD05Sep2003 replace formar with formar102w slsfile write.
.release   init      "3.35"           JD29Aug2003 replace ar with formar slsfile write.
.release   init      "3.31"           JD31Jan2002 forced mlr read use z3 for cobn.
.release   init      "3.3"           DLH  18Jun01 Email report totals to DH
.release   init      "3.21"           JD  13Dec00 increased keyin timeout update last inv #.
.release   init      "3.2"           DLH 13Sep00 add end of report flag for missing MOA/ code 96 entries
.release   init      "3.1.1"         DLH 31Jan00 change maildate criteria to be just prior not prior 1 month
.release   init      "3.1"         DLH 31Jan00 allow for 00 maildates and 99 to 00 year date check
.release   init      "3.0"         DLH 26Apr99 NININV Y2k
.release   init      "2.9"         DLH 8Feb99  test of new compute
.release   init      "2.8"         ASH 30DEC98 NINORD Y2K, File expansion; CONSACCT.INC VAR EXPANSION
.release   init      "2.7"         DLH 13Mar98 added breakout of List management Exchange fee
.RELEASE  INIT       "2.6"         dlh 30OCT97  added dsprog. 
.RELEASE  INIT       "2.6"         dlh 01OCT97 Create flat file for GL balancing 
.RELEASE  INIT      "2.5"          DLH 23SEP97 TRACK COMMISION RECAPTURE
.RELEASE  init      "2.4"          JD12Mar96 update of NINVLAST AT EOM. 
.RELEASE  init      "2.3"          DLH 20Feb96 added creation of slsTEMP.new - 
.                                 as input file for new Owner activety report.
.release  init      "2.2"          JD  13nov95 added print "*" next lr for net.
.release  init      "2.1"         DLH 17may95 added check for moa entry on prepays
.release  init      "2.0"         DLH 11nov94 add new compute (splits)
.RELEASE  INIT         "1.6"         JD  18JAN94 CORRECTED DATE CHECK ON BATCH BILL
.RELEASE  INIT         "1.5"         DLH 25MAR93 ACCRUAL OF BATCH BILLING.
.RELEASE  INIT      "1.4"        DLH 16FEB93 GNXTxx.INC
.RELEASE  INIT      "1.3"        DLH  30JUN92  BREAKOUT TDMC CHARGES.
.
.RELEASE  INIT      "1.2"       DLH  23MAR92  NORDXX, NINVXX, NBILXX
.RELEASE  INIT      "1.1"       DLH 01/31/92  COMBINED LM TOTALS &
.                              CHANGED ORDER TO MATCH OTHER EOM JOBS.
.RELEASE  INIT      "1.0"       91
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
time          Dim             8
.FILES.
...............................................................................
.
.
clninv   file                           .flat sales file used to reconcile
SLSFILE  FILE
.
DUPEOWN  IFILE     KEYLEN=4
OWNKEY   DIM       4    *DUPE OWNER FILE.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
.
.
.  KEY VARIABLES
.............................................
.
BEGINV   DIM       6    *STARTING INVOICE NUMBER FOR MONTH
TAB      FORM      "37"
.
. WORK VARIABLES
..............................................
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
ANS      DIM       1
TYPIST   DIM       2

HoldAR             form           10.2      ACCOUNTS RECEIVABLE (A/R),  X,XXX,XXX.XX
HoldAP1            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XX
HoldAP2            form           10.2      2ND ACCOUNTS PAYABLE (A/P2), X,XXX,XXX.XX
HoldAP3            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XXHOldAr        

.
TOTARp   FORM      9.2       *prepaid
TOTpMOA  FORM      9.2       *MOA Applied to prepaid
TOTAR    FORM      9.2
TOTAP1   FORM      9.2
TOTAP2   FORM      9.2
TOTAP    FORM      9.2
TOTNIN   FORM      9.2
TOTLR    FORM      9.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
.Begin patch 3.75
TotAP3    Form      9.2
TotXninc  Form      9.2
TOTPLARp   FORM      9.2       *prepaid
TOTPLpMOA  FORM      9.2       *MOA Applied to prepaid
TOTPLAR    FORM      9.2
TOTPLAP1   FORM      9.2
TOTPLAP2   FORM      9.2
TOTPLAP    FORM      9.2
TOTPLNIN   FORM      9.2
TOTPLLR    FORM      9.2
TOTPLSTAX  FORM      9.2
TOTPLCTAX  FORM      6.2
TOTPLPOST  FORM      5.2
TotPLAP3  Form      9.2
TotPLXninc          Form      9.2
.end patch 3.75
.begin patch 3.92
NewBizbody dim        3000
.end patch 3.92
. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
.LManage  init      "018710"    List management exchange fees only
.01 July 2008 DLH
.use exfeelst from cons
.EXFEELST INIT      "018710-024593"
TDMCAMT  FORM       8.2
tottdmc  form       9.2
MT$tdmc DIM       15
TDMChrg  dim        1           (Y) = tdmc related charges
runchrg  form       9.2          =Running charges
grunrlr  form       9.2          =Total NINCA income on triplex billing
grunrar  form       9.2          =Total NINCA adc billing (TDMC)
grunrflat  form       9.2        =total ninca rental flat billing
grunrpass  form       9.2        =total ninca rental pass through billing  RC, Selects
TDMCFLAT   form       9.2        =estimated triplex flat charges shipping, Mag tape....
IncRFlat   form       9.2        =Calced NINCA income from flat charges.
PRCflat    form       3          =percent cost associated to flat charges
PRCPASS    form       3          =percent cost associated to /m charges
PRCLRflat  form       3          =percent income associated to flat charges
prcLRPASS  form       3          =percent income associated to /m charges
.
.END TDMC.
.
ninMRinc form      9.2      Total NIN ACD MAnAGEMENT RENTAL inc
ninMEinc form      9.2      Total NIN ACD MAnAGEMENT exch inc
ninBRinc form      9.2      Total NIN ACD Brokerage RENTAL inc
ninBEinc form      9.2      Total NIN ACD Brokerage Exch inc
Calc92     Form       9.2
.begin patch 3.75
ninPLMRinc form      9.2      Total NIN ACD MAnAGEMENT RENTAL inc
ninPLMEinc form      9.2      Total NIN ACD MAnAGEMENT exch inc
ninPLBRinc form      9.2      Total NIN ACD Brokerage RENTAL inc
ninPLBEinc form      9.2      Total NIN ACD Brokerarge Exch inc
.end patch 3.75
LRMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      9.2      UNKNOWN LR INCOME.
LREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
LRBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
.begin patch 3.75
LRPLMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRPLMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRPLMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRPLBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRPLBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRPLUNKN   FORM      9.2      UNKNOWN LR INCOME.
LRPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
LRPLBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRPLBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.

.end patch 3.75
.
ARMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
AREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM      FORM      9.2      TOTAL MANAGEMENT A/R
ARBR     FORM      9.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN   FORM      9.2      UNKNOWN  A/R.
ARBBE      FORM        9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR      FORM        9.2      TOTAL BATCH BILL A/R RENT PORTION
.
APMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
APM      FORM      9.2      TOTAL MANAGEMENT A/P
APBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN   FORM      9.2      UNKNOWN A/P.
APBBE      FORM        9.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR      FORM        9.2      TOTAL BATCH BILL A/P RENT PORTION
.begin patch 3.75
ARPLMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
ARPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARPLME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARPLM      FORM      9.2      TOTAL MANAGEMENT A/R
ARPLBR     FORM      9.2      TOTAL BROKERAGE/RENTAL  A/R
ARPLBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARPLUNKN   FORM      9.2      UNKNOWN  A/R.
ARPLBBE      FORM        9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARPLBBR      FORM        9.2      TOTAL BATCH BILL A/R RENT PORTION
.
APPLMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APPLME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
APPLM      FORM      9.2      TOTAL MANAGEMENT A/P
APPLBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APPLBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APPLUNKN   FORM      9.2      UNKNOWN A/P.
APPLBBE      FORM        9.2      TOTAL BATCH BILL A/P EXCH PORTION
APPLBBR      FORM        9.2      TOTAL BATCH BILL A/P RENT PORTION

.end patch 3.75
.
COLDLm   FORM        9.2      New Biz LM
COLDBR   FORM        9.2      New Biz BR
COLDBE   FORM        9.2      New Biz BE
newbiz    init      "N"
.begin patch 3.84
COLDLAR   FORM        9.2      New Biz LM AR
COLDBAR   FORM        9.2      New Biz BR AR
COLDLAP   FORM        9.2      New Biz LM AP
COLDBAP   FORM        9.2      New Biz BE AP

.end patch 3.84
.begin patch 3.75
COLDPLLm   FORM        9.2      New Biz LM
COLDPLBR   FORM        9.2      New Biz BR
COLDPLBE   FORM        9.2      New Biz BE
.end patch 3.75
ppflag   dim       1            'P' if equal else blank
PMASK    DIM       1
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM9A    FORM      9
FORM52   FORM      5.2
FORM11   FORM      11
form122  form      12.2
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
.begin patch 3.75
COUNTPL  FORM      5
.end patch 3.75
CO       FORM      1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
SALESNUM DIM       2
TEAM1    INIT      "01"     Brokerage            .2007
TEAM2    INIT      "02"    Brokerage    .2007
TEAM3    INIT      "03"    LIST MANAGEMENT
DIM9     DIM       9
dim9B    dim       9
DIM9A    DIM       9
AMOUNT1  DIM       10
FORM102  FORM      10.2
FORM102W FORM      10.2
newfld   dim       10
MINUS    INIT      "-"
TOMOV    INIT      "0}1J2K3L4M5N6O7P8Q9R"
FORM9B    FORM      9
FORM9    FORM      9
apsw     dim       1
. 
.
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       15
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.
MT$AR    DIM       15
MT$ARP   DIM       15
MT$pMOA  DIM       15     *prepaid 
MT$AP1    DIM       15
MT$AP2   DIM       15
MT$STAX  DIM       15
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       15
MT$NINC  DIM       15
.
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
innets   dim       1
lastinv  dim       6
EXFEFLAG DIM       1          ."Y" = EXCHANGE MAN FEE "N" DEFAULT
mrgsw    dim       1
shipsw   dim       1
moabadflag Init     "N"          Y = missing moa
check    form      1
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
check1   form      5
check2   form      5 
today1   form      5
revdat   form      5
OLDTOT   FORM    10.2
OLDTOT1   FORM    10.2
typer    dim      1
srcr     dim      1
cidr     dim      6
mmrep    dim       2
yyrep    dim       2
ccrep    dim       2
str10a     dim        10
.............................................................................................
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
.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
              move    "4250" to Header1
              move    "4150" to Title1       
              move    "5450" to Title2
              move    "6750" to Title3       
        move    "200",column
        move    "575",column1
        move    "1000",column2
        move    "1500",column3
        move    "3200",column4
        move    "3600",column5
        move    "4400",column6
        move    "5400",column7
        move    "6250",column8
        move    "6800",column9
        move    "7300",column10
        move    "4000",column4R
        move    "4700",column5R
        move    "5400",column6R
        move    "6100",column7R
        move    "6600",column8R
        move    "7100",column9R
        move    "7700",column10R
        
.
         MOVE      "Names in the News" TO COMPNME
         MOVE      "NEOM0004" TO PROGRAM
         MOVE      "MONTHLY INVOICE REGISTER" TO STITLE
.Prepare Flag file
.begin patch 3.9
.          Call      PDF995Auto
.          call      SetPDFFlag
         
          If        (PRtName  = "")
          MOve      "SalesReg",PRtname
          endif
          pack      str55 from "c:\work\pdf\",prtname,".pdf"                    
.               PRTOPEN prfile,"PDF995",PRTNAME
               PRTOPEN prfile,"PDF:",str55
.end patch 3.9
               PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                                              *Duplex=2;                                                 
         OPEN      DUPEOWN,"\\nins1\e\data\index\DUPEOWN",READ
         PREPARE   SLSFILE,"e:\data\SLSTEMP.new|NINS1:502"
         MATCH     B8 TO TODAY
         IF        EQUAL
         goto      clock
                   ELSE
         IF        EOS
         goto      clock
         ENDIF
         ENDIF
         goto      date1
.       

CLOCK    CLOCK     DATE TO TODAY
...       MOVe      "06-30-08",today
.         MOVe      "01-31-15",today
DATE1    MOVE      TODAY TO DATEMASK
         UNPACK    TODAY INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
.Patchjd
        CALL      CVTJUL
         MOVE      juldays TO TODAY1
.Patchjd
         DISPLAY   *P01:06,"Input File  :NININV ":
                   *P01:07,"Eom Date : "
         clear     str55
         pack            str4 using sysyr,sysmo
         rep             zfill,str4
         append    "\\nins1\d\accounting\gl\clninv." to str55
         append    str4,str55
         append    ".csv|NINS1:502" to str55
         reset      str55
         PREPARE   clninv,str55
.
OPEN     TRAP      SHAREINV GIVING ERROR IF IO
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         GOTO      BEGIN
SHAREINV TRAPCLR   IO
         DISPLAY   *P1:23,*EL,"NININV2 READ ONLY FAILED, FILE SHARED";
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
         GOTO      OPENREST
BEGIN
           MOVE      "NINVLAST" TO GNXTFLD
           CALL      GNXTKEY
           MOVE      GNXTNUM TO NINVFLD
           MOVE      GNXTNUM TO HOLDREC
.           move    "540769",ninvfld          .may 2007
.           move    "540769",holdrec
.           move    "542058",ninvfld          .June 2007
.           move    "542058",holdrec
.           move    "542861",ninvfld          .July 2007
.           move    "542861",holdrec
.           move    "544076",ninvfld          .Aug 2007
.           move    "544076",holdrec
.           move    "545409",ninvfld          .Aug 2007
.           move    "545409",holdrec
.           move    "547126",ninvfld
.           move    "547126",holdrec               .for JUne 2008
...           move  "558531",ninvfld
...           move  "558531",holdrec               
.           move    "576319",ninvfld
.           move    "576319",holdrec               .rerun Dec 2009
.           move    "620720",ninvfld
.           move    "620720",holdrec               .rerun Jan 2015
           

           REP       ZFILL IN GNXTFLD
         CALL      NINVTST
         ADD       C1 TO HOLDREC
.
INPUT    COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
.
.
READINV  CALL      NINVKS
         GOTO      TOTAL IF OVER
        
         move       no to newbiz

         MOVE      B1 TO RUNFLAG
         MATCH     INVDTEM TO SYSMO
         GOTO      TOTAL IF NOT EQUAL
.begin patch 3.75
.         ADD       C1 TO COUNT
.         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
.                   *P15:07,today
.end patch 3.75
              Move            c0,holdar
              Move            c0,holdap1
              Move            c0,holdap2
              Move            c0,holdap3
              Move            Ar,HOldar
              move            ap1,holdap1
              move            ap2,holdap2
              move            ap3,holdap3

MISSCHK  MOVE      INVNUM TO NEXTREC          *CHECK
         COMPARE   NEXTREC TO HOLDREC         * FOR MISSING
         GOTO      MISSING IF NOT EQUAL        *INVOICE
         ADD       C1 TO HOLDREC
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.
.begin patch 3.77 move
..begin patch 3.75
.         if        (OcompId = "P")                         
.         ADD       C1 TO COUNTPL
.         else
.         ADD       C1 TO COUNT
.         endif
.         DISPLAY   *P10:12,"NUMBER OF NIN INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
.                   *P10:13,"NUMBER OF PL  INVOICES PROCESSED: ",COUNTPL,b5,lrn,b1,invnum:
.                   *P15:07,today
..end patch 3.75
.end patch 3.77 move

         DISPLAY   *P10:15,"Mail Date",b5,b5,omdtem,slash,omdted,slash,omdtec,omdtey
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST          .triplex running charge list?
         IF         EQUAL                     .Yes
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
...
         MOVE       C0 TO CMPT92
         MOVE       QTYbild TO FORM82
         MOVE       FORM82 TO CMPT92    
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
         MULT       ".01" BY FORM92
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
...........................................................................
         move       no to tdmchrg
         move       c0 to runchrg

         move      lrn to nxcgfld
         rep       zfill in nxcgfld
         call      nxcgkey
         goto      runexit if over
         move       yes to tdmchrg
         add        nxcgar to runchrg
runloop  call       nxcgks
         goto       runexit if over       
         match      nxcglr to nxcgfld
         goto       runexit if not equal
         add        nxcgar to runchrg
         goto       runloop
runexit         
.
...........................................................................
.
...............................................................................
.NOTE THIS TABLE NEEDS TO BE ADJUSTED WHEN EVER SALES PERSONNEL CHANGES.
...............................................................................
.CONVERT SALESPERSONS TO SALES TEAMS.
         MOVE      C0 TO SALESBR
         PACK      SALESNUM FROM OSALES10,OSALES
         MOVE      SALESNUM TO SALESBR
         COMPARE   C0 TO SALESBR
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
          if        equal
          MOVE      C1 TO SALESBR
          MOVE      C2 TO OELCODE
          GOTO      PROCESS
          endif               
          Cmatch    "B",lrn
          if        equal
          MOVE      C1 TO SALESBR                             .brokerage
          GOTO      PROCESS
          endif
          Cmatch    "M",lrn
          if        equal
          MOVe      "P",Ocompid2
          MOVE      C3 TO SALESBR                             .Management
          GOTO      PROCESS
          endif
         ENDIF
.

.See Oslspern.inc for details.
...............................................................................
.
.          if        (seller <> "02" & Seller <> "06" & Seller <> "19" & Seller <> "27" & Seller <> "28" & Seller <> "30")
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM3,TEAM1,Team1,team1:                 .1-5
                   TEAM3,Team1,TEAM1,TEAM1,TEAM1:                            .6-10        
                   TEAM1,Team1,TEAM1,Team1,Team1:                            .11-15       
                   TEAM1,Team1,Team1,TEAM3,TEAM1:                            .16-20       
                   TEAM1,TEAM1,team1,team1,team1:                            .21-25       
.begin patch 3.75
.                   TEAM1,TEAM1,team1,team1,team3:                                     .26-30       
                   TEAM1,TEAM3,team3,team1,team3:                            .26-30       
.end patch 3.75
                   TEAM1,TEAM1,team1,team1,team1                             .31-35       
         MOVE      SALESNUM TO SALESBR
...............................................................................
.begin patch 3.77 move
.begin patch 3.75
.begin patch 3.82
.PROCESS   if        (OcompId = "P" & SalesBr = c1)                              
.          ADD       C1 TO COUNTPL
.          elseif (OcompId2 = "P" & SalesBr = c3)                      
.          ADD       C1 TO COUNTPL
.          else
.          ADD       C1 TO COUNT
.          endif
Process   Add       C1,Count
.end patch 3.82
         DISPLAY   *P10:12,"NUMBER OF NIN INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
                   *P10:13,"NUMBER OF PL  INVOICES PROCESSED: ",COUNTPL,b5,lrn,b1,invnum:
                   *P15:07,today
.end patch 3.75
.end patch 3.77 move
.
.PROCESS  
         MOVE      C0 TO FORM9A
         MOVE       QTYbild TO FORM9a
          move      ppm to form32
.
.
         CALL      READMLR
.
.
         move      c1 to nownpath
         move      olon to nownfld
         call      nownkey
         move      c2 to tdmcflag
         MOVE      NORDFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL        NMRGKEY
         if      not over
                       move    yes to mrgsw
         endif   

.get triplex info
         move        c0 to TDMCAMT
         move       lrn to tinvfld
         call       tinvkey
         if         not over
         move       yes to tdmchrg
         move       tinvdolr to form122
         mult       ".01" by form122
         move        c0 to TDMCAMT
         add         form122 to TDMCAMT
moretdmc call       tinvks
         goto       tdmcexit if over           
         match      tinvfld to tinvlr
         goto       tdmcexit if not equal
         move       c0 to form122
         move       tinvdolr to form122
         mult       ".01" by form122
         add         form122 to TDMCAMT
         goto       moretdmc
         endif
tdmcexit
         add        tdmcamt to tottdmc

...........................................................................
         move      c2 to tdmcflag     .force compute to calc tdmc goodies
............................................................................         
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
.not nec to read ship as already billed
         move      lrn to nshpfld
         move      no to shipsw

         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               

         CALL      COMPUTE
.
MASKIT   
...
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.

         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
.begin patch 3.75
         move      c0 to form102w                                     .used for clninv write
         add       formar to form102w
.
         MOVE      MASK92 TO M$AP1
         EDIT      AP TO M$AP1
.begin patch 3.75
.
          Clear     M$AP2
          IF        (Formap2 <> c0)
          MOVE      MASK92 TO M$AP2
          EDIT      FORMAP2 TO M$AP2
          endif
.         COMPARE   C0 TO FORMAP2
.         CALL      ZEROAP2 IF EQUAL
.
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
.begin patch 3.77
.
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
.begin patch 3.76
.begin patch 3.77
.         if        (OcompId = "P")                         
.Begin patch 3.80
.         if        (OcompId = "P" & Salesbr = c1 or Ocompid2 = "P" & Salesbr = c3)       
.begin patch 3.82
.          if        ((OcompId = "P" & Salesbr = c1) or (Ocompid2 = "P" & Salesbr = c3))                       
..end patch 3.80
..end patch 3.77
.          ADD       PREPAY TO TOTPLARP      .TOTAL PREPAID ORDERS.
.          ADD       FORMAR TO TOTPLAR
.          ADD       AP TO TOTPLAP1
.          ADD       FORMAP2 TO TOTPLAP2
.          ADD       AP3 TO TOTPLAP3
.            ADD       LRINC TO TOTPLLR
.          ADD       NININC TO TOTPLNIN
.          ADD       XNINC TO TOTPLXNINc
.          ADD       TAXES TO TOTPLSTAX
.          ADD       POST TO TOTPLPOST
.          else
.end patch 3.82
          ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
          ADD       FORMAR TO TOTAR
          ADD       AP TO TOTAP1
          ADD       FORMAP2 TO TOTAP2
          ADD       AP3 TO TOTAP3
          ADD       LRINC TO TOTLR
          ADD       NININC TO TOTNIN
          ADD       XNINC TO TOTXNINc
          ADD       TAXES TO TOTSTAX
          ADD       POST TO TOTPOST
.begin patch 3.82
.          endif
.end patch 3.82
.
         MOVE      MASK52 TO M$STAX
         EDIT      TAXES TO M$STAX
.
         MOVE      C0 TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
.*****************************************************************************
         MOVE      C0 TO RENTSW
         MOVE      OELCODE TO RENTSW
............................................................................
           PACK      MKEY FROM MLRN,z3
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
           CALL      NMLRKEY
           IF       (Mcode = "B" or MCode = "A")
           goto     Bdate
           Else
           GOto     Slstype
           endif
.           CMATCH    "B" TO MCODE          .BATCH BILL ?
.         goto      bdate if equal
.           CMATCH    "A" TO MCODE          .BATCH BILL ?
.         goto      bdate if equal
.         goto      slstype 
bdate
         MOVE      C0 TO N2
         MOVE      OMDTEM TO N2           .we will use the month instead
bdatedebug
         compare   c0 to n2
         goto     slstype if equal
         move     c0 to n4
         clear     str4
         pack      str4 from omdtec,omdtey     .maildate century & year
         move      str4 to n4
         clear     str4
         pack      str4 from cc,sysyr          .current century & year
         move      c0 to n5
         move       str4 to n5
        sub        n4 from n5
          if       (n5 > 0 )                  .from previous year
          move     c1 to batchbr              .so set flag  on          
          goto      slstype
           ENDIF
          if       (n5 < 0 )                  .from future year
          move     c0 to batchbr              .so set flag off
          goto      slstype
           ENDIF
MONTHCK    MOVE      C0 TO N2
           MOVE      OMDTEM TO N2 
           MOVE      SYSMO TO N3
           SUB       N2 FROM N3
           if        (n3 > 0)                   maildate is from a previous month
bdatedebug1
             MOVE      C1 TO BATCHBR
             ENDIF
..............................................................................
slstype
          if        (lrn = "733741")
          call      debug
          endif
.begin patch 3.80
          Reset     EXFEELST
.         MATCH     OLNUM TO Lmanage
          scan      Olnum in EXFEELST
.end patch 3.80
          IF        EQUAL
          call      debug
          MOVE      YES TO EXFEFLAG      
.begin patch xxx
           MOVe       c3,salesbr            .credit to LM
           clear      Osales10
           move       c6,osales
.end patch xxx
          goto      ManEXch               .Management exchange fee
          ENDIF
          MOVE      NO TO EXFEFLAG
          BRANCH    SALESBR OF BROKER,BROKER,MANAGE         
          goto      broker
.
.MANEXCH - LIST MANAGEMENT EXCHANGE FEE.

MANEXCH
.begin patch 3.80
.begin patch 3.75
.begin patch 3.82
.          If        (olnum = "024593")
..         if        (Ocompid2 = "P")                        
..end patch 3.80
.         ADD       LRINC TO LRPLEXFEE
.         ADD       FORMAR TO ARPLEXFEE
.         ADD       AP TO APPLEXFEE
.         ADD       FORMAP2 TO APPLEXFEE
.         Else
.end patch 3.82
          ADD       LRINC TO LREXFEE
          ADD       FORMAR TO AREXFEE
          ADD       AP TO APEXFEE
          ADD       FORMAP2 TO APEXFEE
.begin patch 3.82
.          Endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT

MANAGE
         move      "M",src
         move      b1,type
         move      OLNUM to cid
         BRANCH   RENTSW OF MRENT,MEXCH,MEXCH
MRENT
        move       no to newbiz
         call      newbus
.begin patch 3.75
.begin patch 3.82
.                              if        (newbiz = yes & Ocompid2 = "P")
.                              add       lrinc to coldPLlm
.                              move      no to newbiz
.                              Elseif    (newbiz = yes)                       
.                              add       lrinc to coldlm
.                              move      no to newbiz
.                              else
.                              if        (Ocompid2 = "P")                        
.                                        ADD       LRINC TO LRPLMRINC        *RENT/MANAGEMENT INCOME
.                                        else
.                                        ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
.                                        endif
.end patch 3.75
.                             move       no to newbiz
.                              endif
                    iF        (nEWbIZ = yES)
                    add       lrinc,coldlm
.begin patch 3.84
                    Add       FormAr,ColdLAR       
                    ADD       AP,ColdLAP
                    ADD       FORMAP2,ColdLAP
                    else      
                    add       Lrinc,lrmrinc
                    ADD       FORMAR TO ARMR
                    ADD       AP TO APMR
                    ADD       FORMAP2 TO APMR
                    endif
.                    move      No,Newbiz
.end patch 3.82
.end patch 3.84
 
.begin patch 3.75
.begin patch 3.82
.          if        (Ocompid2 = "P")                        
.          ADD       NININC TO NINPLMRINC        *RENT/MANAGEMENT INCOME
.          ADD       FORMAR TO ARPLMR
.          ADD       AP TO APPLMR
.          ADD       FORMAP2 TO APPLMR
.          else
          ADD       NININC TO NINMRINC        *RENT/MANAGEMENT INCOME
.         endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT
MEXCH    
.begin patch 3.75
.begin patch 3.82
.          if        (Ocompid2 = "P")                        
.          ADD       LRINC TO LRPLMEINC        *EXCH/MANAGEMENT INCOME
.          ADD       NININC TO NINPLMEINC        *EXCH/MANAGEMENT INCOME
.          ADD       FORMAR TO ARPLME
.          ADD       AP TO APPLME
.          ADD       FORMAP2 TO APPLME
.          else
          ADD       LRINC TO LRMEINC        *EXCH/MANAGEMENT INCOME
          ADD       NININC TO NINMEINC        *EXCH/MANAGEMENT INCOME
          ADD       FORMAR TO ARME
          ADD       AP TO APME
          ADD       FORMAP2 TO APME
.         endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT
.
BROKER 
       
         BRANCH    BATCHBR OF BROKER2
.
BROKER1  BRANCH    RENTSW OF BRENT,BEXCH,BEXCH
           GOTO      BRENT
BROKER2  BRANCH    RENTSW OF BRENTB,BEXCHB,BEXCHB
           GOTO      BRENTB
BRENT
.begin patch 3.7
         move      "B",src
         move      "R",type
.begin patch 3.84
          Pack      Cid,Compnum
.                              pack      cid,"00",mlrn
.end patch 3.84
        move       no to newbiz
         call      newbus
.begin patch 3.75
.begin patch 3.82
.          if                  (newbiz = yes & Ocompid = "P")
.             add              lrinc to coldPLbr
.             move       no to newbiz
.             Elseif (newbiz = yes)
.             add              lrinc to coldbr
.             move       no to newbiz
.           ElseIf   (OcompId = "P")
.           add       lrinc to lrPLbrinc
.             else
.            add       lrinc to lrbrinc
.          endif
             if     (newbiz = yes)
             add              lrinc to coldbr
.begin patch 3.84
                    Add       FormAr,ColdBAR       
                    ADD       AP,ColdBAP
                    ADD       FORMAP2,ColdBAP
.end patch 3.84
             else
            add       lrinc to lrbrinc
          ADD       FORMAR TO ARBR
          ADD       AP TO APBR
          ADD       FORMAP2 TO APBR
          endif

.end patch 3.82

.begin patch 3.82
.          if        (OcompId = "P")                         
.                    ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
.                    ADD       FORMAR TO ARPLBR
.                    ADD       AP TO APPLBR
.                    ADD       FORMAP2 TO APPLBR
.                    Else
                    ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
.                    endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT
BEXCH
         move      "B",src
.begin patch 3.84         .all brokerage is combined
         move      "R",type
.         move      "E",type
.end patch 3.84         .all brokerage is combined
.begin patch 3.84
          Pack      Cid,Compnum
.                              pack      cid,"00",mlrn
.end patch 3.84
        move       no to newbiz
          call      newbus
.begin patch 3.75
.begin patch 3.82
.          if                  (newbiz = yes & Ocompid = "P")
.          add       lrinc to coldPLbe
.             move       no to newbiz
.             Elseif (newbiz = yes)
.          add       lrinc to coldbe
.             move       no to newbiz
.           ElseIf   (OcompId = "P")
.          add        lrinc to lrPLbeinc
.          else
.          add        lrinc to lrbeinc
.          endif
             If (newbiz = yes)
          add       lrinc to coldbe
.begin patch 3.84
                    Add       FormAr,ColdBAR       
                    ADD       AP,ColdBAP
                    ADD       FORMAP2,ColdBAP
.end patch 3.84
          else
          add        lrinc to lrbeinc
          ADD       FORMAR TO ARBE
          ADD       AP TO APBE
          ADD       FORMAP2 TO APBE
          endif
.end patch 3.82
.begin patch 3.75
.begin patch 3.82
.          if        (OcompId = "P")                         
.          ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
.          ADD       FORMAR TO ARPLBE
.          ADD       AP TO APPLBE
.          ADD       FORMAP2 TO APPLBE
.          else
          ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
.          endif
.end patch 3.82
.end patch 3.75
          GOTO      PRINT
BRENTB
         move      "B",src
         move      "R",type
.begin patch 3.84
          Pack      Cid,Compnum
.                              pack      cid,"00",mlrn
.end patch 3.84
.....**** DLH 01 july 08 I think this needs to be here
          Move      No,Newbiz
........  
         call      newbus
.begin patch 3.75
.begin patch 3.82
.          if                  (newbiz = yes & Ocompid = "P")
.          add       lrinc to coldPLbr
.          move      no to newbiz
.             Elseif (newbiz = yes)
.          add       lrinc to coldbr
.          move      no to newbiz
.           ElseIf   (OcompId = "P")
.          add       lrinc to lrPLbbr
.          else
.          add       lrinc to lrbbr
.          endif
          If (newbiz = yes)
          add       lrinc to coldbr
.begin patch 3.84
                    Add       FormAr,ColdBAR       
                    ADD       AP,ColdBAP
                    ADD       FORMAP2,ColdBAP
.end patch 3.84
          else
          add       lrinc to lrbbr
           ADD       FORMAR TO ARBBR
          ADD       AP TO APBBR
          ADD       FORMAP2 TO APBBR
         endif
.end patch 3.82
.end patch 3.75
.begin patch 3.82
.          if        (OcompId = "P")                         
.          ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
.          ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
.          ADD       FORMAR TO ARPLBBR
.          ADD       AP TO APPLBBR
.          ADD       FORMAP2 TO APPLBBR
.          else
          ADD       NININC TO NINBRINC        *REnt/BROKERAGE INCOME


.          endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT
BEXCHB
         move      "B",src
.begin patch 3.84         .all brokerage is combined
         move      "R",type
.         move      "E",type
.end patch 3.84         .all brokerage is combined
.begin patch 3.84
          Pack      Cid,Compnum
.                              pack      cid,"00",mlrn
.end patch 3.84
.....**** DLH 01 july 08 I think this needs to be here
          Move      No,Newbiz
........  
         call      newbus
.begin patch 3.75
.begin patch 3.82
.          if                  (newbiz = yes & Ocompid = "P")
.          add       lrinc to coldPLbE
.          move      no to newbiz
.             Elseif (newbiz = yes)
.          add       lrinc to coldbE
.          move      no to newbiz
.           ElseIf   (OcompId = "P")
.          add       lrinc to lrPLbbe
.          else
.          add       lrinc to lrbbe
.          endif
             if (newbiz = yes)
          add       lrinc to coldbE
.begin patch 3.84
                    Add       FormAr,ColdBAR       
                    ADD       AP,ColdBAP
                    ADD       FORMAP2,ColdBAP
.end patch 3.84
          else
          add       lrinc to lrbbe
          ADD       FORMAR TO ARBBE
          ADD       AP TO APBBE
          ADD       FORMAP2 TO APBBE
          endif
.end patch 3.82

.begin patch 3.82
.          if        (OcompId = "P")                         
.          ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
.          ADD       FORMAR TO ARPLBBE
.          ADD       AP TO APPLBBE
.          ADD       FORMAP2 TO APPLBBE
.          else
          ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
.          endif
.end patch 3.82
.end patch 3.75
         GOTO      PRINT
.
.ZEROAP2  CLEAR     M$AP2
.         RETURN
.end patch 3.75
.
READMLR
.         PACK      MKEY FROM MLRN,COBN
         PACK      MKEY FROM MLRN,z3
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
         clear     bilname
.         MOVE      MCCTO TO BILNAME
           CLEAR     BRCOMP
           CLEAR     BRaddr
           CLEAR     BRcity
           CLEAR     BRstate
           CLEAR     BRzip
           CLEAR     NBRKFLD
           PACK      NBRKFLD FROM iBRKNUM,iBRKCNT
           CMATCH    B1 TO NBRKFLD
           return    IF EOS
         call      nbrkkey
         IF        NOT OVER
         move      mcomp to bilCOMP
           move      BRCOMP to bilNAME
           ENDIF
         RETURN
.  
NEWBUS
.2016 March 1 DLH turn off this Jose bullshit
.                match     "M",src
.                goto       projsav if equal
.       PACK      MKEY FROM mlrn,"000"
..turned off 3/15 JD  new client contract date, belv is causing previous months totals zero.
.       call        NMLRKEY
.       move       cc to ccrep
.       move       mm to mmrep
.       move       yy to yyrep
.      unpack    COMPCNTDATE INTO cc,yy,mm,dd
.       move      cid to cidr
.       move      compnum to cid
.       move      type,typer
.       move      src,srcr
.       move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                              move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           usage in last year
.         if LESS
.                              move       yes to newbiz
.                              return
.                              endif
.                              endif
.         move      ccrep to cc
.         move      mmrep to mm
.         move      yyrep to yy
.         goto     projread
.
projsav
          move    type,typer
          move    src,srcr
          move    cid,cidr
.Patch 3.71
projread
.Patch 3.76
.Patch 3.78 Projection Section Update Yearly!!
           if         (cid = "021278" | cid = "005136")
           call       debug
           endif
           move      C1,N2
           move       c0,Prjar
           move      N2,str3
           rep       zfill,str3
           pack      NPRJFLD with Type,Src,CID,"2016",str3
           call      NPRJKEY
           if         over
           move       Yes to NewBiz
           goto       checkrev
           endif
           move       No to NewBiz
           
           loop
           add       C1,N2
           move      N2,str3
           rep       zfill,str3
           pack      NPRJFLD with Typer,Srcr,CIDr,"2016",str3
           call      NPRJKEY
           until      over
           until     (PrjMast = YES)
           repeat

           If        (PrjAR > 0)                    
           MOve      No,Newbiz
           Elseif   (PrjAR = 0)                    
           move       Yes to NewBiz
           endif

checkrev
           if         (NewBiz = yes)
                      if         (Typer = "B")
                      move       "R",srcr
                      endif
                 pack    nrevfld,typer,srcr,cidr,"2015" ......<<<<<<<<Yearly??????????????????????????
                 call    nrevkey
                 call    swaptype if over
..begin patch 3.84

                              Calc      OldTot1=(JanLR+FebLR+MarLR+AprLR+MayLR+JunLR+JulLR+AugLR+SepLR+OctLR+NovLR+DecLR)
                              Calc      OldTot=(JanAR+FebAR+MarAR+AprAR+MayAR+JunAR+JulAR+AugAR+SepAR+OctAR+NovAR+DecAR)

                    if (OLDTOT <> C0) 
                    move        No to newbiz
                    move   c0,oldtot
                                                       return
                                                       endif
.begin patch 3.92
                    append       nprjfld,Newbizbody
                    append       " - ",Newbizbody
                    append       lrn,Newbizbody
                    append       " - ",Newbizbody
                    if           (srcr = "M")
                    append       mlstname,Newbizbody
                    else
                    append       mcomp,Newbizbody
                    endif
                    Append    "<br>",NewBizbody
                    return
                    endif
.end patch 3.92
.          endif
          move   c0,oldtot
          return                                     
*......................................................................
.
PRINT    
              COMPARE  "9900" to ROW
         CALL      HEADER IF not less
         clear     ppflag
         cmatch    yes to ppsw
         if        equal 
         move      "P" to ppflag
         endif
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        equal
                       move      b1 to innets
         else
                       move      "*" to innets
         endif
                       COMPARE   C1 TO CMREFLAG
         IF        EQUAL
                       MATCH     YES TO EXFEFLAG
                       IF        EQUAL
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,*ulon,MLRN,*uloff,*boldoff;
.innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INNETS,*boldoff;                   
                              prtpage prfile;*ll,*boldon,LRN,*boldoff;             
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVNUM,*boldoff;                   
.RUNFLAG      
                              prtpage prfile;*ll,*boldon,RUNFLAG,*boldoff; 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILNAME,*boldoff;   
.GUARPAY       
                              prtpage prfile;*ll,*boldon," ",GUARPAY,*boldoff;                                          
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AR,*boldoff;    
.PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP1,*boldoff;                  
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$NINC,*boldoff;                 
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$LRINC,*boldoff;                
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$STAX,*boldoff;          
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$CTAX,*boldoff;          
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$POST,*boldoff;
                       add     eightlpi,row        
.begin patch 3.84
                              if        (newbiz = yes)
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"(NEW)",*boldoff;                   
                              endif
.end patch 3.84
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff; 
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                   
                   
                       ELSE
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,MLRN,*boldoff;
.innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INNETS,*boldoff;                   
                              prtpage prfile;*boldon,LRN,*boldoff;                 
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVNUM,*boldoff;                   
.RUNFLAG      
                              prtpage prfile;*ll,*boldon,RUNFLAG,*boldoff; 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILNAME,*boldoff;   
.GUARPAY       
                              prtpage prfile;*ll,*boldon," ",GUARPAY,*boldoff;
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AR,*boldoff;    
.PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP1,*boldoff;                  
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$NINC,*boldoff;                 
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$LRINC,*boldoff;                
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$STAX,*boldoff;          
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$CTAX,*boldoff;          
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$POST,*boldoff;
                       add     eightlpi,row        
.begin patch 3.84
                              if        (newbiz = yes)
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"(NEW)",*boldoff;                   
                              endif
.end patch 3.84
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff;   
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                       ENDIF
         ELSE
                       MATCH     YES TO EXFEFLAG
                       IF        EQUAL
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*ulon,MLRN,*uloff;
.innets  
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,INNETS;                     
                              prtpage prfile;LRN;                   
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVNUM;                     
.RUNFLAG      
                              prtpage prfile;RUNFLAG;       
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILNAME;     
.GUARPAY       
                              prtpage prfile;" ",GUARPAY;                                 
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AR;      
.PPFLAG       
.                             prtpage prfile;*boldon,PPFLAG,*boldoff;      
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                                                                           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP1;                    
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$NINC;                   
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$LRINC;                  
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$STAX;            
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$CTAX;            
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$POST;
                       add     eightlpi,row        
.begin patch 3.84
                              if        (newbiz = yes)
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"(NEW)",*boldoff;                   
                              endif
.end patch 3.84
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;  
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                       ELSE
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,MLRN;
.innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,INNETS;                     
                              prtpage prfile;LRN;                   
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVNUM;                     
.RUNFLAG      
                              prtpage prfile;RUNFLAG;       
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILNAME;     
.GUARPAY       
                              prtpage prfile;" ",GUARPAY;                                 
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AR;      
.PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
.                             prtpage prfile;*boldon,PPFLAG,*boldoff;                     
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP1;                    
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$NINC;                   
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$LRINC;                  
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$STAX;            
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$CTAX;            
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$POST;
                       add     eightlpi,row        
.begin patch 3.84
                              if        (newbiz = yes)
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"(NEW)",*boldoff;                   
                              endif
.end patch 3.84
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;    
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                       ENDIF
         ENDIF
         ADD       c2 TO LINES
         if        (tdmchrg = "Y" | runrar > 0)
         add        c2 to lines
.
         if        (runchrg > 0)
         add       runchrg to runrar             .add exchange charges.
         endif
.                                                .prog 19 charges
         move      runrar to runrlr
         sub       tdmcamt from runrlr
..................
.         
         move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         sub       runrpass,tdmcflat           .what part of tdmc charge is flat charges
         if        (tdmcflat < 0 )   
         move      c0 to tdmcflat
         endif
         
.         
         calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         calc      prcpass=runrpass/(runrpass+tdmcflat)*100
         calc      prcLRPASS=(RUNRLR/runrar)*100
         
         MOVE      MASK92 TO Mt$AP1        
         MOVE      MASK92 TO Mt$AP2
         MOVE      MASK92 TO Mt$AR
         MOVE      MASK92 TO Mt$lrinc
         EDIT      RUNRAR TO Mt$AR
         EDIT      RUNRPASS TO Mt$AP1
         EDIT      RUNRflat TO Mt$AP2
         EDIT      RUNRlr TO Mt$lrinc
.tdmcamt = $ from Triplex invoice file
.Runrar = total receivable $ from triplex related additional charges
.runrpass = receivables from Run Charge, commisionable selects
.runrflat = receivables from Flat charges ie Mag tape, Shipping, etc.
.IncRFLat   = Profit from flat charges (tdmcamt - runrpass = tdmc flat charges 'TDMCFLAT'; 
.runrflat - tdmcflat = IncRflat)         
         if        (runrlr < 0 | incRflat < 0)        
                      sub     eightlpi,row  
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,*ll,*ulon,*boldon,"S.B. Billed : ",tdmcamt,"  We Billed";
               prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,mt$ar;
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,mt$ap1; 
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"/m ",prcpass,"%";                                    
               prtpage prfile;*pcolumn7R:row,*ALIGNMENT=*Right,*ll,mt$lrinc;                                                                          
               prtpage prfile;*pcolumn7R:row,*ALIGNMENT=*Left,*ll,B1,PRCLRPASS,"%";                                                                                                 
                      add     eightlpi,row                  
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TDMCFLAT;              
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left," Flat ",PRCFLAT,"%";                                     
               prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,INCRFLAT;              
               prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,b2,PRCLRFLAT,"%",*uloff,*boldoff;                                    
                      add     eightlpi,row   
         else          
                      sub     eightlpi,row  
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,*ll,"S.B. Billed : ",tdmcamt,"  We Billed";
               prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,mt$ar;
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,mt$ap1; 
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"/m ",prcpass,"%";                                    
               prtpage prfile;*pcolumn7R:row,*ALIGNMENT=*Right,*ll,mt$lrinc;                                                                          
               prtpage prfile;*pcolumn7R:row,*ALIGNMENT=*Left,*ll,B1,PRCLRPASS,"%";                                                                                                 
                      add     eightlpi,row                  
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,TDMCFLAT;              
               prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left," Flat ",PRCFLAT,"%";                                     
               prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,INCRFLAT;              
               prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,b2,PRCLRFLAT,"%";                                     
                      add     eightlpi,row        
         endif
         clear     tdmchrg      
         add        runrar to grunrar
         add        runrpass to grunrpass
         add        runrflat to grunrflat    
         add        runrlr to grunrlr
         endif
...........................................................................         
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal
        CALL      HEADER IF not less         


........................................
         cmatch    yes to ppsw
         if        equal                                           1
         move      lrn to nmoafld5                                 1
         rep       zfill in nmoafld5                               1
         move      c5 to  nmoapath                                 1   
         call      nmoakey                                         1
         if        over                                     2 
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal
        CALL      HEADER IF not less
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
        add     eightlpi,row  
         move      yes to moabadflag
         add       c2 to lines                                     2
         goto      moaexit                                         2  
                endif                                              2
moaloop  compare   "18" to reason                                  1 
                if        equal                                    2
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal                                  2
        CALL      HEADER IF not less                               2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"MOA Prepayment ";
.slightly off alignment with column4r        
              prtpage prfile;*p3900:row,*ALIGNMENT=*Right,*ll,onamount;    
        add     eightlpi,row  
        add     eightlpi,row         
          IF        (Ocompid = "P" )       .may want to change to MOAcomp
         add       onamount to totPLpmoa       .total prepay moa     2
          Else
         add       onamount to totpmoa       .total prepay moa     2
          endif
         add       c2 to lines                                     2
         goto      moaexit                                         2
                endif                                              1
         call      nmoaks                                          2
                if        over                                     2
              COMPARE  "9900" to ROW
         CALL      HEADER IF equal                                 2
         CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
        add     eightlpi,row  
        add     eightlpi,row         
         move      yes to moabadflag
         add       c2 to lines                                     2
         goto      moaexit                                         2
                else                                                 
         goto      moaloop                                         2a 
                endif                                              2a
         endif                                                     1
         add       c1 to lines
moaexit  CLEAR     PMASK
.begin patch 3.72
.              call            debug
              IF              (ar <> HoldAr)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed Ar does not Match!!!!!!",HoldAr,*boldoff;
              endif
              IF              (ap1 <> HoldAp1)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP1 does not Match!!!!!!",HoldAP1,*boldoff;
              endif
              IF              (ap2 <> HoldAp2)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP2 does not Match!!!!!!",HoldAP2,*boldoff;
              endif
              IF              (ap3 <> HoldAp3)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP3 does not Match!!!!!!",HoldAP3,*boldoff;
              endif
.end patch 3.72

WRTSLS
         move      Mcomp,sLSCNAME
         CMATCH    "P" TO STATB
         if        equal
         move      "P" to slscode
         else
         move      "O" to slscode
         endif
         MOVE      C0 TO FORM102
         ADD       FORMAR TO FORM102
         MULT      HUND BY FORM102
         MOVE      C0 TO FORM9B
         ADD       FORM102 TO FORM9B    *A/R
.
         MOVE      C0 TO FORM102
         MOVE      AP TO FORM102
         MULT      HUND BY FORM102
         MOVE      C0 TO FORM9A
         ADD       FORM102 TO FORM9A
.
         MOVE      C0 TO FORM102
         MOVE      FORMAP2 TO FORM102
         MULT      HUND INTO FORM102
         MOVE      C0 TO FORM9
         ADD       FORM102 TO FORM9
         MOVE      NO TO APSW
         COMPARE   C0 TO FORM9A
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
         COMPARE   C0 TO FORM9
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
...
         MOVE      FORM9A TO DIM9A
         SCAN      MINUS IN DIM9A
         IF        EQUAL
         RESET     DIM9A
         CLEAR     AMOUNT1
         PACK      AMOUNT1 FROM C0,DIM9A
         CALL      KMINUS
         RESET     AMOUNT1 TO 4
         CLEAR     DIM9A
         APPEND    AMOUNT1 TO DIM9A
         ENDIF
.
....
         MOVE      FORM9B TO DIM9B
         SCAN      MINUS IN DIM9B
         IF        EQUAL
         RESET     DIM9B
         CLEAR     AMOUNT1
         PACK      AMOUNT1 FROM C0,DIM9B
         CALL      KMINUS
         RESET     AMOUNT1 TO 3
         CLEAR     DIM9B
         APPEND    AMOUNT1 TO DIM9B
         RESET     DIM9B
         ENDIF
.
         MOVE      FORM9 TO DIM9
         SCAN      MINUS IN DIM9
         IF        EQUAL
         RESET     DIM9
         CLEAR     AMOUNT1
         PACK      AMOUNT1 FROM C0,DIM9
         CALL      KMINUS
         RESET     AMOUNT1 TO 2
         CLEAR     DIM9
         APPEND    AMOUNT1 TO DIM9
         ENDIF
.
         RESET     DIM9A
         RESET     DIM9B
         RESET     DIM9
         REP       ZFILL IN DIM9A     .A/P1
         REP       ZFILL IN DIM9B     .A/R
         REP       ZFILL IN DIM9     .A/P2
OWNPREP  MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF
         clear     str12
         pack      str12 from invdtem,"/",invdted,"/",invdtec,invdtey
         compare   c1 to count
         if        equal
         write     clninv,seq;*cdfon,"Mailer ##","Lr##","Inv##","Prepay","Owner","Brk Guar":
                   "cnt","List","Ord Date","A/P1","Guar","adj cde","Inv Date","A/P2","List":
                   "Status","Guar","A/R","CHK Date","Mailer","TDMC A/R","TDMC","NIN","LR","A/P3","XNINC","Sales","team"
         endif           
          WRITE     clninv,SEQ;*cdfon,MLRN:
                   LRN:
                   invnum:
                   ppflag:
                   OLON:
                   OBRKGUAR:
                   COBN:
                   OLNUM:
                   *ZF,str10:
                   *ZF,ap1:
                   WSJPC:
                   ADJC:
                   *zf,str12:
                   *ZF,ap2:
                   O1DES:
                   slscode:
                   GUARPAY:
                   form102w:
                   *ZF,str10a:
                   SLSCNAME:                     .   -105
                   tdmcamt:                      triplex billings to us
                   runrar:
                   nininc:
                   lrinc:                        .We billed for tdmc charges
                 *ZF,ap3:                     .patch 3.75
                   XNINC:                     .patch 3.75
                      str2:
                      salesbr
         move       c0 to runrar
         move       c0 to runrlr
         move       c0 to runrpass
         move       c0 to runrflat
.         cmatch    yes to apsw
.         goto      input if not equal
          IF        (apsw = yes)
          WRITE     SLSFILE,SEQ;MLRN:
                   LRN:
                   OLON:
                   OBRKGUAR:
                   COBN:
                   OLNUM:
                   OMDTEC,OMDTEY,OMDTEM,OMDTED:
                   *ZF,ap1:
                   WSJPC:
                   ADJC:
                   invdtec,INVDTEY,INVDTEM,INVDTED:
                   *ZF,ap2:
                   O1DES:
                   slscode:
                   GUARPAY:
                   form102w:
                   chk1dtec:
                   chk1dtey:                        -100
                   chk1dtem:
                   chk1dted:
                   SLSCNAME                     .   -105
          endif
.end patch 3.0
          if        (totlr <> LRBRINC+LRBEINC+LRMRINC+LRMEINC+Coldbr+Coldbe+Coldlm+LREXFEE+LRBBR+LRBBE)
.         call      debug
.               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"**** LR Out of Balance!!!!!!****",*boldoff;   
.                      add     eightlpi,row                                 
          endif
         GOTO      INPUT
*......................................................................
.
TOTAL    
              COMPARE  "6000" to ROW
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$AR
         EDIT      TOTAR TO MT$AR

         MOVE      MASK92 TO MT$ARp
         mult      seq by totarp
         EDIT      TOTARp TO MT$ARp

         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP1 TO MT$AP1

         MOVE      MASK92 TO MT$AP2
         EDIT      TOTAP2 TO MT$AP2

         MOVE      MASK92 TO MT$NINC
         EDIT      TOTNIN TO MT$NINC

         MOVE      MASK92 TO MT$LRINC
         EDIT      TOTLR TO MT$LRINC

         MOVE      MASK92 TO MT$STAX
         EDIT      TOTSTAX TO MT$STAX

         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTCTAX TO MT$CTAX

         MOVE      MASK52 TO MT$POST
         EDIT      TOTPOST TO MT$POST
.         
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"## INVOICES ",COUNT,*boldoff;
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Totals",*boldoff;                              
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff; 
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                            
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$CTAX,*boldoff;
        add     eightlpi,row           
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;               
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$NINC,*boldoff;                             
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$STAX,*boldoff;                             
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$POST,*boldoff;        
        add     eightlpi,row                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Code 96 ",*boldoff;                     
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$ARP,*boldoff;                              
        add     eightlpi,row                                
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Total MOA Debit",*boldoff;      
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$PMOA,*boldoff;                             
        add     eightlpi,row                 
         cmatch    yes to moabadflag
         if        equal
                      add     eightlpi,row                                 
                      add     eightlpi,row   
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"****MOA Prepayment entries are MISSING!!!!!!****",*boldoff;   
                      add     eightlpi,row                                 
                      add     eightlpi,row                                                        
         endif

         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP TO MT$AP1

              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL ACCOUNTS PAYABLE",*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;               
        add     eightlpi,row                 
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBRINC TO MT$LRINC

         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR

         EDIT      ARBR TO MT$AR
         EDIT      APBR TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"BROKERAGE/RENTAL",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBEINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARBE TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APBE TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"BROKERAGE/EXCHANGE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LRMRINC TO LRMINC
         ADD       LRMEINC TO LRMINC
         EDIT      LRMINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         ADD       ARMR TO ARM
         ADD       ARME TO ARM
         EDIT      ARM TO MT$AR

         MOVE      MASK92 TO MT$AP1
         MOVE      C0 TO APM
         ADD       APMR TO APM
         ADD       APME TO APM
         EDIT      APM TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"LIST MANAGEMENT",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            Coldbr to Mt$Lrinc
.begin patch 3.84              
          MOVE      MASK92 TO MT$AR
          Edit      ColdBAR,MT$AR
          MOVE      MASK92 TO MT$AP1
          Edit      ColdBAP,MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Rent",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
.end patch 3.84              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            Coldbe to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Exch",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdLm to Mt$Lrinc
.begin patch 3.84              
          MOVE      MASK92 TO MT$AR
          Edit      ColdLAR,MT$AR
          MOVE      MASK92 TO MT$AP1
          Edit      ColdLAP,MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New List management",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
.end patch 3.84              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
.        add     eightlpi,row  
.              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New LR Income excluded in TOTALS",*boldoff;                    
        add     eightlpi,row  
.
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LREXFEE TO LRMINC
         EDIT      LRMINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         ADD       AREXFEE TO ARM
         EDIT      ARM TO MT$AR

         MOVE      MASK92 TO MT$AP1
         MOVE      C0 TO APM
         ADD       APEXFEE TO APM
         EDIT      APM TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT EXCH FEE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBR TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARBBR TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APBBR TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/RENT BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBE TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARBBE TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APBBE TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/EXCH BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/RENT ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBEINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/EXCH ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
.         add       ninmeinc to ninmrinc
           move       c0,calc92
           add        NINmrinc,calc92
           add        ninmeinc,calc92
.         EDIT      NINMRINC TO MT$LRINC
         EDIT      Calc92 TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL MANAGEMENT ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
        add     eightlpi,row         
.
..............................................................................
.TRIPLEX  EXCHANGES
         MOVE      MASK92 TO MT$LRINC
         EDIT      RUNLR TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      RUNAR TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      RUNPASS TO MT$AP1

         MOVE      MASK92 TO MT$AP2
         EDIT      RUNFLAT TO MT$AP2

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"S.B. Breakout",*ULOFF,*boldoff;                    
        add     eightlpi,row  
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Running Charge Invoices","##",RUNCOUNT,*boldoff;                            
        add     eightlpi,row  
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;                                              
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                                          
        add     eightlpi,row                 
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"FLAT PASS THROUGH",*boldoff;                                                                  
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;                                                     
        add     eightlpi,row                 
        add     eightlpi,row                         
.
.TRIPLEX  prog 19 entries for month
         MOVE      MASK92 TO MT$LRINC
         EDIT      GRUNRLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      GRUNRAR TO MT$AR
         EDIT      GRUNRPASS TO MT$AP1
         EDIT      GRUNRFLAT TO MT$AP2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Rental TDMC & Prog 19","##",RUNRCNT,*boldoff; 
        add     eightlpi,row  
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;                                              
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                                          
        add     eightlpi,row                 
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"FLAT PASS THROUGH",*boldoff;                                                                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;                                                     
        add     eightlpi,row  
        add     eightlpi,row          
.
         MOVE      MASK92 TO MT$TDMC
         EDIT      TOTTDMC TO MT$TDMC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"S.B. Billed us  Charges",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$TDMC,*boldoff;                                                           

.begin patch 3.75
        add     eightlpi,row  
        add     eightlpi,row          
              COMPARE  "6000" to ROW
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$AR
         EDIT      TOTPLAR TO MT$AR

         MOVE      MASK92 TO MT$ARp
         mult      seq by totPLarp
         EDIT      TOTPLARp TO MT$ARp

         MOVE      MASK92 TO MT$AP1
         EDIT      TOTPLAP1 TO MT$AP1

         MOVE      MASK92 TO MT$AP2
         EDIT      TOTPLAP2 TO MT$AP2

         MOVE      MASK92 TO MT$NINC
         EDIT      TOTPLNIN TO MT$NINC

         MOVE      MASK92 TO MT$LRINC
         EDIT      TOTPLLR TO MT$LRINC

         MOVE      MASK92 TO MT$STAX
         EDIT      TOTPLSTAX TO MT$STAX

         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTPLCTAX TO MT$CTAX

         MOVE      MASK52 TO MT$POST
         EDIT      TOTPLPOST TO MT$POST
.         
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"## PL INVOICES ",COUNTPL,*boldoff;
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Totals",*boldoff;                              
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff; 
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                            
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$CTAX,*boldoff;
        add     eightlpi,row           
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;               
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$NINC,*boldoff;                             
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$STAX,*boldoff;                             
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$POST,*boldoff;        
        add     eightlpi,row                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Code 96 ",*boldoff;                     
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$ARP,*boldoff;                              
        add     eightlpi,row                                
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTPLpmoa TO MT$pmoa
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Total MOA Debit",*boldoff;      
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$PMOA,*boldoff;                             
        add     eightlpi,row                 
         cmatch    yes to moabadflag
         if        equal
                      add     eightlpi,row                                 
                      add     eightlpi,row   
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"****MOA Prepayment entries are MISSING!!!!!!****",*boldoff;   
                      add     eightlpi,row                                 
                      add     eightlpi,row                                                        
         endif

         ADD       TOTPLAP1 TO TOTPLAP
         ADD       TOTPLAP2 TO TOTPLAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTPLAP TO MT$AP1

              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL ACCOUNTS PAYABLE",*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;               
        add     eightlpi,row                 
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBRINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBR TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APPLBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"BROKERAGE/RENTAL",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBEINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBE TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APPLBE TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"BROKERAGE/EXCHANGE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRPLMINC
         ADD       LRPLMRINC TO LRPLMINC
         ADD       LRPLMEINC TO LRPLMINC
         EDIT      LRPLMINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         ADD       ARPLMR TO ARM
         ADD       ARPLME TO ARM
         ADD       APPLMR TO APM
         EDIT      ARM TO MT$AR

         MOVE      MASK92 TO MT$AP1
         MOVE      C0 TO APM
         ADD       APPLME TO APM
         EDIT      APM TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"LIST MANAGEMENT",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdPLbr to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Rent",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdPLbe to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Exch",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdPLLm to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New List management",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
.        add     eightlpi,row  
.              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New LR Income excluded in TOTALS",*boldoff;                    
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRPLMINC
         ADD       LRPLEXFEE TO LRPLMINC
         EDIT      LRPLMINC TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         ADD       ARPLEXFEE TO ARM
         EDIT      ARM TO MT$AR

         MOVE      MASK92 TO MT$AP1
         MOVE      C0 TO APM
         ADD       APPLEXFEE TO APM
         EDIT      APM TO MT$AP1

              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT EXCH FEE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBBR TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBBR TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APPLBBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/RENT BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBBE TO MT$LRINC

         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBBE TO MT$AR

         MOVE      MASK92 TO MT$AP1
         EDIT      APPLBBE TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/EXCH BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINPLBRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/RENT ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINPLBEINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/EXCH ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         add       ninPLmeinc to ninPLmrinc
         EDIT      NINPLMRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL MANAGEMENT ACD",*boldoff;                    
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
        add     eightlpi,row         
.
..............................................................................
.end patch 3.75

         GOTO      EOJ
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
              if not equal
                                             PRTPAGE prfile;*NEWPAGE:
                                             *UNITS=*HIENGLISH:
                        *ORIENT=*PORTRAIT:
                                             *Duplex=2                        
              endif
        MOVE      c6 TO LINES 
              clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        clock   timestamp,str8
.        unpack  str8,str2,yy,mm,dd
.        clear   str10
.        pack    str10,mm,slash,dd,slash,str2,yy
.        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",Today;        

        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"NIN Monthly Sales Register",*boldoff;  
        add     eightlpi,row        
        add     eightlpi,row        
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
              prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Accounts",*boldoff;                               
              prtpage prfile;*pTitle2:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Commissions",*boldoff;                                          
              prtpage prfile;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Taxes",*boldoff;                                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Mailer Bill-To",*boldoff;                                        
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Mailer",*ULOFF,*boldoff;           
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"LR    ",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Invoice",*ULOFF,*boldoff;                
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Name and Thru",*ULOFF,*boldoff;                                        
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Receivable",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Payable",*ULOFF,*boldoff;                             
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"NIN Income",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"LR Income",*ULOFF,*boldoff;                           
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"State",*ULOFF,*boldoff;         
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"City",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Postage",*ULOFF,*boldoff;                     
        add     eightlpi,row  
        add     eightlpi,row  

         RETURN
.
*........................
MISSING  
              COMPARE  "9900" to ROW

        CALL      HEADER IF equal
        CALL      HEADER IF not less
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"********* MISSING INVOICE ******",HOLDREC,*ULOFF,*boldoff;          
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"********* MISSING INVOICE ******",HOLDREC,*ULOFF,*boldoff;              
        add     eightlpi,row  
              clear str6
              move holdrec to str6
              append str6,taskname
              append crlf,taskname
         DISPLAY   *P10:15,"MISSING INVOICE: ",HOLDREC
         ADD       C1 TO HOLDREC
         add       c3 to lines
         GOTO      MISSCHK
........................
.KMINUS - CONVERT TO MINUS OVERPUNCH. ENTER WITH AMOUNT1 EXIT AMOUNT1
KMINUS
         RESET     AMOUNT1
         MOVE      AMOUNT1 TO NEWFLD
         REP       "-0" IN NEWFLD
         ENDSET    NEWFLD
         REP       TOMOV IN NEWFLD         *MINUS OVERPUNCH CONVERT
         RESET     NEWFLD
         CLEAR     AMOUNT1
         MOVE      NEWFLD TO AMOUNT1
         RETURN
swaptype
                 IF      (srcr = "M")
                 return
                 endif
.CHANGE ANNUALLY                 
                 match   "R" to typer
                 if      equal
.begin patch 3.81
.needs previous year if $ its not new
                 pack    nrevfld from "EB","00",Cidr,"2015"            ....YEARLY
                 else
                 pack    nrevfld from "RB","00",Cidr,"2015"
.end patch 3.81
                 endif
                 call    nrevkey
                 return
.
.
EOJ
              prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
.begin patch 3.9
.          Call      PDF995Auto0
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.              pack            APIFileName,STR45,hexzero
.              loop
.               call           FindFirstFile
.               until (APIResult = 0 | APIResult = hexeight)
.               pause          "1"
.              repeat
.end patch 3.9
              pause           "2"
         WEOF      SLSFILE,SEQ
         CLOSE     SLSFILE,EOFSIZE
         WEOF      clninv,SEQ
         CLOSE     clninv,EOFSIZE
         MOVE      NO to str1
              if (INITS = YES)
               reset taskname
               if (taskname = " ")
                              move "No missing Orders" to taskname
               endif
               alert caution,taskname,result,"Missing Invoices (if blank there are none)"
               pause c2
               move nextrec to str6
               alert caution,str6,result,"Last Invoice(Please check invoice program to confirm this is last invoice for the month"     
              endif
updater
         KEYIN     *P1:24,*EL,*B,*B,*B,"OK TO UPDATE NINVLAST NOW  ?",*RV,*t254,STR1;
         CMATCH    YES TO STR1
         IF         EQUAL
                       MOVE      "NINVNXT" TO GNXTFLD
                       CALL      GNXTKEY
                       MOVE      GNXTNUM TO lastinv
                       MOVE     "NINVLAST" TO GNXTFLD
                       CALL     GNXTKEY
                       MOVE     lastinv TO GNXTNUM
                       CALL     GNXTUPD
                       keyin   *p1:24,*el,"Log Number Then hit Enter  ",*p20:24,*DV,lastinv,*p27:24,str1
                       goto    fini
         endif
         add      c1 to check    
         compare  c2 to check
         goto     updater if not equal
fini
.........................................................................................................................
          if        (count = c0)
          endif
          Move      "NEOM0004 - output",MailSubjct
          Move      "creques@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",Mailreply
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
          
          Append    Count,MailBody
          Append    B1,MailBody
          Append    "Invoices",MailBody
          Append    "<br>",mailbody
.begin patch 3.84
          Append    "A/R TOTALS ",Mailbody
          Append    TotAR,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Rental A/R ",Mailbody
          Append    ARBR,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Prev Rental A/R ",Mailbody
          Append    ARBBR,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Exchange A/R ",Mailbody
          Append    ARBE,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Prev Exch A/R ",Mailbody
          Append    ARBBE,Mailbody
          Append    "<br>",mailbody
          Move      C0,ARM
          add       ARMR,ARM
          add       ARME,ARM
          Append    "Management  A/R ",Mailbody
          Append    ARM,Mailbody
          Append    "<br>",mailbody
          Append    "Unknown A/R   ",Mailbody
          Append    ARUNKN,Mailbody
          Append    "<br>",mailbody
          Append    "A/P TOTALS ",Mailbody
          Append    TotAp,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Rental A/P ",Mailbody
          Append    APBR,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Prev Rental A/P ",Mailbody
          Append    APBBR,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Exchange A/P ",Mailbody
          Append    APBE,Mailbody
          Append    "<br>",mailbody
          Append    "Brokerage Prev Exch A/P ",Mailbody
          Append    APBBE,Mailbody
          Append    "<br>",mailbody
          Move      C0,APM
          add       APMR,APM
          add       APME,APM

          Append    "Management  A/P ",Mailbody
          Append    APM,Mailbody
          Append    "<br>",mailbody
          Append    "Unknown A/P   ",Mailbody
          Append    APUNKN,Mailbody
          Append    "<br>",mailbody
          
          Append    "Cold Brokerage  A/R A/P ",Mailbody
          Append    ColdBAR,Mailbody
          Append    " ,",MailBody
          Append    ColdBAP,Mailbody
          Append    "<br>",mailbody
          Append    "Cold Management  A/R A/P ",Mailbody
          Append    ColdLAR,Mailbody
          Append    " ,",MailBody
          Append    ColdLAP,Mailbody
          Append    "<br>",mailbody
          
.end patch 3.84


          Append    "LR INCOME TOTALS ",Mailbody
          Append    TotLR,Mailbody
          Append    "<br>",mailbody
          Append    "NIN INCOME TOTALS ",Mailbody
          Append    TotNIN,Mailbody
          Append    "<br>",mailbody
          Append    "LR INCOME Rental ",Mailbody
          Append    LRBRINC,Mailbody
          Append    "<br>",mailbody
          Append    "LR INCOME Exchange ",Mailbody
          Append    LRBEINC,Mailbody
          Append    "<br>",mailbody
.
              MOVE      C0 TO LRMINC
              ADD       LRMRINC TO LRMINC
              ADD       LRMEINC TO LRMINC
          Append    "LR INCOME List Management  ",MailBody            
          Append    LRMINC,MailBody
          append    "<br>",mailbody
              MOVE      C0 TO LRMINC
              ADD       LREXFEE TO LRMINC
          Append    "List Management Exch Fee ",MailBody              
          Append    LRExFee,MailBody
          append    "<br>",mailbody
          Append    "LR Income Rent (Previous Month) ",MailBody                 
          Append    LRBBr,MailBody
          append    "<br>",mailbody
          Append    "LR Income Exch (Previous Month) ",MailBody                 
          Append    LRBBE,MailBody
          append    "<br>",mailbody
.
.begin patch 3.7
.jose here
          Append    "New Business Brokerage Rent ",MailBody           
          Append    ColdBr,MailBody
          append    "<br>",mailbody
.
          Append    "New Business Brokerage Exch ",MailBody           
          Append    ColdBe,MailBody
          append    "<br>",mailbody
          Append    "New Business List Management ",MailBody                    
          Append    ColdLM,MailBody
          append    "<br>",mailbody
.begin patch 3.75
.         Reset     MailBody
.end patch 3.7
          Append    "PL Invoices",MailBody
          Append    "<br>",mailbody
          Append    "PL LR INCOME TOTALS ",Mailbody
          Append    TotPLLR,Mailbody
          Append    "<br>",mailbody
          Append    "PL NIN INCOME TOTALS ",Mailbody
          Append    TotPLNIN,Mailbody
          Append    "<br>",mailbody
          Append    "PL LR INCOME Rental ",Mailbody
          Append    LRPLBRINC,Mailbody
          Append    "<br>",mailbody
          Append    "PL LR INCOME Exchange ",Mailbody
          Append    LRPLBEINC,Mailbody
          Append    "<br>",mailbody
.
.DH Aug 1 2008
.              MOVE      C0 TO LRMINC
              MOVE      C0 TO LRPLMINC
.DH Aug 1 2008
              ADD       LRPLMRINC TO LRPLMINC
              ADD       LRPLMEINC TO LRPLMINC
          Append    "PL LR INCOME List Management  ",MailBody                   
          Append    LRPLMINC,MailBody
          append    "<br>",mailbody
              MOVE      C0 TO LRMINC
              ADD       LRPLEXFEE TO LRMINC
          Append    "PL List Management Exch Fee ",MailBody           
          Append    LRPLExFee,MailBody
          append    "<br>",mailbody
          Append    "PL LR Income Rent (Previous Month) ",MailBody              
          Append    LRPLBBr,MailBody
          append    "<br>",mailbody
          Append    "PL LR Income Exch (Previous Month) ",MailBody              
          Append    LRPLBBE,MailBody
          append    "<br>",mailbody
.
.begin patch 3.7
.jose here
          Append    "PL New Business Brokerage Rent ",MailBody                  
          Append    ColdPLBr,MailBody
          append    "<br>",mailbody
.
          Append    "PL New Business Brokerage Exch ",MailBody                  
          Append    ColdPLBe,MailBody
          append    "<br>",mailbody
          Append    "PL New Business List Management ",MailBody                 
          Append    ColdPLLM,MailBody
          append    "<br>",mailbody
.begin patch 3.92
           RESET      NewBizbody           
           append     NewBizbody,Mailbody
          append    "<br>",mailbody
.end patch 3.92

.end patch 3.75
          Reset     MailBody
          move      c1,MailType

          Call      SendMail
.End patch 3.73     
.Open Excel application
OhMy
          if        (count = c0)
          goto      JDExit                        .no records processed skip this
          endif
              Create  ex
              rep   zfill,str4
.              pack            Taskname,"\\nins1\d\users\dherric\monday2009.xlsx"
.              pack            Taskname,"\\nins1\d\users\dherric\monday2009.xls"       
.begin patch 3.83
.              pack            Taskname,"\\nins1\d\accounting\monday2009.xls"       
              pack            Taskname,"\\nins1\d\accounting\monday2016.xlsx"       
.end patch 3.83
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
           unpack today into mm,str1,dd,str1,yy
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
        pack    RowNumber,"C","1"
        setprop sheet.range(RowNumber),*Value=str10
        pack    RowNumber,"D","1"
        setprop sheet.range(RowNumber),*Value=Time
        pack    RowNumber,"A","1"
        setprop sheet.range(RowNumber),*Value="Brokerage Total"
        pack    RowNumber,"B","1"
        setprop sheet.range(RowNumber),*Value=Totlr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","2"
        setprop sheet.range(RowNumber),*Value="NIN Total"
        pack    RowNumber,"B","2"
        setprop sheet.range(RowNumber),*Value=TOTNIN,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","3"
        setprop sheet.range(RowNumber),*Value="Brokerage Rent"
        pack    RowNumber,"B","3"
        setprop sheet.range(RowNumber),*Value=LRBRINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","4"
        setprop sheet.range(RowNumber),*Value="Brokerage Exch"
        pack    RowNumber,"B","4"
        setprop sheet.range(RowNumber),*Value=LRBEINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","5"
              MOVE      C0 TO LRMINC
              ADD       LRMRINC TO LRMINC
              ADD       LRMEINC TO LRMINC
        setprop sheet.range(RowNumber),*Value="List Management"
        pack    RowNumber,"B","5"
        setprop sheet.range(RowNumber),*Value=LRMINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","6"
        setprop sheet.range(RowNumber),*Value="List Management EX Fee"
        pack    RowNumber,"B","6"
        setprop sheet.range(RowNumber),*Value=LRExfee,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","7"
        setprop sheet.range(RowNumber),*Value="Brokerage Rent Previous"
        pack    RowNumber,"B","7"
        setprop sheet.range(RowNumber),*Value=LRBBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","8"
        setprop sheet.range(RowNumber),*Value="Brokerage Exch Previous"
        pack    RowNumber,"B","8"
        setprop sheet.range(RowNumber),*Value=LRBBE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.7
.Here Jose
        pack    RowNumber,"A","38"
        setprop sheet.range(RowNumber),*Value="New Biz Brokerage Rent"
        pack    RowNumber,"B","38"
       setprop sheet.range(RowNumber),*Value=coldbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","39"
        setprop sheet.range(RowNumber),*Value="New Biz Brokerage Exch"
        pack    RowNumber,"B","39"
        setprop sheet.range(RowNumber),*Value=coldbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","40"
        setprop sheet.range(RowNumber),*Value="New Biz List Management"
        pack    RowNumber,"B","40"
        setprop sheet.range(RowNumber),*Value=coldlm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.7
.begin patch 3.75
        pack    RowNumber,"F","1"
        setprop sheet.range(RowNumber),*Value=TotPLlr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","2"
        setprop sheet.range(RowNumber),*Value=TOTPLNIN,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","3"
        setprop sheet.range(RowNumber),*Value=LRPLBRINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","4"
        setprop sheet.range(RowNumber),*Value=LRPLBEINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
              MOVE      C0 TO LRPLMINC
              ADD       LRPLMRINC TO LRPLMINC
              ADD       LRPLMEINC TO LRPLMINC
        pack    RowNumber,"F","5"
        setprop sheet.range(RowNumber),*Value=LRPLMINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","6"
        setprop sheet.range(RowNumber),*Value=LRPLExfee,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","7"
        setprop sheet.range(RowNumber),*Value=LRPLBBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","8"
        setprop sheet.range(RowNumber),*Value=LRPLBBE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","38"
       setprop sheet.range(RowNumber),*Value=coldPLbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","39"
        setprop sheet.range(RowNumber),*Value=coldPLbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","40"
        setprop sheet.range(RowNumber),*Value=coldPLlm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

.end patch 3.75
.begin patch 3.84
        pack    RowNumber,"A","43"
        setprop sheet.range(RowNumber),*Value="Sales Total AR"
        pack    RowNumber,"A","44"
        setprop sheet.range(RowNumber),*Value="Sales Total AP"
        pack    RowNumber,"A","45"
        setprop sheet.range(RowNumber),*Value="Sales Brk Rent AR"
        pack    RowNumber,"A","46"
        setprop sheet.range(RowNumber),*Value="Sales Brk Rent Prev AR"
        pack    RowNumber,"A","47"
        setprop sheet.range(RowNumber),*Value="Sales Brk Exch AR"
        pack    RowNumber,"A","48"
        setprop sheet.range(RowNumber),*Value="Sales Brk Exch Prev AR"
        pack    RowNumber,"A","49"
       setprop sheet.range(RowNumber),*Value="Sales Management AR"
        pack    RowNumber,"A","50"
        setprop sheet.range(RowNumber),*Value="Sales Unknown AR"
        pack    RowNumber,"A","51"
        setprop sheet.range(RowNumber),*Value="Sales Brk Rent AP"
        pack    RowNumber,"A","52"
        setprop sheet.range(RowNumber),*Value="Sales Brk Rent Prev AP"
        pack    RowNumber,"A","53"
        setprop sheet.range(RowNumber),*Value="Sales Brk Exch AP"
        pack    RowNumber,"A","54"
        setprop sheet.range(RowNumber),*Value="Sales Brk Exch Prev AP"
        pack    RowNumber,"A","55"
        setprop sheet.range(RowNumber),*Value="Sales Management AP"
        pack    RowNumber,"A","56"
        setprop sheet.range(RowNumber),*Value="Sales Unknown AP"
        pack    RowNumber,"A","71"
        setprop sheet.range(RowNumber),*Value="New Biz Brokerage AR"
        pack    RowNumber,"A","72"
        setprop sheet.range(RowNumber),*Value="New Biz Brokerage AP"
        pack    RowNumber,"A","73"
        setprop sheet.range(RowNumber),*Value="New Biz Management AR"
        pack    RowNumber,"A","74"
        setprop sheet.range(RowNumber),*Value="New Biz Management AP"

        pack    RowNumber,"B","43"
        setprop sheet.range(RowNumber),*Value=Totar,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","44"
        setprop sheet.range(RowNumber),*Value=TotaP,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","45"
        setprop sheet.range(RowNumber),*Value=arbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","46"
        setprop sheet.range(RowNumber),*Value=arbbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","47"
        setprop sheet.range(RowNumber),*Value=arbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","48"
        setprop sheet.range(RowNumber),*Value=arbbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","49"
          Move      C0,ARM
          add       ARMR,ARM
          add       ARME,ARM
.          add       AREXFEE,arm
       setprop sheet.range(RowNumber),*Value=arM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","50"
        setprop sheet.range(RowNumber),*Value=arunkn,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","51"
        setprop sheet.range(RowNumber),*Value=apbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","52"
        setprop sheet.range(RowNumber),*Value=apbbr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"B","53"
        setprop sheet.range(RowNumber),*Value=apbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","54"
        setprop sheet.range(RowNumber),*Value=apbbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
          Move      C0,APM
          add       APMR,APM
          add       APME,APM
.          add       APEXFEE,aPm

        pack    RowNumber,"B","55"
        setprop sheet.range(RowNumber),*Value=apM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","56"
        setprop sheet.range(RowNumber),*Value=apunkn,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"B","71"
        setprop sheet.range(RowNumber),*Value=ColdBAR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","72"
        setprop sheet.range(RowNumber),*Value=ColdBAP,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","73"
        setprop sheet.range(RowNumber),*Value=ColdLAR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","74"
        setprop sheet.range(RowNumber),*Value=ColdLAP,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.84
.begin patch 3.91
        pack    RowNumber,"A","79"
        setprop sheet.range(RowNumber),*Value="NIN MAN RENT "
        pack    RowNumber,"A","80"
        setprop sheet.range(RowNumber),*Value="NIN MAN EXCH "
        pack    RowNumber,"A","81"
        setprop sheet.range(RowNumber),*Value="NIN BRK RENT "
        pack    RowNumber,"A","82"
        setprop sheet.range(RowNumber),*Value="NIN BRK EXCH "
        pack    RowNumber,"A","85"
        setprop sheet.range(RowNumber),*Value="InfoGroup Amt "
        pack    RowNumber,"B","79"
        setprop sheet.range(RowNumber),*Value=NINMRINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","80"
        setprop sheet.range(RowNumber),*Value=NINMEINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","81"
        setprop sheet.range(RowNumber),*Value=NINBRINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","82"
        setprop sheet.range(RowNumber),*Value=NINBEINC,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"B","85"
        setprop sheet.range(RowNumber),*Value=tottdmc,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

.end patch 3.91
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
        ex.quit
        destroy ex
JDexit
         SHUTDOWN  "cls"
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         SHUTDOWN  "cls"
         STOP

                              include        compio.inc
                              include        cntio.inc
         INCLUDE   NORDIO.INC
              include         compute.inc
              INCLUDE         ninvio.inc
              Include         NInvAcdio.inc
         include   nownio.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         include   cvt.inc
         INCLUDE   GNXTIO.inc
         include   nmoaio.inc
         include   nmrgio.inc
         include   nslsio.inc
         include   tinvio.inc
         include   nxcgio.inc
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
        include nrevio.inc
        include nprjio.inc
         INCLUDE    COMLOGIC.inc



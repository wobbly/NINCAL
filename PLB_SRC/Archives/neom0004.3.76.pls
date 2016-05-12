...............................................................................
.NEOM0004 - sales register
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   consacct.inc
         include   hp.inc
.begin patch 3.4
.         INC       NINVDD.inc
              INC             ninvdd.inc
              Include         NInvAcddd.inc
.end patch 3.4
.patch3.37
                              include        compdd.inc
                              include        cntdd.inc
.         INC       NMLRDD.inc
.patch3.37
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         INCLUDE   GNXTDD.inc
         INCLUDE   NDAT3DD.inc
.patch3.37
.         INCLUDE   NBRKDD.INC
.patch3.37
.START PATCH 3.38 - ADDED LOGIC
        include winapi.inc
.END PATCH 3.38 - ADDED LOGIC
         inc       nmrgdd.inc
         include   nslsdd.inc
         include   nmoadd.inc
         include   nxcgdd.inc
         include   tinvdd.inc
.begin patch 3.0
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
.end patch 3.0
           include nprjdd.inc
           include nrevdd.inc
          
.
...........................................
Release	Init	"3.76"		JD	30March2007    Updated projection read

.Release	Init	"3.75"	DLH	12March2007    Pacific Lists - brokerage side

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
.RECNUM   FILE      FIX=42
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
.begin patch 3.72
HoldAR             form           10.2      ACCOUNTS RECEIVABLE (A/R),  X,XXX,XXX.XX
HoldAP1            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XX
HoldAP2            form           10.2      2ND ACCOUNTS PAYABLE (A/P2), X,XXX,XXX.XX
HoldAP3            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XXHOldAr        
.end patch 3.72

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
.end patch 3.75

. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
LManage  init      "018710"    List management exchange fees only
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
.begin patch 2.9
ninMRinc form      9.2      Total NIN ACD MAnAGEMENT RENTAL inc
ninMEinc form      9.2      Total NIN ACD MAnAGEMENT exch inc
ninBRinc form      9.2      Total NIN ACD Brokerage RENTAL inc
ninBEinc form      9.2      Total NIN ACD Brokerarge Exch inc
.end patch 2.9
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
.begin patch 3.7
COLDLm   FORM        9.2      New Biz LM
COLDBR   FORM        9.2      New Biz BR
COLDBE   FORM        9.2      New Biz BE
newbiz    init      "N"
.end patch 3.7
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
.START PATCH #2.8 - REPLACED VARS
.FORM7    FORM      7
FORM9A    FORM      9
.END PATCH #2.8 - REPLACED VARS
FORM52   FORM      5.2
.START PATCH #2.8 - DUPLICATE VAR
.FORM92   FORM      9.2
.END PATCH #2.8 - DUPLICATE VAR
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
TEAM2    INIT      "02"    Brokerage	.2007
TEAM3    INIT      "03"    LIST MANAGEMENT
.TEAM1    INIT      "01"     SUSAN
.TEAM2    INIT      "02"    ELAINE
.TEAM3    INIT      "03"    LIST MANAGEMENT
.RUNCODES INIT      "005051-009766"
DIM9     DIM       9
.START PATCH #2.8 - REPLACED VARS
.dim8     dim       8
.DIM7     DIM       7
dim9B    dim       9
DIM9A    DIM       9
.END PATCH #2.8 - REPLACED VARS
AMOUNT1  DIM       10
FORM102  FORM      10.2
FORM102W FORM      10.2
newfld   dim       10
MINUS    INIT      "-"
TOMOV    INIT      "0}1J2K3L4M5N6O7P8Q9R"
.START PATCH #2.8 - REPLACED VARS
.FORM8    FORM      8
FORM9B    FORM      9
.END PATCH #2.8 - REPLACED VARS
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
.begin patch 3.0
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.MASK32   INIT      "ZZZ.ZZ-"
.end patch 3.0
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
.START PATCH #2.8 - INCREASED VARS
.M$AR     DIM       13
M$AR     DIM       15
.END PATCH #2.8 - INCREASED VARS
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
.START PATCH #2.8 - INCREASED VARS
.M$AP1    DIM       13
.M$AP2    DIM       13
.M$STAX   DIM       8
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
.END PATCH #2.8 - INCREASED VARS
M$CTAX   DIM       8
M$POST   DIM       6
.START PATCH #2.8 - INCREASED VARS
.M$LRINC  DIM       13
.M$NINC   DIM       13
.M$GROSS  DIM       13
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.END PATCH #2.8 - INCREASED VARS
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
.begin release 3.2
moabadflag Init     "N"          Y = missing moa
check    form      1
.end release 3.2
.>Patch 3.38
PRFILE        pfile
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
Column8  form    9
Column9  form    9
Column10 form    9
Column11 form    9
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
typer    dim      1
srcr     dim      1
cidr     dim      6
mmrep    dim       2
yyrep    dim       2
ccrep    dim       2
.............................................................................................
.begin patch 3.6
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
.end patch 3.6
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
.        move    "8000",column11
        move    "4000",column4R
        move    "4700",column5R
        move    "5400",column6R
        move    "6100",column7R
        move    "6600",column8R
        move    "7100",column9R
        move    "7700",column10R
        
.>Patch 3.38         
.
         MOVE      "Names in the News" TO COMPNME
         MOVE      "NEOM0004" TO PROGRAM
         MOVE      "MONTHLY INVOICE REGISTER" TO STITLE
.>Patch 3.38         Comment Out
.         IFZ       PC
.         SPLOPEN   "\\nins1\e\data\SALESREG.LST"
.         XIF
.>Patch 3.38 Logic Addition for PDF Quality Control
              call            "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
              "Parameters":
              "ProcessPDF":
              "\\nts0\c\apps\plb\code\pdftest.bat":
              result
              if (result = C0)
.Prepare Flag file
               prep           tempfile,"c:\progra~1\pdf995\flag.dat"
               write          tempfile,SEQ;"flag set"
               close          tempfile
              endif
.>Patch 3.38 Logic Addition for PDF Quality Control                                       
.Replace Logic 3.38
.Begin patch 3.73
	If	(PRtName  = "")
	MOve	"SalesReg",PRtname
	endif
.End patch 3.73
               PRTOPEN prfile,"PDF995",PRTNAME
               PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                                              *Duplex=2;                                                 
.>Patch 3.38         
         OPEN      DUPEOWN,"\\nins1\e\data\index\DUPEOWN",READ
.         clear     str25
.         clear     str45
.         append    "\\nins1\d\users\accounti\gl\clninv." to str45
.         append    mm,str45
.         append    ".tmp" to str45
.         reset      str45
.         PREPARE   clninv,str45
         PREPARE   SLSFILE,"\\nins1\e\data\SLSTEMP.new"
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
.	MOVe	"05-31-07",today
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
         clear     str45
.>Patch 3.5 Code Modified         
.         append    "\\nins1\d\users\accounti\gl\clninv." to str45
         append    "\\nins1\d\accounting\gl\clninv." to str45
.>Patch 3.5 Code Modified End         
         append    sysmo,str45
         append    ".tmp" to str45
         reset      str45
         PREPARE   clninv,str45
.
.DATE     MOVE      YES TO ANS
.         KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,ANS
.         CMATCH    YES TO ANS
.         GOTO      OPEN IF EQUAL
.         GOTO      DATE IF EOS
.         KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,SYSMO,"/":
.                   *DE,*JR,*ZF,*RV,SYSDY,"/",*DE,*JR,*ZF,*RV,SYSYR
.         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
.         MOVE      DATE TO DATEMASK
.         MOVE      DATEMASK TO TODAY
.         CALL      PAINT
.         CALL      FUNCDISP
.         GOTO      DATE
OPEN     TRAP      SHAREINV GIVING ERROR IF IO
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
.         DISPLAY   *P1:23,*EL,"OPENING NININV2 READ ONLY";
.         OPEN      NINVFILE,"NININV2",READ
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
.         OPEN      RECNUM,"INVNUM"
         TRAPCLR   IO
         GOTO      BEGIN
SHAREINV TRAPCLR   IO
         DISPLAY   *P1:23,*EL,"NININV2 READ ONLY FAILED, FILE SHARED";
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
.         OPEN      NINVFILE,"NININV2",SHARE
         GOTO      OPENREST
BEGIN
..         MOVE      BEGINV TO NINVFLD
           MOVE      "NINVLAST" TO GNXTFLD
           CALL      GNXTKEY
           MOVE      GNXTNUM TO NINVFLD
           MOVE      GNXTNUM TO HOLDREC
.           move	"540769",ninvfld
.           move	"540769",holdrec
           
           REP       ZFILL IN GNXTFLD
         CALL      NINVTST
.         MOVE      BEGINV TO HOLDREC
         ADD       C1 TO HOLDREC
.
INPUT    COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
.
.
READINV  CALL      NINVKS
         GOTO      TOTAL IF OVER
         MOVE      B1 TO RUNFLAG
         MATCH     INVDTEM TO SYSMO
         GOTO      TOTAL IF NOT EQUAL
.begin patch 3.75
.         ADD       C1 TO COUNT
.         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
.                   *P15:07,today
.end patch 3.75
.begin patch 3.0
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 3.0
.
.begin patch 3.72
              Move            c0,holdar
              Move            c0,holdap1
              Move            c0,holdap2
              Move            c0,holdap3
              Move            Ar,HOldar
              move            ap1,holdap1
              move            ap2,holdap2
              move            ap3,holdap3
.end patch 3.72

MISSCHK  MOVE      INVNUM TO NEXTREC          *CHECK
         COMPARE   NEXTREC TO HOLDREC         * FOR MISSING
         GOTO      MISSING IF NOT EQUAL        *INVOICE
         ADD       C1 TO HOLDREC
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.
.begin patch 3.75
	if	(OcompId = "P")	  		
	ADD       C1 TO COUNTPL
	else
	ADD       C1 TO COUNT
	endif
         DISPLAY   *P10:12,"NUMBER OF NIN INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
                   *P10:13,"NUMBER OF PL  INVOICES PROCESSED: ",COUNTPL,b5,lrn,b1,invnum:
                   *P15:07,today
.end patch 3.75

.begin patch 3.1.1
         DISPLAY   *P10:15,"Mail Date",b5,b5,omdtem,slash,omdted,slash,omdtec,omdtey
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST          .triplex running charge list?
         IF         EQUAL                     .Yes
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
.START PATCH #2.8 - INCREASED VARS
.         MOVE       C0 TO FORM72
.         MOVE       QTYSHP TO FORM82
.         MOVE       FORM82 TO FORM72    
.         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
.         MULT       ".00156" BY FORM72          40%  COMMISSION ON 3.90 
.         ADD        FORM82 TO RUNPASS       TDMC PORTION
.         ADD        FORM72 TO RUNLR         LR INC PORTION
.         ADD        FORM72 TO FORM82        TOTAL RUNNING CHARGE
...
         MOVE       C0 TO CMPT92
.begin patch 3.0
.         MOVE       QTYSHP TO FORM82
         MOVE       QTYbild TO FORM82
.end patch 3.0
         MOVE       FORM82 TO CMPT92    
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
.END PATCH #2.8 - INCREASED VARS
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
.DLH 02Jun98 begin
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
.DLH 02Jun98 - end
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
         GOTO      LOADOK IF NOT EQUAL
         MOVE      C1 TO SALESBR
         MOVE      C2 TO OELCODE
         GOTO      PROCESS
         ENDIF
.
.                                       2   5  3  4  4  6    ??  1  1
.   LOAD      SALESNUM FROM SALESBR OF LISA,BO,SA,EM,NP,INES,??,JC,TM
.                   1  5  7  3  7   ?   2   ?   7  6  5  3
.                   GH,JP,JE,MG,SMM,???,BM,???,BT,MD,LM,LT
.
...............................................................................
.
.LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM1,TEAM1,TEAM2:
.                   TEAM2,TEAM3,TEAM2,TEAM1:
.                   TEAM1,TEAM1,TEAM1,TEAM2:
.                   TEAM1,TEAM2,TEAM2,TEAM1:
.                   TEAM2,TEAM2,TEAM3,TEAM1,TEAM1,TEAM1
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM3,TEAM1,TEAM2,team1:                 .1-5
                   TEAM3,TEAM2,TEAM1,TEAM1,TEAM1:			       .6-10	
                   TEAM1,TEAM2,TEAM1,TEAM2,TEAM2:			       .11-15	
                   TEAM1,TEAM2,TEAM2,TEAM3,TEAM1:			       .16-20	
                   TEAM1,TEAM1,team1,team1,team1:			       .21-25	
.begin patch 3.75
.                   TEAM1,TEAM1,team1,team1,team3:			       .26-30	
                   TEAM1,TEAM3,team3,team1,team3:			       .26-30	
.end patch 3.75
                   TEAM1,TEAM1,team1,team1,team1			       .31-35	
         MOVE      SALESNUM TO SALESBR
...............................................................................
.
PROCESS  
.START PATCH #2.8 - REPLACED VARS
.         MOVE      C0 TO FORM7
.         MOVE      QTYSHP TO FORM7
         MOVE      C0 TO FORM9A
.begin patch 3.0
.         MOVE       QTYSHP TO FORM9a
         MOVE       QTYbild TO FORM9a
.end patch 3.0
.END PATCH #2.8 - REPLACED VARS
.
.START PATCH #2.8 - INCREASED VARS
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
.begin patch 3.0
.         MOVE      PPM TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
.END PATCH #2.8 - INCREASED VARS
          move      ppm to form32
.end patch 3.0
.
.
         CALL      READBILL
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
.DLH 02June98 begin
         move      c2 to tdmcflag     .force compute to calc tdmc goodies
.DLH 02June98 end
............................................................................         
.begin patch  3.0
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
.end patch 3.0
.begin patch 3.4
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               
.end patch 3.4

         CALL      COMPUTE
.
MASKIT   
.START PATCH #2.8 - INCREASED VARS
.         MOVE      MASK72 TO M$GROSS
.         EDIT      GROSS TO M$GROSS
..
.         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
..
.         MOVE      MASK72 TO M$AR
.         EDIT      FORMAR TO M$AR
.         ADD       FORMAR TO TOTAR
..
.         MOVE      MASK72 TO M$AP1
.         EDIT      AP TO M$AP1
.         ADD       AP TO TOTAP1
..
.         MOVE      MASK72 TO M$AP2
.         EDIT      FORMAP2 TO M$AP2
.         ADD       FORMAP2 TO TOTAP2
.         COMPARE   C0 TO FORMAP2
.         CALL      ZEROAP2 IF EQUAL
..
.         MOVE      MASK72 TO M$LRINC
.         EDIT      LRINC TO M$LRINC
.         ADD       LRINC TO TOTLR
..
.         MOVE      MASK72 TO M$NINC
.         EDIT      NININC TO M$NINC
.         ADD       NININC TO TOTNIN
..
.         MOVE      MASK42 TO M$STAX
.         EDIT      TAXES TO M$STAX
.         ADD       TAXES TO TOTSTAX
...
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
.begin patch 3.75
	if	(OcompId = "P")	  		
         ADD       PREPAY TO TOTPLARP      .TOTAL PREPAID ORDERS.
	Else
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
	endif	.
.end patch 3.75
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
.begin patch 3.75
	if	(OcompId = "P")	  		
         ADD       FORMAR TO TOTPLAR
	Else
         ADD       FORMAR TO TOTAR
         	endif
.end patch 3.75
         move      c0 to form102w
         add       formar to form102w
.
         MOVE      MASK92 TO M$AP1
         EDIT      AP TO M$AP1
.begin patch 3.75
	if	(OcompId = "P")	  		
         	ADD       AP TO TOTPLAP1
         	else
         	ADD       AP TO TOTAP1
         	endif
.end patch 3.75
.
         MOVE      MASK92 TO M$AP2
         EDIT      FORMAP2 TO M$AP2
         ADD       FORMAP2 TO TOTAP2
         COMPARE   C0 TO FORMAP2
         CALL      ZEROAP2 IF EQUAL
.
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
.begin patch 3.76
	if	(OcompId = "P")	  		
            ADD       LRINC TO TOTPLLR
	else
         	ADD       LRINC TO TOTLR
         	endif
.end patch 3.76
.
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
.begin patch 3.76
	if	(OcompId = "P")	  		
         ADD       NININC TO TOTPLNIN
	else
         ADD       NININC TO TOTNIN
         	endif
.end patch 3.76
.
         MOVE      MASK52 TO M$STAX
         EDIT      TAXES TO M$STAX
.begin patch 3.76
	if	(OcompId = "P")	  		
         ADD       TAXES TO TOTPLSTAX
	Else
         ADD       TAXES TO TOTSTAX
         	endif
.end patch 3.76
.END PATCH #2.8 - INCREASED VARS
.
         MOVE      C0 TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
.begin patch 3.76
	if	(OcompId = "P")	  		
         ADD       POST TO TOTPLPOST
         else
         ADD       POST TO TOTPOST
         
         	endif
.end patch 3.76
*****************************************************************************
         MOVE      C0 TO RENTSW
         MOVE      OELCODE TO RENTSW
............................................................................
.  BATCH BILLING SECTION. 25MAR93
.           PACK      MKEY FROM MLRN,COBN
           PACK      MKEY FROM MLRN,z3
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
.          DISPLAY   *P10:16,*EL,*P10:18,*EL 
           CALL      NMLRKEY
           CMATCH    "B" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
           CMATCH    "A" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
         goto      slstype 
bdate
         MOVE      C0 TO N2
.begin patch 3.1.1
.zero check against year is not longer a valid check for no maildate in 2000
.         MOVE      OMDTEY TO N2
         MOVE      OMDTEM TO N2           .we will use the month instead
.end patch 3.1.1
.begin patch  3.1
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
.         MOVE      SYSYR TO N3
        sub        n4 from n5
.         SUB       N2 FROM N3
.         COMPARE   C1 TO N3
.begin patch 3.1.1
.         compare    c1 to n5
.          compare    c0 to n5
          if       (n5 > 0 )                  .from previous year
          move     c1 to batchbr              .so set flag  on          
          goto      slstype
..end patch  3.1
.         IF        EQUAL
.           MOVE      C0 TO N2
.           MOVE      OMDTEM TO N2 
..begin patch 3.1
.         compare     c0 to n2
.         goto        slstype if equal
.end patch 3.1
.         endif
.           MOVE      SYSMO TO N3
..          DISPLAY   *P10:16,"BATCH BILL"
.           SUB       N2 FROM N3
.             COMPARE   "-11" TO N3           .MAILDATE FROM PREVIOUS YEAR?
.             IF        EQUAL
.             MOVE      C1 TO BATCHBR
..            DISPLAY   *P10:18,"MONTH HIT"
.           goto      slstype
.             ENDIF
           ENDIF
          if       (n5 < 0 )                  .from future year
          move     c0 to batchbr              .so set flag off
          goto      slstype
           ENDIF
.end patch 3.1.1
.must be current year, lets check the month
MONTHCK    MOVE      C0 TO N2
           MOVE      OMDTEM TO N2 
           MOVE      SYSMO TO N3
.          DISPLAY   *P10:16,"BATCH BILLER"
           SUB       N2 FROM N3
.begin patch 3.1.1 
           if        (n3 > 0)                   maildate is from a previous month
.             COMPARE   C1 TO N3           .MAILDATE FROM LAST MONTH?
.             IF        EQUAL
bdatedebug1
             MOVE      C1 TO BATCHBR
.            DISPLAY   *P10:18,"MONTH HIT"
             ENDIF
.end patch 3.1.1 
..............................................................................
slstype   MATCH     OLNUM TO Lmanage
          IF        EQUAL
          MOVE      YES TO EXFEFLAG      
          goto      ManEXch               .Management exchange fee
          ENDIF
          MOVE      NO TO EXFEFLAG
          BRANCH    SALESBR OF BROKER,BROKER,MANAGE         
          goto      broker
.
.MANEXCH - LIST MANAGEMENT EXCHANGE FEE.

MANEXCH
         ADD       LRINC TO LREXFEE
         ADD       FORMAR TO AREXFEE
         ADD       AP TO APEXFEE
         ADD       FORMAP2 TO APEXFEE
         GOTO      PRINT

MANAGE
.begin patch 3.7
         move      "M",src
         move      b1,type
         move      OLNUM to cid
         BRANCH   RENTSW OF MRENT,MEXCH,MEXCH
.         call      newbus
.                              if        (newbiz = yes)
.                              add       lrinc to coldlm
.                              move      no to newbiz
.                              endif
.         BRANCH   RENTSW OF MRENT,MEXCH,MEXCH
MRENT
.begin patch 3.71
        move       no to newbiz
.begin patch 3.71
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldlm
                              move      no to newbiz
                              else
.begin patch 3.75
			if	(Ocompid2 = "P")	  		
                              	ADD       LRINC TO LRPLMRINC        *RENT/MANAGEMENT INCOME
                              	else
                              	ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
                              	endif
.end patch 3.75
.begin patch 3.71
                             move       no to newbiz
.begin patch 3.71
                              endif

.         ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
.end patch 3.7
.begin patch 2.9
.begin patch 3.75
	if	(Ocompid2 = "P")	  		
         	ADD       NININC TO NINPLMRINC        *RENT/MANAGEMENT INCOME
         	ADD       FORMAR TO ARPLMR
         	ADD       AP TO APPLMR
         	ADD       FORMAP2 TO APPLMR
	else
         ADD       NININC TO NINMRINC        *RENT/MANAGEMENT INCOME
.end patch 2.9
         ADD       FORMAR TO ARMR
         ADD       AP TO APMR
         ADD       FORMAP2 TO APMR
         endif
.end patch 3.75
         GOTO      PRINT
MEXCH    
.begin patch 3.75
	if	(Ocompid2 = "P")	  		
         	ADD       LRINC TO LRPLMEINC        *EXCH/MANAGEMENT INCOME
         	ADD       NININC TO NINPLMEINC        *EXCH/MANAGEMENT INCOME
         	ADD       FORMAR TO ARPLME
         	ADD       AP TO APPLME
         	ADD       FORMAP2 TO APPLME
	else
         ADD       LRINC TO LRMEINC        *EXCH/MANAGEMENT INCOME
.begin patch 2.9
         ADD       NININC TO NINMEINC        *EXCH/MANAGEMENT INCOME
.end patch 2.9
         ADD       FORMAR TO ARME
         ADD       AP TO APME
         ADD       FORMAP2 TO APME
         endif
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
                              pack      cid,"00",mlrn
.begin patch 3.71
        move       no to newbiz
.begin patch 3.71
         call      newbus
	if        	(newbiz = yes)
.begin patch 3.75
		if	(OcompId = "P")	  		
                              	add       lrinc to coldPLbr
                              	Else
                              	add       lrinc to coldbr
                              	endif
.end patch 3.75
.begin patch 3.71
                            move       no to newbiz
.begin patch 3.71
                              else
.begin patch 3.75
			if	(OcompId = "P")	  		
            	                  	add       lrinc to lrPLbrinc
            	                  	else
            	                  	add       lrinc to lrbrinc
            	                  	endif
.end patch 3.75
                              move      no to newbiz
                              endif
.         ADD       LRINC TO LRBRINC        *RENTAL/BROKERAGE INCOME
.end patch 3.7
.begin patch 2.9
.begin patch 3.75
	if	(OcompId = "P")	  		
         		ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
         		ADD       FORMAR TO ARPLBR
         		ADD       AP TO APPLBR
         		ADD       FORMAP2 TO APPLBR
         		Else
         		ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
.end patch 2.9
         		ADD       FORMAR TO ARBR
         		ADD       AP TO APBR
         		ADD       FORMAP2 TO APBR
		endif
.end patch 3.75
         GOTO      PRINT
BEXCH
.begin patch 3.7
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
.begin patch 3.71
        move       no to newbiz
.begin patch 3.71
         	call      newbus
	if        (newbiz = yes)
.begin patch 3.75
		if	(OcompId = "P")	  		
                              	add       lrinc to coldPLbe
                              	else
                              	add       lrinc to coldbe
                              	endif
.end patch 3.75
.begin patch 3.71
                             move       no to newbiz
.begin patch 3.71
		else
.begin patch 3.75
			if	(OcompId = "P")	  		
                              	add        lrinc to lrPLbeinc
                              	else
                              	add        lrinc to lrbeinc
                              	endif
.end patch 3.75
                              move      no to newbiz
                              endif
.         ADD       LRINC TO LRBEINC        *EXCH/BROKERAGE INCOME
.end patch 3.7
.begin patch 2.9
.begin patch 3.75
	if	(OcompId = "P")	  		
         	ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
         	ADD       FORMAR TO ARPLBE
         	ADD       AP TO APPLBE
         	ADD       FORMAP2 TO APPLBE
         	else
         	ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
.end patch 2.9
         	ADD       FORMAR TO ARBE
         	ADD       AP TO APBE
         	ADD       FORMAP2 TO APBE
         	endif
.end patch 3.75
         	GOTO      PRINT
BRENTB
         move      "B",src
         move      "R",type
                              pack      cid,"00",mlrn
         call      newbus
                              if        (newbiz = yes)
.begin patch 3.75
			if	(OcompId = "P")	  		
                              	add       lrinc to coldPLbr
                              	else
                              	add       lrinc to coldbr
                              	endif
                              else
			if	(OcompId = "P")	  		
                              	add       lrinc to lrPLbbr
                              	else
                              	add       lrinc to lrbbr
                              	endif
                              move      no to newbiz
.end patch 3.75
                              endif
.         ADD       LRINC TO LRBBR        *RENTAL/BROKERAGE INCOME
.begin patch 2.9
.begin patch 3.75
	if	(OcompId = "P")	  		
         	ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
         	ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
         	ADD       FORMAR TO ARPLBBR
         	ADD       AP TO APPLBBR
         	ADD       FORMAP2 TO APPLBBR
         	else
.end patch 2.9
         	ADD       FORMAR TO ARBBR
         	ADD       AP TO APBBR
         	ADD       FORMAP2 TO APBBR
	endif
.end patch 3.75
         GOTO      PRINT
BEXCHB
.begin patch 3.7
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
         call      newbus
                              if        (newbiz = yes)
.begin patch 3.75
			if	(OcompId = "P")	  		
                              	add       lrinc to coldPLbE
                              	else
                              	add       lrinc to coldbE
                              	endif
                              	
                              else
.begin patch 3.75
			if	(OcompId = "P")	  		
                              	add       lrinc to lrPLbbe
                              	else
                              	add       lrinc to lrbbe
                              	endif
.end patch 3.75
                              move      no to newbiz
                              endif
.         ADD       LRINC TO LRBBE        *EXCH/BROKERAGE INCOME
.end patch 3.7
.begin patch 2.9
.begin patch 3.75
	if	(OcompId = "P")	  		
         	ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
         	ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
         	ADD       FORMAR TO ARPLBBE
         	ADD       AP TO APPLBBE
         	ADD       FORMAP2 TO APPLBBE
         	else
.end patch 2.9
         	ADD       FORMAR TO ARBBE
         	ADD       AP TO APBBE
         	ADD       FORMAP2 TO APBBE
         	endif
.end patch 3.75
         GOTO      PRINT
.
ZEROAP2  CLEAR     M$AP2
         RETURN
.
.READBIL
READBILL
.
.         PACK      NBILFLD FROM MLRN,COBN,BILLTN
.         CALL      NBILKEY
.         CALL      READMLR IF OVER
.         RETURN
.READMLR 
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
.begin patch 3.7
NEWBUS
                match     "M",src
                goto       projsav if equal
       PACK      MKEY FROM mlrn,"000"
.turned of 3/15 JD  new client contract date, belv is causing previous months totals zero.
       call        NMLRKEY
       move       cc to ccrep
       move       mm to mmrep
       move       yy to yyrep
      unpack    COMPCNTDATE INTO cc,yy,mm,dd
       move      cid to cidr
       move      compnum to cid
       move      type,typer
       move      src,srcr
       move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           usage in last year
         if LESS
                              move       yes to newbiz
                              return
                              endif
                              endif
         move      ccrep to cc
         move      mmrep to mm
         move      yyrep to yy
         goto     projread
.
projsav
          move    type,typer
                   move    src,srcr
                   move    cid,cidr
.Patch 3.71
projread
.Patch 3.76
          pack    nPrjfld with TYPE,SRC,cid,"200701"
.          pack    nPrjfld with TYPE,SRC,cid,"200601"
.Patch 3.76
                 call    nprjkey
                 if      (projlr > 0)                                                 
                 return
                 else  
                 pack    nrevfld,typer,srcr,cidr,"2005"
                 call    nrevkey
                 call    swaptype if over
                                add JANLR,OLDTOT
                                add FEBLR,OLDTOT
                                add MARLR,OLDTOT
                                add APRLR,OLDTOT
                                add MAYLR,OLDTOT
                                add JUNLR,OLDTOT
                                add JULLR,OLDTOT
                                add AUGLR,OLDTOT
                                add SEPLR,OLDTOT
                                add OCTLR,OLDTOT
                                add NOVLR,OLDTOT
                                add DECLR,OLDTOT
                           if (OLDTOT = C0) 
                 move        yes to newbiz
                 move   c0,oldtot
                 return
                 endif
                 endif
                 move   c0,oldtot
      return                                     
.Patch 3.71
.projread
.          pack    nPrjfld with TYPE,SRC,cid,"200601"
.                 call    nprjkey
.                 if      (projlr > 0)                                                 
.                 return
.                 else  
.                 pack    nrevfld,typer,srcr,cidr,"2006"
..                 call    nrevkey
.                       add JANLR,OLDTOT
.                                add FEBLR,OLDTOT
.                                add MARLR,OLDTOT
.                                add APRLR,OLDTOT
.                                add MAYLR,OLDTOT
.                                add JUNLR,OLDTOT
.                                add JULLR,OLDTOT
.                                add AUGLR,OLDTOT
.                                add SEPLR,OLDTOT
.                                add OCTLR,OLDTOT
.                                add NOVLR,OLDTOT
.                                add DECLR,OLDTOT
.                              if (OLDTOT <> C0) 
.                 move        yes to newbiz
.                                                              move   c0,oldtot
.                 return
.                 endif
.                 endif
.      return                                     
.                unpack    cid,str2,str4
.       PACK      MKEY FROM mlrn,"000"
.       call   NMLRKEY
.       unpack   COMPCNTDATE INTO cc,yy,mm,dd
.              move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                             move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           usage in last year
.         if LESS
.                             move       yes to newbiz
.                             move     nmmrep to nmm
.                             move     yyrep to yy
.                             move     ccrep to cc
.                             return
.                             endif
.                             endif
.                             move     nmmrep to nmm
.                             move     yyrep to yy
.                             move     ccrep to cc
.                             return
.                             endif
.end patch 3.7
*......................................................................
.
PRINT    
.>Patch 3.38         Comment Out
.             COMPARE   "56" TO LINES
.>Patch 3.38         Comment Out             
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
.         CALL      HEADER IF EQUAL
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
.>Patch 3.38 Comment Out         
.         PRINT     HPBON,*2,HPUNON,MLRN,HPUNOFF,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
.                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
.                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
.                   *117,M$CTAX,*127,M$POST:
.                   *L,*1,COBN,"-",BILLTN:
.                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
.                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPBOFF
.>Patch 3.38 Comment Out
.>Patch 3.38 Code Added
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
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff; 
                       add     eightlpi,row        
                       add     eightlpi,row                                               
.>Patch 3.38 Code Added
                   
                   
                       ELSE
.>Patch 3.38 Comment Out         
.         PRINT     HPBON,*2,MLRN,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
.                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
.                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
.                   *117,M$CTAX,*127,M$POST:
.                   *L,*1,COBN,"-",BILLTN:
.                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
.                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPBOFF
.>Patch 3.38 Comment Out
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
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff;   
                       add     eightlpi,row        
                       add     eightlpi,row                                               
.>Patch 3.38 Code Added                   
                       ENDIF
         ELSE
                       MATCH     YES TO EXFEFLAG
                       IF        EQUAL
.>Patch 3.38 Comment Out         
.         PRINT     *2,HPuNoN,MLRN,HPUNOFF,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
.                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
.                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
.                   *117,M$CTAX,*127,M$POST:
.                   *L,*1,COBN,"-",BILLTN:
.                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
.                   *26,BILCOMP,*64,M$AP2," ",PMASK
.>Patch 3.38 Comment Out
.>Patch 3.38 Code Added
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
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;  
                       add     eightlpi,row        
                       add     eightlpi,row                                               
.>Patch 3.38 Code Added                   
                       ELSE
.>Patch 3.38 Comment Out         
.         PRINT     *2,MLRN,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
.                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
.                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
.                   *117,M$CTAX,*127,M$POST:
.                   *L,*1,COBN,"-",BILLTN:
.                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
.                   *26,BILCOMP,*64,M$AP2," ",PMASK
.>Patch 3.38 Comment Out
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
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;    
                       add     eightlpi,row        
                       add     eightlpi,row                                               
.>Patch 3.38 Code Added   
                       ENDIF
         ENDIF
         ADD       c2 TO LINES
.02Jun98 DLH begin
         if        (tdmchrg = "Y" | runrar > 0)
.         Cmatch    yes to TDMChrg
.         if         equal
         add        c2 to lines
.
         if        (runchrg > 0)
         add       runchrg to runrar             .add exchange charges.
         endif
.                                                .prog 19 charges
         move      runrar to runrlr
         sub       tdmcamt from runrlr
.not nec ?? DLH
.         add       runchrg to runrpass           
.         move      c0 to form92
.         move      runchrg to form92
.         sub       tdmcamt,form92
.         add       form92 to runrlr
..................
.         
         move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         sub       runrpass,tdmcflat           .what part of tdmc charge is flat charges
         if        (tdmcflat < 0 )   
         move      c0 to tdmcflat
         endif
         
.         move      c0 to IncRflat               
.         move      RunRFlat to incRflat           .income on flat charges
.         sub       tdmcflat,IncRFlat
.         
         calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         calc      prcpass=runrpass/(runrpass+tdmcflat)*100
.         calc      prcLRflat=INCrflat/(incrflat+runrlr)*100
         calc      prcLRPASS=(RUNRLR/runrar)*100
.         move      c0 to LRPlus
.         calc      LRPlus=runrar-tdmcamt-runrlr-incrflat
         
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
.>Patch 3.38 Comment Out         
.         print     hpunon,hpbon,*2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
.                   prcpass,"%":
.                   *89,mt$lrinc,B1,PRCLRPASS,"%":
.                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
.                   *91,INCrFLAT,B2,PRCLRFLAT,"%",hpboff,hpunoff;
.>Patch 3.38 Comment Out            
.>Patch 3.38 Code Added 
                      sub     eightlpi,row  
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,*ll,*ulon,*boldon,"Triplex Billed : ",tdmcamt,"  We Billed";
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
.>Patch 3.38 Code Added                              
         else          
.>Patch 3.38 Comment Out                     
.         print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
.                   prcpass,"%":
.                   *89,mt$lrinc,B1,PRCLRPASS,"%":
.                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
.                   *91,INCrFLAT,B2,PRCLRFLAT,"%";
.>Patch 3.38 Code Added 
                      sub     eightlpi,row  
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,*ll,"Triplex Billed : ",tdmcamt,"  We Billed";
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
.>Patch 3.38 Code Added 
         endif
         clear     tdmchrg      
         add        runrar to grunrar
         add        runrpass to grunrpass
         add        runrflat to grunrflat    
         add        runrlr to grunrlr
         endif
.02Jun98 DLH end
...........................................................................         
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal
        CALL      HEADER IF not less         
.>Patch 3.38      End Code Addtion


........................................
         cmatch    yes to ppsw
         if        equal                                           1
         move      lrn to nmoafld5                                 1
         rep       zfill in nmoafld5                               1
         move      c5 to  nmoapath                                 1   
         call      nmoakey                                         1
         if        over                                     2 
.>Patch 3.38         Comment Out                
.        COMPARE   "56" TO LINES
.>Patch 3.38         Comment Out        
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
        CALL      HEADER IF equal
        CALL      HEADER IF not less
.>Patch 3.38 Comment Out                    
.         print     hpbon,*2,"No MOA entry found for this invoice!!!!!!",hpboff,*l
.>Patch 3.38 Comment Out            
.>Patch 3.38 Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
        add     eightlpi,row  
.>Patch 3.38 Code Added           
.begin  release 3.2
         move      yes to moabadflag
.end  release 3.2
         add       c2 to lines                                     2
         goto      moaexit                                         2  
                endif                                              2
moaloop  compare   "18" to reason                                  1 
                if        equal                                    2
.>Patch 3.38         Comment Out
.        COMPARE   "56" TO LINES                                    2
.>Patch 3.38         Comment Out        
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
        CALL      HEADER IF equal                                  2
        CALL      HEADER IF not less                               2
.>Patch 3.38 Comment Out                    
.         print     *2,"MOA Prepayment ",*52,onamount,*l            2
.>Patch 3.38 Comment Out            
.>Patch 3.38 Code Added          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"MOA Prepayment ";
.slightly off alignment with column4r        
              prtpage prfile;*p3900:row,*ALIGNMENT=*Right,*ll,onamount;    
        add     eightlpi,row  
        add     eightlpi,row         
.>Patch 3.38 Code Added          
	IF	(Ocompid = "P" )       .may want to change to MOAcomp
         add       onamount to totPLpmoa       .total prepay moa     2
	Else
         add       onamount to totpmoa       .total prepay moa     2
	endif
         add       c2 to lines                                     2
         goto      moaexit                                         2
                endif                                              1
         call      nmoaks                                          2
                if        over                                     2
.>Patch 3.38         Comment Out                
.         COMPARE   "56" TO LINES                                   2
.>Patch 3.38         Comment Out         
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
         CALL      HEADER IF equal                                 2
         CALL      HEADER IF not less                              2
.>Patch 3.38 Comment Out                     
.         print     hpbon,*2,"No MOA Prepayment entry found for this invoice!!!!!!",hpboff,*l
.>Patch 3.38 Comment Out            
.>Patch 3.38 Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
        add     eightlpi,row  
        add     eightlpi,row         
.>Patch 3.38 Code Added           
.begin  release 3.2
         move      yes to moabadflag
.end  release 3.2
         add       c2 to lines                                     2
         goto      moaexit                                         2
                else                                                 
         goto      moaloop                                         2a 
                endif                                              2a
         endif                                                     1
.>Patch 3.38 Comment Out                     
.         print     b1
.>Patch 3.38 Comment Out                     
         add       c1 to lines
moaexit  CLEAR     PMASK
.begin patch 3.72
              call            debug
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
.START PATCH #2.8 - REPLACED VARS
.         MOVE      C0 TO FORM8
.         ADD       FORM102 TO FORM8    *A/R
         MOVE      C0 TO FORM9B
         ADD       FORM102 TO FORM9B    *A/R
.END PATCH #2.8 - REPLACED VARS
.
         MOVE      C0 TO FORM102
         MOVE      AP TO FORM102
         MULT      HUND BY FORM102
.START PATCH #2.8 - REPLACED VARS
.         MOVE      C0 TO FORM7
.         ADD       FORM102 TO FORM7
         MOVE      C0 TO FORM9A
         ADD       FORM102 TO FORM9A
.END PATCH #2.8 - REPLACED VARS
.
         MOVE      C0 TO FORM102
         MOVE      FORMAP2 TO FORM102
         MULT      HUND INTO FORM102
         MOVE      C0 TO FORM9
         ADD       FORM102 TO FORM9
         MOVE      NO TO APSW
.START PATCH #2.8 - REPLACED VARS
.         COMPARE   C0 TO FORM7
         COMPARE   C0 TO FORM9A
.END PATCH #2.8 - REPLACED VARS
.         IF        NOT EQUAL
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
         COMPARE   C0 TO FORM9
.         IF        NOT EQUAL
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
.START PATCH #2.8 - REPLACED VARS
.         MOVE      FORM7 TO DIM7
.         SCAN      MINUS IN DIM7
.         IF        EQUAL
.         RESET     DIM7
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,C0,C0,DIM7
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 4
.         CLEAR     DIM7
.         APPEND    AMOUNT1 TO DIM7
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
.END PATCH #2.8 - REPLACED VARS
         ENDIF
.
.         cmatch    yes to apsw
.         goto      input if not equal
.
.START PATCH #2.8 - REPLACED VARS
.         MOVE      FORM8 TO DIM8
.         SCAN      MINUS IN DIM8
.         IF        EQUAL
.         RESET     DIM8
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,C0,DIM8
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 3
.         CLEAR     DIM8
.         APPEND    AMOUNT1 TO DIM8
.         RESET     DIM8
.         ENDIF
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
.END PATCH #2.8 - REPLACED VARS
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
.START PATCH #2.8 - REPLACED VARS
.         RESET     DIM7
.         RESET     DIM8
         RESET     DIM9A
         RESET     DIM9B
.END PATCH #2.8 - REPLACED VARS
         RESET     DIM9
.START PATCH #2.8 - REPLACED VARS
.         REP       ZFILL IN DIM7     .A/P1
.         REP       ZFILL IN DIM8     .A/R
         REP       ZFILL IN DIM9A     .A/P1
         REP       ZFILL IN DIM9B     .A/R
.END PATCH #2.8 - REPLACED VARS
         REP       ZFILL IN DIM9     .A/P2
OWNPREP  MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF
.Start patch #2.8 - remmed logic and replaced with vars in follwing write
.         clear     str6
.         pack      str6 from omdtem,omdted,omdtey
.End patch #2.8 - remmed logic and replaced with vars in follwing write
         clear     str7
         pack      str7 from invdtem,invdted,invdtey
.begin patch 3.0
.         clear     str8
.         pack      str8 from chkdtem,chkdted,chkdtey
.end patch 3.0
         compare   c1 to count
         if        equal
         write     clninv,seq;*cdfon,"Mailer ##","Lr##","Inv##","Prepay","Owner","Brk Guar":
                   "cnt","List","Ord Date","A/P1"," ","adj cde","Inv Date","A/P2","List":
                   "SLSCode","A/R","CHK Date"," ","TDMC A/R","TDMC"
         endif           
.Start patch #2.8 - remmed logic and replaced with vars in follwing write
.          WRITE     clninv,SEQ;*cdfon,MLRN:
.                   LRN:
.                   invnum:
.                   ppflag:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   str6:
.                   *ZF,DIM7:
.                   WSJPC:
.                   ADJC:
.                   str7:
.                   *ZF,DIM9:
.                   O1DES:
.                   slscode:
.                   GUARPAY:
.                   DIM8:
.                   str8:                        -100
.                   SLSCNAME:                     .   -105
.                   tdmcamt:                      triplex billings to us
.                   runrar                        .We billed for tdmc charges
.          WRITE     clninv,SEQ;*cdfon,MLRN:
.                   LRN:
.                   invnum:
.                   ppflag:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   omdtem:
.                   omdted:
.                   omdtec:
.                   omdtey:
.                   *ZF,DIM9A:
.                   WSJPC:
.                   ADJC:
.                   str7:
.                   *ZF,DIM9:
.                   O1DES:
.                   slscode:
.                   GUARPAY:
.                   DIM9B:
.                   str8:                        -100
.                   SLSCNAME:                     .   -105
.                   tdmcamt:                      triplex billings to us
.                   runrar                        .We billed for tdmc charges
..End patch #2.8 - remmed logic and replaced with vars in follwing write
.begin patch 3.0
          WRITE     clninv,SEQ;*cdfon,MLRN:
                   LRN:
                   invnum:
                   ppflag:
                   OLON:
                   OBRKGUAR:
                   COBN:
                   OLNUM:
                   omdtem:
                   omdted:
                   omdtec:
                   omdtey:
                   *ZF,ap1:
                   WSJPC:
                   ADJC:
                   str7:
                   *ZF,ap2:
                   O1DES:
                   slscode:
                   GUARPAY:
                   form102w:
                   chk1dtem:
                   chk1dted:
                   chk1dtec:
                   chk1dtey:                        -100
                   SLSCNAME:                     .   -105
                   tdmcamt:                      triplex billings to us
                   runrar:
                   nininc:
                   lrinc                        .We billed for tdmc charges
.end patch 3.0
         move       c0 to runrar
         move       c0 to runrlr
         move       c0 to runrpass
         move       c0 to runrflat
         cmatch    yes to apsw
         goto      input if not equal

.Start patch #2.8 - added var
.          WRITE     SLSFILE,SEQ;MLRN:
.                   LRN:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   OMDTEM,OMDTED,OMDTEY:
.                   *ZF,DIM7:
.                   WSJPC:
.                   ADJC:
.                   INVDTEM,INVDTED,INVDTEY:
.                   *ZF,DIM9:
.                   O1DES:
.                   slscode:
.                   GUARPAY:
.                   DIM8:
.                   chkdtem:                        -100
.                   chkdted:
.                   chkdtey:
.                   SLSCNAME                     .   -105
.          WRITE     SLSFILE,SEQ;MLRN:
.                   LRN:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   OMDTEC,OMDTEY,OMDTEM,OMDTED:
.                   *ZF,DIM9A:
.                   WSJPC:
.                   ADJC:
.                   CC,INVDTEY,INVDTEM,INVDTED:
.                   *ZF,DIM9:
.                   O1DES:
.                   slscode:
.                   GUARPAY:
.                   DIM9B:
.                   chkdtem:                        -100
.                   chkdted:
.                   chkdtey:
.                   SLSCNAME                     .   -105
.End patch #2.8 - added var
.begin patch 3.0
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
.end patch 3.0
         GOTO      INPUT
*......................................................................
.
TOTAL    
.         COMPARE   "59" TO LINES
.>Patch 3.38         Comment Out         
.         COMPARE   "40" TO LINES
.>Patch 3.38         Comment Out                  
.>Patch 3.38      Code Added  
              COMPARE  "6000" to ROW
.>Patch 3.38      End Code Addtion         
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
.>Patch 3.38         Comment Out                 
.         PRINT     *1,"## INVOICES ",COUNT:
.                   *26,"*** TOTALS",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC:
.                   *115,MT$CTAX:
.                   *L,*62,MT$AP2,*77,MT$NINC,*103,MT$STAX,*124,MT$POST:
.                   *L,*26,"Code 96 ",*48,MT$ARP
.>Patch 3.38         Comment Out
.>Patch 3.38      Code Added  
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
.>Patch 3.38      End Code Addition
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
.>Patch 3.38         Comment Out           
.         print     *26,"total Moa debit",*48,MT$pmoa
.>Patch 3.38         Comment Out        
.>Patch 3.38      Code Added  
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Total MOA Debit",*boldoff;      
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$PMOA,*boldoff;                             
        add     eightlpi,row                 
.>Patch 3.38      End Code Addition          
.begin release 3.2
         cmatch    yes to moabadflag
         if        equal
.>Patch 3.38         Comment Out                 
.                      print     hpbon,*2,"MOA Prepayment entries are MISSING!!!!!!",hpboff,*l
.>Patch 3.38         Comment Out        
.>Patch 3.38      Code Added  
                      add     eightlpi,row                                 
                      add     eightlpi,row   
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"****MOA Prepayment entries are MISSING!!!!!!****",*boldoff;   
                      add     eightlpi,row                                 
                      add     eightlpi,row                                                        
.>Patch 3.38      End Code Addition                   
         endif
.end release 3.2

         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP TO MT$AP1
.>Patch 3.38      Comment Out                 
.         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,MT$AP1
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL ACCOUNTS PAYABLE",*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;               
        add     eightlpi,row                 
.>Patch 3.38      Code Added  .
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBRINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBR TO MT$AR
         EDIT      APBR TO MT$AP1
.>Patch 3.38      Comment Out           
.         PRINT     *L,*L,*5,"TOTAL BROKERAGE/RENTAL     ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/RENTAL",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added                 
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBEINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBE TO MT$AR
         EDIT      APBE TO MT$AP1
.>Patch 3.38      Comment Out                 
.         PRINT     *5,"TOTAL BROKERAGE/EXCHANGE   ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/EXCHANGE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added                   
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LRMRINC TO LRMINC
         ADD       LRMEINC TO LRMINC
         EDIT      LRMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       ARMR TO ARM
         ADD       ARME TO ARM
         ADD       APMR TO APM
         ADD       APME TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
.>Patch 3.38      Comment Out                 
.         PRINT     *5,"TOTAL MANAGEMENT    ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.begin patch 3.7
              Move            Mask92 to Mt$lrinc
              edit            Coldbr to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Rent",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            Coldbe to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Exch",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdLm to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New List management",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New LR Income excluded in TOTALS",*boldoff;                    
        add     eightlpi,row  
.end patch 3.7
.>Patch 3.38      Code Added                    
.
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LREXFEE TO LRMINC
         EDIT      LRMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       AREXFEE TO ARM
         ADD       APEXFEE TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
.>Patch 3.38      Comment Out          
.         PRINT     *5,"TOTAL MANAGEMENT EXCH FEE   ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT EXCH FEE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added                    
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBR TO MT$AR
         EDIT      APBBR TO MT$AP1
.>Patch 3.38      Comment Out                 
.         PRINT     *L,*L,*5,"TOTAL BROKERAGE/RENT BATCH Prev Months",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/RENT BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added                     
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBE TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBE TO MT$AR
         EDIT      APBBE TO MT$AP1
.>Patch 3.38      Comment Out           
.         PRINT     *5,"TOTAL BROKERAGE/EXCH BATCH Prev Months",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/EXCH BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
.>Patch 3.38      Code Added                     
.begin patch 2.9
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBRINC TO MT$LRINC
.>Patch 3.38      Comment Out          
.         PRINT     *L,*L,*5,"TOTAL BROKER/RENT ACD  ",*89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/RENT ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added            
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBEINC TO MT$LRINC
.>Patch 3.38      Comment Out                 
.         PRINT     *5,"TOTAL BROKER/EXCH ACD  ",*89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/EXCH ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code Added           
.
         MOVE      MASK92 TO MT$LRINC
         add       ninmeinc to ninmrinc
         EDIT      NINMRINC TO MT$LRINC
.>Patch 3.38      Comment Out                 
.         PRINT     *5,"TOTAL MANAGEMENT ACD   ",*89,MT$LRINC
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL MANAGEMENT ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
        add     eightlpi,row         
.>Patch 3.38      Code Added          
.
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      NINMEINC TO MT$LRINC
.         PRINT     *5,"TOTAL MANAGE/EXCH ACD  ",*89,MT$LRINC
.
.end patch 2.9
.
..............................................................................
.TRIPLEX  EXCHANGES
         MOVE      MASK92 TO MT$LRINC
         EDIT      RUNLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      RUNAR TO MT$AR
         EDIT      RUNPASS TO MT$AP1
         EDIT      RUNFLAT TO MT$AP2
.>Patch 3.38      Comment Out                 
.         PRINT     *5,"TRIPLEX BREAKOUT IS FOR TEST PURPOSES ONLY!!!!!!!!":
.                   *L,*5,"--------------------------------------------------":
.                   *L
.         PRINT     *5,"Running charge Ivoices","##",RUNCOUNT,*48,MT$AR:
.                   *62,MT$AP1:
.                   *89,MT$LRINC:
.                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Triplex Breakout",*ULOFF,*boldoff;                    
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
.>Patch 3.38      Code Added                     
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
.>Patch 3.38      Comment Out         
.         PRINT     *5,"Rental TDMC & Prog 19","##",RUNRCNT,*48,MT$AR:
.                   *62,MT$AP1:
.                   *89,MT$LRINC:
.                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
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
.>Patch 3.38      Code Added                                        
.
         MOVE      MASK92 TO MT$TDMC
         EDIT      TOTTDMC TO MT$TDMC
.>Patch 3.38      Comment Out                 
.         PRINT     *l,*5,hpbon,"TRIPLEX Billed us  Charges","##",*48,MT$TDMC,hpboff
.>Patch 3.38      Comment Out        
.>Patch 3.38      Code Added           
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"TRIPLEX Billed us  Charges",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$TDMC,*boldoff;                                                           
.>Patch 3.38      Code Added                 

.         MOVE      MASK92 TO MT$LRINC
.         EDIT      CMREDOLR TO MT$LRINC
.         PRINT     *L,*L,HPBON:
.                    *5,"10 %COMMISION RECAPTURE    ":
.                   *89,MT$LRINC,HPBOFF
.
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      LRMEINC TO MT$LRINC
.         MOVE      MASK92 TO MT$AP1
.         MOVE      MASK92 TO MT$AR
.         EDIT      ARME TO MT$AR
.         EDIT      APME TO MT$AP1
.         PRINT     *5,"TOTAL MANAGEMENT/EXCHANGE  ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.
.>Patch 3.38      Comment Out 
.         PRINT     *FLUSH
.>Patch 3.38      Comment Out          
.         COMPARE   C0 TO LRUNKN
.         GOTO      EOJ IF EQUAL
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      LRUNKN TO MT$LRINC
.         MOVE      MASK92 TO MT$AP1
.         MOVE      MASK92 TO MT$AR
.         EDIT      ARUNKN TO MT$AR
.         EDIT      APUNKN TO MT$AP1
.         PRINT     *5,"TOTAL UNKOWN   ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.         PRINT     *FLUSH

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
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBR TO MT$AR
         EDIT      APPLBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/RENTAL",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBEINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBE TO MT$AR
         EDIT      APPLBE TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/EXCHANGE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRPLMINC
         ADD       LRPLMRINC TO LRMINC
         ADD       LRPLMEINC TO LRMINC
         EDIT      LRPLMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       ARPLMR TO ARM
         ADD       ARPLME TO ARM
         ADD       APPLMR TO APM
         ADD       APPLME TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT",*boldoff;                    
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
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New LR Income excluded in TOTALS",*boldoff;                    
        add     eightlpi,row  
.end patch 3.7
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRPLMINC
         ADD       LRPLEXFEE TO LRMINC
         EDIT      LRPLMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       ARPLEXFEE TO ARM
         ADD       APPLEXFEE TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT EXCH FEE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBBR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBBR TO MT$AR
         EDIT      APPLBBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/RENT BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRPLBBE TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARPLBBE TO MT$AR
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
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.>Patch 3.38      Code
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINPLBEINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/EXCH ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         add       ninPLmeinc to ninPLmrinc
         EDIT      NINPLMRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL MANAGEMENT ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
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
.>Patch 3.38         Comment Out           
.         if         equal
.         print      hp17ptch,hpdupl,*f          .compressed, duplex
..         print      hp17ptch,hpdupl,*f,*f          .compressed, duplex
.         endif
.         PRINT     *f,*31,"***  N I N   M O N T H L Y   ":
.                   *60,"S A L E S   R E G I S T E R  ***":
.                   *116,"DATE ",DATEMASK:
.                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
.                   *L,*L,*1,"MAILER",*11,"LR":
.                   *17,"INVOICE":
.                   *26,"MAILER BILL-TO":
.                   *52,"--------ACCOUNTS--------":
.                   *79,"-------COMMISSIONS------":
.                   *108,"------TAXES----",*128,"OUR":
.                   *L,*1,"NUMBER",*9,"NUMBER":
.                   *18,"NUMBER",*26,"NAME AND THRU":
.                   *53,"RECEIVABLE":
.                   *69,"PAYABLE",*81,"NIN INCOME":
.                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
.                   *126,"POSTAGE",*L
.         MOVE      c6 TO LINES
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
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
.        prtpage prfile;*font=font8,str10;
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
.             prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"000000";                     
.             prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"111111";                    
.             prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,"222222";                            
.        prtclose prfile
.        stop

         RETURN
.>Patch 3.38         Comment Out
.
*........................
MISSING  
.>Patch 3.38         Comment Out
.        COMPARE   "56" TO LINES
.>Patch 3.38         Comment Out
.>Patch 3.38      Code Added  
              COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion

        CALL      HEADER IF equal
        CALL      HEADER IF not less
.>Patch 3.38      Comment Out 
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"********* MISSING INVOICE ******",HOLDREC,*ULOFF,*boldoff;          
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"********* MISSING INVOICE ******",HOLDREC,*ULOFF,*boldoff;              
        add     eightlpi,row  
.         PRINT     *1,"********* MISSING INVOICE ******  ",HOLDREC;
.         PRINT     *1,"********* MISSING INVOICE ******  ",HOLDREC,*l,*l:
.                   *FLUSH
.>Patch 3.38      Comment Out             
.>Patch 3.38 Code Added
              clear str6
              move holdrec to str6
              append str6,taskname
              append crlf,taskname
.>Patch 3.38 Code Added
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
.Patch 3.71
swaptype
                 IF      (srcr = "M")
                 return
                 endif
.                 
                 match   "R" to typer
                 if      equal
                 pack    nrevfld from "EB","00",mlrn,"2005"
                 else
                 pack    nrevfld from "RB","00",mlrn,"2005"
                 endif
                 call    nrevkey
                 return
.Patch 3.71
.
debugger  
              return
.
EOJ
.>Patch 3.38 Code Added
              prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
              pack            APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
              loop
               call           FindFirstFile
               until (APIResult = 0 | APIResult = hexeight)
               pause          "1"
              repeat
              pause           "2"
              erase           "c:\progra~1\pdf995\flag.dat"                
.>Patch 3.38 Code Added
.>Patch 3.38  Comment Out
.         splclose
.>Patch 3.38  Comment Out         
         WEOF      SLSFILE,SEQ
         CLOSE     SLSFILE,EOFSIZE
         WEOF      clninv,SEQ
         CLOSE     clninv,EOFSIZE
         MOVE      NO to str1
.>Patch 3.38 Code Added
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
.>Patch 3.38 Code Added         
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
.begin patch 3.3
.........................................................................................................................
.Begin patch 3.73
	if	(count = c0)
	endif
	Move	"NEOM0004 - output",MailSubjct
	Move	"DavidHerrick@nincal.com",MailFrom
	Move	"DavidHerrick@nincal.com",MailTo
	Clear	MailBody
	Append	Count,MailBody
	Append	B1,MailBody
	Append	"Invoices",MailBody
	Append	CRLF,MailBOdy
	Append	"LR INCOME TOTALS ",Mailbody
	Append	TotLR,Mailbody
	Append	CRLF,Mailbody
	Append	"NIN INCOME TOTALS ",Mailbody
	Append	TotNIN,Mailbody
	Append	CRLF,Mailbody
	Append	"LR INCOME Rental ",Mailbody
	Append	LRBRINC,Mailbody
	Append	CRLF,Mailbody
	Append	"LR INCOME Exchange ",Mailbody
	Append	LRBEINC,Mailbody
	Append	CRLF,Mailbody
.
              MOVE      C0 TO LRMINC
              ADD       LRMRINC TO LRMINC
              ADD       LRMEINC TO LRMINC
	Append 	"LR INCOME List Management  ",MailBody		
	Append	LRMINC,MailBody
	append	CRLF,MailBody
              MOVE      C0 TO LRMINC
              ADD       LREXFEE TO LRMINC
	Append 	"List Management Exch Fee ",MailBody		
	Append	LRExFee,MailBody
	append	CRLF,MailBody
	Append 	"LR Income Rent (Previous Month) ",MailBody		
	Append	LRBBr,MailBody
	append	CRLF,MailBody
	Append 	"LR Income Exch (Previous Month) ",MailBody		
	Append	LRBBE,MailBody
	append	CRLF,MailBody
.
.begin patch 3.7
.jose here
	Append 	"New Business Brokerage Rent ",MailBody		
	Append	ColdBr,MailBody
	append	CRLF,MailBody
.
	Append 	"New Business Brokerage Exch ",MailBody		
	Append	ColdBe,MailBody
	append	CRLF,MailBody
	Append 	"New Business List Management ",MailBody		
	Append	ColdLM,MailBody
	append	CRLF,MailBody
.begin patch 3.75
.	Reset	MailBody
.end patch 3.7
	Append	"PL Invoices",MailBody
	Append	CRLF,MailBOdy
	Append	"PL LR INCOME TOTALS ",Mailbody
	Append	TotPLLR,Mailbody
	Append	CRLF,Mailbody
	Append	"PL NIN INCOME TOTALS ",Mailbody
	Append	TotPLNIN,Mailbody
	Append	CRLF,Mailbody
	Append	"PL LR INCOME Rental ",Mailbody
	Append	LRPLBRINC,Mailbody
	Append	CRLF,Mailbody
	Append	"PL LR INCOME Exchange ",Mailbody
	Append	LRPLBEINC,Mailbody
	Append	CRLF,Mailbody
.
              MOVE      C0 TO LRMINC
              ADD       LRPLMRINC TO LRPLMINC
              ADD       LRPLMEINC TO LRPLMINC
	Append 	"PL LR INCOME List Management  ",MailBody		
	Append	LRPLMINC,MailBody
	append	CRLF,MailBody
              MOVE      C0 TO LRMINC
              ADD       LRPLEXFEE TO LRMINC
	Append 	"PL List Management Exch Fee ",MailBody		
	Append	LRPLExFee,MailBody
	append	CRLF,MailBody
	Append 	"PL LR Income Rent (Previous Month) ",MailBody		
	Append	LRPLBBr,MailBody
	append	CRLF,MailBody
	Append 	"PL LR Income Exch (Previous Month) ",MailBody		
	Append	LRPLBBE,MailBody
	append	CRLF,MailBody
.
.begin patch 3.7
.jose here
	Append 	"PL New Business Brokerage Rent ",MailBody		
	Append	ColdPLBr,MailBody
	append	CRLF,MailBody
.
	Append 	"PL New Business Brokerage Exch ",MailBody		
	Append	ColdPLBe,MailBody
	append	CRLF,MailBody
	Append 	"PL New Business List Management ",MailBody		
	Append	ColdPLLM,MailBody
	append	CRLF,MailBody

	Reset	MailBody
.end patch 3.75

	Call	SendMail
.End patch 3.73	
.end patch 3.3
.begin patch 3.6
.Open Excel application
OhMy
.Begin patch 3.74
	if	(count = c0)
	goto	JDExit                        .no records processed skip this
	endif
.End patch 3.74
              Create  ex
              pack            Taskname,"\\nins1\d\users\dherric\monday.xls"       
              getprop ex,*Workbooks=books
              Books.Open using *Filename=taskname
              books.item giving book using 1
              getprop book,*Sheets=sheets
.              getprop book,*workSheets=sheets
.Reset Default of Worksheets found in a Workbook         .eight
.should try getting the property here and reseting it when done.
        getprop ex,*SheetsInNewWorkbook=NumberofSheets
.Create Workbooks collection
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
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
.end patrch 3.6
JDexit
         SHUTDOWN  "cls"
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         SHUTDOWN  "cls"
         STOP

.         include   compute.inc
.patch3.37
                              include        compio.inc
                              include        cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.37
         INCLUDE   NORDIO.INC
.begin patch 3.4
.         INCLUDE   NINVIO.inc
.         include   compute.inc
              include         compute.inc
              INCLUDE         ninvio.inc
              Include         NInvAcdio.inc
.end patch 3.4
         include   nownio.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         include   cvt.inc
         INCLUDE   GNXTIO.inc
         include   nmoaio.inc
         include   nmrgio.inc
.patch3.37
.         INCLUDE   NBRKIO.INC
.patch3.37
         include   nslsio.inc
         include   tinvio.inc
         include   nxcgio.inc
.begin patch 3.0
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
.end patch 3.0
        include nrevio.inc
        include nprjio.inc
         INCLUDE    COMLOGIC.inc



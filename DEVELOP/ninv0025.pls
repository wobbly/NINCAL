...............................................................................
.Ninv0025 - detail billing report
...............................................................................
.
PC        EQU       0
          INC       COMMON.inc
          INCLUDE   CONS.inc
          include   consacct.inc
          include   hp.inc
            INC             ninvdd.inc
            Include         NInvAcddd.inc
          include        compdd.inc
          include        cntdd.inc
          INCLUDE   NBILDD.inc
          INCLUDE   NORDDD.INC
          include   nowndd.inc
          INCLUDE   GNXTDD.inc
          INCLUDE   NDAT3DD.inc
              INCLUDE         NSEL2DD.INC
          include winapi.inc
          inc       nmrgdd.inc
          include   nslsdd.inc
          include   nmoadd.inc
          include   nxcgdd.inc
          include   tinvdd.inc
          include   nshpdd.inc
          include   ndatdd.inc
          include   nacddd.inc
              include         nmoddd.inc
           include nprjdd.inc
           include nrevdd.inc
          INclude   Excel.inc

#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
          
.
...........................................
Release  Init      "1.00"     DLH           Smile train request
RelDate  INit      "27 April 2009"
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
DATEPRT2 DIM       10
time          Dim             8
ExcelAdrec          form      2
.FILES.
InPut     FIle
...............................................................................
.
.
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
O2DES2        DIM             40                            .Second half of Select Name, if necessary
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

. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
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
ninBEinc form      9.2      Total NIN ACD Brokerarge Exch inc
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
.sheetno    form      2
.NumberofSheets Integer        4,"0x00000000"
.RowNumber     Dim             9
.books   automation
.book    automation
.sheets  automation
.sheet   automation
.Rowcol  automation
.ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
.VT_BOOL EQU 11          .Boolean
.OTRUE   variant
.OFALSE  variant
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
              call            "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
              "Parameters":
              "ProcessPDF":
              "\\nts1\e\apps\plb\code\pdftest.bat":
              result
              if (result = C0)
.Prepare Flag file
               prep           tempfile,"c:\progra~1\pdf995\flag.dat"
               write          tempfile,SEQ;"flag set"
               close          tempfile
              endif
          If        (PRtName  = "")
          MOve      "SalesReg",PRtname
          endif
         OPEN      DUPEOWN,"\\nts1\e\data\index\DUPEOWN",READ
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
DATE1    MOVE      TODAY TO DATEMASK
         UNPACK    TODAY INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
        CALL      CVTJUL
         MOVE      juldays TO TODAY1
         DISPLAY   *P01:06,"Input File  :NININV ":
                   *P01:07,"Eom Date : "
.
OPEN     TRAP      SHAREINV GIVING ERROR IF IO
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
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
          OPen      INput,"diskin58.dat|10.10.30.103:502",exclusive
          move      c1 to ninvoutflag              .print add charges flag`
.
INPUT    
.
.Booleans
               create  OTRUE,VarType=VT_BOOL,VarValue=1
               create  OFALSE,VarType=VT_BOOL,VarValue=0
.START PATCH 3.9 REPLACED LOGIC
.              create  Zoom85,VarType=VT_I4,VarValue=1
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.END PATCH 3.9 REPLACED LOGIC
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                      setprop ex,*SheetsInNewWorkbook=C1
               setprop ex,*WindowState=xlMinimized
.Create Workbooks collection
               getprop ex,*Workbooks=books
.Create/Add a single Workbook
               books.add
               books.item giving book using 1
.Create Worksheets collection
               getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
               sheets.item giving sheet using 1
.START PATCH 3.9 ADDED LOGIC
               setprop sheet.PageSetup,*Orientation=xlLandscape
               setprop sheet.PageSetup,*Zoom=Zoom80
               setprop sheet.PageSetup,*TopMargin=AllMargin
               setprop sheet.PageSetup,*BottomMargin=AllMargin
               setprop sheet.PageSetup,*RightMargin=AllMargin
               setprop sheet.PageSetup,*LeftMargin=AllMargin
               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal
.END PATCH 3.9 ADDED LOGIC
               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
               sheet.range("A1:E1").Merge
               sheet.Shapes.AddPicture using "\\nts1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
......................................................
               setprop        sheet.Range("A8"),*Value="LR Number"
               setprop        sheet.Range("B8"),*Value="Inv Number"
               setprop        sheet.Range("C8"),*Value="List"
               setprop        sheet.Range("C9"),*Value="Name"
               setprop        sheet.Range("D8"),*Value="List"
               setprop        sheet.Range("D9"),*Value="Select"
               setprop        sheet.Range("E8"),*Value="Mail Date"
               setprop        sheet.Range("F8"),*Value="Order"
               setprop        sheet.Range("F9"),*Value="Quantity"
               setprop        sheet.Range("G8"),*Value="Billed"
               setprop        sheet.Range("G9"),*Value="Quantity"
               setprop        sheet.Range("H8"),*Value="Price"
               setprop        sheet.Range("I8"),*Value="Account"
               setprop        sheet.Range("I9"),*Value="Receivable"
               setprop        sheet.Range("j8"),*Value="Additional Charge 1"
               setprop        sheet.Range("j9"),*Value="Description"
               setprop        sheet.Range("k9"),*Value="Amount"
               setprop        sheet.Range("l8"),*Value="Additional Charge 2"
               setprop        sheet.Range("l9"),*Value="Description"
               setprop        sheet.Range("m9"),*Value="Amount"

               setprop        sheet.Range("n8"),*Value="Additional Charge 3"
               setprop        sheet.Range("n9"),*Value="Description"
               setprop        sheet.Range("o9"),*Value="Amount"
               setprop        sheet.Range("p8"),*Value="Additional Charge 4"
               setprop        sheet.Range("p9"),*Value="Description"
               setprop        sheet.Range("q9"),*Value="Amount"
               setprop        sheet.Range("r8"),*Value="Additional Charge 5"
               setprop        sheet.Range("r9"),*Value="Description"
               setprop        sheet.Range("s9"),*Value="Amount"
               setprop        sheet.Range("t8"),*Value="Additional Charge 6"
               setprop        sheet.Range("t9"),*Value="Description"
               setprop        sheet.Range("u9"),*Value="Amount"
               setprop        sheet.Range("v8"),*Value="Additional Charge 7"
               setprop        sheet.Range("v9"),*Value="Description"
               setprop        sheet.Range("w9"),*Value="Amount"
               setprop        sheet.Range("x8"),*Value="Additional Charge 8"
               setprop        sheet.Range("x9"),*Value="Description"
               setprop        sheet.Range("y9"),*Value="Amount"
               setprop        sheet.Range("z8"),*Value="Additional Charge 9"
               setprop        sheet.Range("z9"),*Value="Description"
               setprop        sheet.Range("aa9"),*Value="Amount"
               setprop        sheet.Range("ab8"),*Value="Additional Charge 10"
               setprop        sheet.Range("ab9"),*Value="Description"
               setprop        sheet.Range("ac9"),*Value="Amount"
.
               setprop xlRowHeight,VarValue="27.0"
               setprop        sheet.range("A8:ab9").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A8:ab9"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A8:ab9").Font,*Bold="True"
               //Setting up Borders so that user is clear that
               sheet.range("A8:ab9").BorderAround using *LineStyle=1,*Weight=MedThick
               move           "10",howmany
.
READINV  
          read      input,seq;ordvars
         GOTO      TOTAL IF OVER
          reset     cancodes                                .ST suppress canceled
          scan      Ostat in cancodes
          goto      Readinv  if equal
          packkey   ninvfld from olrn       
          CALL      NINVKey
         GOTO      ReadINV IF OVER
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               move           O2DES,NSEL2NAME
               clear          O2DES2
               unpack         OPPM,str3,str2
               pack           str6,str3,".",str2
               rep            zfill,str6
               move           C0,NSEL2PRICE
               move           str6,NSEL2PRICE
               move           "/M",NMODDESC
              else
               call           PARSITUP using O2DES,NSEL2NAME,C1
               call           PARSITUP using O2DES2,NSEL2NAME,C1
               reset          NSEL2NAME
               call           Trim using NSEL2NAME
               endif     
         MOVE      B1 TO RUNFLAG
              Move            c0,holdar
              Move            c0,holdap1
              Move            c0,holdap2
              Move            c0,holdap3
              Move            Ar,HOldar
              move            ap1,holdap1
              move            ap2,holdap2
              move            ap3,holdap3

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
          MOVE      C3 TO SALESBR                             .brokerage
          GOTO      PROCESS
          endif
         ENDIF
.

.See Oslspern.inc for details.
...............................................................................
.
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM3,TEAM1,Team1,team1:                 .1-5
                   TEAM3,Team1,TEAM1,TEAM1,TEAM1:                            .6-10        
                   TEAM1,Team1,TEAM1,Team1,Team1:                            .11-15       
                   TEAM1,Team1,Team1,TEAM3,TEAM1:                            .16-20       
                   TEAM1,TEAM1,team1,team1,team1:                            .21-25       
                   TEAM1,TEAM3,team3,team1,team3:                            .26-30       
                   TEAM1,TEAM1,team1,team1,team1                             .31-35       
         MOVE      SALESNUM TO SALESBR
...............................................................................
PROCESS   if        (OcompId = "P" & SalesBr = c1)                              
          ADD       C1 TO COUNTPL
          elseif (OcompId2 = "P" & SalesBr = c3)                      
          ADD       C1 TO COUNTPL
          else
          ADD       C1 TO COUNT
          endif
         DISPLAY   *P10:12,"NUMBER OF NIN INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
                   *P10:13,"NUMBER OF PL  INVOICES PROCESSED: ",COUNTPL,b5,lrn,b1,invnum:
                   *P15:07,today
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
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
          move      "NInvAcdTst",Filename
          pack      KeyLocation,"Key: ",NInvAcdFld
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               move           yes to subppsw
               

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
          Clear     M$AP2
          IF        (Formap2 <> c0)
          MOVE      MASK92 TO M$AP2
          EDIT      FORMAP2 TO M$AP2
          endif
.
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
.
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
          if        ((OcompId = "P" & Salesbr = c1) or (Ocompid2 = "P" & Salesbr = c3))                       
          ADD       PREPAY TO TOTPLARP      .TOTAL PREPAID ORDERS.
          ADD       FORMAR TO TOTPLAR
          ADD       AP TO TOTPLAP1
          ADD       FORMAP2 TO TOTPLAP2
          ADD       AP3 TO TOTPLAP3
            ADD       LRINC TO TOTPLLR
          ADD       NININC TO TOTPLNIN
          ADD       XNINC TO TOTPLXNINc
          ADD       TAXES TO TOTPLSTAX
          ADD       POST TO TOTPLPOST
          else
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
          endif
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
.begin patch 3.80
          Reset     EXFEELST
.         MATCH     OLNUM TO Lmanage
          scan      Olnum in EXFEELST
.end patch 3.80
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
          If        (olnum = "024593")
          ADD       LRINC TO LRPLEXFEE
          ADD       FORMAR TO ARPLEXFEE
          ADD       AP TO APPLEXFEE
          ADD       FORMAP2 TO APPLEXFEE
          Else
          ADD       LRINC TO LREXFEE
          ADD       FORMAR TO AREXFEE
          ADD       AP TO APEXFEE
          ADD       FORMAP2 TO APEXFEE
          Endif
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
                              if        (newbiz = yes & Ocompid2 = "P")
                              add       lrinc to coldPLlm
                              move      no to newbiz
                              Elseif    (newbiz = yes)      
                              add       lrinc to coldlm
                              move      no to newbiz
                              else
                              if        (Ocompid2 = "P")                        
                                        ADD       LRINC TO LRPLMRINC        *RENT/MANAGEMENT INCOME
                                        else
                                        ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
                                        endif
.end patch 3.75
                             move       no to newbiz
                              endif

.begin patch 3.75
          if        (Ocompid2 = "P")                        
          ADD       NININC TO NINPLMRINC        *RENT/MANAGEMENT INCOME
          ADD       FORMAR TO ARPLMR
          ADD       AP TO APPLMR
          ADD       FORMAP2 TO APPLMR
          else
          ADD       NININC TO NINMRINC        *RENT/MANAGEMENT INCOME
          ADD       FORMAR TO ARMR
          ADD       AP TO APMR
          ADD       FORMAP2 TO APMR
         endif
.end patch 3.75
         GOTO      PRINT
MEXCH    
.begin patch 3.75
          if        (Ocompid2 = "P")                        
          ADD       LRINC TO LRPLMEINC        *EXCH/MANAGEMENT INCOME
          ADD       NININC TO NINPLMEINC        *EXCH/MANAGEMENT INCOME
          ADD       FORMAR TO ARPLME
          ADD       AP TO APPLME
          ADD       FORMAP2 TO APPLME
          else
          ADD       LRINC TO LRMEINC        *EXCH/MANAGEMENT INCOME
          ADD       NININC TO NINMEINC        *EXCH/MANAGEMENT INCOME
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
        move       no to newbiz
         call      newbus
.begin patch 3.75
          if                  (newbiz = yes & Ocompid = "P")
             add              lrinc to coldPLbr
             move       no to newbiz
             Elseif (newbiz = yes)
             add              lrinc to coldbr
             move       no to newbiz
           ElseIf   (OcompId = "P")
           add       lrinc to lrPLbrinc
             else
            add       lrinc to lrbrinc
          endif

          if        (OcompId = "P")                         
                    ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
                    ADD       FORMAR TO ARPLBR
                    ADD       AP TO APPLBR
                    ADD       FORMAP2 TO APPLBR
                    Else
                    ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
                    ADD       FORMAR TO ARBR
                    ADD       AP TO APBR
                    ADD       FORMAP2 TO APBR
                    endif
.end patch 3.75
         GOTO      PRINT
BEXCH
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
        move       no to newbiz
          call      newbus
.begin patch 3.75
          if                  (newbiz = yes & Ocompid = "P")
          add       lrinc to coldPLbe
             move       no to newbiz
             Elseif (newbiz = yes)
          add       lrinc to coldbe
             move       no to newbiz
           ElseIf   (OcompId = "P")
          add        lrinc to lrPLbeinc
          else
          add        lrinc to lrbeinc
          endif
.begin patch 3.75
          if        (OcompId = "P")                         
          ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
          ADD       FORMAR TO ARPLBE
          ADD       AP TO APPLBE
          ADD       FORMAP2 TO APPLBE
          else
          ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
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
.....**** DLH 01 july 08 I think this needs to be here
          Move      No,Newbiz
........  
         call      newbus
.begin patch 3.75
          if                  (newbiz = yes & Ocompid = "P")
          add       lrinc to coldPLbr
          move      no to newbiz
             Elseif (newbiz = yes)
          add       lrinc to coldbr
          move      no to newbiz
           ElseIf   (OcompId = "P")
          add       lrinc to lrPLbbr
          else
          add       lrinc to lrbbr
          endif
.end patch 3.75
          if        (OcompId = "P")                         
          ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
          ADD       NININC TO NINPLBRINC        *RENTAL/BROKERAGE INCOME
          ADD       FORMAR TO ARPLBBR
          ADD       AP TO APPLBBR
          ADD       FORMAP2 TO APPLBBR
          else
          ADD       FORMAR TO ARBBR
          ADD       AP TO APBBR
          ADD       FORMAP2 TO APBBR
          endif
.end patch 3.75
         GOTO      PRINT
BEXCHB
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
.....**** DLH 01 july 08 I think this needs to be here
          Move      No,Newbiz
........  
         call      newbus
.begin patch 3.75
          if                  (newbiz = yes & Ocompid = "P")
          add       lrinc to coldPLbE
          move      no to newbiz
             Elseif (newbiz = yes)
          add       lrinc to coldbE
          move      no to newbiz
           ElseIf   (OcompId = "P")
          add       lrinc to lrPLbbe
          else
          add       lrinc to lrbbe
          endif

          if        (OcompId = "P")                         
          ADD       NININC TO NINPLBEINC        *EXCH/BROKERAGE INCOME
          ADD       FORMAR TO ARPLBBE
          ADD       AP TO APPLBBE
          ADD       FORMAP2 TO APPLBBE
          else
          ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
          ADD       FORMAR TO ARBBE
          ADD       AP TO APBBE
          ADD       FORMAP2 TO APBBE
          endif
         GOTO      PRINT
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
      return                                     
*......................................................................
.
PRINT    
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value=OLRN
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value=Invnum
              pack            str12,"C",str9
              call            Trim using O1DES
              setprop         sheet.Range(str12),*Value=O1DES
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=NSEL2NAME
          PACK      DATEPRT2 FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=DatePRt2
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=Oqty,*NumberFormat="##,####0"
              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=QTYBILD,*NumberFormat="##,####0"
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=PPM,*NumberFormat="###00"
              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=M$ar
.now get addtional charges
                    MOve      c1,ExcelAdrec
               FOR           AcdRecCount,"1","15"
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
                              MOve   NInvAcdRec(AcdRecCount).NInvAcdPercRec,NInvAcdPerc
                              MOve   NInvAcdRec(AcdRecCount).NINVAcdANINCDRec,NINVAcdANINCD
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdqtyRec,str9
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdTotalRec,str15
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdAextcdRec,NinvAcdAextcd
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateTRec,NinvAcdRateT
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdDescRec,nacdtext

               move           NINVAcdANINCD to str3
               call           trim using str3
               move           c0 to anincd
               move           str3 to Anincd

               If             (NinvacdNum = "")
               Break
               endif
                              if             (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "041")
                              Goto           DontShowDet
                              endif                           *
..
                              if        (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "085")
                              Goto           DontShowDet
                              endif                           *

                              if        (ninvoutflag = 1 & (ANINCD = c3 or ANINCD = c4))
                              Goto           DontShowDet
                              endif                           *

                              if        (ninvoutflag = 1 & (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 ))
                              Goto           DontShowDet               ;mailer copies do not print codes 1,3, or 4
                              endif                           *

                              if        (ninvoutflag = 1 & Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
                              Goto           DontShowDet
                              endif                           *

                clear          str10
                              If             (NinvAcdRate > 0)
                                             move           mask72 to str12
                                             edit           NinvAcdRate to str12
                                             Move           "@" to Str1
                                             else
                                             Clear          str1
                                             clear          str12
                              endif
                              move           "ZZZ.9999" to str8
                              edit           NInvAcdPerc to str8
                              scan      "-",str15
                              if        equal
                              reset     str15
                              call      removechar using str15,DASH
                              clear     str20
                              call      trim using str15
                              pack      str20 from Dash,str15
                              move      str20,str15
                              else          
                              reset     str15
                              endif


.keep track of how many for correct column (In future)
. for now 1=J 2=L          
                    call      debug
                              move            howmany,str9
                              call            Trim using str9

                              if        (ExcelAdrec = c1)
                              pack            str12,"J",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"K",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c2)
                              pack            str12,"L",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"M",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c3)
                              pack            str12,"N",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"O",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c4)
                              pack            str12,"P",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"Q",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c5)
                              pack            str12,"R",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"S",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c6)
                              pack            str12,"T",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"U",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c7)
                              pack            str12,"v",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"w",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c8)
                              pack            str12,"x",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"y",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = c9)
                              pack            str12,"z",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"aa",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                            
                              Elseif        (ExcelAdrec = 10)
                              pack            str12,"ab",str9
                              setprop         sheet.Range(str12),*Value=NacdText
                              pack            str12,"ac",str9
                              setprop         sheet.Range(str12),*Value=STR15,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
                              endif
                              add       c1,ExcelAdRec
DontShowDet
               repeat

         GOTO      readinv
*......................................................................
.
TOTAL    
.
.
EOJ
fini
                              move      howmany,str9
                              call      trim using str9
                              pack      str5 from "A8"
                              pack    str12,"AC",str9
                              sheet.range(str5,str12).Columns.Autofit
.........................................................................................................................
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              Clear     Taskname
                              Append    "C:\WORK\Invoice",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              reset     taskname
                              erase          taskname
                              
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

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
              include         nmodio.inc
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
              INCLUDE         NSEL2IO.INC
        include nrevio.inc
        include nprjio.inc
         INCLUDE    COMLOGIC.inc



//..............................................................................
//NEOM0024 - list owner income summary report
//..............................................................................

PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   consacct.inc
         INCLUDE   hp.inc
	 INCLUDE   ninvdd.inc
	 INCLUDE   NInvAcddd.inc
 	 INCLUDE   compdd.inc
	 INCLUDE   cntdd.inc
         INCLUDE   NmtxDD.INC
         INCLUDE   NORDDD.INC
         INCLUDE   nowndd.inc
         INCLUDE   GNXTDD.INC
         INCLUDE   NDAT3DD.INC
         INCLUDE   nslsdd.inc
         INCLUDE   nmrgdd.inc
         INCLUDE   NLOBDD.INC
         INCLUDE   NJSTDD.inc
shipsw   DIM       1
mrgsw    DIM       1
         INCLUDE   npaydd.inc
         INCLUDE   nmoadd.inc
         INCLUDE   nshpdd.inc
         INCLUDE   ndatdd.inc
         INCLUDE   nacddd.inc
         INCLUDE   PrtPagedd.inc
//..........................................
Release  init      "2.53"               DB 26JUN2006 XLS Conversion
//release  init      "2.52"               JD 07Feb2006 write to label file for mailing reports
//release  init      "2.51"              JD 23Jan2006 ready for 2006.
//release  	init      	"2.5"         DLH 2March2005 Invoice COnversion
//release	init	"2.48"	ASH 21MAR2005  Added logic to test for empty Input file 
//release  init      "2.47"         JD	        12Jan2005 ready for 2005.
//release  init      "2.46"         JD	        08NOV2004 prtpagedd.inc
//release  init      "2.45"        ASH	06AUG2004	Logo Conversion
//release  init      "2.46"        DMB	26MAY2004	Mailer Conversion
//release   init      "2.431"               JD 11Feb04 send SMTP message do Comp Request.
//release  init      "2.43"                JD 06Jan2004 fixed if with exflag check.
//release  init      "2.42"                DB 01MAY2003 added code to create pdf in c:\work
//release  init      "2.41"               DLH 07Jan2003 Convert to PRTpage
//release  init      "2.40"               JD 28jun2002 added xfoot ending balance.
//release  init      "2.31"              DLH 14Feb2002 reason code 37
//release  init      "2.30"              JD21NOV01 added exchange breakout/care spool file.
//release  init      "2.17"              JD 07Sep01 added New adjres codes.
//release  init      "2.16"              JD 10may01 added lob read if no new billing.
//release  init      "2.15"              JD 19mar01 added owner print info on adjustment page.
//release  init      "2.14"             ASH 02OCT2000 NEW SERVER ADDED
//release  init      "2.13"             JD 27sep00   IF 2aps print owner info payment info.
//release  init      "2.12"             JD 13jul00  fixed mask ap2
//release  init      "2.11"             JD 28mar00  changed to new address.
//release  init      "2.1"              JD 31jan00  temp fix for jan new year check.
//release  init      "2.0"             DLH 25Oct99 convert form to plb
//release  init      "1.8"             DLH 27Apr99 NININV Y2K
//RELEASE  INIT      "1.72"            JD  31mar99 changed dates without cc
//RELEASE  INIT      "1.7"            ASH 30DEC98 NINORD Y2K, File expansion; CONSACCT.INC VAR EXPANSION
//release  init      "1.6"            JD30nov98 compressed mcomp print.
//release         init      "1.5"            JD 24jul98 fixed bug adj total/mtax print.
//release         init      "1.4"            JD 25Feb97   print only option
//release  init      "1.3"            JD 31Jan97  flat file for whitney
//release  init      "1.2"           DLH 27Mar96 cleanup # of break problems.
//release  init      "1.1"          DLH 04Mar96 
//change primary input file to ownerstm.srt - so we don't miss any accounts that did not have new billings.
//release  init      "1.0"          DLH 12Feb96 
//runs for report types as one report per owner/list
//primary input file is pareom.own
//.............................................................................
//input file  ownerstm.srt created by neom0023 
input     FILE      uncomp
//.......................................................................
//parfile - hold all exclusive list owner numbers and flag for a/r info on the report
parfile   IFILE     keylen=4
OUTPUT    FILE      UNCOMP
labels    IFILE     keylen=4
//..............................................................................
//CLOCK     FUNCTION
//....... ................
DATE      DIM       8
SYSMO     DIM       2
SYSDY     DIM       2
SYSYR     DIM       2
DATEMASK  INIT      "XX/XX/XX"
DATEPRT1  DIM       8
DATEPRT2  DIM       8
AP1OUT    DIM       15
ADJAPOUT  DIM       12
AP2OUT    DIM       15
aplotus   FORM      10.2
aplotown  FORM      10.2
AP1FORM   FORM      10.2
AP2FORM   FORM      10.2
aplotmsk  INIT      "ZZZZZZZZZZ99-"
TOTMASK   INIT      "$$,$$$,$$9.99-"
APMASK    INIT      "$$,$$$,$$9.99-"
ARMASK    INIT      "$,$$$,$$$,$$9.99-"
adjAPMSK  INIT      "$$$$,$$9.99-"
APMASK2   INIT      "$$,$$$,$$9.99-"
TOTOMSK   INIT      "$$,$$$,$$9.99-"
APCHECK   FORM      "000000001"
tadjmask  DIM       17
APSW      DIM       1
AP2SW     DIM       1
TAX501    FORM      1
form102   FORM      10.2
incount   FORM      5
flatflg   DIM       1
rflag     DIM       1
//.............................................................................
//adjustment reason code descriptions
nadjtext  DIM       35
adjres1   INIT      "Adjustment to Quantity"
adjres2   INIT      "Shipping"
adjres3   INIT      "Selection fee"
adjres4   INIT      "Running Charges"
adjres5   INIT      "Change in price"
adjres6   INIT      " "
adjres7   INIT      "Adjust Within A/P "
adjres8   INIT      "Adjust A/R & LR "
adjres9   INIT      "Adjust A/P & LR"
adjres10  INIT      "Cancel entire Bill"
adjres11  INIT      "No invoice A/P to Lr "
adjres12  INIT      "Late Lo inv LR to A/P "
adjres13  INIT      "Adjustment of Income"
adjres14  INIT      "Advance Payment to LO"
adjres15  INIT      "Adjustment of Tax"
adjres16  INIT      "Short Payment"
adjres17  INIT      "Commission"
adjres18  INIT      "Postage"
adjres19  INIT      "Direct Payment to LO"
adjres20  INIT      " "
adjres21  INIT      "Advance Payment to LO"
adjres22  INIT      "Reduction of A/R"
adjres23  INIT      "Reduction of A/P (Contra)"
adjres24  INIT      "Discount Earned"
adjres25  INIT      "Additional Billing"
adjres26  INIT      "Write off of A/R"
adjres27  INIT      "Prepayment"
adjres28  INIT      "Write off of A/P"
adjres29  INIT      "                        "
adjres30  INIT      "Taking Credit-Original open"
adjres31  INIT      "Credit Transfer"
adjres32  INIT      "Refund Credit Taken"
adjres33  INIT      "Cancelled/Billing Adjusted "
adjres34  INIT      "Adj due to Order Change"
adjres35  INIT      "Court Imposed Bankruptcy Charge"
adjres36  INIT      "Bankruptcy, Un-collectible A/R "
adjres37  INIT      "Void Check"
adjres99  INIT      "Entry Correction"
//
//FILES.
//.............................................................................
// WORK VARIABLES
//.............................................................................
XFootFlag      Init           "Y"            ;if set to No account did not balance            
ELEVEN    FORM      "11"
FIFTY1    FORM      "51"
TYPIST    DIM       2
TOTARp    FORM      10.2       *prepaid
TOTpMOA   FORM      10.2       *MOA Applied to prepaid
TOTAR     FORM      10.2
TOTAP1    FORM      10.2
TOTAP2    FORM      10.2
TOTAP     FORM      10.2
TOTNIN    FORM      10.2
TOTLR     FORM      10.2
TOTSTAX   FORM      10.2
TOTCTAX   FORM      6.2
TOTPOST   FORM      5.2
//TRIPLEX BILLING VARIABLES.
TDMCLIST  INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
LRMRINC   FORM      10.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC   FORM      10.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC    FORM      10.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC   FORM      10.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC   FORM      10.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN    FORM      10.2      UNKNOWN LR INCOME.
LRBBE     FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR     FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
ARMR      FORM      10.2      TOTAL MANAGEMENT/RENTAL  A/R
ARME      FORM      10.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM       FORM      10.2      TOTAL MANAGEMENT A/R
ARBR      FORM      10.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE      FORM      10.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN    FORM      10.2      UNKNOWN  A/R.
ARBBE     FORM      10.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR     FORM      10.2      TOTAL BATCH BILL A/R RENT PORTION
APMR      FORM      10.2      TOTAL MANAGEMENT/RENTAL A/P
APME      FORM      10.2      TOTAL MANAGEMENT/EXCHANGE A/P
APM       FORM      10.2      TOTAL MANAGEMENT A/P
APBR      FORM      10.2      TOTAL BROKERAGE/RENTAL A/P
APBE      FORM      10.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN    FORM      10.2      UNKNOWN A/P.
APBBE     FORM      10.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR     FORM      10.2      TOTAL BATCH BILL A/P RENT PORTION

ppflag    DIM       1            'P' if equal else blank
PMASK     DIM       1
FORM2     FORM      2
FORM22    FORM      2.2
FORM7     FORM      7
FORM9     FORM      9
FORM52    FORM      5.2
FORM11    FORM      11
NUM10     FORM      10
COUNT     FORM      5                 total reads input file
COUNTA    FORM      5                 new invoices
COUNTB    FORM      5                 new payments
countC    FORM      5                 new adjs
countD    FORM      5                 current open bills 
CO        FORM      1
BATCHBR   FORM      1       "0" =NO, "1" = YES.
RENTSW    FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR   FORM      2
SALESNUM  DIM       2
TEAM1     INIT      "01"     SUSAN
TEAM2     INIT      "02"    ELAINE
TEAM3     INIT      "03"    LIST MANAGEMENT
//RUNCODES INIT     "005051-009766"
//..............................................................................
//PRINT MASK VARIABLES
MASK22    INIT      "ZZ.ZZ"
MASK42    INIT      "Z,ZZZ.ZZ-"
MASK72    INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52    INIT      "ZZ,ZZZ.ZZ-"
MASK62    INIT      "ZZZ,ZZZ.ZZ-"
MASK7     INIT      "Z,ZZZ,ZZZ"
MASK9     INIT      "ZZZ,ZZZ,ZZZ"
M$RTAX    DIM       5      *RETURN-TO TAX PERCENT
M$AR      DIM       13
M$ARp     DIM       13     *prepaid 
M$PPM     DIM       6
;Start Patch #1.7 - increased vars to handle OQTY increase
;m$oqty   DIM       9
m$oqty    DIM       11
M$QTY     DIM       11
M$AP1     DIM       13
M$AP2     DIM       13
M$STAX    DIM       8
M$CTAX    DIM       8
M$POST    DIM       6
M$LRINC   DIM       13
M$NINC    DIM       13
M$GROSS   DIM       13
;
MT$AR     DIM       15
MT$ARP    DIM       15
MT$pMOA   DIM       15     *prepaid 
MT$AP1    DIM       15
MT$AP2    DIM       15
MT$STAX   DIM       15
MT$CTAX   DIM       10
MT$POST   DIM       9
MT$LRINC  DIM       15
MT$NINC   DIM       15
;
NEW       FORM      5
REPRINT   FORM      5
PAGE      FORM      4
;LINES    FORM      2
innets    DIM       1
rectype   DIM       1
holdid    DIM       1
holdown   FORM       4
holdlist  FORM       6
holdlst1  DIM       35
HOLDREC   DIM       10
newMLR    FORM      4        1-4     MAILER NUMBER.            NIN
newLR     FORM      6        5-10    LIST RENTAL NUMBER.       NIN
newOWN    FORM      4       11-14    LIST OWNER NUMBER.        NIN
newGUAR1  DIM       1       15-15    OUTSIDE GUARANTY          NIN
newCNT    FORM      3       16-18    BROKER NUMBER.            NIN
newLIST   FORM      6       19-24    LIST NUMBER.              NIN
newMDTE   FORM      6       25-30    MAIL DATE.                NIN
newAP1    FORM      7       31-37    ACCOUNT PAYABLE ONE.      NIN
newDJCD   DIM       1       38-38    DOW JONES CODE.           NIN
newADJCD  DIM       1       39-39    ADJUSTMENT CODE.          NIN
newIDTE   FORM      6       40-45    INVOICE DATE.             NIN
newAP2    FORM      9       46-54    ACCOUNTS PAYABLE TWO.     NIN
newLST1   DIM       35      55-89    LIST DESCRIPTION ONE.     NIN
newCODE   DIM       1       90-90    CREDIT/DEBIT CODE,'C or D'NIN
newGUAR   DIM       1       91-91    GUARANTY CODE.            NIN
newAR     DIM       8       92-99    A/R (NO DECIMAL)
newCHKDTE DIM       6      100-105   INV CHECK DATE
newCNAME  DIM       25     106-130   CLIENT NAME
newadjsw  FORM      1      131-131   adjustment switch 2-adjusted
newxchrg  FORM      7.2
;
hldarflg  DIM       1
DATEKEY   DIM       6
TAXPRT    INIT      "       "
APTOTOWN  FORM      10.2       DLH
APTOWNck  FORM      10.2       DLH
APTOTWNX  FORM      10.2       jd
APTOTWNR  FORM      10.2       jd
OWNERMSK  DIM       17
OWNRMSKX  DIM       17
OWNRMSKR  DIM       17
arform    FORM      10.2
arout     DIM       17
arflag    DIM       1        from parfile if True print ar
pass      FORM      1        1=new bills, 2=old bills, 3=old bills, 4=adjustments
LISTCHG   DIM       1
CBLSTNUM  FORM      6
CBOWNNUM  FORM      4
JSTN      FORM      1
manpay    FORM      10.2
MANMASK   INIT      "$,$$$,$$$,$$9.99-"
OWNBR     DIM       1
PAYCHK    FORM      1
first     DIM       1
CHKDATE   DIM       8
APTOTDET  FORM      10.2
LSTMASK   DIM       17
APTOTLST  FORM      10.2
ARTOTLST  FORM      10.2
PAYKEY    DIM       5
adjap     FORM      7.2
adjap1    FORM      7.2
adjap2    FORM      7.2
aplist    FORM      10
lastrec   INIT      "F"      'T'=true
newflag   INIT      "F"      'T'=true no previous balance
listbrk   INIT      "F"
thisown   FORM      4
hit       FORM      5        0=no records for this owner/list
exflag    INIT      "F"
exerflag  INIT      "N"
splpass   FORM      "0"
carebrk   INIT      "N"
splname   DIM       45




Cell DIM	5
Cell1 DIM	5
CurCellNum FORM 5
CellRange DIM   255
BalanceRow FORM 5
STR255     DIM  255
//Patch 2.53
//In order to use any of the properties/methods associated with all parent objects
//of the Worksheet, I need to create automation objects for each of them.
//
//Look at Excel Object Model to understand heirarchy.  This can be found in hard
//documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
//Software available via PL/B Designer - create a Container object on a form, create an Excel
//Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
//Browser, which will give you SOME of the components of the Object Model.  To browse the Object
//Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
//In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
//view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
//option to locate Help topics to see specifics.
//
//General heirarchy:
// Excel Application
//       Workbooks Collection (all open Workbooks)
//               Single Workbook
//                       Worksheets Collection (all Worksheets in this Workbook)
//                               Single Worksheet
//                                       SortColumn (a Single Column in that Worksheet used for sorting)

books   			AUTOMATION
book    			AUTOMATION
.......................
//bars	automation
//bar	automation
//menubar INTEGER 2,"0x00000006"
.......................
SubFlag form    1
sheets  			AUTOMATION
sheet   			AUTOMATION
sortcol 			AUTOMATION
sortcol1			AUTOMATION
ex      			AUTOMATION       class="Excel.Application"
exrange      			AUTOMATION       

test				automation

HPageBreaks			AUTOMATION
HPageBreak			AUTOMATION
VT_BOOL 			EQU		 11
OTRUE   			VARIANT
OFALSE  			VARIANT
VT_I4   			EQU		 3           .4 byte integer
Zoom85  			VARIANT
VT_R8				EQU		 5           .Double - 8 byte Real
xlColumnWidth			VARIANT

xlColumnWidthGross		VARIANT

xlColumnWidthMailer		VARIANT
xlColumnWidthPrice		VARIANT

TopMargin			VARIANT
BottomMargin			VARIANT
LeftMargin			VARIANT
xlCenter			VARIANT
//Formatting vars needed
//This constant was found in the Object Browser in Excel under the Help topic for the
//HorizontalAlignment property of the Range object.
AlignLeft              		integer 4,"0xffffefdd"
AlignRight             		integer 4,"0xffffefc8"
AlignCenter            		integer 4,"0xffffeff4"
SheetsDefault          		integer 4,"0x00000000"
xlLandscape            		integer 4,"0x2"                     .2
xlMinimized            		integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle		integer 4,"0x2"

xlBorderWeightMedium		variant
xlPageBreakManual		variant
xlPageBreakAutomatic 		variant

SheetIndex			variant
CELLPOINT			FORM	5.2
C15	FORM "15"

CellRowCnt	FORM	"46"
CellRowCnt1	FORM	"54"

//Create the Variant objects
//Initialize variables
        move    C0,SubFlag
	create  Zoom85,VarType=VT_I4,VarValue=70
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	xlColumnWidth,VarType=VT_R8,VarValue="0.0"

	create	xlColumnWidthGross,VarType=VT_R8,VarValue="10.5"	
	create	xlColumnWidthMailer,VarType=VT_R8,VarValue="40.5"	
	create	xlColumnWidthPrice,VarType=VT_R8,VarValue="9.0"		
//"1" increment in Excel interface equals "1.3888" in OLE logic
	create	TopMargin,VarType=VT_R8,VarValue="18"		Roughly equals .25 inches:  18 * 1.388 = 25
	create	BottomMargin,VarType=VT_R8,VarValue="36"	Roughly equals .50 inches:  36 * 1.388 = 50
	create	LeftMargin,VarType=VT_R8,VarValue="14"		Roughly equals .25 inches:  18 * 1.388 = 25	
//Patch 2.53
	create	xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
	create	xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"	
	create	xlCenter,VarType=VT_R8,VarValue="-4108"		
	create	xlBorderWeightMedium,VarType=VT_R8,VarValue="-4138"	
	
	create	SheetIndex,VarType=VT_I4        		
//Patch 2.53



//Patch 2.53
//NINLogo	PICT
//	CREATE	NINLogo=3:13:30:50:
//		"\\nts0\c\netutils\NIN logo black outline.jpg"
//         MOVE      "Names in the News Ca" TO COMPNME

//         MOVE      "NEOM0024" TO PROGRAM
         MOVE      "MONTHLY List Owner Report" TO STITLE
         PACK      STR35,"c:\work\loact.tmp"
         PACK      STR45,"c:\work\Ownerstm.LST"
         prepare   output,STR35
//Start Patch 2.52
         PACK   STR35,NTWKPATH1,"EXCLOWNS"
         PACK   STR45,NTWKPATH1,"EXCLOWNS"
         PREPARE   labels,STR35,STR45,"4","190"
//End Patch 2.52
//begin patch 2.41
//         SPLOPEN   STR45
//END PATCH 2.14 REPLACED LOGIC
//         PRINT     hpreset,hpland:
//                   033,"&l51P":               page length
//                   033,"&l50F":               number lines
//                   033,"&l0E",033,"&a0c0R"     top margin * print position
//begin patch 2.41
//patch2.42  - commentingout/moving so prtfile can pick up correct date prefix for filename 
//         CALL      PRTFORM
//patch2.42
CHOOSE

         Call      PAINT
         Move     "E" to str1
         Keyin     *P25:14,"(E)om processing, (P)rint report":
                   " ",*T60,*RV,str1;
         Cmatch    "P" TO str1
         IF	Equal
         	Move	yes to rflag
         else   
         	move	no to rflag
         endif
      
CLOCK    CLOCK     DATE TO DATE
         MOVE      DATE TO DATEMASK
.         MOVE      DATE TO TODAY
         UNPACK    DATE INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         KEYIN     *CL
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
         MATCH     "NEOM0024" TO PROGRAM         .ENTRY FROM DSINIT?
       IF        NOT EQUAL                     . NO.
DATE
         MOVE      YES TO STR1
         KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,STR1
         CMATCH    YES TO STR1
         GOTO      OPEN IF EQUAL
         GOTO      DATE IF EOS
         KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,SYSMO,"/":
                   *DE,*JR,*ZF,*RV,SYSDY,"/",*DE,*JR,*ZF,*RV,SYSYR
         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
//         PACK      DATE FROM SYSMO,SLASH,"31",SLASH,SYSYR
         MOVE      DATE TO DATEMASK
         MOVE      DATEMASK TO TODAY
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      DATE
         endif
OPEN     
//patch2.42
//         clock     timestamp,str6
//         unpack    str6,str4,str2
//         bump      str4 by 2
         pack      str8,"own",sysmo,sysyr
         pack      splname,"c:\work\",str8,".lst"
         
//         PRTOPEN   Laser,"\\NTS0\Laser6",str8,noprint,spoolfile=splname
//Create a new excel worksheet
//.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True"
	setprop ex.CommandBars("Standard"),*Visible="True"
	setprop ex.CommandBars("Formatting"),*Visible="True"
	setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
//........................
//.	setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
//.	setprop ex,*DisplayFullScreen=OTRUE
//........................
//.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C1
//........................
//	getprop ex,*CommandBars=bars
//	getprop	bars,*ActiveMenuBar=bar
//	setprop	bar,*Visible="True"
//	setprop	bar,*Position=menubar
//........................
//.Create Workbooks collection
        getprop ex,*Workbooks=books
//.Create/Add a single Workbook
        books.add
        books.item giving book using 1
//.Create Worksheets collection
        getprop book,*Sheets=sheets
//.Create a single Worksheet - we did not need to add it as we set the default above to
//.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
       setprop sheet.PageSetup,*Orientation=xlLandscape
.        pack taskname," &D",crlf," &P"
.        setprop sheet.PageSetup,*RightHeader=taskname        
//.START PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
        setprop sheet.PageSetup,*Zoom=Zoom85
        setprop sheet.PageSetup,*TopMargin=TopMargin
        setprop sheet.PageSetup,*BottomMargin=BottomMargin
        setprop sheet.PageSetup,*FooterMargin=TopMargin
        setprop sheet.PageSetup,*LeftMargin=LeftMargin
        setprop sheet.PageSetup,*RightMargin=TopMargin       
        
        
//
	getprop sheet,*Hpagebreaks=HpageBreaks
	pack str5 with "L1",":","P1"
.        Setprop Sheet.Range(str5),*ColumnWidth=xlColumnWidth


.        Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual 	

	 move c1 to curcellnum

//         PRTOPEN   Laser,"\\NTS0\Laser6",str8,noprint,spoolfile=splname
//        CALL      PRTFORM
//patch2.42
//         trap      eoj if f5
        open      parfile,"pareom"
//         move      sysmo to n2
//         sub       c1 from n2
//         compare   c0 to n2
//         if        equal          .its jan last month was dec set key accordingly
//         move     sysyr to n2
//         sub      c1 from n2
//         move     n2 to str2
//begin patch 2.47
//         pack     DATEKEY from "200312"
//         pack     DATEKEY from "200412"
//         rep      zfill in datekey
//         else
//         move     n2 to str2
//         pack     DATEKEY from cc,sysyr,str2
//         rep      zfill in datekey
//         endif
//begin patch 2.51
	move	sysmo to n2
	sub	c1 from n2
	compare	c0 to n2
	if equal          //.its jan last month was dec set key accordingly
		move	sysyr to n2
		sub	c1 from n2
		move	n2 to str2
.
		pack	str4,CC,SYSYR
		move	str4,N4
		sub	C1,N4
		move	N4,str4
.
		pack	DATEKEY,str4,"12"
 	else
		move	n2 to str2
		pack	DATEKEY from cc,sysyr,str2
	endif
//End patch 2.51
//end patch 2.47
         rep      zfill in datekey
//         open      input,"ownerstm.srt"
        open      input,"c:\work\ownerstm.srt"
         move      c1 to nslspath
//begin patch 1.8
         move      c1 to ndatpath
//end patch 1.8
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
BEGIN
         trap      eoj if f5
         move      c0 to holdown
        
input  
READsls  
         read      input,seq;rectype,slsvars
	
         if        over
//START PATCH 2.48 ADDED LOGIC
//Test to see if Input file was empty
		if (count = C0)
			shutdown
		endif
//END PATCH 2.48 ADDED LOGIC
//patch 2.53
.		if ("T" = hldarflg)
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete   
.        		Setprop Sheet.Range("L1"),*ColumnWidth=xlColumnWidthGross			
.		else
.	
..			sheet.Columns(12).Delete         					
.			sheet.Columns(9).Delete         					
.			sheet.Columns(12).Delete         	
.			sheet.Columns(12).Delete         	
..			sheet.Columns(12).Delete         	
.			sheet.Columns(12).Delete         			
.		endif    
//patch 2.53
         	move      "T" to lastrec
         	GOTO      TOTold
         endif
         Add       c1 to count
         DISPLAY   *P10:10,"Input records PROCESSED: ",COUNT,b1,slsown,b1,slslist
.Begin patch 2.41
         Compare	" 113" to Slsown
         goto		Readsls if not equal            ;CARE is processed by Neom0024A
.Begin patch 2.30
.         compare   c1 to splpass
.         goto      chkcare if equal
.         compare   "5457" to slsown
.         if        equal
.         call      totold
.         splclose
.         PACK      STR45,NTWKPATH1,"CAREOAR.LST"
.         SPLOPEN   STR45
.         PRINT     hpreset,hpland:
.                   033,"&l51P":               page length
.                   033,"&l50F":               number lines
.                   033,"&l0E",033,"&a0c0R"     top margin * print position
.
.         CALL      PRTFORM
.         move      yes to carebrk
.         move      c1 to splpass
.         goto      processr
.         endif
.
.chkcare
.         compare   "5457" to slsown
.         if        not equal
.         cmatch    yes to carebrk
.         if        equal
.         call      totold
.         move      no to carebrk
.         splclose
.         PACK      STR45,NTWKPATH1,"Ownerstm.LST"
.         SPLOPEN   STR45,"Q"
.         endif
.         endif
.End patch 2.30
.End patch 2.41
processr
         move      no to exflag
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         IF        EQUAL
         	MOVE      C0 TO N9
         	MOVE      OEXQTY TO N9
         	COMPARE   C0 TO N9
.Start patch 2.43
.         	IF        EQUAL
         	GOTO      SPLITOK IF NOT EQUAL
         	Move      yes to exflag
.         goto      readsls
.         endif
.End patch 2.43
         endif
;         endif
SPLITOK
         clear     str4
         move      slsown to str4
         rep       zfill in str4
         read      parfile,str4;str4,arflag                  .on the list????
         goto      readsls if over                   .no
//Force NWF gross billing

	move "T" to arflag
         match     "4020" to str4
         if	equal
         	move	yes to flatflg
         else
         	move	no to flatflg
         endif
         add	c1 to incount
         compare	c1 to incount
         if	equal
         	move	slsown to holdown
         	move	slslist to holdlist
         	move	rectype to holdid
         	move	c1 to pass
//Patch 2.53         
        	setprop sheet,*Name=holdown
//Patch 2.53        	         	
         endif
         compare   slsown to holdown            .break ?
         if        not equal
         
.		if ("T" = hldarflg)
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete         	
.			sheet.Columns(13).Delete      
.        		Setprop Sheet.Range("L1"),*ColumnWidth=xlColumnWidthGross			
.		else
.			sheet.Columns(12).Delete         					
.			sheet.Columns(12).Delete         	
.			sheet.Columns(12).Delete         	
.			sheet.Columns(12).Delete         	
.			sheet.Columns(12).Delete         			
.		endif         
         
         
         	call      totold
         	move      slsown to holdown
         	move      slslist to holdlist
         	move      rectype to holdid
         	

//Patch 2.53        	         	
.		getprop sheets,*Count=n5		
.		move n5 to str5
.		call trim using str5
.		pack str25,"After#:=",str5
        	
test

        	
        	sheets.add using *After=sheet

	       	getprop sheets,*Count=sheetindex
        	sheets.item giving sheet using sheetindex


        	Call SheetSetup
        	
.        	move c0 to curcellnum
        	move c1 to curcellnum
        	
        	
        	move c0 to CellRowCnt
        	setprop sheet,*Name=holdown        	
//Patch 2.53        	         	
         	
         endif
          compare   slslist to holdlist
          goto      nobreak if equal
          move      c4 to pass
          call      totold
          move      slslist to holdlist
          move      rectype to holdid
;
nobreak   MOVE      B1 TO RUNFLAG
          cmatch    "A" to rectype
          goto      readslsa if equal
          cmatch    "B" to rectype
          goto      readpaid if equal
          cmatch    "C" to rectype
          goto      readadj if equal
          cmatch    "D" to rectype
          goto       readsls1 if equal          
          display   *p1:24,*el,*b,"Invalid record type",*b,*w4
          goto      readsls

readslsa ADD       C1 TO COUNTA      
         DISPLAY   *P10:12,"New Sales records PROCESSED: ",COUNTA,b1,slsown,b1,slslist
;
         add       c1 to hit
.Begin patch 1.8
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.End patch 1.8
         COMPARE   C0 TO SLSAP2
         IF      EQUAL
         	MOVE    NO TO AP2SW
         ELSE     
         	MOVE     YES TO AP2SW
         ENDIF
         compare   c1 to hit
         call      preplob if equal
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      getmore
...............................................................................
.readsls1 - read sales file and get all  ninsls.old 4th pass
READsls1  
         Move	c4 to pass
         Move	B1 TO RUNFLAG
         Add	C1 TO COUNTd
         DISPLAY   *P10:15,"NUMBER OF Sales records PROCESSED: ",COUNTD,b1,slsown,b1,slslist
.Begin patch 1.8
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.End patch 1.8
         Add	c1 to hit
         Compare   c1 to countd     .1st open invoice?
	If	equal            .yes
		Compare   countc to c0      .were there adjustments
		If        less
			Call      totadj        .yes go print total
			Move      c4 to pass
			Goto      redsls1a
		Endif
		Compare   countb to c0   .were there payments?
		If    less
			Call      totpaid        .yes
			Move      c4 to pass
			Goto      redsls1a
		Endif
		Compare  counta to c0    .no, were there new bills
		Call     totbill if less    .yes
		Move     c4 to pass
	Endif
redsls1a COMPARE   C0 TO SLSAP2
         IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE     
         MOVE     YES TO AP2SW
         ENDIF
;
         compare   c1 to hit
         call      preplob if equal
;
;
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      getmore
;............................................................................
readpaid
         Move	c2 to pass
         Add	c1 to hit
         Add	C1 TO countB
         DISPLAY   *P10:13,"New payment records PROCESSED: ",countB,b1,slsown,b1,slslist
         Move	SLSLR TO NINVFLD
         Rep	ZFILL IN NINVFLD
         Call	NINVKEY
         Call	READPAY    
         Compare c1 to countb
         If	equal
		Compare counta to c0         .need to print new billing totals?
	        If	less      .yes   
		        Call	totbill
			Move	c2 to pass
		Else
	 	        Move      c2 to pass
	 	        Call      header
		Endif
         Endif
PROCES2
         COMPARE   C0 TO SLSAP2
         IF      EQUAL
         	MOVE    NO TO AP2SW
         ELSE     
         	MOVE     YES TO AP2SW
         ENDIF
.         MOVE      SLSLR TO NINVFLD
.         REP       ZFILL IN NINVFLD
.         CALL      NINVKEY
.         CALL      READPAY    
         PACK      MKEY FROM SLSMLR,Z3
         REP       zfill IN MKEY
         CALL      NMLRKEY
         IF        OVER
         	PACK      MKEY FROM Z3,Z3,Z3
         	CALL      NMLRKEY
         ENDIF
         CALL      READTAX
     
.
.Start Patch #1.7 - remmed and replaced logic
.         MOVE      DATEMASK TO DATEPRT1
.         EDIT      SLSMDTE  TO DATEPRT1
.         MOVE      DATEMASK TO DATEPRT2
.         EDIT      SLSIDTE TO DATEPRT2
.         MOVE      "99/99/9999" TO DATEPRT1
.         CLEAR     STR8
.         UNPACK    SLSMDTE TO STR4,STR4
.         APPEND    STR4,STR8                    .MMDD
.         UNPACK    SLSMDTE TO STR4
.         APPEND    STR4,STR8                    .CCYY
.         RESET     STR8                         .MMDDCCYY
.         EDIT      STR8 TO DATEPRT1
.
.         MOVE      "99/99/9999" TO DATEPRT2
.         CLEAR     STR8
.         UNPACK    SLSIDTE TO STR4,STR4
.         APPEND    STR4,STR8                    .MMDD
.         UNPACK    SLSIDTE TO STR4
.         APPEND    STR4,STR8                    .CCYY
.         RESET     STR8                         .MMDDCCYY
.         EDIT      STR8 TO DATEPRT2
.End Patch #1.7 - remmed and replaced logic
         MOVE      "99/99/99" TO DATEPRT1
.Start patch #1.7 - increased var
.         EDIT      STEMLDDT TO DATEPRT1
         MOVE      "99/99/99" TO DATEPRT1
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsMDTe TO cc,str2,str4
         pack       str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
.End patch #3.1 - increased var
.Start patch #1.7 - increased var
         MOVE      "99/99/99" TO DATEPRT2
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsiDTe TO cc,str2,str4
         pack      str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT2
.END patch #1.7 - increased var
.Begin patch 1.8
.        PACK      CHKDATE FROM CHKDTEM,SLASH,CHKDTED,SLASH,CHKDTEY
         PACK      CHKDATE FROM CHK1dTEM,SLASH,CHK1DTED,SLASH,CHK1DTEY
.End patch 1.8
         move      c0  to invdate
.Start Patch #1.7 - remmed and replaced logic
.         unpack    slsidte into mm,dd,yy
         unpack    slsidte into STR2,YY,mm,dd
.END Patch #1.7 - remmed and replaced logic
         CALL      CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE      JULDAYS TO INVDATE
         MOVE      SLSAP1 TO AP1FORM
         MOVE      YES TO APSW
         COMPARE   APCHECK TO AP1form
         IF	NOT GREATER
        	MOVE	NO TO APSW
         ENDIF
         Mult      "100" by ap1form
         MOVE    APMASK TO AP1OUT
         EDIT    AP1FORM TO AP1OUT
         ADD     SLSAP1 TO APTOTDET
         MATCH   YES TO AP2SW
         IF	EQUAL
.        display  *p10:12,"GOT IT 2",paychk,b1,slsap2,b1,aptotown,*w4
        	ADD	SLSAP2 TO APTOTOWN
         ELSE
.        display  *p10:12,"GOT IT 1",paychk,b1,slsap1,b1,aptotown,*w4
         	COMPARE   C0 TO PAYCHK
         	IF	EQUAL
        		ADD	SLSAP1 TO APTOTOWN
         	ENDIF
         ENDIF
         MATCH	yes TO AP2SW
         IF	EQUAL
.        MOVE    TOTMASK TO AP2OUT
.        EDIT    SLSAP2 TO AP2OUT
         	Move   slsap2 to ap2form
         	Mult	"100" by ap2form
         	MOVE	APMASK TO AP2OUT
         	EDIT    AP2FORM TO AP2OUT
.        ELSE
.        MOVE    TOTMASK TO AP2OUT
.        EDIT    SLSAP2 TO AP2OUT
.         move   slsap2 to ap2form
.         mult      "100" by ap2form
.        MOVE    APMASK TO AP2OUT
.        EDIT    AP2FORM TO AP2OUT
.        MOVE    YES TO AP2SW
        ENDIF
         MATCH     YES TO AP2SW
         IF        EQUAL
         	ADD	SLSAP2 TO APTOTLST
         ELSE      
         	COMPARE   C0 TO PAYCHK
         	IF	EQUAL
         		ADD	SLSAP1 TO APTOTLST
         	ENDIF
         ENDIF
         MATCH	YES TO AP2SW
         IF	EQUAL
         	GOTO      print
         ELSE
         	COMPARE    C0 TO PAYCHK
         	IF         NOT EQUAL
         		GOTO      readsls                    .sales read
         	ENDIF
         ENDIF
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.         RESET     EXCODES
.         SCAN      OELCODE IN EXCODES
.         goto      readsls if equal
         goto      print
.
.............................................................................
readadj
         move      c3 to pass
         add       c1 to countc
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         CALL      READPAY    
         move      slslr to nordfld
         rep       zfill in nordfld
         call      nordkey
         DISPLAY   *P10:14,"Adjustment records PROCESSED: ",countc,b1,olon,b1,olnum
;
         compare   c1 to countc       .1st adj?
         if        equal
                compare   countb to c0   .yes,  were there payments?
                if    less
                call      totpaid        .yes
                MOVE      C3 TO PASS
                GOTO      READADJ1
                ENDIF
           compare  counta to c0    .no, were there new bills
.           call     totbill if less    .yes
               if       less 
               call     totbill     .yes
               goto     readadj1
               endif 
          call       header
         MOVE       C3 TO PASS
         endif

READADJ1 compare   c1 to hit
         if        equal
 	        call      preplob 
         endif
         move      SLSlr to nINVfld
         rep       zfill in nINVfld
         call      nINVkey
         PACK      NJSTFLD FROM INVNUM,SLSADJSW
         REP       ZFILL IN NJSTFLD
.         DISPLAY   *P1:24,*EL,NJSTFLD,*B,*W3
         CALL      NJSTKEY
.         STOP      IF OVER
.Begin patch 1.9
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.         call       cvt
.         move       c0 to form92
.         move       cvtfld to form92
.          div        hund into form92
         move       c0 to adjap1
.        add        form92 to adjap1
         add        jstap1 to adjap1
.
         compare    c0 to adjap1
         if         not equal
	         compare    c0 to ap2
	         goto       readsls if not equal
         endif

.         move       c0 to cvtfld
.         move      jstap2 to cvtfld
.         call       cvt
.         move       c0 to form92
.         move       cvtfld to form92
.         div        hund into form92
         move       c0 to adjap2
.         add        form92 to adjap2
         add        jstap2 to adjap2
.End patch 1.9
         compare    c0 to adjap2          .if both ap adjs 0 skip
         if         equal
         compare    c0 to adjap1
         goto       readsls if equal
         endif
.         move      slslr to ninvfld
.         call      ninvkey
.         move      c0 to cvtfld
.         move       ap2 to cvtfld
.         call       cvt
.         move       c0 to form92
.         move       cvtfld to form92
         compare   c0 to adjap2
         if        not equal
	         move      yes to ap2sw
	         MOVE    apmask TO AP2OUT
	         EDIT    adjap2 TO AP2OUT
.         compare  c0 to adjap2
.         goto     readsls if equal
         else
	         move     no to ap2sw
	         MOVE    adjAPMSK TO adjAPOUT
	         EDIT     adjap1 TO adjAPOUT
	         compare  c0 to adjap1
	         goto     readsls if equal
         endif
         move     jstreasn to n2
         clear   nadjtext
         load     nadjtext from n2 of adjres1,adjres2,adjres3,adjres4,adjres5:
                 adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                 adjres13,adjres14,adjres15,adjres16,adjres17,adjres18,adjres19:
                 adjres20,adjres21,adjres22,adjres23,adjres24,adjres25,adjres26:
                 adjres27,adjres28,adjres29,adjres30,adjres31,adjres32,adjres33:
                 adjres34,adjres35,adjres36,adjres37
         compare  "99" to n2
         if      equal
         move    adjres99 to nadjtext
         endif
;
;Start Patch #1.7 - remmed and replaced logic
;         pack     str6 from omdtem,omdted,omdtey
;         move     str6 to slsmdte
;         pack      STR8 from omdtem,omdted,omdtec,omdtey
;         MOVE      STR8,SLSMDTE
;End Patch #1.7 - remmed and replaced logic
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      print
;
;............................................................................
getmore
         MOVE      slsLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         MOVE      slsLR TO NinvFLD
         REP       ZFILL IN NinvFLD
         call      ninvkey
         call      readpay
         move      paytn to paychk
         compare   c0 to page
         call      header if equal
;
         REP       ZFILL IN olnum
         MATCH     olnum TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
.START PATCH #1.7 - INCREASED VAR
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
;begin patch 1.8
         MOVE       QTYbild TO cmpt92
;         MOVE       QTYSHP TO FORM82
;         MOVE       FORM82 TO CMPT92
;end patch 1.8
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
;END PATCH #1.7 - INCREASED VAR
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
;begin patch 1.8
;         MULT       ".01" BY FORM92
;end patch 1.8
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
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
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM1,TEAM1,TEAM2:
                   TEAM2,TEAM3,TEAM2,TEAM1:
                   TEAM1,TEAM1,TEAM1,TEAM2:
                   TEAM1,TEAM2,TEAM2,TEAM1:
                   TEAM2,TEAM2,TEAM3,TEAM1,TEAM1,TEAM1
         MOVE      SALESNUM TO SALESBR
;..............................................................................
;
PROCESS
;begin patch 1.8
;         MOVE      C0 TO FORM7
;         MOVE      QTYSHP TO FORM7
;end patch 1.8
;
;START PATCH #1.7 - INCREASED VAR
;         MOVE      PPM TO FORM72
;         DIVIDE    HUND INTO FORM72
;         MOVE      FORM72 TO FORM32
         MOVE      PPM TO CMPT92
;begin patch 1.8
;         DIVIDE    HUND INTO CMPT92
;end patch 1.8
         MOVE      CMPT92 TO FORM32
.END PATCH #1.7 - INCREASED VAR
.
.
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
.
.
         MOVE      SLSAP1 TO AP1FORM
         MOVE      SLSAP2 TO AP2FORM

         MOVE      YES TO APSW
         COMPARE   APCHECK TO AP1form
         IF          NOT GREATER
         MOVE      NO TO APSW
         ENDIF

        MOVE    APMASK TO AP1OUT
        mult      "100" by ap1form
        EDIT    AP1FORM TO AP1OUT
        MOVE    APMASK TO AP2OUT
        mult      "100" by ap2form
        EDIT    AP2FORM TO AP2OUT
.         MOVE    APMASK TO AP2OUT
.         EDIT    slsap2 TO AP2OUT

         COMPARE   C0 TO SLSAP2
         IF      EQUAL
	         MOVE    NO TO AP2SW
         ELSE     
	         MOVE     YES TO AP2SW
         ENDIF
         move      c2 to tdmcflag
         MOVE      NORDFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
.begin patch 1.8
         move      lrn to nshpfld
         move      no to shipsw
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         move      olnum to ndatfld
         call      ndatkey
         call      wipecvars
.end patch 1.8
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE

.......................................................................

.BEGIN PATCH 2.41
PRINT     
//Patch 2.53 May not be a need for row count
.	  if       (row > 7100)
.          	call      Header
.          endif
//Patch 2.53          
.PRINT    COMPARE   "46" TO LINES
.         CALL      HEADER IF not less
.END PATCH 2.41
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        equal
         move      b1 to innets
         else
         move      "*" to innets
         endif
.Start Patch #1.7 - remmed and replaced logic
         MOVE      "99/99/99" TO DATEPRT1
.         EDIT      SLSMDTE  TO DATEPRT1
         MOVE      "99/99/99" TO DATEPRT1
.Start patch #1.7 - increased var
.         EDIT      STEMLDDT TO DATEPRT1
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsMDTe TO cc,str2,str4
         pack       str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
.End patch #3.1 - increased var
.End Patch #1.7 - remmed and replaced logic
.START PATCH #1.7 - INCREASED VAR
.         MOVE      PPM TO FORM72
.         MOVE      MASK32 TO M$PPM
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
         MOVE      PPM TO CMPT92
         MOVE      MASK32 TO M$PPM
.begin patch 1.8
.        DIVIDE    HUND INTO CMPT92
.end patch 1.8
        MOVE      CMPT92 TO FORM32
.END PATCH #1.7 - INCREASED VAR
         EDIT      FORM32 TO M$PPM
.begin patch 1.8
.        MOVE      QTYSHP TO FORM7
.         MOVE      MASK7 TO M$QTY
.         EDIT      FORM7 TO M$QTY
        MOVE      QTYbild TO FORM9
         MOVE      MASK9 TO M$QTY
         EDIT      FORM9 TO M$QTY
.End patch 1.8
.Start patch #1.7 - remmed and replaced logic to handle OQTY increase
.         MOVE      oQTY TO FORM7
.         MOVE      MASK7 TO M$oQTY
.         EDIT      FORM7 TO M$oQTY
         MOVE      oQTY TO FORM9
         MOVE      MASK9 TO M$oQTY
         EDIT      FORM9 TO M$oQTY
.End patch #1.7 - remmed and replaced logic to handle OQTY increase
         move        c0 to arform
         move        slsar to arform
         branch    pass of dt1,dt2,dt3,dt4
//.............................................................................
//dt1 - new billing detail print section |
//........................................
dt1                                                                        
        clear      str40
        move       mcomp to str40
.Begin  patch 2.41
.        cmatch     yes to exflag
.        If         equal
.        	call       exprint
.		add        c1 to lines
.		goto       readsls
.        Endif
//Patch 2.53 Comment Out
//        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
//                              *p=2125:row,*font=prtpg85,slslr:
//                              *p=2690:row,Invnum:
//                              *p=3195:row,DatePrt1:
//                              *p=5750:row,*Alignment=*right,M$Oqty:
//                              *p=6525:row,M$qty:
//                              *p=6850:row,"@":
//                              *p=7250:row,m$ppm,*Alignment=*Left
//Patch 2.53
	Add C1 to CurCellNum
	Add C1 to CellRowCnt
	If (Page = C1)	
		If (CellRowCnt > 54)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif
	Else
		If (CellRowCnt > 46)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif	
	Endif
	Move CurCellNum,Str5
	call trim using str5
	Pack Cell,"A",Str5
        setprop sheet.range(Cell),*Value=str40                      
	Pack Cell,"B",Str5        
        setprop sheet.range(Cell),*Value=slslr,*HorizontalAlignment=AlignCenter                          
	Pack Cell,"C",Str5
        setprop sheet.range(Cell),*Value=Invnum,*HorizontalAlignment=AlignCenter              
	Pack Cell,"D",Str5
        setprop sheet.range(Cell),*Value=DatePrt1,*HorizontalAlignment=AlignRight   
//Payment Part        
//        setprop sheet.range("F17"),*Value="Check Date",*HorizontalAlignment=AlignLeft           
//        setprop sheet.range("G17"),*Value="Chk. Number",*HorizontalAlignment=AlignLeft                   

//Adjustment Part
//	Pack Cell,"H",CurCellNum	
//        setprop sheet.range(Cell),*Value="Adjustment Description",*HorizontalAlignment=AlignLeft   
//        sheet.range(Cell).Columns.Autofit               
	Call Trim Using M$Oqty
	Pack Cell,"F",str5	
        setprop sheet.range(Cell),*Value=M$Oqty,*HorizontalAlignment=AlignRight               
	Pack Cell,"G",str5
        setprop sheet.range(Cell),*Value=M$qty,*HorizontalAlignment=AlignRight                       
	Pack Cell,"H",str5
        setprop sheet.range(Cell),*Value=m$ppm,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"         
        
        
//	Pack Cell,"L",CurCellNum	
//        setprop sheet.range(Cell),*Value="List",*HorizontalAlignment=AlignRight           
//	Pack Cell,"M",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Select",*HorizontalAlignment=AlignRight                 
//	Pack Cell,"N",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Billing",*HorizontalAlignment=AlignRight                      
//	Pack Cell,"O",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Comm",*HorizontalAlignment=AlignRight                  
//	Pack Cell,"P",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Comm",*HorizontalAlignment=AlignRight           
//	Pack Cell,"Q",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Select",*HorizontalAlignment=AlignRight             
//Special Part End        
//	Pack Cell,"R",CurCellNum	        
//        setprop sheet.range(Cell),*Value="Mlr Tax",*HorizontalAlignment=AlignRight                      



//        setprop sheet.range("A16","R20").Font,*Bold="True"
//        sheet.range("A16","R17").BorderAround using *LineStyle=1,*Weight=4
//        sheet.range("E17").Columns.Autofit       
//        sheet.range("F17").Columns.Autofit    
//        sheet.range("G17").Columns.Autofit            
//        sheet.range("H17").Columns.Autofit            

//
.        PRINT     *3,hpdtch70,hpuprght,str40,*flush:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1:
.                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm;
         Match	YES TO AP2SW
	 If	EQUAL
//Patch 2.53 New Logic
		Pack Cell,"E",str5     
         	Call Trim Using AP2OUT
		Setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic		
//         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
.         PRINT     hpt400,AP2OUT;
.End patch 2.41
		Move	slsap2 to aplotus
.Begin patch 1.8
.         	  Div     hund into aplotus
.End patch 1.8
		Add	SLSAP2 TO APTOTOWN
         Else
		Compare	C0 TO PAYCHK
		If	Equal
.Begin  patch 2.41
//Patch 2.53 Comment Out
//         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
		Pack Cell,"E",str5       	 
		setprop sheet.range(Cell),*Value=AP1Out,*HorizontalAlignment=AlignRight               
//Patch 2.53 New Logic			
.		        Print	hpt400,AP1OUT;
.End  patch 2.41
			Move	slsap1 to aplotus
.Begin patch 1.8
.			Div	hund into aplotus
.End patch 1.8
.			Move	slsap1 to aplotus
			Add	SLSAP1 TO APTOTOWN
.Begin  patch 2.41
			If	(exflag <> Yes)
				Add       SLSAP1,APTOTwnr
			Else
				Add       SLSAP1,APTOTwnx
			Endif
.			Add       SLSAP1 TO APTOTwnr
.End Patch 2.41
		Else
.Begin Patch 2.41
//Patch 2.53 Comment Out
//          PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
//Patch 2.53 Comment Out
//Patch 2.53 New Logic
			Pack Cell,"E",str5   
	         	Call Trim Using AP2OUT
			setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic			
.       		PRINT     hpt400,AP2OUT;
.End Patch 2.41
         		Move      slsap2 to aplotus
.Begin Patch 1.8
.         		div       hund into aplotus
.End Patch 1.8
.         		move      slsap2 to aplotus
         		Add       SLSAP2 TO APTOTOWN
.Begin Patch 2.41
                  	If (exflag <> Yes)
                   		ADD	SLSAP2 TO APTOTwnr
                   	Else
                   		ADD	SLSAP2 TO APTOTwnx
                   	Endif
..         ADD       SLSAP2 TO APTOTwnr
.End Patch 2.41
         	Endif
         Endif
         Cmatch    yes to flatflg
         If        Equal
.Begin  patch 1.8
.        WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
.                     comma,slslr:
.                     comma,invnum:
.                     comma,dateprt1:
.                     comma,oqty:
.                     comma,qtyshp:
.                     comma,m$ppm:
.                     comma,aplotus:
.                     comma,taxprt:
.                     comma,holdlst1
		   WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,oqty:                    
                      comma,qtybild:
                      comma,m$ppm:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1
.End Patch 1.8
         Endif
         CMatch      "T" to hldarflg
         If     Equal
         	Move armask to arout
         	Edit arform TO AROUT
.Begin Patch 2.41
.	        PRINT       hpt825,arout:
               	If   (exflag = Yes)      
.	        cmatch     yes to exflag
			call trim using ARout
			Pack Cell,"I",str5	        
	        	Setprop sheet.range(Cell),*Value=ARout,*HorizontalAlignment=AlignRight                     
			Pack Cell,"Q",str5	        
	        	Setprop sheet.range(Cell),*Value="Exchange List Recovery",*HorizontalAlignment=AlignLeft                    
	        	
//                              PrtPage	Laser;*font=prtpg85:
//                                             *p=8125:Row,*Alignment=*Right,ARout:
//                                             *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
//                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
		Else
			call trim using ARout		
			Pack Cell,"I",str5	        
	        	Setprop sheet.range(Cell),*Value=ARout,*HorizontalAlignment=AlignRight                     		
//                              PrtPage	Laser;*font=prtpg85:
//                                             *p=8125:Row,*Alignment=*Right,ARout:
//                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
		Endif
.                                             PRINT       hpt850,hpdtch85,hpuprght,arout:
.			                      hpdtch85,hpuprght,hpt950,TAXPRT
         Else
.Begin patch 2.41
               If       (exflag = Yes)      
.              cmatch     yes to exflag
//               		PrtPage Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left:
//                                 *p=8500:row,*Alignment=*Left,"Exchange List Recovery"
			Pack Cell,"Q",str5	        
	        	Setprop sheet.range(Cell),*Value="Exchange List Recovery",*HorizontalAlignment=AlignLeft                                                      
               Else
//               		PrtPage Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
               Endif
.              PRINT       hpt825:
.              hpt950,TAXPRT
         Endif
//Patch 2.53
	 Pack Cell,"R",str5	        
         Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight  
//Patch 2.53
.        Add   c1 to lines
	 Add   "200" to Row
.End Patch 2.41
	 Goto      readsls
//.............................................................................
//.dt4 - detail print for previously billed/open invoices |
//.........................................................
dt4
        clear      str40
        move       mcomp to str40
.Begin patch 2.41
.        cmatch     yes to exflag
.        if         equal
.        call       exprint
.        add        c1 to lines
.        goto       readsls
.        endif
//Patch 2.53 Comment Out
//        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
//                              *p=2125:row,*Alignment=*Left,*font=prtpg85,slslr:
//                              *p=2690:row,Invnum:
//                              *p=3195:row,DatePrt1:
//                              *p=5750:row,*Alignment=*right,M$Oqty:
//                              *p=6525:row,M$qty:
//                              *p=6850:row,"@":
//                              *p=7250:row,m$ppm,*Alignment=*Left
//Patch 2.53 Comment Out End
//Patch 2.53 New Logic
	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	If (Page = C1)	
		If (CellRowCnt > 54)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif
	Else
		If (CellRowCnt > 46)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif	
	Endif
	Move CurcellNum to str5
	Call Trim Using Str5
	Pack Cell,"A",str5
        setprop sheet.range(Cell),*Value=str40                      
	Pack Cell,"B",str5      
        setprop sheet.range(Cell),*Value=slslr              
	Pack Cell,"C",str5       
        setprop sheet.range(Cell),*Value=Invnum
	Pack Cell,"D",str5       
        setprop sheet.range(Cell),*Value=DatePrt1,*HorizontalAlignment=AlignRight   
//Patch 2.53 New Logic        
//Payment Part        
//        setprop sheet.range("F17"),*Value="Check Date",*HorizontalAlignment=AlignLeft           
//        setprop sheet.range("G17"),*Value="Chk. Number",*HorizontalAlignment=AlignLeft                   

//Adjustment Part
//	Pack Cell,"H",CurCellNum	
//        setprop sheet.range(Cell),*Value="Adjustment Description",*HorizontalAlignment=AlignLeft   
//        sheet.range(Cell).Columns.Autofit               
	Call Trim Using M$Oqty
	Pack Cell,"F",str5	
        setprop sheet.range(Cell),*Value=M$Oqty,*HorizontalAlignment=AlignRight               
	Pack Cell,"G",str5	
        setprop sheet.range(Cell),*Value=M$qty,*HorizontalAlignment=AlignRight                       

	Pack Cell,"H",str5	
        setprop sheet.range(Cell),*Value=m$ppm,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"          
//Patch 2.53
                              
                              
.         PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1;
.per sa/lp 25mar96 DLH
..                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm;
         Match	YES TO AP2SW
         If	EQUAL
		ADD	SLSAP2 TO APTOTOWN
		If (exflag <> Yes)
			ADD	SLSAP2 TO APTOTwnr
		Else
			ADD	SLSAP2 TO APTOTwnx
		Endif
//Patch 2.53 New Logic
		Pack Cell,"E",str5        	 
         	Call Trim Using AP2OUT		
		Setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        

//Patch 2.53 New Logic                   
//Patch 2.53
//		PrtPage	Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
//Patch 2.53		
.         ADD     SLSAP2 TO APTOTwnr
.         PRINT     hpt400,AP2OUT;
.End patch 2.41
		Move	slsap2 to aplotus
.Begin patch 1.8
.         div       hund into aplotus
.End patch 1.8
.         move     slsap2 to aplotus
	Else
		COMPARE   C0 TO PAYCHK
		If	EQUAL
			ADD	SLSAP1 TO APTOTOWN
.Begin patch 2.41
.         ADD       SLSAP1 TO APTOTWNr
.         PRINT        hpt400,AP1OUT;
			If (exflag <> Yes)
				Add       SLSAP1 TO APTOTwnr
			Else
				Add       SLSAP1 TO APTOTwnx
			Endif
//Patch 2.53 New Logic
			Pack Cell,"E",str5        	 
			Setprop sheet.range(Cell),*Value=AP1Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out
.		PrtPage	Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
//Patch 2.53 Comment Out End         
.End patch 2.41
			Move	slsap1 to aplotus
.Begin patch 1.8
.         div       hund into aplotus
.End patch 1.8
.         move      slsap1 to aplotus
		Else
			Add	SLSAP2 TO APTOTOWN
.Begin patch 2.41
.         ADD       SLSAP2 TO APTOTWNr
			If (exflag <> Yes)
				Add	SLSAP2 TO APTOTwnr
			Else
				Add	SLSAP2 TO APTOTwnx
			Endif
//Patch 2.53 New Logic
			Pack Cell,"E",str5        	 
         		Call Trim Using AP2OUT			
			Setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out                   
//        PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
//Patch 2.53 Comment Out        
.		PRINT     hpt400,AP2OUT;
.End patch 2.41
			Move      slsap2 to aplotus
.Begin patch 1.8
.		Div       hund into aplotus
.End patch 1.8
.		Move     slsap2 to aplotus
		Endif
	Endif
	Cmatch  yes to flatflg
	If	Equal
		WRITE     OUTPUT,SEQ;B5,comma,"O",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1
	Endif
	Cmatch      "T" to hldarflg
	If	equal
		Move	armask to arout
		EDIT	arform TO AROUT
.Begin patch 2.41
		If	(exflag = Yes)      ;        cmatch     yes to exflag
//Patch 2.53 New Logic				
			call trim using ARout
			Pack Cell,"I",str5	        
	        	Setprop sheet.range(Cell),*Value=ARout,*HorizontalAlignment=AlignRight                     
			Pack Cell,"Q",str5	        
	        	Setprop sheet.range(Cell),*Value="Exchange List Recovery",*HorizontalAlignment=AlignLeft  		
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out    	        	
//			PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85:
//                          *p=8125:Row,*Alignment=*Right,Arout:
//                          *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
//                          *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                          
                          
		Else
//Patch 2.53 New Logic		
			call trim using ARout
			Pack Cell,"I",str5	        
	        	Setprop sheet.range(Cell),*Value=ARout,*HorizontalAlignment=AlignRight                     
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out 		
//			PrtPage     Laser;*font=prtpg85:
//                                             *p=8125:Row,*Alignment=*Right,ARout:
//                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out 
		Endif
.                      PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
	Else
		If             (exflag = Yes)      ;        cmatch     yes to exflag
			Pack Cell,"Q",str5	        
	        	Setprop sheet.range(Cell),*Value="Exchange List Recovery",*HorizontalAlignment=AlignLeft 		
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out  		
//			PrtPage	Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left:
//        	                         *p=8500:row,*Alignment=*Left,"Exchange List Recovery"
//Patch 2.53 Comment Out  	        	
//		Else
//			PrtPage	Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left	
		Endif
//Patch 2.53 Comment Out  				
//Patch 2.53 New Logic
.	 Pack Cell,"R",str5	        
.         Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight  
//Patch 2.53 New Logic
.		PRINT       hpt875:
.       hpt950,TAXPRT
	Endif
//Patch 2.53 New Logic
	 Pack Cell,"R",str5	        
         Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight  
//Patch 2.53 New Logic	
	add            "200" to row
;         add       c1 to lines
;end patch 2.41
         goto      readsls
;.............................................................................
;dt2 - detail print section for new payments made.|
;..................................................
dt2 
        Match      no to apsw
        Goto       prt3a2 if equal
        Clear      str40
        Move       mcomp to str40
.Begin patch 2.41
        Add	"200" to row
//Patch 2.53 New Logic
	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	If (Page = C1)	
		If (CellRowCnt > 54)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif
	Else
		If (CellRowCnt > 46)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif	
	Endif	
	Move CurCellNum,Str5
	call trim using str5
	Pack Cell,"A",str5
        setprop sheet.range(Cell),*Value=str40                      
	Pack Cell,"B",str5     
        setprop sheet.range(Cell),*Value=slslr              
	Pack Cell,"C",str5   
        setprop sheet.range(Cell),*Value=Invnum
	Pack Cell,"D",str5        
        setprop sheet.range(Cell),*Value=DatePrt1,*HorizontalAlignment=AlignRight   
//Patch 2.53 New Logic          
//Patch 2.53 Comment Out        
//        PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
//                   *p=2125:row,*font=prtpg85,slslr:
//                   *p=2690:row,Invnum:
//                   *p=3195:row,DatePrt1
//Patch 2.53 Comment Out End                   
.         PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1;
.End patch 2.41
GOODONE2
         MATCH     YES TO AP2SW
         If        EQUAL
.         print     hpt400,ap2out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
//Patch 2.53 New Logic
			Pack Cell,"E",str5       	 
         		Call Trim Using AP2OUT			
			Setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out
.Begin Patch 2.41
//          PrtPage	Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
//Patch 2.53 Comment Out          
.         PRINT     hpt400,AP2OUT;
.End Patch 2.41
.         print     hpt400,ap2out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
         	MOVE	CHKN2 TO CHKN1
         	Move	slsap2 to aplotus
.Begin patch 1.8
.         div       hund into aplotus
.End patch 1.8
.         move      slsap2 to aplotus
         Else
.         print     hpt400,ap1out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.Begin patch 2.41
//Patch 2.53 New Logic
		Pack Cell,"E",str5       	 
		Setprop sheet.range(Cell),*Value=AP1Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out
//		PrtPage	Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
//Patch 2.53 Comment Out		
.         PRINT     hpt400,AP1OUT;
.End patch 2.41
		Move	slsap1 to aplotus
.Begin patch 1.8
.         div       hund into aplotus
.End patch 1.8
.         move      slsap1 to aplotus
         Endif
	 Move	armask to arout
	 Add	arform to artotlst
	 Edit	arform TO AROUT
//Patch 2.53 New Logic
		Pack Cell,"J",str5       	 
		Setprop sheet.range(Cell),*Value=ChkDate,*HorizontalAlignment=AlignRight                        
		Pack Cell,"K",str5       	 		
		Setprop sheet.range(Cell),*Value=Chkn1,*HorizontalAlignment=AlignRight                        		
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out	 
//	 PrtPage        Laser;*p=7500:row,*Alignment=*Left,ChkDate:
//                        *p=8600:row,*Alignment=*Right,chkn1,*Alignment=*Left
//Patch 2.53 Comment Out                        
.        PRINT       hpt750,CHKDATE,b1,chkn1;                    CHKN1;
	 Cmatch      "T" to hldarflg
	 If	Equal
//Patch 2.53 New Logic
		Pack Cell,"J",str5       	 
		Setprop sheet.range(Cell),*Value=ChkDate,*HorizontalAlignment=AlignRight                        
		Pack Cell,"K",str5  		
		Setprop sheet.range(Cell),*Value=Chkn1,*HorizontalAlignment=AlignRight                        		
//Patch 2.53 New Logic			
//Patch 2.53 Comment Out	 
.Begin patch 2.41
//         	PrtPage        Laser;*p=7500:row,*Alignment=*Left,ChkDate:
//                        *p=8600:row,*Alignment=*Right,chkn1,*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out                        
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
         Else
//Patch 2.53 Comment Out         
//         	PrtPage             Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out         	
//Patch 2.53 New Logic
.	 Pack Cell,"R",str5	        
.         Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight  
//Patch 2.53 New Logic         	
.         PRINT       hpt875:
.                      hpt950,TAXPRT
.End patch 2.41
         Endif
//Patch 2.53 New Logic
	 Pack Cell,"R",str5	        
         Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight  
//Patch 2.53 New Logic           
         
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,chkdate:
                      comma,chkn1
         endif
prt3a2
.BEGIN PATCH 2.41
.ADD       C1 TO lines
.END PATCH 2.41
         MOVE      C1 TO JSTN
         MOVE      SLSLR TO NINVFLD
         CALL      NINVKEY
DETADJ2  
         PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      readsls IF OVER
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
	         ADD	C1 TO JSTN
	         GOTO	DETADJ2
         ENDIF         
.Begin patch 1.9
.        move       c0 to cvtfld
.        move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        add        form92 to aptotown            *new
.        mult       seq by form92
.        div        hund into form92
         move       c0 to adjap
.        add        form92 to adjap
         add        jstap1 to adjap
         add        jstap1 to aptotown
.End patch 1.9

         move      adjapmsk to adjapout
         move      adjap to aplotus
.        div       hund into aplotus
.        move      adjap to aplotus
	 Edit       adjap to adjapout
         CMATCH     YES TO AP2SW
         If          EQUAL
	        GOTO       CONVAP2
         Endif
         COMPARE    C0 TO ADJAP
         IF         NOT EQUAL
	        ADD        ADJAP TO MANPAY
	        move       adjap to aplist
	        mult       hund into aplist
	        add        aplist to aptotlst
         ENDIF
CONVAP2
.Begin patch 1.9
.       MOVE       C0 TO CVTFLD
.        move       jstap2 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        add        form92 to aptotown            *new
.        div        hund into form92
.        mult       seq by form92
..START PATCH #1.7 - INCREASED VAR
..        move       c0 to form72
..        add        form92 to form72
..        move       apmask to ap2out
..        edit       form72 to ap2out
..         move      form72 to aplotus
..         div       hund into aplotus
...        move       form72 to aplotus
..        cmatch     yes to ap2sw
..        if         equal
..        compare    c0 to form72
..        goto       readsls if equal
..        endif
..        ADD        FORM72 TO MANPAY
..        move       c0 to adjap
..        add        form72 to adjap
...
         move       c0 to CMPT92
.        add        form92 to CMPT92
         add        jstap2 to cmpt92
.End patch 1.9

        move       apmask2 to ap2out    .APMASK2 - NEW VAR WHICH WILL HOLD NEW VALUE OF CMPT92
        edit       CMPT92 to ap2out
         move      CMPT92 to aplotus
.         div       hund into aplotus
.        move       CMPT92 to aplotus
        cmatch     yes to ap2sw
        if         equal
        	compare    c0 to CMPT92
        	goto       readsls if equal
        endif
        ADD        CMPT92 TO MANPAY
        move       c0 to adjap
        add        CMPT92 to adjap    
.END PATCH #1.7 - INCREASED VAR
        COMPARE    C0 TO ADJAP
        IF         NOT EQUAL
        	move       adjap to aplist
        	mult       hund into aplist
        	add        aplist to aptotlst
        ENDIF
.Begin patch 2.41
//Patch 2.53 May not be a need for row count
.         if        (row > 7100)
.         	call       Header
.         endif
//Patch 2.53         
.        COMPARE   "44" to    
.         CALL      HEADer IF NOT LESS
              add            "200" to row
//Patch 2.53 New Logic
	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	If (Page = C1)	
		If (CellRowCnt > 54)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif
	Else
		If (CellRowCnt > 46)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif	
	Endif
	move CurCellNum,Str5
	Call Trim Using Str5
	Pack Cell,"A",str5
        setprop sheet.range(Cell),*Value=str40                      
	Pack Cell,"B",str5      
        setprop sheet.range(Cell),*Value=slslr              
	Pack Cell,"C",str5      
        setprop sheet.range(Cell),*Value=Invnum
	Pack Cell,"D",str5        
        setprop sheet.range(Cell),*Value=DatePrt1,*HorizontalAlignment=AlignRight   
//Patch 2.53 New Logic          
//Patch 2.53 Comment Out                
//          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
//                   *p=2000:row,*Alignment=*right,*font=prtpg85,slslr:
//                   *p=2500:row,Invnum:
//                   *p=3200:row,DatePrt1,*Alignment=*Left
//Patch 2.53 Comment Out                     
.         PRINT     *3,hpdtch85,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1;
.end patch 2.41
.START PATCH #1.7 - INCREASED VAR
.         COMPARE   C0 TO form72
         COMPARE   C0 TO CMPT92
.END PATCH #1.7 - INCREASED VAR
         IF        NOT EQUAL
.Begin patch 2.41
//Patch 2.53 Comment Out
//         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
		Pack Cell,"E",str5
         	Call Trim Using AP2OUT		
		Setprop sheet.range(Cell),*Value=AP2Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	         
.         PRINT     hpt400,AP2OUT;
.End patch 2.41
	         MOVE      "MANUAL" TO CHKN1
         ELSE
.         print     hpt400,ap1out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.Begin patch 2.41
//Patch 2.53 Comment Out         
//         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
		Pack Cell,"E",str5       	 
		Setprop sheet.range(Cell),*Value=AP1Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	                  
.         PRINT     hpt400,AP1OUT;
.End patch 2.41
         ENDIF
         MOVE      "MANUAL" TO CHKN1
         UNPACK    JSTDATE INTO MM,DD,YY
         PACK      CHKDATE FROM MM,SLASH,DD,SLASH,YY
         move        c0 to arform
         move        slsar to arform
         move        armask to arout
         add         arform to artotlst
         EDIT        arform TO AROUT
         PRINT       hpt750,CHKDATE,b1,CHKN1;
         cmatch      "T" to hldarflg
	 If          Equal
.Begin patch 2.41
//Patch 2.53 Comment Out         
//         	PrtPage	Laser;*p=8750:row,*Alignment=*Right,*font=prtpg85,arout:
//                              *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
		Pack Cell,"E",str5        	 
		Setprop sheet.range(Cell),*Value=AP1Out,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	                               
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
//Patch 2.53 Comment Out         
//	 Else
//		PrtPage	Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out         		
.       PRINT       hpt875:
.                     hpt950,TAXPRT
.End patch 2.41
         Endif
         
//Patch 2.53 New Logic
	Pack Cell,"R",str5       	 
	Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	         
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,chkdate:
                      comma,chkn1
         endif
;Begin patch 2.41
         add       "200" to row
.         ADD       C1 TO lines
.End patch 2.41
         GOTO      readsls
.
.............................................................
.DT3  - ADJUSTMENTS FOR THE MONTH      |
........................................
dt3
        clear      str40
        move       mcomp to str40
.Begin patch 2.41
//Patch 2.53 Comment Out  
//        PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
//                   *p=2125:row,*Alignment=*left,*font=prtpg85,jstlr:
//                   *p=2690:row,Invnum:
//                   *p=3195:row,DatePrt1:
//                   *p=7500:row,*Alignment=*left,nadjtext
//Patch 2.53 Comment Out                     
//Patch 2.53 New Logic
	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	If (Page = C1)	
		If (CellRowCnt > 54)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif
	Else
		If (CellRowCnt > 46)
			Call Header
			Add C1 to CurCellNum
			Add C1 to CellRowCnt		
				
		Endif	
	Endif	
	Move CurCellNum,Str5
	call trim using str5
	Pack Cell,"A",str5
        setprop sheet.range(Cell),*Value=str40                      
	Pack Cell,"B",str5
        setprop sheet.range(Cell),*Value=slslr              
	Pack Cell,"C",str5
        setprop sheet.range(Cell),*Value=Invnum
	Pack Cell,"D",str5
        setprop sheet.range(Cell),*Value=DatePrt1,*HorizontalAlignment=AlignRight   
	Pack Cell,"L",str5
        setprop sheet.range(Cell),*Value=Nadjtext,*HorizontalAlignment=AlignLeft  
//Patch 2.53 New Logic          
                  
.         PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,jstlr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1:
.                   hpdtch85,hpuprght,hpt750,nadjtext,hpdtch85,hpuprght;
.end patch 2.41
.                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm:
.                   hpdtch85,hpuprght,hpt750,nadjtext,hpdtch85,hpuprght;
         MATCH     YES TO AP2SW
         IF        EQUAL
.         MOVE    apmask TO AP2OUT
.         edit    slsap2 to ap2out
.begin patch 2.41
//Patch 2.53 Comment Out         
//               PrtPage       Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,Adjap2,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
		move adjap2,str10
		call trim using str10
		Pack Cell,"E",str5      	 
		Setprop sheet.range(Cell),*Value=str10,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	                                  
.         PRINT     hpt400,Adjap2
.end patch 2.41
         	Move     adjap2 TO APlotus
.         move      slsap2 to aplotus
.         move     c0 to form92
.         move     adjap2 to form92
.         mult     "100" by form92
.         ADD     form92 TO APTOTOWN
         	Add     SLSAP2 TO APTOTOWN
	Else
		COMPARE   C0 TO PAYCHK
		IF	EQUAL
.         move     c0 to form92
.         move     adjap1 to form92
.         mult     "100" by form92
.begin patch 1.9
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92         *added 7/24/98 jd

.        add        form92 TO APTOTOWN
			add	jstap1 to aptotown
.end patch 1.9

.        ADD     SLSAP1 TO APTOTOWN        *turned off 7/24/98 jd
.         MOVE    apmask TO AP1OUT
.         edit    slsap1 to ap1out
.begin patch 2.41
//Patch 2.53 Comment Out         
//	         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,ADjAPOUT,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
			call trim using ADJAPOUT
			Pack Cell,"E",str5       	 
			Setprop sheet.range(Cell),*Value=ADjAPOUT,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	         
.         PRINT     hpt400,AdjAPOUT;
.end patch 2.41
			move	adjap1 TO APlotus
.         move     slsap1 to aplotus
		Else
.         PRINT        hpt400,AP1OUT;
.         ADD     SLSAP2 TO APTOTOWN
	        Endif
         Endif
.BEGIN patch 2.41
//Patch 2.53 Comment Out         
//               PrtPage               Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
//Patch 2.53 Comment Out         
//Patch 2.53 New Logic
	Pack Cell,"R",str5       	 
	Setprop sheet.range(Cell),*Value=TaxPrt,*HorizontalAlignment=AlignRight                        
//Patch 2.53 New Logic	                 
.         PRINT       hpt875:
.                      hpt950,TAXPRT
.end patch 2.41
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"A",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,nadjtext
         endif
;BEGIN PATCH 2.41
         add       "200" to row
;         add       c1 to lines
;END PATCH 2.41
         goto      readsls
;............................................................................
HEADER
         ADD       C1 TO PAGE
         branch    pass of hd1a,hd2a,hd3a,hd4a 
*............................................................
hd1a     
.BEGIN PATCH 2.41
   	  Move           "1300" to row
.         MOVE      c10 TO LINES
.         MOVE      c5 TO LINES
.Begin patch 2.0
.         PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"New billing for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
//Patch 2.53
//	  PrtPage        Laser;*newpage
//	  
.         PRINT     *f;  
//Patch 2.53 Comment Out
.	  Call      Prtform
.          Call      PrtOwner
.          Call      PrtsubHead
//Patch 2.53 Repalcement Code

          Call      PrtOwner
	  Call      Prtform
.         Call      PrtsubHead
          
//Patch 2.53 Repalcement Code End
          
//                PrtPage       Laser;*alignment=*right,*p=6875:725,"New Billing for List:",*Alignment=*Left

.        print      033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"New billings for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.End PATCH 2.41
.End patch 2.0
         CMatch    "T" TO HLDARFLG
         If        EQUAL
.Begin release 2.41
//Patch 2.53 Comment Out
//               PrtPage        Laser;*p=8125:1235,*Alignment=*Right,*font=prtpg9bi,"Gross":
//                              *p=8125:1355,*font=prtpg9bi,"Billed",*Alignment=*Left
//Patch 2.53 Comment Out End                              
.               PRINT     HPT900,"Billed":
.                   hpboff,hpuprght,*flush;
.End release 2.41
.         else
.        print     hpboff,hpuprght,*flush;
.End release 2.41
         Endif
         Compare	c1 to page
         If	Equal
         	Move	lobbal To form102
         	Mult	"100" By form102
         	Move	form102 To aptotown
         	Move    TOTOMSK TO OWNERMSK
         	Edit    APTOTOWN TO OWNERMSK
         	Move    Form102 to aplotown
.	        Div       hund by aplotown
.         	Move      c0 to aptotown
.Begin patch 2.41
                Move	"1625" to row
//Patch 2.53
		Move BalanceRow,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
        	setprop sheet.range(Cell),*Value="Opening Balance:"                              
        	setprop sheet.range(cell).Font,*Bold="True"            	
		Pack Cell,"E",str5        	
        	setprop sheet.range(Cell),*Value=Ownermsk           
        	setprop sheet.range(cell).Font,*Bold="True"            	
        	
//Patch 2.53
//Patch 2.53               
//               PrtPage	laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
//                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
//Patch 2.53                              
               Add	"400" to row
//Patch 2.53               
//               PrtPage	laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
//Patch 2.53               
.              Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk,*n,*n,*3,"New Billing:":
.                   hpdtch85,hpuprght,*flush
.	       Add       c5 to lines
               Add	"200" to Row              s
.End patch 2.41
         	Cmatch	yes to flatflg
         	If	Equal
			WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
         	             comma,b6:                         
         	             comma,b6:                         
         	             comma,b8:                      
         	             comma,b9:                    
         	             comma,b9:                    
         	             comma,b4:                    
         	             comma,aplotown:
         	             comma,b7,comma,holdlst1
         	Endif
         	Move      c0 to aptotown
         	Move      c0 to aptotwnx
         	Move      c0 to aptotwnr
         Endif
.
         compare   c1 to page
         If	Not Equal
.Begin patch 2.41
		Add	"400" to row
//Patch 2.53		
//		PrtPage        laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
//Patch 2.53		
.         	Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"New Billing: ":
.                   hpdtch85,hpuprght,*flush
		Add            "200" to row
.         	Add       c3 to lines
.End patch 2.41
         Endif
         RETURN
*............................................................
hd4a 
.BEGIN PATCH 2.41
               Move           "1300" to row
.         MOVE      c10 TO LINES
.Begin patch 2.0
.          PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"Open Invoices as of end of month:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.          print     *f;
//Patch 2.53 Comment Out
.         PRtPage   Laser;*Newpage
.         Call      prtform
.         Call      PrtOwner
.         Call      PrtsubHead
//Patch 2.53 Comment Out         
//Patch 2.53 Replacement Code
          Call      PrtOwner
          Call      Prtform
//Patch 2.53 Replacement Code
//       	 PrtPage       Laser;*alignment=*right,*p=6875:725,"Current Open Invoices:",*alignment=*Left
//               add            "200" to row
.          PRINT     033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"Open Invoices as of end of month:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.End patch 2.0
.         CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         PRINT     HPT900,"Billed":
.                   hpboff,hpuprght,*flush;
.         else
.         print     hpboff,hpuprght,*flush;
.         endif
.End patch 2.41
.Begin release 2.41
	  CMATCH	"T" TO HLDARFLG
	  IF	EQUAL
//Patch 2.53	  
//		PrtPage        Laser;*p=8125:1235,*Alignment=*Right,*font=prtpg9bi,"Gross":
//                              *p=8125:1355,*font=prtpg9bi,"Billed",*Alignment=*Left
//Patch 2.53                              
.		PRINT     HPT900,"Billed":
.		hpboff,hpuprght,*flush;
	  Endif
.End Release 2.41
          Compare   c0 to counta
          If	equal
         	Compare   c1 to page
          	If        equal 
			Move      lobbal to form102
			Mult      "100" by form102
			Move      form102 to aptotown
		        Move      TOTOMSK TO OWNERMSK
			Edit      APTOTOWN TO OWNERMSK
			Move      form102 to aplotown
.         		div       hund by aplotown
.         		move      c0 to aptotown
.Begin Patch 2.41
	               Move           "1625" to row
//Patch 2.53               
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
//                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
//Patch 2.53                              
//Patch 2.53
			Move BalanceRow,str5
			Call Trim Using Str5
			Pack Cell,"A",str5
	        	setprop sheet.range(Cell),*Value="Opening Balance:"   
        		setprop sheet.range(cell).Font,*Bold="True"            		        	
			Pack Cell,"E",str5        	
	        	setprop sheet.range(Cell),*Value=Ownermsk  
        		setprop sheet.range(cell).Font,*Bold="True"            		        	
	        	Add C5,CurCellNum
			Add C5 to CellRowCnt	        	
			Move CurCellNum,str5
			Call Trim Using Str5
			Pack Cell,"A",str5
        		setprop sheet.range(Cell),*Value="No new Billing this month."                            
	        	

//Patch 2.53
               add            "500" to row
//Patch 2.53               
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
//Patch 2.53               
               add            "200" to row
.
.               Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"
.         add       c4 to lines
.End patch 2.41
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         endif
         endif

         compare   c0 to countb
         if        equal
         add        "500" to Row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
//Patch 2.53         
//Patch 2.53
	        Add C5,CurCellNum
		Add C5 to CellRowCnt	        
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		setprop sheet.range(Cell),*Value="No new Payments this month."    
//Patch 2.53
;         print      *n,*3,"No new Payments this month"
         add        "200" to Row
 ;        add       c2 to lines
         endif

         compare   c0 to countc
         if        equal
         add        "200" to Row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"No Adjustments to Payments this month.",*font=prtpg9
//Patch 2.53      
//Patch 2.53
	        Add C5,CurCellNum
		Add C5 to CellRowCnt	        
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		setprop sheet.range(Cell),*Value="No Adjustments to Payments this month."   
	        Add C5,CurCellNum
		Add C5 to CellRowCnt	               		
//Patch 2.53
         add        "400" to Row
.        add       c2 to lines
.         print     *n,*3,"No Adjustments to payment this month":
.                   hpdtch85,hpuprght,*flush
         endif

         add        "500" to Row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Open invoices as of end of month.",*font=prtpg9
//Patch 2.53  
//Patch 2.53
.	        Add C5,CurCellNum
.		Move CurCellNum,str5
.		Call Trim Using Str5
.		Pack Cell,"A",str5
.       		setprop sheet.range(Cell),*Value="Open invoices as of end of month."    
//Patch 2.53
         add        "200" to Row
.         Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Open invoices as of end of month : ":
.                   hpdtch85,hpuprght,*flush
         RETURN
*............................................................
hd2a
.Begin patch 2.41
               Move           "1300" to row
.         MOVE      c10 TO LINES
.End patch 2.41
.Begin patch 2.0
.          PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"New payments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon,"Check date & Number";
.Start Patch 2.13
           MATCH   YES TO AP2SW
           IF      EQUAL
           call    nownkey
           endif
. turned off 7/31/01 need to check out timing.
.         pack      nlobfld from slsown,slslist,DATEKEY
.         rep       zfill in nlobfld
.         display   *p1:24,*el,*b,"at prepok",nlobfld,*b,*w10
.         move      slsown to holdown
.         move      slslist to holdlist
.         move      slsown to nownfld
.         move      slslst1 to holdlst1
.         move      slslr to ninvfld
.         move      arflag to hldarflg
.         call      ninvkey
.         CALL      READPAY
.         move      c0 to lobbal
.                 call      nlobkey
.                 if        over
.                 move      "T" to newflag
.                 else
.                 move      "F" to newflag
.                 endif
.End Patch 2.13
.Begin patch 2.41
.         PRINT     *f;
//               prtPage        Laser;*newpage
                call      PrtOwner
                call      prtform                
//                
               
//                PrtPage       Laser;*alignment=*right,*p=6875:725,"New Payments for List:",*alignment=*Left:
//                              *p=7500:1300,*font=prtpg9bi,"Check date & Number"
//                              
.                print     033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"New payments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon,"Check date & Number";
.End patch 2.0
.         CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         PRINT     HPT900,"Billed":
.                   hpboff,hpuprght,*flush;
.         else
.         print     hpboff,hpuprght,*flush;
.         endif
.End patch 2.41

         compare   c0 to counta
         if        equal
         	move      lobbal to form102
         	mult      "100" by form102
         	move      form102 to aptotown
         	MOVE      TOTOMSK TO OWNERMSK
         	EDIT      APTOTOWN TO OWNERMSK
         	move      form102 to aplotown
//         	div       hund by aplotown
//        	move      c0 to aptotown
//begin patch 2.41
                Move           "1625" to row
//             add            "750" to row
//Patch 2.53
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
//                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
//Patch 2.53                  
//Patch 2.53
		Move BalanceRow,str5
		Call Trim Using Str5
.		Pack Cell,"A",str5
.        	setprop sheet.range(Cell),*Value="Opening Balance:"                              
.		Pack Cell,"E",str5        	
.        	setprop sheet.range(Cell),*Value=Ownermsk           
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="No new Billing this month."         
//Patch 2.53
               add            "500" to row
//Patch 2.53               
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
//Patch 2.53               
               add            "200" to Row
.         Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"
.         add       c4 to lines
.End patch 2.41
               cmatch    yes to flatflg
               if        equal
               		WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
               endif
               move      c0 to aptotown
               move      c0 to aptotwnx
               move      c0 to aptotwnr
         endif
.Begin patch 2.41 
         add            "500" to row
//Patch 2.53 Comment Out	
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"Payments during the month:",*font=prtpg9
//Patch 2.53 Comment Out             
         
.         print     *n,*n,*3,hpdtch85,hpuprght,"Payments during the month:",hpdtch85,hpuprght
.End patch 2.41 
         RETURN
.............................................................
hd3a
.Begin patch 2.41
	Move           "1300" to row
.         MOVE      c10 TO LINES
.End patch 2.41
.Begin patch 2.0
.         PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"Payment adjustments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.         print     hpboff,hpuprght,*flush;
.Start Patch 2.15
	MATCH   YES TO AP2SW
        IF      EQUAL
	        call    nownkey
        Endif
.End Patch 2.15
.Begin patch 2.41
//Patch 2.53
//               PrtPage        Laser;*Newpage
//Patch 2.53               
.              PRINT     *f;
         call       PrtOwner
         call       prtform         
//

//         PrtPage       Laser;*alignment=*right,*p=6875:725,"Payment adjustments for list:",*alignment=*Left
//         
.         print      033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"Payment adjustments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.         print     hpboff,hpuprght,*flush;

.End patch 2.41
.End patch 2.0
         compare   c0 to counta
         if        equal
         	compare   c1 to page
         	if        equal
         		move      lobbal to form102
         		mult      "100" by form102
         		move      form102 to aptotown
         		MOVE      TOTOMSK TO OWNERMSK
         		EDIT      APTOTOWN TO OWNERMSK
         		move      form102 to aplotown
.        		div       hund by aplotown
.        		move      c0 to aptotown
.Begin patch 2.41
         		Move           "1625" to row
.               add            "750" to row
//
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
//                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
//Patch 2.53
			Move BalanceRow,str5
			Call Trim Using Str5
.			Pack Cell,"A",str5
.	        	setprop sheet.range(Cell),*Value="Opening Balance:"                              
.			Pack Cell,"E",str5        	
.	        	setprop sheet.range(Cell),*Value=Ownermsk           
//Patch 2.53
	        	Add C5,CurCellNum
			Add C5 to CellRowCnt	        	
			Move CurCellNum,str5
			Call Trim Using Str5
			Pack Cell,"A",str5
	       		Setprop sheet.range(Cell),*Value="No new Billing this month."        
//Patch 2.53
//Patch 2.53
	                Add            "500" to row
//Patch 2.53               
//               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
//Patch 2.53               
	                Add            "200" to row
.         Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"
.         add       c4 to lines
.End patch 2.41
         		Cmatch    yes to flatflg
         		If        equal
         			WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
         		             comma,b6:                         
         		             comma,b6:                         
         		             comma,b8:                      
         		             comma,b9:                    
         		             comma,b9:                    
         		             comma,b4:                    
         		             comma,aplotown:
         		             comma,b7,comma,holdlst1
         		Endif
         		Move      c0 to aptotown
         		Move      c0 to aptotwnx
         		Move      c0 to aptotwnr
       		Endif
       	Endif
        Compare   c0 to countb
        If        equal
.Begin patch 2.41
        	Add             "500" to row
//Patch 2.53  Comment Out       
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
//Patch 2.53 
//Patch 2.53
	        	Add C5,CurCellNum
			Add C5 to CellRowCnt	        	
			Move CurCellNum,str5
			Call Trim Using Str5
			Pack Cell,"A",str5
	       		Setprop sheet.range(Cell),*Value="No new Payments this month."        
//Patch 2.53


.         print     *n,*3,"No new Payments this month"
.         add       c2 to lines
         	Add            "200" to row
        Endif
        Add     "500" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Payment Adjustments :",*font=prtpg9
//Patch 2.53         
//Patch 2.53
.        Add C5,CurCellNum
.	Move CurCellNum,str5
.	Call Trim Using Str5
.	Pack Cell,"A",str5
.       	Setprop sheet.range(Cell),*Value="No new Payments this month."         
//Patch 2.53

.         print     *n:
.                   *n,*3,"Payment Adjustments :",hpdtch85,hpuprght,*flush
         add            "200" to row
.End patch 2.41
         RETURN
.
.............................................................
.Begin patch 1.9
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.End patch 1.9
..........................................................................
.
.totbILL - done with new invoices let's get new payments
totbiLL  compare   c0 to countA             -did we have any new?
         if        equal                -no
         move      c0 to aptotown
         move      c0 to page
         move      c2 to pass
         return
         endif
         move      aptotown to form102
;         mult      ".01" by form102
         add       form102 to lobbal
         MOVE      TOTOMSK TO OWNERMSK
         mult      "100" by aptotown
         EDIT      APTOTOWN TO OWNERMSK
         MOVE      TOTOMSK TO OWNRMSKx
         mult      "100" by aptotwnx
         EDIT      APTOTWNx TO OWNRMSKx
         MOVE      TOTOMSK TO OWNRMSKR
         mult      "100" by aptotwnr
         EDIT      APTOTWNr TO OWNRMSKr
.Begin patch 2.41
.         COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
//Patch 2.53 May not be a need for row count
.         If        (row > 7100)
.         	Call       Header
.         Endif
//Patch 2.53 May not be a need for row count         
         Compare   c0 to aptotwnx
         If     not equal
         	Add "200" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total New Income +",*Alignment=*right,*p=4750:row,OwnerMsk:
//                              *Alignment=*Left
//Patch 2.53         
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif	
		Endif       	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5	        
		move Cell,str8		
		Pack Cell,"R",str5	        
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium

		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total New Income"         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=OwnerMsk        
       		
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
//Patch 2.53
		Add	"200" to row
//Patch 2.53         
//         PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
//                       *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
//                       add            "200" to row
//         PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Rental":
//                        *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left,*font=prtpg9
//Patch 2.53               
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif       	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total Exchange List Recovery"         
		Pack Cell,"E",str5
       		Setprop sheet.range(Cell),*Value=ownrmskx               		
        	Add C1,CurCellNum
		Add C1 to CellRowCnt        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total Rental"         
		Pack Cell,"E",str5
       		Setprop sheet.range(Cell),*Value=ownrmskr               		       		
//Patch 2.53
.         print     *n,*02,"Total New Income +",hpt400,ownermsk,hpt515,"Total Exchange List Recovery",hpt650,ownrmskx
.         print     hpt515,"Total Rental",hpt650,ownrmskr
.End patch 2.41
         Else
.BEGIN patch 2.41
         	Add	"200" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total New Income +",*Alignment=*right,*p=4750:row,OwnerMsk:
//                              *Alignment=*Left
//Patch 2.53                   
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif	
		Endif
		Move CurCellNum,str5
		Call Trim Using Str5
		
		Pack Cell,"A",str5	        
		move Cell,str8		
		Pack Cell,"R",str5	        
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium
	        
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total New Income"         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=OwnerMsk      
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
       		
//Patch 2.53
.         print     *n,*02,"Total New Income +",hpt400,ownermsk
.END patch 2.41
         Endif
         Move      c0 to aptotown
         Move      c0 to aptotwnx
         Move      c0 to aptotwnr
         Compare   c1 to pass
         If        equal 
         	Move c2 to pass
         Endif
       	 Add C1,CurCellNum
	 Add C1 to CellRowCnt       	 
	 Move CurCellNum,str5
	 Call Trim Using Str5
  	 Pack Cell,"A",str5         
	 Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual 
         Call	Header
         Return
...........................................................................
.Totold - done with old invoices - done
totold
.	 getprop HPageBreaks,*count=n4
.	 for n1,"1",n4
.         	HPageBreaks.item giving HPageBreak using n1
.         	getprop HPageBreak,*type=n8
.		getprop HPageBreak,*Location=exrange        	         	
.         	If (n8 = "-4105")     .softbreak
.			getprop HPageBreak,*Location=exrange	         	         	
.         		HPageBreaks.Add using exrange giving HPageBreak
.			setprop HPageBreak,*Location=exrange
.         		HPageBreak.Delete
.         	Endif
.	 repeat
         compare   c0 to countd                .no opens????
         if        equal
         	compare   countc to c0         .need to print adj totals?
         	if         less      .yes   
         		call      totadj
         	else
         		compare   countb to c0         .need to print paid totals?
         		if         less      .yes   
         			move      c4 to pass
         			call      totpaid
         		endif
.         
         	endif
         endif
         move      aptotown to form102
.         mult      ".01" by form102
         cmatch    "T" to newflag
         if        equal
         	move       form102 to lobbal           .no prev. bal
         endif
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
.Begin patch 2.40
         move      c0 to aptownck
         move      aptotown to aptownck
.End patch 2.40
         move      c0 to aptotown
         goto      totals
..........................................................................
.totpaid - done with newly paid invoices let's get all adjustments
totpaid
         compare   c0 to countB
         if        equal
         	move      c0 to aptotown
         	move      c3 to pass
         	move      c0 to page
         	return
         endif
         move      c0 to form102
         move      aptotown to form102
.         mult      ".01" by form102
         sub       form102 to lobbal
         MOVE      TOTOMSK TO OWNERMSK
         mult      "100" by aptotown
         EDIT      APTOTOWN TO OWNERMSK
.BEGIN PATCH 2.41
//Patch 2.53 May not be a need for row count
.         if        (row > 7100)
.         	call      header
.         endif
//Patch 2.53         
         add            "400" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total Paid Income -",*Alignment=*right,*p=4750:row,OwnerMsk:
//                              *Alignment=*Left
//Patch 2.53      
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif	
		Endif
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5	        
		move Cell,str8		
		Pack Cell,"R",str5	        
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium
		
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total Paid Income "         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=OwnerMsk    
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
//Patch 2.53
.         COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
.         print     *n,*02,"Total Paid Income -",hpt400,ownermsk
.END PATCH 2.41
         move      c0 to aptotown
       
         compare   c4 to pass
         if        not equal 
         move      c3 to pass
         endif
       	 Add C1,CurCellNum
	 Add C1 to CellRowCnt       	 
	 Move CurCellNum,str5
	 Call Trim Using Str5
 	 Pack Cell,"A",str5         
	 Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual 
         call      header
         return
..........................................................................
.totadj   - done with all adjustments lets do the next account
totadj 
         compare   c0 to countC
         if equal
         move      c0 to aptotown
         move      c4 to pass
         move      c0 to page
         return
         endif
         move      c0 to form102
         mult      "100" by aptotown
         move      aptotown to form102
         mult      ".01" by form102
         add       form102 to lobbal
         MOVE      totomsk TO tadjMaSK
         EDIT      APTOTOWN TO tadjMaSK
.BEGIN PATCH 2.41
//Patch 2.53 May not be a need for row count
.         If        (row > 7100)
.         	call      Header
.         Endif
//Patch 2.53         
         add            "400" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total adjustments to Income +/-",*Alignment=*right,*p=4750:row,tadjmask:
//                              *Alignment=*Left
//Patch 2.53
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif	
		Endif
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5	        
		move Cell,str8		
		Pack Cell,"R",str5	        
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium
	        
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total adjustments to Income"        
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=tadjmask      
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
//Patch 2.53
.         COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
.         print     *n,*02,"Total adjustments to Income +/-",hpt400,tadjmask
.END PATCH 2.41
         move      c0 to aptotown
         move      c4 to pass
       	 Add C1,CurCellNum
	 Add C1 to CellRowCnt       	 
	 Move CurCellNum,str5
	 Call Trim Using Str5
 	 Pack Cell,"A",str5         
	 Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual 
         call      header
         return
..................................................................         
totals
.         branch    pass of totbill,totpaid,totadj
.         compare   c3 to pass
.         if        equal
.         call      totadj
.         endif
.Begin patch 2.40         
         compare   lobbal to aptownck
         if        not equal
         call      noxfoot
.begin patch 2.41
.         goto      endingb
               Move         No to XFootFlag
.End patch 2.41
         endif
.End patch 2.40

         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund into aplotown
         scan      "-" in ownermsk
         if        equal
         move      c0 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         endif
         MOVE      TOTOMSK TO OWNRMSKx
         mult      "100" by aptotwnx
         EDIT      APTOTWNx TO OWNRMSKx
         MOVE      TOTOMSK TO OWNRMSKR
         mult      "100" by aptotwnr
         EDIT      APTOTWNr TO OWNRMSKr
.BEGIN PATCH 2.41
//Patch 2.53 May not be a need for row count
.         IF        (row > 7100)
.         	Call      Header
.         Endif
//Patch 2.53         
.        COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
         compare   c0 to aptotwnx
         If	not equal
         	Add            "400" to row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,"Ending Balance",*Alignment=*right,*p=4750:row,OwnerMsk:
//                              *Alignment=*left
//Patch 2.53
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
			Endif	
		Endif
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5	        
		move Cell,str8		
.         	If  (slsown = holdown)
			Pack Cell,"R",str5	        
.         	Else
.			If ("T" = hldarflg)		
.				Pack Cell,"N",str5	        
.			Else
.				Pack Cell,"M",str5	        		
.			Endif
.		Endif
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium

		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Ending Balance"        
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=OwnerMsk   
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
		Move CurCellNum,Str5
		call trim using str5
		Pack Cell,"S",Str5         
	        sheet.range("B1",CELL).Columns.Autofit        
	        Setprop Sheet.Range("A1"),*ColumnWidth=xlColumnWidthMailer
	        
	        Setprop Sheet.Range("H1"),*ColumnWidth=xlColumnWidthPrice
	        
//Patch 2.53
.         	If        (row > 7100)
.	       		Add C1,CurCellNum
.			Move CurCellNum,str5
.			Call Trim Using Str5
.	 		Pack Cell,"A",str5         
.	 		Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual 
.         		Call	Header
.         	Endif
         	Add            "200" to row
//Patch 2.53         
//        PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
//                       *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
//Patch 2.53                       
         	Add            "200" to row
//Patch 2.53         
//          PrtPage       Laser;*p=5150:row,"Total Rental":
//                        *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left
//Patch 2.53      
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif        	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total Exchange List Recovery"         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=ownrmskx               		
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
        	Add C1,CurCellNum
		Add C1 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif         	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Total Rental"         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=ownrmskr               		       		
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"        
//Patch 2.53
.         print     *n,*02,"Ending Balance",hpt400,ownermsk,hpt515,"Total Exchange List Recovery",hpt650,ownrmskx
.         print     hpt515,"Total Rental",hpt650,ownrmskr
         Else
         	Add            "400" to row
//Patch 2.53         
//        PrtPage        laser;*p=125:row,*font=prtpg9b,"Ending Balance",*Alignment=*right,*p=4750:row,ownermsk:
//                              *Alignment=*Left
//Patch 2.53                              
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif        	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5	        
		move Cell,str8	
.         	If  (slsown = holdown)
			Pack Cell,"R",str5	        
.         	Else		
.			If ("T" = hldarflg)		
.				Pack Cell,"N",str5	        
.			Else
.				Pack Cell,"M",str5	        		
.			Endif
.		Endif
		move Cell,str9
	        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
	        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium
	        
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Ending Balance"         
		Pack Cell1,"E",str5
       		Setprop sheet.range(Cell1),*Value=ownermsk      
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"        
		Move CurCellNum,Str5
		call trim using str5
		Pack Cell,"S",Str5         	
	        sheet.range("B1",CELL1).Columns.Autofit    
	        Setprop Sheet.Range("A1"),*ColumnWidth=xlColumnWidthMailer	        
//Patch 2.53

.         print     *n,*02,"Ending Balance",hpt400,ownermsk
.End Patch 2.41
         Endif
         If          (XFootFlag = No)
		Add         "200" to Row
//Patch 2.53         
//         PrtPage        laser;*p=125:row,*font=prtpg9b,*boldon,*ulon,"Ending Balance Does NOT Xfoot  ":
//                        Lobbal," <> ",Aptownck,*boldoff,*uloff
//Patch 2.53                     
//Patch 2.53
        	Add C5,CurCellNum
		Add C5 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif        	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Ending Balance Does NOT Xfoot  "        
		Pack Cell1,"E",str5
		pack str255 with Lobbal," <> ",Aptownck		
       		Setprop sheet.range(Cell1),*Value=str255         
        	setprop sheet.range(Cell,Cell1).Font,*Bold="True"               		
       		
//Patch 2.53



         	Move        Yes to XFootFlag                   ;reset
         Endif



 
Endingb



         Move      c0 to aptotwnx
         Move      c0 to aptotwnr
.         add       "750" to row
.         add       c2 to lines
         CMatch    yes to flatflg
         If        Equal
         		WRITE     OUTPUT,SEQ;B5,comma,"E",comma,b25:          
         	             comma,b6:                         
         	             comma,b6:                         
         	             comma,b8:                      
         	             comma,b9:                    
         	             comma,b9:                    
         	             comma,b7:                    
         	             comma,aplotown:
         	             comma,b7,comma,holdlst1
         Endif
         call      mtaxprt
//patch 2.53
       	If  (slsown <> holdown)
		if ("T" = hldarflg)
			sheet.Columns(13).Delete         	
			sheet.Columns(13).Delete         	
			sheet.Columns(13).Delete         	
			sheet.Columns(13).Delete   
        		Setprop Sheet.Range("I1"),*ColumnWidth=xlColumnWidthGross			
		else
	
.			sheet.Columns(12).Delete         					
			sheet.Columns(9).Delete         					
			sheet.Columns(12).Delete         	
			sheet.Columns(12).Delete         	
			sheet.Columns(12).Delete         	
			sheet.Columns(12).Delete         			
		endif    
	Endif
//patch 2.53         
         pack      nlobfld from holdown,holdlist,cc,sysyr,sysmo
         move      holdown to loblon
         move      holdlist to loblist
         move      cc to lobcc
         move      sysyr to lobyy
         move      sysmo to lobmm      
         rep       zfill in nlobfld
         rep       zfill in loblon
         rep       zfill in loblist
         cmatch    yes to rflag
         if        not equal
         	call      nlobwrt
         endif
         cmatch    "T" to lastrec
         goto      eoj if equal
         move      c0 to lobbal
         move      slsown to holdown
         move      slslist to holdlist
         move      slslst1 to holdlst1
//Force NWF to show gross         
..         move      arflag to hldarflg
	move      "T" to hldarflg
         move      slslr to ninvfld
         rep       zfill in ninvfld
.add goodies get next beg bal
         pack      nlobfld from slsown,slslist,DATEKEY
         rep       zfill in nlobfld
         move       slsown to nownfld
         move      c0 to aptotown
.         move      c0 to aptotwnx
.         move      c0 to aptotwnr
         move       c0 to lobbal
         call       nlobkey
.                 call      nlobkey
         if        over
         	move      "T" to newflag
         else 
                move      "F" to newflag
         endif
.         display   *p1:24,*el,*b,"Beg bal",nlobfld,b1,newflag,*b,*w10
         move      c0 to page
         move      c0 to counta
         move      c0 to countb
         move      c0 to countc
         move      c0 to countd
         move      c1 to pass
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         Call      readtax
toteoj   
         return
..........................................................................
READPAY  MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      PAYTN TO str1
         MOVE      PAYTN TO PAYCHK
         PACK      PAYKEY FROM NOWNFLD,str1
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
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         CMATCH    YES TO AP2SW
         IF        NOT EQUAL
         MOVE      PCOMP TO ownocpy
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ENDIF
         endif
         endif
         RETURN
...............................................................................
preplob  branch    pass to prepok
.         pack      nlobfld from slsown,slslist,cc,sysyr,sysmo
.         rep       zfill in nlobfld
.         call      nlobtst
.         if        over                   *it really is new
.         goto      prepok
.         else                             *all ready done it
.         move      c0 to hit
.         noreturn
.         branch    pass of error,readpaid,readadj,readsls1
.         endif
.error    display   *p1:24,*el,*b,*b,*red,*blinkon,"ERROR AT PREPLOB",*b,*w10
.         stop
prepok   pack      nlobfld from slsown,slslist,DATEKEY
         rep       zfill in nlobfld
.         display   *p1:24,*el,*b,"at prepok",nlobfld,*b,*w10
         move      slsown to holdown
         move      slslist to holdlist
         move      slsown to nownfld
         move      slslst1 to holdlst1
         move      slslr to ninvfld
//         Force gross billings for nwf
.	move      arflag to hldarflg
         move      "T" to hldarflg
         call      ninvkey
         CALL      READPAY    
         move      c0 to lobbal
                 call      nlobkey
                 if        over
                 move      "T" to newflag
                 else 
                 move      "F" to newflag
                 endif
         call      header
         return
;

;.....................................................................
readtax
.         MOVE      SLSMLR TO nmtxfld
         MOVE      compnum TO nmtxfld
         CLEAR     TAXPRT
         MOVE      C0 TO MTXC501
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
mtaxprt
.         compare   n4 to pass
.         if        equal
//Patch 2.53
.         if        (row > 7800)
.         call      header
.         endif
//Patch 2.53
.         COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
mtaxprt2
.         print     *l;
.         add       c1 to lines
.         compare   "44" to lines
.         goto      mtaxprt2 if not equal
.         print     *3,"Mailers's tax status is provided as a service though":
.                   " its accuracy cannot be guaranteed."
.         print     033,"*p2438.0y0.0X",*3,"Mailers's tax status is provided as a service though":
.                   " its accuracy cannot be guaranteed."               
//               PrtPage        Laser;*p=1:7800,"Mailers's tax status is provided as a service though":
//                              " its accuracy cannot be guaranteed."               
        	Add C1,CurCellNum
		Add C1 to CellRowCnt        	
		If (Page = C1)	
			If (CellRowCnt > 54)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif
		Else
			If (CellRowCnt > 46)
				Call Header
				Add C1 to CurCellNum
				Add C1 to CellRowCnt		
					
			Endif	
		Endif        	        	
		Move CurCellNum,str5
		Call Trim Using Str5
		Pack Cell,"A",str5
       		Setprop sheet.range(Cell),*Value="Mailers's tax status is provided as a service though its accuracy cannot be guaranteed."        
        	Add C1,CurCellNum       		
		Add C1 to CellRowCnt        	
		Move CurCellNum,str5
	 	Call Trim Using Str5
  	 	Pack Cell,"A",str5         
	 	Setprop Sheet.Rows(str5),*PageBreak=xlPageBreakManual         	
         return
                    
.
EOJ
.patch2.42
.             PrtCLose             Laser
.             PRTPLAY splname,"Acrobat Distiller"
.             PRTPLAY splname,"PDF995"
.patch2.42
.             splclose
//Patch 2.53
.         book.printout
//Patch 2.53
         shutdown
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
.Begin patch 2.0
................................................................................................
.prtform
prtform
.BEGIN PATCH 2.41
.START PATCH 2.45 REPLACED LOGIC
.       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
.                   *MarginL=0,*MarginT=0:
.                   *Alignment=*Left:
.                   *p=3433:250,*font=prtpg12B,"Names  ":
.                   *font=prtpg12I,"  in the News":
.                   *PENSIZE=10,*p=3305:425,*Line=4940:425:
.                   *p=3413:450,*font=prtpg9,"C  a  l  i  f  o  r  n  i  a    I  n  c .":
.                   *p=3616:630,*font=prtpg6,"1300 Clay Street, 11th Floor":
.                   *p=3586:755, "Oakland, CA 94612-1429":
.                   *p=3516:880, " 415-989-3350 ","·"," Fax 415-433-7796":
.                   *RECT=1:250:5500:7250:
.                   *p=5550:1,*font=prtpg12B,"Owner Activity Report":
.                   *p=1:1,"Confidential":
.                   *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
.                   *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
.                   *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
.                   *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
.                   *PENSIZE=10,*p=2575:1250,*Line=2575:8000:             ;3rd
.                   *PENSIZE=10,*p=3150:1250,*Line=3150:8000:             ;4th
.                   *PENSIZE=10,*p3725:1250,*Line=3725:8000:              ;5th
.                   *PENSIZE=10,*p4975:1250,*Line=4975:8000:              ;6th
.                   *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;7th
.                   *PENSIZE=10,*p=1:7990,*Line=1875:7990:                    ;bottom lines   1
.                   *PENSIZE=10,*p=2000:7990,*Line=2500:7990:                    ;bottom lines 2
.                   *PENSIZE=10,*p=2575:7990,*Line=3025:7990:                    ;bottom lines   3
.                   *PENSIZE=10,*p=3150:7990,*Line=3650:7990:                    ;bottom lines   4
.                   *PENSIZE=10,*p=3725:7990,*Line=4850:7990:                    ;bottom lines   5
.                   *PENSIZE=10,*p=4975:7990,*Line=9800:7990:                    ;bottom lines   6
.                   *PENSIZE=10,*p=9875:7990,*Line=10400:7990:                    ;bottom lines   7
.                   *p=583:1300,*font=prtpg9bi,"Mailer":
.                   *p=2125:1300,*font=prtpg9bi,"LR##":
.                   *p=2700:1300,*font=prtpg9bi,"Inv##":
.                   *p=3140:1300,*font=prtpg9bi,"Mail Date":
.                   *p=3850:1300,*font=prtpg9bi,"Income Amount":
.                   *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
//       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
//                   *MarginL=0,*MarginT=0:
//                   *Alignment=*Left:
//                   *Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
//                   *RECT=1:250:5500:7250:
//                   *p=5550:1,*font=prtpg12B,"Owner Activity Report":
//                   *p=1:1,"Confidential":
//                   *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
//                   *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
//                   *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
//                   *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
//                   *PENSIZE=10,*p=2575:1250,*Line=2575:8000:             ;3rd
//                   *PENSIZE=10,*p=3150:1250,*Line=3150:8000:             ;4th
//                   *PENSIZE=10,*p3725:1250,*Line=3725:8000:              ;5th
//                   *PENSIZE=10,*p4975:1250,*Line=4975:8000:              ;6th
//                   *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;7th
//                   *PENSIZE=10,*p=1:7990,*Line=1875:7990:                    ;bottom lines   1
//                   *PENSIZE=10,*p=2000:7990,*Line=2500:7990:                    ;bottom lines 2
//                   *PENSIZE=10,*p=2575:7990,*Line=3025:7990:                    ;bottom lines   3
//                   *PENSIZE=10,*p=3150:7990,*Line=3650:7990:                    ;bottom lines   4
//                   *PENSIZE=10,*p=3725:7990,*Line=4850:7990:                    ;bottom lines   5
//                   *PENSIZE=10,*p=4975:7990,*Line=9800:7990:                    ;bottom lines   6
//                   *PENSIZE=10,*p=9875:7990,*Line=10400:7990:                    ;bottom lines   7
//                   *p=583:1300,*font=prtpg9bi,"Mailer":
//                   *p=2125:1300,*font=prtpg9bi,"LR##":
//                   *p=2700:1300,*font=prtpg9bi,"Inv##":
//                   *p=3140:1300,*font=prtpg9bi,"Mail Date":
//                   *p=3850:1300,*font=prtpg9bi,"Income Amount":
//                   *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
//Special Detail Header
	Add C4 to CurCellNum
	Add C4 to CellRowCnt	
	Move CurCellNum,str5
	Call Trim Using Str5
	Pack Cell,"F",str5
	pack str8 with "A",str5
        setprop sheet.range(Cell),*Value="Order",*HorizontalAlignment=AlignRight      
	Pack Cell,"G",str5     
        setprop sheet.range(Cell),*Value="Billed",*HorizontalAlignment=AlignRight      	
.	Pack Cell,"L",str5       
.        setprop sheet.range(Cell),*Value="Owner Income",*HorizontalAlignment=AlignRight   	
.	Pack Cell1,"M",str5
.	Pack CellRange,Cell,":",Cell1
.	sheet.range(CellRange).Merge
//Gross Billed For Sierra Club
.	Pack Cell,"L",str5	        
.        setprop sheet.range(Cell),*Value="Gross",*HorizontalAlignment=AlignRight   
//Special Part	

//	Pack Cell,"N",str5		
//        setprop sheet.range(Cell),*Value="Gross",*HorizontalAlignment=AlignRight    	
//	Pack Cell,"O",str5		        
//        setprop sheet.range(Cell),*Value="Broker",*HorizontalAlignment=AlignRight             
//	Pack Cell,"P",str5		        
//	setprop sheet.range(Cell),*Value="List Mgmt",*HorizontalAlignment=AlignRight        	        

	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	Move CurCellNum,str5
	Call Trim Using Str5
	Pack Cell,"A",str5	
	
        setprop sheet.range(Cell),*Value="Mailer"                       
	Pack Cell,"B",str5	        
        setprop sheet.range(Cell),*Value="LR##",*HorizontalAlignment=AlignCenter              
	Pack Cell,"C",str5	        
        setprop sheet.range(Cell),*Value="Inv##",*HorizontalAlignment=AlignCenter
	Pack Cell,"D",str5	        
        setprop sheet.range(Cell),*Value="Mail Date",*HorizontalAlignment=AlignRight   
	Pack Cell,"E",str5	        
        setprop sheet.range(Cell),*Value="Income Amount",*HorizontalAlignment=AlignRight               
        sheet.range(Cell).Columns.Autofit               

	Pack Cell,"F",str5	
        setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignRight               
	Pack Cell,"G",str5	
        setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignRight                       

	Pack Cell,"H",str5	
        setprop sheet.range(Cell),*Value="Price/M",*HorizontalAlignment=AlignRight         
        
//Gross Billed For Sierra Club
	Pack Cell,"I",str5	        
        setprop sheet.range(Cell),*Value="Gross Billed",*HorizontalAlignment=AlignRight                             
        sheet.range(Cell).Columns.Autofit 




//Payment Part        
	Pack Cell,"J",str5	
        setprop sheet.range(Cell),*Value="Check Date",*HorizontalAlignment=AlignRight          
        sheet.range(Cell).Columns.Autofit            
	Pack Cell,"K",str5	        
        setprop sheet.range(Cell),*Value="Chk. Number",*HorizontalAlignment=AlignRight                  
        sheet.range(Cell).Columns.Autofit                    
//Adjustment Part
	Pack Cell,"L",str5	
        setprop sheet.range(Cell),*Value="Adjustment Description",*HorizontalAlignment=AlignLeft   
        sheet.range(Cell).Columns.Autofit                       
            
//	Pack Cell,"L",str5	
//        setprop sheet.range(Cell),*Value="List",*HorizontalAlignment=AlignRight           
//	Pack Cell,"M",str5	        
//        setprop sheet.range(Cell),*Value="Select",*HorizontalAlignment=AlignRight                 
//	Pack Cell,"N",str5	        
//        setprop sheet.range(Cell),*Value="Billing",*HorizontalAlignment=AlignRight                      
//	Pack Cell,"O",str5	        
//        setprop sheet.range(Cell),*Value="Comm",*HorizontalAlignment=AlignRight                  
//	Pack Cell,"P",str5	        
//        setprop sheet.range(Cell),*Value="Comm",*HorizontalAlignment=AlignRight           
//	Pack Cell,"R",str5	        
//        setprop sheet.range(Cell),*Value="Billed",*HorizontalAlignment=AlignRight                             
	Pack Cell,"Q",str5	        
        setprop sheet.range(Cell),*Value="Description",*HorizontalAlignment=AlignLeft                     
        sheet.range(Cell).Columns.Autofit           
//Special Part End        
	Pack Cell,"R",str5	        
        setprop sheet.range(Cell),*Value="Mlr Tax",*HorizontalAlignment=AlignRight                      
	move Cell,str9
        setprop sheet.range(str8,str9).Font,*Name="Arial"        
        setprop sheet.range(str8,str9).Font,*Size="10"         
        setprop sheet.range(str8,str9).Font,*Bold="True"        
.	Pack Cell,"N",str5	        
.	Pack Cell1,"P",str5
.	Pack CellRange,Cell,":",Cell1
.        setprop sheet.range(Cell),*Value="List Costs",*HorizontalAlignment=AlignCenter                   
.	sheet.range(CellRange).Merge              
.        sheet.range(str8,str9).BorderAround using *LineStyle=1,*Weight=4                
        setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
        setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium


	Add C1 to CurCellNum
	Add C1 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5
        Move CurcellNum to BalanceRow	
//	Pack Cell,"A",str5	        
//      setprop sheet.range(Cell),*Value="Opening Balance:"
        





	Add C2 to CurCellNum        
	Add C2 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5	
	Pack Cell,"A",str5	        
	If (pass = c1)	
        	setprop sheet.range(Cell),*Value="New Billing:"                                       
        	setprop sheet.range(cell).Font,*Bold="True"           	
	elseif (pass = c2)	        
        	setprop sheet.range(Cell),*Value="Payments During the Month:"                                               	
        	setprop sheet.range(cell).Font,*Bold="True"           	
	elseif (pass = c3)	
        	setprop sheet.range(Cell),*Value="Payment Adjustments:"                                               	
        	setprop sheet.range(cell).Font,*Bold="True"           	
	else
        	setprop sheet.range(Cell),*Value="Open Invoices as of the end of the Month:" 
        	setprop sheet.range(Cell).Font,*Bold="True"         	
	Endif
     
//        setprop sheet.range("A16","R20").Font,*Bold="True"
//        sheet.range("A16","R17").BorderAround using *LineStyle=1,*Weight=4
	                        
                   
                   
.END PATCH 2.45 REPLACED LOGIC
.         IF        (HLDARFLG = "T")
.;print detail goodies
.               PRTPAGE     Laser;*p=5100:1235,*font=prtpg9bi:
.                           *p=6350:1365,"  List/Select":
.                           *p=6350:1235,"Owner Income"
.Begin patch 2.41
.          else
.print regular goodies
.               PRTPAGE     Laser;*p=5100:11300,*font=prtpg9bi,"Order Qty"
.
.               endif
.End patch 2.41
//print horiz line top of page     .note pattern 2G-horiz line 1G  vert line
//                         pattern   height in dots (length)   terminate in A for horiz. B for vert
//                         /           /                  vert width in dots      *cxA would be hor width
//                        /           /                  /         vert position in dots
//                       /           /                  /         /
//                 033,"*c2G",033,"*c5825.2000A",033,"*c3B",033,"*p63.40y2.50X":
//                 033,"*c0P";                                            \
.                           \                                               Horiz pos in dots
;                            start printing in black
;
;
;         print   033,"&l1E",033,"&a0c0R":
;                   033,"*p1030x75Y":
;                   033,"(8U",033,"(s1p12.00v0s+3b5T","Names":
;                   b2,033,"(8U",033,"(s1p12.00v1s-3b5T","in the News":
;                   033,"*p993x95Y",033,"*c525a02b0p":                         .LINE
;                   033,"*p1030x135Y":
;               033,"(8U",033,"(s1p06.00v0s-2b5T","C  A  L  I  F  O  R  N  I  A        I  N  C .":
;               033,"*p1085.0x189Y":
;               033,"(8U",033,"(s1p06.00v0s-2b5T","1300 Clay St., 11th Floor":
;.              033,"(8U",033,"(s1p06.00v0s-2b5T","One Bush Street, Suite 300":
;               033,"*p1060x226.5Y":
;               033,"(8U",033,"(s1p06.00v0s-2b5T","   Oakland, CA  94612-1429":
;.              033,"(8U",033,"(s1p06.00v0s-2b5T","San Francisco, CA 94104-4447":
;               033,"*p1030x264.0Y":
;               " 415-989-3350 ",bullet," Fax 415-433-7796":
;               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p0.0y1650.0X",033,"*c0P":            .box top
;               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p75.0y1650.0X",033,"*c0P":            .box bottom
;               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y1650.0X",033,"*c0P":            .box line left
;               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y2400.0X",033,"*c0P":            .box line right
;               033,"*p1665x55Y": 
;               033,"(8U",033,"(s1p13.00v1s+3b5T","OWNER ACTIVITY REPORT":
;               033,"*p1x40Y": 
;               033,"(8U",033,"(s1p13.00v0s+3b5T","CONFIDENTIAL":
;               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p375.0y2.5X",033,"*c0P":            .horizontal lines header top
;               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p450.0y2.5X",033,"*c0P":            .horizontal lines header bottm
;               033,"*c2G",033,"*c562.5A",033,"*c3B",033,"*p2400.0y2.5X",033,"*c0P":            .horizontal lines bottom of form col 1
;               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y587.5X",033,"*c0P":            .horizontal lines bottom of form col 2
;               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y765.625X",033,"*c0P":            .horizontal lines bottom of form col 3
;               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y943.75X",033,"*c0P":            .horizontal lines bottom of form col 4
;               033,"*c2G",033,"*c375.0A",033,"*c3B",033,"*p2400.0y1121.875X",033,"*c0P":            .horizontal lines bottom of form col 5
;               033,"*c2G",033,"*c1237.0A",033,"*c3B",033,"*p2400.0y1534.375X",033,"*c0P":            .horizontal lines bottom of form col 6
;               033,"*c2G",033,"*c337.0A",033,"*c3B",033,"*p2400.0y2809.375X",033,"*c0P":            .horizontal lines bottom of form col 7
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2.5X",033,"*c0P":            . 1st vert line left
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y587.5X",033,"*c0P":            . 2nd vert line 
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y765.625X",033,"*c0P":            . 3rd vert line 
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y943.75X",033,"*c0P":            . 4th vert line 
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1121.875X",033,"*c0P":            . 5th vert line 
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1534.375X",033,"*c0P":            . 6th vert line 
;               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2809.375X",033,"*c0P":            . 7th vert line 
;               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p375.0y3182.5X",033,"*c0P":            . last vert line end header bar
;               033,"*p175x425.0Y":                                                          .position for header lettering
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer":                              . bold & italicised
;               033,"*p624.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","LR##":                              . bold & italicised
;               033,"*p814.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Inv##":                              . bold & italicised
;               033,"*p955.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Mail Date":                              . bold & italicised
;               033,"*p1195.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Income Amount":                              . bold & italicised
;               033,"*p1609.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Order Qty        Billed Qty       Price/m":                              . bold & italicised
;               033,"*p2900.0x425.0Y":
;               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer Tax":                              . bold & italicised
;              033,"*p0.0x0.0Y"                                .reset position (nec?)

              return
;exprint
;        PRINT     *3,hpdtch70,hpuprght,str40,*flush:
;                   hpt200,hpdtch85,hpuprght,b1,slslr:
;                   hpt250,b2,INVNUM:
;                   hpt325,b4,DATEPRT1:
;                   hpt575,"Exchange List Recovery",hpdtch85;
;.                   hpt525,m$oqty,hpt600,"Exchange List Recovery",hpdtch85;
;         MATCH     YES TO AP2SW
;         IF        EQUAL
;         PRINT     hpt400,AP2OUT;
;         move      slsap2 to aplotus
;.begin patch 1.8
;.         div       hund into aplotus
;.end patch 1.8
;         ADD     SLSAP2 TO APTOTOWN
;         ADD     SLSAP2 TO APTOTWNX
;         ELSE
;         COMPARE   C0 TO PAYCHK
;         IF        EQUAL
;         PRINT        hpt400,AP1OUT;
;         move      slsap1 to aplotus
;.begin patch 1.8
;.         div       hund into aplotus
;.end patch 1.8
;.         move      slsap1 to aplotus
;         ADD       SLSAP1 TO APTOTOWN
;         ADD     SLSAP1 TO APTOTWNX
;         else
;         PRINT     hpt400,AP2OUT;
;         move      slsap2 to aplotus
;.begin patch 1.8
;.         div       hund into aplotus
;.end patch 1.8
;.         move      slsap2 to aplotus
;         ADD       SLSAP2 TO APTOTOWN
;         ADD     SLSAP2 TO APTOTWNX
;         ENDIF
;         ENDIF
;         cmatch    yes to flatflg
;         if        equal
;.begin  patch 1.8
;.        WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
;.                     comma,slslr:
;.                     comma,invnum:
;.                     comma,dateprt1:
;.                     comma,oqty:
;.                     comma,qtyshp:
;.                     comma,m$ppm:
;.                     comma,aplotus:
;.                     comma,taxprt:
;.                     comma,holdlst1
;         WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
;                      comma,slslr:
;                      comma,invnum:                         
;                      comma,dateprt1:                      
;                      comma,oqty:                    
;                      comma,qtybild:
;                      comma,m$ppm:                    
;                      comma,aplotus:
;                      comma,taxprt:
;                      comma,holdlst1
;.end patch 1.8
;         endif
;.
;.         cmatch      "T" to hldarflg
;.         if          equal
;.         move        armask to arout
;.         EDIT        arform TO AROUT
;.         PRINT       hpt825,arout:
;.         PRINT       hpt850,hpdtch85,hpuprght,arout:
;.                      hpdtch85,hpuprght,hpt950,TAXPRT
;.         else
;         PRINT       hpt825:
;                      hpt950,TAXPRT
;.         endif
;         return
;END PATCH 2.41
;...............................................................................................
;end patch 2.0
;...............................................................................................
;begin patch 2.41
PrtOWner
//                call          trim using Ownlocty
//                prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p10000:500,Page:
//                              *p=10000:375,Today,*alignment=*left:
//                              *p=500:575,OwnLonm:
//                              *p=500:725,Ownocpy,*p=7000:725,Holdlst1:
//                              *p=500:875,Ownlosa,*p=7000:875,Holdlist:
//                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc
.
.Start Patch 2.52
	Read    labels,ownlon;;
        If	over
		move	holdown to ownlon
		rep	zfill in ownlon
                write	labels,ownlon;ownvars
	Endif
	Move c0 to CellRowCnt
.End Patch 2.52
//Owner Header
.	Add C1 to CurCellNum
.	Add C1 to CellRowCnt	
.	Move CurCellNum to str5
.	Call Trim Using Str5
	
	
.	setprop	sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.	Pack Cell with "A",str5
.	Pack Cell1 with "D",str5	
.	Pack CellRange with Cell,":",Cell1
.	sheet.range(CellRange).Merge
	If (CurCellNum = C1)
.	
		Move C1 to CellRowCnt		
		Move CurCellNum to str5
		Call Trim Using Str5		
.		
		If ("T" = arflag)	
			Pack Cell  with "R",str5	
		Else
			Pack Cell  with "S",str5		
		Endif		
.		Pack Cell  with "S",str5	
	        setprop sheet.range(Cell),*Value=Today,*HorizontalAlignment=AlignRight	
		Mult CurCellNum,C15,CELLPOINT
		sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60	
		Pack Cell  with "E",str5
		Add C1 to CurCellNum
		Add C1 to CellRowCnt	
		Move CurCellNum to str5
		Call Trim Using Str5	
		Pack Cell1 with "J",str5	
		Pack CellRange with Cell,":",Cell1
		sheet.range(CellRange).Merge 	
	        setprop sheet.range(CellRange),*Value="Owner Activity Report",*HorizontalAlignment=AlignCenter                   
	        setprop sheet.range(CellRange).Font,*Bold="True"         
	        setprop sheet.range(CellRange).Font,*Name="Arial"        
	        setprop sheet.range(CellRange).Font,*Size="16"
	        setprop sheet.range(CellRange),*HorizontalAlignment=xlCenter      
	        setprop sheet.range(CellRange),*VerticalAlignment=xlCenter 
		Add c5 to CurCellNum
		Add C5 to CellRowCnt	
		Move CurCellNum to str5
		Call Trim Using Str5	
		Pack Cell with "A",str5
	        setprop sheet.range(Cell),*Value="Confidential"
	        setprop sheet.range(Cell).Font,*Bold="True"       	        
	        sheet.range(CellRange).BorderAround using *LineStyle=1,*Weight=2		
	        pack    str11,"1:","7"
        	setprop sheet.PageSetup,*PrintTitleRows=str11		
		Add c1 to CurCellNum
		Add c1 to CellRowCnt	        	
        	
	Else
.		Add C1 to CurCellNum
.		Add C1 to CellRowCnt	
.		Mult CurCellNum,C15,CELLPOINT
.		Sub "30" from CellPOINT
.		sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,CELLPOINT,190,60
	Endif
                
        
         

.	Add c2 to CurCellNum
.	Add C2 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5	
	Pack Cell with "A",str5        
	Move Cell to str8	
	
	setprop sheet.range(Cell),*Value=Ownlon,*HorizontalAlignment=AlignLeft
	If ("T" = hldarflg)	
		Pack Cell  with "R",str5	
	Else
		Pack Cell  with "S",str5		
	Endif
	Move Page,Str4
	Call Trim using Str4
	setprop sheet.range(Cell),*Value=Str4,*HorizontalAlignment=AlignRight
.Following used as a temporary measure until all files have had Mailer field converted
//Bold All this

	Add c1 to CurCellNum
	Add C1 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5	
	Pack Cell with "A",str5        
        setprop sheet.range(Cell),*Value=OwnLonm
        

	Add c1 to CurCellNum
	Add C1 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5	
	Pack Cell with "A",str5                
        setprop sheet.range(Cell),*Value=Ownocpy
        
	Pack Cell with "H",str5        
	If (Pass = C1)
	        setprop   sheet.range(Cell),*Value="New Billings For List:"
	ElseIf (Pass = C2)
	        setprop   sheet.range(Cell),*Value="New Payments for List:"	
	ElseIf (Pass = C3)
	        setprop   sheet.range(Cell),*Value="Payment Adjustments for List:"	
	Else
	        setprop   sheet.range(Cell),*Value="Current Open Invoices:"	
	Endif         
        
	Add c1 to CurCellNum
	Add C1 to CellRowCnt	
	Move CurCellNum to str5
	Call Trim Using Str5	

	Pack Cell with "A",str5                
        setprop sheet.range(Cell),*Value=Ownlosa

        call Trim Using Ownlocty
        pack taskname with Ownlocty,",  ",Ownlos,"  ",Ownlozc
	Pack Cell with "H",str5                
        setprop sheet.range(Cell),*Value=Holdlst1                

	Add c1 to CurCellNum     
	Add C1 to CellRowCnt	

	Move CurCellNum to str5
	Call Trim Using Str5
	Pack Cell with "A",str5                
	setprop sheet.range(Cell),*Value=taskname

	Pack Cell with "H",str5                	
        setprop sheet.range(Cell),*Value=Holdlist,*HorizontalAlignment=AlignLeft       

	Move cell to str9
        
        
        

.	Add c1 to CurCellNum	
.	Move CurCellNum to str5
.	Call Trim Using Str5
.	Pack Cell with "H",str5        
.	If (Pass = C1)
.	        setprop   sheet.range(Cell),*Value="New Billing For List:"
.	ElseIf (Pass = C2)
.	        setprop   sheet.range(Cell),*Value="New Payments for List:"	
.	ElseIf (Pass = C3)
.	        setprop   sheet.range(Cell),*Value="Payment Adjustments for List:"	
.	Else
.	        setprop   sheet.range(Cell),*Value="Current Open Invoices:"	
.	Endif 	

.	Add c2 to CurCellNum            
.	Move CurCellNum to str5
.	Call Trim Using Str5	
.	Pack Cell with "H",str5                
.        setprop sheet.range(Cell),*Value=Holdlst1                
.	Add c1 to CurCellNum            
.	Move CurCellNum to str5
.	Call Trim Using Str5	
.	Pack Cell with "H",str5                	
.        setprop sheet.range(Cell),*Value=Holdlist,*HorizontalAlignment=AlignLeft


        setprop sheet.range(str8,str9).Font,*Name="Arial"        
        setprop sheet.range(str8,str9).Font,*Size="10"         
        setprop sheet.range(str8,str9).Font,*Bold="True"        
//End Bold All this        
..........Line Skip..........
//

               return
               
//Patch 2.53 
SheetSetup
        setprop sheet.PageSetup,*Orientation=xlLandscape
.        pack taskname," &D",crlf," &P"
.        setprop sheet.PageSetup,*RightHeader=taskname        
//.START PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
        setprop sheet.PageSetup,*Zoom=Zoom85
        setprop sheet.PageSetup,*TopMargin=TopMargin
        setprop sheet.PageSetup,*BottomMargin=BottomMargin
        setprop sheet.PageSetup,*FooterMargin=TopMargin
        setprop sheet.PageSetup,*LeftMargin=LeftMargin
        setprop sheet.PageSetup,*RightMargin=LeftMargin        

//
	getprop sheet,*Hpagebreaks=HpageBreaks
	pack str5 with "M1",":","P1"
        Setprop Sheet.Range(str5),*ColumnWidth=xlColumnWidth
	return        
//Patch 2.53
.End patch 2.41
...............................................................................................
.Begin patch 2.41
//Patch 2.53 Comment Out
.PrtSubHEad
.               PRTPAGE     Laser;*p=5750:1300,*font=prtpg9bi,*Alignment=*right,"Order Qty":
.                           *p=6725:1300,*font=prtpg9bi,"Billed Qty":
.                           *p=7250:1300,"Price/m"
.               Return
//Patch 2.53 Comment Out               
.End patch 2.41
...............................................................................................

.Begin patch 2.40
noxfoot
              move          "OAR Ending balance does not xfoot for owner # and list #"" to SmtpSubject Subject
;
	Clear         SmtpTextMessage(2)   Array <Text message >
	Move         "Out of balance for Owner / List ",SmtpTextMessage(2)   Array <Text message >
;
	Clear         SmtpTextMessage(3)   Array <Text message >
	append         b1,SmtpTextMessage(3)   Array <Text message >
	append         LOBLON,SmtpTextMessage(3)   Array <Text message >
	append         slash,SmtpTextMessage(3)   Array <Text message >
	append         loblist,SmtpTextMessage(3)   Array <Text message >
              reset          smtpTextMessage(3)

	Clear          SmtpTextMessage(4)   Array <Text message >
	append         "New Ending Balance = ",SmtpTextMessage(4)   Array <Text message >
	append         LOBBal,SmtpTextMessage(4)   Array <Text message >
               reset           smtpTextMessage(4)

              	Clear          SmtpTextMessage(5)   Array <Text message >
	append         "Balance should = ",SmtpTextMessage(5)   Array <Text message >
	append         ApTownck,SmtpTextMessage(5)   Array <Text message >
               reset           smtpTextMessage(5)

              	Move       "5",SmtpTextIndexLast                               Index to last entry in TextMessage array
;
        	move       "STAN" to str45
        	move       "Sarah Tan" to str55
.        	call       Mailmesg
;
        	move       "SInouye" to str45
        	move       "Sandra Inouye" to str55
.        	call       Mailmesg
              winshow
;
;        	move       "InformationServices" to str45
;        	move       "Information Services" to str55
        	move       "CReques" to str45
        	move       "Computer Request" to str55
..        	call       Mailmesg
        return
;begin patch 2.40
;
         STOP
.patch2.44
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.44
         INCLUDE   NORDIO.INC
 ;begin patch 2.5
 ;       INCLUDE   NINVIO.INC
        	INCLUDE   	ninvio.inc
	INclude	NInvAcdio.inc
 ;end patch 2.5
        include   nownio.inc
         INCLUDE   NmtxIO.INC
         INCLUDE   NDAT3IO.INC
         INCLUDE   GNXTIO.INC
         include   comlogic.inc
         include   nmoaio.inc
         include   nmrgio.inc
         INCLUDE   NLOBIO.INC
         include   nslsio.inc
;begin patch 1.9
         INCLUDE   NJSTIO.inc
;end patch 1.9
;begin patch 1.8
 ;begin patch 2.5
;         include   compute.inc
         	include   	compute.inc
 ;end patch 2.5
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
;end patch 1.8
         include   npayio.inc


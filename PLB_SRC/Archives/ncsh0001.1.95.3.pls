	include	common.inc
	include	cons.inc
	include	nctrdd.inc
	include	nchkdd.inc
	include	ncshdd.inc
	include	norddd.inc
.	include	f:\library\develop\backups\norddd.inc
;begin patch 1.96
;	include	ninvdd.inc
	include	ninvdd.inc
	Include	NinvAcddd.inc
;end patch 1.96
.	include	f:\library\develop\backups\ninvdd.inc
	include	nadjdd.inc
	include	njstdd.inc
	include	nmoadd.inc
	include	nmobdd.inc
.START PATCH 1.94 REPLACED LOGIC
.               include        nmlrdd.inc
.               include        nbrkdd.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 1.94 REPLACED LOGIC
	include	gnxtdd.inc
	include	npasdd.inc
;begin patch 1.93
	Include	MOANotesDD.inc
MoaNoteYesNo   Dim            1                   ;notes to write ?
;end patch 1.93
.Used with Search logic
        include ndatdd.inc
        include nrtndd.inc
        include ncmpdd.inc
.START PATCH 1.6 ADDED LOGIC
	include	nowndd.inc
.END PATCH 1.6 ADDED LOGIC
..........................
.EXTERNAL ROUTINES FROM NADJ0001.PLC
LoadAdjustListViews external "NADJ0001;AdjustLoadListViews"
DisableAdjustForm external "NADJ0001;AdjustDisableForm"
EnableAdjustForm external "NADJ0001;AdjustEnableForm"

PC      EQU     1

Release        INit           "1.96"         DLH	2005March02 Invoice Conversion
;Release        INit           "1.95.3"         ASH	01JUL2005 SMALL PATCH TO PREVENT ENTRY OF DETAIL RECORDS WITH 'P'/'Q' W/O ADJUSTMENT RECORD
;Reldate        Init           "1 July 2005"
Reldate        Init           "6 Sept 2005"
.Release        INit           "1.95.2"         ASH	10JUN2005 SMALL PATCH TO PREVENT DUPES WITH CODE 'P'
.Release        INit           "1.95.1"         ASH	31JAN2005 SMALL PATCH TO ALLOW RENNE SIMII TO PAY RENT
.Reldate        Init           "31 JANUARY 2005"
.Release        INit           "1.95"         ASH	11JAN2005 MAILER/BROKER FIELD CONVERSION:  DAT25N, NINCHK
.Reldate        Init           "11 JANUARY 2005"
.Release        INit           "1.94"         ASH	MAILER CONVERSION
.Reldate        Init           "27 May 2004"
.Release        INit           "1.93"         DLH            Begin adding code for MOANotes - for code N,D,d
.Reldate        Init           "20 February 2004"
;release			init	"1.92"	DMB	06NOV2003	Replaced the old broker test read with the company test read.
.release			init	"1.91"	ASH	17APR2003	ADDED SMALL PATCH PER REQUEST OF S. INOUYE - TRACKFREE CASE 55
.release			init	"1.9"	ASH	13FEB2003	ADDED ADMINISTRATOR MODE FOR CONTROL RECORD
.release			init	"1.8"	ASH	02JAN2003	CHANGED LOGIC TO ALLOW CONTROLS FROM PREVIOUS YEARS
.									ADDED SAFETY CHECK FOR DETAIL RECORDS WITH SAME LR IN TWO SEPARATE CONTROLS
.									ADDED LOGIC TO ENSURE FOCUS IS PLACED ON DETAIL LIST VIEWS WHEN USING DETAIL SEARCH, OTHERWISE I40 POSSIBLE FROM ACCESSING CONTROL LISTVIEW
.release			init	"1.7"	ASH	30SEP2002	ADDED ABILITY TO USE CODE 'M' FOR PAID INVOICES
.release			init	"1.6"	ASH	18SEP2002	ADDED FUNCTIONALITY OF SEARCH.PLF TO SEARCH FOR OWNER - OBSOLETE FOR THIS APPLICATION!!!!!
.release			init	"1.5"	ASH	14AUG2002	FIXED PROBLEM CAUSING I55 WHEN CREATING DUPLICATE CONTROLS
.release			init	"1.4"	ASH	11JUN2002	EXTENDED ISAM FOR NINCHK TO INCLUDE CONTROL DATE
.release			init	"1.3"	DMB	24JAN2002	Commented out testing dialog box and used xcosmo to complete password verify when doing changed after an edit has printed
.release		init	"1.2"	ASH	19DEC2001	ADDED LOGIC FOR NEW MODAL DIALOG BOXES WITHOUT BUTTONS SO THAT ACCTG MUST USE MOUSE TO CLOSE THEM DOWN
.release		init	"1.1"	ASH	30NOV2001
.release	init	"1.0"
               move    "NCSH0001.PLS",Wprognme
        move    "Cash Receipts Program",Wfunction
        move    "Andrew Harkins",Wauthor
;begin patch 1.93
;        move    "1.0",Wrelease
;        move    "April 17, 2003",Wreldate
               move           Release,Wrelease
               move           Reldate,Wreldate
;begin patch 1.93

.START PATCH 1.2 NEW LOGIC
STYLE   INTEGER 1,"0x000030"
.END PATCH 1.2 NEW LOGIC
Timer   Timer
.Used to keep track of tabs during Updating and Saving
TabNum  form   "01"
F102    form    10.2
.ExitFlag init	"Y"
ExitFlag DIM	%1
	MOVE	YES,EXITFLAG
ReturnFlag init "N"
NewFlag init    "N"
ViewFlag form   "1"
View1Flag form  "1"
View2Flag form  "1"
StopFlag form   "0"
Cash2Flag form  "0"
LRFlag  init    "N"
ChkFlag init    "N"     .Used to indicate if Check Number was modified
ChkFlag1 init   "N"     .Used to indicate if Check Number was modified AND if CID will need to be updated
ISStaff init    "DH AH DM DB JD "
SecFlag form	"0"
.Vars used for Report Screen
RptCan  dim     1

.Hold Variables
.START PATCH 1.4 REPLACED LOGIC
.hold    dim     99      .length of CASHVARS
.hold1   dim     77      .length of NCHKVARS
.hold2   dim     26      .length of NCTRVARS
.hold3   dim     400     .length of Invoice record
.HoldCont dim    3       .Holds Active Control
.HoldChk dim     9       .Holds Active Check Record
hold    dim     132     .length of CASHVARS
hold1   dim     110     .length of NCHKVARS
hold2   dim     42      .length of NCTRVARS
hold3   dim     400     .length of Invoice record
HoldCont dim    11      .Holds Active Control
.START PATCH 1.95 REPLACED LOGIC
.HoldChk dim     17      .Holds Active Check Record
HoldChk dim     23      .Holds Active Check Record
str23	dim	23
DateHold dim	10
DateHold2 dim	10
.END PATCH 1.95 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
.START PATCH 1.95 REPLACED LOGIC
.HoldCsh dim     30      .Holds Active Cash Record
.HoldMlr dim     4
.HoldBrk dim     4
HoldCsh dim     36      .Holds Active Cash Record
HoldMlr dim     6
HoldBrk dim     6
.END PATCH 1.95 REPLACED LOGIC
HoldCAMOUNT form 10.2
HoldCNUM dim    3
.START PATCH 1.95 REPLACED LOGIC
.HoldNCSHCHK dim 6
HoldNCSHCHK dim 12
.END PATCH 1.95 REPLACED LOGIC
HoldCLR dim     6
HoldCID  dim     2
HoldMlrKey dim  12
HoldMlrPO dim   12
HoldControl dim 3
.START PATCH 1.4 ADDED LOGIC
HoldControlDate dim	8
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.95 REPLACED LOGIC
.HoldCheck dim   6
HoldCheck dim   12
.END PATCH 1.95 REPLACED LOGIC
HoldChkAmt form 12.2

ARTotal form    12.2
ARSelTotal form 12.2
ChkTotal form   14.2
DetTotal form   14.2
Temp122 form    12.2
Temp122B form   12.2
Temp122C form   12.2
Temp122D form   12.2

CID2      DIM        2
.START PATCH 1.95 REPLACED LOGIC
.CMLR2     DIM        4
CMLR2     DIM        6
.END PATCH 1.95 REPLACED LOGIC
CLR2      DIM        6
CFILL12   DIM        2
CCE2      DIM        2
CYR2      DIM        2
CMO2      DIM        2
CDY2      DIM        2
CAMOUNT2  FORM    10.2
CEXTCD2   DIM        1
CFILL22   DIM        1
CNUM2     DIM        3
.START PATCH 1.4 ADDED LOGIC
CNUMDATE2 DIM	     8
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.95 REPLACED LOGIC
.NCSHCHK2  DIM        6
NCSHCHK2  DIM        12
.END PATCH 1.95 REPLACED LOGIC
.nckdtec2  dim        2
.nckdtey2  dim        2
.nckdtem2  dim        2
.nckdted2  dim        2
.npayor2   dim       41
.START PATCH 1.95 REPLACED LOGIC
.NCSHBRK2  DIM        4
NCSHBRK2  DIM        6
.END PATCH 1.95 REPLACED LOGIC
AP       FORM      7.2
ap1tot   form      8.2
ap2tot   form      8.2
ppext    dim       1
AMOUNTB  FORM      8.2       WORK FIELD FOR ON ACCOUNT.
.Used by NMOBIO.INC
CHANGE   FORM      7.2       CHANGE TO BE APPLIED TO BALANCE.

.Pointers
DimPtr  dim     ^
DimPtr1 Dim     ^
DimPtr2 Dim     ^
DimPtr3 Dim     ^
FrmPtr  form    ^
FrmPtr1 form    ^
FrmPtr2 form    ^
FrmPtr3 form    ^
StatPtr StatText ^
EditPtr EditText ^
ListPtr ListView ^

.Objects that are created and destroyed dynamically
.Note that this EditText, generally, fills spot for ErrorMssgStat4!!
.START PATCH 1.4 REPLACED LOGIC
.EditTextBoxes   EditText (2)
EditTextBoxes   EditText (3)
.END PATCH 1.4 REPLACED LOGIC
Buttons         Button  (3)
.CheckBoxes      CheckBox (2)
ComboBoxes      ComboBox (2)
.START PATCH 1.4 REPLACED LOGIC
.StatTextBoxes   StatText (2)
StatTextBoxes   StatText (3)
.END PATCH 1.4 REPLACED LOGIC
.ListViews       ListView (2)
.OrderInfoListView ListView
.OrderInfoEditText EditText
.Following Collection is used to dynamically destroy objects listed above
.Each time a form is loaded with those objects, dump them into this collection
.and then destroy whole collection when needed
ObjectColl      Collection

Carr    init    0x7f

.Colors
white   color
grey    color
red     color
black   color
tcolor  color

.Set Up Menu Bar
mFile   menu
mEdit   menu
mOptions menu
mReports menu
mSecurity menu
mHelp   menu
.Menu Bar for Report Screen
mRSearch menu
.Set Up SubMenu for Options
sSearch submenu
.START PATCH 1.9 ADDED LOGIC
sSecurity submenu
.END PATCH 1.9 ADDED LOGIC

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2;-;&Preferences;-;&Administrator Mode"
RData   init    "&Reports;&Control"
SEData  init    "Security;FIXORD mode;FIXBUSY"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To;&Campaign"
.START PATCH 1.9 ADDED LOGIC
sSData   init    ";&Control;&Detail"
.END PATCH 1.9 ADDED LOGIC


err     plform  Error
pass	plform	Passwrd
srch    plform  Search
rpt     plform  Report2
abt     plform  About
x       plform  ncsh0001
NCSH001A plform  NCSH001A
NCSH001B plform  NCSH001B
        winhide
.Load Forms, Always load parent form first
        formload x
        formload NCSH001A,ncsh0001
        formload NCSH001B,ncsh0001
        formload abt
        formload rpt
        formload srch
	formload pass
        formload err
.        formload Cash

        CashListView.InsertColumn using "Control",45,1
.START PATCH 1.95 REPLACED LOGIC
.        CashListView.InsertColumn using "Check",45,2
        CashListView.InsertColumn using "Check",75,2
.END PATCH 1.95 REPLACED LOGIC
        CashListView.InsertColumn using "Date",65,3
.START PATCH 1.95 REPLACED LOGIC
.        CashListView.InsertColumn using "Amount",75,4
.        CashListView.InsertColumn using "UnApplied",75,5
        CashListView.InsertColumn using "Amount",70,4
        CashListView.InsertColumn using "UnApplied",70,5
.END PATCH 1.95 REPLACED LOGIC
        CashListView.InsertColumn using "Payor",185,6
        CashListView.InsertColumn using "Other Detail",0,7
        CashListView.SetColumnFormat using 3,1
        CashListView.SetColumnFormat using 4,1
.
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewA.InsertColumn using "Check",45,1
        CashListViewA.InsertColumn using "Check",75,1
.END PATCH 1.95 REPLACED LOGIC
        CashListViewA.InsertColumn using "Control",45,2
        CashListViewA.InsertColumn using "Date",65,3
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewA.InsertColumn using "Amount",75,4
.        CashListViewA.InsertColumn using "UnApplied",75,5
        CashListViewA.InsertColumn using "Amount",70,4
        CashListViewA.InsertColumn using "UnApplied",70,5
.END PATCH 1.95 REPLACED LOGIC
        CashListViewA.InsertColumn using "Payor",185,6
        CashListViewA.InsertColumn using "Other Detail",0,7
        CashListViewA.SetColumnFormat using 3,1
        CashListViewA.SetColumnFormat using 4,1
.
        CashListViewB.InsertColumn using "",0,1
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewB.InsertColumn using "Amount",75,2
        CashListViewB.InsertColumn using "Amount",70,2
.END PATCH 1.95 REPLACED LOGIC
        CashListViewB.InsertColumn using "Control",45,3
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewB.InsertColumn using "Check",45,4
        CashListViewB.InsertColumn using "Check",75,4
.END PATCH 1.95 REPLACED LOGIC
        CashListViewB.InsertColumn using "Date",65,5
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewB.InsertColumn using "UnApplied",75,6
        CashListViewB.InsertColumn using "UnApplied",70,6
.END PATCH 1.95 REPLACED LOGIC
        CashListViewB.InsertColumn using "Payor",185,7
        CashListViewB.InsertColumn using "Other Detail",0,8
        CashListViewB.SetColumnFormat using 1,1
        CashListViewB.SetColumnFormat using 5,1
.
	CashListViewC.InsertColumn using "Payor",185,1
        CashListViewC.InsertColumn using "Control",45,2
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewC.InsertColumn using "Check",45,3
        CashListViewC.InsertColumn using "Check",75,3
.END PATCH 1.95 REPLACED LOGIC
        CashListViewC.InsertColumn using "Date",65,4
.START PATCH 1.95 REPLACED LOGIC
.        CashListViewC.InsertColumn using "Amount",75,5
.        CashListViewC.InsertColumn using "UnApplied",75,6
        CashListViewC.InsertColumn using "Amount",70,5
        CashListViewC.InsertColumn using "UnApplied",70,6
.END PATCH 1.95 REPLACED LOGIC
        CashListViewC.InsertColumn using "Other Detail",0,7
        CashListViewC.SetColumnFormat using 4,1
        CashListViewC.SetColumnFormat using 5,1
.
.START PATCH 1.95 ADDED LOGIC
        CashListViewD.InsertColumn using "",0,1
        CashListViewD.InsertColumn using "Date",65,2
        CashListViewD.InsertColumn using "Check",75,3
        CashListViewD.InsertColumn using "Amount",70,4
        CashListViewD.InsertColumn using "Control",45,5
        CashListViewD.InsertColumn using "UnApplied",70,6
        CashListViewD.InsertColumn using "Payor",185,7
        CashListViewD.InsertColumn using "Other Detail",0,8
        CashListViewD.SetColumnFormat using 2,1
        CashListViewD.SetColumnFormat using 3,1
.END PATCH 1.95 ADDED LOGIC
.
.START PATCH 1.95 REPLACED LOGIC
.        CashListView2.InsertColumn using "LR",45,1
.        CashListView2.InsertColumn using "Invoice",45,2
.        CashListView2.InsertColumn using "Payor",185,3
.        CashListView2.InsertColumn using "Mailer",45,4
.        CashListView2.InsertColumn using "Control",45,5
.        CashListView2.InsertColumn using "Check",50,6
.        CashListView2.InsertColumn using "Amount",75,7
.        CashListView2.InsertColumn using "Check Date",65,8
.        CashListView2.InsertColumn using "Code",45,9
.        CashListView2.InsertColumn using "Other Detail",0,10
.        CashListView2.SetColumnFormat using 6,1
..
.        CashListView2A.InsertColumn using "Check",50,1
.        CashListView2A.InsertColumn using "Control",45,2
.        CashListView2A.InsertColumn using "Mailer",45,3
.        CashListView2A.InsertColumn using "LR",45,4
.        CashListView2A.InsertColumn using "Invoice",45,5
.        CashListView2A.InsertColumn using "Payor",185,6
.        CashListView2A.InsertColumn using "Amount",75,7
.        CashListView2A.InsertColumn using "Check Date",65,8
.        CashListView2A.InsertColumn using "Code",45,9
.        CashListView2A.InsertColumn using "Other Detail",0,10
.        CashListView2A.SetColumnFormat using 6,1
..
.        CashListView2B.InsertColumn using "Mailer",45,1
.        CashListView2B.InsertColumn using "LR",45,2
.        CashListView2B.InsertColumn using "Invoice",45,3
.        CashListView2B.InsertColumn using "Payor",185,4
.        CashListView2B.InsertColumn using "Control",45,5
.        CashListView2B.InsertColumn using "Check",50,6
.        CashListView2B.InsertColumn using "Amount",75,7
.        CashListView2B.InsertColumn using "Check Date",65,8
.        CashListView2B.InsertColumn using "Code",45,9
.        CashListView2B.InsertColumn using "Other Detail",0,10
.        CashListView2B.SetColumnFormat using 6,1
..
.        CashListView2C.InsertColumn using "Control",45,1
.        CashListView2C.InsertColumn using "Mailer",45,2
.        CashListView2C.InsertColumn using "LR",45,3
.        CashListView2C.InsertColumn using "Invoice",45,4
.        CashListView2C.InsertColumn using "Payor",185,5
.        CashListView2C.InsertColumn using "Check",50,6
.        CashListView2C.InsertColumn using "Amount",75,7
.        CashListView2C.InsertColumn using "Check Date",65,8
.        CashListView2C.InsertColumn using "Code",45,9
.        CashListView2C.InsertColumn using "Other Detail",0,10
.        CashListView2C.SetColumnFormat using 6,1
.........................................
        CashListView2.InsertColumn using "LR",45,1
        CashListView2.InsertColumn using "Invoice",45,2
        CashListView2.InsertColumn using "Payor",165,3
        CashListView2.InsertColumn using "Mailer",45,4
        CashListView2.InsertColumn using "Control",45,5
        CashListView2.InsertColumn using "Check",80,6
        CashListView2.InsertColumn using "Amount",70,7
        CashListView2.InsertColumn using "Check Date",65,8
        CashListView2.InsertColumn using "Code",35,9
        CashListView2.InsertColumn using "Other Detail",0,10
        CashListView2.SetColumnFormat using 6,1
.
        CashListView2A.InsertColumn using "Check",80,1
        CashListView2A.InsertColumn using "Control",45,2
        CashListView2A.InsertColumn using "Mailer",45,3
        CashListView2A.InsertColumn using "LR",45,4
        CashListView2A.InsertColumn using "Invoice",45,5
        CashListView2A.InsertColumn using "Payor",165,6
        CashListView2A.InsertColumn using "Amount",70,7
        CashListView2A.InsertColumn using "Check Date",65,8
        CashListView2A.InsertColumn using "Code",35,9
        CashListView2A.InsertColumn using "Other Detail",0,10
        CashListView2A.SetColumnFormat using 6,1
.
        CashListView2B.InsertColumn using "Mailer",45,1
        CashListView2B.InsertColumn using "LR",45,2
        CashListView2B.InsertColumn using "Invoice",45,3
        CashListView2B.InsertColumn using "Payor",165,4
        CashListView2B.InsertColumn using "Control",45,5
        CashListView2B.InsertColumn using "Check",80,6
        CashListView2B.InsertColumn using "Amount",70,7
        CashListView2B.InsertColumn using "Check Date",65,8
        CashListView2B.InsertColumn using "Code",35,9
        CashListView2B.InsertColumn using "Other Detail",0,10
        CashListView2B.SetColumnFormat using 6,1
.
        CashListView2C.InsertColumn using "Control",45,1
        CashListView2C.InsertColumn using "Mailer",45,2
        CashListView2C.InsertColumn using "LR",45,3
        CashListView2C.InsertColumn using "Invoice",45,4
        CashListView2C.InsertColumn using "Payor",165,5
        CashListView2C.InsertColumn using "Check",80,6
        CashListView2C.InsertColumn using "Amount",70,7
        CashListView2C.InsertColumn using "Check Date",65,8
        CashListView2C.InsertColumn using "Code",35,9
        CashListView2C.InsertColumn using "Other Detail",0,10
        CashListView2C.SetColumnFormat using 6,1
.
        CashListView2D.InsertColumn using "",0,1
        CashListView2D.InsertColumn using "Check Date",65,2
        CashListView2D.InsertColumn using "Check",80,3
        CashListView2D.InsertColumn using "Amount",70,4
        CashListView2D.InsertColumn using "Control",45,5
        CashListView2D.InsertColumn using "Mailer",45,6
        CashListView2D.InsertColumn using "Payor",165,7
        CashListView2D.InsertColumn using "LR",45,8
        CashListView2D.InsertColumn using "Invoice",45,9
        CashListView2D.InsertColumn using "Code",35,10
        CashListView2D.InsertColumn using "Other Detail",0,11
        CashListView2D.SetColumnFormat using 3,1
.END PATCH 1.95 REPLACED LOGIC
.
.        Cash2ListView.InsertColumn using "LR",50,1
.        Cash2ListView.InsertColumn using "Invoice",50,2
.        Cash2ListView.InsertColumn using "Mailer",50,3
.        Cash2ListView.InsertColumn using "Broker",50,4
.        Cash2ListView.InsertColumn using "Key",60,5
.        Cash2ListView.InsertColumn using "Other Detail",0,6
        Cash2ListView.InsertColumn using "",0,1
        Cash2ListView.InsertColumn using "Mail Date",60,2
        Cash2ListView.InsertColumn using "LR",50,3
        Cash2ListView.InsertColumn using "Invoice",50,4
        Cash2ListView.InsertColumn using "Mailer",50,5
        Cash2ListView.InsertColumn using "Broker",50,6
        Cash2ListView.InsertColumn using "Key",60,7
        Cash2ListView.InsertColumn using "AR Amount",75,8
        Cash2ListView.InsertColumn using "AP Amount",75,9
        Cash2ListView.InsertColumn using "Other Detail",0,10
        Cash2ListView.SetColumnFormat using 7,1
        Cash2ListView.SetColumnFormat using 8,1
.
        Cash2ListView2.InsertColumn using "LR",50,1
        Cash2ListView2.InsertColumn using "Invoice",50,2
        Cash2ListView2.InsertColumn using "Mail Date",60,3
        Cash2ListView2.InsertColumn using "Mailer",50,4
        Cash2ListView2.InsertColumn using "Broker",50,5
        Cash2ListView2.InsertColumn using "Key",60,6
        Cash2ListView2.InsertColumn using "AR Amount",75,7
        Cash2ListView2.InsertColumn using "AP Amount",75,8
        Cash2ListView2.InsertColumn using "Other Detail",0,9
        Cash2ListView2.SetColumnFormat using 6,1
        Cash2ListView2.SetColumnFormat using 7,1
.
        Cash2ListView3.InsertColumn using "Invoice",50,1
        Cash2ListView3.InsertColumn using "LR",50,2
        Cash2ListView3.InsertColumn using "Mail Date",60,3
        Cash2ListView3.InsertColumn using "Mailer",50,4
        Cash2ListView3.InsertColumn using "Broker",50,5
        Cash2ListView3.InsertColumn using "Key",60,6
        Cash2ListView3.InsertColumn using "AR Amount",75,7
        Cash2ListView3.InsertColumn using "AP Amount",75,8
        Cash2ListView3.InsertColumn using "Other Detail",0,9
        Cash2ListView3.SetColumnFormat using 6,1
        Cash2ListView3.SetColumnFormat using 7,1
.
        Cash2ListView4.InsertColumn using "",0,1
        Cash2ListView4.InsertColumn using "AR Amount",75,2
        Cash2ListView4.InsertColumn using "LR",50,3
        Cash2ListView4.InsertColumn using "Invoice",50,4
        Cash2ListView4.InsertColumn using "Mail Date",60,5
        Cash2ListView4.InsertColumn using "Mailer",50,6
        Cash2ListView4.InsertColumn using "Broker",50,7
        Cash2ListView4.InsertColumn using "Key",60,8
        Cash2ListView4.InsertColumn using "AP Amount",75,9
        Cash2ListView4.InsertColumn using "Other Detail",0,10
        Cash2ListView4.SetColumnFormat using 1,1
        Cash2ListView4.SetColumnFormat using 8,1
.START PATCH 1.4 ADDED LOGIC
        CashListView3.InsertColumn using "",0,1
        CashListView3.InsertColumn using "Control",45,2
        CashListView3.InsertColumn using "Date",60,3
        CashListView3.InsertColumn using "Amount",75,4
        CashListView3.InsertColumn using "Status",100,5
        CashListView3.InsertColumn using "Other Detail",0,6
        CashListView3.SetColumnFormat using 1,1
        CashListView3.SetColumnFormat using 3,1
.END PATCH 1.4 ADDED LOGIC
	call	LoadAdjustListViews using ncsh0001

.Timer creation
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  ncsh0001;mFile,FData
        create  ncsh0001;mEdit,EData,mFile
        create  ncsh0001;mOptions,OData,mEdit
        create  ncsh0001;mReports,RData,mOptions
.        reset   revtyps
.        scan    INITS,revtyps
.        if equal
                create  ncsh0001;mSecurity,SEData,mReports
                create  ncsh0001;mHelp,HData,mSecurity
.START PATCH 1.9 ADDED LOGIC
.Create SubMenu
	        create  ncsh0001;sSecurity,sSData,mOptions,5
.END PATCH 1.9 ADDED LOGIC
.        else
.                create  ncsh0001;mHelp,HData,mReports
.        endif
.Create SubMenu
        create  ncsh0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mOptions,OptionsGo,result
        activate mReports,ReportGo,result
.        reset   revtyps
.        scan    INITS,revtyps
.        if equal
                activate mSecurity,SecurityGo,result
.START PATCH 1.9 ADDED LOGIC
.Activate SubMenus
	        activate sSecurity,SecurityGoA,result
.END PATCH 1.9 ADDED LOGIC
.        endif
        activate mHelp,HelpGo,result

.Activate SubMenus
        activate sSearch,SearchGo,result
.Create Colors
        create  white=*white
        create  grey=220:220:220
        create  red=*red
        create  black=*black

.Setup Screen
        call    CashDisableUpper2
        call    CashDisableButtons2A
        call    CashDisableLower1
        call    CashDisableLower2
.        move    "AH",INITS
        scan    INITS,ISStaff
        if equal
                setprop CashEditID,width=25,tabid=311
        endif
.
	move	"C",progcode
	setprop	Passwrd,visible=1
.
        move    C3,NCHKLOCK
        move    C3,NCTRLOCK
        move    C3,NCSHLOCK
.
        setfocus CashSearchControl
.Patch1.3
.	ALERT	TYPE=STYLE,"TESTING",RESULT
.Patch1.3
	loop
		waitevent
.                setitem timer,0,18000   .reset to 30 minutes
.START TESTER LOGIC
		deactivate TIMER
		ACTIVATE TIMER,Timeout,RESULT
.END TESTER LOGIC
        repeat

Timeout
.Test to make sure nothing is left open in Modify Mode
        getprop CashEditControlDate,enabled=N8
        getprop CashEditChkDate,enabled=N9
        getprop CashEditCheckDate,enabled=N10
        if (N8 = 1 | N9 = 1 | N10 = 1)
                call    Click_CashQuit
        endif
        beep
        beep
        beep
        shutdown

CashSetControlPrint
.Allows selection of Control for printing
.Called by Reports menu, Campaign submenu
        call    Report2DestroyObjects
        setprop Report2,title="NIN Control Print"
        move    NO,RptCan
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Control",""
        create  Report2;StatTextBoxes(2)=150:170:10:310,"","",fgcolor=red
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1
.START PATCH 1.4 REPLACED LOGIC
        create  Report2;StatTextBoxes(3)=70:90:10:110,"Date",""
        create  Report2;EditTextBoxes(2)=70:90:80:155,MaxChars=8,EditType=2,SelectAll=1,Style=1,Border=1
        create  Report2;ComboBoxes(1)=90:111:80:310,"",";C)ash Receipts;)Edit"
.END PATCH 1.4 REPLACED LOGIC
        create  Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
        activate StatTextBoxes(1)
        activate StatTextBoxes(2)
        activate EditTextBoxes(1)
.START PATCH 1.4 ADDED LOGIC
        activate StatTextBoxes(3)
        activate EditTextBoxes(2)
.END PATCH 1.4 ADDED LOGIC
        activate ComboBoxes(1)
        activate Buttons(1),CashControlOK,result
        listins ObjectColl,StatTextBoxes(1),StatTextBoxes(2),EditTextBoxes(1),ComboBoxes(1),Buttons(1)
        setfocus EditTextBoxes(1)
        return

CashControlOK
        getitem EditTextBoxes(1),0,str3
        call    Trim using str3
        if (str3 = "")
                setitem StatTextBoxes(2),0,"Not a valid Control Number!!"
                setfocus EditTextBoxes(1)
        else
.START PATCH 1.4 REPLACED LOGIC
.                move    str3,NCTRFLD
.                call    ZFILLIT using NCTRFLD,C0
.                setitem EditTextBoxes(1),0,NCTRFLD
.                move    "CashC.OK-NCTRTST",Location
.                pack    KeyLocation,"Key: ",NCTRFLD
.                call    NCTRTST
.                if over
.                        setitem StatTextBoxes(2),0,"Not a valid Control Number!!"
.                        setfocus EditTextBoxes(1)
.                else
.                        setprop Report2,visible=0
.                endif
..............
                call    ZFILLIT using str3,C0
                setitem EditTextBoxes(1),0,str3
.
	        getitem EditTextBoxes(2),0,str8
	        call    Trim using str8
		count	N2,str8
	        if (str8 = "" | N2 <> 8)
	                setitem StatTextBoxes(2),0,"Not a valid Control Date!!"
	                setfocus EditTextBoxes(2)
	        else
			unpack	str8,MM,DD,CC,YY
	                pack	NCTRFLD,str3,CC,YY,MM,DD
	                move    "CashC.OK-NCTRTST",Location
	                pack    KeyLocation,"Key: ",NCTRFLD
	                call    NCTRTST
	                if over
	                        setitem StatTextBoxes(2),0,"Not a valid Control!!"
	                        setfocus EditTextBoxes(1)
	                else
	                        setprop Report2,visible=0
	                endif
		endif
.END PATCH 1.4 REPLACED LOGIC
        endif
        return
;begin patch 1.93
SetMoaNotes
.Allows Entering of MOA notes on external D,d,N
        call    Report2DestroyObjects
        setprop Report2,title="Enter MOA Notes"
        move    NO,RptCan
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Note:",""
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=500,EditType=5,SelectAll=1,Style=1,Border=1
;        create  Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
        activate StatTextBoxes(1)
        activate EditTextBoxes(1)
;        activate Buttons(1),CashAdminOK,result
;        activate Buttons(1)
;        setprop  Report2OK,visible=1
        listins ObjectColl,StatTextBoxes(1),EditTextBoxes(1)
        setfocus EditTextBoxes(1)
        return
;end patch 1.93

CashSetAdminMode
.Allows selection of Control for printing
.Called by Reports menu, Campaign submenu
        call    Report2DestroyObjects
        setprop Report2,title="NIN Administrator Mode Password"
        move    NO,RptCan
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Password",""
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=10,EditType=5,SelectAll=1,Style=1,Border=1
        create  Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
        activate StatTextBoxes(1)
        activate EditTextBoxes(1)
        activate Buttons(1),CashAdminOK,result
        listins ObjectColl,StatTextBoxes(1),EditTextBoxes(1),Buttons(1)
        setfocus EditTextBoxes(1)
        return

CashAdminOK
        getitem EditTextBoxes(1),0,str10
        call    Trim using str10
        if (str10 = "")
                setitem StatTextBoxes(2),0,"Not a valid Password!!"
                setfocus EditTextBoxes(1)
        else
		if (str10 = "COSMO")
			move	C1,SecFlag
		endif
                setprop Report2,visible=0
        endif
        return

..Routine which destroys all objects created from above routines
Report2DestroyObjects
        destroy ObjectColl
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo2
FileGo1
        goto ReportGo1

FileGo2
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
HelpGo
        branch  result to HelpGo1
HelpGo1
        setprop AboutMssg,visible=1
        return

SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5
SearchGo1
.BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3
.MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo5
.CAMPAIGN
        move    C5,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.START PATCH 1.6 ADDED LOGIC
SearchGo6
.OWNER
        move    C6,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
.END PATCH 1.6 ADDED LOGIC
SearchLoad
.START PATCH 1.6 ADDED LOGIC
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.END PATCH 1.6 ADDED LOGIC
SearchLoad1
.BROKER
.START PATCH 1.95 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45,str55
.        if (TabNum = 1)
.                getprop CashEditBroker,enabled=result
.                if (result = 1)
.                        setitem CashEditBroker,0,str4
.                        setitem CashStatBrokerName,0,str45
.                        setfocus CashEditBroker
.                endif
.        elseif (TabNum = 2)
.                setitem Cash2EditBroker,0,str4
.                setitem Cash2StatBrk,0,str45
.                setfocus Cash2EditBroker
.        endif
.        return
..................................
	unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
	if (TabNum = 1)
		getprop CashEditBroker,enabled=result
		if (result = 1)
			setitem CashEditBroker,0,str6
			setitem CashStatBrokerName,0,str45
			setfocus CashEditBroker
		endif
	elseif (TabNum = 2)
		setitem Cash2EditBroker,0,str6
		setitem Cash2StatBrk,0,str45
		setfocus Cash2EditBroker
	endif
	return
.END PATCH 1.95 REPLACED LOGIC
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
.START PATCH 1.95 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
.        if (TabNum = 1)
.                getprop CashEditMlr,enabled=result
.                if (result = 1)
.                        setitem CashEditMlr,0,str4
.                        setitem CashStatMlrName,0,str45
.                        setfocus CashEditMlr
.                endif
.        elseif (TabNum = 2)
.                setitem Cash2EditMailer,0,str4
.                setitem Cash2StatMlr,0,str45
.                setfocus Cash2EditMailer
.        endif
.        return
................................
	unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
	if (TabNum = 1)
		getprop CashEditMlr,enabled=result
		if (result = 1)
			setitem CashEditMlr,0,str6
			setitem CashStatMlrName,0,str45
			setfocus CashEditMlr
		endif
	elseif (TabNum = 2)
		setitem Cash2EditMailer,0,str6
		setitem Cash2StatMlr,0,str45
		setfocus Cash2EditMailer
	endif
	return
.END PATCH 1.95 REPLACED LOGIC
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
.START PATCH 1.6 ADDED LOGIC
SearchLoad6
.OWNER - not an option with this program
        return
.END PATCH 1.6 ADDED LOGIC

Optionsgo
        branch  result to ViewGo,ViewGo,ViewGo,OptionsGo1,OptionsGo2
ViewGo
        return
NetGo
        return
OptionsGo1
        return
OptionsWritePref
        return
OptionsGo2
.START PATCH 1.9 REMOVED LOGIC
.	getprop	CashQuit,enabled=N9
.	if (N9 <> 1)
.		move	C0,SecFlag
.		call	CashSetAdminMode
.		setprop	Report2,visible=1
.		if (SecFlag = C1)
.			setitem	CashComboBox,0,3
.			call	CashSwitchTab using C1
.			call	Click_CashModify
.		endif
.	endif
.END PATCH 1.9 REMOVED LOGIC
        return
ReportGo
        branch  result to ReportGo1
ReportGo1
        if (ExitFlag = NO)
                return
        endif
        call    CashSetControlPrint
        setprop Report2,visible=1
        if (RptCan = NO)
                call    CashSetMouseBusy
                clear   taskname
                append  "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2  f:\apps\winbatch\BUTIL.exe ",TASKNAME
                getitem ComboBoxes(1),0,N9
                if (N9 = 1)     .Cash Receipts
                        append  "job=CASHN",TASKNAME
                else            .Edit
                        append  "job=CASHNE",TASKNAME
                endif
.                append  "job=CASHN",TASKNAME
                append  " F=default C=1",TASKNAME
                append  " B=",TASKNAME
                append  user,taskname
                append  " CN=",TASKNAME
.START PATCH 1.4 REPLACED LOGIC
.                append  NCTRFLD,TASKNAME
		unpack	NCTRFLD,str3
                append  str3,TASKNAME
.END PATCH 1.4 REPLACED LOGIC
                reset   TASKNAME
                move    C0,NCSHFLG2
                move    C0,NCSHFLG3
                close   NCSHLIST
                close   NCSHFILE
                execute taskname
                call    CashSetMouseFree
                alert   note,"Print request submitted",result
        endif
        return
ReportGo2
        return
SecurityGo
.START PATCH 1.9 ADDED LOGIC
	return
SecurityGoA
        branch  result to SecurityGo1,SecurityGo2
.END PATCH 1.9 ADDED LOGIC
SecurityGo1
.START PATCH 1.9 ADDED LOGIC
	getprop	CashQuit,enabled=N9
	if (N9 <> 1)
		move	C0,SecFlag
		call	CashSetAdminMode
		setprop	Report2,visible=1
		if (SecFlag = C1)
			setitem	CashComboBox,0,1
			call	CashSwitchTab using C1
			call	Click_CashModify
		endif
	endif
.END PATCH 1.9 ADDED LOGIC
        return
SecurityGo2
.START PATCH 1.9 ADDED LOGIC
	getprop	CashQuit,enabled=N9
	if (N9 <> 1)
		move	C0,SecFlag
		call	CashSetAdminMode
		setprop	Report2,visible=1
		if (SecFlag = C1)
			setitem	CashComboBox,0,3
			call	CashSwitchTab using C1
			call	Click_CashModify
		endif
	endif
.END PATCH 1.9 ADDED LOGIC
        return

CashClearScreens
.        setitem CashSearchControl,0,""
        setitem CashEditControlDate,0,""
        setitem CashEditControlAmount,0,""
        setitem CashStatMasterCont,0,""
        setitem CashStatStatus,0,""
.START PATCH 1.9 ADDED LOGIC
	setitem	CashEditControlCode,0,""
.END PATCH 1.9 ADDED LOGIC
        return
CashClearScreens1
        CashListView.DeleteAllItems giving N9
        CashListViewA.DeleteAllItems giving N9
        CashListViewB.DeleteAllItems giving N9
        CashListViewC.DeleteAllItems giving N9
.START PATCH 1.95 ADDED LOGIC
        CashListViewD.DeleteAllItems giving N9
.END PATCH 1.95 ADDED LOGIC
CashClearScreens1A
        setitem CashEditChkControl,0,""
.START PATCH 1.4 ADDED LOGIC
        setitem CashEditChkContDate,0,""
.END PATCH 1.4 ADDED LOGIC
        setitem CashEditChkCheck,0,""
        setitem CashEditChkDate,0,""
        setitem CashEditChkAmount,0,""
        setitem CashEditChkPayor,0,""
        setitem CashStatCheckTotal,0,""
        setitem CashStatUnApplied,0,""
        return
CashClearScreens2
        CashListView2.DeleteAllItems giving N9
        CashListView2A.DeleteAllItems giving N9
        CashListView2B.DeleteAllItems giving N9
        CashListView2C.DeleteAllItems giving N9
.START PATCH 1.95 REPLACED LOGIC
        CashListView2D.DeleteAllItems giving N9
.END PATCH 1.95 REPLACED LOGIC
CashClearScreens2A
        setitem CashEditAmount,0,""
        setitem CashEditCheckAmt,0,""
        setitem CashEditCheckDate,0,""
        setitem CashEditCheckNum,0,""
        setitem CashComboCode,0,1
        setitem CashEditControl,0,""
.START PATCH 1.4 ADDED LOGIC
        setitem CashEditCshControlDate,0,""
.END PATCH 1.4 ADDED LOGIC
        setitem CashEditID,0,""
        setitem CashEditInvoice,0,""
        setitem CashEditLR,0,""
        setitem CashEditMOA,0,""
        setitem CashEditMlr,0,""
        setitem CashEditBroker,0,""
        setitem CashEditPayor,0,""
        setitem CashEditRecordDate,0,""
        setitem CashStatMlrName,0,""
        setitem CashStatBrokerName,0,""
        setitem CashStatDetailTotal,0,""
        setitem CashStatDetailTotal2,0,""
        setitem CashStatUnApplied2,0,""
        return

CashEnableUpper
        setprop CashOK,enabled=1
        setprop CashSearchControl,enabled=1,bgcolor=white
        return
CashEnableUpper2
        setprop CashSearchControl,enabled=1,bgcolor=white
        setprop CashEditControlDate,enabled=1,bgcolor=white
        setprop CashEditControlAmount,enabled=1,bgcolor=white
        return
CashDisableUpper
        setprop CashOK,enabled=0
        call    CashDisableButtons1
        setprop CashSearchControl,enabled=0,bgcolor=grey
        return
CashDisableUpper2
.        setprop CashSearchControl,enabled=0,bgcolor=grey
        setprop CashEditControlDate,enabled=0,bgcolor=grey
        setprop CashEditControlAmount,enabled=0,bgcolor=grey
.START PATCH 1.9 ADDED LOGIC
        setprop CashEditControlCode,enabled=0,width=0
.END PATCH 1.9 ADDED LOGIC
        return

CashEnableButtons1
        setprop CashOK,enabled=1
        setprop CashModify,enabled=1
CashEnableButtons1A
        setprop CashNew,enabled=1
        setprop CashSearch,enabled=1
        return
CashEnableButtons2
        setprop CashQuit,enabled=1
        setprop CashSave,enabled=1
.        setprop CashDelete,enabled=1
        return

CashDisableButtons1
        setprop CashSearch,enabled=0
        setprop CashRetrieve,enabled=0
        setprop CashNew,enabled=0
        setprop CashModify,enabled=0
        return
CashDisableButtons2A
        setprop CashModify,enabled=0
CashDisableButtons2
        setprop CashQuit,enabled=0
        setprop CashSave,enabled=0
        setprop CashDelete,enabled=0
        setprop CashRetrieve,enabled=0
        return

.EnableLower section broken down into sub-routines in order to allow maximum flexibility
CashEnableLowerEdit1
        setprop CashEditChkControl,enabled=1,bgcolor=white
.START PATCH 1.4 ADDED LOGIC
        setprop CashEditChkContDate,enabled=1,bgcolor=white
.END PATCH 1.4 ADDED LOGIC
        setprop CashEditChkCheck,enabled=1,bgcolor=white
        setprop CashEditChkDate,enabled=1,bgcolor=white
        setprop CashEditChkAmount,enabled=1,bgcolor=white
        setprop CashEditChkPayor,enabled=1,bgcolor=white
        return
.START PATCH 1.1 REPLACED LOGIC
.CashEnableLowerEdit2
.       setprop CashComboCode,enabled=1,bgcolor=white
CashEnableLowerEdit2A
        setprop CashComboCode,enabled=1,bgcolor=white
CashEnableLowerEdit2
.END PATCH 1.1 REPLACED LOGIC
        setprop CashEditAmount,enabled=1,bgcolor=white
        setprop CashEditCheckAmt,bgcolor=white
        setprop CashEditCheckDate,bgcolor=white
        setprop CashEditCheckNum,enabled=1,bgcolor=white
        setprop CashEditControl,bgcolor=white
.START PATCH 1.4 ADDED LOGIC
        setprop CashEditCshControlDate,bgcolor=white
.END PATCH 1.4 ADDED LOGIC
        setprop CashEditID,enabled=1,bgcolor=white
        setprop CashEditInvoice,enabled=1,bgcolor=white
        setprop CashEditLR,enabled=1,bgcolor=white
        setprop CashEditMOA,enabled=1,bgcolor=white
        setprop CashEditMlr,enabled=1,bgcolor=white
        setprop CashEditBroker,enabled=1,bgcolor=white
        setprop CashEditPayor,bgcolor=white
        setprop CashEditRecordDate,bgcolor=white
        call    CashResetDetailFlags
        return

CashResetDetailFlags
        move    NO,LRFlag
        move    NO,ChkFlag
        move    NO,ChkFlag1
        return

CashEnableLower1
        call    CashEnableLowerEdit1
        call    CashEnableButtons2
        return

CashEnableLower2
.START PATCH 1.1 REPLACED LOGIC
.        call    CashEnableLowerEdit2
        call    CashEnableLowerEdit2A
.END PATCH 1.1 REPLACED LOGIC
        call    CashEnableButtons2
        return

.DisableLower section broken down into sub-routines in order to allow maximum flexibility
CashDisableLowerEdit1
        setprop CashEditChkControl,enabled=0,bgcolor=grey
.START PATCH 1.4 ADDED LOGIC
        setprop CashEditChkContDate,enabled=0,bgcolor=grey
.END PATCH 1.4 ADDED LOGIC
        setprop CashEditChkCheck,enabled=0,bgcolor=grey
        setprop CashEditChkDate,enabled=0,bgcolor=grey
        setprop CashEditChkAmount,enabled=0,bgcolor=grey
        setprop CashEditChkPayor,enabled=0,bgcolor=grey
        return
CashDisableLowerEdit2
        setprop CashEditAmount,enabled=0,bgcolor=grey
        setprop CashEditCheckAmt,enabled=0,bgcolor=grey
        setprop CashEditCheckDate,enabled=0,bgcolor=grey
        setprop CashEditCheckNum,enabled=0,bgcolor=grey
        setprop CashComboCode,enabled=0,bgcolor=grey
        setprop CashEditControl,enabled=0,bgcolor=grey
.START PATCH 1.4 ADDED LOGIC
        setprop CashEditCshControlDate,enabled=0,bgcolor=grey
.END PATCH 1.4 ADDED LOGIC
        setprop CashEditID,enabled=0,bgcolor=grey
        setprop CashEditInvoice,enabled=0,bgcolor=grey
        setprop CashEditLR,enabled=0,bgcolor=grey
        setprop CashEditMOA,enabled=0,bgcolor=grey
        setprop CashEditMlr,enabled=0,bgcolor=grey
        setprop CashEditBroker,enabled=0,bgcolor=grey
        setprop CashEditPayor,enabled=0,bgcolor=grey
        setprop CashEditRecordDate,enabled=0,bgcolor=grey
        return

CashDisableLowerButtons2
.        setprop CashQuit2,enabled=0
.        setprop CashNew2,enabled=0
.        setprop CashModify2,enabled=0
.        setprop CashDelete2,enabled=0
.        setprop CashSave2,enabled=0
.        setprop CashSearch2,enabled=0
.        setprop CashListView2,enabled=0
        return

.CashDisableLowerEdit1
..        setprop CashEditControl,enabled=0,bgcolor=grey
.        setprop CashEditControlDate,enabled=0,bgcolor=grey
.        return

CashDisableLowerButtons1
        setprop CashModify,enabled=0
        setprop CashQuit,enabled=0
        setprop CashSave,enabled=0
        return

CashDisableLower1
        call    CashDisableLowerEdit1
        setprop CashListView,enabled=0
        setprop CashListViewA,enabled=0
        setprop CashListViewB,enabled=0
        setprop CashListViewC,enabled=0
.START PATCH 1.95 ADDED LOGIC
        setprop CashListViewD,enabled=0
.END PATCH 1.95 ADDED LOGIC
        return
CashDisableLower2
        call    CashDisableLowerEdit2
        setprop CashListView2,enabled=0
        setprop CashListView2A,enabled=0
        setprop CashListView2B,enabled=0
        setprop CashListView2C,enabled=0
.START PATCH 1.95 REPLACED LOGIC
        setprop CashListView2D,enabled=0
.END PATCH 1.95 REPLACED LOGIC
        return

Cash2EnableSearch
        setprop Cash2EditMailer,enabled=1,bgcolor=white
        setprop Cash2EditBroker,enabled=1,bgcolor=white
        setprop Cash2EditKey,enabled=1,bgcolor=white
        setprop Cash2EditMlrPO,enabled=1,bgcolor=white
        setprop Cash2OK,enabled=1
        setprop Cash2Add,enabled=1
        return

Cash2DisableSearch
        setprop Cash2EditMailer,enabled=0,bgcolor=grey
        setprop Cash2EditBroker,enabled=0,bgcolor=grey
        setprop Cash2EditKey,enabled=0,bgcolor=grey
        setprop Cash2EditMlrPO,enabled=0,bgcolor=grey
        setprop Cash2OK,enabled=0
        setprop Cash2Add,enabled=0
        return
....................................
.....................................
CashRunNew LRoutine FrmPtr
        move    YES,NewFlag
        move    NO,ExitFlag
.	setprop CashListView,enabled=0
.	setprop CashListViewA,enabled=0
.	setprop CashListViewB,enabled=0
.	setprop CashListViewC,enabled=0
.	setprop CashListView2,enabled=0
.	setprop CashListView2A,enabled=0
.	setprop CashListView2B,enabled=0
.	setprop CashListView2C,enabled=0
        if (FrmPtr = 1)         .Control
.Eventually following line will need to be replaced with a move from the Controls.dat var
.to the hold variable
.START PATCH 1.4 REPLACED LOGIC
.                move    NCTRNUM,HoldCont
		pack	HoldCont,NCTRNUM,NCTRDATE
.END PATCH 1.4 REPLACED LOGIC
	        call	CashDisableUpper
                call	CashClearScreens
                setitem CashStatModify,0,"Currently Creating a New Control."
        	clear	str3
	        call	CashGetNextControl using str3
                setitem CashSearchControl,0,str3
                clock   timestamp,timestamp
                unpack  timestamp,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
                setitem CashEditControlDate,0,str10
                call    CashEnableUpper2
                call    CashEnableButtons2
                setfocus CashSearchControl
        elseif (FrmPtr = 2)     .Checks
        	setprop CashListView,enabled=0
        	setprop CashListViewA,enabled=0
        	setprop CashListViewB,enabled=0
        	setprop CashListViewC,enabled=0
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=0
.END PATCH 1.95 ADDED LOGIC
	        call	CashDisableUpper
                call	CashClearScreens1A
                setitem CashStatModify,0,"Currently Creating a New Check."
                call    Trim using NCHKCONT
                if (NCHKCONT <> "")
.START PATCH 1.4 REPLACED LOGIC
.                        pack    HoldChk,NCHKCONT,NCHKNUM
                        pack    HoldChk,NCHKCONT,NCHKCONTD,NCHKNUM
			unpack	NCHKCONTD,CC,YY,MM,DD
			call	Trim using DD
			if (DD <> "")
				pack	str10,MM,SLASH,DD,SLASH,CC,YY
			else
				clear	str10
			endif
                        setitem CashEditChkContDate,0,str10
.END PATCH 1.4 REPLACED LOGIC
                        setitem CashEditChkControl,0,NCHKCONT
                endif
                call    CashEnableLowerEdit1
                call    CashEnableButtons2
                setfocus CashEditChkControl
        elseif (FrmPtr = 3)     .Detail Records
        	setprop CashListView2,enabled=0
        	setprop CashListView2A,enabled=0
        	setprop CashListView2B,enabled=0
        	setprop CashListView2C,enabled=0
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=0
.END PATCH 1.95 REPLACED LOGIC
..Give option of retaining values
.        	if (Cash2Flag = C0)
.        		alert	plain,"Do You Want to Retain Previous Values?",result
.        		if (result = C2)	.NO, clear screen
.        			call	OrderClear
.        		elseif (result = C3)	.CANCEL, reset NewFlag and return
.        			move	"N",NewFlag
.        			return
.        		else
.        			getitem	Order1EditList,0,NDATFLD
.        			call	OrderGetCaller
.        		endif
.        	endif
	        call	CashDisableUpper
                call	CashClearScreens2A
                setitem CashStatModify,0,"Currently Creating a New Detail Record."
                call    Trim using CNUM
                if (CNUM <> "")
                        pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
		else
			getitem	CashStatMasterCont,0,CNUM
                endif
		setitem CashEditControl,0,CNUM
.START PATCH 1.4 ADDED LOGIC
		if (CNUMDATE <> "")
			unpack	CNUMDATE,CC,YY,MM,DD
			call	Trim using DD
			if (DD <> "")
				pack	str10,MM,SLASH,DD,SLASH,CC,YY
			else
				clear	str10
			endif
		else
			getitem	CashEditControlDate,0,str10
		endif
		setitem CashEditCshControlDate,0,str10
.END PATCH 1.4 ADDED LOGIC
                clock   timestamp,timestamp
                unpack  timestamp,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
                setitem CashEditRecordDate,0,str10
.                move	NO,LRFlag
.START PATCH 1.1 REPLACED LOGIC
.                call    CashEnableLowerEdit2
                call    CashEnableLowerEdit2A
.END PATCH 1.1 REPLACED LOGIC
                setprop CashEditControl,enabled=1
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditCshControlDate,enabled=1
.END PATCH 1.4 ADDED LOGIC
                call    CashEnableButtons2
                setfocus CashEditLR
        endif
        return

CashRunModify LRoutine FrmPtr
        if (FrmPtr = 1)
.START PATCH 1.4 REPLACED LOGIC
.                call    CashTestControl using NCTRNUM,N4
		pack	str11,NCTRNUM,NCTRDATE
                call    CashTestControl using str11,N4
.END PATCH 1.4 REPLACED LOGIC
.START PATCH 1.9 REPLACED LOGIC
.		if (N4 = 1)
.			setprop	CashComboBox,enabled=1
.			return
.		endif
		if (SecFlag <> C1)
			if (N4 = 1)
				setprop	CashComboBox,enabled=1
				return
			endif
		endif
.END PATCH 1.9 REPLACED LOGIC
        elseif (FrmPtr = 2)
                move	SEQ,result
        	move	result,N9
                if (ViewFlag = 1)
                        CashListView.GetNextItem giving result using C2,N9
                elseif (ViewFlag = 2)
                        CashListViewA.GetNextItem giving result using C2,N9
                elseif (ViewFlag = 3)
                        CashListViewB.GetNextItem giving result using C2,N9
                elseif (ViewFlag = 4)
                        CashListViewC.GetNextItem giving result using C2,N9
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        CashListViewD.GetNextItem giving result using C2,N9
.END PATCH 1.95 ADDED LOGIC
                endif
        	if (result = SEQ)
                        setprop	CashComboBox,enabled=1
                        return
                endif
.START PATCH 1.4 REPLACED LOGIC
.                call    CashTestControl using NCHKCONT,N4
		pack	str11,NCHKCONT,NCHKCONTD
                call    CashTestControl using str11,N4
.END PATCH 1.4 REPLACED LOGIC
                if (N4 = 1)
                        setprop	CashComboBox,enabled=1
                        return
                endif
        elseif (FrmPtr = 3)
                move	SEQ,result
        	move	result,N9
                if (View1Flag = 1)
                        CashListView2.GetNextItem giving result using C2,N9
                elseif (View1Flag = 2)
                        CashListView2A.GetNextItem giving result using C2,N9
                elseif (View1Flag = 3)
                        CashListView2B.GetNextItem giving result using C2,N9
                elseif (View1Flag = 4)
                        CashListView2C.GetNextItem giving result using C2,N9
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        CashListView2D.GetNextItem giving result using C2,N9
.END PATCH 1.95 REPLACED LOGIC
                endif
        	if (result = SEQ)
                        setprop	CashComboBox,enabled=1
                        return
                endif
.START PATCH 1.4 REPLACED LOGIC
.                call    CashTestControl using CNUM,N4
		pack	str11,CNUM,CNUMDATE
                call    CashTestControl using str11,N4
.END PATCH 1.4 REPLACED LOGIC
                if (N4 = 1)
                        setprop	CashComboBox,enabled=1
                        return
                endif
        endif
        move    NO,ExitFlag
.	setprop CashListView,enabled=0
.	setprop CashListViewA,enabled=0
.	setprop CashListViewB,enabled=0
.	setprop CashListViewC,enabled=0
.	setprop CashListView2,enabled=0
.	setprop CashListView2A,enabled=0
.	setprop CashListView2B,enabled=0
.	setprop CashListView2C,enabled=0
        if (FrmPtr = 1)         .Control
.Eventually following line will need to be replaced with a move from the Controls.dat var
.to the hold variable
.START PATCH 1.4 REPLACED LOGIC
.                move    NCTRNUM,HoldCont
		pack	HoldCont,NCTRNUM,NCTRDATE
.END PATCH 1.4 REPLACED LOGIC
	        call	CashDisableUpper
                setitem CashStatModify,0,"Currently Modifying a Control."
                setprop CashSearchControl,readonly=1
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditControlDate,readonly=1
.END PATCH 1.4 ADDED LOGIC
                call    CashEnableUpper2
                call    CashEnableButtons2
.START PATCH 1.9 REPLACED LOGIC
.                setfocus CashEditControlDate
		if (SecFlag = C1)
               	        setprop	CashEditControlCode,enabled=1,width=20
	                setfocus CashEditControlCode
		else
	                setfocus CashEditControlDate
		endif
.END PATCH 1.9 REPLACED LOGIC
        elseif (FrmPtr = 2)     .Checks
        	setprop CashListView,enabled=0
        	setprop CashListViewA,enabled=0
        	setprop CashListViewB,enabled=0
        	setprop CashListViewC,enabled=0
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=0
.END PATCH 1.95 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.		pack    HoldChk,NCHKCONT,NCHKNUM
		pack    HoldChk,NCHKCONT,NCHKCONTD,NCHKNUM
.END PATCH 1.4 REPLACED LOGIC
	        call	CashDisableUpper
                setitem CashStatModify,0,"Currently Modifying a Check."
                setprop CashEditChkControl,readonly=1
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditChkContDate,readonly=1
.END PATCH 1.4 ADDED LOGIC
.                setprop CashEditChkCheck,readonly=1
                call    CashEnableLowerEdit1
                call    CashEnableButtons2
                setprop CashDelete,enabled=1
                setfocus CashEditChkAmount
        elseif (FrmPtr = 3)     .Detail Records
        	setprop CashListView2,enabled=0
        	setprop CashListView2A,enabled=0
        	setprop CashListView2B,enabled=0
        	setprop CashListView2C,enabled=0
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=0
.END PATCH 1.95 REPLACED LOGIC
                pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
                pack    HoldCLR,CLR
                pack    HoldCNUM,CNUM
                pack    HoldNCSHCHK,NCSHCHK
                move    C0,HoldCAMOUNT
                move    CAMOUNT,HoldCAMOUNT
                pack    HoldCID,CID
	        call	CashDisableUpper
                setitem CashStatModify,0,"Currently Modifying a Detail Record."
		if (SecFlag = C1)
               	        call    CashEnableLowerEdit2
       	                call    CashEnableButtons2
		else
                	move    "DdNOQ",str6
        	        scan    CEXTCD,str6
	                if equal
.You can only delete these records!
        	                setprop CashQuit,enabled=1
	                else
                	        call    CashEnableLowerEdit2
        	                call    CashEnableButtons2
	                endif
		endif
                setprop CashDelete,enabled=1
                setfocus CashEditLR
        endif
        return

CashRunSearch LRoutine FrmPtr
        if (FrmPtr = 1)         .Control
                setprop	CashComboBox,enabled=1
                return
        endif
        move    NO,ExitFlag
        move	"S",NewFlag
        call	CashDisableUpper
        call    CashDisableButtons2
        setprop CashPrint,enabled=0
        if (FrmPtr = 2)     .Checks
                call    CashClearScreens1
        	setprop CashListView,enabled=0
        	setprop CashListViewA,enabled=0
        	setprop CashListViewB,enabled=0
        	setprop CashListViewC,enabled=0
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=0
.END PATCH 1.95 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.		pack    HoldChk,NCHKCONT,NCHKNUM
		pack    HoldChk,NCHKCONT,NCHKCONTD,NCHKNUM
.END PATCH 1.4 REPLACED LOGIC
                setitem CashStatModify,0,"Currently Searching for a Check."
                setprop CashEditChkCheck,enabled=1,bgcolor=white
                setprop CashEditChkAmount,enabled=1,bgcolor=white
                setprop CashEditChkPayor,enabled=1,bgcolor=white
.START PATCH 1.95 ADDED LOGIC
                setprop CashEditChkDate,enabled=1,bgcolor=white
.END PATCH 1.95 ADDED LOGIC
                setfocus CashEditChkCheck
        elseif (FrmPtr = 3)     .Detail Records
                call    CashClearScreens2
        	setprop CashListView2,enabled=0
        	setprop CashListView2A,enabled=0
        	setprop CashListView2B,enabled=0
        	setprop CashListView2C,enabled=0
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=0
.END PATCH 1.95 REPLACED LOGIC
                pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
                pack    HoldCLR,CLR
                pack    HoldCNUM,CNUM
                pack    HoldNCSHCHK,NCSHCHK
                move    C0,HoldCAMOUNT
                move    CAMOUNT,HoldCAMOUNT
                pack    HoldCID,CID
                setitem CashStatModify,0,"Currently Searching for a Detail Record."
                setprop CashEditCheckNum,enabled=1,bgcolor=white
                setprop CashEditLR,enabled=1,bgcolor=white
.START PATCH 1.95 ADDED LOGIC
                setprop CashEditCheckDate,enabled=1,bgcolor=white
.END PATCH 1.95 ADDED LOGIC
                setfocus CashEditLR
        endif
        setprop CashRetrieve,enabled=1
        setprop CashQuit,enabled=1
        return

CashRunRetrieve LRoutine FrmPtr
        if (FrmPtr = 1)         .Control
                return
        endif
        setprop CashRetrieve,enabled=0
        setprop CashQuit,enabled=0
        if (FrmPtr = 2)     .Checks
.Initialize some values
                move    C3,NCHKLOCK	.No locks
                move	C0,N1		.used to determine if at least one valid key field entered
.Verify Data Format
                clear	NCHKFLD1
.
                clear   NCHKFLD2
.START PATCH 1.95 REPLACED LOGIC
.                getitem	CashEditChkCheck,0,str6
.                call    Trim using str6
.                call    ZFillIt using str6,C0
.                if (str6 <> "")
.                        append	"02X",NCHKFLD2
.                        append	str6,NCHKFLD2
                getitem	CashEditChkCheck,0,str12
                call    Trim using str12
                call    ZFillIt using str12,C0
                if (str12 <> "")
                        append	"02X",NCHKFLD2
                        append	str12,NCHKFLD2
.END PATCH 1.95 REPLACED LOGIC
                        reset	NCHKFLD2
                else
                        add	C1,N1
                endif
.
                clear	NCHKFLD3
                getitem CashEditChkAmount,0,str18
                call    Trim using str18
                call    RemoveChar using str18,COMMA
                move    C0,Temp122
                move    str18,Temp122
                if (str18 <> "" AND Temp122 > C0)       .Cannot Search for an AMOUNT of ".00"!!
                        append	"03X",NCHKFLD3
                        append	Temp122,NCHKFLD3
                        reset	NCHKFLD3
                else
                        add	C1,N1
                endif
.
                clear	NCHKFLD4
                getitem	CashEditChkPayor,0,str45
                call    Trim using str45
                if (str45 <> "")
                        append	"04F",NCHKFLD4
                        append	str45,NCHKFLD4
                        reset	NCHKFLD4
                else
                        add	C1,N1
                endif
                if (N1 = 3)
                        clear	taskname
                        append	"You must supply at least one of the following:",taskname
                        append	carr,taskname
                        append	carr,taskname
                        append	"                Check Number",taskname
                        append	carr,taskname
                        append	"                Check Amount(>'.00')",taskname
                        append	carr,taskname
                        append	"                Payor",taskname
                        reset	taskname
                        alert	caution,taskname,result
                        setprop CashRetrieve,enabled=1
                        setprop CashQuit,enabled=1
                        setfocus CashEditChkCheck
                        return
                endif
.Start Searching
                move	"CashRetrieve-NCHKAIM",Location
		pack	KeyLocation,"Key: ",NCHKFLD2,COMMA,NCHKFLD3,COMMA,NCHKFLD4
                call	NCHKAIM
        	if over
                        alert	caution,"No Records Found!",result
                        setprop CashRetrieve,enabled=1
                        setprop CashQuit,enabled=1
			setfocus CashEditChkCheck
                        return
                endif
.START PATCH 1.95 REPLACED LOGIC
.                call    CashLoadListView
		getitem	CashEditChkDate,0,DateHold
		call	RemoveChar using DateHold,SLASH
		call	Trim using DateHold
		if (DateHold <> "")
			unpack	DateHold,str4,str5
			pack	DateHold,str5,str4
			if (NCHKDATE = DateHold)
				call    CashLoadListView
			endif
		else
			call    CashLoadListView
		endif
.END PATCH 1.95 REPLACED LOGIC
..Set up Icon to display while searching
.        moveaddr NCSHONE3,AnimateWindow
.        move    C1,AnimateCurIcon
.        move    C4,AnimateFrames
.        move    C0,AnimateIconID
..position Icon to sit to left of FindIt button
.        move    AniH,H
.        move    AniV,V
.Reset NewFlag to allow breaking of loop, Allow breaking of loop via Quit button
                move	"F",NewFlag
                setprop	CashQuit,enabled=1
                move	"CashRetrieve-NCHKKG",Location
                loop
.                        call	ANIMATEIT
                        call	NCHKKG
                        until over
.START PATCH 1.95 REPLACED LOGIC
.                        call    CashLoadListView
			if (DateHold <> "")
				if (NCHKDATE = DateHold)
					call    CashLoadListView
				endif
			else
				call    CashLoadListView
			endif
.END PATCH 1.95 REPLACED LOGIC
                        eventcheck
                        until (NewFlag = "S")
                repeat
.                destroy AnimateIcon
.Modify Screen
                call	CashDisableLowerEdit1
                call	CashEnableButtons1
                call	CashEnableUpper
                setprop CashQuit,enabled=0
                if (ViewFlag = 1)
                        CashListView.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView.EnsureVisible using 0,0
                        setfocus CashListView
                        call	Click_CashListView
                elseif (ViewFlag = 2)
                        CashListViewA.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListViewA.EnsureVisible using 0,0
                        setfocus CashListViewA
                        call	Click_CashListViewA
                elseif (ViewFlag = 3)
                        CashListViewB.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListViewB.EnsureVisible using 0,0
                        setfocus CashListViewB
                        call	Click_CashListViewB
                elseif (ViewFlag = 4)
                        CashListViewC.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListViewC.EnsureVisible using 0,0
                        setfocus CashListViewC
                        call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        CashListViewD.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListViewD.EnsureVisible using 0,0
                        setfocus CashListViewD
                        call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                endif
        	setprop CashListView,enabled=1
        	setprop CashListViewA,enabled=1
        	setprop CashListViewB,enabled=1
        	setprop CashListViewC,enabled=1
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=1
.END PATCH 1.95 ADDED LOGIC
.START PATCH 1.8 ADDED LOGIC
		if (ViewFlag = 1)
			setfocus CashListView
		elseif (ViewFlag = 2)
			setfocus CashListViewA
		elseif (ViewFlag = 3)
			setfocus CashListViewB
		elseif (ViewFlag = 4)
			setfocus CashListViewC
.START PATCH 1.95 ADDED LOGIC
		elseif (ViewFlag = 5)
			setfocus CashListViewD
.END PATCH 1.95 ADDED LOGIC
		endif
.END PATCH 1.8 ADDED LOGIC
        elseif (FrmPtr = 3)     .Detail Records
.Initialize some values
                move    C3,NCSHLOCK	.No locks
                move	C0,N1		.used to determine if at least one valid key field entered
                move    C0,N2           .used to determind which type of Search is being performed
.Verify Data Format
                clear	NCSHFLD
                clear	NCSHFLD1
                clear	NCSHFLD2
.START PATCH 1.4 ADDED LOGIC
		clear	NCSHFLD5
.END PATCH 1.4 ADDED LOGIC
.
                getitem	CashEditLR,0,NCSHFLD3
                call    Trim using NCSHFLD3
                call    ZFillIt using NCSHFLD3,C0
                if (NCSHFLD3 = "" OR NCSHFLD3 = "000000")
                        add	C1,N1
                else
                        move    C1,N2
                endif
.
                clear	NCSHFLD4
.START PATCH 1.95 REPLACED LOGIC
.                getitem CashEditCheckNum,0,str6
.                call    Trim using str6
.                call    ZFillIt using str6,C0
.                if (str6 <> "")
.                        append	"04X",NCSHFLD4
.                        append	str6,NCSHFLD4
                getitem CashEditCheckNum,0,str12
                call    Trim using str12
                call    ZFillIt using str12,C0
                if (str12 <> "")
                        append	"04X",NCSHFLD4
                        append	str12,NCSHFLD4
.END PATCH 1.95 REPLACED LOGIC
                        reset	NCSHFLD4
                        if (N2 = C0)
                                move    C2,N2
                        endif
                else
                        add	C1,N1
                endif
                if (N1 = 2)
                        clear	taskname
                        append	"You must supply at least one of the following:",taskname
                        append	carr,taskname
                        append	carr,taskname
                        append	"                LR Number",taskname
                        append	carr,taskname
                        append	"                Check Number",taskname
                        reset	taskname
                        alert	caution,taskname,result
                        setprop CashRetrieve,enabled=1
                        setprop CashQuit,enabled=1
                        setfocus CashEditLR
                        return
                endif
.Start Searching
                if (N2 = 1)
                        move	"CashRetrieve-NCSHKEY",Location
                        pack	KeyLocation,"Key: ",NCSHFLD3
                        call	NCSHKEY
                else
                        move	"CashRetrieve-NCSHAIM",Location
                        pack	KeyLocation,"Key: ",NCSHFLD4
                        call	NCSHAIM
                endif
        	if over
                        alert	caution,"No Records Found!",result
                        setprop CashRetrieve,enabled=1
                        setprop CashQuit,enabled=1
			setfocus CashEditLR
                        return
                endif
.START PATCH 1.95 ADDED LOGIC
		getitem	CashEditCheckDate,0,DateHold
		call	RemoveChar using DateHold,SLASH
		call	Trim using DateHold
		if (DateHold <> "")
			unpack	DateHold,str4,str5
			pack	DateHold,str5,str4
		        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
		        move    "CashRetrieve-NCHKKEY",Location
        		pack    KeyLocation,"Key: ",NCHKFLD
	        	call    NCHKKEY
			pack	DateHold2,NCHKDATE
		endif
.END PATCH 1.95 ADDED LOGIC
                if (N2 = 1 AND NCSHFLD4 <> "")  .ISAM Search with entry for Check Number
                        bump    NCSHFLD4,3
                        if (NCSHFLD4 = NCSHCHK)
.START PATCH 1.95 REPLACED LOGIC
.                                call    CashLoadListView2
				if (DateHold <> "")
					if (DateHold2 = DateHold)
						call    CashLoadListView2
					endif
				else
					call    CashLoadListView2
				endif
.END PATCH 1.95 REPLACED LOGIC
                        endif
                else
.START PATCH 1.95 REPLACED LOGIC
.			call    CashLoadListView2
			if (DateHold <> "")
				if (DateHold2 = DateHold)
					call    CashLoadListView2
				endif
			else
				call    CashLoadListView2
			endif
.END PATCH 1.95 REPLACED LOGIC
                endif
..Set up Icon to display while searching
.        moveaddr NCSHONE3,AnimateWindow
.        move    C1,AnimateCurIcon
.        move    C4,AnimateFrames
.        move    C0,AnimateIconID
..position Icon to sit to left of FindIt button
.        move    AniH,H
.        move    AniV,V
.Reset NewFlag to allow breaking of loop, Allow breaking of loop via Quit button
                move	"F",NewFlag
                setprop	CashQuit,enabled=1
                if (N2 = 1)
                        move	"CashRetrieve-NCSHKS",Location
                else
                        move	"CashRetrieve-NCSHKG",Location
                endif
                loop
.                        call	ANIMATEIT
                        if (N2 = 1)
                                call    NCSHKS
                                if (CLR <> NCSHFLD3)
                                        break
                                endif
                        else
                                call	NCSHKG
                        endif
                        until over
.START PATCH 1.95 ADDED LOGIC
		        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
		        move    "CashRetrieve2-NCHKKEY",Location
        		pack    KeyLocation,"Key: ",NCHKFLD
	        	call    NCHKKEY
			pack	DateHold2,NCHKDATE
.END PATCH 1.95 ADDED LOGIC
                        if (N2 = 1 AND NCSHFLD4 <> "")  .ISAM Search with entry for Check Number
                                if (NCSHFLD4 = NCSHCHK)
.START PATCH 1.95 REPLACED LOGIC
.					call    CashLoadListView2
					if (DateHold <> "")
						if (DateHold2 = DateHold)
							call    CashLoadListView2
						endif
					else
						call    CashLoadListView2
					endif
.END PATCH 1.95 REPLACED LOGIC
                                endif
                        else
.START PATCH 1.95 REPLACED LOGIC
.				call    CashLoadListView2
				if (DateHold <> "")
					if (DateHold2 = DateHold)
						call    CashLoadListView2
					endif
				else
					call    CashLoadListView2
				endif
.END PATCH 1.95 REPLACED LOGIC
                        endif
                        eventcheck
                        until (NewFlag = "S")
                repeat
.                destroy AnimateIcon
.Modify Screen
                call	CashDisableLowerEdit2
                call	CashEnableButtons1
                call	CashEnableUpper
                setprop CashQuit,enabled=0
                if (View1Flag = 1)
                        CashListView2.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView2.EnsureVisible using 0,0
                        setfocus CashListView2
                        call	Click_CashListView2
                elseif (View1Flag = 2)
                        CashListView2A.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView2A.EnsureVisible using 0,0
                        setfocus CashListView2A
                        call	Click_CashListView2A
                elseif (View1Flag = 3)
                        CashListView2B.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView2B.EnsureVisible using 0,0
                        setfocus CashListView2B
                        call	Click_CashListView2B
                elseif (View1Flag = 4)
                        CashListView2C.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView2C.EnsureVisible using 0,0
                        setfocus CashListView2C
                        call	Click_CashListView2C
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        CashListView2D.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
                        CashListView2D.EnsureVisible using 0,0
                        setfocus CashListView2D
                        call	Click_CashListView2D
.END PATCH 1.95 REPLACED LOGIC
                endif
        	setprop CashListView2,enabled=1
        	setprop CashListView2A,enabled=1
        	setprop CashListView2B,enabled=1
        	setprop CashListView2C,enabled=1
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=1
.END PATCH 1.95 REPLACED LOGIC
.START PATCH 1.8 ADDED LOGIC
	        if (View1Flag = 1)
	                setfocus CashListView2
	        elseif (View1Flag = 2)
	                setfocus CashListView2A
	        elseif (View1Flag = 3)
	                setfocus CashListView2B
	        elseif (View1Flag = 4)
	                setfocus CashListView2C
.START PATCH 1.95 REPLACED LOGIC
	        elseif (View1Flag = 5)
	                setfocus CashListView2D
.END PATCH 1.95 REPLACED LOGIC
	        endif
.END PATCH 1.8 ADDED LOGIC
        endif
.Reset Flags
        move	NO,NewFlag
        move	YES,ExitFlag
        setprop	CashComboBox,enabled=1
        setitem CashStatModify,0,""
.IMPORTANT - KEEP THIS!!! Clears acumulated LostFocus, GotFocus, etc. events
.that have gathered up from Enabling\Disabling Objects
        LOOP
                CLEAREVENT
                UNTIL OVER
        REPEAT
        return

CashTestControl LRoutine DimPtr,FrmPtr2
        move    C0,FrmPtr2
        move    DimPtr,NCTRFLD
        move    "TestCont.-NCTRKEY",Location
        pack    KeyLocation,"Key: ",NCTRFLD
        call    NCTRKEY
        if not over
                if (NCTRCODE = 2)
                        alert   caution,"This Control is Closed!!",result
                        move    C1,FrmPtr2
		elseif (NCTRCODE = 1)
			move	"x",progcode
			setitem	PasswordEdit,0,""
			setfocus PasswordEdit
			setprop	Passwrd,visible=1
.Patch1.3
.                        move "COSMO" to npaskey
.			if (NPASKEY <> "COSMO" AND NPASKEY <> "cosmo")
			if (NPASKEY <> "xCOSMO" AND NPASKEY <> "xcosmo")
.Patch1.3
				move	C1,FrmPtr2
			endif
			move	"C",progcode
                endif
        else
                alert   caution,"This Control does not exist!!",result
                move    C1,FrmPtr2
        endif
        return

CashRunQuit LRoutine FrmPtr
        call    CashDisableButtons2
        if (FrmPtr = 1)
                call	CashDisableUpper2
                call	CashEnableButtons1
.Refresh Variables
                if (NewFlag = YES)
.START PATCH 1.4 REPLACED LOGIC
.                        setitem CashSearchControl,0,HoldCont
			unpack	HoldCont,str3
                        setitem CashSearchControl,0,str3
.END PATCH 1.4 REPLACED LOGIC
                        call    Click_CashOK
                else
                        move    HoldCont,NCTRFLD
                        move    "Quit-NCTRKEY",Location
                        pack    KeyLocation,"Key: ",NCTRFLD
                        call    NCTRKEY
                        setitem CashSearchControl,0,NCTRNUM
                        call    CashLoadScreen1
                endif
                setprop CashSearchControl,readonly=0
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditControlDate,readonly=0
.END PATCH 1.4 ADDED LOGIC
        elseif (FrmPtr = 2)
                call	CashDisableLowerEdit1
                call	CashEnableButtons1
                call	CashEnableUpper
                setprop CashEditChkControl,readonly=0
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditChkContDate,readonly=0
.END PATCH 1.4 ADDED LOGIC
.                setprop CashEditChkCheck,readonly=0
.Refresh Variables
                call    CashSetItem using HoldChk
                if (ViewFlag = 1)
                        call	Click_CashListView
                elseif (ViewFlag = 2)
                        call	Click_CashListViewA
                elseif (ViewFlag = 3)
                        call	Click_CashListViewB
                elseif (ViewFlag = 4)
                        call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                endif
        elseif (FrmPtr = 3)
                call	CashDisableLowerEdit2
                call	CashEnableButtons1
                call	CashEnableUpper
        	if (Cash2Flag <> C0)
                        goto CashRunQuitEnd
	        endif
.Refresh Variables
                call    CashSetItem2 using HoldCsh
                if (View1Flag = 1)
                        call	Click_CashListView2
                elseif (View1Flag = 2)
                        call	Click_CashListView2A
                elseif (View1Flag = 3)
                        call	Click_CashListView2B
                elseif (View1Flag = 4)
                        call	Click_CashListView2C
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        call	Click_CashListView2D
.END PATCH 1.95 REPLACED LOGIC
                endif
        endif
CashRunQuitEnd
	move	C0,SecFlag
        move    NO,NewFlag
        setitem CashStatModify,0,""
        CashListView.GetItemCount giving result
        if (result > C0)
        	setprop CashListView,enabled=1
        	setprop CashListViewA,enabled=1
        	setprop CashListViewB,enabled=1
        	setprop CashListViewC,enabled=1
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=1
.END PATCH 1.95 ADDED LOGIC
        endif
        CashListView2.GetItemCount giving result
        if (result > C0)
        	setprop CashListView2,enabled=1
        	setprop CashListView2A,enabled=1
        	setprop CashListView2B,enabled=1
        	setprop CashListView2C,enabled=1
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=1
.END PATCH 1.95 REPLACED LOGIC
        endif
        move    YES,ExitFlag
       	if (Cash2Flag <> C0)
                move	C2,Cash2Flag
                goto Cash2WriteToDetailEnd1
        endif
        return

CashRunSave LRoutine FrmPtr
        call    CashDisableButtons2
        move	NO,ReturnFlag
        if (FrmPtr = 1)
                call	CashVerifyControl
                if (ReturnFlag = YES)
                        call	CashDisableUpper
                        if (NewFlag = YES)
                                setitem CashStatModify,0,"Currently Creating a New Control."
                        else
                                setitem CashStatModify,0,"Currently Modifying a Control."
                        endif
                        call    CashEnableUpper2
                        call    CashEnableButtons2
                        return
                endif
                if (NewFlag = YES)
.START PATCH 1.4 REPLACED LOGIC
.                        pack    NCTRFLD,NCTRNUM
                        pack    NCTRFLD,NCTRNUM,NCTRDATE
.END PATCH 1.4 REPLACED LOGIC
.START PATCH 1.9 REPLACED LOGIC
.                        move    C0,NCTRCODE
			if (SecFlag <> 1)
				move    C0,NCTRCODE
			endif
.END PATCH 1.9 REPLACED LOGIC
                        move    "Save-NCTRWRT",Location
                        pack    KeyLocation,"Key: ",NCTRFLD
                        call    NCTRWRT
.Update GetNext File
                        move    "NCSHNEXT",GNXTFLD
                        move    "Save-GNXTTST",Location
                        pack    KeyLocation,"Key: ",GNXTFLD
.Make certain I am sitting on right record
                        call    GNXTTST
                        move    C0,N6
                        move    NCTRNUM,N6
                        move    N6,GNXTNUM
                        move    "Save-GNXTUPD",Location
                        call    GNXTUPD
                else
.START PATCH 1.4 ADDED LOGIC
                        pack    NCTRFLD,NCTRNUM,NCTRDATE
.START PATCH 1.9 REPLACED LOGIC
.                        move    C0,NCTRCODE
			if (SecFlag <> 1)
				move    C0,NCTRCODE
			endif
.END PATCH 1.9 REPLACED LOGIC
                        move    "Save-NCTRTST",Location
                        pack    KeyLocation,"Key: ",NCTRFLD
                        call    NCTRTST
.END PATCH 1.4 ADDED LOGIC
                        move    "Save-NCTRUPD",Location
                        pack    KeyLocation,"Key: ",NCTRFLD
                        call    NCTRUPD
                endif
                call	CashDisableUpper2
                call	CashEnableButtons1
                setprop CashSearchControl,readonly=0
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditControlDate,readonly=0
.END PATCH 1.4 ADDED LOGIC
                call    Click_CashOK
        elseif (FrmPtr = 2)
                call	CashVerifyCheck
                if (ReturnFlag = YES)
        	        call	CashDisableUpper
                        if (NewFlag = YES)
                                setitem CashStatModify,0,"Currently Creating a New Check."
                        else
                                setitem CashStatModify,0,"Currently Modifying a Check."
                        endif
                        call    CashEnableLowerEdit1
                        call    CashEnableButtons2
                        return
                endif
.START PATCH 1.4 REPLACED LOGIC
.                pack    NCHKFLD,NCHKCONT,NCHKNUM
                pack    NCHKFLD,NCHKCONT,NCHKCONTD,NCHKNUM
.END PATCH 1.4 REPLACED LOGIC
                if (NewFlag = YES)
                        move    "Save-NCHKWRT",Location
                        pack    KeyLocation,"Key: ",NCHKFLD
                        call    NCHKWRT
                        getitem CashStatMasterCont,0,str3
                        if (str3 = NCHKCONT)
                                call    CashLoadListView
                        endif
                else
                        move    "Save-NCHKTST",Location
                        call    NCHKTST
                        if over         .D'oh
				pack    NCHKFLD,HoldChk
	                        move    "Save2-NCHKTST",Location
        	                call    NCHKTST
				if over
	                                alert   caution,"Check Record does not exist!",result
				else
		                        move    "Save-NCHKDEL",Location
        		                pack    KeyLocation,"Key: ",NCHKFLD
                		        call    NCHKDEL
.START PATCH 1.4 REPLACED LOGIC
.					pack    NCHKFLD,NCHKCONT,NCHKNUM
					pack    NCHKFLD,NCHKCONT,NCHKCONTD,NCHKNUM
.END PATCH 1.4 REPLACED LOGIC
		                        move    "Save2-NCHKWRT",Location
        		                pack    KeyLocation,"Key: ",NCHKFLD
                		        call    NCHKWRT
				endif
                        else
                                move    "Save-NCHKUPD",Location
                                pack    KeyLocation,"Key: ",NCHKFLD
                                call    NCHKUPD
                        endif
                endif
                call	CashDisableLowerEdit1
                call	CashEnableButtons1
                call	CashEnableUpper
                setprop CashEditChkControl,readonly=0
.START PATCH 1.4 ADDED LOGIC
                setprop CashEditChkContDate,readonly=0
.END PATCH 1.4 ADDED LOGIC
.                setprop CashEditChkCheck,readonly=0
                if (NewFlag <> YES)
                        call    CashLoadChecks
                endif
                call    CashSetItem using NCHKFLD
                if (ViewFlag = 1)
                        call	Click_CashListView
                elseif (ViewFlag = 2)
                        call	Click_CashListViewA
                elseif (ViewFlag = 3)
                        call	Click_CashListViewB
                elseif (ViewFlag = 4)
                        call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                endif
        elseif (FrmPtr = 3)
                call	CashVerifyDetail
                if (ReturnFlag = YES)
        	        call	CashDisableUpper
.START PATCH 1.1 REPLACED LOGIC
.                        if (NewFlag = YES)
.                                setitem CashStatModify,0,"Currently Creating a New Detail Record."
.                        else
.                                setitem CashStatModify,0,"Currently Modifying a Detail Record."
.                        endif
.                        call    CashEnableLowerEdit2
...........
                        if (NewFlag = YES)
                                setitem CashStatModify,0,"Currently Creating a New Detail Record."
	                        call    CashEnableLowerEdit2A
                        else
                                setitem CashStatModify,0,"Currently Modifying a Detail Record."
	                        call    CashEnableLowerEdit2
                        endif
.END PATCH 1.1 REPLACED LOGIC
                        call    CashEnableButtons2
                	return
                endif
        	clear	NCSHFLD
	        clear	NCSHFLD1
	        clear	NCSHFLD2
	        clear	NCSHFLD3
        	clear	NCSHFLD4
.START PATCH 1.4 ADDED LOGIC
		clear	NCSHFLD5
.END PATCH 1.4 ADDED LOGIC
                if (NewFlag = YES)
                        call    CashUpdateCID
                        if (N2 >= 98)
                                alert   caution,"Unable to create record.  Contact I.S.!",result
                        else
                                move    "Save-NCSHWRT",Location
                                call    NCSHWRT
.
                                if (CEXTCD = "O" | CEXTCD = "Q" | CEXTCD = "D" | CEXTCD = "d" | CEXTCD = "N")
                                        clear   MOAVARS
                                        if (CEXTCD = "O" | CEXTCD = "Q")
                                                move    AMOUNTB,CHANGE
                                                move    "07",REASON
                                        elseif (CEXTCD = "D" | CEXTCD = "d" | CEXTCD = "N")
                                                mult    SEQ,AMOUNTB
                                                move    AMOUNTB,CHANGE
;Begin patch 1.93   code for popup here
;get text from popup
;can't do the write here as we dont have trans number yet - so get text and set a flag
						call	SetMoaNotes
						setprop	Report2,visible=1
						Getitem	EditTextBoxes(1),0,MOANotes
						call	Trim using MOAnotes
						IF (Moanotes ="")
							MOve	No to MOANoteYesNo
						else
							MOve	Yes to MOANoteYesNo
						endif
;end patch 1.93     code for popup here
                                                if (cextcd = "D")
                                                        move    "11",REASON
                                                elseif (cextcd = "d")
                                                        move    "13",REASON
                                                elseif (cextcd = "N")
                                                        move    "10",REASON
                                                endif
                                        endif
                                        pack    TRANDATE,CCE,CYR,CMO,CDY
                                        pack    RECDATE,timestamp
                                        move    "NONANXT",GNXTFLD
                                        move    "RunSave3-GNXTKEY",Location
                                        pack    KeyLocation,"Key: ",GNXTFLD
                                        call    GNXTKEY
                                        move    GNXTNUM,N7
                                        loop
                                                add     C1,N7
                                                move    N7,TRANSNUM
                                                rep     ZFILL,TRANSNUM
                                                move    TRANSNUM,NMOAFLD
                                                move    C2,NMOAPATH
                                                move    "RunSave3-NMOATST",Location
                                                pack    KeyLocation,"Key: ",NMOAFLD
                                                call    NMOATST
                                                until over
                                        repeat
                                        move    N7,str7
                                        bump    str7 BY 1
                                        move    str7,GNXTNUM
                                        rep     ZFILL,GNXTNUM
                                        move    "RunSave3-GNXTUPD",Location
                                        pack    KeyLocation,"Key: ",GNXTFLD
                                        call    GNXTUPD
.NOTE GNXT ONLY ALLOWS 6 BYTE NUMBER. DLH.
.                                        clear   MOAVARS
                                        move    "000",MCNT
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                        pack    NMOAFLD,CMLR,MCNT
					pack	COMPFLD,CMLR
					move	"RunSave3-COMPKEY",Location
					pack	KeyLocation,"Key: ",COMPFLD
					call	COMPKEY
                                        pack    NMOAFLD,COMPOLDMLR,MCNT
					move	COMPOLDMLR,str4
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                        move    C4,NMOAPATH
                                        move    CLR,LRNUM
.                                        pack    MKEY,CMLR,"000"
.                                        move    "RunSave3-NMLRKEY",Location
.                                        pack    KeyLocation,"Key: ",MKEY
.                                        call    NMLRKEY
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                        pack    NMOAFLD4,NCSHBRK,CMLR
					pack	COMPFLD,NCSHBRK
					move	"RunSave3B-COMPKEY",Location
					pack	KeyLocation,"Key: ",COMPFLD
					call	COMPKEY
					call	Trim using COMPOLDBRK
					if (COMPOLDBRK = "")
						move	"0000",COMPOLDBRK
					endif
                                        pack    NMOAFLD4,COMPOLDBRK,str4
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                        rep     ZFILL,NMOAFLD4
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                        move    CMLR,MLR
.                                        move    CMLR,NMOBMLR
                                        move    str4,MLR
                                        move    str4,NMOBMLR
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                        move    MCNT,NMOBMCNT
                                        move    CNUM,CONTROL
.                                        call    Trim using LRNUM
                                        if (LRNUM <> "" AND LRNUM <> "      ")
                                                move    INVNUM,INVOICE
                                                call    TRIM using INVDTEY
                                                count   N1,INVDTEY
                                                if (N1 > 0)
                                                        pack    INVDATE,INVDTEC,INVDTEY,INVDTEM,INVDTED
                                                endif
                                        endif
                                        move    AMOUNTB,ONAMOUNT
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                        move    NCSHBRK,NMOABRK
.					rep	zfill,NMOABRK
.                                        move    NCSHBRK,NMOBBRK
                                        move    COMPOLDBRK,NMOABRK
					rep	zfill,NMOABRK
                                        move    COMPOLDBRK,NMOBBRK
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
					rep	zfill,NMOBBRK
                                        move    "0",INAMOUNT
                                        move    OLNUM,LIST
.START PATCH 1.95 REPLACED LOGIC
.                                        move    NCSHCHK,CHECKNUM
                                        unpack	NCSHCHK,str6,CHECKNUM
.END PATCH 1.95 REPLACED LOGIC
                                        move    C2,NMOBPATH
                                        move    INITS,NMOAINIT
                                        move    "RunSave3-NMOAWRT",Location
                                        pack    KeyLocation,"Key: ",NMOAFLD
                                        call    NMOAWRT
                                        move    "RunSave3-NMOBUPD",Location
                                        call    NMOBUPD
                                endif
;Begin patch 1.93
				If (MoaNoteYesNo = yes)
					PackKey	MOANotesFld,transnum
					call	MOANotesTsT
					if Not over
						alert	Plain,"This will overwrite the current note !",result
						IF (result = c1)
							call	MOANotesUpd
						else
							call	MOANotesKey
							return
						endif
					else
						PackKey	MOANotesKey,transnum
						call	MOANoteswrt
					endif
					move	No to MoaNoteYesNo
				endif
;end patch 1.93
                                getitem CashStatMasterCont,0,str3
                                if (str3 = CNUM)
					unpack	CID,CID
					unpack	CMLR,CMLR
					unpack	CLR,CLR
					unpack	CFILL1,CFILL1
					unpack	CCE,CCE
					unpack	CYR,CYR
					unpack	CMO,CMO
					unpack	CDY,CDY
					unpack	CEXTCD,CEXTCD
					unpack	CFILL2,CFILL2
					unpack	CNUM,CNUM
					unpack	NCSHCHK,NCSHCHK
					unpack	nckdtec,nckdtec
					unpack	nckdtey,nckdtey
					unpack	nckdtem,nckdtem
					unpack	nckdted,nckdted
					unpack	npayor,npayor
					unpack	NCSHBRK,NCSHBRK
					unpack	CNUMDATE,CNUMDATE
					unpack	CFiller,CFiller
.
					reset	CID
					reset	CMLR
					reset	CLR
					reset	CFILL1
					reset	CCE
					reset	CYR
					reset	CMO
					reset	CDY
					reset	CEXTCD
					reset	CFILL2
					reset	CNUM
					reset	NCSHCHK
					reset	nckdtec
					reset	nckdtey
					reset	nckdtem
					reset	nckdted
					reset	npayor
					reset	NCSHBRK
					reset	CNUMDATE
					reset	CFiller
.
					setlptr CID
					setlptr CMLR
					setlptr	CLR
					setlptr	CFILL1
					setlptr	CCE
					setlptr	CYR
					setlptr	CMO
					setlptr	CDY
					setlptr	CEXTCD
					setlptr	CFILL2
					setlptr	CNUM
					setlptr	NCSHCHK
					setlptr	nckdtec
					setlptr	nckdtey
					setlptr	nckdtem
					setlptr	nckdted
					setlptr	npayor
					setlptr	NCSHBRK
					setlptr	CNUMDATE
					setlptr	CFiller
.
                                        call    CashLoadListView2
                                endif
                                pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
                                call    CashSetItem2 using HoldCsh
                        endif
                else
                        pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
.If Check Number has changed make sure to update CID
                        if (ChkFlag1 = YES | ChkFlag1 = "2")
                                call    CashUpdateCID
                                if (N2 >= 98)
                                        alert   caution,"Unable to create record.  Contact I.S.!",result
                                endif
.                                pack    HoldCsh,CID,CLR,CAMOUNT,CNUM,NCSHCHK
.START PATCH 1.4 REPLACED LOGIC
.                                pack    HoldChk,CNUM,NCSHCHK
                                pack    HoldChk,CNUM,CNUMDATE,NCSHCHK
.END PATCH 1.4 REPLACED LOGIC
                        endif
                        call    CashPackTemp
.Allow modification of LR Number
                        pack    NCSHFLD3,HoldCLR
                        pack    KeyLocation,"Key: ",NCSHFLD3
                        move    "Save-NCSHKEY",Location
                        call    NCSHKEY
                        loop
                                until over
                                until (NCSHFLD3 <> HoldCLR)
                                until (HoldCNUM = CNUM & HoldNCSHCHK = NCSHCHK & HoldCID = CID & HoldCAMOUNT = CAMOUNT)
                                move    "Save-NCSHKS",Location
                                call    NCSHKS
                        repeat
                        if over                         .D'oh
                                alert   caution,"Detail Record does not exist!",result
                        elseif (NCSHFLD3 <> HoldCLR)    .D'oh
                                alert   caution,"Detail Record does not exist!",result
                        else
                                call    CashPackTemp2
                                move    "Save-NCSHUPD",Location
                                call    NCSHUPD
                        endif
                endif
                call	CashDisableLowerEdit2
                call	CashEnableButtons1
                call	CashEnableUpper
                if (NewFlag <> YES)
                        call    CashLoadDetails
                endif
.If New Check has been Added Refresh the Check 'Screen'
                if (ChkFlag1 = "2")
                        call    CashLoadChecks
                        call    CashSetItem using HoldChk
                        if (ViewFlag = 1)
                                call	Click_CashListView
                        elseif (ViewFlag = 2)
                                call	Click_CashListViewA
                        elseif (ViewFlag = 3)
                                call	Click_CashListViewB
                        elseif (ViewFlag = 4)
                                call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                        elseif (ViewFlag = 5)
                                call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                        endif
                else    .Refresh UnApplied Amount
                        call    CashFindCheckUnApplied
                        if (ViewFlag = 1)
                                call	Click_CashListView
                        elseif (ViewFlag = 2)
                                call	Click_CashListViewA
                        elseif (ViewFlag = 3)
                                call	Click_CashListViewB
                        elseif (ViewFlag = 4)
                                call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                        elseif (ViewFlag = 5)
                                call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                        endif
                endif
.        	if (Cash2Flag <> C0)
.                        goto CashRunSaveEnd
.	        endif
                call    CashSetItem2 using HoldCsh
                if (View1Flag = 1)
                        call	Click_CashListView2
                elseif (View1Flag = 2)
                        call	Click_CashListView2A
                elseif (View1Flag = 3)
                        call	Click_CashListView2B
                elseif (View1Flag = 4)
                        call	Click_CashListView2C
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        call	Click_CashListView2D
.END PATCH 1.95 REPLACED LOGIC
                endif
        endif
CashRunSaveEnd
        if (NewFlag = YES)
		setprop	CashNew,default=1
                setfocus CashNew
        endif
	move	C0,SecFlag
        move    NO,NewFlag
        setitem CashStatModify,0,""
        CashListView.GetItemCount giving result
        if (result > C0)
        	setprop CashListView,enabled=1
        	setprop CashListViewA,enabled=1
        	setprop CashListViewB,enabled=1
        	setprop CashListViewC,enabled=1
.START PATCH 1.95 ADDED LOGIC
        	setprop CashListViewD,enabled=1
.END PATCH 1.95 ADDED LOGIC
        endif
        CashListView2.GetItemCount giving result
        if (result > C0)
        	setprop CashListView2,enabled=1
        	setprop CashListView2A,enabled=1
        	setprop CashListView2B,enabled=1
        	setprop CashListView2C,enabled=1
.START PATCH 1.95 REPLACED LOGIC
        	setprop CashListView2D,enabled=1
.END PATCH 1.95 REPLACED LOGIC
        endif
        move    YES,ExitFlag
       	if (Cash2Flag <> C0)
       		move	C3,Cash2Flag
                goto Cash2WriteToDetailEnd1
       	endif
        return

CashUpdateCID
        move	C2,NCSHPATH
        move    C0,N2
        pack	NCSHFLD,"02X",CAMOUNT
        pack	NCSHFLD2,"03X",CNUM
        pack	NCSHFLD3,CLR
        pack	NCSHFLD4,"04X",NCSHCHK
.START PATCH 1.4 ADDED LOGIC
	pack	NCSHFLD5,"05X",CNUMDATE
.END PATCH 1.4 ADDED LOGIC
        move    "Save-NCSHAIMA",Location
        loop
                move    N2,CID
                rep     zfill,CID
	        pack	NCSHFLD1,"01X",CID
.START PATCH 1.4 REPLACED LOGIC
.                pack	KeyLocation,"Key: ",NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD3,NCSHFLD4
                pack	KeyLocation,"Key: ",NCSHFLD1,NCSHFLD,NCSHFLD2,NCSHFLD3,NCSHFLD4,NCSHFLD5
.END PATCH 1.4 REPLACED LOGIC
                call    NCSHAIMA
                until over
                until (N2 >= 98)
                add     C1,N2
        repeat
        return

CashFindCheckUnApplied
.START PATCH 1.95 REPLACED LOGIC
.        move    NCSHCHK,str7
        move    NCSHCHK,str13
.END PATCH 1.95 REPLACED LOGIC
        clear   str25
.START PATCH 1.4 REPLACED LOGIC
.        pack    NCHKFLD,CNUM,NCSHCHK
        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
.END PATCH 1.4 REPLACED LOGIC
        move    "FindUnApplied-NCHKKEY",Location
        pack    KeyLocation,"Key: ",NCHKFLD
        call    NCHKKEY
        if not over
                move    C0,Temp122
                move    C0,Temp122B
                move    C0,Temp122C
                move    NCHKAMT,Temp122
                clear   str2
.START PATCH 1.4 REPLACED LOGIC
.                call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,C0,str2,Temp122C
                call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,C0,str2,Temp122C,NCHKCONTD
.END PATCH 1.4 REPLACED LOGIC
                if (Temp122 <> C0)
                        move    Temp122,str15
                        unpack  str15,str12,str3
                        call    Trim using str12
                        if (Temp122 > C0)
                                call    FormatNumeric using str12,str15
                                pack    str25,str15,str3
                        elseif (Temp122 < C0)
                                call    RemoveChar using str12,DASH
                                call    FormatNumeric using str12,str15
                                pack    str18,str15,str3
                                pack    str25,"(",str18,")"
                        endif
                endif
        endif
.
        move    SEQ,N8
        CashListView.GetItemCount giving result
        if (result > C0)
                for N9 from C0 to result
.START PATCH 1.95 REPLACED LOGIC
.                        CashListView.GetItemText giving str6 using N9,1
.                        if (str6 = str7)
                        CashListView.GetItemText giving str12 using N9,1
                        if (str12 = str13)
.END PATCH 1.95 REPLACED LOGIC
                                move    N9,N8
                                break
                        endif
                repeat
                if (N8 > SEQ)
                        CashListView.SetItemText using N8,str25,4
                endif
        endif
.
        move    SEQ,N8
        CashListViewA.GetItemCount giving result
        if (result > C0)
                for N9 from C0 to result
.START PATCH 1.95 REPLACED LOGIC
.                        CashListViewA.GetItemText giving str6 using N9,0
.                        if (str6 = str7)
                        CashListViewA.GetItemText giving str12 using N9,0
                        if (str12 = str13)
.END PATCH 1.95 REPLACED LOGIC
                                move    N9,N8
                                break
                        endif
                repeat
                if (N8 > SEQ)
                        CashListViewA.SetItemText using N8,str25,4
                endif
        endif
.
        move    SEQ,N8
        CashListViewB.GetItemCount giving result
        if (result > C0)
                for N9 from C0 to result
.START PATCH 1.95 REPLACED LOGIC
.                        CashListViewB.GetItemText giving str6 using N9,3
.                        if (str6 = str7)
                        CashListViewB.GetItemText giving str12 using N9,3
                        if (str12 = str13)
.END PATCH 1.95 REPLACED LOGIC
                                move    N9,N8
                                break
                        endif
                repeat
                if (N8 > SEQ)
                        CashListViewB.SetItemText using N8,str25,5
                endif
        endif
.
        move    SEQ,N8
        CashListViewC.GetItemCount giving result
        if (result > C0)
                for N9 from C0 to result
.START PATCH 1.95 REPLACED LOGIC
.                        CashListViewC.GetItemText giving str6 using N9,2
.                        if (str6 = str7)
                        CashListViewC.GetItemText giving str12 using N9,2
                        if (str12 = str13)
.END PATCH 1.95 REPLACED LOGIC
                                move    N9,N8
                                break
                        endif
                repeat
                if (N8 > SEQ)
                        CashListViewC.SetItemText using N8,str25,5
                endif
        endif
.START PATCH 1.95 ADDED LOGIC
.
        move    SEQ,N8
        CashListViewD.GetItemCount giving result
        if (result > C0)
                for N9 from C0 to result
                        CashListViewD.GetItemText giving str12 using N9,2
                        if (str12 = str13)
                                move    N9,N8
                                break
                        endif
                repeat
                if (N8 > SEQ)
                        CashListViewD.SetItemText using N8,str25,5
                endif
        endif
.END PATCH 1.95 ADDED LOGIC
        return

CashRunDelete LRoutine FrmPtr
        if (FrmPtr = 1)
.NOT AN OPTION
        elseif (FrmPtr = 2)
                call    CashDisableButtons2
                call	CashDisableLowerEdit1
                call	CashEnableButtons1
                call	CashEnableUpper
        	clear	NCHKFLD
                pack    NCHKFLD,HoldCHK
                pack    KeyLocation,"Key: ",NCHKFLD
                move    "Delete-NCHKTST",Location
                call    NCHKTST
                if over                         .D'oh
                        alert   caution,"Check Record does not exist!",result
                else
                        move    "Delete-NCHKDEL",Location
                        call    NCHKDEL
		endif
        elseif (FrmPtr = 3)
		if (SecFlag <> C1)
        	        move    "POQ",str6
	                scan    CEXTCD,str6
                	if equal
.You should never get here, but double-checking
        	                clear   taskname
	                        append  "Deletion not allowed",taskname
                	        append  NewLine,taskname
        	                append  "on Records with that External Code!",taskname
	                        reset   taskname
                		alert   caution,taskname,result
        	                return
	                endif
		endif
                call    CashDisableButtons2
                call	CashDisableLowerEdit2
                call	CashEnableButtons1
                call	CashEnableUpper
        	clear	NCSHFLD
	        clear	NCSHFLD1
	        clear	NCSHFLD2
	        clear	NCSHFLD3
        	clear	NCSHFLD4
.START PATCH 1.4 ADDED LOGIC
		clear	NCSHFLD5
.END PATCH 1.4 ADDED LOGIC
.                pack    NCSHFLD3,CLR
                pack    NCSHFLD3,HoldCLR
                pack    KeyLocation,"Key: ",NCSHFLD3
                move    "Delete-NCSHKEY",Location
                call    NCSHKEY
                loop
                        until over
.                        until (NCSHFLD3 <> CLR)
                        until (NCSHFLD3 <> HoldCLR)
                        until (HoldCNUM = CNUM & HoldNCSHCHK = NCSHCHK & HoldCID = CID & HoldCAMOUNT = CAMOUNT)
                        move    "Delete-NCSHKS",Location
                        call    NCSHKS
                repeat
                if over                         .D'oh
                        alert   caution,"Detail Record does not exist!",result
.                elseif (NCSHFLD3 <> CLR)        .D'oh
                elseif (NCSHFLD3 <> HoldCLR)    .D'oh
                        alert   caution,"Detail Record does not exist!",result
                else
                        move    "Delete-NCSHDEL",Location
                        call    NCSHDEL
...................................................................
                        move    CLR,NINVFLD
                        move    C1,NINVPATH
                        move    "Delete-NINVKEY",Location
                        pack    KeyLocation,"Key: ",NINVFLD
                        call    NINVKEY
                        clear   str6
                        append  "CSH",str6
                        append  CNUM,str6
                        reset   str6
.                        match   CNUM,CHKN1          .all ready updated invoice record?
.                        if equal                    .yes
                        if (str6 = CHKN1)
                                clear   CHKN1
                                clear   MLRPAYR
                                clear   MLRPAYD
                                move    "Delete-NINVUPD",Location
                                call    NINVUPD
                        endif
                        if (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N")
.I am still using this code as OLNUM is used later in code
                                move    CLR,NORDFLD
                                move    C1,NORDPATH
                                move    "Delete-NORDKEY",Location
                                pack    KeyLocation,"Key: ",NORDFLD
                                call    NORDKEY
.Old code which is not needed now that we have a place to store the Broker number in Cash file
.                                if over                       .18mar96 need broker number if over
.                                        keyin   *p05:18,"BRK ",*dv,obrknum:
.                                                *p05:18,"BRK ",*jr,*zf,*rv,obrknum
.                                        match   b4,obrknum
.                                        if equal
.                                                move    "0000",obrknum
.                                        endif
.                                        cmatch  b1,obrknum
.                                        if eos
.                                                move    "0000",obrknum
.                                        endif
.                                        type    obrknum
.                                        if not equal
.                                                move    "0000",obrknum
.                                        endif
.                                endif
                                move    C2,NMOBPATH
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                pack    NMOAFLD4,NCSHBRK,CMLR
				pack	COMPFLD,CMLR
				move	"Delete-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPKEY
				move	COMPOLDMLR,str4
.
				pack	COMPFLD,NCSHBRK
				move	"DeleteB-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPKEY
.
				call	Trim using COMPOLDBRK
				if (COMPOLDBRK = "")
					move	"0000",COMPOLDBRK
				endif
                                pack    NMOAFLD4,COMPOLDBRK,str4
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                rep     ZFILL,NMOAFLD4
                                move    "Delete-NMOBTST",Location
                                pack    KeyLocation,"Key: ",NMOAFLD4
                                call    NMOBTST
                                if over
                                        alert   caution,"Invalid MOB File Key!!",result
                                endif
.
                                move    CAMOUNT,AMOUNTB
                                move    "99",REASON
                                pack    TRANDATE,CCE,CYR,CMO,CDY
                                pack    RECDATE,timestamp
                                move    "NONANXT",GNXTFLD
                                move    "Delete-GNXTKEY",Location
                                pack    KeyLocation,"Key: ",GNXTFLD
                                call    GNXTKEY
                                move    GNXTNUM,N7
                                loop
                                        add     C1,N7
                                        move    N7,TRANSNUM
                                        rep     ZFILL,TRANSNUM
                                        move    TRANSNUM,NMOAFLD
                                        move    C2,NMOAPATH
                                        move    "Delete-NMOATST",Location
                                        pack    KeyLocation,"Key: ",NMOAFLD
                                        call    NMOATST
                                        until over
                                repeat
                                move    N7,str7
                                bump    str7,1
                                move    str7,GNXTNUM
                                rep     ZFILL,GNXTNUM
                                move    "Delete-GNXTUPD",Location
                                pack    KeyLocation,"Key: ",GNXTFLD
                                call    GNXTUPD
.NOTE GNXT ONLY ALLOWS 6 BYTE NUMBER. DLH.
                                move    Z3,MCNT
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                pack    NMOAFLD,CMLR,MCNT
                                pack    NMOAFLD,str4,MCNT
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                move    C4,NMOAPATH
                                move    CLR,LRNUM
                                if (NCSHBRK = "" | NCSHBRK = "    ")
.START PATCH 1.95 REPLACED LOGIC
.                                        move    "0000",NCSHBRK
                                        move    "000000",NCSHBRK
.END PATCH 1.95 REPLACED LOGIC
                                endif
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                pack    NMOAFLD4,NCSHBRK,CMLR
.                                rep     ZFILL,NMOAFLD4
.                                move    CMLR,MLR
.                                move    CMLR,NMOBMLR
                                pack    NMOAFLD4,COMPOLDBRK,str4
                                rep     ZFILL,NMOAFLD4
                                move    str4,MLR
                                move    str4,NMOBMLR
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
                                move    Z3,MCNT
                                move    MCNT,NMOBMCNT
                                move    CNUM,CONTROL
                                move    CLR,NINVFLD
                                move    C1,NINVPATH
                                rep     ZFILL,NINVFLD
                                clear   INVNUM
                                clear   INVDATE
                                move    "Delete-NINVKEY",Location
                                pack    KeyLocation,"Key: ",NINVFLD
                                call    NINVKEY
                                move    INVNUM,INVOICE
                                call    TRIM using INVDTEY
                                if (INVDTEY <> "")
                                        pack    INVDATE,INVDTEC,INVDTEY,INVDTEM,INVDTED
                                else
                                        clear   INVDATE
                                endif
                                move    AMOUNTb,ONAMOUNT
                                move    AMOUNTb,CHANGE
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
.                                move    NCSHBRK,NMOABRK
.				rep	zfill,NMOABRK
.                                move    NCSHBRK,NMOBBRK
                                move    COMPOLDBRK,NMOABRK
				rep	zfill,NMOABRK
                                move    COMPOLDBRK,NMOBBRK
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY PATCH
				rep	zfill,NMOBBRK
                                move    "0",INAMOUNT
                                move    OLNUM,LIST
.START PATCH 1.95 REPLACED LOGIC
.                                move    NCSHCHK,CHECKNUM
                                unpack    NCSHCHK,str6,CHECKNUM
.END PATCH 1.95 REPLACED LOGIC
                                move    C2,NMOBPATH
                                move    INITS,NMOAINIT
                                move    "Delete-NMOAWRT",Location
                                call    NMOAWRT
                                move    "Delete-NMOBUPD",Location
                                pack    KeyLocation,"Key: ",NMOAFLD4
                                call    NMOBUPD
                        endif
...................................................................
.Refresh UnApplied Amount
                        call    CashFindCheckUnApplied
                        if (ViewFlag = 1)
                                call	Click_CashListView
                        elseif (ViewFlag = 2)
                                call	Click_CashListViewA
                        elseif (ViewFlag = 3)
                                call	Click_CashListViewB
                        elseif (ViewFlag = 4)
                                call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                        elseif (ViewFlag = 5)
                                call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                        endif
                endif
        endif
.Refresh Variables
        setitem CashStatModify,0,""
        call    Click_CashOK
        move    NO,NewFlag
        move    YES,ExitFlag
        return

CashLoadControl
.Load Control Information
        call    CashLoadScreen1
        call    CashLoadChecks
        call    CashLoadDetails
        return

.START PATCH 1.4 ADDED LOGIC
CashLoadControlListView
	pack	hold2,NCTRVARS
	CashListView3.InsertItem giving N9 using NCTRDATE
	CashListView3.SetItemText using N9,NCTRNUM,1
	call	Trim using NCTRDATE
	if (NCTRDATE <> "")
		unpack	NCTRDATE,CC,YY,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	CashListView3.SetItemText using N9,str10,2
	unpack	NCTRAMT,str12,str3
	call	Trim using str12
	call	FormatNumeric using str12,str15
	pack	str25,str15,str3
	CashListView3.SetItemText using N9,str25,3
	if (NCTRCODE = 1)
		move	"Edited",str25
	elseif (NCTRCODE = 2)
		move	"Checks Run!!",str25
	else
		clear	str25
	endif
	CashListView3.SetItemText using N9,str25,4
	CashListView3.SetItemText using N9,hold2,5
	return
.END PATCH 1.4 ADDED LOGIC

CashLoadChecks
.Load Checks associated with Control
        move    C0,ChkTotal
        CashListView.DeleteAllItems giving N9
        CashListViewA.DeleteAllItems giving N9
        CashListViewB.DeleteAllItems giving N9
        CashListViewC.DeleteAllItems giving N9
.START PATCH 1.95 ADDED LOGIC
        CashListViewD.DeleteAllItems giving N9
.END PATCH 1.95 ADDED LOGIC
        clear   NCHKFLD1
        clear   NCHKFLD2
.START PATCH 1.4 REPLACED LOGIC
.        pack    NCHKFLD1,"01X",NCTRFLD
        pack    NCHKFLD1,"01X",NCTRNUM,NCTRDATE
.END PATCH 1.4 REPLACED LOGIC
        move    "CashLoadCont.-NCHKAIM",Location
        pack    KeyLocation,"Key: ",NCHKFLD1
        call    NCHKAIM
        loop
                until over
                call    CashLoadListView
        	move	"C.LoadControl-NCHKKG",Location
	        pack	KeyLocation,"Key: ",NCHKFLD1
        	call	NCHKKG
        repeat
        CashListView.GetItemCount giving result
        if (result > C0)
                if (ViewFlag = 1)
                        CashListView.EnsureVisible using 0,0
                        CashListView.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView
                elseif (ViewFlag = 2)
                        CashListViewA.EnsureVisible using 0,0
                        CashListViewA.SetItemState giving N9 using 0,2,2
                        call	Click_CashListViewA
                elseif (ViewFlag = 3)
                        CashListViewB.EnsureVisible using 0,0
                        CashListViewB.SetItemState giving N9 using 0,2,2
                        call	Click_CashListViewB
                elseif (ViewFlag = 4)
                        CashListViewC.EnsureVisible using 0,0
                        CashListViewC.SetItemState giving N9 using 0,2,2
                        call	Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        CashListViewD.EnsureVisible using 0,0
                        CashListViewD.SetItemState giving N9 using 0,2,2
                        call	Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
                endif
                setprop CashListView,enabled=1
                setprop CashListViewA,enabled=1
                setprop CashListViewB,enabled=1
                setprop CashListViewC,enabled=1
.START PATCH 1.95 ADDED LOGIC
                setprop CashListViewD,enabled=1
.END PATCH 1.95 ADDED LOGIC
                move    ChkTotal,str18
                unpack  str18,str14,str3
                call    Trim using str14
                call    FormatNumeric using str14,str18
                pack    str45,"Check(s) Total: ",str18,str3
                setitem CashStatCheckTotal,0,str45
        else
                call    CashClearScreens1
        endif
        return
CashLoadDetails
.Load Detail Records associated with Control
        move    C0,DetTotal
        CashListView2.DeleteAllItems giving N9
        CashListView2A.DeleteAllItems giving N9
        CashListView2B.DeleteAllItems giving N9
        CashListView2C.DeleteAllItems giving N9
.START PATCH 1.95 REPLACED LOGIC
        CashListView2D.DeleteAllItems giving N9
.END PATCH 1.95 REPLACED LOGIC
	move	C2,NCSHPATH
	clear	NCSHFLD
        clear   NCSHFLD1
	clear	NCSHFLD2
	clear	NCSHFLD4
.START PATCH 1.4 ADDED LOGIC
.	pack	NCSHFLD2,"03X",NCTRFLD
.	move	"OK-NCSHAIM",Location
.	pack	KeyLocation,"Key: ",NCSHFLD2
	pack	NCSHFLD2,"03X",NCTRNUM
	pack	NCSHFLD5,"05X",NCTRDATE
	move	"OK-NCSHAIM",Location
	pack	KeyLocation,"Key: ",NCSHFLD2,COMMA,NCSHFLD5
.END PATCH 1.4 ADDED LOGIC
	call	NCSHAIM
	loop
                until over
                call    CashLoadListView2
        	move	"C.LoadControl-NCSHKG",Location
	        pack	KeyLocation,"Key: ",NCSHFLD2
        	call	NCSHKG
        repeat
        CashListView2.GetItemCount giving result
        if (result > C0)
                if (View1Flag = 1)
                        CashListView2.EnsureVisible using 0,0
                        CashListView2.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView2
                elseif (View1Flag = 2)
                        CashListView2A.EnsureVisible using 0,0
                        CashListView2A.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView2A
                elseif (View1Flag = 3)
                        CashListView2B.EnsureVisible using 0,0
                        CashListView2B.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView2B
                elseif (View1Flag = 4)
                        CashListView2C.EnsureVisible using 0,0
                        CashListView2C.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView2C
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        CashListView2D.EnsureVisible using 0,0
                        CashListView2D.SetItemState giving N9 using 0,2,2
                        call	Click_CashListView2D
.END PATCH 1.95 REPLACED LOGIC
                endif
                setprop CashListView2,enabled=1
                setprop CashListView2A,enabled=1
                setprop CashListView2B,enabled=1
                setprop CashListView2C,enabled=1
.START PATCH 1.95 REPLACED LOGIC
                setprop CashListView2D,enabled=1
.END PATCH 1.95 REPLACED LOGIC
.                CashListView2.EnsureVisible using 0,0
.                CashListView2.SetItemState giving N9 using 0,2,2
.                setprop CashListView2,enabled=1
.                call    Click_CashListView2
                move    DetTotal,str18
                unpack  str18,str14,str3
.                call    Trim using str14
                call    FormatNumeric using str14,str18
                pack    str25,str18,str3
                setitem CashStatDetailTotal,0,str25
        else
                call    CashClearScreens2
        endif
        return
...............................
.        unpack  NCSHFLD2,str3,str3
..        setitem CashEditControl,0,str3
.        CashListView2.DeleteAllItems giving N9
.        loop
.                call    CashLoadListView
.        	move	"C.LoadControl-NCSHKG",Location
.	        pack	KeyLocation,"Key: ",NCSHFLD2
.        	call	NCSHKG
.                until over
.        repeat
.        CashListView2.GetItemCount giving result
.        if (result > C0)
.                CashListView2.EnsureVisible using 0,0
.                CashListView2.SetItemState giving N9 using 0,2,2
.                setprop CashListView2,enabled=1
.                goto    Click_CashListView2
.        endif
.        return

CashLoadListView
        pack    hold1,NCHKVARS
        CashListView.InsertItem giving N9 using NCHKCONT
        CashListView.SetItemText using N9,NCHKNUM,1
        call    Trim using NCHKDATE
        if (NCHKDATE <> "")
                unpack  NCHKDATE,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        else
                clear   str10
        endif
        CashListView.SetItemText using N9,str10,2
        add     NCHKAMT,ChkTotal
        move    NCHKAMT,str15
        unpack  str15,str12,str3
        call    Trim using str15
        call    FormatNumeric using str12,str15
        pack    str24,str15,str3
        CashListView.SetItemText using N9,str24,3
.        move    C0,Temp122
.        move    NCHKAMT,Temp122
.        call    CashCalcUnApplied using NCHKCONT,NCHKNUM,Temp122
..........................
        move    C0,Temp122
        move    C0,Temp122B
        move    C0,Temp122C
        move    NCHKAMT,Temp122
        clear   str2
.START PATCH 1.4 REPLACED LOGIC
.        call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,C0,str2,Temp122C
        call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,C0,str2,Temp122C,NCHKCONTD
.END PATCH 1.4 REPLACED LOGIC
..........................
        clear   str25
        if (Temp122 <> C0)
                move    Temp122,str15
                unpack  str15,str12,str3
                call    Trim using str12
                if (Temp122 > C0)
                        call    FormatNumeric using str12,str15
                        pack    str25,str15,str3
                elseif (Temp122 < C0)
                        call    RemoveChar using str12,DASH
                        call    FormatNumeric using str12,str15
                        pack    str18,str15,str3
                        pack    str25,"(",str18,")"
                endif
        endif
        CashListView.SetItemText using N9,str25,4
        call    Trim using NCHKPAYOR
        CashListView.SetItemText using N9,NCHKPAYOR,5
        CashListView.SetItemText using N9,hold1,6
.
        CashListViewA.InsertItem giving N9 using NCHKNUM
        CashListViewA.SetItemText using N9,NCHKCONT,1
        CashListViewA.SetItemText using N9,str10,2
        CashListViewA.SetItemText using N9,str24,3
        CashListViewA.SetItemText using N9,str25,4
        CashListViewA.SetItemText using N9,NCHKPAYOR,5
        CashListViewA.SetItemText using N9,hold1,6
.
        move    NCHKAMT,str15
        rep     zfill,str15
        CashListViewB.InsertItem giving N9 using str15
        CashListViewB.SetItemText using N9,str24,1
        CashListViewB.SetItemText using N9,NCHKCONT,2
        CashListViewB.SetItemText using N9,NCHKNUM,3
        CashListViewB.SetItemText using N9,str10,4
        CashListViewB.SetItemText using N9,str25,5
        CashListViewB.SetItemText using N9,NCHKPAYOR,6
        CashListViewB.SetItemText using N9,hold1,7
.
        CashListViewC.InsertItem giving N9 using NCHKPAYOR
        CashListViewC.SetItemText using N9,NCHKCONT,1
        CashListViewC.SetItemText using N9,NCHKNUM,2
        CashListViewC.SetItemText using N9,str10,3
        CashListViewC.SetItemText using N9,str24,4
        CashListViewC.SetItemText using N9,str25,5
        CashListViewC.SetItemText using N9,hold1,6
.START PATCH 1.95 ADDED LOGIC
	unpack  NCHKDATE,CC,YY,MM,DD
	call	CvtJul
	move	JULDAYS,str5
        CashListViewD.InsertItem giving N9 using str5
        CashListViewD.SetItemText using N9,str10,1
        CashListViewD.SetItemText using N9,NCHKNUM,2
        CashListViewD.SetItemText using N9,str24,3
        CashListViewD.SetItemText using N9,NCHKCONT,4
        CashListViewD.SetItemText using N9,str25,5
        CashListViewD.SetItemText using N9,NCHKPAYOR,6
        CashListViewD.SetItemText using N9,hold1,7
.END PATCH 1.95 ADDED LOGIC
        return

CashLoadListView2
        pack    hold,CASHVARS
        CashListView2.InsertItem giving N9 using CLR
        move    C1,NINVPATH
        move    CLR,NINVFLD
        move    "C.LoadListV.-NINVKEY",Location
        pack    KeyLocation,"Key: ",NINVFLD
        call    NINVKEY
        if over
                clear   INVNUM
        endif
        CashListView2.SetItemText using N9,INVNUM,1
.        call    Trim using NPAYOR
.        CashListView2.SetItemText using N9,NPAYOR,2
        CashListView2.SetItemText using N9,CMLR,3
        CashListView2.SetItemText using N9,CNUM,4
        CashListView2.SetItemText using N9,NCSHCHK,5
        clear   str18
        add     CAMOUNT,DetTotal
        if (CAMOUNT > 0)
                move    CAMOUNT,str13
                unpack  str13,str10,str3
                call    Trim using str10
                call    FormatNumeric using str10,str13
                pack    str18,str13,str3
        endif
        CashListView2.SetItemText using N9,str18,6
.        call    Trim using NCKDTEM
.        if (NCKDTEM = "")
.                clear   str10
.        else
.                pack    str10,NCKDTEM,SLASH,NCKDTED,SLASH,NCKDTEC,NCKDTEY
.        endif
.START PATCH 1.95 REPLACED LOGIC
.        if (NCSHCHK = "000MOA")
.START PATCH 1.95 ADDED LOGIC
	clear	str5	.Initialize
.END PATCH 1.95 ADDED LOGIC
        if (NCSHCHK = "000000000MOA")
.END PATCH 1.95 REPLACED LOGIC
                clear   str10
                clear   NCHKPAYOR
        else
.START PATCH 1.4 REPLACED LOGIC
.	        pack    NCHKFLD,CNUM,NCSHCHK
	        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
.END PATCH 1.4 REPLACED LOGIC
                move    "LoadLV2-NCHKKEY",Location
                pack    KeyLocation,"Key: ",NCHKFLD
                call    NCHKKEY
                if over
                        clear   str10
                        clear   NCHKPAYOR
                else
                        unpack  NCHKDATE,CC,YY,MM,DD
                        pack    str10,MM,SLASH,DD,SLASH,CC,YY
.START PATCH 1.95 REPLACED LOGIC
			call	CvtJul
			move	JULDAYS,str5
.END PATCH 1.95 REPLACED LOGIC
                endif
        endif
        CashListView2.SetItemText using N9,NCHKPAYOR,2
        CashListView2.SetItemText using N9,str10,7
        CashListView2.SetItemText using N9,CEXTCD,8
        CashListView2.SetItemText using N9,hold,9
.
        CashListView2A.InsertItem giving N9 using NCSHCHK
        CashListView2A.SetItemText using N9,CNUM,1
        CashListView2A.SetItemText using N9,CMLR,2
        CashListView2A.SetItemText using N9,CLR,3
        CashListView2A.SetItemText using N9,INVNUM,4
        CashListView2A.SetItemText using N9,NCHKPAYOR,5
        CashListView2A.SetItemText using N9,str18,6
        CashListView2A.SetItemText using N9,str10,7
        CashListView2A.SetItemText using N9,CEXTCD,8
        CashListView2A.SetItemText using N9,hold,9
.
        CashListView2B.InsertItem giving N9 using CMLR
        CashListView2B.SetItemText using N9,CLR,1
        CashListView2B.SetItemText using N9,INVNUM,2
        CashListView2B.SetItemText using N9,NCHKPAYOR,3
        CashListView2B.SetItemText using N9,CNUM,4
        CashListView2B.SetItemText using N9,NCSHCHK,5
        CashListView2B.SetItemText using N9,str18,6
        CashListView2B.SetItemText using N9,str10,7
        CashListView2B.SetItemText using N9,CEXTCD,8
        CashListView2B.SetItemText using N9,hold,9
.
        CashListView2C.InsertItem giving N9 using CNUM
        CashListView2C.SetItemText using N9,CMLR,1
        CashListView2C.SetItemText using N9,CLR,2
        CashListView2C.SetItemText using N9,INVNUM,3
        CashListView2C.SetItemText using N9,NCHKPAYOR,4
        CashListView2C.SetItemText using N9,NCSHCHK,5
        CashListView2C.SetItemText using N9,str18,6
        CashListView2C.SetItemText using N9,str10,7
        CashListView2C.SetItemText using N9,CEXTCD,8
        CashListView2C.SetItemText using N9,hold,9
.START PATCH 1.95 REPLACED LOGIC
.
        CashListView2D.InsertItem giving N9 using str5
        CashListView2D.SetItemText using N9,str10,1
        CashListView2D.SetItemText using N9,NCSHCHK,2
        CashListView2D.SetItemText using N9,str18,3
        CashListView2D.SetItemText using N9,CNUM,4
        CashListView2D.SetItemText using N9,CMLR,5
        CashListView2D.SetItemText using N9,NCHKPAYOR,6
        CashListView2D.SetItemText using N9,CLR,7
        CashListView2D.SetItemText using N9,INVNUM,8
        CashListView2D.SetItemText using N9,CEXTCD,9
        CashListView2D.SetItemText using N9,hold,10
.END PATCH 1.95 REPLACED LOGIC
        return

.CashLoadScreens
CashLoadScreen1
        setitem CashStatMasterCont,0,NCTRNUM
        call    Trim using NCTRDATE
        if (NCTRDATE <> "")
                unpack  NCTRDATE,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        else
                clear   str10
        endif
        setitem CashEditControlDate,0,str10
        clear   str18
        if (NCTRAMT > C0)
                move    NCTRAMT,str15
                unpack  str15,str12,str3
                call    Trim using str12
                call    FormatNumeric using str12,str15
                pack    str18,str15,str3
        endif
        setitem CashEditControlAmount,0,str18
        if (NCTRCODE = 1)
                setitem CashStatStatus,0,"Edited"
        elseif (NCTRCODE = 2)
                setitem CashStatStatus,0,"Checks Run!!"
        else
                setitem CashStatStatus,0,""
        endif
.START PATCH 1.9 ADDED LOGIC
	move	NCTRCODE,str1
	setitem CashEditControlCode,0,str1
.END PATCH 1.9 ADDED LOGIC
        return
CashLoadScreen2
        setitem CashEditChkControl,0,NCHKCONT
.START PATCH 1.4 ADDED LOGIC
.str10 is loaded in ListView Click Event so I cannot use it here!
	unpack	NCHKCONTD,CC,YY,MM,DD
	call	Trim using DD
	if (DD <> "")
		pack	str11,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str11
	endif
        setitem CashEditChkContDate,0,str11
.END PATCH 1.4 ADDED LOGIC
        setitem CashEditChkCheck,0,NCHKNUM
        setitem CashEditChkAmount,0,str18
        call    Trim using str25
        if (str25 <> "")
                scan    "(",str25
                if not equal
                        setprop CashStatUnApplied,fgcolor=black
                else
                        setprop CashStatUnApplied,fgcolor=red
                endif
                reset   str25
        endif
        setitem CashStatUnApplied,0,str25
        setitem CashEditChkDate,0,str10
        call    Trim using NCHKPAYOR
        setitem CashEditChkPayor,0,NCHKPAYOR
        return

CashLoadScreen3 LRoutine FrmPtr2
        setitem CashEditControl,0,CNUM
.START PATCH 1.4 ADDED LOGIC
.str10 is loaded in ListView Click Event so I cannot use it here!
	unpack	CNUMDATE,CC,YY,MM,DD
	call	Trim using DD
	if (DD <> "")
		pack	str11,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str11
	endif
	setitem CashEditCshControlDate,0,str11
.END PATCH 1.4 ADDED LOGIC
        setitem CashEditID,0,CID
        setitem CashEditMlr,0,CMLR
        call    CashLoadMailer using CMLR,CashStatMlrName
        setitem CashEditBroker,0,NCSHBRK
        call    CashLoadBroker using NCSHBRK,CashStatBrokerName
        call    Trim using CMO
        if (CMO <> "")
                pack    str15,CMO,SLASH,CDY,SLASH,CCE,CYR
        else
                clear   str15
        endif
        setitem CashEditRecordDate,0,str15
        setitem CashEditLR,0,CLR
        setitem CashEditInvoice,0,INVNUM
        move    C1,N9
        move    C0,result
        loop
                getitem CashComboCode,N9,str1
                if (str1 = CEXTCD)
                        move    N9,result
                        break
                endif
                until (N9 > 10)
                add     C1,N9
        repeat
        setitem CashComboCode,0,result
        setitem CashEditCheckNum,0,NCSHCHK
        setitem CashEditAmount,0,str18
        move    C0,Temp122
.START PATCH 1.4 REPLACED LOGIC
.        pack    NCHKFLD,CNUM,NCSHCHK
        pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
.END PATCH 1.4 REPLACED LOGIC
        call    Trim using NCHKFLD
.ADD CODE HERE TO PROTECT AGAINST A SITUATION WHERE THERE ARE NO SELECTED RECORDS IN LISTVIEW
        if (NCHKFLD <> "")
                move    "LoadScreen2-NCHKKEY",Location
                pack    KeyLocation,"Key: ",NCHKFLD
                call    NCHKKEY
                if not over
                        move    NCHKAMT,Temp122
                endif
                move    Temp122,str15
                unpack  str15,str12,str3
                call    Trim using str12
                call    FormatNumeric using str12,str15
                pack    str25,str15,str3
                call    Trim using str25
        else
                clear   NCHKAMT
                clear   str25
                clear   str10
                clear   str55
        endif
        setitem CashEditCheckAmt,0,str25
        setitem CashEditCheckDate,0,str10
        setitem CashEditPayor,0,str55
.CashCalcUnApplied must be called last as it refreshes CASHVARS
        call    CashPackTemp

.        move    C0,Temp122
.        move    NCHKAMT,Temp122
.        call    CashCalcUnApplied using CNUM,NCSHCHK,Temp122
...............................
        move    C0,Temp122
        move    C0,Temp122B
        move    NCHKAMT,Temp122
        move    CID,str2
        move    CAMOUNT,Temp122C
.START PATCH 1.4 REPLACED LOGIC
.        call    CashCalcTotal using CNUM,NCSHCHK,Temp122,Temp122B,FrmPtr2,str2,Temp122C
        call    CashCalcTotal using CNUM,NCSHCHK,Temp122,Temp122B,FrmPtr2,str2,Temp122C,CNUMDATE
.END PATCH 1.4 REPLACED LOGIC
...............................
        clear   str25
        if (Temp122 <> C0)
                move    Temp122,str15
                unpack  str15,str12,str3
                call    Trim using str12
                if (Temp122 > C0)
                        setprop CashStatUnApplied2,fgcolor=black
                        call    FormatNumeric using str12,str15
                        pack    str25,str15,str3
                elseif (Temp122 < C0)
                        setprop CashStatUnApplied2,fgcolor=red
                        call    RemoveChar using str12,DASH
                        call    FormatNumeric using str12,str15
                        pack    str18,str15,str3
                        pack    str25,"(",str18,")"
                endif
        endif
        setitem CashStatUnApplied2,0,str25
.Temp122b calculated in CashCalcUnApplied
        move    Temp122B,str15
        unpack  str15,str12,str3
        call    Trim using str12
        call    FormatNumeric using str12,str15
        pack    str18,str15,str3
        setitem CashStatDetailTotal2,0,str18
        call    CashPackTemp2
        return

CashLoadMailer LRoutine DimPtr,StatPtr
.START PATCH 1.95 REPLACED LOGIC
.        pack    MKEY,DimPtr,"000"
.        move    C1,NMLRPATH
.        move    "C.LoadMlr-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        if over
.                clear   MCOMP
.        endif
.        setitem StatPtr,0,MCOMP
.        return
...........................
	pack	COMPFLD,DimPtr
        move    "C.LoadMlr-COMPKEY",Location
        pack    KeyLocation,"Key: ",COMPFLD
        call    COMPKEY
        if over
                clear   COMPCOMP
	elseif (COMPMLRFLG <> "T")
                clear   COMPCOMP
        endif
        setitem StatPtr,0,COMPCOMP
        return
.END PATCH 1.95 REPLACED LOGIC
CashLoadBroker LRoutine DimPtr,StatPtr
.START PATCH 1.95 REPLACED LOGIC
.        pack    NBRKFLD,DimPtr,"000"
.        move    C1,NBRKPATH
.        move    "C.LoadBrk-NBRKKEY",Location
.        pack    KeyLocation,"Key: ",NBRKFLD
.        call    NBRKKEY
.        if over
.                clear   BRCOMP
.        endif
.        setitem StatPtr,0,BRCOMP
.        return
................................
	pack    COMPFLD,DimPtr
	move    "C.LoadBrk-COMPKEY",Location
	pack    KeyLocation,"Key: ",COMPFLD
	call    COMPKEY
	if over
		clear   COMPCOMP
	elseif (COMPBRKFLG <> "T" & COMPCLRFLG <> "T")
		clear   COMPCOMP
	endif
	setitem StatPtr,0,COMPCOMP
	return
.END PATCH 1.95 REPLACED LOGIC

CashVerifyControl
        getitem CashSearchControl,0,str3
        call    ZFillIt using str3,C0
        setitem CashSearchControl,0,str3
.START PATCH 1.4 REPLACED LOGIC
.        if (NewFlag = YES | str3 <> HoldCont)   .Second condition is not a real possibility as I set readonly property
.                pack    NCTRFLD,str3
        getitem CashEditControlDate,0,str10
	call	Trim using str10
	call	RemoveChar using str10,SLASH
.START PATCH 1.5 ADDED LOGIC
	unpack	str10,MM,DD,CC,YY
	pack	str10,CC,YY,MM,DD
.END PATCH 1.5 ADDED LOGIC
	pack	str11,str3,str10
        if (NewFlag = YES | str11 <> HoldCont)   .Second condition is not a real possibility as I set readonly property
                pack    NCTRFLD,str11
.END PATCH 1.4 REPLACED LOGIC
                move    "VerifyCont.-NCTRTST",Location
                pack    KeyLocation,"Key: ",NCTRFLD
                call    NCTRTST
                if not over
                        alert   caution,"This Control Already Exists!",result
                        move    YES,ReturnFlag
                        setfocus CashSearchControl
                        return
                endif
        endif
        move    str3,NCTRNUM
.
        getitem CashEditControlDate,0,str10
        call    RemoveChar using str10,SLASH
        call    Trim using str10
        count   N9,str10
        if (N9 <> 8)
                alert   caution,"Date must be in MMDDCCYY Format!",result
                move    YES,ReturnFlag
                setfocus CashEditControlDate
                return
        endif
        unpack  str10,MM,DD,CC,YY
        pack    NCTRDATE,CC,YY,MM,DD
.
        getitem CashEditControlAmount,0,str25
        call    RemoveChar using str25,COMMA
        call    Trim using str25
        move    C0,NCTRAMT
        move    str25,NCTRAMT
.START PATCH 1.9 ADDED LOGIC
	if (SecFlag = C1)
		getitem	CashEditControlCode,0,str1
		move	str1,NCTRCODE
	endif
.END PATCH 1.9 ADDED LOGIC
        return

CashVerifyCheck
	getitem	CashEditChkControl,0,str3
        call    Trim using str3
        if (str3 = "")
                alert   caution,"Valid Control Required!",result
                move    YES,ReturnFlag
                setfocus CashEditChkControl
                return
        endif
	call	ZFillIt using str3,C0
	setitem	CashEditChkControl,0,str3
.START PATCH 1.4 ADDED LOGIC
	getitem	CashEditChkContDate,0,str10
        call    Trim using str10
	call	RemoveChar using str10,SLASH
	count	N2,str10
        if (str10 = "" | N2 <> 8)
                alert   caution,"Valid Control Date Required!",result
                move    YES,ReturnFlag
                setfocus CashEditChkContDate
                return
        endif
.END PATCH 1.4 ADDED LOGIC
        if (NewFlag = YES)
.Verify this Control exists
.START PATCH 1.4 REPLACED LOGIC
.                pack    NCTRFLD,str3
		unpack	str10,MM,DD,CC,YY
                pack    NCTRFLD,str3,CC,YY,MM,DD
.START PATCH 1.4 REPLACED LOGIC
                move    "VerifyCheck-NCTRKEY",Location
                pack    KeyLocation,"Key: ",NCTRFLD
                call    NCTRKEY
                if over
                        alert   caution,"This Control does not exist!",result
                        move    YES,ReturnFlag
                        setfocus CashEditChkControl
                        return
                elseif (NCTRCODE = 2)
                        alert   caution,"This Control is Closed!!",result
                        move    YES,ReturnFlag
                        setfocus CashEditChkControl
                        return
                elseif (NCTRCODE = 1)
			pack	taskname,"This Control has already had an Edit run!",NewLine,"Please supply password to add a new Check."
			alert	caution,taskname,result
			move	"x",progcode
			setitem	PasswordEdit,0,""
			setfocus PasswordEdit
			setprop	Passwrd,visible=1
			move	"C",progcode
.Patch1.3
.                        move    "COSMO" to NPASKEY
.			if (NPASKEY <> "COSMO" AND NPASKEY <> "cosmo")
			if (NPASKEY <> "xCOSMO" AND NPASKEY <> "xcosmo")
.Patch1.3
	                        move    YES,ReturnFlag
        	                setfocus CashEditChkControl
				return
			endif
                endif
        endif
        move    str3,NCHKCONT
.START PATCH 1.4 ADDED LOGIC
	pack	NCHKCONTD,CC,YY,MM,DD
.END PATCH 1.4 ADDED LOGIC
.
.START PATCH 1.95 REPLACED LOGIC
.        getitem CashEditChkCheck,0,str6
.        call    Trim using str6
.        if (str6 = "")
.                alert   caution,"Valid Check Required!",result
.                move    YES,ReturnFlag
.                setfocus CashEditChkCheck
.                return
.        endif
.        call	ZFillIt using str6,C0
.	setitem	CashEditChkCheck,0,str6
.        if (NewFlag = YES)
..Verify this Control/Check combo does not already exist
..START PATCH 1.4 REPLACED LOGIC
..                pack    NCHKFLD,NCHKCONT,str6
.                 pack    NCHKFLD,NCHKCONT,NCHKCONTD,str6
..END PATCH 1.4 REPLACED LOGIC
.                move    "VerifyCheck-NCHKTST",Location
.                pack    KeyLocation,"Key: ",NCHKFLD
.                call    NCHKTST
.                if not over
.                        alert   caution,"This Check already exists!",result
.                        move    YES,ReturnFlag
.                        setfocus CashEditChkCheck
.                        return
.                endif
.        endif
.        move    str6,NCHKNUM
.....................................
        getitem CashEditChkCheck,0,str12
        call    Trim using str12
        if (str12 = "")
                alert   caution,"Valid Check Required!",result
                move    YES,ReturnFlag
                setfocus CashEditChkCheck
                return
        endif
        call	ZFillIt using str12,C0
	setitem	CashEditChkCheck,0,str12
        if (NewFlag = YES)
                pack    NCHKFLD,NCHKCONT,NCHKCONTD,str12
                move    "VerifyCheck-NCHKTST",Location
                pack    KeyLocation,"Key: ",NCHKFLD
                call    NCHKTST
                if not over
                        alert   caution,"This Check already exists!",result
                        move    YES,ReturnFlag
                        setfocus CashEditChkCheck
                        return
                endif
        endif
        move    str12,NCHKNUM
.END PATCH 1.95 REPLACED LOGIC
.
        getitem CashEditChkAmount,0,str18
        if (str18 = "")
                alert   caution,"Valid Check Amount Required!",result
                move    YES,ReturnFlag
                setfocus CashEditChkAmount
                return
        endif
        call    RemoveChar using str18,COMMA
        move    str18,NCHKAMT
.
        getitem CashEditChkDate,0,str10
        if (str10 = "")
                alert   caution,"Valid Check Date Required!",result
                move    YES,ReturnFlag
                setfocus CashEditChkDate
                return
        endif
        call    RemoveChar using str10,SLASH
        unpack  str10,MM,DD,CC,YY
        pack    NCHKDATE,CC,YY,MM,DD
.
        getitem CashEditChkPayor,0,NCHKPAYOR
        call    Trim using NCHKPAYOR
        return

CashVerifyDetail
.Clear some flags
        clear   ppext
.
        getitem CashEditControl,0,str3
        call    Trim using str3
        move    str3,CNUM
	call	ZFillIt using CNUM,C0
	setitem	CashEditControl,0,CNUM
.START PATCH 1.4 ADDED LOGIC
        getitem CashEditCshControlDate,0,str10
        call    Trim using str10
	call	RemoveChar using str10,SLASH
	unpack	str10,MM,DD,CC,YY
	pack	CNUMDATE,CC,YY,MM,DD
.END PATCH 1.4 ADDED LOGIC
        if (NewFlag = YES)
                if (str3 = "")
                        alert   caution,"Valid Control Required!",result
                        move    YES,ReturnFlag
                        setfocus CashEditControl
                        return
                endif
.START PATCH 1.4 REPLACED LOGIC
..Verify this Control exists
.                pack    NCTRFLD,CNUM
		count	N2,CNUMDATE
		if (CNUMDATE = "" | N2 <> 8)
                        alert   caution,"Valid Control Date Required!",result
                        move    YES,ReturnFlag
                        setfocus CashEditCshControlDate
                        return
		endif
.Verify this Control exists
                pack    NCTRFLD,CNUM,CNUMDATE
.END PATCH 1.4 REPLACED LOGIC
                move    "VerifyDetail-NCTRKEY",Location
                pack    KeyLocation,"Key: ",NCTRFLD
                call    NCTRKEY
                if over
                        alert   caution,"Valid Control Required!",result
                        move    YES,ReturnFlag
                        setfocus CashEditControl
                        return
                elseif (NCTRCODE = 2)
                        alert   caution,"This Control is Closed!!",result
                        move    YES,ReturnFlag
                        setfocus CashEditControl
                        return
                elseif (NCTRCODE = 1)
			pack	taskname,"This Control has already had an Edit run!",NewLine,"Please supply password to add a new Detail Record."
			alert	caution,taskname,result
			move	"x",progcode
			setitem	PasswordEdit,0,""
			setfocus PasswordEdit
			setprop	Passwrd,visible=1
			move	"C",progcode
.Patch1.3
.                        move    "COSMO" to NPASKEY
.			if (NPASKEY <> "COSMO" AND NPASKEY <> "cosmo")
			if (NPASKEY <> "xCOSMO" AND NPASKEY <> "xcosmo")
.Patch1.3
	                        move    YES,ReturnFlag
        	                setfocus CashEditControl
				return
			endif
                endif
        endif
.ID
        getitem CashEditID,0,CID
        call    ZFillIt using CID,C0
.LR/Invoice
        getitem CashEditLR,0,CLR
        call    Trim using CLR
        getitem CashEditInvoice,0,str6
        call    Trim using str6
        if (CLR <> "")
                call	ZFillIt using CLR,C0
                pack    NORDFLD,CLR
                move    C1,NORDPATH
                move    "VerifyDetail-NORDTST",Location
                pack    KeyLocation,"Key: ",NORDFLD
                call    NORDKEY
                if over
                        alert   caution,"Valid LR Required!",result
                        move    YES,ReturnFlag
                        setfocus CashEditLR
                        return
                endif
		getitem CashComboCode,0,result
		getitem CashComboCode,result,str1
		call    Trim using str1
.
		move    C1,NINVPATH
		move    CLR,NINVFLD
		move    "Ver.Det.-NINVKEY",Location
	        pack    KeyLocation,"Key: ",NINVFLD
		call    NINVKEY
		if over
                        if (str1 = "")
                                clear   taskname
                                append  "LR has not been Invoiced!",taskname
                                append  NewLine,taskname
                                append  "In order to continue you need either an active Invoice or an External Code!",taskname
                                reset   taskname
                                alert	caution,taskname,result
                                move    YES,ReturnFlag
                                setfocus CashEditLR
                                return
                        endif
.START PATCH 1.7 REPLACED LOGIC
.		elseif (STATB = "P" AND (str1 <> "d" AND str1 <> "D" AND str1 <> "N"))
		elseif (STATB = "P" AND (str1 <> "d" AND str1 <> "D" AND str1 <> "N" AND str1 <> "M"))
.END PATCH 1.7 REPLACED LOGIC
			if (NewFlag = YES)
				clear   taskname
        	                append  "                              LR Invoice has already been Paid!",taskname
                	        append  NewLine,taskname
.START PATCH 1.7 REPLACED LOGIC
.                        	append  "In order to continue you need either an active Invoice or an External Code of 'd', 'D', or 'N'!",taskname
                        	append  "In order to continue you need either an active Invoice or an External Code of 'd', 'D', 'N' or 'M'!",taskname
.END PATCH 1.7 REPLACED LOGIC
	                        reset   taskname
        	                alert	caution,taskname,result
	       	               	move    YES,ReturnFlag
        	       		setfocus CashEditLR
                                return
			else
				alert	caution,"LR Invoice has already been Paid!",result
			endif
.			alert	caution,"LR Invoice has already been Paid!",result
.                        if (NewFlag = YES)
.	       	               	move    YES,ReturnFlag
.        	       		setfocus CashEditLR
.                                return
.                        endif
                else
                        if (NewFlag = YES)
                                reset   CHKN1
                                scan    "CSH",CHKN1
                                if equal
                                        reset   CHKN1
                                        clear   taskname
                                        append  "Already Run through Cash Control!",taskname
                                        append  NewLine,taskname
                                        append  "Do you want to continue?",taskname
                                        reset   taskname
                                        alert   plain,taskname,result
                                        if (result <> 1)
                                                move    YES,ReturnFlag
                	              		setfocus CashEditLR
                                                return
                                        endif
                                endif
                        endif
                        call    ZFillIt using str6
                        if (str6 <> INVNUM)
                                alert   caution,"Input Invoice ## does not match input LR ##!",result
                                move    YES,ReturnFlag
                                setfocus CashEditInvoice
                                return
                        endif
                        if (NewFlag = YES)
                                move    C0,ap1tot
                                move    C0,ap2tot
.START PATCH 1.95.3 REPLACED LOGIC
.                                move    NO,str1
				if (str1 = "P" | str1 = "Q")
					pack	taskname,"You have entered an External Code of '",str1,"' without an Adjustment!"
					pack    NJSTFLD1,"01X",CLR
					move    "VerifyInv.-NJSTAIM",Location
					pack    KeyLocation,"Key: ",NJSTFLD1
					call    NJSTAIM
					if over
						alert   caution,taskname,result
						move    YES,ReturnFlag
						setfocus CashComboCode
						return
					endif
				endif
                                move    NO,str2
.END PATCH 1.95.3 REPLACED LOGIC
                                move    C1,N2
                                loop
                                        pack    NJSTFLD,INVNUM,N2
                                        rep     zfill,NJSTFLD
                                        move    "VerifyInv.-NJSTKEY",Location
                                        pack    KeyLocation,"Key: ",NJSTFLD
                                        call    NJSTKEY
                                        if over
.START PATCH 1.95.3 REPLACED LOGIC
.                                                if (str1 = YES)
                                                if (str2 = YES)
.END PATCH 1.95.3 REPLACED LOGIC
                                                        move    ap2,Temp122
                                                        if (Temp122 <> C0)
                                                                add     ap2tot TO Temp122
                                                                if (Temp122 = C0)
                                                                        alert   note,"** PREPAID ** ",result
                                                                        move    YES,ppext
                                                                else
                                                                        clear   taskname
                                                                        append  "         ** PARTIAL PREPAID ** ",taskname
                                                                        append  NewLine,taskname
                                                                        append  "A/P must be Adjusted out and be",taskname
                                                                        append  NewLine,taskname
                                                                        append  "paid Manual, or moved to MOA/LRINC! ",taskname
                                                                        reset   taskname
                                                                        alert   caution,taskname,result
                                                                        move    YES,ReturnFlag
                                                                        setfocus CashEditInvoice
                                                                        return
                                                                endif
                                                        else
                                                                move    ap1,AP
                                                                add     ap1tot,AP
                                                                if (AP = C0)
                                                                        alert   note,"** PREPAID ** ",result
                                                                        move    YES,ppext
                                                                else
                                                                        clear   taskname
                                                                        append  "         ** PARTIAL PREPAID ** ",taskname
                                                                        append  NewLine,taskname
                                                                        append  "A/P must be Adjusted out and be",taskname
                                                                        append  NewLine,taskname
                                                                        append  "paid Manual, or moved to MOA/LRINC! ",taskname
                                                                        reset   taskname
                                                                        alert   caution,taskname,result
                                                                        move    YES,ReturnFlag
                                                                        setfocus CashEditInvoice
                                                                        return
                                                                endif
                                                        endif
                                                endif
                                                break
                                        endif
                                        if (JSTREASN = "14")
.START PATCH 1.95.3 REPLACED LOGIC
.                                                move    YES,str1
                                                move    YES,str2
.END PATCH 1.95.3 REPLACED LOGIC
                                        endif
                                        add     C1,N2
                                        add     jstap2,ap2tot
                                        add     JSTAP1,AP1TOT
                                repeat
                        endif
		endif
        elseif (str6 <> "")
                alert   caution,"Valid LR Required!",result
                move    YES,ReturnFlag
                setfocus CashEditLR
                return
        else
                move    "      ",CLR
        endif
        setitem CashEditLR,0,CLR
        setitem CashEditInvoice,0,str6
.
.START PATCH 1.95 REPLACED LOGIC
.        getitem CashEditMlr,0,str4
.        call    Trim using str4
.        if (str4 = "" AND (CLR = "" OR CLR = "      "))
.                alert   caution,"Valid Mailer Required!",result
.                move    YES,ReturnFlag
.                setfocus CashEditMlr
.                return
.        endif
.        call    ZFillIt using str4
.        setitem CashEditMlr,0,str4
.        if (CLR <> "" & CLR <> "      ")
.                if (str4 <> OMLRNUM)
.                        clear   taskname
.                        append  "Your Mailer, '",taskname
.                        append  str4,taskname
.                        append  "'",taskname
.                        append  NewLine,taskname
.                        append  "does not match LR Mailer, '",taskname
.                        append  OMLRNUM,taskname
.                        append  "'",taskname
.                        reset   taskname
.                        alert   caution,taskname,result
.                        move    YES,ReturnFlag
.			call	CashLoadMailer using OMLRNUM,CashStatMlrName
.                        setitem CashEditMlr,0,OMLRNUM
.                        setfocus CashEditMlr
.                        return
.                endif
.        endif
.        pack    MKEY,str4,"000"
.        move    "VerifyDetail-NMLRTST",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRTST
.        if over
.                alert   caution,"Valid Mailer Required!",result
.                move    YES,ReturnFlag
.                setfocus CashEditMlr
.                return
.        endif
.        move    str4,CMLR
..
.        getitem CashEditBroker,0,str4
.        call    Trim using str4
..        if (str4 = "")
..                alert   caution,"Valid Broker Required!",result
..                move    YES,ReturnFlag
..                setfocus CashEditBroker
..                return
..        endif
.        if (str4 <> "")
.                call    ZFillIt using str4
.                setitem CashEditBroker,0,str4
.                if (CLR <> "" & CLR <> "      ")
.                        if (str4 <> OBRKNUM)
.                                clear   taskname
.                                append  "Your Broker, '",taskname
.                                append  str4,taskname
.                                append  "'",taskname
.                                append  NewLine,taskname
.                                append  "does not match LR Broker, '",taskname
.                                append  OBRKNUM,taskname
.                                append  "'",taskname
.                                reset   taskname
.                                alert   caution,taskname,result
.                                move    YES,ReturnFlag
.                                setitem CashEditBroker,0,OBRKNUM
.				call	CashLoadBroker using OBRKNUM,CashStatBrokerName
.                                setfocus CashEditBroker
.                                return
.                        endif
.                endif
..patch1.92
.                pack    NBRKFLD,str4,"000"
.	        pack    COMPFLD4 from nbrkfld
.               move    "VerifyDetail-COMPTST2",Location
..               move    "VerifyDetail-NBRKTST",Location
.                pack    KeyLocation,"Key: ",COMPFLD4
..                pack    KeyLocation,"Key: ",NBRKFLD
.		CALL	COMPTST2
..patch1.92
..comment out for 1.92
..                call    NBRKTST
..end comment out
.                if over
.                        alert   caution,"Valid Broker Required!",result
.                        move    YES,ReturnFlag
.                        setfocus CashEditBroker
.                        return
.                endif
.                move    str4,NCSHBRK
.        else
.                move    "    ",NCSHBRK
.        endif
......................................................
	getitem CashEditMlr,0,str6
	call    Trim using str6
	if (str6 = "" AND (CLR = "" OR CLR = "      "))
		alert   caution,"Valid Mailer Required!",result
		move    YES,ReturnFlag
		setfocus CashEditMlr
		return
	endif
	call    ZFillIt using str6
	setitem CashEditMlr,0,str6
.START PATCH 1.95.1 ADDED LOGIC
	if (str6 <> "000000")
.END PATCH 1.95.1 ADDED LOGIC
	if (CLR <> "" & CLR <> "      ")
.Start Temporary Logic until Order file is converted
.                if (str6 <> OMLRNUM)
.                        clear   taskname
.                        append  "Your Mailer, '",taskname
.                        append  str6,taskname
.                        append  "'",taskname
.                        append  NewLine,taskname
.                        append  "does not match LR Mailer, '",taskname
.                        append  OMLRNUM,taskname
.                        append  "'",taskname
.                        reset   taskname
.                        alert   caution,taskname,result
.                        move    YES,ReturnFlag
.            		 call	CashLoadMailer using OMLRNUM,CashStatMlrName
.                        setitem CashEditMlr,0,OMLRNUM
.                        setfocus CashEditMlr
.                        return
		pack    COMPFLD3,OMLRNUM
		move    "VerifyDetail(Temp)-COMPKEY3",Location
		pack    KeyLocation,"Key: ",COMPFLD3
		call    COMPKEY3
		if (str6 <> COMPNUM)
			clear   taskname
			append  "Your Mailer, '",taskname
			append  str6,taskname
			append  "'",taskname
			append  NewLine,taskname
			append  "does not match LR Mailer, '",taskname
			append  COMPNUM,taskname
			append  "'",taskname
			reset   taskname
			alert   caution,taskname,result
			move    YES,ReturnFlag
			call	CashLoadMailer using COMPNUM,CashStatMlrName
			setitem CashEditMlr,0,COMPNUM
			setfocus CashEditMlr
			return
.End Temporary Logic until Order file is converted
		endif
	endif
	pack    COMPFLD,str6
	move    "VerifyDetail-COMPKEY",Location
	pack    KeyLocation,"Key: ",COMPFLD
	call    COMPKEY
	if over
		alert   caution,"Valid Mailer Required!",result
		move    YES,ReturnFlag
		setfocus CashEditMlr
		return
	elseif (COMPMLRFLG <> "T")
		alert   caution,"Valid Mailer Required!",result
		move    YES,ReturnFlag
		setfocus CashEditMlr
		return
	endif
.START PATCH 1.95.1 ADDED LOGIC
	endif
.END PATCH 1.95.1 ADDED LOGIC
	move    str6,CMLR
.
	getitem CashEditBroker,0,str6
	call    Trim using str6
	if (str6 <> "")
		call    ZFillIt using str6
		setitem CashEditBroker,0,str6
		if (CLR <> "" & CLR <> "      ")
.Start Temporary Logic until Order file is converted
.                        if (str6 <> OBRKNUM)
.                                clear   taskname
.                                append  "Your Broker, '",taskname
.                                append  str6,taskname
.                                append  "'",taskname
.                                append  NewLine,taskname
.                                append  "does not match LR Broker, '",taskname
.                                append  OBRKNUM,taskname
.                                append  "'",taskname
.                                reset   taskname
.                                alert   caution,taskname,result
.                                move    YES,ReturnFlag
.                                setitem CashEditBroker,0,OBRKNUM
.				call	CashLoadBroker using OBRKNUM,CashStatBrokerName
.                                setfocus CashEditBroker
.                                return
			pack    COMPFLD4,OBRKNUM
			move    "VerifyDetailB(Temp)-COMPKEY2",Location
			pack    KeyLocation,"Key: ",COMPFLD4
			CALL	COMPKEY2
			if (str6 <> COMPNUM)
				clear   taskname
				append  "Your Broker, '",taskname
				append  str6,taskname
				append  "'",taskname
				append  NewLine,taskname
				append  "does not match LR Broker, '",taskname
				append  COMPNUM,taskname
				append  "'",taskname
				reset   taskname
				alert   caution,taskname,result
				move    YES,ReturnFlag
				setitem CashEditBroker,0,COMPNUM
				call	CashLoadBroker using COMPNUM,CashStatBrokerName
				setfocus CashEditBroker
				return
.End Temporary Logic until Order file is converted
			endif
		endif
	        pack    COMPFLD,str6
	        move    "VerifyDetailB-COMPKEY",Location
	        pack    KeyLocation,"Key: ",COMPFLD
	        call    COMPKEY
	        if over
	                alert   caution,"Valid Broker Required!",result
	                move    YES,ReturnFlag
	                setfocus CashEditBroker
	                return
		elseif (COMPBRKFLG <> "T" & COMPCLRFLG <> "T")
	                alert   caution,"Valid Broker Required!",result
	                move    YES,ReturnFlag
	                setfocus CashEditBroker
	                return
	        endif
                move    str6,NCSHBRK
        else
                move    "      ",NCSHBRK
        endif
.END PATCH 1.95 REPLACED LOGIC
.
        getitem CashEditRecordDate,0,str15
        call    RemoveChar using str15,SLASH
        call    Trim using str15
        count   N9,str15
        if (N9 <> 8)
                alert   caution,"Valid Record Date Required!",result
                move    YES,ReturnFlag
                setfocus CashEditRecordDate
                return
        endif
        unpack  str15,CMO,CDY,CCE,CYR
.
        getitem CashEditAmount,0,str18
        call    RemoveChar using str18,COMMA
        call    Trim using str18
        move    C0,CAMOUNT
        move    str18,CAMOUNT
        if (CAMOUNT < C0)
                alert   caution,"Amount must be greater than '0'!",result
                move    YES,ReturnFlag
                setfocus CashEditAmount
                return
        endif
.
        getitem CashEditCheckNum,0,NCSHCHK
        call    Trim using NCSHCHK
        if (NCSHCHK = "")
		if (CAMOUNT <> C0)
	                alert   caution,"Valid Check Required!",result
                	move    YES,ReturnFlag
        	        setfocus CashEditCheckNum
	                return
		endif
        endif
        call    ZFillIt using NCSHCHK
        setitem CashEditCheckNum,0,NCSHCHK
.START PATCH 1.95 REPLACED LOGIC
.        if (NCSHCHK = "000MOA")
        if (NCSHCHK = "000000000MOA")
.END PATCH 1.95 REPLACED LOGIC
                setitem CashEditCheckDate,0,""
                setitem CashEditCheckAmt,0,""
                setitem CashEditPayor,0,""
        elseif (NCSHCHK <> "")
.START PATCH 1.4 REPLACED LOGIC
.                pack    NCHKFLD,CNUM,NCSHCHK
.                move    "VerifyDetail-NCHKTST",Location
.                pack    KeyLocation,"Key: ",NCHKFLD
.                call    NCHKTST
.                if over
..New Check Info
.                        move    CNUM,NCHKCONT
..
.                        move    NCSHCHK,NCHKNUM
..
.                        getitem CashEditCheckDate,0,str10
.                        call    Trim using str10
.                        call    RemoveChar using str10,SLASH
.                        count   N9,str10
.                        if (N9 <> 8)
.                                alert   caution,"Check Date must be CCYYMMDD Format!",result
.                                setprop CashEditCheckDate,enabled=1
.                                setfocus CashEditCheckDate
.                                move    YES,ReturnFlag
.                                return
.                        endif
.                        unpack  str10,MM,DD,CC,YY
.                        pack    NCHKDATE,CC,YY,MM,DD
..............................................
                pack    NCHKFLD,CNUM,CNUMDATE,NCSHCHK
                move    "VerifyDetail-NCHKTST",Location
                pack    KeyLocation,"Key: ",NCHKFLD
                call    NCHKTST
                if over
.New Check Info
                        move    CNUM,NCHKCONT
.
                        move    NCSHCHK,NCHKNUM
.
			move	CNUMDATE,NCHKCONTD
.
                        getitem CashEditCheckDate,0,str10
                        call    Trim using str10
                        call    RemoveChar using str10,SLASH
                        count   N9,str10
                        if (N9 <> 8)
                                alert   caution,"Check Date must be CCYYMMDD Format!",result
                                setprop CashEditCheckDate,enabled=1
                                setfocus CashEditCheckDate
                                move    YES,ReturnFlag
                                return
                        endif
                        unpack  str10,MM,DD,CC,YY
                        pack    NCHKDATE,CC,YY,MM,DD
.
.END PATCH 1.4 REPLACED LOGIC
.
                        getitem CashEditCheckAmt,0,str18
                        if (str18 = "")
                                alert   caution,"Valid Check Amount Required!",result
                                setprop CashEditCheckAmt,enabled=1
                                setfocus CashEditCheckAmt
                                move    YES,ReturnFlag
                                return
                        endif
                        call    RemoveChar using str18,COMMA
                        move    str18,NCHKAMT
.
                        getitem CashEditPayor,0,NCHKPAYOR
                        call    Trim using NCHKPAYOR
                        if (NCHKPAYOR = "")
                                alert   caution,"Valid Payor Required!",result
                                setprop CashEditPayor,enabled=1
                                setfocus CashEditPayor
                                move    YES,ReturnFlag
                                return
                        endif
.
	                move    "CheckLostFocus-NCHKWRT",Location
                        call    NCHKWRT
.Refresh ID variable
                        move    "00",CID
.Set flag to prompt refresh of Check ListView
                        move    "2",ChkFlag1
                endif
        endif
.......................................
.START PATCH 1.95 REPLACED LOGIC
.	if (NCSHCHK <> "000MOA")
	if (NCSHCHK <> "000000000MOA")
.END PATCH 1.95 REPLACED LOGIC
	        move    C0,Temp122
	        move    C0,Temp122B
	        move    NCHKAMT,Temp122
	        if (NewFlag = YES)
	                move    C0,Temp122C
	                move    C0,N1
	                clear   str2
	        else
	                move    CAMOUNT,Temp122C
	                move    C1,N1
	                move    CID,str2
	        endif
	        call    CashPackTemp
.START PATCH 1.4 REPLACED LOGIC
.	        call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,N1,str2,Temp122C
        	call    CashCalcTotal using NCHKCONT,NCHKNUM,Temp122,Temp122B,N1,str2,Temp122C,NCHKCONTD
.END PATCH 1.4 REPLACED LOGIC
	        call    CashPackTemp2
	        sub     CAMOUNT,Temp122

	        if (Temp122 < C0)
	                alert   caution,"Check Balance will not cover Amount!",result
	                move    YES,ReturnFlag
	                setfocus CashEditAmount
	                return
	        endif
	endif
.......................................
.
        getitem CashComboCode,0,result
        getitem CashComboCode,result,CEXTCD
.        if (NewFlag = YES)
        call    Trim using CEXTCD
        move    CLR,NCSHFLD3
        rep     ZFILL,NCSHFLD3
        pack    KeyLocation,"Key: ",NCSHFLD3
        move    "VerifyCEXTCD1-NCSHTST",Location
        call    NCSHTST
        loop
                until over
                until (NCSHFLD3 <> str6)
.START PATCH 1.95 REPLACED LOGIC
.                unpack  str10,str4,str7
                unpack  str16,str4,str12
.END PATCH 1.95 REPLACED LOGIC
                bump    str4
                if (str4 = CNUM)
.START PATCH 1.95 REPLACED LOGIC
.                        if (NewFlag = YES | (str2 <> CID AND str7 <> NCSHCHK))
                        if (NewFlag = YES | (str2 <> CID AND str12 <> NCSHCHK))
.END PATCH 1.95 REPLACED LOGIC
                                call    Trim using str1
.                                if (CEXTCD <> "")
.                                        if (str1 <> "")
.                                                alert   caution,"You already have this LR in this Control WITH an External Code!",result
.                                                move    YES,ReturnFlag
.                                                setfocus CashComboCode
.                                                goto CashVerifyDetailEnd
.                                        endif
.                                else
                                if (CEXTCD = "")
                                        if (str1 = "")
                                                clear   taskname
                                                append  "You already have this LR in this Control WITHOUT an External Code!",taskname
                                                append  NewLine,taskname
                                                append  "LR:  ",taskname
                                                append  STR6,taskname
                                                append  NewLine,taskname
                                                append  "CID:  ",taskname
                                                append  STR2,taskname
                                                append  NewLine,taskname
                                                append  "Control:  ",taskname
                                                append  str4,taskname
                                                append  NewLine,taskname
                                                append  "Check:  ",taskname
.START PATCH 1.95 REPLACED LOGIC
.                                                append  str7,taskname
                                                append  str12,taskname
.END PATCH 1.95 REPLACED LOGIC
                                                reset   taskname
                                                alert   caution,taskname,result
                                                move    YES,ReturnFlag
                                                setfocus CashComboCode
                                                goto CashVerifyDetailEnd
                                        endif
                                endif
                        endif
.START PATCH 1.95.2 ADDED LOGIC
			if (NewFlag = YES | str2 <> CID)
				if (CEXTCD = "P" AND str1 = "P")
					clear   taskname
					append  "You already have this LR in this Control with an External Code of 'P'!",taskname
					append  NewLine,taskname
					append  "LR:  ",taskname
					append  STR6,taskname
					append  NewLine,taskname
					append  "CID:  ",taskname
					append  STR2,taskname
					append  NewLine,taskname
					append  "Control:  ",taskname
					append  str4,taskname
					append  NewLine,taskname
					append  "Check:  ",taskname
					append  str12,taskname
					reset   taskname
					alert   caution,taskname,result
					move    YES,ReturnFlag
					setfocus CashComboCode
					goto CashVerifyDetailEnd
				endif
			endif
.END PATCH 1.95.2 ADDED LOGIC
.START PATCH 1.8 ADDED LOGIC
		else
			pack	taskname,"This LR is already in Control ",str4,".",newline,"Do you wish to Quit?"
			alert	plain,taskname,result
			if (result <> 2)
				move    YES,ReturnFlag
				setfocus CashEditLR
				goto CashVerifyDetailEnd
			endif
.END PATCH 1.8 ADDED LOGIC
                endif
                move    "VerifyCEXTCD1-NCSHKSA",Location
                call    NCSHKSA
        repeat
.        endif
        if (ppext = YES & (CEXTCD <> "P" & CEXTCD <> "Q" & CEXTCD <> "M"))
                alert   caution,"You must supply an External Code of 'P', 'Q' or 'M'!",result
                move    YES,ReturnFlag
                setfocus CashComboCode
                return
        endif
        if (CEXTCD = "D" | CEXTCD = "d" | CEXTCD = "N")
.Cannot enter Detail without Mailer, following code is overkill, but keep it just in case
                if (CMLR = "" | CMLR = "    ")
                        alert   caution,"Valid Mailer Required with that Code!",result
                        move    YES,ReturnFlag
                        setfocus CashEditMlr
                        return
.                elseif (NCSHBRK = "" | NCSHBRK = "    ")
.                        alert   caution,"Valid Broker Required with that Code!",result
.                        move    YES,ReturnFlag
.                        setfocus CashEditBroker
.                        return
                elseif (NCSHCHK = "" | NCSHCHK = "      ")
                        alert   caution,"Valid Check Required with that Code!",result
                        move    YES,ReturnFlag
                        setfocus CashEditCheckNum
                        return
                endif
.
                add     CAMOUNT,BALANCE
                move    CAMOUNT,AMOUNTB
        else
                if (CLR = "" | CLR = "      ")
			if (CEXTCD <> "M")
                        	alert   caution,"You must supply an External Code of 'D', 'd', 'N' or 'M'!",result
                	        move    YES,ReturnFlag
        	                setfocus CashComboCode
	                        return
			endif
                endif
                if (CEXTCD = "O" | CEXTCD = "Q")
.START PATCH 1.95 REPLACED LOGIC
.                        move    "000MOA",NCSHCHK
                        move    "000000000MOA",NCSHCHK
.END PATCH 1.95 REPLACED LOGIC
                        setitem CashEditCheckNum,0,NCSHCHK
                        clear   NCHKDATE
                        setitem CashEditCheckDate,0,NCHKDATE
                        setitem CashEditPayor,0,""
.
                        move    C2,NMOBPATH
                        clear   NMOAFLD4
.START PATCH 1.95 REPLACED LOGIC - TEMPORARY LOGIC
.                        pack    NMOAFLD4,NCSHBRK,CMLR
			pack	COMPFLD,CMLR
			move	"VerifyCode-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			move	COMPOLDMLR,str4
.
			pack	COMPFLD,NCSHBRK
			move	"VerifyCodeB-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
.
			call	Trim using COMPOLDBRK
			if (COMPOLDBRK = "")
				move	"0000",COMPOLDBRK
			endif
			pack    NMOAFLD4,COMPOLDBRK,str4
.END PATCH 1.95 REPLACED LOGIC - TEMPORARY LOGIC
                        rep     ZFILL,NMOAFLD4
                        move    "VerifyCode-NMOBKEY",Location
                        pack    KeyLocation,"Key: ",NMOAFLD4
                        call    NMOBKEY      .IS THERE ENOUGH MONEY ON ACCOUNT?
                        if over
                                clear   taskname
                                append  "Following Key Value not found in MOB file: ",taskname
                                append  NMOAFLD4,taskname
                                reset   taskname
                                alert   caution,taskname,result
                                setfocus CashEditAmount
                                move    YES,ReturnFlag
                                return
                        endif
                        add     CAMOUNT,BALANCE
                        move    CAMOUNT,AMOUNTB
                        if (BALANCE > C0)
.			compare C0,BALANCE
.                        if not LESS
                                clear   taskname
                                append  "Insufficient funds on Account by: ",taskname
                                append  BALANCE,taskname
                                append  NewLine,taskname
                                append  "Key Value: ",taskname
                                append  NMOAFLD4,taskname
                                reset   taskname
                                alert   caution,taskname,result
                                setfocus CashEditAmount
                                move    YES,ReturnFlag
                                return
                        endif
                endif
.Final check for previous record with MOA account
                if (CEXTCD <> "M")
                        if (NewFlag = YES)
                                move    CLR,NCSHFLD3
                                rep     ZFILL,NCSHFLD3
                                pack    KeyLocation,"Key: ",NCSHFLD3
                                move    "VerifyCEXTCD-NCSHTST",Location
                                call    NCSHTST
                                if not over
                                        if (str1 = "O" | str1 = "Q")
                                                clear   taskname
                                                append  "LR already in Cash file with",taskname
                                                append  NewLine,taskname
                                                append  "an External Code of '",taskname
                                                append  str1,taskname
                                                append  "'!",taskname
                                                reset   taskname
                                                alert   caution,taskname,result
                                                move    YES,ReturnFlag
                                                setfocus CashEditLR
                                                return
                                        else
                                                clear   taskname
                                                append  "LR is already in Cash file.",taskname
                                                append  NewLine,taskname
                                                append  "Do you want to add it anyway?",taskname
                                                reset   taskname
                                                alert   plain,taskname,result
                                                if (result <> C1)
                                                        move    YES,ReturnFlag
                                                        setfocus CashEditLR
                                                        return
                                                endif
                                        endif
                                endif
                        endif
                        if (CLR <> "" AND CLR <> "      ")
                                call    CashFindAdjAR using CLR,Temp122
.                                move    AR,Temp122
.                                clear   ASRECADJ
.                                move    CLR,NADJFLD
.                                move    "VerifyCode-NADJKEY",Location
.                                pack    KeyLocation,"Key: ",NADJFLD
.                                call    NADJKEY
.                                if not over
.                                        add     ASRECADJ,Temp122
.                                endif
                                if (Temp122 <> CAMOUNT)
                                        clear   taskname
                                        append  "Amount entered: ",taskname
                                        move    CAMOUNT,str15
                                        call    Trim using str15
                                        append  str15,taskname
                                        append  NewLine,taskname
                                        append  "does not match Invoice: ",taskname
                                        move    Temp122,str15
                                        call    Trim using str15
                                        append  str15,taskname
                                        reset   taskname
                                        alert   caution,taskname,result
                                        move    YES,ReturnFlag
                                        setfocus CashEditAmount
                                        return
                                endif
                        endif
                endif
                if (CEXTCD = "")
                        move    B1,CEXTCD
                endif
        endif
.
        unpack  B55,CFILL1,CFILL2,NCKDTEC,NCKDTEY,NCKDTEM,NCKDTED
        move    B55,NPAYOR
CashVerifyDetailEnd
        return

CashFindAdjAR LRoutine DimPtr,FrmPtr
.Assumes a call to Invoice file has already occurred!
        move    C0,FrmPtr
        move    AR,FrmPtr
        clear   ASRECADJ
        move    DimPtr,NADJFLD
        move    "C.FindAdjAR-NADJKEY",Location
        pack    KeyLocation,"Key: ",NADJFLD
        call    NADJKEY
        if not over
                add     ASRECADJ,FrmPtr
        endif
        return

CashFindAdjAP LRoutine DimPtr,FrmPtr
.Assumes a call to Invoice file has already occurred!
        if (AP2 > C0)
                move    AP2,FrmPtr
        else
                move    AP1,FrmPtr
        endif
        clear   ASPAYAD1
        clear   ASPAYAD2
        move    DimPtr,NADJFLD
        move    "C.FindAdjAP-NADJKEY",Location
        pack    KeyLocation,"Key: ",NADJFLD
        call    NADJKEY
        if not over
                if (AP2 > C0)
                        add     ASPAYAD2,FrmPtr
                else
                        add     ASPAYAD1,FrmPtr
                endif
        endif
        return

CashGetNextControl Routine DimPtr
.START PATCH 1.4 ADDED LOGIC
.Possible logic, once we stop archiving CONTROLS.DAT at the end of the year
.	unpack	timestamp,str8		.Get Date, which is part of Key
.	move	str8,str9
.Possible logic, if we decide to allow only one Control per year
.	unpack	timestamp,str4		.Get Date, which is part of Key
.	move	str4,str5
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.8 ADDED LOGIC
	clock   timestamp,timestamp
	unpack  timestamp,str4
	move	str4,str5
.END PATCH 1.8 ADDED LOGIC
        move    "NCSHNEXT",GNXTFLD
        move    "GetNext-GNXTKEY",Location
        pack    KeyLocation,"Key: ",GNXTFLD
        call    GNXTKEY
        move    C0,N6
        move    GNXTNUM,N6
        loop
                move    N6,str6
.START PATCH 1.4 REPLACED LOGIC
.                unpack  str6,NCTRFLD,NCTRFLD
.                call    Trim using NCTRFLD
.                call    ZFillIt using NCTRFLD,C0
.                move    "GetNext-NCTRTST",Location
.                pack    KeyLocation,"Key: ",NCTRFLD
.                call    NCTRTST
.                until over
                unpack  str6,str3,str3
                call    Trim using str3
                call    ZFillIt using str3,C0
		pack	NCTRFLD1,"01X",str3
                move    "GetNext-NCTRAIMA",Location
                pack    KeyLocation,"Key: ",NCTRFLD1
                call    NCTRAIMA
                until over
.START PATCH 1.8 ADDED LOGIC
.Default to one particular Control per year
		loop
			if over
				goto CashGetNextControleEnd
			endif
			move	str8,str4
			until (str4 = str5)
	                move    "GetNext-NCTRKGA",Location
	                call    NCTRKGA
		repeat
		if (str4 <> str5)
			break
		endif
.END PATCH 1.8 ADDED LOGIC
.END PATCH 1.4 REPLACED LOGIC
                if (N6 >= 999)
                        move    C0,N6
                else
                        add     C1,N6
                endif
        repeat
.START PATCH 1.4 REPLACED LOGIC
.        move    NCTRFLD,DimPtr
CashGetNextControleEnd
        move    str3,DimPtr
.END PATCH 1.4 REPLACED LOGIC
        return

Cash2FindInvoice
        move	OLRN,NINVFLD
       	move    C1,NINVPATH
        move    "Cash2OK-NINVKEY",Location
       	pack    KeyLocation,"Key: ",NINVFLD
        call    NINVKEY
       	if not over
.Test if Order is paid
      	        if (STATB <> "P")
			call	Cash2LoadListView
      	        endif
        endif
        return

Cash2LoadListView
        pack    hold3,INVVARS
        call    CashFindAdjAR using LRN,Temp122
        move    Temp122,AR
        add     AR,ARTotal
        unpack  AR,str10,str3
        call    Trim using str10
        call    FormatNumeric using str10,str13
        pack    str25,str13,str3
.	if (AP2 > C0)
.		unpack  AP2,str10,str3
.	else
.		unpack  AP1,str10,str3
.	endif
.        call    Trim using str10
.        call    FormatNumeric using str10,str13
.        pack    str35,str13,str3
        call    CashFindAdjAP using LRN,Temp122
        unpack  Temp122,str12,str3
        call    Trim using str12
        call    FormatNumeric using str12,str15
        pack    str35,str15,str3
.
        pack    str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
        if (str8 = "00000000")  .Dummy Order - Use Invoice Date
                pack    str8,INVDTEC,INVDTEY,INVDTEM,INVDTED
                pack    str11,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY,"i"
        else
                pack    str11,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
        endif
        Cash2ListView.InsertItem giving N9 using str8
        Cash2ListView.SetItemText using N9,str11,1
        Cash2ListView.SetItemText using N9,LRN,2
        pack    str7,INVNUM,GUARPAY
        Cash2ListView.SetItemText using N9,str7,3
        Cash2ListView.SetItemText using N9,MLRN,4
        Cash2ListView.SetItemText using N9,IBRKNUM,5
        Cash2ListView.SetItemText using N9,OMLRKY,6
        Cash2ListView.SetItemText using N9,str25,7
        Cash2ListView.SetItemText using N9,str35,8
        Cash2ListView.SetItemText using N9,hold3,9
.
        Cash2ListView2.InsertItem giving N9 using LRN
        Cash2ListView2.SetItemText using N9,str7,1
        Cash2ListView2.SetItemText using N9,str11,2
        Cash2ListView2.SetItemText using N9,MLRN,3
        Cash2ListView2.SetItemText using N9,IBRKNUM,4
        Cash2ListView2.SetItemText using N9,OMLRKY,5
        Cash2ListView2.SetItemText using N9,str25,6
        Cash2ListView2.SetItemText using N9,str35,7
        Cash2ListView2.SetItemText using N9,hold3,8
.
        Cash2ListView3.InsertItem giving N9 using str7
        Cash2ListView3.SetItemText using N9,LRN,1
        Cash2ListView3.SetItemText using N9,str11,2
        Cash2ListView3.SetItemText using N9,MLRN,3
        Cash2ListView3.SetItemText using N9,IBRKNUM,4
        Cash2ListView3.SetItemText using N9,OMLRKY,5
        Cash2ListView3.SetItemText using N9,str25,6
        Cash2ListView3.SetItemText using N9,str35,7
        Cash2ListView3.SetItemText using N9,hold3,8
.
	move	AR,str13
        Cash2ListView4.InsertItem giving N9 using str13
        Cash2ListView4.SetItemText using N9,str25,1
        Cash2ListView4.SetItemText using N9,LRN,2
        Cash2ListView4.SetItemText using N9,str7,3
        Cash2ListView4.SetItemText using N9,str11,4
        Cash2ListView4.SetItemText using N9,MLRN,5
        Cash2ListView4.SetItemText using N9,IBRKNUM,6
        Cash2ListView4.SetItemText using N9,OMLRKY,7
        Cash2ListView4.SetItemText using N9,str35,8
        Cash2ListView4.SetItemText using N9,hold3,9
        return
CashCalcARSelTotal LRoutine FrmPtr
        setitem Cash2StatARSelTotal,0,""
        move    C0,ARSelTotal
	move	SEQ,result
	loop
		move	result,N9
		if (View2Flag = 1)
			Cash2ListView.GetNextItem giving result using C2,N9
		elseif (View2Flag = 2)
			Cash2ListView2.GetNextItem giving result using C2,N9
		elseif (View2Flag = 3)
			Cash2ListView3.GetNextItem giving result using C2,N9
		elseif (View2Flag = 4)
			Cash2ListView4.GetNextItem giving result using C2,N9
		endif
		until (result = SEQ)
		if (View2Flag = 1)
			Cash2ListView.GetItemText giving str13 using result,C7
		elseif (View2Flag = 2)
			Cash2ListView2.GetItemText giving str13 using result,C6
		elseif (View2Flag = 3)
			Cash2ListView3.GetItemText giving str13 using result,C6
		elseif (View2Flag = 4)
			Cash2ListView4.GetItemText giving str13 using result,C1
		endif
                call    RemoveChar using str13,COMMA
                move    C0,AR
                move    str13,AR
                add     AR,ARSelTotal
        repeat
        if (ARSelTotal > C0)
                unpack  ARSelTotal,str12,str3
                call    Trim using str12
                call    FormatNumeric using str12,str15
                pack    str25,str15,str3
                setitem Cash2StatARSelTotal,0,str25
        endif
        return

.CashCalcUnApplied Routine DimPtr,DimPtr1,FrmPtr
..DimPtr  = Control Number
..DimPtr1 = Check Number
..FrmPtr  = Return value (UnApplied Amount)
.        move    C0,Temp122B
.	move	C2,NCSHPATH
.	clear	NCSHFLD
.        clear   NCSHFLD1
.	clear	NCSHFLD2
.	clear	NCSHFLD4
.        call    Trim DimPtr
.        call    Trim DimPtr1
.        if (DimPtr <> "" | DimPtr1 <> "")
.                pack	NCSHFLD2,"03X",DimPtr
.                pack	NCSHFLD4,"04X",DimPtr1
.        	move	"CalcUn-NCSHAIM",Location
.	        pack	KeyLocation,"Key: ",NCSHFLD2,NCSHFLD4
.        	call	NCSHAIM
.	        loop
.                        until over
.                        add     CAMOUNT,Temp122B
.                        move	"C.LoadControl-NCSHKG",Location
.        	        pack	KeyLocation,"Key: ",NCSHFLD2
.                	call	NCSHKG
.                repeat
.        endif
.        sub     Temp122B,FrmPtr
.        return

.START PATCH 1.4 REPLACED LOGIC
.CashCalcTotal Routine DimPtr,DimPtr1,FrmPtr,FrmPtr1,FrmPtr2,DimPtr2,FrmPtr3
CashCalcTotal Routine DimPtr,DimPtr1,FrmPtr,FrmPtr1,FrmPtr2,DimPtr2,FrmPtr3,DimPtr3
.END PATCH 1.4 REPLACED LOGIC
.DimPtr  = Control Number
.DimPtr1 = Check Number
.FrmPtr  = Check Amount + Return value (UnApplied Amount)
.FrmPtr1 = Return value (Check Total for Detail records)
.FrmPtr2 = Flag used to determine if a record is not to be calculated
.DimPtr2 = CID (Key value used in conjunction with FrmPtr2)
.FrmPtr3 = CAMOUNT (Key value used in conjunction with FrmPtr2)
.DimPtr3 = Control Date
        move    C0,FrmPtr1
	move	C2,NCSHPATH
	clear	NCSHFLD
        clear   NCSHFLD1
	clear	NCSHFLD2
	clear	NCSHFLD4
.START PATCH 1.4 ADDED LOGIC
	clear	NCSHFLD5
        call    Trim DimPtr3
.END PATCH 1.4 ADDED LOGIC
        call    Trim DimPtr
        call    Trim DimPtr1
.START PATCH 1.95 REPLACED LOGIC
.        if (DimPtr1 = "000MOA")
        if (DimPtr1 = "000000000MOA")
.END PATCH 1.95 REPLACED LOGIC
                move    C0,FrmPtr
        elseif (DimPtr <> "" | DimPtr1 <> "")
                pack	NCSHFLD2,"03X",DimPtr
                pack	NCSHFLD4,"04X",DimPtr1
.START PATCH 1.4 REPLACED LOGIC
.        	move	"CalcUn-NCSHAIM",Location
.	        pack	KeyLocation,"Key: ",NCSHFLD2,NCSHFLD4
		pack	NCSHFLD5,"05X",DimPtr3
        	move	"CalcUn-NCSHAIM",Location
	        pack	KeyLocation,"Key: ",NCSHFLD2,NCSHFLD4,NCSHFLD5
.END PATCH 1.4 REPLACED LOGIC
        	call	NCSHAIM
	        loop
                        until over
                        if (FrmPtr2 = 1)
                                if (DimPtr2 = CID & FrmPtr3 = CAMOUNT)
                                        goto CashCalcTotalSkip
                                endif
                        endif
                        add     CAMOUNT,FrmPtr1
CashCalcTotalSkip
                        move	"C.LoadControl-NCSHKG",Location
.START PATCH 1.4 REPLACED LOGIC
.        	        pack	KeyLocation,"Key: ",NCSHFLD2,NCSHFLD4
        	        pack	KeyLocation,"Key: ",NCSHFLD2,NCSHFLD4,NCSHFLD5
.END PATCH 1.4 REPLACED LOGIC
                	call	NCSHKG
                repeat
        endif
        sub     FrmPtr1,FrmPtr
        return

CashPackTemp
        pack    CID2,CID
        pack    CMLR2,CMLR
        pack    CLR2,CLR
        pack    CFILL12,CFILL1
        pack    CCE2,CCE
        pack    CYR2,CYR
        pack    CMO2,CMO
        pack    CDY2,CDY
        move    CAMOUNT,CAMOUNT2
        pack    CEXTCD2,CEXTCD
        pack    CFILL22,CFILL2
        pack    CNUM2,CNUM
.START PATCH 1.4 ADDED LOGIC
	pack	CNUMDATE2,CNUMDATE
.END PATCH 1.4 ADDED LOGIC
        pack    NCSHCHK2,NCSHCHK
.        pack    nckdtec2,nckdtec
.        pack    nckdtey2,nckdtey
.        pack    nckdtem2,nckdtem
.        pack    nckdted2,nckdted
.        pack    npayor2,npayor
        pack    NCSHBRK2,NCSHBRK
        return

CashPackTemp2
        pack    CID,CID2
        pack    CMLR,CMLR2
        pack    CLR,CLR2
        pack    CFILL1,CFILL12
        pack    CCE,CCE2
        pack    CYR,CYR2
        pack    CMO,CMO2
        pack    CDY,CDY2
        move    CAMOUNT2,CAMOUNT
        pack    CEXTCD,CEXTCD2
        pack    CFILL2,CFILL22
        pack    CNUM,CNUM2
.START PATCH 1.4 ADDED LOGIC
	pack	CNUMDATE,CNUMDATE2
.END PATCH 1.4 ADDED LOGIC

        pack    NCSHCHK,NCSHCHK2
.        pack    nckdtec,nckdtec2
.        pack    nckdtey,nckdtey2
.        pack    nckdtem,nckdtem2
.        pack    nckdted,nckdted2
.        pack    npayor,npayor2
        pack    NCSHBRK,NCSHBRK2
        return

.............................................................
....................GUI HOUSEKEEPING.........................
.............................................................
CashSwitchTab LRoutine FrmPtr
        if (TabNum <> FrmPtr)
                move    TabNum,N2
                call    CashTabClick
                move    FrmPtr,N2
                call    CashTabChange
                setitem CashTabControl,0,FrmPtr
        endif
        return
CashTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
        if (N2 = C1)
                Deactivate Ncsh001a
        elseif (N2 = C2)
                Deactivate Ncsh001b
        elseif (N2 = C3)
		call	DisableAdjustForm
        elseif (N2 = C4)
        elseif (N2 = C5)
        elseif (N2 = C6)
        elseif (N2 = C7)
        else    N2 = C8
        endif
        return

CashTabChange
        move    N2,TabNum
.
        if (N2 = C1)
                Activate Ncsh001a
.START PATCH 1.95 REPLACED LOGIC
.                if (ViewFlag = 1)
.                        setprop	CashListViewA,visible=0
.                        setprop	CashListViewB,visible=0
.                        setprop	CashListViewC,visible=0
.                elseif (ViewFlag = 2)
.                        setprop	CashListView,visible=0
.                        setprop	CashListViewB,visible=0
.                        setprop	CashListViewC,visible=0
.                elseif (ViewFlag = 3)
.                        setprop	CashListViewA,visible=0
.                        setprop	CashListView,visible=0
.                        setprop	CashListViewC,visible=0
.                elseif (ViewFlag = 4)
.                        setprop	CashListViewA,visible=0
.                        setprop	CashListViewB,visible=0
.                        setprop	CashListView,visible=0
.............................................................
		if (ViewFlag = 1)
			setprop	CashListViewA,visible=0
			setprop	CashListViewB,visible=0
			setprop	CashListViewC,visible=0
			setprop	CashListViewD,visible=0
		elseif (ViewFlag = 2)
			setprop	CashListView,visible=0
			setprop	CashListViewB,visible=0
			setprop	CashListViewC,visible=0
			setprop	CashListViewD,visible=0
		elseif (ViewFlag = 3)
			setprop	CashListViewA,visible=0
			setprop	CashListView,visible=0
			setprop	CashListViewC,visible=0
			setprop	CashListViewD,visible=0
		elseif (ViewFlag = 4)
			setprop	CashListViewA,visible=0
			setprop	CashListViewB,visible=0
			setprop	CashListView,visible=0
			setprop	CashListViewD,visible=0
		elseif (ViewFlag = 5)
			setprop	CashListView,visible=0
			setprop	CashListViewA,visible=0
			setprop	CashListViewB,visible=0
			setprop	CashListViewC,visible=0
.END PATCH 1.95 ADDED LOGIC
                endif
.START PATCH 1.95 REPLACED LOGIC
.                if (View1Flag = 1)
.                        setprop	CashListView2A,visible=0
.                        setprop	CashListView2B,visible=0
.                        setprop	CashListView2C,visible=0
.                elseif (View1Flag = 2)
.                        setprop	CashListView2,visible=0
.                        setprop	CashListView2B,visible=0
.                        setprop	CashListView2C,visible=0
.                elseif (View1Flag = 3)
.                        setprop	CashListView2A,visible=0
.                        setprop	CashListView2,visible=0
.                        setprop	CashListView2C,visible=0
.                elseif (View1Flag = 4)
.                        setprop	CashListView2A,visible=0
.                        setprop	CashListView2B,visible=0
.                        setprop	CashListView2,visible=0
.                endif
......................................................
		if (View1Flag = 1)
			setprop	CashListView2A,visible=0
			setprop	CashListView2B,visible=0
			setprop	CashListView2C,visible=0
			setprop	CashListView2D,visible=0
		elseif (View1Flag = 2)
			setprop	CashListView2,visible=0
			setprop	CashListView2B,visible=0
			setprop	CashListView2C,visible=0
			setprop	CashListView2D,visible=0
		elseif (View1Flag = 3)
			setprop	CashListView2A,visible=0
			setprop	CashListView2,visible=0
			setprop	CashListView2C,visible=0
			setprop	CashListView2D,visible=0
		elseif (View1Flag = 4)
			setprop	CashListView2A,visible=0
			setprop	CashListView2B,visible=0
			setprop	CashListView2D,visible=0
			setprop	CashListView2,visible=0
		elseif (View1Flag = 4)
			setprop	CashListView2,visible=0
			setprop	CashListView2A,visible=0
			setprop	CashListView2B,visible=0
			setprop	CashListView2C,visible=0
		endif
.END PATCH 1.95 REPLACED LOGIC
        elseif (N2 = C2)
                Activate Ncsh001b
                if (View2Flag = 1)
                        setprop	Cash2ListView2,visible=0
                        setprop	Cash2ListView3,visible=0
                        setprop	Cash2ListView4,visible=0
                elseif (View2Flag = 2)
                        setprop	Cash2ListView,visible=0
                        setprop	Cash2ListView3,visible=0
                        setprop	Cash2ListView4,visible=0
                elseif (View2Flag = 3)
                        setprop	Cash2ListView,visible=0
                        setprop	Cash2ListView2,visible=0
                        setprop	Cash2ListView4,visible=0
                elseif (View2Flag = 4)
                        setprop	Cash2ListView,visible=0
                        setprop	Cash2ListView2,visible=0
                        setprop	Cash2ListView3,visible=0
                endif
        elseif (N2 = C3)
		call	EnableAdjustForm
        elseif (N2 = C4)
        elseif (N2 = C5)
        elseif (N2 = C6)
        elseif (N2 = C7)
        else   .N2 = C8
        endif
        return

CashAmountLostFocus Routine EditPtr
	getitem	EditPtr,0,str15
        call    RemoveChar using str15,COMMA
        move	C0,Temp122
	move	str15,Temp122
	move	Temp122,str15
        unpack  str15,str12,str3
        call    Trim using str12
        call    FormatNumeric using str12,str15
        pack    str18,str15,str3
	setitem	EditPtr,0,str18
        return

CashSetItem LRoutine DimPtr
        move    C0,N10
        call    Trim using DimPtr
.Return if Account did not have any detail records associated with it.
        if (DimPtr = "")
                goto CashSetItemEnd
        endif
        move    SEQ,result
        loop
                move    result,N9
                if (ViewFlag = 1)
                        CashListView.GetNextItem giving result using C0,N9
                elseif (ViewFlag = 2)
                        CashListViewA.GetNextItem giving result using C0,N9
                elseif (ViewFlag = 3)
                        CashListViewB.GetNextItem giving result using C0,N9
                elseif (ViewFlag = 4)
                        CashListViewC.GetNextItem giving result using C0,N9
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        CashListViewD.GetNextItem giving result using C0,N9
.END PATCH 1.95 ADDED LOGIC
                endif
                until (result = SEQ)
.START PATCH 1.4 REPLACED LOGIC
.                if (ViewFlag = 1)
.                        CashListView.GetItemText giving str9 using result,C6
.                elseif (ViewFlag = 2)
.                        CashListViewA.GetItemText giving str9 using result,C6
.                elseif (ViewFlag = 3)
.                        CashListViewB.GetItemText giving str9 using result,C7
.                elseif (ViewFlag = 4)
.                        CashListViewC.GetItemText giving str9 using result,C6
.                endif
.                if (str9 = DimPtr)
.START PATCH 1.95 REPLACED LOGIC
.                if (ViewFlag = 1)
.                        CashListView.GetItemText giving str17 using result,C6
.                elseif (ViewFlag = 2)
.                        CashListViewA.GetItemText giving str17 using result,C6
.                elseif (ViewFlag = 3)
.                        CashListViewB.GetItemText giving str17 using result,C7
.                elseif (ViewFlag = 4)
.                        CashListViewC.GetItemText giving str17 using result,C6
.                endif
.                if (str17 = DimPtr)
                if (ViewFlag = 1)
                        CashListView.GetItemText giving str23 using result,C6
                elseif (ViewFlag = 2)
                        CashListViewA.GetItemText giving str23 using result,C6
                elseif (ViewFlag = 3)
                        CashListViewB.GetItemText giving str23 using result,C7
                elseif (ViewFlag = 4)
                        CashListViewC.GetItemText giving str23 using result,C6
.START PATCH 1.95 ADDED LOGIC
                elseif (ViewFlag = 5)
                        CashListViewD.GetItemText giving str23 using result,C7
.END PATCH 1.95 ADDED LOGIC
                endif
                if (str23 = DimPtr)
.END PATCH 1.95 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
                        move    result,N10
                endif
        repeat
CashSetItemEnd
        if (ViewFlag = 1)
                CashListView.SetItemState giving N9 using N10,2,2
                CashListView.EnsureVisible using N10,0
                call    Click_CashListView
        elseif (ViewFlag = 2)
                CashListViewA.SetItemState giving N9 using N10,2,2
                CashListViewA.EnsureVisible using N10,0
                call    Click_CashListViewA
        elseif (ViewFlag = 3)
                CashListViewB.SetItemState giving N9 using N10,2,2
                CashListViewB.EnsureVisible using N10,0
                call    Click_CashListViewB
        elseif (ViewFlag = 4)
                CashListViewC.SetItemState giving N9 using N10,2,2
                CashListViewC.EnsureVisible using N10,0
                call    Click_CashListViewC
.START PATCH 1.95 ADDED LOGIC
        elseif (ViewFlag = 5)
                CashListViewD.SetItemState giving N9 using N10,2,2
                CashListViewD.EnsureVisible using N10,0
                call    Click_CashListViewD
.END PATCH 1.95 ADDED LOGIC
        endif
        return

CashSetItem2 LRoutine DimPtr
        move    C0,N10
        call    Trim using DimPtr
.Return if Account did not have any detail records associated with it.
        if (DimPtr = "")
                goto CashSetItem2End
        endif
        move    SEQ,result
        loop
                move    result,N9
                if (View1Flag = 1)
                        CashListView2.GetNextItem giving result using C0,N9
                elseif (View1Flag = 2)
                        CashListView2A.GetNextItem giving result using C0,N9
                elseif (View1Flag = 3)
                        CashListView2B.GetNextItem giving result using C0,N9
                elseif (View1Flag = 4)
                        CashListView2C.GetNextItem giving result using C0,N9
.START PATCH 1.95 REPLACED LOGIC
                elseif (View1Flag = 5)
                        CashListView2D.GetNextItem giving result using C0,N9
.END PATCH 1.95 REPLACED LOGIC
                endif
                until (result = SEQ)
.START PATCH 1.95 REPLACED LOGIC
.                if (View1Flag = 1)
.                        CashListView2.GetItemText giving str50 using result,C9
.                elseif (View1Flag = 2)
.                        CashListView2A.GetItemText giving str50 using result,C9
.                elseif (View1Flag = 3)
.                        CashListView2B.GetItemText giving str50 using result,C9
.                elseif (View1Flag = 4)
.                        CashListView2C.GetItemText giving str50 using result,C9
.                endif
.                unpack  str50,str2,str4,str6,str10,str13,str1,str1,str9
.                pack    str30,str2,str6,str13,str9
.                call    Trim using str30
.                if (str30 = DimPtr)
		if (View1Flag = 1)
			CashListView2.GetItemText giving str55 using result,C9
		elseif (View1Flag = 2)
			CashListView2A.GetItemText giving str55 using result,C9
		elseif (View1Flag = 3)
			CashListView2B.GetItemText giving str55 using result,C9
		elseif (View1Flag = 4)
			CashListView2C.GetItemText giving str55 using result,C9
		elseif (View1Flag = 5)
			CashListView2D.GetItemText giving str55 using result,C10
                endif
                unpack  str55,str2,str6,str6,str10,str13,str1,str1,str15
                pack    str40,str2,str6,str13,str15
                call    Trim using str40
                if (str40 = DimPtr)
.END PATCH 1.95 REPLACED LOGIC
                        move    result,N10
                endif
        repeat
CashSetItem2End
        if (View1Flag = 1)
                CashListView2.SetItemState giving N9 using N10,2,2
                CashListView2.EnsureVisible using N10,0
                call    Click_CashListView2
        elseif (View1Flag = 2)
                CashListView2A.SetItemState giving N9 using N10,2,2
                CashListView2A.EnsureVisible using N10,0
                call    Click_CashListView2A
        elseif (View1Flag = 3)
                CashListView2B.SetItemState giving N9 using N10,2,2
                CashListView2B.EnsureVisible using N10,0
                call    Click_CashListView2B
        elseif (View1Flag = 4)
                CashListView2C.SetItemState giving N9 using N10,2,2
                CashListView2C.EnsureVisible using N10,0
                call    Click_CashListView2C
.START PATCH 1.95 REPLACED LOGIC
        elseif (View1Flag = 5)
                CashListView2D.SetItemState giving N9 using N10,2,2
                CashListView2D.EnsureVisible using N10,0
                call    Click_CashListView2D
.END PATCH 1.95 REPLACED LOGIC
        endif
        return
;
CashSetMouseBusy
        setmode *mcursor=*wait
        return
CashSetMouseFree
        setmode *mcursor=*arrow
        return

CashDisableSave
        setitem CashSave,0,"Save"
        return
CashEnableSave
        setitem CashSave,0,"Sa&ve"
        return

        include nctrio.inc
        include nchkio.inc
        include ncshio.inc
        include nordio.inc
;begin patch 1.96
;        include ninvio.inc
        	include 	ninvio.inc
	Include	NInvAcdio.inc
;end patch 1.96
        include nadjio.inc
        include njstio.inc
        include nmoaio.inc
        include nmobio.inc
.START PATCH 1.94 REPLACED LOGIC
.        include nmlrio.inc
.        include nbrkio.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.94 REPLACED LOGIC
        include gnxtio.inc
	include	npasio.inc
.Used with Search logic
        include searchio.inc
        include ndatio.inc
        include nrtnio.inc
;begin patch 1.93
	Include	MOANotesIO.inc
;end patch 1.93
        include ncmpio.inc
.START PATCH 1.6 ADDED LOGIC
	include	nownio.inc
.END PATCH 1.6 ADDED LOGIC
..........................
	include	comlogic.inc

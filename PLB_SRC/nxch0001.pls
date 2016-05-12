*******************************************************************************
. THIS IS THE EXCHANGE STATUS MAINTENANCE PROGRAM    MARCH 1982
*******************************************************************************
. Written for Names in the News California By David Herrick
*******************************************************************************
.
* VARIABLES USED BY THE EXCHANGE STATUS PROGRAM.
. ..............................................
PC        EQU       1
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NDATDD.inc
.patch7.6
.         INCLUDE   NMLRDD.inc
          include   compdd.inc
          include   cntdd.inc
.patch7.6
.         INCLUDE   \\nts0\c\library\develop\backups\NXNGDD.inc
          INCLUDE   NXNGDD.inc
          INCLUDE   NXRFDD.inc
          INCLUDE   NXCHDD.inc
.         INCLUDE   \\nts0\c\library\develop\backups\NXCHDD.inc
          INCLUDE   NXRF1DD.INC
          include   nofrdd.inc
.Patch7.6
.         include   nbrkdd.inc
.Patch7.6
          include   media.inc
          include   NCNTDD.inc
          include   shipping.inc
          include   nspedd.inc
          include   nspidd.inc
          INCLUDE   NPASDD.inc
          include   nrtndd.inc
          INCLUDE   NORDDD.inc
          include   nowndd.inc
          INCLUDE   GNXTDD.inc
          INCLUDE   OSLSPERN.inc
          include   logdata.inc
          include   nusedd.inc
..............................................................
.Following used only in order to load Search.plf
          include   ncmpdd.inc
...............................................................................................
Release   Init        "8.01"    DLH    .auto fill mailer fields on IS tab from main screen
RelDate   Init           "2015 October 05"
.Release   Init        "8.00"    DLH    .allow drill down from summary list view
.RelDate   Init           "2015 March 6"
.Release   Init        "7.97"    DLH    .Getlistuniverse fixed 
.RelDate   Init           "2015 February 23"
.Release   Init        "7.96"    DLH    .allow more options with summary excel
.RelDate   Init           "2014 August 5"
.Release   Init       "7.95"    DLH    .set PDF as default print option
.RelDate   Init           "2014 May 2"
.Release   Init                "7.94"    DLH    allow Excel on summary report
.RelDate   Init           "14 March 2012"
.Release   Init                "7.93"    DLH    Select exclusive includes PL
.RelDate   Init           "02 March 2009"

.Release   Init                "7.92"    DLH    Temp comment var bumped up to match record
.RelDate   Init           "11 February 2009"
.Release  Init                "7.91"    JD    Variable ISLIST updated to include JL
.RelDate  Init           "17 June 2008"
.Release  Init                "7.9"     DLH Use Sendmail
.RelDate  Init           "22 April 2008"
.Release            Init                "7.8"     DLH 22May2007 Pacific Lists and changed butil to run locally
.RelDate        Init           "22 May 2007"
.Release            Init                "7.74"                      JD 20Nov2006 Updated ISLIST variable.
.Release            Init                "7.73"              DS 25April2006 bug fix on search box double-click work order #1058
.Release            Init                "7.72"              DS        20March2006         Remove uneeded/inaccurate message box (work order 959)
.Release        Init           "7.71"            JD       12JAN2005   Include inactive accounts if checked
.Release        Init           "7.7"            ASH      15MAR2005    Exchange File Conversion
.RelDate        Init           "12 December 2005"
.RelDate        Init           "15 March 2005"
.         Global variable changes for reading ease: (Undocumented - in order to keep code clean!)
.                   NXCH0001STATTEXT001 - > NxchStatSearchMlr1
.                   NXCH0001STATTEXT002 - > NxchStatSearchMlr2
.                   NXCH0001EditText001 - > NxchEditSearchMlr1
.                   NXCH0001EDITTEXT002 - > NxchEditSearchMlr2
.                   NXCH0001MLRCOMP001  - > NxchStatSearchMlr1Name
.                   NXCH0001MLRCOMP002  - > NxchStatSearchMlr2Name
.                   NXCH0001StatActive  - > NxchStatActive
.....................................
.Release        Init           "7.6"            DMB      26MAY2004    Mailer Conversion
.Release        Init           "7.5"            ASH    20FEB2004 DATACARD CONVERSION
.RelDate        Init           "20 February 2004"
.Release        Init           "7.4"            DLH    17July2003 Force Beg Bal option on detail reports
.RelDate        Init           "17 July 2003"
.Release  init       "7.3"                     .ASH   17JUN03 ADDED SORT OPTION VIA ORDER DATE
.Release  init       "7.2"                     .DMB   04SEP02 Fixed bug which did not allow modification of a detail record when calling up record by lr(was comparing last read of all details to detail record shown in detail screen-not always the same)
.Release  init       "7.1"                    .DMB   03SEP02 Modified code to show list cross reference even if only one record
.Release  init       "7.0"                    .DMB   19JUN02 IS Only Added Reinstatement button to detail page-will add qty back if adjusted and clear stat
.Release  init      "6.92"                    .DMB   17MAY02 Modified code to include flag for names owed to client/never used
.Release  init      "6.91"                    .DMB   16MAY02 Added code to display Inactive accounts
.Release  init      "6.9"                     .DMB   15MAY02 Cleaned up inactivate button code
.Release  init      "6.8"                     .DMB   13MAY02 Added Code Allow IS to inactivate Clients
.Release  init      "6.7"                     .DMB   10MAY02 Added Code to grab balances on lists that we are exclusive list managers
.Release  init      "6.6"                     .DMB  01MAR02  Added Code and form to have a dialog box to tell user that prog is processing rpt
.Release  init      "6.5"                     .DMB  27FEB02  Added/Modified code to make more user friendly
.Release  init      "6.1"                     .DMB  21FEB02  Added busy cursor in print section to show when busy creating file for printing
.Release  init      "6.0"                     .DMB  14FEB02  Fixed bugs in detail screen to add verf etc.
.Release  init      "5.1"                     .DLH  15Jan02  tighten up data verf for reports
.Release  init      "5.0"                     .TEXT TO GUI AND LOGIC REWRITE DLH
.BEGIN PATCH 7.7 ADDED LOGIC
.EXTERNAL ROUTINES FROM       INFO.PLC
NxchLoadForm external "INFO;LoadForm"
NxchDisplayCompany external "INFO;DisplayCompany" .05/01/2008 DLH
NxchDisplayList external "INFO;DisplayList"
.
MouseForm form      10
T1        form      4
L1        form      4
.
EditPtr   EditText  ^
StatPtr   StatText  ^
FrmPtr    form                ^
colordim dim        8
.END PATCH 7.7 ADDED LOGIC
Akey1    init      "01X"
Akey4    init      "04X"
.Following key not used by program but required for wsearch.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
datestring dim     10
Indexstring dim    10
str10a  dim        10
Carr    init    0x7f
testff  init    0xff
test55  init    0x55
test00  init    0x00
test01  init    0x01
test1   init    0x33
test2   init    0x08
.hexeight integer 4,"4294967295"
hextwo  init    0x02
hexfour init    0x04
ScanBreak form  "0"
.....................................................
.begin patch 7.94
ExcelFlag Dim       1
.end Patch 7.94
umbrflag      form          1              1=no, 2=umbrella exch org.
.BEGIN PATCH 7.7 REPLACED LOGIC
.MLR1          DIM           4              MAILER#1
.MLR2          DIM           4              MAILER # 2
MLR1          DIM           6              MAILER#1
MLR2          DIM           6              MAILER # 2
.I use following instead of str6 because str6 is used a lot for LR number
str6a         dim       6
str6B         dim       6
Hold          dim       200            .Holds Detail Record
userlogn      dim           7
.END PATCH 7.7 REPLACED LOGIC
CancelFlag    dim           1              .Cancelation mode
RentalFlag    dim           1              .Change to Rental mode
AdjustFlag    dim           1              .Cancelation or change to Rental "Y" = adjust qty
AddFlag       Dim           1
ModFlag       Dim           1
MLRFLAG       dim           1              .holds mlrsw during balance adjustment
.Patch7.0
REINSTATEFLAG DIM           1
.subPatch7.0
.BEGIN PATCH 7.7 REPLACED LOGIC
.ACKEY1X       DIM           4
.ACKEY2X       DIM           4
ACKEY1X       DIM           6
ACKEY2X       DIM           6
.END PATCH 7.7 REPLACED LOGIC
mlrdispflag   form                1
Lastentry     form                5
mailerswap    dim                 1             .if yes search was entered mailer a & mailer b
.                                 however they were stored in record as Mailer b and Mailer a
.begin patch 7.9
LogoFlag  Dim       1                " " or "N" = NIN  "P" = PLI
.end patch 7.9      
DuplexFlag    dim           1
PdfFlag       Dim           1
ExclusFlag    DIM           1              Include Only Exclusive Lists
BalanceFilter dim           1              = suppress balances less than 1000
InhouseFlag   dim           1             "Y" = INHOUSE option on summary report default = "N"
UnivFlag      Form          1             "2" = Print List Universes option on summary report default = "1"
TranFlag      Form          1             "2" = Print Last Transaction date option on summary report default = "1"
InactiveFlag  dim           1              "Y" = INCLUDE inactive accounts on summary default = "N"
.BEGIN PATCH 7.7 REPLACED LOGIC
.ReportMlr     dim           4             .Holds mailer number selected for Summary report
.RMLR1         DIM           4              MAILER#1 during summary detail reads
.RMLR2         DIM           4              MAILER # 2 during summary detail reads
ReportMlr     dim           6             .Holds mailer number selected for Summary report
RMLR1         DIM           6              MAILER#1 during summary detail reads
RMLR2         DIM           6              MAILER # 2 during summary detail reads
ISViewFlag    form      "1"
.END PATCH 7.7 REPLACED LOGIC
MlrSearchFlag Form          1
ForceBegBalance Dim           1
LoopDone      Dim           1
CCField       Dim           2             .hold Century
OldQty        form          9             .holds old qty in event of change
NewDate1      DIM          10
HoldMcomp1    dim          45
HoldMcomp2    dim          45
ListViewFlag  form                1
.BEGIN PATCH 7.5 REPLACED LOGIC
.N9A           form          9
N10A          form          10
.END PATCH 7.5 REPLACED LOGIC
work9         form          9
calcuse       form          9
SaveEntry     form          5
***************************************************************************************
**DB's Flags**
.Will determine if user can exit -flag turned to "N" when user is modifying\saving detail record
ExitFlag      init          "Y"
.BEGIN PATCH 7.7 ADDED LOGIC
ExitFlag2     init          "Y"
ExitFlag3     init          "Y"
NewFlag2      init          "N"                   .For Master records on IS Screen
NewFlag3      init          "N"                   .For Detail records on IS Screen
ReturnFlag2   init          "N"                   .For Master records on IS Screen
ReturnFlag3   init          "N"                   .For Detail records on IS Screen
.END PATCH 7.7 ADDED LOGIC
.Will Not allow access to the other page or allow object to be enabled when detaiflag = "Y"
DetailFlag    init          "N"
.Exchange Record Flag
EXGOODFLAG    init          "N"
.ListSearchFlag-allows input of search in mlr\list xreference update
LSEARCHFLAG   init          "N"
.Set if Canceled out of Nord001k form
OneKCNCFLAG        init     "N"
.Check to see if loading detail/listview by lr
DETAILLRFLAG   init         "N"
**************************************************************************************
.BEGIN PATCH 7.7 REPLACED LOGIC
.DUM4           dim          4          .Used to load combobox
.DUM4a          dim          4          .Used to load combobox
DUM4           dim          6          .Used to load combobox
DUM4a          dim          6          .Used to load combobox
.END PATCH 7.7 REPLACED LOGIC
******************************************************************************************
.BEGIN PATCH 7.7 REMOVED VARIABLES - NO LONGER USED
..Not used???
.HMLR1          dim          4          .used to do compare to see if new mailers to do reload list object
.HMLR2          dim          4          .used to do compare to see if new mailers to do reload list object
.END PATCH 7.7 REMOVED VARIABLES - NO LONGER USED
.?????
HENTRY         DIM          5          .Hold Entry Number on first instance of adjustment to usage
.For DUMMY LR
dumnum         dim          6                .Used for dummy lr for new detail record
ndumnum        FORM         5                .Used for dummy lr for new detail record
.=============================
.For xrefcheck
temmlr         dim          6
temmlr2        dim          6
xrefflag       dim          2
mRSearch menu
Quesbox        integer    1,"0x000004"    .Question box to add xref,beginning balance
.=============================
***********************************************
deadflag       dim          1        "I"=inactive
HoldXlist      dim          6
.BEGIN PATCH 7.7 REPLACED LOGIC
.RECMST   FILE    FIXED=176         PICK OFF OUTPUT FILE
.begin patch 7.8
.RECMST   FILE            PICK OFF OUTPUT FILE
RECMST   FILE    FIXED=291         PICK OFF OUTPUT FILE
.end patch 7.8
.END PATCH 7.7 REPLACED LOGIC
RECNAME  DIM       30             FIELD USED IN CREATION OF OUTPUT FILE.
NEWNAME  DIM       30              SEE RECNAME
FILENUM  FORM      2              NUMERIC WORK FIELD USED IN CREATION OF OUTPUT

.holds new info
NewLR       DIM       6
NewQTY      FORM      9
NewLIST     DIM       6        49-54      LIST #
NewDAT      DIM       8        55-62      DATE CCYYMMDD
WhichMlr   Form       1        64-64      1 IF MLR1, 2 IF MLR2
NewTYPE     DIM       2        65-66      TYPIST INITALS
.begin patch 7.92
.NewXCHCOMNT DIM       30       67-96      KEYED IN COMMENTS
NewXCHCOMNT DIM       100       67-96      KEYED IN COMMENTS
.end patch 7.92
....end
.begin patch 7.4
bbdATE         FORM           5             ;USED FOR FORCED BEG BAL
BBWriteFlag     Dim            1             ;if forced beb bal set to yes after beg bal written
FUsage1        form           10
Fusage2        form           10
.end patch 7.4
blank5        dim           5
Dim11    dim        11
Dim13a   dim        13
Dim13b   dim        13
total1   form       10
total2   form       10
mask13   init       "Z,ZZZ,ZZZ,ZZ9"
mask11   init       "ZZZ,ZZZ,ZZ9"
.mask10   init      "ZZ,ZZZ,ZZ9"
TotIndx  form       5                     .total number of rows in list view object
NewSearch dim       1
.Flag for Names owed never been used
NUsedflag dim       1
.Patch7.21
.Flag for Including Revision Date in summary report
REVVFLAG  DIM       1
.Patch7.21
....MISC. ITEMS....................................
colorfile file
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
.
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.
.Set Up Menu Bar
mFile    menu
.mEdit    menu
mHelp    menu
.Set Up SubMenu for Options
mOptions Menu
sColor   submenu
sSearch  submenu
sPrint   submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
.Patch 6.8
.Patch 7.74
.ISLIST    init      "DHERRIC,JDUENAS,DMONTOY,AHARKIN,CREQUES,DBACA"
.ISLIST    init      "DHERRIC,JDUENAS,CREQUES"
.Patch 7.91
.ISLIST    init      "DHERRIC,JDUENAS,CREQUES,JLACOMB"
ISLIST    init      "DHERRIC,RWHITIN,CREQUES"
.Patch 7.91
.Patch 7.74
OData   init    "&Options;&Search-F2;-;&Color;-;&Administrator Mode;"
.OData   init    "&Options;&Search-F2;-;&Color;"
.subpatch6.8
HData   init    "&Help;&About;&Tutor"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
RData   init    "&Print;&Summary;&Detail"
.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
.
endindex form      9
.*******************
.Timers for DialogWait Message Box
timer1 timer
.timer2 timer
.*******************
Timer   Timer
holdsInfo dim      36
.For Use in inactivating and activating between clients
.Define Collections for Object Colors
ColText Collection
ColBack Collection


.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1

.report stuff
RptCan  dim     1
EditTextBoxes   EditText (4)
Buttons         Button  (3)
.CheckBoxes      CheckBox (6)
.;Patch7.21
.begin patch 7.94
CheckBoxes      CheckBox (11)
.begin patch 7.8
.CheckBoxes      CheckBox (10)
.end patch 7.94
.CheckBoxes      CheckBox (9)
.end patch 7.8
.Patch7.21
.CheckBoxes      CheckBox (8)
ComboBoxes      ComboBox (4)
StatTextBoxes   StatText (7)
ListViews       ListView (2)


.................................
ObjectColl      Collection
.coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590
Tabnum  form          2
SaveTab form          2
.............................
.Constants for Animate Icon
.These coordinates will place Icon directly
AniH    init    "250"
AniV    init    "550"
+
.............................................................................................................
.Collection for Add/Modify Routine
ColButt collection

.Set Vars used for About Box
        move    "NXCH0001.PLS",Wprognme
        move    "Exchange Information",Wfunction
        move    "David Herrick",Wauthor
        move     Release,Wrelease
.        move    "May 13, 2002",Wreldate
          Move      Reldate to Wreldate
.
.Declare forms, Always declare child forms first
.rpt     plform  Report
Nord001k plform Nord001k        .OrderXBal
animicon plform Animate         .CONTAINS ALL THE ICONS
SRCH    plform  Search
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
rpt2    plform  Report2
dlgbx   plform  dialogwait
ExchA   plform  NXCH0001A                    .blue card
ExchB   plform  NXCH0001B                    .detail
ExchC   plform  NXCH0001C                    .History
x       plform  NXCH0001
.BEGIN PATCH 7.7 ADDED LOGIC
ExchD     plform    NXCH0001D
ExchD1    plform    NXCH001D1
ExchD2    plform    NXCH001D2
.END PATCH 7.7 ADDED LOGIC

.ExchA   plform  NXCH0001A                    .blue card
.ExchB   plform  NXCH0001B                    .detail
.ExchC   plform  NXCH0001C                    .History
.x       plform  NXCH0001
        winhide
************************************************
.Collection for Add/Modify Routine
.ColButt collection
          listins ColButt,NXCH0001bButtonADD,NXCH0001bButtonMOD,NXCH0001bButtonCNC:
                    NXCH0001bButtonRENT,NXCH0001bButtonPrev,NXCH0001bButtonNext,NXCH0001BReinstate,NxchEditSearchMlr1:
                    NxchEditSearchMlr2
************************************************
.Load Forms, Always load parent form first
          formload x
          formload EXCHa,NXCH0001
          formload EXCHb,NXCh0001
          formload EXCHC,NXCh0001
.BEGIN PATCH 7.7 ADDED LOGIC
          formload ExchD,NXCh0001
          formload ExchD1,NXCh0001
          formload ExchD2,NXCh0001
.END PATCH 7.7 ADDED LOGIC
          formload Nord001k,NXCH0001 BegBalance
          formload abt
          FORMLOAD rpt2
          FORMLOAD pss
          formload mss1
          formload SRCH
          formload animicon
          formload dlgbx
.Set tab index
          move    C2,TabNum
..............................................................
.===========================================================
.For DialogWait Message Box
.        create timer1,50
.        create timer2,100
.===========================================================
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        CREATE  NXCH0001;MFile,FData
        create  NXCH0001;mOptions,OData,mFile
        create  NXCH0001;mHelp,HData,mOptions
        CREATE  NXCH0001;sCOlor,Cdata,mOptions,1
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions,OptionsGo,result
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
.Create SubMenu
        create  NXCH0001;sSearch,SData,mOptions,1
        create  NXCH0001;sPrint,RData,mFile,1
.Activate SubMenus
        activate sSearch,SearchGo,result
        activate sPrint,PrintGo,result
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
.Create Colors for EditText Inquiry
        create  white=*white
.BEGIN PATCH 7.7 REPLACED LOGIC
.        create  grey=*ltgray
        create      grey=220:220:220
.END PATCH 7.7 REPLACED LOGIC
        create  RED=*RED
        create  black=*black

.Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\program files\nincal\NXCH0001.col"
        goto    colorerror if over
        clear   n1
        loop
                add     c1,n1
                read    colorfile,seq;colornum(n1)
                until   over
                until   (n1 = 2)
        repeat
        close   colorfile
        trapclr io
        unpack  colornum(1),Fred,Fgreen,Fblue
        create  FTC=Fred:Fgreen:Fblue
        setprop ColText,fgcolor=FTC

        unpack  colornum(2),Fred,Fgreen,Fblue
        create  BGC=Fred:Fgreen:Fblue
        setprop ColBack,bgcolor=BGC
aftercolor
        move    "N",PassFlag

         move      c3 to ncntpath
.        call      findport
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
         if        over
                   move      c2 to cntprint
         endif
.BEGIN PATCH 7.7 ADDED LOGIC
          move      CNTNAME,str1
          scan      B1,CNTNAME
          bump      CNTNAME
          move      CNTNAME,str6
          pack      str7,str1,str6
          call      Trim using str7
          uppercase str7
          reset     ISLIST
          move      str7,userlogn
          scan      str7,ISLIST
          if equal
                    setprop   NXCH001aTabControl001,TABLABEL="&Blue Card;&Detail;I.S."
.                   getprop   NXCH0001dGroupBox001,bgcolor=BGC
.                   setprop   Nxch0001dMRadioNoDetail,bgcolor=BGC
.                   setprop   Nxch0001dMRadioNoMailer,bgcolor=BGC
.                   setprop   Nxch0001dMRadioEntries,bgcolor=BGC
.                   setprop   Nxch0001dDRadioNoMaster,bgcolor=BGC
.                   setprop   Nxch0001dDRadioNoMailer,bgcolor=BGC
.                   setprop   NXCH0001dDRadioNoLr,bgcolor=BGC
.                   setprop   NXCH0001dRadioReport,bgcolor=BGC
.                   setprop   NXCH0001dRadioLV,bgcolor=BGC
.                   setprop   NXCH0001dStatText001,bgcolor=BGC
.                   setprop   NXCH0001dStatText002,bgcolor=BGC
.                   setprop   NXCH0001dStatText003,bgcolor=BGC
.Default values
                    setitem   Nxch001d2MRadioNoDetail,0,1
                    setitem   NXCH001d2RadioReport,0,1
          endif
          reset     ISLIST
          reset     CNTNAME
.
          call      ExchDisableScreenDMasterLower
          call      ExchDisableScreenDDetailLower
.
          NXCH1dDListView.InsertColumn using "Entry",45,0
          NXCH1dDListView.InsertColumn using "Mlr 1",50,1
          NXCH1dDListView.InsertColumn using "Mlr 2",50,2
          NXCH1dDListView.InsertColumn using "LR",50,3
          NXCH1dDListView.InsertColumn using "Usage 1",65,4
          NXCH1dDListView.InsertColumn using "Usage 2",65,5
          NXCH1dDListView.InsertColumn using "Detail",0,6
          NXCH1dDListView.InsertColumnFgClr using *Index=7
          NXCH1dDListView.SetColumnFormat using 4,1
          NXCH1dDListView.SetColumnFormat using 5,1
.Load OrderInfo
          call      NXCHLoadForm
          clock     timestamp,timestamp
.END PATCH 7.7 ADDED LOGIC
..........................................................................................
.Create NXCH0001aListView001 Columns
.Column Clicking
.BEGIN PATCH 7.3 REPLACED LOGIC
.        NXCH0001aListView001.InsertColumn using "Orderdate",75,1
.        NXCH0001aListView001.InsertColumn using "LR",50,2
.        NXCH0001aListView001.InsertColumn using "Quantity",80,3
.        NXCH0001aListView001.InsertColumn using "Balance",95,4
.        NXCH0001aListView001.InsertColumn using "Record Key",30,5
.        NXCH0001aListView001.SetColumnFormat using 2,1              .set qty column justify right
.        NXCH0001aListView001.SetColumnFormat using 3,1              .set bal column justify right
....................
          NXCH0001aListView001.InsertColumn using "Key",0,1
          NXCH0001aListView001.InsertColumn using "Orderdate",75,2
          NXCH0001aListView001.InsertColumn using "LR",50,3
          NXCH0001aListView001.InsertColumn using "Quantity",80,4
          NXCH0001aListView001.InsertColumn using "Balance",95,5
          NXCH0001aListView001.InsertColumn using "Record Key",30,6
        NXCH0001aListView001.SetColumnFormat using 3,1              .set qty column justify right
        NXCH0001aListView001.SetColumnFormat using 4,1              .set bal column justify right
.         setprop NXCH0001AListView001,SortOrder=3
.END PATCH 7.3 REPLACED LOGIC
.
.Create NXCH0001aListView002 Columns
.Column Clicking
.BEGIN PATCH 7.3 REPLACED LOGIC
.        NXCH0001aListView002.InsertColumn using "Orderdate",75,1
.        NXCH0001aListView002.InsertColumn using "LR",50,2
.        NXCH0001aListView002.InsertColumn using "Quantity",80,3
.        NXCH0001aListView002.InsertColumn using "Balance",95,4
.        NXCH0001aListView002.InsertColumn using "Record Key",30,5
.        NXCH0001aListView002.SetColumnFormat using 2,1              .set qty column justify right
.        NXCH0001aListView002.SetColumnFormat using 3,1              .set bal column justify right
....................
          NXCH0001aListView002.InsertColumn using "Key",0,1
          NXCH0001aListView002.InsertColumn using "Orderdate",75,2
          NXCH0001aListView002.InsertColumn using "LR",50,3
          NXCH0001aListView002.InsertColumn using "Quantity",80,4
          NXCH0001aListView002.InsertColumn using "Balance",95,5
          NXCH0001aListView002.InsertColumn using "Record Key",30,6
        NXCH0001aListView002.SetColumnFormat using 3,1              .set qty column justify right
        NXCH0001aListView002.SetColumnFormat using 4,1              .set bal column justify right
.         setprop NXCH0001AListView002,SortOrder=3
.END PATCH 7.3 REPLACED LOGIC
..........................................................................................
*ONLINE REPORT SCREENS
..........................................................................................
.Create NXCH0001CListView001 Columns by Mailer
.Column Clicking
        NXCH0001CListView001.InsertColumn using "Mailer",225,1
        NXCH0001CListView001.InsertColumn using "Owes",75,2
        NXCH0001CListView001.InsertColumn using "Is Owed",75,3
        NXCH0001CListView001.InsertColumn using "Universe",75,4
        NXCH0001CListView001.InsertColumn using "Last Use",70,5
        NXCH0001CListView001.InsertColumn using "List ##",60,6
.Patch6.8
        NXCH0001CListView001.InsertColumn using "Mailer ##",0,7
        NXCH0001CListView001.InsertColumn using "Owes",0,8
        NXCH0001CListView001.InsertColumn using "Is Owed",0,9
        NXCH0001CListView001.InsertColumn using "Universe",0,10
.EndPatch6.8
        NXCH0001CListView001.SetColumnFormat using 1,1              .set qty column justify right
        NXCH0001CListView001.SetColumnFormat using 2,1              .set qty column justify right
        NXCH0001CListView001.SetColumnFormat using 3,1              .set Unv column justify right
.
.Create NXCH0001CListView002 Columns by Owes
.Column Clicking
.        NXCH0001CListView002.InsertColumn using "Owes",0,1
.        NXCH0001CListView002.InsertColumn using "Mailer",225,2
.        NXCH0001CListView002.InsertColumn using "Owes",75,3
.        NXCH0001CListView002.InsertColumn using "Is Owed",75,4
.        NXCH0001CListView002.InsertColumn using "Universe",75,5
.        NXCH0001CListView002.InsertColumn using "Last Use",70,6
.        NXCH0001CListView002.InsertColumn using "List ##",60,7
.        NXCH0001CListView002.SetColumnFormat using 2,1              .set qty column justify right
.        NXCH0001CListView002.SetColumnFormat using 3,1              .set qty column justify right
.        NXCH0001CListView002.SetColumnFormat using 4,1              .set Unv column justify right
.Create NXCH0001CListView003 Columns by Owed
.Column Clicking
.        NXCH0001CListView003.InsertColumn using "Is Owed",0,1
.        NXCH0001CListView003.InsertColumn using "Mailer",225,2
.        NXCH0001CListView003.InsertColumn using "Owes",75,3
.        NXCH0001CListView003.InsertColumn using "Is Owed",75,4
.        NXCH0001CListView003.InsertColumn using "Universe",75,5
.        NXCH0001CListView003.InsertColumn using "Last Use",70,6
.        NXCH0001CListView003.InsertColumn using "List ##",60,7
.        NXCH0001CListView003.SetColumnFormat using 2,1              .set Unv column justify right
.        NXCH0001CListView003.SetColumnFormat using 3,1              .set qty column justify right
.        NXCH0001CListView003.SetColumnFormat using 4,1              .set qty column justify right
.........................................................................................
*End of Online report
.Create NXCH0001BCListView001B
..        NXCH0001BListView001A.InsertColumn using "List ##",60,1
..        NXCH0001BListView001A.InsertColumn using "List",125,2
..        NXCH0001BListView001A.SetColumnFormat using 1,0              .set column justify left
..        NXCH0001BListView001A.SetColumnFormat using 2,0              .set column justify left
.        NXCH0001BListView001B.GetExtendedStyle giving c1

.........................................................................................
.Create NXCH0001BCListView001B
        NXCH0001BListView001B.InsertColumn using "List ##",60,1
        NXCH0001BListView001B.InsertColumn using "List",125,2
        NXCH0001BListView001B.SetColumnFormat using 1,0              .set column justify left
        NXCH0001BListView001B.SetColumnFormat using 2,0              .set column justify left
.        NXCH0001BListView001B.GetExtendedStyle giving c1
.........................................................................................
.........................................................................................
        setprop  NXCH0001BStatText012,Height=0
        setprop  NXCH0001BCheck001,Height=0
        setprop  NXCH0001BButtonPrev,Icon="UpArrow.ico"
        setprop  NXCH0001BButtonNext,Icon="DnArrow.ico"
..........................................................................................
.Set Error Message Stat Text Boxes
        call    SetNXCHErrorMssgDefault
. .................................................................
        Deactivate EXCHa
        Deactivate EXCHb
        Deactivate EXCHC
MAIN
**********************************************************
.Goes to detailscreen
.BEGIN PATCH 7.7 REPLACED LOGIC
.        move    C2,n1
.        call    NXCHTABCLICK
.        move    C1,n1
.        call    NXCHTABCHANGE
          call      NXCHSwitchTab using C1
.END PATCH 7.7 REPLACED LOGIC
**********************************************************
.                   call    NXCHSetFocusTab
..        move        c1 to n1
.        move        c1 to tabnum
..        call        NXCHTabChange
          setfocus NxchEditSearchMlr1
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout

.......................................................................................................
*********************************************************************
*********************************************************************
.New code under lost focus event of edit text Box for Mailer1
.BEGIN PATCH 7.7 REPLACED LOGIC
.Mailer1
.         getitem   NxchEditSearchMlr1,0,str10
.         if (str10="")
.                   return
.         endif
.         count     n2,str10
.         if (n2 <= c4)
.                move str10 to str4
.                call zfillit using str4
..                rep  zfill to str4
.                pack mkey with str4,z3
.                call nmlrkey
.                if over
.                    alert caution,"Mailer Does not Exist!",result,"Invalid Mailer"
.         setitem   NxchEditSearchMlr1,0,""
.         setitem   NxchStatSearchMlr1Name,0,""
.         setfocus  NxchEditSearchMlr1
.                else
.         setitem   NxchEditSearchMlr1,0,mnum
.         setitem   NxchStatSearchMlr1Name,0,Mcomp
.;.                   if        (mnum <> mlr1)
.;.        move       yes to newsearch
.;.                   endif
.         move      mnum to mlr1
.         move      mcomp to HoldMcomp1
.                endif
.         else
.                   move      str10 to str6
.                   call      zfillit using str6
.;.                rep  zfill to str6
.                   setitem   NxchEditSearchMlr1,0,str6
.         endif
.         move      c1 to MlrSearchFlag
.         move      c1 to  MLRDISPFLAG
.;.Need to set search flag
.         setprop NXCH0001ButtonGO,default=c1
.......................................
ExchangeDisplayMailer Routine EditPtr,StatPtr
.EditPtr  = TextBox which holds Company Number
.StatPtr  = Matching Static TextBox which holds Mailer Name
          clear     COMPCOMP
          clear     COMPNUM
          getitem   EditPtr,0,str6
          if (str6="")
                    return
          endif
          call      ZFillIt using str6
          count     N2,str6
          if (N2 <= C6)
                    pack      COMPFLD,str6
                    move      "Disp.Mailer-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if not over
                              if (CompMlrFlg = "T")
                                        setitem   EditPtr,0,COMPNUM
                                        setitem   StatPtr,0,COMPCOMP
                              else
                                        clear     COMPCOMP
                                        clear     COMPNUM
                              endif
                    endif
          endif
.END PATCH 7.7 REPLACED LOGIC
          return

Start
.First Code Done after go button hit
.BEGIN PATCH 7.7 REPLACED LOGIC
.         setprop   NxchEditSearchMlr1,enabled=c0
.         setprop NxchEditSearchMlr2,enabled=c0
.         getitem NxchEditSearchMlr1,0,str10
.         if (str10="")
.                   alert     caution,"Cannot have a null entry in this field!",result,"Invalid Mailer"
.                   setprop   NxchEditSearchMlr1,enabled=c1
.                   setprop   NxchEditSearchMlr2,enabled=c1
.                   setfocus NxchEditSearchMlr1
.                   return
.         endif
.         count     n2,str10
.;.         count str10,n2
.         if (n2 <= c4)
.         getitem   NxchEditSearchMlr2,0,str4
.         if (str4="")
.                   move     str10 to str4
..........................................................
          setitem   NxchStatCount,0,""
          setitem   NxchStatMlrLeft,0,""
          setitem   NxchStatMlrRight,0,""
.
          setprop   NxchEditSearchMlr1,enabled=c0,bgcolor=grey
          setprop NxchEditSearchMlr2,enabled=c0,bgcolor=grey
.
          getitem NxchEditSearchLR,0,str6
          call      Trim using str6
          if (str6 = "")
.Only do Mlr/Mlr search if LR field is NOT supplied!!
                    getitem NxchEditSearchMlr1,0,str6
                    call      Trim using str6
                    getitem NxchEditSearchMlr2,0,str6B
                    call      Trim using str6B
                    if (str6 = "" & str6B = "")
                              alert     caution,"You must supply an LR number OR at least 1 Mailer Number!",result
                              setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                              setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
                              setfocus NxchEditSearchMlr1
                              return
                    endif
                    count     n2,str6
                    if (n2 <= c6)
                              move      str6,mlr1
                              packkey   COMPFLD,str6
                              move      "start-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if not over
                                        move      COMPCOMP,HoldMcomp1
                              else
                                        clear     HoldMcomp1
                              endif
                              if (str6B = "")
.END PATCH 7.7 REPLACED LOGIC
..=======================================================================================
..                        if       (n2 = c0 & MlrDispFlag <> 1)
..IF NO 2ND MAILER LETS ADD CODE TO DO AN ON SCREEN EXCHANGE history REPORT.
...Dynamically reset Animate as same size as NMDL001A
                                        getprop   NXCH0001,height=H
                                        setprop   Animate,height=H
                                        getprop   NXCH0001,width=V
                                        setprop   Animate,width=V
.Must clear Resize Event which was tiggered when Animate was resized,
.as this is where the AnimateIt subroutine sits.
                                        clearevent
                                        moveaddr NXCH0001,AnimateWindow
                                        move      C1,AnimateCurIcon
                                        move      C4,AnimateFrames
                                        move      C0,AnimateIconID
                                        move      "410",H
                                        move      "540",V
                                        Deactivate ExchA
                                        Deactivate ExchB
.BEGIN PATCH 7.7 ADDED LOGIC
                                        Deactivate ExchD
                                        Deactivate ExchD1
                                        Deactivate ExchD2
.END PATCH 7.7 ADDED LOGIC
                                        setprop   NXCH0001AListView001,visible=0
                                        setprop   NXCH0001AListView002,visible=0
                                        setprop   NxchStatSearchMlr2,visible=0
                                        Setprop   NxchEditSearchMlr2,visible=0
                                        setprop   NxchStatSearchMlr2Name,visible=0
..-                             Setprop   NXCH0001CListView003,enabled=1,bgcolor=white
..-                       setprop       NXCH0001CListView002,enabled=1,bgcolor=white
                                        setprop   NXCH0001CListView001,enabled=1,bgcolor=white
                                        setprop   NXCH0001CListView003,visible=0
                                        setprop   NXCH0001CListView002,visible=0
                                        setprop   NXCH0001CListView001,visible=1
                                        setfocus NXCH0001CListView001
Load1CAgain
.Patch6.8
                                        NXCH0001CListView001.DeleteAllItems giving N9
.Patch6.8
                                        call      NXCHClearListView
                                        MOVE      C1 TO NXCHPATH
                                        move      c0 to total1
                                        move      c0 to total2
                                        Activate ExchC
.Patch6.8
                                        Nxch0001CListView001.EnsureVisible using c1,0
                                        setprop   NXCH0001CInactivate,visible=c0
                                        setprop   NXCH0001CInactivate,enabled=c0
.subpatch6.8
                                        clear     nxngfld1
                                        clear     nxngfld2
                                        packkey   nxngfld1 from "01X",mlr1
                                        call      NXNGAIM
                                        if over
                                                  goto XNGPAssTwo
                                        else
                                                  cmatch    "I" to flag              .inactive?
                                                  goto xloop1 if equal
                                                  goto xloop1a
                                        endif
Xloop1
.BEGIN PATCH 7.71 REPLACED LOGIC
                          cmatch yes to inactiveflag
                                        goto   xloop1a if equal
.END PATCH 7.71 REPLACED LOGIC
                                        call      nxngkg
                                        goto XNGPASSTWO if over
                                        cmatch    "I" to flag              .inactive?
                                        goto xloop1 if equal
xloop1a
                                        clear     nxchfld1
                                        packkey   nxchfld1 from acckey,entry
                                        rep       zfill in nxchfld1
                                        call      nxchkey
                                        goto xloop1 if over                 .............bad
..build items for list views
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   unpack         acckey into str4,str4
.                   clear          mkey
.                   packkey        mkey from str4,z3
.                   Call           nmlrkey
.                   if             over
.                             move           "Mailer Desc not Found" to mcomp
                                        unpack    acckey,str6B,str6B
                                        packkey   COMPFLD,str6B
                                        move      "xloop1a-COMPKEY",Location
                                        pack      KeyLocation,"Key: ",COMPFLD
                                        call      COMPKEY
                                        if over
                                                  move      "Mailer Desc not Found",COMPCOMP
.END PATCH 7.7 REPLACED LOGIC
                                        endif
                                        if (usage1 = usage2)
                                                  move      b1 to str10            owes
                                                  move      b1 to str10a             owed
                                        endif
                                        if (usage1 > usage2)
                                                  sub       usage2 from usage1
                                                  move      usage1 to str10
                                                  move      b1 to str10a
                                                  add       usage1 to total1
                                        else
                                                  sub       usage1 from usage2
                                                  move      usage2 to str10a
                                                  move      b1 to str10
                                                  Add       usage2 to total2
                                        endif
                                        call      LoadNxch0001cListView
                                        goto xloop1
.
XNgpassTwo
                                        clear     nxngfld1
                                        clear     nxngfld2
                                        packkey   nxngfld2 from "02X",mlr1
                                        call      nxngaim
                                        goto XloopExit if over
                                        cmatch    "I" to flag              .inactive?
                                        goto xloop2 if equal
                                        goto xloop2a
Xloop2
.BEGIN PATCH 7.71 REPLACED LOGIC
                                        cmatch yes to inactiveflag
                                        goto xloop2a if equal
.End PATCH 7.71 REPLACED LOGIC
                                        call      nxngkg
                                        goto XloopExit if over
                                        cmatch    "I" to flag              .inactive?
                                        goto xloop2 if equal
xloop2a
                                        clear     nxchfld1
                                        packkey   nxchfld1 from acckey,entry
                                        rep       zfill in nxchfld1
                                        call      nxchkey
                                        goto xloop2 if over                 .............bad
..build items for list views
.BEGIN PATCH 7.7 REPLACED LOGIC
.                  unpack         acckey into str4
.                  clear          mkey
.                  packkey        mkey from str4,z3
.                  call           nmlrkey
.                  if             over
.                        move           "Mailer Desc not Found" to mcomp
                                        unpack    acckey,str6B
                                        packkey   COMPFLD,str6B
                                        move      "xloop2a-COMPKEY",Location
                                        pack      KeyLocation,"Key: ",COMPFLD
                                        call      COMPKEY
                                        if over
                                                  move      "Mailer Desc not Found",COMPCOMP
.END PATCH 7.7 REPLACED LOGIC
                                        endif
                                        if (usage1 = usage2)
                                                  move      b1 to str10            owes
                                                  move      b1 to str10a             owed
                                        endif
                                        if (usage1 < usage2)
                                                  sub       usage1 from usage2
                                                  move      usage2 to str10
                                                  move      b1 to str10a
                                                  add       usage2 to total1
                                        else
                                                  sub       usage2 from usage1
                                                  move      usage1 to str10a
                                                  move      b1 to str10
                                                  add       usage1 to total2
                                        endif
                                        call      LoadNxch0001cListView
                                        goto xloop2
                              else
*******************************************************************************************
.if second mailer field has something in it try to find exchange info
.BEGIN PATCH 7.7 REPLACED LOGIC
.       call zfillit using str4
.;.                                         rep zfill in str4
.                   pack mkey with str4,z3
.                   call nmlrkey
.                   if over
.                        alert caution,"Mailer Does not Exist!",result,"Invalid Mailer"
.                        setitem   NxchEditSearchMlr2,0,""
.                        setitem   NxchStatSearchMlr2Name,0,""
.                        setprop   NxchEditSearchMlr1,enabled=c1
.                        setprop   NxchEditSearchMlr2,enabled=c1
.                                   setfocus  NxchEditSearchMlr2
.                  else
.                        setitem   NxchEditSearchMlr2,0,mnum
.                                   setitem   NxchStatSearchMlr2Name,0,Mcomp
.;.                                     if        (mnum <> mlr2)
.;.                                                 move       yes to newsearch
.;.                                 endif
.                        move      mnum  to mlr2
.                        move      mcomp to HoldMcomp2
......................................
                                        call      zfillit using str6B
                                        pack      COMPFLD,str6B
                                        move      "xloop1a,ELSE-COMPKEY",Location
                                        pack      KeyLocation,"Key: ",COMPFLD
                                        call      COMPKEY
                                        if over
                                                  alert     caution,"Mailer Does not Exist!",result,"Invalid Mailer"
                                                  setitem   NxchEditSearchMlr2,0,""
                                                  setitem   NxchStatSearchMlr2Name,0,""
                                                  setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                                                  setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
                                                  setfocus NxchEditSearchMlr2
                                        else
                                                  setitem   NxchEditSearchMlr2,0,COMPNUM
                                                  setitem   NxchStatSearchMlr2Name,0,COMPCOMP
                                                  move      COMPNUM,mlr2
                                                  move      COMPCOMP,HoldMcomp2
.END PATCH 7.7 REPLACED LOGIC
                                                  call    NXCHClearListView
                                                  call    NXCHLoadListView
                                        endif
                                        setitem   NXCH001aTabControl001,0,c1
                                        return
                              endif
                    endif
.=========================================================================================================
xloopExit
.doing galley no return
                    setprop   Nxch0001CListView001,visible=1
                    setprop   Nxch0001CListView003,visible=0
                    setprop   Nxch0001CListView002,visible=0
                    Nxch0001CListView001.EnsureVisible using c1,0
                    setfocus Nxch0001Clistview001
                    clear     dim13a
                    move      mask13 to dim13a
                    edit      total1 to dim13a
                    setitem   NXCH0001StatText003,0,"<- Owes"
                    setitem   NXCH001Total1,0,dim13a
                    setitem   NXCH0001StatText005,0,"Is owed ->"
                    clear     dim13a
                    move      mask13 to dim13a
                    edit      total2 to dim13a
                    setitem   NXCH001Total2,0,dim13a
                    destroy AnimateIcon
.===========================================================================================
.Move to no so it won't display first nxch001alistview
.See if there are any records in listview object
                    NXCH0001CListView001.GetItemCount GIVING n4
                    if (n4 = c0)
                              alert     caution,"No Exchange Records Found!!",result,"No Records"
                    endif
.BEGIN PATCH 7.7 ADDED LOGIC
                    move      N4,str4
                    call      FormatNumeric using str4,str5
                    if (N4 = 1)
                              pack      str25,str5," Record Found."
                    else
                              pack      str25,str5," Records Found."
                    endif
                    setitem   NxchStatCount,0,str25
                    setitem   NxchStatMlrLeft,0,HoldMComp1
                    setitem   NxchStatMlrRight,0,HoldMComp2
.END PATCH 7.7 ADDED LOGIC
                    move      NO to NEWSEARCH
                    call      ClearDetail
                    move      NO to EXGOODFLAG
                    setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                    setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
..===========================================================================================
..Probably going to comment out-adding dupicate record to display
..need to put return here
..We'll tryit
                    return
..                          noreturn
LoadNxch0001cListView
                    call      GetListUniverse
                    unpack    dat into str2,yy,mm,dd
                    pack      datestring from mm,slash,dd,slash,str2,yy
..view 1
..               NXCH0001CListView001.GetItemCount GIVING totIndx
..         NXCH0001CListView001.InsertItem giving N9 using mcomp, *index=totIndx
.BEGIN PATCH 7.7 REPLACED LOGIC
.                        NXCH0001CListView001.InsertItem giving N9 using mcomp
                    NXCH0001CListView001.InsertItem giving N9 using COMPCOMP
.END PATCH 7.7 REPLACED LOGIC
                    clear     str13
                    move      mask13 to str13
                    move      c0 to n10
                    move      str10 to n10
                    edit      n10 to str13
                    NXCH0001CListView001.SetItemText using N9,str13,1
                    clear     str13
                    move      mask13 to str13
                    move      c0 to n10
                    move      str10a to n10
                    Edit      n10 to str13
                    NXCH0001CListView001.SetItemText using N9,str13,2
                    clear     str13
                    move      mask13 to str13
                    move      c0 to n10
                    move      Universe to n10
                    edit      n10 to str13
                    NXCH0001CListView001.SetItemText using N9,str13,3
                    NXCH0001CListView001.SetItemText using N9,Datestring,4
                    NXCH0001CListView001.SetItemText using N9,nxrflist,5
..Patch6.8
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   NXCH0001CListView001.SetItemText using N9,mnum,6
                    NXCH0001CListView001.SetItemText using N9,COMPNUM,6
.END PATCH 7.7 REPLACED LOGIC
                    move      "0000000000" to indexstring
                    move      str10 to indexstring
                    setlptr   indexstring,10
                    rep       zfill in indexstring
                    NXCH0001CListView001.SetItemText using N9,indexstring,7
                    move      "0000000000" to indexstring
                    move      str10a to indexstring
                    setlptr   indexstring,10
                    rep       zfill in indexstring
                    NXCH0001CListView001.SetItemText using N9,indexstring,8
                    move      "0000000000" to indexstring
                    move      universe to indexstring
                    setlptr   indexstring,10
                    rep       zfill in indexstring
                    NXCH0001CListView001.SetItemText using N9,indexstring,9
..End Patch6.8
..view 2
..USE INDEXSTRING
..-                       move  "0000000000" to indexstring
..-                     move  str10 to indexstring
..-                       setlptr indexstring,10
..-                       rep   zfill in indexstring
..-                       NXCH0001CListView002.InsertItem giving N9 using indexstring
..-                       NXCH0001CListView002.SetItemText using N9,mcomp,1
..-                     clear  str13
..-                         move   mask13 to str13
..-                 move   c0 to n10
..-                     move   str10 to n10
..-                         edit   n10 to str13
..-                       NXCH0001CListView002.SetItemText using N9,str13,2
..-                 clear  str13
..-                     move   mask13 to str13
..-                         move   c0 to n10
..-                 move   str10a to n10
..-                     edit   n10 to str13
..-                       NXCH0001CListView002.SetItemText using N9,str13,3
..-                 clear  str13
..-                 move   mask13 to str13
..-                     move   c0 to n10
..-                         move   Universe to n10
..-                 edit   n10 to str13
..-                       NXCH0001CListView002.SetItemText using N9,str13,4
..-                    NXCH0001CListView002.SetItemText using N9,Datestring,5
..-                       NXCH0001CListView002.SetItemText using N9,nxrflist,6

..view 3
..-.               NXCH0001CListView003.GetItemCount GIVING totIndx
..USE INDEXSTRING
..-                     move  "0000000000" to indexstring
..-                       move  str10a to indexstring
..-                       setlptr indexstring,10
..-                     rep   zfill in indexstring
..-                       NXCH0001CListView003.InsertItem giving N9 using indexstring
..-                       NXCH0001CListView003.SetItemText using N9,mcomp,1
..-                       clear  str13
..-                     move   mask13 to str13
..-                      move   c0 to n10
..-                     move   str10 to n10
..-                       edit   n10 to str13
..-                       NXCH0001CListView003.SetItemText using N9,str13,2
..-                       clear  str13
..-                     move   mask13 to str13
..-                       move   c0 to n10
..-                      move   str10a to n10
..-                     edit   n10 to str13
..-                       NXCH0001CListView003.SetItemText using N9,str13,3
..-                       clear  str13
..-                     move   mask13 to str13
..-                     move   c0 to n10
..-                     move   Universe to n10
..-                     edit   n10 to str13
..-                       NXCH0001CListView003.SetItemText using N9,str13,4
..-                       NXCH0001CListView003.SetItemText using N9,Datestring,5
..-                       NXCH0001CListView003.SetItemText using N9,nxrflist,6
..-                       setprop      NXCH0001ButtonGO,visible=0
***********************************************
..Move to no so it won't display first nxch001alistview
..Try moving it up a little
...                     move      NO to NEWSEARCH
...                     call      ClearDetail
...                       move      NO to EXGOODFLAG
.***********************************************
                    return
..=========================================================================================================
..                          alert    caution,"Cannot have a null entry in this field!",result,"Invalid Mailer"
..                          setprop  NxchEditSearchMlr1,enabled=c1
..                            setprop  NxchEditSearchMlr2,enabled=c1
..                  setfocus NxchEditSearchMlr1
..                  return

..=========================================================================================
          else
..Continuation of if --if greater than 4 numbers must be lr
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   move      str10 to str6
.END PATCH 7.7 REPLACED LOGIC
                    call      zfillit using str6
..                 rep  zfill in str6
                    Goto MAYBELR
          endif
          return
...........................................................................................................
..build lr key
MaybeLR
..check to see if lr is all zeros
.BEGIN PATCH 7.7 REPLACED LOGIC
.         move      str6 to n6
.         if (N6 = c0)
.                   alert     caution,"LR cannot be zero!!!",result,"Invalid LR"
.                   return
.         endif
.         count     n2 in str10
.         if (n2 < c5)
...............................
          count     n2 in str6
          if (n2 < c6)
.END PATCH 7.7 REPLACED LOGIC
                    setprop   ErrorMssgStat1,visible=1
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=1
                    setprop   ErrorMssgStat4,visible=1
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"Or 6 Digit LR Number:"
                    setprop   ErrorMssg,visible=1
..set errormessg
...        call          SetNXCHErrorMssgDefault
                    setfocus NxchEditSearchMlr1
.BEGIN PATCH 7.7 ADDED LOGIC
                    setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                    setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
.END PATCH 7.7 ADDED LOGIC
                    return
          endif
          goto BUILDLR
................................................................................
BUILDLR
.patch7.2
          clear     lr
          packkey   lr from str6
          MOVE      LR TO NXCHFLD2
          MOVE      C2 TO NXCHPATH
.                call        NXCHLoadDetail
          CALL      NXCHKEY
          if over
..error dialog box
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"Record not found for that LR!"
                    setprop   ErrorMssg,visible=1
.BEGIN PATCH 7.7 ADDED LOGIC
                    setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                    setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
.END PATCH 7.7 ADDED LOGIC
                    setfocus NxchEditSearchMlr1
                    return
          endif
.subpatch7.2
          if (LR <> NXCHFLD2)
..                        alert caution,"Record does not exist!!",result,"Invalid Read"
                    call      clearmailers
                    call      ClearDetail
                    call      NXCHClearListView
                    setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                    setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
                    setfocus NxchEditSearchMlr1
                    return
          endif
..+++++++++++++++++++++++====
..Commented out to try to load detail when typing in LR
...         move        c2 to tabnum               .going to
...         move        c1 to n1                   .currently on
...         call        NXCHTabChange
..+++++++++++++++++++++++====
..         Deactivate  EXCHa
..         activate    EXCHb
..now pull info from record and build everything else
          unpack    exkey into mlr1,mlr2
          setitem   NxchEditSearchMlr1,0,mlr1
          setitem   NxchStatSearchMlr1Name,0,holdmcomp1
          setitem   NxchEditSearchMlr2,0,mlr2
          setitem   NxchStatSearchMlr2Name,0,holdmcomp2
          Move      c1 to MlrDispFlag
.BEGIN PATCH 7.7 REPLACED LOGIC
.                 move        mlr1 to str4
          move      mlr1 to str6B
.END PATCH 7.7 REPLACED LOGIC
          Call      BuildMkey
..                call        getmailer
          Move      c2 to MlrDispFlag
.BEGIN PATCH 7.7 REPLACED LOGIC
.                 move        mlr2 to str4
          move      mlr2 to str6B
.END PATCH 7.7 REPLACED LOGIC
          Call      BuildMkey
..                call        getmailer
          move      YES to DETAILLRFLAG
          call      NXCHClearListView
          call      NXCHLoadListView
          move      NO to DETAILLRFLAG
..         move        c2 to n1
..         move        c1 to tabnum
..         call        NXCHTabChange
..         Deactivate EXCHa
..         activate   EXCHb
.patch7.2
          clear     lr
          packkey   lr from str6
          MOVE      LR TO NXCHFLD2
          MOVE      C2 TO NXCHPATH
.7.3
          call      nxchkey
          if over
                    create    ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
                    activate EditTextBoxes(1)
                    clear     ErrMssg
.                   append    "ERROR = ",ErrMssg
.                   append    ERROR,ErrMssg
                    reset     ErrMssg
                    setprop   ErrorMssgStat1,visible=1
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=1
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=1
                    setitem   ErrorMssgStat1,0,"I Can't Find my lr *"
                    setitem   ErrorMssgStat2,0,"for Detail Screen"
                    setitem   ErrorMssgStat5,0,"    Leave on Screen and Inform I.S.!"
                    setitem   ErrorMssgStat3,0,ErrMssg
                    setitem   ErrorMssgOK,0,"&Stop"
                    loop
                              setfocus EditTextBoxes(1)
                              setprop   ErrorMssg,visible=1
                              getitem   EditTextBoxes(1),0,str1
                              until (str1 = "X")
                    repeat
                    destroy   EditTextBoxes(1)
                    stop
          endif
.                call        NXCHLoadDetail

.;Added to Read Exchange by entry # ISAM it's how the entry is updated when modified.
          MOVE      C1 TO NXCHPATH
          MOVE      EXKEY TO NXCHFLD1
          CALL      NXCHLOADDetail
.7.3
.patch7.2
**********************************************************
..Goes to detailscreen
.BEGIN PATCH 7.7 REPLACED LOGIC
.         move      C1,n1
.         call      NXCHTABCLICK
.         move      C2,n1
.         call      NXCHTABCHANGE
          call      NXCHSwitchTab using C2
.END PATCH 7.7 REPLACED LOGIC
**********************************************************
          call      NXCHSetFocusTab
          return
.=====================================================================================================
BUILDMKEY
.BEGIN PATCH 7.7 REPLACED LOGIC
.         clear     mkey
.         packkey   mkey from str4,z3
.         clear     mnum
.         call      nmlrkey
.         if        not over
.                   if       (MlrDispFlag = 1)
.                                setitem  NxchStatSearchMlr1Name,0,Mcomp
.;.                               if       (mnum <> mlr1)
.;.                                               move       yes to newsearch
.;.                               endif
.                                move     mnum to mlr1
.                                move     mcomp to HoldMcomp1
.                   else
.                                setitem  NxchStatSearchMlr2Name,0,Mcomp
.;.                               if       (mnum <> mlr2)
.;.                                               move  yes to newsearch
.;.                               endif
.                                move     mnum to mlr2
.                                move     mcomp to HoldMcomp2
.                   endif
.         else
.                   if       (MlrDispFlag = 1)
.                               setitem   NxchStatSearchMlr1Name,0," "
.                               move      mnum to mlr1
.                               move      b1 to HoldMcomp1
.                   else
.                               setitem   NxchStatSearchMlr2Name,0," "
.                               move      mnum to mlr2
.                               move      b1 to HoldMcomp2
.                   endif
.         endif
........................................
          clear     COMPFLD
          packkey   COMPFLD,str6B
          move      "BUILDMKEY-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if not over
                    if  (MlrDispFlag = 1)
                              setitem   NxchStatSearchMlr1Name,0,COMPCOMP
                              move      COMPNUM,mlr1
                              move      COMPCOMP,HoldMcomp1
                    else
                              setitem   NxchStatSearchMlr2Name,0,COMPCOMP
                              move      COMPNUM,mlr2
                              move      COMPCOMP,HoldMcomp2
                    endif
          else
                    if (MlrDispFlag = 1)
                              setitem   NxchStatSearchMlr1Name,0," "
                              move      COMPNUM,mlr1
                              move      b1,HoldMcomp1
                    else
                              setitem   NxchStatSearchMlr2Name,0," "
                              move      COMPNUM,mlr2
                              move      b1,HoldMcomp2
                    endif
          endif
.END PATCH 7.7 REPLACED LOGIC
          return
.......................................................................................................
NxchLoadDetail
..         MOVE      LR TO NXCHFLD2
..         MOVE      C2 TO NXCHPATH
          CALL      NXCHKEY
          if over
..error dialog box
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"Record not found for that LR!"
                    setprop   ErrorMssg,visible=1
..                   call          SetNXCHErrorMssgDefault
..                 setprop     ErrorMssg,visible=1
                    setfocus NxchEditSearchMlr1
                    return
          endif
.*********************************************************
..Useful when there is only a beginning balance record
..will fill combo box with two mailers if user wants to add a record
          unpack    exkey into dum4,dum4a
          clear     n7
          for n7,c1,c2
.BEGIN PATCH 7.7 REPLACED LOGIC
.                        clear str4
.                        load           str4 with n7,dum4,dum4a
.                        clear          mkey
.                        packkey        mkey from str4,z3
.                        call           nmlrkey
.                        if              over
.                                store "Mailer Desc not Found",n7,holdmcomp1,holdmcomp2
.                        else
.                                store   mcomp in n7,holdmcomp1,holdmcomp2
.                        endif
..............................
                    clear     str6B
                    load      str6B with n7,dum4,dum4a
                    clear     COMPFLD
                    packkey   COMPFLD from str6B
                    move      "NxchLoadDetail-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if over
                              store     "Mailer Desc not Found",n7,holdmcomp1,holdmcomp2
                    else
                              store     COMPCOMP in n7,holdmcomp1,holdmcomp2
                    endif
.END PATCH 7.7 REPLACED LOGIC
          repeat
*********************************************************
          deleteitem NXCH0001BComboBox001,0
          insertitem NXCH0001BComboBox001,c1,holdmcomp1
          insertitem NXCH0001BComboBox001,c2,holdmcomp2
.*********************************************************
..try to load mailer that ordered list for this detail
          clear     n1
          move      MLRSW,n1
          setitem   NXCH0001BComboBox001,n3,n1
.********************************************************
..Always want combo box to be present
..         If          (addflag = yes)
..          setitem     NXCH0001BComboBox001,n3,c0
..         setProp     NXCH0001BComboBox001,visible=1
..          SETFOCUS    NXCH0001BEditText001
..         SETFOCUS    NXCH0001BComboBox001
..         else
..         setprop     NXCH0001BComboBox001,visible=0
..         endif
**********************************************************
          setitem   NXCH0001BEditText001,0,lr
          unpack    dat into str2,yy,mm,dd
          pack      str10 from mm,slash,dd,slash,str2,yy
          setitem   NXCH0001BEditText002,0,str10
          clear     dim11
          move      mask11 to dim11
          edit      qty to dim11
          setitem   NXCH0001BEditText003,0,dim11
          setitem   NXCH0001BEditText004,0,list
          packkey   ndatfld from list
          rep       zfill in ndatfld
          move      c1 to ndatpath
          call      ndatkey
          setitem   NXCH0001BStatText005,0,OLSTNAME
          setitem   NXCH0001BStatText006,0,Holdmcomp1
          setitem   NXCH0001BStatText007,0,holdmcomp2
          clear     dim13a
          move      mask13 to dim13a
          edit      usage1 to dim13a
          setitem   NXCH0001BStatText009,0,dim13a
          clear     dim13b
          move      mask13 to dim13b
          edit      usage2 to dim13b
          setitem   NXCH0001BStatText010,0,dim13b
          setitem   NXCH0001BStatText011,0," "
          setprop   NXCH0001BStatText011,enabled=1,fgcolor=Black
          if (stat = "C")
                    setitem   NXCH0001BStatText011,0,"Cancelled and Adjusted"
                    setprop   NXCH0001BStatText011,enabled=1,fgcolor=RED
          elseif (stat = "R")
                    setitem   NXCH0001BStatText011,0,"Converted to rental and Adjusted"
                    setprop   NXCH0001BStatText011,enabled=1,fgcolor=RED
          elseif (stat = "X")
                    setitem   NXCH0001BStatText011,0,"Cancelled and NOT adjusted"
                    setprop   NXCH0001BStatText011,enabled=1,fgcolor=RED
          endif
          Setitem   NXCH0001BStatText015,0,Type
          Setitem   NXCH0001BEditText005,0,XCHCOMNT
.begin patch 7.92
          if        (date1 <> "")
          unpack    date1 into str2,yy,mm,dd
          pack      str10 from mm,slash,dd,slash,str2,yy
          Else
          endif
          Setitem  NXCH0001BEditText006,0,str10
          if        (dateM <> "")
          unpack    date1 into str2,yy,mm,dd
          pack      str10 from mm,slash,dd,slash,str2,yy
          Else
          Clear     str10
          endif
          Setitem  NXCH0001BEditText007,0,str10
.end patch 7.92

********************************************
..if beginning balance record do not allow modification
.BEGIN PATCH 7.7 REPLACED LOGIC
.              unpack        Exkey into Str8,str5
          unpack    Exkey into Str12,str5
.END PATCH 7.7 REPLACED LOGIC
          move      str5 to n5
          compare   n5 to c0
          if equal
..if beginning balance record do not allow modification
                    setprop   NXCH0001BButtonMod,enabled=c0
                    setprop   NXCH0001BButtonCnc,enabled=c0
                    setprop   NXCH0001BButtonREnt,enabled=c0
          else
                    setprop   COLBUTT,enabled=c1
          endif
          return
.......................................................................................................
PrevDetail
..Used with up arrow in detail screen
.BEGIN PATCH 7.7 REPLACED LOGIC
.              unpack        Exkey into Str8,str5
          unpack    Exkey into Str12,str5
.END PATCH 7.7 REPLACED LOGIC
          move      str5 to n5
          compare   n5 to c0
          if not equal
                    sub       c1 from n5
                    move      n5 to str5
.BEGIN PATCH 7.7 REPLACED LOGIC
.                      pack        exkey from str8,str5
                    pack      exkey from STR12,str5
.END PATCH 7.7 REPLACED LOGIC
                    rep       zfill in exkey
.*****************************************************
.setread information for key look up
                    move      Exkey to Nxchfld1
                    move      c1 to nxchpath
                    call      nxchloaddetail
.******************************************************
.........bizarre
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   move      c2 to n1
.                   call      NXCHTabChange
                    call      NXCHSwitchTab using C2
.END PATCH 7.7 REPLACED LOGIC
          else
..if beginning balance record do not allow modification
                    setprop   NXCH0001BButtonMod,enabled=c0
                    setprop   NXCH0001BButtonCnc,enabled=c0
                    setprop   NXCH0001BButtonREnt,enabled=c0
                    Alert     note,"Already at the First Record!",result,"Beginning Record"
          endif
          return
.......................................................................................................
NextDetail
..Used with down arrow in detail screen
.BEGIN PATCH 7.7 REPLACED LOGIC
.              unpack        Exkey into Str8,str5
          unpack    Exkey into STR12,str5
.END PATCH 7.7 REPLACED LOGIC
          move      str5 to n5
          compare   n5 to lastentry
          if not equal
                    add       c1 to n5
                    move      n5 to str5
.BEGIN PATCH 7.7 REPLACED LOGIC
.                         pack          exkey from str8,str5
                    pack      exkey from STR12,str5
.END PATCH 7.7 REPLACED LOGIC
                    rep       zfill in exkey
.*******************************************************************
..setread information for key look up
                    move      Exkey to Nxchfld1
                    move      c1 to nxchpath
                    call      NxchLoadDetail
.*******************************************************************
........bizarre
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   move      c2 to n1
.                   call      NXCHTabChange
                    call      NXCHSwitchTab using C2
.END PATCH 7.7 REPLACED LOGIC
..if not beginning balance record allow modification
                    setprop   NXCH0001BButtonMod,enabled=c1
                    setprop   NXCH0001BButtonCnc,enabled=c1
                    setprop   NXCH0001BButtonREnt,enabled=c1
                    setprop   NXCH0001BButtonMod,visible=c1
                    setprop   NXCH0001BButtonCnc,visible=c1
                    setprop   NXCH0001BButtonREnt,visible=c1
          else
                    Alert     note,"Already at the last Record!",result,"Last Record"
          endif
          return
.......................................................................................................
.......................................................................................................
NXCHLoadListView
.BEGIN PATCH 7.7 ADDED LOGIC
          move      C0,N4
          call      NXCHSwitchTab using C1
.END PATCH 7.7 ADDED LOGIC
          setprop   NXCH0001aListView002,enabled=1,bgcolor=white
          setprop   NXCH0001aListView001,enabled=1,bgcolor=white
          setprop   NXCH0001aListView002,visible=1
          setprop   NXCH0001aListView001,visible=1
          setfocus NXCH0001aListView001
          move      no to mailerswap
          CLEAR     ACCKEY
          PACKkey   ACCKEY FROM MLR1,MLR2
          PACKkey   NXNGFLD1 FROM AKEY1a,MLR1
          PACKkey   NXNGFLD2 FROM Akey2a,MLR2
          CALL      NXNGAIM
          if over
                    setitem   NxchEditSearchMlr1,0,mlr2
                    setitem   NxchEditSearchMlr2,0,mlr1
..Reset First Mailer
                    getitem   NxchEditSearchMlr1,0,mlr1
.BEGIN PATCH 7.7 REPLACED LOGIC
.                    move  mlr1,str4
                    move      mlr1,str6B
.END PATCH 7.7 REPLACED LOGIC
                    Move      c1 to MlrDispFlag
                    CALL      BUILDMKEY
...                    call      getmailer
..Reset Second Mailer
                    Move      c2 to MlrDispFlag
                    getitem   NxchEditSearchMlr2,0,mlr2
.BEGIN PATCH 7.7 REPLACED LOGIC
.                    move      mlr2,str4
                    move      mlr2,str6B
.END PATCH 7.7 REPLACED LOGIC
                    CALL      BUILDMKEY
...                    call      getmailer
..         move      yes to mailerswap
..         move      mlr2 to str4
..         move      mlr1 to mlr2
..         move      str4 to mlr1
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM Akey1a,MLR1
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR2
                    CALL      NXNGAIM
                    if over
..try to flag if they go to detail screen and want to add a record
                              move      NO to EXGOODFLAG
..ClearDetailScreen if not a valid exchange record
                              call      ClearDetail
                              setprop   ErrorMssgStat1,visible=0
                              setprop   ErrorMssgStat2,visible=1
                              setprop   ErrorMssgStat3,visible=0
                              setprop   ErrorMssgStat4,visible=0
                              setprop   ErrorMssgStat5,visible=0
                              setitem   ErrorMssgStat2,0,"Exchange Account not found!"
                              setprop   ErrorMssg,visible=1
.                   call        SetNXCHErrorMssgDefault
                              setfocus NxchEditSearchMlr1
                              setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                              setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
                              CLEAREVENT
..Patch6.91
                              setprop   NxchStatActive,visible=c0
..Subpatch6.91
                              return
                    endif
          endif
..Patch6.91
          if (flag = "I")         ;.if inactive show this statement
                    setprop   NxchStatActive,visible=c1
          else
                    setprop   NxchStatActive,visible=c0
          endif
..Subpatch6.91
          move      entry to lastentry
          move      c0 to entry
          CLEAR     EXKEY
          PACK      EXKEY FROM ACCKEY,ENTRY
          REP       zfill,EXKEY
.
READBAL2
          REP       zfill IN EXKEY
          MOVE      C1 TO NXCHPATH
          MOVE      EXKEY TO NXCHFLD1
          CALL      NXCHKEY
          if over
.try to flag if they go to detail screen and want to add a record
                    move      NO to EXGOODFLAG
.ClearDetailScreen
                    call      ClearDetail
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"Beginning Balance Record Missing!"
                    setprop   ErrorMssg,visible=1
                    setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                    setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
..         call     SetNXCHErrorMssgDefault
                    setfocus NxchEditSearchMlr1
                    return
          endif
.         MOVE      LR TO KEY              *FOR MATCH IN MODIF.BEG.BAL.MODE.
.load beg bal
.===================================================================
.if Read is valid don't prompt in detail add to add beginning balance
          move      YES to EXGOODFLAG
.===================================================================
          move      c0 to total1
          move      c0 to total2
          call      NXCHLoadListView1
...............................................................................................
Details
          loop
.                call    ANIMATEIT
                    if (entry = lastentry)        .already done
********************************************************************
.If only beginning balance fill deatil screen with beg balance record
.Also if only beg bal record fill usage info on the botton of screen one-ex. owes ,owed etc.
                              if (entry = c0)
                                        call      detailx
************
                                        move      Exkey to Nxchfld1
                                        move      c1 to nxchpath
                                        call      NxchLoadDetail
                                        setprop   NxchEditSearchMlr1,enabled=c1,bgcolor=white
                                        setprop   NxchEditSearchMlr2,enabled=c1,bgcolor=white
************
                              endif
********************************************************************
                              return
                    endif
                    call      NXCHKS
                    if not over
.BEGIN PATCH 7.7 REPLACED LOGIC
.                         unpack  exkey into str8,entry
                              unpack    exkey into str12,entry
.END PATCH 7.7 REPLACED LOGIC
                              call      NXCHLoadListView1
                    else
                              goto detailx
                    endif
.BEGIN PATCH 7.7 REPLACED LOGIC
.                until  (entry = lastentry or acckey <> str8)
                    until (entry = lastentry or acckey <> str12)
.END PATCH 7.7 REPLACED LOGIC
          repeat
.        destroy  AnimateIcon
detailx
          if (total1 > total2)
                    sub       total2 from total1
                    clear     dim13a
                    move      mask13 to dim13a
                    edit      total1 to dim13a
                    setitem   NXCH0001StatText003,0,"Owes ->"
                    setitem   NXCH0001StatText004,0,dim13a
                    setprop   NXCH0001StatText004,enabled=1,fgcolor=RED
                    setitem   NXCH0001StatText005,0,"<- Is owed"
                    setprop   NXCH0001StatText003,enabled=1,fgcolor=RED
                    setprop   NXCH0001StatText005,enabled=1,fgcolor=BLACK
          else
                    sub       total1 from total2
                    clear     dim13b
                    move      mask13 to dim13b
                    edit      total2 to dim13b
                    setitem   NXCH0001StatText003,0,"Is Owed ->"
                    setitem   NXCH0001StatText004,0,dim13b
                    setprop   NXCH0001StatText004,enabled=1,fgcolor=RED
                    setitem   NXCH0001StatText005,0,"<- Owes"
                    setprop   NXCH0001StatText005,enabled=1,fgcolor=RED
                    setprop   NXCH0001StatText003,enabled=1,fgcolor=BLACK
          endif
.BEGIN PATCH 7.7 ADDED LOGIC
          move      N4,str4
          call      FormatNumeric using str4,str5
          if (N4 = 1)
                    pack      str25,str5," Record Found."
          else
                    pack      str25,str5," Records Found."
          endif
          setitem   NxchStatCount,0,str25
          setitem   NxchStatMlrLeft,0,HoldMComp1
          setitem   NxchStatMlrRight,0,HoldMComp2
.END PATCH 7.7 ADDED LOGIC
*********************************************************************************
.If call by Lr detail has already been created
test
          IF (DETAILLRFLAG = NO)
                    move      Exkey to Nxchfld1
                    move      c1 to nxchpath
                    call      NxchLoadDetail
          ENDIF
*********************************************************************************
.add compare of stored totals to calced and error box if don't match
          return
.==============================================================================================
NXCHLoadListView1
.
.         call    ANIMATEIT
          unpack    dat into str2,yy,mm,dd
          pack      str10 from mm,slash,dd,slash,str2,yy
          clear     dim13a
          clear     dim13b
.BEGIN PATCH 7.3 ADDED LOGIC
.BEGIN PATCH 7.7 REPLACED LOGIC
.         unpack    EXKEY,str8,str5
          unpack    EXKEY,str12,str5
.END PATCH 7.7 REPLACED LOGIC
          rep       zfill,str5
.END PATCH 7.3 ADDED LOGIC
          if (entry = 0)
                    move      usage1 to total1
                    move      usage2 to total2
                    move      mask13 to dim13a
                    move      mask13 to dim13b
                    edit      total1 to dim13a
                    edit      total2 to dim13b
                    if (Mailerswap = "N")
.BEGIN PATCH 7.3 REPLACED LOGIC
.                             NXCH0001aListView001.GetItemCount GIVING totIndx
.                             NXCH0001aListView001.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView001.SetItemText using N9,lr,1
.                             NXCH0001aListView001.SetItemText using N9,"0",2
.                             NXCH0001aListView001.SetItemText using N9,Dim13a,3
.                             NXCH0001aListView001.SetItemText using N9,exkey,4
.;.
.                             NXCH0001aListView002.GetItemCount GIVING totIndx
.                             NXCH0001aListView002.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView002.SetItemText using N9,lr,1
.                             NXCH0001aListView002.SetItemText using N9,"0",2
.                             NXCH0001aListView002.SetItemText using N9,Dim13b,3
.                             NXCH0001aListView002.SetItemText using N9,exkey,4
.                   else
.                             NXCH0001aListView001.GetItemCount GIVING totIndx
..                            NXCH0001aListView001.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView001.SetItemText using N9,lr,1
.                             NXCH0001aListView001.SetItemText using N9,"0",2
.                             NXCH0001aListView001.SetItemText using N9,Dim13b,3
.                             NXCH0001aListView001.SetItemText using N9,exkey,4
.;.
.                             NXCH0001aListView002.GetItemCount GIVING totIndx
.                             NXCH0001aListView002.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView002.SetItemText using N9,lr,1
.                             NXCH0001aListView002.SetItemText using N9,"0",2
.                             NXCH0001aListView002.SetItemText using N9,Dim13a,3
.                             NXCH0001aListView002.SetItemText using N9,exkey,4
................................
                              NXCH0001aListView001.InsertItem giving N9 using str5
                              NXCH0001aListView001.SetItemText using N9,str10,1
                              NXCH0001aListView001.SetItemText using N9,lr,2
                              NXCH0001aListView001.SetItemText using N9,"0",3
                              NXCH0001aListView001.SetItemText using N9,Dim13a,4
                              NXCH0001aListView001.SetItemText using N9,exkey,5
..
                              NXCH0001aListView002.InsertItem giving N9 using str5
                              NXCH0001aListView002.SetItemText using N9,str10,1
                              NXCH0001aListView002.SetItemText using N9,lr,2
                              NXCH0001aListView002.SetItemText using N9,"0",3
                              NXCH0001aListView002.SetItemText using N9,Dim13b,4
                              NXCH0001aListView002.SetItemText using N9,exkey,5
                    else
                              NXCH0001aListView001.InsertItem giving N9 using str5
                              NXCH0001aListView001.SetItemText using N9,str10,1
                              NXCH0001aListView001.SetItemText using N9,lr,2
                              NXCH0001aListView001.SetItemText using N9,"0",3
                              NXCH0001aListView001.SetItemText using N9,Dim13b,4
                              NXCH0001aListView001.SetItemText using N9,exkey,5
..
                              NXCH0001aListView002.InsertItem giving N9 using str5
                              NXCH0001aListView002.SetItemText using N9,str10,1
                              NXCH0001aListView002.SetItemText using N9,lr,2
                              NXCH0001aListView002.SetItemText using N9,"0",3
                              NXCH0001aListView002.SetItemText using N9,Dim13a,3
                              NXCH0001aListView002.SetItemText using N9,exkey,5
.END PATCH 7.3 REPLACED LOGIC
                    endif
          else
                    if (mlrsw = "1")
                              clear     dim13a
                              if (stat <> "C" and stat <> "R")
                                        add       qty to total1
                                        move      mask13 to dim13a
                                        edit      total1 to dim13a
                              else
                                        if (stat = "C")
                                                  move      "Cancelled" to dim13a
                                        elseif (stat = "R")
                                                  move      "Rental" to dim13a
                                        endif
                              endif
                              clear     dim11
                              move      mask11 to dim11
                              edit      qty to dim11
.BEGIN PATCH 7.3 REPLACED LOGIC
.                             NXCH0001aListView001.GetItemCount GIVING totIndx
.                             NXCH0001aListView001.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView001.SetItemText using N9,lr,1
.                             NXCH0001aListView001.SetItemText using N9,dim11,2
.                             NXCH0001aListView001.SetItemText using N9,Dim13a,3
.                             NXCH0001aListView001.SetItemText using N9,exkey,4
..................
                              NXCH0001aListView001.InsertItem giving N9 using str5
                              NXCH0001aListView001.SetItemText using N9,str10,1
                              NXCH0001aListView001.SetItemText using N9,lr,2
                              NXCH0001aListView001.SetItemText using N9,dim11,3
                              NXCH0001aListView001.SetItemText using N9,Dim13a,4
                              NXCH0001aListView001.SetItemText using N9,exkey,5
.END PATCH 7.3 REPLACED LOGIC
                    else
                              clear     dim13b
                              if (stat <> "C" and stat <> "R")
                                        add       qty to total2
                                        move      mask13 to dim13b
                                        edit      total2 to dim13b
                              else
                                        if (stat = "C")
                                                  move      "Cancelled" to dim13b
                                        elseif (stat = "R")
                                                  move      "Rental" to dim13b
                                        endif
                              endif
                              clear     dim11
                              move      mask11 to dim11
                              edit      qty to dim11
.BEGIN PATCH 7.3 REPLACED LOGIC
.                             NXCH0001aListView002.GetItemCount GIVING totIndx
.                             NXCH0001aListView002.InsertItem giving N9 using str10, *Index=totIndx
.                             NXCH0001aListView002.SetItemText using N9,lr,1
.                             NXCH0001aListView002.SetItemText using N9,dim11,2
.                             NXCH0001aListView002.SetItemText using N9,Dim13b,3
.                             NXCH0001aListView002.SetItemText using N9,exkey,4
.................
                              NXCH0001aListView002.InsertItem giving N9 using str5
                              NXCH0001aListView002.SetItemText using N9,str10,1
                              NXCH0001aListView002.SetItemText using N9,lr,2
                              NXCH0001aListView002.SetItemText using N9,dim11,3
                              NXCH0001aListView002.SetItemText using N9,Dim13b,4
                              NXCH0001aListView002.SetItemText using N9,exkey,5
.END PATCH 7.3 REPLACED LOGIC
                    endif
          endif
          clear     dim13a
          clear     dim13b
          move      mask13 to dim13a
          move      mask13 to dim13b
          edit      total1 to dim13a
          edit      total2 to dim13b
          setitem   NXCH001Total1,0,dim13a
          setitem   NXCH001Total2,0,dim13b
.BEGIN PATCH 7.7 ADDED LOGIC
          add       C1,N4
.END PATCH 7.7 ADDED LOGIC
          return
.......................................................................................................
NXCHClearListView
          NXCH0001aListView001.DeleteAllItems giving N9
          NXCH0001aListView002.DeleteAllItems giving N9
..-       NXCH0001CListView003.DeleteAllItems giving N9
          sub       qty from qty
          sub       total1 from total1
          sub       total2 from total2
          setitem   NXCH001Total1,0,""
          setitem   NXCH001Total2,0,""
          setitem   NXCH0001StatText003,0,""
          setitem   NXCH0001StatText004,0,""
          setitem   NXCH0001StatText005,0,""
          if (DETAILLRFLAG = no)
                    setitem   NXCH0001BStatText011,0,""
                    setitem   NXCH0001BStatText015,0,""
          endif
          setprop   NXCH0001BStatText012,Height=0
          setprop   NXCH0001BCheck001,Height=0
..Button control set under save button
..                   setprop  NXCH0001BButtonSave,visible=0
..                   setprop  NXCH0001BButtonQuit,visible=0
..                   setprop  NXCH0001BButtonADD,visible=1
..                   setprop  NXCH0001BButtonMod,visible=1
..                   setprop  NXCH0001BButtonCnc,visible=1
..                   setprop  NXCH0001BButtonRent,visible=1
          return
.......................................................................................................
RestoreMain
..Used by exit button on Nxch0001C
          Deactivate ExchC
          Activate ExchA
          Activate ExchB
          Setprop   NXCH0001BButtonNext,visible=c1                    
          Setprop   NXCH0001BButtonPrev,visible=c1                    
          setprop   NXCH0001ButtonGO,visible=1
          NXCH0001CListView001.DeleteAllItems giving N9
..-        NXCH0001CListView002.DeleteAllItems giving N9
..-        NXCH0001CListView003.DeleteAllItems giving N9
          setprop   NXCH0001AListView001,visible=1
          setprop   NXCH0001AListView002,visible=1
.*******************************************************
          setitem   NxchStatSearchMlr2Name,0,""
          setitem   NxchEditSearchMlr2,0,""
          clear     mlr2
.BEGIN PATCH 7.7 REMOVED VARIABLES - NO LONGER USED
.       clear     hmlr2
.END PATCH 7.7 REMOVED VARIABLES - NO LONGER USED
.******************************************************
          setprop   NxchStatSearchMlr2,visible=1
          setprop   NxchEditSearchMlr2,visible=1
          setprop   NxchStatSearchMlr2Name,visible=1
.**********************************************************
..Goes to detailscreen
..                move    C1,n1
..                call    NXCHTABCLICK
..                move    C2,n1
..                call    NXCHTABCHANGE
.**********************************************************
..        call      NXCHSetFocusTab
          setfocus NxchEditSearchMlr1
.BEGIN PATCH 7.7 ADDED LOGIC
          setitem   NxchStatCount,0,""
          setitem   NxchStatMlrLeft,0,""
          setitem   NxchStatMlrRight,0,""
.END PATCH 7.7 ADDED LOGIC
          goto main
.=============================================================================================
.......................................................................................................
FileGo
          branch    result to FileGo1,FileGo2,FileGo2
          return
.filego1 - report options for printing
FileGo1
          return
FileGo2
          call      click_Nxch0001ButtonExit
          return
***********************************************
.              winshow
.              stop
***********************************************
.......................................................................................................
TutorGo
          execute   "!c:\progra~1\plus!\micros~1\iexplore.exe http://web01/programs/ninca/nxch0001.html"
          if over                .Failed
                    execute   "!c:\progra~1\Intern~1\iexplore.exe http://web01/programs/ninca/nxch0001.html"
          endif
          return
.......................................................................................................

colorerror
          noreturn
          move      C1,colorflag
          goto aftercolor
ColorGo
          if (result = C1)
                    call      BackColor
          elseif (result = C2)
                    call      TextColor
          else
                    return
          endif
          clear     n1
          prep      colorfile,"c:\program files\nincal\NXCH0001.col"
          loop
                    add       c1,n1
                    write     colorfile,seq;colornum(n1)
                    until (n1 =2)
          repeat
          close     colorfile
          return
.Trap for Cancel Entry in Color System Menu
ColorTrap
          noreturn
          return
BackColor
          trap      ColorTrap if object
          create    BGC
          trapclr   object
          setprop   ColBack,bgcolor=BGC
          getitem   BGC,1,Fred
          getitem   BGC,2,Fgreen
          getitem   BGC,3,Fblue
          pack      colornum(2),Fred,Fgreen,Fblue
          return
TextColor
          trap      ColorTrap if object
          create    FTC
          trapclr   object
          setprop   ColText,fgcolor=FTC
          getitem   FTC,1,Fred
          getitem   FTC,2,Fgreen
          getitem   FTC,3,Fblue
          pack      colornum(1),Fred,Fgreen,Fblue
          return
ExchangeKeyPress
          if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
                    goto SearchGo3
          elseif (N9 = 120)     .F9 Key closes Search Function
                    setprop   Search,visible=0
          endif
          return
............................................................................................................
PrintGo
          branch    result to PrintGo1,PrintGo2,PrintGo3,PrintGo4
PrintGo1
.......................................
.............MESSAGE BOXES.............
.Following are dynamically created Message Boxes using Report2, which only contains a couple of objects.
.Each time a new Message box is created:  1)the Objects from the previous incarnation must be destroyed
. 2)the Form must receive a new Title  3)the Objects must be dumped into the ObjectColl collection in order
.to facilitate easy destruction.
.Each Message Box may have several associated sub-routines, triggered by object events, ie Lost_Focus,
.Click, etc.
.Keep all dynamic use of Report 2 in this section in order to better monitor its use.  ASH
.......................................
.Allows selection of different reports for Exchange Summary Reports
.
          call      Report2DestroyObjects
          setprop   Report2,title="NIN Exchange Report"
.begin patch 7.8    
.         create    Report2;StatTextBoxes(1)=30:50:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
.         create    Report2;StatTextBoxes(2)=50:70:10:110,"Report Type","'>MS Sans Serif'(8)"
.         create    Report2;StatTextBoxes(3)=70:90:10:110,"Copies","'>MS Sans Serif'(8)"
.         create    Report2;ComboBoxes(1)=30:91:80:310,"",";).;).",ToolTip="Use Scroll or Search (F2) to change"
.         create    Report2;ComboBoxes(2)=50:71:80:310,"",";S)ummary;)Inhouse Summary"
.         create    Report2;EditTextBoxes(1)=70:91:80:110,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1

          create    Report2;StatTextBoxes(1)=20:40:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
          create    Report2;StatTextBoxes(2)=40:60:10:110,"Report Type","'>MS Sans Serif'(8)"
          create    Report2;StatTextBoxes(3)=60:80:10:110,"Copies","'>MS Sans Serif'(8)"
          create    Report2;ComboBoxes(1)=20:81:80:310,"",";).;).",ToolTip="Use Scroll or Search (F2) to change"
          create    Report2;ComboBoxes(2)=40:61:80:310,"",";S)ummary;)Inhouse Summary"
          create    Report2;EditTextBoxes(1)=60:81:80:110,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1
.end patch 7.8
          eventreg ComboBoxes(1),10,PrintGo1KeyPress,RESULT=N9
          move      NO,str1
.begin patch 7.8
          create    Report2;CheckBoxes(10)=80:95:10:140,"PLI Logo"

.end patch 7.8

          create    Report2;CheckBoxes(1)=100:115:10:140,"Exclude < 1000"
          create    Report2;CheckBoxes(2)=120:135:10:80,"Duplex"
          create    Report2;CheckBoxes(3)=140:155:10:80,"PDF"
.Patch7.21
          create    Report2;CheckBoxes(9)=80:95:200:390,"Revision Date"
.Patch7.21
          create    Report2;CheckBoxes(4)=100:115:200:390,"List Universe"
          create    Report2;CheckBoxes(5)=120:135:200:390,"Last Transaction Date"
          create    Report2;CheckBoxes(6)=140:155:200:390,"Include Inactive"
.Patch6.7
          create    Report2;CheckBoxes(7)=160:175:200:390,"Exclusive Only"
.EndSubPatch6.7
.Patch6.92
          create    Report2;CheckBoxes(8)=160:175:10:180,"Owed Names/Never Used",TOOLTIP="Will bold mailer if client is owed names and has never used mailer's list"
.EndSubPatch6.92
.Begin Patch 7.94
          create    Report2;CheckBoxes(11)=180:195:10:180,"Excel",TOOLTIP="Will create excel spreadsheet"
.End patch 7.94
          activate StatTextBoxes(1)
          activate StatTextBoxes(2)
          activate StatTextBoxes(3)
.begin patch 5.1
          setitem   EditTextBoxes(1),1,"001"          .default copies
.end patch 5.1
          activate EditTextBoxes(1)
          activate CheckBoxes(1)
          activate CheckBoxes(2)
          activate CheckBoxes(3)
          activate CheckBoxes(4)
          activate CheckBoxes(5)
          activate CheckBoxes(6)
          activate CheckBoxes(7)
          activate CheckBoxes(8)
.begin patch 7.95
           Setitem   checkBoxes(3),0,N1
.end patch 7.95


.Patch7.21
          activate CheckBoxes(9)
.Patch7.21
.Begin Patch 7.94
          activate CheckBoxes(11)
.End Patch 7.94
.begin patch 7.8
          activate CheckBoxes(10)
          if        (NuseComp = "P")
                    Move      c1,n1
                    Else
                    move      C0,n1
          endif
          setitem   checkBoxes(10),0,n1             ;.set status of logo
.end patch 7.8

          activate ComboBoxes(1)
          deleteitem ComboBoxes(1),0
          insertitem ComboBoxes(1),1,Holdmcomp1
          insertitem ComboBoxes(1),2,Holdmcomp2
          setitem   ComboBoxes(1),0,1
          activate ComboBoxes(2)
.Patch7.21
          listins   ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),ComboBoxes(1),ComboBoxes(2),EditTextBoxes(2):
                    CheckBoxes(1),CheckBoxes(2),CheckBoxes(3),CheckBoxes(4),CheckBoxes(5),CheckBoxes(6),CheckBoxes(7),CheckBoxes(8):
                    CheckBoxes(9),CheckBoxes(10),CheckBoxes(11)
.Patch7.21
          setfocus ComboBoxes(1)
          setprop   report2,visible=1
..if rptcan=no  then verify User has ok'd lets go and do job
          If (rptcan = No)
                    call      OrderSetMouseBusy
..Creates Timers for DialogWait Message Box
                    create    timer1,10
                    activate timer1,tome1,result
..              create timer2,60
..              activate timer2,tome2,result

                    setitem   DIALOGWAITMESSAGE,0,"Processing Your Request.....!!"
                    setprop   DialogWait,visible=1
.********************************************************************
..get mailer, report type, print options, etc
                    getitem   checkBoxes(1),0,n1             ;.include balances less than 1000 if checked
                    if (n1 = c1)                      ;.not checked default
                              move      YES to BalanceFilter
                    else
                              move      No to BalanceFilter
                    endif
                    getitem   checkBoxes(4),0,n1             .Print Universes?
                    if (n1 = c1)
                              move      C2 to UnivFlag
                    else
                              move      C1 to UnivFlag
                    endif
                    getitem   checkBoxes(5),0,n1             .Last Transaction date?
                    if (n1 = c1)
                              move      C2 to TranFlag          .yes
                    else
                              move      c1 to TranFlag
                    endif
.
                    getitem   checkBoxes(6),0,n1             .Print Inactive accounts
                    if (n1 = c1)
                              move      Yes to InactiveFlag
                    else
                              move      No to INACTIVEFLAG              .default
                    endif
.
                    getitem   ComboBoxes(2),0,n1
                    if (n1 = c1)
                              move      no to InhouseFlag
                    else
                              move      YES to InhouseFlag
                    endif
..Patch6.7
                    getitem   checkBoxes(7),0,n1             .Print Only Ninca Exclusives
                    if (n1 = c1)
                              move      Yes to EXCLUSFLAG
                    else
                              move      No to EXCLUSFLAG
                    endif
..Endsubpatch6.7
..Patch6.92
                    getitem   checkBoxes(8),0,n1             .Bold Mailer with owed names but has never been used before
                    if (n1 = c1)
                              move      YES to NUSEDFLAG
                    else
                              move      NO to NUSEDFLAG
                    endif
..Endsubpatch6.92
.Patch7.21
                    getitem   checkBoxes(9),0,n1             ;Include Revision Date
                    if (n1 = c1)
                              move      YES to REVVFLAG
                    else
                              move      NO to REVVFLAG
                    endif
.begin patch 7.8
                    getitem   checkBoxes(10),0,n1             .get logo info
                    if (n1 = c1)
                              move      "P" to LogoFlag
                    else
                              move      NO to LogoFLAG             .needs to get  passed 
                    endif

.end patch 7.8
.begin patch 7.94
                    getitem   checkBoxes(11),0,n1             .Excel?
                    if (n1 = c1)
                              move      Yes to ExcelFlag
                    else
                              move      NO to ExcelFLAG             
                    endif

.end patch 7.94
.Endsubpatch7.21
..begin patch 5.1
                    CLEAR     ReportMlr
..end patch 5.1
                    getitem   ComboBoxes(1),0,n1
                    if (n1 = c1)
                              move      mlr1 to ReportMLR
                    else
                              move      Mlr2 to ReportMLR
                    endif
..begin patch 5.1
                    if (ReportMlr = "")
                              Alert     note,"Sorry you need to select a Mailer!",result,"Mailer Needed"
                              call      OrderSetMouseFree
                              return
                    endif
..end patch 5.1
                    MOVE      "01",FILENUM
                    call      Filename
                    clear     str35
                    clear     str10
                    clear     str4
                    clear     str3
.BEGIN PATCH 7.7 ADDED LOGIC
                    clear     str6B
.END PATCH 7.7 ADDED LOGIC
..Patch6.92
..              WRITE         RECMST,SEQ;ReportMLR,"H   ",c4,univflag,TRANFLAG:
..                            str35,str35,str35,str10,str10,str10,str4
.Patch7.21
.begin patch 7.8
                    WRITE     RECMST,SEQ;ReportMLR,"H   ",c4,univflag,TRANFLAG,NUsedflag,REVVFLAG:
                              str35,str35,str35,str10,str10,str10,str2,LogoFlag
.                             LogoFlag,str35,str35,str35,str10,str10,str10,str2
.                   WRITE     RECMST,SEQ;ReportMLR,"H   ",c4,univflag,TRANFLAG,NUsedflag,REVVFLAG:
.                             str35,str35,str35,str10,str10,str10,str2
.end patch 7.8
.              WRITE          RECMST,SEQ;ReportMLR,"H   ",c4,univflag,TRANFLAG,NUsedflag:
.                             str35,str35,str35,str10,str10,str10,str3
.Patch7.21
.EndSub6.92
                    PACK      NXNGfld1 FROM AKey1A,ReportMlr
                    CLEAR     NXNGFld2
                    call      nxngaim
                    goto AimKeyTwo if over
.                   cmatch    "I" to flag                 .active account ?
.                   if not equal                    .yes
.                             call      GetSummaryRepDetail
.                   endif
.BEGIN PATCH 7.71 REPLACED LOGIC
                    if     (flag <> "I" | inactiveflag = "Y")
                              call      GetSummaryRepDetail
                    endif
.End PATCH 7.71 REPLACED LOGIC
                    move      no to loopdone
                    loop
                              call      nxngkg
                              if over
                                        move      yes to loopdone
                              endif
.                   cmatch    "I" to flag                 .active account ?
.                   if not equal                    .yes
.                             call      GetSummaryRepDetail
.                   endif
.BEGIN PATCH 7.71 REPLACED LOGIC
                    if     (flag <> "I" | inactiveflag = "Y")
                              call      GetSummaryRepDetail
                    endif
.End PATCH 7.71 REPLACED LOGIC
                              Until (loopdone = yes)
                    repeat
.
AimKeyTwo
                    PACK      NXNGfld2 FROM AKey2A,ReportMlr
                    CLEAR     NXNGFld1
                    call      nxngaim
                    goto SummaryDone if over
.         cmatch    "I" to flag                 .active account ?
.         if not equal                    .yes
.                   call      GetSummaryRepDetail
.         endif
.BEGIN PATCH 7.71 REPLACED LOGIC
                    if     (flag <> "I" | inactiveflag = "Y")
                              call      GetSummaryRepDetail
                    endif
.End PATCH 7.71 REPLACED LOGIC
                    move      no to loopdone
                    loop
                              call      nxngkg
                              if over
                                        move      yes to loopdone
                              endif
.                   cmatch    "I" to flag                 .active account ?
.                   if not equal                    .yes
.                             call      GetSummaryRepDetail
.                   endif
.BEGIN PATCH 7.71 REPLACED LOGIC
                    if     (flag <> "I" | inactiveflag = "Y")
                              call      GetSummaryRepDetail
                    endif
.End PATCH 7.71 REPLACED LOGIC
                              Until (loopdone = yes)
                    repeat
                    goto SummaryDone
PRintGo1KeyPress
                    if (N9 = 113) ;.F2 Key calls Search Function
..Virtual Key Value
                              move      c3 to MlrSearchFlag
                              goto SearchGo3
                    elseif (N9 = 120)     ;.F9 Key closes Search Function
                              setprop   Search,visible=0
                    endif
                    return
..........................................................................................................
GetSummaryRepDetail
                    unpack    acckey into Rmlr1,Rmlr2
                    CLEAR     Nxchfld1
                    PACKKey   Nxchfld1 FROM ACCKEY,ENTRY
                    REP       ZFILL,Nxchfld1
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   if (nxchfld1 = "0000000000000")
                    if (nxchfld1 = "00000000000000000")
.END PATCH 7.7 REPLACED LOGIC
                              return
                    endif
                    call      nxchkey
                    if over
.begin patch 7.9
                              Move      "This is an Error e-mail from Nxch0001",MailSubjct
                              Clear     Mailbody
                              append    "This is an error message",Mailbody
                              append    "<br>",mailbody
                              append    "Job was for ",Mailbody
                              append    "<br>",mailbody
                              append    User,Mailbody
                              append    "<br>",mailbody
                              append    "Detail REcord Not FOund for ",Mailbody
                              Append    Nxchfld1,MailBody
                              append    "<br>",mailbody
                              Append    "Subroutine GetSummaryRepDetail",MailBody
                              reset     Mailbody
.                             Move      "This is an Error e-mail from Nxch0001",SmtpSubject Subject
.                             Move      "This is an error message",SmtpTextMessage(1)   Array <Text message >
.                             Clear     SmtpTextMEssage(2)
.                             append    "Job was for ",SmtpTextMEssage(2)
.                             append    User,SmtpTextMEssage(2)
.                             reset     SmtpTextMEssage(2)
.                             Clear     SmtpTextMEssage(3)
.                             append    "Detail record Not Found for ",SmtpTextMEssage(3)
.                             append    nxchfld1,SmtpTextMEssage(3)
.                             reset     SmtpTextMEssage(3)
.                             Move      "Subroutine GetSummaryRepDetail",SmtpTextMessage(4)   Array <Text message >
.                             Move      "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
.end patch 7.9
                              call      errmesg
                    endif
                    If (reportmlr = Rmlr1)
.BEGIN PATCH 7.7 REPLACED LOGIC
.         pack    mkey from Rmlr2,z3
.         call    nmlrkey
.              endif
.              If            (reportmlr = Rmlr2)
.         pack  mkey from Rmlr1,z3
.                    call  nmlrkey
.............
                              pack      COMPFLD,RMLR2
                              move      "GetSum-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                    endif
                    If (reportmlr = Rmlr2)
                              pack      COMPFLD,RMLR1
                              move      "GetSum,2-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
.END PATCH 7.7 REPLACED LOGIC
                    endif
baddave
                    If (Yes = BalanceFilter)                         ;.default exclude bal < 1000
                              If (usage1 > usage2 & usage1-usage2 < 1000)
                                        return
                              endif
                              If (usage2 > usage1 & usage2-usage1 < 1000)
                                        return
                              endif
                    endif
                    MOVE      LIST TO NDATFLD
                    REP       ZFILL IN NDATFLD
                    MOVE      C1 TO NDATPATH
                    CALL      NDATKEY
                    if over
                              move      "No List found!" to Olstname
                    endif
..Patch6.7
                    If (exclusflag = YES)              .Yes NINCA Exclusive
..=======================================================
                              move      c2 to nxrfpath
.BEGIN PATCH 7.7 REPLACED LOGIC
.                        unpack   mkey to nxrffld2,str3
                              unpack    COMPFLD to nxrffld2
.END PATCH 7.7 REPLACED LOGIC
                              Clear     Nxrfmlr
                              call      NXRFKEY
                              loop
                                        match     NXRFMLR to nxrffld2
                                        if equal
                                                  move      nxrflist to NDATFLD
                                                  call      NDATKEY
.begin patch 7.93
.                                                  If        (Elstcde = "C" or Elstcde = "P")
                                                  If        (Elstcde <> "C" & Elstcde <>"P")
                                                  return
.                                                  goto      ContSUm
                                                  endif
                                                  goto      Contsum
.                                                  cmatch    "C" with ELSTCDE
.                                                  return if not equal
.                                                  goto CONTSUM
.end patch 7.93
..                     goto CONTSUM if equal
                                        endif
                                        call      NXRFKS
                                        until over
                              repeat
                              return
.=====================================================
                    endif
CONTSUM
.EndsubPatch6.7
.BEGIN PATCH 7.7 REPLACED LOGIC
.              unpack    exkey into str8,str5
                    unpack    exkey into STR12,str5
.END PATCH 7.7 REPLACED LOGIC
                    match     "00000" to str5
                    if equal
                              clear     mlrsw
                    endif
.BEGIN PATCH 7.7 REPLACED LOGIC
.              WRITE     RECMST,SEQ;nxchvars,Olstname,Mcomp
one
.could add a byte (flag) for inactive accounts on summary report
                    WRITE     RECMST,SEQ;nxchvars,Olstname,COMPCOMP,logoflag
.END PATCH 7.7 REPLACED LOGIC
                    return
............................................................................................
SummaryDone
.record selection done let's finalize and submit printing
                    getitem   checkBoxes(2),0,n1
                    if (n1 = c1)
                              move      yes to duplexFlag
                    else
                              move      No to duplexFlag
                    endif
                    getitem   checkBoxes(3),0,N1
                    IF (n1 = c1)
                              move      YES to PdfFlag
                    else
                              move      No to PdfFlag
                    endif
                    WEOF      RECMST,SEQ
                    CLOSE     RECMST,EOFSIZE
                    getitem   EditTextBoxes(1),0,str3          .number of copies
..begin patch 5.1
                    move      c0 to n3
                    move      str3 to n3
                    if (str3 = "")
                              move      c1 to str3
                    elseif (n3 = c0)
                              clear     str55
                              pack      str55 from User," your Job Using ",newname," Is Done"
                              call      ordersetmousefree
                              Alert     note,str55,result
                              return
                    endif
..end patch 5.1
                    clear     taskname
.begin patch 7.94
.begin patch 7.96
                    If        (ExcelFlag = Yes & INHOUSEFLAG = Yes)
                              append    "!\\nins1\winbatch\BUTIL job=xchxlsINH INfile=",TASKNAME
.                    If        (ExcelFlag = Yes)
                    ElseIf        (ExcelFlag = Yes)
                              append    "!\\nins1\winbatch\BUTIL job=xchxls INfile=",TASKNAME
.end patch 7.96
                    
                    ElseIf (INHOUSEFLAG = Yes & DuplexFlag = No)
.                    If (INHOUSEFLAG = Yes & DuplexFlag = No)
.begin patch 7.8
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=xchINH INfile=",TASKNAME
                              append    "!\\nins1\winbatch\BUTIL job=xchINH INfile=",TASKNAME
.end patch 7.8
.                    endif
                    ElseIf (INHOUSEFLAG = Yes & DuplexFlag = yes)
.begin patch 7.8
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=xchINHDP INfile=",TASKNAME
                              append    "!\\nins1\winbatch\BUTIL job=xchINHDP INfile=",TASKNAME
.end patch 7.8
.                    endif
                    ElseIf (INHOUSEFLAG = No & DuplexFlag = No)
.begin patch 7.8
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=nxchg INfile=",TASKNAME
                              append    "!\\nins1\winbatch\BUTIL job=nxchg INfile=",TASKNAME
.end patch 7.8
.                    endif
                    ElseIf (INHOUSEFLAG = No & DuplexFlag = yes)
.begin patch 7.8
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=nxchgDP INfile=",TASKNAME
                              append    "!\\nins1\winbatch\BUTIL job=nxchgDP INfile=",TASKNAME
.end patch 7.8
                    endif
                    APPEND    NEWNAME TO TASKNAME
                    APPEND    " F=default C=",TASKNAME
                    APPEND    str3,TASKNAME
                    APPEND    " B=",TASKNAME
                    APPEND    user TO TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
                     APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
                    APPEND    cntprint TO TASKNAME
                    if (pdfflag = yes)
                              append    " PDF=Y " to taskname
                    endif
                    RESET     TASKNAME
.BEGIN PATCH 7.72   REPLACED LOGIC
.                   Clear     str55
.                   pack      str55 from User," your Job Using ",newname," Has been submitted"
.*********************************************************************
.                   call      OrderSetMouseFree
.*********************************************************************
.                   Alert     note,str55,result
.                   EXECUTE   TASKNAME
...............................................
                    EXECUTE TASKNAME    // new for patch
*********************************************************************
                    call      OrderSetMouseFree
*********************************************************************
.END PATCH 7.72     REPLACED LOGIC
          endif
          return
...........................................................................................................
PrintGo2
.detail report
          call      Report2DestroyObjects
          setprop   Report2,title="NIN Exchange Report"
.begin patch 7.8    
.         create    Report2;StatTextBoxes(1)=50:70:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
.         create    Report2;StatTextBoxes(2)=70:90:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
.         create    Report2;StatTextBoxes(3)=90:110:10:110,"Copies","'>MS Sans Serif'(8)"
.         create    Report2;EditTextBoxes(2)=50:71:80:310,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1,ToolTip="Use Search (F2) to change"
.         create    Report2;EditTextBoxes(3)=70:91:80:310,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1,ToolTip="Use Search (F2) to change"
          create    Report2;StatTextBoxes(1)=40:60:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
          create    Report2;StatTextBoxes(2)=60:80:10:110,"Mailer","'>MS Sans Serif'(8)",ToolTip="Use Search (F2) to change"
          create    Report2;StatTextBoxes(3)=80:100:10:110,"Copies","'>MS Sans Serif'(8)"
          create    Report2;EditTextBoxes(2)=40:61:80:310,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1,ToolTip="Use Search (F2) to change"
          create    Report2;EditTextBoxes(3)=60:81:80:310,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1,ToolTip="Use Search (F2) to change"
          create    Report2;CheckBoxes(5)=100:110:10:80,"PLI Logo"
.end patch 7.8      
          create    Report2;CheckBoxes(1)=120:130:10:80,"Duplex"
          create    Report2;CheckBoxes(2)=140:150:10:80,"PDF"
.begin patch 7.4
          create    Report2;CheckBoxes(3)=160:170:10:110,"Force BegBal"
.Begin Patch 7.94
          create    Report2;CheckBoxes(6)=180:195:10:180,"Excel",TOOLTIP="Will create excel spreadsheet"
.End patch 7.94
          create    Report2;EditTextBoxes(4)=155:175:120:210,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1,ToolTip="Format MM/DD/CCYY"
.end patch 7.4
          create    Report2;EditTextBoxes(1)=90:111:80:110,MaxChars=3,EditType=2,SelectAll=1,Style=1,Border=1
          activate CheckBoxes(1)
          activate CheckBoxes(2)
.begin patch 7.95
           Setitem   checkBoxes(2),0,N1
.end patch 7.95


.begin patch 7.4
          activate CheckBoxes(3)
.end patch 7.4
.Begin Patch 7.94
          activate CheckBoxes(6)
.end patch 7.4
          activate StatTextBoxes(1)
          activate StatTextBoxes(2)
          activate StatTextBoxes(3)
.begin patch 7.4
          activate EditTextBoxes(4)
.end patch 7.4
.begin patch 7.8    
          activate CheckBoxes(5)
          if        (NuseComp = "P")
                    Move      c1,n1
                    Else
                    move      C0,n1
          endif
          setitem   checkBoxes(5),0,n1             ;.set status of logo
..end patch 7.8     
.begin patch 5.1
          setitem   EditTextBoxes(1),1,"001"
.end patch 5.1
          activate EditTextBoxes(1)
          activate EditTextBoxes(2)
          activate EditTextBoxes(3)
          setitem   EditTExtBoxes(2),0,Holdmcomp1
          setitem   EditTExtBoxes(3),0,Holdmcomp2
          setprop   EditTextBoxes(2),ToolTip="Use Search (F2) to change"
          eventreg EditTextBoxes(2),10,PrintGo2KeyPress1,RESULT=N9
          eventreg EditTextBoxes(3),10,PrintGo2KeyPress2,RESULT=N9
..        activate ComboBoxes(2)
          listins   ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),CheckBoxes(1),CheckBoxes(2),CheckBoxes(3):
                    EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3),EditTextBoxes(4),CheckBoxes(4),CheckBoxes(5),CheckBoxes(6)
          setfocus EditTextBoxes(1)
          setprop  report2,visible=1
.begin patch 7.8    
          getitem   checkBoxes(5),0,n1             .PL Logo?
                    if (n1 = c1)
                              move      "P" to LogoFlag
                    else
                              move      NO to LogoFLAG             .needs to get  passed 
                    endif
.end patch 7.8
.begin patch 7.94
          getitem   checkBoxes(6),0,n1             .Excel?
                    if (n1 = c1)
                              move      Yes to ExcelFlag
                    else
                              move      NO to ExcelFLAG             
                    endif
.end patch 7.94

.end patch 7.8      

.begin patch 7.4
.          call      debug
          getitem   checkBoxes(3),0,n1             ;.For Beg Balance on report?
          if (n1 = c1)                      ;.not checked default
                    move      Yes to ForceBegBalance
          else
                    move      No to ForceBegBalance
          endif
          If (ForceBegBalance = Yes)      ;if selected verify date
                    getitem   EditTextBoxes(4),0,str10
                    call      RemoveChar using str10,slash
                    call      TRIM using str10
                    MOVE      STR10 TO STR8
                    if (str10="")
                              alert     caution,"Date Cannot be a null value!",result,"Bad Date"
                              setfocus EditTextBoxes(4)
                              CLEAREVENT
                              return
                    endif
                    CALL      ZFILLIT USING STR8
                    if (str8="00000000")
                              alert     caution,"Date Cannot be a null value!",result,"Bad Date"
                              setfocus EditTextBoxes(4)
                              CLEAREVENT
                              return
                    endif
                    count     N2,str10
                    if (N2 = 0)
                              clear     MM
                              clear     DD
                              clear     CCField
                              clear     YY
                              alert   caution,"Date Must be in MMDDCCYY Format",result
                              setfocus EditTextBoxes(4)
                              ClearEvent
                              Return
                    else
                              if (N2 = 10)
                                        unpack  str10,MM,str1,DD,str1,CCField,YY
                              elseif (N2 = 8)
                                        unpack  str10,MM,DD,CCField,YY
                              elseif (N2 <> 0)
                                        alert   caution,"Order Date Must be in MMDDCCYY Format",result
                                        setfocus EditTextBoxes(4)
                                        ClearEvent
                                        Return
                              endif
                              move    MM,N2
                              if (N2 > "12")
                                        alert   caution,"Invalid Month!",result
                                        setfocus EditTextBoxes(4)
                                        ClearEvent
                                        Return
                              else
                                        move    DD,N2
                                        if (N2 > "31")
                                                  alert   caution,"Invalid Day!",result
                                                  setfocus EditTextBoxes(4)
                                                  ClearEvent
                                                  Return
                                        else
                                                  move    CCField,N2
                                                  if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus EditTextBoxes(4)
                                                            ClearEvent
                                                            Return
                                                  elseif (N2 = "19")
                                                            move    YY,N2
                                                            if (N2 < "80")
                                                                      alert   caution,"Invalid Year!",result
                                                                      setfocus EditTextBoxes(4)
                                                                      ClearEvent
                                                                      Return
                                                            endif
                                                  endif
                                        endif
                              endif
                    endif
                    call    TRIM using MM
                    count   N2,MM
                    if (N2 <> 0 AND MM <> "00")
                              call      cvtjul
                              mOVE      jULDAYS TO bbdATE
                    else
                    endif
          endif
.end patch 7.4
..if rptcan=no  then verify we have 2 mailers and go do job
          If (rptcan = No)
                    call     OrderSetMouseBusy
..Creates Timers for Message Box
                    create   timer1,30
                    activate timer1,tome1,result
..                 create   timer2,60
..                 activate timer2,tome2,result
                    setitem   DIALOGWAITMESSAGE,0,"Processing Your Request.....!!"
                    setprop   DialogWait,visible=1
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM Akey1a,MLR1
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR2
..begin patch 5.1
baddave1
                    if (MLR1 = "" or MLR2 = "")
                              call      OrderSetMouseFree
                              Alert     note,"Sorry you need to select Mailers!",result,"Mailers Needed"
                              return
                    endif
..end patch 5.1
                    CALL      NXNGAIM
                    if over
                              setprop   ErrorMssgStat1,visible=0
                              setprop   ErrorMssgStat2,visible=1
                              setprop   ErrorMssgStat3,visible=0
                              setprop   ErrorMssgStat4,visible=0
                              setprop   ErrorMssgStat5,visible=0
                              setitem   ErrorMssgStat2,0,"Exchange Account not found!"
                              setprop   ErrorMssg,visible=1
..                 call        SetNXCHErrorMssgDefault
..                 setfocus    NxchEditSearchMlr1
                              CLEAREVENT
                              call      OrderSetMouseFree
                              return
                    endif
                    move    c0 to str5
                    packkey nxchfld1 from acckey,str5
                    rep       zfill in nxchfld1
                    MOVE    "01",FILENUM
                    call    Filename
                    call    nxchkey
                    if not over
                              MOVE      LIST TO NDATFLD
                              REP       ZFILL IN NDATFLD
                              MOVE      C1 TO NDATPATH
                              CALL      NDATKEY
                              if over
                                        move      "No List found!" to Olstname
                              endif
.begin patch 7.4
two
                              if (ForceBegBalance = Yes)
                                        unpack    DAT into str2,yy,mm,dd
                                        call      cvtjul
                                        If (Juldays >= BBdate)
                                                  move      yes to BBWriteFlag
.begin patch 7.8
                                                  WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1,logoflag
.                                                 WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1
.end patch 7.8
                                        else
                                                  move      no to BBwriteFlag
                                                  Move      Usage1 to FUsage1
                                                  Move      Usage2 to FUsage2
                                        endif
                              Else
.begin patch 7.8
                                        WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1,logoflag
.                                       WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1
.end patch 7.8
                              endif
                    endif
detailwrt
                    call      nxchks
                    if over
                              weof      recmst,seq
                              close     recmst
                    else
.                If            (lr = "433088")
.                call          debug
.                endif
.BEGIN PATCH 7.7 REPLACED LOGIC
.                   unpack exkey into str8
.                              match  str8 to acckey
                              unpack    exkey into str12
                              match     str12 to acckey
.END PATCH 7.7 REPLACED LOGIC
                              if not equal
                                        weof      recmst,seq
                                        close     recmst
                              else
                                        MOVE      LIST TO NDATFLD
                                        REP       ZFILL IN NDATFLD
                                        MOVE      C1 TO NDATPATH
                                        CALL      NDATKEY
                                        if over
                                                  move      "No List found!" to Olstname
                                        endif
                                        if (ForceBegBalance = Yes & BBWriteFlag = No &( stat = "C"  or Stat = "R"))
                                                  goto Detailwrt
                                        endif
.Following : If we are forcing beb balance date & we have not done it yet and this record is not cancelled or changed to a rental - process
                                        if (ForceBegBalance = Yes & BBWriteFlag = No & stat <> "C"  & Stat <> "R")
                                                  unpack    DAT into str2,yy,mm,dd
                                                  call      cvtjul
                                                  If (Juldays >= BBdate)
                                                            move      yes to BBWriteFlag
.ok let's write the record
                                                            If (mlrsw = "1")                     ;add current qty to correct side
                                                                      add       qty to fusage1
                                                                      move      fusage1 to usage1
                                                            else
                                                                      add       qty to fusage2
                                                                      move      fusage2 to usage2
                                                            endif
.BEGIN PATCH 7.7 REPLACED LOGIC
.                                                      unpack         exkey into str8,str5                 ;fake out print program - make this entry 00000
                                                            unpack    exkey into STR12,str5                 ;fake out print program - make this entry 00000
.END PATCH 7.7 REPLACED LOGIC
                                                            move      "00000" to str5
.BEGIN PATCH 7.7 REPLACED LOGIC
.                                                pack           exkey from str8,str5
                                                            pack      exkey from STR12,str5
.END PATCH 7.7 REPLACED LOGIC
                                                            Clear     Mlrsw                                ;so sort of print file works correctly
                                                  Else                                                ;Nope not ready to write
                                                            If (mlrsw = "1")                     ;add it up
                                                                      add       qty to fusage1
                                                            else
                                                                      add       qty to fusage2
                                                            endif
                                                            goto detailwrt
                                                  endif
                                        endif
.BEGIN PATCH 7.7 REPLACED LOGIC
.                             unpack         exkey into str8,str5                 ;fake out print program - make this entry 00000
                                        unpack    exkey into STR12,str5                 ;fake out print program - make this entry 00000
.END PATCH 7.7 REPLACED LOGIC
                                        match     "00000" to str5
                                        if equal
                                                  clear     mlrsw
                                        endif
three
.begin patch 7.8
                                        WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1,logoflag
.                                       WRITE     RECMST,SEQ;nxchvars,Olstname,HoldMCOMP1
.end patch 7.8
                                        goto detailwrt
                              endif
.end patch 7.4
..here look at printer and etc and submit to batch
                    endif
                    Clear     Taskname
.begin patch 5.1
                    getitem   EditTextBoxes(1),0,str3
                    move      c0 to n3
                    move      str3 to n3
                    if (str3 = "")
                              move      c1 to str3
                    elseif (n3 = c0)
                              call      ordersetmousefree
                              return
                    endif
..end patch 5.1
                    getitem   checkBoxes(1),0,n1
                    move      n1 to duplexFlag
                    getitem   checkBoxes(2),0,N1
                    move      n1 to PDFFlag
.begin patch 7.94
                    If        (ExcelFlag = Yes)
                              append    "\\nins1\winbatch\BUTIL job=nxchgexcel INfile=",TASKNAME

                    Elseif (DuplexFlag <> "1")
.begin patch 7.8
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=nxchg INfile=",TASKNAME
                              append    "\\nins1\winbatch\BUTIL job=nxchg INfile=",TASKNAME
                    else
.                             append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\BUTIL job=nxchgDP INfile=",TASKNAME
                              append    "\\nins1\winbatch\BUTIL job=nxchgDP INfile=",TASKNAME
.end patch 7.8
                    endif
                    APPEND    NEWNAME TO TASKNAME
                    APPEND    " F=default C=",TASKNAME
                    APPEND    str3,TASKNAME
                    APPEND    " B=",TASKNAME
                    APPEND    user TO TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
                    APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
                    APPEND    cntprint TO TASKNAME
                    if (pdfflag = "1")
                              append    " PDF=Y" to taskname
                    endif
                    RESET     TASKNAME
.BEGIN PATCH 7.72   REPLACED LOGIC
..                  Clear     str55
.                   pack      str55 from User," your Job Using ",newname," Has been submitted"
.                   call      OrderSetMouseFree
.                   Alert     note,str55,result
.                   EXECUTE TASKNAME
...........................
                    EXECUTE TASKNAME  // new for patch
                    call      OrderSetMouseFree
.END PATCH 7.72     REPLACED LOGIC
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRintGo2KeyPress1
          if (N9 = 113) ;.F2 Key calls Search Function
.Virtual Key Value
                    move      c4 to MlrSearchFlag
                    goto    SearchGo3
          elseif (N9 = 120)     ;.F9 Key closes Search Function
                    setprop Search,visible=0
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRintGo2KeyPress2
          if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
                    move      c5 to MlrSearchFlag
                    goto    SearchGo3
          elseif (N9 = 120)     .F9 Key closes Search Function
                    setprop Search,visible=0
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintGo3
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintGo4
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchGo
          branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchGo1
..BROKER
..        move    C1,SrchFlag
..        call    SearchSetTitle
..        call    SearchSetVisible
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchGo2
..LIST
..If adding or modifying a record show list view object insted of search form
          if (DetailFlag = YES)
                    if (LSEARCHFLAG = YES)
                              move    C2,SrchFlag
                              call    SearchSetTitle
                              call    SearchSetVisible
                              return
                    endif
                    NXCH0001BListView001B.GetItemCount GIVING n2
.patch7.1
.                   if (N2 > c1)
                    setprop NXCH0001BListView001B,enabled=c1
                    setprop NXCH0001BListView001B,visible=c1
                    setprop NXCH0001BListView001B,height=170
                    return
.         else
.                         Alert note,"Only One List in Xreference for this Mailer!",result,"One List"
.                         return
.                   endif
.EndPatch7.1
..========================================================================
..        else
..                move    C2,SrchFlag
..                call    SearchSetTitle
..                call    SearchSetVisible
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchGo3
..MAILER
          if (DetailFlag <> YES)
                    move    C3,SrchFlag
                    call    SearchSetTitle
                    call    SearchSetVisible
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchGo4
..SHIP-TO
.. not an option with this program
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchLoad
          branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchLoad1
.BEGIN PATCH        7.73      ADDED COMMENT
.         the below will handle campaign and owner selections from search box
.         they will just return also
.END PATCH          7.73      ADDED COMMENT
..BROKER
..- not an option with this program
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchLoad2
..LIST - Ok if adding a detail record
.BEGIN PATCH        7.73      REMOVED LOGIC
          alert note, "this would have been an error", result
.         unpack  Srchstr,str6,str1,str35
.         pack    str25,"LIST: ",str6
.         setitem StatTextBoxes(2),0,str25
.         setitem StatTextBoxes(3),0,str35
.         setitem EditTextBoxes(1),0,str6
.END PATCH          7.73      REMOVED LOGIC
..                  unpack  Srchstr,str6,str1,str35
..                  setitem NXCH0001BEditText004,0,str6
..        Setitem   NXCH0001BStatText005,0,str35
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchLoad3
.MAILER
.check which mailer
.BEGIN PATCH 7.7 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
.              IF    (MlrSearchFlag = c4)
.         move          str4 to mlr1
.         setitem       EditTextBoxes(2),0,str45
.              elseif  (MlrSearchFlag = c5)
.         move          str4 to mlr2
.         setitem       EditTextBoxes(3),0,str45
.              elseif  (MlrSearchFlag = c3)
.         deleteitem ComboBoxes(1),0
.         insertitem ComboBoxes(1),1,str45
.         setitem ComboBoxes(1),0,1
.                    move str4 to mlr1
.              elseif   (MlrSearchFlag = 2)
.                    setitem NxchEditSearchMlr2,0,str4
.         setitem NxchStatSearchMlr2Name,0,str45
.              elseif       (MlrSearchFlag = 1)
.         setitem  NxchEditSearchMlr1,0,str4
.         setitem  NxchStatSearchMlr1Name,0,str45
.         setfocus NxchEditSearchMlr2
.              elseif (MlrSearchFlag = c6)
.              endif
.              move C0 to MlrSearchflag
.....................................
          unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
          IF (MlrSearchFlag = c4)
                    move      str6 to mlr1
                    setitem    EditTextBoxes(2),0,str45
          elseif (MlrSearchFlag = c5)
                    move       str6 to mlr2
                    setitem   EditTextBoxes(3),0,str45
          elseif (MlrSearchFlag = c3)
                    deleteitem ComboBoxes(1),0
                    insertitem ComboBoxes(1),1,str45
                    setitem   ComboBoxes(1),0,1
                    move      str6 to mlr1
          elseif (MlrSearchFlag = 2)
.dh testing can I fill screen 3 at the same time?
                    setitem   NxchEditSearchMlr2,0,str6
                    setitem   NXCH1dStatMMlrName2,0,str45
                    setitem   NXCH1dStatSearchName2,0,str45
                    setitem   NXCH1dEditSearch2,0,str6
                    setitem   NxchStatSearchMlr2Name,0,str45
          elseif (MlrSearchFlag = 1)
                    setitem   NxchEditSearchMlr1,0,str6
                    setitem   NXCH1dStatMMlrName1,0,str45
                    setitem   NXCH1dStatSearchName1,0,str45
                    setitem   NXCH1dEditSearch1,0,str6
                    setitem   NxchStatSearchMlr1Name,0,str45
                    setfocus NxchEditSearchMlr2
          elseif (MlrSearchFlag = c6)
          endif
          move      C0 to MlrSearchflag
.END PATCH 7.7 REPLACED LOGIC
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SearchLoad4
.SHIP-TO - not an option with this program
          return
.
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HelpGo
          if (result <> 2)
                    setprop   AboutMssg,visible=1
                    return
          endif
.         execute   "!c:\progra~1\plus!\micros~1\iexplore.exe http://Web01/programs/ninca/nxch0001.html"
.         if over                .Failed
                    execute   "!c:\progra~1\Intern~1\iexplore.exe http://Web01/programs/ninca/nxch0001.html"
.         endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OptionsGO
          if (result = c5)
                    getprop NXCH0001CListView001,visible=n1
                    if (n1 = c1)
.Verify Password if necessary
                              if (PassFlag = "N")
                                        setitem   PasswordEdit,0,""
                                        setfocus PasswordEdit
                                        move    "E" to progcode
                                        setprop Passwrd,visible=1
                                        if (passflag = "Y")
                                                  move    NPASUSER,str10
                                                  alert   note,"Password Accepted!",result
                                                  setprop NXCH0001CInactivate,visible=c1
                                                  setprop NXCH0001CInactivate,enabled=c1
                                                  return
                                        endif
..Patch6.9
                              elseif (PassFlag = "Y")
..if password was given already avtivate incativate button
                                        setprop NXCH0001CInactivate,visible=c1
                                        setprop NXCH0001CInactivate,enabled=c1
..Endpatch6.9
                              endif
                              if (PassFlag = "N")
                                        setprop NXCH0001CInactivate,visible=c0
                                        setprop NXCH0001CInactivate,enabled=c0
                                        alert   note,"Invalid Password!",result
                                        return
                              endif
                    endif
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.BEGIN PATCH 7.7 REPLACED LOGIC
.NXCHSwitchToTwo
.         if (TabNum <> C1)
.                   if (TabNum = C2)
.                             move    C1,N1
.                   endif
.                   call    NXCHTabClick
.                   move    C1,N1
.                   call    NXCHTabChange
.                   setitem NXCH001aTabControl001,0,2
.         endif
.         return
NXCHSwitchTab LRoutine        FrmPtr
          if (TabNum <> FrmPtr)
                    move      TabNum,N1
                    call      NXCHTabClick
                    move      FrmPtr,N1
                    call      NXCHTabChange
                    setitem   NXCH001aTabControl001,0,FrmPtr
          endif
          return
.END PATCH 7.7 REPLACED LOGIC
..........................................................................................
...........................................................................................
.tab click deactivate the tab page we were on.
NXCHTabClick
          IF (N1 = C1)
                    Deactivate EXCHa
          elseif (N1 = C2 )
                    if (Detailflag = NO)
                              Deactivate EXCHb
                    endif
.BEGIN PATCH 7.7 ADDED LOGIC
          elseif (N1 = C3)
                    Deactivate EXCHd
                    Deactivate EXCHd1
                    Deactivate EXCHd2
.END PATCH 7.7 ADDED LOGIC
          Endif
          return
NXCHTabChange
          IF (N1 = C1)
.                move    C1,TabNum
*********************************************************
.added to make sure user cannot switch screen during detail mod or adding
.did this to allow visiblity changes to work but now changed from vis to enabling buttons
.possible to let switch back an forth but not exit -advise
                    if (DetailFlag = NO)
                              move    C1,TabNum
                              Deactivate EXCHb
                              Activate EXCHa
                    endif
*********************************************************
                    LOOP
                              CLEAREVENT
                              UNTIL OVER
                    REPEAT
          elseif (N1 = C2)
                    move    C2,TabNum
                    Deactivate EXCHa
                    Activate EXCHb
                    Setprop   NXCH0001BButtonNext,visible=c1                    
                    Setprop   NXCH0001BButtonPrev,visible=c1                    
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                    LOOP
                              CLEAREVENT
                              UNTIL OVER
                    REPEAT
.BEGIN PATCH 7.7 ADDED LOGIC
          elseif (N1 = C3)
                    move      C3,TabNum
                    Activate EXCHd
                    if (ISViewFlag = 1)
                              Activate EXCHd1
                    else
                              Activate EXCHd2
                    endif
                    LOOP
                              CLEAREVENT
                              UNTIL OVER
                    REPEAT
.END PATCH 7.7 ADDED LOGIC
          Endif
          return

.BEGIN PATCH 7.7 ADDED LOGIC
NXCHISTabClick
          IF (N1 = C1)
                    Deactivate EXCHd1
          elseif (N1 = C2 )
                    Deactivate EXCHd2
          Endif
          return

NXCHISTabChange
          IF (N1 = C1)
                    Activate EXCHd1
          elseif (N1 = C2)
                    Activate EXCHd2
          Endif
          move      N1,ISViewFlag
          return
.END PATCH 7.7 ADDED LOGIC
.............................................................................................................
.NXCHSetFocusTab
NXCHSetFocusTab
          setitem     NXCH001aTabControl001,0,c2
          setfocus    NXCH001aTabControl001,1
          return
.............................................................................................................
DetailSaveVerify
.BEGIN by verifying require fields
.LR, List Rental number
          CLEAR     STR6
          Getitem   NXCH0001BEditText001,0,str6           .LR number  -- note if in mod mode do not allow change
          count     n1,str6
          if (n1 < 1)
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"LR Number is Required"
                    setprop   ErrorMssg,visible=1
.                  call              SetNXCHErrorMssgDefault
                    setfocus NXCH0001BEditText001
                    CLEAREVENT
                    call      enabletext
                    return
          ENDIF
          Bump      str6 by 6
          reset     str6
          CALL      ZFILLIT USING STR6
.              rep           zfill in str6
*************************************************************************
          if (str6 = "000000")
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"LR Number is Required"
                    setprop   ErrorMssg,visible=1
.                  call              SetNXCHErrorMssgDefault
                    setfocus NXCH0001BEditText001
                    CLEAREVENT
                    call      enabletext
                    return
          endif
************************************************************************
          If (ModFlag = YEs and Str6 <> LR)
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"LR Number cannot be modified"
                    setprop   ErrorMssg,visible=1
..                 call        SetNXCHErrorMssgDefault
                    setitem   nxch0001BeditText001,0,LR
                    setfocus NXCH0001BEditText001
                    call      enabletext
                    CLEAREVENT
                    return
          endif
          move      str6 to LR
..Order Date .NXCH0001BEditText002
          getitem NXCH0001BEditText002,0,str10
          call    RemoveChar using str10,slash
          call    TRIM using str10
          MOVE    STR10 TO STR8
          if (str10="")
                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
                    setfocus NXCH0001BEditText002
                    call      enabletext
                    CLEAREVENT
                    return
          endif
          CALL      ZFILLIT USING STR8
          if (str8="00000000")
                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
                    setfocus NXCH0001BEditText002
                    call      enabletext
                    CLEAREVENT
                    return
          endif
          count   N2,str10
          if (N2 = 0)
                    clear   MM
                    clear   DD
                    clear   CCField
                    clear   YY
                    alert   caution,"Order Date Must be in MMDDCCYY Format",result
                    setfocus NXCH0001BEditText002
                    ClearEvent
                    call      enabletext
                    Return
          else
                    if (N2 = 10)
                              unpack  str10,MM,str1,DD,str1,CCField,YY
                    elseif (N2 = 8)
                              unpack  str10,MM,DD,CCField,YY
                    elseif (N2 <> 0)
                              alert   caution,"Order Date Must be in MMDDCCYY Format",result
                              setfocus NXCH0001BEditText002
                              ClearEvent
                              call enabletext
                              Return
                    endif
                    move    MM,N2
                    if (N2 > "12")
                              alert   caution,"Invalid Month!",result
                              setfocus NXCH0001BEditText002
                              ClearEvent
                              call      enabletext
                              Return
                    else
                              move    DD,N2
                              if (N2 > "31")
                                        alert   caution,"Invalid Day!",result
                                        setfocus NXCH0001BEditText002
                                        ClearEvent
                                        call      enabletext
                                        Return
                              else
                                        move    CCField,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                  alert   caution,"Invalid Year!",result
                                                  setfocus NXCH0001BEditText002
                                                  ClearEvent
                                                  call      enabletext
                                                  Return
                                        elseif (N2 = "19")
                                                  move    YY,N2
                                                  if (N2 < "80")
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus NXCH0001BEditText002
                                                            ClearEvent
                                                            call      enabletext
                                                            Return
                                                  endif
                                        endif
                              endif
                    endif
          endif
          call    TRIM using MM
          count   N2,MM
          if (N2 <> 0 AND MM <> "00")
                    pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
          else
                    clear   Newdate1
          endif
          setitem NXCH0001BEditText002,0,NewDate1
          If (modflag = YES or AddFlag = Yes)
                    pack      Dat from ccfield,yy,mm,dd
          endif
.begin patch 9.72
          if        (addFlag = yes)
          Clock     Timestamp,timestamp
          unpack    timestamp into date1
          endif
.end patch 9.72
.............
.Qty NXCH0001BEditText003
.
          move    c0 to oldqty
          getitem NXCH0001BEditText003,0,str11
          call    RemoveChar using str11,COMMA
          call    Trim using str11
          if ((Str11 = "")|(str11="0"))
                    move      "00000000000",str11
                    Alert     caution,"Invalid Quantity!",result,"Null Quantity"
                    setfocus NXCH0001BEditText003
                    call      enabletext
                    return
          endif
          CLEAR   N9
          move    str11,n9
          IF (N9 < C1)
                    Alert     caution,"Invalid Quantity!",result,"Null Quantity"
                    setfocus NXCH0001BEditText003
                    call      enabletext
                    return
          ENDIF
          if (MODFLAG = yes)
                    move      qty to n10
                    if (n9 <> n10)
                              Move   Yes to Adjustflag
                              move   qty to oldqty
                    endif
          endif
          move      n9 to qty
..List number NXCH0001BEditText004
          getitem NXCH0001BEditText004,0,str6
          call    Trim using str6
          if (str6 <> "")
                    call    ZFILLIT using str6,C0
          else
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"List Number not valid!"
                    setprop   ErrorMssg,visible=1
..                 call        SetNXCHErrorMssgDefault
                    setfocus NXCH0001BEditText004
                    CLEAREVENT
                    call      enabletext
                    return
          endif
          pack    NDATFLD,str6
          rep     zfill,NDATFLD
          move    C1,NDATPATH
          move    "ExchangeDetailVerf-NDATKEY",Location
          pack    KeyLocation,"Key: ",NDATPATH
          call    NDATKEY
          if over
                    setprop   ErrorMssgStat1,visible=0
                    setprop   ErrorMssgStat2,visible=1
                    setprop   ErrorMssgStat3,visible=0
                    setprop   ErrorMssgStat4,visible=0
                    setprop   ErrorMssgStat5,visible=0
                    setitem   ErrorMssgStat2,0,"List Number not valid!"
                    setprop   ErrorMssg,visible=1
.                  call              SetNXCHErrorMssgDefault
                    setfocus NXCH0001BEditText004
                    CLEAREVENT
                    call      enabletext
                    return
          else
                    setitem NXCH0001BStatText005,0,OLSTNAME
                    move        str6 to List
          endif
....................................
.comments NXCH0001BEditText005
          getitem   NXCH0001BEditText005,0,XCHCOMNT
.begin patch 9.71
          getitem   NXCH0001BEditText007,0,str10
          call    RemoveChar using str10,slash
          call    TRIM using str10
          MOVE    STR10 TO STR8
          if (str10="")
                    goto      ExitMdate
                    alert     caution,"Mail Date Cannot be a null value!",result,"Bad Date"
                    setfocus NXCH0001BEditText007
                    call      enabletext
                    CLEAREVENT
                    return
          endif
          CALL      ZFILLIT USING STR8
          if (str8="00000000")
                    alert     caution,"Mail Date Cannot be a null value!",result,"Bad Date"
                    setfocus NXCH0001BEditText007
                    call      enabletext
                    CLEAREVENT
                    return
          endif
          count   N2,str10
          if (N2 = 0)
                    clear   MM
                    clear   DD
                    clear   CCField
                    clear   YY
                    alert   caution,"Mail Date Must be in MMDDCCYY Format",result
                    setfocus NXCH0001BEditText007
                    ClearEvent
                    call      enabletext
                    Return
          else
                    if (N2 = 10)
                              unpack  str10,MM,str1,DD,str1,CCField,YY
                    elseif (N2 = 8)
                              unpack  str10,MM,DD,CCField,YY
                    elseif (N2 <> 0)
                              alert   caution,"Mail Date Must be in MMDDCCYY Format",result
                              setfocus NXCH0001BEditText007
                              ClearEvent
                              call enabletext
                              Return
                    endif
                    move    MM,N2
                    if (N2 > "12")
                              alert   caution,"Invalid Month!",result
                              setfocus NXCH0001BEditText007 
                              ClearEvent
                              call      enabletext
                              Return
                    else
                              move    DD,N2
                              if (N2 > "31")
                                        alert   caution,"Invalid Day!",result
                                        setfocus NXCH0001BEditText007
                                        ClearEvent
                                        call      enabletext
                                        Return
                              else
                                        move    CCField,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                  alert   caution,"Invalid Year!",result
                                                  setfocus NXCH0001BEditText007
                                                  ClearEvent
                                                  call      enabletext
                                                  Return
                                        elseif (N2 = "19")
                                                  move    YY,N2
                                                  if (N2 < "80")
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus NXCH0001BEditText007
                                                            ClearEvent
                                                            call      enabletext
                                                            Return
                                                  endif
                                        endif
                              endif
                    endif
          endif
          call    TRIM using MM
          count   N2,MM
          if (N2 <> 0 AND MM <> "00")
                    pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
          else
                    clear   Newdate1
          endif
          setitem NXCH0001BEditText007,0,NewDate1
          If (modflag = YES or AddFlag = Yes)
                    pack      DateM from ccfield,yy,mm,dd
          endif
ExitMdate          
.end patch 9.71
.........................
.if add mode add where?
CheckSwitch
          IF (Yes = addFlag)
                    getitem   NXCH0001BComboBox001,0,n1
                    if (n1 = c1)
                              move      c1 to mlrsw           addflag to mailer 1 side
                              move      c1 to WhichMlr
                    else
                              move      c2 to Mlrsw           .add flag to mailer 2 side
                              move      c2 to WhichMlr
                    endif
                    if (n1 <> c1 and n1 <> c2)
                              setprop   ErrorMssgStat1,visible=0
                              setprop   ErrorMssgStat2,visible=1
                              setprop   ErrorMssgStat3,visible=0
                              setprop   ErrorMssgStat4,visible=0
                              setprop   ErrorMssgStat5,visible=0
                              setitem   ErrorMssgStat2,0,"Add Where????!"
                              setprop   ErrorMssg,visible=1
.                            call              SetNXCHErrorMssgDefault
                              setfocus NXCH0001BComboBox001
                              CLEAREVENT
                              return
                    endif
          endif
.........................
          cmatch    YES to AddFlag
          if equal
                    move      lr to newlr
                    move      dat to newdat
                    move      list to newlist
                    move      XCHCOMNT to NewXCHCOMNT
                    move      qty to newqty
                    move      inits to newtype
                    move      "NXNGAIM",Location
                    pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                    call      NXNGAIM
                    move      ENTRY,BLANK5
Xloop
                    add       C1,ENTRY        .yes do it
..              move    "EXACC-NXNGUPD",Location
..              pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                    call      NXNGUPD
                    pack      NXCHFLD1,AccKEY,BLANK5
                    rep       zfill in nxchfld1
                    move      "ExAcLoop-NXCHKEY",Location
                    pack      KeyLocation,"Key: ",NXCHFLD1
                    call      NXCHKEY
                    if over                                 .try to reset counter
                              goto COUNTERR
                    else
                              clear     blank5
                              move      entry to blank5
                              rep       zfill in blank5
                              pack      NXCHFLD1,AccKEY,BLANK5
                              call      nxchtst
                              if not over
                                        goto xloop
                              endif
**************************************************************
*Mlrsw gets changed above with previous read -must reset it before write
                              If (whichmlr = c1)
                                        move      c1 to mlrsw
                                        add       newqty to usage1
                              elseif (whichmlr = c2)
                                        move      c2 to mlrsw
                                        add       newqty to usage2
                              endif
*****************************************************************
                              move      nxchfld1 to exkey
                              move      Newlr to lr
                              move      Newdat,dat
                              move      newlist,list
                              move      NewXCHCOMNT,XCHCOMNT
                              move      newqty,qty
                              move      newtype,type
                              clear     stat
                              call      nxchwrt
                              call      NXCHClearListView
                              call      NXCHLoadListView
                              call      NxchSetFocusTab
                    endif
          endif
.
MODIFYIT
          cmatch   YES to ModFlag
          if equal
                    move     inits to type
                    cmatch   YES to AdjustFlag
                    if equal
                              Call      AdjustUpdate
                    else
                              CALL      NXCHUpd
                    endif
                    call      NXCHClearListView
                    call      NXCHLoadListView
                    call      NxchSetFocusTab
          endif
          cmatch    yes to CancelFlag
          if equal
                    cmatch    YES to AdjustFlag
                    if equal
                              move      "C" to stat             .cancel and adjust
                              Call      AdjustUpdate
                    else
                              move      "X" to STAT             .Cancelled no adjust
                              CALL      NXCHUpd
                    endif
                    call      NXCHClearListView
                    call      NXCHLoadListView
                    call      NxchSetFocusTab
          endif
          cmatch    yes to RentalFlag
          if equal
                    cmatch    YES to AdjustFlag
                    if equal
                              move      "R" to stat             .Rental and adjust  .currently forced to true
                              Call      AdjustUpdate
                    else
                              move      "X" to STAT             .Rental no adjust  . not currently allowed
                              CALL      NXCHUpd
                    endif
                    call      NXCHClearListView
                    call      NXCHLoadListView
                    call      NxchSetFocusTab
          endif
**************************
.Patch7.0
          cmatch    yes to ReinstateFlag
          if equal
                    if (stat = "R")
                              move      " " to stat
                              call      NXCHUPD
                              Call      AdjustUpdate
                    endif
                    if (stat = "C")
                              move      " " to stat
                              call      NXCHUPD
                              Call      AdjustUpdate
                    endif
                    if (stat = "X")
                              move      " " to stat
                              call      NXCHUPD
                    endif
                    call      NXCHClearListView
                    call      NXCHLoadListView
                    call      NxchSetFocusTab
          endif
..subpatch7.0
*****************************************************
          setprop  colbutt,enabled=c1
          setprop  NXCH0001BButtonSave,enabled=0
          setprop  NXCH0001BButtonQuit,enabled=0
          move     NO to DetailFlag
          move     YES to ExitFlag
          setprop  NXCH0001BComboBox001,enabled=c0
..               move   NO to MODFlag
..               move   NO to ADDFlag
..               move   NO to CancelFlag
..               move   NO to RentalFlag
****************************************************
          setitem   Nxch001aTabControl001,0,c1
          move      No to CancelFlag
          move      No to AdjustFlag
          move      No to RentalFlag
          move      No to AddFlag
          move      No to Modflag
.Patch7.0
          move      No to REINSTATEFLAG
.EndPatch7.0
.              setprop       NXCH0001BComboBox001,visible=0
          return
.............................................................................................................
AdjustUpdate
          MOVE      Inits TO TYPE
          REP       zfill,LIST
          MOVE      EXKEY TO NXCHFLD1
          MOVE      C1 TO NXCHPATH
****************************************************************
.BEGIN PATCH 7.7 REPLACED LOGIC
.         unpack  exkey into str8,hentry
          unpack  exkey into STR12,hentry
.END PATCH 7.7 REPLACED LOGIC
.         move      ENTRY to HENTRY
****************************************************************
          CALL      NXCHUPD
********************************************************
.May comment this back in but I want to retain stat value after update
.         MOVE      B1,STAT
********************************************************
          MOVE      QTY,WORK9
          Move      Mlrsw to MlrFlag              .lets keep track of which side to adjust
          UNPACK    EXKEY INTO ACCKEY
          UNPACK    ACCKEY INTO ACKEY1X,ACKEY2X
          PACK      NXNGFLD1 FROM AKEY1a,ACKEY1X
          PACK      NXNGFLD2 FROM AKEY2a,ACKEY2X
          clear     deadflag
          CALL      NXNGAIM
          move      flag to deadflag
          MOVE      ENTRY TO SaveEntry
****************************************************************
          move      HENTRY to ENTRY
****************************************************************
          CLEAR     EXKEY
          PACK      EXKEY FROM ACCKEY,ENTRY
          REP       zfill,EXKEY
          MOVE      EXKEY TO NXCHFLD1
..Subtract oldqty from modified qty to get difference to add to usage below
          move      oldqty to calcuse
..patch7.0
          if (reinstateflag = YES)
                    goto AdjustLoop
          endif
..EndPatch7.0
          IF ((STAT="R")|(STAT="C"))
                    SUB       WORK9  FROM calcuse
                    move      calcuse to  WORK9
          Else
                    SUB       calcuse FROM WORK9
          Endif
AdjustLoop
          call   nxchkey
          GOTO   Adjustloop1 IF OVER                .hopefully means deleted records ( a gap)
.*************************************************************
..Make sure correct usage is adjusted
..Should only adjust usage for the mailer being cancelled
..              move mlrsw to mlrflag
.*************************************************************
          if (mlrFlag = "1")
.**************************************************************
..Added to correctly move new qty and add usage correctly
..                            move      oldqty to calcuse
..                                SUB             calcuse FROM WORK9
.**************************************************************
...                            if ((STAT="R")|(STAT="C"))
..Will subtract old qty if Rental or Cancel Adjusted
...                                    SUB WORK9 FROM USAGE1
...                            else
..Add difference of modified value from above
                    ADD       WORK9 TO USAGE1
..                            endif
          else
.**************************************************************
..Added to correctly move new qty and add usage correctly
..                            move      oldqty to calcuse
..                                SUB             calcuse FROM WORK9
.**************************************************************
..                            if ((STAT="R")|(STAT="C"))
..Will subtract old qty if Rental or Cancel Adjusted
..                                    SUB WORK9 FROM USAGE2
..                            else
..Add difference of modified value from above
                    ADD       WORK9 TO USAGE2
..                            Endif
          endif
          CALL      NXCHUPD
          COMPARE   ENTRY TO SaveEntry
          Return IF EQUAL
AdjustLoop1
          ADD       c1 TO ENTRY
          PACK      EXKEY FROM ACCKEY,ENTRY
          rep       zfill in exkey
          MOVE      EXKEY TO NXCHFLD1
          GOTO AdjustLoop
.............................................................................................................
XREpSortListView
..Dynamically sorts Different ListViews.
..In order to switch between different ListViews we need two pieces of information.
..We need to ascertain which column was clicked AND which ListView we currently
..have visible, as each ListView has its' columns ordered differently.
..Getprops will determine which ListView is currently active, #EventResult passed to result
..prior to calling this subroutine will determine which column was clicked.
..-        getprop Nxch0001CListView001,visible=N9
..-        if (N9 = C1)    .Nxch0001CListView001 is visible
..if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
..-                If (result <> 1 and result <> 2)  .no click?
..-                        setprop Nxch0001CListView001,visible=1
..-                        setprop Nxch0001CListView003,visible=0
..-                        setprop Nxch0001CListView002,visible=0
..-                          Nxch0001CListView001.EnsureVisible using c1,0
..-                          setfocus Nxch0001Clistview001
..-                endif
          if (result = 0)
                    Nxch0001CListView001.SortColumn using *Column=0,*Type=1
..-                        setprop Nxch0001CListView001,visible=0
..-                        setprop Nxch0001CListView003,visible=0
..-                        setprop Nxch0001CListView002,visible=1
..-                         Nxch0001CListView002.EnsureVisible using c1,0
..-                        setfocus Nxch0001Clistview002
          Elseif (result = 1)
                    Nxch0001CListView001.SortColumn using *Column=7,*Type=2
          Elseif (result = 2)
                    Nxch0001CListView001.SortColumn using *Column=8,*Type=2
..-                        setprop Nxch0001CListView001,visible=0
..-                        setprop Nxch0001CListView003,visible=1
..-                        setprop Nxch0001CListView002,visible=0
..-                         Nxch0001CListView003.EnsureVisible using c1,0
..-                        setfocus Nxch0001Clistview003
          Elseif (result = 3)
                    Nxch0001CListView001.SortColumn using *Column=9,*Type=2
          endif
..        else
..-                getprop Nxch0001CListView002,visible=N9
..-                if (N9 = C1)    .Nxch0001CListView002 is visible
..if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
..-                        if (result <> 1 and result <> 3)
..-                                setprop Nxch0001CListView001,visible=0
..-                                setprop Nxch0001CListView002,visible=2
..-                                setprop Nxch0001CListView003,visible=0
..-                         Nxch0001CListView002.EnsureVisible using c1,0
..-                         setfocus Nxch0001Clistview002
..-                        Elseif (result = 1 ) .mailer order
..-                                setprop Nxch0001CListView001,visible=1
..-                                setprop Nxch0001CListView002,visible=0
..-                                setprop Nxch0001CListView003,visible=0
..-                         Nxch0001CListView001.EnsureVisible using c1,0
..-                               setfocus Nxch0001Clistview001
..-                        Elseif (result = 3 )
..-                                setprop Nxch0001CListView001,visible=0
..-                                setprop Nxch0001CListView002,visible=0
..-                                setprop Nxch0001CListView003,visible=1
..-                                   Nxch0001CListView003.EnsureVisible using c1,0
..-                               setfocus Nxch0001Clistview003
..-                       endif
..-         else
..if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
..-                        if (result <> 1 and result <> 2 )
..-                                setprop Nxch0001CListView003,visible=1
..-                                setprop Nxch0001CListView002,visible=0
..-                                setprop Nxch0001CListView003,visible=0
..-                         Nxch0001CListView003.EnsureVisible using c1,0
..-                         setfocus Nxch0001Clistview003
..-                        Elseif (result = 1 )   .mailer order
..-                                setprop Nxch0001CListView001,visible=1
..-                                setprop Nxch0001CListView002,visible=0
..-                                setprop Nxch0001CListView003,visible=0
..-                                   Nxch0001CListView001.EnsureVisible using c1,0
..-                               setfocus Nxch0001Clistview001
..-                        Elseif (result = 2 )      .owes order
..-                                setprop Nxch0001CListView001,visible=0
..-                                setprop Nxch0001CListView002,visible=1
..-                                setprop Nxch0001CListView003,visible=0
..-                                   Nxch0001CListView002.EnsureVisible using c1,0
..-                               setfocus Nxch0001Clistview002
..-                       endif
..-               endif
..-        endif
          return
.........................................................................................................................
..must arrive with mailer in Mnum
GetListUniverse
          clear     HoldxList
          move      c2 to nxrfpath
          clear     nxrffld2
          clear     universe
.BEGIN PATCH 7.5 REPLACED LOGIC
.              move       c0 to n9
.              move       c0 to N9A
          move      c0 to n10
          move      c0 to N10A
.END PATCH 7.5 REPLACED LOGIC
.BEGIN PATCH 7.7 REPLACED LOGIC
.         move      mnum to nxrffld2
          move      COMPNUM to nxrffld2
.END PATCH 7.7 REPLACED LOGIC
          rep       zfill in nxrffld2
          move      c1 to umbrflag
          call      nxrfkey
          if not over
                    clear     ndatfld
                    clear     universe
.BEGIN PATCH 7.5 REPLACED LOGIC
.         move       c0 to n9
                    move      c0 to n10
.END PATCH 7.5 REPLACED LOGIC
                    move      nxrflist to ndatfld
                    move      nxrflist to HoldXList
                    move      c1 to ndatpath
                    rep       zfill in ndatfld
           if         (ndatfld = "005543")
           call       debug
           endif
                    call      ndatkey
                    goto uniexit if over
                    CMATCH    b1  TO status
                    goto moreuni if not equal
.BEGIN PATCH 7.5 REPLACED LOGIC
.         move      c0 to n9
.         move      universe to n9
.         add       n9 to n9a
                    move      c0 to n10
.begin patch 7.97
                    call      trim using universe  
                    call      zfillit using universe

.end patch 7.97
                    move      universe to n10
                    add       n10 to n10a
.END PATCH 7.5 REPLACED LOGIC
moreuni
                    call      nxrfks
                    if not over                .an umbrella
                              match     nxrffld2 to nxrfmlr
                              goto uniexit if not equal
..                move      c2 to umbrflag          *yes an umbrella
                              move      c1 to ndatpath
                              move      nxrflist to ndatfld
                              move      nxrflist to HoldXList
                              rep       zfill in ndatfld
                              clear     universe
                              call      ndatkey
                              CMATCH    b1  TO status
                              goto moreuni if not equal
.BEGIN PATCH 7.5 REPLACED LOGIC
.         move      c0 to n9
.         move      universe to n9
.         add       n9 to N9A
                              move      c0 to n10
                              move      universe to n10
                              add       n10 to N10A
.END PATCH 7.5 REPLACED LOGIC
                              goto moreuni
                    endif
          else
                    move      "         0" to universe
                    clear     nxrflist
          endif
uniexit
          move      "         0" to universe
.BEGIN PATCH 7.5 REPLACED LOGIC
.         move       N9A to Universe
          move      N10A to Universe
.END PATCH 7.5 REPLACED LOGIC
          move      HOLDXLIST to nxrflist
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Timeout
          beep
          beep
          beep
          winshow
          stop
.......................................................................................................
COUNTERR
          beep
          beep
          clear   taskname
          append  "Counter Error Running BAL Not Found ",taskname
          append  ackey,taskname
          append  B1,taskname
          append  NXCHFLD1,taskname
          append  Carr,taskname         .Carriage Return
          append  "Inform Computer Personel!!!!!",taskname
          alert   note,taskname,result
.BEGIN PATCH 7.7 REPLACED LOGIC
.        unpack  ACKEY INTO str4,str5
.        pack    NXNGFLD1 FROM AKey1A,str4
.        pack    NXNGFLD2 FROM AKey2A,str5
          unpack  ACKEY INTO str6B,str6a
          pack    NXNGFLD1 FROM AKey1A,str6B
          pack    NXNGFLD2 FROM AKey2A,str6a
.END PATCH 7.7 REPLACED LOGIC
          clear   error
          append  "XCH Add counterr ",error
          append  ackey,error
          append  B1,error
          append  NXCHFLD1,error
          reset   error
.begin patch 7.9
          Move      "This is an Error e-mail from Nxch0001",MailSubjct
          Clear     Mailbody
          append    "This is an error message",Mailbody
          append    "<br>",mailbody
          append    Error,Mailbody
          append    "<br>",mailbody
          append    User,Mailbody
          append    "<br>",mailbody
          append    "Subroutine Counterr",Mailbody
          append    "<br>",mailbody
          reset     Mailbody
          
.         Move    "This is an Error e-mail from Nxch0001",SmtpSubject Subject
..   Set the text message that is send with the attachments
.         Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
.         Move    error,SmtpTextMessage(2)   Array <Text message >
.         Move    "Subroutine Counterr",SmtpTextMessage(3)   Array <Text message >
.         Move    "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
.end patch 7.9
          call    errmesg
          Alert      note,"Don't know what to do!",result
          winshow
          stop
          return
LoadLists
          move      c2 to nxrfpath
          clear     n2
          getitem   NXCH0001BComboBox001,n2,n1
..If mailer in combobox is mailer one then check the cross reference file for
..The mailer two's list cross reference
..Don't think to hard!
Loadview
          NXCH0001BListView001B.DeleteAllItems giving N9
.BEGIN PATCH 7.7 REPLACED LOGIC
.         load     str4 with n1,mlr2,mlr1
.         move     str4 to NXRFFLD2
          load      str6B with n1,mlr2,mlr1
          move      str6B to NXRFFLD2
.END PATCH 7.7 REPLACED LOGIC
          call      NXRFKEY
          loop
.BEGIN PATCH 7.7 REPLACED LOGIC
.                  match  NXRFMLR to str4
                    match     NXRFMLR to str6B
.END PATCH 7.7 REPLACED LOGIC
                    if equal
                              move      nxrflist to NDATFLD
                              call      NDATKEY
                              If (STATUS = " ")
                                        NXCH0001BListView001B.InsertItem giving N9 using LSTNUM
                                        move      NXRFLIST to str6B
                                        NXCH0001BListView001B.SetItemText using N9,OLSTNAME,1
                                        move      OLSTNAME to str35
                              Endif
                    endif
                    call     NXRFKS
                    until over
          repeat
..Not sure on what list should actually be shown first???
          NXCH0001BListView001B.GetItemCount GIVING n2
          if (N2 < c1)
                    Alert     note,"No List in Cross Reference or list withdrawn!",result,"No XREF"
                    call      ordersetmousefree
                    return
          endif
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MoveList
..load the list edit text box in the Detail Screen
          NXCH0001BListView001B.getnextitem giving n5 using c2,seq
..        NXCH0001BListView001B.getnextitem giving n5 using c2,c0
          NXCH0001BListView001B.getitemtext giving str35 using n5,c1
          Setitem NXCH0001BStatText005,0,str35
          NXCH0001BListView001B.getitemtext giving str6 using n5,c0
          setitem NXCH0001BEditText004,0,str6
          setprop NXCH0001BListView001B,height=0
          setprop NXCH0001BListView001B,enabled=c0
          setprop NXCH0001BListView001B,visible=c0
          return
...LOADDETAILLIST
...       NXCH0001BListView001A.DeleteAllItems giving N9
...       NXCH0001BListView001B.DeleteAllItems giving N9
...       clear n1
...        move c1 to n1
...DetLoop
...        move c2 to nxrfpath
...        load     str4 with n1,mlr1,mlr2
...       move     str4 to NXRFFLD2
...        call     NXRFKEY
...       loop
..                  call     NXRFKS
..        until over
...               match  NXRFMLR to str4
...                 if equal

...                        move nxrflist to NDATFLD
...                         move c1 to ndatpath
...                        call NDATKEY
...                        If (STATUS = " ")

...                                if (n1 = c1)
...                                               NXCH0001BListView001A.InsertItem giving N9 using LSTNUM
..                                move NXRFLIST to str6
...                                               NXCH0001BListView001A.SetItemText using N9,OLSTNAME,1
..                          move OLSTNAME to str35
...                                else

...                                               NXCH0001BListView001B.InsertItem giving N9 using LSTNUM
..                                move NXRFLIST to str6
...                                               NXCH0001BListView001B.SetItemText using N9,OLSTNAME,1
..                          move OLSTNAME to str35
...                                Endif
...                        else
..                                goto LoadView
...                        Endif
...               endif
...                 call     NXRFKS
...        until over
...       repeat
...        add c1 to n1

...        if (n1=c3)
...               Return
...        else
...                goto Detloop
...        endif
...        return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FirstDetailList
          move      c2 to nxrfpath
          clear     n2
          getitem   NXCH0001BComboBox001,n2,n1
..If mailer in combobox is mailer one then check the cross reference file for
..The mailer two's list cross reference
..Don't think to hard!
.BEGIN PATCH 7.7 REPLACED LOGIC
.        load     str4 with n1,mlr2,mlr1
.        move     str4 to NXRFFLD2
          load      str6B with n1,mlr2,mlr1
          move      str6B to NXRFFLD2
.END PATCH 7.7 REPLACED LOGIC
          call      NXRFKEY
          if over
                    call      EXRefVerify
.BEGIN PATCH 7.7 REPLACED LOGIC
.         pack    MKEY,NXRFFLD2,"000"
.         call    Nmlrkey
.         pack    taskname,NXRFFLD2,b1,dash,b1,mcomp
                    pack      COMPFLD,NXRFFLD2
                    move      "FirstDetailList-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    pack    taskname,NXRFFLD2,b1,dash,b1,COMPCOMP
.END PATCH 7.7 REPLACED LOGIC
                    setitem StatTextBoxes(5),0,taskname
                    MOVE    YES TO LSEARCHFLAG
                    setprop Report2,visible=1
                    MOVE    NO TO LSEARCHFLAG
                    alert   caution,"Xreference file not updated for this Mailer!",result,"XREFERENCE"
                    setitem NXCH0001BEditText004,0,""
                    setitem NXCH0001BStatText005,0,""
                    return
          endif
          call      ordersetmousebusy
          loop
.BEGIN PATCH 7.7 REPLACED LOGIC
.         match  NXRFMLR to str4
                    match     NXRFMLR to str6B
.END PATCH 7.7 REPLACED LOGIC
                    if equal
                              move      nxrflist to NDATFLD
                              call      NDATKEY
                              If (STATUS = " ")
                                        move      NXRFLIST to str6
                                        move      OLSTNAME to str35
                                        setitem NXCH0001BEditText004,0,str6
                                        Setitem   NXCH0001BStatText005,0,str35
                                        call      ordersetmousefree
                                        return
.dave goes bad                                        
                              Elseif    (status = "W" or Status = "T")
                                                            pack      str255 from "List is Withdrawn, ",CRLF,"Use anyway? ",crlf:
                                                            "Yes = you fixed, No=try again, Canncel= abort"                 
                                                            ALERT    PLAIN,str255,RESULT
                                                            IF       (RESULT = 1)
                                                                      ALERT    NOTE,"YES was pressed.",RESULT
                                        move      NXRFLIST to str6
                                        move      OLSTNAME to str35
                                        setitem NXCH0001BEditText004,0,str6
                                        Setitem   NXCH0001BStatText005,0,str35
                                        call      ordersetmousefree
                                        return
                                                            ELSEIF   (RESULT = 2)
                                                                      ALERT    NOTE,"NO was pressed.",RESULT
                                                            ELSEIF  (RESULT = 3)
                                                                      ALERT   NOTE,"CANCEL was pressed.",RESULT
                                                            ENDIF

.dave goes bad                                        
                              Endif
                    Endif
                    call      NXRFKS
                    until over
          repeat
..Not sure on what list should actually be shown first???
..                            NXCH0001BListView001B.GetItemCount GIVING n2
..                            if (N2 < c1)
          call      ordersetmousefree
          Alert     note,"No List in Cross Reference or list withdrawn!",result,"No XREF"
          return
..                        endif
..                  setitem NXCH0001BEditText004,0,str6
..                            Setitem   NXCH0001BStatText005,0,str35
...         getitem  NXCH0001BComboBox001,n2,n1
...        if (n1=c1)
...                 NXCH0001BListView001A.getnextitem giving n3 using c0,seq
...                 NXCH0001BListView001A.getitemtext giving str35 using n3,c1
...                 Setitem NXCH0001BStatText005,0,str35
...                 NXCH0001BListView001A.getitemtext giving str6 using n3,c0
...               setitem NXCH0001BEditText004,0,str6
...        else
...                 NXCH0001BListView001B.getnextitem giving n3 using c0,seq
...                 NXCH0001BListView001B.getitemtext giving str35 using n3,c1
...                 Setitem NXCH0001BStatText005,0,str35
...                 NXCH0001BListView001B.getitemtext giving str6 using n3,c0
...               setitem NXCH0001BEditText004,0,str6
...        endif
...        return
..======================================================================
DUMMY
          getitem   NXCH0001BEditText001,0,str6
          rep       "dD" in str6
          call      trim using str6
          match     "D",str6
          IF equal
                    MOVE      "NXCHDUM" TO GNXTFLD
                    CALL      GNXTKEY
                    MOVE      GNXTNUM TO str6
                    setitem   NXCH0001BEditText001,0,str6
                    move      gnxtnum to dumnum
                    MOVE      dumnum TO str1
                    BUMP      dumnum BY 1
                    MOVE      dumnum TO Ndumnum
                    RESET     dumnum
                    ADD       c1 to ndumnum
                    PACK      dumnum FROM str1,Ndumnum
                    rep       zfill,dumnum
                    move      dumnum to gnxtnum
                    move      "NXCHDUM" to GNXTKEY
                    call      gnxtupd
          endif
          return
.+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Enabletext
..ADD
          if (addflag=YES)
****************************************************
                    Setprop  NXCH0001BEditText001,readonly=0
                    setprop  NXCH0001BComboBox001,enabled=c1
                    Setprop  NXCH0001BEditText002,readonly=0
                    Setprop  NXCH0001BEditText003,readonly=0
                    Setprop  NXCH0001BEditText004,readonly=0
                    Setprop  NXCH0001BEditText005,readonly=0
.begin patch 7.92
                    Setprop  NXCH0001BEditText007,readonly=0
.end patch 7.92
                    return
          endif
****************************************************
.Mod
****************************************************
          if (modflag=YES)
                    Setprop  NXCH0001BEditText002,readonly=0
                    Setprop  NXCH0001BEditText003,readonly=0
                    Setprop  NXCH0001BEditText004,readonly=0
                    Setprop  NXCH0001BEditText005,readonly=0
.begin patch 2.97
                    Setprop  NXCH0001BEditText007,readonly=0
.end patch 2.97
                    return
          endif
***************************************************
.CANCEL
          if (cancelflag = YES)
                    move      Yes to Cancelflag
                    setprop   NXCH0001BStatText012,height=20
                    setprop   NXCH0001BCheck001,height=20
                    setfocus NXCH0001BCheck001
                    setprop   ColButt,enabled=c0
                    setprop   NXCH0001BButtonSave,enabled=1
                    setprop   NXCH0001BButtonQuit,enabled=1
                    setprop  NXCH0001BButtonSave,visible=1
                    setprop  NXCH0001BButtonQuit,visible=1
                    move      YES to DetailFlag
                    move      NO to ExitFlag
                    Return
          endif
****************************************************
.Rental
          if (rentalflag = YES)
                    setprop  NXCH0001BStatText012,height=20
                    setprop  NXCH0001BCheck001,height=20
                    setprop   ColButt,enabled=c0
                    setprop  NXCH0001BButtonSave,enabled=1
                    setprop  NXCH0001BButtonQuit,enabled=1
                    setprop  NXCH0001BButtonSave,visible=1
                    setprop  NXCH0001BButtonQuit,visible=1
                    move      YES to DetailFlag
                    move      NO to ExitFlag
                    setfocus NXCH0001BCheck001
                    setItem  NXCH0001BCheck001,0,1
                    Return
          endif
****************************************************
..Reinstatement
.Patch7.0
          if (reinstateflag = YES)
                    setprop   ColButt,enabled=c0
                    setprop  NXCH0001BButtonSave,enabled=1
                    setprop  NXCH0001BButtonQuit,enabled=1
                    setprop  NXCH0001BButtonSave,visible=1
                    setprop  NXCH0001BButtonQuit,visible=1
                    move      YES to DetailFlag
                    move      NO to ExitFlag
                    Return
          endif
          return
..EndPatch7.0
CrossCheck
***********************************************************************************
.added to check if either list is a rental only list
          for n7,c1,c2
.BEGIN PATCH 7.7 REPLACED LOGIC
.                        load           str4 with n7,mlr1,mlr2
.                        clear          mkey
.                        packkey        mkey from str4,z3
.                        call           nmlrkey
..................
                    load      str6B with n7,mlr1,mlr2
                    clear     COMPFLD
                    packkey   COMPFLD,str6B
                    move      "CrossCheck-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
.END PATCH 7.7 REPLACED LOGIC
                    if over
                              alert caution,"Unable to locate mailer!",result,"No Mailer found"
                              call   ADDBEGABORT
..                                       setprop colbutt,enabled=c0
..                                       setprop NxchEditSearchMlr1,enabled=c1
..                                            setprop NxchEditSearchMlr2,enabled=c1
..                                            setprop NXCH0001BButtonAdd,enabled=c1
..                                        move     NO to DetailFlag
..                                                move     YES to ExitFlag
                              return
                    else
                              If (MRCODE="R")
.BEGIN PATCH 7.7 REPLACED LOGIC
.                                        pack  str55,"This is a rental only client!  ","Mailer: ",mkey
                                        pack  str55,"This is a rental only client!  ","Mailer: ",COMPFLD
.END PATCH 7.7 REPLACED LOGIC
                                        alert caution,str55,result,"Rental Only"
..                                        alert caution,"This is a rental only client!",result,"Rental Only"
                                        call   ADDBEGABORT
..                                        setprop colbutt,enabled=c0
..                                        setprop NxchEditSearchMlr1,enabled=c1
..                                            setprop NxchEditSearchMlr2,enabled=c1
..                                            setprop NXCH0001BButtonAdd,enabled=c1
..                                        move    NO to DetailFlag
..                                                move    YES to ExitFlag
                                        return
                              Endif
                    endif
          repeat
*********************************************************************************
          alert   type=quesbox,"Do you want to add the beginning balance and X ref if needed?",result,"New Exchange Records"
          if (result = c7)         .NO
                    call    ADDBEGABORT
..                setprop  colbutt,enabled=c1
..                  move     NO to DetailFlag
..                  move     YES to ExitFlag
                    return
          endif
          move    MLR1,NXRFFLD2             .Do First Mailer XREFcheck
Crosscheck2
          move    NO,XrefFlag               .
          clear   NXRFMLR
          clear   TEMMLR
          move    C2,NXRFPATH
          move    "Crosscheck2-NXRFKEY",Location
          pack    KeyLocation,"Key: ",NXRFFLD
          call    NXRFKEY
          if not over
..                move    NXRFLIST,TEMMLR
                    move    YES,XrefFlag       .There is a cross reference
                    if (mlr2<>NXRFFLD2)        .If not checked cross reference for second mailer check again
                              move mlr2 to Nxrffld2
                              goto crosscheck2
                    else
                              goto  Newbalance    .If both have valid cross references add beginning bal
                              return
                    endif
          endif
          call    EXRefVerify                  .Load XREF FORM
          setitem EditTextBoxes(1),0,TEMMLR
******************************************************
.BEGIN PATCH 7.7 REPLACED LOGIC
.        pack    MKEY,NXRFFLD2,"000"
.        call    Nmlrkey
.        pack    taskname,NXRFFLD2,b1,dash,b1,mcomp
          pack    COMPFLD,NXRFFLD2
          move      "Crosscheck2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call    COMPKEY
          pack    taskname,NXRFFLD2,b1,dash,b1,COMPCOMP
.END PATCH 7.7 REPLACED LOGIC
          setitem StatTextBoxes(5),0,taskname     .Display Mailer that the list will be xref'd to
******************************************************
..        call    EXRefOK
          MOVE    YES TO LSEARCHFLAG              .Allow user to use search function for list xreference
          setprop Report2,visible=1
          MOVE    NO TO LSEARCHFLAG               .Turn off
          If (EXGOODFLAG = NO)                    .If mailers do not have exchange history
                    if (mlr2<>NXRFFLD2)             .if not second pass
                              move mlr2 to Nxrffld2
                              setprop Buttons(2),enabled=1
                              setprop Report2Cancel,enabled=1
                              goto crosscheck2
                    else
                              goto  Newbalance        .if both have xreferences create beg balance
..                        return
                    endif
          endif
          return
ExRefVerify
..Allows selection of Cross Reference Mailers
..Prepare Cross reference form
          call    Report2DestroyObjects
          setprop Report2,title="NIN XREF File Maintenance"
          move    NO,RptCan
          create  Report2;mRSearch,"&Search;&List"
          create  Report2;StatTextBoxes(1)=50:70:10:110,"List",""
          create  Report2;StatTextBoxes(2)=130:150:10:310,"",""
          create  Report2;StatTextBoxes(3)=150:170:10:310,"",""
          create  Report2;StatTextBoxes(4)=70:90:10:310,"",""
          create  Report2;StatTextBoxes(5)=10:30:10:310,"",""
          create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
          create  Report2;Buttons(1)=225:250:50:100,"O&K",zorder=500,default=1
          create  Report2;Buttons(2)=225:250:140:190,"Finish",enabled=0
          activate mRSearch,ExRefSearchGo,result
          activate StatTextBoxes(1)
          activate StatTextBoxes(2)
          activate StatTextBoxes(3)
          activate StatTextBoxes(4)
          activate StatTextBoxes(5)
..When dynamically creating an EditTextBox, you are only given three default events: GotFocus,LostFocus,LostFocus+Change.
..Any other events must be registered manually.
..Below we register a KeyPress event.
          eventreg EditTextBoxes(1),10,ExRefKeyPress,RESULT=N9
          activate EditTextBoxes(1),ExRefEditChange,result
          activate Buttons(1),ExRefOK,result
          activate Buttons(2),ExRefFinish,result
          listins ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
                    StatTextBoxes(5),EditTextBoxes(1),Buttons(1),Buttons(2)
          setfocus EditTextBoxes(1)
          return
ExRefSearchGo
          goto SearchGo2
ExRefEditChange
          if (result = C2)        .Lost Focus + Change
                    setprop Buttons(2),enabled=0
                    setitem StatTextBoxes(2),0,""
                    setitem StatTextBoxes(3),0,""
                    setitem StatTextBoxes(4),0,""
                    getitem EditTextBoxes(1),0,TEMMLR2
                    call    Trim using TEMMLR2
                    call    ZFILLIT using TEMMLR2,C0
                    setitem EditTextBoxes(1),0,TEMMLR2
          endif
          return
ExRefOK
          getitem EditTextBoxes(1),0,TEMMLR2
          call    Trim using TEMMLR2
          call    ZFILLIT using TEMMLR2,C0
          if (TEMMLR2 = "" | TEMMLR2 = "000000")
                    setitem StatTextBoxes(2),0,""
                    setitem StatTextBoxes(4),0,""
                    setitem StatTextBoxes(5),0,""
                    setitem StatTextBoxes(3),0,"Not a valid List Number!!"
                    setprop Buttons(2),enabled=0
                    setfocus EditTextBoxes(1)
          else
                    setitem StatTextBoxes(3),0,TEMMLR2
                    move    C1,NDATPATH
                    move    "ExRefOK-NDATKEY",Location
                    pack    KeyLocation,"Key: ",NDATFLD
                    move    temmlr2 to ndatfld
                    call    NDATKEY
                    if over
                              setitem StatTextBoxes(2),0,""
                              setitem StatTextBoxes(4),0,""
                              setitem StatTextBoxes(5),0,""
                              setitem StatTextBoxes(3),0,"Not a valid List Number!!"
                              setprop Buttons(2),enabled=0
                              setfocus EditTextBoxes(1)
                    else
..                        if (TEMMLR2 <> TEMMLR)
..                                scan    YES,XrefFlag
..                                if equal
..                                        move    "B",XrefFlag        .Mailer XRef has changed
..                                endif
..                                reset   XrefFlag
..                        endif
                              pack    str25,"LIST: ",LSTNUM
                              setitem StatTextBoxes(2),0,str25
                              setitem StatTextBoxes(3),0,OLSTNAME
...                        pack    MKEY,NXRFFLD2,"000"
...                        call    Nmlrkey
...                        pack    taskname,NXRFFLD2,b1,dash,b1,mcomp
...                       setitem StatTextBoxes(5),0,taskname
..                        if (MRCODE = "R")
..                                setitem StatTextBoxes(4),0,"Rental Only!"
..                        else
                              setitem StatTextBoxes(4),0,"If List Info is Correct, click 'Finish'"
..                        endif
                              setprop Buttons(2),enabled=1
                              setfocus Buttons(2)
                    endif
          endif
..        pack    NDATFLD,OLSTNAME
..        move    C1,NDATPATH
..        move    "ExRefOK2-NDATKEY",Location
..        pack    KeyLocation,"Key: ",NDATFLD
..        call    NDATKEY
          return
ExRefFinish
          setprop Buttons(2),enabled=0
          setprop Report2Cancel,enabled=0
          move    TEMMLR2,TEMMLR     .refresh TEMMLR in order to update or write to file
          reset   XrefFlag
          scan    NO,XrefFlag        .If there is no xreference record
          if equal        *WAS NOT IN LIST MLR FILE
                    move    NXRFFLD2,NXRFMLR
                    move    LSTNUM,NXRFLIST
                    call    NXRFWRT
                    move    YES to XREFFLAG
                    setprop Report2,visible=0
***************************************************
                    if (exgoodflag = Yes)      .If not creating a new account
                              goto FirstDetailList
                    else
                              if (mlr2<>NXRFFLD2) .if not second pass
                                        move mlr2 to Nxrffld2
                                        setprop Buttons(2),enabled=1
                                        setprop Report2Cancel,enabled=1
                                        goto crosscheck2
                              else
                                        goto  Newbalance  .if second pass done created beg bal
..                          return
                              endif
                    endif
...                goto crosscheck2
..                IF (LRINIT = 1)
..                move    "NINXRF - ExRefFinish",str45
..                call    OrderWriteLRFile using str45
..                ENDIF
          endif
          return
...        setprop Buttons(2),enabled=1
...        setprop Report2Cancel,enabled=1
...        goto crosscheck2
..        reset   XrefFlag
..        scan    "B",XrefFlag
..        if equal        *LISTMLR FILE WAS WRONG
..                alert   plain,"Update List/Mlr XREF File?",result
..                if (result = C1)
..Who is doing an update, and what are they trying to update?!?
..                        move    "This is a message from NORDTEST",SmtpSubject Subject
..   Set the text message that is send with the attachments
..                        clear   str45
..                        append  INITS,str45
..                        append  " tried to update XREF file.",str45
..                        reset   str45
..                        move    str45,SmtpTextMessage(1)   Array <Text message >
..                        clear   str45
..                        append  "List: ",str45
..                        append  OLNUM,str45
..                        reset   str45
..                        move    str45,SmtpTextMessage(2)   Array <Text message >
..                        clear   str45
..                        append  "Old Mailer: ",str45
..                        append  NXRFMLR,str45
..                        reset   str45
..                        move    str45,SmtpTextMessage(3)   Array <Text message >
..                        clear   str45
..                        append  "New Mailer: ",str45
..                        append  TEMMLR,str45
..                        reset   str45
..                        move    str45,SmtpTextMessage(4)   Array <Text message >
..                        clear   str45
..                        append  "LR: ",str45
..                        append  OLRN,str45
..                        reset   str45
..                        move    str45,SmtpTextMessage(5)   Array <Text message >
..                        move    "5",SmtpTextIndexLast                               Index to last entry in TextMessage array
..                        move    "NTS4",SmtpEmailServer                   Address of email serverc
..                        move    "InformationServices@nincal.com",SmtpEmailAddress
..                        move    "InformationServices",SmtpUserName                                User name
..                        move    "InformationServices",SmtpUserFullName              User Full Name
..                        move    smtpemailaddress,SmtpDestinations(1,1)
..                        move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
..                        move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
..                        clear   SmtpLogFile                                         'Clear' disables the LogFile
..                        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
...
..                        move    TEMMLR,NXRFMLR
..                        call    NXRFUPD                                 10/23/00 *turned off jd/dlh.                           IF (LRINIT = 1)
..                        move    "NINXRF - Update,ExRefFinish",str45
..                        call    OrderWriteLRFile using str45
..                elseif (result = C3)
..                        setitem EditTextBoxes(1),0,TEMMLR
..                        goto ExRefOK
..                endif
..        endif
ExRefKeyPress
          if (N9 = 113) .F2 Key calls Search Function
..Virtual Key Value
                    goto SearchGo2
          elseif (N9 = 120)     .F9 Key closes Search Function
                    setprop Search,visible=0
          endif
          return
NewBalance
READAC1
          clear   ACKEY
          clear   NXNGFLD1
          clear   NXNGFLD2
          pack    ACKEY,MLR1,MLR2
          pack    NXNGFLD1,AKey1A,MLR1
          pack    NXNGFLD2,AKey2A,MLR2
          rep     ZFILL,ACKEY
          move    "READAC1-NXNGAIM",Location
          pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          call    NXNGAIM
          goto REV if over
          alert    caution,"Cannot add exchange Record!",result,"Exchange Record Already Exists"
          call     ADDBEGABORT
...        setprop  colbutt,enabled=c1
...       move     NO to DetailFlag
...       move     YES to ExitFlag
          return
..       move    "NINXNUM",FMESG
..       match   ACKEY,ACCKEY
..       goto ISAMBAD IF NOT EQUAL
..       move    C1,MLRSW
..       move    C1,EFLAG
..       if (mod = 3 OR (mod = 7 AND NORD5STAT <> "04"))
..       if ((mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
..               goto ExacLoop
..       endif
..       GOTO    EXACC
..
.. REV - CHECKS TO SEE IF NXNGFILE EXISTS WITH CLIENT NUMBERS REVERSED.
REV
          clear   ACKEY
          clear   NXNGFLD2
          clear   NXNGFLD1
          pack    ACKEY,MLR2,MLR1
          pack    NXNGFLD2,AKey2A,MLR1
          pack    NXNGFLD1,AKey1A,MLR2
          rep     ZFILL,ACKEY
          move    "REV-NXNGAIM",Location
          pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          call    NXNGAIM
          goto NEWACC if over
          alert    caution,"Cannot add exchange Record!",result,"Exchange Record Exists"
          call     ADDBEGABORT
..                setprop  colbutt,enabled=c1
..                  move     NO to DetailFlag
..                  move     YES to ExitFlag
          return
NEWACC
          pack    ACKEY,MLR1,MLR2
          rep     ZFILL,ACKEY
          move    ACKEY,ACCKEY
          move    C0,ENTRY
          move    "NEWACC-NXNGWRT",Location
          pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
..Writing Beginning Entry
          call    NXNGWRT
..        move    C1,EFLAG
          move    C0,MLRSW
***************************************************
..Get Beginning Balance usage
KEYNEW
          clear   taskname
          append  "Beginning balance for ",taskname
.BEGIN PATCH 7.7 REPLACED LOGIC
.        pack str7 with mlr1,"000"
.        call zfillit using str7
.        move str7 to mkey
.        call NMLRKEY
.        append  MCOMP,taskname
.        append  COLON,taskname
.        reset   taskname
.        setitem OrderXBalStat1,0,taskname
.        setitem OrderXBalEdit1,0,"0"
.        clear   taskname
.        append  "Beginning balance for ",taskname
.        pack str7 with mlr2,"000"
.        call zfillit using str7
.        move str7 to mkey
.        call NMLRKEY
.        append  MCOMP,taskname
..............................
          clear     COMPFLD
          move      mlr1,COMPFLD
          move      "KEYNEW-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          append  COMPCOMP,taskname
          append  COLON,taskname
          reset   taskname
          setitem OrderXBalStat1,0,taskname
          setitem OrderXBalEdit1,0,"0"
          clear   taskname
          append  "Beginning balance for ",taskname
          clear     COMPFLD
          move      mlr2,COMPFLD
          move      "KEYNEW,2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          append  COMPCOMP,taskname
.END PATCH 7.7 REPLACED LOGIC
          append  COLON,taskname
          reset   taskname
          setitem OrderXBalStat2,0,taskname
          setitem OrderXBalEdit2,0,"0"
          setprop OrderXBalOK,enabled=1
          setprop OrderXBalCancel,enabled=1
          setfocus OrderXBalOK
          move NO to OneKCNCFLAG
          setprop Nord001k,visible=1
..If cancelbuttonpress when adding beg bal comes back here after record deleted
          if (OneKCNCFLAG = YES)
                    setprop  colbutt,enabled=c1
                    move     NO to DetailFlag
                    move     YES to ExitFlag
                    return
          endif
..Else Write out RECORD
          move    C0,ENTRY
          pack    EXKEY,ACKEY,ENTRY
          rep     ZFILL,EXKEY
..        setprop Nord001k,visible=1
KEYNEW1
          move    "NXCHNXT",GNXTFLD
          move    "KEYNEW1-GNXTKEY",Location
          pack    KeyLocation,"Key: ",GNXTFLD
..Grab dummy beg bal entry from getnext file
          call    GNXTKEY
          move    GNXTNUM,LR
          move    LR,str1
          bump    LR,1
          move    LR,N5
          reset   LR
          add     C1,N5
          clear   LR
          pack    LR,str1,N5
          rep     ZFILL,LR
          move    LR,GNXTNUM
          move    "KEYNEW1-GNXTUPD",Location
          pack    KeyLocation,"Key: ",GNXTFLD
..Update getnext file
          call    GNXTUPD
..        IF (LRINIT = 1)
          move    "GNXT - Update,KEYNEW1",str45
..        call    OrderWriteLRFile using str45
..        ENDIF
          move    C2,NXCHPATH
          pack    NXCHFLD2,LR
          rep     zfill,NXCHFLD2
          move    "KEYNEW1-NXCHTST",Location
          pack    KeyLocation,"Key: ",NXCHFLD2
          call    NXCHTST
          goto    NEWEXLR if over
          clear   taskname
          append  "This BEG/BAL LR## is already on file!",taskname
          append  carr,taskname
          append  "I will try again.",taskname
          reset   taskname
          alert   note,taskname,result
          goto    KEYNEW1
NEWEXLR
          clear   XCHCOMNT
          CLOCK   TIMESTAMP,str8
..        move    timestamp,str8
          move    str8,DAT
          move    C0,QTY
          move    "000000",LIST
          clear   STAT
          clear   MLRSW
          pack    NXCHFLD1,EXKEY
          rep     zfill,NXCHFLD1
..NXCHWRT sets value of NXCHPATH
          move    INITS,TYPE
          move    "NEWEXLR-NXCHWRT",Location
          pack    KeyLocation,"Key: ",NXCHFLD1
          call    NXCHWRT
          MOVE    YES TO EXGOODFLAG
********************************************************
          setprop   colbutt,enabled=c0
          setprop  NxchEditSearchMlr1,enabled=c1,bgcolor=white
          setprop  NxchEditSearchMlr2,enabled=c1,bgcolor=white
          setprop  NXCH0001BComboBox001,enabled=c0
          setprop  NXCH0001BButtonSave,enabled=0
          setprop  NXCH0001BButtonQuit,enabled=0
          move   NO to DetailFlag
          move   YES to ExitFlag
          move   NO to MODFlag
          move   NO to ADDFlag
          move   NO to CancelFlag
          move   NO to RentalFlag
..Patch7.0
          move   NO to REINSTATEFLAG
..EndPatch7.0
          setprop  NXCH0001BButtonADD,enabled=1
          setprop  NXCH0001BButtonADD,Visible=1
..reload listview objects
          call NXCHClearListView
          call NXCHLoadListView
**********************************************************
..Goes to bluecard screen
.BEGIN PATCH 7.7 REPLACED LOGIC
.        move    C2,n1
.        call    NXCHTABCLICK
.        move    C1,n1
.        call    NXCHTABCHANGE
          call      NXCHSwitchTab using C1
.END PATCH 7.7 REPLACED LOGIC
**********************************************************
..        call          NxchSetFocusTab
          return
DELNEW
..deletes newly created begbalance entry #
..        move    "Y",ReturnFlag
          move    "DELNEW-NXNGTST",Location
          pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          move    YES to OneKCNCFLAG
          call    NXNGTST
          if over
.BEGIN PATCH 7.7 REPLACED LOGIC
.          unpack  NXNGFLD1,str3,str4
.          unpack  NXNGFLD2,str3,str5
.          pack    NXNGFLD1,AKey1A,str5
.          pack    NXNGFLD2,AKey2A,str4
                    unpack  NXNGFLD1,str3,str6B
                    unpack  NXNGFLD2,str3,str6A
                    pack    NXNGFLD1,AKey1A,str6A
                    pack    NXNGFLD2,AKey2A,str6B
.END PATCH 7.7 REPLACED LOGIC
                    move    "DELNEW-NXNGTST-2nd",Location
                    pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                    call    NXNGTST
                    if over
..   Set the text message that is send with the attachments
                              clear   str45
                              append  Keylocation,str45
                              reset   str45
                              Clear     MailBody
                              append    str45,Mailbody
                              Append    "<br>",mailbody
                              clear   str45
                              append  inits,str45
                              append  " cancelled out of beg bal add",str45
                              reset   str45
                              append    str45,Mailbody
                              Append    "<br>",mailbody
                              clear   str45
                              append  "Could not find record",str45
                              reset   str45
                              append    str45,Mailbody
                              Append    "<br>",mailbody
                              clear   str45
                              append  Location,str45
                              reset   str45
                              append    str45,Mailbody
                              Append    "<br>",mailbody
                              Reset     Mailbody
                              move    "InformationServices@nincal.com",MailFrom
                              move    "InformationServices@nincal.com",MailTO
                              move    c1,mailtype
                              call    Sendmail
                              return
                    endif
          endif
          move    "DELNEW-NXNGDEL",Location
          pack    KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          call    NXNGDEL
          return
ClearDetail
          setitem  NXCH0001BEditText001,0,""
          SetItem  NXCH0001BEditText002,0,""
          SetItem  NXCH0001BEditText003,0,""
          SetItem  NXCH0001BEditText004,0,""
          setitem  NXCH0001BStatText005,0,""
          SetItem  NXCH0001BStatText006,0,""
          SetItem  NXCH0001BStatText007,0,""
          SetItem  NXCH0001BStatText009,0,""
          SetItem  NXCH0001BStatText010,0,""
          SetItem  NXCH0001BStatText011,0,""
          setitem  NXCH0001BStatText015,0,""
          setitem  NXCH0001BComboBox001,n3,c0
          return
.==========================================================
ADDBEGABORT
          setprop colbutt,enabled=c0
          setprop NxchEditSearchMlr1,enabled=c1,bgcolor=white
          setprop NxchEditSearchMlr2,enabled=c1,bgcolor=white
          setprop NXCH0001BButtonAdd,enabled=c1
          setprop NXCH0001BButtonAdd,visible=c1
          move    NO to DetailFlag
          move    YES to ExitFlag
          return
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
OrderSetMouseBusy
          setmode *mcursor=*wait
          return
OrderSetMouseFree
          setmode *mcursor=*arrow
          return
.................................................................................................................
Report2DestroyObjects
          destroy ObjectColl
          return
.................................................................................................................
.
FILENAME
          CLEAR     NEWNAME
          APPEND    "EXSTAT",NEWNAME
          MOVE      FILENUM,str2
          REP       ZFILL,str2
          APPEND    str2,NEWNAME
          RESET     NEWNAME TO 8
          RESET     NEWNAME
          TRAP      GOODFILE GIVING ERROR IF IO
          OPEN      RECMST,NEWNAME
          CLOSE     RECMST
ADDFILE
          ADD       "1" TO FILENUM
          GOTO      FILENAME
.
GOODFILE
          TRAPCLR   IO
          noreturn
          SCAN      "0030-0031" IN ERROR         .rms
          GOTO      ADDFILE IF EQUAL
          RESET     ERROR
          SCAN      "I * Y" IN ERROR         .fixed 3/22/93 dlh had I*Y
          GOTO      ADDFILE IF EQUAL
          reset     error
          SCAN      "I10" IN ERROR          .plb
          GOTO      ADDFILE IF EQUAL
          RESET     ERROR
          CLEAR     RECNAME
          APPEND    NTWKPATH1 TO RECNAME
          APPEND    NEWNAME TO RECNAME
          RESET     RECNAME
          MOVE      B1 TO ERROR
          PREPARE   RECMST,RECNAME,CREATE
          return
.============================================================================================
ClearMailers
.clear Mailers and mailervars on invalid read
          clear    mlr1
          clear    mlr2
          clear    holdmcomp1
          clear    holdmcomp2
          setitem  NxchEditSearchMlr1,0,""
          setitem  NxchStatSearchMlr1Name,0,""
          setitem  NxchEditSearchMlr2,0,""
          setitem  NxchStatSearchMlr2Name,0,""
          setfocus NxchEditSearchMlr1
...............................................................................................
SetNXCHErrorMssgDefault
          setprop ErrorMssgStat1,visible=1
          setprop ErrorMssgStat2,visible=1
          setprop ErrorMssgStat3,visible=1
          setprop ErrorMssgStat4,visible=1
          setprop ErrorMssgStat5,visible=0
          setitem ErrorMssgStat1,0,"Enter 4 Digit Mailer Number:"
          setitem ErrorMssgStat2,0,""
          setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
          setitem ErrorMssgStat4,0,"      By Company Name"
          setitem ErrorMssgStat5,0,"      That Mailer Does Not Exist!"
          setitem ErrorMssgOK,0,"&OK"
          return
.......................................................................................................................
*********************************************************************
.subroutines for message box
TOME1
          setprop DialogWait,visible=0
..        setitem DIALOGWAITMESSAGE,0,"I'll Let you know when I am Done!"
          destroy timer1
          return
..TOME2
..        setprop DialogWait,visible=0
..        destroy timer2
..        return

.begin patch 8.0
ViewDetails
          move  seq to n5
          move n5 to n9
          NXCH0001CListView001.getnextitem giving n5 using c2,N9
          if (n5 = seq)
                    alert caution,"Need to Select a record in the Listview Object",result,"No Records Selected"
                    return
          endif
                    NXCH0001CListView001.getnextitem giving n5 using c2,n9
                    NXCH0001CListView001.getitemtext giving MLR2 using n5,c6
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM AKEY1a,MLR1
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR2
..First Read
          CALL      NXNGAIM
          if        over
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM Akey1a,MLR2
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR1
..Second Read
                    CALL      NXNGAIM
                    if        over
                              call        ClearDetail
                              setprop     ErrorMssgStat1,visible=0
                              setprop     ErrorMssgStat2,visible=1
                              setprop     ErrorMssgStat3,visible=0
                              setprop     ErrorMssgStat4,visible=1
                              setprop     ErrorMssgStat5,visible=0
                              pack        str25 with mlr1," & ",mlr2
                              setitem     ErrorMssgStat4,0,str25
                              setitem     ErrorMssgStat2,0,"Exchange Account not found!"
                              setprop     ErrorMssg,visible=1
                              CLEAREVENT
                              return
                    endif  
                    else
.do stuff
           endif
           return
.end patch 8.0
*********************************************************************
..SubPatch6.8
INACTIVATE
          move  seq to n5
          move n5 to n9
          NXCH0001CListView001.getnextitem giving n5 using c2,N9
          if (n5 = seq)
                    alert caution,"Need to Select at least One record in the Listview Object",result,"No Records Selected"
                    return
          endif
          clear n5
          clear n9
          move  seq to n5
          loop
                    move n5 to n9
                    NXCH0001CListView001.getnextitem giving n5 using c2,n9
                    NXCH0001CListView001.getitemtext giving MLR2 using n5,c6
                    until (n5 = seq)
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM AKEY1a,MLR1
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR2
..First Read
          CALL      NXNGAIM
          if        over
                    CLEAR     ACCKEY
                    PACKkey   ACCKEY FROM MLR1,MLR2
                    PACKkey   NXNGFLD1 FROM Akey1a,MLR2
                    PACKkey   NXNGFLD2 FROM Akey2a,MLR1
..Second Read
                    CALL      NXNGAIM
                    if        over
                              call        ClearDetail
                              setprop     ErrorMssgStat1,visible=0
                              setprop     ErrorMssgStat2,visible=1
                              setprop     ErrorMssgStat3,visible=0
                              setprop     ErrorMssgStat4,visible=1
                              setprop     ErrorMssgStat5,visible=0
                              pack        str25 with mlr1," & ",mlr2
                              setitem     ErrorMssgStat4,0,str25
                              setitem     ErrorMssgStat2,0,"Exchange Account not found!"
                              setprop     ErrorMssg,visible=1
                              CLEAREVENT
                              return
                    else
..If valid second read
                              if (flag <> "I")
                                        move "I" to flag
                                        clock timestamp,str8
                                        move str8 to nxngdate
                                        call    NXNGUPD
                              elseif (flag = "I")
                                        pack str55 with Nxngdate,b1,mlr1," & ",mlr2
                                        alert note,str55,result,"Already Inactivated.  I will Continue"
                                        endif
                              endif
                    else
..If valid first read
                              if (flag <> "I")
                                        move "I" to flag
                                        clock  timestamp,str8
                                        move   str8 to nxngdate
                                        call   NXNGUPD
                              elseif (flag = "I")
                                        pack str55 with Nxngdate,b1,mlr1," & ",mlr2
                                        alert note,str55,result,"Already Inactivated.  I will Continue"
                              endif
                    endif
          repeat
          NXCH0001CListView001.DeleteAllItems giving N9
          GOTO Load1CAgain
..EndPatch6.8
........................................................
.BEGIN PATCH 7.7 ADDED LOGIC
. LOGIC ASSOCIATED WITH NEW SCREEN
ExchDisableScreenDUpper
          setprop   NXCH1dOK,enabled=0
          setprop   NXCH1dEditSearch1,enabled=0,bgcolor=grey
          setprop   NXCH1dEditSearch2,enabled=0,bgcolor=grey
          setprop   NXCH1dMNew,enabled=0
          setprop   NXCH1dDNew,enabled=0
          return

ExchEnableScreenDUpper
          setprop   NXCH1dEditSearch1,enabled=1,bgcolor=white
          setprop   NXCH1dEditSearch2,enabled=1,bgcolor=white
          setprop   NXCH1dOK,enabled=1
          setprop   NXCH1dMNew,enabled=1,height=15
          setprop   NXCH1dDNew,enabled=1
          setprop   NXCH1dDListView,enabled=1
          NXCH1dDListView.GetItemCount giving result
          if (result > 0)
                    setprop   NXCH1dDModify,enabled=1
          else
                    setprop   NXCH1dDModify,enabled=0
          endif
          return

ExchEnableScreenDMFlags
          move      YES,ExitFlag2
          move      NO,NewFlag2
          return

ExchEnableScreenDDFlags
          move      YES,ExitFlag3
          move      NO,NewFlag3
          return

ExchDisableScreenDMasterLower
          setprop   NXCH1dMDelete,enabled=0,height=0
          setprop   NXCH1dMQuit,enabled=0
          setprop   NXCH1dMSave,enabled=0
          setprop   NXCH1dComboMInactive,enabled=0,bgcolor=grey
          setprop   NXCH1dEditMDate,enabled=0,bgcolor=grey
          setprop   NXCH1dEditMEntry,enabled=0,bgcolor=grey
          setprop   NXCH1dEditMMlr1,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditMMlr2,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditMNotes,enabled=0,bgcolor=grey
          setprop   NXCH1dMNew,enabled=1,height=15
          return

ExchEnableScreenDMasterLower
          setprop   NXCH1dMQuit,enabled=1
          setprop   NXCH1dMSave,enabled=1
          move      NO,ExitFlag2
          setprop   NXCH1dComboMInactive,enabled=1,bgcolor=white
          setprop   NXCH1dEditMDate,enabled=1,bgcolor=white
          setprop   NXCH1dEditMEntry,enabled=1,bgcolor=white
          setprop   NXCH1dEditMMlr1,enabled=1,bgcolor=white
          setprop   NXCH1dEditMMlr2,enabled=1,bgcolor=white
          setprop   NXCH1dEditMNotes,enabled=1,bgcolor=white
ExchEnableScreenDMasterLower2
          setprop   NXCH1dMNew,enabled=0,height=0
          setprop   NXCH1dMModify,enabled=0
          return

ExchDisableScreenDDetailLower
          setprop   NXCH1dDDelete,enabled=0
          setprop   NXCH1dDQuit,enabled=0
          setprop   NXCH1dDSave,enabled=0
          setprop   NXCH1dComboDStatus,enabled=0,bgcolor=grey
          setprop   NXCH1dComboDSwitch,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDDate,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDEntry,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditDInits,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDLR,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditDList,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDMlr1,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditDMlr2,enabled=0,bgcolor=grey,readonly=1
          setprop   NXCH1dEditDNotes,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDQty,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDUsage1,enabled=0,bgcolor=grey
          setprop   NXCH1dEditDUsage2,enabled=0,bgcolor=grey
          setprop   NXCH1dDNew,enabled=1
          setprop   NXCH1dDListView,enabled=1
          return

ExchEnableScreenDDetailLower
          setprop   NXCH1dDListView,enabled=0
          setprop   NXCH1dDQuit,enabled=1
          setprop   NXCH1dDSave,enabled=1
          move      NO,ExitFlag3
          setprop   NXCH1dComboDStatus,enabled=1,bgcolor=white
          setprop   NXCH1dComboDSwitch,enabled=1,bgcolor=white
          setprop   NXCH1dEditDDate,enabled=1,bgcolor=white
          setprop   NXCH1dEditDEntry,enabled=1,bgcolor=white
          setprop   NXCH1dEditDInits,enabled=1,bgcolor=white
          setprop   NXCH1dEditDLR,enabled=1,bgcolor=white
          setprop   NXCH1dEditDList,enabled=1,bgcolor=white
          setprop   NXCH1dEditDMlr1,enabled=1,bgcolor=white
          setprop   NXCH1dEditDMlr2,enabled=1,bgcolor=white
          setprop   NXCH1dEditDNotes,enabled=1,bgcolor=white
          setprop   NXCH1dEditDQty,enabled=1,bgcolor=white
          setprop   NXCH1dEditDUsage1,enabled=1,bgcolor=white
          setprop   NXCH1dEditDUsage2,enabled=1,bgcolor=white
ExchEnableScreenDDetailLower2
          setprop   NXCH1dDNew,enabled=0
          setprop   NXCH1dDModify,enabled=0
          return

ExchMailerLostFocus Routine EditPtr,StatPtr
          setprop   StatPtr,fgcolor=black         .Default
          getitem   EditPtr,0,str6
          call      Trim using str6
          if (str6 <> "")
                    call      ZFillIt using str6
                    setitem   EditPtr,0,str6
                    pack      COMPFLD,str6
                    move      "MlrLF-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if not over
                              if (COMPMLRFLG <> "T")
                                        setprop   StatPtr,fgcolor=red
                              endif
                              setitem   StatPtr,0,COMPCOMP
                    else
                              setitem   StatPtr,0,""
                    endif
          else
                    setitem   StatPtr,0,""
          endif
          return

ExchClearMaster
          setitem   NXCH1dComboMInactive,0,1
          setitem   NXCH1dEditMDate,0,""
          setitem   NXCH1dEditMEntry,0,""
          setitem   NXCH1dEditMMlr1,0,""
          setitem   NXCH1dEditMMlr2,0,""
          setitem   NXCH1dEditMNotes,0,""
          setitem   NXCH1dStatMMlrName1,0,""
          setitem   NXCH1dStatMMlrName2,0,""
          return

ExchLoadMaster
          if (FLAG = "I")
                    setitem   NXCH1dComboMInactive,0,2
          else
                    setitem   NXCH1dComboMInactive,0,1
          endif
.
          call      Trim using nxngdate
          if (nxngdate <> "")
                    unpack    nxngdate,str4,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
          endif
          setitem   NXCH1dEditMDate,0,str10
.
          move      ENTRY,str5
          call      Trim using str5
          setitem   NXCH1dEditMEntry,0,str5
.
          setprop   NXCH1dStatMMlrName1,fgcolor=black       .Default value
          unpack    ACCKEY,str6b
          pack      COMPFLD,str6b
          move      "ELM1-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    setitem   NXCH1dEditMMlr1,0,str6b
                    setitem   NXCH1dStatMMlrName1,0,"Mailer does not Exist!"
          else
                    if (COMPMLRFLG <> "T")
                              setprop   NXCH1dStatMMlrName1,fgcolor=red
                    endif
                    setitem   NXCH1dEditMMlr1,0,COMPNUM
                    setitem   NXCH1dStatMMlrName1,0,COMPCOMP
          endif
.
          setprop   NXCH1dStatMMlrName2,fgcolor=black       .Default value
          unpack    ACCKEY,str6b,str6b
          pack      COMPFLD,str6b
          move      "ELM2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    setitem   NXCH1dEditMMlr2,0,str6b
                    setitem   NXCH1dStatMMlrName2,0,"Mailer does not Exist!"
          else
                    if (COMPMLRFLG <> "T")
                              setprop   NXCH1dStatMMlrName2,fgcolor=red
                    endif
                    setitem   NXCH1dEditMMlr2,0,COMPNUM
                    setitem   NXCH1dStatMMlrName2,0,COMPCOMP
          endif
.Not yet implemented!!
          setitem   NXCH1dEditMNotes,0,""
          return

ExchVerifyMaster
          setprop   NXCH1dStatMMlrName1,fgcolor=black       .Default value
          getitem   NXCH1dEditMMlr1,0,str6
          call      Trim using str6
          if (str6 = "")
                    alert     note,"Mailer 1 is required!",result
                    move      YES,ReturnFlag2
                    setfocus NXCH1dEditMMlr1
                    return
          endif
          pack      COMPFLD,str6
          move      "EVm1-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    alert     note,"Mailer 1 does not exist!",result
                    move      YES,ReturnFlag2
                    setfocus NXCH1dEditMMlr1
                    return
          else
                    if (COMPMLRFLG <> "T")
.Since I.S. is the only department able to access this Screen, allow input of Non-Mailer clients.
                              setprop   NXCH1dStatMMlrName1,fgcolor=red
                              pack      taskname,"Mailer 1 is not a valid Mailer!",newline,"Do you wish to continue?"
                              alert     plain,taskname,result
                              if (result <> 1)
                                        move      YES,ReturnFlag2
                                        setfocus NXCH1dEditMMlr1
                                        return
                              endif
                    endif
                    setitem   NXCH1dEditMMlr1,0,COMPNUM
                    setitem   NXCH1dStatMMlrName1,0,COMPCOMP
          endif
.
          setprop   NXCH1dStatMMlrName2,fgcolor=black       .Default value
          getitem   NXCH1dEditMMlr2,0,str6B
          call      Trim using str6B
          if (str6B = "")
                    alert     note,"Mailer 2 is required!",result
                    move      YES,ReturnFlag2
                    setfocus NXCH1dEditMMlr2
                    return
          endif
          pack      COMPFLD,str6B
          move      "EVm2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    alert     note,"Mailer 2 does not exist!",result
                    move      YES,ReturnFlag2
                    setfocus NXCH1dEditMMlr2
                    return
          else
                    if (COMPMLRFLG <> "T")
.Since I.S. is the only department able to access this Screen, allow input of Non-Mailer clients.
                              setprop   NXCH1dStatMMlrName2,fgcolor=red
                              pack      taskname,"Mailer 2 is not a valid Mailer!",newline,"Do you wish to continue?"
                              alert     plain,taskname,result
                              if (result <> 1)
                                        move      YES,ReturnFlag2
                                        setfocus NXCH1dEditMMlr2
                                        return
                              endif
                    endif
                    setitem   NXCH1dEditMMlr2,0,COMPNUM
                    setitem   NXCH1dStatMMlrName2,0,COMPCOMP
          endif
.
          pack      NXNGFLD1,"01X",str6
          pack      NXNGFLD2,"02X",str6b
          move      "EVM-NXNGAIM",Location
          pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          call      NXNGAIM
          if over
                    pack      NXNGFLD1,"01X",str6b
                    pack      NXNGFLD2,"02X",str6
                    move      "EVM2-NXNGAIM",Location
                    pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                    call      NXNGAIM
                    if over
                              if (NewFlag2 <> YES)
                                        pack      taskname,"This Mailer/Mailer no longer Exists!",newline,"You cannot Modify!"
                                        alert     note,taskname,result
                                        move      "M",ReturnFlag2
                                        return
                              endif
                    elseif (NewFlag2 = YES)
                              alert     note,"This Mailer/Mailer Combination already Exists!",result
                              move      YES,ReturnFlag2
                              setfocus NXCH1dEditMMlr2
                              return
                    endif
          elseif (NewFlag2 = YES)
                    alert     note,"This Mailer/Mailer Combination already Exists!",result
                    move      YES,ReturnFlag2
                    setfocus NXCH1dEditMMlr2
                    return
          endif
          pack      ACCKEY,str6,str6b
.
          getitem   NXCH1dComboMInactive,0,N1
          if (N1 = 2)
                    move      "I",FLAG
          else
                    clear     FLAG
          endif
.
          getitem   NXCH1dEditMDate,0,str10
          call      Trim using str10
          call      RemoveChar using str10,SLASH
          if (str10 <> "")
                    unpack    str10,MM,DD,str4
                    if (MM < "01" | MM > "12")
                              alert     note,"Valid Month Required!",result
                              move      YES,ReturnFlag2
                              setfocus NXCH1dEditMDate
                              return
                    elseif (DD < "01" | DD > "31")
                              alert     note,"Valid Day Required!",result
                              move      YES,ReturnFlag2
                              setfocus NXCH1dEditMDate
                              return
                    elseif (str4 < "1980" | str4 > "2100")
                              alert     note,"Valid Year Required!",result
                              move      YES,ReturnFlag2
                              setfocus NXCH1dEditMDate
                              return
                    endif
                    pack      nxngdate,str4,MM,DD
          else
                    clear     nxngdate
          endif
.
          getitem   NXCH1dEditMEntry,0,str5
          call      Trim using str5
          move      C0,N5
          move      str5,N5
          move      N5,ENTRY
.Not yet implemented!!
.         getitem   NXCH1dEditMNotes,0,str100
          return


ExchLoadDetailListView
.temp code for testing
.          call      debug
          unpack    NXCHFLD3,str3,str6
          unpack    NXCHFLD4,str3,str6a
          pack      str20 from str6,str6a
          unpack    EXKEY,str12,str5
          if        (str12 <> str25)
.          call      debug
          return
.temp code for testing
          endif
          pack      str20 from str6a,str6
          pack      hold,NXCHVARS
          unpack    EXKEY,str12,str5
          call      Trim using str5
          NXCH1dDListView.InsertItem giving N9 using str5
          unpack    EXKEY,str6b
          NXCH1dDListView.SetItemText using N9,str6b,1
          unpack    EXKEY,str6b,str6b
          NXCH1dDListView.SetItemText using N9,str6b,2
          call      Trim using str5
          NXCH1dDListView.SetItemText using N9,LR,3
          move      USAGE1,str10
          call      FormatNumeric using str10,str13
          NXCH1dDListView.SetItemText using N9,str13,4
          move      USAGE2,str10
          call      FormatNumeric using str10,str13
          NXCH1dDListView.SetItemText using N9,str13,5
          NXCH1dDListView.SetItemText using N9,hold,6
          if (ACCKEY <> "")
                    if (str12 = ACCKEY)
                              move      "0x000000",colordim           .Black
                    else      .Reverse Search
                              move      "0xFF0000",colordim           .Red
                    endif
          endif
          NXCH1dDListView.SetItemText using N9,colordim,7
          return

ExchClearDetail
          setitem   NXCH1dComboDStatus,0,1
          setitem   NXCH1dComboDSwitch,0,1
          setitem   NXCH1dEditDDate,0,""
          setitem   NXCH1dEditDEntry,0,""
          setitem   NXCH1dEditDInits,0,""
          setitem   NXCH1dEditDLR,0,""
          setitem   NXCH1dEditDList,0,""
          setitem   NXCH1dEditDMlr1,0,""
          setitem   NXCH1dEditDMlr2,0,""
          setitem   NXCH1dEditDNotes,0,""
          setitem   NXCH1dEditDQty,0,""
          setitem   NXCH1dEditDUsage1,0,""
          setitem   NXCH1dEditDUsage2,0,""
          setitem   NXCH1dStatDMlrName1,0,""
          setitem   NXCH1dStatDMlrName2,0,""
          setitem   NXCH1dStatDListName,0,""
          return

ExchLoadDetail
          if (STAT = "C")
                    setitem   NXCH1dComboDStatus,0,2
          elseif (STAT = "R")
                    setitem   NXCH1dComboDStatus,0,3
          elseif (STAT = "X")
                    setitem   NXCH1dComboDStatus,0,4
          else
                    setitem   NXCH1dComboDStatus,0,1
          endif
.
          if (MLRSW = "1")
                    setitem   NXCH1dComboDSwitch,0,1
          elseif (MLRSW = "2")
                    setitem   NXCH1dComboDSwitch,0,2
          else
                    setitem   NXCH1dComboDSwitch,0,1
          endif
.
          call      Trim using DAT
          if (DAT <> "")
                    unpack    DAT,str4,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
          endif
          setitem   NXCH1dEditDDate,0,str10
.
          setprop   NXCH1dStatDMlrName1,fgcolor=black       .Default Value
          unpack    EXKEY,str6b
          pack      COMPFLD,str6b
          move      "ELD1-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    setitem   NXCH1dEditDMlr1,0,str6b
                    setitem   NXCH1dStatDMlrName1,0,"Mailer does not Exist!"
          else
                    if (COMPMLRFLG <> "T")
                              setprop   NXCH1dStatDMlrName1,fgcolor=red
                    endif
                    setitem   NXCH1dEditDMlr1,0,COMPNUM
                    setitem   NXCH1dStatDMlrName1,0,COMPCOMP
          endif
.
          setprop   NXCH1dStatDMlrName2,fgcolor=black       .Default Value
          unpack    EXKEY,str6b,str6b,str5
          pack      COMPFLD,str6b
          move      "ELD2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    setitem   NXCH1dEditDMlr2,0,str6b
                    setitem   NXCH1dStatDMlrName2,0,"Mailer does not Exist!"
          else
                    if (COMPMLRFLG <> "T")
                              setprop   NXCH1dStatDMlrName2,fgcolor=red
                    endif
                    setitem   NXCH1dEditDMlr2,0,COMPNUM
                    setitem   NXCH1dStatDMlrName2,0,COMPCOMP
          endif
.
          call      Trim using str5
          setitem   NXCH1dEditDEntry,0,str5
.
          setitem   NXCH1dEditDInits,0,TYPE
.
          setitem   NXCH1dEditDLR,0,LR
.
          call      Trim using LIST
          if (LIST <> "")
                    setitem   NXCH1dEditDList,0,LIST
                    pack      NDATFLD,LIST
                    move      "ELD-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
                    setitem   NXCH1dStatDListName,0,MLSTNAME
          else
                    setitem   NXCH1dEditDList,0,""
                    setitem   NXCH1dStatDListName,0,""
          endif
.
          setitem   NXCH1dEditDNotes,0,XCHCOMNT
.
          move      QTY,str9
          call      FormatNumeric using str9,str11
          setitem   NXCH1dEditDQty,0,str11
.
          move      USAGE1,str10
          call      FormatNumeric using str10,str13
          setitem   NXCH1dEditDUsage1,0,str13
.
          move      USAGE2,str10
          call      FormatNumeric using str10,str13
          setitem   NXCH1dEditDUsage2,0,str13
          return

ExchVerifyDetail
          setprop   NXCH1dStatDMlrName1,fgcolor=black       .Default value
          getitem   NXCH1dEditDMlr1,0,str6
          call      Trim using str6
          if (str6 = "")
                    alert     note,"Mailer 1 is required!",result
                    move      YES,ReturnFlag3
                    setfocus NXCH1dEditDMlr1
                    return
          endif
          pack      COMPFLD,str6
          move      "EVD1-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    alert     note,"Mailer 1 does not exist!",result
                    move      YES,ReturnFlag3
                    setfocus NXCH1dEditDMlr1
                    return
          else
                    if (COMPMLRFLG <> "T")
.Since I.S. is the only department able to access this Screen, allow input of Non-Mailer clients.
                              setprop   NXCH1dStatDMlrName1,fgcolor=red
                              pack      taskname,"Mailer 1 is not a valid Mailer!",newline,"Do you wish to continue?"
                              alert     plain,taskname,result
                              if (result <> 1)
                                        move      YES,ReturnFlag3
                                        setfocus NXCH1dEditDMlr1
                                        return
                              endif
                    endif
                    setitem   NXCH1dEditDMlr1,0,COMPNUM
                    setitem   NXCH1dStatDMlrName1,0,COMPCOMP
          endif
.
          setprop   NXCH1dStatDMlrName2,fgcolor=black       .Default value
          getitem   NXCH1dEditDMlr2,0,str6B
          call      Trim using str6B
          if (str6B = "")
                    alert     note,"Mailer 2 is required!",result
                    move      YES,ReturnFlag3
                    setfocus NXCH1dEditDMlr2
                    return
          endif
          pack      COMPFLD,str6B
          move      "EVD2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    alert     note,"Mailer 2 does not exist!",result
                    move      YES,ReturnFlag3
                    setfocus NXCH1dEditDMlr2
                    return
          else
                    if (COMPMLRFLG <> "T")
.Since I.S. is the only department able to access this Screen, allow input of Non-Mailer clients.
                              setprop   NXCH1dStatDMlrName2,fgcolor=red
                              pack      taskname,"Mailer 2 is not a valid Mailer!",newline,"Do you wish to continue?"
                              alert     plain,taskname,result
                              if (result <> 1)
                                        move      YES,ReturnFlag3
                                        setfocus NXCH1dEditDMlr2
                                        return
                              endif
                    endif
                    setitem   NXCH1dEditDMlr2,0,COMPNUM
                    setitem   NXCH1dStatDMlrName2,0,COMPCOMP
          endif
.
          pack      NXNGFLD1,"01X",str6
          pack      NXNGFLD2,"02X",str6b
          move      "EVD-NXNGAIM",Location
          pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
          call      NXNGAIM
          if over
                    pack      NXNGFLD1,"01X",str6b
                    pack      NXNGFLD2,"02X",str6
                    move      "EVD2-NXNGAIM",Location
                    pack      KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
                    call      NXNGAIM
                    if over
                              pack      taskname,"This Mailer/Mailer Master Account does not Exist!",newline,"Do you want to continue anyway?"
                              alert     plain,taskname,result
                              if (result <> 1)
                                        move      "Y",ReturnFlag3
                                        setfocus NXCH1dEditDMlr1
                                        return
                              endif
                    else
                              pack      EXKEY,str6b,str6
                              unpack    EXKEY,str6,str6b
                    endif
          endif
.
          getitem   NXCH1dEditDEntry,0,str5
          call      Trim using str5
          move      C0,N5
          move      str5,N5
          move      N5,str5
          rep       zfill,str5
          setitem   NXCH1dEditDEntry,0,str5
.
          pack      EXKEY,str6,str6b,str5
.Test using Primary Key Value!!
          move      C1,NXCHPATH
          pack      NXCHFLD1,EXKEY
          move      "EVD-NXCHTST",Location
          pack      KeyLocation,"Key: ",NXCHFLD1
          call      NXCHTST
          if over
                    if (NewFlag3 <> YES)          .Attempting a Modify a record that no longer exists!
                              pack      taskname,"This Detail Record No longer Exists!",newline,"Key 1: ",NXCHFLD1
                              alert     note,taskname,result
                              move      "M",ReturnFlag3
                              return
                    endif
          elseif (NewFlag3 = YES)                 .Attempting to Create a record that already exists!
                    pack      taskname,"This Detail Record already Exists!",newline,"Key 1: ",NXCHFLD1
                    alert     note,taskname,result
                    move      "Y",ReturnFlag3
                    setfocus NXCH1dEditDMlr1
                    return
          endif
.
          getitem   NXCH1dEditDLR,0,str6a
          call      Trim using str6a
          if (str6a = "")
                    alert     note,"Valid LR Number Required!",result
                    move      "Y",ReturnFlag3
                    setfocus NXCH1dEditDLR
                    return
          else
                    type      str6a
                    if equal
                              move      C1,NORDPATH
                              pack      NORDFLD,str6a
                              move      "EVD-NORDKEY",Location
                              pack      KeyLocation,"Key: ",NORDFLD
                              call      NORDKEY
                              if over
                                        alert     note,"Valid LR Number Required!",result
                                        move      "Y",ReturnFlag3
                                        setfocus NXCH1dEditDLR
                                        return
                              endif
                    else
                              move      "A",str1
                              scan      str1,str6a
                              if equal
                                        if (str5 <> "00000")
                                                  pack      taskname,"This Detail Account looks like a Beg. Balance.",newline,"Do you want to set the Entry Number to '00000'?"
                                                  alert     plain,taskname,result
                                                  if (result = 1)
                                                            setitem   NXCH1dEditDEntry,0,"00000"
                                                            pack      EXKEY,str6,str6b,"00000"
                                                  endif
                                        endif
                              endif
                              reset     str6a
                    endif
          endif
          move      str6a,LR
.Test using Secondary Key Value!!
          move      C2,NXCHPATH
          pack      NXCHFLD2,LR
          move      "EVD2-NXCHTST",Location
          pack      KeyLocation,"Key: ",NXCHFLD2
          call      NXCHTST
          if over
                    if (NewFlag3 <> YES)          .Attempting a Modify a record that no longer exists!
                              pack      taskname,"This Detail Record No longer Exists!",newline,"Key 2: ",NXCHFLD2
                              alert     note,taskname,result
                              move      "M",ReturnFlag3
                              return
                    endif
          elseif (NewFlag3 = YES)                 .Attempting to Create a record that already exists!
                    pack      taskname,"This Detail Record already Exists!",newline,"Key 2: ",NXCHFLD2
                    alert     note,taskname,result
                    move      "Y",ReturnFlag3
                    setfocus NXCH1dEditDLR
                    return
          endif
.
          getitem   NXCH1dComboDSwitch,0,N1
          move      N1,MLRSW
.
          getitem   NXCH1dComboDStatus,0,N1
          if (N1 = 2)
                    move      "C",STAT
          elseif (N1 = 3)
                    move      "R",STAT
          elseif (N1 = 4)
                    move      "X",STAT
          else
                    clear     STAT
          endif
.
          getitem   NXCH1dEditDDate,0,str10
          call      Trim using str10
          call      RemoveChar using str10,SLASH
          if (str10 <> "")
                    unpack    str10,MM,DD,str4
                    if (MM < "01" | MM > "12")
                              alert     note,"Valid Month Required!",result
                              move      YES,ReturnFlag3
                              setfocus NXCH1dEditDDate
                              return
                    elseif (DD < "01" | DD > "31")
                              alert     note,"Valid Day Required!",result
                              move      YES,ReturnFlag3
                              setfocus NXCH1dEditDDate
                              return
                    elseif (str4 < "1980" | str4 > "2100")
                              alert     note,"Valid Year Required!",result
                              move      YES,ReturnFlag3
                              setfocus NXCH1dEditDDate
                              return
                    endif
                    pack      DAT,str4,MM,DD
          else
                    clear     DAT
          endif
.
          getitem   NXCH1dEditDUsage1,0,str10
          call      Trim using str10
          call      RemoveChar using str10,COMMA
          move      C0,USAGE1
          move      str10,USAGE1
.
          getitem   NXCH1dEditDUsage2,0,str10
          call      Trim using str10
          call      RemoveChar using str10,COMMA
          move      C0,USAGE2
          move      str10,USAGE2
.
          getitem   NXCH1dEditDQty,0,str9
          call      Trim using str9
          call      RemoveChar using str9,COMMA
          move      C0,QTY
          move      str9,QTY
.
          setitem   NXCH1dStatDListName,0,""
          getitem   NXCH1dEditDList,0,NDATFLD
          call      Trim using NDATFLD
          if (NDATFLD <> "")
                    call      ZFillIt using NDATFLD
                    move      C1,NDATPATH
                    move      "EVD-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
                    if over
                              alert     note,"Valid List Required!",result
                              move      YES,ReturnFlag3
                              setfocus NXCH1dEditDList
                              return
                    else
                              setitem   NXCH1dEditDList,0,NDATFLD
                    endif
                    setitem   NXCH1dStatDListName,0,MLSTNAME
          endif
          move      NDATFLD,LIST
.
          getitem   NXCH1dEditDInits,0,TYPE
.
          getitem   NXCH1dEditDNotes,0,XCHCOMNT
          return
.END PATCH 7.7 ADDED LOGIC
XchStatCMPName_Click
                    call            NxchDisplayCompany using NXCH0001,NxchEditSearchMlr1,NxchEditText001,N4,MouseForm,T1,L1
              return
XchStatCMPName_Click2
                    call            NxchDisplayCompany using NXCH0001,NxchEditSearchMlr2,NxchEditText001,N4,MouseForm,T1,L1
              return


.patch7.6
.                        INCLUDE NMLRIO.inc
          include   compio.inc
          include   cntio.inc
.patch7.6
          INCLUDE NDATIO.inc
          INCLUDE NXNGIO.inc
          INCLUDE NXCHIO.inc
          include nownio.inc
          include nofrio.inc
          INCLUDE NPASIO.inc
          INCLUDE GNXTIO.inc
          include nxrfio.inc
          INCLUDE NXRF1IO.INC
          Include MLRHELP.inc
          include nordio.inc
          include nuseio.inc
.patch7.6
.     include nbrkio.inc
.patch7.6
          include nspeio.inc
          include nspiio.inc
          include nrtnio.inc
          include NCNTIO.inc
.............................................................
.Following used only in order to load Search.plf
          include ncmpio.inc
          include searchio.inc      ;.contains logic for search.plf
..end of required for search.plf
.............................................................
          INCLUDE   COMLOGIC.INC
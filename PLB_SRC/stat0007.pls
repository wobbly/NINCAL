pC         EQU         1
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
         include   consacct.inc
           INCLUDE   statsDD.inc
           INCLUDE   SXRFDD.inc
         include   NPKGdd.inc
.patch2.7
                              include compdd.inc
                              include   cntdd.inc
.         include  nmlrdd.inc
.patch2.7

         include  ndatdd.inc
         include  norddd.inc
.
.begin patch 2.3
           INCLUDE    NXRFDD.inc
           INCLUDE    NXCHDD.inc
           INCLUDE    NXNGDD.inc
.end patch 2.3
.begin patch 2.32
              include        SlctClndd.inc
.end patch 2.32
.
.Following used only in order to load Search.plf
        include ncmpdd.inc
        include nrtndd.inc
.patch2.7
.        include nbrkdd.inc
.patch2.7
.START PATCH 2.48 ADDED LOGIC
          include   nowndd.inc
.END PATCH 2.48 ADDED LOGIC
.START PATCH 2.6 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 2.6 ADDED LOGIC
.        include norddd.inc
.................................................................................................................
release   init      "2.8.1"   ASH       27JAN2005 INCREASED STATS MAILER FIELD TO 6 BYTES
reldate   init      "JANUARY 27, 2005"
.release  init      "2.8"     ASH       25OCT2004 INCREASED MAILER FIELD TO 6 BYTES
.reldate  init      "OCTOBER 25, 2004"
.release  init      "2.7"     DMB       26MAY2004 Mailer Conversion
.release  init      "2.6"     ASH       29JAN2004  DATACARD CONVERSION
.reldate  init      "JANUARY 29, 2004"
.release init    "2.51"           DLH     package read for TNC during history file prep
.RelDate   Init     "December 10, 2003"
.release init    "2.50"           12March03 DLH     stat xref read
.RelDate   Init     "March 12, 2003"
.release init    "2.49"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.RelDate   Init     "November 19, 2002"
..Release   init     "2.48"       18SEP02  ASH  ADDED FUNCTIONALITY OF SEARCH.PLF TO SEARCH FOR OWNER - OBSOLETE FOR THIS APPLICATION!!!!!
..RelDate   Init     "September 18, 2002"
..Release   init     "2.47"       12AUG02  DLH  USED statsxref list desc for history read using mailer & mailers code.
..RelDate   Init     "August 12, 2002"
.Release   init     "2.46"       09AUG02  JD USED statsxref list desc for history
.Release   init     "2.45"       29Jul02  DLH Update package file read in stat0007a.plf.
.Release   init     "2.44"       08Jul02  DLH add Package-date and RR*Ag columns to history spreadsheet.
.Release   init     "2.43"       21May02  DLH display stat package code
.                                            and date verification (minimal)
.Release   init     "2.42"       15May02  DLH little excuse me
.Release   init     "2.41"       09APR02  DLH force for statxfld1 reads if 1st time fails convert to uppercase and
.                               try again.
.Release   init     "2.4"       08aPR02  DLH Add list view object on xref tab
.Release   init     "2.33"       08aPR02  DLH CHANGE IN StatxFLD1
.Release   init     "2.32"       07Feb02  DLH add "selection cleanup" at output
.release  init      "2.31"       19NOV01 ASH CONVERTED NINSTATS, CONVERTED NINPKG
.release  init      "2.3"        08July2001 DLH report options
.release  init      "2.2"       10July2001 DLH New ListView object for all details associated with an LR and detail
.                              lookup by lr also on detail screen if change keycode on TNC record do xref lookup
.                      and update info.
.release  init      "2.12"     5July2001 DLH New Indices see statsdd
.release  init      "2.11"     15May2001 DLH repair missing code
.release  init      "2.1"     24April2001 DLH add search function
.release  init      "2.0"     April2001 DLH Gui interface allow maintenance of package and list cross reference
.                            info.
.release  init      "1.0"     061200JD allow Sales to correct Stat info "LR's"
.START PATCH 2.31 NEW LOGIC
.EXTERNAL ROUTINES FROM NPKG0001.PLC
STATPackageLoadListViews external "NPKG0001;PackageLoadListViews"
STATPackageDisableForm external "NPKG0001;PackageDisableForm"
STATPackageEnableForm external "NPKG0001;PackageEnableForm"
STATPackageOKClick external "NPKG0001;PackageOKClick"
ExitFlag4 dim       %1
          move      "Y",ExitFlag4
.END PATCH 2.31 NEW LOGIC
SAVELR     DIM         6
ITEM     FORM      2
NewXRef  form   1          ..1=add record mode, 2= modify record
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
................................................................................................................
.begin patch 2.4
XListTry       Form           1
tempfile2      file
CurRec    form           5.2
CurVal    form      3
LastVal   form      3
RecVal    form      9
DimPtr    dim       ^
DimPtr1   dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
.end patch 2.4
................................................................................................................
.begin patch 2.3
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
.Formatting vars needed
.This constant was found in the Object Browser in excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
formula1 dim       25
formula2 dim       35
formula3 dim       60               .total cost       .15June98 DLH increase size
formula4 dim       30               .revenue/loss
formula5 dim       35
formula6 dim       120               .projected responses
formula7 dim       100               .projected revenue
formula8 dim       80               .projected response rate
formula9 dim       100               .Estimated responses in
formula10 dim      100               .Estimated Total Revenue/(Cost) in

RECSIN   FORM      6
RECSout FORM      6
newDate2 dim      10

STR256   DIM       300
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
track    dim        8
offer    dim         3
minusflg dim        1
mqty     dim       11
dim30    dim       30
mqtyb    dim       10
z96      dim        6
mmyy     dim        4
Akey1    dim        3
rpara    init       ")"
LPara    init       "("
RESP     dim        8
Rresp     dim        8
NCI      dim        8
pci      dim        8
CI       dim        8
revenue  dim        9
gift     dim        5
lstcpm   dim        10
tlst$     dim        12
pckcpm    dim        6
tpck      dim        12
totcpm    dim        9
mailcost  dim        10
unitpc    dim        8
totpc     dim        9
totcst    dim        15
totcst13  dim        13
totlcost  dim        11
netrev8   dim        8
netrev    dim        13
nrpci     dim        10
nrnci     dim        10
nrpcia    dim        9          was 8
nrncia    dim        9          was 8  dlh 03jun97
CstA9     dim        9
CstA      dim        10
pcaci     dim        10
Ncaci     dim        10
CTA       dim        31
cost$     dim        6
nqty      dim        12
inv       dim        10
lcpm      dim        7
NAMresp   dim        10
NAMrev    dim        10
IAMresp   dim        10
IaMrev    dim        10
Assocresp dim        9
assocrev  dim        9
assocrresp dim       8
basresp   dim        9
BRResp    dim        8
BREv      dim        9
TAresp    dim        9
inmailcpm dim        6
aggrindx  dim        4
premcost  dim        6
oldflag   form       1
page      form       4
lines     form       2
.START PATCH 2.8.1 REPLACED LOGIC
.clientin  dim        4
clientin  dim        6
.END PATCH 2.8.1 REPLACED LOGIC
path      init       "\\nins1\e\data\statflat"
extension init       ".dat"
CountOut  form       5
TOTALROW  FORM       5
exbal    form      10
usage2c   dim       10
XFLAG    FORM          1
startdate  form       8
datechk    form       8
datesw     dim        1         y=alldates n=start date
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
.Look at Excel Object Model to understand heirarchy.  This can be found in hard
.documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.Software available via PL/B Designer - create a Container object on a form, create an Excel
.Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can
.view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.option to locate Help topics to see specifics.
.
.General heirarchy:
. Excel Application
.       Workbooks Collection (all open Workbooks)
.               Single Workbook
.                       Worksheets Collection (all Worksheets in this Workbook)
.                               Single Worksheet
.                                       SortColumn (a Single Column in that Worksheet used for sorting)
.
books    automation
book     automation
sheets   automation
sheet    automation
sortcol  automation
sortcol2 automation
sortcol3 automation
sortcol4 automation
ex       automation      class="Excel.Application"
.end patch 2.3
..............................................................................................

.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
newdate1 dim     10
olddate dim     10
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
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
.begin patch 2.3
mReports menu
.end patch 2.3
mHelp    menu
mOptions menu
.Set Up SubMenu for Options
sColor  submenu
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2;-;&Color;"
.begin patch 2.3
RData   init    "&Reports;&History"
.end patch 2.3
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"


.............................
.Set Vars used for About Box
        move    "Stat0007.PLS",Wprognme
        move    "Stats File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
SRCH    plform  Search
mss1    plform  Error
abt     plform  About
stat1   plform  stat0007a
.START PATCH 2.31 REPLACED LOGIC
.stat2   plform  stat0007b
.END PATCH 2.31 REPLACED LOGIC
stat3   plform  stat0007c
.begin patch 2.32
stat4   plform  stat0007D
.end patch 2.32
x       plform  Stat0007
.begin patch 2.3
rpt     plform  StatReport
.end patch 2.3
        winhide
.Used to keep track of tabs during Updating and Saving
TabNum   form   1
.
Timer   Timer
.Load Forms, Always load parent form first
        formload x
        formload stat1,Stat0007
.START PATCH 2.31 REPLACED LOGIC
.        formload stat2,Stat0007
.END PATCH 2.31 REPLACED LOGIC
        formload stat3,Stat0007
.begin patch 2.32
        formload stat4,Stat0007
.end patch 2.32
.begin patch 2.3
        formload rpt
.end patch 2.3
        formload abt
        formload mss1
        formload SRCH
.Set tab index
        move    C2,TabNum

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  Stat0007;mFile,FData
        create  Stat0007;mOptions,OData,mFile
        create  Stat0007;mHelp,HData,mOptions
.Create SubMenu
        create  Stat0007;sSearch,SData,mOptions,1

...........................................................................................
.FileGo leads to stop
        activate mFile,FileGo,result
        activate MOptions
        activate mHelp,HelpGo,result
.Create SubMenu
        create  Stat0007;sSearch,SData,mOptions,1
.begin patch 2.3
        create  stat0007;mReports,RData,mOptions
.end patch 2.3
.Activate SubMenus
        activate sSearch,SearchGo,result
.begin patch 2.3
        activate mReports,ReportGo,result
.end patch 2.3
.START PATCH 2.31 ADDED LOGIC
.Load Package Screen
          call      STATPackageLoadListViews using STAT0007,C3
.END PATCH 2.31 ADDED LOGIC
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
.          MOVE        "Stat0007" TO PROGRAM
.          MOVE      "Names In The News Ca Inc" TO COMPNME
.          MOVE        "Update Stat lr's" TO STITLE
.Set Error Message Stat Text Boxes
         move      c1 to ndatpath
.         move      c1 to nmlrpath
         call    SetstatsErrorMssgDefault
.begin patch 2.12
.         move      c3 to statpath
         move      c1 to statpath
.         call      statopen3
         call      statopen
.end patch 2.12
.          CALL        PAINT
.
.begin patch 2.2
        StatDetListView001.InsertColumn using "Source Code",100,1
        StatDetListView001.InsertColumn using "Package",120,2
        StatDetListView001.InsertColumn using "qty",60,3
.end patch 2.2
.begin patch 2.4
        StatXRefListView001.InsertColumn using "Code",50,1
        StatXRefListView001.InsertColumn using "Client List Name",250,2
        StatXRefListView001.InsertColumn using "List ##",50,3
.
        StatXRefListView002.InsertColumn using "Client List Name",250,1
        StatXRefListView002.InsertColumn using "Code",50,2
        StatXRefListView002.InsertColumn using "List ##",50,3
.end patch 2.4
.

        if    (portn = 15)
        setprop Form001Button001,visible=1
        else
        setprop Form001Button001,visible=0
        endif
START
.         CALL      WIPEVAR
        move        c1 to n1
          call        StatsTabChange
        Setfocus    StatsEditText021
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto         timeout
           display     *P1:3,*EF,"STAT RECORD mlr#/source?"
         display   *p13:3,statmlr,"/",statsrce
         KEYIN     *P13:3,*JR,*ZF,*RV,*edit,statmlr,"/",*P18:3,*jl,*rv,*edit,statsrce;
.START PATCH 2.8.1 REPLACED LOGIC
.         MATCH    "000*",statmlr
         MATCH    "00000*",statmlr
.END PATCH 2.8.1 REPLACED LOGIC
         GOTO      EOJ IF EQUAL
         scan      Star in statmlr
         GOTO      EOJ IF EQUAL
         reset     statmlr
         scan      Star in statsrce
         GOTO      EOJ IF EQUAL
         reset     statsrce
         DISPLAY   *P13:3,statmlr,"/",statsrce
.END PATCH 1.2 - ADDED VAR
         KEYIN     *P76:3,"OK?",STR1;
         cmatch    yes to str1
         goto      start if not equal
.begin patch 2.12
.         PACKkey     statFLD3 FROM statmlr,statsrce    added 12/13/99 jd
.         FILEPI    1;statFLE3
.         READ      statFLE3,statFLD3;statvars
         PACKkey     statFLD FROM statmlr,statsrce
         FILEPI    1;statFile
         READ      statFiLE,statFLD;statvars
        if         over
.         DISPLAY    *P1:24,*EL,*HON,*B,"NO RECORD FOUND MATCHING KEY ! ",statfld3,*w2
         DISPLAY    *P1:24,*EL,*HON,*B,"NO RECORD FOUND MATCHING KEY ! ",statfld,*w2
.end patch 2.12
           GOTO        START
         endif
         goto      start3
START3   DISPLAY   *P1:4,"(1) MAILER: ",STATMLR:
                   *P1:5,"(2) CAMPAIGN: ",STATCAMPN:
                   *P1:6,"(3) MAIL DATE: ",STATMDATE:
                   *P1:7,"(4) WEEKS OUT: ",STATWKSO:
                   *P1:8,"(5) MAIL PROCESS THRU: ",STATPDATE:
                   *P1:9,"(6) PACKAGE DESC: ",STATPANEL:
                   *P1:10,"(7) MAILER SOURCE: ",STATSRCE:
                   *P1:11,"(8) MAILER'S LIST: ",STATLDES:
                   *P1:12,"(9) LIST SELECT: ",STATSEL:
                   *P1:13,"(10) NINCA's LIST #: ",STATLIST:
                     *P1:14,"(11)LIST TYPE: ",STATTYPE:
                     *p1:15,"(12)QUANTITY MAILED:",STATMQTY:
                   *p1:16,"(13)NUMBER RESPONSES:",statresp:
                     *p1:17,"(14)GROSS REVENUE:",statrev:
                     *p1:18,"(15)LIST COST/M:",statlcpm:
                     *p1:19,"(16)IN MAIL COST/M:",statimcst:
                     *p1:20,"(17)NINCA's LR#:",statlr:
                   *p1:21,"(18)MAILER'S KEY CODE:",statkycd:
                   *p1:22,"(19)TOTAL PACKAGE COST:",statpack:
                     *p1:23,"(20)TOTAL PACKAGE COST/M: ",statpckm
         KEYIN     *P1:24,*EL,"IS THIS THE STAT RECORD YOU WANT? ",STR1;
         CMATCH    YES,STR1
         GOTO      START IF NOT EQUAL
GETITEM  KEYIN     *P1:24,*EL,"WHAT ITEM NUMBER DO YOU WANT TO CHANGE? ",ITEM;
         COMPARE   "99",ITEM
         GOTO      UPDATE IF EQUAL
         BRANCH    ITEM OF T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,t11,t12:
                   t13,t14,t15,t16,t17,t18,t19,t20
         GOTO      GETITEM
T1
         GOTO      GETITEM
T2
         GOTO      GETITEM
T3
         GOTO      GETITEM
T4
         GOTO      GETITEM
T5
         GOTO      GETITEM
T6
         GOTO      GETITEM
T7
         GOTO      GETITEM
T8
         GOTO      GETITEM
T9
         GOTO      GETITEM
T10
           GOTO      GETITEM
T11
         GOTO      GETITEM
T12
         GOTO      GETITEM
T13
         GOTO      GETITEM
T14
         GOTO      GETITEM
T15
         GOTO      GETITEM
T16
         GOTO      GETITEM
T17
        KEYIN     *P23:20,*jr,statlr
         GOTO      GETITEM
T18
         GOTO      GETITEM
T19
        GOTO      GETITEM
T20
        GOTO      GETITEM
UPDATE
           call    LoadVars
           clear   statsrce
           getitem StatsEditText021,0,statmlr
           getitem StatsDetEditText001,0,statsrce
           call    trim using statsrce
         reset     statsrce
         PACKkey     statFLD FROM statmlr,statsrce
           move          c1 to statpath
          call           stattst
          call      statupd
          call           cleanview
.argh   13July2001
.        move         c1 to statpath
.         call        stattst
.         call        statwrt
.         call        statupd
        return
.argh   13July2001

         GOTO      START
EOJ      STOP
WIPEVAR
           RETURN
LoadView
.begin patch 2.2
          setitem StatsDetEditText001,0,statsrce         .source     - redisplay of looked up by source else new if by lr
.end patch 2.2
.START PATCH 2.8.1 REPLACED LOGIC
.         setitem StatsEditText001,0,mcomp              .mailer
          setitem StatsEditText001,0,COMPcomp              .mailer
.END PATCH 2.8.1 REPLACED LOGIC
         setitem StatsDetEditText002,0,statcampn              .campaign
         unpack  statmdate into mm,dd,cc,yy
         clear   str10
         pack    str10 from mm,slash,dd,slash,cc,yy
           setitem StatsDetEditText005,0,str10                .maildate
         clear   str6
         move    statwkso to str6
         setitem StatsDetEditText003,0,str6                 .weeksout
         setitem statsDetEditText004,0,statpanel             .package
.begin patch 2.43
         setitem statsDetEditText020,0,statpckcde             .package
.end patch 2.43
         setitem StatsDEtEditText006,0,OLSTNAME               .order list name
.begin patch 2.50
.START PATCH 2.8.1 REPLACED LOGIC
.               if         (statmlr = "0170" or statmlr = "0055")        .NWF/Unicef - does not use codes uses description field
               if         (statmlr = "000913" or statmlr = "000811")        .NWF/Unicef - does not use codes uses description field
.END PATCH 2.8.1 REPLACED LOGIC
               packkey     STATXFLD1 from statmlr,statldes
               move        c2 to Statxpath
               clear       Statxlist
               call        Statxkey
                    if          not over
                              setitem     StatsDetEditText009,0,statXdesc1               .Mailers list name
               Else
                              setitem StatsDetEditText009,0,statldes               .Mailers list name
                    endif
               else
               packkey     STATXFLD from statmlr,statkycd
               move        c1 to Statxpath
               clear       Statxlist
.begin patch 2.42
.               clear       statlist
.end patch 2.42
               call        Statxkey
                    if          not over
                              setitem     StatsDetEditText009,0,statXdesc1               .Mailers list name
               Else
                              setitem StatsDetEditText009,0,statldes               .Mailers list name
                        endif
               endif
.end patch 2.50
.         setitem StatsDetEditText009,0,statldes               .Mailers list name
         setitem StatsDetEditText010,0,statsel                .Mailers List Select
         setitem StatsDetEditText007,0,Statlist               .NIN List number
         setitem StatsDetEditText008,0,stattype               .List type
         clear   str8
         move    statMqty to str8
         setitem StatsDetEditText012,0,str8               .Qty Mailed
         clear   str7
         move    statresp to str7
         setitem StatsDetEditText013,0,str7                  .# of responses
         clear   str9
         move    statrev to str9
         setitem StatsDEtEditText014,0,str9                 .Gross revenue
         clear   str7
         move     statlcpm to str7
         setitem StatsDetEditText015,0,str7               .List cost per m
         clear   str9
         move     statImcst to str9
         setitem StatsDetEditText016,0,str9                .In Mail cost per m
         setitem StatsDEtEditText017,0,statkycd               .keycode
         clear   str8
         move    statpack to str8
         setitem StatsDetEditText018,0,str8                   .package cost
.START PATCH 2.31 REPLACED LOGIC
.         clear   str6
.         move    statpckm to str6
.         setitem StatsDetEditText019,0,str6               .package cost per m
         clear   str8
         move    statpckm to str8
         setitem StatsDetEditText019,0,str8               .package cost per m
.END PATCH 2.31 REPLACED LOGIC
         setitem StatsDetEditText011,0,statLR                 .List Rental number
               getitem        statsDetEditText020,0,statpckcde             .package
          clear     NPKGFLD5
          clear     NPKGFLD4
          clear     NPKGFLD2
          clear     NPKGFLD3
          clear     NPKGFLD1
.START PATCH 2.8.1 REPLACED LOGIC
..START PATCH 2.8 REPLACED LOGIC
..        pack      NPKGFLD1,"01X",STATMLR
.         move      "COMPKEY3",Location
.         pack      COMPFLD3,STATMLR
.         pack      KeyLocation,"Key: ",COMPFLD3
.         call      COMPKEY3
.         pack      NPKGFLD1,"01X",COMPNUM
..END PATCH 2.8 REPLACED LOGIC
          pack      NPKGFLD1,"01X",STATMLR
.END PATCH 2.8.1 REPLACED LOGIC
          call      trim using statpckcde
          pack      NPKGFLD4,"04X",statpckcde
          clear     STATPANEL
          move      C1,NPKGPATH
          move      "NPKGAIM",Location
          pack      KeyLocation,"Key: ",NPKGFLD1,NPKGFLD4
          call      NPKGAIM
          if not over
                    move      NPKGPNAME,STATPANEL
                    setitem   statsDetEditText004,0,NPKGPName             .package
          else
                    setitem   statsDetEditText004,0,"No package found for that code!"         .package
          endif

.   end of bad dave
         return
.begin patch 2.2
LookForAllLRs
           call    trim using statlr
           move    c2 to statpath
           packkey statfld2 with statlr
               call    statkey
               if      not over
                clear        str8
                move         statmqty to str8
                move    statlr to str6
                StatDetListView001.InsertItem giving N9 using statsrce
                StatDetListView001.SetItemText using N9,statpanel,1
                StatDetListView001.SetItemText using N9,str8,2
                StatDetListView001.EnsureVisible using N9,0
                else
                return
               endif
.
LRLOOP        call    statks
               if      not over
           call    trim using statlr
                         if      (statlr = str6)
                          clear        str8
                      move         statmqty to str8
                          StatDetListView001.InsertItem giving N9 using statsrce
                          StatDetListView001.SetItemText using N9,statpanel,1
                          StatDetListView001.SetItemText using N9,str8,2
                          StatDetListView001.EnsureVisible using N9,0
                              goto          lrloop
                              else
                              return
                              endif
              else
              return
              endif
.end patch 2.2
CleanView
         setitem StatsDetEditText002,0,""               .mailer
         setitem StatsDetEditText003,0,""               .campaign
         setitem StatsDetEditText004,0,""                .maildate
.begin patch 2.43
         setitem statsDetEditText020,0,""             .package
.end patch 2.43
         setitem StatsDetEditText005,0,""               .weeksout
         setitem StatsDetEditText006,0,""               .package
         setitem StatsDetEditText007,0,""               .order list name
         setitem StatsDetEditText008,0,""               .Mailers list name
         setitem StatsDetEditText009,0,""               .Mailers List Select
         setitem StatsDetEditText010,0,""               .NIN List number
         setitem StatsDetEditText011,0,""               .List type
         setitem StatsDetEditText012,0,""               .Qty Mailed
         setitem StatsDetEditText013,0,""               .# of responses
         setitem StatsDetEditText014,0,""               .Gross revenue
         setitem StatsDetEditText015,0,""               .List cost per m
         setitem StatsDetEditText016,0,""               .In Mail cost per m
         setitem StatsDetEditText017,0,""               .keycode
         setitem StatsDetEditText018,0,""               .package cost
         setitem StatsDetEditText019,0,""               .package cost per m
         setitem StatsDetEditText011,0,""               .lr
         StatDetListView001.DeleteAllItems
         return
LoadVars
           getitem StatsEditText021,0,statmlr
.START PATCH 2.8.1 REPLACED LOGIC
.         Getitem StatsEditText001,0,mcomp              .mailer
         Getitem StatsEditText001,0,COMPcomp              .mailer
.END PATCH 2.8.1 REPLACED LOGIC
         Getitem StatsDetEditText002,0,statcampn              .campaign
.begin patch 2.43
         clear   str10
           Getitem StatsDetEditText005,0,str10                .maildate
          call      Trim using str10
          count     N9,str10
          if (N9 = C10)
                    unpack    str10,MM,str1,DD,str1,CC,YY
                    pack      str10,MM,DD,CC,YY
          elseif (N9 = C8)
                    unpack    str10,MM,DD,CC,YY
                    pack      newdate1,MM,SLASH,DD,SLASH,CC,YY
                    setitem   StatsDetEditText005,0,newdate1
          elseif (N9 <> C0)
               alert          caution,"Date must be in MM/DD/YYYY format!!!!",result
                    setfocus       StatsDetEditText005
                    return
          endif
          if (str10 <> "")
                    type      str10
                    if not equal
               alert          caution,"Date must be in MM/DD/YYYY format!!!!",result
                    setfocus       StatsDetEditText005
                              return
                    endif
          else
                    move      C0,str10  .Set Up   Default
          endif

.         unpack  str10 into mm,slash,dd,slash,cc,yy
.         unpack  str10 into mm,str1,dd,str1,cc,yy
.end patch 2.43
         pack    statmdate from mm,dd,cc,yy
         Getitem StatsDetEditText003,0,str6
         move    str6 to statwkso                       .weeksout
         Getitem StatsDetEditText004,0,statpanel             .package
.begin patch 2.43
         getitem statsDetEditText020,0,statpckcde             .package
.end patch 2.43
         Getitem StatsDetEditText006,0,OLSTNAME               .order list name
         Getitem StatsDetEditText009,0,statldes               .Mailers list name
         Getitem StatsDetEditText010,0,statsel                .Mailers List Select
         Getitem StatsDetEditText007,0,Statlist               .NIN List number
         Getitem StatsDetEditText008,0,stattype               .List type
         Getitem StatsDetEditText012,0,str8                   .Qty Mailed
         move    str8 to statmqty                          .Qty Mailed
         Getitem StatsDetEditText013,0,str7
         move    str7 to statresp                          .# of responses
         Getitem StatsDetEditText014,0,str9
         move    str9 to statrev                .Gross revenue
         Getitem StatsDetEditText015,0,str7
         move    str7 to statlcpm               .List cost per m
         Getitem StatsDetEditText016,0,str9
         move    str9 to statImcst               .In Mail cost per m
         Getitem StatsDetEditText017,0,statkycd               .keycode
         Getitem StatsDetEditText018,0,str8
         move    str8,statpack               .package cost
.START PATCH 2.31 REPLACED LOGIC
.         Getitem StatsDetEditText019,0,str6
.         move    str6 to statpckm               .package cost per m
         Getitem StatsDetEditText019,0,str8
         move    str8 to statpckm               .package cost per m
.END PATCH 2.31 REPLACED LOGIC
         move    statlr to str6
         Getitem StatsDetEditText011,0,statLR                 .package cost per m
         if      (str6 <> statlr)
         move    statlr to nordfld
         move    c1 to nordpath
         call    nordkey
.begin patch 2.32
.START PATCH 2.6 REPLACED LOGIC
.               packkey        SlctClnFld from o2des
                    packkey   NSEL2FLD,"1",OLRN
                    move      "NSEL2KEY",Location
                    pack      KeyLocation,"Key: ",NSEL2FLD
                    call      NSEL2KEY
                    if over
                              move      O2DES,NSEL2NAME
                    endif
               packkey        SlctClnFld from NSEL2NAME
.END PATCH 2.6 REPLACED LOGIC
               call           SlctClnKey
               if             not over
               move           SlctClnText to Statsel          .clean select
               else
.START PATCH 2.6 REPLACED LOGIC
.               move           o2des to statsel
               move           NSEL2NAME to statsel
.END PATCH 2.6 REPLACED LOGIC
               endif
.         move    o2des to statsel
.end patch 2.32
         endif
         return

SetStatsErrorMssgDefault
.Set Default for Owner File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"You need a 4 Digit Mailer Number"
        setitem ErrorMssgStat2,0,"and the Full Source code"
        setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
        setitem ErrorMssgStat4,0,""
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.............................................................................................................
Timeout
        beep
        beep
        beep
        stop
StatsSwitchToTwo
        if (TabNum <> C2)
                if (TabNum = C3)
                        move    C2,N1
                elseif (TabNum = C4)
                        move    C3,N1
                elseif (TabNum = C5)
                        move    C4,N1
                endif
                call    StatsTabClick
                move    C1,N1
                call    StatsTabChange
                setitem StatsTabControl001,0,1
        endif
        return

StatsSwitchToThree
        if (TabNum <> C3)
                if (TabNum = C2)
                        move    C1,N1
                elseif (TabNum = C4)
                        move    C3,N1
                elseif (TabNum = C5)
                        move    C4,N1
                endif
                call    StatsTabClick
                move    C2,N1
                call    StatsTabChange
                setitem StatsTabControl001,0,2
        endif
        return
............................................................................................
StatsTabClick
        IF (N1 = C1)
                Deactivate Stat1
        elseIF (N1 = C2 )
.START PATCH 2.31 REPLACED LOGIC
.                Deactivate Stat2
                    call      STATPackageDisableForm
.END PATCH 2.31 REPLACED LOGIC
        elseif   (N1 = C3 )
                Deactivate Stat3
        elseif   (N1 = C4 )
                Deactivate Stat4
        Endif
        return

StatsTabChange
        IF (N1 = C1)
                move    C1,TabNum
                Activate Stat1
                setfocus StatsDetEditText001
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C2)
                move    C2,TabNum
.START PATCH 2.31 REPLACED LOGIC
.                Activate Stat2
.                setfocus StatPkgEditText001
                    call      STATPackageEnableForm
.END PATCH 2.31 REPLACED LOGIC
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        elseif  (N1 = C3)
                move    C3,TabNum
                Activate Stat3
                setfocus StatXRefEditText001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Else
                move    C4,TabNum
                Activate Stat4
                setfocus SLctClnEditText001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
.        goto    StartPrint
FileGo2
        return
FileGo3
                winshow
                stop
        return
HelpGo
        setprop AboutMssg,visible=1
        return
EditGo
        return
.............................................................................................................
.START PATCH 2.31 NEW LOGIC
          call      STATPackageOKClick using MNUM
.END PATCH 2.31 NEW LOGIC
.START PATCH 2.31 REPLACED LOGIC
.loadpkgview
.         setitem    StatPkgEditText002,0,NPKGdesc
.         CLEAR      STR8
.         UNPACK     NPKGDATE INTO STR2,YY,MM,DD
.         MOVE       C0 TO N2
.         MOVE       MM TO N2
.         IF         (N2 > 0 & N2 < 13)
.         PACK       STR8 FROM MM,SLASH,DD,SLASH,YY
.         ENDIF
.         setitem    StatPkgEditText003,0,STR8
.         CLEAR      STR6
.          MOVE       NPKGcost TO STR6
.         setitem    StatPkgEditText004,0,STR6
.          setprop    StatPkgButton001,visible=1            .Delete
.          setprop    StatPkgButton004,visible=0            .add
.          setprop    StatPkgButton003,visible=0            .save
.         RETURN
.END PATCH 2.31 REPLACED LOGIC
.............................................................................................................
.START PATCH 2.31 REPLACED LOGIC
.Clearpkgview
.         setitem    StatPkgEditText002,0," "
.         CLEAR      STR8
.         setitem    StatPkgEditText003,0,STR8
..         CLEAR      STR6
.         setitem    StatPkgEditText004,0,STR8
.          setprop    StatPkgButton001,visible=0            .Delete
.          setprop    StatPkgButton004,visible=0            .add
.          setprop    StatPkgButton003,visible=0            .save
.         RETURN
.END PATCH 2.31 REPLACED LOGIC
.............................................................................................................
.START PATCH 2.31 REPLACED LOGIC
.VerifyPackage
.        getitem    statPkgEditText001,0,str10
.        call       trim using str10
.        cmatch     b1 to str10
.        return     if equal
.        return     if eos
.        move       str10 to npkgcode
..cost
.        move       c0 to npkgcost
.        getitem    statPkgEditText004,0,str6
.        call       trim using str6
.        move       str6 to npkgcost
..date
.        clear      str8
.        clear      str2
.        clear      mm
.        clear      dd
.        clear      yy
.        getitem    statPkgEditText003,0,str8
.        call       trim using str8
.        unpack     str8 into mm,str1,dd,str1,yy
.        pack       NPKGDATE from cc,yy,mm,dd
.        getitem    StatPkgEditText002,0,NPKGdesc
.        if         (statmlr = b1 or statmlr = "")
.                setprop ErrorMssgStat1,visible=0
.                setprop ErrorMssgStat2,visible=0
.                setprop ErrorMssgStat3,visible=0
.                setprop ErrorMssgStat4,visible=0
.                setprop ErrorMssgStat5,visible=1
.                setitem ErrorMssgStat5,0,"  I need a valid 4 byte Mailer ##"
.                setprop ErrorMssg,visible=1
.                call    SetstatsErrorMssgDefault
.                setfocus StatsEditText021
.         endif
.
.        move       statmlr to Npkgmlr
.        packkey    Npkgfld from Statmlr,Npkgcode
.        call       Npkgtst
.        if         over
.        call       Npkgwrt
.        else
.        call       Npkgupd
.        endif
.         call       loadpkgview
.
.        return
.END PATCH 2.31 REPLACED LOGIC
.............................................................................................................
.loadXrefview
.         getitem    StatXRefEditText001,0,StatXfld
.        call       trim using statXfld
.        call       StatxTST
.        if         not over.
.         setprop    StatXRefButton004,visible=0       .add button.
.         setprop    StatXRefButton004,visible=1       .Modify button
.         setprop    StatXRefButton005,visible=1       .Delete button
.         setprop    StatXRefButton003,visible=1       .Save button
.       else
.       call       ClearXrefview
.         setprop    StatXRefButton004,visible=1       .add button
.         setprop    StatXRefButton004,visible=0       .Modify button
.         setprop    StatXRefButton005,visible=0       .Delete button
.         setprop    StatXRefButton003,visible=1       .Save button
.        setfocus   StatXRefEditText003
.        endif
.        return
.............................................................................................................
ClearXrefview
        setitem    StatXRefEditText002,0," "
        setitem    StatXRefEditText003,0," "
        setitem    StatXRefEditText004,0," "
        setitem    StatXRefEditText005,0," "
          setprop    StatXRefButton004,visible=0       .add button
          setprop    StatXRefButton004,visible=0       .Modify button
          setprop    StatXRefButton005,visible=0       .Delete button
          setprop    StatXRefButton003,visible=0       .Save button
        return
.............................................................................................................
VerifyXref
        return
.............................................................................................................
loadListview
           getitem StatsEditText021,0,statmlr
        if         (statmlr = b1 or statmlr = "")
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.START PATCH 2.8.1 REPLACED LOGIC
.                setitem ErrorMssgStat5,0,"  I need a valid 4 byte Mailer ##"
                setitem ErrorMssgStat5,0,"  I need a valid 6 byte Mailer ##"
.END PATCH 2.8.1 REPLACED LOGIC
                setprop ErrorMssg,visible=1
                call    SetstatsErrorMssgDefault
                setfocus StatsEditText021
         endif

.START PATCH 2.8.1 REPLACED LOGIC
.         if         (statmlr = "0170" or statmlr = "0055")        .NWF/Unicef - does not use codes uses description field
         if         (statmlr = "000913" or statmlr = "000811")        .NWF/Unicef - does not use codes uses description field
.END PATCH 2.8.1 REPLACED LOGIC
         getitem    StatXRefEditText003,0,statxdesc1
         call       trim using statxdesc1
               If             (statxdesc1 = "")
               alert          caution,"You must enter a list to search on!!!!",result
               setfocus       StatXRefeditText003
               return
               endif
         packkey    statxfld1 from StatMlr,StatxDESC1
               Move           C1 to XListTry
.         packkey    statxfld1 from StatxDESC1
         move       c2 to statxpath
         else
         move       c1 to statxpath
         getitem    StatXRefEditText001,0,statxcode
         call       trim using statxcode
.begin patch 2.11
         Clear      statxfld
         Clear      statxfld1
               If             (statxcode = "")
               alert          caution,"You must enter a code to search on!!!!",result
               setfocus       StatXRefeditText001
               return
               endif
         packkey    statxfld from statmlr,Statxcode
.end patch 2.11
.
         endif
XListRead
         call       statxkey
         if         not over
                   if         (statxpath = c2)
                   goto       verflist
                 else
                 goto       LoadListView1
                 endif
         else                                 .over on read
                   if         (statxpath = c2 & Xlisttry = c1)
                        getitem    StatXRefEditText003,0,statxdesc1
                        rep        LowUp in Statxdesc1
                        clear      statxfld1
                        packkey    statxfld1 from statmlr,statxdesc1
                        add        c1 to XListTRy
                        goto       XlistRead
                        endif

         setprop     StatXRefButton002,visible=1       .Quit button
         setprop     StatXRefButton003,visible=0       .save button
         setprop     StatXRefButton005,visible=0       .Delete button
         setprop     StatXRefButton004,visible=1       .Add button
           setitem    StatXRefEditText002,0,""
           setitem    StatXRefEditText004,0,""
           setitem    StatXRefEditText005,0,""
         endif
         return
.................................................................
LoadListView1
           setitem    StatXRefEditText002,0,StatxList
           setitem    StatXRefEditText003,0,Statxdesc1
           setitem    StatXRefEditText005,0,StatxMrkt
         packkey    ndatfld from statxlist
         call       ndatkey
         if         not over
           setitem    StatXRefEditText004,0,Olstname
         else
           setitem    StatXRefEditText004,0,"List not found"
         endif
         setprop     StatXRefButton002,visible=1       .Quit button
         setprop     StatXRefButton003,visible=1       .save button
         setprop     StatXRefButton005,visible=0       .Delete button
         setprop     StatXRefButton004,visible=0       .Add button
         return
.
.................................................................
Verflist
        match       statmlr to statxmlr
        goto        LoadListView1 if equal
        call        statxks
          goto        verflist if not over
        call        clearlistview
        setprop     StatXRefButton002,visible=1       .Quit button
        setprop     StatXRefButton003,visible=0       .save button
        setprop     StatXRefButton005,visible=0       .Delete button
        setprop     StatXRefButton004,visible=1       .Add button
        setitem    StatXRefEditText002,0,""
          setitem    StatXRefEditText004,0,""
          setitem    StatXRefEditText005,0,""
        return
.
.................................................................
ClearListView
           setitem    StatXRefEditText002,0,""
           setitem    StatXRefEditText004,0,""
           setitem    StatXRefEditText005,0,""
         CLEAR      StatxList
         CLEAR      Statxdesc1
         CLEAR      StatxMrkt
         return
..................................................................
stat0007aGo
           clear   statsrce
           getitem StatsEditText021,0,statmlr
           getitem StatsDetEditText001,0,statsrce
           call    trim using statsrce
         scan      Star in statsrce
         GOTO      EOJ IF EQUAL
         reset     statsrce
.begin patch 2.2
           cmatch    b1 to statsrce
           goto      lookbylr if eos
.end patch 2.2
.begin patch 2.12
.         PACKkey     statFLD3 FROM statmlr,statsrce    added 12/13/99 jd
         PACKkey     statFLD FROM statmlr,statsrce
           move          c1 to statpath
.begin patch 2.2
           count       n3 from statfld
.START PATCH 2.8.1 REPLACED LOGIC
.          if          (n3 <= c4)
           if          (n3 <= c6)
.END PATCH 2.8.1 REPLACED LOGIC
           goto        LookByLR
           endif
.end patch 2.2
.
stat0007aGoRead
.
.end patch 2.12
           call      statkey
        if Over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                setprop ErrorMssgStat5,visible=0
                setprop ErrorMssgStat1,visible=1
                setprop ErrorMssgStat2,visible=1
                setprop ErrorMssgStat3,visible=1
                setprop ErrorMssgStat4,visible=1
                call    SetstatsErrorMssgDefault
                setfocus StatsDetEditText001
           clear     statfld
           call      cleanview
           goto      stat0007agodone
          endif
.
           setitem StatsEditText021,0,statmlr
.START PATCH 2.8.1 REPLACED LOGIC
.                packkey   mkey from statmlr,z3
.                call      nmlrkey
.          setitem StatsEditText001,0,mcomp              .mailer
          pack      COMPFLD,statmlr
          move      "stat0007aGoRead-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          setitem StatsEditText001,0,COMPcomp              .mailer
.END PATCH 2.8.1 REPLACED LOGIC
           packkey   ndatfld from statlist
           call      ndatkey
           call      loadview
           call       LookForAllLRs
           goto      stat0007agodone
............
.begin patch 2.2
LookByLR
           getitem StatsDetEditText011,0,statlr
           call    trim using statlr
           move    c2 to statpath
           packkey statfld2 with statlr
           goto    stat0007aGoRead
.
Stat0007aGoDone
.end patch 2.2
        return
................................................................................................
stat0007akycd
.patch 2.12
.if mailer is TNC and this object changes - read x ref file update NIN list number and
.name and Mlr list name.
           getitem StatsEditText021,0,statmlr
           getitem StatsDetEditText017,0,statkycd
           call    trim using statkycd
         clear       statlist
         clear       statldes
         packkey     STATXFLD from statmlr,statkycd
        move        c1 to Statxpath
         clear       Statxlist
         clear       statlist
         call        Statxkey
           if          not over
         move        Statxlist to statlist
                 move         Statxdesc1 to statldes
                 move        StatXlist to ndatfld
                 move        c1 to ndatpath
                 move        c3 to ndatlock
                 rep         zfill in ndatfld
                 call        ndatkey
                         if          not over
                         move        olstname to statldes
                         endif
          endif
         setitem StatsDEtEditText006,0,OLSTNAME               .order list name
               If             (statxDesc1 <> "")
               setitem        StatsDetEditText009,0,statXdesc1               .Mailers list name
               else
               setitem        StatsDetEditText009,0,statldes               .Order list name
               endif
         setitem StatsDetEditText010,0,statsel                .Mailers List Select
         setitem StatsDetEditText007,0,Statlist               .NIN List number
         return
.end patch 2.12
.................................................................
Xrefverify
.need valid key(s)
        if         (statmlr = b1 or statmlr = "")
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.START PATCH 2.8.1 REPLACED LOGIC
.                setitem ErrorMssgStat5,0,"  I need a valid 4 byte Mailer ##"
                setitem ErrorMssgStat5,0,"  I need a valid 6 byte Mailer ##"
.END PATCH 2.8.1 REPLACED LOGIC
                setprop ErrorMssg,visible=1
                call    SetstatsErrorMssgDefault
                setfocus StatsEditText021
         endif
         move       statmlr to statxmlr
           getitem    StatXRefEditText001,0,Str8
         call       trim using str8
         move       str8 to statXcode
           getitem    StatXRefEditText002,0,StatxList
           getitem    StatXRefEditText003,0,Statxdesc1
           getitem    StatXRefEditText005,0,StatxMrkt

         if         (newXref = 1)
         call       StatXWrt
         else
         call       statXUpd
         endif
        setprop     StatXRefButton002,visible=1       .Quit button
        setprop     StatXRefButton003,visible=0       .save button
        setprop     StatXRefButton005,visible=0       .Delete button
        setprop     StatXRefButton004,visible=0       .Add button
        setfocus    StatXRefEditText001
        return
.............................................................................................
StatXRefKeyPress
        if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
                goto SearchGo3
        elseif (N9 = 120)     .F9 Key closes Search Function
                setprop Search,visible=0
        endif
        return
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
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
SearchGo3 Routine
.Needs to be a Routine as NPKG0001 will call it!
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
SearchLoad
.START PATCH 2.48 ADDED LOGIC
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.END PATCH 2.48 ADDED LOGIC
SearchLoad1
.BROKER - not used in this program
        return
SearchLoad2
.LIST
                unpack  Srchstr,str6,str1,str35
                setitem StatXRefEditText002,0,str6
                setitem StatXRefEditText004,0,str35
                setfocus StatXRefEditText005
        return
SearchLoad3
.MAILER
.START PATCH 2.8 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
.        setitem StatsEditText021,0,str4
.        setitem StatsEditText001,0,str45
.        setfocus StatsButton001
          unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        setitem StatsEditText021,0,str6
        setitem StatsEditText001,0,str45
        setfocus StatsButton001
.END PATCH 2.8 REPLACED LOGIC
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.START PATCH 2.48 ADDED LOGIC
SearchLoad5
.CAMPAIGN - not an option with this program
        return
SearchLoad6
.OWNER - not an option with this program
        return
.END PATCH 2.48 ADDED LOGIC
............................................................................................................
.begin patch 2.3
.
ReportGo
           setprop StatReport,visible=1
           clear   statsrce
           getitem StatsEditText021,0,statmlr
..............................................................................
.lets prepare the spreadsheet
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.
.Open Excel application
        create  ex
.START PATCH 2.49 ADDED LOGIC
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 2.49 REPLACED LOGIC
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
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
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
        sheet.range("A1","CC3000").Clear
.All objects except for the SortColumn have been created.
.
.
.....Campaign Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
.START PATCH 2.8.1 REPLACED LOGIC
.        call    Trim using MCOMP
.        setprop sheet.range("B1","B1"),*Value="Statistical History Maildates:"
.        setprop sheet.range("B2","B2"),*Value=MCOMP
        call    Trim using COMPCOMP
        setprop sheet.range("B1","B1"),*Value="Statistical History Maildates:"
        setprop sheet.range("B2","B2"),*Value=COMPCOMP
.END PATCH 2.8.1 REPLACED LOGIC
        setprop sheet.range("B3","B3"),*Value="Last Update:"
        setprop sheet.range("Z4","Z4"),*Value="Estimated"
        setprop sheet.range("AA4","AA4"),*Value="Projected"
        setprop sheet.range("AE4","AE4"),*Value="Clients"
.begin patch 2.44
        setprop sheet.range("AF4","AF4"),*Value="Package"
        setprop sheet.range("AG4","AG4"),*Value="RR * AG"
.end patch 2.44
        setprop sheet.range("A5","A5"),*Value="Source"
        setprop sheet.range("B5","B5"),*Value="Key"
        setprop sheet.range("C5","C5"),*Value="List Name"
        setprop sheet.range("d5","d5"),*Value="Select"
        setprop sheet.range("e5","e5"),*Value="Package"
        setprop sheet.range("f5","f5"),*Value="Maildate"
        setprop sheet.range("g5","g5"),*Value="Track"
        setprop sheet.range("h5","h5"),*Value="List"
        setprop sheet.range("i5","i5"),*Value="Quantity"
        setprop sheet.range("j5","j5"),*Value="Response"
        setprop sheet.range("k5","k5"),*Value="##"
        setprop sheet.range("m5","m5"),*Value="Average"
        setprop sheet.range("n5","n5"),*Value="List"
        setprop sheet.range("o5","o5"),*Value="In Mail"
        setprop sheet.range("p5","p5"),*Value="Total"
        setprop sheet.range("q5","q5"),*Value="Net"
        setprop sheet.range("r5","r5"),*Value="Cost To"
        setprop sheet.range("s5","s5"),*Value="NINCA"
        setprop sheet.range("v5","v5"),*Value="Projected"
        setprop sheet.range("w5","w5"),*Value="Projected"
        setprop sheet.range("x5","x5"),*Value="Projected"
        setprop sheet.range("y5","y5"),*Value="Projected"
        setprop sheet.range("z5","z5"),*Value="% Response"
        setprop sheet.range("aa5","aa5"),*Value="Total"
        setprop sheet.range("ae5","aE5"),*Value="Package"
.begin patch 2.44
        setprop sheet.range("Af5","Af5"),*Value="Date"
.        setprop sheet.range("AG5","AG5"),*Value="Index"
.end patch 2.44
        setprop sheet.range("h6","h6"),*Value="Type"
        setprop sheet.range("i6","i6"),*Value="Mailed"
        setprop sheet.range("j6","j6"),*Value="Rate"
        setprop sheet.range("k6","k6"),*Value="Responses"
        setprop sheet.range("l6","l6"),*Value="Revenue"
        setprop sheet.range("m6","m6"),*Value="Gift"
        setprop sheet.range("n6","n6"),*Value="CPM"
        setprop sheet.range("o6","o6"),*Value="Cost/M"
        setprop sheet.range("p6","p6"),*Value="Cost"
        setprop sheet.range("q6","q6"),*Value="Revenue/(Loss)"
        setprop sheet.range("r6","r6"),*Value="Acquire"
        setprop sheet.range("s6","s6"),*Value="LR"
        setprop sheet.range("u6","u6"),*Value="Pkg/m"
        setprop sheet.range("v6","v6"),*Value="Weeks Out"
        setprop sheet.range("w6","w6"),*Value="Responses"
        setprop sheet.range("x6","x6"),*Value="Revenue"
        setprop sheet.range("y6","y6"),*Value="% Resp"
        setprop sheet.range("z6","z6"),*Value="In"
        setprop sheet.range("aa6","aa6"),*Value="Revenue/(Loss)"
        setprop sheet.range("ae6","ae6"),*Value="Code"
.Set up Header Formatting
        setprop sheet.range("A5","aa6").Font,*Bold="True"
.        Setprop Sheet.Range("A5","aa6").Font,*Name="Times New Roman",*Fontstyle="Bold",*size="48"

        sheet.range("A5","aa6").BorderAround using *LineStyle=1,*Weight=2
.
.....Records.....
.Records for this report always start at row 8

.end patch 2.0
..............................................................................
.looper lets get recordds
looper  CALL       STATSEQ
        goto       REPeoj if over
        if         (statsrce = "AHOMQ010301981")
        call       debug
        endif
        clear       str6
        move        recsin to str6
.        display    *p10:10,"records in ",recsin,b1,statsrce,b1,statmlr;
        add        c1 to recsin
        clear       str6
        move        recsin to str6
.temp temp temp
.        setitem     Statscountin,0,str6

        match      clientin to statmlr
        goto       looper if not equal
.
        if         (dateSW <> YES)      .all dates not selected we must have a range
        unpack      statmdate into mm,dd,cc,yy
        call       cvtjul

        if        (lodate > 0)
         COMPARE   LODATE TO JULdays              CHECK IF IN DTE RNG.
         GOTO      looper IF LESS              NO. TRY NEXT ORDER.
         endif

         if        (hidate > 0)
         COMPARE   JULdays TO HIDATE
         GOTO      looper IF LESS              NO. TRY NEXT ORDER.
         endif

         goto      gotone
        endif
gotone
.        display    *p10:12,"records out ",recsout;
        add        c1 to recsout
        clear       str6
        move        recsout to str6
.temp temp temp
.        setitem     Statscountout,0,str6
..............................................................................
        CLEAR      TRACK
.START PATCH 2.8.1 REPLACED LOGIC
.        if         (statmlr = "0170")
        if         (statmlr = "000913")
.END PATCH 2.8.1 REPLACED LOGIC
        move       "Assoc" to track
        SCAN       "contri" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "CONTRI" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "CARD" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "Card" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "Seeds" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "Wrapping" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Contrib" to track
                  endif

                  reset      statpanel
                  SCAN       "CALENDAR" IN STATPANEL
                  IF         EQUAL
                  MOVE       "Calendar" to track
                  endif
.        endif

ARRGH
.              if         (statmlr = "0173")
.START PATCH 2.8.1 REPLACED LOGIC
.              Elseif         (statmlr = "0173")
              Elseif         (statmlr = "000619")
.END PATCH 2.8.1 REPLACED LOGIC
             clear      track
.begin patch x.xx
          clear     NPKGFLD5
          clear     NPKGFLD4
          clear     NPKGFLD2
          clear     NPKGFLD3
          clear     NPKGFLD1
.START PATCH 2.8.1 REPLACED LOGIC
..START PATCH 2.8 REPLACED LOGIC
..        pack      NPKGFLD1,"01X",STATMLR
.         move      "2-COMPKEY3",Location
.         pack      COMPFLD3,STATMLR
.         pack      KeyLocation,"Key: ",COMPFLD3
.         call      COMPKEY3
.         pack      NPKGFLD1,"01X",COMPNUM
..END PATCH 2.8 REPLACED LOGIC
          pack      NPKGFLD1,"01X",STATMLR
.END PATCH 2.8.1 REPLACED LOGIC
          call      trim using statpckcde
          pack      NPKGFLD4,"04X",statpckcde
          clear     STATPANEL
          move      C1,NPKGPATH
          move      "NPKGAIM",Location
          pack      KeyLocation,"Key: ",NPKGFLD1,NPKGFLD4
          call      NPKGAIM
          if not over
                    move      NPKGPNAME,STATPANEL
          else
                    move      "No package found for code!",STATPANEL
          endif
.end patch x.xx

.begin patch 2.47
.        move       c3 to Statxpath          .read by ninlist #
         Move        c1 to statxpath          .read by mailer and mailers code
               move       statlist to Statxfld2
               pack       statxfld from statmlr,statkycd
.end patch 2.47

                       call       Statxkey
                  if         not over
.start patch 2.46
.begin patch 2.47      pack       Ndatfld,Statxlist
                       call       ndatkey
.                       move       statxdesc1 to statldes
                                       if         not over
                                       move       OLSTNAME to statldes
                                       endif

.end patch 2.47
.end patch 2.46
                        match      statmlr to statXmlr
                          if         equal
                            move       Statxmrkt to track
                        else
Statxloop               call       statxks
                        if         not over
                            if   (statmlr = statxmlr and statxfld2 = statxlist)
                            move       statxmrkt to track
                                 endif
                        endif
                         endif
                endif
        endif
.end patch 1.4
.........................................................................................................................
        type       statlr
        if         equal
        move       statlr to nordfld
        move       c1 to nordpath
        call       nordkey
        if         not over
        clear      str4
        clear      str3
        unpack     oodnum into str4,str3
        move       c0 to n3
        move       str3 to n3
.START PATCH 2.8.1 REPLACED LOGIC
.               if         (statmlr = "0170")
               if         (statmlr = "000913")
.END PATCH 2.8.1 REPLACED LOGIC
                              compare    "14" to n3
                              if         equal
                              MOVE       "Contrib" to track
                              endif
                              compare    "15" to n3
                              if         equal
                              MOVE       "Basic" to track
                              endif
                              compare    "16" to n3
                              if         equal
                              MOVE       "Assoc" to track
                              endif
                              compare    "20" to n3
                              if         equal
                              MOVE       "Contrib" to track
                              endif
               endif
        endif
        endif
..............................................................................
        compare     c1 to recsout
        if          equal
.START PATCH 2.8.1 REPLACED LOGIC
.        pack        mkey from statmlr,b3
.        replace     zfill in mkey
.        move        c1 to nmlrpath
.        call        nmlrkey
                    pack      COMPFLD,statmlr
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
.END PATCH 2.8.1 REPLACED LOGIC
        clear       str1
..............................................................................
        move        c8 to CountOut
        endif
        clear       str5
        move        CountOut to str5
...............................................................................
.hopefully temp code.
.recalc weeksout as some bad data is in the file
.note skip if either statmdate or statpdate is missing
        cmatch      b1 to statmdate
        goto        da if eos
        cmatch      b1 to statpdate
        goto        da if eos
        clear       mm
        clear       dd
        clear       yy
        unpack      statpdate to mm,dd,str2,yy
        type        yy
        goto       da if not equal
        call       cvtjul
        move       juldays to n6                 .date of last returns
        clear       mm
        clear       dd
        clear       yy
        unpack      statmdate to mm,dd,str2,yy
        type        yy
        goto       da if not equal
        call       cvtjul
        sub        juldays from n6           .days between
        divide     c7 into n6                .get weeks
        move       n6 to statwkso
        if         (n6 < c0 )                 .have not mailed yet
        move       c0 to statwkso
        endif

..............................................................................
.response rate      .col J
DA      cmatch      b1 to str5
        if          equal
        bump        str5 by 1
        goto        Da
        endif
        clear       formula1
        append      "=k",formula1
        append      str5 to formula1
        append      "/i",formula1
        append      str5,formula1
.begin patch  2.0
.        append      "*100",formula1
.end patch  2.0
        reset       formula1

..............................................................................
.average gift  .col M
        clear       formula2
        append      "=if(k",formula2
        append      str5 to formula2
        append      ">0,",formula2
        append      "l",formula2
        append      str5 to formula2
        append      "/k",formula2
        append      str5,formula2
        append      ",0)",formula2
        reset       formula2

..............................................................................
.Total cost              .Col P
        clear       formula3
.dlh 29Mar00
.        append      "=(n",formula3
.        append      str5 to formula3
.        append      "*i",formula3
.        append      str5,formula3
.        append      "/1000)+o",formula3
        append      "=O",formula3
.end change
        append      str5,formula3
        append      "*i",formula3
        append      str5,formula3
        append      "/1000",formula3
        reset       formula3

..............................................................................
.Net REvenue/Loss          .Col Q
        clear       formula4
        append      "=l",formula4
        append      str5 to formula4
.        append      "-O",formula4
.dlh 01072000  o looks wrong should be column P .  ???
        append      "-P",formula4
        append      str5,formula4
        reset       formula4

..............................................................................
.cost to acquire       .Col R
        clear       Formula5
        append      "=if(k",formula5
        append      str5 to formula5
        append      ">0,",formula5
         append      "q",formula5
        append      str5 to formula5
        append      "/k",formula5
        append      str5,formula5
        append      ",0)",formula5
        reset       formula5
..............................................................................
.Projected Responses
.("weeksout"<7,G4/0.8,IF("weeksout"<11,G4/0.9,G4))))     .col W
        clear       formula6
        append      "=if(V",formula6
        append      str5 to formula6
        append      "=3,",formula6
         append      "k",formula6
        append      str5 to formula6
        append      "/0.5,IF(V",formula6
        append      str5 to formula6
        append      "=4,",formula6
         append      "k",formula6
        append      str5 to formula6
        append      "/0.65,IF(V",formula6
        append      str5,formula6
        append      "<7,",formula6
        append      "k",formula6
        append      str5 to formula6
        append      "/0.8,IF(V",formula6
        append      str5,formula6
        append      "<11,",formula6
        append      "k",formula6
        append      str5 to formula6
        append      "/0.9,K",formula6
        append      str5 to formula6
          append     "))))",formula6
        reset       formula6
..............................................................................
. projected revenue formula = projected responses(W) * aver gift(M)                                                                     .col X
        clear       formula7
        append      "=w",formula7
        append      str5 to formula7
        append      "*M",formula7
        append      str5,formula7
        reset       formula7
..............................................................................
. projected response rate  = projecte responses / qty mailed                                                                            .col Y
        clear       formula8
        append      "=W",formula8
        append      str5 to formula8
        append      "/I",formula8
        append      str5,formula8
        reset       formula8
..............................................................................
.  percent of responses in  =IF("weeksout"<=3,0.5,IF("weeksout"=4,0.65,IF("weeksout"<7,0.8,IF("weeksout"<11,0.9,1))))                    .Col Z
        clear       formula9
        append      "=IF(V",formula9
        append      str5 to formula9
        append      "<=3,0.5,IF(V",formula9
        append      str5,formula9
        append      "=4,0.65,IF(V",formula9
        append      str5,formula9
        append      "<7,0.8,IF(V",formula9
        append      str5,formula9
        append      "<11,0.9,1))))",formula9
        reset       formula9
..............................................................................
.projected total revenue/loss
        clear       formula10
        append      "=X",formula10
        append      str5 to formula10
        append      "-P",formula10
        append      str5,formula10
        reset       formula10
..............................................................................

        rep         ", " in statsel
        rep         ", " in statldes
        rep         ", " in statsrce
        rep         ", " in statkycd
        rep         zfill in statmdate
        clear       mm
        clear       dd
        clear       str4
        unpack      statmdate into mm,dd,str4
        clear       str10
        pack        str10 from mm,slash,dd,slash,str4
        call        getexch
..............................................................................
LoadDetail
.Prep work
        move    countout,str9
        call    Trim using str9
.
        clear   str7
        pack    str7,"A",str9
        setprop sheet.range(str7),*Value=statsrce,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"B",str9
        setprop sheet.range(str7,str7),*Value=statkycd
.
        clear   str7
        pack    str7,"C",str9
        setprop sheet.range(str7,str7),*Value=statLdes,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"D",str9
        setprop sheet.range(str7,str7),*Value=statSel,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"E",str9
        setprop sheet.range(str7,str7),*Value=statPanel,*HorizontalAlignment=AlignLeft
.MAILDATE
        clear   str7
        pack    str7,"F",str9
        setprop sheet.range(str7,str7),*Value=str10,*NumberFormat="mm/dd/yyyy"
.
        clear   str7
        pack    str7,"G",str9
        setprop sheet.range(str7,str7),*Value=Track,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"H",str9
        setprop sheet.range(str7,str7),*Value=statType
.
        clear   str7
        pack    str7,"I",str9
        setprop sheet.range(str7,str7),*Value=statMqty,*NumberFormat="##,####0_);[Red](##,####0)"
.
        clear   str7
        pack    str7,"J",str9
        setprop sheet.range(str7,str7),*Value=formula1,*NumberFormat="###.####%"
.
        clear   str7
        pack    str7,"K",str9
        setprop sheet.range(str7,str7),*Value=statRESp,*NumberFormat="##,####0_);[Red](##,####0)"
.
        clear   str7
        pack    str7,"L",str9
        setprop sheet.range(str7,str7),*Value=statREv,*NumberFormat="$##,####0_);[Red]($##,####0)"
.FORMULA2 average gift  .col M
.
        clear   str7
        pack    str7,"M",str9
        setprop sheet.range(str7,str7),*Value=formula2,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"N",str9
        setprop sheet.range(str7,str7),*Value=statlcpm,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"O",str9
        setprop sheet.range(str7,str7),*Value=statImcst,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"P",str9
        setprop sheet.range(str7,str7),*Value=Formula3,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"Q",str9
        setprop sheet.range(str7,str7),*Value=Formula4,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"R",str9
        setprop sheet.range(str7,str7),*Value=Formula5,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"S",str9
        setprop sheet.range(str7,str7),*Value=Statlr
.
        clear   str7
        pack    str7,"T",str9
        setprop sheet.range(str7,str7),*Value=Statpack
.
        clear   str7
        pack    str7,"U",str9
        setprop sheet.range(str7,str7),*Value=Statpckm,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"V",str9
        setprop sheet.range(str7,str7),*Value=Statwkso
.
        clear   str7
        pack    str7,"W",str9
        setprop sheet.range(str7,str7),*Value=Formula6,*NumberFormat="##,####0_);[Red](##,####0)"
.FORMULA7  projected revenue formula = projected responses(W) * aver gift(M)                                                                     .col X
        clear   str7
        pack    str7,"X",str9
        setprop sheet.range(str7,str7),*Value=Formula7,*NumberFormat="$##,####0_);[Red]($##,####0)"
.formula8 projected response rate
        clear   str7
        pack    str7,"Y",str9
        setprop sheet.range(str7,str7),*Value=Formula8,*NumberFormat="###.####%"

.
        clear   str7
        pack    str7,"Z",str9
        setprop sheet.range(str7,str7),*Value=Formula9,*NumberFormat="###.####%"
.FORMULA10 .Estimated Total Revenue/(Cost) in
        clear   str7
        pack    str7,"AA",str9
        setprop sheet.range(str7,str7),*Value=Formula10,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"AB",str9
        setprop sheet.range(str7,str7),*Value=Exbal,*NumberFormat="##,####0"
.
        clear   str7
        pack    str7,"AC",str9
.START PATCH 2.6 ADDED LOGIC
.        setprop sheet.range(str7,str7),*Value=Revdate,*NumberFormat="mm/dd/yyyy"
          unpack    REVDATE,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
        setprop sheet.range(str7,str7),*Value=str10,*NumberFormat="mm/dd/yyyy"
.END PATCH 2.6 ADDED LOGIC
.
        clear   str7
        pack    str7,"AD",str9
        setprop sheet.range(str7,str7),*Value=statlist
.
        clear   str7
        pack    str7,"Ae",str9
        setprop sheet.range(str7,str7),*Value=statpckcde

.begin patch 2.44
.COLUMN AF Package Date
        Clear           Str7
        pack            str7,"AF",str9
        clear           str256
        pack            str256 with statpanel,StatMDate
        setprop         sheet.range(str7,str7),*Value=STR256,*HorizontalAlignment=AlignLeft

.COLUMN AG RR*AG - HM index
        Clear           Str7
        pack            str7,"AG",str9
        clear           str35
        append          "=Sum(M",Str35
        append          str9 to str35
        append          "*J" to str35
        append          str9 to str35
        append          ")" to str35
        reset           str35
        setprop         sheet.range(str7,str7),*Value=str35,*NumberFormat="##,####0"
.end patch 2.44


        add        c1 to CountOut
          goto       looper
......................................................................................................................
getexch
. temporary     ???????????
             return
. temporary
           MOVE      statlist TO NDATFLD
           rep       zfill in ndatfld
           MOVE        C1 TO NDATPATH
           CALL        NDATKEY
           clear     nxrffld                    .dlh 04feb98
           MOVE      statlist TO NXRFFLD
           REP       ZFILL IN NXRFFLD
           MOVE        C1 TO NXRFPATH
           CLEAR     NXRFMLR
           CALL      NXRFKEY
                   if        over
                   move      c0 to exbal
                   return                        *nothing more to do get out    DLH 23Jul97.
                   endif
           MOVE      C1 TO XFLAG
           CLEAR     NXNGFLD1
           APPEND    "01X" TO  NXNGFLD1
           APPEND    statMLR TO NXNGFLD1
           RESET     NXNGFLD1
           CLEAR     NXNGFLD2
           APPEND    "02X" TO  NXNGFLD2
           APPEND    NXRFMLR TO NXNGFLD2
           RESET     NXNGFLD2
           CALL      NXNGAIM
                     IF        OVER
                     MOVE        C2 TO XFLAG
                     CLEAR     NXNGFLD1
                     APPEND    "01X" TO  NXNGFLD1
                     APPEND    NXRFMLR TO NXNGFLD1
                     RESET     NXNGFLD1
                     CLEAR     NXNGFLD2
                     APPEND    "02X" TO  NXNGFLD2
                     APPEND    statMLR TO NXNGFLD2
                     RESET     NXNGFLD2
                     CALL      NXNGAIM
                               IF        OVER
                               move      c0 to exbal
                               RETURN
                               ENDIF
                     ENDIF
           MOVE        C1 TO NXCHPATH
           PACK      NXCHFLD1 FROM ACCKEY,ENTRY
           REP       ZFILL IN NXCHFLD1
           CALL      NXCHKEY
           if        over                   lets double check. 21sep93.
           move      c0 to exbal
           return
           endif
           SUB       USAGE1 FROM USAGE2
           COMPARE   C1 TO XFLAG
                     IF        EQUAL
                     COMPARE   USAGE2 TO C0
                     GOTO        EVEN  IF EQUAL
                             IF        NOT LESS
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                     ELSE
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                               ENDIF
          ELSE
                     COMPARE USAGE2 TO C0
                     GOTO      EVEN IF EQUAL
                             IF        NOT LESS
                               MULT      SEQ BY USAGE2
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                     ELSE
                               move      c0 to usage2c
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                               mult      seq by exbal        .JD20apr98
                               ENDIF
           ENDIF
           RETURN
EVEN
         move      c0 to exbal

           RETURN

.................................................................
repeoj
.lets only sort if we have records
          if          (recsout > 0)
.Sort by List Name
        pack    str7,"DD",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
        getprop sheet.range("C8"),*Columns(1)=sortcol
        getprop sheet.range("D8"),*Columns(1)=sortcol2
        getprop sheet.range("F8"),*Columns(1)=sortcol3
.        getprop sheet.range("G8"),*Columns(1)=sortcol4
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.        sheet.range("A8",str7).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol2,*Order2=1,*Key3=sortcol3,*Order3=1,*Key4=sortcol4,*Order4=1
        sheet.range("A8",str7).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol2,*Order2=1,*Key3=sortcol3
.......................
.add in total goodies here
.GO DOWN TWO ROWS FROM LAST ENTRY
.
        MOVE    COUNTOUT TO N5
        ADD     C2 TO N5
        MOVE    N5 TO TOTALROW
        MOVE    TOTALROW TO STR8
        CALL    TRIM USING STR8
.NUMBER OF RECORDS OUTPUT
        clear   str7
        pack    str7,"A",str8
        CLEAR   STR35
        APPEND  "=COUNTA(A8:A",STR35
        APPEND  STR9 TO STR35
        append  ")" to str35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*HorizontalAlignment=AlignLeft
        clear   str7
        pack    str7,"B",str8
        setprop sheet.range(str7),*Value="Records",*HorizontalAlignment=AlignLeft

.
.COLUMN i TOTAL mAILED QTY
        clear   str7
        pack    str7,"I",str8
        CLEAR   STR35
        APPEND  "=SUM(I8:I",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##,####0"
.
.overall repsponse rate
        clear   str7
        pack    str7,"J",str8
        CLEAR   STR35
        APPEND  "=SUM(K" to str35
        append  str8 to str35
        append  "/I" to str35
        append  str8 to str35
        append  ")" to str35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##.####%"

.COLUMN "K" TOTAL responses QTY
        clear   str7
        pack    str7,"K",str8
        CLEAR   STR35
        APPEND  "=SUM(K8:K",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##,####0"
.
.COLUMN L TOTAL
        clear   str7
        pack    str7,"L",str8
        CLEAR   STR35
        APPEND  "=SUM(L8:L",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
.COLUMN M TOTAL overall average gift
        clear      str7
        pack       str7,"M",str8
        clear      str35
        append      "=if(k",str35
        append      str8 to str35
        append      ">0,",str35
        append      "l",str35
        append      str8 to str35
        append      "/k",str35
        append      str8,str35
        append      ",0)",str35
        reset       str35
        setprop sheet.range(str7,str7),*Value=str35,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.


.Set up totals Formatting
        clear   str7
        pack    str7 from "A",str8
        pack    str9 from "AA",str8
        setprop sheet.range(str7,str9).Font,*Bold="True"
        sheet.range(str7,str9).BorderAround using *LineStyle=1,*Weight=3
        sheet.range("A1",str9).Columns.AutoFit
        endif
.


.......................................

.
StatFileNameSelect
         setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.no good         setprop books.CommandBars("Worksheet Menu Bar"),*Enabled="True"
        clear   taskname
        append  "\\nins1\d\USERS",taskname
        call    Trim using USER
        if (USER <> "")
                append  "\",taskname
                append  USER,taskname
        endif
        append  "\",taskname
        append  "History",taskname
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                trap    TrapStatObject if Object
                book.saveas giving N9 using *Filename=taskname
.
                trapclr Object
        endif
.START PATCH 2.49 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 2.49 ADDED LOGIC
StatCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sortcol
        destroy sortcol2
        destroy sortcol3
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
        stop

TrapStatobject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
        scan    "Cannot access",str55
        if equal
.Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",carr,"Select another Filename!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto StatFileNameSelect

errortrap
.testing purposes
        getinfo exception,taskname
        return

        return
............................................................................................................
.SetRepDates - check for date goodies
SetRepDates
.
        clear   mm
        clear   dd
        clear   yy
        clear   lodate
        clear   hidate

        getitem StatRepEditText001,0,str10
        clear   mm
        clear   dd
        clear   str2
        clear   yy
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
                    setfocus StatRepEditText001
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus StatRepEditText001
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus StatRepEditText001
                          goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus StatRepEditText001
                                    goto BadDate
                        endif
                endif
        endif
        move    c0 to lodate
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   newdate1
        endif
        setitem StatRepEditText001,0,newdate1
.
        clear   mm
        clear   dd
        clear   yy
        clear   str2
        clear   newdate2
        getitem StatRepEditText002,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
                    setFocus StatRepEditText002
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus StatRepEditText002
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus StatRepEditText002
                        goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus StatRepEditText002
                                goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    NewDate2,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   NewDate2
        endif
        setitem StatRepEditText002,0,NewDate2

        if      (Newdate1  <= b1 and Newdate2 <= b1)
        move    yes to datesw                    - take all dates
              else
        move    no to datesw                    - date select
        endif
        move    c0 to lodate
        move    c0 to hidate
        if      (newdate1 > b1)
        unpack  newdate1 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to lodate
        endif
        if      (newdate2 > b1)
        unpack  newdate2 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to hidate
        endif
        return

BadDate
        return
.end patch 2.3
............................................................................................................
setFocusStatMlr
                setfocus StatsEditText021
        return
............................................................................................................
.begin patch 2.4
DeleteXrefListView
               StatXRefListView002.DeleteAllItems giving N9
               StatXRefListView001.DeleteAllItems giving N9
               return
............................................................................................................
LoadXrefListView
               Close          StatXFLst              .close file(s) to insure we get all
               move           C0 to STatxFlag               .let IO sub no file(s) closed
          setprop   StatXRefProgressBar,visible=1
          call      StatXRefInitProgressBar
          move      "\\nins1\e\data\text\StatXref.dat",str45
          open      tempfile2,str45
          positeof tempfile2
          fposit    tempfile2,N10
          calc      howmany=(N10/52)    .'52 = 50(record length) + 2 bytes for CR/LF
          close     tempfile2
.
          move      "Driver Prep-StatXSEQ",Location
          clearevent

          loop
                    Call           StatXSEQ
                    until          over
                              If             (StatMlr = StatXmlr)
                    StatXRefListView001.InsertItem giving N9 using Statxcode
                         StatXRefListView001.SetItemText using N9,Statxdesc1,1
                         StatXRefListView001.SetItemText using N9,Statxlist,2
                    StatXRefListView001.EnsureVisible using N9,0
.
                    StatXRefListView002.InsertItem giving N9 using Statxdesc1
                    StatXRefListView002.SetItemText using N9,StatxCode,1
                    StatXRefListView002.SetItemText using N9,Statxlist,2
                    StatXRefListView002.EnsureVisible using N9,0
                              endif
                    call      StatXRefUpdateProgressBar

          repeat
.cleanup
          loop
                    clearevent
                    until over
          repeat
          StatXRefListView001.EnsureVisible using 0,0
          StatXRefListView001.SetItemState giving N9 using 0,2,2
          setfocus       StatXRefListView001
.         call      Click_StatXRefListView002
          return
*..............................................................................
StatXRefSortListView Routine FrmPtr,FrmPtr1
.Dynamically sorts Different ListViews.
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
          if (FrmPtr = 1)               .StatXrefListView001
                    if (result = 1)
                              setprop StatXrefListView001,height=0
                              setprop StatXrefListView002,height=200
                              setfocus StatXrefListView002
.                             call      Click_StatXrefListView002
                              endif
          else                          .StatXrefListView002
                              setprop StatXrefListView002,height=0
                              setprop StatXRefListView001,height=200
                              setfocus StatXrefListView001
.                             call      Click_StatXrefListView001
          endif
          return
............................................................................................................
StatXRefupdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   StatXRefProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
StatXRefInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return


.end patch 2.4
............................................................................................................
.debug
.        return
............................................................................................................
BUGGED   DISPLAY    *P1:24,*EL,*HON,*B,"B U G G E D !!!!!"
           STOP
.Following used only in order to load Search.plf
         include   ncmpio.inc
         include   nrtnio.inc
.patch2.7
.         include   nbrkio.inc
.patch2.7
.START PATCH 2.48 ADDED LOGIC
          include   nownio.inc
.END PATCH 2.48 ADDED LOGIC
         include   searchio.inc      .contains logic for search.plf
         include   nordio.inc
..............................................
           include    statsio.inc
.patch2.7
                              include compio.inc
                              include   cntio.inc
.               include    nmlrio.inc
.patch2.7
               include    ndatio.inc
.
.begin patch 2.3
           INCLUDE    NXRFIO.inc
           INCLUDE    NXCHIO.inc
           INCLUDE    NXNGIO.inc
.end patch 2.3
.begin patch 2.32
               include        SlctClnio.inc
.end patch 2.32
           INCLUDE   SXRFIO.inc
           include   NPKGIO.inc
.START PATCH 2.6 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 2.6 ADDED LOGIC
           INCLUDE   COMLOGIC.inc

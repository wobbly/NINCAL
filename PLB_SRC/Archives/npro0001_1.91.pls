PC            EQU             1
              include         COMMON.inc
              include         CONS.inc
              include         winapi.inc
              include         ndat3dd.inc
              include         ndatdd.inc
.Patch1.6
.include nmlrdd.inc
              include         compdd.inc
              include         cntdd.inc
.Patch1.6
              include         nxrfdd.inc
              include         oslspern.inc
              include         CONSACCT.inc
              include         nacddd.inc
.begin patch 1.7
.             include         ninvdd.inc
              include         ninvdd.inc
              INClude         NInvAcddd.inc
.end patch 1.7
              include         norddd.inc
              include         nmrgdd.inc
              include         nbildd.inc
              include         nowndd.inc
              include         nshpdd.inc
              include         gnxtdd.inc
              include         njstdd.inc
              include         nadjdd.inc
.patch1.6
.include      nbrkdd.inc
.patch1.6
              include         nrtndd.inc
              include         nprjdd.inc
.begin patch 1.3
               Include        Tinvdd.inc
               include        NRCHGDD.INC
.end patch 1.3
.Start - Files needed only for Search.plf
              include         ncmpdd.inc
.End   - Files needed only for Search.plf
.START PATCH 1.5 - ADDED LOGIC
              INCLUDE         NSELDD.INC
              INCLUDE         NSEL2DD.INC
              INCLUDE         NTXTDD.INC
.END PATCH 1.5 - ADDED LOGIC
.begin patch 1.8
              Include         Nrevdd.inc
.end patch 1.8
.
.............................................................................
................................................
release   init    "1.91"                DLH   comp code for PLI
Reldate   Init      "04 Dec 2008"
.release  init    "1.9"                 08Mar2007  DLH      Oslspern.inc expansion
.Release       Init            "1.8"          DLH 19Oct2005  add previous year actuals
.release       init      "1.7"        DLH        10March2005  Invoice Conversion
.release  init      "1.69"        ASH           19MAY2005  LISTMLR Conversion
.release  init      "1.68"        ASH           07APR2005  COMMPER Conversion
.release  init      "1.67"        ASH           17NOV2004  Mailer Conversion
.release  init      "1.66"        ASH           09AUG2004  Logo Conversion
.release  init      "1.65"        JD            09JUn2004  Refix required trim of datacard var Commper
.RELEASE  INIT      "1.6"          26MAY04 DMB Mailer Conversion
.RELEASE  INIT      "1.5"          29jan04 ASH DATACARD CONVERSION
.release init    "1.4"           04DEC02 ASH     Added small fix
.release init    "1.2"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release  init      "1.1"       15NOV02 ASH ADDED FEATURES
.release  init      "1.0"       03October2000 DLH first implementation.

.;begin patch 1.3
.;output   ifile     keylen=8,fix=692,NODUPLICATES
.begin patch 1.91
.output        ifile           keylen=8,fix=1006,NODUPLICATES
output        ifile           keylen=8,fix=1007,NODUPLICATES
.end patch 1.91
output2       ifile           keylen=15,fix=210
.;
Form122        Form           12.2
.end patch 1.3
key15    dim       15   *FILE KEY.
PrjStr6       Dim             6
key8     dim       8
key6     dim       6
ANS      DIM       1
TIPE     DIM       1
CHECK    FORM      1
SOURCE   DIM       1
M        INIT      "M"
B        INIT      "B"
R        INIT      "R"
owner    dim       4
INCNAME  DIM       45        11-55
ARTOT    FORM      10.2        56-68
APTOT    FORM      10.2        69-81
LRTOT    FORM      10.2        82-94
NINTOT    FORM      10.2       95-107
QTYTOT   FORM      8          108-115
ADJARTOT FORM      10.2       116-128
ADJAPTOT FORM      10.2       129-141
ADJLRTOT FORM      10.2       142-154
ADJNINTOT FORM      10.2      155-167
unbill   form      8.2        168-190
unbilinc form      8.2       110-120
.begin patch 1.91
PCompCde  Dim       1         .company code "P" = Pacific Lists  ... pulled from company and datacard files
.end patch 1.91
.begin patch 1.3
TDMCTOT  FORM      10.2
.end patch 1.3
.
LRcalc   FORM      10.2
NINcalc  FORM      10.2
.begin patch 1.3
TDMCcalc  FORM      10.2
DownloadFlag  Dim             1                  :if yes ignore timeout
.end patch 1.3
.output record
.byte 1 nothing
.source   dim      1                2-2
.ID       dim      6                3-8
.name     dim      45               9-53
.owner    dim      4               54-57
jan1     FORM      10.2            58-70
feb1     FORM      10.2
mar1     FORM      10.2
apr1     FORM      10.2
may1     FORM      10.2
jun1     FORM      10.2
jul1     FORM      10.2
aug1     FORM      10.2
sep1     FORM      10.2
oct1     FORM      10.2
nov1     FORM      10.2
dec1     FORM      10.2
janNIN1  FORM      10.2
febNIN1  FORM      10.2
marNIN1  FORM      10.2
aprNIN1  FORM      10.2
mayNIN1  FORM      10.2
junNIN1  FORM      10.2
julNIN1  FORM      10.2
augNIN1  FORM      10.2
sepNIN1  FORM      10.2
octNIN1  FORM      10.2
novNIN1  FORM      10.2
decNIN1  FORM      10.2
janx1    FORM      10.2           308-
febx1    FORM      10.2
marx1    FORM      10.2
aprx1    FORM      10.2
mayx1    FORM      10.2
junx1    FORM      10.2
julx1    FORM      10.2
augx1    FORM      10.2
sepx1    FORM      10.2
octx1    FORM      10.2
novx1    FORM      10.2
decx1    FORM      10.2
janNINx1 FORM      10.2
febNINx1 FORM      10.2
marNINx1 FORM      10.2
aprNINx1 FORM      10.2
mayNINx1 FORM      10.2
junNINx1 FORM      10.2
julNINx1 FORM      10.2
augNINx1 FORM      10.2
sepNINx1 FORM      10.2
octNINx1 FORM      10.2
novNINx1 FORM      10.2
decNINx1 FORM      10.2               681
unbilled form      8.2            682-692
.begin patch 1.3
janTDMC     FORM      10.2            58-70
feBTdmc     FORM      10.2
marTDMC     FORM      10.2
aprTDMC     FORM      10.2
mayTDMC     FORM      10.2
junTDMC     FORM      10.2
julTDMC     FORM      10.2
augTDMC     FORM      10.2
sepTDMC     FORM      10.2
octTDMC     FORM      10.2
novTDMC     FORM      10.2
decTDMC     FORM      10.2
janTDMCx     FORM      10.2            58-70
feBTdmcx     FORM      10.2
marTDMCx     FORM      10.2
aprTDMCx     FORM      10.2
mayTDMCx     FORM      10.2
junTDMCx     FORM      10.2
julTDMCx     FORM      10.2
augTDMCx     FORM      10.2
sepTDMCx     FORM      10.2
octTDMCx     FORM      10.2
novTDMCx     FORM      10.2
decTDMCx     FORM      10.2
.end patch 1.3

Total         form      10.2
RTotal        form      10.2
ETotal        form      10.2
NINTotal    form      10.2
RNINTotal   form      10.2
ENINTotal   form      10.2
.begin patch 1.3
TDMCTotal   Form      10.2
.end patch 1.3
Mpercent      form      10.4
rpercent      form      10.4
Epercent      form      10.4
NINperc       form      10.4
.begin patch 1.3
TdmcPErc Form         10.4
.end patch 1.3
percent  dim       16
....................................................................

.hold vars
projnew       form      11          hold exchange portion  LR
NINnew        form      11          hold exchange portion NIN
projlast      form      11          hold exchange portion  LR  for requested prev Year
NINlast       form      11          hold exchange portion NIN  for requested Previous Year
projlast1e  form      11          hold exchange portion projected LR  for Previous Year
NINlast1e   form      11          hold exchange portion projected NIN  for Previous Year
projlast1r  form      11          hold rent portion projected  LR  for Previous Year
NINlast1r   form      11          hold rent portion projected NIN  for Previous Year
NINActlstE    Form            11
.begin patch 1.8
ActLRE     form      11          hold exchange portion Actual LR  for Previous Year
ActNINE    form      11          hold exchange portion Actual NIN  for Previous Year
ActLrR     form      11          hold rent portion Actual  LR  for Previous Year
ActNINR    form      11          hold rent portion Actual NIN  for Previous Year
.end patch 1.8
.......................................................................
.SORTVAR       INIT              "c:\data\average7.dat,c:\data\average7.srt;2,9-53"
SORTVAR        INIT              "c:\work\average7.dat,c:\work\average7.srt;2,9-53"
countout      form      5
sheetno       form      2
Hmany1        form      9
Hmany2        form      9
Hmany3        form      9
Hmany4        form      9
Hmany5        form      9
Hmany6        form      9
Hmany7        form      9
Hmany8        form      9
.begin patch 1.3
Hmany9        form      9
Hmany10       form      9
Hmany11       form      9
.end patch 1.3
NumberofSheets Integer        4,"0x00000000"
xlMinimized   integer 4,"0xFFFFEFD4"
endrow        form      9
.............................................................................................
.some excel goodies
books         automation
book          automation
sheets        automation
sheet         automation
rowcol        automation
ex            automation      class="Excel.Application"
RecordHeader  form 9
RecordTop     form  9
N34           form    3.4
N92           form    9.2
MailDate      dim    4
.hexeight integer 4,"4294967295"
VT_BOOL       EQU 11          .Boolean
OTRUE         variant
OFALSE        variant
VT_I4         EQU 3           .4 byte integer
Zoom95        variant
VT_R8         EQU 5           .Double - 8 byte Real
Inch25         variant         ..25 Inch
Inch50         variant         ..50 Inch
Inch100        variant         .1.0 Inch
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
xlHAlignLeft integer 4,"0xffffefdd"             .-4131
xlHAlignCenter integer 4,"0xffffeff4"           .-4108
xlEdgeTop integer 4,"0x8"                       .8
xlEdgeBottom integer 4,"0x9"                    .9
xlEdgeRight integer 4,"0xA"                     .10
xlContinuous integer 4,"0x1"                    .1 - Doubles for xlPaperLetter, xlDownThenOver
xlDouble integer 4,"0xffffefe9"                 .-4119
xlLandscape integer 4,"0x2"                     .2
xlPrintNoComments integer 4,"0xffffefd2"        .-4142
IntIndex      integer 4
IntIndex2     integer 4
IntIndex3     integer 4
IntIndex4     integer 4
IntIndex5     integer 4
IntIndex6     integer 4
IntIndex7     integer 4
ColHeads      automation
ColHead       automation
ListIts       automation
ListIts2      automation
ListIt        automation
ListIt2       automation
SubIt         automation
$KeyUp        CONST  "4294966692"
badorder      dim             1
.............................................................................................
.....................................BEGIN NEW GUI LOGIC.....................................
.............................................................................................
.tempfile file
tempfile2 file
.output2      ifile           keylen=15,fix=190
DUPEOWN       ifile           keylen=4
DUPE1         dim             1     5-5
NEWOLON       dim             4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
SEQEOF2       form            "-4"
CurRec        form    5.2
CurVal        form            3
LastVal       form            3
SIX           form            "6"
SALESNUM form 2
SALENUM       dim             2
HBILLKEY dim  8    *FOR MATCH MLR/BILL-TO BREAK?
mrgsw         dim             1
shipsw        dim             1
PASS          form            1
ADJAR         form            7.2
ADJLR         form            7.2
ADJNIN        form            7.2
ADJAP         form            9.2
SPLITSW       dim             1                  RENT/EXCHANGE SPLIT = 'Y'
MDATE         form            5
.CHKJUL       form            5
JUNDATE       form            6
QTYCHK        form            9
TENDOLL       form            "99999"
NINEDOLL form "199999"
SEVDOLL       form            "300000"
JUNEDAT       init            "060191"
SYSJDATE form 5
SYSMO         dim             2
SYSDY         dim             2
SYSYR         dim             2
SYSCC         dim             2
UNBILAMT form 9.2
form52        form            5.2
specl         dim             1
card$         form            5.2
net92         form            9.2            holding field while calcing net charges

DimPtr        dim             ^
DimPtr1       dim             ^
FrmPtr        form            ^
FrmPtr1       form            ^
EditPtr       EditText ^
AutPtr        automation      ^

salesper dim    25
FromDate dim  8
ToDate        dim             8
.begin patch 1.3
fromdate6      dim            6
ToDate6        Dim            6
.end patch 1.3
TabNum        form            "001"

StopFlag dim  1
StopFlag2 dim 1
ExitFlag init "Y"
ReturnFlag dim 1
NewFlag       dim             1
ClientFlag form               1              .Used to determine which EditText box called Search.plf
MergeFlag form 1              .Used to determine whether Brokerage records should be concatenated
MergeCtr form 1
SalesFlag form 2
FirstYear init "1992"
LastYear dim  4
PrevYear dim  4
FromYear dim  4
ToYear        dim             4
Search3Type dim 1
Search3Src dim 1
Search3Sls dim 2
TIMER         timer

specs   form  4(4)
sizeH         form            "1.000"
sizeV         form            "1.000"
ScrH          form            4
ScrV          form            4
infostring dim 590
font5         font
.START PATCH 1.5 - ADDED LOGIC
TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
.END PATCH 1.5 - ADDED LOGIC

.Colors
white   color
grey    color

.Set Up Menu Bar
mFile   menu
mEdit   menu
mOptions menu
mReports menu
mHelp   menu
.Menu Bar for Report Screen
mRSearch menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2"
RData   init    "&Reports;"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&List;&Mailer"

.Set Vars used for About Box
        move    "NPRO0001.PLS",Wprognme
        move    "Income Projection Program",Wfunction
        move    "Andrew Harkins",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate


one     plform  Search
mss1    plform  Error
abt     plform  About
x       plform  \\nins1\e\library\plb_Src\Npro0001
Npro0002 plform Npro0002
Npro0003 plform Npro0003
              winhide
              formload x
              formload Npro0002,Npro0001
              formload Npro0003,Npro0001
              formload abt
              formload mss1
              formload one

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
.Create font used for dynamic resizing of objects
              create  font5,"Arial",size=12
.Timer creation
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT

.Create Menus
              create  Npro0001;mFile,FData
              create  Npro0001;mEdit,EData,mFile
              create  Npro0001;mOptions,OData,mEdit
              create  Npro0001;mReports,RData,mOptions
              create  Npro0001;mHelp,HData,mReports
.Create SubMenu
        create  Npro0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mOptions,OptionsGo,result
        activate mReports,ReportGo,result
        activate mHelp,HelpGo,result

.Activate SubMenus
        activate sSearch,SearchGo,result

.Load up Objects
.Delete blank entries
              deleteitem Proj2ComboSales,0
              deleteitem Proj3ComboSales,0
        insertitem Proj2ComboSales,N2,str45
        insertitem Proj3ComboSales,N2,str45
        loop
                move    osls0,salesper
                          
                load    salesper from N2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                        osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                        osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
                if (salesper <> OSLS0)
                        call    Trim using salesper
                        if (salesper <> "")
                                pack    str35,salesper,B55
                                move    N2,str2
                                rep     zfill,str2
                                pack    str45,str35,B1,str2
                                add     C1,N3
                                      insertitem Proj2ComboSales,N3,str45
                                      insertitem Proj3ComboSales,N3,str45
                        endif
                endif
                add     C1,N2
                until (N2 > 35)
        repeat
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom95,VarType=VT_I4,VarValue=95
.Convert Inches to Points.  There are 72 Points to an Inch
        create  Inch25,VarType=VT_R8,VarValue="18"      ..25 Inch
        create  Inch50,VarType=VT_R8,VarValue="36"      ..5 Inch
        create  Inch100,VarType=VT_R8,VarValue="72"     .1.0 Inch
.Create Proj2ListView/2 Columns
              call            Proj2CreateListViewColumns
        getprop Proj2ListView2,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
              ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
              ColHeads.Add using *Index=2,*Key="one",*Text="Type",*Width=37
              ColHeads.Add using *Index=3,*Key="two",*Text="Source",*Width=50
              ColHeads.Add using *Index=4,*Key="three",*Text="Client",*Width=50
              ColHeads.Add using *Index=5,*Key="four",*Text="Client Name",*Width=70
              ColHeads.Add using *Index=6,*Key="five",*Text="Year",*Width=45
              ColHeads.Add using *Index=7,*Key="six",*Text="Num.",*Width=37
              ColHeads.Add using *Index=8,*Key="seven",*Text="LR Income",*Width=70,*Alignment=1
              ColHeads.Add using *Index=9,*Key="eight",*Text="NIN Income",*Width=70,*Alignment=1
              ColHeads.Add using *Index=10,*Key="nine",*Text="Date",*Width=70
              ColHeads.Add using *Index=11,*Key="ten",*Text="Notes",*Width=100
              ColHeads.Add using *Index=12,*Key="eleven",*Text="Mod. Date",*Width=65
              ColHeads.Add using *Index=13,*Key="twelve",*Text="SalesPerson",*Width=65
              ColHeads.Add using *Index=14,*Key="thirteen",*Text="Master",*Width=20
              ColHeads.Add using *Index=15,*Key="fourteen",*Text="Sort Key",*Width=0
              ColHeads.Add using *Index=16,*Key="fifteen",*Text="Sort Key2",*Width=0
.
              setprop         Proj2ListView2,*HideColumnHeaders=OFALSE
              setprop         Proj2ListView2,*HideSelection=OFALSE
.             setprop         Proj2ListView2,*HotTracking=OTRUE
              setprop         Proj2ListView2,*FullRowSelect=OTRUE
.             setprop         Proj2ListView2,*MultiSelect=OTRUE
              setprop         Proj2ListView2,*Sorted=OTRUE
              setprop         Proj2ListView2,*SortOrder=1
              setprop         Proj2ListView2,*SortKey=14
              setprop         Proj2ListView2,*AllowColumnReorder=OTRUE
              setprop         Proj2ListView2,*LabelEdit=1
              setprop         Proj2ListView2,*View=3
.             setprop         Proj2ListView2,*Font=font2
.
        getprop Proj2ListView2,*ListItems=ListIts
........................................
........................................
.             getprop Proj2ListView,*ColumnHeaders=ColHeads
.             getprop         ColHeads,*Count=howmany
.             getprop         Proj2ListView,*ListItems=ListIts
.             ListIts.Add giving ListIt using *Index=1,*Text=str8
.             sub             C1,howmany
.             for IntIndex2,"5",howmany
.              move           IntIndex2,N10
.              move           N10,str14
.              setprop ListIt,*SubItems(IntIndex2)=str14
.             repeat
........................................
........................................
.
              clock           timestamp,timestamp
              unpack          timestamp,syscc,sysyr,sysmo,sysdy
              move            sysdy,DD
              move            sysmo,MM
              move            sysyr,YY
              call            datetest
.cheat  n2 now holds last day of the month
              move            n2,sysdy
              rep             ZFILL,sysdy
              rep             ZFILL,sysmo
.Set default for Previous Year allowing modification
              pack            str4,syscc,sysyr
              rep             zfill,str4
              setitem         Proj3EditPrevYr,0,str4
.
              call            Proj2DisableDetail
.
              loop
               waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
.Test to make sure nothing is left open in Modify Mode
              if              (downLoadFlag = yes)
                              setitem timer,0,18000   .reset to 30 minutes
                              return
              endif                
        getprop Proj2Edit2Client,enabled=N9
        if (N9 = 1)
                call    Click_Proj2Quit
        endif
        beep
        beep
        beep
        shutdown


FileGo
.ExitFlag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo2

FileGo1
              return

FileGo2
              if (ExitFlag = NO)
               return
              endif
              winshow
              shutdown
EditGo
              return
OptionsGo
SearchGo
        branch  result to SearchGo2,SearchGo3
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
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST
              unpack  Srchstr,PrjStr6,str1,str35
              if (ClientFlag = 0)
               setitem Proj2EditClient,0,PrjStr6
               setitem        Proj2ComboSource,0,3
               setitem        Proj2ComboType,0,1
                setfocus Proj2EditClient
        else
                setitem Proj2Edit2Client,0,PrjStr6
               setitem        Proj2Stat2ClientName,0,str35
               setitem        Proj2Combo2Source,0,3
                setfocus Proj2Edit2Client
        endif
        return
SearchLoad3
.MAILER
.START PATCH 1.67 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
.             if (ClientFlag = 0)
.              setitem Proj2EditClient,0,str4
.              setitem        Proj2ComboSource,0,2
.                setfocus Proj2EditClient
.        else
.                setitem Proj2Edit2Client,0,str4
.              setitem        Proj2Stat2ClientName,0,str45
.              setitem        Proj2Combo2Source,0,2
.                setfocus Proj2Edit2Client
.        endif
        unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,PrjStr6
              if (ClientFlag = 0)
               setitem Proj2EditClient,0,PrjStr6
               setitem        Proj2ComboSource,0,2
                setfocus Proj2EditClient
        else
                setitem Proj2Edit2Client,0,PrjStr6
               setitem        Proj2Stat2ClientName,0,str45
               setitem        Proj2Combo2Source,0,2
                setfocus Proj2Edit2Client
        endif
.END PATCH 1.67 REPLACED LOGIC
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
ReportGo
              return
HelpGo
        branch  result to HelpGo1
HelpGo1
        setprop AboutMssg,visible=1
        return


Proj2ClearSearchFields
              setitem         Proj2StatType2,0,""
              setitem         Proj2StatSource2,0,""
              setitem         Proj2StatClient2,0,""
              setitem         Proj2StatFromYr2,0,""
              setitem         Proj2StatToYr2,0,""
              setitem         Proj2StatRecords,0,""
              setitem         Proj2Stat2Records,0,""
              Proj2ListView.ListItems.Clear
              Proj2ListView2.ListItems.Clear
              return

Proj3ClearSearchFields
              setitem         Proj3StatType2,0,""
              setitem         Proj3StatSource2,0,""
              setitem         Proj3StatFromDate2,0,""
              setitem         Proj3StatToDate2,0,""
              setitem         Proj3StatPrevYr2,0,""
              return

Proj2ClearDetail
              setitem         Proj2Check2Master,0,0
              setitem         Proj2Combo2Source,0,1
              setitem         Proj2Combo2Type,0,1
              setitem         Proj2Edit2Client,0,""
              setitem         Proj2Edit2Date,0,""
              setitem         Proj2Edit2Key,0,""
              setitem         Proj2Edit2LRInc,0,""
              setitem         Proj2Edit2NINInc,0,""
              setitem         Proj2Edit2Notes,0,""
              setitem         Proj2Edit2Year,0,""
              setitem         Proj2Stat2ClientName,0,""
              setitem         Proj2Stat2ModDate2,0,""
              setitem         Proj2Stat2Sales2,0,""
              return

Proj2DisableUpperButtons
              setprop         ProjExit,enabled=0
              setprop         Proj2OK,enabled=0
              setprop         Proj2New,enabled=0
              setprop         Proj2Modify,enabled=0
              return
Proj2EnableUpperButtons
              setprop         Proj2OK,enabled=1
              setprop         Proj2New,enabled=1
              setprop         ProjExit,enabled=1
              move            YES,ExitFlag
              return
Proj2DisableLowerButtons
              setprop         Proj2Delete,enabled=0
              setprop         Proj2Save,enabled=0
              setprop         Proj2Quit,enabled=0
              return
Proj2EnableLowerButtons
              setprop         Proj2Delete,enabled=1
Proj2EnableLowerButtons2
              setprop         Proj2Save,enabled=1
              setprop         Proj2Quit,enabled=1
              return

Proj2EnableDetail
.Only available for New Records
              setprop         Proj2Combo2Source,enabled=1,bgcolor=white
              setprop         Proj2Combo2Type,enabled=1,bgcolor=white
              setprop         Proj2Edit2Client,enabled=1,bgcolor=white
              setprop         Proj2Edit2Year,enabled=1,bgcolor=white
              setprop         Proj2Edit2Key,bgcolor=white
Proj2EnableDetail2
              move            NO,ExitFlag
              setprop         Proj2Check2Master,enabled=1
              setprop         Proj2Edit2Date,bgcolor=white
              setprop         Proj2Edit2LRInc,enabled=1,bgcolor=white
              setprop         Proj2Edit2NINInc,enabled=1,bgcolor=white
              setprop         Proj2Edit2Notes,readonly=0
              return

Proj2DisableDetail
              setprop         Proj2Check2Master,enabled=0
              setprop         Proj2Combo2Source,enabled=0,bgcolor=grey
              setprop         Proj2Combo2Type,enabled=0,bgcolor=grey
              setprop         Proj2Edit2Client,enabled=0,bgcolor=grey
              setprop         Proj2Edit2Date,enabled=0,bgcolor=grey
              setprop         Proj2Edit2Key,enabled=0,bgcolor=grey
              setprop         Proj2Edit2LRInc,enabled=0,bgcolor=grey
              setprop         Proj2Edit2NINInc,enabled=0,bgcolor=grey
              setprop         Proj2Edit2Notes,readonly=1
              setprop         Proj2Edit2Year,enabled=0,bgcolor=grey
              move            NO,NewFlag
              return

ProjLoadProjections Routine FrmPtr
              move            NO,StopFlag
        getprop Proj2ListView,*ColumnHeaders=ColHeads
              if (FrmPtr = 3)                .SalesPerson Read
               if (SalesFlag = 6)            .List Management
                              pack           NPRJFLD2,"02XM"
                              goto ProjLoadProjections2
               else
                              pack           NPRJFLD2,"02XB"
                              goto ProjLoadProjections2
               endif
              elseif (FrmPtr = 2)            .Sequential File Read
               if (NPRJFLG2 <> 1)
                              call           NPRJOPN2
               endif
               reposit        NPRJFLSQ,C0
               move           "ProjL.Proj.-NPRJSEQ",Location
               pack           KeyLocation,"Key: Sequential"
               loop
                              call           NPRJSEQ
                              until over
                              call           ProjLoadListView
                              checkevent
                              if (StopFlag = YES)
                                             break
                              endif
               repeat
              elseif (FrmPtr = 1)            .Aim read via Years
               move           C0,N4
               if (FromYear = "")
                              move           FirstYear,N4
               else
                              move           FromYear,N4
               endif
               loop
                              move           N4,str4
                              if ((ToYear <> "" AND str4 > ToYear) | str4 > LastYear)
                                             break
                              endif
                              pack           NPRJFLD4,"04X",str4
                              move           "ProjL.Proj.-NPRJAIM",Location
                              pack           KeyLocation,"Key: ",NPRJFLD4
                              call           NPRJAIM
                              loop
                                             until over
                                             call           ProjLoadListView
                                             checkevent
                                             if (StopFlag = YES)
                                                            break
                                             endif
                                             move           "ProjL.Proj.-NPRJKG",Location
                                             call           NPRJKG
                              repeat
                              checkevent
                              if (StopFlag = YES)
                                             break
                              endif
                              add            C1,N4
               repeat
              else                                          .Regular Aim read
ProjLoadProjections2
               move           "ProjL.Proj.2-NPRJAIM",Location
               pack           KeyLocation,"Key: ",NPRJFLD1,NPRJFLD2,NPRJFLD3
               call           NPRJAIM
               loop
                              until over
                              call           ProjLoadListView
                              checkevent
                              if (StopFlag = YES)
                                             break
                              endif
                              move           "ProjL.Proj.2-NPRJKG",Location
                              call           NPRJKG
               repeat
              endif
              getprop         ListIts,*Count=N9
              if (N9 <> C0)
               move           N9,str9
               call           Trim using str9
               call           FormatNumeric using str9,str11
               pack           str35,str11," Record(s) Found"
               setitem        Proj2StatRecords,0,str35
               setprop        ListIts(1),*Selected=C1
               call           Click_Proj2ListView
              endif
              return

ProjLoadListView
.See if it already exists in ListView
              if (MergeFlag = 1)
               pack           str8,ProjSrc,ProjClient
              else
               pack           str8,ProjType,ProjSrc,ProjClient
              endif
              call            Trim using str8
              getprop         Proj2ListView,*ListItems=ListIts
              getprop         ListIts,*Count=N9
              if (N9 <> C0)
               move           C0,N2
               for IntIndex3,C1,N9
                              getprop        ListIts(IntIndex3),*Text=str9
                              call           Trim using str9
                              if (str9 = str8)
                                             getprop        ListIts,*Item(IntIndex3)=ListIt
                                             call           ProjAddNewSubItem
                                             move           C1,N2
                                             break
                              endif
               repeat
               if (N2 = C0)
                              call           ProjAddNewItem
               endif
.What the heck do I need to do to get this piece of code to work????????
.              Proj2ListView.FindItem giving ListIt using str8,1,1,0
.              Proj2ListView.FindItem using str8,1,1,0
..             getprop        ListIt,*ClassID=str25
.              if (str25 <> "")
..Entry found
.              else
..Add New Item
.                             call           ProjAddNewItem
.              endif
              else
.Add New Item
               call           ProjAddNewItem
              endif
              return

ProjAddNewItem
              if (SalesFlag > 0)
               if (ProjSrc = "B")            .Brokerage
.START PATCH 1.67 REPLACED LOGIC
.                             unpack         ProjClient,str2,str4
.                             move           C1,NMLRPATH
.                             pack           MKEY,str4,"000"
.                             move           "ProjAddNew-NMLRKEY",Location
.                             pack           KeyLocation,"Key: ",MKEY
.                             call           NMLRKEY
.                             if not over
.                                            move           SalesFlag,str2
.                                            rep            zfill,str2
.                                            if (str2 <> MSLSPER)
.                                                           return
.                                            endif
.                             else
.                                            return
.                             endif
...................................................
                              pack           COMPFLD,ProjClient
                              move           "ProjAddNew-COMPKEY",Location
                              pack           KeyLocation,"Key: ",COMPFLD
                              call           COMPKEY
                              if not over
                                             move           SalesFlag,str2
                                             rep            zfill,str2
                                             if (str2 <> compcontact)
                                                            return
                                             endif
                              else
                                             return
                              endif
.END PATCH 1.67 REPLACED LOGIC
               endif
              endif
              call            ProjAddNewItem2
              call            ProjAddNewSubItem
              return

ProjAddNewItem2
              if (MergeFlag = 1)
               pack           str8,ProjSrc,ProjClient
              else
               pack           str8,ProjType,ProjSrc,ProjClient
              endif
              ListIts.Add giving ListIt using *Index=1,*Text=str8
              call            ProjAddNewItem3 using ListIt,MergeFlag
              return

ProjAddNewItem3 Routine AutPtr,FrmPtr
              if (FrmPtr = C1)               .MergeFlag
               setprop AutPtr,*SubItems(1)=B1
              else
               setprop AutPtr,*SubItems(1)=ProjType
              endif
              setprop AutPtr,*SubItems(2)=ProjSrc
              if (ProjSrc = "B")             .Brokerage
.START PATCH 1.67 REPLACED LOGIC
.              unpack         ProjClient,str2,str4
.              setprop AutPtr,*SubItems(3)=str4
.              move           C1,NMLRPATH
.              pack           MKEY,str4,"000"
.              move           "NewItem-NMLRKEY",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              if not over
.                             setprop AutPtr,*SubItems(4)=MCOMP
.              endif
..........................................
               setprop AutPtr,*SubItems(3)=ProjClient
               pack           COMPFLD,ProjClient
               move           "NewItem-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               if not over
                              setprop AutPtr,*SubItems(4)=COMPCOMP
               endif
.END PATCH 1.67 REPLACED LOGIC
              else                                          .List Management
               setprop AutPtr,*SubItems(3)=ProjClient
               move           C1,NDATPATH
               pack           NDATFLD,ProjClient
               move           "NewItem-NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               call           NDATKEY
               if not over
                              setprop AutPtr,*SubItems(4)=OLSTNAME
.begin patch 1.65
.START PATCH 1.68 REMOVED LOGIC
.               call           Trim using Commper
.END PATCH 1.68 REMOVED LOGIC
.ARGH
.end patch 1.65
               endif
              endif
              return

ProjAddNewSubItem
              move            C0,N1
              getprop         ColHeads,*Count=howmany
              for IntIndex,C6,howmany
               sub            C1,IntIndex,IntIndex2
               getprop        ColHeads.Item(IntIndex),*Text=str11
               unpack         str11,str4,str5,str2
               if (str4 = ProjYr AND str2 = ProjKey)
                              call           Trim using str5
                              if (str5 = "LR")
                                             if (MergeFlag = 1)
                                                            getprop ListIt,*SubItems(IntIndex2)=str14
                                                            call           RemoveChar using str14,COMMA
                                                            move           C0,N11
                                                            move           str14,N11
                                                            add            ProjLR,N11
                                                            move           N11,str11
                                             else
                                                            move           ProjLR,str11
                                             endif
                                             call           FormatNumeric using str11,str14
                                             setprop ListIt,*SubItems(IntIndex2)=str14
                                             add            C1,N1
                              else
                                             if (MergeFlag = 1)
                                                            getprop ListIt,*SubItems(IntIndex2)=str14
                                                            call           RemoveChar using str14,COMMA
                                                            move           C0,N11
                                                            move           str14,N11
                                                            add            ProjNIN,N11
                                                            move           N11,str11
                                             else
                                                            move           ProjNIN,str11
                                             endif
                                             call           FormatNumeric using str11,str14
                                             setprop ListIt,*SubItems(IntIndex2)=str14
                                             add            C1,N1
                              endif
                              if (N1 > C1)
                                             goto AfterLoop
                              endif
               endif
              repeat
AfterLoop
              return

ProjLoadListView2
.ListIts Collection still referring to Proj2ListView object.
.IntIndex still referring to first selected item
.Pulling part of key value from "Combined" record
              getprop         ListIts(IntIndex),*Text=str8
              getprop         ListIts,*Item(IntIndex)=ListIt
.Associate ListIts2 with Proj2ListView2
              getprop         Proj2ListView2,*ListItems=ListIts2
              getprop Proj2ListView,*ColumnHeaders=ColHeads
              getprop         ColHeads,*Count=howmany
              sub             C1,howmany
              for IntIndex2,"5",howmany,"2"
               add            C1,IntIndex2,IntIndex3
               getprop        ListIt,*SubItems(IntIndex2)=str14
               call           Trim using str14
               getprop        ListIt,*SubItems(IntIndex3)=str15
               call           Trim using str15
               if ((str14 <> "" and str14 <> "0") OR (str15 <> "" and str15 <> "0"))
                              getprop        ColHeads(IntIndex3),*Text=str11
                              unpack         str11,ProjYr,str5,ProjKey
                              move           C0,MergeCtr
                              loop
                                             if (MergeFlag = 1)
                                                            call           Trim using str8
                                                            if (MergeCtr = 0)
                                                                           pack           NPRJFLD,"E",str8,ProjYr,ProjKey
                                                                           add            C1,MergeCtr
                                                            else
                                                                           pack           NPRJFLD,"R",str8,ProjYr,ProjKey
                                                                           add            C1,MergeCtr
                                                            endif
                                             else
                                                            packkey        NPRJFLD,str8,ProjYr,ProjKey
                                                            move           C2,MergeCtr
                                             endif
                                             move           C1,NPRJPATH
                                             move           "LoadLV2-NPRJKEY",Location
                                             pack           KeyLocation,NPRJFLD
                                             call           NPRJKEY
                                             if not over
.                                                           ListIts2.Add giving ListIt2 using *Index=1,*Text=NPRJFLD
                                                            ListIts2.Add giving ListIt2 using *Text=NPRJFLD
                                                            call           ProjAddNewItem3 using ListIt2,C0
                                                            setprop ListIt2,*SubItems(5)=ProjYr
                                                            setprop ListIt2,*SubItems(6)=ProjKey
                                                            move           ProjLR,str11
                                                            call           FormatNumeric using str11,str14
                                                            setprop ListIt2,*SubItems(7)=str14
                                                            move           ProjNIN,str11
                                                            call           FormatNumeric using str11,str14
                                                            setprop ListIt2,*SubItems(8)=str14
                                                            call           Trim using ProjDate
                                                            if (ProjDate <> "")
                                                                           unpack         ProjDate,str4,MM,DD
                                                                           pack           str10,MM,SLASH,DD,SLASH,str4
                                                            else
                                                                           clear          str10
                                                            endif
                                                            setprop ListIt2,*SubItems(9)=str10
                                                            setprop ListIt2,*SubItems(10)=ProjNotes
                                                            call           Trim using ProjMod
                                                            if (ProjMod <> "")
                                                                           unpack         ProjMod,str4,MM,DD
                                                                           pack           str10,MM,SLASH,DD,SLASH,str4
                                                            else
                                                                           clear          str10
                                                            endif
                                                            setprop ListIt2,*SubItems(11)=str10
                                                            if (ProjType = "E")
                                                                           move           C1,str1
                                                            else
                                                                           move           C0,str1
                                                            endif
                                                            if (ProjSrc = "B")
.START PATCH 1.67 REPLACED LOGIC
.                                                                          call           Proj2GetSales using MSLSPER,str35
                                                                           call           Proj2GetSales using COMPCONTACT,str35
.END PATCH 1.67 REPLACED LOGIC
                                                            else
                                                                           move           "List Management",str35
                                                            endif
                                                            setprop ListIt2,*SubItems(12)=str35
                                                            setprop ListIt2,*SubItems(13)=ProjMast
                                                            pack           str7,ProjYr,ProjKey,str1
                                                            setprop ListIt2,*SubItems(14)=str7
                                                            pack           str7,ProjType,ProjYr,ProjKey
                                                            setprop ListIt2,*SubItems(15)=str7
                                                            if (ProjMast = YES)
                                                                           for IntIndex4,"1","15"
                                                                                          setprop        ListIt2.ListSubItems(IntIndex4),*Bold=OTRUE
                                                                           repeat
                                                            endif
                                             endif
                                             until (MergeCtr = 2)
                              repeat
               endif
              repeat
.In order for the sort to apply to last item, I need to add one extra dummy item.
              getprop ListIts2,*Count=result
              if (result > C0)
               move           result,str9
               call           Trim using str9
               call           FormatNumeric using str9,str11
               pack           str35,str11," Record(s) Found"
               setitem        Proj2Stat2Records,0,str35
               ListIts2.Add
               setprop        ListIts2(1),*Selected=C1
               call           Click_Proj2ListView2
              endif
              return

Proj2LoadDetail
              getprop ListIt2,*SubItems(1)=ProjType
              move            C1,N1
              if (ProjType = "E")
               move           C2,N1
              elseif (ProjType = "R")
               move           C3,N1
              endif
              setitem         Proj2Combo2Type,0,N1
              getprop ListIt2,*SubItems(2)=ProjSrc
              move            C1,N1
              if (ProjSrc = "B")
               move           C2,N1
              elseif (ProjSrc = "M")
               move           C3,N1
              endif
              setitem         Proj2Combo2Source,0,N1
              getprop ListIt2,*SubItems(3)=ProjClient
              setitem         Proj2Edit2Client,0,ProjClient
              getprop ListIt2,*SubItems(4)=str45
              setitem         Proj2Stat2ClientName,0,str45
              getprop ListIt2,*SubItems(5)=ProjYr
              setitem         Proj2Edit2Year,0,ProjYr
              getprop ListIt2,*SubItems(6)=ProjKey
              setitem         Proj2Edit2Key,0,ProjKey
              getprop ListIt2,*SubItems(7)=str14
              setitem         Proj2Edit2LRInc,0,str14
              getprop ListIt2,*SubItems(8)=str14
              setitem         Proj2Edit2NINInc,0,str14
              getprop ListIt2,*SubItems(9)=str10
              setitem         Proj2Edit2Date,0,str10
              getprop ListIt2,*SubItems(10)=ProjNotes
              setitem         Proj2Edit2Notes,0,ProjNotes
              getprop ListIt2,*SubItems(11)=str10
              setitem         Proj2Stat2ModDate2,0,str10
              getprop ListIt2,*SubItems(12)=str35
              setitem         Proj2Stat2Sales2,0,str35
              getprop ListIt2,*SubItems(13)=str1
              if (str1 = YES)
               setitem        Proj2Check2Master,0,1
              else
               setitem        Proj2Check2Master,0,0
              endif
.
              setprop         Proj2Modify,enabled=1
              return

Proj2VerifyData
              getitem         Proj2Combo2Source,0,result
              if (result = 2)                .Brokerage
               move           "B",ProjSrc
              elseif (result = 3)            .List Management
               move           "M",ProjSrc
              else
               alert          caution,"You must select a Record Source!",result
               move           YES,ReturnFlag
               setfocus Proj2Combo2Source
               return
              endif
.
              if (ProjSrc = "B")
               getitem        Proj2Combo2Type,0,result
               if (result = 2)                              .Exchange
                              move           "E",ProjType
               elseif (result = 3)           .Rent
                              move           "R",ProjType
               else
                              alert          caution,"You must select a Type code for Brokerage Records!",result
                              move           YES,ReturnFlag
                              setfocus Proj2Combo2Type
                              return
               endif
              else
.List Management records do not have a Type associated with them
               move           " ",ProjType
               setitem        Proj2Combo2Type,0,1
              endif
.
              getitem         Proj2Edit2Client,0,PrjStr6
              call            Trim using PrjStr6
              if (PrjStr6 = "")
               alert          caution,"You must select a valid Client Number!",result
               move           YES,ReturnFlag
               setfocus Proj2Edit2Client
               return
              endif
              if (ProjSrc = "M")
               call           ZFillIt using PrjStr6,C0
               move           C1,NDATPATH
               pack           NDATFLD,PrjStr6
               move           "Proj2Verify-NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               call           NDATKEY
               if over
                              alert          caution,"You must select a valid List Number!",result
                              move           YES,ReturnFlag
                              setfocus Proj2Edit2Client
                              return
               else
                              move           OLSTNAME,str45
               endif
              else
.START PATCH 1.67 REPLACED LOGIC
.              count          howmany,PrjStr6
.              if (howmany = 6)
.                             unpack         PrjStr6,str2,str4
.              elseif (howmany = 5)
.                             unpack         PrjStr6,str1,str4
.              else
.                             move           PrjStr6,str4
.                             call           ZFillIt using str4,C0
.              endif
.              move           str4,PrjStr6
.              move           C1,NMLRPATH
.              pack           MKEY,str4,"000"
.              move           "Proj2Verify-NMLRKEY",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              if over
.                             alert          caution,"You must select a valid Mailer Number!",result
.                             move           YES,ReturnFlag
.                             setfocus Proj2Edit2Client
.                             return
.              else
.                             move           MCOMP,str45
.              endif
........................................
               call           ZFillIt using PrjStr6,C0
               pack           COMPFLD,PrjStr6
               move           "Proj2Verify-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               if over
                              alert          caution,"You must select a valid Mailer Number!",result
                              move           YES,ReturnFlag
                              setfocus Proj2Edit2Client
                              return
               elseif (COMPMLRFLG <> "T")
                              alert          caution,"You must select a valid Mailer Number!",result
                              move           YES,ReturnFlag
                              setfocus Proj2Edit2Client
                              return
               else
                              move           COMPCOMP,str45
               endif
.END PATCH 1.67 REPLACED LOGIC
              endif
              setitem         Proj2Edit2Client,0,PrjStr6
              setitem         Proj2Stat2ClientName,0,str45
              call            ZFillIt using PrjStr6,C0
              move            PrjStr6,ProjClient
.
              getitem         Proj2Edit2Year,0,ProjYr
              call            Trim using ProjYr
              if (ProjYr = "")
               alert          caution,"You must supply a valid Year!",result
               move           YES,ReturnFlag
               setfocus Proj2Edit2Year
               return
              else
               count          howmany,ProjYr
               if (howmany <> 4)
                              alert          caution,"You must supply a valid 4 digit Year!",result
                              move           YES,ReturnFlag
                              setfocus Proj2Edit2Year
                              return
               endif
              endif
.
              if (NewFlag = YES)
               move           C1,NPRJPATH
               move           "Proj2Verify-NPRJTST",Location
               move           C0,N2
               loop
                              add            C1,N2
                              move           N2,ProjKey
                              rep            zfill,ProjKey
                              pack           NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJTST
                              until over
                              if (N2 >= 98)
                                             pack           taskname,"You already have the maximum number of records",newline,"for this Client for this Year!",newline,"Please select a different Client or Year to continue."
                                             alert          caution,taskname,result
                                             move           YES,ReturnFlag
                                             setfocus Proj2Edit2Client
                                             return
                              endif
               repeat
               setitem        Proj2Edit2Key,0,ProjKey
              endif
.
              getitem         Proj2Edit2Date,0,str10
              call            RemoveChar using str10,SLASH
              call            Trim using str10
              count           howmany,str10
              if (howmany <> 8)
              else
               unpack         str10,MM,DD,str4
               pack           ProjDate,str4,MM,DD
              endif
.
              if (NewFlag = YES)
               clear          ProjMod
              else
               unpack         timestamp,ProjMod
              endif
.
              call            Proj2LostFocusAmount using Proj2Edit2LRInc
.Much of this code is repetitious but I need a valid value for str11
              getitem         Proj2Edit2LRInc,0,str14
              move            C0,N11
              call            Trim using str14
              if (str14 <> "")
               call           RemoveChar using str14,COMMA
               move           str14,str11
              endif
              move            str11,N11
              move            N11,ProjLR
.
              call            Proj2LostFocusAmount using Proj2Edit2NINInc
.Much of this code is repetitious but I need a valid value for str11
              getitem         Proj2Edit2NINInc,0,str14
              move            C0,N11
              call            Trim using str14
              if (str14 <> "")
               call           RemoveChar using str14,COMMA
               move           str14,str11
              endif
              move            str11,N11
              move            N11,ProjNIN
.DH wanted to allow entries with no values
.             if (ProjLR = C0 AND ProjNIN = C0)
.              alert          caution,"You must supply either LR Income or NIN Income!",result
.              move           YES,ReturnFlag
.              setfocus Proj2Edit2LRInc
.              return
.             endif
.
              getitem         Proj2Edit2Notes,0,ProjNotes
              call            Trim using ProjNotes
.
              getitem         Proj2Check2Master,0,N1
              if (N1 = 1)
               move           YES,ProjMast
              else
               clear          ProjMast
              endif
              return
..................Some GUI stuff.................
Proj2CreateListViewColumns
        getprop Proj2ListView,*ColumnHeaders=ColHeads
              ColHeads.Clear
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
              ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
              ColHeads.Add using *Index=2,*Key="one",*Text="Type",*Width=50
              ColHeads.Add using *Index=3,*Key="two",*Text="Source",*Width=50
              ColHeads.Add using *Index=4,*Key="three",*Text="Client",*Width=60
              ColHeads.Add using *Index=5,*Key="four",*Text="Client Name",*Width=70
              move            C5,howmany
              move            "1991",N4
              move            "LoadLV-NPRJAIM",Location
              clear           NPRJFLD1
              clear           NPRJFLD2
              clear           NPRJFLD3
              loop
               add            C1,N4
               move           N4,str4
               call           Trim using str4
               pack           NPRJFLD4,"04X",str4
               clear          NPRJFLD5
               pack           KeyLocation,"Key: ",NPRJFLD4
               call           NPRJAIM
               until over
               move           C0,N2
               loop
                              add            C1,N2
                              move           N2,str2
                              rep            zfill,str2
                              pack           NPRJFLD5,"05X",str2
                              pack           KeyLocation,"Key: ",NPRJFLD4,NPRJFLD5
                              call           NPRJAIM
                              until over
.                             pack           PrjStr6,str4,str2
.                             pack           str7,str4,str2,"B"
                              pack           str11,str4," NIN ",str2
                              pack           str12,str4," LR  ",str2
                              add            C1,howmany
                              ColHeads.Add using *Index=howmany,*Text=str11,*Width=75,*Alignment=1
                              add            C1,howmany
                              ColHeads.Add using *Index=howmany,*Text=str12,*Width=75,*Alignment=1
               repeat
               move           str4,LastYear
              repeat
.Year columns were adding in Ascending Value, Rearrange them into Descending Value
              add             C1,howmany,IntIndex
              move            C5,result
              loop
               sub            C1,IntIndex
               add            C1,result
               until (result = howmany)
               setprop        ColHeads.Item(IntIndex),*Position=result
              repeat
.
              setprop         Proj2ListView,*HideColumnHeaders=OFALSE
              setprop         Proj2ListView,*HideSelection=OFALSE
.             setprop         Proj2ListView,*HotTracking=OTRUE
              setprop         Proj2ListView,*FullRowSelect=OTRUE
.             setprop         Proj2ListView,*MultiSelect=OTRUE
              setprop         Proj2ListView,*Sorted=OTRUE
              setprop         Proj2ListView,*SortOrder=0
              setprop         Proj2ListView,*AllowColumnReorder=OTRUE
              setprop         Proj2ListView,*LabelEdit=1
              setprop         Proj2ListView,*View=3
.             setprop         Proj2ListView,*Font=font2

              return

Proj2LostFocusAmount Routine EditPtr
              getitem         EditPtr,0,str14
              call            Trim using str14
              if (str14 <> "")
               call           RemoveChar using str14,COMMA
               move           str14,str11
               call           FormatNumeric using str11,str14
              endif
              setitem         EditPtr,0,str14
              return

Proj2RefreshClient
              clear           str45
              getitem         Proj2Edit2Client,0,PrjStr6
              call            Trim using PrjStr6
              if (PrjStr6 <> "")
               getitem        Proj2Combo2Source,0,result
               if (result > C1)
                              if (result = 2)               .Brokerage
.START PATCH 1.67 REPLACED LOGIC
.                                            count          howmany,PrjStr6
.                                            if (howmany = 6)
.                                                           unpack         PrjStr6,str2,str4
.                                            elseif (howmany = 5)
.                                                           unpack         PrjStr6,str1,str4
.                                            else
.                                                           move           PrjStr6,str4
.                                                           call           ZFillIt using str4,C0
.                                            endif
.                                            move           str4,PrjStr6
.                                            move           C1,NMLRPATH
.                                            pack           MKEY,str4,"000"
.                                            move           "Proj2Refresh-NMLRKEY",Location
.                                            pack           KeyLocation,"Key: ",MKEY
.                                            call           NMLRKEY
.                                            if not over
.                                                           move           MCOMP,str45
.                                            endif
...........................................
                                             call           ZFillIt using PrjStr6,C0
                                             pack           COMPFLD,PrjStr6
                                             move           "Proj2Refresh-COMPKEY",Location
                                             pack           KeyLocation,"Key: ",COMPFLD
                                             call           COMPKEY
                                             if not over
                                                            if (COMPMLRFLG = "T")
                                                                           move           COMPCOMP,str45
                                                            endif
                                             endif
.END PATCH 1.67 REPLACED LOGIC
                              else                                         .List Management
                                             call           ZFillIt using PrjStr6,C0
                                             move           C1,NDATPATH
                                             pack           NDATFLD,PrjStr6
                                             move           "Proj2Refresh-NDATKEY",Location
                                             pack           KeyLocation,"Key: ",NDATFLD
                                             call           NDATKEY
                                             if not over
                                                            move           OLSTNAME,str45
                                             endif
                              endif
               endif
              endif
              setitem         Proj2Edit2Client,0,PrjStr6
              setitem         Proj2Stat2ClientName,0,str45
              return

Proj2GetSales Routine DimPtr,DimPtr1
              clear           DimPtr1
        move    C2,N2           .Must start with first legitimate entry!!
        clear   str2
        loop
                getitem Proj2ComboSales,N2,str45
                unpack  str45,str35,str1,str2
                if (str2 = "" | str2 = " " | str2 = "  ")
                        clear str35
                        break
                endif
                until (str2 = DimPtr)
                add     C1,N2
.Extra protection
                if (N2 >= 98)
                        clear str35
                        break
                endif
        repeat
              move            str35,DimPtr1
        return

................................................
............ Logic for Screen 3 ................
................................................
ProjLoadRecords
.Work out details around file later.  These must be close and reopened I think
              close           DUPEOWN
              close           OUTPUT2
              close           tempfile
.????? DH 10/13/2006    goto drew
.
              open            DUPEOWN,"DUPEOWN",READ
.begin patch 1.3
.             prepare         OUTPUT2,"c:\work\INCOME7","c:\work\INCOME7","15","190",EXCLUSIVE
               prepare        OUTPUT2,"c:\work\INCOME7","c:\work\INCOME7","15","210",EXCLUSIVE
.end patch 1.3

     move    NO,StopFlag2
.....Invoice File Read.....
.
     call    ProjInitProgressBar
     move    "NININV",str25
     call    ProjTestForFile using str25,N1,C1
.
     if (N1 = C0)
             clear   taskname
             append  NTWKPATH1,taskname
.             append  "TEXT\NININV.DAT,C:\WORK\NININV.SRT;112-119,S=#"112>='",taskname
             append  "TEXT\NININV.DAT,C:\WORK\NININV.SRT;130-137,S=#"130>='",taskname
             append  FromDate,taskname
.             append  "'&112<='",taskname
             append  "'&130<='",taskname
             append  ToDate,taskname
             append  "'#"",taskname
             reset   taskname
             setitem Proj3StatProgress,0,"Sorting NININV.DAT"
             sort    taskname
     endif
.
     setitem Proj3StatProgress,0,"Reading c:\work\NININV.SRT"
     move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\NININV.SRT",hexzero
              call    FindFirstFile
              if (APIResult = 0 | APIResult = hexeight)
               alert          caution,"NININV.SRT Sort Failed - Job Cancelled!",result
               return
              endif
              move            "c:\work\NININV.SRT",str45
              open            tempfile,str45,read                        ;test using exclusive 06sep05
              positeof tempfile
              fposit          tempfile,N10
.             calc            howmany=(N10/402)             .402 = 400(NININV record length) + 2 bytes for CR/LF
              calc            howmany=(N10/302)             .302 = 300(NININV record length) + 2 bytes for CR/LF
              reposit tempfile,C0                           .Reposition File Pointer to beginning of file
.
              move            C1,PASS
              loop
               read           tempfile,SEQ;INVVARS
               until over
               pack           str10,INVDTEC,INVDTEY,INVDTEM,INVDTED
.Check is done again in case User is re-using an existing Sort file that may not meet exact specs.
               if (str10 >= FromDate AND str10 <= ToDate)
                              call           ProjReadFiles
               endif
.Update Progress Bar
               call           ProjUpdateProgressBar
              repeat
              close           tempfile
.
.....Adjustment File Read.....
.
              call            ProjInitProgressBar
              move            "NADJUST",str25
              call            ProjTestForFile using str25,N1,C2
.
              if (N1 = C0)
               clear          taskname
               append         NTWKPATH1,taskname
               append         "TEXT\NADJUST.DAT,C:\WORK\NADJUST.SRT;125-132,S=#"125>='",taskname
               append         FromDate,taskname
               append         "'&125<='",taskname
               append         ToDate,taskname
               append         "'#"",taskname
               reset          taskname
               setitem        Proj3StatProgress,0,"Sorting NADJUST.DAT"
               sort           taskname
              endif
              setitem         Proj3StatProgress,0,"Reading c:\work\NADJUST.SRT"
              move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\NADJUST.SRT",hexzero
              call    FindFirstFile
              if (APIResult = 0 | APIResult = hexeight)
               alert          caution,"NADJUST Sort Failed - Job Cancelled!",result
               return
              endif
.
              move            "c:\work\NADJUST.SRT",str45
              open            tempfile,str45,read
              positeof tempfile
              fposit          tempfile,N10
              calc            howmany=(N10/179)             .179 = 177(NINJST record length) + 2 bytes for CR/LF
              reposit tempfile,C0                           .Reposition File Pointer to beginning of file
.
              move            C2,Pass
              loop
               read           tempfile,SEQ;JSTVARS
               until over
               unpack         JSTDATE,INVDTEC,INVDTEY,INVDTEM,INVDTED
               pack           str10,INVDTEC,INVDTEY,INVDTEM,INVDTED
.Check is done again in case User is re-using an existing Sort file that may not meet exact specs.
               if (str10 >= FromDate AND str10 <= ToDate)
                              call           ProjReadFiles2
               endif
.Update Progress Bar
               call           ProjUpdateProgressBar
              repeat
              close           tempfile
.begin patch 1.3
.....Triplex invoice File Read.....
.AweCrap
.
              call            ProjInitProgressBar
              move            "TDMCINV",str25
              call            ProjTestForFile using str25,N1,C4
.
              if (N1 = C0)
               clear          taskname
               append         NTWKPATH1,taskname
               append         "TEXT\TDMCINV.DAT,C:\WORK\TDMCINV.SRT;1-6,S=#"11>='",taskname
               append         FromDate,taskname
               append         "'&11<='",taskname
               append         ToDate,taskname
               append         "'#"",taskname
               reset          taskname
               setitem        Proj3StatProgress,0,"Sorting TDMCINV.DAT"
               sort           taskname
              endif
              setitem         Proj3StatProgress,0,"Reading c:\work\TDMCINV.SRT"
              move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\TDMCINV.SRT",hexzero
              call    FindFirstFile
              if (APIResult = 0 | APIResult = hexeight)
               alert          caution,"TDMCINV Sort Failed - Job Cancelled!",result
               return
              endif
.
.AweCrap
              setitem         Proj3StatProgress,0,"Reading c:\work\TDMCINV.SRT"
               move           "c:\work\TDMCINV.SRT",str45
              open            tempfile,str45,read
              positeof tempfile
              fposit          tempfile,N10
              calc            howmany=(N10/68)              .68 = 66(TDMCINV record length) + 2 bytes for CR/LF
              reposit tempfile,C0                           .Reposition File Pointer to beginning of file
.
              move            C4,Pass
              loop
               read           tempfile,SEQ;TInvVars
               until over
               unpack         TinvDate,INVDTEC,INVDTEY,INVDTEM,INVDTED
               pack           str10,INVDTEC,INVDTEY,INVDTEM,INVDTED
.Check is done again in case User is re-using an existing Sort file that may not meet exact specs.
               if (str10 >= FromDate AND str10 <= ToDate)
                              call           ProjReadFiles4
               endif
.Update Progress Bar
               call           ProjUpdateProgressBar
              repeat
              close           tempfile
.....Excharge invoice File Read.....
AweCrap
.
               clear          FromDate6
               append         fromDate,Fromdate6
               reset          Fromdate6
               clear          ToDate6
               append         ToDate,Todate6
               reset          Todate6
              call            ProjInitProgressBar
              move            "EXCHARGE",str25
              call            ProjTestForFile using str25,N1,C5
.
              if (N1 = C0)
                             clear           taskname
               append         NTWKPATH1,taskname
               append         "TEXT\EXCHARGE.DAT,C:\WORK\EXCHARGE.SRT;1-6,S=#"61>='",taskname
               append         FromDate6,taskname
               append         "'&61<='",taskname
               append         ToDate6,taskname
               append         "'#"",taskname
               reset          taskname
               setitem        Proj3StatProgress,0,"Sorting Excharge.DAT"
               sort           taskname
              endif
              setitem         Proj3StatProgress,0,"Reading c:\work\excharge.SRT"
              move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\excharge.SRT",hexzero
              call    FindFirstFile
              if (APIResult = 0 | APIResult = hexeight)
               alert          caution,"excharge Sort Failed - Job Cancelled!",result
               return
              endif
.
.AweCrap
              setitem         Proj3StatProgress,0,"Reading c:\work\Excharge.SRT"
               move           "c:\work\excharge.SRT",str45
              open            tempfile,str45,read
              positeof tempfile
              fposit          tempfile,N10
              calc            howmany=(N10/68)              .68 = 66(excharge record length) + 2 bytes for CR/LF
              reposit tempfile,C0                           .Reposition File Pointer to beginning of file
.
              move            C5,Pass
              loop
               read           tempfile,SEQ;NRCHGVARS
               until over
                              move           NRCHGCE,invdtec
                              move           NRCHGYR,invdtey
                              move           NRCHGmo,invdtem
.                              move           "01" to invdted
               pack           str10,INVDTEC,INVDTEY,INVDTEm
.Check is done again in case User is re-using an existing Sort file that may not meet exact specs.
               if (str10 >= FromDate6 AND str10 <= ToDate6)
                              call           ProjReadFiles5
               endif
.Update Progress Bar
               call           ProjUpdateProgressBar
              repeat
              close           tempfile
.end patch 1.3

.UnBilled File Read
              call            ProjInitProgressBar
              move            "UNBILLED",str25
.ToDate modified to end of year
              unpack          ToDate,str2,YY,MM,DD
              pack            ToDate,str2,YY,"1231"
              call            ProjTestForFile using str25,N1,C3
.
              if (N1 = C0)
.2  = OSTAT
.66 = Return Date
.74 = Mail Date
               clear          taskname
               append         NTWKPATH1,taskname
               append         "UNBILLED.DAT,C:\WORK\UNBILLED.SRT;74-81,S=#"66>='",taskname
               append         FromDate,taskname
               append         "'&66<='",taskname
               append         ToDate,taskname
               append         "'&74>='",taskname
               append         FromDate,taskname
               append         "'&74<='",taskname
               append         ToDate,taskname
               append         "'&2<>'B'&2<>'X'&2<>'Q'&2<>'x'#"",taskname
               reset          taskname
               setitem        Proj3StatProgress,0,"Sorting UNBILLED.DAT"
               sort           taskname
              endif
              setitem         Proj3StatProgress,0,"Reading c:\work\UNBILLED.SRT"
              move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\UNBILLED.SRT",hexzero
              call    FindFirstFile
              if (APIResult = 0 | APIResult = hexeight)
               alert          caution,"UNBILLED Sort Failed - Job Cancelled!",result
               return
              endif
.
              move            "c:\work\UNBILLED.SRT",str45
              open            tempfile,str45,read
              positeof tempfile
              fposit          tempfile,N10
              calc            howmany=(N10/500)             .500 = 498(UNBILLED record length) + 2 bytes for CR/LF
              reposit tempfile,C0                           .Reposition File Pointer to beginning of file
.
              move            "12",MM
              move            "31",DD
              move            sysyr,YY
              call            cvtjul
              move            JULDAYS,sysjdate
              move            C3,Pass
              loop
               read           tempfile,SEQ;ORDVARS
               until over
               pack           str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
               pack           str11,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
.Check is done again in case User is re-using an existing Sort file that may not meet exact specs.
               if (str10 >= FromDate AND str10 <= ToDate AND str11 <= ToDate AND OSTAT <> "B")
                              reset          CANCODES
                              scan           OSTAT,CANCODES
                              if not equal
                                             call           ProjReadFiles3
                              endif
               endif
.Update Progress Bar
               call           ProjUpdateProgressBar
              repeat
              setitem         Proj3ProgressBar,0,C0
drew
              setitem         Proj3StatProgress,0,"Creating Spreadsheet"
              setitem         Proj3StatProgress2,0,""
              setitem         Proj3StatProgress3,0,""
              setmode *mcursor=*wait
              call            ProjCreateSpreadsheet
              setmode *mcursor=*arrow
              setitem         Proj3StatProgress,0,""
              setitem         Proj3StatProgress2,0,""
              setitem         Proj3StatProgress3,0,""
              return

ProjReadFiles
              pack            NORDFLD,LRN
              call            ProjReadOrder
              if              (badorder = yes)
              return
              endif
              if    (OLNUM = "025755")
              call  Debug
              endif
.              if (SALESNUM = 6 | SALESNUM = 19)
              if (SALESNUM = 6 | SALESNUM = 19 | SALESNUM = 27 | SALESNUM = 28)
               goto MANAGE
              endif
              match           OLNUM,exfeelst
              if equal
               move           "E",TIPE
               goto MANAGE
              endif
.
              if (SALESNUM = C0)
               reset          RUNCODES
               scan           OLNUM,RUNCODES
               if equal
                              move           "E",TIPE
               else
                              goto MANAGE
               endif
              endif
              move            B,SOURCE
              pack            NBILFLD,Omlrnum,COBN,BILLTN
              if (NBILFLD <> HBILLKEY)
               call           ProjReadBillTo
              endif
              goto CONTIN

MANAGE
              move            M,SOURCE
              call            ProjOwnerPrep
              call            ProjReadOwner
CONTIN
              move            NORDFLD,NMRGFLD
              rep             ZFILL,NMRGFLD
              move            C0,NMRGRQTY
              move            C0,NMRGIQTY
              call            WipeCvars
              move            NO,MRGSW
              move            NO,SHIPSW
              move            C0,NMRGNET
              move            "CONTIN-NMRGKEY",Location
              pack            KeyLocation,"Key :",NMRGFLD
              call            NMRGKEY
              if not over
               move           YES,MRGSW
              endif
              move            NORDFLD,NSHPFLD
              rep             ZFILL,NSHPFLD
              move            "CONTIN-NSHPKEY",Location
              pack            KeyLocation,"Key :",NSHPFLD
              call            NSHPKEY
              if not over
               move           YES,SHIPSW
              endif
.
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
              call            COMPUTE
.
.         GOTO      WRITEREC
.
.
WriteRec
              move            C0,str1
              move            " ",ANS
              pack            KEY15,TIPE,SOURCE,key6,CC,INVDTEY,INVDTEM,ANS
              read            OUTPUT2,KEY15;;
              goto            WRITEIT IF OVER
.begin patch 1.3
.             read            OUTPUT2,KEY15;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
.              ADJARTOT,ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL
              read            OUTPUT2,KEY15;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
               ADJARTOT,ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL,tdmctot
.             branch          PASS,ADDINV,ADDADJ,ADDUNBIL
              branch          PASS,ADDINV,ADDADJ,ADDUNBIL,AddTDMC,addRunChrg
.end patch 1.3
ADDINV
              add             FORMAR,ARTOT
              add             AP,APTOT
              add             LRINC,LRTOT
              add             NININC,NINTOT
              move            QTYBILD,N9
              add             N9,QTYTOT
              goto UPDATE
ADDADJ
              add             FORMAR,ADJARTOT
              add             AP,ADJAPTOT
              add             LRINC,ADJLRTOT
              add             NININC,NINTOT
              goto UPDATE
addunbil
              add             UNBILINC,UNBILL
.begin patch 1.3
               Goto           Update
AddTdmc
               Add            Form122 to TDMCtot
               Goto           Update
AddRunChrg
              add             NININC,NINTOT
               Goto           Update
.;end patch 1.3
UPDATE
.;begin patch 1.3
.;             update          OUTPUT2;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT,ADJARTOT:
.;              ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL
              update          OUTPUT2;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT,ADJARTOT:
               ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL,TdmcTOT
.;end patch 1.3
              call            WIPER
              return
.             branch          PASS,INPUT,GETADJ,GETREC
WRITEIT
              call            Wiper
              move            B1,ANS
.;begin patch 1.3
.;             branch          PASS,ADDINV1,ADDADJ1,ADUNBIL1
              branch          PASS,ADDINV1,ADDADJ1,ADUNBIL1,AddTdmc1,AddRunChrg1
.;end patch 1.3
ADDINV1
              move            FORMAR,ARTOT
              move            AP,APTOT
              move            LRINC,LRTOT
              move            NININC,NINTOT
              move            QTYBILD,N9
              move            N9,QTYTOT
              goto WRITEIT1
ADDADJ1
              move            FORMAR,ADJARTOT
              move            AP,ADJAPTOT
              move            LRINC,ADJLRTOT
              goto WRITEIT1
ADUNBIL1
              move            UNBILINC,UNBILL
.;begin patch 1.3
               Goto           WriteIt1
AddTDMC1
.;             add             NININC,NINTOT
               Move            Form122 to TDMCtot
               Goto           WriteIt1
AddRunChrg1
              add             NININC,NINTOT
               Goto           Writeit1
.;end patch 1.3
WRITEIT1
.;begin patch 1.3
.;               write         OUTPUT2,KEY15;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
.;              ADJARTOT,ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL
               write          OUTPUT2,KEY15;KEY15,INCNAME,OWNER,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
               ADJARTOT,ADJAPTOT,ADJLRTOT,ADJNINTOT,UNBILL,TdmcTot
.end patch 1.3
              call            Wiper
              return

WIPER
              move            C0,ARTOT
              move            C0,APTOT
              move            C0,LRTOT
              move            C0,adjninTOT
              move            C0,NINTOT
              move            C0,QTYTOT
              move            C0,ADJARTOT
              move            C0,ADJAPTOT
              move            C0,ADJLRTOT
              move            C0,unbilinc
              move            C0,unbill
.begin patch 1.3
               move           c0 to TDmctot
.end patch 1.3
              return

ProjReadFiles2
              move            JSTLR,NORDFLD
              call            ProjReadOrder
              if              (badorder = yes)
              return
              endif
              move            JSTAR,ADJAR
              move            JSTLRINC,ADJLR
              move            JSTNININC,adjNIN
              move            C0,ADJAP
              move            JSTAP1,ADJAP
              add             JSTAP2,ADJAP
.              if (SALESNUM = 6 | SALESNUM = 19)
              if (SALESNUM = 6 | SALESNUM = 19 | SALESNUM = 27 | SALESNUM = 28)
               goto MANAGE1
              endif
              if (SALESNUM = C0)
               reset          RUNCODES
               scan           OLNUM,RUNCODES
               if equal
                              move           "E",TIPE
               else
                              goto MANAGE1
               endif
              endif
              move            B,SOURCE
              pack            NBILFLD,JSTMLR,JSTCNT,JSTBILTO
              call            ProjReadBillTo
              goto PROCESS2
MANAGE1
              move            M,SOURCE
              call            ProjOwnerPrep
              call            ProjReadOwner
PROCESS2
              move            C0,FORMAR
              move            C0,AP
              move            C0,LRINC
              move            ADJAR,FORMAR
              move            ADJAP,AP
              move            ADJLR,LRINC
              move            ADJNIN,NININC
              goto WriteRec

ProjReadFiles3
              reset           EXCODES
              scan            OELCODE,EXCODES
              if not equal
               move           "R",TIPE
              else
               move           "E",TIPE
              endif
              clear           SALENUM
              pack            SALENUM,OSALES10,OSALES
              rep             zfill,SALENUM
              move            C0,SALESNUM
              move            SALENUM,SALESNUM
              move            OLON,NOWNFLD
              rep             ZFILL,NOWNFLD
              move            C1,NOWNPATH
              move            "ProjRead3-NOWNKEY",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNKEY
              pack            str2,OSALES10,OSALES
              rep             ZFILL,str2
. 
              move            "N",LSTMSW
.DH  --- WHY USE WHAT IS ON THE ORDER!!!!!!!!!!!!!!!!!
.17 Jul 2009
.              move            C1,NBRKPATH
.              pack            NBRKFLD,OBRKNUM,OBRKCNT
.              rep             ZFILL,NBRKFLD
.              clear           BRSALES
.              move            "ProjRead3-NBRKKEY",Location
.              pack            KeyLocation,"Key: ",NBRKFLD
.              call            NBRKKEY
.              move            C0,N2
.              move            BRSALES,N2
                    Move      str2,N2
.DH  --- WHY USE WHAT IS ON THE ORDER!!!!!!!!!!!!!!!!!
.              if (N2 = C6)
          If        (N2 = "6" | N2 = "19" | N2 = "27" | N2 = "28")

               move           YES,LSTMSW
.START PATCH 1.67 REPLACED LOGIC
.              clear          MCODE         .CLEAR VAR.
               clear          COMPBILLCDE         .CLEAR VAR.
.END PATCH 1.67 REPLACED LOGIC
              else
               if (str2 = "00")
                              reset          RUNCODES
                              scan           OLNUM,RUNCODES
                              if not equal
                                             move           YES,LSTMSW
                              endif
               endif
              endif
              move            NO,OVER
.
.             move            OLON,NOWNFLD
.             call            NOWNKEY
              call            MLRREAD
              if (LSTMSW = YES)
.START PATCH 1.67 REPLACED LOGIC
.              clear          MCODE
               clear          COMPBILLCDE
.END PATCH 1.67 REPLACED LOGIC
               move           ORTNDTEM,MM                           .if batch
               move           ORTNDTED,DD                           .dlh/jd 01jun94
               move           ORTNDTEY,YY
               move           ORTNDTEC,CC
               if (ORTNDTEM = "00")
                              call           USEMD
               endif
               call           CVTJUL
              else
.START PATCH 1.67 REPLACED LOGIC
.              if (MCODE = "B" | MCODE = "A")
               if (COMPBILLCDE = "B" | COMPBILLCDE = "A")
.END PATCH 1.67 REPLACED LOGIC
.                             move           ORTNDTEM,MM
.                             move           ORTNDTED,DD
.                             move           ORTNDTEY,YY
.                             move           ORTNDTEC,CC
.                             if (ORTNDTEM = "00")
                                             call           USEMD
.                             endif
               else
                              call           USEMD
               endif
               call           CVTJUL
              endif
              if (JULDAYS < SYSJDATE)
               call           DETAIL
              endif
              return
.I am keeping original logic here so I can compare in the future in case I made a mistake
.        move    SYSJDATE,CHKJUL
.             sub             JULDAYS,CHKJUL
.             compare         CHKJUL,C0               .to be billed this year?
.             goto GETREC if not less                       .No
.             goto            CONTIN1                                      .yes ,PROCESS
..
.CONTIN1
.             call            DETAIL
.             goto getrec
.             return
USEMD
              clear           MM
              clear           DD
              clear           YY
        move    OMDTEM,MM
        move    OMDTED,DD
        move    OMDTEY,YY
              move            OMDTEC,CC
              return

MLRREAD
.START PATCH 1.67 REPLACED LOGIC
.             move            C1,NMLRPATH
.             pack            MKEY,OMLRNUM,OCOBN
.             rep             ZFILL,MKEY
.             call            NMLRKEY
.             cmatch          "B",MCODE      .BATCH BILL ?
.             if not equal                   .NO
.              cmatch         "A",MCODE
.              if not equal   .NO
.                             clear          MCODE         .CLEAR VAR.
.              endif
.             endif
..................................
              pack            COMPFLD3,OMLRNUM
              rep             ZFILL,COMPFLD3
              move            "mlrread-COMPKEY3",Location
              pack            KeyLocation,"Key: ",COMPFLD3
              call            COMPKEY3
               if over
                              alert          caution,"MLR READ _ OVER!",result
                              else
                              cmatch          "B",COMPBILLCDE               .BATCH BILL ?
                              if not equal                   .NO
                                              cmatch         "A",COMPBILLCDE
                                              if not equal   .NO
                                              clear          COMPBILLCDE         .CLEAR VAR.
                                              endif
                              endif
              endif
.END PATCH 1.67 REPLACED LOGIC
              return

DETAIL
              move            C0,CMPT92
              move            C0,UNBILAMT
              move            C0,UNBILINC
              move            C0,FORM52
              move            C0,AR
              move            C0,commper
              clear           specl               *09Apr98 DLH clear for default
              call            MLRREAD
              call            READSHIP            *ORDER SHIPPED????
              call            READMRG
              call            GETCARD
              call            READRTN             *different from nord0013
              sub             CMPT92,CMPT92
              sub             FORM52,FORM52
              move            OQTY,CMPT92
              div             THOUS,CMPT92
              call            SPECIAL                             ,check to see if special pricing
              if (specl <> YES)
.START PATCH 1.5 REPLACED LOGIC
.              move           OPPM,FORM52
.              div            HUND,FORM52
               move           NSEL2PRICE,FORM52
.END PATCH 1.5 REPLACED LOGIC
              endif
              mult            FORM52,CMPT92
              move            CMPT92,UNBILINC
              reset           EXCODES
              scan            OELCODE,EXCODES             EXCHANGE ?
              goto OKEX IF EQUAL
              if (onetfm="M" | onetfm="F")      .discounted order???
               move           onetper,form32              .yes calc it
               mult           ".01",form32
               move           OQTY,CMPT92
               div            THOUS,CMPT92
               mult           form32,CMPT92               .percentage of billable names
               move           CMPT92,net92                 .save it
               mult           FORM52,CMPT92
               move           CMPT92,UNBILINC               .base rental
              endif
              goto RENT
OKEX
              move            C0,CMPT92
              move            OEXQTY,CMPT92
              compare         C0,CMPT92                     .PURE EXCHANGE ?
              if equal                                      .YES
               move           C0,QTYCHK
               move           NO,SPLITSW
               move           OQTY,QTYCHK
               move           QTYCHK,CMPT92
               cmatch         YES,LSTMSW
               if equal
                              move           C0,UNBILINC
                              goto OK
               else
                              goto GETPRICE
               endif
              else
               move           YES,SPLITSW
               cmatch         YES,LSTMSW
               if equal
                              move           C0,UNBILAMT
                              goto RENTPART
               else
                              move           C0,QTYCHK
                              move           OEXQTY,QTYCHK
                              move           QTYCHK,CMPT92
                              goto GETPRICE
               endif
              endif
GETPRICE
              match           "0006",omlrnum
              if equal
               move           C7,form52
               goto calce
              endif
              cmatch          yes,specl
              if equal
               goto calce
              endif
              clear           MM
              clear           DD
              clear           YY
              unpack          JUNEDAT,MM,DD,YY
              call            CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
              move            JULDAYS,JUNDATE    *SAVE RESULT
              clear           MM                *09apr98
              clear           DD                *09apr98
              clear           YY                *09apr98
              move            OODTEM,MM
              move            OODTED,DD
              move            OODTEY,YY
              call            CVTJUL           *CONVERT TODAY'S    DATE TO JULIAN
              move            JULDAYS,MDATE    *SAVE RESULT
              compare         JUNDATE,MDATE
              if not greater
               move           C8,FORM52
               goto CALCE
              endif
              compare         QTYCHK,TENDOLL
              if not less
               move           C10,FORM52
               goto CALCE
              endif
              compare         QTYCHK,NINEDOLL
              if not less
               move           C9,FORM52
               goto CALCE
              endif
              compare         SEVDOLL,QTYCHK
              if not less
               move           C7,FORM52
               goto CALCE
              endif
              move            C8,FORM52
CALCE
              divide          THOUS,CMPT92
              mult            FORM52,CMPT92
              move            CMPT92,UNBILAMT
              cmatch          YES,SPLITSW
              if equal
               goto RENTPART
              else
               move           CMPT92,UNBILINC
               goto OK
              endif
RENTPART
              move            C0,CMPT92          SPLIT RENT/EXCHANGE
              move            C0,N9
              move            OQTY,CMPT92
              move            OEXQTY,N9
              sub             N9,CMPT92           GET RENTAL PORTION
              mult            ".001",CMPT92
.end patch #2.4 - replaced var
.oops should not be here         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
              cmatch          yes,specl
              if equal
               goto calcsp2
              endif

              compare         c0,card$
              if equal
               move           "65.00",FORM52          *ESTIMATED $.   (USE DATACARD?)
              else
               move           card$,form52
              endif
              mult            FORM52,CMPT92
              match           "0192",obrknum
              if equal
.START PATCH 1.68 REMOVED LOGIC
.              match          " 20",commper
.              if equal
               if (COMMPER = "20")
.END PATCH 1.68 REMOVED LOGIC
                              mult           ".1",CMPT92
                              goto addamt
               endif
.START PATCH 1.68 REMOVED LOGIC
.              match          " 30",commper
.              if equal
               if (COMMPER = "30")
.END PATCH 1.68 REMOVED LOGIC
                              mult           ".2",CMPT92
                              goto addamt
               endif
.oddball use 10% commission
               mult           ".1",CMPT92
               goto addamt
              endif
.
              cmatch          YES,LSTMSW
              if equal
               mult           ".1",CMPT92
              else
.START PATCH 1.68 REMOVED LOGIC
.              move           c0,n32      .DLH USE Datacard info 09Jul98
.              move           commper,n32
.              mult           ".01",n32
.              mult           n32,CMPT92
               move           c0,n34      .DLH USE Datacard info 09Jul98
               move           commper,n34
               mult           ".01",n34
               mult           n34,CMPT92
.END PATCH 1.68 REMOVED LOGIC
.         MULT      ".2" BY CMPT92            ESTIMATED LR INCOME.
              endif
addamt2
              add             CMPT92,UNBILAMT
              move            UNBILAMT,UNBILINC
              goto OK
calcsp2
              mult            FORM52,CMPT92
              add             CMPT92,UNBILAMT
              move            UNBILAMT,UNBILINC
              goto OK
.
RENT
              cmatch          yes,specl
              if equal
               goto ok
              endif

              cmatch          YES,LSTMSW             LIST MANAGEMENT?
              if equal
               mult           ".1",UNBILINC            YES
              else
               match          "0192",obrknum
               if equal
                              setitem        Proj3StatProgress3,0,"EPSILON!!!!!!!!!"
.START PATCH 1.68 REMOVED LOGIC
.                             match          " 20",commper
.                             if equal
                              if (COMMPER = "20")
.END PATCH 1.68 REMOVED LOGIC
                                             mult           ".1",UNbilinc
                                             goto ok
                              endif
.START PATCH 1.68 REMOVED LOGIC
.                             match          " 30",commper
.                             if equal
                              if (COMMPER = "30")
.END PATCH 1.68 REMOVED LOGIC
                                             mult           ".2",UNbilinc
.START PATCH 1.68 REMOVED LOGIC
.                                            pack           str45,"Unbilinc ",Unbilinc,b1,commper
                                             move           COMMPER,PrjStr6
                                             pack           str45,"Unbilinc ",Unbilinc,b1,PrjStr6
.END PATCH 1.68 REMOVED LOGIC
                                             setitem        Proj3StatProgress3,0,str45
                                             goto ok
                              endif
.oddball use 10% commission
                              mult           ".1",Unbilinc
                              goto ok
               endif
.START PATCH 1.68 REMOVED LOGIC
.              move           c0,n32      .DLH USE Datacard info 09Jul98
.              move           commper,n32
.              mult           ".01",n32
.              mult           n32,unbilinc
               move           c0,n34      .DLH USE Datacard info 09Jul98
               move           commper,n34
               mult           ".01",n34
               mult           n34,unbilinc
.END PATCH 1.68 REMOVED LOGIC
              endif
.
OK
              noreturn
              compare         SIX,SALESNUM
              goto MANAGE2 IF EQUAL
              compare         "19",SALESNUM
              goto MANAGE2 IF EQUAL
              compare         C0,SALESNUM
              if equal
               reset          RUNCODES
               scan           OLNUM,RUNCODES
               if equal
               move           "E",TIPE
               else
                              goto MANAGE2
               endif
              endif
              move            B,SOURCE
              pack            NBILFLD,omlrnum,z3,c0
              call            ProjReadBillTo
              move            sysmo,invdtem
              move            sysyr,invdtey
              goto WriteRec
MANAGE2
              move            M,SOURCE
              call            ProjOwnerPrep
              call            ProjReadOwner
              move            sysmo,invdtem
              move            sysyr,invdtey
              goto WriteRec

GETCARD
              move            OLNUM,NDATFLD
              move            C1,NDATPATH
              move            "GETCARD-NDATKEY",Location
              pack            KeyLocation,"Key: ",NDATPATH
              call            NDATKEY
              return if over
.START PATCH 1.5 REPLACED LOGIC
.             parse           textdata into TEXT1 using " ~09",noskip,blankfill
.             scan            "EXCHANGE ONLY",TEXT
.             return if equal                 NO USABLE $ RETURN
.             reset           TEXT1
.             scan            "$",TEXT1
.             return if not equal        NO USABLE $ RETURN
.             bump            TEXT1,1
.             pack            str2,TEXT1
.             move            C0,card$
.             move            str2,card$
.             scan            "$",TEXT1        *DO WE HAVE CORRECT PRICE?
.             return if not equal            *YES.
.             clear           str2
.             bump            TEXT1,1
.             pack            str2,TEXT1       *NO, NOW WE DO!
.             move            C0,card$
.             move            str2,card$
              if (NDATCONV = "1")
.              if (NDATEXCH <> "1")
                              clear     NSELFLD1
                              clear     NSELFLD2
                              clear     NSELFLD3

                              pack           NSELFLD1,"01X",LSTNUM
                              pack           NSELFLD2,"02XBASE"
                              move           "NSELAIM",Location
                              pack           KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call           NSELAIM
                              if not over
                                             if (NSELEXC <> "2")
                                                            move           C0,card$
                                                            move           NSELPRICE,card$
                                             endif
                              else
                                             goto DataCheckText
                              endif
.              endif
              else
DataCheckText
               pack           NTXTFLD,LSTNUM,"1"
               move           "NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               if not over
                              move           NTXTTEXT,text1
                              scan           "EXCHANGE ONLY",TEXT1
                              return if equal                 NO USABLE $ RETURN
                              reset          TEXT1
                              scan           "$",TEXT1
                              return if not equal        NO USABLE $ RETURN
                              bump           TEXT1,1
                              pack           str2,TEXT1
                              move           C0,card$
                              move           str2,card$
                              scan           "$",TEXT1        *DO WE HAVE CORRECT PRICE?
                              return if not equal            *YES.
                              clear          str2
                              bump           TEXT1,1
                              pack           str2,TEXT1       *NO, NOW WE DO!
                              move           C0,card$
                              move           str2,card$
               else
                              clear          text1
               endif
              endif
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               unpack         OPPM,str3,str2
               pack           PrjStr6,str3,".",str2
               rep            zfill,PrjStr6
               move           PrjStr6,NSEL2PRICE
              endif

.END PATCH 1.5 REPLACED LOGIC
              return

READSHIP
              move            OLRN,NSHPFLD
              move            "READSHIP-NSHPKEY",Location
              pack            KeyLocation,"Key: ",NSHPFLD
              call            NSHPKEY
              return if over
        move  C0,N9
              move            SQUANT,N9
              if (N9 <> C0)
               move           SQUANT,OQTY
              endif
              return

READRTN
              if (ORTNNUM <> NRTNFLD)
               move           ORTNNUM,NRTNFLD
               move           "READRTN-NRTNKEY",Location
               pack           KeyLocation,"Key: ",NRTNFLD
               call           NRTNKEY
              endif
              return

READMRG
              move            OLRN,NMRGFLD
              rep             zfill,NMRGFLD
              move            C1,NMRGPATH
              move            "READMRG-NMRGKEY",Location
              pack            KeyLocation,"Key: ",NMRGFLD
              call            NMRGKEY
              return if over
              move            NMRGIQTY,OQTY
              return

SPECIAL
              move            NO,specl                   .09Apr98 DLH default answer is no
              if (OMLRNUM = "0020" | OMLRNUM = "0179" | OMLRNUM = "0188" | OMLRNUM = "0171")
               move           C5,FORM52
               move           YES,specl
              endif
              return
.begin patch 1.3
projreadfiles4
              packkey         NORDFLD,TinvLR
               rep            zfill in Nordfld
              call            ProjReadOrder
              if              (badorder = yes)
              return
              endif
.              if (SALESNUM = 6 | SALESNUM = 19)
              if (SALESNUM = 6 | SALESNUM = 19 | SALESNUM = 27 | SALESNUM = 28)
              goto          MANAGE4
              endif
              match           OLNUM,exfeelst
              if equal
               move           "E",TIPE
               goto MANAGE4
              endif
.
              if (SALESNUM = C0)
               reset          RUNCODES
               scan           OLNUM,RUNCODES
               if equal
                              move           "E",TIPE
               else
                              goto MANAGE4
               endif
              endif

              move            B,SOURCE
              reset           EXCODES
              scan            OELCODE,EXCODES
              if equal
               move           "E",TIPE
              else
               move           "R",TIPE
              endif
              pack            NBILFLD,omlrnum,COBN,BILLTN
              if (NBILFLD <> HBILLKEY)
               call           ProjReadBillTo
              endif

              goto CONTIN4

MANAGE4
              move            M,SOURCE
              call            ProjOwnerPrep
              call            ProjReadOwner
CONTIN4
               move           tinvdolr to form122
               mult           ".01" by form122

               goto           writerec
.end patch 1.3

.begin patch 1.3
projreadfiles5
              packkey         NORDFLD,NRCHGLR
               rep            zfill in Nordfld
              call            ProjReadOrder
              if              (badorder = yes)
              return
              endif
.              if (SALESNUM = 6 | SALESNUM = 19)
              if (SALESNUM = 6 | SALESNUM = 19 | SALESNUM = 27 | SALESNUM = 28)
              goto          MANAGE5
              endif
              match           OLNUM,exfeelst
              if equal
               move           "E",TIPE
               goto MANAGE5
              endif
.
              if (SALESNUM = C0)
               reset          RUNCODES
               scan           OLNUM,RUNCODES
               if equal
                              move           "E",TIPE
               else
                              goto MANAGE5
               endif
              endif

              move            B,SOURCE
              reset           EXCODES
              scan            OELCODE,EXCODES
              if equal
               move           "E",TIPE
              else
               move           "R",TIPE
              endif

              pack            NBILFLD,Omlrnum,COBN,BILLTN
              if (NBILFLD <> HBILLKEY)
               call           ProjReadBillTo
              endif

              goto CONTIN5

MANAGE5
              move            M,SOURCE
              call            ProjOwnerPrep
              call            ProjReadOwner
CONTIN5
.               move           c0 to NRCHGar
.               move           c0 to NRCHGap
               move           c0 to nininc
               subtract       NRCHGap from NRCHGar
               move           NRCHGar to nininc
                goto           writerec
.end patch 1.3

ProjReadBillTo
.READBLTO
              move            "ReadBillTo-NBILKEY",Location
              pack            KeyLocation,"Key: ",NBILFLD
              call            NBILKEY
              if not over
.START PATCH 1.67 REPLACED LOGIC
.              move           "00",str2
.              pack           key6,str2,BILMLR
               pack           COMPFLD3,BILMLR
               move           "BillTo-COMPKEY3",Location
               pack           KeyLocation,"Key: ",COMPFLD3
               call           COMPKEY3
                             if over
                                            alert          caution,"bill to comp read _ over",result
                             else         
                             pack           key6,COMPNUM
                             endif
.END PATCH 1.67 REPLACED LOGIC
               return
              endif
.
.think falls thru 12sep2005 dlh
.START PATCH 1.67 REPLACED LOGIC - BIGGER PICTURE: ROUTINE NEVER CALLED!!
ProjReadMlr
..READMLR
..
..START PATCH 1.67 REPLACED LOGIC
..            move            MCOMP,INCNAME
..            move            "00",str2
..            pack            key6,str2,MNUM
              pack            COMPFLD3,Omlrnum
              move            "ReadMlr-COMPKEY3",Location
              pack            KeyLocation,"Key: ",COMPFLD3
              call            COMPKEY3
              if over
                              alert          caution,"ProjReadMLr - over!",result
                              else
              pack            key6,COMPNUM
              move            COMPCOMP,INCNAME
              endif
..END PATCH 1.67 REPLACED LOGIC
              return
.END PATCH 1.67 REPLACED LOGIC - BIGGER PICTURE: ROUTINE NEVER CALLED!!
.
ProjReadOwner
.READOWN
              packkey         NDATFLD,OLNUM
              move            C1,NDATPATH
              rep             zfill,OLNUM
              clear           incname
              move            "ReadOwner-NDATKEY",Location
              pack            KeyLocation,"Key: ",NDATFLD
              call            NDATKEY
              if not over
               move           OLSTNAME,INCNAME
              endif
              move            OLNUM,key6
              move            M,SOURCE
              move            OLON,OWNER
              return

ProjOwnerPrep
.OWNPREP
              rep             ZFILL,NOWNFLD
              read            DUPEOWN,NOWNFLD;NOWNFLD,DUPE1,NEWOLON
              if not over
               move           NEWOLON,NOWNFLD
              else
               move           OLON,NOWNFLD
              endif
              return

ProjReadOrder
              move            C1,NORDPATH
              Move            c3,NORDLOCK
              move            "ProjReadOrder-NORDKEY",Location
              pack            KeyLocation,"Key: ",NORDFLD
              move            no to badorder
              call            NORDKEY
             if              over
                             move            yes to badorder
                             alert          caution,"ProjReadorder - over!",result
                            return
              Endif
.              
              move            OLNUM,NDATFLD
              rep             zfill,NDATFLD
.
              move            OLON,NOWNFLD
              move            C1,NOWNPATH
              move            "ProjReadOrder-NOWNKEY",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNKEY
.
.START PATCH 1.67 REPLACED LOGIC
.             pack            MKEY,OMLRNUM,OCOBN
.             move            C1,NMLRPATH
.             move            "ProjReadOrder-NMLRKEY",Location
.             pack            KeyLocation,"Key: ",MKEY
.        call NMLRKEY
              pack            COMPFLD3,OMLRNUM
              move            "ProjReadOrder-COMPKEY3",Location
              pack            KeyLocation,"Key: ",COMPFLD3
        call  COMPKEY3
             if              over
                             alert          caution,"ProjReadorder - compkey3 over!",result
                            return
              Endif
.END PATCH 1.67 REPLACED LOGIC
.
              reset           EXCODES
              scan            OELCODE,EXCODES
              if equal
               move           "E",TIPE
              else
               move           "R",TIPE
              endif
              clear           SALENUM
              pack            SALENUM,OSALES10,OSALES
              move            C0,SALESNUM
              move            SALENUM,SALESNUM
.START PATCH 1.67 REPLACED LOGIC
.             move            MCOMP,incname
              move            COMPCOMP,incname
.END PATCH 1.67 REPLACED LOGIC
              clear           owner
              return

ProjTestForFile Routine DimPtr,FrmPtr,FrmPtr1
.Set Flag
              move            C0,FrmPtr
.
              move    "                                        ",APIFileName
              clear   APIFileName
              pack    APIFileName,"c:\work\",DimPtr,".SRT",hexzero
              call    FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
               pack           str45,"c:\work\",DimPtr,".SRT"
               open           tempfile,str45,read
               if (FrmPtr1 = 1)
                              read           tempfile,SEQ;INVVARS
                              pack           str10,INVDTEC,INVDTEY,INVDTEM,INVDTED
               elseif (FrmPtr1 = 2)
                              read           tempfile,SEQ;JSTVARS
                              pack           str10,JSTDATE
               elseif (FrmPtr1 = 3)
                              read           tempfile,SEQ;ORDVARS
                              pack           str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
                              pack           str11,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
               endif
               if (str10 >= FromDate)
                              positeof tempfile
                              if (FrmPtr1 = 1)
                                             read           tempfile,SEQ;INVVARS
                                             pack           str10,INVDTEC,INVDTEY,INVDTEM,INVDTED
                              elseif (FrmPtr1 = 2)
                                             read           tempfile,SEQ;JSTVARS
                                             pack           str10,JSTDATE
                              elseif (FrmPtr1 = 3)
                                             read           tempfile,SEQ;ORDVARS
                                             pack           str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                             pack           str11,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
                              endif
                              if (str10 <= ToDate)
                                             if (FrmPtr1 = 3)
                                                            if (str11 <= ToDate)
                                                                           clear          taskname
                                                                           pack           taskname,"There is a copy of C:\WORK\",DimPtr,".DAT that contains records within your Date Range.",newline,"Do you want to use this file and save time sorting?"
                                                                           alert          plain,taskname,result
                                                                           if (result = 1)
                                                                                          move           C1,FrmPtr
                                                                           endif
                                                            endif
                                             else
                                                            clear          taskname
                                                            pack           taskname,"There is a copy of C:\WORK\",DimPtr,".DAT that contains records within your Date Range.",newline,"Do you want to use this file and save time sorting?"
                                                            alert          plain,taskname,result
                                                            if (result = 1)
                                                                           move           C1,FrmPtr
                                                            endif
                                             endif
                              elseif (FrmPtr = 1)
                                             move    "NINVDTE",GNXTFLD
                                      move    "LoadInv.Rec.-GNXTKEY",Location
                                      pack    KeyLocation,"Key: ",GNXTFLD
                                      call    GNXTKEY
                                             call           Trim using GNXTNUM
                                             move           C0,JULDAYS
                                             move           GNXTNUM,JULDAYS
                                             call           CVTGREG
                                             if (YY > "80")
                                                            move           "19",str2
                                             else
                                                            move           "20",str2
                                             endif
                                             pack           str10,str2,YY,MM,DD
                                             if (ToDate >= str10)
                                                            clear          taskname
                                                            pack           taskname,"There is a copy of C:\WORK\",DimPtr,".DAT that contains records within your Date Range.",newline,"Do you want to use this file and save time sorting?"
                                                            alert          plain,taskname,result
                                                            if (result = 1)
                                                                           move           C1,FrmPtr
                                                            endif
                                             endif
                              endif
               endif
               close          tempfile
              endif
              return
..................Windows Housekeeping..................
ProjUpdateProgressBar
              calc            CurRec=(CurRec+1)
              calc            CurVal=((CurRec/howmany)*100)
              if (CurVal <> LastVal)
               setitem        Proj3ProgressBar,0,CurVal
               move           CurVal,LastVal
              endif
              checkevent
              if (StopFlag2 = YES)
               setitem        Proj3StatProgress,0,""
               setitem        Proj3StatProgress2,0,""
               setitem        Proj3StatProgress3,0,""
               setitem        Proj3ProgressBar,0,C0
               noreturn
              endif
              return

ProjInitProgressBar
              move            C0,CurRec
              move            C0,CurVal
              move            C0,LastVal
              return

ProjTabClick
        if (N3 = C1)
                Deactivate Npro0002
        elseif (N3 = C2)
                Deactivate Npro0003
        endif
              return

ProjTabChange
        move    N3,TabNum
        if (N3 = C1)
                Activate Npro0002
        elseif (N3 = C2)
                Activate Npro0003
              endif
              return

ProjCreateSpreadsheet
              close           tempfile
              close           tempfile2
              close           output
              move            C2,NXRFPATH                   .set which key to use
.START PATCH 1.67 REMOVED LOGIC
.             move            C1,NMLRPATH                   .set which key to use
.             move            C3,NMLRLOCK                   .turn off file locking
.END PATCH 1.67 REMOVED LOGIC
              move            C1,NDATPATH                   .set which key to use
              move            C3,NDATLOCK                   .turn off file locking
.income7 currenlyt holds last 12 months income, adjustment, and current unbilled info for
.exclusive clients & lists
              open            tempfile,"c:\work\income7.dat",read
.output the above info will be compiled in the form I want it
.begin patch 1.3
.             prepare         output,"c:\work\average7.dat","c:\work\average7.isi","8","692",exclusive
.begin patch 1.91
.              prepare         output,"c:\work\average7.dat","c:\work\average7.isi","8","1006",exclusive
              prepare         output,"c:\work\average7.dat","c:\work\average7.isi","8","1007",exclusive
.end patch 1.91
.end patch 1.3
input
.begin patch 1.3
.               read          tempfile,SEQ;tipe,source,key6,CC,YY,MM,ans,incname,owner,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
.              ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,unbill
               read           tempfile,SEQ;tipe,source,key6,CC,YY,MM,ans,incname,owner,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
               ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,unbill,TDMCTOT
.end patch 1.3
              goto sortit if over
              if (source = M)
               move           B1,tipe
              endif
              packkey         key8,B1,source,key6
.begin patch 1.3
.             read            output,key8;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
.              janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
.              janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
.              janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
.              unbilled
              read            output,key8;key8:           1-8      8
                              incname:                    9-53    45
                              owner:                     54-57     4
                              jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:   55-798
               janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:   -966
               janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:                           -1134
               janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:    -1470
                              janTDMC,feBTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC:                -1638
                              janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMCx,novTDMCx,decTDMCx:    -1806
.begin patch 1.91
.                              unbilled                                                                                                        -1817
                              unbilled,PcompCde                                                                                                        -1817
.end patch 1.91
.end patch 1.3
              goto write if over

.             add       unbill to unbilled

              move            MM,N2
              if (tipe = B1 OR tipe = "R")
               load           LRcalc from N2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1
               add            lrtot,lrcalc
               add            adjlrtot,lrcalc
               store          LRcalc into N2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1
.
               load           NINcalc from N2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
               add            NINtot,NINcalc
               add            adjNINtot,NINcalc
               store          NINcalc into N2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
.begin patch 1.3
               load           TDMCcalc from N2 of janTDMC,feBTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC
               add            TDMCtot,TDMCcalc
               store          tdmccalc into N2 of  janTDMC,febTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC
.end patch 1.3
              endif
              if (tipe = "E")
               load           LRcalc from N2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1
               add            lrtot,lrcalc
               add            adjlrtot,lrcalc
               store          LRcalc into N2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1
.
               load           NINcalc from N2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
               add            NINtot,NINcalc
               add            adjNINtot,NINcalc
               store          NINcalc into N2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
.begin patch 1.3
               load           TDMCcalc from N2 of janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMC,novTDMCx,decTDMCx
               add            TDMCtot,TDMCcalc
               store          tdmccalc into N2 of janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMC,novTDMCx,decTDMCx
.end patch 1.3
              endif
.????? problem with update on my machine at home ?????? so delete and write
.on my system record was written instead of updated, resulting in multiple records with the same key
              read            output,key8;;
              delete          output,key8
.
              goto write1
write
              move            MM,N2
              move            C0,lrcalc
              add             lrtot,lrcalc
              add             adjlrtot,lrcalc
              move            C0,nincalc
              add             NINtot,NINcalc
              add             adjNINtot,NINcalc
.begin patch 1.3
               move           c0 to TDMCCalc
               add            TDMCtot,TdmcCalc
.end patch 1.3
.
              if (tipe = b1 OR tipe = "R")
               store          LRcalc into N2 of jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1
               store          NINcalc into N2 of janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1
.begin patch 1.3
               store          tdmccalc into N2 of  janTDMC,febTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC
.end patch 1.3
              endif
              if (tipe = "E")
               store          LRcalc into N2 of janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1
               store          NINcalc into N2 of janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1
.begin patch 1.3
               store          tdmccalc into N2 of janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMC,novTDMCx,decTDMCx
.end patch 1.3
              endif
.
              pack            key8,B1,source,key6

write1
.Filter out inappropriate records if necessary.
.Note: I allow the creation of the input file without regard to the following filters in order to allow the same
.input file to be used successively with different filters, thus saving processing time.
              if ((Search3Src = "B" AND source <> "B") | (Search3Src = "M" AND source <> "M"))
               goto input
              endif
              add             unbill,unbilled
              if (source = "B")
               if ((Search3Type = "R" AND tipe <> "R") | (Search3Type = "E" AND tipe <> "E"))
                              goto input
               endif
.START PATCH 1.67 REPLACED LOGIC
.              unpack         key8,str2,str2,str4
.              pack           MKEY,str4,z3
.              move           "write1-NMLRKEY",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              if not over
.                             move           MCOMP,INCNAME
.              endif
.              if (Search3Sls <> "" AND (Search3Sls <> MSLSPER))
.                             goto input
.              endif
...........................................
               unpack         key8,str2,PrjStr6
               pack           COMPFLD,PrjStr6
               move           "write1-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               if             not over
                              if (COMPMLRFLG = "T")
                                             move           COMPCOMP,INCNAME
                              Else
                              alert          caution,"Write - comp read failed",result
                                             
                              endif
.begin patch 1.91
             move   CompExcl,PCompCde   
.end patch 1.91
               endif
.START PATCH 1.67 REPLACED LOGIC
.              if (Search3Sls <> "" AND (Search3Sls <> MSLSPER))
               if (Search3Sls <> "" AND (Search3Sls <> COMPCONTACT))
.END PATCH 1.67 REPLACED LOGIC
                              goto input
               endif
.END PATCH 1.67 REPLACED LOGIC
              endif
              if (source = "M")
               unpack         key8 into str2,PrjStr6
               pack           NDATFLD from PrjStr6
               move           C1,NDATPATH
               move           "write1-NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               call           NDATKEY
               if not over
                              move           OLSTNAME,INCNAME
               endif
.begin patch 1.91
             move   Elstcde,PCompCde    
.end patch 1.91
              endif
.;begin patch 1.3
.;             write           output,key8;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
.;              janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
.;              janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
.;              janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
.;              unbilled
              if              (incname = "")
              alert           Caution,"no NAME!",result
              endif
              write           output,key8;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
               janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
               janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
               janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
                              janTDMC,feBTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC:
                              janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMCx,novTDMCx,decTDMCx:
.begin patch 1.91
.                              unbilled                                                                                                        -1817
                              unbilled,PcompCde                                                                                                        -1817
.end patch 1.91
.;end patch 1.3
              move            C0,unbilled
              goto input
.sortit
sortit
              close           output
              sort            SORTVAR
              if over
               alert          caution,"Sort Error!",result
              endif
.......................................................................................
.pass2
.......................................................................................
.create workbook with eight worksheets and populate with data and formula's
pass2
              open            tempfile2,"c:\work\average7.srt",read
.Open Excel application
              create          ex
              setprop ex,*WindowState=xlMinimized
.START PATCH 1.2 REPLACED LOGIC
..START PATCH 1.1 ADDED LOGIC
.             setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 1.1 ADDED LOGIC
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
              setprop ex.CommandBars("Standard"),*Visible="True"
              setprop ex.CommandBars("Formatting"),*Visible="True"
              setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.2 REPLACED LOGIC
.should try getting the property here and reseting it when done.
              getprop         ex,*SheetsInNewWorkbook=NumberofSheets
.begin patch 1.3
.             setprop         ex,*SheetsInNewWorkbook=C8
              setprop         ex,*SheetsInNewWorkbook=C11
.end patch 1.3
.Create Workbooks collection
              getprop         ex,*Workbooks=books
.Create/Add a single Workbook
              books.add
              books.item giving book using 1
.Create Worksheets collection
              getprop book,*Sheets=sheets
.Create a Worksheet(s) - we did not need to add it as we set the default above to
.begin patch 1.3
..add 8 new Worksheets each time a Workbook is created.
..add 10 new Worksheets each time a Workbook is created.
.end patch 1.3
sheetsetup
              sheets.item giving sheet using 1
              setprop sheet,*Name="Brokerage LR Totals"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
.        add        c1 to n1
.        compare    c9 to n1.
.        repeat     until equal
              sheets.item giving sheet using 2
              setprop sheet,*Name="Brokerage LR Rent"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 3
              setprop sheet,*Name="Brokerage LR Exch"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 4
              setprop sheet,*Name="List Management LR"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 5
              setprop sheet,*Name="Total NIN Income"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 6
              setprop sheet,*Name="Brokerage NIN Rent"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 7
              setprop sheet,*Name="Brokerage NIN Exch"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
              sheets.item giving sheet using 8
              setprop sheet,*Name="List Management NIN"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
.begin patch 1.3
              sheets.item giving sheet using 9
              setprop sheet,*Name="Brokerage Rental - Triplex"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
.
              sheets.item giving sheet using 10
              setprop sheet,*Name="Brokerage Exchange - Triplex"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
.
              sheets.item giving sheet using 11
              setprop sheet,*Name="List Management - Triplex"
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*CenterFooter=" Page &P"
              setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
              setprop sheet.PageSetup,*Draft=OFALSE
              setprop sheet.PageSetup,*PaperSize=xlContinuous
              setprop sheet.PageSetup,*Order=xlContinuous
              setprop sheet.PageSetup,*BlackAndWhite=OFALSE
              setprop sheet.PageSetup,*Zoom=Zoom95
              setprop sheet.PageSetup,*LeftMargin=Inch25
              setprop sheet.PageSetup,*RightMargin=Inch25
              setprop sheet.PageSetup,*TopMargin=Inch100
              setprop sheet.PageSetup,*BottomMargin=Inch100
              setprop sheet.PageSetup,*HeaderMargin=Inch50
              setprop sheet.PageSetup,*FooterMargin=Inch50
.end patch 1.3

              call            hdout
.Reset Default of Worksheets to users original default
booger2
              setprop ex,*SheetsInNewWorkbook=NumberofSheets

looper2
              add             C1,countout
              pack            str55,"Populating spreadsheets record number ",countout
              setitem         Proj3StatProgress2,0,str55
.;begin patch 1.3
.;             read            tempfile2,SEQ;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
.;              janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
.;              janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
.;              janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
.;                              unbilled
              read            tempfile2,seq;key8,incname,owner,jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1:
               janNIN1,febNIN1,marNIN1,aprNIN1,mayNIN1,junNIN1,julNIN1,augNIN1,sepNIN1,octNIN1,novNIN1,decNIN1:
               janx1,febx1,marx1,aprx1,mayx1,junx1,julx1,augx1,sepx1,octx1,novx1,decx1:
               janNINx1,febNINx1,marNINx1,aprNINx1,mayNINx1,junNINx1,julNINx1,augNINx1,sepNINx1,octNINx1,novNINx1,decNINx1:
                              janTDMC,feBTDMC,marTDMC,aprTDMC,mayTDMC,junTDMC,julTDMC,augTDMC,sepTDMC,octTDMC,novTDMC,decTDMC:
                              janTDMCx,feBTDMCx,marTDMCx,aprTDMCx,mayTDMCx,junTDMCx,julTDMCx,augTDMCx,sepTDMCx,octTDMCx,novTDMCx,decTDMCx:
.begin patch 1.91
.                              unbilled                                                                                                        -1817
                              unbilled,PcompCde                                                                                                        -1817
.end patch 1.91
.end patch 1.3
              goto eoj if over
detout
              unpack          key8,str1,source,PrjStr6
.begin patch 1,8              
              MOve            c0,ActLRE     ;hold exchange portion Actual LR  for Previous Year
              move            c0,ActNINE    ;hold exchange portion Actual NIN  for Previous Year
              Move            c0,ActLrR     ;hold rent portion Actual  LR  for Previous Year
              Move            c0,ActNINR    :hold rent portion Actual NIN  for Previous Year
.              pack            Nrevfld from str1,source,prjstr6,"2004"
.              call            Nrevkey

              if              (source = M)
              pack            Nrevfld from str1,source,prjstr6,"2004"
              call            Nrevkey
              Add             JANlr to ActLrR
              add             FEBlr to ActLrR
              add             MARlr to ActLrR      
              add             APRlr to ActLrR      
              add             MAYlr to ActLrR      
              add             JUNlr to ActLrR      
              add             JULlr to ActLrR      
              add             AUGlr to ActLrR      
              add             SEPlr to ActLrR      
              add             OCTlr to ActLrR      
              add             NOVlr to ActLrR      
              add             DEClr to ActLrR      
.
              add             JANnin to ActNINR     
              add             FEBnin to ActNINR     
              add             MARnin to ActNINR     
              add             APRnin to ActNINR     
              add             MAYnin to ActNINR     
              add             JUNnin to ActNINR     
              add             JULnin to ActNINR     
              add             AUGnin to ActNINR     
              add             SEPnin to ActNINR     
              add             OCTnin to ActNINR     
              add             NOVnin to ActNINR     
              add             DECnin to ActNINR
              endif
.end patch 1.8
              move            c0,projlast
              move            c0,ninlast
              move            c0,projlast1e
              move            c0,ninlast1e
              move            c0,projlast1r
              move            c0,ninlast1r
              move            c0,projnew
              move            c0,ninnew
              if (source = "B")
               move           "EB",str2
               move           C0,N2
              loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,PrevYear,str3
                              rep            zfill,NPRJFLD
                              move           "detout-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projnew
                       move   ProjNIN,ninnew
                              until (ProjMast = YES)
               repeat
               move           "RB",str2
               move           C0,N2
               loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,PrevYear,str3
                              rep            zfill,NPRJFLD
                              move           "detout,2-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projlast
                       move   ProjNIN,ninlast
                              until (ProjMast = YES)
               repeat
               move           "EB",str2
               move           C0,N2
               Move           Prevyear to n4
               sub            c1 from n4
               loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,n4,str3
                              rep            zfill,NPRJFLD
                              move           "detout-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projlast1e
                       move   ProjNIN,ninlast1e
                              until (ProjMast = YES)
               repeat
               move           "RB",str2
               move           C0,N2
               loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,n4,str3
                              rep            zfill,NPRJFLD
                              move           "detout,2-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projlast1r
                       move   ProjNIN,ninlast1r
                              until (ProjMast = YES)
               repeat
              endif
              if (source = "M")
               move           " M",str2
               move           C0,N2
               loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,PrevYear,str3
.                             rep            zfill,NPRJFLD
                              move           "detout,3-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projlast
                       move   ProjNIN,ninlast
                              until (ProjMast = YES)
               repeat
               move           " M",str2
               move           C0,N2
               move           Prevyear to n4
               sub            c1 from n4
               loop
                              add            C1,N2
                              move           N2,str3
                              rep            zfill,str3
                              pack           NPRJFLD,str2,PrjStr6,n4,str3
.                             rep            zfill,NPRJFLD
                              move           "detout,3-NPRJKEY",Location
                              pack           KeyLocation,"Key: ",NPRJFLD
                              call           NPRJKEY
                              until over
                       move   ProjLR,projlast1r
                       move   ProjNIN,ninlast1r
                              until (ProjMast = YES)
               repeat
              endif
              if (source = "B")
               clear          ndattdmc
               clear          NXRFLIST
.START PATCH 1.67 REPLACED LOGIC
.              unpack         PrjStr6,str2,str4
.              pack           MKEY,str4,z3
.              rep            zfill,MKEY
.              move           "detout-NMLRKEY",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              if not over
.                             move           MCOMP,INCNAME
.              endif
.              move           str4,NXRFFLD2
.........................
               pack           COMPFLD,PrjStr6
               move           "detout-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               if not over
                              if (COMPMLRFLG = "T")
                                             move           COMPCOMP,INCNAME
                              endif
               endif
                    move      osls0,salesper
                    move      c0,n2
                    move      CompContact,N2
                    load      salesper from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                              osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35

.;begin patch 1.8
              MOve            "E" to str1
               pack            Nrevfld from STR1,source,B2,COMPOLDMLR,n4
               rep            zfill in nrevfld
               call            Nrevkey
              Add             JANlr to ActLrE
              add             FEBlr to ActLrE
              add             MARlr to ActLrE      
              add             APRlr to ActLrE      
              add             MAYlr to ActLrE      
              add             JUNlr to ActLrE      
              add             JULlr to ActLrE      
              add             AUGlr to ActLrE      
              add             SEPlr to ActLrE      
              add             OCTlr to ActLrE      
              add             NOVlr to ActLrE      
              add             DEClr to ActLrE      
.
              add             JANnin to ActNINE     
              add             FEBnin to ActNINE     
              add             MARnin to ActNINE     
              add             APRnin to ActNINE     
              add             MAYnin to ActNINE     
              add             JUNnin to ActNINE     
              add             JULnin to ActNINE     
              add             AUGnin to ActNINE     
              add             SEPnin to ActNINE     
              add             OCTnin to ActNINE     
              add             NOVnin to ActNINE     
              add             DECnin to ActNINE
              MOve            "R" to str1
               pack            Nrevfld from STR1,source,B2,COMPOLDMLR,n4
               rep            zfill in nrevfld
               call            Nrevkey
              Add             JANlr to ActLrR
              add             FEBlr to ActLrR
              add             MARlr to ActLrR      
              add             APRlr to ActLrR      
              add             MAYlr to ActLrR      
              add             JUNlr to ActLrR      
              add             JULlr to ActLrR      
              add             AUGlr to ActLrR      
              add             SEPlr to ActLrR      
              add             OCTlr to ActLrR      
              add             NOVlr to ActLrR      
              add             DEClr to ActLrR      
.
              add             JANnin to ActNINR     
              add             FEBnin to ActNINR     
              add             MARnin to ActNINR     
              add             APRnin to ActNINR     
              add             MAYnin to ActNINR     
              add             JUNnin to ActNINR     
              add             JULnin to ActNINR     
              add             AUGnin to ActNINR     
              add             SEPnin to ActNINR     
              add             OCTnin to ActNINR     
              add             NOVnin to ActNINR     
              add             DECnin to ActNINR
.end patch 1.8
.START PATCH 1.69 REPLACED LOGIC
.              move           COMPOLDMLR,NXRFFLD2
               move           PrjStr6,NXRFFLD2
.END PATCH 1.69 REPLACED LOGIC
.END PATCH 1.67 REPLACED LOGIC
               move           "detout-NXRFKEY",Location
               pack           KeyLocation,"Key: ",NXRFFLD2
               call           NXRFKEY
               move           NXRFLIST,NDAT3FLD
               move           "detout-NDAT3KEY",Location
               pack           KeyLocation,"Key: ",NDAT3FLD
               call           NDAT3KEY
.start with totals sheet for LR income
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - Lr income totals "
               move           C1,sheetno
               sheets.item giving sheet using 1
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop sheet.range(str4),*Value=PrjStr6
               setprop sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop sheet.range(str4),*Value=incname
.Jan total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!C",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!C",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.feb total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!D",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!D",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!E",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!E",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Apr total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!F",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!F",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.May total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!G",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!G",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.JUne total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!H",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!H",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.July total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!I",taskname
               append         str9,taskname
               append  "+'Brokerage LR Exch'!I",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Aug total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!J",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!J",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Sep total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!K",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!K",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Oct total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!L",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!L",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Nov total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!M",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!M",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Dec total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!N",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!N",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.total total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,total
               move           C0,rtotal
               move           C0,etotal
               add            jan1,rtotal
               add            feb1,rtotal
               add            mar1,rtotal
               add            apr1,rtotal
               add            may1,rtotal
               add            jun1,rtotal
               add            jul1,rtotal
               add            aug1,rtotal
               add            sep1,rtotal
               add            oct1,rtotal
               add            nov1,rtotal
               add            dec1,rtotal
               add            janx1,etotal
               add     febx1,etotal
               add            marx1,etotal
               add            aprx1,etotal
               add            mayx1,etotal
               add            junx1,etotal
               add            julx1,etotal
               add            augx1,etotal
               add            sepx1,etotal
               add            octx1,etotal
               add            novx1,etotal
               add            decx1,etotal
               add            etotal,total
               add            rtotal,total
bugger
               setprop sheet.range(str4),*Value=total
.........................................................................................
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!P",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!P",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!Q",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!Q",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.                     setprop sheet.range(str4),*Value="0.00"
.salesperson  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop sheet.range(str4),*Value=salesper
.unbilled
               move    howmany,str9
               call           Trim using str9
               pack           str4,"T",str9
               setprop sheet.range(str4),*Value=unbilled,*NumberFormat="##,####0_);[Red](##,####0)"
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.Total Previous year actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               clear          taskname
               append         "=SUM('Brokerage LR Rent'!U",taskname
               append         str9,taskname
               append         "+'Brokerage LR Exch'!U",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
........................
.ok lets do sheet 2
........................
.brokerage rental
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - Lr rental income totals "
               move           C2,sheetno
               sheets.item giving sheet using 2
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop        sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop sheet.range(str4),*Value=incname
.calculate rent %
               move           C0,rpercent
               move           rtotal,rpercent
               divide         total,rpercent
.Jan rental portion
               move           C0,mpercent
               move           jan1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           c0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb rental portion
               move           C0,mpercent
               move           feb1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           c0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar rental portion
               move           C0,mpercent
               move           mar1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr rental portion
               move           C0,mpercent
               move           apr1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May rental portion
               move           C0,mpercent
               move           may1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun rental portion
               move           C0,mpercent
               move           jun1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           c0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul rental portion
               move           C0,mpercent
               move           jul1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug rental portion
               move           C0,mpercent
               move           Aug1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear   taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep rental portion
               move           C0,mpercent
               move           sep1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct rental portion
               move           C0,mpercent
               move           Oct1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov rental portion
               move           C0,mpercent
               move           nov1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           c0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec rental portion
               move           c0,mpercent
               move           dec1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           c0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.rtotal total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           rpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*'Brokerage LR Totals'!O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.selected proj year  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop sheet.range(str4),*Value=projlast,*NumberFormat="##,####0_);[Red](##,####0)"
.year prior to selected proj year  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop sheet.range(str4),*Value=projlast1r,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop sheet.range(str4),*Value=salesper
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.year prior actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop sheet.range(str4),*Value=ACTLrR,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 2 rental portion
.ok lets do sheet 3
........................
.brokerage exchange
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - Lr Exchange income totals "
               move           C3,sheetno
               sheets.item giving sheet using 3
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop sheet.range(str4),*Value=incname
.calculate exchange %
               move           C0,rpercent
               move           etotal,epercent
               divide         total,epercent
.Jan exchange portion
               move           C0,mpercent
               move           janx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb exchange portion
               move           C0,mpercent
               move           febx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar exchange portion
               move           C0,mpercent
               move           marx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr exchange portion
               move           C0,mpercent
               move           aprx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May exchange portion
               move           C0,mpercent
               move           mayx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun exchange portion
               move           C0,mpercent
               move           junx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul exchange portion
               move           C0,mpercent
               move           julx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug exchange portion
               move           C0,mpercent
               move           Augx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep exchange portion
               move           C0,mpercent
               move           sepx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct exchange portion
               move           C0,mpercent
               move           Octx1,mpercent
               divide         Etotal into mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov exchange portion
               move           C0,mpercent
               move           novx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec exchange portion
               move           C0,mpercent
               move           decx1,mpercent
               divide         Etotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Etotal total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           Epercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*'Brokerage LR Totals'!O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop sheet.range(str4),*Value=projnew,*NumberFormat="##,####0_);[Red](##,####0)"
.prev to lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop sheet.range(str4),*Value=projLast1e,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop sheet.range(str4),*Value=salesper
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.year prior actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop sheet.range(str4),*Value=ACTLrE,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 3 exchange portion
.............................
.ok lets do sheet 5 & 6 & 7
               move           C0,NINtotal
               move           C0,rNINtotal
               move           C0,eNINtotal
               if      (Source = "B" or ndattdmc = "Y")
                              add            janNIN1,rnintotal
                              add            febNIN1,rnintotal
                              add            marNIN1,rnintotal
                              add            aprNIN1,rnintotal
                              add            mayNIN1,rnintotal
                              add            junNIN1,rnintotal
                              add            julNIN1,rnintotal
                              add            augNIN1,rnintotal
                              add            sepNIN1,rnintotal
                              add            octNIN1,rnintotal
                              add            novNIN1,rnintotal
                              add            decNIN1,rnintotal
                              add     janninx1,enintotal
                              add            febninx1,enintotal
                              add            marninx1,enintotal
                              add            aprninx1,enintotal
                              add            mayninx1,enintotal
                              add            junninx1,enintotal
                              add            julninx1,enintotal
                              add            augninx1,enintotal
                              add            sepninx1,enintotal
                              add            octninx1,enintotal
                              add            novninx1,enintotal
                              add            decninx1,enintotal
                              add            enintotal,nintotal
                              add            rnintotal,nintotal
               endif
....................................................................
.start with totals sheet for NIN income
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - NIN income totals "
               move           C5,sheetno
               sheets.item giving sheet using 5
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop sheet.range(str4),*Value=incname
.Jan total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!C",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!C",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.feb total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!D",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!D",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!E",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!E",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Apr total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!F",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!F",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.May total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!G",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!G",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.JUne total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!H",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!H",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.July total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!I",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!I",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Aug total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!J",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!J",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Sep total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!K",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!K",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Oct total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!L",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!L",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Nov total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!M",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!M",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Dec total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!N",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!N",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.brokerage nin totals
               move           NINTOTAL,NINPERc
               divide         total,NINPERc
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=('Brokerage LR Totals'!o",taskname
               append         str9,taskname
               append         "*",taskname
               append         NINPERC,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
...................................................................................
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!P",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!P",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
...................................................................................
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!Q",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!Q",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
...................................................................................
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               clear          taskname
               append         "=SUM('Brokerage NIN Rent'!U",taskname
               append         str9,taskname
               append         "+'Brokerage NIN Exch'!U",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.end brokerage NIN Totals
........................
........................
.brokerage rental  NIN INCome
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - NIN Rent income totals "
               move           C6,sheetno
               sheets.item giving sheet using 6
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop sheet.range(str4),*Value=incname
.calculate rent %
               move           C0,rpercent
               move           rNINtotal,rpercent
               divide         Rtotal,rpercent
.Jan rental portion
               move           C0,mpercent
               move           janNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb rental portion
               move           C0,mpercent
               move           febNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar rental portion
               move           C0,mpercent
               move           marNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr rental portion
               move           C0,mpercent
               move           aprNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May rental portion
               move           C0,mpercent
               move           mayNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun rental portion
               move           C0,mpercent
               move           junNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul rental portion
               move           C0,mpercent
               move           julNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug rental portion
               move           C0,mpercent
               move           AugNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep rental portion
               move           C0,mpercent
               move           sepNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct rental portion
               move           C0,mpercent
               move           OctNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov rental portion
               move           C0,mpercent
               move           novNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec rental portion
               move           C0,mpercent
               move           decNIN1,mpercent
               divide         RNINTotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.rtotal total
               move           C0,ninperc
               move           RNINTOTAL,NINPERc
               divide         Rtotal,NINPERc
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=('Brokerage LR Rent'!o",taskname
               append         str9,taskname
               append         "*",taskname
               move           C0,percent
               move           NINperc,percent
               call           trim using percent
               append         percent,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

.        move    howmany,str9
.        call    Trim using str9
.        pack    str4,"O",str9
.             clear   taskname
.        append  "=SUM(" to taskname
.        append  Rpercent to taskname
.        append  "*'Total NIN Income'!O" to taskname
.        append   str9 to taskname
.        append   ")" to taskname
.        reset    taskname
.        setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop        sheet.range(str4),*Value=ninlast,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop        sheet.range(str4),*Value=ninlast,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop        sheet.range(str4),*Value=salesper
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.lastyear actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop        sheet.range(str4),*Value=ActninR,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 6 rental portion
.ok lets do sheet 7
........................
.brokerage NIN exchange
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - NIN Exch income totals "
               move           C7,sheetno
               sheets.item giving sheet using 7
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop        sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=incname
.calculate exchange %
               move           C0,rpercent
               move           eNINtotal,epercent
               divide         Etotal,epercent
.Jan exchange portion
               move           C0,mpercent
               move           janninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb exchange portion
               move           C0,mpercent
               move           febninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar exchange portion
               move           C0,mpercent
               move           marninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr exchange portion
               move           C0,mpercent
               move           aprninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May exchange portion
               move           C0,mpercent
               move           mayninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun exchange portion
               move           C0,mpercent
               move           junninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul exchange portion
               move           C0,mpercent
               move           julninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug exchange portion
               move           C0,mpercent
               move           Augninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep exchange portion
               move           C0,mpercent
               move           sepninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct exchange portion
               move           C0,mpercent
               move           Octninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov exchange portion
               move           C0,mpercent
               move           novninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec exchange portion
               move           C0,mpercent
               move           decninx1,mpercent
               divide         ENINtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.ENIN total total
               move           C0,ninperc
               move           eNINTOTAL,NINPERc
               divide         etotal,NINPERc
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=('Brokerage LR Exch'!o",taskname
               append         str9,taskname
               append         "*",taskname
               move           C0,percent
               move           NINperc,percent
               call           trim using percent
               append         percent,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"

.
.        move    howmany,str9
.        call    Trim using str9
.        pack    str4,"O",str9
.             clear   taskname
.        append  "=SUM(" to taskname
.        append  Epercent to taskname
.        append  "*'Total NIN Income'!O" to taskname
.        append   str9 to taskname
.        append   ")" to taskname
.        reset    taskname
.        setprop sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop        sheet.range(str4),*Value=ninnew,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop        sheet.range(str4),*Value=ninlast1e,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop        sheet.range(str4),*Value=salesper
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.lastyear actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop        sheet.range(str4),*Value=ActninE,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 6 exchange NIN portion
              endif
              if (source = "M")
.Lets do List Management Commission (LR) first
               clear          NDATTDMC
               move           PrjStr6,NDAT3FLD
               move           "detout-NDAT3KEY",Location
               pack           KeyLocation,"Key: ",NDAT3FLD
               call           NDAT3KEY
               move           "detout-NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               move           PrjStr6,NDATFLD
               call           NDATKEY
               move           C4,sheetno
               sheets.item giving sheet using 4
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
              setprop        sheet.range(str4),*Value=PrjStr6
.               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=olstname

               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,total
               move           C0,Rtotal
               add            jan1,rtotal
               add            feb1,rtotal
               add            mar1,rtotal
               add            apr1,rtotal
               add            may1,rtotal
               add            jun1,rtotal
               add            jul1,rtotal
               add            aug1,rtotal
               add            sep1,rtotal
               add            oct1,rtotal
               add            nov1,rtotal
               add            dec1,rtotal
               add            janx1,rtotal
               add            febx1,rtotal
               add            marx1,rtotal
               add            aprx1,rtotal
               add            mayx1,rtotal
               add            junx1,rtotal
               add            julx1,rtotal
               add            augx1,rtotal
               add            sepx1,rtotal
               add            octx1,rtotal
               add            novx1,rtotal
               add            decx1,rtotal
               add            rtotal,total
               setitem        Proj3StatProgress3,0,"Populating spreadsheet - NIN LM income totals "

.Jan LM total
               move           C0,mpercent
               move           jan1,mpercent
               add            janx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb LM portion
               move           C0,mpercent
               move           feb1,mpercent
               add            febx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  LM portion
               move           C0,mpercent
               move           mar1,mpercent
               add            marx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr LM portion
               move           C0,mpercent
               move           apr1,mpercent
               add            aprx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May LM portion
               move           C0,mpercent
               move           may1,mpercent
               add            mayx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun LM portion
               move           C0,mpercent
               move           jun1,mpercent
               add            junx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul LM portion
               move           C0,mpercent
               move           jul1,mpercent
               add            julx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug LM portion
               move           C0,mpercent
               move           Aug1,mpercent
               add            Augx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep LM portion
               move           C0,mpercent
               move           sep1,mpercent
               add            sepx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct LM portion
               move           C0,mpercent
               move           Oct1,mpercent
               add            Octx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov LM portion
               move           C0,mpercent
               move           nov1,mpercent
               add            novx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec LM portion
               move           C0,mpercent
               move           dec1,mpercent
               add            decx1,mpercent
               divide         rtotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.LM total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               setprop        sheet.range(str4),*Value=Total
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop        sheet.range(str4),*Value=projlast,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop        sheet.range(str4),*Value=projlast1r,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - Not nec for LM
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop        sheet.range(str4),*Value=b1
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.Owner
               move           howmany,str9       ."7"
               call           Trim using str9
               pack           str4,"S",str9
               setprop        sheet.range(str4),*Value=Owner
.unbilled
               move           howmany,str9
               call           Trim using str9
               pack           str4,"T",str9
               setprop        sheet.range(str4),*Value=unbilled,*NumberFormat="##,####0_);[Red](##,####0)"

.Last year actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop        sheet.range(str4),*Value=ACTLRR,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 4 LM LR portion
.
.Lets do List Management INcome (NIN)
               move           C8,sheetno
               sheets.item giving sheet using 8
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
              setprop        sheet.range(str4),*Value=PrjStr6
.               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=olstname

               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,NINtotal
               add            JanNin1,nintotal
               add            FebNin1,nintotal
               add            MarNin1,nintotal
               add            AprNin1,nintotal
               add            MayNin1,nintotal
               add            JunNin1,nintotal
               add            JulNin1,nintotal
               add            AugNin1,nintotal
               add            SepNin1,nintotal
               add            OctNin1,nintotal
               add            NovNin1,nintotal
               add            DecNin1,nintotal
               add            JanNinx1,nintotal
               add            FebNinx1,nintotal
               add            MarNinx1,nintotal
               add            AprNinx1,nintotal
               add            MayNinx1,nintotal
               add            JunNinx1,nintotal
               add            JulNinx1,nintotal
               add            AugNinx1,nintotal
               add            SepNinx1,nintotal
               add            OctNinx1,nintotal
               add            NovNinx1,nintotal
               add            DecNinx1,nintotal

.Jan LM total
               move           C0,mpercent
               move           JanNin1,mpercent
               add            JanNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb LM portion
               move           C0,mpercent
               move           FebNin1,mpercent
               add            FebNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  LM portion
               move           C0,mpercent
               move           MarNin1,mpercent
               add            MarNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr LM portion
               move           C0,mpercent
               move           AprNin1,mpercent
               add            AprNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May LM portion
               move           C0,mpercent
               move           MayNin1,mpercent
               add            MayNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               append         mpercent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun LM portion
               move           C0,mpercent
               move           JunNin1,mpercent
               add            JunNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul LM portion
               move           C0,mpercent
               move           JulNin1,mpercent
               add            JulNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug LM portion
               move           C0,mpercent
               move           AugNin1,mpercent
               add            AugNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep LM portion
               move           C0,mpercent
               move           SepNin1,mpercent
               add            SepNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct LM portion
               move           C0,mpercent
               move           OctNin1,mpercent
               add            OctNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov LM portion
               move           C0,mpercent
               move           NovNin1,mpercent
               add            NovNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec LM portion
               move           C0,mpercent
               move           DecNin1,mpercent
               add            DecNinx1,mpercent
               divide         nintotal,mpercent
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           mpercent,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.LM total
               move           C0,ninperc
               move           ninTOTAL,NINPERc              .nin total
               divide         Rtotal,NINPERc               .lr total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               clear          taskname
               append         "=('List Management LR'!o",taskname
               append         str9,taskname
               append         "*",taskname
               move           C0,percent
               move           NINperc,percent
               call           trim using percent
               append         percent,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.        move    howmany,str9
.        call    Trim using str9
.        pack    str4,"O",str9
.        setprop sheet.range(str4),*Value=Total
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"P",str9
               setprop        sheet.range(str4),*Value=ninlast,*NumberFormat="##,####0_);[Red](##,####0)"
.lastyear  - add code to pull
               move           howmany,str9
               call           Trim using str9
               pack           str4,"Q",str9
               setprop        sheet.range(str4),*Value=ninlast1r,*NumberFormat="##,####0_);[Red](##,####0)"
.salesperson  - Not nec for LM
               move           howmany,str9
               call           Trim using str9
               pack           str4,"R",str9
               setprop        sheet.range(str4),*Value=b1
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.Owner
               move           howmany,str9
               call           Trim using str9
               pack           str4,"S",str9
               setprop        sheet.range(str4),*Value=Owner
.Last year actual
               move           howmany,str9
               call           Trim using str9
               pack           str4,"U",str9
               setprop        sheet.range(str4),*Value=ACTNINR,*NumberFormat="##,####0_);[Red](##,####0)"
.end of sheet 4 LM LR portion
        endif
.begin patch 1.3  sheet 9 br rental tdmc
.Lets do Triplex Brokerage Rental cost
              if (source = "B")
               move           C9,sheetno
               sheets.item giving sheet using 9
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop        sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=incname

               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,Tdmctotal
               add            JanTDMC,TDMCTotal
               add            FebTDMC,TDMCTotal
               add            MarTDMC,TDMCTotal
               add            AprTDMC,TDMCTotal
               add            MayTDMC,TDMCTotal
               add            JunTDMC,TDMCTotal
               add            JulTDMC,TDMCTotal
               add            AugTDMC,TDMCTotal
               add            SepTDMC,TDMCTotal
               add            OctTDMC,TDMCTotal
               add            NovTDMC,TDMCTotal
               add            DecTDMC,TDMCTotal
.Jan  total
               move           C0,TDMCperc
               move           JanTDMC,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb  portion
               move           C0,TDMCperc
               move           FebTDmc,TDMCperc
               divide         Tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  portion
               move           C0,TDMCperc
               move           MarTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr portion
               move           C0,TDMCperc
               move           AprTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May  portion
               move           C0,TDMCperc
               move           MayTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               append         TDMCperc,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun  portion
               move           C0,TDMCperc
               move           Juntdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul portion
               move           C0,TDMCperc
               move           Jultdmc,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug  portion
               move           C0,TDMCperc
               move           Augtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep  portion
               move           C0,TDMCperc
               move           Septdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct  portion
               move           C0,TDMCperc
               move           Octtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov  portion
               move           C0,TDMCperc
               move           Novtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec  portion
               move           C0,TDMCperc
               move           Dectdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
. total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9

               setprop        sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)",*value=TDMCTotal
.end of sheet 9
.Lets do Triplex Brokerage exchange cost
               move           "10",sheetno
               sheets.item giving sheet using 10
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop        sheet.range(str4),*Value=PrjStr6
               setprop        sheet.range(str4),*Value=NXRFFLD2
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=incname

               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,Tdmctotal
               add            JanTDMCx,TDMCTotal
               add            FebTDMCx,TDMCTotal
               add            MarTDMCx,TDMCTotal
               add            AprTDMCx,TDMCTotal
               add            MayTDMCx,TDMCTotal
               add            JunTDMCx,TDMCTotal
               add            JulTDMCx,TDMCTotal
               add            AugTDMCx,TDMCTotal
               add            SepTDMCx,TDMCTotal
               add            OctTDMCx,TDMCTotal
               add            NovTDMCx,TDMCTotal
               add            DecTDMCx,TDMCTotal
.Jan  total
               move           C0,TDMCperc
               move           JanTDMCx,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb  portion
               move           C0,TDMCperc
               move           FebTDmcx,TDMCperc
               divide         Tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  portion
               move           C0,TDMCperc
               move           MarTDMCx,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr portion
               move           C0,TDMCperc
               move           AprTDMCx,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May  portion
               move           C0,TDMCperc
               move           MayTDMCx,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               append         TDMCperc,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun  portion
               move           C0,TDMCperc
               move           Juntdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul portion
               move           C0,TDMCperc
               move           Jultdmcx,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug  portion
               move           C0,TDMCperc
               move           Augtdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep  portion
               move           C0,TDMCperc
               move           Septdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct  portion
               move           C0,TDMCperc
               move           Octtdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov  portion
               move           C0,TDMCperc
               move           Novtdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec  portion
               move           C0,TDMCperc
               move           Dectdmcx,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
. total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               setprop        sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)",*value=TDMCTotal
.end of sheet 10
               endif
              if (source = "M")
.Lets do Triplex List Management cost
               move           C11,sheetno
               sheets.item giving sheet using 11
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               add            C1,howmany
               store          howmany to sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8,hmany9,hmany10,hmany11
               move           howmany,str9
               call           Trim using str9
               pack           str4,"A",str9
.START PATCH 1.4 REPLACED LOGIC
.              setprop        sheet.range(str4),*Value=PrjStr6
.              setprop        sheet.range(str4),*Value=NXRFFLD2
               setprop        sheet.range(str4),*Value=NDatfld
.END PATCH 1.4 REPLACED LOGIC
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"B",str9
               setprop        sheet.range(str4),*Value=incname

               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               move           C0,Tdmctotal
               add            JanTDMC,TDMCTotal
               add            FebTDMC,TDMCTotal
               add            MarTDMC,TDMCTotal
               add            AprTDMC,TDMCTotal
               add            MayTDMC,TDMCTotal
               add            JunTDMC,TDMCTotal
               add            JulTDMC,TDMCTotal
               add            AugTDMC,TDMCTotal
               add            SepTDMC,TDMCTotal
               add            OctTDMC,TDMCTotal
               add            NovTDMC,TDMCTotal
               add            DecTDMC,TDMCTotal
.Jan  total
               move           C0,TDMCperc
               move           JanTDMC,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"C",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.Feb  portion
               move           C0,TDMCperc
               move           FebTDmc,TDMCperc
               divide         Tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"D",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.
.mar  portion
               move           C0,TDMCperc
               move           MarTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"E",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Apr portion
               move           C0,TDMCperc
               move           AprTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"F",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.May  portion
               move           C0,TDMCperc
               move           MayTDMC,TDMCperc
               divide         TDMCtotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"G",str9
               clear          taskname
               append         "=SUM(",taskname
               append         TDMCperc,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jun  portion
               move           C0,TDMCperc
               move           Juntdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"H",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Jul portion
               move           C0,TDMCperc
               move           Jultdmc,TDMCperc
               divide         TDmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"I",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Aug  portion
               move           C0,TDMCperc
               move           Augtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"J",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Sep  portion
               move           C0,TDMCperc
               move           Septdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"K",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Oct  portion
               move           C0,TDMCperc
               move           Octtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"L",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Nov  portion
               move           C0,TDMCperc
               move           Novtdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"M",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
.Dec  portion
               move           C0,TDMCperc
               move           Dectdmc,TDMCperc
               divide         tdmctotal,TDMCperc
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"N",str9
               clear          taskname
               append         "=SUM(",taskname
               move           C0,percent
               move           TDMCperc,percent
               call           trim using percent
               append         percent,taskname
               append         "*O",taskname
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Formula=Taskname,*NumberFormat="##,####0_);[Red](##,####0)"
. total
               move           howmany,str9
               call           Trim using str9
               pack           str4,"O",str9
               setprop        sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)",*value=TDMCTotal
.end of sheet 11

        endif
.end of patch 1.3
        goto       looper2
.
hdout
.Excel Header sheet One (1) Brokerage LR Income
              move            C1,sheetno
              sheets.item giving sheet using 1
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage Commission Income Projections"
              call            hdout1
.
.Excel Header sheet two (2) Brokerage LR rental Income
              move            C2,sheetno
              sheets.item giving sheet using 2
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage Commission Rental Income Projections"
              call            hdout1
.
.Excel Header sheet (3) Brokerage LR exchange Income
              move            C3,sheetno
              sheets.item giving sheet using 3
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage Commission Exchange Income Projections"
              call            hdout1
.
.Excel Header sheet (4) List Management LR Income
              move            C4,sheetno
              sheets.item giving sheet using 4
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="List Managenent LR Income Projections"
              call            hdout1
.
.Excel Header sheet  (5) Brokerage NIN Income
              move            C5,sheetno
              sheets.item giving sheet using 5
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage NIN Income Projections"
              call            hdout1
.
.Excel Header sheet  (6) Brokerage NIN Rental Income
              move            C6,sheetno
              sheets.item giving sheet using 6
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage NIN Rental Income Projections"
              call            hdout1
.
.Excel Header sheet (7) Brokerage NIN exch Income
              move            C7,sheetno
              sheets.item giving sheet using 7
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage NIN Exchange Income Projections"
              call            hdout1
.
.Excel Header sheet (8) List Managemnet NIN Income
              move            C8,sheetno
              sheets.item giving sheet using 8
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
.START PATCH 1.66 REPLACED LOGIC
.             setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS"
.END PATCH 1.66 REPLACED LOGIC
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="List Management NIN Income Projections"
              call            hdout1
.begin patch 1.3
.Excel Header sheet (9) Triplex Brokerage rental
              move            C9,sheetno
              sheets.item giving sheet using 9
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage Rental TDMC Charges"
              call            hdout1
.Excel Header sheet (10)  Triplex Brokerage exchange
              move            "10",sheetno
              sheets.item giving sheet using 10
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="Brokerage Exchange TDMC Charges"
              call            hdout1
.Excel Header sheet (11)    List Managemnet TDMC COst
              move            "11",sheetno
              sheets.item giving sheet using 11
              move            C0,RecordTop
              move            C0,RecordHeader
              move            C1,howmany
              move            howmany,str9       ."1"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
              setprop         sheet.range(str4).Font,*Bold="True"
              add             C1,howmany
              move            howmany,str9       ."2"
              call            Trim using str9
              pack            str4,"A",str9
              setprop         sheet.range(str4),*Value="List Management TDMC Charges"
              call            hdout1
.end patch 1.3
        return
.
.hdout1         sets headers for all sheets
hdout1
              pack            str4,"F",str9
              setprop         sheet.range(str4),*Formula="=TODAY()",*NumberFormat="dd-mmm-yy"
.column A   MLR #
              add             C2,howmany
              move            howmany,str9       ."5"
              call            Trim using str9
              pack            str4,"A",str9
.begin patch 1.3
.             if (SHEETNO = 4 OR SHEETNO = 8)
              if (SHEETNO = 4 OR SHEETNO = 8 or SheetNo = 11)
.end patch 1.3
               setprop        sheet.range(str4),*Value="LIST ##"
              else
               setprop        sheet.range(str4),*Value="MLR ##"
              endif
              setprop         sheet.range(str4).Font,*Bold="True"
.Column b   mlr OR LIST name
              move            howmany,str9       ."6"
              call            Trim using str9
              pack            str4,"B",str9
              setprop         sheet.range(str4),*Value="Name"
.Jan
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"C",str9
              setprop         sheet.range(str4),*Value="January"
.Feb
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"D",str9
              setprop         sheet.range(str4),*Value="February"
.Mar
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"E",str9
              setprop         sheet.range(str4),*Value="March"
.Apr
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"F",str9
              setprop         sheet.range(str4),*Value="April"
.may
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"G",str9
              setprop         sheet.range(str4),*Value="May"
.Jun
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"H",str9
              setprop         sheet.range(str4),*Value="June"
.July
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"I",str9
              setprop         sheet.range(str4),*Value="July"
.aug
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"J",str9
              setprop         sheet.range(str4),*Value="August"
.sep
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"K",str9
              setprop         sheet.range(str4),*Value="September"
.oct
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"L",str9
              setprop         sheet.range(str4),*Value="October"
.Nov
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"M",str9
              setprop         sheet.range(str4),*Value="November"
.Dec
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"N",str9
              setprop         sheet.range(str4),*Value="December"
.Total
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"O",str9
              setprop         sheet.range(str4),*Value="Total"
.Previous year
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"P",str9
              move            Prevyear to str5
              setprop         sheet.range(str4),*Value=str5
.Previous year
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"Q",str9
              move            Prevyear to n4
              sub             c1 from n4
              move            n4 to str5
              setprop         sheet.range(str4),*Value=str5
.Salesperson
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"R",str9
              setprop         sheet.range(str4),*Value="Salesperson"
.begin patch 1.91
.
               move           howmany,str9
               call           Trim using str9
               pack           str4,"V",str9
               setprop sheet.range(str4),*Value=PCompCde
.end patch 1.91
.Owner
              if (sheetno = 4 or sheetno = 8)
               move           howmany,str9       ."7"
               call           Trim using str9
               pack           str4,"S",str9
               setprop        sheet.range(str4),*Value="Owner ##"
              endif
.Unbilled
              move            howmany,str9       ."7"
              call            Trim using str9
              pack            str4,"T",str9
              setprop         sheet.range(str4),*Value="Unbilled"
.
              add             C1,howmany
.             setprop         sheet.range(str4),*Value=OMLRKY,*HorizontalAlignment=xlHAlignCenter

.begin patch 1.3
.             store           howmany into sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
              store           howmany into sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8:
                              Hmany9,hmany10,hmany11
.end patch 1.3
        return
.
EOJ
              move            C1,sheetno

eoj1
              loop
               if (sheetno = 1)
                              sheets.item giving sheet using 1
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 2)
                              sheets.item giving sheet using 2
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 3)
                              sheets.item giving sheet using 3
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 4)
                              sheets.item giving sheet using 4
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 5)
                              sheets.item giving sheet using 5
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 6)
                              sheets.item giving sheet using 6
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 7)
                              sheets.item giving sheet using 7
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 8)
                              sheets.item giving sheet using 8
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
.begin patch 1.3
               if (sheetno = 9)
                              sheets.item giving sheet using 9
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 10)
                              sheets.item giving sheet using 10
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
               if (sheetno = 11)
                              sheets.item giving sheet using 11
                              sheet.range("B1","Q1000").Columns.AutoFit
               endif
.              load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8
               load           howmany from sheetno of hmany1,hmany2,hmany3,hmany4,hmany5,hmany6,hmany7,hmany8:
                                             Hmany9,hmany10,hmany11
.end patch 1.3
               reset          str16
               clear          str16
               move           "CDEFGHIJKLMNOPQT",str16       .setprop the column
               move           str16,str1
               move           howmany,endrow
               add            C2,endrow

               call           totrow
               add            C1,sheetno
              repeat while (Sheetno < 9)
              goto filenameselect

totrow
              loop
               move           endrow,str9
               call           Trim using str9
               pack           str4,str1,str9
               clear          taskname
               append         "=sum(",taskname
               append         str1,taskname
               append         "6:",taskname
               append         str1,taskname
               move           howmany,str9
               call           trim using str9
               append         str9,taskname
               append         ")",taskname
               reset          taskname
               setprop        sheet.range(str4),*Value=taskname,*NumberFormat="##,####0_);[Red](##,####0)"
               bump           str16,1
               move           str16,str1
              repeat while (str1 <> "T")
              return
.
FileNameSelect
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
              move            N2,str2
              rep             zfill,str2
              clear           taskname
.        append  "c:\data\projections",taskname
              append          "c:\work\projections",taskname
              append          "_",taskname
              append          str2,taskname
              append          ".xlsx",taskname
              reset           taskname
              setprop         ex,*DisplayAlerts=OFALSE
              clear           APIFileName
              pack            APIFileName,taskname,hexzero
              call            FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
.File exists, try incrementing name
               goto TrapObject2
        endif
              trap            TrapObject if Object
              book.saveas giving N9 using *Filename=taskname
              trapclr         Object
.START PATCH 1.1 ADDED LOGIC
              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.1 ADDED LOGIC
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first
              destroy         rowcol
              destroy         sheet
              destroy         sheets
              destroy         book
              destroy         books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
.Keep spreadsheet open
.             ex.quit
              destroy         ex
              return
TrapObject
              noreturn
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.
TrapObject2
              if (N2 >= 99)
               goto CleanUp     .Quit the job
              endif
              add             C1,N2
.Send them back to select another File name and try to Save again.
              goto FileNameSelect
              stop
..................................................................................................
.................................Dynamic resizing of Forms........................................
..................................................................................................
.OrderMaximizeScreen
..coll1       collection
.
.             create  font5,"Arial",size=12
..Getinfo - NOT YET IMPLEMENTED!!!!!!!
.             getinfo system,infostring
.             bump    infostring,12
.             move    infostring,str4
.             bump    infostring,4
.             move    infostring,str5
.
.              move    "1.25",size
.              getprop coll1,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.              mult    size,specs
.              setprop coll1,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5

ProjResetScreen Routine FrmPtr
              getinfo system,infostring
              bump    infostring,12
              unpack          infostring,ScrH,ScrV
.             bump    infostring,4
.             move    infostring,str5
.Default Screen is always designed for 640X480 resolution.  Make adjustments as necessary
              div             "640",ScrH,sizeH
              div             "480",ScrV,sizeV
.NPRO0001.PLF
              getprop         NPRO0001,font=font5
              getprop         font5,size=N32
              if (FrmPtr = 0)
               mult    sizeH,N32
              else
               div            sizeH,N32
              endif
.
              getprop         ProjExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         ProjExit,font=font5
              setprop         font5,size=N32
              setprop         ProjExit,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         ProjExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         ProjTabControl001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         ProjTabControl001,font=font5
              setprop         font5,size=N32
              setprop         ProjTabControl001,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         ProjTabControl001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
.NPRO0002.PLF
              getprop         Proj2Combo2Source,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Combo2Source,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Combo2Type,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Combo2Type,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2ComboSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2ComboSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2ComboSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2ComboSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2ComboType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2ComboType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Delete,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Delete,font=font5
              setprop         font5,size=N32
              setprop         Proj2Delete,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Delete,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2Client,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2Client,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2Client,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2Client,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2Date,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2Date,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2Date,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2Date,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2Key,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2Key,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2Key,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2Key,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2LRInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2LRInc,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2LRInc,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2LRInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2NINInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2NINInc,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2NINInc,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2NINInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2Notes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2Notes,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2Notes,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2Notes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Edit2Year,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Edit2Year,font=font5
              setprop         font5,size=N32
              setprop         Proj2Edit2Year,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Edit2Year,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2EditClient,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2EditClient,font=font5
              setprop         font5,size=N32
              setprop         Proj2EditClient,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2EditClient,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2EditFromYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2EditFromYr,font=font5
              setprop         font5,size=N32
              setprop         Proj2EditFromYr,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2EditFromYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2EditToYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2EditToYr,font=font5
              setprop         font5,size=N32
              setprop         Proj2EditToYr,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2EditToYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2GroupBox,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2GroupBox,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2GroupBox001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2GroupBox001,font=font5
              setprop         font5,size=N32
              setprop         Proj2GroupBox001,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2GroupBox001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2ListView,top=specs(1),height=specs(2),left=specs(3),width=specs(4)                                  Control Class="{BDD1F04B-858B-11D1-B16A-00C0F0283628}"
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2ListView,top=specs(1),height=specs(2),left=specs(3),width=specs(4)                   Control Class="{BDD1F04B-858B-11D1-B16A-00C0F0283628}"
.
              getprop         Proj2ListView2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)                Control Class="{BDD1F04B-858B-11D1-B16A-00C0F0283628}"
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2ListView2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)                  Control Class="{BDD1F04B-858B-11D1-B16A-00C0F0283628}"
.
              getprop         Proj2Modify,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Modify,font=font5
              setprop         font5,size=N32
              setprop         Proj2Modify,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Modify,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2New,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2New,font=font5
              setprop         font5,size=N32
              setprop         Proj2New,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2New,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2OK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2OK,font=font5
              setprop         font5,size=N32
              setprop         Proj2OK,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2OK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Quit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Quit,font=font5
              setprop         font5,size=N32
              setprop         Proj2Quit,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Quit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Save,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Save,font=font5
              setprop         font5,size=N32
              setprop         Proj2Save,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Save,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Client,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Client,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Client,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Client,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2ClientName,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2ClientName,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2ClientName,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2ClientName,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Date,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Date,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Date,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Date,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Key,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Key,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Key,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Key,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2LRInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2LRInc,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2LRInc,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2LRInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2ModDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2ModDate,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2ModDate,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2ModDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2ModDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2ModDate2,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2ModDate2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2ModDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2NINInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2NINInc,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2NINInc,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2NINInc,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Notes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Notes,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Notes,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Notes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Rec,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Rec,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Rec,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Rec,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Records,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Records,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Records,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Records,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Sales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Sales,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Sales,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Sales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Sales2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Sales2,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Sales2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Sales2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Source,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Source,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Source,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Source,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Type,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Type,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Type,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Type,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stat2Year,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stat2Year,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stat2Year,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stat2Year,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatClient,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatClient,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatClient,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatClient,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatClient2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatClient2,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatClient2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatClient2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatClient3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatClient3,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatClient3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatClient3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatFromYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatFromYr,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatFromYr,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatFromYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatFromYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatFromYr2,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatFromYr2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatFromYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatFromYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatFromYr3,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatFromYr3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatFromYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatRec,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatRec,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatRec,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatRec,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatRecords,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatRecords,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatRecords,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatRecords,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatSales,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatSales,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatSource,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatSource,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatSource2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatSource2,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatSource2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatSource2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatSource3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatSource3,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatSource3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatSource3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatToYear,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatToYear,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatToYear,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatToYear,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatToYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatToYr2,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatToYr2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatToYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatToYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatToYr3,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatToYr3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatToYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatType,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatType,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatType2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatType2,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatType2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatType2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2StatType3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2StatType3,font=font5
              setprop         font5,size=N32
              setprop         Proj2StatType3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2StatType3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj2Stop,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj2Stop,font=font5
              setprop         font5,size=N32
              setprop         Proj2Stop,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj2Stop,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.NPRO0003.PLF
              getprop         Proj3ComboSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3ComboSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3ComboSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3ComboSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3ComboType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3ComboType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3EditFromDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3EditFromDate,font=font5
              setprop         font5,size=N32
              setprop         Proj3EditFromDate,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3EditFromDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3EditPrevYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3EditPrevYr,font=font5
              setprop         font5,size=N32
              setprop         Proj3EditPrevYr,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3EditPrevYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3EditToDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3EditToDate,font=font5
              setprop         font5,size=N32
              setprop         Proj3EditToDate,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3EditToDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3GroupBox001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3GroupBox001,font=font5
              setprop         font5,size=N32
              setprop         Proj3GroupBox001,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3GroupBox001,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3OK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3OK,font=font5
              setprop         font5,size=N32
              setprop         Proj3OK,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3OK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3ProgressBar,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3ProgressBar,font=font5
              setprop         font5,size=N32
              setprop         Proj3ProgressBar,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3ProgressBar,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatFromDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatFromDate,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatFromDate,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatFromDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatFromDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatFromDate2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatFromDate2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatFromDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatFromDate3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatFromDate3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatFromDate3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatFromDate3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatPrevYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatPrevYr,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatPrevYr,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatPrevYr,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatPrevYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatPrevYr2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatPrevYr2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatPrevYr2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatPrevYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatPrevYr3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatPrevYr3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatPrevYr3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatProgress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatProgress,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatProgress,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatProgress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatProgress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatProgress2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatProgress2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatProgress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatProgress3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatProgress3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatProgress3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatProgress3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatSales,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatSales,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatSales,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatSource,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatSource,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatSource,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatSource2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatSource2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatSource2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatSource2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatSource3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatSource3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatSource3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatSource3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatToDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatToDate,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatToDate,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatToDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatToDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatToDate2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatToDate2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatToDate2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatToDate3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatToDate3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatToDate3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatToDate3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatType,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatType,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatType,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatType2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatType2,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatType2,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatType2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3StatType3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3StatType3,font=font5
              setprop         font5,size=N32
              setprop         Proj3StatType3,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3StatType3,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.
              getprop         Proj3Stop,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              getprop         Proj3Stop,font=font5
              setprop         font5,size=N32
              setprop         Proj3Stop,font=font5
              if (FrmPtr = 0)
               mult    sizeV,specs(1)
               mult    sizeV,specs(2)
               mult    sizeH,specs(3)
               mult    sizeH,specs(4)
              else
               div    sizeV,specs(1)
               div    sizeV,specs(2)
               div    sizeH,specs(3)
               div    sizeH,specs(4)
              endif
              setprop         Proj3Stop,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
              return

              include nxrfio.inc
              include ndat3io.inc
.Patch1.6
              include         compio.inc
              include         cntio.inc
.include nmlrio.inc
.Ptch1.6
              include ndatio.inc
              include         gnxtio.inc
              include         nordio.inc
              include         nmrgio.inc
              include         nbilio.inc
              include         nownio.inc
              include         nshpio.inc
              include         nacdio.inc
.begin patch 1.7
.             include         compute.inc
              include         compute.inc
              Include         ninvio.inc
              INClude         NInvAcdio.inc
.             include         ninvio.inc
.end patch 1.7
              include         njstio.inc
              include         nadjio.inc
.Patch1.6
.include      nbrkio.inc
.Patch1.6
              include         nrtnio.inc
              include         nprjio.inc
.begin patch 1.3
               Include        Tinvio.inc
               include        NRCHGio.INC
.end patch 1.3
        include searchio.inc      .contains logic for search.plf
.Start - Files needed only for Search.plf
              include         ncmpio.inc
.End   - Files needed only for Search.plf
.START PATCH 1.5 - ADDED LOGIC
              INCLUDE         NSELIO.INC
              INCLUDE         NSEL2IO.INC
              INCLUDE         NTXTIO.INC
.END PATCH 1.5 - ADDED LOGIC
.begin patch 1.8
              Include         Nrevio.inc
.end patch 1.8
              include comlogic.inc

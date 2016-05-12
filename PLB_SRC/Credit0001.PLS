PC       EQU       0
              INCLUDE         COMMON.INC
              INCLUDE         CONS.INC
              INCLUDE         NSHPDD.INC
              INCLUDE         NORDDD.INC
.START PATCH 1.51 REPLACED LOGIC
.             INCLUDE         NMLRDD.INC
.             INCLUDE         NbrkDD.INC
              include         compdd.inc
              include         cntdd.inc
.END PATCH 1.51 REPLACED LOGIC
              INCLUDE         NOWNDD.INC
              include         winapi.inc
.begin patch 1.8
.             include         ninvdd.inc
              include         ninvdd.inc
              include         Ninvacddd.inc
.end patch 1.8
              include         ndatdd.inc
.begin patch 1.3
              include       nadjdd.inc
              include       consacct.inc
              include       nmrgdd.inc
              include       nacddd.inc
              include       ndat3dd.inc
.end patch 1.3
.Following used only in order to load Search.plf
               include        ncmpdd.inc
               include        nrtndd.inc
.begin patch 1.5
               include        njstdd.inc
.end patch 1.5
.begin patch 1.81
NumProc       form            9
NumHit        form            5
.end patch 1.81
.begin patch 1.86  
           include    nmlddd.inc
.end patch 1.86  

Release        init           "1.86"               DLH    .user orginal maildate for aging
RelDate        Init           "2015 january 26"
.Release        init           "1.85"               DLH    optimize
.RelDate        Init           "12 February 2010"
.Release        init           "1.84"               DLH    Paid Total
.RelDate        Init           "Sep 6, 2007"
.Release        init           "1.83"               JD    Insert new NIN guaranty column.
.RelDate        Init           "Aug 24, 2006"
.Release        init           "1.82"              DLH   Change Variable N9 to IN9  as new compute can zero out the variable N9
.RelDate        Init           "Oct 18, 2005"
.Release        init           "1.81"              DLH  5May2005 Change to REadlast, Readkgp
.RelDate        Init           "May 5, 2005"
.Release        init           "1.8"              DLH  2005March02 Invoice file conversion
.RelDate        Init           "March 2, 2005"
.Release        init           "1.7"              ASH  12Oct2004 CTF Request - Also added winshow statement
.RelDate        Init           "October 12, 2004"
.Release        init           "1.6"              DLH  29June2004 add OPtions - search mailer only records (no brk)
.RelDate        Init           "June 29, 2004"
.Release  init      "1.51"              JD  26MAY2004 MAILER CONVERSION
.Release  init      "1.50"              JD  22Aug03    Added adjustment detail read for advlopay
.Release  init      "1.43"             DLH 005Mar03   .Add broker/consultant to list view
.RelDate  Init      "March 5, 2003"
.                                                    and problem with comlumn click on view 2
.Release  init      "1.42"             DLH 18Dec02   .Force type of date select if dates entered
.Release  init      "1.41"             DLH 15Apr02   .Turned off calls to AnimateIt
.Release  init      "1.4"             DLH 24Oct01   .Changed listview index for "Days" view so they are in order.
.Release  init      "1.3"             DLH 17May01   .Added Acct receivable fields and objects, fixed listview focus after
.                                                   tab change
.Release  init      "1.2"             DLH 17May01
.RelDate  Init      "May 17, 2001"
.Release  init      "1.1"             DLH 06April01
.Release  init      "1.0"             DLH 28March01 NEW
.............................................................................
..............................................................
DIFF     FORM      10
fivePER  FORM      10
last     form      3
Days     dim       5
NDays    form      5
Akey1    init      "01X"
Akey4    init      "04X"
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
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
ManFeeflag dim      1
.....................................................
holdmdate form     5
DateOkFlag dim     1
okformore init     "Y"
longest    form     5
longestO   form     5
shortest   form     5
shortestO  form     5
PaidDays   form     7
OpenDays   form     7
Paidcount  form     5
opencount  form     5
prepayflag dim      1
prepaycount form    5
prepaycounto form   5
Guarcount   form    5         guaranteed and paid
GuarcountO  form    5         guaranteed and open
AdvLoPay    form    5         Advanced payment to List Owner
AdvLoPayO   form    5         Advanced payment to List Owner  on open invoice
.begin patch 1.3
ARTotal     form    9.2
.end patch 1.3
.begin patch 1.84
ARPTotal     form    9.2           total paid
.end patch 1.84
.....counters for Non List Management
longestN    form     5
longestNO   form     5
shortestN   form     5
shortestNO  form     5
PaidDaysN   form     7
OpenDaysN   form     7
PaidcountN  form     5
opencountN  form     5
prepayflagN dim      1
prepaycountN form    5
prepaycountNo form   5
GuarcountN   form    5         guaranteed and paid
GuarcountNO  form    5         guaranteed and open
AdvLoPayN    form    5         Advanced payment to List Owner
AdvLoPayNO   form    5         Advanced payment to List Owner  on open invoice
.begin patch 1.3
ARNonLO      form    9.2
.end patch 1.3
........................
DateBranch  form    1       .'1=no date, 2=order date, 3=maildate, 4=returndate, 5= invoice date
newdate1    dim    10      mm/dd/ccyy
newdate2    dim    10      mm/dd/ccyy
startdate   form      5
Enddate     form      5
str10a      dim     10
CreditStopFlag dim   1         .No if break search loop
.begin patch 1.3
Mask14      INIT     "$$$,$$$,$$9.99"
CalcARFlag  dim      1
shipsw      dim      1
mrgsw       dim      1
.end patch 1.3
JSTN     DIM       2
........................................................................................................
.
.
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
sColor  submenu
sSearch submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
.EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2;-;&Color;&Preferences;"
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
.
endindex form      9
......Search  form       1
Timer   Timer
holdsInfo dim      36

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

.................................
coll1         collection
specs         form          4(4)
size          form          "1.000"
infostring    dim        590
Tabnum        form          2
SaveTab       form          2
ListViewNum   form          2
.............................
.Constants for Animate Icon
.These coordinates will place Icon directly
AniH    init    "20"
.AniV    init    "350"
AniV    init    "550"
................................................................................................................................
.begin patch 1.6
preffile       file
ObjectColl    Collection
.Objects used by OptionsOrd.plf              (a generic form)
.OptionsArrayEx               dim            x(y,z)
.Following Array will allow:  y              rows - representing each Screen
.                                   z        columns        - representing # of preferances              for each Screen
.                                   x        length of each field in       each Screen
.
OptionsArr1 CONST             "10"
OptionsArr2 CONST             "5"
OptionsArrSize CONST          "5"
OptionsArray dim              OptionsArrSize(OptionsArr1,OptionsArr2)
.Screen       1
Options1Coll   Collection
OptionsScreenInit             ComboBox
OptionsMLROnly                CheckBox
.Screen       2
Options2Coll   Collection
.Screen       3
Options3Coll   Collection
.Screen       4
Options4Coll   Collection
.Screen       5
Options5Coll   Collection
OptionsScreen5BrkFilter       CheckBox
OptionsScreen5EMailOption CheckBox
OptionsScreen5FileDefault CheckBox
.Screen       6
Options6Coll   Collection
.OptionsScreen6Init CheckBox
.Screen       7
Options7Coll   Collection
OptionsScreen7Proj CheckBox
.Screen       8
Options8Coll   Collection
.OptionsScreen8OrderSearch CheckBox
.Screen       9
Options9Coll   Collection
OptionsScreen9View CheckBox
.Screen       10
Options10Coll  Collection
OptionsScreen10View CheckBox
OptionsScreen10Usage CheckBox
.end patch 1.6
................................................................................................................................

+
.............................................................................................................
.Set Vars used for About Box
        move    "Credit0001.PLS",Wprognme
        move    "Credit Information",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
.        move    "April 24, 2001",Wreldate
.
.Declare forms, Always declare child forms first
.rpt     plform  Report
.begin patch 1.85
.animicon       plform         Animate         .CONTAINS ALL THE ICONS
.end patch 1.85
SRCH           plform         Search
mss1           plform         Error
abt            plform         About
Credita        plform         credit0001A
Creditb        plform         credit0001B
.begin patch 1.6
opt           plform          OptionsOrd
.end patch 1.6
x              plform         credit0001
               winhide
.Load Forms, Always load parent form first
               formload       x
               formload       creditb,credit0001
               formload       credita,credit0001
               formload       abt
               formload       mss1
               formload       SRCH
.begin patch 1.6
              formload       opt
.end patch 1.6
.begin patch 1.85
.               formload       animicon
.end patch 1.85
.Set tab index
        move    C2,TabNum
.begin patch 1.85
.Dynamically reset Animate as same size as Main window
        getprop credit0001,height=H
.        setprop Animate,height=H
        getprop credit0001,width=V
.        setprop Animate,width=V
.end patch 1.85
.

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        CREATE  credit0001;MFile,FData
        create  Credit0001;mOptions,OData,mFile
        create  Credit0001;mHelp,HData,mOptions
        CREATE  Credit0001;sCOlor,Cdata,mOptions,1
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.begin patch 1.6
        activate       mOptions,OptionsGo,Result
.end patch 1.6
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
.Create SubMenu
        create  Credit0001;sSearch,SData,mOptions,1
.Activate SubMenus
        activate sSearch,SearchGo,result
        activate Credit001Radio001
.set properties in collection
        listins ColText,Credit0001,credit001EditText001,credit001EditText002,credit001bEditText003:
                credit001bEditText004,credit001bEditText005,credit001bEditText006,credit001bEditText007:
                credit001bEditText008,credit001bEditText009,credit001bEditText010,credit001bEditText011:
                credit001bEditText012,Credit001GoButton:
                Credit001Mlrcomp:
                credit001bstatText003,credit001bstatText004,credit001bstatText005,credit001bstatText006:
                credit001bstatText007,credit001bstatText008,credit001bstatText009,credit001bstatText010:
                credit001bstatText011,credit001bstatText012,credit001bstatText013,credit001bstatText014:
                credit001bstatText015,credit001bstatText016,credit001bstatText017,credit001bstatText018:
                Credit001brkcomp

        listins Colback,Credit0001,credit001bEditText003:
                credit001bEditText004,credit001bEditText005,credit001bEditText006,credit001bEditText007:
                credit001bEditText008,credit001bEditText009,credit001bEditText010,credit001bEditText011:
                credit001bEditText012,Credit001GoButton:
                Credit001Mlrcomp:
                credit001bstatText003,credit001bstatText004,credit001bstatText005,credit001bstatText006:
                credit001bstatText007,credit001bstatText008,credit001bstatText009,credit001bstatText010:
                credit001bstatText011,credit001bstatText012,credit001bstatText013,credit001bstatText014:
                credit001bstatText015,credit001bstatText016,credit001bstatText017,credit001bstatText018:
                Credit001brkcomp
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black
.
.Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\program files\nincal\credit0001.col"
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

.Create Credit001aListView001 Columns
.Column Clicking
        Credit001aListView001.InsertColumn using "LR",50,1
.START PATCH 1.7 REPLACED LOGIC
.        Credit001aListView001.InsertColumn using "Orderdate",75,2
.        Credit001aListView001.InsertColumn using "Status",80,3
.        Credit001aListView001.InsertColumn using "Days",50,4
.        Credit001aListView001.InsertColumn using "Maildate",75,5
.        Credit001aListView001.InsertColumn using "List",190,6
.        Credit001aListView001.InsertColumn using "Amount Due",75,7
.;begin patch 1.43
.        Credit001aListView001.InsertColumn using "Broker/Consultant",75,8
.;end patch 1.43
.        CREDIT001aListView001.SetColumnFormat using 3,1              .set DAYS column justify right
...............................................

.begin patch 1.83
.              Credit001aListView001.InsertColumn using "Broker GOP",80,2
.              Credit001aListView001.InsertColumn using "Orderdate",70,3
.              Credit001aListView001.InsertColumn using "Status",65,4
.              Credit001aListView001.InsertColumn using "Days",50,5
.              Credit001aListView001.InsertColumn using "Maildate",70,6
.              Credit001aListView001.InsertColumn using "List",175,7
.              Credit001aListView001.InsertColumn using "Amount Due",75,8
.              Credit001aListView001.InsertColumn using "Broker/Consultant",75,9
.        CREDIT001aListView001.SetColumnFormat using 4,1              .set DAYS column justify right
.              Setprop Credit001aListView001,Sortorder=2
..........................................................................................................
              Credit001aListView001.InsertColumn using "Broker GOP",80,2
              Credit001aListView001.InsertColumn using "NIN GOP",65,3
              Credit001aListView001.InsertColumn using "Orderdate",70,4
              Credit001aListView001.InsertColumn using "Status",65,5
              Credit001aListView001.InsertColumn using "Days",50,6
              Credit001aListView001.InsertColumn using "Maildate",70,7
              Credit001aListView001.InsertColumn using "List",175,8
              Credit001aListView001.InsertColumn using "Amount Due",75,9
              Credit001aListView001.InsertColumn using "Broker/Consultant",75,10
              Credit001aListView001.InsertColumn using "Mailer PO",50,11
        CREDIT001aListView001.SetColumnFormat using 5,1              .set DAYS column justify right
              Setprop Credit001aListView001,Sortorder=2
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.
.Create Credit001aListView002 Columns by days
.Column Clicking
.begin patch 1.4
        Credit001aListView002.InsertColumn using "Pseudo Days",0,1
        Credit001aListView002.InsertColumn using "Days",50,2
        Credit001aListView002.InsertColumn using "LR",50,3
.START PATCH 1.7 REPLACED LOGIC
.        Credit001aListView002.InsertColumn using "Orderdate",75,4
.        Credit001aListView002.InsertColumn using "Status",80,5
.        Credit001aListView002.InsertColumn using "Maildate",75,6
.        Credit001aListView002.InsertColumn using "List",190,7
.        Credit001aListView002.InsertColumn using "Amount Due",75,8
.;begin patch 1.43
.        Credit001aListView002.InsertColumn using "Broker/Consultant",75,9
.;end patch 1.43
.        CREDIT001aListView002.SetColumnFormat using 1,1              .set DAYS column justify right
..............................................
.begin patch 1.83
.              Credit001aListView002.InsertColumn using "Broker GOP",70,4
.              Credit001aListView002.InsertColumn using "Orderdate",70,5
.              Credit001aListView002.InsertColumn using "Status",65,6
.              Credit001aListView002.InsertColumn using "Maildate",70,7
.              Credit001aListView002.InsertColumn using "List",175,8
.              Credit001aListView002.InsertColumn using "Amount Due",75,9
.              Credit001aListView002.InsertColumn using "Broker/Consultant",75,10
.
              Credit001aListView002.InsertColumn using "Broker GOP",70,4
              Credit001aListView002.InsertColumn using "NIN GOP",65,5
              Credit001aListView002.InsertColumn using "Orderdate",70,6
              Credit001aListView002.InsertColumn using "Status",65,7
              Credit001aListView002.InsertColumn using "Maildate",70,8
              Credit001aListView002.InsertColumn using "List",175,9
              Credit001aListView002.InsertColumn using "Amount Due",75,10
              Credit001aListView002.InsertColumn using "Broker/Consultant",75,11
              Credit001aListView002.InsertColumn using "Mailer PO",50,12
        CREDIT001aListView002.SetColumnFormat using 1,1              .set DAYS column justify right
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.end patch 1.4
.
.Create Credit001aListView003 Columns by list
.Column Clicking
.START PATCH 1.7 REPLACED LOGIC
.        Credit001aListView003.InsertColumn using "List",190,1
.        Credit001aListView003.InsertColumn using "LR",50,2
.        Credit001aListView003.InsertColumn using "Orderdate",75,3
.        Credit001aListView003.InsertColumn using "Status",80,4
.        Credit001aListView003.InsertColumn using "Days",50,5
.        Credit001aListView003.InsertColumn using "Maildate",75,6
.        Credit001aListView003.InsertColumn using "Amount Due",75,7
.;begin patch 1.43
.        Credit001aListView003.InsertColumn using "Broker/Consultant",75,8
.;end patch 1.43
.        CREDIT001aListView003.SetColumnFormat using 5,1              .set DAYS column justify right
................................................
.begin patch 1.83
.              Credit001aListView003.InsertColumn using "List",175,1
.              Credit001aListView003.InsertColumn using "LR",50,2
.              Credit001aListView003.InsertColumn using "Broker GOP",70,3
.              Credit001aListView003.InsertColumn using "Orderdate",70,4
.              Credit001aListView003.InsertColumn using "Status",65,5
.              Credit001aListView003.InsertColumn using "Days",50,6
.              Credit001aListView003.InsertColumn using "Maildate",70,7
.              Credit001aListView003.InsertColumn using "Amount Due",75,8
.              Credit001aListView003.InsertColumn using "Broker/Consultant",75,9
.              CREDIT001aListView003.SetColumnFormat using 5,1              .set DAYS column justify right
.              
              Credit001aListView003.InsertColumn using "List",175,1
              Credit001aListView003.InsertColumn using "LR",50,2
              Credit001aListView003.InsertColumn using "Broker GOP",70,3
              Credit001aListView003.InsertColumn using "NIN GOP",65,4
              Credit001aListView003.InsertColumn using "Orderdate",70,5
              Credit001aListView003.InsertColumn using "Status",65,6
              Credit001aListView003.InsertColumn using "Days",50,7
              Credit001aListView003.InsertColumn using "Maildate",70,8
              Credit001aListView003.InsertColumn using "Amount Due",75,9
              Credit001aListView003.InsertColumn using "Broker/Consultant",75,10
              Credit001aListView003.InsertColumn using "Mailer PO",50,11
              CREDIT001aListView003.SetColumnFormat using 6,1              .set DAYS column justify right
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.
        setitem Credit001ComboBox001,0,c1

.Set Error Message Stat Text Boxes
               call           SetCreditErrorMssgDefault
.........................................................................................................................
.begin patch 1.6
.Create/Activate Objects on OptionsOrd.plf
.Screen       1
              create          Options;OptionsMLROnly=60:80:40:255,"'Exclude non direct orders' Default = 'No'",zorder=100
.             create          Options;OptionsCancNo=80:100:40:300,"'Include Cnc/open in Search' Default = 'No'",zorder=100
.             create          Options;OptionsPassInit=100:120:40:305,"Skip Password Check at Program Execution",zorder=100
              listins         Options1Coll,OptionsScreenInit,OptionsMLROnly
              setitem         OptionsMLROnly,0,c0
              setprop         Options1Coll,visible=1        .always        start with the first tab visible

.Screen       5
.             create          Options;OptionsScreen5BrkFilter=80:100:40:250,"Filter Broker Report Records",zorder=100
.             create          Options;OptionsScreen5EMailOption=100:120:40:250,"Display EMail            Option Message",zorder=100
.             create          Options;OptionsScreen5FileDefault=120:140:40:250,"Use Default Notes File",zorder=100
.             listins         Options5Coll,OptionsScreen5BrkFilter,OptionsScreen5EMailOption,OptionsScreen5FileDefault

.Screen       6
.              create          Options;OptionsScreen6Init=80:100:40:250,"Open             Program        on Campaign Screen",zorder=100
.              listins Options6Coll,OptionsScreen6Init

.Screen       7
.              create          Options;OptionsScreen7Proj=80:100:40:250,"Allow Viewing of Projection Breakdown",zorder=100
.              listins Options7Coll,OptionsScreen7Proj
.Screen       8
.              create          Options;OptionsScreen8OrderSearch=80:100:40:250,"Include Order            File in        Searches",zorder=100
.              listins Options8Coll,OptionsScreen8OrderSearch

.Screen       9
.             create          Options;OptionsScreen9View=80:100:40:250,"Disable Viewing of Screen 9",zorder=100
.             listins         Options9Coll,OptionsScreen9View

.Screen       10
.             create          Options;OptionsScreen10View=80:100:40:250,"Disable Viewing of Screen 10",zorder=100
.             create          Options;OptionsScreen10Usage=100:120:40:250,"Automatically Calculate Usage/Orders",zorder=100
.             listins         Options10Coll,OptionsScreen10View,OptionsScreen10Usage

               call           openpref
.end patch 1.6
.begin patch 1.85
                    MOve      C3,Nordlock
                    move      c3,Ninvlock
                    move      c3,Ndatlock
.end patch 1.85
. .................................................................
               Deactivate     Credita
               Deactivate     Creditb
               move           c1 to n1
              call           CreditTabChange
               setfocus       credit001EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout
..............................................................................................................
.SetCreditDates - check for date goodies
SetCreditDates
.
        move    yes to dateokflag
        clear   mm
        clear   dd
        clear   yy
        clear   newdate1
        clear   newdate2
drew
        getitem Credit001ComboBox001,0,result
        move    result to datebranch
        getitem Credit001EditDate1,0,str10
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
               setFocus Credit001EditDate1
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
               setFocus Credit001EditDate1
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus Credit001EditDate1
                              goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                             setFocus Credit001EditDate1
                               goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   newdate1
        endif
        setitem Credit001EditDate1,0,newdate1
.
        clear   mm
        clear   dd
        clear   yy
        clear   str2
        clear   newdate2
        getitem Credit001EditDate2,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
               setFocus Credit001EditDate2
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
               setFocus Credit001EditDate2
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                                             setFocus Credit001EditDate2
                        goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                             setFocus Credit001EditDate2
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
        setitem Credit001EditDate2,0,NewDate2
        if      (Newdate1  <= b1 and Newdate2 <= b1)
        move    c0 to datebranch
        endif
        move    c0 to startdate
        move    c0 to enddate
        if      (newdate1 > b1)
        unpack  newdate1 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to startdate
        endif
        if      (newdate2 > b1)
        unpack  newdate2 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to enddate
        endif
dave142
.begin patch 1.42
        if     ((startdate > 0 or EndDate > 0)& DateBranch = 1)       .dates entered no date type
                                alert   caution,"Specify Date type!",result
                                setfocus  Credit001ComboBox001
                                goto BadDate
        endif

        return

BadDate
        move    No to dateokflag
        setprop Credit001GoButton,visible=1
        setprop Credit001Stop,visible=0
        return
.LoadCreditSearchList - sort out keys being used and go to it
LoadCreditSearchList
         setprop   Credit001aListView003,enabled=1,bgcolor=white
         setprop   Credit001aListView002,enabled=1,bgcolor=white
         setprop   Credit001aListView001,enabled=1,bgcolor=white
         setprop   Credit001aListView003,visible=0
         setprop   Credit001aListView002,visible=0
         setprop   Credit001aListView001,visible=1
               setfocus  Credit001aListView001
         call      ClearCreditSearchList
         MOVE      C2 TO NORDPATH
         MOVE      C1 TO NinvPATH
         clear     nordfld1
         clear     nordfld2
         clear     nordfld3
         clear     nordfld4
         clear     nordfld5
         if        (mnum > " " and mnum <> "")
         packkey   nordfld1 from akey1,mnum
         endif
         if        (brknum > " " and brknum <> "")
         packkey   nordfld4 from akey4,brknum
         endif
.begin patch 1.85
.Set up Icon to display while searching
.        moveaddr credit0001,AnimateWindow
.        move    C1,AnimateCurIcon
.        move    C4,AnimateFrames
.        move    C0,AnimateIconID
..position Icon to sit to left of FindIt button
.        move    AniH,H
.        move    AniV,V
..        call    ANIMATEIT
.end patch 1.85

.               call           nordaim
               call           nordlast
               if             not over
.begin patch 1.81
               add            c1 to numproc
               move           numproc to str9
               setitem        Credit001EditText003,0,str9
.end patch 1.81
.begin patch 1.86
               call   CheckMLDate       
.end patch 1.86
.list view goodies
.patch1.51
                call   getbrk
.patch1.51
                call   checkinv
.patch1.51 Commentout
.                call   getbrk
.End patch1.51 Commentout
                call      LRLoop
         endif
.begin patch 1.85
.                      destroy  AnimateIcon
.end patch 1.85
         return

LRloop
        loop
.                call    ANIMATEIT
                eventcheck
                              if      (CreditStopFlag = No)
                              call     ClearCreditSearchList
                        move     no to okformore
                              goto     Fini
                       endif
.                call          NordKG
                call          NordKGP
                if            not over
.begin patch 1.81
               add            c1 to numproc
               move           numproc to str9
               setitem        Credit001EditText003,0,str9
.end patch 1.81
                move          yes to okformore
.begin patch 1.86
               call   CheckMLDate       
.end patch 1.86

.patch1.51
                call   getbrk
.patch1.51
                call    checkinv
.patch1.51 Commentout
.                call   getbrk
.End patch1.51 Commentout
                else
                move    no to okformore
.begin patch 1.85
.                      destroy  AnimateIcon
.end patch 1.85
                      endif
                until  (okformore = NO)
        repeat
        clear    str5
fini
        move     opencount to str5
              setitem  credit001bEditText004,0,str5         .open count
.
        clear    str5
        move     Paidcount to str5
              setitem  credit001bEditText003,0,str5         .Paid count
.
        clear    str5
        move     longest to str5
              setitem  credit001bEditText005,0,str5         .Paid
.
        clear    str5
        move     longestO to str5
              setitem  credit001bEditText006,0,str5         .Open
.
        clear    str5
        move     Shortest to str5
              setitem  credit001bEditText007,0,str5         .Paid
.
.begin patch 1.3
              move     mask14 to str14
              edit     ARtotal to str14
              setitem  credit001bEditText030,0,str14         .Total AR
.
              move     mask14 to str14
              edit     ARNonLO to str14
              setitem  credit001bEditText029,0,str14         .Non List Management related AR
.end patch 1.3
.begin patch 1.84
              move     mask14 to str14
              edit     ARPtotal to str14
              setitem  credit001bEditText031,0,str14         .Total AR paid
.end patch 1.84
.
 
.
        clear    str5
        if       (opencount > 0)
        move     ShortestO to str5
        else
        move     c0 to str5
        endif
              setitem  credit001bEditText008,0,str5         .Open

.
        if       (paidcount > 0 & paiddays > 0)
        divide   paidcount into paiddays
        move     paiddays into str7
              setitem  credit001bEditText009,0,str7         .Paid average
        endif
.
        if       (opencount > 0 & opendays > 0)
        divide   opencount into opendays
        move     opendays into str7
              setitem  credit001bEditText010,0,str7         .open average
        endif
.
        clear    str5
        move     prepaycount to str5
              setitem  credit001bEditText011,0,str5         .Paid Prepay count
.
        clear    str5
        move     prepaycounto to str5
              setitem  credit001bEditText012,0,str5         .Open Prepay count
.
        clear    str5
        move     Guarcount to str5
              setitem  credit001bEditText017,0,str5         .Paid Guar count
.
        clear    str5
        move     GuarcountO to str5
              setitem  credit001bEditText015,0,str5         .Open Guar count
.
        clear    str5
        move     AdvLoPay to str5
              setitem  credit001bEditText018,0,str5         .Paid Guar count adv pay to LO
.
        clear    str5
        move     AdvLoPayO to str5
              setitem  credit001bEditText016,0,str5         .Open Guar count adv pay to LO
.........................................................
        move     opencountN to str5
              setitem  credit001bEditText028,0,str5         .open count
.
        clear    str5
        move     PaidcountN to str5
              setitem  credit001bEditText001,0,str5         .Paid count
.
        clear    str5
        move     longestN to str5
              setitem  credit001bEditText013,0,str5         .Paid
.
        clear    str5
        move     longestNO to str5
              setitem  credit001bEditText026,0,str5         .Open
.
        clear    str5
        move     ShortestN to str5
              setitem  credit001bEditText002,0,str5         .Paid
.
        clear    str5
        if       (opencountN > 0)
        move     ShortestNO to str5
        else
        move     c0 to str5
        endif
              setitem  credit001bEditText027,0,str5         .Open

.
        if       (paidcountN > 0 & paiddaysN > 0)
        divide   paidcountN into paiddaysN
        move     paiddaysN into str7
              setitem  credit001bEditText020,0,str7         .Paid average
        endif
.
        if       (opencountN > 0 & opendaysN > 0)
        divide   opencountN into opendaysN
        move     opendaysN into str7
              setitem  credit001bEditText023,0,str7         .open average
        endif
.
        clear    str5
        move     prepaycountN to str5
              setitem  credit001bEditText014,0,str5         .Paid Prepay count
.
        clear    str5
        move     prepaycountNo to str5
              setitem  credit001bEditText025,0,str5         .Open Prepay count
.
        clear    str5
        move     GuarcountN to str5
              setitem  credit001bEditText019,0,str5         .Paid Guar count
.
        clear    str5
        move     GuarcountNO to str5
              setitem  credit001bEditText022,0,str5         .Open Guar count
.
        clear    str5
        move     AdvLoPayN to str5
              setitem  credit001bEditText021,0,str5         .Paid Guar count adv pay to LO
.
        clear    str5
        move     AdvLoPayNO to str5
              setitem  credit001bEditText024,0,str5         .Open Guar count adv pay to LO
        return
.................................................................................................
checkinv
.begin patch 1.6
               call           checkMlrOnly
.end  patch  1.6

               if             (OSTAT = "X" OR OSTAT = "z" OR OSTAT = "x" or Ostat = "l")
               return
               endif
.
.
        move      no to ManFeeFlag
        rep       zfill in olnum
              if              (olnum = "014477" or olnum = "017889" or olnum = "009766" or olnum = "018710")            .List Management Related
              move            Yes to ManFeeFlag
              endif
              if              (olnum = "005051" or olnum = "016909" or olnum = "016898")            .List Management Related
              move            Yes to ManFeeFlag
              endif
        packkey    ninvfld from olrn
.        call       ANIMATEIT
        call       ninvkey
        return     if over
Datechk branch     datebranch of datechkx,Orderdate,MailDate,ReturnDate,InvoiceDate
        goto       datechkx                  .no select goto exit
.........................................................................................................................
.begin patch 1.6
CHeckMlrOnly
               Getitem         OptionsMLROnly,0,n5         ;include?

               If             (N5 = c0)                    ;take all
               return                                      ;continue filters
               Endif
.
              If             (Obrknum = "" or Obrknum = "    " or Obrknum = "0000")
              Return
              Else
              NoReturn                 ;Pop Stack
              Return                   ;Exit don't take this record
              endif
.
              Return
.end patch 1.6
.........................................................................................................................
..........................
.select by order date
Orderdate
        move        oodtem to mm
        move        oodted to dd
        move        oodtey to yy
        call        cvtjul
        goto        datechk1
Maildate
        move        omdtem to mm
        move        omdted to dd
        move        omdtey to yy
        call        cvtjul
        goto        datechk1
Returndate
        move        ortndtem to mm
        move        ortndted to dd
        move        ortndtey to yy
        call        cvtjul
        goto        datechk1
Invoicedate
        move        invdtem to mm
        move        invdted to dd
        move        invdtey to yy
        call        cvtjul
        goto        datechk1
datechk1
        if          (startdate > 0 and enddate > 0)
                    if       (startdate <= juldays and juldays <= enddate)
                    goto     datechkx
                    else
.begin patch 1.81
                              if             (numhit = c1 & startdate-juldays > 365)
                                             Move           no to okformore            ;if date range & we have had one hit & we are now
                              endif                                                     : at a record that is a year outside the date range - stop
.end patch 1.81
                    return
                    endif
        endif

        if          (startdate > 0 and juldays >= startdate)
        goto        datechkx
        endif

        if          (enddate > 0 and juldays <= enddate)
        goto        datechkx
        endif

        return
.
.datechkx - date check exit load the list views
Datechkx
.begin patch 1.81
              if              (numhit = c0)
              move            c1 to numhit
              endif
.end patch 1.81
.credit001alistview001
               Credit001aListView001.InsertItem giving IN9 using OLRN
.
.START PATCH 1.7 ADDED LOGIC
              if (OBRKGUAR = "1")
               move           "30 Day",str12
              elseif (OBRKGUAR = "2")
               move           "45 Day",str12
              elseif (OBRKGUAR = "3")
               move           "60 Day",str12
              else
               clear          str12
              endif
.              
              if (GUARcode = "1")
               move           "30 Day",str13
              elseif (GUARcode = "2")
               move           "45 Day",str13
              elseif (GUARcode = "3")
               move           "60 Day",str13
              elseif (GUARcode = "6")
               move           "Prepaid",str13
              elseif (GUARcode = "7")
               move           "Prepaid 30",str13
                    elseif (GUARcode = "8")
               move           "Prepaid 45",str13
              elseif (GUARcode = "9")
               move           "Prepaid 60",str13
               else
               clear          str13
              endif
              Credit001aListView001.SetItemText using IN9,str12,1
              clear     str55
.              pack      str55,str12,slash,str13
              Credit001aListView001.SetItemText using IN9,str13,2
.END PATCH 1.7 ADDED LOGIC
         clear     str10
         pack      str10 from oodtem,slash,oodted,slash,oodtec,oodtey
.START PATCH 1.7 REPLACED LOGIC
.         Credit001aListView001.SetItemText using IN9,str10,1
.begin patch 1.83
.         Credit001aListView001.SetItemText using IN9,str10,2
         Credit001aListView001.SetItemText using IN9,str10,3
.begin patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.
         clear     str10a
         if        (omdtem <> "00" and omdtem <> "  ")
         pack      str10a from omdtem,slash,omdted,slash,omdtec,omdtey
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         else
         pack      str10a from invdtem,slash,invdted,slash,invdtec,invdtey
         move      invdtem to mm
         move      invdted to dd
         move      invdtey to yy
         endif
         call      cvtjul
         move      juldays to holdmdate
        cmatch     "P" to statb
        if         equal
        unpack     mlrpayd into str2,yy,mm,dd
        else
        clock      date to today
        unpack     today into mm,str1,dd,str1,yy
        endif
        call      cvtjul
.
        sub       holdmdate from juldays
        move      c0 to days
......oct 5 2011
        if          (juldays >= c0)  
        move      juldays to days
          endif
......oct 5 2011
        move      days to Ndays
.
        move       c0 to n1
        move       guarcode to n1
        if         (statb = "P" and N1 > 0 and N1 <=9)
        add        c1 to Guarcount
        endif
        if         (statb = "O" and N1 > 0 and N1 <=9)
        add        c1 to GuarcountO
        endif
.
.begin patch 1.5
         MOVE      C1 TO n2
DETADJIN move      n2 to jstn
         clear     njstfld
         rep       zfill in jstn
          PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      chkpend IF OVER
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO n2
         GOTO      DETADJIN
         ENDIF
Gotadj
         if         (statb = "P" )
         add        c1 to Advlopay
         else
         add        c1 to Advlopayo
         endif
.end patch 1.5
.add another check for pending - in a control right now.
chkpend
        clear      prepayflag
.begin patch 1.8
.               FOR           AcdRecCount,"1","15"
.                              .MOve           NInvAcdRec(AcdRecCount).NinvAcdNumRec TO NinvAcdNum
.                              MOve           NInvAcdRec(AcdRecCount).NinvAcdCodeRec TO NinvAcdCode
.                              if             (ninvacdCode = "096")
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               FOR           AcdRecCount,"1","15"
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
               if             (NinvacdCode = "096")
               move       yes to prepayflag
               goto       prepayexit
                              break
                              endif
               repeat
.        match      "096" to addchg1
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg2
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg3
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg4
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg5
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg6
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg7
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg8
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg9
.        if         equal
.        move       yes to prepayflag
.        goto       prepayexit
.        endif
.        match      "096" to addchg10
.        if         equal
.        move       yes to prepayflag
.        endif
.end patch 1.8
prepayexit
.
.START PATCH 1.7 REPLACED LOGIC
.             Credit001aListView001.SetItemText using IN9,Days,3
.             Credit001aListView001.SetItemText using IN9,str10a,4
.        Credit001aListView001.SetItemText using IN9,o1des,5
.        Credit001aListView001.EnsureVisible GIVING n10 using IN9,0
.begin patch 1.83

.              Credit001aListView001.SetItemText using IN9,Days,4
.              Credit001aListView001.SetItemText using IN9,str10a,5
.        Credit001aListView001.SetItemText using IN9,o1des,6
              Credit001aListView001.SetItemText using IN9,Days,5
              Credit001aListView001.SetItemText using IN9,str10a,6
        Credit001aListView001.SetItemText using IN9,o1des,7
.end patch 1.83
        Credit001aListView001.EnsureVisible GIVING n10 using IN9,0
.END PATCH 1.7 REPLACED LOGIC
.begin patch 1.84
               match      "590252" to lrn
               call       debugger if equal
              call      wipecvars
              if            (CalcARflag = yes)
              move          yes to subppsw
              MOVE      OLON TO NOWNFLD
              CALL      NOWNKEY
              MOVE     YES TO SUBPPSW
              MOVE      NinvFLD to nmrgfld
              REP       ZFILL IN NMRGFLD
              move      c0 to nmrgrqty
              move      c0 to nmrgiqty
              move      c0 to nmrgnet
              CALL      NMRGKEY
.
              move      lrn to nshpfld
              call      nshpkey
              move      olnum to ndatfld
              MOVE      C1 TO NDATPATH
              call      ndatkey
.
              move     no to shipsw
              move     no to mrgsw
.
              MOVE      LRN TO NmrgFLD
.
              MOVE      LRN TO NshpFLD
              call      nmrgkey
              if        not over
              move     yes to mrgsw
              endif
              call      nshpkey
              if        not over
              move     yes to shipsw
              endif
.
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad

              call          compute
              MOVE          LRN TO NADJFLD
              call          nadjkey
              add           asrecadj to formar
.
              endif
.end patch 1.84

        if         (statb = "P")                    .paid invoice
        if         (prepayflag <> yes)               .not prepaid
                      add        c1 to paidcount
                if       (ManFeeFlag = No)            .NON List Management Related
                add        c1 to paidcountn
                endif

                if      (ndays > 900)
.                call    debug
                endif
                       if         (Ndays > Longest)
                       move       Ndays to longest
                              endif

                       if         (Ndays > LongestN & ManFeeFlag = No)
                       move       Ndays to longestN
                              endif

                              if         (Ndays < shortest & Ndays > 0)
                       move       Ndays to shortest
                              endif

                              if         (Ndays < shortestN & Ndays > 0 & ManFeeFlag = No)
                       move       Ndays to shortestN
                              endif

                       if         (Ndays > 0)
                              add        Ndays to PaidDays
                       endif

                       if         (Ndays > 0 & ManFeeFlag = No)
                              add        Ndays to PaidDaysN
                       endif
              else
              add       c1 to prepaycount
                            if        (ManFeeFlag = No)
                            add       c1 to prepaycountN
                            endif
              endif
                      add        FormAR to ARPTotal

.OPEN INVOICES Follow
        else                                      .open invoices
.begin patch 1.84
.begin patch 1.3
.               match      "590252" to lrn
.               call       debugger if equal
.              call      wipecvars
.              if            (CalcARflag = yes)
.              move          yes to subppsw
.              MOVE      OLON TO NOWNFLD
.              CALL      NOWNKEY
.              MOVE     YES TO SUBPPSW
.              MOVE      NinvFLD to nmrgfld
.              REP       ZFILL IN NMRGFLD
.              move      c0 to nmrgrqty
.              move      c0 to nmrgiqty
.              move      c0 to nmrgnet
.              CALL      NMRGKEY
..
.              move      lrn to nshpfld
.              call      nshpkey
.              move      olnum to ndatfld
.              MOVE      C1 TO NDATPATH
.              call      ndatkey
..
.              move     no to shipsw
.              move     no to mrgsw
..
.              MOVE      LRN TO NmrgFLD
..
.              MOVE      LRN TO NshpFLD
.              call      nmrgkey
.              if        not over
.              move     yes to mrgsw
.              endif
.              call      nshpkey
.              if        not over
.              move     yes to shipsw
.              endif
..
.               call           Ninvacdrecclear
.               CLEAR          NInvAcdfld
.               pack           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
.               call           NinvAcdTst
.               Call           NInvAcdRecLoad
.
.              call          compute
.              MOVE          LRN TO NADJFLD
.              call          nadjkey
.              add           asrecadj to formar
..
.              endif
..end patch 1.3
.end patch 1.84
        if         (prepayflag <> yes)
                      add        c1 to opencount
.begin patch 1.3
                      add        FormAR to ARTotal
.end patch 1.3
                            if        (ManFeeFlag = No)
                            add        c1 to opencountN
.begin patch 1.3
                            add        Formar to ARNonLO
.end patch 1.3
                            endif

                       if         (Ndays > LongestO)
                       move       Ndays to longestO
                       endif

                       if         (Ndays > LongestNO & ManFeeFlag = No)
                       move       Ndays to longestNO
                       endif

                              if         (Ndays < shortestO & Ndays > 0)
                       move       Ndays to shortestO
                       endif

                              if         (Ndays < shortestNO & Ndays > 0 & ManFeeFlag = No)
                       move       Ndays to shortestNO
                       endif

                       if         (Ndays > 0)
                       add        Ndays to OPENdays
                       endif

                       if         (Ndays > 0 & ManFeeFlag = No)
                       add        Ndays to OPENdaysN
                       endif
                else

                add     c1 to prepaycounto
.begin patch 1.3
                      add        FormAR to ARTotal
.end patch 1.3
                        if    (ManFeeFlag = No)
                              add    c1 to prepaycountNo
.begin patch 1.3
                            add        Formar to ARNonLO
.end patch 1.3
                        endif
                endif
        endif
.
        cmatch     "P" to statb
        if         equal
.START PATCH 1.7 REPLACED LOGIC
.             Credit001aListView001.SetItemText using IN9,"Paid",2
.begin patch 1.83
.              Credit001aListView001.SetItemText using IN9,"Paid",3
              Credit001aListView001.SetItemText using IN9,"Paid",4
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
        else
              call    debug
.START PATCH 1.7 REPLACED LOGIC
.             Credit001aListView001.SetItemText using IN9,"Open",2
.              move          mask14 to str14
.              edit          formar to str14
.              Credit001aListView001.SetItemText using IN9,str14,6
.begin patch 1.83
.               Credit001aListView001.SetItemText using IN9,"Open",3
               Credit001aListView001.SetItemText using IN9,"Open",4
               move           mask14 to str14
               edit           formar to str14
.               Credit001aListView001.SetItemText using IN9,str14,7
               Credit001aListView001.SetItemText using IN9,str14,8
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
        endif
.begin patch 1.43
.START PATCH 1.7 REPLACED LOGIC
.              Credit001aListView001.SetItemText using IN9,brcomp,7
.begin patch 1.83
               Credit001aListView001.SetItemText using IN9,brcomp,9
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.end patch 1.43
               Credit001aListView001.SetItemText using IN9,OMLRPON,10
................
.credit001alistview002
TESTDAVE
.begin patch 1.4
               move         days to Ndays             .create pseudo days for correct sort
               add          "10000" to Ndays
               move         ndays to str5
               move         days to Ndays             .restore variable

               Credit001aListView002.InsertItem giving IN9 using str5
               Credit001aListView002.SetItemText Using IN9,days,1
.
         clear     str10
         pack      str10 from oodtem,slash,oodted,slash,oodtec,oodtey
         Credit001aListView002.SetItemText using IN9,olrn,2
.
.START PATCH 1.7 REPLACED LOGIC
.         Credit001aListView002.SetItemText using IN9,str10,3
.;
.        cmatch     "P" to statb
.        if         equal
.             Credit001aListView002.SetItemText using IN9,"Paid",4
.        else
.             Credit001aListView002.SetItemText using IN9,"Open",4
.              move          mask14 to str14
.              edit          formar to str14
.              Credit001aListView002.SetItemText using IN9,str14,7
.        endif
.;
.             Credit001aListView002.SetItemText using IN9,str10a,5
.        Credit001aListView002.SetItemText using IN9,o1des,6
.;end patch 1.4
.        Credit001aListView002.EnsureVisible GIVING n10 using IN9,0
.;begin patch 1.43
.              Credit001aListView002.SetItemText using IN9,brcomp,8
.;end patch 1.43
.........................................
              Credit001aListView002.SetItemText using IN9,str12,3

.start patch 1.83
              Credit001aListView002.SetItemText using IN9,str13,4
.              Credit001aListView002.SetItemText using IN9,str10,4
              Credit001aListView002.SetItemText using IN9,str10,5
.
              cmatch     "P" to statb
              if         equal
.               Credit001aListView002.SetItemText using IN9,"Paid",5
               Credit001aListView002.SetItemText using IN9,"Paid",6
              else
.               Credit001aListView002.SetItemText using IN9,"Open",5
               Credit001aListView002.SetItemText using IN9,"Open",6
               move          mask14 to str14
               edit          formar to str14
.               Credit001aListView002.SetItemText using IN9,str14,8
               Credit001aListView002.SetItemText using IN9,str14,9
              endif
.              Credit001aListView002.SetItemText using IN9,str10a,6
.              Credit001aListView002.SetItemText using IN9,o1des,7
.              Credit001aListView002.EnsureVisible GIVING n10 using IN9,0
.              Credit001aListView002.SetItemText using IN9,brcomp,9
.
              Credit001aListView002.SetItemText using IN9,str10a,7
              Credit001aListView002.SetItemText using IN9,o1des,8
              Credit001aListView002.EnsureVisible GIVING n10 using IN9,0
              Credit001aListView002.SetItemText using IN9,brcomp,10
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
               Credit001aListView002.SetItemText using IN9,OMLRPON,11
................
.credit001alistview003
               Credit001aListView003.InsertItem Giving IN9 using o1des
.
         clear     str10
         pack      str10 from oodtem,slash,oodted,slash,oodtec,oodtey
         Credit001aListView003.SetItemText using IN9,olrn,1
.
.START PATCH 1.7 REPLACED LOGIC
.         Credit001aListView003.SetItemText using IN9,str10,2
.;
.        cmatch     "P" to statb
.        if         equal
.             Credit001aListView003.SetItemText using IN9,"Paid",3
.        else
.             Credit001aListView003.SetItemText using IN9,"Open",3
.              move          mask14 to str14
.              edit          formar to str14
.              Credit001aListView003.SetItemText using IN9,str14,6
.        endif
.;
.             Credit001aListView003.SetItemText using IN9,days,4
.             Credit001aListView003.SetItemText using IN9,str10a,5
.
.        Credit001aListView003.EnsureVisible GIVING n10 using IN9,0
.;begin patch 1.43
.              Credit001aListView003.SetItemText using IN9,brcomp,7
.;end patch 1.43
.........................................
              Credit001aListView003.SetItemText using IN9,str12,2
.;start patch 1.83
.              Credit001aListView003.SetItemText using IN9,str10,3
              Credit001aListView003.SetItemText using IN9,str13,3
              Credit001aListView003.SetItemText using IN9,str10,4
.
              cmatch          "P",statb
              if equal
.               Credit001aListView003.SetItemText using IN9,"Paid",4
               Credit001aListView003.SetItemText using IN9,"Paid",5
              else
.               Credit001aListView003.SetItemText using IN9,"Open",4
               Credit001aListView003.SetItemText using IN9,"Open",5
               move           mask14,str14
               edit           formar,str14
.               Credit001aListView003.SetItemText using IN9,str14,7
               Credit001aListView003.SetItemText using IN9,str14,8
              endif
.
.              Credit001aListView003.SetItemText using IN9,days,5
.              Credit001aListView003.SetItemText using IN9,str10a,6
.              Credit001aListView003.EnsureVisible GIVING n10 using IN9,0
.              Credit001aListView003.SetItemText using IN9,brcomp,8
.              
              Credit001aListView003.SetItemText using IN9,days,6
              Credit001aListView003.SetItemText using IN9,str10a,7
              Credit001aListView003.EnsureVisible GIVING n10 using IN9,0
              Credit001aListView003.SetItemText using IN9,brcomp,9

.END PATCH 1.7 REPLACED LOGIC
              Credit001aListView003.SetItemText using IN9,OMLRPON,10
.
        return
. .........................................................................
.
Timeout
        beep
        beep
        beep
        winshow
        stop

ClearCreditSearchList
.Clear ListView
        Credit001aListView001.DeleteAllItems Giving IN9
        Credit001aListView002.DeleteAllItems Giving IN9
        Credit001aListView003.DeleteAllItems Giving IN9

.
        move         c0 to longest
        move         "900" to shortest
        move         c0 to longesto
        move         "900" to shortesto
        move         c0 to opencount
        move         c0 to paidcount
        move         c0 to opendays
        move         c0 to paiddays
        move         c0 to ndays
        move         c0 to prepaycount
        move         c0 to prepaycounto
        move         c0 to Guarcount
        move         c0 to Guarcounto
        move         c0 to AdvLoPay
        move         c0 to AdvLoPayO
.begin patch 1.3
              move            c0 to ARTotal
              move          c0 to ArNonLO
.end patch 1.3
.
        move         c0 to longestN
        move         "900" to shortestN
        move         c0 to longestNo
        move         "900" to shortestNo
        move         c0 to opencountN
        move         c0 to paidcountN
        move         c0 to opendaysN
        move         c0 to paiddaysN
        move         c0 to prepaycountN
        move         c0 to prepaycountNo
        move         c0 to GuarcountN
        move         c0 to GuarcountNo
        move         c0 to AdvLoPayN
        move         c0 to AdvLoPayNO
.
        clear        str5
              setitem  credit001bEditText003,0,str5
              setitem  credit001bEditText004,0,str5
              setitem  credit001bEditText005,0,str5
              setitem  credit001bEditText006,0,str5
              setitem  credit001bEditText007,0,str5
              setitem  credit001bEditText008,0,str5
              setitem  credit001bEditText009,0,str5
              setitem  credit001bEditText010,0,str5
              setitem  credit001bEditText011,0,str5
              setitem  credit001bEditText012,0,str5
.             setitem  credit001bEditText013,0,str5
.             setitem  credit001bEditText014,0,str5
              setitem  credit001bEditText015,0,str5
              setitem  credit001bEditText016,0,str5
              setitem  credit001bEditText017,0,str5
              setitem  credit001bEditText018,0,str5
.
              setitem  credit001bEditText001,0,str5
              setitem  credit001bEditText002,0,str5
              setitem  credit001bEditText019,0,str5
              setitem  credit001bEditText020,0,str5
              setitem  credit001bEditText021,0,str5
              setitem  credit001bEditText022,0,str5
              setitem  credit001bEditText023,0,str5
              setitem  credit001bEditText024,0,str5
              setitem  credit001bEditText025,0,str5
              setitem  credit001bEditText026,0,str5
              setitem  credit001bEditText027,0,str5
              setitem  credit001bEditText028,0,str5
.begin patch 1.3
              setitem  credit001bEditText029,0,str5
              setitem  credit001bEditText030,0,str5
              setitem  credit001bEditText031,0,str5
              call     wipecvars
              sub      formar from formar
              clear    str14
.begin patch 1.3
        return
..........................................................................................................
getmailer
               getitem   credit001EditText001,0,str4
               call      trim using str4

               count     n2 in str4
               if        (n2 = c3)
               move      str4 to str3
               pack      str4 from c0,str3
               goto      buildmkey
               endif
               if        (n2 = c2)
               move      str4 to str2
               pack      str4 from c0,c0,str2
               goto      buildmkey
               endif
               if        (n2 = c1)
               move      str4 to str1
               pack      str4 from c0,c0,c0,str1
               goto      buildmkey
               endif
               if             (n2 = c0)
               setitem   credit001Mlrcomp,0," "
              return
              endif

buildmkey
               clear     mkey
               packkey   mkey from str4,z3
               clear     mnum
               call      nmlrkey
               if        not over
               setitem   credit001Mlrcomp,0,Mcomp
         else
               setitem   credit001Mlrcomp,0,"** Not Found ** "
               endif
         return
GetBroker
               getitem   credit001EditText002,0,str4

               call      trim using str4
               count     n2 in str4
               if        (n2 = c3)
               move      str4 to str3
               pack      str4 from c0,str3
               goto      buildbrkfld
               endif
               if        (n2 = c2)
               move      str4 to str2
               pack      str4 from c0,c0,str2
               goto      buildbrkfld
               endif
               if        (n2 = c1)
               move      str4 to str1
               pack      str4 from c0,c0,c0,str1
               goto      buildbrkfld
               endif
               If        (N2 = c0)
               setitem   credit001BRkcomp,0," "
               return
               endif

buildbrkfld
               clear     nbrkfld
               packkey   nbrkfld from str4,z3
               clear     brknum
               call      nbrkkey
               if        not over
               setitem   credit001BRkcomp,0,BRcomp
         else
               setitem   credit001BRkcomp,0,"**Error** "
               endif
         return

STOP
.         CLOSE     LOGFILE
                winshow
         STOP
.
SetCreditErrorMssgDefault
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
CreditSwitchToTwo
        if (TabNum <> C1)
                if (TabNum = C2)
                        move    C1,N1
                endif
                call    CreditTabClick
                move    C1,N1
                call    CreditTabChange
                setitem Credit001TabControl001,0,2
        endif
        return
.......................................................................................................................
.begin patch 1.43
CreditForceToOne
               setitem Credit001TabControl001,0,1
               Deactivate Credita
               Deactivate Creditb
               move        c1 to n1
              call        CreditTabChange

               return
.end patch 1.43

...........................................................................................
CreditTabClick
        IF (N1 = C1)
.begin patch 1.3
                getprop Credit001aListView001,visible=N9
                move    n9 to ListViewNum
.end patch 1.3
                Deactivate Credita
        else (N1 = C2 )
                Deactivate Creditb
.begin patch 1.3
        setprop Credit001aListView001,visible=ListViewNum
        call    CreditSortListView
.end patch 1.3
        Endif
        return

CreditTabChange
        IF (N1 = C1)
                move    C1,TabNum
                Activate Credita
                setfocus Credit001aListView001
.begin patch 1.3
                setprop Credit001aListView001,visible=ListViewNum
                call    CreditSortListView
.end patch 1.3
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        else (N1 = C2)
.begin patch 1.3
                getprop Credit001aListView001,visible=N9
                move    n9 to ListViewNum
.end patch 1.3
                move    C2,TabNum
                Activate Creditb
                setfocus Credit001bEditText003
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
CreditSortListView
.Dynamically sorts Different ListViews.
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop Credit001aListView001,visible=N9
        if (N9 = C1)    .crecit001aListView001 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch 1.3
.START PATCH 1.7 REPLACED LOGIC
.                If (result <> 3 and result <> 5)  .no click?
.start patch 1.83
.                .If (result <> 4 and result <> 6)  .no click?
                If (result <> 5 and result <> 7)  .no click?
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
                        setprop credit001aListView001,visible=1
                        setprop credit001aListView003,visible=0
                        setprop credit001aListView002,visible=0
                       Credit001aListView001.EnsureVisible using c1,0
                        setfocus Credit001alistview001
                endif
.end patch 1.3
.START PATCH 1.7 REPLACED LOGIC
.                if (result = 3)            .clicked on days
.start patch 1.83
.                if (result = 4)            .clicked on days
                if (result = 5)            .clicked on days
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
                        setprop credit001aListView001,visible=0
                        setprop credit001aListView003,visible=0
                        setprop credit001aListView002,visible=1
                       Credit001aListView002.EnsureVisible using c1,0
.                             Credit001aListView002.SetItemState Giving IN9 using 1,2,2
                        setfocus Credit001alistview002
.START PATCH 1.7 REPLACED LOGIC
.                Elseif (result = 5)  .clicked on list
.                Elseif (result = 6)  .clicked on list
.start patch 1.83
               Elseif (result = 7)  .clicked on list
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
                        setprop credit001aListView001,visible=0
                        setprop credit001aListView003,visible=1
                        setprop credit001aListView002,visible=0
                       Credit001aListView003.EnsureVisible using c1,0
.                             Credit001aListView003.SetItemState Giving IN9 using 1,2,2
                        setfocus Credit001alistview003
                endif
        else
                getprop Credit001aListView002,visible=N9
                if (N9 = C1)    .Credit001aListView002 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch 1.3
.begin patch - note we have a hidden pseudo day column that scews the count
.                        if (result <> 1 and result <> 5)
.START PATCH 1.7 REPLACED LOGIC
.                        if (result <> 2 and result <> 6)
.start patch 1.83
.                        if (result <> 2 and result <> 7)
                        if (result <> 2 and result <> 8)
.start patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.end patch
                                setprop Credit001aListView001,visible=0
                                setprop Credit001aListView002,visible=2
                                setprop Credit001aListView003,visible=0
                                Credit001aListView002.EnsureVisible using c1,0
                                setfocus Credit001alistview002
.begin patch
.                        Elseif (result = 1 )
                        Elseif (result = 2 )
.end patch
.end patch 1.3
                                setprop Credit001aListView001,visible=1
                                setprop Credit001aListView002,visible=0
                                setprop Credit001aListView003,visible=0
                 Credit001aListView001.EnsureVisible using c1,0
                                      setfocus Credit001alistview001
.begin patch 1.83
.                        Elseif (result = 5 )
.START PATCH 1.7 REPLACED LOGIC
.                        Elseif (result = 6 )
.start patch 1.83
.                        Elseif (result = 7 )
                        Elseif (result = 8 )
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
.end patch
                                setprop Credit001aListView001,visible=0
                                setprop Credit001aListView002,visible=0
                                setprop Credit001aListView003,visible=1
                                      Credit001aListView003.EnsureVisible using c1,0
                                      setfocus Credit001alistview003
                              endif
          else
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch 1.3
.START PATCH 1.7 REPLACED LOGIC
.                        if (result <> 1 and result <> 4 )
.start patch 1.83
.                         Elseif (result = 7 )

.                        if (result <> 1 and result <> 5 )
                        if (result <> 1 and result <> 6 )
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
                                setprop Credit001aListView001,visible=1
                                setprop Credit001aListView002,visible=0
                                setprop Credit001aListView003,visible=0
                                Credit001aListView003.EnsureVisible using c1,0
                                setfocus Credit001alistview003
.                        if (result = 1 )   .want lr order
                        Elseif (result = 1 )   .want lr order
.end patch 1.3
                                setprop Credit001aListView001,visible=1
                                setprop Credit001aListView002,visible=0
                                setprop Credit001aListView003,visible=0
                                      Credit001aListView001.EnsureVisible using c1,0
                                      setfocus Credit001alistview001
.START PATCH 1.7 REPLACED LOGIC
.                        Elseif (result = 4 )      .age order
.start patch 1.83
.                        Elseif (result = 5 )      .age order
                        Elseif (result = 6 )      .age order
.end patch 1.83
.END PATCH 1.7 REPLACED LOGIC
                                setprop Credit001aListView001,visible=0
                                setprop Credit001aListView002,visible=1
                                setprop Credit001aListView003,visible=0
                                      Credit001aListView002.EnsureVisible using c1,0
                                      setfocus Credit001alistview002
                              endif
                endif
        endif
        return
.begin patch 1.6
.==============================================================================
Optionsgo
               branch         result to SearchGo,OptionsGo1
OptionsGo1
              setprop         Options,visible=1
              return
OptionsWritePref
.Pull values from OptionsOrd.plf
.Screen       1
              getitem         OptionsMLROnly,0,N5
              move            N5,OptionsArray(1,1)
.Screen       5
.             getitem         OptionsScreen5BrkFilter,0,N1
.             move            N1,OptionsArray(5,1)
.Screen       6
.              getitem OptionsScreen6Init,0,N1
.              move            N1,OptionsArray(6,1)
.Screen       7
.             getitem         OptionsScreen7Proj,0,N1
.             move             N1,OptionsArray(7,1)
.Screen       8
.              getitem OptionsScreen8OrderSearch,0,N1
.              move            N1,OptionsArray(8,1)
.Screen9
.             getitem         OptionsScreen9View,0,N1
.             move            N1,OptionsArray(9,1)
.Screen10
.             getitem         OptionsScreen10View,0,N1
.             move            N1,OptionsArray(10,1)
.             getitem         OptionsScreen10Usage,0,N1
.             move            N1,OptionsArray(10,2)

.Write to the file
              pack            APIFileName,"c:\program files\nincal\Credit0001.pre",hexzero
              call            DeleteFile
              prep            preffile,"c:\program files\nincal\Credit0001.pre"
              move            C0,N9
              loop
               add            C1,N9
               move           C0,N8
               loop
                              add            C1,N8
                              write          preffile,seq;OptionsArray(IN9,N8)
                              until          over
                              until (N8 = OptionsArr2)
               repeat
               until (N9 = OptionsArr1)
              repeat
              close           preffile
              return
OptionsTabClick
              if (N2 = C1)
               setprop        Options1Coll,visible=0
              elseif (N2 = C2)
.               setprop Options2Coll,visible=0
              elseif (N2 = C3)
.               setprop Options3Coll,visible=0
              elseif (N2 = C4)
.               setprop Options4Coll,visible=0
              elseif (N2 = C5)
               setprop        Options5Coll,visible=0
              elseif (N2 = C6)
.               setprop Options6Coll,visible=0
              elseif (N2 = C7)
               setprop Options7Coll,visible=0
              elseif (N2 = C8)
.               setprop Options8Coll,visible=0
              elseif (N2 = C9)
               setprop        Options9Coll,visible=0
              elseif (N2 = C10)
               setprop        Options10Coll,visible=0
              endif
              return

OptionsTabChange
              if (N2 = C1)
               setprop        Options1Coll,visible=1
              elseif (N2 = C2)
.               setprop Options2Coll,visible=1
              elseif (N2 = C3)
.               setprop Options3Coll,visible=1
              elseif (N2 = C4)
.               setprop Options4Coll,visible=1
              elseif (N2 = C5)
               setprop        Options5Coll,visible=1
              elseif (N2 = C6)
.               setprop Options6Coll,visible=1
              elseif (N2 = C7)
               setprop Options7Coll,visible=1
              elseif (N2 = C8)
.               setprop Options8Coll,visible=1
              elseif (N2 = C9)
               setprop        Options9Coll,visible=1
              elseif (N2 = C10)
               setprop        Options10Coll,visible=1
              endif
              return
...........................................................................................................
.Open Preferences File
openpref
              pack            APIFileName,"c:\program files\nincal\Credit0001.pre",hexzero
              call            FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
.               trap           Preferror if IO
               open           preffile,"c:\program files\nincal\Credit0001.pre"
               move           C0,N9
               loop
                              add            C1,N9
                              move           C0,N8
                              loop
                                             add            C1,N8
                                             read           preffile,seq;OptionsArray(IN9,N8)
                                             until          over
                                             until (N8 = OptionsArr2)
                              repeat
                              until (N9 = OptionsArr1)
               repeat
               close          preffile
.               trapclr io
               move           C0,N9
.Screen1
               move           OptionsArray(1,1),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsMLROnly,0,N5
.
.              move           OptionsArray(1,2),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsFaxLONo,0,N5
.Screen       5
.              move           OptionsArray(5,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen5BrkFilter,0,N5
.
.Screen       6
.               move           OptionsArray(6,1),str5
.               call           Trim using str5
.               move           str5,N5
.               setitem OptionsScreen6Init,0,N5
.START PATCH 3.66 ADDED LOGIC
.Screen       7
.              move            OptionsArray(7,1),str5
.              call            Trim using str5
.              move            str5,N5
.Screen       8
.               move           OptionsArray(8,1),str5
.               call           Trim using str5
.               move           str5,N5
.               setitem OptionsScreen8OrderSearch,0,N5
.Screen9
.              move           OptionsArray(9,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen9View,0,N5
.Screen10
.              move           OptionsArray(10,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen10View,0,N5
.
.              move           OptionsArray(10,2),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen10Usage,0,N5
              endif
               return
.end patch 1.6
.......................................................................................................
FileGo
                winshow
                stop

colorerror
        noreturn
        move    C1,colorflag
        goto aftercolor
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
        prep    colorfile,"c:\program files\nincal\credit0001.col"
                      loop
                add     c1,n1
                write   colorfile,seq;colornum(n1)
                until (n1 =2)
        repeat
        close   colorfile
        return
.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
BackColor
        trap    ColorTrap if object
        create  BGC
        trapclr object
        setprop ColBack,bgcolor=BGC
        getitem BGC,1,Fred
        getitem BGC,2,Fgreen
        getitem BGC,3,Fblue
        pack    colornum(2),Fred,Fgreen,Fblue
        return

TextColor
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColText,fgcolor=FTC
        getitem FTC,1,Fred
        getitem FTC,2,Fgreen
        getitem FTC,3,Fblue
        pack    colornum(1),Fred,Fgreen,Fblue
        return
CreditXRefKeyPress
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
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem  Credit001EditText002,0,str4
        setitem  Credit001BRkcomp,0,str45
        setfocus Credit001EditText002
        return
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem Credit001EditText001,0,str4
        setitem Credit001Mlrcomp,0,str45
        setfocus Credit001EditText001
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.

debugger
              return
............................................................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return
............................................................................................................................
.
.begin patch 1.43
getbrk
               clear     nbrkfld
               packkey   nbrkfld from obrknum,z3
               clear     brknum
                clear     brcomp
               call      nbrkkey
                return
.end patch 1.43
.begin patch 3.7
CheckMLdate
              pack            NMLDFLD1,"01X",OLRN
        clear   str8
              pack            str8,"99999999"
              call            NMLDAIM
              loop
               until over
               if (NMLDDATE < str8)
                              move           NMLDDATE,str8
               endif
               call           NMLDKG
              repeat
              if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
              else
.Use current Mail Date
              endif
           Return
.end patch 3.7

.
              INCLUDE         NSHPIO.INC
.START PATCH 1.51 REPLACED LOGIC
.             INCLUDE         NMLRIO.INC
.             INCLUDE         NbrkIO.INC
              include         compIO.inc
              include         cntIO.inc
.END PATCH 1.51 REPLACED LOGIC
              INCLUDE         NORDIO.INC
              INCLUDE         NOWNIO.INC
.begin patch 1.8
              include         ninvio.inc
              include         Ninvacdio.inc
.             include         ninvio.inc
.end patch 1.8
.begin patch 1.3
              include       Nadjio.inc
.begin patch 1.8
.              include       compute.inc
              include       compute.inc
.end patch 1.8
              include       nmrgio.inc
              include       nacdio.inc
              include       ndat3io.inc
.end patch 1.3
.Following used only in order to load Search.plf
         include ncmpio.inc
         include   ndatio.inc
         include   nrtnio.inc
         include   searchio.inc      .contains logic for search.plf
.begin patch 1.5
         include   njstio.inc
.end patch 1.5
.begin patch 1.86  
           include    nmldio.inc
.end patch 1.86  
         INCLUDE   COMLOGIC.INC


........................................
. Program:      NCAL0001.PLS
. Function:     List Management Broker Call File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   November 29, 2000
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include ncaldd.inc
        include ncal2dd.inc
.START PATCH 1.13 REPLACED LOGIC
.        INCLUDE   NMLRDD.inc
.        include   nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.13 REPLACED LOGIC
        include ndatdd.inc
.Following add in order to include SEARCH.PLF
.        include Infodd.inc
        include nrtndd.inc
        include ncmpdd.inc
.START PATCH 1.1 ADDED LOGIC
          include   nowndd.inc
.END PATCH 1.1 ADDED LOGIC
OLRN    DIM     6
.End
.        include nxrfdd.inc
.begin patch 1.2
          include   Oslspern.inc
.end patch 1.2
        include nmdldd.inc
        include nusedd.inc
        include ncntdd.inc
        include npasdd.inc
        include nxrfdd.inc
        include winapi.inc


Release    INIT    "1.43"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release   init      "1.42"   DLH Allow dynamic resizing
.Reldate   Init      "2013 June 6"
.Release   init      "1.41"   DLH fix sales person display on detail click event 
.Reldate   Init      "2013 May 9"
.Release   init      "1.4"   DLH add excel report
.Reldate   Init      "11 July 2012"
.Release   init      "1.3"   DLH add follow up date
.Reldate   Init      "12 June 2012"
.Release   init      "1.2"   DLH add Oslspern, make more flexible so all sales can use
.Reldate   Init      "15 Dec 2009"
.Release init    "1.15"   DMB 07JUL2006 Bug Fix post mailer conversion where data verification was invalid due to corruption of compmlrflg
//field check from a previous broker read when checking the mailer flag..updated the broker verify subroutine as well as this may affect it as well.
//Release init    "1.14"   ASH 22NOV2004 MAILER CONVERSION - FINALLY INCREASED MAILER/BROKER VARS!!
.Release init    "1.13"   ASH 27MAY2004 MAILER CONVERSION
.Release init    "1.12"   DMB 11NOV2003  Added code to do company test read in place of old broker read
.release init    "1.11"   ASH 16SEP2002 APPLIED KLUDGEY PATCH TO CIRCUMVENT PROBLEM WHERE SOME LINES THAT HAD AN 'R' AS THE
.                                                 FINAL CHARACTER WAS BEING TRUNCATED.  OBVIOUSLY A PRINTER DRIVER PROBLEM, BUT THIS
.                                                 IS AN EASIER FIX THAN GOING THROUGH DRIVER/FONT CHANGES
.release init    "1.1"   ASH 18SEP2002 ADDED FUNCTIONALITY OF SEARCH.PLF TO SEARCH FOR OWNER - OBSOLETE FOR THIS APPLICATION!!!!!
.release init    "1.0"   ASH 29nov2000 ORIGINAL RELEASE

.Begin patch 1.4
.to find version of excel
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
.Formatting vars needed
AlignLeft     integer 4,"0xffffefdd"
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
xlColumnWidth variant
VT_R8a         EQU 5           .Double - 8 byte Real

.
range     dim       20
range2    dim       20
ExcelFlag FOrm      1
FileCheck FIle
trapcount form      4
.end patch 1.4
wdWindowStateMinimize integer 4,"0x00000002"
.EXTERNAL ROUTINES
CallLoadCombo external "NORDTEST;OrderLoadCombo"
CallDisplayMailer external "INFO;DisplayMailer"
CallDisplayBroker external "INFO;DisplayBroker"
CallDisplayList external "INFO;DisplayList"
CallLoadForm external "INFO;LoadForm"
.EXTERNAL ROUTINES FROM SPELLCHECK.PLC
CallSpellCheck external "SPELLCHECK;SpellCheck"
.
Timer   Timer
.Files
prfile  pfile
colorfile file
.tempfile file
tempfile2 file
line1   dim     75
.START PATCH 1.11 ADDED LOGIC
line2   dim     76
.END PATCH 1.11 ADDED LOGIC
.Flags
ExitFlag init   "Y"
ReturnFlag init "N"
NewFlag init    "N"
NewFlag2 init   "N"
ViewFlag form   "1"
AamFlag init    "N"
.osflag   form   1       .1=win 95,98, 2=NT
DteFlag form    9
HdrFlag form    "1"
StopFlag init   "N"
SpellFlag init  "N"     .Used to determine if SpellCheck API is up and running
RptFlag form    9       .Used to determine type of Report desired
.
.AAMKEY CRITERIA
AKey1   init    "01X"
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
area    dim     3
newnum  dim     13
olddate dim     10
.Length of records
hold    dim     814
hold2   dim     868
Str27     Dim       27
.key holds longest possible Aam
key     dim     45
.START PATCH 1.14 REPLACED LOGIC
.holdkey dim     10
.holdkey2 dim    15
holdkey dim     12
holdkey2 dim    17
.END PATCH 1.14 REPLACED LOGIC
Mod1hold form   9       .Holds enable status of Call1Modify when Print button is used
Mod2hold form   9       .Holds enable status of Call2Modify when Print button is used
.START PATCH 1.14 REPLACED LOGIC
.HoldMlr dim     4       .Holds Mlr Number for breaks in Call Report
.HoldList dim    4       .Holds List Number for breaks in Call Report
HoldMlr dim     6       .Holds Mlr Number for breaks in Call Report
HoldList dim    6       .Holds List Number for breaks in Call Report
HoldBRK  dim    6       .Holds Broker Number for breaks in Call Report
.END PATCH 1.14 REPLACED LOGIC
MaskCAL2COMMENT DIM     750     .Used to hold last saved CAL2COMMENT value
MaskCAL2DATE2   DIM     8       .Used to hold last saved CAL2DATE2 value
linenumber form 1
maxline form    "7"
page    form    9
Carr    init    0x7f
.hexeight integer 4,"4294967295"
.Produces alert box with 'Yes', 'No' buttons.  'Yes' button set as default
STYLE1  INTEGER 1,"0x000004"
.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
.
date    dim     8
.START PATCH 1.14 ADDED LOGIC
str6a     dim       6
str6b     dim       6
MlrName   dim       55
BrkName   dim       55
.END PATCH 1.14 ADDED LOGIC
.Objects
EditTextBoxes   EditText
StatTextBoxes   StatText
.begin patch 1.4
CheckBoxes      CheckBox (4)
.CheckBoxes      CheckBox (5)
.end patch 1.4
ComboBoxes      ComboBox
.Pointers to Objects
EditPtr EditText        ^
EditPtr1 EditText       ^
DimPtr  Dim             ^
DimPtr1 Dim             ^
DimPtr2 Dim             ^
FrmPtr  form            ^
StatPtr StatText        ^
.
ScrRight form   4
ScrBottom form  4
MouseForm form  10
FarRight form   4
FarBottom form  4
T1      form    4
L1      form    4

NoteText  Dim       4000
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch  submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search"
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
SData   init    ";&Broker;&List;&Mailer"

.Getinfo - NOT YET IMPLEMENTED!!!!!!!
.        getinfo system,infostring
.        bump    infostring,12
.        move    infostring,str4
.        bump    infostring,4
.        move    infostring,str5

.Set Vars used for About Box
        move    "ncal0001.PLS",Wprognme
        move    "LM Broker Call File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch    plform  Search
.opt     plform  Options
rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  ncal0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1
        formload rpt
.        formload opt
        formload srch

.Load OrderInfo
        call    CallLoadForm
.
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  ncal0001;mFile,FData
        create  ncal0001;mEdit,EData,mFile
        create  ncal0001;mOptions,OData,mEdit
        create  ncal0001;mHelp,HData,mOptions
.Create SubMenus
        create  ncal0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under these
        activate mOptions
        activate mHelp,HelpGo,result
.Activate SubMenus
        activate sSearch,SearchGo,result

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
        create  RED=*RED
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=11
.        create  font1,"Times New Roman",size=13,bold
.        create  font2,"Times New Roman",size=12
.        create  font3,"Helvetica",size=9
.        create  font4,"Arial",size=14,italic
        move    "B",progcode
        move    "N",PassFlag

.Load up Objects
.ListViews
        CallListView.InsertColumn using "List Name",80,1
        CallListView.InsertColumn using "Mailer Name",80,2
        CallListView.InsertColumn using "Broker Name",80,3
        CallListView.InsertColumn using "List",50,4
        CallListView.InsertColumn using "Mailer",50,5
        CallListView.InsertColumn using "Broker",50,6
        CallListView.InsertColumn using "Detail",50,7
.
        CallListView1.InsertColumn using "Mailer Name",80,1
        CallListView1.InsertColumn using "List Name",80,2
        CallListView1.InsertColumn using "Broker Name",80,3
        CallListView1.InsertColumn using "List",50,4
        CallListView1.InsertColumn using "Mailer",50,5
        CallListView1.InsertColumn using "Broker",50,6
        CallListView1.InsertColumn using "Detail",50,7
.
        CallListView2.InsertColumn using "Broker Name",80,1
        CallListView2.InsertColumn using "List Name",80,2
        CallListView2.InsertColumn using "Mailer Name",80,3
        CallListView2.InsertColumn using "List",50,4
        CallListView2.InsertColumn using "Mailer",50,5
        CallListView2.InsertColumn using "Broker",50,6
        CallListView2.InsertColumn using "Detail",50,7
........
        Call2ListView.InsertColumn using "",0,1                 .CALL DATE
        Call2ListView.InsertColumn using "Call Date",60,2
        Call2ListView.InsertColumn using "Print",40,3
        Call2ListView.InsertColumn using "Comment",300,4
        Call2ListView.InsertColumn using "Detail",50,5
..begin patch 1.4        
        Call2ListView.InsertColumn using "Call Back",60,6
..end patch 1.4        
.ComboBox
        move    C1,N2
        move    "  ",str45
.Must delete blank items entered to ensure adequate space
        deleteitem Call2ComboSender,0
        insertitem Call2ComboSender,N2,str45
        loop
                move    C1,NCNTPATH
                move    "Load-NCNTSEQ",Location
                call    NCNTSEQ
                until over
                if (CNTCNT = "1")
                        pack    str45,CNTNAME,B1,CNTNUM
                        add     C1,N2
                        insertitem Call2ComboSender,N2,str45
                endif
        repeat
        setitem Call2ComboSender,0,1
          deleteitem Call2ComboboxSales,c0
.START PATCH 3.3.6 ADDED LOGIC
.Allow for NO Salesperson - Service Bureaus do not have association!!
          insertitem Call2ComboBoxSales,9999,""
.END PATCH 3.3.6 ADDED LOGIC
.         for n2,"1","23"
          for n2,"1","36"
                    load      str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                              osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
                              move      n2 to str2
                    rep       zfill,str2
                    pack      str27,str25,str2
.                   call      trim using str27
                              insertitem Call2ComboBoxSales,9999,str27
          repeat




.Find out system information
.Begin patch 1.3
.        getinfo system,str6
.        unpack  str6 into str1,str1
.        move    C0,osflag
.        if (str1 = "3" or str1 = "4")
.                move    C1,osflag
.        elseif (str1 = "1" | str1 = "5")
.                move    C2,osflag
.        endif
          call      Getwinver
.end patch 1.3
.Main Loop
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.Set Error Message Stat Text Boxes
        call    CallSetErrorMssgDefault
.Set Report Default
        call    CallSetReportDefault
.Set Flags to Open NINBRK.DAT
        move    C0,NCALFLAG
        move    C0,NCAL2FLAG
.Load SpellCheck DLL
.        pack    APILibFileName,"\\nts0\c\library\include\spell\SPELMATE.DLL",hexzero
.        call    LoadLibrary
.        if (APIResult = C0 OR APIResult = hexeight)
.                alert   caution,"SPELMATE.DLL has not been loaded!!!",result
.        else
.SpellCheckInitProfile   Profile         Spelmate.dll:           Dll library
.                                        SpelMateInit:           Entry Point
.                                        Int1                    Return value
.
.
.                         WinApi SpellCheckInitProfile Giving APIResult
.                         if (APIResult >= 0)
.                                alert   caution,"SpellChecking Program was not loaded properly!",result
..                         else
.                                move    YES,SpellFlag
.                         endif
.                         move    YES,SpellFlag
.        endif
.Reset Screen
        call    CallDisableLower1
        call    CallDisableLower2
        setfocus CallSearchList
.begin patch 1.42
           EVENTREG  X, 17, XRESIZE
.end patch 1.42

.
        loop
                waitevent
.Start Patch #1.9 - timer logic
.                setitem timer,0,18000   .reset to 30 minutes
.START TESTER LOGIC
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
.END TESTER LOGIC
        repeat
...............................................................................
.   SpellCheck - Spell check an Edit Text box
...............................................................................
.   Notes    :
.
.              PLB Variable      PLB type Var.  Notes.
.              ===============   ============== ===============================
.   On entry : APIWindowHandle    - Int    4       Window handle of window to receive dialog box
.              APIFileName        - Dim    260     Path and file name to receive image(Zero term)
.              APIResult          - Int    4       Don't care
.   On exit  : APIWindowHandle    - Int    4       Unchanged
.              APIFileName        - Dim    260     Unchanged
.              APIResult          - Int    4       1 = Image acquired to File
.                                               0 = Image load failed

.SpellCheckProfile       Profile         Spelmate.dll:           Dll library
.                                        SpellEdit:              Entry Point
.                                        Int1:
.                                        Int4
.
.
.
.SpellCheck              WinApi SpellCheckProfile Giving APIResult Using APIFileHandle
..                        Call            GetLastError              Load the last error value
.                        RETURN
.
Timeout
        beep
        beep
        beep
        stop

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
FileGo2
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
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
SearchGo5
.CAMPAIGN
        move    C5,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.START PATCH 1.1 ADDED LOGIC
SearchGo6
.OWNER
        move    C6,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.END PATCH 1.1 ADDED LOGIC

SearchLoad
.START PATCH 1.1 ADDED LOGIC
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.END PATCH 1.1 ADDED LOGIC
SearchLoad1
.BROKER
.START PATCH 1.14 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45,str55
.        getprop Call1EditBroker,enabled=result
.        if (result = 1)
.                setitem Call1EditBroker,0,str4
.                setitem Call1EditBrkCnt,0,str3
.                setitem Call1StatBroker1,0,str45
.                setfocus Call1EditBroker
.        else
.                setitem CallSearchBroker,0,str4
.                setitem CallStatSearchBroker1,0,str45
.                setfocus CallSearchBroker
.        endif
.......................................................
        unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        getprop Call1EditBroker,enabled=result
        if (result = 1)
                setitem Call1EditBroker,0,str6
                setitem Call1EditBrkCnt,0,"000"
                setitem Call1StatBroker1,0,str45
                setfocus Call1EditBroker
        else
                setitem CallSearchBroker,0,str6
                setitem CallStatSearchBroker1,0,str45
                setfocus CallSearchBroker
        endif
.END PATCH 1.14 REPLACED LOGIC
        return
SearchLoad2
.LIST
        unpack  Srchstr,str6,str1,str35
        getprop Call1EditList,enabled=result
        if (result = 1)
                setitem Call1EditList,0,str6
                setitem Call1StatListName,0,str35
                setfocus Call1EditList
        else
                setitem CallSearchList,0,str6
                setitem CallStatSearchList1,0,str35
                setfocus CallSearchList
        endif
        return
SearchLoad3
.MAILER
.START PATCH 1.14 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
        unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
.END PATCH 1.14 REPLACED LOGIC
        getprop Call1EditMailer,enabled=result
        if (result = 1)
.START PATCH 1.14 REPLACED LOGIC
.                setitem Call1EditMailer,0,str4
                setitem Call1EditMailer,0,str6
.END PATCH 1.14 REPLACED LOGIC
                setitem Call1StatMailer1,0,str45
                setfocus Call1EditMailer
        else
.START PATCH 1.14 REPLACED LOGIC
.                setitem CallSearchMailer,0,str4
                setitem CallSearchMailer,0,str6
.END PATCH 1.14 REPLACED LOGIC
                setitem CallStatSearchMailer1,0,str45
                setfocus CallSearchMailer
        endif
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
.START PATCH 1.1 ADDED LOGIC
SearchLoad6
.OWNER - not an option with this program
        return
.END PATCH 1.1 ADDED LOGIC

CallSetErrorMssgDefault
.Set Default for Contact File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=0
        setprop ErrorMssgStat4,visible=0
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search For Notes:"
        setitem ErrorMssgStat2,0,"Enter List, Mailer or Broker Number"
        setitem ErrorMssgStat3,0,""
        setitem ErrorMssgStat4,0,""
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        return

CallSetReportDefault
        getprop ReportStatText1,left=N9,height=N10,width=N8,top=howmany
        sub     N10,howmany
        add     howmany,N10,result
        add     N9,N8
        create  Report;StatTextBoxes=howmany:result:N9:N8,"Report Type","'>MS Sans Serif'(10)"
        getprop ReportEditText1,left=N9,height=N10,width=N8,top=howmany
        sub     N10,howmany
        add     howmany,N10,result
        add     C10,howmany
        add     N9,N8
.        create  Report;ComboBoxes=howmany:result:N9:N8,"",";L)ist;)Mailer;B)roker;S)alesPerson"
        create  Report;ComboBoxes=howmany:result:N9:N8,"",";L)ist;)Mailer;B)roker;S)alesPerson;D)ate"
.
        setitem ReportStatText1,0,"List"
        setitem ReportStatText2,0,"From Date"
        setitem ReportStatText3,0,"To Date"
        setitem ReportStatText4,0,"Report Name"
        getprop ReportStatText5,left=N9
        setprop ReportStatText5,width=0
        setprop ReportEditText1,MaxChars=6,EditType=2,SelectAll=1
        setprop ReportEditText2,MaxChars=10,EditType=2,SelectAll=1
        setprop ReportEditText3,MaxChars=10,EditType=2,SelectAll=1
        setprop ReportEditText4,width=0,tabid=0
        getprop ReportEditText5,width=N8,left=N10
        sub     N9,N10
        add     N10,N8
        setprop ReportEditText5,left=N9,width=N8,MaxChars=45,EditType=5,SelectAll=1
        getprop ReportCheck1,left=N9,width=N10,height=N8,top=howmany
        add     howmany,N8,result
        add     N9,N10
        create  Report;CheckBoxes(1)=howmany:result:N9:N10,"Call Date",font="'>MS Sans Serif'(10)"
        getprop ReportCheck2,top=howmany
        add     howmany,N8,result
        create  Report;CheckBoxes(2)=howmany:result:N9:N10,"Record Date",font="'>MS Sans Serif'(10)"
        getprop ReportCheck3,top=howmany
        add     howmany,N8,result
.        create  Report;CheckBoxes(3)=howmany:result:N9:N10,"Select Printer",font="'>MS Sans Serif'(10)"
.        getprop ReportCheck4,top=howmany
.        add     howmany,N8,result
        create  Report;CheckBoxes(3)=howmany:result:N9:N10,"Print Preview",font="'>MS Sans Serif'(10)"
.begin patch 1.4
        getprop ReportCheck4,top=howmany
        add     howmany,N8,result
        create  Report;CheckBoxes(4)=howmany:result:N9:N10,"Send to Excel",font="'>MS Sans Serif'(10)"
        Setprop ReportCheck5,visible=0

.end patch 1.4
        setprop ReportCheck1,width=0,tabid=0
        setprop ReportCheck2,width=0,tabid=0
        setprop ReportCheck3,width=0,tabid=0
        setprop ReportCheck4,width=0,tabid=0
.begin patch 1.4
.        setprop ReportCheck5,width=0,tabid=0
.begin patch 1.4
        activate StatTextBoxes
        activate ComboBoxes,CallClickComboBox,result
        activate CheckBoxes(1),CallReportCheckClick1,result
        activate CheckBoxes(2),CallReportCheckClick2,result
        activate CheckBoxes(3),CallReportCheckClick3,result
        activate CheckBoxes(4),CallReportCheckClick4,result
.begin patch 1.4
.        activate CheckBoxes(5),CallReportCheckClick5,result
.begin patch 1.4
        return

CallClickComboBox
        if (result = 1) .List
                setitem ReportStatText1,0,"List"
        elseif (result = 2)            .Mailer
                setitem ReportStatText1,0,"Mailer"
        elseif (result = 3)            .broker
                setitem ReportStatText1,0,"Broker"
        elseif (result = 4)            .Salesperson
                setitem ReportStatText1,0,"Salesperson"
        else            .Date
                setitem ReportStatText1,0,"Date"
        endif
        return
CallReportCheckClick1
        call    ExclusiveCheckBox using CheckBoxes(1),CheckBoxes(2),CheckBoxes(2)
        return

CallReportCheckClick2
        call    ExclusiveCheckBox using CheckBoxes(2),CheckBoxes(1),CheckBoxes(1)
        return

CallReportCheckClick3
        call    ExclusiveCheckBox using CheckBoxes(3),CheckBoxes(4),CheckBoxes(4)
        return

CallReportCheckClick4
        call    ExclusiveCheckBox using CheckBoxes(4),CheckBoxes(3),CheckBoxes(3)
        return
.begin patch 1.4
.CallReportCheckClick5
.        call    ExclusiveCheckBox using CheckBoxes(5),CheckBoxes(4),CheckBoxes(4)
.        return
.begin patch 1.4

CallDisableUpper
        setprop CallSearchBroker,enabled=0,bgcolor=grey
        setprop CallSearchList,enabled=0,bgcolor=grey
        setprop CallSearchMailer,enabled=0,bgcolor=grey
        setprop CallListView,enabled=0,bgcolor=grey
        setprop CallOK,enabled=0
        setprop CallPrint,enabled=0
        return

CallEnableUpper
        setprop CallSearchBroker,enabled=1,bgcolor=white
        setprop CallSearchList,enabled=1,bgcolor=white
        setprop CallSearchMailer,enabled=1,bgcolor=white
        setprop CallListView,enabled=1,bgcolor=white
        setprop CallOK,enabled=1
        setprop CallPrint,enabled=1
        return

CallDisableLower1
        setprop Call1EditBroker,enabled=0,bgcolor=grey
        setprop Call1EditBrkCnt,enabled=0,bgcolor=grey
        setprop Call1EditDate,enabled=0,bgcolor=grey
        setprop Call1EditList,enabled=0,bgcolor=grey
        setprop Call1EditMailer,enabled=0,bgcolor=grey
        setprop Call2EditDate3,enabled=0,bgcolor=grey
        setprop Call1EditNotes,readonly=1
        setprop Call1Delete,enabled=0
        setprop Call1Modify,enabled=0
        setprop Call1New,enabled=0
        setprop Call1Quit,enabled=0
        setprop Call1Save,enabled=0
        return

CallEnableLower1
        setprop Call1EditBroker,enabled=1,bgcolor=white
        setprop Call1EditBrkCnt,enabled=1,bgcolor=white
        setprop Call1EditDate,bgcolor=white
        setprop Call2EditDate3,bgcolor=white
        setprop Call1EditList,enabled=1,bgcolor=white
        setprop Call1EditMailer,enabled=1,bgcolor=white
        setprop Call1EditNotes,readonly=0
        return

CallDisableLower1Buttons
        setprop Call1Delete,enabled=0
        setprop Call1Quit,enabled=0
        setprop Call1Save,enabled=0
CallDisableLower1Buttons2
        setprop Call1Modify,enabled=0
        setprop Call1New,enabled=0
        return

CallEnableLower1Buttons1
        setprop Call1Quit,enabled=1
        setprop Call1Save,enabled=1
        return
CallEnableLower1Buttons2
        CallListView.GetItemCount giving N9
        if (N9 > C0)
                setprop Call1Modify,enabled=1
        else
                setprop Call1Modify,enabled=0
        endif
        setprop Call1New,enabled=1
        return

CallDisableLower2
        setprop Call2CheckPrint,enabled=0
        setprop Call2ComboSender,enabled=0,bgcolor=grey
        setprop Call2ComboBoxSales,enabled=0,bgcolor=grey
        setprop Call2EditCall,enabled=0,bgcolor=grey
        setprop Call2EditCallDate,enabled=0,bgcolor=grey
        setprop Call2EditDate,enabled=0,bgcolor=grey
        setprop Call2EditNotes,readonly=1
        setprop Call2EditRecipient,enabled=0,bgcolor=grey
        setprop Call2Delete,enabled=0
        setprop Call2Modify,enabled=0
        setprop Call2New,enabled=0
        setprop Call2Quit,enabled=0
        setprop Call2Save,enabled=0
        setprop Call2EditDate3,enabled=0,bgcolor=grey
        return

CallEnableLower2
        setprop Call2CheckPrint,enabled=1
        setprop Call2ComboSender,enabled=1,bgcolor=white
        setprop Call2ComboBoxSales,enabled=1,bgcolor=white
        setprop Call2EditCall,bgcolor=white
        setprop Call2EditCallDate,enabled=1,bgcolor=white
        setprop Call2EditDate,bgcolor=white
        setprop Call2EditNotes,readonly=0
        setprop Call2EditRecipient,enabled=1,bgcolor=white
        setprop Call2EditDate3,enabled=1,bgcolor=white
.        setprop Call2ListView,enabled=1,bgcolor=white
        return

CallDisableLower2Buttons
.        setprop Call2Delete,enabled=0
        setprop Call2Quit,enabled=0
        setprop Call2Save,enabled=0
CallDisableLower2Buttons2
        setprop Call2Modify,enabled=0
        setprop Call2New,enabled=0
        return

CallEnableLower2Buttons1
        setprop Call2Quit,enabled=1
        setprop Call2Save,enabled=1
        return
CallEnableLower2Buttons2
        clear   str55
        Call2ListView.GetItemCount giving N9
        if (N9 > C0)
                move    N9,str9
                call    Trim using str9
                pack    str55,str9," Record(s) Found."
                setprop Call2Modify,enabled=1
                setprop Call2Delete,enabled=1
        else
                setprop Call2Modify,enabled=0
                setprop Call2Delete,enabled=0
        endif
        setitem Call2StatRecords,0,str55
        CallListView.GetItemCount giving N9
        if (N9 > C0)
                setprop Call2New,enabled=1
        else
                setprop Call2New,enabled=0
        endif
        setprop Call2ListView,enabled=1,bgcolor=white
        return

CallClearUpper
        setitem CallStatSearchBroker1,0,""
        setitem CallStatSearchList1,0,""
        setitem CallStatSearchMailer1,0,""
        return

CallClearLower1
        setitem Call1EditBroker,0,""
        setitem Call1EditBrkCnt,0,""
        setitem Call1EditDate,0,""
        setitem Call2EditDate3,0,""
        setitem Call1EditList,0,""
        setitem Call1EditMailer,0,""
        setitem Call1EditNotes,0,""
        setitem Call1StatBroker1,0,""
        setitem Call1StatListName,0,""
        setitem Call1StatMailer1,0,""
        setitem Call1StatRevision,0,""
        setitem Call1StatInits1,0,""
        return

CallClearLower2
        setitem Call2EditCall,0,""
        setitem Call2EditCallDate,0,""
        setitem Call2EditDate,0,""
        setitem Call2EditNotes,0,""
        setitem Call2EditRecipient,0,""
        setitem Call2StatRevision,0,""
        setitem Call2StatType2,0,""
        setitem Call2ComboSender,0,0
        setitem Call2ComboboxSales,0,0
        setitem Call2EditDate3,0,0
        
.        Call2ListView.DeleteAllItems giving N9
        return

CallLoadMailer LRoutine DimPtr,StatPtr
.START PATCH 1.14 REPLACED LOGIC
.        call    Trim using DimPtr
.        if (DimPtr = "")
.                clear   MCOMP
.        else
.                packkey MKEY,DimPtr,"000"
.                move    "LoadMlr-NMLRKEY",Location
.                pack    KeyLocation,"Key: ",MKEY
.                move    C1,NMLRPATH
.                call    NMLRKEY
.                if over
.                        clear   MCOMP
.                endif
.        endif
.        setitem StatPtr,0,MCOMP
.....................................
          call    Trim using DimPtr
          if (DimPtr = "")
                    clear   COMPCOMP
          else
                    pack      COMPFLD,DimPtr
                    call      ZFillIt using COMPFLD
                    move    "LoadMlr-COMPKEY",Location
                    pack    KeyLocation,"Key: ",COMPFLD
                    call    COMPKEY
                    if over
                              clear     COMPCOMP
                    elseif (COMPMLRFLG <> "T")
                              clear     COMPCOMP
                    endif
          endif
          setitem StatPtr,0,COMPCOMP
.END PATCH 1.14 REPLACED LOGIC
        return

CallLoadBroker LRoutine DimPtr,DimPtr1,StatPtr
.START PATCH 1.14 REPLACED LOGIC
.        call    Trim using DimPtr
.        if (DimPtr = "")
.                clear   BRCOMP
.        else
.                call    Trim using DimPtr1
.                if (DimPtr1 = "")
.                        pack    DimPtr1,"000"
.                endif
.                packkey NBRKFLD,DimPtr,DimPtr1
.                move    "LoadBrk-NBRKKEY",Location
.                pack    KeyLocation,"Key: ",NBRKFLD
.                move    C1,NBRKPATH
.                call    NBRKKEY
.                if over
.                        clear   BRCOMP
.                endif
.        endif
.        setitem StatPtr,0,BRCOMP
.....................................
          call    Trim using DimPtr
          if (DimPtr = "")
                    clear   COMPCOMP
          else
                    call    Trim using DimPtr1
                    if (DimPtr1 = "")
                              pack    DimPtr1,"000"
                    endif
                    packkey COMPFLD,DimPtr
                    move    "LoadBrk-COMPKEY",Location
                    pack    KeyLocation,"Key: ",COMPFLD
                    call    COMPKEY
                    if over
                              clear   COMPCOMP
                    elseif (COMPBRKFLG <> "T")
                              clear   COMPCOMP
                    endif
          endif
          setitem StatPtr,0,COMPCOMP
.END PATCH 1.14 REPLACED LOGIC
        return

CallLoadList LRoutine DimPtr,StatPtr
        call    Trim using DimPtr
        if (DimPtr = "")
                clear   OLSTNAME
        else
                packkey NDATFLD,DimPtr
                move    "LoadList-NDATKEY",Location
                pack    KeyLocation,"Key: ",NDATFLD
                move    C1,NDATPATH
                call    NDATKEY
                if over
                        clear   OLSTNAME
                endif
        endif
        setitem StatPtr,0,OLSTNAME
        return

CalcPseudoMouseForm LRoutine EditPtr
.Function used to give a psuedo Mouse_Down_Event in order to disply OrderInfo
.by hitting the F3 key while sitting on certain Edit Text Boxes.
        getprop EditPtr,top=T1,left=L1,height=N8,width=N9
        calc    MouseForm=((N9*10000)+N8)
        return

CallRefreshIndex
.START PATCH 1.14 REPLACED LOGIC
.        pack    NCALFLD,str6,str5
        pack    NCALFLD,str6,str6A
.END PATCH 1.14 REPLACED LOGIC
        move    C3,NCALLOCK
        move    "RefreshISAM-NCALKEY",Location
        pack    KeyLocation,"Key: ",NCALFLD
        move    C1,NCALPATH
        call    NCALKEY
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
                call    CallSetErrorMssgDefault
                setfocus CallSearchList
        else
.START PATCH 1.14 REPLACED LOGIC
.                if (str4 = "" | str4 = CALBRK)
                if (str6B = "" | str6B = CALBRK)
.END PATCH 1.14 REPLACED LOGIC
                        loop
                                call    CallLoadListView
                                move    "RefreshISAM-NCALKS",Location
                                pack    KeyLocation,"Key: ",NCALFLD
                                call    NCALKS
                                until over
.START PATCH 1.14 REPLACED LOGIC
.                                packkey str10,CALLIST,CALMLR
.                                until (NCALFLD <> str10)
                                packkey str12,CALLIST,CALMLR
                                until (NCALFLD <> str12)
.END PATCH 1.14 REPLACED LOGIC
                        repeat
                endif
        endif
        return

CallRefreshAamdex
        clear   NCALFLD1
        clear   NCALFLD2
        clear   NCALFLD3
        if (str6 <> "")
                pack    NCALFLD1,"01X",str6
        endif
.START PATCH 1.14 REPLACED LOGIC
.        if (str5 <> "")
.                pack    NCALFLD2,"02X",str5
.        endif
.        if (str4 <> "")
.                pack    NCALFLD3,"03L",str4
.        endif
..These need to be separate as LostFocus_CallSearchMailer uses str4
.        if (str6 <> "")
.                call    LostFocus_CallSearchList
.        endif
.        if (str5 <> "")
.                call    LostFocus_CallSearchMailer
.        endif
.        if (str4 <> "")
.                call    LostFocus_CallSearchBroker
.        endif
....................................
        if (str6A <> "")
                pack    NCALFLD2,"02X",str6A
        endif
        if (str6B <> "")
                pack    NCALFLD3,"03L",str6B
        endif
.These need to be separate as LostFocus_CallSearchMailer uses str6a
        if (str6 <> "")
                call    LostFocus_CallSearchList
        endif
        if (str6A <> "")
                call    LostFocus_CallSearchMailer
        endif
        if (str6B <> "")
                call    LostFocus_CallSearchBroker
        endif
.END PATCH 1.14 REPLACED LOGIC
        move    C3,NCALLOCK
        move    "RefreshAAM-NCALAIM",Location
        pack    KeyLocation,"Key: ",NCALFLD1,NCALFLD2,NCALFLD3
        call    NCALAIM
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
                call    CallSetErrorMssgDefault
                setfocus CallSearchList
        else
                setprop CallStop,enabled=1
                move    NO,StopFlag
                loop
                        call    CallLoadListView
                        move    "RefreshAAM-NCALKG",Location
                        pack    KeyLocation,"Key: ",NCALFLD1,NCALFLD2,NCALFLD3
                        call    NCALKG
                        until over
                        eventcheck
                        until (StopFlag = YES)
                repeat
        endif
        setprop CallStop,enabled=0
        call    CallSetItem using NCALFLD,C1
        clear   str55
        CallListView.GetItemCount giving N9
        if (N9 > 0)
                move    N9,str9
                call    Trim using str9
                pack    str55,str9," Record(s) Found."
        endif
        setitem Call1StatRecords,0,str55
        return

CallLoadListView
        call    CallLoadVars using CALLIST,CALMLR,CALBRK
.
        pack    hold,CALVARS
.
        CallListView.InsertItem giving N9 using OLSTNAME
.START PATCH 1.14 REPLACED LOGIC
.        CallListView.SetItemText using N9,MCOMP,1
.        CallListView.SetItemText using N9,BRCOMP,2
        CallListView.SetItemText using N9,MlrName,1
        CallListView.SetItemText using N9,BrkName,2
.END PATCH 1.14 REPLACED LOGIC
        CallListView.SetItemText using N9,CALLIST,3
        CallListView.SetItemText using N9,CALMLR,4
        CallListView.SetItemText using N9,CALBRK,5
        CallListView.SetItemText using N9,hold,6
.
.START PATCH 1.14 REPLACED LOGIC
.        CallListView1.InsertItem giving N9 using MCOMP
.        CallListView1.SetItemText using N9,OLSTNAME,1
.        CallListView1.SetItemText using N9,BRCOMP,2
        CallListView1.InsertItem giving N9 using MlrName
        CallListView1.SetItemText using N9,OLSTNAME,1
        CallListView1.SetItemText using N9,BrkName,2
.END PATCH 1.14 REPLACED LOGIC
        CallListView1.SetItemText using N9,CALLIST,3
        CallListView1.SetItemText using N9,CALMLR,4
        CallListView1.SetItemText using N9,CALBRK,5
        CallListView1.SetItemText using N9,hold,6
.
.START PATCH 1.14 REPLACED LOGIC
.        CallListView2.InsertItem giving N9 using BRCOMP
.        CallListView2.SetItemText using N9,OLSTNAME,1
.        CallListView2.SetItemText using N9,MCOMP,2
        CallListView2.InsertItem giving N9 using BrkName
        CallListView2.SetItemText using N9,OLSTNAME,1
        CallListView2.SetItemText using N9,MlrName,2
.END PATCH 1.14 REPLACED LOGIC
        CallListView2.SetItemText using N9,CALLIST,3
        CallListView2.SetItemText using N9,CALMLR,4
        CallListView2.SetItemText using N9,CALBRK,5
        CallListView2.SetItemText using N9,hold,6
        return

CallLoadVars LRoutine DimPtr,DimPtr1,DimPtr2
.DimPtr = List
.DimPtr1 = Mailer
.DimPtr2 = Broker/Broker Contact
        if (DimPtr <> "")
                pack    NDATFLD,DimPtr
                move    "CallLoadVars-NDATKEY",Location
                pack    KeyLocation,"Key: ",NDATFLD
                move    C1,NDATPATH
                call    NDATKEY
                if over
                        clear   OLSTNAME
                else
                        call    Trim using OLSTNAME
                endif
        endif
.
        if (DimPtr1 <> "")
.START PATCH 1.14 REPLACED LOGIC
.                pack    MKEY,DimPtr1,"000"
.                move    "CallLoadVars-NMLRKEY",Location
.                pack    KeyLocation,"Key: ",MKEY
.                move    C1,NMLRPATH
.                call    NMLRKEY
.                if over
.                        clear   MCOMP
.                else
.                        call    Trim using MCOMP
.                endif
.............................................
                pack    COMPFLD,DimPtr1
                move    "CallLoadVars-COMPKEY",Location
                pack    KeyLocation,"Key: ",COMPFLD
                call    COMPKEY
                if over
                        clear   MlrName
                    elseif (COMPMLRFLG <> "T")
                        clear   MlrName
                else
                              move      COMPCOMP,MlrName
                        call    Trim using MlrName
                endif
.END PATCH 1.14 REPLACED LOGIC
        endif
.
        if (DimPtr2 <> "")
.START PATCH 1.14 REPLACED LOGIC
.                pack    NBRKFLD,DimPtr2
.                move    "CallLoadVars-NBRKKEY",Location
.                pack    KeyLocation,"Key: ",NBRKFLD
.                move    C1,NBRKPATH
.                call    NBRKKEY
.                if over
.                        clear   BRCOMP
.                else
.                        call    Trim using BRCOMP
.                endif
.............................................
                pack    COMPFLD,DimPtr2
                move    "CallLoadVars2-COMPKEY",Location
                pack    KeyLocation,"Key: ",COMPFLD
                call    COMPKEY
                if over
                        clear   BrkName
                    elseif (COMPBRKFLG <> "T")
                        clear   BrkName
                else
                        move    COMPCOMP,BrkName
                        call    Trim using BrkName
                endif
.END PATCH 1.14 REPLACED LOGIC
        endif
        return

CallLoadScreens
        call    CallLoadVars using CALLIST,CALMLR,CALBRK
.
        setitem Call1EditList,0,CALLIST
        setitem Call1StatListName,0,OLSTNAME
.
        setitem Call1EditMailer,0,CALMLR
.START PATCH 1.14 REPLACED LOGIC
.        setitem Call1StatMailer1,0,MCOMP
        setitem Call1StatMailer1,0,MlrName
.END PATCH 1.14 REPLACED LOGIC
.
.START PATCH 1.14 REPLACED LOGIC
.        unpack  CALBRK,str4,str3
.        setitem Call1EditBroker,0,str4
.        setitem Call1EditBrkCnt,0,str3
.        setitem Call1StatBroker1,0,BRCOMP
        unpack  CALBRK,str6b,str3
        setitem Call1EditBroker,0,str6b
        setitem Call1EditBrkCnt,0,str3
        setitem Call1StatBroker1,0,BrkName
.END PATCH 1.14 REPLACED LOGIC
.
        clear   str10
        call    Trim using CALDATE
        if (CALDATE <> "")
                unpack  CALDATE,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        setitem Call1EditDate,0,str10
.begin patch 1.4
       clear   str10
        call    Trim using CAL2DATE3
        if (CAL2DATE3 <> "")
                unpack  CAL2DATE3,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        setitem Call2EditDate3,0,str10
.end patch 1.4

.
        clear   str10
        unpack  CALREVDATE,CC,YY,MM,DD
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        call    Trim using CALREVINITS
        clear   str55
        append  "Revised ",str55
        append  str10,str55
        append  " by ",str55
        append  CALREVINITS,str55
        reset   str55
        setitem Call1StatRevision,0,str55
.
        setitem Call1StatInits1,0,CALINITS
.
        call    Trim using CALCOMMENT
        setitem Call1EditNotes,0,CALCOMMENT
.Load Lower ListView Object
        call    CallClearLower2
        Call2ListView.DeleteAllItems giving N9
.
        clear   NCAL2FLD1
        clear   NCAL2FLD2
        call    Trim using CALLIST
        if (CALLIST <> "")
                pack    NCAL2FLD1,AKey1,CALLIST
        endif
        call    Trim using CALMLR
        if (CALMLR <> "")
                pack    NCAL2FLD2,"02X",CALMLR
        endif
        if (NCAL2FLD1 = "" & NCAL2FLD2 = "")
                goto CallLoadScreensEnd
        endif
        move    C3,NCALLOCK
        move    "LoadScreens-N2CALAIM",Location
        pack    KeyLocation,"Key: ",NCAL2FLD1,NCAL2FLD2
        move    C1,NCAL2PATH
        call    N2CALAIM
        if not over
                loop
                        call    CallLoadListView2
                        move    "LoadScreens-N2CALKG",Location
                        pack    KeyLocation,"Key: ",NCAL2FLD1,NCAL2FLD2
                        call    N2CALKG
                        until over
                repeat
           move      C2,N2               .Must start with first legitimate entry!!
          clear     str2
.patch1.09
          move      Cal2SALES to str2
          call      zfillit using str2
          move      str2 to n2
          call      debug
          loop
                    getitem   call2ComboboxSales,N2,str27
                    unpack    str27,str25,str2
                    if (str2 = "" |     str2 = " " | str2 = "  ")
                              move      C1,N2
                              break
                    endif
          until (str2 = Cal2SALES)
                    add       C1,N2
.Extra protection
                    if (N2 >= 98)
                              move      C1,N2
                              break
                    endif
          repeat
          setitem   call2ComboboxSales,0,N2
       endif
.
CallLoadScreensEnd
.        CallListView.EnsureVisible using 0,0
        call    CallSetItem2 using holdkey2
.        Call2ListView.SetItemState giving N9 using 0,2,2
        call    Click_Call2ListView
        call    CallEnableLower1Buttons2
        call    CallEnableLower2Buttons2
        setprop Call2New,default=1
        return

CallLoadListView2
        pack    hold2,CAL2VARS
.
.        Call2ListView.InsertItem giving N9 using CAL2NUM
        Call2ListView.InsertItem giving N9 using CAL2DATE2
        unpack  CAL2DATE2,CC,YY,MM,DD
        clear   str10
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        Call2ListView.SetItemText using N9,str10,1
        clear   str3
        if (CAL2PRINT = "1")
                move    "Yes",str3
        endif
        Call2ListView.SetItemText using N9,str3,2
        call    Trim using CAL2COMMENT
        Call2ListView.SetItemText using N9,CAL2COMMENT,3
        Call2ListView.SetItemText using N9,hold2,4
.
.begin patch 1.4
          unpack  CAL2DATE3,CC,YY,MM,DD
          clear   str10
          call    Trim using MM
          if (MM <> "")
                    pack    str10,MM,SLASH,DD,SLASH,CC,YY
          endif
          Call2ListView.SetItemText using N9,str10,5
.end patch 1.4

        return

CallLoadScreens2
        unpack  hold2,CAL2VARS
.
        if (hold2 = "")         .Trigger an over
                move    "(*&^)(",CAL2LIST
.START PATCH 1.14 REPLACED LOGIC
.                move    "(*&^",CAL2MLR
                move    "(*&^)(",CAL2MLR
.END PATCH 1.14 REPLACED LOGIC
        endif
.START PATCH 1.14 REPLACED LOGIC
.        clear   str7
.        call    CallLoadVars using CAL2LIST,CAL2MLR,str7
        clear   str9
        call    CallLoadVars using CAL2LIST,CAL2MLR,str9
.END PATCH 1.14 REPLACED LOGIC
        setitem Call2StatList,0,OLSTNAME
.START PATCH 1.14 REPLACED LOGIC
.        setitem Call2StatMailer,0,MCOMP
        setitem Call2StatMailer,0,MlrName
.END PATCH 1.14 REPLACED LOGIC
.
        setitem Call2EditCall,0,CAL2NUM
.
        clear   str10
        unpack  CAL2DATE2,CC,YY,MM,DD
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        setitem Call2EditCallDate,0,str10
.
        clear   str10
        unpack  CAL2DATE,CC,YY,MM,DD
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        setitem Call2EditDate,0,str10
.
        clear   str10
        unpack  CAL2REVDATE,CC,YY,MM,DD
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,str2,YY
        endif
        call    Trim using CAL2REVINITS
        clear   str55
        append  "Revised ",str55
        append  str10,str55
        append  " by ",str55
        append  CAL2REVINITS,str55
        reset   str55
        setitem Call2StatRevision,0,str55
.
        move    C0,N9
        move    CAL2PRINT,N9
        setitem Call2CheckPrint,0,N9
.
        setitem Call2StatType2,0,CAL2INITS
.
        call    CallLoadCombo using Call2ComboSender,CAL2CNT
.
        setitem Call2EditRecipient,0,CAL2CNT2
.
        call    Trim using CAL2COMMENT
        setitem Call2EditNotes,0,CAL2COMMENT

        clear   str10
        unpack  CAL2DATE3,CC,YY,MM,DD
        call    Trim using MM
        if (MM <> "")
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        endif
        setitem Call2EditDate3,0,str10
.patch1.09
          move      Cal2SALES to str2
          call      zfillit using str2
          move      str2 to n2
          call      debug
          loop
                    getitem   call2ComboboxSales,N2,str27
                    unpack    str27,str25,str2
                    if (str2 = "" |     str2 = " " | str2 = "  ")
                              move      C1,N2
                              break
                    endif
          until (str2 = Cal2SALES)
                    add       C1,N2
.Extra protection
                    if (N2 >= 98)
                              move      C1,N2
                              break
                    endif
          repeat
          setitem   call2ComboboxSales,0,N2


        return

CallStartNew LRoutine FrmPtr
        call    CallDisableLower1Buttons2
        call    CallDisableLower2Buttons
        call    CallDisableUpper
        setprop Call2ListView,enabled=0,bgcolor=grey
.
.        alert   Type=style1,"Do You Want to Retain Previous Values?",result
.        if (FrmPtr = C1)
.                if (result <> C6)
.                        call    CallClearLower1
.                endif
.                call    CallClearLower2
.        elseif (FrmPtr = C2 AND result <> C6)
.                call    CallClearLower2
.        endif
        if (FrmPtr = C1)
                call    CallClearLower1
                call    CallClearLower2
        elseif (FrmPtr = C2)
                call    CallClearLower2
        endif
.
        move    NO,ExitFlag
.
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        return

CallStartQuit
       move    YES,ExitFlag
       call    CallEnableUpper
.
       if (ViewFlag = 1)
                call    Click_CallListView
        elseif (ViewFlag = 2)
                call    Click_CallListView1
        elseif (ViewFlag = 3)
                call    Click_CallListView2
        endif
        return

.Verify Data Entry
CallVerifyData1
        getitem Call1EditList,0,str6
        call    Trim using str6
          call    ZFILLIT using str6,C0
.DH allow no list
.        if (str6 = "")
.                alert   caution,"List Number is a Required Field!",result
.                move    YES,ReturnFlag
.                setfocus Call1EditList
.                return
.        else
.                call    ZFILLIT using str6,C0
          if        (str6 <> "000000")
                packkey NDATFLD,str6
                move    "Verify1-NDATTST",Location
                pack    KeyLocation,"Key: ",NDATFLD
                move    C1,NDATPATH
                call    NDATTST
                if over
                        alert   caution,"Invalid List Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditList
                        return
                endif
        endif
        move    str6,CALLIST
.
.START PATCH 1.14 REPLACED LOGIC
.        getitem Call1EditMailer,0,str4
.        call    Trim using str4
.        if (str4 = "")
.                alert   caution,"Mailer Number is a Required Field!",result
.                move    YES,ReturnFlag
.                setfocus Call1EditMailer
.                return
.        else
.                call    ZFILLIT using str4,C0
.                packkey MKEY,str4,"000"
.                move    "Verify1-NMLRTST",Location
.                pack    KeyLocation,"Key: ",MKEY
.                move    C1,NMLRPATH
.                call    NMLRTST
.                if over
.                        alert   caution,"Invalid Mailer Number!",result
.                        move    YES,ReturnFlag
.                        setfocus Call1EditMailer
.                        return
.                endif
.        endif
.        move    str4,CALMLR
........................................
        getitem Call1EditMailer,0,str6
        call    Trim using str6
        call    ZFILLIT using str6,C0
.allow no mailer
.        if (str6 = "")
.                alert   caution,"Mailer Number is a Required Field!",result
.                move    YES,ReturnFlag
.                setfocus Call1EditMailer
.                return
.        else
.                call    ZFILLIT using str6,C0
          if        (str6 <> "000000")
                packkey COMPFLD,str6
                
//Patch 1.15 Comment Out code    
//                move    "Verify1-COMPTST",Location
//                pack    KeyLocation,"Key: ",COMPFLD
//                call    COMPTST
//Patch 1.15 Comment Out code Ends   
//Patch 1.15 New code for valid read 
                move    "Verify1-COMPKEY",Location
                pack    KeyLocation,"Key: ",COMPFLD
                call    COMPKEY
//Patch 1.15 New code for valid read Ends Right Herrrr
                if over
                        alert   caution,"Invalid Mailer Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditMailer
                        return
                    elseif (COMPMLRFLG <> "T")
                        alert   caution,"Invalid Mailer Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditMailer
                        return
                endif
        endif
        move    str6,CALMLR
.END PATCH 1.14 REPLACED LOGIC
.At this point we need to test for duplicate Accounts
        pack    NCALFLD,CALLIST,CALMLR
        move    C3,NCALLOCK
        move    "Verify1-NCALTST",Location
        pack    KeyLocation,"Key: ",NCALFLD
        move    C1,NCALPATH
        call    NCALTST
        if not over
                if (NewFlag = YES)
                        alert   caution,"This Account already exists!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditList
                        return
                endif
        endif
.
.START PATCH 1.14 REPLACED LOGIC
.        getitem Call1EditBroker,0,str4
.        call    Trim using str4
.        if (str4 = "")
.                clear   str3
.        else
.                call    ZFILLIT using str4,C0
.                getitem Call1EditBrkCnt,0,str3
.                call    Trim using str3
.                call    ZFILLIT using str3,C0
..PATCH1.12
.                packkey NBRKFLD,str4,str3
.                 pack    COMPFLD4 from nbrkfld
.                move    "Verify1-COMPTST2",Location
.                pack    KeyLocation,"Key: ",COMPFLD4
.                   CALL      COMPTST2
..                packkey NBRKFLD,str4,str3
..                move    "Verify1-NBRKTST",Location
..                pack    KeyLocation,"Key: ",NBRKFLD
..                move    C1,NBRKPATH
..                call    NBRKTST
..1.12
.                if over
.                        alert   caution,"Invalid Broker Number!",result
.                        move    YES,ReturnFlag
.                        setfocus Call1EditBroker
.                        return
.                endif
.        endif
.        pack    CALBRK,str4,str3
..........................................
        getitem Call1EditBroker,0,str6b
        call    Trim using str6b
        if (str6b = "")
                clear   str3
        else
                call    ZFILLIT using str6b,C0
                getitem Call1EditBrkCnt,0,str3
                call    Trim using str3
                call    ZFILLIT using str3,C0
.
                pack          COMPFLD,str6b
//Patch 1.15 Comment Out code                 
//                move    "Verify1B-COMPTST",Location
//                pack    KeyLocation,"Key: ",COMPFLD
//                  CALL      COMPTST
//Patch 1.15 Comment Out code Ends   
//Patch 1.15 New code for valid read 
                move    "Verify1B-COMPKEY",Location
                pack    KeyLocation,"Key: ",COMPFLD
                    CALL      COMPKEY
//Patch 1.15 New code for valid read Ends Right Herrrr                
                if over
                        alert   caution,"Invalid Broker Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditBroker
                        return
                    elseif (COMPBRKFLG <> "T")
                        alert   caution,"Invalid Broker Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditBroker
                        return
                endif
.
                    pack      CNCTFLD,str6b,str3
                move    "Verify1-CNCTTST",Location
                pack    KeyLocation,"Key: ",CNCTFLD
                    CALL      CNCTTST
                if over
                        alert   caution,"Invalid Broker Contact Number!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditBrkCnt
                        return
                    endif
        endif
        pack    CALBRK,str6b,str3
          if        (calbrk = "000000000" & calmlr = "000000" & callist = "000000")
                        alert   caution,"List or Mailer or Broker Number Required!",result
                        move    YES,ReturnFlag
                        setfocus Call1EditBroker
                         return
          endif                         
.END PATCH 1.14 REPLACED LOGIC
.Following will only really apply if we eventually allow modification of Record Date
.        getitem Call1EditDate,0,str10
.        call    TRIM using str10
.        count   N2,str10
.        if (N2 = 0)     .Should apply if OREUSE
.                alert   caution,"Return Date Must be in MMDDCCYY Format",result
.                move    YES,ReturnFlag
.                setfocus Call1EditDate
.                return
.        else
.                if (N2 = 10)
.                        unpack  str10,MM,str1,DD,str1,CC,YY
.                elseif (N2 = 8)
.                        unpack  str10,MM,DD,CC,YY
.                elseif (N2 <> 0)
.                        alert   caution,"Return Date Must be in MMDDCCYY Format",result
.                        move    YES,ReturnFlag
.                        setfocus Call1EditDate
.                        return
.                endif
.                move    MM,N2
.                if (N2 > "12")
.                        alert   caution,"Invalid Month!",result
.                        move    YES,ReturnFlag
.                        setfocus Call1EditDate
.                        return
.                else
.                        move    DD,N2
.                        if (N2 > "31")
.                                alert   caution,"Invalid Day!",result
.                                move    YES,ReturnFlag
.                                setfocus Call1EditDate
.                                return
.                        else
.                                move    CC,N2
.                                if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
.                                        alert   caution,"Invalid Year!",result
.                                        move    YES,ReturnFlag
.                                        setfocus Call1EditDate
.                                        return
.                                elseif (N2 = "19")
.                                        move    YY,N2
.                                        if (N2 < "80")
.                                                alert   caution,"Invalid Year!",result
.                                                move    YES,ReturnFlag
.                                                setfocus Call1EditDate
.                                                return
.                                        endif
.                                endif
.                        endif
.                endif
.        endif
.        pack    CALDATE,CC,YY,MM,DD
.....................................
        getitem Call1EditNotes,0,CALCOMMENT
        call    Trim using CALCOMMENT
        return

.Verify Data Entry
CallVerifyData2
.        getitem Call2EditCall,0,str5
.        call    Trim using str5
.        type    str5
.        if not equal
.                alert   caution,"Call Number is a Numeric Field!",result
.                move    YES,ReturnFlag
.                setfocus Call2EditCall
.                return
.        endif
.        move    C0,N5
.        move    str5,N5
.        move    N5,str5
.        call    ZFILLIT using str5,C0
.        move    str5,CAL2NUM
.
        getitem Call2EditCallDate,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 <> 0)
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                else
                        alert   caution,"Call Date Must be in MMDDCCYY Format",result
                        move    YES,ReturnFlag
                        setfocus Call2EditCallDate
                        return
                endif
                move    MM,N2
                if (N2 > "12")
                        alert   caution,"Invalid Month!",result
                        move    YES,ReturnFlag
                        setfocus Call2EditCallDate
                        return
                else
                        move    DD,N2
                        if (N2 > "31")
                                alert   caution,"Invalid Day!",result
                                move    YES,ReturnFlag
                                setfocus Call2EditCallDate
                                return
                        else
                                move    CC,N2
                                if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                        alert   caution,"Invalid Year!",result
                                        move    YES,ReturnFlag
                                        setfocus Call2EditCallDate
                                        return
                                elseif (N2 = "19")
                                        move    YY,N2
                                        if (N2 < "80")
                                                alert   caution,"Invalid Year!",result
                                                move    YES,ReturnFlag
                                                setfocus Call2EditCallDate
                                                return
                                        endif
                                endif
                        endif
                endif
                pack    CAL2DATE2,CC,YY,MM,DD
        else
                clear   CAL2DATE2
        endif

        getitem Call2EditDate3,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 <> 0)
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                else
                        alert   caution," Date Must be in MMDDCCYY Format",result
                        move    YES,ReturnFlag
                        setfocus Call2EditDate3
                        return
                endif
                move    MM,N2
                if (N2 > "12")
                        alert   caution,"Invalid Month!",result
                        move    YES,ReturnFlag
                        setfocus Call2EditDate3
                        return
                else
                        move    DD,N2
                        if (N2 > "31")
                                alert   caution,"Invalid Day!",result
                                move    YES,ReturnFlag
                                setfocus Call2EditDate3
                                return
                        else
                                move    CC,N2
                                if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                        alert   caution,"Invalid Year!",result
                                        move    YES,ReturnFlag
                                        setfocus Call2EditDate3
                                        return
                                elseif (N2 = "19")
                                        move    YY,N2
                                        if (N2 < "80")
                                                alert   caution,"Invalid Year!",result
                                                move    YES,ReturnFlag
                                                setfocus Call2EditDate3
                                                return
                                        endif
                                endif
                        endif
                endif
                pack    cal2date3,CC,YY,MM,DD
        else
                clear   cal2date3
        endif



.....................................
.Following will only really apply if we eventually allow modification of Record Date
.        getitem Call2EditDate,0,str10
.        call    TRIM using str10
.        count   N2,str10
.        if (N2 = 0)     .Should apply if OREUSE
.                alert   caution,"Record Date Must be in MMDDCCYY Format",result
.                move    YES,ReturnFlag
.                setfocus Call2EditDate
.                return
.        else
.                if (N2 = 10)
.                        unpack  str10,MM,str1,DD,str1,CC,YY
.                elseif (N2 = 8)
.                        unpack  str10,MM,DD,CC,YY
.                elseif (N2 <> 0)
.                        alert   caution,"Record Date Must be in MMDDCCYY Format",result
.                        move    YES,ReturnFlag
.                        setfocus Call2EditDate
.                        return
.                endif
.                move    MM,N2
.                if (N2 > "12")
.                        alert   caution,"Invalid Month!",result
.                        move    YES,ReturnFlag
.                        setfocus Call2EditDate
.                        return
.                else
.                        move    DD,N2
.                        if (N2 > "31")
.                                alert   caution,"Invalid Day!",result
.                                move    YES,ReturnFlag
.                                setfocus Call2EditDate
.                                return
.                        else
.                                move    CC,N2
.                                if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
.                                        alert   caution,"Invalid Year!",result
.                                        move    YES,ReturnFlag
.                                        setfocus Call2EditDate
.                                        return
.                                elseif (N2 = "19")
.                                        move    YY,N2
.                                        if (N2 < "80")
.                                                alert   caution,"Invalid Year!",result
.                                                move    YES,ReturnFlag
.                                                setfocus Call2EditDate
.                                                return
.                                        endif
.                                endif
.                        endif
.                endif
.        endif
.        pack    CAL2DATE,CC,YY,MM,DD
.....................................
.
        getitem Call2CheckPrint,0,N1
        move    N1,CAL2PRINT
.
        getitem Call2ComboSender,0,N2
        if (N2 <= 1)
                clear   CAL2CNT
        else
                getitem Call2ComboSender,N2,str45
                unpack  str45,str35,str1,CAL2CNT
        endif
          call      debug
                    clear     n2
          getitem   Call2ComboBoxSales,N1,N2   ;Internal Contact Information
          getitem   Call2ComboBoxSales,N2,str27          ;Internal Contact Information
          unpack    str27,str25,str2
          if (str2 = "" |     str2 = " " | str2 = "  ")               ;Did not pick a contact
                   alert     caution,"Need a Salesperson!!",result,"Salesperson Needed"
                   setfocus  Call2ComboBoxSales
                   return
                    endif
          move      str2,CAl2SALES

.
        getitem Call2EditRecipient,0,CAL2CNT2
        call    Trim using CAL2CNT2
.
        getitem Call2EditNotes,0,CAL2COMMENT
        call    Trim using CAL2COMMENT
.Make sure they didn't just enter in Carriage Returns
.Essentially code below is a version of Trim
        clear   str1    .Used to flag either blank entry, or carriage returns only
        if (CAL2COMMENT <> "")
                clear   N9
                clear   N8
                movelptr CAL2COMMENT,N9
                loop
                        cmatch  Carr,CAL2COMMENT
                        if not equal
                                move    YES,str1
                                goto CallVerifyData2End
                        endif
                        bump    CAL2COMMENT
                        movefptr CAL2COMMENT,N8
                        until   (N8 = N9)
                repeat
.must do one last compare
                cmatch  Carr,CAL2COMMENT
                if not equal
                        move    YES,str1
                endif
        endif
CallVerifyData2End
        if (str1 <> YES)
                alert   caution,"Notes are Required!",result
                move    YES,ReturnFlag
                setitem Call2EditNotes,0,""
                setfocus Call2EditNotes
                return
        endif
        return

..................................................................
......................GUI HOUSEKEEPING............................
..................................................................

CallSetMouseBusy
        setmode *mcursor=*wait
        return

CallSetMouseFree
        setmode *mcursor=*arrow
        return

CallSetItem LRoutine DimPtr,FrmPtr
        move    C0,N10
        move    C0,howmany
        move    C0,N8
        move    SEQ,result
        loop
                move    result,N9
                CallListView.GetNextItem giving result using C0,N9
                until (result = SEQ)
.START PATCH 1.14 REPLACED LOGIC
.                CallListView.GetItemText giving str10 using result,6
.                if (str10 = DimPtr)
                CallListView.GetItemText giving str12 using result,6
                if (str12 = DimPtr)
.END PATCH 1.14 REPLACED LOGIC
                        move    result,N10
                endif
        repeat
        move    SEQ,result
        loop
                move    result,N9
                CallListView1.GetNextItem giving result using C0,N9
                until (result = SEQ)
.START PATCH 1.14 REPLACED LOGIC
.                CallListView1.GetItemText giving str10 using result,6
.                if (str10 = DimPtr)
                CallListView1.GetItemText giving str12 using result,6
                if (str12 = DimPtr)
.END PATCH 1.14 REPLACED LOGIC
                        move    result,howmany
                endif
        repeat
        move    SEQ,result
        loop
                move    result,N9
                CallListView2.GetNextItem giving result using C0,N9
                until (result = SEQ)
.START PATCH 1.14 REPLACED LOGIC
.                CallListView2.GetItemText giving str10 using result,6
.                if (str10 = DimPtr)
                CallListView2.GetItemText giving str12 using result,6
                if (str12 = DimPtr)
.END PATCH 1.14 REPLACED LOGIC
                        move    result,N8
                endif
        repeat
        CallListView.SetItemState giving N9 using N10,2,2
        CallListView1.SetItemState giving N9 using howmany,2,2
        CallListView2.SetItemState giving N9 using N8,2,2
        CallListView.EnsureVisible using N10,0
        CallListView1.EnsureVisible using howmany,0
        CallListView2.EnsureVisible using N8,0
        if (FrmPtr <> C1)
                goto CallSetItemEnd
        endif
        if (ViewFlag = 1)
                call    Click_CallListView
        elseif (ViewFlag = 2)
                call    Click_CallListView1
        elseif (ViewFlag = 3)
                call    Click_CallListView2
        endif
CallSetItemEnd
        call    CallSetItem2 using holdkey2
        return

CallSetItem2 LRoutine DimPtr
        move    C0,N10
        call    Trim using DimPtr
.Return if Account did not have and detail records associated with it.
        if (DimPtr = "")
                goto CallSetItem2End
        endif
        move    SEQ,result
        loop
                move    result,N9
                Call2ListView.GetNextItem giving result using C0,N9
                until (result = SEQ)
.START PATCH 1.14 REPLACED LOGIC
.                Call2ListView.GetItemText giving str15 using result,C4
.                if (str15 = DimPtr)
                Call2ListView.GetItemText giving str17 using result,C4
                if (str17 = DimPtr)
.END PATCH 1.14 REPLACED LOGIC
                        move    result,N10
                endif
        repeat
.        if (N10 > C0)
CallSetItem2End
                Call2ListView.SetItemState giving N9 using N10,2,2
                Call2ListView.EnsureVisible using N10,0
                call    Click_Call2ListView
.        endif
        return

...........Reports............
CallReport1 LRoutine FrmPtr
        clear   NCAL2FLD1
        clear   NCAL2FLD2
.begin patch 1.4
        clear   NCAL2FLD3
        clear   NCAL2FLD4
.end patch 1.4
        if (str6 <> "")
                if (FrmPtr = C1)
                        pack    NCAL2FLD1,"01X",NDATFLD
.begin patch 1.4
.                else
                Elseif (FrmPtr = C2)
.end patch 1.4
.START PATCH 1.14 REPLACED LOGIC
.                        pack    NCAL2FLD2,"02X",MKEY
                        pack    NCAL2FLD2,"02X",COMPFLD
.END PATCH 1.14 REPLACED LOGIC
                    endif
.begin patch 1.4
 
        endif
          if        (FrmPtr = C5)
                    if        (DteFlag = c1)
                    call      Trim using str10
                    pack      NCAL2FLD3,str10
                    move      c3,ncal2path
                              else
                    call      Trim using str10                            
                    pack      NCAL2FLD4,str10
                    move      c4,ncal2path
                    endif
          Endif
        move    C3,NCAL2LOCK
.begin patch 1.4
        if      (FrmPtr = C1 or FrmPtr = C2)
        move    "Print-N2CALAIM",Location
        pack    KeyLocation,"Key: ",NCAL2FLD1
        call    N2CALAIM
        else
          move c0,NCAL2FLAG
        call    N2CALKey
          endif
.end patch 1.4
        if over
                if (FrmPtr = C1)
                          alert         caution,"There are no Accounts using that List!",result
.begin patch 1.4
                Elseif     (FrmPtr = C2)
                          alert         caution,"There are no Accounts using that Mailer!",result
                else
.                          alert         caution,"There are no Accounts using that Mailer!",result
                           Goto         PrepIt   
.end patch 1.4
                endif
                    goto CallPrintEnd
          else
PrepIT             clock     timestamp,timestamp
                    pack      str40,"C:\WORK\m",timestamp,".dat"
                    pack      str35,"C:\WORK\m",timestamp,".srt"
                    prepare   tempfile,str40,exclusive
          endif
          move    "Print-N2CALKG",Location
          loop
.begin patch 1.4
.                if (FrmPtr = C1)
.end patch 1.4
.START PATCH 1.14 REPLACED LOGIC
.                   pack    MKEY,CAL2MLR,"000"
.                        move    "Print-NMLRKEY",Location
.                        pack    KeyLocation,"Key: ",MKEY
.                        move    C1,NMLRPATH
.                        call    NMLRKEY
...................................................
                    pack    COMPFLD,CAL2MLR
                        move    "Print-COMPKEY",Location
                        pack    KeyLocation,"Key: ",COMPFLD
                        call    COMPKEY
.END PATCH 1.14 REPLACED LOGIC
.begin patch 1.4
.                else
.end patch 1.4
                    pack    NDATFLD,CAL2LIST
                        move    "Print-NDATKEY",Location
                        pack    KeyLocation,"Key: ",NDATFLD
                        move    C1,NDATPATH
                        call    NDATKEY
.START PATCH 1.14 REPLACED LOGIC
.                        pack    MCOMP,OLSTNAME,B55
.begin patch 1.4
.                        pack    COMPCOMP,OLSTNAME,B55
.end patch 1.4
.END PATCH 1.14 REPLACED LOGIC
.begin patch 1.4
.                endif
.end patch 1.4
.START PATCH 1.14 REPLACED LOGIC
.                write        tempfile,seq;MCOMP,CAL2VARS
.begin patch 1.4
.                write         tempfile,seq;COMPCOMP,CAL2VARS
                    call      Debug
                    Clear     Str8
                    if (DteFlag = C1)
                              move      CAL2DATE2,str8
                    else
                              move      CAL2DATE,str8
                    endif
                    call      Trim using str8
                    if (str8 = "" | (str8 >= str10 AND str8 <= str11))
                    write         tempfile,seq;COMPCOMP,CAL2VARS,Olstname
                    endif
.end patch 1.4
.END PATCH 1.14 REPLACED LOGIC
.begin patch 1.4
        if      (FrmPtr = C1 or FrmPtr = C2)
        move    "Print-N2CALKG",Location
        pack    KeyLocation,"Key: ",NCAL2FLD1
        call    N2CALKG
        else
        move    "Print-N2CALKs",Location
        pack    KeyLocation,"Key: KeyS"
        call    N2CALKS    
        endif
.end patch 1.4

                    until over
          repeat
          close     tempfile
          clear   taskname
.Sort by Mailer Name and then Call Number
.Following will not work as record "00001" can be deleted and a new record added will assume that
.number despite having an older record with a record # "00002".  Need to sort by date fields.
.         pack      taskname,str40,COMMA,str35,";1-45,56-60"
        if (DteFlag = 1)        .Call Date Sort
                 pack         taskname,str40,COMMA,str35,";1-45,69-76"
        else                    .Record Date Sort
          pack      taskname,str40,COMMA,str35,";1-45,61-68"
        endif
        sort    taskname
          open      tempfile,str35,read
          call      CallPrintOpenReport
          move      C0,page
          call      CallPrintHeader using FrmPtr
          clear     HoldMlr
        clear   HoldList

          loop
.START PATCH 1.14 REPLACED LOGIC
.                   read      tempfile,seq;MCOMP,CAL2VARS
.begin patch 1.4
.                    read      tempfile,seq;COMPCOMP,CAL2VARS
                    read      tempfile,seq;COMPCOMP,CAL2VARS,Olstname
.end patch 1.4
.END PATCH 1.14 REPLACED LOGIC
                    until over
                    call      CallPrintGetDetail using FrmPtr
          repeat
.begin patch 1.4
          if        (excelFlag = c1)
          Move      Howmany2,Str9
          call      trim using str9
          Pack      str12,"C",Str9         
          sheet.range("A5",str12).Columns.Autofit        
          Move      Howmany2,Str9
          call      trim using str9
          Pack      str12,"G",Str9         
          sheet.range("E5",str12).Columns.Autofit        
         create     xlColumnWidth,VarType=VT_R8a,VarValue="50.00"
         setprop         sheet.range("d1:d1").Columns,*ColumnWidth=xlColumnWidth
          call      trim using str9
          Pack      str12,"G",Str9         
          sheet.range("A5",str12).Rows.Autofit        

ExcelFileNameSelect
                              clear   taskname
                              if        (#ver = c1)
                              Move  "C:\WORK\CallRep.xlsx",taskname
                              else
                              move  "C:\WORK\CallRep.XLS",taskname
                              endif
                              erase     taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapExcelObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
                              destroy   OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
                              destroy sheet
                              destroy sheets
                              destroy book
                              destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                              setprop ex,*DisplayAlerts=OFALSE
                              destroy   OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
.Email new XLS to User
                              move    "Here is your call Report in Excel",MailSubjct
                              pack      MailBOdy,"Input File:  ",INPNAME
                              Pack      MailTO from User,"@nincal.com"
                              Pack      MailFrom from User,"@nincal.com"
                              if        (#ver = c1)
                              Pack      MailAttach from "C:\WORK\CallRep.xlsx"
                              else
                              Pack      MailAttach from "C:\WORK\CallRep.xls"
                              endif
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck
                             
                              call      SendMail

          Else
          prtclose prfile
.begin patch 1.4
          endif
.end patch 1.4

          close     tempfile
          pack      APIFileName,B55,B55,B55,B55,B55,B55
          pack      APIFileName,str35,HexZero
          call      DeleteFile
          pack      APIFileName,B55,B55,B55,B55,B55,B55
          pack      APIFileName,str40,HexZero
          call      DeleteFile
          return

CallPrintGetDetail LRoutine FrmPtr
          if (CAL2PRINT = "1")
                    if (DteFlag = C1)
                              move      CAL2DATE2,str8
                    else
                              move      CAL2DATE,str8
                    endif
                    call      Trim using str8
                    if (str8 = "" | (str8 >= str10 AND str8 <= str11))
.Create NEWPAGE while in the middle of a List
                              if (FrmPtr = C1 & HoldMlr <> CAL2MLR)
                                        add     sixlpi,row
                        elseif (FrmPtr = C2 & HoldList <> CAL2LIST)
                                        add     sixlpi,row
                              endif
                              if (row >= 7450)
.begin patch 1.4
                                        if (excelflag <> c1)
                                        prtpage prfile;*NEWPAGE;
                                        endif
.begin patch 1.4
                                        call      CallPrintHeader using FrmPtr
                                        add     sixlpi,row
                                       if ((FrmPtr = C1 & HoldMlr = CAL2MLR) | (FrmPtr = C2 & HoldList = CAL2LIST) | FrmPtr= c5)
.START PATCH 1.14 REPLACED LOGIC
.                                                 pack      taskname,MCOMP
                                                  pack      taskname,COMPCOMP
.END PATCH 1.14 REPLACED LOGIC
.begin patch 1.4
                                                  if        (excelflag = c1)
                                                  add       c1,howmany2
                                                  move      howmany2,str9
                                                  call      Trim using str9
                                                  pack      str12,"A",str9
                                                            if        (FrmPtr = c1)                 
                                                            setprop   sheet.Range(str12),*Value=CompComp
                                                            else
                                                            setprop   sheet.Range(str12),*Value=Olstname
                                                            endif
                                                  else
                                                  prtpage prfile;*pcolumn:row,taskname;
                                                  endif
                                      
.begin patch 1.4
                                        endif
                              endif
                              if (FrmPtr = C1 & HoldMlr <> CAL2MLR)
                                        move      CAL2MLR,HoldMlr
.START PATCH 1.14 REPLACED LOGIC
.                                       prtpage prfile;*pcolumn:row,MCOMP;
.begin patch 1.4
                                                  if        (excelflag = c1)
                                                  add       c1,howmany2
                                                  move      howmany2,str9
                                                  call      Trim using str9
                                                  pack      str12,"A",str9
                                                  setprop   sheet.Range(str12),*Value=CompComp
                                                  else
                                        prtpage prfile;*pcolumn:row,COMPCOMP;
                                                  endif
.end patch 1.4                                                  
.END PATCH 1.14 REPLACED LOGIC
                        elseif (FrmPtr = C2 & HoldList <> CAL2LIST)
                                        move      CAL2List,HoldList
.START PATCH 1.14 REPLACED LOGIC
.                                       prtpage prfile;*pcolumn:row,MCOMP;
.begin patch 1.4
                                                  if        (excelflag = c1)
                                                  add       c1,howmany2
                                                  move      howmany2,str9
                                                  call      Trim using str9
                                                  pack      str12,"A",str9
.                                                  setprop   sheet.Range(str12),*Value=CompComp
                                                  setprop   sheet.Range(str12),*Value=Olstname
                                                  else
                                        prtpage prfile;*pcolumn:row,COMPCOMP;
                                                  endif
                        elseif (FrmPtr = C5)
                                                  if        (excelflag = c1)
                                                  add       c1,howmany2
                                                  move      howmany2,str9
                                                  call      Trim using str9
                                                  pack      str12,"A",str9
                                                  setprop   sheet.Range(str12),*Value=CompComp
                                                  else
                                        prtpage prfile;*pcolumn:row,COMPCOMP;
                                                  endif

.end patch 1.4                                                  
.END PATCH 1.14 REPLACED LOGIC
                              endif
                              call      CallPrintDetail using FrmPtr
                    endif
          endif
          return

CallPrintDetail LRoutine FrmPtr
          unpack    str8,CC,YY,MM,DD
          call      Trim using MM
          if (MM <> "")
                    pack      str25,MM,SLASH,DD,SLASH,YY
          else
                    pack      str25,"No Date"
          endif
.begin patch 1.4
          if        (excelflag = c1)
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"B",str9
          setprop   sheet.Range(str12),*Value=str25
          else
          prtpage prfile;*pcolumn1:row,str25;
          endif
.end patch 1.4
          loop
                    cmatch    carr,CAL2COMMENT
                    if equal
                              move      C1,str1
                    else
                              move      C0,str1
                    endif
                       call    PARSITUP using line1,CAL2COMMENT,C0
                    until (line1 = "" and str1 = "0")
.Create NEWPAGE while in the middle of a Comment for a Mailer
.begin patch 1.4
          if        (excelflag <> c1)
                    if (row >= 7450 & line1 <> "")
                              prtpage prfile;*NEWPAGE;
                              call      CallPrintHeader using FrmPtr
.START PATCH 1.14 REPLACED LOGIC
.                             pack      taskname,MCOMP
                              pack      taskname,COMPCOMP
.END PATCH 1.14 REPLACED LOGIC
                              prtpage prfile;*pcolumn:row,taskname;
                              prtpage prfile;*pcolumn1:row,str25;
                    endif
          endif
.end patch 1.4          
                    if (line1 <> carr)
.START PATCH 1.11 REPLACED LOGIC
.                   prtpage prfile;*pcolumn2:row,line1;
                              pack      line2,line1,B1
.begin patch 1.4
                    if        (excelflag = c1)
                    append    line2,Notetext
.                    move      howmany2,str9
.                    call      Trim using str9
.                    pack      str12,"D",str9
.                    setprop   sheet.Range(str12),*Value=line2
.
.                    move      howmany2,str9
.                    call      Trim using str9
.                    pack      str12,"e",str9
.                    setprop   sheet.Range(str12),*Value=CAL2CNT2
.
.                    unpack    cal2date3,CC,YY,MM,DD
.                    call      Trim using MM
.                    if (MM <> "")
.                    pack      str25,MM,SLASH,DD,SLASH,YY
.                    else
.                    move      "No Date",str25
.                    endif
.                    move      howmany2,str9
.                    call      Trim using str9
.                    pack      str12,"F",str9
.                    setprop   sheet.Range(str12),*Value=Str25
.
.                    if        (frmptr = C5)
.                    move      howmany2,str9
.                    call      Trim using str9
.                    pack      str12,"G",str9
.                    setprop   sheet.Range(str12),*Value=Olstname
.                    ENDIF
.
.                    add       c1, howmany2
                    else
                    prtpage prfile;*pcolumn2:row,line2;
                    endif
.end patch 1.4                    
.END PATCH 1.11 REPLACED LOGIC
                endif
                    add     sixlpi,row
        repeat
.         add     sixlpi,row
.begin patch 1.4
                    if        (excelflag = c1)
                    Reset     NoteText
                    move      howmany2,str9
                    call      Trim using str9
                    pack      str12,"D",str9
                    move      howmany2,str9
                    setprop   sheet.Range(str12),*Value=NoteText,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
                    Clear     NoteTExt

                    call      Trim using str9
                    pack      str12,"e",str9
                    setprop   sheet.Range(str12),*Value=CAL2CNT2

                    unpack    cal2date3,CC,YY,MM,DD
                    call      Trim using MM
                    if (MM <> "")
                    pack      str25,MM,SLASH,DD,SLASH,YY
                    else
                    move      "No Date",str25
                    endif
                    move      howmany2,str9
                    call      Trim using str9
                    pack      str12,"F",str9
                    setprop   sheet.Range(str12),*Value=Str25

                    if        (frmptr = C5)
                    move      howmany2,str9
                    call      Trim using str9
                    pack      str12,"G",str9
                    setprop   sheet.Range(str12),*Value=Olstname
                    ENDIF

                    add       c1, howmany2
                    endif
          return

CallPrintOpenReport
.begin patch 1.4
.          getitem   CheckBoxes(5),0,N9
          getitem   CheckBoxes(4),0,N9
          if (N9 = C1)
          Move      c1,ExcelFlag
.do excel crud, get out
.Create the Variant objects
.Booleans
                    create  OTRUE,VarType=VT_BOOL,VarValue=1
                    create  OFALSE,VarType=VT_BOOL,VarValue=0
                    create  Zoom85,VarType=VT_I4,VarValue=1
.
                    create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
                    create  ex
          getprop ex,*SheetsInNewWorkbook=SheetsDefault
                  setprop ex,*SheetsInNewWorkbook=C1
.begin patch 1.43
.                    setprop ex,*WindowState=xlMinimized
.end patch 1.43
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.get exel version info

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
                    setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
                    sheet.range("A1:E1").Merge
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
          return
          endif
.          getitem   CheckBoxes(4),0,N9
.          if (N9 = C1)
.
.                    if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt
.                              PRTOPEN PrFile,"@\\NINs2\Laser3","FAXFILE.PRN"
.                    elseif (osflag = C3 | osflag = C4)
.                              PRTOPEN PrFile,"@Laser3","FAXFILE.PRN"
.                    else   .(osflag = c0)         .Don't know prompt for printer
.                              PRTOPEN PrFile,"@-","FAXFILE.PRN"
.                    endif
.
.          else
                getitem CheckBoxes(3),0,N8
                if (N8 = C1)
                              PRTOPEN prfile,"@?","FAXFILE.PRN"
                else
                    if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt
                                        PRTOPEN prfile,"","FAXFILE.PRN"
.                                        PRTOPEN prfile,"\\NINs2\Laser3","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag = c4)         .win 95 98
                                        PRTOPEN prfile,"","FAXFILE.PRN"
.                                        PRTOPEN prfile,"Laser3","FAXFILE.PRN"
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN prfile,"","FAXFILE.PRN"
.                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
                endif
.          endif
          return

CallPrintHeader LRoutine FrmPtr
        add     C1,page
.begin patch 1.4
          if        (excelflag <> c1)
.begin patch 1.4
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;
        move    "300",row
        prtpage prfile;*p4750:7800,*font=font2,page;
        prtpage prfile;*pcolumn:row,*font=font1,*boldon,str45;
          add     sixlpi,row
        prtpage prfile;*pcolumn:row,"RECOMMENDATION & RESULT REPORT",*boldoff;
        add     sixlpi,row
        add     sixlpi,row
        if (FrmPtr = C1)
                prtpage prfile;*pcolumn:row,*font=font2,*ulon,"Mailer";
        else
                prtpage prfile;*pcolumn:row,*font=font2,*ulon,"List";
        endif
          prtpage prfile;*pcolumn1:row,"Date";
          prtpage prfile;*pcolumn2:row,"Comments",*uloff;
.        add     sixlpi,row
          add     sixlpi,row
.begin patch 1.4
          Else
                    if        (page = c1)
.Header information
                    setprop   sheet.Range("A3"),*Value=Str45
                    setprop   sheet.range("A3").Font,*Bold="True"                               
                    setprop   sheet.Range("A4"),*Value="RECOMMENDATION & RESULT REPORT"
                    setprop   sheet.range("A4").Font,*Bold="True"                               
                              if (FrmPtr = C1)
                              setprop   sheet.Range("A5"),*Value="Mailer"
.begin patch 1.4
.                              else
                              Elseif (FrmPtr = C2)
                              setprop   sheet.Range("A5"),*Value="List"
                              Elseif (FrmPtr = C5)
                              setprop   sheet.Range("A5"),*Value="Mailer"
.end patch 1.4
                              endif

                    setprop   sheet.Range("B5"),*Value="Date"
                    setprop   sheet.Range("C5"),*Value="  "
                    setprop   sheet.Range("D5"),*Value="Comments"
                    setprop   sheet.Range("E5"),*Value="Broker "
                    setprop   sheet.Range("F5"),*Value="Follow Up "
                    if        (FrmPtr = c5)
                    setprop   sheet.Range("G5"),*Value="List Name "
                    endif
.Header Formatting
                    if        (FrmPtr = c5)
                    setprop   sheet.Range("A5:G5"),*HorizontalAlignment=xlAlignCenter
                    setprop   sheet.Range("A5:G5").Font,*Bold="True"
                    sheet.range("A5:G5").BorderAround using *LineStyle=1,*Weight=MedThick
                    Else
                    setprop   sheet.Range("A5:F5"),*HorizontalAlignment=xlAlignCenter
                    setprop   sheet.Range("A5:F5").Font,*Bold="True"
                    sheet.range("A5:F5").BorderAround using *LineStyle=1,*Weight=MedThick
                    endif
.
                    move      C6,HowMany2                      .starting row
                    
                    endif
          endif
.begin patch 1.4
          return
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Call Report - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Pack      MailTO from User,"@nincal.com"
                    Move      "dherric@nincal.com",MailCC
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

TrapExcelObject
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
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
        endif
.Send them back to select another File name and try to Save again.
        goto ExcelFileNameSelect
.begin patch 1.42
XRESIZE
           Ncal0001.Scale
           RETURN
.end patch 1.42



.Include IO file
        include ncalio.inc
        include ncal2io.inc
.START PATCH 1.13 REPLACED LOGIC
.        INCLUDE   NMLRIO.inc
.        include   nbrkIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.13 REPLACED LOGIC
        include ndatio.inc
.Following add in order to include SEARCH.PLF
.        include infoio.inc
        include nrtnio.inc
        include ncmpio.inc
.START PATCH 1.1 ADDED LOGIC
          include   nownio.inc
.END PATCH 1.1 ADDED LOGIC
.End
        include searchio.inc      .contains logic for search.plf
        include nxrfio.inc
        include nuseio.inc
        include nmdlio.inc
        include ncntio.inc
        include npasio.inc
        include comlogic.inc

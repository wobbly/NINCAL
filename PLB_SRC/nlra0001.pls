.......................................
. Program:      NLRA0001.PLS
. Function:     LRA Maintenance
. Author:       David Herrick
. Orig. Date:   2014 April 14
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include compdd.inc
        include cntdd.inc
        include npasdd.inc
        include NLRAdd.inc
        include NLRA2dd.inc
        include nusedd.inc
        include winapi.inc
.Following used only in order to load Search.plf
        include ndatdd.inc
        include nrtndd.inc
        include ncmpdd.inc
        include norddd.inc
          include   nowndd.inc
release   init      "1.10"   DLH display PDF in PLB Object Browser
reldate    init       "2014 November 19"
.release   init      "1.01"   DLH add sort function to Object LRAListView
.reldate    init       "2014 August 15"
.release   init      "1.00"   
.reldate   init       "2014 April 14"

.EXTERNAL ROUTINES FROM INFO.PLC
LRALoadForm external "INFO;LoadForm"
.Need to call these routines with Order prefix as they are called via NORD001F.PLF
OrderDisplayMessage external "INFO;DisplayMessage"
OrderInfoClose external "INFO;InfoClose"

.TESTVARS
TESTINT1        INTEGER 4
TESTINT2        INTEGER 4
str4b   dim     4
yesno1    integer   1,"0x000004"

Timer   Timer
.Files to open
prfile  pfile
preffile file
TabNum   form   1
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
NewFlag init    "N"
MlrFlag init    "N"
HoldFlag form   "1"
AKey1   init    "01X"
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

.Length of record plus space for B1 and space for "/"
hold    dim     62
key     dim     45
holdkey dim     7
holdList dim     6
str6a      dim        6

holdcnt dim     3
holdstat dim    1

.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
List    form    1
Inactive form   1
Preview form    1
Default form    1
Select  form    1

LRA2Mode   dim        1          .M=mod,N=New
LRApath init    "\\nins1\e\data\LRAS\"                              ."



.Objects used by Options.plf (a generic form)
OptionsGroupBox GroupBox
OptionsScannerCheckBox CheckBox
OptionsStatRes  StatText
OptionsStatX    StatText
OptionsEditX    EditText
OptionsStatY    StatText
OptionsEditY    EditText
OptionsStatType StatText
OptionsTypeComboBox ComboBox
OptionsStatDim  StatText
OptionsStatW    StatText
OptionsEditW    EditText
OptionsStatH    StatText
OptionsEditH    EditText
OptionsStatUnits StatText
OptionsUnitsComboBox ComboBox
OptionsStatScale StatText
OptionsEditScale EditText
OptionsStatThresh StatText
OptionsEditThresh EditText
OptionsCollection Collection
.Objects used by OptionsOrd.plf         (a generic form)
.OptionsArrayEx     dim       x(y,z)
.Following Array will allow:  y         rows - representing each Screen
.                                   z   columns   - representing # of preferances         for each Screen
.                                   x   length of each field in       each Screen
.
OptionsArr1 CONST   "2"       .Current Number of items used in Preference Screen
OptionsArr2 CONST   "5"
OptionsArrSize CONST          "5"
OptionsArray dim    OptionsArrSize(OptionsArr1,OptionsArr2)
.
.
colordim dim        8

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

.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
timestamp2 dim  16
HoldDir   dim       200
HoldFName dim       45
FileName1 dim       250
FileName2 dim       250

.Set Vars used for About Box
        move    "NLRA0001.PLS",Wprognme
        move    "LRA File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch    plform  Search
opt     plform  Options
rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
LRA2    plform  NLRA0002
x       plform  NLRA0001
        winhide

.Load Forms, Always load parent form first
        formload x
        formload LRA2
        formload abt
        formload pss
        formload mss1
        formload rpt
        formload opt
        formload srch

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT

.Create Menus
        create  NLRA0001;mFile,FData
        create  NLRA0001;mEdit,EData,mFile
        create  NLRA0001;mOptions,OData,mEdit
        create  NLRA0001;mHelp,HData,mOptions

.Create SubMenu
        create  NLRA0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
        activate mOptions,OptionsGo,result
.Only a SubMenu under this one
        activate mHelp,HelpGo,result

.Activate SubMenus
        activate sSearch,SearchGo,result

.Create Colors for EditText Inquiry
        create  white=*white
          create    grey=220:220:220
        create  RED=*RED
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic

        Deactivate      LRA2

.Open Preferences File
openpref
          pack      APIFileName,"c:\program files\nincal\NLRA0001.pre",hexzero
          call      FindFirstFile
          if (APIResult <> 0 & APIResult <> hexeight)
                    open      preffile,"c:\program files\nincal\NLRA0001.pre"
                    for N9,"1","2"
                              if (N9 = 1)
                                        read      preffile,seq;str1
                                        if over
                                                  break
                                        endif
                                        move      C0,N1
                                        move      str1,N1
                              endif
                    repeat
                    close     preffile
          endif
.Set up LRAListView columns
          LRAListView.InsertColumn using "Cmp ## ",50,0
          LRAListView.InsertColumn using "MLR Name",230,1
          LRAListView.InsertColumn using "Date ",75,2
          LRAListView.InsertColumn using "LRAVarS ",0,3
.Refresh Lower Screen to lighten up the grey color
          call      LRADisableLower
.Load OrderInfo
        call    LRALoadForm

.Main Loop
        move    "z",progcode
        move    "N",PassFlag
          call      GetWinVer
.
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.Get User Info
        move    C0,N3
        move    PORTN,N3
        if (N3 <  C1)
                clock   PORT,str3
                unpack  str3,str2,str1
                pack    str3,str1,str2
                move    str3,PORTN
        endif
        move    C0,NUSEFLD
        move    C1,NUSEPATH
        move    PORTN,NUSEFLD
        rep     zfill,NUSEFLD
        call    NUSEKEY
.Set Error Message Stat Text Boxes
        call    SetLRAErrorMssgDefault

        move    C0,COMPFLAG
        setfocus LRAEditList
          setmode   *GRAYSCALE=0

           EVENTREG  X, 17, XRESIZE

        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
        beep
        beep
        beep
        stop

LRASetScanner
        if (result = 1) .Box is checked
                setitem OptionsScannerCheckBox,0,0
                move    C0,result
        else
                setitem OptionsScannerCheckBox,0,1
                move    C1,result
        endif
LRASetScanner2
        if (result = 1) .Box is checked
                setprop OptionsCollection,enabled=0
        else
                setprop OptionsCollection,enabled=1
        endif
        return

SetLRAErrorMssgDefault
.Set Default for LRA File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"Enter 6 Digit List Number:"
        setitem ErrorMssgStat2,0,""
        setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
        setitem ErrorMssgStat4,0,"      By List Name"
        setitem ErrorMssgStat5,0,"      That List Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return

SetLRAReportScreen
.Set Default for Report Screen
        setitem ReportStatText1,0,"From Mailer :"
        setitem ReportStatText2,0,"To Mailer   :"
        setprop ReportStatText3,width=200
        setitem ReportStatText3,0,"Enter Dates As MMDDYYYY"
        setitem ReportStatText4,0,"From Date   :"
        setitem ReportStatText5,0,"To Date     :"
        setprop ReportEditText1,edittype=3,maxchars=4
        setitem ReportEditText1,0,""
        setprop ReportEditText2,edittype=3,maxchars=4
        setitem ReportEditText2,0,""
        setprop ReportEditText3,visible=0
        setprop ReportEditText4,edittype=3,maxchars=8
        setitem ReportEditText4,0,""
        setprop ReportEditText5,edittype=3,maxchars=8
        setitem ReportEditText5,0,""
        setitem ReportCheck1,0,"Long Listing"
        setitem ReportCheck1,0,0
        setitem ReportCheck2,0,"Exclude Inactives"
        setitem ReportCheck2,0,1
        setprop ReportCheck3,visible=0
        setprop ReportCheck4,visible=0
        setfocus ReportEditText1
        move    NO,RptCan
        return
LRAClearRec
.Clear all Text Fields
          LRAListView.DeleteAllItems giving result
        setitem LRAEditEnterDate,0,""
        setitem LRAEditMailer,0,""
        setitem LRAEditLRADate,0,""
        setitem LRAStatMlrName,0,""
        setitem LRAEditUser,0,""

LRAClearRec2
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
        RETURN
FileGo2
        RETURN
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
Optionsgo
        branch  result to Viewgo
ViewGo
        setprop Options,visible=1
        return
OptionsWritePref
.Pull values from Options.plf
.Screen   1
          return

EditGo
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
SearchLoad
.Called by SearchDataList_DoubleClick
.Only load if not in Inquiry mode
        getprop LRAEditMailer,enabled=N9
        if (N9 <> C1)
                return
        endif
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
.        unpack  Srchstr,str6a,str1,str3,str1,str45,str35,str10,str1,str6
        unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        setitem LRAEditMailer,0,str6
        setitem LRAStatMlrName,0,str45
        setfocus LRAEditMailer
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.
.Disable Upper Screen
LRADisableUpper
        setprop LRAOK,enabled=0
        setprop LRAExit,enabled=0
        setprop LRANew,enabled=0
        setprop LRAEditList,enabled=0,bgcolor=grey
          setprop LRAListView,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
LRAEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop LRAEditMailer,enabled=1
        setprop LRAOK,enabled=1
        setprop LRAExit,enabled=1
        setprop LRANew,enabled=1
        setprop LRASave,enabled=1
        setprop LRAQuit,enabled=1
        setprop LRAEditList,bgcolor=white
        setprop LRAListView,enabled=1,bgcolor=white
        return

.Disable Lower Screen
LRADisableLower
        setprop LRAEditEnterDate,enabled=0,bgcolor=grey
        setprop LRAEditLRADate,enabled=0,bgcolor=grey
        setprop LRAEditMailer,enabled=0,bgcolor=grey
        setprop LRAEditUser,enabled=0,bgcolor=grey
           setprop    LRAEditLRANum,enabled=0
           setprop    LRAEditLRANum,bgcolor=grey
           setprop    LRAComboLRADesc,enabled=0
           setprop    LRAComboLRADesc,bgcolor=grey
        setprop LRAQuit,enabled=0
        setprop LRASave,enabled=0
        setprop LRAView,enabled=0
        setprop LRADelete,visible=0
          setprop LRA2ButtonQuitDesc,enabled=0
          setprop LRA2ButtonSaveDesc,enabled=0
           setprop    LRA2EditLRANum,enabled=0
           setprop    LRA2EditLRANum,bgcolor=grey
           setprop    LRA2EditLRADESC,enabled=0
           setprop    LRA2EditLRADESC,bgcolor=grey
        return

.Enable Lower Screen
LRAEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
        setprop LRAEditEnterDate,bgcolor=white
        setprop LRAEditMailer,bgcolor=white
        setprop LRAEditLRADate,enabled=1,bgcolor=white
        setprop LRAEditUser,enabled=1,bgcolor=white
           setprop    LRAEditLRANum,enabled=1
           setprop    LRAEditLRANum,bgcolor=white
           setprop    LRAComboLRADesc,enabled=1
           setprop    LRAComboLRADesc,bgcolor=white
        setprop LRASave,enabled=1
        setprop LRAQuit,enabled=1
           setprop    LRA2EditLRANum,enabled=1
           setprop    LRA2EditLRANum,bgcolor=white
           setprop    LRA2EditLRADESC,enabled=1
           setprop    LRA2EditLRADESC,bgcolor=white
          setprop LRA2ButtonQuitDesc,enabled=1
          setprop LRA2ButtonSaveDesc,enabled=1
           return

.Disable Upper Screen
LRADisableView
        setprop LRAView,enabled=0
        return

LRALoadList
.Enable Modify button
        setprop LRAModify,enabled=1
        setitem LRAStatListName,0,Mlstname
          LRAListView.DeleteAllItems giving result      .Prepare to refresh it
          pack      NLRAFLD,str6
          move    "S.LoadLRAs-NLRAISAM",Location
          pack    KeyLocation,"Key: ",NLRAFLD
           move       c1,nlrapath
          call    NLRAKey
        loop
                until over
                    pack      taskname,NLRAVARS
.call compkey and get mailer name
                    LRAListView.InsertItem giving N9 using NLRAmailer
                      pack    COMPFLD,NLRAMailer
                     move    "S.VerifyData-COMPKEY",Location
                      pack    KeyLocation,"Key: ",COMPFLD
                      move    C1,COMPPATH
                      call    COMPKEY
                    LRAListView.SetItemText using N9,compcomp,1
.insert date
                      unpack     NLRADTE,str4,mm,str2
                      pack       str10 from mm,slash,str2,slash,str4
                    LRAListView.SetItemText using N9,str10,2
                    LRAListView.SetItemText using N9,taskname,3
                    move    "S.LoadLRAs-NLRAKS",Location
                    call    NLRAKS
                    if           (nlrafld <> NLRALIST)             .make sure only looking at requested list
                    break
                    endif
.check list number or change to aim                    
        repeat
          LRAListView.EnsureVisible using 0,0
          LRAListView.SetItemState giving N9 using 0,2,2
          call      Click_LRAListView
LRALoadList2
.Enable View button
        setprop LRAView,enabled=1
        clear   newdate1
        count   N2,NLRADATE
        if (N2 = "8")
                unpack  NLRADATE,CC,YY,MM,DD
                pack    newdate1,MM,slash,DD,slash,CC,YY
        endif
        setitem LRAEditEnterDate,0,newdate1
        setitem LRAEditMailer,0,NLRAMailer
        clear   newdate1
        count   N2,NLRADTE
        if (N2 = "8")
                unpack  NLRADTE,CC,YY,MM,DD
                pack    newdate1,MM,slash,DD,slash,CC,YY
        endif
        setitem LRAEditLRADate,0,newdate1
        setitem LRAEditUser,0,NLRAUSER
          move      C0,N1
           setitem    LRAEditLRANum,0,Nlranum
           setprop    LRAEditLRANum,enabled=0
           setprop    LRAEditLRANum,bgcolor=grey
           move       NLranum,n3
           setitem   LRAComboLRADesc,0,N3
           setprop    LRAComboLRADesc,enabled=0
           setprop    LRAComboLRADesc,bgcolor=grey
           setitem    LRA2EditLRANum,0,Nlranum
           packkey    nLRA2fld from NLRAlist,Nlranum
           call       Nlra2key
           if         not over
           setitem    LRA2EditLRADesc,0,NLRA2Desc
           else
           setitem    LRA2EditLRADesc,0," "
           endif

        return
.............................................................................................................................
.Verify Data Entry
LRAVerifyData
        getitem LRAEditMailer,0,NLRAMailer    .ZFILL'ed at LostFocus_Event
        call    Trim Using NLRAMailer
......................................
        pack    COMPFLD,NLRAMailer
        move    "S.VerifyData-COMPKEY",Location
        pack    KeyLocation,"Key: ",COMPFLD
        move    C1,COMPPATH
        call    COMPKEY
        if over
                alert   caution,"Valid Mailer Required!",result
                setfocus LRAEditMailer
                move    YES,ReturnFlag
                return
          elseif (COMPMLRFLG <> "T")
                alert   caution,"Valid Mailer Required!",result
                setfocus LRAEditMailer
                move    YES,ReturnFlag
                return
        endif
           getitem LRAEditList,0,NLRAList
           unpack     NLRADTE,str6,str2
        packkey    NLRAFLD1,NlraList,NLRAMailer,str6
           move       c2,nlrapath
        rep           zfill,nlrafld
        move    "S.VerifyData-NLRATST",Location
        pack    KeyLocation,"Key: ",NLRAFLD
        call    NLRATST
        if over
                if (NewFlag = "M")       .This should not happen!!
                        clear   taskname
                        append  "Modification Not Allowed!!",taskname
                        append  carr,taskname
                        append  "Record does not Exist!!",taskname
                        reset   taskname
                        alert   caution,taskname,result
                        setfocus LRAEditMailer
                        move    YES,ReturnFlag
                        return
                endif
        else
.                if (NewFlag = YES)
.                        clear   taskname
.                        append  "Record Already Exists!!",taskname
.                        append  carr,taskname
.                        append  "List: ",taskname
.                        append  NLRAList,taskname
.                        append  carr,taskname
.                        append  "Mailer: ",taskname
.                        append  NLRAMailer,taskname
.                        append  carr,taskname
.                        append  "If you wish to re-use this KEY,",taskname
.                        append  carr,taskname
.                        append  "Delete existing record first.",taskname
.                        reset   taskname
.                        alert   caution,taskname,result
.                        setfocus LRAEditMailer
.                        move    YES,ReturnFlag
.                        return
.                endif
        endif
        getitem LRAEditEnterDate,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)     .This should never happen!!
                alert   caution,"Valid Enter Date Required!",result
                setfocus LRAEditEnterDate
                move    YES,ReturnFlag
                return
        else
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                elseif (N2 <> 0)
                        alert   caution,"Enter Date Must be in MMDDCCYY Format!",result
                        setfocus LRAEditEnterDate
                        move    YES,ReturnFlag
                        return
                endif
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                setfocus LRAEditEnterDate
                move    YES,ReturnFlag
                return
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                        setfocus LRAEditEnterDate
                        move    YES,ReturnFlag
                        return
                else
                        move    CC,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                setfocus LRAEditEnterDate
                                move    YES,ReturnFlag
                                return
                        endif
                endif
        endif
        pack    NLRADATE,CC,YY,MM,DD
        pack    newdate1,MM,str1,DD,str1,CC,YY
        setitem LRAEditEnterDate,0,newdate1
.
        getitem LRAEditLRADate,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)     .This should never happen!!
                alert   caution,"Valid LRA Date Required!",result
                setfocus LRAEditLRADate
                move    YES,ReturnFlag
                return
        else
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                elseif (N2 <> 0)
                        alert   caution,"Enter Date Must be in MMDDCCYY Format!",result
                        setfocus LRAEditLRADate
                        move    YES,ReturnFlag
                        return
                endif
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                setfocus LRAEditLRADate
                move    YES,ReturnFlag
                return
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                        setfocus LRAEditLRADate
                        move    YES,ReturnFlag
                        return
                else
                        move    CC,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                setfocus LRAEditLRADate
                                move    YES,ReturnFlag
                                return
                        endif
                endif
        endif
           move       c0,n3
           getitem   LRAComboLRADesc,0,N3
           if         (n3 > 0)
           move       n3,Nlranum
           rep        ZFILL in Nlranum
           else
           alert   caution,"Invalid lra description!",result
           setfocus LRAComboLRADesc
           return
           endif
           

        pack    NLRADTE,CC,YY,MM,DD
        pack    newdate1,MM,str1,DD,str1,CC,YY
        setitem LRAEditLRADate,0,newdate1
.
        getitem LRAEditUser,0,NLRAUSER
        call    Trim using NLRAUSER
        if (NLRAUSER = "")
                alert   caution,"Valid User Name Required!",result
                setfocus LRAEditUser
                move    YES,ReturnFlag
                return
        endif
.
        return
............................................................................................
.Verify Data Entry
LRA2VerifyData
        getitem LRA2EditLRADESC,0,str55
           call       Trim using str55
           count      n2,str55
           if         (n2 < 5)
           alert      caution,"Valid description Required!",result
                setfocus LRA2EditLRADESC
                move    YES,ReturnFlag
           return
           endif
           move       str55,Nlra2Desc
.check mode mod or add
                      move       NDatLRADte,n2
           if         (n2 < 2)
                      alert      caution,"Valid LRA Renewal Month - required!",result
                           move    YES,ReturnFlag
                      return
                      endif

           if         (lra2mode = "M")           .modify
        getitem LRA2EditLRANUM,0,NLRA2NUm    .ZFILL'ed at LostFocus_Event
           packkey    nLRA2fld from NLRAlist,Nlra2num
           count      n2,nlra2fld
                      if         (n2 <> "9")
                      alert      caution,"Valid List & number required!",result
                           move    YES,ReturnFlag
                      return
                      endif
           call       Nlra2tst
                      if         over
                      alert      caution,"I did not find a record to update!",result
                           move    YES,ReturnFlag
                      else
                      call       NLRA2Upd
                      endif
           return
           Elseif     (lra2mode = "N")           .New
.find next number
           move       c1,n3
           move       n3,str3
           packkey    nLRA2fld from NLRAlist,str3
           rep        ZFILL in Nlra2fld
           loop       
           call       nlra2tst
           until      over
           add        c1,n3
           move       n3,str3
           packkey    nLRA2fld from NLRAlist,str3
           rep        ZFILL in Nlra2fld
           repeat
           unpack     nlra2fld into Nlra2list,Nlra2num
           rep        ZFILL in Nlra2num
           call       Nlra2wrt
           else
           alert      caution,"Lost!",result
                setfocus LRA2EditLRADESC
                move    YES,ReturnFlag
           return
           endif



           return
............................................................................................
LRASearchForList
.RefreshLRAIndexSearchList
.Disable Modify button
        setprop LRAModify,enabled=0
...............................................
        getitem LRAEditList,0,str6
        count   HowMany,str6
        if (HowMany > "6")
                setprop ErrorMssg,visible=1
                setprop LRAModify,enabled=0
                setfocus LRAEditList
                call    LRAClearRec
                return
        elseif (HowMany < "6")
                sub     HowMany from "6" giving N1
                setlptr filler2, N1
                pack    NdatFLD,filler2,str6
                move    NdatFLD,str6
        endif
        pack    NdatFLD,str6
        move    C3,NdatLOCK
        move    "S.Search4list-1rst NdatKEY",Location
        pack    KeyLocation,"Key: ",NdatFLD
        call    NdatKEY
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
                call    SetLRAErrorMssgDefault
                setfocus LRAEditList
                call    LRAClearRec
          else
                      move       NDatLRADte,n2

                      setitem    LRAComboBoxLRARenewal,0,n2
                      setprop    LRAComboBoxLRARenewal,enabled=0
                      setprop    LRAComboBoxLRARenewal,bgcolor=grey
                      setitem    LRAEditList,0,str6
                      move       str6,NLRAList                    
                      call       LRA2Loaddesc
                      call    LRALoadList
        endif
        return

LRA2Loaddesc
           deleteitem LRAComboLRADesc,0                           .empty the object
           move       c1,n3
           move       n3,str3
           packkey    nLRA2fld from NLRAlist,str3
           rep        ZFILL in Nlra2fld

           loop
           call       Nlra2key
           until      over
           insertitem LRAComboLRADesc,N3,NLRA2Desc
           add        c1,n3
           move       n3,str3
           packkey    nLRA2fld from NLRAlist,str3
           rep        ZFILL in Nlra2fld
           repeat

           return

LRAAcquirePDF
                    move      "\\nins1\d\Everyone\SCANNER",taskname
                    call      debug
                    move      "",str45
                    getfname open,"Open PDF File",str45,taskname,"PDF"
                    pack      FileName1,taskname,str45
                    if (str45 = "")     .User hit 'Cancel'
                              noreturn
                              goto Click_LRAQuit
                    endif
......................................................................................
.Force a view of the PDF file to ensure this is the one they want
......................................................................................

.get code from using scanner folder         Click_LRAView
.prompt for ok        
.if ok move and rename the file
.begin patch 1.10  display in browser object
           activate   LRA2
.Strange work-around - zorder for this object is somehow lost when DEACTIVATE/ACTIVATE is used on Child Form!!!
                    getprop   NLRA0002Explorer1,zorder=result
                    setprop   NLRA0002Explorer1,zorder=result
           setprop    NLRA0002,Visible=1
           setprop    NLRA0002Explorer1,visible=1
           setprop    NLRA0002Exit,visible=1
           setprop    NLRA0002ok,visible=1
           setfocus   NLRA0002
        loop
                getprop NLRA0002Explorer1,*ReadyState=result
                until (result = 4)      .ReadyState_Complete=4
        repeat
           NLRA0002Explorer1.Navigate2 USING Filename1

           setfocus   NLRA0002Explorer1 
.        loop
.                waitevent
.                setitem timer,0,18000   .reset to 30 minutes
.        repeat

.           NLRA0002Explorer1.Navigate2 USING "about:blank"
.           Batch      FileName1
.                      pause    c5
.                    alert type=yesno1," Is this the correct LRA? (if yes, Close it & I will add it to the database)",n1
.                    if (n1=6)    . 6 = yes , 7 = no
..do the copy rename thing  set flag of some sort perhaps
.                   Deactivate   LRA2
.                    elseif (n1=7)    . 6 = yes , 7 = no
.                              alert    caution,"Sorry try again",result
.                                 MOve Yes,ReturnFlag
...setfocus etc
.                                        Deactivate   LRA2
.                                        return
.                      endif
.end patch 1.10
        return

XRESIZE
           NLRA0001.Scale
           RETURN


.Include IO files
           include compio.inc
           include cntio.inc
           include npasio.inc
           include NLRAio.inc
           include NLRA2io.inc
           include nuseio.inc
.Following used only in order to load Search.plf
           include ndatio.inc
           include nrtnio.inc
           include searchio.inc      .contains logic for search.plf
           include ncmpio.inc
          include   nownio.inc
           include comlogic.inc

........................................
. Program:      NSPR0001.PLS
. Function:     Suppression Report Program
. Author:       Andrew Harkins
. Date:         August 25, 2000
. Release:      1.0
. Notes:
........................................

PC      EQU     0
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
.Patch1.3
                              include   compdd.inc
                              include   cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
.patch1.3
        include ndatdd.inc
        include nrtndd.inc
        include ncmpdd.inc
.START PATCH 1.22 ADDED LOGIC
          include   nowndd.inc
.END PATCH 1.22 ADDED LOGIC
        include ncntdd.inc
        include nusedd.inc
        include winapi.inc
RELEASE   INIT      "1.5"       ASH    .change exclusive option to allow secondary mailer info
Reldate   Init      "2013 June 06"     
.RELEASE   INIT      "1.4"       DLH    .DH version for Exclusive option covered by Patch 1.5 - only one bit of code remains...
.RELEASE   INIT      "1.37"       DLH    .add managed list to excel verion 
.Reldate   Init      "2013 April 22"     
.RELEASE   INIT      "1.36"       DLH    check for excel version
.Reldate   Init      "09 August 2012"     
.RELEASE   INIT      "1.35"       DLH    Create new read for Managed lists 
.Reldate   Init      "28 August 2012"     
.RELEASE   INIT                "1.34"       DLH    PLB CLient
.Reldate   Init      "18 June 2009"     
.RELEASE   INIT                "1.33"       DLH    Susan's Printer
.Reldate   Init      "23 April 2008"     

.RELEASE            INIT                "1.32"       DLH    08Oct2007 Include PL lists in exclusive only
.Reldate  Init      "08 October 2007"   
.RELEASE  INIT      "1.31"       ASH    09AUG2004 Logo Conversion
.release            init      "1.27"    ASH 10MAR2004 ADDED OPTION TO QUIT REPORT
.RELEASE  INIT      "1.30"       DMB 26MAY2004 Mailer Coversion
.release         init    "1.26"         ASH 29JAN2004  DATACARD CONVERSION
.release         init    "1.25"         ASH 13JAN2003 ADDED OPTION TO DUMP DATA TO EXCEL
.release         init    "1.24"   ASH 27SEP2002 REWORKED LOGIC ADDED WITH 1.23 SO THAT IT APPLIES TO PRIMARY MAILERS
.                                        INCREASED TIMER
.release         init    "1.23"   ASH 19SEP2002 ADDED LOGIC TO ALLOW DATE FILTER TO LISTS
.release         init    "1.22"   ASH 18SEP2002 ADDED FUNCTIONALITY OF SEARCH.PLF TO SEARCH FOR OWNER - OBSOLETE FOR THIS APPLICATION!!!!!
.release         init    "1.21"   ASH 09MAY2002 Activate Print option under File Menu.  Remove Preview option under File Menu
.release         init    "1.2"   ASH 01JAN2002 ADDED REVISED DATE TO SuppressListViewFinal
.release         init    "1.1"   ASH 31MAY01 ADDED OPTION FOR EXCLUSIVES
.release         init    "1.0"   ASH 14SEP00 New release

.Files used
TempName init   "C:\WORK\Suppress.dat"
.TempName2 init  "C:\WORK\Suppress.srt"
TempIName init  "C:\WORK\Suppress.isi"
TempFile1 ifile  KEYLEN=6
TempFile2 file
Prfile  pfile
preffile file
.Vars for TempFile
PendStat dim    1
LCRStat dim     1

Timer   Timer
ReturnFlag init "N"
ExitFlag init   "Y"
WithdrawnFlag form      1
LCRFlag form    1
PendingFlag form        1
.START PATCH 1.1 ADDED LOGIC
ExcFlag   form      1
.END PATCH 1.1 ADDED LOGIC
.begin patch 1.32
PLFlag    Form      1
.end patch 1.32
.START PATCH 1.24 ADDED LOGIC
RptFlag   form      1
FrmPtr    form      ^
.END PATCH 1.24 ADDED LOGIC
.START PATCH 1.27 ADDED LOGIC
StopFlag form    1
.END PATCH 1.27 ADDED LOGIC
Carr    init    0x7f
.hexeight integer 4,"4294967295"
MailerFocus form "0"
.osflag   form   1       .1=win 95,98, 2=NT
page    form    9
.userinfo dim    500
userlogn dim    7
.userlogw dim    7

.AAMKEY CRITERIA
AKey1   init    "01X"
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"

.Pointer used
EditPtr  EditText       ^
ListViewPtr ListView    ^

.Objects used by OptionsOrd.plf (a generic form)
.OptionsArrayEx dim     x(y,z)
.Following Array will allow:  y rows - representing each Screen
.                             z columns - representing # of preferances for each Screen
.                             x length of each field in each Screen
.
OptionsArr1 CONST       "2"
OptionsArr2 CONST       "5"
OptionsArrSize CONST    "5"
OptionsArray dim        OptionsArrSize(OptionsArr1,OptionsArr2)
.Screen 1
Options1Coll Collection
OptionsIncludeWithdrawns CheckBox
OptionsIncludeLCRs  CheckBox
OptionsIncludePendings  CheckBox
.Screen 2
Options2Coll Collection

.Date Parameters
FromDate1 dim   8
ToDate1  dim    8
FromDate2 dim   8
ToDate2  dim    8
DateParam1 form 1
DateParam2 form 1
.START PATCH 1.23 ADDED LOGIC
FromDate3 dim   8
ToDate3  dim    8
DateParam3 form 1
.END PATCH 1.23 ADDED LOGIC
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu

.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
.START PATCH 1.21 REPLACED LOGIC
.FData   init    "&File;&Print;Pre&view;-;E&xit"
FData   init    "&File;&Print;-;E&xit"
.END PATCH 1.21 REPLACED LOGIC
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To;&Campaign"

.Define colors EditText Inquiries
white   color
grey    color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
.begin Patch 1.37
Managed   Dim       1
.end Patch 1.37

.START PATCH 1.25 ADDED LOGIC
books     automation
book      automation
sheets    automation
sheet     automation

.begin patch 1.34  ! should direct plbclient to run locally plbwin should ignore
ex        automation          class="!Excel.Application"
.ex        automation          class="Excel.Application"
.end patch 1.34
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
.Formatting vars needed
.These constants were found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle integer 4,"0x2"
FirstRec form   8
.END PATCH 1.25 ADDED LOGIC
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER

.Set vars for About Box
        move    "NSPR0001.PLS",Wprognme
        move    "Suppression Report Program",Wfunction
        move    "Andrew Harkins",Wauthor
.        move    "1.0",Wrelease
.        move    "September 14,2000",Wreldate
        move    Release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch    plform  Search
abt     plform  about
opt     plform  OptionsOrd
x       plform  NSPR0001
        winhide
.Load Forms, Always declare parent form first
        formload x
        formload opt
        formload abt
        formload srch

.START PATCH 1.24 REPLACED LOGIC
.        CREATE  TIMER,18000     .30 minutes
        CREATE  TIMER,36000     .60 minutes
.END PATCH 1.24 REPLACED LOGIC
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  NSPR0001;mFile,FData
        create  NSPR0001;mEdit,EData,mFile
        create  NSPR0001;mOptions,OData,mEdit
        create  NSPR0001;mHelp,HData,mOptions

.Create SubMenu
        create  NSPR0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
        activate mOptions,OptionsGo,result
        activate mHelp,HelpGo,result

.Activate SubMenu
        activate sSearch,SearchGo,result

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=9
        create  font5,"Arial",size=10
        create  font6,"Arial",size=14

.START PATCH 1.31 ADDED LOGIC
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
NINLogo   PICT
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.31 ADDED LOGIC

.Create the ListView object
.START PATCH 1.2 REPLACED LOGIC
.        SuppressListViewFinal.InsertColumn using "List",160,1
.        SuppressListViewFinal.InsertColumn using "List ##",60,2
.        SuppressListViewFinal.InsertColumn using "Universe",70,3
.        SuppressListViewFinal.InsertColumn using "Times Used",70,4
        SuppressListViewFinal.InsertColumn using "List",150,1
        SuppressListViewFinal.InsertColumn using "List ##",50,2
        SuppressListViewFinal.InsertColumn using "Universe",70,3
        SuppressListViewFinal.InsertColumn using "Used",40,4
        SuppressListViewFinal.InsertColumn using "Revised Date",70,5
.START PATCH 1.23 ADDED LOGIC
        SuppressListViewFinal.InsertColumn using "Creation Date",70,6
.END PATCH 1.23 ADDED LOGIC
.begin Patch 1.37
        SuppressListViewFinal.InsertColumn using "Managed",40,7
.end Patch 1.37
.END PATCH 1.2 REPLACED LOGIC

.User Information
        move    C0,NUSEFLD
        move    C1,NUSEPATH
        move    PORTN,NUSEFLD
        rep     zfill,NUSEFLD
        call    NUSEKEY
.        goto userng if over
        scan    "INVALID",NUSEUSER
.        goto userng if equal
        reset   NUSEUSER
.Find out system information
        getinfo system,str6
        unpack  str6 into str1,str1
        move    C0,osflag
        if (str1 = "3" or str1 = "4")
                move    C1,osflag
.        elseif (str1 = "1" or str1 = "5" or str1 = "6")
        elseif (str1 = "1" or str1 = "5")
                move    C2,osflag
        endif
.Find more system info
        call    Trim using NUSEUSER
        scan    "BILLING",NUSEUSER
        if not equal
                move    NUSEUSER,str1
                loop
                        bump    NUSEUSER,1
                        cmatch  B1,NUSEUSER
                        until equal
                        until eos
                repeat
                if not eos
                        bump    NUSEUSER,1
                        move    NUSEUSER,str6
                        clear   userlogn
                        pack    userlogn,str1,str6
                endif
        endif
        reset   NUSEUSER
.Get default printer
        move    PORTN,NCNTFLD1
        rep     zfill,NCNTFLD1
        move    C3,NCNTPATH
        move    "Driver-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD1
        call    NCNTKEY
        if over
                move    C2,CNTPRINT    .Laser 3
        endif
.Prepare Objects
        SuppressListViewPrimary.InsertColumn using "Mailer ##",50,1
        SuppressListViewPrimary.InsertColumn using "Mailer Name",200,2
.
        SuppressListViewSecondary.InsertColumn using "Mailer ##",50,1
        SuppressListViewSecondary.InsertColumn using "Mailer Name",200,2
.
        SuppressListViewPrimary2.InsertColumn using "Mailer ##",50,1
        SuppressListViewPrimary2.InsertColumn using "Mailer Name",200,2
.
        SuppressListViewSecondary2.InsertColumn using "Mailer ##",50,1
        SuppressListViewSecondary2.InsertColumn using "Mailer Name",200,2
.Set up columns
        move    "500",column
        move    "1200",column1
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "6250",column6
        move    "7000",column7

.Create/Activate Objects on OptionsOrd.plf
.Screen 1
        create  Options;OptionsIncludeWithdrawns=60:80:40:255,"Include Withdrawns",zorder=100
        create  Options;OptionsIncludeLCRs=80:100:40:255,"Include LCRs",zorder=100
        create  Options;OptionsIncludePendings=100:120:40:255,"Include Pendings",zorder=100
        listins Options1Coll,OptionsIncludeWithdrawns,OptionsIncludeLCRs,OptionsIncludePendings
        setprop Options1Coll,visible=1  .always start with the first tab visible
.nothing currently in Options2Coll
.Get rid of excess Tabs
        setprop OptionsTabControl,tablabel="&1;&2"
.Open Preferences File
openpref
        pack    APIFileName,"c:\program files\nincal\nsup0001.pre",hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
.                trap    Preferror if IO
                open    preffile,"c:\program files\nincal\nsup0001.pre"
                move    C0,N9
                loop
                        add     C1,N9
                        move    C0,N8
                        loop
                                add     C1,N8
                                read    preffile,seq;OptionsArray(N9,N8)
                                until   over
                                until (N8 = OptionsArr2)
                        repeat
                        until (N9 = OptionsArr1)
                repeat
                close   preffile
.                trapclr io
                move    C0,N9
.Screen1
openpref2
                move    OptionsArray(1,1),str5
                call    Trim using str5
                move    str5,N5
                setitem OptionsIncludeWithdrawns,0,N5
.
                move    OptionsArray(1,2),str5
                call    Trim using str5
                move    str5,N5
                setitem OptionsIncludeLCRs,0,N5
.
                move    OptionsArray(1,3),str5
                call    Trim using str5
                move    str5,N5
                setitem OptionsIncludePendings,0,N5
        endif

.Main Loop
           EVENTREG  X, 17, XRESIZE

        clock   timestamp to timestamp
        setfocus SuppressEditMailer1
        loop
                waitevent
.START PATCH 1.24 REPLACED LOGIC
.                setitem timer,0,18000   .reset to 30 minutes
                setitem timer,0,36000   .reset to 60 minutes
.END PATCH 1.24 REPLACED LOGIC
        repeat

Timeout
        beep
        beep
        beep
        stop

FileGo
.Flag set to "N" if in Modify or New mode
.START PATCH 1.21 REPLACED LOGIC
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1,FileGo2,FileGo2
.END PATCH 1.21 REPLACED LOGIC
FileGo1
.Print
          call      Click_SuppressPrint
        return
.START PATCH 1.21 REPLACED LOGIC
.FileGo2
.        return
.FileGo3
.END PATCH 1.21 REPLACED LOGIC
FileGo2
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
        return
OptionsGo
        goto OptionsGo1
OptionsGo1
        setprop Options,visible=1
        return
OptionsWritePref
.Pull values from Options.plf
.Screen 1
        getitem OptionsIncludeWithdrawns,0,N5
        move    N5,OptionsArray(1,1)
        getitem OptionsIncludeLCRs,0,N5
        move    N5,OptionsArray(1,2)
        getitem OptionsIncludePendings,0,N5
        move    N5,OptionsArray(1,3)

.Write to the file
        pack    APIFileName,"c:\program files\nincal\nsup0001.pre",hexzero
        call    DeleteFile
        prep    preffile,"c:\program files\nincal\nsup0001.pre"
        move    C0,N9
        loop
                add     C1,N9
                move    C0,N8
                loop
                        add     C1,N8
                        write   preffile,seq;OptionsArray(N9,N8)
                        until   over
                        until (N8 = OptionsArr2)
                repeat
                until (N9 = OptionsArr1)
        repeat
        close   preffile
        return
HelpGo
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
.START PATCH 1.22 ADDED LOGIC
SearchGo6
.OWNER
        move    C6,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.END PATCH 1.22 ADDED LOGIC

SearchLoad
.Called by SearchDataList_DoubleClick
.Only load if in modify mode
.START PATCH 1.22 ADDED LOGIC
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.END PATCH 1.22 ADDED LOGIC
SearchLoad1
.BROKER
        return
SearchLoad2
.LIST
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        if (MailerFocus = C2)
                setitem SuppressEditMailer2,0,str4
                setfocus SuppressEditMailer2
        else
                setitem SuppressEditMailer1,0,str4
                setfocus SuppressEditMailer1
        endif
        return
SearchLoad4
.SHIP-TO
        return
SearchLoad5
.CAMPAIGN
        return
.START PATCH 1.22 ADDED LOGIC
SearchLoad6
.OWNER
        return
.END PATCH 1.22 ADDED LOGIC

SuppressDisableUpper
        setprop SuppressRun,enabled=0
        setprop SuppressDelete,enabled=0
        setprop SuppressPrint,enabled=0
.START PATCH 1.25 ADDED LOGIC
        setprop SuppressExcel,enabled=0
.END PATCH 1.25 ADDED LOGIC
        setprop SuppressAdd1,enabled=0
        setprop SuppressEditMailer1,enabled=0,bgcolor=grey
        setprop SuppressListViewPrimary,enabled=0
        setprop SuppressExit,enabled=0
        setprop SuppressRemove1,enabled=0
          setprop   SuppressCheckExclusive,enabled=0
.START PATCH 1.24 MOVED LOGIC
        setprop SuppressComboDate3,enabled=0
        setprop SuppressEditFromDate3,enabled=0,bgcolor=grey
        setprop SuppressEditToDate3,enabled=0,bgcolor=grey
.END PATCH 1.24 MOVED LOGIC
SuppressDisableUpper2
.DH 28 Aug 2012
.START PATCH 1.5 DH commented out this code with above patch, I bring it back in
        setprop SuppressComboDate1,enabled=0
        setprop SuppressEditFromDate1,enabled=0,bgcolor=grey
        setprop SuppressEditToDate1,enabled=0,bgcolor=grey
.END PATCH 1.5 DH commented out this code with above patch, I bring it back in
.DH 28 Aug 2012
        setprop SuppressAdd2,enabled=0
        setprop SuppressComboDate2,enabled=0
        setprop SuppressEditFromDate2,enabled=0,bgcolor=grey
        setprop SuppressEditMailer2,enabled=0,bgcolor=grey
        setprop SuppressEditToDate2,enabled=0,bgcolor=grey
        setprop SuppressListViewSecondary,enabled=0
        setprop SuppressRemove2,enabled=0
.START PATCH 1.23 ADDED LOGIC
.START PATCH 1.24 MOVED LOGIC
.        setprop SuppressComboDate3,enabled=0
.        setprop SuppressEditFromDate3,enabled=0,bgcolor=grey
.        setprop SuppressEditToDate3,enabled=0,bgcolor=grey
.END PATCH 1.24 MOVED LOGIC
.END PATCH 1.23 ADDED LOGIC
        return
SuppressEnableUpper
        move    NO,ExitFlag
        setprop SuppressAdd1,enabled=1
        setprop SuppressEditMailer1,enabled=1,bgcolor=white
        setprop SuppressListViewPrimary,enabled=1
        setprop SuppressExit,enabled=1
        setprop SuppressRemove1,enabled=1
          setprop   SuppressCheckExclusive,enabled=1
        setprop SuppressRun,enabled=1
        setprop SuppressDelete,enabled=1
        setprop SuppressPrint,enabled=1
.START PATCH 1.25 ADDED LOGIC
        setprop SuppressExcel,enabled=1
.END PATCH 1.25 ADDED LOGIC
.START PATCH 1.24 MOVED LOGIC
        setprop SuppressComboDate3,enabled=1
        setprop SuppressEditFromDate3,enabled=1,bgcolor=white
        setprop SuppressEditToDate3,enabled=1,bgcolor=white
.END PATCH 1.24 MOVED LOGIC
.START PATCH 1.27 ADDED LOGIC
          setprop   SuppressStop,enabled=0,height=0                   .Redundant double-check, Click event takes care of this!
.END PATCH 1.27 ADDED LOGIC
SuppressEnableUpper2
        setprop SuppressComboDate1,enabled=1
        setprop SuppressEditFromDate1,enabled=1,bgcolor=white
        setprop SuppressEditToDate1,enabled=1,bgcolor=white
        setprop SuppressAdd2,enabled=1
        setprop SuppressComboDate2,enabled=1
        setprop SuppressEditFromDate2,enabled=1,bgcolor=white
        setprop SuppressEditMailer2,enabled=1,bgcolor=white
        setprop SuppressEditToDate2,enabled=1,bgcolor=white
        setprop SuppressListViewSecondary,enabled=1
        setprop SuppressRemove2,enabled=1
.START PATCH 1.23 ADDED LOGIC
.START PATCH 1.24 MOVED LOGIC
.        setprop SuppressComboDate3,enabled=1
.        setprop SuppressEditFromDate3,enabled=1,bgcolor=white
.        setprop SuppressEditToDate3,enabled=1,bgcolor=white
.END PATCH 1.24 MOVED LOGIC
.END PATCH 1.23 ADDED LOGIC
        return

SuppressClearScreen
.Header Info
        SuppressListViewPrimary2.DeleteAllItems giving N9
        setitem SuppressStatPrimary2,0,""
        SuppressListViewSecondary2.DeleteAllItems giving N9
        setitem SuppressStatSecondary2,0,""
.Record Info
        SuppressListViewFinal.DeleteAllItems giving N9
        setitem SuppressStatTotal,0,""
        return

SuppressVerifyData
.Clear anything leftover in two Mailer fields
        setitem SuppressEditMailer1,0,""
        setitem SuppressEditMailer2,0,""
.Mailer Fields
        move    SEQ,result
        move    SEQ,N9
        SuppressListViewPrimary.GetNextItem giving result using C0,N9
        if (result = SEQ)
                alert   caution,"You must enter your Client(s)!",result
                moveaddr SuppressEditMailer1,EditPtr
                move    YES,ReturnFlag
                return
        endif
.START PATCH 1.1 ADDED LOGIC
          getitem   SuppressCheckExclusive,0,ExcFlag
.begin patch 1.32

          Getitem   SuppressCheck001,0,PlFlag
.end patch 1.32
        move    SEQ,result
        move    SEQ,N9
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.24 REPLACED LOGIC
.        SuppressListViewSecondary.GetNextItem giving result using C0,N9
.START PATCH 1.1 REPLACED LOGIC
..        if (result = SEQ)
..                alert   caution,"You must enter Secondary Mailer(s)!",result
..                moveaddr SuppressEditMailer2,EditPtr
..                move    YES,ReturnFlag
..                return
..        endif
..
.        if (result = SEQ AND ExcFlag = C0)
.                alert   caution,"You must enter Secondary Mailer(s)!",result
.                moveaddr SuppressEditMailer2,EditPtr
.                move    YES,ReturnFlag
.                return
.         elseif (result <> SEQ AND ExcFlag = C1)
.                   pack      taskname,"Suppression Reports for Exclusive Lists do not require a Secondary Mailer!",NewLine,"All Secondary Mailer information will be disregarded."
.                   alert     note,taskname,result
.        endif
.END PATCH 1.1 REPLACED LOGIC
...................
          move      C0,RptFlag
          if (ExcFlag = 1)
                    move      C1,RptFlag
          else
                  getitem SuppressEditFromDate3,0,str10
                  call    Trim using str10
                    if (str10 <> "")
                              move      C1,RptFlag
                    else
                            getitem SuppressEditToDate3,0,str10
                              call    Trim using str10
                              if (str10 <> "")
                                        move      C1,RptFlag
                              endif
                    endif
          endif
        SuppressListViewSecondary.GetNextItem giving result using C0,N9
        if (result = SEQ AND RptFlag = C0)
                alert   caution,"You must enter Secondary Mailer(s)!",result
                moveaddr SuppressEditMailer2,EditPtr
                move    YES,ReturnFlag
                return
          elseif (result <> SEQ AND RptFlag = C1)
.END PATCH 1.5 REPLACED LOGIC
.                    pack      taskname,"Suppression Reports for Exclusive Lists and List Date Selects do not require a Secondary Mailer!",NewLine,"All Secondary Mailer information will be disregarded."
.                    alert     note,taskname,result
                    move      C2,RptFlag          .Exclusive run with suppressing Mailers
.END PATCH 1.5 REPLACED LOGIC
        endif
.END PATCH 1.24 REPLACED LOGIC
.Date fields
.START PATCH 1.1 ADDED LOGIC
.START PATCH 1.24 REPLACED LOGIC
.         if (ExcFlag = C0)   .Not an Exclusive Suppression
.START PATCH 1.5 REPLACED LOGIC
.          if (RptFlag = C0)   .Regular Suppression
          if (RptFlag <> C1)   .Regular Suppression OR Exclusive with Secondary Mailers
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.24 REPLACED LOGIC
.END PATCH 1.1 ADDED LOGIC
                  move    "00000000",FromDate1
                  getitem SuppressEditFromDate1,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"From Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditFromDate1,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                          setitem SuppressEditFromDate1,0,str10
                          pack    FromDate1,CC,YY,MM,DD
                  endif
                  move    "99999999",ToDate1
                  getitem SuppressEditToDate1,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"To Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditToDate1,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                          setitem SuppressEditToDate1,0,str10
                          pack    ToDate1,CC,YY,MM,DD
                  endif
.
                  move    "00000000",FromDate2
                  getitem SuppressEditFromDate2,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"From Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditFromDate2,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                          setitem SuppressEditFromDate2,0,str10
                          pack    FromDate2,CC,YY,MM,DD
                  endif
.
                  move    "99999999",ToDate2
                  getitem SuppressEditToDate2,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"To Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditToDate2,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                    setitem SuppressEditToDate2,0,str10
                  pack    ToDate2,CC,YY,MM,DD
                  endif
.START PATCH 1.24 REMOVED LOGIC
..START PATCH 1.23 ADDED LOGIC
.                 move    "00000000",FromDate3
.                 getitem SuppressEditFromDate3,0,str10
.                 call    Trim using str10
.                 count   N9,str10
.                 if (N9 <> 0 & N9 <> 8 & N9 <> 10)
.                         alert   caution,"From Date must be in MMDDYYYY format!",result
.                         moveaddr SuppressEditFromDate3,EditPtr
.                         move    YES,ReturnFlag
.                         return
.                 elseif (N9 <> 0)
.                         if (N9 = 8)
.                                 unpack  str10,MM,DD,CC,YY
.                         elseif (N9 = 10)
.                                 unpack  str10,MM,str1,DD,str1,CC,YY
.                         endif
.                         pack    str10,MM,SLASH,DD,SLASH,CC,YY
.                         setitem SuppressEditFromDate3,0,str10
.                         pack    FromDate3,CC,YY,MM,DD
.                 endif
..
.                 move    "99999999",ToDate3
.                 getitem SuppressEditToDate3,0,str10
.                 call    Trim using str10
.                 count   N9,str10
.                 if (N9 <> 0 & N9 <> 8 & N9 <> 10)
.                         alert   caution,"To Date must be in MMDDYYYY format!",result
.                         moveaddr SuppressEditToDate3,EditPtr
.                         move    YES,ReturnFlag
.                         return
.                 elseif (N9 <> 0)
.                         if (N9 = 8)
.                                 unpack  str10,MM,DD,CC,YY
.                         elseif (N9 = 10)
.                                 unpack  str10,MM,str1,DD,str1,CC,YY
.                         endif
.                         pack    str10,MM,SLASH,DD,SLASH,CC,YY
.                   setitem SuppressEditToDate3,0,str10
.                 pack    ToDate3,CC,YY,MM,DD
.                 endif
..END PATCH 1.23 ADDED LOGIC
.END PATCH 1.24 REMOVED LOGIC
.
                  getitem SuppressComboDate1,0,DateParam1
                  getitem SuppressComboDate2,0,DateParam2
.START PATCH 1.24 REMOVED LOGIC
..START PATCH 1.23 ADDED LOGIC
.                 getitem SuppressComboDate3,0,DateParam3
..END PATCH 1.23 ADDED LOGIC
.END PATHC 1.24 REMOVED LOGIC
.START PATCH 1.24 ADDED LOGIC
.START PATCH 1.5 REPLACED LOGIC
.          else      .Exclusive/Datacard Date Select
             endif
             if (RptFlag > C0)          .Any version of Exclusive run
.END PATCH 1.5 REPLACED LOGIC
                  move    "00000000",FromDate3
                  getitem SuppressEditFromDate3,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"From Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditFromDate3,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                          setitem SuppressEditFromDate3,0,str10
                          pack    FromDate3,CC,YY,MM,DD
                  endif
.
                  move    "99999999",ToDate3
                  getitem SuppressEditToDate3,0,str10
                  call    Trim using str10
                  count   N9,str10
                  if (N9 <> 0 & N9 <> 8 & N9 <> 10)
                          alert   caution,"To Date must be in MMDDYYYY format!",result
                          moveaddr SuppressEditToDate3,EditPtr
                          move    YES,ReturnFlag
                          return
                  elseif (N9 <> 0)
                          if (N9 = 8)
                                  unpack  str10,MM,DD,CC,YY
                          elseif (N9 = 10)
                                  unpack  str10,MM,str1,DD,str1,CC,YY
                          endif
                          pack    str10,MM,SLASH,DD,SLASH,CC,YY
                    setitem SuppressEditToDate3,0,str10
                  pack    ToDate3,CC,YY,MM,DD
                  endif
                    getitem SuppressComboDate3,0,DateParam3
.END PATCH 1.24 ADDED LOGIC
.START PATCH 1.1 ADDED LOGIC
          endif
.END PATCH 1.1 ADDED LOGIC
        return

SuppressLoadReport
        call    SuppressClearScreen
.Delete any existing Suppression Files
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempName,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempIName,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.Check to ensure file does not already exist
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempName,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
                clear   taskname
                append  "Unable to delete ",taskname
                append  TempName,taskname
                append  "!",taskname
                append  carr,taskname
                append  "Contact I.S. for assistance.",taskname
                reset   taskname
                alert   caution,taskname,result
                return
        endif
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempIName,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
                clear   taskname
                append  "Unable to delete ",taskname
                append  TempIName,taskname
                append  "!",taskname
                append  carr,taskname
                append  "Contact I.S. for assistance.",taskname
                reset   taskname
                alert   caution,taskname,result
                return
        endif
.Create new file
.START PATCH 1.2 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"79"
.START PATCH 1.23 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"88"
.START PATCH 1.26 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"96"
.START PATCH 1.5 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"115"
        prepare TempFile1,TempName,TempIName,C6,"116"
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.26 REPLACED LOGIC
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.Retrieve Filters
        getitem OptionsIncludeWithdrawns,0,WithdrawnFlag
        getitem OptionsIncludeLCRs,0,LCRFlag
        getitem OptionsIncludePendings,0,PendingFlag
.START PATCH 1.5 ADDED LOGIC
          if (RptFlag = 2)    .Exclusive run with Secondary Mailer data
                    if (ExcFlag = 1)
                              if (FromDate3 = "00000000" AND ToDate3 = "99999999")
                                        setitem SuppressStatPrimary2,0,"Exclusives Only"
                              else
                                        setitem SuppressStatPrimary2,0,"Exclusives Only/List Date Select"
                              endif
                    else
                              setitem SuppressStatPrimary2,0,"List Date Select"
                    endif
          endif
.END PATCH 1.5 ADDED LOGIC

.Load It Up
        move    SEQ,result
        loop
.START PATCH 1.27 ADDED LOGIC
                    eventcheck
                    until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                move    result,N9
                SuppressListViewSecondary.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewSecondary.GetItemText giving str4 using result,C0
                SuppressListViewSecondary.GetItemText giving str45 using result,C1
                call    Trim using str4
                until (str4 = "")       .Safety measure
.Load Header
                SuppressListViewSecondary2.InsertItem giving howmany using str4
                SuppressListViewSecondary2.SetItemText using howmany,str45,1
                clear   str45
                if (DateParam2 = 1)     .Order Dates
                        append  "Order Dates ",str45
                elseif (DateParam2 = 2) .Mail Dates
                        append  "Mail Dates ",str45
                elseif (DateParam2 = 3) .Return Dates
                        append  "Return Dates ",str45
                endif
                if (FromDate2 = "00000000")
                        clear   str10
                else
                        unpack  FromDate2,CC,YY,MM,DD
                        pack    str10,MM,SLASH,DD,SLASH,CC,YY
                endif
                if (ToDate2 = "99999999")
                        clear   str11
                else
                        unpack  ToDate2,CC,YY,MM,DD
                        pack    str11,MM,SLASH,DD,SLASH,CC,YY
                endif
                if (str10 = "" AND str11 = "")
                        move    "No Date Parameters",str25
                elseif (str10 = "" AND str11 <> "")
                        pack    str25,"Up to ",str11
                elseif (str10 <> "" AND str11 = "")
                        pack    str25,"From ",str10
                else
                        pack    str25,str10,DASH,str11
                endif
                append  str25,str45
                reset   str45
                setitem SuppressStatSecondary2,0,str45
.
                pack    NORDFLD1,"01R",str4
                clear   NORDFLD2
                clear   NORDFLD3
                clear   NORDFLD4
                move    C3,NORDLOCK
                move    "SuppressLoad-NORDAIM",Location
                pack    KeyLocation,"Key: ",NORDFLD1
                call    NORDAIM
                loop
                        until over
.START PATCH 1.27 ADDED LOGIC
                              eventcheck
                              until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                        if (DateParam2 = 1)     .Order Dates
                                pack    str10,OODTEC,OODTEY,OODTEM,OODTED
                        elseif (DateParam2 = 2) .Mail Dates
                                pack    str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
                        elseif (DateParam2 = 3) .Return Dates
                                pack    str10,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
                        endif
.begin dh goes astray 08Oct07
                    move      c0,n6
                    move      Olrn,n6
.                        if (str10 >= FromDate2 AND str10 <= ToDate2)
.plflag = 1 suppress imported records
                        if ((str10 >= FromDate2 & str10 <= ToDate2 & PLFlag = c1 & n6 <> 0) or (str10 >= FromDate2 & str10 <= ToDate2 & PLFlag = c0 ))
.START PATCH 1.2 REPLACED LOGIC
.                                read    TempFile1,OLNUM;str6,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5
.START PATCH 1.23 REPLACED LOGIC
.                                read    TempFile1,OLNUM;str6,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE
.begin Patch 1.37
.                                read    TempFile1,OLNUM;str6,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                                read    TempFile1,OLNUM;str6,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.START PATCH 1.23 REPLACED LOGIC
.                                if over
.                                        move    YES,str1
.                                else
.                                        move    NO,str1
.                                endif
                                        if over
                                                  move    YES,str2
                                        else
                                                  move    NO,str2
                                        endif
.END PATCH 1.23 REPLACED LOGIC
                                move    C3,NDATLOCK
                                move    C1,NDATPATH
                                pack    NDATFLD,OLNUM
                                move    "SuppressLoad-NDATKEY",Location
                                pack    KeyLocation,"Key: ",NDATFLD
                                call    NDATKEY
                                if not over
.START PATCH 1.5 ADDED LOGIC
                                                  if (RptFlag = 2)    .Exclusive run with Secondary Mailer data
                                                            if (DateParam3 = 1)     .Creation Date
                                                                      pack    str10,NEWDATE
                                                            elseif (DateParam3 = 2) .Revised Date
                                                                      pack    str10,REVDATE
                                                            endif
                                                            rep       zfill,str10         .Need to do this step in order to cover bad data, especially in NEWDATE field
                                                  endif
                                                  if (RptFlag <> 2 OR (str10 >= FromDate3 AND str10 <= ToDate3))
                                                            if (RptFlag <> 2 OR (ExcFlag = 0 OR ELSTCDE = "C" OR ELSTCDE = "P"))
                                                                      if (ELSTCDE = "C" OR ELSTCDE = "P")
                                                                                MOve      Yes,Managed
                                                                      else
                                                                                move      No,Managed
                                                                      endif
.END PATCH 1.5 ADDED LOGIC
                                                                      if (WithdrawnFlag = C1 OR (STATUS <> "W" AND STATUS <> "T"))
                                                                                if (LCRFlag = C1 OR (OSTAT <> "l" AND OSTAT <> "z"))
                                                                                          if (PendingFlag = C1 OR (OSTAT <> "p" AND OSTAT <> "x"))
.START PATCH 1.23 ADDED LOGIC
.START PATCH 1.24 REMOVED LOGIC
.                                                                           if (DateParam3 = 1)     .Creation Date
.                                                                                         unpack    NEWDATE,str4,str7
.                                                                                 pack    str10,str7,str4
.                                                                         elseif (DateParam3 = 2) .Revised Date
.                                                                                         unpack    REVDATE,MM,str1,DD,str1,str4
.                                                                                 pack    str10,str4,MM,DD
.                                                                         endif
.                                                                         if (str10 >= FromDate3 AND str10 <= ToDate3)
.END PATCH 1.24 REMOVED LOGIC
.END PATCH 1.23 ADDED LOGIC
                                                                                                    if (OSTAT = "l" | OSTAT = "z")
                                                                                                              move    "L",LCRStat
                                                                                                    else
                                                                                                              clear   LCRStat
                                                                                                    endif
                                                                                                    if (OSTAT = "p" | OSTAT = "x")
                                                                                                              move    "P",PendStat
                                                                                                    else
                                                                                                              clear   PendStat
                                                                                                    endif
.START PATCH 1.23 REPLACED LOGIC
.                                                                 if (str1 = YES)
                                                                                                    if (str2 = YES)
.END PATCH 1.23 REPLACED LOGIC
                                                                                                              move    C1,str5
.START PATCH 1.2 REPLACED LOGIC
.                                                                                filepi  1;TempFile1
.                                                                              write   TempFile1,OLNUM;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5
.START PATCH 1.23 REPLACED LOGIC
.                                                                           filepi  1;TempFile1
.                                                                         write   TempFile1,OLNUM;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE
                                                                                                              filepi  1;TempFile1
.begin Patch 1.37
.                                                                          write   TempFile1,OLNUM;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                                                                                                              write   TempFile1,OLNUM;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
                                                                                                    else
                                                                                                              move    C0,N5
                                                                                                              call    Trim using str5
                                                                                                              move    str5,N5
                                                                                                              add     C1,N5
                                                                                                              move    N5,str5
                                                                                                              call    Trim using str5
                                                                                                              filepi  1;TempFile1
.START PATCH 1.2 REPLACED LOGIC
.                                                                            update  TempFile1;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5
.START PATCH 1.23 REPLACED LOGIC
.                                                                         update  TempFile1;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE
.begin Patch 1.37
.                                                                          update  TempFile1;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                                                                                                              update  TempFile1;OLNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
                                                                                                    endif
.                                                                               endif
                                                                            endif
                                                                    endif
                                                            endif
.START PATCH 1.5 ADDED LOGIC
                                                            endif
                                                  endif
.END PATCH 1.5 ADDED LOGIC
                                endif
                        endif
                        move    "SuppressLoad-NORDKG",Location
                        pack    KeyLocation,"Key: ",NORDFLD1
                        call    NORDKG                    
                repeat
        repeat
.Perform Suppression
        move    SEQ,result
        loop
.START PATCH 1.27 ADDED LOGIC
                    eventcheck
                    until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                move    result,N9
                SuppressListViewPrimary.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewPrimary.GetItemText giving str4 using result,C0
                SuppressListViewPrimary.GetItemText giving str45 using result,C1
                call    Trim using str4
                until (str4 = "")       .Safety measure
.Load Header
                SuppressListViewPrimary2.InsertItem giving howmany using str4
                SuppressListViewPrimary2.SetItemText using howmany,str45,1
                SuppressListViewPrimary2.SetItemText using howmany,str45,1
                clear   str45
                if (DateParam1 = 1)     .Order Dates
                        append  "Order Dates ",str45
                elseif (DateParam1 = 2) .Mail Dates
                        append  "Mail Dates ",str45
                elseif (DateParam1 = 3) .Return Dates
                        append  "Return Dates ",str45
                endif
                if (FromDate1 = "00000000")
                        clear   str10
                else
                        unpack  FromDate1,CC,YY,MM,DD
                        pack    str10,MM,SLASH,DD,SLASH,CC,YY
                endif
                if (ToDate1 = "99999999")
                        clear   str11
                else
                        unpack  ToDate1,CC,YY,MM,DD
                        pack    str11,MM,SLASH,DD,SLASH,CC,YY
                endif
                if (str10 = "" AND str11 = "")
                        move    "No Date Parameters",str25
                elseif (str10 = "" AND str11 <> "")
                        pack    str25,"Up to ",str11
                elseif (str10 <> "" AND str11 = "")
                        pack    str25,"From ",str10
                else
                        pack    str25,str10,DASH,str11
                endif
                append  str25,str45
                reset   str45
                setitem SuppressStatPrimary2,0,str45
.
                pack    NORDFLD1,"01R",str4
                clear   NORDFLD2
                clear   NORDFLD3
                clear   NORDFLD4
                move    C3,NORDLOCK
                move    "SuppressLoad-NORDAIM",Location
                pack    KeyLocation,"Key: ",NORDFLD1
                call    NORDAIM
                loop
                        until over
.START PATCH 1.27 ADDED LOGIC
                              eventcheck
                              until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                        until (str4 <> OMLRNUM)
                        if (DateParam1 = 1)     .Order Dates
                                pack    str10,OODTEC,OODTEY,OODTEM,OODTED
                        elseif (DateParam1 = 2) .Mail Dates
                                pack    str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
                        elseif (DateParam1 = 3) .Return Dates
                                pack    str10,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
                        endif
.START PATCH 1.5 ADDED LOGIC
                              if (LCRFlag = C1 OR (OSTAT <> "l" AND OSTAT <> "z"))
                                        if (PendingFlag = C1 OR (OSTAT <> "p" AND OSTAT <> "x"))
.END PATCH 1.5 ADDED LOGIC
                                                  if (str10 >= FromDate1 AND str10 <= ToDate1)
                                                            read    TempFile1,OLNUM;;
                                                            if not over
                                                                      filepi  1;TempFile1
                                                                      delete  TempFile1,OLNUM
                                                            endif
                                                  endif
.START PATCH 1.5 ADDED LOGIC
                                        endif
                              endif
.END PATCH 1.5 ADDED LOGIC
                        move    "SuppressLoad-NORDKG",Location
                        call    NORDKG
                repeat
        repeat
        close   TempFile1
.Load ListView Object
.        move    "(over)",str7
.        read    TempFile1,str7;;
        open    TempFile1,TempIName
        move    C0,howmany
        loop
.START PATCH 1.27 ADDED LOGIC
                    eventcheck
                    until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5
.START PATCH 1.23 REPLACED LOGIC
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE
.begin Patch 1.37
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
                until over
                add     C1,howmany
                SuppressListViewFinal.InsertItem giving N9 using MLSTNAME
                clear   str8
                append  str6,str8
                if (str1 = "W" | str1 = "T")
                        append  str1,str8
                endif
                reset   str8
                SuppressListViewFinal.SetItemText using N9,str8,1
.START PATCH 1.26 REPLACED LOGIC
.                call    FormatNumeric using UNIVERSE,str11
.                SuppressListViewFinal.SetItemText using N9,str11,2
                call    FormatNumeric using UNIVERSE,str13
                SuppressListViewFinal.SetItemText using N9,str13,2
.END PATCH 1.26 REPLACED LOGIC
                call    Trim using str5
                call    FormatNumeric using str5,str7
                clear   str9
                append  str7,str9
                call    Trim using LCRStat
                append  LCRStat,str9
                call    Trim using PendStat
                append  PendStat,str9
                reset   str9
                SuppressListViewFinal.SetItemText using N9,str9,3
.START PATCH 1.2 ADDED LOGIC
.START PATCH 1.26 REPLACED LOGIC
.                SuppressListViewFinal.SetItemText using N9,REVDATE,4
                    unpack    REVDATE,CC,YY,MM,DD
                    rep       zfill,MM
                    rep       zfill,DD
                    rep       zfill,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                SuppressListViewFinal.SetItemText using N9,str10,4
.END PATCH 1.26 REPLACED LOGIC
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.23 ADDED LOGIC
.START PATCH 1.26 REPLACED LOGIC
.                   unpack    NEWDATE,MM,DD,CC,YY
                    unpack    NEWDATE,CC,YY,MM,DD
.END PATCH 1.26 REPLACED LOGIC
                    rep       zfill,MM
                    rep       zfill,DD
                    rep       zfill,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    SuppressListViewFinal.SetItemText using N9,str10,5
                    SuppressListViewFinal.SetColumnFormat using 4,1
.END PATCH 1.23 ADDED LOGIC
.begin Patch 1.37
                    SuppressListViewFinal.SetItemText using N9,Managed,6
.end Patch 1.37

                SuppressListViewFinal.SetColumnFormat using 2,1
                SuppressListViewFinal.SetColumnFormat using 3,1
        repeat
        if (howmany > C0)
                clear   str35
                move    howmany,str9
                call    Trim using str9
                pack    str35,str9," Record(s) Found."
                setitem SuppressStatTotal,0,str35
.Load Header information

        endif
        close   TempFile1
        return

.START PATCH 1.1 ADDED LOGIC
SuppressLoadReport2
        call    SuppressClearScreen
          setitem   SuppressCheckExclusive,0,0
.Delete any existing Suppression Files
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempName,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempIName,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.Check to ensure file does not already exist
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempName,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
                clear   taskname
                append  "Unable to delete ",taskname
                append  TempName,taskname
                append  "!",taskname
                append  carr,taskname
                append  "Contact I.S. for assistance.",taskname
                reset   taskname
                alert   caution,taskname,result
                return
        endif
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,TempIName,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
                clear   taskname
                append  "Unable to delete ",taskname
                append  TempIName,taskname
                append  "!",taskname
                append  carr,taskname
                append  "Contact I.S. for assistance.",taskname
                reset   taskname
                alert   caution,taskname,result
                return
        endif
.Create new file
.START PATCH 1.24 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"79"
.START PATCH 1.26 REPLACED LOGIC
.        prepare TempFile1,TempName,TempIName,C6,"96"
.begin Patch 1.37
.        prepare TempFile1,TempName,TempIName,C6,"115"
        prepare TempFile1,TempName,TempIName,C6,"116"
.end Patch 1.37
.END PATCH 1.26 REPLACED LOGIC
.END PATCH 1.24 REPLACED LOGIC
.Retrieve Filters
        getitem OptionsIncludeWithdrawns,0,WithdrawnFlag
        getitem OptionsIncludeLCRs,0,LCRFlag
        getitem OptionsIncludePendings,0,PendingFlag
.Perform Suppression
.START PATCH 1.24 REPLACED LOGIC
.         setitem SuppressStatPrimary2,0,"Exclusives Only"
          if (ExcFlag = 1)
                    if (FromDate3 = "00000000" AND ToDate3 = "99999999")
                              setitem SuppressStatPrimary2,0,"Exclusives Only"
                    else
                              setitem SuppressStatPrimary2,0,"Exclusives Only/List Date Select"
                    endif
          else
                    setitem SuppressStatPrimary2,0,"List Date Select"
          endif
.END PATCH 1.24 REPLACED LOGIC
        move    C0,howmany    .Counter used to determine how many records read
        move    SEQ,result
        loop
.START PATCH 1.27 ADDED LOGIC
                    eventcheck
                    until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                move    result,N9
                SuppressListViewPrimary.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewPrimary.GetItemText giving str4 using result,C0
                SuppressListViewPrimary.GetItemText giving str45 using result,C1
                call    Trim using str4
                until (str4 = "")       .Safety measure
.Load Header
                SuppressListViewPrimary2.InsertItem giving howmany using str4
                SuppressListViewPrimary2.SetItemText using howmany,str45,1
                SuppressListViewPrimary2.SetItemText using howmany,str45,1
                    move      C1,NDATPATH
                    move      "-4",SEQ
.begin patch 1.35 DLH 28 Aug 2012
                    MOve      c3,NDATLOCK
                    Move      c4,Ndatpath
                    move      "999999",ndatfld
                    call      Ndatkey
.end patch 1.35 DLH 28 Aug 2012
                    loop
                              move    "LoadReport2A-NDATSEQ",Location
.begin patch 1.35 DLH 28 Aug 2012
                              call NdatKP
.                           call    NDATSEQ
.end patch 1.35 DLH 28 Aug 2012
                           until over
                    repeat
                    move      "-1",SEQ
.begin patch 1.35 DLH 28 Aug 2012
                    MOve      c3,NDATLOCK
                    Move      c4,Ndatpath
                    move      "000000",ndatfld
                    call      Ndatkey
.end patch 1.35 DLH 28 Aug 2012
                  loop
.START PATCH 1.27 ADDED LOGIC
                              eventcheck
                              until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
                          move    "LoadReport2B-NDATSEQ",Location
.begin patch 1.35 DLH 28 Aug 2012
                              call NdatKS
.                           call    NDATSEQ
.end patch 1.35 DLH 28 Aug 2012
                          until over
.START PATCH 1.24 REPLACED LOGIC
.                             if (ELSTCDE = "C")
.                                       if (WithdrawnFlag = C1 OR (STATUS <> "W" AND STATUS <> "T"))
.                                             pack    NORDFLD1,"01R",str4
.                                             pack          NORDFLD2,"02X",LSTNUM
.                                             clear   NORDFLD3
.                                             clear   NORDFLD4
.                                             move    C3,NORDLOCK
.                                             move    "SuppressLoad2-NORDAIM",Location
.                                             pack    KeyLocation,"Key: ",NORDFLD1,NORDFLD2
.                                             call    NORDAIM
.                                                 if over
.                                                   read    TempFile1,OLNUM;;
.                                                   if over
.                                                        clear   LCRStat
.                                                        clear   PendStat
.                                                                     pack      str5,"N/A"
.                                                        filepi  1;TempFile1
..START PATCH 1.2 REPLACED LOGIC
..                                                        write   TempFile1,LSTNUM;LSTNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5
..START PATCH 1.23 REPLACED LOGIC
..                                                        write   TempFile1,LSTNUM;LSTNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE
.                                                        write   TempFile1,LSTNUM;LSTNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
..END PATCH 1.23 REPLACED LOGIC
..END PATCH 1.2 REPLACED LOGIC
.                                                           else
.                                                           filepi  1;TempFile1
.                                                           delete  TempFile1,LSTNUM
.                                                   endif
.                                                 endif
.                                       endif
.                             endif
....................
                              if (DateParam3 = 1)     .Creation Date
.START PATCH 1.26 REPLACED LOGIC
.                                       unpack    NEWDATE,str3,str1,str7
.                                       pack    str10,str7,str3,str1
                                        pack    str10,NEWDATE
.END PATCH 1.26 REPLACED LOGIC

                              elseif (DateParam3 = 2) .Revised Date
.START PATCH 1.26 REPLACED LOGIC
.                                       unpack    REVDATE,MM,str1,DD,str1,str3,str1
.                                       pack    str10,str3,str1,MM,DD
                                        pack    str10,REVDATE
.END PATCH 1.26 REPLACED LOGIC
                              endif
.START PATCH 1.5 ADDED LOGIC
                                        rep       zfill,str10
.END PATCH 1.5 ADDED LOGIC
                              if (str10 >= FromDate3 AND str10 <= ToDate3)
.begin patch 1.32
.                                       if (ExcFlag = 0 OR ELSTCDE = "C")
                                        if (ExcFlag = 0 OR ELSTCDE = "C" OR ELSTCDE = "P")
.end patch 1.32
.begin Patch 1.37
                                        if (ELSTCDE = "C" OR ELSTCDE = "P")
                                        MOve      Yes,Managed
                                        else
                                        move      No,Managed
                                        endif
.end Patch 1.37

                                                  if (WithdrawnFlag = C1 OR (STATUS <> "W" AND STATUS <> "T"))
                                                        pack    NORDFLD1,"01R",str4
                                                        pack          NORDFLD2,"02X",LSTNUM
                                                        clear   NORDFLD3
                                                        clear   NORDFLD4
                                                        move    C3,NORDLOCK
                                                        move    "SuppressLoad2-NORDAIM",Location
                                                        pack    KeyLocation,"Key: ",NORDFLD1,NORDFLD2
                                                        call    NORDAIM
                                                            if over
                                                              read    TempFile1,OLNUM;;
                                                              if over
                                                                  clear   LCRStat
                                                                  clear   PendStat
                                                                                pack      str5,"N/A"
                                                                  filepi  1;TempFile1
.begin Patch 1.37
.                                                                  write   TempFile1,LSTNUM;LSTNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                                                                  write   TempFile1,LSTNUM;LSTNUM,MLSTNAME,STATUS,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
                                                                      else
                                                                      filepi  1;TempFile1
                                                                      delete  TempFile1,LSTNUM
                                                              endif
                                                            endif
                                                  endif
                                        endif
                              endif
.END PATCH 1.24 REPLACED LOGIC
                    repeat
          repeat
        close   TempFile1
.Load ListView Object
        open    TempFile1,TempIName
        move    C0,howmany
        loop
.START PATCH 1.27 ADDED LOGIC
                    eventcheck
                    until (StopFlag = 1)
.END PATCH 1.27 ADDED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5
.START PATCH 1.23 REPLACED LOGIC
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE
.begin Patch 1.37
.                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE
                read    TempFile1,SEQ;str6,MLSTNAME,str1,PendStat,LCRStat,UNIVERSE,str5,REVDATE,NEWDATE,Managed
.end Patch 1.37
.END PATCH 1.23 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
                until over
                add     C1,howmany
                SuppressListViewFinal.InsertItem giving N9 using MLSTNAME
                clear   str8
                append  str6,str8
                if (str1 = "W" | str1 = "T")
                        append  str1,str8
                endif
                reset   str8
                SuppressListViewFinal.SetItemText using N9,str8,1
.START PATCH 1.26 REPLACED LOGIC
.                call    FormatNumeric using UNIVERSE,str11
.                SuppressListViewFinal.SetItemText using N9,str11,2
                call    FormatNumeric using UNIVERSE,str13
                SuppressListViewFinal.SetItemText using N9,str13,2
.END PATCH 1.26 REPLACED LOGIC
                call    Trim using str5
                call    FormatNumeric using str5,str7
                clear   str9
                append  str7,str9
                call    Trim using LCRStat
                append  LCRStat,str9
                call    Trim using PendStat
                append  PendStat,str9
                reset   str9
                SuppressListViewFinal.SetItemText using N9,str9,3
.START PATCH 1.2 ADDED LOGIC
.START PATCH 1.26 REPLACED LOGIC
.                SuppressListViewFinal.SetItemText using N9,REVDATE,4
                    unpack    REVDATE,CC,YY,MM,DD
                    rep       zfill,MM
                    rep       zfill,DD
                    rep       zfill,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                SuppressListViewFinal.SetItemText using N9,str10,4
.END PATCH 1.26 REPLACED LOGIC
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.23 ADDED LOGIC
.START PATCH 1.26 REPLACED LOGIC
.                   unpack    NEWDATE,MM,DD,CC,YY
                    unpack    NEWDATE,CC,YY,MM,DD
.END PATCH 1.26 REPLACED LOGIC
                    rep       zfill,MM
                    rep       zfill,DD
                    rep       zfill,YY
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    SuppressListViewFinal.SetItemText using N9,str10,5
                    SuppressListViewFinal.SetColumnFormat using 4,1
.END PATCH 1.23 ADDED LOGIC
.begin Patch 1.37
                    SuppressListViewFinal.SetItemText using N9,Managed,6
.end Patch 1.37
                SuppressListViewFinal.SetColumnFormat using 2,1
                SuppressListViewFinal.SetColumnFormat using 3,1
        repeat
        if (howmany > C0)
                clear   str35
                move    howmany,str9
                call    Trim using str9
                pack    str35,str9," Record(s) Found."
                setitem SuppressStatTotal,0,str35
        endif
        close   TempFile1
        return
.END PATCH 1.1 ADDED LOGIC

.SuppressRunReport
..Delete any existing Suppression Files
.        move    "                                        ",APIFileName
.        clear   APIFileName
.        pack    APIFileName,TempName,hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.        endif
.                move    "                                        ",APIFileName
.        clear   APIFileName
.        pack    APIFileName,TempName2,hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.        endif
.        move    "                                        ",APIFileName
.        clear   APIFileName
.        pack    APIFileName,TempIName,hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.        endif
..Check to ensure file does not already exist
.        move    "                                        ",APIFileName
.        clear   APIFileName
.        pack    APIFileName,TempName,hexzero
.        call    FindFirstFile
.        if (APIResult <> 0 & APIResult <> hexeight)
.                clear   taskname
.                append  "Unable to delete ",taskname
.                append  TempName,taskname
.                append  "!",taskname
.                append  carr,taskname
.                append  "Contact I.S. for assistance.",taskname
.                reset   taskname
.                alert   caution,taskname,result
.                return
.        endif
.        move    "                                        ",APIFileName
.        clear   APIFileName
.        pack    APIFileName,TempIName,hexzero
.        call    FindFirstFile
.        if (APIResult <> 0 & APIResult <> hexeight)
.                clear   taskname
.                append  "Unable to delete ",taskname
.                append  TempIName,taskname
.                append  "!",taskname
.                append  carr,taskname
.                append  "Contact I.S. for assistance.",taskname
.                reset   taskname
.                alert   caution,taskname,result
.                return
.        endif
..Create new file
.        prepare TempFile1,TempName,TempIName,C6,"42"
..Load It Up
.        move    SEQ,result
.        loop
.                move    result,N9
.                SuppressListViewSecondary.GetNextItem giving result using C0,N9
.                until (result = SEQ)
.                SuppressListViewSecondary.GetItemText giving str4 using result,C0
.                call    Trim using str4
.                until (str4 = "")       .Safety measure
.                pack    NORDFLD1,"01R",str4
.                clear   NORDFLD2
.                clear   NORDFLD3
.                clear   NORDFLD4
.                move    C3,NORDLOCK
.                move    "SuppressRun-NORDAIM",Location
.                pack    KeyLocation,"Key: ",NORDFLD1
.                call    NORDAIM
.                loop
.                        until over
.                        if (DateParam2 = 1)     .Order Dates
.                                pack    str10,OODTEC,OODTEY,OODTEM,OODTED
.                        elseif (DateParam2 = 2) .Mail Dates
.                                pack    str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                        elseif (DateParam2 = 3) .Return Dates
.                                pack    str10,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
.                        endif
.                        if (str10 >= FromDate2 AND str10 <= ToDate2)
.                                read    TempFile1,OLNUM;;
.                                if over
.                                        move    C3,NDATLOCK
.                                        move    C1,NDATPATH
.                                        pack    NDATFLD,OLNUM
.                                        move    "SuppressRun-NDATKEY",Location
.                                        pack    KeyLocation,"Key: ",NDATFLD
.                                        call    NDATKEY
.                                        if not over
.                                                if (STATUS <> "W" AND STATUS <> "T")
.                                                        filepi  1;TempFile1
.                                                        write   TempFile1,OLNUM;OLNUM,MLSTNAME
.                                                endif
.                                        endif
.                                endif
.                        endif
.                        move    "SuppressRun-NORDKG",Location
.                        pack    KeyLocation,"Key: ",NORDFLD1
.                        call    NORDKG
.                repeat
.        repeat
..Perform Suppression
.        move    SEQ,result
.        loop
.                move    result,N9
.                SuppressListViewPrimary.GetNextItem giving result using C0,N9
.                until (result = SEQ)
.                SuppressListViewPrimary.GetItemText giving str4 using result,C0
.                call    Trim using str4
.                until (str4 = "")       .Safety measure
.                pack    NORDFLD1,"01R",str4
.                clear   NORDFLD2
.                clear   NORDFLD3
.                clear   NORDFLD4
.                move    C3,NORDLOCK
.                move    "SuppressRun-NORDAIM",Location
.                pack    KeyLocation,"Key: ",NORDFLD1
.                call    NORDAIM
.                loop
.                        until over
.                        if (DateParam1 = 1)     .Order Dates
.                                pack    str10,OODTEC,OODTEY,OODTEM,OODTED
.                        elseif (DateParam1 = 2) .Mail Dates
.                                pack    str10,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                        elseif (DateParam1 = 3) .Return Dates
.                                pack    str10,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
.                        endif
.                        if (str10 >= FromDate1 AND str10 <= ToDate1)
.                                read    TempFile1,OLNUM;;
.                                if not over
.                                        filepi  1;TempFile1
.                                        delete  TempFile1,OLNUM
.                                endif
.                        endif
.                        move    "SuppressRun-NORDKG",Location
.                        call    NORDKG
.                repeat
.        repeat
..Write Report
.        call    SuppressOpenPrintFile
.        move    C0,page
.        call    SuppressPrintHeader
.        call    SuppressPrintRecord
.        close   TempFile1
.        close   TempFile2
.        prtclose prfile
.        return

SuppressRunReport
.Write Report
        move    SEQ,howmany
        move    howmany,N9
        SuppressListViewFinal.GetNextItem giving howmany using C2,N9
        if (howmany = SEQ)
                alert   caution,"You must select records to print!",result
                return
        endif
        call    SuppressOpenPrintFile
        move    C0,page
        call    SuppressPrintHeader
        call    SuppressPrintRecord
        prtclose prfile
        return

.START PATCH 1.25 ADDED LOGIC
SuppressRunReport2
.Write Report in Excel Format
        move    SEQ,howmany
        move    howmany,N9
        SuppressListViewFinal.GetNextItem giving howmany using C2,N9
        if (howmany = SEQ)
                alert   caution,"You must select records to print!",result
                return
        endif
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
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
          sheet.activate
.Set Printing Sizes
.       setprop sheet.PageSetup,*Zoom=Zoom87
.START PATCH 1.31 REPLACED LOGIC
.        setprop sheet.range("A1"),*Value="Names in the News",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A1").Font,*Name="Times New Roman",*Underline=xlUnderlineStyleSingle,*Size=20
.        setprop sheet.range("A1").Characters(1,5).Font,*Bold="True"
.        setprop sheet.range("A1").Characters(7,11).Font,*Italic="True"
.        sheet.range("A1:C1").Merge
.        setprop sheet.range("A2"),*Value="C  A  L  I  F  O  R  N  I  A       I  N  C .",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A2").Font,*Name="Times New Roman",*Size=10
.        sheet.range("A2:C2").Merge
........................
          setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet.range("A1:E1").Merge
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.END PATCH 1.31 REPLACED LOGIC
          pack      str55,"TO:  ",CNTNAME
        setprop sheet.range("A4"),*Value=str55
          unpack    timestamp,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
          pack      str55,"DATE:  ",str10
        setprop sheet.range("C4"),*Value=str55
        setprop sheet.range("B6"),*Value="SUPPRESSION REPORT",*HorizontalAlignment=AlignCenter
        setprop sheet.range("B6").Font,*Bold="True",*Size=12
        setprop sheet.range("A8"),*Value="My Client(s):"
          move      "8",howmany
        getitem SuppressStatPrimary2,0,str45
        setprop sheet.range("C8"),*Value=str45
        move    SEQ,result
        loop
                add     C1,howmany
                move    result,N9
                SuppressListViewPrimary2.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewPrimary2.GetItemText giving MNUM using result,C0
                SuppressListViewPrimary2.GetItemText giving MCOMP using result,C1
                    move      howmany,str9
                    call      Trim using str9
                    pack      str10,"B",str9
                    pack      str55,MNUM,B1,B1,MCOMP
                setprop sheet.range(str10),*Value=str55
        repeat
        add     C1,howmany
.
          move      howmany,str9
          call      Trim using str9
          pack      str10,"A",str9
          setprop sheet.range(str10),*Value="Secondary Mailer(s):"
        getitem SuppressStatSecondary2,0,str45
          pack      str10,"C",str9
          setprop sheet.range(str10),*Value=str45
        move    SEQ,result
        loop
                add     C1,howmany
                move    result,N9
                SuppressListViewSecondary2.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewSecondary2.GetItemText giving MNUM using result,C0
                SuppressListViewSecondary2.GetItemText giving MCOMP using result,C1
                    move      howmany,str9
                    call      Trim using str9
                    pack      str10,"B",str9
                pack          str55,MNUM,B1,B1,MCOMP
                setprop sheet.range(str10),*Value=str55
        repeat
          add     C1,howmany
          move      howmany,str9
          call      Trim using str9
          clear     str1
        if (WithdrawnFlag = C1)
                    pack      str10,"A",str9
                setprop sheet.range(str10),*Value="Includes Withdrawns"
                    setprop sheet.range(str10).Font,*Italic="True"
                add     C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    move      YES,str1
        endif
        if (LCRFlag = C1)
                    pack      str10,"A",str9
                setprop sheet.range(str10),*Value="Includes LCRs"
                    setprop sheet.range(str10).Font,*Italic="True"
                add     C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    move      YES,str1
        endif
        if (PendingFlag = C1)
                    pack      str10,"A",str9
                setprop sheet.range(str10),*Value="Includes Pending Orders"
                    setprop sheet.range(str10).Font,*Italic="True"
                add     C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    move      YES,str1
        endif
          if (str1 = YES)
                    add     C1,howmany
                    move      howmany,str9
                    call      Trim using str9
          endif
          move      howmany,FirstRec
          move      howmany,str9
          call      Trim using str9
          pack      str10,"A",str9
          setprop sheet.range(str10),*Value="List ##"
          pack      str11,"B",str9
          setprop sheet.range(str11),*Value="List Name"
          pack      str11,"C",str9
          setprop sheet.range(str11),*Value="Universe"
          pack      str11,"D",str9
          setprop sheet.range(str11),*Value="Times Used"
.begin Patch 1.37
          pack      str11,"E",str9
          setprop sheet.range(str11),*Value="Managed List"
.end Patch 1.37
          setprop sheet.range(str10,str11).Font,*Bold="True"
          sheet.range(str10,str11).BorderAround using *LineStyle=1,*Weight=2
          pack    str11,"1:",str9
        setprop sheet.PageSetup,*PrintTitleRows=str11
          add     C1,howmany
        move    C0,N10
        move    SEQ,result
        loop
                add     C1,howmany
                move    result,N9
                SuppressListViewFinal.GetNextItem giving result using C2,N9
                until (result = SEQ)
                SuppressListViewFinal.GetItemText giving MLSTNAME using result,C0
                SuppressListViewFinal.GetItemText giving str8 using result,C1
                SuppressListViewFinal.GetItemText giving str11 using result,C2
                SuppressListViewFinal.GetItemText giving str12 using result,C3
.begin Patch 1.37
                SuppressListViewFinal.GetItemText giving managed using result,C6
.end Patch 1.37

                    move      howmany,str9
                    call      Trim using str9
                    pack      str10,"A",str9
                setprop sheet.range(str10),*NumberFormat="@",*Value=str8        .NumberFormat="@" - Option to treat Numbers as Text
                    pack      str10,"B",str9
                setprop sheet.range(str10),*Value=MLSTNAME
                    pack      str10,"C",str9
                    setprop sheet.range(str10),*Value=str11,*HorizontalAlignment=AlignRight
                    pack      str10,"D",str9
                setprop sheet.range(str10),*Value=str12,*HorizontalAlignment=AlignRight
.begin Patch 1.37
                    pack      str10,"E",str9
                setprop sheet.range(str10),*Value=Managed,*HorizontalAlignment=AlignCenter
.end Patch 1.37
                add     C1,N10
        repeat
        move    N10,str10
        call    Trim using str10
        pack    str25,str10," Record(s)"
          add       C2,howmany
          move      howmany,str9
          call      Trim using str9
          pack      str11,"A",str9
          setprop sheet.range(str11),*Value=str25
.
.....Format document.....
          pack      str12,"A",str9
          move    FirstRec,str10
          call    Trim using str10
          pack    str11,"A",str10
          sheet.range(str11,str12).Columns.Autofit
          pack      str11,"B",str9
          pack      str12,"B",str10
          sheet.range(str11,str12).Columns.Autofit
.         pack      str11,"E",str9
.         pack      str12,"E",str10
.         sheet.range(str11,str12).Columns.Autofit
.         pack      str11,"F",str9
.         pack      str12,"F",str10
.         sheet.range(str11,str12).Columns.Autofit
FileNameSelect
.begin patch 1.36 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.end patch 1.36 get exel version info

          clear   taskname
          move      "C:\WORK\",taskname                     ."
          setprop ex,*DefaultFilePath=taskname
          pack    taskname,taskname,"SUPPRESS"
          setmode *mcursor=*arrow
          ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
          if (taskname <> "0")
                    movelptr taskname,N9
                    reset   taskname,N9
.begin patch 1.36
                              if        (#ver = c1)
                              append  "xlsx",taskname
                              else
                              append  "xls",taskname
                              endif

.                    append  "xls",taskname
.end patch 1.36
                    reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                    trap    TrapObject if Object
                    book.saveas giving N9 using *Filename=taskname
                    trapclr Object
          endif
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.        book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy sheet
          destroy sheets
          destroy book
          destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
          setprop ex,*DisplayAlerts=OFALSE
          setprop ex,*SheetsInNewWorkbook=SheetsDefault
          destroy OTRUE
          destroy OFALSE
          destroy ex
          return

TrapObject
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
.                goto FileNameSelect
          endif
.Send them back to select another File name and try to Save again.
          goto FileNameSelect
.        goto CampaignCleanUp

errortrap
.testing purposes
          getinfo exception,taskname
          return

        return
.END PATCH 1.25 ADDED LOGIC

SuppressOpenPrintFile
        if (CNTPRINT = 1)
                if (osflag = c2)         .nt
                        PRTOPEN prfile,"@Laser3 on NINS2","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@Laser3","FAXFILE.PRN"
                elseif (osflag = c6)         .XP
                        PRTOPEN prfile,"@Laser3 On NINS2","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif
        elseif (CNTPRINT = 2)  .Laser3
                if (osflag = c2)         .nt
.                        PRTOPEN prfile,"@Laser3 Blankstock on NINS2","FAXFILE.PRN"
                        PRTOPEN prfile,"@\\NINS2\laser3","FAXFILE.PRN"
                elseif (osflag = c6)         .XP
                        PRTOPEN prfile,"@Laser3 Blankstock on NINS2","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@Laser3 Blankstock","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif
        elseif (CNTPRINT = 3)     .Laser2
                if (osflag = c2)         .nt
                        PRTOPEN prfile,"@\\NINS2\Laser2","FAXFILE.PRN"
                elseif (osflag = c6)         .XP
                        PRTOPEN prfile,"@Laser2 on NINS2","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@Laser2","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif
        elseif (CNTPRINT = 5)     .Susan
                if (osflag = c2)         .nt
                        PRTOPEN prfile,"@Kyocera FS-1030D","FAXFILE.PRN"
.                        PRTOPEN prfile,"@\\NIN0032\KYOCERAS","FAXFILE.PRN"
                elseif (osflag = c6)         .XP
                        PRTOPEN prfile,"@Kyocera FS1030D","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@KYOCERAS","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif
        elseif (CNTPRINT = 7)     .DH
                if (osflag = c2 or Osflag = c6)         .nt +
                        PRTOPEN prfile,"@Kyocera Mita FS-C5016N","FAXFILE.PRN"
.                        PRTOPEN prfile,"@\\NIN0031\KYOCERAM","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@KYOCERAM","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif

        else                    .Laser2 = Default
                if (osflag = c2)         .nt
                        PRTOPEN prfile,"@Laser2 on NINS2","FAXFILE.PRN"
                elseif (osflag = c6)         .XP
                        PRTOPEN prfile,"@Laser2 on NINS2","FAXFILE.PRN"
                elseif (osflag = c1)         .win 95 98
                        PRTOPEN prfile,"@Laser2","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"@","FAXFILE.PRN"
                endif
        endif
        return

SuppressPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        unpack  timestamp,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.31 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
..............
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
.END PATCH 1.31 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,"TO:";
        prtpage prfile;*p800:row,CNTNAME;
        prtpage prfile;*p5500:row,"DATE:  ";
        prtpage prfile;*p6000:row,str10;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font1,*boldon,"SUPPRESSION REPORT",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,"My Client(s):";
        getitem SuppressStatPrimary2,0,str45
        prtpage prfile;*p5500:row,str45
        move    SEQ,result
        loop
                add     eightlpi,row
                move    result,N9
                SuppressListViewPrimary2.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewPrimary2.GetItemText giving MNUM using result,C0
                SuppressListViewPrimary2.GetItemText giving MCOMP using result,C1
                prtpage prfile;*pcolumn1:row,MNUM;
                prtpage prfile;*pcolumn2:row,MCOMP;
        repeat
        add     eightlpi,row
.
        prtpage prfile;*pcolumn:row,"Secondary Mailer(s):";
        getitem SuppressStatSecondary2,0,str45
        prtpage prfile;*p5500:row,str45
        move    SEQ,result
        loop
                add     eightlpi,row
                move    result,N9
                SuppressListViewSecondary2.GetNextItem giving result using C0,N9
                until (result = SEQ)
                SuppressListViewSecondary2.GetItemText giving MNUM using result,C0
                SuppressListViewSecondary2.GetItemText giving MCOMP using result,C1
                prtpage prfile;*pcolumn1:row,MNUM;
                prtpage prfile;*pcolumn2:row,MCOMP;
        repeat
        add     eightlpi,row
        if (WithdrawnFlag = C1)
                prtpage prfile;*pcolumn:row,"Includes Withdrawns";
                add     eightlpi,row
        endif
        if (LCRFlag = C1)
                prtpage prfile;*pcolumn:row,"Includes LCRs";
                add     eightlpi,row
        endif
        if (PendingFlag = C1)
                prtpage prfile;*pcolumn:row,"Includes Pending Orders";
                add     eightlpi,row
        endif
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*boldon,"List ##";
        prtpage prfile;*pcolumn1:row,"List Name";
        prtpage prfile;*pcolumn6:row,*alignment=*right,"Universe",*alignment=*left;
        prtpage prfile;*pcolumn7:row,"Times Used",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

SuppressPrintRecord
.howmany cannot be used by any sub-routine called by this routine as the value
.must remain intact
        move    C0,N10
        move    SEQ,howmany
        loop
.TEST FOR ENOUGH ROOM ON PAGE
                if (row >= 9250)        .Position of Largest Possible Last Record
                        prtpage prfile;*NEWPAGE;
                        call    SuppressPrintHeader
                endif
                add     eightlpi,row
                move    howmany,N9
                SuppressListViewFinal.GetNextItem giving howmany using C2,N9
                until (howmany = SEQ)
                SuppressListViewFinal.GetItemText giving MLSTNAME using howmany,C0
                SuppressListViewFinal.GetItemText giving str8 using howmany,C1
                SuppressListViewFinal.GetItemText giving str11 using howmany,C2
                SuppressListViewFinal.GetItemText giving str9 using howmany,C3
                prtpage prfile;*pcolumn:row,str8;
                prtpage prfile;*pcolumn1:row,MLSTNAME;
                prtpage prfile;*pcolumn6:row,*alignment=*right,str11,*alignment=*left;
                prtpage prfile;*pcolumn7:row,str9;
                add     C1,N10
        repeat
        move    N10,str10
        call    Trim using str10
        pack    str25,str10," Record(s)"
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn1:row,str25;
        return

SuppressAddToList LRoutine EditPtr,ListViewPtr
        getitem EditPtr,0,str4
        call    Trim using str4
        if (str4 <> "")
                call    ZFILLIT using str4,C0
                setitem EditPtr,0,str4
                pack    MKEY,str4,"000"
                move    C1,NMLRPATH
                move    C3,NMLRLOCK
                move    "SuppressAdd-NMLRKEY",Location
                pack    KeyLocation,"Key: ",MKEY
                call    NMLRKEY
                if not over
                        ListViewPtr.InsertItem giving result using MNUM
                        ListViewPtr.SetItemText using result,MCOMP,1
                endif
        endif
        setfocus EditPtr
        return

SuppressRemoveFromList LRoutine ListViewPtr
        loop
.Since Deletion will reorder the object, search for any selected items.
                move    SEQ,result
                move    result,N9
                ListViewPtr.GetNextItem giving result using C2,N9
                until (result = SEQ)
                ListViewPtr.DeleteItem giving howmany using result
        repeat
        return

.START PATCH 1.24 ADDED LOGIC
SuppressEnableDisble LRoutine FrmPtr
.START PATCH 1.5 REPLACED LOGIC
.          if (FrmPtr = C1)
.                    getitem   SuppressCheckExclusive,0,result
.                    if (result = 1)
.                              getitem   SuppressEditFromDate3,0,str10
.                              call      Trim using str10
.                              if (str10 <> "")
.                                        call      SuppressDisableUpper2
.                              else
.                                        getitem   SuppressEditToDate3,0,str10
.                                        call      Trim using str10
.                                        if (str10 <> "")
.                                                  call      SuppressDisableUpper2
.                                        else
.                                                  call      SuppressEnableUpper2
.                                        endif
.                              endif
.                              setitem SuppressCheckExclusive,0,0
.                    else
.                              call      SuppressDisableUpper2
.                              setitem SuppressCheckExclusive,0,1
.                    endif
.
.          else
.                    getitem   SuppressEditFromDate3,0,str10
.                    call      Trim using str10
.                    if (str10 <> "")
.                              call      SuppressDisableUpper2
.                    else
.                              getitem   SuppressEditToDate3,0,str10
.                              call      Trim using str10
.                              if (str10 <> "")
.                                        call      SuppressDisableUpper2
.                              else
.                                        getitem   SuppressCheckExclusive,0,result
.                                        if (result = 1)
.                                                  call      SuppressDisableUpper2
.                                        else
.                                                  call      SuppressEnableUpper2
.                                        endif
.                              endif
.                    endif
.          endif
          if (FrmPtr = C1)
                    getitem   SuppressCheckExclusive,0,result
                    if (result = 1)
                              setitem SuppressCheckExclusive,0,0
                    else
                              setitem SuppressCheckExclusive,0,1
                    endif

          else
                    getitem   SuppressEditFromDate3,0,str10
                    call      Trim using str10
                    if (str10 <> "")
                              setitem SuppressCheckExclusive,0,1
                    else
                              getitem   SuppressEditToDate3,0,str10
                              call      Trim using str10
                              if (str10 <> "")
                                        setitem SuppressCheckExclusive,0,1
                              endif
                    endif
          endif
.END PATCH 1.5 REPLACED LOGIC
          return
.END PATCH 1.24 ADDED LOGIC
.................................................
................GUI HOUSEKEEPING.................
.................................................
OptionsTabClick
        if (N2 = C1)
                setprop Options1Coll,visible=0
        elseif (N2 = C2)
.                setprop Options2Coll,visible=0
        endif
        return

OptionsTabChange
        if (N2 = C1)
                setprop Options1Coll,visible=1
        elseif (N2 = C2)
.                setprop Options2Coll,visible=1
        endif
        return

SuppressSetMouseBusy
        setmode *mcursor=*wait
        return
SuppressSetMouseFree
        setmode *mcursor=*arrow
        return
XRESIZE
           NSPR0001.Scale
           RETURN



.Include IO
        include nordio.inc
;Patch1.3
                              include   compio.inc
                              include   cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
;Patch1.3
        include ndatio.inc
        include nrtnio.inc
        include ncmpio.inc
.START PATCH 1.22 ADDED LOGIC
          include   nownio.inc
.END PATCH 1.22 ADDED LOGIC
        include ncntio.inc
        include nuseio.inc
        include searchio.inc      .contains logic for search.plf
        include comlogic.inc

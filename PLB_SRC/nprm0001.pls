          include   common.inc
PC        EQU       1
          include   cons.inc
          include   winapi.inc
          include   ndatdd.inc
        include nusedd.inc
.START PATCH 1.2 ADDED LOGIC
          include   nprmdd.inc
          include   gnxtdd.inc
                    include             hp.inc
          include PRTPAGEDD.INC
.END PATCH 1.2 ADDED LOGIC
................................................
.EXTERNAL ROUTINES FROM NPRM0002.PLC
PromoPrintFile external "NPRM0002;PromoPrintFile"
.Does not appear to have been used since at least 2006 (last time indices where updated until 2013 aug 12 when I updated and pointed to correct servers) DLH
release  init      "1.35"       09AUG2004         ASH       Logo Conversion
.release  init      "1.34"       10FEB2004        DMB       Added code to ask whether they want a cover page or not for weekly datacard
.release  init      "1.33"       11NOV2003        DMB       Changed back copy cmd from print cmd when sending to nts2\fax on xp machines
.release  init      "1.32"       13Jan2003        DMB       Changed copy cmd to print cmd when sending to nts2\fax on xp machines
.                                                           ASH       ADDED BINARY OPTION FOR COPY COMMANDS
.                                                           ASH       Changed copy cmd to print cmd when sending to nts2\fax on xp machines - FOR PROMOS
;release  init      "1.31"       11Sep2002        ASH       Increased Timer per request of NP
.release  init      "1.3"       08Apr2002         DLH       Added 3rd list view on Maint page
.release  init      "1.21"       18Mar2002        ASH       COMBINED LOGIC WITH PROGRAM BY DH
.release  init      "1.2"       18Mar2002         ASH       DH REVAMPED FILES
.release  init      "1.1"       04FEB2002         ASH       SHUTDOWN WILL CLOSE DOWN MASTER MENU - MUST USE STOP!!
.release  init      "1.0"       13DEC01 ASH       DEVELOPMENT RELEASE

.tempfile file
tempfile2 file
prfile    pfile
.START PATCH 1.2 REMOVED LOGIC
.PhoneNum dim       20
.Name     dim       29
.CompName dim       50
.END PATCH 1.2 REMOVED LOGIC
CurRec    form    5.2
CurVal    form      3
LastVal   form      3
RecVal    form      9

DimPtr    dim       ^
DimPtr1   dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
ExitFlag init       "Y"
ReturnFlag dim      1
TIMER     timer
.hexeight integer 4,"4294967295"
userlogn dim    7
DefTest   form      "1"
DefDate   dim       10
DefDate2 form       5

interp    init      "\\nins1\e\apps\plb\code\plbwin.exe"
reformat init       "\\nins1\e\apps\plb\code\reformat"
CopyVar   dim       500
CopyVar2 dim        5000

.Colors
white   color
grey    color
.START PATCH 1.21 ADDED VARS
RED       COLOR
BLACK     COLOR
Yellow    Color
.END PATCH 1.21 ADDED VARS

.Set Up Menu Bar
mFile   menu
mEdit   menu
.START PATCH 1.21 REMOVED VARS
.mOptions menu
.mReports menu
.END PATCH 1.21 REMOVED VARS
mHelp   menu
.Menu Bar for Report Screen
mRSearch menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
.START PATCH 1.21 REMOVED LOGIC
.OData   init    "&Options;&Search-F2"
.RData   init    "&Reports;"
.END PATCH 1.21 REMOVED LOGIC
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&List;&Mailer"

.Set Vars used for About Box
        move    "NPRM0001.PLS",Wprognme
        move    "Promotion Send Program",Wfunction
        move    "Andrew Harkins",Wauthor
        move    release,Wrelease
        move    "September 11, 2002",Wreldate

.START PATCH 1.21 ADDED VARS
.ErrorMssgEditTextBox that is created and destroyed dynamically
.Note that this EditText fills spot for ErrorMssgStat4!!
ErrorMssgEdit1  EditText
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
Dim20          dim            20
Area           Dim            3                .Hold Area code
.
endindex form      9

newNum         Dim            13
Mode           dim            1           A=add M=modify
SlctInfo       Dim            50          .select info for sort
SortInfo       Dim            50             .sort info for sort
SortVar        Dim            300
Tabnum         form          2
SaveTab        form          2
ListSlctCount  Form           2
WeeklySw       Dim            1
PromoSw        Dim            1
BrokerSw       Dim            1
ClientSw       Dim            1
Page           Form             4
ReportTitle    Dim            50
SortFlag       Form           1           1=primary sort by Comp, 2=Primary sort by Contact
.Patch1.34
yesno   integer  1,"0x000004"
.Patch1.34
.

PromoLoadLV         form      "0"
.END PATCH 1.21 ADDED VARS
.one     plform  Search
mss1    plform  Error
abt     plform  About
x       plform  Nprm0001
.START PATCH 1.21 ADDED LOGIC
Nprm001A plform  Nprm001A
Nprm001B plform  Nprm001B
Nprm001C plform  Nprm001C
.END PATCH 1.21 ADDED LOGIC
          winhide
          formload x
          formload abt
          formload mss1
.START PATCH 1.21 ADDED LOGIC
          formload Nprm001A,Nprm0001
          formload Nprm001B,Nprm0001
          formload Nprm001C,Nprm0001
.END PATCH 1.21 ADDED LOGIC

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
.START PATCH 1.21 ADDED LOGIC
        create  RED=*RED
        create  Yellow=*Yellow
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10
.START PATCH 1.35 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.35 ADDED LOGIC
.
        Prm002ListView001.InsertColumn using "Company",160,1
        Prm002ListView001.InsertColumn using "Contact",140,2
        Prm002ListView001.InsertColumn using "Fax ##",140,3
        Prm002ListView001.InsertColumn using "ID Num",40,4
.
        Prm002ListView002.InsertColumn using "Contact",160,1
        Prm002ListView002.InsertColumn using "Company",140,2
        Prm002ListView002.InsertColumn using "Fax ##",140,3
        Prm002ListView002.InsertColumn using "ID num",40,4
.
        Prm002ListView003.InsertColumn using "Fax ##",140,1
        Prm002ListView003.InsertColumn using "Contact",160,2
        Prm002ListView003.InsertColumn using "Company",140,3
        Prm002ListView003.InsertColumn using "ID num",40,4
.END PATCH 1.21 ADDED LOGIC

.Create font used for dynamic resizing of objects
.         create  font5,"Arial",size=12
.Timer creation
.START PATCH 1.31 REPLACED LOGIC
.        CREATE  TIMER,18000     .30 minutes
        CREATE  TIMER,36000     .60 minutes
.END PATCH 1.31 REPLACED LOGIC
        ACTIVATE TIMER,Timeout,RESULT

.Create Menus
          create  Nprm0001;mFile,FData
          create  Nprm0001;mEdit,EData,mFile
.START PATCH 1.21 REPLACED LOGIC
.         create  Nprm0001;mOptions,OData,mEdit
.         create  Nprm0001;mReports,RData,mOptions
.         create  Nprm0001;mHelp,HData,mReports
.Create SubMenu
.        create  Nprm0001;sSearch,SData,mOptions,1
          create  Nprm0001;mHelp,HData,mEdit
.END PATCH 1.21 REPLACED LOGIC

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
.START PATCH 1.21 REMOVED LOGIC
.        activate mOptions,OptionsGo,result
.        activate mReports,ReportGo,result
.END PATCH 1.21 REMOVED LOGIC
        activate mHelp,HelpGo,result

.Activate SubMenus
.        activate sSearch,SearchGo,result

          move    C0,NUSEFLD
          move    C1,NUSEPATH
          move    PORTN,NUSEFLD
          rep     zfill,NUSEFLD
          call    NUSEKEY
          reset   NUSEUSER
..Find out system information
.         getinfo system,str6
.         unpack  str6 into str1,str1
.         move    C0,osflag
.       if (str1 = "3" or str1 = "4")           .95/98
.                   move    C1,osflag
.         elseif (str1 = "1"or str1 = "5")        .NT4/NT5
.                   move    C2,osflag
.         endif
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
.
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          pack      DefDate,MM,SLASH,DD,SLASH,CC,YY
          setitem   PromoEditDate,0,DefDate
          call      CVTJUL
          move      JULDAYS,DefDate2
.
          setfocus PromoComboJob
.
          loop
                    waitevent
.START PATCH 1.31 REPLACED LOGIC
.                setitem timer,0,18000   .reset to 30 minutes
                setitem timer,0,36000   .reset to 60 minutes
.END PATCH 1.31 REPLACED LOGIC
        repeat

Timeout
.Test to make sure nothing is left open in Modify Mode
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
.START PATCH 1.1 REPLACED LOGIC
.         shutdown
          stop
.END PATCH 1.1 REPLACED LOGIC
EditGo
          return
OptionsGo
SearchGo
        branch  result to SearchGo2,SearchGo3
SearchGo2
.LIST
.        move    C2,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchGo3
.MAILER
.        move    C3,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
..LIST
.         unpack  Srchstr,str6,str1,str35
.         if (ClientFlag = 0)
.                   setitem Proj2EditClient,0,str6
.                   setitem   Proj2ComboSource,0,3
.                   setitem   Proj2ComboType,0,1
.                setfocus Proj2EditClient
.        else
.                setitem Proj2Edit2Client,0,str6
.                   setitem   Proj2Stat2ClientName,0,str35
.                   setitem   Proj2Combo2Source,0,3
.                setfocus Proj2Edit2Client
.        endif
.        return
SearchLoad3
.MAILER
.        unpack  Srchstr,str4,str1,str3,str1,str45
.         if (ClientFlag = 0)
.                   setitem Proj2EditClient,0,str4
.                   setitem   Proj2ComboSource,0,2
.                setfocus Proj2EditClient
.        else
.                setitem Proj2Edit2Client,0,str4
.                   setitem   Proj2Stat2ClientName,0,str45
.                   setitem   Proj2Combo2Source,0,2
.                setfocus Proj2Edit2Client
.        endif
.        return
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

PromoDisableItems
          move      NO,ExitFlag
          setprop   PromoOK,enabled=0
          setprop   PromoExit,enabled=0
          setprop   PromoAdd,enabled=0
          setprop   PromoRemove,enabled=0
          setprop   PromoSearch,enabled=0
          setprop   PromoComboJob,enabled=0,bgcolor=grey
          setprop   PromoCheckTest,enabled=0
          setprop   PromoDataFile,enabled=0,bgcolor=grey
          setprop   PromoEditDate,enabled=0,bgcolor=grey
          setprop   PromoEditFile,enabled=0,bgcolor=grey
          return

PromoEnableItems
          setprop   PromoOK,enabled=1
          setprop   PromoExit,enabled=1
          setprop   PromoAdd,enabled=1
          setprop   PromoRemove,enabled=1
          setprop   PromoSearch,enabled=1
          setprop   PromoComboJob,enabled=1,bgcolor=white
          setprop   PromoCheckTest,enabled=1
          setprop   PromoEditDate,enabled=1,bgcolor=white
          setprop   PromoDataFile,enabled=1,bgcolor=white
          setprop   PromoEditFile,enabled=1,bgcolor=white
          move      YES,ExitFlag
          return

.START PATCH 1.21 ADDED LOGIC
DisplayDetail
          call      PrmKey
          setitem   Prm002EditText001,0,PrmCoName
          setitem   Prm002EditText002,0,PrmCntName
          clear     newnum
          unpack    PrmPhNum,str2,area,str1,str3,str1,str4
          pack      Newnum,"(",area,")",str3,Dash,str4
.
          setitem   Prm002EditText003,0,Newnum
          if (PrmWeekly = YES)
                    setitem   Prm002Radio001,0,1
          else
                    setitem   Prm002Radio001,0,0
          endif
          if (PrmPromo = YES)
                    setitem   Prm002Radio002,0,1
          else
                    setitem   Prm002Radio002,0,0
          endif
          if (PrmBroker = YES)
                    setitem   Prm002Radio003,0,1
          else
                    setitem   Prm002Radio003,0,0
          endif
          if  (PrmClient = YES)
                    setitem   Prm002Radio004,0,1
          else
                    setitem   Prm002Radio004,0,0
          endif
          return

PromoClearDetail
          setitem   Prm002EditText001,0,""
          setitem   Prm002EditText002,0,""
          setitem   Prm002EditText003,0,""
          setitem   Prm002Radio001,0,0
          setitem   Prm002Radio002,0,0
          setitem   Prm002Radio003,0,0
          setitem   Prm002Radio004,0,0
          return
.................................................................................

DataVerf
.the only thing we are really going to verify is that we have a good fax number and a contact or company
.after verify passes we will update the record - unless in add mode
          getitem   Prm002EditText001,0,PrmCoName
          getitem   Prm002EditText002,0,PrmCntName
          getitem   Prm002EditText003,0,Dim20
          move      "(",str1
          call      RemoveChar using dim20,str1
          move      ")",str1
          call      RemoveChar using dim20,str1
          move      "-",str1
          call      RemoveChar using dim20,str1
          clear     Newnum
          count     N2,Dim20
          if (N2 = "10")
                    unpack    dim20,area,str3,str4
                    pack      Newnum,"(",area,")",str3,Dash,str4
                    setitem   Prm002EditText003,0,Newnum
                    clear     PrmPhnum
                    pack      PrmPHNum from c1,dash,area,dash,str3,dash,str4
          else
                    alert     caution,"10 Digit Phone Number Required!",result
                    setfocus Prm002EditText003
                    return
          endif
          if (PrmCoName = "" & PrmCntName = "")
                    alert     caution,"A company or contact name required!",result
                    setfocus Prm002EditText001
                    return
          endif
.
          getitem   Prm002Radio001,0,N1
          if (N1 = C1)
                    move      YES,PrmWeekly
          else
                    clear     PrmWeekly
          endif
.
          getitem   Prm002Radio002,0,N1
          if (N1 = C1)
                    move      YES,PrmPromo
          else
                    clear     PrmPromo
          endif
.
          getitem   Prm002Radio003,0,N1
          if (N1 = C1)
                    move      YES,PrmBroker
          else
                    clear     PrmBroker
          endif
.
          getitem   Prm002Radio004,0,N1
          if (N1 = C1)
                    move      YES,PrmClient
          else
                    clear     PrmClient
          endif
.
          If (Mode = "A")
                    move      "PROMONXT",gnxtfld
                    call      Gnxtkey
                    move      GNXTNUM,Prmfld
testnum
                    rep       zfill,prmfld
                    call      PRMTST
                    if over
                              move      Prmfld,PrmIdNum
                              call      prmwrt
                              move      gnxtnum,N6
                              add       C1,N6
                              move      N6,gnxtnum
                              call      Gnxtupd
                    else
                              add       C1,N6
                              move      N6,Gnxtnum
                              move      GNXTNUM,Prmfld
                              goto      testnum
                    endif
          else
                    call      prmupd
                    call      Findit
          endif
          move      "M",mode
          call      UpdateListView
          setProp   Prm002Add,visible=1
          setProp   Prm002Remove,visible=1
          setProp   Prm002Quit,visible=0
          return
.................................................................................
UpdateListView
.insert the updated item into all listviews
          Prm002ListView001.InsertItem giving N9 using PrmCoName
          Prm002ListView001.SetItemText using N9,PrmCntName,1
          Prm002ListView001.SetItemText using N9,prmPhNum,2
          Prm002ListView001.SetItemText using N9,PrmIdnum,3
          Prm002ListView001.EnsureVisible using N9,0
.
          Prm002ListView002.InsertItem giving N9 using PrmCntName
          Prm002ListView002.SetItemText using N9,PrmCoName,1
          Prm002ListView002.SetItemText using N9,prmPhNum,2
          Prm002ListView002.SetItemText using N9,PrmIdnum,3
          Prm002ListView002.EnsureVisible using N9,0
.
          Prm002ListView003.InsertItem giving N9 using prmPhNum
          Prm002ListView003.SetItemText using N9,PrmCntName,1
          Prm002ListView003.SetItemText using N9,PrmCoName,2
          Prm002ListView003.SetItemText using N9,PrmIdnum,3
          Prm002ListView003.EnsureVisible using N9,0
          return
*..............................................................................
PrmSortListView Routine FrmPtr,FrmPtr1
.Dynamically sorts Different ListViews.
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
          if (FrmPtr = 1)               .Prm002ListView001
                    if (result = 1)
                              setprop Prm002ListView001,height=0
                              setprop Prm002ListView003,height=0
                              setprop Prm002ListView002,height=135
                              setfocus Prm002ListView002
                              call      Click_Prm002ListView002
                              elseIf  (result = 2)
                              setprop Prm002ListView001,height=0
                              setprop Prm002ListView002,height=0
                              setprop Prm002ListView003,height=135
                              setfocus Prm002ListView003
                              call      Click_Prm002ListView003
                              endif
.         else                          .Prm002ListView002
          elseif  (frmptr = 2)                              .Prm002ListView002
                    if (result = 1)
                              setprop Prm002ListView002,height=0
                              setprop Prm002ListView003,height=0
                              setprop Prm002ListView001,height=135
                              setfocus Prm002ListView001
                              call      Click_Prm002ListView001
                    ElseIf (result = 2)
                              setprop Prm002ListView002,height=0
                              setprop Prm002ListView001,height=0
                              setprop Prm002ListView003,height=135
                              setfocus Prm002ListView003
                              call      Click_Prm002ListView003
                              endif
               else                                         .prm002ListView003
                    if (result = 1)
                              setprop Prm002ListView003,height=0
                              setprop Prm002ListView001,height=0
                              setprop Prm002ListView002,height=135
                              setfocus Prm002ListView002
                              call      Click_Prm002ListView002
                    ElseIf (result = 2)
                              setprop Prm002ListView003,height=0
                              setprop Prm002ListView002,height=0
                              setprop Prm002ListView001,height=135
                              setfocus Prm002ListView001
                              call      Click_Prm002ListView001
                              endif
          endif
          return
*..............................................................................
RestoreFocus
.what view are we using?
          getprop   Prm002ListView001,height=N8
          if (N8 = 0)                    .not visible
                         getprop        Prm002ListView002,height=N8
                         if (N8 = 0)                    .not visible
                    setfocus Prm002ListView003
                    call      Click_Prm002ListView003
                              else
                    setfocus Prm002ListView002
                    call      Click_Prm002ListView002
                              endif
          else
                    setfocus Prm002ListView001
                    call      Click_Prm002ListView001
          endif
          return
..................................................................................
Findit
          call      findit1
          call      findit2
          call      findit3
          return
...............................................................................
.findit 1 find the modified item in listview
findit1
          Prm002ListView001.getitemcount  giving endindex
          add       C1,endindex
          move      C0,N9
findit1Lookup
          Prm002ListView001.GetItemText giving str5 using N9,3
          match     PrmIDNum,str5
          if equal
                    Prm002ListView001.DeleteItem Using N9
          else
                    add       C1,N9
                    if (N9 <  endindex)
                              goto findit1Lookup
                              return
                    endif
          endif
          return

.findit 2 find the modified item in listview2
findit2
          Prm002listview002.getitemcount  giving endindex
          add       C1,endindex
          move      C0,N9
findit2Lookup
          Prm002ListView002.GetItemText giving str5 using N9,3
          match     PrmIdnum,str5
          if equal
                    Prm002ListView002.DeleteItem Using N9
          else
                    add       C1,N9
                    if (N9 <  endindex)
                              goto findit2Lookup
                              return
                    endif
          endif
          return
.findit 3 find the modified item in listview2
findit3
          Prm002listview003.getitemcount  giving endindex
          add       C1,endindex
          move      C0,N9
findit3Lookup
          Prm002ListView003.GetItemText giving str5 using N9,3
          match     PrmIdnum,str5
          if equal
                    Prm002ListView003.DeleteItem Using N9
          else
                    add       C1,N9
                    if (N9 <  endindex)
                              goto findit3Lookup
                              return
                    endif
          endif
          return
*..............................................................................

PRMUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   Prm002ProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
PRMInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return

PrintPrep
.get and verify request
.sort file
.Invoke printer dialogue
.print
.First what records
          move      C0,ListSlctCount
          getitem   Prm003Radio001,0,result
          if (result = C1)
                    move      YES,WeeklySw           .Weekly datacard list is selected
                    add       C1,ListSlctCount       .track number of lists selected must be at least 1
          else
                    move      NO,WeeklySw
          endif
.
          getitem   Prm003Radio002,0,result
          if (result = c1)
                    move      YES,PromoSw           .Weekly datacard list is selected
                    add       C1,ListSlctCount       .track number of lists selected must be at least 1
          else
                    move      NO,PromoSw
          endif
.
          getitem   Prm003Radio003,0,result
          if (result = c1)
                    move      YES,BrokerSw           .Weekly datacard list is selected
                    add       C1,ListSlctCount       .track number of lists selected must be at least 1
          else
                    move      NO,BrokerSw
          endif
.
          getitem   Prm003Radio004,0,result
          if (result = c1)
                    move      YES,ClientSw           .Weekly datacard list is selected
                    add       C1,ListSlctCount       .track number of lists selected must be at least 1
          else
                    move      NO,ClientSw
          endif
.
          if (ListSlctCount < c1)
                    alert     caution,"You must select at least one list to print!",result
                    setfocus Prm003Radio001
                    return
          endif
.
.               if             (ListSlctCount > c1)
.              alert          caution,"You can't select more than one list to print!",result
.               setfocus       Prm003Radio001
.               return
.               endif
.
.build select for sort
          clear     SlctInfo
          if (WeeklySw = Yes & ListSlctCount = C1)
                    append    "S112=Y",Slctinfo
          elseif (WeeklySw = Yes )
                    append    "S=#"112='Y'",Slctinfo                            ."
          endif
          if (weeklySw = Yes & PromoSw = Yes)
                    append    "|113='Y'",Slctinfo
          elseif (PromoSw = Yes)
                    append    "S113=Y",Slctinfo
          endif
          if ((weeklySw = Yes | PromoSw = Yes) & BrokerSw = Yes)
                    append    "|114='Y'",Slctinfo
          elseif (BrokerSw = Yes)
                    append    "S114=Y",Slctinfo
          endif
          if ((weeklySw = Yes | PromoSw = Yes | BrokerSw = Yes) & ClientSw = Yes)
                    append    "|115='Y'",Slctinfo
          elseif (ClientSw = Yes)
                    append    "S115=Y",Slctinfo
          endif
          if (ListSlctCount > C1)
                    append    "#"",SlctInfo
          endif
          reset     SlctInfo
.build sort info for sort
          clear     Sortinfo
          getitem   Prm003Radio005,0,result
          if (result = C1)           .primary sort by Company name
                    move      C1,SortFlag
                    append    ",56-105",sortinfo
          endif
          getitem   Prm003Radio006,0,result
          if (result = C1)           .primary sort by Contact name
                    move      C2,SortFlag
                    append    ",21-55",sortinfo
          endif
          getitem   Prm003Radio007,0,result
          if (result = C1)           .2nd sort by Contact name
                    append    ",21-55",sortinfo
          endif
          getitem   Prm003Radio008,0,result
          if (result = C1)           .2nd sort by Company name
                    append    ",56-105",sortinfo
          endif
          reset     sortinfo
          clear     SortVar
          append    "\\nins1\e\data\text\Faxpromo.dat,c:\work\Faxpromo.srt;",sortvar
.               append         "c:\data\Faxpromo.dat,c:\work\Faxpromo.srt;" to sortvar
          append    SlctInfo,SortVar
          append    SortInfo,SortVar
          reset     SortVar
          sort      SortVar
          if over
                    alert     Caution,Sortvar,result
                    alert     caution,S$ERROR$,result
                    return
          endif
...................
.ok lets print
.starting with a single list report and adding as we go.
          prtopen   Laser,"",WPrognme
          clock     DATE,Today
          unpack    Today,mm,str1,dd,str1,yy
          open      tempfile2,"c:\work\faxpromo.srt",exclusive
          prtpage   Laser;*UNITS=*HIENGLISH;
          prtpage   Laser;*ORIENT=*Portrait;
          if (ListSlctCount = c1)
                    if (WeeklySw = Yes)
                              move      "Fax List - Weekly Datacard",ReportTitle
                    elseif (PromoSw = Yes)
                              move      "Fax List - Promotional",ReportTitle
                    elseif (BrokerSw = Yes)
                              move      "Fax List - Broker",ReportTitle
                    elseif (ClientSw = Yes)
                              move      "Fax List - Client/Prospect",ReportTitle
                    endif
          else
                    move      "Fax Lists Report - Multilist select",ReportTitle
          endif
          call      Header
PrintLoop
          read      tempfile2,seq;Prmvars
          goto PrintExit if over
.check line count - whaterver and call header
          if (row >= 10000)
                    if (ListSlctCount = C1)
                              prtpage   Laser;*p=1:10450,"Page ",page
                    else
                              prtpage   Laser;*p=1:10450,"Page ",page,*p=2500:10500,"'W'=Weekly Datcards, 'P'=Promotional, ":
                                        "'B'=Broker, 'C'=Client/Prospect"
                    endif
                    prtpage   Laser;*NEWPAGE;
                    call      header
          endif
.
          unpack    PrmPhNum,str2,area,str1,str3,str1,str4
          pack      Newnum,"(",area,")",str3,Dash,str4
          if (ListSlctCount = C1)
                    if (SortFlag = C1)
                              prtPage   Laser;*p=1:Row,*font=PRTpg10,PrmCoName,*p=4500:row,PrmCntName,*p=7000:row,newnum
                    elseif (SortFlag = C2)
                              prtPage   Laser;*p=1:Row,*font=PRTpg10,PrmCntName,*p=4500:row,PrmCoName,*p=7000:row,newnum
                    endif
                    add       "200",row
          else
                    if (SortFlag = C1)
                              prtpage   Laser;*p=1:Row,*font=PRTpg10,PrmCoName,*p=3000:row,PrmCntName,*p=5500:row,newnum
                    elseif (SortFlag = c2)
                              prtpage   Laser;*p=1:Row,*font=PRTpg10,PrmCntName,*p=3000:row,PrmCoName,*p=5500:row,newnum
                    endif
                    if (PrmWeekly = YES)
                              prtpage   Laser;*p=7000:row,"W"
                    endif
                    if (PrmPromo = YES)
                              prtpage   Laser;*p=7150:row,"P"
                    endif
                    if (PrmBroker = YES)
                              prtpage   Laser;*p=7250:row,"B"
                    endif
                    if (PrmClient = YES)
                              prtpage   Laser;*p=7350:row,"C"
                    endif

                    add       "200",row
          endif
          goto            PrintLoop
PrintExit
          if (ListSlctCount = C1)
                    prtpage   Laser;*p=1:10450,"Page ",page
          else
                    prtpage   Laser;*p=1:10450,"Page ",page,*p=2500:10500,"'W'=Weekly Datcards, 'P'=Promotional, ":
                              "'B'=Broker, 'C'=Client/Prospect"
          endif
          prtclose Laser
          close     tempfile2
          return
Header
.START PATCH 1.35 REPLACED LOGIC
.         prtpage   Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
.                   *p=25:102,*font=PRTpg10B,"Names  ":
.                   *font=PRTpg10i," in the News":
.                   *p=5:240,*Line=1260:240:
.                   *p=10:250,*font=PRTpg10,"C a l i f o r n i a    I n c .":
.                   *p=3500:250,ReportTitle,*p=7200:250,Today
.         if (ListSlctCount = C1)
.                   if (SortFlag = C1)
.                             prtpage   Laser;*p=50:525,*font=PRTpg10B,"Company",*p=4550:525,"Contact",*p=7000:525,"Fax Number"
.                   elseif (SortFlag = C2)
.                             prtpage   Laser;*p=50:525,*font=PRTpg10B,"Contact",*p=4550:525,"Company",*p=7000:525,"Fax Number"
.                   endif
.         else
.                   if (SortFlag = C1)
.                             prtpage   Laser;*p=50:525,*font=PRTpg10B,"Company",*p=3050:525,"Contact",*p=5500:525,"Fax Number"
.                   elseif (SortFlag = c2)
.                             prtpage   Laser;*p=50:525,*font=PRTpg10B,"Contact",*p=3050:525,"Company",*p=5500:525,"Fax Number"
.                   endif
.         endif
.         move      "800",row
............................
          prtpage   Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
                    *p=3500:250,ReportTitle,*p=7200:250,Today
          prtpage   Laser;*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
          if (ListSlctCount = C1)
                    if (SortFlag = C1)
                              prtpage   Laser;*p=50:925,*font=PRTpg10B,"Company",*p=4550:925,"Contact",*p=7000:925,"Fax Number"
                    elseif (SortFlag = C2)
                              prtpage   Laser;*p=50:925,*font=PRTpg10B,"Contact",*p=4550:925,"Company",*p=7000:925,"Fax Number"
                    endif
          else
                    if (SortFlag = C1)
                              prtpage   Laser;*p=50:925,*font=PRTpg10B,"Company",*p=3050:925,"Contact",*p=5500:925,"Fax Number"
                    elseif (SortFlag = c2)
                              prtpage   Laser;*p=50:925,*font=PRTpg10B,"Contact",*p=3050:925,"Company",*p=5500:925,"Fax Number"
                    endif
          endif
          move      "1200",row
.END PATCH 1.35 REPLACED LOGIC
          add       C1,page
.add code for single or multiple list headers
          return
.END PATCH 1.21 ADDED LOGIC
..................Windows Housekeeping..................
PromoUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/RecVal)*100)
          if (CurVal <> LastVal)
                    setitem   PromoProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return

PromoInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          move      C0,RecVal
          setitem   PromoStatProgress,0,""
          return

.START PATCH 1.21 ADDED LOGIC
PromoTabClick
          if (N1 = C1)
                    Deactivate NPrm001A
          elseif (N1 = C2)
                    Deactivate NPrm001B
          elseif (N1 = C3)
                    Deactivate NPrm001C
          endif
        return

PromoTabChange
          if (N1 = C1)
                    Activate Nprm001A
                    move      C1,TabNum
          elseif (N1 = C2)
                    if (PromoLoadLV = C0)
                              call      PromoLoadListView
                    endif
                    Activate Nprm001b
                    move      C2,TabNum
          elseif (N1 = C3)
                    Activate Nprm001c
                    move      C3,TabNum
                setitem       Prm003Radio005,0,1
                setitem       Prm003Radio007,0,1
          endif
        return

PromoLoadListView
          setprop   Prm002ProgressBar,visible=1
          call      PrmInitProgressBar
          move      "\\nins1\e\data\text\FaxPromo.dat",str45
          open      tempfile2,str45
          positeof tempfile2
          fposit    tempfile2,N10
          calc      howmany=(N10/127)   .'127 = 125(NIPrmST record length) + 2 bytes for CR/LF
          close     tempfile2
.
          move      "Driver Prep-NPrmSEQ",Location
          open      PrmfList
          clearevent
          loop
                    read      Prmfile,seq;Prmvars
                    until over
.Update Progress Bar
                    call      PrmUpdateProgressBar
................................
                    Prm002ListView001.InsertItem giving N9 using PrmCoName
                    Prm002ListView001.SetItemText using N9,PrmCntName,1
                    Prm002ListView001.SetItemText using N9,prmPhNum,2
                    Prm002ListView001.SetItemText using N9,PrmIdnum,3
                    Prm002ListView001.EnsureVisible using N9,0
.
                         Prm002ListView003.InsertItem giving N9 using prmPhNum
                         Prm002ListView003.SetItemText using N9,PrmCntName,1
                         Prm002ListView003.SetItemText using N9,PrmCoName,2
                         Prm002ListView003.SetItemText using N9,PrmIdnum,3
                         Prm002ListView003.EnsureVisible using N9,0
.
                    Prm002ListView002.InsertItem giving N9 using PrmCntName
                    Prm002ListView002.SetItemText using N9,PrmCoName,1
                    Prm002ListView002.SetItemText using N9,prmPhNum,2
                    Prm002ListView002.SetItemText using N9,PrmIdnum,3
                    Prm002ListView002.EnsureVisible using N9,0
          repeat
.cleanup
          loop
                    clearevent
                    until over
          repeat
          Prm002ListView002.EnsureVisible using 0,0
          Prm002ListView002.SetItemState giving N9 using 0,2,2
          setfocus Prm002ListView002
          call      Click_Prm002ListView002
          move      C1,PromoLoadLV
          return
.END PATCH 1.21 ADDED LOGIC

        include nuseio.inc
          include   ndatio.inc
.        include searchio.inc      .contains logic for search.plf
.START PATCH 1.2 ADDED LOGIC
          include   nprmio.inc
          include   gnxtio.inc
.END PATCH 1.2 ADDED LOGIC

          include   comlogic.inc

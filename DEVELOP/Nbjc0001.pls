.Names in the News batch Job Controller
.NBJC0001
PC        Equ       0
          Include   Common.inc
          Include   Cons.inc
          include   Nbjcdd.inc
Release   init      "PRE"
RelDate   Init      "06 June 2012"
GoFlag    Init      "Y"    .yes



abt       plform  About
x         plform  NBJC0001
DimPtr    dim       ^
DimPtr1    dim       ^

.Menu
.Set Up Menu Bar
mFile     menu
mEdit     menu
mOptions  menu
mHelp     menu
FData     init    "&File;E&xit"
EData     init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData     init    "&Options;&Search;-;&Preferences"
HData     init    "&Help;&About"
Timer     Timer
          winhide

          formload  x
          formload  abt

          CREATE    TIMER,100     .10 seconds
          ACTIVATE  TIMER,Checker,RESULT


          create    NBjc0001;mFile,FData
          create    NBjc0001;mEdit,EData,mFile
          create    NBjc0001;mOptions,OData,mEdit
          create    NBjc0001;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
          activate  mFile,FileGo,result
.Need this when it works
          activate  mEdit,EditGo,result
          activate  mOptions,OptionsGo,result
          activate  mHelp,HelpGo,result
          
          EVENTREG  X, 17, XRESIZE

          NBJC0001ListView1.InsertColumn using "Status",20,1
          NBJC0001ListView1.InsertColumn using "Task ID",80,2
          NBJC0001ListView1.InsertColumn using "Received",80,3
          NBJC0001ListView1.InsertColumn using "Submitted",80,4
          NBJC0001ListView1.InsertColumn using "Task Details",150,5

Looper
          SetFocus NBJC0001ListView1

          loop
          waitevent
          setitem   timer,0,100   .reset to 10 seconds
          repeat
          goto      Checker
.................................................................................
LoadListView
          if        (goflag <> yes)
          return
          endif
          Call      NBJCOpen
          loop
          call      NBJCseq
          until     over
          call      trim      using NBJCSubmt
          if        (NBJCSTAT = "W")
          call      Add2sched
          endif
          NBJC0001ListView1.InsertItem giving IN9 using NBJCSTAT
          NBJC0001ListView1.SetItemText using IN9,NBJCKey,1
          NBJC0001ListView1.SetItemText using IN9,NBJCRecvd,2
          NBJC0001ListView1.SetItemText using IN9,NBJCSubmt,3
          NBJC0001ListView1.SetItemText using IN9,NBJCTask,4
          repeat
          Return
.................................................................................
ClearListView
        NBJC0001ListView1.DeleteAllItems Giving IN9
        return
.................................................................................
Add2Sched
          if        (goflag <> yes)
          return
          endif
          clock     timestamp,str15           .get a unique task name
          append    "!schtasks /Create /tn ",Taskname
          append    str15,taskname
          append    " /tr #"",TASKNAME
          call      Trim using NBJCTask
          append    NBJCTask,Taskname
          call      trim using NBJCSrvr
          if        (NBJCSrvr = "")
          Append    "#"  /S VM0001 /sc Once /st ",taskname
          else
          Append    "#"  /S ",taskname
          Append    NBJCSrvr,Taskname
          Append    " /sc Once /st ",taskname
          endif          
          call      calcstarttime
          pack      str5 from Hours,":",minutes
          REP       zFILL IN STR5
          Append    str5,taskname
          append    " /RU nincal\wadmin /RP xnincal /F",taskname
          reset     taskname
          Execute   Taskname
          Move      "W",NBJCstat
          packkey   NBJCfld from NBJCstat,NBJCKey
          call      NBJCkey
          move      str15,NBJCSubmt
          Move      "D",NBJCstat
          call      NBJCUPD
          return
.................................................................................
Checker
          if        (goflag <> yes)
          goto      looper
          endif

          call      ClearListView
          call      LoadListView
          goto      looper
.................................................................................
FileGo
          branch    result to FileGo1
FileGo1
          Shutdown   "CLS"
          RETURN

Optionsgo
          return
EditGo
          return
HelpGo
          setprop   AboutMssg,visible=1
          return
BJCSetMouseBusy
          setmode   *mcursor=*wait
          return
BJCSetMouseFree
          setmode   *mcursor=*arrow
          return

XRESIZE
          NBJC0001.Scale
          RETURN
.................................................................................
.externally called routine  taskname, server name
WriteNBJC Routine   DimPtr,DimPtr1
          clock     timestamp,str15           .get a unique task name
          move      str15,NBJCREcvd
          move      str15,NBJCKey
          Move      "W",NBJCstat
          packkey   NBJCfld from NBJCKey
          packkey   NBJCfld2 from NBJCstat,NBJCKey
          call      trim using dimPtr1
          move      DimPtr1,NBJCSrvr
          MOVe      DimPtr,NBJCTask 
          call      NBJCWRT
          return
.................................................................................
          Include   Nbjcio.inc
          include   comlogic.inc
          
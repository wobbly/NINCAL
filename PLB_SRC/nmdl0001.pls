. .............................................................................
.
. NMDL0001.pls -- List Booking Instructions PROGRAM
.
. .............................................................................
PC       EQU       1      .gui

          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NDATDD.inc
          INCLUDE   NPASDD.inc
          INCLUDE   NDATEDD.inc
          INCLUDE   NXRFDD.inc
          INCLUDE   NSCHDD.inc
.Patch1.5.3
          include   compdd.inc
          include   cntdd.inc
.         INCLUDE   NMLRDD.INC
.patch1.5.3
          INCLUDE   NMDLDD.inc
          include   nusedd.inc
.For Search Screen
          include   norddd.inc
.patch1.5.3
.         include   nbrkdd.inc
.patch1.5.3
          include   nrtndd.inc
          include   nowndd.inc
          include   ncmpdd.inc
release   init    "1.6"   DLH     external call to nmdl0002
          move    "2012",Wreldate
.release   init    "1.5.9"   DLH Sept2007          PLI
.          move    "September 2007",Wreldate

.release  init    "1.5.8"   DLH 11Aug2006         PLB 9.1
.         move    "August 11, 2006",Wreldate
.release  init    "1.5.7"   ASH 18MAY2005         LISTMLR/NINMDLST Conversion
.         move    "May 18, 2005",Wreldate
.release  init    "1.5.6"   ASH 16MAR2005         Patch to fix Delete
.         move    "March 16, 2005",Wreldate
.release  init    "1.5.5"   ASH 24FEB2004         Removed Password protection - per SMM.  Note that no one in Sales could keep track of how Booking
..                                                Instructions were to be modified, and kept coming to Data Entry and I.S. for modifications.  Major problem.
.         move    "February 24, 2005",Wreldate
.release  init    "1.5.4"   ASH 05OCT2004         Increased size of Free Text
.         move    "October 05, 2004",Wreldate
.release  init    "1.5.3"   DMB 05MAY2004         Mailer Conversion
.release  init    "1.5.2"   ASH Added new Checkbox
.         move    "18 May 2004",Wreldate
.release  init    "1.5.1"   ASH Small patch to allow null entry of Planners
..                                      Allow entry of 'X' for Callers/Planners
.         move    "10 May 2004",Wreldate
.release init    "1.5"   ASH 28Apr2004 Rewrite of Program
.        move    "28 April 2004",Wreldate
.                             Overhaul: renamed various variables, added some GUI routines for clarity
.release init    "1.4"   DLH 19Mar02 remove AnimIcon - improve load perf.
.release init    "1.3"   DLH 23Oct01 ADD progress bar
.release init    "1.2"   DLH 22Sep99 verify planner/caller inits
.release init    "1.1"   DLH 16jUL1999  LISTMLR XREF
.release init    "1.0"   DLH 28May1999  DEVELOPMENT RELEASE
.Set Vars used for About Box
        move    "NMDL0001.PLS",Wprognme
        move    "Booking Instructions Program",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
.............................................................................
NMDLPrint           external "NMDL0002;ExtPRint"


.START PATCH 1.5.5 REMOVED LOGIC
.holdpassok dim     1
.holdpassok2 dim    1
.END PATCH 1.5.5 REMOVED LOGIC
Mode      form      1       1=add mode 2=modify
.endindex form      9
HOLD      dim       8
plancallflag form 1
CurRec    form      5.2
CurVal    form      3
LastVal   form      3
ExitFlag init       "Y"
ReturnFlag init     "N"
colordim dim        8
.tempfile file
Timer   Timer
.RevTyp   init      "DH JD AH DB DM CF FU AF SA SM JC SK AL SJA BS BC"          .Used to determine Rights
.START PATCH 1.5.5 REMOVED LOGIC
.RevTyp   init      "DH JD AH DB DM CF FU AF SA SM AL SJA BC"         .Used to determine Rights
.START PATCH 1.5.5 REMOVED LOGIC
RevTyp2   init      "DH JD AH DB DM "                                           .Used to determine Rights
.Vars used for Report Screen
RptCan    dim       1
.
ObjectColl          Collection
StatTextBoxes       StatText (4)
EditTextBoxes       EditText (4)
Buttons   Button (2)
PackData  DataList
OldCal    dim       3
NewCal    dim       3
OldPlan   dim       3
NewPlan   dim       3
.
.Note that this EditText fills spot for ErrorMssgStat4!!
ErrorMssgEdit1  EditText

.Colors
white     color
grey      color
RED       COLOR
BLACK     COLOR
Yellow    Color
blue      color

.Define Fonts to be used
font1     font
font2     font
font3     font
font4     font
font5     font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch  submenu

.Present Data for Menu Bar
FData     init      "&File;&Print;-;E&xit"
EData     init      "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Replace"
HData     init      "&Help;&About"

.Present Data for Search SubMenu
SData   init    ";&List;&Mailer"

.Declare forms, Always declare child forms first
rpt2      plform    Report2
srch    plform  Search
pass      plform    Passwrd
mss1      plform    Error
abt       plform    About
x         plform    Nmdl001a
plfinits plform Nmdl001c
          winhide

.Load Forms, Always load parent form first
          formload x
          formload abt
          formload mss1
          formload pass
          formload plfinits
        formload srch
        formload rpt2
          move      c2 to nusepath
.
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.
.Create Menus
          create  NMDL001a;mFile,FData
          create  nmdl001a;mEdit,EData,mFile
        create  nmdl001a;mOptions,OData,mEdit
          create  nmdl001a;mHelp,HData,mOptions
.Create SubMenus
        create  nmdl001a;sSearch,SData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under these
        activate mOptions,OptionsGo,result
        activate mHelp,HelpGo,result
.Activate SubMenus
        activate sSearch,SearchGo,result

.Create Colors for EditText Inquiry
          create  white=*white
          create    grey=220:220:220
          create  RED=*RED
          create  Yellow=*Yellow
          create  black=*black
        create  blue=*blue

.Create fonts to be used
          create  font1,"Arial",size=12,bold
          create  font2,"Arial",size=8
          create  font3,"Helvetica",size=9
          create  font4,"Arial",size=14,italic
          create  font5,"Arial",size=10
.
          MOVE      C1 TO NXRFPATH         .SET ACCESS TO LIST KEY.
.         move      c1 to nmlrpath
          move      c1 to ndatpath
.         move      c3 to nmlrlock
          move      c3 to ndatlock
.
start
.Load Data List
.Column Clicking
.Check out notes under MDL3ListView_ColumnClick for other options.
.{To implement option #2 references to MDL3ListView2 could be remmed or removed.} ASH
          call      NMDLInitProgressBar
          setitem   NMDLStatProgress,0,""
          setitem   NMDLStatProgress,0,"Loading Instructions"
.begin patch 1.5.8
          move      "\\nins1\e\data\text\NINMDLST.dat|10.10.30.103:502" to taskname
.         move      "\\nins1\e\data\text\NINMDLST.dat" to str45
.         open      tempfile,str45
          open      tempfile,taskname
.end patch 1.5.8
          positeof tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/300)   .'300 = 298(NINMDLST record length) + 2 bytes for CR/LF
          close     tempfile
.
          MDL3ListView.InsertColumn using "Caller",40,1
          MDL3ListView.InsertColumn using "Planner",60,2
          MDL3ListView.InsertColumn using "List Description",280,3
          MDL3ListView.InsertColumn using "List",60,4
          MDL3ListView.InsertColumn using "Mlr",60,5
          MDL3ListView.InsertColumn using "Excl.",60,6
          MDL3ListView.InsertColumnFgClr using *Index=6

          MDL3ListView2.InsertColumn using "Planner",60,1
          MDL3ListView2.InsertColumn using "Caller",40,2
          MDL3ListView2.InsertColumn using "List Description",280,3
          MDL3ListView2.InsertColumn using "List",60,4
          MDL3ListView2.InsertColumn using "Mlr",60,5
          MDL3ListView2.InsertColumn using "Excl.",60,6
          MDL3ListView2.InsertColumnFgClr using *Index=6

          MDL3ListView3.InsertColumn using "List Description",280,1
          MDL3ListView3.InsertColumn using "Planner",60,2
          MDL3ListView3.InsertColumn using "Caller",40,3
          MDL3ListView3.InsertColumn using "List",60,4
          MDL3ListView3.InsertColumn using "Mlr",60,5
          MDL3ListView3.InsertColumn using "Excl.",60,6
          MDL3ListView3.InsertColumnFgClr using *Index=6

          NMDL3001cListView001.InsertColumn using "Contact Name",180,1
          NMDL3001cListView001.InsertColumn using "Initials",60,2

        deleteitem MDL3ComboReport,0
          insertitem MDL3ComboReport,1," "
          insertitem MDL3ComboReport,2,"Galley"
          insertitem MDL3ComboReport,3,"LCR"
          insertitem MDL3ComboReport,4,"Galley-2"

          move      "Driver-NmdlSEQ",Location
.Dynamically reset Animate as same size as NMDL001A
anim1
          getprop NMDL001A,height=H
          getprop NMDL001A,width=V
          loop
                    call      NmdlSEQ
                    until over
.Update Progress Bar
                    call      NMDLUpdateProgressBar
                    call      BookingAddItems
          repeat
          call      NMDLInitProgressBar
          setitem   NMDLStatProgress,0,""
          setitem   NMDLStatProgress,0,"Creating Views"
          move      "\\nins1\e\data\text\NINuser.dat" to str45
.         open      tempfile,str45
.         positeof tempfile
.         fposit    tempfile,N10
          calc      howmany=(N10/44)    .'44 = 42(NINuser record length) + 2 bytes for CR/LF
.         close     tempfile
.
          move      "Driver-NuseSEQ",Location
        loop
                    call      NuseSEQ
                    until over
.Update Progress Bar
                    call      NMDLUpdateProgressBar
.
                    scan      "INVALID" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    scan      "WILDCAT" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    scan      "BATCH" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    scan      "TELECOM" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    scan      "PC0022" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    scan      "CONFERENCE" in nuseuser
                    goto skipuser if equal
                    reset     nuseuser
                    NMDL3001CListView001.InsertItem giving N9 using nuseUser
                    NMDL3001CListView001.SetItemText using N9,nuseinit,1
                    NMDL3001CListView001.EnsureVisible using N9,0
skipuser
          repeat
.
.START PATCH 1.5.5 REMOVED LOGIC
.         call      Trim using INITS
.         scan      INITS,revtyp
.         if equal
.                   reset     revtyp
.                   move      YES,holdpassok
.         endif
.END PATCH 1.5.5 REMOVED LOGIC
.
          call      BookingDisableLower
          setitem   NMDLStatProgress,0,""
          setprop   NMDLProgressBar,visible=0
          clear   hold
.Main Loop
.Set Error Message Stat Text Boxes
          call      SetMDLErrorMssgDefault
.Set tab index
          clock   timestamp,timestamp
          unpack  timestamp,CC,YY,MM,DD
          setfocus MDL3ListView
          MDL3ListView.EnsureVisible using 0,0
          Mdl3ListView.SetItemState giving N9 using 0,2,2
.
          MDL3ListView.GetItemText giving mdlkey using 0,c3
          MDL3ListView.GetItemText giving str1 using 0,c4
          if (str1 = YES)
                    move      C1,mdltype
          else
                    clear     mdltype
          endif
          call     dispbook
.
           EVENTREG  X, 17, XRESIZE


          loop
                    waitevent
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
          repeat

Timeout
        beep
        beep
        beep
        stop

FileGo
.ExitFlag set to "N" if in Modify or New mode
          branch result to FileGo1,FileGo2,FileGo2
FileGo1
          winshow
          cALL      NMDLPRint using user
          Winhide
          return
FileGo2
          if (ExitFlag = YES)
                    CLOSE     NMDLFILE
                    winshow
                    stop
          endif
          return
EditGo
          return
HelpGo
        setprop AboutMssg,visible=1
        return
OptionsGo
          reset     RevTyp2
          scan      INITS,RevTyp2
          if equal
                    call      BookingSetReplaceOptions
          endif
          return
SearchGo
.        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
        branch  result to SearchGo2,SearchGo3
SearchGo1
.BROKER - not an option with this program
        return
SearchGo2
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3 Routine
.MAILER
          move      C3,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo4
.SHIP-TO - not an option with this program
        return
SearchGo5
.CAMPAIGN - not an option with this program
        return
SearchGo6
.OWNER - not an option with this program
        return

SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST
        unpack  Srchstr,str6,str1,str35
        getprop MDL3EditList,readonly=result
        if (result = 0)
                    setitem   MDL3CheckMailer,0,0
                setitem MDL3EditList,0,str6
                setitem MDL3StatList,0,str35
                setfocus MDL3EditList
        endif
        return
SearchLoad3
.MAILER
          unpack    Srchstr,str4,str1,str3,str1,str45
        getprop MDL3EditList,readonly=result
        if (result = 0)
                    setitem   MDL3CheckMailer,0,1
                setitem MDL3EditList,0,str4
                setitem MDL3StatList,0,str45
                setfocus MDL3EditList
        endif
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
SearchLoad6
.OWNER - not an option with this program
        return

SetMDLErrorMssgDefault
.Set Default for OrderFile Maintenance
          setprop   ErrorMssgStat1,visible=1
          setprop ErrorMssgStat2,visible=1
          setprop ErrorMssgStat3,visible=0
          setprop ErrorMssgStat4,visible=0
          setprop ErrorMssgStat5,visible=0
          setitem ErrorMssgStat1,0," "
          setitem ErrorMssgStat2,0," "
          setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
          setitem ErrorMssgOK,0,"&OK"
          return

.Routine which      destroys all objects created from above routines
Report2DestroyObjects
          destroy   ObjectColl
          return

BookingSetReplaceOptions
.Allows mass changes of Caller/Planner
          call      Report2DestroyObjects
          setprop   Report2,title="Booking Instructions Mass Replace"
          move      NO,RptCan
          create    Report2;StatTextBoxes(1)=50:70:10:110,"Old Caller",""
          create    Report2;StatTextBoxes(2)=70:90:10:110,"New Caller",""
          create    Report2;StatTextBoxes(3)=90:110:10:110,"Old Planner",""
          create    Report2;StatTextBoxes(4)=110:130:10:110,"New Planner",""
          create    Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=3,EditType=1,SelectAll=1,Style=1,Border=1
          create    Report2;EditTextBoxes(2)=70:90:80:130,MaxChars=3,EditType=1,SelectAll=1,Style=1,Border=1
          create    Report2;EditTextBoxes(3)=90:110:80:130,MaxChars=3,EditType=1,SelectAll=1,Style=1,Border=1
          create    Report2;EditTextBoxes(4)=110:130:80:130,MaxChars=3,EditType=1,SelectAll=1,Style=1,Border=1
          create    Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
          create    Report2;PackData=1:1:1:1
          activate StatTextBoxes(1)
          activate StatTextBoxes(2)
          activate StatTextBoxes(3)
          activate StatTextBoxes(4)
          activate EditTextBoxes(1)
          activate EditTextBoxes(2)
          activate EditTextBoxes(3)
          activate EditTextBoxes(4)
          activate Buttons(1),BookingReplaceOK,result
          activate PackData
          listins   ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
                    EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3),EditTextBoxes(4),Buttons(1),PackData
          setfocus EditTextBoxes(1)
          setprop   Report2,visible=1
          return
BookingReplaceOK
          getitem   EditTextBoxes(1),0,OldCal
          call      Trim using OldCal
          getitem   EditTextBoxes(2),0,NewCal
          call      Trim using NewCal
          getitem   EditTextBoxes(3),0,OldPlan
          call      Trim using OldPlan
          getitem   EditTextBoxes(4),0,NewPlan
          call      Trim using NewPlan
          if (OldCal = "" & OldPlan = "")
                    alert     note,"You must supply a Caller and/or a Planner!",result
                    setfocus EditTextBoxes(1)
                    return
          else
                    if (OldCal <> "")
                              if (NewCal = "")
                                        alert     note,"You must supply a New Caller!",result
                                        setfocus EditTextBoxes(2)
                                        return
                              else
                                        rep       lowup,NewCal
                                        move      C2,NUSEPATH
                                        pack      NUSEFLD2,NewCal,"   "
                                        move      "NUSEKEY-A",Location
                                        pack      KeyLocation,"Key: ",NUSEFLD2
                                        call      NUSEKEY
                                        if over
                                                  alert     note,"You must supply a valid New Caller!",result
                                                  setfocus EditTextBoxes(2)
                                                  return
                                        endif
                              endif
                    elseif (NewCal <> "")
                              alert     note,"You must supply a valid Old Caller!",result
                              setfocus EditTextBoxes(1)
                              return
                    endif
                    if (OldPlan <> "")
                              if (NewPlan = "")
                                        alert     note,"You must supply a New Planner!",result
                                        setfocus EditTextBoxes(4)
                                        return
                              else
                                        rep       lowup,NewPlan
                                        move      C2,NUSEPATH
                                        pack      NUSEFLD2,NewPlan,"   "
                                        move      "NUSEKEY-B",Location
                                        pack      KeyLocation,"Key: ",NUSEFLD2
                                        call      NUSEKEY
                                        if over
                                                  alert     note,"You must supply a valid New Planner!",result
                                                  setfocus EditTextBoxes(4)
                                                  return
                                        endif
                              endif
                    elseif (NewPlan <> "")
                              alert     note,"You must supply a valid Old Planner!",result
                              setfocus EditTextBoxes(3)
                              return
                    endif
.Perform replacement
                    deleteitem PackData,0
                    if (OldCal <> "")
                              pack      OldCal,OldCal,"   "
                    endif
                    if (OldPlan <> "")
                              pack      OldPlan,OldPlan,"   "
                    endif
                    setmode   *mcursor=*wait
                    move      "-4",SEQ
                    loop
                              move      "Back-NMDLSEQ",Location
                              call      NMDLSEQ
                              until over
                    repeat
                    move      "-1",SEQ
                    loop
                              move      "NMDLSEQ",Location
                              call      NMDLSEQ
                              until over
                              if (OldCal <> "" & OldCal = MDLCALL)
                                        if (NewCal <> "")
                                                  pack      NMDLFLD,mdlkey,mdltype
                                                  insertitem PackData,0,NMDLFLD
                                        endif
                              elseif (OldPlan <> "" & OldPlan = MDLPlan)
                                        if (NewPlan <> "")
                                                  pack      NMDLFLD,mdlkey,mdltype
                                                  insertitem PackData,0,NMDLFLD
                                        endif
                              endif
                    repeat
                    PackData.GetCount giving howmany
                    if (howmany > C0)
                              for result,"1",howmany
                                        getitem   PackData,result,NMDLFLD
                                        move      "NMDLKEY",Location
                                        call      NMDLKEY
                                        if not over
                                                  move      C0,N1
                                                  if (OldCal <> "" & OldCal = MDLCALL)
                                                            if (NewCal <> "")
                                                                      pack      MDLCALL,NewCal,"   "
                                                                      move      C1,N1
                                                            endif
                                                  endif
                                                  if (OldPlan <> "" & OldPlan = MDLPlan)
                                                            if (NewPlan <> "")
                                                                      pack      MDLPlan,NewPlan,"   "
                                                                      move      C1,N1
                                                            endif
                                                  endif
                                                  if (N1 = C1)
                                                            move      "NMDLUPD",Location
                                                            call      NMDLUPD
                                                  endif
                                        endif
                              repeat
                    endif
          endif
          setmode   *mcursor=*arrow
          setprop   Report2,visible=0
          move      howmany,str9
          call      Trim using str9
          call      FormatNumeric using str9,str11
          pack      taskname,str11," Record(s) updated!"
          alert     note,taskname,result
          return

BookingEnableLower
          setprop   MDL3ListView,enabled=0
          setprop   MDL3ListView2,enabled=0
          setprop   MDL3ListView3,enabled=0
          setprop   MDL3CheckLCR,enabled=1
.START PATCH 1.5.2 ADDED LOGIC
          setprop   MDL3CheckLCR2,enabled=1
.END PATCH 1.5.2 ADDED LOGIC
          setprop   MDL3CheckBook,enabled=1
          setprop   MDL3CheckMailer,enabled=1
          setprop   MDL3EditList,enabled=1,bgcolor=white
          setprop   MDL3ComboReport,enabled=1,bgcolor=white
          setprop   MDL3EditInstructions,readonly=0
          setprop   MDL3EditPlanner,enabled=1,bgcolor=white
          setprop   MDL3EditCaller,enabled=1,bgcolor=white
          return
BookingDisableLower
          setprop   MDL3CheckLCR,enabled=0
.START PATCH 1.5.2 ADDED LOGIC
          setprop   MDL3CheckLCR2,enabled=0
.END PATCH 1.5.2 ADDED LOGIC
          setprop   MDL3CheckBook,enabled=0
          setprop   MDL3CheckMailer,enabled=0
          setprop   MDL3EditPlanner,enabled=0,bgcolor=grey
          setprop   MDL3EditCaller,enabled=0,bgcolor=grey
          setprop   MDL3EditList,enabled=0,bgcolor=grey,readonly=1
          setprop   MDL3ComboReport,enabled=0,bgcolor=grey
          setprop   MDL3EditInstructions,readonly=1
.START PATCH 1.5.5 REMOVED LOGIC
.         move      NO,holdpassok2
.END PATCH 1.5.5 REMOVED LOGIC
          return
BookingEnableUpper
          setprop mdl3ButtonNew,enabled=1
          setprop mdl3ButtonMod,enabled=1
          setprop mdl3ButtonSave,enabled=0
          setprop mdl3ButtonQuit,enabled=0
          setprop MDL3ButtonDelete,enabled=0
          setprop   MDL3ListView,enabled=1
          setprop   MDL3ListView2,enabled=1
          setprop   MDL3ListView3,enabled=1
          setprop mdl3Exit,enabled=1
          move      YES,ExitFlag
          return
BookingDisableUpper
          setprop mdl3ButtonNew,enabled=0
          setprop mdl3ButtonMod,enabled=0
          setprop mdl3ButtonSave,enabled=1
          setprop mdl3ButtonQuit,enabled=1
          setprop mdl3Exit,enabled=0
          move      NO,ExitFlag
          return

BookingClearFields
.Clear Fields
          setitem mdl3checkLCR,c0,c0
.START PATCH 1.5.2 ADDED LOGIC
          setitem mdl3checkLCR2,c0,c0
.END PATCH 1.5.2 ADDED LOGIC
          setitem mdl3checkBook,c0,c0
          setitem MDL3CheckMailer,c0,c0
          setitem mdl3editInstructions,0,""
          setitem MDL3StatRevised,0,""
        setitem Mdl3editcaller,0,""
        setitem Mdl3editPlanner,0,""
        setitem MDL3EditList,0,""
        setitem MDL3StatList,0,""
        setitem MDL3StatListStatus,0,""
        setitem MDL3ComboReport,0,1
          return

BookingSortListView
.Dynamically sorts Different ListViews.
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
          getprop   MDL3ListView,visible=N9
          if (N9 = C1)    .MDL3ListView is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                    if (result = 1)
                              setprop   MDL3ListView,visible=0
                              setprop MDL3ListView2,visible=1
                              setprop MDL3ListView3,visible=0
                              MDL3ListView2.EnsureVisible using c1,0
                              MDL3ListView2.GetItemText giving mdlkey using c1,c3
                              MDL3ListView2.GetItemText giving str1 using c1,c4
                              if (str1 = YES)
                                        move      C1,mdltype
                              else
                                        clear     mdltype
                              endif
                              call      dispbook
                              Mdl3ListView2.SetItemState giving N9 using 1,2,2
                              setfocus mdl3listview2
                    elseif (result = 2)
                              setprop MDL3ListView,visible=0
                              setprop MDL3ListView2,visible=0
                              setprop MDL3ListView3,visible=1
                              MDL3ListView3.EnsureVisible using c1,0
                              MDL3ListView3.GetItemText giving mdlkey using c1,c3
                              MDL3ListView3.GetItemText giving str1 using c1,c4
                              if (str1 = YES)
                                        move      C1,mdltype
                              else
                                        clear     mdltype
                              endif
                              call      dispbook
                              Mdl3ListView3.SetItemState giving N9 using 1,2,2
                              setfocus mdl3listview3
                    endif
          else
                    getprop MDL3ListView2,visible=N9
                    if (N9 = C1)    .MDL3ListView2 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                              if (result = 1)
                                        setprop MDL3ListView,visible=1
                                        setprop MDL3ListView2,visible=0
                                        setprop MDL3ListView3,visible=0
                                        MDL3ListView.EnsureVisible using c1,0
                                        MDL3ListView.GetItemText giving mdlkey using c1,c3
                                        MDL3ListView.GetItemText giving str1 using c1,c4
                                        if (str1 = YES)
                                                  move      C1,mdltype
                                        else
                                                  clear     mdltype
                                        endif
                                        call      dispbook
                                        Mdl3ListView.SetItemState giving N9 using 1,2,2
                                        setfocus mdl3listview
                              elseif (result = 2)
                                        setprop MDL3ListView,visible=0
                                        setprop MDL3ListView2,visible=0
                                        setprop MDL3ListView3,visible=1
                                        MDL3ListView3.EnsureVisible using c1,0
                                        MDL3ListView3.GetItemText giving mdlkey using c1,c3
                                        MDL3ListView3.GetItemText giving str1 using c1,c4
                                        if (str1 = YES)
                                                  move      C1,mdltype
                                        else
                                                  clear     mdltype
                                        endif
                                        call      dispbook
                                        Mdl3ListView3.SetItemState giving N9 using 1,2,2
                                        setfocus mdl3listview3
                              endif
                    else            .MDL3ListView3 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                              if (result = 1)
                                        setprop MDL3ListView,visible=0
                                        setprop MDL3ListView2,visible=1
                                        setprop MDL3ListView3,visible=0
                                        MDL3ListView.EnsureVisible using c1,0
                                        MDL3ListView.GetItemText giving mdlkey using c1,c3
                                        MDL3ListView.GetItemText giving str1 using c1,c4
                                        if (str1 = YES)
                                                  move      C1,mdltype
                                        else
                                                  clear     mdltype
                                        endif
                                        call      dispbook
                                        Mdl3ListView.SetItemState giving N9 using 1,2,2
                                        setfocus mdl3listview
                              elseif (result = 2)
                                        setprop MDL3ListView,visible=1
                                        setprop MDL3ListView2,visible=0
                                        setprop MDL3ListView3,visible=0
                                        MDL3ListView2.EnsureVisible using c1,0
                                        MDL3ListView2.GetItemText giving mdlkey using c1,c3
                                        MDL3ListView2.GetItemText giving str1 using c1,c4
                                        if (str1 = YES)
                                                  move      C1,mdltype
                                        else
                                                  clear     mdltype
                                        endif
                                        call      dispbook
                                        Mdl3ListView2.SetItemState giving N9 using 1,2,2
                                        setfocus mdl3listview2
                              endif
                    endif
          endif
          return

BookingAddItems
          if (MDLTYPE = "1")
                    reset     MDLKEY
.START PATCH 1.5.7 REPLACED LOGIC
.                   unpack    MDLKEY into str2,str4
.                   pack      MKEY,str4,z3
.                   move      "AddItem-NMLRKEY",Location
.                   pack      KeyLocation,"Key: ",MKEY
.                   call      NMLRKEY
.                   move      MCOMP,OLSTNAME
                    move      MDLKEY,COMPFLD
                    move      "AddItem-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    move      COMPCOMP,OLSTNAME
.END PATCH 1.5.7 REPLACED LOGIC
          else
                    reset     MDLKEY
                    move      MDLKEY,NDATFLD
                    move      "AddItem-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
          endif
          clear     str3
          move      MDLCALL to str3
          if (MDLTYPE = "1")
                    move      YES,str1
                    move      "0x0000FF",colordim           .Blue
          else
                    clear     str1
                    move      "0x000000",colordim           .Black
          endif
.
.begin patch 1.5.9
.         if (ELSTCDE = "C")
          if (ELSTCDE = "C" | ELSTCDE = "P" )
.end patch 1.5.9
                    move      YES,str2
          else
                    clear     str2
          endif
.
          MDL3ListView.InsertItem giving N9 using str3
          MDL3ListView.SetItemText using N9,MDLPLAN,1
          MDL3ListView.SetItemText using N9,OLSTNAME,2
          MDL3ListView.SetItemText using N9,MDLKEY,3
          MDL3ListView.SetItemText using N9,str1,4
          MDL3ListView.SetItemText using N9,str2,5
          MDL3ListView.SetItemText using N9,colordim,6

          MDL3ListView2.InsertItem giving N9 using MDLPLAN
          MDL3ListView2.SetItemText using N9,str3,1
          MDL3ListView2.SetItemText using N9,OLSTNAME,2
          MDL3ListView2.SetItemText using N9,MDLKEY,3
          MDL3ListView2.SetItemText using N9,str1,4
          MDL3ListView2.SetItemText using N9,str2,5
          MDL3ListView2.SetItemText using N9,colordim,6

          MDL3ListView3.InsertItem giving N9 using OLSTNAME
          MDL3ListView3.SetItemText using N9,MDLPLAN,1
          MDL3ListView3.SetItemText using N9,str3,2
          MDL3ListView3.SetItemText using N9,MDLKEY,3
          MDL3ListView3.SetItemText using N9,str1,4
          MDL3ListView3.SetItemText using N9,str2,5
          MDL3ListView3.SetItemText using N9,colordim,6
          return

BookingVerifyData
          getitem MDL3CheckMailer,c0,N1
          if (N1 = C0)
                    clear     MDLTYPE
                    getitem   MDL3EditList,0,str6
                    call      Trim using str6
                    if (str6 = "")
                              alert     note,"Valid List Number is Required!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
                    call      ZFillIt using str6
                    move      str6,NDATFLD
                    move      "Verify-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
                    if over
                              alert     note,"Valid List Number is Required!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
                    pack      NMDLFLD,str6," "
                    move      "Verify-NMDLTST",Location
                    pack      KeyLocation,"Key: ",NMDLFLD
                    call      NMDLTST
                    if not over
                              if (mode = 1)                 .New
                                        alert     note,"Instructions already exist for this List!",result
                                        move      YES,ReturnFlag
                                        setfocus MDL3EditList
                                        return
                              endif
                    elseif (mode = 2)
                              alert     note,"Booking File In Use.  Try Again Later!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
          else
                    getitem   MDL3EditList,0,str6
                    call      Trim using str6
                    if (str6 = "")
                              alert     note,"Valid Mailer Number is Required!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
.START PATCH 1.5.7 REPLACED LOGIC
.                   move      C0,N4
.                   move      str6,N4
.                   move      N4,str4
.                   rep       zfill,str4
.                   pack      MKEY,str4,"000"
.                   move      "Verify-NMLRKEY",Location
.                   pack      KeyLocation,"Key: ",MKEY
.                   call      NMLRKEY
.                   if over
.                             alert     note,"Valid Mailer Number is Required!",result
.                             move      YES,ReturnFlag
.                             setfocus MDL3EditList
.                             return
.                   endif
.                   move      C2,NXRFPATH
.                   clear     NXRFFLD
.                   pack      NXRFFLD2,str4
.                   move      "Verify-NXRFKEY",Location
.                   pack      KeyLocation,"Key: ",NXRFFLD2
.                   call      NXRFKEY
.                   if not over
.                             pack      taskname,"This Mailer has associated Lists in our system!",newline,"Are you sure you wish to continue?"
.                             alert     plain,taskname,result
.                             if (result <> C1)
.                                       move      YES,ReturnFlag
.                                       setfocus MDL3EditList
.                                       return
.                             endif
.                   endif
.                   pack      NMDLFLD,"00",str4,"1"
.................................
                    move      C0,N6
                    move      str6,N6
                    move      N6,str6
                    rep       zfill,str6
                    pack      COMPFLD,str6
                    move      "Verify-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if over
                              alert     note,"Valid Mailer Number is Required!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    elseif (COMPMLRFLG <> "T")
                              alert     note,"Valid Mailer Number is Required!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
                    move      C2,NXRFPATH
                    clear     NXRFFLD
                    pack      NXRFFLD2,str6
                    move      "Verify-NXRFKEY",Location
                    pack      KeyLocation,"Key: ",NXRFFLD2
                    call      NXRFKEY
                    if not over
                              pack      taskname,"This Mailer has associated Lists in our system!",newline,"Are you sure you wish to continue?"
                              alert     plain,taskname,result
                              if (result <> C1)
                                        move      YES,ReturnFlag
                                        setfocus MDL3EditList
                                        return
                              endif
                    endif
                    pack      NMDLFLD,str6,"1"
.END PATCH 1.5.7 REPLACED LOGIC
                    move      "Verify-NMDLTST",Location
                    pack      KeyLocation,"Key: ",NMDLFLD
                    call      NMDLTST
                    if not over
                              if (mode = 1)                 .New
                                        alert     note,"Instructions already exist for this Mailer!",result
                                        move      YES,ReturnFlag
                                        setfocus MDL3EditList
                                        return
                              endif
                    elseif (mode = 2)
                              alert     note,"Booking File In Use.  Try Again Later!",result
                              move      YES,ReturnFlag
                              setfocus MDL3EditList
                              return
                    endif
                    pack      str6,NMDLFLD
                    move      C1,MDLTYPE
          endif
          move      str6,MDLKEY
          getitem MDL3EditPlanner,0,MDLPlan
          call      Trim using MDLPlan
          rep       lowup,MDLPlan
          setitem MDL3EditPlanner,0,MDLPlan
.begin patch  1.5.9
.         if (MDLPlan = "" & ELSTCDE = "C")
          if (MDLPlan = "" & ( ELSTCDE = "C" | ELSTCDE = "P"))
.end patch  1.5.9
.START PATCH 1.5.1 ADDED LOGIC
.                   call      BadPlanner
.                   move      YES,ReturnFlag
.                   setfocus MDL3EditPlanner
.                   return
.END PATCH 1.5.1 ADDED LOGIC
.START PATCH 1.5.1 REPLACED LOGIC
.         elseif (MDLPlan <> "")
          elseif (MDLPlan <> "" & MDLPlan <> "X")
.END PATCH 1.5.1 REPLACED LOGIC
                    move      C2,NUSEPATH              .set path to inits
                    packkey   NUSEFLD2,MDLPlan
                    move      "Verify-NUSETST",Location
                    pack      KeyLocation,"Key: ",NUSEFLD2
                    call      NUSETST
                    if over
                              call      BadPlanner
                              move      YES,ReturnFlag
                              setfocus MDL3EditPlanner
                              return
                    endif
          endif
          getitem MDL3EditCaller,0,MDLCall
          call      Trim using MDLCall
          rep       lowup,MDLCall
          setitem MDL3EditCaller,0,MDLCall
.begin patch  1.5.9
.         if (MDLCall = "" & ELSTCDE = "C")
          if (MDLCall = "" & ( ELSTCDE = "C" | ELSTCDE = "P"))
.end patch  1.5.9
                    call      BadCaller
                    move      YES,ReturnFlag
                    setfocus MDL3EditCaller
                    return
.START PATCH 1.5.1 REPLACED LOGIC
.         elseif (MDLCall <> "")
          elseif (MDLCall <> "" & MDLCall <> "X")
.END PATCH 1.5.1 REPLACED LOGIC
                    move      C2,NUSEPATH              .set path to inits
                    packkey   NUSEFLD2,MDLCall
                    move      "Verify2-NUSETST",Location
                    pack      KeyLocation,"Key: ",NUSEFLD2
                    call      NUSETST
                    if over
                              call      BadCaller
                              move      YES,ReturnFlag
                              setfocus MDL3EditCaller
                              return
                    endif
          endif
.START PATCH 1.5.5 REMOVED LOGIC
.         if (MDLTYPE = "" & ELSTCDE = "C")
.                   reset     revtyp
.                   scan      INITS,revtyp
.                   if not equal
..START PATCH 1.5.1 REPLACED LOGIC
..                            if (MDLCall <> INITS)
.                             if (MDLCall <> INITS & MDLCall <> "X")
..END PATCH 1.5.1 REPLACED LOGIC
.                                       if (holdpassok2 <> YES)
.                                                 pack      taskname,"Assigning a Caller other than yourself requires a Password!"
.                                                 alert     note,taskname,result
.                                                 setitem   PasswordEdit,0,""
.                                                 setfocus PasswordEdit
.                                                 move      "Q",progcode
.                                                 setprop   Passwrd,visible=1
.                                                 if (PassFlag = NO)
..                                                          pack      taskname,"Assigning a Caller other than yourself requires a Password!"
..                                                          alert     note,taskname,result
.                                                           move      YES,ReturnFlag
.                                                           setfocus MDL3EditCaller
.                                                           return
.                                                 endif
.                                       endif
.                             endif
.                   endif
.         endif
.END PATCH 1.5.5 REMOVED LOGIC
          unpack    timestamp,MDLDATE
          clear     MDLINITS
          move      C0,NUSEFLD
          move      C1,NUSEPATH
          move      PORTN,NUSEFLD
          rep       zfill,NUSEFLD
          move      "Verify-NUSEKEY",Location
          pack      KeyLocation,"Key: ",NUSEFLD
          call      NUSEKEY
          if not over
                    call      Trim using NUSEUSER
                    if (NUSEUSER <> "")
                              scan      B1,NUSEUSER
                              if equal
                                        bump      NUSEUSER
                                        movefptr NUSEUSER,N9
                                        setlptr NUSEUSER,N9
                              endif
                              reset     NUSEUSER
                              if (NUSEUSER <> "")
                                        pack      MDLINITS,NUSEUSER,PERIOD
                              endif
                    endif
          endif
          getitem MDL3EditInstructions,0,MDLtext
          call      Trim using MDLtext
          getitem MDL3CheckLCR,0,N1
          if (N1 = C1)
                    move      YES,MDLLCRCD
          else
                    clear     MDLLCRCD
          endif
          getitem MDL3CheckBook,0,N1
          if (N1 = C1)
                    move      yes to MDLCODE
          else
                    move      no to MDLCODE
          endif
.START PATCH 1.5.2 ADDED LOGIC
          getitem MDL3CheckLCR2,0,N1
          if (N1 = C1)
                    move      YES,MDLLCRCD2
          else
                    clear     MDLLCRCD2
          endif
.END PATCH 1.5.2 ADDED LOGIC
          return

dispbook
          pack      NMDLFLD,MDLKEY,MDLTYPE
.START PATCH 1.5.6 ADDED LOGIC
          call      Trim using NMDLFLD
          if (NMDLFLD = "")
                    return
          endif
.END PATCH 1.5.6 ADDED LOGIC
          move      "dispbook-NMDLKEY",Location
          pack      KeyLocation,"Key: ",NMDLFLD
          call      nmdlkey
          if over
.Should never happen since we load them all at the beginning.
                    call      BookingClearFields
                    setitem mdl3editInstructions,0,"No Instructions Found"
          else
                    call      Trim using MDLtext
                    setitem mdl3editInstructions,0,MDLtext
                    setitem Mdl3editcaller,0,mdlcall
                    setitem Mdl3editPlanner,0,mdlplan
                    move      C0,N1
                    move      MDLTYPE,N1
                    setitem mdl3checkMailer,c0,N1
                    cmatch    yes,mdllcrcd
                    if equal
                              setitem mdl3checkLCR,c0,c1
                    else
                              setitem mdl3checkLCR,c0,c0
                    endif
.START PATCH 1.5.2 ADDED LOGIC
                    cmatch    yes,mdllcrcd2
                    if equal
                              setitem mdl3checkLCR2,c0,c1
                    else
                              setitem mdl3checkLCR2,c0,c0
                    endif
.END PATCH 1.5.2 ADDED LOGIC
                    cmatch    yes to mdlcode
                    if equal
                              setitem mdl3checkBook,c0,c1
                    endif
                    cmatch  b1 to mdlcode
                    if equal
                              setitem mdl3checkBook,c0,c1
                    endif
                    clear     taskname
                    call      Trim using mdldate
                    call      Trim using mdlinits
                    if (mdldate <> "" | mdlinits <> "")
                              clear     str10
                              append    "Revised",taskname
                              if (mdldate <> "")
                                        unpack    mdldate,CC,YY,MM,DD
                                        pack      str10,MM,SLASH,DD,SLASH,CC,YY
                                        append    " ",taskname
                                        append    str10,taskname
                              endif
                              if (mdlinits <> "")
                                        append    " by ",taskname
                                        append    mdlinits,taskname
                              endif
                              reset     taskname
                    endif
                    setitem   MDL3StatRevised,0,taskname
                    rep       zfill in mdllcrcd1
                    move      mdllcrcd1 to n2
                    add       c1 to n2
                    setitem    mdl3ComboReport,1,n2
.get datacard status
                    setitem mdl3STATListStatus,0,""
                    if (MDLTYPE = B1)
                              move      nmdlfld to ndatfld
                              move      "dispbook-ndatkey",Location
                              pack      KeyLocation,"Key: ",ndatfld
                              call      ndatkey
dispbooklist
                              if (status = "W" | status = "T")
                                        setprop mdl3STATListStatus,fgcolor=blue
                                        if (status = "W")
                                                  setitem   mdl3STATListStatus,0,"Withdrawn"
                                        else
                                                  setitem mdl3STATListStatus,0,"Temp With"
                                        endif
                              else
                                        setprop mdl3STATListStatus,fgcolor=black
                              endif
                              setitem   MDL3EditList,0,LSTNUM
                              setitem   MDL3StatList,0,MLSTNAME
                    else
.START PATCH 1.5.7 REPLACED LOGIC
.                             unpack    MDLKEY,str2,str4
.                             pack      MKEY,str4,"000"
.                             move      "dispbook-NMLRKEY",Location
.                             pack      KeyLocation,"Key: ",MKEY
.                             call      NMLRKEY
.dispbookmailer
.                             setprop mdl3STATListStatus,fgcolor=black
.                             setitem   MDL3EditList,0,MNUM
.                             setitem   MDL3StatList,0,MCOMP
.................................
                              move      MDLKEY,COMPFLD
                              move      "dispbook-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
dispbookmailer
                              setprop mdl3STATListStatus,fgcolor=black
                              setitem   MDL3EditList,0,COMPNUM
                              setitem   MDL3StatList,0,COMPCOMP
.START PATCH 1.5.7 REPLACED LOGIC
                    endif
          endif
          return
.
BookingDeleteItem
.Delete Entries in ListView objects - Will add back later
          MDL3ListView.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              MDL3ListView.DeleteItem Using result
                              break
                    endif
          repeat
.
          MDL3ListView2.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView2.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              MDL3ListView2.DeleteItem Using result
                              break
                    endif
          repeat
.
          MDL3ListView3.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView3.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              MDL3ListView3.DeleteItem Using result
                              break
                    endif
          repeat
          return

BookingRefreshItems
.Delete Entries in ListView objects - Will add back later
          MDL3ListView.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              Mdl3ListView.SetItemState giving N9 using result,2,2
                              break
                    endif
          repeat
.
          MDL3ListView2.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView2.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              Mdl3ListView2.SetItemState giving N9 using result,2,2
                              break
                    endif
          repeat
.
          MDL3ListView3.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    MDL3ListView3.GetItemText giving str6 using result,c3
                    if (str6 = MDLKEY)
                              Mdl3ListView3.SetItemState giving N9 using result,2,2
                              break
                    endif
          repeat
          return

RestoreFocus
.what view are we using?
          getprop MDL3ListView,visible=N8
          if (N8 = C1)    .MDL3ListView is visible
                    setfocus MDL3ListView
                    MDL3ListView.GetNextItem giving N9 using C2
                    MDL3ListView.GetItemText giving mdlkey using N9,c3
                    MDL3ListView.GetItemText giving str1 using N9,c4
                    MDL3ListView.EnsureVisible using N9,0
                    if (str1 = YES)
                              move      C1,mdltype
                    else
                              clear     mdltype
                    endif
                    call      dispbook
          else
                    getprop MDL3ListView2,visible=N8
                    if (N8 = C1)    .MDL2ListView2 is visible
                              setfocus MDL3ListView2
                              MDL3ListView2.GetNextItem giving N9 using C2
                              MDL3ListView2.GetItemText giving mdlkey using N9,c3
                              MDL3ListView2.GetItemText giving str1 using N9,c4
                              MDL3ListView2.EnsureVisible using N9,0
                              if (str1 = YES)
                                        move      C1,mdltype
                              else
                                        clear     mdltype
                              endif
                              call      dispbook
                    else
                              getprop MDL3ListView3,visible=N8
                              if (N8 = C1)    .MDLListView3 is visible
                                        setfocus MDL3ListView3
                                        MDL3ListView3.GetNextItem giving N9 using C2
                                        MDL3ListView3.GetItemText giving mdlkey using N9,c3
                                        MDL3ListView3.GetItemText giving str1 using N9,c4
                                        MDL3ListView3.EnsureVisible using N9,0
                                        if (str1 = YES)
                                                  move      C1,mdltype
                                        else
                                                  clear     mdltype
                                        endif
                                        call      dispbook
                              endif
                    endif
          endif
          call      BookingDisableLower
          call      BookingEnableUpper
          move    c0 to mode
          return
.
BadCaller
          setitem NMDL3001cEdittext001,0,"Invalid Caller Select one"
          move      C2,plancallflag
          setfocus NMDL3001CListVIew001
          NMDL3001CListView001.EnsureVisible using c1,0
          NMdl3001CListView001.SetItemState giving N9 using 1,2,2
          setprop NMDL3001C,visible=1
          return

BadPlanner
          setitem NMDL3001cEdittext001,0,"Invalid Planner Select one"
          move      C1,plancallflag
          setfocus NMDL3001CListVIew001
          NMDL3001CListView001.EnsureVisible using c1,0
          NMdl3001CListView001.SetItemState giving N9 using 1,2,2
          setprop NMDL3001C,visible=1
          return
.
NMDLUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   NMDLProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
NMDLInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
XRESIZE
           Nmdl001a.Scale
           RETURN

.................................................................................

          INCLUDE   NDATIO.inc
          INCLUDE   NPASIO.inc
          INCLUDE   NXRFIO.inc
          INCLUDE   NSCHIO.inc
          INCLUDE   NMDLIO.inc
.Patch1.5.3
.         INCLUDE   NMLRIO.inc
.Patch1.5.3
          include   nuseio.inc
.For Search Screen
        include searchio.inc      .contains logic for search.plf
.Patch1.5.3
          include   compio.inc
          include   cntio.inc
.         include   nbrkio.inc
.Patch1.5.3
          include   nrtnio.inc
          include   nownio.inc
          include   ncmpio.inc
.
          INCLUDE   COMLOGIC.inc

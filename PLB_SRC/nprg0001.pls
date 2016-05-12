........................................
. Program:      NPRG0001.PLS
. Function:     INFORMATION SERVICES IN-HOUSE PROGRAM File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   July 18,2006
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include nprgdd.inc

release init    "1.0"   ASH 18JUL2006 ORIGINAL RELEASE
          move      "August 8, 2005",Wreldate

.
Timer   Timer
ExitFlag init   "Y"
ReturnFlag init "N"
NewFlag init    "N"
SecFlag   form      1
hold    dim     700 .NPRGVARS
holdkey dim     5
.
NPRGFLD1hold        DIM       53
NPRGFLD2hold        DIM       13
NPRGFLD3hold        DIM       53
NPRGFLD4hold        DIM       53
NPRGFLD5hold        DIM       4
NPRGFLD6hold        DIM       503
.
DimPtr    dim       ^
FrmPtr    form      ^

.Colors
white   color
grey    color

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"

.Set Vars used for About Box
          move    "NPRG0001.PLS",Wprognme
          move    "I.S. In-House Program File Maintenance",Wfunction
          move    "Andrew Harkins",Wauthor

.Declare forms, Always declare child forms first
mss1    plform  Error
abt     plform  About
x       plform  Nprg0001
        winhide
.Load Forms, Always load parent form first
          formload x
          formload abt
          formload mss1

          CREATE  TIMER,18000     .30 minutes
          ACTIVATE TIMER,Timeout,RESULT
.Create Menus
          create  Nprg0001;mFile,FData
          create  Nprg0001;mEdit,EData,mFile
          create  Nprg0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
          activate mFile,FileGo,result
.Need this when it works
          activate mEdit,EditGo,result
.Only a SubMenu under this one
          activate mHelp,HelpGo,result
.
.Load List View Columns
          Nprg0001ListView.InsertColumn using "ID",50,0
          Nprg0001ListView.InsertColumn using "Program Name",150,1
          Nprg0001ListView.InsertColumn using "Type",75,2
          Nprg0001ListView.InsertColumn using "Developer",75,3
          Nprg0001ListView.InsertColumn using "Lead",75,4
          Nprg0001ListView.InsertColumn using "Status",75,5
          Nprg0001ListView.InsertColumn using "Date",75,6
          Nprg0001ListView.InsertColumn using "Details",0,7
          Nprg0001ListView.InsertColumn using "Date Sort",0,8
.
          clock     timestamp,timestamp
.
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
.
          move      C1,NPRGPATH
          setfocus Nprg0001EditSearch
.Main Loop
          clock   timestamp,timestamp
          call      Nprg0001DisableLower

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

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1
FileGo1
        if (ExitFlag = "Y")
                winshow
                shutdown
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return

.Disable Upper Screen
Nprg0001DisableUpper
          move    "N",ExitFlag
          setprop Nprg0001OK,enabled=0
          setprop Nprg0001Exit,enabled=0
          setprop Nprg0001New,enabled=0
          setprop Nprg0001Modify,enabled=0
.
          setprop Nprg0001EditSearch,enabled=0,bgcolor=grey
          setprop Nprg0001ComboSearchType,enabled=0,bgcolor=grey
          setprop Nprg0001ComboStatusSearch,enabled=0,bgcolor=grey
          setprop Nprg0001RadioDeveloperSearch,enabled=0
          setprop Nprg0001RadioLeadSearch,enabled=0
          setprop Nprg0001RadioNameSearch,enabled=0
          setprop Nprg0001RadioNotesSearch,enabled=0
          setprop Nprg0001RadioTypeSearch,enabled=0
          setprop Nprg0001ListView,enabled=0
        return

.Enable Upper Screen
Nprg0001EnableUpper
.Allow Exit
          move    "Y",ExitFlag
          setprop Nprg0001Delete,enabled=0
          setprop Nprg0001OK,enabled=1
          setprop Nprg0001Exit,enabled=1
          setprop Nprg0001New,enabled=1
.
          setprop Nprg0001EditSearch,enabled=1,bgcolor=white
          setprop Nprg0001ComboSearchType,enabled=1,bgcolor=white
          setprop Nprg0001ComboStatusSearch,enabled=1,bgcolor=white
          setprop Nprg0001RadioDeveloperSearch,enabled=1
          setprop Nprg0001RadioLeadSearch,enabled=1
          setprop Nprg0001RadioNameSearch,enabled=1
          setprop Nprg0001RadioNotesSearch,enabled=1
          setprop Nprg0001RadioTypeSearch,enabled=1
          setprop Nprg0001ListView,enabled=1
          Nprg0001ListView.GetItemCount giving howmany
          if (howmany > C0)
                    setprop   Nprg0001Modify,enabled=1
          else
                    setprop   Nprg0001Modify,enabled=0
          endif
          move      "N",NewFlag
        return

.Disable Lower Screen
Nprg0001DisableLower
          setprop Nprg0001ComboStatus,enabled=0,bgcolor=grey
          setprop Nprg0001EditDate,enabled=0,bgcolor=grey
          setprop Nprg0001EditDeveloper,enabled=0,bgcolor=grey
          setprop Nprg0001EditID,enabled=0,bgcolor=grey
          setprop Nprg0001EditLead,enabled=0,bgcolor=grey
          setprop Nprg0001EditMenu,enabled=0,bgcolor=grey
          setprop Nprg0001EditName,enabled=0,bgcolor=grey
          setprop Nprg0001EditNotes,readonly=1
          setprop Nprg0001EditType,enabled=0,bgcolor=grey
Nprg0001DisableLowerButtons
          setprop Nprg0001Save,enabled=0
          setprop Nprg0001Quit,enabled=0
        return

.Enable Lower Screen
Nprg0001EnableLower
          setprop Nprg0001Save,enabled=1
          setprop Nprg0001Quit,enabled=1
          setprop Nprg0001ComboStatus,enabled=1,bgcolor=white
          setprop Nprg0001EditDate,enabled=1,bgcolor=white
          setprop Nprg0001EditDeveloper,enabled=1,bgcolor=white
          setprop Nprg0001EditID,enabled=1,bgcolor=white
          setprop Nprg0001EditLead,enabled=1,bgcolor=white
          setprop Nprg0001EditMenu,enabled=1,bgcolor=white
          setprop Nprg0001EditName,enabled=1,bgcolor=white
          setprop Nprg0001EditNotes,readonly=0
          setprop Nprg0001EditType,enabled=1,bgcolor=white
Nprg0001EnableLowerButtons
          setprop Nprg0001Save,enabled=1
          setprop Nprg0001Quit,enabled=1
        return

.Clear Lower Screen
Nprg0001ClearScreen
          setitem Nprg0001EditID,0,""
          setitem Nprg0001EditName,0,""
          setitem Nprg0001EditMenu,0,""
          setitem Nprg0001EditType,0,""
          setitem Nprg0001EditDeveloper,0,""
          setitem Nprg0001EditLead,0,""
          setitem Nprg0001ComboStatus,0,0
          setitem Nprg0001EditDate,0,""
          setitem Nprg0001EditNotes,0,""
        return

Nprg0001ClearKeyFields
          clear     NPRGFLD1
          clear     NPRGFLD2
          clear     NPRGFLD3
          clear     NPRGFLD4
          clear     NPRGFLD5
          clear     NPRGFLD6
          return

Nprg0001ReadAAM
          getitem   Nprg0001ComboSearchType,0,N1
          if (N1 = 1)         .Mutually Exclusive Search
                    move      "Read-NPRGAIM",Location
                    pack      KeyLocation,"Key: ",NPRGFLD1,NPRGFLD2,NPRGFLD3,NPRGFLD4,NPRGFLD5,NPRGFLD6
                    call      NPRGAIM
                    loop
                              until over
                              call      Nprg0001LoadListView
                              move      "Read-NPRGKG",Location
                              call      NPRGKG
                    repeat
          else                .Inclusive Searches
                    move      NPRGFLD1,NPRGFLD1hold
                    move      NPRGFLD2,NPRGFLD2hold
                    move      NPRGFLD3,NPRGFLD3hold
                    move      NPRGFLD4,NPRGFLD4hold
                    move      NPRGFLD5,NPRGFLD5hold
                    move      NPRGFLD6,NPRGFLD6hold
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD1hold <> "")
                              move      NPRGFLD1hold,NPRGFLD1
                              call      Nprg0001ReadAAMInclusive
                    endif
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD2hold <> "")
                              move      NPRGFLD2hold,NPRGFLD2
                              call      Nprg0001ReadAAMInclusive
                    endif
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD3hold <> "")
                              move      NPRGFLD3hold,NPRGFLD3
                              call      Nprg0001ReadAAMInclusive
                    endif
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD4hold <> "")
                              move      NPRGFLD4hold,NPRGFLD4
                              call      Nprg0001ReadAAMInclusive
                    endif
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD5hold <> "")
                              move      NPRGFLD5hold,NPRGFLD5
                              call      Nprg0001ReadAAMInclusive
                    endif
.
                    call      Nprg0001ClearKeyFields
                    if (NPRGFLD6hold <> "")
                              move      NPRGFLD6hold,NPRGFLD6
                              call      Nprg0001ReadAAMInclusive
                    endif
          endif
          call      Nprg0001ReadEnd
          return

Nprg0001ReadAAMInclusive
          move      "Read2-NPRGAIM",Location
          pack      KeyLocation,"Key: ",NPRGFLD1,NPRGFLD2,NPRGFLD3,NPRGFLD4,NPRGFLD5,NPRGFLD6
          call      NPRGAIM
          loop
                    until over
                    move      C0,N1     .Set Flag
                    Nprg0001ListView.GetItemCount giving howmany
                    sub       C1,howmany
                    for result,C0,howmany
                              Nprg0001ListView.GetItemText giving str5 using result,0
                              if (str5 = NPRGNUM)
                                        move      C1,N1
                                        break
                              endif
                    repeat
                    if (N1 = C0)
                              call      Nprg0001LoadListView
                    endif
                    move      "Read2-NPRGKG",Location
                    call      NPRGKG
          repeat
          return

Nprg0001ReadSEQ
        branch      NPRGFLAG,Nprg0001ReadSEQ2
        call        NPRGOPEN
Nprg0001ReadSEQ2
          move      "Read-SEQEOF",Location
          pack      KeyLocation,"Key: SEQEOF"
          read      NPRGFILE,SEQEOF;;
.
          loop
                    move      "Read-NPRGSEQ2",Location
                    pack      KeyLocation,"Key: SEQ"
                    call      NPRGSEQ2
                    until over
                    call      Nprg0001LoadListView
          repeat
          call      Nprg0001ReadEnd
          return

Nprg0001ReadEnd
          Nprg0001ListView.GetItemCount giving howmany
          if (howmany > C0)
                    if (howmany = 1)
                              pack      taskname,"1 Record Found."
                    else
                              move      howmany,str9
                              call      FormatNumeric using str9,str11
                              pack      taskname,str11," Records Found."
                    endif
                    setitem   Nprg0001StatRecords,0,taskname
                    Nprg0001ListView.SetItemState giving result using 0,2,2
                    Nprg0001ListView.EnsureVisible using 0,0
                    call      Click_Nprg0001ListView
                    setprop   Nprg0001Modify,enabled=1
          else
                    setitem   Nprg0001StatRecords,0,"No Records Found."
                    setprop   Nprg0001Modify,enabled=0
          endif
          return

Nprg0001LoadListView
          //Need to load hold variable before you start trimming each field
          pack      hold,NPRGVARS
          call      Trim using NPRGNUM
          Nprg0001ListView.InsertItem giving result using NPRGNUM
          call      Trim using NPRGPNAME
          Nprg0001ListView.SetItemText using result,NPRGPNAME,1
          call      Trim using NPRGTYPE
          Nprg0001ListView.SetItemText using result,NPRGTYPE,2
          call      Trim using NPRGDEV
          Nprg0001ListView.SetItemText using result,NPRGDEV,3
          call      Trim using NPRGDEV2
          Nprg0001ListView.SetItemText using result,NPRGDEV2,4
          call      Trim using NPRGSTATUS
          if (NPRGSTATUS = "1")
                    move      "Live",taskname
          elseif (NPRGSTATUS = "2")
                    move      "In Development",taskname
          elseif (NPRGSTATUS = "3")
                    move      "Archived",taskname
          else
                    clear     taskname
          endif
          Nprg0001ListView.SetItemText using result,taskname,5
          call      Trim using NPRGDATE
          if (NPRGDATE <> "")
                    unpack    NPRGDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    call      CVTJUL
                    move      JULDAYS,str5
          else
                    clear     str10
                    move      "0",str5
          endif
          //Following is out of sequence, but I want to ensure integrity of str5
          Nprg0001ListView.SetItemText using result,str5,8
          Nprg0001ListView.SetItemText using result,str10,6
          Nprg0001ListView.SetItemText using result,hold,7
        return

Nprg0001LoadScreen
          setitem Nprg0001EditID,0,NPRGNUM
          setitem Nprg0001EditName,0,NPRGPNAME
          setitem Nprg0001EditMenu,0,NPRGMENU
          setitem Nprg0001EditType,0,NPRGTYPE
          setitem Nprg0001EditDeveloper,0,NPRGDEV
          setitem Nprg0001EditLead,0,NPRGDEV2
          move      C0,N1
          move      NPRGSTATUS,N1
          add       C1,N1
          setitem Nprg0001ComboStatus,0,N1
          call      Trim using NPRGDATE
          if (NPRGDATE <> "")
                    unpack    NPRGDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
          else
                    clear     str10
          endif
          setitem Nprg0001EditDate,0,str10
          setitem Nprg0001EditNotes,0,NPRGNOTES
        return

Nprg0001Verify
          getitem Nprg0001EditID,0,NPRGNUM
          call      Trim using NPRGNUM
          move      C0,N5
          move      NPRGNUM,N5
          move      N5,NPRGNUM
          rep       zfill,NPRGNUM
.
          getitem Nprg0001EditName,0,NPRGPNAME
          call      Trim using NPRGPNAME
          if (NPRGPNAME = "")
                    alert     note,"Program name is required!",result
                    move      YES,ReturnFlag
                    setfocus Nprg0001EditName
                    return
          endif
.
          getitem Nprg0001EditMenu,0,NPRGMENU
          call      Trim using NPRGMENU
.
          getitem Nprg0001EditType,0,NPRGTYPE
          call      Trim using NPRGTYPE
.
          getitem Nprg0001EditDeveloper,0,NPRGDEV
          call      Trim using NPRGDEV
.
          getitem Nprg0001EditLead,0,NPRGDEV2
          call      Trim using NPRGDEV2
.
          getitem Nprg0001ComboStatus,0,N1
          if (N1 = 0 | N1 = 1)
                    clear     NPRGSTATUS
          else
                    sub       C1,N1
                    move      N1,NPRGSTATUS
          endif
.
          getitem Nprg0001EditDate,0,str10
          call      Trim using str10
          call      RemoveChar using str10,SLASH
          if (str10 <> "")
                    unpack    str10,MM,DD,CC,YY
                    pack      NPRGDATE,CC,YY,MM,DD
          else
                    clear     NPRGDATE
          endif
.
          getitem Nprg0001EditNotes,0,NPRGNOTES
          call      Trim using NPRGNOTES
        return
XRESIZE
           NPRG0001.Scale
           RETURN

.Include IO file
          include   nprgio.inc
        include comlogic.inc
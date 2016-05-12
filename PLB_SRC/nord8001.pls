........................................
. Program:      NORD8001.PLS
. Function:     NINORD8 New Record File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   August 8,2005
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
          include   nord8dd.inc
          include   nord4dd.inc
          include   nord5dd.inc
          include   nord6dd.inc
        include     compdd.inc
        include     cntdd.inc
          include   nsel2dd.inc
          include   nusedd.inc
Release    INIT    "1.52"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release  init      "1.51"  DLH LOAD users from file - always
.release init    "1.5"   JD  01MAR2007 ADDED NEW EMPLOYEES
.release init       "1.4"     DMS 24FEB2006 ADDED QTY SORT, MOVED COLUMN ORDER
.release init    "1.3"   ASH 18OCT2005 ADDED NEW EMPLOYEE
.release init    "1.2"   ASH 18OCT2005 ADDED NEW EMPLOYEE
.release init    "1.1"   ASH 06SEP2005 ADDED NEW EMPLOYEE
.release init    "1.0"   ASH 08AUG2005 ORIGINAL RELEASE
.          move      "March 1, 2007",Wreldate
.         move      "August 8, 2005",Wreldate

.
Timer   Timer
ExitFlag init   "Y"
ReturnFlag init "N"
NewFlag init    "N"
SecFlag   form      1
hold    dim     75  .ORD8VARS
holdkey dim     6
.
SearchLR dim        6
SearchFromDate dim  10
SearchToDate dim    10
Nord8Loop form      1
count1    form      9
count1a   form      9
count2    form      9
count2a   form      9
.
DimPtr    dim       ^
DimPtr1   dim       ^
FrmPtr    form      ^
AutPtr    automation          ^
.
ColHeads automation
ColHead   automation
ListIts   automation
ListIt    automation
SubIt     automation
IntIndex integer 4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
.
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
SheetsDefault   integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlRowHeight         variant
xlMinimized integer 4,"0xFFFFEFD4"
VT_R8     EQU 5           .Double - 8 byte Real

.Colors
white   color
grey    color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"

.Set Vars used for About Box
          move    "NORD8001.PLS",Wprognme
          move    "New Record File Maintenance",Wfunction
          move    "Andrew Harkins",Wauthor

.Declare forms, Always declare child forms first
mss1    plform  Error
abt     plform  About
x       plform  Nord8001
        winhide
.Load Forms, Always load parent form first
          formload x
          formload abt
          formload mss1

          CREATE  TIMER,18000     .30 minutes
          ACTIVATE TIMER,Timeout,RESULT
.Create Menus
          create  Nord8001;mFile,FData
          create  Nord8001;mEdit,EData,mFile
          create  Nord8001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
          activate mFile,FileGo,result
.Need this when it works
          activate mEdit,EditGo,result
.Only a SubMenu under this one
          activate mHelp,HelpGo,result
.
.turn always
..         move      "AH DB DM JD DH JG ",str25
.          move      "DH SA CLF",str25
.          scan      INITS,str25
.          if equal
                    move      C1,SecFlag
.          else
.                    setprop   Nord8Delete,height=0,tabid=0
.          endif

.Load User Names
          if (SecFlag = C1)
                    move      C1,NUSEPATH
                    loop
                              call      NUSESEQ
                              until over
                              insertitem Nord8DataUsers,9999,NUSEUSER
                    repeat
          else
                    insertitem Nord8DataUsers,9999,"KATINKA PARTRIDGE"
                    insertitem Nord8DataUsers,9999,"BECKY CHAVEZ"
                    insertitem Nord8DataUsers,9999,"JOEY GAMACHE"
                    insertitem Nord8DataUsers,9999,"AGNES ALVAREZ"
                    insertitem Nord8DataUsers,9999,"JOSEPH CABRAL"
                    insertitem Nord8DataUsers,9999,"SHERENE KELLY"
                    insertitem Nord8DataUsers,9999,"KEVIN BLAIR"
.START PATCH 1.1 ADDED LOGIC
                    insertitem Nord8DataUsers,9999,"JACK FORDER"
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.2 ADDED LOGIC
                    insertitem Nord8DataUsers,9999,"ELLENE HOLT"
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.2 ADDED LOGIC
                    insertitem Nord8DataUsers,9999,"KATHERINE ESPERANZA"
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.5 ADDED LOGIC
                    insertitem Nord8DataUsers,9999,"Rameen Gasery"
                    insertitem Nord8DataUsers,9999,"AARON CINCO"
                    insertitem Nord8DataUsers,9999,"BAHAR TABATABAI"
                    insertitem Nord8DataUsers,9999,"CAROL FRAZER"
                    insertitem Nord8DataUsers,9999,"MIELLE SULLIVAN"
                    insertitem Nord8DataUsers,9999,"MARTA YOHANNES"
.END PATCH 1.5 ADDED LOGIC
          endif
.Load List View Columns
        getprop Nord8ListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
.START PATCH 1.4 REPLACED LOGIC
.         ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
.         ColHeads.Add using *Index=2,*Key="one",*Text="New Status",*Width=75
.         ColHeads.Add using *Index=3,*Key="two",*Text="LR",*Width=70
.         ColHeads.Add using *Index=4,*Key="three",*Text="New Date",*Width=85
.         ColHeads.Add using *Index=5,*Key="four",*Text="User",*Width=100
.         ColHeads.Add using *Index=6,*Key="five",*Text="Cur. Status",*Width=75
.         ColHeads.Add using *Index=7,*Key="six",*Text="Order Date",*Width=75
.         ColHeads.Add using *Index=8,*Key="seven",*Text="Mail Date",*Width=75
.         ColHeads.Add using *Index=9,*Key="eight",*Text="Mailer",*Width=75
.         ColHeads.Add using *Index=10,*Key="nine",*Text="List",*Width=75
.         ColHeads.Add using *Index=11,*Key="ten",*Text="Select",*Width=75
.         ColHeads.Add using *Index=12,*Key="eleven",*Text="Qty",*Width=70,*Alignment=1
.         ColHeads.Add using *Index=13,*Key="twelve",*Text="PO",*Width=75
.         ColHeads.Add using *Index=14,*Key="thirteen",*Text="Complete Record",*Width=0
          ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
          ColHeads.Add using *Index=2,*Key="one",*Text="New Status",*Width=75
          ColHeads.Add using *Index=3,*Key="two",*Text="LR",*Width=70
          ColHeads.Add using *Index=4,*Key="three",*Text="New Date",*Width=85
          ColHeads.Add using *Index=5,*Key="four",*Text="User",*Width=100
          ColHeads.Add using *Index=6,*Key="five",*Text="Mail Date",*Width=75
          ColHeads.Add using *Index=7,*Key="six",*Text="Qty",*Width=70,*Alignment=1
          ColHeads.Add using *Index=8,*Key="seven",*Text="Cur. Status",*Width=75
          ColHeads.Add using *Index=9,*Key="eight",*Text="Order Date",*Width=75
          ColHeads.Add using *Index=10,*Key="nine",*Text="Mailer",*Width=75
          ColHeads.Add using *Index=11,*Key="ten",*Text="List",*Width=75
          ColHeads.Add using *Index=12,*Key="eleven",*Text="Select",*Width=75
          ColHeads.Add using *Index=13,*Key="twelve",*Text="PO",*Width=75
          ColHeads.Add using *Index=14,*Key="thirteen",*Text="Complete Record",*Width=0
.ENDPATCH 1.4 REPLACED LOGIC
.Sort Fields
          ColHeads.Add using *Index=15,*Key="fourteen",*Text="New Date Sort",*Width=0
          ColHeads.Add using *Index=16,*Key="fifteen",*Text="Order Date Sort",*Width=0
          ColHeads.Add using *Index=17,*Key="sixteen",*Text="Mail Date Sort",*Width=0
.START PATCH 1.4 ADDED LOGIC
          ColHeads.Add using *Index=18,*Key="seventeen",*Text="Qty Sort", *Width=0
.END PATCH 1.4 ADDED LOGIC
.Set some properties for ListView object
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.
          setprop   Nord8ListView,*HideColumnHeaders=OFALSE
          setprop   Nord8ListView,*HideSelection=OFALSE
          setprop   Nord8ListView,*FullRowSelect=OTRUE
          setprop   Nord8ListView,*MultiSelect=OTRUE
          setprop   Nord8ListView,*Sorted=OTRUE
          setprop   Nord8ListView,*SortOrder=0
          setprop   Nord8ListView,*SortKey=2
          setprop   Nord8ListView,*AllowColumnReorder=OTRUE
          setprop   Nord8ListView,*LabelEdit=1
          setprop   Nord8ListView,*View=3
        getprop Nord8ListView,*ListItems=ListIts
.
          clock     timestamp,timestamp
.
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
          setfocus Nord8EditSearchLR
.
          move      C1,NORDPATH
          setfocus Nord8DataType
.Main Loop
          clock   timestamp,timestamp
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
        if (ExitFlag = "Y" & Nord8Loop = C0)
                winshow
                    Stop
                shutdown
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return


.Disable Upper Screen
Nord8DisableUpper
          move    "N",ExitFlag
          setprop Nord8OK,enabled=0
          setprop Nord8Quit,enabled=1
          setprop Nord8Exit,enabled=0
          setprop Nord8Delete,enabled=0
          setprop Nord8Print,enabled=0
          setprop Nord8TypeFrom,enabled=0
          setprop Nord8TypeTo,enabled=0
          setprop Nord8UserFrom,enabled=0
          setprop Nord8UserTo,enabled=0
          setprop Nord8DataType,enabled=0
          setprop Nord8DataType2,enabled=0
          setprop Nord8DataUsers,enabled=0
          setprop Nord8DataUsers2,enabled=0
          setprop Nord8EditFromDate,enabled=0,bgcolor=grey
          setprop Nord8EditToDate,enabled=0,bgcolor=grey
          setprop Nord8EditSearchLR,enabled=0,bgcolor=grey
          setprop Nord8ListView,*enabled=0
        return

.Enable Upper Screen
Nord8EnableUpper
.Allow Exit
        move    "Y",ExitFlag
          setprop Nord8Quit,enabled=0
          setprop Nord8OK,enabled=1
          setprop Nord8Exit,enabled=1
          setprop Nord8TypeFrom,enabled=1
          setprop Nord8TypeTo,enabled=1
          setprop Nord8UserFrom,enabled=1
          setprop Nord8UserTo,enabled=1
          setprop Nord8DataType,enabled=1
          setprop Nord8DataType2,enabled=1
          setprop Nord8DataUsers,enabled=1
          setprop Nord8DataUsers2,enabled=1
          setprop Nord8EditFromDate,enabled=1,bgcolor=white
          setprop Nord8EditToDate,enabled=1,bgcolor=white
          setprop Nord8EditSearchLR,enabled=1,bgcolor=white
          setprop Nord8ListView,*enabled=1
        return

Nord8ReadFiles
          move      C1,Nord8Loop
          setprop   Nord8Quit,enabled=1
          if (SearchLR <> "")
                    pack      NORD8FLD,SearchLR
                    move      "NORD8KEY",Location
                    pack      KeyLocation,"Key: ",NORD8FLD
                    call      NORD8KEY
                    loop
                              until over
                              until (NORD8FLD <> NORD8LR)
                              eventcheck
                              if (Nord8Loop = C0)
                                        goto Nord8LoopBreak
                              endif
                              call      Nord8LoadListView
                              move      "NORD8KS",Location
                              call      NORD8KS
                    repeat
          else
                    Nord8DataUsers2.GetCount giving count1
                    Nord8DataType2.GetCount giving count2
                    if (count2 > 0)
                              for count2a,C1,count2
                                        getitem   Nord8DataType2,count2a,str45
                                        if (str45 = "LCR")
                                                  move      "l",str1
                                        elseif (str45 = "Pending Order")
                                                  move      "p",str1
                                        elseif (str45 = "Live Order")
                                                  move      "0",str1
                                        elseif (str45 = "Cancelled LCR")
                                                  move      "z",str1
                                        elseif (str45 = "Cancelled Pending")
                                                  move      "x",str1
                                        elseif (str45 = "Cancelled Order")
                                                  move      "X",str1
                                        elseif (str45 = "Billed Order")
                                                  move      "B",str1
                                        elseif (str45 = "Cancelled Billed")
                                                  move      "Q",str1
                                        else
                                                  clear     str1
                                        endif
                                        if (str1 <> "")
                                                  pack      NORD8FLD1,"01X",str1
                                        else
                                                  clear     NORD8FLD1
                                        endif
                                        if (count1 > 0)
                                                  for count1a,C1,count1
                                                            getitem   Nord8DataUsers2,count1a,str35
                                                            call      Trim using str35
                                                            if (str35 <> "")
                                                                      pack      NORD8FLD2,"02L",str35
                                                            else
                                                                      clear     NORD8FLD2
                                                            endif
                                                            call      Nord8ReadAAMFiles
                                                  repeat
                                        else
                                                  clear     NORD8FLD2
                                                  call      Nord8ReadAAMFiles
                                        endif
                              repeat
                    elseif (count1 > 0)
                              clear     NORD8FLD1
                              for count1a,C1,count1
                                        getitem   Nord8DataUsers2,count1a,str35
                                        call      Trim using str35
                                        if (str35 <> "")
                                                  pack      NORD8FLD2,"02L",str35
                                                  call      Nord8ReadAAMFiles
                                        endif
                              repeat
                    else
.Read Entire File
                              close     NORD8FLIST
                              move      C0,NORD8FLAG
                              loop
                                        call      NORD8SEQ
                                        until over
                                        eventcheck
                                        if (Nord8Loop = C0)
                                                  goto Nord8LoopBreak
                                        endif
                                        call      Nord8PreLoadListView
                              repeat
                    endif
          endif
Nord8LoopBreak
          setprop   Nord8Quit,enabled=0
          getprop   ListIts,*Count=N9
          if (N9 <> C0)
                    if (N9 = 1)
                              pack      taskname,"1 Record Found."
                    else
                              move      N9,str9
                              call      FormatNumeric using str9,str11
                              pack      taskname,str11," Records Found."
                    endif
                    setitem   Nord8StatRecords,0,taskname
                    for IntIndex,"1",N9
                              setprop   ListIts(IntIndex),*Selected=C0
                    repeat
                    setprop   ListIts(1),*Selected=C1
                    if (SecFlag = C1)
                              setprop   Nord8Delete,enabled=1
                    endif
                    setprop Nord8Print,enabled=1
          else
                    setitem   Nord8StatRecords,0,"No Records Found."
                    setprop   Nord8Delete,enabled=0
                    setprop Nord8Print,enabled=0
          endif
          move      C0,Nord8Loop
          return

Nord8ReadAAMFiles
          if (NORD8FLD1 <> "" | NORD8FLD2 <> "")
                    move      "NORD8AIM",Location
                    pack      KeyLocation,"Key: ",NORD8FLD1,COMMA,NORD8FLD2
                    call      NORD8AIM
                    loop
                              until over
                              eventcheck
                              if (Nord8Loop = C0)
                                        noreturn
                                        goto Nord8LoopBreak
                              endif
                              call      Nord8PreLoadListView
                              move      "NORD8KG",Location
                              call      NORD8KG
                    repeat
          endif
          return

Nord8PreLoadListView
          if (NORD8DATE < SearchFromDate | NORD8DATE > SearchToDate)
                    return
          endif
Nord8LoadListView Routine FrmPtr
JD
          if (SecFlag <> C1)
                    scan      "KATINKA PARTRIDGE",NORD8USER
                    if not equal
                              scan      "BECKY CHAVEZ",NORD8USER
                              if not equal
                                        scan      "JOEY GAMACHE",NORD8USER
                                        if not equal
                                                  scan      "AGNES ALVAREZ",NORD8USER
                                                  if not equal
                                                            scan      "JOSEPH CABRAL",NORD8USER
                                                            if not equal
                                                                      scan      "SHERENE KELLY",NORD8USER
                                                                      if not equal
                                                                                scan      "KEVIN BLAIR",NORD8USER
                                                                                if not equal
.START PATCH 1.1 REPLACED LOGIC
.                                                                                         return
                                                                                          scan      "JACK FORDER",NORD8USER
                                                                                          if not equal
.START PATCH 1.2 REPLACED LOGIC
.                                                                                         return
                                                                                                    scan      "ELLENE HOLT",NORD8USER
                                                                                                    if not equal
.START PATCH 1.3 REPLACED LOGIC
.                                                                                                             return
                                                                                                              scan      "Rameen Gasery",NORD8USER
                                                                                                              if not equal
                                                                                                                        return
                                                                                                              endif
                                                                                                              scan      "AARON CINCO",NORD8USER
                                                                                                              if not equal
                                                                                                                        return
                                                                                                              endif
                                                                                                              scan      "BAHAR TABATABAI",NORD8USER
                                                                                                              if not equal
                                                                                                                        return
                                                                                                              endif
                                                                                                              scan      "MARTA YOHANNES",NORD8USER
                                                                                                              if not equal
                                                                                                                        return
                                                                                                              endif
                                                                                                              scan      "CAROL FRAZER",NORD8USER
                                                                                                              if not equal
                                                                                                                        return
                                                                                                              endif
.END PATCH 1.3 REPLACED LOGIC
                                                                                                    endif
.END PATCH 1.2 REPLACED LOGIC
                                                                                          endif
.END PATCH 1.1 REPLACED LOGIC
                                                                                endif
                                                                      endif
                                                            endif
                                                  endif
                                        endif
                              endif
                    endif
          endif

          ListIts.Add giving ListIt using *Index=1,*Text=""
.'New' LR Status
          call      Nord8LoadOstat using NORD8OSTAT,str45
          setprop ListIt,*SubItems(1)=str45
.LR Number
          setprop ListIt,*SubItems(2)=NORD8LR
.'New' Date
          call      Trim using NORD8DATE
          if (NORD8DATE <> "")
                    unpack    NORD8DATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    call      CVTJUL
          else
                    clear     str10
                    move      C0,JULDAYS
          endif
          move      JULDAYS,str5
          rep       zfill,str5
          setprop ListIt,*SubItems(3)=str10
          setprop ListIt,*SubItems(14)=str5
.User
          setprop ListIt,*SubItems(4)=NORD8USER
.Format
          getprop   Listit,*ListSubItems=SubIt
          setprop   SubIt(1),*Bold=OTRUE
          setprop   SubIt(2),*Bold=OTRUE
          setprop   SubIt(3),*Bold=OTRUE
          setprop   SubIt(4),*Bold=OTRUE
.Values from NINORD
          pack      NORDFLD,NORD8LR
          move      "NORDKEY",Location
          pack      KeyLocation,"Key: ",NORDFLD
          call      NORDKEY
          if not over
                    call      Nord8LoadOstat using OSTAT,str45
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(5)=str45
                    setprop ListIt,*SubItems(7)=str45
.END PATCH 1.4 REPLACED LOGIC
.Order Date
                    call      Trim using OODTEM
                    if (OODTEM <> "")
                              move      OODTEM,MM
                              move      OODTED,DD
                              move      OODTEC,CC
                              move      OODTEY,YY
                              pack      str10,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
                              call      CVTJUL
                    else
                              clear     str10
                              move      C0,JULDAYS
                    endif
                    move      JULDAYS,str5
                    rep       zfill,str5
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(6)=str10
                    setprop ListIt,*SubItems(8)=str10
.END PATCH 1.4 REPLACED LOGIC
                    setprop ListIt,*SubItems(15)=str5
.Mail Date
                    call      Trim using OMDTEM
                    if (OMDTEM <> "")
                              move      OMDTEM,MM
                              move      OMDTED,DD
                              move      OMDTEC,CC
                              move      OMDTEY,YY
                              pack      str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                              call      CVTJUL
                    else
                              clear     str10
                              move      C0,JULDAYS
                    endif
                    move      JULDAYS,str5
                    rep       zfill,str5
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(7)=str10
                    setprop ListIt,*SubItems(5)=str10
.END PATCH 1.4 REPLACED LOGIC
                    setprop ListIt,*SubItems(16)=str5
.Mailer Name
                    pack      COMPFLD3,OMLRNUM
                    move      "COMPKEY3",Location
                    pack      KeyLocation,COMPFLD3
                    call      COMPKEY3
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(8)=COMPCOMP
                    setprop ListIt,*SubItems(9)=COMPCOMP
.END PATCH 1.4 REPLACED LOGIC
.List Name
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(9)=O1DES
                    setprop ListIt,*SubItems(10)=O1DES
.END PATCH 1.4 REPLACED LOGIC
.Select
                    packkey   NSEL2FLD,"1",OLRN
                    move      "NSEL2KEY",Location
                    pack      KeyLocation,"Key: ",NSEL2FLD
                    call      NSEL2KEY
                    if over
                              move      O2DES,NSEL2NAME
                    endif
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(10)=NSEL2NAME
                    setprop ListIt,*SubItems(11)=NSEL2NAME
.END PATCH 1.4 REPLACED LOGIC
.Order Qty
                    call      Trim using OQTY
                    if (OQTY <> "")
                              move      OQTY,str9
                              call      FormatNumeric using str9,str11
                    else
                              clear     str11
                    endif
.START PATCH 1.4 REPLACED LOGIC
.                   setprop ListIt,*SubItems(11)=str11
                    setprop ListIt,*SubItems(6)=str11
.Qty Search
                    call      ZFillIt using OQTY,C0
                    setprop ListIt,*SubItems(17)=OQTY
.END PATCH 1.4 REPLACED LOGIC

.PO Number
                    setprop ListIt,*SubItems(12)=OMLRPON
.Complete Record
                    pack      hold,ORD8VARS
                    setprop ListIt,*SubItems(13)=hold
          endif
        return

Nord8LoadOstat Routine DimPtr,DimPtr1
          if (DimPtr = "l")
                    pack      DimPtr1,"LCR"
          elseif (DimPtr = "z")
                    pack      DimPtr1,"Cancelled LCR"
          elseif (DimPtr = "p")
                    pack      DimPtr1,"Pending Order"
          elseif (DimPtr = "x")
                    pack      DimPtr1,"Cancelled Pending"
          elseif (DimPtr = "0")
                    pack      DimPtr1,"Live Order"
          elseif (DimPtr = "B")
                    pack      DimPtr1,"Billed Order"
          elseif (DimPtr = "Q")
                    pack      DimPtr1,"Cancelled-Billed"
          elseif (DimPtr = "X")
                    pack      DimPtr1,"Cancelled Order"
          else
                    clear     DimPtr1
          endif
          return

Nord8CreateReport Routine AutPtr
.Open Excel application
        create  ex
.begin patch 1.52
.        setprop ex,*WindowState=xlMinimized
.end patch 1.52
        setprop ex,*Visible="False"
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
        setprop sheet.PageSetup,*Orientation=xlLandscape
.Create Header
          setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet.range("A1:E1").Merge
          sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
          setprop sheet.range("A5"),*Value="Status"
          setprop sheet.range("B5"),*Value="LR"
          setprop sheet.range("C5"),*Value="Date"
          setprop sheet.range("D5"),*Value="User"
          sheet.range("A5:D5").BorderAround using *LineStyle=1,*Weight=2
          setprop sheet.range("A5:D5").Font,*Bold="True"
          move      "6",result
.
          for IntIndex,"1",N9
                    getprop   AutPtr(IntIndex),*Selected=N5
                    if (N5 <> 0)
                              getprop   AutPtr(IntIndex),*SubItems(13)=hold
                              unpack    hold,ORD8VARS
                              call      Trim using NORD8LR
                              call      Trim using NORD8OSTAT
                              call      Trim using NORD8DATE
                              call      Trim using NORD8USER
                              add       C1,result
                              move      result,str9
                              call      Trim using str9
                              call      Nord8LoadOstat using NORD8OSTAT,str45
                              pack      str11,"A",str9
                              setprop sheet.range(str11),*Value=str45
                              pack      str11,"B",str9
                              setprop sheet.range(str11),*Value=NORD8LR
                              if (NORD8DATE <> "")
                                        unpack    NORD8DATE,str4,MM,DD
                                        pack      str10,MM,SLASH,DD,SLASH,str4
                              else
                                        clear     str10
                              endif
                              pack      str11,"C",str9
                              setprop sheet.range(str11),*Value=str10
                              pack      str11,"D",str9
                              setprop sheet.range(str11),*Value=NORD8USER
                    endif
          repeat
          pack    str10,"A1"
          pack    str11,"A",str9
          sheet.range(str10,str11).Columns.Autofit
          pack    str10,"B1"
          pack    str11,"B",str9
          sheet.range(str10,str11).Columns.Autofit
          pack    str10,"C1"
          pack    str11,"C",str9
          sheet.range(str10,str11).Columns.Autofit
          pack    str10,"D1"
          pack    str11,"D",str9
          sheet.range(str10,str11).Columns.Autofit
          setprop sheet.range("A3"),*Value="New Order Report"
          setprop sheet.range("A3").Font,*Bold="True"
          setprop ex,*Visible="True"
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
        destroy ex
          return
XRESIZE
           NORD8001.Scale
           RETURN

.Include IO file
        include nordio.inc
          include   nord8io.inc
          include   nord4io.inc
          include   nord5io.inc
          include   nord6io.inc
          include   compio.inc
          include   cntio.inc
          include   nsel2io.inc
          include   nuseio.inc
        include comlogic.inc
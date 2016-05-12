.LIST MANAGEMENT CANCELLATION/DENIED REPORT PROGRAM
        include common.inc
        include cons.inc
        include norddd.inc
        include nord4dd.inc
        include nord5dd.inc
        include nspedd.inc
        include nspe2dd.inc
        include npnddd.inc
.patch1.2
                                        include   compdd.inc
                                        include   cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
.patch1.2
        include ndatdd.inc
        include ncntdd.inc
Release   Init      "1.32"    DLH       can't use mapped drive, switch to internal verb
Reldate   INit      "23 APril 2008"
.release  init      "1.31"        JD       23Nov2005        PLB 9.0 new aimdex ver.
.release  init      "1.3"       ASH     20JUL2004 Work Order 471 - Include Mailer Notes
.release  init      "1.2"       DMB     26MAY2004 Mailer Conversion
.elease init    "1.1"         ASH 06MAY2004 ADDED OPTION FOR LIVE/BILLED ORDERS
.release init    "1.0"        ASH 15JAN2004 NEW RELEASE

FrmPtr    form      ^
.
.START PATCH 1.3 REMOVED VAR - NOW SITS IN CONS.INC
.tempfile file
.END PATCH 1.3 REMOVED VAR - NOW SITS IN CONS.INC
FromDate dim        10
ToDate    dim       10
FromDateB form      8
ToDateB   form      8
RecType   dim       1
RepType   dim       1                   .1=Cancelled & Denied, 2=Cancelled Only,3=Denied only
DeptType dim        1                   .1=all, 2= List Management, 3=Brokerage
DateType dim        1                   .1=**Defaults to Denied/Cancelled Date
PackData DataList
PackData2 DataList
FirstRec form       1
FirstRec2 form      1
PIndex    form      9(2)
LIndex    form      9(2)
CurLine   form      9
TotLine1 form       9
TotLine2 form       9
PTotal    form      9(2)
LTotal    form      9(2)
.In order to use any of the properties/methods associated with all parent objects
.of the Worksheet, I need to create automation objects for each of them.
.
.Look at Excel Object Model to understand heirarchy.  This can be found in hard
.documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.Software available via PL/B Designer - create a Container object on a form, create an Excel
.Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
.view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.option to locate Help topics to see specifics.
.
.General heirarchy:
. Excel Application
.       Workbooks Collection (all open Workbooks)
.               Single Workbook
.                       Worksheets Collection (all Worksheets in this Workbook)
.                               Single Worksheet
.                                       SortColumn (a Single Column in that Worksheet used for sorting)
.
books   automation
book    automation
sheets  automation
sheet1  automation
sortcol automation
ex      automation      class="Excel.Application"

.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom99 variant
Zoom85  variant
.START PATCH 1.3 ADDED LOGIC
Zoom76  variant
.END PATCH 1.3 ADDED LOGIC
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignTop integer 4,"0XFFFFEFC0"
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
DblLine integer 4,"0xffffefe9"
BorderHor integer 4,"0xc"
        
          winhide
.Test to make sure input file is available
          trap      IOFileError if IO
          open      tempfile,"c:\work\LCRDeny.dat"
          trapclr   IO
          create    PackData=1:1:1:1
          create    PackData2=1:1:1:1
.
          read      tempfile,SEQ;str8
          call      Trim using str8
          move      str8,FromDateB
          if (str8 <> "" & str8 <> "00000000")
                    unpack    str8,str4,MM,DD
                    pack      FromDate,MM,SLASH,DD,SLASH,str4
          else
                    clear     FromDate
          endif
.
          read      tempfile,SEQ;str8
          call      Trim using str8
          move      str8,ToDateB
          if (str8 <> "" & str8 <> "00000000")
                    unpack    str8,str4,MM,DD
                    pack      ToDate,MM,SLASH,DD,SLASH,str4
          else
                    clear     ToDate
          endif
.
          read      tempfile,SEQ;RecType
          read      tempfile,SEQ;RepType
          read      tempfile,SEQ;DeptType
          read      tempfile,SEQ;DateType
.
          loop
                    read      tempfile,SEQ;str12
                    until over
                    unpack    str12,str6,str7
                    if (str6 = "LIST  ")
                              insertitem PackData,999999,str7
                    elseif (str6 = "MAILER")
                              insertitem PackData2,999999,str7
                    endif
          repeat
.....TEST........
.         GOTO AFTERaam
.         goto beforewrite

AAMDEX
          close     tempfile
          clear     taskname
.         pack      taskname,"!f:\apps\plb\code\sunadxnt \\nins1\e\data\text\ninord.dat c:\work\ninlcr l408 -2,196-197,16-21"
.>Patch 1.31 Begin
.         pack      taskname,"!f:\apps\plb\code\sunaamdx \\nins1\e\data\text\ninord.dat c:\work\ninlcr l408 -2,196-197,16-21"
.begin patch 1.32
.         pack      taskname,"!\\nts0\c\apps\plb\code\sunaamdx \\nins1\e\data\text\ninord.dat c:\work\ninlcr l408 -2,196-197,16-21"
          pack      taskname,"\\nins1\e\data\text\ninord.dat c:\work\ninlcr l408 -2,196-197,16-21"
.>Patch 1.31 End
          AAmdex    Taskname
.         execute   taskname
.end patch 1.32
beforewrite
.
          pack      NORDNME2,"c:\work\ninlcr"
          call      NORDOPN2
.
          erase     "c:\work\LCRDeny2.dat"
          prepare   tempfile,"c:\work\LCRDeny2.dat"
          if (DeptType = "2")
                    pack      NORDFLD2,"02X06"
          endif
          clear     howmany             .Initialize counter
          if (RecType = "1" | RecType = "3")
                    if (RepType = "1" | RepType = "3")
                              pack      NORDFLD1,"01Xp"
                              call      OrderReadRecords
                    endif
                    if (RepType = "1" | RepType = "2")
                              pack      NORDFLD1,"01Xx"
                              call      OrderReadRecords
                    endif
          endif
          if (RecType = "1" | RecType = "2")
                    if (RepType = "1" | RepType = "3")
                              pack      NORDFLD1,"01Xl"
                              call      OrderReadRecords
                    endif
                    if (RepType = "1" | RepType = "2")
                              pack      NORDFLD1,"01Xz"
                              call      OrderReadRecords
                    endif
          endif
.START PATCH 1.1 ADDED LOGIC
          if (RecType = "4")
                    pack      NORDFLD1,"01XX"
                    call      OrderReadRecords
                    pack      NORDFLD1,"01XQ"
                    call      OrderReadRecords
                    move      C0,DateType
          endif
.END PATCH 1.1 ADDED LOGIC
          close     tempfile
          if (howmany = 0)
                    alert     note,"No records were found matching your criteria!!",result
                    shutdown "cls"
          endif
...Create the Variant objects
AFTERaam
.Booleans
          create  OTRUE,VarType=VT_BOOL,VarValue=1
          create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
          create  Zoom99,VarType=VT_I4,VarValue=99
          create  Zoom85,VarType=VT_I4,VarValue=85
.START PATCH 1.3 ADDED LOGIC
          create  Zoom76,VarType=VT_I4,VarValue=76
.END PATCH 1.3 ADDED LOGIC
.Open Excel application
          create  ex
          setprop ex,*WindowState=xlMinimized
          setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.Reset Default of Worksheets found in a Workbook
          getprop ex,*SheetsInNewWorkbook=SheetsDefault
          setprop ex,*SheetsInNewWorkbook=1
.Create Workbooks collection
          getprop ex,*Workbooks=books
.Create/Add a single Workbook
          books.add
          books.item giving book using 1
.Create Worksheets collection
          getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet1 using 1
          setprop sheet1.PageSetup,*Orientation=xlLandscape
          setprop sheet1.PageSetup,*CenterFooter=" Page &P of &N"
.Margins are set up in POINTS.  One inch equals 72 points.  I wanted .25" so the following calculation gave me my figure:  72/4=18
          setprop sheet1.PageSetup,*HeaderMargin="18"
          setprop sheet1.PageSetup,*FooterMargin="18"
          setprop sheet1.PageSetup,*TopMargin="18"
          setprop sheet1.PageSetup,*BottomMargin="18"
          setprop sheet1.PageSetup,*LeftMargin="18"
          setprop sheet1.PageSetup,*RightMargin="18"
.START PATCH 1.3 ADDED LOGIC
          setprop sheet1.PageSetup,*PrintGridlines=OTrue
          setprop sheet1.PageSetup,*PrintTitleRows="16:17"
.END PATCH 1.3 ADDED LOGIC

.Set Viewing Sizes
.As the first sheet is always inherently active I do not need to activate it
.         sheet1.activate
.START PATCH 1.3 REPLACED LOGIC
..Set Zoom property
.         setprop book.windows(1),*Zoom=Zoom99
..Set Printing Sizes
.         setprop sheet1.PageSetup,*Zoom=Zoom85
.Set Zoom property
          setprop book.windows(1),*Zoom=Zoom85
.Set Printing Sizes
          setprop sheet1.PageSetup,*Zoom=Zoom76
.END PATCH 1.3 REPLACED LOGIC
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
.        sheet2.range("A1","IV1000").Clear
.Greatest column possible is "IV"
          setprop   sheet1.Range("A1","IV1000").Font,*Size=10,*Bold="True"

.....Report Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
..................................
          setprop sheet1.range("A2"),*Value="Cancellation/Denied Report"
          setprop sheet1.range("A2").Font,*Size=14
.
          setprop sheet1.range("B4"),*Value="Date Parameters:"
          if (FromDate <> "" & FromDate <> "00/00/0000")
                    if (ToDate <> "" & ToDate <> "99/99/9999")
                              pack      str55,FromDate," - ",ToDate
                    else
                              pack      str55,"From ",FromDate
                    endif
          elseif (ToDate <> "" & ToDate <> "99/99/9999")
                    pack      str55,"To ",ToDate
          else
                    clear     str55
          endif
          setprop sheet1.range("B5"),*Value=str55
          if (DateType = "1")
                    setprop sheet1.range("B6"),*Value="**Defaults to Denied/Cancelled Date"
                    setprop sheet1.range("B6").Font,*Size=8,*Italic="True"
          endif
.
          setprop sheet1.range("C4"),*Value="Record Type:"
          if (RecType = "1")
                    pack      str55,"LCRs & Pending Orders"
          elseif (RecType = "2")
                    pack      str55,"LCRs Only"
          elseif (RecType = "3")
                    pack      str55,"Pending Orders Only"
.START PATCH 1.1 ADDED LOGIC
          elseif (RecType = "4")
                    pack      str55,"Live/Billed Orders Only"
.END PATCH 1.1 ADDED LOGIC
          else      .Not really possible
                    clear     str55
          endif
          setprop sheet1.range("C5"),*Value=str55
.
          setprop sheet1.range("D4"),*Value="Report Type:"
          if (RepType = "1")
                    pack      str55,"Cancelled & Denied"
          elseif (RepType = "2")
                    pack      str55,"Cancelled Only"
          elseif (RepType = "3")
                    pack      str55,"Denied Only"
          else      .Not really possible
                    clear     str55
          endif
          setprop sheet1.range("D5"),*Value=str55

.
          setprop sheet1.range("E4"),*Value="Department:"
          if (DeptType = "1")
                    pack      str55,"All Teams"
          elseif (DeptType = "2")
                    pack      str55,"List Management Only"
          elseif (DeptType = "3")
                    pack      str55,"Brokerage Only"
          else      .Not really possible
                    clear     str55
          endif
          setprop sheet1.range("E5"),*Value=str55
.
          move      C4,N2
          PackData.GetCount giving howmany
          if (howmany > C0)
                    setprop sheet1.range("F4"),*Value="List Filter:"
                    for result,"1",howmany
                              getitem   PackData,result,str6
                              add       C1,N2
                              move      N2,str2
                              call      Trim using str2
                              pack      str3,"F",str2
                              setprop sheet1.range(str3),*Value=str6
                    repeat
          endif
.
          move      C4,N3
          PackData2.GetCount giving howmany
          if (howmany > C0)
                    setprop sheet1.range("G4"),*Value="Mailer Filter:"
                    for result,"1",howmany
                              getitem   PackData2,result,str6
                              add       C1,N3
                              move      N3,str3
                              call      Trim using str3
                              pack      str4,"G",str3
                              setprop sheet1.range(str4),*Value=str6
                    repeat
          endif
.
          if (N2 > N3)
                    move      N2,N3
          endif
          add       C3,N3,CurLine
.
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"C",str9
.START PATCH 1.1 REPLACED LOGIC
.         setprop sheet1.range(str10),*Value="## of LR/LCR's",*HorizontalAlignment=AlignRight
          if (RecType = "4")
                    setprop sheet1.range(str10),*Value="## of LR's",*HorizontalAlignment=AlignRight
          else
                    setprop sheet1.range(str10),*Value="## of LR/LCR's",*HorizontalAlignment=AlignRight
          endif
.END PATCH 1.1 REPLACED LOGIC
          pack      str10,"D",str9
          setprop sheet1.range(str10),*Value="Quantity",*HorizontalAlignment=AlignRight
          add       C2,CurLine
          move      CurLine,TotLine1
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"B",str9
          move      str10,str12
.START PATCH 1.1 REPLACED LOGIC
.         setprop sheet1.range(str10),*Value="Pending Orders"
          if (RecType = "4")
                    setprop sheet1.range(str10),*Value="Cancelled Orders"
          else
                    setprop sheet1.range(str10),*Value="Pending Orders"
          endif
.END PATCH 1.1 REPLACED LOGIC
.
          add       C1,CurLine
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"B",str9
.START PATCH 1.1 REPLACED LOGIC
.         setprop sheet1.range(str10),*Value="LCRs"
          if (RecType = "4")
                    setprop sheet1.range(str10),*Value="Cancelled/Billed Orders"
          else
                    setprop sheet1.range(str10),*Value="LCRs"
          endif
.END PATCH 1.1 REPLACED LOGIC
.
          move      CurLine,TotLine2
          add       C2,CurLine
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"B",str9
          setprop sheet1.range(str10),*Value="Grand Total:"
          pack      str11,"D",str9
.START PATCH 1.3 REMOVED LOGIC
.        setprop    sheet1.range(str12,str11).Borders(8),*LineStyle=1                     .Top Line, Single
.        setprop    sheet1.range(str12,str11).Borders(BorderHor),*LineStyle=1             .Horizontal Borders/Lines, Single
.        setprop    sheet1.range(str12,str11).Borders(9),*LineStyle=DblLine                         .Bottom Line, Double
.END PATCH 1.3 REMOVED LOGIC
.
          add       C3,CurLine
.Write Out Records
          move      C1,NDATPATH
          move      C1,NMLRPATH
          move      C1,NBRKPATH
          open      tempfile,"c:\work\LCRDeny2.dat"
          loop
                    read      tempfile,SEQ;ORDVARS,str55
                    until over
                    if (FirstRec = 0 & (OSTAT = "p" | OSTAT = "x"))
                              call      OrderLoadSubHeader using C0
                              move      C1,FirstRec
                              add       C1,CurLine,PIndex(1)
                    elseif (FirstRec2 = 0 & (OSTAT = "l" | OSTAT = "z"))
                              if (FirstRec = 1)
                                        move      CurLine,PIndex(2)
                                        add       C3,CurLine
                              else
                                        move      PIndex(1),PIndex(2)
                              endif
                              call      OrderLoadSubHeader using C1
                              move      C1,FirstRec2
                              add       C1,CurLine,LIndex(1)
.START PATCH 1.1 ADDED LOGIC
                    elseif (FirstRec = 0 & OSTAT = "X")
                              call      OrderLoadSubHeader using C0
                              move      C1,FirstRec
                              add       C1,CurLine,PIndex(1)
                    elseif (FirstRec2 = 0 & OSTAT = "Q")
                              if (FirstRec = 1)
                                        move      CurLine,PIndex(2)
                                        add       C3,CurLine
                              else
                                        move      PIndex(1),PIndex(2)
                              endif
                              call      OrderLoadSubHeader using C1
                              move      C1,FirstRec2
                              add       C1,CurLine,LIndex(1)
.END PATCH 1.1 ADDED LOGIC
                    endif
                    add       C1,CurLine
                    move      CurLine,str9
                    call      Trim using str9
                    pack      str10,"A",str9
                    setprop sheet1.range(str10),*Value=OLRN,*HorizontalAlignment=AlignLeft
                    pack      str10,"B",str9
                    setprop sheet1.range(str10),*Value=str55,*WrapText="True"
                    packkey   NDATFLD,OLNUM
                    move      "NDATKEY",Location
                    pack      KeyLocation,NDATFLD
                    call      NDATKEY
                    call      Trim using OLSTNAME
                    pack      str10,"C",str9
                    setprop sheet1.range(str10),*Value=OLSTNAME,*WrapText="True"
                    packkey   MKEY,OMLRNUM,"000"
                    move      "NMLRKEY",Location
                    pack      KeyLocation,MKEY
                    call      NMLRKEY
                    call      Trim using MCOMP
                    pack      str10,"D",str9
                    setprop sheet1.range(str10),*Value=MCOMP,*WrapText="True"
                    packkey   NBRKFLD,OBRKNUM,"000"
                    move      "NBRKKEY",Location
                    pack      KeyLocation,NBRKFLD
                    call      NBRKKEY
                    call      Trim using BRCOMP
                    pack      str10,"E",str9
                    setprop sheet1.range(str10),*Value=BRCOMP,*WrapText="True"
                    packkey   NCNTFLD,OCO2CODE
                    move      "NCNTKEY",Location
                    pack      KeyLocation,NCNTFLD
                    call      NCNTKEY
                    call      Trim using CNTNAME
                    pack      str10,"F",str9
                    setprop sheet1.range(str10),*Value=CNTNAME,*WrapText="True"
                    pack      str10,"G",str9
                    setprop sheet1.range(str10),*Value=OQTY,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0_);(##,####0)"
                    packkey   NSPEFLD,OLRN
                    move      "NSPEKEY",Location
                    pack      KeyLocation,NSPEFLD
                    call      NSPEKEY
                    pack      str2,newline,B1
                    rep       str2,DESC002
                    call      Trim using DESC002
                    pack      str10,"H",str9
                    setprop sheet1.range(str10),*Value=DESC002,*WrapText="True"
                    setprop sheet1.range(str10).Font,*Size=8
                    packkey   NSPE2FLD,OLRN
                    move      "NSPE2KEY",Location
                    pack      KeyLocation,NSPE2FLD
                    call      NSPE2KEY
                    rep       str2,DESC003
                    call      Trim using DESC003
                    pack      str10,"I",str9
                    setprop sheet1.range(str10),*Value=DESC003,*WrapText="True"
                    setprop sheet1.range(str10).Font,*Size=8
.START PATCH 1.3 ADDED LOGIC
                    rep       str2,DESC004
                    call      Trim using DESC004
                    pack      str10,"J",str9
                    setprop sheet1.range(str10),*Value=DESC004,*WrapText="True"
                    setprop sheet1.range(str10).Font,*Size=8
.END PATCH 1.3 ADDED LOGIC
.
                    pack      str11,"A",str9
                    setprop sheet1.range(str11,str10),*VerticalAlignment=AlignTop
          repeat
          if (PIndex(2) = 0 & PIndex(1) <> 0)
                    move      CurLine,PIndex(2)
          endif
          move      CurLine,LIndex(2)
.Total Section
.  Pending Records
          if (PIndex(1) <> C0 & PIndex(2) <> C0)
                    move      PIndex(1),str9
                    call      Trim using str9
                    pack      str11,"A",str9
                    move      PIndex(2),str10
                    call      Trim using str10
                    pack      str12,"A",str10
                    pack      taskname,"=COUNTIF(",str11,":",str12,",#">1#")"
                    move      TotLine1,str15
                    call      Trim using str15
                    pack      str25,"C",str15
                    setprop   sheet1.range(str25),*Formula=taskname
          else
                    move      TotLine1,str15
                    call      Trim using str15
                    pack      str25,"C",str15
                    setprop   sheet1.range(str25),*Value="0"
          endif
.
          pack      str25,"D",str15
          if (PIndex(1) <> C0 & PIndex(2) <> C0)
                    pack      taskname,"=SUM(G",str9,":G",str10,")"
                    setprop   sheet1.range(str25),*Formula=taskname
          else
                    setprop   sheet1.range(str25),*Value="0"
          endif
.  LCRs
          if (LIndex(1) <> C0 & LIndex(2) <> C0)
                    move      LIndex(1),str9
                    call      Trim using str9
                    pack      str11,"A",str9
                    move      LIndex(2),str10
                    call      Trim using str10
                    pack      str12,"A",str10
                    pack      taskname,"=COUNTIF(",str11,":",str12,",#">1#")"
                    move      TotLine2,str15
                    call      Trim using str15
                    pack      str25,"C",str15
                    setprop   sheet1.range(str25),*Formula=taskname
          else
                    move      TotLine2,str15
                    call      Trim using str15
                    pack      str25,"C",str15
                    setprop   sheet1.range(str25),*Value="0"
          endif
.
          pack      str25,"D",str15
          if (LIndex(1) <> C0 & LIndex(2) <> C0)
                    pack      taskname,"=SUM(G",str9,":G",str10,")"
                    setprop   sheet1.range(str25),*Formula=taskname
          else
                    setprop   sheet1.range(str25),*Value="0"
          endif
.Grand Totals
          move      TotLine1,str9
          call      Trim using str9
          move      TotLine2,str10
          call      Trim using str10
          add       C2,TotLine2,result
          move      result,str11
          call      Trim using str11
          pack      taskname,"=SUM(C",str9,":C",str10,")"
          pack      str15,"C",str11
          setprop   sheet1.range(str15),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
          pack      taskname,"=SUM(D",str9,":D",str10,")"
          pack      str15,"D",str11
          setprop   sheet1.range(str15),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.General Formatting of Document
....Sort by List Name...
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
          if (PIndex(1) <> C0 & PIndex(2) <> C0)
                    move      PIndex(1),str9
                    call      Trim using str9
                    move      PIndex(2),str10
                    call      Trim using str10
                    pack    str25,"C",str9,":C",str10
                    getprop sheet1.range(str25),*Columns(1)=sortcol
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str25,"A",str9,":I",str10
                    pack    str25,"A",str9,":J",str10
.END PATCH 1.3 REPLACED LOGIC
                    sheet1.range(str25).sort using *Key1=sortcol,*Order1=1
          endif
.
          if (LIndex(1) <> C0 & LIndex(2) <> C0)
                    move      LIndex(1),str9
                    call      Trim using str9
                    move      LIndex(2),str10
                    call      Trim using str10
                    pack    str25,"C",str9,":C",str10
                    getprop sheet1.range(str25),*Columns(1)=sortcol
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str25,"A",str9,":I",str10
                    pack    str25,"A",str9,":J",str10
.END PATCH 1.3 REPLACED LOGIC
                    sheet1.range(str25).sort using *Key1=sortcol,*Order1=1
          endif
....Set Column Widths...
.        sheet1.range(range1,range2).Columns.Autofit
          setprop sheet1.Columns("A"),*ColumnWidth="7"
          setprop sheet1.Columns("B"),*ColumnWidth="20"
          setprop sheet1.Columns("C"),*ColumnWidth="22"
          setprop sheet1.Columns("D"),*ColumnWidth="22"
          setprop sheet1.Columns("E"),*ColumnWidth="22"
          setprop sheet1.Columns("F"),*ColumnWidth="14.5"
          setprop sheet1.Columns("G"),*ColumnWidth="8.5"
          setprop sheet1.Columns("H"),*ColumnWidth="18.5"
          setprop sheet1.Columns("I"),*ColumnWidth="18.5"
.START PATCH 1.3 ADDED LOGIC
          setprop sheet1.Columns("J"),*ColumnWidth="18.5"
.END PATCH 1.3 ADDED LOGIC
....Set Row Widths......
          if (PIndex(1) <> C0 & PIndex(2) <> C0)
                    move      PIndex(1),str9
                    call      Trim using str9
                    move      PIndex(2),str10
                    call      Trim using str10
.START PATCH 1.3 REPLACED LOGIC
.                   pack      str25,"A",str9,":I",str10
                    pack      str25,"A",str9,":J",str10
.END PATCH 1.3 REPLACED LOGIC
                    sheet1.range(str25).Rows.Autofit
          endif
.
          if (LIndex(1) <> C0 & LIndex(2) <> C0)
                    move      LIndex(1),str9
                    call      Trim using str9
                    move      LIndex(2),str10
                    call      Trim using str10
.START PATCH 1.3 REPLACED LOGIC
.                   pack      str25,"A",str9,":I",str10
                    pack      str25,"A",str9,":J",str10
.END PATCH 1.3 REPLACED LOGIC
                    sheet1.range(str25).Rows.Autofit
          endif
.Format Whole Document
.START PATCH 1.3 REPLACED LOGIC
.         pack      str25,"A1:I",str10
          pack      str25,"A1:J",str10
.END PATCH 1.3 REPLACED LOGIC
          sheet1.range(str25).BorderAround using *LineStyle=1,*Weight=4,*ColorIndex=5               .'5' is index for Blue
          setprop   sheet1,*DisplayPageBreaks="True"
.
FileNameSelect
          clear   taskname
          move      "c:\work\",taskname ."
          setprop ex,*DefaultFilePath=taskname
          pack    taskname,taskname,"Cancelled Denied Report"
          setmode *mcursor=*arrow
          ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
          if (taskname <> "0")
                    movelptr taskname,N9
                    reset   taskname,N9
                    append  "xls",taskname
                    reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                    trap    TrapObject if Object
                    book.saveas giving N9 using *Filename=taskname
                    trapclr Object
          endif
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.         book.printout
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy sortcol
          destroy sheet1
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
          shutdown "cls"

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
                    pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                    alert   caution,taskname,result
.                   goto CampaignFileNameSelect
          endif
.Send them back to select another File name and try to Save again.
          goto FileNameSelect
.         goto CleanUp

OrderReadRecords
          PackData.GetCount giving result
          if (result > C0)
                    for N9,"1",result
                              getitem   PackData,N9,str6
                              pack      NORDFLD3,"03X",str6
                              call      OrderReadRecordsB
                    repeat
          else
                    goto OrderReadRecordsB
          endif
          return

OrderReadRecordsB
          move      "NORD0045-Read NINLCR",Location
          pack      KeyLocation,"Key: ",NORDFLD1,NORDFLD2
        trap        IOMssg Giving Error if IO
          read      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4;ORDVARS
          trapclr   IO
          loop
                    until over
                    clear     str55
                    if (OSTAT = "x" | OSTAT = "z")
                              if (OSTAT = "x")
                                        pack      str55,"Cancelled Pending Order"
                              elseif (OSTAT = "z")
                                        pack      str55,"Cancelled LCR"
                              endif
                              call      OrderWriteRecords
                    else
                              if (OSTAT = "p")
                                        packkey   NORD4FLD,OLRN
                                        clear     NORD4STAT
                                        move      "NORD4KEY",Location
                                        pack      KeyLocation,"Key: ",NORD4FLD
                                        call      NORD4KEY
                                        if not over
                                                  if (NORD4STAT = "06")
                                                            pack      str55,"Denied Pending Order"
                                                            call      OrderWriteRecords
                                                  elseif (NORD4STAT = "07")
                                                            pack      str55,"Cancelled Pending Order"
                                                            call      OrderWriteRecords
                                                  endif
                                        endif
                              elseif (OSTAT = "l")
                                        packkey   NORD5FLD,OLRN
                                        clear     NORD5STAT
                                        move      "NORD5KEY",Location
                                        pack      KeyLocation,"Key: ",NORD5FLD
                                        call      NORD5KEY
                                        if not over
                                                  if (NORD5STAT = "07")
                                                            pack      str55,"Denied LCR"
                                                            call      OrderWriteRecords
                                                  elseif (NORD5STAT = "05")
                                                            pack      str55,"Cancelled LCR"
                                                            call      OrderWriteRecords
                                                  endif
                                        endif
.START PATCH 1.1 ADDED LOGIC
                              elseif (OSTAT = "X")
                                        pack      str55,"Cancelled Order"
                                        call      OrderWriteRecords
                              elseif (OSTAT = "Q")
                                        pack      str55,"Cancelled Billed Order"
                                        call      OrderWriteRecords
.END PATCH 1.1 ADDED LOGIC
                              endif
                    endif
                    trap      IOMssg Giving Error if IO
                    move      "NORD0045-ReadKG NINLCR",Location
                    pack      KeyLocation,"Key: ",NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4
                    readkg    NORDFLE2;ORDVARS
                    trapclr   IO
          repeat
          return

OrderWriteRecords
          if (DeptType = "3" & OSALES10 = "0" & OSALES = "6")
                    return
          endif
          move      C0,N8
.START PATCH 1.1 REPLACED LOGIC
.         if (DateType = "1") .Use Denied/Approval Date as Default
          if (DateType = "1" & RecType <> "4")    .Use Denied/Approval Date as Default
.END PATCH 1.1 REPLACED LOGIC
                    pack      str8,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED
                    call      Trim using str8
                    if (str8 = "")
                              pack      str8,OODTEC,OODTEY,OODTEM,OODTED
                    else
                              type      str8
                              if not equal
                                        pack      str8,OODTEC,OODTEY,OODTEM,OODTED
                              endif
                    endif
          else
                    pack      str8,OODTEC,OODTEY,OODTEM,OODTED
          endif
          move      str8,N8
.         if (N8 = C0 OR (N8 >= FromDateB AND N8 <= ToDateB))
          if (N8 >= FromDateB AND N8 <= ToDateB)
                    PackData2.GetCount giving result
                    if (result > C0)
                              for N9,"1",result
                                        getitem   PackData2,N9,str4
                                        if (str4 = OMLRNUM)
                                                  write     tempfile,SEQ;ORDVARS,str55
                                                  add       C1,howmany
                                                  break
                                        endif
                              repeat
                    else
                              write     tempfile,SEQ;ORDVARS,str55
                              add       C1,howmany
                    endif
          endif
          return

OrderLoadSubHeader LRoutine FrmPtr
.FrmPtr = Record Type:  "0" = Pending Orders, "1" = LCRs
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"A",str9
.START PATCH 1.1 REPLACED LOGIC
.         if (FrmPtr = C0)
.                   setprop sheet1.range(str10),*Value="Pending Order Detail"
.         else
.                   setprop sheet1.range(str10),*Value="List Clearance Request Detail"
.         endif
          if (RecType = "4")
                    if (FrmPtr = C0)
                              setprop sheet1.range(str10),*Value="Cancelled Order Detail"
                    else
                              setprop sheet1.range(str10),*Value="Cancelled/Billed Order Detail"
                    endif
          else
                    if (FrmPtr = C0)
                              setprop sheet1.range(str10),*Value="Pending Order Detail"
                    else
                              setprop sheet1.range(str10),*Value="List Clearance Request Detail"
                    endif
          endif
.END PATCH 1.1 REPLACED LOGIC
          setprop sheet1.range(str10).Font,*Size=12
.
          add       C1,CurLine
          move      CurLine,str9
          call      Trim using str9
          pack      str10,"A",str9
          setprop sheet1.range(str10),*Value="LR##"
          pack      str10,"B",str9
          setprop sheet1.range(str10),*Value="Status"
          pack      str10,"C",str9
          setprop sheet1.range(str10),*Value="List Name"
          pack      str10,"D",str9
          setprop sheet1.range(str10),*Value="Mailer Name"
          pack      str10,"E",str9
          setprop sheet1.range(str10),*Value="Broker"
          pack      str10,"F",str9
          setprop sheet1.range(str10),*Value="NIN Caller"
          pack      str10,"G",str9
          setprop sheet1.range(str10),*Value="Quantity"
          pack      str10,"H",str9
          setprop sheet1.range(str10),*Value="Special Instructions"
          pack      str10,"I",str9
          setprop sheet1.range(str10),*Value="Internal Notes"
.START PATCH 1.3 ADDED LOGIC
          pack      str10,"J",str9
          setprop sheet1.range(str10),*Value="Mailer Notes"
.END PATCH 1.3 ADDED LOGIC
          pack      str11,"A",str9
.Establish Borders
.START PATCH 1.3 REMOVED LOGIC
.         setprop   sheet1.range(str11,str10).Borders(8),*LineStyle=1                     .Top Line, Single
.         setprop   sheet1.range(str11,str10).Borders(9),*LineStyle=1                     .Bottom Line, Single
.END PATCH 1.3 REMOVED LOGIC
.Establish Row Height
          getprop   sheet1.rows(CurLine),*RowHeight=N32
          mult      C2,N32
          setprop   sheet1.rows(CurLine),*RowHeight=N32
          add       C1,CurLine
          return

IOFileError
          alert     caution,"Input File does not exist!  Please try again.",result
          shutdown "cls"

        include nordio.inc
        include nord4io.inc
        include nord5io.inc
        include nspeio.inc
        include nspe2io.inc
        include npndio.inc
.patch1.2
                                        include   compio.inc
                                        include   cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
.patch1.2
        include ndatio.inc
        include ncntio.inc
        include comlogic.inc
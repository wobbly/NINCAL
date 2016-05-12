        include common.inc
        include cons.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
        include ndatdd.inc
.START PATCH 1.2 ADDED LOGIC
          include   winapi.inc
.END PATCH 1.2 ADDED LOGIC

Release    INIT    "1.41"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release init    "1.4"         DLH       Change Logo logic to match datacard print
.reldate   Init      "13 Aug 2008"
.release init    "1.3"        ASH       18JUN2007 PLI Inclusion
.release init    "1.2"        ASH       22SEP2004 Logo Conversion Patch
.release init    "1.1"        ASH       09AUG2004 Logo Conversion
.release init    "1.0"        ASH       Initial Release
.
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
sheet   automation
sortcol automation
sortcol1 automation
ex      automation      class="Excel.Application"
xlMinimized integer 4,"0xFFFFEFD4"
SheetsDefault   integer 4,"0x00000000"
AlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
.START PATCH 1.1 ADDED LOGIC
xlRowHeight         variant
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_R8     EQU 5           .Double - 8 byte Real
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.4 
FrmPtr3   form      ^
.END PATCH 1.4

.START PATCH 1.2 ADDED LOGIC
.hexeight integer 4,"4294967295"
.END PATCH 1.2 ADDED LOGIC

.START PATCH 1.3 ADDED LOGIC
SReturn        init 0x0a                     ;soft return/line feed
.END PATCH 1.3 REPLACED LOGIC

.begin patch 1.4
.PrintUsageToExcel Routine
PrintUsageToExcel Routine FrmPtr3
          call      debug
          Move      FrmPtr3,Company
.FrmPtr3 = Company
.end patch 1.4

.START PATCH 1.2 ADDED LOGIC
          pack      APIFileName,"c:\work\datusage.dat",hexzero
          call      FindFirstFile
          if (APIResult = 0 | APIResult = hexeight)
                    return
          endif
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.1 ADDED LOGIC
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.END PATCH 1.1 ADDED LOGIC
.Initialize variables
          open      tempfile,"c:\work\datusage.dat"
.Open Excel application
        create  ex
.begin patch 1.41        
.        setprop ex,*WindowState=xlMinimized
.end patch 1.41
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
.START PATCH 1.1 REPLACED LOGIC
.        setprop sheet.range("A1"),*Value="Names in the News",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A1").Font,*Name="Times New Roman",*Underline=xlUnderlineStyleSingle,*Size=20
.        setprop sheet.range("A1").Characters(1,5).Font,*Bold="True"
.        setprop sheet.range("A1").Characters(7,11).Font,*Italic="True"
.        sheet.range("A1:C1").Merge
.        setprop sheet.range("A2"),*Value="C  A  L  I  F  O  R  N  I  A       I  N  C .",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A2").Font,*Name="Times New Roman",*Size=10
.        sheet.range("A2:C2").Merge
........................................
.START PATCH 1.3 MOVED LOGIC BELOW
.         setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.         sheet.range("A1:E1").Merge
..START PATCH 1.2 REPLACED LOGIC
..        sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.         sheet.Shapes.AddPicture using "\\nins1\e\netutils\NIN Logo black outline.jpg",OTRUE,OTRUE,0,0,240,75
.END PATCH 1.3 MOVE LOGIC BELOW
.END PATCH 1.2 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
.
          move      C1,NDATPATH
          move      C0,result .Must be Initialized for successive runs
          loop
                    read      tempfile,SEQ;NDATFLD,COMPFLD
                    until over
                    if (result = C0)
                              move      "NDATKEY",Location
                              pack      KeyLocation,"Key: ",NDATFLD
                              call      NDATKEY
.START PATCH 1.3 MOVED LOGIC FROM ABOVE
.begin patch 1.4 change to match datacard logic
.                             if (ELSTCDE = "P")
                              if        (Company = c2)
.end patch 1.4                          
                                        clear taskname
                                        setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
                                        setprop sheet.range("A1"),*HorizontalAlignment=AlignCenter
                                        setprop sheet.range("A1").Font,*Name="Times New Roman",*Size=10,*Bold="True"
                                        append    "Pacific Lists, Inc.",taskname
                                        append    Sreturn,taskname
                                        append    "180 Grand Avenue, Suite 1365",taskname
                                        append    Sreturn,taskname
                                        append    "Oakland, CA 94612-3716",taskname
                                        append    Sreturn,taskname
                                        append    "415-945-9450 · Fax 415-945-9451",taskname
                                        append    Sreturn,taskname
                                        append    "A Division of Names in the News",taskname
                                        reset     taskname
                                        setprop   sheet.range("A1"),*Value=taskname
                                        setprop sheet.range("A1").Characters(1,19).Font,*Size=18
                                        sheet.range("A1:C1").Merge
                              else
                                        setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
                                        sheet.range("A1:E1").Merge
                                        sheet.Shapes.AddPicture using "\\nins1\e\netutils\NIN Logo black outline.jpg",OTRUE,OTRUE,0,0,240,75
                              endif
.END PATCH 1.3 MOVE LOGIC FROM ABOVE
                              call      Trim using MLSTNAME
                              pack      taskname,"List Usage Report for ",MLSTNAME
                            setprop sheet.range("B4"),*Value=taskname
                            setprop sheet.range("B4").Characters(23,75).Font,*Bold="True"
                            setprop sheet.range("A6"),*Value="Mailer Number"
                            setprop sheet.range("B6"),*Value="Name"
                              sheet.range("A6:B6").BorderAround using *LineStyle=1,*Weight=2
                              move      "7",result
                    endif
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    call      Trim using COMPCOMP
                    move      result,str9
                    call      Trim using str9
                    pack      str10,"A",str9
                  setprop sheet.range(str10),*Value=COMPOLDMLR
                    pack      str10,"B",str9
                  setprop sheet.range(str10),*Value=COMPCOMP
                    add       C1,result
          repeat
          close     tempfile
          erase     "c:\work\datusage.dat"
        pack    str4,"A6"
        pack    str10,"A",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"B4"
        pack    str10,"B",str9
        sheet.range(str4,str10).Columns.Autofit
FileNameSelect
          setprop ex,*Visible="True"
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        destroy ex
        return

          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
        include ndatio.inc
        include comlogic.inc

.Program which goes through entire Company file and dumps into spreadsheet, breaking
.Company records down into the different categories.  Useful for ascertaining dupes
.for Conversions, as well as general maintenance.
        include common.inc
        include cons.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc

release init    "1.0"         ASH 08DEC2004 New Release

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
.......................
sheets  automation
sheet   automation
sheet2    automation
sortcol automation
sortcol1 automation
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
Zoom85  variant
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
TopMargin variant
BottomMargin        variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
SheetsDefault   integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle integer 4,"0x2"
.Create the Variant objects
.Booleans
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        create  Zoom85,VarType=VT_I4,VarValue=85
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50

.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C2
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
          setprop   sheet,*Name="Company"
        sheets.item giving sheet2 using 2
          setprop   sheet2,*Name="Contacts"
          sheet2.activate
.Reset focus on the first Worksheet
          sheet.activate
.....Header.....
          setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet.range("A1:E1").Merge
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
        setprop sheet.range("A4"),*Value="Company Name"
        setprop sheet.range("B4"),*Value="Number"
        setprop sheet.range("C4"),*Value="Mailer?"
        setprop sheet.range("D4"),*Value="Broker?"
        setprop sheet.range("E4"),*Value="List Owner?"
        setprop sheet.range("F4"),*Value="Service Bureau?"
        setprop sheet.range("G4"),*Value="Consultant?"
        setprop sheet.range("H4"),*Value="List Manager?"
        setprop sheet.range("I4"),*Value="Old Mailer"
        setprop sheet.range("J4"),*Value="Old Broker"
        setprop sheet.range("K4"),*Value="Old Owner"
        setprop sheet.range("L4"),*Value="Old Service Bureau"
.
        pack    str11,"1:4"
        setprop sheet.PageSetup,*PrintTitleRows=str11
.
          move      C5,howmany
          loop
                    call      COMPSEQ
                    until over
                    add       C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    pack      str11,"A",str9
                    call      Trim using COMPCOMP
                  setprop sheet.range(str11),*Value=COMPCOMP
                    pack      str11,"B",str9
                  setprop sheet.range(str11),*Value=COMPNUM
                    pack      str11,"C",str9
                    if (COMPMLRFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"D",str9
                    if (COMPBRKFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"E",str9
                    if (COMPOWNFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"F",str9
                    if (COMPSVBFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"G",str9
                    if (COMPCLRFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"H",str9
                    if (COMPMNGFLG = "T")
                              move      YES,str3
                    else
                              clear     str3
                    endif
                  setprop sheet.range(str11),*Value=str3
                    pack      str11,"I",str9
                  setprop sheet.range(str11),*Value=COMPOLDMLR
                    pack      str11,"J",str9
                  setprop sheet.range(str11),*Value=COMPOLDBRK
                    pack      str11,"K",str9
                  setprop sheet.range(str11),*Value=COMPOLDOWN
                    pack      str11,"L",str9
                  setprop sheet.range(str11),*Value=COMPOLDSVB
          repeat
.
.....Format document.....
.Set up Header Formatting
          setprop sheet.range("A4:L4").Font,*Bold="True"
          sheet.range("A4:L4").BorderAround using *LineStyle=1,*Weight=2
.Sort by Company Name
          pack    str11,"L",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
          getprop sheet.range("A6"),*Columns(1)=sortcol
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
          pack    str10,"A6"
          sheet.range(str10,str11).sort using *Key1=sortcol,*Order1=1
.Set Column Widths for larger fields
          move      howmany,str9
        call    Trim using str9
        pack    str10,"A4"
        pack    str11,"A",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"B4"
        pack    str11,"B",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"C4"
        pack    str11,"C",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"D4"
        pack    str11,"A",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"E4"
        pack    str11,"E",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"F4"
        pack    str11,"F",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"G4"
        pack    str11,"G",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"H4"
        pack    str11,"H",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"I4"
        pack    str11,"I",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"J4"
        pack    str11,"J",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"K4"
        pack    str11,"K",str9
        sheet.range(str10,str11).Columns.Autofit
        pack    str10,"L4"
        pack    str11,"L",str9
        sheet.range(str10,str11).Columns.Autofit
.........Second Sheet
.....Header.....
          setprop   sheet2.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet2.range("A1:E1").Merge
          sheet2.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
        setprop sheet2.range("A4"),*Value="Contact Name"
        setprop sheet2.range("B4"),*Value="Number"
        setprop sheet2.range("C4"),*Value="Company"
        setprop sheet2.range("D4"),*Value="Type"
        setprop sheet2.range("E4"),*Value="Old Broker"
        setprop sheet2.range("F4"),*Value="Last Name"
.
        pack    str11,"1:4"
        setprop sheet2.PageSetup,*PrintTitleRows=str11
.
          move      C5,howmany
          move      C0,COMPFLAG
          loop
                    call      CNCTSEQ
                    until over
                    pack      COMPFLD,CNCTCODE
                    call      COMPKEY
                    add       C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    pack      str11,"A",str9
                    call      Trim using CNCTFNAME
                  setprop sheet2.range(str11),*Value=CNCTFNAME
                    pack      str11,"B",str9
                  setprop sheet2.range(str11),*Value=CNCTID
                    pack      str11,"C",str9
                    call      Trim using COMPCOMP
                  setprop sheet2.range(str11),*Value=COMPCOMP
                    pack      str11,"D",str9
                    if (CNCTTYPE = "1")
                              move      "Mailer",str25
                    elseif (CNCTTYPE = "2")
                              move      "Broker",str25
                    elseif (CNCTTYPE = "3")
                              move      "List Owner",str25
                    elseif (CNCTTYPE = "4")
                              move      "Service Bureau",str25
                    elseif (CNCTTYPE = "5")
                              move      "Consultant",str25
                    elseif (CNCTTYPE = "6")
                              move      "Manager",str25
                    else
                              clear     str25
                    endif
                  setprop sheet2.range(str11),*Value=str25
                    pack      str11,"E",str9
                    call      Trim using CNCTCNT
                    if (CNCTCNT <> "")
                              unpack    CNCTCNT,str4,str3
                              pack      str8,str4,SLASH,str3
                    else
                              clear     str8
                    endif
                  setprop sheet2.range(str11),*Value=str8
                    pack      str11,"F",str9
                  setprop sheet2.range(str11),*Value=CNCTLNAME
          repeat
.
.....Format document.....
.Set up Header Formatting
          setprop sheet2.range("A4:L4").Font,*Bold="True"
          sheet2.range("A4:L4").BorderAround using *LineStyle=1,*Weight=2
.Sort by Company Name
          pack    str11,"L",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
          getprop sheet2.range("F6"),*Columns(1)=sortcol
          getprop sheet2.range("A6"),*Columns(1)=sortcol1
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
          pack    str10,"A6"
          sheet2.range(str10,str11).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.Set Column Widths for larger fields
          move      howmany,str9
        call    Trim using str9
        pack    str10,"A4"
        pack    str11,"A",str9
        sheet2.range(str10,str11).Columns.Autofit
        pack    str10,"B4"
        pack    str11,"B",str9
        sheet2.range(str10,str11).Columns.Autofit
        pack    str10,"C4"
        pack    str11,"C",str9
        sheet2.range(str10,str11).Columns.Autofit
        pack    str10,"D4"
        pack    str11,"A",str9
        sheet2.range(str10,str11).Columns.Autofit
        pack    str10,"E4"
        pack    str11,"E",str9
        sheet2.range(str10,str11).Columns.Autofit
        pack    str10,"F4"
        pack    str11,"F",str9
        sheet2.range(str10,str11).Columns.Autofit
.Skipping FileName selection!!
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy Zoom85
        destroy sortcol
        destroy sortcol1
        destroy sheet
        destroy sheet2
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*Visible="True"
        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        destroy ex
        shutdown


          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
        include comlogic.inc

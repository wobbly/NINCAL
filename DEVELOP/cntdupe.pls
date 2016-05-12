.USED TO TEST FOR DUPES IN CONTACT FILE!!
        include common.inc
        include cons.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
	include	winapi.inc

release init    "1.0"	ASH	Initial Release
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
xlRowHeight	variant
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_R8	EQU 5           .Double - 8 byte Real
.hexeight integer 4,"4294967295"

	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Initialize variables
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
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
        setprop sheet.range("A1"),*Value="Company Number"
        setprop sheet.range("B1"),*Value="Contact Number"
        setprop sheet.range("C1"),*Value="Full Name"
        setprop sheet.range("D1"),*Value="Last Name"
        setprop sheet.range("E1"),*Value="Mailer"
        setprop sheet.range("F1"),*Value="Broker"
        setprop sheet.range("G1"),*Value="Consultant"
.
	move	C1,result
	loop
		CALL	CNCTSEQ
		until over
		add	C1,result
		move	result,str9
		call	Trim using str9
		pack	str10,"A",str9
	        setprop sheet.range(str10),*Value=CNCTCODE
		pack	str10,"B",str9
	        setprop sheet.range(str10),*Value=CNCTID
		pack	str10,"C",str9
		call	Trim using CNCTFNAME
	        setprop sheet.range(str10),*Value=CNCTFNAME
		pack	str10,"D",str9
		call	Trim using CNCTLNAME
	        setprop sheet.range(str10),*Value=CNCTLNAME
		if (CNCTTYPE = "1" | CNCTTYPE = "2" | CNCTTYPE = "5")
			if (CNCTTYPE = "1")	.Mailer
				pack	str10,"E",str9
			elseif (CNCTTYPE = "2")	.Broker
				pack	str10,"F",str9
			elseif (CNCTTYPE = "5")	.Consultant
				pack	str10,"G",str9
			endif
		        setprop sheet.range(str10),*Value="T"
		endif
	repeat
FileNameSelect
	setprop ex,*Visible="True"
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        destroy ex
        shutdown

	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
        include comlogic.inc

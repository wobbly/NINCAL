        include common.inc
        include cons.inc
        include ndatdd.inc
	include	nseldd.inc

release init    "1.0"	ASH 31AUG2004 NEW RELEASE

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
sheet	automation
ex      automation      class="Excel.Application"
.
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
xlMinimized integer 4,"0xFFFFEFD4"
xlNormal integer 4,"0xFFFFEFD1"
xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"
        
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
.....Header.....
	setprop sheet.range("A1"),*Value="List"
	setprop sheet.range("B1"),*Value="Name"
	setprop sheet.range("C1"),*Value="Select Number"
	setprop sheet.range("D1"),*Value="Name"
	setprop sheet.range("E1"),*Value="Type"
	setprop sheet.range("A1:E1").Font,*Bold="True",*Underline=xlUnderlineStyleSingle
..........Line Skip..........
	move	C1,NSELPATH
	move	C2,howmany
	open	tempfile,"\\nts1\e\data\data100.dat"
	loop
		read	tempfile,SEQ;DATVARS
		until over
		move	howmany,str9
		call	Trim using str9
		pack	str11,"A",str9
		setprop sheet.range(str11),*Value=LSTNUM
		pack	str11,"B",str9
		setprop sheet.range(str11),*Value=MLSTNAME
		pack	NSELFLD1,"01X",LSTNUM
		move	"NSELAIM",Location
		pack	KeyLocation,"Key: ",NSELFLD1
		call	NSELAIM
		loop
			until over
			add	C1,howmany
			move	howmany,str9
			call	Trim using str9
		pack	str11,"A",str9
 		setprop sheet.range(str11),*Value=LSTNUM
		pack	str11,"B",str9
		setprop sheet.range(str11),*Value=MLSTNAME
			pack	str11,"C",str9
			setprop sheet.range(str11),*Value=NSELNUM
			pack	str11,"D",str9
			setprop sheet.range(str11),*Value=NSELSNAME
			if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
				clear	NSELBASE
			endif
			pack	str11,"E",str9
			setprop sheet.range(str11),*Value=NSELBASE
			move	"NSELKG",Location
			call	NSELKG
		repeat
		add	C1,howmany
	repeat
	move	howmany,str9
	call	Trim using str9
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
        pack    str10,"E1"
        pack    str11,"E",str9
        sheet.range(str10,str11).Columns.Autofit
	setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
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
        destroy ex
        shutdown


        include ndatio.inc
	INCLUDE	NSELIO.INC
        include comlogic.inc

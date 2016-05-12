.Clean up for Mailer XREF for LM Clients.
        include common.inc
        include cons.inc
        include norddd.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
	include	ndatdd.inc
	include	nowndd.inc
	INCLUDE	nmdldd.inc
	include	nusedd.inc
	include	nxrfdd.inc

release init    "1.0"	ASH 15MAR2006 NEW RELEASE

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
sheet  automation
ex      automation      class="Excel.Application"
.
books2   automation
book2    automation
sheets2  automation
sheet2  automation
ex2      automation      class="Excel.Application".
.
str9a	dim	9
str11a	dim	11
STR75	DIM	75
result2	form	9
.
colornum form	24
yellow	color
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
VT_I8   EQU 5           .8 byte real
.VT_VARIANT EQU 12
Zoom100 variant
Zoom75  variant
Zoom85  variant
Zoom60  variant
Zoom87  variant
Zoom70  variant
Zoom97  variant
Zoom32  variant
Zoom125  variant
xlRowHeight	variant
VT_R8	EQU 5           .Double - 8 byte Real
.AcctType variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
AutoCalc integer 4,"0xffffeff7"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlNormal integer 4,"0xFFFFEFD1"
xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"
onehun	integer	4,"0x100"
DblLine integer 4,"0xffffefe9"
MedThick integer 4,"0xFFFFEFD6"
BorderHor integer 4,"0xc"
BorderVert integer 4,"0xb"
SReturn        init	0x0a                     ;soft return/line feed

	prep	tempfile,"c:\andrew\strategic\except4.dat"
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.open using "c:\andrew\strategic\AprilBoardMeetingDraft6.xls"
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 4
.
	move	C7,result
	loop
		move	result,str9
		call	Trim using str9
.List Number
		pack	str11,"A",str9
		getprop sheet.range(str11),*Value=NDATFLD
		call	Trim using NDATFLD
		until (NDATFLD = "")
		call	ZFillIt using NDATFLD
		call	NDATKEY
.
		pack	NXRFFLD,LSTNUM
		call	NXRFKEY
.
		pack	str11,"F",str9
		getprop sheet.range(str11),*Value=str6
		if (str6 <> NXRFMLR)
			write	tempfile,SEQ;LSTNUM,B1,NXRFMLR
		endif
		add	C1,result
	repeat
CampaignCleanUp
	destroy sheet
	destroy book
	destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
	setprop ex,*DisplayAlerts=OFALSE
	EX.QUIT
	destroy ex
	shutdown

        include nordio.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
	include	ndatio.inc
	include	nownio.inc
	INCLUDE	nmdlio.inc
	include	nuseio.inc
	include	nxrfio.inc
        include comlogic.inc

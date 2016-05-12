        include common.inc
        include cons.inc
        include nmlrxydd.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc

release init    "1.0"	ASH	16NOV2004	Initial Release
LstPtr ListView ^
ListData ListView
hold	dim	1068
str12a	dim	12
str10a	dim	10
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
xlRowHeight	variant
VT_R8	EQU 5           .Double - 8 byte Real
TopMargin	variant
BottomMargin	variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignTop integer 4,"0xFFFFEFC0"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle integer 4,"0x2"
.Create the Variant objects
.Booleans
.Others
        
CreateMailerMailer Routine LstPtr
	LstPtr.GetItemCount giving howmany
	if (howmany = 0)
		return
	endif
.Initialize variables
.Create work var
	create	ListData=1:1:1:1
	ListData.InsertColumn using "Key",0,0
	ListData.InsertColumn using "KeyReversed",0,1
	ListData.InsertColumn using "Record",0,2
.
        create  Zoom85,VarType=VT_I4,VarValue=85
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	xlRowHeight,VarType=VT_R8,VarValue="75.0"
."1" increment in Excel interface equals "1.3888" in OLE logic
	create	TopMargin,VarType=VT_R8,VarValue="18"		Roughly equals .25 inches:  18 * 1.388 = 25
	create	BottomMargin,VarType=VT_R8,VarValue="36"	Roughly equals .50 inches:  36 * 1.388 = 50
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
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
        setprop sheet.PageSetup,*Zoom=Zoom85
        setprop sheet.PageSetup,*TopMargin=TopMargin
        setprop sheet.PageSetup,*BottomMargin=BottomMargin
        setprop sheet.PageSetup,*FooterMargin=TopMargin
.....Header.....
	setprop	sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
	sheet.range("A1:E1").Merge
	sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
        setprop sheet.range("A3"),*Value="Mailer 1"
        setprop sheet.range("B3"),*Value="Mailer 1 Name"
        setprop sheet.range("C3"),*Value="Mailer 2"
        setprop sheet.range("D3"),*Value="Mailer 2 Name"
        setprop sheet.range("E3"),*Value="Date"
        setprop sheet.range("F3"),*Value="Special Arrangements"
        setprop sheet.range("G3"),*Value="Comments"
        pack    str11,"1:3"
        setprop sheet.PageSetup,*PrintTitleRows=str11
.....Records.....
	LstPtr.GetItemCount giving howmany
	sub	C1,howmany
	for result,C0,howmany
		LstPtr.GetItemText giving str6 using result
		pack	NMLRXYFLD1,"01F",str6
		move	"NMLRXYAIM",Location
		pack	KeyLocation,"Key: ",NMLRXYFLD1
		call	NMLRXYAIM
		loop
			until over
			if (str6 = NMLRXYMLR1 | str6 = NMLRXYMLR2)
				pack	str12,NMLRXYMLR1,NMLRXYMLR2
				pack	str12a,NMLRXYMLR2,NMLRXYMLR1
				ListData.FindItem giving N9 using *Text=str12
				if (N9 = SEQ)
					ListData.FindItem giving N9 using *Text=str12A
					if (N9 = SEQ)
					        ListData.InsertItem giving N9 using str12
					        ListData.SetItemText using N9,str12a,1
						pack	hold,NMLRXYVARS
					        ListData.SetItemText using N9,hold,2
					endif
				endif
			endif
			move	"NMLRXYKG",Location
			pack	KeyLocation,"Key: ",NMLRXYFLD1
			call	NMLRXYKG
		repeat
	repeat
	ListData.GetItemCount giving howmany
        if (howmany > 0)
		move	C4,N9
		sub	C1,howmany
		for result,C0,howmany
			add	C1,N9
			move	N9,str9
			call	Trim using str9
.
			ListData.GetItemText giving hold using result,2
			unpack	hold,NMLRXYVARS
			pack	str10a,"A",str9
			setprop sheet.range(str10a),*Value=NMLRXYMLR1
			move	NMLRXYMLR1,COMPFLD
			move	"1-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			call	Trim using COMPCOMP
			pack	str10,"B",str9
			setprop sheet.range(str10),*Value=COMPCOMP
			move	NMLRXYMLR2,COMPFLD
			move	"2-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			call	Trim using COMPCOMP
			pack	str10,"C",str9
			setprop sheet.range(str10),*Value=NMLRXYMLR2
			pack	str10,"D",str9
			setprop sheet.range(str10),*Value=COMPCOMP
			call	Trim using NMLRXYUDATE
			if (NMLRXYUDATE <> "")
				unpack	NMLRXYUDATE,str4,MM,DD
				pack	str11,MM,SLASH,DD,SLASH,str4
			else
				clear	str11
			endif
			pack	str10,"E",str9
			setprop sheet.range(str10),*Value=str11
			call	Trim using NMLRXYNOTE
			pack	str10,"F",str9
			setprop sheet.range(str10),*Value=NMLRXYNOTE
			call	Trim using NMLRXYNOTE2
			pack	str10,"G",str9
			setprop sheet.range(str10),*Value=NMLRXYNOTE2
			setprop sheet.range(str10a,str10),*VerticalAlignment=AlignTop
		repeat
	endif
.....Format document.....
.Set up Header Formatting
        setprop sheet.range("A3:G3").Font,*Bold="True",*Size=12
        sheet.range("A3:G3").BorderAround using *LineStyle=1,*Weight=2
.Sort by Mailer 1 Name
        pack    str11,"G",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.Mailer 1 Name
        getprop sheet.range("B5"),*Columns(1)=sortcol
.Key1 set to Mailer 1 Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    str10,"A5"
        sheet.range(str10,str11).sort using *Key1=sortcol,*Order1=1
        pack    str4,"A1"
        pack    str10,"A",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"B1"
        pack    str10,"B",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"C1"
        pack    str10,"C",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"D1"
        pack    str10,"D",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"E1"
        pack    str10,"E",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"F1"
        pack    str10,"F",str9
        sheet.range(str4,str10).Columns.Autofit
        pack    str4,"G1"
        pack    str10,"G",str9
        sheet.range(str4,str10).Columns.Autofit
.
	pack	str4,"A4"
        sheet.range(str4,str10).Rows.Autofit
MailerFileNameSelect
	setprop ex,*Visible="True"
        clear   taskname
        append  "\\nins1\d\USERS",taskname
        append  "\",taskname
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
............................................
        setmode *mcursor=*arrow
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                trap    TrapCampaignObject if Object
.                sheet.saveas giving N9 using *Filename=taskname
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
        endif
.        book.printout
MailerCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
	destroy Zoom85
        destroy sortcol
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
.        ex.quit
	destroy	ListData
        destroy ex
        return

TrapCampaignObject
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
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto MailerFileNameSelect
.        goto MailerCleanUp

errortrap
.testing purposes
        getinfo exception,taskname
        return
        
        include nmlrxyio.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
        include comlogic.inc

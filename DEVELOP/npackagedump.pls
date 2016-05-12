.HISTORY PIVOT TABLE
        include common.inc
        include cons.inc
	include npkgdd.inc
release init    "1.0"	ASH 25FEB2002 NEW RELEASE
.EXTERNAL ROUTINES FROM NORDTEST.PLC
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
.Objects needed for Excel Spreadsheet creation
books   automation
book    automation
sheets  automation
sheet1  automation
ex      automation      class="Excel.Application"
.
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
AlignLeft integer 4,"0xffffefdd"
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant

        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
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
        sheets.item giving sheet1 using 1
	setprop	sheet1,*Name="TNC Packages"
.Set Viewing Sizes
.As the first sheet is always inherently active I do not need to activate it
.    sheet1.activate
.As the Zoom property is automatically set to 100% I do not need to set it
.    setprop book.windows(1),*Zoom=Zoom100
.Reset focus on the first Worksheet
	sheet1.activate
.
.Set Printing Sizes
.        setprop sheet1.PageSetup,*Zoom=Zoom87
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
.        sheet2.range("A1","IV1000").Clear
.Greatest column possible is "IV"
.	setprop	sheet1.Range("A1","IV1000").Font,*Size=10
.....History Raw Data Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
..................................
.Raw Data Header Information
        setprop sheet1.range("A3"),*Value="TNC ID"
        setprop sheet1.range("A3").Font,*Bold="True"
        setprop sheet1.range("B3"),*Value="Package Name"
        setprop sheet1.range("B3").Font,*Bold="True"
        setprop sheet1.range("C3"),*Value="NINCA ID"
        setprop sheet1.range("C3").Font,*Bold="True"
	move	C3,howmany
	move	C1,NPKGPATH
	pack	NPKGFLD1,"01X0173"
	clear	NPKGFLD2
	clear	NPKGFLD3
	clear	NPKGFLD4
	move	"NPKGAIM",Location
	pack	KeyLocation,"Key: ",NPKGFLD1
	call	NPKGAIM
	loop
		until over
		add	C1,howmany
		move	howmany,str9
		call	Trim using str9
		call	Trim using NPKGID
		pack	str11,"A",str9
                setprop sheet1.range(str11),*Value=NPKGID,*HorizontalAlignment=AlignLeft
		call	Trim using NPKGPNAME
                pack    str11,"B",str9
                setprop sheet1.range(str11),*Value=NPKGPNAME
		call	Trim using NPKGNUM
                pack    str11,"C",str9
                setprop sheet1.range(str11),*Value=NPKGNUM
		move	"NPKGKG",Location
		call	NPKGKG
	repeat
        pack    str4,"A3"
        pack    str5,"A",str9
        sheet1.range(str4,str5).Columns.Autofit
        pack    str4,"B3"
        pack    str5,"B",str9
        sheet1.range(str4,str5).Columns.Autofit
        pack    str4,"C3"
        pack    str5,"C",str9
        sheet1.range(str4,str5).Columns.Autofit
.
CampaignFileNameSelect
        clear   taskname
	pack	taskname,"C:\WORK\"
        setprop ex,*DefaultFilePath=taskname
        pack    taskname,taskname,"TNCPACKAGES"
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
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
        endif
.        book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
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
        shutdown

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
        goto CampaignFileNameSelect
.        goto CampaignCleanUp

errortrap
.testing purposes
        getinfo exception,taskname
        return
        
	include npkgio.inc
        include comlogic.inc

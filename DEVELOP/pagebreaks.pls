        include common.inc
        include cons.inc

release init    "1.0"
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
VT_R8	EQU 5           .Double - 8 byte Real

xlPageBreakManual integer 4,"0xFFFFEFD9"

	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
        create  ex
        setprop ex,*Visible="False"
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
.
	for result,C1,"200"
		move	result,str9
		call	Trim using str9
		pack	str10,"A",str9
		pack	taskname,"Page ",str9
		setprop sheet.range(str10),*Value=taskname
	repeat
	//Set Manual page breaks - these will supercede automatic ones
	setprop	sheet.Rows(25),*PageBreak=xlPageBreakManual
	move	"54",result
	setprop	sheet.Rows(result),*PageBreak=xlPageBreakManual
	setprop	sheet.Rows(121),*PageBreak=xlPageBreakManual

CampaignFileNameSelect
	setprop ex,*Visible="True"
        pack	taskname,"c:\work\"	."
        setprop ex,*DefaultFilePath=taskname
        pack    taskname,taskname,"pagebreak"
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
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sheet
        destroy sheets
        destroy book
        destroy books
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

        include comlogic.inc

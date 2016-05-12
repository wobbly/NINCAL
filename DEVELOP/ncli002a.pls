        include common.inc
        include cons.inc
        include norddd.inc
           INCLUDE    COMPDD.inc
           INCLUDE    CNTDD.inc

release init    "1.0" ASH 15MAR2006 NEW RELEASE


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
colornum form         24
yellow     color
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
xlRowHeight           variant
VT_R8      EQU 5           .Double - 8 byte Real
.AcctType variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
XlVAlignTop Integer  4,"0xFFFFEFC0"          .-4160
AutoCalc integer 4,"0xffffeff7"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlNormal integer 4,"0xFFFFEFD1"
xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"
onehun     integer    4,"0x100"
DblLine integer 4,"0xffffefe9"
MedThick integer 4,"0xFFFFEFD6"
BorderHor integer 4,"0xc"
BorderVert integer 4,"0xb"
SReturn        init   0x0a                     ;soft return/line feed

.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        create  Zoom100,VarType=VT_I4,VarValue=100
        create  Zoom75,VarType=VT_I4,VarValue=75
        create  Zoom85,VarType=VT_I4,VarValue=85
        create  Zoom60,VarType=VT_I4,VarValue=60
.
        create  Zoom87,VarType=VT_I4,VarValue=87
        create  Zoom70,VarType=VT_I4,VarValue=70
        create  Zoom97,VarType=VT_I4,VarValue=97
        create  Zoom32,VarType=VT_I4,VarValue=32
        create  Zoom125,VarType=VT_I4,VarValue=125
.
           create     xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Colors
           create     yellow=255:255:160
.Initialize variables
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
           setprop ex.CommandBars("Standard"),*Visible="True"
           setprop ex.CommandBars("Formatting"),*Visible="True"
           setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C4
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.open using "c:\andrew\strategic\lostclients.xls"
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
.Detail Records
           move       C2,result
           loop
                      move       result,str9
                      call       Trim using str9
                      pack       str11,"A",str9
                      getprop sheet.range(str11),*Value=COMPFLD
                      call       Trim using COMPFLD
                      until (COMPFLD = "")
                      call       COMPKEY
                      call       Trim using COMPCOMP
                      pack       str11,"C",str9
                      setprop sheet.range(str11),*Value=COMPCOMP
                      add        C1,result
           repeat
.
CampaignFileNameSelect
           move       "c:\work\",taskname              ."
           setprop ex,*DefaultFilePath=taskname
           pack    taskname,taskname,"Client"
           setmode *mcursor=*arrow
           ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
           if (taskname <> "0")
                      movelptr taskname,N9
                      reset   taskname,N9
                      append  "xlsx",taskname
                      reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                      trap    TrapCampaignObject if Object
                      book.saveas giving N9 using *Filename=taskname
                      trapclr Object
           endif
           setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.          book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.          destroy sortcol
.          destroy sortcol1
           destroy sheet
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

        include nordio.inc
           INCLUDE    COMPIO.inc
           INCLUDE    CNTIO.inc
        include comlogic.inc

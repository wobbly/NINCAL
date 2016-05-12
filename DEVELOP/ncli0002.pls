        include common.inc
        include cons.inc
        include norddd.inc
           INCLUDE    COMPDD.inc
           INCLUDE    CNTDD.inc

release init    "1.0" ASH 15MAR2006 NEW RELEASE

PackData DataList
range1     dim        10
range2     dim        10
range3     dim        10
range4     dim        10

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
sheet2  automation
sheet3  automation
sheet4  automation
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
.Create work var
           create     PackData=1:1:1:1
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
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet1 using 1
           setprop    sheet1,*Name="Clients-Brokerage"
        sheets.item giving sheet2 using 2
           setprop    sheet2,*Name="Clients-LM"
        sheets.item giving sheet3 using 3
           setprop    sheet3,*Name="Ex-Clients-Brokerage"
        sheets.item giving sheet4 using 4
           setprop    sheet4,*Name="Ex-Clients-LM"
        setprop sheet1.PageSetup,*Orientation=xlLandscape
        setprop sheet2.PageSetup,*Orientation=xlLandscape
        setprop sheet3.PageSetup,*Orientation=xlLandscape
        setprop sheet4.PageSetup,*Orientation=xlLandscape
.As the Zoom property is automatically set to 100% I do not need to set it
.    setprop book.windows(1),*Zoom=Zoom100
           sheet2.activate
           sheet3.activate
           sheet4.activate
.Reset focus on the first Worksheet
           sheet1.activate
.
           setprop    sheet1.range("A1:A1").Rows,*RowHeight=xlRowHeight
           sheet1.range("A1:E1").Merge
           sheet1.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
           setprop    sheet2.range("A1:A1").Rows,*RowHeight=xlRowHeight
           sheet2.range("A1:E1").Merge
           sheet2.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
           setprop    sheet3.range("A1:A1").Rows,*RowHeight=xlRowHeight
           sheet3.range("A1:E1").Merge
           sheet3.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.
           setprop    sheet4.range("A1:A1").Rows,*RowHeight=xlRowHeight
           sheet4.range("A1:E1").Merge
           sheet4.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.....Titles......
           setprop sheet1.range("A4"),*Value="Brokerage Client Report - Last 12 Months - Ranked by Volume ($ of Commissions)"
           setprop sheet1.range("A4").Font,*Bold="True",*Size=12
.
           setprop sheet2.range("A4"),*Value="Management Client Report - Last 12 Months - Ranked by Volume ($ of Commissions)"
           setprop sheet2.range("A4").Font,*Bold="True",*Size=12
.
           setprop sheet3.range("A4"),*Value="Brokerage Ex-Client Report - Last 18 Months - Ranked by Volume ($ of Commissions)"
           setprop sheet3.range("A4").Font,*Bold="True",*Size=12
.
           setprop sheet4.range("A4"),*Value="Management Ex-Client Report - Last 18 Months - Ranked by Volume ($ of Commissions)"
           setprop sheet4.range("A4").Font,*Bold="True",*Size=12
.....Headers......
           setprop sheet1.range("A6"),*Value="Client"
           setprop sheet1.range("B6"),*Value="Client Name"
           setprop sheet1.range("C6"),*Value="Client Address"
           setprop sheet1.range("D6"),*Value="Years w/NIN"
           setprop sheet1.range("E6"),*Value="LR Income"
           setprop sheet1.range("F6"),*Value="NIN Income"
           setprop sheet1.range("G6"),*Value="Income Total"
           setprop sheet1.range("H6"),*Value="Salesperson"
           setprop sheet1.range("I6"),*Value="Contact"
           setprop sheet1.range("J6"),*Value="Caller"
           setprop sheet1.range("A6:J6").Font,*Bold="True"
           setprop sheet1.range("A6:J6"),*HorizontalAlignment=AlignCenter
           sheet1.range("A6:J6").BorderAround using *LineStyle=1,*Weight=2
.
           setprop sheet2.range("A6"),*Value="Client"
           setprop sheet2.range("B6"),*Value="List Owner Name"
           setprop sheet2.range("C6"),*Value="List Owner Address"
           setprop sheet2.range("D6"),*Value="Years w/NIN"
           setprop sheet2.range("E6"),*Value="LR Income"
           setprop sheet2.range("F6"),*Value="NIN Income"
           setprop sheet2.range("G6"),*Value="Income Total"
           setprop sheet2.range("H6"),*Value="Contact"
           setprop sheet2.range("I6"),*Value="Caller"
           setprop sheet2.range("A6:I6").Font,*Bold="True"
           setprop sheet2.range("A6:I6"),*HorizontalAlignment=AlignCenter
           sheet2.range("A6:I6").BorderAround using *LineStyle=1,*Weight=2
.
           setprop sheet3.range("A6"),*Value="Client"
           setprop sheet3.range("B6"),*Value="Client Name"
           setprop sheet3.range("C6"),*Value="Client Address"
           setprop sheet3.range("D6"),*Value="Years w/NIN"
           setprop sheet3.range("E6"),*Value="LR Income"
           setprop sheet3.range("F6"),*Value="NIN Income"
           setprop sheet3.range("G6"),*Value="Income Total"
           setprop sheet3.range("H6"),*Value="Current Broker"
           setprop sheet3.range("I6"),*Value="NIN Salesperson"
           setprop sheet3.range("J6"),*Value="NIN Contact"
           setprop sheet3.range("K6"),*Value="NIN Caller"
           setprop sheet3.range("A6:K6").Font,*Bold="True"
           setprop sheet3.range("A6:K6"),*HorizontalAlignment=AlignCenter
           sheet3.range("A6:K6").BorderAround using *LineStyle=1,*Weight=2
.
           setprop sheet4.range("A6"),*Value="Client"
           setprop sheet4.range("B6"),*Value="List Owner Name"
           setprop sheet4.range("C6"),*Value="List Owner Address"
           setprop sheet4.range("D6"),*Value="Years w/NIN"
           setprop sheet4.range("E6"),*Value="LR Income"
           setprop sheet4.range("F6"),*Value="NIN Income"
           setprop sheet4.range("G6"),*Value="Income Total"
           setprop sheet4.range("H6"),*Value="Current LM"
           setprop sheet4.range("I6"),*Value="NIN Contact"
           setprop sheet4.range("J6"),*Value="NIN Caller"
           setprop sheet4.range("A6:J6").Font,*Bold="True"
           setprop sheet4.range("A6:J6"),*HorizontalAlignment=AlignCenter
           sheet4.range("A6:J6").BorderAround using *LineStyle=1,*Weight=2

.Title Rows
           pack    str11,"1:6"
           setprop sheet1.PageSetup,*PrintTitleRows=str11
           setprop sheet2.PageSetup,*PrintTitleRows=str11
           setprop sheet3.PageSetup,*PrintTitleRows=str11
           setprop sheet4.PageSetup,*PrintTitleRows=str11
.
           move       C6,howmany,howmany2,howmany3,howmany4
.Detail Records
.TESTING
           add        C1,howmany
           move       howmany,str9
           call       Trim using str9
           pack       range1,"A",str9
           //NumberFormat MUST be established before setting Value!!
           setprop sheet1.range(range1),*NumberFormat="@",*Value="000001"
           pack       range1,"B",str9
           setprop sheet1.range(range1),*Value="Some Client that we work with"
           pack       range1,"C",str9
           pack       taskname,"1556 8th Avenue",SReturn,"SF, CA 94122"
           setprop sheet1.range(range1),*Value=taskname
           pack       range1,"D",str9
           setprop sheet1.range(range1),*Value=""
           pack       range1,"E",str9
           setprop sheet1.range(range1),*Value="1004.98",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
           pack       range1,"F",str9
           setprop sheet1.range(range1),*Value="100.09",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
           pack       range1,"G",str9
           pack       taskname,"=SUM(E",str9,":F",str9,")"
           setprop sheet1.range(range1),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
           pack       range1,"H",str9
           setprop sheet1.range(range1),*Value="Susan Anstrand"
           pack       range1,"I",str9
           setprop sheet1.range(range1),*Value="Mac McIntosh"
           pack       range1,"J",str9
           setprop sheet1.range(range1),*Value="Kattt Sammon"


..SORT SHEET 2
..Sort by List Name
.          move    LastRec,str9
.          call       Trim using str9
.          pack       range1,"AM",str9
..Select a column on which to sort
..This is ugly code.  You need to set the key value of the Sort method to a specific column.
..The Columns property returns a Range object, which is then used by the Sort method.
..Again, all this info found in the Object Browser in Excel.
..List Name
.        move    FirstRec,str9
.        call    Trim using str9
.        pack    range2,"E",str9
.        getprop sheet2.range(range2),*Columns(1)=sortcol
..Package Name
.        pack    range2,"J",str9
.        getprop sheet2.range(range2),*Columns(1)=sortcol1
..Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.        pack    range2,"A",str9
.        sheet2.range(range2,range1).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.        move    howmany,N10
.        add     "1",N10
.        move    N10,str10
.        call    Trim using str10
.        move    howmany2,N10
.        add     "1",N10
.        move    N10,str12
.        call    Trim using str12
.          move       hdrrow,str9

.Autofit
           move       howmany,str9
           call       Trim using str9
           pack       range1,"A6"
           pack       range2,"J",str9
           sheet1.range(range1,range2).Columns.Autofit
           sheet1.range(range1,range2).Rows.Autofit
           setprop    sheet1.range(range1,range2),*VerticalAlignment=XlVAlignTop
.
           move       howmany2,str9
           call       Trim using str9
           pack       range1,"A6"
           pack       range2,"I",str9
           sheet2.range(range1,range2).Columns.Autofit
           sheet2.range(range1,range2).Rows.Autofit
           setprop    sheet2.range(range1,range2),*VerticalAlignment=XlVAlignTop
.
           move       howmany3,str9
           call       Trim using str9
           pack       range1,"A6"
           pack       range2,"K",str9
           sheet3.range(range1,range2).Columns.Autofit
           sheet3.range(range1,range2).Rows.Autofit
           setprop    sheet3.range(range1,range2),*VerticalAlignment=XlVAlignTop
.
           move       howmany4,str9
           call       Trim using str9
           pack       range1,"A6"
           pack       range2,"J",str9
           sheet4.range(range1,range2).Columns.Autofit
           sheet4.range(range1,range2).Rows.Autofit
           setprop    sheet4.range(range1,range2),*VerticalAlignment=XlVAlignTop

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
           destroy sheet1
           destroy sheet2
           destroy sheet3
           destroy sheet4
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
           destroy    PackData
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

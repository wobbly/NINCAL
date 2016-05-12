	include common.inc
	include cons.inc
 	include xls.inc
 	
release init "1.0"
//Excel Vars
C15				Form 	 "15"
CELLPOINT			Form	 5

//Create the Variant objects
//Initialize variables
	create  Zoom85,VarType=VT_I4,VarValue=70
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	xlRowHeight,VarType=VT_R8,VarValue="15.0"
//"1" increment in Excel interface equals "1.3888" in OLE logic
	create	TopMargin,VarType=VT_R8,VarValue="18"		Roughly equals .25 inches:  18 * 1.388 = 25
	create	BottomMargin,VarType=VT_R8,VarValue="36"	Roughly equals .50 inches:  36 * 1.388 = 50
	create	xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
	create	xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"	

	create	xlColumnWidth,VarType=VT_R8,VarValue="0.0" 
 
//.Open Excel application
	create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True"
	setprop ex.CommandBars("Standard"),*Visible="True"
	setprop ex.CommandBars("Formatting"),*Visible="True"
	setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
//........................
//.	setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
//.	setprop ex,*DisplayFullScreen=OTRUE
//........................
//.Reset Default of Worksheets found in a Workbook
		        getprop ex,*SheetsInNewWorkbook=SheetsDefault
		        setprop ex,*SheetsInNewWorkbook=C1
//........................
//	getprop ex,*CommandBars=bars
//	getprop	bars,*ActiveMenuBar=bar
//	setprop	bar,*Visible="True"
//	setprop	bar,*Position=menubar
//........................
//.Create Workbooks collection
		        getprop ex,*Workbooks=books
//.Create/Add a single Workbook

	books.open giving book using *Filename="\\nts1\d\INFORMATIONSERVICES\Data Exchange\MMI\15673lol.xls"

        book.saveas giving N9 using *Filename="\\nts1\d\INFORMATIONSERVICES\Data Exchange\MMI\15673loltest.csv",*FileFormat=xlCSV
	setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        ex.quit
        destroy ex
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
	destroy Zoom65
        destroy sortcol
        destroy sortcol1
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        Destroy exrange
	Destroy HPageBreaks
	Destroy HPageBreak			

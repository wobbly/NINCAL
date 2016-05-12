	include common.inc
	include cons.inc
 	include xls.inc
 	
release init "1.0"
//Excel Vars
C15				Form 	 "15"
CELLPOINT			Form	 5
num9    form 9
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
			books.open giving book using *Filename="\\nts1\e\data\diskin62.csv"
			Getprop book,*Sheets=sheets
        		sheets.item giving sheet using 1			
        		book.PivotCaches giving PivotCaches
	       		Sheet.PivotTableWizard giving PivotTable *SourceData=range("diskin12!$A$1:$F$7308")
	       		Setprop PivotTable.PivotFields("List Name"),*Orientation=xlRowField *Position=1


	       		Setprop PivotTable.PivotFields("Mail Date"),*Orientation=xlColumnField *Position=1
			PivotTable.PivotSelect using *NAME="",*MODE=xlDataAndLabel			
			

		
			
.    ActiveSheet.PivotTables("PivotTable1").AddDataField ActiveSheet.PivotTables( _
.        "PivotTable1").PivotFields("Order Date"), "Count of Order Date", xlCount
.    ActiveSheet.PivotTables("PivotTable1").AddDataField ActiveSheet.PivotTables( _
.        "PivotTable1").PivotFields("Rental Qty"), "Sum of Rental Qty", xlSum
.    ActiveSheet.PivotTables("PivotTable1").AddDataField ActiveSheet.PivotTables( _
.        "PivotTable1").PivotFields("Exchange Qty"), "Sum of Exchange Qty", xlSum








			
        		sheets.item giving sheet1 using 1
        		getprop sheet1,*Name=str55
        		getprop sheet,*Name=str55        		
			Sheet1.Range("B2").Select			
			
			
.			move "37259" to juldays
			
.			call cvtgreg

			TimePeriod.SetElemCount	giving n9 USING *Size=7
			TimePeriod.GetElemCount giving n9

 			TimePeriod.PutElement giving n8 using *Value=OFALSE:
 			*Index=c1
 			TimePeriod.PutElement giving n8 using *Value=OFALSE:
 			*Index=c2
 			TimePeriod.PutElement using *Value=OFALSE:
 			*Index=c3
 			TimePeriod.PutElement using *Value=OFALSE:
 			*Index=c4
 			TimePeriod.PutElement using *Value=OFALSE:
			*Index=c5
 			TimePeriod.PutElement using *Value=OTRUE:
 			*Index=c6
			TimePeriod.GetElemCount giving n9

	       		Setprop PivotTable.PivotFields("Order Qty"),*Orientation=xlColumnField,*Position=2

			PivotTable.PivotFields.item giving DataField Using "Order Qty"			
			PivotTable.AddDataField using DataField "Sum of Order Qty" xlSum

  			Sheet1.Range("B1").Group using *Start=OTRUE, *End=OTrue, *Periods=Timeperiod

			PivotTable.PivotFields.item giving DataField Using "Exchange Qty"			
			PivotTable.AddDataField using DataField "Exch Qty" xlSum

			PivotTable.PivotFields.item giving DataField Using "Rental Qty"			
			PivotTable.AddDataField using DataField "Rent Qty" xlSum			
			
			Setprop PivotTable.DataPivotField,*Orientation=xlColumnField,*Position=2
			
			Getprop sheet1,*UsedRange=exRange
			Getprop exrange.rows,*count=n9

			add c2 to n9			
			move n9 to str9
			call trim using str9
			pack str10,"A",str9
			pack taskname with "Rental Totals"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignLeft

			call trim using str9
			pack str10,"B",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"E",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"H",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight


			call trim using str9
			pack str10,"K",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight


	        	
			call trim using str9
			pack str10,"N",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight
	        	
			call trim using str9
			pack str10,"Q",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight	        	
	        	
	        	
			call trim using str9
			pack str10,"T",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight
	        	
			call trim using str9
			pack str10,"W",str9
			pack taskname with "=R[-2]C[2]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight	        	
	        	

	        	
			add c1 to n9
			move n9,str9

			call trim using str9
			
			pack str10,"A",str9
			pack taskname with "Exchange Totals"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignLeft			
			
			pack str10,"B",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"E",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"H",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"K",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"N",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"Q",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight

			call trim using str9
			pack str10,"T",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight
	        	
			call trim using str9
			pack str10,"W",str9
			pack taskname with "=R[-3]C[1]"
	        	setprop sheet1.range(str10),*Value=Taskname,*HorizontalAlignment=AlignRight	        	
	        	
	        	
	        	
	        	pack    str11,"1:","3"
           	        setprop sheet1.PageSetup,*PrintTitleRows=str11	
        		Setprop sheet1.PageSetup,*Orientation=xlLandscape  
	        	Setprop sheet1.PageSetup,*Zoom=Zoom60			        		
	        	
			setprop sheet1.range("C1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("D1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("F1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("G1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("I1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("J1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("L1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("M1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("O1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("P1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("R1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("S1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("U1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("V1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("X1"),*ColumnWidth=xlColumnWidth
			setprop sheet1.range("Y1"),*ColumnWidth=xlColumnWidth			



	stop

	include comlogic.inc


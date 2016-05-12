.Variant Types0
VT_EMPTY	EQU	0	.nothing
VT_NULL		EQU	1	.SQL style null
VT_I2		EQU	2	.2 byte signed integer
VT_I4		EQU	3	.4 byte signed integer
VT_R4		EQU	4	.4 byte real - Float
VT_R8		EQU	5	.8 byte real - Double
VT_CY		EQU	6	.Currency
VT_DATE		EQU	7	.Date
VT_BSTR		EQU	8	.OLE automation string
VT_DISPATCH	EQU	9	.Idispatch FAR *
VT_ERROR	EQU	10	.SCODE
VT_BOOL		EQU	11	.Boolean - True = -1, False = 0
VT_VARIANT	EQU	12	.VARIANT FAR *
VT_UNKNOWN	EQU	13	.Unknown type
VT_UI1		EQU	17	.Unsigned character
VT_ARRAY	EQU	0x2000	.Variant Array
. Modified 06/13/08 to allow more rows  --- DH
.//In order to use any of the properties/methods associated with all parent objects
.//of the Worksheet, I need to create automation objects for each of them.
.//
.//Look at Excel Object Model to understand heirarchy.  This can be found in hard
.//documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.//Software available via PL/B Designer - create a Container object on a form, create an Excel
.//Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.//Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.//Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.//In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
.//view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.//option to locate Help topics to see specifics.
.//
.//General heirarchy:
.// Excel Application
ex      			AUTOMATION       class="Excel.Application"
.//       Workbooks Collection (all open Workbooks)
books   			AUTOMATION
.//               Single Workbook
book    			AUTOMATION
.//                       Worksheets Collection (all Worksheets in this Workbook)
sheets  			AUTOMATION
.//                               Single Worksheet
sheet   			AUTOMATION
sheet1   			AUTOMATION
.//                                       SortColumn (a Single Column in that Worksheet used for sorting)
sortcol 			AUTOMATION
sortcol1			AUTOMATION
.//					 Range within a worksheet
exrange      			AUTOMATION       

PivotCaches                     AUTOMATION
PivotCache                     AUTOMATION 
PivotTable                     AUTOMATION 
DataFields			AUTOMATION
DataField			AUTOMATION
........................
.//bars	automation
.//bar	automation
.//menubar INTEGER 2,"0x00000006"
.......................


HPageBreaks			AUTOMATION
HPageBreak			AUTOMATION

.//Variable for use in spreadsheet
OTRUE   			VARIANT
OFALSE  			VARIANT

.//Zoom allows you to adjust the size of the data within a sheet in order to allow certain information to appear within a sheet.
Zoom85  			VARIANT
Zoom80  			VARIANT
Zoom75  			VARIANT
Zoom70  			VARIANT
Zoom65  			VARIANT
Zoom60  			VARIANT
Zoom55  			VARIANT


VT_R8A				EQU		 5           .Double - 8 byte Real
.//A general width for columns - more may be added within your own program
xlColumnWidth			VARIANT

.//Margins of a page
TopMargin			VARIANT
BottomMargin			VARIANT
LeftMargin			VARIANT
RightMargin			VARIANT

.//The thickness of a line
xlBorderWeightMedium		VARIANT
.//Manually sets the page break when excel does not
xlPageBreakManual			VARIANT
.//Have not used this yet
xlPageBreakAutomatic 		VARIANT
.//Index of a worksheet
SheetIndex			VARIANT

TimePeriod			VARIANT

xlCenter			VARIANT
.//Average row height is 12 in 2003 15 in 2007
xlRowHeight			VARIANT

.//Formatting vars needed
.//This constant was found in the Object Browser in Excel under the Help topic for the
.//HorizontalAlignment property of the Range object.
AlignLeft              		integer 4,"0xffffefdd"
AlignRight             		integer 4,"0xffffefc8"
AlignCenter            		integer 4,"0xffffeff4"
.//Tells how many sheets by default will appear in a new workbook - in 2003 it's 3
SheetsDefault          		integer 4,"0x00000000"

xlPortrait            		integer 4,"0x1"                     .1
xlLandscape            		integer 4,"0x2"                     .2

xlMinimized            		integer 4,"0xFFFFEFD4"

xlUnderlineStyleSingle		integer 4,"0x2"


.//              //LineStyle 1 = Continuous
.//              //LineStyle 4 = Dash Dot
.//              //Object Viewer Help tells us that xlInsideHorizontal has a value of "12" Decimal
.//              //Object Viewer Help tells us that xlInsideVertical has a value of "11" Decimal
.//              //Object Viewer Help tells us that xlEdgeright has a value of "10" Decimal
.//               sheet.range("B11:b37").BorderAround using *LineStyle=1,*Weight=3
.//Used to define range borders
xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range


.//Delete Cell and Direct Which way to shift cells
.//              Sheet.Range("L1:L37").Delete Using xlShiftToLeft                 
.//              Sheet.Range(CellRange).Delete Using xlShiftUp                 
.//
XLShiftToLeft		Variant                          .range delete shift to left
XLShiftUp    		Variant                          .range delete shift Up     
xlCSV			Variant
xlDatabase              integer 4,"0x1"
xlRowField              integer 4,"0x1"
xlColumnField           integer 4,"0x2" 
xlsum			Variant
xlDataAndLabel          integer 4,"0x0" 
xlPivotTableVersion12   integer 4,"0x3"
.//Variable used to pack with cell corrdinates 
.//	Pack Cell,"A","5"
Cell	DIM	7
Cell1	DIM     	7
.Cell 	   			DIM	5
.Cell1                           DIM     5
.//Variable Used to pack range that you will set properties on.						
.//Pack Cell,"A",Str5
.//Pack Cell1,"S",str5
.//Pack CellRange,Cell,":",Cell1
CellRange  			DIM   255
CellRowCnt			Form	6
CellRowCnt1			Form	6
CellRowCnt2			Form	6
CurCellNum                      Form    6

.CellRowCnt			Form	5
.CellRowCnt1			Form	5
.CellRowCnt2			Form	5
.CurCellNum                      Form    5
.//used to keep track of what row you are on for so you can id when a soft page break will occur - soft page break is when excel does an auto page break
.//Max row before a soft page break on page 1 - Page 1 appears to have a different max then do the other pages may be because i repeat the header on other pages.
RowMaxPage1   			Form	9      
.//Max row before a soft page break - all except page 1
RowMax 	  			Form	9      //Max row before a soft page break

.//Colors for font coloring
Red 				Color
RGB 				Form 24

SourceData			VARIANT


XLSNAME    DIM  255

.//Source Data for Pivot Table
.	create  SourceData,VarType=VT_BSTR,VarValue="diskin57!A1:F7308"	


.//Create the Variant objects
.//Initialize variables

.//Zoom initialization Values
	create  Zoom85,VarType=VT_I4,VarValue=85
	create  Zoom80,VarType=VT_I4,VarValue=80
	create  Zoom75,VarType=VT_I4,VarValue=75
	create  Zoom70,VarType=VT_I4,VarValue=70
	create  Zoom65,VarType=VT_I4,VarValue=65
	create  Zoom60,VarType=VT_I4,VarValue=60
	create  Zoom55,VarType=VT_I4,VarValue=55	

	
.//Boolean Values
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
.//should be defined by your prgram as they may need to be formatted differently	
.//	create	xlColumnWidth,VarType=VT_R8,VarValue="0.0"

.//"1" increment in Excel interface equals "1.3888" in OLE logic

.//	create	TopMargin,VarType=VT_R8,VarValue="18"		Roughly equals .25 inches:  18 * 1.388 = 25
.//	create	BottomMargin,VarType=VT_R8,VarValue="36"	Roughly equals .50 inches:  36 * 1.388 = 50
.//	create	LeftMargin,VarType=VT_R8,VarValue="14"		Roughly equals .25 inches:  18 * 1.388 = 25	

.//	create	xlRowHeight,VarType=VT_R8,VarValue="15.0"


.//Constants 
	create	xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
	create	xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"	
	create	xlCenter,VarType=VT_R8,VarValue="-4108"		
	create	xlBorderWeightMedium,VarType=VT_R8,VarValue="-4138"	
	create	SheetIndex,VarType=VT_I4        		
       	create  xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
       	create  xlShiftUp,VarType=VT_R8,VarValue="-4162"
        
	create  xlsum,VarType=VT_R8,VarValue="-4157" (&HFFFFEFC3)        
	create  xlCSV,VarType=VT_I4,VarValue="6"        	
//create red for font	
        create  	Red=255:0:0
	
	
	getitem	Red,0,RGB

	create Timeperiod,VarType=(VT_ARRAY+VT_BOOL),VarValue=OFALSE
.test1
.	Move   (VT_ARRAY+VT_BOOL),TimePeriod	
.vtarrayt integer 4 ..array of integers	
.	MOVE	(VT_ARRAY+VT_BOOL),vtarrayt ;or same as add in this case ;)	
.CREATE	safearray,VARTYPE=vtarrayt

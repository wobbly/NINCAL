	include	common.inc
	include	cons.inc
release	init	"test"
...........................................
.Sample for using MSFlexGrid 6.0 and MSListView 6.0
.
.	Andrew Harkins
.	10/26/2001
.
...........................................
.Use RegSrv32 to register *.OCX files
.Create a form named Form1
.Create an MS FlexGrid named FlexGrid1
.Create an MS ListView named ListView1
.Compile and run program
...........................................
.result	form	9
.howmany	form	9
.N9	form	9
.C0	form	"0"
.C1	form	"1"
.SEQ	form	"-1"
.str9 	dim	9
.str10	dim	10
.str11	dim	11

tempfile file
tempfile2 file
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
OBOOL	variant

test	integer	4
test2	integer	4
ColHeads automation
ColHead	automation
ListIts	automation
ListIt	automation
SubIt	automation

$Click CONST "4294966696"
$MouseMove CONST "4294966690"
$ColumnClick CONST "3"

mauve	color
colornum form	24
yellow	color
red	color
blue	color
RowHold	form	9
	
x	plform	form1
	winhide
	formload x

	open	tempfile,"C:\work\tempfile.dat"
	open	tempfile2,"C:\work\tempfile2.dat"
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  OBOOL,VarType=VT_BOOL,VarValue=0
	create	blue=*blue
	create	red=*red
	create	yellow=255:255:160
        create  mauve=150:60:100
	getitem	mauve,0,colornum

.Set some properties for FlexGrid object
	setprop	FlexGrid1,*Cols=5
	setprop	FlexGrid1,*FixedCols=0
	setprop	FlexGrid1,*FixedRows=1
	setprop	FlexGrid1,*OLEDropMode=1
.Register Events
	eventreg FlexGrid1,$Click,Click_FlexGrid1
	eventreg FlexGrid1,$MouseMove,MouseMove_FlexGrid1
.
	eventreg ListView1,$ColumnClick,ColumnClick_ListView giving ColHead
.Set Column Header Text
	setprop FlexGrid1,*TextMatrix(0,0)="Index"
	setprop FlexGrid1,*TextMatrix(0,1)="String Asc."
	setprop FlexGrid1,*TextMatrix(0,2)="String Des."
	setprop FlexGrid1,*TextMatrix(0,3)="Numeric Asc."
	setprop FlexGrid1,*TextMatrix(0,4)="Numeric Des."
.
        getprop ListView1,*ColumnHeaders=ColHeads
	ColHeads.Add using *Index=1,*Key="one",*Text="Index",*Width=50
	ColHeads.Add using *Index=2,*Key="two",*Text="String Asc.",*Width=70
	ColHeads.Add using *Index=3,*Key="three",*Text="String Des.",*Width=70
	ColHeads.Add using *Index=4,*Key="four",*Text="Numeric Asc.",*Width=70
	ColHeads.Add using *Index=5,*Key="five",*Text="Numeric Des.",*Width=70
.Set some properties for ListView object
	setprop	ListView1,*HideColumnHeaders=OFALSE
	setprop	ListView1,*HideSelection=OFALSE
	setprop	ListView1,*HotTracking=OTRUE
	setprop	ListView1,*FullRowSelect=OTRUE
	setprop	ListView1,*MultiSelect=OTRUE
	setprop	ListView1,*Sorted=OTRUE
	setprop	ListView1,*SortOrder=0
	setprop	ListView1,*AllowColumnReorder=OTRUE
	setprop	ListView1,*View=3

.Add Items
	getitem	red,0,colornum
        getprop ListView1,*ListItems=ListIts
	for result,"1","25"
		FlexGrid1.AddItem using result
		move	result,str9
		call	Trim using str9
		ListIts.Add giving ListIt using *Index=1,*Text=str9
		setprop FlexGrid1,*TextMatrix(result,0)=str9
.
		read	tempfile,SEQ;str10
		read	tempfile2,SEQ;str11
.One way to add sub-items - Implicit
		setprop ListIt,*SubItems(1)=str10
		setprop ListIt,*SubItems(2)=str10
.Another way to add sub-items (and allow setting of properties) - Explicit
		ListIt.ListSubItems.Add giving SubIt using *Text=str11
		call	Trim using str11
		move	C0,N9
		move	str11,N9
		if (N9 < C0)
			setprop SubIt,*ForeColor=colornum
		endif
		ListIt.ListSubItems.Add giving SubIt using *Text=str11
		if (N9 < C0)
			setprop SubIt,*Bold=OTRUE
		endif
		for howmany,"1","4"
			if (howmany = 3 | howmany = 4)
				setprop FlexGrid1,*TextMatrix(result,howmany)=str11
			else
				setprop FlexGrid1,*TextMatrix(result,howmany)=str10
			endif
		repeat
	repeat
.	setprop	FlexGrid1,*ColWidth(0)=0
.Allow resizing of columns
	setprop	FlexGrid1,*AllowUserResizing=1
.Force selection by rows as opposed to individual cells or columns
	setprop	FlexGrid1,*SelectionMode=1
.	setprop	FlexGrid1,*AllowBigSelection=OTRUE
.Colors
	getitem	red,0,colornum
	setprop	FlexGrid1,*ForeColorFixed=colornum
	getitem	mauve,0,colornum
	setprop	FlexGrid1,*Row=3
	for howmany,"0","4"
		setprop	FlexGrid1,*Col=howmany
		setprop	FlexGrid1,*CellForeColor=colornum
	repeat
.Strike Through
	setprop	FlexGrid1,*Row=4
	for howmany,"0","4"
		setprop	FlexGrid1,*Col=howmany
		setprop	FlexGrid1,*CellFontStrikeThrough=OTRUE
	repeat
.Underline
	setprop	FlexGrid1,*Row=5
	for howmany,"0","4"
		setprop	FlexGrid1,*Col=howmany
		setprop	FlexGrid1,*CellFontUnderline=OTRUE
	repeat
	setprop	FlexGrid1,*fixedrows=1
	loop
		waitevent
	repeat

Click_FlexGrid1
.You must find where the Mouse is sitting, NOT where the focus is.
.With a Fixed Header, you will never pull the first ColIndex, and with
.SelectionMode property set to "Whole Row", you will never pull anything
.but the first RowIndex.
	getprop	FlexGrid1,*MouseRow=howmany
	if (howmany = 0)
		getprop	FlexGrid1,*MouseCol=result
.If you do not set the Col property, you will always sort on the first column - see documentation above.
		setprop	FlexGrid1,*Col=result
		if (result = 0)		.Numeric Ascending
			setprop	FlexGrid1,*Sort=3
		elseif (result = 1)	.Character case-insensitive Ascending
			setprop	FlexGrid1,*Sort=5
		elseif (result = 2)	.Character case-insensitive Descending
			setprop	FlexGrid1,*Sort=6
		elseif (result = 3)	.Numeric Ascending
			setprop	FlexGrid1,*Sort=3
		elseif (result = 4)	.Numeric Descending
			setprop	FlexGrid1,*Sort=4
		endif
	endif
	return

MouseMove_FlexGrid1
.	getprop	FlexGrid1,*MouseRow=howmany
.	getprop	FlexGrid1,*RowSel=result
..Don't modify CellForeColor on currently selected items, nor Column Headers
.	if (howmany <> result AND howmany <> 0)
.		setprop	FlexGrid1,*Row=RowHold
.		for N9,"0","4"
.			setprop	FlexGrid1,*Col=N9
.			setprop	FlexGrid1,*CellForeColor=0
.		repeat
.		getitem	blue,0,colornum
.		setprop	FlexGrid1,*Row=howmany
.		for N9,"0","4"
.			setprop	FlexGrid1,*Col=N9
.			setprop	FlexGrid1,*CellForeColor=colornum
.		repeat
.		move	howmany,RowHold
.	endif
	return

ColumnClick_ListView
.Documentation for MS tells us one parameter is returned and that is the particular
.Column Header that was clicked.
	eventinfo C0,ARG1=ColHead
.We use Key as Index will change when sorting occurs
	getprop	ColHead,*Key=str10
	getprop ColHead,*Index=result
	sub	C1,result
	setprop	ListView1,*SortKey=result
	if (str10 = "one")
		setprop	ListView1,*SortOrder=0
	elseif (str10 = "two")
		setprop	ListView1,*SortOrder=0
	elseif (str10 = "three")
		setprop	ListView1,*SortOrder=1
	elseif (str10 = "four")
		setprop	ListView1,*SortOrder=0
	elseif (str10 = "five")
		setprop	ListView1,*SortOrder=1
	endif
	return

	include	comlogic.inc

...............................................................................
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.         inCLUDE   oSLSPERN.INC
	include	ncntdd.inc



. income by salesperson dumped to excel
.Ninv0019 creates/updates the input fi;e
Release   Init      "1.00"      DLH  New Year
Reldate   Init      "2014 June 2"
Results   IFILE      Keylen=8,FIX=37          .PHYSICAL INput FILE "lRdetMST.ISI|10.10.30.103:502"
.file layout
resultvars    list
ResultKey     dim           8        1-8   salesnum+ccyymm          
TotINV        form          5        9-13
TotNIN	    Form	        9.2     14-25
TOTLR	    Form	        9.2     26-37
              listend





.xx = sales number
.TotINV        form          5                    9-13
.TotNIN         Form          9.2                 14-25
.TOTLR         Form          9.2                 26-37
.
. ..........................................................................
.
.
.
DATE     DIM       8
TIME     DIM       8
YR       FORM      2         POS 2-3    YEAR
MO       FORM      2         POS 5-6    MONTH
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
MINIT      DIM       2         POS 17-18  MINUTE
SEC      DIM       2         POS 20-21  SECOND
.
. .WORK SPACE
.
FINAL    DIM       1         CHECKS END OF PROGRAM
C        DIM       1         CHECKS FOR "0" IN TOTAL
OK       DIM       1
CHECK    DIM       1
.
COUNT    FORM      2
COUNT1   FORM      5         COUNTS INVOICE RECORDS
COUNT2   FORM      5         OUTPUT MONTH COUNT
COUNT3   FORM      5         OUTPUT YEAR COUNT
WORK     FORM      10.4
B14      DIM       14
B17      DIM       17
B20      DIM       20
B23      DIM       23
.
.
. .TOTAL INVOICE WORK FIELDS
.
.TOTINV   FORM      5
FININV   FORM      5(3)         TOTAL INVOICES PLACED IN YEAR
FORM5    FORM      5
.
. .ACCOUNTS PAYABLE WORK FIELDS
.
.TOTNIN    FORM      9.2
FINNIN    FORM      9.2(3)         TOTAL NAMES INVOICED DURING YEAR

FORM9P   FORM      10
ACCPAY   FORM      10
FORM9    FORM      10
.
. .ACCOUNTS RECIEVABLE WORK FIELDS
.
.TOTLR    FORM      9.2
FINLR    FORM      9.2(3)
FINLRB1  DIM       10
FORM9R   FORM      10
ACCREC   FORM      10
EndLRA    FORM      9.2
EndLRB    FORM      9.2
FORM7    FORM      7
.
ColumnA	dim	2
ColumnB	dim	2
ColumnC	dim	2
Formula	Dim	25
.
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"


..............................................
.Array

.
StartMonth	Dim	2(12),("02"),("03"),("04"),("05"),("06"),("07"),("08"),("09"),("10"),("11"),("12"),("01")
calcrow		Dim	2(12),("10"),("11"),("12"),("13"),("14"),("15"),("16"),("17"),("18"),("19"),("20"),("21")
HoldStartMM	Form	2
HoldStartYr	form	2
Curryy		form	2
currmm		form	2
.
slsfound		form	2
MonthCOunt	Form	2
holdToday       dim         6


calcinv       form          5
calcLR     form          9

TOTinvA	   FOrm	5(12)
TOTLRA     form          9(12)


......................................................................................................
.some excel goodies
.to find version of excel
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end to find version of excel

sheetno    form      2
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
N34     form    3.4

VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
XLAlignLeft                               integer 4,"0xffffefdd"
XlAlignRight                              integer 4,"0xffffefc8"
xlAlignCenter integer 4,"0xffffeff4"
xlBottom  integer      4,"0xffffeff5"
XlLineStyleDBl Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2
xlUnderlineStyleSingle integer 4,"0x2"

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidth2 variant
xlColumnWidth13 variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant

ExRange1      Dim             6
ExRange2      Dim             6
ExRange3      Dim            14
Row1          form            3
Row2          Form            3
Row3          form            3
Row4          Form            3
DimRow1       Dim             3

.
         MOVE      "NINV0027" TO PROGRAM
         MOVE      "EXIT" TO PF5
         MOVE      "Names in the News" TO COMPNME
         MOVE      "INV Sales Rep ANALYSIS Report" TO STITLE
         move      c1 to v
         TRAP      Stop IF F5
         CALL      PAINT
         CALL      FUNCDISP
.....INITIAL TIME ANS SCREEN DISPLAY;
.
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
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
        create   xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
        create   xlShiftUp,VarType=VT_R8,VarValue="-4162"
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
        setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
        create         xlColumnWidth,VarType=VT_R8,VarValue="0.0"                     .Default
        create    xlColumnWidth2,VarType=VT_R8,VarValue="2"  
        create    xlColumnWidth13,VarType=VT_R8,VarValue="13"  
          setprop ex,*Visible=OTRUE


BEGIN
          DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
          clock     date to today
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
.calc start date
	move	mm,n2
	Move	Startmonth(n2),Holdstartmm
	if	(HoldStartMm < "12")
	move	yy,n3
	calc	HoldStartyr=(n3-1)
	else
	move	yy,Holdstartyr
	endif
	move	holdstartyr,curryy
	move	holdstartmm,currmm
	move	c4,ncntpath	.read by salesperson
	move	c0,slsfound	.number of valid sales people found	
.
	move	holdstartmm,N2
          Move       "10",Row1
	For	monthcount from c1 to "12" using c1

	call	BigDate
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=str9
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
	if	(n2 = "12")
	move	c1,N2
	else
	add	c1,n2
	endif
	add	c1,row1
	repeat
	Move	"23",DimRow1
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Totals"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          pack      ExRange1,"A8"
          pack      ExRange2,"D23"
          sheet.range(ExRange1,ExRange2).BorderAround using *LineStyle=1,*Weight=2

	move	"8",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          pack      Exrange2 from "D",DimRow1
          pack      Exrange3,Exrange1,":",Exrange2
          setprop sheet.range(ExRange1,ExRange1),*Value="Totals",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          sheet.range(EXRange3).Merge
	move	"9",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Invoices"
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Income"

.
         OPEN      Results,"lRdetMST.ISI|NINS1:502",exclusive
....................................
. loop for sales person
	loop
	call	ncntks
	until	over
	rep	zfill,cntsales
	if	(CntInactive <> yes & cntsales <> "00")		.check if active
	add	c1,slsfound
. loop date parameters
	call	subhead	
.reset starting dates
	move	holdstartyr,curryy
	move	holdstartmm,currmm

		For	monthcount from c1 to "12" using c1
		
		packkey	ResultKey from CntSales,cc,curryy,currmm
		rep	zfill,resultkey
		read       results,ResultKey;resultvars
			if        Not over
			call	populate
			endif
		if	(currmm < 12)
		add	c1,currmm
		else
		move	c1,currmm
		add	c1,curryy
		endif
                    Repeat
	endif

.increment date here
          REpeat                        
.
.finish up and stop
.set width first
	pack    EXrange1,"A8"
	pack    Exrange2,ColumnC,"23"
	sheet.range(Exrange1,Exrange2).Columns.Autofit	

	setprop xlColumnWidth,VarValue="1.86"
	pack    EXrange1,"D8"
	pack    EXrange2,"D23"
	setprop sheet.range(EXrange1,EXrange2).Columns,*ColumnWidth=xlColumnWidth


	Move	"B",ColumnA
	Move	"C",ColumnB
	For	monthcount from c1 to "12" using c1
	move	TotInvA(Monthcount),TotInv
	move	TotLRA(Monthcount),TotLR
	move	calcrow(Monthcount),row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=TotInv,*NumberFormat="##,####0_)"
          pack      Exrange2 from ColumnB,DimRow1
          setprop sheet.range(ExRange2,ExRange2),*Value=TotLR,*NumberFormat="##,####0_)"

	Repeat
	move	"23",Dimrow1
          pack      Exrange1 from ColumnA,DimRow1
	pack	Formula from "=sum(b10:b21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"
          pack      Exrange1 from ColumnB,DimRow1
	pack	Formula from "=sum(C10:C21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"
	
	goto	stop
.
.
...............................................................................
.................................................................................
Populate
.need away to keep track . pehaps load a table and dump all to excel at the end?
.Column A Month Label
.Column B Total order count
.Column C Total order Vol
.Column D Total order Vol %
.Column E 1st person order count
.Column F 1st person order Vol
.Column G 1st person order Vol %
.Column H 1st person order count
.Column I 1st person order Vol
.Column J 1st person order Vol %
.Start Row 8 NAME
.row 9 Column labels
.row 10 data
	move	calcrow(Monthcount),row1

         add        Totinv to calcINV
         add        TotLR to calcLR
         add	TotNIN to	CalcLR

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=CalcINv,*NumberFormat="##,####0_)"
          pack      Exrange2 from ColumnB,DimRow1
          setprop sheet.range(ExRange2,ExRange2),*Value=CalcLR,*NumberFormat="##,####0_)"
          pack      Exrange1 from ColumnC,DimRow1
	pack	formula from "=",ColumnB,dimrow1,"/C",dimrow1
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="0.0%"
.store totals 
	move	TotInvA(Monthcount),TotInv
         add	CalcINv,TotInv        
	move	TotInv,TotInvA(Monthcount)
	move	TotLRA(Monthcount),TotLR
         add	CalcLR,TotLR
	move	TotLR,TotLRA(Monthcount)

        move          c0 to CalcINv
        move          c0 to CalcLR
        move          c0 to TotInv
        move          c0 to TotLR


............
	Return
....................................................................................
subhead
	if	(slsfound = c1)
	move	"e",ColumnA
	move	"f",ColumnB
	move	"g",ColumnC
	Elseif	(slsfound = c2)
	move	"h",ColumnA
	move	"i",ColumnB
	move	"j",ColumnC
	Elseif	(slsfound = c3)
	move	"k",ColumnA
	move	"l",ColumnB
	move	"m",ColumnC
	Elseif	(slsfound = c4)
	move	"n",ColumnA
	move	"o",ColumnB
	move	"p",ColumnC
	Elseif	(slsfound = c5)
	move	"q",ColumnA
	move	"r",ColumnB
	move	"s",ColumnC
	Elseif	(slsfound = c6)
	move	"t",ColumnA
	move	"u",ColumnB
	move	"v",ColumnC
	Elseif	(slsfound = c7)
	move	"w",ColumnA
	move	"x",ColumnB
	move	"y",ColumnC
	Elseif	(slsfound = c8)
	move	"z",ColumnA
	move	"aa",ColumnB
	move	"ab",ColumnC
	Elseif	(slsfound = c9)
	move	"ac",ColumnA
	move	"ad",ColumnB
	move	"ae",Columnc
	endif
	move	"8",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          pack      Exrange2 from ColumnC,DimRow1
          pack      Exrange3,Exrange1,":",Exrange2
	if	(cntsales = "06")
	move	"List Management",cntname
	endif
	call	Trim using cntname
          setprop sheet.range(ExRange1,ExRange1),*Value=CNTNAME,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          sheet.range(EXRange3).Merge
	move	"9",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Invoices"
          pack      Exrange1 from ColumnB,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Income"
          pack      Exrange1 from ColumnC,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="% Inc."
.totals
	move	"23",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
	pack	Formula from "=sum(",ColumnA,"10:",ColumnA,"21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"

          pack      Exrange1 from ColumnB,DimRow1
	pack	Formula from "=sum(",ColumnB,"10:",ColumnB,"21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"

          pack      Exrange1 from ColumnC,DimRow1
	pack	formula from "=",ColumnB,dimrow1,"/C",dimrow1
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="0.0%"

          pack      ExRange1,columnA,"8"
          pack      ExRange2,columnC,"23"
          sheet.range(ExRange1,ExRange2).BorderAround using *LineStyle=1,*Weight=2

	return
.......................................................................................
STOP
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
	Append    "C:\WORK\InvoicesBySales",taskname    
	if        (#ver = c1)
	append  ".xlsx",taskname
	else
	append  ".xls",taskname
	endif
	Reset     Taskname                              
	erase          taskname
	book.saveas giving N9 using *Filename=taskname
	trapclr Object
.
	setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
	destroy        OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
	destroy sheet
	destroy sheets
	destroy book
	destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
	setprop ex,*DisplayAlerts=OFALSE
	destroy        OFALSE
	ex.quit
	destroy ex
.Email new XLS to User



          pause     "10"

.
          Move      "Here is your Invoice History by Salesperson",MailSubjct
          Move      "Creques@nincal.com",MailFrom
.          Move      "DavidHerrick@nincal.com,SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailTo
          Move      "DavidHerrick@nincal.com",MailTo
          Move      taskname,MailBody
          MOve      taskname,MailAttach
          call      SendMail
          winshow
         pause     "30"

          shutdown
         STOP
         STOP
......................................................................................................
ERR      DISPLAY   *P1:1,*ES,"RANGE ERROR :",ERROR;
         KEYIN     *P22:1,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      STOP IF EQUAL
         GOTO      ERR
NOfile
         return
.
IO       DISPLAY   *P1:24,*EL,"IO ERROR ",ERROR;
         STOP

BigDate
	load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
		m07,m08,m09,m010,m011,m012
	return

.
	include  ncntIO.inc
         	INCLUDE   COMLOGIC.inc


PC EQU 0
	INCLUDE	common.inc
	INCLUDE	cons.inc

MONTHINCREPORT	PFILE	
PRTFILENAME	DIM	50
PRINTNAME	DIM	50

Arial8    font
        create  Arial8,"Arial",size=8
Arial9    font
        create  Arial9,"Arial",size=9
Arial10    font
        create  Arial10,"Arial",size=10
Arial12    font
        create  Arial12,"Arial",size=12
Arial16    font
        create  Arial16,"Arial",size=16
Header1	form	9
Header2	form	9
Header3	form	9
Header4	form	9
Header5	form	9
Header6	form	9
Header7	form	9
Header9	form	9
Header10	form	9
Header11	form	9
Header12	form	9
Header13	form	9
Header14	form	9
Header15	form	9
Header16	form	9
Header17	form	9
Header18	form	9
Release	INIT	"1.0"	Initial Release	of BO Monthy income report for clients
SingleSpaced	FORM	"160"
OneandahalfSpaced	FORM	"240"
DoubleSpaced	FORM	"320"
LgBoxHeight	FORM	"7491"
SmBoxHeight	FORM	"490"
HalfsmboxHeight FORM	"245"
MonthTextSmall 	FORM	"480"
BegRowLine	FORM	9
BoxOneLeft	FORM	"0"
BoxOneRight	FORM	"600"
BoxTwoLeft	FORM	"650"
BoxTwoRight	FORM	"2750"
BoxTwoLeft1	FORM	"1700"
BoxTwoRight1	FORM	"2750"
BoxThreeLeft	FORM	"2800"
BoxThreeRight	FORM	"6400"
BoxFourLeft	FORM	"6600"
BoxFourRight	FORM	"7200"
BoxFiveLeft	FORM	"7250"
BoxFiveRight	FORM	"9500"
BoxFiveLeft1	FORM	"8375"
BoxFiveRight1	FORM	"9500"
BoxSixLeft	FORM	"9550"
BoxSixRight	FORM	"10500"
;List 
BoxTwoVert	FORM	"1700"
BoxThreeVert1	FORM	"4000"
BoxThreeVert2	FORM	"5200"
BoxFiveVert	FORM	"8375"
;Report Description
	move "5250" to Header1
;Description of Fiscal Year 1
	move	"2400" to Header2
;Description of Fiscal Year 2
	move	"8000" to Header3
;Description of Fiscal Year 3
	move	"1600" to Header4
;Description of Fiscal Year 4
	move	"3500" to Header5
;Description of Fiscal Year 1
	move	"5500" to Header6
;Description of Fiscal Year 2
	move	"8000" to Header7
;Cell Header equations
;Header9
	sub BoxTwoleft,BoxTworight,n9
	div c4 in n9,n8
	add BoxTwoLeft,n8,Header9
;Header10
	sub BoxTwoleft,BoxTworight,n9
	div c4 in n9,n8
	mult c3,n8
	add BoxTwoLeft,n8,Header10
;Header11
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	add BoxThreeLeft,n8,Header11
;Header12
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c3,n8
	add BoxThreeLeft,n8,Header12
;Header13
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c5,n8
	add BoxThreeLeft,n8,Header13
;Header14
	sub BoxFiveleft,BoxFiveright,n9
	div c4 in n9,n8
	add BoxFiveLeft,n8,Header14
;Header15
	sub BoxFiveleft,BoxFiveright,n9
	div c4 in n9,n8
	mult c3,n8
	add BoxFiveLeft,n8,Header15
;Header16
	sub BoxSixleft,BoxSixright,n9
	div c2 in n9,n8
	add BoxSixLeft,n8,Header16


	move "db.lst",PRINTNAME
	move	"c:\work\db.lst" to prtfilename
IncHeader
	PRTOPEN MONTHINCREPORT,"@\\nts0\Laser6",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
	PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
;	prtpage MONTHINCREPORT;*pColumn:row,*ALIGNMENT=*Left,*font=,"Confidential";
;	prtpage MONTHINCREPORT;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
;	clock   timestamp,str8
;	unpack  str8,str2,yy,mm,dd
;	clear   str10
;	pack    str10,mm,slash,dd,slash,str2,yy
;	prtpage MONTHINCREPORT;*font=font8,str10;
	prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial16,*ll,*boldon,"Special Olympics International",*boldoff;
	add	OneandahalfSpaced to row
	prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A CASH BASIS (BY CHECK DATE)";
	add	Doublespaced to row
	prtpage MONTHINCREPORT;*pHeader2:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"FISCAL YEAR 2003 (01/01/03 - 12/31/03)",*boldoff;
	prtpage MONTHINCREPORT;*pHeader3:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"FISCAL YEAR 2002 (01/01/02 - 12/31/02)",*boldoff;
	add	Doublespaced to row
;Header4
	prtpage MONTHINCREPORT;*pBoxTwoVert:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader5:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Income",*boldoff;
	prtpage MONTHINCREPORT;*pHeader6:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Income",*boldoff;
;Header7
	prtpage MONTHINCREPORT;*pBoxFiveVert:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Volume",*boldoff;
ExcelGrid
	
	add	OneandaHalfSpaced to row
	move	row to BegRowLine
;Volume 1
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxThreeLeft:BoxThreeRight
;Volume2
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxSixLeft:BoxSixRight
;Rent exchange halfboxes
		add HalfsmboxHeight,row
		prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=10,*overlayon,*line=BoxTwoRight1:row;
		prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=10,*overlayon,*line=BoxFiveRight1:row;
		sub HalfsmboxHeight,row
;Title Cell
	move row to n9
;Topmost
	prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised",*boldoff;
;Top
	prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Total",*boldoff;
	prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch. Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Total",*boldoff;
	prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch Volume",*boldoff;
;Middle Centered
	add singlespaced to row
	prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Projections",*boldoff;
	prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
	prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Actual",*boldoff;
;Second Line
	add singlespaced to row
	prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,"Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised as of 8/13/03",*boldoff;

	move n9 to row



;Month Boxes
	add	SmBoxHeight to row
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxOneLeft:BoxOneRight
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxFourLeft:BoxFourRight
;Months Inserted
	Clear N2
	move row to n9
	add	halfsmboxheight to row
	add	"40" to row
	loop	

		add	c1 to n2
		load	str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
		prtpage MONTHINCREPORT;*p300:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,str3;
		prtpage MONTHINCREPORT;*p6900:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,*boldon,str3;
		add	SmBoxHeight to row
	until 	(n2 = "12")
	repeat
	move n9 to row
;Horizontal Lines
	Clear N2
	loop	
	        prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=10,*line=BoxOneRight:row;
	        prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=10,*line=BoxTwoRight:row;
        	prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=10,*line=BoxThreeRight:row;
	        prtpage MONTHINCREPORT;*pBoxFourLeft:row,*pensize=10,*line=BoxFourRight:row;
	        prtpage MONTHINCREPORT;*pBoxFiveLeft:row,*pensize=10,*line=BoxFiveRight:row;
        	prtpage MONTHINCREPORT;*pBoxSixLeft:row,*pensize=10,*line=BoxSixRight:row;
		add	c1 to n2
		add	HalfsmboxHeight,row
	        prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=10,*line=BoxTwoRight1:row;
	        prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=10,*overlayon,*line=BoxFiveRight1:row;
		sub	HalfsmboxHeight,row
		add	SmBoxHeight to row
	until 	(n2 = "12")
	repeat
;Vertical Lines
        	prtpage MONTHINCREPORT;*pBoxTwoVert:BegRowLine,*pensize=10,*line=BoxTwoVert:LgBoxHeight;
        	prtpage MONTHINCREPORT;*pBoxThreeVert1:BegRowLine,*pensize=10,*line=BoxThreeVert1:LgBoxHeight;
        	prtpage MONTHINCREPORT;*pBoxThreeVert2:BegRowLine,*pensize=10,*line=BoxThreeVert2:LgBoxHeight;
        	prtpage MONTHINCREPORT;*pBoxFiveVert:BegRowLine,*pensize=10,*line=BoxFiveVert:LgBoxHeight;
;Fill Months in






	prtclose	MONTHINCREPORT
;	prtplay		PRTFILENAME,"\\NTS0\Laser6"
	
	stop
IncFooter

	INCLUDE	COMLOGIC.INC
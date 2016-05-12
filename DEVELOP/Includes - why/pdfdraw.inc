. pdf drawing routines.
. 
PDFPenSize FUNCTION
pensize	form	2.3
	ENTRY
content	dim	9
	MOVE	pensize,CurrentPen
	PACK	content,pensize," w",CRLF
	APPEND	content,page(curpage).content
	
	FUNCTIONEND
.
PDFRect	FUNCTION
top	form 5.3
bottom  form 5.3
left    form 5.3
right   form 5.3
fill	form	1
	ENTRY

y1	form	3.3
height	form	3.3
width	form	3.3

content	dim	50
.
	 CALL	CVTVUnit giving top using top
	 CALL	CVTVUnit giving bottom using bottom
	 CALL	CVTHUnit giving left using left
	 CALL	CVTHUnit giving right using right
.
	ADD	(MarginT/2),top
	Add	(MarginT/2),bottom
	Add	MarginL,Left
	Add	MarginL,right
.
	MOVE	(Page(curpage).Height-bottom),y1
	MOVE	(bottom-top),height
	MOVE	(right-left),width
	PACK	content,left," ",y1," ",width," ",height," re",CRLF
	ENDSET	content
.
	IF ( fill )
	APPEND	"B",content
	ELSE
	APPEND	"S",content
	ENDIF
	APPEND	CRLF,content
.
	RESET	content
	APPEND	content,page(curpage).content
.
	FUNCTIONEND
.
. PDF doesn't have a forground and background color concept.  
.    It is outline(pen) and fill.  
.    
.    Text uses fill color but lines, rect, etc use pen.  
.    So which is foreground?
.
PDFColor FUNCTION
Red	FORM 3
Green   FORM 3
Blue    FORM 3
	Entry
PDFRed	form	3.3
PDFGreen form	3.3
PDFBlue	form	3.3
content	dim	100
	DIVIDE	"255",Red,PDFRed
	DIVIDE	"255",Green,PDFGreen
	DIVIDE	"255",Blue,PDFBlue

	PACK	content,PDFRed," ",PDFGreen," ",PDFBlue," RG",CRLF: //pen color
	                PDFRed," ",PDFGreen," ",PDFBlue," rg",CRLF  // fill color
	APPEND	content,page(curpage).content
	FUNCTIONEND
.
PDFABSPos FUNCTION
left	form	5.3
top	form	5.3
	ENTRY
.
	 CALL	CVTVUnit giving top using top
	 CALL	CVTHUnit giving left using left
	CALL	ABSPos using Left,Top
	FUNCTIONEND

ABSPos	LFUNCTION
left	form	5.3
top	form	5.3
	entry
	MOVE	(Page(curpage).Height-top),PagePos.y	//page height - top
	MOVE	left,PagePos.x
.
	FUNCTIONEND
.
PDFHPos FUNCTION
horz	form	5.3
	ENTRY

	CALL	CVTHUnit giving horz using horz
	MOVE	(MarginL+horz),pagepos.x
	FUNCTIONEND

PDFHAdj FUNCTION
horz	FORM	5.3
	ENTRY

	CALL	CVTHUnit giving horz using horz
	SUB	horz,pagepos.x

	FUNCTIONEND
PDFVPos FUNCTION
vert	form	5.3
	ENTRY
	CALL	CVTVUnit giving vert using vert
	MOVE	(page(curpage).height-vert-MarginT),pagepos.y
	FUNCTIONEND
.
PDFVAdj FUNCTION
vert	form	5.3
	ENTRY
Ly form	5.3
	CALL	CVTVUnit giving Ly using vert
	SUB	Ly,pagepos.y

	FUNCTIONEND

PDFLineTo	Function
x	form	5.3
y	form	5.3
	ENTRY
.
	CALL	CVTVUnit giving y using y
	CALL	CVTHUnit giving x using x
	CALL	LineTo using x,y
	FUNCTIONEND

LineTo LFUNCTION
x	form	5.3
y	form	5.3
	ENTRY
ToY	form	4.3
content	dim	50

	MOVE	(Page(curpage).Height-y),ToY
	PACK	content,PagePos.x," ",PagePos.y," m",crlf,x," ",Toy," l S",CRLF
	APPEND	content,page(curpage).content
.
	FUNCTIONEND	
.
PDFOVAL FUNCTION
top	form	5.3
bottom  form	5.3
left    form	5.3
right   form	5.3
	ENTRY
content	dim	100
.
. to draw a circle... using Cubic Bezier curves
.
x	form	5.3
y	form	5.3
x1	form	5.3
y1	form	5.3
x2	form	5.3
y2	form	5.3
x3	form	5.3
y3	form	5.3

height	form	5.5
width	form	5.3
bt	form	5.3
.
	 CALL	CVTVUnit giving top using top
	 CALL	CVTVUnit giving bottom using bottom
	 CALL	CVTHUnit giving left using left
	 CALL	CVTHUnit giving right using right
.
	CALC	height=bottom-top
	CALC	width=right-left
.start/finish point
	MOVE	left,x
	CALC	y=(Page(curpage).Height-top)-(height/2)  //(top + 1/2 height
	MOVE	(Page(curpage).Height-bottom),bt
.
	MOVE	left,x1
	CALC	y1=y+((height/2)*0.55)

	CALC	x2=left+((width/2)*.45)
	CALC	y2=(Page(curpage).Height-top)
	MOVE	(Page(curpage).Height-top),y3
	CALC	x3=(width/2)+left // left + 1/2 width

	// first corner
	PACK	content,x," ",y," m",CRLF:
			x1," ",y1," ",x2," ",y2," ",x3," ",y3," c",CRLF
	APPEND	content,page(curpage).content

	CALC	x1=x3+((width/2)*.55)
	PACK	content,x1," ",y3," ",right," ",y1," ",right," ",y," c",CRLF
	APPEND	content,page(curpage).content

	CALC	y3=y-((height/2)*0.55)
	CALC	x1=right-((width/2)*.45)
	PACK	content,right," ",y3," ",x1," ",bt," ",x3," ",bt," c",CRLF
	APPEND	content,page(curpage).content

	CALC	x1=left+((width/2)*.45)
	PACK	content,x1," ",bt," ",left," ",y3," ",x," ",y," c S",CRLF
	APPEND	content,page(curpage).content

	FUNCTIONEND
.	
PDFRNDRECT	FUNCTION
uTop	form 5.3
uBottom  form 5.3
Left    form 5.3
Right   form 5.3
Height  form 5.3
Width   form 5.3
	ENTRY
content	dim	200

top	form	5.3
bottom form	5.3
start	form	5.3
stop	form	5.3
.
	 CALL	CVTVUnit giving utop using utop
	 CALL	CVTVUnit giving ubottom using ubottom
	 CALL	CVTHUnit giving left using left
	 CALL	CVTHUnit giving right using right
	 CALL	CVTVUnit giving height using height
	 CALL	CVTHUnit giving width using width
.
	SUB	MarginT,utop
	SUB	MarginT,ubottom

	ADD	MarginL,Left
	ADD	MarginL,Right
	CALC	top=Page(curpage).Height-uTop
	CALC	bottom=Page(curpage).Height-uBottom
.
. start position and top line
	CALC	start=left+width
	CALC	stop=right-width
	PACK 	content,start," ",top," m",crlf:
		        stop," ",top," l",crlf
	APPEND content,page(curpage).content
. top right corner
 	CALC	stop=top-height
	PACK	content,right," ",top," ",right," ",top," ",right," ",stop," c",crlf
	APPEND content,page(curpage).content
. right side
	CALC	stop=bottom+height
	PACK	content,right," ",stop," l",crlf
	APPEND content,page(curpage).content
. bottom right
	CALC	stop=right-width
	PACK	content,right," ",bottom," ",right," ",bottom," ",stop," ",bottom," c",crlf
	APPEND content,page(curpage).content
. bottom side
	PACK	content,start," ",bottom," l",crlf
	APPEND content,page(curpage).content
. bottom left
	CALC	stop=bottom+height
	PACK	content,left," ",bottom," ",left," ",bottom," ",left," ",stop," c",crlf
	APPEND content,page(curpage).content
.left
	CALC	stop=top-height
	PACK	content,left," ",stop," l",crlf
	APPEND content,page(curpage).content
. top left
	PACK	content,left," ",top," ",left," ",top," ",start," ",top," c S",CRLF
	APPEND content,page(curpage).content

	FUNCTIONEND

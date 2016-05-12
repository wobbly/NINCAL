.  get bar code to work.
.  

	include	pdfext.pls


color	color
intblue	integer	4,"0x00007f7f"
	create	color=*BLUE


.	prtopen ...
	CALL	PDFOpen	using "testing.pdf"

	pdfprint	fgcolor=intblue,units=UnitLOENG:
			v=100,h=100,margint=100,marginl=100:
			font="Times(12)",text="Hello World",n:
			font="curior(11)",text="goodbye world"

	CALL	PDFTTFont using "FreeBar39.ttf","12"
	pdfprint	va=50,text="*012345*",n

.	
	CALL	PDFTTFont using "c:\windows\fonts\timesbi.ttf","12"
	pdfprint	text="*012345*",n
.
.
	CALL	PDFClose
.	
	EXECUTE	"testing.pdf"
	

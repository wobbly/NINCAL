FILEMENU	MENU
.
.	"Program written by"
.	"Oystein Stokkeland"
.	"Datapoint, Sandnes, Norway"
.	"email: Oystein.Stokkeland@datapoint.no"
.	"21mar1997"
.
.
FILEITEM	INIT	")File;E)xit"
Info		MENU
InfoData	INIT	"&Info;&Fonts"
.helpMenu	 menu
.helpItem	 init	 ")Help;)About"

res	form	10
DATA	dim	610
w1linje dim	1024

 Font Info
fontObj 	FONT
.
fntAscent	DIM	4
fntDescent	DIM	4
fntHeight	DIM	4
fntFullHeight	DIM	4
fntAvgChrWidth	DIM	4
fntMaxChrWidth	DIM	4
fntStringWidth	DIM	4
fntFirstChar	DIM	1
fntLastChar	DIM	1

liste1	plform	ospfont
.helpabout plform ospabout

	winhide
	formload	liste1
	create		ospfont;FILEMENU,FILEITEM
	activate	FILEMENU,fileRutine,res
	create		ospfont;info,infodata,FILEMENU
	activate	info,infoRutine,res

.	 create 	 ospfont;helpMenu,helpItem,info
.	 activate	 helpMenu,helpRutine,res

	deActivate	fontListe

	LOOP
	  waitevent
	REPEAT

inforutine
	IF	(res = 1)
	  deleteItem fontListe,0
	  activate   fontListe
	  getInfo   FONTS,fontListe
	  getInfo    FONTS,DATA
	  setItem    lblDefaultFont,0,DATA
	  setItem    fontListe,0,1

	  move	"1" to res
	  call	showInfo
	ENDIF
	return

showInfo
	getItem fontListe,0,res
	getItem fontListe,res,w1linje
	create	fontObj,w1linje,SIZE=12
	move	"ABCDEFGHIJKLMNOPQRSTUVWXYZÆØÅabcdefghijklmnopqrstuvwxyzæøå" to DATA
	getInfo TYPE=fontObj,DATA
	UNPACK	DATA INTO fntAscent, fntDescent, fntHeight:
				fntFullHeight, fntAvgChrWidth, fntMaxChrWidth:
				fntStringWidth, fntFirstChar, fntLastChar

	setItem dispFontName,0,w1linje
	setItem dispAscent,0,fntAscent
	setItem dispDescent,0,fntDescent
	setItem dispHeight,0,fntHeight
	setItem dispFullHeight,0,fntHeight
	setItem dispAvgChrWidth,0,fntAvgChrWidth
	setItem dispMaxChrWidth,0,fntMaxChrWidth
	setItem dispLengthOfString,0,fntStringWidth
	setItem dispFirstChar,0,fntFirstChar
	setItem dispLastChar,0,fntLastChar

	setProp fontSample,FONT=fontObj

	return

.helpRutine
....This never got completed!  sri.
....	formload	helpAbout
.	 return

fileRutine
	winshow
	stop
.
.

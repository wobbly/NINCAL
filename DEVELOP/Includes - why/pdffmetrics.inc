. basic 14 font metrics data
. 
pdffonts	dim	21(14),("Times-Roman"),("Times-Bold"),("Times-Italic"),("Times-BoldItalic"):
				("Helvetica"),("Helvetica-Bold"),("Helvetica-Oblique"),("Helvetica-BoldOblique"):
				("Courier"),("Courier-Bold"),("Courier-Oblique"),("Courier-BoldOblique"):
				("Symbol"),("ZapfDingbats")
fontIdx	form	2
.
pdffontres	form	4(14)
pdffontobj	form	4(14)
.
fonts	DIM	^2000	// just enough room for all 14 fonts...
CurrentFont	dim	14	// page content tag
CurFontRes	dim	20	// page dictionary resource
fontsize form	2
fontnum	form	2
.
FontMetrics	RECORD
ulPos	form	4.3 // underline pos in char metric
ulpen	form	3.3 // underline pen size
Descender	form	4.3
GlyphWidths	form	5.3(256)
		recordend
.
TimesMetric	INIT	"-100,50,-217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"250,333,408,500,500,833,778,333,333,333,500,564,250,333,250,278,500,500,500,":
			"500,500,500,500,500,500,500,278,278,564,564,564,444,921,722,667,667,722,611,":
			"556,722,722,333,389,722,611,889,722,722,556,722,667,556,611,722,722,944,722,":
			"722,611,333,278,333,469,500,333,444,500,444,500,444,333,500,500,278,278,500,":
			"278,778,500,500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,541,":
			"333,500,500,167,500,500,500,500,180,444,500,333,333,556,556,500,500,500,250,":
			"453,350,333,444,444,500,1000,1000,444,333,333,333,333,333,333,333,333,333,":
			"333,333,333,333,1000,889,276,611,722,889,310,667,278,278,500,722,500,0,0,0,0"
TimesBMetric	init	"-100,50,-217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"250,333,555,500,500,1000,833,333,333,333,500,570,250,333,250,278,500,500,500,":
			"500,500,500,500,500,500,500,333,333,570,570,570,500,930,722,667,722,722,667,":
			"611,778,778,389,500,778,667,944,722,778,611,778,722,556,667,722,722,1000,722,":
			"722,667,333,278,333,581,500,333,500,556,444,556,444,333,500,556,278,333,556,":
			"278,833,556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,520,":
			"333,500,500,167,500,500,500,500,278,500,500,333,333,556,556,500,500,500,250,":
			"540,350,333,500,500,500,1000,1000,500,333,333,333,333,333,333,333,333,333,333,":
			"333,333,333,1000,1000,300,667,778,1000,330,722,278,278,500,722,556,0,0,0,0"
TimesIMetric	INIT	"-100,50,-217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"250,333,420,500,500,833,778,333,333,333,500,675,250,333,250,278,500,500,500,":
			"500,500,500,500,500,500,500,333,333,675,675,675,500,920,611,611,667,722,611,":
			"611,722,722,333,444,667,556,833,667,722,611,722,611,500,556,722,611,833,611,":
			"556,556,389,278,389,422,500,333,500,500,444,500,444,278,500,500,278,278,444,":
			"278,722,500,500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,541,":
			"389,500,500,167,500,500,500,500,214,556,500,333,333,500,500,500,500,500,250,":
			"523,350,333,556,556,500,889,1000,500,333,333,333,333,333,333,333,333,333,333,":
			"333,333,333,889,889,276,556,722,944,310,667,278,278,500,667,500,0,0,0,0"
TimesBIMetric	init	"-100,50,-217,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"250,389,555,500,500,833,778,333,333,333,500,570,250,333,250,278,500,500,500,":
			"500,500,500,500,500,500,500,333,333,570,570,570,500,832,667,667,667,722,667,":
			"667,722,778,389,500,667,611,889,722,722,611,722,667,556,611,722,667,889,667,":
			"611,611,333,278,333,570,500,333,500,500,444,500,444,333,500,556,278,278,500,":
			"278,778,556,500,500,500,389,389,278,556,444,667,500,444,389,348,220,348,570,":
			"389,500,500,167,500,500,500,500,278,500,500,333,333,556,556,500,500,500,250,":
			"500,350,333,500,500,500,1000,1000,500,333,333,333,333,333,333,333,333,333,":
			"333,333,333,333,1000,944,266,611,722,944,300,722,278,278,500,722,500,0,0,0,0"
HelveticaMetric init	"-100,50,-207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"278,278,355,556,556,889,667,222,333,333,389,584,278,333,278,278,556,556,556,":
			"556,556,556,556,556,556,556,278,278,584,584,584,556,1015,667,667,722,722,":
			"667,611,778,722,278,500,667,556,833,722,778,667,778,722,667,611,722,667,944,":
			"667,667,611,278,278,278,469,556,222,556,556,500,556,556,278,556,556,222,222,":
			"500,222,833,556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,":
			"584,333,556,556,167,556,556,556,556,191,333,556,333,333,500,500,556,556,556,":
			"278,537,350,222,333,333,556,1000,1000,611,333,333,333,333,333,333,333,333,333,":
			"333,333,333,333,1000,1000,370,556,778,1000,365,889,278,222,611,944,611,0,0,0,0"
HelveticaBMetric init	"-100,50,-207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"278,333,474,556,556,889,722,278,333,333,389,584,278,333,278,278,556,556,556,":
			"556,556,556,556,556,556,556,333,333,584,584,584,611,975,722,722,722,722,667,":
			"611,778,722,278,556,722,611,833,722,778,667,778,722,667,611,722,667,944,667,":
			"667,611,333,278,333,584,556,278,556,611,556,611,556,333,611,611,278,278,556,":
			"278,889,611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,":
			"333,556,556,167,556,556,556,556,238,500,556,333,333,611,611,556,556,556,278,":
			"556,350,278,500,500,556,1000,1000,611,333,333,333,333,333,333,333,333,333,":
			"333,333,333,333,1000,1000,370,611,778,1000,365,889,278,278,611,944,611,0,0,0,0"
HelveticaIMetric init	"-100,50,-207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"278,278,355,556,556,889,667,222,333,333,389,584,278,333,278,278,556,556,556,":
			"556,556,556,556,556,556,556,278,278,584,584,584,556,1015,667,667,722,722,667,":
			"611,778,722,278,500,667,556,833,722,778,667,778,722,667,611,722,667,944,667,":
			"667,611,278,278,278,469,556,222,556,556,500,556,556,278,556,556,222,222,500,":
			"222,833,556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,584,":
			"333,556,556,167,556,556,556,556,191,333,556,333,333,500,500,556,556,556,278,":
			"537,350,222,333,333,556,1000,1000,611,333,333,333,333,333,333,333,333,333,333,":
			"333,333,333,1000,1000,370,556,778,1000,365,889,278,222,611,944,611,0,0,0,0"
HelveticaBIMetric init	"-100,50,-207,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"278,333,474,556,556,889,722,278,333,333,389,584,278,333,278,278,556,556,556,":
			"556,556,556,556,556,556,556,333,333,584,584,584,611,975,722,722,722,722,667,":
			"611,778,722,278,556,722,611,833,722,778,667,778,722,667,611,722,667,944,667,":
			"667,611,333,278,333,584,556,278,556,611,556,611,556,333,611,611,278,278,556,":
			"278,889,611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,":
			"333,556,556,167,556,556,556,556,238,500,556,333,333,611,611,556,556,556,278,":
			"556,350,278,500,500,556,1000,1000,611,333,333,333,333,333,333,333,333,333,":
			"333,333,333,333,1000,1000,370,611,778,1000,365,889,278,278,611,944,611,0,0,0,0"
CourierMetric	init	"-100,50,-157,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,":
			"600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,0,0,0,0"
SymbolMetric INIT 	"-100,50,-293,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"250,333,713,500,549,833,778,439,333,333,500,549,250,549,250,278,500,500,500,":
			"500,500,500,500,500,500,500,278,278,549,549,549,444,549,722,667,722,612,611,":
			"763,603,722,333,631,722,686,889,722,722,768,741,556,592,611,690,439,768,645,":
			"795,611,333,863,333,658,500,500,631,549,549,494,439,521,411,603,329,603,549,":
			"549,576,521,549,549,521,549,603,439,576,713,686,493,686,494,480,200,480,549,":
			"750,620,247,549,167,713,500,753,753,753,753,1042,987,603,987,603,400,549,411,":
			"549,549,713,494,460,549,549,549,549,1000,603,1000,658,823,686,795,987,768,768,":
			"823,768,768,713,713,713,713,713,713,713,768,713,790,790,890,823,549,250,713,":
			"603,603,1042,987,603,987,603,494,329,790,790,786,713,384,384,384,384,384,384,":
			"494,494,494,494,329,274,686,686,686,384,384,384,384,384,384,494,494,494,0"
ZapfDingbatsMetric init	"-100,50,-143,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,":
			"278,974,961,974,980,719,789,790,791,690,960,939,549,855,911,933,911,945,974,":
			"755,846,762,761,571,677,763,760,759,754,494,552,537,577,692,786,788,788,790,":
			"793,794,816,823,789,841,823,833,816,831,923,744,723,749,790,792,695,776,768,":
			"792,759,707,708,682,701,826,815,789,789,707,687,696,689,786,787,713,791,785,":
			"791,873,761,762,762,759,759,892,892,788,784,438,138,277,415,392,392,668,668,":
			"390,390,317,317,276,276,509,509,410,410,234,234,334,334,732,544,544,910,667,":
			"760,760,776,595,694,626,788,788,788,788,788,788,788,788,788,788,788,788,788,":
			"788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,":
			"788,788,788,788,788,788,788,788,894,838,1016,458,748,924,748,918,927,928,928,":
			"834,873,828,924,924,917,930,931,463,883,836,836,867,867,696,696,874,874,760,":
			"946,771,865,771,888,967,888,831,873,927,970,918,0"
.
MetricsA	dim	^(14),(TimesMetric),(TimesBMetric),(TimesIMetric),(TimesBIMetric):
			(HelveticaMetric),(HelveticaBMetric),(HelveticaIMetric),(HelveticaBIMetric):
			(CourierMetric),(CourierMetric),(CourierMetric),(CourierMetric): // fixed font just re-use the same array
			(SymbolMetric),(ZapfDingbatsMetric)
.
.
LastFntIdx	form	2
.
PDFFont	FUNCTION
FontName	dim	80
	ENTRY
PDFFontName	dim	80
char2	dim	2
attributes record
attrib1	dim	2
attrib2	dim	2
attrib3	dim	2
	recordend
pdffontoffset	form	1
content	DIM	150
.
	CLEAR	fontsize,fontidx,usingttf
	EXPLODE	FontName,"(",PDFFontName	// to-do... check for valid font
.
	IF not zero
	 EXPLODE	FontName,",)",attributes
	 TYPE		attributes.attrib1
	 IF EQUAL
	  MOVE		attributes.attrib1,fontsize
	 ENDIF
	 TYPE		attributes.attrib2
	 IF EQUAL
	  MOVE		attributes.attrib2,fontsize
	 ENDIF
	 TYPE		attributes.attrib3
	 IF EQUAL
	  MOVE		attributes.attrib3,fontsize
	 ENDIF
	 IF (attributes.attrib1 = "B" or attributes.attrib2 = "B" or attributes.attrib3 = "B")
	  ADD		"1",pdffontoffset // italic = pdffont array +1
	 ENDIF
	 IF (attributes.attrib1 = "I" or attributes.attrib2 = "I" or attributes.attrib3 = "I")
	  ADD		"2",pdffontoffset // italic = pdffont array +2
	 ENDIF
	ENDIF
.
	IF (Fontsize=0)
	 MOVE	"12",fontsize	// default
	ENDIF
.
	SEARCH	PDFFontName,pdffonts(1),"14",fontIdx
	IF	(fontIdx = 0)
	 MOVE	"9",fontIdx	//Courier(12) is default
	 CLEAR	FontName
	ENDIF	 
.
	IF 	(FontIdx != 1 and FontIdx != 5 and FontIdx != 9)
	 CLEAR	pdffontoffset	// bold and italic only apply to 1 5 or 9
	ELSE
	 ADD	pdffontoffset,fontIdx
	ENDIF
.
	IF (pdffontres(fontidx)=0)
.
	 EXPLODE	MetricsA(fontIdx),",",FontMetrics
.
	 INCR		ObjNum
	 INCR		fontnum
	 MOVE		fontnum,pdffontres(fontidx)
	 MOVE		ObjNum,pdffontobj(fontidx)
.        
	 SQUEEZE	fontnum,char2
	 SQUEEZE	objNum,scratch
	 PACK		content,scratch," 0 obj",CRLF:
	                        " << /Type /Font",CRLF:
                                "    /Subtype /Type1",CRLF:
                                "    /Name /F",char2,CRLF:
                                "    /BaseFont /",pdffonts(fontIdx),CRLF:
                                " >>",CRLF:
                                "endobj",CRLF,CRLF
	 APPEND		content,fonts
	ELSE
	 SQUEEZE	pdfFontRes(fontidx),char2
	 IF 	(fontidx != LastFntIdx)
	  EXPLODE	MetricsA(fontIdx),",",FontMetrics
	 ENDIF
	 MOVE		fontidx,LastFntIdx
	ENDIF
.
	PACK	CurrentFont,"/F",char2," ",fontsize," Tf"
.
	RESET	page(curpage).fontres
	SQUEEZE	pdffontobj(fontidx),scratch
	PACK	CurFontRes,"/F",char2," ",scratch," 0 R "
	SCAN	CurFontRes,page(curpage).fontres
	IF NOT EQUAL
	 ENDSET page(curpage).fontres
	 APPEND	CurFontRes,page(curpage).fontres
	ENDIF
.
	FUNCTIONEND
.
PDFBoldOn FUNCTION
          entry
boldfont	dim	30
.
	SELECT	from fontIdx // check if already bold
	WHEN "2" or "4" or "6" or "8" or "10" or "12"
	 RETURN
	ENDSELECT

	INCR	fontIdx
	PACK	boldfont,pdffonts(fontIdx),"(",fontsize,")"
	call	PDFFont using boldfont

          FUNCTIONEND

PDFBoldOff FUNCTION
          entry
boldfont	dim	30
.
	SELECT	from fontIdx // check if bold already off
	WHEN "1" or "3" or "5" or "7" or "9" or "11"
	 RETURN
	ENDSELECT

	DECR	fontIdx
	PACK	boldfont,pdffonts(fontIdx),"(",fontsize,")"
	call	PDFFont using boldfont

          FUNCTIONEND
.
PDFUnderLineOn FUNCTION
               entry
	SET	ul
               FUNCTIONEND
.
PDFUnderLineOff FUNCTION
               entry
	CLEAR	ul
               FUNCTIONEND
...........................................
.Text Metrics ... Get character widths
.     metrics in AFM files ( and pdf font widths array ) are in glyph space
.     glyph space is 1/1000th text space
.     using default settings, text space is 72DPI
.     
. I have not implamented anything to change default units
.
. character output width = glyph width * font size / 1000
.
LoadFontMetricsFF LFUNCTION	//From File
FontName	DIM	40
	ENTRY
afmname	dim	250
afmfile	file
data	dim	100

CharMetrics record
char	dim	7
width	dim	12
name	dim	40
box	dim 	25
	recordend

startcharmetrics	form	1

unit	dim	2
chvalue	form	3
wdtype	dim	4
wdvalue	form	5
.
	CLEAR		FontMetrics.GlyphWidths	// discard old data
	PACK		afmname,FontName,".afm"
	OPEN		afmfile,afmname,read
	LOOP
	 READ		afmfile,seq;data
	 UNTIL OVER
.
	 MATCH		"UnderlinePosition ",data
	 IF EQUAL
	  BUMP		data,18
	  CHOP		data
	  MOVE		data,FontMetrics.ulpos
	 ENDIF
.
	 MATCH		"UnderlineThickness ",data
	 IF EQUAL
	  BUMP		data,19
	  CHOP		data
	  MOVE		data,FontMetrics.ulpen
	 ENDIF
.
	 MATCH		"Descender ",data // how far below baseline does the 
	 IF EQUAL			  // font go?  needed for underline pos
	  BUMP		data,10
	  chop		data
	  move		data,FontMetrics.Descender
	 ENDIF
.
	 MATCH		"StartCharMetrics ",data
	 IF EQUAL
	  SET		startcharmetrics
	  CONTINUE
	 ENDIF
	 CONTINUE IF (startcharmetrics=0)
. glyph value
	 EXPLODE	data,";",CharMetrics
	 PARSE		CharMetrics.char,unit,"AZ" // C = decimal CH = hex
	 PARSENUM	CharMetrics.char,chvalue // character value ( parsenum handles hex :D )
.
. not supporting named characters at this time
.
	 BREAK	if (chvalue = seq)  // -1 is named character instead of value
. char width
	 PARSE		CharMetrics.width,wdtype,"AZ" // width type  ( WX is what we want)
	 PARSENUM	CharMetrics.width,wdvalue // width value
.
	 MATCH		"WX ",wdtype
	 IF NOT EQUAL
	  MATCH		"W1X ",wdtype
	 ENDIF
	 IF EQUAL
	  MOVE		wdvalue,FontMetrics.GlyphWidths(chvalue)
	 ENDIF
.	 
	REPEAT
	CLOSE	afmfile
.
	FUNCTIONEND
.
PDFMeasureString FUNCTION
string	dim	^
	entry
char	dim	1
cval	integer	1
strlen	form	5.3
fptr	form	10

	MOVEFPTR	string,fptr
	LOOP
	 REMOVE	string,char
	 UNTIL EOS
	 MOVE	char,cval
	 ADD	(FontMetrics.GlyphWidths(cval)*fontsize/1000),strlen
	REPEAT
	RESET	string,fptr

	FUNCTIONEND USING strlen

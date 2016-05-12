PDFOPEN  EXTERN "writepdf;PDFOpen"
PDFCLOSE EXTERN "writepdf;PDFCLOSE"
PDFMarginT  EXTERN "writepdf;PDFMarginT"
PDFMarginL  EXTERN "writepdf;PDFMarginL"
PDFOrient  EXTERN "writepdf;PDFOrient"
PDFPWidth  EXTERN "writepdf;PDFPWidth"
PDFPLength  EXTERN "writepdf;PDFPLength"
PDFABSPos  EXTERN "writepdf;PDFABSPos"
PDFHPos  EXTERN "writepdf;PDFHPos"
PDFHAdj  EXTERN "writepdf;PDFHAdj"
PDFVPos  EXTERN "writepdf;PDFVPos"
PDFVAdj  EXTERN "writepdf;PDFVAdj"
PDFTextOut  EXTERN "writepdf;PDFTextOut"
PDFNewLine EXTERN "writepdf;PDFNewLine"
PDFNewPage EXTERN "writepdf;PDFNewPage"
PDFFont	 EXTERN "writepdf;PDFFont"
PDFTTFont EXTERN "writepdf;PDFTTFont"
PDFBoldOn EXTERN "writepdf;PDFBoldOn"
PDFBoldOff EXTERN "writepdf;PDFBoldOff"
PDFUnderLineOn EXTERN "writepdf;PDFUnderLineOn"
PDFUnderLineOff EXTERN "writepdf;PDFUnderLineOff"
PDFAlignment  EXTERN "writepdf;PDFAlignment"
PDFMeasureString  EXTERN "writepdf;PDFMeasureString"
PDFColor  EXTERN "writepdf;PDFColor"
PDFPenSize  EXTERN "writepdf;PDFPenSize"
PDFUnits  EXTERN "writepdf;PDFUnits"
PDFRect  EXTERN "writepdf;PDFRect"
PDFRndRect  EXTERN "writepdf;PDFRndRect"
PDFLineTo  EXTERN "writepdf;PDFLineTo"
PDFOval  EXTERN "writepdf;PDFOval"
PDFImage  EXTERN "writepdf;PDFImage"
.

UnitChars CONST "1"
UnitLOENG CONST "2"
UnitLOMET CONST "3"
UnitHIENG CONST "4"
UnitHIMET CONST "5"
UnitPIXEL CONST "6"  // points.. 1/72".. default
UnitFONT  CONST "7"
UnitTwips CONST "8"
.
PORTRATE CONST	"1"	// default
LANDSCAPE CONST	"2"	
.
DECIMAL		CONST	"1"
LEFT		CONST	"2"
RIGHT		CONST	"3"
CENTER		CONST	"4"




PDFPRINT VERB	font=varlit:
         	text=varlit:
         	N,F,NEWPAGE:
         	H=nvarlit,HA=nvarlit,V=nvarlit,VA=nvarlit:
         	MARGINT=nvarlit,MARGINL=nvarlit:
         	ORIENT=nvarlit:
         	PWIDTH=nvarlit,PLENGTH=nvarlit,P=nvarlit:nvarlit:
         	BOLDON,BOLDOFF,ULON,ULOFF,ALIGNMENT=nvarlit:
         	FGCOLOR=any,PENSIZE=nvarlit,UNITS=nvarlit:
         	RECT=nvarlit:nvarlit:nvarlit:nvarlit:
         	RNDRECT=nvarlit:nvarlit:nvarlit:nvarlit:nvarlit:nvarlit:
         	LINE=nvarlit:nvarlit,OVAL=nvarlit:nvarlit:nvarlit:nvarlit:
         	PICT=nvarlit:nvarlit:nvarlit:nvarlit:cvarlit

PDFPRINT	FUNCTION
	ENTRY

pname	DIM	30
pvalue1	VAR	@
pvalue2	VAR	@
pvalue3	VAR	@
pvalue4	VAR	@
pvalue5	VAR	@
pvalued	DIM	^
pvaluen1 FORM	^
pvaluen2 FORM	^
pvaluen3 FORM	^
pvaluen4 FORM	^
pvaluen5 FORM	^
pvaluei1 INTEGER	^
pvaluei2 INTEGER	^
pvaluei3 INTEGER	^
pvaluei4 INTEGER	^
pvaluei5 INTEGER	^

pfont	FONT	^
pcolor	COLOR	^
ppict	PICT	^

vartype	FORM	10
d4	dim	4
int4	integer	4
a	integer	1
r	integer	1
g	integer	1
b	integer	1


	loop

	GETPARM pname,pvalue1,pvalue2,pvalue3,pvalue4,pvalue5
	until over

	uppercase	pname

	switch	pname

	CASE "FONT"
. to-do
.  FONT object... don't know how to get font file name from font name though...
.  check if file name... break out size, attribute and file name for truetype...
.   ?? substitute TT Times, Courior, Arial for internal Times, Courior, Helvitica 
.   
	 TYPE	pvalue1,vartype
	 if (vartype=5168)	//font object
	  ALERT	NOTE,"Font object not supported, use filename or #"Times#", #"Courier#" or #"Helvetica#"",vartype
	  continue
	 ENDIF
	  

	 MOVEPTR	pvalue1,pvalued
	 CALL		PDFFont using pvalued
	CASE "TEXT"
	 MOVEPTR	pvalue1,pvalued
	 CALL		PDFTextOut using pvalued
	CASE "N"
	 CALL		PDFNewLine
	CASE "F"
	 CALL		PDFNewPage
	CASE "NEWPAGE"
	 CALL		PDFNewPage
        CASE "H"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFHpos using pvaluen1
        CASE "HA"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFHAdj using pvaluen1
        CASE "V"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFVpos using pvaluen1
        CASE "VA"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFVAdj using pvaluen1
        CASE "MARGINT"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFMarginT using pvaluen1
        CASE "MARGINL"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFMarginL using pvaluen1
        CASE "ORIENT"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFORIENT using pvaluen1
        CASE "PWIDTH"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFPWidth using pvaluen1
        CASE "PLENGTH"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFPLength using pvaluen1
        CASE "P"
	 MOVEPTR	pvalue1,pvaluen1
	 MOVEPTR	pvalue2,pvaluen2
	 CALL		PDFABSPos using pvaluen1,pvalue2
        CASE "BOLDON"
         CALL		PDFBoldOn
        CASE "BOLDOFF"
         CALL		PDFBoldOff
        CASE "ULON"
         CALL		PDFUnderLineOn
        CASE "ULOFF"
         CALL		PDFUnderLineOff
        CASE "ALIGNMENT"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFAlignment using pvaluen1
        CASE "FGCOLOR"

. to-do
. fixup for color object
.   break out value into RGB componants
.
 debug
	 TYPE	pvalue1,vartype
	 IF (vartype=4656)	//color object
	  MOVEPTR	pvalue1,pcolor
	  GETITEM	pcolor,4,int4
	 ELSE if (vartype=8 or vartype=1032 or vartype=776 or vartype=520 or vartype=264 ) // integers
	  MOVEPTR	pvalue1,pvaluei1
	  MOVE		pvaluei1,int4
	 ELSE
	  MOVEPTR	pvalue1,pvaluen1
	  MOVE		pvaluen1,int4
	 
	 ENDIF
.
	 MOVE		int4,d4
	 UNPACK		d4,r,g,b,a
	 IF (a)
	  ALERT	NOTE,"System colors or alpha channel not supported!",vartype
	  continue
	 ENDIF

	 CALL		PDFColor using r,g,b
        CASE "PENSIZE"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFPenSize using pvaluen1
        CASE "UNITS"
	 MOVEPTR	pvalue1,pvaluen1
	 CALL		PDFUnits using pvaluen1
        CASE "RECT"
	 MOVEPTR	pvalue1,pvaluen1
	 MOVEPTR	pvalue2,pvaluen2
	 MOVEPTR	pvalue3,pvaluen3
	 MOVEPTR	pvalue4,pvaluen4
	 CALL		PDFRect using pvaluen1,pvaluen2,pvaluen3,pvaluen4
        CASE "RNDRECT"
. VERB GETPARM doesn't support enough parameters... NOT SUPPORTED
        CASE "LINE"
	 MOVEPTR	pvalue1,pvaluen1
	 MOVEPTR	pvalue2,pvaluen2
	 CALL		PDFLineTo using pvaluen1,pvaluen2
        CASE "OVAL"
	 MOVEPTR	pvalue1,pvaluen1
	 MOVEPTR	pvalue2,pvaluen2
	 MOVEPTR	pvalue3,pvaluen3
	 MOVEPTR	pvalue4,pvaluen4
	 CALL		PDFOval using pvaluen1,pvaluen2,pvaluen3,pvaluen4
        CASE "PICT"
.
. I never did like the fact that you CANNOT get the file name out
. of a pict object.  A pict object allows you to write it out to a
. BMP file but that will cause a very large ineffeciency here as
. each time a pict is use, a new file would be created resulting in an
. image such as a signature to be embeded EVERY time its used.  While this
. is what windows pdf print drivers do, my routines re-use resources to 
. save space.  Not being able to get a filename out of a pict object 
. prevents transparent portability with PRTPAGE for this control.

	 TYPE	pvalue1,vartype
	 if (vartype=2684)	//pict object
	  ALERT	NOTE,"PICT object not supported, use filename",vartype
	  continue
	 ENDIF
	

	 MOVEPTR	pvalue1,pvaluen1
	 MOVEPTR	pvalue2,pvaluen2
	 MOVEPTR	pvalue3,pvaluen3
	 MOVEPTR	pvalue4,pvaluen4
	 MOVEPTR	pvalue5,pvalued
	 CALL		PDFImage using pvaluen1,pvaluen2,pvaluen3,pvaluen4,pvalued

	ENDSWITCH


	repeat


	functionend


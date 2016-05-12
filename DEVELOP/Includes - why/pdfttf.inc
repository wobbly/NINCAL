.
. get information from TrueType font files
. 
.  This version only supports 8-bit character sets.
.    TTF normally has a unicode character->glyph maping table but as
.    the PL/B language doesn't support unicode, there is no need to 
.    parse it.
.
.  This version does not subset embedded TTF fonts.
.
.some standard windows fonts... everybody should have these so don't embed them.
.     
winfontscount	const	"12"
winfonts	dim	250(winfontscount),("C:\WINDOWS\FONTS\TIMES.TTF"):
				("C:\WINDOWS\FONTS\TIMESBD.TTF"):
				("C:\WINDOWS\FONTS\TIMESI.TTF"):
				("C:\WINDOWS\FONTS\TIMESBI.TTF"):
				("C:\WINDOWS\FONTS\ARIAL.TTF"):
				("C:\WINDOWS\FONTS\ARIALBD.TTF"):
				("C:\WINDOWS\FONTS\ARIALI.TTF"):
				("C:\WINDOWS\FONTS\ARIALBI.TTF"):
				("C:\WINDOWS\FONTS\COUR.TTF"):
				("C:\WINDOWS\FONTS\COURBD.TTF"):
				("C:\WINDOWS\FONTS\COURI.TTF"):
				("C:\WINDOWS\FONTS\COURBI.TTF")
.
pdfFontCache	IFILE
dim260	dim	260
hash	dim	16
hashkey	dim	24

fontattr	record
hash	dim	24
Filename dim	260
Name	dim	80	//
embedOK form	1
FirstChar form	"32"	//
LastChar  form	"255"	//
Widths	form	5(0..256) //
MissingW form	5	//
Flags	form	10	//  need to parse OS/2 table to determin serif font!!
Stemv	form	2	// ?? default ??
Ascent	form	5	//
Descent	form	5	//
.capheight form	5
Bbox	record		//
L	form	5	//
B	form	5	//
R	form	5	//
T	form	5	//
	recordend
.Unicode	form	1	//Font is unicode!!!
	recordend
cmap	record
format		integer	2
length		integer	2
lang		integer	2
glyphArray	integer	1(0..255)
	recordend

cmap6	record
format		integer	2
length		integer	2
lang		integer	2
firstCode	integer	2
entryCount	integer	2
glyphArray	integer	2(0..255)  // hope we can do 8 bit with unicode table
	recordend



d2	dim	2
$2bytes record
int1	integer	1
int2	integer	1
	recordend

d4	dim	4
$4bytes record
int1	integer	1
int2	integer	1
int3	integer	1
int4	integer	1
	recordend

swap2	macro	&param
		move	&param,d2
		unpack	d2,$2bytes
		pack	d2,$2bytes.int2,$2bytes.int1
		move	d2,&param
	mend

swap4	macro	&param
		move	&param,d4
		unpack	d4,$4bytes
		pack	d4,$4bytes.int4,$4bytes.int3,$4bytes.int2,$4bytes.int1
		move	d4,&param
	mend

.
. Read TTF file directly...
.   Code ported from LIBHARU  hpdf_fontdef_tt.c
.
AttrIdx	form	1
NumOfTTFonts form 2
TTFonts RECORD [30] //allow use of 30 TT fonts
File	dim	260
Res	form	3[0..3] // 0=plane 1=bold 2=italic 3=Bold+Italic
Descriptor form	3
	recordend

PDFTTFont FUNCTION
filename	dim	260
fsize		form	3
attrib		dim	^
	ENTRY
FontFile	FILE
Header	record
version		integer	4
tablecnt	integer 2
search		integer	2
entrsel		integer	2
range		integer	2
	recordend
.
table		record	(40) // max 20 table entries
tag		dim	4
checksum	integer 4
offset		integer	4
length		integer	4
	recordend
.
head	record
ver		integer	4
rev		integer	4
chk		integer	4
magicnm		integer	4
flags		integer	2
units_p		integer	2	//
created		dim	8
modified	dim	8
x_min		integer	2	bbox
y_min		integer	2
x_max		integer	2
y_max		integer	2
mac_stl		integer	2
low_rec_ppem	integer 2
font_direction	integer 2
loc_fmt		integer	2
glyph_fmt	integer 2
	recordend
.
hhea	record
ascent		integer	2
descent		integer	2
		dim	26
h_metric	integer 2  //count of
	recordend
.
cmapt	record
version		integer	2
count		integer	2
	recordend
savepos		integer	4
cmapEnt	record
platform	integer	2
encoding	integer	2
offset		integer	4
	recordend
cmapfmt		integer	2
.
nametbl	record
format		integer	2
count		integer	2
strOff		integer	2
	recordend
.
nameEnt	record (100)
platform	integer	2
encoding	integer	2
language	integer	2
nameID		integer	2
length		integer	2
offset		integer	2
	recordend
.
cntr	integer	1
cntr1	integer	1
int2	integer	2
int2a	integer	2
noffset	integer	4
nlen	integer	2
aoffset	integer	4
alen	integer	2
.
fm10	form	10
streamdat dim ^
.
.HasCmapEnc6	form	1
// Get attribute index first
	CLEAR		AttrIdx
	MOVEPTR	attrib,attrib
	IF NOT OVER
	 WHEREIS	"Italic",Attrib
	 IF NOT ZERO
	  ADD		"2",AttrIdx
	 ENDIF
	 WHEREIS	"Bold",Attrib
	 IF NOT ZERO
	  ADD		"1",AttrIdx
	 ENDIF
	ENDIF
.
. check if font already used
. 
	UPPERCASE Filename
	FOR	cntr,1,NumOfTTFonts
	 IF (filename=TTFonts(cntr).File)
	  // re-using so we know data is in the cache at this point
	  CALL		ReadTTFCache using filename

	  IF ( TTFonts(cntr).Res(AttrIdx) )
	  // re-use
	    SQUEEZE	TTFonts(cntr).Res(AttrIdx),scratch
	    PACK	CurFontRes,"/TF",scratch," ",scratch," 0 R"
.
	    PACK	CurrentFont,"/TF",scratch," ",fsize," Tf"
	    reset	page(curpage).fontres
	    SCAN	CurFontRes,page(curpage).fontres
	    IF NOT EQUAL
	     ENDSET 	page(curpage).fontres
	     APPEND	CurFontRes,page(curpage).fontres
	    ENDIF
	    SET		usingttf
	    SETFLAG NOT OVER
	    RETURN
	  ELSE
	   // new base font object, re-use descriptor
	   CALL		WriteBaseFont using cntr,TTFonts(cntr).Descriptor,fsize,attrib

	  ENDIF

	  GOTO		LoadLocal

	 ENDIF
	REPEAT
.
. load data from cache if we have ever used this font
.
	EXCEPTSET	nocache if io
	CALL		ReadTTFCache using filename
	GOTO		FromCache IF NOT OVER
.
. first time use, parse the TTF file for font metrix
.
nocache
	EXCEPTCLEAR	io
	CLEAR	fontattr
	MOVE	"32",fontattr.FirstChar
	MOVE	"255",fontattr.LastChar
.
. Load the header and TTF table of contents
. 
	MOVE	filename,Fontattr.Filename
	OPEN	FontFile,filename,READ
	READ	FontFile,seq;*ABSON,Header;
	swap2	Header.tablecnt
.
	FOR	cntr,1,Header.tablecnt
	  READ	FontFile,seq;*ABSON,table(cntr);
	  swap4	table(cntr).offset
	REPEAT
.
. we have the tables and they can appear in any order but they must be 
. processed in a particular order due to dependancies.
.
	FOR	cntr,1,Header.tablecnt
	  BREAK	IF (table(cntr).tag="head")
	REPEAT
	REPOSIT	FontFile,table(cntr).offset
	READ	FontFile,seq;*ABSON,head;
	swap2	head.x_min
	swap2	head.y_min
	swap2	head.x_max
	swap2	head.y_max
	swap2	head.units_p
.
.	// make signed and calculate pdf glyph box
	MOVE	head.x_min,fm10
	IF 	(fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.bbox.l = fm10 * 1000 / head.units_p
.
	MOVE	head.y_min,fm10
	IF (fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.bbox.b = fm10 * 1000 / head.units_p
.
	MOVE	head.x_max,fm10
	IF (fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.bbox.r = fm10 * 1000 / head.units_p
.
	MOVE	head.y_max,fm10
	IF (fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.bbox.t = fm10 * 1000 / head.units_p
.
	FOR	cntr,1,Header.tablecnt
	  BREAK	IF (table(cntr).tag="hhea")
	REPEAT
	REPOSIT	FontFile,(table(cntr).offset+4)
	READ	FontFile,seq;*ABSON,hhea;
	swap2	hhea.ascent
	swap2	hhea.descent
	swap2	hhea.h_metric
.
	MOVE	hhea.ascent,fm10
	IF	(fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.ascent = fm10 *1000 / head.units_p
.
	MOVE	hhea.descent,fm10
	IF	(fm10 > 32768 )
	 SUB	"65536",fm10
	ENDIF
	CALC	fontattr.descent = fm10 *1000 / head.units_p
.
. get char mapping (http://developer.apple.com/fonts/ttrefman/rm06/Chap6cmap.html)
.
	FOR	cntr,1,Header.tablecnt
	  BREAK	IF (table(cntr).tag="cmap")	// char -> glyph index mapping
	REPEAT
	REPOSIT	FontFile,table(cntr).offset

	READ	FontFile,seq;*ABSON,cmapt;
	swap2	cmapt.version
	swap2	cmapt.count
.
. locate byte encoding
.
	FOR	cntr1,1,cmapt.count
	 READ		FontFile,seq;*ABSON,cmapEnt;
	 swap2		cmapEnt.platform
	 swap2		cmapEnt.encoding
	 swap4		cmapEnt.offset
	 FPOSIT		FontFile,savepos
	 REPOSIT	FontFile,( table(cntr).offset + cmapEnt.offset )
	 READ		FontFile,seq;*ABSON,cmapfmt
	 swap2		cmapfmt
	 IF ( cmapfmt = 0 )
	  BREAK
	 ENDIF
.	 if ( cmapfmt = 6 )
.	  set HasCmapEnc6
.	  REPOSIT	FontFile,( table(cntr).offset + cmapEnt.offset )
.	  READ	FontFile,seq;*ABSON,cmap6;
.	  swap2	cmap6.format
.	  swap2	cmap6.length
.	  swap2	cmap6.lang
.	  swap2	cmap6.firstCode
.	  swap2	cmap6.entryCount
.	  IF (cmap6.entryCount > 256)
.	   MOVE	"256",cmap6.entryCount 
.	  ENDIF
.	 ENDIF
	 REPOSIT	FontFile,savepos
	REPEAT
.
	IF ( cntr1 > cmapt.count ) //and HasCmapEnc6 = 0)
	 SETFLAG OVER
	 RETURN
	ENDIF
.	ELSE IF ( cntr1 > cmapt.count )
.	 MOVE	cmap6.format,cmap.format
.	 move	cmap6.length,cmap.length
.	 move	cmap6.lang,cmap.length
.	 move	cmap6.firstCode,fontattr.FirstChar
.	 move	cmap6.firstCode,fontattr.LastChar
.	 Add	cmap6.entryCount,fontattr.LastChar
.	 for cntr1,1,cmap6.entryCount
.	  swap2 cmap6.glyphArray(cntr1) 
.	  move	cmap6.glyphArray(cntr1),cmap.glyphArray(cntr1)
.	 repeat
.	ELSE
.
	REPOSIT	FontFile,( table(cntr).offset + cmapEnt.offset )
	READ	FontFile,seq;*ABSON,cmap;
	swap2	cmap.format
	swap2	cmap.length
	swap2	cmap.lang
.	ENDIF
.
.
. get char widths
. 
	FOR	cntr,1,Header.tablecnt
	  BREAK	IF (table(cntr).tag="hmtx")	// horizontal metrix...Widths
	REPEAT
	REPOSIT	FontFile,table(cntr).offset
.
. fonts can have thousands of characters... we only want the 8 bit ansi set
.
	IF (hhea.h_metric > 254 )	// only working with 8 bit characters
	 MOVE	"254",hhea.h_metric
	ENDIF
.
	FOR cntr,0,hhea.h_metric
	  READ	FontFile,seq;*ABSON,int2;	// advance_width
	  swap2	int2
	  CALC	fontattr.Widths(cntr) = int2 * 1000 / head.units_p
	  if (fontattr.Widths(cntr) )
	  MOVE	fontattr.Widths(cntr),int2a  // save in case of incomplete table
	  ENDIF
	  READ	FontFile,seq;*ABSON,int2;	// lsb ??
	REPEAT
	if (hhea.h_metric<254)
	 loop
	  MOVE	int2a,fontattr.Widths(cntr)
	  incr	cntr
	  until (cntr>254)
	 repeat
	endif

	MOVE	fontattr.Widths(cmap.glypharray(32)),fontattr.MissingW  // just use space width for missing
	IF 	(fontattr.widths(cmap.glypharray(105)) = fontattr.widths(cmap.glypharray(119)))  // compair 'i' to 'w' for fixed
	 ADD	"1",fontattr.Flags
	ENDIF
.
. get the font name
. 
	FOR	cntr1,1,Header.tablecnt
	  break	if (table(cntr1).tag="name")	// name and attributes
	REPEAT
	REPOSIT	FontFile,table(cntr1).offset
.
	READ	FontFile,seq;*ABSON,nametbl;
	swap2	nametbl.format
	swap2	nametbl.count
	swap2	nametbl.strOff
	
	FOR	cntr,1,nametbl.count
	 READ	FontFile,seq;*ABSON,nameEnt(cntr);
	 swap2	nameEnt(cntr).platform
	 swap2	nameEnt(cntr).encoding
.	 swap2	nameEnt(cntr).language	// not used
	 swap2	nameEnt(cntr).nameID
	 swap2	nameEnt(cntr).length
	 swap2	nameEnt(cntr).offset
	  // ansi name
	 IF (nameEnt(cntr).platform = 1 && nameEnt(cntr).encoding = 0 && nameEnt(cntr).nameID=6 )
	  CALC	noffset = table(cntr1).offset + nametbl.strOff + nameEnt(cntr).offset
	  MOVE	nameEnt(cntr).length,nlen
	 ENDIF
	  // ansi attributes
	 IF (nameEnt(cntr).platform = 1 && nameEnt(cntr).encoding = 0 && nameEnt(cntr).nameID=2 )
	  CALC	aoffset = table(cntr1).offset + nametbl.strOff + nameEnt(cntr).offset
	  MOVE	nameEnt(cntr).length,alen
	 ENDIF
	  // not doing unicode...for now
	REPEAT

	REPOSIT	FontFile,noffset
	READ	FontFile,seq;*ABSON,fontattr.name;
	SETLPTR	fontattr.name,nlen
	SQUEEZE fontattr.name,fontattr.name	//remove spaces from name
.
	REPOSIT	FontFile,aoffset
	READ	FontFile,seq;*ABSON,scratch;
	SETLPTR	scratch,alen
	WHEREIS	"Bold",scratch
	IF NOT ZERO
	 ADD	"262144",fontattr.Flags  // bit 19
	ENDIF
	WHEREIS	"Italic",scratch
	IF NOT ZERO
	 ADD	"64",fontattr.Flags	// bit 7
	ENDIF

	ADD	"4",fontattr.Flags	// bit 3  - symbolic ( 8bit )
//
// can we embed this font? 
// 
	FOR	cntr,1,Header.tablecnt
	  BREAK	IF (table(cntr).tag="OS/2")
	REPEAT
	REPOSIT	FontFile,table(cntr).offset
	READ	FontFile,seq;*abson,int2;	// table version
.
	IF (int2=0) // http://developer.apple.com/fonts/ttrefman/rm06/Chap6OS2.html
	 REPOSIT	FontFile,(table(cntr).offset + 7 ) //fsType
	ELSE	//http://www.microsoft.com/typography/otspec/os2.htm#ver
	 REPOSIT	FontFile,(table(cntr).offset + 8 ) //fsType
	ENDIF
.
	CLEAR	FontAttr.EmbedOK
	READ	FontFile,seq;*abson,int2;
	swap2	int2
	IF	( int2 )  // has restrictions
	 AND	268,int2  //( 0x100 | 0x008 | 0x004 )  embedding allowed
	 IF 	( int2 )
	   SET	FontAttr.EmbedOK
	 ENDIF
	ELSE  // no restrictions
	  SET	FontAttr.EmbedOK
	ENDIF
.
	UPPERCASE FontAttr.Filename
	SEARCH	FontAttr.Filename,winfonts(1),winfontscount,int2a
	IF (int2a)
	 CLEAR	FontAttr.EmbedOK
	ENDIF
..
.. this would be needed to embed font subsets
..
.	FOR	cntr,1,Header.tablecnt
.	  BREAK	IF (table(cntr).tag="loca")	//  glyph offset table first
.	REPEAT
.	REPOSIT	FontFile,table(cntr).offset
....
.	FOR	cntr,1,Header.tablecnt
.	  BREAK	IF (table(cntr).tag="glyf")	//  glyphs table
.	REPEAT
.	IF	(Head.loc_fmt=0)
.	REPOSIT	FontFile,( table(cntr).offset + (GlyphOffset(char)*2) + 2 )
.	ELSE
.	REPOSIT	FontFile,( table(cntr).offset + GlyphOffset(char) + 2 )
.	ENDIF
.
. WRITE the font data to the PDF
.

	CLOSE	FontFile
	GETFILE	pdfFontCache
	IF NOT EQUAL
	PREP	pdfFontCache,"pdfFontCache","pdfFontCache","1-20","1958"
	ENDIF
.
	PACKKEY	dim260,fontattr.Filename
.
	FINDFILE fontattr.Filename,NAME=fontattr.hash
.	HASH	dim260,hash
.	ENCODE64 hash,fontattr.hash
.
	WRITE	pdfFontCache;fontattr,cmap.glypharray
	CLOSE	pdfFontCache
.
FromCache
.
	INCR	NumOfTTFonts
	CHOP	fontattr.Filename,TTFonts(NumOfTTFonts).File
.
	CALL	WriteBaseFont using NumOfTTFonts,TTFonts(NumOfTTFonts).Descriptor,fsize,attrib
.
//
// font descriptor
	MOVE	ObjNum,TTFonts(NumOfTTFonts).Descriptor
.
	FPOSIT	pdffile,xref(objnum).offset
	CLEAR	xref(ObjNum).Gen
	MOVE	"n",xref(ObjNum).type

	WRITE	pdffile,seq;*ll,scratch," 0 obj"
	WRITE	pdffile,seq;"<<"
	WRITE	pdffile,seq;"  /Type /FontDescriptor"
	WRITE	pdffile,seq;"  /FontName /",*ll,fontattr.Name
	WRITE	pdffile,seq;"  /ItalicAngle 0"
	WRITE	pdffile,seq;"  /MissingWidth ",fontattr.MissingW
	WRITE	pdffile,seq;"  /Flags ",fontattr.Flags
.
	WRITE	pdffile,seq;"  /StemV ",fontattr.Stemv
	WRITE	pdffile,seq;"  /Ascent ",fontattr.Ascent
	WRITE	pdffile,seq;"  /Descent ",fontattr.Descent
	WRITE	pdffile,seq;"  /FontBBox [",fontattr.bbox,"]"
.
	IF (fontattr.embedOK)
	 INCR		ObjNum  // xref for font descriptor
	 SQUEEZE	ObjNum,scratch
	 WRITE		pdffile,seq;"  /FontFile2 ",*ll,scratch," 0 R"
	ENDIF

	WRITE	pdffile,seq;">>"
	WRITE	pdffile,seq;"endobj"

	IF (fontattr.embedOK)
// font file

	 FPOSIT	pdffile,xref(objnum).offset
	 CLEAR	xref(ObjNum).Gen
	 MOVE	"n",xref(ObjNum).type
.
. may have come from cache to open, then read
.       
	 FINDFILE Fontattr.Filename,filesize=fm10
	 DMAKE	streamdat,fm10
	 OPEN	FontFile,fontattr.Filename,READ
	 READ	FontFile,seq;*abson,streamdat;
	 CLOSE	FontFile
.
	 WRITE	pdffile,seq;*ll,scratch," 0 obj"
	 WRITE	pdffile,seq;"<<"
	 WRITE	pdffile,seq;"  /Length ",fm10
	 WRITE	pdffile,seq;"  /Length1 ",fm10
	 WRITE	pdffile,seq;">>"
	 WRITE	pdffile,seq;"stream"
	 WRITE	pdffile,seq;*ll,streamdat
	 WRITE	pdffile,seq;"endstream"
	 WRITE	pdffile,seq;"endobj"

	ENDIF
.
LoadLocal
.
. load our internal font structure
.
	MOVE	fontattr.Descent,FontMetrics.Descender
	MOVE	"50",FontMetrics.ulpen
	MOVE	fontattr.Descent,FontMetrics.ulPos
 	CLEAR	FontMetrics.GlyphWidths
	FOR 	cntr,32,254
	 MOVE	fontattr.Widths(cmap.glypharray(cntr)),FontMetrics.GlyphWidths(cntr)
	REPEAT
	MOVE	fsize,fontsize
.
	ENDSET page(curpage).fontres
	APPEND	CurFontRes,page(curpage).fontres
.
	SET	usingttf
	SETFLAG NOT OVER
	FUNCTIONEND


WriteBaseFont	LFUNCTION
TTFNum		form	2
descriptor	form	^
fsize	form 3
attrib	dim	^
	ENTRY
cntr integer 1

	INCR	ObjNum  // xref for font object
	SQUEEZE	objNum,scratch
	FPOSIT	pdffile,xref(objnum).offset
	CLEAR	xref(ObjNum).Gen
	MOVE	"n",xref(ObjNum).type
.
	MOVE	ObjNum,TTFonts(TTFNum).Res(AttrIdx)
.
	PACK	CurFontRes,"/TF",scratch," ",scratch," 0 R"
.
	PACK	CurrentFont,"/TF",scratch," ",fsize," Tf"

	WRITE	pdffile,seq;*ll,scratch," 0 obj"
	WRITE	pdffile,seq;"<<"
	WRITE	pdffile,seq;"  /Type /Font"
	WRITE	pdffile,seq;"  /Subtype /TrueType"
	WRITE	pdffile,seq;"  /FirstChar ",fontattr.FirstChar
	WRITE	pdffile,seq;"  /LastChar ",fontattr.LastChar
	Write	pdffile,seq;"  /Encoding /WinAnsiEncoding"
	WRITE	pdffile,seq;"  /BaseFont /",*ll,fontattr.Name;
	MOVEPTR	attrib,attrib
	IF NOT OVER
	 IF	(attrib != "")
	  WRITE	pdffile,seq;",",attrib
	 ELSE
	  WRITE	pdffile,seq;
	 ENDIF
	ELSE
	 WRITE	pdffile,seq;
	ENDIF
	WRITE	pdffile,seq;"  /Widths [";

	FOR	cntr,32,254
	 WRITE	pdffile,seq;fontattr.Widths(cmap.glyphArray(cntr));
	REPEAT
	WRITE	pdffile,seq;"]"

	IF	(descriptor = 0)
	 INCR	ObjNum
	 MOVE	ObjNum,descriptor
	ENDIF
	
	SQUEEZE	descriptor,scratch
	WRITE	pdffile,seq;"  /FontDescriptor ",*ll,scratch," 0 R"
	WRITE	pdffile,seq;">>"
	WRITE	pdffile,seq;"endobj"

	FUNCTIONEND
	
ReadTTFCache	LFUNCTION
filename	dim	^
	ENTRY
	   OPEN		pdfFontCache,"pdfFontCache.isi"
	  FINDFILE	filename,name=hashkey
.	   PACKKEY	dim260,filename
.	   HASH		dim260,hash
.	   ENCODE64	hash,hashkey
	   READ		pdfFontCache,hashkey;*ll,fontattr,cmap.glypharray
	FUNCTIONEND

.
. pdf image support
.
. Aug 03 2011 added jpeg support
.  
. support up to 100 unique images.
. 
.  Aug 11 2011 added jpeg
.  Aug 11 2011	logic to preserve aspect ratio of original image
.
ImageFNames	dim	^250(99)
ImageObjNum	form	4(99)
ImageWidth	integer	4(99)
ImageHeight	integer	4(99)
ImageNum	form	2

.
PDFImage FUNCTION
top	form	5.2
bottom	form	5.2
left	form	5.2
right	form	5.2
ImageFileName	dim	250
	ENTRY
.
. images are an Xobject that are basically resources to a page.
.   They are then referenced in the page by the following within the data stream:
.	q  		%save graphics state
.	 /Image1 Do	%draw the image
.	Q 		% restore graphics state
.	
.  this routine therefor has to generate the Xobject and the stream commands.
.  
.  Note to self...Page resources in the code above are currently not dynamic enough
.  
.	path Resources needs  /XObject << /Image1 resnum subnum R >>
.
. resnum subnum obj
. 	<<
. 	/Type /XObject
. 	/Subtype /Image
. 	/Width  {image width}
. 	/Height {image height}
. 	/ColorSpace /DeviceRGB
. 	/BitsPerComponent {1 2 4 8 16}  % 8 = 24 bit image
. 	/Filter [ /DCTDecode ] %option used for JPEG images, not needed for BMP
. 	>>
. stream
. {image data}
. endstream
. endobj
.  
BMPr	RECORD
type	integer	4
width	integer	4
height	integer	4
wb	integer	4	//widthbytes
planes	integer	2
bitspix	integer	2
bits	integer	4
	recordend

fm10	form	10

BMPFileHdr	RECORD
type	dim	2	//"BM"
size	integer	4
res	integer	4
offbits	integer	4
		recordend

bmpi	record
size	integer	4
width	integer	4
height	integer	4
planes	integer	2
bitcount integer 2
compres	integer	4
szimg	integer	4
xppm	integer	4
yppm	integer	4
clruse	integer	4
clrimp	integer	4
	recordend

char2	dim	2
imgf	FILE
strlen	form	10
strdat	dim	^
.
pdfpos	record
width	form	5.4
	form	2	//??
	form	2	//??
height	form	5.4
horz	form	5.4
vert	form	5.4
	recordend
.
PDFFilter	DIM	22
tmp	dim	250

jpghead	record
tag	integer	1
type	integer 1
len	dim	1(2)	// fbo int 2
	recordend
.
d2	dim	2
da5	dim	1(5)
tag	integer	1,"0xff"
jpgtype	integer	1,"0xd8"
sizetyp integer	1,"0xc0"
fpos	integer	4
int2	integer	2
.
fscratch1 form	4.5
fscratch2 form	4.5
AspectI	form	4.5
AspectO	form	4.5
.
content	dim	200
.
	 CALL	CVTVUnit giving top using top
	 CALL	CVTVUnit giving bottom using bottom
	 CALL	CVTHUnit giving left using left
	 CALL	CVTHUnit giving right using right
.
.	ADD	MarginT,top
.	ADD	MarginT,bottom
	ADD	MarginL,Left
	Add	MarginL,right
.
	MOVE	(Page(curpage).Height-top),pdfpos.vert  //pagesize
	MOVE	left,pdfpos.horz
	MOVE	(bottom-top),pdfpos.height
	MOVE	(right-left),pdfpos.width
	SUB	pdfpos.height,pdfpos.vert  // bottom up, not top down.
.
. check if we already have this image resource
. 
	SEARCH	ImageFileName,ImageFNames(1),"99",ImageNum
	IF NOT EQUAL
	 MOVE		ImageFileName,tmp
	 LOWERCASE	tmp
	 WHEREISLAST	".",tmp,strlen
	 RETURN if zero	// unable to determine image type
	 RESET		tmp,strlen
	 CHOP		tmp
.
	SWITCH	tmp
	CASE	".jpg"
	 NOBREAK
	CASE	".jpeg"
	 MOVE	"/Filter [ /DCTDecode ]",PDFFilter
	 clear	fpos
	 OPEN	imgf,ImageFileName
	 READ	imgf,seq;*abson,jpghead;
	 if ( jpghead.tag != tag and jpghead.type != jpgtype )
	  close imgf
	  setflag zero  // unsupported image type
	  return
	 endif
. jpeg header:
.  FF nn  tag type  ( FF D8 is begin of file and has no size)
.  xx xx  tag size ( forward byte order )
.  xxxxx data
.
.   image dimensions are in tag
.   	FF C0
.   	00 11
.   	08 
.   	hh ww height width 
	 LOOP
	  ADD	"2",fpos
	  REPOSIT imgf,fpos
	  READ	imgf,seq;*abson,jpghead;
	  IF OVER
	   CLOSE imgf
	   SETFLAG ZERO
	   RETURN
	  ENDIF
	  BREAK If ( jpghead.tag = tag and jpghead.type = sizetyp )
	  PACK	d2,jpghead.len(2),jpghead.len(1)
	  MOVE	d2,int2
	  ADD	int2,fpos
	 REPEAT
.
	 READ	imgf,seq;*abson,da5;
	 PACK	d2,da5(3),da5(2)
	 MOVE	d2,int2
	 MOVE	int2,BMPi.height
	 PACK	d2,da5(5),da5(4)
	 MOVE	d2,int2
	 MOVE	int2,BMPi.width
.
	 READ	imgf,EOF;;
	 FPOSIT	imgf,strlen
	 REPOSIT imgf,fzero
	 DMAKE	strdat,strlen
	 READ	imgf,seq;*ABSON,strdat;
	 CLOSE	imgf
	 
	CASE	".bmp"	// bitmap
	 CLEAR	PDFFilter
.
. get image data
. 
	OPEN	imgf,ImageFileName
	READ	imgf,seq;*ABSON,BMPFileHdr;
	READ	imgf,seq;*ABSON,BMPI;
.
. calculate stream length for image data
. 
	CALC	strlen=BMPFileHdr.size-BMPFileHdr.offbits
	DMAKE	strdat,strlen
.
. read the image data
. 
	REPOSIT	imgf,BMPFileHdr.offbits
	READ	imgf,seq;*ABSON,strdat;

	IF ( bmpi.bitcount = 16 or bmpi.bitcount = 32 )
	 CALL	cvt24 using bmpi.bitcount,strdat
	ENDIF
. convert to PDF stream
	CALL	BMP2PDF using strdat,BMPi.width,BMPi.height
.
	DEFAULT
	 setflag	zero	// unsupported image type
	 RETURN
	ENDSWITCH
.
. figure out image scaling and adjust size to prevent stretch
.
	MOVE	bmpi.width,fscratch1
	MOVE	bmpi.height,fscratch2
.
	CALC 	aspecti = pdfpos.width / fscratch1
	CALC 	aspecto = pdfpos.height / fscratch2
	IF 	( aspecti < aspecto )
	 MOVE	(aspecti*fscratch2), pdfpos.height
	ELSE
	 MOVE	(aspecto*fscratch1), pdfpos.width
	ENDIF
.
. Write the XObject
.
	INCR	ObjNum
	INCR	pictnum
	SQUEEZE	pictnum,char2
	SQUEEZE	objNum,scratch
.
	  FPOSIT	pdffile,xref(objnum).offset
	  CLEAR		xref(ObjNum).Gen
	  MOVE		"n",xref(ObjNum).type
.
	WRITE	pdffile,seq;*ll,scratch," 0 obj"
	WRITE	pdffile,seq;" << /Type /XObject"
	WRITE	pdffile,seq;"    /Subtype /Image"
	WRITE	pdffile,seq;"    /Name /Img",*ll,char2
	WRITE	pdffile,seq;"    /ColorSpace /DeviceRGB ",*ll,PDFFilter
.
	MOVE	BMPi.width,fm10
	WRITE	pdffile,seq;"    /Width ",fm10
.
	MOVE	BMPi.height,fm10
	WRITE	pdffile,seq;"    /Height ",fm10
.
	WRITE	pdffile,seq;"    /BitsPerComponent 8" //24 bit images have 8 bit components
	WRITE	pdffile,seq;"    /Length ",strlen
	WRITE	pdffile,seq;" >>"
.
	WRITE	pdffile,seq;"stream"
	WRITE	pdffile,seq;strdat
	WRITE	pdffile,seq;"endstream"
	WRITE	pdffile,seq;"endobj"
	WRITE	pdffile,seq;
.
	DFREE	strdat
.
	MOVE	ImageFileName,ImageFNames(pictnum)
	MOVE	ObjNum,ImageObjNum(pictnum)
	MOVE	bmpi.width,ImageWidth(pictnum)
	MOVE	bmpi.height,ImageHeight(PictNum)
.
	ELSE
.
	SQUEEZE	ImageNum,Char2
	MOVE	ImageNum,pictnum
.
. figure out image scaling and adjust size to prevent stretch
.
	MOVE	ImageWidth(pictnum),fscratch1
	MOVE	ImageHeight(pictnum),fscratch2
.
	CALC 	aspecti = pdfpos.width / fscratch1
	CALC 	aspecto = pdfpos.height / fscratch2
	IF 	( aspecti < aspecto )
	 MOVE	(aspecti*fscratch2), pdfpos.height
	ELSE
	 MOVE	(aspecto*fscratch1), pdfpos.width
	ENDIF
.
	ENDIF
.
.	WRITE	pages(curpage),seq;"q"
.	WRITE	pages(curpage),seq;pdfpos," cm" //width ? ? height horz vert 
.	WRITE	pages(curpage),seq;"  /Img",*ll,char2," Do"
.	WRITE	pages(curpage),seq;"Q"
	PACK	content,"q ",pdfpos," cm /Img",char2," Do Q",CRLF
	APPEND	content,page(curpage).content

	TYPE	page(curpage).imgres
	if EOS
	 MOVE	" ",page(curpage).imgres
	endif
	   APPEND	"/Img",page(curpage).imgres
	   APPEND	char2,page(curpage).imgres
	   APPEND	" ",page(curpage).imgres
	   APPEND	ImageObjNum(pictnum),page(curpage).imgres
	   APPEND	" 0 R ",page(curpage).imgres

	FUNCTIONEND
.
. convert 16 or 32 bit images to 24 bit
.
cvt24 LFUNCTION
inBitCnt	form	2
data	dim	^
	entry
inlen	form	10
outdata	dim	^
pixel	dim	^
pixred	integer	2
pixgrn	integer	2
pixblu	integer	2
outpixel integer	3
outpixd	dim	3
.
	MOVEPLEN	Data,inlen
.
.calculate size needed for conversion
.          
	IF	 (inBitCnt = 16 )
	 DIV	"2",inlen	
	 DMAKE	pixel,2
	ELSE
	 DIV	"4",inlen
	 DMAKE	pixel,4
	ENDIF
	MULT	"3",inlen
.
	DMAKE		outdata,inlen
.
	LOOP
	 REMOVE		data,pixel
	 BREAK IF EOS
	 IF 		(inBitCnt=32)
	  BUMP		pixel,4 // strip alpha
	  MOVE		pixel,outpixd
	 ELSE
.
. break out 16-bit RGB (assumes 5-5-5 scheme)
. 
	  MOVE		pixel,pixred
	  SHIFTRIGHT	pixred,10	// right justify red
	  SHIFTLEFT	pixred,3	// intesify to 8 bits
	  AND		31,pixgrn	// strip off MSB
          
	  MOVE		pixel,pixgrn
	  SHIFTRIGHT 	pixgrn,5	// right justify green
	  AND		31,pixgrn	// strip off red
	  SHIFTLEFT	pixgrn,3	// intesify to 8 bits
          
	  MOVE		pixel,pixblu
	  AND		31,pixblu	// strip red and green
	  SHIFTLEFT	pixgrn,3	// intesify to 8 bits
. asseble 24-bit RGB
	  MOVE		pixred,outpixel
	  SHIFTLEFT	outpixel,8
	  OR		pixgrn,outpixel
	  SHIFTLEFT	outpixel,8
	  OR		pixblu,outpixel
	  MOVE		outpixel,outpixd
	 ENDIF
.
	 APPEND		outpixd,outdata

	REPEAT
	RESET		outdata
.
	MOVE		outdata,data 
	DFREE		outdata
	DFREE		pixel
.	 
	FUNCTIONEND
.
BMP2PDF	LFUNCTION
imgdata	dim	^
Width	integer	4
Height	integer	4
       	ENTRY
scanline	dim	^
outimg	dim	^
row	integer	4
sz	integer	4
.
pix	dim	3
r	integer	1
g	integer	1
b	integer	1
cntr	integer	4
.
.	output buffer calculated before alignment size because there should be
.	no padding in the PDF stream
.	
	CALC	sz=width*height*3
. 
	DMAKE		outimg,sz
	SETLPTR		outimg
.
	MOVE	Width,sz
	MULT	"3",sz	// 3-bytes per pixel
	DMAKE	scanline,sz
.
. adjust width for alignment padding
.
	ADD	"3",width
	DIV	"4",width
	MULT	"4",width
.
	MULT	"3",width	// byte count
.
. walk the scan lines
. 
	FOR		row,1,height
	 MOVE		imgdata,scanline
	 BUMP		imgdata,width
.       
	 FOR		cntr,1,sz		// BGR to RGB
	  MOVE		scanline,pix
	  UNPACK	pix,b,g,r
	  PACK		pix,r,g,b
	  MOVELS	pix,scanline
	  BUMP		scanline,3
	 REPEAT
	 RESET		scanline
.        
	 RESET		outimg,(((height-row)*sz)+1)
	 MOVELS		scanline,outimg
	REPEAT
.
	MOVE		outimg,imgdata
	DFREE		scanline
	DFREE		outimg
.
       	FUNCTIONEND

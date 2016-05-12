.
. routine to use external PDF as a page template...1st page only
.
cachedforms	form	3
FormCache	record (100)
pdf		dim	260
resnum		form	4
		recordend



PDFForm FUNCTION
InPDF	DIM	^	// pdf file name
        ENTRY
// for our purposes, we will assume the first page of the specified PDF is
// our form XObject.
//  process page dictionary...
//  /MediaBox is ourput as /BBox
//  /Type /XObject /Subtype /Form /FormType 1

//  /MetaData copied as is if exists
//  /LastModified copied as is if exists

//  /Resources are copied ( all referenced objects have to renumbered and copied. )
//  		// check if font resources already exist??
//  		
//  /Contents resource entry is discarded and the contents are read and output as stream
//
//  XObject reference added to page(curpage).imgres  ( xobject resource list )
//    Append "/PDFForm",page(curpage).imgres
//    append formnum,page(curpage).imgres
//    append " ",page(curpage).imgres
//    append obj,page(curpage).imgres
//    append " 0 R ",page(curpage).imgres
//          
//  finally:
//  		is "cm" necessary?  if so, pagewidth," 0 0 ",pageheight," 0 0 cm"
//  	PACK	content,"q /PDFForm",formnum." Do Q",crlf
//  	APPEND	content,page(curpage).content
.
inxref		record	(0..100)	// should be enough for a "template" pdf
Offset		FORM	10
.		dim	1
Gen		form	5
.		dim	1
type		dim	1
		RECORDEND
startxref	form	10
infile		file
cntr		form	4
scratch		dim	15000
scratch2	dim	40
startobj	form	10
endobj		form	10
obj		form	4
pageobj		form	4
contentobj	form	4
contentlen	form	10
content		dim	^
.
bbox		record
left		form 5
bottom  	form 5
right   	form 5
top     	form 5
		recordend
resStart	form	3
ws		init	0x0a,0x20,0x0d,0x20
filter		dim	20
fontres 	dim	100
xobjres		dim	100
resname		dim	10
newfres		dim	100
newxres		dim	100
buffer		dim	^
.
reslist		dim	^
newres		dim	^
HaveFontFile	form	1
lf	init	0x0a
cr	init	0x0d
.
. check to see if we have used this form before
.
	FOR cntr,1,cachedforms
	 IF (inPDF=FormCache(cntr).pdf)
	  SQUEEZE	FormCache(cntr).resnum,scratch2
	  GOTO	ContentOut
	 ENDIF
	REPEAT
.
	DMAKE	buffer,2000000  // this should be good enough for our purposes
.
. open source pdf and load the cross reference
.
	OPEN	infile,inPDF,READ
	READ	infile,EOF;;
	FPOSIT	infile,startobj
	SUB	"15000",startobj
	IF LESS
	 CLEAR	startobj
	ENDIF
	REPOSIT	infile,startobj
.
	 READ		infile,SEQ;*ABSON,scratch;
	 SCAN		"startxref",scratch
	 return if not equal
.
	BUMP		scratch,9
	REPLACE		ws,scratch
	PARSENUM	scratch,startxref
	REPOSIT		infile,startxref
	READ		infile,seq;*abson,scratch
.
	MATCH	"xref",scratch
	RETURN IF NOT EQUAL	// unsupported construct 
.				//(can be an object to compress xref 
.				// but we don't do compression)
.
.  NOTE, we are assuming!!!  xref can contain gaps 
.  	we are assuming all the objects are sequentially numbered (normal)
.
	BUMP	scratch,4
	REPLACE	ws,scratch
	PARSE	scratch,scratch2,"09"
	MOVE	scratch2,startobj
	PARSE	scratch,scratch2,"09"
	MOVE	scratch2,endobj  // normally =  0 objcnt
	CALC	obj=endobj-startobj-1
	FOR 	cntr,"0",obj
	 PARSE	scratch,scratch2,"09"
	 MOVE	scratch2,inxref(cntr).Offset
	 PARSE	scratch,scratch2,"09"
	 MOVE	scratch2,inxref(cntr).Gen
	 PARSE	scratch,inxref(cntr).type,"az"
	REPEAT
.
. go to root object to find top of page tree
.
	REPOSIT	infile,inxref(1).Offset
.
	READ	infile,seq;*abson,scratch
	SCAN	"/Pages",scratch
	RETURN if not EQUAL
	BUMP	scratch,7
	REPLACE	ws,scratch
	PARSE	scratch,scratch2,"09"
	MOVE	scratch2,obj
.
	REPOSIT	infile,inxref(obj).Offset
	READ	infile,seq;*abson,scratch
.
. walk page tree to find first page
. 
ptNode
	SCAN	"/Kids",scratch
	SCAN	"[",scratch
	BUMP	scratch
	REPLACE	WS,scratch
	PARSE	scratch,scratch2,"09"
	MOVE	scratch2,obj	// first page
.
	REPOSIT	infile,inxref(obj).Offset
	READ	infile,seq;*abson,scratch
.
	SCAN	"endobj",scratch
	LENSET	scratch
	RESET	scratch
	REPLACE	WS,scratch
.
	SCAN	"/Type",scratch
	BUMP	scratch,5
	PARSE	scratch,scratch2,"//azAZ"
	IF 	(scratch2="/Pages")
	 RESET	scratch
	 GOTO	ptNode
	ENDIF
	RESET	scratch
	MOVE	obj,pageobj
.
. We have our page object!!!
.
.
	SCAN	"/MediaBox",scratch
	SCAN	"[",scratch
	BUMP	scratch
	PARSE	scratch,scratch2,"  09"
	EXPLODE	scratch2," ",bbox
.
	RESET	scratch
	SCAN	"/Contents",scratch
// we are assuming again,  While unusual, this can be an array!!
	BUMP	scratch,9
	PARSE	scratch,scratch2,"09"
	MOVE	scratch2,contentobj // content stream resource
.
. grab our contents
. 
	REPOSIT		infile,inxref(contentobj).Offset
	READ		infile,seq;*abson,scratch
	REPLACE		WS,scratch
	SCAN		"/Length",scratch
	BUMP		scratch,7
	PARSE		scratch,scratch2,"  09AZ"
	CHOP		scratch2
	TYPE		scratch2
	IF EQUAL	// actual length
	 MOVE		scratch2,contentlen
	ELSE		// resource contains length
	 PARSENUM 	scratch2,obj
	 REPOSIT	infile,inxref(obj).Offset
	 READ		infile,seq;*abson,scratch2
	 REPLACE	ws,scratch2
	 SCAN		"obj",scratch2
	 BUMP		scratch2,3
	 PARSENUM	scratch2,contentlen
	 REPOSIT	infile,inxref(contentobj).Offset
	 READ		infile,seq;*abson,scratch
	ENDIF
.   get the filter type if applicable
	RESET		scratch
	SCAN		"/Filter",scratch
	IF EQUAL
	 BUMP		scratch,7
	 PARSE		scratch,filter,"//AZaz"
	ENDIF
.
	DMAKE		content,(contentlen+2) // allow a little fluff
	SCAN		">>",scratch
	SCAN		"stream",scratch
	BUMP		scratch,6
	MOVEFPTR	scratch,cntr
	REPOSIT		infile,( inxref(contentobj).Offset + cntr )
	READ		infile,seq;*ABSON,content;
.
. We now have our contents, go back to the page dictionary and look for resources
.
	REPOSIT	infile,inxref(pageobj).Offset
	READ	infile,seq;*abson,scratch
	scan	"endobj",scratch
	lenset	scratch
	reset	scratch
	REPLACE	WS,scratch
.
	SCAN	"/Resources",scratch
	SCAN	"<<",scratch
	BUMP	scratch,2
	MOVEFPTR	scratch,resStart
.
. Get list of fonts
. 
	SCAN	"/Font",scratch
	IF EQUAL  // can be a dictionary or resources
	 BUMP	scratch,5
	 PARSE	scratch,scratch2,"09<<"
	 IF	(scratch2="<<") // dictionary
	  EXPLODE	scratch,">",fontres
	 ELSE
	  TYPE	scratch2
	  IF EQUAL	// resource
..
	    MOVE	scratch2,obj
	    REPOSIT	infile,inxref(obj).Offset
	    READ	infile,seq;*abson,scratch
	    SCAN	"<<",scratch
	    bump	scratch,2
	    EXPLODE	scratch,">",fontres
.
	    REPOSIT	infile,inxref(pageobj).Offset
	    READ	infile,seq;*abson,scratch
	    scan	"endobj",scratch
	    lenset	scratch
	    reset	scratch,resStart
	    REPLACE	WS,scratch
.
	  ENDIF
	 ENDIF
	ENDIF
.
. get list of xobjects ( images )
.
	RESET	scratch,resStart
	SCAN	"/XObject",scratch
	IF EQUAL
	 BUMP	scratch,8
	 PARSE	scratch,scratch2,"09<<"
	 IF	(scratch2="<<") // dictionary
	  EXPLODE	scratch,">",xobjres
	 ELSE
	  TYPE	scratch2
	  IF EQUAL	// resource
..
	    MOVE	scratch2,obj
	    REPOSIT	infile,inxref(obj).Offset
	    READ	infile,seq;*abson,scratch
	    SCAN	"<<",scratch
	    BUMP	scratch,2
	    EXPLODE	scratch,">",xobjres
.
	  ENDIF
	 ENDIF
	ENDIF
.
. We are finished parsing the necessary information from the page dictionary
.  now we need to parse resource lists and copy the relivant objects.
.
	replace	ws,fontres
	replace	ws,xobjres
.
. copy objects
.	
	move	"1",cntr	
	loop
	 loadadr	reslist,cntr,fontres,xobjres
	 loadadr	newres,cntr,newfres,newxres
	loop
	 scan		"/",reslist
	 break if not equal
	 parse		reslist,resname,"//AZaz09"
	 break if EOS
	 break if not EQUAL
	 fsave
	 parsenum	reslist,obj
.
	 REPOSIT	infile,inxref(obj).Offset
	 READ		infile,seq;*abson,buffer
	 SCAN		"endobj",buffer
	 bump		buffer,6
	 lenset		buffer
	 reset		buffer
.
. font descriptor and file... handle non-basefonts
.    check for /FontDescriptor
.     if exists 
.       check descriptor for /FontFile /FontFile2 or /FontFile3
.       
	if (cntr=1) // font resource
	  replace	ws,buffer
	  SCAN		"/FontDescriptor",buffer
	  IF EQUAL
	   MOVEFPTR	buffer,startxref
	   BUMP		buffer,15
	   PARSE	buffer,scratch2,"<<[[AZaz09"
	   TYPE		scratch2
	   IF EQUAL  // another object to copy
	    MOVE	scratch2,obj	// obj to copy
	    RESET	buffer,startxref
	    BUMP	buffer,16
.
	    INCR	objnum
	    SQUEEZE	objnum,scratch2
	    PACK	scratch2,scratch2," "
	    SPLICE	scratch2,buffer,2	// replace with new resource number
	    RESET	buffer
.
.	get the font descriptor object
.
	    REPOSIT	infile,inxref(obj).Offset
	    READ	infile,seq;*abson,scratch
	    SCAN	"endobj",scratch
	    BUMP	scratch,6
	    LENSET	scratch
	    RESET	scratch
	    SCAN	" ",scratch
	    SPLICE	scratch2,scratch,0
	    CHOP	scratch,scratch  // left justify var in dim
	    scan	"stream",scratch
	    if not equal
	     replace	ws,scratch
	    else
	     reset	scratch
	    endif
.
. check for fontfile
.
	    SCAN	"/FontFile",scratch
	    IF EQUAL
	     SCAN	" ",scratch
	     MOVEFPTR	scratch,startxref
	     PARSE	scratch,scratch2,"<<AZaz09"
	     TYPE	scratch2
	     IF EQUAL
	      SET	HaveFontFile
	      MOVE	scratch2,obj
	      INCR	objnum
	      SQUEEZE	objnum,scratch2
	      PACK	scratch2,scratch2," "
	      RESET	scratch,startxref
	      BUMP	scratch
	      SPLICE	scratch2,scratch,1
	      movelptr	scratch2,startxref
	      bump	scratch,startxref
	      loop
	       cmatch	" ",scratch
	       break if equal
	       splice	"",scratch,1
	      repeat
	      RESET	scratch
	     ENDIF
	    ENDIF
.
.      output descriptor to our pdf ( keep in mind we incramented
.      
	    FPOSIT	pdffile,xref(objnum-HaveFontFile).offset
	    CLEAR	xref(ObjNum-HaveFontFile).Gen
	    MOVE	"n",xref(ObjNum-HaveFontFile).type
	    WRITE	pdffile,seq;*ll,scratch
.
.	Get font file if applicable
.
	    IF (HaveFontFile)
	     REPOSIT	infile,inxref(obj).Offset
	     READ	infile,seq;*abson,scratch
	     SCAN	"endobj",scratch
	     BUMP	scratch,6
	     LENSET	scratch
	     RESET	scratch
.    scratch2 contains our object number
	     SPLICE	scratch2,scratch,2
.    /Length can be a resource identifier so we need to resolve that too
.    
	     SCAN	"/Length ",scratch
	     BUMP	scratch,8
	     MOVEFPTR	scratch,startxref
	     PARSE	scratch,scratch2,"09  RR"
	     OCCURS	"R",scratch2,startobj
	     IF (startobj)
	      movelptr	scratch2,endobj
	      PARSENUM	scratch2,obj
	      reposit	infile,inxref(obj).offset
	      read	infile,seq;*abson,scratch2
	      scan	"endobj",scratch2
	      bump	scratch2,seq
	      lenset	scratch2
	      reset	scratch2
	      scan	"obj",scratch2
	      bump	scratch2,3
	      replace	ws,scratch2
	      chop	scratch2,scratch2
.
	      reset	scratch,startxref
	      splice	scratch2,scratch,endobj
	      reset	scratch
	     ENDIF

	     FPOSIT	pdffile,xref(objnum).offset
	     CLEAR	xref(ObjNum).Gen
	     MOVE	"n",xref(ObjNum).type
	     WRITE	pdffile,seq;*ll,scratch
	    
	    ENDIF
	   ENDIF
	   RESET	buffer
	  ENDIF
	ENDIF
.
. image may need /ColorSpace
.
	IF (cntr=2)
	  SCAN		"/ColorSpace",buffer
	  IF EQUAL
	   MOVEFPTR	buffer,startxref
	   BUMP		buffer,11
	   PARSE	buffer,scratch2,"<<[[AZaz09"
	   TYPE		scratch2
	   IF EQUAL  // another object to copy
	    MOVE	scratch2,obj	// obj to copy
	    RESET	buffer,startxref
	    BUMP	buffer,12
.
	    INCR	objnum
	    SQUEEZE	objnum,scratch2
	    SPLICE	scratch2,buffer,2	// replace with new resource number
	    RESET	buffer
.
.	get the colorspace object
.
	    REPOSIT	infile,inxref(obj).Offset
	    READ	infile,seq;*abson,scratch
	    SCAN	"endobj",scratch
	    BUMP	scratch,6
	    LENSET	scratch
	    RESET	scratch
	    SCAN	" ",scratch
.
.      output to our pdf
.      
	    FPOSIT	pdffile,xref(objnum).offset
	    CLEAR	xref(ObjNum).Gen
	    MOVE	"n",xref(ObjNum).type
	    WRITE	pdffile,seq;*ll,scratch2,*ll,scratch
.
	   ENDIF
	   RESET	buffer
	  ENDIF
	ENDIF
.
	 reset		buffer
	 scan		" ",buffer	// skip over old object number
.
	 INCR		objnum	// local object number
	 FPOSIT		pdffile,xref(objnum).offset
	 CLEAR		xref(ObjNum).Gen
	 MOVE		"n",xref(ObjNum).type
	 squeeze	objnum,scratch2
.
	 WRITE		pdffile,seq;*ll,scratch2,*ll,buffer
	 PACK		newres,newres,resname," ",scratch2," 0 R"
.
	 frestore
	 break if less
	repeat
	incr		cntr
	break if (cntr>2)
	repeat
.
. output the actual form XObject
.
	INCR		objnum	// local object number
	SQUEEZE		objnum,scratch2
.
	FPOSIT		pdffile,xref(objnum).offset
	CLEAR		xref(ObjNum).Gen
	MOVE		"n",xref(ObjNum).type
.
	WRITE	pdffile,seq;*ll,scratch2," 0 obj"
	WRITE	pdffile,seq;"<<"
	WRITE	pdffile,seq;"  /Type /XObject"
	WRITE	pdffile,seq;"  /Subtype /Form"
	WRITE	pdffile,seq;"  /FormType 1"
	WRITE	pdffile,seq;"  /BBox [",bbox,"]"
	WRITE	pdffile,seq;"  /Resources <<"
	IF (newfres!="")
	WRITE	pdffile,seq;"               /Font <<",*ll,newfres,">>"
	endif
	IF (newxres!="")
	WRITE	pdffile,seq;"               /XObject <<",*ll,newxres,">>"
	endif
	WRITE	pdffile,seq;"             >>"
	WRITE	pdffile,seq;"  /Length ",contentlen
	IF (filter!="")
	WRITE	pdffile,seq;"  /Filter ",*ll,filter
	ENDIF
	WRITE	pdffile,seq;">>"
	WRITE	pdffile,seq;"stream"
	WRITE	pdffile,seq;*abson,*ll,content
	WRITE	pdffile,seq;"endstream"
	WRITE	pdffile,seq;"endobj"
.
	incr	cachedforms
	move	inPDF,FormCache(cachedforms).pdf
	move	scratch2,FormCache(cachedforms).resnum
ContentOut
	type	page(curpage).imgres
	if EOS
	MOVE	" ",page(curpage).imgres
	endif
    Append "/PDFForm",page(curpage).imgres
    append scratch2,page(curpage).imgres
    append " ",page(curpage).imgres
    append scratch2,page(curpage).imgres
    append " 0 R ",page(curpage).imgres
.
	type	page(curpage).content
	if EOS
	MOVE	" ",page(curpage).content
	endif
	append	"/PDFForm",page(curpage).content
	append	scratch2,page(curpage).content
	append	" Do",page(curpage).content
	append	crlf,page(curpage).content

	SETFLAG	EQUAL

        FUNCTIONEND

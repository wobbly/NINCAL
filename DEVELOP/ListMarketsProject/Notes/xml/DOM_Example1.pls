*************************************************************************************
*                           PL/B XMLDOM Sample
*************************************************************************************
. Author    : Ross Wells
. Revision  : 1.0
. Date      : 11th October 2000
.
. Documented/Modified by ASH May 31, 2006
*************************************************************************************
. sample node types
$node_element	const "1"
$node_attribute	const "2"
$node_text 	const "3"
.
xmldoc	  	automation          class="Microsoft.XMLDOM"
root      	automation
nodelist  	automation (99)     .This is a dangerous assumption
thisnode  	automation
parent    	automation
reply     	dim       1
f1        	form      1
f2              form      2
d2              dim       2
d80             dim       80
fileid    	dim       80
result    	form      1
nodetype  	form      1
nodeval         dim       250
parname   	dim       250
index     	form      6(99)
count     	form      6(99)
level     	form      2
record          dim       1024
.
xtype           form      "17"
xpath           dim       80
xfilt           init      ",XML file(s) (*.xml),*.XML"
.
xmlfm           plform    DOM_Example
*************************************************************************************
*         Mainline
*************************************************************************************
	winhide
.
	trap	noxml if object
		create xmldoc
	trapclr	object
	//S$CMDLIN is a 128 byte DIM for SUNDB86x or a 250 byte DIM for PLBCMP
	//from which any data following the program name entered on the command
	//line is placed.  It is null unless data followed the program name
	//entered on the command line.
	//
	//Essentially, we are allowing the XML to be determined when
	//the program is run via a command line.
	parse	s$cmdlin into fileid using " ~"
	count	f2,fileid
	call	getfile if equal
	goto exit if over
.
	formload xmlfm
.
mainproc
	move	"1",level
	setitem	DOM_st_sbtext,0,fileid
	deleteitem DOM_dl_text,0
.
	//The Default for *async property is '1' which means that the loading
	//of the XML into the XMLDOC object is independent of all other instructions.
	//If we keep this property set to it's default value, we will allow
	//instructions to be performed without assurance that the XML has completely
	//loaded - which is apt to cause an "object not found" error when attempting
	//to access nodes not yet created.
	setprop	xmldoc,*async=0
	//Following instruction LOADS XML file into XMLDOM - a parser
	xmldoc.load giving result using fileid

	//Original code from Ross Wells is incorrect:  if (result2 <> 1)
	//result is actually a Boolean return value.  Boolean values have the following
	//integer representation:
	// False = "0"
	// True  = anything but "0", usually "-1"
	if (result = 0)
		alert	stop,"XMLDOM Load Failed",f1,"XMLDOM Error"
		goto exit
	endif
.
	//Get Root
	getprop	xmldoc,*documentElement=root
	//Get Collection of child elements from "root"
	getprop	root,*childNodes=nodelist(level)
	call	procxml
.
	setitem	DOM_dl_text,0,1
	setfocus DOM_dl_text
	//Allows multiple calls in order to open/view multiple XML files
	loop
		eventwait
	repeat
.
exit
	destroy	xmldoc
	stop
.
noxml
	alert	stop,"XMLDOM is not installed",f1,"XMLDOM Error"
	stop
*************************************************************************************
*         Procxml - walk through XML tree - Beware, there is RECURSION here!!
*************************************************************************************
procxml
	getprop	nodelist(level),*length=count(level)
	for index(level),"0",(count(level)-1)
		nodelist(level).item giving thisnode using index(level)
		getprop	thisnode,*nodeType=nodetype
		//Note:  $node_text are the only nodes that contain data
		//which can be displayed.  These nodes sit directly under
		//the node that refers to the tag name.
		//For the following tag:
		//<Data>Some Data</Data>
		//You can drill down to the node that holds 'Data',
		//but in order to get to the contents, you would need to
		//go down one more level and hit the $node_text in order to
		//retrieve 'Some Data'.
		if (nodetype=$node_text)
			call	dispnode
		endif
		//Original code from Ross Wells is incorrect:  if (f1=1)
		//result is actually a Boolean return value.  Boolean values have the following
		//integer representation:
		// False = "0"
		// True  = anything but "0", usually "-1"
		getprop	thisnode,*hasChildNodes=f1
		if (f1<>0)	//Indicates TRUE - there are ChildNodes
			add	"1" to level
			if not equal
				getprop	thisnode,*childNodes=nodelist(level)
				//Egads - Recursion!!
				call	procxml 	.(down to next level in tree)
			endif
		endif
	repeat
	sub	"1",level
	return
.
*************************************************************************************
*         Dispnode - display nodetype=text
*************************************************************************************
dispnode
	getprop	thisnode,*parentNode=parent
	getprop	parent,*nodeName=parname
	getprop	thisnode,*nodeValue=nodeval
	move	level,d2
	rep	" 0",d2
	pack	record,"(",d2,") ",parname," : ",nodeval
	insertitem DOM_dl_text,99999,record
	return
*************************************************************************************
*         Getfile - get XML file to process
*************************************************************************************
getfile
	move	" ",d80
	clear	xpath
	getfname type=xtype,"Select XML file to open":
	   	d80,xpath,xfilt
	pack	fileid,xpath,d80
	return
*************************************************************************************
*************************************************************************************
*                           PL/B XMLDOM Sample
*************************************************************************************
. Author    : Andrew Harkins
. Revision  : 1.0
. Date      : May 31, 2006
. Notes     : Spawned from code that Ross Wells wrote in 2000.  This variation
.             illustrates some specific DOM properties/methods
.
*************************************************************************************
. sample node types
$node_element	const "1"
$node_attribute	const "2"
$node_text 	const "3"
.
xmldoc	  	automation	class="Microsoft.XMLDOM"
root      	automation
nodelist  	automation (99)	.This is a dangerous assumption
thisnode  	automation
nextnode	automation
parent    	automation
newnode		automation
attributescol	automation
VT_I4 EQU 3
XMLError variant
tester integer 4,"0x00000000"

result        	form      9
result2        	form      9
result3        	form      9
howmany        	form      2
str2            dim       2
str25		dim	  25
str45		dim	  45
str80           dim       80
fileid    	dim       80
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
NewLine		Init    0x7F
taskname	dim	500
colordim	dim  8
.
xmlfm           plform    DOM_Example2
*************************************************************************************
*         Mainline
*************************************************************************************
	winhide
.
	create  XMLError,VarType=VT_I4,VarValue=0
.
	trap	noxml if object
		create	xmldoc
	trapclr	object

	//Hard-coding the XML file we are working with
	move	"f:\library\develop\frontline\test.XML",fileid
.
	formload xmlfm

	DOM_ListView.InsertColumn using "Tree Node",50,0
	DOM_ListView.InsertColumn using "Level",50,1
	DOM_ListView.InsertColumn using "Element Node",100,2
	DOM_ListView.InsertColumn using "Text Node Value",150,3
	DOM_ListView.InsertColumn using "Attribute Node",100,4
	DOM_ListView.InsertColumn using "Attribute Node Value",150,5
	DOM_ListView.InsertColumnBgClr using *Index=6
.
mainproc
	move	"1",level
	setitem	DOM_st_sbtext,0,fileid
.
	//The Default for *async property is '1' which means that the loading
	//of the XML into the XMLDOC object is independent of all other instructions.
	//If we keep this property set to it's default value, we will allow
	//instructions to be performed without assurance that the XML has completely
	//loaded - which is apt to cause an "object not found" error when attempting
	//to access nodes not yet created.
	setprop	xmldoc,*async=0

	//ReadyState lets us know if loading has completed.  This property is actually
	//useless with *async set to "0", but I include it for information-sake
	getprop	xmldoc,*ReadyState=result
	//*ReadyState values are as follows:
	// "0" - Uninitialized: loading has not started.
	// "1" - Loading: while the load method is executing.
	// "2" - Loaded: load method is complete.
	// "3" - Interactive: enough of the DOM is available for read-only examination and the data has only been partially parsed.
	// "4" - Completed: data is loaded and parsed and available for read/write operations.

	//Following instruction LOADS XML file into XMLDOM - a parser
	xmldoc.load giving result using fileid
	//We could have the load method validate against a Schema file (or DTD),
	//but we are foregoing this bit.

	//Original code from Ross Wells is incorrect:  if (result <> 1)
	//result is actually a Boolean return value.  Boolean values have the following
	//integer representation:
	// False = "0"
	// True  = anything but "0", usually "-1"
	if (result = 0)
		alert	stop,"XMLDOM Load Failed",result,"XMLDOM Error"
		goto exit
	endif

	//Validate the XML file
validate
	xmldoc.Validate giving XMLError
	getprop	XMLError,VarValue=tester
.
	//Get Root
	getprop	xmldoc,*documentElement=root
	//Get Collection of child elements from "root"
	getprop	root,*childNodes=nodelist(level)
	call	procxml
.
	setfocus DOM_ListView
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
	alert	stop,"XMLDOM is not installed",result,"XMLDOM Error"
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
		//Original code from Ross Wells is incorrect:  if (result = 1)
		//result is actually a Boolean return value.  Boolean values have the following
		//integer representation:
		// False = "0"
		// True  = anything but "0", usually "-1"
		getprop	thisnode,*hasChildNodes=result
		if (result <> 0)	//Indicates TRUE - there are ChildNodes
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
	add	"1",howmany
	getprop	thisnode,*parentNode=parent
	getprop	parent,*nodeName=parname
	getprop	thisnode,*nodeValue=nodeval
	move	howmany,str2
	rep	" 0",str2
	DOM_ListView.InsertItem giving result2 using str2
	move	level,str2
	rep	" 0",str2
	DOM_ListView.SetItemText using result2,str2,1
	DOM_ListView.SetItemText using result2,parname,2
	DOM_ListView.SetItemText using result2,nodeval,3
	DOM_ListView.SetItemText using result2,colordim,6
	return

GetNewLCRs
	DOM_ListView.DeleteAllItems
	return
InsertNewLCR
	clear	taskname
	append	"Inserting a tag in a new LCR, after following values:",taskname
	append	newline,taskname
	append	"Account.List: Best Friends Animal Society",taskname
	append	newline,taskname
	append	"Added Tag Value:",taskname
	append	newline,taskname
	append	"<NewElement>Here is a new Inserted Element</NewElement>",taskname
	reset	taskname
	alert	note,taskname,result
.Node creation
	//Create new node element (tag)
	xmldoc.createElement giving newnode using "NewElement"

	//Create new node text (actually the child to above)
	xmldoc.createTextNode giving nextnode using "Here is a new Inserted Element"

	//Append/associate text node with element node
	newnode.appendChild using nextnode
.Find where we will place new node
	move	"NewLCRRecord",taskname
	xmldoc.getElementsByTagName giving parent using taskname
	//getElementsByTagName returns a COLLECTION of nodes that match the value indicated!!

	move	"0",result
	parent.item giving thisnode using result
	//We are now positioned on the first instance of the tag <NewLCRRecord>

	//Test value
	getprop	thisnode,*text=str25

	//Go to the first child - <Account> - this will be the parent of our new node
	getprop	thisnode,*firstchild=parent

	//Go to the next child - <List>
	getprop	parent,*firstchild=thisnode

	//Go to sibling of <List> - <Mailer>
	getprop	thisnode,*nextSibling=nextnode

	//Insert new node before <Mailer>
	parent.InsertBefore using newnode,nextnode
.
	getprop	xmldoc,*documentElement=root
	//Get Collection of child elements from "root"
	move	"1",level
	getprop	root,*childNodes=nodelist(level)
	DOM_ListView.DeleteAllItems
	call	procxml
	return

RemoveNewLCR
	pack	taskname,"Removing <NewElement>"
	alert	note,taskname,result
.Find new node to delete
	move	"NewElement",taskname
	xmldoc.getElementsByTagName giving parent using taskname
	//getElementsByTagName returns a COLLECTION of nodes that match the value indicated!!

	move	"0",result
	parent.item giving thisnode using result

	//Need to go up to the parent node of thisnode
	getprop	thisnode,*parentNode=parent

	parent.removeChild using thisnode
.
	getprop	xmldoc,*documentElement=root
	//Get Collection of child elements from "root"
	move	"1",level
	getprop	root,*childNodes=nodelist(level)
	DOM_ListView.DeleteAllItems
	call	procxml
	return
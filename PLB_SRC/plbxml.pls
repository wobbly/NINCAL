*************************************************************************************
*                           PL/B XMLDOM Sample
*************************************************************************************
. Author    : Ross Wells
. Revision  : 1.0
. Date      : 11th October 2000
*************************************************************************************
. sample node types
$node_element	const "1"
$node_attribute	const "2"
$node_text 	const "3"
.
xmldoc	  	automation          class="Microsoft.XMLDOM"
root      	automation
nodelist  	automation (99)
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
xmlfm           plform    plbxml
*************************************************************************************
*         Mainline
*************************************************************************************
          winhide
          trap noxml if object
            create xmldoc
          trapclr object
          parse s$cmdlin into fileid using " ~"
          count f2 from fileid
          call getfile if equal
          goto exit if over
.
          formload xmlfm
.
mainproc
          move "1" to level
          setitem plbxml_st_sbtext,0,fileid
          deleteitem plbxml_dl_text,0
.
          setprop xmldoc,*async=0
          xmldoc.load giving result using fileid
          if (result<>1)
             alert stop,"XMLDOM Load Failed",f1,"XMLDOM Error"
             goto exit
          endif
.
          getprop xmldoc,*documentElement=root
          getprop root,*childNodes=nodelist(level)
          call procxml 
.
          setitem plbxml_dl_text,0,1
          setfocus plbxml_dl_text
          loop
            eventwait
          repeat
.
exit      destroy   xmldoc
          stop
.
noxml     alert stop,"XMLDOM is not installed",f1,"XMLDOM Error"
          stop
*************************************************************************************
*         Procxml - walk through XML tree
*************************************************************************************
procxml
          getprop nodelist(level),*length=count(level)
          for index(level) from "0" to (count(level)-1)
            nodelist(level).item giving thisnode using index(level)
            getprop thisnode,*nodeType=nodetype
            if (nodetype=$node_text)
              call dispnode
            endif
            getprop thisnode,*hasChildNodes=f1
            if (f1=1)
              add "1" to level
              if not equal
                getprop thisnode,*childNodes=nodelist(level)
                call procxml (down to next level in tree)
              endif
            endif
          repeat
          sub "1" from level
          return
.
*************************************************************************************
*         Dispnode - display nodetype=text 
*************************************************************************************
dispnode
          getprop thisnode,*parentNode=parent
          getprop parent,*nodeName=parname
          getprop thisnode,*nodeValue=nodeval
          move level to d2
          rep " 0" in d2
          pack record from "(",d2,") ",parname," : ",nodeval
          insertitem plbxml_dl_text,99999,record
          return
*************************************************************************************
*         Getfile - get XML file to process
*************************************************************************************
getfile
          move " " to d80
          clear xpath
          getfname type=xtype,"Select XML file to open":
                   d80,xpath,xfilt
          pack fileid from xpath,d80
          return
*************************************************************************************


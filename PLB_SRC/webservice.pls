. A generic web serivices loadmod.
.         Copyright 2009 Sunbelt Computer Systems, Inc.
.         By Matther Lake
. 
. Requires seperately compiled sockets loadmod. ( socket.pls )
.
. This module is implamented using FUNCTION.  Calling syntax is as follows
.
.         CALL      function GIVING output USING input1, input2
.
. test code belose illustrates all but he CALLSERVICE function.
.
.  function                   input                                   output
.
. SETSOAPENDPOINT   URL to service end point.
. GETFUNCTIONNAMES                                          delimited list of functions
. GETINPUTTEMPLATE  function name                           xml template of input parameters
. GETOUTPUTTEMPLATE function name                           xml template of output parameters
. CALLSERVICE                 function name, xml input data xml data of output
........................
...... test code .......
........................
service1  INIT      "http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php"
service2  INIT      "http://www.webservicex.net/geoipservice.asmx"
service3  INIT      "http://www.webservicex.net/genericbarcode.asmx"
service4  init      "http://www.webservicex.net/mortgage.asmx"
...... 
testdata  DIM       40000 // know your service... may require more or less than this!!!
FunctionName        DIM       80

Edt                 EDITTEXT
Edt2                EDITTEXT
Edt3                EDITTEXT
btn                 BUTTON
cbo                 COMBOBOX
cto7f               INIT      ",",0x7f

          CREATE              cbo=1:1:1:40,"Select Function","",rowsvisible=4
          DISPLAY             *v=2,"Input",*H=41,"Output";
          CREATE              Edt=3:20:1:40,MULTILINE=999
          CREATE              Edt2=3:20:41:80,MULTILINE=999
          CREATE              Edt3=21:25:1:80,MULTILINE=999
          ACTIVATE  Edt
          ACTIVATE  Edt2
          ACTIVATE  Edt3
          SETPROP             Edt,TEXT=testdata

          CALL                SETSOAPENDPOINT using service4
          CALL                GETFUNCTIONNAMES giving testdata

          REPLACE             cto7f,testdata
          INSERTITEM          cbo,0,testdata
          EVENTREG  cbo,"4",cboselect //$CLICK
          ACTIVATE  cbo
          
          LOOP
            EVENTWAIT
          REPEAT

cboselect
fm4       FORM      4
          cbo.GetCurSel giving fm4
          cbo.GetText giving FunctionName using fm4

          CALL                GETINPUTTEMPLATE giving testdata using FunctionName
          SETPROP             edt,TEXT=testdata
          CREATE              btn=18:20:36:40,"click":
                                        tooltip="if you fill in the input by hand, click here for result":
                                        zorder=999
          ACTIVATE  btn
          EVENTREG  btn,"4",btnclick //$CLICK
          
          CALL                GETOUTPUTTEMPLATE giving testdata using FunctionName
          SETPROP             edt2,TEXT=testdata
          RETURN

btnclick
fields    dim       80
field     dim       80
result    xfile
fm10      form      10
seq       form      "-1"
          GETPROP   edt,TEXT=testdata
          CALL      CALLSERVICE giving testdata using FunctionName,testdata
          setprop   edt3,text=testdata
          return



..... End test code .....
..............................................................................
connect   INIT      "socket;CONNECT" //#SOCKET,#HOST,#PORT,#MODE
send      INIT      "socket;SEND"       //#SOCKET,#MSG,#TIMEOUT
recv      INIT      "socket;RECV"       //#SOCKET,#MSG,#TIMEOUT
C         COMFILE
.SEQ      FORM      "-1"
FZERO     FORM      "0"
crlf      INIT      13,10
.
SoapEndpoint        DIM       500
wsdl                XFILE
SoapServices        XFILE
SoapBindings        XFILE
SoapPortTypes       XFILE
SoapMessages        XFILE
SoapSchema          XFILE
WsdlNameSpace       DIM       20
SchemaNameSpace     DIM       20
targetnamespace DIM 250

.
SoapHeadder         INIT      "<?xml version=#"1.0#"?>",13,10:
                              "<soap:Envelope xmlns:xsi=#"http://www.w3.org/2001/XMLSchema-instance#" xmlns:xsd=#"http://www.w3.org/2001/XMLSchema#" xmlns:soap=#"http://schemas.xmlsoap.org/soap/envelope/#">",13,10:
                              "  <soap:Body>",13,10
SoapFooter          INIT      "  </soap:Body>",13,10:
                              "</soap:Envelope>",13,10
.
. Some utility functions:
. 
..
. add item to a delimited list without duplicates
.
AddToList LFUNCTION
Item      DIM       ^
List      DIM       ^
          ENTRY
ListItem  DIM       100

          TEST      List
          GOTO      newlist IF EOS

          LOOP
            EXPLODE List,",",ListItem
            BREAK if (Item=ListItem)
          REPEAT UNTIL ZERO
          IF (Item!=ListItem)
            ENDSET  List
            APPEND  ",",List
newlist
            APPEND  Item,List
          ENDIF
          RESET               List

          FUNCTIONEND
.
.SOAP xml parser..namespace treated as part of tag name.
.
. FixUpXML changes namespace elements in the xml to be compatible with
.         Sunbelts XFILE
FixUpXML  LFUNCTION
filename  DIM       250
          ENTRY
Chars     INIT      "<>"
INFILE    FILE
flsize    FORM      6
buffer    DIM       ^
buffer2   DIM       ^
data      DIM       ^
TAG       DIM       500
istag     FORM      1
delim     DIM       1
done      FORM      1

          FINDFILE  filename,filesize=flsize
          DMAKE               buffer,flsize
          DMAKE               buffer2,flsize
          DMAKE               data,flsize
          OPEN                infile,filename
          READ                infile,seq;*abson,buffer;
          CLOSE               infile
          CLEAR               done
          SCAN                "<?",buffer         ; advance to start of XML tag
          LOOP
            CMATCH  "<",buffer
            IF EQUAL
              SET             istag
              BUMP  buffer
            ELSE
              CLEAR           istag
            ENDIF
.
            EXPLODE buffer,Chars,data
            IF ZERO
              SET             done
            ENDIF
.           
            IF      (istag)
              CALL  ParseTag using data
              APPEND          "<",buffer2
              APPEND          data,buffer2
              APPEND          ">",buffer2
            ELSE
              APPEND          data,buffer2
              BUMP  buffer,SEQ
            ENDIF
.
          REPEAT until (done)
.
          RESET     buffer2
          FUNCTIONEND using buffer2
  
PARSETAG  LFUNCTION
data      dim       ^
          ENTRY
ISQUOTED  FORM      1
          LOOP
            CMATCH  """,data
            IF EQUAL
              IF (ISQUOTED)
                CLEAR         ISQUOTED
              ELSE
                SET ISQUOTED
              ENDIF
            ENDIF
            IF (ISQUOTED=0)
            CMATCH  ":",data
            IF EQUAL
              CMOVE "_",data
            endif
            ENDIF
            BUMP              data
          REPEAT UNTIL EOS
          RESET               data
          FUNCTIONEND
.
.  Before we can internally generate a template for a given web service, we
. need to get the WSDL for the server.  This is located at the %ENDPOINT?wsdl
. and retrieved via a HTTP GET request.
.
SETSOAPENDPOINT FUNCTION 
Endpoint  DIM       500
          ENTRY
scratch             FILE
buffer              DIM       1000
Hostname  DIM       80
location  DIM       420
xmlbuff             DIM       ^
d1                  DIM       1
tag                 DIM       80
roottag             DIM       80
tm                  FORM      "1"
filename  INIT      "$TEMP\wsdl-temp.txt"
flsize              FORM      10
types               XFILE
          MOVE                Endpoint,SoapEndpoint
          MOVE                SoapEndpoint,buffer
          UPPERCASE buffer
          MATCH               "HTTP://",buffer
          RETURN IF NOT EQUAL // we are only going to support http based services.
.
. break out host and location needed for http request.
.
          BUMP                SoapEndpoint,7
          EXPLODE             SoapEndpoint,"/",hostname
          RESET               SoapEndpoint
          PACK                location,SoapEndpoint,"?wsdl"
.
. build the http request
.
          PACK                buffer,"GET ",location," HTTP/1.1",crlf:
                                        "Host: ",hostname,crlf:
                                        "Connection: Close",crlf,crlf
.debug
.         goto skip

.
. get the data
.
          PREP                scratch,filename,exclusive
          WEOF                scratch,seq
          CALLS               connect using c,hostname,"80","R"
          RETURN IF OVER // connect failed
.
          CALLS               send using c,buffer,tm
.
          LOOP
            CALLS             recv using c,buffer,tm
            UNTIL OVER
            CONTINUE IF ZERO
            WRITE             scratch,seq;*ll,buffer;
          REPEAT
          IF (LESS AND EOS)
            CALLS             recv using c,buffer,tm
            WRITE             scratch,seq;*ll,buffer;
          ENDIF       
 
          WEOF                scratch,seq
          CLOSE               scratch
skip
.
          FINDFILE  filename,filesize=flsize
          DMAKE               xmlbuff,flsize
. fixup wsdl data
          CALL                FixUpXML giving xmlbuff using "$TEMP\wsdl-temp.txt"
. load wsdl into an XFILE
          OPEN                wsdl,d1,DATA=xmlbuff
.
. since we cannot get root level attributes, parese them off by hand 
. to figure out what our namespace is.
. 
          SCAN                "?>",xmlbuff
          SCAN                "<",xmlbuff
          BUMP                xmlbuff
...
          PARSE               xmlbuff,WsdlNameSpace,"azAZ__09--.."
          WHEREIS             "_",WsdlNameSpace,flsize
          IF NOT ZERO
            setlptr WsdlNameSpace,flsize
          ELSE
            CLEAR             WsdlNameSpace
          ENDIF
.
. get the soap message target name space
.
          SCAN                "targetNamespace=",xmlbuff
          BUMP                xmlbuff,17
          MOVE                xmlbuff,targetnamespace
          SCAN                "#"",targetnamespace
          BUMP                targetnamespace,seq
          LENSET              targetnamespace
          RESET               targetnamespace
.
. Get the various wsdl record sets
.
          PACK                tag,WsdlNameSpace,"service"
          READ                wsdl,seq;@tag=SoapServices;
          PACK                tag,WsdlNameSpace,"binding"
          READ                wsdl,seq;@tag=SoapBindings;
          PACK                tag,WsdlNameSpace,"portType"
          READ                wsdl,seq;@tag=SoapPortTypes;
          PACK                tag,WsdlNameSpace,"message"
          READ                wsdl,seq;@tag=SoapMessages;
.
. get the schema name space and record set
.
          PACK                tag,WsdlNameSpace,"types"
          READ                wsdl,seq;@tag=types;
          READ                types,SEQ;;
          GETFILE             types,CURFIELDS=buffer
          EXPLODE             buffer,",",SchemaNameSpace
.
          WHEREIS             "_",SchemaNameSpace,flsize
          IF NOT ZERO
            SETLPTR SchemaNameSpace,flsize
          ELSE
            CLEAR             SchemaNameSpace
          ENDIF
.
          PACK      tag,SchemaNameSpace,"schema"
          READ      types,seq;@tag=SoapSchema

                FUNCTIONEND
.
. Get a delimited list of services
.   Basically, get the NS_service record "name" attributes
GETFUNCTIONNAMES FUNCTION
          ENTRY
BindingName         DIM       80
BindingFilter       DIM       100
FunctionNameList    DIM       1000
.
portTAG             DIM       80
soapports XFILE
operations          XFILE
OperationTag        DIM       80
FunctionName        DIM       80
.
. Get the Service Ports
          PACK                portTAG,WsdlNameSpace,"port"
          REPOSIT             SoapServices,fZERO
          READ                SoapServices,seq;@porttag=soapports
.
. Loop though the ports
          LOOP
.Get the binding for each port
            READ              SoapPorts,SEQ;binding=BindingName
            UNTIL             OVER
            REPOSIT SoapBindings,fZERO
. remove namespace place holder if any
            SCAN              ":",BindingName
            IF EQUAL
              BUMP  BindingName
            ENDIF
. Get the operations available on this binding
            PACK              BindingFilter,"name='",BindingName,"'"
            PACK              OperationTag,WsdlNameSpace,"operation"
            READ              SoapBindings,BindingFilter;@OperationTag=operations
            LOOP
              READ  operations,seq;name=FunctionName
              UNTIL OVER
              CALL  AddToList using FunctionName,FunctionNamelist
            REPEAT
. Next Port
          REPEAT
.
                FUNCTIONEND using FunctionNameList
.
. some quick helper functions
. 
GETINPUTTEMPLATE FUNCTION
Function  DIM       80
          ENTRY
ParameterList       DIM       4000
          CALL      GetParameters giving ParameterList using Function,"input"
          FUNCTIONEND using ParameterList
.
GETOUTPUTTEMPLATE FUNCTION
Function  DIM       80
          ENTRY
ParameterList       DIM       4000
          CALL      GetParameters giving ParameterList using Function,"output"
          FUNCTIONEND using ParameterList
.
GetParameters FUNCTION
Function  DIM       80
parametertype       DIM       7
          ENTRY
ParameterList       DIM       4000
BindingName         DIM       80
BindingFilter       DIM       100
portTAG             DIM       80
soapports XFILE
operations          XFILE
PortKey             DIM       100
InputKey  DIM       100
inputs              XFILE
OperationFilter     DIM       80
OperationTag        DIM       80
FunctionName        DIM       80
messagetype         DIM       80
messageKey          DIM       80
parts               XFILE
PartTag             DIM       80
portType  DIM       80
schemapart          XFILE
complexType         XFILE
stag                DIM       80
partname  DIM       80
elementName         DIM       80
typeName  DIM       80
nestparams          DIM       2000

.
. Get the Service Ports
          PACK                portTAG,WsdlNameSpace,"port"
          REPOSIT             SoapServices,fZERO
          READ                SoapServices,seq;@porttag=soapports
.
. Loop though the ports
          LOOP
.Get the binding for each port
            READ              SoapPorts,SEQ;binding=BindingName
            UNTIL             OVER
            REPOSIT SoapBindings,fZERO
. remove namespace place holder if any
            SCAN              ":",BindingName
            IF EQUAL
              BUMP  BindingName
            ENDIF
. Get the operations available on this binding
            PACK              BindingFilter,"name='",BindingName,"'"
            PACK              OperationTag,WsdlNameSpace,"operation"
            READ              SoapBindings,BindingFilter;@OperationTag=operations,type=portType
            PACK              OperationFilter,"name='",Function,"'"
            READ              operations,OperationFilter;;
            IF NOT OVER
              REPOSIT         SoapPortTypes,fZERO
              SCAN  ":",portType
              IF EQUAL
                BUMP          portType
              ENDIF
.
              PACK  PortKey,"name='",portType,"'"
              READ  SoapPortTypes,PortKey;@OperationTag=operations           
.
. get the message type for this operation
. 
              PACK  InputKey,WsdlNameSpace,parametertype
              READ  operations,OperationFilter;@InputKey=inputs
              READ  inputs,seq;message=messagetype
              SCAN  ":",messageType
              IF EQUAL
                BUMP          messageType
              ENDIF
              CLEAR ParameterList
....        
              PACK  messageKey,"name='",messageType,"'"
              PACK  PartTag,WsdlNameSpace,"part"
              READ  SoapMessages,messageKey;@PartTag=parts
.
. get all the parts of this message
. 
              LOOP
                READ          parts,SEQ;name=partname,element=elementName,type=typename
                UNTIL OVER
                REPLACE       ":_",typename
..          
                CALL          SchemaType giving nestparams using partname,elementname,typename
                APPEND        nestparams,ParameterList
              REPEAT
              RESET ParameterList
              BREAK  // no need to check next port since we found our data
            ENDIF
. Next Port
          REPEAT

          FUNCTIONEND using ParameterList
.
SchemaType LFUNCTION
partname  DIM       80
elementName         DIM       80
typeName  DIM       80
          ENTRY
ParameterList       DIM       2000
nestparams          DIM       2000
fields              DIM       100
seqtag              DIM       50
stag                DIM       60
key                 DIM       80
schemaSet XFILE
complexType         XFILE
simpleType          XFILE
sequence  XFILE
elements  XFILE
nsize               FORM      2
elementTag          DIM       80
subpartname         DIM       80
ftype               DIM       1
PartNameOpen        FORM      1
nkey                DIM       1
issimple  FORM      1
..
. do we have a type or element reference?
. 
          TYPE                typeName
          IF NOT EOS          //type=
....
            APPEND  "<",ParameterList
            APPEND  partname,ParameterList
            APPEND  ">",ParameterList
....
. if the type starts with the schema name space, we have a base type and
. there is no need to look it up in the schema.
. 
            MATCH   SchemaNameSpace,typename
            IF NOT EQUAL //need to look up schema reference
              SCAN  "_",typeName
              IF EQUAL
               BUMP typeName
              ENDIF
              // not going to output closeing tag until sub-parts are processed
              SET             PartNameOpen
.
              PACK  stag,SchemaNameSpace,"complexType"
              READ  SoapSchema,nkey;@stag=complexType;
              PACK  key,"name='",typename,"'"
              READ  complexType,key;;
              IF OVER
                // if we don't have a complex type, it is a simple type
                PACK          stag,SchemaNameSpace,"simpleType"
                READ          SoapSchema,nkey;@stag=simpleType;
                READ          simpleType,key;;
                IF NOT OVER
                  SET         issimple
              //ELSE
              // to-do... if not complex or simple types... I don't know what to do here
              //              because I have not encounter this situation yet.
                ENDIF
              ENDIF
            ELSE //schema base type
              // just output the type and closing tag and were done
              COUNT nsize,SchemaNameSpace
              BUMP  typeName,nsize
              APPEND          "[",ParameterList
              APPEND          typeName,ParameterList
              APPEND          "]",ParameterList
              APPEND          "</",ParameterList
              APPEND          partname,ParameterList
              APPEND          ">",ParameterList
              APPEND          crlf,ParameterList
              GOTO  done //so sue me ;)
            ENDIF
.
          ELSE // element=
          // elements need to be lookup up in the schema
            SCAN              ":",elementName
            BUMP              elementName

            PACK              stag,SchemaNameSpace,"element"
            READ              SoapSchema,seq;@stag=schemaSet;

            PACK              key,"name='",elementName,"'"
            PACK              stag,SchemaNameSpace,"complexType"
            READ              schemaSet,key;@stag=complexType
            READ              complexType,seq;;
            // I don't know if simple types can occur here... didn't see one 
            // during development
            RETURN IF OVER  //null parameter
          ENDIF
....
          IF (issimple)  
. simple type...only one encountered in testing is an enumeration
.                   so build the enumeration list
.
           GETFILE  simpleType,CURFIELDS=fields
           //find the record set name
           LOOP
             EXPLODE          fields,",",seqtag,ftype
             BREAK  if (ftype="R")
             SCAN             ";",fields
             BUMP             fields
           REPEAT
.
           READ               simpleType,seq;@seqtag=sequence
           PACK               elementTag,SchemaNameSpace,"enumeration"
           READ               sequence,seq;@elementTag=elements
           IF NOT OVER
             APPEND "[enum ",ParameterList
             LOOP
               READ           elements,seq;value=subpartname
               UNTIL OVER
               APPEND         subpartname,ParameterList
               APPEND         ":",ParameterList
             REPEAT
             BUMP             ParameterList,seq //stamp out last colon
             APPEND "]",ParameterList
           ENDIF
.
          ELSE
. complex type... get name of record set containing subtypes
           GETFILE  complexType,CURFIELDS=fields
           LOOP
             EXPLODE          fields,",",seqtag,ftype
             BREAK  if (ftype="R")
             SCAN             ";",fields
             BUMP             fields
           REPEAT
.
           READ               complexType,seq;@seqtag=sequence  
           PACK               elementTag,SchemaNameSpace,"element"
           READ               sequence,seq;@elementTag=elements
           IF NOT OVER
. get all the fields of this type...nested call in case of further sub-sets
            LOOP
              READ  elements,seq;name=subpartname,type=typename
              UNTIL OVER
..        
              REPLACE         ":_",typename
              CALL  SchemaType giving nestparams using subpartname,"",typename
              APPEND          nestparams,ParameterList
.         
            REPEAT
           ENDIF
          ENDIF
.
. output closing tag of the part we are working on
. 
          IF (PartNameOpen)
            APPEND  "</",ParameterList
            APPEND  partname,ParameterList
            APPEND  ">",ParameterList
            APPEND  crlf,ParameterList
          ENDIF
done
          RESET     ParameterList

 FUNCTIONEND using ParameterList
. call the service
. 
CALLSERVICE FUNCTION
Operation DIM       80
data                DIM       ^
          ENTRY
httpheadder         DIM       500
soapmessage         DIM       2000
buffer              DIM       2500
Hostname  DIM       80
msgsize             FORM      4
scratch             FILE
filename  INIT      "$TEMP\soap-result.txt"
flsize              FORM      10
xmlbuff             DIM       ^
c                   COMFILE
tm                  FORM      2
ActionTag DIM       100
portTAG             DIM       40
soapports XFILE
BindingName         DIM       80
OperationTag        DIM       80
BindingFilter       DIM       80
operations          XFILE
portType  DIM       80
OperationFilter     DIM       80
rstag               DIM       80
fielddesc DIM       80
fieldtype DIM       1
d1                  DIM       1
op                  XFILE
SoapNameSpace       DIM       20
responceXML         XFILE
responceXML2        XFILE
.
. now we need to figure out what our action tag will be:
.
. Get the Service Ports
          PACK                portTAG,WsdlNameSpace,"port"
          REPOSIT             SoapServices,fZERO
          READ                SoapServices,seq;@porttag=soapports
.
. Loop though the ports
          LOOP
.Get the binding for each port
            READ              SoapPorts,SEQ;binding=BindingName
            RETURN IF OVER //passed in operation not fount
.           
            REPOSIT SoapBindings,fZERO
. remove namespace place holder if any
            SCAN              ":",BindingName
            IF EQUAL
              BUMP  BindingName
            ENDIF
. Get the operations available on this binding
            PACK              BindingFilter,"name='",BindingName,"'"
            PACK              OperationTag,WsdlNameSpace,"operation"
            READ              SoapBindings,BindingFilter;@OperationTag=operations,type=portType;
            PACK              OperationFilter,"name='",Operation,"'"
            READ              operations,OperationFilter;;
. go to next binding if operation not found
            CONTINUE IF OVER

.
. since we don't know the soap namespace, look for a recordset that is not
. the wsdl name space and that should be it.
.
            GETFILE operations,CURFIELDS=buffer
            CLEAR             rstag
            LOOP
              EXPLODE         buffer,";",fielddesc
              FSAVE
              EXPLODE         fielddesc,",",rstag,fieldtype
              IF              (fieldtype="R")
              MATCH wsdlNameSpace,rstag
              BREAK IF NOT EQUAL // soap parameter in different namespace
              ENDIF
              FRESTORE
            REPEAT UNTIL ZERO
            RETURN IF ZERO
.
. now get the soap operation action tag needed to execute the service
. 
            READ              operations,d1;@rstag=op
            READ              op,seq;soapAction=ActionTag
            BREAK
          REPEAT
.
. assemble our HTTP message
.
          BUMP                SoapEndpoint,7
          EXPLODE             SoapEndpoint,"/",Hostname
          RESET               SoapEndpoint
.
          PACK                httpheadder,"POST ",SoapEndpoint," HTTP/1.1",crlf:
                                        "Host: ",Hostname,crlf:
                                        "Connection: Close",crlf:
                                        "SOAPAction: #"",ActionTag,"#"",crlf:
                                        "Content-Type: text/xml",crlf:
                                        "Content-Length: "

          PACK                soapmessage,SoapHeadder:
                              "    <",Operation," xmlns=#"",targetnamespace,"#">",crlf:
                              "      ",data,crlf:
                              "    </",Operation,">",crlf:
                              SoapFooter

          Count               msgsize,SoapMessage
          PACK                buffer,httpheadder,msgsize,crlf,crlf,soapmessage

.
. create scratch file to hold results
.
          PREP                scratch,filename,exclusive
          WEOF                scratch,seq
.
          CALLS               connect using c,hostname,"80","R"
          RETURN IF OVER // connect failed
.
          CALLS               send using c,buffer,tm
.
          LOOP
            CALLS             recv using c,buffer,tm
            UNTIL OVER
            CONTINUE IF ZERO
            WRITE             scratch,seq;*ll,buffer;
          REPEAT
          WEOF                scratch,seq
.
. load the result into a buffer
.
          FPOSIT              scratch,flsize
          DMAKE               xmlbuff,flsize
          CLOSE               scratch
.
          CALL                FIXUPXML giving xmlbuff using filename
.
. isolate the responce xml inside the soap message
.
          OPEN                responceXML,d1,DATA=xmlbuff
          SCAN                "?>",xmlbuff
          SCAN                "<",xmlbuff
          BUMP                xmlbuff
...
          PARSE               xmlbuff,SoapNameSpace,"azAZ__09--.."
          WHEREIS             "_",SoapNameSpace,flsize
          IF NOT ZERO
            SETLPTR SoapNameSpace,flsize
          ELSE
            CLEAR             SoapNameSpace
          ENDIF
.
          PACK                rstag,SoapNameSpace,"Body"
          READ                responceXML,seq;@rstag=op
          READ                op,seq;;
          GETFILE             op,curfields=buffer
          CLEAR               rstag
          LOOP
            EXPLODE buffer,";",fielddesc
            FSAVE
            EXPLODE fielddesc,",",rstag,fieldtype
            BREAK IF (fieldtype="R")
            FRESTORE
          REPEAT UNTIL ZERO
          READ      op,d1;@rstag=responceXML2
.
          GETFILE   responceXML2,XMLSIZE=flsize
          DFREE     xmlbuff
          DMAKE     xmlbuff,flsize
          GETFILE   responceXML2,XMLDATA=xmlbuff
// can I just return the xfile here??
            FUNCTIONEND using xmlbuff
            

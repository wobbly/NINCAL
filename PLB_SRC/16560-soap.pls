. test program for NOAA web service to get tempature.
. 
. start by getting gps location of zip code.
.
C COMFILE
Cstat dim	21
cconn init	"100000000000000000000"
cwrite init	"010000000000000000000"
cread init	"001000000000000000000"
fragment dim	1024
SOAP	DIM	5000

httphead init	"POST /forecasts/xml/SOAP_server/ndfdXMLserver.php HTTP/1.1",13,10:
		"Host: www.weather.gov",13,10:
		"Content-Type: text/xml",13,10:
		"Content-Length: "

soapmethod init 13,10,"SOAPAction: http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl##LatLonListZipCode"

byZip	init "<?xml version=#"1.0#" encoding=#"ISO-8859-1#"?>",13,10:
	     "<soap:Envelope xmlns:soap=#"http://www.w3.org/2001/12/soap-envelope#" soap:encodingStyle=#"http://schemas.xmlsoap.org/soap/encoding/#">",13,10:
	     "<soap:Body>",13,10:
	     "<z:LatLonListZipCode xmlns:z=#"uri:http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl#">",13,10:
	     "<z:listZipCodeList xsi:type=#"xsd:string#">75702</z:listZipCodeList>",13,10:
	     "</z:LatLonListZipCode>",13,10:
	     "</soap:Body>",13,10:
	     "</soap:Envelope>",13,10

Len	form	5
dLen	dim	5
crlf	init	13,10

	COUNT	Len,byZip
	SQUEEZE Len,dLen
	PACK	SOAP,httphead,dLen,soapmethod,crlf,crlf,byZip
	COMOPEN	C,"S,O,www.weather.gov,80"
	COMCHECK C,cconn,"10",Cstat
	COMCHECK C,cwrite,"1",Cstat
	COMWRITE C;SOAP
	CLEAR	SOAP
	LOOP
	COMCHECK C,cread,"8",Cstat
	BREAK IF over
	COMREAD	C;fragment;
	APPEND	fragment,SOAP
	REPEAT
	DEBUG
	RESET	SOAP
	COMCLOSE C
	DISPLAY SOAP
	keyin	dlen

PC	EQU	0

	include	common.inc
	include	cons.inc

release	init	"1.0"
XFILE_CLOSEDSCHEMA           Integer            4,"0x02"

//Records, below, are formatted in order to illustrate inherent Tree structure
VendorRep	RECORD
Date			dim	10
Vendor			RECORD
Number				form	4
Name				dim	75
			RECORDEND
Invoice			RECORD	(10)
//We are creating an array with an upperbound limit of 10 items - this is unfortunate!!
Number				form	4
Amount				dim	15
			RECORDEND
		RECORDEND
simplexml	xfile
Vendorxml	xfile
Invoicexml	xfile

...................................................................
.....................Simple version................................
...................................................................

	//We are opening a version of the xml where we have multiple Invoice tags
	//As createxml.pls is not producing this, we are working with a hand created xml!!
	open	simplexml,"f:\library\develop\xml\vendorxml2.xml",SCHEMA="F:\library\develop\xml\VendorReport2.xsd"
	read	simplexml,SEQ;*LL,Date=VendorRep.Date;
	//Next item is a sub-record:  Vendor information
	read	simplexml,SEQ;*LL,Vendor=Vendorxml;
	//Create a temp XML file to hold Child Vendor elements
	read	Vendorxml,SEQ;*LL,VendorNumber=VendorRep.Vendor.Number;
	read	Vendorxml,SEQ;*LL,VendorName=VendorRep.Vendor.Name;
	//Next item is a sub-record:  Invoice information
	read	simplexml,SEQ;*LL,Invoice=Invoicexml;
	//Create a temp XML file to hold Child Vendor elements
	read	Invoicexml,SEQ;*LL,InvoiceNumber=VendorRep.Invoice(1).Number:
					InvoiceAmount=VendorRep.Invoice(1).Amount;
	//THIS IS NOT WORKING YET!!  HAVE NOT BEEN ABLE TO PRODUCE MULTIPLE CHILD NODES
	read	Invoicexml,SEQ;*LL,InvoiceNumber=VendorRep.Invoice(2).Number:
					InvoiceAmount=VendorRep.Invoice(2).Amount;
	close	simplexml
	shutdown


	erase	"f:\library\develop\xml\myxml.xml"
	prep	simplexml,"f:\library\develop\xml\myxml.xml","BrokerReport",RECORDSET="Report"
	//Note attribute applies to recordset root
	write	simplexml,SEQ;Date="06/21/2006",BrokerNumber="0105",BrokerCntNumber="020",BrokerCntNumber="020",BrokerPhone="4145405050",$PhoneType="home",BrokerFax="4145405050"
	close	simplexml
...................................................................
..........Complex version, using Records and Schema file...........
...................................................................

.Load the Records - this would be done by some data processing routine
	//VendorReport Record
	move	"6/21/2006",VendorRep.Date
	//Vendor 'Sub'-Record
	move	"0105",VendorRep.Vendor.Number
	move	"AB Data",VendorRep.Vendor.Name
	//Invoice 'Sub'-Records
.Note that we are limited to 10 instances of an Invoice record.
.This limitation is NOT expressed in Schema file!  In order to tighten data
.relationships, we should either not use a Record for data storage, or
.add the following modifier to "InvoiceType" in VendorReport.xsd:  maxOccurs="10"
	move	"1111",VendorRep.Invoice(1).Number
	move	"$22.53",VendorRep.Invoice(1).Amount
	move	"2222",VendorRep.Invoice(2).Number
	move	"$66.98",VendorRep.Invoice(2).Amount
.
	erase	"f:\library\develop\xml\vendorxml2.xml"
	prep	simplexml,"f:\library\develop\xml\vendorxml2.xml","VendorReport",MODE=XFILE_CLOSEDSCHEMA,SCHEMA="F:\library\develop\xml\VendorReport2.xsd"
	write	simplexml,SEQ;*LL,Date=VendorRep.Date;
	//Next item is a sub-record:  Vendor information
	write	simplexml,SEQ;*LL,Vendor=Vendorxml;
	//Create a temp XML file to hold Child Vendor elements
	write	Vendorxml,SEQ;*LL,VendorNumber=VendorRep.Vendor.Number;
	write	Vendorxml,SEQ;*LL,VendorName=VendorRep.Vendor.Name;
	//Next item is a sub-record:  Invoice information
	write	simplexml,SEQ;*LL,Invoice=Invoicexml;
	//Create a temp XML file to hold Child Vendor elements
	write	Invoicexml,SEQ;*LL,InvoiceNumber=VendorRep.Invoice(1).Number:
					InvoiceAmount=VendorRep.Invoice(1).Amount;
	//THIS IS NOT WORKING YET!!  HAVE NOT BEEN ABLE TO PRODUCE MULTIPLE CHILD NODES
	write	Invoicexml,SEQ;*LL,InvoiceNumber=VendorRep.Invoice(2).Number:
					InvoiceAmount=VendorRep.Invoice(2).Amount;
	close	simplexml
	shutdown

	include	comlogic.inc
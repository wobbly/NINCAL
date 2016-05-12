.
. PURPOSE - READS INPUT file (NPRINT/TEMP)
.           AND PRINTS ORDER FORMS.
.
PC       EQU       0
	INCLUDE	COMMON.INC
	INCLUDE	CONS.INC
	include	compdd.inc
	include	cntdd.inc
	include	ncntdd.inc
	INCLUDE	NORDDD.INC
	INCLUDE	NCRCDD.INC
	INCLUDE	NRTNDD.INC
	INCLUDE	NOWNDD.INC
	INCLUDE	NINVDD.INC
	INCLUDE	SHIPPING.INC
	include	nspIdd.inc
	include	nspedd.inc
	include	hp.inc
	INCLUDE	MEDIA.INC
	INCLUDE	nsmpdd.inc
	INCLUDE	NOFRDD.INC
	include 	winapi.inc
	INCLUDE	NSEL2DD.INC
	INCLUDE	NSEL3DD.INC
	INCLUDE	NADDDD.INC
	INCLUDE	NSLTDD.INC
	INCLUDE	NREFDD.INC
	INCLUDE	NMODDD.INC
	liston

release 	init    "9.70"    DLH  OPtion to Email Fulfillment copies
REldate	Init	"xx March 2008"
.All previous release details see archives version 9.62

.release 	init    "9.62"    DLH  Use SendMail to find snt faxes
.REldate	Init	"20 March 2008"
.release 	init    "9.61"    DLH  Suppress Mailer copies unless COMPConFlag or hotflag overrides
.REldate	Init	"xx March 2008"
.release 	init    "9.6"    DLH  CountArray redone
.REldate	Init	"06 March 2008"
.release init    "9.52"    DLH  13Aug07     More PL
.release init    "9.51"    JD  6May07     Changed  to Jamie for notifications
.release init    "9.50"	   JD  2May07     Updated Consumer Direct Fax
.release init    "9.49"	   DLH 9Mar07     Pacific Lists  - brokerage & Contact
.release init    "9.481"		JD01FEB07     Changed Bahar to Elizabeth for notifications
.release	 init	   "9.48"	         JD     27DEC2006 Patch not reading contact for fulfillments.
.release	 init	   "9.47"	 DMB    25OCT2006 Patch post conversion bug - not trimming var getting a false compare
.release	 init	   "9.46"	 JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release init    "9.45"	JD 03OCT2006     Broker/Consultant address print.(HOT Prints)
.release init    "9.44"	JD 08SEP2006     Fulfillment sort fix, Broker/Consultant address print.
.release init    "9.43"	DMS 29JUN2006    Fulfillment Convervsion
.release init    "9.42"	JD 23JUN2006     Changed Frontdesk email to Bahar for notifications
.release init    "9.41"	ASH 16MAY2006 RECONCILIATION OF LANGUAGE BETWEEN:  NCSH002A, NORD002L, NINV002L, NADJ002L, NORD0024, NORD024B
.release 	init  	"9.40"      DMS	25April2006 Replaced reference to "G:\"
.release  init      "9.39"          JD	30JAN2006 Print out company address on Mailer copy.
.release  init      "9.38.6"       ASH	31OCT2005 Temporary patch for Triplex
.release  init      "9.38.5"       ASH	19OCT2005 Added dynamic Web option
.release  init      "9.38.4"       ASH	15JUN2005 Reworked PDF option
.					Added patch so that requests for Fulfillment copies
.					for Orders that do not have associated Fulfillment houses
.					do not email user stating 'PDF File not found'.  This is misleading!!
.release  init      "9.38.3"       ASH	09DEC2004 Faxfile.lst
.release  init      "9.38.2"       ASH	03NOV2004 Sample File conversion - Increased Mailer field to 6 bytes
.release  init      "9.38.1"       DMB	20SEP2004 Changed Epsi Contacts.
.release  init      "9.38"        ASH	08AUG2004	Logo Conversion
.release  init      "9.37"        DMB	26MAY2004	Mailer Conversion
.					ASH Corrected logic around NINSEL3
.RELEASE  INIT      "9.36"          29jan04 ASH DATACARD CONVERSION
.release		init	"9.352"	DMB	Added Code to check for Spool if true then send email and try again.
.release		init	"9.351"	JD	 use nprint.tmp for overlay sort.
.release		init	"9.35"	JD	 writing to temp file, companies faxed out.
.release		init	"9.34"	ASH	SAMPLE FORMAT CONVERSION
.release		init	"9.33"	commented out piece of code that was hanging Fulfilment run.
.release		init	"9.32"	DMB 27AUG2002 ADD GETWINVER SUBROUTINE TO PROGRAM
.release		init	"9.31"	ASH 30JUL2002 FORCE RUNCODE LISTS TO NOT PRINT OFFER FOR OFFICE COPIES
.release		init	"9.3"	ASH 25JUL2002 BUG FIXES
.release		init	"9.2"	ASH 23JUL2002 BUG FIXES
.release		init	"9.1"	ASH 19JUL2002 BUG FIXES
.release		init	"9.0"	ASH 16MAY2002 COMPLETE REWRITE, REMOVING ALL PREVIOUS PATCHES

MlrNameHold	dim	75
MlrMNameHold	dim	75
PackData DataList
N6a	Form	6
CountIndex	Form	3
CountRecord    Record         (999)                                   ;artificial cap of 999 companies
CntRecComp     Form            6              01-06              Company #
CntRecCount	Form	3	  07-09              Number of orders
	RecordEnd
cnt	dim	35
intrnet	dim	50                .print contact's internet address
pict1	pict
Laser	pfile
Laser2	pfile
file1	pfile
file2	file
INPUT	FILE	FIXED=696,STATIC=12
INPUT2	FILE	.Used for Fulfillment & Overlay Passes
OUTPUT	FILE
output2	FILE
dfile	file
.begin patch 9.7
AbsFileIN	FIle 	BUFFER=1000
AbsFileOut	FIle 	BUFFER=1000
AbsData	Dim	1000
FileCheck	FIle
trapcount	form	4
.end patch 9.7
SAVEFILE IFILE	KEYLEN=6,var=80
OFormBFlag	Init          "Y"
ANS	DIM	1
FILE	FORM	2         BRANCHING CONSTANT FOR I/O TRAPS
DATE	DIM	8         'MM/DD/YY'.
OFFEROUT DIM	11       FOR OUTPUT OF OFFER, SUPPRESSED IF NO OFFER SELCTD.
COUNT	FORM	5        TOTAL NUMBER OF RECORDS SPOOLED?
COUNT1	FORM	5        TOTAL NUMBER OF MLR INPUT READS
COUNT2	FORM	5        TOTAL NUMBER OF OWNER INPUT READS
COUNT3	FORM	5        TOTAL NUMBER OF FULFILMENT INPUT READS
COUNT4	FORM	5        TOTAL NUMBER OF OFFICE INPUT READS
COUNT5	FORM	5        TOTAL NUMBER OF OVERLAY INPUT READS
mlrcnt	form	3        .counts number of orders for a particular brk/mlr.
owncnt	form	3        .counts number of orders for a particular owner.
ownscnt	form	3        .counts number of samples for a particular owner.
NFIELD23 FORM	3.2                  (NUMERIC WORK FIELD)
V1	FORM	2
smpflag	form	1      *1=there is a file *2= file with data
hotflag	form	1      *1=normal print 2= special print.
FORMFLAG FORM	1      1=MAILER, 2=OWNER, 3=FULFILLMENT, 4=OFFICE, 5=overlay/zip screen
FLAGPAID DIM	2      *USED TO FLAG PRE-PAID ORDERS.
PDFFlag	form	1	.Allows PDF Option
REVTXT	INIT	"Revised: "
CANTXT	INIT	"**CANCELLED** : "
BILDTXT	INIT	"**Billed Order**"
REVDATA	DIM	30
BILDDATA DIM	16
REVTYP	DIM	3
EXCHANGE DIM	15         *USED FOR ORDER PRINT
TEST	DIM	15         *USED FOR ORDER PRINT
SAMPLE	DIM	26        *USED FOR ORDER PRINT
F3	DIM	3         *USED FOR ORDER PRINT
F2	DIM	2         *USED FOR ORDER PRINT
ENTIRE	DIM	1        *USED FOR ORDER PRINT
CORTN	DIM	3         *USED FOR ORDER PRINT
CONT	DIM	23        *USED FOR ORDER PRINT
CONT1	DIM	20        *USED FOR ORDER PRINT
CONTDTE	DIM	10         *USED FOR ORDER PRINT
CONTQTY	DIM	11         *USED FOR ORDER PRINT
QTYMSK	INIT	"ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT	DIM	11         *USED FOR ORDER PRINT
QTYNUM	FORM	9         *USED FOR ORDER PRINT, QTY FORMATING.
MEDMEMO	DIM	25        *USED FOR ORDER PRINT, ON MAG TAPE.
COMSLCT	DIM	25        *USED FOR ORDER PRINT, COMSELECT ORDERS.
REPRT	DIM	15        *USED FOR ORDER PRINT, REPRINTED ORDERS.
.                            *AND CANCELLED ORDERS, REPRINT IMPLIED.
LROUT	DIM	6         *USED FOR ORDER & LABEL PRINT.
LRMASK	INIT	"ZZZZZ9"
LRNUM	FORM	6
COUNTR	FORM	4         RECORDS IN?
PRICECK	DIM	5
.PHONE DISPLAY VAR'S
LP	DIM	1
RP	DIM	1
EXT	DIM	3
ARCD	DIM	3
PHONE	DIM	4
FAX1	DIM	13
rtEXT	DIM	3       .return-to
rtARCD	DIM	3       .return-to
rtPHONE	DIM	4       .return-to
MEDTYPE	DIM	1
faxname	dim	45
faxtele	dim	10
faxattn	dim	45
save	dim	47
savecoms dim	1
ofosave	dim	2
rprtcode init	"RXQ"
COPY	FORM	1
careof	dim	3
holdmlr	dim	4
holdbrk	dim	4                   .used for owner break
holdown	dim	4                   .used for owner break
smpown	dim	4
fhandle	dim	8                   .use to create fax files.
holdcom	dim	1                   .used for list owner ccto break.
holdccto dim	6                    .used for list owner ccto break.
holdcccnt dim	45                   .used for list owner ccto break. (Contact Name)
holdcccmp dim	55                   .used for list owner ccto break. (Company Name)
ovrTEL1	init	"7732901789"
ovrTEL2	init	"2033536661"
ovrTEL3	init	"6124816363"
ovroct1	form	2                number of orders
ovroct2	form	2                number of orders
ovroct3	form	2                number of orders
ovrnum	form	2                table index
faxflag	form	1                    .1=no, 2=yes.
FaxNumFlag form	1                    is set if Mailer fax number is valid.
ovr1	init	"C"
ovr2	init	"L"
ovr3	init	"I"
TRIPLEX	INIT	" "
attchlst dim	1000
LPTCNT	FORM	4                 .LENGTH OF ATTCHLST
spoolfl2 DIM	40                   .order  SPOOL FILEs
TIME	DIM	8
MO	DIM	2       MONTH
YR	DIM	2
LONGDIST DIM	1
DCX	INIT	".TIF"
malchow	INIT	"3392-4427-4517-4814-4840"    Malchow list owner numbers.
str45a	dim	45
lstmgt	dim	2
epsiflag dim	1
epsicvr	form	1
epsicon1 init	"0024"
epsicon2 init	"2702-1696"
epsicon3 init	"9374-1696"
epsicon4 init	"1762-5759"
attn	dim	6
nfaxtel	dim	15
nfaxtel2 dim	15
FirstFlag init	"Y"
FirstFlag1 init	"Y"
PrintFlag form	"0"
LastFlag form	1
nosmpl	dim	1
DCX2	dim	30
DCXFile	dim	120
SPOOLF	dim	120
hotprt	dim	9                  if a hot print prnt "Hot Print" by typist inits on office copy
dcxpath	init	"\\nts1\e\data\samples\"	."
FilePath init	"C:\WORK\"			."
faxkount form  3
SMPArray dim	12(50)
SMPIndex form	2
rtphmask dim	14
line1	dim	55
line2	dim	55
line3	dim	55
line4	dim	55
line5	dim	55
line6	dim	55
line7	dim      55
line8   dim	55
line9	dim	55
line10	dim	55
line11	dim	55
line12	dim	55
line13	dim	55
line14	dim	55
hotkey	dim	6
tipe	dim	1
rptcan	dim	1
.Create fonts to be used
font1		font
Font4		font
font5		font
fontO8		font
fontO9		font
fontO9I		font
fontO10		font
fontO10n	font
fontO10B	font
fontO12B	font
fontO14		font
FontO14B	font
FontO14BI	font
FontO18I	font
FontO7		font
FontO7dot5	font
FontO7dot5B	font
FontO7dot5I	font
FontO7dot5BI	font
FontO18B	font
FontO18BI	font
PRTPG24B	font
PRTPG24I	font
PRTPG10		font
Font08I	font
Font08BI font
font7	font
font8	font
font9	font

NINLogo	PICT
Blockout	PICT
Blockout1	PICT

sevenfive	form	"7.5"
externalmode	integer 1
DimPtr		dim	^

NFULNUM		DIM	6
NFULCOMP	DIM	55
NFULCNT		DIM	45
NFULFAX		DIM	10
.begin patch 9.7 
NFULEMAIL	Dim	50
EmailFlag	Dim	1
.end patch 9.7 

Start
dlFiles 	datalist
DLresult 	form 9
DLndx 	form 9
dmFileName 	dim 80
PdfFName	Dim	25

x       plform  report
        formload X

**************************************************
* PROGRAM MAIN.
* *************
	create	font1,"Times New Roman",size=14,bold
	create	fontO8,"Times New Roman",size=8
	create	font5,"Times New Roman",size=11
	Create	fontO9,"Times New Roman",size=9
	create	fontO9I,"Times New Roman",size=9,Italic
	create	fontO10,"Times New Roman",size=10
	create	fontO10n,"Courier New",size=11
	create	fontO10B,"Times New Roman",size=10,Bold
	create	fontO12B,"Times New Roman",size=12,Bold
	create	fontO14,"Times New Roman",size=14
	create	fontO14B,"Times New Roman",size=14,Bold
	create	fontO14BI,"Times New Roman",size=14,Bold,Italic
	create	fontO18I,"Times New Roman",size=18,Italic
	create	fontO7dot5,"Times New Roman",size=sevenfive
	create	fontO7dot5I,"Times New Roman",size=sevenfive,Italic
	create	fontO7dot5b,"Times New Roman",size=sevenfive,Bold
	create	fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
	create	fontO18BI,"Times New Roman",size=18,Bold,Italic
.
	create	PRTpg24B,"Times New Roman",size=24,Bold
	create	PRTpg24I,"Times New Roman",size=24,Italic
	create	PRTpg10,"Times New Roman",size=10
	create	font08I,"Times New Roman",size=8,Italic
	create	font08bI,"Times New Roman",size=8,Bold,Italic
.Create work var
	create	PackData=1:1:1:1

	move	"750",column
	move	"1750",column1
	move	"3000",column2
	create	font7,"Helvetica",size=14,bold
	create	font8,"Helvetica",size=14,italic
	create	font9,"Arial",size=12
	if (externalMode)
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold

		CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
	else
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
		CREATE	NINLogo=3:13:30:50:
			"\\nts0\c\netutils\NIN logo black outline.jpg"
	endif
		CREATE  	Blockout=3:20:30:50:
			"\\nts0\c\netutils\blockout.tif"
		CREATE  	Blockout1=3:20:30:50:
			"\\nts0\c\netutils\blockout2.tif"
.

.Find out system information

	CALL GETWINVER
.
	clock	time,time
	clock	timestamp,timestamp
.
	rep	lowup,PROGRAM
	match	"NORD002L",PROGRAM   .case sensitive
	if not equal
		move	"NORD002L",PROGRAM
		move	"ORDER PRINT  ",STITLE
		move	C0,copy
		move	C1,hotflag
	else
		unpack	inpname,str6,str1,str3
		move	str6,NORDFLD
		move	C2,hotflag
		move	C0,formflag
		move	C1,NORDPATH
		move	C0,NCRCFLAG
		rep	"M1L2F3O4A0",str1         .get request
		rep	"10213243",str1      .set copy to match
		move	str1,copy
		move	"HOT ORDER PRINT ",STITLE
		move	COMMENT,TRIPLEX
		if (FUNC = "2")
			move	C1,PDFFlag
			if (!externalmode)
.
				call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
					"Parameters":
					"ProcessPDF":
					"\\nts0\c\apps\plb\code\pdftest.bat":
					result
				if (result = C0)
.Prepare Flag file
					prep	tempfile,"c:\progra~1\pdf995\flag.dat"
					write	tempfile,SEQ;"flag set"
					close	tempfile
				endif
			endif
		else
			move	C0,PDFFlag
		endif
	endif
	move	C0,howmany
	move	"Names In The News",COMPNME
	call	PAINT
	move	"Exit",PF5
	trap	END if F5
	call	FUNCDISP
. OPEN FILES.
prepit
	display	*P1:24,"OPENING FILES";
	move	C4,FILE
	move	C1,NBRKPATH      .SET ACCESS ISI.
	move	C5,FILE
CLOCK
	clock	DATE,DATE
	clock	DATE,today
	unpack	DATE,MM,DD,YY
	rep	" 0",DD
	pack	DATE FROM MM,SLASH,DD,SLASH,YY
	add	C1,FILE
	add	C1,FILE
	clear	tipe
	scan	"FAX",prtname
	if equal
		move	"F",tipe
	else
		move	"H",tipe
	endif
	if (hotflag = C2)
		if (!externalmode)
 			pack	taskname,NTWKPATH7,"HOTORDERS"
			Open 	SAVEFILE,Taskname
			clear	hotkey
			move	NORDFLD,hotkey
			filepi	4;savefile
			read	savefile,HOTKEY;;
			if over
				write	SAVEFILE;"******",today
				write	SAVEFILE,NORDFLD;NORDFLD,tipe,str3
			endif
		endif
	endif
	reset	prtname
...........................................................................
.Note:  When testing, faxfile needs to be set differently for Full Run and HotOrd Run!!!!!!!
.       Full Run requires faxfile to print to:  \\nts0\d\data\fax\Faxfileprn - CORRECTED 12/9/2004 ASH NOW USES C:\WORK
.       HotOrd Run requires faxfile to print to:  c:\work\Faxfile.prn
...........................................................................
	if (hotflag = 1)	.Full Run
.NPRINT.TMP used for Mailer run
		trap	IO GIVING ERROR IF IO
		prepare	output2,"\\nts1\e\data\orders.fax"
		WRITE	output2,SEQ;B1,today," -Company's Receiving Order FAXES !!!",b1,time

		open	INPUT,"\\nts1\e\data\NPRINT.TMP"
		sub	FILE FROM FILE
		display	*P1:24,*EL;
		for formflag,C1,C5
.For testing, leave it in.
.			until (formflag > 2)
			loop
				move	C1,FILE
				add	C1,COUNTR
				display	*P10:12,*EL,"COUNT READ ",COUNTR,B1,OLRN
				read	INPUT,SEQ;ORDVARS
				if over
.FOLLOWING CODE INCLUDES BREAK, WHICH WILL ALLOW MULTIPLE PASSES!!!!
.Print last records
					move	YES,FirstFlag
					if (formflag = 1)                      .mailer copies
						call	BRKBREAK
					elseif (formflag = 2)		   .owner								
						call	OWNBREAK
						call	prepsplB
					elseif (formflag = 3)		    .Fullfillment
.No need to call fulbreak, as PRTNAME is created before actual file.
						call	FULEOJ
					elseif (formflag = 4)		    .office
					elseif (formflag = 5)		     .overlay
						call	ovrbreak
					endif
					move	YES,FirstFlag
					close	input
					trap	IO GIVING ERROR IF IO
					if (formflag = 1)
.Prep for Owner run
					open	INPUT,"\\nts1\e\data\NPRINT.OWN"

					elseif (formflag = 2)
.Prep for Fulfillment run	
FULLER
						If	(olrn = "684272")
.						call	debug
						endif
.;							pack	taskname,"\\nts1\e\data\NPRINT.FLL,\\nts1\e\data\NPRINT.FUL;409-414"
						open	INPUT,"\\nts1\e\data\NPRINT.OWN"
							pack	taskname,"\\nts1\e\data\NPRINT.own,\\nts1\e\data\NPRINT.FUL;329-334"
							sort	taskname
							open	INPUT,"\\nts1\e\data\NPRINT.FUL"
							goto READO
					elseif (formflag = 3)
.Prep for Office run, which includes creation of file for Overlay run
						open	INPUT,"\\nts1\e\data\NPRINT.LR"
.Writing to above file happens in Office Print loop, so I will close it later
					elseif (formflag = 4)
.Prep for Overlay run
						pack	APIFileName,"\\nts1\e\data\NPRINT.tmp",hexzero
						call	FindFirstFile
						if (APIResult <> 0 & APIResult <> hexeight)
							pack	taskname,"\\nts1\e\data\NPRINT.tmp,\\nts1\e\data\NPRINT.OVR;211-211"
							sort	taskname
							open	INPUT,"\\nts1\e\data\NPRINT.OVR"
						else
						erase	"\\nts1\e\data\NPRINT.ovr"
						prepare	INPUT,"\\nts1\e\data\NPRINT.ovr"
						endif
					elseif (formflag = 5)
.Nada happens
					endif
					call	PrtCloseFile
					BREAK
				endif
				call	Trim using OLRN
				if (OLRN <> "")
					reset	rprtcode
					scan	OSTAT,rprtcode
					if not equal
						if (formflag <> "2")
							call	CreateRecord
							if (formflag = "3")	.Office run
							endif
.10Jul2001 DLH if List Manager is Hussey & List = 17865 & Pass = owner copy then suppress
						elseif (OLON <> "5192" | OLNUM <> "017865")
.27Jun97 DLH if List Manager is Malchow & Pass = owner copy then suppress
							reset	Malchow		."3392-4427-4517-4814-4840"    Malchow list owner numbers.
							scan	OLON,Malchow
							if not equal
								call	CreateRecord
							endif
						endif
					endif
				endif
			repeat
READO
			move	C0,countr
		repeat
	elseif (hotflag = 2)	.Individual HotPrints
.Following label is strictly referential.  No longer used in code.
.readlive
		rep	zfill,NORDFLD
		move	C1,NORDPATH
		move	"readlive-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		move	str3,ODOWJ
		move	OLRN,NSPEFLD
		rep	zfill,NSPEFLD
		move	"readlive-NSPEKEY",Location
		pack	KeyLocation,"Key: ",NSPEFLD
		call	NSPEKEY
.Clean up before hand
		pack	str45,"C:\WORK\",prtname
		erase	str45
		pack	str45,"C:\WORK\Faxfile.prn"
		erase	str45
.
		call	CreateRecord
		call	PrtCloseFile
		if (PDFFlag = 1)
			if (!externalmode)
.Give the email a chance of rendering itself before updating the INI file.
				pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
				loop
					call	FindFirstFile
					until (APIResult = 0 | APIResult = hexeight)
					pause	"1"
				repeat
				pause	"2"
			endif
		else
			pack	APIFileName,"C:\WORK\Faxfile.prn",hexzero
			call	FindFirstFile
			if (APIResult <> 0 & APIResult <> hexeight)
				pack	str45,"C:\WORK\",prtname
				erase	str45
				rename   "C:\WORK\Faxfile.prn",str45
			endif
		endif
	endif
.PROGRAM WILL HAVE TO END HERE
	if (hotflag = 1)	.Full Run
		write  	output2,seq;b1,"Total Company's Faxed Out ",faxkount
		Weof	Output2,seq
		close  	output2
		open 	dfile,"\\nts1\e\data\orders.FAX"
		Clear	MailBody
		Append	Today,MailBody
		Append	" -Company's Receiving Order FAXES !!! ",MailBOdy
		append	Time,mailbody
		append	CRLF,MailBOdy		
		loop
			read dfile,seq;str55
			until over
			append	STR55,mailbody
			append	CRLF,MailBOdy		
		repeat
		Reset	Mailbody
		move    	"Please verify that total matches Fax queue",Mailsubjct
		pack	Mailfrom,"JamieMittone@nincal.com"
		pack	Mailto,"JamieMittone@nincal.com,JamieMittone@nincal.com"
		call	SendMail
	endif
	if (externalmode)
		return
	endif
	shutdown

CreateRecord
.Get Offer
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
	bump	OODNUM,4
	pack	NOFRFLD,OMLRNUM,OODNUM
	reset	OODNUM
	move	"C.Record-NOFRKEY",Location
	pack	KeyLocation,"Key: ",NOFRFLD
	call	NOFRKEY
.
	move	"10",FILE
	clear	ARCD
	clear	EXT
	clear	PHONE
	clear	LP
	clear	RP
	clear	DASH
	move	OLON,NOWNFLD
	rep	zfill,NOWNFLD
	move	"C.Record-NOWNKEY",Location
	pack	KeyLocation,"Key: ",NOWNFLD
	call	NOWNKEY

	call Trim using OFULLFIL
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"DISRTN-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	COMPVARS
			clear	NFULNUM
			clear	NFULCOMP
			clear	NFULCNT
			clear	NFULFAX
			clear   	cnctfname
.Begin patch 9.7 
			Clear	NFulEmail
.end patch 9.7 

		else
			if (COMPSVBFLG <> "T")
				clear	COMPFLD
				clear	COMPVARS
				clear	NFULNUM
				clear	NFULCOMP
				clear	NFULCNT
				clear	NFULFAX
				clear   cnctfname
.Begin patch 9.7 
				Clear	NFulEmail
.end patch 9.7 
			else
						Packkey CNCTFLD2 to "01X",COMPNUM
						Call	CNCTAIM
						loop
						until over
						until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
							call	CNCTKG
						repeat
				move	COMPNUM,NFULNUM
				move	COMPCOMP,NFULCOMP
				move	CNCTFNAME,NFULCNT
				move	COMPFAX,NFULFAX
.begin patch 9.7
				Move	CompEmail,NFULEMAIL
				if	(formflag = c3 & NfulEmail <> "")          . I think we have a winner
				Move	Yes,EmailFlag
				else
				Move	No,EmailFlag
				endif
.end patch 9.7 
			endif
		endif
	else	.// OFULLFIL = ""
		clear	COMPFLD
		clear	COMPVARS
		clear	NFULNUM
		clear	NFULCOMP
		clear	NFULCNT
		clear	NFULFAX
		clear   cnctfname
	endif
	match	"0000000000",OWNTELE		*PHONE NUMBER?
	call	PHONE if not equal		*YES
	clear	BILDDATA
	move	OLRN,NINVFLD
	move	C1,NINVPATH
	move	"C.Record-NINVTST",Location
	pack	KeyLocation,"Key: ",NINVFLD
	call	NINVTST
	if not over
		move	BILDTXT,BILDDATA
	endif
	clear	REVDATA
	clear	REVTYP
	pack	NCRCFLD,OLRN
	move	"C.Record-NCRCKEY",Location
	pack	KeyLocation,"Key: ",NCRCFLD
	call	NCRCKEY
	loop
		until over
		until (NCRCFLD <> NCRCKEY)
		move	NCRCTYP,REVTYP
		if (NCRCCODE = "C")
			pack	REVDATA,CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
			break
		else
			pack	REVDATA,REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
		endif
		move	"C.Record-NCRCKS",Location
		call	NCRCKS
	repeat
	sub	FILE,FILE
.
. PRINT VARIABLES FROM ACCESSED RECORD
.
PROCESS
.IF REGULAR CYCLE USE FORMFLAG . ELSE . USES COPY.
	branch	FORMFLAG,PRTMLRBX,PRTOWNBX,PRTFULBX,PRTOFFBX,PRTOVRLY
	branch	COPY,PRTOWNBX,PRTFULBX,PRTOFFBX
PRTMLRBX
	add	C1,COUNT1
	display	*P10:13,*EL,"mlr COUNT READ ",COUNT1,B1,OLRN
	move	C0,N10
	call	Trim using OQTY
	move	OQTY,N10
	if (N10 = C0)
		if (hotflag = 2)
			goto PRTOFFBX	.HotPrints automatically have Office Copy printed
		else
			return
		endif
	endif
	reset	RUNCODES
	scan	OLNUM,RUNCODES
	if equal
		return
	endif
.if not hotprint And Mailer preference for hardcopy is not True & its not a list management order = return
		pack	MKEY,OMLRNUM,Z3
		rep	zfill,MKEY
		move	"SetPrintFlag-NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY             .make sure we have last brker info
	clear	lstmgt
	pack	lstmgt,OSALES10,OSALES
	
	if	(Hotflag = c1 & COMPConFlag <> "T" & LStmgt <> "06"  & LStmgt <> "19"  & LStmgt <> "27"  & LStmgt <> "28"  )
	return
	endif
	
mlronly
	if (hotflag = C2)	.HotPrint
		move	YES,FirstFlag
	else
		move	NO,epsiflag
		clear	lstmgt
		pack	lstmgt,OSALES10,OSALES
		rep	zfill,lstmgt
.dlh march 2008  Start this code can probably go
		if (lstmgt <> "06")
			if (OBRKNUM = "0192")
				move	YES,epsiflag
			endif
		endif
.dlh march 2008  end this code can probably go
		if (OBRKNUM <> HOLDBRK)
			move	YES,FirstFlag
.Do NOT put a Trim in here as there is a possibility of having a Broker Number of "    ".
.This filter is purely for the first record read.  We do not want to call BRKBREAK for that record, and
.we do not want to have to include a flag.  HOLDBRK is initialized with nothing.
			if (HOLDBRK <> "")
.Called after vars have been set in SetPrintFlag for previous record
				call	BRKBREAK
			endif
.Establishes flags/settings for current record
			call	SetPrintFlag
			move	OBRKNUM,HOLDBRK
		endif
		if (HOLDMLR <> "" AND HOLDMLR <> "    " AND HOLDMLR <> OMLRNUM)
			if (epsiflag = YES)
				call	newcover
.FaxNumFlag AND PrintFlag are only set if SetPrintFlag does not find a valid Broker/Broker Fax Number.
.FaxNumFlag is set if Mailer fax number is valid.
.PrintFlag is set if neither a Broker Fax number nor a Mailer Fax Number is found and document is to be printed.
.In either case, if the Mailer number has changed, you would need to look for a valid fax number once again.
			elseif (FaxNumFlag = 1 | PrintFlag = 1)
				move	YES,FirstFlag
.Called after vars have been set in SetPrintFlag for previous record
				call	BRKBREAK
.Establishes flags/settings for current record
				call	SetPrintFlag
				move	OBRKNUM,HOLDBRK
			endif
		endif
		move	OMLRNUM,HOLDMLR
		add	C1,mlrCNT
	endif
mlronlyend
	move	YES,OformBFlag
	call	prtordfrm
	call	prtmlrboxGui
	call	process1
	return

SetPrintFlag
	display	*p1:24,*el,"Set Print Flag"
	move	"                                             ",faxattn
	move	"                                             ",faxname
	clear	faxattn
	clear	faxname
.
	pack	NBRKFLD,OBRKNUM,Z3
	rep	zfill,NBRKFLD
	move	"SetPrintFlag-NBRKKEY",Location
	pack	KeyLocation,"Key: ",NBRKFLD
	call	NBRKKEY             .make sure we have last brker info
.......................
	move	C0,FaxNumFlag
	move	C0,N10
	move	BRFAX,N10
	if (N10 = C0)
		pack	MKEY,OMLRNUM,Z3
		rep	zfill,MKEY
		move	"SetPrintFlag-NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY             .make sure we have last brker info

		move	C0,N10
		move	MFAX,N10
		if (N10 = C0)
			move	C1,PrintFlag
			move	C1,faxflag
		elseif (MFaxOFlag <> "T")
			move	C1,PrintFlag
			move	C1,faxflag
		else
			pack	fhandle,OBRKNUM,MNUM
			pack	str2," x"
			rep	str2,fhandle
			move	MCOMP,faxname
			move	MFAX,FAXTELE
			move	MCONTCT,faxattn
.
			move	C1,FaxNumFlag
			move	C2,faxflag	.do fax.
		endif
	elseif (BrFaxOFlag <> "T")
		move	C1,PrintFlag
		move	C1,faxflag
	else
		move	BRKNUM,fhandle
		move	BRCOMP,faxname
		move	BRFAX,FAXTELE
		move	BRCNTCT,faxattn
.
		move	C2,faxflag	.do fax.
	endif
	return

BRKBREAK
	display	*p1:24,*el,"broker break"
	if (faxflag = 2)
		display	*p1:24,*el,"FAX IT "
.		call	prepspl2
		call	prepspl
.		call	prepfax2
		call	prepfax
	else
		display	*p1:24,*el,"No Fax Number"
	endif
	move	C0,mlrcnt		.reset count for cover sheet
.Initialize HOLDMLR each time a new break occurs
	clear	HOLDMLR
	return

*******************************************************************************
newcover
	prtpage	Laser;*NEWPAGE
newcover2
	call	contacts
        move    "300",row
..Go ahead and print the last line now
	IF	(OcompID2 = "P")
	prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
		*p=6014:443,"Oakland, CA 94612-1429":
		*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=5980:643,"A Division of Names in the News"

	Else
	prtpage	Laser2;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1300:2500:10100:NINLogo
	endif



        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
        prtpage Laser;*p3500:row,*boldon,*font=fontO14B,"LIST ORDERS",*boldoff;
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
        prtpage Laser;*p3500:row,"VIA FACSIMILE";
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
        prtpage Laser;*p650:row,"Date:";
	unpack	timestamp,CC,YY,MM,DD
	pack	str8,MM,DASH,DD,DASH,CC
        prtpage Laser;*p1500:row,str8;
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
        prtpage Laser;*p650:row,"To:";
        prtpage Laser;*p1500:row,faxname;
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	call	Trim using faxattn
	if (faxattn <> "")
	        prtpage Laser;*p650:row,"Attn:";
	        prtpage Laser;*p1500:row,faxattn;
		add	eightlpi,row
		add	eightlpi,row
		add	eightlpi,row
	endif
        prtpage Laser;*p650:row,"From:";
        prtpage Laser;*p1500:row,"List Management";
	return

******************************************************************************
PRTOWNbx
	add	C1,COUNT2
	display	*P10:14,*EL,"Owner COUNT READ ",COUNT2,b1,olrn
	move	C0,N10
	call	Trim using OQTY
	move	OQTY,N10
	if (N10 = C0)
		if (hotflag = 2)
			goto prtoffbx
		else
			return
		endif
	endif
	reset	RUNCODES
	scan	OLNUM,RUNCODES
	if EQUAL
.Need to refresh last valid OLON, so that .LST files are properly named
		move	holdown,OLON
		return
	endif
	if (hotflag = 2)
		move	YES,FirstFlag
	endif
	branch	hotflag,regown,hotown
regown
	match	olon,holdown              *same owner?
	if not equal
		move	YES,FirstFlag
		call	ownbreak
		call	OwnSetPrintFlag
	endif
	add	C1,OWNCNT
hotown
hotown1
	move	NO,OformBFlag
	call	prtordfrm
	call	prtownerboxGui
	call	Process1
	return

ownbreak
	display	*p1:24,*el,"owner break",COUNT2,b1,olrn
	if (countr = C1)
		move	C1,faxflag
		move	OLON,holdown
		move	OLON,smpown
		return
	endif
.was last owner faxable ?
	compare	C2,faxflag
	if equal          .yes
		move	"                                             ",faxattn
		move	"                                             ",faxname
		clear	faxattn
		clear	faxname
		move	holdown,NOWNFLD
		rep	zfill,NOWNFLD
		move	"ownbreak-NOWNKEY",Location
		pack	KeyLocation,"Key: ",NOWNFLD
		call	NOWNKEY			.make sure we have last owners info
		move	holdown,fhandle
		move	ownocpy,faxname
		move	OWNFAX,FAXTELE
		move	ownlonm to faxattn
		call	prepfax
.Refresh after creating Files
		move	OLON,NOWNFLD
		rep	zfill,NOWNFLD
		move	"ownbreakB-NOWNKEY",Location
		pack	KeyLocation,"Key: ",NOWNFLD
		call	NOWNKEY			.make sure we have last owners info
	endif
.Initialize a bunch of vars
	move	C1,faxflag		.don't fax, re-initialize
	move	C0,owncnt		.reset count for cover sheet.
	move	C0,ownscnt		.reset sample count for cover sheet.
	move	OLON,holdown		.set break
	return

OwnSetPrintFlag
	display	*p1:24,*el,"checking owner ",ownocpy
.When we decide to Fax out everything, we can remove all of the following up to ownbrk1, and also remove ownbrk2 label
        call  trim using ofullfil
	if (OFULLFIL <> "")
.
		move	C0,N10
		goto	ownbrk2
	else
		clear	COMPFLD6
		clear	COMPVARS
		clear	NFULNUM
		clear	NFULCOMP
		clear	NFULCNT
		clear	NFULFAX
.END PATCH 9.43 REPLACED LOGIC
	endif
ownbrk1
	move	ownfax,N10
ownbrk2
	compare	C0,N10
	if equal
.printing  not faxing - no or invalid fax number
		display	*p1:24,*el,"no fax number/Print Copy"
		move	C1,faxflag        .
		move	C1,PrintFlag
	else
		move	C2,faxflag        .do fax.
		display	*p1:24,*el,"fax IT "
		call	prepspl
		move	OLON,smpown
	endif
	return
*******************************************************************************
PRTFULbx
	add	C1,COUNT3
	display	*P10:15,*EL,"Ful COUNT READ ",COUNT3,b1,olrn
	move	C0,N10
	call	Trim using OQTY
	move	OQTY,N10
	compare	C0,N10
	if equal
		if (PDFFlag = 1)
			if (!externalmode)
				erase	"c:\progra~1\pdf995\flag.dat"
			endif
		endif
		return
	endif
	call	Trim using NFULCOMP
	call	Trim using OFULLFIL
	if (OFULLFIL = "009406")
		if (TRIPLEX <> "1")
			display	*p1:24,*el,"IT's Triplex, Skip"
			if (PDFFlag = 1)
				if (!externalmode)
					erase	"c:\progra~1\pdf995\flag.dat"
				endif
			endif
			return
		endif
	endif
	match	"0001",ortnnum       .REUSE
	if equal
		display	*p1:24,*el,"IT's RE-USE, Skip"
		if (PDFFlag = 1)
			if (!externalmode)
				erase	"c:\progra~1\pdf995\flag.dat"
			endif
		endif
		return
	endif
	reset	RUNCODES
	scan	OLNUM IN RUNCODES
	if equal
		display	*p1:24,*el,"It's Running charges, Skip"
		if (PDFFlag = 1)
			if (!externalmode)
				erase	"c:\progra~1\pdf995\flag.dat"
			endif
		endif
		return
	endif
	call	Trim using OFULLFIL
	if (OFULLFIL = "")
		if (PDFFlag = 1)
			if (!externalmode)
				erase	"c:\progra~1\pdf995\flag.dat"
			endif
		endif
		return
	endif
	if (hotflag = 1)
		goto regful
	elseif (hotflag =2)
		goto prtfulby
	endif
regful
	move	C0,N6
	move	NFULNUM,N6
	if (N6 > C0)
.find company in record
		for	CountIndex,"1","999"
		MOve	CountRecord(countindex).CntREcComp to N6a               .get company
		if	(N6a = N6)			.Match  increment counter
		MOve	CountRecord(countindex).CntREcCount to n3               .get count
		add	c1,n3
		MOve	N3 to CountRecord(countindex).CntREcCount               .save count
		Break
.
		Elseif	(N6a = c0)	                  .company not in record
		MOve	N6 to CountRecord(countindex).CntRecComp        .Save company #
		MOve	c1 to CountRecord(countindex).CntRecCount        .Save #
		break
		
		endif
		Repeat
			
	endif
whoanelly
	if (NFULNUM <> HOLDCCTO)
		move	YES,FirstFlag
		call	fulbreak
	endif
	goto      prtfulby
fulbreak
.	call	debug
	display	*p1:24,*el,"FULFILLMENT break"
	if (COUNTR = C1)
		move	C1,faxflag
	endif
	if (faxflag = C2)		.was last one faxable?
.		pack	fhandle,"f",holdccto
		pack	fhandle,holdccto
		fill	B1,faxname
		move	holdcccmp,faxname
		call	Trim using holdcccnt
		if (holdcccnt = "")
			move	"Order Fulfillment",faxattn
		else
			move	holdcccnt,faxattn
		endif
		pack	prtname,"nordF",holdccto,".lst"
	endif
.now lets set up for current one.
	clear	holdccto
	move	NFULNUM,holdccto
	move	NFULCNT,holdcccnt
	move	NFULCOMP,holdcccmp
	move	C1,faxflag
	move	C0,N6
	call	Trim using NFULNUM
	move	NFULNUM,N6
	if (N6 > C0)
.If var somehow was not zero-filled correctly, do it now
		move	N6,NFULNUM
		rep	zfill,NFULNUM
		move	C0,N10
		call	Trim using NFULFAX
		move	NFULFAX,N10
		if (N10 > 0)
			move	C2,faxflag        .yes set fax fulfilment flag on.
			move	NFULFAX,FAXTELE
			return
		endif
	endif
	return

prtfulby
	move	NO,OformBFlag
	call	prtordfrm
	call	prtfulfilboxGui
	call	Process1
	return
.
PRTovrly
	add	C1,COUNt5
	display	*P10:17,*EL,"Overlay COUNT READ ",COUNT5,b1,olrn
	move	C0,N10
	call	Trim using OQTY
	move	OQTY,N10
	compare	C0,N10
	if equal
		if (hotflag = 1)
			return
		elseif (hotflag = 2)
			goto prtoffbx
		endif
	endif
	reset	RUNCODES
	scan	OLNUM IN RUNCODES
	if equal
		return
	endif
	call	Trim using OCOMSLCT
	if (OCOMSLCT = "")
		return
	endif
	call	Trim using holdcom
	if (OCOMSLCT <> holdcom)
		if (holdcom <> "")
			move	YES,FirstFlag
			call	ovrbreak
		endif
		move	C0,OWNCNT
		move	OCOMSLCT,holdcom
	endif
	add	C1,OWNCNT
	call	prtovr
	return
.
ovrbreak
	display	*p1:24,*el,"overlay break"
	if (countr = C1)
		move	C1,FAXFLAG
	endif
.now lets set up for current one.
	move	C2,faxflag
	clear	faxattn
	move	B55,faxname
	if (holdcom = ovr1)
		move	ovrtel1,FAXTELE
		move	"COMS",fhandle
		move	"Consumer Direct",faxname
	elseif (holdcom = ovr2)
		move	ovrtel2,FAXTELE
		move	"LIFE",fhandle
		move	"Lifestyle",faxname
	elseif (holdcom = ovr3)
		move	ovrtel3,FAXTELE
		move	"IC",fhandle
		move	"IC Systems",faxname
	else
		clear	FAXTELE
		move	"OVER",fhandle
		move	C1,faxflag
		move	C1,PrintFlag
		return
	endif
	pack	prtname,"nord",fhandle,".lst"
	call	prepfax
	return
prtovr
	move	NO,OformBFlag
	call	prtordfrm
	call	prtfulfilboxGui
	call	Process1
	return
.
PRToffbx

	reset	RUNCODES
	scan	OLNUM IN RUNCODES
	if equal
		clear	OODNUM
		clear	OFDESC
	endif
	add	C1,COUNT4
	display	*P10:16,*EL,"Office COUNT READ ",COUNT4,b1,olrn
	move	YES,OformBFlag
	if (hotflag = 1)
		clear	prtname
		move	C1,PrintFlag
		move	YES,FirstFlag
	endif
	call	prtordfrm
	call	prtofficeboxGui
	call	Process1
	return

prepfax
.....TESTING.....
.	move	"5106288313",faxtele
.....TESTING.....
	count	N2,faxtele
	compare	C10,N2
	if equal
		move	C1,LONGDIST
		unpack	faxtele,str3,str7
		match	"510",str3		.LOCAL ?
		if equal
			move	str7,faxtele
			clear	LONGDIST
		else
			match	B3,str3		.LOCAL ?
			if equal
				move	str7,faxtele
				clear	LONGDIST
			endif
		endif
	endif
FILENAME
.FORMFLAG FORM	1      1=MAILER, 2=OWNER, 3=FULFILLMENT, 4=OFFICE, 5=overlay/zip screen
	call	Trim using fhandle
	if (fhandle = "")
		return
	endif

	move	B1,ERROR
	clear	spoolfl2
	if	(formflag = 2)
	append	"\\nts0\d\data\fax\nord",spoolfl2
	pack	prtname,"nord",fhandle,".LST"
	pack	PdfFname from "nord",fhandle
	Elseif	(formflag = 3)
	append	"\\nts0\d\data\fax\nordf",spoolfl2
	pack	prtname,"nordf",fhandle,".LST"
	pack	PdfFname from "nordf",fhandle
	Else
		if	(faxNumFlag = 1)                     .valid mailer fax?
		append	"\\nts0\d\data\fax\nrdm",spoolfl2
		pack	prtname,"nrdm",fhandle,".LST"
		pack	PdfFname from "nrdm",fhandle
		else
		append	"\\nts0\d\data\fax\nrdb",spoolfl2
		pack	prtname,"nrdb",fhandle,".LST"
		pack	PdfFname from "nrdb",fhandle
		endif
	Pack	str35 From "C:\work\hdrfile.prn"
	erase	str35
	endif

	append	fhandle,spoolfl2
	append	".cvr",spoolfl2
	reset	spoolfl2
	splopen	spoolfl2
	reset	faxname,45
blankc
	cmatch	B1,faxname
	if equal
		bump	faxname,-1
		goto blankc if not eos
	else
		lenset	faxname
		reset	faxname,1
	endif
	reset	attchlst
	move	C1,N2
	unpack	date,mm,dd,yy
	clock	time,time
	clear	str5
	append	time,str5
	reset	str5
	write 	output2,seq;faxname
	add   	c1 to faxkount
.......................................
.This is a total cheat.  I am creating a shortened cover page that only includes the fax information.
.Backing up existing version of Faxfile.lst.  Creating a new Cover sheet using Faxfile.lst.
.Using Binary option to combine spool file and new Cover sheet.  Giving this new combined file the
.same name as original spool file.  Renaming original Faxfile.lst back to Faxfile.lst.
	print	"^[D",longdist,faxtele,"^[N",faxname:
		"^[SOrders"," ^]"
	splclose
	prtclose Laser
	erase	"C:\WORK\faxfile.sav"

	if	(EMAILfLAG <> YES)
	pack	Str35 from "C:\WORK\Faxfile.prn"
	FINDFILE 	str35,WRITE=Str25
		if	zero			.file found				
		rename	"C:\WORK\Faxfile.prn","C:\WORK\faxfile.sav"
		endif
	PRTOPEN 	Laser2,"faxfile",""
	eLSE
	PRTOPEN 	Laser2,"PDF995","Coverpage.pdf"
	ENDIF
	prtpage	Laser2;*UNITS=*HIENGLISH;
	IF	(formflag = 3)          .Fulfillment
		prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
			*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
			*p=6014:443,"Oakland, CA 94612-1429":
			*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
			*p=5980:643,"A Division of Names in the News"
	
		prtpage	Laser2;*Pictrect=*off,*PICT=0:975:0:5700:NINLogo

	Else
		IF	(OcompID = "P")
		prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
			*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
			*p=6014:443,"Oakland, CA 94612-1429":
			*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
			*p=5980:643,"A Division of Names in the News"
	
		Else
		prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
		endif
	endif

	move	"1500",row
	if	(formflag = 2 | formflag = 3)                   .owner or fulfillment
	prtpage	Laser2;*pcolumn2:row,*font=font7,"  LIST ORDERS"
	else
	prtpage	Laser2;*pcolumn2:row,*font=font7,"LIST ORDER CONFIRMATION"
	endif
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	if	(EmailFlag = Yes)
	prtpage	Laser2;*pcolumn2:row,*font=font8,"VIA Email"
	else
	prtpage	Laser2;*pcolumn2:row,*font=font8,"VIA FACSIMILE"
	endif
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,*font=font9,"Date:"
	prtpage	Laser2;*pcolumn1:row,today
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,"To:"
	prtpage	Laser2;*pcolumn1:row,faxname
	call	Trim using faxattn
	if (faxattn <> "")
		add	eightlpi,row
		add	eightlpi,row
		add	eightlpi,row
		prtpage	Laser2;*pcolumn:row,"Attn:"
		prtpage	Laser2;*pcolumn1:row,faxattn
	endif
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
.list managment ?	
	if	(LStmgt <> "06"  & LStmgt <> "19"  & LStmgt <> "27"  & LStmgt <> "28"  )
		IF	(formflag = 3)
		prtpage	Laser2;*pcolumn:row,"From:"
		prtpage	Laser2;*pcolumn1:row,"Order Requests"
		else
		prtpage	Laser2;*pcolumn:row,"From:"
		prtpage	Laser2;*pcolumn1:row,"Brokerage"
		endif
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,owncnt," Orders Enclosed"
	if (ownscnt > 0)
		add	eightlpi,row
		add	eightlpi,row
		add	eightlpi,row
		prtpage	Laser2;*pcolumn:row,ownscnt," Sample(s) Enclosed"
		if (owncnt > 1)
			add	eightlpi,row
			add	eightlpi,row
			add	eightlpi,row
			prtpage	Laser2;*pcolumn:row,"Note: Sample(s) May need to be attached to multiple Orders."
		endif
	endif
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,"Please call if you do not receive all pages."
	
	Else
	
	prtpage	Laser2;*pcolumn:row,"FROM:"
		IF	(OcompID2 = "P")
		prtpage	Laser2;*pcolumn1:row,"Pacific Lists Inc."
		Else
		prtpage	Laser2;*pcolumn1:row,"Names in the News"
		endif
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn1:row,"List Management Dept."
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn1:row,"Tel:  ",nfaxtel2
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,mlrcnt," order confirmation(s) included."
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,"PLEASE NOTE:"
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn:row,"This is a ",*ULON,"confirmation",*ULOff," of your brokerage order(s) for our Names in the News managed "
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn:row,"list(s). Please review this information and contact our list managers if you note any"
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn:row,"discrepancies or errors. We can be reached at (415) 989-3350. Our fax number is (415)"
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn:row,"433-7796."
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn:row,"Thank you for ordering Names in the News-managed lists. We appreciate your business."
	Endif
	prtclose Laser2
.Clean up
	clear   taskname
.begin patch 9.7
	Call	SenditOut
	Return

.	Path	Exist,"c:\windows"
.	if over			.nt/2000
.		append	"!c:\winnt\system32\cmd.exe /c ",taskname
.	elseif (osflag = c6)	.XP
.		append	"!c:\windows\system32\cmd.exe /c ",taskname
.	else			.95/98
.		append	"!c:\command.com /c ",taskname
.	endif
..note this will not work easily under VISTA
.	append  "copy ",taskname
.	append  spoolfl2,taskname
.........................
.	append  " /b + C:\WORK\Faxfile.prn /b C:\WORK\faxfile2.prn",taskname
.	reset   taskname
.	execute taskname
.	copyfile "C:\WORK\faxfile2.prn",spoolfl2
.	copyfile "C:\WORK\faxfile.sav","C:\WORK\Faxfile.prn"
.	erase	"C:\WORK\faxfile2.prn"
.	erase	"C:\WORK\faxfile.sav"
.End patch 9.7
	if (countr = C1)
		clear	outname
		pack taskname, NTWKPATH1,"nord"
		append taskname, OUTNAME
		append	OLON,OUTNAME
		append	".smp",OUTNAME
		reset	OUTNAME
		prepare	OUTPUT,OUTNAME
		move	C1,smpflag                           .we have sample file
	endif
	return

.prepare spool file.
prepspl
.	mailer copy?
	if	(Formflag = c1)
.prepare spool file. mailer copy
		if (FaxNumFlag = 1)
			pack	prtname,"nrdm",fhandle,".LST"
		else
			pack	prtname,"nrdb",fhandle,".LST"
		endif
	cmatch	YES,epsiflag
		if not equal
			move	YES,OformBFlag
		endif
	return
	endif
	move	NO,nosmpl
	clear	attchlst                       .start attachment list
	append	"nord",attchlst
	append	OLON,attchlst
	append	".LST",attchlst
	movelptr ATTCHLST,LPTCNT
prepsplB
.CLOSE LAST OWNERS "ATTATCHMENT FILE"
	if (smpflag = c1 | smpflag = c2)     .do we have file and/or data
		weof	output,seq                        .yes
		close	output                            .ditto
	endif                                       .done
.ARE WE FAXING? IF SO BUILD FILENAME TO HOLD FAX IMAGES
	compare	C1,countr            .very first record?
	if not equal               .no so continue process
LASTRUN
.if email create pdf instead
			if (smpflag = c1 | smpflag = c2)
				clear	outname
				pack taskname, NTWKPATH1,"nord"
				append taskname, OUTNAME
				append	smpown,OUTNAME
				append	".smp",OUTNAME
				reset	outname
				clear	spoolf
				append	"\\nts0\d\data\fax\nord",SPOOLF
				append	smpown,spoolf
				append	".sam",spoolf
				reset	spoolf
				trap	drewbreak giving error if IO
				open	file2,outname
				trap	io giving error if io
.
				move	Yes,FirstFlag1
				clear	N8
				read	file2,seq;dcx2
				if over
					move	YES,nosmpl
					goto nosmp
				endif
				trap RetryPrint2 if SPOOL
				prtopen	file1,"FAXFILE",""
				TrapClr	SPOOL
				close	file2
				clear	SMPArray
				trap	drewbreak giving error if IO
				open	file2,outname
				trap	io giving error if io
..Below logic used for testing purposes to save speed - KEEP IT!!!!
..To test:  Unrem all lines preceded with ".**" and rem all others until you hit "End Test Logic"
.**        loop
.**bigloop         read    file2,seq;DCX2
.**                until over
.**                clear   SMPIndex
.**                move    C1,SMPIndex
.**               loop
.**                        scan    DCX2 in SMPArray(SMPIndex)
.**                        goto BigLoop if equal
.**                        until   (SMPIndex = 50)
.**                        until   (SMPArray(SMPIndex) = "")
.**                        add     C1,SMPIndex
.**                repeat
.**                move    DCX2,SMPArray(SMPIndex)
.**                prtpage file1;*P2:2,DCX2
.**        repeat
.**        PRTclose file1,"FAXFILE",""
BigLoop
				call	PrtCloseFile
				loop
					read	file2,seq;DCX2
					until over
					call	TRIM using DCX2
AFTERTRIM
					if (DCX2 = "")
						goto printstop
					endif
					clear	SMPIndex
					move	C1,SMPIndex
					loop
						scan	DCX2,SMPArray(SMPIndex)
						goto BigLoop if equal
						until (SMPIndex = 50)
						until (SMPArray(SMPIndex) = "")
						add	C1,SMPIndex
					repeat
					move	DCX2,SMPArray(SMPIndex)
					pack	DCXFile,DCXPath,DCX2
.Print and Display First Page
					clear	N9
............................................
                                        CREATE  PICT1=70:495:70:620:
                                                DCXFile,BORDER=0,AUTOZOOM=0
                                        PICT1.GetPageCount GIVING N9
                                        if (FirstFlag1 = Yes)
                                                move    NO,FirstFlag1
                                                prtpage file1;*P2:2,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
                                        else
                                                prtpage file1;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
                                        endif
.Print and Display Additional Pages
					if (N9 > C1)    .Only Enter loop if more than one page
						clear	N8
						move	C1,N8   .Start with SECOND PAGE as first page already printed
						loop
							add	C1,N8
							until (N8 > N9)
                                                        CREATE  PICT1=70:495:70:620:
                                                                DCXFile,BORDER=0,AUTOZOOM=0,PAGE=N8
                                                        activate PICT1
                                                        prtpage file1;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
						repeat
					endif
				repeat
printstop
				PRTclose file1
				DESTROY PICT1
test3
				move	C0,LastFlag
				rename   "C:\WORK\Faxfile.prn",spoolf
			endif
	endif
.
NOSMP
	clear	outname
	pack taskname,NTWKPATH1,"nord"
	append taskname,OUTNAME
	append	OLON,OUTNAME
	append	".smp",OUTNAME
	reset	OUTNAME
	prepare	OUTPUT,OUTNAME
	move	C1,smpflag
	return
..prepare spool file. mailer copy
.prepspl2
.	if (FaxNumFlag = 1)
.		pack	prtname,"nrdm",fhandle,".LST"
.	else
.		pack	prtname,"nrdb",fhandle,".LST"
.	endif
.	cmatch	YES,epsiflag
.	if not equal
.		move	YES,OformBFlag
.	endif
.	return

PROCESS1
	move	" ",EXCHANGE
	move	" ",ENTIRE
	move	" ",TEST
	move	" ",CONT
	move	" ",CONT1
	move	" ",REPRT
	pack	MKEY,OMLRNUM,OCOBN
	move	"Process1-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	
	clear	BRCOMP
	clear	NBRKFLD
	pack	NBRKFLD,OBRKNUM,OBRKCNT
	call	Trim using NBRKFLD
	if (NBRKFLD <> "")
		move	"Process1-NBRKKEY",Location
		pack	KeyLocation,"Key: ",NBRKFLD
		call	NBRKKEY
		if not over
			move	MCOMP,MNAME
		endif
	endif
        move    compaddr,str35
	move	C0,NFIELD23
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
		move	"/M",NMODDESC
	else
		pack	NMODFLD,NSEL2DESC
		rep	zfill,NMODFLD
		move	"NMODKEY",Location
		pack	KeyLocation,"Key: ",NMODFLD
		call	NMODKEY
		if over
			move	"/M",NMODDESC
		else
			call	Trim using NMODDESC
		endif
	endif
	if (copy = 1 | copy = 2 | copy = 3)
		call	Trim using OFOSAVE
		if (OFOSAVE <> "")
			move	OFOSAVE,OFOCODE
		endif
	endif
	clear	MEDIA
	call	Trim using OFOCODE
	if (OFOCODE <> "")
		move	C0,NFIELD23
		move	OFOCODE,OFOSAVE         *SAVE VARIABLE
		move	OFOCODE,NFIELD23
		move	MED0,MEDIA
		load	MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
			MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
			MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
			MED23,MED24,MED25,med26,med27,med28,med29
		clear	MEDTYPE
		move	YES,MEDTYPE
		load	MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
			YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES,no,no
	endif
.SAMPLE
	clear	SAMPLE
	clear	NSMPDES1
	call	SAMPLE
	clear	RTCNTCT
	clear	COMSLCT
	clear	RTCOMP
	clear	RTCITY
	clear	RTSTATE
	clear	RTZIP
	clear	RTTELE
	clear	CORTN
	clear	CONTDTE
	clear	CONT
	clear	CONTQTY
	clear	MEDMEMO
	clear	FLAGPAID
	if (OCOMSLCT = "C")		.COMSELECT OVERLAY?
		call	COMSLCT
	elseif (OCOMSLCT = "L")		.LIFESTYLE OVERLAY?
		call	LIFESTYL
	elseif (OCOMSLCT = "I")		.IC SYSTEMS OVERLAY?
		call	ICSYSTEM
	endif
	pack	QTYOUT,QTYMSK
	call	Trim using OQTY
	move	OQTY,QTYNUM
	edit	QTYNUM,QTYOUT
	if (OCCODE = "1")		.CONTINUATION ORDER
		call	CONTIN
	elseif (OCCODE = "2")		.CONTINUATION ORDER/NO OMIT
		call	CONTIN1
	endif
	if (GUARCODE = "6" | GUARCODE = "7" | GUARCODE = "8" | GUARCODE = "9")	.PREPAID ORDER?????
		call	PREPAID
	endif
	bump	OODNUM,4
	move	OODNUM,OFFEROUT
	move	ORTNNUM,NRTNFLD
	move	"Process1-NRTNKEY",Location
	pack	KeyLocation,"Key: ",NRTNFLD
	call	NRTNKEY
	clear	RTARCD
	unpack	RTTELE,RTARCD,RTEXT,RTPHONE
	clear	RTPHMASK
	move	"-",DASH
	call	Trim using RTARCD
	if (RTARCD <> "")
		pack	RTPHMASK from LP,RTARCD,RP,B1,RTEXT,DASH,RTPHONE
	else
		clear	RTPHMASK
	endif
	if (ORTNNUM <> "2531")
		clear	str45
		clear	str45a
		move	MCOMP,str45
		move	RTCOMP,str45a
		rep	uplow,str45
		rep	uplow,str45a
		reset	str45
		reset	str45a
		if (str45 <> str45a)
			call	CHNGRET
		endif
	endif
	if (OMLRNUM = "0677" | OMLRNUM = "0210" | OMLRNUM = "0053" | OMLRNUM = "0702" | OMLRNUM = "0965" | OMLRNUM = "1361")
.USE MLR.OFR DESC ON RET-TO
		call	USEOFR
	endif
	if (ANS = "R")
.REPRINT
		call	REPRT
	endif
TEST
	move	C0,NFIELD23           *CLEAR FIELD
	move	OTOCODE,NFIELD23
	if (NFIELD23 = 1 | NFIELD23 = 2)
		move	"X",TEST
	endif
	move	"0",NFIELD23
	move	OELCODE,NFIELD23
	if (NFIELD23 = 1)
		call	ENTRENT
	elseif (NFIELD23 = 2)
		call	EXCHANG1
	elseif (NFIELD23 = 3)
		call	ENTIRE
	endif
	call	OPRINT1
	return

SAMPLE
	move	C0,NFIELD23
	move	OSCODE,NFIELD23
	move	"                          ",SAMPLE
	call	Trim using OSCODE
	if (OSCODE <> "" & OSCODE <> "0")
		branch	NFIELD23,SAM1,SAM2,SAM3
		clear	SAMPLE
	endif
	return
PHONE
	unpack	OWNFAX,str3,str2,str1,str4
	call	Trim using str3
	if (str3 <> "")
		pack	FAX1,"(",str3,")",str2,str1,"-",str4
	else
		pack	FAX1,str2,str1,"-",str4
	endif
	unpack	OWNTELE,ARCD,EXT,PHONE
	move	"-",DASH
	match	"   ",ARCD
	if not equal
		move	"(",LP
		move	")",RP
	endif
	return
PREPAID
	move	"**",FLAGPAID
	return
SAM1
	move	"Sample enclosed",SAMPLE
	match	Z3,OSAMCDE
	return if equal
	clear	NSMPFLD
	move	MCOMP,MlrNameHold
	move	MNAME,MlrMNameHold
.
	move	"SAM1-COMPKEY3",Location
	pack	COMPFLD3,OMLRNUM
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	pack	NSMPFLD,COMPNUM,OSAMCDE
.Need to refresh MCOMP
	move	MlrNameHold,MCOMP
	move	MlrMNameHold,MNAME
	rep	zfill,NSMPFLD
	move	"SAM1-NSMPKEY",Location
	pack	KeyLocation,"Key: ",NSMPFLD
	call	NSMPKEY
	return
SAM2
	move	"Sample to follow",SAMPLE
	return
SAM3
	move	"Sample previously cleared",SAMPLE
	return
ENTRENT
	move	"X",ENTIRE
	move	"        ",EXCHANGE
	return
EXCHANG1
	match	"         ",OEXQTY
	if not equal
		return
	endif
	move	"Exchange",EXCHANGE
	return
ENTIRE
	move	"Exchange",EXCHANGE
	move	"X",ENTIRE
	return
OPRINT1
	if (faxflag <> 2)
		prtpage	Laser;*p=2000:425,*font=fontO12b,REVDATA
	endif
	prtpage	Laser;*p=2000:810,*font=fontO12b,REPRT	???????????????????????
	prtpage	Laser;*p=2000:645,BILDDATA
	prtpage	Laser;*p=625:800,*font=fontO10,Olrn
	prtpage	Laser;*p=3125:800,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	prtpage	Laser;*p=1000:988,OMLRPON
	cmatch	B1,BRCOMP
	if not eos
		branch	FORMFLAG OF BRKOK,BRKNTOK,BRKNTOK,BRKOK,brkntok
		branch	copy of brkntok,brkntok,brkok
		goto brkok
brkntok
		clear	careof
		clear	str45
		clear   str35
		clear	attn
		clear	brcntct
		clear compaddr
		clear brcity
		clear brstate
		clear brzip
		goto	printmlr
brkok
		move	"c/o",careof
		cmatch	B1,BRCNTCT
		if equal
			clear	attn
		else
			move	"Attn: ",attn
		endif
		move	BRCOMP,str45
		move    str35,compaddr
printmlr
		prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
		prtpage	Laser;*p=1000:1176,MNAME
		prtpage	Laser;*p=1000:1351,careof,b1,str45
		prtpage	Laser;*p=3810:1351,attn,BRCNTCT
   	prtpage	Laser;*p=1000:1526,compaddr
	   if (brCITY = "")
	   prtpage	Laser;*p=1000:1701,"                            "
		else
		prtpage	Laser;*p=1000:1701,brCITY,comma,brSTATE," ",brZIP
		endif
		goto OPRINT1B
           endif
OPRINT1A
	branch	FORMFLAG TO DOCC,WIPECC,WIPECC,DOCC,wipecc
	branch	COPY OF WIPECC,WIPECC
	goto docc
wipecc
	clear	str15
	goto	prntmlr1
docc
	move	mccto,str15
prntmlr1
	branch	FORMFLAG OF prntmlr2,prntmlr3,prntmlr3,prntmlr2,prntmlr3
	branch	copy of prntmlr3,prntmlr3,prntmlr3
prntmlr2
	prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
	prtpage	Laser;*p=1000:1176,MNAME
	prtpage	Laser;*p=1000:1351,MCOMP,b5,str15
	prtpage	Laser;*p=1000:1526,compaddr
	prtpage	Laser;*p=1000:1701,COMPCITY,", ",COMPSTATE," ",COMPZIP
	prtpage	Laser;*p=1000:1876,OFDESC
	prtpage	Laser;*p=125:2020,"##",OFFEROUT
	prtpage	Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
	goto oprint1b2
prntmlr3
	prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
	prtpage	Laser;*p=1000:1351,MCOMP,b5,str15
OPRINT1B
	prtpage	Laser;*p=1000:1876,OFDESC
	prtpage	Laser;*p=125:2020,"##",OFFEROUT
	prtpage	Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
oprint1b2
   branch	FORMFLAG TO prtlist,PRTOWN,PRTOWN,PRTOWN,prtown
	compare	C0,copy
	goto prtlist if equal
prtown
.
	prtpage	Laser;*p=1000:2281,OWNLONM
	prtpage	Laser;*p=125:2456,"##",OLON
	prtpage	Laser;*p=1000:2456,OWNOCPY,B1,LP,ARCD,RP,EXT,DASH,PHONE,"   Fax: ",FAX1
	prtpage	Laser;*p=1000:2631,OWNLOSA
	call	Trim using OWNLOCTY
	if (OWNLOCTY <> "")
		pack	taskname,OWNLOCTY,", ",OWNLOS," ",OWNLOZC
	else
		pack	taskname,OWNLOS," ",OWNLOZC
	endif
	prtpage	Laser;*p=1000:2806,taskname
	prtpage	Laser;*p=1000:3006,NFULCOMP
	goto prtlist
prtlist
	prtpage	Laser;*p=1000:3211,O1DES
	if (NSEL2SPRICE > C0)
		unpack	NSEL2SPRICE,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
		call	Trim using NSEL2NAME
		pack	taskname,NSEL2NAME," @ ",str9,NMODDESC
	else
		pack	taskname,NSEL2NAME
	endif
	prtpage	Laser;*p=125:3399,"##",OLNUM
	prtpage	Laser;*p=1000:3399,taskname
	cmatch	B1,onetfm
	if  equal
		goto pqty
	endif
	move	C0,N2
	move	onetper,N2
	compare	C0,N2
	if not equal
		cmatch	NO,onetfm
		if equal
			prtpage	Laser;*p=1000:4903,*font=FontO7Dot5I,"Per List Owner - Gross Billing No Deductions"
		endif
		cmatch	"F",onetfm           ."F" = flat/volume discount
		if not equal               .this one is a net
			prtpage	Laser;*p=1000:4903,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
			prtpage	Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
		else                                 .this is flat/volume
			prtpage	Laser;*p=1000:4903,*font=Font08bI,onetper,"% Net Arrangement",*font=Font08I,", Run charge @ $",onetrc,"/m"
			prtpage	Laser;*p=1000:5033,"No Deducts, No CV required."

		endif
	endif
pqty
.........................................
	prtpage	Laser;*p=1000:3587,*font=fontO10,QTYOUT
	match	"         ",OEXQTY
	goto OPRINT2 IF EOS
	goto OPRINT2 IF EQUAL
	prtpage	Laser;*p=3000:3587,"SEE BELOW"
	goto OPRINT3
OPRINT2
	match	"EXCHANGE",EXCHANGE
	goto REALPPM IF NOT EQUAL
	if (NSEL2PRICE <> C0)
		goto REALPPM
	endif
	prtpage	Laser;*p=3000:3587,"EXCHANGE"
	goto OPRINT3
REALPPM
	unpack	NSEL2PRICE,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	pack	taskname,str9,NMODDESC," ",EXCHANGE
	prtpage	Laser;*p=3000:3587,taskname
OPRINT3
	deleteitem PackData,0
	pack	NSEL3FLD1,"01X1",OLRN
	move	"NSEL3AIM",Location
	pack	KeyLocation,"Key: ",NSEL3FLD1
	call	NSEL3AIM
	loop
		until over
		if (NSEL3CODE = "A")
			pack	NADDFLD,OLNUM,NSEL3NUM
			move	"NADDKEY",Location
			pack	KeyLocation,"Key: ",NADDFLD
			call	NADDKEY
			if not over
				pack	NREFFLD,"A",NADDNUM
				move	"NREFKEY",Location
				pack	KeyLocation,"Key: ",NREFFLD
				call	NREFKEY
				pack	NMODFLD,NADDDESC
				rep	zfill,NMODFLD
				move	"NMODKEY",Location
				pack	KeyLocation,"Key: ",NMODFLD
				call	NMODKEY
				pack	taskname,NREFDESC,NSEL3PRICE,NMODDESC
				insertitem PackData,0,taskname
			endif
		elseif (NSEL3CODE = "L")
			pack	NSLTFLD,OLNUM,NSEL3NUM
			move	"NSLTKEY",Location
			pack	KeyLocation,"Key: ",NSLTFLD
			call	NSLTKEY
			if not over
				pack	NREFFLD,"L",NSLTNUM
				move	"NREFKEY-2",Location
				pack	KeyLocation,"Key: ",NREFFLD
				call	NREFKEY
				pack	NMODFLD,NSLTDESC
				rep	zfill,NMODFLD
				move	"NMODKEY-2",Location
				pack	KeyLocation,"Key: ",NMODFLD
				call	NMODKEY
				pack	taskname,NREFDESC,NSEL3PRICE,NMODDESC
				insertitem PackData,0,taskname
			endif
		endif
		move	"NSEL3KG",Location
		call	NSEL3KG
	repeat
	PackData.GetCount giving howmany
	if (howmany > C0)
.		move	"625",N8
.Sales wants this moved over.  They will bitch about it later, no doubt.
		move	"1000",N8
		move	"3587",N9
		for result,"1",howmany
			if (result = 7)
				move	"3000",N8
				move	"3587",N9
			endif
			getitem	PackData,result,taskname
			unpack	taskname,NREFDESC,NSEL3PRICE,NMODDESC
			if (NSEL3PRICE > 0)
				unpack	NSEL3PRICE,str5,str3
				call	FormatNumeric using str5,str6
				call	Trim using NREFDESC
				call	Trim using NMODDESC
				pack	taskname,str6,str3,NMODDESC,B1,NREFDESC
				pack	taskname,NREFDESC," @ ",str6,str3,NMODDESC
			else
				pack	taskname,NREFDESC
			endif
			add	"188",N9
			prtpage	Laser;*p=N8:N9,taskname
		repeat
	endif
..........................
	prtpage	Laser;*p=1000:5251,OMLRKY
	prtpage	Laser;*p=1000:5501,MEDIA,"  ",MEDMEMO
	prtpage	Laser;*p=1000:5751,RTCNTCT
	prtpage	Laser;*p=125:5926,"##",ORTNNUM,B1,CORTN
	prtpage	Laser;*p=1000:5926,RTCOMP,B1,rtphmask
	call	Trim using RTADDR
	if (ORTNNUM = "0001")
		prtpage	Laser;*p=1000:6101,RTADDR,B1,OREUSE
	else
		prtpage	Laser;*p=1000:6101,RTADDR
	endif
	call	Trim using RTCITY
	if (RTCITY <> "")
. TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!

		if (RTNUM = "5318")
			pack	taskname,"incoming.files@donnelley.infousa.com"
		else
			pack	taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
		endif
. TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
	else
		pack	taskname,RTSTATE,B1,B1,RTZIP
	endif
	prtpage	Laser;*p=1000:6276,taskname
	prtpage	Laser;*p=125:6501,COMSLCT
	prtpage	Laser;*p=1250:6689,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.
	match	"  ",OSHP
	goto NOSHIP IF EQUAL
	goto NOSHIP IF EOS
	move	OSHP,NFIELD23
	move	SHIP0,SHIPdesc
	load	SHIPdesc FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
		SHIP6,SHIP7,SHIP8,SHIP9,ship10
	goto OPRINT4
NOSHIP
	clear	SHIPdesc
OPRINT4
	prtpage	Laser;*p=2500:6689,SHIPdesc
	prtpage	Laser;*p=1250:6876,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	prtpage	Laser;*p=5640:6733,TEST
	prtpage	Laser;*p=5640:7116,CONT1
	prtpage	Laser;*p=5640:7366,CONT
	prtpage	Laser;*p=5640:7616,OLRNCO,"  ",CONTDTE," ",CONTQTY
	prtpage	Laser;*p=5640:7866,ENTIRE
DISREGO
	move	"7126",result
	move	OLRN,NSPEFLD
	rep	ZFILL,NSPEFLD
	move	"REGO-NSPEKEY",Location
	pack	KeyLocation,"Key: ",NSPEFLD
	call	NSPEKEY
	call	TRIM using DESC002
	call	PARSITUP using line1,DESC002,C1
	call	PARSITUP using line2,DESC002,C1
	call	PARSITUP using line3,DESC002,C1
	call	PARSITUP using line4,DESC002,C1
	call	PARSITUP using line5,DESC002,C1
	call	PARSITUP using line6,DESC002,C1
	call	PARSITUP using line7,DESC002,C1
	call	PARSITUP using line8,DESC002,C1
	call	PARSITUP using line9,DESC002,C1
	call	PARSITUP using line10,DESC002,C1
	call	PARSITUP using line11,DESC002,C1
	call	PARSITUP using line12,DESC002,C1
	call	PARSITUP using line13,DESC002,C1
	call	PARSITUP using line14,DESC002,C1
	call	SPCLNSTO                           SPEC INSTRUC ROUTINE
	move	line2,line1
	call	SPCLNSTO
	move	line3,line1
	call	SPCLNSTO
	move	line4,line1
	call	SPCLNSTO
	move	line5,line1
	call	SPCLNSTO
	move	line6,line1
	call	SPCLNSTO
	move	line7,line1
	call	SPCLNSTO
	move	line8,line1
	call	SPCLNSTO
	move	line9,line1
	call	SPCLNSTO
	move	line10,line1
	call	SPCLNSTO
	move	line11,line1
	call	SPCLNSTO
	move	line12,line1
	call	SPCLNSTO
	move	line13,line1
	call	SPCLNSTO
	move	line14,line1
	call	SPCLNSTO
	goto TYPIST
.
. ROUTINE FOR SPECIAL INSTRUCTION PRINT
.
SPCLNSTO
	add	"188",result
	prtpage	Laser;*p=125:result,*font=fontO10n,line1,*font=fontO10
	return
TYPIST
	branch	hotflag of cntques,needcnt
cntques
	branch	copy of discon2B,discon2B,discon2B
needcnt
	move	C0,NFIELD23
	move	OCOCODE,NFIELD23
	type	OCOCODE
	goto 	CON10 IF NOT EQUAL
	move	OCOCODE,str2
	goto 	DISCON2A
CON10
	clear	str2
	move	OCOCODE,str2
	rep	"A0B1C2D3E4F5G6H7I8J9",str2
	type	str2
	goto 	CON20 IF NOT EQUAL
	move	str2,NFIELD23
	add	"10",NFIELD23
	goto 	DISCON2A
con20
	clear	str2
	move	OCOCODE,str2
	rep	"K0L1M2N3O4P5Q6R7S8T9",str2
	type	str2
	goto 	CON30 IF NOT EQUAL
	move	str2,NFIELD23
	add	"20",NFIELD23
	goto 	DISCON2A
CON30
	clear	str2
	move	OCOCODE,str2
	rep	"U0V1X2Y3Z4",str2
DISCON2A
	clear	NCNTFLD
	move	C1,NCNTPATH
	move	C3,NCNTLOCK
	move	str2,NCNTFLD
	if 	(NCNTFLD = "" or NCNTFLD = "  " or NCNTFLD = " ")
		move	"00",NCNTFLD
	endif
	clear	cntphone
	clear	cnt
	move	"DISCON2A-NCNTKEY",Location
	pack	KeyLocation,"Key: ",NCNTFLD
	call	NCNTKEY
	if over
		move	"NOTHING",CNT
		move	"(415)989-3350",CNTPHONE
		move	"NAMES@NINCAL.COM",INTRNET
		goto CNTEXIT
	endif
	scan	"()",cntphone
	if equal
		clear	cntphone
	endif
	move	cntname,cnt
	clear	intrnet
	reset	cnt
	scan	"BILLING",cnt
.
	if not equal
		reset	CNT
		scan	"Billing",CNT
		if equal
			reset	CNT
			goto cntexit
		endif
	else
		reset	CNT
		goto cntexit
	endif
	reset	CNT
	call	RemoveChar using cntname,B1
	call	Trim using cntname
	if (cntname = "")
		goto cntexit
	endif
	IF	(CntComp = "2")
	pack	intrnet,cntname,"@pacificlists.com"
	Else
	pack	intrnet,cntname,"@nincal.com"
	endif
cntexit
	prtpage	Laser;*p=5625:9500,CNT
	prtpage	Laser;*p=5625:9700,cntphone
	prtpage	Laser;*p=5625:9900,intrnet
DISCON2B
	if (hotflag = 2 & copy = 3)
		move	"Hot Print",hotprt
	else
		clear	hotprt
	endif
.note:  Because of following instruction, which skips verbage below it,
.       I am going to allow two more lines of special instructions to be
.       printed.  This will need to be amended in case logic is reinstated.  ASH 05/20/99 PATCH 7.4
	goto REG                    .11/5/93 dlh per sa
	match	"06",MSLSPER
	goto REG IF EQUAL
	match	YES,MEDTYPE
	if equal
		prtpage	Laser;*p=125:10312,"Mailer will not pay for names identified as errors,"
		prtpage	Laser;*p=125:10312,"bad zips, foreign, non-personal, intrafile or "
		prtpage	Laser;*p=125:10312,"family duplicates, or hits to DMA MPS file."
		goto PRTDONE
	endif
REG
yesnumb
	if (hotflag = 2 & copy = 3)
		move	"Hot Print",hotprt
	else
		clear	hotprt
	endif
	clear	taskname
	call	TRIM using DESC001
	if (DESC001 <> "")
		scan	"After this",DESC001
		if not equal
			reset	DESC001
			append	"After this order is fulfilled: ",taskname
			append	DESC001,taskname
			reset	taskname
		else
			pack	taskname,DESC001
		endif
	endif
	call	PARSITUP using line1,taskname,C1
	call	SPCLNSTO
	call	PARSITUP using line2,taskname,C1
	move	line2,line1
	call	SPCLNSTO
	prtpage	Laser;*p=125:10400,ODOWJ,SLASH,REVTYP,B1,hotprt
numbdone
	branch	FORMFLAG TO PRTDONE,CHKSAM,PRTDONE,PRTDONE,prtdone
	goto PRTDONE
.CHKSAM - LIST OWNER COPY CHECK FOR SAMPLE
CHKSAM
	move	C0,N1
	move	OSCODE,N1
	branch	N1,CHKSAM1          .SAMPLE CODE = 1 SAMPLE ENCLUSED?
	goto PRTDONE               .NO
CHKSAM1
	type	OSAMCDE
	goto PRTDONE IF NOT EQUAL
	branch	formflag  to prtdone,smpwrt,prtdone,prtdone,prtdone
	goto prtdone
smpwrt
	move	"S",str1
.START PATCH 9.34 REPLACED LOGIC
.	move	".dcx",str4
	move	".TIF",str4
.END PATCH 9.34 REPLACED LOGIC
.START PATCH 9.38.2 REPLACED LOGIC
.	pack	str12,str1,OMLRNUM,OSAMCDE,str4
.	scan	str12,attchlst
	move	MCOMP,MlrNameHold
	move	MNAME,MlrMNameHold
.
	move	"smpwrt-COMPKEY3",Location
	pack	COMPFLD3,OMLRNUM
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	pack	str14,str1,COMPNUM,OSAMCDE,str4
	scan	str14,attchlst
.Need to refresh MCOMP
	move	MlrNameHold,MCOMP
	move	MlrMNameHold,MNAME
	goto prtdone if equal              .already in list
	reset	attchlst
	setlptr	ATTCHLST,LPTCNT
	endset	attchlst
	append	",",attchlst
	append	str14,attchlst            .add to list
	compare	C2,faxflag
	if equal          .yes
		move	"                                        ",APIFileName
		clear	APIFileName
		pack	APIFileName,dcxpath,str14,hexzero
		call	FindFirstFile
		if (APIResult <> 0 & APIResult <> hexeight)
			write	output,seq;str14
			add	C1,ownscnt                .keep track of count
			move	C2,smpflag
		endif
	endif
PRTDONE
	reset	attchlst
	add	C1,COUNT
	display	*P14:20,*EL,"NINCAL ORDERS SPOOLED : ",COUNT
	if (hotflag = 2)	.Hot Print
		add	C1,COPY
	endif
	return

. CHNGRET - PRINT MAILER COMPANY AS RETURN TO CONTACT.
CHNGRET
	move	MCOMP,RTCNTCT
	move	"c/o",CORTN
	return
. USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
	move	OFDESC,RTCNTCT
	move	"c/o",CORTN
	return
. CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
	move	"X",CONT
	pack	CONTDTE,OODTECOM,SLASH,OODTECOD,SLASH,OODTECOY
	move	QTYMSK,CONTQTY
	call	Trim using OQTYCO
	move	OQTYCO,QTYNUM
	edit	QTYNUM,CONTQTY
	return
. CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
	move	"X",CONT1
	return
. REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
	move	"*** REPRINT ***",REPRT
	return
COMSLCT
	move	"**CC: CONSUMER DIRECT",COMSLCT
	return
. LIFESTYL - LIFESTYLE OVERLAY.
LIFESTYL
	move	"CC:LIFESTYLE SELECTOR",COMSLCT
	return
.
ICSYSTEM
	move	"**CC: IC SYSTEMS **",COMSLCT
	return
FULEOJ
.find company in record
		Move	c0,n3                                  .set to zero
		for	CountIndex,"1","999"
		MOve	CountRecord(countindex).CntREcComp to N6               .get company
		MOve	CountRecord(countindex).CntREcCount to n3               .get count
		If	(n3 > c0)
			move	N6,COMPFLD
			rep	zfill,COMPFLD
			move	C1,COMPPATH
			move	"FULEOJ-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			if over
				// do nothing
			else
				if (COMPSVBFLG = "T")  // make sure it's a fulfillment house
					clear	faxattn
.
						Packkey CNCTFLD2 to "01X",COMPNUM
						Call	CNCTAIM
						loop
						until over
						until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
							call	CNCTKG
						repeat
.
					call	Trim using CNCTFNAME
					if (CNCTFNAME = "")
						move	"Order Fulfillment",faxattn
					else
						move	CNCTFNAME,faxattn
					endif
					move	C0,ownscnt
					move	C0,owncnt
					MOve	CountRecord(countindex).CntREcCount to owncnt               .get count
					
					move	C2,faxflag        .yes set fax fulfilment flag on.
					call	Trim using COMPFAX
					move	COMPFAX,FAXTELE
					fill	B1,faxname
					clear	faxname
					move	COMPCOMP,faxname
					move	COMPNUM,holdccto
					move	CNCTFNAME,holdcccnt
					move	COMPCOMP,holdcccmp
.					pack	fhandle,"f",COMPNUM
					pack	fhandle,COMPNUM
					call	prepfax
				endif
			endif
		endif
	repeat
	return

contacts
	move	"Epsilon",faxname
	reset	epsicon2
	scan	OMLRNUM,epsicon2
	if equal
		move	C2,epsicvr
		goto	epsicons
	endif
	reset	epsicon4
	scan	OMLRNUM,epsicon4
	if equal
		move	C4,epsicvr
		goto	epsicons
	endif
Epsicons
	branch	epsicvr of karen,heather,dave,evelyn
	move	"Epsilon",faxattn
karen
	move	"Karen Forbes",faxattn

	return
dave
	move	"Teri Morrow",faxattn
	return
heather
	move	"Teri Morrow",faxattn
	return
evelyn
	move	"Evelyn Raymond",faxattn
	return

prtordfrm
	call	PrtOpenFile
	clear	str2
	pack	str2,OSALES10,OSALES
	if (OformBflag = "Y" & (str2 = "06" | str2 = "19" | str2 = ""| str2 = "27" | str2 = "28" ))          .list management
		call	prtordfrmGuiA
	else
		call	prtordfrmGuiB
	endif
	return

PrtOpenFile
.printing  not faxing - no or invalid fax number
	if (FirstFlag = YES)
		call	PrtCloseFile
		if (PDFFlag = 1)
			PRTOPEN	Laser,"PDF995",inpname
			return
		endif
		if (PrintFlag = C1)
			trap RetryPrint if SPOOL
			if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
				PRTOPEN Laser,"\\NTS0\Laser8",prtname
			ELSEIF (OSFLAG = "3" or OSFLAG = "4")		.95/98
				PRTOPEN Laser,"Laser8",prtname
			else   .(osflag = c0)         .Don't know prompt for printer
				PRTOPEN Laser,"-",prtname
			endif
			move	C0,PrintFlag
			move	C1,FaxNumFlag
		else
			if	(emailFlag = No)
			PRTOPEN Laser,"faxfile",""
			else
.			PRTOPEN Laser,"faxfile","",NoPRint,SpoolFile="faxfile.prn"
			PRTOPEN Laser,"PDF995",PdfFname
			endif
		endif
		TrapClr	SPOOL
		move	NO,FirstFlag
		if (hotflag = 1 & formflag = 1 & epsiflag = YES)
.Full Run, Mailer Run, New Broker/Mailer combo, Epsilon as Broker
			prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
			call	newcover2
			prtpage	Laser;*NEWPAGE
		endif
	else
		prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
		prtpage	Laser;*NEWPAGE
	endif
	return

PrtCloseFile
	prtclose Laser
	call	Trim using prtname
	if (prtname <> "")
.HotOrders will always have this value filled.  This is for regular run
		if (hotflag <> 2)
			if (!externalmode)
.HotOrders should not rename first.  This is for regular run.
				pack	taskname,"\\nts0\d\data\fax\",prtname          ."
				pack	APIFileName,taskname,hexzero
				call	FindFirstFile
				if (APIResult <> 0 & APIResult <> hexeight)
					erase	taskname
				endif
.....................................
				pack	APIFileName,"C:\WORK\Faxfile.prn",hexzero
				call	FindFirstFile
				if (APIResult <> 0 & APIResult <> hexeight)
					rename   "C:\WORK\Faxfile.prn",taskname
				endif
.Following will prevent files from being erased/overwritten
				clear	prtname
			endif
		else
			pack	APIFileName,prtname,hexzero
			call	FindFirstFile
			if (APIResult <> 0 & APIResult <> hexeight)
				erase	prtname
			endif
		endif
	endif
	return

IO
         TRAPCLR    IO
         NORETURN
         DISPLAY   *P1:23,*EL,ERROR
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE:
                   SIX,SEVEN,EIGHT,NINE
ZERO
         DISPLAY   *P1:24,"UNDEFINED FILE ERROR",error,*W2;
         GOTO      IOEXIT
ONE
         DISPLAY   *P1:24,"ORDER FILE ERROR",error,*W2;
         GOTO      IOEXIT
TWO
         DISPLAY   *P1:24,"CLOCK FILE ERROR",error,*W2;
         GOTO      IOEXIT
THREE
         DISPLAY   *P1:24,"NINMLR FILE ERROR",error,*W2;
         GOTO      IOEXIT
FOUR
         DISPLAY   *P1:24,"BROKER FILE ERROR",error,*W2;
         GOTO      IOEXIT
FIVE
         DISPLAY   *P1:24,"NINSPI FILE ERROR",error,*W2;
         GOTO      IOEXIT
SIX
         DISPLAY   *P1:24,"NINRTN FILE ERROR",error,*W2;
         GOTO      IOEXIT
SEVEN
         DISPLAY   *P1:24,"TDMCORD FILE ERROR",error,*W2;
         GOTO      IOEXIT
EIGHT
         DISPLAY   *P1:24," LABEL xxx FILE ERROR",error,*W2;
         KEYIN     *R,*P1:24,"DO YOU WANT ME TO CREATE FILE ",ANS;
         CMATCH    "Y" TO ANS
         GOTO      PREPLABL IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      EIGHT IF NOT EQUAL
         GOTO      IOEXIT
NINE
         DISPLAY   *P1:24,"ORDER xxx FILE ERROR",*W2;
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P60:24,*B,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOEXIT1 IF EQUAL
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT
         GOTO      ZERO
DREWBREAK
         NORETURN
         TRAP      IO giving error IF IO
         SCAN      "I03" IN ERROR
         GOTO      NOSMP IF EQUAL
         GOTO      ZERO
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "CLS"
PREPLABL
         GOTO      CLOCK
	listoff

DREWTEST
	MOVE	STR1,STR1
	RETURN
RetryPrint
		move    	"Nord002l May have a spool error.",MailSubjct
		Clear	MailBody
		Append	"Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print)",MailBody
		Append	CRLF,MailBOdy
		call 	EMAILIS
	 	if (PrintFlag = C1)
			if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
				PRTOPEN Laser,"\\NTS0\Laser8",prtname
			ELSEIF (OSFLAG = "3" or OSFLAG = "4")		.95/98
				PRTOPEN Laser,"Laser8",prtname
			else   .(osflag = c0)         .Don't know prompt for printer
				PRTOPEN Laser,"-",prtname
			endif
		else
			if	(emailFlag = No)
			PRTOPEN Laser,"faxfile",""
			else
.			PRTOPEN Laser,"faxfile","",NoPRint,SpoolFile="faxfile.prn"
			PRTOPEN Laser,"PDF995",PdfFname
			endif
		endif
		return
RetryPrint2
		move    "Nord002l May have a spool error trying to open faxfile.",MailSubjct
		Clear	MailBody
		Append    "Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print2)",MailBody
		Append	CRLF,MailBOdy
		call 	EMAILIS
		prtopen	file1,"FAXFILE",""
		return
.begin patch 9.7
SendItOut
.Problem under Vista droping to cmd

.	Path	Exist,"c:\windows"
.	if over			.nt/2000
.		append	"!c:\winnt\system32\cmd.exe /c ",taskname
.	elseif (osflag = c6)	.XP
.		append	"!c:\windows\system32\cmd.exe /c ",taskname
.	else			.95/98
.		append	"!c:\command.com /c ",taskname
.	endif
.	append  "copy ",taskname
.	append  spoolfl2,taskname
.Problem under Vista 

	erase	"C:\WORK\spoolmerge.lst"
	PRepare	AbsFileout,"c:\work\spoolmerge.lst",exclusive


	FINDFILE 	Spoolfl2,WRITE=Str25
	if	zero			.file found				

	OPen	ABSfilein,spoolfl2		.fax goodies

.	Loop
	read	AbsfileIN,seq;*ABSON,AbsData;
	Write	AbsFIleOut,seq;*LL,*ABSON,AbsData;
.	Until	Over
.	Write	AbsFIleOut,seq;*LL,*ABSON,AbsData;
.	Repeat	
	endif
	
	pack	taskname from "C:\work\faxfile.prn"
	FINDFILE 	taskname,WRITE=Str25
	if	zero			.file found				
	OPen	ABSfilein,"C:\work\faxfile.prn"		.goodies

	Loop
	read	AbsfileIN,seq;*ABSON,AbsData;
	Until	Over
	Write	AbsFIleOut,seq;*LL,*ABSON,AbsData;
	Repeat	
	Close	AbsFilein
	endif

	Weof	AbsFIleout,seq	
	Close	AbsFileOut

	pack	taskname from "C:\WORK\SpoolMerge.lst"
	FINDFILE 	taskname,WRITE=Str25
	if	zero			.file found				
	copyfile 	"C:\WORK\spoolmerge.lst",spoolfl2
	endif

..................................................
	clear	Str35
	pack	str35 from "C:\WORK\faxfile.sav"
	FINDFILE 	STr35,WRITE=Str25
	if	zero			.file found				
	copyfile "C:\WORK\faxfile.sav","C:\WORK\Faxfile.prn"
	endif
	erase	"C:\WORK\spoolmerge.lst"
	if	((formflag = c2 | FormFlag = c3) & EmailFlag = Yes)          . I think we have a winner
	call	debug
.wait for the PDF's

	pack	Str35 from "c:\work\pdf\coverpage.pdf"
	pack	Str55 from "c:\work\pdf\",PdfFname,".pdf"

CheckFile
	trap	WaitForEnd giving error if IO
	open	FileCheck,STR35,Exclusive	
	Close	FIleCHeck

	trap	WaitForEnd giving error if IO
	open	FileCheck,STR55,Exclusive	
	Close	FIleCHeck

	call	debug
	pack	MailAttach from str35,";",str55

.for testing	
		Move	"davidHerrick@nincal.com",Mailto
.		move	NfulEmail to MailTO
.for testing	
		Move	"Order Fulfillment",MailSubjct
		MOve	"ComputerRequests@nincal.com",MailFrom                      .keep copy just incase .. rule on that inbox to move
		MOve	"ComputerRequests@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
		clear	Mailbody
.for testing	
		append	NFUlEmail,Mailbody
		append	CRLF,Mailbody
		append	Str55,Mailbody
		append	CRLF,Mailbody
		reset	Mailbody
.for testing	
..First check 995 autolaunch settings
		call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
			"Parameters":
			"ProcessPDF":
			"\\nts0\c\apps\plb\code\pdftest.bat":
			result
			if (result = C0)
.Prepare Flag file
				prep	tempfile,"c:\progra~1\pdf995\flag.dat"
				write	tempfile,SEQ;"flag set"
				close	tempfile
			endif
		Call	PDF995Auto
.get file name chop off ,cvr		
.	call	debug
		call	SendMail
		return
	Else

.So add the orders to the cover
	PRepare	AbsFileout,"c:\work\spoolmerge.lst",exclusive

	FINDFILE 	Spoolfl2,WRITE=Str25
		if	zero			.file found				
	
		OPen	ABSfilein,spoolfl2		.coversheet


		Loop
		read	AbsfileIN,seq;*ABSON,AbsData;
		Until	Over
		Write	AbsFIleOut,seq;*LL,*ABSON,AbsData;
		Repeat	
		endif
	
	Clear	Str55
	pack	str55 from "\\nts0\d\data\fax\",prtname                        ."
	FINDFILE 	str55,WRITE=Str25
		if	zero			.file found				
		OPen	ABSfilein,str55
	
		Loop
		read	AbsfileIN,seq;*ABSON,AbsData;
		Until	Over
		Write	AbsFIleOut,seq;*LL,*ABSON,AbsData;
		Repeat	
		endif
		Close	AbsFilein
		Weof	AbsFIleout,seq	
		Close	AbsFileOut
		endif	

	pack	str35 from "C:\WORK\spoolmerge.lst"                        ."
	FINDFILE 	str35,WRITE=Str25
		if	Zero
		copyfile 	"C:\WORK\spoolmerge.lst",str55
		endif
.		Erase	SpoolFL2
		erase	str55
.	endif
.end patch 9.7
	erase	"C:\WORK\faxfile2.prn"
	erase	"C:\WORK\faxfile.sav"
	return
.Begin patch 9.7
WaitForEnd
		TrapClr	IO
.check the error if file does not exist just get out
		add	C1,Trapcount
		pause	c5
		noreturn
.		if	(trapcount > 240)   .20 min are you kidding me
		if	(trapcount > 60)   .5 min are you kidding me
		Pack	 MailSubjct,"Orders - ",str35,b1,str55
		Move	"CReques@nincal.com",MailFrom
		Move	"CReques@nincal.com",MailTO
		append	CRLF,MailBOdy
		append	str35,MailBody
		append	CRLF,MailBOdy
		append	str55,MailBody
		append	CRLF,MailBOdy
		append	"I am sorry I could not send the file",Mailbody
		reset	Mailbody
		Move	B1,Mailattach
		call	SendMail
		return
		endif
	
		goto	checkfile
.end patch 9.7





.Email I.S.
EMAILIS
.;.   Set the text message that is send with the attachments
	Reset	MailBOdy
	pack	Mailto,"InformationServices@nincal.com"
	Pack	MailFrom,"ComputerRequest@nincal.com"
	call	SendMail
	return

webGenerate routine DimPtr
	set	externalmode
	move	"2",FUNC
	pack	INPNAME from DimPtr,"M","WEB"
	move	"NORD002L",PROGRAM
	call	start
	return

	include	prtorderpage1.inc
	include	compio.inc
	include	cntio.inc
	include	NCRCIO.INC
	include	NRTNIO.INC
	include	NOWNIO.INC
	include	NINVIO.INC
	include	nordio.inc
	include	nspIIO.inc
	include	nspeio.inc
	include	nsmpio.inc
	include	NOFRIO.INC
	include	ncntio.inc
	include	hpio.inc
	INCLUDE	NSEL2io.INC
	INCLUDE	NSEL3io.INC
	INCLUDE	NADDIO.INC
	INCLUDE	NSLTIO.INC
	INCLUDE	NREFIO.INC
	INCLUDE	NMODIO.INC
	include	COMLOGIC.INC

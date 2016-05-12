.
. PURPOSE - READS INPUT file (NPRINT/TEMP)
.           AND PRINTS ORDER FORMS.
.
PC       EQU       0
	INCLUDE	COMMON.INC
	INCLUDE	CONS.INC
.patch9.37
	include	compdd.inc
	include	cntdd.inc
.         INC       NMLRDD.inc
.patch9.37
	include	ncntdd.inc
.	include	f:\library\develop\backups\NORDDD.INC
	INCLUDE	NORDDD.INC
	INCLUDE	NCRCDD.INC
	INCLUDE	NRTNDD.INC
	INCLUDE	NOWNDD.INC
.	include	f:\library\develop\backups\nowndd.inc
	INCLUDE	NINVDD.INC
.patch9.36
.	INCLUDE	NBRKDD.INC
.patch9.36
	INCLUDE	SHIPPING.INC
	include	nspIdd.inc
	include	nspedd.inc
	include	hp.inc
	INCLUDE	MEDIA.INC
	INCLUDE	nsmpdd.inc
	INCLUDE	NOFRDD.INC
	include winapi.inc
.START PATCH 9.43 REMOVED LOGIC
.	include NFULDD.INC
.END PATCH 9.43 REMOVED LOGIC
.START PATCH 9.36 - ADDED LOGIC
	INCLUDE	NSEL2DD.INC
	INCLUDE	NSEL3DD.INC
	INCLUDE	NADDDD.INC
	INCLUDE	NSLTDD.INC
	INCLUDE	NREFDD.INC
	INCLUDE	NMODDD.INC
.END PATCH 9.36 - ADDED LOGIC
	liston

release 	init    "9.62"    DLH  Use SendMail to find snt faxes
REldate	Init	"20 March 2008"
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
.release  init      "9.38.3"       ASH	09DEC2004 FAXFILE.PRN
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

.START PATCH 9.38.2 ADDED LOGIC
MlrNameHold	dim	75
MlrMNameHold	dim	75
.END PATCH 9.38.2 ADDED LOGIC
.START PATCH 9.36 - ADDED LOGIC
PackData DataList
.PriceFlag form	1
.END PATCH 9.36 - ADDED LOGIC
.CountIndex	const	"99999"
.begin patch 9.6
.06March2008 DLH
.CountArray holds the # of orders for a paricular owner or Fulfillment company the count is put on the coversheet
.Previosly the index was the owner of Fulfiment number which was 4 bytes, Now fulfillment numbers have reached 5 bytes and can go
.to 6 bytes owners will soon follow suit. there fore I am going to use a record
.CountIndex	const	"9999"
.CountArray	form	6(CountIndex)                     .needs to be as large as the largest fulfillment # DLH 13 Feb 08
N6a	Form	6
CountIndex	Form	3
CountRecord    Record         (999)                                   ;artificial cap of 999 companies
CntRecComp     Form            6              01-06              Company #
CntRecCount	Form	3	  07-09              Number of orders
	RecordEnd
.end patch 9.6
.CountArray	form	4(CountIndex)
.hexeight	integer	4,"4294967295"
cnt	dim	35
.begin patch 9.49
.intrnet	dim	46                .print contact's internet address
intrnet	dim	50                .print contact's internet address
.end patch 9.49
pict1	pict
. FILES.
Laser	pfile
.START PATCH 9.38 ADDED LOGIC
Laser2	pfile
.END PATCH 9.38 ADDED LOGIC
file1	pfile
file2	file
INPUT	FILE	FIXED=696,STATIC=12
INPUT2	FILE	.Used for Fulfillment & Overlay Passes
OUTPUT	FILE
output2	FILE
dfile	file
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
.START PATCH 9.38.4 ADDED LOGIC
PDFFlag	form	1	.Allows PDF Option
.END PATCH 9.38.4 ADDED LOGIC
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
.START PATCH 9.43 REPLACED LOGIC
.holdccto dim	4                    .used for list owner ccto break.
.holdcccnt dim	45                   .used for list owner ccto break. (Contact Name)
.holdcccmp dim	45                   .used for list owner ccto break. (Company Name)
holdccto dim	6                    .used for list owner ccto break.
holdcccnt dim	45                   .used for list owner ccto break. (Contact Name)
holdcccmp dim	55                   .used for list owner ccto break. (Company Name)
.END PATCH 9.43 REPLACED LOGIC
.START PATCH 9.50
ovrTEL1	init	"7732901789"
;ovrTEL1	init	"7737742975"
.End PATCH 9.50
ovrTEL2	init	"2033536661"
ovrTEL3	init	"6124816363"
ovroct1	form	2                number of orders
ovroct2	form	2                number of orders
ovroct3	form	2                number of orders
ovrnum	form	2                table index
faxflag	form	1                    .1=no, 2=yes.
FaxNumFlag form	1
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
.START PATCH 9.34 REPLACED LOGIC
.DCX	INIT	".DCX"
DCX	INIT	".TIF"
.END PATCH 9.34 REPLACED LOGIC
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
dcxpath	init	"\\nins1\e\data\samples\"	."
.START PATCH 9.38.3 REPLACED LOGIC
.FilePath init	"\\nts0\d\data\fax\"
FilePath init	"C:\WORK\"			."
.END PATCH 9.38.3 REPLACED LOGIC
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
.START PATCH 9.41 ADDED LOGIC
Font08I	font
Font08BI font
.END PATCH 9.41 ADDED LOGIC
.Create fonts to be used
sevenfive	form	"7.5"
.START PATCH 9.38.5 ADDED LOGIC
externalmode	integer 1
DimPtr		dim	^

.START PATCH 9.43 ADDED LOGIC
NFULNUM		DIM	6
NFULCOMP	DIM	55
NFULCNT		DIM	45
NFULFAX		DIM	10
.END PATCH 9.43 ADDED LOGIC

Start
.END PATCH 9.38.5 ADDED LOGIC

.begin patch 9.62
dlFiles 	datalist
DLresult 	form 9
DLndx 	form 9
dmFileName 	dim 80
.end patch 9.62

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
.START PATCH 9.41 ADDED LOGIC
	create	font08I,"Times New Roman",size=8,Italic
	create	font08bI,"Times New Roman",size=8,Bold,Italic
.END PATCH 9.41 ADDED LOGIC
.START PATCH 9.36 ADDED LOGIC
.Create work var
	create	PackData=1:1:1:1
.END PATCH 9.36 ADDED LOGIC

.START PATCH 9.38 ADDED LOGIC
font7	font
font8	font
font9	font
NINLogo	PICT
Blockout	PICT
Blockout1	PICT

	move	"750",column
	move	"1750",column1
	move	"3000",column2
	create	font7,"Helvetica",size=14,bold
	create	font8,"Helvetica",size=14,italic
	create	font9,"Arial",size=12
.START PATCH 9.38.5 REPLACED LOGIC
.	CREATE	NINLogo=3:13:30:50:
.		"\\nts0\c\netutils\NIN logo black outline.jpg"
	if (externalMode)
.Begin Patch 9.49
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
.End Patch 9.49

		CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
	else
.Begin Patch 9.49
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
.End Patch 9.49
		CREATE	NINLogo=3:13:30:50:
			"\\nts0\c\netutils\NIN logo black outline.jpg"
	endif
		CREATE  	Blockout=3:20:30:50:
			"\\nts0\c\netutils\blockout.tif"
		CREATE  	Blockout1=3:20:30:50:
			"\\nts0\c\netutils\blockout2.tif"
.
.END PATCH 9.38.5 REPLACED LOGIC
.END PATCH 9.38 ADDED LOGIC

.Find out system information
;;PATCH9.32

	CALL GETWINVER
;	getinfo	system,str6
;	unpack	str6 into str1,str1
;	move	C0,osflag
;	if (str1 = "3" or str1 = "4")		.95/98
;		move	C1,osflag
;	elseif (str1 = "1"or str1 = "5")	.NT4/NT5
;		move	C2,osflag
;	endif
;;subPATCH9.32
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
.START PATCH 9.38.4 ADDED LOGIC
		if (FUNC = "2")
			move	C1,PDFFlag
.START PATCH 9.38.5 ADDED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 ADDED LOGIC
.Keep Following commented logic.  Used for testing purposes
.			call	"GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.				"Parameters":
.				"ProcessPDF":
.				str45
.Following would be used if I were to use strings instead of literal strings in Write_To_Ini call
.			move	"ProcessPDF",str25
.			move	"\\nts0\c\apps\plb\code\pdftest.bat",str35
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
.START PATCH 9.38.5 ADDED LOGIC
			endif
.END PATCH 9.38.5 ADDED LOGIC
		else
			move	C0,PDFFlag
		endif
.END PATCH 9.38.4 ADDED LOGIC
	endif
	move	C0,howmany
	move	"Names In The News CA",COMPNME
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
.START PATCH 9.38.5 ADDED LOGIC
		if (!externalmode)
.END PATCH 9.38.5 ADDED LOGIC
 			pack	taskname,NTWKPATH7,"HOTORDERS"
			Open 	SAVEFILE,Taskname
.			open	SAVEFILE,"HOTORDERS"
			clear	hotkey
			move	NORDFLD,hotkey
			filepi	4;savefile
			read	savefile,HOTKEY;;
			if over
				write	SAVEFILE;"******",today
				write	SAVEFILE,NORDFLD;NORDFLD,tipe,str3
			endif
.START PATCH 9.38.5 ADDED LOGIC
		endif
.END PATCH 9.38.5 ADDED LOGIC
	endif
	reset	prtname
...........................................................................
.Note:  When testing, faxfile needs to be set differently for Full Run and HotOrd Run!!!!!!!
.       Full Run requires faxfile to print to:  \\nts0\d\data\fax\faxfile.prn - CORRECTED 12/9/2004 ASH NOW USES C:\WORK
.       HotOrd Run requires faxfile to print to:  c:\work\faxfile.prn
...........................................................................
.START PATCH 9.35 ADDED LOGIC
	if (hotflag = 1)	.Full Run
.NPRINT.TMP used for Mailer run
		trap	IO GIVING ERROR IF IO
		prepare	output2,"\\nins1\e\data\orders.fax"
		WRITE	output2,SEQ;B1,today," -Company's Receiving Order FAXES !!!",b1,time
.end PATCH 9.35 ADDED LOGIC
		open	INPUT,"\\nins1\e\data\NPRINT.TMP"
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
					if (formflag = 1)
						call	BRKBREAK
					elseif (formflag = 2)
						call	OWNBREAK
						call	prepsplB
					elseif (formflag = 3)
.No need to call fulbreak, as PRTNAME is created before actual file.
						call	FULEOJ
					elseif (formflag = 4)
					elseif (formflag = 5)
						call	ovrbreak
					endif
					move	YES,FirstFlag
					close	input
					trap	IO GIVING ERROR IF IO
					if (formflag = 1)
.Prep for Owner run, which includes creation of file for Fulfillment run
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.						open	INPUT,"\\nins1\e\data\NPRINT.OWN"
.Clean up past instances
.						erase	"\\nins1\e\data\NPRINT.FLL"
.						prepare	INPUT2,"\\nins1\e\data\NPRINT.FLL"
.						loop
.							read	INPUT,SEQ;ORDVARS
.							until over
.							move	OLON,NOWNFLD
.							rep	zfill,NOWNFLD
.							move	"FullRun-NOWNKEY",Location
.							pack	KeyLocation,"Key: ",NOWNFLD
.							call	NOWNKEY
..							if not over
.								call	Trim using OWNCTN
;
.START PATCH 9.43 REPLACED LOGIC
.								if (OWNCTN <> "")
.									pack	NFULFLD,OWNCTN
.									rep	zfill,NFULFLD
.									move	C1,NFULPATH
.									move	"FullRun-NFULKEY",Location
.									pack	KeyLocation,"Key: ",NFULFLD
.									call	NFULKEY
.									if not over
.										write	INPUT2,SEQ;ORDVARS,NFULNUM
;
.								if (OWNCTN <> "")
.									pack	COMPFLD6,OWNCTN
.									rep	zfill,COMPFLD6
.									move	C1,COMPPATH
.									move	"FullRun-COMPKEY6",Location
.									pack	KeyLocation,"Key: ",COMPFLD6
.									call	COMPKEY6
.									if not over
.										if (COMPSVBFLG = "T")
.											write	INPUT2,SEQ;ORDVARS,COMPNUM
.										endif
.END PATCH 9.43 REMOVED LOGIC
.									endif
.								endif
.							endif
.						repeat
.						close	INPUT
.						close	INPUT2
						open	INPUT,"\\nins1\e\data\NPRINT.OWN"
					elseif (formflag = 2)
.Prep for Fulfillment run	
FULLER
						If	(olrn = "684272")
						call	debug
						endif
.						pack	APIFileName,"\\nins1\e\data\NPRINT.FLL",hexzero
.						call	FindFirstFile
.						if (APIResult <> 0 & APIResult <> hexeight)
.Start PATCH 9.44
.							pack	taskname,"\\nins1\e\data\NPRINT.FLL,\\nins1\e\data\NPRINT.FUL;409-412"
;							pack	taskname,"\\nins1\e\data\NPRINT.FLL,\\nins1\e\data\NPRINT.FUL;409-414"
						open	INPUT,"\\nins1\e\data\NPRINT.OWN"
							pack	taskname,"\\nins1\e\data\NPRINT.own,\\nins1\e\data\NPRINT.FUL;329-334"
.End   PATCH 9.44
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
							sort	taskname
.							erase	"\\nins1\e\data\NPRINT.FLL"
							open	INPUT,"\\nins1\e\data\NPRINT.FUL"
.						else
.						erase	"\\nins1\e\data\NPRINT.FuL"
.						prepare	INPUT,"\\nins1\e\data\NPRINT.FuL"
							goto READO
.						endif
					elseif (formflag = 3)
.Prep for Office run, which includes creation of file for Overlay run
						open	INPUT,"\\nins1\e\data\NPRINT.LR"
.Writing to above file happens in Office Print loop, so I will close it later
					elseif (formflag = 4)
.Prep for Overlay run
.START PATCH 9.351 ADDED LOGIC
						pack	APIFileName,"\\nins1\e\data\NPRINT.tmp",hexzero
						call	FindFirstFile
						if (APIResult <> 0 & APIResult <> hexeight)
							pack	taskname,"\\nins1\e\data\NPRINT.tmp,\\nins1\e\data\NPRINT.OVR;211-211"
.end PATCH 9.351 ADDED LOGIC
							sort	taskname
							open	INPUT,"\\nins1\e\data\NPRINT.OVR"
						else
.							goto READO
						erase	"\\nins1\e\data\NPRINT.ovr"
						prepare	INPUT,"\\nins1\e\data\NPRINT.ovr"
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
.START PATCH 9.33 ADDED LOGIC
.								call	Trim using OCOMSLCT
.								if (OCOMSLCT <> "")
.									write	INPUT2,SEQ;ORDVARS
.end PATCH 9.2 ADDED LOGIC
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
.START PATCH 9.2 ADDED LOGIC
		pack	str45,"C:\WORK\FAXFILE.PRN"
		erase	str45
.END PATCH 9.2 ADDED LOGIC
.
		call	CreateRecord
		call	PrtCloseFile
.		pack	APIFileName,"\\nts0\d\data\fax\faxfile.prn",hexzero
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
.START PATCH 9.38.5 ADDED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 ADDED LOGIC
.Give the email a chance of rendering itself before updating the INI file.
				pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
				loop
					call	FindFirstFile
					until (APIResult = 0 | APIResult = hexeight)
					pause	"1"
				repeat
				pause	"2"
.START PATCH 9.38.5 ADDED LOGIC
			endif
.END PATCH 9.38.5 ADDED LOGIC
		else
.END PATCH 9.38.4 ADDED LOGIC
			pack	APIFileName,"C:\WORK\faxfile.prn",hexzero
			call	FindFirstFile
			if (APIResult <> 0 & APIResult <> hexeight)
.				rename   "\\nts0\d\data\fax\faxfile.prn",prtname
				pack	str45,"C:\WORK\",prtname
				erase	str45
				rename   "C:\WORK\faxfile.prn",str45
			endif
.START PATCH 9.38.4 ADDED LOGIC
		endif
.END PATCH 9.38.4 ADDED LOGIC
	endif
.PROGRAM WILL HAVE TO END HERE
.START PATCH 9.35 ADDED LOGIC
	if (hotflag = 1)	.Full Run
.begin patch 9.62
		write  	output2,seq;b1,"Total Company's Faxed Out ",faxkount
		Weof	Output2,seq
		close  	output2
		open 	dfile,"\\nins1\e\data\orders.FAX"
		Clear	MailBody
		Append	Today,MailBody
		Append	" -Company's Receiving Order FAXES !!! ",MailBOdy
		append	Time,mailbody
		append	CRLF,MailBOdy		
		loop
			read dfile,seq;str55
			until over
.			add c1 to n3
			append	STR55,mailbody
			append	CRLF,MailBOdy		
.			move    str55,SmtpTextMessage(n3)   Array <Text message >
		repeat
		Reset	Mailbody
.		move    "Please verify that total matches Fax queue",SmtpSubject Subject
		move    "Please verify that total matches Fax queue",Mailsubjct
;.   Set the text message that is send with the attachments
.		move    str55,SmtpTextMessage(n3)   Array <Text message >
.		move    n3,SmtpTextIndexLast                               Index to last entry in TextMessage array
.		move    "NTS4",SmtpEmailServer                   Address of email serverc
.;smtpemailaddress is who it's FROM!!!
		pack	Mailfrom,"JamieMittone@nincal.com"
		pack	Mailto,"JamieMittone@nincal.com,JamieMittone@nincal.com"
.		clear   smtpemailaddress
.		append  "ComputerRequest",SmtpEmailAddress
.		append  "@nincal.com",SmtpEmailAddress
.		reset   smtpemailaddress
.		move    "Computer Request",SmtpUserName                                User name
.		move	"Computer Request",SmtpUserFullName
.;   Set the destinations of the email. Max 100 (Mime spec)
.;destination is of course where it's going.  1 - Display Name 2- address , 3 - result code(not used)
..START PATCH 9.42 Replaced logic
..START PATCH 9.481 Replaced logic
..START PATCH 9.51 Replaced logic
.		move	"JamieMittone@nincal.com",SmtpDestinations(1,1)
.		move    "Jamie Mitonne",SmtpDestinations(1,2)
..End PATCH 9.51 Replaced logic
..START PATCH 9.481 Replaced logic
.;		move	"ElizabethAyres@nincal.com",SmtpDestinations(1,1)
.;		move    "Elizabeth Ayres",SmtpDestinations(1,2)
..End PATCH 9.481
.;		move	"BaharTabatabai@nincal.com",SmtpDestinations(1,1)
.;		move    "Bahar Tabatabai",SmtpDestinations(1,2)
.;		move	"FrontDesk@nincal.com",SmtpDestinations(1,1)
.;		move    "Front Desk",SmtpDestinations(1,2)
..End PATCH 9.42 Replaced logic
.		move    "creques@nincal.com",SmtpDestinations(2,1)
.		move    "Computer Request",SmtpDestinations(2,2)
.		move    "1",SmtpDestIndexLast                          originators UserName
.		move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.		clear   SmtpLogFile                                         'Clear' disables the LogFile
.		move    "1",SmtpProgress                                    Enable progress bars
.		call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.		if not equal
.			pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.			"Status Code ",SmtpStatus," - ",SmtpStatusText
.			move    "Delivery Failed",SmtpSubject Subject
.			move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.			call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.		endif
		call	SendMail
.end patch 9.62
	endif
.end PATCH 9.35 ADDED LOGIC
.START PATCH 9.38.5 ADDED LOGIC
	if (externalmode)
		return
	endif
.END PATCH 9.38.5 ADDED LOGIC
	shutdown

CreateRecord
.GetInv
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
.	call	Trim using OWNCTN
.START PATCH  9.43 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"C.Record-NFULKEY",Location
.		pack	KeyLocation,"Key: ",NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULVARS
.	endif
;
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"C.Record-COMPKEY6",Location
.		pack	KeyLocation,"Key: ",COMPFLD6
.		call	COMPKEY6

.	call	Trim using OWNCTN
//Patch 9.47
	call Trim using OFULLFIL
//Patch 9.47
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
			clear   cnctfname
		else
			if (COMPSVBFLG <> "T")
				clear	COMPFLD
				clear	COMPVARS
				clear	NFULNUM
				clear	NFULCOMP
				clear	NFULCNT
				clear	NFULFAX
				clear   cnctfname
			else
//Patch 9.48
						Packkey CNCTFLD2 to "01X",COMPNUM
						Call	CNCTAIM
						loop
						until over
						until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
							call	CNCTKG
						repeat
//Patch 9.48
				move	COMPNUM,NFULNUM
				move	COMPCOMP,NFULCOMP
				move	CNCTFNAME,NFULCNT
				move	COMPFAX,NFULFAX
			endif
		endif
.	else	// OWNCTN = ""
	else	// OFULLFIL = ""
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
		clear	COMPFLD
		clear	COMPVARS
		clear	NFULNUM
		clear	NFULCOMP
		clear	NFULCNT
		clear	NFULFAX
		clear   cnctfname
	endif
.END PATCH  9.43 REPLACED LOGIC
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
.begin patch 9.61 OK somewhere here we add the suppression of mailer copies
.if not hotprint And Mailer preference for hardopy is not True & its not a list management order = return
.not live yet
	clear	lstmgt
	pack	lstmgt,OSALES10,OSALES
.	if	(Hotflag = c1 & COMPConFlag <> "T" & LStmgt <> "06"  & LStmgt <> "19"  & LStmgt <> "27"  & LStmgt <> "28"  )
.	return
.	endif
.end patch 9.61 OK somewhere here we add the suppression of mailer copies
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
.START PATCH 9.3 REMOVED LOGIC
..START PATCH 9.1 ADDED LOGIC
..Catch any Mailer that is not flagged to be Faxed
.			else
.				pack	MKEY,OMLRNUM,Z3
.				rep	zfill,MKEY
.				move	"mlronly-NMLRKEY",Location
.				pack	KeyLocation,"Key: ",MKEY
.				call	NMLRKEY             .make sure we have last brker info
.				if (MFaxOFlag <> "T")
.					move	YES,FirstFlag
..Called after vars have been set in SetPrintFlag for previous record
.					call	BRKBREAK
..Establishes flags/settings for current record
.					call	SetPrintFlag
.					move	OBRKNUM,HOLDBRK
.				endif
..END PATCH 9.1 ADDED LOGIC
.END PATCH 9.3 REMOVED LOGIC
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
.START PATCH 9.3 REPLACED LOGIC
..START PATCH 9.1 REPLACED LOGIC
..	move	C0,FaxNumFlag
..	move	C0,N10
..	move	BRFAX,N10
..	if (N10 = C0)
..		pack	MKEY,OMLRNUM,Z3
..		rep	zfill,MKEY
..		move	"SetPrintFlag-NMLRKEY",Location
..		pack	KeyLocation,"Key: ",MKEY
..		call	NMLRKEY             .make sure we have last brker info
..
..		move	C0,N10
..		move	MFAX,N10
..		if (N10 = C0)
..			move	C1,PrintFlag
..			move	C1,faxflag
..		else
..			pack	fhandle,OBRKNUM,MNUM
..			pack	str2," x"
..			rep	str2,fhandle
..			move	MCOMP,faxname
..			move	MFAX,FAXTELE
..			move	MCONTCT,faxattn
...
..			move	C1,FaxNumFlag
..			move	C2,faxflag	.do fax.
..		endif
..	else
..		move	BRKNUM,fhandle
..		move	BRCOMP,faxname
..		move	BRFAX,FAXTELE
..		move	BRCNTCT,faxattn
...
..		move	C2,faxflag	.do fax.
..	endif
.....................
.	pack	MKEY,OMLRNUM,Z3
.	rep	zfill,MKEY
.	move	"SetPrintFlag-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY             .make sure we have last MLR info
..
.	move	C0,FaxNumFlag
.	move	C0,N10
..
.	if (MFaxOFlag <> "T" OR BrFaxOFlag <> "T")
.		move	C1,PrintFlag
.		move	C1,faxflag
.	else
.		move	BRFAX,N10
.		if (N10 = C0)
.			move	C0,N10
.			move	MFAX,N10
.			if (N10 = C0)
.				move	C1,PrintFlag
.				move	C1,faxflag
.			else
.				pack	fhandle,OBRKNUM,MNUM
.				pack	str2," x"
.				rep	str2,fhandle
.				move	MCOMP,faxname
.				move	MFAX,FAXTELE
.				move	MCONTCT,faxattn
..
.				move	C1,FaxNumFlag
.				move	C2,faxflag	.do fax.
.			endif
.		else
.			move	BRKNUM,fhandle
.			move	BRCOMP,faxname
.			move	BRFAX,FAXTELE
.			move	BRCNTCT,faxattn
..
.			move	C2,faxflag	.do fax.
.		endif
.	endif
..END PATCH 9.1 REPLACED LOGIC
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
.END PATCH 9.1 REPLACED LOGIC
.END PATCH 9.3 REPLACED LOGIC
	return

BRKBREAK
	display	*p1:24,*el,"broker break"
	if (faxflag = 2)
		display	*p1:24,*el,"FAX IT "
		call	prepspl2
		call	prepfax2
	else
		display	*p1:24,*el,"No Fax Number"
	endif
	move	C0,mlrcnt		.reset count for cover sheet
.Initialize HOLDMLR each time a new break occurs
	clear	HOLDMLR
	return

*******************************************************************************
prepfax2
.....TESTING.....
.	move	"5106288313",faxtele
.....TESTING.....
	count	N2,faxtele
	compare	C10,N2
	if equal
		move	C1,LONGDIST
		unpack	faxtele,STR3,STR7
		match	"510",STR3           .LOCAL ?
		if equal
			move	str7,faxtele
			clear	LONGDIST
		else
			match	B3,str3           .LOCAL ?
			if equal
				move	str7,faxtele
				clear	LONGDIST
			endif
		endif
	endif
FILENAM
	clear	spoolfl2
	if (FaxNumFlag = 1)
		append	"\\nts0\d\data\fax\nrdm",spoolfl2
	else
		append	"\\nts0\d\data\fax\nrdb",spoolfl2
	endif
	append	fhandle,spoolfl2
	append	".cvr",spoolfl2
	reset	spoolfl2
	splopen	spoolfl2
	move	"                                        ",APIFileName
	clear	APIFileName
.START PATCH 9.38.3 REPLACED LOGIC
.	pack	APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
	pack	APIFileName,"C:\WORK\hdrfile.prn",hexzero
.END PATCH 9.38.3 REPLACED LOGIC
	call	DeleteFile
	if (APIResult = 0 | APIResult = hexeight)
	endif
.blankc2
	call	Trim using faxname
	move	C1,N2
	unpack	date,MM,DD,YY
	clock	time,time
	clear	str5
	append	time,str5
	reset	str5
	cmatch	YES,epsiflag
	call	contacts if equal
.START PATCH 9.35 ADDED LOGIC
	write output2,seq;faxname
	add   c1 to faxkount
.end PATCH 9.35 ADDED LOGIC
.START PATCH 9.38 REPLACED LOGIC
.	print	"^[D",longdist,faxtele,"^[N",faxname:
.		"^[SOrders"," ^]":
.		*n,032,hpreset:
.		hpttray:
.		hpport:
.		033,"&l66P":               page length
.		033,"&l65F"
.	call	PortraitLTRHEAD
.	print	*n,*n,*n,*n,*n:
.		*n,*n,*n,hpt285,hpdtch10,hpbon,"LIST ORDER CONFIRMATION":
.		*L,hpboff:
.		*N,hpt350,"VIA FACSIMILE":
.		*N,*N,*n,*n,*n,hpt100,hpdtch10,"DATE: ",hpt200,today;
.	goto wrtnxt2
.	print	*N,*N,hpt050,"TO: ",hpt150,faxname:
.		*N,hpt050,"Attn: ",hpt150,faxattn:
.		*N,*N,hpt050,"From:",hpt150,"List Management"
.	goto finform2
.wrtnxt2
.	unpack	faxtele,arcd,str3,str4
.	clear	nfaxtel
.	pack	nfaxtel,"(",arcd,") ",str3,dash,str4
.	pack	nfaxtel2,"(","415",") 989-3350"
.	print	*N,*N,hpt100,"TO: ",hpt200,faxname:
.		*N,hpt200,"Brokerage Division":
.		*N,hpt200,"Fax:",nfaxtel
.	print	*N,*N,hpt100,"FROM:",hpt200,"Names in the News":
.		*N,hpt200,"List Management Dept.":
.		*N,hpt200,"Tel:",nfaxtel2:
.		*N,*N
.finform2
.	print	*N,hpt050
.	compare	C2,formflag
.	goto wrtcnt2 if equal
.wrtcnt2
.	print	*N,hpt100,mlrcnt," order confirmation(s) attached."
.wrtlst2
.	print	*N,hpt100,"PLEASE NOTE: ":
.		*N,hpt100,"This is a ",HPUNON,"confirmation",hpunoff," of your brokerage order(s) for our Names in the News managed ":
.		*n,hpt100,"list(s). Please review this information and contact our list managers if you note any":
.		*n,hpt100,"discrepancies or errors. We can be reached at (415) 989-3350. Our fax number is (415)":
.		*n,hpt100,"433-7796.":
.		*n,*n,hpt100,"Thank you for ordering Names in the News-managed lists. We appreciate your business."
.wrteof2
.	splclose
.......................................
.This is a total cheat.  I am creating a shortened cover page that only includes the fax information.
.Backing up existing version of faxfile.prn.  Creating a new Cover sheet using faxfile.prn.
.Using Binary option to combine spool file and new Cover sheet.  Giving this new combined file the
.same name as original spool file.  Renaming original faxfile.prn back to faxfile.prn.
	print	"^[D",longdist,faxtele,"^[N",faxname:
		"^[SOrders"," ^]"
	splclose
	prtclose Laser
.START PATCH 9.38.3 REPLACED LOGIC
.	erase	"\\nts0\d\data\fax\faxfile.sav"
.	rename	"\\nts0\d\data\fax\faxfile.prn","\\nts0\d\data\fax\faxfile.sav"
	erase	"C:\WORK\faxfile.sav"
	rename	"C:\WORK\faxfile.prn","C:\WORK\faxfile.sav"
.END PATCH 9.38.3 REPLACED LOGIC
	PRTOPEN Laser2,"faxfile",""
	prtpage	Laser2;*UNITS=*HIENGLISH;


.begin patch 9.49
.	prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.	IF	(OcompID = "P") 
.if this is a confirm from LM it should use id2  DLH 18Sep07
	IF	(OcompID2 = "P")
	prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
		*p=6014:443,"Oakland, CA 94612-1429":
		*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=5980:643,"A Division of Names in the News"
	Else
	prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
	endif
.end patch 9.49





	move	"1500",row
	prtpage	Laser2;*pcolumn2:row,*font=font7,"LIST ORDER CONFIRMATION"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn2:row,*font=font8,"          VIA FACSIMILE"
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
.
	unpack	faxtele,arcd,str3,str4
	clear	nfaxtel
	pack	nfaxtel,"(",arcd,") ",str3,dash,str4
	pack	nfaxtel2,"(","415",") 989-3350"
.
	prtpage	Laser2;*pcolumn:row,"TO:"
	prtpage	Laser2;*pcolumn1:row,faxname
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn1:row,"Brokerage Division"
	add	tenlpi,row
	add	twelvelpi,row
	prtpage	Laser2;*pcolumn1:row,"Fax:  ",nfaxtel
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
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
	prtclose Laser2
.Clean up
	clear   taskname
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe /c ",taskname
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe /c ",taskname
	else			.95/98
		append	"!c:\command.com /c ",taskname
	endif
	append  "copy ",taskname
	append  spoolfl2,taskname
.START PATCH 9.38.3 REPLACED LOGIC
.	append  " /b + \\nts0\d\data\fax\faxfile.prn /b \\nts0\d\data\fax\faxfile2.prn",taskname
.	reset   taskname
.	execute taskname
.	copyfile "\\nts0\d\data\fax\faxfile2.prn",spoolfl2
.	copyfile "\\nts0\d\data\fax\faxfile.sav","\\nts0\d\data\fax\faxfile.prn"
.	erase	"\\nts0\d\data\fax\faxfile2.prn"
.	erase	"\\nts0\d\data\fax\faxfile.sav"
..................................................
	append  " /b + C:\WORK\faxfile.prn /b C:\WORK\faxfile2.prn",taskname
	reset   taskname
	execute taskname
	copyfile "C:\WORK\faxfile2.prn",spoolfl2
	copyfile "C:\WORK\faxfile.sav","C:\WORK\faxfile.prn"
	erase	"C:\WORK\faxfile2.prn"
	erase	"C:\WORK\faxfile.sav"
.END PATCH 9.38.3 REPLACED LOGIC
.END PATCH 9.38 REPLACED LOGIC
	return
newcover
	prtpage	Laser;*NEWPAGE
newcover2
	call	contacts
        move    "300",row
.START PATCH 9.38 REPLACED LOGIC
.        prtpage Laser;*p2700:row,*font=font10,"Names";
.        prtpage Laser;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage Laser;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage Laser;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage Laser;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
...............
.begin patch 9.49
.	prtpage	Laser;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1300:2500:10100:NINLogo
.begin patch 22Aug07 DLH of LM we need to check ocompid2
.	IF	(OcompID = "P")            
	IF	(OcompID2 = "P")
.end patch 22Aug07 DLH of LM we need to check ocompid2
	prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
		*p=6014:443,"Oakland, CA 94612-1429":
		*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=5980:643,"A Division of Names in the News"

.	prtpage	Laser2;*units=*HIENGLISH,*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
.		*p=6120:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
.		*p=6014:443,"Corte Madera, CA 94925-1182":
.		*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
.		*p=5980:643,"A Division of Names in the News"
	Else
	prtpage	Laser2;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1300:2500:10100:NINLogo
	endif
.end patch 9.49



        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
.END PATCH 9.38 REPLACED LOGIC
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
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	display	*p1:24,*el,"checking owner ",ownocpy,b1,ownctn
	display	*p1:24,*el,"checking owner ",ownocpy
.When we decide to Fax out everything, we can remove all of the following up to ownbrk1, and also remove ownbrk2 label
        call  trim using ofullfil
	if (OFULLFIL <> "")
.	call	Trim using OWNCTN
.	if (OWNCTN <> "")
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 9.2 REPLACED LOGIC
.		if (ORTNNUM = "0040")
.			move	C0,N10
.			goto	ownbrk2
.		endif
.		call	Trim using NFULCOMP
.		if (NFULCOMP = "TDMC" OR NFULCOMP = "Tdmc" OR NFULCOMP = "tdmc" OR NFULFLD = "0026")
.			move	C0,N10
.			goto	ownbrk2
.		endif
.
		move	C0,N10
		goto	ownbrk2
.END PATCH 9.2 REPLACED LOGIC
	else
.START PATCH 9.43 REPLACED LOGIC
.		clear	NFULFLD
.		clear	NFULVARS
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
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
.START PATCH 9.38.5 REPLACED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 REPLACED LOGIC
				erase	"c:\progra~1\pdf995\flag.dat"
.START PATCH 9.38.5 REPLACED LOGIC
			endif
.END PATCH 9.38.5 REPLACED LOGIC
		endif
.END PATCH 9.38.4 ADDED LOGIC
		return
	endif
	call	Trim using NFULCOMP
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.	if (OWNCTN = "0026" | NFULCOMP = "TDMC" | NFULCOMP = "Tdmc")
	if (OFULLFIL = "009406")
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
		if (TRIPLEX <> "1")
			display	*p1:24,*el,"IT's Triplex, Skip"
.START PATCH 9.38.4 ADDED LOGIC
			if (PDFFlag = 1)
.START PATCH 9.38.5 REPLACED LOGIC
				if (!externalmode)
.END PATCH 9.38.5 REPLACED LOGIC
					erase	"c:\progra~1\pdf995\flag.dat"
.START PATCH 9.38.5 REPLACED LOGIC
				endif
.END PATCH 9.38.5 REPLACED LOGIC
			endif
.END PATCH 9.38.4 ADDED LOGIC
			return
		endif
	endif
	match	"0001",ortnnum       .REUSE
	if equal
		display	*p1:24,*el,"IT's RE-USE, Skip"
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
.START PATCH 9.38.5 REPLACED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 REPLACED LOGIC
				erase	"c:\progra~1\pdf995\flag.dat"
.START PATCH 9.38.5 REPLACED LOGIC
			endif
.END PATCH 9.38.5 REPLACED LOGIC
		endif
.END PATCH 9.38.4 ADDED LOGIC
		return
	endif
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	reset	ownctn,15
.	reset	ownctn
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	reset	RUNCODES
	scan	OLNUM IN RUNCODES
	if equal
		display	*p1:24,*el,"It's Running charges, Skip"
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
.START PATCH 9.38.5 REPLACED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 REPLACED LOGIC
				erase	"c:\progra~1\pdf995\flag.dat"
.START PATCH 9.38.5 REPLACED LOGIC
			endif
.END PATCH 9.38.5 REPLACED LOGIC
		endif
.END PATCH 9.38.4 ADDED LOGIC
		return
	endif
.Start Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	call	Trim using OWNCTN
.	if (OWNCTN = "")
	call	Trim using OFULLFIL
	if (OFULLFIL = "")
.End Patch 9.46 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
.START PATCH 9.38.5 REPLACED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 REPLACED LOGIC
				erase	"c:\progra~1\pdf995\flag.dat"
.START PATCH 9.38.5 REPLACED LOGIC
			endif
.END PATCH 9.38.5 REPLACED LOGIC
		endif
.END PATCH 9.38.4 ADDED LOGIC
		return
	endif
	if (hotflag = 1)
		goto regful
	elseif (hotflag =2)
		goto prtfulby
	endif
regful
.START PATCH 9.43 REPLACED LOGIC
.	move	C0,N4
.	move	NFULNUM,N4
.	if (N4 > C0)
.		add	C1,CountArray(N4)
.	endif
.whoanelly
.	if (NFULNUM <> HOLDCCTO)
	move	C0,N6
	move	NFULNUM,N6
	if (N6 > C0)
.begin patch 9.6
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
			
.		add	C1,CountArray(N6)
.end patch 9.6
	endif
whoanelly
	if (NFULNUM <> HOLDCCTO)
.END PATCH 9.43 REPLACED LOGIC
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
		pack	fhandle,"f",holdccto
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
.START PATCH 9.43 REPLACED LOGIC
.	move	C0,N4
.	call	Trim using NFULNUM
.	move	NFULNUM,N4
.	if (N4 > C0)
..If var somehow was not zero-filled correctly, do it now
.		move	N4,NFULNUM
	move	C0,N6
	call	Trim using NFULNUM
	move	NFULNUM,N6
	if (N6 > C0)
.If var somehow was not zero-filled correctly, do it now
		move	N6,NFULNUM
.END PATCH 9.43 REPLACED LOGIC
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

.	RETURN

.START PATCH 9.31 ADDED LOGIC
.Force Offer field to be blank
	reset	RUNCODES
	scan	OLNUM IN RUNCODES
	if equal
		clear	OODNUM
		clear	OFDESC
	endif
.END PATCH 9.31 ADDED LOGIC
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
	move	B1,ERROR
	clear	spoolfl2
	append	"\\nts0\d\data\fax\nord",spoolfl2
	call	Trim using fhandle
	if (fhandle = "")
		return
	endif
	append	fhandle,spoolfl2
	append	".cvr",spoolfl2
	pack	prtname,"nord",fhandle,".LST"
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
.START PATCH 9.35 ADDED LOGIC
	write output2,seq;faxname
	add   c1 to faxkount
.end PATCH 9.35 ADDED LOGIC
.START PATCH 9.38 REPLACED LOGIC
.	print	"^[D",longdist,faxtele,"^[N",faxname:
.		"^[SOrders","^]":
.		*n,032,hpreset:
.		hpttray:
.		hpport:
.		033,"&l66P":               page length
.		033,"&l65F"
.	call	PortraitLTRHEAD
.	print	*n,*n,*n,*n,*n:
.		*n,*n,*n,hpt350,hpdtch10,hpbon,"LIST ORDERS":
.		*L,hpboff:
.		*N,hpt350,"VIA FACSIMILE":
.		*N,*N,*n,*n,*n,hpt050,hpdtch14,"Date: ",hpt150,today;
.	cmatch	B1,faxattn
.	goto wrtnxt if eos
.	goto wrtnxt if equal
.	print	*N,*N,hpt050,"To: ",hpt150,faxname:
.		*N,*N,hpt050,"Attn: ",hpt150,faxattn:
.		*N,*N,hpt050,"From:",hpt150,"Brokerage"
.	goto finform
.wrtnxt
.	print	*N,*N,hpt050,"To: ",hpt150,faxname:
.		*N,*N,hpt050,"From:",hpt150,"Brokerage":
.		*N,*N
.finform
.	print	*N,hpt050
.	compare	C2,formflag
.	goto wrtcnt if equal
.wrtcnt
.	print	*N,hpt050,owncnt," Orders Enclosed"
.	compare	C0,ownscnt
.	if equal
.		print	*N,hpt050
.	else
.		print	*N,hpt050,ownscnt," Samples Enclosed "
.		compare	C1,owncnt
.		if not equal
.			print	*N,hpt050,"Note: Sample(s) May need to be attached to ":
.				"multiple Orders."
.		endif
.	endif
.wrtlst
.	print	*N,hpt050,"Please call if you do not receive all pages. "
.wrteof
.	splclose
.	if (countr = C1)
.		clear	outname
.		append	"G:\DATA\nord",OUTNAME
.;		append	"\\nts0\d\DATA\nord",OUTNAME
.		append	OLON,OUTNAME
.		append	".smp",OUTNAME
.		reset	OUTNAME
.		prepare	OUTPUT,OUTNAME
.		move	C1,smpflag                           .we have sample file
.	endif
.......................................
.This is a total cheat.  I am creating a shortened cover page that only includes the fax information.
.Backing up existing version of faxfile.prn.  Creating a new Cover sheet using faxfile.prn.
.Using Binary option to combine spool file and new Cover sheet.  Giving this new combined file the
.same name as original spool file.  Renaming original faxfile.prn back to faxfile.prn.
	print	"^[D",longdist,faxtele,"^[N",faxname:
		"^[SOrders"," ^]"
	splclose
	prtclose Laser
.START PATCH 9.38.3 REPLACED LOGIC
.	erase	"\\nts0\d\data\fax\faxfile.sav"
.	rename	"\\nts0\d\data\fax\faxfile.prn","\\nts0\d\data\fax\faxfile.sav"
	erase	"C:\WORK\faxfile.sav"
	rename	"C:\WORK\faxfile.prn","C:\WORK\faxfile.sav"
.END PATCH 9.38.3 REPLACED LOGIC
	PRTOPEN Laser2,"faxfile",""
	prtpage	Laser2;*UNITS=*HIENGLISH;
.begin patch 9.49
.	prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.begin patch 9.6
	IF	(formflag = 3)          .Fulfillment
		prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
			*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
			*p=6014:443,"Oakland, CA 94612-1429":
			*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
			*p=5980:643,"A Division of Names in the News"
	
.		prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:0:7600:NINLogo
		prtpage	Laser2;*Pictrect=*off,*PICT=0:975:0:5700:NINLogo

.end patch 9.6
	Else
		IF	(OcompID = "P")
		prtpage	Laser2;*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
			*p=6120:343,*font=fontO7,"1300 Clay St. 11th Floor":
			*p=6014:443,"Oakland, CA 94612-1429":
			*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
			*p=5980:643,"A Division of Names in the News"
	
.	prtpage	Laser2;*units=*HIENGLISH,*p=5663:25,*font=fontO18b,"Pacific Lists, Inc.":
.		*p=6120:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
.		*p=6014:443,"Corte Madera, CA 94925-1182":
.		*p=5980:543,"415-945-9450 ","·"," Fax 415-945-9451":
.		*p=5980:643,"A Division of Names in the News"
		Else
		prtpage	Laser2;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
		endif
.begin patch 9.6
	endif
.end patch 9.6
.end patch 9.49

	move	"1500",row
	prtpage	Laser2;*pcolumn2:row,*font=font7,"  LIST ORDERS"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser2;*pcolumn2:row,*font=font8,"VIA FACSIMILE"
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
.test DLH 13 Feb 2008
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
	prtclose Laser2
.Clean up
	clear   taskname
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe /c ",taskname
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe /c ",taskname
	else			.95/98
		append	"!c:\command.com /c ",taskname
	endif
	append  "copy ",taskname
	append  spoolfl2,taskname
.START PATCH 9.38.3 REPLACED LOGIC
.	append  " /b + \\nts0\d\data\fax\faxfile.prn /b \\nts0\d\data\fax\faxfile2.prn",taskname
.	reset   taskname
.	execute taskname
.	copyfile "\\nts0\d\data\fax\faxfile2.prn",spoolfl2
.	copyfile "\\nts0\d\data\fax\faxfile.sav","\\nts0\d\data\fax\faxfile.prn"
.	erase	"\\nts0\d\data\fax\faxfile2.prn"
.	erase	"\\nts0\d\data\fax\faxfile.sav"
........................
	append  " /b + C:\WORK\faxfile.prn /b C:\WORK\faxfile2.prn",taskname
	reset   taskname
	execute taskname
	copyfile "C:\WORK\faxfile2.prn",spoolfl2
	copyfile "C:\WORK\faxfile.sav","C:\WORK\faxfile.prn"
	erase	"C:\WORK\faxfile2.prn"
	erase	"C:\WORK\faxfile.sav"
.END PATCH 9.38.3 REPLACED LOGIC
	if (countr = C1)
		clear	outname
.START PATCH	9.40	REPLACED LOGIC
.		append	"G:\DATA\nord",OUTNAME
		pack taskname, NTWKPATH1,"nord"
		append taskname, OUTNAME
.END PATCH	9.40	REPLACED LOGIC
;		append	"\\nts0\d\DATA\nord",OUTNAME
		append	OLON,OUTNAME
		append	".smp",OUTNAME
		reset	OUTNAME
		prepare	OUTPUT,OUTNAME
		move	C1,smpflag                           .we have sample file
	endif
.END PATCH 9.38 REPLACED LOGIC
	return

.prepare spool file.
prepspl
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
.		compare	C2,faxflag
.		if equal          .yes
LASTRUN
			if (smpflag = c1 | smpflag = c2)
				clear	outname
.START PATCH	9.40	REPLACE LOGIC
.				append	"G:\DATA\nord",OUTNAME
				pack taskname, NTWKPATH1,"nord"
				append taskname, OUTNAME
.END PATCH	9.40	REPLACE LOGIC
.				append	"\\nts0\d\DATA\nord",OUTNAME
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
.Patch9.352
				trap RetryPrint2 if SPOOL
.Patch9.352
				prtopen	file1,"FAXFILE",""
.Patch9.352
				TrapClr	SPOOL
.Patch9.352
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
.START PATCH 9.34 REPLACED LOGIC
.					create	PICT1=70:470:100:500:
.						DCXFile,BORDER,SCROLLBAR,AUTOZOOM
.					PICT1.GetPageCount GIVING N9
.					if (FirstFlag1 = Yes)
.						move	NO,FirstFlag1
.						prtpage	file1;*P2:2,*PICTvis=6:72:3:83:PICT1;
.					else
.						prtpage	file1;*P2:2,*NEWPAGE,*PICTvis=6:72:3:83:PICT1;
.					endif
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
.END PATCH 9.34 REPLACED LOGIC
.Print and Display Additional Pages
					if (N9 > C1)    .Only Enter loop if more than one page
						clear	N8
						move	C1,N8   .Start with SECOND PAGE as first page already printed
						loop
							add	C1,N8
							until (N8 > N9)
.START PATCH 9.34 REPLACED LOGIC
.							create	PICT1=70:470:100:500:
.								DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
.							activate PICT1
.							prtpage	file1;*P2:2,*NEWPAGE,*PICTvis=6:72:3:83:PICT1;
                                                        CREATE  PICT1=70:495:70:620:
                                                                DCXFile,BORDER=0,AUTOZOOM=0,PAGE=N8
                                                        activate PICT1
                                                        prtpage file1;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
.END PATCH 1.49.2 REPLACED LOGIC
.END PATCH 9.34 REPLACED LOGIC
						repeat
					endif
				repeat
printstop
				PRTclose file1
				DESTROY PICT1
test3
				move	C0,LastFlag
.START PATCH 9.38.3 REPLACED LOGIC
.				rename   "\\nts0\d\data\fax\faxfile.prn",spoolf
				rename   "C:\WORK\faxfile.prn",spoolf
.END PATCH 9.38.3 REPLACED LOGIC
			endif
.		endif
	endif
.
NOSMP
	clear	outname
.START PATCH	9.40 	REPLACED LOGIC
.	append	"G:\DATA\nord",OUTNAME
	pack taskname,NTWKPATH1,"nord"
	append taskname,OUTNAME
.END PATCH	9.40 	REPLACED LOGIC
;                    append	"\\nts0\d\DATA\nord",OUTNAME
	append	OLON,OUTNAME
	append	".smp",OUTNAME
	reset	OUTNAME
	prepare	OUTPUT,OUTNAME
	move	C1,smpflag
	return
.prepare spool file. mailer copy
prepspl2
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
.Start patch 9.45
        move    compaddr,str35
.End patch 9.45
	move	C0,NFIELD23
.START PATCH 9.36 - REPLACED LOGIC
.	unpack	OPPM,F3,F2
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
.END PATCH 9.36 - REPLACED LOGIC
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
.START PATCH 9.38.2 REPLACED LOGIC
.	pack	NSMPFLD,OMLRNUM,OSAMCDE
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
.END PATCH 9.38.2 REPLACED LOGIC
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
.begin patch 9.52
.	pack	LROUT,LRMASK
.	move	OLRN,LRNUM
.	edit	LRNUM,LROUT
	if (faxflag <> 2)
		prtpage	Laser;*p=2000:425,*font=fontO12b,REVDATA
	endif
	prtpage	Laser;*p=2000:810,*font=fontO12b,REPRT	???????????????????????
	prtpage	Laser;*p=2000:645,BILDDATA
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=625:887,*font=fontO10,LROUT
.	prtpage	Laser;*p=3125:887,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.	prtpage	Laser;*p=1000:1076,OMLRPON
	prtpage	Laser;*p=625:800,*font=fontO10,Olrn
.	prtpage	Laser;*p=625:800,*font=fontO10,LROUT
.end patch 9.52
	prtpage	Laser;*p=3125:800,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	prtpage	Laser;*p=1000:988,OMLRPON
.END PATCH 9.36 REPLACED LOGIC
	cmatch	B1,BRCOMP
	if not eos
		branch	FORMFLAG OF BRKOK,BRKNTOK,BRKNTOK,BRKOK,brkntok
		branch	copy of brkntok,brkntok,brkok
		goto brkok
brkntok
		clear	careof
		clear	str45
.Start PATCH 9.45
		clear   str35
.End PATCH 9.45
		clear	attn
		clear	brcntct
.PATCH 9.39
.		clear braddr
.Start PATCH 9.44
		clear compaddr
.Start PATCH 9.44
		clear brcity
		clear brstate
		clear brzip
.PATCH 9.39
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
.Start PATCH 9.45
		move    str35,compaddr
.End PATCH 9.45
printmlr
.START PATCH 9.36 REPLACED LOGIC
.		prtpage	Laser;*p=3125:1076,OMLRNUM,SLASH,OCOBN
.		prtpage	Laser;*p=1000:1263,MNAME
.		prtpage	Laser;*p=1000:1438,careof,b1,str45
.		prtpage	Laser;*p=3810:1438,attn,BRCNTCT
		prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
		prtpage	Laser;*p=1000:1176,MNAME
		prtpage	Laser;*p=1000:1351,careof,b1,str45
.PATCH 9.39
		prtpage	Laser;*p=3810:1351,attn,BRCNTCT
.Start PATCH 9.44
.   	prtpage	Laser;*p=1000:1526,braddr
   	prtpage	Laser;*p=1000:1526,compaddr
.End   PATCH 9.44
	   if (brCITY = "")
	   prtpage	Laser;*p=1000:1701,"                            "
		else
		prtpage	Laser;*p=1000:1701,brCITY,comma,brSTATE," ",brZIP
		endif
.PATCH 9.39

.END PATCH 9.36 REPLACED LOGIC
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
;	branch	copy of prntmlr3,prntmlr3,prntmlr2
.PATCH 9.39
	branch	copy of prntmlr3,prntmlr3,prntmlr3
.PATCH 9.39
prntmlr2
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=3125:1076,OMLRNUM,SLASH,OCOBN
.	prtpage	Laser;*p=1000:1263,MNAME
.	prtpage	Laser;*p=1000:1438,MCOMP,b5,str15
	prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
	prtpage	Laser;*p=1000:1176,MNAME
	prtpage	Laser;*p=1000:1351,MCOMP,b5,str15
.PATCH 9.39
	prtpage	Laser;*p=1000:1526,compaddr
	prtpage	Laser;*p=1000:1701,COMPCITY,", ",COMPSTATE," ",COMPZIP
.END PATCH 9.36 REPLACED LOGIC
	prtpage	Laser;*p=1000:1876,OFDESC
	prtpage	Laser;*p=125:2020,"##",OFFEROUT
	prtpage	Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
	goto oprint1b2
.PATCH 9.39
prntmlr3
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=3125:1076,OMLRNUM,SLASH,OCOBN
.	prtpage	Laser;*p=1000:1438,MCOMP,b5,str15
	prtpage	Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
	prtpage	Laser;*p=1000:1351,MCOMP,b5,str15
.END PATCH 9.36 REPLACED LOGIC
OPRINT1B
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=1000:1763,OFDESC
.	prtpage	Laser;*p=125:1951,"##",OFFEROUT
.	prtpage	Laser;*p=1000:1951,SAMPLE,B1,Nsmpdes1
.PATCH 9.39
	prtpage	Laser;*p=1000:1876,OFDESC
	prtpage	Laser;*p=125:2020,"##",OFFEROUT
	prtpage	Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
.PATCH 9.39
.END PATCH 9.36 REPLACED LOGIC
oprint1b2
   branch	FORMFLAG TO prtlist,PRTOWN,PRTOWN,PRTOWN,prtown
	compare	C0,copy
	goto prtlist if equal
prtown
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=1000:2263,OWNLONM
.	prtpage	Laser;*p=125:2438,"##",OLON
.	prtpage	Laser;*p=1000:2438,OWNOCPY,B1,LP,ARCD,RP,EXT,DASH,PHONE,"   Fax: ",FAX1
.	prtpage	Laser;*p=1000:2613,OWNLOSA
.	call	Trim using OWNLOCTY
.	if (OWNLOCTY <> "")
.		pack	taskname,OWNLOCTY,", ",OWNLOS," ",OWNLOZC
.	else
.		pack	taskname,OWNLOS," ",OWNLOZC
.	endif
.	prtpage	Laser;*p=1000:2788,taskname
.	prtpage	Laser;*p=1000:3138,NFULCOMP
;	prtpage	Laser;*p=1000:2093,OWNLONM
;	prtpage	Laser;*p=125:2268,"##",OLON
;	prtpage	Laser;*p=1000:2268,OWNOCPY,B1,LP,ARCD,RP,EXT,DASH,PHONE,"   Fax: ",FAX1
;	prtpage	Laser;*p=1000:2443,OWNLOSA
;	call	Trim using OWNLOCTY
;	if (OWNLOCTY <> "")
;		pack	taskname,OWNLOCTY,", ",OWNLOS," ",OWNLOZC
;	else
;		pack	taskname,OWNLOS," ",OWNLOZC
;	endif
;	prtpage	Laser;*p=1000:2618,taskname
;	prtpage	Laser;*p=1000:2918,NFULCOMP
.
.PATCH 9.39
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
.PATCH 9.39
.END PATCH 9.36 REPLACED LOGIC
	goto prtlist
prtlist
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=1000:3451,O1DES
.	prtpage	Laser;*p=125:3639,"##",OLNUM
.	prtpage	Laser;*p=1000:3639,O2DES
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
.END PATCH 9.36 REPLACED LOGIC
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
.START PATCH 9.36 REPLACED LOGIC
.			prtpage	Laser;*p=1000:3827,*font=FontO7Dot5I,"Per List Owner - Gross Billing No Deductions"
			prtpage	Laser;*p=1000:4903,*font=FontO7Dot5I,"Per List Owner - Gross Billing No Deductions"
.END PATCH 9.36 REPLACED LOGIC
		endif
		cmatch	"F",onetfm           ."F" = flat/volume discount
		if not equal               .this one is a net
.START PATCH 9.36 REPLACED LOGIC
.			prtpage	Laser;*p=1000:3827,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
.			prtpage	Laser;*p=1000:3957,"& will pay $",onetrc,"/m running charge on unused names."
			prtpage	Laser;*p=1000:4903,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
			prtpage	Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
..END PATCH 9.36 REPLACED LOGIC</B>
		else                                 .this is flat/volume
.START PATCH 9.36 REPLACED LOGIC
.			prtpage	Laser;*p=1000:3827,*font=FontO7Dot5I,onetper,"% Volume Discount on Gross Names Shipped"
.			prtpage	Laser;*p=1000:3957,"& will pay $",onetrc,"/m running charge on unused names."
.START PATCH 9.41 REPLACED LOGIC
.			prtpage	Laser;*p=1000:4903,*font=FontO7Dot5I,onetper,"% Volume Discount on Gross Names Shipped"
.			prtpage	Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
			prtpage	Laser;*p=1000:4903,*font=Font08bI,onetper,"% Net Arrangement",*font=Font08I,", Run charge @ $",onetrc,"/m"
			prtpage	Laser;*p=1000:5033,"No Deducts, No CV required."

.END PATCH 9.41 REPLACED LOGIC
.END PATCH 9.36 REPLACED LOGIC
		endif
	endif
pqty
.START PATCH 9.36 REPLACED LOGIC
.	prtpage	Laser;*p=1000:4076,*font=fontO10,QTYOUT
.	match	"         ",OEXQTY
.	goto OPRINT2 IF EOS
.	goto OPRINT2 IF EQUAL
.	prtpage	Laser;*p=3000:4076,"SEE BELOW"," ",FLAGPAID
.	goto OPRINT3
.........................................
	prtpage	Laser;*p=1000:3587,*font=fontO10,QTYOUT
	match	"         ",OEXQTY
	goto OPRINT2 IF EOS
	goto OPRINT2 IF EQUAL
	prtpage	Laser;*p=3000:3587,"SEE BELOW"
	goto OPRINT3
.END PATCH 9.36 REPLACED LOGIC
OPRINT2
	match	"EXCHANGE",EXCHANGE
	goto REALPPM IF NOT EQUAL
.START PATCH 9.36 - REPLACED LOGIC
.	pack	PRICECK,F3,PERIOD,F2
.	match	"   .00",PRICECK
.	goto REALPPM IF NOT EQUAL
.	prtpage	Laser;*p=3000:4076,"EXCHANGE"," ",FLAGPAID
	if (NSEL2PRICE <> C0)
		goto REALPPM
	endif
	prtpage	Laser;*p=3000:3587,"EXCHANGE"
.END PATCH 9.36 - REPLACED LOGIC
	goto OPRINT3
REALPPM
.START PATCH 9.36 - REPLACED LOGIC
.	prtpage	Laser;*p=3000:4076,F3,".",F2," ",EXCHANGE,FLAGPAID
	unpack	NSEL2PRICE,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	pack	taskname,str9,NMODDESC," ",EXCHANGE
.	prtpage	Laser;*p=3000:3587,str9,NMODDESC," ",EXCHANGE,FLAGPAID
	prtpage	Laser;*p=3000:3587,taskname
.END PATCH 9.36 - REPLACED LOGIC
OPRINT3
.START PATCH 9.36 - ADDED LOGIC
	deleteitem PackData,0
.	move	C0,PriceFlag
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
.START PATCH 9.37 REPLACED LOGIC
.				pack	taskname,NREFDESC,NADDPRICE,NMODDESC
				pack	taskname,NREFDESC,NSEL3PRICE,NMODDESC
.END PATCH 9.37 REPLACED LOGIC
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
.START PATCH 9.37 REPLACED LOGIC
.				pack	taskname,NREFDESC,NSLTPRICE,NMODDESC
				pack	taskname,NREFDESC,NSEL3PRICE,NMODDESC
.END PATCH 9.37 REPLACED LOGIC
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
.			if (result = 10 & howmany > 10)
.				prtpage	Laser;*p=N8:N9,"Additional Prices Below"
..				move	C1,PriceFlag
.				break
.			endif
			prtpage	Laser;*p=N8:N9,taskname
		repeat
	endif
.END PATCH 9.36 - ADDED LOGIC
.START PATCH 9.36 - REPLACED LOGIC
.	prtpage	Laser;*p=1000:4451,OMLRKY
.	prtpage	Laser;*p=1000:4751,MEDIA,"  ",MEDMEMO
.	prtpage	Laser;*p=1000:5051,RTCNTCT
.	prtpage	Laser;*p=125:5226,"##",ORTNNUM,B1,CORTN
.	prtpage	Laser;*p=1000:5226,RTCOMP,B1,rtphmask
.	call	Trim using RTADDR
.	if (ORTNNUM = "0001")
.		prtpage	Laser;*p=1000:5401,RTADDR,B1,OREUSE
.	else
.		prtpage	Laser;*p=1000:5401,RTADDR
.	endif
.	call	Trim using RTCITY
.	if (RTCITY <> "")
.		pack	taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
.	else
.		pack	taskname,RTSTATE,B1,B1,RTZIP
.	endif
.	prtpage	Laser;*p=1000:5576,taskname
.	prtpage	Laser;*p=125:6129,COMSLCT
.	prtpage	Laser;*p=1250:6501,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
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
.START PATCH 9.38.6 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!
.		pack	taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
		if (RTNUM = "5318")
			pack	taskname,"incoming.files@donnelley.infousa.com"
		else
			pack	taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
		endif
.END PATCH 9.38.6 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
	else
		pack	taskname,RTSTATE,B1,B1,RTZIP
	endif
	prtpage	Laser;*p=1000:6276,taskname
	prtpage	Laser;*p=125:6501,COMSLCT
	prtpage	Laser;*p=1250:6689,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.END PATCH 9.36 - REPLACED LOGIC
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
.START PATCH 9.1 REPLACED LOGIC
.	goto cntexit if equal
.	reset	cnt
.	scan	"Billing",cnt
.	goto	cntexit if equal
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
.END PATCH 9.1 REPLACED LOGIC
	call	RemoveChar using cntname,B1
	call	Trim using cntname
	if (cntname = "")
		goto cntexit
	endif
.begin patch 9.49a
.	IF	(OcompID = "P")
	IF	(CntComp = "2")
	pack	intrnet,cntname,"@pacificlists.com"
	Else
	pack	intrnet,cntname,"@nincal.com"
	endif
.End patch 9.49
.START PATCH 9.1 MOVED LOGIC
cntexit
.END PATCH 9.1 MOVED LOGIC
	prtpage	Laser;*p=5625:9500,CNT
	prtpage	Laser;*p=5625:9700,cntphone
	prtpage	Laser;*p=5625:9900,intrnet
.START PATCH 9.1 MOVED LOGIC
.cntexit
.	reset	cnt
.END PATCH 9.1 MOVED LOGIC
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
.END PATCH 9.38.2 REPLACED LOGIC
	goto prtdone if equal              .already in list
	reset	attchlst
	setlptr	ATTCHLST,LPTCNT
	endset	attchlst
	append	",",attchlst
.START PATCH 9.38.2 REPLACED LOGIC
.	append	str12,attchlst            .add to list
	append	str14,attchlst            .add to list
.END PATCH 9.38.2 REPLACED LOGIC
	compare	C2,faxflag
	if equal          .yes
		move	"                                        ",APIFileName
		clear	APIFileName
.START PATCH 9.38.2 REPLACED LOGIC
.		pack	APIFileName,dcxpath,str12,hexzero
		pack	APIFileName,dcxpath,str14,hexzero
.END PATCH 9.38.2 REPLACED LOGIC
		call	FindFirstFile
		if (APIResult <> 0 & APIResult <> hexeight)
.START PATCH 9.38.2 REPLACED LOGIC
.			write	output,seq;str12
			write	output,seq;str14
.END PATCH 9.38.2 REPLACED LOGIC
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
.Create all the cover sheets
.START PATCH 9.43 REPLACED LOGIC
.	for N4,C1,CountIndex
.		if (CountArray(N4) > C0)
.			move	N4,NFULFLD
.			rep	zfill,NFULFLD
.			move	C1,NFULPATH
.			move	"FULEOJ-NFULKEY",Location
.			pack	KeyLocation,"Key: ",NFULFLD
.			call	NFULKEY
..
.			clear	faxattn
.			call	Trim using NFULCNT
.			if (NFULCNT = "")
.				move	"Order Fulfillment",faxattn
.			else
.				move	NFULCNT,faxattn
.			endif
.			move	C0,ownscnt
.			move	C0,owncnt
.			move	CountArray(N4),owncnt
.			move	C2,faxflag        .yes set fax fulfilment flag on.
.			call	Trim using NFULFAX
.			move	NFULFAX,FAXTELE
..			clear	recname
.			fill	B1,faxname
.			clear	faxname
.			move	NFULCOMP,faxname
.			move	NFULNUM,holdccto
.			move	NFULCNT,holdcccnt
.			move	NFULCOMP,holdcccmp
.			pack	fhandle,"f",NFULNUM
.			call	prepfax
.		endif
.	repeat
.	return
. for N6 from c1 to countindex using 1
.begin patch 9.6
.find company in record
	call	debug
		Move	c0,n3                                  .set to zero
		for	CountIndex,"1","999"
		MOve	CountRecord(countindex).CntREcComp to N6               .get company
		MOve	CountRecord(countindex).CntREcCount to n3               .get count
.		Repeat
			
.		add	C1,CountArray(N6)

.	for N6,C1,CountIndex
.		if (CountArray(N6) > C0)
		If	(n3 > c0)
.end patch 9.6
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
.//Patch 9.48
						Packkey CNCTFLD2 to "01X",COMPNUM
						Call	CNCTAIM
						loop
						until over
						until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
							call	CNCTKG
						repeat
.//Patch 9.48
					call	Trim using CNCTFNAME
					if (CNCTFNAME = "")
						move	"Order Fulfillment",faxattn
					else
						move	CNCTFNAME,faxattn
					endif
					move	C0,ownscnt
					move	C0,owncnt
.begin patch 9.6
.					move	CountArray(N6),owncnt
					MOve	CountRecord(countindex).CntREcCount to owncnt               .get count
					
.end patch 9.6
					move	C2,faxflag        .yes set fax fulfilment flag on.
					call	Trim using COMPFAX
					move	COMPFAX,FAXTELE
					fill	B1,faxname
					clear	faxname
					move	COMPCOMP,faxname
					move	COMPNUM,holdccto
					move	CNCTFNAME,holdcccnt
					move	COMPCOMP,holdcccmp
					pack	fhandle,"f",COMPNUM
					call	prepfax
				endif
			endif
		endif
	repeat
	return
.END PATCH 9.43 REPLACED LOGIC
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
.logic added 9.38.1
.	move	"David Pugh",faxattn
	move	"Teri Morrow",faxattn
.logic added 9.38.1
	return
heather
.logic added 9.38.1
.	move	"Heather Rudinsky",faxattn
	move	"Teri Morrow",faxattn
.logic added 9.38.1
	return
evelyn
	move	"Evelyn Raymond",faxattn
	return

prtordfrm
	call	PrtOpenFile
	clear	str2
	pack	str2,OSALES10,OSALES
.begin patch 9.52	
.	if (OformBflag = "Y" & (str2 = "06" | str2 = "19" | str2 = ""))          .list management
	if (OformBflag = "Y" & (str2 = "06" | str2 = "19" | str2 = ""| str2 = "27" | str2 = "28" ))          .list management
.end patch 9.52	
		call	prtordfrmGuiA
	else
		call	prtordfrmGuiB
	endif
	return

PrtOpenFile
.printing  not faxing - no or invalid fax number
	if (FirstFlag = YES)
		call	PrtCloseFile
.START PATCH 9.38.4 ADDED LOGIC
		if (PDFFlag = 1)
			PRTOPEN	Laser,"PDF995",inpname
			return
		endif
.END PATCH 9.38.4 ADDED LOGIC
		if (PrintFlag = C1)
.Patch9.352
			trap RetryPrint if SPOOL
.Patch9.352
;SUBPATCH9.32
			if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
;			if (osflag = c2)         .nt
				PRTOPEN Laser,"\\NTS0\Laser8",prtname
			ELSEIF (OSFLAG = "3" or OSFLAG = "4")		.95/98
;                        elseif (osflag = c1)         .win 95 98
				PRTOPEN Laser,"Laser8",prtname
			else   .(osflag = c0)         .Don't know prompt for printer
				PRTOPEN Laser,"-",prtname
			endif
;END PATCH9.32
			move	C0,PrintFlag
			move	C1,FaxNumFlag
		else
			PRTOPEN Laser,"faxfile",""
		endif
.Patch9.352
		TrapClr	SPOOL
.Patch9.352
		move	NO,FirstFlag
		if (hotflag = 1 AND formflag = 1 AND epsiflag = YES)
.Full Run, Mailer Run, New Broker/Mailer combo, Epsilon as Broker
			prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
			call	newcover2
			prtpage	Laser;*NEWPAGE
		endif
	else
;dave's attempt 6/4/2003
		prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
;End dave's attempt
		prtpage	Laser;*NEWPAGE
	endif
	return

PrtCloseFile
	prtclose Laser
	call	Trim using prtname
	if (prtname <> "")
.HotOrders will always have this value filled.  This is for regular run
		if (hotflag <> 2)
.START PATCH 9.38.5 ADDED LOGIC
			if (!externalmode)
.END PATCH 9.38.5 ADDED LOGIC
.HotOrders should not rename first.  This is for regular run.
				pack	taskname,"\\nts0\d\data\fax\",prtname
				pack	APIFileName,taskname,hexzero
				call	FindFirstFile
				if (APIResult <> 0 & APIResult <> hexeight)
					erase	taskname
				endif
.START PATCH 9.38.3 REPLACED LOGIC
.			pack	APIFileName,"\\nts0\d\data\fax\faxfile.prn",hexzero
.			call	FindFirstFile
.			if (APIResult <> 0 & APIResult <> hexeight)
.				rename   "\\nts0\d\data\fax\faxfile.prn",taskname
.			endif
.....................................
				pack	APIFileName,"C:\WORK\faxfile.prn",hexzero
				call	FindFirstFile
				if (APIResult <> 0 & APIResult <> hexeight)
					rename   "C:\WORK\faxfile.prn",taskname
				endif
.END PATCH 9.38.3 REPLACED LOGIC
.Following will prevent files from being erased/overwritten
				clear	prtname
.START PATCH 9.38.5 ADDED LOGIC
			endif
.END PATCH 9.38.5 ADDED LOGIC
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
.Patch9.352
RetryPrint
.begin patch 9.62
.		move    "Nord002l May have a spool error.",SmtpSubject Subject
		move    "Nord002l May have a spool error.",MailSubjct
.		move    "Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print)",SmtpTextMessage(1)   Array <Text message >
		Clear	MailBody
		Append	"Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print)",MailBody
		Append	CRLF,MailBOdy
		call EMAILIS
		if (PrintFlag = C1)
			if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
				PRTOPEN Laser,"\\NTS0\Laser8",prtname
			ELSEIF (OSFLAG = "3" or OSFLAG = "4")		.95/98
				PRTOPEN Laser,"Laser8",prtname
			else   .(osflag = c0)         .Don't know prompt for printer
				PRTOPEN Laser,"-",prtname
			endif
		else
			PRTOPEN Laser,"faxfile",""
		endif
		return
RetryPrint2
.		move    "Nord002l May have a spool error trying to open faxfile.",SmtpSubject Subject
		move    "Nord002l May have a spool error trying to open faxfile.",MailSubjct
		Clear	MailBody
.		move    "Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print2)",SmtpTextMessage(1)   Array <Text message >
		Append    "Please Check Nord002l to see if it recovered from a spooling error. (Location: Retry Print2)",MailBody
		Append	CRLF,MailBOdy
.end patch 9.62
		call EMAILIS
		prtopen	file1,"FAXFILE",""
		return

EMAILIS
.begin patch 9.62
.;.   Set the text message that is send with the attachments
	Reset	MailBOdy
.	move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.	move    "NTS4",SmtpEmailServer                   Address of email serverc
.	clear   smtpemailaddress
.	append  "ComputerRequest",SmtpEmailAddress
.	append  "@nincal.com",SmtpEmailAddress
.	reset   smtpemailaddress
.	move    "Computer Request",SmtpUserName                                User name
.;   Set the destinations of the email. Max 100 (Mime spec)
.	move    smtpemailaddress,SmtpDestinations(1,1)
.	move    "Computer Request",SmtpDestinations(1,2)
.	move    "InformationServices@nincal.com",SmtpDestinations(2,1)
.	move    "Information Services",SmtpDestinations(2,2)
	pack	Mailto,"InformationServices@nincal.com"
	Pack	MailFrom,"ComputerRequest@nincal.com"
.	move    "2",SmtpDestIndexLast                          originators UserName
.	move    "",SmtpAttachments(1,1)                     Attached file name
.	move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.	clear   SmtpLogFile                                         'Clear' disables the LogFile
.	move    "1",SmtpProgress                                    Enable progress bars
.	call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	call	SendMail
.end patch 9.62
	return

.START PATCH 9.38.5 ADDED LOGIC
webGenerate routine DimPtr
	set	externalmode
	move	"2",FUNC
	pack	INPNAME from DimPtr,"M","WEB"
	move	"NORD002L",PROGRAM
	call	start
	return
.END PATCH 9.38.5 ADDED LOGIC

.Patch9.352

	include	prtorderpage1.inc
.patch9.37
				include	compio.inc
				include	cntio.inc
.	include	NMLRIO.INC
.patch9.37
	include	NCRCIO.INC
	include	NRTNIO.INC
	include	NOWNIO.INC
	include	NINVIO.INC
.patch9.37
.	include	NBRKIO.INC
.patch9.37
	include	nordio.inc
	include	nspIIO.inc
	include	nspeio.inc
	include	nsmpio.inc
	include	NOFRIO.INC
	include	ncntio.inc
	include	hpio.inc
.START PATCH 9.43 REMOVED LOGIC
.	include       NFULIO.inc
.END PATCH 9.43 REMOVED LOGIC
.START PATCH 9.36 - ADDED LOGIC
	INCLUDE	NSEL2io.INC
	INCLUDE	NSEL3io.INC
	INCLUDE	NADDIO.INC
	INCLUDE	NSLTIO.INC
	INCLUDE	NREFIO.INC
	INCLUDE	NMODIO.INC
.END PATCH 9.36 - ADDED LOGIC
	include	COMLOGIC.INC

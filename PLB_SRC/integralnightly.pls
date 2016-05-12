PC	EQU	1
	include	common.inc
	include	cons.inc
	include	nxngdd.inc
	include	nxchdd.inc
	include	winapi.inc

Release          Init         "1.0"    12MAY2006 ASH  Initial Release of nightly file dump for Integral

.EXTERNAL ROUTINES FROM	INTEGRAL3.PLC
Integral3Run external "INTEGRAL3;Integral3Run"
output	file

mss1	plform	Error

	formload mss1

	call	Paint
ExchangeFile
	close	output
	erase	"c:\work\ExchFile.dat"
	prepare	output,"c:\work\ExchFile.dat",exclusive
	display	*P10:10,*EL,"Creating ExchFile.dat"
	move	C0,howmany
	loop
		pack	Location,"NXNGSEQ"
		call	NXNGSEQ
		until over
		add	C1,howmany
		display	*p10:12,"Exchange records ",howmany
		clear	NXCHFLD1
		pack	NXCHFLD1,ACCKEY,ENTRY
		rep	ZFILL,NXCHFLD1
		move	"NO BALANCE ",ERROR
		move	C1,NXCHPATH
		pack	Location,"NXCHKEY"
		call	NXCHKEY
.		if not over
			write	Output,SEQ;ACCKEY:
				nxngdate:
				ENTRY:
				Flag:
				Usage1:
				Usage2
.		endif
	repeat
ZipCreate
	display	*P10:10,*EL,"Creating Zip File"
	erase	"c:\work\IntNight.zip"
	pack	taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe c:\work\IntNight.zip c:\work\Exchfile.dat"
	execute	taskname
emailing
.Email the ZIP file
	move	"Files from NIN",SmtpSubject Subject
	move	"0",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	move	"NTS4",SmtpEmailServer			 Address of email serverc
	move	"InformationServices@nincal.com",SmtpEmailAddress
	move	"Information Services",SmtpUserName				  User name
	move	"Information Services",SmtpUserFullName		    User Full Name
	move	"DavidHerrick@nincal.com",SmtpDestinations(1,1)
	move	"1",SmtpDestIndexLast				    Index to last entry	in Dest	array
.
	pack	APIFileName,"c:\work\IntNight.zip",hexzero
	call	FindFirstFile
	if (APIResult <> 0 & APIResult <> hexeight)
		move	"IntNight.zip",SmtpAttachments(1,1)				.Attached file name
		move	"c:\work",SmtpAttachments(1,2)				.Path to attached file name
		move	"1",SmtpAttIndexLast				    Index to last entry	- Only 1 entry
		clear	SmtpLogFile					    'Clear' disables the LogFile
		call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	endif
	move	"!\\nins1\e\apps\winbatch\IntegralFTP2",taskname
	execute	taskname
	shutdown "cls"

ShutDown
	move	"Files from NINCA",SmtpSubject Subject
	move	"0",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	move	"NTS4",SmtpEmailServer			 Address of email serverc
	move	"InformationServices@nincal.com",SmtpEmailAddress
	move	"Information Services",SmtpUserName				  User name
	move	"Information Services",SmtpUserFullName		    User Full Name
	move	"InformationServices@nincal.com",SmtpDestinations(1,1)
	move	"1",SmtpDestIndexLast				    Index to last entry	in Dest	array
	move	"Nordtest.plc not available!",SmtpTextMessage(1)   Array <Text message >
	move	"IntegralNightly.plc did not run!!",SmtpTextMessage(2)   Array <Text message >
	move	"2",SmtpTextIndexLast
	clear	SmtpLogFile					    'Clear' disables the LogFile
	call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	shutdown "cls"

	include	nxngio.inc
	include	nxchio.inc
	include	comlogic.inc
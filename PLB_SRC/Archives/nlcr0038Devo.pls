........................................
. Program:      NLCR0038.PLS
. Function:     In-House LCR Galley Report Program
. Author:       Andrew Harkins
. Orig. Date:   AUGUST 19, 1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
;Patch2.03
			include	compdd.inc
			include	cntdd.inc
.        include nmlrdd.inc
;Patch2.03
        include nxrfdd.inc
        include ndatdd.inc
        include nowndd.inc
        include npnddd.inc
        include nord5dd.inc
        include nspedd.inc
.START PATCH 1.75 REPLACED LOGIC
.        include contact1.inc
        include ncntdd.inc
.END PATCH 1.75 REPLACED LOGIC
        include nsmpdd.inc
.START PATCH 1.2 - ADDED LOGIC
        include nofrdd.inc
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 1.4 - ADDED LOGIC
        include winapi.inc
.END PATCH 1.4 - ADDED LOGIC
.START PATCH 2.0A ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 2.0A ADDED LOGIC

.TESTING FOR WATERMARK
.PICT1   PICT
.        CREATE	PICT1=3:13:30:50:
.                "F:\LIBRARY\DEVELOP\2NDREQUEST9.BMP"
.RESULT2 FORM    9
.RESULT3 FORM    9

release  init      "2.15"        DLH	08Mar2007 PL
.release  init      "2.14"        DLH	05Dec2006 patch to SendMail
.release  init      "2.13"        ASH	03MAR2006 PATCHED PDF995.INI LOGIC
.release  init      "2.12"        ASH	27JAN2006	Bug fix with sort file collision
.release  init      "2.11"        ASH	20DEC2005	Bug fix with PDF File Dump option for Galley Report
.release  init      "2.10"        ASH	18JUN2005	Changed IP address of file manager
.release  init      "2.09"        ASH	18MAY2005	LISTMLR Conversion
.release  init      "2.08"        ASH	20APR2005	Modified how PDF files are emailed
.	Patch 2.08 adds new functionality:
. In general this program is run in 2 ways - as a batch program once a day w/o user intervention, AND
. as a dynamically called program from Program 1, Screen 5.  In this second scenario, the user is allowed
. to choose their printer preference, including 2 different PDF options:  1) Have server run program
. and email the file back to them.  2) Run program on their machine and save the file on their
. harddrive.  Option 1 is most convenient.  PDF995 is dynamically modified using 2 API calls.  Modifications
. establish email keywords as well as setting ProcessPDF, which triggers PDF995 to call a program
. when the PDF is finished being rendered.  This program is actually the argument of ProcessPDF.  What
. I do is create a temporary "flag file" prior to updating PDF995.INI file.  The program/argument for
. ProcessPDF is actually a short BAT that contains one command to delete this "flag file".  After the PDF
. has been created the program enters a loop testing for the existence of "flag file".  Once it no longer
. exists, I know that PDF995 has finished rendering the file.  Option 2 uses the "flag file" logic as
. well, but the file is simply saved, and an email message with a hyperlink is sent.  With this option
. the program is run on the users machine.
.
. Note:  Option 1 - logic to have PDF995 dynamically email PDF files in inconsistent.  Using WINEMAIL instead.
..........................................
.release  init      "2.07"        ASH	09DEC2004	FAXFILE
.release  init      "2.06"        ASH	02DEC2004	Conversion from Acrobat Distilller to PDF995
.release  init      "2.05"        ASH	02NOV2004	Sample File conversion - Mailer field increased to 6 bytes
.release  init      "2.04"        ASH	06AUG2004	Logo Conversion
.release  init      "2.03"        DMB	26MAY2004	Mailer Conversion
.release   init      "2.02"       21MAY04 ASH Removed Idea of the month - 1.93/1.95
.release   init      "2.01"       19Feb04 send SMTP message thru winbatch
.release   init      "2.00"       11Feb04 send SMTP message do Comp Request.
.RELEASE   INIT      "2.0A"        28JAN04  ASH  DATACARD CONVERSION
.RELEASE   INIT      "1.99"        11SEP2003 ASH  SAMPLE FORMAT CONVERSION
.RELEASE   INIT      "1.98"        12AUG2003 ASH  ADDED LIST OWNER PHONE NUMBER
.RELEASE   INIT      "1.97"        16JUN2003 ASH  ADDED LIST OWNER PHONE NUMBER
.RELEASE   INIT      "1.96"        08MAY2003 DMB  Corrected bug which possilby put wrong int fax # on clearance req
.RELEASE   INIT      "1.95"       16APR2003 ASH  MODIFIED PATCH 1.93 TO FILTER OUT EXCLUSIVE LIST
.RELEASE   INIT      "1.94"       20MAR2003 ASH  ADDED FAX NUMBER FOR LIST MANAGEMENT
.RELEASE   INIT      "1.93"       18FEB2003 ASH  ADDED LANGUAGE FOR IDEA OF THE MONTH WINNER
.RELEASE   INIT      "1.92"       19AUG2002 ASH  REMOVED REFERENCE TO PLBVOL_P
.RELEASE   INIT      "1.91"       30Jul2002 ASH  CORRECTED SMALL BUG
.RELEASE   INIT      "1.9"       12Jul2002 DLH  ADDED LOGIC TO use GetWinVer subroutine
.RELEASE   INIT      "1.89"       20MAR2001 ASH  ADDED LOGIC TO ALLOW FOR UNIQUE FILENAMES FOR PDF FILES
.RELEASE   INIT      "1.88"       07NOV2000 ASH  NEW SERVER PART TWO
.RELEASE   INIT      "1.87"       05OCT2000 ASH  CORRECTED LIMITATION WITH INPNAME - ADDED USERS DRIVE PATH
.RELEASE   INIT      "1.86"       02OCT2000 ASH NEW SERVER ADDED
.RELEASE   INIT      "1.85"       26SEP2000 ASH   New logic due to 8.4 releas
.RELEASE   INIT      "1.84"       02AUG2000 ASH   NEW SERVER SETUP
.RELEASE   INIT      "1.83"       18MAy2000 ASH   Reformatting of report as per Sales Meeting 5/18/2000
.RELEASE   INIT      "1.82"       16MAy2000 ASH   ADDED TEST LOGIC TO SEE IF DELETION OF EXISTING HEADER FILE WILL CURE FUNKY CONTROL CHARACTER DISPLAY FOR FAXES.
.RELEASE   INIT      "1.81"       01MAy2000 ASH   REPLACED EMAIL LOGIC, REPLACED CRITERIA FOR "Pending Order" verbage
.RELEASE   INIT      "1.8"       15MAR2000 ASH   RE-WORKED LOGIC FOR HEADER/FOOTER, REPLACED AREA CODE
.RELEASE   INIT      "1.75"      16MAR2000 ASH   REPLACED CONTACT1.INC WITH NCNTDD.INC
.RELEASE   INIT      "1.7"       18FEB2000 ASH   SAMPLE DIRECTORY MOVED
.release   init      "1.6"       17FEB2000 ASH   FONTS added/increased in CONS.INC
.release   init      "1.5"       07FEB2000 ASH   Added logic to test for file sizes of Samples
.release   init      "1.4"       02FEB2000 ASH   Added enhancement to allow reports to be produced through Program 1
.release   init      "1.3"       18JAN2000 ASH   Added enhancement to break by List
.release   init      "1.2"       22DEC99 ASH     Added Fax Number and Offer
.release   init      "1.1"       29Nov99 ASH     Replaced OTOCODE with ORENT
.release init    "1.0"   ASH 19AUG99  DEVELOPMENT RELEASE

.START PATCH 1.98 ADDED LOGIC
str60	dim	60
.END PATCH 1.98 ADDED LOGIC
.Counters
COUNTR  FORM    9
COUNTR2 FORM    9
.Files to open
prfile  pfile
input2  file
.>Patch 2.10
.input2i ifile    Name="NINPRINT.isi|20.20.30.103:502"
input2i ifile    Name="NINPRINT.isi|10.10.30.103:502"
.>Patch 2.10
input3  file
First   init    "Y"
GoodStat init   "zlpx"
PrtFlag dim     1
;begin patch 1.9
;osflag   form   1       .1=win 95,98, 2=NT
;end patch 1.9
HOLDOWN dim     4
HOLDLIST dim    6
COMPHOLD DIM    45
newdate1 dim    10
faxnum  dim     10
LONGDIST dim    1
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
.START PATCH 1.93 REPLACED LOGIC
.EditQuan dim    11
EditQuan dim    20
.END PATCH 1.93 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
.START PATCH 1.83 REPLACED LOGIC
.line1   dim     100
line1   dim     88
.END PATCH 1.83 REPLACED LOGIC
.line1   dim     90
.END PATCH 1.6 REPLACED LOGIC
Carr    init    0x7f
DESC003 dim     725     .DESC002 + O2DES

.Sample variables
DCXFile dim     120
.START PATCH 1.7 - REPLACED LOGIC
.dcxpath init    "f:\data\samples\s"
.START PATCH 1.86 REPLACED LOGIC
.dcxpath init    "G:\data\samples\s"
dcxpath DIM     45
.END PATCH 1.86 REPLACED LOGIC
.END PATCH 1.7 - REPLACED LOGIC
.START PATCH 1.99 REPLACED LOGIC
.dcxext  init    ".dcx"
dcxext  init    ".TIF"
.END PATCH 1.99 REPLACED LOGIC
FilePath dim    40
.START PATCH 2.05 REPLACED LOGIC
.SMPArray dim    7(50)
SMPArray dim    9(50)
.END PATCH 2.05 REPLACED LOGIC
SMPIndex form   2

.START PATCH 1.4 - ADDED VARS
PrintFlag form  "00"    .Default is Laser2!!!
input2Name dim  40
output1 dim     40
.START PATCH 2.11 REPLACED LOGIC
.output2 dim     40
output2 dim     80
.END PATCH 2.11 REPLACED LOGIC
userinfo dim    500
userlogn dim    7
userlogw dim    7
.hexeight integer 4,"4294967295"
timestamp1 dim  16
timestamp2 dim  16
time1   form    16
time2   form    16
time3   form    16
.END PATCH 1.4 - ADDED VARS
.START PATCH 1.5 - ADDED VARS
testint INTEGER 4
.END PATCH 1.5 - ADDED VARS
.START PATCH 2.08 ADDED LOGIC
PDFFlag	form	1
.END PATCH 2.08 ADDED LOGIC
.START PATCH 1.8 - REMOVED VAR
.pict1   pict
.pict2   pict
.END PATCH 1.8 - REMOVED VAR
pict3   pict
mss1    plform  Error
.START PATCH 1.6 REMMED LOGIC
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.....
font6   font
.Begin Patch 2.15
FontO7		font
FontO18B	font
.end Patch 2.15
....
.END PATCH 1.6 REMMED LOGIC
        formload mss1
.START PATCH 1.6 REMMED LOGIC
.Create fonts to be used
.START PATCH 1.83 REPLACED LOGIC
.        create  font1,"Arial",size=12,bold
.        create  font2,"Arial",size=8
.        create  font3,"Helvetica",size=9
.        create  font4,"Fixed",size=9
.        create  font5,"Arial",size=10
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
.END PATCH 1.83 REPLACED LOGIC
.Begin Patch 2.15
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
.End Patch 2.15
......
        create  font6,"Arial",size=14
......
.END PATCH 1.6 REMMED LOGIC
.START PATCH 2.04 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
.END PATCH 2.04 ADDED LOGIC

.	prtopen	PRFile,"",""
.	call	OrderPrintHeader
.	prtpage	prfile;*newpage
.	move	"P",CompExcl
.	call	OrderPrintHeader
.	stop

.START PATCH 1.8 - REPLACED LOGIC
..Create Header Logo
.        CREATE  PICT1=1:15:1:10:
.                "c:\progra~1\nincal\HEADER.BMP",AUTOZOOM
.        CREATE  PICT2=20:35:20:30:
.                "c:\progra~1\nincal\FOOTER.BMP",AUTOZOOM
.END PATCH 1.8 - REPLACED LOGIC
.START PATCH 1.4 - ADDED LOGIC
       PACK    DCXPATH,NTWKPATH1,"samples\s"
        match   "NLCR0038",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0038",PROGRAM
.START PATCH 1.86 REPLACED LOGIC
.                move    "G:\DATA\NPRINT2.LCR",input2Name
.                move    "G:\DATA\LCRFILE.DAT",output1
.                move    "G:\DATA\LCRFILE.SRT",output2
                PACK    input2Name,NTWKPATH1,"NPRINT2.LCR"
                PACK    output1,NTWKPATH1,"LCRFILE.DAT"
                PACK    output2,NTWKPATH1,"LCRFILE.SRT"
.END PATCH 1.86 REPLACED LOGIC
        else
                clear   input2Name
.START PATCH 1.87 ADDED LOGIC
.                append  "c:\work\",input2Name
                append  NTWKPATH3,input2Name
.END PATCH 1.87 ADDED LOGIC
.                append  "10461988",input2Name
.                append  ".DAT",input2Name
.                reset   input2Name
.                move    "2",PrintFlag
.                move    "aharkin",userlogn
.....................
                append  inpname,input2Name
                append  ".DAT",input2Name
                reset   input2Name
                move    prtname,PrintFlag
                move    user,userlogn
.                move    "USER",userinfo
.                clock   env,userinfo
.                scan    "LOGIN",userinfo
.                if  equal
.                        bump    userinfo,6
.                        clear   userlogw
.                        move    userinfo,userlogw
.                        movefptr userlogw,result
.                        scan    COMMA,userlogw
.                        if equal
.                                movefptr userlogw,howmany
.                                sub      C3,howmany
.                                reset    userlogw
.                                setlptr  userlogw,howmany
.                                clear    userlogn
.                                append   userlogw,userlogn
.                                reset    userlogn
.                        endif
.                else
.                        bump    userinfo,5
.                        move    userinfo,userlogn
.                endif
                call    Trim using userlogn
                clear   output1
                clear   output2
                clear   FilePath
.START PATCH 1.84 REPLACED LOGIC
.                append  "F:\USERS\",output1
.                append  "F:\USERS\",output2
.                append  "F:\USERS\",FilePath
........
                append  "\\nins1\D\USERS\",output1
                append  "\\nins1\D\USERS\",output2
                append  "\\nins1\D\USERS\",FilePath
.END PATCH 1.84 REPLACED LOGIC
                if (userlogn <> "")
                        append  userlogn,output1
                        append  "\",output1
                        append  userlogn,output2
                        append  "\",output2
                        append  userlogn,FilePath
                        append  "\",FilePath
.START PATCH 2.08 ADDED LOGIC
			if (PrintFlag = 2 | PrintFlag = 4)
				if (PrintFlag = 2)
.Clean up working directory on server.  If running on individual machine, do not worry about this.
.We do not want to clean up the directory on a users hard drive as they may be keeping files out there
.that they may later want.
					clear	taskname
					clear	str55
					call	getwinver   ;make sure we have osflag
					If (osflag = c1 | osflag =C5)
						append	"!c:\winnt\system32\cmd.exe",taskname
						append	"!c:\winnt\system32\cmd.exe",str55
					elseif              (osflag = C3 | osflag =C4)
						append	"!c:\command.com",taskname
						append	"!c:\command.com",str55
					elseif              (Osflag = C6)
						append	"!c:\windows\system32\cmd.exe",taskname
						append	"!c:\windows\system32\cmd.exe",Str55
					endif
					append	" /c del c:\work\PDF\*.pdf",taskname
					append	" /c del c:\work\PDF\*.log",str55
					reset	taskname
					reset	str55
					execute	taskname
					execute	str55
				endif
				call	"GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
					"Parameters":
					"ProcessPDF":
					str45
				move	"ProcessPDF",str25
				move	"\\nts0\c\apps\plb\code\pdftest.bat",str35
.
				call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
					"Parameters":
					"ProcessPDF":
					"\\nts0\c\apps\plb\code\pdftest.bat":
					result
				if (result = C0)
.Prepare Flag file
					move	C1,PDFFlag
					prep	tempfile,"c:\progra~1\pdf995\flag.dat"
					write	tempfile,SEQ;"flag set"
					close	tempfile
.Following logic not working so commenting out.  I still get corrupted files emailed.
.					if (PrintFlag = 2)
..PDF option with email attachment
.						move	C0,PDFFlag	.Initialize
.						move	"Email",str25
.						move	"1",str35
.						call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.							"Parameters":
.							str25:
.							str35:
.							result
.						if (result = C0)
.							move	"SMTP",str25
.							move	"1",str35
.							call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.								"Parameters":
.								str25:
.								str35:
.								result
.							if (result = C0)
.								move	"Email Recipient Address",str25
.								pack	taskname,userlogn,"@nincal.com"
.								move	taskname,str35
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									str25:
.									str35:
.									result
.								if (result = C0)
.									move	"Email Server",str25
.									move	"NTS4",str35
.									call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.										"Parameters":
.										str25:
.										str35:
.										result
.									if (result = C0)
..Following arguments are NOT mandatory, so success is NOT essential
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email Subject":
.											"Here is your PDF File":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email text":
.											"Here is your PDF File":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email From Address":
.											"batch32c@nincal.com":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email Recipient Name":
.											taskname:
.											result
.										move	C1,PDFFlag
.									else
.										move	C2,PDFFlag
.									endif
.								else
.									move	C2,PDFFlag
.								endif
.							else
.								move	C2,PDFFlag
.							endif
.						else
.							move	C2,PDFFlag
.						endif
.					elseif (PrintFlag = 4)
..PDF option with creation on hard drive
.						move	C0,PDFFlag	.Initialize
.						move	"Email",str25
.						move	"0",str35
.						call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.							"Parameters":
.							str25:
.							str35:
.							result
.						if (result = C0)
.							move	"SMTP",str25
.							move	"0",str35
.							call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.								"Parameters":
.								str25:
.								str35:
.								result
.							if (result = C0)
.								clear	str35
.								move	"Email Recipient Address",str25
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									str25:
.									str35:
.									result
.								if (result = C0)
.									move	"Email Server",str25
.									call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.										"Parameters":
.										str25:
.										str35:
.										result
.									if (result = C0)
..Following arguments are NOT mandatory, so success is NOT essential
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email Subject":
.											"":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email text":
.											"":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email From Address":
.											"":
.											result
.										call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.											"Parameters":
.											"Email Recipient Name":
.											"":
.											result
.										move	C1,PDFFlag
.									else
.										move	C2,PDFFlag
.									endif
.								else
.									move	C2,PDFFlag
.								endif
.							else
.								move	C2,PDFFlag
.							endif
.						else
.							move	C2,PDFFlag
.						endif
.					endif
				else
					move	C2,PDFFlag
				endif
				if (PDFFlag = C2)
.Send message to I.S.
					move	"This is a message from	NLCR0038",SmtpSubject Subject
					move	"Failure to update pdf995.ini.",SmtpTextMessage(1)
					pack	SmtpTextMessage(2),"Field: ",str25
					pack	SmtpTextMessage(3),"Value: ",str35
					move	"3",SmtpTextIndexLast
					move	"NTS4",SmtpEmailServer
					move	"InformationServices@nincal.com",SmtpEmailAddress
					move	"InformationServices",SmtpUserName
					move	"InformationServices",SmtpUserFullName
					move	smtpemailaddress,SmtpDestinations(1,1)
					move	"1",SmtpDestIndexLast
					move	"0",SmtpAttIndexLast
					clear	SmtpLogFile
					call	SmtpSend
					move	C0,PDFFlag
				endif
			endif
.END PATCH 2.08 ADDED LOGIC
                endif
.START PATCH 2.11 REPLACED LOGIC
.                append  "LCRFILE2.SRT",output2
		if (PrintFlag = 4)
			clock	timestamp,timestamp
			append	timestamp,output1
			append	".DAT",output1
			append	timestamp,output2
			append	".SRT",output2
		else
			append  "LCRFILE2.DAT",output1
                	append  "LCRFILE2.SRT",output2
                endif
.END PATCH 2.11 REPLACED LOGIC
                reset   output1
                reset   output2
                reset   FilePath
........................
.                move    "C:\WORK\LCRFILE2.DAT",output1
.                move    "C:\WORK\LCRFILE2.SRT",output2
.                move    "C:\WORK\",FilePath
        endif
.END PATCH 1.4 - ADDED LOGIC
        move    "NLCR0038",WPrognme
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
;begin patch 1.9
                    call                GetWinVer
;        getinfo system,str6
;        unpack  str6 into str1,str1
;        move    C0,osflag
;        if (str1 = "3" or str1 = "4")
;                move    C1,osflag
;.START PATCH 1.85 REPLACED LOGIC
;        elseif (str1 = "1" | str1 = "5")
;.END PATCH 1.85 REPLACED LOGIC
;                move    C2,osflag
;        endif
;end patch 1.9
.Set Flags to Open NINORD.DAT
        move    C0,NORDFLAG
        move    C0,NORDFLG2

.START PATCH 1.4 - REPLACED LOGIC
.        call    Paint
        if (PrintFlag = 0)
                call    Paint
        endif
.END PATCH 1.4 - REPLACED LOGIC

.START PATCH 1.4 - REPLACED LOGIC
.        DISPLAY *P1:24,"OPENING FILES";
        if (PrintFlag = 0)
                DISPLAY *P1:24,"OPENING FILES";
        endif
.END PATCH 1.4 - REPLACED LOGIC
.        goto printfile
.Open Files
.START PATCH 1.4 - REPLACED LOGIC
.        open    input2,"G:\DATA\NPRINT2.LCR"
.        open    input2i,"g:\data\index\ninprint.isi"
.        prepare input3,"G:\DATA\LCRFILE.DAT"
        open    input2,input2Name
.START PATCH 1.86 REPLACED LOGIC
.        open    input2i,"g:\data\index\ninprint.isi"
.        PACK    STR35,NTWKPATH1,"INDEX\NINPRINT.ISI"
.>Patch 2.10
.        PACK    STR35,"NINPRINT.ISI|20.20.30.103:502"
        PACK    STR35,"NINPRINT.ISI|10.10.30.103:502"
.>Patch 2.10
        open    input2i,STR35
.END PATCH 1.86 REPLACED LOGIC
        prepare input3,output1
.END PATCH 1.4 - REPLACED LOGIC

.START PATCH 1.4 - REPLACED LOGIC
.        DISPLAY "READING NINPRINT FILE";
        if (PrintFlag = 0)
                DISPLAY *P1:24,"READING NINPRINT FILE";
        endif
.END PATCH 1.4 - REPLACED LOGIC
.        goto printfile
LCRLoop
        loop
                read    input2,seq;ordvars
                until over
.                reset   GoodStat
.                scan    OSTAT,GoodStat
.                if equal
.                        if (OHIST = "l" | )
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
                                filepi  1;input3
.START PATCH 1.1 - REPLACED LOGIC
.                                write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM:
.                                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,OTOCODE,OELCODE,OQTY,OCOCODE:
.                                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.2 - REPLACED LOGIC
.                                write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.75 REPLACED LOGIC
.                                write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES,OFDESC:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.97 REPLACED LOGIC
.                                write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES,OFDESC:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
.START PATCH 2.0A ADDED LOGIC
.				write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.					OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,O2DES,OFDESC:
.					OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.					OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
.START PATCH 2.05 REPLACED LOGIC
.				write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.					OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
.					OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.					OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
				write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
					OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
					OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
					OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM
.END PATCH 2.05 REPLACED LOGIC
.END PATCH 2.0A ADDED LOGIC
.END PATCH 1.97 REPLACED LOGIC
.END PATCH 1.75 REPLACED LOGIC
.end PATCH 1.2 - REPLACED LOGIC
.END PATCH 1.1 - REPLACED LOGIC
.DELETE CANCELLED\DENIED RECORDS - SHOULD ALREADY BE DONE, THIS IS A FAIL SAFE!!!
.                        elseif (OSTAT = "z")
                        if (OSTAT = "z")
.START PATCH 1.4 - REPLACED LOGIC
.                                filepi  2;input2i
.                                read    input2i,OLRN;;
.                                if not over
..                                        delete  input2i,OLRN
.                                endif
........
                                if (PrintFlag = C0)
                                        filepi  2;input2i
                                        read    input2i,OLRN;;
                                        if not over
.                                                delete  input2i,OLRN
                                        endif
                                endif
.END PATCH 1.4 - REPLACED LOGIC
                        endif
.                endif
        repeat
.START PATCH 1.4 - REPLACED LOGIC
.        DISPLAY *P1:24,"SORTING LCRFILE FILE ";
        if (PrintFlag = 0)
                DISPLAY *P1:24,"SORTING LCRFILE FILE ";
        endif
.END PATCH 1.4 - REPLACED LOGIC
SortFile
        clear   taskname
.START PATCH 1.4 - REPLACED LOGIC
.        move    "LCRFILE.DAT,g:\data\LCRFILE.SRT,G:\DATA;1-4,5-10,15-20",taskname
.        reset   taskname
        append  output1,taskname
        append  COMMA,taskname
        append  output2,taskname
        append  ";",taskname
        append  "1-4,5-10,15-20",taskname
        reset   taskname
.END PATCH 1.4 - REPLACED LOGIC
.START PATCH 2.12 REPLACED LOGIC
.        sort    taskname
.        if over
.                move    s$error$,error
.                move    "Sort did not work!",Location
.                clear	KeyLocation
.                call    IOMssg
.                shutdown
.        endif
.        clear   taskname
	move	C0,N1
	for N2,1,10
		sort    taskname
		if not over
			move	C0,N1
			break
		else
			move	C1,N1
		endif
		pause	"5"
        repeat
	if (N1 = C1)
		move    s$error$,error
		move    "Sort did not work!",Location
		clear	KeyLocation
		call    IOMssg
.begin patch 2.16
	call	pdf995auto0
.end patch 2.16
 
		shutdown
	endif
.END PATCH 2.12 REPLACED LOGIC
.START PATCH 1.4 - REPLACED LOGIC
.        CALL    PAINT
.        DISPLAY *P1:24,"PRINTING FILES        ";
        if (PrintFlag = 0)
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
        endif
.END PATCH 1.4 - REPLACED LOGIC
.        shutdown
PrintFile
.Set up columns
        move    "500",column
.START PATCH 1.2 -ADDED LOGIC
.START PATCH 1.83 REPLACED LOGIC
.        move    "1700",column1
        move    "1200",column1
.END PATCH 1.83 REPLACED LOGIC
.END PATCH 1.2 -ADDED LOGIC
.START PATCH 1.6 - REPLACED LOGIC
        move    "2700",column2
.        move    "2500",column2
.END PATCH 1.6 - REPLACED LOGIC
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
.START PATCH 1.6 - REPLACED LOGIC
        move    "5700",column6
.        move    "5900",column6
.END PATCH 1.6 - REPLACED LOGIC
.        move    "6000",column6
        move    "6750",column7
.        move    "7200",column7
.Initialize HLDBRK
        clear   HOLDOWN
.close unsorted file
.
       close   input3
.
.open newly sorted file

.START PATCH 1.4 - REPLACED LOGIC
.        open    input3,"G:\DATA\LCRFILE.SRT"
        open    input3,output2
.END PATCH 1.4 - REPLACED LOGIC
        loop
.START PATCH 1.1 - REPLACED LOGIC
.                read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM:
.                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,OTOCODE,OELCODE,OQTY,OCOCODE:
.                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.2 - REPLACED LOGIC
.                read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.75 REPLACED LOGIC
.                read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES,OFDESC:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,OCNT,str15,UNIVERSE,NPNDDESC
.START PATCH 1.97 REPLACED LOGIC
.                read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                        OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,O2DES,OFDESC:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                        OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
.START PATCH 2.0A REPLACED LOGIC
.		read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.			OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,O2DES,OFDESC:
.			OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.			OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
.START PATCH 2.05 REPLACED LOGIC
.		read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.			OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
.			OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.			OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC
		read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
			OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
			OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
			OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM
.END PATCH 2.05 REPLACED LOGIC
.END PATCH 2.0A REPLACED LOGIC
.END PATCH 1.97 REPLACED LOGIC
.END PATCH 1.75 REPLACED LOGIC
.END PATCH 1.2 - REPLACED LOGIC
.END PATCH 1.1 - REPLACED LOGIC
                if (olon = "2006")
                        call    trapit
                endif
                goto    LastRec if over
.START PATCH 1.3 REPLACED LOGIC
.                if (OLON <> HOLDOWN)
                if (OLON <> HOLDOWN | OLNUM <> HOLDLIST)
.END PATCH 1.3 REPLACED LOGIC
                        if (FIRST = YES)
                                move    OWNOCPY,COMPHOLD
                        endif
                        if (FIRST = NO)
                                call    PrintSamplePage
                                clear   SMPArray
                                PRTCLOSE prfile
.START PATCH 1.4 - ADDED LOGIC
.This will only happen if called from Program 1
.START PATCH 2.08 REPLACED LOGIC
.                                if (PrintFlag = 2)
..It takes some time for the file to be created, so we must check
..Allow 20 seconds to originally load DISTILLER
..                                        move    "                                        ",APIFileName
..                                        clear   APIFileName
..                                        pack    APIFileName,"C:\WORK\",str55,hexzero
.                                        clock   timestamp,timestamp1
.                                        move    timestamp1,time1
.                                        loop
..                                                call    FindFirstFile
.                                                clock   timestamp,timestamp2
..                                                if (APIResult <> 0 & APIResult <> hexeight)
..                                                        unpack  timestamp2,str8,str8
..                                                        move    str8,N9
..                                                        unpack  APIFileData,str18,str2,str4,str4
..                                                        move    str4,testint
..                                                        move    testint,N8
..                                                        sub     N8,N9
..                                                        if (N9 > 2000)
..                                                                break
..                                                        endif
..                                                else
.                                                        move    timestamp2,time2
.                                                        sub     time1,time2,time3
.                                                        if (time3 > 3500) .20 Seconds Maximum
.                                                                break
.                                                        endif
..                                                endif
.                                        repeat
.                                        call    OrderCreatePDFFile
..................................................
                                if (PrintFlag = 2 | PrintFlag = 4)
					if (PDFFlag <> C1)	.Not using PDF995 to create Email
.It takes some time for the file to be created, so we must check.
.Allow 35 seconds
						clock	timestamp,timestamp1
						move	timestamp1,time1
						loop
							clock	timestamp,timestamp2
							move	timestamp2,time2
							sub	time1,time2,time3
							if (time3 > 3500) .35 Seconds Maximum
								break
							endif
						repeat
					else
.Give the email a chance of rendering itself before updating the INI file.
						pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.START PATCH 2.13 REPLACED LOGIC
.						loop
						for N3,C1,"100"
.END PATCH 2.13 REPLACED LOGIC
							call	FindFirstFile
							until (APIResult = 0 | APIResult = hexeight)
							pause	"1"
						repeat
						pause	"5"
.Prepare Flag file once again
						prep	tempfile,"c:\progra~1\pdf995\flag.dat"
						write	tempfile,SEQ;"flag set"
						close	tempfile
					endif
					call    OrderCreatePDFFile
.END PATCH 2.08 REPLACED LOGIC
                                endif
.END PATCH 1.4 - ADDED LOGIC
                                if (PrtFlag = NO)
.This will never be the case if called from Program 1
                                        clear   taskname
;begin patch 1.9
                                        call                getwinver   ;make sure we have osflag
;                                        Path    Exist,"c:\windows"
;                                        if      over
                                        If                  (osflag = c1 | osflag =C5)
                                                append  "!c:\winnt\system32\cmd.exe",taskname
;                                        else
                                        elseif              (osflag = C3 | osflag =C4)
                                                append  "!c:\command.com",taskname
                                        elseif              (Osflag = C6)
                                                append  "!c:\windows\system32\cmd.exe",taskname
;end patch 1.9
                                        endif
.START PATCH 1.88 REPLACED LOGIC
.                                        append  " /c copy f:\data\fax\hdrfile.prn /b + f:\data\fax\faxfile.prn /b f:\data\fax\fax",taskname
                                        append  " /c copy ",taskname
.START PATCH 2.07 REPLACED LOGIC
.                                        append  NTWKPATH4,taskname
.                                        append  "fax\hdrfile.prn /b + ",taskname
.                                        append  NTWKPATH4,taskname
.                                        append  "fax\faxfile.prn /b ",taskname
.............................
                                        append  "C:\WORK\hdrfile.prn /b + ",taskname
                                        append  "C:\WORK\faxfile.prn /b ",taskname
.END PATCH 2.07 REPLACED LOGIC
                                        append  NTWKPATH4,taskname
                                        append  "fax\fax",taskname
.END PATCH 1.88 REPLACED LOGIC
                                        append  HOLDOWN,taskname
                                        append  ".prn /b",taskname
                                        reset   taskname
                                        execute taskname
                                        clear   taskname
;begin patch 1.9
                                        call                getwinver   ;make sure we have osflag
;                                        Path    Exist,"c:\windows"
;                                        if      over
                                        If                  (osflag = c1 | osflag =C5)
                                                append  "!c:\winnt\system32\cmd.exe",taskname
;                                        else
                                        elseif              (osflag = C3 | osflag =C4)
                                                append  "!c:\command.com",taskname
                                        elseif              (Osflag = C6)
                                                append  "!c:\windows\system32\cmd.exe",taskname
;end patch 1.9
                                        endif
.START PATCH 1.88 REPLACED LOGIC
.                                        append  " /c copy f:\data\fax\fax",taskname
                                        append  " /c copy ",taskname
                                        append  NTWKPATH4,taskname
                                        append  "fax\fax",taskname
.END PATCH 1.88 REPLACED LOGIC
                                        append  HOLDOWN,taskname
                                        append  ".prn \\nts2\fax",taskname
                                        reset   taskname
                                        execute taskname
                                endif
                                call    OrderOpenFile
                        else
                                move    NO,FIRST
                                call    OrderOpenFile
                        endif
                        move    OLON,HOLDOWN
                        move    OLNUM,HOLDLIST
                        move    OWNOCPY,COMPHOLD
                        clear   page
                        call    OrderPrintHeader
                        call    OrderListHeader
.START PATCH 1.3 REPLACED LOGIC
.                elseif (OLNUM <> HOLDLIST)
.                        move    OLNUM,HOLDLIST
.                        call    OrderListHeader
.END PATCH 1.3 REPLACED LOGIC
                endif
                call    OrderPrintRecord
                if (OSCODE = "1")
.START PATCH 1.2 - REPLACED LOGIC
.                        move    C1,SMPIndex
.                        move    C0,N1
.                        pack    str7,OMLRNUM,OSAMCDE
.                        loop
.                                clear   str8
.                                move    SMPArray(SMPIndex),str8
.                                scan    str7,str8
.                                if equal
.                                        move    C1,N1
.                                        break
.                                endif
.                                until   (SMPIndex = 50)
.                                until   (SMPArray(SMPIndex) = "")
.                                add     C1,SMPIndex
.                        repeat
.                        if (N1 = C0)
.                                move    str7,SMPArray(SMPIndex)
.                        endif
.
                        type    OSAMCDE
                        if equal
                                if (OSAMCDE <> "000")
                                        move    C1,SMPIndex
                                        move    C0,N1
.START PATCH 2.05 REPLACED LOGIC
.                                        pack    str7,OMLRNUM,OSAMCDE
.                                        loop
.                                                clear   str8
.                                                move    SMPArray(SMPIndex),str8
.                                                scan    str7,str8
.                                                if equal
.                                                        move    C1,N1
.                                                        break
.                                                endif
.                                                until   (SMPIndex = 50)
.                                                until   (SMPArray(SMPIndex) = "")
.                                                add     C1,SMPIndex
.                                        repeat
.                                        if (N1 = C0)
..START PATCH 1.5 - REPLACED LOGIC
..                                                move    str7,SMPArray(SMPIndex)
.                                                move    "                                        ",APIFileName
.                                                clear   APIFileName
.                                                pack    APIFileName,dcxpath,str7,dcxext,hexzero
.                                                call    FindFirstFile
.                                                if (APIResult <> 0 & APIResult <> hexeight)
.                                                        unpack  APIFileData,str32,str4
.                                                        move    str4,testint
.                                                        if (testint > "10000")  .minimum file size in bytes
.                                                                move    str7,SMPArray(SMPIndex)
.                                                        else
.                                                                call    OrderBadSample
.                                                        endif
.                                                else
.                                                        call    OrderBadSample
.                                                endif
..END PATCH 1.5 - REPLACED LOGIC
.                                        endif
....................................................................
                                        pack    str9,COMPNUM,OSAMCDE
                                        loop
                                                clear   str10
                                                move    SMPArray(SMPIndex),str10
                                                scan    str9,str10
                                                if equal
                                                        move    C1,N1
                                                        break
                                                endif
                                                until   (SMPIndex = 50)
                                                until   (SMPArray(SMPIndex) = "")
                                                add     C1,SMPIndex
                                        repeat
                                        if (N1 = C0)
                                                move    "                                        ",APIFileName
                                                clear   APIFileName
                                                pack    APIFileName,dcxpath,str9,dcxext,hexzero
                                                call    FindFirstFile
                                                if (APIResult <> 0 & APIResult <> hexeight)
                                                        unpack  APIFileData,str32,str4
                                                        move    str4,testint
                                                        if (testint > "10000")  .minimum file size in bytes
                                                                move    str9,SMPArray(SMPIndex)
                                                        else
                                                                call    OrderBadSample
                                                        endif
                                                else
                                                        call    OrderBadSample
                                                endif
                                        endif
.END PATCH 2.05 REPLACED LOGIC
                                endif
                        endif
.END PATCH 1.2 - REPLACED LOGIC
                endif
        repeat

LastRec
        call    PrintSamplePage
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.START PATCH 1.4 - ADDED LOGIC
.This will only happen if callded from Program 1
        if (PrintFlag = 2 | PrintFlag = 4)
.START PATCH 2.08 REPLACED LOGIC
..It takes some time for the file to be created, so we must check
.                move    C0,N9
.                move    "                                        ",APIFileName
.                clear   APIFileName
..START PATCH 2.06 REPLACED LOGIC
..                pack    APIFileName,"C:\WORK\",str55,hexzero
.                pack    APIFileName,"C:\WORK\PDF\",str55,hexzero
..END PATCH 2.06 REPLACED LOGIC
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
..                        call    FindFirstFile
.                        clock   timestamp,timestamp2
..                        if (APIResult <> 0 & APIResult <> hexeight)
..                                unpack  timestamp2,str8,str8
..                                unpack  APIFileData,str18,str2,testint
..                                move    testint,N8
..                                move    str8,N9
..                                sub     N8,N9
..                                if (N9 > 2000)
..                                        break
..                                endif
..                        else
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
.                                if (time3 > 3500) .20 Seconds Maximum
.                                         break
.                                endif
..                        endif
.                repeat
.                call    OrderCreatePDFFile
...........................................
		if (PDFFlag <> C1)	.Not using PDF995 to create Email
.It takes some time for the file to be created, so we must check.
.Allow 35 seconds
			clock	timestamp,timestamp1
			move	timestamp1,time1
			loop
				clock	timestamp,timestamp2
				move	timestamp2,time2
				sub	time1,time2,time3
				if (time3 > 3500) .35 Seconds Maximum
					break
				endif
			repeat
		else
.Give the email a chance of rendering itself before updating the INI file.
			pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
			loop
				call	FindFirstFile
				until (APIResult = 0 | APIResult = hexeight)
				pause	"1"
			repeat
			pause	"5"
		endif
		call    OrderCreatePDFFile
		move	"ProcessPDF",str25
		clear	str35
		call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
			"Parameters":
			str25:
			"":
			result
		if (result = 0)
			move	C1,PDFFlag
.			if (PrintFlag = 2)
..PrintFlag = 4 has already cleared these fields.
.				move	C0,PDFFlag	.Initialize
.				move	"Email",str25
.				move	"0",str35
.				call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.					"Parameters":
.					str25:
.					str35:
.					result
.				if (result = C0)
.					move	"SMTP",str25
.					move	"0",str35
.					call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.						"Parameters":
.						str25:
.						str35:
.						result
.					if (result = C0)
.						move	"Email Recipient Address",str25
.						clear	str35
.						call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.							"Parameters":
.							str25:
.							str35:
.							result
.						if (result = C0)
.							clear	str35
.							move	"Email Server",str25
.							call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.								"Parameters":
.								str25:
.								str35:
.								result
.							if (result = C0)
..Following arguments are NOT mandatory, so success is NOT essential
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									"Email Subject":
.									str35:
.									result
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									"Email text":
.									str35:
.									result
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									"Email From Address":
.									str35:
.									result
.								call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.									"Parameters":
.									"Email Recipient Name":
.									str35:
.									result
.							else
.								move	C2,PDFFlag
.							endif
.						else
.							move	C2,PDFFlag
.						endif
.					else
.						move	C2,PDFFlag
.					endif
.				else
.					move	C2,PDFFlag
.				endif
.			endif
		else
			move	C2,PDFFlag
		endif
		if (PDFFlag = C2)
.Send message to I.S.
			move	"This is a message from	NLCR0038",SmtpSubject Subject
			move	"Failure to re-update pdf995.ini.",SmtpTextMessage(1)
			move	"Pdf995.ini has been modified.",SmtpTextMessage(2)
			pack	SmtpTextMessage(3),"Field: ",str25
			pack	SmtpTextMessage(4),"Value: ",str35
			move	"4",SmtpTextIndexLast
			move	"NTS4",SmtpEmailServer
			move	"InformationServices@nincal.com",SmtpEmailAddress
			move	"InformationServices",SmtpUserName
			move	"InformationServices",SmtpUserFullName
			move	smtpemailaddress,SmtpDestinations(1,1)
			move	"1",SmtpDestIndexLast
			move	"0",SmtpAttIndexLast
			clear	SmtpLogFile
			call	SmtpSend
			move	C0,PDFFlag
		endif
.END PATCH 2.08 REPLACED LOGIC
        endif
.END PATCH 1.4 - ADDED LOGIC
        if (PrtFlag = NO)
.This will never be the case if called from Program 1
                clear   taskname
;begin patch 1.9
                 call                getwinver   ;make sure we have osflag
;                                        Path    Exist,"c:\windows"
;                                        if      over
                 If                  (osflag = c1 | osflag =C5)
                 append  "!c:\winnt\system32\cmd.exe",taskname
;                                        else
                 elseif              (osflag = C3 | osflag =C4)
                 append  "!c:\command.com",taskname
                 elseif              (Osflag = C6)
                 append  "!c:\windows\system32\cmd.exe",taskname
;end patch 1.9
                endif
.START PATCH 1.88 REPLACED LOGIC
.                append  " /c copy f:\data\fax\hdrfile.prn /b + f:\data\fax\faxfile.prn /b f:\data\fax\fax",taskname
                append  " /c copy ",taskname
.START PATCH 2.07 REPLACED LOGIC
.                append  NTWKPATH4,taskname
.                append  "fax\hdrfile.prn /b + ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\faxfile.prn /b ",taskname
...................
                append  "C:\WORK\hdrfile.prn /b + ",taskname
                append  "C:\WORK\faxfile.prn /b ",taskname
.END PATCH 2.07 REPLACED LOGIC
                append  NTWKPATH4,taskname
                append  "fax\fax",taskname
.end PATCH 1.88 REPLACED LOGIC
                append  HOLDOWN,taskname
                append  ".prn /b",taskname
                reset   taskname
                execute taskname
                clear   taskname
;begin patch 1.9
                 call                getwinver   ;make sure we have osflag
;                                        Path    Exist,"c:\windows"
;                                        if      over
                 If                  (osflag = c1 | osflag =C5)
                 append  "!c:\winnt\system32\cmd.exe",taskname
;                                        else
                 elseif              (osflag = C3 | osflag =C4)
                 append  "!c:\command.com",taskname
                 elseif              (Osflag = C6)
                 append  "!c:\windows\system32\cmd.exe",taskname
;end patch 1.9
                endif
.START PATCH 1.88 REPLACED LOGIC
.                append  " /c copy f:\data\fax\fax",taskname
                append  " /c copy ",taskname
                append  NTWKPATH4,taskname
                append  "fax\fax",taskname
.end PATCH 1.88 REPLACED LOGIC
                append  HOLDOWN,taskname
                append  ".prn \\nts2\fax",taskname
                reset   taskname
                execute taskname
        endif
.START PATCH 1.4 - REPLACED LOGIC
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "!c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del g:\data\LCRfile.dat",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "!c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del g:\data\LCRfile.srt",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "!c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del f:\data\fax\faxfile.prn",taskname
.        reset   taskname
.        execute taskname
.        PAUSE   C2
.......................
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,output1,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,output2,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.
        if (PrintFlag = 0)
                move    "                                        ",APIFileName
                clear   APIFileName
.START PATCH 1.88 REPLACED LOGIC
.                pack    APIFileName,"f:\data\fax\faxfile.prn",hexzero
.START PATCH 2.07 REPLACED LOGIC
.                pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 2.07 REPLACED LOGIC
.end PATCH 1.88 REPLACED LOGIC
                call    DeleteFile
                if (APIResult = 0 | APIResult = hexeight)
                endif
                PAUSE   C2
.START PATCH 1.89 REPLACED LOGIC
.START PATCH 2.08 REPLACED LOGIC
.        elseif (PrintFlag = 2)
        elseif (PrintFlag = 2 | PrintFlag = 4)
.END PATCH 2.08 REPLACED LOGIC
                close   input2
                move    "                                        ",APIFileName
                clear   APIFileName
                pack    APIFileName,input2Name,hexzero
                call    DeleteFile
                if (APIResult = 0 | APIResult = hexeight)
                endif
                PAUSE   C2
.END PATCH 1.89 REPLACED LOGIC
        endif
.START PATCH 2.08 MOVED LOGIC TO BEGINNING OF PROGRAM
.        clear   taskname
.        clear   str55
.;begin patch 1.9
.                 call                getwinver   ;make sure we have osflag
.;                                        Path    Exist,"c:\windows"
.;                                        if      over
.                 If                  (osflag = c1 | osflag =C5)
.                 append  "!c:\winnt\system32\cmd.exe",taskname
.                 append  "!c:\winnt\system32\cmd.exe",str55
.;                                        else
.                 elseif              (osflag = C3 | osflag =C4)
.                 append  "!c:\command.com",taskname
.                 append  "!c:\command.com",str55
.                 elseif              (Osflag = C6)
.                 append  "!c:\windows\system32\cmd.exe",taskname
.                 append  "!c:\windows\system32\cmd.exe",Str55
.;end patch 1.9
.        endif
..START PATCH 2.06 REPLACED LOGIC
..        append  " /c del c:\work\*.pdf",taskname
..        append  " /c del c:\work\*.log",str55
.        append  " /c del c:\work\PDF\*.pdf",taskname
.        append  " /c del c:\work\PDF\*.log",str55
..END PATCH 2.06 REPLACED LOGIC
.        reset   taskname
.        reset   str55
.        execute taskname
.        execute str55
.END PATCH 2.08 MOVED LOGIC TO BEGINNING OF PROGRAM
.END PATCH 1.4 - REPLACED LOGIC
.begin patch 2.16
	call	pdf995auto0
.end patch 2.16
         shutdown

.START PATCH 1.4 - ADDED LOGIC
OrderCreatePDFFile
.        clear   taskname
.        clear   str25
.        append  userlogn,str25
.        append  "_",str25
.        append  HOLDLIST,str25
.        append  ".pdf",str25
.        reset   str25
.        pack    taskname,"!f:\apps\pcl2pdf\pcl2pdf32 c:\work\pdffile.prn ",FilePath,str25
.        execute taskname
..
.Patch2.01
..			move    "Here is your PDF File",SmtpSubject Subject
.   Set the text message that is send with the attachments
..        move    str55,SmtpTextMessage(1)   Array <Text message >
..        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
..        move    "NTS4",SmtpEmailServer                   Address of email serverc
..        clear   smtpemailaddress
..        append  userlogn,SmtpEmailAddress
..        append  "@nincal.com",SmtpEmailAddress
..        reset   smtpemailaddress
..        move    userlogn,SmtpUserName                                User name
.   Set the destinations of the email. Max 100 (Mime spec)
..        move    smtpemailaddress,SmtpDestinations(1,1)
..        move    userlogn,SmtpDestinations(1,2)
..        move    "1",SmtpDestIndexLast                          originators UserName
..        move    str55,SmtpAttachments(1,1)                     Attached file name
..        move    "C:\WORK",SmtpAttachments(1,2)           Path to attached file name
..        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
..        move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
..        clear   SmtpLogFile                                         'Clear' disables the LogFile
..        move    "1",SmtpProgress                                    Enable progress bars
..        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..        if not equal
..                pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
..                        "Status Code ",SmtpStatus," - ",SmtpStatusText
..                move    "PDF File not found",SmtpSubject Subject
..                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
..                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.                Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
..        else
.                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
..        endif

.Patch2.01EndCommentOut
.START PATCH 2.08 REPLACED LOGIC
.		PAUSE	  120
.		clear   TASKNAME
.		pack    TASKNAME,"f:\apps\winbatch\butil job=WINEMAIL infile=",str55," b=",userlogn
.		Execute TASKNAME
	if (PrintFlag = 4)
.File creation on harddrive!
		call	EmailUserPDF
		return
	endif
	PAUSE	  C2
.begin patch 2.14
	clear   TASKNAME
	pack    TASKNAME,"f:\apps\winbatch\butil job=WINEMAIL infile=",str55," b=",userlogn
	Execute TASKNAME
.begin New code

.	Clear	Mailbody
.	append	"This is a message from	the Galley Report Program",Mailbody
.	Append  	CRLF,MailBody					
.	Append	"Your PDF file was created!",Mailbody
.	Append  	CRLF,MailBody					
.	append	"Location:  c:\work\pdf\ ",mailbody
.	Append  	CRLF,MailBody					
.	append	"filename:  ",mailbody
.	append	str55,mailbody
.	Append  	CRLF,MailBody					
.	Reset 	MailBody
.		
.	pack	mailattach,"c:\work\pdf\",str55	
.	pack	Mailto,userlogn,"@nincal.com"
.	pack	Mailfrom,"Creques@nincal.com"
.	move	"Your PDF file was created ",Mailsubjct
.	PAUSE	"120"
.	pack	 APIFileName,"c:\work\",str10,".fax",HexZero
.
.	loop
.	call	FindFirstFile
.	if (APIResult =	0 | APIResult =	hexeight)
..If file not ready then do not attempt
.	clock	timestamp,timestamp2
.	move	timestamp2,time2
.	sub	time1,time2,time3
.		if (time3 > 3000) ..30 Seconds Maximum
.		endif
.	endif
.	until	(apiresult <> 0	& APIRESULT <> Hexeight)
.       	repeat
.	
.	call	SendMail
.end patch 2.14
.END PATCH 2.08 REPLACED LOGIC
.End Patch2.01
.Clean up afterwards
        move    "                                        ",APIFileName
        clear   APIFileName
.START PATCH 2.06 REPLACED LOGIC
.        pack    APIFileName,"C:\WORK\",str55,hexzero
        pack    APIFileName,"C:\WORK\PDF\",str55,hexzero	."
.END PATCH 2.06 REPLACED LOGIC
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
        return
.END PATCH 1.4 - ADDED LOGIC

OrderBadSample
        move    "This is a Error e-mail from LCR Galley Report",SmtpSubject Subject
.   Set the text message that is send with the attachments
        move    "You have included a Bad Sample!!",SmtpTextMessage(1)
        clear   taskname
.START PATCH 2.05 REPLACED LOGIC
.        unpack  str7,str4,str3
.        pack    taskname,"LR: ",OLRN,B1,"MAILER: ",str4,B1,"SAMPLE: ",str3
        unpack  str9,str6,str3
        pack    taskname,"LR: ",OLRN,B1,"MAILER: ",str6,B1,"SAMPLE: ",str3
.END PATCH 2.05 REPLACED LOGIC
        move    taskname,SmtpTextMessage(2)   Array <Text message >
        move    "2",SmtpTextIndexLast                               Index to last entry in TextMessage array

        clear   taskname
        move    "NTS4",SmtpEmailServer                   Address of email serverc
        clear   smtpemailaddress
        move    str24 to SmtpEmailAddress
        move    str24,str7
        move    str7,SmtpUserName                                User name
.START PATCH 1.75 REPLACED LOGIC
.        move    OCNT,SmtpUserFullName              User Full Name
        move    CNTNAME,SmtpUserFullName              User Full Name
.END PATCH 1.75 REPLACED LOGIC
.   Set the destinations of the email. Max 100 (Mime spec)

        move   "ComputerRequest@nincal.com",SmtpDestinations(1,1)
        move    "Computer Request",SmtpDestinations(1,2)
.        move    "InformationServices@NINCAL.com",SmtpDestinations(1,1)            Destination e-mail addr
        move    smtpemailaddress to SmtpDestinations(2,1)                  .send a copy to the originator
.        move    "Information Services",SmtpDestinations(1,2)         Destination UserName
.START PATCH 1.75 REPLACED LOGIC
.        move    OCNT,SmtpDestinations(2,2)                          originators UserName
        move    CNTNAME,SmtpDestinations(2,2)                          originators UserName
.END PATCH 1.75 REPLACED LOGIC
        move    "2",SmtpDestIndexLast                               Index to last entry in Dest array
        move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
        clear   SmtpLogFile                                         'Clear' disables the LogFile
        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
        if not equal
                pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                        "Status Code ",SmtpStatus," - ",SmtpStatusText
                move    "Bad Sample",SmtpSubject Subject
                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.                Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
        else
.                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
        endif
        return

OrderOpenFile
.Print newly sorted file
        move    OWNFAX,faxnum
        match   "0000000000",faxnum
        if      equal
                move    YES,PrtFlag             .PRINT IT
        else
                type    faxnum
                if not equal
                        move    YES,PrtFlag     .PRINT IT
                else
                        move    NO,PrtFlag      .FAX IT
                        count   N2,faxnum
                        compare C10,N2
                        if equal
                                move    C1,LONGDIST
                                unpack  faxnum,str3,str7
.START PATCH 1.8 REPLACED LOGIC
.                                match   "415",str3
                                match   "510",str3
.END PATCH 1.8 REPLACED LOGIC
                                if equal
                                        move    str7,faxnum
                                        clear   LONGDIST
                                else
                                        match   B3,str3
                                        if equal
                                                move    str7,faxnum
                                                clear   LONGDIST
                                        endif
                                endif
                        endif
                endif
        endif
.START PATCH 1.4 - ADDED LOGIC
.Prevent faxes from being run if you are calling this program dynamically through Program 1
        if (PrintFlag <> C0)
                move    YES,PrtFlag
        endif
.END PATCH 1.4 - ADDED LOGIC
.
.START NEW TEST - PREVENT FAXES FROM BEING CREATED
.        MOVE    YES,PRTFLAG
.END NEW TEST
.
        if (PrtFlag = YES)
.START PATCH 1.4 - REPLACED LOGIC
..Printer of your choice
..        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.        if (osflag = c2)         .nt
.                PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
.        elseif (osflag = c1)         .win 95 98
.                PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.        else   .(osflag = c0)         .Don't know prompt for printer
.                PRTOPEN prfile,"@","FAXFILE.PRN"
.        endif
..        PRTOPEN prfile,"@","FAXFILE.PRN"
..........................
.Printer of your choice
                if (PrintFlag = C0 | PrintFlag = 3)     .Laser2 = Default
.                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
;begin patch 1.9
;                        if (osflag = c2)         .nt
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
;                        elseif (osflag = c1)         .win 95 98
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"Laser2","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
.START PATCH 1.85 REPLACED LOGIC
.                                PRTOPEN prfile,"@","FAXFILE.PRN"
                                PRTOPEN prfile,"-","FAXFILE.PRN"
.END PATCH 1.85 REPLACED LOGIC
                        endif
                elseif (PrintFlag = 1)  .Laser3
;                        if (osflag = c2 | osflag = C5)         .nt
                        if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
                                PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","FAXFILE.PRN"
;                        elseif (osflag = c1)         .win 95 98
                        elseif (osflag = c3 | osflag =c4)         .win 95 98
                                PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
;end patch 1.9
.START PATCH 1.85 REPLACED LOGIC
.                                PRTOPEN prfile,"@","FAXFILE.PRN"
                                PRTOPEN prfile,"-","FAXFILE.PRN"
.END PATCH 1.85 REPLACED LOGIC
                        endif
.START PATCH 2.08 REPLACED LOGIC
.                elseif (PrintFlag = 2)  .PDF
                elseif (PrintFlag = 2 | PrintFlag = 4)  .PDF
.begin patch 2.16
	call	pdf995auto
.end patch 2.16
 
.END PATCH 2.08 REPLACED LOGIC
.                        PRTOPEN prfile,"faxfile2","pdffile.prn"
.START PATCH 2.11 REPLACED LOGIC
.                        clear   str25
.                        append  OLON,str25
.                        append  "_",str25
.                        append  OLNUM,str25
..                        append  ".pdf",str25
.                        reset   str25
..START PATCH 2.06 REPLACED LOGIC
..                        PRTOPEN prfile,"Acrobat Distiller",str25
..                        pack    str55,str25,".pdf"
.                        PRTOPEN prfile,"PDF995",str25
.                        pack    str55,str25,".pdf"
..END PATCH 2.06 REPLACED LOGIC
......................................
			clear   str45
			append  OLON,str45
			append  "_",str45
			append  OLNUM,str45
			if (PrintFlag = 4)
				clock	timestamp,timestamp
				append  "_",str45
				append	timestamp,str45
			endif
			reset   str45
			PRTOPEN prfile,"PDF995",str45
			pack    str55,str45,".pdf"
.END PATCH 2.11 REPLACED LOGIC
                endif
                ADD     C1,COUNTR2
.END PATCH 1.4 - REPLACED LOGIC
        else
.START TEST - PREVENT FILES FROM ACTUALLY BEING SENT OUT TO CLIENTS
.                clear   LONGDIST
.                MOVE    "4337796",FAXNUM
.END TEST
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
.START PATCH 1.82 ADDED LOGIC
                move    "                                        ",APIFileName
                clear   APIFileName
.START PATCH 1.88 REPLACED LOGIC
.                pack    APIFileName,"f:\data\fax\hdrfile.prn",hexzero
.START PATCH 2.07 REPLACED LOGIC
.                pack    APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
                pack    APIFileName,"C:\WORK\hdrfile.prn",hexzero
.END PATCH 2.07 REPLACED LOGIC
.END PATCH 1.88 REPLACED LOGIC
                call    DeleteFile
                if (APIResult = 0 | APIResult = hexeight)
                endif
.END PATCH 1.82 ADDED LOGIC
.START PATCH 1.92 REPLACED LOGIC
.                SPLOPEN "HDRFILE/PRN:P"
.START PATCH 2.07 REPLACED LOGIC
.                SPLOPEN "\\nts0\d\DATA\FAX\HDRFILE.PRN"
                SPLOPEN "C:\WORK\HDRFILE.PRN"
.END PATCH 2.07 REPLACED LOGIC
.END PATCH 1.92 REPLACED LOGIC
.START PATCH 1.75 REPLACED LOGIC
.                print   "^[D",longdist,faxnum,"^[N",OWNOCPY:
.                        "^[S",OCNT,B2,str15," ^]"
                print   "^[D",longdist,faxnum,"^[N",OWNOCPY:
                        "^[S",CNTNAME,B2,CNTPHONE," ^]"
.END PATCH 1.75 REPLACED LOGIC
                SPLCLOSE
                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
                ADD     C1,COUNTR
        endif
.START PATCH 1.4 - REPLACED LOGIC
.        DISPLAY *P10:12,*EL,"FAX   COUNT ",COUNTR
.        DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR2
        if (PrintFlag = 0)
                DISPLAY *P10:12,*EL,"FAX   COUNT ",COUNTR
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR2
        endif
.END PATCH 1.4 - REPLACED LOGIC
        RETURN

OrderReadOtherFiles
.Open other files to retrieve appropriate information
.Mailer File
        rep     zfill,OMLRNUM
        pack    MKEY,OMLRNUM,"000"      .Master Record
        move    C3,NMLRLOCK
        move    "Driver-NMLRKEY,1rst",Location
        call    NMLRKEY
        if over
                pack   MCOMP,"UNKNOWN",B55
        endif
.Owner File
        move    OLON,NOWNFLD
        rep     zfill,NOWNFLD
        move    "Driver-NOWNKEY,1rst",Location
        call    NOWNKEY
        if over

        endif
.Special Instructions
        move    OLRN,NSPEFLD
        rep     zfill,NSPEFLD
        move    C3,NSPELOCK
        move    "Driver-NSPEKEY,1rst",Location
        call    NSPEKEY
        call    Trim Using DESC001
.Contact
.START PATCH 1.75 REPLACED LOGIC
.        move    OCO2CODE,N2
.        pack    str45,B55
.        clear   str15
.        clear   OCNT
.        load    str45 FROM N2 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6,OCNT7:
.                OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15,OCNT16,OCNT17
.        scan    "(",str45
.        if equal
.                movefptr str45,result
.                append  str45,str15
.                reset   str15
.                scan    "()",str15
.                if equal
.                        clear   str15
.                endif
.                sub     C1,result
.                reset   str45
.                setlptr str45,result
.                append  str45,OCNT
.        endif
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
.END PATCH 1.75 REPLACED LOGIC
.Data Card Universe
        clear   UNIVERSE
.START PATCH 2.09 REPLACED LOGIC - TEMPORARY PATCH
.        move    MKEY,NXRFFLD2
	pack	COMPFLD3,OMLRNUM
	move	"COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
        move    COMPNUM,NXRFFLD2
.END PATCH 2.09 REPLACED LOGIC - TEMPORARY PATCH
        rep     zfill,NXRFFLD2
        move    C2,NXRFPATH
        move    "Driver-NXRFKEY",Location
        call    NXRFKEY
        if not over
                move    NXRFLIST,NDATFLD
                move    C0,UNIVERSE
                move    C1,NDATPATH
                move    "Driver-NDATKEY",Location
                call    NDATKEY
                if over
                       move     C0,UNIVERSE
                endif
        endif
.Sample File
.START PATCH 2.05 REPLACED LOGIC - TEMPORARY PATCH
.        pack    NSMPFLD,OMLRNUM,OSAMCDE
	move	"Driver-COMPKEY3",Location
	pack	COMPFLD3,OMLRNUM
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	if over
		clear	COMPNUM
	endif
	pack	NSMPFLD,COMPNUM,OSAMCDE
.END PATCH 2.05 REPLACED LOGIC - TEMPORARY PATCH
        move    "Driver-NSMPKEY",Location
        clear   NSMPDES1
        call    NSMPKEY
        if over
                pack    NSMPDES1,"Sample Not Found!"
        else
                if (NSMPDES1 = "")
                pack    NSMPDES1,"Description Not Found"
                endif
        endif
.START PATCH 1.2 - ADDED LOGIC
.Offer file
        bump    OODNUM,4
        pack    NOFRFLD,OMLRNUM,OODNUM
        rep     zfill in NOFRFLD
        move    "O.LoadOffer-NOFRKEY",Location
        call    NOFRKEY
        if over
                clear   OFDESC
        endif
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 2.0A ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 2.0A ADDED LOGIC
        return

OrderReadPendFiles
.NINORD5 File
        move    OLRN,NORD5FLD
        rep     zfill,NORD5FLD
        call    NORD5KEY
        if over
                move    "No Status Found!",NPNDDESC
        else
.NINPND File
                pack    NPNDFLD,OSTAT,NORD5STAT
                rep     zfill,NPNDFLD
                move    "Driver-NPDNKEY",Location
                call    NPNDKEY
                if over
                        move    "No Status Found!",NPNDDESC
                endif
        endif
        return

.Print Heading
OrderPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon;
.START PATCH 1.8 - REPLACED LOGIC
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=1:15000:1:8750:PICT1
..        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9400:14700:100:8750:PICT2
.        move    "300",row
.        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page
.        add     sixlpi,row
.        add     sixlpi,row
.        add     eightlpi,row
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 2.04 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.begin patch 2.15
	if	(CompExcl = "P")
	prtpage	PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=500:343,*font=fontO7,"1300 Clay St. 11th Floor":
		*p=400:443,"Oakland, CA 94612-1429":
		*p=335:543,"415-945-9450 ","�"," Fax 415-945-9451":
		*p=335:643,"A Division of Names in the News",*font=font2
	else
	prtpage	prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
	endif
.end patch 2.15
.END PATCH 2.04 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 2.04 REPLACED LOGIC
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 � 415-989-3350 � Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
.END PATCH 2.04 REPLACED LOGIC
.END PATCH 1.8 - REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
.START PATCH 1.97 REPLACED LOGIC
.        prtpage prfile;*p800:row,OWNLONM;
        prtpage prfile;*p900:row,OWNLONM;
.END PATCH 1.97 REPLACED LOGIC
.START PATCH 1.98 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"FROM:";
        prtpage prfile;*p5200:row,"FROM:";
.END PATCH 1.98 REPLACED LOGIC
.START PATCH 1.75 REPLACED LOGIC
.        prtpage prfile;*p6000:row,OCNT;
.START PATCH 1.98 REPLACED LOGIC
.        prtpage prfile;*p6000:row,CNTNAME;
        prtpage prfile;*p5700:row,CNTNAME;
.END PATCH 1.98 REPLACED LOGIC
.END PATCH 1.75 REPLACED LOGIC
        add     eightlpi,row
.START PATCH 1.97 REPLACED LOGIC
.        prtpage prfile;*p800:row,COMPHOLD;
        prtpage prfile;*p900:row,COMPHOLD;
.END PATCH 1.97 REPLACED LOGIC
.START PATCH 1.75 REPLACED LOGIC
.        prtpage prfile;*p6000:row,str15;
.START PATCH 1.98 REPLACED LOGIC
.        prtpage prfile;*p6000:row,CNTPHONE;
        prtpage prfile;*p5700:row,CNTPHONE;
.END PATCH 1.98 REPLACED LOGIC
.END PATCH 1.75 REPLACED LOGIC
        add     eightlpi,row
.START PATCH 1.97 REPLACED LOGIC
.        prtpage prfile;*p800:row,OWNLOSA;
        prtpage prfile;*p900:row,OWNLOSA;
.END PATCH 1.97 REPLACED LOGIC
.START PATCH 1.75 REPLACED LOGIC
.        scan    "BILLING",OCNT
.        goto cntexit if equal
.        move    OCNT,str1
.cntloopy
.        bump    OCNT,1
.        cmatch  B1,OCNT
.        goto    cntloopy if not equal
.        goto    cntexit if eos
.        bump    OCNT,1
.        move    OCNT,str6
.        clear   str24
.        pack    str24,str1,str6,"@NINCAL.COM"
.cntexit reset   OCNT
...
.START PATCH 1.98 REPLACED LOGIC
.        scan    "BILLING",CNTNAME
.        goto cntexit if equal
.        move    CNTNAME,str1
.cntloopy
.        bump    CNTNAME,1
.        cmatch  B1,CNTNAME
.        goto    cntloopy if not equal
.        goto    cntexit if eos
.        bump    CNTNAME,1
..START PATCH 1.81 REPLACED LOGIC
..        move    CNTNAME,str6
.        move    CNTNAME,str7
.        call    RemoveChar using str7,B1
.        move    str7,str6
..END PATCH 1.81 REPLACED LOGIC
.        clear   str24
.        pack    str24,str1,str6,"@NINCAL.COM"
.cntexit reset   CNTNAME
..END PATCH 1.75 REPLACED LOGIC
.        prtpage prfile;*p6000:row,str24;
.........................................
	clear	str60
	call	Trim using CNTNAME
	if (CNTNAME <> "" & CNTNAME <> "BILLING")
		call	RemoveChar using CNTNAME,B1
		pack	str60,CNTNAME,"@nincal.com"
	endif
	prtpage prfile;*p5700:row,str60;
.END PATCH 1.98 REPLACED LOGIC
        add     eightlpi,row
        call    Trim using OWNLOCTY
        if (OWNLOCTY <> "")
                pack    taskname,OWNLOCTY,COMMA,OWNLOS,B1,OWNLOZC
.START PATCH 1.97 REPLACED LOGIC
.                prtpage prfile;*p800:row,taskname;
                prtpage prfile;*p900:row,taskname;
.END PATCH 1.97 REPLACED LOGIC
        endif
.START PATCH 1.2 - ADDED LOGIC
.START PATCH 1.98 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"FAX:";
        prtpage prfile;*p5200:row,"FAX:";
.END PATCH 1.98 REPLACED LOGIC
.START PATCH 1.94 REPLACED LOGIC
.        prtpage prfile;*p6000:row,"(415) 433-7796";
;patch1.96
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
;patch1.96
.START PATCH 1.98 REPLACED LOGIC
.	if (CNTTEAM = "05")	.List Management
.	        prtpage prfile;*p6000:row,"(510) 628-8313";
.	else
.	        prtpage prfile;*p6000:row,"(415) 433-7796";
.	endif
.begin patch 2.15
	IF	(CompExcl <> "P")
		if (CNTTEAM = "05")	.List Management
	        		prtpage prfile;*p5700:row,"(510) 628-8313";
		else
	        		prtpage prfile;*p5700:row,"(415) 433-7796";
	        	endif
	Elseif	(CompExcl = "P")        	
        		prtpage prfile;*p5700:row,"(415) 945-9451";
	endif
.End patch 2.15
.END PATCH 1.98 REPLACED LOGIC
.END PATCH 1.94 REPLACED LOGIC
.END PATCH 1.2 - ADDED LOGIC
        add     eightlpi,row
.START PATCH 1.97 REPLACED LOGIC
.        call    Trim using OWNFAX
.        if (OWNFAX <> "")
.                unpack  OWNFAX,str3,str2,str1,str4
.                pack    taskname,"(",str3,") ",str2,str1,"-",str4
.                prtpage prfile;*p800:row,taskname;
.        endif
..START PATCH 1.2 - ADDED LOGIC
.        add     eightlpi,row
..END PATCH 1.2 - ADDED LOGIC
...............
	call    Trim using OWNTELE
	if (OWNTELE <> "")
		unpack  OWNTELE,str3,str2,str1,str4
		pack    taskname,"(",str3,") ",str2,str1,"-",str4
		prtpage prfile;*p900:row,taskname;
	endif
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"FAX:";
	call    Trim using OWNFAX
	if (OWNFAX <> "")
		unpack  OWNFAX,str3,str2,str1,str4
		pack    taskname,"(",str3,") ",str2,str1,"-",str4
		prtpage prfile;*p900:row,taskname;
	endif
.END PATCH 1.97 REPLACED LOGIC
.START PATCH 1.98 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"DATE:  ";
.        prtpage prfile;*p6000:row,newdate1;
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,newdate1;
.END PATCH 1.98 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font5,*boldon,"REQUEST FOR CLEARANCE",*boldoff;
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p3000:row,*font=font1,*boldon,"REQUEST FOR CLEARANCE",*boldoff;
.END PATCH 1.6 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.2 - REMMED LOGIC
.        add     eightlpi,row
.END PATCH 1.2 - REMMED LOGIC
.        add     eightlpi,row
        return

OrderListHeader
        prtpage prfile;*pcolumn:row,*font=font5,"List:  ",O1DES;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN##/";

.START PATCH 1.6 REPLACED LOGIC
.START PATCH 1.2 - ADDED LOGIC
        prtpage prfile;*pcolumn1:row,"Status";
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 1.83 - REMOVED LOGIC
.        prtpage prfile;*pcolumn3:row,"Mailer";
        prtpage prfile;*pcolumn4:row,"Mail Date/";
.END PATCH 1.83 - REMOVED LOGIC
        prtpage prfile;*pcolumn6:row,"Sample";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Mailer/";
.START PATCH 1.83 - REPLACED LOGIC
.        prtpage prfile;*pcolumn2:row,"Mail Date";
.        prtpage prfile;*pcolumn3:row,"Universe";
.END PATCH 1.83 - REPLACED LOGIC
        prtpage prfile;*pcolumn4:row,"Quantity";
        prtpage prfile;*pcolumn5:row,"Reco.";
        prtpage prfile;*pcolumn6:row,"Attached/";
        prtpage prfile;*pcolumn7:row,"Answer";

        add     eightlpi,row
.START PATCH 1.2 - REPLACED LOGIC
.        prtpage prfile;*pcolumn:row,"Status";
        prtpage prfile;*pcolumn:row,"Offer/";
.END PATCH 1.2 - REPLACED LOGIC
        prtpage prfile;*pcolumn6:row,"Description";
..................
.        prtpage prfile;*pcolumn1:row,"Mailer";
.        prtpage prfile;*pcolumn6:row,"Sample";
.        add     eightlpi,row
.        prtpage prfile;*pcolumn:row,"Status/";
.        prtpage prfile;*pcolumn2:row,"Mail Date";
.        prtpage prfile;*pcolumn3:row,"Mailer";
.        prtpage prfile;*pcolumn4:row,"Quantity";
.        prtpage prfile;*pcolumn5:row,"Reco.";
.        prtpage prfile;*pcolumn6:row,"Attached/";
.        add     eightlpi,row
..START PATCH 1.2 - REPLACED LOGIC
..        prtpage prfile;*pcolumn:row,"Status";
.        prtpage prfile;*pcolumn:row,"Offer/";
.        prtpage prfile;*pcolumn3:row,"Universe";
..END PATCH 1.2 - REPLACED LOGIC
.        prtpage prfile;*pcolumn6:row,"Description";
..END PATCH 1.6 REPLACED LOGIC
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

OrderPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
.START PATCH 1.83 REPLACED LOGIC
.        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
.START PATCH 2.04 REPLACED LOGIC
.        if (row >= 8372)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
.END PATCH 2.04 REPLACED LOGIC
.END PATCH 1.83 REPLACED LOGIC
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderListHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN;
.START PATCH 1.6 REPLACED LOGIC
.START PATCH 1.2 - ADDED LOGIC - MOVED
.START PATCH 1.81 REPLACED LOGIC
.        if (OSTAT = "l")
        if (OSTAT = "l" | OSTAT = "z")
.END PATCH 1.81 REPLACED LOGIC
                if (NORD5STAT = "02" OR NORD5STAT = "03")
                        prtpage prfile;*boldon;
                endif
                prtpage prfile;*pcolumn1:row,NPNDDESC,*boldoff;
        else
                prtpage prfile;*pcolumn1:row,"Pending Order",*boldoff;
        endif
.END PATCH 1.2 - ADDED LOGIC - MOVED
.START PATCH 1.83 - REPLACED LOGIC
.START PATCH 1.91 MOVED LOGIC - MOVED FROM BELOW
        call    TRIM using OMDTEM
        count   N2,OMDTEM
.END PATCH 1.91 MOVED LOGIC
        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                prtpage prfile;*pcolumn4:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                prtpage prfile;*pcolumn4:row,"As Soon As Possible";
        elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
                prtpage prfile;*pcolumn4:row,"See Special Instructions";
        endif
.END PATCH 1.83 - REPLACED LOGIC
        move    row,N10
        move    row,result
        add     eightlpi,N10
        add     eightlpi,N10
        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6000;


..TESTING FOR WATERMARK
.        move    row,result3
.        move    row,result2
.        add     eightlpi,result2
.        add     eightlpi,result2
.        add     eightlpi,result2
.        add     eightlpi,result2
.        add     eightlpi,result2
.        add     eightlpi,result2



        move    column6,N9
        add     "100",N9
        move    row,howmany
        add     "70",howmany
        if (OSCODE = "1")
                prtpage prfile;*pN9:howmany,*font=font1,"X",*font=font2;
        endif
        prtpage prfile;*RECT=row:N10:column7:7800;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,MCOMP;
.START PATCH 1.91 MOVED LOGIC - MOVED ABOVE
.        call    TRIM using OMDTEM
.        count   N2,OMDTEM
.END PATCH 1.91 MOVED LOGIC
.START PATCH 1.83 - REPLACED LOGIC
.        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
.                prtpage prfile;*pcolumn2:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
.        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
.                prtpage prfile;*pcolumn2:row,"As Soon As Possible";
.        elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
.                prtpage prfile;*pcolumn2:row,"See Special Instructions";
.        endif
.        move    EditMask,EditQuan
.        move    C0,N9
.        move    UNIVERSE,N9
.        edit    N9,EditQuan
.        prtpage prfile;*pcolumn3:row,EditQuan;
.END PATCH 1.83 - REPLACED LOGIC
.START PATCH 1.93 REPLACED LOGIC
.        if (OELCODE = "1" OR OELCODE = "3")
.                move    "All",EditQuan
.        else
.                move    EditMask,EditQuan
.                move    C0,N9
.                move    OQTY,N9
.                edit    N9,EditQuan
.        endif
.        prtpage prfile;*pcolumn4:row,EditQuan;
	move    C0,N9
	move    OQTY,N9
	if (OELCODE = "1" OR OELCODE = "3")
.START PATCH 2.02 REPLACED LOGIC
..START PATCH 1.95 REPLACED LOGIC
..		if (N9 > 0)
.		if (N9 > 0 & (OCO2CODE = "" | OCO2CODE = "  "))
..END PATCH 1.95 REPLACED LOGIC
.			move    EditMask,str11
.			edit    N9,str11
.			call	Trim using str11
.			pack	EditQuan,"ALL/ ",str11
.			add     eightlpi,row,N9
.			sub	"500",column4,N10
.		        prtpage prfile;*pN10:row,EditQuan;
.		        prtpage prfile;*pN10:N9,"Please advise actual quantity.";
.		else
.			move    "All",EditQuan
.		        prtpage prfile;*pcolumn4:row,EditQuan;
.		endif
.	else
.		move    EditMask,EditQuan
.		edit    N9,EditQuan
.	        prtpage prfile;*pcolumn4:row,EditQuan;
.	endif
		if (N9 > 0)
			move    EditMask,str11
			edit    N9,str11
			call	Trim using str11
			pack	EditQuan,str11,"/ALL"
			add     eightlpi,row,N9
			sub	"500",column4,N10
		        prtpage prfile;*pN10:row,EditQuan;
		else
			move    "All",EditQuan
		        prtpage prfile;*pcolumn4:row,EditQuan;
		endif
	else
		move    EditMask,EditQuan
		edit    N9,EditQuan
	        prtpage prfile;*pcolumn4:row,EditQuan;
	endif
.END PATCH 2.02 REPLACED LOGIC
.END PATCH 1.93 REPLACED LOGIC
        clear   str9
.START PATCH 1.1 - REPLACED LOGIC
.        if (OTOCODE = "R")
        if (ORENT = "1")
.END PATCH 1.1 - REPLACED LOGIC
                if (OELCODE = "2" OR OELCODE = "3")
                        append  "RENT/EXC",str9
                else
                        append  "RENTAL",str9
                endif
        else
                append  "EXCHANGE",str9
        endif
        reset   str9
        prtpage prfile;*pcolumn5:row,str9;
................
.        prtpage prfile;*pcolumn1:row,MCOMP;
.        move    row,N10
.        move    row,result
.        add     eightlpi,N10
.        add     eightlpi,N10
.        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6200;
.        move    column6,N9
.        add     "100",N9
.        move    row,howmany
.        add     "70",howmany
.        if (OSCODE = "1")
.                prtpage prfile;*pN9:howmany,*font=font1,"X",*font=font2;
.        endif
.        prtpage prfile;*RECT=row:N10:column7:7800;
.        add     eightlpi,row
.        if (OSTAT = "l")
.                if (NORD5STAT = "02" OR NORD5STAT = "03")
.                        prtpage prfile;*boldon;
.                endif
.                prtpage prfile;*pcolumn:row,NPNDDESC,*boldoff;
.        else
.                prtpage prfile;*pcolumn:row,"Pending Order",*boldoff;
.        endif
.        call    TRIM using OMDTEM
.        count   N2,OMDTEM
.        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
.                prtpage prfile;*pcolumn2:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
.        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
.                prtpage prfile;*pcolumn2:row,"As Soon As Possible";
.        elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
.                prtpage prfile;*pcolumn2:row,"See Special Instructions";
.        endif
.        move    EditMask,EditQuan
.        move    C0,N9
.        move    UNIVERSE,N9
.        edit    N9,EditQuan
.        prtpage prfile;*pcolumn3:row,EditQuan;
.        if (OELCODE = "1" OR OELCODE = "3")
.                move    "All",EditQuan
.        else
.                move    EditMask,EditQuan
.                move    C0,N9
.                move    OQTY,N9
.                edit    N9,EditQuan
.        endif
.        prtpage prfile;*pcolumn4:row,EditQuan;
.        clear   str9
.        if (ORENT = "1")
.               if (OELCODE = "2" OR OELCODE = "3")
.                        append  "RENT/EXC",str9
.                else
.                        append  "RENTAL",str9
.                endif
.        else
.                append  "EXCHANGE",str9
.        endif
.        reset   str9
.        prtpage prfile;*pcolumn5:row,str9;
.END PATCH 1.6 REPLACED LOGIC
.Note:  The Box holding "Answer" is printed after the XSTAT
.       This is done so that complete box is printed.  Following vars must, therefore, must remain.
        add     eightlpi,row
.START PATCH 1.2 - REPLACED LOGIC
.        if (OSTAT = "l")
.                if (NORD5STAT = "02" OR NORD5STAT = "03")
.                        prtpage prfile;*boldon;
.                endif
.                prtpage prfile;*pcolumn:row,NPNDDESC,*boldoff;
.        else
.                prtpage prfile;*pcolumn:row,"Pending Order",*boldoff;
.        endif
        prtpage prfile;*pcolumn:row,OFDESC;
.END PATCH 1.2 - REPLACED LOGIC
.START PATCH 1.83 - REPLACED LOGIC
.        prtpage prfile;*pcolumn2:row,DESC001;
.END PATCH 1.83 - REPLACED LOGIC
        if (OSCODE = "1")
                prtpage prfile;*pcolumn6:row,NSMPDES1;
        endif
        call    PrintSpecialInstructions


..TESTING FOR WATERMARK
.        PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=result3:result2:COLUMN1:column6:pict1


        add     eightlpi,row
        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
.START PATCH 1.83 - REPLACED LOGIC
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,DESC001;
.END PATCH 1.83 - REPLACED LOGIC
.START PATCH 2.0A REPLACED LOGIC
.        call    TRIM using O2DES
.        if (O2DES <> "")
.                pack    DESC003,"Select:  ",O2DES,".  ",DESC002
.        else
.                pack    DESC003,DESC002
.        endif
        call    TRIM using NSEL2NAME
        if (NSEL2NAME <> "")
                pack    DESC003,"Select:  ",NSEL2NAME,".  ",DESC002
        else
                pack    DESC003,DESC002
        endif
.END PATCH 2.0A REPLACED LOGIC
        call    TRIM using DESC003
        if (DESC003 <> "")
                pack    str2,carr,B1
                rep     str2,DESC003
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC003,C1
                        call    Trim using line1
                        if (line1 <> carr AND line1 <> "")
                                move    YES,str1
                                add     eightlpi,row
                                prtpage prfile;*pcolumn:row,*font=font4,line1,*font=font2;
                        endif
                        add     C1,howmany
                        until   (howmany >= 7)
                repeat
                if (str1 = YES)
                        add     eightlpi,row
                endif
        endif
        add     N9,row
        return

PrintSamplePage
        move    C1,SMPIndex
        loop
.START PATCH 2.05 REPLACED LOGIC
.                clear   str7
.                move    SMPArray(SMPIndex),str7
.                call    Trim using str7
.                if (str7 = "")
.                        break
.                endif
                clear   str9
                move    SMPArray(SMPIndex),str9
                call    Trim using str9
                if (str9 = "")
                        break
                endif
.END PATCH 2.05 REPLACED LOGIC
                if (SMPIndex >= 50)
                        break
                endif
.START PATCH 2.05 REPLACED LOGIC
.                pack    DCXFile,DCXPath,str7,DCXExt
                pack    DCXFile,DCXPath,str9,DCXExt
.END PATCH 2.05 REPLACED LOGIC
.Print and Display First Page
                clear   N9
.START PATCH 1.99 REPLACED LOGIC
.                CREATE  PICT3=70:700:100:550:
.                        DCXFile,BORDER,SCROLLBAR,AUTOZOOM
.                PICT3.GetPageCount GIVING N9
.                prtpage prfile;*P2:2,*NEWPAGE,*PICTvis=6:10000:3:10000:PICT3;
                CREATE  PICT3=70:700:100:550:
                        DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0
                PICT3.GetPageCount GIVING N9
                prtpage prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
.END PATCH 1.99 REPLACED LOGIC
.Print and Display Additional Pages
                if (N9 > C1)    .Only Enter loop if more than one page
                        clear   N8
                        move    C1,N8   .Start with SECOND PAGE as first page already printed
                        loop
                                add     C1,N8
                                until (N8 > N9)
.START PATCH 1.99 REPLACED LOGIC
.                                CREATE  PICT3=70:700:100:550:
.                                        DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
.                                prtpage prfile;*P2:2,*NEWPAGE,*PICTvis=6:10000:3:10000:PICT3;
                                CREATE  PICT3=70:700:100:550:
                                        DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0,PAGE=N8
                                prtpage prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
.END PATCH 1.99 REPLACED LOGIC
                        repeat
                endif
                add     C1,SMPIndex
        repeat
printstop
        DESTROY PICT3
        return
trapit
        return
.START PATCH 2.08 ADDED LOGIC
EmailUserPDF
.This Routine only called if Program is run locally and is using PDF995
.begin patch 2.14
	move	"This is a message from	the Galley Report Program",SmtpSubject Subject
	move	"Your PDF file was created!",SmtpTextMessage(1)
	move	"Location:  c:\work\pdf\ ",SmtpTextMessage(2)
	pack	SmtpTextMessage(3),"Filename:  ",str55
	pack	SmtpTextMessage(4),"FILE:///c:\work\pdf\",str55
	move	"4",SmtpTextIndexLast
	move	"NTS4",SmtpEmailServer
	pack	SmtpEmailAddress,userlogn,"@nincal.com"
	move	userlogn,SmtpUserName
	move	userlogn,SmtpUserFullName
	move	smtpemailaddress,SmtpDestinations(1,1)
	move	"1",SmtpDestIndexLast
	move	"0",SmtpAttIndexLast
	clear	SmtpLogFile
	call	SmtpSend
.begin New code
	Clear	Mailbody
	append	"This is a message from	the Galley Report Program",Mailbody
	Append  	CRLF,MailBody					
	Append	"Your PDF file was created!",Mailbody
	Append  	CRLF,MailBody					
	append	"Location:  c:\work\pdf\ ",mailbody
	Append  	CRLF,MailBody					
	append	"filename:  ",mailbody
	append	str55,mailbody
	Append  	CRLF,MailBody					
	Reset 	MailBody
		
	pack	mailattach,"c:\work\pdf\",str55	
	pack	Mailto,userlogn,"@nincal.com"
	pack	Mailfrom,userlogn,"@nincal.com"
	move	"Your PDF file was created ",Mailsubjct
	call	SendMail
.end patch 2.14

	return
.END PATCH 2.08 ADDED LOGIC
.Include IO file
        include nordio.inc
;Patch2.03
			include	compio.inc
			include	cntio.inc
.        include nmlrio.inc
;Patch2.03
        include nxrfio.inc
        include ndatio.inc
        include nownio.inc
        include npndio.inc
        include nord5io.inc
        include nspeio.inc
        include nsmpio.inc
.START PATCH 1.2 - ADDED LOGIC
        include nofrio.inc
.END PATCH 1.2 - ADDED LOGIC
.START PATCH 1.75 ADDED LOGIC
        include ncntio.inc
.END PATCH 1.75 ADDED LOGIC
.START PATCH 2.0A ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 2.0A ADDED LOGIC
        include comlogic.inc
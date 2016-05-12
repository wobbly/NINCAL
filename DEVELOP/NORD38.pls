........................................
. Program:      NORD0038.PLS
. Function:     LM Pending Order/Shipping/New LCR Report Program
. Author:       Andrew Harkins
. Orig. Date:   December 2,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
.        include f:\library\develop\backups\norddd.inc
.patch3.13
	include	compdd.inc
	include	cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
.patch3.13
        include nshpdd.inc
        include ndatdd.inc
        include npnddd.inc
        include nord4dd.inc
.        include f:\library\develop\backups\nord4dd.inc
        include nord5dd.inc
.        include f:\library\develop\backups\nord5dd.inc
.START PATCH 3.18 ADDED LOGIC
	include	winapi.inc
.END PATCH 3.18 ADDED LOGIC

release  init      "3.18"        ASH	17JAN2006 ADDED EMAIL OPTION
.release  init      "3.17"        DMB	18JUN2005 FM IP CHG
.release  init      "3.16"        ASH	09DEC2004	FAXFILE.PRN
.release  init      "3.15"        ASH	10AUG2004	Work Order 496 - Added logic
.release  init      "3.14"        ASH	09AUG2004	Logo Conversion
.release  init      "3.13"        DMB	26MAY2004	Mailer Conversion
.release   init      "3.12         "       11Feb04 send SMTP message do Comp Request.
.release	init	"3.1"	ASH	15JAN04 Added features for SK/BC/LM
.release	init	"3.00"	ASH	02JAN03 Rewrite of program to include Contact breaks

.Counters
COUNTR  FORM    9
COUNTR2 FORM    9
.Files to open
prfile  pfile
input   file
inputi  ifile
input2  file
input2i ifile
input3  file
input4  file
input4i ifile
First   init    "Y"
GoodStat init   "xp"
PENDING init    "P"
SHIPPING init   "S"
LCRING init   "X"
LCRFLAG init   "T"
PENDFLAG init   "T"
SHIPFLAG init   "T"
STATFLAG dim    1
PrtFlag dim     1
holdbrk dim     7
holdbrk2 dim	7
COMPHOLD DIM    45
CONTHOLD DIM    45
.START PATCH 3.1 ADDED LOGIC
CREDHOLD DIM	1
.END PATCH 3.1 ADDED LOGIC
newdate1 dim    10
.START PATCH 3.18 REPLACED LOGIC
.hold    dim     300     .Should be 284 but I am being cautious
hold    dim     400     .Should be 334 but I am being cautious
BrkEmail dim	50
HoldBrkEmail dim	50
HoldBrkName dim	75
PDFFlag	dim	1
.END PATCH 3.18 REPLACED LOGIC
faxnum  dim     10
LONGDIST dim    1
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    11
.
mss1    plform  Error
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.END PATCH 1.8 - REMMED LOGIC

        formload mss1
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10

.START PATCH 3.14 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
.END PATCH 3.14 ADDED LOGIC

        move    "NORD0038",WPrognme
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.was this supposed to be temp????              goto    sortfile
.Set Flags to Open NINORD.DAT
        move    C0,NORDFLAG
        move    C0,NORDFLG2

.Find out system information
	call	GetWinVer
        call    Paint
.        goto printfile
.Testing Only!
.	move	"\\nts2\d\data\",NTWKPATH1


	goto Printfile


.START PATCH 3.18 ADDED LOGIC
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
.
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
	else
.Send message to I.S.
		move	"This is a message from	NORD0038",SmtpSubject Subject
		move	"Failure to update pdf995.ini.",SmtpTextMessage(1)
		pack	SmtpTextMessage(2),"Field: ",str25
		pack	SmtpTextMessage(3),"Value: ",str35
		pack	SmtpTextMessage(4),"No Broker Reports will be emailed."
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
	endif
	clock	timestamp,str8
.END PATCH 3.18 ADDED LOGIC
.
        DISPLAY *P1:24,"OPENING FILES";
.was this supposed to be temp????        GOTO SORTFILE
.Open Files
	PACK     STR35,NTWKPATH1,"shipfax.dat"
        open    input,STR35
	PACK     STR35,NTWKPATH1,"index\shipfax.isi"
        open    inputi,STR35
.Patch 3.17 Begin
.	open    input2,"NINPRINT.DAT|20.20.30.103:502"
	open    input2,"NINPRINT.DAT|10.10.30.103:502"
.	open    input2i,"NINPRINT.ISI|20.20.30.103:502"
	open    input2i,"NINPRINT.ISI|10.10.30.103:502"
.Patch 3.17 End
.	open    input2,"NINPRINT.DAT"
.	open    input2i,"NINPRINT.ISI"
	PACK    STR35,NTWKPATH1,"lcrfax.dat"
        open    input4,STR35
	PACK    STR35,NTWKPATH1,"index\lcrfax.isi"
        open    input4i,STR35
        PACK     STR35,NTWKPATH1,"ORDFILE.DAT"
        prepare input3,STR35
        move    C1,NORDPATH
        call    NORDOPEN
        DISPLAY *P1:24,"READING NINPRINT FILE";
PendLoop
        loop
                read    input2,seq;ordvars
                until over
                scan    OSTAT,GoodStat
                if equal
                        reset   GoodStat
                        read    NORDFILE,OLRN;str1,str1
                        if over         .Record missing from NINORD
                                move    "This is an Error e-mail from NORD0038",SmtpSubject Subject
.   Set the text message that is send with the attachments
                                clear   str45
                                append  "LR ##: ",str45
                                append  OLRN,str45
                                reset   str45
                                move    str45,SmtpTextMessage(1)   Array <Text message >
                                move    "Record missing from NINORD!",SmtpTextMessage(2)   Array <Text message >
                                move    "2",SmtpTextIndexLast                               Index to last entry in TextMessage array
                                move    "NTS4",SmtpEmailServer                   Address of email serverc
.                                move    "InformationServices@nincal.com",SmtpEmailAddress
                                move    "ComputerRequest@nincal.com",SmtpEmailAddress
                                move    "InformationServices",SmtpUserName                                User name
                                move    "InformationServices",SmtpUserFullName              User Full Name
                                move    smtpemailaddress,SmtpDestinations(1,1)
                                move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
                                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                                clear   SmtpLogFile                                         'Clear' disables the LogFile
                                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                        elseif (OSTAT <> str1)      .Problem with NINPRINT!!!
                                move    "This is an Error e-mail from NORD0038",SmtpSubject Subject
.   Set the text message that is send with the attachments
                                clear   str45
                                append  "LR ##: ",str45
                                append  OLRN,str45
                                reset   str45
                                move    str45,SmtpTextMessage(1)   Array <Text message >
                                clear   str45
                                append  "OSTAT from NINPRINT: ",str45
                                append  OSTAT,str45
                                reset   str45
                                move    str45,SmtpTextMessage(2)   Array <Text message >
                                clear   str45
                                append  "OSTAT from NINORD: ",str45
                                append  str1,str45
                                reset   str45
                                move    str45,SmtpTextMessage(3)   Array <Text message >
                                move    "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
                                move    "NTS4",SmtpEmailServer                   Address of email serverc
.                                move    "InformationServices@nincal.com",SmtpEmailAddress
                                move    "ComputerRequest@nincal.com",SmtpEmailAddress
                                move    "InformationServices",SmtpUserName                                User name
                                move    "InformationServices",SmtpUserFullName              User Full Name
                                move    smtpemailaddress,SmtpDestinations(1,1)
                                move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
                                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                                clear   SmtpLogFile                                         'Clear' disables the LogFile
                                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                        endif
                        call    OrderReadOtherFiles
                        call    OrderReadPendFiles
                        filepi  1;input3
.START PATCH 3.1 REPLACED LOGIC
.                        write   input3,seq;PENDING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
.                                ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX
                        readtab NORDFILE,OLRN;*162,OCLRSTAT
.START PATCH 3.18 REPLACED LOGIC
.                        write   input3,seq;PENDING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
.                                ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,OCLRSTAT
                        write   input3,seq;PENDING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
                                ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,OCLRSTAT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
.DELETE CANCELLED\DENIED RECORDS AFTER THEY ARE WRITTEN TO NEW FILE
                        clear   N2
                        move    NORD4STAT to N2
                        if (OSTAT = "x" AND (N2 = 6 | N2 = 7))
				filepi  2;input2i
				read    input2i,OLRN;;
				delete  input2i,OLRN
                        endif
                else
                        reset   GoodStat
                endif
         repeat
        DISPLAY *P1:24,"READING SHIPFAX FILE ";
ShipLoop
        loop
                read    input,seq;ordvars
                until over
                call    OrderReadShippingFile
                filepi  1;input3
.START PATCH 3.1 REPLACED LOGIC
.		write   input3,seq;SHIPPING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,SQUANT,SDATE:
.		        SINFO,STRACK,BRFAX
.START PATCH 3.18 REPLACED LOGIC
.		write   input3,seq;SHIPPING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,SQUANT,SDATE:
.		        SINFO,STRACK,BRFAX,BRCREDIT
		write   input3,seq;SHIPPING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,SQUANT,SDATE:
		        SINFO,STRACK,BRFAX,BRCREDIT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
.DELETE RECORDS AFTER THEY ARE WRITTEN TO NEW FILE
			filepi  2;inputi
			read    inputi,OLRN;;
			delete  inputi,OLRN
        repeat

LCRLoop
        DISPLAY *P1:24,"READING LCRFAX FILE ";
        loop
                read    input4,seq;str6
		until over
		call	Trim using str6
		if (str6 <> "")
			move	C1,NORDPATH
			pack	NORDFLD,str6
			move	"LCRLoop-NORDKEY",Location
			pack	KeyLocation,"Key: ",NORDFLD
			call	NORDKEY
                	if not over
				if (OSTAT = "l" AND OSALES10 = "0" AND OSALES = "6")
		        	        call    OrderReadLCRFiles
					if (NORD5STAT <> "04" & NORD5STAT <> "05")
				                call    OrderReadOtherFiles
				               	filepi  1;input3

.START PATCH 3.1 REPLACED LOGIC
.			        	        write   input3,seq;LCRING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
.		        	        	        OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX
.START PATCH 3.18 REPLACED LOGIC
.			        	        write   input3,seq;LCRING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
.		        	        	        OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT
			        	        write   input3,seq;LCRING,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES,OQTY:
		        	        	        OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
					endif
				endif
			endif
.DELETE RECORDS AFTER THEY ARE WRITTEN TO NEW FILE
			filepi  2;input4i
			read    input4i,str6;;
			delete  input4i,str6
		endif
        repeat
        DISPLAY *P1:24,"SORTING ORDFILE FILE ";
SortFile
        clear   taskname
        move    NTWKPATH1,str13         .Depend on str13 to cut off last SLASH of NTWKPATH1
.START PATCH 3.18 REPLACED LOGIC
.        PACK    TASKNAME,"ORDFILE.DAT,",NTWKPATH1,"ORDFILE.SRT,",str13,";2-8,1-1"
        PACK    TASKNAME,"ORDFILE.DAT,",NTWKPATH1,"ORDFILE.SRT,",str13,";2-11,1-1"
.END PATCH 3.18 REPLACED LOGIC
        reset   taskname
        sort    taskname
        if over
                move    s$error$,error
                move    "Sort did not work!",Location
                clear	KeyLocation
                call    IOMssg
                stop
        endif
        clear   taskname
        CALL    PAINT
        DISPLAY *P1:24,"PRINTING FILES        ";
PrintFile
.Set up columns
        move    "500",column
        move    "1500",column2
        move    "3700",column3
        move    "4500",column4
        move    "5600",column5
.Initialize HLDBRK
        clear   HOLDBRK
.close unsorted file
        close   input3
.open newly sorted file
        PACK    STR35,NTWKPATH1,"ORDFILE.SRT"


	prep	tempfile,"c:\work\email.dat"
        open    input3,"c:\work\ordfile.srt"
        loop
                read    input3,seq;hold
                until over
                move    hold,STATFLAG
.NOTE:  Both BRCOMP & BRFAX are really only used for the first record!!!
                if (STATFLAG = SHIPPING)
.START PATCH 3.1 REPLACED LOGIC
.                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                                SQUANT,SDATE,SINFO,STRACK,BRFAX
.START PATCH 3.18 REPLACED LOGIC
.                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                                SQUANT,SDATE,SINFO,STRACK,BRFAX,BRCREDIT
                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
                                SQUANT,SDATE,SINFO,STRACK,BRFAX,BRCREDIT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
                elseif (STATFLAG = PENDING)
.START PATCH 3.1 REPLACED LOGIC
.                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                                OQTY,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX
.START PATCH 3.18 REPLACED LOGIC
.                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                                OQTY,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,OCLRSTAT
                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
                                OQTY,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,OCLRSTAT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
		else	.STATFLAG = LCRING
.START PATCH 3.1 REPLACED LOGIC
.                        unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                                OQTY,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX
.START PATCH 3.18 REPLACED LOGIC
.			unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.				OQTY,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT
			unpack  hold,STATFLAG,OBRKNUM,OBRKCNT,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
				OQTY,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC,BRFAX,MSTAT,BRCREDIT,BrkEmail
.END PATCH 3.18 REPLACED LOGIC
.END PATCH 3.1 REPLACED LOGIC
                endif
                write	tempfile,SEQ;BrkEmail
	repeat
	shutdown
afterloop
	loop
		pack	HOLDBRK2,OBRKNUM,OBRKCNT
                if (HOLDBRK2 <> HOLDBRK)
.When you run out of pending orders but there are no shipped orders you should still print
.the pending header.  However, in some very obscure instances there are exactly 13 pending
.orders and no shipping orders.  OrderShipHeader will then call OrderPrintHeader, using current
.Broker Company(which may be incorrect).  Without COMPHOLD what would happen in this extreme
.case is the second page would print but list a different Broker Company at the top.
                        if (FIRST = YES)
                                move    BRCOMP,COMPHOLD
                                move    BRCNTCT,CONTHOLD
.START PATCH 3.1 ADDED LOGIC
				move	BRCREDIT,CREDHOLD
.END PATCH 3.1 ADDED LOGIC
                        endif
                        if (LCRFLAG = FALSE)
	                        if (SHIPFLAG = FALSE)
	                                call    OrderShipHeader
	                                add     eightlpi,row
	                                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
	                                add     eightlpi,row
	                                add     eightlpi,row
	                                add     eightlpi,row
	                        endif
                                call    OrderLCRHeader
                                add     eightlpi,row
                                prtpage prfile;*p3100:row,*font=font2,"     --- NO NEW LCRS ---";
                                add     eightlpi,row
                        endif
                        if (FIRST = NO)
                                PRTCLOSE prfile
.START PATCH 3.18 REPLACED LOGIC
.                                if (PrtFlag = NO)
                                if (PrtFlag = "E")	.Email it?
.Give the email a chance of rendering itself before updating the INI file.
					pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
					loop
						call	FindFirstFile
						until (APIResult = 0 | APIResult = hexeight)
						pause	"1"
					repeat
					pause	"5"
.Prepare Flag file once again
					prep	tempfile,"c:\progra~1\pdf995\flag.dat"
					write	tempfile,SEQ;"flag set"
					close	tempfile
.
					call	CreatePDFFile
                                elseif (PrtFlag = NO)
.END PATCH 3.18 REPLACED LOGIC
                                        clear   taskname
                                        Path    Exist,"c:\windows"
                                        if      over
                                                append  "c:\winnt\system32\cmd.exe",taskname
                                        else
                                                append  "!c:\command.com",taskname
                                        endif
.START PATCH 3.16 REPLACED LOGIC
.                                        append  " /c copy \\nts0\d\data\fax\hdrfile.prn /b + \\nts0\d\data\fax\faxfile.prn /b \\nts0\d\data\fax\fax",taskname
                                        append  " /c copy C:\WORK\hdrfile.prn /b + C:\WORK\faxfile.prn /b \\nts0\d\data\fax\fax",taskname
.END PATCH 3.16 REPLACED LOGIC
                                        append  HOLDBRK,taskname
                                        append  ".prn /b",taskname
                                        reset   taskname
                                        execute taskname
                                        clear   taskname
                                        Path    Exist,"c:\windows"
                                        if      over
                                                append  "c:\winnt\system32\cmd.exe",taskname
                                        else
                                                append  "!c:\command.com",taskname
                                        endif
                                        append  " /c copy \\nts0\d\data\fax\fax",taskname
                                        append  HOLDBRK,taskname
                                        append  ".prn \\nts2\fax",taskname
                                        reset   taskname
                                        execute taskname
                                endif
                                call    OrderOpenFile
                        else
                                move    NO,FIRST
                                call    OrderOpenFile
                        endif
			pack	HOLDBRK,OBRKNUM,OBRKCNT
                        move    BRCOMP,COMPHOLD
			move    BRCNTCT,CONTHOLD
.START PATCH 3.1 ADDED LOGIC
			move	BRCREDIT,CREDHOLD
.END PATCH 3.1 ADDED LOGIC

                        clear   page
                        call    OrderPrintHeader
                        call    OrderPendHeader
                        move    FALSE,SHIPFLAG
                        move    FALSE,PENDFLAG
                        move    FALSE,LCRFLAG
                        if (STATFLAG <> PENDING)
                                add     eightlpi,row
                                prtpage prfile;*p3100:row,*font=font2,"--- NO PENDING ORDERS ---";
                                add     eightlpi,row
                                add     eightlpi,row
                                add     eightlpi,row
                                call    OrderShipHeader
	                        if (STATFLAG <> SHIPPING)
	                                add     eightlpi,row
	                                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
	                                add     eightlpi,row
	                                add     eightlpi,row
	                                add     eightlpi,row
	                                call    OrderLCRHeader
				endif
                        endif
                endif
                if (STATFLAG = PENDING)
                        move    TRUE,PENDFLAG
                        call    OrderPrintPendRecord
                elseif (STATFLAG = SHIPPING)
                        if (PENDFLAG = TRUE)
                                move    FALSE,PENDFLAG
                                call    OrderShipHeader
                        endif
                        move    TRUE,SHIPFLAG
                        call    OrderPrintShipRecord
		else	.STATFLAG = LCRING
                        if (PENDFLAG = TRUE | SHIPFLAG = TRUE)
				if (SHIPFLAG = FALSE)
		                        call    OrderShipHeader
	                                add     eightlpi,row
	                                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
	                                add     eightlpi,row
	                                add     eightlpi,row
	                                add     eightlpi,row
				endif
                                move    FALSE,PENDFLAG
                                move    FALSE,SHIPFLAG
                                call    OrderLCRHeader
                        endif
                        move    TRUE,LCRFLAG
                        call    OrderPrintLCRRecord
                endif
        repeat

LastRec
        if (LCRFLAG = FALSE)
	        if (SHIPFLAG = FALSE)
	                call    OrderShipHeader
	                add     eightlpi,row
	                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
	                add     eightlpi,row
		        add     eightlpi,row
		        add     eightlpi,row
	        endif
                call    OrderLCRHeader
                add     eightlpi,row
                prtpage prfile;*p3100:row,*font=font2,"--- NO NEW LCRS ---";
                add     eightlpi,row
        endif
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.START PATCH 3.18 REPLACED LOGIC
.	if (PrtFlag = NO)
	if (PrtFlag = "E")	.Email it?
.Give the email a chance of rendering itself before updating the INI file.
		pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
		loop
			call	FindFirstFile
			until (APIResult = 0 | APIResult = hexeight)
			pause	"1"
		repeat
		pause	"5"
		call	CreatePDFFile
	elseif (PrtFlag = NO)
.END PATCH 3.18 REPLACED LOGIC
                clear   taskname
                Path    Exist,"c:\windows"
                if      over
                        append  "c:\winnt\system32\cmd.exe",taskname
                else
                        append  "!c:\command.com",taskname
                endif
.START PATCH 3.16 REPLACED LOGIC
.                append  " /c copy \\nts0\d\data\fax\hdrfile.prn /b + \\nts0\d\data\fax\faxfile.prn /b \\nts0\d\data\fax\fax",taskname
                append  " /c copy C:\WORK\hdrfile.prn /b + C:\WORK\faxfile.prn /b \\nts0\d\data\fax\fax",taskname
.END PATCH 3.16 REPLACED LOGIC
                append  HOLDBRK,taskname
                append  ".prn /b",taskname
                reset   taskname
                execute taskname
                clear   taskname
                Path    Exist,"c:\windows"
                if      over
                        append  "c:\winnt\system32\cmd.exe",taskname
                else
                        append  "!c:\command.com",taskname
                endif
                append  " /c copy \\nts0\d\data\fax\fax",taskname
                append  HOLDBRK,taskname
                append  ".prn \\nts2\fax",taskname
                reset   taskname
                execute taskname
        endif
        clear   taskname
        Path    Exist,"c:\windows"
        if      over
                append  "c:\winnt\system32\cmd.exe",taskname
        else
                append  "!c:\command.com",taskname
        endif
        append  " /c del ",taskname
        append  NTWKPATH1,taskname
        append  "ordfile.dat",taskname
        reset   taskname
        execute taskname

	pack	taskname,NTWKPATH1,"ordfile.srt"
	pack	str55,NTWKPATH1,"nord38tst.srt"
	copyfile taskname,str55


        clear   taskname
        Path    Exist,"c:\windows"
        if      over
                append  "c:\winnt\system32\cmd.exe",taskname
        else
                append  "!c:\command.com",taskname
        endif
        append  " /c del ",taskname
        append  NTWKPATH1,taskname
        append  "ordfile.srt",taskname
        reset   taskname
        execute taskname
        clear   taskname
        Path    Exist,"c:\windows"
        if      over
                append  "c:\winnt\system32\cmd.exe",taskname
        else
                append  "!c:\command.com",taskname
        endif
.START PATCH 3.16 REPLACED LOGIC
.        append  " /c del \\nts0\d\data\fax\faxfile.prn",taskname
        append  " /c del C:\WORK\faxfile.prn",taskname
.END PATCH 3.16 REPLACED LOGIC
        reset   taskname
        execute taskname
        PAUSE   C5
        STOP

OrderOpenFile
.Print newly sorted file
.START PATCH 3.18 ADDED LOGIC
	call	Trim using BrkEmail
	if (BrkEmail <> "" & PDFFlag = "1")
		move	BrkEmail,HoldBrkEmail
		pack	HoldBrkName,BRCNTCT
		move	"E",PrtFlag
	else
		clear	HoldBrkEmail
		clear	HoldBrkName
.END PATCH 3.18 ADDED LOGIC
		move    BRFAX,faxnum
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
					match   "510",str3
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
.START PATCH 3.18 ADDED LOGIC
	endif
.END PATCH 3.18 ADDED LOGIC
.START NEW TEST - PREVENT FAXES FROM BEING CREATED
.	move    YES,PrtFlag             .PRINT IT
.END NEW TEST - PREVENT FAXES FROM BEING CREATED
.START PATCH 3.18 REPLACED LOGIC
.        if (PrtFlag = YES)
	if (PrtFlag = "E")
.Email it
		pack	str55,str8,OBRKNUM,OBRKCNT
		PRTOPEN prfile,"PDF995",str55
		ADD     C1,COUNTR
	elseif (PrtFlag = YES)
.END PATCH 3.18 REPLACED LOGIC
.Printer of your choice
                if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
                        PRTOPEN prfile,"\\NTS0\Laser6","FAXFILE.PRN"
		elseif (osflag = C3 | osflag = C4)
                        PRTOPEN prfile,"Laser6","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"-","FAXFILE.PRN"
                endif
                ADD     C1,COUNTR2
        else
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
.START PATCH 3.16 REPLACED LOGIC
.                SPLOPEN "\\nts0\d\DATA\FAX\HDRFILE.PRN"
                SPLOPEN "C:\WORK\HDRFILE.PRN"
.END PATCH 3.16 REPLACED LOGIC
                print   "^[D",longdist,faxnum,"^[N",brcomp:
                        "^[SList Management"," ^]"
                SPLCLOSE
                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
                ADD     C1,COUNTR
        endif
        DISPLAY *P10:12,*EL,"FAX   COUNT ",COUNTR
        DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR2
        RETURN
OrderReadShippingFile
.includes OrderReadOtherFiles
.Shipping File
         rep     zfill,OLRN
         pack    NSHPFLD,OLRN
         move    C3,NSHPLOCK
         move    "Driver-NSHPKEY,1rst",Location
         pack    KeyLocation,"Key: ",NSHPFLD
         call    NSHPKEY
         if over
                 pack   SQUANT,"UNKNOWN"
                 clear  SDATE
                 pack   SINFO,"UNKNOWN"
                 pack   STRACK,"UNKNOWN"
         endif
OrderReadOtherFiles
.Open other files to retrieve appropriate information
.Mailer File
         rep     zfill,OMLRNUM
         pack    MKEY,OMLRNUM,"000"      .Master Record
         move    C3,NMLRLOCK
         move    "Driver-NMLRKEY,1rst",Location
         pack    KeyLocation,"Key: ",MKEY
         call    NMLRKEY
         if over
                 pack   MCOMP,"UNKNOWN",B55
         endif
OrderReadBrokerFile
.Broker File
	rep     zfill,OBRKNUM
	rep     zfill,OBRKCNT
	pack    NBRKFLD,OBRKNUM,OBRKCNT
	move    C3,NBRKLOCK
	move    "Driver-NBRKKEY,1rst",Location
	pack    KeyLocation,"Key: ",NBRKFLD
	call    NBRKKEY
	if over
.                 move   "000",OBRKCNT
.                 pack   NBRKFLD,OBRKNUM,OBRKCNT
.                 call   NBRKKEY        .CALL USING HEAD RECORD TO EXTRACT BRCOMP
.                 if over
                         pack   BRCNTCT,"UNKNOWN",B55
.                 endif
.START PATCH 3.18 ADDED LOGIC
		clear	BrkEmail
.END PATCH 3.18 ADDED LOGIC
	else
.START PATCH 3.18 ADDED LOGIC
		move	CNCTEMAIL,BrkEmail
.END PATCH 3.18 ADDED LOGIC
.Test for valid Fax Number - If invalid use default for Company, but retain contact name
		call	Trim using BRFAX
		if (BRFAX = "")
			move	BRCNTCT,str45
			pack    NBRKFLD,OBRKNUM,"000"
			move    "Driver-NBRKKEY,2ND",Location
			pack    KeyLocation,"Key: ",NBRKFLD
			call    NBRKKEY
			move	str45,BRCNTCT
		endif
	endif
	return

OrderReadPendFiles
.NINORD4 File
        move    OLRN,NORD4FLD
        rep     zfill,NORD4FLD
        move    C3,NORD4LOCK
        move    "O.ReadPend-NORD4KEY",Location
        pack    KeyLocation,"Key: ",NORD4FLD
        call    NORD4KEY
        if over
                move    "No Status Found!",NPNDDESC
        else
.NINPND File
                if (NORD4STAT = "11" | NORD4STAT = "12")
                        move    "00",NORD4STAT
                endif
                move    "p",str1
                pack    NPNDFLD,str1,NORD4STAT
                rep     zfill,NPNDFLD
                move    C3,NPNDLOCK
                move    "Driver-NPNDKEY",Location
                pack    KeyLocation,"Key: ",NPNDFLD
                call    NPNDKEY
                if over
                        move    "No Status Found!",NPNDDESC
                endif
        endif
        return

OrderReadLCRFiles
.NINORD5 File
        move    OLRN,NORD5FLD
        rep     zfill,NORD5FLD
        move    C3,NORD5LOCK
        move    "O.ReadPend-NORD5KEY",Location
        pack    KeyLocation,"Key: ",NORD5FLD
        call    NORD5KEY
        if not over
.NINPND File
                if (NORD5STAT = "04" | NORD5STAT = "05")
                        return
                endif
.                move    "l",str1
.                pack    NPNDFLD,str1,NORD5STAT
.                rep     zfill,NPNDFLD
.                move    C3,NPNDLOCK
.                move    "Driver-NPNDKEY",Location
.                pack    KeyLocation,"Key: ",NPNDFLD
.                call    NPNDKEY
.                if over
.                        move    "No Status Found!",NPNDDESC
.                endif
        endif
	move	"Received",NPNDDESC
        return

.Print Heading
OrderPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 3.14 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
..............................
	prtpage	prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
.END PATCH 3.14 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:  ";
        prtpage prfile;CONTHOLD;
        prtpage prfile;*p6000:row,"FROM:  List Management";
        add     eightlpi,row
.....START TESTING PURPOSES ONLY!.......
.If the Fax Number is to be printed out then it will need to be dumped into a hold variable like COMPHOLD - faxnum might work.
.	call	Trim using BRFAX
.	if (BRFAX = "0000000000")
.		clear	str18
.	else
.		count	howmany,BRFAX
.		if (howmany = 7)
.			unpack	BRFAX,str2,str1,str4
.			pack	str18," - ",str2,str1,DASH,str4
.		else
.			unpack	BRFAX,str3,str2,str1,str4
.			pack	str18," - (",str3,")",str2,str1,DASH,str4
.		endif
.	endif
.	prtpage prfile;*pcolumn:row,"        ",COMPHOLD,str18;
.....END TESTING PURPOSES ONLY!.......
.START PATCH 3.1 REPLACED LOGIC
.        prtpage prfile;*pcolumn:row,"        ",COMPHOLD;
...............................................
;		' '=OK,
;	"*" = ON HOLD.
;	"I" = INACTIVE,
;	"B" = CREDIT RISK.  -      reset nightly if released
;	"N" =   NEW MAILER.
;	"P" = POLITICAL MAILER.  - reset nightly if released
;	"W" = Warning - read note		; 21Dec2000
;	"M" = Must Prepay			; 05Mar2002
;	"9" = On hold until over 90s paid	; 05Mar2002
;	"G" = Guarantees are always required	; 05Mar2002
...............................................
	move	COMPHOLD,taskname
	call	Trim using taskname
 	if (CREDHOLD = "*")
		pack	taskname,taskname," - Broker Credit Hold"
	elseif (CREDHOLD = "B")
		pack	taskname,taskname," - Broker Credit Risk"
	elseif (CREDHOLD = "M")
		pack	taskname,taskname," - Broker Must Prepay"
	elseif (CREDHOLD = "9")
		pack	taskname,taskname," - Broker on Hold until over 90s paid"
	elseif (CREDHOLD = "G")
		pack	taskname,taskname," - Broker Guarantees are always required"
	endif
	prtpage prfile;*pcolumn:row,"        ",taskname;
.END PATCH 3.1 REPLACED LOGIC
        prtpage prfile;*p6000:row,"DATE:   ",newdate1;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Below is the status information on your order(s).  ";
        prtpage prfile;"Please distribute to the listed Contact name(s).  Thank you.";
        add     eightlpi,row
.START PATCH 3.15 REPLACED LOGIC
        prtpage prfile;*pcolumn:row,"**All pricing subject to rate verification at time of order. Please verify pricing at www.namesinthenews.com";
.END PATCH 3.15	REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        return
OrderPendHeader
.........Pending Header
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Broker PO";
        prtpage prfile;*pcolumn2:row,"Contact";
        prtpage prfile;*pcolumn3:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Request Date";
        prtpage prfile;*pcolumn5:row,"Status";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NIN ##";
        prtpage prfile;*pcolumn2:row,"Mailer";
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"List";
        add     eightlpi,row
        prtpage prfile;*p3000:row,"STATUS OF ORDERS RECEIVED";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7300:row;
        add     eightlpi,row
        return
OrderShipHeader
.........Shipping Header
.........Test for enough space
        if (row > 8496)                         .POSITION OF 11th PENDING RECORD
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Broker PO";
        prtpage prfile;*pcolumn2:row,"Contact";
        prtpage prfile;*pcolumn3:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Ship Date";
        prtpage prfile;*pcolumn5:row,"Ship Method";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NIN ##";
        prtpage prfile;*pcolumn2:row,"Mailer";
        prtpage prfile;*pcolumn5:row,"Tracker ##";
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"List";
        add     eightlpi,row
        prtpage prfile;*p3100:row,"SHIPPING CONFIRMATION";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*line=7300:row;
        add     eightlpi,row
        return

OrderLCRHeader
.........LCR Header
.........Test for enough space
        if (row > 8496)                         .POSITION OF 11th PENDING RECORD
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Broker PO";
        prtpage prfile;*pcolumn2:row,"Contact";
        prtpage prfile;*pcolumn3:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Record Date";
        prtpage prfile;*pcolumn5:row,"Status";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NIN ##";
        prtpage prfile;*pcolumn2:row,"Mailer";
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"List";
        add     eightlpi,row
        prtpage prfile;*p3000:row,"NEW LIST CLEARANCE REQUESTS";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7300:row;
        add     eightlpi,row
        return

OrderPrintPendRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 9576)        .POSITION OF 13th RECORD
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderPendHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OMLRPON;
        prtpage prfile;*pcolumn2:row,*boldon,BRCNTCT,*boldoff;
        move    EditMask,EditQuan
        move    C0,N9
        move    OQTY,N9
        edit    N9,EditQuan
        prtpage prfile;*pcolumn3:row,EditQuan;
        call    TRIM using ORTNDTEM
        count   N2,ORTNDTEM
        if (N2 > 0 AND ORTNDTEM <> "00")
                prtpage prfile;*pcolumn4:row,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY;
        endif
        prtpage prfile;*pcolumn5:row,NPNDDESC;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OLRN;
        prtpage prfile;*pcolumn2:row,MCOMP;
.START PATCH 3.1 ADDED LOGIC
.1=EXCHANGE, 2=RENT, 3=EXC/SPLIT
 	if (OCLRSTAT = "1")
		prtpage prfile;*pcolumn5:row,"Approved for Exchange";
	elseif (OCLRSTAT = "2")
		prtpage prfile;*pcolumn5:row,"Approved for Rent";
	elseif (OCLRSTAT = "3")
		prtpage prfile;*pcolumn5:row,"Approved for Split";
	endif
.END PATCH 3.1 ADDED LOGIC
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,O1DES;
.START PATCH 3.1 ADDED LOGIC
.		' '=OK,
;	"*" = ON HOLD.
;	"I" = INACTIVE,
;	"B" = CREDIT RISK.  -      reset nightly if released
;	"N" =   NEW MAILER.
;	"P" = POLITICAL MAILER.  - reset nightly if released
;	"W" = Warning - read note		; 21Dec2000
;	"M" = Must Prepay			; 05Mar2002
;	"9" = On hold until over 90s paid	; 05Mar2002
;	"G" = Guarantees are always required	; 05Mar2002
...............................................
 	if (MSTAT = "*")
		prtpage prfile;*pcolumn5:row,"Mailer Credit Hold";
	elseif (MSTAT = "B")
		prtpage prfile;*pcolumn5:row,"Mailer Credit Risk";
	elseif (MSTAT = "M")
		prtpage prfile;*pcolumn5:row,"Mailer Must Prepay";
	elseif (MSTAT = "9")
		prtpage prfile;*pcolumn5:row,"Mailer on Hold until over 90s paid";
	elseif (MSTAT = "G")
		prtpage prfile;*pcolumn5:row,"Mailer Guarantees are always required";
	endif
.END PATCH 3.1 ADDED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        return

OrderPrintLCRRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 9576)        .POSITION OF 13th RECORD
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderLCRHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OMLRPON;
        prtpage prfile;*pcolumn2:row,*boldon,BRCNTCT,*boldoff;
        move    EditMask,EditQuan
        move    C0,N9
        move    OQTY,N9
        edit    N9,EditQuan
        prtpage prfile;*pcolumn3:row,EditQuan;
        call    TRIM using OODTEM
        count   N2,OODTEM
        if (N2 > 0 AND OODTEM <> "00")
                prtpage prfile;*pcolumn4:row,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY;
        endif
        prtpage prfile;*pcolumn5:row,NPNDDESC;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OLRN;
        prtpage prfile;*pcolumn2:row,MCOMP;
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,O1DES;
.START PATCH 3.1 ADDED LOGIC
.		' '=OK,
;	"*" = ON HOLD.
;	"I" = INACTIVE,
;	"B" = CREDIT RISK.  -      reset nightly if released
;	"N" =   NEW MAILER.
;	"P" = POLITICAL MAILER.  - reset nightly if released
;	"W" = Warning - read note		; 21Dec2000
;	"M" = Must Prepay			; 05Mar2002
;	"9" = On hold until over 90s paid	; 05Mar2002
;	"G" = Guarantees are always required	; 05Mar2002
...............................................
 	if (MSTAT = "*")
		prtpage prfile;*pcolumn5:row,"Mailer Credit Hold";
	elseif (MSTAT = "B")
		prtpage prfile;*pcolumn5:row,"Mailer Credit Risk";
	elseif (MSTAT = "M")
		prtpage prfile;*pcolumn5:row,"Mailer Must Prepay";
	elseif (MSTAT = "9")
		prtpage prfile;*pcolumn5:row,"Mailer on Hold until over 90s paid";
	elseif (MSTAT = "G")
		prtpage prfile;*pcolumn5:row,"Mailer Guarantees are always required";
	endif
.END PATCH 3.1 ADDED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        return

OrderPrintShipRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 9576)        .POSITION OF 13th RECORD
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderShipHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OMLRPON;
        prtpage prfile;*pcolumn2:row,*boldon,BRCNTCT,*boldoff;
        move    EditMask,EditQuan
        move    C0,N9
        move    SQUANT,N9
        edit    N9,EditQuan
        prtpage prfile;*pcolumn3:row,EditQuan;
        unpack  SDATE,str2,YY,MM,DD
        call    TRIM using MM
        count   N1,MM
        if (N1 > 0 AND MM <> "00")
                prtpage prfile;*pcolumn4:row,MM,SLASH,DD,SLASH,str2,YY;
        endif
        prtpage prfile;*pcolumn5:row,SINFO;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OLRN;
        prtpage prfile;*pcolumn2:row,MCOMP;
        prtpage prfile;*pcolumn5:row,STRACK;
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,O1DES;
        add     eightlpi,row
        add     eightlpi,row
        return

.START PATCH 3.18 ADDED LOGIC
CreatePDFFile
	move	"NIN Broker Report",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	"Attached is your NIN Broker Report.",SmtpTextMessage(1)
	move	"1",SmtpTextIndexLast
	move	"NTS4",SmtpEmailServer
	move	"JoeyGamache@nincal.com",SmtpEmailAddress
	move	"",SmtpUserName
	move	"",SmtpUserFullName
.Destination is of course where it's going.  1 - Display Name 2- address , 3 - result code(not used)
	move	HoldBrkName,SmtpDestinations(1,1)
	move    HoldBrkEmail,SmtpDestinations(1,2)
.
	pack	APIFileName,"c:\work\pdf\",str55,".Pdf",hexzero
	call	FindFirstFile
	move	"1",SmtpDestIndexLast
	move	"0",SmtpAttIndexLast
	if (APIResult <> 0 & APIResult <> hexeight)
	//Only send if attachment is available
		pack	SmtpAttachments(1,1),str55,".Pdf"
		move	"c:\work\pdf",SmtpAttachments(1,2)
		move	"1",SmtpAttIndexLast
		clear	SmtpLogFile
		call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	else
	//Send message stating that report did NOT go out!!
		move	"NIN Broker Report",SmtpSubject Subject
		move	"The Report for the following Broker was NOT emailed!",SmtpTextMessage(1)
		unpack	str55,str16,SmtpTextMessage(2)
		move	"2",SmtpTextIndexLast
		move	"NTS4",SmtpEmailServer
		move	"informationservices@nincal.com",SmtpEmailAddress
		move	smtpemailaddress,SmtpDestinations(1,1)
		pack	APIFileName,"c:\work\pdf\",str55,".Pdf",hexzero
		call	FindFirstFile
		move	"1",SmtpDestIndexLast
		move	"0",SmtpAttIndexLast
		clear	SmtpLogFile
		call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	endif
	return
.END PATCH 3.18 ADDED LOGIC

.Include IO file
        include nordio.inc
.patch3.13
	include	compio.inc
	include	cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
.patch3.13
        include nshpio.inc
        include ndatio.inc
        include npndio.inc
        include nord4io.inc
        include nord5io.inc
        include comlogic.inc
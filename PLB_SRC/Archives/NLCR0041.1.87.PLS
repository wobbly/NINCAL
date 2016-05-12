........................................
. Program:      NLCR0041.PLS
. Function:     LM Outside Broker Report Program (produced on Screen5 of program 1)
. Author:       Andrew Harkins
. Orig. Date:   September 17, 1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
        include nord5dd.inc
        include nord4dd.inc
        include nordPdd.inc
;Patch1.8

			include	compdd.inc
			include	cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
;Patch1.8
        include nxrfdd.inc
        include ndatdd.inc
        include npnddd.inc
.START PATCH 1.64 REPLACED LOGIC
.        include nspedd.inc
        include nspe2dd.inc
.END PATCH 1.64 REPLACED LOGIC
        include ncntdd.inc
        include nusedd.inc
        include winapi.inc
.START PATCH 1.7 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 1.7 ADDED LOGIC

release  init      "1.87"        ASH	17JAN2006	Corrected Sort Error
.release  init      "1.86"        ASH	09AUG2005	Small Changes for List Management
.release  init      "1.85"        ASH	18MAY2005	LISTMLR Conversion
.release  init      "1.84"        ASH	03MAY2005	Changed Printer flags in Program 1
.release  init      "1.83"        ASH	09DEC2004	FAXFILE
.release  init      "1.82"        ASH	02DEC2004	Conversion from Acrobat Distiller to PDF995
.release  init      "1.81"        ASH	06AUG2004	Logo Conversion
.release  init      "1.80"        DMB	26MAY2004	Mailer Conversion
.release   init      "1.70"        28JAN04  ASH  DATACARD CONVERSION
.release   init      "1.69.1"        02FEB2004	DMB change phone number from main fax line to lm fax
.release   init      "1.69"        12Aug2003        ASH Changed email address for LM Contact
.release   init      "1.68"        01Aug2003        ASH Patch 1.67 was incomplete
.release   init      "1.67"        12Jul2002        DLH    Use GetWinVer
.release   init      "1.66"        20APR2001        ASH     CHANGED CONTACT TO KOBEE
.release   init      "1.65"        20NOV2000        ASH     NEW SERVER ADDED
.RELEASE   INIT      "1.64"       31OCT2000 ASH  CHANGED CONTACT TO JHANSEN
.                                                CHANGED LOGIC FOR DYNAMIC FAXES
.                                                AS PER MEETING WITH JN, SM, BLO - MAILER NOTES WILL BE PRINTED
.RELEASE   INIT      "1.63"       05OCT2000 ASH  CORRECTED LIMITATION WITH INPNAME - ADDED USERS DRIVE PATH
.                                                ADDED CONDITION FOR NT5 FOR OSFLAG
.                                                CORRECTED LOGIC FOR PRTOPEN
.RELEASE   INIT      "1.61"       11SEP2000 ASH   ALLOW AUTOMATIC FAXING
.RELEASE   INIT      "1.6"       01AUG2000 ASH   ALLOW PDF FORMAT ON SERVER
.RELEASE   INIT      "1.52"       07JUN2000 ASH   ADDED SELECT
.RELEASE   INIT      "1.51"       01MAY2000 ASH   REPLACED EMAIL LOGIC
.                                               REPLACED CRITERIA FOR "Pending Order" verbage
.                                               ADDED OPTION FOR LASER2
.RELEASE   INIT      "1.5"       15MAR2000 ASH   RE-WORKED LOGIC FOR HEADER/FOOTER
.release   init      "1.4"       25FEB2000 ASH   Added option for Denied records
.release   init      "1.3"       17FEB2000 ASH   Removed/Increased fonts, now part of CONS.INC
.release   init      "1.2"       03FEB2000 ASH   Added code to use with BUTIL (undocmented)
.                                               Replaced DOS Del with DeleteFileA
.                                               Replaced all G:\DATA with C:\WORK
.release   init      "1.1"       29Nov99 ASH     Replaced OTOCODE with ORENT
.release init    "1.0"   ASH 17SEP99  DEVELOPMENT RELEASE

.START PATCH 1.6 ADDED VARS
timestamp1 dim  16
timestamp2 dim  16
time1   form    16
time2   form    16
time3   form    16
.END PATCH 1.6 ADDED VARS
.Counters
COUNTR  FORM    9
.Files to open
prfile  pfile
input2  file
input3  file
input2Name dim    40
First   init    "Y"
.Some vars
HOLDBRK dim     7
HOLDMAIL dim    4
newdate1 dim    10
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    11
.START PATCH 1.3 - REPLACED CODE
line1   dim    100
.line1   dim     90
.END PATCH 1.3 - REPLACED CODE
Carr    init    0x7f
;begin patch 1.67
;osflag   form   1       .1=win 95,98, 2=NT
;end patch 1.67
PrintFlag form  "01"    .Default is Laser3!!!
userinfo dim    500
userlogn dim    7
userlogw dim    7
.FilePath dim    25
.
.START PATCH 1.6 ADDED VARS
faxnum  dim     10
LONGDIST dim    1
PrtFlag dim     1
.END PATCH 1.6 ADDED VARS
.START PATCH 1.2 - ADDED VAR
.hexeight integer 4,"4294967295"
.END PATCH 1.2 - ADDED VAR
.START PATCH 1.5 REMOVED VAR
.pict1   pict
.pict2   pict
.END PATCH 1.5 REMOVED VAR
mss1    plform  Error
.START PATCH 1.3 - REMMED CODE
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
......
font6   font
.END PATCH 1.3 - REMMED CODE
        formload mss1
.START PATCH 1.3 - REMMED CODE
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
;        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=9
        create  font5,"Arial",size=10
        create  font6,"Arial",size=14
.END PATCH 1.3 - REMMED CODE
.START PATCH 1.81 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
.END PATCH 1.81 ADDED LOGIC

.START PATCH 1.5 REMOVED VAR
..Create Header Logo
.. note update winrev to make sure users have directory and files
.        CREATE  PICT1=1:15:1:10:
.                "c:\progra~1\nincal\HEADER.BMP",AUTOZOOM
.        CREATE  PICT2=20:35:20:30:
.                "c:\progra~1\nincal\FOOTER.BMP",AUTOZOOM
.END PATCH 1.5 REMOVED VAR
        match   "NLCR0041",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0041",PROGRAM
        else
.START PATCH 1.63 REPLACED LOGIC
.               unpack  inpname,input2Name
                scan    ":",inpname
                if not equal
                        reset   inpname
                        pack    input2Name,NTWKPATH3,inpname
                else
                        reset   inpname
                        unpack  inpname,input2Name
                endif
.END PATCH 1.63 REPLACED LOGIC
                move    prtname,PrintFlag
.START PATCH 1.6 REPLACED LOGIC
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
                move    user,userlogn
                call    Trim using userlogn
.END PATCH 1.6 REPLACED LOGIC
        endif
        move    "NLCR0041",WPrognme
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
;begin patch 1.67
                    call                GetWinVer
;        getinfo system,str6
;        unpack  str6 into str1,str1
;        move    C0,osflag
;        if (str1 = "3" or str1 = "4")
;                move    C1,osflag
;.START PATCH 1.53 REPLACED LOGIC
;.        elseif (str1 = "1")
;        elseif (str1 = "1" OR str1 = "5")
;.END PATCH 1.53 REPLACED LOGIC
;                move    C2,osflag
;end patch 1.67
;        endif
.Set Flags to Open NINORD.DAT
        move    C0,NORDFLAG
        move    C0,NORDFLG2

.START PATCH 1.2 REPLACED LOGIC
.        call    Paint
.        DISPLAY *P1:24,"OPENING FILES";
.        GOTO    PRINTFILE
        if (PrintFlag <> 2)
                call    Paint
                DISPLAY *P1:24,"OPENING FILES";
        endif
.END PATCH 1.2 REPLACED LOGIC

.Open Files
        open    input2,input2Name
.START PATCH 1.2 REPLACED LOGIC
.        prepare input3,"G:\DATA\BRKFILE2.DAT"
.        DISPLAY *P1:24,"READING BRKFILE.DAT FILE";
        prepare input3,"C:\WORK\BRKFILE2.DAT"
        if (PrintFlag <> 2)
                DISPLAY *P1:24,"READING BRKFILE.DAT FILE";
        endif
.END PATCH 1.2 REPLACED LOGIC

.        goto printfile
.Prep file before reading it sequentially!!
.FILE MUST EITHER BE REFORMATTED AND REINDEXED PRIOR TO THIS JOB OR DONE RIGHT HERE!!!!!
LCRLoop
        loop
                move    "LCRLoop-readseq,input3",Location
                READ    input2,SEQ;ORDVARS
                until over
                call    OrderReadOtherFiles
                filepi  1;input3
.START PATCH 1.4 REPLACED LOGIC
.                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE
.START PATCH 1.52 REPLACED LOGIC
.                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST
.START PATCH 1.64 REPLACED LOGIC
.                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,O2DES
.START PATCH 1.7 ADDED LOGIC
.                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,O2DES
                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME
.END PATCH 1.7 ADDED LOGIC
.END PATCH 1.64 REPLACED LOGIC
.END PATCH 1.52 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
        repeat
.START PATCH 1.2 REPLACED LOGIC
.        DISPLAY *P1:24,"SORTING BRKFILE2 FILE ";
        if (PrintFlag <> 2)
                DISPLAY *P1:24,"SORTING BRKFILE2 FILE ";
        endif
.END PATCH 1.2 REPLACED LOGIC
SortFile
        clear   taskname
.START PATCH 1.2 REPLACED LOGIC
.        move    "BRKFILE2.DAT,g:\data\BRKFILE2.SRT,G:\DATA;1-7,8-11,12-17",taskname
        move    "C:\WORK\BRKFILE2.DAT,C:\WORK\BRKFILE2.SRT;1-7,8-11,12-17",taskname
.END PATCH 1.2 REPLACED LOGIC
        reset   taskname
.START PATCH 1.87 REPLACED LOGIC
.        sort    taskname
.        if over
.                move    s$error$,error
.                move    "Sort did not work!",Location
.                clear	KeyLocation
.                call    IOMssg
.                shutdown
.        endif
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
		shutdown
	endif
.END PATCH 1.87 REPLACED LOGIC
        clear   taskname
.START PATCH 1.2 REPLACED LOGIC
.        CALL    PAINT
.        DISPLAY *P1:24,"PRINTING FILES        ";
        if (PrintFlag <> 2)
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
        endif
.END PATCH 1.2 REPLACED LOGIC
.        shutdown
PrintFile
.Set up columns
        move    "500",column
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
        move    "6750",column7
.Initialize HLDBRK
        clear   HOLDBRK
.close unsorted file
        close   input3
.open newly sorted file
.START PATCH 1.2 REPLACED LOGIC
.        open    input3,"G:\DATA\BRKFILE2.SRT"
        open    input3,"C:\WORK\BRKFILE2.SRT"
.END PATCH 1.2 REPLACED LOGIC
        loop
.START PATCH 1.4 REPLACED LOGIC
.                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE
.START PATCH 1.52 REPLACED LOGIC
.                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST
.START PATCH 1.64 REPLACED LOGIC
.                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC002,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,O2DES
.START PATCH 1.7 REPLACED LOGIC
.                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
.                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
.                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,O2DES
                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.64 REPLACED LOGIC
.END PATCH 1.52 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
                goto    LastRec if over
                if (NBRKFLD <> HOLDBRK | OMLRNUM <> HOLDMAIL)
                        if (FIRST = NO)
                                PRTCLOSE prfile
                                if (PrintFlag = 2)
.START PATCH 1.6 REPLACED LOGIC
.                                        call    OrderCreatePDFFile
..................
.It takes some time for the file to be created, so we must check
.Allow 20 seconds to originally load DISTILLER
                                        clock   timestamp,timestamp1
                                        move    timestamp1,time1
                                        loop
                                                clock   timestamp,timestamp2
                                                move    timestamp2,time2
                                                sub     time1,time2,time3
                                                if (time3 > 2000) .20 Seconds Maximum
                                                        break
                                                endif
                                        repeat
                                        call    OrderCreatePDFFile
.END PATCH 1.6 REPLACED LOGIC
.START PATCH 1.61 ADDED LOGIC
.START PATCH 1.84 REPLACED LOGIC
.                                elseif (PrintFlag = 4 AND PrtFlag = NO)
                                elseif (PrintFlag = 5 AND PrtFlag = NO)
.END PATCH 1.84 REPLACED LOGIC
                                        clear   taskname
.START PATCH 1.68 REPLACED LOGIC
.                                        Path    Exist,"c:\windows"
.                                        if      over
.                                                append  "c:\winnt\system32\cmd.exe",taskname
.                                        else
.                                                append  "!c:\command.com",taskname
.                                        endif
					Path	Exist,"c:\windows"
					if over			.nt/2000
						append	"!c:\winnt\system32\cmd.exe",taskname
					elseif (osflag = c6)	.XP
						append	"!c:\windows\system32\cmd.exe",taskname
					else			.95/98
						append	"!c:\command.com",taskname
					endif
.END PATCH 1.68 REPLACED LOGIC
.                                        append  " /c copy f:\data\fax\hdrfile.prn /b + f:\data\fax\faxfile.prn /b f:\data\fax\fax",taskname
                                        append  " /c copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\",taskname
                                        append  HOLDBRK,taskname
                                        append  HOLDMAIL,taskname
                                        append  ".prn /b",taskname
                                        reset   taskname
                                        execute taskname
                                        clear   taskname
.START PATCH 1.68 REPLACED LOGIC
.                                        Path    Exist,"c:\windows"
.                                        if      over
.                                                append  "c:\winnt\system32\cmd.exe",taskname
.                                        else
.                                                append  "!c:\command.com",taskname
.                                        endif
					Path	Exist,"c:\windows"
					if over			.nt/2000
						append	"!c:\winnt\system32\cmd.exe",taskname
					elseif (osflag = c6)	.XP
						append	"!c:\windows\system32\cmd.exe",taskname
					else			.95/98
						append	"!c:\command.com",taskname
					endif
.END PATCH 1.68 REPLACED LOGIC
.                                        append  " /c copy f:\data\fax\fax",taskname
                                        append  " /c copy c:\work\",taskname
                                        append  HOLDBRK,taskname
                                        append  HOLDMAIL,taskname
                                        append  ".prn \\nts2\fax",taskname
                                        reset   taskname
                                        execute taskname
.START PATCH 1.61 ADDED LOGIC
                                endif
                                call    OrderOpenFile
                        else
                                move    NO,FIRST
                                call    OrderOpenFile
                        endif
                        move    NBRKFLD,HOLDBRK
                        move    OMLRNUM,HOLDMAIL
                        clear   page
                        call    OrderPrintHeader
                        call    OrderMailHeader
                endif
                call    OrderPrintRecord
        repeat
LastRec
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
        if (PrintFlag = 2)
.START PATCH 1.6 REPLACED LOGIC
.                call    OrderCreatePDFFile
.......................
.It takes some time for the file to be created, so we must check
                move    C0,N9
                move    "                                        ",APIFileName
                clear   APIFileName
.START PATCH 1.82 REPLACED LOGIC
.                pack    APIFileName,"C:\WORK\",str55,hexzero
                pack    APIFileName,"C:\WORK\PDF\",str55,hexzero	."
.END PATCH 1.82 REPLACED LOGIC
                clock   timestamp,timestamp1
                move    timestamp1,time1
                loop
                        clock   timestamp,timestamp2
                        move    timestamp2,time2
                        sub     time1,time2,time3
                        if (time3 > 2000) .20 Seconds Maximum
                                break
                        endif
                repeat
                call    OrderCreatePDFFile
.START PATCH 1.61 ADDED LOGIC
.START PATCH 1.84 REPLACED LOGIC
.        elseif (PrintFlag = 4 AND PrtFlag = NO)
        elseif (PrintFlag = 5 AND PrtFlag = NO)
.END PATCH 1.84 REPLACED LOGIC
                clear   taskname
.START PATCH 1.68 REPLACED LOGIC
.		Path    Exist,"c:\windows"
.		if      over
.			append  "c:\winnt\system32\cmd.exe",taskname
.		else
.			append  "!c:\command.com",taskname
.		endif
		Path	Exist,"c:\windows"
		if over			.nt/2000
			append	"!c:\winnt\system32\cmd.exe",taskname
		elseif (osflag = c6)	.XP
			append	"!c:\windows\system32\cmd.exe",taskname
		else			.95/98
			append	"!c:\command.com",taskname
		endif
.END PATCH 1.68 REPLACED LOGIC
.                append  " /c copy f:\data\fax\hdrfile.prn /b + f:\data\fax\faxfile.prn /b f:\data\fax\fax",taskname
                append  " /c copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\",taskname
                append  HOLDBRK,taskname
                append  HOLDMAIL,taskname
                append  ".prn /b",taskname
                reset   taskname
                execute taskname
                clear   taskname
.START PATCH 1.68 REPLACED LOGIC
.		Path    Exist,"c:\windows"
.		if      over
.			append  "c:\winnt\system32\cmd.exe",taskname
.		else
.			append  "!c:\command.com",taskname
.		endif
		Path	Exist,"c:\windows"
		if over			.nt/2000
			append	"!c:\winnt\system32\cmd.exe",taskname
		elseif (osflag = c6)	.XP
			append	"!c:\windows\system32\cmd.exe",taskname
		else			.95/98
			append	"!c:\command.com",taskname
		endif
.END PATCH 1.68 REPLACED LOGIC
.                append  " /c copy f:\data\fax\fax",taskname
                append  " /c copy c:\work\",taskname
                append  HOLDBRK,taskname
                append  HOLDMAIL,taskname
                append  ".prn \\nts2\fax",taskname
                reset   taskname
                execute taskname
.START PATCH 1.61 ADDED LOGIC
.END PATCH 1.6 REPLACED LOGIC
        endif
.START PATCH 1.2 - REPLACED CODE
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del G:\DATA\BRKFILE2.DAT",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del G:\DATA\BRKFILE2.SRT",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        if (PrintFlag = 2)
.                append  " /c del g:\data\PDFFILE.PRN",taskname
.        else
.                append  " /c del f:\data\fax\faxfile.prn",taskname
.        endif
.        reset   taskname
.        execute taskname
.............
        pack    APIFileName,"C:\WORK\BRKFILE2.DAT",hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
.
        pack    APIFileName,"C:\WORK\BRKFILE2.SRT",hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
.
        if (PrintFlag = 2)
.START PATCH 1.82 REPLACED LOGIC
.                pack    APIFileName,"C:\WORK\PDFFILE.PRN",hexzero
                pack    APIFileName,"C:\WORK\PDF\PDFFILE.PRN",hexzero
.END PATCH 1.82 REPLACED LOGIC
.START PATCH 1.61 ADDED LOGIC
.START PATCH 1.84 REPLACED LOGIC
.        elseif (PrintFlag = 4)
        elseif (PrintFlag = 5)
.END PATCH 1.84 REPLACED LOGIC
                pack    APIFileName,"C:\work\faxfile.prn",hexzero
.END PATCH 1.61 ADDED LOGIC
        else
.START PATCH 1.65 ADDED LOGIC
.                pack    APIFileName,"f:\data\fax\faxfile.prn",hexzero
.START PATCH 1.83 REPLACED LOGIC
.                pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 1.83 REPLACED LOGIC
.END PATCH 1.65 ADDED LOGIC
        endif
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
        clear   taskname
        clear   str55
.START PATCH 1.61 ADDED LOGIC
        clear   str50
.END PATCH 1.61 ADDED LOGIC
.START PATCH 1.68 REPLACED LOGIC
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.                append  "c:\winnt\system32\cmd.exe",str55
..START PATCH 1.61 ADDED LOGIC
.                append  "c:\winnt\system32\cmd.exe",str50
..END PATCH 1.61 ADDED LOGIC
.        else
.                append  "!c:\command.com",taskname
.                append  "!c:\command.com",str55
..START PATCH 1.61 ADDED LOGIC
.                append  "!c:\command.com",str50
..END PATCH 1.61 ADDED LOGIC
.        endif
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe",taskname
		append	"!c:\winnt\system32\cmd.exe",str55
		append	"!c:\winnt\system32\cmd.exe",str50
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe",taskname
		append	"!c:\windows\system32\cmd.exe",str55
		append	"!c:\windows\system32\cmd.exe",str50
	else			.95/98
		append	"!c:\command.com",taskname
		append	"!c:\command.com",str55
		append	"!c:\command.com",str50
	endif
.END PATCH 1.68 REPLACED LOGIC
.START PATCH 1.82 REPLACED LOGIC
.        append  " /c del c:\work\*.pdf",taskname
.        append  " /c del c:\work\*.log",str55
        append  " /c del c:\work\PDF\*.pdf",taskname
        append  " /c del c:\work\PDF\*.log",str55
.END PATCH 1.82 REPLACED LOGIC
.START PATCH 1.61 ADDED LOGIC
        append  " /c del c:\work\*.prn",str50
        reset   str50
        execute str50
.END PATCH 1.61 ADDED LOGIC
        reset   taskname
        reset   str55
        execute taskname
        execute str55
.END PATCH 1.2 - REPLACED CODE
        shutdown

OrderCreatePDFFile
.START PATCH 1.6 REPLACED LOGIC
.        clear   taskname
.        clear   str25
.        append  HOLDBRK,str25
.        append  "_",str25
.        append  HOLDMAIL,str25
.        append  ".pdf",str25
.        reset   str25
..        pack    taskname,"!f:\apps\pcl2pdf\pcl2pdf32 C:\WORK\pdffile.prn ",FilePath,str25
.        append  "!f:\apps\pcl2pdf\pcl2pdf32 C:\WORK\pdffile.prn C:\WORK\",taskname
.        append  str25,taskname
.        reset   taskname
.        execute taskname
..
.        move    "Here is your PDF File",SmtpSubject Subject
..   Set the text message that is send with the attachments
.        move    str25,SmtpTextMessage(1)   Array <Text message >
.        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.        move    "NTS4",SmtpEmailServer                   Address of email serverc
.        clear   smtpemailaddress
.        append  userlogn,SmtpEmailAddress
.        append  "@nincal.com",SmtpEmailAddress
.        reset   smtpemailaddress
.        move    userlogn,SmtpUserName                                User name
..   Set the destinations of the email. Max 100 (Mime spec)
.        move    smtpemailaddress,SmtpDestinations(1,1)
.        move    userlogn,SmtpDestinations(1,2)                          originators UserName
.        move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
.        move    str25,SmtpAttachments(1,1)                        Attached file name
.        move    "C:\WORK",SmtpAttachments(1,2)           Path to attached file name
.        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.        clear   SmtpLogFile                                         'Clear' disables the LogFile
.        move    "1",SmtpProgress                                    Enable progress bars
.        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.        if not equal
.                pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                        "Status Code ",SmtpStatus," - ",SmtpStatusText
.                move    "PDF File not found",SmtpSubject Subject
.                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..                Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
.        else
..                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
.        endif
..Clean up afterwards
..START PATCH 1.2 - REPLACE LOGIC
..        clear   taskname
..        Path    Exist,"c:\windows"
..        if      over
..                append  "c:\winnt\system32\cmd.exe",taskname
..        else
..                append  "!c:\command.com",taskname
..        endif
..        append  " /c del G:\DATA\",taskname
..        append  str25,taskname
..        reset   taskname
..        execute taskname
.        pack    APIFileName,"C:\WORK\",str25,hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.
.        endif
..END PATCH 1.2 - REPLACED LOGIC
.        return
........................
        move    "Here is your PDF File",SmtpSubject Subject
.   Set the text message that is send with the attachments
        move    str55,SmtpTextMessage(1)   Array <Text message >
        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
        move    "NTS4",SmtpEmailServer                   Address of email serverc
        clear   smtpemailaddress
        append  userlogn,SmtpEmailAddress
        append  "@nincal.com",SmtpEmailAddress
        reset   smtpemailaddress
        move    userlogn,SmtpUserName                                User name
.   Set the destinations of the email. Max 100 (Mime spec)
        move    smtpemailaddress,SmtpDestinations(1,1)
        move    userlogn,SmtpDestinations(1,2)
        move    "1",SmtpDestIndexLast                          originators UserName
        move    str55,SmtpAttachments(1,1)                     Attached file name
.START PATCH 1.82 REPLACED LOGIC
.        move    "C:\WORK",SmtpAttachments(1,2)           Path to attached file name
.        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
        move    "C:\WORK\PDF",SmtpAttachments(1,2)           Path to attached file name
        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
        move    "C:\work\PDF\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.END PATCH 1.82 REPLACED LOGIC
        clear   SmtpLogFile                                         'Clear' disables the LogFile
        move    "1",SmtpProgress                                    Enable progress bars
        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
        if not equal
                pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                        "Status Code ",SmtpStatus," - ",SmtpStatusText
                move    "PDF File not found",SmtpSubject Subject
                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
        else
.                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
        endif
        return
.END PATCH 1.6 REPLACED LOGIC
OrderOpenFile
.Print newly sorted file

        if (PrintFlag = 2)
.START PATCH 1.6 REPLACED LOGIC
.                PRTOPEN prfile,"faxfile2","pdffile.prn"
                clear   str25
                append  NBRKFLD,str25
                append  "_",str25
                append  userlogn,str25
                reset   str25
.START PATCH 1.82 REPLACED LOGIC
.                PRTOPEN prfile,"Acrobat Distiller",str25
                PRTOPEN prfile,"PDF995",str25
.END PATCH 1.82 REPLACED LOGIC
                pack    str55,str25,".pdf"
.END PATCH 1.6 REPLACED LOGIC
.START PATCH 1.51 REPLACED LOGIC
.        else
        elseif (PrintFlag = 3)
;begin patch 1.67
;                if (osflag = c2)         .nt
                if (osflag = c1 | osflag = C5 | osflag = c6)
                        PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
;                elseif (osflag = c1)         .win 95 98
                elseif (osflag = c3 | osflag = c4 )         .win 95 98
                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
;end patch 1.67
                else   .(osflag = c0)         .Don't know prompt for printer
.START PATCH 1.63 REPLACED LOGIC
.                        PRTOPEN prfile,"@","FAXFILE.PRN"
                        PRTOPEN prfile,"-","FAXFILE.PRN"
.END PATCH 1.63 REPLACED LOGIC
                endif
.START PATCH 1.61 ADDED LOGIC
.START PATCH 1.84 REPLACED LOGIC
.        elseif (PrintFlag = 4)
        elseif (PrintFlag = 5)
.END PATCH 1.84 REPLACED LOGIC
.START PATCH 1.64 ADDED LOGIC
.Set default to print
                move    YES,PrtFlag             .PRINT IT
.END PATCH 1.64 ADDED LOGIC
                move    BRFAX,faxnum
                match   "0000000000",faxnum
                if      equal
.START PATCH 1.64 REMOVED LOGIC
.                        move    YES,PrtFlag             .PRINT IT
.END PATCH 1.64 REMOVED LOGIC
                else
                        type    faxnum
                        if not equal
.START PATCH 1.64 REMOVED LOGIC
.                                move    YES,PrtFlag     .PRINT IT
.END PATCH 1.64 REMOVED LOGIC
                        else
.START PATCH 1.64 REMOVED LOGIC
.                                move    NO,PrtFlag      .FAX IT
.END PATCH 1.64 REMOVED LOGIC
                                count   N2,faxnum
                                compare C10,N2
                                if equal
.START PATCH 1.64 ADDED LOGIC
                                        move    NO,PrtFlag      .FAX IT
.END PATCH 1.64 ADDED LOGIC
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
..START NEW TEST - PREVENT FAXES FROM BEING CREATED
.                MOVE    YES,PrtFlag
..END NEW TEST
                if (PrtFlag = YES)
;begin patch 1.67
;                        if (osflag = c2)         .nt
                        if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
                                PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","FAXFILE.PRN"
;                        elseif (osflag = c1)         .win 95 98
                        elseif (osflag = c3 | osflag = c4)         .win 95 98
;end patch 1.67
                                PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
.START PATCH 1.63 REPLACED LOGIC
.                                PRTOPEN prfile,"@","FAXFILE.PRN"
                                PRTOPEN prfile,"-","FAXFILE.PRN"
.END PATCH 1.63 REPLACED LOGIC
                        endif
                else
..START TEST - PREVENT FILES FROM ACTUALLY BEING SENT OUT TO CLIENTS
.                        move    C1,LONGDIST
.                        MOVE    "4154337796",FAXNUM
..END TEST
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.START PATCH 1.64 ADDED LOGIC
                        pack    APIFileName,"c:\work\HDRFILE.prn",hexzero
                        call    DeleteFile
                        if (APIResult = 0 | APIResult = hexeight)

                        endif
.END PATCH 1.64 ADDED LOGIC
                        SPLOPEN "c:\work\HDRFILE.prn"
                        print   "^[D",longdist,faxnum,"^[N",brcomp:
                                "^[SList Management"," ^]"
                        SPLCLOSE
                        PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
                endif
.END PATCH 1.61 ADDED LOGIC
        else
.END PATCH 1.51 REPLACED LOGIC
.                if (osflag = c2)         .nt
.                        PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
.                elseif (osflag = c1)         .win 95 98
.                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"@","FAXFILE.PRN"
.                endif
;begin patch 1.67
;                if (osflag = c2)         .nt
                if (osflag = c1 | osflag = c5 | osflag = C6)         .nt
                        PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","FAXFILE.PRN"
;                elseif (osflag = c1)         .win 95 98
                elseif (osflag = c3 | osflag = c4)         .win 95 98
                        PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
;end patch 1.67
                else   .(osflag = c0)         .Don't know prompt for printer
.START PATCH 1.63 REPLACED LOGIC
.                        PRTOPEN prfile,"@","FAXFILE.PRN"
                        PRTOPEN prfile,"-","FAXFILE.PRN"
.END PATCH 1.63 REPLACED LOGIC
                endif
        endif
        ADD     C1,COUNTR
.START PATCH 1.2 REPLACED LOGIC
.        DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
        if (PrintFlag <> 2)
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
        endif
.END PATCH 1.2 REPLACED LOGIC
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
.Broker File
        pack    NBRKFLD,OBRKNUM,OBRKCNT
        rep     zfill,NBRKFLD
        move    "Driver-NBRKKEY,1rst",Location
        call    NBRKKEY
        if over
                pack   BRCOMP,"UNKNOWN",B55
        endif
.Special Instructions
.START PATCH 1.64 REPLACED LOGIC
.        move    OLRN,NSPEFLD
.        rep     zfill,NSPEFLD
.        move    C3,NSPELOCK
.        move    "Driver-NSPEKEY,1rst",Location
.        call    NSPEKEY
.        call    Trim Using DESC001
        move    OLRN,NSPE2FLD
        rep     zfill,NSPE2FLD
        move    C3,NSPE2LOCK
        move    "Driver-NSPE2KEY,1rst",Location
        call    NSPE2KEY
.END PATCH 1.64 REPLACED LOGIC
.Contact
        move    C1,NCNTPATH
        move    OCOCODE,NCNTFLD
        move    "NCNTKEY",Location
        call    NCNTKEY
        if over
                pack    CNTNAME,B55
                pack    CNTPHONE,B55
        endif
.Data Card Universe
        clear   UNIVERSE
.START PATCH 1.85 REPLACED LOGIC - TEMPORARY PATCH
.        move    MKEY,NXRFFLD2
	move	OMLRNUM,COMPFLD3
	move	"COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	move	COMPNUM,NXRFFLD2
.END PATCH 1.85 REPLACED LOGIC - TEMPORARY PATCH
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
.START PATCH 1.7 ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 1.7 ADDED LOGIC
        return

.Print Heading
OrderPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
.START PATCH 1.5 - REPLACED LOGIC
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=1:15000:1:8750:PICT1
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
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
	prtpage	prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
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
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
.END PATCH 2.04 REPLACED LOGIC
.END PATCH 1.5 - REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"DATE:  ";
.        prtpage prfile;*p6000:row,newdate1;
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,newdate1;
.END PATCH 1.69 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
        prtpage prfile;*p1000:row,BRCNTCT;
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"FROM:  ";
        prtpage prfile;*p5200:row,"FROM:  ";
.END PATCH 1.69 REPLACED LOGIC
.        call    Trim using CNTNAME
.        if (CNTNAME = "")
                move    "List Management",CNTNAME
                move    "(415) 989-3350",CNTPHONE
.        endif
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p6000:row,CNTNAME;
        prtpage prfile;*p5700:row,CNTNAME;
.END PATCH 1.69 REPLACED LOGIC
        add     eightlpi,row
        prtpage prfile;*p1000:row,BRCOMP;
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p6000:row,"Names In The News, CA";
        prtpage prfile;*p5700:row,"Names In The News";
.END PATCH 1.69 REPLACED LOGIC
        add     eightlpi,row
        call    Trim using BRTELE
        if (BRTELE <> "")
                unpack  BRTELE,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p6000:row,CNTPHONE;
        prtpage prfile;*p5700:row,CNTPHONE;
.END PATCH 1.69 REPLACED LOGIC
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"FAX:";
        call    Trim using BRFAX
        if (BRFAX <> "")
                unpack  BRFAX,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p5500:row,"FAX:  ";
.        prtpage prfile;*p6000:row,"(415) 433-7796";
        prtpage prfile;*p5200:row,"FAX:  ";
.patch 1.69.1
.       prtpage prfile;*p5700:row,"(415) 433-7796";
        prtpage prfile;*p5700:row,"(510) 628-8313";
.patch 1.69.1
.END PATCH 1.69 REPLACED LOGIC
        add     eightlpi,row
        move    CNTNAME,str1
cntloopy
        bump    CNTNAME,1
        cmatch  B1,CNTNAME
        goto    cntloopy if not equal
        goto    cntexit if eos
        bump    CNTNAME,1
.START PATCH 1.51 REPLACED LOGIC
.        move    CNTNAME,str6
        move    CNTNAME,str7
        call    RemoveChar using str7,B1
        move    str7,str6
.END PATCH 1.51 REPLACED LOGIC
        clear   str24
        pack    str24,str1,str6,"@NINCAL.COM"
cntexit reset   CNTNAME
.        prtpage prfile;*p6000:row,str24;
.START PATCH 1.64 REPLACED LOGIC
.        prtpage prfile;*p6000:row,"GKetter@NINCAL.COM";
.START PATCH 1.64 REPLACED LOGIC
.        prtpage prfile;*p6000:row,"JHansen@NINCAL.COM";
.START PATCH 1.69 REPLACED LOGIC
.        prtpage prfile;*p6000:row,"KathyObee@NINCAL.COM";
.START PATCH 1.86 REPLACED LOGIC
.        prtpage prfile;*p5700:row,"ListManagementClearances@nincal.com";
        prtpage prfile;*p5700:row,"www.ninlists.com";
.END PATCH 1.86 REPLACED LOGIC
.END PATCH 1.69 REPLACED LOGIC
.END PATCH 1.64 REPLACED LOGIC
.END PATCH 1.64 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p2800:row,*font=font1,*boldon,"LIST CLEARANCE REPORT",*boldoff;
.START PATCH 1.86 REPLACED LOGIC
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
	add     eightlpi,row
	prtpage prfile;*p5600:row,*font=font2,*boldon,"If approved on Rental please refer";
	add     eightlpi,row
	prtpage prfile;*p5600:row,"to our Datacard on www.ninlists.com";
	add     eightlpi,row
	prtpage prfile;*p5600:row,"for pricing.",*boldoff;
	add     eightlpi,row
.END PATCH 1.86 REPLACED LOGIC
        return

OrderMailHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Mailer:  ",*boldoff,MCOMP;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NINCA ##/";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"List/";
        prtpage prfile;*pcolumn2:row,"Mail Date";
.START PATCH 1.52 REPLACED LOGIC
.        prtpage prfile;*pcolumn4:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Quantity/";
.END PATCH 1.52 REPLACED LOGIC
        prtpage prfile;*pcolumn6:row,"Answer";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Broker ##";
.START PATCH 1.52 ADDED LOGIC
        prtpage prfile;*pcolumn4:row,"Select";
.END PATCH 1.52 ADDED LOGIC
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

OrderPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderMailHeader
        endif
.START PATCH 1.51 REPLACED LOGIC
.        if (OSTAT = "p")
.Should never need to list a Cancelled Pending Order, but in case you do logic will now allow it
        if (OSTAT = "p" | OSTAT = "x")
.END PATCH 1.51 REPLACED LOGIC
                prtpage prfile;*pcolumn:row,*font=font2,*boldon,OLRN,B1,"Pending Order",*boldoff;
        else
                prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN;
        endif
        move    row,N10
        move    row,result
        add     eightlpi,N10
        add     eightlpi,N10
        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6750;
        move    column6,N9
        add     "70",N9
        move    row,howmany
        add     "70",howmany
.START PATCH 1.4 REPLACED LOGIC
.        if (OCLRSTAT = "1")
.                prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
.        elseif (OCLRSTAT = "2")
.                prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
.        elseif (OCLRSTAT = "3")
.                prtpage prfile;*pN9:howmany,*font=font1,"EXC/RENT",*font=font2;
.        else
.                if (ORENT = "1")
.                        if (OELCODE = "2" OR OELCODE = "3")
.                                prtpage prfile;*pN9:howmany,*font=font1,"RENT/EXC",*font=font2;
.                        else
.                                prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
.                        endif
.                else
.                        prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
.                endif
.        endif
.
        if (OHIST = "z")
                prtpage prfile;*pN9:howmany,*font=font1,"DENIED",*font=font2;
        else
                if (OCLRSTAT = "1")
                        prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
                elseif (OCLRSTAT = "2")
                        prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
                elseif (OCLRSTAT = "3")
                        prtpage prfile;*pN9:howmany,*font=font1,"EXC/RENT",*font=font2;
                else
                        if (ORENT = "1")
                                if (OELCODE = "2" OR OELCODE = "3")
                                        prtpage prfile;*pN9:howmany,*font=font1,"RENT/EXC",*font=font2;
                                else
                                        prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
                                endif
                        else
                                prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
                        endif
                endif
        endif
.END PATCH 1.4 REPLACED LOGIC
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,O1DES;
        call    TRIM using OMDTEM
        count   N2,OMDTEM
        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                prtpage prfile;*pcolumn2:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                prtpage prfile;*pcolumn2:row,"As Soon As Possible";
        endif
.        if (OELCODE = "1" OR OELCODE = "3")
.                move    "All",EditQuan
.        else
                move    EditMask,EditQuan
                move    C0,N9
                move    OQTY,N9
                edit    N9,EditQuan
.        endif
        prtpage prfile;*pcolumn4:row,EditQuan;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OMLRPON;
.START PATCH 1.52 ADDED LOGIC
.START PATCH 1.7 REPLACED LOGIC
.        prtpage prfile;*pcolumn4:row,O2DES;
        prtpage prfile;*pcolumn4:row,NSEL2NAME;
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.52 ADDED LOGIC
        call    PrintSpecialInstructions
        add     eightlpi,row
        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
.START PATCH 1.64 REPLACED LOGIC
.        call    TRIM using DESC002
.        if (DESC002 <> "")
.                pack    str2,carr,B1
.                rep     str2,DESC002
.                move    C0,howmany
.                move    NO,str1
.                loop
.                        call    PARSITUP using line1,DESC002
.
        call    TRIM using DESC004
        if (DESC004 <> "")
                pack    str2,carr,B1
                rep     str2,DESC004
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC004,C1
.END PATCH 1.64 REPLACED LOGIC
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

.Include IO file
        include nordio.inc
        include nord5io.inc
        include nord4io.inc
        include nordPio.inc
;patch1.8
	include	compio.inc
	include	cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
;patch1.8
        include nxrfio.inc
        include ndatio.inc
        include npndio.inc
.START PATCH 1.64 REPLACED LOGIC
.        include nspeio.inc
        include nspe2io.inc
.END PATCH 1.64 REPLACED LOGIC
        include ncntio.inc
.START PATCH 1.7 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 1.7 ADDED LOGIC
        include comlogic.inc

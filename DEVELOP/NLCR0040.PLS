........................................
. Program:      NLCR0040.PLS
. Function:     Planner Report Program
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
.         include f:\library\develop\backups\nordpdd.inc
.Patch1.88
                              include   compdd.inc
                              include   cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
.Patch1.88
        include nxrfdd.inc
        include ndatdd.inc
        include npnddd.inc
        include nspedd.inc
        include ncntdd.inc
.START PATCH 1.7 - ADDED LOGIC
        include winapi.inc
.START PATCH 1.7 - ADDED LOGIC
.need to convert login to use PDF flag file logic
release  init      "2.01"        DLH    do not delete Denied but only include on report for 7 days
Reldate   Init      "2013 August 1"
.release  init      "2.00"        DLH    Sunbelt PDF
.Reldate   Init      "2013 April 24"
.release  init      "1.96"        DLH    Remove PLI
.Reldate   Init      "2009"
.release  init      "1.95"        DLH    Try and cleanup PDF Emails
.Reldate   Init      "05August2009"
.release  init      "1.94"        DLH    26Sep2007 PLI Cleanup
.release  init      "1.93"        ASH   13JUN2007 PLI Inclusion
.release  init      "1.92"        ASH   18MAY2005 LISTMLR Conversion
.release  init      "1.91"        ASH   09DEC2004 FAXFILE
.release  init      "1.90"        ASH   02DEC2004 Conversion from Acrobat Distiller to PDF995
.release  init      "1.89"        ASH   06AUG2004 Logo Conversion
.release  init      "1.88"        DMB   26MAY2004 Mailer Conversion
.release   init      "1.87"      20FEB2004        ASH       DATACARD CONVERSION
.release   init      "1.86"      12AUG2003        ASH       Patched email address logic
.release   init      "1.85"      12JUl2002        DLH       Use GetWinVer
.release   init      "1.84"      11JUN2002        ASH       REMOVE LIST MANAGEMENT FROM NIGHTLY JOB
.release   init      "1.83"      07FEB2002        ASH       IDEA OF THE MONTH - MODIFY SO THAT DENIED RECORDS WILL ONLY PRINT ONCE
.release   init      "1.82"      24MAY2001        ASH       REPLACED REFERENCES TO F:\USERS\
.release   init      "1.81"      02OCT2000        ASH     NEW SERVER ADDED

.release   init      "1.75"      16OCT000       ASH     SERVER SWITCHED TO NT5.  CORRECTED LOGIC WITH PRTOPEN
.release   init      "1.7"      05MAY2000       ASH     REPORT MODIFIED TO ONLY PRINT DENIED RECORDS
.release   init      "1.6"      01MAY2000       ASH     FIX TO REPORT - OSTATS OF "x" ARE PENDING ORDERS AS WELL
.                                                       FIX TO EXTRACT SPACES FROM LAST NAMES
.                                                       REPLACED CRITERIA FOR "Pending Order" verbage
.release   init      "1.5"       15MAR2000       ASH     RE-WORKED LOGIC FOR HEADER/FOOTER
.release   init      "1.4"       16FEB2000       ASH     LOGIC TO HOLD DENIED RECORDS FOR 5 DAYS
.release   init      "1.3"       10FEB2000       ASH     LOGIC TO SORT BY APPROVAL DATE (DESCENDING)
.release   init      "1.2"       28DEC99 ASH     NEW LOGIC FOR LIST MANAGEMENT, with new vars
.release   init      "1.1"       29Nov99 ASH     Replaced OTOCODE with ORENT
.release init    "1.0"   ASH 17SEP99  DEVELOPMENT RELEASE

.START PATCH 1.86 REPLACED LOGIC
str60     dim       60
.END PATCH 1.86 REPLACED LOGIC
.Counters
COUNTR  FORM    9
.Files to open
prfile  pfile
input3  file
First   init    "Y"
Caller  init    "Y"
.Some vars
HOLDCONT dim    2
HOLDMAIL dim    4
.START PATCH 1.93 ADDED LOGIC
HOLDEXCL dim        1
.END PATCH 1.93 ADDED LOGIC
newdate1 dim    10
page    form    9
date    dim     8
.START PATCH 1.87 REPLACED LOGIC
.EditMask init   "ZZZ,ZZZ,ZZZ"
.EditQuan dim    11
EditMask init   "Z,ZZZ,ZZZ,ZZZ"
EditQuan dim    13
.END PATCH 1.87 REPLACED LOGIC
line1   dim     100
Carr    init    0x7f
.begin patch 1.85
.osflag   form   1       .1=win 95,98, 2=NT
.begin patch 1.85
.
.START PATCH 1.7 - ADDED VARS
PrintFlag form  "00"    .Default is Laser3!!!
input2Name dim  40
input1  file
output1 dim     40
output2 dim     40
userinfo dim    500
userlogn dim    7
userlogw dim    7
.hexeight integer 4,"4294967295"
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.END PATCH 1.7 - ADDED VARS

.START PATCH 1.5 REMOVED VARS
.pict1   pict
.pict2   pict
.END PATCH 1.5 REMOVED VARS
mss1    plform  Error
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.....
font6   font
.START PATCH 1.93 ADDED LOGIC
font7     font
.END PATCH 1.93 ADDED LOGIC
.begin patch 1.95
PFname    DIm       55        .print file name
FileCheck FIle
trapcount form      4
.end patch 1.95

        formload mss1
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=9
        create  font5,"Arial",size=10
......
        create  font6,"Arial",size=14
......
.START PATCH 1.93 ADDED LOGIC
          create    font7,"Times New Roman",size=9
.END PATCH 1.93 ADDED LOGIC

.START PATCH 1.89 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.89 ADDED LOGIC

.START PATCH 1.5 REMOVED VARS
..Create Header Logo
.. note update winrev to make sure users have directory and files
.        CREATE  PICT1=1:15:1:10:
.                "c:\program files\nincal\HEADER.BMP",AUTOZOOM
.        CREATE  PICT2=20:35:20:30:
.                "c:\program files\nincal\FOOTER.BMP",AUTOZOOM
.END PATCH 1.5 REMOVED VARS

.START PATCH 1.7 - REPLACED LOGIC
.        move    "NLCR0040",WPrognme
        match   "NLCR0040",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0040",WPrognme
.START PATCH 1.8 REPLACED LOGIC
.                move    "G:\DATA\LCRFILE.DAT",output1
.                move    "G:\DATA\LCRFILE.SRT",output2
.path1 = \\server\drive\data        .general data directory for print and temp files
                PACK    OUTPUT1,NTWKPATH1,"LCRFILE.DAT"
                PACK    OUTPUT2,NTWKPATH1,"LCRFILE.SRT"
.END PATCH 1.8 REPLACED LOGIC
        else
                clear   input2Name
                append  inpname,input2Name
                append  ".DAT",input2Name
                reset   input2Name
                move    prtname,PrintFlag
                move    user,userlogn
                call    Trim using userlogn
                clear   output1
                clear   output2
.START PATCH 1.82 REPLACED LOGIC
.                append  "F:\USERS\",output1
.                append  "F:\USERS\",output2
                append  NTWKPATH3,output1
                append  NTWKPATH3,output2
.path 3 is \\server\drive\"users"\directory
.ENDTCH 1.82 REPLACED LOGIC
                if (userlogn <> "")
                        append  userlogn,output1
                        append  "\",output1
                        append  userlogn,output2
                        append  "\",output2
                endif
                append  "LCRFILE3.DAT",output1
                append  "LCRFILE3.SRT",output2
                reset   output1
                reset   output2
        endif
.END PATCH 1.7 - REPLACED LOGIC

.START PATCH 1.4 - REPLACED LOGIC
.        clock   date to date
.        unpack  date,MM,STR1,DD,STR1,YY
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
        call    CVTJUL
        move    JULDAYS,howmany
.END PATCH 1.4 - REPLACED LOGIC
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
.begin patch 1.85
.        getinfo system,str6
.        unpack  str6 into str1,str1
.        move    C0,osflag
.
.        if (str1 = "3" or str1 = "4")
.                move    C1,osflag
.;START PATCH 1.75 REPLACED LOGIC
.;        elseif (str1 = "1")
.        elseif (str1 = "1" OR STR1 = "5")
.;END PATCH 1.75 REPLACED LOGIC
.                move    C2,osflag
.        endif
                    call                GetWinVer
.End Patch 1.85

.START PATCH 1.7 - REPLACED LOGIC
..Set Flags to Open NINORD.DAT
.        move    C0,NORDFLAG
.        move    C0,NORDFLG2
.
.        call    Paint
.        DISPLAY *P1:24,"OPENING FILES";
..Open Files
.        prepare input3,"G:\DATA\LCRFILE.DAT"
.        DISPLAY *P1:24,"READING NINPRINTL FILE";
        if (PrintFlag = 0)
.Set Flags to Open NINORD.DAT
                move    C0,NORDFLAG
                move    C0,NORDFLG2
                call    Paint
                DISPLAY *P1:24,"OPENING FILES";
        endif
.          goto redo
.Open Files
        prepare input3,output1

        if (PrintFlag = 0)
                DISPLAY *P1:24,"READING NINPRINTL FILE";
        else
                DISPLAY *P1:24,"READING ",input2Name," FILE";
                open    input1,Input2Name
        endif
.END PATCH 1.7 - REPLACED LOGIC
.        goto printfile
.Prep file before reading it sequentially!!
.FILE MUST EITHER BE REFORMATTED AND REINDEXED PRIOR TO THIS JOB OR DONE RIGHT HERE!!!!!
LCRLoop
        loop
.START PATCH 1.7 - REPLACED LOGIC
.                move    "LCRLoop-NORDPSEQ",Location
.                call    NORDPSEQ
                if (PrintFlag = 0)
                        move    "LCRLoop-NORDPSEQ",Location
                        call    NORDPSEQ
                else
                        read    input1,SEQ;ORDVARS
                endif
.END PATCH 1.7 - REPLACED LOGIC
                until over
.START PATCH 1.7 - REPLACED LOGIC
.                if (OHIST = "E" | OHIST = "z")  .Only pull records that have been Cleared or Denied
..START PATCH 1.2 - NEW LOGIC - FIELD OUT RECORDS ALREADY SENT TO OUTSIDE BROKERS
.                        if (OSALES10 = "0" & OSALES = "6" & (OBRKRPT = "1" | OHIST = "E"))
..if these conditions are true, do not throw these records on Planner report
.                        else
..END PATCH 1.2 - NEW LOGIC - FIELD OUT RECORDS ALREADY SENT TO OUTSIDE BROKERS
.                                call    OrderReadOtherFiles
.                                call    OrderReadPendFiles
.                                filepi  1;input3
..START PATCH 1.1 - REPLACED LOGIC
..                        write   input3,seq;OCOCODE,OLNUM,OLRN,OMLRNUM,MCOMP,O1DES:
..                                OMDTEM,OMDTED,OMDTEC,OMDTEY,OTOCODE,OELCODE,OQTY,OCO2CODE:
..                                DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC:
..                                NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY
..START PATCH 1.2 - REPLACED LOGIC
..                                write   input3,seq;OCOCODE,OMLRNUM,OLRN,OLNUM,MCOMP,O1DES:
..                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
..                                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
..                                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
.                                write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
.                                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
..END PATCH 1.2 - REPLACED LOGIC
..END PATCH 1.1 - REPLACED LOGIC
..START PATCH 1.2 - NEW LOGIC - FIELD OUT RECORDS ALREADY SENT TO OUTSIDE BROKERS
.                        endif
..END PATCH 1.2 - NEW LOGIC - FIELD OUT RECORDS ALREADY SENT TO OUTSIDE BROKERS
.                endif
.        repeat
..Delete Denied In-House LCR's.  This is done here in order to maintain integrity of file pointer above.
.
.        clear   NORDPFLD
.        clear   NORDPFLD1
.        clear   NORDPFLD2
.        clear   NORDPFLD3
.        pack    NORDPFLD4,"04Xz"
.        move    C2,NORDPPATH
.        move    "LCRLoop-NORDPAIM",Location
.        call    NORDPAIM
.        if not over
.                if (OHIST = "z")        .Delete Denied records
..START PATCH 1.4 - REPLACED LOGIC
..                        pack    NORDPFLD,OLRN
..                        filepi  1;NORDPFLE
..                        delete  NORDPFLE
..                        filepi  1;NORDPFILE
..                        deletek NORDPFILE,NORDPFLD
..                        move    "LCRLoop-NORDPKG",Location
..
.                        move    OCLRDTEC,CC
.                        move    OCLRDTEY,YY
.                        move    OCLRDTEM,MM
.                        move    OCLRDTED,DD
.                        call    CVTJUL
.                        sub     JULDAYS,howmany,N9
.                        if (N9 >= 7)
.                                pack    NORDPFLD,OLRN
.                                filepi  1;NORDPFLE
.                                delete  NORDPFLE
.                                filepi  1;NORDPFILE
.                                deletek NORDPFILE,NORDPFLD
.                                move    "LCRLoop-NORDPKG",Location
.                        endif
..END PATCH 1.4 - REPLACED LOGIC
.                endif
.                loop
.                        call    NORDPKG
.                        until over
.                        if (OHIST = "z")        .Delete Denied records
..START PATCH 1.4 - REPLACED LOGIC
..                                pack    NORDPFLD,OLRN
..                                filepi  1;NORDPFLE
..                                delete  NORDPFLE
..                                filepi  1;NORDPFILE
..                                deletek NORDPFILE,NORDPFLD
..
.                                move    OCLRDTEC,CC
.                                move    OCLRDTEY,YY
.                                move    OCLRDTEM,MM
.                                move    OCLRDTED,DD
.                                call    CVTJUL
.                                sub     JULDAYS,howmany,N9
.                                if (N9 >= 7)
.                                        pack    NORDPFLD,OLRN
.                                        filepi  1;NORDPFLE
.                                        delete  NORDPFLE
.                                        filepi  1;NORDPFILE
.                                        deletek NORDPFILE,NORDPFLD
.                                endif
..END PATCH 1.4 - REPLACED LOGIC
.                        endif
.                repeat
.        endif
.        DISPLAY *P1:24,"SORTING LCRFILE FILE ";
.......................................
                if (PrintFlag = 0)
.START PATCH 1.84 ADDED LOGIC
                              pack      str2,OSALES10,OSALES
.begin patch 1.94
.                             if (str2 <> "06")
                              if (str2 <> "06" & Str2 <> "27" & Str2 <> "28")      .what about 19??
.end patch 1.94
.END PATCH 1.84 ADDED LOGIC
                                  if (OHIST = "z")  .Only pull records that have been Denied
.With patch 1.84, following line is obsolete.  Leave it here in case things change.  AH 6/11/2002
                                  if (OSALES10 = "0" & OSALES = "6" & (OBRKRPT = "1" | OHIST = "E"))
.if these conditions are true, do not throw these records on Planner report
                                    else
.START PATCH 1.83 REPLACED LOGIC
.                                        call    OrderReadOtherFiles
.                                        call    OrderReadPendFiles
.                                        filepi  1;input3
.                                        write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
.                                                OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                                DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
.                                                NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
                                                            if (ORCODE = "F")
                                                            call    OrderReadOtherFiles
                                                            call    OrderReadPendFiles
                                                          filepi  1;input3
.START PATCH 1.93 REPLACED LOGIC
.                                                       write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
.                                                             OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                                           DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
.                                                                   NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
                                                        write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
                                                              OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                                            DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
                                                                    NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP,HOLDEXCL
.END PATCH 1.93 REPLACED LOGIC
                                                            endif
.END PATCH 1.83 REPLACED LOGIC
                                  endif
                                  endif
.START PATCH 1.84 ADDED LOGIC
                              endif
.END PATCH 1.84 ADDED LOGIC
.begin patch 2.01
                else
                         if    (OHIST = "E") .Only pull records that have been Cleared
.                        if (OHIST = "E" | OHIST = "z")  .Only pull records that have been Cleared or Denied
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
                                filepi  1;input3
.START PATCH 1.93 REPLACED LOGIC
.                                write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
.                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
.                                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
                                write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
                                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP,HOLDEXCL
                         Elseif    (OHIST = "z")
                                        move    OCLRDTEC,CC
                                        move    OCLRDTEY,YY
                                        move    OCLRDTEM,MM
                                        move    OCLRDTED,DD
                                        call    CVTJUL
                                        sub     JULDAYS,howmany,N9
                                        if (N9 <= 7)
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
                                filepi  1;input3
                                write   input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
                                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
                                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP,HOLDEXCL
                                        endif
.end patch 2.01
.END PATCH 1.93 REPLACED LOGIC
                        endif
                endif
.END PATCH 1.7 - REPLACED LOGIC
        repeat
.START PATCH 1.7 - REPLACED LOGIC
.Delete Denied In-House LCR's.  This is done here in order to maintain integrity of file pointer above.
        if (PrintFlag = 0)
                clear   NORDPFLD
                clear   NORDPFLD1
                clear   NORDPFLD2
                clear   NORDPFLD3
                pack    NORDPFLD4,"04Xz"
                move    C2,NORDPPATH
                move    "LCRLoop-NORDPAIM",Location
                call    NORDPAIM
                if not over
                        if (OHIST = "z")        .Delete Denied records
                                move    OCLRDTEC,CC
                                move    OCLRDTEY,YY
                                move    OCLRDTEM,MM
                                move    OCLRDTED,DD
                                call    CVTJUL
                                sub     JULDAYS,howmany,N9
                                if (N9 >= 7)
                                        pack    NORDPFLD,OLRN
                                        filepi  1;NORDPFLE
                                        delete  NORDPFLE
                                        filepi  1;NORDPFILE
                                        deletek NORDPFILE,NORDPFLD
.START PATCH 1.83 ADDED LOGIC
                                        elseif (ORCODE = "F")
                                                  move      "s",ORCODE
                                            move    "LCRLoop-Update NORDP",Location
                                                  update    nordPFLE;ordvars
.END PATCH 1.83 ADDED LOGIC
                                endif
                        endif
                        loop
                                        move    "LCRLoop-NORDPKG",Location
                                call    NORDPKG
                                until over
.begin patch 2.01
.                                if (OHIST = "z")        .Delete Denied records
.                                        move    OCLRDTEC,CC
.                                        move    OCLRDTEY,YY
.                                        move    OCLRDTEM,MM
.                                        move    OCLRDTED,DD
.                                        call    CVTJUL
.                                        sub     JULDAYS,howmany,N9
..                                        if (N9 >= 7)
.                                        if (N9 >= 180)      .temp changed DH 2013 July 31
.                                                pack    NORDPFLD,OLRN
.                                                filepi  1;NORDPFLE
.                                                delete  NORDPFLE
.                                                filepi  1;NORDPFILE
.                                                deletek NORDPFILE,NORDPFLD
.START PATCH 1.83 ADDED LOGIC
.                                                  elseif (ORCODE = "F")
                                                  if (ORCODE = "F")
.end patch 2.01
                                                      move      "s",ORCODE
                                                      move    "LCRLoop-Update NORDP",Location
                                                  update    nordPFLE;ordvars
.END PATCH 1.83 ADDED LOGIC
                                        endif

.begin patch 2.01
.                                endif
.end patch 2.01
                        repeat
                endif
                DISPLAY *P1:24,"SORTING LCRFILE FILE ";
        endif
.END PATCH 1.7 - REPLACED LOGIC
SortFile
        clear   taskname
.START PATCH 1.3 - REPLACED LOGIC
.        move    "LCRFILE.DAT,g:\data\LCRFILE.SRT,G:\DATA;1-2,3-6,7-12",taskname
.START PATCH 1.7 - REPLACED LOGIC
.        move    "LCRFILE.DAT,g:\data\LCRFILE.SRT,G:\DATA;1-2,3-6,D13-20,A7-12",taskname
.        reset   taskname
        pack    taskname,output1,COMMA,output2,";1-2,3-6,D13-20,A7-12"
.END PATCH 1.7 - REPLACED LOGIC
.END PATCH 1.3 - REPLACED LOGIC
        sort    taskname
        if over
                move    s$error$,error
                move    "Sort did not work!",Location
                call    IOMssg
                stop
        endif

        clear   taskname
.START PATCH 1.7 - REPLACED LOGIC
.        CALL    PAINT
.        DISPLAY *P1:24,"PRINTING FILES        ";
        if (PrintFlag = 0)
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
        endif
.END PATCH 1.7 - REPLACED LOGIC
.        stop
PrintFile
.Set up columns
        move    "500",column
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
.        move    "6000",column6
        move    "7000",column7
.        move    "7200",column7
.Initialize HLDBRK
        clear   HOLDCONT
.close unsorted file
        close   input3
.open newly sorted file
.START PATCH 1.7 - REPLACED LOGIC
.        open    input3,"G:\DATA\LCRFILE.SRT"
REDO
        open    input3,output2
.END PATCH 1.7 - REPLACED LOGIC
        loop
.START PATCH 1.1 - REPLACED LOGIC
.                read    input3,seq;OCOCODE,OLNUM,OLRN,OMLRNUM,MCOMP,O1DES:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,OTOCODE,OELCODE,OQTY,OCO2CODE:
.                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC:
.                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY
.START PATCH 1.2 - REPLACED LOGIC
.                read    input3,seq;OCOCODE,OMLRNUM,OLRN,OLNUM,MCOMP,O1DES:
.                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
.                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP
.START PATCH 1.93 REPLACED LOGIC
                read    input3,seq;OCOCODE,OMLRNUM,OLRN,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,OLNUM,MCOMP,O1DES,OCLRSTAT:
                        OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                        DESC001,DESC002,CNTNAME,OHIST,str45,str15,UNIVERSE,NPNDDESC,NORD4STAT:
                        NORD5STAT,OODTEM,OODTED,OODTEC,OODTEY,OSTAT,OSALES10,OSALES,BRCOMP,HOLDEXCL
.END PATCH 1.93 REPLACED LOGIC
.END PATCH 1.2 - REPLACED LOGIC
.END PATCH 1.1 - REPLACED LOGIC
                goto    LastRec if over
                if (OCOCODE <> HOLDCONT OR OMLRNUM <> HOLDMAIL)
.                if (OCOCODE <> HOLDCONT)
                        if (FIRST = NO)
                                PRTCLOSE prfile
.START PATCH 1.7 - ADDED LOGIC
.This will only happen if called from Program 1
                                if (PrintFlag = 2)
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
                                endif
.END PATCH 1.7 - ADDED LOGIC
                                call    OrderOpenFile
                        else
                                move    NO,FIRST
                                call    OrderOpenFile
                        endif
                        move    OCOCODE,HOLDCONT
                        move    OMLRNUM,HOLDMAIL
                        clear   page
                        call    OrderPrintHeader
                        call    OrderMailHeader
.                elseif (OMLRNUM <> HOLDMAIL)
.                        move    OMLRNUM,HOLDMAIL
.                        add     eightlpi,row
.                        call    OrderMailHeader
                endif
                call    OrderPrintRecord
        repeat

LastRec
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.START PATCH 1.7 - ADDED LOGIC
.This will only happen if called from Program 1
        if (PrintFlag = 2)
.It takes some time for the file to be created, so we must check
.Allow 20 seconds to originally load DISTILLER
.begin patch 2.0
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                        move    timestamp2,time2
.                        sub     time1,time2,time3
.                        if (time3 > 2000) .20 Seconds Maximum
.                                break
.                        endif
.                repeat
.end patch 2.0
                call    OrderCreatePDFFile
        endif
.END PATCH 1.7 - ADDED LOGIC
.START PATCH 1.7 REPLACED LOGIC
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del g:\data\LCRfile.dat",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del g:\data\LCRfile.srt",taskname
.        reset   taskname
.        execute taskname
.        clear   taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del f:\data\fax\faxfile.prn",taskname
.        reset   taskname
.        execute taskname
.        PAUSE   C2
.        STOP
....................................
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,output1,hexzero
        call    DeleteFile
.
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,output2,hexzero
        call    DeleteFile
.
        if (PrintFlag = 0)
                move    "                                        ",APIFileName
                clear   APIFileName
.START PATCH 1.81 REPLACED LOGIC
.                pack    APIFileName,"f:\data\fax\faxfile.prn",hexzero
.START PATCH 1.91 REPLACED LOGIC
.                pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 1.91 REPLACED LOGIC
.end PATCH 1.81 REPLACED LOGIC
                call    DeleteFile
                PAUSE   C2
        endif
.
        move    "                                        ",APIFileName
        clear   APIFileName
.START PATCH 1.90 REPLACED LOGIC
.        pack    APIFileName,"c:\work\*.pdf",hexzero
        pack    APIFileName,"c:\work\PDF\*.pdf",hexzero
.END PATCH 1.90 REPLACED LOGIC
        call    DeleteFile
.
        move    "                                        ",APIFileName
        clear   APIFileName
.START PATCH 1.90 REPLACED LOGIC
.        pack    APIFileName,"c:\work\*.log",hexzero
        pack    APIFileName,"c:\work\PDF\*.log",hexzero
.END PATCH 1.90 REPLACED LOGIC
        call    DeleteFile
        pause   C2
        shutdown
.END PATCH 1.4 - REPLACED LOGIC
.START PATCH 1.7 - ADDED LOGIC
OrderCreatePDFFile
        move    "Here is your PDF File",MailSubjct
.   Set the text message that is send with the attachments
.begin patch 1.95
.        move    str55,MailBody

          move    Pfname,MailBody
.end patch 1.95
          pack      Mailto from userlogn,"@nincal.com"
          pack      Mailfrom from userlogn,"@nincal.com"
.begin patch 1.95
.          pack      MailAttach from "c:\work\pdf\",str55    ."
.begin patch 2.0
.          pack      MailAttach from "c:\work\pdf\",PFName    ."
          pack      MailAttach from PFName    
.end patch 2.0
CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.end patch 1.95
          call      SendMail
        return
.END PATCH 1.7 - ADDED LOGIC

...................

OrderOpenFile
.Test logic
.         PRTOPEN prfile,"@?",""
.         return
.Print newly sorted file
.       PRTOPEN prfile,"FAXFILE.PRN",""
.       PRTOPEN prfile,"Laser6","FAXFILE.PRN"
.........
.START PATCH 1.7 REPLACED LOGIC
.        if (osflag = c2)         .nt
.                PRTOPEN prfile,"\\NTS0\Laser6","FAXFILE.PRN"
.        elseif (osflag = c1)         .win 95 98
.                PRTOPEN prfile,"Laser6","FAXFILE.PRN"
.        else   .(osflag = c0)         .Don't know prompt for printer
.                PRTOPEN prfile,"@","FAXFILE.PRN"
.        endif
.        ADD     C1,COUNTR
.        DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
..
        if (PrintFlag = 0)
.Begin patch 1.85
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                ADD     C1,COUNTR
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
        elseif (PrintFlag = 1)  .Laser3
.Begin patch 1.85
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif

.END PATCH 1.75 REPLACED LOGIC
        elseif (PrintFlag = 2)  .PDF
                clear   str25
                append  OCOCODE,str25
                append  "_",str25
                append  OMLRNUM,str25
                reset   str25
.START PATCH 1.90 REPLACED LOGIC
.                PRTOPEN prfile,"Acrobat Distiller",str25
.begin patch 2.0
                pack    PFName from "C:\work\pdf\",str25,".pdf"
.                PRTOPEN prfile,"PDF995",str25
                PRTOPEN prfile,"PDF:",PFNAME
.END PATCH 1.90 REPLACED LOGIC
.begin patch 1.95
..                pack    str55,str25,".pdf"
.                pack    PFName,str25,".pdf"
.end patch 2.0
.end patch 1.95
        elseif (PrintFlag = 3)     .Laser3
.Begin patch 1.85
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
.END PATCH 1.75 REPLACED LOGIC
        else                    .Laser3 = Default
.Begin patch 1.85
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
        endif
.END PATCH 1.7 REPLACED LOGIC
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
.START PATCH 1.93 REPLACED LOGIC
.         endif
                    clear     HOLDEXCL
          else
                    move      COMPEXCL,HOLDEXCL
          endif
.END PATCH 1.93 REPLACED LOGIC
.Broker File
        pack    NBRKFLD,OBRKNUM,OBRKCNT
        rep     zfill,NBRKFLD
        move    "Driver-NBRKKEY,1rst",Location
        call    NBRKKEY
        if over
                pack   BRCOMP,"UNKNOWN",B55
        endif
.Special Instructions
        move    OLRN,NSPEFLD
        rep     zfill,NSPEFLD
        move    C3,NSPELOCK
        move    "Driver-NSPEKEY,1rst",Location
        call    NSPEKEY
        call    Trim Using DESC001
.Contact
        pack    str45,B55
        clear   str15
        move    C1,NCNTPATH
        move    OCOCODE,NCNTFLD
        move    "NCNTKEY",Location
        call    NCNTKEY
        if not over
                move    CNTNAME,str45
                move    CNTPHONE,str15
        endif
.Caller
        pack    CNTNAME,B55
        move    C1,NCNTPATH
        move    OCO2CODE,NCNTFLD
        move    "NCNTKEY",Location
        call    NCNTKEY
.Data Card Universe
        clear   UNIVERSE
.START PATCH 1.92 REPLACED LOGIC
.        move    MKEY,NXRFFLD2
          move      OMLRNUM,COMPFLD3
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          move      COMPNUM,NXRFFLD2
.END PATCH 1.92 REPLACED LOGIC
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
        return

OrderReadPendFiles
.NINORD5/NINORD4 File
        if (OSTAT = "l" | OSTAT = "z")
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
        elseif (OSTAT = "p" | OSTAT = "x")
                move    OLRN,NORD4FLD
                rep     zfill,NORD4FLD
                call    NORD4KEY
                if over
                        move    "No Status Found!",NPNDDESC
                else
.NINPND File
                        pack    NPNDFLD,OSTAT,NORD4STAT
                        rep     zfill,NPNDFLD
                        move    "Driver-NPDNKEY",Location
                        call    NPNDKEY
                        if over
                                move    "No Status Found!",NPNDDESC
                        endif
                endif
        else
                move    "No Status Found!",NPNDDESC
        endif
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
..        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9400:14700:100:8750:PICT2
.        move    "300",row
.        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page
.        add     sixlpi,row
.        add     sixlpi,row
.        add     eightlpi,row
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.89 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.START PATCH 1.93 REPLACED LOGIC
.         prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          if (HOLDEXCL = "P")
.                    prtpage   prfile;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
.                              *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
.                              *p=1630:550,"Oakland, CA 94612-1429":
.                              *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
.                              *p=1630:800,"A Division of Names in the News":
.                              *font=font2,*Alignment=*Left
.          else
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
.END PATCH 1.93 REPLACED LOGIC
.END PATCH 1.89 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.89 REPLACED LOGIC
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
.END PATCH 1.89 REPLACED LOGIC
.END PATCH 1.5 - REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"CONTACT:";
        prtpage prfile;*p1500:row,str45;
        prtpage prfile;*p5500:row,"DATE:  ";
        prtpage prfile;*p6000:row,newdate1;
        add     eightlpi,row
        prtpage prfile;*p1500:row,str15;
        add     eightlpi,row
.START PATCH 1.86 REPLACED LOGIC
.        move    str45,str1
.cntloopy
.        bump    str45,1
.        cmatch  B1,str45
.        goto    cntloopy if not equal
.        goto    cntexit if eos
.        bump    str45,1
.;START PATCH 1.6 REPLACED LOGIC
.;        move    str45,str6
.;Allow for names with spaces in them, but extract spaces
.        move    str45,str7
.        call    RemoveChar using str7,B1
.        move    str7,str6
.;END PATCH 1.6 REPLACED LOGIC
.        clear   str24
.        pack    str24,str1,str6,"@NINCAL.COM"
.cntexit reset   str45
.        prtpage prfile;*p1500:row,str24;
........................................
          clear     str60
          call      Trim using str45
          if (str45 <> "")
                    call      RemoveChar using str45,B1
.START PATCH 1.93 REPLACED LOGIC
                   pack      str60,str45,"@nincal.com"
.                    if (HOLDEXCL = "P")
.                              pack      str60,str45,"@pacificlists.com"
.                    else
.                              pack      str60,str45,"@nincal.com"
.                    endif
.END PATCH 1.93 REPLACED LOGIC
          endif
          prtpage prfile;*p1500:row,str60;
.END PATCH 1.86 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p2500:row,*font=font1,*boldon,"IN-HOUSE LCR PLANNER REPORT",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        return

OrderMailHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Mailer:  ",*boldoff,MCOMP;
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.93 REPLACED LOGIC
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN ##/";
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*pcolumn:row,*font=font5,*boldon,"PLI ##/";
.          else
.          prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN ##/";
.        endif
.END PATCH 1.93 REPLACED LOGIC
        prtpage prfile;*pcolumn2:row,"LCR Date/";
        prtpage prfile;*pcolumn3:row,"Mailer";
        prtpage prfile;*pcolumn4:row,"Caller/";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"List/";
        prtpage prfile;*pcolumn2:row,"Mail Date";
        prtpage prfile;*pcolumn3:row,"Universe";
        prtpage prfile;*pcolumn4:row,"Quantity";
        prtpage prfile;*pcolumn6:row,"Answer";
        prtpage prfile;*pcolumn7:row,"Date";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Status";
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
.START PATCH 1.6 CORRECTED LOGIC
.        if (OSTAT = "p")
        if (OSTAT = "p" | OSTAT = "x")
.END PATCH 1.6 CORRECTED LOGIC
                prtpage prfile;*pcolumn:row,*font=font2,*boldon,OLRN,B1,"Pending Order",*boldoff;
        else
                prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN;
        endif
        prtpage prfile;*pcolumn2:row,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY;
        prtpage prfile;*pcolumn4:row,CNTNAME;
        move    row,N10
        move    row,result
        add     eightlpi,N10
        add     eightlpi,N10
        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6750;
        move    column6,N9
        add     "70",N9
        move    row,howmany
        add     "70",howmany
        if (OHIST = "z")
                prtpage prfile;*pN9:howmany,*font=font1,"DENIED",*font=font2;
        else
.START PATCH 1.2 - REPLACED LOGIC
.Use of OELCODE will be obsolete once OCLRSTAT is fully in place
..START PATCH 1.1 - REPLACED LOGIC
..                if (OTOCODE = "R")
.                if (ORENT = "1")
..END PATCH 1.1 - REPLACED LOGIC
.                        if (OELCODE = "2" OR OELCODE = "3")
.                                prtpage prfile;*pN9:howmany,*font=font1,"RENT/EXC",*font=font2;
.                        else
.                                prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
.                        endif
.                else
.                        prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
.                endif
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
.END PATCH 1.2 - REPLACED LOGIC
        endif
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,O1DES;
        call    TRIM using OMDTEM
        count   N2,OMDTEM
        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                prtpage prfile;*pcolumn2:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                prtpage prfile;*pcolumn2:row,"As Soon As Possible";
        endif
        move    EditMask,EditQuan
.START PATCH 1.87 REPLACED LOGIC
.        move    C0,N9
.        move    UNIVERSE,N9
.        edit    N9,EditQuan
        move    C0,N10
        move    UNIVERSE,N10
        edit    N10,EditQuan
.END PATCH 1.87 REPLACED LOGIC
        prtpage prfile;*pcolumn3:row,EditQuan;
        if (OELCODE = "1" OR OELCODE = "3")
                move    "All",EditQuan
        else
                move    EditMask,EditQuan
.START PATCH 1.87 REPLACED LOGIC
.                move    C0,N9
.                move    OQTY,N9
.                edit    N9,EditQuan
                move    C0,N10
                move    OQTY,N10
                edit    N10,EditQuan
.END PATCH 1.87 REPLACED LOGIC
        endif
        prtpage prfile;*pcolumn4:row,EditQuan;
        call    TRIM using OCLRDTEM
        count   N2,OCLRDTEM
        if (N2 > 0 AND OCLRDTEM <> "00")
                prtpage prfile;*pcolumn7:row,OCLRDTEM,SLASH,OCLRDTED,SLASH,OCLRDTEC,OCLRDTEY;
        endif
        add     eightlpi,row
        if (OSTAT = "l" | OSTAT = "z")
                if (NORD5STAT = "02" OR NORD5STAT = "03")
                        prtpage prfile;*boldon;
                endif
        endif
        prtpage prfile;*pcolumn:row,NPNDDESC,*boldoff;
        prtpage prfile;*pcolumn2:row,DESC001;
        if (OSALES10 = "0" & OSALES = "6")
                add     eightlpi,row
                sub     "500",column2,N9
                prtpage prfile;*pN9:row,*boldon,"Broker:  ",*boldoff;
                prtpage prfile;*pcolumn2:row,BRCOMP;
        endif
        call    PrintSpecialInstructions
        add     eightlpi,row
        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
        call    TRIM using DESC002
        if (DESC002 <> "")
                pack    str2,carr,B1
                rep     str2,DESC002
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC002,C1
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
.begin patch 1.95
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Planner Report - ",str25,b1,str55
.                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
.end patch 1.95

.Include IO file
        include nordio.inc
        include nord5io.inc
        include nord4io.inc
        include nordPio.inc
.Patch1.88
                              include   compio.inc
                              include   cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
.Patch1.88
        include nxrfio.inc
        include ndatio.inc
        include npndio.inc
        include nspeio.inc
        include ncntio.inc
        include comlogic.inc

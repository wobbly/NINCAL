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
          include   compdd.inc
          include   cntdd.inc
        include nxrfdd.inc
        include ndatdd.inc
        include nowndd.inc
        include npnddd.inc
        include nord5dd.inc
        include nspedd.inc
        include ncntdd.inc
        include nsmpdd.inc
        include nofrdd.inc
        include winapi.inc
          INCLUDE   NSEL2DD.INC

Release   Init      "2.4"     DLH   64 bit os
Reldate   Init      "Dec 5 2011"
.Release   Init      "2.3"     DLH   create all via pdf enable email, add ownemail to file
.Reldate   Init      "June 2011"
.release   init      "2.21"        DLH    cleanup
.reldate   Init      "11 May 2010"
.See archives for previous revision code
.release   init      "2.20"        DLH    Drop PL
.reldate   Init      "16 February 2010"
.release   init      "2.19"        DLH   Change file name to stop collisions
.reldate   Init      "11 September 2008"
.release            init      "2.18"        DLH   Extra check for output file #2 (sort)
.reldate  Init      "25 March 2008"
.release  init      "2.17"        DLH   18Oct2007 New Facsys
.release  init      "2.16"        DLH   27Apr2007 check for PDF autoload
.release  init      "2.15"        DLH   08Mar2007 PL
.                                       ASH Added logic to this patch on 6/18/2007 (Marked as 2.15.1)
.release  init      "2.14"        DLH   05Dec2006 patch to SendMail
.release  init      "2.13"        ASH   03MAR2006 PATCHED PDF995.INI LOGIC
.release  init      "2.12"        ASH   27JAN2006 Bug fix with sort file collision
.release  init      "2.11"        ASH   20DEC2005 Bug fix with PDF File Dump option for Galley Report
.release  init      "2.10"        ASH   18JUN2005 Changed IP address of file manager
.release  init      "2.09"        ASH   18MAY2005 LISTMLR Conversion
.release  init      "2.08"        ASH   20APR2005 Modified how PDF files are emailed
.         Patch 2.08 adds new functionality:
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
..........................................
.release  init      "2.07"        ASH   09DEC2004 FAXFILE
.release  init      "2.06"        ASH   02DEC2004 Conversion from Acrobat Distilller to PDF995
.release  init      "2.05"        ASH   02NOV2004 Sample File conversion - Mailer field increased to 6 bytes
.release  init      "2.04"        ASH   06AUG2004 Logo Conversion
.release  init      "2.03"        DMB   26MAY2004 Mailer Conversion
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
.begin patch 2.21
DLresult  form 9
DLndx     form 9
dmFileName          dim 80
.end patch 2.21

FileCheck FIle
trapcount form      4

str60     dim       60
.Counters
COUNTR  FORM    9
COUNTR2 FORM    9
.Files to open
prfile  pfile
input2  file
input2i ifile    Name="NINPRINT.isi|10.10.30.103:502"
.>Patch 2.10
input3  file
First   init    "Y"
GoodStat init   "zlpx"
PrtFlag dim     1
HOLDOWN dim     4
HOLDLIST dim    6
HOLDEXCL dim        1
COMPHOLD DIM    45
newdate1 dim    10
faxnum  dim     10
LONGDIST dim    1
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    20
line1   dim     88
Carr    init    0x7f
DESC003 dim     725     .DESC002 + O2DES

.Sample variables
DCXFile dim     120
dcxpath DIM     45
dcxext  init    ".TIF"
.END PATCH 1.99 REPLACED LOGIC
FilePath dim    40
SMPArray dim    9(50)
SMPIndex form   2

PrintFlag form  "00"    .Default is Laser3!!!  01=Laser 3, 02=PDF format, 03=Laser 2

input2Name dim  40
output1 dim     40
output2 dim     80
userinfo dim    500
userlogn dim    7
userlogw dim    7
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.begin patch 2.3
EmailfLAG  iNIT      "N"
.end patch 2.3
testint INTEGER 4
PDFFlag   form      1
pict3   pict
mss1    plform  Error
font1   font
font2   font
font3   font
font4   font
font5   font
.....44
font6   font
....
Font7     font
        formload mss1
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
......
        create  font6,"Arial",size=14
......
          create    font7,"Times New Roman",size=9
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
       PACK    DCXPATH,NTWKPATH1,"samples\s"

          CALL GETWINVER
          Call      GetPDFPath


        match   "NLCR0038",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0038",PROGRAM
                PACK    input2Name,NTWKPATH1,"NPRINT2.LCR"
                PACK    output1,NTWKPATH1,"LCRFILE.DAT"
                PACK    output2,NTWKPATH1,"LCRFILE.SRT"
        else
                clear   input2Name
                append  NTWKPATH3,input2Name
.....................
                append  inpname,input2Name
                append  ".DAT",input2Name
                reset   input2Name
                move    prtname,PrintFlag
                move    user,userlogn
                call    Trim using userlogn
                clear   output1
                clear   output2
                clear   FilePath
                append  "\\nins1\d\USERS\",output1                           ."
                append  "\\nins1\d\USERS\",output2               ."    
                append  "\\nins1\d\USERS\",FilePath                          ."
.
                if (userlogn <> "")
                        append  userlogn,output1
                        append  "\",output1                           ."
                        append  userlogn,output2
                        append  "\",output2                           ."
                        append  userlogn,FilePath
                        append  "\",FilePath                          ."
.
                              if (PrintFlag = 2 | PrintFlag = 4)
                                        if (PrintFlag = 2)
.Clean up working directory on server.  If running on individual machine, do not worry about this.
.We do not want to clean up the directory on a users hard drive as they may be keeping files out there
.that they may later want.
.begin patch 2.21
                                        DISPLAY *P1:24,"CLEANING UP OLD FILES";

                                                  clear     taskname
                                                  clear     str55
                                        clear     Mailbody
                                        FIndDIr   "c:\work\pdf\*.pdf",MailBody,Itemcount=n5
                                        if        (n5 > c0)
                                        FOr       n4 from c0 to N5
                                        explode   MailBody,"|",Dmfilename 
                                        match     "f",Dmfilename 

                                                  if        equal
          
                                                  clear     taskname
                                                  bump      DmFileName,c1
                                                  pack      taskname from "C:\work\",DmFIleName  ."comment :)
                                                  FINDFILE  Taskname,WRITE=Str25
                                                  unpack    str25,CC,YY,MM,DD
                                                  call      cvtjul
                                                  move      Juldays,Howmany
                                                  calc      N9=(result-howmany)
                                                            if (N9 >= 1)
                                                            erase          taskname
                                                            endif
                                                  endif
                                        repeat
                                        endif
                                        clear     Mailbody
                                        FIndDIr   "c:\work\pdf\*.log",MailBody,Itemcount=n5
                                        if        (n5 > c0)
                                        FOr       n4 from c0 to N5
                                        explode   MailBody,"|",Dmfilename 
                                        match     "f",Dmfilename 

                                                  if        equal
          
                                                  clear     taskname
                                                  bump      DmFileName,c1
                                                  pack      taskname from "C:\work\",DmFIleName  ."comment :)
                                                  FINDFILE  Taskname,WRITE=Str25
                                                  unpack    str25,CC,YY,MM,DD
                                                  call      cvtjul
                                                  move      Juldays,Howmany
                                                  calc      N9=(result-howmany)
                                                            if (N9 >= 1)
                                                            erase          taskname
                                                            endif
                                                  endif
                                        repeat
                                        endif
.end patch 2.21
                                        endif
                                        call       GetPDFPath
                                        pack      str45 from PDFPATH,"\res\pdf995.ini"

.                                        call      "GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
                                        pack      str45 from PDFPATH,"\res\pdf995.ini"
                                        call      "GU$INI;WRITE_TO_INI" USING Str45:
                                                  "Parameters":
                                                  "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
                             "\\nins1\e\apps\plb\code\pdftest.bat":
                                                  result

.                                        move      "\\nins1\e\apps\plb\code\pdftest.bat",str35
.
.                                       call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                                                 "Parameters":
.                                                 "ProcessPDF":
.                                                 "\\nins1\e\apps\plb\code\pdftest.bat":
.                                                 result
                                        if (result = C0)
.Prepare Flag file
                                                  move      C1,PDFFlag
                                                  pack      str45 from PDFPATH,"\flag.dat"
.                                                  prep      tempfile,"c:\progra~1\pdf995\flag.dat"
.                                                  write     tempfile,SEQ;"flag set"
                                                  prep      tempfile,Str45
                                                  write     tempfile,SEQ;"flag set"
                                                  close     tempfile
                                        else
                                                  move      C2,PDFFlag
                                        endif
                                        if (PDFFlag = C2)
.Send message to I.S.
                                                  move      "This is a message from       NLCR0038",MailSubjct
                                                  CLear     MailBody
                                                  Append    "Failure to update pdf995.ini.",MailBOdy
                                                  append    CRLF,Mailbody
                                                  Append    "Field: ",Mailbody
                                                  append    str25,Mailbody
                                                  append    CRLF,Mailbody
                                                  Append    "Value: ",Mailbody
                                                  append    str35,mailbody
                                                  append    CRLF,Mailbody
                                                  reset     Mailbody
                                                  move      "InformationServices@nincal.com",Mailto
                                                  move      "InformationServices@nincal.com",MailFrom
                                                  
                                                  call      SendMail
                                                  move      C0,PDFFlag
                                        endif
                              endif
                endif
                    if (PrintFlag = 4 | PrintFlag = 2)
                              clock     timestamp,timestamp
                              append    timestamp,output1
                              append    ".DAT",output1
                              append    timestamp,output2
                              append    ".SRT",output2
                    else
                              append  "LCRFILE2.DAT",output1
                              append  "LCRFILE2.SRT",output2
                endif
                reset   output1
                reset   output2
                reset   FilePath
........................
        endif
        move    "NLCR0038",WPrognme
.        clock   date to date
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD

.        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
                    call                GetWinVer
        move    C0,NORDFLAG
        move    C0,NORDFLG2

        if (PrintFlag = 0)
                call    Paint
        endif

        if (PrintFlag = 0)
                DISPLAY *P1:24,"OPENING FILES";
        endif
.Open Files
        open    input2,input2Name
        PACK    STR35,"NINPRINT.ISI|10.10.30.103:502"
        open    input2i,STR35
        prepare input3,output1
        if (PrintFlag = 0)
                DISPLAY *P1:24,"READING NINPRINT FILE";
        endif
LCRLoop
        loop
                read    input2,seq;ordvars
                until over
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
                                filepi  1;input3
.begin patch 2.3
.                                        write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                                                  OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
.                                                  OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                                  OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL
                                        write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                                                  OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                                                  OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                                  OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL
.end patch 2.3
.DELETE CANCELLED\DENIED RECORDS - SHOULD ALREADY BE DONE, THIS IS A FAIL SAFE!!!
                        if (OSTAT = "z")
........
                                if (PrintFlag = C0)
                                        filepi  2;input2i
                                        read    input2i,OLRN;;
                                        if not over
.                                                delete  input2i,OLRN
                                        endif
                                endif
                        endif
        repeat

        if (PrintFlag = 0)
                DISPLAY *P1:24,"SORTING LCRFILE FILE ";
        endif
SortFile
        clear   taskname
        append  output1,taskname
        append  COMMA,taskname
        append  output2,taskname
        append  ";",taskname
        append  "1-4,5-10,15-20",taskname
        reset   taskname
. avoid collision with user running mult jobs at the same time
DBLcheck
          FindFIle  output2
          move      C0,N1
          for N2,1,10
                    sort    taskname
                    if not over
                              move      C0,N1
                              break
                    else
                              move      C1,N1
                    endif
                    pause     "5"
        repeat
          if (N1 = C1)
                    move    s$error$,error
                    move    "Sort did not work!",Location
                    clear     KeyLocation
                    call    IOMssg
          call      pdf995auto0
                    shutdown
          endif
        if (PrintFlag = 0)
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
        endif
PrintFile
.Set up columns
        move    "500",column
        move    "1200",column1
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
        move    "6750",column7
.Initialize HLDBRK
        clear   HOLDOWN
.close unsorted file
.
       close   input3
.
.open newly sorted file

          open      input3,output2,exclusive
          loop
.begin patch 2.3
                    read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                              OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                              OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                              OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL
.end patch 2.3
                 DISPLAY *P10:10,"Reading MLR/LR## ",Omlrnum," ",olrn;

                if (olon = "2006")
                        call    trapit
                endif
                goto    LastRec if over
                if (OLON <> HOLDOWN | OLNUM <> HOLDLIST)
.PATCH 1.51.1 NOTE:  If the logic goes back to breaking only on LO Number,
.then we would need to sort on Company association (PLI/NIN) as well as
.break on it.
                        if (FIRST = YES)
                                move    OWNOCPY,COMPHOLD
                        endif
                        if (FIRST = NO)
                                call    PrintSamplePage
                                clear   SMPArray
                                PRTCLOSE prfile
.This will only happen if called from Program 1
..................................................
                                if (PrintFlag = 2 | PrintFlag = 4)
                                                  if (PDFFlag <> C1)  .Not using PDF995 to create Email
.It takes some time for the file to be created, so we must check.
.Allow 35 seconds
                                                            clock     timestamp,timestamp1
                                                            move      timestamp1,time1
                                                            loop
                                                                      clock     timestamp,timestamp2
                                                                      move      timestamp2,time2
                                                                      sub       time1,time2,time3
                                                                      if (time3 > 3500) .35 Seconds Maximum
                                                                                break
                                                                      endif
                                                            repeat
                                                  else
.Give the email a chance of rendering itself before updating the INI file.
                                                            pack      APIFileName from PDFPATH,"\flag.dat"

.                                                            pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
                                                            for N3,C1,"100"
                                                            repeat
                                                            pause     "5"
                                                  endif
                                                  call    OrderCreatePDFFile
                                endif
                                if (PrtFlag = NO)
.This will never be the case if called from Program 1
.begin patch 2.3   send the PDF via email

.                                        clear   taskname
.                                        call                getwinver   ;make sure we have osflag
.                                        If                  (osflag = c1 | osflag =C5)
.                                                append  "!c:\winnt\system32\cmd.exe",taskname
..                                        else
.                                        elseif              (osflag = C3 | osflag =C4)
.                                                append  "!c:\command.com",taskname
.                                        elseif              (Osflag = C6)
.                                                append  "!c:\windows\system32\cmd.exe",taskname
.                                        endif
.                                        append  " /c copy ",taskname
..............................
.                                        append  "C:\WORK\hdrfile.prn /b + ",taskname
.                                        append  "C:\WORK\faxfile.prn /b ",taskname
.                                        append  NTWKPATH4,taskname
.                                        append  "fax\fax",taskname
.                                        append  HOLDOWN,taskname
.                                        append  ".prn /b",taskname
.                                        reset   taskname
.                                        execute taskname
.                                        clear   taskname
.                                        call                getwinver   ;make sure we have osflag
.                                        If                  (osflag = c1 | osflag =C5)
.                                                append  "!c:\winnt\system32\cmd.exe",taskname
..                                        else
.                                        elseif              (osflag = C3 | osflag =C4)
.                                                append  "!c:\command.com",taskname
.                                        elseif              (Osflag = C6)
.                                                append  "!c:\windows\system32\cmd.exe",taskname
.                                        endif
.                                        append  " /c copy /b ",taskname
..path not nec as batch changes to the directory --- fingers crossed
.                                        append  NTWKPATH4,taskname
.                                        append  "fax\fax",taskname
.                                        append  HOLDOWN,taskname
.                                        append  ".prn \\srv2008a\fax",taskname
.                                        reset   taskname
.                                        execute taskname
                                CaLL    OrderCreatePDFFile1
                                endif
.end patch 2.3
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
                endif
                call    OrderPrintRecord
                if (OSCODE = "1")
                        type    OSAMCDE
                        if equal
                                if (OSAMCDE <> "000")
                                        move    C1,SMPIndex
                                        move    C0,N1
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
                                endif
                        endif
                endif
        repeat

LastRec
        call    PrintSamplePage
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.This will only happen if callded from Program 1
        if (PrintFlag = 2 | PrintFlag = 4)
...........................................
                    if (PDFFlag <> C1)  .Not using PDF995 to create Email
.It takes some time for the file to be created, so we must check.
.Allow 35 seconds
                              clock     timestamp,timestamp1
                              move      timestamp1,time1
                              loop
                                        clock     timestamp,timestamp2
                                        move      timestamp2,time2
                                        sub       time1,time2,time3
                                        if (time3 > 3500) .35 Seconds Maximum
                                                  break
                                        endif
                              repeat
                    else
.Give the email a chance of rendering itself before updating the INI file.
                              pack      APIFileName from PDFPATH,"\flag.dat"
.                              pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
                              pause     "5"
                    endif
                    call    OrderCreatePDFFile
                    move      "ProcessPDF",str25
                    clear     str55
                    call       GetPDFPath
                    pack      str45 from PDFPATH,"\res\pdf995.ini"
                    call      "GU$INI;WRITE_TO_INI" USING Str45:
                              "Parameters":
                              "ProcessPDF":
                              "":
                              RESULT
.                    call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                              "Parameters":
.                              str25:
.                              "":
.                              result
                    if (result = 0)
                              move      C1,PDFFlag
                    else
                              move      C2,PDFFlag
                    endif
                    if (PDFFlag = C2)
.Send message to I.S.
                              move      "This is a message from       NLCR0038",MailSubjct
                              Clear     Mailbody
                              Append    "Failure to re-update pdf995.ini.",Mailbody
                              append    CRLF,Mailbody
                              append    "Pdf995.ini has been modified.",Mailbody
                              append    CRLF,Mailbody
                              Append    "Field: ",Mailbody
                              append    str25,Mailbody
                              append    CRLF,Mailbody
                              append    "Value: ",Mailbody
                              append    str35,Mailbody
                              append    CRLF,Mailbody
                              reset     Mailbody
                              move      "InformationServices@nincal.com",Mailto
                              move      "InformationServices@nincal.com",Mailfrom
                              call      SendMail
                              move      C0,PDFFlag
                    endif
        endif
        if (PrtFlag = NO)
.begin patch 2.3  send the fax via email
.                clear   taskname
.                 call                getwinver   ;make sure we have osflag
.                 If                  (osflag = c1 | osflag =C5)
.                 append  "!c:\winnt\system32\cmd.exe",taskname
..                                        else
.                 elseif              (osflag = C3 | osflag =C4)
.                 append  "!c:\command.com",taskname
.                 elseif              (Osflag = C6)
.                 append  "!c:\windows\system32\cmd.exe",taskname
.                endif
.                append  " /c copy ",taskname
....................
.                append  "C:\WORK\hdrfile.prn /b + ",taskname
.                append  "C:\WORK\faxfile.prn /b ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\fax",taskname
.                append  HOLDOWN,taskname
.                append  ".prn /b",taskname
.                reset   taskname
.                execute taskname
.                clear   taskname
.                 call                getwinver   ;make sure we have osflag
.                 If                  (osflag = c1 | osflag =C5)
.                 append  "!c:\winnt\system32\cmd.exe",taskname
..                                        else
.                 elseif              (osflag = C3 | osflag =C4)
.                 append  "!c:\command.com",taskname
.                 elseif              (Osflag = C6)
.                 append  "!c:\windows\system32\cmd.exe",taskname
.                endif
.               append  " /c copy /b ",taskname
.              append  NTWKPATH4,taskname
.              append  "fax\fax",taskname
..               append  "fax",taskname
.                append  HOLDOWN,taskname
.                append  ".prn \\srv2008a\fax",taskname
.                reset   taskname
.                execute taskname
          cALL      OrderCreatePDFFile1
.enD patch 2.3  send the fax via email
        endif
.......................
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,output1,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.
          Close     input3,delete
.end patch 2.18    api call can be removed 
          
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
                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
                call    DeleteFile
                if (APIResult = 0 | APIResult = hexeight)
                endif
                PAUSE   C2
        elseif (PrintFlag = 2 | PrintFlag = 4)
                close   input2
                move    "                                        ",APIFileName
                clear   APIFileName
                pack    APIFileName,input2Name,hexzero
                call    DeleteFile
                if (APIResult = 0 | APIResult = hexeight)
                endif
                PAUSE   C2
        endif
          call      pdf995auto
        shutdown

OrderCreatePDFFile
          if (PrintFlag = 4)
.File creation on harddrive!
                    call      EmailUserPDF
                    return
          endif
          PAUSE       C10
.begin patch April 26th 2010
                    call      testclient
                    if        (ClntServFlag = c1)
                    Pause     "20"
                    pack      MailAttach from "c:\work\pdf\",str45,".pdf"              ."
                    pack      taskname from "!c:\work\pdf\",str45,".pdf"              ."
                    Pause     C5
                    copyfile  taskname,mailattach          
                    Pause     "20"
                    endif
.end patch April 26th 2010

          Move      c0,Trapcount
.begin patch 2.3
          call      Trim using CNTNAME
          if        (CNTNAME <> "" & CNTNAME <> "BILLING")
          call      RemoveChar using CNTNAME,B1
          pack      mailto,CNTNAME,"@nincal.com"
          else
          call      trim using Userlogn
                    if        (UserLogn <> "" & UserLogn <> "BILLING")
                    call      RemoveChar using UserLogn,B1
                    pack      mailto,UserLogn,"@nincal.com"
                    else
                    pack      mailto,"ComputerRequest@nincal.com"
                    endif
          endif          
          move      Mailto,Mailfrom
          pack      mailbcc,"ComputerRequest@nincal.com"
          goto      Checkfile

.end patch 2.3
.begin patch 2.3
OrderCreatePDFFile1
          Move      c0,Trapcount
.end patch 2.3

.dave goes bad
CheckFile
.          Pause     "90"
                    call      waitin using "9000"
          pack      str55 from "c:\work\pdf\",str45,".pdf"
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from str55

.begin patch 2.3
.          CLEAR     MailTO
.          append    Userlogn,Mailto
.          append    "@nincal.com",Mailto
.          reset     Mailto
.          move      Mailto,Mailfrom
.end patch 2.3
          Move      "Galley report",Mailsubjct
          Move      "360",MailTimer
          call      SendMail


.Clean up afterwards
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,"C:\WORK\PDF\",str55,hexzero    ."
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
        return

OrderBadSample
        move    "This is a Error e-mail from LCR Galley Report",MailSubjct
          Clear     Mailbody
          Append    "You have included a Bad Sample!!",Mailbody
          Append    CRLF,Mailbody
          clear   taskname
        unpack  str9,str6,str3
        pack    taskname,"LR: ",OLRN,B1,"MAILER: ",str6,B1,"SAMPLE: ",str3
          append    taskname,Mailbody
          Append    CRLF,Mailbody
          reset     Mailbody
          MOVe      "ComputerRequest@nincal.com",Mailfrom
          pack      Mailto from Mailfrom,CNTNAME
          call      SendMail
        return

OrderOpenFile
.Print newly sorted file
.begin patch 2.3
          Scan      "@",ownemail
          if        equal
          move      Yes,Emailflag
          else
          move      no,emailflag
          endif
          Reset     OwnEmail
.end patch 2.3
        move    OWNFAX,faxnum
        match   "0000000000",faxnum
        if      equal
.begin patch 2.3
                if      (emailflag = No)          .no fax & No email                     
                move    YES,PrtFlag             .PRINT IT
                endif
.end patch 2.3
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
.Prevent faxes from being run if you are calling this program dynamically through Program 1
        if (PrintFlag <> C0)
                move    YES,PrtFlag
        endif
.
        if (PrtFlag = YES)
..........................
.Printer of your choice
                if (PrintFlag = C0 | PrintFlag = 3)     .Laser3 = Default
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 1)  .Laser3
                        if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                PRTOPEN prfile,"\\NINs2\Laser8","FAXFILE.PRN"
                        elseif (osflag = c3 | osflag =c4)         .win 95 98
                                PRTOPEN prfile,"Laser8","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 2 | PrintFlag = 4)  .PDF
          call      pdf995auto

                              clear   str45
                              append  OLON,str45
                              append  "_",str45
                              append  OLNUM,str45
                              if (PrintFlag = 4)
                                        clock     timestamp,timestamp
                                        append  "_",str45
                                        append    timestamp,str45
                              endif
                              reset   str45
                              PRTOPEN prfile,"PDF995",str45
                              pack    str55,str45,".pdf"
                endif
                ADD     C1,COUNTR2
        else 
.begin patch 2.3
.fax change to use PDF & check for Email
                    call      pdf995auto

                    clear   str45
                    append  OLON,str45
                    append  "_",str45
                    append  OLNUM,str45
                    reset   str45
                    PRTOPEN prfile,"PDF995",str45
                    pack    str55,str45,".pdf"

.START TEST - PREVENT FILES FROM ACTUALLY BEING SENT OUT TO CLIENTS
.                clear   LONGDIST
.                MOVE    "4337796",FAXNUM
.END TEST
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,"C:\WORK\hdrfile.prn",hexzero
.                call    DeleteFile
.                if (APIResult = 0 | APIResult = hexeight)
.                endif
.                SPLOPEN "C:\WORK\HDRFILE.PRN"
.                print   "^[D",longdist,faxnum,"^[N",OWNOCPY:
.                        "^[S",CNTNAME,B2,CNTPHONE," ^]"
.                SPLCLOSE
.                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
                ADD     C1,COUNTR
                    scan      "@",OwnEmail
                    if        equal
                    Reset     OwnEmail
                    move      OwnEmail,Mailto
                    else
                    pack      MailTo,"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                    endif
         Reset     OwnEmail
         append     cntname,MailBody
         append     CRLF,MailBody
         append     ownocpy,Mailbody
         append     CRLF,MailBody
.end patch 2.3
        endif
        if (PrintFlag = 0)
                DISPLAY *P10:12,*EL,"Sent   COUNT ",COUNTR
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR2
        endif
.END PATCH 1.4 - REPLACED LOGIC
        RETURN

OrderReadOtherFiles
.Open other files to retrieve appropriate information
.START PATCH 2.15.1 ADDED LOGIC
          move    OLNUM,NDATFLD
          move    C1,NDATPATH
          move    "Driver-NDATKEY,1rst",Location
          call    NDATKEY
          if over
                    clear     HOLDEXCL
          else
                    move      ELSTCDE,HOLDEXCL
          endif
          //.Clean up afterwards
          move    C0,UNIVERSE
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
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
          Move      C1,NCntPAth
        call    NCNTKEY
.Data Card Universe
        clear   UNIVERSE
          pack      COMPFLD3,OMLRNUM
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
        move    COMPNUM,NXRFFLD2
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
          move      "Driver-COMPKEY3",Location
          pack      COMPFLD3,OMLRNUM
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          if over
                    clear     COMPNUM
          endif
          pack      NSMPFLD,COMPNUM,OSAMCDE

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
.Offer file
        bump    OODNUM,4
        pack    NOFRFLD,OMLRNUM,OODNUM
        rep     zfill in NOFRFLD
        move    "O.LoadOffer-NOFRKEY",Location
        call    NOFRKEY
        if over
                clear   OFDESC
        endif
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
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
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
        prtpage prfile;*p900:row,OWNLONM;
        prtpage prfile;*p5200:row,"FROM:";
        prtpage prfile;*p5700:row,CNTNAME;
        add     eightlpi,row
        prtpage prfile;*p900:row,COMPHOLD;
        prtpage prfile;*p5700:row,CNTPHONE;
        add     eightlpi,row
        prtpage prfile;*p900:row,OWNLOSA;
.........................................
          clear     str60
          call      Trim using CNTNAME
          if (CNTNAME <> "" & CNTNAME <> "BILLING")
                    call      RemoveChar using CNTNAME,B1
                              pack      str60,CNTNAME,"@nincal.com"
          endif
          prtpage prfile;*p5700:row,str60;
        add     eightlpi,row
        call    Trim using OWNLOCTY
        if (OWNLOCTY <> "")
                pack    taskname,OWNLOCTY,COMMA,OWNLOS,B1,OWNLOZC
                prtpage prfile;*p900:row,taskname;
        endif
.begin patch 2.3   indicate fax or email
          If        (emailFlag = Yes)
          prtpage prfile;*p5200:row,"EMAIL:";
                    prtpage prfile;*p900:row,Mailto;
          else
          prtpage prfile;*p5200:row,"FAX:";
          call    Trim using OWNTELE
          if (OWNTELE <> "")
                    unpack  OWNTELE,str3,str2,str1,str4
                    pack    taskname,"(",str3,") ",str2,str1,"-",str4
                    prtpage prfile;*p900:row,taskname;
          endif
          endif
.        prtpage prfile;*p5200:row,"FAX:";
.end patch 2.3   indicate fax or email
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
          move      c1,NCntPath
        call    NCNTKEY
                    if (CNTTEAM = "05") .List Management
                                    prtpage prfile;*p5700:row,"(415) 945-9451";
                    else
                              prtpage prfile;*p5700:row,"(415) 433-7796";
                    endif
        add     eightlpi,row
...............
.          call    Trim using OWNTELE
.          if (OWNTELE <> "")
.                    unpack  OWNTELE,str3,str2,str1,str4
.                    pack    taskname,"(",str3,") ",str2,str1,"-",str4
.                    prtpage prfile;*p900:row,taskname;
.          endif
        add     eightlpi,row
.begin patch 2.3   indicate fax or email
          If        (emailFlag = Yes)
          prtpage prfile;*pcolumn:row,"Email:";
          else
          prtpage prfile;*pcolumn:row,"FAX:";
          endif
.end patch 2.3   indicate fax or email
          call    Trim using OWNFAX
          if (OWNFAX <> "")
                    unpack  OWNFAX,str3,str2,str1,str4
                    pack    taskname,"(",str3,") ",str2,str1,"-",str4
                    prtpage prfile;*p900:row,taskname;
          endif
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,newdate1;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        return

OrderListHeader
        prtpage prfile;*pcolumn:row,*font=font5,"List:  ",O1DES;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN##/";

        prtpage prfile;*pcolumn1:row,"Status";
        prtpage prfile;*pcolumn4:row,"Mail Date/";
        prtpage prfile;*pcolumn6:row,"Sample";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Mailer/";
        prtpage prfile;*pcolumn4:row,"Quantity";
        prtpage prfile;*pcolumn5:row,"Reco.";
        prtpage prfile;*pcolumn6:row,"Attached/";
        prtpage prfile;*pcolumn7:row,"Answer";

        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Offer/";
        prtpage prfile;*pcolumn6:row,"Description";
..................
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

OrderPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderListHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN;
        if (OSTAT = "l" | OSTAT = "z")
                if (NORD5STAT = "02" OR NORD5STAT = "03")
                        prtpage prfile;*boldon;
                endif
                prtpage prfile;*pcolumn1:row,NPNDDESC,*boldoff;
        else
                prtpage prfile;*pcolumn1:row,"Pending Order",*boldoff;
        endif
        call    TRIM using OMDTEM
        count   N2,OMDTEM
        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                prtpage prfile;*pcolumn4:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                prtpage prfile;*pcolumn4:row,"As Soon As Possible";
        elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
                prtpage prfile;*pcolumn4:row,"See Special Instructions";
        endif
        move    row,N10
        move    row,result
        add     eightlpi,N10
        add     eightlpi,N10
        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6000;





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
          move    C0,N9
          move    OQTY,N9
          if (OELCODE = "1" OR OELCODE = "3")
                    if (N9 > 0)
                              move    EditMask,str11
                              edit    N9,str11
                              call      Trim using str11
                              pack      EditQuan,str11,"/ALL"
                              add     eightlpi,row,N9
                              sub       "500",column4,N10
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
        clear   str9
        if (ORENT = "1")
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
.Note:  The Box holding "Answer" is printed after the XSTAT
.       This is done so that complete box is printed.  Following vars must, therefore, must remain.
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OFDESC;
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
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,DESC001;
        call    TRIM using NSEL2NAME
        if (NSEL2NAME <> "")
                pack    DESC003,"Select:  ",NSEL2NAME,".  ",DESC002
        else
                pack    DESC003,DESC002
        endif
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
                clear   str9
                move    SMPArray(SMPIndex),str9
                call    Trim using str9
                if (str9 = "")
                        break
                endif
                if (SMPIndex >= 50)
                        break
                endif
                pack    DCXFile,DCXPath,str9,DCXExt
                clear   N9
                CREATE  PICT3=70:700:100:550:
                        DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0
                PICT3.GetPageCount GIVING N9
                prtpage prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
.Print and Display Additional Pages
                if (N9 > C1)    .Only Enter loop if more than one page
                        clear   N8
                        move    C1,N8   .Start with SECOND PAGE as first page already printed
                        loop
                                add     C1,N8
                                until (N8 > N9)
                                CREATE  PICT3=70:700:100:550:
                                        DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0,PAGE=N8
                                prtpage prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
                        repeat
                endif
                add     C1,SMPIndex
        repeat
printstop
        DESTROY PICT3
        return
trapit
        return
EmailUserPDF
          Clear     Mailbody
          append    "This is a message from       the Galley Report Program",Mailbody
          Append    CRLF,MailBody                                               
          Append    "Your PDF file was created!",Mailbody
          Append    CRLF,MailBody                                               
          append    "Location:  c:\work\pdf\ ",mailbody
          Append    CRLF,MailBody                                               
          append    "filename:  ",mailbody
          append    str55,mailbody
          Append    CRLF,MailBody                                               
          Reset     MailBody
.begin patch April 26th 2010
                    call      testclient
                    if        (ClntServFlag = c1)
.                    Pause     "20"
                    call      waitin using "2000"
                    pack      MailAttach from "c:\work\pdf\",str55              ."
                    pack      taskname from "!c:\work\pdf\",str55              ."
                    Pause     C5
                    copyfile  taskname,mailattach          
.                    Pause     "20"
                    call      waitin using "2000"
                    endif
.end patch April 26th 2010

                    
          pack      mailattach,"c:\work\pdf\",str55                  ."
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "Your PDF file was created ",Mailsubjct
          call      SendMail
.end patch 2.14

          return
.END PATCH 2.08 ADDED LOGIC
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    
                    call      waitin using "1000"
.                    pause     c10
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me
                    Pack       MailSubjct,"Galley Lcrs - ",str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
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



.Include IO file
        include nordio.inc
                              include   compio.inc
                              include   cntio.inc
        include nxrfio.inc
        include ndatio.inc
        include nownio.inc
        include npndio.inc
        include nord5io.inc
        include nspeio.inc
        include nsmpio.inc
        include nofrio.inc
        include ncntio.inc
          INCLUDE   NSEL2IO.INC
        include comlogic.inc
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

release   init    "3.12"     DLH  New fax gateway, added new Faxflag as Emailflag is true for fax or email, some cleanup
REldate   Init      "2015 April 14"    
.release   init    "3.11"     DLH  additional smp code "smpcleanup"
.REldate   Init      "2015 April 13"    
.release   init    "3.10"     DLH  Convert samples to pdf and include link instead of printing and added code to try and recover from Spooling S10 error
.                            Replace all    CRLF,MailBody with "<br>",mailbody for emails to HTML
.REldate   Init      "2015 January xx"    
.Release   Init      "3.00"     DLH   if func=1 allow (default?) creation of Excel instead of pdf (may just use print switch = 6)
.Reldate   Init      "2014 March 27"
.Release   Init      "2.62"     DLH   if func=1 use userlogn for email not contact name (Manual request via nordtest & Butil)
.Reldate   Init      "2014 March 10"
.Release   Init      "2.61"     DLH   torn *Overlayon to try and fix some pdf issues
.Reldate   Init      "2013 July 31"
.Release   Init      "2.6"     ASH  New LCR Status
.Reldate   Init      "2013 June 3"
.Release   Init      "2.5"     DLH   Sunbelt PDF
.Reldate   Init      "2013 April 16"
.Release   Init      "2.4"     DLH   64 bit os
.Reldate   Init      "Dec 5 2011"
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
.begin 3.00 
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
books   automation
book    automation
sheets  automation
Range1    automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight   variant
VT_R8         EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom80  variant
.
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
.
AllMargin     variant
xlColWidth    variant
xlLandscape integer 4,"0x2"                     .2
xlAlignRight integer 4,"0xFFFFEFC8"
xlAlignLeft integer 4,"0xFFFFEFDD"
xlPaperLegal integer 4,"0x5"
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
range         dim             20
range2        dim             20
XLSName    Dim        255
Samples    dim        1                     .if no samples in excel mode it holds "N"
.end patch 3.00 

FileCheck FIle
trapcount form      4

str60     dim       60
.Counters
COUNTR  FORM    9
COUNTR2 FORM    9
.Files to open
prfile  pfile
input2  file
input2i ifile    Name="NINPRINT.isi|NINS1:502"
.>Patch 2.10
input3  file
First   init    "Y"
GoodStat init   "zlpx"
PrtFlag dim     1
HOLDOWN dim     4
HOLDLIST dim    6
.begin patch 3.10
Holdnsmpdes1          Dim        30
HoldOco2code          Dim        2
.end patch 3.10
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
.END PATCH 1.99 REPLACED LOGIC
.begin patch 3.10
.dcxext  init    ".TIF"
DCX2      dim       30
DCXext  INIT      ".PDF"
SMPmailBody    dim         25000       .for sample links
.end patch 3.10

FilePath dim    40
.begin patch 3.10
.SMPArray dim    9(50)                       .compnumber,sample number
.SMPIndex form   2
.end patch 3.10

PrintFlag form  "00"    .Default is Laser3!!!  01=Laser 3, 02=PDF Email, 03=Laser 2, 04=PDF dump, 05=fax, 06=Excel

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
FaxfLAG  iNIT      "N"
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
.begin patch 2.5
.          Call      GetPDFPath
.end patch 2.5
.begin patch 3.10
PackData Datalist                           .Holds sample data
          create    PackData=1:1:1:1
.end patch 3.10


        match   "NLCR0038",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0038",PROGRAM
                PACK    input2Name,NTWKPATH1,"NPRINT2.LCR"
                PACK    output1,NTWKPATH1,"LCRFILE.DAT"
                PACK    output2,NTWKPATH1,"LCRFILE.SRT"
.begin patch 2.62
                      move       c0,func                                                .force flag off                             
.end patch 2.62
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
.begin patch 2.5
.                                        call       GetPDFPath
.                                        pack      str45 from PDFPATH,"\res\pdf995.ini"
.
..                                        call      "GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                                        pack      str45 from PDFPATH,"\res\pdf995.ini"
.                                        call      "GU$INI;WRITE_TO_INI" USING Str45:
.                                                  "Parameters":
.                                                  "ProcessPDF":
..                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                             "\\nins1\e\apps\plb\code\pdftest.bat":
.                                                  result
.
..                                        move      "\\nins1\e\apps\plb\code\pdftest.bat",str35
..
..                                       call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
..                                                 "Parameters":
..                                                 "ProcessPDF":
..                                                 "\\nins1\e\apps\plb\code\pdftest.bat":
..                                                 result
.                                        if (result = C0)
..Prepare Flag file
.                                                  move      C1,PDFFlag
.                                                  pack      str45 from PDFPATH,"\flag.dat"
..                                                  prep      tempfile,"c:\progra~1\pdf995\flag.dat"
..                                                  write     tempfile,SEQ;"flag set"
.                                                  prep      tempfile,Str45
.                                                  write     tempfile,SEQ;"flag set"
.                                                  close     tempfile
.                                        else
.                                                  move      C2,PDFFlag
.                                        endif
.                                        if (PDFFlag = C2)
..Send message to I.S.
.                                                  move      "This is a message from       NLCR0038",MailSubjct
.                                                  CLear     MailBody
.                                                  Append    "Failure to update pdf995.ini.",MailBOdy
.                                                  append    "<br>",MailBody
.                                                  Append    "Field: ",Mailbody
.                                                  append    str25,Mailbody
.                                                  append    "<br>",MailBody
.                                                  Append    "Value: ",Mailbody
.                                                  append    str35,mailbody
.                                                  append    "<br>",MailBody
.                                                  reset     Mailbody
.                                                  move      "InformationServices@nincal.com",Mailto
.                                                  move      "InformationServices@nincal.com",MailFrom
.                                                  
.                                                  call      SendMail
.                                                  move      C0,PDFFlag
.                                        endif
.end patch 2.5

                              endif
                endif
.begin patch 3.0
                     DISPLAY *P1:24,*el,"Sorting";

.                    if (PrintFlag = 4 | PrintFlag = 2)
                    if (PrintFlag = 4 | PrintFlag = 2 | PrintFlag = 6)
.end patch 3.0
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
        PACK    STR35,"NINPRINT.ISI|NINS1:502"
        open    input2i,STR35
        prepare input3,output1
        if (PrintFlag = 0)
                DISPLAY *P1:24,"READING NINPRINT FILE";
        endif
LCRLoop
        loop
                read    input2,seq;ordvars
                until over
.START PATCH 2.6 ADDED LOGIC
                    if (OSTAT <> "l" OR OHIST <> "t")       .Prevent Tentative Approvals from appearing on report
.END PATCH 2.6 ADDED LOGIC
                              call    OrderReadOtherFiles
                              call    OrderReadPendFiles
.START PATCH 2.6 ADDED LOGIC
                              if (OSTAT <> "l" OR NORD5STAT <> "9")   .extra safey precaution to prevent Tentative Approvals from appearing on report
.END PATCH 2.6 ADDED LOGIC
                                filepi  1;input3
.begin patch 2.3
.                                        write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
.                                                  OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,NSEL2NAME,OFDESC:
.                                                  OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
.                                                  OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL
.patch 3.0 add order date
                                        write   input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                                                  OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                                                  OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                                                  OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL:
                                                  oodtec,oodtey,oodted,oodtem
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
.START PATCH 2.6 ADDED LOGIC
                              endif
                    endif
.END PATCH 2.6 ADDED LOGIC
        repeat

        if (PrintFlag = 0)
                DISPLAY *P1:24,"SORTING LCRFILE FILE ";
        endif
.Sortfile sorting by list owner #,List #, lr #
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
.begin patch 2.5
.          call      pdf995auto0
.end patch 2.5
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
.begin patch 3.10 .build sample data
          open      input3,output2,exclusive
          
          loop
                    read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                              OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                              OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                              OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL:
                                 oodtec,oodtey,oodted,oodtem
                   until over
                      IF         (OSCODE = "1")           .sample enclosed
                                
                      trap      IOMssg giving Error if IO
                      move      "COMPKEY3",Location
                      pack      COMPFLD3,OMLRNUM
                      pack      KeyLocation,"Key: ",COMPFLD3
                      call      COMPKEY3
                      move      OLON,NOWNFLD
                      rep       zfill,NOWNFLD
                      call      NOWNKEY
                      call       trim using osamcde
.                      pack      str18,OCO2CODE,OLon,COMPNUM,OSAMCDE
                      pack      str25,OCO2CODE,OLon,olnum,COMPNUM,OSAMCDE
                      pack      str9,COMPNUM,OSAMCDE

.                      PackData.FindString giving result using str18,SEQ
                      call       debug
                      PackData.FindString giving result using str25,SEQ
                     if (result = SEQ & OWNBLK <> "1")             .ownblk = 1 means owner does not allow samples
                      move    "                                        ",APIFileName
                      clear   APIFileName
                      pack    APIFileName,dcxpath,str9,dcxext,hexzero
                      call    FindFirstFile
                                        if (result = SEQ & OWNBLK <> "1" & Osamcde <> ""  & Osamcde <> "000")             .ownblk = 1 means owner does not allow samples
.                                                  insertitem PackData,999999,str12
                                            if (APIResult <> 0 & APIResult <> hexeight)
                                            unpack  APIFileData,str32,str4
                                            move    str4,testint
                                                       if (testint > "10000")  .minimum file size in bytes
                      call debug
.                                                       insertitem PackData,999999,str18
                                                       insertitem PackData,999999,str25
                                                       else
                                                       call    OrderBadSample
                                                       endif
                                            else
                                            call    OrderBadSample
                                            endif
                                        endif   
                                 endif
                    endif
          Repeat            
           Close    Input3
.end patch 3.10

          open      input3,output2,exclusive
          loop
.begin patch 2.3
.patch 3.0 add order date
                    read    input3,seq;OLON,OLNUM,OMLRNUM,OLRN,MCOMP,O1DES,OWNLONM,OSTAT:
                              OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC,OWNFAX,OWNTELE,OwnEmail,NSEL2NAME,OFDESC:
                              OMDTEM,OMDTED,OMDTEC,OMDTEY,ORENT,OELCODE,OQTY,OCO2CODE:
                              OSCODE,OSAMCDE,NSMPDES1,NORD5STAT,DESC001,DESC002,CNTNAME,CNTPHONE,UNIVERSE,NPNDDESC,COMPNUM,HOLDEXCL:
                                 oodtec,oodtey,oodted,oodtem
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
.emtpy the array
.           for        SMPINDEx From "1" to "50"
.           move       b1,SMPArray(SMPIndex)
.           repeat
                                PRTCLOSE prfile
.This will only happen if called from Program 1
..................................................
.REview this
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
.begin patch 2.5

.                                                            pack      APIFileName from PDFPATH,"\flag.dat"
.
..                                                            pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                                                            for N3,C1,"100"
.                                                            repeat
.                                                            pause     "5"
.end patch 2.5
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
.begin patch 3.10
                        Move     oco2code,HoldOco2code
.end patch 3.10
                        clear   page
.begin patch 3.0
                      if         (printflag = 6)
                      call       OrderHeaderXls
                      else

                        call    OrderPrintHeader
                        call    OrderListHeader
                      endif
.end patch 3.0
                endif
                call    OrderPrintRecord
.                     call debug
        repeat

LastRec
.begin patch 3.0
           IF         (PrintFlag = 6)
.excel mode lets do some formating
           move       howmany,str9
           call       trim using str9
           pack       Range,"a12"
          pack        Range2,"f",str9
          sheet.range(Range,Range2).Columns.Autofit
          pack        Range,"H12"
          pack        Range2,"O",str9
          sheet.range(Range,Range2).Columns.Autofit

           setprop    xlColWidth,VarValue="24.43"
           pack       range,"g12"
           pack       range2,"g",str9
           setprop    sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
           pack       range,"k12"
           pack       range2,"K",str9
           setprop    sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth
           setprop    xlColWidth,VarValue="37.20"
           pack       range,"p12"
           pack       range2,"p",str9
           setprop    sheet.range(range,range2).Columns,*ColumnWidth=xlColWidth

           move       howmany,str9
           call       trim using str9
           pack       Range,"a12"
          pack        Range2,"p",str9
          sheet.range(Range,Range2).Rows.Autofit        


.if in excel mode we need to save the file
CampaignFileNameSelect
           Clear      Taskname
           clear   str45
           append  Holdown,str45
           append  "_",str45
           append  Holdlist,str45
                      if (PrintFlag = 4)
           clock     timestamp,timestamp
           append  "_",str45
           append    timestamp,str45
           endif
           reset   str45
           Append    "C:\WORK\",taskname                          ."
           APPEND     STR45,TASKNAME
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                      if        (#ver = c1)
                      append  ".xlsx",taskname
                      else
           append  ".xls",taskname
                    endif
                    Reset     Taskname                              
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapCampaignObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CampaignCleanUp
.Clean up after myself
                              destroy        OTRUE
.All automation objects need to be destroyed before you close down spreadsheet!
                              destroy sheet
                              destroy sheets
                              destroy book
                              destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                              setprop ex,*DisplayAlerts=OFALSE
                              destroy        OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
           Move       Taskname,XLSName
           endif
.end patch 3.0

        call    PrintSamplePage
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
.This will only happen if callded from Program 1
.Begin patch 3.0
.        if (PrintFlag = 2 | PrintFlag = 4)
        if (PrintFlag = 2 | PrintFlag = 4 | PrintFlag = 6)
.end patch 3.0
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
.begin patch 2.5
.                              pack      APIFileName from PDFPATH,"\flag.dat"
..                              pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
.                              pause     "5"
.end patch 2.5
                    endif
                    call    OrderCreatePDFFile
.begin patch 2.5
.                    move      "ProcessPDF",str25
.                    clear     str55
.                    call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
.                    call      "GU$INI;WRITE_TO_INI" USING Str45:
.                              "Parameters":
.                              "ProcessPDF":
.                              "":
.                              RESULT
...                    call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
..                              "Parameters":
..                              str25:
..                              "":
..                              result
.                    if (result = 0)
.                              move      C1,PDFFlag
.                    else
.                              move      C2,PDFFlag
.                    endif
.                    if (PDFFlag = C2)
..Send message to I.S.
.                              move      "This is a message from       NLCR0038",MailSubjct
.                              Clear     Mailbody
.                              Append    "Failure to re-update pdf995.ini.",Mailbody
.                              append    "<br>",MailBody
.                              append    "Pdf995.ini has been modified.",Mailbody
.                              append    "<br>",MailBody
.                              Append    "Field: ",Mailbody
.                              append    str25,Mailbody
.                              append    "<br>",MailBody
.                              append    "Value: ",Mailbody
.                              append    str35,Mailbody
.                              append    "<br>",MailBody
.                              reset     Mailbody
.                              move      "InformationServices@nincal.com",Mailto
.                              move      "InformationServices@nincal.com",Mailfrom
.                              call      SendMail
.                              move      C0,PDFFlag
.                    endif
.end patch 2.5

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
.begin patch 2.5
.          call      pdf995auto
.end patch 2.5
        shutdown

OrderCreatePDFFile
          if (PrintFlag = 4)
.File creation on harddrive!
                    call      EmailUserPDF
.begin patch 3.0
          Elseif (PrintFlag = 6)
                    call      EmailUserXLS
.end patch 3.0
                    return
          endif
.          PAUSE       C10
          PAUSE       C5
.begin patch April 26th 2010
                    call      testclient
                    if        (ClntServFlag = c1)
.begin patch 3.0
                                 if         (printflag = 6)
.add code to check for sample if present need both files else just xls
                               Pause     "20"
                               pack      MailAttach from "c:\work\pdf\",str45,".pdf"              ."
                               pack      taskname from "!c:\work\pdf\",str45,".pdf"              ."
                               Pause     C5
                               copyfile  taskname,mailattach          
                               Pause     "10"
                               pack      MailAttach from "c:\work\",str55              ."
                               pack      taskname from "!c:\work\",str55              ."
                               Pause     C5
                               copyfile  taskname,mailattach          
                               Pause     "10"

.                     pack       MailAttach from

                      Else
                               Pause     "10"
                               pack      MailAttach from "c:\work\pdf\",str45,".pdf"              ."
                               pack      taskname from "!c:\work\pdf\",str45,".pdf"              ."
                               Pause     C5
                               copyfile  taskname,mailattach          
                               Pause     "10"
.                    Pause     "20"
.                    pack      MailAttach from "c:\work\pdf\",str45,".pdf"              ."
.                    pack      taskname from "!c:\work\pdf\",str45,".pdf"              ."
.                    Pause     C5
.                    copyfile  taskname,mailattach          
.                    Pause     "20"
                                 endif
.end patch 3.0
                    endif
.end patch April 26th 2010

          Move      c0,Trapcount
.begin patch 2.3
          call      Trim using CNTNAME
.begin patch 2.62
.          if        (CNTNAME <> "" & CNTNAME <> "BILLING")
          if        (CNTNAME <> "" & CNTNAME <> "BILLING" & Func <> "1")
.end patch 2.62
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
.begin patch 2.5
.                    call      waitin using "9000"
.end patch 2.5
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
.          Move      "360",MailTimer
          Move      "50",MailTimer
.begin patch 3.10
            move      c1,MailType         .force e-mail body  message type
         
          Clear       mailbody
          append  "<br>",MailBody
          Reset       smpmailbody
          call        trim using smpmailbody
          count       n5,smpmailbody
          if          not zero
          append      smpmailbody,mailbody 
          append  "<br>",MailBody
           endif
          Reset     MailBody
           Clear      SMPmailbody

.end patch 3.10
          call      SendMail
.test test test
          Clear       mailbody
           Clear      SMPmailbody
.test test test


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
          Append    "<br>",MailBody
          clear   taskname
        unpack  str9,str6,str3
        pack    taskname,"LR: ",OLRN,B1,"MAILER: ",str6,B1,"SAMPLE: ",str3
          append    taskname,Mailbody
          Append    "<br>",MailBody
          reset     Mailbody
          MOVe      "ComputerRequest@nincal.com",Mailfrom
          pack      Mailto from Mailfrom,",",user,"@nincal.com"
.begin patch 3.10
            move      c1,MailType         .force e-mail body to HTML message
.begin patch 3.10
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
                                move        yes,faxflag
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
.dave goes bad
        Trap SPOOL1 giving error if SPOOL

                if (PrintFlag = C0 | PrintFlag = 3)     .Laser3 = Default
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 1)  .Laser8
                        if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                PRTOPEN prfile,"\\NINs2\Laser8","FAXFILE.PRN"
                        elseif (osflag = c3 | osflag =c4)         .win 95 98
                                PRTOPEN prfile,"Laser8","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 2 | PrintFlag = 4)  .PDF
.begin patch 2.5
.          call      pdf995auto
.end patch 2.5

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
.begin patch 2.5
                              pack      str55 from "c:\work\pdf\",str45,".pdf"
                              PRTOPEN prfile,"PDF:",str55
                              pack    str55,str45,".pdf"
.                              PRTOPEN prfile,"PDF995",str45
.                              pack    str55,str45,".pdf"
.end patch 2.5
.begin patch 3.0
                elseif (PrintFlag = 6)  .XLS
CreateSheet
.Create the Variant objects
.Booleans
               create  OTRUE,VarType=VT_BOOL,VarValue=1
               create  OFALSE,VarType=VT_BOOL,VarValue=0
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                      setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
               getprop ex,*Workbooks=books
.Create/Add a single Workbook
               books.add
               books.item giving book using 1
.Create Worksheets collection
               getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
               sheets.item giving sheet using 1
               setprop sheet.PageSetup,*Orientation=xlLandscape
               setprop sheet.PageSetup,*Zoom=Zoom80
               setprop sheet.PageSetup,*TopMargin=AllMargin
               setprop sheet.PageSetup,*BottomMargin=AllMargin
               setprop sheet.PageSetup,*RightMargin=AllMargin
               setprop sheet.PageSetup,*LeftMargin=AllMargin
.               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal

               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.               sheet.range("A1:E1").Merge    .error why  2015 March 17
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
           setprop         sheet.Range("a1:c1"),*Formula=taskname
               sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75

.          setprop    ex,*visible="True"
               
......................................................
.master heading
           setprop    sheet.Range("A3"),*Value="To:"                         
           setprop    sheet.Range("H3"),*Value="From:"                       
           setprop    sheet.Range("A8"),*Value="Via:"                        
           setprop    sheet.Range("h8"),*Value="Date:"                       
           setprop    sheet.Range("A10"),*Value="List:"                                 
           
......................................................
.detail heading
               setprop        sheet.Range("A12"),*Value="Date"
               setprop        sheet.Range("B12"),*Value="LR Number"
               setprop        sheet.Range("C12"),*Value="Mailer"
               setprop        sheet.Range("D12"),*Value="Offer"
               setprop        sheet.Range("e12"),*Value="Mail Date"
               setprop        sheet.Range("f12"),*Value="Quantity"
               setprop        sheet.Range("G12"),*Value="Select"
               setprop        sheet.Range("h12"),*Value="LO Owes"
               setprop        sheet.Range("i12"),*Value="LO is Owed"
               setprop        sheet.Range("j12"),*Value="Reco."
               setprop        sheet.Range("k12"),*Value="Sample Description / Attached"
                sheet.range("k12:l12").Merge
               setprop        sheet.Range("m12"),*Value="Status"
               setprop        sheet.Range("N12"),*Value="Comments"
               setprop        sheet.Range("O12"),*Value="Answer"
               setprop        sheet.Range("p12"),*Value="Additional"
.
.
               setprop xlRowHeight,VarValue="27.0"
               setprop        sheet.range("A3:S12").Rows,*RowHeight=xlRowHeight
.Header Formatting
               setprop        sheet.Range("A2:S12"),*HorizontalAlignment=xlAlignCenter
               setprop        sheet.Range("A2:S12").Font,*Bold="True"
.               //Setting up 2 sets of Borders so that user is clear that
.               //the second portion does not actually print
               sheet.range("A12:P12").BorderAround using *LineStyle=1,*Weight=MedThick
.               sheet.range("P8:U8").BorderAround using *LineStyle=1,*Weight=MedThick
               move           "12",howmany
.end patch 3.0
                endif
                ADD     C1,COUNTR2
        else 
.begin patch 2.3
.fax change to use PDF & check for Email
.begin patch 2.5
.                    call      pdf995auto

                    clear   str45
                    append  OLON,str45
                    append  "_",str45
                    append  OLNUM,str45
                    reset   str45
                    pack      str55 from "c:\work\pdf\",str45,".pdf"
                    PRTOPEN prfile,"PDF:",str55
                    pack    str55,str45,".pdf"

.                    PRTOPEN prfile,"PDF995",str45
.                    pack    str55,str45,".pdf"
.end patch 2.5

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
                 MOve No,Faxflag
                ADD     C1,COUNTR
                    scan      "@",OwnEmail
                    if        equal
                    Reset     OwnEmail
                    move      OwnEmail,Mailto
                    else
.                    pack      MailTo,"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                    pack      MailTo,"+",longdist,faxnum,"@fax.nincal.com"
                      MOve Yes,Faxflag
                    endif
         Reset     OwnEmail
         append     cntname,MailBody
         append     "<br>",MailBody
         append     ownocpy,Mailbody
         append     "<br>",MailBody
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
        prtpage prfile;*p5200:row,"DID:";
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
.          If        (emailFlag = Yes)
          If        (emailFlag = Yes & FaxFlag <> Yes)
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
        prtpage prfile;*p5200:row,"FAX:";
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
.          If        (emailFlag = Yes)
          If        (emailFlag = Yes & FaxFlag <> Yes)
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
.begin patch 3.0
           if         (printflag = 6)
                      call       OrderDetailXls
           else
.end patch 3.0
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderListHeader
        endif

        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN,*OVERLAYON;
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
.begin patch 3.0
           endif
.end patch 3.0

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
.begin patch 3.0
.begin patch 3.10   No need for pdf as will be part of email
.        move    C1,SMPIndex
.           if (PrintFlag = 6)  .XLS
..if we are in excel mode we need to create the pdf file
..check if any samples
.           clear   str9
.           move    SMPArray(SMPIndex),str9
.           call    Trim using str9
.                      if (str9 = "")
.           move       No,Samples
..NO SAMPLES
.                      ELSE
.                      clear   str45
.                      append  Holdown,str45
.                      append  "_",str45
.                      append  Holdlist,str45
.                                 if (PrintFlag = 4)
.                      clock     timestamp,timestamp
.                      append  "_",str45
.                      append    timestamp,str45
.                      endif
.                      reset   str45
.                      pack      str55 from "c:\work\pdf\",str45,".pdf"
.                      PRTOPEN prfile,"PDF:",str55
.                      pack    str55,str45,".pdf"
.                      prtpage prfile;*UNITS=*HIENGLISH;
.                      endif
.           ENDIF      
.begin patch 3.10   No need for pdf as will be part of email
.end patch 3.0

          call        SMPRecClear
.end patch 2.8
                    mOVE         nsmpdes1,Holdnsmpdes1  
                    PackData.GetCount giving howmany
                    if (howmany > C0)
.                             add       howmany,ownpcnt
                              for result,"1",howmany
.begin patch 2.8  add contact and owner to datalist 
.                                        getitem   PackData,result,DCX2
                                        getitem   PackData,result,taskname
.                                        unpack         taskname into str2,str4,dcx2
                                        unpack         taskname into str2,str4,str6,dcx2
.                                        if (str4 = Holdown & str2 = Holdoco2code)
                                        call   debug
                                        if (str4 = Holdown & str2 = Holdoco2code & str6 = holdlist)
.end patch 2.8  add contact and owner to datalist 
.begin patch 2.8
                                        pack      DCXFile,filePath,"s",DCX2,DCXExt
.
                                                      move      "SAM1-COMPKEY",Location
                                                      unpack      dcx2 into compnum,osamcde 
                                                      pack      COMPFLD,compnum
                                                      pack      KeyLocation,"Key: ",COMPFLD
                                                      call      COMPKEY
                                                      pack      NSMPFLD,COMPNUM,OSAMCDE
                                                      rep       zfill,NSMPFLD
                                                      move      "SAM1-NSMPKEY",Location
                                                      pack      KeyLocation,"Key: ",NSMPFLD
                                                      call      NSMPKEY
                                                call   trim using compcomp
                                                call   trim using nsmpdes1
                                                clear   taskname
                                                pack    Taskname from "<a href=#"http://www.nincal.com/data/samples/S",dcx2,dcxext,"#">",compcomp,dash,nsmpdes1,"</a>"
                                                append  Taskname,SmpMailBody
                                                append  "<br>",SmpMailBody
 
                                          endif
                              repeat
.begin patch 3.11 - because of the way breaks work in this program we need to ensure that once used they are not picked up again, so we delete them from the datalist
. this could be incorporated into the above loop. however this seemed cleaner as the delete appears to mess up the loop and you have to get the new count after and start
. again.
............
smpcleanup          PackData.GetCount giving howmany
                    if (howmany > C0)
                              for result,"1",howmany
                                        getitem   PackData,result,taskname
                                        unpack         taskname into str2,str4,str6,dcx2
                                        call   debug
                                        if (str4 = Holdown & str2 = Holdoco2code & str6 = holdlist)
                                        Deleteitem Packdata,result       
                                        break
                                        goto smpcleanup
                                        endif    
                    repeat  
                    endif
.end patch 3.11
.............

                    endif
                    mOVE         Holdnsmpdes1,nsmpdes1

printstop
.end patch 3.10
        return
trapit
        return
EmailUserPDF
          Clear     Mailbody
          append    "This is a message from       the Galley Report Program",Mailbody
          Append    "<br>",MailBody                                               
          Append    "Your PDF file was created!",Mailbody
          Append    "<br>",MailBody                                               
          append    "Location:  c:\work\pdf\ ",mailbody
          Append    "<br>",MailBody                                               
          append    "filename:  ",mailbody
          append    str55,mailbody
          Append    "<br>",MailBody                                               
.begin patch 3.10
          Reset       smpmailbody
          call        trim using smpmailbody
          count       n5,smpmailbody
          if          not zero
          append      smpmailbody,mailbody 
          append  "<br>",MailBody
           endif
           Clear      SmpMailbody
.end patch 3.10
          Reset     MailBody
.begin patch April 26th 2010
                    call      testclient
                    if        (ClntServFlag = c1)
.                    Pause     "20"
                    call      waitin using "1000"
                    pack      MailAttach from "c:\work\pdf\",str55              ."
                    pack      taskname from "!c:\work\pdf\",str55              ."
                    Pause     C5
                    copyfile  taskname,mailattach          
.                    Pause     "20"
                    call      waitin using "1000"
                    endif
.end patch April 26th 2010

                    
          pack      mailattach,"c:\work\pdf\",str55                  ."
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "Your PDF file was created ",Mailsubjct
.begin patch 3.10
            move      c1,MailType         .force e-mail body to HTML message
.begin patch 3.10
          call      SendMail
.end patch 2.14
.test test test
           Clear      SmpMailbody
           Clear      Mailbody
.test test test

          return
.begin patch 3.0
.add code to check for samples
EmailUserXLS
          Clear     Mailbody
          append    "This is a message from the Galley Report Program",Mailbody
          Append    "<br>",MailBody                                               
          Append    "Your file(s) have been created!",Mailbody
          Append    "<br>",MailBody                                               
.begin patch 3.10
.           if         (samples = No)
          append    "Location:  C:\work\ ",mailbody
.           else
.          append    "Location:  C:\work\ & c:\work\pdf\ ",mailbody
.           endif
.end patch 3.10
          Append    "<br>",MailBody                                               
          append    "filename(s):  ",mailbody
          Append    "<br>",MailBody                                               
           append     XlsName,Mailbody
          Append    "<br>",MailBody                                               
          append    str55,mailbody
          Append    "<br>",MailBody                                               
.begin patch 3.10
          Reset       smpmailbody
          call        trim using smpmailbody
          count       n5,smpmailbody
          if          not zero
          append      smpmailbody,mailbody 
          append  "<br>",MailBody
           endif
           Clear      SMPmailbody
.end patch 3.10
          Reset     MailBody
.cleanup mailattach names ,etc
                    call      testclient
                    if        (ClntServFlag = c1)
                    call      waitin using "2000"
                                 if         (samples <> No)
                               pack      MailAttach from "c:\work\pdf\",str55              ."
                               pack      taskname from "!c:\work\pdf\",str55              ."
                               Pause     C5
                               copyfile  taskname,mailattach          
                                 endif
                    call      waitin using "2000"
                    pack      MailAttach from XlsName              ."
                    pack      taskname from "!",Xlsname              ."
                    Pause     C5
                    copyfile  taskname,mailattach          
                    call      waitin using "2000"
                    endif

                    
.begin patch 3.10
.           if         (samples <> No)
.          pack      mailattach,"c:\work\pdf\",str55,";",xlsname                  ."
.           else
          pack      mailattach,xlsname  
.          endif
.end patch 3.10
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "Your file(s) have been created!",Mailsubjct
.begin patch 3.10
            move      c1,MailType         .force e-mail body to HTML message
.begin patch 3.10
          call      SendMail
.test test test
           Clear      SmpMailbody
           Clear      Mailbody
.test test test

          return
.end patch 3.0
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
                    append    "<br>",MailBody
                    append    str55,MailBody
                    append    "<br>",MailBody
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
.begin patch 3.10
                   move      c1,MailType         .force e-mail body to HTML message
.begin patch 3.10
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
.Begin  patch 3.0
.XLS Heading
OrderHeaderXLS
           clock   date to date
           unpack  date,MM,STR1,DD,STR1,YY
           pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
           setprop    sheet.Range("B3"),*Value=ownlonm,*HorizontalAlignment=xlAlignLeft
           sheet.range("B3:C3").Merge
           setprop    sheet.Range("I3"),*Value=CntName,*HorizontalAlignment=xlAlignLeft
           sheet.range("I3:J3").Merge
           setprop    sheet.Range("B4"),*Value=CompHold,*HorizontalAlignment=xlAlignLeft
           sheet.range("B4:C4").Merge
           pack       str25 from cntphone," DID"
           setprop    sheet.Range("I4"),*Value=str25,*HorizontalAlignment=xlAlignLeft
           sheet.range("i4:j4").Merge
           setprop    sheet.Range("B5"),*Value=OwnLoSa,*HorizontalAlignment=xlAlignLeft
           sheet.range("B5:C5").Merge
          clear     str60
          call      Trim using CNTNAME
          if (CNTNAME <> "" & CNTNAME <> "BILLING")
                    call      RemoveChar using CNTNAME,B1
                              pack      str60,CNTNAME,"@nincal.com"
          endif
           setprop    sheet.Range("I5"),*Value=str60,*HorizontalAlignment=xlAlignLeft
           sheet.range("I5:k5").Merge
           call    Trim using OWNLOCTY
           if (OWNLOCTY <> "")
                pack    taskname,OWNLOCTY,COMMA,OWNLOS,B1,OWNLOZC
           setprop    sheet.Range("B6"),*Value=taskname,*HorizontalAlignment=xlAlignLeft
           sheet.range("B6:C6").Merge
           endif
. indicate fax or email
          If        (emailFlag = Yes & Faxflag <> Yes)
           setprop    sheet.Range("B8"),*Value="EMAIL:"
           setprop    sheet.Range("c8"),*Value=OwnEMail,*HorizontalAlignment=xlAlignLeft
           sheet.range("c8:d8").Merge
          else
           setprop    sheet.Range("B8"),*Value="FAX:"
          call    Trim using OWNFax
           if (OWNfax <> "")
                      unpack  OWNfax,str3,str2,str1,str4
                      pack    taskname,"(",str3,") ",str2,str1,"-",str4," Fax"
                                 setprop    sheet.Range("c8"),*Value=taskname,*HorizontalAlignment=xlAlignLeft
                      sheet.range("c8:d8").Merge
           
           endif
          endif
           setprop    sheet.Range("I8"),*Value=newdate1,*HorizontalAlignment=xlAlignLeft
           sheet.range("I8:J8").Merge

.........................................
        pack    NCNTFLD,OCO2CODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
          move      c1,NCntPath
        call    NCNTKEY
                    if (CNTTEAM = "05") .List Management
                      setprop    sheet.Range("I6"),*Value="(415) 628-8313 Fax",*HorizontalAlignment=xlAlignLeft
                    else
                      setprop    sheet.Range("I6"),*Value="(415) 433-7796 Fax",*HorizontalAlignment=xlAlignLeft
                    endif
          sheet.range("I6:J6").Merge
          call    Trim using OWNTELE
           if (OWNTELE <> "")
                      unpack  OWNTELE,str3,str2,str1,str4
                      pack    taskname,"(",str3,") ",str2,str1,"-",str4
                                 setprop    sheet.Range("b6"),*Value=taskname,*HorizontalAlignment=xlAlignLeft
           endif
           setprop    sheet.Range("B10"),*Value=O1DES,*Wraptext=OTRUE,*HorizontalAlignment=xlAlignLeft
          sheet.range("B10:C10").Merge
           move       "12",howmany
        return

.........................................
orderDetailXLS
.LR
           add        c1,Howmany
           move       howmany,str9
           call       Trim using str9
           pack       str12,"B",str9
           setprop    sheet.Range(str12),*Value=OLRN
.Create Date
           move       howmany,str9
           call       Trim using str9
           pack       str12,"A",str9
           pack       str14 from oODTEM,SLASH,oODTED,SLASH,oODTEC,oODTEY
           setprop    sheet.Range(str12),*Value=str14

.status
           move       howmany,str9
           call       Trim using str9
           pack       str12,"M",str9
           if (OSTAT = "l" | OSTAT = "z")
           setprop    sheet.Range(str12),*Value=NPNDDESC
           else
           setprop    sheet.Range(str12),*Value="Pending Order"
           endif
.maildate
           move       howmany,str9
           call       Trim using str9
           pack       str12,"E",str9

           call    TRIM using OMDTEM
           count   N2,OMDTEM
           if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
           pack       str14 from OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
           setprop    sheet.Range(str12),*Value=str14
           
           elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
           setprop    sheet.Range(str12),*Value="As Soon As Possible"
           
           elseif (OMDTEM = "11" AND OMDTED = "11"  AND OMDTEC = "11" AND OMDTEY = "11")
           setprop    sheet.Range(str12),*Value="See Additional"
           endif
.sample
           move       howmany,str9
           call       Trim using str9
           pack       str12,"L",str9
           if (OSCODE = "1")
           setprop    sheet.Range(str12),*Value="X"
          sheet.range(str12).BorderAround using *LineStyle=1,*Weight=2
           move       howmany,str9
           call       Trim using str9
           pack       str12,"k",str9
           setprop    sheet.Range(str12),*Value=NSMPDES1
           endif
.Mailer
           move       howmany,str9
           call       Trim using str9
           pack       str12,"C",str9
           setprop    sheet.Range(str12),*Value=Mcomp,*HorizontalAlignment=xlAlignLeft
.quantity
           move       howmany,str9
           call       Trim using str9
           pack       str12,"F",str9


          move    C0,N9
          move    OQTY,N9
          if (OELCODE = "1" OR OELCODE = "3")
                    if (N9 > 0)
                              move    EditMask,str11
                              edit    N9,str11
                              call      Trim using str11
                              pack      EditQuan,str11,"/ALL"
                    else
                              move    "All",EditQuan
                    endif
          else
                    move    EditMask,EditQuan
                    edit    N9,EditQuan
          endif
           setprop    sheet.Range(str12),*Value=EditQuan
.reco
           move       howmany,str9
           call       Trim using str9
           pack       str12,"J",str9

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
           setprop    sheet.Range(str12),*Value=str9
.offer
           move       howmany,str9
           call       Trim using str9
           pack       str12,"D",str9
           setprop    sheet.Range(str12),*Value=OFDESC
.select
           move       howmany,str9
           call       Trim using str9
           pack       str12,"G",str9
           call    TRIM using NSEL2NAME
           if (NSEL2NAME <> "")
           setprop    sheet.Range(str12),*Value=NSEL2NAME,*Wraptext=OTRUE
           endif
.special/additional instructions
.note will need to set width and wrap
           move       howmany,str9
           call       Trim using str9
           pack       str12,"P",str9
           pack       str2,carr,B1
           rep        str2,DESC002

           call    TRIM using DESC002
           if (DESC002 <> "")
           setprop    sheet.Range(str12),*Value=DESC002,*Wraptext=OTRUE
           endif
.exchange status

           move       howmany,str9
           call       Trim using str9
           call       Trim using mcomp
           match      Mcomp,desc001
           if         equal
                      scan       "owes ",desc001
                      if         equal
                      pack       str12,"I",str9
                      call       getexstat
                      setprop    sheet.Range(str12),*Value=str20
                      endif
           Else
                      scan       "owes ",desc001
                      if         equal
                      pack       str12,"H",str9
                      call       getexstat
                      setprop    sheet.Range(str12),*Value=str20
                      endif
           endif
.answer
           move       howmany,str9
           call       Trim using str9
           pack       str12,"O",str9
          sheet.range(str12).BorderAround using *LineStyle=1,*Weight=2

           return
......................................................
getExstat
           bump       desc001,5
           MOVEFPTR   desc001,n8
bump1
           bump       desc001,1
           cmatch     b1,desc001
           goto       bump1 if not equal
           MOVEFPTR   desc001,n9
           sub        c1 from n9
           reset      desc001,n8
           SETLPTR    desc001,n9
           MOve       desc001,str20
           return
......................................................
TrapCampaignObject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
        scan    "Cannot access",str55
        if equal
.Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect

.end patch 3.0
Spool1
         Display    *P10:11,*EF,*cyan,error
         Pause c2
         TRAPCLR   SPOOL
         Trap SPOOL1 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
         Display    *P10:11,*EF,*cyan,"Trying Again"
                if (PrintFlag = C0 | PrintFlag = 3)     .Laser3 = Default
                        if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN prfile,"laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 1)  .Laser8
                        if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                PRTOPEN prfile,"\\NINs2\Laser8","FAXFILE.PRN"
                        elseif (osflag = c3 | osflag =c4)         .win 95 98
                                PRTOPEN prfile,"Laser8","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                elseif (PrintFlag = 2 | PrintFlag = 4)  .PDF
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
                              pack      str55 from "c:\work\pdf\",str45,".pdf"
                              PRTOPEN prfile,"PDF:",str55
                              pack    str55,str45,".pdf"
                endif
         endif
         return
.begin patch 3.10
SMPRecClear
               Clear        SmpMailbody  
               Return
.end patch 3.10


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
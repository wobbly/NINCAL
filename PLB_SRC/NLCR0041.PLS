........................................
. Program:      NLCR0041.PLS
. Function:     LM Outside Broker Report Program (produced on Screen5 of program 1)
. Author:       Andrew Harkins
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
.        Include Smtp.PRI  (PRI - Profile Routine Interface)
        include norddd.inc
        include nord5dd.inc
        include nord4dd.inc
        include nordPdd.inc
          include   compdd.inc
          include   cntdd.inc
        include nxrfdd.inc
        include ndatdd.inc
        include npnddd.inc
        include nspe2dd.inc
        include ncntdd.inc
        include nusedd.inc
        include winapi.inc
          INCLUDE   NSEL2DD.INC

release  init      "2.11"        DLH    New fax gateway
Reldate   INit      "2015 April 14"
.release  init      "2.10"        DLH    Sunbelt PDF
.Reldate   INit      "2013 April 24"
.release  init      "2.02"        DLH    Turn off PL
.Reldate   INit      "16 February 10"
.release  init      "2.01"        DLH    More code before emailing pdf
.Reldate   INit      "05 August 09"
.release  init      "2.00"        DLH    add email / change fax to use same pdf as email
.Reldate   INit      "04 February 09"
.See archives for previous revisions
.release  init      "1.91"        DLH    change Laser3 printer name for XP
.Reldate   INit      "02 February 09"
.release  init      "1.89"        DLH    Sept2007  PLI Inclusion & new fax server
.release  init      "1.88"        ASH   18JUN2007 PLI Inclusion
.release  init      "1.87"        ASH   17JAN2006 Corrected Sort Error
.release  init      "1.86"        ASH   09AUG2005 Small Changes for List Management
.release  init      "1.85"        ASH   18MAY2005 LISTMLR Conversion
.release  init      "1.84"        ASH   03MAY2005 Changed Printer flags in Program 1
.release  init      "1.83"        ASH   09DEC2004 FAXFILE
.release  init      "1.82"        ASH   02DEC2004 Conversion from Acrobat Distiller to PDF995
.release  init      "1.81"        ASH   06AUG2004 Logo Conversion
.release  init      "1.80"        DMB   26MAY2004 Mailer Conversion
.release   init      "1.70"        28JAN04  ASH  DATACARD CONVERSION
.release   init      "1.69.1"        02FEB2004    DMB change phone number from main fax line to lm fax
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

.timestamp1 dim  16
.timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.Counters
COUNTR  FORM    9
.Files to open
prfile  pfile
input2  file
input3  file
input2Name dim    40
First   init    "Y"
HOLDBRK dim     7
HOLDMAIL dim    4
HOLDEXCL dim        1
newdate1 dim    10
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    11
line1   dim    100
Carr    init    0x7f
faxnum  dim     10
LONGDIST dim    1
PrtFlag dim     1            .default is yes print, No = fax
PrintFlag form  "01"    .Default is Laser3!!!    '2'= pdf, '3' = laser2  '5' = fax
userinfo dim    500
userlogn dim    7
userlogw dim    7
mss1    plform  Error
.begin patch 2.0
dmFileName          dim       80
BrokerAddr          Dim       50        .email 
.end patch 2.0
.begin patch 2.01
PDFname    DIm       55        .print file name
FileCheck FIle
trapcount form      4
.end patch 2.01
timestamp2 dim  16
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
......
font6   font
font7     font
        formload mss1
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=9
        create  font5,"Arial",size=10
        create  font6,"Arial",size=14
          create    font7,"Times New Roman",size=9

NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
        match   "NLCR0041",PROGRAM   .case sensitive
        if not equal
                move    "NLCR0041",PROGRAM
        else
                scan    ":",inpname
                if not equal
                        reset   inpname
                        pack    input2Name,NTWKPATH3,inpname
                else
                        reset   inpname
                        unpack  inpname,input2Name
                endif
                move    prtname,PrintFlag
                move    user,userlogn
                call    Trim using userlogn
        endif
.testing
.          Goto      PrintFIle
        move    "NLCR0041",WPrognme
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY

.Find out system information
                    call                GetWinVer
.Set Flags to Open NINORD.DAT
        move    C0,NORDFLAG
        move    C0,NORDFLG2

.testing
.          Goto      PrintFIle
.end testing
          if (PrintFlag <> 2)
                call    Paint
                DISPLAY *P1:24,"OPENING FILES";
        endif

.Open Files
        open    input2,input2Name
        prepare input3,"C:\WORK\BRKFILE2.DAT"
        if (PrintFlag <> 2)
                DISPLAY *P1:24,"READING BRKFILE.DAT FILE";
        endif
.Prep file before reading it sequentially!!
.FILE MUST EITHER BE REFORMATTED AND REINDEXED PRIOR TO THIS JOB OR DONE RIGHT HERE!!!!!
LCRLoop
        loop
                move    "LCRLoop-readseq,input3",Location
                READ    input2,SEQ;ORDVARS
                until over
                call    OrderReadOtherFiles
                filepi  1;input3
                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,BREMAIL,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME,HOLDEXCL
        repeat
        if (PrintFlag <> 2)
                DISPLAY *P1:24,"SORTING BRKFILE2 FILE ";
        endif
SortFile
        clear   taskname
        move    "C:\WORK\BRKFILE2.DAT,C:\WORK\BRKFILE2.SRT;1-7,8-11,12-17",taskname
        reset   taskname
          move      C0,N1
          for N2,1,10
                    sort    taskname
                    if not over
                              move      C0,N1
                              break
                    else
                              move      C1,N1
                    endif
.                    pause     "5"
                    call      waitin using "10"
.                    pause     "10"
        repeat
          if (N1 = C1)
                    move    s$error$,error
                    move    "Sort did not work!",Location
                    clear     KeyLocation
                    call    IOMssg
                    shutdown
          endif
        clear   taskname
        if (PrintFlag <> 2)
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
        endif
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
.begin patch 2.0
.        open    input3,"C:\WORK\BRKFILE2.SRT"
        open    input3,"C:\WORK\BRKFILE2.SRT",exclusive
.end patch 2.0
        loop
                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,BREMAIL,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME,HOLDEXCL
                goto    LastRec if over
                if (NBRKFLD <> HOLDBRK | OMLRNUM <> HOLDMAIL)
                        if (FIRST = NO)
                                PRTCLOSE prfile
                                if (PrintFlag = 2)                   .pdf
..................
.It takes some time for the file to be created, so we must check
.Allow 20 seconds to originally load DISTILLER
.begin patch 2.10
.                                        clock   timestamp,timestamp1
.                                        move    timestamp1,time1
.                                        loop
.                                                clock   timestamp,timestamp2
.                                                move    timestamp2,time2
.                                                sub     time1,time2,time3
.                                                if (time3 > 2000) .20 Seconds Maximum
.                                                        break
.                                                endif
.                                        repeat
                                        call    SendPDFFile
                                elseif (PrintFlag = 5 AND PrtFlag = NO)            .faxing
.begin patch 2.0
.                                        clock   timestamp,timestamp1
.                                        move    timestamp1,time1
.                                        loop
.                                                clock   timestamp,timestamp2
.                                                move    timestamp2,time2
.                                                sub     time1,time2,time3
.                                                if (time3 > 2000) .20 Seconds Maximum
.                                                        break
.                                                endif
.                                        repeat
.end patch 2.10
                                        call    SendPDFFile
.                                        clear   taskname
.                                                  Path      Exist,"c:\windows"
.                                                  if over                       .nt/2000
.                                                            append    "!c:\winnt\system32\cmd.exe",taskname
.                                                  elseif (osflag = c6)          .XP
.                                                            append    "!c:\windows\system32\cmd.exe",taskname
.                                                  else                          .95/98
.                                                            append    "!c:\command.com",taskname
.                                                  endif
.                                        append  " /c copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\",taskname   .":)
.                                        append  HOLDBRK,taskname
.                                        append  HOLDMAIL,taskname
.                                        append  ".prn /b",taskname
.                                        reset   taskname
.                                        execute taskname
.                                        clear   taskname
.                                                  Path      Exist,"c:\windows"
.                                                  if over                       .nt/2000
.                                                            append    "!c:\winnt\system32\cmd.exe",taskname
.                                                  elseif (osflag = c6)          .XP
.                                                            append    "!c:\windows\system32\cmd.exe",taskname
.                                                  else                          .95/98
.                                                            append    "!c:\command.com",taskname
.                                                  endif
.                                        append  " /c copy c:\work\",taskname            .":)
.                                        append  HOLDBRK,taskname
.                                        append  HOLDMAIL,taskname
.                                        append  ".prn \\nts3\fax",taskname
.                                        reset   taskname
.                                        execute taskname
                                endif
.end patch 2.0
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
        if (PrintFlag = 2)                                   .pdf
.......................
.It takes some time for the file to be created, so we must check
.begin patch 2.10
.                move    C0,N9
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,"C:\WORK\PDF\",PDFname,hexzero      ."
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                        move    timestamp2,time2
.                        sub     time1,time2,time3
.                        if (time3 > 4000) .40 Seconds Maximum
.                                break
.                        endif
.                repeat
.end patch 2.10
                call    SendPDFFile
        elseif (PrintFlag = 5 AND PrtFlag = NO)             .fax
.begin patch 2.0
.begin patch 2.10
.                                        clock   timestamp,timestamp1
.                                        move    timestamp1,time1
.                                        loop
.                                                clock   timestamp,timestamp2
.                                                move    timestamp2,time2
.                                                sub     time1,time2,time3
.                                                if (time3 > 4000) .40 Seconds Maximum
.                                                        break
.                                                endif
.                                        repeat
.end patch 2.10
                                        call    SendPDFFile

.                clear   taskname
.                    Path      Exist,"c:\windows"
.                    if over                       .nt/2000
.                              append    "!c:\winnt\system32\cmd.exe",taskname
.                    elseif (osflag = c6)          .XP
.                              append    "!c:\windows\system32\cmd.exe",taskname
.                    else                          .95/98
.                              append    "!c:\command.com",taskname
.                    endif
.                append  " /c copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\",taskname           ."
.                append  HOLDBRK,taskname
.                append  HOLDMAIL,taskname
.                append  ".prn /b",taskname
.                reset   taskname
.                execute taskname
.                clear   taskname
.                    Path      Exist,"c:\windows"
.                    if over                       .nt/2000
.                              append    "!c:\winnt\system32\cmd.exe",taskname
.                    elseif (osflag = c6)          .XP
.                              append    "!c:\windows\system32\cmd.exe",taskname
.                    else                          .95/98
.                              append    "!c:\command.com",taskname
.                    endif
.                append  " /c copy c:\work\",taskname                       ."
.                append  HOLDBRK,taskname
.                append  HOLDMAIL,taskname
.                append  ".prn \\nts3\fax",taskname
.                reset   taskname
.                execute taskname
.end patch 2.0
        endif
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
        if (PrintFlag = 2)                      .pdf
                pack    APIFileName,"C:\WORK\PDF\PDFFILE.PRN",hexzero
        elseif (PrintFlag = 5)                             .fax
.begin patch 2.0
                pack    APIFileName,"C:\WORK\PDF\PDFFILE.PRN",hexzero
.                pack    APIFileName,"C:\work\faxfile.prn",hexzero
.end patch 2.0
        else
                pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
        endif
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)

        endif
.begin patch 2.0
.*** note  rewritten for Vista or Windows 7
.first kill from folder  c:\work\pdf
          clear     Mailbody
          FIndDIr   "C:\work\pdf\*.pdf",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "C:\work\pdf\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
.log files          
          clear     Mailbody
          FIndDIr   "C:\work\pdf\*.log",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "C:\work\pdf\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
.prn files          
          clear     Mailbody
          FIndDIr   "C:\work\pdf\*.prn",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "C:\work\pdf\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
.        clear   taskname
.        clear   str55
.        clear   str50
.          Path      Exist,"c:\windows"
.          if over                       .nt/2000
.                    append    "!c:\winnt\system32\cmd.exe",taskname
.                    append    "!c:\winnt\system32\cmd.exe",str55
.                    append    "!c:\winnt\system32\cmd.exe",str50
.          elseif (osflag = c6)          .XP
.                    append    "!c:\windows\system32\cmd.exe",taskname
.                    append    "!c:\windows\system32\cmd.exe",str55
.                    append    "!c:\windows\system32\cmd.exe",str50
.          else                          .95/98
.                    append    "!c:\command.com",taskname
.                    append    "!c:\command.com",str55
.                    append    "!c:\command.com",str50
.          endif
.        append  " /c del c:\work\PDF\*.pdf",taskname
.        append  " /c del c:\work\PDF\*.log",str55
.        append  " /c del c:\work\*.prn",str50
.        reset   str50
.        execute str50
.        reset   taskname
.        reset   str55
.        execute taskname
.        execute str55
.end patch 2.0
        shutdown

SendPDFFile
........................
.          Pause     "10"
.begin patch 2.10
.          call      Waitin using "10"
.end patch 2.10
        move    "Broker Report",MailSubjct
.   Set the text message that is send with the attachments
        move    PDFname,MailBOdy
.begin patch 2.0
.          Pack      MailTo from UserLogn,"@nincal.com"
          Pack      MailTo from UserLogn,"@nincal.com,",BrokerAddr
.end patch 2.0
          Pack      MailFrom from UserLogn,"@nincal.com"
.begin patch 2.10
.          Pack      MailAttach from "c:\work\pdf\",PDFname              ."
          Pack      MailAttach from PDFname              ."
.end patch 2.10
.begin patch 2.01
CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.end patch 2.01
          call      SendMail
        return
OrderOpenFile

.Print newly sorted file
        if (PrintFlag = 2)                                          .pdf
                clear   str25
                append  NBRKFLD,str25
                append  "_",str25
                append  userlogn,str25
                reset   str25
.begin patch 2.10
.                PRTOPEN prfile,"PDF995",str25
                pack    PDFNAME,"c:\work\pdf\",str25,".pdf"
                PRTOPEN prfile,"PDF:",PDFname
.                pack    PDFNAME,str25,".pdf"
.end patch 2.10
        elseif (PrintFlag = 3)                                   .laser2
                if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)
                        PRTOPEN prfile,"\\NINs2\Laser2","FAXFILE.PRN"
                elseif (osflag = c3 | osflag = c4 )         .win 95 98
                        PRTOPEN prfile,"Laser2","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"-","FAXFILE.PRN"
                endif
        elseif (PrintFlag = 5)                                    .fax
                move    YES,PrtFlag             .PRINT IT
.begin patch 2.0 -- lets check for email first
                    Clear      Brokeraddr
                    call      Trim using BrEmail
                    if        (bremail <> "")               .we have somthing
                              scan      "@",Bremail
                              if        equal                         .presuming valid
                              reset     bremail
                              move      bremail,BRoKeraddr
                              move      No,PRtflag                    .send it
                              endif
                    Else
                    
                    move    BRFAX,faxnum
                    match   "0000000000",faxnum
                              if      equal                                       .no good zeroed out
                    
                              else
                                        type    faxnum
                                        if not equal
                                        else
                                        count   N2,faxnum
                                        compare C10,N2
                                                  if equal
                                                  move    NO,PrtFlag      .Looks like we have number, FAX IT
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
.                                                  pack      BrokerAddr from"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                                                  pack      BrokerAddr from "+",longdist,faxnum,"@fax.nincal.com"
                                          endif
                              endif
                    endif
.....DH TEST
                clear   str25
                append  NBRKFLD,str25
                append  "_",str25
                append  userlogn,str25
                reset   str25
.begin patch 2.10
.                PRTOPEN prfile,"PDF995",str25
                pack    PDFNAME,"c:\work\pdf\",str25,".pdf"
                PRTOPEN prfile,"PDF:",PDFname
.                pack    PDFNAME,str25,".pdf"

.....DH TEST
                endif
                if (PrtFlag = YES)
.begin patch 1.91
                        if (osflag >= c6)
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"

.                        if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
                        Elseif (osflag = c1 | osflag = c5)         .nt
.end patch 1.91
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock","FAXFILE.PRN"
.                        elseif (osflag = c1)         .win 95 98
                        elseif (osflag = c3 | osflag = c4)         .win 95 98
.end patch 1.67
                                PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN prfile,"-","FAXFILE.PRN"
                        endif
                else
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
                        pack    APIFileName,"c:\work\HDRFILE.prn",hexzero
                        call    DeleteFile
                        if (APIResult = 0 | APIResult = hexeight)

                        endif
.begin patch 2.0
.                        SPLOPEN "c:\work\HDRFILE.prn"
.                        print   "^[D",longdist,faxnum,"^[N",brcomp:
.                                "^[SList Management"," ^]"
.                        SPLCLOSE
.end patch 2.0
.....DH TEST
.                        PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
.....DH TEST
                endif
        else
.begin patch 1.91
                        if (osflag >= c6)
                                PRTOPEN prfile,"\\NINS2\laser3 Blankstock","FAXFILE.PRN"

                        Elseif (osflag = c1 | osflag = c5)         .nt
.end patch 1.91
                        PRTOPEN prfile,"\\NINs2\Laser3","FAXFILE.PRN"
                elseif (osflag = c3 | osflag = c4)         .win 95 98
                        PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                else   .(osflag = c0)         .Don't know prompt for printer
                        PRTOPEN prfile,"-","FAXFILE.PRN"
                endif
        endif
        ADD     C1,COUNTR
        if (PrintFlag <> 2)
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
        endif
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
                    clear     HOLDEXCL
          else
                    move      COMPEXCL,HOLDEXCL
          endif
.END PATCH 1.88 REPLACED LOGIC
.Broker File
        pack    NBRKFLD,OBRKNUM,OBRKCNT
        rep     zfill,NBRKFLD
        move    "Driver-NBRKKEY,1rst",Location
        call    NBRKKEY
        if over
                pack   BRCOMP,"UNKNOWN",B55
.begin patch 2.0
          Pack      BREMAIL,b55                
.end patch 2.0
        endif
.Special Instructions
        move    OLRN,NSPE2FLD
        rep     zfill,NSPE2FLD
        move    C3,NSPE2LOCK
        move    "Driver-NSPE2KEY,1rst",Location
        call    NSPE2KEY
.NIN Contact
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
          move      OMLRNUM,COMPFLD3
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          move      COMPNUM,NXRFFLD2
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
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
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
.begin patch 2.02
.                  if (HOLDEXCL = "P")
.                    prtpage   prfile;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
.                              *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
.                              *p=1630:550,"Oakland, CA 94612-1429":
.                              *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
.                              *p=1630:800,"A Division of Names in the News":
.                              *font=font2,*Alignment=*Left
.          else
.                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 2.02
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,newdate1;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
        prtpage prfile;*p1000:row,BRCNTCT;
        prtpage prfile;*p5200:row,"FROM:  ";
                move    "List Management",CNTNAME
.begin patch 2.02
.                          if (HOLDEXCL = "P")
.                    move    "(415) 945-9450",CNTPHONE
.          else
.                    move    "(415) 989-3350",CNTPHONE
.          endif
                    move    "(415) 989-3350",CNTPHONE
.end patch 2.02
        prtpage prfile;*p5700:row,CNTNAME;
        add     eightlpi,row
        prtpage prfile;*p1000:row,BRCOMP;
.begin patch 2.02
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*p5700:row,"Pacific Lists";
.          else
.                    prtpage prfile;*p5700:row,"Names In The News";
.          endif
                    prtpage prfile;*p5700:row,"Names In The News";
.end patch 2.02
        add     eightlpi,row
        call    Trim using BRTELE
        if (BRTELE <> "")
                unpack  BRTELE,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
        prtpage prfile;*p5700:row,CNTPHONE;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"FAX:";
        call    Trim using BRFAX
        if (BRFAX <> "")
                unpack  BRFAX,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
        prtpage prfile;*p5200:row,"FAX:  ";
.begin patch 2.02
                    prtpage prfile;*p5700:row,"(415) 945-9451";
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*p5700:row,"(415) 945-9451";
.          else
.          prtpage prfile;*p5700:row,"(510) 628-8313";
.        endif
.end patch 2.02
        add     eightlpi,row
        move    CNTNAME,str1
cntloopy
        bump    CNTNAME,1
        cmatch  B1,CNTNAME
        goto    cntloopy if not equal
        goto    cntexit if eos
        bump    CNTNAME,1
        move    CNTNAME,str7
        call    RemoveChar using str7,B1
        move    str7,str6
        clear   str24
.begin patch 2.02
.          if (HOLDEXCL = "P")
.          pack    str24,str1,str6,"@pacificlists.COM"
.          else
.          pack    str24,str1,str6,"@NINCAL.COM"
.         endif
          pack    str24,str1,str6,"@NINCAL.COM"
.end patch 2.02
cntexit reset   CNTNAME
.begin patch 2.02
.          if (HOLDEXCL = "P")
.          prtpage prfile;*p5700:row,"www.pacificlists.com";
.        else
.          prtpage prfile;*p5700:row,"www.ninlists.com";
.        endif
          prtpage prfile;*p5700:row,"www.ninlists.com";
.end patch 2.02
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p2800:row,*font=font1,*boldon,"LIST CLEARANCE REPORT",*boldoff;
          add     eightlpi,row
.begin patch 2.02
.          if (HOLDEXCL = "P")
.                   add     eightlpi,row
.                    add     eightlpi,row
.          else
                    prtpage prfile;*p5600:row,*font=font2,*boldon,"If approved on Rental please refer";
                    add     eightlpi,row
                    prtpage prfile;*p5600:row,"to our Datacard on www.ninlists.com";
                    add     eightlpi,row
                    prtpage prfile;*p5600:row,"for pricing.",*boldoff;
.          endif
.end patch 2.02
          add     eightlpi,row
        return

OrderMailHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Mailer:  ",*boldoff,MCOMP;
        add     eightlpi,row
        add     eightlpi,row
.begin patch 2.02
.          if (HOLDEXCL = "P")
.                   prtpage prfile;*pcolumn:row,*font=font5,*boldon,"PLI ##/";
.         else
          prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN ##/";
.        endif
.end patch 2.02
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"List/";
        prtpage prfile;*pcolumn2:row,"Mail Date";
        prtpage prfile;*pcolumn4:row,"Quantity/";
        prtpage prfile;*pcolumn6:row,"Answer";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Broker ##";
        prtpage prfile;*pcolumn4:row,"Select";
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
.Should never need to list a Cancelled Pending Order, but in case you do logic will now allow it
        if (OSTAT = "p" | OSTAT = "x")
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
                move    C0,N9
                move    OQTY,N9
                edit    N9,EditQuan
        prtpage prfile;*pcolumn4:row,EditQuan;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OMLRPON;
        prtpage prfile;*pcolumn4:row,NSEL2NAME;
        call    PrintSpecialInstructions
        add     eightlpi,row
        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
.
        call    TRIM using DESC004
        if (DESC004 <> "")
                pack    str2,carr,B1
                rep     str2,DESC004
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC004,C1
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
.begin patch 2.01
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    call      waitin using "5"
.                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Broker Report - ",str25
.                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    PDFName,MailBody
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
.end patch 2.01

.Include IO file
        include nordio.inc
        include nord5io.inc
        include nord4io.inc
        include nordPio.inc
          include   compio.inc
          include   cntio.inc
        include nxrfio.inc
        include ndatio.inc
        include npndio.inc
        include nspe2io.inc
        include ncntio.inc
          INCLUDE   NSEL2IO.INC
        include comlogic.inc

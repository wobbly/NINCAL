........................................
. Program:      NMLR002A.PLS
. Function:     Mailer PDF Sample Print Program
. Author:       Andrew Harkins
. Orig. Date:   DECEMBER 26, 2001
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include nsmpdd.inc
        include winapi.inc
.begin patch 1.1
               include        nusedd.inc
.end patch 1.1

release  init      "1.6"        DLH      change waiting for email logic
Reldate   Init      "2013 April 22"
.release  init      "1.5"        DLH      change waiting for email logic
.Reldate   Init      "17 December 2008"
.release  init      "1.4"        DLH    03Oct2007 Conversion from SMTPMLR to SendMail
.release  init      "1.3"        ASH    02DEC2004 Conversion from Acrobat Distiller to PDF995
.release  init      "1.2"     11SEP2003 ASH       SAMPLE FORMAT CONVERSION
.release  init      "1.1"     07Feb2002 DLH       use portn and get user full name
.release  init      "1.0"     26DEC2001 ASH       DEVELOPMENT RELEASE

.begin patch 1.5
FileCheck FIle
trapcount form      4
.end patch 1.5
.Files to open
prfile  pfile
DCXFile dim     120
dcxpath DIM     45
.START PATCH 1.2 REPLACED LOGIC
.dcxext  init    ".dcx"
dcxext  init    ".TIF"
.END PATCH 1.2 REPLACED LOGIC
userlogn dim    7
timestamp1 dim  16
timestamp2 dim  16
time1   form    16
time2   form    16
time3   form    16
pict3   pict
mss1    plform  Error

        formload mss1

        match   "NMLR002A",PROGRAM   .case sensitive
        if not equal
                    shutdown
        endif
.begin patch 1.1
          move      user,userlogn
.               MOVE           PORTn TO NUSEFLD
.               REP            zfill IN NUSEFLD
.               CALL           NUSEKEY
.               move           NuseUser to str35
.         call      RemoveChar using str35,B1
.end patch 1.1

          pack      DCXPATH,NTWKPATH1,"samples\s",INPNAME
          call      Trim using userlogn
          Display   *p10:10,"printing Sample : ",dcxpath
          Display   *p10:12,"For : ",UserLogn
          

.START PATCH 1.3 REPLACED LOGIC
.         PRTOPEN prfile,"Acrobat Distiller",INPNAME
.begin patch 4.6
.          PRTOPEN prfile,"PDF995",INPNAME
          pack      str55 from "c:\work\pdf\",inpname,".pdf"
          PRTOPEN prfile,"PDF:",str55
.end patch 4.6
.END PATCH 1.3 REPLACED LOGIC
          pack    str55,INPNAME,".pdf"
.          pack      DCXFile,DCXPath,DCXExt
          pack      DCXFile,"e:\data\samples\s010265001.tif|10.10.30.103:502"
.Print and Display First Page
        prtpage prfile;*UNITS=*HIENGLISH;
          clear     N9
          
          
.START PATCH 1.2 REPLACED LOGIC
.         CREATE    PICT3=70:700:100:550:                              
.                   DCXFile,BORDER,SCROLLBAR,AUTOZOOM            
.         PICT3.GetPageCount GIVING N9
.         prtpage prfile;*P2:2,*PICTvis=6:10000:3:10000:PICT3;
          CREATE    PICT3=70:700:100:550:                              
                    DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0
          PICT3.GetPageCount GIVING N9
          prtpage prfile;*P2:2,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
.END PATCH 1.2 REPLACED LOGIC
.Print and Display Additional Pages
          if (N9 > C1)    .Only Enter loop if more than one page
                    clear   N8
                    move    C1,N8   .Start with SECOND PAGE as first page already printed
                    loop
                              add     C1,N8
                              until (N8 > N9)
.START PATCH 1.2 REPLACED LOGIC
.                             CREATE  PICT3=70:700:100:550:
.                                       DCXFile,BORDER,SCROLLBAR,AUTOZOOM,PAGE=N8
.                             prtpage prfile;*P2:2,*NEWPAGE,*PICTvis=6:10000:3:10000:PICT3;
                              CREATE  PICT3=70:700:100:550:
                                        DCXFile,BORDER=0,SCROLLBAR,AUTOZOOM=0,PAGE=N8
                              prtpage prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=6:10000:3:10000:PICT3;
.END PATCH 1.2 REPLACED LOGIC
                    repeat
          endif
          destroy   PICT3
          prtclose prfile
          clock     timestamp,timestamp1
          move      timestamp1,time1

.begin patch 1.5

.begin patch 4.6
.          pack      Srchstr from "c:\work\pdf\",str55                            ."
          pack      Srchstr from "c:\work\pdf\",inpname,".pdf"                            ."
          pack      str55 from srchstr
.end patch 4.6
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,SrchSTR,Exclusive   
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from SrchStr


.Patch 1.4 CommentOut
.         loop  
.                   clock     timestamp,timestamp2
.                   move      timestamp2,time2
.                   sub       time1,time2,time3
.                   if (time3 > 5000) .20 Seconds Maximum
.                             break
.                   endif
.Patch 1.4 CommentOut
.Patch 1.4 Code Added
.      Trap      notready if IO
.         pack         Srchstr with "c:\work\pdf\",str55                  .."
.      Open     tempfile,srchstr,exclusive
.      Close    Tempfile
.   break                                
.      goto       Ready                               
.Notready
.      trapclr    IO
.      Noreturn
.
.         repeat
.Ready
..;Patch 1.4 CommentOut
.         loop  
.                   clock     timestamp,timestamp2
.                   move      timestamp2,time2
.                   sub       time1,time2,time3
.                   if (time3 > 3000) .30 Seconds Maximum
.                             break
.                   endif
.         repeat
.end patch 1.5
.Patch 1.4 CommentOut
.Patch 1.4 Code Added
.
.        move    "Here is your PDF File",SmtpSubject Subject
        move    "Here is your PDF File",MailSubjct
.   Set the text message that is send with the attachments
          Clear     Mailbody
          append    Str55,Mailbody
          append    Crlf,Mailbody
.        move    str55,SmtpTextMessage(1)   Array <Text message >
.        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.        move    "NTS4",SmtpEmailServer                   Address of email serverc
.        clear   smtpemailaddress
.begin patch 1.1
          Clear     MailTO
          Clear     MailFrom
          Append    UserLOgn,Mailto
.        append  userlogn,SmtpEmailAddress
.        append  userlogn,SmtpEmailAddress
.        append   str35,SmtpEmailAddress
.end patch 1.1
.        append  "@nincal.com",SmtpEmailAddress
.        reset   smtpemailaddress
        append  "@nincal.com",MailTO
        reset   MailTO
        MOVe        Mailto,MailFrom
.begin patch 1.1
.        move    userlogn,SmtpUserName                                User name
..        move    NuseUser,SmtpUserName                                User name
.end patch 1.1
.   Set the destinations of the email. Max 100 (Mime spec)
.        move    smtpemailaddress,SmtpDestinations(1,1)
.        move    userlogn,SmtpDestinations(1,2)
.        move    "1",SmtpDestIndexLast                          originators UserName
          Pack      MailAttach from "c:\work\pdf\",Str55                   .."
.        move    str55,SmtpAttachments(1,1)                     Attached file name
.START PATCH 1.3 REPLACED LOGIC
..        move    "C:\WORK",SmtpAttachments(1,2)           Path to attached file name
.        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.        move    "C:\WORK\PDF",SmtpAttachments(1,2)           Path to attached file name
.        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        move    "C:\work\PDF\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.END PATCH 1.3 REPLACED LOGIC
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
          Call      SendMail
.begin patch 1.5

.begin patch 4.6
.          pack      taskname from "C:\work\pdf\",str55           ." :)
.          erase     Taskname                      
          erase     str55
.end patch 4.6
.end patch 1.5

          shutdown
.begin patch 1.5
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     "10"
                    noreturn
                    if        (trapcount > 60)   . are you kidding me
                    Pack       MailSubjct,"Sample - ",srchstr
                    Append    UserLOgn,Mailto
                    append  "@nincal.com",MailTO
                    reset   MailTO
                    MOVe      Mailto,MailFrom
                    append    CRLF,MailBOdy
                    append    srchstr,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile

.end patch 1.5
.Include IO file
        include nsmpio.inc
.begin patch 1.1
               include        nuseio.inc
.end patch 1.1

          include   comlogic.inc


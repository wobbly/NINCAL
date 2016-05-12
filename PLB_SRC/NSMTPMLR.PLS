PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC

Release   INit      "2.1"     DLH       check for attachment being ready
Reldate   INit      20 August 2008"
.Release  INit      "2.0"     DLH       SendMail
.Reldate  INit      12 MAy 2008"
.release  init      "1.7"            DMB 02SEP2003          Added Text to Vars to check  ."
.release  init      "1.6"            DMB 29Aug2003  Increased length of subjectline
.release  init      "1.5"           DLH 17Jan20001 If user = "" user Information Services as sender
.release  init      "1.4"           DLH 07Jan20001 Allow Destination addresses
.release  init      "1.3"           DLH 06Jan20001 Allow multiple file attachments
.release  init      "1.2"           ASH 03OCT20000 NEW SERVER ADDED
.release  init      "1.1"           DLH 12Sep00  add NSTMP.dat file & info
.release  init      "1.0"           DLH 28Sep99
info     file
path    DIM        45                   use Path name ie \\SRV2008A\E\data\text
Subject  dim      150
record   dim      375
Files    dim      350
Recipients dim     350
BODY      DIM       250
.begin patch 2.1
FileCheck FIle
trapcount form      4
Enclosure Dim       1
.end patch 2.1


BEGIN    CMATCH    B1 TO PROGRAM             .CHAINED FROM DSINIT?
         IF        EOS                       .NO
         stop
         else
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "LOCAL" TO PRTNAME
         ENDIF

         MOVE      "SMTP Mailer" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
         trap      noclue if io
         open      info,"C:\WORK\NSMTP.DAT"

getinfo  read      info,seq;record
         goto      infodone if over
         scan      "$SUBJECT=" in record
         if        equal
         bump      record by 9
         move      record to subject
         goto      getinfo

         endif
         reset     record
         scan      "$PATH=" in record

         if        equal
         bump      record by 6
         move      record to Path
         goto      getinfo
         endif

         reset     record
         scan      "$FILES=" in record

         if        equal
         bump      record by 7
.begin patch 2.0
         move      record to Files
.         Pack      mailattach from path,"\",files                    ."
.end patch 2.0
         goto      getinfo
         endif

         reset     record
         scan      "$SENDTO=" in record

         if        equal
         bump      record by 8
         move      record to Recipients
         goto      getinfo
         endif

         reset     record

         scan      "$BODY=" in record

         if        equal
         bump      record by 6
.begin patch 2.0
         move      record to BODY
.end patch 2.0
         goto      getinfo
         endif

         reset     record
         goto      getinfo
.these currently are the only three variables checked for just read until done
infodone close     info
         if        (files = "" & inpname <> "")
         move      inpname to files
         endif
.Note comment field is only 30 bytes multiple send to should use $SendTO
         if        (Recipients = "" & Comment > "")
         move      Comment to Recipients
         endif

.................................................................
Subject  CMATCH    B1 TO SUBJECT
         IF        EOS
.begin patch 2.0
.         MOVE      "Here is your PDF file" to subject
         MOVE      "Here is your PDF file" to MailSubjct
         Else
         Move       Subject,MailSubjct
.end patch 2.0
         endif

         CMATCH    B1 TO path
         IF        EOS
         MOVE      NTWKPATH1 to path
         endif

.begin patch 2.0
.        Move       Subject to SmtpSubject Subject

.   Set the text message that is send with the attachments


.       Move    inpname,SmtpTextMessage(1)   Array <Text message >
          call      Trim using files
          move      b1,Mailbody
          append    Body,MailBody
          append    CRLF,MailBody
          Count     n3,files
          if        (N3 >= 4)
          Pack      MailAttach from Path,"\",files              ."
.begin patch 2.1
          Move      Yes,Enclosure
.end patch 2.1
          else
          Move      B1,MailAttach
.begin patch 2.1
          Move      No,Enclosure
.end patch 2.1
          endif
          append    MailAttach,Mailbody
          append    CRLF,MailBody
          if      (user > "" & user <> " ")
                    scan      "@",user
                    if equal
                    Move      User to MailFrom
                    else
                    pack      Mailfrom from user,"@nincal.com"
                    endif
                  else
          Move      "Creques@nincal.com",MailFrom
          endif

          call      debug

          pack      Mailto from Recipients
          Scan      "@",Mailto
          if      equal                
                    reset     MailTO              .all is well
                              Cmatch    "@",Mailto                        .double check
                              if        equal
                              Pack      Mailto from "INformationServices@nincal.com"
                              append    CRLF,Mailbody
                              append    "no valid email for destination",Mailbody
                              append    CRLF,Mailbody
                              append    "User = ",Mailbody
                              append    user,Mailbody
                              append    CRLF,Mailbody
                              append    "Comment = ",Mailbody
                              append    Comment,Mailbody
                              append    CRLF,Mailbody
                              append    "REcipients = ",Mailbody
                              append    Recipients,Mailbody
                              append    CRLF,Mailbody
                              else
                              endif
.                         reset   comment
.                         pack    SMTPEMAILADDRESS from comment
.                         pack    SMTPUserName from comment
.                             reset   recipients
.                             pack    SMTPEMAILADDRESS from recipients
.                             pack    SMTPUserName from recipients
                    else                                .then let us pray that in is NINCAL.com
                              Pack      Mailto from "INformationServices@nincal.com"
                              append    CRLF,Mailbody
                              append    "no valid email for destination",Mailbody
                              append    CRLF,Mailbody
                              append    "User = ",Mailbody
                              append    user,Mailbody
                              append    CRLF,Mailbody
                              append    "Comment = ",Mailbody
                              append    Comment,Mailbody
                              append    CRLF,Mailbody
.                             Move    user,SmtpUserName                                User name
.                             append  user to SmtpEmailAddress
.                             append  "@nincal.com",SmtpEmailAddress
.                             reset    smtpemailaddress
                    endif
          reset     MailBody
.intend to change this to allow multiple files, DLH
.        if        (inpname = "" or inpname = " ")
        if        (files = "" or files = " ")
.        cmatch    b1 to inpname
.Begin patch 2.0
.       Move    "0",SmtpAttIndexLast                                Index to NO entry
.end patch 2.0
.;patch1.7
.         move      BODY to SmtpTextMessage(1)
.         Move      "1",SmtpTextIndexLast         Index to how many lines<ARRAY> in body
.;patch1.7
        else
.dave goes mad
.        MOVELPTR Files,n6
.        move    c1 to n4
.        move    c1 to n3                    .number of files
.        scan    ";" in Files
.        if      not equal
.        reset   Files
.        Move    Files,SmtpAttachments(1,1)                        Attached file name
.        Move    path,SmtpAttachments(1,2)           Path to attached file name
.        Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
.        else
.morefiles
.        MOVEFPTR Files,n5
.        bump     Files,-1
.        lenset   Files
.        reset    Files,n4
.        Move    Files,SmtpAttachments(n3,1)                        Attached file name
.        Move    path,SmtpAttachments(n3,2)           Path to attached file name
.        Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
.        ADD     c1 to n5
.        move    n5 to n4
..?        reset   Files,25
.        reset   Files,n6
.        reset   Files,n5
.checkit scan    ";" in Files
.                if      equal
.                add     c1 to n3
.                goto    morefiles           .else this is the last one
.                else
.                add     c1 to n3
.                reset   Files,n6
.                reset   Files,n5
.                Move    Files,SmtpAttachments(n3,1)                        Attached file name
.                Move    path,SmtpAttachments(n3,2)           Path to attached file name
.                Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
.                endif
        endif
..end patch 1.3

.        endif
drew
..   For debugging
..   "Dot" out the line 'Clear ..." if you want to enable the LogFile
.
..        Move    "0",SmtpDispMode                                    Display log file
.        Move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
.        Clear   SmtpLogFile                                         'Clear' disables the LogFile
..   Progress bar. It is currently enabled so you can have an indication of what
..   goes on.If this lines of code is removed would the default be 0
.
.        Move    "1",SmtpProgress                                    Enable progress bars
.
..   Send the above eMail to the destination which happens to be myself
..   If any errors detected the error code and human readable text is given
.begin patch 2.1
          If        (Enclosure = Yes)
          Move      c0,Trapcount

CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck
          endif
.end patch 2.1

          Call      SendMail
.        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.
.        If      NOT Equal
.         Pack   Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                     "Status Code ",SmtpStatus," - ",SmtpStatusText
.        Move    "PDF File not found",SmtpSubject Subject
.        Move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
..         Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
.        Else
..         Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
.        Endif
.end patch 2,0
        shutdown    "cls"
        stop
.begin patch 2.1
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"NSMTPMLR - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 2.1
        
.begin release 1.1
.......................................................................
.noclue   c:\WORK\NSMTP.dat file was not found - use default values
Noclue
        trapclr  io
        noreturn
        goto     subject
........................................................................
.end release 1.1
        include  comlogic.inc


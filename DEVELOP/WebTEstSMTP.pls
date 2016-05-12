PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
           include ncntdd.inc
.test 2015 Dec 28  test with office 365
Release   INit      "2.0"     DLH       SendMail
Reldate   INit      12 MAy 2008"                            ."
info     file
path    DIM        45                   .use Path name ie \\nts0\c\data\text        ." 
Subject  dim      150
record   dim      375
Files    dim      350

Recipients dim     350
BODY      DIM       250
Dump      FIle
          Move      "NSMTPMLR",program
BEGIN    CMATCH    B1 TO PROGRAM             .CHAINED FROM DSINIT?
         IF        EOS                       .NO
         stop
         else
         MOVE      "Names In The News" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "LOCAL" TO PRTNAME
         ENDIF
         MOVE      "SMTP Mailer" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
         call         getwinver
.test 
.          Move      "10.10.30.1",MailServer
          Move      "10.10.30.74",MailServer
.          Move      "smtp.office365.com",MailServer
.          move      "creques",user  
.          move      "creques@nincal.com",mailuser  
          Move      "DavidHerrick@nincal.com",Mailfrom
.          move      "creques@nincal.com",mailfrom
          Move      "DavidHerrick@nincal.com",Mailreply  
          move      "davidHerrick@nincal.com,robbwhiting@nincal.com,+14154337796@fax.nincal.com",mailto
.          move      "dherric@msn.com",mailto
.          Move      "99Webmail99",mailpass
          move      yes,mailtrace
.test 
.          move      yes,MailTTLS          
          move        no,mailttls 
          move      No,mailssl
          
        clock      port to str3           .plb note
        unpack     str3 into str2,str1
        pack       str3 from str1,str2
        move       str3 to portN         
         move      c3 to ncntpath
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
         if        over
                   move      c2 to cntprint
         endif

.BEGIN PATCH 7.7 ADDED LOGIC
.          move      ME,str1
.          scan      B1,CNTNAME
.          bump      CNTNAME
.          move      CNTNAME,str6
.          pack      str7,str1,str6
.          call      Trim using str7
.        pack         MailReply from str7,"@nincal.com"
          goto      infodone

         trap      noclue if io
         open      info,"C:\WORK\NSMTP.DAT"
getinfo  read      info,seq;record
         goto      infodone if over
         scan      "$SUBJECT=" in record
         if        equal
         bump      record by 9
         move      record to subject
         move      record to MailSubjct
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
         move      record to Files
         goto      getinfo
         endif
         reset     record
         scan      "$SENDTO=" in record
         if        equal
         bump      record by 8
         move      record to MailTo
         goto      getinfo
         endif
         reset     record
         scan      "$BODY=" in record
         if        equal
         bump      record by 6
         move      record to BODY
         move      record to MailBody
         goto      getinfo
         endif
         reset     record
         goto      getinfo
.these currently are the only three variables checked for just read until done
infodone close     info
         if        (files = "" & inpname <> "")
         move      inpname to files
         endif
         if        (Recipients = "" & Comment > "")
         move      Comment to Recipients
         endif
.................................................................
Subject  CMATCH    B1 TO SUBJECT
         IF        EOS
.         MOVE      "Here is your website test" to MailSubjct
         MOVE      "Here is your SMTP test" to MailSubjct
         endif
         CMATCH    B1 TO path
         IF        EOS
         MOVE      NTWKPATH1 to path
         endif

.   Set the text message that is send with the attachments
          Clear     mailAttach
          call      trim using files
          if        (files <> "")
          pack      Mailattach          from Path,"\",files           ."
          endif
          Clear     Mailbody
          append    body,Mailbody
          append    CRLF,MailBody
          append    MailAttach,Mailbody
          append    CRLF,MailBody
          append    "Reply to: ",mailbody   
          append    mailreply,mailbody
          reset     mailbody
          
          
          
          


        if        (files = "" or files = " ")
        else
        endif
drew
          Move      "Y",Mailtrace

          Call      SendMail
          if        (MailOkFlag = no)
          Prepare   Dump,"c:\work\MailError.dmp"
          write     dump,seq;S$ERROR$
          write     dump,seq;S$CMDLIN
          write     dump,seq;"from: ",mailfrom
          write     dump,seq;"to: ",mailto
          write     dump,seq;"server: ",mailserver
          write     dump,seq;"pass: ",mailpass
          weof      dump,seq
          close     dump
          endif

        shutdown
.......................................................................
.noclue   c:\WORK\NSMTP.dat file was not found - use default values
Noclue
        trapclr  io
        noreturn
        goto     subject
........................................................................
           include ncntio.inc
        include  comlogic.inc


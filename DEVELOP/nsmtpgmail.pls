PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC

Release   INit      "2.0"     DLH       SendMail
Reldate   INit      12 MAy 2008"                            ."
.release  init      "1.7"            DMB 02SEP2003          Added Text to Vars to check
.release  init      "1.6"            DMB 29Aug2003  Increased length of subjectline
.release  init      "1.5"           DLH 17Jan20001 If user = "null" user Information Services as sender   ."
.release  init      "1.4"           DLH 07Jan20001 Allow Destination addresses
.release  init      "1.3"           DLH 06Jan20001 Allow multiple file attachments
.release  init      "1.2"           ASH 03OCT20000 NEW SERVER ADDED
.release  init      "1.1"           DLH 12Sep00  add NSTMP.dat file & info
.release  init      "1.0"           DLH 28Sep99
.begin release 1.1
info     file
path    DIM        45                   .use Path name ie \\nts0\c\data\text        ." 
.;patch1.6
Subject  dim      150
.;Subject  dim      30
.;patch1.6
.begin patch 1.3
.record   dim      150
record   dim      375
Files    dim      350
Recipients dim     350
.end release 1.1
.end patch 1.3
.;patch1.7
BODY      DIM       250
.;patch1.7

BEGIN   
         MOVE      "Names In The News" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "LOCAL" TO PRTNAME
         MOVE      "SMTP Mailer" TO STITLE
.         CALL      PAINT
.         CALL      FUNCDISP
.................................................................
Subject  CMATCH    B1 TO SUBJECT
         IF        EOS
.begin patch 2.0
.         MOVE      "Here is your PDF file" to subject
         MOVE      "Here is your test file" to MailSubjct
         MOVE      "Here is your test file (not)" to Body
         
.end patch 2.0
         endif
         CMATCH    B1 TO path
         IF        EOS
.START PATCH 1.2 REPLACED LOGIC
.         MOVE      "g:\data" to path
         MOVE      NTWKPATH1 to path
.END PATCH 1.2 REPLACED LOGIC
         endif
.end release 1.1
          MOve        "Y",MailSSL    
          MOve        "Y",MailTrace
          move        "dherrick@nincal.com",Mailuser
          move        "3uDDuBy#$Q*B",mailpass
.begin patch 2.0
. pack    MailTo,"[FACSYS:JOSE{fax} Duenas@+1 415 433 7796]"
.         pack      MailTo,"Jose @ FACSys {FAX: 1(415)433-7796; TO: Jose}"
.         pack      MailTo,"FACSYS@nincal.com"
          MOVe      "dherrick@nincal.com",Mailto
.          move      "dherric@nincal.com",MailCC
.         MOVe      "14154337796",Mailto
.         move      "NTS3",MailServer

.   Set the text message that is send with the attachments
          Clear     mailAttach
.          pack      Mailattach          from "C:\work\pdf\nxch0006.pdf"

.       Move    inpname,SmtpTextMessage(1)   Array <Text message >
          Clear     Mailbody
          append    Mailto,Mailbody
          append    CRLF,MailBody
          append    body,Mailbody
          append    CRLF,MailBody
          append    MailAttach,Mailbody
          append    CRLF,MailBody
          reset     mailbody
          Move      "david.herrick@gmail.com",MailFrom

          if        (MailServer = "")
          Move      "smtp.gmail.com",MailServer
          endif
          if        (MailType < "0")
          move      c0,MailType         .text
          endif
          if        (MailTimer = "0")
          move      c5,MailTimer
          endif
          Count     n4,MailBody
          if        (N4 = c0)                              .body(text) null not allowed
          Append    B5,Mailbody
          Append    CRLF,Mailbody
          REset     Mailbody
          endif
          if        (MailTo = "")
.         add alert
          Alert  note,"I cannot send the email. No Destination!!!!",result
          Move      No,MailOkFlag
          return
          endif
          if        (MailFrom = "")
          Alert  note,"I cannot send the email. Sender!!!!",result
          Move      No,MailOkFlag
          return
          endif
          MOve      Yes,MailTrace
          call      Sendmail

        shutdown
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


PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         Include Smtp.PRI  (PRI - Profile Routine Interface)

release  init      "1.0"            DLH to test facsys via email
info     file

BEGIN   
         MOVE      "Names In The News" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "LOCAL" TO PRTNAME
         MOVE      "SMTP Mailer" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
          goto      test      
         trap      noclue if io
getinfo  
Test
.        move       "[FACSYS: David @+1 415433-7796]",mailto

          Move      "10.10.30.74",MailServer
          pack      Mailto,"IMCEAFACSYS-14154337796@nincal.com"
.          pack      Mailto,"+18553246978@fax.nincal.com"
          pack      Mailto,"+18553246978@fax.nincal.com"
.          pack      Mailto,"IMCEAFXC-Dave@18553246978@nincal.com"
.          pack      Mailto,"dherric@nincal.com"
          pack      Mailcc,"Creques@nincal.com,Rwhitin@nincal.com"
          pack      Mailfrom,"Creques@nincal.com"
         Move      "fax test Memo",MailSubjct
          Clear     MailBody
          append    "Fax Test",mailbody
          append    CRLF,Mailbody
         append       mailto,mailbody
          append    CRLF,Mailbody
         append   "<a href=#"http://web01/wp-content/forms/Employee-Handbook-January-2014.pdf#">Employee Handbook</a>",Mailbody
          append    CRLF,Mailbody
          Reset     MailBOdy
        move        "dherric",user
           move       c1,mailtype
          Call      SendMail

drew
        shutdown
.......................................................................
.noclue   c:\WORK\NSMTP.dat file was not found - use default values
Noclue
        trapclr  io
        noreturn
        goto     test
........................................................................
        include  comlogic.inc


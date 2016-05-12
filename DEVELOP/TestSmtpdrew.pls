
                Include Smtpdrew.PRI  (PRI - Profile Routine Interface)

Mess    Dim     250
F1      Form    1
NewLine Init    0x7F

.   For demostration purposes.
.   Set contents for call to Smtp.PLC and to send an eMail.
.   All the variables and their defaults are set in Smtp.Pri. All you need to
.   do is to change their values if you want to change the email.
.   This contents could be set directly or from a file
.   The arrays used are manually loaded for testing purposes
.   Identify the mail server and the mailer


.        Move    "%harkin",SmtpPassword                            ESMTP Password

.        Move    "NTS4",SmtpEmailServer                   Address of email server
        Move    "Mail.nincal.com",SmtpEmailServer                   Address of email server
        Move    "Dherric@nincal.com",SmtpEmailAddress                Your email address
        Move    "dherric",SmtpUserName                              User name
        Move    "David Herrick",SmtpUserFullName             User Full Name

.   Set the destinations of the email. Max 100 (Mime spec)
.   Take note : Some ISP's detect if the same email is send to the same destination and would therefore not duplicate
.               sending the same email as below. In this case only expect one email. If you send it to different email
.               addressed would each destination receive it's email copy.
        Move    "dherric@nincal.com",SmtpDestinations(1,1)            Destination e-mail addr
        Move    "David Herrick",SmtpDestinations(1,2)         Destination UserName
        Move    "Dherric@nincal.com",SmtpDestinations(2,1)            Destination e-mail addr
        Move    "David Herrick",SmtpDestinations(2,2)         Destination UserName

        Move    "2",SmtpDestIndexLast                               Index to last entry in Dest array

.   Set the subject that will be applied to the email message

        Move    "This is a test e-mail with 2 attachments",SmtpSubject Subject

.   Set the text message that is send with the attachments

        Move    "This is the text message line 1",SmtpTextMessage(1)   Array <Text message >
        Move    "This is the text message line 2",SmtpTextMessage(2)   Array <Text message >
        Move    "This is the text message line 3",SmtpTextMessage(3)   Array <Text message >
        Move    "This is the text message line 4",SmtpTextMessage(4)   Array <Text message >
        Move    "This is the text message line 5",SmtpTextMessage(5)   Array <Text message >

        Move    "5",SmtpTextIndexLast                                  Index to last entry in TextMessage array

.   Set all the attached files names. They could be the same file if you just
.   want to test it

.        Move    "5192_017865.pdf",SmtpAttachments(1,1)                        Attached file name
.        Move    "c:\work\pdf",SmtpAttachments(1,2)         Path to attached file name
        Move    "drew.pdf",SmtpAttachments(1,1)                        Attached file name
.        Move    "new.dat",SmtpAttachments(1,1)                        Attached file name
        Move    "c:\work\pdf",SmtpAttachments(1,2)         Path to attached file name

        Move    "1",SmtpAttIndexLast                                   Index to last entry - Only 1 entry

.   For debugging
.   "Dot" out the line 'Clear ..." if you want to enable the LogFile

        Move    "0",SmtpDispMode                                       Display log file
        Move    "c:\work\pdf\eMail.Log",SmtpLogFile        Path/filename to Log all socket read/writes
...     Clear   SmtpLogFile                                            'Clear' disables the LogFile

.   Progress bar. It is currently enabled so you can have an indication of what
.   goes on.If this lines of code is removed would the default be 0

        Move    "1",SmtpProgress                                       Enable progress bars

.   Send the above eMail to the destination which happens to be myself
.   If any errors detected the error code and human readable text is given

        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )

        If      NOT Equal
         Pack   Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                     "Status Code ",SmtpStatus," - ",SmtpStatusText
         Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
        Else
         Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
        Endif

        STOP


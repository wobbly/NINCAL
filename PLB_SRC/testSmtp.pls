               Include common.inc        
                Include Smtp2.PRI  (PRI - Profile Routine Interface)

.   For demostration purposes.
.   Set contents for call to Smtp.PLC and to send an eMail.
.   All the variables and their defaults are set in Smtp.Pri. All you need to
.   do is to change their values if you want to change the email.
.   This contents could be set directly or from a file
.   The arrays used are manually loaded for testing purposes
.   Identify the mail server and the mailer

.        Move    "mail.icon.co.za",EmailServer                   Address of email server
.         Move    "NTS3.nincal.com",EmailServer                   Address of email serverc
.         Move    "NTS4.nincal.com",EmailServer                   Address of email serverc
         Move    "mail.nincal.com",EmailServer                   Address of email serverc
.        Move    "plbsa@icon.co.za",EmailAddress                 Your email address
.        Move    "plbsa",UserName                                User name
.        Move    "Festus Redelinghuys",UserFullName              User Full Name
.        Move    "Dherric@astound.net",EmailAddress                 Your email address
.        Move    "DHerric",UserName                                User name
.        Move    "David Herrick",UserFullName              User Full Name
        Move    "informationservices@nincal.com",EmailAddress                 Your email address
        Move    "Dherric",UserName                                User name
        Move    "David Herrick",UserFullName              User Full Name

.   Set the destinations of the email. Max 100 (Mime spec)

.        Move    "plbsa@icon.co.za",Destinations(1,1)            Destination e-mail addr
.        Move    "Festus Redelinghuys",Destinations(1,2)         Destination UserName
.        Move    "DHerric@astound.net",Destinations(1,1)             Destination e-mail addr
.        Move    "David Herrick",Destinations(1,2)         Destination UserName
        Move    "crequest@nincal.com",Destinations(1,1)             Destination e-mail addr
        Move    "computer request",Destinations(1,2)         Destination UserName

.        Move    "Dherric@astound.net",Destinations(2,1)             Destination e-mail addr
.        Move    "David Herrick",Destinations(2,2)         Destination UserName
.        Move    "marius@is.co.za",Destinations(2,1)             Destination e-mail addr
.        Move    "Marius Redelinghuys",Destinations(2,2)         Destination UserName

        Move    "2",DestIndexLast                               Index to last entry in Dest array

.   Set the subject that will be applied to the email message

        Move    "This is a test e-mail with 4 attachments",SmtpSubject Subject

.   Set the text message that is send with the attachments

        Move    "This is the text message line 1",TextMessage(1)   Array <Text message >
        Move    "This is the text message line 2",TextMessage(2)   Array <Text message >
        Move    "This is the text message line 3",TextMessage(3)   Array <Text message >
        Move    "This is the text message line 4",TextMessage(4)   Array <Text message >
        Move    "This is the text message line 5",TextMessage(5)   Array <Text message >

        Move    "5",TextIndexLast                               Index to last entry in TextMessage array

.   Set all the attached files names. They could be the same file if you just
.   want to test it
   
        Move    "readme.txt",Attachments(1,1)                     Attached file name
        Move    "C:\Work",Attachments(1,2)           Path to attached file name

        Move    "readme.txt",Attachments(2,1)                     Attached file name
        Move    "C:\Work",Attachments(2,2)           Path to attached file name

        Move    "readme.txt",Attachments(3,1)                     Attached file name
        Move    "C:\Work",Attachments(3,2)           Path to attached file name

        Move    "standa~1.mdb",Attachments(4,1)                     Attached file name
        Move    "C:\Work",Attachments(4,2)           Path to attached file name

.        Move    "0",AttIndexLast                                Index to last entry - Only 1 entry
        Move    "4",AttIndexLast                                Index to last entry - Only 1 entry

.   For debugging

        Move    "0",DispMode                                    Display log file 
        Move    "C:\Work\smtpl.Log",LogFile          Path/filename to Log all socket read/writes
        Clear   LogFile                                         'Clear' disables the LogFile

.   Progress bar. It is currently enabled so you can have an indication of what
.   goes on.If this lines of code is removed would the default be 0

        Move    "1",Progress                                    Enable progress bars

.   Send the above eMail to the destination which happens to be myself
HereweGo
        Call    Send   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                                                                
.   Verify email transmission success
.   If you need to know more about any errors test 'Result' and 'Status' variables
.   for returned error codes from Winsock.rtn and Smtp.rtn
Mess    Dim     50
F1      Form    1

        If      NOT Equal
         Pack   Mess,"Error - Result=",Result," Status=",Status
         Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
        Else
         Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
        Endif

        STOP


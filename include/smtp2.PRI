*********************************************************************************
*                                                                               *     
*     Program Name         : Smtp.PRI Version 1.0                               *     
*                                                                               *     
*     Type of program      : This program sends email to a mail server(internet)*     
*                            It is entirely written in PL/B                     *     
*                            Very easy to interface to PL/B programs            *     
*                            Interface it to CGI's                              *     
*                                                                               *     
*     Routines required    : WinSock.Plc(WinSock.RTN,WinSock.PRI)               *     
*                          : SMTP2.PLC   (Smtp.RTN)                              *     
*                                                                               *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za             *
*                                                                               *     
*     Date                 : 07 November 1998                                   *     
*                                                                               *     
*     Copyright            : FreeWare                                           *     
*                                                                               *     
*     Specifications       : Email text 250 characters wide X 1000 lines        *     
*                          : 100 Attachments in any format(EXE,ZIP,OBJ...)      *     
*                          : 100 Different destinations                         *     
*                                                                               *     
*********************************************************************************
                           SetFlag Equal
                           Goto    SmtpJump if equal  Incase of illegal entry
...............................................................................
.   NOTES:

.   TECHNICAL INFORMATION.

.   The PLB Smtp mailer is based on Rfc821,Rfc822,Rfc2045.
.   I recommend that you search for these specifications on the internet to
.   be able to modify any of this programs code
.   If you obtained this program from the Sunbelt WebBoard in the .ZIP format
.   then the above 3 specifications would be included.

.   IMPORTANT.
.   This Simple Mail Transport Protocol (Smtp) written here is not intended
.   to replace or emulate current available Smtp mailers. It is rather a
.   PLB interface to Smtp to do interactive mailing from applications without
.   the need to manually send emails. The 'Text' section of the email is not
.   encoded to base 64 and must remain US-Ascii 7-bit ( Dec 0 to Dec 127 )
.   The reason being that other PLB applications can read the text section
.   without having to do any decoding first.
.   All attachments are encoded to base 64, whether they are US-Ascii text or
.   binary.
.   This Smtp application can be used in:
.   1. Electronic mailing of statements or messages.
.   2. Electronic distribution of programs/updates to clients
.   3. CGI auto email of eq. a password to a given eMail address to verify
.      a valid eMail address and the person who used that eMail address.
.   4. General posting of statistics from a database to clients.
.   5. Online registration of your applications without the user interfering
.      You could go as far as forcing a successful email send before an application
.      is enabled for use.You don't need a CGI to get the information
.   6. Use email to report application events (errors, backup's etc) to the
.      systems administrator
.   7. A remote application can send you data,text or binary files without
.      user intervention. This is handy if you need to manually edit/fix a remote file
.
.   WinSock.rtn has got a build in polling loop to test the status of the
.   Window socket during reading/writing. You don't need to loop to test the
.   Windows socket. A delay mechanism is build in for this purpose which uses
.   a sleep delay. This 'SleepEx' delay is a WinApi routine. SleepEx creates
.   delays by giving up a process's share of the multi tasking time slicing to
.   the amount of the requested delay. The end result of this type of delay
.   is VERY little processor overhead. When any IO action is sensed on the IO
.   port is the requested delay time cut short and processing continieues as
.   normal.

.   FIELD DESCRIPTIONS:

.   1.Result       - The return value/status of actions,generally from winsock.
.                    0 = Error - Futher detail unknown
.                    1 = Successfull
.                    2 = Mail server error code in Status
.                    x = Error and the error code
.   2.Status       - Status of Mail server communication. Error codes returned
.                    to indicate any errors during communication with the mail
.                    server. Some of the returned error codes would be duplicated
.                    in 'Result' above.Error codes above 100 are error codes
.                    returned from the mail server and Result will indicate so
.                    PLB has a bug in 'Call' using "xx" in that it gives F05 errors.
.                    To overcome the problem I used constants as below.
SmtpEc001  Form      "000" = Error unknown
SmtpEc002  Form      "001" = NO errors
SmtpEc003  Form      "002" = Result contains error code
SmtpEc004  Form      "003" = Lost syncronization with mail server. This would normally
.                            be due to a communications error in Winsock or a mail
.                            server that is not complient to Rfc 821,822,2045
SmtpEc010  Form      "010" = Failed to connect to Mail server
SmtpEc011  Form      "011" = Could not syncronize with mail server
SmtpEc012  Form      "012" = Your email address is invalid
SmtpEc013  Form      "013" = Destination invalid 
SmtpEc014  Form      "014" = Mail Server rejected command-Mail server is not Rfc821 complient
SmtpEc015  Form      "015" = Email data not accepted-Could be NON us-ascii characters in text section,
.                            corrupt attachment files. 
SmtpEc016  Form      "016" = Not currently used.
.   3.EmailServer  - Address of email server. This could be either a IP address number
.                    or name eq. "mail.icon.co.za" or "196.26.208.2"
.   4.EmailAddress - Your email address.eq. "plbsa.icon.co.za" 
.   5.UserName     - Your user name. Normally you mailbox name eq. plbsa
.   6.UserFullName - User Full Name. This will appear on the email.
.                    This need not be any registered name Eq. "Festus Redelinghuys"
.   7.PassWord     - Not currently implemented, must be ignored.
.   8.Destinations - Smtp protocol allows a maximum of 100 destinations for any
.                    single email message. Using multiple destinations instead
.                    of sending the email for each destination reduces network
.                    traffic. 'Destinations' is an array 100 deep(max connections)
.                    with three members in the array
.                    1. - Destination - Max 256 characters and must contain
.                         atleast one valid email address destination for the email.
.                    2. - Destination user name.
.                    3. - Status - An entry would be placed in this field if any
.                         errors occur for this destination.
.   9.DestIndexLast- Index to last entry in Destination array. This field MUST be
.                    filled in with the index into the last array entry of the destinations.
.                    Eq. If an email message is send to only one destination then
.                    this field should contain '1'.
.  10.Subject      - Subject about the e-mail. This would appear on all the emails
.                    send
.  11.TextMessage  - Text message. 250 characters wide and 1000 lines deep.
.                    ONLY 7 bit Ascii characters allowed in the text.
.                    ( your normal English printable Ascii character set ). No encoding is done
.                    on the text section and may not contain any ALT-X type characters
.                    Only the lines upto the 'TextIndexLast' index is transmitted.
.                    The Smtp mailer inserts a CRLF for each line and therefore
.                    the text lines must not use CRLF for the next line unless
.                    it is a purpose double spacing.                   
.  12.TextIndexLast- Index to last entry in TextMessage array.This MUST be set and may be
.                    zero if there is no text lines to be send.
.  13.Attachments  - Up to 100 files/documents may be attached to the email.
.                    All attachments are considered to be 8 bit characters whether they
.                    are or not and is automatically Encoded before they are send.
.                    to the destination. The encoding format is Mime Base 64 which
.                    is the most commonly format used, including by Unix.
.                    You can therefore attach Exe,Zip,Doc,Txt,Dbs,PLC etc....
.                    The 'Attachments' is an array 100 deep( Max Attachments) by
.                    three members:
.                    1. File name - Only file name and MUST include an extention
.                                   This will also be the file name at the destination
.                    2. Path      - Path to this local file on your computer
.                                   without the filename eq. "C:\Develop\WinCode" and
.                                   must NOT end with a '\'. This path is only used to load
.                                   the attachment file.
.                    3. Status    - The status of the encoding and transmission of the file
.                                   If any errors occured would it be reflected in this field
.  14.AttIndexLast - Index to last entry in Attachment array. This index is taken as the
.                    indicator whether there are any attachments. If zero then NO attachments
.                    would be send.
.  15. DispMode    - Used to test an application 
.                    0 = Normal, Log file entries are not displayed
.                    1 = Display the Log file entries as they are made
.                        This is very useful to check communication timing
.                    2 = Same as 1 but a keyin statement holds the screen
.                        until enter is pressed for the next line
.  16. LogFile     - Path and file name of LogFile
.                    Used to log all Winsock Read/Write/Open/Close and Status actions
.                    If the LenghtPointer (LP) of LogFile is any other than zero
.                    then an attempt is made to create the file. If not used clear
.                    the FP and LP of Logfile to zero
.  17. Progress    - Progress bar. Display a progress bar during transmission
.                    0 = Progress bar disabled
.                    1 = Progress bar enabled ( You must turn the display of the LogFile off )


              SetFlag         Equal               Remove warning from compiler
              Goto            SmtpJump If Equal   Incase of illegal entry always jump

              Include         WinSock2.PRI         Profile Routine Interface

.------------------------------------------------------------------------------

.   Variables used in calling SMTP2.PLC

Result              Form    3                 Result
Status              Form    3                 Status of Mail server communication
EmailServer         Dim     64                Address of email server
EmailAddress        Dim     256               Your email address
UserName            Dim     64                User name
UserFullName        Dim     64                User Full Name
PassWord            Dim     15                User e-mail password
Destinations        Dim     256(100,3)        Array <Dest. e-mail addr>,<Dest. UserName><status>
DestIndexLast       Form    3                 Index to last entry in Dest array 
;Subject             Dim     256               Subject (Max
SmtpSubject         Dim     256               Subject (Max
TextMessage         Dim     250(1000)         Array <Text message >
TextIndexLast       Form    4                 Index to last entry in TextMessage array
Attachments         Dim     256(100,3)        Array <Filename.ext><Local path><status>
AttIndexLast        Form    3                 Index to last entry in Attachment array
DispMode            Form    1                 Display log during session
LogFile             Dim     256               Path/filename to Log all socket read/writes
ServerCode          Dim     3                 Codes returned by mail server
Progress            Form    1                 Display progress bar

...............................................................................
...............................................................................
.   SMTP2.PLC entry points

Send    CallS           "SMTP2.PLC;Send" Using Result: Result
                        Status:         Status of Mail server communication
                        EmailServer:    Address of email server
                        EmailAddress:   Your email address
                        UserName:       User name
                        UserFullName:   User FULL Name
                        PassWord:       User e-mail password
                        Destinations:   All Destinations
                        DestIndexLast:  Index to last entry in Dest array 
                        SMTPSubject:        Subject
                        TextMessage:    Text message
                        TextIndexLast:  Index to last entry in TextMessage array
                        Attachments:    All attachments list ';' seperated
                        AttIndexLast:   Index to last entry in Attachment array
                        DispMode:       Display Logfile entries as they are made
                        LogFile:        Log file name for all IO via WinSock2.PLC
                        ServerCode:     Codes returned by mail server
                        Progress        Display progress bar
        RETURN
...............................................................................
SmtpJump
.                       END OF SMTP.PRI
...............................................................................


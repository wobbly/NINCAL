
*******************************************************************************
*                                                                             *     
*     Program Name         : Smtp.Rtn Version 1.0                             *     
*                                                                             *     
*     Type of program      : Smtp interface for Plb                           *     
*                                                                             *     
*     Compilation          : Compile as a ROUTINE                             *     
*                                                                             *     
*     Files required       : WinSock.Plc  (Winsock.Rtn,Winsock.PRI)           *     
*                          : ProBar .Plc  (ProBar .Rtn,ProBar .PRI)           *     
*                                                                             *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za           *
*                                                                             *     
*     Date                 : 12 November 1998                                 *     
*                                                                             *     
*     Copyright            : All rights reserved - Festus Redelinghuys        *     
*                                                                             *     
*******************************************************************************
              SetFlag         Equal               Remove warning from compiler
              Goto            SmtpJump If Equal   Incase of illegal entry always jump
...............................................................................
              Include         WinSock.PRI         Profile Routine Interface
              Include         ProBar.PRI          Inline progress bar
.------------------------------------------------------------------------------
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

.   If communication errors are experienced would it seem that the program
.   'freezes' up. This is NOT the case. The Winsock.Rtn is looping to continieu
.   with the transmission and the aforesaid SleepEx loop will time out after 30 seconds
.   If you want to see this happen enable 'DispMode' and set the 'LogFile' name
.   for the Winsock transactions to be logged into.

.   FIELD DESCRIPTIONS:

.   1.Result       - The return value/status of actions,generally from winsock.
.                    000 = Error - Futher detail unknown
.                    001 = Successfull
.                    002 = Mail server error code in Status
.                    xxx = Error and the error code
.   2.ResultText   - Human readable text of above code
.   3.Status       - Status of Mail server communication. Error codes returned
.                    to indicate any errors during communication with the mail
.                    server. Some of the returned error codes would be duplicated
.                    in 'Result' above.Error codes above 100 are error codes
.                    returned from the mail server and Result will indicate so
.                    PLB has a bug in 'Call' using "xx" in that it gives F05 errors.
.                    To overcome the problem I used constants .
.   4.StatusText   - Human readable text of above code
.   5.EmailServer  - Address of email server. This could be either a IP address number
.                    or name eq. "mail.icon.co.za" or "196.26.208.2"
.   6.EmailAddress - Your email address.eq. "plbsa.icon.co.za" 
.   7.UserName     - Your user name. Normally you mailbox name eq. plbsa
.   8.UserFullName - User Full Name. This will appear on the email.
.                    This need not be any registered name Eq. "Festus Redelinghuys"
.   9.PassWord     - Not currently implemented, must be ignored.
.  10.Destinations - Smtp protocol allows a maximum of 100 destinations for any
.                    single email message. Using multiple destinations instead
.                    of sending the email for each destination reduces network
.                    traffic. 'Destinations' is an array 100 deep(max connections)
.                    with three members in the array
.                    1. - Destination - Max 256 characters and must contain
.                         atleast one valid email address destination for the email.
.                    2. - Destination user name.
.                    3. - Status - An entry would be placed in this field if any
.                         errors occur for this destination.
.  11.DestIndexLast- Index to last entry in Destination array. This field MUST be
.                    filled in with the index into the last array entry of the destinations.
.                    Eq. If an email message is send to only one destination then
.                    this field should contain '1'.
.  12.Subject      - Subject about the e-mail. This would appear on all the emails
.                    send
.  13.TextMessage  - Text message. 250 characters wide and 1000 lines deep.
.                    ONLY 7 bit Ascii characters allowed in the text.
.                    ( your normal English printable Ascii character set ). No encoding is done
.                    on the text section and may not contain any ALT-X type characters
.                    Only the lines upto the 'TextIndexLast' index is transmitted.
.                    The Smtp mailer inserts a CRLF for each line and therefore
.                    the text lines must not use CRLF for the next line unless
.                    it is a purpose double spacing.                   
.  14.TextIndexLast- Index to last entry in TextMessage array.This MUST be set and may be
.                    zero if there is no text lines to be send.
.  15.Attachments  - Up to 100 files/documents may be attached to the email.
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
.  16.AttIndexLast - Index to last entry in Attachment array. This index is taken as the
.                    indicator whether there are any attachments. If zero then NO attachments
.                    would be send.
.  17. DispMode    - Used to test an application 
.                    0 = Normal, Log file entries are not displayed
.                    1 = Display the Log file entries as they are made
.                        This is very useful to check communication timing
.                    2 = Same as 1 but a keyin statement holds the screen
.                        until enter is pressed for the next line
.  18. LogFile     - Path and file name of LogFile
.                    Used to log all Winsock Read/Write/Open/Close and Status actions
.                    If the LenghtPointer (LP) of LogFile is any other than zero
.                    then an attempt is made to create the file. If not used clear
.                    the FP and LP of Logfile to zero
.  19. Progress    - Progress bar. Display a progress bar during transmission
.                    0 = Progress bar disabled
.                    1 = Progress bar enabled ( You must turn the display of the LogFile off )

.------------------------------------------------------------------------------

...............................................................................
.   Local variables
taskname        dim     1000
result          form    9

SmtpLoopDest            Form    3       Loop counter for destination array
SmtpLoopText            Form    3       Loop counter for Text array
SmtpLoopAttach          Form    3       Loop counter for Attachments array
SmtpEncodeFileName      Dim     260     Temp encoded file output name
SmtpEncodeFile          FILE            .File to be encoded and transmitted
SmtpVeriCode            Dim     3       Verify code against server returned code
SmtpVersion             Dim     17      Clocked version info
SmtpSysDate             Dim     41      Clock system date
SmtpClockWeekDay        Dim     3       System clocked Mon..Sun
SmtpClockMonth          Dim     3       System clocked Jan..Dec
SmtpClockDay            Dim     2       System clocked 01..31
SmtpClockTime           Dim     8       System clocked 00:00:00
SmtpClockYear           Dim     4       System clocked yyyy 1998....
SmtpClockRunVersion     Dim     5       System clocked runtime version '8.2G '
SmtpClockRunTime        Dim     8       System clocked Runtime name 'PLBWIN  '
SmtpClockUnknown        Dim     2       System clocked Unknown value
SmtpClockDate           Dim     8       System clocked date mm-dd-yy
SmtpRecvCode            Dim     3       Code received from server
SmtpClockTimeZone       Dim     3       System clocked CDT,GMT,EST...

.   Buffers
SmtpHelpCmd             Dim     1       Help info returned from server-Disabled
SmtpMailList            Dim     4096    MailList buffer
SmtpRecvExtra           Dim     4096    Extra buffer receive

.   Error code list
SmtpEc001  Form      "000"
SmtpEc002  Form      "001" 
SmtpEc003  Form      "002" 
SmtpEc004  Form      "003" 
SmtpEc010  Form      "010" 
SmtpEc011  Form      "011" 
SmtpEc012  Form      "012" 
SmtpEc013  Form      "013"  
SmtpEc014  Form      "014" 
SmtpEc015  Form      "015"  
SmtpEc016  Form      "016" 
SmtpErrorList  Init  "000 - Refer to Status - Winsock                                                 ":
                     "001 - NO errors                                                                 ":
                     "002 - Result contains error code                                                ":
                     "003 - Lost syncronization with mail server. This would normally                 ":
                     "010 - Failed to connect to Mail server                                          ":
                     "011 - Could not syncronize with mail server                                     ":
                     "012 - Your email address is invalid                                             ":
                     "013 - Destination invalid                                                       ":
                     "014 - Mail Server rejected command-Mail server is not Rfc821 complient          ":
                     "015 - Email data not accepted-Could be NON us-ascii characters in text section  ":
                     "016 - Not currently used.                                                       " 
SmtpScanCode   Init  "      "           Error scan code key

.   General
SmtpSeq                 Form    "-1"    Sequential read forward
SmtpF10                 Form    10      Work storage
SmtpF1                  Form    1       Work Storage
SmtpD3                  Dim     3       Work Storage
SmtpD10                 Dim     10      Work Storage
SmtpD1                  Dim     1       Work Storage
SmtpCRLF                Init    0x0D,0x0A Carraige return line feed
SmtpQuote               Init    "#""    Literal Quote
SmtpH0A                 Init    0x0D    CR in hex
SmtpH0D                 Init    0x0A    LF in hex
SmtpH3F                 Init    0x3F    6 Bit AND mask
SmtpInitZero            Init    0x00    Used for zero termination
.
...............................................................................
.   Entry point variables

SmtpResult              Form    ^         3   Smtp Result
SmtpResultText          Dim     ^         80  Human readable text for above code
SmtpStatus              Form    ^         2   Winsock status
SmtpStatusText          Dim     ^         80  Human readable text for above code
SmtpEmailServer         Dim     ^         64  Address of email server
SmtpEmailAddress        Dim     ^         256 Your email address
SmtpUserName            Dim     ^         64  User name
SmtpUserFullName        Dim     ^         64  User Full Name
SmtpPassWord            Dim     ^         15  User e-mail password
SmtpDestinations        Dim     ^(100,3)  256 Array <Dest. e-mail addr>,<Dest. UserName><status>
SmtpDestIndexLast       Form    ^         3   Index to last entry in Dest array 
SmtpSubject             Dim     ^         256 Subject (Max
SmtpTextMessage         Dim     ^(1000)   Array <Text message >
SmtpTextIndexLast       Form    ^         4   Index to last entry in TextMessage array
SmtpAttachments         Dim     ^(100,3)  256 Array <Filename.ext><Local path><status>
SmtpAttIndexLast        Form    ^         3   Index to last entry in Attachment array
SmtpDispMode            Form    ^         1   Display Logfile entries as they are made
SmtpLogFile             Dim     ^         256 Path/filename to Log all socket read/writes
SmtpServerCode          Dim     ^         3   Codes returned by mail server
SmtpProgress            Form    ^         1   Display progress bar

...............................................................................
.   SMTP entry points

SEND    ROUTINE         SmtpResult:                     Result of winsock or other
                        SmtpResultText:                 Human readable text for above code
                        SmtpStatus:                     Status of Mail server communication
                        SmtpStatusText:                 Human readable text for above code
                        SmtpEmailServer:                Address of email server
                        SmtpEmailAddress:               Your email address
                        SmtpUserName:                   User name
                        SmtpUserFullName:               User FULL Name
                        SmtpPassWord:                   User e-mail password
                        SmtpDestinations:               All Destinations 
                        SmtpDestIndexLast:              Index to last entry in Dest array 
                        SmtpSubject:                    Subject
                        SmtpTextMessage:                Text message
                        SmtpTextIndexLast:              Index to last entry in TextMessage array
                        SmtpAttachments:                All attachments list ';' seperated
                        SmtpAttIndexLast:               Index to last entry in Attachment array
                        SmtpDispMode:                   Display Logfile entries as they are made
                        SmtpLogFile:                    LogFile name/Path
                        SmtpServerCode:                 Codes returned by mail server
                        SmtpProgress                    Display progress bar

.   Hide the primary window.

        If              ( SmtpDispMode = 0 )            Must we hide the display?
         WinHide
        Endif

.   If a progress bar has been selected set the defaults now

        If              ( SmtpProgress = 1 )            Progress bar display enabled?
         Move           "PL/B Smtp Emailer",ProBarTitle Set Progress Bar window title
         Move           "1",ProBarShow                  Enable display of progress bar
        Endif

.   Execute the Smtp.RTN now. The routine would return to here if any errors.
.   On EXIT would the returned flag indicate the status of this process.

        Clock           SysDate,SmtpSysDate             Get current system date(24)

        Call            SmtpROUTINE                     Do this routine/program now

        If              Equal                           Any errors?
         If             ( SocketVariables = 1 )         Only if var have been initialized
          Call          SmtpQuit                        Close
          Call          SocketClose                     Close the windows socket
         Endif
         If             ( SmtpProgress = 1 )            Progress bar display enabled?
          Move          "0",ProBarShow                  Distroy progress bar
          Call          ProBarUpdate                    Display and update Progress Bar
         Endif
         Move           SocketStatus,SmtpStatus         Retrieve status from winsock
         Move           SocketStatusText,SmtpStatusText Retrieve status from winsock
         Setflag        Equal
         RETURN                                         WITH SUCCESS
        Else
         If             ( SocketVariables = 1 )         Only if var have been initialized
          Call          SmtpQuit                        Attempt to quit mail server
          Call          SocketClose                     Close the windows socket
         Endif
         If             ( SmtpProgress = 1 )            Progress bar display enabled?
          Move          "0",ProBarShow                  Distroy progress bar
          Call          ProBarUpdate                    Display and update Progress Bar
         Endif
         Move           SocketStatus,SmtpStatus         Retrieve status from winsock
         Move           SocketStatusText,SmtpStatusText Retrieve status from winsock
         Setflag        NOT equal                       Overide Socket close status, remember failure
         RETURN                                         WITH FAILURE
        Endif

.==============================================================================
.   Execution point of Smtp.Rtn

SmtpROUTINE

...............................................................................
.   Validate any attachments.
.   We only use attached files,if any attachments, halfway throught the program
.   and if any of the attached files are not found on the local disk will it
.   cause an invalid email being started. Before we make any connection we
.   first validate that all the files and paths specified for attachments are
.   valid.

        If              ( SmtpAttIndexLast != 0 )       Is there any attachments
         If             ( SmtpProgress = 1 )            Enable Progress bar display
          Move          SmtpAttIndexLast,ProBarHiRange  Get upper range of Progress bar
         Endif
         Move           "0",SmtpLoopAttach              Init Attachment array
         Loop
          Add           "1",SmtpLoopAttach              Point to next text message line
          If            ( SmtpProgress = 1 )            Progress bar display enabled?
           Pack         ProBarText,"Verifying attachment ",SmtpLoopAttach," of ",SmtpAttIndexLast
           Move         SmtpLoopAttach,ProBarCurValue   Get current bar value
           Call         ProBarUpdate                    Display and update Progress Bar
          Endif
         Until          ( SmtpLoopAttach > SmtpAttIndexLast) .Do all text lines
          Pack          SmtpEncodeFileName,SmtpAttachments(SmtpLoopAttach,2):   Build file name PLB read
                        "\":
                        SmtpAttachments(SmtpLoopAttach,1)
          Trap          SmtpBadAttachment if IO         Incase file not found
          Open          SmtpEncodeFile,SmtpEncodeFileName  Open our temp encoded file
          TrapClr       IO                              Not needed anymore
          Close         SmtpEncodeFile                  File and path OK
         REPEAT                                         Do all files and paths
        Endif
...............................................................................
.   Connect to the mail server

        Call            SmtpDial                        Connect to mail server
        Return          If not equal                    Abort if connection failed
        Call            SmtpReceive,"220",SmtpEc010     We expect Server ready
        If              Not Equal                       Sync failed?
         Call           SmtpReceive,"220",SmtpEc010     Try once more
         Return         If Not Equal                    Failed to sync a second time?
        Endif
        Call            SmtpHELO                        Greet the server with our name
        Call            SmtpReceive,"250",SmtpEc011     We expect OK from Server
        Return          If not Equal                    Abort if sync lost

...............................................................................
.   Initialize message and set destinations. Max 100 destinations allowed
.   In the event that a destination is not accepted would that destination be skipped

        Call            SmtpMAIL                        .From
        Call            SmtpReceive,"250",SmtpEc012     We expect OK from Server
        Return          If not Equal                    Abort if sync lost

        Move            "0",SmtpLoopDest                Init the destination loop
        Loop
         Add            "1",SmtpLoopDest                Point to next destination
         If             ( SmtpProgress = 1 )            Progress bar display enabled?
          Pack          ProBarText,"Verifying Destinations ",SmtpLoopDest," of ",SmtpDestIndexLast
          Move          SmtpLoopDest,ProBarCurValue     Get current bar value
          Call          ProBarUpdate                    Display and update Progress Bar
         Endif
        Until           ( SmtpLoopDest > SmtpDestIndexLast | SmtpLoopDest > 100 ) 
         Call           SmtpRCPT                        .To
         Call           SmtpReceive,"250",SmtpEc013     We expect OK from Server
         Return         If Not Equal                    Return if Recipient is invalid
        REPEAT

...............................................................................
.   Send DATA header portion of email message

        If              ( SmtpProgress = 1 )            Progress bar display enabled?
         Move           SmtpDestIndexLast,ProBarHiRange Get upper range of Progress bar
        Endif

        Call            SmtpDATA                        Inform server DATA is following
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpReceive,"354",SmtpEc014     Expect 'Start mail input' from server               
        Return          If not Equal                    Abort if sync lost

.   Origin and destinations of text

        Call            SmtpTextMessageID               Message-ID:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextFrom                    .From:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextTo                      .To:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextSubject                 Subject:
        Return          If Not Equal                    Respond to comm's errors

.   Tell mail server what to expect in the DATA section of the email

        Call            SmtpTextMIME                    MIME-Version:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpBase64Content               Emmulate contents are plain and base64
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextTransfer7               Content-Transfer-Encoding:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextPriority                X-Priority:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextMsMail                  X-MSMail-Priority:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextMailer                  X-Mailer:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextOle                     X-MimeOLE:
        Return          If Not Equal                    Respond to comm's errors

.   Send header of text message

        Call            SmtpTextNextPart                Text section starts
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpPlainContent                Text is always plain type
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpTextTransfer7               Content-Transfer-Encoding:
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpCRLF                        Must be before text

.   Send body of text message

        Move            "0",SmtpLoopText                Init text message array
        Loop
         Add            "1",SmtpLoopText                Point to next text message line
         If             ( SmtpProgress = 1 )            Progress bar display enabled?
          Pack          ProBarText,"Sending eMail message line ",SmtpLoopText
          Move          SmtpLoopText,ProBarCurValue     Get current bar value
          Call          ProBarUpdate                    Display and update Progress Bar
         Endif
        Until           ( SmtpLoopText > SmtpTextIndexLast ) Do all text lines
         Call           SmtpLineDATA                    Send the line of data
         Return         If not equal                    Abort if comm's errors
        REPEAT

...............................................................................
.   Send attachments - Send attachment header first

        If              ( SmtpAttIndexLast != 0 )       Is there any attachments

         Move           "0",SmtpLoopAttach              Init Attachment array
         Loop
          Add           "1",SmtpLoopAttach              Point to next text message line
         Until          ( SmtpLoopAttach > SmtpAttIndexLast) .Do all text lines
          Call          SmtpTextNextPart                Insert next part seperator
          Return        If Not Equal                    Respond to comm's errors
          Call          SmtpTextContentApp              Set document name
          Return        If Not Equal                    Respond to comm's errors
          Call          SmtpTextTransfer64              Set encoding base 64
          Return        If Not Equal                    Respond to comm's errors
          Call          SmtpTextDispo                   Dispossition-attachment
          Return        If Not Equal                    Respond to comm's errors
 
.   Build attachment file path and name and open. The file read,base64 encode
.   and transmit is done in one routine to increes speed.

          Call          SmtpEncodeAndSend               Encode the file and transmit
          Return        If not Equal                    Abort if any errors
         REPEAT 
         Call           SmtpTextLastPart                Insert Last part terminator
         Return         If Not Equal                    Comm's error?       
        Endif                                           .END of attachments

...............................................................................
.   End of DATA section

        Call            SmtpEndDATA                     Inform server it was last of text data
        Return          If Not Equal                    Respond to comm's errors
        Call            SmtpReceive,"250",SmtpEc015     We expect OK from Server
        Return          If not Equal                    Abort if server did not accept DATA
 
.   Return with a good status

        Setflag         Equal                           Set return Flag
        RETURN                                          

.------------------------------------------------------------------------------
.   For future release. Reading what mail is at the mail server and downloading
.   the email.
Receive         ROUTINE         SmtpResult:             Result
                                SmtpEmailServer:        Address of email server
                                SmtpEmailAddress:       Your email address
                                SmtpUserFullName:       User Name
                                SmtpPassWord            User e-mail password
                RETURN

.==============================================================================
.                              SUB ROUTINES
.==============================================================================

.   Standard Commands that can be send to the server
...............................................................................

.   Identify the sender to the mail server
.   Cmd Format - HELO <SP> <DOMAIN> <CRLF>
SmtpHELO                             
                Pack    SocketTxBuffer,"HELO":          Server Command                                       " ":             Space seperator
                                       " ":             Space seperator
                                       SmtpEmailServer: Domain name
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Sender initiates a mail transaction with server   
.   Cmd Format - MAIL <SP> FROM:<reverse-path> <CRLF>
SmtpMAIL                            
                Pack    SocketTxBuffer,"MAIL":          Server Command
                                       " ":             Space seperator
                                       "FROM:":         From identifier
                                       "<":             Left Bracket
                                       SmtpEmailAddress:      Originator email address
                                       ">":             Right Bracket
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Identify an individual receiver of an email
.   Cmd Format - RCPT <SP> TO:<Forward-path> <CRLF>
SmtpRCPT                               
                Pack    SocketTxBuffer,"RCPT":          Server Command
                                       " ":             Space seperator
                                       "TO:":           From identifier
                                       "<":             Left Bracket
                                       SmtpDestinations(SmtpLoopDest,1): Destination email address
                                       ">":             Right Bracket
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

...............................................................................

.   Start DATA sending sequence
.   Cmd Format - DATA <CRLF>
SmtpDATA                             
                Pack    SocketTxBuffer,"DATA":          Server Command
                                       " ":             Space seperator
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

...............................................................................

.   Send data. Max 76 characters per line
.   Cmd Format - <string max 76>
.   As per Rfc821: If the first char is a '.' then another '.' must be inserted

SmtpLineDATA                          
                Cmatch  ".",SmtpTextMessage(SmtpLoopText) Have we got a leading '.'?
                If      Equal
                 Pack   SocketTxBuffer,".",SmtpTextMessage(SmtpLoopText),SmtpCRLF Insert additional '.'
                Else
                 Pack   SocketTxBuffer,SmtpTextMessage(SmtpLoopText),SmtpCRLF
                Endif
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

...............................................................................

.   End data sending sequence
.   Cmd Format - <.> <CRLF>
SmtpEndDATA                             
                Move    "                                     ",SocketTxBuffer
                Clear   SocketTxBuffer
                Pack    SocketTxBuffer,".":             Single full stop on its own line
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

...............................................................................

.   Carraige return/Line feed when needed
.   Cmd Format - <CRLF>
SmtpCRLF                                 
                Pack    SocketTxBuffer,SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

...............................................................................

.   Initiate mail transaction to TERMINALS
.   Cmd Format - SEND <SP> FROM:<reverse-path> <CRLF>
SmtpSEND                               
                Pack    SocketTxBuffer,"SEND":          Server Command
                                       " ":             Space seperator
                                       "FROM:":         From identifier
                                       "<":             Left Bracket
                                       SmtpEmailAddress: Originator email address
                                       ">":             Right Bracket
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
                
...............................................................................

.   Initiate mail transaction to TERMINALS and MAIL BOXES
.   Cmd Format - SOML <SP> FROM:<reverse-path> <CRLF>
SmtpSOML                              
                Pack    SocketTxBuffer,"SOML":          Server Command
                                       " ":             Space seperator
                                       "FROM:":         From identifier
                                       "<":             Left Bracket
                                       SmtpEmailAddress: Originator email address
                                       ">":             Right Bracket
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Initiate mail transaction to TERMINALS and MAIL BOXES.   
.   Cmd Format - SAML <SP> FROM:<reverse-path> <CRLF>
SmtpSAML                             
                Pack    SocketTxBuffer,"SAML":          Server Command
                                       " ":             Space seperator
                                       "FROM:":         From identifier
                                       "<":             Left Bracket
                                       SmtpEmailAddress: Originator email address
                                       ">":             Right Bracket
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Abort current mail transaction
.   Cmd Format - RSET <CRLF>
SmtpRSET                              
                Pack    SocketTxBuffer,"RSET":          Server Command
                                       " ":             Space seperator
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Verify user name 
.   Cmd Format - VRFY <SP> <string> <CRLF>
SmtpVRFY                               
                Pack    SocketTxBuffer,"VRFY":          Server Command
                                       " ":             Space seperator
                                       SmtpUserName:    User name to verify
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Verify a valid mailing list
.   Cmd Format - EXPN <SP> <string> <CRLF>
SmtpEXPN                               
                Pack    SocketTxBuffer,"EXPN":          Server Command
                                       " ":             Space seperator
                                       SmtpMailList:    Mailing list to verify
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Server would send HELP information
.   Cmd Format - HELP <SP> <string> <CRLF>
SmtpHELP                                 
                Pack    SocketTxBuffer,"HELP":          Server Command
                                       " ":             Space seperator
                                       SmtpHelpCmd:     Request help on server command
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   No operation - NOP
.   Cmd Format - NOOP <CRLF>
SmtpNOOP                             
                Pack    SocketTxBuffer,"NOOP":          Server Command
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag
...............................................................................

.   Server must send OK reply and then quit
.   Cmd Format - QUIT <CRLF>
SmtpQUIT                               
                Pack    SocketTxBuffer,"QUIT":          Server Command
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                Return  If not Equal                    Abort if Comm's errors
;DB Commented out-smtp receive check - caught in loop because nts3 not giving reponse back to quit command or plb not recognizing it
;12/30/02
;                Call    SmtpReceive,"221",SmtpEc003     We expect Closing channel from server
.                                                       Ignore result,closing anyway
                Call    SocketClose                     Close the socket                
                RETURN                                  Return with Tx result flag

...............................................................................

.   Server must take over Master function
.   Cmd Format - TURN <CRLF>
SmtpTURN                                 
                Pack    SocketTxBuffer,"TURN":          Server Command
                                       SmtpCRLF         Terminate with CRLF
                Call    SocketWrite                     Transmit command
                RETURN                                  Return with Tx result flag

.------------------------------------------------------------------------------
.   Interface this program with the Socket.PLC(rtn) via Socket.RPI(include file)
.   This is the total interface with the e-mail server and TCP/IP Windows socket
...............................................................................
.   Dial and connect to the server
.   If a TCP/IP connection does not exist use the Windows dialer to connect

SmtpDial                                                                
                Move    SmtpEmailServer,SocketHostIP    Set mail server IP address
                Move    "25",SocketPort                 Mail servers are on port 25
                Move    "O",SocketMethod                Open our port
                Move    "R",SocketPacketType            Raw data
                Move    SmtpDispMode,SocketDispMode     Set display mode
                Move    SmtpLogFile,SocketLogFile       Get the Logfile name
                Call    SocketConnect                   Connect to ISP
                If      Not Equal                       Failed to connect?
                 Move   "010",SmtpStatus                Set the error code
                 Call   SmtpErrorText                   Get human readable text of error code
                 SetFlag Not Equal                      Retain flag
                Endif
                RETURN                                  With flags

...............................................................................
.   Read the mail server and unpack buffer received
.   SmtpStatus does nothing but change the variable to the passed value incase
.   a error occurs.

SmtpReceive     LROUTINE  SmtpVeriCode,SmtpStatus

                Call    SocketRead                      Read data from mail server
                Return  If Not Equal                    

.   Parse out message from the server

                Move    "    ",SmtpRecvCode             Make sure no garbage left over
                Unpack  SocketRxBuffer,SmtpRecvCode,SmtpRecvExtra

.   We always expect a certain return code from the server for any command
.   we issue to the server. When the return code is received we test that
.   return code against a presset expected return code. If it does not match
.   then something has gone wrong.

.   The following code may seem to be lenghty and that it could have been
.   written much shorter, Yes, but I kept it in is current format for future
.   development whereby more 'artificial intellegence' could be incorporated
.   into responses to the mail server.

.   Code received from server is what we expected.
.   Decode the code received from the server

                Move            SmtpRecvCode,SmtpServerCode For return if error
                Switch          SmtpRecvCode
...............................................................................
.   Syntax error,cmd unrecognised

                Case            "500"                   Syntax error,cmd unrecognised

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN

...............................................................................
.   Syntax error in parameters

                Case            "501"                   Syntax error in parameters

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Command not implemented

                Case            "502"                   Command not implemented

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Bad sequence of commands

                Case            "503"                   Bad sequence of commands

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Cmd parameter not implemented

                Case            "504"                   Cmd parameter not implemented

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   System status or help reply

                Case            "211"                   System status or help reply

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Help message

                Case            "214"                   Help message

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   <domain> Service ready

                Case            "220"                   <domain> Service ready
                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   <domain> closing channel

                Case            "221"                   <domain> closing channel

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   <domain> service not available

                Case            "421"                   <domain> service not available

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Request mail action OK

                Case            "250"                   Request mail action OK

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   User not local, will forward to <fwrd path)

                Case            "251"                   User not local, will forward to <fwrd path)

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Request mail action not taken,Mailbox busy

                Case            "450"                   Request mail action not taken,Mailbox busy

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Request mail action not taken,Mailbox not found

                Case            "550"                   Request mail action not taken,Mailbox not found

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Requested action aborted, error in processing

                Case            "451"                   Requested action aborted, error in processing

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   User not local, Please try < forward path>

                Case            "551"                   User not local, Please try < forward path>

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
..............................................................................
.   Requested action not taken,Insufficient system storage

                Case            "452"                   Requested action not taken,Insufficient system storage

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Requested mail action aborted, exceed storage allocation

                Case            "552"                   Requested mail action aborted, exceed storage allocation

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Requested action not taken, mailbox name not allowed

                Case            "553"                   Requested action not taken, mailbox name not allowed

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Start mail input, end with CRLF

                Case            "354"                   Start mail input, end with CRLF

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
.   Transaction failed

                Case            "554"                   Transaction failed

                IF              ( SmtpRecvCode != SmtpVeriCode ) Code Recv'ed verification
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                Else
                 Setflag        Equal                   Recv'd code IS expected code
                Endif
                RETURN
...............................................................................
                Default                                 Unknown return code received, ignore
                 Move           "2",SmtpResult          SmtpResult points to error code in SmtpStatus
                 Setflag        NOT Equal               Recv'd code is NOT expected code
                 RETURN
                Endswitch
.------------------------------------------------------------------------------
.   Text section header information
...............................................................................
.   This function is not currently implemented
SmtpTextOrigin                           

..                Pack    SocketTxBuffer,"X-SMTP-Posting-Origin:":     Key
..                Call    SocketWrite                     Transmit command
                  RETURN

...............................................................................
.   The Message-ID MUST be a world unique id. This is used by Mail servers/routers
.   to assemble mail that has been transmitted partly. Besides the number
.   that has to be unique there is nothing else about it you need to worry about

SmtpTextMessageID                      
                Clock   Version,SmtpVersion             Get Plb Version(17)
                Pack    SocketTxBuffer,"Message-ID:":   Key
                                       "<":
                                       SmtpSysDate:     Clocked PLB SysDate
                                       " ":
                                       SmtpVersion:     Clocked PLB version
                                       ">":
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
                
...............................................................................
.   This 'From' would be placed in the received email 'From' textbox

SmtpTextFrom                         
                Pack    SocketTxBuffer,"From:":         Key
                                       SmtpQuote:       "
                                       SmtpUserFullName:    User Full name
                                       SmtpQuote:       "
                                       "<":             <
                                       SmtpEmailAddress: User email address
                                       ">":
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   This 'To' would be placed in the received email 'To' textbox
                     
SmtpTextTo
                If      ( SmtpDestIndexLast = 1 )       Only one destination?
                 Pack    SocketTxBuffer,"To:":          Key
                                        SmtpQuote:      "
                                        SmtpDestinations(1,2):  Destination email address
                                        SmtpQuote:      "
                                        "<":            <
                                        SmtpDestinations(1,1):  Destination email address
                                        ">":             >
                                        SmtpCRLF        End of line
                Else
                clear   taskname
                for     result,"1",(SmtpLoopDest - 1)
                        append  SmtpDestinations(result,1),taskname
                        if (result < (SmtpLoopDest - 1))
                                append  ";",taskname
                        endif
                repeat
                reset   taskname
                 Pack    SocketTxBuffer,"To:":          Key
                                        taskname:  Destination email address
                                        SmtpCRLF        End of line
                Endif
                Call    SocketWrite                     Transmit command
                RETURN
.SmtpTextTo
.                If      ( SmtpDestIndexLast = 1 )       Only one destination?
.                 Pack    SocketTxBuffer,"To:":          Key
.                                        SmtpQuote:      "
.                                        SmtpDestinations(1,2):  Destination email address
.                                        SmtpQuote:      "
.                                        "<":            <
.                                        SmtpDestinations(1,1):  Destination email address
.                                        ">":             >
.                                        SmtpCRLF        End of line
.                Else
.                 Pack    SocketTxBuffer,"To:":          Key
.                                        " ":            Mailer server must insert this
.                                        SmtpCRLF        End of line
.                Endif
.                Call    SocketWrite                     Transmit command
.                RETURN
...............................................................................
.   This 'Subject' would be placed in the received email 'Subject' textbox

SmtpTextSubject                         
                Pack    SocketTxBuffer,"Subject:":      Key
                                       SmtpSubject:     The email subject
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Date in email - Note that the PL/B manual is incorrect on the SYSDATE format
.   The date could be inserted but the Mail server will change it anyway
SmtpTextDate
                Clock   SysDate,SmtpClockDate
                Unpack  SmtpClockDate,SmtpClockWeekDay:
                                      SmtpD1:
                                      SmtpClockMonth:
                                      SmtpD1:
                                      SmtpClockDay:
                                      SmtpD1:
                                      SmtpClockTime:
                                      SmtpD1:
                                      SmtpClockYear:
                                      SmtpD1:
                                      SmtpClockRunVersion:
                                      SmtpD1:
                                      SmtpClockRuntime:
                                      SmtpD1:
                                      SmtpClockUnknown

                Pack    SocketTxBuffer,"Date:":
                                       " ":             The mail server would insert the date
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Set the MIME encoding version, always 1
SmtpTextMIME                         

                Pack    SocketTxBuffer,"MIME-Version:":      Key
                                       " 1.0":          Mime version always 1.0
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   We either send a standalone text email or a text email with attachments
.   If the attachments last entry index is zero then there are NO attachments
.   If index is more than zero then we have attachments and must the email
.   reader know of it in advance. This is done with this routine
.   Attachments are sepearted from each other with a pre-defined boundary
.   string that has to be world unique. We use our Message ID for that purpose
.   and add to that "--Next Part--".
SmtpTextContent                      
                If      ( SmtpAttIndexLast = 0 )        If no attachements the plain text only
                 Call   SmtpPlainContent                Only plain contents in email
                Else
                 Call   SmtpBase64Content               Plain contents and Base 64 contents
                Endif
                RETURN
...............................................................................
.   Plain text 7 bit and Base 64 contents in this email
SmtpBase64Content
                Pack   SocketTxBuffer,"Content-Type:": Key
                                      "multipart/mixed;": Attachments follow
                                      " boundary=":    Attachment seperator key
                                      SmtpQuote:       "
                                      "Next_Part__":   '_-' Makes this not possible in base64
                                      SmtpSysDate:     Clocked PLB SysDate
                                      " ":
                                      SmtpVersion:     Clocked PLB version
                                      SmtpQuote:       "
                                      SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Plain contents - Only ascii text file with no attachements
SmtpPlainContent
                Pack   SocketTxBuffer,"Content-Type:": Key
                                      "text/plain;":   Only a plain text message
                                      " charset=":     Key
                                      SmtpQuote:       "
                                      "us-ascii":      Charset used
                                      SmtpQuote:       "
                                      SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   The email receiver needs to know if the text received is 7/8bit or base64
.   encoded characters. 
SmtpTextTransfer8                       
                Pack    SocketTxBuffer,"Content-Transfer-Encoding:": Key
                                       " 8bit":         8 bit encoding
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN

...............................................................................
.   The email receiver needs to know if the text received is 7/8bit or base64
.   encoded characters. 
SmtpTextTransfer7                         
                Pack    SocketTxBuffer,"Content-Transfer-Encoding:": Key
                                       " 7bit":         7 bit encoding
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN

...............................................................................
.   The email receiver needs to know if the text received is 7/8bit or base64
.   encoded characters. 
SmtpTextTransfer64                           
                Pack    SocketTxBuffer,"Content-Transfer-Encoding:": Key
                                       " base64":       MimeBase64
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN

...............................................................................
.   eMail priority - always 3
SmtpTextPriority                          
                Pack    SocketTxBuffer,"X-Priority:":   Key
                                       " 3":            Priority
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Only Microsoft knows what this is about
SmtpTextMsMail                        
                Pack    SocketTxBuffer,"X-MSMAIL-Priority:":  Key
                                       " Normal":       Normal
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   The program that does the mailing,this program
SmtpTextMailer                        
                Pack    SocketTxBuffer,"X-Mailer:":     Key
                                       "PL/B-Programming Language for Bussiness": 
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   This program producers name - Please don't change this contents as this
.   is my work and I deserve the credit for it. Thanks - Festus
.   This will not appear on any screen or email except in the source code of the email
SmtpTextOle
                Pack    SocketTxBuffer,"X-MimeOLE:":    Key
                        "Produced by PlbSA Smtp mailer : plbsa@icon.co.za":
                        SmtpCRLF                        End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Send Attachment boundary - Prefix
.   All boundary identifiers send are prefixed with '--'

SmtpTextNextPart
                Call    SmtpCRLF                       There must be a space inbetween
                Return  If Not Equal                   Must be a comm error
                Clock    Version,SmtpVersion           Get Plb Version(17)
                Pack    SocketTxBuffer,"--":
                                       "Next_Part__":  '_' Makes this not possible in base64
                                       SmtpSysDate:    Clocked PLB SysDate
                                       " ":
                                       SmtpVersion:    Clocked PLB version
                                       SmtpCRLF        End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Send Attachment boundary - Suffix
.   When the last boundary identifier is send is the prefix and suffix '--' added
.   This is the case after all attachments have been encoded

SmtpTextLastPart
                Call    SmtpCRLF                       There must be a space inbetween
                Return  If Not Equal                   Must be a comm error
                Pack    SocketTxBuffer,"--":
                                       "Next_Part__":  '_' Makes this not possible in base64
                                       SmtpSysDate:    Clocked PLB SysDate
                                       " ":
                                       SmtpVersion:    Clocked PLB version
                                       "--":           Last part indicator
                                       SmtpCRLF        End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   Transmit lines of encoded base64 data
.   The Tx buffer MUST be preLoaded before a call is made here
.   Reading a pre-encoded mime base 64 file for transmission an Tx the data
SmtpTextTxEncoded                        
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   The email receiver must know that it is an attachment and the file name
.   of the attachment once decoded
SmtpTextDispo                           
                Pack   SocketTxBuffer,"Content-Disposition:": Key
                                       " attachment;"   Only a plain text message
                Call    SocketWrite                     Transmit command
                Return  If not Equal                    Respond to comm's errors
                Pack    SocketTxBuffer,"  filename=":    Key
                                       SmtpQuote:       "                                       
                                       SmtpAttachments(SmtpLoopAttach,1): FileName
                                       SmtpQuote:       "
                                       SmtpCRLF:        End of line
                                       SmtpCRLF         Space before base 64 coding                             
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   The email receiver must know that it is an attachment and the file name
.   of the attachment once decoded
SmtpTextContentApp                        
                Pack   SocketTxBuffer,"Content-Type:": Key
                                       "application/octet-stream;" 
                Call   SocketWrite                      Transmit command
                Return If not Equal                     Respond to comm's errors
                Pack   SocketTxBuffer,"        filename=":    Key
                                       SmtpQuote:       "                                       
                                       SmtpAttachments(SmtpLoopAttach,1): FileName
                                       SmtpQuote:       "
                                       SmtpCRLF         End of line
                Call    SocketWrite                     Transmit command
                RETURN
...............................................................................
.   This routine reads an input file in absolute code, encodes it into
.   Mime Base 64 code, and transmits the encoded file to Smtp server.
.   Three bytes of absolute code is read from the input file, encoded to
.   base 64 code, packed into the send buffer to a size of 76 bytes and
.   the buffers are transmitted until end of the input file.
.   About Mime Base 64 encoding:
.   For messages to be send via a routable protocol such as TCP/IP must the
.   data being send be 7 bit characters. Base 64 encoding is the standard
.   encoding for 8 bit files to 7 bits. This encoding results in 4 bytes of
.   code being generated of every 3 bytes of data input. 3 Bytes of 8 bits
.   are packed into a 24 bit string and then unpacked into 4 bytes of 6 bits
.   each. A Base64 translation table encodes the 6 bits into a valid 7 bit
.   ascii code which is could then be transmitted via TCP/IP.
.   All 8 bit file sizes that are not in multiples of 3 bytes which results in
.   cases where there are bytes short to fill up the last 4 bytes. These bytes
.   are padded with "="'s in the output buffer.
.   One note about EOF.
.   As far as the Windows operating system is concerned is there NO EOF mark
.   within a Windows file. The Windows Operating system uses the FAT to see if
.   EOF has been reached. Now this is confusing because some files has got an
.   EOF mark( 0D0A -Dos ) in it and some editors do put one in. This is still
.   a carry over from old operating systems. You will find that the DOS EDIT.EXE
.   still inserts a EOF mark. For the aforesaid reason do we not use OVER to
.   find the EOF mark but we rather use the file size and consider everything
.   in it to be data, even if it might include an EOF mark. 
.   When a file is coded with Base64 and decoded back into its original shape
.   must the original file and (the base64 encoded and again decoded) file be
.   exactly the same in contents and in byte lenght. Any but any variation from
.   that is not a true base64 encoding.

SmtpB64Table            Init            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz":
                                        "0123456789+/"  Base 64 Table          
SmtpTrip                Integer         4,"0"           3 Bytes read are packed in
SmtpQuad                Init            "    "          Output from Base64 encoding
SmtpB64Index            Integer         1,"0"           Index into Base64 table
SmtpB64IndexD           Dim             1               Index into Base64 table-DIM for AND
SmtpEOF                 Form            "0"             0=EOF not reached, 1=EOF reached
SmtpCounter             Form            "00"            General loop counter
SmtpBack                Form            "-4"            Read file seq backward
SmtpInputFile           File                            .Input file
SmtpByte                Init            " "             ABSON byte read from file
SmtpOutputBufferSize    Form            2               Size of output buffer
SmtpI1                  Integer         1,"0"           Work Space

.   Variables used for FindFirstFile WinApi

SmtpEncode              Dim             260             Encode file path/name for file size( zero terminated)
SmtpFileData            Dim             318             FILEDATA data structure
SmtpFileAttributes      Integer         4               File attributes
SmtpCreationTime        Dim             8               STRUCTURE - Creation time
SmtpLastAccessTime      Dim             8               STRUCTURE - Last time file was accessed
SmtpLastWriteTime       Dim             8               STRUCTURE - Last time written to file
SmtpFileSizeHigh        Integer         4               High order of file size ( 64 bits )
SmtpFileSizeLow         Integer         4               Low order of file size ( 64 bits )
SmtpReserved1           Integer         4               Reserved
SmtpReserved2           Integer         4               Reserved
SmtpFileName            Dim             260             Path and File name
SmtpAlternateFileName   Dim             14              Alternate file name ( DOS file name)
SmtpFindFirstFile       Profile         kernel32.Dll:   Dll library
                                        FindFirstFileA: Entry Point
                                        Int4:           Type return
                                        Dim:            File name ZT
                                        Dim             File Data structure data
.SmtpFindFirstFileW      Profile         kernel32.Dll:   Dll library
.                                        FindFirstFileW: Entry Point
.                                        Int4:           Type return
.                                        Dim:            File name ZT
.                                        Dim             File Data structure data
SmtpFileSize            Form            10              File to decode input file size(Max 2GB)
SmtpApiResult           Integer         4,"0"           Result of WinApi
SmtpFindClose           Profile         kernel32.Dll:   Dll library
                                        FindClose:      Entry Point
                                        Int4:            Type return
                                        Int4
.SmtpFindCloseW          Profile         kernel32.Dll:   Dll library
.                                        FindCloseW:     Entry Point
.                                        Int4            Type return
SmtpFileHandle          Integer         4,"0"           File handle for FindFirstFile
SmtpHexZero             Integer         4,"0"           Integer zero
SmtpTcount              Form            "0"             Triplet counter

...............................................................................

SmtpEncodeAndSend                          

.   Because we read the input file in *ABSON mode and can we NOT
.   use the OVER flag to test for EOF. We use a WinApi to determin the size
.   of the file and then read so many bytes. Only the lower byte of the fileSize
.   is used,32 bits = 4 GB ( You won't email a file that size anyway)

                Pack            SmtpEncodeFileName,SmtpAttachments(SmtpLoopAttach,2):   Build file name PLB read
                                "\":
                                SmtpAttachments(SmtpLoopAttach,1)
                Pack            SmtpEncode,SmtpEncodeFileName,SmtpInitZero              Build file name API read
.dave runs amuck
                path            exist,"c:\windows"
                if              not over
                WinApi          SmtpFindFirstFile Giving SmtpApiResult Using SmtpEncode:
                                SmtpFileData                                            File Data structure data
                Goto            SmtpApiBadAttachment If ( SmtpApiResult = SmtpHexZero ) File not found??
                Move            SmtpApiResult,SmtpFileHandle                            Get file handle    
                WinApi          SmtpFindClose Giving SmtpApiResult Using SmtpFileHandle Close file handle
                Goto            SmtpApiBadAttachment If ( SmtpApiResult = SmtpHexZero ) Memory leak if error
                else             .presume NT
.                WinApi          SmtpFindFirstFileW Giving SmtpApiResult Using SmtpEncode:
.                                SmtpFileData                                            File Data structure data
                WinApi          SmtpFindFirstFile Giving SmtpApiResult Using SmtpEncode:
                                SmtpFileData                                            File Data structure data
                Goto            SmtpApiBadAttachment If ( SmtpApiResult = SmtpHexZero ) File not found??
                Move            SmtpApiResult,SmtpFileHandle                            Get file handle    
                WinApi          SmtpFindClose Giving SmtpApiResult Using SmtpFileHandle Close file handle
                Goto            SmtpApiBadAttachment If ( SmtpApiResult = SmtpHexZero ) Memory leak if error
                endif
                Unpack          SmtpFileData,SmtpFileAttributes:   File attributes
                                             SmtpCreationTime:     STRUCTURE - Creation time
                                             SmtpLastAccessTime:   STRUCTURE - Last time file was accessed
                                             SmtpLastWriteTime:    STRUCTURE - Last time written to file
                                             SmtpFileSizeHigh:     High order of file size ( 64 bits )
                                             SmtpFileSizeLow:      Low order of file size ( 64 bits )
                                             SmtpReserved1:        Reserved
                                             SmtpReserved2:        Reserved
                                             SmtpFileName:         Path and File name
                                             SmtpAlternateFileName Alternate file name ( DOS file name)
                Move            SmtpFileSizeLow,SmtpFileSize       Get the file size

.   Open the input file for processing

          Trap          SmtpBadAttachment if IO         Incase file not found
          Open          SmtpEncodeFile,SmtpEncodeFileName  Open our temp encoded file
          TrapClr       IO                              Not needed anymore

.   Beginning of main loop

           If          ( SmtpProgress = 1 )            Progress bar display enabled?
            Move       SmtpFileSize,ProBarHiRange      Get upper range of Progress bar
           Endif

           Move        "0",SmtpEOF                      We have not reached EOF yet
           Clear       SocketTxBuffer                   Prepare for first buffer output
           LOOP                                         MAIN ENCODE AND TRANSMIT LOOP
           If          ( SmtpProgress = 1 )             Progress bar display enabled?
            Pack       ProBarText,"Sending attachment ",SmtpLoopAttach," of ",SmtpAttIndexLast
            Sub        SmtpFileSize,ProBarHiRange,ProBarCurValue   Calc neg to positive
            Call       ProBarUpdate                     Display and update Progress Bar
           Endif
           UNTIL       ( SmtpEOF = 1 )                  Did we have an EOF?            

.   Read input file in single bytes. Because we read with *ABSON is it our
.   responsibility to look out for EOF sequence ( which we do with file size checking )

             Move      "0",SmtpTcount                   Reset loop counter
             Move      "0",SmtpTrip                     Reset work integer
             Loop
             Until     ( SmtpTcount = 3 )               Have we already got 3 bytes
              If       ( SmtpFileSize > 0 )             Do all of file
               Read    SmtpEncodeFile,SmtpSeq;*ABSON,SmtpByte;  Read and hold file pointer
               Sub     "1",SmtpFileSize                 Decrement bytes read
               Add      "1",SmtpTcount                  Next byte of 3 bytes done
               Mult    "256",SmtpTrip                   Shift left 8 bits
               Move    SmtpByte,SmtpI1                  Format integer
               OR      SmtpI1,SmtpTrip                  Add to output
              Else
               Move    "1",SmtpEOF                      Remember we reached EOF
               Switch  SmtpTcount                       What happens to last bytes
               Case    "1"                              1 done,2 bytes short
                       Mult "256",SmtpTrip              Shift left 8 bits,Zero fill from right
                       Mult "256",SmtpTrip              Shift left 8 bits,Zero fill from right
               Case    "2"                              2 done,1 byte short
                       Mult "256",SmtpTrip              Shift left 8 bits,Zero fill from right
               Endswitch
               BREAK                                    Exit the loop now
              Endif
             REPEAT                                     For 3 bytes
             
             Mult      "64",SmtpTrip                    BIAS-Shift left 6 bits

.   Convert to base 64
.   When we shift out 6 bits we do it right-wise therefore pack output bytes
.   reverse-wise.

             Move     "5",SmtpCounter                   Reset loop counter for downcount
             Move     "    ",SmtpQuad                   Prepare output
             Loop
              Sub     "1",SmtpCounter                   Point to next 6 bits
             While    ( SmtpCounter > 0 )
              DIV     "64",SmtpTrip                     Shift six bits into Low Byte of Low Word
              Move    SmtpTrip,SmtpB64IndexD            Get into maskoff work space
              AND     SmtpH3F,SmtpB64IndexD             Mask of 6 bits
              Move    SmtpB64IndexD,SmtpB64Index        Format integer
              Add     "1",SmtpB64Index                  Bias for 0 = A(Fp=1)
              Reset   SmtpB64Table,SmtpB64Index         Point to B64 Ascii value
              Reset   SmtpQuad,SmtpCounter              Pack in correct order
              Cmove   SmtpB64Table,SmtpQuad             Get the B64 byte  
             REPEAT
             Reset    SmtpQuad,1                        Fix up FP

.   If we were short on bytes then pad them now.Note that in the event where
.   we are short one or two bytes would they be filled with "A" or "AA" at
.   this stage (0x00 = A). We need to replace them with "=" or "==". These
.   bytes are at possitions 3 and 4 in SmtpQuad

             If       ( SmtpEOF = 1 & SmtpTcount = 1 )  Are we 2 bytes short?
              SetLPtr SmtpQuad                          .Physical length
              Reset   SmtpQuad,3                        Point to byte 3
              CMove   "=",SmtpQuad                      Replace with "="
              Reset   SmtpQuad,4                        Point to byte 4
              CMove   "=",SmtpQuad                      Replace with "="
              Reset   SmtpQuad,1                        Fix up FP
             Else IF  ( SmtpEOF = 1 & SmtpTcount = 2 )  Are we one byte short?
              SetLPtr SmtpQuad                          .Physical length
              Reset   SmtpQuad,4                        Point to byte 4
              Cmove   "=",SmtpQuad                      Replace with "="
              Reset   SmtpQuad,1                        Fix up FP
             Endif

.   Pack the output buffer with 76 bytes data.(Mime specification)
.   SmtpTcount can only be zero when we have just completed 3 input bytes
.   and on return to the top of the loop we found there are no more bytes(EOF reached).
.   When we come down because of it then it is not a valid SmtpQuad and must
.   it be ignored.

             If      ( SmtpTcount != 0 )                EOF before next 3 bytes started?
              Append SmtpQuad,SocketTxBuffer            Add to buffer
              Add    "4",SmtpOutputBufferSize           Increment
             Endif
             If      ( SmtpOutputBufferSize > 72 | SmtpEOF = 1 ) Buffer full?
              Append SmtpCRLF,SocketTxBuffer            CRLF end of buffer
              Reset  SocketTxBuffer                     We have a full buffer
              Call   SocketWrite                        Transmit command
              Return If not equal                       Respond to comms errors
              Move   "0",SmtpOutputBufferSize           Reset our output buffer
              Clear  SocketTxBuffer                     Prepare for next buffer output
             Endif

.   End of main loop
.   If we had an EOF then there is no more bytes to be done.

           REPEAT

           Setflag   Equal                               Return with GOOD flag
           RETURN

.------------------------------------------------------------------------------
SmtpBadAttachment
           NORETURN
           TrapClr   IO                                 Not needed anymore
SmtpApiBadAttachment
           Move      "013",SmtpResult                   Set the error code
           Call      SmtpErrorText                      Set message
           Move      "Not Found",SmtpAttachments(SmtpLoopAttach,3)  Set status
           Setflag   Not Equal
           RETURN                                       ABORT with flag

.------------------------------------------------------------------------------
.   This routine converts the Smtp error code to a human readable text string
.   and returns it to the caller

SmtpErrorText                                      
        Move    SmtpResult,SmtpD3                           Get the code
        Replace " 0",SmtpD3                                 Make sure in right format
        Pack    SmtpScanCode,SmtpD3," - "                   Build a unique scan code
        SetLPtr SmtpErrorList                               .Make sure we use full code list
        Reset   SmtpErrorList                               .From the beginning
        Scan    SmtpScanCode,SmtpErrorList                  Find the error code in out list
        If      Not EOS                                     We found it!
         Bump   SmtpErrorList,6                             Point to start of text of code
         Move   SmtpErrorList,SmtpResultText                Get the text message
         SetLPtr SmtpResultText,74                          Cut of extra garbage
        Else                                                We did NOT find it
         Pack   SmtpStatusText,"Unknown Error Code = ",SmtpScanCode We don't have a text for this code
        Endif
        RETURN                                              With a text message

.------------------------------------------------------------------------------
SmtpJump
.                        END OF SMTP.RTN
.------------------------------------------------------------------------------

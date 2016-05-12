PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         Include Smtp.PRI  (PRI - Profile Routine Interface)

release  init      "1.7"            DMB 02SEP2003	Added Text to Vars to check
;release  init      "1.6"            DMB 29Aug2003  Increased length of subjectline
;release  init      "1.5"           DLH 17Jan20001 If user = "" user Information Services as sender
.release  init      "1.4"           DLH 07Jan20001 Allow Destination addresses
.release  init      "1.3"           DLH 06Jan20001 Allow multiple file attachments
.release  init      "1.2"           ASH 03OCT20000 NEW SERVER ADDED
.release  init      "1.1"           DLH 12Sep00  add NSTMP.dat file & info
.release  init      "1.0"           DLH 28Sep99
.begin release 1.1
info     file
path    DIM        45                   use Path name ie \\nts0\c\data\text
;patch1.6
Subject  dim      150
;Subject  dim      30
;patch1.6
.begin patch 1.3
.record   dim      150
record   dim      375
Files    dim      350
Recipients dim     350
.end release 1.1
.end patch 1.3
;patch1.7
BODY	DIM	250
;patch1.7

BEGIN   
.	CMATCH    B1 TO PROGRAM             .CHAINED FROM DSINIT?
.         IF        EOS                       .NO
.         stop
.         else
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "LOCAL" TO PRTNAME
.         ENDIF
         MOVE      "SMTP Mailer" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
.begin release 1.1
.	Jump to test
	goto	test	
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
.begin patch 1.3
         scan      "$FILES=" in record
         if        equal
         bump      record by 7
         move      record to Files
         goto      getinfo
         endif
         reset     record
.end patch 1.3
.begin patch 1.4
         scan      "$SENDTO=" in record
         if        equal
         bump      record by 8
         move      record to Recipients
         goto      getinfo
         endif
         reset     record
.end patch 1.4
.begin patch 1.7
         scan      "$BODY=" in record
         if        equal
         bump      record by 6
         move      record to BODY
         goto      getinfo
         endif
         reset     record
.end patch 1.7
         goto      getinfo
.these currently are the only three variables checked for just read until done
infodone close     info
.begin patch 1.3
         if        (files = "" & inpname <> "")
         move      inpname to files
         endif
.end patch 1.3
.begin patch 1.4
         if        (Recipients = "" & Comment > "")
         move      Comment to Recipients
         endif
.end patch 1.3
.................................................................
Subject  CMATCH    B1 TO SUBJECT
         IF        EOS
         MOVE      "Here is your PDF file" to subject
         endif
         CMATCH    B1 TO path
         IF        EOS
.START PATCH 1.2 REPLACED LOGIC
.         MOVE      "g:\data" to path
         MOVE      NTWKPATH1 to path
.END PATCH 1.2 REPLACED LOGIC
         endif
.end release 1.1
Test
        Move       Subject to SmtpSubject Subject
        move	"nord0917.pdf",files
        move	"[FACSYS: Jose @+1 415433-7796]",recipients
        move	"dherric",user
        move	"c:\work\pdf\",path
	MOVe	"Fax Test" to SmtpSubject Subject
.   Set the text message that is send with the attachments

.       Move    inpname,SmtpTextMessage(1)   Array <Text message >
       Move    files,SmtpTextMessage(1)   Array <Text message >

        Move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
         clear     taskname
         Move    "NTS4.nincal.com",SmtpEmailServer                   Address of email serverc
         clear   smtpemailaddress
.begin patch 1.4
        MOVELPTR Recipients,n6
        move    c1 to n4
        move    c1 to n3                    .number of recipients
        if      (user > "" & user <> " ")
        append  user to SMTPEmailAddress                            .sender info
        append  "@nincal.com",SMTPEmailAddress                            .sender info
        reset    SMTPEmailaddress                            .sender info
        Move    user,SmtpUserName                             .sender info
        Move    user,SmtpUserFullName
        else
        append  "InformationServices" to SMTPEmailAddress                            .sender info
        append  "@nincal.com",SMTPEmailAddress                            .sender info
        reset    SMTPEmailaddress                            .sender info
        Move    "Information Services",SmtpUserName                             .sender info
        Move    "Information Services",SmtpUserFullName
        endif
        scan    ";" in Recipients
        if      not equal                   .no delimiter preume a single destination
		reset   Recipients
.         scan    "@" in comment
		scan    "@" in Recipients
		if      equal                .presume non NINCAL address and it is in the COmment field
.	                reset   comment
.	                pack    SMTPEMAILADDRESS from comment
.	                pack    SMTPUserName from comment
			reset   recipients
			pack    SMTPEMAILADDRESS from recipients
			pack    SMTPUserName from recipients
		else                                .then let us pray that in is NINCAL.com
			Move    user,SmtpUserName                                User name
			append  user to SmtpEmailAddress
			append  "@nincal.com",SmtpEmailAddress
			reset    smtpemailaddress
		endif

		MOVE    smtpemailaddress to SmtpDestinations(1,1)
		MOVE    user,SmtpDestinations(1,2)                          originators UserName
		Move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
         else                         .We belv we have multiple destinations
.        Move    usere,SmtpUserFullName              User Full Name
moreDest
        MOVEFPTR Recipients,n5
        bump     Recipients,-1
        lenset   Recipients
        reset    Recipients,n4
        MOVE     recipients to SmtpDestinations(n3,1)
.        scan     "@",recipients
.        if equal
.                 movefptr recipients,N7
.                 sub    C1,N7
.                 reset  recipients,N4
.                 setlptr recipients,N7
.        endif
        MOVE     recipients to SmtpDestinations(n3,2)
        Move     N3,SmtpDestIndexLast                               Index to last entry in Dest array
        ADD      c1 to n5
        move     n5 to n4
        reset    Recipients,n6
        reset    Recipients,n5
check   scan    ";" in Recipients
                if      equal
                add     c1 to n3
                goto    moreDest           .else this is the last one
                else
                add     c1 to n3
                reset   Recipients,n6
                reset   Recipients,n5
                MOVE     recipients to SmtpDestinations(n3,1)
                MOVE     recipients to SmtpDestinations(n3,2)
                Move    N3,SmtpDestIndexLast                               Index to last entry in Dest array
                endif
.   Set the destinations of the email. Max 100 (Mime spec)

.        MOVE    smtpemailaddress to SmtpDestinations(1,1)
.        MOVE     user,SmtpDestinations(1,2)                          originators UserName
.        Move    "1",SmtpDestIndexLast                               Index to last entry in Dest array
        endif
.end patch 1.4


.begin patch 1.3
.intend to change this to allow multiple files, DLH
.        if        (inpname = "" or inpname = " ")
        if        (files = "" or files = " ")
.        cmatch    b1 to inpname
        Move    "0",SmtpAttIndexLast                                Index to NO entry
;patch1.7
	move	BODY to SmtpTextMessage(1)
	Move	"1",SmtpTextIndexLast	Index to how many lines<ARRAY> in body
;patch1.7
        else
.dave goes mad
        MOVELPTR Files,n6
        move    c1 to n4
        move    c1 to n3                    .number of files
        scan    ";" in Files
        if      not equal
        reset   Files
        Move    Files,SmtpAttachments(1,1)                        Attached file name
        Move    path,SmtpAttachments(1,2)           Path to attached file name
        Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
        else
morefiles
        MOVEFPTR Files,n5
        bump     Files,-1
        lenset   Files
        reset    Files,n4
        Move    Files,SmtpAttachments(n3,1)                        Attached file name
        Move    path,SmtpAttachments(n3,2)           Path to attached file name
        Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
        ADD     c1 to n5
        move    n5 to n4
.?        reset   Files,25
        reset   Files,n6
        reset   Files,n5
checkit scan    ";" in Files
                if      equal
                add     c1 to n3
                goto    morefiles           .else this is the last one
                else
                add     c1 to n3
                reset   Files,n6
                reset   Files,n5
                Move    Files,SmtpAttachments(n3,1)                        Attached file name
                Move    path,SmtpAttachments(n3,2)           Path to attached file name
                Move    n3,SmtpAttIndexLast                 Index to last entry - Only 1 entry
                endif
        endif
.end patch 1.3

        endif
drew
.   For debugging
.   "Dot" out the line 'Clear ..." if you want to enable the LogFile

.        Move    "0",SmtpDispMode                                    Display log file
        Move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
        Clear   SmtpLogFile                                         'Clear' disables the LogFile
.   Progress bar. It is currently enabled so you can have an indication of what
.   goes on.If this lines of code is removed would the default be 0

        Move    "1",SmtpProgress                                    Enable progress bars

.   Send the above eMail to the destination which happens to be myself
.   If any errors detected the error code and human readable text is given

        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )

        If      NOT Equal
         Pack   Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                     "Status Code ",SmtpStatus," - ",SmtpStatusText
        Move    "PDF File not found",SmtpSubject Subject
        Move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
        Call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.         Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
        Else
.         Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
        Endif
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


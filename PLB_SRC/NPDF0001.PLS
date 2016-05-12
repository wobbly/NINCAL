PC EQU 0
          include common.inc
                    include cons.inc

;Note title(minus ".lst") and spoolname must be identical
release   init "1.0"
spoolfile dim   30
NUSER     dim   10
fstrlen   form  3
lstrlen   form  3
PDF       init  ".pdf"
;                    move "DBACA" to user
;         move "DBACA2003031315273097" to comment
;                    alert caution,comment,result
                    pack spoolfile,comment,pdf
                    move user to nuser
test
;                    scan period in spoolfile
;                    movelptr spoolfile,lstrlen 
;                    movefptr spoolfile,fstrlen 
;                    sub  c1 from fstrlen
;                    reset spoolfile 
;                    setlptr spoolfile,fstrlen
                    pack str45,ntwkpath1,comment,".lst"                 ;must be listfile in \\nins1\e\data
.Patch1.1
                    PRTPLAY str45,"PDF995"
.                    PRTPLAY str45,"Acrobat Distiller"
.Patch1.1
                                                              pause c5
                    move    "Here is your PDF File",SmtpSubject Subject
;.   Set the text message that is send with the attachments
                    move    str55,SmtpTextMessage(1)   Array <Text message >
                    move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
                    move    "Ninmail",SmtpEmailServer                   Address of email serverc
                    clear   smtpemailaddress
                    append  nuser,SmtpEmailAddress
                    append  "@nincal.com",SmtpEmailAddress
                    reset   smtpemailaddress
                    move    nuser,SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
                    move    smtpemailaddress,SmtpDestinations(1,1)
                    move    NUSER,SmtpDestinations(1,2)
                    move    "1",SmtpDestIndexLast                          originators UserName
                    move    spoolfile,SmtpAttachments(1,1)                     Attached file name
.Patch1.1
                    move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.                    move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.Patch1.1
                    move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                    clear   SmtpLogFile                                         'Clear' disables the LogFile
                    move    "1",SmtpProgress                                    Enable progress bars
                    call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                    if not equal
                            pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                                "Status Code ",SmtpStatus," - ",SmtpStatusText
                            move    "PDF File not found",SmtpSubject Subject
                            move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                            call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                    else 
;                            erase spoolfile
                    endif
                    shutdown  "cls"
                    stop
                    include comlogic.inc
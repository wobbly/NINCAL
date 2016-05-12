........................................
. Program:      NORD0042.PLS
. Function:     Locked Record Alert Program
. Author:       Andrew Harkins
. Orig. Date:   October 16,2000
. Release:      1.0
. Notes:        This program reads File1Path and finds any files
.               representing records locked open.  It then reads each file
.               and sends a message out announcing the locked record.
........................................

PC      EQU     0
.Include Files
        include common.inc
        include cons.inc
        include nusedd.inc
        include winapi.inc

release 	init    "1.2"  DLH  SendMail
reldate	Init	"24 April 2008"
.release init    "1.1"  DLH 12Jul2002    Use GetWinVer
;release init    "1.0"  ASH 16OCT2000    New Release

.Files
file1   file    		.File1Path + file1nme
file1nme init   "open.dat"
file2   file
.
File1Path dim   200
.hexeight integer 4,"4294967295"

.Establish File1Path
        pack    File1Path,NTWKPATH4,"open\"			."
.Delete any existing instances of Directory File
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,File1Path,file1nme,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
.Create new one
        clear   taskname
;begin patch 1.1
                    call                GetWinVer
;        Path    Exist,"c:\windows"
;        if      over
                    If                  (osflag = c1 | osflag = c5)
                append  "!c:\winnt\system32\cmd.exe",taskname
;        else
                ElseIf                  (osflag = c3 | osflag = c4)
                append  "!c:\command.com",taskname
                ElseIf                  (osflag = c6)
                append  "!c:\windows\system32\cmd.exe",taskname
;end patch 1.1
        endif
        append  " /c dir ",taskname
        append  File1Path,taskname
        append  " /B > ",taskname
        append  File1Path,taskname
        append  file1nme,taskname
        reset   taskname
        execute taskname
.Open and read Directory File
        pack    str55,File1Path,file1nme
        open    file1,str55
        loop
LoopStart
                move    C0,N1
                read    file1,seq;str2,str6,str1,str3
                until over
                if (str2 = "lr")
                        type    str6
                        if equal        Modified LR
                                move    C1,N1
                        else
                                scan    PERIOD,str6
                                if equal        New Record
                                        move    C2,N1
                                endif
                                reset   str6
                        endif
                endif
                if (N1 > C0)    Valid file was discovered
                        pack    str25,str2,str6,str1,str3
                        call    Trim using str25
                        pack    str55,File1Path,str25
                        move    "                                        ",APIFileName
                        clear   APIFileName
                        pack    APIFileName,str55,hexzero
                        call    FindFirstFile
                        if (APIResult <> 0 & APIResult <> hexeight)
                                trap    FileIOError if IO
                                open    file2,str55
                                read    file2,seq;NUSEFLD                 .PORTN
                                read    file2,seq;str15                 .LR or NEW RECORD
                                read    file2,seq;str4,str2,str3        .DATE
                                trapclr IO
                                clear   str11
                                append  str2,str11
                                move    str3,str2
                                append  SLASH,str11
                                append  str2,str11
                                append  SLASH,str11
                                append  str4,str11
                                reset   str11
                                call    Trim using str15
                                move    C1,NUSEPATH
                                rep     zfill,NUSEFLD
                                call    NUSEKEY
                                if not over     Should NOT happen
                                        scan    "BILLING",NUSEUSER
                                        if not equal
                                                move    NUSEUSER,str1
                                                loop
                                                        bump    NUSEUSER,1
                                                        cmatch  B1,NUSEUSER
                                                        until equal
                                                        until eos
                                                repeat
                                                if not eos
                                                        bump    NUSEUSER,1
                                                        move    NUSEUSER,str7
                                                        call    RemoveChar using str7,B1
                                                        move    str7,str6
                                                        clear   str24
                                                        pack    str24,str1,str6,"@NINCAL.COM"
                                                endif
                                                reset   NUSEUSER
                                                clear   str10
                                                if (N1 = 1)
                                                        pack    str10,"LR "
                                                endif
                                                pack    MailSubjct,str10,str15
.   Set the text message that is send with the attachments
                                                clear   MailBody
                                                Append	"This record was locked opened by you on ",MAilbody
                                                append	str11,Mailbody
                                                append	PERIOD,Mailbody
                                                append	CRLF,Mailbody
                                                Append    	"I.S. has saved a copy of this record prior to any modifications you might have made.",Mailbody
                                                append	CRLF,Mailbody
                                                Append    	"Please review and let I.S. know when we can release it.",Mailbody
                                                append	CRLF,Mailbody
				Reset	Mailbody
				Pack	Mailfrom from "InformationServices@nincal.com"
				Pack	MailTo from nuseuser,"@nincal.com,","InformationServices@nincal.com"
                                                call    	SendMail
                                        endif
                                endif
                        endif
                endif
        repeat
.Delete any existing instances of Directory File
        move    "                                        ",APIFileName
        clear   APIFileName
        pack    APIFileName,File1Path,file1nme,hexzero
        call    DeleteFile
        if (APIResult = 0 | APIResult = hexeight)
        endif
        STOP

FileIOError
        noreturn
        goto LoopStart

        include nuseio.inc
        include comlogic.inc


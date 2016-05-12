        INC       COMMON.inc
        include   cons.inc


NINPRINT         IFILE     KEYLEN=6,FIXED=696
PRINTLFILE       IFILE     KEYLEN=6,FIXED=408
REN      DIM       55
BATPATH       INIT    "\\nins1\e\apps\WINBATCH\"
OPNFLG   FORM  1           
PRNTFILE DIM  10
ERRMSG    DIM  55
.========================================================================================
         MOVE      "FM0001" TO PROGRAM
         MOVE      "File Manager Check" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
release   init      "1.22"       18JUUN05 Changed IP address of File Manager
.release   init      "1.21"       11Feb04 send message do Comp Request.
;release   init      "1.2"        Changed time of stop fm and start fm.
;release   init      "1.1"       Loop to check if time is after scheduled restart of FM
.                               Email now sent if FM job ran
.release   init      "1.0"      06July2001 jd new.
.                      

         CLEAR       OPNFLG
         CLEAR     PRNTFILE
NINPRINTs
         DISPLAY   *P30:12,*EL,"OPENING NINPRINT EXCLUSIVE"
         TRAP      LOCKPRINT GIVING ERROR IF IO
.>Patch 1.22 Begin
.         OPEN      NINPRINT,"NINPRINT.ISI|20.20.30.103:502",exclusive
          OPEN      NINPRINT,"NINPRINT.ISI|NINS1:502",exclusive
.>Patch 1.22 End         
         DISPLAY   *P30:12,*EL,"NINPRINT EXCLUSIVE OPEN OK!!!"
         TRAPCLR   IO
         close     NINPRINT
         goto      PRINTL

LOCKPRINT
         TRAPCLR   IO
         DISPLAY   *P30:12,*EL,"NINPRINT EXCLUSIVE OPEN FAILED",*w3
;         execute "c:\winnt\system32\cmd.exe /c at \\nins1 18:53 c:\FMSTOP.bat"
         execute "c:\winnt\system32\cmd.exe /c at \\nins1 19:05 c:\FMSTOP.bat"
         pause     "60"
;         execute "c:\winnt\system32\cmd.exe /c at \\nins1 18:55 c:\FMSTart.bat"
         execute "c:\winnt\system32\cmd.exe /c at \\nins1 19:08 c:\FMSTart.bat"
.         pause     "30"
           MOVE C1 TO OPNFLG
           goto LASTCHANCE



PRINTL
         DISPLAY   *P30:12,*EL,"OPENING NINPRINTL EXCLUSIVE"
         TRAP      LOCKPRINTL GIVING ERROR IF IO
.>Patch 1.22 Begin
.         OPEN      PRINTLFILE,"NINPRINTL.ISI|20.20.30.103:502",exclusive
         OPEN      PRINTLFILE,"NINPRINTL.ISI|NINS1:502",exclusive
.>Patch 1.22 End         
         DISPLAY   *P30:12,*EL,"OPENING NINPRINTL EXCLUSIVE OPEN OK!!!"
         TRAPCLR   IO
         close     PRINTLFILE
         call      ALLGOOD
           goto      FINISHED

LOCKPRINTL
         TRAPCLR IO
         DISPLAY   *P30:12,*EL,"NINPRINTL EXCLUSIVE READ FAILED",*w6
         execute "c:\winnt\system32\cmd.exe /c at \\nins1 19:05 c:\FMSTOP.bat"
         pause     "60"
         execute "c:\winnt\system32\cmd.exe /c at \\nins1 19:08 c:\FMSTart.bat"
.                             pause     "30"
          MOVE C2 TO OPNFLG
.                             goto LastChance


LASTCHANCE
          LOOP
          clock time,str11
                UNPACK STR11,HH,STR1,MN
                UNTIL (MN > "15")
          REPEAT
          BRANCH OPNFLG TO LASTCHANCE1,LASTCHANCE2
LASTCHANCE1
                              TRAPCLR IO
                              DISPLAY   *P30:12,*EL,"Verifing File Manager Started",*w3
                    MOVE "NINPRINT" TO PRNTFILE
                              TRAP      FAILED GIVING ERROR IF IO
.>Patch 1.22 Begin                      
.                             OPEN      NINPRINT,"NINPRINT.ISI|20.20.30.103:502",exclusive
                    OPEN      NINPRINT,"NINPRINT.ISI|NINS1:502",exclusive
.>Patch 1.22 End                        
                    DISPLAY   *P30:12,*EL,"NINPRINT EXCLUSIVE OPEN OK!!!",*w6
                              TRAPCLR   IO
                              CALL ALLGOOD
                              goto finished
LASTCHANCE2
                              DISPLAY   *P30:12,*EL,"GIVING NINPRINTL EXCLUSIVE ONE MORE TRY!!",*w6
                    MOVE "NINPRINTL" TO PRNTFILE
                              TRAP      FAILED GIVING ERROR IF IO
.>Patch 1.22 Begin                                                    
.                             OPEN      PRINTLFILE,"NINPRINTL.ISI|20.20.30.103:502",exclusive
                    OPEN      PRINTLFILE,"NINPRINTL.ISI|NINS1:502",exclusive
.>Patch 1.22 End                                            
                    DISPLAY   *P30:12,*EL,"OPENING NINPRINTL EXCLUSIVE OPEN OK!!!",*w6
                    TRAPCLR   IO
                              CALL ALLGOOD
                                                  goto finished
FAILED
                              PACK      REN,"REN ",BATPATH,"NORDPRT.EXE NORDPRT.FLD"
                              PACK     TASKNAME,"c:\winnt\system32\cmd.exe /c ",REN
                    Execute   TASKNAME

SNDMAIL
         Move    "FILE LOCKED BY THE FILE MANAGER!!!!!",MailSubjct
.   Set the text message to IS reporting Locked files
                      PACK    ERRMSG,ERROR," error received while opening ",PRNTFILE
          Clear     MailBody
          append    ErrMSG,MailBOdy
          append    CRLF,MailBody
        clear   str55
        append  PRNTFILE to str55
        append  " WAS LOCKED BY THE FILE MANAGER." to str55
        reset   str55
          Append    Str55,Mailbody
          append    CRLF,MailBody

          Append    "Nordprt Renamed...File Manager needs to be cleared and Jobs Ran.",MailBody
          append    CRLF,MailBody
          Reset     MailBody

         clear     taskname
          move      "INformationServices@nincal.com",MailFrom

        move   "ComputerRequest@nincal.com",MailTO


          call      SendMail

        DISPLAY   *P30:12,*EL,"EMAIL....SENT!!!!!!",*w6
                      goto finished1
FINISHED
         DISPLAY   *P30:12,*EL,"JOB SUCCESSFUL........BYE!!!!!!"
         stop
FINISHED1
          DISPLAY   *P30:12,*EL,"JOB WAS NOT SUCCESSFUL....CHECK FM!!!!!!"
        stop
        include   comlogic.inc
.         stop
ALLGOOD
         Move    "FM0001 Ran!!!.........",MailSubjct
.   Set the text message to IS reporting No Locked files detected
                      if (OPNFLG = C1 OR OPNFLG = C2)
                    PACK    ERRMSG,PRNTFILE," Locked last night...SHOULD BE CLEAR NOW."
                      ELSE
                              PACK    ERRMSG,"No locked files found!!!"
                      ENDIF
          Clear     MailBody
          append    ErrMSG,MailBOdy
          append    CRLF,MailBody
          Reset     MailBody

          clear     taskname
          move      "INformationServices@nincal.com",MailFrom

          move   "ComputerRequest@nincal.com",MailTO


          call      SendMail
        
        DISPLAY   *P30:12,*EL,"EMAIL....SENT!!!!!!",*w6

PC         EQU        0
           INCLUDE    COMMON.inc
           INCLUDE    CONS.inc
           INClude    compdd.inc
           INclude    Cntdd.inc

Release    Init       "2.02"     DLH Modify emai to HTML use office365 email server
Reldate    INit       "2015 December 30"
.Release   Init       "2.01"     DLH sort summary file
.Reldate   INit       "13 November 2008"
.Release   Init       "2.0"      DLH total rewrite of Neom0007
.Reldate   INit       "24 June 2008"
.
.
. ..................
.
.
. PROGRAM VARIABLES
. .................
Input      FIle
UNlock     FIle
READNUM    FORM      5       NUMBER OF RECORDS READ.
UPDNUM     FORM      5       NUMBER OF RECORD UPDATED.
DATE       DIM       8
.
.
           TRAP       EXIT IF F5
            MOVE                 "ABORT " TO PF5
           MOVE       "Credit0002" TO PROGRAM
           MOVE       "NINCAL" TO COMPNME
           MOVE       "CLEAR CREDIT Status" TO STITLE
           MOVE       "99/99/99" TO TODAY
           CLOCK      DATE TO DATE
           IFNZ          PC
           MOVE       DATE TO N6
           EDIT       N6 TO TODAY
           XIF
           IFZ           PC
           MOVE       DATE TO TODAY
           XIF
           CALL          PAINT
           CALL       FUNCDISP
           move       c1,comppath
           DISPLAY    *P15:12,"NUMBER OF Records READ    : ":
                      *P15:14,"NUMBER OF Records UPDATED : ";
....
.          goto       donex
.

           Open       Input,"company.dat|NINS1:502"
           Prepare    UnLock,"c:\work\UNLock.dat",Exclusive
           
...............................................................................
READ       
           FilePi     1;Input
           READ       Input,SEQ;COMPVARS
           GOTO       DONE IF OVER
           ADD        C1 TO READNUM
           DISPLAY    *P44:12,READNUM;
.if mailer  check status
           if         (COMPMLRFLG <> "T")
           GOTO       READ
           Else

.              ' '=OK,
.             "*" = ON HOLD.
.             "I" = INACTIVE,
.             "B" = CREDIT RISK.  -      reset nightly if released
.             "N" =   NEW MAILER.
.             "P" = POLITICAL MAILER.  - reset nightly if released
.             "W" = Warning - read note                     ; 21Dec2000
.             "M" = Must Prepay                                            ; 05Mar2002
.             "9" = On hold until over 90s paid             ; 05Mar2002
.             "G" = Guarantees are always required          ; 05Mar2002
.             "g" = Guarantees No longer accepted           ; 06Apr2004
                      if         (COMPCREDIT <> "*")
                      goto       REad
                      endif
           Packkey    COmpfld from Compnum
           ADD        C1 TO UPDNUM
.
           CALL       CompKEY
                      if         OVer
                      Move       "dherric@nincal.com",Mailto
                      move       "Creques@nincal.com",MailReply
                      move       "credit Read failure",Mailsubjct
.                      Move      "smtp.office365.com",MailServer
.                      Move      "99Webmail99",mailpass
                      move      "creques@nincal.com",mailuser  
.                      move        Yes,Mailttls
                      move      c1,MailType          .HTML


                      Clear      Mailbody
                      Append     "Credit0002 Nightly Run",Mailbody                      
                      Append     "<br>",Mailbody
                      append     "Company ## ",mailBody
                      append     Compnum,Mailbody
                      Append     "<br>",Mailbody
                      reset      Mailbody
                      call       SendMail
                      GOTO       READ
                      endif
           MOVE       " " TO CompCredit
           call       Debug
           CALL       CompUPD
           DISPLAY    *P44:14,UPDNUM;
           write      Unlock,seq;Compnum,compcomp

.          Clear      Mailbody
.          Move       "dherric@nincal.com",Mailto
.          move       "Creques@nincal.com",Mailfrom
.          move       "credit Released",Mailsubjct
.          Append     "Credit0002 Nightly Run",Mailbody                      
.          Append     CRLF,Mailbody
.          append     "Company ## ",mailBody
.          append     Compnum,Mailbody
.          Append     CRLF,Mailbody
.          append     Compcomp,Mailbody
.          Append     CRLF,Mailbody
.          reset      Mailbody
.          call       SendMail
           GOto       REad
           endif
.
DONE
           Weof       Unlock,seq
           Close      Unlock
Donex
.temp fix
.          MOve       "56",updnum
.temp fix
           DISPLAY    *P1:24,"JOB DONE / RETURNING TO CHAIN",*W5;
           Clear      Mailbody
           Move       "dherric@nincal.com",Mailto
           move       "Creques@nincal.com",Mailfrom
.           move       "Creques@nincal.com",MailReply
.           Move      "smtp.office365.com",MailServer
.           Move      "99Webmail99",mailpass
.           move      "creques@nincal.com",mailuser  
.           move        Yes,Mailttls
           move      c1,MailType          .HTML


           move       "credit Released",Mailsubjct
           Append     "Credit0002 Nightly Run",Mailbody                      
           append     "<br>",mailbody
           append     "Records Read: ",mailBody
           append     Readnum,Mailbody
           append     "<br>",mailbody
           append     "Mailers Released: ",Mailbody
           append     Updnum,Mailbody
           append     "<br>",mailbody
           if         (Updnum > 0)
.begin patch 2.02
           MOve       "C:\work\unlock.srt",Taskname
           erase      Taskname
           Display    *p1:22,*el,"sorting unlock"
           Pack       Taskname from "c:\work\unlock.Dat C:\work\unlock.srt -7-61"
           Sort       Taskname
.          Open       UNlock,"C:\work\unlock.dat"
           Open       UNlock,"C:\work\unlock.srt",exclusive
.end patch 2.02
           loop
           read       UnLock,seq;compnum,mcomp
           until      over
.temp fix
.          Packkey    COmpfld from Compnum
..
.          CALL       CompKEY
.          move       compcomp,mcomp
.temp fix

           append     compnum,mailbody
           append     b1,mailbody
           append     Mcomp,mailbody
           append     "<br>",mailbody
           repeat     
           endif
           reset      Mailbody
           call       SendMail

           Shutdown   "cls"

EXIT
           TRAPCLR   F5

           Shutdown   "cls"

           INCLUDE    CompIO.inc
           INclude    CntIO.inc
           INCLUDE    COMLOGIC.inc


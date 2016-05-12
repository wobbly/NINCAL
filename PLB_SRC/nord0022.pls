.TDMCDELPRT - WRITTEN 03/17/87.
...............................................................................
.PURPOSE - PRINTS INFORMATION ON ORDERS THAT SHOULD HAVE BEEN T/C'D TO 
.          TRIPLEX BUT HAVE BEEN REMOVED FROM T/C FILE.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
release  init      "1.6"       DLH Sunbelt PDF
Reldate   Init      "2013 April 24"
.release  init      "1.5"       DLH only print if records
.Reldate   Init      "05 March 2010"
.release  init      "1.4"       ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.3"       ASH 21Sep98 NINMLR Y2K File expansion
.                              TCDELETE file written to in NORD0023.PLS
.RELEASE  INIT      "1.2"       DLH 18MAR92
...............................................................................
.
.Start Patch #1.3 - File expanded
.DELETE   IFILE     KEYLEN=6,VAR=76
DELETE   IFILE     KEYLEN=6,VAR=96
.End Patch #1.3 - File expanded
.
.VARIABLES.
............
.Start Patch #1.3 - remmed and replaced line, var expanded
.MCOMP    DIM       25             MAILER COMPANY NAME.
MCOMP    DIM       45             MAILER COMPANY NAME.
.End Patch #1.3 - remmed and replaced line, var expanded
O1DES    DIM       35             LIST NAME
KEYMD    DIM       6              LR NUMBER
TYPIST   DIM       2              INITIALS OF TYPIST WHO DELETED LR.
.
DATE     DIM       8
SIXTY    FORM      "60"
THREE    FORM      "3"
LINES    FORM      2
.begin patch 1.5
          Include   PrtPagedd.inc
         INCLUDE   HP.inc
Page      Form      2
Count     Form      1
FileCheck FIle
trapcount form      4
.end patch 1.5
.
         IFNZ      PC
         OPEN      DELETE,"TDMCDELETE"
         XIF
         IFZ      PC
         OPEN      DELETE,"TCDELETE"
         XIF
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,SLASH,DD,SLASH,YY
         MOVE      DATE TO TODAY
         XIF
         MOVE      "NORD0022" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "TDMC T/C DELETES - PRINT" TO STITLE
.START PATCH 1.4 REPLACED LOGIC
.         splopen   "g:\data\tcdel.lst"

.begin patch 1.5
.begin patch 1.6
.          Call      PDF995Auto
.end patch 1.5
.          PRTOPEN   Laser,"PDF995","TDMCDEl"
          pack      str45,"c:\work\pdf\TDMCDEL.pdf"
          PRTOPEN   Laser,"PDF:",str45
.          pack      str45,"TDMCDEL.pdf"
.end patch 1.6
.         PACK      STR35,NTWKPATH1,"tdmcdel.lst"
.         splopen   STR35
.END PATCH 1.4 REPLACED LOGIC
         CALL      PAINT
.begin patch 1.5
.         CALL      HEADER
.end patch 1.5
.
READ     filepi    1;delete
         READ      DELETE,SEQ;KEYMD,MCOMP,O1DES,DATE,TYPIST
         GOTO      EOJ IF OVER
         COMPARE   SIXTY TO LINES
         CALL      HEADER IF NOT LESS
         CALL      HEADER IF EQUAL
.begin patch 1.5
          add       c1,count
         if         (page = c0)
         call       Header
         endif
.end patch 1.5
         ADD       "2" TO LINES
         PRINT     *N,KEYMD,*8,MCOMP,SLASH,O1DES,"  ",DATE,"  ",TYPIST
         GOTO      READ
HEADER   MOVE      "05" TO LINES
.begin patch 1.5
          Add       C1,page
          If (Page = c1)
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0
          else
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0,*Newpage
          endif
          prtpage   Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          move      "1500",row
          prtpage   Laser;*p=1750:row,"ORDERS NOT SENT TO INFOGRP":
                    *p=1:row,"Date: ",Today,*p=6750:row,"Page: ",Page
          add       eightlpi to row
          add       eightlpi to row
          add       eightlpi to row
          prtpage   Laser;*p=250:row,"LR",*p=1050:row,"Mailer",*p=3000:row,"List",*p=4500:row,"Date Deleted"
          add       eightlpi to row
          add       eightlpi to row
.         PRINT     *F,*10,"ORDERS NOT SENT TO TRIPLEX";
.         PRINT     *C,*10,"__________________________",*60,TODAY:
.         PRINT     *C,*10,"__________________________",*60,TODAY,B1,"Page: ",page:
.                   *N,*N,*3,"LR",*17,"MAILER",*49,"LIST",*71,"DATE DELETED";
.         PRINT     *C,*3,"__",*17,"______",*49,"____",*71,"____________"
..         PRINT     *75,"TYPIST",*N
.end patch 1.5
         RETURN
EOJ      CLOSE     DELETE
.begin patch 1.5
.         splclose 
          PrtCLose  Laser
.begin patch 1.6
.          Call      PDF995Auto0
.end patch 1.6
          if        (count >= c1)
CheckFile
.begin patch 1.6
.          pack      MailAttach from "C:\WORK\pdf\",Str45                  ."
          pack      MailAttach from Str45                  
.end patch 1.6
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.end patch 1.39


          Move      "Orders not Sent",MailSubjct
          Clear     MailBody
          append    str45,mailbody
          append    CRLF,Mailbody
          Reset     MailBOdy
          pack      Mailto from "Creques@nincal.com"
          pack      MailFrom from "Creques@nincal.com"
          Call      SendMail
          Call      PDF995Auto0
          
.end patch 1.5
TEST         
.begin patch 1.5
.START PATCH 1.4 REPLACED LOGIC
.         execute "c:\command.com /c copy g:\DATA\tdmcdel.LST \\nts0\LASER2 "
.         PACK    TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"tdmcdel.LST \\nts0\LASER2 "
.         execute TASKNAME
.         PACK    TASKNAME,NTWKPATH1,"tdmcdel.LST"
.         erase      Taskname
          Erase     MailAttach
.END PATCH 1.4 REPLACED LOGIC
.         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.         pause     "10"
          endif
         stop
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me
                    Pack       MailSubjct,"Nord0022  ",str55
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str45,MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 1.5
         INCLUDE   COMLOGIC.inc


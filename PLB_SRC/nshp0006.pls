PC       EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NORDDD.inc
          include   compdd.inc
          include   cntdd.inc
          INCLUDE   NSHPDD.inc
          INCLUDE   NOWNDD.inc
          include   hp.inc
          include   nofrdd.inc
          INCLUDE   NSEL2DD.INC
          INCLUDE          PRTPAGEDD.INC
.begin patch 1.36
          include   winapi.inc
.end patch 1.36
.
. .............................................................................
Release   init      "1.40"                      DLH        Sunbelt PDF
Reldate   init      "2013 April 23"
.Release   init      "1.38"                      DLH         speed it up
.Reldate   init      "March 03, 2010"
.Release   init      "1.37"                      DLH         wait for email attachment
.Reldate   init      "August 07, 2008"
.Release  init      "1.36"                      DLH         M0dify add PLI LM and pdf it, email to Joey and eliminate old changes
.                                               see previous release for release changes prior
.Reldate  init      "March 11, 2008"
.Release  init      "1.35"                      ASH         19JUN2007 PLI Inclusion
.Reldate  init      "July 19, 2007"
.Release  init      "1.34"                      DMB         12OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release  init      "1.33"              JD        15SEP2006 No date check, include TDMC
.Release  init      "1.32"              DMS       22JUN2006 Fulfillment Conversion
.Release  init      "1.31"              JD        22MAY2006 Created print file
.Release  init      "1.3"               DMB       26MAY2004 Mailer Conversion
.release  init      "1.23"              ASH       29JAN2004  DATACARD CONVERSION
.Reldate  init      "OCTOBER 12, 2006"
.reldate  init      "JANUARY 29, 2004"
.Release   init     "1.22"                        18Mar2002 added extra date check, only include orders that are past due 3 days.
.Release   init     "1.21"              ASH       05FEB2002 ASH NINFUL CONVERSION
.Release   init     "1.2"               DLH       27Sep2001 DLH attempt to elim phantom reuse orders
.release  init     "1.1"                DLH       06Feb2001 DLH fax, email, or as last resort print the reports.
.RELEASE INIT      "1.0"                DLH       22January2001 DLH  as report
* NAMES IN THE NEWS CALIF. Nightly List Management SHIPPING Info request REPORT PROGRAM
. .................................................................................................................
.begin patch 1.37
FileCheck FIle
trapcount form      4
.end patch 1.37

.
. WORK VARIABLES
.
PDATE    DIM       8
TELEMASK INIT      "(999)999-9999"
ORDMASK INIT       "ZZZ,ZZ9,999"
ORDQTY   DIM       11
QTYNUM   FORM      9
SYSJDATE FORM      5
TELE1    DIM       13
fax1    DIM       13
fax2    DIM       5
CODENUM  FORM      2
.
PROGNAME DIM       8
.
.begin patch 1.36
.timestamp1          dim       16
timestamp2          dim       16
.time1     form      16
.time2     form      16
.time3     form      16
PageOne   Form      3
Vpos      FOrm      5
.end patch 1.36
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
HOLDOWN  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "46"
COUNTO   FORM      8                  NUMBER OF ORDERS READ.
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
Output   File
.
NFULCOMP  DIM       55
HOLDEXCL  DIM       1
. .............................................................................
*******************************************************************************
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F3
         MOVE      "EXIT" TO PF3
         MOVE      "NSHP0006" TO PROGRAM
         MOVE      "Nightly SHIPPING REPORT" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         move      c2 to nshplock
;START PATCH 1.2 REPLACED LOGIC
         pack      str35,NTWKPATH1,"weekship.lst"
         splopen   STR35
         clear     str35
*****************
*****************
.
PassOne
.begin patch 1.38
.            move     "643810" to nordfld            .1st lr of 2008
            move     "686400" to nordfld            .1st lr of 2009
            move     "795000" to nordfld            .early lr of 2014
.end patch 1.38
          CLOCK     DATE TO PDATE
.          move      "07-16-15 ",Pdate
          move      pdate to date
          UNPACK    DATE INTO MM,str1,DD,str1,YY
          CALL      CVTJUL
          MOVE      JULDAYS TO SYSJDATE
          move      juldays to hidate
          move      juldays to lodate
          sub       c3 from lodate
.begin patch 1.38
          Move      c3,Nordlock
.end patch 1.38
          move     c1 to nordpath
          call       nordtst
.begin patch 1.38
          PACK   STR35,NTWKPATH1,"DISKIN71.tmp"
          PACK   Taskname,NTWKPATH1,"DISKIN71.tmp|nins1:502"
.          prepare  output,str35,exclusive
          prepare  output,taskname,exclusive
.end patch 1.38
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..8 = Windows CE
        if       (str1 = "3" or str1 = "4")
        move     c1 to osflag
        endif
        if       (str1 = "1" or str1 = "5")
        move     c2 to osflag
        endif
.begin patch 1.36
          Move      c1,PageOne
.end patch 1.36

.        goto        passtwo


Pass1Loop call     nordks
          goto     eoj1 if over
          DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
          add      c1 to counto
          DISPLAY   *P15:10,"RECORDS IN = ",COUNTO;
          RESET     CANCODES               *RESET FORM POINTER.
          SCAN      OSTAT IN CANCODES       *CANCELLED?
          GOTO      Pass1Loop IF EQUAL
          move      "pxlz" to str4
          scan     ostat in str4
          goto      Pass1Loop if equal
          RESET     RUNCODES
          SCAN      OLNUM IN RUNCODES
          GOTO      Pass1Loop IF EQUAL
          CMatch    "B" to ostat
          goto      PASS1LOOP if equal            .already billed skip.

          match      "0001",ortnnum       .REUSE
          goto       pass1loop if equal
          move       c0 to n4
          move       ortnnum to n4
          branch     n4 of pass1loop

.START PATCH 1.35 ADDED LOGIC
.This is a cheat.  Since we need to look in 2 different places, I am using
.an unused variable (ORCODE) to store the value I want to sort on
.this needs to be redone it should be a combination of the OcompId's and if list management or brokerage

.begin patch 1.36            For Joeys report I am ignoring
.         if (OCompID = "P" | OCompID2 = "P")
.                   move      "P",ORCODE
.         else
                    clear     ORCODE
.         endif
.end patch 1.36

          clear     str2
          pack      str2 from OSALES10,osales
          move      c0 to n2
          move      str2 to n2
.begin patch 1.36
          Move      c1,PageOne
.          if        (n2 = 6 or N2 = 19)           .List Management?
.          if        (n2 = 6 or N2 = 19)           .List Management?
          if        (n2 = 6 or N2 = 19 or N2 = 27 or N2 = 28)           .List Management?
.end patch 1.36
          goto      checkdate
          else
          goto      pass1loop
          endif
.
CheckDate

.
                    MOVE      OLRN TO NSHPFLD
                    rep       zfill in nshpfld
                    CALL      NSHPKEY
                    goto      pass1loop if not over         .already shipped
                    MOVE      OLON TO NOWNFLD
                    REP       ZFILL IN NOWNFLD
                    CALL      NOWNKEY
          Call Trim Using OFULLFIL

          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit usin COMPFLD
                    move      C1,COMPPATH
                    move      "checktwo-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY

                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif

          else      //  OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif

                    if        (ortndtem > "0" & ortndtem < "13")
                    move      ORTNDTEM  to mm
                    move      ORTNDTEd  to dd
                    move      ORTNDTEy  to yy
                    move      ORTNDTEc  to cc
                    else
                    move      OMDTED  to mm
                    move      OmdTEd  to dd
                    move      OmdTEy  to yy
                    move      OmdTEc  to cc
                    endif
                    call      cvtjul
                    if        (juldays <= sysjdate)
                    write     Output,seqeof;ordvars
                    else
                    endif

          goto      Pass1Loop
eoj1
.close - sort
         weof       output,seqeof
.         DISPLAY   *P15:23,*EL,"Sorting records = ";
          DISPLAY   *P01:24,*EL,*HON,"S-O-R-T-I-N-G",*HOFF;
         close      nordfile
         close      output
        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;1-1,22-25,66-73"
        sort    taskname
        if over
                move    s$error$,error
                   DISPLAY   *P15:23,*EL,"Sorting Error = ",Error,*w5,*b,*w5;
                stop
        endif
.passtwo - print
passtwo

         move       C0 TO COUNTO
         CALL      FUNCDISP
        clear   taskname
         move       c0 to nordflag
.begin patch 1.38
          PACK   STR35,NTWKPATH1,"DISKIN71.dat"
          PACK   Taskname,NTWKPATH1,"DISKIN71.dat|NINS1:502"
.         open      output,str35,exclusive
         open      output,taskname,exclusive
.end patch 1.38

.begin patch 1.36
.begin patch 1.4
.          call      pdf995auto

.;START PATCH 1.31 REPLACED LOGIC
.         pack      str35,NTWKPATH1,"weekship.lst"
.         splopen   STR35
                        move    "Nshp0006" to  str25
.                        PRTOPEN Laser,"PDF995",str25
                        PRTOPEN Laser,"PDF:","c:\work\pdf\NShp0006.pdf"
.                        pack    str55,str25,".pdf"
                        Pack  Str55 from "c:\work\pdf\NShp0006.pdf"
.end patch 1.4
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Portrait
.end patch 1.36
                        
         
.begin patch 1.36
.         PRINT     hp17ptch,hptop                .compressed
.end patch 1.36
          MOVE      "                    " TO FERROR
.
GETREC    DISPLAY   *P01:24,*EL,*HON,"P-R-I-N-T-I-N-G",*HOFF;
.
          read      output,seq;ordvars
          GOTO      EXIT IF OVER
          bump      OODNUM,4
          pack      NOFRFLD,OMLRNUM,OODNUM
          reset     OODNUM
          move      "Rest-NOFRKEY",Location
          call      NOFRKEY
          ADD       C1 TO COUNTO
          DISPLAY   *P15:12,"Printing Record = ",COUNTO;
          MOVE      OLON TO NOWNFLD
          REP       ZFILL IN NOWNFLD
          CALL      NOWNKEY
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD                   
                    move      C1,COMPPATH
                    move      "GETREC-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif

          if (NOWNFLD <> HOLDOWN | HOLDEXCL <> ORCODE)
                    call      BREAK
          endif

          CALL      MLRREAD
          GOTO      PRINT
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CALL      OVER IF OVER
         RETURN
.
.
*......................................................................
HEADER   ADD       C1 TO PAGE
         MOVE      TELEMASK TO TELE1
         EDIT      OWNTELE TO TELE1
         clear     fax1
         clear     fax2
         if        (ownfax <> " " & ownfax <> "")
         move      telemask to fax1
         edit      ownfax to fax1
         move      "Fax ##" to fax2
         endif
.begin patch 1.36
.         if (ORCODE = "P")
.begin patch 1.36
.                   PRINT     *F,*n,*50,"P A C I F I C       L I S T S",*119,"DATE:":
.                             PDATE:
.                             *L,*40,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                             *119,"PAGE ## ",PAGE:
.                             *L,*L,*7,"LIST OWNER ##",*21,OLON:
.                             *L,*21,OWNLONM,tele1:
.                             *L,*21,OWNOCPY,fax1,b1,fax2:
.                             *L,*21,NFULCOMP
          IF        (PageOne = c1)
                    add       c1,PageOne
          else
                    PrtPage   Laser;*Newpage
          endif
                    PrtPage   Laser;*FONT=prtpg10:
                              *P=3275:50,"N A M E S   I N   T H E   N E W S":
.                              *P=3525:225,"P A C I F I C       L I S T S":
                              *p=7250:50,"DATE:",Pdate:
                              *p=7250:225,"PAGE ## ",PAGE:
                              *p=3000:400,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
                              *p=600:700,"Owner/Manager ##",*p=1600:700,OLON:
                              *p=600:825,OWNLONM,*p=3000:825,tele1:
                              *p=600:950,OWNOCPY,*p=3000:950,fax1,b1,fax2:
                              *p=600:1075,NFULCOMP:
                              *P=3250:1500,"Mail/Rtn":
                              *P=4000:1500,"Quantity":
                              *P=4750:1500,"Shipped":
                              *P=5575:1500,"Postage":
                              *P=6250:1500,"Shipping Method":
                              *P=1:1750,"LR ##/List":
                              *P=750:1750,"Mailer/Offer":
                              *P=3250:1750,"Date":
                              *P=4000:1750,"Ord/Ship":
                              *P=5575:1750,"Amount":
                              *P=6250:1750,"Tracking Number"
                    Move      "2000",VPos         
.                   goto      Exit

.         prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
.                       *Pictrect=*off,*PICT=0:1000:2250:8550:NINLogo:
.                    *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
.                    *FONT=prtpg10,*P=7000:375,"Time: ",Time:

.end patch 1.36
.         else
.                   PRINT     *F,*n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
.                             PDATE:
.                             *L,*40,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                             *119,"PAGE ## ",PAGE:
.                             *L,*L,*7,"LIST OWNER ##",*21,OLON:
.                             *L,*21,OWNLONM,tele1:
.                             *L,*21,OWNOCPY,fax1,b1,fax2:
.                             *L,*21,NFULCOMP
.         endif
.         PRINT     *N,*50,"MAIL/RTN",*60,"QUANTITY",*72,"SHIPPED":
.                   *81,"POSTAGE":
.                   *93,"SHIPPING Method":
.                   *L,*1,"LR ##",*10,"MAILER/OFFER/LIST",*52,"DATE":
.                   *60,"ORD/SHIP",*74,"DATE",*81,"AMOUNT":
.                   *93,"Tracking Number"
.         MOVE      C10 TO LINES
.end patch 1.36
         RETURN
*......................................................................
PRINT
.Begin patch 1.36

          if        (Vpos >= 9500)
          call      Header
          endif
.         COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
.         COMPARE   C0 TO LINES
.         CALL      HEADER IF EQUAL
.end patch 1.36
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO ORDQTY
DETAIL
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.begin patch 1.36
          PrtPage   Laser;*FONT=prtpg10:
                    *p=1:vPos,Olrn:
                    *p=750:Vpos,*FONT=prtpg9,Mcomp,*FONT=prtpg10:
                    *p=3250:Vpos,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
                    *p=4000:Vpos,ORDQTY,*p=4750:Vpos,"__/__/____",*p=5575:Vpos,"$______",*p=6250:Vpos,"______________________________"
                    Add       "250",Vpos
          PrtPage   Laser;*p=750:Vpos,*FONT=prtpg9,OFDESC,*FONT=prtpg10:
                    *p=3250:Vpos,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
                    *p=4000:Vpos,"__________",*p=6250:Vpos,"______________________________"
                    Add       "250",Vpos
          PrtPage   Laser;*FONT=prtpg10B:
                    *p=1:vpos,O1DES,*FONT=prtpg10:
                    *p=3250:Vpos,NSel2Name
                    add       "500",Vpos
          
.         PRINT     *L,*1,OLRN,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *58,ORDQTY,*71,"__/__/____",*82,"$______",*91,"______________________________":
.                   *L,*10,OFDESC,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                   *59,"__________",*91,"______________________________":
.                   *L,*1,Hpbon,*10,O1DES,hpboff,b3,NSEL2NAME:
.                   *L
.         ADD       C4 TO LINES
.end patch 1.36
         GOTO      GETREC
BREAK
         CALL      NOWNKEY
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "BREAK-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif
         move      c0 to page
         CALL      HEADER
         MOVE      NOWNFLD TO HOLDOWN
          move      ORCODE,HOLDEXCL

*.................add code to create fax codes or email goodies

         RETURN
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
.begin patch 1.36
              PrtClose      Laser
          DISPLAY   *P01:24,*EL,*HON,"Preparing EMail",*HOFF;

                move    C0,N9
.begin patch 1.4
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,"C:\WORK\PDF\",str55,hexzero                ."
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
.                                if (time3 > 12000) .120 Seconds Maximum
.                                         break
.                                endif
.              repeat
.end patch 1.4

          Move      "Here is your Shipping Tickler",MailSubjct
          Move      "Creques@nincal.com",MailFrom
.          Move      "JoeyGamache@nincal.com",MailTo
.          MOve      "KrsniWatkins@nincal.com",MailTo
          MOve      "JenniferMagee@nincal.com",MailTo
.         Move      "DHerric@nincal.com",MailTo
          Move      "Creques@nincal.com",Mailcc
          Move      Str55,MailBody
          MOve      "c:\work\pdf\NSHP0006.pdf",MailAttach
          DISPLAY   *P01:24,*EL,*HON,"E-M-A-I-L-I-N-G",*HOFF;
.........................
.begin patch 1.37
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck

          call      debug
          Move      Yes,Mailtrace

.end patch 1.37

          call      SendMail

.begin patch 1.4
.          call      pdf995auto0
.end patch 1.4
          Erase     Mailattach

.          SPLCLOSE
.end patch 1.36
          shutdown
          STOP

EXIT1     shutdown
          STOP
.begin patch 1.37
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nshp0006 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 1.37

* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
..........................................................................
          INCLUDE   NSHPIO.inc
          INCLUDE   NOWNIO.inc
          include   compio.inc
          include   cntio.inc
.begin patch 1.36
          INCLUDE          PRTPAGEio.INC
.end patch 1.36
         INCLUDE   NORDIO.inc
         include   nofrio.inc
          INCLUDE   NSEL2IO.INC
        INCLUDE   COMLOGIC.inc

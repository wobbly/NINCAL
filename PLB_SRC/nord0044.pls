PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.72 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfuldd.inc
          include compdd.inc
          include cntdd.inc
..END PATCH 1.5 ADDED LOGIC
.END PATCH 1.72 REPLACED LOGIC
.begin patch 1.90
          include winapi.inc
.end patch 1.90
.
.Nord0044-- failed INFOGROUP CONFIRMS
Release   Init      "2.03"              .Replace Krsni with List Orders
Reldate   Init      "2015 June 9"
.Release   Init      "2.02"              .Replace Reuben with Robb
.Reldate   Init      "2015 May 18"
.Release   Init      "2.01"              .suppress email if no MIA records
.Reldate   Init      "2015 February 10"
.Release   Init      "2.00"              Sunbelt PDF
.Reldate   Init      "2013 April 23"
.Release   Init      "1.92"              remove Jack Forder from email
.Reldate   Init      "15 September 2011"
.Release   Init      "1.91"              update starting LR  725000
.Reldate   Init      "15 August 2011"
.Release   Init      "1.90"              update pdf handling to min standard
.Reldate   Init      "09 March 2011"
.Release   Init      "1.80"              Change starting LR to 700000, convert to PDF
.Reldate   Init      "05 November 2010"
.release    init         "1.79"               DLH  laser8
.Reldate   Init      "12 July 2010"
.release    init         "1.78"               JD 18Jun2008  Updated starting LR 670000, output laser2
.release   init         "1.77"             JD      Check for dummy billing lists
.release   init         "1.76"             DLH New fax server oct 2007
.release   init         "1.75"             JD Updated starting LR 600000, 625000.
.release   init         "1.74"             JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.RELEASE  INIT          "1.73"             DMB 27SEP2006 Changed Notification file to NINNOT.dat from tdmcnotify.dat
.RELEASE  INIT          "1.72"             DMS 22JUN2006 FULFILLMENT CONVERSION
.RELEASE  INIT       "1.71"         JD  20JAN2006 Updated to fax report back to Triplex/Donnelley
.RELEASE  INIT       "1.7"         DMB 18JUN2005 FM IP CHG
.RELEASE  INIT       "1.6"         jd  06MAY2004  ADDED GETWINVER.
.RELEASE  INIT       "1.5"        ASH 05FEB2002  NINFUL CONVERSION
LIVETC   IFILE     KEYLEN=6,VAR=7
TDMCORD  IFILE     KEYLEN=6,VAR=7
INFILE   FILE
tdmcout2 ifile     KEYLEN=6,var=21
TDMCOUT  IFILE     KEYLEN=6,VAR=7
prfile   pfile
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      10
MISSCNT  FORM      10
LR       DIM       6
check    form      5
check2   form      5 
check3   form      5
orddate  form      5
ODATE    DIM       6             .Order Date to be Printed
grpflg   form      1
clsflg   form      1
Rowcount form      3             .KEEP TRACK OF ROW PER PAGE
.osflag   form      1             . 1=win 95,98, 2=NT
Fulfil   DIM       6
Pfulfil  DIM       6
Liveflg  dim       2
YR       FORM      2
.PATCH 1.71 
faxflag  dim       1
.Begin patch 1.80
FileCheck           FIle
trapcount           form      4
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.end patch 1.80
.PATCH 1.71 
.
         MOVE      "NORD0044" to PROGRAM
         MOVE      "Names in the News" TO COMPNME
.begin patch 1.6
         Call           GetWinVer
.end patch 1.6
.Temp
.         OPEN      INFILE,"\\nins1\e\data\FAILED2"
.                    goto eoj
.temp
.>Patch 1.7
.         open      LIVETC,"tdmcord.isi|20.20.30.103:502"
         open      LIVETC,"tdmcord.isi|NINS1:502"
.>Patch 1.7
.>Patch 1.73 Begin
.         OPEN      TDMCOUT,"tdmcNOTIFY"
.         OPEN      TDMCOUT,"NINNOT"
         OPEN      TDMCOUT,"NINNOT|NINS1:502"
.>Patch 1.73 End         
         prepare   tdmcout2,"\\nins1\e\data\FAILED2.dat":
                            "\\nins1\e\data\FAILED2","6","21"
         OPEN      INFILE,"\\nins1\e\data\FAILED2"
         open      tdmcout2,"\\nins1\e\data\FAILED2"
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         MOVE      C1 TO NORDPATH        .SET ACCESS TO ISI.
.====================================================================================
..............................................................
.Menu
.Set Up Menu Bar
.mFile    menu
.mEdit    menu
.mOptions menu
.mHelp    menu
.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
.FData   init    "&File;E&xit"
.EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
.OData   init    "&Options;&Search;-;&Preferences"
.HData   init    "&Help;&About"
.====================================================================================
        move    "NORD0044.PLS",Wprognme
        move    "Check for InfoGroup Notification Records",Wfunction
        move    "Jose Duenas",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate
        call     paint

.        loop
.        waitevent
.        repeat
.====================================================================================
Start
.        call OrderSetMouseBusy
        clear readcnt
        clear misscnt
.=====================================================================================
          clock    timestamp,str8
        unpack   str8,str2,yy,mm,dd
        move     DD to n2
        sub            c1 from n2
        MOVE     N2 TO DD
        CALL     CVTJUL
        MOVE     JULDAYS TO CHECK3

.begin patch 1.80
.         move      "03" to mm
.         move      "21" to dd
.         move      "03" to yy
          MOve      check3,Check2
          Sub       "365",check2
.         call      cvtjul
         move      c1 to clsflg
.         move      juldays to check2
.end patch 1.80
.Start patch 1.78
.MOVE      "625000" TO NORDFLD
         MOVE      "725000" TO NORDFLD
.End patch 1.78
.Start patch 1.75
.MOVE      "600000" TO NORDFLD
.         MOVE      "625000" TO NORDFLD
.End patch 1.75
.         MOVE      "460000" TO NORDFLD
.         MOVE      "480000" TO NORDFLD
         CALL      NORDKEY
read
         call      nordks
         GOTO      EOJ IF OVER
.
         ADD       ONE TO READCNT
.         move      readcnt to str11
.         setitem   Nord0043StatRecord,0,str11
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
         if ((ostat = "0")|(ostat = "B"))
         else
                goto read
         endif
         if (oqty = "000000000")
                goto read
         endif
.         match     "0" to ostat
.         goto      read if not equal
.Start patch 1.77
          reset     EXFEELST
          scan      OLNUM,EXFEELST
          goto    read if equal
.End patch 1.77
.         
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         pack      odate with mm,dd,yy
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         IF        (CHECK >CHECK2 & CHECK <= CHECK3)
         GOTO      CONT
         ELSE
         GOTO      READ
         ENDIF
CONT
         move      olon to nownfld
         call      nownkey
          call      Trim using OWNCTN
.START PATCH 1.72 REPLACED LOGIC
.         if (OWNCTN <> "")
.                   pack      NFULFLD,OWNCTN
.                   rep       zfill,NFULFLD
.                   move      C1,NFULPATH
.                   move      "READ-NFULKEY",Location
.                   pack      KeyLocation,NFULFLD
.                   call      NFULKEY
.         else
.                   clear     NFULFLD
.                   clear     NFULCOMP
.         endif
.         if (NFULFLD = "0026")
.                   reset     RUNCODES
.                   scan      OLNUM,RUNCODES
.                   if not equal
.                             move      "fulfill",fulfil
.                             goto writdmc
.                   endif
.         else
.                   scan      "TDMC",NFULCOMP
.                   if equal
.                             reset     RUNCODES
.                             scan      OLNUM,RUNCODES
.                             if not equal
.                                       move      "fulfill",fulfil
.                                       goto writdmc
.                             endif
.                   else
.                             reset     NFULCOMP
.                             scan      "TRIPLEX",NFULCOMP
.;        if (OWNCTN <> "")
..                  pack      COMPFLD6,OWNCTN
..                  rep       zfill,COMPFLD6
..                  move      C1,COMPPATH
..                  move      "READ-COMPKEY6",Location
..                  pack      KeyLocation,COMPFLD6
..                  call      COMPKEY6
..                            if over
..                                      clear     COMPFLD6
..                                      clear     COMPCOMP
..                            else
..                                      if (COMPSVBFLG <> "T")
..                                                clear     COMPFLD6
..                                                clear     COMPCOMP
..                                      endif
..                            endif     
..        else      // OWNCTN = ""
..                  clear     COMPFLD6
..                  clear     COMPCOMP
..        endif
..        if (COMPFLD6 = "0026")
..                  reset     RUNCODES
..                  scan      OLNUM,RUNCODES
..                  if not equal
..                            move      "fulfill",fulfil
..                            goto writdmc
..                  endif
..        else
..                  scan      "TDMC",COMPCOMP
..                  if equal
..                            reset     RUNCODES
..                            scan      OLNUM,RUNCODES
..                            if not equal
..                                      move      "fulfill",fulfil
..                                      goto writdmc
..                            endif
..                  else
..                            reset     COMPCOMP
..                            scan      "TRIPLEX",COMPCOMP
.END PATCH 1.72 REPLACED LOGIC
..                            if equal
..                                      reset     RUNCODES
..                                      scan      OLNUM,RUNCODES
..                                      if not equal
..                                                move      "fulfill",fulfil
..                                                goto writdmc
..                                      endif
..                            endif
..                  endif
.Start Patch 1.74 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
.;
..        if (OWNCTN <> "")
..                  pack      COMPFLD6,OWNCTN
..                  rep       zfill,COMPFLD6
..                  move      C1,COMPPATH
..                  move      "READ-COMPKEY6",Location
..                  pack      KeyLocation,COMPFLD6
..                  call      COMPKEY6
. .
..                  if over
..                            clear     COMPFLD6
..                            clear     COMPCOMP
..                  else
..                            if (COMPSVBFLG <> "T")
..                                      clear     COMPFLD6
..                                      clear     COMPCOMP
..                            endif
..                  endif
..        else      // OWNCTN = ""
..                  clear     COMPFLD6
..                  clear     COMPCOMP
..        endif
.         if (OFULLFIL <> "")
.                   pack      COMPFLD,OFULLFIL
.                   call      zfillit using COMPFLD
.                   move      C1,COMPPATH
.                   move      "DISRTN-COMPKEY",Location
.                   pack      KeyLocation,COMPFLD
.                   call      COMPKEY
.                             if over
.                                       clear     COMPFLD
.                                       clear     COMPCOMP
.                             endif
.         else      // OFULLFIL = ""
.                   clear     COMPFLD
.                   clear     COMPCOMP
.         endif
          if (OFULLFIL = "009406")
                    reset     RUNCODES
                    scan      OLNUM,RUNCODES
                    if not equal
                              move      "fulfill",fulfil
                              goto writdmc
                    endif
          endif
.End Patch 1.74 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.          
.END PATCH 1.4 REPLACED LOGIC
.         goto      writdmc if equal
         goto      read
         
WRITDMC
         MATCH     "017717",OLNUM
         goto      read if equal
         MATCH     "018710",OLNUM
         goto      read if equal
         MATCH     "0002",OMLRNUM
         goto      read if equal
         MATCH     "0001",ORTNNUM
         goto      read if equal
         move      olrn to key
         FILEPI    1;TDMCOUT
         read      tdmcout,olrn;;
         goto      read if not over
.Check LiveTC file

         FILEPI    2;LIVETC
         move      key to olrn
         read      LIVETC,olrn;;
         goto      read if not over
writelr    WRITE     TDMCOUT2,key;key,odate,fulfil,liveflg
         add       c1 to misscnt
         move      yes to faxflag
         DISPLAY   *P10:16,"NUMBER OF MISSING NOTIFICATION ORDERS : ",MISSCNT
         GOTO      READ


EOJ
.===========================================================
chkflag
.begin patch 1.90
.begin patch 2.00
.          call      pdf995auto
.end patch 2.00
.end patch 1.90

.        getinfo  system,str6
.        unpack   str6 into str1,str2
.        unpack   str2 into str1
.        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..8 = Windows CE
.        if (str1 = "3" or str1 = "4")
.                 move     c1 to osflag
.        endif
.        if (str1 = "1" or str1 = "5" or str1 = "6")
.         move     c2 to osflag
.        endif
.begin patch 1.6
.begin patch 1.80
.               if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
..PATCH 1.71 
..         PRTOPEN prfile,"\\NINs2\Laser8","Failed2.dat"
.          PRTOPEN prfile,"faxfile",""
.               Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
..         PRTOPEN prfile,"Laser8","Failed2.dat"
.          PRTOPEN prfile,"faxfile",""
..PATCH 1.71 
.         Elseif         (osflag = c0)         .Don't know prompt for printer
.               splopen        "","R"
.               endif

.Begin patch 1.90 Logic Addition for PDF Quality Control
.begin patch 2.00
.                    call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
..                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                             "\\nins1\e\apps\plb\code\pdftest.bat":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.
.
.
          move    "Failed" to  str25
.          PRTOPEN Prfile,"PDF995",str25
          pack    str55,"c:\work\pdf\",str25,".pdf"
          PRTOPEN Prfile,"PDF:",str55
.          pack    str55,"c:\work\pdf\",str25,".pdf"
.end patch 2.00
.end patch 1.90
.end patch 1.80


.end patch  1.6
.==================================================================
.===============================================================================
.         CLOSE     INFILE
.        if (osflag = c2)
.         PRTOPEN prfile,"\\NINs2\Laser8","Failed2.dat"
.        else
.         PRTOPEN prfile,"Laser8","Failed2.dat"
.        endif
.=================================================================================
Page
.PATCH 1.71 
.        prtpage   prfile;*NEWPAGE:
.                  *UNITS=*HIENGLISH;
.PATCH 1.71 
        clear     rowcount
        clear     row
        move      "300",row
.======================================================================

        clock timestamp,str16
        unpack str16,str8,str6
        unpack str8,str4,mm,dd
        pack   str10,mm,slash,dd,slash,str4
        clear  str8
        unpack str6,hh,mn,ss
        pack   str8,hh,colon,mn,colon,ss
        pack   str24,str10,b4,str8
          prtpage   prfile;*UNITS=*HIENGLISH;
.PATCH 1.71 
.        prtpage prfile;*p1:1,*font=font12,"^[D14025376101^[NBrandi-TDMC  ^]";
.        prtpage prfile;*p1:1,*font=font12,"^[D14025376101^[NSherri-TDMC  ^]";
.Begin PATCH 1.80 
.        prtpage prfile;*p1:1,*font=font12,"^[D14025376101^[NEmily-TDMC  ^]";
          prtpage   PrFile;*Pictrect=*off,*PICT=0:1000:300:8300:NINLogo
                      add    eightlpi,row
                      add    eightlpi,row
                      add    eightlpi,row
.end PATCH 1.80
                      add    eightlpi,row
.PATCH 1.71 
        prtpage prfile;*p6100:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str24,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
.        if        (grpflg = c7)
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Brokerage and List Management",*ULOFF,*boldoff;
.        endif
        add     eightlpi,row
        add     "60",row
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Missing NOTICATION InfoGroup Orders",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"LR",*ULOFF,*boldoff;
        prtpage prfile;*p3000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Order Date",*ULOFF,*boldoff;
        prtpage prfile;*p4000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Ful/Ship",*ULOFF,*boldoff;
        prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Status",*ULOFF,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     "20",row
        loop
                    add    c1 to rowcount
                  read   infile,seq;str6,odate,str12
        until over

                unpack odate,mm,dd,yy
                pack   str8,mm,slash,dd,slash,yy
                  prtpage prfile;*p2000:row,*ALIGNMENT=*Left,*font=font12,str6;
                  prtpage prfile;*p3000:row,*ALIGNMENT=*Left,*font=font12,str8;
                unpack str12,Pfulfil,str2
                  prtpage prfile;*p4000:row,*ALIGNMENT=*Left,*font=font12,Pfulfil;
                if (str2 = "  ")
                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,"Not in Print File";
                else
                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str2,*ULOFF,*boldoff;
                endif
          add     eightlpi,row
                  add     "20",row
          if (rowcount = "55")
                    goto page
                  endif
        repeat
        PRTCLOSE prfile
.begin patch 2.01
           if         (misscnt > c0)          .did we find any?
           move      c3,MailType         .not html

.end patch 2.01
..PATCH 1.71 
..begin patch 1.90
.Begin patch 2.00
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.          pack      APIFileName,STR45,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
.          pause     "2"
.          erase     str45          
.end patch 2.00
.end patch 1.90

..begin patch 1.80


.
          Move      "Creques@nincal.com",MailFrom
          Move      "NOTICATION of Missing InfoGroup Orders",MailSubjct

          Move      "listfulfill@infogroup.com,Emily.Duryea@infousa.com,Cheryl.Achee@infogroup.com",MailTo
.          Move      "Matt.Ringen@infogroup.com,Emily.Duryea@infousa.com,Cheryl.Achee@infogroup.com",MailTo
.;          Move      "Matt.Ringen@infogroup.com,Emily.Duryea@infousa.com,Linda.Lemons@infoUSA.com,Cheryl.Achee@infogroup.com",MailTo
.          Move      "ReubenHolland@nincal.com,KrsniWatkins@nincal.com,CReques@nincal.com",MailCC
.          Move      "RobbWhiting@nincal.com,KrsniWatkins@nincal.com,CReques@nincal.com",MailCC
          Move      "RobbWhiting@nincal.com,ListOrders@nincal.com,CReques@nincal.com",MailCC
          Move      Str55,MailBody
          MOve      Str55,MailAttach
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,Mailattach,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO

          pause     "10"


          call      SendMail
          pause     "10"
.begin patch 2.01
           endif
.end patch 2.01
          erase     MailAttach


.                    cALL                getwinver
.                    If                  (osflag = c1 | osflag = c5)
..Start patch 1.78
.         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ","C:\work\faxfile.prn \\NINs2\Laser8 "
..end patch 1.78
.         Execute   TASKNAME
..         else
.                    ElseIf              (osflag = c3 | osflag = c4)
.         PACK      TASKNAME,"c:\command.com /c copy ","c:\work\faxfile.prn \\NINs2\Laser8 "
.         EXECUTE   TASKNAME
.                    Elseif              (osflag = c6)
..Start patch 1.78
.         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ","c:\work\faxfile.prn \\NINs2\Laser8 "
..end patch 1.78
.         Execute   TASKNAME
.         endif
.         cmatch    yes to faxflag
.         if         equal
.         if        (osflag = c1 | osflag = C5)
..         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ","c:\work\faxfile.prn \\nts3\fax "
.         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ","c:\work\faxfile.prn \\srv2008a\fax "
.         Execute   TASKNAME
.         Elseif        (osflag = c3 | osflag = C4)
.         PACK      TASKNAME,"c:\command.com /c copy ","c:\work\faxfile.prn \\srv2008a\fax "
.         EXECUTE   TASKNAME
.         Elseif        (osflag = c6)
.         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy /b ","c:\work\faxfile.prn \\srv2008a\fax "
.         Execute   TASKNAME
.                              endif
.                              endif
.end patch 1.80
.PATCH 1.71 

.========================================================================================

.          call        OrderSetMouseFree
.         alert     note,"Job is Done!!!!!!!",result
.         move      c0 to clsflg
.         return
.begin patch 1.80
.begin patch 2.00
.          call      pdf995auto0
.end patch 2.00
.end patch 1.80

         STOP
.Begin patch 1.80
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Failed InfoGroup PDF - ",str25
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    Stop
                    endif
          
                    goto      checkfile
.end patch 1.80

         include   nordio.inc
         include   nownio.inc
.START PATCH 1.72 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfulio.inc
          include compio.inc
          include cntio.inc
..END PATCH 1.5 ADDED LOGIC
.START PATCH 1.72 REPLACED LOGIC
 
        INCLUDE   COMLOGIC.inc


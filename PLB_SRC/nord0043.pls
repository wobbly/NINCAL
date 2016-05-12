PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.56 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfuldd.inc
          include compdd.inc
          include cntdd.inc
..END PATCH 1.5 ADDED LOGIC
.END PATCH 1.56 REPLACED LOGIC
.begin patch 2.0
           include    nshpdd.inc
.end patch 2.0
.
RELEASE   INIT           "2.00"           DLH     if we have shipping data we should be ok, if rapid return and already billed =ok.
Reldate   Init      "2015 February 9"
.RELEASE   INIT           "1.70"           DLH     suppress printing, Put In Email
.Reldate   Init      "29 August 12"
.RELEASE   INIT           "1.60"           DLH     Add OSflag for win 7
.Reldate   Init      "06 April 11"
.RELEASE   INIT           "1.59"           DLH     more files to Data manager update starting lr
.Reldate   Init      "20 Feb 09"
.RELEASE   INIT          "1.58"           JD   05Feb06 New PL exch fee list
.RELEASE   INIT          "1.57"           DMD  12OCT06 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.REldate  INit      "05 Feb 08"
.RELEASE   INIT          "1.56"           DMS  22JUN06 FULFILLMENT CONVERSION
.RELEASE   INIT      "1.55"       JD    08Dec05  Added new Ship-to number for Donnelley/InfoUSA
.RELEASE   INIT      "1.54"               ASH  10NOV05  Added new Ship-to number for Donnelley/InfoUSA
.RELEASE  INIT       "1.53"       DMB  18JUN2005  FM IP CHG
.RELEASE  INIT       "1.52"       ASH  24JAN2005 Updated PRTOPEN
.RELEASE  INIT       "1.51"       JD   06JAN2005 Updated starting LR
.RELEASE  INIT       "1.5"        ASH   05FEB2002  NINFUL CONVERSION
.RELEASE  INIT       "1.4"        DMB   03APR2002  Changed format to non gui in order to automate
.RELEASE  INIT       "1.3"        ASH   01FEB2002  ADDED TDMCORD.DAT TO FILE MANAGER
.RELEASE  INIT       "1.2"         DB   02JAN01  Add  code to omit NINCA Mlr and List also added prepare for failed
.                                               added code to print out if fulfillment or shipto
.                                                           also excludes "for billing purposes lists
.RELEASE  INIT      "1.1"       DB12DEC01  Add code to omit running charge Lr's
.Initial RELEASE of Gui Missing Triplex Orders
.RELEASE  INIT      "1.0"       JD27mar97  Missing Triplex Orders
LIVETC   IFILE     KEYLEN=6,VAR=7
HOTORDS  IFILE     KEYLEN=7
TDMCORD  IFILE     KEYLEN=6,VAR=7
INFILE   FILE
tdmcout2 ifile     KEYLEN=6,var=21
TDMCOUT  IFILE     KEYLEN=6,VAR=7
NINPRT1  IFILE     KEYLEN=6,FIXED=408,NODUPLICATES
prfile   pfile
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      10
MISSCNT  FORM      10
LR       DIM       6
check    form      5
check2   form      5
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
.
         MOVE      "NORD0043" to PROGRAM
         MOVE      "Names in the News" TO COMPNME
.         CALL      PAINT
.         OPEN      INFILE,"c:\work\ninprin2.cop"

.START PATCH 1.3 REPLACED LOGIC
.         open      LIVETC,"tdmcord"
.tdmcord.dat file
.>Patch 1.53
.         open      LIVETC,"tdmcord.isi|20.20.30.103:502"
         open      LIVETC,"tdmcord.isi|NINS1:502"
.>Patch 1.53
.END PATCH 1.3 REPLACED LOGIC
.hotorder file
.         open      hotords,"hotorders"
.begin patch 1.59
         open      hotords,"hotorders.isi|NINS1:502"
.tdmcord.sav file
.         OPEN      TDMCOUT,"tdmcords"
         OPEN      TDMCOUT,"tdmcords.isi|NINS1:502"
.end patch 1.59
         prepare   tdmcout2,"\\nins1\e\data\FAILED.dat":
                            "\\nins1\e\data\FAILED","6","21"
.         OPEN      INFILE,"\\nins1\e\data\FAILED"
         open      tdmcout2,"\\nins1\e\data\FAILED"
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        pack    str35,NTWKPATH7,"NINPRINT"
        OPEN    NINPRT1,str35,READ
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
        move    "NORD0043",Wprognme
        move    "Check for INfoGroup Ords",Wfunction
        move    "Jose Duenas",Wauthor
          Move      Release,Wrelease
          move      Reldate,Wreldate
        call     paint

.set date for previous month
          clock    timestamp,str8
        unpack   str8,str2,yy,mm,dd
        move     mm to n2
        sub          c2 from n2
.;Feb
        if       (n2 = c0)
                 move "12" to mm
                 clear n2
                 move yy to n2
                 sub c1 from n2,yr
                 move yr to yy
                 rep zfill,yy
.        call     zfillit using yy
        endif
.;Jan
        if       (n2 = seq)
                 move "11" to mm
                 clear n2
                 move yy to n2
                 sub c1 from n2,yr
                 move yr to yy
                 rep zfill,yy
.                 call zfillit using yy
        endif

.====================================================================================
Start
        clear readcnt
        clear misscnt
.=====================================================================================
         call      cvtjul
         move      c1 to clsflg
         move      juldays to check2
.begin patch 1.51
.        MOVE      "440000" TO NORDFLD
.         MOVE      "410000" TO NORDFLD
.begin patch 1.59
.         MOVE      "500000" TO NORDFLD
.         MOVE      "550000" TO NORDFLD
.**********.686400 FIRST NIN LR OF 2010
.         MOVE      "767000" TO NORDFLD
         MOVE      "780000" TO NORDFLD
.end patch 1.51
.end patch 1.59
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
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         pack      odate with mm,dd,yy
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
. Flag if live order 1 day past current date.
         compare   check to check2
         goto      read if not less
.begin patch 2.0
           packkey    nshpfld,olrn
           call       nshpkey
           goto       read if not over
           if         (ostat = "B" & OLON = "7202")         billed and rapid returns lo skip
           goto       read
           endif

.end patch 2.0




         move      olon to nownfld
         call      nownkey
.START PATCH 1.5 REPLACED LOGIC
.         RESET     OWNCTN
.         SCAN      "TDMC" IN OWNCTN          *TRIPLEX CCTO?
.         IF        EQUAL
.                   RESET     RUNCODES
.                   SCAN      OLNUM IN RUNCODES
.                   IF NOT EQUAL
.                        move      "fulfill" to fulfil
.                             GOTO      WRITDMC
.                   endif
.         endif
..         goto      writdmc if equal
.         RESET     OWNCTN
.         SCAN      "TRIPLEX" IN OWNCTN          *TRIPLEX CCTO?
.         IF        EQUAL
.                   RESET     RUNCODES
.                   SCAN      OLNUM IN RUNCODES
.                   IF NOT EQUAL
.                        move      "fulfill" to fulfil
.                             GOTO      WRITDMC
.                   endif
.         endif
.Start Patch 1.57 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
.End Patch 1.57 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                    
.START PATCH 1.56 REPLACED LOGIC
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
.Start Patch 1.57 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.   
.         if (OWNCTN <> "")
.                   pack      COMPFLD6,OWNCTN
.                   rep       zfill,COMPFLD6
.                   move      C1,COMPPATH
.                   move      "READ-COMPKEY6",Location
.                   pack      KeyLocation,COMPFLD6
.                   call      COMPKEY6
.                   if over
.                             clear     COMPFLD6
.                             clear     COMPCOMP
.                   else
.                             if (COMPSVBFLG <> "T")
.                                       clear     COMPFLD6
.                                       clear     COMPCOMP
.                             endif
.                   endif
.         else      // OWNCTN = ""
.                   clear     COMPFLD6
.                   clear     COMPCOMP
.         endif
.         if (COMPFLD6 = "0026")
.                   reset     RUNCODES
.                   scan      OLNUM,RUNCODES
.                   if not equal
.                             move      "fulfill",fulfil
.                             goto writdmc
.                   endif
.         else
.                   scan      "TDMC",COMPCOMP
.                   if equal
.                             reset     RUNCODES
.                             scan      OLNUM,RUNCODES
.                             if not equal
.                                       move      "fulfill",fulfil
.                                       goto writdmc
.                             endif
.                   else
.                             reset     COMPCOMP
.                             scan      "TRIPLEX",COMPCOMP
..END PATCH 1.56 REPLACED LOGIC
.                             if equal
.                                       reset     RUNCODES
.                                       scan      OLNUM,RUNCODES
.                                       if not equal
.                                                 move      "fulfill",fulfil
.                                                 goto writdmc
.                                       endif
.                             endif
.                   endif
.         endif
.End Patch 1.57 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.               
.Start Patch 1.57 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
.         if (OFULLFIL <> "")
.                   pack      COMPFLD,OFULLFIL
.                   call      zfillit using COMPFLD
.                   move      C1,COMPPATH
.                   move      "READ-COMPKEY",Location
.                   pack      KeyLocation,COMPFLD
.                   call      COMPKEY
.                   if over
.                             clear     COMPFLD
.                             clear     COMPCOMP
.                   endif
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
.End Patch 1.57 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                    
.END PATCH 1.4 REPLACED LOGIC
.         goto      writdmc if equal
.START PATCH 1.54 REPLACED LOGIC
.         MATCH     "0040",ORTNNUM |"5224",ortnnum
.         IF        EQUAL
.         if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.         if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 1.54 REPLACED LOGIC
.start PATCH 1.55 REPLACED LOGIC
         if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.END PATCH 1.55 REPLACED LOGIC
          RESET     RUNCODES
          SCAN      OLNUM IN RUNCODES
          IF NOT EQUAL
          move      "ship" to fulfil
                    GOTO      WRITDMC
          endif
         endif
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
.START PATCH 1.58
         MATCH     "024593",OLNUM
         goto      read if equal
.END PATCH 1.58
.         clear     b2
.         PACK     b2 from OSALES10,OSALES


.         if        (grpflg = c6)
.                 match     "06" to b2              *list management order?
.                  goto      read if not equal
.         endif
.         if        (grpflg = c5)
.                  match     "06" to b2              *list management order?
.                  goto      read if equal
.         endif
         move      olrn to key
.         FILEPI    1;TDMCOUT
         read      tdmcout,olrn;;
         goto      read if not over
.===============================================================================
.Check New hotorder file

.         FILEPI    2;HOTORDS
         move      key to olrn
         read      HOTORDS,olrn;;
         goto      read if not over
.===============================================================================
.Check LiveTC file

.         FILEPI    2;LIVETC
         move      key to olrn
         read      LIVETC,olrn;;
         goto      read if not over
.===============================================================================
.===============================================================================
.Check PrintFile

         clear     str2
.         FILEPI    2;NINPRT1
         move      key to olrn
         read      NINPRT1,olrn;str2
         IF        (str2 = "S0")
                   move "*" to liveflg
.                   goto read
         else
                   move str2 to liveflg
.                  clear liveflg
         Endif
.===============================================================================
         FILEPI    4;TDMCOUT2
         move      key to olrn
         read      tdmcout2,olrn;;
         goto      read if not over
writelr    WRITE     TDMCOUT2,key;key,odate,fulfil,liveflg
         add       c1 to misscnt
         DISPLAY   *P10:16,"NUMBER OF MISSING ORDERS : ",MISSCNT
.         move      misscnt to str12
.         setitem   Nord0043statmiss,0,str12
         GOTO      READ

.FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1
.FileGo1
.        call click_NORD0043Exit
.        RETURN
.Optionsgo
.        return
.EditGo
.        return
.HelpGo
.        setprop AboutMssg,visible=1
.        return

.OrderSetMouseBusy
.        setmode *mcursor=*wait
.        return
.OrderSetMouseFree
.        setmode *mcursor=*arrow
.        return

EOJ
.===========================================================
.START PATCH 1.52 REPLACED LOGIC
.        getinfo  system,str6
.        unpack   str6 into str1,str2
.        unpack   str2 into str1
.        move     c0 to osflag
...0 = unknown
...1 = Windows NT
...2 = WIN32s Windows 3.1x (obsolete)
...3 = Window 95
...4 = Window 98
...5 = Windows 2000
...8 = Windows CE
.        if (str1 = "3" or str1 = "4")
.                 move     c1 to osflag
.        endif
.        if (str1 = "1" or str1 = "5")
.         move     c2 to osflag
.        endif
..==================================================================
..===============================================================================
..         CLOSE     INFILE
.        if (osflag = c2)
.         PRTOPEN prfile,"\\NTS0\Laser8","Failed.dat"
.        else
.         PRTOPEN prfile,"Laser8","Failed.dat"
.        endif
.............................................
         OPEN      INFILE,"\\nins1\e\data\FAILED"
          call      GetWinVer
.          if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
.                    PRTOPEN   prfile,"\\NINs2\Laser8","Failed.dat"
.          elseif (osflag = c3 | osflag =c4)         .win 95 98
.                    PRTOPEN   prfile,"Laser8","Failed.dat"
.          elseif (osflag = "9" )         .win 7
.                    PRTOPEN   prfile,"\\NINs2\Laser8","Failed.dat"
.          else   .(osflag = c0)         .Don't know prompt for printer
.                    PRTOPEN   prfile,"","A"
.          endif
.END PATCH 1.52 REPLACED LOGIC
.=================================================================================
Page
.        prtpage   prfile;*NEWPAGE:
.                   *UNITS=*HIENGLISH;
        clear     rowcount
        clear     row
        move      "300",row
.======================================================================

.        if        (grpflg = c5)
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Brokerage",*ULOFF,*boldoff;
.        endif
.        if        (grpflg = c6)
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"List Management",*ULOFF,*boldoff;
.        endif
        clock timestamp,str16
        unpack str16,str8,str6
        unpack str8,str4,mm,dd
        pack   str10,mm,slash,dd,slash,str4
        clear  str8
        unpack str6,hh,mn,ss
        pack   str8,hh,colon,mn,colon,ss
        pack   str24,str10,b4,str8
.        prtpage prfile;*p6100:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str24,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
.        if        (grpflg = c7)
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Brokerage and List Management",*ULOFF,*boldoff;
.        endif
        add     eightlpi,row
        add     "60",row
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Missing Triplex Orders",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"LR",*ULOFF,*boldoff;
.        prtpage prfile;*p3000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Order Date",*ULOFF,*boldoff;
.        prtpage prfile;*p4000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Ful/Ship",*ULOFF,*boldoff;
.        prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Status",*ULOFF,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     "20",row
          Move      "Missing InfoGroup Orders - Nord0043",mailsubjct
          append    "Brokerage and List Management",mailbody
          append    CRLF,mailbody
          append    "Missing InfoGroup Orders",Mailbody
          append    CRLF,mailbody
          append    "LR       Order Date          Ful/Ship            Status",Mailbody
          append    CRLF,mailbody
          append    CRLF,mailbody
          
        loop
                    add    c1 to rowcount
                  read   infile,seq;str6,odate,str12
        until over
          append    str6,mailbody
          append    b10,mailbody
          
                unpack odate,mm,dd,yy
                pack   str8,mm,slash,dd,slash,yy
.                  prtpage prfile;*p2000:row,*ALIGNMENT=*Left,*font=font12,str6;
.                  prtpage prfile;*p3000:row,*ALIGNMENT=*Left,*font=font12,str8;
          append    str8,mailbody
          append    b10,mailbody
                unpack str12,Pfulfil,str2
          append    Pfulfil,Mailbody
          append    b10,mailbody
.                  prtpage prfile;*p4000:row,*ALIGNMENT=*Left,*font=font12,Pfulfil;
                if (str2 = "  ")
.                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,"Not in Print File";
          append    "Not in Print File",Mailbody
                else
.                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str2,*ULOFF,*boldoff;
          append    str2,mailbody
                endif
          append    CRLF,mailbody
          add     eightlpi,row
                  add     "20",row
          if (rowcount = "55")
                    goto page
                  endif
        repeat
          if        (misscnt > 0)
          move      "computerRequest@nincal.com",mailfrom
          move      "computerRequest@nincal.com",mailTo
          reset     Mailbody
          move      c3,MailType         .not html
          call      sendmail
          endif
.        PRTCLOSE prfile

.========================================================================================

.          call        OrderSetMouseFree
.         alert     note,"Job is Done!!!!!!!",result
.         move      c0 to clsflg
.         return
         STOP

         include   nordio.inc
         include   nownio.inc
.START PATCH 1.56 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfulio.inc
..END PATCH 1.5 ADDED LOGIC
          include compio.inc
          include cntio.inc
.END PATCH 1.56 REPLACED LOGIC
.begin patch 2.0
           include    nshpio.inc
.end patch 2.0

        INCLUDE   COMLOGIC.inc


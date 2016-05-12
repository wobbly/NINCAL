PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.55 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfuldd.inc
          include compdd.inc
          include cntdd.inc
..END PATCH 1.5 ADDED LOGIC
.END PATCH 1.55 REPLACED LOGIC
.
RELEASE   INIT           "1.62"            RVW     Update e-mail message
Reldate   Init      "2014 April 15"
.RELEASE   INIT           "1.61"           DLH     Update management fee exclusion
.Reldate   Init      "2014 January 6"
.RELEASE   INIT           "1.60"           DLH     suppress printing Put In Email
.Reldate   Init      "29 August 12"
.release    init         "1.56"             JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Reldate   Init      "13 Oct 2006"
.RELEASE  INIT         "1.55"       DMS  22JUN06  FULFILLMENT CONVERSION        // 1.55 seems to be 1st avail patch num
.RELEASE  INIT      "1.0"       JD05Dec05  Missing Triplex Orders/Weekly pass
LIVETC   IFILE     KEYLEN=6,VAR=7
HOTORDS  IFILE     KEYLEN=7
TDMCORD  IFILE     KEYLEN=6,VAR=7
Weekly    FILE
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
;osflag   form      1             . 1=win 95,98, 2=NT
Fulfil   DIM       6
Pfulfil  DIM       6
Liveflg  dim       2
YR       FORM      2
.
         MOVE      "NORD0046" to PROGRAM
         MOVE      "Names in the News" TO COMPNME
.         CALL      PAINT

.START PATCH 1.3 REPLACED LOGIC
.         open      LIVETC,"tdmcord"
.tdmcord.dat file
.>Patch 1.53
.         open      LIVETC,"tdmcord.isi|20.20.30.103:502"
         open      LIVETC,"tdmcord.isi|10.10.30.103:502"
.>Patch 1.53
.END PATCH 1.3 REPLACED LOGIC
.hotorder file
         open      hotords,"hotorders"
.tdmcord.sav file
         OPEN      TDMCOUT,"tdmcords"
         prepare   tdmcout2,"\\nins1\e\data\FAILED.dat":
                            "\\nins1\e\data\FAILED","6","21"
         OPEN      INFILE,"\\nins1\e\data\FAILED"
         OPEN      WEEKLY,"\\nins1\e\data\diskweek"
         open      tdmcout2,"\\nins1\e\data\FAILED"
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        pack    str35,NTWKPATH7,"NINPRINT"
        OPEN    NINPRT1,str35,READ
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         MOVE      C1 TO NORDPATH        .SET ACCESS TO ISI.
.====================================================================================
..............................................................
.====================================================================================
        move    "NORD0046.PLS",Wprognme
        move    "Check for Triplex Ords",Wfunction
        move    "Jose Duenas",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate
        call     paint
.====================================================================================
Start
        clear readcnt
        clear misscnt
.==================================================
.        getprop TRIORDRadioALL,SELGROUPID=grpflg         5- Brokerage 6-list mgnt 7-ALL
.==================================================
read
         read      Weekly,seq;ordvars
         GOTO      EOJ IF OVER
.
         ADD       ONE TO READCNT
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
         if ((ostat = "0")|(ostat = "B"))
         else
                goto read
         endif
         if (oqty = "000000000")
                goto read
         endif
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
.         call      Trim using OWNCTN
.START PATCH 1.55 REPLACED LOGIC
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
;.                  pack      COMPFLD6,OWNCTN
;.                  rep       zfill,COMPFLD6
;.                  move      C1,COMPPATH
;.                  move      "READ-COMPKEY6",Location
;.                  pack      KeyLocation,COMPFLD6
;.                  call      COMPKEY6
;.                            if over
;.                                      clear     COMPFLD6
;.                                      clear     COMPCOMP
;.                            else
;.                                      if (COMPSVBFLG <> "T")
;.                                                clear     COMPFLD6
;.                                                clear     COMPCOMP
;.                                      endif
;.                            endif
;.        else      // OWNCTN = ""
;.                  clear     COMPFLD6
;.                  clear     COMPCOMP
;.        endif
;.        if (COMPFLD6 = "0026")
;.                  reset     RUNCODES
;.                  scan      OLNUM,RUNCODES
;.                  if not equal
;.                            move      "fulfill",fulfil
;.                            goto writdmc
;.                  endif
;.        else
;.                  scan      "TDMC",COMPCOMP
;.                  if equal
;.                            reset     RUNCODES
;.                            scan      OLNUM,RUNCODES
;.                            if not equal
;.                                      move      "fulfill",fulfil
;.                                      goto writdmc
;.                            endif
;.                  else
;.                            reset     COMPCOMP
;.                            scan      "TRIPLEX",COMPCOMP
.END PATCH 1.55 REPLACED LOGIC
;.                            if equal
;.                                      reset     RUNCODES
;.                                      scan      OLNUM,RUNCODES
;.                                      if not equal
;.                                                move      "fulfill",fulfil
;.                                                goto writdmc
;.                                      endif
;.                            endif
;.                  endif
.Start Patch 1.56 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
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
.END PATCH 1.4 REPLACED LOGIC
.         goto      writdmc if equal
.START PATCH 1.54 REPLACED LOGIC
.         MATCH     "0040",ORTNNUM |"5224",ortnnum
.         IF        EQUAL
.         if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
         if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319" )
.END PATCH 1.54 REPLACED LOGIC
          RESET     RUNCODES
          SCAN      OLNUM IN RUNCODES
          IF NOT EQUAL
                        move      "ship" to fulfil
                    GOTO      WRITDMC
          endif
         endif
         goto      read

WRITDMC
.begin patch 1.61
          reset     EXFEELST
          scan      OLNUM,EXFEELST
          goto      Read if equal

         MATCH     "017717",OLNUM
         goto      read if equal
.         MATCH     "018710",OLNUM
.         goto      read if equal
.end patch 1.61
         MATCH     "0002",OMLRNUM
         goto      read if equal
         MATCH     "0001",ORTNNUM
         goto      read if equal
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
         FILEPI    1;TDMCOUT
         read      tdmcout,olrn;;
         goto      read if not over
.===============================================================================
.Check New hotorder file

         FILEPI    2;HOTORDS
         move      key to olrn
         read      HOTORDS,olrn;;
         goto      read if not over
.===============================================================================
.Check LiveTC file

         FILEPI    2;LIVETC
         move      key to olrn
         read      LIVETC,olrn;;
         goto      read if not over
.===============================================================================
.===============================================================================
.Check PrintFile

         clear     str2
         FILEPI    2;NINPRT1
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
         GOTO      READ

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
.         PRTOPEN prfile,"\\NINs2\Laser8","Failed.dat"
.        else
.         PRTOPEN prfile,"Laser8","Failed.dat"
.        endif
.............................................
          call      GetWinVer
.          trap      SPoolErr if Spool giving error
.          if (osflag = c1 | osflag = c5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt,2k,xp,vista,win 7
.                    PRTOPEN   prfile,"\\NINs2\laser8","Failed.dat"
.          elseif (osflag = c3 | osflag =c4)         .win 95 98
.                    PRTOPEN   prfile,"Laser3","Failed.dat"
.          else   .                           .Don't know prompt for printer
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
.       prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Brokerage and List Management",*ULOFF,*boldoff;
.        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Weekly Brokerage and List Management",*ULOFF,*boldoff;
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
.Begin Patch 1.62
          Move      "Orders not reported received by Triplex and appear already processed by NINCAL",mailsubjct
.End Patch 1.62          
          append    "Nord0046 - Report",mailbody
          append    CRLF,mailbody
          append    "Weekly Brokerage and List Management",mailbody
          append    CRLF,mailbody
.Begin Patch 1.62
          append    "These orders were able to be found in the Triplex log file",Mailbody
          append    CRLF,mailbody
          append    "or in NINCAL's processing queue",mailbody
.End Patch 1.62     
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
          append    "Not in Print File",Mailbody
.                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,"Not in Print File";
                else
          append    str2,mailbody
.                            prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str2,*ULOFF,*boldoff;
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
          call      sendmail
          endif
.        PRTCLOSE prfile

.========================================================================================

.          call        OrderSetMouseFree
.         alert     note,"Job is Done!!!!!!!",result
.         move      c0 to clsflg
.         return
         STOP
SpoolErr
          trapclr   Spool
          Trap      Spoolerr if Spool Giving Error
          PRTOPEN   prfile,"","A"
          trapclr   Spool
          return

         include   nordio.inc
         include   nownio.inc
.START PATCH 1.55 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         include   nfulio.inc
          include compio.inc
          include cntio.inc
..END PATCH 1.5 ADDED LOGIC
.END PATCH 1.55 REPLACED LOGIC
        INCLUDE   COMLOGIC.inc


.NAMES IN THE NEWS CALIF. TRIPLEX TO NINCAL MERGE UPDATE

PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   hp.inc
         include   norddd.inc
.patch2.12
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
.patch2.12
         INCLUDE   NMRGDD.inc
         include   ninvdd.inc
.begin patch 1.8
.begin patch 1.9
.         include   contact1.inc
         include    ncntdd.inc
.end patch 1.9
         include   winapi.inc
.end patch 1.8
Release   init        "2.20"  DLH  make sure valid order before writing to database
RElDate   Init      "22 April 2008"

.Release  init        "2.13"                      JD  27DEC2005  New Triplex/Donnelley format.
.Release  init        "2.12"        DMB 26MAY2004 Mailer Conversion
.release  init      "2.11"          DMB 22Mar2004 turned off email code.
.release  init      "2.1"          DMB 18Mar2004 Added code to separate pages for mailers and to send email to users from acct.
.release  init      "2.00"          DMB 30Dec2003 Added code to print out order/merge qty variance
.release  init      "1.99"          DMB 18Dec2003 Added code to trim 15 byte var to 10 for mlr key code var
.release  init      "1.98"          DMB 12Jun2003 Added code to skip email if orders were cancelled or zero qty
.release  init      "1.97"          06Jun2003 send email to info services instead of user.
.release  init      "1.96"         12Jul2002 DLH added GetWinVer
.release  init      "1.95"         07MAR2002 JD added duplex print to report
.release  init      "1.94"        31JAN2002 ASH NEW SERVER ADDED
.release  init      "1.93"        02OCT2000 ASH NEW SERVER ADDED
.release  init      "1.92"        01may00 ASH REPLACED EMAIL LOGIC
.release  init      "1.91"        28Feb00 JD using trim instead of rtrim doesn't destroy str1.
.release  init      "1.9"        04Feb00 DLH replace contact1.inc with ncntdd & io .inc
.release  init      "1.8"        21Dec99 DLH add variance notification
.release  init      "1.7"        JS22Jun99 NT copy
.release  init      "1.6"        JDnov2898 print for mcomp
.release  init      "1.5"       DLH 19Jan96  do not update if already billed
.RELEASE  INIT      "1.4"         jd 24aug95 new variable.
.RELEASE  INIT      "1.3"        jd  11oct94  added print of total recs applied.
.RELEASE  INIT      "1.2"       JD   02FEB94 CHANGE FILE FORMAT,ADDED NEW RECORDS.
.RELEASE  INIT      "1.1"       JD  22SEP93 added print for incoming records.
.RELEASE  INIT      "1.0"      DLH 06JAN93
.patch1.99
LASER     PFILE
PrintFile dim       200
                                        Call PrintOpen
.         PRTOPEN LASER,"\\NTS0\Laser2","\\nins1\e\data\NMRGINF.lst"
mask10      init    "ZZ,ZZZ,ZZ9"         ;formatting vars
Dim10a      dim     10            ;formatting vars
Dim10ab      dim     10           ;formatting vars
mask11      init    "ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim11a      dim     11            ;formatting vars
mask13      init    "Z,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim13a      dim     13            ;formatting vars
mask12      init    "(ZZ,ZZZ,ZZ9)"         ;formatting vars
Dim12c      dim     12            ;formatting vars
.patch1.99
IN         FORM        5
APPLIED    FORM        5
first    init      "Y"
eop      form      "58"
lines    form      2
page     form      2
NUM      FORM      1
TDMCLR   DIM       10
INPUT    FILE      var=441
skipped  file      var=441
skipcnt  form      5
.begin patch 1.8
DIFF     FORM      10
fivePER  FORM      10
last     form      3
.end patch 1.8
.patch1.99
IRQTY     DIM       1
CHGQTY    FORM      8
.patch1.99
.Patch2.1
HOLDMLR   DIM       4
.Patch2.1
.START PATCH 2.13 ADDED LOGIC
newflag   init      "N"
.END PATCH 2.13 ADDED LOGIC
           MOVE      "Names In The News Ca." TO COMPNME
           MOVE      "NMRG0003" TO PROGRAM
           MOVE      "APPLY INFOGRP MERGE INFO" TO STITLE
           CLOCK    DATE TO TODAY
           CALL      PAINT
.START PATCH 1.93 REPLACED LOGIC
.         splopen   "g:\data\nmrginf.lst"
.         open      skipped,"g:\data\text\mrgeskip.ped"
.patch1.99 comment out
.         PACK      STR35,NTWKPATH1,"NMRGINF.LST"
         PACK      STR45,NTWKPATH1,"text\mrgeskip.ped"
.         splopen   STR35
.patch1.99
         open      skipped,STR45
.END PATCH 1.93 REPLACED LOGIC
         move       c1 to nordpath
         move       c1 to ninvpath
.Patch2.1
         call       header
.Patch2.1
         MOVE       NO TO STR1
         KEYIN     *P10:12,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*T20,STR1
         REPLACE   "Y1N2" IN STR1
         MOVE      STR1,NUM
.patch1.99
Qty
           MOVE     "I" to str1
         KEYIN     *P10:12,"APPLY (I)nput Qty or (R)eceived Qty ?? ",*uc,*rv,*T20,STR1
          If        ((str1 <> "I") & (str1 <> "R"))
                    goto qty
          else
          MOVE      STR1,IRQTY
          endif
.patch1.99
           OPEN      INPUT,"TDMCMRGE",EXCLUSIVE
LOOP
.patch2.13
             cmatch    yes to newflag
         goto      input2 if equal
.patch2.13
           READ          INPUT,SEQ;TDMCLR,str1:
                              NMRGLNAM,b2:
.patch2.0
.                              NMRGKCOD,str1:
                              str15,str1:
.patch2.0
                                                                                NMRGRQTY,str1:
                              NMRGIQTY,str1:
                              NMRGTREJ,str1:
                              NMRGID,str1:
                              NMRGNETI,str1:
                              NMRGELIM,str1:
                              NMRGHDRP,str1:
                              NMRGCS,str1:
                              NMRGUDUP,str1:
                              NMRGND,str1:
                              NMRGDUPM,str1:
                              NMRGNET,str1:
                              NMRGZIPV,str1:
                              NMRGZIPC,str1:
                              NMRGZIP4,str1:
                              NCOAMWF,str1:
                              NCOAMNF,str1:
                              NCOATOTM,str1:
                              NIXIEM,str1:
                              NCOAUNM,str1:
                              NCOANFRJ,str1:
                              NCOANIX1,str1:
                              NCOANIX2,str1:
                              NCOANIX3,str1:
                              NMRGERR,str1:
                              NMRGDISF,str1:
                              NMRGNPER,str1:
                              NMRGDMA,str1:
                              NMRGELMX,str1:
                              NMRGZ4,str1:
                              NMRGNIX,str1:
                              NMRGTDMC,str1:
                              NMRGPRIS,str1:
                              NMRGDROP,str1:
                              NCOAREJ,str1:
                              NMRGCUST,str1:
                              NMRGFAM,str1:
                              NMRGHH,str1:
                              str8:               *FAMILY DUPE DROPS/DUP FIELD.
                        str1:
                              nmrgrep:           new field 8/24/95. (DNC)
                              str1:
                              nmrgnnet            new field 8/24/95. (DNC)
           GOTO        STOP IF OVER
.
.patch2.13
.                               goto               process
.
.
                                scan    "*****",TDMCLR
                                if equal
                                move        yes to newflag
                                goto        loop
                                endif
                                goto               process
.
INPUT2
.                             READ          INPUT,SEQ;TDMCLR,str3:
.                              NMRGLNAM,b2:
.                              str55,str1:
.                                   NMRGRQTY:
.                              NMRGIQTY:
.                              NMRGID:
..                              NMRGFam:
.                              NMRGELIM:
.                              NMRGCS,str1:
.                              NMRGUDUP,str1:
.                              NMRGNET,str1:
.                              NCOAMNF,str1:
.                              NMRGERR,str1:
.                              NMRGDISF,str1:
.                              NMRGNPER,str1:
.                              NMRGDMA,str1:
.                              NMRGELMX,str1:
.                              NMRGZ4,str1:
.                              NMRGTDMC,str1:
.                              NMRGPRIS,str1:
.                              NMRGDROP,str1:
..                              NCOAREJ,str1:
.                              NMRGCUST,str1:
.                              NMRGHH,str1

                              READ      INPUT,SEQ;TDMCLR:
                                        NMRGLNAM:
                                        str1:
                                        str55,str18,str1:
                                        NMRGRQTY:
                                        NMRGIQTY:
                                        NMRGID:
                                        NMRGFam:
                                        N8:
                                        NMRGELIM:
                                        NMRGCS:
                                        NMRGUDUP:
                                        NMRGNET:
                                        NCOAMNF:
                                        NMRGERR:
                                        NMRGDISF:
                                        NMRGNPER:
                                        NMRGDMA:
                                        NMRGZ4:
                                        NMRGTDMC:
                                        NMRGPRIS:
                                        NMRGDROP:
                                        NMRGCUST:
                                        N8:
                                        N8:
                                        N8:
                                        N8:
                                        N8:
                                        NMRGHH

           GOTO        STOP IF OVER

Process
.patch2.13
           ADD         C1 TO IN
           DISPLAY   *P10:10,"RECORDS READ : ",IN
.Patch2.1
                              if (IN <> c1)

                   move      tdmclr to nordfld
                move      "Order not found!!" to mcomp
             call      nordkey
                                        if not over
                                                  if (HOLDMLR <> OMLRNUM)
                                                            call Header
                                                            move OMLRNUM to HOLDMLR
                                                  endif
                                        endif
                              else
                   move      tdmclr to nordfld
                move      "Order not found!!" to mcomp
             call      nordkey
                                        move OMLRNUM to HOLDMLR
                              endif
.Patch2.1
.patch2.0
                                        call trim using str15
                                        move str15 to NMRGKCOD
.patch2.0
.patch1.99
                    clear n9
                    clear     n10
                    clear n8
                    clear dim10ab
                    move      NMRGRQTY,n9
                    move      NMRGIQTY,n10
                    if        (n9 <> n10)
.Input Qty
                              move      NMRGRQTY,n8
                            move mask10 to dim10ab
                              edit  n8 to dim10ab
                            prtpage laser;*p4800:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,DIM10ab,*boldoff;
.Received Qty
                              clear n8
                              move      NMRGIQTY,n8
                              move mask10 to dim10a
                              edit  n8 to dim10a
                            prtpage laser;*p5600:row,*font=font12,*ll,*ALIGNMENT=*Left,dim10a;
.Difference Between Received and Input Qty
                              clear n8
                              sub n10 from n9,n8
                            move mask12 to dim12c
                              edit  n8 to dim12c
                    prtpage laser;*p6400:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,DIM12c,*boldoff;
                              if (n8 < c0)
                                    MULT      SEQ BY n8
                              endif
.patch2.0 comment out
.                             if (n8 >= 1000)                      .Skip if over a thousand diff betw received and input qty
.                                       add c1 to skipcnt
.                                       goto skipwrt
.                             endif
.patch2.0
                              if (irqty = "I")
                                        move NMRGRQTY to NMRGIQTY
                              elseif (irqty = "R")
                                        move NMRGIQTY to NMRGRQTY
                              endif
                    else
.Input Qty
                              move      NMRGRQTY,n8
                            move mask10 to dim10ab
                              edit  n8 to dim10ab
                            prtpage laser;*p4800:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,DIM10ab,*boldoff;
                    endif
.patch1.99
           UNPACK    TDMCLR INTO NMRGLR,NMRGFILL
         MATCH     B10 TO NMRGLR
         GOTO      LOOP IF EQUAL
           TYPE      NMRGLR
           GOTO      LOOP IF NOT EQUAL
           MOVE      NMRGLR TO NMRGFLD
           REP       ZFILL IN NMRGFLD
         BRANCH    NUM OF NOCHK,chk
CHK      move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         if        not over
         move      "Previously billed/skip it" to mcomp
         add       c1 to skipcnt
         goto      skipwrt
         endif
.begin patch 2.20         
.NOCHK      CALL        NMRGTST
NOCHK      
          clear     omlrky
          move      nmrglr to nordfld
          call      nordkey
          if        not over

          CALL        NMRGTST

                              IF          OVER
                              CALL        NMRGWRT
                              ADD         C1 TO APPLIED
                              ELSE
                              CALL      NMRGDEL
                              CALL        NMRGWRT
                              ADD         C1 TO APPLIED
                              ENDIF

          PACK      MKEY FROM OMLRNUM,OCOBN
          REP       ZFILL IN MKEY
          CALL      NMLRKEY

          Else                
.         move      nmrglr to nordfld
         move      "Order not found!!" to mcomp
.         clear     omlrky
.         call      nordkey
.         if        not over
.         PACK      MKEY FROM OMLRNUM,OCOBN
.         REP       ZFILL IN MKEY
.         CALL      NMLRKEY
          Endif
.end patch 2.20         
.begin patch 1.8
chkvariance
         MOVE      C0 TO N10
         MOVE      OQTY TO N10
         mult      ".05" by N10
         MOVE      N10 TO fivePER
         MOVE      NMRGRQTY TO N10
         MOVE      OQTY TO DIFF
         SUB       N10 FROM DIFF
         COMPARE   C0 TO DIFF
         CALL      NEG IF LESS
         COMPARE   DIFF TO fivePER
.         CALL      sendnews IF NOT GREATER
.patch2.0
                              clear n9
                              move      NMRGRQTY to n9
                              if ((n9 = c0)|(OSTAT ="X")|(OSTAT ="Q"))
                                        goto det
                              endif
                              if (DIFF >= 1000 | DIFF <= -1000)
.                             if (DIFF >= 1000)
                                        clear dim11a
                          move mask11 to dim11a
                                        move oqty to n9
                                        edit  n9 to dim11a
                          prtpage laser;*p7200:row,*font=font12,*ALIGNMENT=*Left,*boldon,DIM11a,*boldoff;
.                  CALL      sendnews
                              endif
.patch2.0
         goto      det
NEG      MULT      SEQ BY DIFF
         RETURN
sendnews
.patch1.98

                    clear n9
          move      NMRGRQTY to n9
          if ((n9 = c0)|(OSTAT ="X")|(OSTAT ="Q"))
                    return
          endif
.patch2.0 comment out see above if 1000 or more
.patch1.99
.                   clear dim11a
.                 move mask11 to dim11a
.                   move oqty to n9
.                   edit  n9 to dim11a
.                 prtpage laser;*p7200:row,*font=font12,*ALIGNMENT=*Left,*boldon,DIM11a,*boldoff;
.patch1.99
.patch2.0 comment out
.patch1.98
.begin patch 1.9
        move      c1 to ncntpath      .set path to read by contact id#
        move      c3 to ncntlock      .no locks
        move      ococode to ncntfld
.        MOVE      ococode,n3
.
         clear     cntname
         clear     str35
         call      ncntkey
         if        not over
         move      cntname to str35
         endif
.         LOAD      str35 FROM N3 OF ocnt1,ocnt2,ocnt3,ocnt4,ocnt5,ocnt6,ocnt7:
.                   ocnt8,ocnt9,ocnt10,ocnt11,ocnt12,ocnt13,ocnt14,ocnt15,ocnt16,ocnt17
.
.
.         SCAN      "(" IN str35
.         clear      str25
.         MOVEFPTR  str35 TO LAST
.         sub       c1 from last
.         SETLPTR   str35 TO LAST
.         RESET     STR35
.START PATCH 1.94 REPLACED LOGIC
.         clear     str25
.         reset     str35
.         APPEND    str35 TO str25
.
.         reset      str25
.         clear       str1
.         append      str25 to str1        . .1st init
.         reset        str1
.         scan       b1 in str25
.         bump       str25 by 1
..START PATCH 1.92 REPLACED LOGIC
..         clear      str6
..         append     str25 to str6
..         reset      str6
...         call       trim using str6
..         call       rtrim using str6
.         move   str25 to str7
.         call   RemoveChar using str7,B1
.         move   str7,str6
..END PATCH 1.92 REPLACED LOGIC
.         pack       str7 from str1,str6
.         RESET      STR25
..         reset      str35
..         setlptr    str35 to last
..         MOVEFPTR   str35 TO c0
..end patch 1.9
          call      RemoveChar using CNTNAME,B1
.END PATCH 1.94 REPLACED LOGIC
         move       str35 to user
      Move    "This is a Informational e-mail from  the Merge Info program",MailSubjct
          Clear     MailBOdy
          Append    "This is a Informational e-mail from the Merge Info program",Mailbody
          append    CRLF,MailBody
          append    "record## ",MailBody
          append    olrn,MailBody
          append    b1,MailBody
          append    CRLF,MailBody
          Append    " Your above LR had a Merge qty variance:",MailBody
          append    CRLF,MailBody
          append    "Order qty ",MailBody
          append    oqty,MailBody
          append    ", Merge Input qty ",MailBody
          move      NMRGRQTY to str10
          append    str10,MailBody
          append    CRLF,MailBody
          Append              "Please review & correct as necessary.",MailBody
          append    CRLF,MailBody
          Reset     MailBody
          Pack      MailFrom from "Accounting@nincal.com"
          pack      MailTO from         MailFrom,",",User,"@nincal,com"
          Call      SendMail

        winshow
        return

.end patch 1.8
.begin patch 2.20
.         endif
.end patch 2.20         

det
.patch1.99
        prtpage laser;*p100:row,*font=font12,*ll,*ALIGNMENT=*Left,TDMCLR;
        prtpage laser;*p1000:row,*font=font12,*ll,*ALIGNMENT=*Left,mcomp;
.         clear n8
.         move      NMRGRQTY,n8
.         move mask10 to dim10a
.         edit  n8 to dim10a
.        prtpage laser;*p4000:row,*font=font12,*ll,*ALIGNMENT=*Left,dim10a;
        prtpage laser;*p3300:row,*font=font12,*ll,*ALIGNMENT=*Left,omlrky;
.         clear n9
.         clear     n10
.         move      NMRGRQTY,n9
.         move      NMRGIQTY,n10
.         clear     str55
.         if        (n9 <> n10)
.                   clear dim10ab
.                   clear n8
.                   move      NMRGIQTY,n8
.                 move mask10 to dim10ab
.                   edit  n8 to dim10ab
.                 prtpage laser;*p4800:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,DIM10ab,*boldoff;
.                   clear n8
.                   sub n10 from n9,n8
.                 move mask12 to dim12c
.                   edit  n8 to dim12c
.                 prtpage laser;*p5600:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,DIM12c,*boldoff;
....              prtpage laser;*p7500:row,*font=font12,*ALIGNMENT=*Left,*ll,*boldon,"SKIPPED",*boldoff;
.         endif
        add     eightlpi,row
        add     "60",row
.comment out patch1.99
.      PRINT     *1,nmrglr,*08,mcomp,*55,nmrgrqty,*65,omlrky

.         add       c1 to lines
.         compare   eop to lines
          if (row > "9750")
                    call header
          endif
.         call      header if not less
.patch1.99
           DISPLAY   *ef,*P10:14,"RECORDS SKIPPED : ",skipcnt
           DISPLAY   *ef,*P10:12,"RECORDS APPLIED : ",APPLIED
           GOTO      LOOP
HEADER
         ADD       C1 TO PAGE
.=================================================================================
        prtpage   Laser;*NEWPAGE:
                   *UNITS=*HIENGLISH;
.                             *Duplex=2;
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
        prtpage laser;*p6100:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str24,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
        prtpage laser;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"INFOGRP Merge Information",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
        prtpage laser;*p100:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"LR",*ULOFF,*boldoff;
        prtpage laser;*p1000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Mailer",*ULOFF,*boldoff;
        prtpage laser;*p3500:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"M/P",*ULOFF,*boldoff;
        prtpage laser;*p4800:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Inp. Qty",*ULOFF,*boldoff;
        prtpage laser;*p5600:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Rec'd Qty",*ULOFF,*boldoff;
        prtpage laser;*p6400:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Variance",*ULOFF,*boldoff;
        prtpage laser;*p7200:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Order Qty",*ULOFF,*boldoff;
.        prtpage laser;*p7500:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row

.         compare    c1 to page
.         if        equal
.         PRINT     *F,hpdupl,*N
.         PRINT     *1,today,*21,"* * * NEW TDMC MERGE INFO * * *";
.         PRINT     *70,"Page: ",PAGE
.         PRINT     *N
.         PRINT     *1,"LR ##",*08,"MAILER",*54,"QTY",*65,"M/P ##"
.         else
.         PRINT     *F,*N
.         PRINT     *1,today,*21,"* * * NEW TDMC MERGE INFO * * *";
.         PRINT     *70,"Page: ",PAGE
.         PRINT     *N
.         PRINT     *1,"LR ##",*08,"MAILER",*54,"QTY",*65,"M/P ##"
.         endif
.         move       c6 to lines
         RETURN
.
skipwrt  write         skipped,SEQ;TDMCLR,str1:
                              NMRGLNAM,b2:
                              NMRGKCOD,str1:
                              NMRGRQTY,str1:
                              NMRGIQTY,str1:
                              NMRGTREJ,str1:
                              NMRGID,str1:
                              NMRGNETI,str1:
                              NMRGELIM,str1:
                              NMRGHDRP,str1:
                              NMRGCS,str1:
                              NMRGUDUP,str1:
                              NMRGND,str1:
                              NMRGDUPM,str1:
                              NMRGNET,str1:
                              NMRGZIPV,str1:
                              NMRGZIPC,str1:
                              NMRGZIP4,str1:
                              NCOAMWF,str1:
                              NCOAMNF,str1:
                              NCOATOTM,str1:
                              NIXIEM,str1:
                              NCOAUNM,str1:
                              NCOANFRJ,str1:
                              NCOANIX1,str1:
                              NCOANIX2,str1:
                              NCOANIX3,str1:
                              NMRGERR,str1:
                              NMRGDISF,str1:
                              NMRGNPER,str1:
                              NMRGDMA,str1:
                              NMRGELMX,str1:
                              NMRGZ4,str1:
                              NMRGNIX,str1:
                              NMRGTDMC,str1:
                              NMRGPRIS,str1:
                              NMRGDROP,str1:
                              NCOAREJ,str1:
                              NMRGCUST,str1:
                              NMRGFAM,str1:
                              NMRGHH,str1:
                              str8:               *FAMILY DUPE DROPS/DUP FIELD.
                        str1:
                              nmrgrep:           new field 8/24/95. (DNC)
                              str1:
                              nmrgnnet:            new field 8/24/95. (DNC)
                                                                                                    nmrgdpv:             new field 6/25/04 PIDI
                                                                                                    nmrgfil2
         goto       det
.
STOP
.         compare   eop to lines
.         call      header if not less
.patch1.99
          if (row > "9750")
                    call header
          endif

        add     eightlpi,row
        add     "60",row
        prtpage laser;*p1500:row,*font=font12,*ALIGNMENT=*Left,*boldon,"Records Applied  :  ",*boldoff;
        prtpage laser;*font=font12,applied
          prtclose  laser
.patch1.99 Comment out
.         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
.         splclose
.end comment out patch1.99
         weof      skipped,seq
         close     skipped
.patch1.99
                              PRTPLAY Printfile,"\\NINs2\Laser2"
                              PRTPLAY Printfile,"\\NINs2\Laser2"
.patch1.99
.         .execute "F:\PUBLIC\NPRINT g:\DATA\nmrginf.LST Q=LASER2 NT NA=GS_JL f=0 S=NTS0_fpnw C=2"
.begin patch 1.7
.begin patch 1.96
.patch1.99 Commentout
.                    call                GetWinVer
.patch1.99 Commentout
.         path      exist,"c:\windows"
.         if        over
.START PATCH 1.93 REPLACED LOGIC
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrginf.LST \\nts0\LASER2 "
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrginf.LST \\nts0\LASER2 "
.         else
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrginf.LST \\nts0\LASER2 "
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrginf.LST \\nts0\LASER2 "
.
.patch1.99 Commentout
.         IF         (osflag = c1 | osflag =c5)
.         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"nmrginf.LST \\nts0\LASER2 "
.         Execute   TASKNAME
.         Execute   TASKNAME
.patch1.99 Commentout
.         else
.patch1.99 Commentout
.                    Elseif              (osflag = c3 | osflag = c4)
.         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"nmrginf.LST \\nts0\LASER2 "
.                    elseif              (osflag = c6)
.         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"nmrginf.LST \\nts0\LASER2 "
.patch1.99 Commentout
.end patch 1.96
.         EXECUTE   TASKNAME
.         EXECUTE   TASKNAME
.END PATCH 1.93 REPLACED LOGIC
.patch1.99 Commentout
.         endif
.patch1.99 Commentout
.end patch 1.7
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop
PrintOpen
                              pack printfile,"\\nins1\e\data\NMRGINF.lst"
          PRTOPEN LASER,"\\NINs2\Laser2","INFOGRP Merge Printout",noprint,spoolfile=printfile
                              return
         INCLUDE   NMRGIO.inc
.patch2.12
          include   compio.inc
          include   cntio.inc
.         include   nmlrio.inc
.patch2.12
         include   nordio.inc
         include   ninvio.inc
.begin patch 1.9
         include   ncntio.inc
.end patch 1.9
         INCLUDE   COMLOGIC.inc


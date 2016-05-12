PC        EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NORDDD.inc
          include   compdd.inc
          include   cntdd.inc
.Begin Patch 1.73
          Include   Ndatdd.inc
.end Patch 1.73
          INCLUDE   NSHPDD.inc
          INCLUDE   NOWNDD.inc
          include   hp.inc
.Begin Patch 1.8
          include winapi.inc
.end patch 1.8
.
. .................................................................................................................
Release   init     "2.03"            DLH Some HTML cleanup
Reldate   Init      "2016 February 22"
.Release   init     "2.02"            DLH Use a a test for Office 365
.Reldate   Init      "2015 December 29"
.Release   init     "2.00"            DLH replace pdf form with all in the email body, change from +3 days to +1
.Reldate   Init      "2015 May xx"
.Release   init     "1.95"            DLH replace reuben with Jennifer
.Reldate   Init      "2015 May 18"
.Release   init     "1.94"            DLH New Fax Gateway
.Reldate   Init      "2015 April 14"
.Release   init     "1.93"            DLH Patch use new owner field to suppress requests, get rid of Nofaxlist file, skip old PL records
.Reldate   Init      "2015 February 12"
.Release   init     "1.92"            DLH Patch supress  5723 Rosenberg fund
.Reldate   Init      "2015 February 11"

.Release   init     "1.91"            DLH increase email vars to 100
.Reldate   Init      "2014 April 1"
.Release   init     "1.90"            DLH Sunbelt PDF
.Reldate   Init      "2013 April 23"
.Release   init     "1.88"            DLH Patch supress Beth Foster
.Reldate   Init      "5 April  2013"
.Release   init     "1.87"            DLH Patch to run under vista and higher windows - create pdf's and fax via email
.Reldate   Init      "4 May  2011"
.Release   init     "1.86"            DLH enhance      see this version for old code
.Reldate   Init      "30 April  2011"
.Release   init     "1.85"            DLH enhance
.Reldate   Init      "03 February  2010"
.Release   init     "1.84"            DLH misc cleanup
.Reldate   Init      "11 February  2010"
.Release   init     "1.83"            DLH add suppression of reports based on CompShpFlag
.Reldate   Init      "05 September  2008"
.Release   init     "1.82"            DLH AH pdf checking does not seem to always cut it added additional check before email
.Reldate  Init      "14 August  2008"
.Release   init     "1.81"            DLH Email add bcc to COmputerrequests.com & skip return date = order date = today
.Reldate  Init      "30 April  2008"
.Release   init     "1.8"            DLH Email
.Reldate  Init      "14 April  2008"
.Release   init     "1.73"            DLH Pull all exclusives       see this version for old code
.Reldate  Init      "19 March  2008"
.* NAMES IN THE NEWS . Nightly List Management SHIPPING Info request REPORT PROGRAM
. .................................................................................................................
.
. WORK VARIABLES
.
.begin patch 1.82
FileCheck FIle
trapcount form      4
.end patch 1.82

.begin patch 2.03
HexCRLF   INIT 0x0D, 0x0A
.end patch 2.03

.begin patch 1.8
EmailFlag Dim       1
.end patch 1.8

PDATE    DIM       8
TELEMASK INIT      "(999)999-9999"
ORDMASK INIT       "ZZZ,ZZ9,999"
ORDQTY   DIM       11
QTYNUM   FORM      9
.begin patch 1.81
ORDJDATE  FORM      5
RTNJDATE  FORM      5
.end patch 1.81
SYSJDATE FORM      5
TELE1    DIM       13
fax1    DIM       13
fax2    DIM       5
CODENUM  FORM      2
.
PROGNAME DIM       8
.
outflag  form   1          1=print, 2=fax, 3= email, 4= fax & email
LONGDIST DIM       1
. .............................................................................
NFULCOMP  DIM       55
NFULCNT   DIM       55
NFULFAX   DIM       10
.begin patch 1.73
.NFULEMAIL Dim       50
.EmailAddr Dim       50
NFULEMAIL Dim       100
EmailAddr Dim       100
.end patch 1.73
PrintDoc  pfile
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.Column8 form 8
.Column9 form 8
          move "300" to Column1
          move "500" to Column2
          move "1200" to Column3
          move "1500" to Column4
          move "4000" to Column5
          move "4050" to Column6
          move "5050" to Column7
          move "6050" to Column8
          move "6800" to Column9
CopyVar2 dim        5000
TimesNew10          font
tempfle   dim       100
          create    TimesNew10,"Times New Roman",size=10
                    move "180" to eightlpi
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
HOLDFUL  DIM       6
HOLDOWN  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "9180"
CountI   FORM      8                  NUMBER OF ORDERS READ.
CountO   Form       8                 records written out
Output   File
.begin patch 1.93
.NOFAXLIST  iFile    kEYL=6
.end patch 1.93
save     dim       47
fhandle  dim        4                   .use to create fax files.
spoolfle DIM       40                   .order  SPOOL FILEs
faxname  dim       45
faxtele  dim       10
faxattn  dim       45
FontO7              font
FontO18B  font

          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.
. .............................................................................
*******************************************************************************
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F3
         MOVE      "EXIT" TO PF3
         MOVE      "NSHP0007" TO PROGRAM
         MOVE      "Daily SHIPPING REPORT" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
.DLH why???? is this why thinks get locked up? turned off 2014 April 08
.         move      c2 to nshplock        .record locking
           move      c3 to nshplock        .No locking
.end ?
           move          c3,nordlock                .no locking
           move          c3,nownlock                .no locking
*****************
          Trap      PrintErr giving error if spool
*****************
.
.          Goto      Restart
.           move      "01/01/15",date
.           move      "01/01/15",pdate
.                    UNPACK    DATE INTO MM,str1,DD,str1,YY
.                    CALL      CVTJUL
.                    MOVE      JULDAYS TO SYSJDATE
.           
.           goto passtwo
PassOne
.Begin Patch 1.8
          move     "800000" to nordfld                      .order date of 5/19/2014
.          move     "760000" to nordfld                      .March 2012
.          move     "730000" to nordfld                      .April 2011
.          move     "630000" to nordfld
.end Patch 1.8
.Begin Patch 1.84
          Move      C3,Nordlock
.          OPEN      FAXLIST,"\\nins1\e\data\text\FAXLIST"
.begin patch 1.93
.          OPEN      nOFAXLIST,"nOFAXLIST.ISI|NINS1:502"
.end patch 1.93
.end Patch 1.84
                    CLOCK     DATE TO PDATE
                    move      pdate to date
                    UNPACK    DATE INTO MM,str1,DD,str1,YY
                    CALL      CVTJUL
                    MOVE      JULDAYS TO SYSJDATE
.begin patch 2.0
.                    add       c3 to sysjdate
.end patch 2.0
                    move     c1 to nordpath
                    call       nordtst
.begin patch 1.85
                    PACK      STR35,NTWKPATH1,"Diskin91.tmp"
.                    prepare   output,str35,exclusive
                    PACK      taskname,NTWKPATH1,"Diskin91.tmp|NINS1:502"
                    prepare   output,taskname,exclusive
.end patch 1.85
          call      GetWinVer
Pass1Loop call     nordks
          goto     eoj1 if over
          add      c1 to CountI
          DISPLAY   *P15:10,"RECORDS IN = ",CountI,b3,olrn;
          if        (olrn = "414521")
          call      debug
          endif
.HARD WIRED  0 SKIP THESE LISTS.  in NOFaxlist
.          match     "019539",olnum                .animal place    skip it
.          goto      pass1loop if equal
..
.          match     "014900",olnum     .Alaska Conservation Foundation
.          goto      pass1loop if equal
.
.          match     "021334",olnum     .Kerry for Pres
.          goto      pass1loop if equal
.
.          match     "002312",olnum     .Nat'l African Wildlife skip
.          goto      pass1loop if equal
.
.          match     "007823",olnum     .Ntl urban league per SKIP
.          goto      pass1loop if equal
.
.          match     "020565",olnum     .WaterKeeper Allianace  per JN SKIP
.          goto      pass1loop if equal
.
.          match     "018900",olnum     .Shambhala Sun  per Aaron skip
.          goto      pass1loop if equal
.
.Begin Patch 1.73
           cmatch     "B",olrn              .old PL records
           goto       pass1loop if equal
           cmatch     "M",olrn              .old PL records
           goto       pass1loop if equal
          move      c1,ndatpath
          packkey   Ndatfld,Olnum
          call      Ndatkey
.end Patch 1.73


NOFAX
.Begin Patch 1.73 . incorporated in new Owner flag
.            READ      NOFAXLIST,Ndatfld;str1
.            if        not over
.             goto   Pass1Loop
.            endif
.end Patch 1.73
          RESET     CANCODES               *RESET FORM POINTER.
          SCAN      OSTAT IN CANCODES       *CANCELLED?
          GOTO      Pass1Loop IF EQUAL
          move      "pxlz" to str4
          scan      ostat in str4
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
          clear     str2
          pack      str2 from OSALES10,osales
          move      c0 to n2
          move      str2 to n2
.begin patch 1.73
.          if        (n2 = 6 or N2 = 19 or N2 = 27 or N2 = 28 or )           .List Management?
          if        (n2 = 6 or N2 = 19 or N2 = 27 or N2 = 28 or ELSTCDE = "N" or ELSTCDE = "P")           .List Management or exclusive? (some overlap on logic)
.end patch 1.73
          goto      checkdate
          else
          goto      pass1loop
          endif
.
CheckDate
.begin patch 1.81 if return date = order date = today, skip
                    move                ORTNDTEM  to mm
                    move                ORTNDTEd  to dd
                    move                ORTNDTEy  to yy
                    move                ORTNDTEc  to cc
          call      CVTJul
          move      Juldays,RTNJDATE

                    move                OODTEM  to mm
                    move                OODTEd  to dd
                    move                OODTEy  to yy
                    move                OODTEc  to cc
          call      CVTJul
          move      Juldays,ORDJDATE
          
          if        (ORDJDATE = RTNJDATE  & ORDJDATE = SYSJDATE)
          goto      Pass1Loop
          Elseif        (ORDJDATE + 1 = RTNJDATE & RTNJDATE = SYSJDATE)
          goto      Pass1Loop
          endif
.end patch 1.81     
.return date past or within the next 3 days?
          if        (ortndtem > "0" & ortndtem < "13")
          move      ORTNDTEM  to mm
          move      ORTNDTEd  to dd
          move      ORTNDTEy  to yy
          move      ORTNDTEc  to cc
          else
          move      OMDTEM  to mm
          move      OmdTEd  to dd
          move      OmdTEy  to yy
          move      OmdTEc  to cc
          endif
          call      cvtjul
          if        (juldays <= sysjdate)
          goto      checktwo
          else
          goto      pass1Loop
          endif
.
checktwo
          MOVE      OLRN TO NSHPFLD
          rep       zfill in nshpfld
          CALL      NSHPKEY
          goto      pass1loop if not over         .already shipped
          MOVE      OLON TO NOWNFLD
          REP       ZFILL IN NOWNFLD
          CALL      NOWNKEY
.begin patch 1.73   not our file forget about it
.         if      (OLNUM = "015003")       .E (The Environmental Magazine) List
.                   Move "009390",OFULLFIL        .Still KABLE
.         endif
.end patch 1.73   not our file forget about it
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "checktwo-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                              clear     NFULCNT
                              clear     NFULFAX
.begin patch 1.73
                              clear     NFULEMAIL
.end patch 1.73
                    else
.begin patch 1.83
.suppress if requested
                    if        (CompShpFlag = "T")
                    goto      Pass1Loop
                    endif
.end patch 1.83
.Grab the active contact full name if there is one
                              Packkey CNCTFLD2 to "01X",COMPNUM
                              Call      CNCTAIM
                              loop
                              until over
                              until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                        call      CNCTKG
                              repeat
                              move      COMPCOMP,NFULCOMP
                              move      CNCTFNAME,NFULCNT
                              move      COMPFAX,NFULFAX
.begin patch 1.73
                              Move      CompEmail,NFULEMAIL
                              Move      CompEmail,EMailAddr

.end patch 1.73
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
                    clear     NFULCNT
                    clear     NFULFAX
.begin patch 1.73
                    clear     NFULEMAIL
.end patch 1.73
.begin patch 1.88
.if no fulfillment company (SB) suppress  Beth Foster
.5203 bradley for PRes, 5322 CMSH, 5325 Yanisch, 5346 Harkin, 5404 CMSH, 5464 Vilsack, 5471 Gephardt, 5503 Kennedy
.5239 Flanagan, 5515 Wild Pac, 5642 Patrick Kennedy, 5651 Baldwin, 5686 Tom Pac, 5707 Castor, 5710 Cantwell, 5737 Beth Foster
.5246 McCarthy, 5859 All American PAC, 5878 Kennedy, 5879 cmtte Dem Maj., 7230 Franken, 5723 Rosenberg fund, DCCC
.begin patch 1.93   
                    if        (OwnNoShp = yes)
                    goto         pass1loop
                    endif
.                    if        (olon = "5723" )
.                    goto      Pass1Loop
.                    endif
.                    if        (olon = "5737" | olon = "5203" | olon = "5239" | olon = "5246" | olon = "5322" | olon = "5325" | olon = "5346" )
.                    goto      Pass1Loop
.                    endif
.                    if        (olon = "5404" | olon = "5464" | olon = "5471" | olon = "5503" | olon = "5515" | olon = "5642" | olon = "5651" )
.                    goto      Pass1Loop
.                    endif
.                    if        (olon = "5686" | olon = "7230" | olon = "5879" | olon = "5878" | olon = "5859" | olon = "5710" | olon = "5707" )
.                    goto      Pass1Loop
.                    endif
.end patch 1.93    

.end patch 1.88
          endif
.turned off - hopefully temporary 2013 November 4 DLH          
.          Goto      Pass1Loop If (OFULLFIL = "009406")      .Donnelley/Triplex/TDMC
          
.begin patch 1.93    
.          Goto      Pass1Loop If (OLON = "7222")      .Lester
.suppress Beth Foster
.                    if        (olon = "5737")
.                    goto      Pass1Loop
.                    endif
.end patch 1.93    

.end patch xxx

          write     Output,seqeof;ordvars
          add       c1,countO                               .records writtten
          DISPLAY   *P15:11,"RECORDS Out = ",Counto,b3,olrn;

          goto      Pass1Loop
eoj1
.close - sort
         weof       output,seqeof
         DISPLAY   *P15:23,*EL,"Sorting records = ";
         close      nordfile
         close      output
         if         (CountI = c0)
         Goto       Exit1               .shutdown
         endif
Restart         
.22-25 Owner #, 329-334 fulfillment ##, 214-247 list desc
        pack    taskname,"\\nins1\e\data\diskin91.tmp,\\nins1\e\data\diskin91.dat;22-25,329-334,214-247"
        sort    taskname
        if over
                move    s$error$,error
                   DISPLAY   *P15:23,*EL,"Sorting Error = ",Error,*w5,*b,*w5;
                stop
        endif

.passtwo -
passtwo
          move                C0 TO CountI
          CALL                FUNCDISP
          clear     taskname
          clear     holdown
          Clear     HoldFul         
          move      c0 to nordflag
.Begin Patch 1.84
.          PACK      STR35,NTWKPATH1,"Diskin91.dat"
.          open                output,str35,exclusive
          PACK      STR55,NTWKPATH1,"Diskin91.dat|NINS1:502"
          open                output,str55,read
.end Patch 1.84
          MOVE                "                    " TO FERROR
.
GETREC   DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
.
         read      output,seq;ordvars
         GOTO      EXIT IF OVER
         ADD       C1 TO CountI
         if         (olrn = "766262")
         call       debug
         endif
         DISPLAY   *P15:12,"Printing Record = ",CountI,b3,olrn;
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         MOVE       OwnEmail,EmailAddr

          call      Trim using OFULLFIL
          if (OFULLFIL <> "")                             .use fulfilment info
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "GETREC-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                              clear     NFULCNT
                              clear     NFULFAX

                    else
.Grab the active contact full name if there is one
                              Packkey CNCTFLD2 to "01X",COMPNUM
                              Call      CNCTAIM
                              loop
                              until over
                              until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                        call      CNCTKG
                              repeat              
                              move      COMPCOMP,NFULCOMP
                              move      CNCTFNAME,NFULCNT
                              move      COMPFAX,NFULFAX
.begin patch 1.73
                              Move      CompEmail,NFULEMAIL
                              Move      CompEmail,EMailAddr

.end patch 1.73
                    endif
          else      .// NFULLFIL = ""
.begin patch 1.73
                    Clear     NFULEMAIL
.end patch 1.73
                    clear     COMPFLD
                    clear     NFULCOMP
                    clear     NFULCNT
                    clear     NFULFAX
          endif
          if        (CountI = c1)
          call      PDFPREP
          endif
          IF        (Nownfld <> HOldOwn)
          call      Break
          Elseif    (OFullfil <> HOldFul)
          call      Break
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
         move      c0 to n10
         move      faxtele to n10
         compare   c0 to n10
         if        not equal
         type      faxtele
                if  equal
                   move      faxtele to ownfax
           endif
         endif
         move      telemask to fax1
         edit      faxtele to fax1
HD1
.begin patch 2.0
.         if         (page <> c1)
.        prtpage PrintDoc;*NEWPAGE:
.                                        *UNITS=*HIENGLISH:
.                        *ORIENT=*PORTRAIT;
.                              endif
..          If        (Ocompid2 = "P")            .pacific lists mangement list order
..          prtpage   PrintDoc;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
..                    *p=451:343,*font=fontO7,"180 Grand Avenue, Suite 1365":
..                    *p=451:443,"Oakland, CA 94612-3716":
..                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                    *p=317:643,"A Division of Names in the News"
..
..          else
.          prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
..          endif
.          
.                                        
.                                        move "300" to row
.                                        Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"DATE:";
.                                        Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PDATE;
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"PAGE ## ";
.                                        Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PAGE;
.                                        add eightlpi to ROW
.                                        add "600" to Row
.                                        Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew10,*ll,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t";
.                                        add eightlpi to ROW
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,faxattn;
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ownocpy;
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,nfulcomp;
.                    if        (EMailFlag = Yes)
.                                        Prtpage PrintDoc;*font=TimesNew10,"Via  - Email ";
.                                        Prtpage PrintDoc;*font=TimesNew10,EMailAddr;
.                    else
.                                        Prtpage PrintDoc;*font=TimesNew10,"Via  - Fax ";
.                                        Prtpage PrintDoc;*font=TimesNew10,fax1;
.                    endif
.                                        add eightlpi to ROW
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Rtn/Mail/";
.                                        Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Order/Ship";
.                                        Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Tracking";
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"LR ##";
.                                        Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"List/Mailer";
.                                        Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shp Date";
.                                        Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Quantities";
.                                        Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Number";
.                                        add eightlpi to ROW
           Clear      Mailbody
           append     "<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">",Mailbody   
           append     "<html>",Mailbody
           append     "<head>",Mailbody
           append     "<html><Head>",Mailbody
           append     "<meta name=#"generator#" content=#"HTML Tidy for Linux (vers 25 March 2009), see www.w3.org#">",mailbody
           append     "<title>Shipment Information Request</title>",Mailbody
           append     "</head>",Mailbody
           append     HexCRLF,mailbody
           append     "<body>",Mailbody
           append     "<h2 align=#"center#">Shipment Information Request</h2>",Mailbody
           append     "<b>VIA: ",Mailbody
           if        (EMailFlag = Yes)
           append     EMailAddr,mailbody         
           else
           append     Fax1,mailbody
           endif
           append     "<br><br>",mailbody
           append     "Recently we placed an order for a list that you manage and fulfill, and this notice is a request for order fulfillment or 'shipping' information.<br>",mailbody
           append     "<br>",mailbody
           append     HexCRLF,mailbody
           
           append     "Could you please contact us to let us know:<br>",mailbody
           append     "<br>",mailbody
           append     HexCRLF,mailbody
           append     "1) When you expect to send the file, or if you have emailed or posted the file.<br>",mailbody
           append     "2) A confirmation of the quantity sent, and the date it was sent.<br>",mailbody
           append     "3) Include the mailer name and our LR## (order number).<br>",mailbody
           append     HexCRLF,mailbody
           append     "<br>",mailbody
           append     "This will help us make sure that the orders are recognized as received at the merge/purge facility and also provide us with accurate information",mailbody
           append     " for billing or for exchange status posting.<br>",mailbody
           append     HexCRLF,mailbody
           append     "<br>",mailbody
           append     "If you you have not received an order for your list as described below, or are getting repeated requests for an order that has shipped,",mailbody
           append     " please contact: JenniferMagee@nincal.com, (510)302-4625.<br>",mailbody
           append     HexCRLF,mailbody
           append     "<br>",mailbody
           append     "Thank you so much for your time and attention. We greatly appreciate working with you. <br>",mailbody
           append     HexCRLF,mailbody
           
.end patch 2.0

         RETURN
*......................................................................
PRINT
.begin patch 2.0
.          COMPARE   PBREAK TO ROW
.          CALL      HEADER IF NOT LESS
          MOVE      ORDMASK TO ORDQTY
          MOVE      OQTY TO QTYNUM
          EDIT      QTYNUM TO ORDQTY
.                    add eightlpi to ROW
.                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,OLRN;
.                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*boldon,O1DES,*boldoff;
.                    PACK STR10 WITH ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
.                    Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ORDQTY;
.                    Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"______________________________";
.                    add eightlpi to ROW
.                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,MCOMP;
.                    PACK STR10 WITH OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
.                    add eightlpi to ROW
.                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipped Via:";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"____________________________________________";
.                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__/__/____";
.                    Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__________";
.                    Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipping Cost: ";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"$__________";
.                    add eightlpi to ROW

           append     "<table width=#"100%#" border=#"1#" cellpadding=#"2#" cellspacing=#"2#" >",mailbody
           append     "<tbody>",mailbody
           append     "<tr>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>LR##: ",mailbody
           append     olrn,mailbody
           append     "</b></td>",mailbody
           append     "<td width=#"50%#" valign=#"top#"><b>Mailer: ",mailbody
           append     mcomp,mailbody
           append     "</b> <br>",mailbody
           append     "</td>",mailbody
           append     "<td width=#"25%#" valign=#"top#"><b>Mail Date: ",mailbody
           PACK STR10 WITH OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
           append     str10,mailbody           
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "<tr>",mailbody
           append     "<td valign=#"top#"><b>Return Date: ",mailbody
           PACK STR10 WITH ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
           append     str10,Mailbody
           append     "</b></td>",mailbody
           append     "<td valign=#"top#"><b>List: ",mailbody
           append     O1Des,Mailbody
           append     "</b> <br>",mailbody
           append     "</td>",mailbody        
           append     "<td valign=#"top#"><b>Order Qty: ",mailbody
           append     ordqty,mailbody
           append     "</b></td>",mailbody
           append     "</tr>",mailbody
           append     "<tr>",mailbody
           append     "<td valign=#"top#"><b>Shipped Via:_________________</b></td>",mailbody
           append     "<td valign=#"top#"><b>Ship Date:____________</b></td>",mailbody
           append     "<td valign=#"top#"><b>Shipped Qty:</b>_____________</td>",mailbody
           append     "</tr>",mailbody
           append     "</tbody>",mailbody
           append     "</table>",mailbody
           append     HexCRLF,mailbody




.end patch 2.0

.some cleanup 2013 jun 28 DLH
          clear     faxattn
          clear     COMPFLD
          clear     NFULCOMP
          clear     NFULCNT
          clear     NFULFAX
          clear     ownocpy

         GOTO      GETREC
........................................................................................................
BREAK
         call       sendit
.

         move      c0 to outflag              .reset flag
.........................................
.begin patch 1.73
.                   ADD code here to do Email instead of fax - so would create PDF and email the request
.         make print subroutine called so can use for both faxing and email.
.begin patch 1.8

          call      Trim using EMailAddr
          if        (EMailAddr <> "")
                    Scan "@",EMailAddr
                    if        equal
                    RESET     EMAILADDR
                    MOVE      EMAILADDR,mAILTO
                    goto      Faxer
                    ENDIF
          RESET     eMAILADDR
          ENDIF
.check for fax
                    clear     faxtele
                    clear     faxattn
                    move      NOWNFLD,fhandle
                    move      "           ",faxname
                    call      Trim using NFULCNT
                              if (NFULCNT = "")
                              move      "Order Fulfillment",faxattn
                    else
                              move      NFULCNT,faxattn
                    endif

                    move      faxattn,faxname
                    call      Trim using NFULFAX
                    move      C0,N10
                    move      NFULFAX,N10
                    if (N10 > 0)
                              move      NFULFAX,faxtele
                    else
..begin patch 1.72 exception for Sherene   02/15/2008
.                              if        (Olnum <> "017432")   Whitney museum & One Source productions
                              move      ownfax,faxtele
.                              endif
.end patch 1.72 exception for Sherene
                    endif

                    move      C0,N10
                    move      faxtele to n10
.begin Patch 1.87
                    compare   c0 to n10

                    if        equal
                    bump      spoolfle by 16                 .for next fax.
                    reset     spoolfle
                    move      c1 to outflag                  .will be copied to the printer
                    call      PDFPREP
                    ELSE
                    move      c2 to outflag                  .will be copied to fax
                    COUNT     N2,faxtele
                    COMPARE   C10 TO N2
                              IF        EQUAL
                              MOVE      C1 TO LONGDIST
                              UNPACK    faxtele INTO STR3,STR7
                              MATCH     "510" TO STR3           .LOCAL ?
                                        IF         EQUAL
                                        MOVE       STR7 TO faxtele
                                        CLEAR      LONGDIST
                              else
                                        MATCH      B3 TO STR3           .LOCAL ?
                                                  IF         EQUAL
                                                  MOVE       STR7 TO faxtele
                                                  CLEAR      LONGDIST
                                                  ENDIF
                                        ENDIF
                              ENDIF

                              if        (outflag = c2)    .fax
.                              pack      EMailAddr,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
                              pack      EMailAddr,"+",longdist,faxtele,"@fax.nincal.com"
                    Else      
                              Clear     EmailAddr
                              endif
          
faxer
          Move      "Request for confirmation of order fulfillment",MailSubjct
.          MOve      "JoeyGamache@nincal.com",MailFrom
.begin patch 2.01
          Move      "JenniferMagee@nincal.com",MailReply
          Move      "JenniferMagee@nincal.com",Mailfrom
.end patch 2.01
.Begin patch 1.81   
          MOve      "ComputerRequest@nincal.com,JenniferMagee@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
.end patch 1.81     
.begin patch 2.0
.          Move      "c:\work\PDF\Shipping.pdf",MailAttach
          MOve      EMailAddr,MailTo      .are we using owner or fulfillemnt email?????
          MOve      Yes,EmailFlag
          MOve      C3,OutFlag
.          call      PDFPREP
         move      c0 to page
         CALL      HEADER
          move     nownfld to holdown
          mOVE      ofullfIL,hoLDfuL         
.end patch 2.0

         endif
          
          REturn
          

         move      c0 to page
         CALL      HEADER
breakexit
          move     nownfld to holdown
          mOVE      ofullfIL,hoLDfuL         
          RETURN
sendit
         if         (CountI <> c1)
.begin patch 2.0
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Please fax Shipping information back to (510)628-8313.";
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"If you did not receive a listed order, Please let us know immediately.";
.                                        add eightlpi to ROW
.                                        add eightlpi to ROW
.                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Names in the News - List Management (415)989-3350.";
.                                        prtclose PrintDoc
           append     "<br>",mailbody
           append     "Please Email shipping information back to JenniferMagee@nincal.com, or Fax to (510)628-8313.<br>",mailbody
           append     "Names in the News - List Management (415)989-3350.<br>",mailbody
           append     "<br>",mailbody
           append     "</body>",mailbody
           append     "</html>",mailbody
           reset      mailbody
.end patch 2.0
.begin patch 1.8
.                                        If        (EmailFlag = Yes)
                                        IF        (OUTFLAG > C1)
.begin patch 2.0
.          Clear     Mailbody
.          Append    MailSubjct,mailbody
.          append    crlf,mailbody
.          append    "Shipping Request",Mailbody
.          append    crlf,mailbody
.          
.          reset     Mailbody
.end patch 2.0
                                        Goto      SendItEmail                                                 
.Begin patch 1.90
.                                        ElseIf    (outFlag = c1)
                                        ElseIf    (outFlag <= c1)
.                                        move      "ReubenHolland@nincal.com",Mailto
                                        move      "JenniferMagee@nincal.com",Mailto
.begin patch 2.0
.          Append    MailSubjct,mailbody
.          append    crlf,mailbody
.          append    "MIA Shipping Request",Mailbody
          Move     "MIA Shipping Request",Mailsubjct
.          append    crlf,mailbody
.end patch 2.0
          
.          reset     Mailbody
                                        Goto      SendItEmail                                                 
.                                        else
.                                        call       GetPDFPath
.                                        pack      APIFileName,PDFPATH,"\flag.dat",hexzero
.                                        loop
.                                                  call      FindFirstFile
.                                        until (APIResult = 0 | APIResult = hexeight)
.                                                  pause     "1"
.                                        repeat
.                                        pause     "5"
.                                        pack      taskname from "\\nins1\winbatch\shipreq1.exe"
.                                        execute   Taskname
.                                        pause               "5"
.                                        erase               MailAttach
.                                        Return
                                        endif
.end patch 1.90
.end patch 1.8
         clear     taskname
         endif
.
         if        (outflag = 2 or outflag = 4)    .to fax or fax
         endif
.
.begin patch 1.8         
SendItEmail
.          Move      "NINS1",mailserver           .VM testing

          if        (outflag > 1)    .to email with email addy or fax #
.It takes some time for the file to be created, so we must check

. Use the Flag to check if pdf has been created
.Begin patch 1.90
.        call       GetPDFPath
.                              pack      APIFileName,PDFPATH,"\flag.dat",hexzero
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        pause     "1"
.                              repeat
                              pause     "2"
.                              pack      str45,PDFPATH,"\flag.dat",hexzero
.                              erase     str45
.end patch 1.90
.begin patch 2.0
          Clear     Mailsubjct
          Move    "Request for confirmation of order fulfillment",MailSubjct
.          append    "Request for confirmation of order fulfillment",MailSubjct
.          append    b1,Mailsubjct
.          append    Mailto,Mailsubjct
.          reset     Mailsubjct
.end patch 2.0
.begin patch 1.82
          Move      c0,TrapCount                   .reset

CheckFile

.begin patch 2.0
.          trap      WaitForEnd giving error if IO
.          open      FileCheck,MailAttach,Exclusive          
.          Close     FIleCHeck
.end patch 2.0

.end patch 1.82
.temp for testing
.                      move       "Dherric@nincal.com",mailto
.temp for testing
                     move      c1,MailType         .HTML
                     move        "500",Mailtimer
                    call      SendMail
                    erase     str45
                    Move      No,EmailFlag

         endif
.

        return
.begin patch 1.82
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"NShp0007 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
.                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
.                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 1.82        
.................................................................................................
PDFPREP
.First check 995 autolaunch settings
.Begin patch 1.90
.                              Call      PDF995Auto
.                              call      SetPDFFlag
.                              PRTOPEN PrintDoc,"PDF995","Shipping"
.begin patch 2.0
.                              PRTOPEN PrintDoc,"PDF:","c:\work\pdf\Shipping.pdf"
.                              pack    str45,"Shipping.pdf"
..end patch 1.90
.
.                              PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
.                              *ORIENT=*Portrait:
.                              *MarginL=1;
.end patch 2.0
          move      c0 to page
          CALL       HEADER
          move       nownfld to holdown
          mOVE      ofullfIL,hoLDfuL     
          Return        
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
          if        (CountI > C0)
          call       sendit
          endif
          shutdown
          STOP
EXIT1     shutdown
          STOP
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
PrintErr         
         DISPLAY   *P1:24,*EL,"Spool ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
          if        equal
.begin patch 1.90
.                              PRTOPEN PrintDoc,"PDF995","Shipping"
                              PRTOPEN PrintDoc,"PDF:","c:\work\pdf\Shipping.pdf"
                              pack    str45,"Shipping.pdf"
.end patch 1.90
                              PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                              *ORIENT=*Portrait:
                              *MarginL=1;
          move      c0 to page
          TrapClr Spool
          Trap      Printerr giving error if spool
.          noreturn
          goto      hd1
          endif          
         GOTO      Printerr
       
         
..........................................................................
          INCLUDE   NSHPIO.inc
          INCLUDE   NOWNIO.inc
          include   compio.inc
          include   cntio.inc
          Include   Ndatio.inc
          INCLUDE   NORDIO.inc
          include   hpio.inc
          INCLUDE   COMLOGIC.inc


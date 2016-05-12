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
Release   init     "1.87"            DLH Patch to run under vista and higher windows - create pdf's and fax via email
Reldate   Init      "4 May  2011"
.Release   init     "1.86"            DLH enhance
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
NFULEMAIL Dim       50
EmailAddr Dim       50
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
COUNTO   FORM      8                  NUMBER OF ORDERS READ.
Output   File
NOFAXLIST  iFile    kEYL=6
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
         move      c2 to nshplock        .record locking
           move          c3,nordlock                .no locking
*****************
          Trap      PrintErr giving error if spool
*****************
.
          Goto      Restart
PassOne
.Begin Patch 1.8
          move     "730000" to nordfld                      .April 2011
.          move     "630000" to nordfld
.end Patch 1.8
.Begin Patch 1.84
          Move      C3,Nordlock
.          OPEN      FAXLIST,"\\nins1\e\data\text\FAXLIST"
          OPEN      nOFAXLIST,"nOFAXLIST.ISI|10.10.30.103:502"
.end Patch 1.84
                    CLOCK     DATE TO PDATE
                    move      pdate to date
                    UNPACK    DATE INTO MM,str1,DD,str1,YY
                    CALL      CVTJUL
                    MOVE      JULDAYS TO SYSJDATE
                    add       c3 to sysjdate
                    move     c1 to nordpath
                    call       nordtst
.begin patch 1.85
                    PACK      STR35,NTWKPATH1,"DISKIN71.tmp"
.                    prepare   output,str35,exclusive
                    PACK      taskname,NTWKPATH1,"DISKIN71.tmp|10.10.30.103:502"
                    prepare   output,taskname,exclusive
.end patch 1.85
          call      GetWinVer
Pass1Loop call     nordks
          goto     eoj1 if over
          add      c1 to counto
          DISPLAY   *P15:10,"RECORDS IN = ",COUNTO;
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
          move      c1,ndatpath
          packkey   Ndatfld,Olnum
          call      Ndatkey
.end Patch 1.73


NOFAX
.            READ      FAXLIST,seq;STR6
            READ      NOFAXLIST,Ndatfld;str1
            if        not over
.                    if        (str6 = olnum)
.                    goto      pass1loop if equal
.                    else
.                    goto      NOFAX
             goto   Pass1Loop
            endif
.            else
.            endif
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
          endif
          if        (ORDJDATE + 1 = RTNJDATE & RTNJDATE = SYSJDATE)
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
          endif
          
          Goto      Pass1Loop If (OFULLFIL = "009406")      .Donnelley/Triplex/TDMC
          

          write     Output,seqeof;ordvars
          goto      Pass1Loop
eoj1
.close - sort
         weof       output,seqeof
         DISPLAY   *P15:23,*EL,"Sorting records = ";
         close      nordfile
         close      output
Restart         
        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,329-334,214-247"
        sort    taskname
        if over
                move    s$error$,error
                   DISPLAY   *P15:23,*EL,"Sorting Error = ",Error,*w5,*b,*w5;
                stop
        endif

.passtwo -
passtwo
          move                C0 TO COUNTo
          CALL                FUNCDISP
          clear     taskname
          clear     holdown
          Clear     HoldFul         
          move      c0 to nordflag
.Begin Patch 1.84
.          PACK      STR35,NTWKPATH1,"DISKIN71.dat"
.          open                output,str35,exclusive
          PACK      STR55,NTWKPATH1,"DISKIN71.dat|10.10.30.103:502"
          open                output,str55,read
.end Patch 1.84
          MOVE                "                    " TO FERROR
.
GETREC   DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
.
         read      output,seq;ordvars
         GOTO      EXIT IF OVER
         ADD       C1 TO COUNTO
         if         (olrn = "766262")
         call       debug
         endif
         DISPLAY   *P15:12,"Printing Record = ",COUNTO;
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
          if        (counto = c1)
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
         if         (page <> c1)
        prtpage PrintDoc;*NEWPAGE:
                                        *UNITS=*HIENGLISH:
                        *ORIENT=*PORTRAIT;
                              endif
.          If        (Ocompid2 = "P")            .pacific lists mangement list order
.          prtpage   PrintDoc;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=451:343,*font=fontO7,"180 Grand Avenue, Suite 1545":
.                    *p=451:443,"Oakland, CA 94612-3799":
.                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=317:643,"A Division of Names in the News"
.
.          else
          prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
.          endif
          
                                        
                                        move "300" to row
                                        Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"DATE:";
                                        Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PDATE;
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"PAGE ## ";
                                        Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PAGE;
                                        add eightlpi to ROW
                                        add "600" to Row
                                        Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew10,*ll,                   "S h i p m e n t   I n f o r m a t i o n   R e q u e s t";
                                        add eightlpi to ROW
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,faxattn;
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ownocpy;
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,nfulcomp;
                    if        (EMailFlag = Yes)
                                        Prtpage PrintDoc;*font=TimesNew10,"Via  - Email ";
                                        Prtpage PrintDoc;*font=TimesNew10,EMailAddr;
                    else
                                        Prtpage PrintDoc;*font=TimesNew10,"Via  - Fax ";
                                        Prtpage PrintDoc;*font=TimesNew10,fax1;
                    endif
                                        add eightlpi to ROW
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Rtn/Mail/";
                                        Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Order/Ship";
                                        Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Tracking";
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"LR ##";
                                        Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"List/Mailer";
                                        Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shp Date";
                                        Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Quantities";
                                        Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Number";
                                        add eightlpi to ROW

         RETURN
*......................................................................
PRINT
          COMPARE   PBREAK TO ROW
          CALL      HEADER IF NOT LESS
          MOVE      ORDMASK TO ORDQTY
          MOVE      OQTY TO QTYNUM
          EDIT      QTYNUM TO ORDQTY
                    add eightlpi to ROW
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,OLRN;
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*boldon,O1DES,*boldoff;
                    PACK STR10 WITH ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
                    Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ORDQTY;
                    Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"______________________________";
                    add eightlpi to ROW
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,MCOMP;
                    PACK STR10 WITH OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
                    add eightlpi to ROW
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipped Via:";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"____________________________________________";
                    Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__/__/____";
                    Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__________";
                    Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipping Cost: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"$__________";
                    add eightlpi to ROW
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
                              pack      EMailAddr,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
                    Else      
                              Clear     EmailAddr
                              endif
          
faxer
          Move      "Order Fulfillment",MailSubjct
.          MOve      "JoeyGamache@nincal.com",MailFrom
          Move      "AaronCinco@nincal.com",MailFrom
.Begin patch 1.81   
          MOve      "ComputerRequest@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
.end patch 1.81     
          Move      "c:\work\PDF\Shipping.pdf",MailAttach
          MOve      EMailAddr,MailTo      .are we using owner or fulfillemnt email?????
          MOve      Yes,EmailFlag
          MOve      C3,OutFlag
          call      PDFPREP
          
          REturn
          
.end patch 1.8
.end Patch 1.87
.TESTING DH
.         MOVe      "4154337796",N10
.         MOVe      "4154337796",faxtele
          
.         clear     spoolfle
..         append    "\\nts0\d\data\fax\nSHP",spoolfle
.         append    "\\nins1\d\data\fax\nSHP",spoolfle
.         append    nownfld to spoolfle
.          IF        (OFullfil <> "")
.          Append    OFullFil,spoolfle        
.          else      
.          Append    "000000",Spoolfle
.          endif
.         APPEND    ".LST" TO spoolfle
.         reset     spoolfle
.         clear     tempfle
..         append    "\\nts0\d\data\fax\nSHP",tempfle
.         append    "\\nins1\d\data\fax\nSHP",tempfle
.         append    nownfld to tempfle
.          IF        (OFullfil <> "")
.          Append    OFullFil,tempfle        
.          else      
.          Append    "000000",tempfle
.          endif
.         APPEND    ".TMP" TO tempfle
.         reset     tempfle
..begin Patch 1.87
.          call      GetWinVer
.                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                        PRTOPEN   PrintDoc,"\\SRV2008a\laser8",Str45
.                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
.                                        PRTOPEN   PrintDoc,"Laser8",Str45
.                              elseif (OSFLAG = "8" or OSFLAG = "9")             
.                                        PRTOPEN   PrintDoc,"\\SRV2008a\laser8",Str45
.                              else   .(osflag = c0)         .Don't know prompt for printer
.                                        PRTOPEN   PrintDoc,"",Str45
.                              endif
.
..                              PRTOPEN PrintDoc,"faxfile","Faxfile.prn"
..end Patch 1.87
.                              PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
.                              *ORIENT=*Portrait:
.                              *MarginL=1;
.          compare   c0 to n10
.          if        equal
.          bump      spoolfle by 16                 .for next fax.
.                   reset     spoolfle
.                move      c1 to outflag                  .will be copied to the printer
.          ELSE
.             move      c2 to outflag                  .will be copied to fax
.                   COUNT     N2,faxtele
.            COMPARE   C10 TO N2
.            IF        EQUAL
.                   MOVE      C1 TO LONGDIST
.                UNPACK    faxtele INTO STR3,STR7
.             MATCH     "510" TO STR3           .LOCAL ?
.                      IF         EQUAL
.                                MOVE       STR7 TO faxtele
.                              CLEAR      LONGDIST
.                    else
.                                MATCH      B3 TO STR3           .LOCAL ?
.                                IF         EQUAL
.                                          MOVE       STR7 TO faxtele
.                                       CLEAR      LONGDIST
.                                ENDIF
.                      ENDIF
.            ENDIF
..begin Patch 1.87
..                              pack      taskname,"c:\work\hdrfile.prn"
..                              splopen   taskname
..                              print   hpreset,Hpcour,"^[D",longdist,faxtele,"^[N",faxname,"^]",Hpfixed;
..                              splclose
..end Patch 1.87
.         endif

         move      c0 to page
         CALL      HEADER
breakexit
          move     nownfld to holdown
          mOVE      ofullfIL,hoLDfuL         
          RETURN
sendit
         if         (counto <> c1)
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Please fax Shipping information back to (510)628-8313.";
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"If you did not receive a listed order, Please let us know immediately.";
                                        add eightlpi to ROW
                                        add eightlpi to ROW
                                        Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Names in the News - List Management (415)989-3350.";
                                        prtclose PrintDoc
.                                        Pause     "5"               .make sure done with file - sigh my machine is to fast dh
.begin patch 1.8
.                                        If        (EmailFlag = Yes)
                                        IF        (OUTFLAG > C1)
                                        Goto      SendItEmail                                                 
                                        else
.help
.                                        copyfile  "C:\WORK\faxfile.prn",SPOOLFLE
.                                        Erase     "C:\WORK\faxfile.prn"
.                              Copyfile            Mailattach,"\\srv2008a\laser8"
                                        call       GetPDFPath
                                        pack      APIFileName,PDFPATH,"\flag.dat",hexzero
                                        loop
                                                  call      FindFirstFile
                                        until (APIResult = 0 | APIResult = hexeight)
                                                  pause     "1"
                                        repeat
                                        pause     "5"
                                        pack      taskname from "\\nins1\winbatch\shipreq1.exe"
                                        execute   Taskname
                                        pause               "5"
                                        erase               MailAttach
.                                       Rename "C:\WORK\faxfile.prn",SPOOLFLE
.test DH
                                        Return
.test DH
.help
                                        endif
.end patch 1.8
         clear     taskname
.begin Patch 1.87
.         if        (outflag = 1)   .to printer
.                              Call GETWINVER
.                              clear     copyvar2
.                              if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
.                                        append    "!c:\winnt\system32\cmd.exe",CopyVar2
.                                        append  " /c copy ",CopyVar2
.                              elseif (OSFLAG = "6")  .XP
.                                        append    "!c:\windows\system32\cmd.exe",CopyVar2
.                                        append  " /c copy ",CopyVar2
.                              else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
.                                        append    "!c:\command.com",CopyVar2
.                                        append  " /c copy ",CopyVar2
.                              endif
.         append    spoolfle to CopyVar2
.                              append  " \\SRV2008a\laser8",CopyVar2
.                              reset   CopyVar2
.                              execute CopyVar2
.                              erase "c:\work\hdrfile.prn"
.         pause     c2
.                   if        not over            .execute was ok

.                 call      debug
.          call      Trim using Spoolfle
.               MOVELPTR  Spoolfle,n2 
.                 scan      period in spoolfle
.                 lenset    spoolfle
.                 clear     str40
.                 reset     spoolfle
.                 append    spoolfle to str40
.                 append    "SNT" to str40
.                 reset     str40
.                 reset     spoolfle,n2
.                 reset     spoolfle
.               rename    spoolfle,str40
.                   endif
.end Patch 1.87
         endif
.
         if        (outflag = 2 or outflag = 4)    .to fax or fax
.begin Patch 1.87
.                                        Call GETWINVER
.                                        clear     copyvar2
.                                        if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
.                                                  append    "!c:\winnt\system32\cmd.exe",CopyVar2
.                                                  append  " /c copy ",CopyVar2
.                                        elseif (OSFLAG = "6")  .XP
.                                                  append    "!c:\windows\system32\cmd.exe",CopyVar2
.                                                  append  " /c copy ",CopyVar2
.                                        else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
.                                                  append    "!c:\command.com",CopyVar2
.                                                  append  " /c copy ",CopyVar2
.                                        endif
.                                        append  "c:\work\hdrfile.prn /b + ",CopyVar2
.                                        append  SPOOLFLE,CopyVar2
.                                        append    " /b ",CopyVar2
.                                        append  TEMPFLE,CopyVar2
.
.                                        reset   CopyVar2
.                                        execute CopyVar2
.                                        clear   CopyVar2
.                                        erase     SPOOLFLE
.                                        rename TEMPFLE,SPOOLFLE
.                                        if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
.                                                  append    "!c:\winnt\system32\cmd.exe",CopyVar2
.                                        elseif (OSFLAG = "6")  .XP
.                                                  append    "!c:\windows\system32\cmd.exe",CopyVar2
.                                        else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
.                                                  append    "!c:\command.com",CopyVar2
.                                        endif
.                                        append  " /c copy ",CopyVar2
.                                        append  SPOOLFLE,CopyVar2
.                                        append  " \\srv2008a\fax",CopyVar2
.                                        reset   CopyVar2
.                                        execute CopyVar2
.                                        erase "c:\work\hdrfile.prn"

.         pause     c2
.                   if        not over            .execute was ok
.                 call      debug
.          call      Trim using Spoolfle
.               MOVELPTR  Spoolfle,n2 
.                 
.                 scan      period in spoolfle
.                 lenset    spoolfle
.                 clear     str40
.                 reset     spoolfle
.                 append    spoolfle to str40
.                 append    "SNT" to str40
.                 reset     str40
.                 reset     spoolfle,n2
.                 reset     spoolfle
.                     rename    spoolfle,str40
.                   endif
.end Patch 1.87
         endif
.
.begin patch 1.8         
SendItEmail
.          if        (outflag = 1)    .printint
.          Copyfile            Mailattach,"\\srv2008a\laser8"
.          pause               "5"
.          erase               MailAttach
.          endif


          if        (outflag > 1)    .to email with email addy or fax #
.It takes some time for the file to be created, so we must check
.Allow 20 seconds

. Use the Flag to check if pdf has been created
.                              pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
        call       GetPDFPath
                              pack      APIFileName,PDFPATH,"\flag.dat",hexzero
                              loop
                                        call      FindFirstFile
                              until (APIResult = 0 | APIResult = hexeight)
                                        pause     "1"
                              repeat
                              pause     "2"
                              pack      str45,PDFPATH,"\flag.dat",hexzero
                              erase     str45
.testing DH
          Clear     Mailsubjct
          append    "Order Fulfillment",MailSubjct
          append    b1,Mailsubjct
          append    Mailto,Mailsubjct
          reset     Mailsubjct
          Clear     Mailbody
          Append    MailSubjct,mailbody
          append    crlf,mailbody
          append    "Shipping Request",Mailbody
          append    crlf,mailbody
          
          reset     Mailbody
.         clear     Mailbody
.testing
.         move      "DavidHerrick@nincal.com",mailto
.........................
.begin patch 1.82
          Move      c0,TrapCount                   .reset

CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck

.          call      debug
.         Move      Yes,Mailtrace

.end patch 1.82
                    call      SendMail
                    erase     str45
                    Move      No,EmailFlag

         endif
.
         endif

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

.end patch 1.82        
.................................................................................................
PDFPREP
.First check 995 autolaunch settings
                              Call      PDF995Auto
                              call      SetPDFFlag
                              PRTOPEN PrintDoc,"PDF995","Shipping"
                              pack    str45,"Shipping.pdf"
                              PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                              *ORIENT=*Portrait:
                              *MarginL=1;
          move      c0 to page
          CALL       HEADER
          move       nownfld to holdown
          mOVE      ofullfIL,hoLDfuL     
          Return        
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
          call       sendit
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
                              PRTOPEN PrintDoc,"PDF995","Shipping"
                              pack    str45,"Shipping.pdf"
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
.Begin Patch 1.73
          Include   Ndatio.inc
.end Patch 1.73
          INCLUDE   NORDIO.inc
          include   hpio.inc
          INCLUDE   COMLOGIC.inc


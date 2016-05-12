PC        EQU       0

          INCLUDE   COMMON.INC
          INCLUDE   CONS.INC
          INCLUDE   NORDDD.INC
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
          INCLUDE   NCNTDD.INC
          INCLUDE   NOWNDD.INC
          include   hp.inc
          INCLUDE   NUSEDD.INC
          include   nspedd.inc
          include   nmdldd.inc
          INCLUDE   NOFRDD.INC
          include   nord5dd.inc
          INCLUDE   NSEL2DD.INC
        include winapi.inc
.begin patch 2.8
         INCLUDE   nsmpdd.inc
.end patch 2.8

release   init    "2.91"     DLH  break every record, print sample description on form. this is a partial release see develop
REldate   Init      "2015 December 3"    
.release   init    "2.90"     DLH  Add break by mailer
.REldate   Init      "2015 October 26"    
.release   init    "2.81"     DLH  New fax gateway
.REldate   Init      "2015 April 14"    
.release   init    "2.8"     DLH  Convert samples to pdf and include link instead of printing. see previous version for old code
.REldate   Init      "2014 December xx"    
.release   init    "2.75"     DLH  switch message type to HTML change CRLF to <br>
.REldate   Init      "2014 December 17"    
.Release   Init      "2.74"    DLH             Add LR to subject line of emails
.RelDate   Init      "2014 September 12"
.Release   Init      "2.73"    DLH             bump email var to 100
.RelDate   Init      "2014 April 1"
.Release   Init      "2.72"    ASH       ADDED LOGIC TO KEEP FROM PRINTING CLEARED OUTSIDE LCRs
.RelDate   Init      "2013 August 20"
.Release   Init      "2.71"    ASH      ADDED NEW LCR STATUS
.RelDate   Init      "2013 June 01"
.Release   Init      "2.70"    DLH      Sunbelt PDF
.RelDate   Init      "2013 April 23"
.Release   Init      "2.61"    DLH      add Spool error trap and recovery
.RelDate   Init      "23 January 2013"
.Release   Init      "2.6"    DLH      Change Break to List Owner & NIN COntact, Use NIN contact as from email
.RelDate   Init      "14 September 2012"
.Release   Init      "2.54"    DLH      Use NIN for all
.RelDate   Init      "08 January 2010"
.Release   Init      "2.53"    DLH      add please do not reply....  to email
.RelDate   Init      "19 October 2009"
.Release   Init      "2.52"    DLH      Do not increment sample counter for owners that do not allow samples
.RelDate   Init      "26 March 2009"
.Release   Init      "2.51"    DLH      Additional checking rename failures
.RelDate   Init      "12 March 2009"
.Release   Init      "2.50"    DLH      Email as well as faxing
.RelDate   Init      "09 February 2009"
.Release   Init      "2.43"    DLH 08March2007     PL Logo
.RelDate   Init      "08 March 2007"
.release  init      "2.42"        JD    17JAN2006 New Logo No main Fax# just for list clearances.
.release  init      "2.41"        JD    13DEC2005 New LCR response fax #.
.release  init      "2.4"        ASH    13JUN2005 RETIREMENT OF PCL2PDF
.release  init      "2.3"        ASH    09DEC2004 FAXFILE.PRN
.RELEASE   INIT      "2.2"         05NOV2004  ASH Sample Conversion - Increased Mailer field to 6 bytes
.RELEASE   INIT      "2.1"         28OCT2004  ASH PATCH TO CALL NMLRKEY WHEN PDFing
.RELEASE   INIT      "2.0"         26AUG2004  ASH LOGO CONVERSION - COMPLETE REWRITE
.RELEASE   INIT      "1.0"         1999 AUG 08  DLH.

.EXTERNAL ROUTINES FROM       NORDTEST.PLC
CleanUpLCRFaxFiles external "NORDTEST;CleanUpLCRFaxFiles"

. *****************************************************************************
. **** LIST  CLEARANCE  PRINT  PROGRAM  08/18/99
. *****************************************************************************
.
line1     dim       55              .Used for Printing Special Instructions
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"
REPLN1    DIM       30
QTYMASK   INIT      "ZZZ,ZZZ,ZZ9"
QTYPRNT   DIM       11
QTYPRNT2 DIM        45
smpflag   form      1         *1=there is a file
.
. FILE DEFINITIONS
.
ORDPRINT FILE       FIX=696   DAILY PRINT FILE. 434 BYTES (INCLUDES 00/99/98 TEXT)
pict1     pict
prfile    pfile
FMESG     DIM       22
STATUS    DIM       15
newdate1 DIM        10
bigdate   dim       25
owncnt    form      3         .counts number of lcr's for a particular owner.
ownscnt   form      3         .counts number of samples for a particular owner.
owncnta   form      3         .for cover sheet
ownscnta form       3         .for cover sheet
count     form      4
EXT       DIM       3
ARCD      DIM       3
PHONE     DIM       4
faxsal    dim       5         fax salutation
faxname   dim       25
faxtele   dim       10
faxattn   dim       25
HoldOcocode         Dim       2         .used for Contact Break
holdown   dim       4         .used for owner break
.begin patch 2.90
HoldComp   dim        6       .mailer company number
.end patch 2.90
faxflag   form      1         .1=no, 2=yes.
LPTCNT    FORM      4         .LENGTH OF ATTCHLST
LONGDIST DIM        1
.begin patch 2.8
.DCX       INIT      ".TIF"
DCX       INIT      ".PDF"
SMPmailBody    dim         25000       .for sample links
.end patch 2.8
intrnet   dim       50        .print contact's internet address
DCX2      dim       30
DCXFile   dim       120
SPOOLF    dim       120
FilePath DIM        45
          PACK      FILEPATH,NTWKPATH1,"SAMPLES\"                .
." just here to fix display of vars :)  
SMPArray dim        12(50)
SMPIndex form       2
EmailFlag DIm       1
FileCheck FIle
trapcount form      4
EmailAddr Dim       100
dmFileName          dim 80
StoredOLRN dim      1000
.Begin patch 2.91
SAMPLE2   DIM       26        *USED FOR ORDER PRINT
MlrNameHold         dim       75
MlrMNameHold        dim       75
.end patch 2.91

.substituting report at faxform generates and Object error at load time under 8.6
rptcan    dim       1
ProgFlag form       "0"
DimPtr    dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
FontO7              font
FontO18B  font
NINLogo   PICT
PackData Datalist                           .Holds sample data
.begin patch 2.90
.PackData2 Datalist                          .Hold NIN contact, LO, # lcr's, # samples  
PackData2 Datalist                          .Hold NIN contact, LO,mlr Comp#, # lcr's, # samples  
.end patch 2.90

x         plform    report
        formload X
...........................................................
.Create work var
.testing
.          create    PackData=1:100:1:100
.           activate packdata
.           setprop    packdata,Visible=1
          create    PackData=1:1:1:1
          create    PackData2=1:1:1:1
.

          move      "NLCR002L",PROGRAM
          move      "Names in the News",COMPNME
          move      "LCR PRINT/Fax ",STITLE
          move      "EXIT",PF5
          clock     DATE,TODAY
          call      PAINT
          call      FUNCDISP
          trap      NOFILE IF IO
          trap      EXIT IF F5
          Clear     Holdown
          Clear     HoldCOmp
....testing
.          goto        woopy
OPEN1
          display   *P1:24,*el,"Deleting \\nins1\d\data\lcrs\delete\*.*";         
          clear     Mailbody
          FIndDIr   "\\nins1\d\data\LCRs\delete\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "\\nins1\d\data\LCRS\delete\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
          display   *P1:24,*el,"moving from \\nins1\d\data\lcrs\old\*.*";         

          clear     Mailbody
          FIndDIr   "\\nins1\d\data\Lcrs\old\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      str55 from "\\nins1\d\data\lcrs\old\",DmFIleName           ."comment :)
                    pack      taskname from "\\nins1\d\data\lcrs\delete\",DmFIleName  ."comment :)
                    FindFIle  Str55
                              if        zero                .file is there
                              Rename    Str55,Taskname                          
                              endif
                    endif
          repeat
          endif
          display   *P1:24,*el,"moving from \\nins1\d\data\lcrs\sent\*.*";         

          clear     Mailbody
          FIndDIr   "\\nins1\d\data\lcrs\sent\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      str55 from "\\nins1\d\data\lcrs\Sent\",DmFIleName     ."comment :)
                    pack      taskname from "\\nins1\d\data\lcrs\old\",DmFIleName    ."comment :)
                    FindFIle  Str55
                              if        zero                .file is there
                              Rename    Str55,Taskname                          
                              endif
                    endif
          repeat
          endif
.end patch 2.5
woopy          
          display   *P1:24,*el,"OPENING FILES";
          move      "NIN PRINT FILE       ",FMESG
          pack      STR35,NTWKPATH1,"lcrprint.lcr"
.          pack      STR35,NTWKPATH1,"diskin28.lcr"
          open      ordprint,STR35
          display   *EL,*P01:24;
          trapclr   IO
READ1A
          call      SetColumnsAndFonts
.Do a preliminary read and get Counts
          move      C0,owncnt
          move      C0,ownscnt
          loop
                    read      ORDPRINT,SEQ;ORDVARS
                    until over
                    if (OHIST <> "t" & OHIST <> "E")
                                  call      Trim using OLRN
                                  call      Trim using OLON
.begin patch 2.90 .we need company # as well
.begin patch 2.90
                    pack      MKEY,OMLRNUM,z3
                    move      "Read1A-NMLRKEY",Location
                    call      NMLRKEY

.                                  if (OLRN <> "" & OLON <> "")
.                                        if (OLON <> holdown | OCOCODE <> HoldOCOCODE)
                                  if (OLRN <> "" & OLON <> "" & Compnum <> "")
                                        if (OLON <> holdown | OCOCODE <> HoldOCOCODE  | Compnum <> HoldComp)
                                                  if (holdown <> "")
.                                                            pack      taskname,HoldOCOCODE,holdown,owncnt,ownscnt
                                                            pack      taskname,HoldOCOCODE,holdown,HoldCOmp,owncnt,ownscnt
                                                            insertitem PackData2,999999,taskname
                                                            move      C0,owncnt
                                                            move      C0,ownscnt
.begin patch 2.8  add contact and owner to datalist do not destroy it later
.                                                            deleteitem PackData,0
.end patch 2.8  add contact and owner to datalist do not destroy it later
                                                  endif
                                        move      OLON,holdown
                                        Move      OCOCODE,HoldOCOCODE
                                        move      compnum,holdcomp     
.end patch 2.90 .we need company # as well
                              
                                              endif
                                              add       C1,owncnt
                                         IF         (OSCODE = "1")           .sample enclosed
                                 
                                                  trap      IOMssg giving Error if IO
                                              move      "COMPKEY3",Location
                                              pack      COMPFLD3,OMLRNUM
                                              pack      KeyLocation,"Key: ",COMPFLD3
                                              call      COMPKEY3
.begin patch 2.8  add contact and owner to datalist do not destroy it later
.                                              pack      str12,COMPNUM,OSAMCDE
.                                        PackData.FindString giving result using str12,SEQ
                                 call debug
                                            call       trim sing osamcde
                                              pack      str18,HoldOCOCODE,holdown,COMPNUM,OSAMCDE
                                        PackData.FindString giving result using str18,SEQ

.                                        if (result = SEQ & OWNBLK <> "1")             .ownblk = 1 means owner does not allow samples
                                        if (result = SEQ & OWNBLK <> "1" & Osamcde <> ""  & Osamcde <> "000")             .ownblk = 1 means owner does not allow samples
.                                                  insertitem PackData,999999,str12
                                                  insertitem PackData,999999,str18
.end patch 2.8  add contact and owner to datalist do not destroy it later
                                                  add       C1,ownscnt
                                        endif    
                                        endif
                              endif
                    endif
          repeat
.begin patch 2.90
.          pack      taskname,HoldOCOCODE,holdown,owncnt,ownscnt
          pack      taskname,HoldOCOCODE,holdown,holdcomp,owncnt,ownscnt
.end patch 2.90
          insertitem PackData2,999999,taskname
.
READ1
.Reopen job and actually create
          close     ordprint
          open      ordprint,STR35
.Initialize a bunch of flags
.begin patch 2.8  add contact and owner to datalist do not destroy it later
.          deleteitem PackData,0
.end patch 2.8  add contact and owner to datalist do not destroy it later
          pack      holdown,B55
          move      C0,count
          move      C0,owncnt
          move      C0,ownscnt
          move      C0,FaxFlag
          loop
                    read      ORDPRINT,SEQ;ORDVARS
                    until over
                    if (OHIST <> "t" & OHIST <> "E")
                    if        (olrn = "775315")
                    call      debug
                    endif
                    display   *p10:12,count,b1,OLRN,b1,OLON;
                    call      Trim using OLRN
                    call      Trim using OLON
.begin patch 2.90
                    pack      MKEY,OMLRNUM,z3
                    move      "Read1-NMLRKEY",Location
                    call      NMLRKEY

.                    if (OLRN <> "" & OLON <> "")
.                              if (OLON <> holdown | OCOCODE <> HoldOCOCODE)
                    if (OLRN <> "" & OLON <> "" & Compnum <> "")
                              if (OLON <> holdown | OCOCODE <> HoldOCOCODE | compnum <> HoldComp)
.end patch 2.90
                                        if (count > C0)

                                                  call      OwnerBreak
                                        endif
                                        call      PrepOpenPrint
                                        ADD       C1,OWNCNT
                              endif
                              if (OSTAT = "z")
                                        move      "** CANCELLED **",STATUS
                              else
                                        move      "               ",STATUS
                              endif
... add if count > 0  new page else skip??????
                              if (count > C0)
                              prtpage   prfile;*NEWPAGE;
                              endif
                              call      Print
                    endif
                    add       C1,count
                    endif
          repeat

          call      OwnerBreak
          shutdown

Print
          move      OODTEM,mm
          move      OODTED,dd
          move      OODTEC,cc
          move      OODTEY,yy
          move      MM,N2
          load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                    m07,m08,m09,m010,m011,m012
          move      DD,STR2
          reset     STR2,1
          setlptr   STR2,1
          rep       "0 ",STR2
          setlptr   STR2
          clear     bigDATE
          append    STR9,bigDATE
          append    B1,bigDATE
          append    STR2,bigDATE
          append    B1,bigDATE
          append    ",",bigDATE
          append    cc,bigDATE
          append    YY,bigDATE
          reset     bigDATE
          move      QTYMASK,QTYPRNT
          move      C0,result
          move      OQTY,result
          if (OELCODE = "1" OR OELCODE = "3")
                    if (result > 0 & (OCO2CODE = "" | OCO2CODE = "  "))
                              edit      result,QTYPRNT
                              call      Trim using QTYPRNT
                              pack      QTYPRNT2,"ALL/ ",QTYPRNT," Please advise actual quantity."
                    else
                              move      "    ALL",QTYPRNT2
                    endif
          else
                    edit      result,QTYPRNT
                    move      QTYPRNT,QTYPRNT2
          endif
          move      OLNUM,NMDLFLD
          rep       zfill,NMDLFLD
          clear     mdlcall
          move      "Print-NMDLKEY",Location
          pack      KeyLocation,"Key: ",NMDLFLD
          CALL      NMDLKEY
..............
. DETERMINE  REP NAME.
.
SVCREP
          clear     intrnet
          pack      REPLN1,B10,B10,B10
          pack      NCNTFLD,OCOCODE
          move      "SVCREP-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          call      NCNTKEY
          if not over
SVCREP2
                    move      "NIN Contact,",REPLN1
                    move      CNTNAME,str55
                    scan      "BILLING",str55
                    if not equal
                              call      RemoveChar using str55,B1
                              call      Trim using str55
                              if (str55 <> "")
                                        pack      intrnet,str55,"@nincal.com"
                              endif
                    endif
          endif
          if (ProgFlag = 1)   .Single LCR Report called from other Program
                    move      OLON,NOWNFLD
                    rep       zfill,NOWNFLD
                    call      NOWNKEY
                    pack      MKEY,OMLRNUM,z3
                    move      "SVCREP2-NMLRKEY",Location
                    call      NMLRKEY
                    call      DISNIN
                    return
          endif
          call      Process1
          return

OwnerBreak
          display   *p1:24,*el,"Break"
.begin patch 2.8
.          call      CreateSamples
.end patch 2.8
          prtclose prfile
.Combine files here
          if (faxflag = C2)
          Clear     Taskname
          Clear     Str55
          Clear     Str35
          pack      str35 from "c:\work\pdf\faxfile.pdf"
          rep       Zfill,Holdown
          move      str35,str55
          MOVE      C0,TrapCount

          
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          Pause     "5"
.begin patch 2.90
.          pack      str55 from "c:\work\pdf\Lcr",HoldOcocode,Holdown,".pdf"
          rep       Zfill,Holdcomp
          pack      str55 from "c:\work\pdf\Lcr",HoldOcocode,Holdown,HoldComp,".pdf"
.end patch 2.90
          rename    str35,str55

          MOVE      C0,TrapCount
Checkfile2
          trap      WaitForEnd2 giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          Pack       MailSubjct,"List Clearance Requests: ",StoredOLRN
           pack       StoredOLRN,B55,B55,B55,B55
           clear      StoredOLRN
          pack      MailAttach from str55

        
          move      emailaddr to mailto
          Move      intrnet,Mailfrom          
          IF        (FrmPTr = "2")            .FRom program 1 send to requestor
          CLEAR     MailTO
          append    User,Mailto
          append    "@nincal.com",Mailto
          reset     Mailto
          Else                                    .live run
          MOve      "ComputerRequest@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
          endif
                    Move      "30",MailTimer
                    pause     "5"
                    move      c1,MailType         .force e-mail body to HTML message
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  

                    Move      "360",MailTimer                 
                    call      SendMail
Daver               pause     "10"
.         after sending copy to folder
.begin patch 2.90
.          pack      taskname from "\\nins1\d\data\Lcrs\sent\Lcr",HoldOcocode,Holdown,".pdf"
          pack      taskname from "\\nins1\d\data\Lcrs\sent\Lcr",HoldOcocode,Holdown,HoldCOmp,".pdf"
.end patch 2.90
          copyfile  str55,taskname
          erase     str55
          return
          Else
          
          endif
          return

PrepOpenPrint
          move      OLON,NOWNFLD
          rep       zfill in NOWNFLD
          move      "PrepOpen-NOWNKEY",Location
          pack      KeyLocation,"Key: ",NOWNFLD
          call      NOWNKEY             .make sure we have last owners info
          if not over
.START TESTING!!!!
.          move      "5106288313",ownfax
.END TESTING!!!!
          move      ownocpy,faxname
          move      OWNFAX,FAXTELE
          move      ownlonm,faxattn
.lets check for email first
          Clear      Emailaddr
          call      Trim using OwnEmail
                    if        (Ownemail <> "")               .we have something
                              scan      "@",Ownemail
                              if        equal                         .presuming valid
                                       reset     Ownemail
                                       
                                       move      Ownemail,Emailaddr
                                       move      c2,Faxflag                    .send it
.
                                       move       Yes,EmailFlag              .used for cover sheet
.
                              endif
                                        call      OpenPrintFile using C4
                                        move      c2,faxflag        .do fax.
                                        display   *p1:24,*el,"send\fax it"

                    Goto      ContOwnPRep
                    
                    Endif
.
                    count     N2,faxtele
                    compare   C10,N2
                              if equal
                                        move      C1,LONGDIST
                                        unpack    faxtele,STR3,STR7
                                        match     "510",STR3           .LOCAL ?
                                        if equal
                                                  move      STR7,faxtele
                                                  clear     LONGDIST
                                        else
                                                  match     B3,STR3           .LOCAL ?
                                                  if equal
                                                            move      STR7,faxtele
                                                            clear     LONGDIST
                                                  endif
                                        endif
.                              pack      EmailAddr from"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
                              pack      EmailAddr from "+",longdist,faxtele,"@fax.nincal.com"
                              move       No,EmailFlag              .used for cover sheet
                              endif
                    display   *p1:24,*el,"checking owner ",ownocpy
                    type      OWNFAX                             .valid fax #?
                              if not equal
.printing not faxing - no or invalid fax number
                                        call      OpenPrintFile using C5
                                        display   *p1:24,*el,"no fax number"
                                        move      c1,faxflag        .
                              elseif (OWNFAX = "0000000000")
                                        call      OpenPrintFile using C5
                                        display   *p1:24,*el,"invalid fax number"
                                        move      c1,faxflag
                              else
                                        call      OpenPrintFile using C4
                                        move      c2,faxflag        .do fax.
                                        display   *p1:24,*el,"send\fax it"
                              endif
          else
.printing not faxing - no or invalid fax number
                    call      OpenPrintFile using C5
                    display   *p1:24,*el,"invalid owner number"
                    move      c1,faxflag        .
          endif
ContOwnPrep
          move      c0,owncnt           .reset count for cover sheet.
          move      c0,ownscnt                    .reset count for cover sheet.
          move      OLON,holdown
          Move      Ococode,HoldOcocode
.begin patch 2.9
          Move        Compnum,HoldCOmp 
.end patch 2.9
.begin patch 2.8
.
          call      CreateSamples
          call      PrintCover
.end patch 2.8
          if (ProgFlag = C0)  .Batch run only
.begin patch 2.8  add contact and owner to datalist do not destroy it later
.                    deleteitem PackData,0
.end patch 2.8  add contact and owner to datalist do not destroy it later
          endif
          return

PrintCover
           move      C0,owncnta
          move      C0,ownscnta
          if (ProgFlag = C0)  .Batch run only
                    PackData2.GetCount giving howmany
                    if (howmany > C0)
                              for result,"1",howmany
                                        getitem   PackData2,result,taskname
.begin patch 2.90
.                                        unpack    taskname,str2,str4
.                                        if (str4 = OLON & str2 = ococode)
.                                                  unpack    taskname,str2,str4,owncnta,ownscnta
                                        unpack    taskname,str2,str4,str6
                                        if (str4 = OLON & str2 = ococode & str6 = compnum)
                                                  unpack    taskname,str2,str4,str6,owncnta,ownscnta
.end patch 2.90
                                                  break
                                        endif
                              repeat
                    endif
          else
                    move      C1,owncnta
                    move      C0,ownscnta
          endif
. no longer printed will be coverpage if fax/email
          Clear     Mailbody
          if        (faxflag = 2)               email or fax
.NIN info moved to bottom version 2.8
          Append    "List Clearances,  ",Mailbody
          Append    "Date: ",mailbody
          append    today,mailbody
.          append    "<br>",Mailbody
          append    "<br>",Mailbody
                    if        (EmailFlag = Yes)        
                    Append    "   VIA Email ",Mailbody
                    Else
                    Append    "   VIA FACSIMILE ",Mailbody
                    endif     
.          append    "<br>",Mailbody
.          Append    "Date: ",mailbody
.          append    today,mailbody
          append    "<br>",Mailbody
          append    "To: ",mailbody
          append    faxname,mailbody
.          append    "<br>",Mailbody
          call        trim using faxattn 
          if (faxattn <> "")
                    append    ",      Attn: ",mailbody
                    append    faxattn,mailbody
                    append    "<br>",Mailbody
          endif

          append    "<br>",Mailbody
          append    "<br>",Mailbody
          Append    owncnta,Mailbody
          append    " Request(s) Enclosed",mailbody
          append    "<br>",Mailbody
          
                    if (ownscnta > C0)
                    append    "<br>",Mailbody
                    append    ownscnta,Mailbody
                    append    " Link(s) to Sample(s) Enclosed",Mailbody
                    append    "<br>",Mailbody
.begin patch 2.8
                      reset      smpmailbody
                      append     smpmailbody,mailbody
                    append    "<br>",Mailbody                  
.end patch 2.8
                    endif
                    if (owncnta <> 1 & ownscnta > C0)
                    append    "<br>",Mailbody
                    append    "<br>",Mailbody
                    Append    "Note: Sample(s) may need to be attached to multiple requests.",Mailbody
                    append    "<br>",Mailbody                  
                    endif
          append    "Please note the fax number (510-302-4632) for all clearance responses.",mailbody
          append    "<br>",Mailbody                  
                    Append    "We will continue to use 415-433-7796 as the general fax number.",mailbody         
          append    "<br>",Mailbody
          append    "<br>",Mailbody
          Append    "Please call if you do not receive all pages.",mailbody         
          append    "<br>",Mailbody

.Begin patch 2.8
                    IF        (OCompid2 = "P")
                              Append    "Names in the News                      Pacific Lists, Inc.",Mailbody
                              append    "<br>",Mailbody
                              append    "180 Grand Avenue, Suite 1545           180 Grand Avenue, Suite 1545",Mailbody
                              append    "<br>",Mailbody
                              append    "Oakland, CA 94612-3799                           Oakland, CA 94612-3799",Mailbody
                              append    "<br>",Mailbody
                              append    "415-989-3350 * Fax 415-443-7796        415-945-9450 * Fax 415-945-9451",Mailbody
                              append    "<br>",Mailbody
          
                    Else
                              append    "<a href=#"http://www.nincal.com#">Names in the News</a>",Mailbody
                              append    "<br>",Mailbody
                              append    "180 Grand Avenue, Suite 1545",Mailbody
                              append    "<br>",Mailbody
                              append    "Oakland, CA 94612-3799",Mailbody
                              append    "<br>",Mailbody
                              append    "415-989-3350 * Fax 415-443-7796",Mailbody
                              append    "<br>",Mailbody
                    endif
.end patch 2.8
          reset     Mailbody          
          

          Else                                    .we are printing
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.
          move      "1500",row
          prtpage   prfile;*pcolumn7:row,*font=font3,"LIST CLEARANCES"
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
.
          if        (EmailFlag = Yes)        
          prtpage   prfile;*pcolumn7:row,*font=font4,"   VIA Email"
          else
          prtpage   prfile;*pcolumn7:row,*font=font4,"   VIA FACSIMILE"
          endif     
.
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,*font=font6,"Date:"
          prtpage   prfile;*pcolumn3:row,today
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"To:"
          prtpage   prfile;*pcolumn3:row,faxname
          call      Trim using faxattn
          if (faxattn <> "")
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Attn:"
                    prtpage   prfile;*pcolumn3:row,faxattn
          endif
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"From:"
          prtpage   prfile;*pcolumn3:row,"Requests"
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,owncnta," Request(s) Enclosed"
          if (ownscnta > C0)
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,ownscnta," Sample(s) Enclosed"
          endif

          if (owncnta <> 1 & ownscnta > C0)
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Note: Sample(s) may need to be attached to multiple requests."
          endif
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"Please note the fax number (510-302-4632) for all clearance responses." 
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"We will continue to use 415-433-7796 as the general fax number."
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"Please call if you do not receive all pages."
          endif
          return

..........................................................................................................................
CreateSamples
.begin patch 2.8
.          compare   c2,faxflag
.          if equal          .yes
          call        SMPRecClear
.end patch 2.8
                    PackData.GetCount giving howmany
                    if (howmany > C0)
.                             add       howmany,ownpcnt
                              for result,"1",howmany
.begin patch 2.8  add contact and owner to datalist 
.                                        getitem   PackData,result,DCX2
                                        getitem   PackData,result,taskname
                                        unpack         taskname into str2,str4,dcx2
.begin patch 2.90
                                        unpack      dcx2 into str6
                                        if (str4 = OLON & str2 = ococode & str6 = Compnum)
                                         
.                                        if (str4 = OLON & str2 = ococode)
.end patch 2.90
.end patch 2.8  add contact and owner to datalist 
.begin patch 2.8
                                        pack      DCXFile,filePath,"s",DCX2,DCX
.
                                                      move      "SAM1-COMPKEY",Location
                                                      unpack      dcx2 into compnum,osamcde 
                                                      pack      COMPFLD,compnum
                                                      pack      KeyLocation,"Key: ",COMPFLD
                                                      call      COMPKEY
                                                      pack      NSMPFLD,COMPNUM,OSAMCDE
                                                      rep       zfill,NSMPFLD
                                                      move      "SAM1-NSMPKEY",Location
                                                      pack      KeyLocation,"Key: ",NSMPFLD
                                                      call      NSMPKEY
                                                call   trim using compcomp
                                                call   trim using nsmpdes1
                                                clear   taskname
                                                pack    Taskname from "<a href=#"http://www.nincal.com/data/samples/S",dcx2,dcx,"#">",compcomp,dash,nsmpdes1,"</a>"
                                                append  Taskname,SmpMailBody
                                                append  "<br>",SmpMailBody
                                            endif
Skipit

..Print and Display First Page
.                                        clear     N9
.                                        CREATE    PICT1=70:495:70:620:
.                                                  DCXFile,BORDER=0,AUTOZOOM=0
.                                        PICT1.GetPageCount GIVING N9
.                                        prtpage   prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:10500:1:8000:PICT1;
..Print and Display Additional Pages
.                                        if (N9 > C1)    .Only Enter loop if more than one page
.                                                  clear     N8
.                                                  move      C1,N8   .Start with SECOND PAGE as first page already printed
.                                                  loop
.                                                            add       C1,N8
.                                                            until (N8 > N9)
.                                                            CREATE    PICT1=70:495:70:620:
.                                                                      DCXFile,BORDER=0,AUTOZOOM=0,PAGE=N8
.                                                            activate PICT1
.                                                            prtpage   prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:10500:1:8000:PICT1;
.                                                  repeat
.                                        endif
.end patch 2.8
                              repeat
                    endif
.begin patch 2.8
.          endif
.end patch 2.8
          return

Process1
                    call Trim using StoredOLRN
                    if (StoredOLRN <> "")
                              pack      StoredOLRN,StoredOLRN,", "
                    endif
                    pack      StoredOLRN,StoredOLRN,OLRN



          pack      MKEY,OMLRNUM,z3
          move      "process1-NMLRKEY",Location
          call      NMLRKEY
.
.begin patch 2.8 already have data don't do again
.          type      OSAMCDE
.          if equal
.                    match     "000",OSAMCDE
.                    if not equal
.                              move      "COMPKEY3-2",Location
.                              pack      COMPFLD3,OMLRNUM
.                              pack      KeyLocation,"Key: ",COMPFLD3
.                              call      COMPKEY3
.                              pack      str12,COMPNUM,OSAMCDE
.                              PackData.FindString giving result using str12,SEQ
.                              if (result = SEQ)
.                                        insertitem PackData,999999,str12
.                              endif
.                    endif
.          endif
.end patch 2.8 already have data don't do again
DISNIN
          move      OLRN,NSPEFLD
          move      "DISNIN-NSPEKEY",Location
          call      NSPEKEY
Process2
.Rental or Exchange
          clear     str18
          if (ORENT = "1")
                    if (OELCODE = "2" OR OELCODE = "3")
                              append    "RENT/EXC",str18
                    else
                              append    "RENTAL",str18
                    endif
          else
                    append    "EXCHANGE",str18
          endif
          reset     str18
.Offer
          reset     OODNUM
          bump      OODNUM,4
          move      OODNUM,str4
          pack      NOFRFLD,OMLRNUM,str4
          rep       zfill,NOFRFLD
          move      "O.LoadOffer-NOFRKEY",Location
          call      NOFRKEY
.LCR Sub-Status
          move      OLRN,NORD5FLD
          rep       zfill in NORD5FLD
          move      "O.PendingStatus-NORD5KEY",Location
          call      NORD5KEY
.
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          clear     ARCD
          clear     EXT
          clear     PHONE
          clear     FAXSAL
          unpack    ownfax,ARCD,EXT,PHONE
          cmatch    b1,phone
          if not equal
                    move      "Fax: ",faxsal
          endif
          move      "1500",row
          if (NORD5STAT = "02")
                    prtpage   prfile;*pcolumn7:row,*font=font4,*ULON,"Second Request",*ULOFF
          elseif (NORD5STAT = "03")
                    prtpage   prfile;*pcolumn7:row,*font=font4,*ULON,"Request Revision",*ULOFF;
          endif
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          call      Trim using OWNLOCTY
          clear     taskname
          if (OWNLOCTY <> "")
                    pack      taskname,OWNlOCTY,COMMA,B1,OWNlOS,B1,OWNlOZC
          endif
          clear     str24
          call      Trim using OWNTELE
          if (OWNTELE <> "")
                    count     result,OWNTELE
                    if (result = 10)
                              unpack    OWNTELE,str2,str1
                              append    str2,str24
                              append    str1,str24
                              append    " ",str24
                              bump      OWNTELE,3
                              unpack    OWNTELE,str2,str1,str6
                              append    str2,str24
                              append    str1,str24
                              append    " ",str24
                              append    str6,str24
                              reset     str24
                    elseif (result = 7)
                              unpack    OWNTELE,str2,str1,str6
                              append    str2,str24
                              append    str1,str24
                              append    " ",str24
                              append    str6,str24
                              reset     str24
                    endif
          endif
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
          if (OCO2CODE <> "" & OCO2CODE <> "  ")
                    prtpage   prfile;*pcolumn1:row,*font=font2,OWNlONM
                    prtpage   prfile;*pcolumn9:row,BIGDATE
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,OWNOCPY
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,OWNlOSA
                    prtpage   prfile;*pcolumn9:row,*font=font1,"LR ## ",OLRN,*font=font2
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,taskname
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,str24
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,faxsal,ARCD,B1,EXT,B1,PHONE
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn4:row,*font=font3,*ULON,"REQUEST FOR LIST APPROVAL",*ULOFF,*font=font2
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"List:"
                    prtpage   prfile;*pcolumn3:row,O1DES
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Select:"
                    prtpage   prfile;*pcolumn3:row,NSEL2NAME
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Mailer:"
                    prtpage   prfile;*pcolumn3:row,MCOMP
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Offer:"
                    prtpage   prfile;*pcolumn3:row,OFDESC
..begin patch 2.91
                      call       Sample
.end patch  2.91                    
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn3:row,"Quantity"
                    prtpage   prfile;*p3750:row,"Mail Period Requested"
                    prtpage   prfile;*pcolumn10:row,"Order Type"
                    add       eightlpi,row
          else
                    prtpage   prfile;*pcolumn1:row,*font=font2,OWNlONM
                    prtpage   prfile;*pcolumn9:row,BIGDATE
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,OWNOCPY
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,OWNlOSA
                    prtpage   prfile;*pcolumn9:row,*font=font1,"LR ## ",OLRN,*font=font2
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,taskname
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,str24
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,faxsal,ARCD,B1,EXT,B1,PHONE
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn4:row,*font=font3,*ULON,"REQUEST FOR LIST APPROVAL",*ULOFF,*font=font2
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"List:"
                    prtpage   prfile;*pcolumn3:row,O1DES
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Select:"
                    prtpage   prfile;*pcolumn3:row,NSEL2NAME
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Mailer:"
                    prtpage   prfile;*pcolumn3:row,MCOMP
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Offer:"
                    prtpage   prfile;*pcolumn3:row,OFDESC
..begin patch 2.91
                      call       Sample
.end patch  2.91                    
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn:row,"Mail Period Requested"
                    prtpage   prfile;*pcolumn5:row,"Quantity"
                    prtpage   prfile;*pcolumn11:row,"Order Type"
                    add       eightlpi,row
          endif
          pack      newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          if (newdate1 = "00/00/0000")
                    if (OCO2CODE <> "" & OCO2CODE <> "  ")
                              prtpage   prfile;*pcolumn3:row,QTYPRNT2
                              prtpage   prfile;*p3750:row,"As Soon As Possible"
                              prtpage   prfile;*pcolumn10:row,str18
                    else
                              prtpage   prfile;*pcolumn:row,"As Soon As Possible"
                              prtpage   prfile;*pcolumn5:row,QTYPRNT2
                              prtpage   prfile;*pcolumn11:row,str18
                    endif
          elseif (newdate1 = "11/11/1111")
                    if (OCO2CODE <> "" & OCO2CODE <> "  ")
                              prtpage   prfile;*pcolumn3:row,QTYPRNT2
                              prtpage   prfile;*p3750:row,"See Special Instructions"
                              prtpage   prfile;*pcolumn10:row,str18
                    else
                              prtpage   prfile;*pcolumn:row,"See Special Instructions"
                              prtpage   prfile;*pcolumn5:row,QTYPRNT2
                              prtpage   prfile;*pcolumn11:row,str18
                    endif
          else
                    if (OCO2CODE <> "" & OCO2CODE <> "  ")
                              prtpage   prfile;*pcolumn3:row,QTYPRNT2
                              prtpage   prfile;*p3750:row,newdate1
                              prtpage   prfile;*pcolumn10:row,str18
                    else
                              prtpage   prfile;*pcolumn:row,newdate1
                              prtpage   prfile;*pcolumn5:row,QTYPRNT2
                              prtpage   prfile;*pcolumn11:row,str18
                    endif
          endif
          add       eightlpi,row
          add       eightlpi,row
          call      OrderPrintLCRSpecialInstructions
          prtpage   prfile;*p500:7625,*font=font2,*LINE=7500:7625
.         move      "8000",row
          move      "7750",row
          prtpage   prfile;*pcolumn6:row,*font=font3,"CLEARANCE APPROVAL",*font=font2
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn4:row,"Please fill in below and fax to (510)302-4632"
          add       sixlpi,row
          prtpage   prfile;*pcolumn2:row,"____Approved"
          prtpage   prfile;*pcolumn8:row,"____Rental"
          prtpage   prfile;*pcolumn10:row,"____Exchange"
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn2:row,"____Not Approved and reason:________________________________________"
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn2:row,"    Signature:______________________________________________________"
PRTREST1
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*p500:row,*font=font2,*LINE=7500:row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,REPLN1
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,CNTNAME
          prtpage   prfile;*pcolumn10:row,MDLCALL
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,*font=font1,cntphone
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,intrnet
.
          if (ProgFlag = 1)
                    return
          endif
          DISPLAY   *P01:24,*EL
WIPEVARS
          CLEAR     OMLRNUM
          CLEAR     OCOBN
          CLEAR     OLNUM
          CLEAR     O2DES
          CLEAR     OELCODE
          CLEAR     OQTY
          CLEAR     OODTEM
          CLEAR     OODTED
          CLEAR     OODTEC
          CLEAR     OODTEY
          CLEAR     OMDTEM
          CLEAR     OMDTED
          CLEAR     OMDTEC
          CLEAR     OMDTEY
          CLEAR     OSAMCDE
          clear     OODNUM
          clear     O1DES
          clear     DESC001
          clear     DESC002
          return

OrderPrintLCRSpecialInstructions
          call      TRIM using DESC002
          move      C0,howmany
          loop
                    call      PARSITUP using line1,DESC002,C1
                    prtpage   prfile;*pcolumn1:row,*font=font5,line1
                    add       sixlpi,row
                    add       C1,howmany
                    until (howmany >= 14)
          repeat
.Print XSTAT last
          call      TRIM using DESC001
          call      PARSITUP using line1,DESC001,C1
          prtpage   prfile;*pcolumn1:row,*font=font5,line1
          add       sixlpi,row
          call      PARSITUP using line1,DESC001,C1
          prtpage   prfile;*pcolumn1:row,*font=font5,line1
          return
......................................
NOFILE
          DISPLAY   *P12:20,FMESG,"NOT ON-LINE ",error;
          TRAPCLR   IO
          KEYIN     *P12:21,str1;
          CMATCH    "Q",str1
          GOTO CONTACT IF EQUAL
          GOTO NOFILE
.
CONTACT
          KEYIN     *P1:23,"PLEASE LEAVE THIS INFORMATION ON THE SCREEN ":
                    "AND INFORM THE COMPUTER PERSONEL. ",str1;
.
          CMATCH    "Q",str1
          GOTO CONTACT IF EOS
          GOTO CONTACT IF NOT EQUAL
          SHUTDOWN  "CLS"
          STOP
DREWBREAK
          NORETURN
          TRAP      zero giving error if io
          SCAN      "I03" IN ERROR
          GOTO ZERO
ZERO
          DISPLAY   *P1:24,"UNDEFINED FILE ERROR",*W2;
          SHUTDOWN "CLS"
.
EXIT
          DISPLAY   *P1:24,*EL,*HON,"Exit Subroutine. SHUTTING DOWN TO CONTINUE":
                    " CHAIN",*W8;
          SHUTDOWN "CLS"

PrintSingleLCR Routine DimPtr,FrmPtr,FrmPtr1
.DimPtr  = LCR Number
.FrmPtr  = Printer
.FrmPtr1 = PORTN
          move      FrmPtr1,PORTN
          move      C1,NORDPATH
          move      C1,ProgFlag
          call      Trim using DimPtr
          if (DimPtr <> "")
                    pack      NORDFLD,DimPtr
                    move      "NORDKEY",Location
                    pack      KeyLocation,"Key: ",NORDFLD
                    call      NORDKEY
                    if not over
                              call      SetColumnsAndFonts
                              Trap      SpoolErr if Spool
                              if (FrmPtr = 4)               .Dynamic Faxing
                                        call      PrepOpenPrint
                              else

                                        call      OpenPrintFile using FrmPtr
                              endif
                              call      Print
                              prtclose prfile
                              if (FrmPtr = 2 | FrmPtr = 4)            .2=PDF, 4=Fax
.Clean up pre-existing files!
                                        call      CleanUpLCRFaxFiles
.
...............................
                                        if (FrmPtr = 4)
.DH is this the prob 5/10/12
.                                                  pack      APIFileName,"c:\work\faxfile.prn",HexZero
.                                                  call      FindFirstFile
.                                                  if (APIResult = 0 | APIResult = hexeight)
..File not created!
.                                                            pack      taskname,"File Creation Failed!  Fax will not be sent!"
.                                                            alert     caution,taskname,result
.                                                            return
.DH is this the prob 5/10/12
                                                  endif
.DH is this the prob 5/10/12
.                                        elseif (FrmPtr = 2)
                                        elseif (FrmPtr = 2 or FrmPtr = 4 )
.DH is this the prob 5/10/12
                                                  call      waitin using "10"
                                                  pack      APIFileName,"c:\work\pdf\lcrfax.pdf",HexZero
                                                  call      FindFirstFile
                                                  if (APIResult = 0 | APIResult = hexeight)
.File not created!
                                                            pack      taskname,"PDF File Creation Failed!  .PDF File not created!"
                                                            alert     caution,taskname,result
                                                            return
                                                  endif
                                        endif
.
                                        if (FrmPtr = 2)                         .PDF
                                                  move      C0,NUSEFLD
                                                  move      C1,NUSEPATH
                                                  move      PORTN,NUSEFLD
                                                  rep       zfill,NUSEFLD
                                                  call      NUSEKEY
                                                  call      Trim using NUSEUSER
                                                  scan      "BILLING",NUSEUSER
                                                  if not equal
                                                            move      NUSEUSER,str1
                                                            loop
                                                                      bump      NUSEUSER,1
                                                                      cmatch    B1,NUSEUSER
                                                                      until equal
                                                                      until eos
                                                            repeat
                                                            if not eos
                                                                      bump      NUSEUSER,1
                                                                      move      NUSEUSER,str6
                                                                      clear     str7
                                                                      pack      str7,str1,str6
                                                            endif
                                                  endif
.
........................................................
.Mail it back to them
                                                  pack      str35,"lcrfax",OLRN,".PDF"
.here
                                                  pack      taskname,"c:\work\",str35
."
.Give the email a chance of rendering
.

                                                  pause     "5"
.
                                                  pack      str25,"c:\work\pdf\lcrfax.pdf"
                                                  copyfile str25,taskname
                                                  erase     str25
                                        pack      str45,str7,"@nincal.com"
.
.
                                                  move      "Here is your PDF File.",MailSubjct
.   Set   the text message that is senT with the attachments
                                                  pack      MailBOdy,taskname

                                                  move      str45,MailFrom
                                                  move      str45,MailTO                                                .User name
                                                  Pack      MailAttach from "c:\work\",str35                  ."
                                                         move      c1,MailType         .The e-mail body is a text message
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  
                    Move      "360",MailTimer                 
                                                  call      SendMail
                                        elseif (FrmPtr = 4)                     .Fax
.
                                        endif
.DH is this the prob 5/10/12
.                              endif
.DH is this the prob 5/10/12
                    endif
          endif
          return

SetColumnsAndFonts
.Set up columns
          move      "750",column
          move      "1250",column1
          move      "1750",column2
          move      "2000",column3
          move      "2500",column4
          move      "2750",column5
          move      "2875",column6
          move      "3125",column7
          move      "4500",column8
          move      "5250",column9
          move      "5750",column10
          move      "6000",column11
          create    font1,"Arial",size=9
          create    font2,"Arial",size=10
          create    font3,"Helvetica",size=14,bold
          create    font4,"Helvetica",size=14,italic
          create    font5,"Fixed",size=12
          create    font6,"Arial",size=12
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold

          create    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN LCR logo black outline.jpg"
          return

OpenPrintFile Routine FrmPtr
.FrmPtr  = Printer
          call      GetWinVer
          Trap      SpoolErr if Spool
          if (FrmPtr = C0 | FrmPtr = 3)     .Laser3 = Default
                              if (osflag >= c6)         .
                              PRTOPEN prfile,"\\NINS2\Laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
          elseif (FrmPtr = 1)  .Laser3
                              if (osflag >= c6)         .
                              PRTOPEN prfile,"\\NINS2\laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag =c4)         .win 95 98
                              PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
          elseif (FrmPtr = 2)  .PDF
.
..This will only be called via Program 1.  Reference to Faxfile.prn will be to a different location.
..Users access c:\work\faxfile.prn.  Network accesses \\nins1\d\data\fax\faxfile.prn
.
.......................................
.This will only be called via Program 1.
.
                    pack      str25,"lcrfax"
                    pack      str55,"c:\work\pdf\lcrfax.pdf"
                    PRTOPEN prfile,"PDF:",str55
          elseif (FrmPtr = 4)
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
                    move      "                                        ",APIFileName
                    clear     APIFileName
          pack      taskname from "c:\work\pdf\faxfile.pdf"

          PRTOPEN   prfile,"PDF:",taskname

          elseif (FrmPtr = 5)  .Laser8
                              if (osflag >= c6)         .
                              PRTOPEN prfile,"\\NINS2\laser8","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag =c4)         .win 95 98
                              PRTOPEN prfile,"Laser8","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
          endif
          prtpage   prfile;*UNITS=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon;
          return
SpoolErr  Trapclr   Spool
          Trap      SpoolErr if Spool
          PRTOPEN prfile,"-","FAXFILE.PRN"
          return
.
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"LCRS - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    append    "<br>",Mailbody
                    append    str55,MailBody
                    append    "<br>",Mailbody
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    move      c1,MailType         .The e-mail body is a text message
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  
                    Move      "360",MailTimer                 
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
.
.DH goes bad checking for failed rename
WaitForEnd2
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 60)   . 5 min are you kidding me
                    Pack       MailSubjct,"LCR's - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    append    "<br>",Mailbody
                    append    str55,MailBody
                    append    "<br>",Mailbody
                    append    "rename appears to have failed",Mailbody
                    append    "<br>",Mailbody
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    move      c1,MailType         .The e-mail body is a text message
.............testing  
.                    move         "davidherrick@nincal.com",mailto
.                    clear        mailbcc
.............testing  
                    Move      "360",MailTimer                 
                    call      SendMail
                    return
                    Pause     "30"
                    Copyfile  str35,"c:\work\faxfile.pdf"                    
                    endif
                    rename    str35,str55                        .TRY AGAIN
          
                    goto      checkfile2
.end DH goes bad checking for failed rename
.begin patch 2.8
SMPRecClear
               Clear        SmpMailbody  
               Return
.end patch 2.8
.begin patch 2.91
SAMPLE
                      move      C0,N2
                      move      OSCODE,N2
                      move      "                          ",SAMPLE2
                      call      Trim using OSCODE
                      if (OSCODE <> "" & OSCODE <> "0")
                      branch    N2,SAM1,SAM2,SAM3
                      clear     SAMPLE2
                      endif
SAM1
                      move      "Sample enclosed",SAMPLE2
                      match     Z3,OSAMCDE
                      return     if equal
                      clear     NSMPFLD
                      move      MCOMP,MlrNameHold
                      move      MNAME,MlrMNameHold
.
                      move      "SAM1-COMPKEY3",Location
                      pack      COMPFLD3,OMLRNUM
                      pack      KeyLocation,"Key: ",COMPFLD3
                      call      COMPKEY3
                      pack      NSMPFLD,COMPNUM,OSAMCDE
.Need to refresh MCOMP
                      move      MlrNameHold,MCOMP
                      move      MlrMNameHold,MNAME
                      rep       zfill,NSMPFLD
                      move      "SAM1-NSMPKEY",Location
                      pack      KeyLocation,"Key: ",NSMPFLD
                      call      NSMPKEY
                      Goto       Samout
SAM2
                      move      "Sample to follow",SAMPLE2
                      Goto       Samout
SAM3
                      move      "Sample previously cleared",SAMPLE2
SamOut                if        (Oscode = "1")
                      add       eightlpi,row
                      prtpage   prfile;*Pcolumn1:ROw,"Sample: ",*Pcolumn3:ROw,SAMPLE2,B1,Nsmpdes1
                      Elseif        (Oscode = "2")
                      add       eightlpi,row
                      prtpage   prfile;*Pcolumn1:ROw,"Sample: ",*Pcolumn3:ROw,SAMPLE2
                      Elseif        (Oscode = "3")
                      add       eightlpi,row
                      prtpage   prfile;*Pcolumn1:ROw,"Sample: ",*Pcolumn3:ROw,SAMPLE2
                      endif
           return
.end patch 2.91


          INCLUDE   NORDIO.INC
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
          include   nmdlio.inc
          INCLUDE   NOWNIO.INC
          include   nuseio.inc
          include   nspeio.inc
          INCLUDE   NOFRIO.INC
          include   nord5io.inc
          include   hpio.inc
          INCLUDE   NCNTIO.INC
          INCLUDE   NSEL2IO.INC
.begin patch 2.8
         INCLUDE   nsmpio.inc
.end patch 2.8
          INCLUDE   COMLOGIC.INC

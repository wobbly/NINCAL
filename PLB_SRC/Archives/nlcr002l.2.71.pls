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

Release   Init      "2.71"    ASH       ADDED NEW LCR STATUS
RelDate   Init      "2013 June 01"
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
.begin patch 2.6
HoldOcocode         Dim       2         .used for Contact Break
.end patch 2.6
holdown   dim       4         .used for owner break
faxflag   form      1         .1=no, 2=yes.
LPTCNT    FORM      4         .LENGTH OF ATTCHLST
LONGDIST DIM        1
DCX       INIT      ".TIF"
.begin patch 2.43
.intrnet  dim       46        .print contact's internet address
intrnet   dim       50        .print contact's internet address
.End patch 2.43
DCX2      dim       30
DCXFile   dim       120
SPOOLF    dim       120
FilePath DIM        45
          PACK      FILEPATH,NTWKPATH1,"SAMPLES\"                .
." just here to fix display of vars :)  
SMPArray dim        12(50)
SMPIndex form       2
.begin patch 2.5
EmailFlag DIm       1
FileCheck FIle
trapcount form      4
EmailAddr Dim       50
dmFileName          dim 80
.end patch 2.5

.substituting report at faxform generates and Object error at load time under 8.6
rptcan    dim       1
ProgFlag form       "0"
DimPtr    dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
.begin patch 2.53
.column8   form      9
.column9   form      9
.column10 form       9
.column11 form       9
.end patch 2.53
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
.Begin Patch 2.43
FontO7              font
FontO18B  font
.end Patch 2.43
NINLogo   PICT
PackData Datalist
PackData2 Datalist
.hexeight integer 4,"4294967295"

x         plform    report
        formload X
...........................................................
.Create work var
          create    PackData=1:1:1:1
          create    PackData2=1:1:1:1
.

          move      "NLCR002L",PROGRAM
          move      "Names in the News Ca",COMPNME
          move      "LCR PRINT/Fax ",STITLE
          move      "EXIT",PF5
          clock     DATE,TODAY
          call      PAINT
          call      FUNCDISP
          trap      NOFILE IF IO
          trap      EXIT IF F5
          Clear     Holdown
OPEN1
.begin patch 2.5
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
          display   *P1:24,*el,"OPENING FILES";
          move      "NIN PRINT FILE       ",FMESG
          pack      STR35,NTWKPATH1,"lcrprint.lcr"
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
.START PATCH 2.71 ADDED LOGIC - THIS IS JUST A SAFETY MEASURE...
                    if (OHIST <> "t")
.END PATCH 2.71 ADDED LOGIC
                                  call      Trim using OLRN
                                  call      Trim using OLON
                                  if (OLRN <> "" & OLON <> "")
.begin patch 2.6
.                              if (OLON <> holdown)
                                        if (OLON <> holdown | OCOCODE <> HoldOCOCODE)
                                                  if (holdown <> "")
.                                                  pack      taskname,holdown,owncnt,ownscnt
                                                            pack      taskname,HoldOCOCODE,holdown,owncnt,ownscnt
                                                            insertitem PackData2,999999,taskname
                                                            move      C0,owncnt
                                                            move      C0,ownscnt
                                                            deleteitem PackData,0
                                                  endif
                                        move      OLON,holdown
                                        Move      OCOCODE,HoldOCOCODE
.end patch 2.6
                              
                                              endif
                                              add       C1,owncnt
.START PATCH 2.2 REPLACED LOGIC
.                                            pack      str12,OMLRNUM,OSAMCDE
                                              move      "COMPKEY3",Location
                                              pack      COMPFLD3,OMLRNUM
                                              pack      KeyLocation,"Key: ",COMPFLD3
                                              call      COMPKEY3
                                              pack      str12,COMPNUM,OSAMCDE
.END PATCH 2.2 REPLACED LOGIC
                                        PackData.FindString giving result using str12,SEQ
.begin patch 2.52
.                              if (result = SEQ)
                                        if (result = SEQ & OWNBLK <> "1")             .ownblk = 1 means owner does not allow samples
.end patch 2.52
                                                  insertitem PackData,999999,str12
                                                  add       C1,ownscnt
                                        endif
                              endif
.START PATCH 2.71 ADDED LOGIC
                    endif
.END PATCH 2.71 ADDED LOGIC
          repeat
.begin patch 2.6
.          pack      taskname,holdown,owncnt,ownscnt
          pack      taskname,HoldOCOCODE,holdown,owncnt,ownscnt
          insertitem PackData2,999999,taskname
.end patch 2.6
.TESTING LOGIC - KEEP HERE IN CASE WE WANT TO ADD SOMETHING ELSE TO IT.
.         prep      tempfile,"c:\work\tester.dat"
.         PackData2.GetCount giving howmany
.         if (howmany > C0)
.                   for result,"1",howmany
.                             getitem   PackData2,result,taskname
.                             write     tempfile,SEQ;taskname
.                   repeat
.         endif
.         close     tempfile
.         shutdown
.
READ1
.Reopen job and actually create
          close     ordprint
          open      ordprint,STR35
.Initialize a bunch of flags
          deleteitem PackData,0
          pack      holdown,B55
          move      C0,count
          move      C0,owncnt
          move      C0,ownscnt
          move      C0,FaxFlag
          loop
                    read      ORDPRINT,SEQ;ORDVARS
                    until over
                    if        (olrn = "775315")
                    call      debug
                    endif
                    display   *p10:12,count,b1,OLRN,b1,OLON;
                    call      Trim using OLRN
                    call      Trim using OLON
                    if (OLRN <> "" & OLON <> "")
.begin patch 2.6
.                              if (OLON <> holdown)
                              if (OLON <> holdown | OCOCODE <> HoldOCOCODE)
.end patch 2.6
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
.begin patch 2.6
.                              prtpage   prfile;*NEWPAGE;
                              if (count > C0)
                              prtpage   prfile;*NEWPAGE;
                              endif
.end patch 2.6
                              call      Print
.                             add       c1,ownpcnt
                    endif
                    add       C1,count
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
.begin patch 2.54
..begin patch 2.43
.                                        IF        (OCompID2 = "P")
.                                        pack      intrnet,str55,"@pacificlists.com"
.                                        ElseIF    (OCompID = "P" & OCompid2 <> "N")
.                                        pack      intrnet,str55,"@pacificlists.com"
.                                        Else
.                                        pack      intrnet,str55,"@nincal.com"
.                                        endif
..                                       pack      intrnet,str55,"@NINCAL.COM"
..End patch 2.43
.end patch 2.54
                                        pack      intrnet,str55,"@nincal.com"
                              endif
                    endif
          endif
          if (ProgFlag = 1)   .Single LCR Report called from other Program
                    move      OLON,NOWNFLD
                    rep       zfill,NOWNFLD
                    call      NOWNKEY
.START PATCH 2.1 ADDED LOGIC
                    pack      MKEY,OMLRNUM,z3
                    move      "SVCREP2-NMLRKEY",Location
                    call      NMLRKEY
.END PATCH 2.1 ADDED LOGIC
                    call      DISNIN
                    return
          endif
          call      Process1
          return

OwnerBreak
          display   *p1:24,*el,"Break"
          call      CreateSamples
          prtclose prfile
JDBREAK
.Combine files here
          if (faxflag = C2)
.begin patch 2.5     .total rewrite we will email to recipient regardless if email or fax
          Clear     Taskname
          Clear     Str55
          Clear     Str35
          pack      str35 from "c:\work\pdf\faxfile.pdf"
          rep       Zfill,Holdown
          move      str35,str55
.          pack      str55 from "c:\work\pdf\Lcr",Holdown,".pdf"
.          pause     "20"
.          rename   "C:\WORK\Faxfile.prn",taskname
.          endif
.          copyfile  taskname,str55
.            erase     taskname
.          pause      "10"
.          rename    taskname,str55
.Begin patch 2.51 DH goes bad checking for failed rename
          MOVE      C0,TrapCount
.end patch 2.51 DH goes bad checking for failed rename

          
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.Begin patch 2.51 DH goes bad checking for failed rename
          Pause     "15"
.end patch 2.51 DH goes bad checking for failed rename
.begin patch 2.6
.          pack      str55 from "c:\work\pdf\Lcr",Holdown,".pdf"
          pack      str55 from "c:\work\pdf\Lcr",HoldOcocode,Holdown,".pdf"
.end patch 2.6
          rename    str35,str55
.Begin patch 2.51 DH goes bad checking for failed rename
          MOVE      C0,TrapCount
Checkfile2
          trap      WaitForEnd2 giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.end patch 2.51 DH goes bad checking for failed rename


.          Pack       MailSubjct,"List Clearance Requests - ",str35,b1,str55
          Pack       MailSubjct,"List Clearance Requests  "
          pack      MailAttach from str55

        
          move      emailaddr to mailto
.begin patch 2.6
          Move      intrnet,Mailfrom          
.          move      "creques@nincal.com" to mailfrom
.end patch 2.6

          IF        (FrmPTr = "2")            .FRom program 1 send to requestor
          CLEAR     MailTO
          append    User,Mailto
          append    "@nincal.com",Mailto
          reset     Mailto
          Else                                    .live run
          MOve      "ComputerRequest@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
          endif
..First check 995 autolaunch settings
.begin patch 2.7
.        call       GetPDFPath
.        pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.        call      "GU$INI;WRITE_TO_INI" USING str45:
.                  "Parameters":
.                  "ProcessPDF":
.                  "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\Flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                    Call      PDF995Auto
.end patch 2.7
                    Move      "30",MailTimer
                    pause     "5"
                    call      SendMail
Daver               pause     "30"
.         after sending copy to folder
          pack      taskname from "\\nins1\d\data\Lcrs\sent\Lcr",HoldOcocode,Holdown,".pdf"
          copyfile  str55,taskname
          erase     str55
          return
          Else
          
.                    clear   taskname
.                    Path      Exist,"c:\windows"
.                    if over                       .nt/2000
.                              append    "!c:\winnt\system32\cmd.exe /c ",taskname
.                    elseif (osflag = c6)          .XP
.                              append    "!c:\windows\system32\cmd.exe /c ",taskname
.                    else                          .95/98
.                              append    "!c:\command.com /c ",taskname
.                    endif
..START PATCH 2.3 REPLACED LOGIC
..                   append  "copy \\nins1\d\data\fax\hdrfile.prn /b + \\nins1\d\data\fax\faxfile.prn /b \\nins1\d\data\fax\lcr",taskname
.                    append  "copy C:\WORK\hdrfile.prn /b + C:\WORK\faxfile.prn /b \\nins1\d\data\fax\lcr",taskname
..END PATCH 2.3 REPLACED LOGIC
.                    append  HOLDOWN,taskname
.                    append  ".prn /b",taskname
.                    reset   taskname
.                    execute taskname
..
.                    clear   taskname
.                    Path      Exist,"c:\windows"
.                    if over                       .nt/2000
.                              append    "!c:\winnt\system32\cmd.exe /c ",taskname
.                    elseif (osflag = c6)          .XP
.                              append    "!c:\windows\system32\cmd.exe /c ",taskname
.                    else                          .95/98
.                              append    "!c:\command.com /c ",taskname
.                    endif
..                   append  "copy \\nins1\d\data\fax\lcr",taskname
.                    append  "copy /b \\nins1\d\data\fax\lcr",taskname
.                    append  HOLDOWN,taskname
.                    append  ".prn \\nts3\fax",taskname
.                    reset   taskname
.                    execute taskname
.end patch 2.5
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
.begin patch 2.5 -- lets check for email first
          Clear      Emailaddr
          call      Trim using OwnEmail
                    if        (Ownemail <> "")               .we have somthing
                              scan      "@",Ownemail
                              if        equal                         .presuming valid
                                       reset     Ownemail
.                                           move      "jduenas@nincal.com",ownemail
                                       
                                       move      Ownemail,Emailaddr
                                       move      c2,Faxflag                    .send it
.19 August 2010
                                       move       Yes,EmailFlag              .used for cover sheet
.19 August 2010
                              endif
                                        call      OpenPrintFile using C4
                                        move      c2,faxflag        .do fax.
                                        display   *p1:24,*el,"send\fax it"

                    Goto      ContOwnPRep
                    
                    Endif
.end patch 2.5
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
.Begin patch 2.5
                              pack      EmailAddr from"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
.START TESTING!!!!
.                              pack      mailbcc   from "jduenas@nincal.com"
.End TESTING!!!!
.end patch 2.5
.19 August 2010
                                       move       No,EmailFlag              .used for cover sheet
.19 August 2010
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
.START TESTING!!!!
.                             call      OpenPrintFile using C0
.END TESTING!!!!
                                        move      c2,faxflag        .do fax.
.begin patch 2.5
.                                        display   *p1:24,*el,"fax it"
                                        display   *p1:24,*el,"send\fax it"
.end patch 2.5
                              endif
          else
.printing not faxing - no or invalid fax number
                    call      OpenPrintFile using C5
                    display   *p1:24,*el,"invalid owner number"
                    move      c1,faxflag        .
          endif
.begin patch 2.5
ContOwnPrep
.end patch 2.5
          move      c0,owncnt           .reset count for cover sheet.
          move      c0,ownscnt                    .reset count for cover sheet.
.         move      c0,ownpcnt
          move      OLON,holdown
.begin patch 2.6
          Move      Ococode,HoldOcocode
.end patch 2.6
          if (ProgFlag = C0)  .Batch run only
                    deleteitem PackData,0
          endif
          call      PrintCover
          return

PrintCover
          move      C0,owncnta
          move      C0,ownscnta
          if (ProgFlag = C0)  .Batch run only
                    PackData2.GetCount giving howmany
                    if (howmany > C0)
                              for result,"1",howmany
                                        getitem   PackData2,result,taskname
.begin patch 2.6
.                                        unpack    taskname,str4
                                        unpack    taskname,str2,str4
                                        if (str4 = OLON & str2 = ococode)
.end patch 2.6
.begin patch 2.6
.                                                  unpack    taskname,str4,owncnta,ownscnta
                                                  unpack    taskname,str2,str4,owncnta,ownscnta
.end patch 2.6
                                                  break
                                        endif
                              repeat
                    endif
          else
                    move      C1,owncnta
                    move      C0,ownscnta
          endif
.begin patch 2.5  --- no longer printed will be coverpage if fax/email
          Clear     Mailbody
          if        (faxflag = 2)               email or fax
                    IF        (OCompid2 = "P")
                              Append    "Names in the News                      Pacific Lists, Inc.",Mailbody
                              append    CRLF,Mailbody
                              append    "180 Grand Avenue, Suite 1545           180 Grand Avenue, Suite 1545",Mailbody
                              append    CRLF,Mailbody
                              append    "Oakland, CA 94612-3799                           Oakland, CA 94612-3799",Mailbody
                              append    CRLF,Mailbody
                              append    "415-989-3350 * Fax 415-443-7796        415-945-9450 * Fax 415-945-9451",Mailbody
                              append    CRLF,Mailbody
          
                    Else
                              Append    "Names in the News",Mailbody
                              append    CRLF,Mailbody
                              append    "180 Grand Avenue, Suite 1545",Mailbody
                              append    CRLF,Mailbody
                              append    "Oakland, CA 94612-3799",Mailbody
                              append    CRLF,Mailbody
                              append    "415-989-3350 * Fax 415-443-7796",Mailbody
                              append    CRLF,Mailbody
                    endif
          Append    "List Clearances",Mailbody
          append    CRLF,Mailbody
          append    CRLF,Mailbody
                    if        (EmailFlag = Yes)        
                    Append    "   VIA Email ",Mailbody
                    Else
                    Append    "   VIA FACSIMILE ",Mailbody
                    endif     
          append    CRLF,Mailbody
          Append    "Date ",mailbody
          append    today,mailbody
          append    CRLF,Mailbody
          append    "To: ",mailbody
          append    faxname,mailbody
          append    CRLF,Mailbody
          if (faxattn <> "")
                    append    "Attn: ",mailbody
                    append    faxattn,mailbody
                    append    CRLF,Mailbody
          endif
.begin patch 2.6
.          Append    "From: Requests",Mailbody
.end patch 2.6
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          Append    owncnta,Mailbody
          append    " Request(s) Enclosed",mailbody
          append    CRLF,Mailbody
          
                    if (ownscnta > C0)
                    append    CRLF,Mailbody
                    append    ownscnta,Mailbody
                    append    " Sample(s) Enclosed",Mailbody
                    append    CRLF,Mailbody
                    endif

                    if (owncnta <> 1 & ownscnta > C0)
                    append    CRLF,Mailbody
                    append    CRLF,Mailbody
                    Append    "Note: Sample(s) may need to be attached to multiple requests.",Mailbody
                    append    CRLF,Mailbody                  
                    endif
          append    "Please note the fax number (510-302-4632) for all clearance responses.",mailbody
          append    CRLF,Mailbody                  
.                    IF        (OCompID <> "P")
                    Append    "We will continue to use 415-433-7796 as the general fax number.",mailbody         
.                    else
.                    Append    "We will continue to use 415-945-9451 as the general fax number.",mailbody         
                    
.                    endif
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          Append    "Please call if you do not receive all pages.",mailbody         
          append    CRLF,Mailbody
.begin patch 2.53
.begin patch 2.6
.          Append    "Please do not reply to this email. Rather use the contact information on the Request.",Mailbody
.          append    CRLF,Mailbody
.end patch 2.6
.end patch 2.53
          reset     Mailbody          
          

          Else                                    .we are printing
.end patch 2.5          
.begin patch 2.54
..begin patch 2.43
.          IF        (OCompID2 = "P")
.          call      debug
.          prtpage   PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=258:343,*font=font1,"1300 Clay St. 11th Floor":
.                    *p=158:443,"Oakland, CA 94612-1429":
.                    *p=93:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=93:643,"A Division of Names in the News"
.          ElseIf    (OCompId = "P" & Ocompid2 <> "N")
.          prtpage   PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=258:343,*font=font1,"1300 Clay St. 11th Floor":
.                    *p=158:443,"Oakland, CA 94612-1429":
.                    *p=93:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=93:643,"A Division of Names in the News"
..                   *p=500:343,*font=font1,"1300 Clay St. 11th Floor":
..                   *p=400:443,"Oakland, CA 94612-1429":
..                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                   *p=335:643,"A Division of Names in the News"
.          Else
.          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
..end patch 2.43
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 2.54
          move      "1500",row
          prtpage   prfile;*pcolumn7:row,*font=font3,"LIST CLEARANCES"
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
.begin patch 2.5
.          prtpage   prfile;*pcolumn7:row,*font=font4,"   VIA FACSIMILE"
          if        (EmailFlag = Yes)        
          prtpage   prfile;*pcolumn7:row,*font=font4,"   VIA Email"
          else
          prtpage   prfile;*pcolumn7:row,*font=font4,"   VIA FACSIMILE"
          endif     
.end patch 2.5
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
.         add       eightlpi,row
.         add       eightlpi,row
.         add       eightlpi,row
.         prtpage   prfile;*pcolumn1:row,ownpcnt," Total Pages (including cover)"
          if (owncnta <> 1 & ownscnta > C0)
                    add       eightlpi,row
                    add       eightlpi,row
                    add       eightlpi,row
                    prtpage   prfile;*pcolumn1:row,"Note: Sample(s) may need to be attached to multiple requests."
          endif
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
.START PATCH 2.41 ADDED LOGIC
.begin patch 2.54
..begin patch 2.43
.          IF        (OCompID <> "P")
.          prtpage   prfile;*pcolumn1:row,"Please note the NEW fax number (510-302-4690) for all clearance responses." 
.          endif
..end patch 2.43
          prtpage   prfile;*pcolumn1:row,"Please note the fax number (510-302-4632) for all clearance responses." 
.end patch 2.54
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
.begin patch 2.54
..begin patch 2.43
.          IF        (OCompID <> "P")
.          prtpage   prfile;*pcolumn1:row,"We will continue to use 415-433-7796 as the general fax number."
.          endif
..end patch 2.43
          prtpage   prfile;*pcolumn1:row,"We will continue to use 415-433-7796 as the general fax number."
.end patch 2.54
.End PATCH 2.41 ADDED LOGIC
          add       eightlpi,row
          add       eightlpi,row
          add       eightlpi,row
          prtpage   prfile;*pcolumn1:row,"Please call if you do not receive all pages."
.begin patch 2.5
          endif
.end patch 2.5
          return

..........................................................................................................................
CreateSamples
          compare   c2,faxflag
          if equal          .yes
                    PackData.GetCount giving howmany
                    if (howmany > C0)
.                             add       howmany,ownpcnt
                              for result,"1",howmany
                                        getitem   PackData,result,DCX2
                                        pack      DCXFile,filePath,"s",DCX2,DCX
.Print and Display First Page
                                        clear     N9
                                        CREATE    PICT1=70:495:70:620:
                                                  DCXFile,BORDER=0,AUTOZOOM=0
                                        PICT1.GetPageCount GIVING N9
                                        prtpage   prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:10500:1:8000:PICT1;
.Print and Display Additional Pages
                                        if (N9 > C1)    .Only Enter loop if more than one page
                                                  clear     N8
                                                  move      C1,N8   .Start with SECOND PAGE as first page already printed
                                                  loop
                                                            add       C1,N8
                                                            until (N8 > N9)
                                                            CREATE    PICT1=70:495:70:620:
                                                                      DCXFile,BORDER=0,AUTOZOOM=0,PAGE=N8
                                                            activate PICT1
                                                            prtpage   prfile;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:10500:1:8000:PICT1;
                                                  repeat
                                        endif
                              repeat
                    endif
          endif
          return

Process1
          pack      MKEY,OMLRNUM,z3
          move      "process1-NMLRKEY",Location
          call      NMLRKEY
.
          type      OSAMCDE
          if equal
                    match     "000",OSAMCDE
                    if not equal
.START PATCH 2.2 REPLACED LOGIC
.                             pack      str12,OMLRNUM,OSAMCDE
                              move      "COMPKEY3-2",Location
                              pack      COMPFLD3,OMLRNUM
                              pack      KeyLocation,"Key: ",COMPFLD3
                              call      COMPKEY3
                              pack      str12,COMPNUM,OSAMCDE
.END PATCH 2.2 REPLACED LOGIC
                              PackData.FindString giving result using str12,SEQ
                              if (result = SEQ)
                                        insertitem PackData,999999,str12
                              endif
                    endif
          endif
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
.begin patch 2.54
..begin patch 2.43
.          IF        (OCompID2 = "P")
.          call      debug
.          prtpage   PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=258:343,*font=font1,"1300 Clay St. 11th Floor":
.                    *p=158:443,"Oakland, CA 94612-1429":
.                    *p=93:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=93:643,"A Division of Names in the News"
..                   *p=500:343,*font=font1,"1300 Clay St. 11th Floor":
..                   *p=400:443,"Oakland, CA 94612-1429":
..                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                   *p=335:643,"A Division of Names in the News"
.
..         prtpage   PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
..                   *p=500:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
..                   *p=400:443,"Corte Madera, CA 94925-1182":
..                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                   *p=335:643,"A Division of Names in the News"
.          ElseIf    (Ocompid = "P" & Ocompid2 <> "N")
.          prtpage   PrFile;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=258:343,*font=font1,"1300 Clay St. 11th Floor":
.                    *p=158:443,"Oakland, CA 94612-1429":
.                    *p=93:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=93:643,"A Division of Names in the News"
..                   *p=500:343,*font=font1,"1300 Clay St. 11th Floor":
..                   *p=400:443,"Oakland, CA 94612-1429":
..                   *p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
..                   *p=335:643,"A Division of Names in the News"
.
.          Else
.          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
..         prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
..end patch 2.43
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 2.54
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
.         prtpage   prfile;*pcolumn4:row,"Please fill in below and fax to (415)433-7796"
.START PATCH 2.41 ADDED LOGIC
.Begin patch 2.54
..Begin patch 2.43
.          IF        (OCompID = "P")
.          prtpage   prfile;*pcolumn4:row,"Please fill in below and fax to (415)945-9451"
.          Else
.          prtpage   prfile;*pcolumn4:row,"Please fill in below and fax to (510)302-4690"
.          endif
..end patch 2.43
          prtpage   prfile;*pcolumn4:row,"Please fill in below and fax to (510)302-4632"
.end patch 2.54
.End PATCH 2.41 ADDED LOGIC
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
.         GOTO NOSMP IF EQUAL
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
.begin patch 2.6
.                                        prtpage   prfile;*NEWPAGE;
.begin patch 2.6
                              else
.START PATCH 2.4 ADDED LOGIC
.begin patch 2.7
.                                        if (FrmPtr = 2)
.                                                  call      "GU$INI;GET_FROM_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
.                                                            "Parameters":
.                                                            "ProcessPDF":
.                                                            str45
.                                                  move      "ProcessPDF",str25
.                                                  move      "\\nins1\e\apps\plb\code\pdftest.bat",str35
..
.                                                call       GetPDFPath
.                                                  pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                                                 call      "GU$INI;WRITE_TO_INI" USING str45:
.                                                           "Parameters":
.                                                           "ProcessPDF":
.                                                           "\\nins1\e\apps\winbatch\Del995flag.exe":
.                                                            result
.                                        if (result = C0)
..Prepare Flag file
.                                                  pack      str45 from PDFPATH,"\flag.dat"
.                                                            prep      tempfile,str45
.                                                            write     tempfile,SEQ;"flag set"
.                                                            close     tempfile
.                                                  endif
.                                        endif
.end patch 2.7

.END PATCH 2.4 ADDED LOGIC
                                        call      OpenPrintFile using FrmPtr
                              endif
                              call      Print
                              prtclose prfile
                              if (FrmPtr = 2 | FrmPtr = 4)            .2=PDF, 4=Fax
.Clean up pre-existing files!
                                        call      CleanUpLCRFaxFiles
.
.START PATCH 2.4 REPLACED LOGIC
.                                       pack      APIFileName,"c:\work\faxfile.prn",HexZero
.                                       call      FindFirstFile
.                                       if (APIResult = 0 | APIResult = hexeight)
..File not created!
.                                                 if (FrmPtr = 2)
.                                                           pack      taskname,"File Creation Failed!  .PDF File not created!"
.                                                 elseif (FrmPtr = 4)
.                                                           pack      taskname,"File Creation Failed!  Fax will not be sent!"
.                                                 endif
.                                                 alert     caution,taskname,result
.                                                 return
.                                       endif
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
.END PATCH 2.4 REPLACED LOGIC
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
.START PATCH 2.4 REPLACED LOGIC
.                                                 clear     taskname
.                                                 append    "\\nins1\c\apps\pcl2pdf\pcl2pdf32 ",taskname
.                                                 append    "c:\work\faxfile.prn ",taskname
.                                                 append    "c:\work\lcrfax",taskname
.                                                 append    OLRN,taskname
..pcl2pdf32 Options:  /S - Silent Mode(no display), /M:# - Maximum number of pages
..I set the second option as the converter is inherently adding a second page that I want to suppress
.                                                 append    ".PDF /S /M:1",taskname
.                                                 reset     taskname
.                                                 execute   taskname
..Mail it back to them
.                                                 pack      str25,"lcrfax",OLRN,".PDF"
.                                                 pack      taskname,"c:\work\",str25
........................................................
.Mail it back to them
                                                  pack      str35,"lcrfax",OLRN,".PDF"
.here
                                                  pack      taskname,"c:\work\",str35
."
.Give the email a chance of rendering itself before updating the INI file.
.begin patch 2.7
.                                                  call      GetPDfPAth
.                                                  pack      str45 from PDFPATH,"\flag.dat"
.                                                  pack      APIFileName,STR45,hexzero
.                                                  loop
.                                                            call      FindFirstFile
.                                                            until (APIResult = 0 | APIResult = hexeight)
.                                                            pause     "1"
.                                                  repeat
.end patch 2.7

                                                  pause     "5"
..Prepare Flag file once again
.                                                 prep      tempfile,"c:\progra~1\pdf995\flag.dat"
.                                                 write     tempfile,SEQ;"flag set"
.                                                 close     tempfile
.
                                                  pack      str25,"c:\work\pdf\lcrfax.pdf"
                                                  copyfile str25,taskname
                                                  erase     str25
.END PATCH 2.4 REPLACED LOGIC
.begin patch 2.54
..begin patch 2.43
.
.                                        IF        (OCompID2 = "P")
.                                        pack      str45,str7,"@pacificlists.com"
.                                        ElseIF    (OCompID = "P" & OCOmpid2 <> "N")
.                                        pack      str45,str7,"@pacificlists.com"
.                                        Else
.                                        pack      str45,str7,"@nincal.com"
.                                        endif
..                                                 pack      str45,str7,"@nincal.com"
..End patch 2.43
                                        pack      str45,str7,"@nincal.com"
.end patch 2.54
.
                                                  move      "Here is your PDF File.",MailSubjct
.   Set   the text message that is senT with the attachments
                                                  pack      MailBOdy,taskname

                                                  move      str45,MailFrom
                                                  move      str45,MailTO                                                .User name
                                                  Pack      MailAttach from "c:\work\",str35                  ."
                                                  call      SendMail
                                        elseif (FrmPtr = 4)                     .Fax
.begin patch 2.5
.                                                  clear   taskname
.                                                  Path      Exist,"c:\windows"
.                                                  if over                       .nt/2000
.                                                            append    "!c:\winnt\system32\cmd.exe /c ",taskname
.                                                  elseif (osflag = c6)          .XP
.                                                            append    "!c:\windows\system32\cmd.exe /c ",taskname
.                                                  else                          .95/98
.                                                            append    "!c:\command.com /c ",taskname
.                                                  endif
.                                                  append  "copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\lcrfax",taskname
.                                                  append  OLRN,taskname
.                                                  append  ".prn /b",taskname
.                                                  reset   taskname
.                                                  execute taskname
..
.                                                  clear   taskname
.                                                  Path      Exist,"c:\windows"
.                                                  if over                       .nt/2000
.                                                            append    "!c:\winnt\system32\cmd.exe /c ",taskname
.                                                  elseif (osflag = c6)          .XP
.                                                            append    "!c:\windows\system32\cmd.exe /c ",taskname
.                                                  else                          .95/98
.                                                            append    "!c:\command.com /c ",taskname
.                                                  endif
.                                                  append  "copy c:\work\lcrfax",taskname
.                                                  append  OLRN,taskname
.                                                  append  ".prn \\nts3\fax",taskname
.                                                  reset   taskname
.                                                  execute taskname
.end patch 2.5
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
.Begin Patch 2.43
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.End Patch 2.43

          create    NINLogo=3:13:30:50:
.                   "\\nins1\c\netutils\NIN logo black outline.jpg"
.START PATCH 2.42
.                   "\\nins1\c\netutils\NIN logo black outline list clearance.jpg"
                    "\\nins1\e\netutils\NIN LCR logo black outline.jpg"
.End PATCH 2.42
          return

OpenPrintFile Routine FrmPtr
.FrmPtr  = Printer
          call      GetWinVer
          Trap      SpoolErr if Spool
.          if (FrmPtr = C0 | FrmPtr = 3)     .Laser2 = Default
.                    if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
.                              PRTOPEN prfile,"\\NINS2\laser2","FAXFILE.PRN"
.                    elseif (osflag = c3 | OSflag =c4)         .win 95 98
.                              PRTOPEN prfile,"Laser2","FAXFILE.PRN"
.                    else   .(osflag = c0)         .Don't know prompt for printer
          if (FrmPtr = C0 | FrmPtr = 3)     .Laser3 = Default
                    if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                              PRTOPEN prfile,"\\NINS2\Laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
          elseif (FrmPtr = 1)  .Laser3
                    if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                              PRTOPEN prfile,"\\NINS2\laser3 Blankstock","FAXFILE.PRN"
                    elseif (osflag = c3 | osflag =c4)         .win 95 98
                              PRTOPEN prfile,"Laser3 Blankstock","FAXFILE.PRN"
                    else   .(osflag = c0)         .Don't know prompt for printer
                              PRTOPEN prfile,"-","FAXFILE.PRN"
                    endif
          elseif (FrmPtr = 2)  .PDF
.START PATCH 2.4 REPLACED LOGIC
..This will only be called via Program 1.  Reference to Faxfile.prn will be to a different location.
..Users access c:\work\faxfile.prn.  Network accesses \\nins1\d\data\fax\faxfile.prn
.                   PRTOPEN   prfile,"FAXFILE","FAXFILE.PRN"
.......................................
.This will only be called via Program 1.
.begin patch 2.7
                    pack      str25,"lcrfax"
.                    PRTOPEN prfile,"PDF995",str25
                    pack      str55,"c:\work\pdf\lcrfax.pdf"
                    PRTOPEN prfile,"PDF:",str55
.end patch 2.7
.END PATCH 2.4 REPLACED LOGIC
          elseif (FrmPtr = 4)
.Create spool file to concatenate with prtfile and send to fax machine
.PRTOPEN will not allow embedded formatting codes and so it has to be done this way :(
.Following line MUST appear in PLBWIN.INI:  PLBVOL_P=F:\DATA\FAX  !!!!
.This will give the path to find HDRFILE.PRN.  All files associated with faxes
.will now appear in this new subdirectory.  (ASH)
                    move      "                                        ",APIFileName
                    clear     APIFileName
.START PATCH 2.3 REPLACED LOGIC
.                   pack      APIFileName,NTWKPATH4,"fax\hdrfile.prn",hexzero
.begin patch 2.5
.                    pack      APIFileName,"C:\WORK\hdrfile.prn",hexzero
..END PATCH 2.3 REPLACED LOGIC
.                    call      DeleteFile
.                    if (APIResult = 0 | APIResult = hexeight)
.                    endif
.                    if (ProgFlag = 1)
.                              SPLOPEN   "c:\work\HDRFILE.PRN"
.                    else
..START PATCH 2.3 REPLACED LOGIC
..                             SPLOPEN   "\\nins1\d\DATA\FAX\HDRFILE.PRN"
.                              SPLOPEN   "C:\WORK\HDRFILE.PRN"
..END PATCH 2.3 REPLACED LOGIC
.                    endif
.                    print     "^[D",longdist,faxtele,"^[N",faxname:
.                              "^[Srequests","^]"
.                    SPLCLOSE
.                    PRTOPEN   prfile,"FAXFILE","FAXFILE.PRN"
.begin patch 2.7
          pack      taskname from "c:\work\pdf\faxfile.pdf"
.          PRTOPEN   prfile,"PDF995",taskname
          PRTOPEN   prfile,"PDF:",taskname
.end patch 2.7
          
.end patch 2.5
          elseif (FrmPtr = 5)  .Laser8
                    if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
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
.Begin patch 2.5
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Orders - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
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
.end patch 2.5
.Begin patch 2.51 DH goes bad checking for failed rename
WaitForEnd2
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 60)   . 3 min are you kidding me
                    Pack       MailSubjct,"LCR's - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "rename appears to have failed",Mailbody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    Pause     "30"
                    Copyfile  str35,"c:\work\faxfile.pdf"                    
                    endif
                    rename    str35,str55                        .TRY AGAIN
          
                    goto      checkfile2
.end patch 2.51 DH goes bad checking for failed rename

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
          INCLUDE   COMLOGIC.INC

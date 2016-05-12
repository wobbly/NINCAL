........................................
. Program:      NLCR0042.PLS
. Function:     LM Outside Broker Report Program Nightly run
. Author:       David Herrick
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
.        Include Smtp.PRI  (PRI - Profile Routine Interface)
        include norddd.inc
        include nord5dd.inc
        include nord4dd.inc
        include nordPdd.inc
          include   compdd.inc
          include   cntdd.inc
        include nxrfdd.inc
        include ndatdd.inc
        include npnddd.inc
        include nspe2dd.inc
          liston
        include ncntdd.inc
        include nusedd.inc
        include winapi.inc
          INCLUDE   NSEL2DD.INC

release  init      "1.12"        DLH    new fax gateway
Reldate   INit      "2015 April 14"
.release  init      "1.11"        DLH    Use date from Dsinit
.Reldate   INit      "2015 March 20"
.release  init      "1.10"        DLH    Sunbelt PDF
.Reldate   INit      "2013 April 25"
.release  init      "1.03"        DLH    use external calls for pdf995.ini
.Reldate   INit      "28 February 12"
.release  init      "1.02"        DLH    replace Pause with subroutine waitin
.Reldate   INit      "07 November 11"
.release  init      "1.01"        DLH    TUrn off PL
.Reldate   INit      "16 February 10"
.release  init      "1.00"        DLH    New (base code from Nlcr0041)auto report both divisions
.Reldate   INit      "13 August 09"

.timestamp1 dim  16
.timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
DimPtr    Dim       ^
.Counters
COUNTR  FORM    9
.Files to open
prfile  pfile
input2    file
input3    file
ORDPRINT  ifile      keylen=6,fixed=696
input2Name          dim    40
First   init    "Y"
HOLDBRK dim     7
HOLDMAIL dim    4
HOLDEXCL dim        1
newdate1 dim    10
page    form    9
date    dim     8
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    11
line1   dim    100
Carr    init    0x7f
faxnum  dim     10
LONGDIST dim    1
PrtFlag dim     1            .default is NO PDF/fax
userinfo dim    500
userlogn dim    7
userlogw dim    7
mss1    plform  Error

dmFileName          dim       80
BrokerAddr          Dim       50        .email 
PDFname    DIm       55        .print file name
FileCheck           FIle
trapcount           form      4
HoldDate   Dim       8
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
......
font6   font
font7     font
        formload mss1

          create  font1,"Arial",size=12,bold
          create  font2,"Arial",size=9
          create  font3,"Helvetica",size=9
          create  font4,"Fixed",size=9
          create  font5,"Arial",size=10
          create  font6,"Arial",size=14
          create    font7,"Times New Roman",size=9

NINLogo   PICT

          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          match   "NLCR0042",PROGRAM   .case sensitive
          if not equal
.send alert and quit :)
         clock   date to date
          move    "NLCR0042",PROGRAM
           else                          .chained from dsinit use date provided
          Move    Today to Date         
          endif
        open      ORDPRINT,"NINPRINT.isi|NINS1:502"
.testing
.          Move      c1,func
.          goto      pRINTFILE
.testing
          
        Move        C2,NordpPath
          Prepare   Input2,"c:\work\brkfile.dat",exclusive
        clear       NordPfld1
        clear       NordPfld2
        clear       NordPfld3
        clear       NordPfld4
        move    "NLCR0042",WPrognme
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        pack        HoldDate,CC,YY,MM,DD
.testing/fix run
.        pack        HoldDate,"20150211"
.testing
.begin patch 1.10
.          Call      GetPDFPath
.end patch 1.10

          if        (Func = "1")     .aquisition team
.create   input file 1 
.first Cleared for contact 48
          pack      NORDPFLD3,"03X48"
          pack      NORDPFLD4,"04XE"
          move      "OK-NORDPAIM",Location
          pack      KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
          call      NORDPAIM
          if over
          goto      next1
          endif
          Loop
          call      NordPkg
          until over
          call      DateCheck
          repeat
Next1
.input file 1 
.first Cleared for contact 48
          pack      NORDPFLD3,"03X48"
          pack      NORDPFLD4,"04Xz"
          move      "OK-NORDPAIM",Location
          pack      KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
          call      NORDPAIM
          if over
          goto      eof1
          endif
          Loop
          call      NordPkg
          until over
          call      DateCheck
          repeat
eof1      weof      INput2,seq
          Close     input2


          Elseif    (Func = "2")          .NIN div
.create   input file 2
          Prepare   Input2,"c:\work\brkfile.dat",exclusive
.first Cleared for contact 87
          pack      NORDPFLD3,"03X87"
          pack      NORDPFLD4,"04XE"
          move      "OK-NORDPAIM",Location
          pack      KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
          call      NORDPAIM
          goto      Next2 if over
          Loop
          call      NordPkg
          until over
          call      DateCheck
          repeat
...............................................................
.input file 2
.first Cleared for contact 87
Next2
          pack      NORDPFLD3,"03X87"
          pack      NORDPFLD4,"04Xz"
          move      "OK-NORDPAIM",Location
          pack      KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
          call      NORDPAIM
          Goto      Eof2 if over

          Loop
          call      NordPkg
          until over
          call      DateCheck
          repeat
eof2      weof      INput2,seq
          Close     input2
          endif

          goto      Process          

DateCheck
          if        (OBRKRPT = "1")           .already sent?
          return                                  .yes
          endif


          clear     str10
          pack      str10,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED
          call      Trim using str10
          if        (str10 >= HoldDate)
          add       C1,HowMany
          write     Input2,seq;ORDVARS

          endif
          return

Process

.Find out system information
                    call                GetWinVer
.Set Flags to Open NINORD.DAT
        move    C0,NORDFLAG
        move    C0,NORDFLG2

.testing
                call    Paint
                DISPLAY *P1:24,"OPENING FILES";
.force filename for print file
.begin patch 1.10
.          PRTOPEN   prfile,"PDF995","BrokerRep"
          PRTOPEN   prfile,"PDF:","c:\work\pdf\BrokerRep.pdf"
.end patch 1.10
          PrtClose  Prfile

.Open Files
.add check for file 
        open    input2,"C:\WORK\BRKFILE.DAT",exclusive
        prepare input3,"C:\WORK\BRKFILE2.DAT",exclusive

                DISPLAY *P1:24,"READING BRKFILE.DAT FILE";

.Prep file before reading it sequentially!!
.FILE MUST EITHER BE REFORMATTED AND REINDEXED PRIOR TO THIS JOB OR DONE RIGHT HERE!!!!!
LCRLoop
        loop
                move    "LCRLoop-readseq,input3",Location
                READ    input2,SEQ;ORDVARS
                until over
                call    OrderReadOtherFiles
                filepi  1;input3
                write   input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,brEmail,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME,HOLDEXCL
        repeat
          Weof      Input3,seq
          close     input2
          close     input3
                DISPLAY *P1:24,"SORTING BRKFILE2 FILE ";
SortFile
        clear   taskname
        move    "C:\WORK\BRKFILE2.DAT,C:\WORK\BRKFILE2.SRT;1-7,8-11,12-17",taskname
        reset   taskname
          move      C0,N1
          for N2,1,10
                    sort    taskname
                    if not over
                              move      C0,N1
                              break
                    else
                              move      C1,N1
                    endif
                    call      Waitin using "10"
.                    pause     "10"
        repeat
          if (N1 = C1)
                    move    s$error$,error
                    move    "Sort did not work!",Location
                    clear     KeyLocation
                    call    IOMssg
                    shutdown
          endif
        clear   taskname
                CALL    PAINT
                DISPLAY *P1:24,"PRINTING FILES        ";
PrintFile
.Set up columns
        move    "500",column
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
        move    "6750",column7
.Initialize HLDBRK
        clear   HOLDBRK
.open newly sorted file
        open    input3,"C:\WORK\BRKFILE2.SRT"
        
        loop
                read    input3,seq;NBRKFLD,OMLRNUM,OLRN,OMLRPON,DESC004,ORENT,OELCODE:
                        BRCOMP,BRCNTCT,BRTELE,BRFAX,brEmail,MCOMP,O1DES,OMDTEM,OMDTED,OMDTEC,OMDTEY:
                        OCLRSTAT,OQTY,OSTAT,CNTNAME,CNTPHONE,OHIST,NSEL2NAME,HOLDEXCL
          Break     if OVer
.                goto    LastRec if over


                if (NBRKFLD <> HOLDBRK | OMLRNUM <> HOLDMAIL)
                        if (FIRST = NO)
                                PRTCLOSE prfile
.begin patch 1.10

.                              pack      APIFileName from PDFPATH,"\flag.dat",hexzero
.                              
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        move      "100",str3
.                                        call      Waitin using str3
.                              repeat
.
.                              pack      APIFileName,PDFPATH,"\flag.dat"
.
.                              erase     APIFILENAME
.
.
.                                Call      PDF995Auto0
.end patch 1.10

                                call    SendPDFFile
                                call    OrderOpenFile
                        else
                                move    NO,FIRST
                                call    OrderOpenFile
                        endif
                        move    NBRKFLD,HOLDBRK
                        move    OMLRNUM,HOLDMAIL
                        clear   page
                        call    OrderPrintHeader
                        call    OrderMailHeader
                endif
                call    OrderPrintRecord
.................
.do updated here
                    move      C1,NORDPPATH
                    packkey   NordPfld,Olrn
                    move      "Nord001DButtonPrint-NORDPKEY",Location
                    pack    KeyLocation,"Key: ",NORDPFLD
                    call      NORDPKEY
                    if over
                              clear     taskname
                              append    "Unable to Print LR#: ",taskname
                              append    NORDPFLD,taskname
                              append    carr,taskname
                              append    "Record no longer present!",taskname
                              reset     taskname
                              alert     caution,taskname,N10
                    else      
                               call      TestBusy using NORDPFLD
                               if (str1 <> STAR)
                                        move      "NORDPUPD",Location
                                        pack    KeyLocation,"Key: ",NORDPFLD
                                        move      "1",OBRKRPT
                                        call      NORDPUPD
                                        if over
                                                  clear     taskname
                                                  append    "Unable to update NINPRINTL LR#: ",taskname
                                                  append    NORDPFLD,taskname
                                                  reset     taskname
                                                  alert     caution,taskname,N10
                                        endif
                                        
                                        move    "NINPRINT",Location
                                        pack    KeyLocation,"Key: ",NORDPFLD
                                        TRAP    IOMssg Giving Error if IO
                                        read      ORDPRINT,NORDPFLD;;
                                        if        not over
                                                  filepi  1;ORDPRINT
                                                  updatab ORDPRINT;*166,OBRKRPT
                                                  if over
                                                            clear     taskname
                                                            append    "Unable to update NINPRINT LR#: ",taskname
                                                            append    NORDPFLD,taskname
                                                            reset     taskname
                                                            alert     caution,taskname,N10
                                                  endif
                                         endif
                                         
                                         TRAPCLR   IO
                                         move      "Nord001DButtonPrint-NORDKEY",Location
                                         move      NORDPFLD,NORDFLD
                                         pack    KeyLocation,"Key: ",NORDFLD
                                         move      C1,NORDPATH
                                         call      NORDKEY
                                         move      "Nord001DButtonPrint-NORDUPD",Location
                                         move      "1",OBRKRPT
                                         call      NORDUPD
                                                  if over
                                                            clear     taskname
                                                            append    "Unable to update NINORD LR#: ",taskname
                                                            append    NORDFLD,taskname
                                                            reset     taskname
                                                            alert     caution,taskname,N10
                                                  endif
                            endif                      
                 endif                                                     
............................cleanup the above

        repeat
..............................................................................
LastRec
        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE

.begin patch 1.10
.                              pack      APIFileName from PDFPATH,"\flag.dat",hexzero
.                              
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
                                        move      "100",str3
                                        call      Waitin using str3
.                              repeat
.
.                              pack      APIFileName,PDFPATH,"\flag.dat"
.
.                              erase     APIFILENAME
.
.        Call      PDF995Auto0
.end patch 1.10

        call    SendPDFFile

.............
          Close     Input2
          Close     Input3
          erase     "C:\WORK\BRKFILE2.DAT"
           if         (func = "1")         
          Copyfile  "c:\work\brkfile2.srt","c:\work\brkfile2.save"   .temp code for testing new version of nlcr0042          
           endif
          Erase     "C:\WORK\BRKFILE2.srt"
.        pack    APIFileName,"C:\WORK\BRKFILE2.DAT",hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.
.        endif
..
.        pack    APIFileName,"C:\WORK\BRKFILE2.SRT",hexzero
.        call    DeleteFile
.        if (APIResult = 0 | APIResult = hexeight)
.
.        endif
.
.*** note  rewritten for Vista or Windows 7
.DOnt erase until they are printed 
.          clear     Mailbody
.          FIndDIr   "C:\work\pdf\*.pdf",MailBody,Itemcount=n5
.          if        (n5 > c0)
.          FOr       n4 from c0 to N5
.          explode   MailBody,"|",Dmfilename 
.          match     "f",Dmfilename 
.
.                    if        equal
.
.                    clear     taskname
.                    bump      DmFileName,c1
.                    pack      taskname from "C:\work\pdf\",DmFIleName  ."comment :)
.                    FindFIle  Taskname
.                              if        Zero          .file is there
.                              erase Taskname                          
.                              endif
.                    endif
.          repeat
.          endif
.log files          
          clear     Mailbody
          FIndDIr   "C:\work\pdf\*.pdf",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "C:\work\pdf\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
        shutdown

SendPDFFile
........................
          if        (prtflag = No)              .good to go
        move    "Broker Report",MailSubjct
.   Set the text message that is send with the attachments
        move    PDFname,MailBOdy
.          if        (func = "1")
.          Pack      MailFrom from "KrsniWatkins@nincal.com"
.          Pack      MailTo from "KrsniWatkins@nincal.com,",BrokerAddr
          Pack      MailFrom from "JenniferMagee@nincal.com"
          Pack      MailTo from "JenniferMagee@nincal.com,",BrokerAddr
.          elseif    (Func = "2")    .NIN div
.          Pack      MailFrom from "AgnesAlvarez@nincal.com"
.          Pack      MailTo from "AgnesAlvarez@nincal.com,",BrokerAddr
          pack      mailbcc from "creques@nincal.com"
.          endif
.begin patch 1.10
.          Pack      MailAttach from "c:\work\pdf\",PDFname              ."
          Pack      MailAttach from PDFname
.end patch 1.10
CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
.          open      FileCheck,"c:\work\pdf\BrokerRep.pdf",Exclusive     
          open      FileCheck,MailAttach,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
.          pause     "5"
          call      waitin using "10"
.          move      "PDF rename",Location
.          erase     str55
.          rename    "c:\work\pdf\BrokerRep.pdf",str55

          call      waitin using "5"
.          pause     "3"
          call      debug
          call      SendMail
          call      waitin using "10"
.          pause     "10"
          erase     mailattach
          else
.leave the file in work to be printed
          endif
        return
OrderOpenFile

.Print newly sorted file
                move    YES,PrtFlag             .PRINT IT
.lets check for email first
                    Clear      Brokeraddr
                    call      Trim using BrEmail
                    if        (bremail <> "")               .we have somthing
                              scan      "@",Bremail
                              if        equal                         .presuming valid
                              reset     bremail
                              move      bremail,BRoKeraddr
                              move      No,PRtflag                    .send it
                              endif
                    Else
                    
                    move    BRFAX,faxnum
                    match   "0000000000",faxnum
                              if      equal                                       .no good zeroed out
                    
                              else
                                        type    faxnum
                                        if not equal
                                        else
                                        count   N2,faxnum
                                        compare C10,N2
                                                  if equal
                                                  move    NO,PrtFlag      .Looks like we have number, FAX IT
                                                  move    C1,LONGDIST
                                                  unpack  faxnum,str3,str7
                                                  match   "510",str3
                                                            if equal
                                                            move    str7,faxnum
                                                            clear   LONGDIST
                                                            else
                                                            match   B3,str3
                                                                      if equal
                                                                            move    str7,faxnum
                                                                            clear   LONGDIST
                                                                      endif
                                                            endif
.                                                  pack      BrokerAddr from"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                                                  pack      BrokerAddr from "+",longdist,faxnum,"@fax.nincal.com"
                                                  endif
                                        endif
                              endif
                    endif
.....DH TEST
                clear   str25
          Pack      str25 from Nbrkfld,"LCREP"
.begin patch 1.10
.          pack    PDFNAME,str25
          pack    PDFNAME from "c:\work\pdf\",str25,".pdf"
.          PRTOPEN prfile,"PDF995","BrokerRep"
.begin patch 1.03
.          call      GetPDFPath
.          Call      PDF995Auto
.          call      SetPDFFlag
.end patch 1.10
.          pack      str45 from PDFPATH,"\res\pdf995.ini"
.
.          call      "GU$INI;WRITE_TO_INI" USING str45:
.                    "Parameters":
.                    "ProcessPDF":
.                    "\\nins1\e\apps\winbatch\Del995Flag.exe":
.                    result
.                   if (result = C0)
..Prepare Flag file
.                   call      getSDrive
.                   pack      taskname from PDFPath,"\flag.dat"
.
.                   prep      tempfile,taskname
.                   write     tempfile,SEQ;"flag set"
.                   close     tempfile
.                   endif
.          Call      PDF995Auto
.end patch 1.03

.begin patch 1.10
.          PRTOPEN prfile,"PDF995",PDFName
          PRTOPEN prfile,"PDF:",PDFName
.          Clear     PDFname
.          pack    PDFNAME,str25,".pdf"
.end patch 1.10

        ADD     C1,COUNTR
                DISPLAY *P10:14,*EL,"PRINT COUNT ",COUNTR
        RETURN

OrderReadOtherFiles
.Open other files to retrieve appropriate information
.Mailer File
        rep     zfill,OMLRNUM
        pack    MKEY,OMLRNUM,"000"      .Master Record
        move    C3,NMLRLOCK
        move    "Driver-NMLRKEY,1rst",Location
        call    NMLRKEY
        if over
                    clear     HOLDEXCL
          else
                    move      COMPEXCL,HOLDEXCL
          endif
.Broker File
        pack    NBRKFLD,OBRKNUM,OBRKCNT
        rep     zfill,NBRKFLD
        move    "Driver-NBRKKEY,1rst",Location
        call    NBRKKEY
        if over
                pack   BRCOMP,"UNKNOWN",B55
          Pack      BREMAIL,b55                
        endif
.Special Instructions
        move    OLRN,NSPE2FLD
        rep     zfill,NSPE2FLD
        move    C3,NSPE2LOCK
        move    "Driver-NSPE2KEY,1rst",Location
        call    NSPE2KEY
.NIN Contact
        move    C1,NCNTPATH
        move    OCOCODE,NCNTFLD
        move    "NCNTKEY",Location
        call    NCNTKEY
        if over
                pack    CNTNAME,B55
                pack    CNTPHONE,B55
        endif
.Data Card Universe
        clear   UNIVERSE
          move      OMLRNUM,COMPFLD3
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          move      COMPNUM,NXRFFLD2
        rep     zfill,NXRFFLD2
        move    C2,NXRFPATH
        move    "Driver-NXRFKEY",Location
        call    NXRFKEY
        if not over
                move    NXRFLIST,NDATFLD
                move    C0,UNIVERSE
                move    C1,NDATPATH
                move    "Driver-NDATKEY",Location
                call    NDATKEY
                if over
                       move     C0,UNIVERSE
                endif
        endif
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
        return

.Print Heading
OrderPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.begin patch 1.01
.                  if (HOLDEXCL = "P")
.                    prtpage   prfile;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
.                              *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
.                              *p=1630:550,"Oakland, CA 94612-1429":
.                              *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
.                              *p=1630:800,"A Division of Names in the News":
.                              *font=font2,*Alignment=*Left
.          else
                    prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
.end patch 1.01
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
        add     "60",row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,newdate1;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
        prtpage prfile;*p1000:row,BRCNTCT;
        prtpage prfile;*p5200:row,"FROM:  ";
                move    "List Management",CNTNAME
.begin patch 1.01
.          if (HOLDEXCL = "P")
.                    move    "(415) 945-9450",CNTPHONE
.          else
                    move    "(415) 989-3350",CNTPHONE
.          endif
.end patch 1.01
        prtpage prfile;*p5700:row,CNTNAME;
        add     eightlpi,row
        prtpage prfile;*p1000:row,BRCOMP;
.begin patch 1.01
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*p5700:row,"Pacific Lists";
.          else
                    prtpage prfile;*p5700:row,"Names In The News";
.          endif
.end patch 1.01
        add     eightlpi,row
        call    Trim using BRTELE
        if (BRTELE <> "")
                unpack  BRTELE,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
        prtpage prfile;*p5700:row,CNTPHONE;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"FAX:";
        call    Trim using BRFAX
        if (BRFAX <> "")
                unpack  BRFAX,str3,str1,str2,str4
                pack    str15,"(",str3,") ",str1,str2,DASH,str4
                prtpage prfile;*p1000:row,str15;
        endif
        prtpage prfile;*p5200:row,"FAX:  ";
.begin patch 1.01
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*p5700:row,"(415) 945-9451";
.          else
          prtpage prfile;*p5700:row,"(510) 628-8313";
.        endif
.end patch 1.01
        add     eightlpi,row
        move    CNTNAME,str1
cntloopy
        bump    CNTNAME,1
        cmatch  B1,CNTNAME
        goto    cntloopy if not equal
        goto    cntexit if eos
        bump    CNTNAME,1
        move    CNTNAME,str7
        call    RemoveChar using str7,B1
        move    str7,str6
        clear   str24
.begin patch 1.01
.          if (HOLDEXCL = "P")
.          pack    str24,str1,str6,"@pacificlists.COM"
.          else
          pack    str24,str1,str6,"@NINCAL.COM"
.        endif
.end patch 1.01
cntexit reset   CNTNAME
.begin patch 1.01
.          if (HOLDEXCL = "P")
.          prtpage prfile;*p5700:row,"www.pacificlists.com";
.        else
          prtpage prfile;*p5700:row,"www.ninlists.com";
.        endif
.end patch 1.01
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p2800:row,*font=font1,*boldon,"LIST CLEARANCE REPORT",*boldoff;
          add     eightlpi,row
.begin patch 1.01
.          if (HOLDEXCL = "P")
.                    add     eightlpi,row
.                    add     eightlpi,row
.          else
                    prtpage prfile;*p5600:row,*font=font2,*boldon,"If approved on Rental please refer";
                    add     eightlpi,row
                    prtpage prfile;*p5600:row,"to our Datacard on www.ninlists.com";
                    add     eightlpi,row
                    prtpage prfile;*p5600:row,"for pricing.",*boldoff;
.          endif
.end patch 1.01
          add     eightlpi,row
        return

OrderMailHeader
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Mailer:  ",*boldoff,MCOMP;
        add     eightlpi,row
        add     eightlpi,row
.begin patch 1.01
.          if (HOLDEXCL = "P")
.                    prtpage prfile;*pcolumn:row,*font=font5,*boldon,"PLI ##/";
.          else
          prtpage prfile;*pcolumn:row,*font=font5,*boldon,"NIN ##/";
.        endif
.end patch 1.01
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"List/";
        prtpage prfile;*pcolumn2:row,"Mail Date";
        prtpage prfile;*pcolumn4:row,"Quantity/";
        prtpage prfile;*pcolumn6:row,"Answer";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Broker ##";
        prtpage prfile;*pcolumn4:row,"Select";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

OrderPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderMailHeader
        endif
.Should never need to list a Cancelled Pending Order, but in case you do logic will now allow it
        if (OSTAT = "p" | OSTAT = "x")
                prtpage prfile;*pcolumn:row,*font=font2,*boldon,OLRN,B1,"Pending Order",*boldoff;
        else
                prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OLRN;
        endif
        move    row,N10
        move    row,result
        add     eightlpi,N10
        add     eightlpi,N10
        prtpage prfile;*pensize=10,*RECT=row:N10:column6:6750;
        move    column6,N9
        add     "70",N9
        move    row,howmany
        add     "70",howmany
.
        if (OHIST = "z")
                prtpage prfile;*pN9:howmany,*font=font1,"DENIED",*font=font2;
        else
                if (OCLRSTAT = "1")
                        prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
                elseif (OCLRSTAT = "2")
                        prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
                elseif (OCLRSTAT = "3")
                        prtpage prfile;*pN9:howmany,*font=font1,"EXC/RENT",*font=font2;
                else
                        if (ORENT = "1")
                                if (OELCODE = "2" OR OELCODE = "3")
                                        prtpage prfile;*pN9:howmany,*font=font1,"RENT/EXC",*font=font2;
                                else
                                        prtpage prfile;*pN9:howmany,*font=font1,"RENTAL",*font=font2;
                                endif
                        else
                                prtpage prfile;*pN9:howmany,*font=font1,"EXCHANGE",*font=font2;
                        endif
                endif
        endif
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,O1DES;
        call    TRIM using OMDTEM
        count   N2,OMDTEM
        if (N2 > 0 AND OMDTEM <> "00" AND OMDTEC <> "11")
                prtpage prfile;*pcolumn2:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY;
        elseif (OMDTEM = "00" AND OMDTED = "00"  AND OMDTEC = "00" AND OMDTEY = "00")
                prtpage prfile;*pcolumn2:row,"As Soon As Possible";
        endif
                move    EditMask,EditQuan
                move    C0,N9
                move    OQTY,N9
                edit    N9,EditQuan
        prtpage prfile;*pcolumn4:row,EditQuan;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OMLRPON;
        prtpage prfile;*pcolumn4:row,NSEL2NAME;
        call    PrintSpecialInstructions
        add     eightlpi,row
        return

PrintSpecialInstructions
        div     C2,eightlpi,N9
        add     N9,row
.
        call    TRIM using DESC004
        if (DESC004 <> "")
                pack    str2,carr,B1
                rep     str2,DESC004
                move    C0,howmany
                move    NO,str1
                loop
                        call    PARSITUP using line1,DESC004,C1
                        call    Trim using line1
                        if (line1 <> carr AND line1 <> "")
                                move    YES,str1
                                add     eightlpi,row
                                prtpage prfile;*pcolumn:row,*font=font4,line1,*font=font2;
                        endif
                        add     C1,howmany
                        until   (howmany >= 7)
                repeat
                if (str1 = YES)
                        add     eightlpi,row
                endif
        endif
        add     N9,row
        return
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    call      Waitin using "5"
.                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Broker Report - ",str25
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    PDFName,MailBody
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
TestBusy LRoutine       DimPtr
.str1 is used as NORDTST uses this variable to determine if Busy Byte is active
          move      C1,NORDPATH
          move      DimPtr,NORDFLD
          move      "O.5TestBusy-NORDTST",Location
          pack      KeyLocation,"Key: ",NORDFLD
          call      NORDTST
          if over
                    clear     taskname
                    append    NORDFLD,taskname
                    append    " is not in Master Order File!!!",taskname
                    reset     taskname
                    alert     caution,taskname,N6
                    move      STAR,str1
          elseif (str1 = STAR)
                    clear     taskname
                    append    NORDFLD,taskname
                    append    " is currently in use.        Try again later!",taskname
                    reset     taskname
                    alert     note,taskname,N6
          endif
          return

.Include IO file
        include nordio.inc
        include nord5io.inc
        include nord4io.inc
        include nordPio.inc
          include   compio.inc
          include   cntio.inc
        include nxrfio.inc
        include ndatio.inc
        include npndio.inc
        include nspe2io.inc
        include ncntio.inc
          INCLUDE   NSEL2IO.INC
        include comlogic.inc

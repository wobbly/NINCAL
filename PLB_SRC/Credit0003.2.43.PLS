PC       EQU       0
.
          INCLUDE             COMMON.inc
          INCLUDE             CONS.inc
          INCLUDE             CompDD.inc
          include   Cntdd.inc
          Include   BrptDD.inc
          INclude   norddd.inc
          Include   NMLDdd.inc
          include   nINVDD.INC
.Begin  patch 2.4
           include    Nadjdd.inc
.end patch 2.4
          include winapi.inc

Release   INit      "2.43"    DLH  .remove Reuben
Reldate   Init      "2015 May 18"
.Release   INit      "2.42"    DLH  .new fax gateway
.Reldate   Init      "2015 April 14"
.Release   INit      "2.41"    DLH  .give acct eamil priority over fax
.Reldate   Init      "2014 May 21"
.Release   INit      "2.4"    DLH  .DUH check for Zero AR
.Reldate   Init      "2014 May 16"

.Release   INit      "2.3"    DLH  add our inv#
.Reldate   Init      "2014 April 16"
.Release   INit      "2.2"    DLH  Internal PDF
.Reldate   Init      "2013 April 22"
.Release   INit      "2.11"    DLH  ADD credit detail to returned to hold emails, use external call for PDF995.ini, use API for flag.dat check = PDF Done
.Reldate   Init      "21 February 2012"
.Release   INit      "2.10"    DLH  ADD cc TO ACCOUNTING FAX OR EMAIL IF EXISTS
.Reldate   Init      "22 juLY 2011"
.Release   INit      "2.09"    DLH  fix counter and final summary email
.Reldate   Init      "29 April 2011"
.Release   INit      "2.08"    DLH  if codes B,M,9,G,P have past due orders include in credit returned to hold emial
.Reldate   Init      "5 January 2011"
.Release   INit      "2.07"    DLH remove Kelly add Jack to Email
.Reldate   Init      "25 May 2010"
.Release   INit      "2.06"    DLH remove Agnes to Email
.Reldate   Init      "10 February 2010"
.Release   INit      "2.05"    DLH add Agnes to Email
.Reldate   Init      "16 October 2009"
.Release   INit      "2.04"    DLH Include LM cold calls
.Reldate   Init      "4 May 2009"
.Release   INit      "2.03"    DLH Replace .dir method with finddir
.Reldate   Init      "24 November 2008"
.Release  INit      "2.02"    DLH sort summary lists
.Reldate  Init      "23 October 2008"
.Release  INit      "2.0"     DLH total rewrite of Neom0008
.Reldate  Init      "09 October 2008"
.Note before turned on we need to start sending out letters of impending credit holds
.LM has agreed we can turn of credit for their clients at 75 days
.Must read "bancruptcy" file and not lock out for those LR's
.envision it being run at least weekly
.
.read invoice file - open items only if not in bancruptcy file and meet other criteria lock out
.create file for impending and credit hold letters
. need to look at bill direct info (for Brokerage). File should have sufficent data to produce letter to correct address with
.information regarding which orders need to be able to sort by broker and mailer
.

.RELEASE  INIT      "1.6"         DLH 23MAR92    NMLRXX,
.RELEASE  INIT      "1.5"        D.L. HERRICK    19AUG91
.              ' '=OK,
.             "*" = ON HOLD.
.             "I" = INACTIVE,
.             "B" = CREDIT RISK.  -      reset nightly if released
.             "N" =   NEW MAILER.
.             "P" = POLITICAL MAILER.  - reset nightly if released
.             "W" = Warning - read note                     ; 21Dec2000
.             "M" = Must Prepay                                            ; 05Mar2002
.             "9" = On hold until over 90s paid             ; 05Mar2002
.             "G" = Guarantees are always required          ; 05Mar2002
.             "g" = Guarantees No longer accepted           ; 06Apr2004
.
.
. FILE DESCRIPTIONS.
. ..................
Input     IFile     FIXED=300,KEYLEN=6,NODUPLICATES
Letter    FILE        .orders we are going to send a warning letter to brokers about 60 & +
Lock      FIle      .orders that are causing LM Mailers to go on credit Hold  75 & + also get a letter
LMfile    File      .list for list management
LMfile1   File      .list for list management mailers relocked
......................................................................................................................................
.
.
. PROGRAM VARIABLES
. .................
Count     Form      5
RCount    Form      5       Number of mailers relocked
READNUM   FORM                7       NUMBER OF RECORDS READ.
UPDNUM    FORM                5       NUMBER OF RECORD UPDATED.
CONTNUM   FORM                3
LockCount Form      5
TodayIs     Form            5              .Todays date in Julian
InvDteIS  Form            5              .Invoice date in Julian
MLDateIS  Form            5              .Mail date in Julian
HoldBrk   dim       7         .brk & contact #'s
HoldMlr   dim       7         .MLR & contact #'s
.begin patch 2.2
.PdfFName  Dim       35
PdfFName  Dim       55
PdfSName  Dim       35            .name without path or extension
.end patch 2.2
Laser     pfile
LetterType          Dim       7
faxname   dim       45
faxtele   dim       10
faxattn   dim       45
FaxFlag   Form      1
FileCheck FIle
trapcount form      4
LttrEmail Dim       75
EmailFlag Dim       1
TimesNew11          font
QTYMSK    INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT    DIM       11         *USED FOR ORDER PRINT
QTYNUM    FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
NINLogo   PICT
Longdist  Dim       1
Pass      Form      1
dlFiles   datalist
DLresult  form      9
DLndx     form      9
dmFileName          dim       80
DetCount  FOrm      2
Str7b     Dim       7         .temp field for old broker 4bytes & old broke contact #'s 3bytes
SJdate    Form      5            starting credit hold date in Julian
str256    Dim       256
Dupemlr   dim       7
.begin patch 2.09
HoldCOMp  Dim       6
.end patch 2.09
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.
.
          MOVE                "Credit0003" TO PROGRAM
          MOVE                "Names In The News" TO COMPNME
          MOVE                "CHECK CLIENT CREDIT" TO STITLE
          CALL                PAINT
          CALL                FUNCDISP
          create    TimesNew11,"Times New Roman",size=11
          move      "300" to Column1
          move      "1200" to Column2
          move      "2500" to Column3
          move      "4000" to Column4
          move      "4500" to Column5
          move      "5000" to Column5
.>>>>>>>>>>>>>>>>>>>>>
.         Goto      PassOne
.         GOTO      exittwo
.          shutdown
.>>>>>>>>>>>>>>>>>>>>>
............................................................................................................
.backup previous sent files
Prep
.lets save previous files
.first kill from oldest folder  \\nins1\e\storage\credit\six
.begin patch 2.03
.         create dlFiles=1:10:1:10,visible=0
          clear     Mailbody
          FIndDIr   "\\nins1\e\storage\credit\six\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopsix   
...
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear taskname
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName

                    pack taskname from "\\nins1\e\storage\credit\six\",DmFIleName  ."comment :)
                    erase Taskname                          
                    endif
          repeat
.         endif

ExitSix
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\six\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      taskname from "\\nins1\e\storage\credit\six\",DmFIleName              ."comment :)
.         
.         FINDFILE Taskname,WRITE=Str25
.               erase          taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
.move all from five  to six   \\nins1\e\storage\credit\Five
.begin patch 2.03
          clear     Mailbody
          FIndDIr   "\\nins1\e\storage\credit\five\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopFive
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\Five\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\six\",DmFIlename               ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif

ExitFive
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\Five\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\Five\",DmFIleName                 ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\six\",DmFIlename               ."comment :)
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
.move all from four  to five   \\nins1\e\storage\credit\Four
.begin patch 2.03
          clear     Mailbody
          FIndDIr   "\\nins1\e\storage\credit\four\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopfour
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\Four\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\five\",DmFIlename              ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif
ExitFour
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\Four\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\Four\",DmFIleName                 ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\five\",DmFIlename              ."comment :)
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
.move all from three  to four   \\nins1\e\storage\credit\three
.begin patch 2.03
          clear     Mailbody
          FIndDIr "\\nins1\e\storage\credit\three\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopthree
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\three\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\four\",DmFIlename              ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif
Exitthree
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\three\*.PDF":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\three\",DmFIleName                ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\four\",DmFIlename              ."comment :)
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
.move all from two  to three   \\nins1\e\storage\credit\two
.begin patch 2.03
          clear     Mailbody
          FIndDIr "\\nins1\e\storage\credit\two\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
looptwo
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\two\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\Three\",DmFIlename             ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif
Exittwo
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\two\*.PDF":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\two\",DmFIleName                  ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\three\",DmFIlename             ."comment :)
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03     
.move all from one  to two   \\nins1\e\storage\credit\one
.begin patch 2.03
          clear     Mailbody
          FIndDIr "\\nins1\e\storage\credit\one\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopOne
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          cmatch    "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\One\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\Two\",DmFIlename               ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif
ExitOne
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\one\*.PDF":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\one\",DmFIleName                  ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\two\",DmFIlename               ."comment :)
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
.         call      debug
.move all from sent  to one   \\nins1\e\storage\credit\sent
.begin patch 2.03
          clear     Mailbody
          FIndDIr "\\nins1\e\storage\credit\sent\*.*",MailBody,Itemcount=Result
.         if        (n5 > c0)
loopZero
          FOr       n4 from c1 to Result
          Clear     DmFIleName
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    clear     Str255
                    unpack    DmFileName into Str1,str255
                    move      Str255,DmFilename
                    call      Trim using DmFileName
                    clear     Str255
                    pack      str255 from "\\nins1\e\storage\credit\sent\",DmFIleName  ."comment :)
                    pack      taskname from "\\nins1\e\storage\credit\one\",DmFIlename               ."comment :)
                    Rename    Str255,Taskname                          
                    endif
          repeat
.         endif
ExitZero
.         shutdown  "cls"

.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\storage\credit\sent\*.PDF":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nins1\e\storage\credit\sent\",DmFIleName                 ."comment :)
.         pack      taskname from "\\nins1\e\storage\credit\one\",DmFIlename               ."comment :)
.         2
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 2.03
PassOne
          MOve      C1,pass
          Erase     "c:\work\Letter.dat"
          Erase     "c:\work\Lock.dat"
          Erase     "\\nins1\e\data\Letter.dat"
          Erase     "\\nins1\e\data\Lock.dat"
          DISPLAY             *P15:12,"NUMBER of Invoices Examined: ":
                    *P15:14,"NUMBER OF Records UPDATED  : ";
          Open      INPut,"NInINv5.isi|NINS1:502"
          move      c1,Brptpath
          Prepare   Letter,"c:\work\Letter.dat",Exclusive
          Prepare   Lock,"c:\work\Lock.dat",Exclusive
          MOve      c3,NORDLOCK              
          MOve      c1,NORDPath
          MOve      c4,NInvPath                lr sequence only open invvoices
.         MOve      c5,NInvPath                invoice sequence only open invvoices
          Move      "11",MM
          Move      "03",dd
          move      "08",yy
          call      cvtjul
          move      Juldays,Sjdate
          clock     timestamp,timestamp
                    unpack    timestamp,str2,yy,mm,dd
                    pack      Today,mm,slash,dd,slash,yy
                    call      cvtjul
                    move      juldays,TODAYIS
          Loop
InputInv  
.         Call      Ninvks
.         Call      NinvSeq
          Readks    Input;Invvars
          UNtil     Over

          ADD       C1 TO READNUM
          DISPLAY   *P45:12,READNUM,b1,Invnum;

          if        (STATB = "P")
          goto      InputINv
          endif
.begin patch 2.4 check for zero due
           if         (ar = c0)
           goto       InputINv
           else
           MOVE      LRN TO NADJFLD
           call      Nadjkey
                      If             Not over
                      add        asrecadj to ar
                                 if         (ar = c0)
                                 goto       InputINv
                                 endif
                      endif
           endif
.end patch 2.4
.check bankruptcy file

          packkey   Brptfld,LRN
          call      BrptKey
          If        Not over
          goto      InputInv 
          endif

          scan      "CSH",CHKN1                   .is it in a control?
          goto      InputINv if equal
          Reset     CHKN1
.Check Invdate if its less than 20 days forget it we had a billing issue skipit
          move      INVDTEY to yy
                    move                InvDted to dd
                    move                Invdtem to mm
                    call                cvtjul
          Move      Juldays,InvDteIs
          

          IF        (invdteIs +20 >= TodayIS)
          goto      InputINv
          endif

          PackKey   NordFld from LRN
          Call      Nordkey
.for now its list managment - skip man fee & ARB's
          reset     EXFEELST
          Scan      Olnum,Exfeelst
          goto      InputInv if equal
          reset     Runcodes
          Scan      Olnum,Runcodes
          goto      InputInv if equal

.for now if not list managment - skip
          pack      str2 from osales10,osales
          rep       Zfill,str2
.begin patch 2.04
.          If        (str2 <> "06" & str2 <> "27" & str2 <> "28")
          If        (str2 <> "06" & str2 <> "27" & str2 <> "28" & str2 <> "02")
.end patch 2.04
          goto      InputInv
          endif
.         call      Debug

.check for original MD
                    pack      NMLDFLD1,"01X",LRN
                    clear   str8
          pack      str8,"99999999"
          call      NMLDAIM
          loop
                    until over
                    if (NMLDDATE < str8)
                              move      NMLDDATE,str8
                    endif
                    call      NMLDKG
          repeat
          if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
               unpack     str8 into omdtec,omdtey,omdtem,omdted
          else
.Use current Mail Date
          endif

          move           omdtey to yy
            move           omdted to dd
            move           omdtem to mm
            call           cvtjul
          Move      Juldays,MLDateIs
.if 60 0r more past maildate = potential warning
.          call      debug
          IF        (todayIS - 60 >= MlDateis & (LET90D = B1 or LET90D = ""))       .no notice yet & over 60
          ADD                 C1 TO UPDNUM
          Write     Letter,Seq;OrdVars
.if 75 0r more past maildate = potential hold
          Elseif    (todayIS - 75 >= MlDateis)       .oops old
                    If        (LET90D = "1")     .already got warning send notice lets put on hold
.add some code as we have not run for a while
.IE if start date is 10/17/08 do NOT do lock outs until 11/01/08
                              if        (todayis > SJdate)         .after 11/01/08 ??
                              ADD                 C1 TO UPDNUM
                              Write     Lock,Seq;OrdVars
                              endif
                    Elseif    (LET90D = "2")     .already got warning, still open put back on hold
.          call      debug
                    Packkey   Mkey from Omlrnum,ocobn
                    rep       Zfill,MKey
                    CALL                Nmlrkey
                              if        (compCredit = " " | compCredit = "")
                              MOVE                "*" TO CompCredit
                              CALL                Compupd
                              Move      "!",ORCODE
                              Write     Lock,Seq;OrdVars
.begin patch 2.08
                              Elseif    (compCredit = "B" | compCredit = "M" | compCredit = "P" | compCredit = "9" | compCredit = "G")
                              Move      "!",ORCODE
                              Write     Lock,Seq;OrdVars
.end patch 2.08

                              endif
                              
                    endif     
          endif               
          DISPLAY             *P45:14,UPDNUM;
          
          Repeat
.........................................................................................................................
.Sort the files
          Weof      Lock,seq
          Weof      Letter,Seq
          close     Lock
          CLose     Letter
.
          call      Debug
          Display   *p1:22,*el,"sorting Lock"
.          Pack      Taskname from "c:\work\Lock.Dat \\nins1\e\data\Lock.Dat -303-306,307-309,3-6,7-12"
          Pack      Taskname from "c:\work\Lock.Dat c:\work\Lock.srt -303-306,307-309,3-6,7-12"
          Sort      Taskname
.
          Display   *p1:22,*el,"sorting Letter"
.          Pack      Taskname from "c:\work\Letter.Dat \\nins1\e\data\Letter.Dat -303-306,307-309,3-6,7-12"
          Pack      Taskname from "c:\work\Letter.Dat c:\work\Letter.srt -303-306,307-309,3-6,7-12"
          Sort      Taskname
          call      Debug
.Letters
PassTwo
          MOve      C2,pass
          Display   *p1:22,*el,"Printing Letter"
          Move      "Notice",LetterType
          Move      C0,Count
.          Open      Letter,"e:\data\letter.dat|NINS1:502"
          Open      Letter,"c:\work\letter.srt",Exclusive
          Loop
          Read      Letter,seq;Ordvars
          Until     OVer
          If        (count = c1)
          call      BreakLtr
.set break goodies  
          call      Debug
          endif
          call      Debug
          pack      STR7b from obrknum,OBRKCNT
          Pack      str7 from Omlrnum,ocobn
          rep       Zfill,str7B
          rep       Zfill,str7
          if        (str7B <> Holdbrk | str7 <> Holdmlr)
          call      BreakLtr
          endif
.how many per page etc
.ok we added to letter mark the invoice
          Move      C1,Ninvpath
          PackKey   Ninvfld,Olrn
          call      Ninvkey
          Move      c1,Let90D
          call      NinvUpd
          Call      Detail
          repeat
twoexit
          if        (PDfFname <> "")
          call      Footer
          call      Senditout
          Clear     PdfFName
          Clear     HOldBrk
          Clear     HoldMLR
          Clear     Str7B
          Clear     Str7
          clear     Obrknum
          Clear     Obrkcnt
          Clear     Omlrnum
          Clear     Ocobn
          endif
.Lock
PassThree
          MOve      C3,pass
          Prepare   LmFIle,"c:\work\LmCredit.dat",exclusive
          Prepare   LmFIle1,"c:\work\LmCredit1.dat",exclusive
          Display   *p1:22,*el,"Printing Lock"
          Move      "Credit",LetterType
          Move      C0,Count
.          Open      Lock,"e:\data\lock.dat|NINS1:502",exclusive
          Open      Lock,"c:\work\lock.srt",exclusive
          clear     holdmlr
          Loop
p3Loop
          Read      Lock,seq;Ordvars
          Until     OVer

          IF        (orcode = "!")

          Packkey   Mkey from Omlrnum,ocobn
                    if        (Mkey <> dupemlr)         
                    add       c1,Rcount                     .returned to hold
                    call      Nmlrkey
          
.          .         write     LmFIle1,seq;mcomp
                    write     LmFIle1,seq;compnum,mcomp
                    move      Mkey,Dupemlr
                    endif
          goto      P3loop
          endif

          add       c1,count
          If        (count = c1)
          call      BreakLtr
.set break goodies  
          endif
          pack      STR7B from obrknum,OBRKCNT
          Pack      str7 from Omlrnum,ocobn
          rep       Zfill,str7B
          rep       Zfill,str7
          if        (str7B <> Holdbrk | str7 <> Holdmlr)
          call      BreakLtr
          endif
          

.how many per page etc
.ok we added to letter mark the invoice
          Move      C1,Ninvpath
          PackKey   Ninvfld,Olrn
          call      Ninvkey
          Move      c2,Let90D
          call      NinvUpd
          Call      Detail

          repeat
          if        (PDfFname <> "")
          call      Footer
          call      Senditout
          endif     
          Goto      Eoj

.warning letter
BreakLtr
.1st sendout old one
          if        (PDfFname <> "")
          call      Footer
          call      Senditout
          endif
.prepNewone         
          if        (pass = 3)
.          add       c1,lockcount
.         write     LmFIle,seq;mcomp,brcomp
          Packkey   Mkey from Omlrnum,ocobn
          rep       Zfill,MKey
          call      Debug
          CALL                Nmlrkey
                    If        (compCredit <> " ")
.status is already set - skip
                    Else               
                    add       c1,lockcount
                    MOVE           "*" TO COMPCREDIT
                    CALL           COMPUPD
                    endif
          
          write     LmFIle,seq;compnum,mcomp
          endif
          
          Call      PrepPrtFile
          call      Header


          return
          
..................................................
PRepPrtFIle         pack      HoldBRk from obrknum,OBRKCNT
          Pack      HoldMLr from Omlrnum,ocobn
          rep       Zfill,Holdbrk
          rep       Zfill,HoldMLR
.begin patch 2.2
.          pack      PdfFname,LetterType,holdbrk,"_",Holdmlr
          pack      PdfSname,LetterType,holdbrk,"_",Holdmlr
          pack      PdfFname from "c:\work\pdf\",LetterType,holdbrk,"_",Holdmlr,".pdf"
.          PRTOPEN   Laser,"PDF995",PdfFname
          PRTOPEN   Laser,"PDF:",PdfFname
.end patch 2.2
          PRTPAGE   Laser;*UNITS=*HIENGLISH:
                    *ORIENT=*Portrait:
                    *MarginL=1;

.get fax / email info
          move      "                                             ",faxattn
          move      "                                             ",faxname
          clear     faxattn
          clear     faxname
          move      No,emailflag
          move      c0,faxflag
          

          pack      MKEY,OMLRNUM,Ocobn
          rep       zfill,MKEY
          move      "SetPrintFlag-NMLRKEY",Location
          pack      KeyLocation,"Key: ",MKEY
          call      NMLRKEY             .make sure we have Mailer info
.
          pack      NBRKFLD,OBRKNUM,Obrkcnt
          rep       zfill,NBRKFLD
          move      "SetPrintFlag-NBRKKEY",Location
          pack      KeyLocation,"Key: ",NBRKFLD
          call      NBRKKEY             .make sure we have last brker info
.......................
          move      C0,N10
          move      BRFAX,N10



          if (N10 = C0)

                    move      C0,N10
                    move      MFAX,N10
                    if (N10 = C0)
                              move      C1,faxflag
                    else
                              move      MCOMP,faxname
                              move      MFAX,FAXTELE
                              move      MCONTCT,faxattn
                              move      C2,faxflag
                              call      Trim with CompEmail
                              call      Trim with cnctemail
                    
                              if        (cnctemail = "")
                              move      CompEmail,LttrEmail
                              else
                              move      CnctEmail,LttrEmail
                              endif

                              if        (LttrEmail <> "")
                              move      Yes,Emailflag
                              else
                              move      No,Emailflag
                              endif
.

                    endif
.         elseif (BrFaxOFlag <> "T")       .ignore order fax flag
          else
                    move      BRCOMP,faxname
                    move      BRFAX,FAXTELE
                    move      BRCNTCT,faxattn
                    move      C2,faxflag          .do fax.

                    call      Trim with BrEmail
                    call      Trim with cnctemail
                    
                    if        (cnctemail = "")
                    move      BREmail,LttrEmail
                    else
                    move      CnctEmail,LttrEmail
                    endif

                    if        (LttrEmail <> "")
                    move      Yes,Emailflag
                    else
                    move      No,Emailflag
                    endif


          endif
.begin patch 2.10          
.begin patch 2.41          
          call      Trim with CompEmail
                    
           if        (CompActEmail <> "")
           move      CompActEmail,MailCC
           Else
          Clear     MailCC
          move      C0,N10
          move      COMPACCTFAX,N10

           if (N10 = C0)

.          call      Trim with CompEmail
.                    
.                    if        (CompActEmail <> "")
.                    move      CompActEmail,MailCC
.                    endif
.          Else
.end patch 2.41          
           count     N2,COMPACCTFAX
           compare   C10,N2
                     if equal
                               move      C1,LONGDIST
                               unpack    COMPACCTFAX,str3,str7
                               match     "510",str3                    .LOCAL ?
                               if equal
                                         move      str7,faxtele
                                         clear     LONGDIST
                               else
                                         match     B3,str3             .LOCAL ?
                                         if equal
                                                   move      str7,faxtele
                                                   clear     LONGDIST
                                         endif
                               endif
                     endif
          endif
.         pack      MailCC,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
          pack      MailCC,"+",longdist,faxtele,"@FAX.nincal.com"
          endif
.end patch 2.10          
          return
....................................................
Header
          move      c0,DetCount
          prtpage   Laser;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
          move "300" to row
          add eightlpi to ROW
          add eightlpi to ROW
          add "600" to Row
          move      C5,Column1

          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ",Today;
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"To: ",Faxname,b2,Faxattn;
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"From: Names in the News - Accounting fax:(415)433-7796";
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Just a reminder that the following order(s) are ";
          Prtpage Laser;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," past due for payment.";
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mailer: ",Mcomp;
          add eightlpi to ROW                                                          
               return
.........................................................................................................................
Detail
          add       c1,DetCount
          if        (Detcount > 13)
          PRTPAGE   Laser;*NewPAge;
          call      Header
          endif
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pColumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"LR:",Olrn;
          Prtpage Laser;*pColumn2:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"PO## ",OMLRPON;
          Pack str10 with omdtem,"/",omdted,"/",omdtec,omdtey
          Prtpage Laser;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date: ",Str10;
          pack      QTYOUT,QTYMSK
          call      Trim using OQTY
          move      OQTY,QTYNUM
          edit      QTYNUM,QTYOUT
          Prtpage Laser;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Quantity: ",qtyout;
.begin patch 2.3
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pColumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"INV## ",invnum;
.end patch 2.3

          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pColumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"List: ",O1Des;


               return

.........................................................................................................................
Footer
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          IF        (pass = 2)
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Timely payment is appreciated and will facilitate acceptance of new orders for your mailer.";
          ElseIf    (pass = 3)
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment is required before we can accept new orders for your mailer.";
          endif
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                          
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
          add eightlpi to ROW 
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"I sent in check ## ________ on  ____/____/____  for $_____________";
          add eightlpi to ROW 
          add eightlpi to ROW                                                          
          add eightlpi to ROW                                                           
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment will be sent on ____/____/____";
          add eightlpi to ROW 
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          If        (pass = 2)
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"If payment is not received within 15 days, we will place the mailer’s credit status on hold.";
          ElseIf    (pass = 3)
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your the mailer’s credit status is on hold until these overdue items are paid.";
          endif
          add eightlpi to ROW 
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Thank you for your attention to this matter.";
          add eightlpi to ROW 
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your name:   _____________________________________________________";
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Phone/Email: _____________________________________________________";
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"You can reach us: Phone: (510)302-4607, Fax: (415)433-7796, Email: Billing@nincal.com";
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"This letter may reference orders placed on Pacific Lists, Inc. managed lists.";
          add eightlpi to ROW                                                          
          add eightlpi to ROW 
          Prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists is a division of Names in the News";
               return
..................................................
SendItOut
          prtclose Laser
.begin patch 2.2
.          call     GetPDFPath
.          pack      APIFileName,"!",PDFPATH,"\flag.dat",hexzero
.          loop
.          call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    move      "100",str3
.                    call      Waitin using str3
.                    repeat
          move      "500",str3
          call      Waitin using str3
.end patch 2.2

          Clear     Str35
          if        (faxflag = 2)
          count     N2,faxtele
          compare   C10,N2
                    if equal
                              move      C1,LONGDIST
                              unpack    faxtele,str3,str7
                              match     "510",str3                    .LOCAL ?
                              if equal
                                        move      str7,faxtele
                                        clear     LONGDIST
                              else
                                        match     B3,str3             .LOCAL ?
                                        if equal
                                                  move      str7,faxtele
                                                  clear     LONGDIST
                                        endif
                              endif
                    endif
          Else
          Clear     Faxtele
          endif
.if to be faxed
          Count     n2,faxtele
          call      Trim using faxname
          if        (EmailFlag = Yes)
          move      LttrEmail,MailTo
          Elseif    (faxflag = 2)
.         elseif    (n2 = 7 | n2 = 10)                .we have fax #
.2010 Apr 20 test DLH
.          pack      Str35,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
.          pack      MailTo,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
.          pack      Str35,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
.          pack      MailTo,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
          pack      MailCC,"+",longdist,faxtele,"@FAX.nincal.com"
          move        Mailto,str35

.2010 Apr 20 test DLH
          
          Else      
          Clear     Mailto                                 .no place to sendit
          call      Trim using Mailto
          return                        .so return it will be printed out by batch job
          endif
          if        (mailto = "" | Mailto = " ")
          call      Debug
          endif
          
.wait for the PDF's

.if its faxable or we can email do so
.else it will sit in c:\work\pdf and get printed
          if        (faxflag = 2 | EmailFlag = Yes)
.begin patch 2.2
.          pack      Str55 from "c:\work\pdf\",PdfFname,".pdf"
          pack      Str55 from PdfFname
.end patch 2.2
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from str55

.....
.outgoing copy
.for testing        
.                   Move      "GemmaSpranza@nincal.com",Mailto
.for testing        
                    Move      "Credit Status",MailSubjct
                    MOve      "ComputerRequest@nincal.com",MailFrom                      .

                    clear     Mailbody
                    Append    "To: ",Mailbody
                    Append    Faxname,Mailbody
                    append    b3,Mailbody
                    append    Faxattn,Mailbody
                    append    "<br>",MailBody
.                    append    CRLF,Mailbody
.                   Append    "RE: ",Mailbody
.                   append    mcomp,Mailbody
                    reset     Mailbody
.begin patch 2.2
..First check 995 autolaunch settings
.                    Call      GetPDFPAth
.                    Call      PDF995Auto
.                    call      SetPDFFlag
.end patch 2.2
                    Move      "30",MailTimer
                    move      c1,mailtype
                    call      SendMail
.Inhouse copy
.                   Pack      Mailto from "CreditLetter@nincal.com,Skelly@nincal.com"
                    Pack      Mailto from "CreditLetters@nincal.com"

                    endset    Mailbody            
                    append    "<br>",MailBody
                    append    LttrEmail,Mailbody
                    append    "<br>",MailBody
                    append    Str35,Mailbody
                    append    "<br>",MailBody
                    reset     Mailbody
                    Move      "30",MailTimer
                    pause     C2
                    move      c1,mailtype
                    call      SendMail



.         after sending copy to folder
.begin patch 2.2
.          pack      taskname from "\\nins1\e\storage\Credit\sent\",PdfFname,".pdf"
          pack      taskname from "\\nins1\e\storage\Credit\sent\",PdfSname,".pdf"
.end patch 2.2
          copyfile  str55,taskname
          pause     C2
          erase     str55
          return

          Else

          endif

          Clear     PdfFName
.begin patch 2.2
          Clear     PdfSName
.end patch 2.2
          return

............................................................................................................
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Credit Letters - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    "<br>",MailBody
                    append    str55,MailBody
                    append    "<br>",MailBody
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    move      c1,mailtype
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
...............................................................................................................         

.         Packkey   COmpfld from Compnum
..
.                   CALL      CompKEY
..                  GOTO      READ IF OVER
.         If        (compCredit <> " ")
.         return                           .status is already set
.         endif
.                   MOVE      "*" TO CompCredit
..                  CALL      CompUPD
.
EOJ       Clear     Mailbody
          weof      Lmfile,seq
          Close     Lmfile
          if        (LockCount > 0)
.begin patch 2.02
          MOve      "C:\work\lmcredit.srt",Taskname
          erase     Taskname
          Display   *p1:22,*el,"sorting Lmfile"
.         Pack      Taskname from "c:\work\Lmcredit.Dat C:\work\lmcredit.srt -1-55"
          Pack      Taskname from "c:\work\Lmcredit.Dat C:\work\lmcredit.srt -7-61"
          Sort      Taskname
.         open      Lmfile,"c:\work\LmCredit.dat",exclusive

          open      Lmfile,"c:\work\LmCredit.srt",exclusive
.end patch 2.02
          Append    "The Following mailers have been put on hold",Mailbody
          Loop
.         Read      Lmfile,seq;mcomp,brcomp
          Read      Lmfile,seq;compnum,mcomp
          until     over
                    if        (compnum <> Holdcomp)
                    append    "<br>",MailBody
                    append    Compnum,Mailbody
                    append    " ",Mailbody
                    append    Mcomp,Mailbody
.                   append    " Thru ",mailbody
.                   append    Brcomp,Mailbody
                    append    "<br>",MailBody
                    move      Compnum,Holdcomp
                    endif
          
          repeat
          endif
          
          weof      Lmfile1,seq
          Close     Lmfile1
          pause     "5"
oopsy     
          if        (RCount > 0)
.begin patch 2.02
          MOve      "C:\work\lmcredit1.srt",Taskname
          erase     Taskname
          Display   *p1:22,*el,"sorting Lmfile1"
.         Pack      Taskname from "c:\work\Lmcredit1.Dat C:\work\lmcredit1.srt -1-55"
          Pack      Taskname from "c:\work\Lmcredit1.Dat C:\work\lmcredit1.srt -7-50"
          Sort      Taskname
          pause     "5"
.         open      Lmfile1,"c:\work\LmCredit1.dat",exclusive
          open      Lmfile1,"c:\work\lmcredit1.srt",exclusive
.end patch 2.02
          append    "<br>",MailBody
          Append    "The Following mailers have been Returned to  hold",Mailbody
          append    Crlf,Mailbody
          Append    "Returned holds - ",Mailbody
          append    RCOunt,Mailbody
          append    "<br>",MailBody

          Loop
.         Read      Lmfile1,seq;mcomp
          Read      Lmfile1,seq;compnum,mcomp
          until     over
          
                    if        (compnum <> Holdcomp)
                    packkey      COMPFLD,Compnum
                    rep       zfill,COMPFLD
                    move      "SetPrintFlag-compKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      CompKEY             .make sure we have Mailer info

                    append    "<br>",MailBody
                    append    Compnum,Mailbody
                    append    " ",Mailbody
                    append    Mcomp,Mailbody
                              if    (compCredit = "M")
                              append    " - Prepay Required",Mailbody
                              Elseif    (compCredit = "*")
                              append    " - Credit Hold",Mailbody
                              Elseif    (compCredit = "B")
                              append    " - Credit risk",Mailbody
                              Elseif    (compCredit = "P")
                              append    " - Political Mailer",Mailbody
                              Elseif    (compCredit = "G")
                              append    " - Guaranty Required",Mailbody
                              Elseif    (compCredit = "9")
                              append    " - On Hold till > 90's paid",Mailbody
                              Elseif    (compCredit = "g")
                              append    " - Guaranties not accepted",Mailbody
                              endif
.                   append    Crlf,Mailbody
                    move      Compnum,Holdcomp
                    endif
          Repeat
          endif
          
          append    "<br>",MailBody
          reset     Mailbody
          move      "Creques@nincal.com",Mailfrom

          Move      "GemmaSpranza@nincal.com,JenniferMagee@nincal.com,KrsniWatkins@nincal.com,DeniseHubbard@nincal.com",Mailto
.          Move      "ReubenHolland@nincal.com,AAronCinco@nincal.com,GemmaSpranza@nincal.com",Mailto
.          Move      "JackForder@nincal.com,AAronCinco@nincal.com,GemmaSpranza@nincal.com",Mailto
.          Move      "ShereneKelly@nincal.com,AAronCinco@nincal.com,GemmaSpranza@nincal.com",Mailto
.          Move      "ShereneKelly@nincal.com,AAronCinco@nincal.com,AgnesAlvarez@nincal.com,GemmaSpranza@nincal.com",Mailto
          Move      "DavidHerrick@nincal.com",Mailcc
          move      "Mailers on credit hold",MailSubjct
          Clear     MailAttach
          move      c1,mailtype
          call      SendMail

          DISPLAY   *P1:24,"Credit0003 DONE ",*W5;
          Shutdown  "cls"
 
EXIT
          Shutdown  "cls"
          STOP
          Include   BrptIO.inc
          INCLUDE             COMPIO.inc
          INclude   Cntio.inc
          INclude   nordIO.inc
          Include   NMLDIO.inc
          include   nINVIO.INC
.Begin  patch 2.4
           include    Nadjio.inc
.end patch 2.4
          INCLUDE             COMLOGIC.inc

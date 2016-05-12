.NINV0022.pls
.produce Broker (outside) Guaranty  Letters.
pc             Equ            0
               include        common.inc
               include        cons.inc
               include        norddd.inc
.begin patch 1.3
.               include        ninvdd.inc
               include        ninvdd.inc
.end patch 1.3
.Patch1.23
                              include   compdd.inc
                              include   cntdd.inc
.               include        nbrkdd.inc
.               include        nmlrdd.inc
.Patch9.31
               include        ndatdd.inc
               INCLUDE        OSLSPERN.inc
               INCLUDE        HP.INC
               INclude        nmlddd.inc
               Include        NINVBRKGDD.inc
               INCLUDE        CONSACCT.inc
.begin patch 1.6
               Include        Winapi.inc     
.end patch 1.6
.patch1.25
          include   NUSEDD.INC
.patch1.25
guarwkly       file
.Tempfile       File
release        init           "1.81"        DLH    New fax gateway
Reldate        INit           "2015 April 14"
.release        init           "1.8"        DLH    Sunbelt PDF
.Reldate        INit           "2013 April 23"
.release        init           "1.7"        DLH    SPOOL tRAP
.Reldate        INit           "10 sEPTEMBER 2012"
.release        init           "1.6"        DLH    Email to Gemma if no fax or email
.Reldate        INit           "21 May 2012"
.release        init           "1.5"        DLH    convert output to PDF add Email
.Reldate        INit           "17 February 2009"
.release        init           "1.4"        ASH    19JUNE2007          PLI Inclusion
.Reldate        INit           "19JUNE2007"
.release        init           "1.3"        DLH   9March2005          Invoice COnversion
.Reldate        INit           "09March2005"
.release  init      "1.27"        ASH   09DEC2004 FAXFILE.PRN
.release  init      "1.26"        DMB   01SEP2004 New Logo - prt page conversion
.release  init      "1.25"        DMB   14JUN2004 Added code to differentiate which server job is running on
.release  init      "1.24"        DMB   11JUN2004 Bug Fix from mailer conversion
.release  init      "1.23"        DMB   26MAY2004 Mailer Conversion  
.release        init           "1.22"  26May2004  DLH Print Mailer # on letter
.Reldate        INit           "01SEP2004"
.release        init           "1.21"  21May2004  DLH TUrn off broker update
.Reldate        INit           "21May2004"
.release        init           "1.2"  13April2004  DLH Suppress all office copies except #4
.Reldate        INit           "13Apr2004"
.release        init           "1.1"  12April2004  DB  Use Comp update for broker credit
.Reldate        INit           "12Apr2004"
.release        init           "1.0"  29JAN2004  DLH  
.Reldate        INit           "02Feb2004"
.

.begin patch 1.5
FileCheck FIle
trapcount form      4
LttrEmail Dim       75
EmailFlag Dim       1
PdfFName  Dim       55
.end patch 1.5

FaxFlag        form           1
countin        form           6
count          form           6
DATE           DIM            8
guartype       dim            4
SYSDAYS        FORM           5
DueDate        Dim            8
LetterFlag     Form           1
OfficeFlag     Form           1              ;0=fax copy, 1=office/file copy
Gdays          Form           2
LONGDIST       DIM            1
TIME           INIT           "HH:MM:SS"
HoldSDate      DIm            8
.**********.305233 FIRST NIN LR OF 1998

StatTextBoxes   StatText (3)
EditTextBoxes   EditText (3)
Button1        Button
ProgressBar         PROGRESS  
CurRec    form                   5.2
CurVal    form      3
LastVal   form      3
.Colors
white          color
grey           color
red            color
black          color
tcolor         color

.Set Up Menu Bar
mFile          menu
mEdit          menu
mHelp          menu
sSecurity      submenu
Timer          Timer 
.Present Data for Menu Bar
FData          init    "&File;-;E&xit"
HData          init    "&Help;&About"
.............................
ChecKBOX  CHECKBOX
.Patch1.25
PrintLoc  form      1
.Patch1.25
.Patch 1.26
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
spoolfle DIM       40                   .order  SPOOL FILEs
Tempfle DIM       40                   .order  SPOOL FILEs
PrintDoc  pfile
TimesNew11          font
.START PATCH 1.4 ADDED LOGIC
font7     font
          create    font7,"Times New Roman",size=9
.END PATCH 1.4 ADDED LOGIC

          create    TimesNew11,"Times New Roman",size=11
                    move "180" to eightlpi
          move "300" to Column1
          move "1200" to Column3
          move "3800" to Column5
CopyVar2 dim        5000 
.Patch 1.26
+
.............................................................................................................
.Set Vars used for About Box
               move    "Ninv0022.PLS",Wprognme
               move    "Outside Guaranty Reminders",Wfunction
               move    "David Herrick",Wauthor
               move    Release,Wrelease
               move    Reldate to Wreldate
.
.Declare forms, Always declare child forms first
mss1           plform  Error
abt            plform  About
x              plform  Generic
               winhide
.Load Forms, Always load parent form first
               formload x
               formload abt
               formload mss1
.
        
.Create Menus
               CREATE  Generic001;MFile,FData
               create  Generic001;mHelp,HData,Mfile
.Activate Menus
.FileGo leads to stop
               activate mFile,FileGo,result
               activate mHelp,HelpGo,result
.Create SubMenu

.Create Colors for EditText Inquiry
               create  white=*white
               create  grey=*ltgray
               create  RED=*RED
               create  black=*black
.
.Set Error Message Stat Text Boxes
.               call           SetErrorMssgDefault
.main
               move           c1 to nordpath
               move           c1 to ninvpath
               move           c3 to nordlock
               move           c1 to ndatpath
               move           c1 to nmlrpath
               move           c1 to nbrkpath
               Call           GetWinVer
         Trap       SPOOL1 giving error if SPOOL
.Patch1.25Code added
                                                  move      c1 to nusepath
                                                  clock     port to str3
                                                  unpack     str3 into str2,str1
                                                  pack       str3 from str1,str2
                                                  MOVE      str3 TO NUSEFLD .removed FOR TESTING only
                                                  REP       ZFILL IN NUSEFLD
                                                  CALL      NUSEKEY
                                                  if over
                                                            MOVE C1 TO PrintLoc
                                                  else
                                                            squeeze    nuseuser,nuseuser
                                                            match "MRNTS2" in NUSEUSER
                                                            if not equal
                                                                      move c1 to Printloc
                                                            else
                                                                      move c2 to Printloc
                                                            endif
                                                  endif
.Patch1.25
.
.               move           "NInv0022" to program
.               call           paint
               CLOCK          DATE TO DATE
               MOVE           DATE TO TODAY
               UNPACK         TODAY TO MM,STR1,DD,STR1,YY
               MOve           "20" to CC
               pack           HoldSdate from cc,yy,mm,dd
.               DISPLAY        *P15:06,MM,SLASH,DD,SLASH,YY
               CALL           CVTJUL
               MOVE           JULDAYS TO SYSDAYS
               SetProp        Generic001,Title="Ninv0022"
               create         Generic001;StatTextBoxes(1)=50:70:10:110,"Records In:",""
               create         Generic001;StatTextBoxes(2)=50:70:210:310,today,""
               create         Generic001;StatTextBoxes(3)=150:170:50:340,"",""
               create         Generic001;EditTextBoxes(1)=50:70:80:130,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
               CREATE         Generic001;CHecKBOX=80:100:10:110:
                              "Goto Phase 2",STYLE=3DOUT
               ACTIVATE       CHecKBOX
               activate       StatTextBoxes(1)
               activate       StatTextBoxes(2)
               activate       EditTextBoxes(1)
               CREATE         Generic001;BUTTON1=80:100:120:160,"Go"
               ACTIVATE       BUTTON1,Timeout,RESULT
               CREATE         TIMER,1800     ; 3.0 minutes
               ACTIVATE       TIMER,Timeout,RESULT
.
          loop
                    waitevent
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
               repeat
Phase1
               prepare        guarwkly,"\\nins1\e\data\brkGuarWkly.dat",exclusive
.
.              move      "305233" to nordfld
              move      "370000" to nordfld
              call      nordkey
..............................................................................
.Main = PHASE 1 GET THE RECORDS
Main           call           nordks
               if             over
               weof           GuarWkly,seq
               close          GuarWkly
               goto           Phase2

               endif
               add            c1 to countin
               move           countin to str6
               setitem        EditTextBoxes(1),0,str6
.               display        *p10:08,"Records reviewed : ",countin,b1,omdtem,"/",omdted,"/",omdtey

               cmatch         b1 to OBRKGUAR
               goto           Main if eos
               goto           Main if equal
               Move           ObrkGuar to N1
               Branch         N1 to GetINv,GetInv,Getinv,Getinv
               goto           Main
GetInv
               move           olrn to ninvfld
               call           ninvkey
               if             over
               Goto           Main                          ;can't remind them to pay a bill not sent
               else
               cmatch         "P" to statb
               goto           Main if equal                 :paid - skip  
.                              if             (wsjpc = "2" or wsjpc = "3")        done with letters
.                              if             (wsjpc = "3")        done with letters
                              if             (wsjpc = "4")        done with letters
                              goto           main
                              endif
               endif
write          write          GuarWkly,seq;ordvars
               add            c1 to count
               display        *p10:10,"Records output : ",count
          goto           Main
.
.PHASE 2 SORT THE RECORDS
phase2        
.dave 08June2009   in case none found
                    If        (count = 0)
                    Call      Sendmess
                    goto      EOJ
                    endif
.dave 08June2009   in case none found
          
.testing testing testing
.               call           CreateProgressBar
.          call      InitProgressBar
.testing testing testing
               setitem        StatTextBoxes(3),0,""
               setitem        StatTextBoxes(3),0,"Printing & Faxing Letters"
               Activate       StatTextBoxes(3)
               move           "\\nins1\e\data\brkGuarWkly.dat" to str45
               open           tempfile,str45
          positeof       tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/504)   .'504 = 502(order record length) + 2 bytes for CR/LF
               close          tempfile
               move           c0 to count
               open           GuarWkly,"\\nins1\e\data\brkGuarWkly.dat",exclusive
               Loop
               Read           GuarWkly,seq;ordvars
               until          over
.check dates and if both letters already sent etc. only letter one so far
.want to send one letter 30 days prior to date due, 2nd 15               
.               if             (olrn = "526901")
.               call           debug
.testing testing testing
.               call UpdateProgressBar
.testing testing testing
               call           getmore
.               endif
               repeat
               goto           EOJ
Getmore
               move           olrn to ninvfld
               call           ninvkey
.add code to update the inv status
.check for original MD
               MOve           Wsjpc to letterflag
               pack NMLDFLD1,"01X",LRN
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
.if maildate is >= today + 91   to soon skipit
               if             (juldays >= (sysdays + 91))
               return
               endif
               move           Juldays to n5             .save original maildate
               call           age                       ;get guar type and due date  Juldays now has due date!
               move           n5 to Juldays             ;restore Juldays as cvtgreg destroyed
               add            Gdays to JUldays          ;Now equal to duedate
               Move           c0 to letterflag          :clear flag - if one not set - skip it.
.if past due  - send letter 3  or 4
               call           debug
               IF             (WSJPC = "2" or WSJPC = "3")
.               call           debug
               endif
               if             (Juldays < sysdays)     
.               call           debug
.                              If             (letterflag = c3) .third already sent
                              If             (WSJPC = "3") .third already sent
                              MOve           c4 to letterflag
                              else
                              move           c3 to letterflag
                              endif
.if  duedate is within 30 days and no letter has gone send letter 1             
.Else if duedate is within 15 days and letter 1 has gone out send letter 2
               Elseif         (sysdays >= (juldays-30) & wsjpc = " ") 
               move           c1 to letterflag
               Elseif         (sysdays >= (juldays-15) & wsjpc = "1") 
               move           c2 to letterflag
               endif
               If             (letterflag = c0)
               return
               endif
               packkey        Mkey from omlrnum,z3
               call           Nmlrkey
               Packkey        Ndatfld from olnum
               call           ndatkey
               PACKKey        NBRKFLD FROM OBRKNUM,OBRKCNT
               rep            zfill in nbrkfld
               CALL           NBRKKEY

               move           c1 to faxflag                 .reset flag
BRfax   
               add            c1 to count
               display        *p10:12,"Memo's produced : ",count
.temp          
.               goto           officecopy
.temp          
               TYPE           BRFax                        .VALID PHONE?
               IF             EQUAL    
               move           c2 to faxflag
.temp
.               MOve           "4154337796" to brfax
.temp          
               else
.we will print instead
               endif
               If             (faxflag = c2)
faxmem       
               COUNT          N2,BRFax
               COMPARE        C10 TO N2
.begin patch 1.5
          IF             Not EQUAL

.begin patch 1.6
.begin patch 1.8
.          Call      PDF995Auto
.          call      SetPDFFlag
.          pack      PDFfname,"Guar",Olrn
.          PRTOPEN   PRintDoc,"PDF995",PdfFname

          pack      PDFfname,"c:\work\pdf\Guar",Olrn,".pdf"
          PRTOPEN   PRintDoc,"PDF:",PdfFname
.end patch 1.8
.          PRTOPEN   PrintDoc,"\\NINs2\Laser2","Guarantee Letters"
.end patch 1.6
          PRTPAGE   PrintDoc;*UNITS=*HIENGLISH:
                    *ORIENT=*Portrait:
                    *MarginL=1;

.               IF             EQUAL
                    Else
.end patch 1.5
               MOVE           C1 TO LONGDIST
               UNPACK         BRFax INTO STR3,STR7
               match          "510" to str3            . local?
                              IF             EQUAL                    
                              MOVE           STR7 TO BRFax
                              CLEAR          LONGDIST
                              endif
               endif
               clock          date to date
               unpack         date into mm,str1,dd,str1,yy
               clock          time to time
               clear          str5
               append         time to str5
               reset          str5
.begin patch 1.5
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
.end patch 1.5

.                              if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
.Patch1.25 Code Added
.Patch1.26 Comment Out
.                                                                                                             if (printloc = c2)
.                                                 splopen        "Fax","A"
.                                                                                                             else
.                                                 splopen        "\\nts2\Fax","A"
.                                                                                                             endif
.                                 Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
.May not be needed
.                                                                                                             if (printloc = c2)
.                                                 splopen        "Fax","A"
.                                                                                                             else
.                                                 splopen        "Fax","A"
.                                                                                                             endif
.                              Elseif         (osflag = c0)         .Don't know prompt for printer
.patch1.25
.                                       splopen        "","A"
.                              endif
.Patch1.26 Comment Out
.begin patch 1.5
.begin patch 1.8
.                                        Call      PDF995Auto
.                                        call      SetPDFFlag
.                                        pack      PDFfname,"Guar",Olrn
.                                        PRTOPEN   PRintDoc,"PDF995",PdfFname
                                        pack      PDFfname,"c:\work\pdf\Guar",Olrn,".pdf"
                                        PRTOPEN   PRintDoc,"PDF:",PdfFname
.end patch 1.8

.Patch 1.26 Logic Added 
.                                       clear     spoolfle
.                                       append    "\\nins1\d\data\fax\GUAR",spoolfle
.                                       append    OLRN to spoolfle
.                                       APPEND    ".LST" TO spoolfle
.                                       reset     spoolfle
.patch 1.5
.                                       clear     tempfle
.                                       append    "\\nins1\d\data\fax\GUAR",tempfle
.                                       append    OLRN to tempfle
.                                       APPEND    ".TMP" TO tempfle
.                                       reset     tempfle
.                                                            PRTOPEN PrintDoc,"faxfile","Faxfile.prn"
                                      PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                                      *ORIENT=*Portrait:
                                      *MarginL=1;
.Patch 1.26 Logic Added
.Patch 1.26 Comment Out
.               print          "^[D",longdist,BRFax,"^[N",BRComp:
.                              "^[T",today,b1,str5,"^[S","Accounting","^]":
.                              *n,032,hpreset;
.patch1.26 Comment Out
.Patch 1.26 Logic Added
.                                                            pack      taskname,"c:\work\hdrfile.prn"
.                                                            splopen   taskname
..                                                           move "4154337796" to BRFax
..                                                           print   "^[D",longdist,BRFAX,"^[N",BRCOMP,"^]":
..                              *n,032,hpreset;
.               print          "^[D",longdist,BRFax,"^[N",BRComp:
.                              "^[T",today,b1,str5,"^[S","Accounting","^]":
.                              *n,032,hpreset;
.                                                            splclose
.Patch 1.26 Logic added
.begin patch xxx
.               Else
.                              if             (osflag = c1 or OsFlag = C5 or OsFlag = C6)         .nt 2000 or XP
..                              splopen        "\\NINs2\Laser2","A"
..Patch1.26 Comment Out
..                              splopen        "\\NINs2\Laser2","R"
..                              Elseif        (osflag = c3 or OsFlag = C4)         .win 95 98
..                              splopen        "Laser2","R"
..                              Elseif        (osflag = c0)         .Don't know prompt for printer
..                              splopen        "","R"
..                              endif
..               Print          HPReset;
..Patch1.26 Comment Out
..Patch 1.26 Logic Added
.                                        PRTOPEN PrintDoc,"\\NINs2\Laser2","Guarantee Letters"
.                                        PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
.                                                  *ORIENT=*Portrait:
.                                                  *MarginL=1;
.                              endif
.end patch 1.5
.Patch 1.26 Logic Added
               endif
.Patch1.26 Comment Out
.               Print          hpttray:
.                              hpport:
.                              033,"&l66P":               page length
.                              033,"&l65F";
.               call           PortraitLTRHEAD
.Patch1.26 Comment Out
.Patch 1.26 Logic Added
.START PATCH 1.4 REPLACED LOGIC
.                   prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
.                    if (OCompID = "P")
.                              prtpage   PrintDoc;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
.                                        *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
.                                        *p=1630:550,"Oakland, CA 94612-1429":
.                                        *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
.                                        *p=1630:800,"A Division of Names in the News":
.                                        *Alignment=*Left
.                    else
                              prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
.                    endif
.END PATCH 1.4 REPLACED LOGIC
.Patch 1.26 Logic Added
               move           c0 to officeflag
               if             (letterflag = c1)
               call           PrintLtr1
               Elseif         (letterflag = c2)
               call           PrintLtr2
               Elseif         (LetterFlag = C3)
               call           printltr3
               Elseif         (LetterFlag = C4)
               call           printltr4
               endif
.               print          hpreset;
.Patch 1.26 Comment Out
.               Splclose
.Patch 1.26 Comment Out
.Patch 1.26 Logic Added
                                        prtclose PrintDoc

.begin patch 1.6
.begin patch 1.8
.                               call     GetPDFPath
.                               pack      APIFileName from PDFPATH,"\flag.dat",hexzero
.                              
.                              loop
.                                        call      FindFirstFile
.                              until (APIResult = 0 | APIResult = hexeight)
.                                        move      "100",str3
.                                        call      Waitin using str3
.                              repeat
.end patch 1.6
.end patch 1.8


.desperate measures
                                        call      Debug
                                        Pause     "10"
               Release
.desperate measures
.begin patch 1.6
.            If             (faxflag = c2)
.end patch 1.6
.START PATCH 1.27 REPLACED LOGIC
.begin patch 1.5
.                                                 Rename "\\nins1\d\data\fax\faxfile.prn",SPOOLFLE
.                                                  Rename "C:\WORK\faxfile.prn",SPOOLFLE
..END PATCH 1.27 REPLACED LOGIC
..Patch 1.5 Logic Updated
.                                                  Call GETWINVER
.                                                  clear     copyvar2
.                                                  if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
.                                                            append    "!c:\winnt\system32\cmd.exe",CopyVar2
.                                                            append  " /c copy ",CopyVar2
.                                                  elseif (OSFLAG = "6")  .XP
.                                                            append    "!c:\windows\system32\cmd.exe",CopyVar2
.                                                            append  " /c copy ",CopyVar2
.                                                  else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
.                                                            append    "!c:\command.com",CopyVar2
.                                                            append  " /c copy ",CopyVar2
.                                                  endif
..Patch 1.5 Logic Updated
.                                                  append  "c:\work\hdrfile.prn /b + ",CopyVar2
.                                                  append  SPOOLFLE,CopyVar2
.                                                  append    " /b ",CopyVar2
.                                                  append  TEMPFLE,CopyVar2
.                                                  reset   CopyVar2
.                                                  execute CopyVar2
.                                                  clear   CopyVar2
.                                                  erase     SPOOLFLE
.                                                  rename TEMPFLE,SPOOLFLE
.                                                  if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
.                                                            append    "!c:\winnt\system32\cmd.exe",CopyVar2
.                                                  elseif (OSFLAG = "6")  .XP
.                                                            append    "!c:\windows\system32\cmd.exe",CopyVar2
.                                                  else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
.                                                            append    "!c:\command.com",CopyVar2
.                                                  endif
.                                                  append  " /c copy ",CopyVar2
.                                                  append  SPOOLFLE,CopyVar2
.                                                  append  " \\nts3\fax",CopyVar2
.                                                  reset   CopyVar2
.                                                  execute CopyVar2
.                                                  erase "c:\work\hdrfile.prn"
..                                                 erase SPOOLFLE
.begin patch 1.8
.          pack      Str55 from "c:\work\pdf\",PdfFname,".pdf"
          pack      Str55 from PdfFname
.end patch 1.8
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
.begin patch 1.6
.          if        (EmailFlag = Yes)
          if        (EmailFlag = No & Faxflag <> 2)
          Move      "GemmaSpranza@nincal.com",Mailto
          Elseif        (EmailFlag = Yes)
.end patch 1.6
          move      LttrEmail,MailTo
          Else     (faxflag = 2)

.          pack      MailTo,"IMCEAFACSYS-",longdist,BRfax,"@nincal.com"
          pack      MailTo,"+",longdist,BRfax,"@fax.nincal.com"
          endif
                    Move      "Guaranty reminder",MailSubjct
                    MOve      "ComputerRequest@nincal.com",MailFrom                      .
                    MOve      "ComputerRequest@nincal.com",MailBCC
.begin patch 1.6
                    Move      "GemmaSpranza@nincal.com,Creques@nincal.com",MailCC
.end patch 1.6

                    clear     Mailbody
                    Append    "Thank you for your attention to this matter. ",Mailbody
                    reset     Mailbody
..First check 995 autolaunch settings
.2014 Oct 7 - why is this still on????  turning off
.                    Call      GetPDFPATH
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                    Call      PDF995Auto
.end 2014 OCt 07
                    Move      "30",MailTimer
                    call      SendMail
.end patch 1.5
                    erase     str55
.begin patch 1.6
.                                        endif
.end patch 1.6
.Patch 1.26 Logic Added
officecopy
.begin patch 1.5
.accounting got a copy via email so we can skip
          goto      UpdateINv
.end patch 1.5

               move           c1 to officeflag
.begin patch 1.2
               if             (letterflag < c4)
               goto           UpdateInv                    for all letters except #4 just update, don't print office copy
               endif
.end patch 1.2
.               if             (osflag = c1 or OsFlag = C5 or OsFlag = C6)         .nt 2000 or XP
.Patch 1.26 Comment Out
.               splopen        "\\NINs2\Laser8","R"
.               splopen        "\\NINs2\Laser2","A"
.               elseif        (osflag = c3 or OsFlag = C4)         .win 95 98
.               splopen        "Laser8","R"
.               Elseif        (osflag = c0)         .Don't know prompt for printer
.               splopen        "","A"
.               endif
.               print          hpreset:
.                              hpttray:
.                              hpport:
.                              033,"&l66P":               page length
.                              033,"&l65F";
.               call           PortraitLTRHEAD
.Patch 1.26 Logic Added
                    PRTOPEN PrintDoc,"\\NINs2\Laser8","Guarantee Letters"
                    PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                              *ORIENT=*Portrait:
                              *MarginL=1;
.START PATCH 1.4 REPLACED LOGIC
.                   prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:0:8000:NINLogo
.                    if (OCompID = "P")
.                              prtpage   PrintDoc;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
.                                        *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
.                                        *p=1630:550,"Oakland, CA 94612-1429":
.                                        *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
.                                        *p=1630:800,"A Division of Names in the News":
.                                        *Alignment=*Left
.                    else
                              prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:0:8000:NINLogo
.                    endif
.END PATCH 1.4 REPLACED LOGIC
                              
.Patch 1.26 Logic Added
.Patch 1.26 Comment Out
               if             (letterflag = c1)
               call           PrintLtr1
               Elseif         (letterflag = c2)
               call           PrintLtr2
               Elseif         (LetterFlag = C3)
               call           printltr3
               Elseif         (LetterFlag = C4)
               call           printltr4
.call credit update routines               
               call           Nmlrkey
.;patch1.24
.               Move           "*" to Mstat
                                                  MOVE           "*" TO COMPCREDIT
               CALL           COMPUPD
.               call           Nmlrupd
.patch1.24
.Patch1.1
               CALL           NBRKKEY
.begin patch 1.21
.         MOVE          "g" TO COMPCREDIT
.               CALL           COMPUPD
.end patch 1.21
.EndPatch1.1
.Comment out 1.1
.               CALL           NBRKKEY
.               Move           "g",BRCREDIT
.               CALL           NBRKUpd
.EndCo 1.1
.begin patch 1.6
               endif
.end patch 1.6
.Patch 1.26 Comment Out
.               print          hpreset
.               SPLCLOSE
.Patch 1.26 Comment out
.Patch 1.26 Logic Added
                                                  prtclose PrintDoc
.begin patch 1.6
                               call     GetPDFPath
                               pack      APIFileName from PDFPATH,"\flag.dat",hexzero
                              
                              loop
                                        call      FindFirstFile
                              until (APIResult = 0 | APIResult = hexeight)
                                        move      "100",str3
                                        call      Waitin using str3
                              repeat
.end patch 1.6

.Patch 1.26 Logic Added
               release
.TEMP
.               Return
.TEmp
UpdateINV
               Move           Letterflag to wsjpc
               packkey        Ninvfld from olrn
               rep            zfill in ninvfld
               call           ninvtst
               if             not over
               call           Ninvupd
               endif
               Packkey        NinvBRKGfld From invnum
               rep            Zfill in NinvBRKGfld
               MOve           NInvBrkGfld to NINvBrkGinv
               unpack         HoldSdate into NinvBRKGMM,NinvBRKGMM,NinvBRKGMM,NinvBRKGdd
               Move           Time,NinvBRKGTime
               MOve           LetterFlag to NinvBRKGLTR
               call           NInvBrkGWrt
               RETURN
.........................................................................................................................
PrintLTr1
.Patch 1.26 Comment Out
               call           PrintLtrTop
.               print          *N,*N,*n,*n,*n,hpt050,"Date: ",Today:
.                              *N,*N,hpt050,"TO: ",hpt150,BRComp,"  -  Via facsimile":
.                              *N,*N,hpt050,"FROM:",hpt150,"Names in the News - Accounting":
.                              *n,*n,*n,*n,hpt050,"Just a reminder that the following order carries a ",Gdays," Day Payment Guarantee due on or before ",DueDate,".":
.                              *n,*n,hpt050,"LR##: ",hpt150,Olrn,hpt200,"   PO## ",OMLRPON:
.                              *n,*n,hpt050,"Mailer: ",hpt150,"(",omlrnum,") ",Mcomp:
.                              *n,*n,hpt050,"List: ",hpt150,Olstname:
.                              *n,*n,hpt050,"Mail Date: ",hpt150,omdtem,"/",omdted,"/",omdtec,omdtey:
.                              *n,*n,*n,hpt050,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.":
.                              *n,*n,hpt050,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.":  
.                              *n,*n,hpt050,"Please take a minute to fill out the following information and fax this form back to Names In The News.":
.                              *n,*n,*n,hpt050,"I sent in check ## ________ on  ____/____/____  for $_____________":
.                              *n,*n,*n,hpt050,"Payment will be sent on ____/____/____":
.                              *n,*n,*n,hpt050,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.":
.                              *n,*n,*n,hpt050,"Thank you for your attention to this matter.":
.                              *n,*n,*n,hpt050,"Your name:   _____________________________________________________":
.                              *n,*n,hpt050,"Phone/Email: _____________________________________________________"
.Patch 1.26 Logic Added
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Today;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"TO: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,BRCOMP;
.begin patch 1.5
                    if        (emailflag = Yes)
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via Email";
                    Else
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via facsimile";
                    endif
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via facsimile";
.end patch 1.5
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"FROM: ";
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Just a reminder that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee due on or before ";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,DueDate;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,".";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"LR:";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLRN;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"   PO## ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRPON;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mailer: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"(";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRNUM;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,") ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,MCOMP;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"List: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLSTNAME;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date: ";
                                Pack str10 with omdtem,"/",omdted,"/",omdtec,omdtey
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,str10;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"I sent in check ## ________ on  ____/____/____  for $_____________";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                           
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment will be sent on ____/____/____";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Thank you for your attention to this matter.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your name:   _____________________________________________________";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Phone/Email: _____________________________________________________";
.Patch 1.26 Logic Added

               return
.........................................................................................................................
PrintLTr2
.Patch 1.26 Comment Out
               call           PrintLtrTop
.               print          *N,*N,*n,*n,*n,hpt050,"Date: ",Today:
.                              *N,*N,hpt050,"TO: ",hpt150,BRComp,"  -  Via facsimile":
.                              *N,*N,hpt050,"FROM:",hpt150,"Names in the News - Accounting":
.                              *n,*n,*n,*n,hpt050,"A second reminder that the following order carries a ",Gdays," Day Payment Guarantee due":
.                              *n,hpt050,"on or before ",DueDate,".":
.                              *n,*n,hpt050,"LR##: ",HPT150,Olrn,hpt200,"PO## ",OMLRPON:
.                              *n,*n,hpt050,"Mailer: ",hpt150,"(",omlrnum,") ",Mcomp:
.                              *n,*n,hpt050,"List: ",hpt150,Olstname:
.                              *n,*n,hpt050,"Mail Date: ",hpt150,omdtem,"/",omdted,"/",omdtec,omdtey:
.                              *n,*n,*n,hpt050,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.":
.                              *n,*n,hpt050,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.":  
.                              *n,*n,hpt050,"Please take a minute to fill out the following information and fax this form back to Names In The News.":
.                              *n,*n,*n,hpt050,"I sent in check ## ________ on  ____/____/____  for $_____________":
.                              *n,*n,*n,hpt050,"Payment will be sent on ____/____/____":
.                              *n,*n,*n,hpt050,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.":
.                              *n,*n,*n,hpt050,"Thank you for your attention to this matter.":
.                              *n,*n,*n,hpt050,"Your name:   _____________________________________________________":
.                              *n,*n,hpt050,"Phone/Email: _____________________________________________________"
.Patch 1.26 Logic Added
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Today;
                    add eightlpi to ROW   
                    add eightlpi to ROW                                                                                                                 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"TO: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,BRCOMP;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via facsimile";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"FROM: ";
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Just a reminder that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee due on or before";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,DueDate;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,".";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"LR:";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLRN;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"   PO## ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRPON;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mailer: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"(";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRNUM;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,") ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,MCOMP;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"List: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLSTNAME;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date: ";
                                Pack str10 with omdtem,"/",omdted,"/",omdtec,omdtey
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,str10;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"I sent in check ## ________ on  ____/____/____  for $_____________";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                           
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment will be sent on ____/____/____";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Thank you for your attention to this matter.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your name:   _____________________________________________________";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Phone/Email: _____________________________________________________";

.Patch 1.26 Logic Added
               return
.........................................................................................................................
PrintLTr3
.Patch 1.26 Comment Out
               call           PrintLtrTop
.               print          *N,*N,*n,*n,*n,hpt050,"Date: ",Today:
.                              *N,*N,hpt050,"TO: ",hpt150,BRComp,"  -  Via facsimile":
.                             *N,*N,hpt050,"FROM:",hpt150,"Names in the News - Accounting":
.                              *n,*n,*n,*n,hpt050,"Please note that the following order carries a ",Gdays," Day Payment Guarantee":
.                              *n,hpt050,"that is now past due. Due date was on or before: ",DueDate,".":
.                              *n,*n,hpt050,"LR##: ",hpt150,Olrn,hpt200,"PO## ",OMLRPON:
.                              *n,*n,hpt050,"Mailer: ",hpt150,"(",omlrnum,") ",Mcomp:
.                              *n,*n,hpt050,"List: ",hpt150,Olstname:
.                              *n,*n,hpt050,"Mail Date: ",hpt150,omdtem,"/",omdted,"/",omdtec,omdtey:
.                              *n,*n,*n,hpt050,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.":
.                              *n,*n,hpt050,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.":  
.                              *n,*n,hpt050,"Please take a minute to fill out the following information and fax this form back to Names In The News.":
.                              *n,*n,*n,hpt050,"I sent in check ## ________ on  ____/____/____  for $_____________":
.                              *n,*n,*n,hpt050,"Payment will be sent on ____/____/____":
.                              *n,*n,*n,hpt050,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.":
.                              *n,*n,*n,hpt050,"Thank you for your attention to this matter.":
.                              *n,*n,*n,hpt050,"Your name:   _____________________________________________________":
.                              *n,*n,hpt050,"Phone/Email: _____________________________________________________"
.Patch 1.26 Comment Out
.Patch 1.26 Logic Added
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Today;
                    add eightlpi to ROW  
                    add eightlpi to ROW                                                                                                                  
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"TO: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,BRCOMP;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via facsimile";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"FROM: ";
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please note that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee that is now past due.  Due date";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," that is now past due.  Due date";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," was on or before ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,DueDate;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,".";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"LR:";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLRN;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"   PO## ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRPON;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mailer: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"(";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRNUM;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,") ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,MCOMP;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"List: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLSTNAME;
                    add eightlpi to ROW        
                    add eightlpi to ROW                                                                                                            
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date: ";
                                Pack str10 with omdtem,"/",omdted,"/",omdtec,omdtey
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,str10;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.";
                    add eightlpi to ROW     
                    add eightlpi to ROW                                                                                                               
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Timely payment is appreciated and will insure further acceptance of guarantees from your company.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"I sent in check ## ________ on  ____/____/____  for $_____________";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                           
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment will be sent on ____/____/____";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"If payment is not received on time, we have the option to place the mailer’s credit status on hold.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Thank you for your attention to this matter.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your name:   _____________________________________________________";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Phone/Email: _____________________________________________________";
.Patch 1.26 Logic Added
               return

.........................................................................................................................
PrintLTr4
               call           PrintLtrTop
.Patch 1.26 Comment Out
.               print          *N,*N,*n,*n,*n,hpt050,"Date: ",Today:
.                              *N,*N,hpt050,"TO: ",hpt150,BRComp,"  -  Via facsimile":
.                              *N,*N,hpt050,"FROM:",hpt150,"Names in the News - Accounting":
.                              *n,*n,*n,*n,hpt050,"Please note that the following order carries a ",Gdays," Day Payment Guarantee":
.                              *n,hpt050,"that is now past due. Due date was on or before: ",DueDate,".":
.                              *n,*n,hpt050,"LR##: ",hpt150,Olrn,hpt200,"PO## ",OMLRPON:
.                              *n,*n,hpt050,"Mailer: ",hpt150,"(",omlrnum,") ",Mcomp:
.                              *n,*n,hpt050,"List: ",hpt150,Olstname:
.                              *n,*n,hpt050,"Mail Date: ",hpt150,omdtem,"/",omdted,"/",omdtec,omdtey:
.                              *n,*n,*n,hpt050,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.":
.                              *n,*n,hpt050,"Despite our multiple attempts to contact you regarding this overdue payment, it has not been received.":
.                              *n,*n,hpt050,"We can no longer accept guarantees from your company, and your mailer is now on credit hold.":  
.                              *n,*n,hpt050,"If your payment is already in the mail please take a minute to fill out the following information and fax":
.                              *n,*n,hpt050,"this form back to Names In The News.":
.                              *n,*n,*n,hpt050,"I sent in check ## ________ on  ____/____/____  for $_____________":
.                              *n,*n,*n,hpt050,"Thank you for your attention to this matter.":
.                              *n,*n,*n,hpt050,"Your name:   _____________________________________________________":
.                              *n,*n,hpt050,"Phone/Email: _____________________________________________________"
.Patch 1.26 Comment Out
.Patch 1.26 Logic Added
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Today;
                    add eightlpi to ROW    
                    add eightlpi to ROW                                                                                                                
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"TO: ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,BRCOMP;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via facsimile";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"FROM: ";
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please note that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee that is now past due.  Due date";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee";
.                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," that is now past due.  Due date";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," was on or before ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,DueDate;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,".";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"LR:";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLRN;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"   PO## ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRPON;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mailer: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"(";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OMLRNUM;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,") ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,MCOMP;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"List: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,OLSTNAME;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Mail Date: ";
                                Pack str10 with omdtem,"/",omdted,"/",omdtec,omdtey
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,str10;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Broker guarantees payment from initial mail date as noted on both our order confirmation and invoice.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Despite our multiple attempts to contact you regarding this overdue payment, it has not been received.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"We can no longer accept guarantees from your company, and your mailer is now on credit hold.";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"If your payment is already in the mail please take a minute to fill out the following information and fax ";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"this form back to Names In The News.";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"this form back to Names In The News.";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
.START PATCH 1.4 REPLACED LOGIC
.                   Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
.END PATCH 1.4 REPLACED LOGIC
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"I sent in check ## ________ on  ____/____/____  for $_____________";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                           
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Payment will be sent on ____/____/____";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Thank you for your attention to this matter.";
                    add eightlpi to ROW 
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Your name:   _____________________________________________________";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW 
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Phone/Email: _____________________________________________________";
.Patch 1.26 Logic Added

               return
.........................................................................................................................
PrintLtrTop
.Patch 1.26 Logic Added
                                                  move "300" to row
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  add "600" to Row
.                                                 add eightlpi to ROW
.                                                 add eightlpi to ROW
.Patch 1.26 Logic Added
               if             (officeflag = c0)
.Patch 1.26 Comment Out
.               print          HPTMSRMN,hpfixed,*n,*n,*n,*n,*n,*n,*n:
.                              *n,*n,*n,hpt325,hpdtch10,hpbon,"Guarantee Reminder":
.                              *L,hpboff;
.Patch 1.26 Comment Out
.Patch 1.26 Logic Added
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,*boldon,"Guarantee Reminder",*boldoff;
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
.Patch 1.26 Logic Added
               Else
.Patch 1.26 Comment Out
.               print          HPTMSRMN,hpfixed,*n,*n,*n,*n,*n,*n,*n:
.                              *n,*n,*n,hpt325,hpdtch10,hpbon,"Guarantee Reminder":
.                              *L,hpboff,hpt325,"File Copy";
.Patch 1.26 Comment Out
.Patch 1.26 Logic Added
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,*boldon,"Guarantee Reminder",*boldoff;
                                                                                          add eightlpi to ROW
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,"File Copy";
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
.Patch 1.26 Logic Added
               endif
               return
.........................................................................................................................
.enters with maildate mm dd yy
.exits with day value for guaranty and  Due Date - both mm,dd,yy and "duedate"
age

               move           b4 to guartype
               if             (OBRKGUAR = "1")
               add            "30" to juldays
               move           "30" to gdays
               endif
               if             (OBRKGUAR = "2")
               add            "45" to juldays
               move           "45" to gdays
               endif
               if             (OBRKGUAR = "3")
               add            "60" to juldays
               move           "60" to gdays
               endif
               if             (OBRKGUAR = "4")
               add            "90" to juldays           .actually open ended.
               move           "90" to gdays
               endif               
               call           cvtgreg
               pack           DueDate from mm,"/",dd,"/",yy
               return
..............................................................................
Eoj            stop
.........................................................................................
FileGo
                winshow
                stop

.

HelpGo
               setprop        AboutMssg,visible=1
               return       
.......................................................................................................
UpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if            (CurVal <> LastVal)
                    setitem   ProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
InitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
CreateProgressBar
               CREATE         Generic001;ProgressBar=120:140:50:160:
                              STYLE=3DOUT
               Activate       ProgressBar
               return
.begin patch 1.5
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

.end patch 1.5
............................................................................................................
SendMEss
                    TrapClr   IO
                    Pack       MailSubjct,"No Outside Guaranty Letters - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "GemmaSpranza@nincal.com",MailTO
                    Move      "DavidHerrick@nincal.com",MailCC
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "No Outside Guaranty Letters",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

          
.......................................................................................................
Timeout
.get check box if checked goto phase 2 else "normal" processing
.                   
.                   
               DisableItem    Button1         
               DisAbleItem    CheckBox
               GETITEM        CHecKBOX,0,RESULT
               if             (Result = 1)
               goto           Phase2
               else
.testing testint
.                    goto      Phase2
               goto           Phase1
               endif
.......................................................................................................
Spool1
         TRAPCLR   SPOOL
         Trap       SPOOL1 giving error if SPOOL

          PRTOPEN PrintDoc,"\\NINs2\Laser8","Guarantee Letters"
          return
.......................................................................................................
.io includes
              include         nordio.inc
.begin patch 1.3
.              include         ninvio.inc
              include         ninvio.inc
.end patch 1.3
.Patch1.23
                              include   compio.inc
                              include   cntio.inc
.              include         nbrkio.inc
.              include         nmlrio.inc
.Patch1.23
              INclude         nmldio.inc
               include        hpio.inc
              include         ndatio.inc
              Include         NINVBRKGIO.inc
.patch1.25
                                        include   NUSEIO.INC
.patch1.25
              include         comlogic.inc


.NINV0025.pls
.produce INhouse Guaranty  reminders.
pc             Equ            0
               include        common.inc
               include        cons.inc
               include        norddd.inc
               include        ninvdd.inc
                              include   compdd.inc
                              include   cntdd.inc
               include        ndatdd.inc
               INCLUDE        OSLSPERN.inc
               INCLUDE        HP.INC
               INclude        nmlddd.inc
               Include        NINVBRKGDD.inc
               INCLUDE        CONSACCT.inc
          include   NUSEDD.INC
guarwkly       file
release        init           "1.3"        DLH    Sunbelt pdf
Reldate        INit           "2013 April 24"
.release        init           "1.2"        DLH    Remvoe SS from Mailto
.Reldate        INit           "15 December 2011"
.release        init           "1.1"        DLH    New for inside guarantees
.Reldate        INit           "7 October 2009"
.

.begin patch 1.5
FileCheck FIle
trapcount form      4
LttrEmail Dim       75
EmailFlag Dim       1
.begin patch 1.3
.PdfFName  Dim       35
PdfFName  Dim       55
.end patch 1.3
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
PrintLoc  form      1
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
spoolfle DIM       40                   .order  SPOOL FILEs
Tempfle DIM       40                   .order  SPOOL FILEs
PrintDoc  pfile
TimesNew11          font
font7     font
          create    font7,"Times New Roman",size=9

          create    TimesNew11,"Times New Roman",size=11
                    move "180" to eightlpi
          move "300" to Column1
          move "1200" to Column3
          move "3800" to Column5
CopyVar2 dim        5000 
+
.............................................................................................................
.Set Vars used for About Box
               move    "Ninv0025.PLS",Wprognme
               move    "NIN Broker Guaranty Reminders",Wfunction
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
               CLOCK          DATE TO DATE
               MOVE           DATE TO TODAY
               UNPACK         TODAY TO MM,STR1,DD,STR1,YY
               MOve           "20" to CC
               pack           HoldSdate from cc,yy,mm,dd
               CALL           CVTJUL
               MOVE           JULDAYS TO SYSDAYS
               SetProp        Generic001,Title="Ninv0025"
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
               prepare        guarwkly,"\\nins1\e\data\NinGuarWkly.dat",exclusive
.
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
               Move           GUARCODE to N1
               Branch         N1 to GetINv,GetInv,Getinv,Getinv
               goto           Main
GetInv
          Pack      str2 from Osales10,osales
          rep       zfill,str2
          if        (str2 <> "11" & str2 <> "29" & str2 <> "21")
          goto      Main
          endif
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
.in case none found
                    If        (count = 0)
                    Call      Sendmess
                    goto      EOJ
                    endif
          
.testing testing testing
.               call           CreateProgressBar
.          call      InitProgressBar
.testing testing testing
               setitem        StatTextBoxes(3),0,""
               setitem        StatTextBoxes(3),0,"Emailing Letters"
               Activate       StatTextBoxes(3)
               move           "\\nins1\e\data\NINGuarWkly.dat" to str45
               open           tempfile,str45
          positeof       tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/504)   .'504 = 502(order record length) + 2 bytes for CR/LF
               close          tempfile
               move           c0 to count
               open           GuarWkly,"\\nins1\e\data\NINGuarWkly.dat",exclusive
               Loop
               Read           GuarWkly,seq;ordvars
               until          over
.check dates and if both reminders already sent etc. only letter one so far
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
          Pack      str2 from Osales10,osales
          rep       zfill,str2
          if        (str2 = "11")
          move      "ListOrders@nincal.com ",Mailto
.          Elseif    (str2 = "29")
.          move      "ShirleySchoevaars@nincal.com",Mailto
.          Elseif    (str2 = "21")
.          move      "PamGrossman@nincal.com",Mailto
          Else
          Move      "GemmaSpranza@nincal.com",MailTO
          endif
BRfax   
               add            c1 to count
               display        *p10:12,"Memo's produced : ",count
faxmem       
               clock          date to date
               unpack         date into mm,str1,dd,str1,yy
               clock          time to time
               clear          str5
               append         time to str5
               reset          str5
                              move      Yes,Emailflag
.begin patch 1.3
.                                        pack      PDFfname,"Guar",Olrn
                                        pack      PDFfname,"c:\work\pdf\Guar",Olrn,".pdf"
.                                        PRTOPEN   PRintDoc,"PDF995",PdfFname
                                        PRTOPEN   PRintDoc,"PDF:",PdfFname
.end patch 1.3

                                      PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
                                                                                                                        *ORIENT=*Portrait:
                                                                                                                        *MarginL=1;
                    if (OCompID = "P")
                              prtpage   PrintDoc;*p=1630:50,*font=font10,*Alignment=*Center,"Pacific Lists, Inc.":
                                        *p=1630:425,*font=font7,"1300 Clay St. 11th Floor":
                                        *p=1630:550,"Oakland, CA 94612-1429":
                                        *p=1630:675,"415-945-9450 ","·"," Fax 415-945-9451":
                                        *p=1630:800,"A Division of Names in the News":
                                        *Alignment=*Left
                    else
                              prtpage   PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
                    endif
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
                                        prtclose PrintDoc
                                        call      Debug
                                        Pause     "10"
               Release

.begin patch 1.3
.          pack      Str55 from "c:\work\pdf\",PdfFname,".pdf"
          pack      Str55 from PdfFname
.end patch 1.3
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from str55

.....
.outgoing copy
                    Move      "Guaranty remimder",MailSubjct
                    MOve      "ComputerRequest@nincal.com",MailFrom                      .
                    MOve      "ComputerRequest@nincal.com",MailBCC

                    clear     Mailbody
                    Append    "Thank you for your attention to this matter. ",Mailbody
                    reset     Mailbody
..First check 995 autolaunch settings
.begin patch 1.3
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                    result
.          if (result = C0)
..Prepare Flag file
.                    pack      str45 from PDFPATH,"\flag.dat"
.                    prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                    Call      PDF995Auto
.end patch 1.3
                    Move      "30",MailTimer
                    call      SendMail
                    erase     str55
officecopy
                    goto      UpdateINv

               move           c1 to officeflag
               if             (letterflag = c1)
               call           PrintLtr1
               Elseif         (letterflag = c2)
               call           PrintLtr2
               Elseif         (LetterFlag = C3)
               call           printltr3
               Elseif         (LetterFlag = C4)
               call           printltr4
               endif
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
               call           PrintLtrTop
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Date: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Today;
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"TO: ";
                    Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,BRCOMP;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"  -  Via Email";
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"FROM: ";
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif

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

               return
.........................................................................................................................
PrintLTr2
               call           PrintLtrTop
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
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

               return
.........................................................................................................................
PrintLTr3
               call           PrintLtrTop
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please note that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee that is now past due.  Due date";
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
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
               return

.........................................................................................................................
PrintLTr4
               call           PrintLtrTop
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Pacific Lists - Accounting";
                    else
                              Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Names in the News - Accounting";
                    endif
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please note that the following order carries a ";
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,Gdays;
                    Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew11,*ll," Day Payment Guarantee that is now past due.  Due date";
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
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"this form back to Names In The News.";
                    endif
                    add eightlpi to ROW                                                          
                    add eightlpi to ROW                                                          
                    if (OCompID = "P")
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Pacific Lists.";
                    else
                              Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew11,*ll,"Please take a minute to fill out the following information and fax this form back to Names In The News.";
                    endif
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

               return
.........................................................................................................................
PrintLtrTop
                                                  move "300" to row
                                                  add eightlpi to ROW
                                                  add eightlpi to ROW
                                                  add "600" to Row
               if             (officeflag = c0)
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,*boldon,"Guarantee Reminder",*boldoff;
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
               Else
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,*boldon,"Guarantee Reminder",*boldoff;
                                                                                          add eightlpi to ROW
                                                                                          Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew11,*ll,"File Copy";
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
                                                                                          add eightlpi to ROW
               endif
               return
.........................................................................................................................
.enters with maildate mm dd yy
.exits with day value for guaranty and  Due Date - both mm,dd,yy and "duedate"
age

               move           b4 to guartype
               if             (GUARCode = "1")
               add            "30" to juldays
               move           "30" to gdays
               Elseif         (GUARCode  = "2")
               add            "45" to juldays
               move           "45" to gdays
               Elseif             (GUARCode  = "3")
               add            "60" to juldays
               move           "60" to gdays
               Elseif             (GUARCode  = "4")
               add            "90" to juldays           .actually open ended.
               move           "90" to gdays
               Elseif             (GUARCode  = "5" or GuarCode = "6")
               add            "00" to juldays           .actually open ended.
               move           "00" to gdays
               Elseif             (GUARCode  = "7")
               add            "30" to juldays           .actually open ended.
               move           "30" to gdays
               Elseif             (GUARCode  = "8")
               add            "60" to juldays           .actually open ended.
               move           "60" to gdays
               Elseif             (GUARCode  = "9")
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
                    Pack       MailSubjct,"No NIN DIv Guaranty Letters - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "DavidHerrick@nincal.com",MailTo
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "No NIN Div Guaranty Letters",Mailbody
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
                    goto      Phase2
               goto           Phase1
               endif
.......................................................................................................
.io includes
              include         nordio.inc
              include         ninvio.inc
                              include   compio.inc
                              include   cntio.inc
              INclude         nmldio.inc
               include        hpio.inc
              include         ndatio.inc
              Include         NINVBRKGIO.inc
                                        include   NUSEIO.INC
              include         comlogic.inc


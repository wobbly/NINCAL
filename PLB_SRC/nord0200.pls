.
. PURPOSE - READS INPUT file (NPRINT/TEMP)
.           AND PRINTS ORDER FORMS.
.
PC       EQU       0
          INCLUDE   COMMON.INC
          INCLUDE   CONS.INC
          include   compdd.inc
          include   cntdd.inc
          include   ncntdd.inc
          INCLUDE   NORDDD.INC
          INCLUDE   NCRCDD.INC
          INCLUDE   NRTNDD.INC
          INCLUDE   NOWNDD.INC
          INCLUDE   NINVDD.INC
          INCLUDE   SHIPPING.INC
          include   nspIdd.inc
          include   nspedd.inc
          include   hp.inc
          INCLUDE   MEDIA.INC
          INCLUDE   nsmpdd.inc
          INCLUDE   NOFRDD.INC
          include   winapi.inc
          INCLUDE   NSEL2DD.INC
          INCLUDE   NSEL3DD.INC
          INCLUDE   NADDDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NREFDD.INC
          INCLUDE   NMODDD.INC
.begin patch 9.8
          include   Nspe3dd.inc
.end patch 9.8          

release   init    "9.8"    DLH  add Missing code for imported PL spec instruction reads
REldate   Init      "12 February 2009"                          .suppress writing to sample list file if hotprint
.release   init    "9.77"    DLH  Change for anomally on owner breaks - sometimes
.REldate   Init      "5 February 2009"                          .suppress writing to sample list file if hotprint
.release   init    "9.76"    DLH  tweak for eom office copies
.REldate   Init      "2 February 2009"                          .suppress writing to sample list file if hotprint
.release   init    "9.75"    DLH  tweak for eom office copies
.REldate   Init      "30 January 2009"
.release   init    "9.74"    JD  Patched for file creation sub.
.REldate   Init      "29 January 2009"
.release   init    "9.73"    DLH  .create a RERUN option for live run
.REldate   Init      "21 January 2009"
.release  init    "9.72"    DLH  Replace error prone .Dir method
.REldate  Init      "24 November 2008"
.release  init    "9.71"    DLH  Suppress TAG Print as we send fulfillment orders electronically
.REldate  Init      "04 September 2008"
.release  init    "9.70"    DLH  OPtion to Email Fulfillment copies
.REldate  Init      "xx March 2008"
.---Future add checking for hotprint of FUlfillment copy that is fulfulled by owner and process correctly
.currently nothing is processed in this interest.
.change processing to create temp file for each break to get count, samples etc
.creating one output PDF file will cover (if applicable) orders and Samples (if applicable)
.Print all to PDF if to be faxed use sendmail to send to facsys server
.if Email use sendmail
.if to be printed use c:\progra~1\pdf55\res\utilities\PrintPDf.exe  "file" "printer"
.All previous release details see archives version 9.62


MlrNameHold         dim       75
MlrMNameHold        dim       75
PackData  DataList
N6a       Form      6
CountIndex          Form      3
CountRecord    Record         (999)                                   ;artificial cap of 999 companies
CntRecComp     Form            6              01-06              Company #
CntRecCount         Form      3           07-09              Number of orders
          RecordEnd
cnt       dim       35
intrnet   dim       50                .print contact's internet address
pict1     pict
Laser     pfile
file2     file
INPUT     FILE      FIXED=696,STATIC=12
INPUT2    FILE      .Used for Fulfillment & Overlay Passes
OUTPUT    FILE
output2   FILE
dfile     file
.begin patch 9.7
FileCheck FIle
trapcount form      4
.end patch 9.7
SAVEFILE IFILE      KEYLEN=6,var=80
OFormBFlag          Init          "Y"
ANS       DIM       1
FILE      FORM      2         BRANCHING CONSTANT FOR I/O TRAPS
DATE      DIM       8         'MM/DD/YY'.
OFFEROUT DIM        11       FOR OUTPUT OF OFFER, SUPPRESSED IF NO OFFER SELCTD.
COUNTspool          FORM      5        TOTAL NUMBER OF RECORDS SPOOLED?
COUNTMlr  FORM      5        TOTAL NUMBER OF MLR INPUT READS
COUNTOwn  FORM      5        TOTAL NUMBER OF OWNER INPUT READS
COUNT3    FORM      5        TOTAL NUMBER OF FULFILMENT INPUT READS
COUNT4    FORM      5        TOTAL NUMBER OF OFFICE INPUT READS
COUNT5    FORM      5        TOTAL NUMBER OF OVERLAY INPUT READS
mlrcnt    form      3        .counts number of orders for a particular brk/mlr.
owncnt    form      3        .counts number of orders for a particular owner.
ownscnt   form      3        .counts number of samples for a particular owner.
NFIELD23 FORM       3.2                  (NUMERIC WORK FIELD)
V1        FORM      2
smpflag   form      1      *1=there is a file *2= file with data
hotflag   form      1      *1=normal print 2= special print.
FORMFLAG FORM       1      1=MAILER, 2=OWNER, 3=FULFILLMENT, 4=OFFICE, 5=overlay/zip screen
FLAGPAID DIM        2      *USED TO FLAG PRE-PAID ORDERS.
PDFFlag   form      1         .Allows PDF Option
REVTXT    INIT      "Revised: "
CANTXT    INIT      "**CANCELLED** : "
BILDTXT   INIT      "**Billed Order**"
REVDATA   DIM       30
BILDDATA DIM        16
REVTYP    DIM       3
.begin patch 9.7
SmpFile   Dim       100
.end patch 9.7

EXCHANGE DIM        15         *USED FOR ORDER PRINT
TEST      DIM       15         *USED FOR ORDER PRINT
SAMPLE    DIM       26        *USED FOR ORDER PRINT
F3        DIM       3         *USED FOR ORDER PRINT
F2        DIM       2         *USED FOR ORDER PRINT
ENTIRE    DIM       1        *USED FOR ORDER PRINT
CORTN     DIM       3         *USED FOR ORDER PRINT
CONT      DIM       23        *USED FOR ORDER PRINT
CONT1     DIM       20        *USED FOR ORDER PRINT
CONTDTE   DIM       10         *USED FOR ORDER PRINT
CONTQTY   DIM       11         *USED FOR ORDER PRINT
QTYMSK    INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT    DIM       11         *USED FOR ORDER PRINT
QTYNUM    FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
MEDMEMO   DIM       25        *USED FOR ORDER PRINT, ON MAG TAPE.
COMSLCT   DIM       25        *USED FOR ORDER PRINT, COMSELECT ORDERS.
REPRT     DIM       15        *USED FOR ORDER PRINT, REPRINTED ORDERS.
.                            *AND CANCELLED ORDERS, REPRINT IMPLIED.
LROUT     DIM       6         *USED FOR ORDER & LABEL PRINT.
LRMASK    INIT      "ZZZZZ9"
LRNUM     FORM      6
COUNTIn   FORM      4         RECORDS IN
PRICECK   DIM       5
.PHONE DISPLAY VAR'S
LP        DIM       1
RP        DIM       1
EXT       DIM       3
ARCD      DIM       3
PHONE     DIM       4
FAX1      DIM       13
rtEXT     DIM       3       .return-to
rtARCD    DIM       3       .return-to
rtPHONE   DIM       4       .return-to
MEDTYPE   DIM       1
faxname   dim       45
faxtele   dim       10
faxattn   dim       45
save      dim       47
savecoms dim        1
ofosave   dim       2
rprtcode init       "RXQ"
COPY      FORM      1
careof    dim       3
.holdmlr  dim       4
HoldBrkMlr          dim       8                   .used for Broker/Mailer break
holdown   dim       4                   .used for owner break
smpown    dim       4
fhandle   dim       8                   .use to create Print files.
holdcom   dim       1                   .used for list owner ccto break.
holdccto dim        6                    .used for list owner ccto break.
holdcccnt dim       45                   .used for list owner ccto break. (Contact Name)
holdcccmp dim       55                   .used for list owner ccto break. (Company Name)
ovrTEL1   init      "7732901789"
ovrTEL2   init      "2033536661"
ovrTEL3   init      "6124816363"
ovroct1   form      2                number of orders
ovroct2   form      2                number of orders
ovroct3   form      2                number of orders
ovrnum    form      2                table index
faxflag   form      1                    .1=no, 2=yes.
FaxNumFlag form     1                    is set if Mailer fax number is valid.
ovr1      init      "C"
ovr2      init      "L"
ovr3      init      "I"
TRIPLEX   INIT      " "
attchlst dim        1000
LPTCNT    FORM      4                 .LENGTH OF ATTCHLST
spoolfl2 DIM        40                   .order  SPOOL FILEs
TIME      DIM       8
MO        DIM       2       MONTH
YR        DIM       2
LONGDIST DIM        1
DCX       INIT      ".TIF"
malchow   INIT      "3392-4427-4517-4814-4840"    Adams & HUssey was Malchow list owner numbers.
str45a    dim       45
lstmgt    dim       2
Holdlstmgt          dim       2
attn      dim       6
nfaxtel   dim       15
nfaxtel2 dim        15
FirstFlag init      "Y"
.FirstFlag1 init    "Y"
PrintFlag form      "0"
LastFlag form       1
nosmpl    dim       1
DCX2      dim       30
DCXFile   dim       120
SPOOLF    dim       120
hotprt    dim       9                  if a hot print prnt "Hot Print" by typist inits on office copy
dcxpath   init      "\\nins1\e\data\samples\"      ."
FilePath init       "C:\WORK\"                              ."
faxkount form  3
SMPArray dim        12(50)
SMPIndex form       2
rtphmask dim        14
line1     dim       55
line2     dim       55
line3     dim       55
line4     dim       55
line5     dim       55
line6     dim       55
line7     dim      55
line8   dim         55
line9     dim       55
line10    dim       55
line11    dim       55
line12    dim       55
line13    dim       55
line14    dim       55
hotkey    dim       6
tipe      dim       1
rptcan    dim       1
.Create fonts to be used
font1               font
Font4               font
font5               font
fontO8              font
fontO9              font
fontO9I             font
fontO10             font
fontO10n  font
fontO10B  font
fontO12B  font
fontO14             font
FontO14B  font
FontO14BI font
FontO18I  font
FontO7              font
FontO7dot5          font
FontO7dot5B         font
FontO7dot5I         font
FontO7dot5BI        font
FontO18B  font
FontO18BI font
PRTPG24B  font
PRTPG24I  font
PRTPG10             font
Font08I   font
Font08BI font
font7     font
font8     font
font9     font
.begin patch 9.73   .allow restart with file move etc
RerunFlag Dim       1                .='Y' if rerun
.end patch 9.73   .allow restart with file move etc

NINLogo   PICT
.Blockout PICT
.Blockout1          PICT

sevenfive form      "7.5"
externalmode        integer 1
DimPtr              dim       ^

NFULNUM             DIM       6
NFULCOMP  DIM       55
NFULCNT             DIM       45
NFULFAX             DIM       10
.begin patch 9.7 
NFULEMAIL Dim       50
EmailFlag Dim       1         .current one is emailable
EmailFlag1          Dim       1         .last one is emailable
.end patch 9.7 

Start
dlFiles   datalist
DLresult  form 9
DLndx     form 9
dmFileName          dim 80
CoverName DIm       25
PdfFName  Dim       25

x       plform  report
        formload X

**************************************************
* PROGRAM MAIN.
* *************
          create    font1,"Times New Roman",size=14,bold
          create    fontO8,"Times New Roman",size=8
          create    font5,"Times New Roman",size=11
          Create    fontO9,"Times New Roman",size=9
          create    fontO9I,"Times New Roman",size=9,Italic
          create    fontO10,"Times New Roman",size=10
          create    fontO10n,"Courier New",size=11
          create    fontO10B,"Times New Roman",size=10,Bold
          create    fontO12B,"Times New Roman",size=12,Bold
          create    fontO14,"Times New Roman",size=14
          create    fontO14B,"Times New Roman",size=14,Bold
          create    fontO14BI,"Times New Roman",size=14,Bold,Italic
          create    fontO18I,"Times New Roman",size=18,Italic
          create    fontO7dot5,"Times New Roman",size=sevenfive
          create    fontO7dot5I,"Times New Roman",size=sevenfive,Italic
          create    fontO7dot5b,"Times New Roman",size=sevenfive,Bold
          create    fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
          create    fontO18BI,"Times New Roman",size=18,Bold,Italic
.
          create    PRTpg24B,"Times New Roman",size=24,Bold
          create    PRTpg24I,"Times New Roman",size=24,Italic
          create    PRTpg10,"Times New Roman",size=10
          create    font08I,"Times New Roman",size=8,Italic
          create    font08bI,"Times New Roman",size=8,Bold,Italic
.Create work var
          create    PackData=1:1:1:1

          move      "750",column
          move      "1750",column1
          move      "3000",column2
          create    font7,"Helvetica",size=14,bold
          create    font8,"Helvetica",size=14,italic
          create    font9,"Arial",size=12
          if (externalMode)
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold

                    CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
          else
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
                    CREATE    NINLogo=3:13:30:50:
                              "\\nts0\c\netutils\NIN logo black outline.jpg"
          endif
.                   CREATE    Blockout=3:20:30:50:
.                             "\\nts0\c\netutilms\blockout.tif"
.                   CREATE    Blockout1=3:20:30:50:
.                             "\\nts0\c\netutils\blockout2.tif"
.

.Find out system information

          CALL GETWINVER
.
          clock     time,time
          clock     timestamp,timestamp
.
          rep       lowup,PROGRAM
.begin patch 9.73   .allow restart with file move etc
          rep       lowup,Comment
          move      No,RerunFlag              .default
.end patch 9.73   .allow restart with file move etc

          match     "NORD0200",PROGRAM   .case sensitive
          if not equal
                    move      "NORD0200",PROGRAM
                    move      "ORDER PRINT  ",STITLE
                    move      C0,copy
                    move      C1,hotflag
.begin patch 9.73   .allow restart with file move etc
.         else
          Elseif    (Comment <> "RERUN")
.end patch 9.73   .allow restart with file move etc
                    unpack    inpname,str6,str1,str3
                    move      str6,NORDFLD
                    move      C2,hotflag
                    move      C0,formflag
                    move      C1,NORDPATH
                    move      C0,NCRCFLAG
                    rep       "M1L2F3O4A0",str1         .get request
.begin patch - new code uses formflag during hot prints
                    Move      Str1,Formflag     
.end patch

                    rep       "10213243",str1      .set copy to match

                    move      str1,copy
                    move      "HOT ORDER PRINT ",STITLE
                    move      COMMENT,TRIPLEX
                    if (FUNC = "2")
                              move      C1,PDFFlag
                              if (!externalmode)
.
                                        call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
                                                  "Parameters":
                                                  "ProcessPDF":
                                                  "\\nts0\c\apps\plb\code\pdftest.bat":
                                                  result
                                        if (result = C0)
.Prepare Flag file
                                                  prep      tempfile,"c:\progra~1\pdf995\flag.dat"
                                                  write     tempfile,SEQ;"flag set"
                                                  close     tempfile
                                        endif
                              endif
                    else
                              move      C0,PDFFlag
                    endif
.begin patch 9.73   .allow restart with file move etc
          Else       
                    move      "NORD0002",PROGRAM
                    move      "ORDER PRINT - RERUN ",STITLE
                    move      Yes,RerunFlag
                    move      C0,copy
                    move      C1,hotflag
.end patch 9.73   .allow restart with file move etc
          endif
          move      C0,howmany
          move      "Names In The News",COMPNME
          call      PAINT
          move      "Exit",PF5
          trap      END if F5
          Trap      ObjError Giving error if Object
          call      FUNCDISP
. OPEN FILES.
prepit
          display   *P1:24,"OPENING FILES";
          move      C4,FILE
          move      C1,NBRKPATH      .SET ACCESS ISI.
          move      C5,FILE
CLOCK
          clock     DATE,DATE
          clock     DATE,today
          unpack    DATE,MM,DD,YY
          rep       " 0",DD
          pack      DATE FROM MM,SLASH,DD,SLASH,YY
          add       C1,FILE
          add       C1,FILE
          clear     tipe
          scan      "FAX",prtname
          if equal
                    move      "F",tipe
          else
                    move      "H",tipe
          endif
          if (hotflag = C2)
                    if (!externalmode)
                              pack      taskname,NTWKPATH7,"HOTORDERS"
                              Open      SAVEFILE,Taskname
                              clear     hotkey
                              move      NORDFLD,hotkey
                              filepi    4;savefile
                              read      savefile,HOTKEY;;
                              if over
                                        write     SAVEFILE;"******",today
                                        write     SAVEFILE,NORDFLD;NORDFLD,tipe,str3
                              endif
                    endif
          endif
          reset     prtname
...........................................................................
...........................................................................
.begin patch 9.73   .allow restart with file move etc
.          if (hotflag = 1)    .Full Run
          if (hotflag = 1 & RerunFlag <> "Y")     .Full Run
.end patch 9.73   .allow restart with file move etc
.lets save previous files
.first kill from del folder  \\nts0\d\data\orders\delete
.begin patch 9.72
          clear     Mailbody
          FIndDIr   "\\nts0\d\data\orders\delete\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "\\nts0\d\data\orders\delete\",DmFIleName  ."comment :)
                    FindFIle  Taskname
                              if        Zero          .file is there
                              erase Taskname                          
                              endif
                    endif
          repeat
          endif
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nts0\d\data\orders\delete\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      taskname from "\\nts0\d\data\orders\delete\",DmFIleName              ."comment :)
.         
.         FINDFILE Taskname,WRITE=Str25
.                              erase          taskname
.         repeat
.         Destroy   DLFIles
.end patch 9.72
.second move all from old to del   \\nts0\d\data\orders\old
.begin patch 9.72
          clear     Mailbody
          FIndDIr   "\\nts0\d\data\orders\old\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      str55 from "\\nts0\d\data\orders\old\",DmFIleName           ."comment :)
                    pack      taskname from "\\nts0\d\data\orders\delete\",DmFIleName  ."comment :)
                    FindFIle  Str55
                              if        zero                .file is there
.                              Rename    Str55,Taskname                          
                              endif
                    endif
          repeat
          endif
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nts0\d\data\orders\old\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      str55 from "\\nts0\d\data\orders\old\",DmFIleName
.         pack      taskname from "\\nts0\d\data\orders\delete\",DmFIlename
.         
.               rename          Str55,taskname
.         repeat
.         Destroy   DLFIles
.end patch 9.72
.last move from sent to old  \\nts0\d\data\orders\sent
.begin patch 9.72
          clear     Mailbody
          FIndDIr   "\\nts0\d\data\orders\sent\*.*",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      str55 from "\\nts0\d\data\orders\Sent\",DmFIleName     ."comment :)
                    pack      taskname from "\\nts0\d\data\orders\old\",DmFIleName    ."comment :)
                    FindFIle  Str55
                              if        zero                .file is there
.                              Rename    Str55,Taskname                          
                              endif
                    endif
          repeat
          endif

.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="\\nts0\d\data\Orders\sent\*.*":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      STr55 from "\\nts0\d\data\orders\sent\",DmFIleName
.         pack      taskname from "\\nts0\d\data\orders\old\",DmFIlename
.         
.         FINDFILE Taskname,WRITE=Str25
.               rename        str55,Taskname
.         repeat
.         Destroy   DLFIles
.end patch 9.72
.NPRINT.TMP used for Mailer run
                    move      "Prepare Orders.fax",Location
                    pack      KeyLocation," "
                    trap      IOMssg giving Error if IO

.                   trap      IO GIVING ERROR IF IO
                    prepare   output2,"\\nins1\e\data\orders.fax"
                    WRITE     output2,SEQ;B1,today," -Company's Receiving Order FAXES !!!",b1,time
                    Erase     "\\nins1\e\data\Nprint.ful"
                    Erase     "\\nins1\e\data\Nprint.own"
                    Erase     "\\nins1\e\data\Nprint.brk"
                    Erase     "\\nins1\e\data\Nprint.lm"
                    Erase     "\\nins1\e\data\Nprint.tmp"
.file for Owner pass
                    Display   *p1:22,*el,"sorting Nprint.dat to Nprint.own"
                    pack      taskname,"\\nins1\e\data\NPRINT.lrb,\\nins1\e\data\NPRINT.own;22-25,16-21,7-12"
                    sort      taskname
.file for Fulfillment pass
                    Display   *p1:22,*el,"sorting Nprint.own to Nprint.Ful"
                    pack      taskname,"\\nins1\e\data\NPRINT.lrb,\\nins1\e\data\NPRINT.FUL;329-334"
                    sort      taskname
.file for Office pass if typist initials indicate it is an Auto billing order
                    Display   *p1:22,*el,"sorting Nprint.dat to Nprint.LR"
                    pack      taskname,"\\nins1\e\data\NPRINT.lrb,\\nins1\e\data\NPRINT.LR;290-292,7-12,S=#"290='AMB'|290='ARB'#""
                    sort      taskname
.file - Mailer/Broker/Consultant copy
.non LM first
                    Display   *p1:22,*el,"sorting Nprint.dat to Nprint.Brk"
                    Pack      Taskname from "\\nins1\e\data\nprint.lrb \\nins1\e\data\nprint.BRK -303-306,307-309,3-6,7-12, S=#"196<>'06'&196<>'19'&196<>'27'&196<>'28'#""
                    Sort      Taskname
.Then LM 
                    Display   *p1:22,*el,"sorting Nprint.dat to Nprint.LM"
                    Pack      Taskname from "\\nins1\e\data\nprint.lrb \\nins1\e\data\nprint.LM -303-306,307-309,3-6,7-12, S=#"196='06'|196='19'|196='27'|196='28'#""
                    Sort      Taskname
.put them together
                    Display   *p1:22,*el,"Combining nprint.brk & nrpint.lm into nprint.tmp"
                    Prepare   output,"\\nins1\e\data\nprint.Tmp",exclusive
                    OPen      INput,"\\nins1\e\data\nprint.brk"
                    move      "Prepare Nprint.tmp",Location
                    pack      KeyLocation,"SEQ "
                    trap      IOMssg giving Error if IO

                    Loop
                    read      input,seq;ordvars
                    until over
                    write     output,seq;ordvars
                    repeat

                    OPen      INput,"\\nins1\e\data\nprint.LM"

                    Loop
                    read      input,seq;ordvars
                    until over
                    write     output,seq;ordvars
                    repeat

                    Weof      Output,Seq
                    Close     Output

                    display   *P1:22,*EL,"files Prepped";
                    display   *P1:22,*EL,"Opening Nprint.tmp";
                    open      INPUT,"\\nins1\e\data\NPRINT.TMP"
                    display   *P1:22,*EL,"Opened Nprint.tmp";
                    display   *P1:22,*EL,"         ";
                    display   *P1:24,*EL;
                    move      "Nprint.tmp",Location
                    pack      KeyLocation,"SEq "
                    trap      IOMssg giving Error if IO

                    for       formflag,C1,C5
.For testing, leave it in.
.                             until (formflag > 2)
MainLoop
                    loop
                    move      C1,FILE
.                   add       C1,COUNTIn
.         call      Debug
.                   display   *P10:12,*EL,"COUNT READ ",COUNTIN,B1,OLRN
                    read      INPUT,SEQ;ORDVARS
                    If        OVer                
                    
.new code will be send previous output file to appropiate device
.FOLLOWING CODE INCLUDES BREAK, WHICH WILL ALLOW MULTIPLE PASSES!!!!
.Print last records
                              move      YES,FirstFlag
                              close     input
                              trap      IOMssg GIVING ERROR IF IO
                                        if (formflag = 1)                      .mailer copies DOne
                                        if        (PdfFName <> "")
                                        call      PrepSend
                                        call      senditout
                                        endif
                                        display   *p1:24,*el,"broker/Mailer DONE"
.Prep for Owner run
                                        display   *P1:22,*EL,"Opening Nprint.Own";
                                        move      "Nprint.Own",Location
                                        pack      KeyLocation,"SEq "
                                        trap      IOMssg giving Error if IO
                                        open      INPUT,"\\nins1\e\data\NPRINT.OWN"
                                        display   *P1:22,*EL,"Opened Nprint.Own";
                                        Clear     PdfFName
                                                  call      debug
                                                  elseif (formflag = 2)                      .owner Done                                                                  
                                        if        (PdfFName <> "")
                                        call      PrepSend
                                        Call      Samples
                                        call      senditout
                                        endif
                                        display   *p1:24,*el,"Owner DONE"
.Prep for Fulfillment run     
                                        display   *P1:22,*EL,"Opening Nprint.Ful";
                                        move      "Nprint.ful",Location
                                        pack      KeyLocation,"SEq "
                                        trap      IOMssg giving Error if IO
                                        open      INPUT,"\\nins1\e\data\NPRINT.FUL"
                                        display   *P1:22,*EL,"Opened Nprint.Ful";
                                        Clear     PdfFName

                                        elseif (formflag = 3)                       .Fullfillment
                                        if        (PdfFName <> "")
.send will create cover page if appropiate
                                        fill      B1,faxname
                                        move      holdcccmp,faxname
                                        call      Trim using holdcccnt
                                                  if (holdcccnt = "")
                                                  move      "Order Fulfillment",faxattn
                                                  else
                                                  move      holdcccnt,faxattn
                                                  endif
.
                                        call      PrepSend
                                        call      senditout
                                        endif     
.Prep for Office run, which includes creation of file for Overlay run
                                        display   *P1:22,*EL,"Opening Nprint.LR";
                                        move      "Nprint.lr",Location
                                        pack      KeyLocation,"SEq "
                                        trap      IOMssg giving Error if IO
                                        open      INPUT,"\\nins1\e\data\NPRINT.LR"
                                        display   *P1:22,*EL,"Opened Nprint.LR";
.Writing to above file happens in Office Print loop, so I will close it later
                                        Clear     PdfFName
                                        elseif (formflag = 4)                       .office
                                        if        (PdfFName <> "")
                                        call      PrepSend
                                        call      senditout
                                        endif
.Prep for Overlay run
                                        pack      APIFileName,"\\nins1\e\data\NPRINT.tmp",hexzero
                                        call      FindFirstFile
                                                  if (APIResult <> 0 & APIResult <> hexeight)
                                                  pack      taskname,"\\nins1\e\data\NPRINTc.dat,\\nins1\e\data\NPRINT.OVR;211-211"
                                                  sort      taskname
                                                  display   *P1:22,*EL,"Opening Nprint.Ovr";
                                                  open      INPUT,"\\nins1\e\data\NPRINT.OVR"
                                                  display   *P1:22,*EL,"Opened Nprint.Ovr";
                                        
                                                  else
                                        
                                                  erase     "\\nins1\e\data\NPRINT.ovr"
                                                  prepare   INPUT,"\\nins1\e\data\NPRINT.ovr"
                                                  endif
                                                  display   *P1:22,*EL,"        ";
                                        move      "Nprint.ovr",Location
                                        pack      KeyLocation,"SEq "
                                        trap      IOMssg giving Error if IO

                                        Clear     PdfFName
                                        elseif (formflag = 5)                        .overlay Done
                                        if        (PdfFName <> "")
                                        call      PrepSend
                                        call      senditout
                                        endif
                                        endif
                              
                                        call      PrtCloseFile

                                        if        (PdfFName <> "")
                                        call      PrepSend
                                        call      senditout
                                        endif

                                        BREAK
                              endif
                    
.MainRun  - was not over lets process   -------------------<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          add       C1,COUNTIn
          display   *P10:12,*EL,"COUNT READ ",COUNTIN,B1,OLRN
          call      Trim using OLRN
                    if (OLRN <> "")
                    reset     rprtcode
                    scan      OSTAT,rprtcode
                              if not equal
                                        if (formflag <> "2")
                                        call      CreateRecord
.27Jun97 DLH if List Manager is Malchow & Pass = owner copy then suppress
                                        Else
                                        reset     Malchow             ."3392-4427-4517-4814-4840"    Malchow list owner numbers.
                                        scan      OLON,Malchow
                                                  if not equal
                                                  call      CreateRecord
                                                  endif
                                        endif
                              endif
                    endif
                    repeat
READO
                    move      C0,countIn
                    repeat
........................................................................HOT PRINT                   
          elseif (hotflag = 2)          .Individual HotPrints
.Following label is strictly referential.  No longer used in code.
.readlive
                    rep       zfill,NORDFLD
                    move      C1,NORDPATH
                    move      "readlive-NORDKEY",Location
                    pack      KeyLocation,"Key: ",NORDFLD
                    call      NORDKEY
                    move      str3,ODOWJ
                    move      OLRN,NSPEFLD
                    rep       zfill,NSPEFLD
                    move      "readlive-NSPEKEY",Location
                    pack      KeyLocation,"Key: ",NSPEFLD
.begin patch 9.8
.                    call      NSPEKEY
                    Type      nspefld                                 ....PL imported records are alpha numeric
                    if        equal
                    call      NSPEKEY
                    else
                    clear     DESC001
                    clear     DESC002
                    pack      NSPE3FLD,OLRN
                    rep       zfill,NSPE3FLD
                    move      C3,NSPE3LOCK
                    move      "O.LoadScreens-NSPE3KEY",Location
                    pack      KeyLocation,"Key: ",NSPE3FLD
                    call      NSPE3KEY
                    call      Trim using DESC005
                    move      desc005,desc002
                    endif
.end patch 9.8
.Clean up before hand
.                   pack      str45,"C:\WORK\",prtname
.                   erase     str45
.                   pack      str45,"C:\WORK\Faxfile.prn"
.                   erase     str45
.
.                   call      debug
                    call      CreateRecord
                    call      PrepPrtPage
.                   call      CreateRecord
...mmmm send it ?             
                    call      PrtCloseFile
                    call      debug
.Begin patch 9.7 
.prep for brk/mlr if appropiate
          if        (formflag = 1)
.begin DH 08OCt
          call      SetPrintFlag
.end DH 08OCt
          move      Emailflag,Emailflag1
          endif
          call      Trim using OFULLFIL
          IF        (OFULLFIL = "" & formflag = 3)              .     
          alert     caution,"You are trying to hotprint & there is no Fulfillment ##",result
          shutdown

          Elseif    (OFULLFIL <> "" & formflag = 3)              .
          call      GetFullFIll
          move      NFULNUM,holdccto
          move      NFULNUM,FHandle
          move      NFULCNT,holdcccnt
          move      NFULCOMP,holdcccmp
          move      EmailFlag,EmailFlag1
          move      holdcccmp,faxname
          call      Trim using holdcccnt
                    if (holdcccnt = "")
                    move      "Order Fulfillment",faxattn
                    else
                    move      holdcccnt,faxattn
                    endif
                    
          endif
.end patch 9.7 
.prep for owner if appropiate
          if        (formflag = 2)
          count     n2,faxtele
                    if        (n2 > 6)
                    move      c2,faxflag
                    endif
.                   call      Debug
          endif
                    if        (PDfFName <> "")
.
                    call      PrepSend
                    endif
.prep for fulfillment if appropiate
                    if        (func <> "1")
                    call      senditout
                    endif
                    shutdown
.need to know if faxing or print or emailing

.begin patch 9.7
.                   if (PDFFlag = 1)
.end patch 9.7
                              if (!externalmode)
.Give the email a chance of rendering itself before updating the INI file.
                                        pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
                                        loop
                                                  call      FindFirstFile
                                                  until (APIResult = 0 | APIResult = hexeight)
                                                  pause     "1"
                                        repeat
                                        pause     "2"
                              endif
.begin patch 9.7
.                   else
.                             pack      APIFileName,"C:\WORK\Faxfile.prn",hexzero
.                             call      FindFirstFile
.                             if (APIResult <> 0 & APIResult <> hexeight)
.                                       pack      str45,"C:\WORK\",prtname
.                                       erase     str45
.                                       rename   "C:\WORK\Faxfile.prn",str45
.                             endif
.                   endif
.end patch 9.7
          endif
.PROGRAM WILL HAVE TO END HERE
..................................................Regular run
          if (hotflag = 1)    .Full Run
                    write     output2,seq;b1,"Total Company's Faxed Out ",faxkount
                    Weof      Output2,seq
                    close     output2
                    open      dfile,"\\nins1\e\data\orders.FAX"
                    Clear     MailBody
                    Append    Today,MailBody
                    Append    " -Company's Receiving Order FAXES !!! ",MailBOdy
                    append    Time,mailbody
                    append    CRLF,MailBOdy                 
                    loop
                              read dfile,seq;str55
                              until over
                              append    STR55,mailbody
                              append    CRLF,MailBOdy                 
                    repeat

                    
                    Reset     Mailbody
                    move      "Please verify that total matches Fax queue",Mailsubjct
                    pack      MailTo,"JamieMittone@nincal.com"
.                   pack      Mailfrom,"dherric@nincal.com"
                    pack      Mailfrom,"CReques@nincal.com"
.                   pack      Mailto,"JDuenas@nincal.com,dherric@nincal.com"
                    Clear     MailAttach
                    call      SendMail

.Cleanup Owner Sample Files
          call debug
.Turned off, cleaning up in winbatch.
..
.          create dlFiles=1:10:1:10,visible=0
.          dlFiles.Dir giving DLresult using *Filespec="\\nins1\e\data\Nord*.smp":
.                                *Flags=0x0000
.          for DLndx from 0 to DLresult
.          dlFiles.GetText giving dmFileName using *Index=DLndx
.          clear     taskname
.          pack      Taskname from "\\nins1\e\data\",DmFIleName                            ."
.          FINDFILE  Taskname,WRITE=Str25
.                    erase               taskname
.          repeat
.          Destroy   DLFIles
.
          endif
.          if (externalmode)
.                    return
.          endif

          shutdown

CreateRecord
............................................................................................................
.begin patch 9.77
.check for breaks first!!!!!!!!!!!!!!!!!!!!!!!!!!


..Get Offer
..EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
.          bump      OODNUM,4
.          pack      NOFRFLD,OMLRNUM,OODNUM
.          reset     OODNUM
.          move      "C.Record-NOFRKEY",Location
.          pack      KeyLocation,"Key: ",NOFRFLD
.          call      NOFRKEY
..
.          move      "10",FILE
.          clear     ARCD
.          clear     EXT
.          clear     PHONE
.          clear     LP
.          clear     RP
.          clear     DASH
.          move      OLON,NOWNFLD
.          rep       zfill,NOWNFLD
.          move      "C.Record-NOWNKEY",Location
.          pack      KeyLocation,"Key: ",NOWNFLD
.          call      NOWNKEY
..Begin patch 9.7 
.          call      Trim using OFULLFIL
.          if        (OFULLFIL <> "" & formflag <> 3)              .test DH if it is formflag 3 take care of elsewhere??
..         if        (formflag <> 3)
.          call      GetFullFIll
.          endif
..end patch 9.7 
.          match     "0000000000",OWNTELE                    *PHONE NUMBER?
.          call      PHONE if not equal            *YES
.          clear     BILDDATA
.          move      OLRN,NINVFLD
.          move      C1,NINVPATH
.          move      "C.Record-NINVTST",Location
.          pack      KeyLocation,"Key: ",NINVFLD
.          call      NINVTST
.          if not over
.                    move      BILDTXT,BILDDATA
.          endif
.          clear     REVDATA
.          clear     REVTYP
.          pack      NCRCFLD,OLRN
.          move      "C.Record-NCRCKEY",Location
.          pack      KeyLocation,"Key: ",NCRCFLD
.          call      NCRCKEY
.
.          loop
.                    until over
.                    until (NCRCFLD <> NCRCKEY)
.                    move      NCRCTYP,REVTYP
.                    if (NCRCCODE = "C")
.                              pack      REVDATA,CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
.                              break
.                    else
.                              pack      REVDATA,REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
.                    endif
.                    move      "C.Record-NCRCKS",Location
.                    call      NCRCKS
.          repeat
.
.          sub       FILE,FILE
.end 05 Feb 2009
.end patch 9.77
..
. PRINT VARIABLES FROM ACCESSED RECORD
.
PROCESS
.IF REGULAR CYCLE USE FORMFLAG . ELSE . USES COPY.
          branch    FORMFLAG,PRTMLRBX,PRTOWNBX,PRTFULBX,PRTOFFBX,PRTOVRLY
          branch    COPY,PRTOWNBX,PRTFULBX,PRTOFFBX
PRTMLRBX
          add       C1,COUNTMlr
          display   *P1:13,*EL,"Brk/Mlr COUNT ",COUNTMlr,B1,OLRN,b1,obrknum,b1,omlrnum,b1
          move      C0,N10
          call      Trim using OQTY
          move      OQTY,N10
          if (N10 = C0)
                    if (hotflag = 2)
                    goto      PRTOFFBX  .HotPrints automatically have Office Copy printed
                    else
                    return
                    endif
          endif
          reset     RUNCODES
          scan      OLNUM,RUNCODES
          if equal
                    return
          endif
.if not hotprint And Mailer preference for hardcopy is not True & its not a list management order = return
.begin patch 9.7
          clear     CompEmail
          Clear     CnctEmail
.end patch 9.7
                    pack      MKEY,OMLRNUM,Z3
                    rep       zfill,MKEY
                    move      "SetPrintFlag-NMLRKEY",Location
                    pack      KeyLocation,"Key: ",MKEY
                    call      NMLRKEY             .make sure we have last brker info
.begin patch 9.7
                    call      Trim with cnctemail
                    if        (cnctemail = "")
                    move      CompEmail,NFulEmail
                    else
                    move      CnctEmail,NFulEmail
                    endif
                    if        (formflag = 1 & Nfulemail <> "")
                    move      Yes,Emailflag
                    else
                    move      No,Emailflag
                    endif
                    
.end patch 9.7

          Move      LstMGt,HoldLstMgt
          clear     lstmgt
          pack      lstmgt,OSALES10,OSALES
          rep       zfill,lstmgt
          
          if        (Hotflag = c1 & COMPConFlag <> "T" & LStmgt <> "06"  & LStmgt <> "19"  & LStmgt <> "27"  & LStmgt <> "28"  )
          return
          endif
          
mlronly
          if (hotflag = C2)   .HotPrint
                    move      YES,FirstFlag
          else                .Fullrun
.<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.begin change of logic if 1st record we want to get broker and mailer info start a counter of number of orders
.and establish if we are printing, faxing, emailing the confirmations - if faxing or emailing we will create a cover sheet
.with number of orders and Sent via info
.<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

                    pack      str8,OBrknum,OMlrnum
                    if (str8 <> HoldBrkMlr)
                              move      YES,FirstFlag
                              if (HoldBrkMlr <> "")
                                        call      BRKBREAK
                              endif
.Establishes flags/settings for current record
                              call      SetPrintFlag
                              pack      HoldBrkMlr from OBrknum,OMlrnum
.
                              call      PrepPRtPage
                    endif
                    
.                   if (HOLDMLR <> "" AND HOLDMLR <> "    " AND HOLDMLR <> OMLRNUM)
..FaxNumFlag AND PrintFlag are only set if SetPrintFlag does not find a valid Broker/Broker Fax Number.
..FaxNumFlag is set if Mailer fax number is valid.
..PrintFlag is set if neither a Broker Fax number nor a Mailer Fax Number is found and document is to be printed.
..In either case, if the Mailer number has changed, you would need to look for a valid fax number once again.

                    add       C1,mlrCNT
          endif
mlronlyend
          move      YES,OformBFlag
          call      prtordfrm
          call      prtmlrboxGui
          call      process1
          return

SetPrintFlag
          display   *p1:24,*el,"Set Print Flag"
          move      "                                             ",faxattn
          move      "                                             ",faxname
          clear     faxattn
          clear     faxname
.
          pack      NBRKFLD,OBRKNUM,Z3
          rep       zfill,NBRKFLD
          move      "SetPrintFlag-NBRKKEY",Location
          pack      KeyLocation,"Key: ",NBRKFLD
          call      NBRKKEY             .make sure we have last brker info
.......................
          move      C0,FaxNumFlag
          move      C0,N10
          move      BRFAX,N10
          call      debug     
          if (N10 = C0)
                    pack      MKEY,OMLRNUM,Z3
                    rep       zfill,MKEY
                    move      "SetPrintFlag-NMLRKEY",Location
                    pack      KeyLocation,"Key: ",MKEY
                    call      NMLRKEY             .make sure we have last brker info

                    move      C0,N10
                    move      MFAX,N10
                    if (N10 = C0)
                              move      C1,PrintFlag
                              move      C1,faxflag
                    elseif (MFaxOFlag <> "T")
                              move      C1,PrintFlag
                              move      C1,faxflag
.begin patch 9.74
                              pack      fhandle,OBRKNUM,OMlrNUM
                              REp       Zfill,Fhandle
                              move      MCOMP,faxname
.end patch 9.7
                    else
                              pack      fhandle,OBRKNUM,OMlrNUM
                              pack      str2," x"
                              rep       str2,fhandle
                              move      MCOMP,faxname
                              move      MFAX,FAXTELE
                              move      MCONTCT,faxattn
.
                              move      C1,FaxNumFlag
                              move      C2,faxflag          .do fax.
                    endif
          elseif (BrFaxOFlag <> "T")
                    move      C1,PrintFlag
                    move      C1,faxflag
.begin patch 9.74
                    pack      fhandle,OBRKNUM,OMlrNUM
                    REp       Zfill,Fhandle
                    move      BRCOMP,faxname
.end patch  9.74
          else
                    pack      fhandle,OBRKNUM,OMlrNUM
.                   move      BRKNUM,fhandle
                    move      BRCOMP,faxname
                    move      BRFAX,FAXTELE
                    move      BRCNTCT,faxattn
.
                    move      C2,faxflag          .do fax.
          endif
.begin patch 9.7
                    call      Trim with BREmail
                    if        (formflag = 1 & BREmail <> "")
                    move      Yes,Emailflag
                    else
                    move      No,Emailflag
                    endif
                    
.end patch 9.7

          return

BRKBREAK
.all breaks ---- ### 1 check and see if we have one to send/print
.then prep new output file
.MMM will this work - if remember to clear pdfname when we change fomr##
          if        (PdfFName <> "")
.send will create cover page if appropiate
          call      PrepSend
          call      senditout
          endif
          
          display   *p1:24,*el,"broker break"
.always call prepPrtPage      
.call cover page if appropiate
.
          call      SetPrintFlag
.

          Call      PrepPRtPage
          if (faxflag = 2)
                    display   *p1:24,*el,"Brk Break"
                    call      Samples                                         .why does samples
          else
                    display   *p1:24,*el,"No Fax Number"
          endif

          move      C0,mlrcnt           .reset count for cover sheet
.         clear     HOLDBrkMLR
          return
.*******************************************************************************
PRTOWNbx
          move      C0,N10
          call      Trim using OQTY
          move      OQTY,N10

          if (N10 = C0 & hotflag = 2)             .if order qty is zero print Office copy
.                   if (hotflag = 2)
                    goto prtoffbx
          ElseIf    (N10 = C0 & hotflag = 1)                .if order qty is zero don't print during full run
.                   else
                    return
.                   endif
          endif

          reset     RUNCODES
          scan      OLNUM,RUNCODES

          if EQUAL
..Need to refresh last valid OLON, so that .LST files are properly named
.22Aug08 DH                   move      holdown,OLON
                    return
          endif
          Move      LstMGt,HoldLstMgt
          clear     lstmgt
          pack      lstmgt,OSALES10,OSALES
          rep       zfill,lstmgt

          add       C1,COUNTOwn
          display   *P10:14,*EL,"Owner COUNT Processed ",COUNTOwn,b1,olrn
          
.Hot Print - set flag and go to Hotown
          if (hotflag = 2)
          move      YES,FirstFlag
          Goto      Hotown              
          endif
.Full Run
regown
          match     olon,holdown              *same owner?
          if not equal
                    move      YES,FirstFlag
                    call      ownbreak
                    call      OwnSetPrintFlag
          endif
          add       C1,OWNCNT
hotown
.begin patch 9.7
          call      PrepOwn
          move      Emailflag,Emailflag1
.end patch 9.7
          move      NO,OformBFlag
          call      prtordfrm
          call      prtownerboxGui
          call      Process1
          return

ownbreak
          if        (PdfFName <> "")       .if not null we have a file to process
..send will create cover page if appropiate
          call      PrepSend
          call      Samples
          call      senditout
          endif
          display   *p1:24,*el,"owner break",COUNTOwn,b1,olrn
.         if (countIn = C1)
          if (countOwn = C1)
                    move      C1,faxflag
                    move      OLON,holdown
                    move      OLON,smpown
                    move      OLON,Fhandle
                    call      prepprtpage
                    return
          endif
.begin patch 9.7
          call      Prepown
.begin patch 9.7
.                   move      "                                             ",faxattn
.                   move      "                                             ",faxname
.                   clear     faxattn
.                   clear     faxname
..                  move      holdown,NOWNFLD
..                  rep       zfill,NOWNFLD
.                   move      OLON,NOWNFLD
.                   rep       zfill,NOWNFLD
.                   move      "ownbreak-NOWNKEY",Location
.                   pack      KeyLocation,"Key: ",NOWNFLD
.                   call      NOWNKEY                       .make sure we have last owners info
..                  move      holdown,fhandle
.
.                   move      C1,faxflag                    .don't fax, re-initialize
.                   move      C0,owncnt           .reset count for cover sheet.
.                   move      C0,ownscnt                    .reset sample count for cover sheet.
.                   move      OLON,holdown                  .set break
.                   move      Olon,Fhandle
.
.                   move      ownocpy,faxname
.                   move      OWNFAX,FAXTELE
.                   move      ownlonm to faxattn
..begin patch 9.7
.                   Clear     NFulEmail
.                   call      Trim Using OwnEmail
.                   move      OwnEmail,NFULEMAIL
.                   if        (formflag = c2 & NfulEmail <> "")          . I think we have a winner
.                   Move      Yes,EmailFlag
.                   else
.                   Move      No,EmailFlag
.                   endif
.         Return
..end patch 9.7 
                    
.                   call      PrepSend
.                   call      samples
.                   call      senditout
.
..Refresh after creating Files
.                   move      OLON,NOWNFLD
.                   rep       zfill,NOWNFLD
.                   move      "ownbreakB-NOWNKEY",Location
.                   pack      KeyLocation,"Key: ",NOWNFLD
.                   call      NOWNKEY                       .make sure we have last owners info
.         endif
.Initialize a bunch of vars
.         move      C1,faxflag                    .don't fax, re-initialize
.         move      C0,owncnt           .reset count for cover sheet.
.         move      C0,ownscnt                    .reset sample count for cover sheet.
.         move      OLON,holdown                  .set break
.         move      Olon,Fhandle
          call      PrepPrtPage
          return
.begin patch 9.7
Prepown
                    move      "                                             ",faxattn
                    move      "                                             ",faxname
                    clear     faxattn
                    clear     faxname
                    move      OLON,NOWNFLD
                    rep       zfill,NOWNFLD
                    move      "ownbreak-NOWNKEY",Location
                    pack      KeyLocation,"Key: ",NOWNFLD
                    call      NOWNKEY                       .make sure we have last owners info

                    move      C1,faxflag                    .don't fax, re-initialize
                    move      C0,owncnt           .reset count for cover sheet.
                    move      C0,ownscnt                    .reset sample count for cover sheet.
                    move      OLON,holdown                  .set break
                    move      Olon,Fhandle

                    move      ownocpy,faxname
                    move      OWNFAX,FAXTELE
                    move      ownlonm to faxattn
.begin patch 9.75
                    move      C0,N10
                    move      OwnFAX,N10
                    if        (N10 = C0)
                    Move      c1,faxflag
                    else
                    Move      c2,faxflag
                    endif
                    call      trim using ofullfil
                    if        (OFULLFIL <> "")            .means there was a fulfillment
                    Move      c1,faxflag
                    Else
                    CLear     NFulcomp                    .make sure empty as no fulfillent
                    move      OLON,smpown
                    endif
.begin patch 9.75
                    Clear     NFulEmail
                    call      Trim Using OwnEmail
                    move      OwnEmail,NFULEMAIL
                    if        (formflag = c2 & NfulEmail <> "")          . I think we have a winner
                    Move      Yes,EmailFlag
                    else
                    Move      No,EmailFlag
                    endif
          Return
.end patch 9.7 

OwnSetPrintFlag
          display   *p1:24,*el,"checking owner ",ownocpy
.When we decide to Fax out everything, we can remove all of the following up to ownbrk1, and also remove ownbrk2 label
.begin patch 9.75             ... put this at prepown if we need to suppress owner copies
          return
.end patch 9.75
        call  trim using ofullfil      
          if (OFULLFIL <> "")                           .we are on owner pass was there a Fulfillment >
.                                                       .yes so do not fax/email owner
                    move      C0,N10
                    move      C1,faxflag        .
                    move      C1,PrintFlag
          else
                    Move      C2,Faxflag                >no fulfillment so set up the owner 
                    clear     COMPFLD6
                    clear     COMPVARS
                    clear     NFULNUM
                    clear     NFULCOMP
                    clear     NFULCNT
                    clear     NFULFAX
                    move      OLON,smpown
          endif
          REturn
*******************************************************************************
PRTFULbx
          add       C1,COUNT3
          display   *P10:15,*EL,"Ful COUNT READ ",COUNT3,b1,olrn
          move      C0,N10
          call      Trim using OQTY
          move      OQTY,N10
          compare   C0,N10
          if equal
                    if (PDFFlag = 1)
                              if (!externalmode)
                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                    endif
                    return
          endif

          call      Trim using OFULLFIL
          if (OFULLFIL = "")
                    if (PDFFlag = 1)
                              if (!externalmode)
                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                    endif
          return
          endif


          call      Trim using NFULCOMP
          call      Trim using OFULLFIL
.begin patch 9.71
          if (OFULLFIL = "009406" | OFULLFIL = "009411")
.         if (OFULLFIL = "009406")
                    if (TRIPLEX <> "1")
                              display   *p1:24,*el,"It's Triplex or Target Analysis, Skip"
.                             display   *p1:24,*el,"IT's Triplex, Skip"
.end patch 9.71
                              if (PDFFlag = 1)
                                        if (!externalmode)
                                                  erase     "c:\progra~1\pdf995\flag.dat"
                                        endif
                              endif
                              return
                    endif
          endif

          match     "0001",ortnnum       .REUSE
          if equal
                    display   *p1:24,*el,"IT's RE-USE, Skip"
                    if (PDFFlag = 1)
                              if (!externalmode)
                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                    endif
                    return
          endif

          reset     RUNCODES
          scan      OLNUM IN RUNCODES
          if equal
                    display   *p1:24,*el,"It's Running charges, Skip"
                    if (PDFFlag = 1)
                              if (!externalmode)
                                        erase     "c:\progra~1\pdf995\flag.dat"
                              endif
                    endif
                    return
          endif

          if        (hotflag = 1)
                    goto regful
          elseif    (hotflag = 2)
                    goto prtfulby
          endif
regful
whoanelly
          if (OFullFil <> HOLDCCTO)
                    move      YES,FirstFlag
                    call      fulbreak
          endif
          add       c1,owncnt
          goto                prtfulby
fulbreak
............................................................
          if        (PdfFName <> "")
.send will create cover page if appropiate
                    fill      B1,faxname
                    move      holdcccmp,faxname
                    call      Trim using holdcccnt
                    if (holdcccnt = "")
                              move      "Order Fulfillment",faxattn
                    else
                              move      holdcccnt,faxattn
                    endif
.
          call      PrepSend
          call      senditout
          endif

          display   *p1:24,*el,"Fulfillment Break",COUNT3,b1,olrn
          if (countIN = C1 | Holdccto = "")               .1st valid record     
                    move      C1,faxflag
                    Call      GetFullFill
                    move      NFULNUM,holdccto
                    move      NFULNUM,FHandle
                    move      NFULCNT,holdcccnt
                    move      NFULCOMP,holdcccmp
                    move      EmailFlag,EmailFlag1
                    call      PrepPrtPage

                    move      C1,faxflag
                    move      C0,N6
                    call      Trim using NFULNUM
                    move      NFULNUM,N6
                    if (N6 > C0)
.If var somehow was not zero-filled correctly, do it now
                              move      N6,NFULNUM
                              rep       zfill,NFULNUM
                              move      C0,N10
                              call      Trim using NFULFAX
                              move      NFULFAX,N10
                              if (N10 > 0)
                                        move      C2,faxflag        .yes set fax fulfilment flag on.
                                        move      NFULFAX,FAXTELE
                              endif
                    endif
.         call      PrepPrtPage
.         
          return
          endif
          
          if        (OfullFil = "" & Holdccto = "")
          return
          endif
          

.now lets set up for current one.
          clear     holdccto
          Call      GetFullFill
          
          move      NFULNUM,holdccto
          move      NFULNUM,FHandle
          move      NFULCNT,holdcccnt
          move      NFULCOMP,holdcccmp
          move      EmailFlag,EmailFlag1
          call      PrepPrtPage
          move      C1,faxflag
          move      C0,N6
          call      Trim using NFULNUM
          move      NFULNUM,N6
          if (N6 > C0)
.If var somehow was not zero-filled correctly, do it now
                    move      N6,NFULNUM
                    rep       zfill,NFULNUM
                    move      C0,N10
                    call      Trim using NFULFAX
                    move      NFULFAX,N10
                    if (N10 > 0)
                              move      C2,faxflag        .yes set fax fulfilment flag on.
                              move      NFULFAX,FAXTELE
                              return
                    endif
          endif
.         call      PrepPrtPage
          return
............................................................................
..................................................................................................
prtfulby
          move      NO,OformBFlag
          call      prtordfrm
          call      prtfulfilboxGui
          call      Process1
          return
.
PRTovrly
          add       C1,COUNt5
          display   *P10:17,*EL,"Overlay COUNT READ ",COUNT5,b1,olrn
          move      C0,N10
          call      Trim using OQTY
          move      OQTY,N10
          compare   C0,N10
          if equal
                    if (hotflag = 1)
                              return
                    elseif (hotflag = 2)
                              goto prtoffbx
                    endif
          endif
          reset     RUNCODES
          scan      OLNUM IN RUNCODES
          if equal
                    return
          endif
          call      Trim using OCOMSLCT
          if (OCOMSLCT = "")
                    return
          endif
          call      Trim using holdcom
          if (OCOMSLCT <> holdcom)
.                    if (holdcom <> "")
                    if (holdcom = "")
                              move      YES,FirstFlag
                              move      OCOMSLCT,holdcom
                              call      ovrbreak
                    endif
                    move      C0,OWNCNT
                    move      OCOMSLCT,holdcom
          endif
          add       C1,OWNCNT
          call      prtovr
          return
.
ovrbreak
          if        (PdfFName <> "")
.send will create cover page if appropiate
          call      PrepSend
          call      senditout
          endif
          display   *p1:24,*el,"overlay break"
          if (countIn = C1)
                    move      C1,FAXFLAG
          endif
.now lets set up for current one.
          move      C2,faxflag
          clear     faxattn
          move      B55,faxname
          if (holdcom = ovr1)
                    move      ovrtel1,FAXTELE
                    move      "COMS",fhandle
                    move      "Consumer Direct",faxname
          elseif (holdcom = ovr2)
                    move      ovrtel2,FAXTELE
                    move      "LIFE",fhandle
                    move      "Lifestyle",faxname
          elseif (holdcom = ovr3)
                    move      ovrtel3,FAXTELE
                    move      "IC",fhandle
                    move      "IC Systems",faxname
          else
                    clear     FAXTELE
                    move      "OVER",fhandle
                    move      C1,faxflag
                    move      C1,PrintFlag
.                    return
          endif
          pack      prtname,"nord",fhandle,".lst"
          pack      pdffname,"nord",fhandle,".lst"
          call      PrepPRtPage
          return
prtovr
          move      NO,OformBFlag
          call      prtordfrm
          call      prtfulfilboxGui
          call      Process1
          return
.
PRToffbx

          reset     RUNCODES
          scan      OLNUM IN RUNCODES
          if equal
                    clear     OODNUM
                    clear     OFDESC
          endif
          add       C1,COUNT4
          display   *P10:16,*EL,"Office COUNT READ ",COUNT4,b1,olrn
          move      YES,OformBFlag
          if (hotflag = 1)
                    clear     prtname
                    move      C1,PrintFlag
                    move      YES,FirstFlag
          endif
          call      prtordfrm
          call      prtofficeboxGui
          call      Process1
          return
..........................................................................................................................
.Create printfile -- all passes
PrepPrtPage
.FORMFLAG FORM      1      1=MAILER, 2=OWNER, 3=FULFILLMENT, 4=OFFICE, 5=overlay/zip screen
          move      B1,ERROR
          clear     PdfFName
          if        (hotflag = 1)
                    if        (formflag = 1)
                              if        (faxNumFlag <> 1)                     .broker copy
                              pack      PdfFname from "nrdb",fhandle
                              Else
                              pack      PdfFname from "nrdm",fhandle
                              endif
                    Elseif    (formflag = 2)
                    pack      PdfFname from "nord",fhandle

                    clear     SmpFIle
                    pack      taskname, NTWKPATH1,"nord"
                    append    taskname,SmpFile
                    append    OLON,SmpFile
                    append    ".smp",SmpFile
                    reset     SmpFile
                    prepare   OUTPUT,SmpFile
                    move      C1,smpflag                           .we have sample file
          
                    Elseif    (formflag = 3)
                    pack      pdfFname,"nordf",Fhandle
                    Elseif    (formflag = 4)
                    pack      pdfFname,"nord",FHandle
                    Elseif    (formflag = 5)
                    pack      pdfFname,"nord",FHandle
                    endif
          else
.         move      "Hotord",PdfFName
          move      inpname,PdfFName
          endif
          if        (PDFFname = "")
          alert     caution,"Location PrepPRtPage no PDFFname",result
          endif
          return



PrepSend
.OK creating coversheet --- as text of email -- can skip if printing or do anyway and it just gets NOT used
.THis section also prints the samples
.....TESTING.....
.         move      "5106288313",faxtele
.....TESTING.....
.Check and see if we should
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
.if to be faxed
          Count     n2,faxtele
          call      Trim using faxname
          if        (EmailFlag1 = Yes)
          move      NfulEmail,MailTo
.         elseif    (n2 = 7)                .we have fax #
          elseif    (n2 = 7 | n2 = 10)                .we have fax #
.         pack      MailTo,"[FACSYS:",faxname,"@",b1,"+",longdist,b1,faxtele,"]"
.         MOVe      "Jduenas@nincal.com",MailCC
          pack      MailTo,"IMCEAFACSYS-",longdist,faxtele,"@nincal.com"
          
.         pack      MailTo,"IMCEAFACSYS-14154337796@nincal.com"
          Else      
          Clear     Mailto                                 .no place to sendit

          call      Trim using Mailto
          endif
.         MOVe      "Jduenas@nincal.com",MailCC
          reset     faxname,45
blankc
          cmatch    B1,faxname
          if equal
                    bump      faxname,-1
                    goto blankc if not eos
          else
                    lenset    faxname
                    reset     faxname,1
          endif
          
          reset     attchlst
          move      C1,N2
          unpack    date,mm,dd,yy
          clock     time,time
          clear     str5
          append    time,str5
          reset     str5
          if        (Hotflag = c1)    .live run
          write     output2,seq;faxname
          endif     
          add       c1 to faxkount
.......................................
          Clear     Mailbody
          
          IF        (EmailFlag1 = yes)              .email
                    IF        (formflag = 2 | formflag = 3)          .Owner / Fulfillment 
                              Append    "Names in the News                      Pacific Lists, Inc.",Mailbody
                              append    CRLF,Mailbody
                              append    "1300 Clay St. 11th Floor               1300 Clay St. 11th Floor",Mailbody
                              append    CRLF,Mailbody
                              append    "Oakland, CA 94612-1429                           Oakland, CA 94612-1429",Mailbody
                              append    CRLF,Mailbody
                              append    "415-989-3350 * Fax 415-443-7796        415-945-9450 * Fax 415-945-9451",Mailbody
                              append    CRLF,Mailbody

                    Else
                              IF        (OcompID = "P" or (oCompid2 = "P" & formflag = 1))
                              Append    "Pacific Lists, Inc.",Mailbody
                              append    CRLF,Mailbody
                              append    "1300 Clay St. 11th Floor",Mailbody
                              append    CRLF,Mailbody
                              append    "Oakland, CA 94612-1429",Mailbody
                              append    CRLF,Mailbody
                              append    "415-945-9450 * Fax 415-945-9451",Mailbody
                              append    CRLF,Mailbody
                              append    "A Division of Names in the News",Mailbody                  
                              append    CRLF,Mailbody
          
                              Else
                              Append    "Names in the News",Mailbody
                              append    CRLF,Mailbody
                              append    "1300 Clay St. 11th Floor",Mailbody
                              append    CRLF,Mailbody
                              append    "Oakland, CA 94612-1429",Mailbody
                              append    CRLF,Mailbody
                              append    "415-989-3350 * Fax 415-443-7796",Mailbody
                              append    CRLF,Mailbody
                              endif
                    endif
          Endif

          if        (formflag = 2 | formflag = 3)                   .owner or fulfillment
          Append    "List Orders",Mailbody
          else
          Append    "List Order Confirmation",Mailbody
          endif

          aPPEND    crlf,mailbody
          aPPEND    crlf,mailbody
          Append    "VIA: ",Mailbody
.add more code ??
          if        (EmailFlag1 = Yes)
          Append    Mailto,Mailbody
          else
          append    FaxTele,Mailbody
          endif

          aPPEND    crlf,mailbody
          aPPEND    crlf,mailbody
          Append    "Date: ",Mailbody
          append    Today,mailbody
          aPPEND    crlf,mailbody
          aPPEND    crlf,mailbody
          Append    "To: ",mailbody
          append    Faxname,Mailbody
          aPPEND    crlf,mailbody

          if (faxattn <> "")
          endif
.list managment ?   
.         if        (LStmgt <> "06"  & LStmgt <> "19"  & LStmgt <> "27"  & LStmgt <> "28"  )
          if        (HoldLStmgt <> "06"  & HoldLStmgt <> "19"  & HoldLStmgt <> "27"  & HoldLStmgt <> "28")
                    IF        (formflag = 2 | formflag = 3)
                    aPPEND    crlf,mailbody
                    Append    "From: ",mailbody
                    append    "Order Requests",Mailbody
.                   else
.                   aPPEND    crlf,mailbody
.                   Append    "From: ",mailbody
.                   append    "Brokerage",Mailbody
                    endif
          aPPEND    crlf,mailbody
          aPPEND    crlf,mailbody
          Append    "Orders Enclosed",Mailbody
          aPPEND    crlf,mailbody
          if (ownscnt > 0)
                    aPPEND    crlf,mailbody
                    aPPEND    crlf,mailbody
                    Append    "Sample(s) Enclosed",Mailbody
                    aPPEND    crlf,mailbody
                    if (owncnt > 1)
                              aPPEND    crlf,mailbody
                              Append    "Note: Sample(s) May need to be attached to multiple Orders.",Mailbody
                              aPPEND    crlf,mailbody
                    endif
          endif
          
          aPPEND    crlf,mailbody
          aPPEND    crlf,mailbody
          Append    "Please call if you do not receive all pages.",Mailbody
          aPPEND    crlf,mailbody
          
          Else
          append    CRLF,Mailbody
                    IF        (OcompID2 = "P")
                    Append    "From: Pacific Lists Inc.",Mailbody
                    Else
                    Append    "From: Names in the News",Mailbody
                    endif
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          Append    "List Management Dept.",Mailbody
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          Append    "Tel:   ",mailbody
          append    Nfaxtel2,Mailbody
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          if        (formflag = 1)           .mailer
          Append    mlrcnt,mailbody          .mailer count    
          endif
          append    " order confirmation(s) included.",mailbody
          append    CRLF,Mailbody
          append    CRLF,Mailbody
          Append    "Please NOTE:",Mailbody
          append    CRLF,Mailbody
                    IF        (OcompID2 = "P")
                    append    "This is a CONFIRMATION of your brokerage order(s) for our Pacific Lists managed ",Mailbody
                    Else
                    append    "This is a CONFIRMATION of your brokerage order(s) for our Names in the News managed ",Mailbody
                    endif
          append    CRLF,Mailbody

          Append    "list(s). Please review this information and contact our list managers if you note any",mailbody
          append    CRLF,Mailbody
                    IF        (OcompID2 = "P")
                    append    "discrepancies or errors. We can be reached at (415) 945-9450. Our fax number is (415)",Mailbody
                    append    CRLF,Mailbody
                    append    "945-9451",mailbody
                    append    CRLF,Mailbody
                    append    CRLF,Mailbody
                    append    "Thank you for ordering Pacific Lists-managed lists. We appreciate your business.",mailbody
                    else
                    append    "discrepancies or errors. We can be reached at (415) 989-3350. Our fax number is (415)",Mailbody
                    append    CRLF,Mailbody
                    append    "433-7796",mailbody
                    append    CRLF,Mailbody
                    append    CRLF,Mailbody
                    append    "Thank you for ordering Names in the News-managed lists. We appreciate your business.",mailbody
                    endif
          append    CRLF,Mailbody
          Endif
          reset     Mailbody  
.Clean up
          clear   taskname

          return

.prepare spool file.
Samples
.         Owner copy?
          if        (Formflag <> c2)
          move      YES,OformBFlag                    .not owner get out
          return
          endif

          move      NO,nosmpl
          clear     attchlst                       .start attachment list
          append    "nord",attchlst
          append    OLON,attchlst
          append    ".LST",attchlst
          movelptr  ATTCHLST,LPTCNT

.CLOSE LAST OWNERS "ATTATCHMENT FILE"
          if (smpflag = c1 | smpflag = c2)     .do we have file and/or data
                    weof      output,seq                        .yes
                    close     output                            .ditto
          endif                                       .done
.ARE WE FAXING? IF SO BUILD FILENAME TO HOLD FAX IMAGES
          compare   C1,countIn            .very first record?
          if not equal               .no so continue process
LASTRUN
.append to existing pdf 
                              if (smpflag = c1 | smpflag = c2)
                                        clear     SmpFIle
                                        pack      taskname, NTWKPATH1,"nord"
                                        append    taskname,SmpFIle
                                        append    smpown,SmpFile
                                        append    ".smp",SmpFile
                                        reset     SmpFile
                                        trap      drewbreak giving error if IO
                                        open      file2,SmpFile
                                        trap      IOMssg giving error if io
.
.                                       move      Yes,FirstFlag1
                                        clear     N8
                                        Clear     DCX2
                                        read      file2,seq;dcx2
                                        if over
                                                  move      YES,nosmpl
                                                  goto nosmp
                                        endif
                                        close     file2

                                        IF        (DCX2 = "")
                                        move      YES,nosmpl
                                        goto nosmp
                                        endif

                                        clear     SMPArray
                                        trap      drewbreak giving error if IO
                                        open      file2,SmpFIle
                                        trap      ioMssg giving error if io
..Below logic used for testing purposes to save speed - KEEP IT!!!!
..To test:  Unrem all lines preceded with ".**" and rem all others until you hit "End Test Logic"
.**        loop
.**bigloop         read    file2,seq;DCX2
.**                until over
.**                clear   SMPIndex
.**                move    C1,SMPIndex
.**               loop
.**                        scan    DCX2 in SMPArray(SMPIndex)
.**                        goto BigLoop if equal
.**                        until   (SMPIndex = 50)
.**                        until   (SMPArray(SMPIndex) = "")
.**                        add     C1,SMPIndex
.**                repeat
.**                move    DCX2,SMPArray(SMPIndex)
.**                prtpage file1;*P2:2,DCX2
.**        repeat
.**        PRTclose file1,"FAXFILE",""
BigLoop
.                                       call      PrtCloseFile
                                        loop
                                                  read      file2,seq;DCX2
                                                  until over

                                                  call      TRIM using DCX2
AFTERTRIM
                                                  if (DCX2 = "")
                                                            goto printstop
                                                  endif
                                                  clear     SMPIndex
                                                  move      C1,SMPIndex

                                                  loop
                                                            scan      DCX2,SMPArray(SMPIndex)
                                                            goto BigLoop if equal
                                                            until (SMPIndex = 50)
                                                            until (SMPArray(SMPIndex) = "")
                                                            add       C1,SMPIndex
                                                  repeat
                                                  move      DCX2,SMPArray(SMPIndex)
                                                  pack      DCXFile,DCXPath,DCX2
.Print and Display First Page
                                                  clear     N9
............................................
                                       CREATE  PICT1=70:495:70:620:
                                                DCXFile,BORDER=0,AUTOZOOM=0
                                        PICT1.GetPageCount GIVING N9
.                                        if (FirstFlag1 = Yes)
.                                                move    NO,FirstFlag1
.                                                prtpage Laser;*P2:2,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
                                                prtpage Laser;*units=*CHAR,*NEWPAGE,*P2:2,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
.                                        else
.                                                prtpage Laser;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
.                                        endif
.Print and Display Additional Pages
                                                  if (N9 > C1)    .Only Enter loop if more than one page
                                                            clear     N8
                                                            move      C1,N8   .Start with SECOND PAGE as first page already printed
                                                            loop
                                                                      add       C1,N8
                                                                      until (N8 > N9)
                                                        CREATE  PICT1=70:495:70:620:
                                                                DCXFile,BORDER=0,AUTOZOOM=0,PAGE=N8
                                                        activate PICT1
                                                        prtpage Laser;*P2:2,*NEWPAGE,*MarginL=0,*MarginT=0,*PICTRECT=*OFF,*PICTvis=1:110:1:87:PICT1;
                                                            repeat
                                                  endif
                                        repeat
printstop
.test 13 Feb09 DLH
                                        close     file2
                                        erase     smpfile
.end test 13 Feb09 DLH
                                        
                                        PRTclose Laser
                                        DESTROY PICT1
test3
                                        move      C0,LastFlag
.begin patch 9.7
.                                       rename   "C:\WORK\Faxfile.prn",spoolf
.begin patch 9.7
                              endif
          endif
.
NOSMP
          clear     SmpFIle
          pack      taskname,NTWKPATH1,"nord"
          append    taskname,SmpFIle
          append    OLON,SmpFIle
          append    ".smp",SmpFIle
          reset     SmpFIle
          prepare   OUTPUT,SmpFile
          move      C1,smpflag
          return

PROCESS1
          move      " ",EXCHANGE
          move      " ",ENTIRE
          move      " ",TEST
          move      " ",CONT
          move      " ",CONT1
          move      " ",REPRT
.begin patch 9.77
..............................................................................................
.Get Offer
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
          bump      OODNUM,4
          pack      NOFRFLD,OMLRNUM,OODNUM
          reset     OODNUM
          move      "C.Record-NOFRKEY",Location
          pack      KeyLocation,"Key: ",NOFRFLD
          call      NOFRKEY
.
          move      "10",FILE
          clear     ARCD
          clear     EXT
          clear     PHONE
          clear     LP
          clear     RP
          clear     DASH
          move      OLON,NOWNFLD
          rep       zfill,NOWNFLD
          move      "C.Record-NOWNKEY",Location
          pack      KeyLocation,"Key: ",NOWNFLD
          call      NOWNKEY
.Begin patch 9.7 
          call      Trim using OFULLFIL
          if        (OFULLFIL <> "" & formflag <> 3)              .test DH if it is formflag 3 take care of elsewhere??
.         if        (formflag <> 3)
          call      GetFullFIll
          endif
.end patch 9.7 
          match     "0000000000",OWNTELE                    *PHONE NUMBER?
          call      PHONE if not equal            *YES
          clear     BILDDATA
          move      OLRN,NINVFLD
          move      C1,NINVPATH
          move      "C.Record-NINVTST",Location
          pack      KeyLocation,"Key: ",NINVFLD
          call      NINVTST
          if not over
                    move      BILDTXT,BILDDATA
          endif
          clear     REVDATA
          clear     REVTYP
          pack      NCRCFLD,OLRN
          move      "C.Record-NCRCKEY",Location
          pack      KeyLocation,"Key: ",NCRCFLD
          call      NCRCKEY

          loop
                    until over
                    until (NCRCFLD <> NCRCKEY)
                    move      NCRCTYP,REVTYP
                    if (NCRCCODE = "C")
                              pack      REVDATA,CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
                              break
                    else
                              pack      REVDATA,REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,NCRCCC,NCRCYY
                    endif
                    move      "C.Record-NCRCKS",Location
                    call      NCRCKS
          repeat

          sub       FILE,FILE
.end patch 9.77

..............................................................................................
.should be able to eliminate these extra reads
          pack      MKEY,OMLRNUM,OCOBN
          move      "Process1-NMLRKEY",Location
          pack      KeyLocation,"Key: ",MKEY
          call      NMLRKEY
          
          clear     BRCOMP
          clear     NBRKFLD
          pack      NBRKFLD,OBRKNUM,OBRKCNT
          call      Trim using NBRKFLD
          if (NBRKFLD <> "")
                    move      "Process1-NBRKKEY",Location
                    pack      KeyLocation,"Key: ",NBRKFLD
                    call      NBRKKEY
                    if not over
                              move      MCOMP,MNAME
                    endif
          endif
        move    compaddr,str35
          move      C0,NFIELD23
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
                    move      "/M",NMODDESC
          else
                    pack      NMODFLD,NSEL2DESC
                    rep       zfill,NMODFLD
                    move      "NMODKEY",Location
                    pack      KeyLocation,"Key: ",NMODFLD
                    call      NMODKEY
                    if over
                              move      "/M",NMODDESC
                    else
                              call      Trim using NMODDESC
                    endif
          endif
          if (copy = 1 | copy = 2 | copy = 3)
                    call      Trim using OFOSAVE
                    if (OFOSAVE <> "")
                              move      OFOSAVE,OFOCODE
                    endif
          endif
          clear     MEDIA
          call      Trim using OFOCODE
          if (OFOCODE <> "")
                    move      C0,NFIELD23
                    move      OFOCODE,OFOSAVE         *SAVE VARIABLE
                    move      OFOCODE,NFIELD23
                    move      MED0,MEDIA
                    load      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
                              MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                              MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                              MED23,MED24,MED25,med26,med27,med28,med29
                    clear     MEDTYPE
                    move      YES,MEDTYPE
                    load      MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
                              YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES,no,no
          endif
.SAMPLE
          clear     SAMPLE
          clear     NSMPDES1
          call      SAMPLE
          clear     RTCNTCT
          clear     COMSLCT
          clear     RTCOMP
          clear     RTCITY
          clear     RTSTATE
          clear     RTZIP
          clear     RTTELE
          clear     CORTN
          clear     CONTDTE
          clear     CONT
          clear     CONTQTY
          clear     MEDMEMO
          clear     FLAGPAID
          if (OCOMSLCT = "C")           .COMSELECT OVERLAY?
                    call      COMSLCT
          elseif (OCOMSLCT = "L")                 .LIFESTYLE OVERLAY?
                    call      LIFESTYL
          elseif (OCOMSLCT = "I")                 .IC SYSTEMS OVERLAY?
                    call      ICSYSTEM
          endif
          pack      QTYOUT,QTYMSK
          call      Trim using OQTY
          move      OQTY,QTYNUM
          edit      QTYNUM,QTYOUT
          if (OCCODE = "1")             .CONTINUATION ORDER
                    call      CONTIN
          elseif (OCCODE = "2")                   .CONTINUATION ORDER/NO OMIT
                    call      CONTIN1
          endif
          if (GUARCODE = "6" | GUARCODE = "7" | GUARCODE = "8" | GUARCODE = "9")          .PREPAID ORDER?????
                    call      PREPAID
          endif
          bump      OODNUM,4
          move      OODNUM,OFFEROUT
          move      ORTNNUM,NRTNFLD
          move      "Process1-NRTNKEY",Location
          pack      KeyLocation,"Key: ",NRTNFLD
          call      NRTNKEY
          clear     RTARCD
          unpack    RTTELE,RTARCD,RTEXT,RTPHONE
          clear     RTPHMASK
          move      "-",DASH
          call      Trim using RTARCD
          if (RTARCD <> "")
                    pack      RTPHMASK from LP,RTARCD,RP,B1,RTEXT,DASH,RTPHONE
          else
                    clear     RTPHMASK
          endif
          if (ORTNNUM <> "2531")
                    clear     str45
                    clear     str45a
                    move      MCOMP,str45
                    move      RTCOMP,str45a
                    rep       uplow,str45
                    rep       uplow,str45a
                    reset     str45
                    reset     str45a
                    if (str45 <> str45a)
                              call      CHNGRET
                    endif
          endif
          if (OMLRNUM = "0677" | OMLRNUM = "0210" | OMLRNUM = "0053" | OMLRNUM = "0702" | OMLRNUM = "0965" | OMLRNUM = "1361")
.USE MLR.OFR DESC ON RET-TO
                    call      USEOFR
          endif
          if (ANS = "R")
.REPRINT
                    call      REPRT
          endif
TEST
          move      C0,NFIELD23           *CLEAR FIELD
          move      OTOCODE,NFIELD23
          if (NFIELD23 = 1 | NFIELD23 = 2)
                    move      "X",TEST
          endif
          move      "0",NFIELD23
          move      OELCODE,NFIELD23
          if (NFIELD23 = 1)
                    call      ENTRENT
          elseif (NFIELD23 = 2)
                    call      EXCHANG1
          elseif (NFIELD23 = 3)
                    call      ENTIRE
          endif
          call      OPRINT1
          return

SAMPLE
          move      C0,NFIELD23
          move      OSCODE,NFIELD23
          move      "                          ",SAMPLE
          call      Trim using OSCODE
          if (OSCODE <> "" & OSCODE <> "0")
                    branch    NFIELD23,SAM1,SAM2,SAM3
                    clear     SAMPLE
          endif
          return
PHONE
          unpack    OWNFAX,str3,str2,str1,str4
          call      Trim using str3
          if (str3 <> "")
                    pack      FAX1,"(",str3,")",str2,str1,"-",str4
          else
                    pack      FAX1,str2,str1,"-",str4
          endif
          unpack    OWNTELE,ARCD,EXT,PHONE
          move      "-",DASH
          match     "   ",ARCD
          if not equal
                    move      "(",LP
                    move      ")",RP
          endif
          return
PREPAID
          move      "**",FLAGPAID
          return
SAM1
          move      "Sample enclosed",SAMPLE
          match     Z3,OSAMCDE
          return if equal
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
          return
SAM2
          move      "Sample to follow",SAMPLE
          return
SAM3
          move      "Sample previously cleared",SAMPLE
          return
ENTRENT
          move      "X",ENTIRE
          move      "        ",EXCHANGE
          return
EXCHANG1
          match     "         ",OEXQTY
          if not equal
                    return
          endif
          move      "Exchange",EXCHANGE
          return
ENTIRE
          move      "Exchange",EXCHANGE
          move      "X",ENTIRE
          return
OPRINT1
.why suppress revision info on faxes or emails??????   13Feb09 DH
.          if (faxflag <> 2)
                    prtpage   Laser;*p=2000:425,*font=fontO12b,REVDATA
.          endif
.why suppress revision info on faxes or emails??????   13Feb09 DH
          prtpage   Laser;*p=2000:810,*font=fontO12b,REPRT  ???????????????????????
          prtpage   Laser;*p=2000:645,BILDDATA
          prtpage   Laser;*p=625:800,*font=fontO10,Olrn
          prtpage   Laser;*p=3125:800,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
          prtpage   Laser;*p=1000:988,OMLRPON
          cmatch    B1,BRCOMP
          if not eos
                    branch    FORMFLAG OF BRKOK,BRKNTOK,BRKNTOK,BRKOK,brkntok
                    branch    copy of brkntok,brkntok,brkok
                    goto brkok
brkntok
                    clear     careof
                    clear     str45
                    clear   str35
                    clear     attn
                    clear     brcntct
                    clear compaddr
                    clear brcity
                    clear brstate
                    clear brzip
                    goto      printmlr
brkok
                    move      "c/o",careof
                    cmatch    B1,BRCNTCT
                    if equal
                              clear     attn
                    else
                              move      "Attn: ",attn
                    endif
                    move      BRCOMP,str45
                    move    str35,compaddr
printmlr
                    prtpage   Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
                    prtpage   Laser;*p=1000:1176,MNAME
                    prtpage   Laser;*p=1000:1351,careof,b1,str45
                    prtpage   Laser;*p=3810:1351,attn,BRCNTCT
          prtpage   Laser;*p=1000:1526,compaddr
             if (brCITY = "")
             prtpage          Laser;*p=1000:1701,"                            "
                    else
                    prtpage   Laser;*p=1000:1701,brCITY,comma,brSTATE," ",brZIP
                    endif
                    goto OPRINT1B
           endif
OPRINT1A
          branch    FORMFLAG TO DOCC,WIPECC,WIPECC,DOCC,wipecc
          branch    COPY OF WIPECC,WIPECC
          goto docc
wipecc
          clear     str15
          goto      prntmlr1
docc
          move      mccto,str15
prntmlr1
          branch    FORMFLAG OF prntmlr2,prntmlr3,prntmlr3,prntmlr2,prntmlr3
          branch    copy of prntmlr3,prntmlr3,prntmlr3
prntmlr2
          prtpage   Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
          prtpage   Laser;*p=1000:1176,MNAME
          prtpage   Laser;*p=1000:1351,MCOMP,b5,str15
          prtpage   Laser;*p=1000:1526,compaddr
          prtpage   Laser;*p=1000:1701,COMPCITY,", ",COMPSTATE," ",COMPZIP
          prtpage   Laser;*p=1000:1876,OFDESC
          prtpage   Laser;*p=125:2020,"##",OFFEROUT
          prtpage   Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
          goto      oprint1b2
prntmlr3
          prtpage   Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
          prtpage   Laser;*p=1000:1351,MCOMP,b5,str15
OPRINT1B
          prtpage   Laser;*p=1000:1876,OFDESC
          prtpage   Laser;*p=125:2020,"##",OFFEROUT
          prtpage   Laser;*p=1000:2020,SAMPLE,B1,Nsmpdes1
oprint1b2
   branch FORMFLAG TO prtlist,PRTOWN,PRTOWN,PRTOWN,prtown
          compare   C0,copy
          goto prtlist if equal
prtown
.
          prtpage   Laser;*p=1000:2281,OWNLONM
          prtpage   Laser;*p=125:2456,"##",OLON
          prtpage   Laser;*p=1000:2456,OWNOCPY,B1,LP,ARCD,RP,EXT,DASH,PHONE,"   Fax: ",FAX1
          prtpage   Laser;*p=1000:2631,OWNLOSA
          call      Trim using OWNLOCTY
          if (OWNLOCTY <> "")
                    pack      taskname,OWNLOCTY,", ",OWNLOS," ",OWNLOZC
          else
                    pack      taskname,OWNLOS," ",OWNLOZC
          endif
          prtpage   Laser;*p=1000:2806,taskname
          prtpage   Laser;*p=1000:3006,NFULCOMP
          goto prtlist
prtlist
          prtpage   Laser;*p=1000:3211,O1DES
          if (NSEL2SPRICE > C0)
                    unpack    NSEL2SPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
                    call      Trim using NSEL2NAME
                    pack      taskname,NSEL2NAME," @ ",str9,NMODDESC
          else
                    pack      taskname,NSEL2NAME
          endif
          prtpage   Laser;*p=125:3399,"##",OLNUM
          prtpage   Laser;*p=1000:3399,taskname
          cmatch    B1,onetfm
          if  equal
                    goto pqty
          endif
          move      C0,N2
          move      onetper,N2
          compare   C0,N2
          if not equal
                    cmatch    NO,onetfm
                    if equal
                              prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,"Per List Owner - Gross Billing No Deductions"
                    endif
                    cmatch    "F",onetfm           ."F" = flat/volume discount
                    if not equal               .this one is a net
                              prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
                              prtpage   Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
                    else                                 .this is flat/volume
                              prtpage   Laser;*p=1000:4903,*font=Font08bI,onetper,"% Net Arrangement",*font=Font08I,", Run charge @ $",onetrc,"/m"
                              prtpage   Laser;*p=1000:5033,"No Deducts, No CV required."

                    endif
          endif
pqty
.........................................
          prtpage   Laser;*p=1000:3587,*font=fontO10,QTYOUT
          match     "         ",OEXQTY
          goto OPRINT2 IF EOS
          goto OPRINT2 IF EQUAL
          prtpage   Laser;*p=3000:3587,"SEE BELOW"
          goto OPRINT3
OPRINT2
          match     "EXCHANGE",EXCHANGE
          goto REALPPM IF NOT EQUAL
          if (NSEL2PRICE <> C0)
                    goto REALPPM
          endif
          prtpage   Laser;*p=3000:3587,"EXCHANGE"
          goto OPRINT3
REALPPM
          unpack    NSEL2PRICE,str5,str3
          call      FormatNumeric using str5,str6
          pack      str9,str6,str3
          pack      taskname,str9,NMODDESC," ",EXCHANGE
          prtpage   Laser;*p=3000:3587,taskname
OPRINT3
          deleteitem          PackData,0
          pack      NSEL3FLD1,"01X1",OLRN
          move      "NSEL3AIM",Location
          pack      KeyLocation,"Key: ",NSEL3FLD1
          call      NSEL3AIM

          loop
                    until over
                    if (NSEL3CODE = "A")
                              pack      NADDFLD,OLNUM,NSEL3NUM
                              move      "NADDKEY",Location
                              pack      KeyLocation,"Key: ",NADDFLD
                              call      NADDKEY
                              if not over
                                        pack      NREFFLD,"A",NADDNUM
                                        move      "NREFKEY",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        pack      NMODFLD,NADDDESC
                                        rep       zfill,NMODFLD
                                        move      "NMODKEY",Location
                                        pack      KeyLocation,"Key: ",NMODFLD
                                        call      NMODKEY
                                        pack      taskname,NREFDESC,NSEL3PRICE,NMODDESC
                                        insertitem PackData,0,taskname
                              endif
                    elseif (NSEL3CODE = "L")
                              pack      NSLTFLD,OLNUM,NSEL3NUM
                              move      "NSLTKEY",Location
                              pack      KeyLocation,"Key: ",NSLTFLD
                              call      NSLTKEY
                              if not over
                                        pack      NREFFLD,"L",NSLTNUM
                                        move      "NREFKEY-2",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        pack      NMODFLD,NSLTDESC
                                        rep       zfill,NMODFLD
                                        move      "NMODKEY-2",Location
                                        pack      KeyLocation,"Key: ",NMODFLD
                                        call      NMODKEY
                                        pack      taskname,NREFDESC,NSEL3PRICE,NMODDESC
                                        insertitem PackData,0,taskname
                              endif
                    endif
                    move      "NSEL3KG",Location
                    call      NSEL3KG
          repeat

          PackData.GetCount giving howmany
          if (howmany > C0)
.                   move      "625",N8
.Sales wants this moved over.  They will bitch about it later, no doubt.
                    move      "1000",N8
                    move      "3587",N9
                    for result,"1",howmany
                              if (result = 7)
                                        move      "3000",N8
                                        move      "3587",N9
                              endif
                              getitem   PackData,result,taskname
                              unpack    taskname,NREFDESC,NSEL3PRICE,NMODDESC
                              if (NSEL3PRICE > 0)
                                        unpack    NSEL3PRICE,str5,str3
                                        call      FormatNumeric using str5,str6
                                        call      Trim using NREFDESC
                                        call      Trim using NMODDESC
                                        pack      taskname,str6,str3,NMODDESC,B1,NREFDESC
                                        pack      taskname,NREFDESC," @ ",str6,str3,NMODDESC
                              else
                                        pack      taskname,NREFDESC
                              endif
                              add       "188",N9
                              prtpage   Laser;*p=N8:N9,taskname
                    repeat
          endif
..........................
          prtpage   Laser;*p=1000:5251,OMLRKY
          prtpage   Laser;*p=1000:5501,MEDIA,"  ",MEDMEMO
          prtpage   Laser;*p=1000:5751,RTCNTCT
          prtpage   Laser;*p=125:5926,"##",ORTNNUM,B1,CORTN
          prtpage   Laser;*p=1000:5926,RTCOMP,B1,rtphmask
          call      Trim using RTADDR
          if (ORTNNUM = "0001")
                    prtpage   Laser;*p=1000:6101,RTADDR,B1,OREUSE
          else
                    prtpage   Laser;*p=1000:6101,RTADDR
          endif
          call      Trim using RTCITY
          if (RTCITY <> "")
. TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!

                    if (RTNUM = "5318")
                              pack      taskname,"incoming.files@donnelley.infousa.com"
                    else
                              pack      taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
                    endif
. TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
          else
                    pack      taskname,RTSTATE,B1,B1,RTZIP
          endif
          prtpage   Laser;*p=1000:6276,taskname
          prtpage   Laser;*p=125:6501,COMSLCT
          prtpage   Laser;*p=1250:6689,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.
          match     "  ",OSHP
          goto      NOSHIP IF EQUAL
          goto      NOSHIP IF EOS
          move      OSHP,NFIELD23
          move      SHIP0,SHIPdesc
          load      SHIPdesc FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                    SHIP6,SHIP7,SHIP8,SHIP9,ship10
          goto OPRINT4
NOSHIP
          clear     SHIPdesc
OPRINT4
          prtpage   Laser;*p=2500:6689,SHIPdesc
          prtpage   Laser;*p=1250:6876,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          prtpage   Laser;*p=5640:6733,TEST
          prtpage   Laser;*p=5640:7116,CONT1
          prtpage   Laser;*p=5640:7366,CONT
          prtpage   Laser;*p=5640:7616,OLRNCO,"  ",CONTDTE," ",CONTQTY
          prtpage   Laser;*p=5640:7866,ENTIRE
DISREGO
          move      "7126",result
          move      OLRN,NSPEFLD
          rep       ZFILL,NSPEFLD
          move      "REGO-NSPEKEY",Location
          pack      KeyLocation,"Key: ",NSPEFLD
.begin patch 9.8
.                    call      NSPEKEY
                    Type      nspefld                                 ....PL imported records are alpha numeric
                    if        equal
                    call      NSPEKEY
                    else
                    clear     DESC001
                    clear     DESC002
                    pack      NSPE3FLD,OLRN
                    rep       zfill,NSPE3FLD
                    move      C3,NSPE3LOCK
                    move      "O.LoadScreens-NSPE3KEY",Location
                    pack      KeyLocation,"Key: ",NSPE3FLD
                    call      NSPE3KEY
                    call      Trim using DESC005
                    move      desc005,desc002
                    endif
.end patch 9.8
          call      TRIM using DESC002
          call      PARSITUP using line1,DESC002,C1
          call      PARSITUP using line2,DESC002,C1
          call      PARSITUP using line3,DESC002,C1
          call      PARSITUP using line4,DESC002,C1
          call      PARSITUP using line5,DESC002,C1
          call      PARSITUP using line6,DESC002,C1
          call      PARSITUP using line7,DESC002,C1
          call      PARSITUP using line8,DESC002,C1
          call      PARSITUP using line9,DESC002,C1
          call      PARSITUP using line10,DESC002,C1
          call      PARSITUP using line11,DESC002,C1
          call      PARSITUP using line12,DESC002,C1
          call      PARSITUP using line13,DESC002,C1
          call      PARSITUP using line14,DESC002,C1
          call      SPCLNSTO                           SPEC INSTRUC ROUTINE
          move      line2,line1
          call      SPCLNSTO
          move      line3,line1
          call      SPCLNSTO
          move      line4,line1
          call      SPCLNSTO
          move      line5,line1
          call      SPCLNSTO
          move      line6,line1
          call      SPCLNSTO
          move      line7,line1
          call      SPCLNSTO
          move      line8,line1
          call      SPCLNSTO
          move      line9,line1
          call      SPCLNSTO
          move      line10,line1
          call      SPCLNSTO
          move      line11,line1
          call      SPCLNSTO
          move      line12,line1
          call      SPCLNSTO
          move      line13,line1
          call      SPCLNSTO
          move      line14,line1
          call      SPCLNSTO
          goto TYPIST
.
. ROUTINE FOR SPECIAL INSTRUCTION PRINT
.
SPCLNSTO
          add       "188",result
          prtpage   Laser;*p=125:result,*font=fontO10n,line1,*font=fontO10
          return
TYPIST
          branch    hotflag of cntques,needcnt
cntques
          branch    copy of discon2B,discon2B,discon2B
needcnt
          move      C0,NFIELD23
          move      OCOCODE,NFIELD23
          type      OCOCODE
          goto      CON10 IF NOT EQUAL
          move      OCOCODE,str2
          goto      DISCON2A
CON10
          clear     str2
          move      OCOCODE,str2
          rep       "A0B1C2D3E4F5G6H7I8J9",str2
          type      str2
          goto      CON20 IF NOT EQUAL
          move      str2,NFIELD23
          add       "10",NFIELD23
          goto      DISCON2A
con20
          clear     str2
          move      OCOCODE,str2
          rep       "K0L1M2N3O4P5Q6R7S8T9",str2
          type      str2
          goto      CON30 IF NOT EQUAL
          move      str2,NFIELD23
          add       "20",NFIELD23
          goto      DISCON2A
CON30
          clear     str2
          move      OCOCODE,str2
          rep       "U0V1X2Y3Z4",str2
DISCON2A
          clear     NCNTFLD
          move      C1,NCNTPATH
          move      C3,NCNTLOCK
          move      str2,NCNTFLD
          if        (NCNTFLD = "" or NCNTFLD = "  " or NCNTFLD = " ")
                    move      "00",NCNTFLD
          endif
          clear     cntphone
          clear     cnt
          move      "DISCON2A-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          call      NCNTKEY
          if over
                    move      "NOTHING",CNT
                    move      "(415)989-3350",CNTPHONE
                    move      "NAMES@NINCAL.COM",INTRNET
                    goto CNTEXIT
          endif
          scan      "()",cntphone
          if equal
                    clear     cntphone
          endif
          move      cntname,cnt
          clear     intrnet
          reset     cnt
          scan      "BILLING",cnt
.
          if not equal
                    reset     CNT
                    scan      "Billing",CNT
                    if equal
                              reset     CNT
                              goto cntexit
                    endif
          else
                    reset     CNT
                    goto cntexit
          endif
          reset     CNT
          call      RemoveChar using cntname,B1
          call      Trim using cntname
          if (cntname = "")
                    goto cntexit
          endif
          IF        (CntComp = "2")
          pack      intrnet,cntname,"@pacificlists.com"
          Else
          pack      intrnet,cntname,"@nincal.com"
          endif
cntexit
          prtpage   Laser;*p=5625:9500,CNT
          prtpage   Laser;*p=5625:9700,cntphone
          prtpage   Laser;*p=5625:9900,intrnet
DISCON2B
          if (hotflag = 2 & copy = 3)
                    move      "Hot Print",hotprt
          else
                    clear     hotprt
          endif
.note:  Because of following instruction, which skips verbage below it,
.       I am going to allow two more lines of special instructions to be
.       printed.  This will need to be amended in case logic is reinstated.  ASH 05/20/99 PATCH 7.4
          goto REG                    .11/5/93 dlh per sa
          match     "06",MSLSPER
          goto REG IF EQUAL
          match     YES,MEDTYPE
          if equal
                    prtpage   Laser;*p=125:10312,"Mailer will not pay for names identified as errors,"
                    prtpage   Laser;*p=125:10312,"bad zips, foreign, non-personal, intrafile or "
                    prtpage   Laser;*p=125:10312,"family duplicates, or hits to DMA MPS file."
                    goto PRTDONE
          endif
REG
yesnumb
          if (hotflag = 2 & copy = 3)
                    move      "Hot Print",hotprt
          else
                    clear     hotprt
          endif
          clear     taskname
          call      TRIM using DESC001
          if (DESC001 <> "")
                    scan      "After this",DESC001
                    if not equal
                              reset     DESC001
                              append    "After this order is fulfilled: ",taskname
                              append    DESC001,taskname
                              reset     taskname
                    else
                              pack      taskname,DESC001
                    endif
          endif
          call      PARSITUP using line1,taskname,C1
          call      SPCLNSTO
          call      PARSITUP using line2,taskname,C1
          move      line2,line1
          call      SPCLNSTO
          prtpage   Laser;*p=125:10400,ODOWJ,SLASH,REVTYP,B1,hotprt
numbdone
          branch    FORMFLAG TO PRTDONE,CHKSAM,PRTDONE,PRTDONE,prtdone
          goto PRTDONE
.CHKSAM - LIST OWNER COPY CHECK FOR SAMPLE
CHKSAM
          move      C0,N1
          move      OSCODE,N1
          branch    N1,CHKSAM1          .SAMPLE CODE = 1 SAMPLE ENCLUSED?
          goto PRTDONE               .NO
CHKSAM1
          type      OSAMCDE
          goto PRTDONE IF NOT EQUAL
          branch    formflag  to prtdone,smpwrt,prtdone,prtdone,prtdone
          goto prtdone
smpwrt
          move      "S",str1
          move      ".TIF",str4
          move      MCOMP,MlrNameHold
          move      MNAME,MlrMNameHold
.
          move      "smpwrt-COMPKEY3",Location
          pack      COMPFLD3,OMLRNUM
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          pack      str14,str1,COMPNUM,OSAMCDE,str4
          scan      str14,attchlst
.Need to refresh MCOMP
          move      MlrNameHold,MCOMP
          move      MlrMNameHold,MNAME
          goto prtdone if equal              .already in list
          reset     attchlst
          setlptr   ATTCHLST,LPTCNT
          endset    attchlst
          append    ",",attchlst
          append    str14,attchlst            .add to list
.begin patch 9.76
          if        (faxflag = c2 & Hotflag = c1)       .faxable & LIve run
.          compare   C2,faxflag
.          if equal          .yes
.end patch 9.76
.                   move      "                                        ",APIFileName
.                   clear     APIFileName
.                   pack      APIFileName,dcxpath,str14,hexzero
.                   call      FindFirstFile
.                   if (APIResult <> 0 & APIResult <> hexeight)
                    pack      taskname from dcxpath,str14
                    FindFIle  Taskname
                              if        Zero                       .its there
                              write     output,seq;str14
                              add       C1,ownscnt                .keep track of count
                              move      C2,smpflag
                    endif
          endif
PRTDONE
          reset     attchlst
          add       C1,COUNTspool
          display   *P14:20,*EL,"NINCAL ORDERS SPOOLED : ",COUNTspool
          if (hotflag = 2)    .Hot Print
                    add       C1,COPY
          endif
          return

. CHNGRET - PRINT MAILER COMPANY AS RETURN TO CONTACT.
CHNGRET
          move      MCOMP,RTCNTCT
          move      "c/o",CORTN
          return
. USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
          move      OFDESC,RTCNTCT
          move      "c/o",CORTN
          return
. CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
          move      "X",CONT
          pack      CONTDTE,OODTECOM,SLASH,OODTECOD,SLASH,OODTECOY
          move      QTYMSK,CONTQTY
          call      Trim using OQTYCO
          move      OQTYCO,QTYNUM
          edit      QTYNUM,CONTQTY
          return
. CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
          move      "X",CONT1
          return
. REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
          move      "*** REPRINT ***",REPRT
          return
COMSLCT
          move      "**CC: CONSUMER DIRECT",COMSLCT
          return
. LIFESTYL - LIFESTYLE OVERLAY.
LIFESTYL
          move      "CC:LIFESTYLE SELECTOR",COMSLCT
          return
.
ICSYSTEM
          move      "**CC: IC SYSTEMS **",COMSLCT
          return

prtordfrm
          call      PrtOpenFile
          clear     str2
          pack      str2,OSALES10,OSALES
          if (OformBflag = "Y" & (str2 = "06" | str2 = "19" | str2 = ""| str2 = "27" | str2 = "28" ))          .list management
                    call      prtordfrmGuiA
          else
                    call      prtordfrmGuiB
          endif
          return

PrtOpenFile
.printing  not faxing - no or invalid fax number
          if (FirstFlag = YES)
                    call      PrtCloseFile
.                   if (PDFFlag = 1)
                    if        (hotFlag = C2)             .hotprint
                              PRTOPEN   Laser,"PDF995",inpname
                              return
                    endif
                    
.begin patch 9.75
.                    if        (PDFFname = "")
.print live run office copies straight to the printer
                    if        (PDFFname = "" & Formflag <> 4)         .if office we will go to printer
                    alert     caution,"Location PrtOpenFIle no PDFFname",result
                    endif
          if        (hotflag = c1 & FOrmflag = 4)               .live & office copies
          PRTOPEN Laser,"\\nts0\laser8","officecopies.prn"
          else
          PRTOPEN Laser,"PDF995",PdfFname
          endif
.end patch 9.75
          TrapClr   SPOOL
          move      NO,FirstFlag
          
          else
.                   prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=1,*Overlayon
                    prtpage   Laser;*NEWPAGE
          endif
          return

PrtCloseFile
          prtclose Laser
          call      Trim using prtname
          if (prtname <> "")
.HotOrders will always have this value filled.  This is for regular run
                    if (hotflag <> 2)
                              if (!externalmode)
.HotOrders should not rename first.  This is for regular run.
                                        pack      taskname,"\\nts0\d\data\fax\",prtname          ."
                                        pack      APIFileName,taskname,hexzero
                                        call      FindFirstFile
                                        if (APIResult <> 0 & APIResult <> hexeight)
                                                  erase     taskname
                                        endif
.....................................
                                        pack      APIFileName,"C:\WORK\Faxfile.prn",hexzero
                                        call      FindFirstFile
                                        if (APIResult <> 0 & APIResult <> hexeight)
                                                  rename   "C:\WORK\Faxfile.prn",taskname
                                        endif
.Following will prevent files from being erased/overwritten
                                        clear     prtname
                              endif
                    else
                              pack      APIFileName,prtname,hexzero
                              call      FindFirstFile
                              if (APIResult <> 0 & APIResult <> hexeight)
                                        erase     prtname
                              endif
                    endif
          endif
          return

ObjError
          Getinfo   Exception,Taskname
          return
DREWBREAK
         NORETURN
         TRAP      IOMssg giving error IF IO
         SCAN      "I03" IN ERROR
         GOTO      NOSMP IF EQUAL
          alert     caution,"Location DrewBreak IO error - Aborting",result
          reset     Error
          alert     caution,Error,result
         SHUTDOWN  "CLS"


DREWTEST
          MOVE      STR1,STR1
          RETURN
RetryPrint
                    move      "Nord0002 May have a spool error.",MailSubjct
                    Clear     MailBody
                    Append    "Please Check Nord0002 to see if it recovered from a spooling error. (Location: Retry Print)",MailBody
                    Append    CRLF,MailBOdy
                    call      EMAILIS
                    PRTOPEN Laser,"PDF995",PdfFname
                    return
.begin patch 9.7
SendItOut
..................................................
          call      PrtCLoseFIle
          if        (faxflag = 2 | EmailFlag1 = Yes | FUNC = "2" | FUNC = "3")
.wait for the PDF's

          pack      Str55 from "c:\work\pdf\",PdfFname,".pdf"
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from str55

          IF        (FUNC = "2" & EmailFlag1 = yes)            .HOTPRINT SEND TO REQUESTOR & Normal recipients
          CLEAR     MailCC
          append    User,MailCC
          append    "@nincal.com",MailCC
          reset     MailCC
          MOve      "ComputerRequest@nincal.com",MailBCC                      .keep copy just incase .. rule on that inbox to move
          ElseIf    (FUNC = "3")            .HOTPRINT SEND TO REQUESTOR ONLY
          CLEAR     MailTO
          append    User,Mailto
          append    "@nincal.com",Mailto
          reset     Mailto
          endif

.for testing        
.                   Scan      "FACSYS",mailto
.                   if        Not equal
.                   REset     Mailto
.                   move      NfulEmail to MailTO
..                  Move      "DHerric@nincal.com",Mailto
.                   Else
.                   REset     Mailto
.                   endif
.for testing        
                    if        (formflag = 1)
                    Move      "Order Confirmation",MailSubjct
                    else
                    Move      "Order Fulfillment",MailSubjct
                    endif
                    MOve      "ComputerRequest@nincal.com",MailFrom                      .keep copy just incase .. rule on that inbox to move
.                   Move      "DHerric@nincal.com",MailFrom
.turn off for testing <<<<<<<<<<<<<
.                   clear     Mailbody
.for testing        
.                   append    NFUlEmail,Mailbody
.                   append    CRLF,Mailbody
.                   append    Str55,Mailbody
.                   append    CRLF,Mailbody
                    reset     Mailbody
.for testing        
..First check 995 autolaunch settings
                    call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
                              "Parameters":
                              "ProcessPDF":
                              "\\nts0\c\apps\plb\code\pdftest.bat":
                              result
                              if (result = C0)
.Prepare Flag file
                                        prep      tempfile,"c:\progra~1\pdf995\flag.dat"
                                        write     tempfile,SEQ;"flag set"
                                        close     tempfile
                              endif
                    Call      PDF995Auto
                    Move      "30",MailTimer
                    call      SendMail
.         after sending copy to folder
          pack      taskname from "\\nts0\d\data\orders\sent\",PdfFname,".pdf"
          copyfile  str55,taskname
          erase     str55
                    return
          Else
          endif
..end patch 9.7
.?????? copy to printer here ?????????????????????????????????????????????????????????/
.I think have winbatch job copy all to the printer
          Clear     PdfFName

          return
.Begin patch 9.7
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
.end patch 9.7
.Begin patch 9.7 
GetFullFill
          call Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "DISRTN-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     COMPVARS
                              clear     NFULNUM
                              clear     NFULCOMP
                              clear     NFULCNT
                              clear     NFULFAX
                              clear     cnctfname
.Begin patch 9.7 
                              Clear     NFulEmail
                              Move      No,EmailFlag
.end patch 9.7 

                    else
                              if (COMPSVBFLG <> "T")
                                        clear     COMPFLD
                                        clear     COMPVARS
                                        clear     NFULNUM
                                        clear     NFULCOMP
                                        clear     NFULCNT
                                        clear     NFULFAX
                                        clear     cnctfname
.Begin patch 9.7 
                                        Clear     NFulEmail
.end patch 9.7 
                              else
                                                            Packkey CNCTFLD2 to "01X",COMPNUM
                                                            Call      CNCTAIM
                                                            loop
                                                            until over
                                                            until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                                                      call      CNCTKG
                                                            repeat
                                        move      COMPNUM,NFULNUM
                                        move      COMPCOMP,NFULCOMP
                                        move      CNCTFNAME,NFULCNT
                                        move      COMPFAX,NFULFAX
.begin patch 9.7
                                        Clear     NFUlEmail
                                        call      Trim using CompEmail
                                        Move      CompEmail,NFULEMAIL

.end patch 9.7 
                              endif
                    endif
          else      .// OFULLFIL = ""
                    clear     COMPFLD
                    clear     COMPVARS
                    clear     NFULNUM
                    clear     NFULCOMP
                    clear     NFULCNT
                    clear     NFULFAX
                    Clear     NFUlEmail
                    Move      No,EmailFlag
                    clear   cnctfname
          endif

          if        (formflag = c3 & NfulEmail <> "")          . I think we have a winner
          Move      Yes,EmailFlag
          else
          Move      No,EmailFlag
          endif

          Return
.end patch 9.7 





.Email I.S.
EMAILIS
.;.   Set the text message that is send with the attachments
          Reset     MailBOdy
          pack      Mailto,"InformationServices@nincal.com"
.         pack      Mailto,"dherric@nincal.com"
          Pack      MailFrom,"ComputerRequest@nincal.com"
.         pack      MailFrom,"dherric@nincal.com"
          call      SendMail
          return

webGenerate routine DimPtr
          set       externalmode
          move      "2",FUNC
          pack      INPNAME from DimPtr,"M","WEB"
          move      "NORD0002",PROGRAM
          call      start
          return

          include   prtorderpage1.inc
.argh
          include   Nspe3io.inc
.argh
          include   compio.inc
          include   cntio.inc
          include   NCRCIO.INC
          include   NRTNIO.INC
          include   NOWNIO.INC
          include   NINVIO.INC
          include   nordio.inc
          include   nspIIO.inc
          include   nspeio.inc
          include   nsmpio.inc
          include   NOFRIO.INC
          include   ncntio.inc
          include   hpio.inc
          INCLUDE   NSEL2io.INC
          INCLUDE   NSEL3io.INC
          INCLUDE   NADDIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NREFIO.INC
          INCLUDE   NMODIO.INC
          include   COMLOGIC.INC

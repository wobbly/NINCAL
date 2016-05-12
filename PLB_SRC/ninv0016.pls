PC         EQU        0
           INCLUDE    COMMON.inc
           INCLUDE    CONS.inc
           INCLUDE    CONSACCT.inc
           INCLUDE    ninvdd.inc
           INCLUDE    NINVAcddd.inc
           include    nacddd.inc
           include    ndatdd.inc
           include    nshpdd.inc
           INCLUDE    NORD2DD.inc
           include   compdd.inc
           include   cntdd.inc
           include    hp.inc
           INCLUDE    NMRGDD.inc
           INCLUDE    NJstDD.inc
           INCLUDE    NADJDD.inc
           INCLUDE    NOWNDD.INC
           include    nbildd.inc
           INCLUDE    NDAT3DD.INC
           include    winapi.inc
           INCLUDE    NSEL2DD.INC
           include    nmoddd.inc
           INCLUDE    NSELDD.INC
           inc        nsel3dd.inc
           inc        nrefdd.inc
           inc        nadddd.inc
           inc        nsltdd.inc
           Include    PrtPagedd.inc
release    init       "4.44"          DLH     update discount portion for MSF/DCCC see 4.43 for previous changes
reldate    Init       "2015 July 21"
.release  init      "4.43"          DLH     added to footer
.reldate   Init      "2014 November 20"
.release  init      "4.42"          DLH     added Flags= to prtopen with PDF:
.reldate   Init      "2014 March 25"
.release   init      "4.41"             ASH         .Adjustments to allow Datacard Select items on Order to remain independent from current Datacard state
.Reldate   Init      "2013 August 05"
.release   init      "4.40"             DLH         .Sunbelt PDF
.Reldate   Init      "2013 April 23"
.release   init      "4.31"             DLH         .review, move to production.
.Reldate   Init      "24 January 2013"
.release   init      "4.3"             DLH         .Add additional charges on unbilled orders, convert to prtpage
.Reldate   Init      "11 April 2012"
.release   init      "4.2"             DLH         .Spreadsheet rearrangement
.Reldate   Init      "10 April 2012"
.release   init      "4.1"             DLH         .run in Lotus mode as advertised - also producing hardcopy
.Reldate   Init      "29 September 2011"
.release   init      "4.02"             DLH         add logo
.Reldate   Init      " June 2010"
.release   init      "4.01"             DLH         TEST estimate LCR's
.Reldate   Init      "23 March 2010"
.release   init      "4.00"             DLH         remove PL
.Reldate   Init      "3 March 2010"
.release  init      "3.99"             DLH         check Excel Version
.Reldate        Init      "13 April 2000"
.release  init      "3.98.1"                  JD   06Jan2009          Updated Copyrite footer
.Reldate        Init      "Jan 06, 2008"
.release  init      "3.98"                    DLH   verbage per Susan
.Reldate        Init      "31 October 2008"
.release  init      "3.97"                    DLH   various estimate fixes
.Reldate        Init      "12 March 2008"
.release  init      "3.96.1"                  JD   04Jan2008          Updated Copyrite footer
.Reldate        Init      "Jan 04, 2008"
.release        init           "3.96"         DLH 26Sep007 PLI cleanup
.Reldate        Init      "Sept 26, 2007"
.release        init           "3.95"          JD 17SEP2007 new merge var CNR
.Reldate        Init      "Sept 17, 2007"
.release        init           "3.93"         ASH 19JUN2007 PLI Inclusion
.release        init           "3.92"         DMB23JAN2006 Code Added for nins1 File Folder Reorganization
.release        init           "3.91"         JD06OCT2005 new nmrgdd.inc var nmrgdisa
.release        init           "3.9"         ASH  9SEP2005  Patch to account for large spreadsheets
.                                                                               This patch is undocumented - but I replaced
.                                                                               all instances of str4/str5 with a larger variable
.                                                                               when being used to dictate a Range
.Reldate        Init      "Sept 17, 2007"
.release        init           "3.8"         DLH  8March2005          Invoice conversion
.Reldate        Init      "Sept 6, 2005"
.release  init      "3.76.8"         DLH          25APR2005 Correct Dawson price
.Reldate        Init      "April 25, 2005"
.release  init      "3.76.7"         ASH          07APR2005 COMMPER CONVERSION
.Reldate        Init      "April 07, 2005"
.release  init      "3.76.6"         ASH          24FEB2005 Patch to change BOLD setting
.Reldate        Init      "February 24, 2005"
.release  init      "3.76.5"         ASH          01FEB2005 Patch to prevent Object error if accidentally submit job w/ ALL LCRs
.Reldate        Init      "February 1, 2005"
.release  init      "3.76.4"         ASH          20SEP2004 Logo Conversion
.Reldate        Init      "September 20, 2004"
.release  init      "3.76.3"         ASH          04AUG2004 First round of changes from Gemma S.
.Reldate        Init      "August 04, 2004"
.release  init      "3.76.2"         DLH          29JUL2004 Updated Copyrite footer
.Reldate        Init      "July 29, 2004"
.release  init      "3.76.1"         ASH          27JUL2004 Added new language from SMM/NP
.Reldate        Init      "July 24, 2004"
.release  init      "3.76"         JD 09JUn2004  Fix required trim of datacard var Commper
.Reldate        Init      "June 9, 2004"
.release init    "3.75"           01JUN04 ASH     CTF CASE #8 - Added Rental Price display if split/exchange.
.Reldate        Init      "June 1, 2004"
.release  init      "3.74"        DMB   26MAY2004 Mailer Conversion
.Reldate        Init      "May 28, 2004"
.release init    "3.73"           28JAN04 ASH     DATACARD CONVERSION
.Reldate        Init      "January 28, 2004"
.                                       27MAY2004 - ASH - ADDED PATCH TO REMOVE COMMANDBARS - UNNECESSARY AND MESSING UP NTS2
.release init    "3.72"           02JUL03 ASH     Trimmed Select Name in attempt to prevent "tripling" of cell lines
.                                                           Added a bit to List/Select Name Column Width as Excel sometimes Autofits the columns too little
.Reldate        Init      "July 2, 2002"
.release init    "3.71"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release        init      "3.7"               15NOV02 ASH ADDED FEATURES
.release        init      "3.6"               24July 2002 DLH Change placement of Data & add list select to spreadsheet - AGAIN!!!
.                                            ALso deleted old comments -- see archived version 3.53 for old code.
.Reldate        Init      "24 July 2002"
.............................................................................
.EXTERNAL ROUTINES FROM       NDAT001a.PLC
SelectTestBase4 external "NDAT001a;SelectTestBase4"
N52       form      5.2
result2   form      9
xlUnderlineStyleSingle      integer 4,"0x2"
.to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel

. WORK VARIABLES
books          automation
book           automation
sheets         automation
sheet          automation
rowcol         automation
ex             automation      class="Excel.Application"
RecordHeader   form           9
RecordTop      form           9
N34            form           3.4
N92            form           9.2
MailDate       dim            4
SReturn        init 0x0a                     ;soft return/line feed
VT_BOOL        EQU            11          .Boolean
OTRUE          variant
OFALSE         variant
VT_I4          EQU            3           .4 byte integer
Zoom90         variant
Zoom75         variant
Zoom72         variant
Zoom80         variant
Inch1286       variant
VT_R8          EQU            5           .Double - 8 byte Real
Inch03         variant
Inch25         variant         ..25 Inch
Inch50         variant         ..50 Inch
Inch70         variant         ..70 Inch
Inch100        variant         .1.0 Inch
xlColWidth     variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
xlHAlignLeft   integer 4,"0xffffefdd"             .-4131
xlHAlignCenter integer 4,"0xffffeff4"           .-4108
xlEdgeTop      integer 4,"0x8"                       .8
xlEdgeBottom   integer 4,"0x9"                    .9
xlEdgeRight    integer 4,"0xA"                     .10
xlContinuous   integer 4,"0x1"                    .1 - Doubles for xlPaperLetter, xlDownThenOver
xlDouble       integer 4,"0xffffefe9"                 .-4119
xlLandscape    integer 4,"0x2"                     .2
xlPrintNoComments integer 4,"0xffffefd2"        .-4142
XlVAlignTop    Integer  4,"0xFFFFEFC0"          .-4160
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom75,VarType=VT_I4,VarValue=75
        create  Zoom72,VarType=VT_I4,VarValue=72
        create  Zoom80,VarType=VT_I4,VarValue=80  .GS first change from original specs
        create  Zoom90,VarType=VT_I4,VarValue=90
.Convert Inches to Points.  There are 72 Points to an Inch
        create  Inch03,VarType=VT_R8,VarValue="3"
        create  Inch25,VarType=VT_R8,VarValue="18"      ..25 Inch
        create  Inch50,VarType=VT_R8,VarValue="36"      ..5 Inch
        create  Inch70,VarType=VT_R8,VarValue="51"      ..7 Inch
        create  Inch100,VarType=VT_R8,VarValue="72"     .1.0 Inch
        create  Inch1286,VarType=VT_R8,VarValue="12.86"
          create    xlColWidth,VarType=VT_R8,VarValue="1.0"
RangeStr  dim       12
RangeStr2 dim       12
FileCheck FIle
trapcount form      4
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
        create  font1,"Times New Roman",size=9
        create  font2,"Times New Roman",size=9,bold
        create  font3,"Times New Roman",size=9,italic
        create  font4,"Times New Roman",size=10
        create  font5,"Times New Roman",size=10,bold
        create  font6,"Times New Roman",size=10,italic
        create  font7,"Times New Roman",size=7


JOBFLAG        FORM      1                   1=PRINT list cost report
.                                            2=LOTUS FLAT FILE Billing Summary with every report
.                                            3=Billing Summary standard batch
.                                            4=Billing Summary standard batch/adj
.                                            5=                 WITH NETS
remtflag       form      1                  1=nothing, 2=print remitance on job=3,4
eojflag        form      1                  1=no, 2=yes
PAGE           FORM      "000"
PAGENUM        DIM       3
ORDMASK        INIT       "ZZZ,ZZ9,999"
ORDMASK10      INIT       "Z,ZZZ,ZZZ,ZZ9"
mask11         init       "ZZ,ZZZ,ZZ9,999"
bqtymask       dim       13
bcstmask       dim       6
mask6          init      "$$9.99"
ORDQTY         DIM       11
ORDQTY10       DIM       13
billqty        form      9
shpmask        init      "ZZZ,ZZ9,999"
shpQTY         DIM       11
PAGEMASK       INIT      "ZZ9"
LINES          FORM      2
PERCENT        FORM      4.2
percout        form      3.2
BILL           DIM       1
DISDATE        DIM       8
nonmail        form      9
costout        form      7.2
gtcost         form      7.2
costsplt       form      7.2
SPLTFLAG       FORM      1
TOTALDOL       FORM      7.2
TOTALOPN       FORM      7.2
totnames       form      11                     .total names "Ordered"
totnbill       form      11                     .total names billed
totinmrg       form      9                      .total names into merge
FARsplt        FORM      7.2
gtrej          form      10
gt92           form      9.2
gtnet          form      8
gt94           form      9.4
.
ADJAR          FORM      8.2
adjarh         form      8.2
UNBILAMT       FORM      9.2
FORM96         FORM      9.6
F96splt        FORM      9.6
F52splt        FORM      5.2
FORM52         FORM      5.2            cost per m
form52pm       form      5.2
NUM10          FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
SPLITSW        DIM       1                  LIST MANAGEMENT INDICATOR.
SYSJDATE       FORM      5
UNBILINC       FORM      9.2
FORM7          FORM      7
MDATE          FORM      5
mfp            form      2
mll            form      2
charge         form      3.2
maledate       form      5
JUNDATE        FORM      6
QTYCHK         FORM      9
CHKMM          FORM      2                  HOLDS MONTH PARAMETER
TENDOLL        FORM      "99999"
NINEDOLL       FORM      "199999"
SEVDOLL        FORM      "300000"
JUNEDAT        INIT      "060191"
INVDATE        FORM      5
PAYDATE        FORM      5
FUNCBR         FORM      "0"       PROGRAM CONTROL BRANCH.
QTYFLAG        FORM      1         QTY FLAG, 1=NO ORDER QTY, 2=YES ORDER QTY
PSTATUS        DIM       6
PAID           DIM       1
MRGINFO        DIM       12
LOCAL          INIT      "LOCAL"
MRGSW          DIM       1    "Y" IF MERGE INFO
SHIPSW         DIM       1    "Y" IF SHIPPING INFO
holdmlr        dim      4
runc94         form     9.4
MoreAR         form     9.4
CalcAR         form     9.4
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
DATE           DIM       8
countin        form      5                  total records read
COUNTO1        FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI         FORM      5                  NUMBER OF INVOICES READ
COUNTI1        FORM      5                  NUMBER OF INVOICES CALCULATED
TOTSUM         FORM      3
TotLCR         Form      3                       Number of LCRS
Num9           FORM      9
Num9b          FORM      9
TOTAL          FORM      9.2
TOTALU         FORM      9.2
TOTALB         FORM      9.2
TOTALP         FORM      9.2
arout          dim       15
armask         init      "$$$,$$$,$$$.99-"
dolrmask       init      "$$$,$$$,$$$.99-"
totpmask       dim       15
totbmask   dim       15
totmask    dim       15
balmask    dim       15
costmask   dim       15
trejmask   dim       13            prtdet4 - non mailable names
qoutmask   dim       13            prtdet4 -output qty
ppmmask    init      "$$,$$$.99"
ppmout     dim       9
mess1      dim       5
mess3      dim       3
mess2      dim       11
F94        FORM      9.4
F32        FORM      3.2
F34        FORM      3.4
F34splt    FORM      3.4
code16SW   dim       1
QTYadjSW   dim       1
bigdate    dim       25
bigdat2    dim       25
M01        INIT      "January"
M02        INIT      "February"
M03        INIT      "March"
M04        INIT      "April"
M05        INIT      "May"
M06        INIT      "June"
M07        INIT      "July"
M08        INIT      "August"
M09        INIT      "September"
M010       INIT      "October"
M011       INIT      "November"
M012       INIT      "December"
str100     Dim        100
HOLDEXCL   dim        1
LastRec    form       9
.
. .............................................................................
. MAINLINE
. .............................................................................
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................

.         ALERT     NOTE,"HARD-CODED VARS!!!",RESULT
.         MOVE      "NINV0016",PROGRAM
.         MOVE      "1",COMPANY
.         MOVE      "CREQUES",USER
.         MOVE      "DISKINAH.DAT",INPNAME
.         MOVE      "DISKINAH.LST",PRTNAME
.         move       "LOTUS" to comment

         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NINV0016" TO PROGRAM
         MOVE      "DISKIN" TO INPNAME
         MOVE      "Produce listcost report" TO PRTNAME
         MOVE      "Names in the News" TO COMPNME
         move       "LOTUS" to comment
         ENDIF
         TRAP      EXIT IF F5
         MOVE      "EXIT" TO PF5
         MATCH     "        " TO TODAY
         IF        EQUAL
         call      GETDATE
         ELSE
         IF        EOS
         call      GETDATE
         ENDIF
         ENDIF
START
         move      c1 to ndatpath
         MOVE      C1 TO Ninvpath
         MOVE      C1 TO NMLRPATH
         MOVE      C0 TO TOTALDOL
         MOVE      "LIST COST REPORT" TO STITLE
         MOVE       C1 TO JOBFLAG
         SCAN      "ORDQTY" IN COMMENT
         IF        EQUAL
         MOVE      C2 TO QTYFLAG
          MOVE       C2 TO JOBFLAG
          ELSE
         MOVE      C1 TO QTYFLAG
         ENDIF
         MOVE       C1 TO remtFLAG
         RESET     COMMENT
         SCAN      "LOTUS" IN COMMENT
         call       optlots if equal
         reset     comment
         SCAN      "NETS" IN COMMENT
         call       optNETS if equal
         RESET     COMMENT
         SCAN      "SUM" IN COMMENT
         call       optSUM if equal
         RESET     COMMENT
         SCAN      "REMIT" IN COMMENT
         call       optremt if equal
         goto       prtget

optlots  MOVE       C2 TO JOBFLAG
         return
optNets  MOVE       C5 TO JOBFLAG
         return
optsum   move       c3 to jobflag
         return
optremt  move       c2 to remtflag
         return
PRTGET
         MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MATCH     LOCAL  TO PRTNAME
         GOTO      begin IF EQUAL
         PACK      PRTFILE WITH PRTNAME
         compare   c2 to remtflag
         if        equal
         clear     prtfile
         move      "R" to str1
         PACK      PRTFILE WITH pdrive,PRTNAME,str1
         endif
         BRANCH    JOBFLAG TO PRTOK,Prtok,prtok,prtok,prtok
PRTOK    
          clear     str45
          move      inpname,str8
          pack      str45 from str8,".pdf"
          move      str45,prtname
          pack      str55 from "c:\work\pdf\",str45         ."
          PRTOPEN   Laser,"PDF:",str55,Flags=PDF_FLAGS_WIN_ANSI_ENCODING
          PRTPAGE   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*portrait:
                    *MarginL=0,*MarginT=0:
                    *Alignment=*Left,*font=prtpg85
         BRANCH    JOBFLAG TO Begin,output,begin,begin,begin
         GOTO       begin
OUTPUT
.Open Excel application
        create  ex
.        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.DH testing July 2010
        setprop ex,*Visible="False",*IgnoreRemoteRequests="True",*Interactive="False"
.DH testing
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.get exel version info
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
        setprop sheet.PageSetup,*PrintComments=xlPrintNoComments
        setprop sheet.PageSetup,*Draft=OFALSE
        setprop sheet.PageSetup,*PaperSize=xlContinuous
        setprop sheet.PageSetup,*Order=xlContinuous
        setprop sheet.PageSetup,*BlackAndWhite=OFALSE
        setprop sheet.PageSetup,*LeftMargin=Inch25
        setprop sheet.PageSetup,*RightMargin=Inch25
        setprop sheet.PageSetup,*TopMargin=Inch70
        setprop sheet.PageSetup,*BottomMargin=Inch100
        setprop sheet.PageSetup,*HeaderMargin=Inch50
        setprop sheet.PageSetup,*FooterMargin=Inch50
.
...................................
        if (OUTNAME = "2")
                setprop sheet.PageSetup,*PrintArea="$A:$Q"
                setprop sheet.PageSetup,*Zoom=Zoom75
        elseif (OUTNAME = "1")
                setprop sheet.PageSetup,*PrintArea="$A:$N"
                setprop sheet.PageSetup,*Zoom=Zoom90
          else
                setprop sheet.PageSetup,*PrintArea="$A:$Q"
                    setprop sheet.PageSetup,*Zoom=Zoom80
        endif
        goto       begin
.
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         return
.
BEGIN    CALL      PAINT
         CALL      FUNCDISP
         DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
         display   *P15:05,COMMENT
         display   *P15:06,inpname
         display   *P15:07,prtname
         MOVE      INPNAME TO NORDNAME
          Move      c3,Ndatlock                       .no updates happening do not lock the files
          Move      c3,Nordlock
         
LOOP     DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
         CALL      NORD2SEQ
         if        over
         move      c2 to eojflag
         goto      eoj
         endif
         if         (olrn = "783410" or olrn = "777027")
         call       debug
         endif
   packkey          NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if not over
                    move      NSEL2NAME,O2DES
          else
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
          endif
.                  .is an be an issue as default pickoff does not excludes LCR's
         move      "lzpx" to str4
         scan      ostat in str4
         goto      loop if equal
         move      olnum to ndatfld
         REP       ZFILL IN NDATFLD
         move      c1 to ndatpath
         call      ndatkey
         move      c1 to eojflag
         add       c1 to countin
         display   *p15:09,countin
         compare   c1 to countin
         if        equal
         PACK       MKEY FROM OMLRNUM,OCOBN
         REP       " 0" IN MKEY
         CALL       NMLRKEY
          move      CompExcl,HOLDEXCL
         cmatch     "A" to mcode     .batch & adjust?
         if         equal
         compare    c3 to jobflag
         if         equal
         move       c4 to jobflag    .yes       was turned on 6/13  DLH
         endif
         endif
         endif

.ARGH
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         PACK      NBRKFLD FROM oBRKNUM,oBRKCNT
         CMATCH    B1 TO NBRKFLD
         goto      GOon IF EOS
         call      nbrkkey
         move      BRaddr to biladdr
         move      BRcity to bilcity
         move     BRstate to bilstate
         move     BRzip to bilzip
         goto     goflag
goon
         MOVE      MADDR TO BILADDR
         MOVE      MCITY TO BILCITY
         MOVE      MSTATE TO BILSTATE
         MOVE      MZIP TO BILZIP
goflag   branch    JOBFLAG of mlrchk,check1,check1,check1,mlrchk
mlrchk   match     omlrnum to holdmlr
         call      newmlr if not equal
CHECK1   move      c1 to spltflag
         move      b5 to mess1
         cmatch    "1",oelcode
         if        equal
         move      "RENT" to mess1
         goto      exexit
         endif
         CMATCH    "2",OELCODE
         GOTO      CHECKOK IF EQUAL
CHECK2
         CMATCH    "3",OELCODE
         GOTO      CHECKOK IF EQUAL
         move      "RENT" to mess1
         GOTO      exexit
CHECKOK
         MOVE      C0 TO Num9b
         MOVE      OEXQTY TO Num9b
         COMPARE    C0 TO Num9b
         GOTO       CHECKOK1 IF NOT EQUAL
         move      "EXCH" to mess1
         GOTO      exexit
CHECKOK1
           move      "SPLIT" to mess1
         move      c2 to spltflag
exexit   RESET     RUNCODES
.
         SCAN      OLNUM IN RUNCODES
         GOTO       LOOP IF EQUAL
         CMATCH    "X" TO OSTAT
         GOTO      LOOP IF EQUAL
         ADD       C1 TO FORM7
         COMPARE   C0 TO page
         CALL      HD0 IF EQUAL              *1ST RECORD PRINT HEADER.
         CALL      PRTREC
         GOTO      LOOP
HD0
         BRANCH    QTYFLAG OF HD1,HD2
.
HD1      COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
         branch     JOBFLAG to hdok,hdok,hdok3,hdok4,hdok
hdok
          Move      "500",row
          if        (page > 1)
          PRTPAGE   Laser;*NewPage
          endif
          PRTPAGE   Laser;*p=250:row,*font=font1,"CONFIDENTIAL":
                              *p=5750:row,*font=font2,"Names in the News",*font=font1
          add     Sixlpi,row
          PRTPAGE   Laser;*p=5750:row,"180 Grand Ave Ste 1665"
          add     Sixlpi,row
          PRTPAGE   Laser;*p=5750:row,"Oakland, Ca 94612-3716"
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"Date: ",TODAY
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"Client: ",ORDCNAME
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"LIST COST REPORT"
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*ULON,"List Name/Select",*p=3500:row,"Lr##     ",*p=4125:row,"Cost         ":
                    *p=5125:row,"Mlr Po      ",*p=6075:row,"Maildate",*p=7000:row,"M/P ##",*ULOFF
          add     Sixlpi,row
          add     Sixlpi,row
          goto      HDout
hd1exit
         RETURN
.
HD2      COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
          Move      "500",row
          if        (page > c1)
          PRTPAGE   Laser;*NewPage
          endif
          PRTPAGE   Laser;*p=250:row,*font=font1,"CONFIDENTIAL":
                              *p=5750:row,*font=font2,"Names in the News",*font=font1
          add     Sixlpi,row
          PRTPAGE   Laser;*p=5750:row,"180 Grand Ave Ste 1365"
          add     Sixlpi,row
          PRTPAGE   Laser;*p=5750:row,"Oakland, Ca 94612-3716"
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"Date: ",TODAY
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"Client: ",ORDCNAME
          add     Sixlpi,row
          PRTPAGE   Laser;*p=500:row,"LIST COST REPORT"
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*ULON,"List Name/Select",*p=3500:row,"Lr##     ",*p=4125:row,"Cost       ":
                    *p=5125:row,"Mlr Po     ",*p=6075:row,"Maildate/M/P ##/QTY",*ULOFF
          add     Sixlpi,row
          add     Sixlpi,row
         MOVE      C10 TO LINES
          goto      HDout
         RETURN
hdok3
         unpack    today into mm,str1,dd,str1,yy

         MOVE      MM TO N2
         LOAD      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                                      m07,m08,m09,m010,m011,m012
         MOVE      dd TO STR2
         RESET     STR2 TO 1
         SETLPTR   STR2 TO 1
         REPLACE   "0 " IN STR2
         SETLPTR   STR2
         CLEAR     bigDATE
         APPEND    STR9  TO bigDATE
         APPEND    B1    TO bigDATE
         APPEND    STR2  TO bigDATE
         APPEND    B1    TO bigDATE
         APPEND    ",19" TO bigDATE
         APPEND    yy    TO bigDATE
         RESET     bigDATE
         MOVE      C13 TO LINES
         add       c3 to lines
          Move      "500",row
          if        (page <> c0)
          PRTPAGE   Laser;*NewPage
          endif
          prtpage   Laser;*Pictrect=*off,*PICT=0:760:5350:10100:NINLogo                 .95%
          PRTPAGE   Laser;*p=2750:row,*font=font2,"BILLING SUMMARY REPORT"
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row

         MOVE      omdtem TO N2
         LOAD      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                                      m07,m08,m09,m010,m011,m012
         MOVE      omdted TO STR2
         RESET     STR2 TO 1
         SETLPTR   STR2 TO 1
         REPLACE   "0 " IN STR2
         SETLPTR   STR2
         CLEAR     bigDAT2
         APPEND    STR9  TO bigDAT2
         APPEND    B1    TO bigDAT2
         APPEND    STR2  TO bigDAT2
         APPEND    B1    TO bigDAT2
         APPEND    "," TO bigDAT2
         call      TRIM using OMDTEC
         count     n1,OMDTEC
         if (n1 = 0)
                   APPEND    CC TO bigDAT2
         else
                   APPEND    OMDTEC TO bigDAT2
         endif
         APPEND    omdtey    TO bigDAT2
         RESET     bigDAT2
          PRTPAGE   Laser;*p=1:row,*font=font2,"Mailer Name: ",*p=1500:row,ORDCNAME,*p=4500:row,"Billing Date:",*p=4500:row,BigDate
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*font=font2,"Mailer##: ",*p=1500:row,omlrnum,*p=4500:row,"Mailer P.O. No.:",*p=6000:row,omlrpon;
          add     Sixlpi,row

         cmatch    yes to mbildrct            .bill direct?  14apr95 dlh
          if       not equal
          match     b1 to ordmname
                  if        not equal
          PRTPAGE   Laser;*p=1:row,*font=font2,"Through ",*p=1500:row,ordmname:
                    *p=4500:row,"Merge ##:",*p=6000:row,omlrky
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,"Ship To:",*p=1500:row,BILADDR:
                    *p=4500:row,"Mail Date",*p=6000:row,BigDat2
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1500:row,BILCITY,b2,Bilstate,b1,BilZip
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
                 add        c3 to lines
                 else
                         goto      hdok3a
                 endif
         else
                 goto      hdok3a
         endif
         goto     hdok3b
hdok3a
          PRTPAGE   Laser;*p=1:row,*font=font2,*p=4500:row,"Merge ##:",*p=6000:row,omlrky
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,"Ship To:",*p=1500:row,BILADDR:
                    *p=4500:row,"Mail Date",*p=6000:row,BigDat2
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1500:row,BILCITY,b2,Bilstate,b1,BilZip
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
         add        c3 to lines
hdok3b
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*font=font2,"List",*p=750:row,"  Input",*p=2000:row,"List ":
                    *p=5125:row,"Total",*p=6000:row,"    Qty",*p=6750:row,"   Base"
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*ULON,"Rental ##",*p=750:row,"  Qty",*p=2000:row,"Description":
                    *p=5125:row,"Billed",*p=6000:row,"    Billed",*p=6750:row,"   Price  ",*ULOFF,*Font=Font1
          goto      HDOut
         RETURN
.
hdok4
         move      olrn to ninvfld
         move      c1 to ninvpath
         call      ninvkey
         unpack    today into mm,str1,dd,str1,yy
         MOVE      mm TO N2
         LOAD      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                                      m07,m08,m09,m010,m011,m012
         MOVE      dd TO STR2
         RESET     STR2 TO 1
         SETLPTR   STR2 TO 1
         REPLACE   "0 " IN STR2
         SETLPTR   STR2
         CLEAR     bigDATE
         APPEND    STR9  TO bigDATE
         APPEND    B1    TO bigDATE
         APPEND    STR2  TO bigDATE
         APPEND    B1    TO bigDATE
         APPEND    ",19" TO bigDATE
         APPEND    yy    TO bigDATE
         RESET     bigDATE
         MOVE      C13 TO LINES
         add       c3 to lines
          move      "500",Row
          PRTPAGE   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Landscape:
                    *MarginL=0,*MarginT=0:
                    *Alignment=*Left,*font=prtpg85
          prtpage   Laser;*Pictrect=*off,*PICT=0:760:5350:10100:NINLogo                 .95%
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=4125:row,*font=font2,"BILLING SUMMARY REPORT"
          add     Sixlpi,row


         MOVE      omdtem TO N2
         LOAD      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                                      m07,m08,m09,m010,m011,m012
         MOVE      omdted TO STR2
         RESET     STR2 TO 1
         SETLPTR   STR2 TO 1
         REPLACE   "0 " IN STR2
         SETLPTR   STR2
         CLEAR     bigDaT2
         APPEND    STR9  TO bigDAT2
         APPEND    B1    TO bigDAT2
         APPEND    STR2  TO bigDAT2
         APPEND    B1    TO bigDAT2
         APPEND    "," TO bigDAT2
         call      TRIM using OMDTEC
         count     n1,OMDTEC
         if (n1 = 0)
                   APPEND    CC TO bigDAT2
         else
                   APPEND    OMDTEC TO bigDAT2
         endif
         APPEND    omdtey  TO bigDAT2
         RESET     bigDAT2
          add     Sixlpi,row
          add     Sixlpi,row
          PRTPAGE   Laser;*p=4125:row,*font=font2,"Mailer Name: ",*p=1500:row,ORDCNAME,*p=6000:row,"Billing Date:":
                   *p=7500:row,bigdate
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,"Mailer##: ",*p=1500:row,omlrnum:
                   *p=6000:row,"Mailer P.O. No.:",*p=7500:row,omlrpon

         cmatch    yes to mbildrct            .bill direct?  14apr95 dlh
          if       not equal
          match     b1 to ordmname
                  if        not equal
          PRTPAGE   Laser;*p=1:row,"Through ",*p=1500:row,ordmname:
                    *p=6000:row,"Merge ##:",*p=7500:row,omlrky
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,"Ship To:",*p=1500:row,BILADDR:
                    *p=6000:row,"Mail Date:",*p=7500:row,bigdat2          
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1500:row,BILCITY," ",BILSTATE," ",BILZIP
          add     Sixlpi,row
                 add        c3 to lines
                 else
                         goto      hdok4a
                 endif
         else
                 goto      hdok4a
         endif
         goto     hdok4b
hdok4a
          PRTPAGE   Laser;*p=6000:row,"Merge ##:",*p=7500:row,omlrky
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,"Ship To:",*p=1500:row,BILADDR:
                    *p=6000:row,"Mail Date:",*p=7500:row,bigdat2          
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1500:row,BILCITY," ",BILSTATE," ",BILZIP
          add     Sixlpi,row

         add        c3 to lines
hdok4b 
          PRTPAGE   Laser;*p=7125:row,*font=font2,*ULON,"Base",*p=6000:row,*p=9500:row,"     Output"
          add     Sixlpi,row
          PRTPAGE   Laser;*p=1:row,*ULON,"List",*p=750:row,"Input",*p=1500:row,"List ":
                   *p=4125:row,"Total",*p=5000:row,"   Qty",*p=5750:row,"   Base",*p=6125:row,"  Deductible":
                   *p=7125:row,"Cost",*p=7750:row,"  Output",*p=8750:row,"   %",*p=9500:row,"     Cost"
          PRTPAGE   Laser;*p=1:row,*ULON,"Rental ##",*p=750:row,"Qty",*p=1500:row,"Description":
                   *p=4125:row,"Billed",*p=5000:row,"   Billed",*p=5750:row,"   Price",*p=6125:row,"  Names":
                   *p=7125:row,"Savings",*p=7750:row,"  Qty",*p=8750:row,"Output",*p=9500:row,"     Per M         ":
                   *ULOFF,*font=Font1

          goto      hdout

         RETURN

hdout
.Report Header
          if        (page > 1)
          return
          endif
        move    C0,RecordTop
        move    C0,RecordHeader
        move    howmany,str9       ."1"
          
        call    Trim using str9
        pack    RangeStr,"A",str9
.
        add     C1,howmany
        move    howmany,str9       ."2"
        Move      C6,str9
        call    Trim using str9
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Formula="=TODAY()",*NumberFormat="dd-mmm-yy"
.
        add     C3,howmany
        move    howmany,str9       ."5"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value=MCOMP
        setprop sheet.range(RangeStr).Font,*Bold="True"
.
        add     C1,howmany
        move    howmany,str9       ."6"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="Through:"
        pack    RangeStr,"B",str9
        rep     lowup,MBILDRCT
        if (MBILDRCT = YES)
                setprop sheet.range(RangeStr),*Value=MNAME
        else
                setprop sheet.range(RangeStr),*Value=BRCOMP
        endif
.
        add     C1,howmany
        move    howmany,str9       ."7"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="List Acquisition"
.
        add     C1,howmany
        move    howmany,str9       ."8"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="Mailer ##:"
        pack    RangeStr,"B",str9
        setprop sheet.range(RangeStr),*Value=OMLRNUM,*HorizontalAlignment=xlHAlignCenter

          if (OUTNAME = "0")  .Includes Rental Savings Portion
          pack    RangeStr,"L",str9
                  setprop sheet.range(RangeStr),*Value="e o      exchange list only"
          endif
.
        add     C1,howmany
        move    howmany,str9       ."9"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="Merge ##:"
        pack    RangeStr,"B",str9
        call    Trim using OMLRKY
        setprop sheet.range(RangeStr),*Value=OMLRKY,*HorizontalAlignment=xlHAlignCenter
.
        add     C1,howmany
        move    howmany,str9       ."10"
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="Mail Date:"
        clear   str10
        call    Trim using OMDTEM
        if (OMDTEM <> "")
                pack    str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                pack    MailDate,OMDTEM,OMDTEY
        else
                unpack  today into mm,str1,dd,str1,yy
                pack    MailDate,MM,YY
        endif
        pack    RangeStr,"B",str9
        setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="dd-mmm-yy"
.Record Header
        add     C1,howmany
        move    howmany,RecordHeader    .save this value for formatting later
        move    howmany,str9       ."11"
        call    Trim using str9
..........
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Value="TOTAL"
.beguin patch 4.44
..........................................
.        if (OUTNAME = "2" | OUTNAME = "1")
        if (OUTNAME = "1")
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="NON-"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="BASE"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
.
                    add     C1,howmany
                    move    howmany,str9       ."12"
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="ORDER ##"
...................................................
                    pack    RangeStr,"B",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE ##"
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="INPUT"
                    pack    RangeStr,"E",str9
                    setprop sheet.range(RangeStr),*Value="LIST"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="NET"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="AMOUNT"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="BASE"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="BILLABLE"

                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="EXCH/"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="NIN COMM."
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="GROSS"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="*PAID*"
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE"
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value="PAYMENT"
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value="MAILER"
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value="THRU"
                    pack    RangeStr,"X",str9
                    setprop sheet.range(RangeStr),*Value="MERGE"
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value="MAIL"
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value="LIST ##"
.
                    add     C1,howmany
                    move    howmany,str9       ."13"
                    call    Trim using str9
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="ORDERED"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="PRICE"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="NAMES"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="SAVINGS"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="PER M"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="SPLIT"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="DIFF."
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value="DATE"

                    Elseif  (outname = "2")
                    
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="ADDT'L COST"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="Discounted-"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="NON-"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="BASE"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="RENT/"

.
                    add     C1,howmany
                    move    howmany,str9       ."12"
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="ORDER ##"
...................................................
                    pack    RangeStr,"B",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE ##"
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="INPUT"
                    pack    RangeStr,"E",str9
                    setprop sheet.range(RangeStr),*Value="LIST"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="NET"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="AMOUNT"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="BASE"

                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="ADJUSTED"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="IF ALL ON"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="Price"

                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="BILLABLE"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="EXCH/"
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value="NIN COMM."
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value="GROSS"
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value="*PAID*"
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE"
                    pack    RangeStr,"X",str9
                    setprop sheet.range(RangeStr),*Value="PAYMENT"
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value="MAILER"
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value="THRU"
                    pack    RangeStr,"AA",str9
                    setprop sheet.range(RangeStr),*Value="MERGE"
                    pack    RangeStr,"AB",str9
                    setprop sheet.range(RangeStr),*Value="MAIL"
                    pack    RangeStr,"AC",str9
                    setprop sheet.range(RangeStr),*Value="LIST ##"
                    pack    RangeStr,"AD",str9
                    setprop sheet.range(RangeStr),*Value="LIST CODE"

                    add     C1,howmany
                    move    howmany,str9       ."13"
                    call    Trim using str9
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="ORDERED"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="PRICE"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="PRICE"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="RENTAL"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="SAVINGS"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="NAMES"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="SAVINGS"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="PER M"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="SPLIT"
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value="DIFF."
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"AB",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
.
          else
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="ADDT'L COST"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="NON-"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="BASE"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="RENT/"
.
                    add     C1,howmany
                    move    howmany,str9       ."12"
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="ORDER ##"
...................................................
                    pack    RangeStr,"B",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE ##"
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="INPUT"
                    pack    RangeStr,"E",str9
                    setprop sheet.range(RangeStr),*Value="LIST"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="NET"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="AMOUNT"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="BASE"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="ADJUSTED"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="IF ALL ON"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="BILLABLE"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="COST"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="EXCH/"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="NIN COMM."
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value="GROSS"
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value="*PAID*"
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value="INVOICE"
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value="PAYMENT"
                    pack    RangeStr,"X",str9
                    setprop sheet.range(RangeStr),*Value="MAILER"
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value="THRU"
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value="MERGE"
                    pack    RangeStr,"AA",str9
                    setprop sheet.range(RangeStr),*Value="MAIL"
                    pack    RangeStr,"AB",str9
                    setprop sheet.range(RangeStr),*Value="LIST ##"
                    pack    RangeStr,"AC",str9
                    setprop sheet.range(RangeStr),*Value="LIST CODE"
                    pack    RangeStr,"AD",str9
                    setprop sheet.range(RangeStr),*Value="Mailer"

.
                    add     C1,howmany
                    move    howmany,str9       ."13"
                    call    Trim using str9
                    pack    RangeStr,"C",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"F",str9
                    setprop sheet.range(RangeStr),*Value="ORDERED"
                    pack    RangeStr,"G",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"H",str9
                    setprop sheet.range(RangeStr),*Value="BILLED"
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value="PRICE"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value="PRICE"
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Value="RENTAL"
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value="NAMES"
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Value="SAVINGS"
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value="QTY"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value="OUTPUT"
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Value="PER M"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value="SPLIT"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value="%"
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value="DIFF."
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"AA",str9
                    setprop sheet.range(RangeStr),*Value="DATE"
                    pack    RangeStr,"AD",str9
                    setprop sheet.range(RangeStr),*Value="LIST CODE"
          endif
        add     C1,howmany,RecordTop    .save this value for formatting later
        return
..............................................................................
PAGENUM  branch    jobflag of pg55,pg55,pg58,pg58,pg55
pg55     
          Move      "10600",row
         goto      pagenum1
pg58     
          Move      "8250",row
         goto      pagenum1
         goto      pagenum1
pagenum1 MOVE      PAGEMASK TO PAGENUM
         EDIT      PAGE TO PAGENUM
         BRANCH    JOBFLAG TO PGOK,PGok,pgok4,pgok4,pgok
PGOK
          add       Sixlpi,row
          PrtPage   Laser;*p=1:row,*font=font1,CopyRightS,"1992-2016, Names in the News",*p=4000:row,Pagenum
         goto      skippg
PGOK4
          add       Sixlpi,row
          PrtPage   Laser;*p=1:row,*font=font1,CopyRightS,"1992-2016, Names in the News",*p=9500:row,Pagenum
SKIPPG   ADD       C1 TO PAGE
         move      c0 to lines
         branch   JOBFLAG to pagexit,pagexit,callhead,PAGEXIT,pagexit
pagexit  RETURN
callhead compare   c2 to eojflag
         goto      pagexit if equal
         perform   jobflag of abort,abort,hdok3,hdok4,abort
         return
.
LINE1    BRANCH    JOBFLAG TO PRTBLANK,PRTBLaNK,prtblank,prtblank,prtblank

PRTBLANK  if        (jobflag = 4 or jobflag = 3)
          add       eightlpi,row
          else
          add       Sixlpi,row
          endif
SKIPBLNK ADD       C1 TO LINES
         GOTO      PAGENUM
...............................................................................

PRTREC
         COMPARE   "52" TO LINES
         CALL      HD0 IF NOT LESS
.
         MOVE      C0 TO CMPT92
         MOVE      C0 TO FORM52
         MOVE      C0 TO FORMAR
         MOVE      NO TO BILL
.begin xxx
           if         (ostat = "B" or Ostat = "Q")
           move       "B" to bill
           goto       Billed
           endif
.         CMATCH    "B" TO OSTAT
.         GOTO      BILLED IF EQUAL
.         CMATCH    "Q" TO OSTAT
.         GOTO      BILLED IF EQUAL
.end xxx
         move      olrn to ninvfld
         call      ninvtst
         if        not over
         move      "B" to bill
         goto      billed                        .dlh 19jul95
         endif
.
         branch     jobflag of nonet,nonet,nonet,nonet,testnet
.
testnet
.check date - pre 10/1/95 do it the old way.
         move      "07" to mm
         move      "01" to dd
         move      "99" to yy
         call      cvtjul
         move      juldays to str5      .date we start billing commission on nets the new way
         move      c0 to mm
         move      c0 to dd
         move      c0 to yy
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         call      cvtjul                 .order date
         move      str5 to n5
         if        (juldays >= n5 )        .order date after 7/1/99
         move      yes to ordteflag
         RESET     DISCMLRS                  .DISCOUNTED MLR?
         UNPACK    OMLRNUM INTO STR4
         SCAN      STR4 IN DISCMLRS
         IF         EQUAL
         move      no to ordteflag           .YES
         ENDIF
         else
         move      no to ordteflag
         endif

         move      c1 to netflag               .clear net flag
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        not equal                   .yes
         move      c2 to netflag             .
         endif

         MOVE      NO TO LSTMSW
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
          If        (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
         MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
           endif
         cmatch    "F" to onetfm
         if        equal
         move      c3 to netflag
         endif
         cmatch    yes to lstmsw                    .Lstmgmt its ok! 1/29/97.
         if        equal
         move      c3 to netflag
         endif
         move      no to shipsw
         move      no to mrgsw
         move      olrn to nshpfld
         move      olrn to nmrgfld
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         call      nmrgkey
         if        not over
         move      yes to mrgsw
         endif

         branch    netflag of nonet
..1st is there a minimum & do we meet it?
         compare   c0 to onetmin
         goto      nomin if equal           .no min
         goto      nomin if less


        CMATCH     YES TO MRGSW
        IF         EQUAL
        MOVE       NMRGRQTY TO argh94
        ELSE
        CMATCH     YES TO SHIPSW
        IF         EQUAL
        MOVE       SQUANT TO argh94
        ELSE
        MOVE       OQTY TO argh94
        ENDIF
        ENDIF
         move      argh94  to net94
         move      onetper to CMPT92
         div       hund into CMPT92
         mult      CMPT92 by net94
         move       argh94 to n10
           compare   onetmin to n10
           if        less           .we don't qualify
           if        (OLnum = "002948")         .lifestyle fundraising     .temp fix dlh
           goto      nets
           endif
           move      c1 to netflag
           goto      nonet
           endif

.
nomin
         move      c0 to argh94
         if        ((c2 = netflag or c3 = netflag) & lstmsw = "N" & ordteflag = "Y")         brokerage ,net order and orderdate 7/1/99 or later
        CMATCH     YES TO MRGSW
        IF         EQUAL
        MOVE       NMRGRQTY TO argh94
        ELSE
        CMATCH     YES TO SHIPSW
        IF         EQUAL
        MOVE       SQUANT TO argh94
        ELSE
        MOVE       OQTY TO argh94
        ENDIF
        ENDIF

         move      argh94 to net94              .names in
         move      onetper to form52            .net on order
         div       hund into form52             .turn into a number we can use
         move      argh94 to runc94             .save number in
         mult      form52 by net94               .estimated net names
         sub       net94 from runc94            .names for run charges
         divide    thous into net94             .get ready for cost per M
         move      c0 to form52
          move      NSEL2PRICE,FORM52
         MULT      form52 BY NEt94
         MOVE      net94 TO GROSS               GROSS BILLING.

         move      runc94 to net94           .calc running charge goodies
         divide    thous into net94         .get ready for cost per M
         mult        onetrc by net94         .running charges on estimated unused names
         add       net94 to gross            .add to cost
.
         move      runc94 to net94           .calc commission on list cost savings
         divide    thous into net94         .get ready for cost per M
          move      NSEL2PRICE,FORM52
         mult      form52 by net94              .
.................................
         MOVE      c0 TO FORM34
         MOVE      COMMPer TO FORM34          .commission rate from datacard
         if        (form34 <= 0)
         move      "20" to form34             .if datacard value null assume 20%
         endif
        mult       ".01" by form34
         mult      form34 by net94              .extra commission ie savings fee
         add       net94 to gross              .add to cost
         MOVE      gross TO unbilinc                  ACCOUNTS RECEIVABLE.
         move      argh94 to oqty              .lets do rest of charges on the names in as well.
         goto      endbase
         endif

nonet
         SUB       CMPT92 FROM CMPT92       .clear
         SUB       FORM52 FROM FORM52         .vars
         MOVE      OQTY TO CMPT92            .move order qty to numeric
        CMATCH     YES TO MRGSW
        IF         EQUAL
        MOVE       NMRGRQTY TO cmpt92
        ELSE
        CMATCH     YES TO SHIPSW
        IF         EQUAL
        MOVE       SQUANT TO cmpt92
        ELSE
        MOVE       OQTY TO cmpt92
        ENDIF
        ENDIF

         DIV       THOUS INTO CMPT92
          move      NSEL2PRICE,FORM52
         MULT      FORM52 BY CMPT92          .mult by cost per thous
         MOVE      CMPT92 TO UNBILINC        .base unbilled income.
..1. media charges.
endbase
         MATCH     B2,OFOCODE
         goto      chkspi if equal       .none check special instructions
         MOVE      C0 TO n2
         TYPE      OFOCODE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         GOTO      gotmedia
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       C10 TO N2
         GOTO      gotmedia
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       "20" TO N2
         GOTO      gotmedia
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO N2
         ADD       "30" TO N2
gotmedia branch    n2 of nomed$,nomed$,nomed$,nomed$,nomed$,nomed$,nomed$:
                   nomed$,nomed$,med$15,med$20,med$25,med$30:
                   med$15,med$20,med$25,med$30,med$7m
nomed$  goto      chkspi           .no media charges identified.
.
med$15    add       "15" to unbilinc      .tape fee
         add       "25" to unbilinc      .shipping fee
         goto      chkspi
med$20    add       "20" to unbilinc      .tape fee
         add       "25" to unbilinc      .shipping fee
         goto      chkspi
med$25   add       "25" to unbilinc      .tape fee
         add       "25" to unbilinc      .shipping fee
         goto      chkspi
med$30    add       "30" to unbilinc      .tape fee
         add       "25" to unbilinc      .shipping fee
         goto      chkspi
.med$7m - label fee
med$7m
.
         SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      c10 by CMPT92             .FEE RAISED TO 10
         add       CMPT92 to unbilinc
         goto      chkspi
.2..chkspi lets check for charges buried in spec instructions.
chkspi
....
.OSPI is defunct
         MOVE      "999-999-999-999-999-999-999-999" TO STR35
         EDIT      OSPI TO STR35
         REP       ZFILL IN STR35
         SCAN      "080" IN STR35
         CALL      SEL$10M IF EQUAL
         RESET     STR35
         SCAN      "071" IN STR35
         CALL      SEL$48M IF EQUAL
         RESET     STR35
         SCAN      "078" IN STR35
         IF        EQUAL
         CALL      SEL$4M
         CALL      SEL$5M
         ENDIF
         RESET     STR35
         SCAN      "039" IN STR35
         CALL      SEL$12M IF EQUAL
         RESET     STR35
         SCAN      "038" IN STR35
         CALL      SEL$48M IF EQUAL
         RESET     STR35
         SCAN      "034" IN STR35
         CALL      SEL$15M IF EQUAL
         RESET     STR35
         SCAN      "035" IN STR35
         CALL      SEL$20M IF EQUAL
         RESET     STR35
         SCAN      "028" IN STR35
         CALL      SEL$5M IF EQUAL
         RESET     STR35
         SCAN      "024" IN STR35
         CALL      SEL$1M IF EQUAL
         RESET     STR35
         SCAN      "002" IN STR35
         CALL      SEL$5M IF EQUAL
         RESET     STR35
         SCAN      "004" IN STR35
         CALL      SEL$5M IF EQUAL
         RESET     STR35
         SCAN      "006" IN STR35
         CALL      SEL$5M IF EQUAL
         RESET     STR35
         SCAN      "008" IN STR35
         CALL      SEL$5M IF EQUAL
         RESET     STR35
.
.3..lets check select line
         move      b1,str1
         move      c0 to mfp
         move      c0 to mll
scanslct
...................
          scan      "@$" in NSEL2NAME              .any charges
          goto scanexch if not equal      .nope
          bump      NSEL2NAME by 2                .found one, get ready
          movefptr NSEL2NAME to mfp               .set formpointer
o1
          bump      NSEL2NAME                      .forward one more
          goto scanexch if eos            .oops nothing there get out
          cmove     NSEL2NAME to str1              .get character
          type      str1                      .numeric ?
          goto o1 if equal               .yes, we will take it
          cmatch    "." to str1               .decimal
          goto o1 if equal               .yes, we will take it
          movefptr NSEL2NAME to mll               .get current position
          reset     NSEL2NAME to mfp              .reset formpointer
          sub       c1 from mll          .move lp back to last good character
          setlptr   NSEL2NAME to mll              .set length pointer
          move      NSEL2NAME to str6             .take it
          move      str6 to charge
          call      sel$                      .calc it
          move      mll to mfp                .get ready to look again
          setlptr   NSEL2NAME to 35                .reset length to max
          reset     NSEL2NAME to mfp               .reset formpointer past 1st hit
         goto       scanslct                   .try again
.
.........
SEL$     SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      charge by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.

SEL$1M   SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      c1 by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$1_5M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "1.5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$2_5M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "2.5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$4M   SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "4" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$48M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "4.8" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$5M   SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$10M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      c10 by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$12M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "12" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$15M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "15" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$20M  SUB       CMPT92 FROM CMPT92       .clear
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92
         mult      "20" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
.check for exchange
SCANEXCH RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      OKEX IF EQUAL
         GOTO      OK
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
....
         MOVE      C0 TO CMPT92
         MOVE      irEXQTY TO CMPT92       invoice have exchange info?
         COMPARE   C0 TO CMPT92            .if = no
         goto      okexinv if not equal    .yes, check it out
         MOVE      C0 TO CMPT92
         MOVE      OEXQTY TO CMPT92
okexinv  COMPARE   C0 TO CMPT92            PURE EXCHANGE ?
         IF        EQUAL                 YES.
         MOVE      C0 TO QTYCHK
         MOVE      NO TO SPLITSW
         MOVE      OQTY TO QTYCHK
         MOVE      QTYCHK TO CMPT92
         GOTO      GETPRICE
              ELSE
         MOVE      YES TO SPLITSW
         MOVE      C0 TO QTYCHK
         MOVE      CMPT92 TO QTYCHK        .order or inv split qty depending
         GOTO      GETPRICE
         ENDIF
.
GETPRICE
         move       c0 to form52
         move       iexppm to form52       .if invoice has price, use it.
         compare    c0 to form52
         goto       calce if not equal
.
         SCAN       "DAWSON" IN MCCTO
         IF         EQUAL
         MOVE       C5 TO FORM52
         GOTO       CALCE
         ENDIF
         match      "0171" to obrknum             .dawson?
         IF         EQUAL
         MOVE       C5 TO FORM52
         GOTO       CALCE
         ENDIF
.
         UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    *SAVE RESULT
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           *CONVERT TODAY'S  DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    *SAVE RESULT
         COMPARE   JUNDATE TO MDATE
         IF        NOT GREATER
         MOVE      C8 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         ENDIF
         MOVE      C8 TO FORM52
CALCE
         DIVIDE    THOUS INTO CMPT92
         MULTIPLY  FORM52 BY CMPT92
         MOVE      CMPT92 TO UNBILAMT
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
         MOVE      CMPT92 TO UNBILINC
          if        (cmpt92 < 50)
          move      "50.00",Unbilinc
          endif
         GOTO      OK
         ENDIF
.
RENTPART
...
         MOVE      C0 TO CMPT92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO Num9b
         MOVE      OQTY TO CMPT92
         MOVE      OEXQTY TO Num9b
         SUBTRACT  Num9b FROM CMPT92           GET RENTAL PORTION
         MULT      ".001" BY CMPT92
         MOVE      "85.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         MULT      FORM52 BY CMPT92
         ADD       CMPT92 TO UNBILAMT
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
OK
....
         MOVE      UNBILINC TO CMPT92
         ADD       CMPT92 TO TOTALU
         ADD       CMPT92 TO TOTAL
         ADD       C1 TO COUNTO1
         MOVE      CMPT92 TO FORMAR
         GOTO      PRTDET
..............................................................................
.billed - we have invoice lets use it
..............................................................................
BILLED   MOVE      C0 TO FORMAR
         MOVE      YES TO BILL
         CALL      READINV
         CMATCH    YES TO OVER
         IF        EQUAL
         MOVE      C0  TO FORMAR      *ORDER MARKED DEAD
         GOTO      PRTDET
         ENDIF
         ADD       FORMAR TO TOTAL
         MATCH     "P" TO STATB
         IF        EQUAL
         MOVE      YES TO PAID
         ADD       FORMAR TO TOTALP
         GOTO      PRTDET
         ENDIF
         match     yes to PPSW
         if        equal
         MOVE      YES TO PAID
         ADD       FORMAR TO TOTALP
         GOTO      PRTDET
         endif
         ADD       FORMAR TO TOTALB
PRTDET   ADD       C1 TO COUNTI1
         MATCH     YES TO PAID
         IF        EQUAL
         MOVE      "*PAID*" TO pstatus
         ELSE
         MOVE      "        " TO pstatus
         ENDIF
         BRANCH     JOBFLAG OF PRTDET1,PRTDET1,detout,detout,prtdet1
PRTDET1
         if        (row >= 10000)
         call       hd0
         endif
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,O1DES,*p=3500:row,OLRN,*p=4750:row,*Alignment=*Right,FORMAR,*Font=Font7,*Alignment=*Left:
                    *p=5125:row,OMLRPON,*Font=Font1,*p=6075:row,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY:
                   *p=7000:row,OMLRKY
         MOVE      NO TO PAID
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
          if        (jobflag = c2)
          call      detout
          endif
         ADD       C2 TO LINES
         BRANCH      QTYFLAG OF DISS1,DISS2
         GOTO       DISS1
.
PRTDET3
.         compare   "55" to lines
.         call      pagenum if equal
          if        (row > 7500)
          call      Pagenum
          endif
         move      ordmask10 to ordqty10
         edit      Num9 to ordqty10
         move      shpmask to shpqty
         edit      billqty to shpqty
         add       billqty to totnbill              .total names billed
         move      armask to arout           .total billed $
         edit      formar to arout                 ."
         move      ppmmask to ppmout             .price per m
         edit      form52 to ppmout              .price per m
         cmatch    yes to paid
         if        equal
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,lrn,"*",*p=750:row,*font=Font7,ordqty10:
                    *font=Font2,*p=2000:row,O1DES,*p=5000:row,*font=Font7,*Alignment=*Right,arout,*Alignment=*Left:
                    *p=6000:row,shpqty,*p=6750:row,*Alignment=*Right,ppmout,*Alignment=*Left,*font=font2
         else
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font1,lrn,*p=750:row,*font=Font7,ordqty10:
                    *font=Font1,*p=2000:row,O1DES,*p=5000:row,*font=Font7,*Alignment=*Right,arout,*Alignment=*Left:
                    *p=6000:row,shpqty,*p=6750:row,*Alignment=*Right,ppmout,*Alignment=*Left,*font=font2
         endif
         MOVE      NO TO PAID
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
         ADD       C1 TO LINES
         CMATCH    YES TO BILL
         IF        NOT EQUAL
          add       eightLPI,Row
          PrtPage   Laser;*p=8000:row,*font=Font7,"Estimated",*font=Font1
         else
          add       SixLPI,Row
         ENDIF
         return
.
PRTDET4
.         compare   "55" to lines
.         call      pagenum if equal
          if        (row >= 7500)
          call      Pagenum
          endif
         move      ordmask10 to ordqty10
         edit      Num9 to ordqty10
         move      shpmask to shpqty
         move      ordmask10 to trejmask        .non mailable names
         add       totrej to gtrej
         edit      totrej to trejmask                  ."
         move      ordmask10 to qoutmask        .output qty
         add       nmrgnet to gtnet
         edit      nmrgnet to qoutmask               . "
         edit      billqty to shpqty
         add       billqty to totnbill              .total names billed
         move      armask to arout           .total billed $
         edit      formar to arout                 ."
         move      ppmmask to ppmout             .price per m
         edit      form52 to ppmout              .price per m
         mult      "100" by f34               .was formatted for lotus dll.
         move       c0 to percout               .output cost per m
         add       f34 to percout               .output cost per m
      move      dolrmask to costmask         .output cost per m
         add       costout to gtcost
         edit      costout to costmask          .output cost per m
         move      mask6 to bcstmask                  .base cost savings
         move      c0 to f32                       .      "
         add       form92 to f32                   .      "
         add       form92 to gt92
         edit      f32 to bcstmask                 .      "
         cmatch    yes to paid
         if        equal
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,lrn,"*",*p=500:row,*font=Font7,ordqty10:
                   *font=Font2,*p=1500:row,O1DES,*p=4750:row,*font=Font7,*Alignment=*Right,Arout,*Alignment=*Left,*p=5000:row,shpqty:
                   *p=5750:row,*Alignment=*Right,ppmout,*Alignment=*Left,*p=6125:row,trejmask,*p=7125:row,bcstmask,*p=7750:row,qoutmask:
                   *p=8750:row,percout,*p=9500:row,costmask,*font=Font2
                   
         else
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,lrn,*p=500:row,*font=Font7,ordqty10:
                   *font=Font2,*p=1500:row,O1DES,*p=47501:row,*font=Font7,*Alignment=*Right,Arout,*Alignment=*Left,*p=5000:row,shpqty:
                   *p=5750:row,ppmout,*p=6125:row,trejmask,*p=7125:row,bcstmask,*p=7750:row,qoutmask:
                   *p=8750:row,percout,*p=9500:row,costmask,*font=Font2
         endif
         MOVE      NO TO PAID
         ADD       C2  TO LINES
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
         CMATCH    YES TO BILL
         IF        NOT EQUAL
          add       eightLPI,Row
          PrtPage   Laser;*p=8000:row,*font=Font7,"Estimated",*font=Font1
         else
         print     b1
         ENDIF
         return
.
DETOUT   REP       "#"'" IN O1DES           .REMOVE DELIMITERS
         REP       "#"'" IN ORDCNAME
         REP       "#"'" IN ORDMNAME                        ."
........>>>>>>>NEED TO ADD CODE TO MAKE SURE HAVE THE CORRECT A/P
         REP       ",-" IN ORDCNAME
         REP       ",-" IN ORDMNAME
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         MOVE      C0 TO Num9b
         MOVE      QTYbild TO Num9b
         SUB       Num9b FROM N9
         MOVE      N9 TO FORM92
         MOVE      C0 TO FORM52
         MOVE      PPM TO FORM52
         MULT      ".01" BY FORM52
         MOVE      FORM52 to FORM52PM
         MULT      FORM52 BY FORM92          .SAVINGS
         mult      ".001" by form92
         move      ap1 to CMPT92
         mult      ".01" by CMPT92
         MOVE       C0 TO INVDATE
         MATCH      B2 TO INVDTEM
         GOTO       SKPINVDT IF EQUAL
         GOTO       SKPINVDT IF EOS
         TYPE       INVDTEM
         GOTO       SKPINVDT IF NOT EQUAL
         MOVE       INVDTEM TO MM
         MOVE       INVDTED TO DD
         MOVE       INVDTEY TO YY
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO INVDATE
SKPINVDT MOVE       C0 TO PAYDATE
         MATCH      B2 TO CHK1DTEM
         GOTO       SKIPCHK IF EQUAL
         GOTO       SKIPCHK IF EOS
         TYPE       CHK1DTEM
         GOTO       SKIPCHK IF NOT EQUAL
         MOVE       C0 TO MM
         MOVE       C0 TO DD
         MOVE       C0 TO YY
         MOVE       CHK1DTEM TO MM
         MOVE       CHK1DTED TO DD
         MOVE       CHK1DTEY TO YY
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO PAYDATE
SKIPCHK
         move      oqty to Num9                 ;order qty
         add       Num9 to totnames             ;total names in merge
         move      qtybild to Num9b               ;billed qty
         move      c0 to totrej
         move     c0 to nmrgnet
         move     c0 to f94
         move     c0 to f34
         move     c0 to calcper
         move      Num9 to nonmail            ;get qty ordered(moved below)
         sub       Num9b from nonmail            ;calc non-mailable
         MOVE      olrn TO NMRGFLD            ;check for
         move      olrn to nshpfld
         move      no to shipsw
         CALL      NshpKEY                    ;on line merge data
         if        not over
         move      yes to shipsw
         endif
         move      no to mrgsw
         CALL      NMRGKEY                    ;on line merge data
         if        not over
         move      yes to mrgsw
         move      nmrgrqty to Num9
         MOVE      NMRGNET TO F94
         MOVE      NMRGIQTY TO N9
         move      nmrgnet to calcper
         ADD        NMRGID TO TOTREJ
         ADD        NMRGERR TO TOTREJ
         ADD        NMRGDISF TO TOTREJ
         ADD        NMRGNPER TO TOTREJ
         ADD        NMRGDMA TO TOTREJ
         ADD        NMRGZ4 TO TOTREJ
         ADD        NMRGPRIS TO TOTREJ
         ADD        NCOAMNF TO TOTREJ
         ADD        NMRGCUST TO TOTREJ       .add customer rejects
         add        nmrgelmx to totrej
         add        nmrgconv to totrej             .epsilon goodies see dd file.
         add        nmrgdisa to totrej            
         add        nmrgcnr to totrej            
         else
         move      oqty to n9
         move      qtybild to f94
         move      qtybild to calcper
         move      nonmail to totrej
         endif
         add       n9 to totinmrg        .total in to merge
         DIVIDE    N9 INTO F94           ordered qty / shipped qty
         MOVE      C0 TO F34
         ADD       F94 TO F34
         MULT      "100" BY F94          .* 100 = percent out2
         MOVE      C0 TO N9
         MOVE      Num9 TO N9        ;if not 0  qty input to merge else qty ordered
         MOVE      C0 TO Num9b
         MOVE      QTYbild TO Num9b      ;qty billed
         SUB       Num9b FROM N9        ;difference
         move      totrej to form92       .dlh 10/7   avail else use oqty less billed qty
         MOVE      C0 TO FORM52
.check for split & adjust cost/m accordingly   dlh '14feb95
.
.new calcsplit - instead of 2 line items we have one and the base cost is
.represented as billable names/gross ar
.ex     10000 @ 70/m         700.00
.       20000 @ 10/m         200.00
.       -----------
.       900.00/30,000x1000  = 30.00m base cost
.
         compare   c1 to spltflag
         if        equal
         MOVE      PPM TO FORM52
         MULT      ".01" BY FORM52
         else
         move      qtybild to Num9b            ;rental qty billed
         move      c0 to f96splt
         move      irexqty to f96splt         .exchange qty
         compare   c0 to f96splt              .    have qty?
            if        equal                      .nope use order
            move      oexqty to f96splt           ;calc cost out per M
            endif
         move      c0 to n6         .lets save ex qty
         move      f96splt to n6    . for later use.
         move      c0 to form96       .lets calc base
         move      Num9b to form96       .rental portion
         MOVE      PPM TO FORM52
         MULT      ".01" BY FORM52
         mult      form52 by form96   .ok
         mult      ".001" by form96
.
         move       c0 to costsplt
         move       iexppm to costsplt       .if invoice has price, use it.
         mult       ".01" by costsplt
         compare    c0 to costsplt
         goto       spltok if not equal
         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO costsplt
         goto      spltok
         ENDIF
         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO costsplt
         goto      spltok
         ENDIF
         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO costsplt
         goto      spltok
         ENDIF
         MOVE      C8 TO costsplt
spltok
         mult      costsplt by f96splt       .exchange portion
         mult      ".001" by f96splt
         add       f96splt to form96    .combined base cost
         move      qtybild to f94            ;total rental qty billed
         add       n6 to f94
         divide    f94 into form96        .get aggregate cost/m
         mult      "1000" by form96
         move      form96 into form52
         move      qtybild to n9            ;total rental qty billed
         add       n6 to n9                ;add exchange
         move      n9 to qtybild           ;total qty billed rent & exchange
         endif
         MOVE      FORM52 to FORM52PM
         MULT      FORM52 BY FORM92          .SAVINGS
         mult      ".001" by form92
         DIVIDE    Num9b INTO CALCPER
         MULT      "100" BY CALCPER
         MOVE      C0 TO PERCENT
         ADD       CALCPER TO PERCENT
         move      ppm to gross
         mult      Num9b by gross
.
         move      c0 to form96
         move      c0 to costout
         move      formar to form96           ;calc cost out per M
         div       nmrgnet into form96
         mult      "1000" by form96
         add       form96 to costout
.
         CMATCH    YES TO BILL
         IF        EQUAL
         move      "          " TO MESS2
         goto      spltq
         endif
         CMATCH    "B" TO BILL
         IF        EQUAL
         move      "INV FOUND !" TO MESS2
         else
         move      "*ESTIMATED*" TO MESS2
         ENDIF
spltq
writeok
         move       c0 to maledate
         MOVE       C0 TO MM
         MOVE       C0 TO DD
         MOVE       C0 TO YY
         MOVE       omdtem TO MM
         MOVE       omdted TO DD
         MOVE       omdtey TO YY
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO maledate
         branch     jobflag of stop,writeok1,prtdet3,prtdet4,stop
writeok1
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
                    move      NSEL2NAME,taskname
                    call      Trim using taskname
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
          endif
          clear     mess3
          if (OELCODE = "2" | OELCODE = "3")      .Exchange Records
                    if (NSEL2PRICE2 = C0)
.Prepare Default Value
                              move      "75",NSEL2PRICE2
.
                              packkey   NSELFLD1,"01X",LSTNUM
                              clear     NSELFLD2
                              packkey   NSELFLD3,"03X",taskname
                              rep       lowup,taskname
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD3
                              call      NSELAIM
                              if not over
                                        loop
.The following is an extreme double-check!!
                                                  call      Trim using NSELSNAME
                                                  rep       lowup,NSELSNAME
                                                  if (taskname = NSELSNAME)
                                                            clear     mess3
                                                            if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
                                                                      call      SelectTestBase4 using NSELLIST,NSELBASE,N52
                                                                      add       N52,NSELPRICE,NSEL2PRICE2
                                                                      packkey   NSELFLD,LSTNUM,NSELBASE
                                                                      move      "NSELKEY-4",Location
                                                                      pack      KeyLocation,"Key: ",NSELFLD
                                                                      call      NSELKEY
                                                                      if not over
                                                                                if (NSELEXC = "2")
                                                                                          move      "e o",mess3
                                                                                endif
                                                                      endif
                                                            else
                                                                      move      NSELPRICE,NSEL2PRICE2
                                                                      if (NSELEXC = "2")
                                                                                move      "e o",mess3
                                                                      endif
                                                            endif
.                                                           move      C0,PriceFlag
                                                            break
                                                  elseif (NSELBASE = "BASE")
                                                            if (NSELEXC = "2")
                                                                      move      "e o",mess3
                                                            endif
                                                  endif
                                                  move      "NSELKG",Location
                                                  call      NSELKG
                                                  until over
                                        repeat
                              else
                                        packkey   NSELFLD1,"01X",LSTNUM
                                        packkey   NSELFLD2,"02XBASE"
                                        clear     NSELFLD3
                                        rep       lowup,taskname
                                        move      "NSELAIM-4",Location
                                        pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                                        call      NSELAIM
                                        if not over
                                                  if (NSELEXC = "2")
                                                            move      "e o",mess3
                                                  else
                                                            move      NSELPRICE,NSEL2PRICE2
                                                  endif
                                        endif
                              endif
                    else                .Try to get Exchange Only portion!
                              type      NSEL2NUM
                              if equal
                                        packkey   NSELFLD,LSTNUM,NSEL2NUM
                                        move      "NSELKEY",Location
                                        pack      KeyLocation,"Key: ",NSELFLD
                                        call      NSELKEY
                                        if not over
                                                  if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
                                                            move      "e o",mess3
                                                  elseif (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
                                                            packkey   NSELFLD,LSTNUM,NSELBASE
                                                            move      "NSELKEY-2",Location
                                                            pack      KeyLocation,"Key: ",NSELFLD
                                                            call      NSELKEY
                                                            if not over
                                                                      if (NSELEXC = "2")
                                                                                move      "e o",mess3
                                                                      endif
                                                            endif
                                                  endif
                                        else      .Not really possible, but will cover bases anyway!
                                                  packkey   NSELFLD1,"01X",LSTNUM
                                                  packkey   NSELFLD2,"02XBASE"
                                                  clear     NSELFLD3
                                                  move      "NSELAIM-2",Location
                                                  pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                                                  call      NSELAIM
                                                  if not over
                                                            if (NSELEXC = "2")
                                                                      move      "e o",mess3
                                                            endif
                                                  endif
                                        endif
                              else                .Not really possible, but will cover bases anyway!
                                        packkey   NSELFLD1,"01X",LSTNUM
                                        packkey   NSELFLD2,"02XBASE"
                                        clear     NSELFLD3
                                        move      "NSELAIM-3",Location
                                        pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                                        call      NSELAIM
                                        if not over
                                                  if (NSELEXC = "2")
                                                            move      "e o",mess3
                                                  endif
                                        endif
                              endif
                    endif
                    if (PPM > NSEL2PRICE2)
                              move      PPM,NSEL2PRICE2
                    endif
          else
                    move      PPM,NSEL2PRICE2
          endif
......................................................................................
          if        (Ostat <> "Q" & OSTAT <> "B")     .not billed lets look for addl charges
          move      c0,MoreAR
          pack      NSEL3FLD1,"01X1",OLRN
          move      "NSEL3AIM",Location
          pack      KeyLocation,"Key: ",NSEL3FLD1
          call      NSEL3AIM
          goto      endcalcadd if over
          call      calcAdd 
          loop
          call      NSEL3KG
                    until over
          call      calcadd
          repeat
          endif
          goto      endcalcadd
calcadd
                    clear     taskname
                    if (NSEL3CODE = "A")
                              pack      NADDFLD,OLNUM,NSEL3NUM
                              move      "NADDKEY",Location
                              pack      KeyLocation,"Key: ",NADDFLD
                              call      NADDKEY

                                                  move      NSEL3NUM,NADDNUM
.
                                                  call      Trim using NSEL3MODNUM
                                                  if (NSEL3MODNUM <> "")
                                                            move      NSEL3MODNUM,NADDDESC
                                                  endif
                                                  call      Trim using NADDDESC
                                                  if (NADDDESC = "")
                                                            move      "XXX",NADDDESC      .Force an over...
                                                  endif
                                        pack      NREFFLD,"A",NADDNUM
                                        move      "NREFKEY",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        call      Trim using NREFDESC
                                        if (NSEL3PRICE = 0)
                                                  pack      taskname,NREFDESC
                                        else
                                                  pack      NMODFLD,NADDDESC
                                                  rep       zfill,NMODFLD
                                                  move      "NMODKEY",Location
                                                  pack      KeyLocation,"Key: ",NMODFLD
                                                  call      NMODKEY
                                                  move      C0,N3
                                                  call      Trim using NMODNUM
                                                  move      NMODNUM,n3
                                                  if        (n3 = c1)           ./m
                                                  move      c0,calcAR
                                                  calc      calcar=(Num9/1000)*Nsel3price          
                                                  add       CalcAR,MoreAR                              
                                                  elseif    (n3 = c2)           .Flat
                                                  add       Nsel3price,MoreAr
                                                  endif                                      
                                        endif
                    elseif (NSEL3CODE = "L")
                              pack      NSLTFLD,OLNUM,NSEL3NUM
                              move      "NSLTKEY",Location
                              pack      KeyLocation,"Key: ",NSLTFLD
                              call      NSLTKEY
                                                  move      NSEL3NUM,NSLTNUM
.
                                                  call      Trim using NSEL3MODNUM
                                                  if (NSEL3MODNUM <> "")
                                                            move      NSEL3MODNUM,NSLTDESC
                                                  endif
                                                  call      Trim using NSLTDESC
                                                  if (NSLTDESC = "")
                                                            move      "XXX",NSLTDESC      .Force an over...
                                                  endif
                                        pack      NREFFLD,"L",NSLTNUM
                                        move      "NREFKEY-2",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        call      Trim using NREFDESC
                                        if (NSEL3PRICE = 0)
                                                  pack      taskname,NREFDESC
                                        else
                                                  pack      NMODFLD,NSLTDESC
                                                  rep       zfill,NMODFLD
                                                  move      "NMODKEY-2",Location
                                                  pack      KeyLocation,"Key: ",NMODFLD
                                                  call      NMODKEY
                                                  move      C0,N3
                                                  call      Trim using NMODNUM
                                                  move      NMODNUM,n3
                                                  if        (n3 = c1)           ./m
                                                  move      c0,calcAR
                                                  calc      calcar=(Num9/1000)*Nsel3price          
                                                  add       CalcAR,MoreAR                              
                                                  elseif    (n3 = c2)           .Flat
                                                  add       Nsel3price,MoreAr
                                                  endif                                      
                                        endif
                    endif
          return
endcalcadd
          if        (Ostat <> "Q" & OSTAT <> "B")     .not billed lets look for addl charges
                    Add       MoreAr,FormAR
            endif
......................................................................................
         cmatch    yes to mbildrct            .bill direct?  14apr95 dlh
          if       equal
         clear     ordmname
         endif
         MATCH     YES TO PAID
         IF        EQUAL
         move      "*PAID*" to mess2
         endif
..................
        add     C1,howmany
        move    howmany,str9
        call    Trim using str9
        pack    RangeStr,"A",str9
...................
        setprop sheet.range(RangeStr),*Value=OLRN
        pack    RangeStr,"B",str9
        setprop sheet.range(RangeStr),*Value=INVNUM
        pack    RangeStr,"C",str9
        setprop sheet.range(RangeStr),*Value=Num9,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
        pack    RangeStr,"D",str9
        setprop sheet.range(RangeStr),*Value=mess3
        pack    RangeStr,"E",str9
        call    Trim using O1DES
        clear    str100
        append   o1des,str100
        append   Sreturn,str100
....................
        rep      Uplow in NSEL2NAME                       ;set to lower case
        call      Trim using NSEL2NAME
        append   NSEL2NAME,str100
        reset    str100
        setprop sheet.range(RangeStr),*Value=str100
.option 1 define cell as text word wrap - set to 35 characters, pad 01des to 35 and append 02des
        pack    RangeStr,"F",str9
        call    Trim using ONETPER
        if (ONETPER = "" | ONETPER = "0")
                clear   str3
        else
                pack    str3,PERIOD,ONETPER
        endif
.Percentage Formatting
        setprop sheet.range(RangeStr),*Value=str3,*NumberFormat="##,####0%_);_(#"-#"_)"
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Value=FORMAR,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
        pack    RangeStr,"H",str9
        setprop sheet.range(RangeStr),*Value=QTYBILD,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
..........................................
.        if (OUTNAME = "2" | OUTNAME = "1")
        if (OUTNAME = "1")                     .=discounted version not in use????
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value=PPM,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.Substitute Rejects for a Formula
.Non-Billable Names Formula  .column J
                    pack    RangeStr,"J",str9
                    clear   str35
                    append  "=Sum(C",str35
                    append  str9,str35
                    append  "-H",str35
                    append  str9,str35
                    append  ")",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.Base Cost Formula   .column k
                    clear   str35
                    append  "=Round(J",str35
                    append  str9,str35
                    append  "/1000*I",str35
                    append  str9,str35
                    append  ",2)",str35
                    reset   str35
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.begin patch xxx   everything beyond K is shifted right for outname = "2" (discounted)

.   .column L

                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value=NMRGNET,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.Percentage Formula    .column M
                    pack    RangeStr,"M",str9
                    clear   str35
                    append  "=Round(L",str35
                    append  str9,str35
                    append  "/C",str35
                    append  str9,str35
                    append  ",4)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"
.Output Cost Formula     .column N
                    clear   str35
                    append  "=Round(G",str35
                    append  str9,str35
                    append  "/L",str35
                    append  str9,str35
                    append  "*1000,2)",str35
                    reset   str35
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value=MESS1
                    pack    RangeStr,"P",str9
                    move    COMMPCT,N3
                    mult    N3,".01",N34
                    setprop sheet.range(RangeStr),*Value=N34,*NumberFormat="##,####0.00%_);_(#"-#"_)"
                    pack    RangeStr,"Q",str9
                    div     "1000",GROSS,N92
                    setprop sheet.range(RangeStr),*Value=N92,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value=N9,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
                    pack    RangeStr,"T",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using INVDTEM
                    if (INVDTEM <> "")
                              pack    str10,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"U",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using CHK1DTEM
                    if (CHK1DTEM <> "")
                              pack    str10,CHK1DTEM,SLASH,CHK1DTED,SLASH,CHK1DTEC,CHK1DTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value=ordcname
                    pack    RangeStr,"W",str9
                    setprop sheet.range(RangeStr),*Value=ordmname
                    pack    RangeStr,"X",str9
                    setprop sheet.range(RangeStr),*Value=OMLRKY
                    pack    RangeStr,"Y",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using OMDTEM
                    if (OMDTEM <> "")
                              pack    str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value=OLNUM
.Current discount version .........<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
               Elseif   (Outname = "2")  

                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value=NSEL2PRICE2,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.Adjusted price take ppm * .8 + the discounted rate
                    pack    RangeStr,"J",str9

                    if      (mess1 <> "EXCH")       
                    call         Debug
                    
                    Move    PPM,form52
.should not be hard coded 80% needs to look at the commission.
.begin patch 4.44a
                    move    COMMPCT,N3
                    if      (n3 = "20")                 .20% comm
                    Mult    ".8",form52
                    else
                    mult    N3,".01",N34                 .get commission
                    calc    n34=(1.00-N34)
                    mult    n34,Form52
                    endif                  
.                    Mult    ".8",form52
.end patch 4.44a
                    
                               if           (omlrnum = "0193" | Omlrnum = "7068")
                               add     "14.00",form52
                               else
                               add     "10.00",form52           .presuming MSF
                               endif
                               if      (form52 < ppm)                            .may be special price or Flat
                               move    form52,ppm
                               endif
                    Else                               .its exchange
                               if           (omlrnum = "0193" | Omlrnum = "7068")
                               move     "14.00",form52
                               else
                               move     "14.00",form52           .presuming MSF
                               endif
                               if      (form52 < ppm)                            .may be special price or Flat                            
                               move    form52,ppm
                               endif
                    endif
                    setprop sheet.range(RangeStr),*Value=PPM,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.savings
.=IF(F58=0,SUM(I58-J58)*(H58/1000),ROUND((((C58/1000)*I58)*0.'Commission')-((C58/1000)*14),2))
                    move    COMMPCT,N3
                    if      (n3 <= "20")                 .may have other exceptions in the future, currently only happens on managed files
                    mult    N3,".01",N34                 .get commission
                    else
                    move   ".20",n34
                    endif
                    pack    RangeStr,"L",str9
                    clear   taskname
                    Append  "=IF(F",taskname
                    append  str9,taskname
                    append  "=0,Sum(i",taskname
                    append  str9,taskname
                    append  "-j",taskname
                    append  str9,taskname
                    append  ")*(H",taskname
                    append  str9,taskname
                    APPEND  "/1000),Round((((C",taskname                
                    append  str9,taskname
                    append  "/1000)*I",taskname                     
                    append  str9,taskname
                    append  ")*",taskname
                    append N34,taskname
                    append ")-((C",taskname
                    append  str9,taskname
.add code for MSF it is $10    DCC, 14 or on axchanges: exchanges $14.00/M on gross names ordered if less than 100,000 names,
.$13/m if 100m – 199m names, $12/m if 200m – 299m names, $11/m if 300m+ names.  Any exchange order less than 5m names will be billed at $70/flat.   
                    if           (omlrnum = "0193" | Omlrnum = "7068")
                    append  "/1000)*14),2))",taskname
                    Else                    .presume it is MSF  
                    append  "/1000)*10),2))",taskname
                    endif
                    reset   taskname
                    setprop sheet.range(RangeStr),*Formula=taskname,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.Substitute Rejects for a Formula
.Non-Billable Names Formula  .column M
                    pack    RangeStr,"M",str9
                    clear   str35
                    append  "=Sum(C",str35
                    append  str9,str35
                    append  "-H",str35
                    append  str9,str35
                    append  ")",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.Base Cost Formula   .column K
.=(J25-K25)*(I25/1000)  Gemma wants this
                    clear   str35
.                    append  "=Round(J",str35
.                    append  str9,str35
.                    append  "/1000*I",str35
.                    append  str9,str35
.                    append  ",2)",str35

                    append  "=Round((I",str35
                    append  str9,str35
                    append  "-J",str35
                    append  str9,str35
                    append  ")*(H",str35
                    append  str9,str35
                    append  "/1000),2)",str35
                    reset   str35

                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.base cost savings
.=ROUND(M14/1000*J14,2)                    
                    clear   str35
                    append  "=Round(M",str35
                    append  str9,str35
                    append  "/1000*J",str35  
                    append  str9,str35
                    append  ",2)",str35
                    reset   str35
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

.   Net out
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value=NMRGNET,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.output Percentage Formula    .column P
                    pack    RangeStr,"P",str9
                    clear   str35
                    append  "=Round(O",str35
                    append  str9,str35
                    append  "/C",str35
                    append  str9,str35
                    append  ",4)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"

.Output Cost Formula     .column Q
                    clear   str35
                    append  "=Round(G",str35
                    append  str9,str35
                    append  "/O",str35
                    append  str9,str35
                    append  "*1000,2)",str35
                    reset   str35
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.rent/exch
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value=MESS1
                    pack    RangeStr,"S",str9
                    move    COMMPCT,N3
                    mult    N3,".01",N34
                    setprop sheet.range(RangeStr),*Value=N34,*NumberFormat="##,####0.00%_);_(#"-#"_)"

                    pack    RangeStr,"T",str9
                    div     "1000",GROSS,N92
                    setprop sheet.range(RangeStr),*Value=N92,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
                    pack    RangeStr,"V",str9
                    setprop sheet.range(RangeStr),*Value=N9,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
                    pack    RangeStr,"W",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using INVDTEM
                    if (INVDTEM <> "")
                              pack    str10,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"X",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using CHK1DTEM
                    if (CHK1DTEM <> "")
                              pack    str10,CHK1DTEM,SLASH,CHK1DTED,SLASH,CHK1DTEC,CHK1DTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value=ordcname
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value=ordmname
                    pack    RangeStr,"AA",str9
                    setprop sheet.range(RangeStr),*Value=OMLRKY
                    pack    RangeStr,"AB",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using OMDTEM
                    if (OMDTEM <> "")
                              pack    str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"AC",str9
                    setprop sheet.range(RangeStr),*Value=OLNUM
.end patch xxx   everything beyond K is shifted right for outname = "2" (discounted)
.begin patch xxxx
                    pack    RangeStr,"AD",str9
                    setprop sheet.range(RangeStr),*Value=OMlrLstCd
.end patch xxxxx
          else               .<<<<<<<<<<<<<<<<<<<<<<<<<regu
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value=NSEL2PRICE2,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value=PPM,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"K",str9
                    clear   str35
                    append  "=(I",str35
                    append  str9,str35
                    append  "-J",str35
                    append  str9,str35
                    append  ")*(H",str35
                    append  str9,str35
                    append  "/1000)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.Substitute Rejects for a Formula
.Non-Billable Names Formula  .column L
                    pack    RangeStr,"L",str9
                    clear   str35
                    append  "=Sum(C",str35
                    append  str9,str35
                    append  "-H",str35
                    append  str9,str35
                    append  ")",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.Base Cost Formula   .column M
                    clear   str35
                    append  "=Round(L",str35
                    append  str9,str35
                    append  "/1000*J",str35
                    append  str9,str35
                    append  ",2)",str35
                    reset   str35
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.   .column L
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value=NMRGNET,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.Percentage Formula    .column O
                    pack    RangeStr,"O",str9
                    clear   str35
                    append  "=Round(N",str35
                    append  str9,str35
                    append  "/C",str35
                    append  str9,str35
                    append  ",4)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"
.Output Cost Formula     .column P
                    clear   str35
                    append  "=Round(G",str35
                    append  str9,str35
                    append  "/N",str35
                    append  str9,str35
                    append  "*1000,2)",str35
                    reset   str35
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"Q",str9
                    setprop sheet.range(RangeStr),*Value=MESS1
                    pack    RangeStr,"R",str9
                    move    COMMPCT,N3
                    mult    N3,".01",N34
                    setprop sheet.range(RangeStr),*Value=N34,*NumberFormat="##,####0.00%_);_(#"-#"_)"
                    pack    RangeStr,"S",str9
                    div     "1000",GROSS,N92
                    setprop sheet.range(RangeStr),*Value=N92,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value=N9,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
                    pack    RangeStr,"V",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using INVDTEM
                    if (INVDTEM <> "")
                              pack    str10,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"W",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using CHK1DTEM
                    if (CHK1DTEM <> "")
                              pack    str10,CHK1DTEM,SLASH,CHK1DTED,SLASH,CHK1DTEC,CHK1DTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"X",str9
                    setprop sheet.range(RangeStr),*Value=ordcname
                    pack    RangeStr,"Y",str9
                    setprop sheet.range(RangeStr),*Value=ordmname
                    pack    RangeStr,"Z",str9
                    setprop sheet.range(RangeStr),*Value=OMLRKY
                    pack    RangeStr,"AA",str9
.Value must be converted from Julian Date to Standard Date
                    clear   str10
                    call    Trim using OMDTEM
                    if (OMDTEM <> "")
                              pack    str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                    endif
                    setprop sheet.range(RangeStr),*Value=str10,*NumberFormat="m/d/yyyy"
                    pack    RangeStr,"AB",str9
                    setprop sheet.range(RangeStr),*Value=OLNUM
.begin patch xxx
                    pack    RangeStr,"AC",str9
                    setprop sheet.range(RangeStr),*Value=NMRGKCOD

.end patch xxx
.begin patch xxxx
                    pack    RangeStr,"AD",str9
                    setprop sheet.range(RangeStr),*Value=OMlrLstCd

.end patch xxxxx
          endif
         MOVE      NO TO PAID
wrtexit  return
.
.
.
DISS1
         CLEAR     DISDATE
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,NSEL2NAME,B1,pstatus;
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         CALL      ESTIMATE
         ENDIF
          add       SixLPI,Row
         ADD       C1 TO LINES
         RETURN
.
DISS2
         CLEAR     DISDATE
         MOVE      OQTY TO Num9b
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,NSEL2NAME,B1,pstatus;
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         CALL      ESTIMATE
         ENDIF
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO Num9b
         EDIT      Num9b TO ORDQTY
          add       SixLPI,Row
          PrtPage   Laser;*p=6500:row,ORDQTY,B1
         ADD       C1 TO LINES
         RETURN

ESTIMATE  
.begin xxx
           if         (ostat = "B" or Ostat = "Q") .we should not be here
           return
           endif
.end xxx
           if        (netflag = "2" or netflag = "3")
          add       SixLPI,Row
          PrtPage   Laser;*p=4000:row,*font=font2,*ULON,"*ESTIMATED ","Net",*ULOFF," COST*",*Font=Font1
         else
          add       SixLPI,Row
          PrtPage   Laser;*p=4125:row,*font=font2,"*ESTIMATED COST",*Font=Font1
         endif
         RETURN
READINV
.......................................................................
.
. ROUTINE TO READ THE INVOICE RECORD AND COMPUTE CODE TOTALS.
.
         MOVE      OLRN TO NINVFLD
           Clear      Bill                  .used if not billed
         CALL      NINVKEY
         GOTO      INVEXIT IF OVER
         MOVE      NO TO OVER
         move      c0 to billqty
         move      qtybild to billqty
         ADD       C1 TO COUNTI
         move      olon to nownfld
         call      nownkey
         move     no to shipsw
         move     no to mrgsw
         move      lrn to nshpfld
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         MOVE      LRN TO NmrgFLD
         REP       ZFILL IN NmrgFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         call      wipecvars
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
               Call           NInvAcdRecLoad
                                                                      CALL      COMPUTE
         CALL      CHKADJ                            *ADDED DLH 12/23/92
         cmatch    yes to qtyadjsw        .adjust done for qty
         if        equal                  .yes
         move      c0 to billqty          .pick up correct qty from order
         move      oqty to billqty
          count     n1,oqty
          if (n1 = 9)
                    BUMP       oqty,C2
                    move       oqty to qtybild
                    RESET      oqty
          elseif (n1 = 8)
                    BUMP       oqty,C1
                    move       oqty to qtybild
                    RESET      oqty
          else         .n1 = 7
                    move       oqty to qtybild
          endif
         endif
        RETURN
.
INVEXIT  NORETURN
         MOVE      YES TO OVER
         MOVE      YES TO BILL
         RETURN
.
CHKADJ   move      no to code16sw
         move      no to qtyadjsw
         MOVE      LRN TO NADJFLD
         CALL      NADJKEY                        ADJUSTED INVOICE?
         RETURN    IF OVER                        .NO
.

          move      asrecadj to adjar
.need to check for a detail adj code 16 and if present add the short payment
.amount back in to reflect the real cost. dlh 15mar95
         match     "P" to statb
         if        equal
         match     yes to PPSW
         if        equal
         compare   adjar to formar
         goto      detadj if equal
         endif
         endif
         ADD       ADJAR  TO FORMAR                  ADD UP AR AMOUNTS.
detadj   move      no to qtyadjsw
         move      no to code16sw
         pack      njstfld from asinvno,c0
         call      njstkey
adjchk   match     "01",jstreasn          qty adjustment ?
         if        equal                  .yes
         move      yes to qtyadjsw
         endif
         match     "16",jstreasn          prepay adjustment ?
         if        equal                  .yes
         move      yes to code16sw
         endif
         call       njstks
         return     if over
         match      asinvno to jstinvno
         return     if not equal
         goto       adjchk
CALCBILL
         RETURN

newmlr
         compare   c0 to page
         goto      newmlrb if equal
         if        (row >= 10000)
         call       hd0
         endif
          add       SixLPI,Row
          add       SixLPI,Row
          if        (TotLCR = c0)
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Orders in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTSUM,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Estimated",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Open",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALB,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Paid",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALP,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Grand Total",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTAL,*Alignment=*Left
         add       c6 to lines
          else      
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Orders in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTSUM,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Clearances in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Estimated",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Open",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALB,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Paid",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALP,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Grand Total",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTAL,*Alignment=*Left
         add       c7 to lines
          endif
         call      pagenum
         move     c0 to totLCR
         move     c0 to totsum
         move     c0 to totalu
         move     c0 to totalp
         move     c0 to total
         move     c0 to totalb
newmlrb  move     omlrnum to holdmlr
         move     c0 to lines
         move     c0 to page
         return
.
EOJ
         if        (row >= 10000)
         call       hd0
         endif
         BRANCH    JOBFLAG TO PRTTOT,PRTTOT,prttot3,prttot4,prttot
PRTTOT
         if        (row > 8750)
         call       hd0
         endif

          add       SixLPI,Row
          add       SixLPI,Row
          if        (TotLCR = c0)       
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Orders in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTSUM,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Estimated",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Open",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALB,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Paid",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALP,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Grand Total",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTAL,*Alignment=*Left
          add       SixLPI,Row
          add       SixLPI,Row
          PrtPage   Laser;*p=125:row,*font=font2,"Estimates are deemed accurate when report generated. Based on previous usage and order information."
          add       SixLPI,Row
          PrtPage   Laser;*p=125:row,*font=font2,"Subject to Mailer change and negotiations with list owner."
          ADD       C9 TO LINES
          Else
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Orders in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTSUM,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"## of Clearances in Summary",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Estimated",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALU,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Open",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALB,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Invoiced Paid",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTALP,*Alignment=*Left
          add       SixLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Grand Total",*p=1500:row,":",*p=2500:row,*Alignment=*Right,TOTAL,*Alignment=*Left
          add       SixLPI,Row
          add       SixLPI,Row
          PrtPage   Laser;*p=125:row,*font=font2,"Estimates are deemed accurate when report generated. Based on previous usage and order information."
          add       SixLPI,Row
          PrtPage   Laser;*p=125:row,*font=font2,"Subject to Mailer change and negotiations with list owner."
         ADD       C10 TO LINES
          endif
         CALL      PAGENUM

         if         (jobflag = c2)
         goto       Totout
         endif


         goto       stop
PRTTOT3
          if        (row >= 7500)
          call      Pagenum
          endif
         move       mask11 to str14
         edit       totinmrg to str14
         move       mask11 to bqtymask
         edit       totnbill to bqtymask
         move       dolrmask to totbmask
         edit       totalb to totbmask
         move       dolrmask to totpmask
         mult       seq by totalp
         edit       totalp to totpmask
         move       dolrmask to totmask
         edit       total to totmask
         move      shpmask to shpqty
         move      ordmask10 to trejmask
         edit      gtrej to trejmask
         move      ordmask10 to qoutmask
         edit      gtnet to qoutmask                ."
         add       gtnet to calcper
         DIVIDE    totnbill INTO calcper
         mult      "100" by calcper
         move      c0 to percout
         add       calcper to percout
         move      dolrmask to costmask         .output cost per m
         edit      gtcost to costmask          .output cost per m
         move      mask6 to bcstmask                  .base cost savings
         move      c0 to f32                       .      "
         add       gt92 to f32                   .      "
         edit      f32 to bcstmask                 .      "
         add       totalp to total
        move      dolrmask to balmask
         edit      total to balmask        .      "


          add       eightLPI,Row
          PrtPage   Laser;*p=500:row,*font=font7,*ULON,b2,str11,*p=500:row,Totmask:
                    *p=6000:row,bqtymask,*ULOFF
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Totals: ",*p=2000:row,totmask
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"*Total paid Invoices: ",*p=2000:row,totpmask
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Balance Due: ",*p=2000:row,balmask
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Orders Processed:",*p=2000:row,totsum
         compare   c2 to remtflag
         if        equal
          add       eightLPI,Row
          PrtPage   Laser;*p=5125:row,*font=font2,*ULON,"REMITTANCE COPY",*ULOFF
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=4500:row,*font=font2,"(Please remit copy with payment, Thank you)"

         else
          add       eightLPI,Row
          PrtPage   Laser;*p=5125:row,*font=font2,*ULON,"CLIENT COPY",*ULOFF
          add       eightLPI,Row
          add       eightLPI,Row
         endif
         ADD       "12" TO LINES
         CALL      PAGENUM





         goto       stop
...............................................................................
PRTTOT4  
          if        (row >= 7500)
          call      pagenum
          endif
         move       mask11 to str11
         edit       totinmrg to str11
         move       mask11 to bqtymask
         edit       totnbill to bqtymask
         move       dolrmask to totbmask
         edit       totalb to totbmask
         move       dolrmask to totpmask
         mult       seq by totalp
         edit       totalp to totpmask
         move       dolrmask to totmask
         edit       total to totmask
         move      shpmask to shpqty
         move      ordmask10 to trejmask
         edit      gtrej to trejmask
         move      ordmask10 to qoutmask
         edit      gtnet to qoutmask                
         move      c0 to f94
         add       gtnet to f94
         DIVIDE    totnbill INTO f94           ordered qty / shipped qty
         mult      "100" by f94               .was formatted for lotus dll.
         move      c0 to percout
         add       f94 to percout               .output cost per m
         move      dolrmask to costmask         .output cost per m
         edit      gtcost to costmask          .output cost per m
         move      mask6 to bcstmask                  .base cost savings
         move      c0 to f32                       .      "
         add       gt92 to f32                   .      "
         edit      f32 to bcstmask
         add       totalp to total
         move      dolrmask to balmask
         edit      total to balmask
         
          add       eightlpi,row
          prtpage   Laser;*p=125:row,*font=Font2,b2,str11,*p=4750:row,b2,bqtymask:
                    *p=6250:row,trejmask,*p=7250:row,bcstmask,*p=7750:row,qoutmask:
                    *p=8750:row,percout,*p=9500:row,costmask
          add       eightlpi,row
          add       eightlpi,row
          add       eightLPI,Row
          PrtPage   Laser;*p=500:row,*font=font7,*ULON,b2,str11,*p=500:row,Totmask:
                    *p=6000:row,bqtymask,*ULOFF
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Totals: ",*p=2000:row,totmask
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"*Total paid Invoices: ",*p=2000:row,totpmask
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Balance Due: ",*p=2000:row,balmask
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=1:row,*font=font2,"Total Orders Processed:",*p=2000:row,totsum
         compare   c2 to remtflag
         if        equal
          add       eightLPI,Row
          PrtPage   Laser;*p=5125:row,*font=font2,*ULON,"REMITTANCE COPY",*ULOFF
          add       eightLPI,Row
          add       eightLPI,Row
          PrtPage   Laser;*p=4500:row,*font=font2,"(Please remit copy with payment, Thank you)"
         else
          add       eightLPI,Row
          PrtPage   Laser;*p=5125:row,*font=font2,*ULON,"CLIENT COPY",*ULOFF
          add       eightLPI,Row
          add       eightLPI,Row
         endif
         ADD       "12" TO LINES
         CALL      PAGENUM
         goto       stop
stop     
          PRtclose  Laser
          pack      MailAttach from "c:\work\pdf\",prtname  ."
          move      Mailattach,Str55
          Move      MailAttach,Str45

CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck



          move    "Here is your PDF File",MailSubjct
          Clear     MailBody
          append    "Cost Savings Report",Mailbody
          append    CRLF,Mailbody
          reset     Mailbody
          pack      Mailto from user,"@nincal.com"
          pack      MailFrom from user,"@nincal.com"
          pack      MailAttach from "c:\work\pdf\",prtname  ."
          call      SendMail
.Clean up afterwards
          pause c7
          erase     mailattach
          shutdown   "cls"

         STOP

TOTOUT
          if (RecordHeader = 0)
                    shutdown  "cls"
          endif
         MOVE      TOTSUM TO N5
         SUB       C1 FROM N5
         move      n5 to str5
da       cmatch    b1 to str5
         if        equal
         bump      str5 by 1
         goto      da
         endif
.......................
.Discount portion
        if (OUTNAME = "2")
.begin patch xxx
.                add     C5,howmany,N7
.                move    N7,str7
.                call    Trim using str7
.                pack    RangeStr,"I",str7
.                sheet.range("I1",RangeStr).Insert *Shift=RightShift
.                pack    RangeStr,"K",str7
.                sheet.range("K1",RangeStr).Insert *Shift=RightShift
.                pack    RangeStr,"N",str7
.                sheet.range("N1",RangeStr).Insert *Shift=RightShift
.                move    RecordHeader,str10
.                call    Trim using str10
.                pack    RangeStr,"K",str10
.                setprop sheet.range(RangeStr),*Value="ADDT'L COST"
.                pack    RangeStr,"N",str10
.                setprop sheet.range(RangeStr),*Value="DISCOUNTED"
.
.                setprop sheet.range(RangeStr),*Value="TOTAL"
.                add     C1,RecordHeader,N10
.                move    N10,str10
.                call    Trim using str10
.                pack    RangeStr,"I",str10
.                setprop sheet.range(RangeStr),*Value="BASE"
.                pack    RangeStr,"J",str10
.                setprop sheet.range(RangeStr),*Value="ADJUSTED"
.                pack    RangeStr,"K",str10
.                setprop sheet.range(RangeStr),*Value="IF ALL ON"
.
.                pack    RangeStr,"N",str10
.                setprop sheet.range(RangeStr),*Value="PRICE"
.                pack    RangeStr,"O",str10
.                setprop sheet.range(RangeStr),*Value="COST"
.                add     C1,N10
.                move    N10,str10
.                call    Trim using str10
.                pack    RangeStr,"I",str10
.                setprop sheet.range(RangeStr),*Value="PRICE"
.                pack    RangeStr,"K",str10
.                setprop sheet.range(RangeStr),*Value="RENTAL"
.
.                pack    RangeStr,"L",str10
.                setprop sheet.range(RangeStr),*Value="SAVINGS"
.                pack    RangeStr,"O",str10
.                setprop sheet.range(RangeStr),*Value="SAVINGS"
.end patch xxx
                for     N9,RecordTop,howmany
                        move    N9,str9
                        call    Trim using str9
                        pack    RangeStr,"M",str9
                        getprop sheet.range(RangeStr),*Formula=str35
                        rep     "JI",str35
                        setprop sheet.range(RangeStr),*Formula=str35
                repeat
        endif
.Set up Header Formatting
        add     C2,RecordHeader,N9
        move    RecordHeader,str9
        move    N9,str10
        call    Trim using str9
        call    Trim using str10
        pack    RangeStr,"A",str9
.......................
        if (OUTNAME = "2")
                pack    RangeStr2,"AD",str10
        elseif (OUTNAME = "1")
                    pack    RangeStr2,"Z",str10
          else
.begin patch xxx
.                pack    RangeStr2,"AB",str10
                pack    RangeStr2,"AC",str10
.end patch xxx
        endif
        setprop sheet.range(RangeStr,RangeStr2).Font,*Bold="True"
        setprop sheet.range(RangeStr,RangeStr2),*HorizontalAlignment=xlHAlignCenter,*ShrinkToFit="True"
        sheet.range(RangeStr,RangeStr2).BorderAround using *LineStyle=1,*Weight=2
.Right Border for Column "Output Cost per M"
........................
        if (OUTNAME = "2")
                pack    RangeStr,"R",str9
                pack    RangeStr2,"R",str10
        elseif (OUTNAME = "1")
                    pack    RangeStr,"N",str9
                    pack    RangeStr2,"N",str10
        else
                pack    RangeStr,"Q",str9
                pack    RangeStr2,"Q",str10
        endif
        setprop sheet.range(RangeStr,RangeStr2).Borders(xlEdgeRight),*LineStyle=xlContinuous
.
        pack    str11,"1:",str10
        setprop sheet.PageSetup,*PrintTitleRows=str11
.Create Formulas
.Sum Formula
        move    RecordTop,str9
        call    Trim using str9
        clear   str25
        append  "=Sum(C",str25
        append  str9,str25
        append  ":C",str25
        move    howmany,str9
        call    Trim using str9
        append  str9,str25
        append  ")",str25
        reset   str25
        add     C1,howmany,N9
        move    N9,str9
        call    Trim using str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr),*Value="Totals:"
        setprop sheet.range(RangeStr).Font,*Bold="True"
        Move     Howmany,Lastrec
.Continuous Line Before Totals
.........................
        if (OUTNAME = "2")
                pack    RangeStr2,"AD",str9
        elseif (OUTNAME = "1")
                    pack    RangeStr2,"Z",str9
          else
.begin patch xxx
.                pack    RangeStr2,"AB",str9
                pack    RangeStr2,"AD",str9
.end patch xxx
        endif
        setprop sheet.range(RangeStr,RangeStr2).Borders(xlEdgeTop),*LineStyle=xlContinuous
.

        pack    RangeStr,"C",str9
        setprop sheet.range(RangeStr),*Formula=str25
.
        pack    RangeStr,"G",str9
        rep     "CG",str25
        setprop sheet.range(RangeStr),*Formula=str25
.
        pack    RangeStr,"H",str9
        rep     "GH",str25
        setprop sheet.range(RangeStr),*Formula=str25
.
.
..........................
        if (OUTNAME = "2")
.addl cost
                pack    RangeStr,"K",str9
                rep     "HK",str25
                 setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

.discount
                pack    RangeStr,"L",str9
                rep     "KL",str25
                 setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

                pack    RangeStr,"N",str9
                rep     "LN",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.non billable names
                pack    RangeStr,"M",str9
                rep     "NM",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.output        
                pack    RangeStr,"O",str9
                rep     "MO",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.savings
                pack    RangeStr,"L",str9
                rep     "OL",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.output
                pack    RangeStr,"O",str9
                rep     "LO",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"

                pack    RangeStr,"P",str9
.=ROUND(O31/C31*1000,2)                
                clear str35 
                append  "=Round(O",str35
                append  str9,str35
                append  "/C",str35
                append  str9,str35
                append  ",4)",str35
                reset   str35
                setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"

                pack    RangeStr,"Q",str9
.=ROUND(G31/O31*1000,2)                
                clear str35 
                append  "=Round(G",str35
                append  str9,str35
                append  "/O",str35
                append  str9,str35
                append  "*1000,2)",str35
                reset   str35
                setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"


          elseif (OUTNAME = "1")                   .other discount option

                    pack    RangeStr,"J",str9
                    rep     "HJ",str25
                    setprop sheet.range(RangeStr),*Formula=str25

                    pack    RangeStr,"K",str9
                    rep     "JK",str25
                    setprop sheet.range(RangeStr),*Formula=str25

                    pack    RangeStr,"L",str9
                    rep     "KL",str25
                    setprop sheet.range(RangeStr),*Formula=str25
        else                                                                 .not discounted
                    add       C2,N9,result2
                    move      result2,str12
                    call      Trim using str12
                    pack      str15,"K",str12


                pack    RangeStr,"K",str9
                rep     "HK",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

                pack    RangeStr,"M",str9
                rep     "KM",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

                pack    RangeStr,"N",str9
                rep     "MN",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"
.percent
.=(K36/(G36+K36+M36))
                 pack      str45,"=(K",str9,"/(G",str9,"+K",str9,"+M",str9,"))"
.
                pack    RangeStr,"L",str9
                rep     "NL",str25
                setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0);_(#"-#"_)"

                pack    RangeStr,"K",str9
                rep     "LK",str25
               setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

        endif
           
.
.........................

.Percentage Formula
        clear   str35
..........................
        if (OUTNAME = "2")
                append  "=Round(P",str35
          elseif (OUTNAME = "1")
                    append  "=Round(L",str35
           append  str9,str35
           append  "/C",str35
           append  str9,str35
           append  ",4)",str35
           reset   str35
           setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"
        else
.DH testing 2015 july 27
           append  "=Round(N",str35
           append  str9,str35
           append  "/C",str35
           append  str9,str35
           append  ",4)",str35
           reset   str35
           pack    RangeStr,"O",str9
           rep     "KO",str25
           setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"
        endif
.
.........................
        if (OUTNAME = "2")

          elseif (OUTNAME = "1")
                    pack    RangeStr,"M",str9
        else
                pack    RangeStr,"O",str9
        setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="##,####0.00%_);_(#"-#"_)"
        endif
.
.Average Formula
        clear   str35
        append  "=Round(G",str35
        append  str9,str35
.........................
        if (OUTNAME = "2")
                append  "/P",str35
          elseif (OUTNAME = "1")
                    append  "/L",str35
        else
                append  "/N",str35
        endif
        append  str9,str35
        append  "*1000,2)",str35
        reset   str35
.........................
        if (OUTNAME = "2")
.         pack    RangeStr,"Q",str9
.        setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
          elseif (OUTNAME = "1")
                    pack    RangeStr,"N",str9
        setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
        else
                pack    RangeStr,"P",str9
        setprop sheet.range(RangeStr),*Formula=str35,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
        endif
.Formulas for possible Discounts
        if (OUTNAME = "2")
.                add     C1,N9,N10
.                move    N10,str10
.                call    Trim using str10
.                clear   str25
.                append  "=(K",str25
.                append  str9,str25
.                append  "/(G",str25
.                append  str9,str25
.                append  "+K",str25
.                append  str9,str25
.                append  "+M",str25
.                append  str9,str25
.                append  "))",str25
.                reset   str25
.
.                clear   str25
.                append  "=(M",str25
.                append  str9,str25
.                append  "/(G",str25
.                append  str9,str25
.                append  "+K",str25
.                append  str9,str25
.                append  "+M",str25
.                append  str9,str25
.                append  "))",str25
.                reset   str25
.                pack    RangeStr,"M",str10
.                setprop sheet.range(RangeStr),*Formula=str25
.
.                clear   str25
.                append  "=(N",str25
.                append  str9,str25
.                append  "/(N",str25
.                append  str9,str25
.                append  "+G",str25
.                append  str9,str25
.                append  "))",str25
.                reset   str25
..if rental formula
.                pack    RangeStr,"k",str10
.                setprop sheet.range(RangeStr),*Formula=str25

.Put formulas in individual records.  This is done here as we have inserted the whole column
.after individual records are created.  Therefore we need a loop to populate these cells.
.begin patch xxx   this is already done on outname = "2"
           if         (outname <> "2")
                move    RecordTop,result
                loop
                        move    result,str10
                        call    Trim using str10
.
                        clear   str55
                        append  "=IF(J",str55
                        append  str10,str55
                        append  ">0,IF(I",str55
                        append  str10,str55
                        append  ">0,(I",str55
                        append  str10,str55
                        append  "-J",str55
                        append  str10,str55
                        append  ")*(H",str55
                        append  str10,str55
                        append  "/1000),0),0)",str55
                        reset   str55

                        pack    RangeStr,"K",str10
                        setprop sheet.range(RangeStr),*Formula=str55,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.
                        clear   str25
                        append  "=Sum(K",str25
                        append  str10,str25
                        append  "+M",str25
                        append  str10,str25
                        append  ")",str25
                        reset   str25
                        pack    RangeStr,"N",str10
                        setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
.
                        add     C1,result
                        until (result > howmany)
                repeat
           endif
.end patch xxx   this is already done
        endif
.
        MOVe   Howmany,Lastrec          
        add           c1,lastrec
        move          howmany,Str10
        call          trim using str10
        add     C1,N9
        move    N9,str9
        call    Trim using str9
        pack    RangeStr,"E",str9
        setprop sheet.range(RangeStr),*Value="Paid:"
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Value=C0,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"

.Formula for Balance Due
        add     C1,howmany,N10
        move    N10,str10
        call    Trim using str10
        clear   str25
        append  "=Sum(G",str25
        append  str10,str25
        append  ":G",str25
        move    N9,str9
        call    Trim using str9
        append  str9,str25
        append  ")",str25
        reset   str25
.
        add     C1,N9
        move    N9,str9
        call    Trim using str9
        pack    RangeStr,"E",str9
        setprop sheet.range(RangeStr),*Value="Balance Due:"
        setprop sheet.range(RangeStr).Font,*Bold="True"
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)"
        setprop sheet.range(RangeStr).Borders(xlEdgeTop),*LineStyle=xlContinuous
        setprop sheet.range(RangeStr).Borders(xlEdgeBottom),*LineStyle=xlDouble
        setprop sheet.range(RangeStr).Font,*Bold="True"
        if            (outname = "2")   
.=(K73/(G73+K73+N73))
           call       debug
                      move       lastrec,str9
                      call       trim using str9
                        clear    str25 
                        append  "=(K",str25
                        append  str9,str25
                        append  "/G",str25
                        append  str9,str25
                        append  "+K",str25
                        append  str9,str25
                        append  "+N",str25
                        append  str9,str25
                        append  ")",str25
                        reset   str25
                       move    N9,str9
                       call    Trim using str9
                        pack    RangeStr,"K",str9
                        setprop sheet.range(RangeStr),*Formula=str25,*NumberFormat="##,####0.00%_);_(#"-#"_)"
           endif

.........................
.Set Column Widths for larger fields
        move    howmany,str9
        call    Trim using str9
        pack    RangeStr,"E1"
        pack    RangeStr2,"E",str9
.START PATCH 3.6 REPLACED LOGIC
.        sheet.range(str4,str5).Columns.Autofit
.When you use CR character to force two or more items on one row,
.MSExcel is hard coded to automatically set the WrapText property.
.This is a problem as Excel will take current column width and wrap
.all the text to fit in that set width.
.So... We need to unset it, establish a very wide column width using
.Autofit (It will be applying to both the List Name and Select Name on the single line-the
.CR character is ignored).
.Then set the WrapText property.  At this point the Column Width is super wide,
.so the text will not bunch up.  And lastly, reset the Autofit to shrink down to a
.reasonable width.
        setprop sheet.range(RangeStr,RangeStr2),*Wraptext=OFALSE
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
        setprop sheet.range(RangeStr,RangeStr2),*Wraptext=OTRUE
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
          getprop   sheet.range(RangeStr,RangeStr2).Columns,*ColumnWidth=xlColWidth
          getprop   xlColWidth,VarValue=N32
          add       "2.0",N32
          setprop   xlColWidth,VarValue=N32
          setprop   sheet.range(RangeStr,RangeStr2).Columns,*ColumnWidth=xlColWidth
        pack    RangeStr,"A1"
        pack    RangeStr2,"BZ5000"
        setprop sheet.range(RangeStr,RangeStr2),*VerticalAlignment=xlVAlignTop

.........................
       if (OUTNAME = "2")
               pack    RangeStr,"Y1"
               pack    RangeStr2,"Y",str9
       elseif (OUTNAME = "1")
               pack    RangeStr,"L13"
               pack    RangeStr2,"L",str9
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
               pack    RangeStr,"V1"
               pack    RangeStr2,"V",str9
       else
               pack    RangeStr,"X1"
               pack    RangeStr2,"X",str9
       endif
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
       if (OUTNAME = "2")
               move   str9,n9
               add    c1,n9
               move   n9,str10
               move   str9,n9      .restore string
               call    trim using str10
               pack    RangeStr,"C1"
               pack    RangeStr2,"C",str10
               sheet.range(RangeStr,RangeStr2).Columns.Autofit
               pack    RangeStr,"G1"
               pack    RangeStr2,"G",str10
               sheet.range(RangeStr,RangeStr2).Columns.Autofit
               pack    RangeStr,"H1"
               pack    RangeStr2,"H",str10
               sheet.range(RangeStr,RangeStr2).Columns.Autofit
       endif    
.........................
       if (OUTNAME = "2")
               pack    RangeStr,"Z1"
               pack    RangeStr2,"Z",str9
       elseif (OUTNAME = "1")
               pack    RangeStr,"W1"
               pack    RangeStr2,"W",str9
       else
               pack    RangeStr,"Y1"
               pack    RangeStr2,"Y",str9
       endif
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
.Hide Column 
        pack    RangeStr,"D1"
        pack    RangeStr2,"D",str9
        setprop sheet.range(RangeStr,RangeStr2).Columns,*ColumnWidth=Inch03
.Sort by List Name
drew
        pack    str6,"AZ",str9
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
        move    RecordTop,str9
        call    Trim using str9
        pack    RangeStr2,"E",str9
        getprop sheet.range(RangeStr2),*Columns(1)=rowcol
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    RangeStr2,"A",str9
        sheet.range(RangeStr2,str6).sort using *Key1=rowcol,*Order1=1
.
.begin patch 4.44
.        if (OUTNAME <> "2" & OUTNAME <> "1")
        if (OUTNAME <> "1")
.                    sub       C2,N9,N10
.                    move      N10,str10
                    Move         Lastrec,str10
                    call      Trim using str10
                    if           (outname = "2")
                    pack      taskname,"=(K",str10,"+L",str10,"+N",str10,")"
                    else
                    pack      taskname,"=(K",str10,"+M",str10,")"
                    endif
.
                    add     C5,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"E",str9
.                    setprop sheet.range(RangeStr),*Value="Savings on this campaign due to negotiating: exchanges, pricing, and net arrangements:"
                     IF (Outname = "2") 
                    setprop sheet.range(RangeStr),*Value="Savings on this campaign due to negotiating: exchanges, pricing, net arrangements and discounted price savings:"
                    else  
                    setprop sheet.range(RangeStr),*Value="Savings on this campaign due to negotiating: exchanges, pricing, net arrangements:"
                    endif
.Formula
.                    pack    RangeStr,"K",str9
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Formula=taskname
                  setprop sheet.range(RangeStr).Borders(xlEdgeTop),*LineStyle=xlContinuous
          setprop sheet.range(RangeStr).Borders(xlEdgeBottom),*LineStyle=xlContinuous
                    pack      RangeStr2,"I1"
                  sheet.range(RangeStr2,RangeStr).Columns.Autofit
.
.                    pack      RangeStr2,"K1"
                    pack      RangeStr2,"K10"
                  sheet.range(RangeStr2,RangeStr).Columns.Autofit


                    add     C3,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"E",str9
                    setprop sheet.range(RangeStr),*Value="If list is not available on a rental basis, a $75.00 rental price is used for calculation."
                    setprop sheet.range(RangeStr).Font,*Italic="True"
                    setprop sheet.range(RangeStr).Font,*Size=8
                    add     C2,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="Payment due upon receipt.  Please return a copy of report with payment to:"
                    add     C1,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="Names in the News"
                    add     C1,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="180 Grand Avenue, Ste ##1365"
                    add     C1,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"A",str9
                    setprop sheet.range(RangeStr),*Value="Oakland, CA 94612-3716"
          endif
          Move      c1,str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr).Font,*Bold="True"
.        sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
        sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logo color box only.jpg",OTRUE,OTRUE,0,0,55,45

        call    Trim using str9
        pack    RangeStr,"E",str9
        setprop sheet.range(RangeStr),*Value="COST SAVINGS/BILLING SUMMARY REPORT"
        setprop sheet.range(RangeStr).Font,*Bold="True"


XReportFileNameSelect
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
        move    N2,str2
        rep     zfill,str2
        clear   taskname
        append  "\\nins1\d\accounting\clients\",taskname                                ."
        append  MNUM,taskname
        append  MailDate,taskname
        append  "_",taskname
        append  str2,taskname
          if        (#ver = c1)
          append  ".xlsx",taskname
          else
        append  ".xls",taskname
          endif
        reset   taskname
        setprop ex,*DisplayAlerts=OFALSE
        clear   APIFileName
        pack    APIFileName,taskname,hexzero
        call    FindFirstFile
        if (APIResult <> 0 & APIResult <> hexeight)
.File exists, try incrementing name
                goto TrapXReportObject2
        endif
        trap    TrapXReportObject if Object
        book.saveas giving N9 using *Filename=taskname
        trapclr Object
.        endif
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.        book.printout
XReportCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
          goto      Stop
         shutdown  "cls"
         STOP
TrapXReportObject
        noreturn
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.
TrapXReportObject2
        if (N2 >= 99)
                goto XReportCleanUp     .Quit the job
        endif
        add     C1,N2
.Send them back to select another File name and try to Save again.
        goto XReportFileNameSelect
.
.*............................................................
.*......................................................................
.* ***************************************************************************
.*  EXIT AND FERROR SUBROUTINES
.* ****************************************************************************
.
abort    DISPLAY   *P01:01,*ES,*P29:12,*HON,"JOB ABORTED !!!",*HOFF,*R;
         BEEP
.
EXIT     DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         BEEP
        goto XReportCleanUp

WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"Cost Savings pdf - ",mailattach
.                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    "mailTo = ",mailbody
                    append    mailto,mailbody
                    append    CRLF,MailBOdy
                    append    "maiLFrom = ",mailbody
                    append    maiLFrom,mailbody
                    
                    append    CRLF,MailBOdy
                    append    str45,MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Clear     Mailto
                    Pack      MailTO,"CReques@nincal.com"
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile




.
           include   nacdio.inc
           include   ndatio.inc
           include   nownio.inc
           include   nshpio.inc
           INCLUDE   NORD2IO.inc
           INCLUDE   NJstIO.inc
           INCLUDE   NADJIO.inc
           INCLUDE        ninvio.inc
           INCLUDE        NINVAcdIO.inc
           INCLUDE        compute.inc
           include   compio.inc
           include   cntio.inc
           include   nmrgio.inc
           INCLUDE   NDAT3IO.INC
           INCLUDE   NSEL2IO.INC
           include   nmodio.inc
           INCLUDE   NSELIO.INC
           inc       nsel3io.inc
           inc       nrefio.inc
           inc       naddio.inc
           inc       nsltio.inc
           INCLUDE   COMLOGIC.inc

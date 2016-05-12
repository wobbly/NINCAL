PC       EQU       0
               INCLUDE        COMMON.inc
               INCLUDE        CONS.inc
               INCLUDE        CONSACCT.inc
.begin patch 3.8
.               INCLUDE        NINVDD.inc
               INCLUDE        ninvdd.inc
               INCLUDE        NINVAcddd.inc
.begin patch 3.8
               include        nacddd.inc
               include        ndatdd.inc
               include        nshpdd.inc
               INCLUDE        NORD2DD.inc
.patch1.5
                              include   compdd.inc
                              include   cntdd.inc
.               INCLUDE        NMLRDD.inc
.patch1.5
               include        hp.inc
               INCLUDE        NMRGDD.inc
               INCLUDE        NJstDD.inc
               INCLUDE        NADJDD.inc
               INCLUDE        NOWNDD.INC
.patch1.5
.               include        nbrkdd.inc
.patch1.5
               include        nbildd.inc
               INCLUDE        NDAT3DD.INC
               include        winapi.inc
.START PATCH 3.73 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
          include   nmoddd.inc
.END PATCH 3.73 ADDED LOGIC
.START PATCH 3.75 ADDED LOGIC
          INCLUDE   NSELDD.INC
.END PATCH 3.75 ADDED LOGIC
release   init      "4.2"             DLH         .Spreadsheet rearrangement
Reldate   Init      "10 April 2012"
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
.START PATCH 3.75 ADDED LOGIC
.EXTERNAL ROUTINES FROM       NDAT001a.PLC
SelectTestBase4 external "NDAT001a;SelectTestBase4"
N52       form      5.2
result2   form      9
.PriceFlag form     1
xlUnderlineStyleSingle      integer 4,"0x2"
.END PATCH 3.75 ADDED LOGIC
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
.hexeight       integer        4,"4294967295"
SReturn        init 0x0a                     ;soft return/line feed
VT_BOOL        EQU            11          .Boolean
OTRUE          variant
OFALSE         variant
VT_I4          EQU            3           .4 byte integer
Zoom90         variant
Zoom75         variant
.START PATCH 3.75 ADDED LOGIC
Zoom72         variant
.END PATCH 3.75 ADDED LOGIC
.START PATCH 3.76.3 ADDED LOGIC
Zoom80         variant
.END PATCH 3.76.3 ADDED LOGIC
Inch1286       variant
VT_R8          EQU            5           .Double - 8 byte Real
Inch03         variant
Inch25         variant         ..25 Inch
Inch50         variant         ..50 Inch
Inch70         variant         ..70 Inch
Inch100        variant         .1.0 Inch
.START PATCH 3.72 ADDED LOGIC
xlColWidth          variant
.END PATCH 3.72 ADDED LOGIC
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
.begin release 3.6
XlVAlignTop    Integer  4,"0xFFFFEFC0"          .-4160
.end release 3.6
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom75,VarType=VT_I4,VarValue=75
.START PATCH 3.75 ADDED LOGIC
        create  Zoom72,VarType=VT_I4,VarValue=72
.END PATCH 3.75 ADDED LOGIC
.START PATCH 3.76.3 ADDED LOGIC
        create  Zoom80,VarType=VT_I4,VarValue=80  .GS first change from original specs
.END PATCH 3.76.3 ADDED LOGIC
        create  Zoom90,VarType=VT_I4,VarValue=90
.Convert Inches to Points.  There are 72 Points to an Inch
        create  Inch03,VarType=VT_R8,VarValue="3"
        create  Inch25,VarType=VT_R8,VarValue="18"      ..25 Inch
        create  Inch50,VarType=VT_R8,VarValue="36"      ..5 Inch
        create  Inch70,VarType=VT_R8,VarValue="51"      ..7 Inch
        create  Inch100,VarType=VT_R8,VarValue="72"     .1.0 Inch
        create  Inch1286,VarType=VT_R8,VarValue="12.86"
.START PATCH 3.72 ADDED LOGIC
          create    xlColWidth,VarType=VT_R8,VarValue="1.0"
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.9 ADDED LOGIC
RangeStr  dim       12
RangeStr2 dim       12
.END PATCH 3.9 ADDED LOGIC


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
.begin patch 4.01
TotLCR    Form      3                       Number of LCRS
.end patch 4.01
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
totbmask dim       15
totmask  dim       15
balmask  dim       15
costmask dim       15
trejmask dim       13            prtdet4 - non mailable names
qoutmask dim       13            prtdet4 -output qty
ppmmask init      "$$,$$$.99"
ppmout   dim       9
.START PATCH 3.75 REPLACED LOGIC
.mess1     dim       4
mess1     dim       5
mess3     dim       3
.END PATCH 3.75 REPLACED LOGIC
mess2    dim       11
F94      FORM      9.4
F32      FORM      3.2
F34      FORM      3.4
F34splt  FORM      3.4
code16SW dim       1
QTYadjSW dim       1
bigdate  dim       25
bigdat2  dim       25
M01      INIT      "January"
M02      INIT      "February"
M03      INIT      "March"
M04      INIT      "April"
M05      INIT      "May"
M06      INIT      "June"
M07      INIT      "July"
M08      INIT      "August"
M09      INIT      "September"
M010     INIT      "October"
M011     INIT      "November"
M012     INIT      "December"
str100   Dim        100
.START PATCH 3.93 ADDED LOGIC
HOLDEXCL dim        1
.END PATCH 3.93 ADDED LOGIC
.
. .............................................................................
. MAINLINE
. .............................................................................
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................
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
         SCAN      "ORDQTY" IN COMMENT
         IF        EQUAL
         MOVE      C2 TO QTYFLAG
         ELSE
         MOVE      C1 TO QTYFLAG
         ENDIF
         MOVE       C1 TO JOBFLAG
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
.begin patch 28 sept 2011
.         BRANCH    JOBFLAG TO PRTOK,OUTPUT,prtok,prtok,prtok
         BRANCH    JOBFLAG TO PRTOK,Prtok,prtok,prtok,prtok
.end patch 28 sept 2011
PRTOK    
          SPLOPEN   PRTFILE
.          Splopen   "\\nins2\laser2","W"
.begin patch 28 sept 2011
         BRANCH    JOBFLAG TO Begin,output,begin,begin,begin
.end patch 28 sept 2011
         GOTO       begin
OUTPUT
.Open Excel application
        create  ex
.START PATCH 3.71 REPLACED LOGIC
..START PATCH 3.7 ADDED LOGIC
.         setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 3.7 ADDED LOGIC
.        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.DH testing July 2010
        setprop ex,*Visible="False",*IgnoreRemoteRequests="True",*Interactive="False"
.DH testing
.START PATCH 3.73 TEMPORARY PATCH
.         setprop ex.CommandBars("Standard"),*Visible="True"
.         setprop ex.CommandBars("Formatting"),*Visible="True"
.         setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 3.73 TEMPORARY PATCH
.END PATCH 3.71 REPLACED LOGIC
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
.        setprop sheet.PageSetup,*PrintQuality=300
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
.START PATCH 3.75 REPLACED LOGIC
.        if (OUTNAME = "2")
.                setprop sheet.PageSetup,*PrintArea="$A:$Q"
.                setprop sheet.PageSetup,*Zoom=Zoom75
.        else
.                setprop sheet.PageSetup,*PrintArea="$A:$N"
.                setprop sheet.PageSetup,*Zoom=Zoom90
.        endif
...................................
        if (OUTNAME = "2")
                setprop sheet.PageSetup,*PrintArea="$A:$Q"
                setprop sheet.PageSetup,*Zoom=Zoom75
        elseif (OUTNAME = "1")
                setprop sheet.PageSetup,*PrintArea="$A:$N"
                setprop sheet.PageSetup,*Zoom=Zoom90
          else
                setprop sheet.PageSetup,*PrintArea="$A:$Q"
.START PATCH 3.76.3 REPLACED LOGIC
.                setprop sheet.PageSetup,*Zoom=Zoom72
.GS first change from original specs - 8/4/2004
                    setprop sheet.PageSetup,*Zoom=Zoom80
.END PATCH 3.76.3 REPLACED LOGIC
        endif
.END PATCH 3.75 REPLACED LOGIC
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
.begin patch 4.0
          Move      c3,Ndatlock                       .no updates happening do not lock the files
          Move      c3,Nordlock
.end patch 4.0
         
LOOP     DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
         CALL      NORD2SEQ
         if        over
         move      c2 to eojflag
         goto      eoj
         endif
         if         (olrn = "766557")
         call       debug
         endif
.START PATCH 3.75 MOVED TO 'WRITEOK1'
.JD turned back on for unbilled records. 6/17/04.
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
..START PATCH 3.73 ADDED LOGIC
.         packkey   NSEL2FLD,"1",OLRN
.         move      "NSEL2KEY",Location
.         pack      KeyLocation,"Key: ",NSEL2FLD
.         call      NSEL2KEY
.         if over
.                   move      O2DES,NSEL2NAME
.                   unpack    OPPM,str3,str2
.                   pack      str6,str3,".",str2
.                   rep       zfill,str6
.                   move      str6,NSEL2PRICE
..                  move      "/m",NMODDESC
..        else
..                  pack      NMODFLD,NSEL2DESC
..                  rep       zfill,NMODFLD
..                  move      "NMODKEY",Location
..                  pack      KeyLocation,"Key: ",NMODFLD
..                  call      NMODKEY
..                  if over
..                            move      "/m",NMODDESC
..                  endif
.         endif
..END PATCH 3.73 ADDED LOGIC
.END PATCH 3.75 MOVED TO 'WRITEOK1'
.begin patch 4.01                    .is an be an issue as default pickoff does not excludes LCR's
         move      "lzpx" to str4
.         move      "zpx" to str4                  .take live lcsrs
.end patch 4.01
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
.START PATCH 3.93 ADDED LOGIC
          move      CompExcl,HOLDEXCL
.END PATCH 3.93 ADDED LOGIC
         cmatch     "A" to mcode     .batch & adjust?
         if         equal
         compare    c3 to jobflag
         if         equal
         move       c4 to jobflag    .yes       was turned on 6/13  DLH
         endif
         endif
         endif

.begin patch 3.76
.START PATCH 3.76.7 REMOVED LOGIC
.               call           Trim using Commper
.END PATCH 3.76.7 REMOVED LOGIC
.ARGH
.end patch 3.76
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
.START PATCH 3.75 REPLACED LOGIC
.         move      b4 to mess1
         move      b5 to mess1
.END PATCH 3.75 REPLACED LOGIC
         cmatch    "1",oelcode
         if        equal
.START PATCH 3.75 REPLACED LOGIC
.         move      b4 to mess1
         move      "RENT" to mess1
.END PATCH 3.75 REPLACED LOGIC
         goto      exexit
         endif
         CMATCH    "2",OELCODE
         GOTO      CHECKOK IF EQUAL
CHECK2
         CMATCH    "3",OELCODE
         GOTO      CHECKOK IF EQUAL
.START PATCH 3.75 ADDED LOGIC
         move      "RENT" to mess1
.END PATCH 3.75 ADDED LOGIC
         GOTO      exexit
CHECKOK
         MOVE      C0 TO Num9b
         MOVE      OEXQTY TO Num9b
         COMPARE    C0 TO Num9b
         GOTO       CHECKOK1 IF NOT EQUAL
         move      "EXCH" to mess1
         GOTO      exexit
CHECKOK1
.START PATCH 3.75 REPLACED LOGIC
.          move      "SPLT" to mess1
           move      "SPLIT" to mess1
.END PATCH 3.75 REPLACED LOGIC
         move      c2 to spltflag
exexit   RESET     RUNCODES
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
.begin patch 28 sep 2011
.         compare    c1 to page
.         if         equal
.         branch     JOBFLAG to hdok,hdout,hdok3,hdok4,hdok
.         endif
.         branch     JOBFLAG to hdok,hdout,hdok3,hdok4,hdok
         branch     JOBFLAG to hdok,hdok,hdok3,hdok4,hdok
.end patch 28 sep 2011
.
.START PATCH 3.76.4 REPLACED LOGIC
.hdok     PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                    *N,*N,*N,*5,hpbon,"Names",hpboff,hpitalic," in the News Ca., Inc.",hpuprght:
.                   *N,*5,"Date: ",TODAY:
.                   *N,*5,"Client: ",ORDCNAME:
.                   *N,*5,"LIST COST REPORT":
.                   *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                   *55,"Mlr Po",*65,"Maildate",*74,"M/P ##";
.START PATCH 3.93 ADDED LOGIC
.hdok     PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                    *N,*N,*N,*5,hpbon,"Names in the News",hpboff:
.                   *N,*5,"Date: ",TODAY:
.                   *N,*5,"Client: ",ORDCNAME:
.                   *N,*5,"LIST COST REPORT":
.                   *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                   *55,"Mlr Po",*65,"Maildate",*74,"M/P ##";
hdok
.begin patch 4.0
.          if (HOLDEXCL = "P")
.                    PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                              *N,*N,*N,*5,hpbon,"Pacific Lists",hpboff:
.                              *N,*5,"Date: ",TODAY:
.                              *N,*5,"Client: ",ORDCNAME:
.                              *N,*5,"LIST COST REPORT":
.                              *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                              *55,"Mlr Po",*65,"Maildate",*74,"M/P ##";
.          else

                    PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
                              hpbon,*60,"Names in the News",hpboff:
                              *N,*60,"180 Grand Ave Ste 1545":
                              *N,*60,"Oakland, Ca 94612-3799":
                              *N:
                              *N,*5,"Date: ",TODAY:
                              *N,*5,"Client: ",ORDCNAME:
                              *N,*5,"LIST COST REPORT":
                              *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
                              *55,"Mlr Po",*65,"Maildate",*74,"M/P ##";
.          endif
.end patch 4.0
.END PATCH 3.93 ADDED LOGIC
.END PATCH 3.76.4 REPLACED LOGIC
         PRINT     *1,*RPTCHAR "_":35,*37,*RPTCHAR "_":6,*44,*RPTCHAR "_":10:
                   *55,*RPTCHAR "_":9,*65,*RPTCHAR "_":8,*74,*RPTCHAR "_":6,*N
         MOVE      C10 TO LINES
          goto      HDout
.         branch     JOBFLAG to hd1exit,hdout
hd1exit
         RETURN
.
HD2      COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
.START PATCH 3.76.4 REPLACED LOGIC
.         PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                   *N,*N,*N,*5,hpbon,"Names",hpboff,hpitalic," in the News Ca., Inc.",hpuprght:
.                   *N,*5,"Date: ",TODAY:
.                   *N,*5,"Client: ",ORDCNAME:
.                   *N,*5,"LIST COST REPORT":
.                   *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                   *55,"Mlr Po",*65,"Maildate/MP##/Qty";
.START PATCH 3.93 ADDED LOGIC
.         PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                   *N,*N,*N,*5,hpbon,"Names in the News",hpboff:
.                   *N,*5,"Date: ",TODAY:
.                   *N,*5,"Client: ",ORDCNAME:
.                   *N,*5,"LIST COST REPORT":
.                   *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                   *55,"Mlr Po",*65,"Maildate/MP##/Qty";
.begin patch 4.0
.          if (HOLDEXCL = "P")
.                    PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
.                              *N,*N,*N,*5,hpbon,"Pacific Lists",hpboff:
.                              *N,*5,"Date: ",TODAY:
.                              *N,*5,"Client: ",ORDCNAME:
.                              *N,*5,"LIST COST REPORT":
.                              *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
.                              *55,"Mlr Po",*65,"Maildate/MP##/Qty";
.          else
                    PRINT     hptop,hpdtch10,hpfixed,*F,*L,*L,*L,*36,"CONFIDENTIAL":
                              *N,*N,*N,*5,hpbon,"Names in the News",hpboff:
                              *N,*5,"Date: ",TODAY:
                              *N,*5,"Client: ",ORDCNAME:
                              *N,*5,"LIST COST REPORT":
                              *L,*L,*L,*1,"List Name/Select",*37,"Lr##",*44,"Cost":
                              *55,"Mlr Po",*65,"Maildate/MP##/Qty";
.          endif
.end patch 4.0
.END PATCH 3.93 ADDED LOGIC
.END PATCH 3.76.4 REPLACED LOGIC
         PRINT     *1,*RPTCHAR "_":35,*37,*RPTCHAR "_":6,*44,*RPTCHAR "_":10:
                   *55,*RPTCHAR "_":9,*65,*RPTCHAR "_":15,*N
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
         PRINT     *F;
         call       PortraitLTRHEAD
         print      hpt300,033,"&a0c0R":          invoke macro.
                   *L:
                   *n,*N,*n,*n,*n,hpdtch10,hpbon,hpt275,"BILLING SUMMARY REPORT"
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
         print     *n,*N,hpt000,hpdtch10,hpbon,"Mailer Name: ":
                   hpt150,ORDCNAME,hpt450,"Billing Date:":
                   hpt600,bigdate:
                   *n,hpt000,hpdtch10,hpbon,"Mailer##: ",hpt150,omlrnum:
                   hpt450,"Mailer P.O. No.:",hpt600,omlrpon;
         cmatch    yes to mbildrct            .bill direct?  14apr95 dlh
          if       not equal
          match     b1 to ordmname
                  if        not equal
                  print    *n,hpt000,"Through ",hpt150,ordmname:
                            hpt450,"Merge ##:",hpt600,omlrky:
                           *n,hpt000,"Ship To:",hpt150,BILADDR:
                            hpt450,"Mail Date:",hpt600,bigdat2:
                           *n,hpt150,BILCITY," ",BILSTATE," ",BILZIP
                 add        c3 to lines
                 else
                         goto      hdok3a
                 endif
         else
                 goto      hdok3a
         endif
         goto     hdok3b
hdok3a
         print    *n,hpt450,"Merge ##:",hpt600,omlrky:
                  *n,hpt000,"Ship To:",hpt150,BILADDR:
                  hpt450,"Mail Date:",hpt600,bigdat2:
                  *n,hpt150,BILCITY," ",BILSTATE," ",BILZIP
         add        c3 to lines
hdok3b
        print      *L,*L,hpunon,hpt000,hpt1050,hpunoff:
                   *L,hpbon,hpt000,"List",hpt075,"  Input",hpt200,"List ":
                   hpt525,"Total",hpt600,"    Qty",hpt675,"   Base":
                   *l,hpunon,hpt000,"Rental ##",hpt075,"  Qty",hpt200,"Description":
                   hpt525,"Billed",hpt600,"    Billed",hpt675,"   Price  ",hpunoff,hpboff
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
         PRINT     *F
         call       LandscapeLTRHEAD
         print     hpland,hplin8:
                   033,"&a0c0R":
                   *L:
                   *n,*N,*n,hpt425,hpdtch10,hpbon:
                   *n,hpt425," BILLING SUMMARY REPORT ":
                   hpboff,*N
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
         print     *n,*N,hpt000,hpdtch10,hpbon,"Mailer Name: ":
                   hpt150,ORDCNAME,hpt600,"Billing Date:":
                   hpt750,bigdate:
                   *n,hpt000,hpdtch10,hpbon,"Mailer##: ",hpt150,omlrnum:
                   hpt600,"Mailer P.O. No.:",hpt750,omlrpon;
         cmatch    yes to mbildrct            .bill direct?  14apr95 dlh
          if       not equal
          match     b1 to ordmname
                  if        not equal
                  print    *n,hpt000,"Through ",hpt150,ordmname:
                            hpt600,"Merge ##:",hpt750,omlrky:
                           *n,hpt000,"Ship To:",hpt150,BILADDR:
                            hpt600,"Mail Date:",hpt750,bigdat2:
                           *n,hpt150,BILCITY," ",BILSTATE," ",BILZIP
                 add        c3 to lines
                 else
                         goto      hdok4a
                 endif
         else
                 goto      hdok4a
         endif
         goto     hdok4b
hdok4a
         print    *n,hpt600,"Merge ##:",hpt750,omlrky:
                  *n,hpt000,"Ship To:",hpt150,BILADDR:
                  hpt600,"Mail Date:",hpt750,bigdat2:
                  *n,hpt150,BILCITY," ",BILSTATE," ",BILZIP
         add        c3 to lines
hdok4b print      *L,hpunon,hpt000,hpt1050,hpunoff:
                   *L,hpbon,hpt625,hpt725,"Base",hpt600:
                   hpt950,"     Output":
                   *l,hpt000,"List",hpt075,"Input",hpt150,"List ":
                   hpt425,"Total",hpt500,"   Qty",hpt575,"   Base",hpt625,"  Deductible":
                   hpt725,"Cost",hpt775,"  Output",hpt875,"   %":
                   hpt950,"     Cost":
                   *l,hpunon,hpt000,"Rental ##",hpt075,"Qty",hpt150,"Description":
                   hpt425,"Billed",hpt500,"   Billed",hpt575,"   Price",hpt625,"  Names":
                   hpt725,"Savings",hpt775,"  Qty",hpt875,"Output",hpt950:
                   "     Per M         ":
                   hpunoff,hpboff

          goto      hdout

         RETURN

hdout
.Report Header
.begin patch 4.1
          if        (page > 1)
          return
          endif
.end patch 4.1
        move    C0,RecordTop
        move    C0,RecordHeader
        move    howmany,str9       ."1"
          
        call    Trim using str9
        pack    RangeStr,"A",str9
.START PATCH 3.76.1 REPLACED LOGIC
.        setprop sheet.range(str4),*Value="NAMES IN THE NEWS/CA"
.START PATCH 3.93 REPLACED LOGIC
.        setprop sheet.range(RangeStr),*Value="NAMES IN THE NEWS"
.begin patch 4.0
.          if (HOLDEXCL = "P")
.          setprop sheet.range(RangeStr),*Value="PACIFIC LISTS"
.        else
.        sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.          setprop sheet.range(RangeStr),*Value="NAMES IN THE NEWS"
.        endif
.end patch 4.0
.END PATCH 3.93 REPLACED LOGIC
.END PATCH 3.76.1 REPLACED LOGIC

.        setprop sheet.range(RangeStr).Font,*Bold="True"
.
        add     C1,howmany
        move    howmany,str9       ."2"
        Move      C6,str9
..        pack    RangeStr,"A",str9
.        pack    RangeStr,"E",str9
.        setprop sheet.range(RangeStr),*Value="COST SAVINGS/BILLING SUMMARY REPORT"
.START PATCH 3.76.3 REPLACED LOGIC
.        pack    str4,"G",str9
.        pack    RangeStr,"I",str9
        call    Trim using str9
.begin patch 4.2
.        pack    RangeStr,"L",str9
        pack    RangeStr,"G",str9
.end patch 4.2
.END PATCH 3.76.3 REPLACED LOGIC
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
.START PATCH 3.75 ADDED LOGIC
          if (OUTNAME = "0")  .Includes Rental Savings Portion
          pack    RangeStr,"L",str9
                  setprop sheet.range(RangeStr),*Value="e o      exchange list only"
          endif
.END PATCH 3.75 ADDED LOGIC
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
.START PATCH 3.75 REPLACED LOGIC
.        pack    str4,"J",str9
.        setprop sheet.range(str4),*Value="NON-"
.        pack    str4,"K",str9
.        setprop sheet.range(str4),*Value="BASE"
.        pack    str4,"N",str9
.        setprop sheet.range(str4),*Value="OUTPUT"
.;
.        add     C1,howmany
.        move    howmany,str9       ."12"
.        call    Trim using str9
.        pack    str4,"A",str9
.        setprop sheet.range(str4),*Value="ORDER ##"
.;...................................................
.        pack    str4,"B",str9
.        setprop sheet.range(str4),*Value="INVOICE ##"
.        pack    str4,"C",str9
.        setprop sheet.range(str4),*Value="INPUT"
.        pack    str4,"E",str9
.        setprop sheet.range(str4),*Value="LIST"
.        pack    str4,"F",str9
.        setprop sheet.range(str4),*Value="NET"
.        pack    str4,"G",str9
.        setprop sheet.range(str4),*Value="AMOUNT"
.        pack    str4,"H",str9
.        setprop sheet.range(str4),*Value="QTY"
.        pack    str4,"I",str9
.        setprop sheet.range(str4),*Value="BASE"
.        pack    str4,"J",str9
.        setprop sheet.range(str4),*Value="BILLABLE"
.        pack    str4,"K",str9
.        setprop sheet.range(str4),*Value="COST"
.        pack    str4,"L",str9
.        setprop sheet.range(str4),*Value="OUTPUT"
.        pack    str4,"M",str9
.        setprop sheet.range(str4),*Value="%"
.        pack    str4,"N",str9
.        setprop sheet.range(str4),*Value="COST"
.        pack    str4,"O",str9
.        setprop sheet.range(str4),*Value="EXCH/"
.        pack    str4,"P",str9
.        setprop sheet.range(str4),*Value="NIN COMM."
.        pack    str4,"Q",str9
.        setprop sheet.range(str4),*Value="GROSS"
.        pack    str4,"R",str9
.        setprop sheet.range(str4),*Value="*PAID*"
.        pack    str4,"S",str9
.        setprop sheet.range(str4),*Value="QTY"
.        pack    str4,"T",str9
.        setprop sheet.range(str4),*Value="INVOICE"
.        pack    str4,"U",str9
.        setprop sheet.range(str4),*Value="PAYMENT"
.        pack    str4,"V",str9
.        setprop sheet.range(str4),*Value="MAILER"
.        pack    str4,"W",str9
.        setprop sheet.range(str4),*Value="THRU"
.        pack    str4,"X",str9
.        setprop sheet.range(str4),*Value="MERGE"
.        pack    str4,"Y",str9
.        setprop sheet.range(str4),*Value="MAIL"
.        pack    str4,"Z",str9
.        setprop sheet.range(str4),*Value="LIST ##"
.;
.        add     C1,howmany
.        move    howmany,str9       ."13"
.        call    Trim using str9
.        pack    str4,"C",str9
.        setprop sheet.range(str4),*Value="QTY"
.        pack    str4,"F",str9
.        setprop sheet.range(str4),*Value="ORDERED"
.        pack    str4,"G",str9
.        setprop sheet.range(str4),*Value="BILLED"
.        pack    str4,"H",str9
.        setprop sheet.range(str4),*Value="BILLED"
.        pack    str4,"I",str9
.        setprop sheet.range(str4),*Value="PRICE"
.        pack    str4,"J",str9
.        setprop sheet.range(str4),*Value="NAMES"
.        pack    str4,"K",str9
.        setprop sheet.range(str4),*Value="SAVINGS"
.        pack    str4,"L",str9
.        setprop sheet.range(str4),*Value="QTY"
.        pack    str4,"M",str9
.        setprop sheet.range(str4),*Value="OUTPUT"
.        pack    str4,"N",str9
.        setprop sheet.range(str4),*Value="PER M"
.        pack    str4,"O",str9
.        setprop sheet.range(str4),*Value="SPLIT"
.        pack    str4,"P",str9
.        setprop sheet.range(str4),*Value="%"
.        pack    str4,"Q",str9
.        setprop sheet.range(str4),*Value="DIFF."
.        pack    str4,"R",str9
.        setprop sheet.range(str4),*Value="DATE"
.        pack    str4,"U",str9
.        setprop sheet.range(str4),*Value="DATE"
.        pack    str4,"Y",str9
.        setprop sheet.range(str4),*Value="DATE"
.        add     C1,howmany,RecordTop    .save this value for formatting later
..........................................
        if (OUTNAME = "2" | OUTNAME = "1")
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
.START PATCH 3.76.1 REPLACED LOGIC
.                   setprop sheet.range(str4),*Value="*BASE"
                    setprop sheet.range(RangeStr),*Value="BASE"
.END PATCH 3.76.1 REPLACED LOGIC
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
          endif
        add     C1,howmany,RecordTop    .save this value for formatting later
.END PATCH 3.75 REPLACED LOGIC
        return
..............................................................................
PAGENUM  branch    jobflag of pg55,pg55,pg58,pg58,pg55
pg55     COMPARE   "55" TO LINES
         IF        NOT EQUAL
         GOTO      LINE1
         ENDIF
         goto      pagenum1
pg58     COMPARE   "58" TO LINES
         IF        NOT EQUAL
         GOTO      LINE1
         ENDIF
         goto      pagenum1
pagenum1 MOVE      PAGEMASK TO PAGENUM
         EDIT      PAGE TO PAGENUM
.         BRANCH    JOBFLAG TO PGOK,SKIPPG,pgok,pgok4,pgok
         BRANCH    JOBFLAG TO PGOK,PGok,pgok,pgok4,pgok
.PGOK     PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2002, ",hpbon,"Names",hpboff,hpitalic," in the News/CA":
.START PATCH 3.76.4 REPLACED LOGIC
.PGOK     PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2004, ",hpbon,"Names",hpboff," in the News":
.                   hpuprght,hpdtch85,hpt375,PAGENUM
.START PATCH 3.93 REPLACED LOGIC
.PGOK     PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2005, ",hpbon,"Names in the News",hpboff:
.                   hpuprght,hpdtch85,hpt375,PAGENUM
PGOK
.begin patch 4.0
.          if (HOLDEXCL = "P")
..START PATCH 3.96.1
..                   PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2005, ",hpbon,"Pacific Lists",hpboff:
..                             hpuprght,hpdtch85,hpt375,PAGENUM
..                   PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2008, ",hpbon,"Pacific Lists",hpboff:
..                             hpuprght,hpdtch85,hpt375,PAGENUM
..START PATCH 3.98.1
.                    PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2009, ",hpbon,"Pacific Lists",hpboff:
.                              hpuprght,hpdtch85,hpt375,PAGENUM
.          else
.                   PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2005, ",hpbon,"Names in the News",hpboff:
.                             hpuprght,hpdtch85,hpt375,PAGENUM
.                   PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2008, ",hpbon,"Names in the News",hpboff:
.                             hpuprght,hpdtch85,hpt375,PAGENUM
                    PRINT     *L,hpt000,hpprop,copyrite,hpdtch06,"1992-2012, ",hpbon,"Names in the News",hpboff:
                              hpuprght,hpdtch85,hpt375,PAGENUM
..End PATCH 3.96.1
..End PATCH 3.98.1
.          endif
.end patch 4.0
.END PATCH 3.93 REPLACED LOGIC
.END PATCH 3.76.4 REPLACED LOGIC
         goto      skippg
.PGOK4    PRINT     *L,hpt000,copyrite,hpdtch06,hpbon,"Names",hpboff,hpitalic," in the News/CA":
.START PATCH 3.76.4 REPLACED LOGIC
.PGOK4    PRINT     *L,hpt000,copyrite,hpdtch06,hpbon,"Names",hpboff," in the News":
.                   hpuprght,hpdtch85,hpt950,"Page ",PAGENUM
.START PATCH 3.93 REPLACED LOGIC
.PGOK4    PRINT     *L,hpt000,copyrite,hpdtch06,hpbon,"Names in the News",hpboff:
.                   hpuprght,hpdtch85,hpt950,"Page ",PAGENUM
PGOK4
.begin patch 4.0
.          if (HOLDEXCL = "P")
.                    PRINT     *L,hpt000,copyrite,hpdtch06,hpbon,"Pacific Lists",hpboff:
.                              hpuprght,hpdtch85,hpt950,"Page ",PAGENUM
.          else
                    PRINT     *L,hpt000,copyrite,hpdtch06,hpbon,"Names in the News",hpboff:
                              hpuprght,hpdtch85,hpt950,"Page ",PAGENUM
.          endif
.end patch 4.0
.END PATCH 3.93 REPLACED LOGIC
.END PATCH 3.76.4 REPLACED LOGIC
SKIPPG   ADD       C1 TO PAGE
         move      c0 to lines
         branch   JOBFLAG to pagexit,pagexit,callhead,PAGEXIT,pagexit
pagexit  RETURN
callhead compare   c2 to eojflag
         goto      pagexit if equal
         perform   jobflag of abort,abort,hdok3,hdok4,abort
         return
.
.LINE1    BRANCH    JOBFLAG TO PRTBLANK,SKIPBLNK,prtblank,prtblank,prtblank
LINE1    BRANCH    JOBFLAG TO PRTBLANK,PRTBLaNK,prtblank,prtblank,prtblank
PRTBLANK PRINT     B1
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
         CMATCH    "B" TO OSTAT
         GOTO      BILLED IF EQUAL
         CMATCH    "Q" TO OSTAT
         GOTO      BILLED IF EQUAL
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
.         move      c0 to cvtjul
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
.begin patch 3.96
          If        (str2 = "06" | str2 = "19" | str2 = "27" | str2 = "28")
.         MATCH     "06" TO STR2
.         IF        EQUAL
         MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
.         ELSE
.            MATCH     "19" TO STR2
.            IF        EQUAL
.            MOVE      YES TO LSTMSW
.            endif
           endif
.end patch 3.96
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
.START PATCH 3.73 REPLACED LOGIC
.         MOVE      OPPM TO FORM52            .move cost /m to numeric
.         div       hund into form52
          move      NSEL2PRICE,FORM52
.END PATCH 3.73 REPLACED LOGIC
         MULT      form52 BY NEt94
         MOVE      net94 TO GROSS               GROSS BILLING.

         move      runc94 to net94           .calc running charge goodies
         divide    thous into net94         .get ready for cost per M
         mult        onetrc by net94         .running charges on estimated unused names
         add       net94 to gross            .add to cost
.
         move      runc94 to net94           .calc commission on list cost savings
         divide    thous into net94         .get ready for cost per M
.START PATCH 3.73 REPLACED LOGIC
.         MOVE      OPPM TO FORM52            .move cost /m to numeric
.         div       hund into form52
          move      NSEL2PRICE,FORM52
.END PATCH 3.73 REPLACED LOGIC
         mult      form52 by net94              .
.START PATCH 3.76.7 REMOVED LOGIC
.         MOVE      c0 TO FORM32
.         MOVE      COMMPer TO FORM32          .commission rate from datacard
.         if        (form32 <= 0)
.         move      "20" to form32             .if datacard value null assume 20%
.         endif
.        mult       ".01" by form32
.         mult      form32 by net94              .extra commission ie savings fee
.................................
         MOVE      c0 TO FORM34
         MOVE      COMMPer TO FORM34          .commission rate from datacard
         if        (form34 <= 0)
         move      "20" to form34             .if datacard value null assume 20%
         endif
        mult       ".01" by form34
         mult      form34 by net94              .extra commission ie savings fee
.END PATCH 3.76.7 REMOVED LOGIC
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
.START PATCH 3.73 REPLACED LOGIC
.         MOVE      OPPM TO FORM52            .move cost /m to numeric
.         DIV       HUND INTO FORM52          .lets get the pennies
          move      NSEL2PRICE,FORM52
.END PATCH 3.73 REPLACED LOGIC
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
.START PATCH 3.73 REPLACED LOGIC
.         scan       "@$" in o2des              .any charges
.         goto       scanexch if not equal      .nope
.         bump       o2des by 2                .found one, get ready
.         movefptr   o2des to mfp               .set formpointer
.o1       bump       o2des                      .forward one more
.         goto       scanexch if eos            .oops nothing there get out
.         cmove      o2des to str1              .get character
.         type       str1                      .numeric ?
.         goto       o1 if equal               .yes, we will take it
.         cmatch     "." to str1               .decimal
.         goto       o1 if equal               .yes, we will take it
.         movefptr   o2des to mll               .get current position
.         reset      o2des to mfp              .reset formpointer
.         sub        c1 from mll          .move lp back to last good character
.         setlptr    o2des to mll              .set length pointer
.         move       o2des to str6             .take it
.         move       str6 to charge
.;         display   *p1:24,*el,"charge ",o2des,b1,str6,b1,charge,b1,mfp,b1,mll,*w4
.         call       sel$                      .calc it
.         move       mll to mfp                .get ready to look again
.;         add        c2 to mll                 .get ready to look again
.         setlptr    o2des to 35                .reset length to max
.         reset      o2des to mfp               .reset formpointer past 1st hit
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
.END PATCH 3.73 REPLACED LOGIC
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
.begin patch 3.76.8
.         MOVE       C7 TO FORM52
         MOVE       C5 TO FORM52
.end patch 3.76.8
         GOTO       CALCE
         ENDIF
         match      "0171" to obrknum             .dawson?
         IF         EQUAL
.begin patch 3.76.8
.         MOVE       C7 TO FORM52
         MOVE       C5 TO FORM52
.end patch 3.76.8
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
.begin patch 3.97
          if        (cmpt92 < 50)
          move      "50.00",Unbilinc
          endif
.end patch 3.97       
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
.begin patch 3.97
.         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         MOVE      "85.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
.end patch 3.97
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
.         BRANCH     JOBFLAG OF PRTDET1,DETOUT,detout,detout,prtdet1
         BRANCH     JOBFLAG OF PRTDET1,PRTDET1,detout,detout,prtdet1
PRTDET1
         PRINT     *L,hpdtch10,hpfixed,*1,O1DES,B1,OLRN,*42,FORMAR,hp17ptch:
                   b1,OMLRPON,hpdtch10,hpfixed,*65,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY:
                   *76,OMLRKY
         MOVE      NO TO PAID
.begin patch 4.1 ?
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
.end patch 4.1  ?
.begin patch 4.1
          if        (jobflag = c2)
          call      detout
          endif
.end patch 4.1
         ADD       C2 TO LINES
         BRANCH      QTYFLAG OF DISS1,DISS2
         GOTO       DISS1
.
PRTDET3
         compare   "55" to lines
         call      pagenum if equal
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
         PRINT     hpt000,hpdtch85,hpbon,lrn,"*",hpt075,hpfixed,hp17ptch,ordqty10:
                   hpprop,hpdtch85,hpt200,O1DES,hpt500:
                   hpbon,hpfixed,HP17PTCH,Arout:
                   hpt600,shpqty,hpt675,ppmout,hpboff,hpprop,hpdtch85;
         else
         PRINT     hpt000,hpdtch85,lrn,hpt075,hpfixed,hp17ptch,ordqty10:
                   hpprop,hpdtch85,hpt200,O1DES,hpt500:
                   hpfixed,HP17PTCH,Arout:
                   hpt600,shpqty,hpt675,ppmout,hpprop,hpdtch85;
         endif
         MOVE      NO TO PAID
.begin patch 4.1
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
.end patch 4.1
         ADD       C1 TO LINES
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         print     hpt800,hpprop,hpdtch06,"Estimated",hpdtch85
         else
         print     b1
         ENDIF
         return
.
PRTDET4
         compare   "55" to lines
         call      pagenum if equal
         move      ordmask10 to ordqty10
         edit      Num9 to ordqty10
         move      shpmask to shpqty
         move      ordmask10 to trejmask        .non mailable names
         add       totrej to gtrej
         edit      totrej to trejmask                  "
         move      ordmask10 to qoutmask        .output qty
         add       nmrgnet to gtnet
         edit      nmrgnet to qoutmask                "
         edit      billqty to shpqty
         add       billqty to totnbill              .total names billed
         move      armask to arout           .total billed $
         edit      formar to arout                 "
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
         PRINT     hpt000,hpdtch85,hpbon,lrn,"*",hpt050,hpfixed,hp17ptch,ordqty10:
                   hpprop,hpdtch06,hpt150,hpbon,O1DES,HPDTCH85,hpt400,hpfixed,hp17ptch:
                   hpbon,Arout,hpt500,shpqty,hpt575,ppmout:
                   hpt625,trejmask,hpt725,bcstmask,hpt775,qoutmask:
                   hpt875,percout,hpt950:
                   costmask,hpdtch85,hpboff
         else
         PRINT     hpt000,hpdtch85,lrn,hpt050,hpfixed,hp17ptch,ordqty10:
                   hpprop,hpdtch06,hpt150,O1DES,HPDTCH85,hpt400,hpfixed,hp17ptch:
                   Arout,hpt500,shpqty,hpt575,ppmout:
                   hpt625,trejmask,hpt725,bcstmask,hpt775,qoutmask:
                   hpt875,percout,hpt950:
                   costmask,hpdtch85
         endif
         MOVE      NO TO PAID
         ADD       C2  TO LINES
.begin patch 4.1
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
.end patch 4.1
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         print     hpt800,hpprop,hpdtch06,"Estimated",hpdtch85
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
.begin patch 3.91
         add        nmrgdisa to totrej            
.begin patch 3.91
.begin patch 3.95
         add        nmrgcnr to totrej            
.end patch 3.95
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
.START PATCH 3.73 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
.START PATCH 3.75 ADDED LOGIC
                    move      NSEL2NAME,taskname
                    call      Trim using taskname
.END PATCH 3.75 ADDED LOGIC
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
          endif
.START PATCH 3.75 ADDED LOGIC
.         move      C0,PriceFlag
          clear     mess3
          if (OELCODE = "2" | OELCODE = "3")      .Exchange Records
                    if (NSEL2PRICE2 = C0)
.Prepare Default Value
                              move      "75",NSEL2PRICE2
.                             move      C1,PriceFlag
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
.START PATCH 3.76.1 ADDED LOGIC
                    if (PPM > NSEL2PRICE2)
                              move      PPM,NSEL2PRICE2
                    endif
.END PATCH 3.76.1 ADDED LOGIC
          else
                    move      PPM,NSEL2PRICE2
          endif
.END PATCH 3.75 ADDED LOGIC
.END PATCH 3.73 ADDED LOGIC
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
        setprop sheet.range(RangeStr),*Value=Num9
.START PATCH 3.75 ADDED LOGIC
        pack    RangeStr,"D",str9
        setprop sheet.range(RangeStr),*Value=mess3
.END PATCH 3.75 ADDED LOGIC
        pack    RangeStr,"E",str9
.begin patch 3.6
        call    Trim using O1DES
        clear    str100
.        rep       LowUp in o1des                     ;set to upper case
        append   o1des,str100
        append   Sreturn,str100
.START PATCH 3.73 REPLACED LOGIC
.        rep      Uplow in o2des                       ;set to lower case
..START PATCH 3.72 ADDED LOGIC
.         call      Trim using O2DES
..END PATCH 3.72 ADDED LOGIC
.        append   o2des,str100
....................
        rep      Uplow in NSEL2NAME                       ;set to lower case
          call      Trim using NSEL2NAME
        append   NSEL2NAME,str100
.END PATCH 3.73 REPLACED LOGIC
        reset    str100
        setprop sheet.range(RangeStr),*Value=str100
.option 1 define cell as text word wrap - set to 35 characters, pad 01des to 35 and append 02des
.end patch 3.6
        pack    RangeStr,"F",str9
        call    Trim using ONETPER
        if (ONETPER = "" | ONETPER = "0")
                clear   str3
        else
                pack    str3,PERIOD,ONETPER
        endif
        setprop sheet.range(RangeStr),*Value=str3
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Value=FORMAR
        pack    RangeStr,"H",str9
        setprop sheet.range(RangeStr),*Value=QTYBILD
.START PATCH 3.75 REPLACED LOGIC
.        pack    str4,"I",str9
.        setprop sheet.range(str4),*Value=PPM
.
.;Substitute Rejects for a Formula
.;Non-Billable Names Formula  .column J
.        pack    str4,"J",str9
.        clear   str35
.        append  "=Sum(C",str35
.        append  str9,str35
.        append  "-H",str35
.        append  str9,str35
.        append  ")",str35
.        reset   str35
.        setprop sheet.range(str4),*Formula=str35
.;
.;Base Cost Formula   .column K
.        clear   str35
.        append  "=Round(J",str35
.        append  str9,str35
.        append  "/1000*I",str35
.        append  str9,str35
.        append  ",2)",str35
.        reset   str35
.        pack    str4,"K",str9
.        setprop sheet.range(str4),*Formula=str35
.;
.;   .column L
.        pack    str4,"L",str9
.        setprop sheet.range(str4),*Value=NMRGNET
.;
.;Percentage Formula    .column M
.        pack    str4,"M",str9
.        clear   str35
.        append  "=Round(L",str35
.        append  str9,str35
.        append  "/C",str35
.        append  str9,str35
.        append  ",4)",str35
.        reset   str35
.        setprop sheet.range(str4),*Formula=str35
.;Output Cost Formula     .column N
.        clear   str35
.        append  "=Round(G",str35
.        append  str9,str35
.        append  "/L",str35
.        append  str9,str35
.        append  "*1000,2)",str35
.        reset   str35
.        pack    str4,"N",str9
.        setprop sheet.range(str4),*Formula=str35
.        pack    str4,"O",str9
.        setprop sheet.range(str4),*Value=MESS1
.        pack    str4,"P",str9
.        move    COMMPCT,N3
.        mult    N3,".01",N34
.        setprop sheet.range(str4),*Value=N34
.        pack    str4,"Q",str9
.        div     "1000",GROSS,N92
.        setprop sheet.range(str4),*Value=N92
.        pack    str4,"R",str9
.        setprop sheet.range(str4),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
.        pack    str4,"S",str9
.        setprop sheet.range(str4),*Value=N9
.        pack    str4,"T",str9
.;Value must be converted from Julian Date to Standard Date
.        clear   str10
.        call    Trim using INVDTEM
.        if (INVDTEM <> "")
.                pack    str10,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
.        endif
.        setprop sheet.range(str4),*Value=str10,*NumberFormat="m/d/yyyy"
.        pack    str4,"U",str9
.;Value must be converted from Julian Date to Standard Date
.        clear   str10
.        call    Trim using CHK1DTEM
.        if (CHK1DTEM <> "")
.                pack    str10,CHK1DTEM,SLASH,CHK1DTED,SLASH,CHK1DTEC,CHK1DTEY
.        endif
.        setprop sheet.range(str4),*Value=str10,*NumberFormat="m/d/yyyy"
.        pack    str4,"V",str9
.        setprop sheet.range(str4),*Value=ordcname
.        pack    str4,"W",str9
.        setprop sheet.range(str4),*Value=ordmname
.        pack    str4,"X",str9
.        setprop sheet.range(str4),*Value=OMLRKY
.        pack    str4,"Y",str9
.;Value must be converted from Julian Date to Standard Date
.        clear   str10
.        call    Trim using OMDTEM
.        if (OMDTEM <> "")
.                pack    str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.        endif
.        setprop sheet.range(str4),*Value=str10,*NumberFormat="m/d/yyyy"
.        pack    str4,"Z",str9
.        setprop sheet.range(str4),*Value=OLNUM
..........................................
        if (OUTNAME = "2" | OUTNAME = "1")
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value=PPM
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
                    setprop sheet.range(RangeStr),*Formula=str35
.Base Cost Formula   .column K
                    clear   str35
                    append  "=Round(J",str35
                    append  str9,str35
                    append  "/1000*I",str35
                    append  str9,str35
                    append  ",2)",str35
                    reset   str35
                    pack    RangeStr,"K",str9
                    setprop sheet.range(RangeStr),*Formula=str35
.   .column L
                    pack    RangeStr,"L",str9
                    setprop sheet.range(RangeStr),*Value=NMRGNET
.Percentage Formula    .column M
                    pack    RangeStr,"M",str9
                    clear   str35
                    append  "=Round(L",str35
                    append  str9,str35
                    append  "/C",str35
                    append  str9,str35
                    append  ",4)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35
.Output Cost Formula     .column N
                    clear   str35
                    append  "=Round(G",str35
                    append  str9,str35
                    append  "/L",str35
                    append  str9,str35
                    append  "*1000,2)",str35
                    reset   str35
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Formula=str35
                    pack    RangeStr,"O",str9
                    setprop sheet.range(RangeStr),*Value=MESS1
                    pack    RangeStr,"P",str9
                    move    COMMPCT,N3
                    mult    N3,".01",N34
                    setprop sheet.range(RangeStr),*Value=N34
                    pack    RangeStr,"Q",str9
                    div     "1000",GROSS,N92
                    setprop sheet.range(RangeStr),*Value=N92
                    pack    RangeStr,"R",str9
                    setprop sheet.range(RangeStr),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
                    pack    RangeStr,"S",str9
                    setprop sheet.range(RangeStr),*Value=N9
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
          else
                    pack    RangeStr,"I",str9
                    setprop sheet.range(RangeStr),*Value=NSEL2PRICE2
                    pack    RangeStr,"J",str9
                    setprop sheet.range(RangeStr),*Value=PPM
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
                    setprop sheet.range(RangeStr),*Formula=str35
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
                    setprop sheet.range(RangeStr),*Formula=str35
.Base Cost Formula   .column M
                    clear   str35
                    append  "=Round(L",str35
                    append  str9,str35
                    append  "/1000*J",str35
                    append  str9,str35
                    append  ",2)",str35
                    reset   str35
                    pack    RangeStr,"M",str9
                    setprop sheet.range(RangeStr),*Formula=str35
.   .column L
                    pack    RangeStr,"N",str9
                    setprop sheet.range(RangeStr),*Value=NMRGNET
.Percentage Formula    .column O
                    pack    RangeStr,"O",str9
                    clear   str35
                    append  "=Round(N",str35
                    append  str9,str35
                    append  "/C",str35
                    append  str9,str35
                    append  ",4)",str35
                    reset   str35
                    setprop sheet.range(RangeStr),*Formula=str35
.Output Cost Formula     .column P
                    clear   str35
                    append  "=Round(G",str35
                    append  str9,str35
                    append  "/N",str35
                    append  str9,str35
                    append  "*1000,2)",str35
                    reset   str35
                    pack    RangeStr,"P",str9
                    setprop sheet.range(RangeStr),*Formula=str35
                    pack    RangeStr,"Q",str9
.START PATCH 3.76.3 REPLACED LOGIC
.                   setprop sheet.range(str4),*Value=MESS1,*HorizontalAlignment=xlHAlignCenter
                    setprop sheet.range(RangeStr),*Value=MESS1
.END PATCH 3.76.3 REPLACED LOGIC
                    pack    RangeStr,"R",str9
                    move    COMMPCT,N3
                    mult    N3,".01",N34
                    setprop sheet.range(RangeStr),*Value=N34
                    pack    RangeStr,"S",str9
                    div     "1000",GROSS,N92
                    setprop sheet.range(RangeStr),*Value=N92
                    pack    RangeStr,"T",str9
                    setprop sheet.range(RangeStr),*Value=MESS2,*HorizontalAlignment=xlHAlignCenter
                    pack    RangeStr,"U",str9
                    setprop sheet.range(RangeStr),*Value=N9
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
          endif
.END PATCH 3.75 REPLACED LOGIC
         MOVE      NO TO PAID
.begin patch 4.1
          if        (ostat <> "l")
         ADD        C1 TO TOTSUM
          else      
          add       c1,Totlcr
          endif
.end patch 4.1
wrtexit  return
.
.
.
DISS1
         CLEAR     DISDATE
.START PATCH 3.73 ADDED LOGIC
.         PRINT    *01,O2DES,B1,pstatus;
         PRINT    *01,NSEL2NAME,B1,pstatus;
.END PATCH 3.73 ADDED LOGIC
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         CALL      ESTIMATE
         ENDIF
         PRINT     B1
         ADD       C1 TO LINES
         RETURN
.
DISS2
         CLEAR     DISDATE
         MOVE      OQTY TO Num9b
.START PATCH 3.73 ADDED LOGIC
.         PRINT    *01,O2DES,B1,pstatus;
         PRINT    *01,NSEL2NAME,B1,pstatus;
.END PATCH 3.73 ADDED LOGIC
         CMATCH    YES TO BILL
         IF        NOT EQUAL
         CALL      ESTIMATE
         ENDIF
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO Num9b
         EDIT      Num9b TO ORDQTY
         PRINT     *65,ORDQTY,B1
         ADD       C1 TO LINES
         RETURN

ESTIMATE if        (netflag = "2" or netflag = "3")
         PRINT    *40,hpbon,"*ESTIMATED ",hpunon,"Net",hpunoff," COST*",hpboff;
         else
         PRINT    *44,hpbon,"*ESTIMATED COST*",hpboff;
         endif
         RETURN
READINV
.......................................................................
.
. ROUTINE TO READ THE INVOICE RECORD AND COMPUTE CODE TOTALS.
.
         MOVE      OLRN TO NINVFLD
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
.               call           NInvAcdRecClear
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
.         RETURN
CALCBILL
         RETURN

newmlr
         compare   c0 to page
         goto      newmlrb if equal
         COMPARE   "52" TO LINES
         CALL      HD0 IF NOT LESS
.begin patch 4.1
          if        (TotLCR = c0)
         PRINT     *N,*1,"## of Orders in Summary   : ",TOTSUM:
                   *N,*1,"Total Estimated          : ",TOTALU:
                   *N,*1,"Total Invoiced Open      : ",TOTALB:
                   *N,*1,"Total Invoiced Paid      : ",TOTALP:
                   *N,*1,"Grand Total              : ",TOTAL
         add       c6 to lines
          else      
         PRINT     *N,*1,"## of Orders in Summary    : ",TOTSUM:
                   *N,*1,"## of Clearances in Summary: ",TOTALU:
                   *N,*1,"Total Estimated           : ",TOTALU:
                   *N,*1,"Total Invoiced Open       : ",TOTALB:
                   *N,*1,"Total Invoiced Paid       : ",TOTALP:
                   *N,*1,"Grand Total               : ",TOTAL
         add       c7 to lines
          endif
.end patch 4.1

.         PRINT     *N,*1,"## of Orders in Summary   : ",TOTSUM:
.                   *N,*1,"Total Estimated          : ",TOTALU:
.                   *N,*1,"Total Invoiced Open      : ",TOTALB:
.                   *N,*1,"Total Invoiced Paid      : ",TOTALP:
.                   *N,*1,"Grand Total              : ",TOTAL
.         add       c6 to lines
         call      pagenum
.begin patch 4.1
         move     c0 to totLCR
.end patch 4.1
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
         COMPARE   "52" TO LINES
         CALL      HD0 IF NOT LESS
.         BRANCH    JOBFLAG TO PRTTOT,TOTOUT,prttot3,prttot4,prttot
         BRANCH    JOBFLAG TO PRTTOT,PRTTOT,prttot3,prttot4,prttot
PRTTOT
         if         (jobflag = c2)
         goto       Totout
         endif
        if         (lines > 48)
        call       hd0
        endif
.begin patch 4.1
          if        (TotLCR = c0)       
         PRINT     *N,*1,"## of Orders in Summary   : ",TOTSUM:
                   *N,*1,"Total Estimated          : ",TOTALU:
                   *N,*1,"Total Invoiced Open      : ",TOTALB:
                   *N,*1,"Total Invoiced Paid      : ",TOTALP:
                   *N,*1,"Grand Total              : ",TOTAL:
                   *n:
                   *n,hpt025,"Estimates are deemed accurate when report generated. Based on previous usage and ":
                   *n,hpt025,"order information. Subject to Mailer change and negotiations with list owner."
         ADD       C9 TO LINES
          Else
         PRINT     *N,*1,"## of Orders in Summary   : ",TOTSUM:
                   *N,*1,"## of Clearances in Summary: ",TOTALU:
                   *N,*1,"Total Estimated          : ",TOTALU:
                   *N,*1,"Total Invoiced Open      : ",TOTALB:
                   *N,*1,"Total Invoiced Paid      : ",TOTALP:
                   *N,*1,"Grand Total              : ",TOTAL:
                   *n:
                   *n,hpt025,"Estimates are deemed accurate when report generated. Based on previous usage and ":
                   *n,hpt025,"order information. Subject to Mailer change and negotiations with list owner."
         ADD       C10 TO LINES
          endif
.end patch 4.1
         CALL      PAGENUM
         SPLCLOSE
         goto       stop
PRTTOT3
         compare    "45" to lines
         call       pagenum if not less
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
         edit      gtnet to qoutmask                "
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
         PRINT     hpunon,hpt000,hpt1050,hpunoff:
                   *N,hpt050,hpfixed,hp17ptch,hpbon,b2,str11,hpt500,totmask:
                   hpt600,bqtymask:
                   hpdtch85,hpboff
         PRINT     *N,hpt000,hpdtch10,hpbon,"Totals: ",hpboff,hpt200,hpfixed,totmask:
                   *n,hpt000,hpdtch10,hpbon,"*Total Paid invoices:",hpt200,hpfixed:
                   hpunon,hpboff,totpmask,hpunoff:
                   *N,hpt000,hpdtch10,hpbon,"BALANCE DUE:",hpt200,hpfixed,balmask:
                   *N,hpt000,hpunon,hpt1050,hpunoff:
                   *N,hpt000,hpunon,hpt1050,hpunoff:
                   *n, hpt000,hpbon,"Total Orders Processed:",hpboff,hpt200,totsum;
         compare   c2 to remtflag
         if        equal
         print     hpt525,hpdtch12,hpitalic,hpbon,"REMITTANCE COPY"
         PRINT     *n,*n,hpdtch85,hpt450,"(Please remit copy with payment, Thank you)"
         else
         print     hpt525,hpdtch12,hpitalic,hpbon,"CLIENT COPY"
         PRINT     *n,*n
         endif
         ADD       "12" TO LINES
         CALL      PAGENUM
         SPLCLOSE
         goto       stop
...............................................................................
PRTTOT4  compare    "45" to lines
         call       pagenum if not less
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
         PRINT     hpunon,hpt000,hpt1050,hpunoff:
                   *N,hpt025,hpfixed,hp17ptch,hpbon,b2,str11,hpt400:
                   hpt475,b2,bqtymask:
                   hpt625,trejmask,hpt725,bcstmask,hpt775,qoutmask:
                   hpt875,percout,hpt950:
                   costmask,hpdtch85,hpboff
         PRINT     *N,hpt000,hpdtch10,hpbon,"Totals: ",hpboff,hpt200,hpfixed,totmask:
                   *n,hpt000,hpdtch10,hpbon,"*Total Paid invoices:",hpt200,hpfixed:
                   hpunon,hpboff,totpmask,hpunoff:
                   *N,hpt000,hpdtch10,hpbon,"BALANCE DUE:",hpt200,hpfixed,balmask:
                   *N,hpt000,hpunon,hpt1050,hpunoff:
                   *N,hpt000,hpunon,hpt1050,hpunoff:
                   *n,*n, hpt000,hpbon,"Total Orders Processed:",hpboff,hpt200,totsum;
         compare   c2 to remtflag
         if        equal
         print     hpt525,hpdtch12,hpitalic,hpbon,"REMITTANCE COPY":
                   *n,*n,hpdtch85,hpt450,"(Please remit copy with payment, Thank you)"
         else
         print     hpt525,hpdtch12,hpitalic,hpbon,"CLIENT COPY":
                  *n,*n,hpdtch85
         endif
         ADD       "12" TO LINES
         CALL      PAGENUM
         SPLCLOSE
         goto       stop
stop     shutdown   "cls"
         STOP

TOTOUT
.START PATCH 3.76.5 ADDED LOGIC
          if (RecordHeader = 0)
                    shutdown  "cls"
          endif
.END PATCH 3.76.5 ADDED LOGIC
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
                add     C5,howmany,N7
                move    N7,str7
                call    Trim using str7
                pack    RangeStr,"I",str7
                sheet.range("I1",RangeStr).Insert *Shift=RightShift
                pack    RangeStr,"K",str7
                sheet.range("K1",RangeStr).Insert *Shift=RightShift
                pack    RangeStr,"N",str7
                sheet.range("N1",RangeStr).Insert *Shift=RightShift
                move    RecordHeader,str10
                call    Trim using str10
                pack    RangeStr,"K",str10
                setprop sheet.range(RangeStr),*Value="DISCOUNTED"
                pack    RangeStr,"N",str10
                setprop sheet.range(RangeStr),*Value="TOTAL"
                add     C1,RecordHeader,N10
                move    N10,str10
                call    Trim using str10
                pack    RangeStr,"I",str10
                setprop sheet.range(RangeStr),*Value="BASE"
                pack    RangeStr,"J",str10
                setprop sheet.range(RangeStr),*Value="ADJUSTED"
                pack    RangeStr,"K",str10
                setprop sheet.range(RangeStr),*Value="PRICE"
                pack    RangeStr,"N",str10
                setprop sheet.range(RangeStr),*Value="COST"
                add     C1,N10
                move    N10,str10
                call    Trim using str10
                pack    RangeStr,"I",str10
                setprop sheet.range(RangeStr),*Value="PRICE"
                pack    RangeStr,"K",str10
                setprop sheet.range(RangeStr),*Value="SAVINGS"
                pack    RangeStr,"N",str10
                setprop sheet.range(RangeStr),*Value="SAVINGS"
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
.START PATCH 3.75 REPLACED LOGIC
.        if (OUTNAME = "2")
.               pack    str5,"AC",str10
.       else
.               pack    str5,"Z",str10
.       endif
.......................
        if (OUTNAME = "2")
                pack    RangeStr2,"AC",str10
        elseif (OUTNAME = "1")
                    pack    RangeStr2,"Z",str10
          else
                pack    RangeStr2,"AB",str10
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Font,*Bold="True"
        setprop sheet.range(RangeStr,RangeStr2),*HorizontalAlignment=xlHAlignCenter,*ShrinkToFit="True"
        sheet.range(RangeStr,RangeStr2).BorderAround using *LineStyle=1,*Weight=2
.Right Border for Column "Output Cost per M"
.START PATCH 3.75 REPLACED LOGIC
.        if (OUTNAME = "2")
.               pack    str4,"Q",str9
.             pack    str5,"Q",str10
.       else
.               pack    str4,"N",str9
.               pack    str5,"N",str10
.       endif
........................
        if (OUTNAME = "2")
                pack    RangeStr,"Q",str9
                pack    RangeStr2,"Q",str10
        elseif (OUTNAME = "1")
                    pack    RangeStr,"N",str9
                    pack    RangeStr2,"N",str10
        else
                pack    RangeStr,"Q",str9
                pack    RangeStr2,"Q",str10
        endif
.END PATCH 3.75 REPLACED LOGIC
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
.Continuous Line Before Totals
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str5,"AC",str9
.       else
.               pack    str5,"Z",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr2,"AC",str9
        elseif (OUTNAME = "1")
                    pack    RangeStr2,"Z",str9
          else
                pack    RangeStr2,"AB",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
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
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"L",str9
.               rep     "HL",str25
.       else
.               pack    str4,"J",str9
.               rep     "HJ",str25
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"L",str9
                rep     "HL",str25
          elseif (OUTNAME = "1")
                    pack    RangeStr,"J",str9
                    rep     "HJ",str25
        else
                pack    RangeStr,"K",str9
                rep     "HK",str25
                  setprop sheet.range(RangeStr),*Formula=str25
.
                    add       C2,N9,result2
                    move      result2,str12
                    call      Trim using str12
                    pack      str15,"K",str12
.=(K36/(G36+K36+M36))
                    pack      str45,"=(K",str9,"/(G",str9,"+K",str9,"+M",str9,"))"
                  setprop sheet.range(str15),*Formula=str45
.
                pack    RangeStr,"L",str9
                rep     "KL",str25
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr),*Formula=str25
.
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"M",str9
.               rep     "LM",str25
.       else
.               pack    str4,"K",str9
.               rep     "JK",str25
.       endif
..........................
        if (OUTNAME = "2")
                pack    RangeStr,"M",str9
                rep     "LM",str25
          elseif (OUTNAME = "1")
                    pack    RangeStr,"K",str9
                    rep     "JK",str25
        else
                pack    RangeStr,"M",str9
                rep     "LM",str25
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr),*Formula=str25
.
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"O",str9
.               rep     "MO",str25
.       else
.               pack    str4,"L",str9
.               rep     "KL",str25
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"O",str9
                rep     "MO",str25
          elseif (OUTNAME = "1")
                    pack    RangeStr,"L",str9
                    rep     "KL",str25
        else
                pack    RangeStr,"N",str9
                rep     "MN",str25
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr),*Formula=str25
.
          if (OUTNAME = "2")
                    pack    RangeStr,"K",str9
                    rep     "OK",str25
                    setprop sheet.range(RangeStr),*Formula=str25
                    pack    RangeStr,"N",str9
                    rep     "KN",str25
                    setprop sheet.range(RangeStr),*Formula=str25
          endif

.Percentage Formula
        clear   str35
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               append  "=Round(O",str35
.       else
.               append  "=Round(L",str35
.       endif
..........................
        if (OUTNAME = "2")
                append  "=Round(O",str35
          elseif (OUTNAME = "1")
                    append  "=Round(L",str35
        else
                append  "=Round(N",str35
        endif
.END PATCH 3.75 REPLACED LOGIC
        append  str9,str35
        append  "/C",str35
        append  str9,str35
        append  ",4)",str35
        reset   str35
.
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"P",str9
.       else
.               pack    str4,"M",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"P",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"M",str9
        else
                pack    RangeStr,"O",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr),*Formula=str35
.
.Average Formula
        clear   str35
        append  "=Round(G",str35
        append  str9,str35
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               append  "/O",str35
.       else
.               append  "/L",str35
.       endif
.........................
        if (OUTNAME = "2")
                append  "/O",str35
          elseif (OUTNAME = "1")
                    append  "/L",str35
        else
                append  "/N",str35
        endif
.END PATCH 3.75 REPLACED LOGIC
        append  str9,str35
        append  "*1000,2)",str35
        reset   str35
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"Q",str9
.       else
.               pack    str4,"N",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"Q",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"N",str9
        else
                pack    RangeStr,"P",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr),*Formula=str35
.Formulas for possible Discounts
        if (OUTNAME = "2")
                add     C1,N9,N10
                move    N10,str10
                call    Trim using str10
                clear   str25
                append  "=(K",str25
                append  str9,str25
                append  "/(G",str25
                append  str9,str25
                append  "+K",str25
                append  str9,str25
                append  "+M",str25
                append  str9,str25
                append  "))",str25
                reset   str25
                pack    RangeStr,"K",str10
                setprop sheet.range(RangeStr),*Formula=str25
                clear   str25
                append  "=(M",str25
                append  str9,str25
                append  "/(G",str25
                append  str9,str25
                append  "+K",str25
                append  str9,str25
                append  "+M",str25
                append  str9,str25
                append  "))",str25
                reset   str25
                pack    RangeStr,"M",str10
                setprop sheet.range(RangeStr),*Formula=str25
.
                clear   str25
                append  "=(N",str25
                append  str9,str25
                append  "/(N",str25
                append  str9,str25
                append  "+G",str25
                append  str9,str25
                append  "))",str25
                reset   str25
                pack    RangeStr,"N",str10
                setprop sheet.range(RangeStr),*Formula=str25
.Put formulas in individual records.  This is done here as we have inserted the whole column
.after individual records are created.  Therefore we need a loop to populate these cells.
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
                        setprop sheet.range(RangeStr),*Formula=str55
.
                        clear   str25
                        append  "=Sum(K",str25
                        append  str10,str25
                        append  "+M",str25
                        append  str10,str25
                        append  ")",str25
                        reset   str25
                        pack    RangeStr,"N",str10
                        setprop sheet.range(RangeStr),*Formula=str25
.
                        add     C1,result
                        until (result > howmany)
                repeat
        endif
.
        add     C1,N9
        move    N9,str9
        call    Trim using str9
        pack    RangeStr,"G",str9
        setprop sheet.range(RangeStr),*Value=C0
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
        setprop sheet.range(RangeStr),*Formula=str25
        setprop sheet.range(RangeStr).Borders(xlEdgeTop),*LineStyle=xlContinuous
        setprop sheet.range(RangeStr).Borders(xlEdgeBottom),*LineStyle=xlDouble
.START PATCH 3.76.6 ADDED LOGIC
        setprop sheet.range(RangeStr).Font,*Bold="True"
.END PATCH 3.76.6 ADDED LOGIC
.I set the NumberFormat property here in order to keep track of them all in one place.
.Numeric Formatting
        move    "##,####0_);[Red](##,####0);_(#"-#"_)",taskname
        move    RecordTop,str10
        call    Trim using str10
        pack    RangeStr,"C",str10
        pack    RangeStr2,"C",str9
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
        pack    RangeStr,"H",str10
        pack    RangeStr2,"H",str9
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"L",str10
.               pack    str5,"L",str9
.       else
.               pack    str4,"J",str10
.               pack    str5,"J",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"L",str10
                pack    RangeStr2,"L",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"J",str10
                    pack    RangeStr2,"J",str9
        else
                pack    RangeStr,"L",str10
                pack    RangeStr2,"L",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"O",str10
.               pack    str5,"O",str9
.       else
.               pack    str4,"L",str10
.               pack    str5,"L",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"O",str10
                pack    RangeStr2,"O",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"L",str10
                    pack    RangeStr2,"L",str9
        else
                pack    RangeStr,"N",str10
                pack    RangeStr2,"N",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"V",str10
.               pack    str5,"V",str9
.       else
.               pack    str4,"S",str10
.               pack    str5,"S",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"V",str10
                pack    RangeStr2,"V",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"S",str10
                    pack    RangeStr2,"S",str9
        else
                pack    RangeStr,"U",str10
                pack    RangeStr2,"U",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.Percentage Formatting
        move    "##,####0%_);_(#"-#"_)",taskname
        pack    RangeStr,"F",str10
        pack    RangeStr2,"F",str9
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"P",str10
.               pack    str5,"P",str9
.       else
.               pack    str4,"M",str10
.               pack    str5,"M",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"P",str10
                pack    RangeStr2,"P",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"M",str10
                    pack    RangeStr2,"M",str9
        else
                pack    RangeStr,"O",str10
                pack    RangeStr2,"O",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        move    "##,####0.00%_);_(#"-#"_)",taskname
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"S",str10
.               pack    str5,"S",str9
.       else
.               pack    str4,"P",str10
.               pack    str5,"P",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"S",str10
                pack    RangeStr2,"S",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"P",str10
                    pack    RangeStr2,"P",str9
        else
                pack    RangeStr,"R",str10
                pack    RangeStr2,"R",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.Accounting Formatting
        move    "$*# ##,####0.00_);[Red]($*# ##,####0.00);$*# #"-#"_)",taskname
        pack    RangeStr,"G",str10
        pack    RangeStr2,"G",str9
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.        pack    str4,"I",str10
.        pack    str5,"I",str9
.        setprop sheet.range(str4,str5).Columns,*NumberFormat=taskname
.        if (OUTNAME = "2")
.                pack    str4,"M",str10
.                pack    str5,"M",str9
.        else
.                pack    str4,"K",str10
.                pack    str5,"K",str9
.        endif
        if (OUTNAME = "2")
                  pack    RangeStr,"I",str10
          pack    RangeStr2,"I",str9
                  setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                pack    RangeStr,"M",str10
                pack    RangeStr2,"M",str9
          elseif (OUTNAME = "1")
                  pack    RangeStr,"I",str10
          pack    RangeStr2,"I",str9
                  setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                    pack    RangeStr,"K",str10
                    pack    RangeStr2,"K",str9
        else
                  pack    RangeStr,"I",str10
          pack    RangeStr2,"I",str9
                  setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                pack    RangeStr,"K",str10
                pack    RangeStr2,"K",str9
                  setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                  setprop sheet.range(str15),*NumberFormat="##,####0.00%_);_(#"-#"_)"
                  pack    RangeStr,"J",str10
          pack    RangeStr2,"J",str9
                  setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                pack    RangeStr,"M",str10
                pack    RangeStr2,"M",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"Q",str10
.               pack    str5,"Q",str9
.       else
.               pack    str4,"N",str10
.               pack    str5,"N",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"Q",str10
                pack    RangeStr2,"Q",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"N",str10
                    pack    RangeStr2,"N",str9
        else
                pack    RangeStr,"P",str10
                pack    RangeStr2,"P",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"T",str10
.               pack    str5,"T",str9
.       else
.               pack    str4,"Q",str10
.               pack    str5,"Q",str9
.       endif
.........................
        if (OUTNAME = "2")
                pack    RangeStr,"T",str10
                pack    RangeStr2,"T",str9
          elseif (OUTNAME = "1")
                    pack    RangeStr,"Q",str10
                    pack    RangeStr2,"Q",str9
        else
                pack    RangeStr,"S",str10
                pack    RangeStr2,"S",str9
        endif
.END PATCH 3.75 REPLACED LOGIC
        setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
        if (OUTNAME = "2")
                pack    RangeStr,"J",str10
                pack    RangeStr2,"J",str9
                setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                pack    RangeStr,"K",str10
                pack    RangeStr2,"K",str9
                setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
                pack    RangeStr,"N",str10
                pack    RangeStr2,"N",str9
                setprop sheet.range(RangeStr,RangeStr2).Columns,*NumberFormat=taskname
.Do Percentages for Discount Portion
                move    "##,####0.00%_);_(#"-#"_)",taskname
                move    str9,N8
                sub     C1,N8
                move    N8,str8
                call    Trim using str8
                pack    RangeStr,"K",str8
                setprop sheet.range(RangeStr).Columns,*NumberFormat=taskname
                pack    RangeStr,"M",str8
                setprop sheet.range(RangeStr).Columns,*NumberFormat=taskname
                pack    RangeStr,"N",str8
                setprop sheet.range(RangeStr).Columns,*NumberFormat=taskname
        endif
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
.START PATCH 3.72 ADDED LOGIC
          getprop   sheet.range(RangeStr,RangeStr2).Columns,*ColumnWidth=xlColWidth
          getprop   xlColWidth,VarValue=N32
          add       "2.0",N32
          setprop   xlColWidth,VarValue=N32
          setprop   sheet.range(RangeStr,RangeStr2).Columns,*ColumnWidth=xlColWidth
.END PATCH 3.72 ADDED LOGIC
        pack    RangeStr,"A1"
        pack    RangeStr2,"BZ5000"
        setprop sheet.range(RangeStr,RangeStr2),*VerticalAlignment=xlVAlignTop

.END PATCH 3.6 REPLACED LOGIC
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"Y1"
.               pack    str5,"Y",str9
.       else
.               pack    str4,"V1"
.               pack    str5,"V",str9
.       endif
.........................
       if (OUTNAME = "2")
               pack    RangeStr,"Y1"
               pack    RangeStr2,"Y",str9
       elseif (OUTNAME = "1")
               pack    RangeStr,"V1"
               pack    RangeStr2,"V",str9
       else
               pack    RangeStr,"X1"
               pack    RangeStr2,"X",str9
       endif
.END PATCH 3.75 REPLACED LOGIC
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
.START PATCH 3.75 REPLACED LOGIC
.       if (OUTNAME = "2")
.               pack    str4,"Z1"
.               pack    str5,"Z",str9
.       else
.               pack    str4,"W1"
.               pack    str5,"W",str9
.       endif
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
.END PATCH 3.75 REPLACED LOGIC
        sheet.range(RangeStr,RangeStr2).Columns.Autofit
.Hide Column C
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
.START PATCH 3.75 ADDED LOGIC
        if (OUTNAME <> "2" & OUTNAME <> "1")
.START PATCH 3.76.1 REPLACED LOGIC
                    sub       C2,N9,N10
                    move      N10,str10
                    call      Trim using str10
                    pack      taskname,"=(K",str10,"+M",str10,")"
.
                    add     C2,N9
                    move    N9,str9
.          Move      C7,str9
                    call    Trim using str9
                    pack    RangeStr,"E",str9
.                   setprop sheet.range(str4),*Value="*Rental Price may or may not reflect actual Price on Datacard."
.                   setprop sheet.range(str4).Font,*Bold="True"
.                   add     C1,N9
.                   move    N9,str9
.                   call    Trim using str9
.                   pack    str4,"E",str9
.                   setprop sheet.range(str4),*Value="If Actual Price is not available, $75.00 Rental Price is used."
.                   setprop sheet.range(str4).Font,*Bold="True"
...............................
.begin patch 3.98
.                   setprop sheet.range(RangeStr),*Value="Savings on this campaign due to exchanges and negotiated rentals:"
                    setprop sheet.range(RangeStr),*Value="Savings on this campaign due to negotiating: exchanges, pricing, and net arrangements:"
.end patch 3.98
.START PATCH 3.76.6 REMOVED LOGIC
.                   setprop sheet.range(str4).Font,*Bold="True"
.END PATCH 3.76.6 REMOVED LOGIC
.Formula
.begin patch 3.98
.                   pack    RangeStr,"I",str9
                    pack    RangeStr,"K",str9
.end patch 3.98
                    setprop sheet.range(RangeStr),*Formula=taskname
                  setprop sheet.range(RangeStr).Borders(xlEdgeTop),*LineStyle=xlContinuous
          setprop sheet.range(RangeStr).Borders(xlEdgeBottom),*LineStyle=xlContinuous
.START PATCH 3.76.6 REMOVED LOGIC
.                   setprop sheet.range(RangeStr).Font,*Bold="True"
.END PATCH 3.76.6 REMOVED LOGIC
                    pack      RangeStr2,"I1"
                  sheet.range(RangeStr2,RangeStr).Columns.Autofit
.
.begin patch 3.98
                    pack      RangeStr2,"K1"
                  sheet.range(RangeStr2,RangeStr).Columns.Autofit

.end patch 3.98

                    add     C3,N9
                    move    N9,str9
                    call    Trim using str9
                    pack    RangeStr,"E",str9
                    setprop sheet.range(RangeStr),*Value="If list is not available on a rental basis, a $75.00 rental price is used for calculation."
                    setprop sheet.range(RangeStr).Font,*Italic="True"
                    setprop sheet.range(RangeStr).Font,*Size=8
.END PATCH 3.76.1 REPLACED LOGIC
          endif
.END PATCH 3.75 ADDED LOGIC
          Move      c1,str9
        pack    RangeStr,"A",str9
        setprop sheet.range(RangeStr).Font,*Bold="True"
        sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.begin patch 4.2
.          Move      C6,str9
.end patch 4.2
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
.>Patch 3.92    Code Modified
.        append  "\\nins1\d\users\accounti\clients\",taskname
        append  "\\nins1\d\accounting\clients\",taskname                                ."
.>Patch 3.92    Code Modified        
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
.START PATCH 3.7 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 3.7 ADDED LOGIC
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
.
.         INCLUDE   COMPUTE.inc
         include   nacdio.inc
         include   ndatio.inc
         include   nownio.inc
         include   nshpio.inc
         INCLUDE   NORD2IO.inc
         INCLUDE   NJstIO.inc
         INCLUDE   NADJIO.inc
.begin patch 3.8
               INCLUDE        ninvio.inc
               INCLUDE        NINVAcdIO.inc
               INCLUDE        compute.inc
.end patch 3.8
.patch1.5
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.5
         include   nmrgio.inc
         INCLUDE   NDAT3IO.INC
.patch1.5
.         include   nbrkio.inc
.patch1.5
         include   hpio.inc
.START PATCH 3.73 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
          include   nmodio.inc
.END PATCH 3.73 ADDED LOGIC
.START PATCH 3.75 ADDED LOGIC
          INCLUDE   NSELIO.INC
.END PATCH 3.75 ADDED LOGIC
         INCLUDE   COMLOGIC.inc

.*TDMCSPOL - CREATED AUG 5 1986.
..
.. PURPOSE - READS INPUT FILE (NINORD) AND SPOOLS
..           RECORDS FOR TRANSMISSION TO TRIPLEX IN READY TO LIST FORMAT.
..
PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
.patch2.75
                                        include   compdd.inc
                                        include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch2.75
..START PATCH 2.53 REPLACED LOGIC
..         INCLUDE   CONTACT1.inc
         INCLUDE   NCNTDD.inc
.END PATCH 2.53 REPLACED LOGIC
         INCLUDE   NORDDD.inc
         INCLUDE   NRTNDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NCRCDD.inc
         INCLUDE   NSPIDD.inc
         INCLUDE   MEDIA.inc
         INCLUDE   SHIPPING.inc
..START PATCH 2.4 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
..END PATCH 2.4 - ADDED LOGIC
..START PATCH 2.5 - ADDED LOGIC
         INCLUDE   NSPEDD.INC
..END PATCH 2.5 - ADDED LOGIC
..START PATCH 2.5 - ADDED LOGIC
         INCLUDE   HP.INC
..END PATCH 2.5 - ADDED LOGIC
..START PATCH 2.56 ADDED LOGIC
        INCLUDE WINAPI.INC
..END PATCH 2.56 ADDED LOGIC
.START PATCH 2.89 REMOVED LOGIC
.;.START PATCH 2.62 ADDED LOGIC
.         INCLUDE   NFULDD.INC
.;.END PATCH 2.62 ADDED LOGIC
.END PATCH 2.89 REMOVED LOGIC
.START PATCH 2.74A ADDED LOGIC
          INCLUDE   NSEL2DD.INC
          INCLUDE   NSEL3DD.INC
          INCLUDE   NADDDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NREFDD.INC
          INCLUDE   NMODDD.INC
.END PATCH 2.74A ADDED LOGIC
release  init      "2.991"         DLH added OMlrLstCd
Reldate   Init      "2016 March 16"
.release  init      "2.99"         DLH create output files in \\nins1\e\storage\export\009406\orders instead of \\nins1\e\data
..                                            use taskname1 instead of str55 and taskname2 instead of str45
.Reldate   Init      "2014 March 5"
.release  init      "2.98"         ASH Adjustments to allow Datacard Select items on Order to remain independent from current Datacard state
.Reldate   Init      "05 August 2013"
.release  init      "2.97"         DLH change tdmc addy
.Reldate   Init      "09 December 2010"
.release  init      "2.96"         JD    10Nov08   Create/Send temp file of order being sent
.release  init      "2.95"        DLH    18Sep07  PLI Inclusion Cleanup
.release  init      "2.94"         JD     30JUL07  Added check for null LR
.release  init      "2.93"        ASH    18JUN07  PLI Inclusion
.release  init      "2.92"        DMB    12OCT06  Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release  init      "2.91"         JD    14AUG06  Added dir path to open
.release  init      "2.90"        DMB   11AUG06  Added code to display error post 9.1b conversion
.release  init         "2.89"  DMS   22JUN06  Fulfillment Conversion
.release  init      "2.88"        ASH   09DEC05  Believe that I corrected another bug after last patch ...we will see.
.release  init      "2.87"        DMB   08DEC05  Believe that I corrected a bug after last patcy ...we will see.
.release  init      "2.86"        JD   08DEC05  Added another ship to for Donnelley 5319.
.release  init      "2.85"        JD  01DEC05  Added another ship to for Donnelley 5316.
.release  init      "2.84"       ASH 30NOV05  Patched for Donnelley/InfoUSA - MP MUST be 7 bytes in length!!
.release  init      "2.83"       ASH 10NOV05  Added new Ship-to number for Donnelley/InfoUSA
.release  init      "2.82"       ASH    31OCT2005 Temporary patch for Triplex
.release  init      "2.81"         ASH  25AUG2005 Added patch to output of Select price
.release  init      "2.80"         JD   11Jan2005 patched code that checks for Entire list.
.release  init      "2.79"        ASH   09NOV2004 FAXFILE.PRN
.release  init      "2.78"        DMB   04NOV2004 Fixed but where return to name was omitted from spooled copy
.release  init      "2.77"        DMB   06OCT2004 Fixed bug where blank pages were being created for lol records
.release  init      "2.76"        DMB   08SEP2004 Logo Conversion
.release  init      "2.75"        DMB   26MAY2004 Mailer Conversion.
.                                                 ASH Corrected logic around NINSEL3
.Release        Init            "2.75"            ASH             26APR2004  Corrected bug associated with Hotfaxing
.Release        Init            "2.74"          DMB             07APR2004  Write First/Last Lr info to file to be placed in email.
.RELEASE   INIT      "2.74A"           ASH  29JAN2004  DATACARD CONVERSION
.Release        Init           "2.73"           JD             08jul2003  write continuation lr info occode = 1
.Release        Init           "2.72"          ASH            19Mar2003  MOVED XML LOGIC INTO SELF CONTAINED ROUTINES SO THAT IT MAY BE CALLED BY PROGRAM 1
.Release        Init           "2.71"          DLH            17Mar2003  .create XML Cleanup
.Release        Init           "2.7"          DLH            28Jan2003  .create XML markup file for TDMC.
.                                                                                         .ASH Added logic to keep integrity of LINE1
.release   init        "2.64"          ASH  27JAN2003       DELETE PRIOR INSTANCES OF FAXFILE PRIOR TO PRTOPEN
.release   init        "2.63"          ASH  09JAN2003       REMOVE CERTAIN MEDIA TYPES FROM LABEL PRINT OUTS
.release   init        "2.62"          ASH  04FEB2002       NINFUL CONVERSION
.release   init        "2.61"          ASH  01FEB2002       EXTENDED EMAIL ADDRESSES
.release   init        "2.6"          ASH  27DEC2001        Changed access to TDMCORD.SAV to access via ISAM file (TDMCORDS.ISI).
.release   init        "2.59"         JD   30OCT2001 added new rtn-to# TDMC 5224.
.release   init        "2.58"        ASH  09SEP2001 ASH 1) SMALL CORRECTION FOR ENTIRE FLAG
.release   init        "2.57"        ASH  28nov2000 ASH 1) Added Retest option
.release   init        "2.56"        ASH  13JUL2000 ASH 1) Added logic to allow Triplex to print on Laser Printer
.                                                      2) Changed format of Labels
.release   init        "2.55"        ASH  01JUN2000 ASH Added Media codes
.release   init        "2.54"        ASH  01MAY2000 ASH REPLACED EMAIL LOGIC
.release   init        "2.53"        ASH  16MAR2000 ASH REPLACED CONTACT1.INC NCNTDD.INC
.release   init        "2.52"        ASH  16DEC99 ADDED CONTACTS TO CONTACT1.INC
.release   init        "2.51"        JD   08oct99 skip reuse lr's
.release   init        "2.5"        ASH  26MAY99 NINSPE.DAT conversion
.release   init        "2.4"        ASH  06MAY99 Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.release   init        "2.32"       DLH  01Apr99 corrected var SHIP to SHIPDESC
.RELEASE  INIT        "2.31"         JD  10mar99 shortend oqtyout field back for tdmc
.RELEASE  INIT        "2.3"         ASH 11JAN99 NINORD Y2K, File expansion
.release  init        "2.2"         ASH 21Sep98 NINMLR Y2K File expansion
.release  init        "2.1"         DLH 07Apr98 corr/canc change.
.RELEASE  INIT        "2.01"        jd  07oct96 contact load fixed.
.RELEASE  INIT        "2.00"       DLH 22may95 fixed load of contact name.
.RELEASE  INIT        "1.91"       DLH 22may95 fixed load of contact name.
.RELEASE  INIT        "1.9"       jd12may95 modified load of contact name.
.RELEASE  INIT        "1.8 "      JD  11feb95   added net name print.
.RELEASE  INIT        "1.7 "      JD  11MAR94   add first,last lr # to spool.
.RELEASE  INIT        "1.6"      JD  11APR93   EXPANDED MEDIA.inc.
.RELEASE   INIT      "1.5"      23NOV92 JD WRITING OUT INFO TO TDMCINFO FILE
.                              FOR OUTSIDE TDMC MERGE ORDERS.
.RELEASE   INIT     "1.4"      17OCT92 JD HARDWIRE NOTE ON NON LSTMGT MERGE ORD
.RELEASE  INIT      "1.3"         JD 11NOV92 PRINT MERGE/PURGE #, AND PPM
.                                 INFO FILE, EXCLUDE ON LST MGMT ORDERS.
.RELEASE  INIT      "1.2"         DLH 08APR92      LIFESTYLE & IC SYSTEMS
.
.RELEASE  INIT     "1.1"         DLH 17MAR92       NRTNXX, REMOVED UNNEC.
.                                                  OFFER INCLUDES, NSPIXX
.RELEASE  INIT      "1.0"
.
. OTHER  VARIABLES.
. ....................
.PATCH 2.74
EMAILFILE FILE
.ENDPATCH2.74
.begin patch 2.99
Taskname1  DIM 150
Taskname2  DIM 150
.end patch 2.99

.Patch 2.76 Logic Added
Laser pfile
LabelListView       listview
#result   form      9
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
sevenfive form      "7.5"
WHITE     COLOR
BLACK     COLOR
num9      FORM      9
.hexeight integer   4,"4294967295"
.Patch 2.76 Logic Added
.START PATCH 2.72 ADDED LOGIC
DimPtr    DIM       ^
.END PATCH 2.72 ADDED LOGIC
SPCL     DIM       2               *SPECIAL INSTRUCTION KEY
.START PATCH 2.5 - VARS NOW OBSOLETE
.DESC0L1  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC0L2  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC991  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC992  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC981  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC982  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.END PATCH 2.5 - VARS NOW OBSOLETE
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
.
REVTXT   INIT      "Revised: "
CANTXT   INIT      "**CANCELLED** : "
BILDTXT  INIT      "**Billed Order**"
REVDATA  DIM       30
BILDDATA DIM       16
.
. FILES.
. ......
TDMCORD   FILE               .TRIPLEX ORDER INPUT FILE.
.Start Patch #2.3 - increased file size
.Added 2 bytes to file
.LABEL    FILE      VAR=115,UNCOMP  *SPOOL FILE FOR LABELS.
.START PATCH 2.56 REPLACED LOGIC
.LABEL    FILE      VAR=137,UNCOMP  *SPOOL FILE FOR LABELS.
LABEL   PFILE
.END PATCH 2.56 REPLACED LOGIC
.End Patch #2.3 - increased file size
.START PATCH 2.5 - NOW READING FROM IO
.OUTSP    IFILE     KEYLEN=6,VAR=288
.END PATCH 2.5 - NOW READING FROM IO
BADORD   FILE
.Start Patch #2.2 - remmed and replaced, increased file size to reflect NINMLR expansion
.NAMFILE  FILE      FIX=163
.WEEKFILE FILE      FIX=122
.Start Patch #2.3 - increased file size
.NAMFILE  FILE      FIX=183
.WEEKFILE FILE      FIX=142
.START PATCH 2.74A REPLACED LOGIC
.NAMFILE  FILE      FIX=185      .Added 2 bytes to file
NAMFILE  FILE      FIX=225      .Added 2 bytes to file
.END PATCH 2.74A REPLACED LOGIC
WEEKFILE FILE      FIX=146      .Added 4 bytes to file
.Start Patch #2.3 - increased file size
.End Patch #2.2 - remmed and replaced, increased file size to reflect NINMLR expansion
.START PATCH 2.6 REPLACED LOGIC
.SAVEFILE FILE
SAVEFILE IFILE
.END PATCH 2.6 REPLACED LOGIC
.Start PATCH 2.96
SENDFILE FILE
.END PATCH 2.96 
.
tdmcstat dim       1
THREE    FORM      "3"
FOUR     FORM      "4"
FIVE     FORM      "5"
SIX      FORM      "6"
SEVEN    FORM      "7"
EIGHT    FORM      "8"
ANS      DIM       1
FILE     FORM      2         BRANCHING CONSTANT FOR I/O TRAPS
DATE     DIM       8         'MM/DD/YY'.
DAY      DIM       2
OFFEROUT DIM       11       FOR OUTPUT OF OFFER, SUPPRESSED IF NO OFFER SELCTD.
COUNT    FORM      5
SPCOUNT  FORM      5
ORIG099  DIM       1
ANY099   DIM       1
WORK06   DIM       6
WORK47   DIM       47
WK247    DIM       47
.begin patch 2.7
Nfield52 form      5.2
.begin patch 2.71
InstructionCounter            form           2
str100         Dim            100
.end patch 2.71
.end patch 2.7
NFIELD23 FORM      3.2                  (NUMERIC WORK FIELD)
NFIELD4  FORM      4
MLRKEY   DIM       7
.STATUS   DIM       15
.START PATCH 2.5 - INCREASED VAR
.V1       FORM      1
V1       FORM      2
.END PATCH 2.5 - INCREASED VAR
MAIL1    DIM       25                       INFOR-
MAIL2    DIM       25                       MATION
MAIL3    DIM       25                       READ
.MAILCC   DIM       17                       MAILER
EXCHANGE DIM       15         *USED FOR ORDER PRINT
TEST     DIM       15         *USED FOR ORDER PRINT
F3       DIM       3         *USED FOR ORDER PRINT
F2       DIM       2         *USED FOR ORDER PRINT
ENTIRE   DIM       1        *USED FOR ORDER PRINT
SAMPLE   DIM       26       *USED FOR ORDER PRINT
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
CORTN    DIM       3         *USED FOR ORDER PRINT
CONT     DIM       23        *USED FOR ORDER PRINT
CONT1    DIM       20        *USED FOR ORDER PRINT
.Start Patch #2.3 - increase vars
.CONTDTE  DIM       8         *USED FOR ORDER PRINT
.CONTQTY  DIM       9         *USED FOR ORDER PRINT
.QTYMSK   INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
.QTYOUT   DIM       9         *USED FOR ORDER PRINT
.QTYNUM   FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
.
CONTDTE  DIM       10        *USED FOR ORDER PRINT
CONTQTY  DIM       11        *USED FOR ORDER PRINT
QTYMSK   INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYMSK2  INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT   DIM       11        *USED FOR ORDER PRINT
QTYNUM   FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
QTYNUM2  FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
.end Patch #2.3 - increase vars
qtyout2  dim       9         * just for tdmc lolfile
MEDMEMO  DIM       25        *USED FOR ORDER PRINT, ON MAG TAPE.
COMSLCT  DIM       25        *USED FOR ORDER PRINT, COMSELECT ORDERS.
REPRT    DIM       15        *USED FOR ORDER PRINT, REPRINTED ORDERS.
.                            *AND CANCELLED ORDERS, REPRINT IMPLIED.
PRTPO    DIM       7         *USED FOR ORDER LABEL PRINT
LBOFR    DIM       25        *USED FOR ORDER LABEL PRINT.
LROUT    DIM       6         *USED FOR ORDER & LABEL PRINT.
LRMASK   INIT      "ZZZZZ9"
LRNUM    FORM      6
COUNTR   FORM      4
BUFFER   FORM      "4"
RTNSTRNG DIM       6
RTNCHEK  FORM      4
PRICECK  DIM       5
DESC     DIM       12
DESC2     DIM      3
TDMCSW   INIT      "N"
MEDTYPE  DIM        1
save     dim        47
prtlines form       2
c33      form       "33"
c66      form       "66"
.Patch2.74
firstlr   dim                           6
.Patch2.74
lastlr   dim        6
lastlabl dim        6
salenumb dim        2
fullCNT      DIM       34
cnt          dim       20
.START PATCH 2.53 REMOVED LOGIC
.cntphone     dim       14
.END PATCH 2.53 REMOVED LOGIC
.START PATCH 2.61 REPLACED LOGIC
.intrnet  dim        24                .print contact's internet address
.START PATCH 2.93 REPLACED LOGIC
.intrnet  dim        46                .print contact's internet address
intrnet  dim        50                .print contact's internet address
.END PATCH 2.93 REPLACED LOGIC
.END PATCH 2.61 REPLACED LOGIC
BEGIN    FORM      2
LAST     FORM      2
loltype  dim       1
loldes   form      1
lolcodes init       "DXLRQ"
.START PATCH 2.56 REPLACED LOGIC
.dim25b   dim       25
dim45b   dim       45
.END PATCH 2.56 REPLACED LOGIC
compm    dim       25
.START PATCH 2.5 - ADDED VARS
holdstr  dim      758
.START PATCH 2.7 ADDED LOGIC
line     dim      55
.END PATCH 2.7 ADDED LOGIC
line1    dim      55
line2    dim      55
line3    dim      55
line4    dim      55
line5    dim      55
line6    dim      55
line7    dim      55
line8    dim      55
line9    dim      55
line10   dim      55
line11   dim      55
line12   dim      55
line13   dim      55
line14   dim      55
line15   dim      55
CARR     INIT     0x7f
carrfill dim      2
.END PATCH 2.5 - ADDED VARS
.START PATCH 2.56 - ADDED VARS
font2   font
columnA form    9
columnB form    9
columnC form    9
row1    form    9
row2    form    9
.END PATCH 2.56 - ADDED VARS
.begin patch 2.7
XMLFile        File
.begin patch 2.99
.XMLFileName    Dim            45
XMLFileName    Dim            150
.end patch 2.99
.end patch 2.7
.START PATCH 2.74A - ADDED LOGIC
PackData DataList
.END PATCH 2.74A - ADDED LOGIC
.START PATCH 2.89 ADDED LOGIC
NFULCOMP  DIM       55
.END PATCH 2.89 ADDED LOGIC
* PROGRAM MAIN.
* *************
.
. OPEN FILES.
. ...........
         MOVE      "NORD0024" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "TRIPLEX SPOOL PROGRAM" TO STITLE
         CALL       PAINT
.START PATCH 2.56 ADDED LOGIC
         create    font2,"Arial",size=9
         MOVE      C0,HowMany
.         MOVE      "500",column1
.         MOVE      "1700",column2
.         MOVE      "1300",column3
.         MOVE      "4200",column4
.         MOVE      "5400",column5
.         MOVE      "5000",column6
.
         MOVE      "750",column1
         MOVE      "1950",column2
         MOVE      "1550",column3
         MOVE      "4500",column4
         MOVE      "5700",column5
         MOVE      "5300",column6
         MOVE      "650",row1
         MOVE      "650",row2
.END PATCH 2.56 ADDED LOGIC
.Patch 2.76 Logic Added
          CREATE    White=*WHITE
          CREATE    BLACK=*BLACK
          CREATE    labellistview=1:50:1:50,SORTHEADER=1,SORTORDER=1
          labellistview.INSERTCOLUMN  USING "",30,0
          labellistview.INSERTCOLUMN  USING "",30,1
          labellistview.INSERTCOLUMN  USING "",100,2
          labellistview.INSERTCOLUMN  USING "",30,3
          labellistview.INSERTCOLUMN  USING "",30,4
          labellistview.INSERTCOLUMN  USING "",100,5
          labellistview.INSERTCOLUMN  USING "",30,6
          labellistview.INSERTCOLUMN  USING "",30,7
          labellistview.INSERTCOLUMN  USING "",100,8
          labellistview.INSERTCOLUMN  USING "",30,9
          labellistview.INSERTCOLUMN  USING "",30,10
          labellistview.INSERTCOLUMN  USING "",100,11
          labellistview.INSERTCOLUMN  USING "",30,12
          labellistview.INSERTCOLUMN  USING "",30,13
          labellistview.INSERTCOLUMN  USING "",100,14
          labellistview.INSERTCOLUMN  USING "",30,15
          labellistview.INSERTCOLUMN  USING "",30,16
          labellistview.INSERTCOLUMN  USING "",100,17
          labellistview.INSERTCOLUMN  USING "",30,18
          labellistview.INSERTCOLUMN  USING "",30,19
          ACTIVATE labellistview
          setprop   LabelListView,visible=c0
          create    font1,"Times New Roman",size=14,bold
          create    fontO8,"Times New Roman",size=8
          create    font5,"Times New Roman",size=11
          create    fontO9,"Times New Roman",size=9
          create    fontO9I,"Times New Roman",size=9,Italic
          create    fontO10,"Times New Roman",size=10
          create    fontO10n,"Courier New",size=11
          create    fontO10B,"Times New Roman",size=10,Bold
          create    fontO12B,"Times New Roman",size=12,Bold
          create    fontO14,"Times New Roman",size=14
          create    fontO14B,"Times New Roman",size=14,Bold
          create    fontO14BI,"Times New Roman",size=14,Bold,Italic
          create    fontO18I,"Times New Roman",size=18,Italic
          create    fontO7,"Times New Roman",size=7
          create    fontO7dot5,"Times New Roman",size=sevenfive
          create    fontO7dot5I,"Times New Roman",size=sevenfive,Italic
          create    fontO7dot5b,"Times New Roman",size=sevenfive,Bold
          create    fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
          create    fontO18B,"Times New Roman",size=18,Bold
          create    fontO18BI,"Times New Roman",size=18,Bold,Italic
.
          create    PRTpg24B,"Times New Roman",size=24,Bold
          create    PRTpg24I,"Times New Roman",size=24,Italic
          create    PRTpg10,"Times New Roman",size=10
font7     font
font8     font
font9     font
NINLogo   PICT
          move      "750",column
          move      "1750",column1
          move      "3000",column2
          create    font7,"Helvetica",size=14,bold
          create    font8,"Helvetica",size=14,italic
          create    font9,"Arial",size=12
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.Patch 2.76 Logic Added

.START PATCH 2.64 ADDED LOGIC
          call      GetWinVer
.               Goto testXml
..END PATCH 2.64 ADDED LOGIC
.PATCH 2.74
     Prepare        EmailFile,"\\nins1\e\data\EMAIL.DAT"
.ENDPATCH2.74
.         TRAP      IO IF IO
.//Patch 2.9 altered code to retrieve the error message
.         TRAP      IO IF IO 
         TRAP      IO GIVING ERROR IF IO 
.//Patch 2.9 altered code to retrieve the error message end
         DISPLAY   *P1:24,"OPENING FILES"
         MOVE      C1 TO FILE
         IFNZ      PC
         OPEN      TDMCORD,"TDMCORD/OUT",EXCLUSIVE
         ADD       C1 TO FILE
.START PATCH 2.56 REPLACED LOGIC
.         PREPARE   LABEL,"TRIPLEX1/OUT:PRINT",PREPARE
.START PATCH 2.64 ADDED LOGIC
          call      CleanFaxfile
.END PATCH 2.64 ADDED LOGIC
.Patch 2.76 Comment Out
.         PRTOPEN   LABEL,"FAXFILE","FAXFILE.PRN"
.         prtpage   LABEL;*UNITS=*HIENGLISH;
.Patch 2.76 Comment Oout
.END PATCH 2.56 REPLACED LOGIC
         ADD       C1 TO FILE
         OPEN      BADORD,"NONTRIPLEX/ORDS:PRINT"
         PREP      NAMFILE,"TDMCINFO:PRINT"
         OPEN      WEEKFILE,"WEEKTDMC/INFO:PRINT"
.START PATCH 2.6 REPLACED LOGIC
.         OPEN      SAVEFILE,"TDMCORD/SAV:TEXT"
         OPEN      SAVEFILE,"TDMCORDS|10.10.30.103:502"
.END PATCH 2.6 REPLACED LOGIC
         XIF
         IFZ      PC
.Patch 2.91 
.        OPEN      TDMCORD,"\\nins1\e\data\text\TDMCORD.OUT",EXCLUSIVE
         OPEN      TDMCORD,"TDMCORD.OUT|10.10.30.103:502",EXCLUSIVE
.Patch 2.91
.         OPEN      TDMCORD,"TDMCORD.OUT"
         ADD       C1 TO FILE
.START PATCH 2.74A ADDED LOGIC
.Create work var
          create    PackData=1:1:1:1
.END PATCH 2.74A ADDED LOGIC

.begin patch 2.7
Testxml
.START PATCH 2.72 REPLACED LOGIC
.               clock          timestamp to timestamp
.               Clear          Str45
.               unpack         timestamp into str8,str6
.               pack           str45 from "\\nins1\e\data\NIN.",str8,".t",str6,".PreTouch"
.;               Prepare        XMLFile,"\\nins1\e\data\TDMC.XML"
.               Prepare        XMLFile,str45
.               write          Xmlfile,seq;"<nin>"
.               weof           xmlfile,seq
.               close          xmlfile
.               Clear          Str45
.               pack           str45 from "\\nins1\e\data\NIN.",str8,".t",str6,".xml"
.;               pack           str45 from "\\nins1\e\data\NIN",timestamp,".Xml"
.               clear          XmLFIleName
.               pack           XmlFileName from "\\nins1\e\data\NIN.",str8,".t",str6,"."
.               Prepare        XMLFile,str45
.               write          Xmlfile,seq;"<nin>"
.Patch 2.76 For Testing
          call      CreateXMLFile
.Patch 2.76 For Testing
.END PATCH 2.72 REPLACED LOGIC
.end patch 2.7
.START PATCH 2.56 REPLACED LOGIC
.         PREPARE   LABEL,"g:\DATA\TRIPLEX1.OUT",PREPARE
.START PATCH 2.64 ADDED LOGIC
          call      CleanFaxfile
.END PATCH 2.64 ADDED LOGIC
.Patch 2.76 Comment Out
.         PRTOPEN   LABEL,"FAXFILE","FAXFILE.PRN"
.         prtpage   LABEL;*UNITS=*HIENGLISH;
.Patch 2.76 Comment Out
.END PATCH 2.56 REPLACED LOGIC
         ADD       C1 TO FILE
         display   *p1:24,"opening nontdmc",*w2

.         OPEN      BADORD,"\\nins1\e\DATA\NONTDMC"
         OPEN      BADORD,"NONTDMC|10.10.30.103:502"
         display   *p1:24,"opening tdmcinfo",*w2
         PREP      NAMFILE,"\\nins1\e\data\TDMCINFO"
         display   *p1:24, "opening weektdmc",*w2
.         OPEN      WEEKFILE,"\\nins1\e\DATA\WEEKTDMC"
         OPEN      WEEKFILE,"WEEKTDMC|10.10.30.103:502"
.START PATCH 2.6 REPLACED LOGIC
.         display   *p1:24, "opening tdmcord.sav",*w2
.         OPEN      SAVEFILE,"\\nins1\e\data\TEXT\TDMCORD.SAV"
         display   *p1:24, "opening tdmcords.isi",*w2
.         OPEN      SAVEFILE,"\\nins1\e\data\index\TDMCORDS"
         OPEN      SAVEFILE,"TDMCORDS|10.10.30.103:502"
.END PATCH 2.96 
         display   *p1:24, "opening tdmcord.snd",*w2
         prep      SENDFILE,"\\nins1\e\data\tdmcord.snd"
         XIF
CLOCK    CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
.START PATCH 2.6 REPLACED LOGIC
.         WRITE     SAVEFILE,SEQEOF;DATE,"***********************************"
          move      DATE,str6
.Patch 2.76 
         WRITE     SAVEFILE,str6;DATE,"***********************************"
.Patch 2.76
.END PATCH 2.6 REPLACED LOGIC
.Patch 2.96 For Testing
         WRITE     SENDFILE,seq;str6,DATE,"***********************************"
.Patch 2.96 For Testing
         ADD       C1 TO FILE
.Comment Out Patch 2.76
.         IFNZ      PC
.         SPLOPEN   "TRIPLEX/OUT:PRINT",,BUFFER
.         XIF
.         IFZ       PC
.Comment Out Patch 2.76
         display   *p1:24, "opening triplex.out",*w2
.Logic Added Patch 2.76
                              PRTOPEN Laser,"faxfile",""
.Logic Added Patch 2.76

.Comment Out Patch 2.76
.         SPLOPEN   "\\nins1\e\DATA\TRIPLEX.OUT"
.         XIF
.Comment Out Patch 2.76
         ADD       C1 TO FILE
         ADD       C1 TO FILE
.START PATCH 2.5 - NOW READING FROM IO
.         display   *p1:24, "opening ninspec",*w2
.         OPEN      OUTSP,"NINSPEC",SHARE
.END PATCH 2.5 - NOW READING FROM IO
         SUB       FILE FROM FILE
. ....................................................
. DISS - DISPLAY PROGRAM NAME.
. ............................
DISS     MOVE      "NORD0024" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "TRIPLEX SPOOL PROGRAM" TO STITLE
         CALL       PAINT
         MOVE       "ABORT" TO PF5
         CALL       FUNCDISP
         TRAP       ABORT IF F5
.
+ *****************************************************************************
. ORD - ORDER SECTION.
. ....................
ORD
         MOVE      C0 TO COUNT
         TRAPCLR   IO
         WRITE     NAMFILE,SEQ;B1,DATE," -MERGING AT TRIPLEX !!!"
.START PATCH 2.74A REPLACED LOGIC
.         WRITE     NAMFILE,SEQ;B1,"LR#      MAILER                     LIST NAME":
.                               "                              ":
.                   "                                    QUANTITY":
.                               "     M/P ##            PRICE/M"
.         WRITE     NAMFILE,SEQ;B1,"---     ------                     ---------":
.                               "                              ":
.                    "                                    --------":
.                               "     -------          --------"
.         WRITE     NAMFILE,SEQ;STAR,DATE," TRIPLEX PRODUCTION ORDERS !!!"
.         WRITE     NAMFILE,SEQ;STAR,"LR#      MAILER                     LIST NAME":
.                               "                              ":
.                   "                                    QUANTITY":
.                               "                       PRICE/M"
.         WRITE     NAMFILE,SEQ;STAR,"---     ------                     ---------":
.                               "                              ":
.                    "                                    --------":
.                               "                      --------"
.......................
         WRITE     NAMFILE,SEQ;B1,"LR#      MAILER                     LIST NAME":
                               "                              ":
                       "                                        ":
                   "                                    QUANTITY":
                               "     M/P ##            PRICE/M"
         WRITE     NAMFILE,SEQ;B1,"---     ------                     ---------":
                               "                              ":
                       "                                        ":
                    "                                    --------":
                               "     -------          --------"
         WRITE     NAMFILE,SEQ;STAR,DATE," TRIPLEX PRODUCTION ORDERS !!!"
         WRITE     NAMFILE,SEQ;STAR,"LR#      MAILER                     LIST NAME":
                               "                              ":
                       "                                        ":
                   "                                    QUANTITY":
                               "                       PRICE/M"
         WRITE     NAMFILE,SEQ;STAR,"---     ------                     ---------":
                               "                              ":
                       "                                        ":
                    "                                    --------":
                               "                      --------"
.END PATCH 2.74A REPLACED LOGIC
.
READO    MOVE      SEVEN TO FILE
         CLEAR     tdmcstat
         filepi    1;tdmcord
         READ      TDMCORD,SEQ;OLRN,tdmcstat
         GOTO      EOJ IF OVER
.Start Patch 2.94 added
.         cmatch    b1 to olrn
.                             goto      reado if equal
         type      olrn
         goto      reado if not equal
.End Patch 2.94 
          ADD       C1 TO COUNTR
.Patch 2.76 Logic Added
.Patch 2.77  Logic Replaced - don't create new page unless a live order
                              if (countr > 1 & tdmcstat =  "0")
.                             if (countr > 1)
.Patch 2.77
                   prtpage LASER;*NEWPAGE:
                                        *UNITS=*HIENGLISH:
                        *ORIENT=*PORTRAIT;
                              endif
.Patch 2.76 Logic Added
         move      tdmcstat to loltype
         rep       "Q1L2X3R4D5" in loltype
         DISPLAY   *P10:12,*EL,"COUNT READ ",COUNTR,b1,olrn
         MOVE      OLRN TO NORDFLD
         MOVE      C1 TO NORDPATH
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         goto      noord if over
.START PATCH 2.84 ADDED LOGIC - SPECIAL FOR INFOUSA
          call      Trim using OMLRKY
          if (OMLRKY <> "")
                    count     result,OMLRKY
                    if (result < 7)
                              type      OMLRKY
                              if equal
                                        move      OMLRKY,str7
                                        call      ZFillIt using str7
                                        move      str7,OMLRKY
                              endif
                    endif
          endif
.END PATCH 2.84 ADDED LOGIC - SPECIAL FOR INFOUSA
.START PATCH 2.4 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.4 - NEW LOGIC
.START PATCH 2.74A ADDED LOGIC
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
                    endif
          endif
.END PATCH 2.74A ADDED LOGIC
         pack      salenumb from osales10,osales
         CMATCH    " " TO OLRN
         GOTO      READO IF EOS
         GOTO      READO IF EQUAL
         MOVE      C0 TO QTYNUM
         MOVE      C0 TO QTYNUM2
         MOVE      OQTY TO QTYNUM
         MOVE      OQTY TO QTYNUM2
         COMPARE   C0 TO QTYNUM
         GOTO      READO IF EQUAL
.START PATCH 2.51 - ADDED LOGIC
         match     "0001",ortnnum
         goto      reado if equal
.end PATCH 2.51 - ADDED LOGIC
.         MATCH     "D" TO tdmcstat
        reset     lolcodes
        scan      tdmcstat in lolcodes
.Patch 2.87 COMMENT OUT
..        IF      EQUAL
..                  MATCH     "0040" TO ORTNNUM
..                  IF        NOT EQUAL
.         GOTO      READO
..                            MATCH     "5224" TO ORTNNUM
..                            IF        NOT EQUAL
.START PATCH 2.83 REPLACED LOGIC
.           GOTO      READO
..                                      MATCH     "5318" TO ORTNNUM
..                                      IF NOT EQUAL
.                             GOTO READO
..                                      ENDIF
.END PATCH 2.83 REPLACED LOGIC
.START PATCH 2.85
..                                      MATCH     "5316" TO ORTNNUM
..                                      IF NOT EQUAL
.                             GOTO READO
..                                      ENDIF
.END PATCH 2.85 REPLACED LOGIC
.START PATCH 2.86
..                                      MATCH     "5319" TO ORTNNUM
..                                      IF NOT EQUAL
..                                                GOTO READO
..                                      ENDIF
.END PATCH 2.85 REPLACED LOGIC
..                            ENDIF
.                   ENDIF
.Patch 2.87 COMMENT OUT
.Patch 2.87
          IF      EQUAL
                    IF ((ORTNNUM <> "0040")&(ORTNNUM <> "5224")&(ORTNNUM <> "5318")&(ORTNNUM <> "5316")&(ORTNNUM <> "5319"))
                              GOTO READO
                    ENDIF

.Patch 2.87
          move      yes to tdmcsw
          PACK      MKEY FROM OMLRNUM,OCOBN
          CALL      NMLRKEY
          PACK      QTYOUT FROM QTYMSK
          EDIT      QTYNUM TO QTYOUT
          PACK      QTYOUT2 FROM QTYMSK2
          EDIT      QTYNUM2 TO QTYOUT2
.START PATCH 2.74A REPLACED LOGIC
.         UNPACK    OPPM INTO F3,F2
.END PATCH 2.74A REPLACED LOGIC
          GOTO      LAYBIL
        ENDIF
         SUB       FILE FROM FILE
         ADD       C1 TO COUNT
         MOVE      "N" TO TDMCSW
.START PATCH 2.5 - NOW READING FROM IO
.         CLEAR     DESC0L1
.         CLEAR     DESC0L2
.         CLEAR     DESC991
.         CLEAR     DESC992
.         CLEAR     DESC981
.         CLEAR     DESC982
.         filepi    1;outsp
.         READ      OUTSP,OLRN;WORK06,DESC0L1,DESC0L2,DESC991,DESC992:
.                   DESC981,DESC982
         move      OLRN,NSPEFLD
         move      "READO-NSPEKEY",Location
         call      NSPEKEY
.END PATCH 2.5 - NOW READING FROM IO
.
. PRINT VARIABLES FROM ACCESSED RECORD
.
.
         MOVE      " " TO EXCHANGE
         MOVE      " " TO ENTIRE
         MOVE      " " TO TEST
         MOVE      " " TO CONT
         MOVE      " " TO CONT1
         MOVE      " " TO REPRT
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         CALL      NOMLR IF OVER
         MOVE      OLON TO NOWNFLD
         MOVE      "10" TO FILE
         REP       " 0" IN NOWNFLD
         CALL      NOWNKEY
.START PATCH 2.62 REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.         GOTO      NOTDMC IF NOT EQUAL
.Start Patch 2.92 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                    
.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
.End Patch 2.92 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                            
.START PATCH 2.89 REPLACED LOGIC
.         if (OWNCTN <> "")
.                   pack      NFULFLD,OWNctn
.                   rep       zfill,NFULFLD
.                   move      C1,NFULPATH
.                   move      "READO-NFULKEY",Location
.                   pack      KeyLocation,NFULFLD
.                   call      NFULKEY
.         else
.                   clear     NFULFLD
.                   clear     NFULCOMP
.         endif
.         if (NFULFLD <> "0026")
.                   goto NOTDMC
.         else
.                   scan      "TDMC",NFULCOMP
.                   if  not equal
.                             reset     NFULCOMP
.                             goto NOTDMC
.                   endif
.                   reset     NFULCOMP
.         endif
.Start Patch 2.92 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                               
.         if (OWNCTN <> "")
.                   pack      COMPFLD6,OWNctn
.                   rep       zfill,COMPFLD6
.                   move      C1,COMPPATH
.                   move      "READO-COMPKEY6",Location
.                   pack      KeyLocation,COMPFLD6
.                   call      COMPKEY6
.                   if over
.                             clear     COMPFLD6
.                             clear     NFULCOMP
.                   else
.                             if (COMPSVBFLG <> "T")
.                                       clear     COMPFLD6
.                                       clear     NFULCOMP
.                             else
.                                       move      COMPCOMP,NFULCOMP
.                             endif
.                   endif
.         else      // OWNCTN = ""
.                   clear     COMPFLD6
.                   clear     NFULCOMP
.         endif
.         if (COMPFLD6 <> "0026")
.                   goto NOTDMC
.         else
.                   scan      "TDMC",NFULCOMP
.                   if  not equal
.                             reset     NFULCOMP
.                             goto NOTDMC
.                   endif
.                   reset     NFULCOMP
.         endif
.End Patch 2.92 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                       
.Start Patch 2.92 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                       
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "READO-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              move      COMPCOMP,NFULCOMP
                    endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     NFULCOMP
          endif
          if (COMPFLD <> "009406")
                    goto NOTDMC
          endif
.End Patch 2.92 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                         
.END PATCH 2.89 REPLACED LOGIC
.END PATCH 2.62 REPLACED LOGIC
.89AUG28 - IF RNT=0, THEN REUSE OF LR AND NOT TC'D.
         CLEAR     REVDATA
         MOVE      OLRN TO NCRCFLD
         CALL      NCRCKEY
         IF        NOT OVER
NCRCLOOP CMATCH    "C" TO NCRCCODE
         IF        EQUAL
         PACK      REVDATA FROM CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ELSE
         PACK      REVDATA FROM REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ENDIF
         CALL      NCRCKS
         GOTO      NCRCEXIT IF OVER
         MATCH     NCRCFLD TO NCRCKEY
         GOTO      NCRCEXIT IF NOT EQUAL
         GOTO      NCRCLOOP
         ENDIF
NCRCEXIT MOVE      "0" TO NFIELD4
         MOVE      ORTNNUM TO NFIELD4
         BRANCH    NFIELD4 OF NOTDMC
         ADD       C1 TO SPCOUNT
        DISPLAY   *P14:20,*EL,"TRIPLEX ORDERS SPOOLED : ",SPCOUNT
         compare   c1 to spcount
         if        equal
.Patch2.74
                                        move olrn to firstlr

.         call      firstord
.Patch2.74
         endif
         move      c0 to prtlines
         move      olrn to lastlr
.START PATCH 2.74A REPLACED LOGIC
.         unpack   oppm into f3,f2
.END PATCH 2.74A REPLACED LOGIC
         CMATCH    " " TO OFOCODE
         GOTO      MEDIAEX IF EQUAL          *NO MEDIA SELECT
         GOTO      MEDIAEX IF EOS            * NO MEDIA SELECT
         MOVE      C0 TO NFIELD23
         TYPE      OFOCODE
         MOVE      OFOCODE TO SAVE         *SAVE VARIABLE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       C10 TO NFIELD23
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       "20" TO NFIELD23
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO NFIELD23
         ADD       "30" TO NFIELD23
DIS27    MOVE      MED0 TO MEDIA
.START PATCH 2.55 REPLACED LOGIC
.         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
.                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
.                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
.                   MED23,MED24,MED25
.         CLEAR     MEDTYPE
.         MOVE      YES TO MEDTYPE
.         LOAD      MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
.                   YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES
         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
         CLEAR     MEDTYPE
         MOVE      YES TO MEDTYPE
         LOAD      MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
                   YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES,NO,NO,NO,NO,NO
.END PATCH 2.55 REPLACED LOGIC
MEDIAEX  CLEAR     SAMPLE
         CALL      SAMPLE
         CLEAR     RTCNTCT
         CLEAR     COMSLCT
         CLEAR     RTCOMP
.START PATCH 2.56 REPLACED LOGIC
.         clear     str25
.         clear     dim25b
         clear     str45
         clear     dim45b
.END PATCH 2.56 REPLACED LOGIC
         CLEAR     RTCITY
         CLEAR     RTSTATE
         CLEAR     RTZIP
         CLEAR     CORTN
         CLEAR     CONTDTE
         CLEAR     CONT
         CLEAR     CONTQTY
.         CLEAR     MEDMEMO
         MATCH     "C" TO OCOMSLCT            *COMSELECT ORDER?
         CALL      COMSLCT IF EQUAL           *YES
         MATCH     "L" TO OCOMSLCT            *LIFESTYLE OVERLAY?
         CALL      LIFESTYL IF EQUAL          *YES
         MATCH     "I" TO OCOMSLCT            *IC SYSTEMS OVERLAY?
         CALL      ICSYSTEM IF EQUAL          *YES
         PACK      QTYOUT FROM QTYMSK
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         move      oqty to qtynum2
         EDIT      QTYNUM2 TO QTYOUT2
         MATCH     "1" TO OCCODE
         CALL      CONTIN IF EQUAL            *CONTINUATION ORDER.
         MATCH     "2" TO OCCODE
         CALL      CONTIN1 IF EQUAL           *CONTINUATION ORDER/NO OMIT.
         BUMP      OODNUM BY 4
         MOVE      OODNUM TO OFFEROUT
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         MATCH     "2531" TO ORTNNUM
         IF        NOT EQUAL
         MATCH     RTCOMP TO MCOMP
         CALL      CHNGRET IF NOT EQUAL
         ENDIF
.
.ON THESE MAILERS THE OFFER DESC. MUST BE USED ON THE RETURN-TO CONTACT
.LINE    677-CMS, 210-COPLON, 53-ANACAPA, 702-MAZEL, 965-ORAM.
.
         MATCH     "0677" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0210" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0053" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0702" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0965" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         CMATCH    "R" TO ANS               *REPRINT ?
         CALL      REPRT IF EQUAL             *YES.
         CMATCH    "X" TO OSTAT               *CANCELLED ORDER?
         CALL      CANCLLED IF EQUAL
TEST     MOVE      "0" TO NFIELD23           *CLEAR FIELD
         MOVE      OTOCODE TO NFIELD23
.START PATCH 2.57 - REPLACED LOGIC
.         BRANCH    NFIELD23 TO TESTYES
         BRANCH    NFIELD23 TO TESTYES,TESTYES
.END PATCH 2.57 - REPLACED LOGIC
         GOTO      EXCHANGE
TESTYES  MOVE      "X" TO TEST
EXCHANGE
.START PATCH 2.58 - ADDED LOGIC
           MOVE        C0,NFIELD23
.END PATCH 2.58 - ADDED LOGIC
           MOVE      OELCODE TO NFIELD23
         BRANCH    NFIELD23 TO ENTRENT,EXCHANG1,ENTIRE
         GOTO      OPRINT1            *NO CODE!!!!
SAMPLE   move     C0,nfield23
         MOVE      OSCODE TO NFIELD23
         BRANCH    NFIELD23 OF SAM1,SAM2,SAM3
         MOVE      "  " TO SAMPLE
         RETURN
SAM1     MOVE      "SAMPLE ENCLOSED" TO SAMPLE
         RETURN
SAM2     MOVE      "SAMPLE TO FOLLOW" TO SAMPLE
         RETURN
SAM3     MOVE      "SAMPLE PREVIOUSLY CLEARED" TO SAMPLE
         RETURN
ENTRENT  MOVE      "X" TO ENTIRE
         MOVE      "        " TO EXCHANGE
         GOTO      OPRINT1
.EXCHANG1 TYPE      OEXQTY
.         GOTO      OPRINT1 IF NOT EOS
EXCHANG1
.Start Patch #2.3 - INCREASE VAR
.         MATCH     "       " TO OEXQTY
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         GOTO      OPRINT1
.ENTIRE   TYPE      OEXQTY
.         GOTO      OPRINT1 IF NOT EOS
ENTIRE
.Start Patch #2.3 - INCREASE VAR
.         MATCH     "       " TO OEXQTY
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         MOVE      "X" TO ENTIRE
OPRINT1
.START PATCH 2.56 - ADDED LOGIC
.Code stolen from NORD002L.PLS
.Print Order Form
.START PATCH 2.74A REPLACED LOGIC
.         print   033,"&l1E",033,"&a0c0R":
.                   033,"*p1687.5x112.5Y":
.                   033,"(8U",033,"(s1p18.00v0s+3b4101T","Names":
.                   b2,033,"(8U",033,"(s1p17.00v1s-3b4101T"," in the News":
.                   033,"*p1620x132.5Y",033,"*c900a02b0p":
.                   033,"*p1687.5x172.5Y":
.               033,"(8U",033,"(s1p09.00v0s-2b4101T","C  A  L  I  F  O  R  N  I  A     I  N  C .":
.               033,"*p1780x264Y":
.               033,"(8U",033,"(s1p08.00v0s-2b4101T","1300 Clay Street, 11th Floor":
.               033,"*p1805x294Y":
.               033,"(8U",033,"(s1p08.00v0s-2b4101T","Oakland, CA 94612-1429":
.               033,"*p1755x324Y":
.               "415-989-3350 ",bullet," Fax 415-433-7796":
.               033,"*c2G",033,"*c300.0A",033,"*c3B",033,"*p50.0y0.0X",033,"*c0P":            .box top
.               033,"*c2G",033,"*c300.0A",033,"*c3B",033,"*p125.0y0.0X",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p50.0y0.0X",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p50.0y300.0X",033,"*c0P":            .box line right
.               033,"*p40x110Y":
.               033,"(8U",033,"(s1p13.00v1s+3b4101T","List Order":
.               033,"*c2G",033,"*c1570.0A",033,"*c3B",033,"*p0.0x210.0Y",033,"*c0P":            .top Line Main form
.               033,"*c2G",033,"*c800.0A",033,"*c3B",033,"*p1620.0x210.0Y",033,"*c0P":            .top Line right side form
.               033,"*c1G",033,"*c3000.0B",033,"*c3A",033,"*p0.0x210.0Y",033,"*c0P":            .Vert Line Left side form
.               033,"*c1G",033,"*c3000.0B",033,"*c3A",033,"*p1620.0x210.0Y",033,"*c0P":            .Vert Line right side form
.               033,"*p1687.5x480Y":
.               033,"(8U",033,"(s1p7.00v0s+2b4101T","Note":
.               033,"*p1687.5x525Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Names are furnished for one-time use to the":
.               033,"*p1687.5x560Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","organization or individual requesting the rental":
.               033,"*p1687.5x595Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","or exchange and are not to be copied, revised in":
.               033,"*p1687.5x630Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","any form, sold, or given to any other party.":
.               033,"*p1687.5x665Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Although we believe the information concerning":
.               033,"*p1687.5x700Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","this list to be accurate we cannot guarantee its":
.               033,"*p1687.5x735Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","accuracy or the outcome of the mailing. Names":
.               033,"*p1687.5x770Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","cannot be used for telemarketing without written":
.               033,"*p1687.5x805Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","permission from the list owner. Post-merge tapes":
.               033,"*p1687.5x840Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","cannot be used for storing and scanning":
.               033,"*p1687.5x875Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","purposes without notification to the list owner.":
.               033,"*p1687.5x910Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Mailer reserves the right to mail duplicates at a":
.               033,"*p1687.5x945Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","later date.":
.               033,"*p1687.5x1040Y":
.               033,"(8U",033,"(s1p7.00v0s+2b4101T","Billing":
.               033,"*p1687.5x1085Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","On completion of addressing, bill rentals c/o":
.               033,"*p1687.5x1120Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Names in the News, California, Inc., less":
.              033,"*p1687.5x1155Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","brokerage commission. We will bill mailer on":
.              033,"*p1687.5x1190Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","behalf of list owner; payment (less commission)":
.              033,"*p1687.5x1225Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","will be made upon receipt of payment from the":
.              033,"*p1687.5x1260Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","mailer. We reserve the right to deduct for":
.              033,"*p1687.5x1295Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","unusable names from payment to list owner. We":
.              033,"*p1687.5x1330Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","act only as agent for the list owner or the mailer":
.              033,"*p1687.5x1365Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","in these transactions.":
.               033,"*p1687.5x1450Y":
.               033,"(8U",033,"(s1p7.00v0s+2b4101T","Payment":
.              033,"*p1687.5x1495Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Due upon receipt of invoice. Full amount":
.              033,"*p1687.5x1530Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","required on orders cancelled after mail date; if":
.              033,"*p1687.5x1565Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Orders, cancelled by mailer prior to mail date are":
.              033,"*p1687.5x1600Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","subject to a $50.00 processing fee.":
.              033,"*p1687.5x1695Y":
.               033,"(8U",033,"(s1p7.00v0s+2b4101T","Addressing":
.              033,"*p1687.5x1740Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Unless stated in special instructions, it is":
.              033,"*p1687.5x1775Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","assumed list is in strict zip sequence. Address":
.              033,"*p1687.5x1810Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","USA names only. All Canadian, foreign, military,":
.              033,"*p1687.5x1845Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","business, library and institutional names are to":
.              033,"*p1687.5x1880Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","be omitted.":
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2000.5y1677.5X",033,"*c0P":            .box top (test)
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2030.5y1677.5X",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2000.5y1677.5X",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2000.5y1707.5X",033,"*c0P":            .box line right
.               033,"*p1715.5x2022.5Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a test. Address a representative cross":
.               033,"*p1687.5x2058Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","section and keep a record to avoid duplication":
.               033,"*p1687.5x2093Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","on continuations.":
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2150.5y1677.5X",033,"*c0P":            .box top (cont)
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2180.5y1677.5X",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2150.5y1677.5X",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2150.5y1707.5X",033,"*c0P":            .box line right
.               033,"*p1715.5x2172.5Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a continuation. No omit required.":
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2233.35Y",033,"*c0P":            .box top (cont)
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2263.35Y",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1677.5x2233.35Y",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1707.5x2233.35Y",033,"*c0P":            .box line right
.               033,"*p1715.5x2255.85Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a continuation. Omit the Following:":
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2383.35Y",033,"*c0P":            .box top (cont)
.               033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2413.35Y",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1677.5x2383.35Y",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1707.5x2383.35Y",033,"*c0P":            .box line right
.               033,"*p1715.5x2405.85Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Entire list/or all available in specified":
.               033,"*p1687.5x2440.85Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","select. Advise if quantity differs by 10% or more.":
.               033,"*p1687.5x2533.0Y":
.               033,"(8U",033,"(s1p7.00v0s+2b4101T","Important":
.              033,"*p1687.5x2578Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","If you cannot comply with these instructions, or":
.              033,"*p1687.5x2613Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","if charges shown are not correct, notify us by":
.              033,"*p1687.5x2648Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","phone for our approval prior to addressing.":
.              033,"*p1687.5x2683Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","Adjustments not approved before addressing will":
.              033,"*p1687.5x2718Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","not be accepted. Magnetic Tape orders - tape will":
.              033,"*p1687.5x2753Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","not be returned unless requested in writing by ":
.              033,"*p1687.5x2788Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","list owner.":
.              033,"*p1687.5x2858Y":
.               033,"(8U",033,"(s1p7.00v0s-2b4101T","NIN Contact:":
.              033,"*p1687.5x3158Y":
.               033,"(8U",033,"(s1p9.00v1s-2b4101T","Member Direct Marketing Association":
.              033,"*p40.0x335.Y":
.               033,"(8U",033,"(s1p7.00v1s+2b4101T","LR ##":
.              033,"*p745.0x335.Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Date:":
.              033,"*p40.0x380.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Mailer P.O.":
.              033,"*p745.0x380.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Client No.:":
.              033,"*p40.0x425.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Mailer:":
.              033,"*p40.0x580.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Offer:":
.              033,"*p40.0x730.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","List Owner:":
.              033,"*p40.0x993.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","C.C. To:":
.              033,"*p40.0x1080.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","List:":
.              033,"*p40.0x1270.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Quantity:":
.              033,"*p745.0x1270.0Y":
.               033,"(8U",033,"(s1p8.00v1s-2b4101T","Per M $":
.              033,"*p40.0x1400.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Key/List ID:":
.              033,"*p40.0x1495.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Furnished on:":
.              033,"*p40.0x1570.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Return to:":
.              033,"*p40.0x2020.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Ship to arrive by:":
.              033,"*p745.0x2020.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Via:":
.              033,"*p40.0x2140.0Y":
.               033,"(8U",033,"(s1p7.00v1s-2b4101T","Mail Date:":
.              033,"*p40.0x2200.0Y":
.               033,"(8U",033,"(s1p7.00v1s+2b4101T","Special Instructions:":
.                033,"*p0x0Y":                                                                   .reset position for inv
.               "&l3E&f1S&f1X&f10X";
..........................................
.Comment Out Patch 2.76
.         print     033,"&l1E",033,"&a0c0R":
.                   033,"*p1687.5x112.5Y":
.                   033,"(8U",033,"(s1p18.00v0s+3b4101T","Names":
.                   b2,033,"(8U",033,"(s1p17.00v1s-3b4101T"," in the News":
.                   033,"*p1620x132.5Y",033,"*c900a02b0p":
.                   033,"*p1687.5x172.5Y":
.                   033,"(8U",033,"(s1p09.00v0s-2b4101T","C  A  L  I  F  O  R  N  I  A     I  N  C .":
.                   033,"*p1780x264Y":
.                   033,"(8U",033,"(s1p08.00v0s-2b4101T","1300 Clay Street, 11th Floor":
.                   033,"*p1805x294Y":
.                   033,"(8U",033,"(s1p08.00v0s-2b4101T","Oakland, CA 94612-1429":
.                   033,"*p1755x324Y":
.                   "415-989-3350 ",bullet," Fax 415-433-7796":
.                   033,"*c2G",033,"*c300.0A",033,"*c3B",033,"*p50.0y0.0X",033,"*c0P":            .box top
.                   033,"*c2G",033,"*c300.0A",033,"*c3B",033,"*p125.0y0.0X",033,"*c0P":            .box bottom
.                   033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p50.0y0.0X",033,"*c0P":            .box line left
.                   033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p50.0y300.0X",033,"*c0P":            .box line right
.                   033,"*p40x110Y":
.                   033,"(8U",033,"(s1p13.00v1s+3b4101T","List Order":
.                   033,"*c2G",033,"*c1570.0A",033,"*c3B",033,"*p0.0x210.0Y",033,"*c0P":            .top Line Main form
.                   033,"*c2G",033,"*c800.0A",033,"*c3B",033,"*p1620.0x210.0Y",033,"*c0P":            .top Line right side form
.                   033,"*c1G",033,"*c3000.0B",033,"*c3A",033,"*p0.0x210.0Y",033,"*c0P":            .Vert Line Left side form
.                   033,"*c1G",033,"*c3000.0B",033,"*c3A",033,"*p1620.0x210.0Y",033,"*c0P":            .Vert Line right side form
.                   033,"*p1687.5x480Y":
.                   033,"(8U",033,"(s1p7.00v0s+2b4101T","Note":
.                   033,"*p1687.5x525Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Names are furnished for one-time use to the":
.                   033,"*p1687.5x560Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","organization or individual requesting the rental":
.                   033,"*p1687.5x595Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","or exchange and are not to be copied, revised in":
.                   033,"*p1687.5x630Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","any form, sold, or given to any other party.":
.                   033,"*p1687.5x665Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Although we believe the information concerning":
.                   033,"*p1687.5x700Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","this list to be accurate we cannot guarantee its":
.                   033,"*p1687.5x735Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","accuracy or the outcome of the mailing. Names":
.                   033,"*p1687.5x770Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","cannot be used for telemarketing without written":
.                   033,"*p1687.5x805Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","permission from the list owner. Post-merge tapes":
.                   033,"*p1687.5x840Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","cannot be used for storing and scanning":
.                   033,"*p1687.5x875Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","purposes without notification to the list owner.":
.                   033,"*p1687.5x910Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Mailer reserves the right to mail duplicates at a":
.                   033,"*p1687.5x945Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","later date.":
.                   033,"*p1687.5x1040Y":
.                   033,"(8U",033,"(s1p7.00v0s+2b4101T","Billing":
.                   033,"*p1687.5x1085Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","On completion of addressing, bill rentals c/o":
.                   033,"*p1687.5x1120Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Names in the News, California, Inc., less":
.                   033,"*p1687.5x1155Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","brokerage commission. We will bill mailer on":
.                   033,"*p1687.5x1190Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","behalf of list owner; payment (less commission)":
.                   033,"*p1687.5x1225Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","will be made upon receipt of payment from the":
.                   033,"*p1687.5x1260Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","mailer. We reserve the right to deduct for":
.                   033,"*p1687.5x1295Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","unusable names from payment to list owner. We":
.                   033,"*p1687.5x1330Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","act only as agent for the list owner or the mailer":
.                   033,"*p1687.5x1365Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","in these transactions.":
.                   033,"*p1687.5x1450Y":
.                   033,"(8U",033,"(s1p7.00v0s+2b4101T","Payment":
.                   033,"*p1687.5x1495Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Due upon receipt of invoice. Full amount":
.                   033,"*p1687.5x1530Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","required on orders cancelled after mail date; if":
.                   033,"*p1687.5x1565Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Orders, cancelled by mailer prior to mail date are":
.                   033,"*p1687.5x1600Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","subject to a $50.00 processing fee.":
.                   033,"*p1687.5x1695Y":
.                   033,"(8U",033,"(s1p7.00v0s+2b4101T","Addressing":
.                   033,"*p1687.5x1740Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Unless stated in special instructions, it is":
.                   033,"*p1687.5x1775Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","assumed list is in strict zip sequence. Address":
.                   033,"*p1687.5x1810Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","USA names only. All Canadian, foreign, military,":
.                   033,"*p1687.5x1845Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","business, library and institutional names are to":
.                   033,"*p1687.5x1880Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","be omitted.":
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2000.5y1677.5X",033,"*c0P":            .box top (test)
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2030.5y1677.5X",033,"*c0P":            .box bottom
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2000.5y1677.5X",033,"*c0P":            .box line left
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2000.5y1707.5X",033,"*c0P":            .box line right
.                   033,"*p1715.5x2022.5Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a test. Address a representative cross":
.                   033,"*p1687.5x2058Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","section and keep a record to avoid duplication":
.                   033,"*p1687.5x2093Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","on continuations.":
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2150.5y1677.5X",033,"*c0P":            .box top (cont)
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p2180.5y1677.5X",033,"*c0P":            .box bottom
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2150.5y1677.5X",033,"*c0P":            .box line left
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p2150.5y1707.5X",033,"*c0P":            .box line right
.                   033,"*p1715.5x2172.5Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a continuation. No omit required.":
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2233.35Y",033,"*c0P":            .box top (cont)
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2263.35Y",033,"*c0P":            .box bottom
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1677.5x2233.35Y",033,"*c0P":            .box line left
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1707.5x2233.35Y",033,"*c0P":            .box line right
.                   033,"*p1715.5x2255.85Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","This is a continuation. Omit the Following:":
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2383.35Y",033,"*c0P":            .box top (cont)
.                   033,"*c2G",033,"*c30.0A",033,"*c3B",033,"*p1677.5x2413.35Y",033,"*c0P":            .box bottom
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1677.5x2383.35Y",033,"*c0P":            .box line left
.                   033,"*c1G",033,"*c30.0B",033,"*c3A",033,"*p1707.5x2383.35Y",033,"*c0P":            .box line right
.                   033,"*p1715.5x2405.85Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Entire list/or all available in specified":
.                   033,"*p1687.5x2440.85Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","select. Advise if quantity differs by 10% or more.":
.                   033,"*p1687.5x2533.0Y":
.                   033,"(8U",033,"(s1p7.00v0s+2b4101T","Important":
.                   033,"*p1687.5x2578Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","If you cannot comply with these instructions, or":
.                   033,"*p1687.5x2613Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","if charges shown are not correct, notify us by":
.                   033,"*p1687.5x2648Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","phone for our approval prior to addressing.":
.                   033,"*p1687.5x2683Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","Adjustments not approved before addressing will":
.                   033,"*p1687.5x2718Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","not be accepted. Magnetic Tape orders - tape will":
.                   033,"*p1687.5x2753Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","not be returned unless requested in writing by ":
.                   033,"*p1687.5x2788Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","list owner.":
.                   033,"*p1687.5x2858Y":
.                   033,"(8U",033,"(s1p7.00v0s-2b4101T","NIN Contact:":
.                   033,"*p1687.5x3158Y":
.                   033,"(8U",033,"(s1p9.00v1s-2b4101T","Member Direct Marketing Association":
.                   033,"*p40.0x285.Y":
.                   033,"(8U",033,"(s1p7.00v1s+2b4101T","LR ##":
.                   033,"*p745.0x285.Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Date:":
.                   033,"*p40.0x330.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Mailer P.O.":
.                   033,"*p745.0x330.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Client No.:":
.                   033,"*p40.0x375.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Mailer:":
.                   033,"*p40.0x530.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Offer:":
.                   033,"*p40.0x680.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","List Owner:":
.                   033,"*p40.0x943.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","C.C. To:":
.                   033,"*p40.0x985.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","List:":
.                   033,"*p40.0x1220.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Quantity:":
.                   033,"*p745.0x1220.0Y":
.                   033,"(8U",033,"(s1p8.00v1s-2b4101T","Price $":
.                   033,"*p40.0x1595.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Key/List ID:":
.                   033,"*p40.0x1690.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Furnished on:":
.                   033,"*p40.0x1765.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Return to:":
.                   033,"*p40.0x2080.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Ship to arrive by:":
.                   033,"*p745.0x2080.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Via:":
.                   033,"*p40.0x2140.0Y":
.                   033,"(8U",033,"(s1p7.00v1s-2b4101T","Mail Date:":
.                   033,"*p40.0x2200.0Y":
.                   033,"(8U",033,"(s1p7.00v1s+2b4101T","Special Instructions:":
.                   033,"*p0x0Y":                                                                   .reset position for inv
.                   "&l3E&f1S&f1X&f10X";
.END PATCH 2.74A REPLACED LOGIC
.Print Fulfillment Box
.          Print        033,"&l1E",033,"*p0x0Y":
.                       033,"*c2G",033,"*c505.0A",033,"*c3B",033,"*p750.0x50.0Y",033,"*c0P":            .box top
.                       033,"*c2G",033,"*c505.0A",033,"*c3B",033,"*p750.0x125.0Y",033,"*c0P":            .box bottom
.                       033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p750.0x50.0Y",033,"*c0P":            .box line left
.                       033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1255.0x50.0Y",033,"*c0P":            .box line right
.                       033,"*p805x110Y":
.                       033,"(8U",033,"(s1p13.00v0s+3b4101T","Fulfilment Copy":
.                       033,"&a0c0R",*l,*l;
.         PRINT     *N,hpdtch85,hpitalic,hpdtch10,hpuprght,hpt200;
.Comment Out Patch 2.76
.Logic Added 2.76
          clear     str2
          pack      str2,OSALES10,OSALES
.Note for patch 2.93:  prtorderpage1.inc takes care of Logos for Pacific Lists!!
.begin patch 2.96
.         if (str2 = "06" | str2 = "19" | str2 = "")          .list management
          if (str2 = "06" | str2 = "19" | str2 = "" | str2 = "27" | str2 = "28")          .list management
.end patch 2.96
                    call      prtordfrmGuiA
          else
                    call      prtordfrmGuiB
          endif
          call      prtfulfilboxGui
.Logic Added 2.76
.END PATCH 2.56 - ADDED LOGIC
         PACK      LROUT FROM LRMASK
         MOVE      OLRN TO LRNUM
         EDIT      LRNUM TO LROUT
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *N,*13,REPRT,*20,REVDATA;
.         PRINT     *N,*7,LROUT;
.Patch 2.74 Commented Out
.         PRINT     hpt0125,REPRT,hpt200,hpbon,REVDATA,*n,hpboff;
.Patch 2.74 Commented Out
.Patch 2.75 Logic Added
                               prtpage  Laser;*p=2000:425,*font=fontO12b,REVDATA
.Patch 2.75 Logic Added
.START PATCH 2.74A REPLACED LOGIC
.         PRINT     *N,*N,hpt050,LROUT;
.Patch 2.74 Commented Out
.         PRINT     *N,hpt050,LROUT;
.Patch 2.74 Commented Out
.Patch 2.75 Logic Added
                              prtpage   Laser;*p=625:800,*font=fontO10,LROUT
.Patch 2.75 Logic Added
.END PATCH 2.74A REPLACED LOGIC
.END PATCH 2.56 - REPLACED LOGIC
.Start Patch #2.3 - added century
.         PRINT     *31,OODTEM,SLASH,OODTED,SLASH,OODTEY:
.                   *N,*10,OMLRPON,*31;
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *31,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
.                   *N,*10,OMLRPON,*31;
.Patch 2.74 Commented Out
.         PRINT     hpt325,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
.                   *N,hpt100,OMLRPON;
.Patch 2.74 Commented Out
.Patch 2.75 Logic Added
                              prtpage   Laser;*p=3125:800,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
                              prtpage   Laser;*p=1000:988,OMLRPON
.Patch 2.75 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
.End Patch #2.3 - added century
.START PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
.         PRINT     OMLRNUM,SLASH,OCOBN:
.                   *N,*12,MNAME,*L,*12,MCOMP,*40,MCCTO:
.                   *N,*L,*L:
.                   *12,OODES,*N,*2,"##",OFFEROUT,*12,SAMPLE:
.                   *N,*12,OWNLONM:
.                   *N,*12,OWNOCPY:
.                   *N,*12,OWNLOSA:
.                   *N,*12,*+,OWNLOCTY,*-,", ",OWNLOS," ",OWNLOZC:
.                   *N,*L,*12,OWNCTN,*L,*L:
.                   *12,O1DES,*N,*2,"##",OLNUM,*12,O2DES:
.                   *N;
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     OMLRNUM,SLASH,OCOBN:
.                   *N,*12,MNAME,*L,*12,MCOMP,*40,MCCTO:
.                   *N,*L,*L:
.                   *12,OFDESC,*N,*2,"##",OFFEROUT,*12,SAMPLE:
.                   *N,*12,OWNLONM:
.                   *N,*12,OWNOCPY:
.                   *N,*12,OWNLOSA:
.                   *N,*12,*+,OWNLOCTY,*-,", ",OWNLOS," ",OWNLOZC:
.                   *N,*L,*12,OWNCTN,*L,*L:
.                   *12,O1DES,*N,*2,"##",OLNUM,*12,O2DES:
.                   *N;
.START PATCH 2.62 REPLACED LOGIC
.         PRINT     hpt325,OMLRNUM,SLASH,OCOBN:
.                   *N,hpt100,MNAME,*L,hpt100,MCOMP,hpt375,MCCTO:
.                   *N,*L:
.                   hpt100,OFDESC,*N,hpt0125,"##",OFFEROUT,hpt100,SAMPLE:
.                   *N,*L,hpt100,OWNLONM:
.                   *N,hpt100,OWNOCPY:
.                   *N,hpt100,OWNLOSA:
.                   *N,hpt100,*+,OWNLOCTY,*-,", ",OWNLOS," ",OWNLOZC:
.                   *N,*L,hpt100,OWNCTN,*L,*L:
.                   hpt100,O1DES,*N,hpt0125,"##",OLNUM,hpt100,O2DES:
.                   *N;
.START PATCH 2.74A REPLACED LOGIC
.         PRINT     hpt325,OMLRNUM,SLASH,OCOBN:
.                   *N,hpt100,MNAME,*L,hpt100,MCOMP,hpt375,MCCTO:
.                   *N,*L:
.                   hpt100,OFDESC,*N,hpt0125,"##",OFFEROUT,hpt100,SAMPLE:
.                   *N,*L,hpt100,OWNLONM:
.                   *N,hpt100,OWNOCPY:
.                   *N,hpt100,OWNLOSA:
.                   *N,hpt100,*+,OWNLOCTY,*-,", ",OWNLOS," ",OWNLOZC:
.                   *N,*L,hpt100,NFULCOMP,*L,*L:
.                   hpt100,O1DES,*N,hpt0125,"##",OLNUM,hpt100,O2DES:
.                   *N;
          if (NSEL2SPRICE > C0)
                    unpack    NSEL2SPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
                    call      Trim using NSEL2NAME
                    pack      taskname,NSEL2NAME," @ ",str9,NMODDESC
          else
                    pack      taskname,NSEL2NAME
          endif
.Patch 2.74 Commented Out
.Patch 2.76 Comment out
.         PRINT     hpt325,OMLRNUM,SLASH,OCOBN:
.                   *N,hpt100,MNAME,*L,hpt100,MCOMP,hpt375,MCCTO:
.                   *N,*L:
.                   hpt100,OFDESC,*N,hpt0125,"##",OFFEROUT,hpt100,SAMPLE:
.                   *N,*L,hpt100,OWNLONM:
.                   *N,hpt100,OWNOCPY:
.                   *N,hpt100,OWNLOSA:
.                   *N,hpt100,*+,OWNLOCTY,*-,", ",OWNLOS," ",OWNLOZC:
.                   *N,*L,hpt100,NFULCOMP,*L:
.                   hpt100,O1DES,*N,hpt0125,"##",OLNUM,hpt100,taskname:
.                   *N;
.Patch 2.76 Comment out
.Patch 2.74 Commented Out
.Patch 2.75 Logic Added
                                                            prtpage   Laser;*p=3125:988,OMLRNUM,SLASH,OCOBN
                                                            prtpage   Laser;*p=1000:1176,MNAME
                                                            prtpage   Laser;*p=1000:1351,MCOMP,b5,MCCTO
                                                            prtpage   Laser;*p=1000:1603,OFDESC
                                                            prtpage   Laser;*p=125:1791,"##",OFFEROUT
                                                            prtpage   Laser;*p=1000:1791,SAMPLE
                                                            prtpage   Laser;*p=1000:2093,OWNLONM
.                                                           prtpage   Laser;*p=125:2268,"##",OLON
                                                            prtpage   Laser;*p=1000:2268,OWNOCPY
                                                            prtpage   Laser;*p=1000:2443,OWNLOSA
                                                            call      Trim using OWNLOCTY
                                                            if (OWNLOCTY <> "")
                                                                      pack      str255,OWNLOCTY,", ",OWNLOS," ",OWNLOZC
                                                            else
                                                                      pack      str255,OWNLOS," ",OWNLOZC
                                                            endif
                                                            prtpage   Laser;*p=1000:2618,str255
                                                            prtpage   Laser;*p=1000:2918,NFULCOMP
                                                            prtpage   Laser;*p=125:3399,"##",OLNUM
                                                            prtpage   Laser;*p=1000:3211,O1DES
                                                            prtpage   Laser;*p=1000:3399,taskname
.Patch 2.75 Logic Added
.END PATCH 2.74A REPLACED LOGIC
.END PATCH 2.62 REPLACED LOGIC
.END PATCH 2.56 - REPLACED LOGIC
.END PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
         move       c0 to n2
         move       onetper to n2
         compare    c0 to n2
         if         not equal
           cmatch    no to onetfm
            if        equal
.START PATCH 2.56 - REPLACED LOGIC
.            print     *12,"Per List Owner - Gross Billing":
.                      " No Deductionss",*l;
.Patch 2.76 Comment Out
.            print     hpt100,hpdtch85,hpitalic,"Per List Owner - Gross Billing":
.                      " No Deductions",hpdtch10,hpuprght,*l;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                                        prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,"Per List Owner - Gross Billing No Deductions"
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
            endif
          cmatch     "F" to onetfm
           if         not equal
.START PATCH 2.56 - REPLACED LOGIC
.           print      *12,"Mailer Guarantees ",onetper:
.                      "% payment on Gross Names Shipped":
.                      *n,*12,"& will pay $",onetrc,"/m running charge on":
.                      " unused names.";
.          else
.           print      *12,onetper,"% Volume Discount":
.                      *n;
.           endif
.         else
.          print      *l;
.         endif
.         print       *L,*8,QTYOUT;
............
.Patch 2.76 Comment out
.           print      hpt100,hpdtch85,hpitalic,"Mailer Guarantees ",onetper:
.                      "% payment on Gross Names Shipped":
.                      *n,hpt100,"& will pay $",onetrc,"/m running charge on":
.                      " unused names.",hpdtch10,hpuprght;
.Patch 2.76 Comment out
.Patch 2.76 Logic Added
                              prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,"Mailer Guarantees ",onetper,"% payment on Gross Names Shipped"
                              prtpage   Laser;*p=1000:5033,"& will pay $",onetrc,"/m running charge on unused names."
.Patch 2.76 Logic Added
          else
.Patch 2.76 Not Sure Check with JD on wording
.                   print     hpt100,hpdtch85,hpitalic,onetper,"% Volume Discount":
.                             hpdtch10,hpuprght;
.                   print     *l,*l;
.Patch 2.76 Not Sure
.Patch 2.76 Logic Added
.                             prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,onetper,"% Volume Discount on Gross Names Shipped"
                              prtpage   Laser;*p=1000:4903,*font=FontO7Dot5I,onetper,"% Volume Discount"
.Patch 2.76 Logic Added
           endif
         else
.Patch 2.76 Not Sure
.          print      *l,*l;
.Patch 2.76 Not Sure
         endif
.Patch 2.76 Comment Out
.         print       *L,hpt100,hpdtch10,hpuprght,QTYOUT;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              prtpage   Laser;*p=1000:3587,*font=fontO10,QTYOUT
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
.Start Patch #2.3 - INCREASE VAR
.         MATCH     "        " TO OEXQTY
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT2 IF EOS
         GOTO      OPRINT2 IF EQUAL
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *27,"SEE BELOW";
.Patch 2.76 Comment Out
.         PRINT     hpt300,"SEE BELOW";
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
          prtpage   Laser;*p=3000:3587,"SEE BELOW"
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
         GOTO      OPRINT3
OPRINT2  MATCH     "EXCHANGE" TO EXCHANGE
         GOTO      REALPPM IF NOT EQUAL
.START PATCH 2.74A REPLACED LOGIC
.         PACK      PRICECK FROM F3,PERIOD,F2
.         MATCH     "   .00" TO PRICECK
.         GOTO      REALPPM IF NOT EQUAL
          if (NSEL2PRICE <> C0)
                    goto REALPPM
          endif
.END PATCH 2.74A REPLACED LOGIC
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *27,"EXCHANGE";
.Patch 2.76 Comment Out
.         PRINT     hpt300,"EXCHANGE";
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
          prtpage   Laser;*p=3000:3587,"EXCHANGE"
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
         GOTO      OPRINT3
REALPPM
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *27,F3,".",F2," ",EXCHANGE;
.OPRINT3  PRINT     *N,*L,*12,OMLRKY:
.                   *N,*L,*12,MEDIA:
.                   *N,*L,*12,RTCNTCT:
.                   *N,*08,CORTN,*12,RTCOMP,*L,*12,RTADDR;
.         MATCH     "0001" TO ORTNNUM
.         CALL      REUSE IF EQUAL
.         PRINT     *N,*12,*+,RTCITY,*-,", ";
.         PRINT     RTSTATE,"  ",RTZIP:
.                   *N,*L,*1,COMSLCT,*N,*L,*L,*12,ORTNDTEM,SLASH;
..Start Patch #2.3 - added century
..         PRINT     ORTNDTED,SLASH,ORTNDTEY;
.         PRINT     ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY;
..End Patch #2.3 - added century
.............
.START PATCH 2.74A REPLACED LOGIC
.         PRINT     hpt300,F3,".",F2," ",EXCHANGE;
          unpack    NSEL2PRICE,str5,str3
          call      FormatNumeric using str5,str6
          pack      str9,str6,str3
          pack      taskname,str9,NMODDESC," ",EXCHANGE
.Patch 2.76 Comment Out
.         PRINT     hpt300,taskname;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
          prtpage   Laser;*p=3000:3587,taskname
.Patch 2.76 Logic Added
.END PATCH 2.74A REPLACED LOGIC
OPRINT3
.START PATCH 2.74A ADDED LOGIC
          deleteitem PackData,0
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
.START PATCH 2.98 REPLACED LOGIC
.                              if not over
                                                       move       NSEL3NUM,NADDNUM
.
                                                       call       Trim using NSEL3MODNUM
                                                       if (NSEL3MODNUM <> "")
                                                                  move       NSEL3MODNUM,NADDDESC
                                                       endif
                                                       call       Trim using NADDDESC
                                                       if (NADDDESC = "")
                                                                  move       "XXX",NADDDESC        .Force an over...
                                                       endif
.END PATCH 2.98 REPLACED LOGIC
                                        pack      NREFFLD,"A",NADDNUM
                                        move      "NREFKEY",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        pack      NMODFLD,NADDDESC
                                        rep       zfill,NMODFLD
                                        move      "NMODKEY",Location
                                        pack      KeyLocation,"Key: ",NMODFLD
                                        call      NMODKEY
.START PATCH 2.75 REPLACED LOGIC
.                                       pack      taskname,NREFDESC,NADDPRICE,NMODDESC
                                        pack      taskname,NREFDESC,NSEL3PRICE,NMODDESC
.END PATCH 2.75 REPLACED LOGIC
                                        insertitem PackData,0,taskname
.START PATCH 2.98 REMOVED LOGIC
.                              endif
.END PATCH 2.98 REMOVED LOGIC
                    elseif (NSEL3CODE = "L")
                              pack      NSLTFLD,OLNUM,NSEL3NUM
                              move      "NSLTKEY",Location
                              pack      KeyLocation,"Key: ",NSLTFLD
                              call      NSLTKEY
.START PATCH 2.98 REPLACED LOGIC
.                              if not over
                                                       move       NSEL3NUM,NSLTNUM
.
                                                       call       Trim using NSEL3MODNUM
                                                       if (NSEL3MODNUM <> "")
                                                                  move       NSEL3MODNUM,NSLTDESC
                                                       endif
                                                       call       Trim using NSLTDESC
                                                       if (NSLTDESC = "")
                                                                  move       "XXX",NSLTDESC        .Force an over...
                                                       endif
.END PATCH 2.98 REPLACED LOGIC
                                        pack      NREFFLD,"L",NSLTNUM
                                        move      "NREFKEY-2",Location
                                        pack      KeyLocation,"Key: ",NREFFLD
                                        call      NREFKEY
                                        pack      NMODFLD,NSLTDESC
                                        rep       zfill,NMODFLD
                                        move      "NMODKEY-2",Location
                                        pack      KeyLocation,"Key: ",NMODFLD
                                        call      NMODKEY
.START PATCH 2.75 REPLACED LOGIC
.                                       pack      taskname,NREFDESC,NSLTPRICE,NMODDESC
                                        pack      taskname,NREFDESC,NSEL3PRICE,NMODDESC
.END PATCH 2.75 REPLACED LOGIC
                                        insertitem PackData,0,taskname
.START PATCH 2.98 REMOVED LOGIC
.                              endif
.END PATCH 2.98 REMOVED LOGIC
                    endif
                    move      "NSEL3KG",Location
                    call      NSEL3KG
          repeat
          move      C0,N2
.Patch 2.76 Commented Out
.         PackData.GetCount giving N10
.         if (N10 > C0)
.                   if (N10 > 12)
.                             move      "12",N10
.                   endif
.                   move      C0,N1
.                   for result,"1",N10
.                             getitem   PackData,result,taskname
.                             unpack    taskname,NREFDESC,NSEL3PRICE,NMODDESC
.                             if (NSEL3PRICE > 0)
.                                       unpack    NSEL3PRICE,str5,str3
.                                       call      FormatNumeric using str5,str6
.                                       call      Trim using NREFDESC
.                                       call      Trim using NMODDESC
.                                       pack      taskname,NREFDESC," @ ",str6,str3,NMODDESC
.                             else
.                                       pack      taskname,NREFDESC
.                             endif
.Patch 2.76 Commented Out
.Patch 2.76 Logic Added
          PackData.GetCount giving N10
          if (N10 > C0)
.                   move      "625",N8
.Sales wants this moved over.  They will bitch about it later, no doubt.
                    move      "1000",N8
                    move      "3587",N9
                    for result,"1",N10
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
.Patch 2.76 Logic Added
.                             if (result = 10 & N10 > 10)
.                                       prtpage   Laser;*p=N8:N9,"Additional Prices Below"
..                                      move      C1,PriceFlag
.                                       break
.                             endif
.Patch 2.76 Commented Out
.                             if (N1 = C0)
.                                       print     *N,hpt100,taskname;
.                                       move      C1,N1
.                                       add       C1,N2
.                             else
.                                       print     hpt300,taskname;
.                                       move      C0,N1
.                             endif
.Patch 2.76 Commented Out
                    repeat
          endif
          for result,N2,"6"
.Patch 2.76 Not Sure
.                   print     *N;
.Patch 2.76 Not Sure
          repeat
.END PATCH 2.74A ADDED LOGIC
.START PATCH 2.74A REPLACED LOGIC
.         PRINT     *N,*L,hpt100,OMLRKY:
.                   *N,*L,hpt100,MEDIA:
.                   *N,*L,hpt100,RTCNTCT:
.                   *N,hpt0125,CORTN,hpt100,RTCOMP,*L,hpt100,RTADDR;
.Patch 2.76 Commented Out
.         PRINT     hpt100,OMLRKY:
.                   *N,*L,hpt100,MEDIA:
.                   *N,*L,hpt100,RTCNTCT:
.                   *N,hpt0125,CORTN,hpt100,RTCOMP,*L,hpt100,RTADDR;
.Patch 2.76 Commented Out
.Patch 2.76 Logic Added
          prtpage   Laser;*p=1000:5251,OMLRKY
          prtpage   Laser;*p=1000:5501,MEDIA,"  ",MEDMEMO
          prtpage   Laser;*p=1000:5751,RTCNTCT
          prtpage   Laser;*p=125:5926,CORTN
          prtpage   Laser;*p=1000:5926,RTCOMP
          call      Trim using RTADDR
          if (ORTNNUM = "0001")
                    prtpage   Laser;*p=1000:6101,RTADDR,B1,OREUSE
          else
                    prtpage   Laser;*p=1000:6101,RTADDR
          endif
.Patch 2.76 Logic Added
.END PATCH 2.74A REPLACED LOGIC
.Patch 2.76 Commented Out
.         MATCH     "0001" TO ORTNNUM
.         CALL      REUSE IF EQUAL
.         PRINT     *N,hpt100,*+,RTCITY,*-,", ";
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              call      Trim using RTCITY
                              if (RTCITY <> "")
.START PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!
.                                       pack      taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
                                        if (RTNUM = "5318")
.begin patch 2.97

                                                  pack      taskname,"incoming.files@infogroup.com",COMMA,B1,RTSTATE,B1,B1,RTZIP
.                                                  pack      taskname,"incoming.files@donnelley.infousa.com",COMMA,B1,RTSTATE,B1,B1,RTZIP
.end patch 2.97
                                        else
                                                  pack      taskname,RTCITY,COMMA,B1,RTSTATE,B1,B1,RTZIP
                                        endif
.END PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
                              else
                                        pack      taskname,RTSTATE,B1,B1,RTZIP
                              endif
                              prtpage   Laser;*p=1000:6276,taskname
.Patch 2.76 Logic Added
.START PATCH 2.74A REPLACED LOGIC
.         PRINT     RTSTATE,"  ",RTZIP:
.                   *N,*L,hpt0125,COMSLCT,*N,*L,*L,*L,hpt125,ORTNDTEM,SLASH;
.         PRINT     ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY,hpt540,TEST;
.Patch 2.76 Comment Out
.         PRINT     RTSTATE,"  ",RTZIP:
.                   *N,*L,hpt0125,COMSLCT,hpt540,TEST,*N,hpt125,ORTNDTEM,SLASH;
.         PRINT     ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              prtpage   Laser;*p=125:6501,COMSLCT
                              prtpage   Laser;*p=5640:6733,TEST
                              prtpage   Laser;*p=1250:6689,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.Patch 2.76 Logic Added
.END PATCH 2.74A REPLACED LOGIC
.END PATCH 2.56 - REPLACED LOGIC
         MOVE      "N" TO TDMCSW
.Start Patch #2.3 - INCREASED VAR
.         CMATCH    " " TO OSHP
         MATCH     "  " TO OSHP
.END Patch #2.3 - INCREASED VAR
         GOTO      NOSHIP IF EQUAL
         GOTO      NOSHIP IF EOS
         move      C0 to nfield23
         MOVE      OSHP TO NFIELD23
         MOVE      SHIP0 TO SHIPdesc
         LOAD      SHIPdesc FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                   SHIP6,SHIP7,SHIP8,SHIP9
         GOTO      OPRINT4
NOSHIP   CLEAR     SHIPdesc
OPRINT4
.START PATCH 2.88 REPLACED LOGIC
.         MATCH     "0040" TO ORTNNUM
..         GOTO      OPRINT4A IF NOT EQUAL
.         IF         NOT Equal
.         Match     "5224" to ortnnum
..START PATCH 2.83 REPLACED LOGIC
..         GOTO      OPRINT4A IF NOT EQUAL
.                   MATCH     "5318" TO ORTNNUM
.                   IF NOT EQUAL
..                            GOTO OPRINT4A
.                   ENDIF
..END PATCH 2.83 REPLACED LOGIC
..END PATCH 2.85 REPLACED LOGIC
.                   MATCH     "5316" TO ORTNNUM
.                   IF NOT EQUAL
..                            GOTO OPRINT4A
.                   ENDIF
..END PATCH 2.85 REPLACED LOGIC
..start PATCH 2.86 REPLACED LOGIC
.                   MATCH     "5319" TO ORTNNUM
.                   IF NOT EQUAL
.                             GOTO OPRINT4A
.                   ENDIF
..END PATCH 2.86 REPLACED LOGIC
.         ENDIF
.         MOVE      "Y" TO TDMCSW
.............................................
          IF ((ORTNNUM = "0040")|(ORTNNUM = "5224")|(ORTNNUM = "5318")|(ORTNNUM = "5316")|(ORTNNUM = "5319"))
                    MOVE      "Y",TDMCSW
          endif
.END PATCH 2.88 REPLACED LOGIC
.START PATCH 2.56 - REPLACED LOGIC
.OPRINT4A PRINT     *28,SHIPdesc;
.         PRINT     *N,*L,*12,OMDTEM,SLASH,OMDTED,SLASH;
..Start Patch #2.3 - added century
..         PRINT     OMDTEY,*N,*L,*L,*L,*L;
.         PRINT     OMDTEC,OMDTEY,*N,*L,*L,*L,*L;
..END Patch #2.3 - added century
.START PATCH 2.74A REPLACED LOGIC
.OPRINT4A PRINT     *N,hpt250,SHIPdesc;
.         PRINT     *N,hpt125,OMDTEM,SLASH,OMDTED,SLASH;
.         PRINT     OMDTEC,OMDTEY,*N,hpt540,CONT1,*L;
.Patch 2.76 Comment Out
OPRINT4A
.                             PRINT     hpt300,SHIPdesc;
.         PRINT     *N,hpt125,OMDTEM,SLASH,OMDTED,SLASH;
.         PRINT     OMDTEC,OMDTEY,*N,hpt540,CONT1,*L;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              prtpage   Laser;*p=2500:6689,SHIPdesc
                              prtpage   Laser;*p=1250:6876,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
                              prtpage   Laser;*p=5640:6733,TEST
                              prtpage   Laser;*p=5640:7116,CONT1
.Patch 2.76 Logic Added
.END PATCH 2.74A REPLACED LOGIC
.END PATCH 2.56 - REPLACED LOGIC
.
DISREGO
.Patch 2.76 Logic Added
          move      "7126",result
.Patch 2.76 Logic Added
..START PATCH 2.5 - REPLACED LOGIC
..START PATCH #2.3 - REPLACED LOGIC
..         UNPACK    OSPI INTO SPCL1,SPCL2,SPCL3,SPCL4,SPCL5,SPCL6
.         UNPACK    OSPI INTO STR1,SPCL1,STR1,SPCL2,STR1,SPCL3,STR1,SPCL4,STR1,SPCL5,STR1,SPCL6
..END PATCH #2.3 - REPLACED LOGIC
.         MOVE      SPCL1 TO SPCL
.         MOVE      C1 TO V1
.         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
.         MOVE      SPCL2 TO SPCL
.         MOVE      C2 TO V1
.         CALL      SPCLNSTO
.         MOVE      SPCL3 TO SPCL
.         MOVE      THREE TO V1
.         CALL      SPCLNSTO
.         MOVE      FOUR TO V1
.         MOVE      SPCL4 TO SPCL
.         CALL      SPCLNSTO
.         MOVE      FIVE TO V1
.         MOVE      SPCL5 TO SPCL
.         CALL      SPCLNSTO
.         MOVE      SIX TO V1
.         MOVE      SPCL6 TO SPCL
.         CALL      SPCLNSTO
.         GOTO      TYPIST
..
.. ROUTINE FOR SPECIAL INSTRUCTION PRINT
..
.SPCLNSTO MATCH     "00" TO SPCL
.         GOTO      PRTSPL0O IF EQUAL
.         MATCH     "99",SPCL
.         GOTO      PRTSPL9O IF EQUAL
.         MATCH     "98" TO SPCL
.         GOTO      PRTSPL98 IF EQUAL
.PRTSPCLO BRANCH    V1 OF W1STO,W2NDO,W3RDO,W4THO,W5THO,W6THO
.W1STO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*58,TEST,*N,*2,WK247;
.         RETURN
.W2NDO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*58,CONT1,*N,*2,WK247;
.         RETURN
.W3RDO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*58,CONT,*N,*2,WK247;
.         PRINT     *56,OLRNCO," ",CONTDTE," ",CONTQTY;
.         RETURN
.W4THO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*58,ENTIRE,*N,*2,WK247;
.         RETURN
.W5THO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*N,*2,WK247;
.         RETURN
.W6THO    CALL      LUKUP1
.         PRINT     *N,*2,WORK47,*N,*2,WK247;
.         RETURN
.PRTSPL0O MOVE      "00",SPCL
.         PRINT     *N,*2,DESC0L1;
.         COMPARE   "1" TO V1
.         CALL      TST IF EQUAL
.         COMPARE   "2" TO V1
.         CALL      CNT1 IF EQUAL
.         COMPARE   "3" TO V1
.         CALL      CONTA IF EQUAL
.         COMPARE   "4" TO V1
.         CALL      ENT IF EQUAL
.         PRINT     *N,*2,DESC0L2;
..         BRANCH    V1 OF RETURN,RETURN,CNT,RETURN,RETURN,RETURN
.         COMPARE   "3" TO V1
.         CALL      CNT IF EQUAL
.         RETURN
..
.PRTSPL9O MOVE      "99",SPCL
.         PRINT     *N,*2,DESC991;
.         COMPARE   "1" TO V1
.         CALL      TST IF EQUAL
.         COMPARE   "2" TO V1
.         CALL      CNT1 IF EQUAL
.         COMPARE   "3" TO V1
.         CALL      CONTA IF EQUAL
.         COMPARE   "4" TO V1
.         CALL      ENT IF EQUAL
.         PRINT     *N,*2,DESC992;
..         BRANCH    V1 OF RETURN,RETURN,CNT,RETURN,RETURN,RETURN
.         COMPARE   "3" TO V1
.         CALL      CNT IF EQUAL
.         RETURN
..
.PRTSPL98 MOVE      "98",SPCL
.         PRINT     *N,*2,DESC981;
.         COMPARE   "1" TO V1
.         CALL      TST IF EQUAL
.         COMPARE   "2" TO V1
.         CALL      CNT1 IF EQUAL
.         COMPARE   "3" TO V1
.         CALL      CONTA IF EQUAL
.         COMPARE   "4" TO V1
.         CALL      ENT IF EQUAL
.         PRINT     *N,*2,DESC982;
..         BRANCH    V1 OF TEST,CNT1,CNT,ENT,RETURN,RETURN
.         COMPARE   "1" TO V1
.         CALL      TST IF EQUAL
.         COMPARE   "2" TO V1
.         CALL      CNT1 IF EQUAL
.         COMPARE   "3" TO V1
.         CALL      CNT IF EQUAL
.         COMPARE   "4" TO V1
.         CALL      ENT IF EQUAL
.         RETURN
..
.TST      PRINT     *58,TEST;
.         RETURN
.CONTA    PRINT     *58,CONT;
.         RETURN
.CNT      PRINT     *56,OLRNCO," ",CONTDTE," ",CONTQTY;
.         RETURN
.CNT1     PRINT     *58,CONT1;
.         RETURN
.ENT      PRINT     *58,ENTIRE;
.         RETURN
..
.RETURN   RETURN
..REUSE - REUSE OF LR##
.REUSE    PRINT     *27,OREUSE;
.         RETURN
..
.LUKUP1   CLEAR     WORK47
.         CLEAR     WK247
.         MOVE      SPCL TO NSPIFLD
.         REP       " 0" IN NSPIFLD
.         CALL      NSPIKEY
.         IF        OVER
.         CLEAR     WORK47
.         CLEAR     WK247
.         RETURN
.         ENDIF
.         MOVE      INST1 TO WORK47
.         MOVE      INST2 TO WK247
.         RETURN
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
.START PATCH 2.56 - REPLACED LOGIC
.         call      Trim using DESC001
.         if (DESC001 <> "")
.                   scan         "After this",DESC001
.                   if not equal
.                                reset        DESC001
.                                clear        taskname
.                                append       "After this order is fulfilled: ",taskname
.                                append       DESC001,taskname
.                                reset        taskname
.                                move         taskname,DESC001
.                   else
.                                reset        DESC001
.                   endif
.         endif
.         pack      holdstr,DESC001,DESC002
.         call      TRIM using holdstr
.         call      PARSITUP using line1,holdstr
.         call      PARSITUP using line2,holdstr
.         call      PARSITUP using line3,holdstr
.         call      PARSITUP using line4,holdstr
.         call      PARSITUP using line5,holdstr
.         call      PARSITUP using line6,holdstr
.         call      PARSITUP using line7,holdstr
.         call      PARSITUP using line8,holdstr
.         call      PARSITUP using line9,holdstr
.         call      PARSITUP using line10,holdstr
.         call      PARSITUP using line11,holdstr
.         call      PARSITUP using line12,holdstr
.         call      PARSITUP using line13,holdstr
.         call      PARSITUP using line14,holdstr
.         call      PARSITUP using line15,holdstr
.         MOVE      C0 TO V1
.         ADD       C1,V1
.         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
.         MOVE      line2,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line3,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line4,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line5,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line6,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line7,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line8,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line9,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line10,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line11,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
.         MOVE      line12,line1
.         ADD       C1,V1
.         CALL      SPCLNSTO
..Printing up through line12 can be done here.  line13-15 must be done when Contact info is printed.  ASH
.         if (line13 <> "")
.                   MOVE         "13",V1
.         endif
.         if (line14 <> "")
.                   MOVE         "14",V1
.         endif
.         if (line15 <> "")
.                   MOVE         "15",V1
.         endif
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
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
.....Following logic removed as decision was made to print DESC001 (XSTAT) in last two lines....
.         call      PARSITUP using line15,DESC002,C1
         MOVE      C0 TO V1
.START PATCH 2.7 ADDED LOGIC
         MOVE      line1,line
.END PATCH 2.7 ADDED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line2,line1
         MOVE      line2,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line3,line1
         MOVE      line3,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line4,line1
         MOVE      line4,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line5,line1
         MOVE      line5,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line6,line1
         MOVE      line6,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line7,line1
         MOVE      line7,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line8,line1
         MOVE      line8,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line9,line1
         MOVE      line9,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line10,line1
         MOVE      line10,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line11,line1
         MOVE      line11,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line12,line1
         MOVE      line12,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
..Printing up through line12 can be done here.  line13-15 must be done when Contact info is printed.  ASH
.         if (line13 <> "")
.                   MOVE         "13",V1
.         endif
.         if (line14 <> "")
.                   MOVE         "14",V1
.         endif
.....Following logic removed as decision was made to print DESC001 (XSTAT) in last two lines....
.         if (line15 <> "")
.                   MOVE         "15",V1
.         endif
.END PATCH 2.56 - REPLACED LOGIC
.Patch 2.76 Logic Added
.;
          prtpage   Laser;*p=5640:7366,CONT
          prtpage   Laser;*p=5640:7616,OLRNCO,"  ",CONTDTE," ",CONTQTY
          prtpage   Laser;*p=5640:7866,ENTIRE
.;
.Patch 2.76 Logic Added
         GOTO      TYPIST
.
. ROUTINE FOR SPECIAL INSTRUCTION PRINT
.
.START PATCH 2.56 - REPLACED LOGIC
.SPCLNSTO PRINT     *N,*2,line1;
.         if (V1 = C1)
.                   PRINT     *58,TEST;
.         elseif (V1 = C3)
.                   PRINT     *58,CONT1;
.         elseif (V1 = C5)
.                   PRINT     *58,CONT;
.         elseif (V1 = C6)
.                   PRINT     *56,OLRNCO," ",CONTDTE," ",CONTQTY;
.         elseif (V1 = C7)
.                   PRINT     *58,ENTIRE;
.         endif
.         return
.START PATCH 2.7 REPLACED LOGIC
.SPCLNSTO PRINT     *N,hpt0125,dtch10fx,line1;
.Patch 2.76 Comment Out
.SPCLNSTO PRINT     *N,hpt0125,dtch10fx,line;
.END PATCH 2.7 REPLACED LOGIC
.         if (V1 = C1)
.                   PRINT     hpt540,CONT;
.         elseif (V1 = C2)
.                   PRINT     hpt540,hpdtch10,hpuprght,OLRNCO," ",CONTDTE," ",CONTQTY;
.         elseif (V1 = C4)
.                   PRINT     hpt540,ENTIRE;
.         endif
.         return
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
SPCLNSTO
          add       "188",result
          prtpage   Laser;*p=125:result,*font=fontO10n,line,*font=fontO10
          return
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
.REUSE - REUSE OF LR##
.START PATCH 2.56 - REPLACED LOGIC
.REUSE    PRINT     *27,OREUSE;
.         RETURN
REUSE
.Patch 2.76 Not Sure
.                             PRINT     *C,hpt225,OREUSE;
.Patch 2.76 Not Sure
         RETURN
.END PATCH 2.56 - REPLACED LOGIC
.END PATCH 2.5 - REPLACED LOGIC
.
.
TYPIST
.START PATCH 2.53 REPLACED LOGIC
.         MOVE      "0" TO NFIELD23
.         MOVE      OCOCODE TO NFIELD23
.         TYPE      OCOCODE
.         GOTO      CON10 IF NOT EQUAL
.         MOVE      OCOCODE TO NFIELD23
.         GOTO      DISCON2A
.CON10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OCOCODE
.         TYPE      OCOCODE
.         GOTO      CON20 IF NOT EQUAL
.         MOVE      OCOCODE TO NFIELD23
.         ADD       "10" TO NFIELD23
.         GOTO      DISCON2A
.CON20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OCOCODE
.         TYPE      OCOCODE
.         GOTO      CON30 IF NOT EQUAL
.         MOVE      OCOCODE TO NFIELD23
.         ADD       "20" TO NFIELD23
.         GOTO      DISCON2A
.CON30    REP       "U0V1X2Y3Z4" IN OCOCODE
.         MOVE      OCOCODE TO NFIELD23
.         MOVE      OCNT0 TO cnt
.DISCON2A
.         MOVE      OCNT0 TO cnt
.         clear     cntphone
..         clear     fullcnt
.         clear     cnt
..START PATCH 2.52 - REPLACED LOGIC
..         LOAD      FULLCNT FROM NFIELD23 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
..                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15
.         LOAD      FULLCNT FROM NFIELD23 OF OCNT1,OCNT2,OCNT3,OCNT4,OCNT5,OCNT6:
.                   OCNT7,OCNT8,OCNT9,OCNT10,OCNT11,OCNT12,OCNT13,OCNT14,OCNT15,OCNT16,OCNT17
..END PATCH 2.52 - REPLACED LOGIC
.         COMPARE   "18" TO NFIELD23
.         IF        NOT LESS
.         MOVE      "NOTHING" TO CNT
.         MOVE      "(415)989-3350" TO CNTPHONE
.         MOVE      "NAMES@NINCAL.COM" TO INTRNET
.         GOTO      CNTEXIT
.         ENDIF
.         MOVEFPTR  fullcnt TO BEGIN
.         SCAN      "(" IN fullcnt
.         If        equal
.         MOVEFPTR  fullcnt TO LAST
.         APPEND    fullcnt TO cntphone
..         SUB       C1 FROM LAST
.         RESET     fullcnt
.         scan      "()" in cntphone
.         if        equal
.         clear     cntphone
.         endif
.         SUB       C1 FROM LAST
.         RESET     fullcnt
.         SETLPTR   fullcnt TO LAST
..         BUMP      fullcnt BY BEGIN
.         APPEND    fullcnt TO cnt
.         endif
.         clear     intrnet
.         reset     cnt
.         scan      "BILLING" in cnt
.         goto      cntexit if equal
.         move      cnt to str1
.cntloopy bump      cnt by 1
.         cmatch    b1 to cnt
.         goto      cntloopy if not equal
.         goto      cntexit if eos
.         bump      cnt by 1
.         move      cnt to str6
.         clear     intrnet
.         pack      intrnet from  str1,str6,"@NINCAL.COM"
.cntexit  reset     cnt
..............
        clear   intrnet
        pack    NCNTFLD,OCOCODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if not over
                scan    "BILLING",CNTNAME
                goto cntexit if equal
.START PATCH 2.61 REPLACED LOGIC
.                move    CNTNAME,str1
.cntloopy
.                bump    CNTNAME,1
.                cmatch  B1,CNTNAME
.                goto    cntloopy if not equal
.                goto    cntexit if eos
.                bump    CNTNAME,1
..START PATCH 2.54 REPLACED LOGIC
..                move    CNTNAME,str6
.                move    CNTNAME,str7
.                call    RemoveChar using str7,B1
.                move    str7,str6
..END PATCH 2.54 REPLACED LOGIC
.                clear   intrnet
.                pack    intrnet,str1,str6,"@NINCAL.COM"
                    move      CNTNAME,str35
                    call      RemoveChar using str35,B1
.START PATCH 2.93 REPLACED LOGIC
.                   pack      intrnet,str35,"@NINCAL.COM"
                    IF        (CntComp = "2")
.                   if (OcompID = "P")
                              pack      intrnet,str35,"@pacificlists.COM"
                    else
                              pack      intrnet,str35,"@NINCAL.COM"
                    endif
.END PATCH 2.93 REPLACED LOGIC
.END PATCH 2.61 REPLACED LOGIC
cntexit         reset   CNTNAME
        endif
.END PATCH 2.53 REPLACED LOGIC
.
.         PRINT     *N,*L,*L,*L,*1,ODOWJ,*65,CNT,*L,*L,*L,*L,*L,*L,*L
.START PATCH 2.5 - REPLACED LOGIC
.         match     "06" to salenumb
.         GOTO      REG IF EQUAL
.         MATCH     YES TO MEDTYPE
.         IF        EQUAL
.         PRINT     *N:
.                   *N,*2,"Mailer will not pay for names identified as errors,":
.                   *L,*2,"bad zips, foreign, non-personal, intrafile or ":
.                   *65,CNT:
.                   *L,*2,"family duplicates, or hits to DMA MPS file.":
.                   *N,*L,*L,*L,*2,ODOWJ:
.                   *L,*L,*L
..         PRINT     *FLUSH;
.         GOTO      LAYBIL
.         ENDIF
.REG      PRINT     *N,*L,*L,*L,*1,ODOWJ,*65,CNT,*L,*L,*L,*L,*L,*L,*L
..         PACK      RTNSTRNG FROM ORTNDTEM,ORTNDTED,ORTNDTEY
..         MOVE      RTNSTRNG TO RTNCHEK
.Print newline characters until we reach appropriate space for Contact info
         if (V1 < 15)
                   LOOP
                           ADD          C1,V1
                           UNTIL (V1 >= 15)
                           if (V1 = 13)
.START PATCH 2.7 REPLACED LOGIC
.                                move    line13,line1
                                move    line13,line
.END PATCH 2.7 REPLACED LOGIC
                                CALL    SPCLNSTO
                           elseif (V1 = 14)
.START PATCH 2.7 REPLACED LOGIC
.                                move    line14,line1
                                move    line14,line
.END PATCH 2.7 REPLACED LOGIC
                                CALL    SPCLNSTO
.Patch 2.76 Comment Out
.                                PRINT   hpt6125,CNTNAME;
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                                                                                                              prtpage   Laser;*p=5625:9500,CNTNAME
.Patch 2.76 Logic Added
                           endif
                   REPEAT
         endif
.START PATCH 2.53 REPLACED LOGIC
.         PRINT        *65,CNT;
.END PATCH 2.53 REPLACED LOGIC
.begin patch 2.95
          If        (SaleNumb = "06" or SaleNumb = "27" or SaleNumb = "28")
.         match     "06" to salenumb
.         GOTO      REG IF EQUAL
          GOto      Reg
          endif
.end patch 2.95
         MATCH     YES TO MEDTYPE
         IF        EQUAL
.START PATCH 2.56 REPLACED LOGIC
.         PRINT     *N,*2,"Mailer will not pay for names identified as errors,":
.                   *L,*2,"bad zips, foreign, non-personal, intrafile or ":
.                   *L,*2,"family duplicates, or hits to DMA MPS file.":
.                   *N,*L,*2,ODOWJ:
.                   *L,*L,*L
.Patch 2.76 Comment Out
.         PRINT     *L,hpt0125,"Mailer will not pay for names identified as errors,":
.                   *L,hpt0125,"bad zips, foreign, non-personal, intrafile or ":
.                   *L,hpt0125,"family duplicates, or hits to DMA MPS file.":
.                   "     ",ODOWJ,*F
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                                                                      prtpage   Laser;*p=125:10312,"Mailer will not pay for names identified as errors,"
                                                                      prtpage   Laser;*p=125:10312,"bad zips, foreign, non-personal, intrafile or "
                                                                      prtpage   Laser;*p=125:10312,"family duplicates, or hits to DMA MPS file."
.patch 2.76 Logic Added
.END PATCH 2.56 REPLACED LOGIC
.         PRINT     *FLUSH;
         GOTO      LAYBIL
         ENDIF
.START PATCH 2.56 - REPLACED LOGIC
.REG      PRINT     *N,*L,*L,*L,*L,*1,ODOWJ,*L,*L,*L
REG
.Patch 2.76 Comment Out
.                             PRINT     *N,*L,*L,hpt0125,ODOWJ,*F
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              prtpage   Laser;*p=125:10400,ODOWJ
.Patch 2.76 Logic Added
.END PATCH 2.56 - REPLACED LOGIC
.         PACK      RTNSTRNG FROM ORTNDTEM,ORTNDTED,ORTNDTEY
.         MOVE      RTNSTRNG TO RTNCHEK
.END PATCH 2.5 - REPLACED LOGIC
LAYBIL   MOVE      "             ",DESC
         CMATCH    "Y" TO TDMCSW
         GOTO      writelol if not equal
         call      mergy
         move      c0 to loldes
         move      loltype to loldes
         branch    loldes of qtych,lstch,canc,rol,reglol
         move      b3,desc2
         goto      writelol
qtych    move      "QTY",desc2
         goto      writelol
lstch    move      "LDC",desc2
         goto      writelol
canc     move      "CAN",desc2
         goto      writelol
ROL      move      "ROL",desc2
         goto      writelol
reglol   move      b3,desc2
writelol
         clear     compm
         move      mcomp to compm
.START PATCH 2.74A REPLACED LOGIC
.         WRITE     NAMFILE,SEQ;OLRN,"  ",COMPM,"  ",O1DES," ",O2DES:
.                   "   ",QTYOUT2,"     ",DESC,"     ",F3,".",F2,desc2
          UNPACK    NSEL2PRICE,str5,str3
          call      FormatNumeric using str5,str6
.START PATCH 2.81 REPLACED LOGIC
.         WRITE     NAMFILE,SEQ;OLRN,"  ",COMPM,"  ",O1DES," ",NSEL2NAME:
.                   "   ",QTYOUT2,"     ",DESC,"     ",str6,str3,desc2
          pack      str9,str6,str3
         WRITE     NAMFILE,SEQ;OLRN,"  ",COMPM,"  ",O1DES," ",NSEL2NAME:
                   "   ",QTYOUT2,"     ",DESC,"     ",str9,desc2
.END PATCH 2.81 REPLACED LOGIC
.END PATCH 2.74A REPLACED LOGIC
.Start Patch #2.3 - added century
.         WRITE     WEEKFILE,SEQ;OMLRNUM,"  ",COMPM,"  ",O1DES:
.                   "   ",QTYOUT,"     ",OMDTEM,SLASH,OMDTED,SLASH,OMDTEY:
.                   "     ",OLRN
         WRITE     WEEKFILE,SEQ;OMLRNUM,"  ",COMPM,"  ",O1DES:
                   "   ",QTYOUT,"     ",OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
                   "     ",OLRN
.END Patch #2.3 - added century
.START PATCH 2.6 REPLACED LOGIC
.         WRITE     SAVEFILE,SEQEOF;OLRN,tdmcstat
.Patch 2.76 For Testing
         WRITE     SAVEFILE,OLRN;OLRN,tdmcstat
.Patch 2.76 For Testing
.Patch 2.96
         WRITE     SENDFILE,seq;OLRN,tdmcstat
.Patch 2.96
.END PATCH 2.6 REPLACED LOGIC
         move      b3,desc2
.begin patch 2.7
.write xml file
          if (tdmcstat =  "0")
.START PATCH 2.72 REPLACED LOGIC
.               write          Xmlfile,seq;"<lforder>"
.               write          Xmlfile,seq;"<lrnum>",OLRN,"</lrnum>"
.               write          Xmlfile,seq;"<orderdate>",OODTEC,OODTEy,OODTEm,OODTEd,"</orderdate>"
.;begin patch 2.71
.               clear          str100
.               Move           Mcomp to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<mailername>",Mcomp,"</mailername>"
.               write          Xmlfile,seq;"<mailername>",Str100,"</mailername>"
.               clear          str100
.               Move           o1des to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<listname>",o1des,"</listname>"
.               write          Xmlfile,seq;"<listname>",str100,"</listname>"
.               clear          str100
.               Move           o2des to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<selection>",o2des,"</selection>"
.               write          Xmlfile,seq;"<selection>",str100,"</selection>"
.               write          Xmlfile,seq;"<qty>",oqty,"</qty>"
.               clear          str100
.               Move           Omlrky to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<keycode>",OMLRKY,"</keycode>"
.               write          Xmlfile,seq;"<keycode>",str100,"</keycode>"
.               if             (Media = "" or Media = " ")
.               move           "******" to Media
.               endif
.               clear          str100
.               Move           media to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<media>",media,"</media>"
.               write          Xmlfile,seq;"<media>",str100,"</media>"
.;end patch 2.71
.               if             (ortnnum = "0001")
.               write          Xmlfile,seq;"<shipcompany>","Reuse Order!!!","</shipcompany>"
.               else
.;begin patch 2.71
.               clear          str100
.               Move           Rtcntct to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<shipcontact>",rtcntct,"</shipcontact>"
.               write          Xmlfile,seq;"<shipcontact>",str100,"</shipcontact>"
.               clear          str100
.               Move           Rtcomp to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<shipcompany>",rtcomp,"</shipcompany>"
.               write          Xmlfile,seq;"<shipcompany>",Str100,"</shipcompany>"
.               clear          str100
.               Move           Rtaddr to str100
.               call           PreReplacit
.;               write          Xmlfile,seq;"<shipaddr>",rtaddr,"</shipaddr>"
.               write          Xmlfile,seq;"<shipaddr>",Str100,"</shipaddr>"
.;end patch 2.71
.               write          Xmlfile,seq;"<shipcity>",rtcity,"</shipcity>"
.               write          Xmlfile,seq;"<shipstate>",rtstate,"</shipstate>"
.               write          Xmlfile,seq;"<shipzip>",rtzip,"</shipzip>"
.               endif
.               write          Xmlfile,seq;"<maildate>",OmDTEC,OMDTEy,OMDTEm,OMDTEd,"</maildate>"
.               write          Xmlfile,seq;"<shipdate>",ORTNDTEC,ORTNDTEy,ORTNDTEm,ORTNDTEd,"</shipdate>"
.               write          Xmlfile,seq;"<shipvia>",SHIPdesc,"</shipvia>"
.               move           c0 to nfield52
.               move           oppm to nfield52
.               mult           ".01" by nfield52
.               write          Xmlfile,seq;"<oppm>",nfield52,"</oppm>"
.               if             (Exchange = "EXCHANGE")
.               write          Xmlfile,seq;"<exchange>",Exchange,"</exchange>"
.               endif
.;need to replace  special characters
.;                                                goto           happy
.;begin patch 2.71
.               write          Xmlfile,seq;"<specialinstructions>"
.;               MOVe           c1 to InstructionCounter
.               FOr            InstructionCOunter from "1" to "14"
.               clear          str100
.               Load           str100 from InstructionCounter of Line1,Line2,Line3,Line4,Line5,Line6,Line7:
.                              Line8,line9,line10,line11,line12,line13,line14
.               iF             (str100 <> "")
.               call           PreReplacit
.;               Move           "&" to str1
.;               move           "&amp;" to str6                        ;must be 1st one
.;               call           ReplaceIt Using str100,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using str100,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using str100,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using str100,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using str100,str1,str6
.               ENDIF
.               write          Xmlfile,seq;str100
.;               add            c1 to InstructionCOunter
.               REpeat
.
.;               iF             (lINE1 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6                        ;must be 1st one
.;               call           ReplaceIt Using Line1,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line1,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line1,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line1,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line1,str1,str6
.;               ENDIF
.;               iF             (lINE2 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line2,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line2,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line2,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line2,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line2,str1,str6
.;               ENDIF
.;               iF             (lINE3 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line3,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line3,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line3,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line3,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line3,str1,str6
.;               ENDIF
.;               iF             (lINE4 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line4,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line4,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line4,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line4,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line4,str1,str6
.;               ENDIF
.;               iF             (lINE5 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line5,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line5,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line5,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line5,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line5,str1,str6
.;               ENDIF
.;               iF             (lINE6 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line6,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line6,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line6,str1,str6
.;               Move           "#"" to str1
.;              move           "&quot;" to str6
.;              call           ReplaceIt Using Line6,str1,str6
.;              Move           "'" to str1
.;              move           "&apos;" to str6
.;               call           ReplaceIt Using Line6,str1,str6
.;               ENDIF
.;               iF             (lINE7 <> "")
.;              Move           "&" to str1
.;              move           "&amp;" to str6
.;;              call           ReplaceIt Using Line7,str1,str6
.;              Move           "<" to str1
.;              move           "&lt;" to str6
.;               call           ReplaceIt Using Line7,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line7,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line7,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line7,str1,str6
.;               ENDIF
.;               iF             (lINE8 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line8,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line8,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line8,str1,str6
.;              Move           "#"" to str1
.;              move           "&quot;" to str6
.;              call           ReplaceIt Using Line8,str1,str6
.;              Move           "'" to str1
.;;              move           "&apos;" to str6
.;              call           ReplaceIt Using Line8,str1,str6
.;              ENDIF
.;              iF             (lINE9 <> "")
.;              Move           "&" to str1
.;              move           "&amp;" to str6
.;              call           ReplaceIt Using Line9,str1,str6
.;              Move           "<" to str1
.;             move           "&lt;" to str6
.;              call           ReplaceIt Using Line9,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line9,str1,str6
.;              Move           "#"" to str1
.;              move           "&quot;" to str6
.;               call           ReplaceIt Using Line9,str1,str6
.;               Move           "'" to str1
.;;              move           "&apos;" to str6
.;               call           ReplaceIt Using Line9,str1,str6
.;               ENDIF
.;               iF             (lINE10 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line10,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line10,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line10,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line10,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line10,str1,str6
.;               ENDIF
.;               iF             (lINE11 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line11,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line11,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line11,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line11,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line11,str1,str6
.;               ENDIF
.;               iF             (lINE12 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line12,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line12,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line12,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line12,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line12,str1,str6
.;               ENDIF
.;               iF             (lINE13 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line13,str1,str6
.;               Move           "<" to str1
.;               move           "&lt;" to str6
.;               call           ReplaceIt Using Line13,str1,str6
.;               Move           ">" to str1
.;               move           "&gt;" to str6
.;               call           ReplaceIt Using Line13,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line13,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line13,str1,str6
.;               ENDIF
.;               iF             (lINE14 <> "")
.;               Move           "&" to str1
.;               move           "&amp;" to str6
.;               call           ReplaceIt Using Line14,str1,str6
.;              Move           "<" to str1
.;              move           "&lt;" to str6
.;;              call           ReplaceIt Using Line14,str1,str6
.;              Move           ">" to str1
.;              move           "&gt;" to str6
.;              call           ReplaceIt Using Line14,str1,str6
.;               Move           "#"" to str1
.;               move           "&quot;" to str6
.;               call           ReplaceIt Using Line14,str1,str6
.;               Move           "'" to str1
.;               move           "&apos;" to str6
.;               call           ReplaceIt Using Line14,str1,str6
.;               ENDIF
.happy
.;               write          Xmlfile,seq;"<specialinstructions>",Line1
.;               write          Xmlfile,seq;Line2
.;               write          Xmlfile,seq;Line3
.;               write          Xmlfile,seq;Line4
.;               write          Xmlfile,seq;Line5
.;               write          Xmlfile,seq;Line6
.;               write          Xmlfile,seq;Line7
.;               write          Xmlfile,seq;Line8
.;               write          Xmlfile,seq;Line9
.;               write          Xmlfile,seq;Line10
.;               write          Xmlfile,seq;Line11
.;               write          Xmlfile,seq;Line12
.;               write          Xmlfile,seq;Line13
.;               write          Xmlfile,seq;Line14
.;end patch 2.71
.               write          Xmlfile,seq;"</specialinstructions>"
.;
.               IF             (occode = "2")         continuation omit
.               write          Xmlfile,seq;"<opu>"
.               write          Xmlfile,seq;"<opulr>",Olrnco,"</opulr>"
.               write          Xmlfile,seq;"<opudate>",oodtecoc,oodtecoy,oodtecom,oodtecod,"</opudate>"
.               write          Xmlfile,seq;"<opuqty>",oqtyco,"</opuqty>"
.               write          Xmlfile,seq;"</opu>"
.               endif
.               write          Xmlfile,seq;"<contact>",CNTNAME,"</contact>"
.               if             (OELCODE = "3")
.               write          Xmlfile,seq;"<entirelist>","TRUE"
.               write          Xmlfile,seq;"</entirelist>"
.               endif
.               write          Xmlfile,seq;"</lforder>"
.
.Patch 2.76 For Testing
                    call      LoadXMLFile
.Patch 2.76 For Testing
.END PATCH 2.72 REPALCED LOGIC
               endif

.end patch 2.7
.Patch 2.76 Comment Out       - will be done on second pass
.         GOTO      PRNTLABL
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                              call                LabelListViewAdd
         GOTO      READO
.Patch 2.76

.begin patch 2.71
PreReplacit
               Move           "&" to str1
               move           "&amp;" to str6                        ;must be 1st one
               call           ReplaceIt Using str100,str1,str6
               Move           "<" to str1
               move           "&lt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           ">" to str1
               move           "&gt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "#"" to str1
               move           "&quot;" to str6    ."
               call           ReplaceIt Using str100,str1,str6
               Move           "'" to str1
               move           "&apos;" to str6
               call           ReplaceIt Using str100,str1,str6
               Return
.end patch 2.71
.START PATCH 2.72 ADDED LOGIC - MOVED INTO SELF-CONTAINED ROUTINES
ExternalXMLCreate Routine DimPtr
.Used by outside programs
          move      DimPtr,NORDFLD
          move      C1,NORDPATH
          rep       ZFILL,NORDFLD
          call      NORDKEY
          if not over
.START PATCH 2.84 ADDED LOGIC - SPECIAL FOR INFOUSA
                    call      Trim using OMLRKY
                    if (OMLRKY <> "")
                              count     result,OMLRKY
                              if (result < 7)
                                        type      OMLRKY
                                        if equal
                                                  move      OMLRKY,str7
                                                  call      ZFillIt using str7
                                                  move      str7,OMLRKY
                                        endif
                              endif
                    endif
.END PATCH 2.84 ADDED LOGIC - SPECIAL FOR INFOUSA
                    pack      MKEY,OMLRNUM,OCOBN
                    move      "XML-NMLRKEY",Location
                    pack      KeyLocation,"Key: ",MKEY
                    call      NMLRKEY
                    move      OLRN,NSPEFLD
                    move      "XML-NSPEKEY",Location
                    pack      KeyLocation,"Key: ",NSPEFLD
                    call      NSPEKEY
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
                    move      ORTNNUM,NRTNFLD
                    move      "XML-NRTNKEY",Location
                    pack      KeyLocation,"Key: ",NRTNFLD
                    call      NRTNKEY
.
                    clear     MEDIA
                    cmatch    " ",OFOCODE
                    goto      MEDIAEXXML IF EQUAL           *NO MEDIA SELECT
                    goto      MEDIAEXXML IF EOS             * NO MEDIA SELECT
                    move      C0,NFIELD23
                    type      OFOCODE
                    goto      MED10XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    goto      DIS27XML
MED10XML
                    rep       "A0B1C2D3E4F5G6H7I8J9",OFOCODE
                    type      OFOCODE
                    goto      MED20XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    add       C10,NFIELD23
                    goto      DIS27XML
MED20XML
                    rep       "K0L1M2N3O4P5Q6R7S8T9",OFOCODE
                    type      OFOCODE
                    goto      MED30XML IF NOT EQUAL
                    move      OFOCODE,NFIELD23
                    add       "20",NFIELD23
                    goto      DIS27XML
MED30XML
                    rep       "U0V1X2Y3Z4",OFOCODE
                    move      OFOCODE,NFIELD23
                    add       "30",NFIELD23
DIS27XML
                    move      MED0,MEDIA
                    load      MEDIA,NFIELD23,MED1,MED2,MED3,MED4,MED5:
                              MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                              MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                              MED23,MED24,MED25,MED26,MED27,MED28,MED29
MEDIAEXXML
                    clear     SHIPDESC
                    call      Trim using OSHP
                    if (OSHP <> "")
                              move      C0,nfield23
                              move      OSHP,NFIELD23
                              move      SHIP0,SHIPDESC
                              load      SHIPDESC FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                                        SHIP6,SHIP7,SHIP8,SHIP9
                    endif
                    clear     EXCHANGE
                    move      C0,NFIELD23
                    move      OELCODE,NFIELD23
                    if (NFIELD23 = 2 | NFIELD23 = 3)
                              move      "EXCHANGE",EXCHANGE
                    endif
                    pack      NCNTFLD,OCOCODE
                    move      "XML-NCNTKEY",Location
                    pack      KeyLocation,"Key: ",NCNTFLD
                    call      NCNTKEY
.START PATCH 2.75 ADDED LOGIC
                    create    PackData=1:1:1:1
.END PATCH 2.75 ADDED LOGIC
.Patch 2.76 For Testing
                    call      CreateXMLFile
                    call      LoadXMLFile
                    call      CloseXMLFile
.Patch 2.76 For Testing
.START PATCH 2.75 ADDED LOGIC
                    destroy   PackData
.END PATCH 2.75 ADDED LOGIC
          endif
          return
CreateXMLFile
          clock     timestamp,timestamp
          clear     str45
          unpack    timestamp,str8,str6
.begin patch 2.99
.          clear     str45
           clear      taskname2
          pack      taskname2,"\\nins1\e\storage\export\009406\orders\NIN.",str8,".t",str6,".PreTouch"
.          pack      str45,"\\nins1\e\data\NIN.",str8,".t",str6,".PreTouch"
..         pack      str45,"c:\work\NIN.",str8,".t",str6,".PreTouch"
.          prepare   XMLFile,str45
          prepare   XMLFile,taskname2
          write     Xmlfile,seq;"<nin>"
          weof      xmlfile,seq
          close     xmlfile
          clear     taskname2
          pack      taskname2,"\\nins1\e\storage\export\009406\orders\NIN.",str8,".t",str6,".xml"
.          pack      str45,"\\nins1\e\data\NIN.",str8,".t",str6,".xml"
..         pack      str45,"c:\work\NIN.",str8,".t",str6,".xml"
          clear     XmLFIleName
          pack      XmlFileName,"\\nins1\e\storage\export\009406\orders\NIN.",str8,".t",str6,"."
.          pack      XmlFileName,"\\nins1\e\data\NIN.",str8,".t",str6,"."
.         pack      XmlFileName,"c:\work\NIN.",str8,".t",str6,"."
.          prepare   XMLFile,str45
          prepare   XMLFile,taskname2
.end patch 2.99
          write     Xmlfile,seq;"<nin>"
          return

LoadXMLFile
          write     Xmlfile,seq;"<lforder>"
          write     Xmlfile,seq;"<lrnum>",OLRN,"</lrnum>"
          write     Xmlfile,seq;"<orderdate>",OODTEC,OODTEY,OODTEM,OODTED,"</orderdate>"
          clear     str100
          move      MCOMP,str100
          call      PreReplacit
          write     Xmlfile,seq;"<mailername>",str100,"</mailername>"
          clear     str100
          move      O1DES,str100
          call      PreReplacit
          write     Xmlfile,seq;"<listname>",str100,"</listname>"
          clear     str100
.START PATCH 2.74A REPLACED LOGIC
.         move      O2DES,str100
          move      NSEL2NAME,str100
.END PATCH 2.74A REPLACED LOGIC
          call      PreReplacit
          write     Xmlfile,seq;"<selection>",str100,"</selection>"
          write     Xmlfile,seq;"<qty>",OQTY,"</qty>"
          clear     str100
          move      OMLRKY,str100
          call      PreReplacit
          write     Xmlfile,seq;"<keycode>",str100,"</keycode>"
          if (MEDIA = "" | MEDIA = " ")
                    move      "******",MEDIA
          endif
          clear     str100
          move      MEDIA,str100
          call      PreReplacit
          write     Xmlfile,seq;"<media>",str100,"</media>"
          if (ORTNNUM = "0001")
                    write     Xmlfile,seq;"<shipcompany>","Reuse Order!!!","</shipcompany>"
          else
                    clear     str100
                    move      RTCNTCT,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipcontact>",str100,"</shipcontact>"
                    clear     str100
                    move      RTCOMP,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipcompany>",str100,"</shipcompany>"
                    clear     str100
                    move      RTADDR,str100
                    call      PreReplacit
                    write     Xmlfile,seq;"<shipaddr>",str100,"</shipaddr>"
.START PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!
.                   write     Xmlfile,seq;"<shipcity>",RTCITY,"</shipcity>"
                       if (RTNUM = "5318")
                              write     Xmlfile,seq;"<shipcity>incoming.files@donnelley.infogroup.com</shipcity>"
                    else
                              write     Xmlfile,seq;"<shipcity>",RTCITY,"</shipcity>"
                    endif
.END PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
                    write     Xmlfile,seq;"<shipstate>",RTSTATE,"</shipstate>"
                    write     Xmlfile,seq;"<shipzip>",RTZIP,"</shipzip>"
          endif
          write     Xmlfile,seq;"<maildate>",OMDTEC,OMDTEY,OMDTEM,OMDTED,"</maildate>"
          write     Xmlfile,seq;"<shipdate>",ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,"</shipdate>"
          write     Xmlfile,seq;"<shipvia>",SHIPDESC,"</shipvia>"
.START PATCH 2.74A REPLACED LOGIC
.         move      C0,nfield52
.         move      OPPM,nfield52
.         mult      ".01",nfield52
.         write     Xmlfile,seq;"<oppm>",nfield52,"</oppm>"
          write     Xmlfile,seq;"<oppm>",NSEL2PRICE,"</oppm>"
          write     Xmlfile,seq;"<selprice>",NSEL2SPRICE,"</selprice>"
.END PATCH 2.74A REPLACED LOGIC
.START PATCH 2.89 REPLACED LOGIC
.         if (Exchange = "EXCHANGE")
          if (OELCODE = "2" | OELCODE = "3")
.END PATCH 2.89 REPLACED LOGIC
                    write     Xmlfile,seq;"<exchange>","EXCHANGE","</exchange>"
          endif
.begin code 2.991
           call       trim using OMlrLstCd
           if         (OMlrLStCd <> "")
                    write     Xmlfile,seq;"<MlrListCode>",OMlrLstCd,"</MlrListCode>"
          endif          
.end code 2.991
.START PATCH 2.74A ADDED LOGIC
          write     Xmlfile,seq;"<specialselections>"
          PackData.GetCount giving N10
          if (N10 > C0)
                    move      C0,N1
                    for result,"1",N10
                              getitem   PackData,result,NREFDESC
                              move      NREFDESC,str100
                              if (str100 <> "")
                                        call      PreReplacit
                              endif
                              write     Xmlfile,seq;str100
                    repeat
          endif
          write     Xmlfile,seq;"</specialselections>"
.END PATCH 2.74A ADDED LOGIC
          write     Xmlfile,seq;"<specialinstructions>"
          for InstructionCOunter,"1","14"
                    clear     str100
                    load      str100,InstructionCounter,Line1,Line2,Line3,Line4,Line5,Line6,Line7:
                              Line8,line9,line10,line11,line12,line13,line14
                    if (str100 <> "")
                              call      PreReplacit
                    endif
                    write     Xmlfile,seq;str100
          repeat
          write     Xmlfile,seq;"</specialinstructions>"
.         if (occode = "2")         continuation omit
.start patch 2.73
          if (occode = "1")         continuation omit
.end  patch 2.73
                    write     Xmlfile,seq;"<opu>"
                    write     Xmlfile,seq;"<opulr>",OLRNCO,"</opulr>"
                    write     Xmlfile,seq;"<opudate>",OODTECOC,OODTECOY,OODTECOM,OODTECOD,"</opudate>"
                    write     Xmlfile,seq;"<opuqty>",OQTYCO,"</opuqty>"
                    write     Xmlfile,seq;"</opu>"
          endif
          write     Xmlfile,seq;"<contact>",CNTNAME,"</contact>"
.Patch 2.80 Logic Added
.         if (OELCODE = "3")
          if (OELCODE = "1" | OELCODE = "3")
.Patch 2.80 Logic Added
                    write     Xmlfile,seq;"<entirelist>","TRUE"
                    write     Xmlfile,seq;"</entirelist>"
          endif
          write     Xmlfile,seq;"</lforder>"
          return

CloseXMLFile
          write     Xmlfile,seq;"</nin>"
          weof      XMLFile,seq
          close     XMLfile
.Begin patch 2.99
          clear     taskname1
          pack      taskname1,XMLFileName,"PreTouch"
          clear     taskname2
          pack      taskname2,XMLFileName,"Touch"
          rename    taskname1,taskname2
.          clear     str55
.          pack      str55,XMLFileName,"PreTouch"
.          clear     str45
.          pack      str45,XMLFileName,"Touch"
.          rename    str55,str45
.end patch 2.99
          return
.END PATCH 2.72 ADDED LOGIC - MOVED INTO SELF-CONTAINED ROUTINES

MERGY
.begin patch 2.95
          If        (SaleNumb = "06" or SaleNumb = "27" or SaleNumb = "28")

.         MATCH     "06",salenumb
.         IF        EQUAL
.end patch 2.95
         MOVE      "MERGE       ",DESC
         else
         MOVE      OMLRKY,DESC
         endif
         RETURN
.         GOTO      READO
PRNTLABL
.Patch 2.76 Logic Added
          Labellistview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          return    if less
.
   clear   taskname
          Path      Exist,"c:\windows"
          if over                       .nt/2000
                    append    "!c:\winnt\system32\cmd.exe",taskname
          elseif (osflag = c6)          .XP
                    append    "!c:\windows\system32\cmd.exe",taskname
          else                          .95/98
                    append    "!c:\command.com",taskname
          endif
        append  " /c del ",taskname
.START PATCH 2.79 REPLACED LOGIC
.         append    NTWKPATH4,taskname
.         append    "fax\faxfile.prn",taskname
          append    "C:\WORK\faxfile.prn",taskname
.END PATCH 2.79 REPLACED LOGIC
        reset   taskname
        execute taskname
.


   PRTOPEN   LABEL,"FAXFILE","FAXFILE.PRN"
   prtpage   LABEL;*UNITS=*HIENGLISH;
          for num9,"0",#result
                    call GetListViewLabels
.Patch 2.76 Logic Added
                              MATCH     "00" TO ORTNDTEM      *BLANK RETURN DATE (MONTH>?
.Patch 2.76 Logic Added
         GOTO      NextLbl IF EQUAL        *YES, DO NOT PRINT LABEL.
.Patch 2.76 Logic Added
.Patch 2.76 Comment Out
.         GOTO      READO IF EQUAL        *YES, DO NOT PRINT LABEL.
.Patch 2.76 Comment Out
         CMATCH    "Y" TO TDMCSW
.Patch 2.76 Logic Added
         GOTO      NextLbl IF EQUAL        *YES, DO NOT PRINT LABEL.
.Patch 2.76 Logic Added
.Patch 2.76 Comment Out
.         GOTO      READO IF EQUAL
.Patch 2.76 Comment Out
.START PATCH 2.63 ADDED LOGIC
.MED25    INIT      "Email @ $50"
.MED26    INIT      "Email"
.MED28    INIT      "FTP"
          if (OFOCODE = "25" | OFOCODE = "26" | OFOCODE = "28")
.Patch 2.76 Comment Out
.                   goto READO
.Patch 2.76 Comment Out
.Patch 2.76 Logic Added
                    goto NextLbl
.Patch 2.76 Logic Added
          endif
.         SWITCH    OFOCODE
.                   CASE      "25"
.                   GOTO READO
.                   CASE      "26"
.                   GOTO READO
.                   CASE      "28"
.                   GOTO READO
.                   DEFAULT
.         ENDSWITCH
.END PATCH 2.63 ADDED LOGIC
.START PATCH 2.56 REPLACED LOGIC
.         move      rtcomp to str25
Test2
         move      rtcomp to str45
.END PATCH 2.56 REPLACED LOGIC
         MOVE      "P.O. ##-" TO PRTPO
         MATCH     "            " TO OMLRPON
         CALL      NOPO IF EQUAL         *NO MLR P.O.
.START PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
.         PACK      LBOFR FROM OODES
         PACK      LBOFR FROM OFDESC
.END PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
.         GOTO      READO
.START PATCH 2.56 REPLACED LOGIC
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;"       ",O1DES:
.                   "    LR## ",LROUT,"        ",O1DES:
.                   "    LR## ",LROUT
.         WRITE     LABEL,SEQ;"       ",O2DES:
.                   "                      ",O2DES
.         IFNZ      PC
.         FLUSH     LABEL
.         XIF
..         DISPLAY   *P1:23,SEQ
.         WRITE     LABEL,SEQ;"         ",QTYOUT,"                           ":
.                   OMLRKY,"          ",QTYOUT:
.                   "                           ":
.                   OMLRKY
.         WRITE     LABEL,SEQ;"        ",LBOFR:
.                   "                                ":
.                   LBOFR
.         WRITE     LABEL,SEQ;"                   ":
.                   PRTPO,OMLRPON:
.                   "                                                ":
.                   PRTPO,OMLRPON
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;"                              ",dim25b:
.                   "                                 ",dim25b
.         WRITE     LABEL,SEQ;"                          ",CORTN," ",str25:
.                   "                             ",CORTN," ",str25
.          WRITE    LABEL,SEQ;"                              ",RTADDR:
.                   "                                 ",RTADDR
.       WRITE     LABEL,SEQ;"                              ",RTCITY," ",RTSTATE:
.                   "         ":
.                   "                               ",RTCITY," ",RTSTATE
.         WRITE     LABEL,SEQ;" ":
.                   "                                             ",RTZIP:
.                   "                                              ",RTZIP
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
.         WRITE     LABEL,SEQ;" "
................
        if (HowMany = C0)
                move    column1,column
                move    column2,columnA
                move    column3,columnB
                if (row1 >= 9000)
                        move    "900",row1
                        move    "900",row2
                        prtpage LABEL;*NEWPAGE;
                endif
                move    row1,row
        else
                move    column4,column
                move    column5,columnA
                move    column6,columnB
                move    row2,row
        endif
        prtpage LABEL;*pcolumn:row,*font=font2,O1DES;
        add     sixlpi,row
.START PATCH 2.74A REPLACED LOGIC
.        prtpage LABEL;*pcolumn:row,O2DES;
        prtpage LABEL;*pcolumn:row,NSEL2NAME;
.END PATCH 2.74A REPLACED LOGIC
        add     sixlpi,row
        add     "100",column,N9
        prtpage LABEL;*pN9:row,QTYOUT;
        call    Trim using OMLRKY
        if (OMLRKY <> "")
                prtpage LABEL;*pcolumnA:row,"Mlr Key: ",OMLRKY;
        endif
        add     sixlpi,row
        prtpage LABEL;*pcolumn:row,LBOFR;
        add     sixlpi,row
        prtpage LABEL;*pcolumn:row,"LR ##: ",LROUT;
        call    Trim using OMLRPON
        if (OMLRPON <> "")
                prtpage LABEL;*pcolumnA:row,"P.O. ##: ",OMLRPON;
        endif
.
        add     sixlpi,row
        add     sixlpi,row
        prtpage LABEL;*pcolumnB:row,dim45b;
        add     sixlpi,row
        sub     "400",columnB,N9
        prtpage LABEL;*pN9:row,"C/O ";
        prtpage LABEL;*pcolumnB:row,str45;
        add     sixlpi,row
        prtpage LABEL;*pcolumnB:row,RTADDR;
        add     sixlpi,row
        call    Trim using RTCITY
        if (RTCITY <> "")
.START PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!
.                pack    taskname,RTCITY,COMMA,B1,RTSTATE,B1,RTZIP
                    if (RTNUM = "5318")
                              pack      taskname,"incoming.files@donnelley.infogroup.com",COMMA,B1,RTSTATE,B1,B1,RTZIP
                    else
                          pack    taskname,RTCITY,COMMA,B1,RTSTATE,B1,RTZIP
                    endif
.END PATCH 2.82 REPLACED LOGIC - TEMPORARY PATCH - REMOVE ONCE NINRTN IS CONVERTED!!!!
                prtpage LABEL;*pcolumnB:row,taskname;
        else
                prtpage LABEL;*pcolumnB:row,RTSTATE,B1,RTZIP;
        endif
        add     sixlpi,row
        add     sixlpi,row
        add     sixlpi,row
        add     sixlpi,row
        add     sixlpi,row
        add     sixlpi,row
        if (HowMany = C0)
                move    row,row1
                move    C1,HowMany
        else
                move    row,row2
                move    C0,HowMany
        endif
.END PATCH 2.56 REPLACED LOGIC
         move      olrn to lastlabl
.Patch 2.76 Logic Added
NextLbl
                              repeat
                              prtclose label
.START PATCH 2.79 REPLACED LOGIC
.                      pack      APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
                       pack      APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 2.79 REPLACED LOGIC
                       pack      APIFileName2,"\\nins1\e\data\TRIPLEX1.OUT",hexzero
                       call      CopyFile
                              return
.Patch 2.76 Logic Added
.Patch 2.76 Comment Out
.         GOTO      READO
.Patch 2.76 Comment Out

NOPO     MOVE      "       " TO PRTPO
         RETURN
. *****************************************************************************
. CALLED SUBROUTINES.
. ...................
.
. CHNGRET - PRINT MAILER COMPANY AS RETURN TO CONTACT.
CHNGRET
.         mOVE      MCOMP TO RTCNTCT
.START PATCH 2.56 REPLACED LOGIC
.         clear     dim25b
.         move      mcomp  to dim25b
         clear     dim45b
         move      mcomp  to dim45b
.END PATCH 2.56 REPLACED LOGIC
         MOVE      "C/O" TO CORTN
         RETURN
.
. USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
.         MOVE      OODES TO RTCNTCT
.START PATCH 2.56 REPLACED LOGIC
.         clear      dim25b
..START PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
..         move       OODES to dim25b
.         move       OFDESC to dim25b
..END PATCH 2.4 - REPLACED LOGIC, OODES --> OFDESC
         clear      dim45b
         move       OFDESC to dim45b
.END PATCH 2.56 REPLACED LOGIC
         MOVE      "C/O" TO CORTN
         RETURN
.
. CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
.         DISPLAY   *P1:24,*EL,"CONTIN",*B;
         MOVE      "X" TO CONT
.Start Patch #2.3 - added century
.         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOY
         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
.END Patch #2.3 - added century
         MOVE      QTYMSK TO CONTQTY
         MOVE      OQTYCO TO QTYNUM
         EDIT      QTYNUM TO CONTQTY
         RETURN
. CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
.         DISPLAY   *P1:24,*EL,"CONTIN NO OMIT",*B;
         MOVE      "X" TO CONT1
         RETURN
.
. REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
.         MOVE      "*** REPRINT ***" TO REPRT
         RETURN
.
. CANCLLED - CANCELLED ORDER, PRINT AT TOP.
CANCLLED
.         MOVE      "**CANCELLED**" TO REPRT
         RETURN
.
. MEDMEMO - MAG TAPE ORDER, INCLUDE ADDITION INFO.
.MEDMEMO  MOVE      "INCLUDE LAYOUT & DUMP." TO MEDMEMO
.MEDMEMO  MOVE      MED0 TO MEDIA
.         RETURN
.
.
. COMSLCT - COMSELECT ORDER.
COMSLCT  MOVE      "**CC: CONSUMER DIRECT" TO COMSLCT
         RETURN
LIFESTYL MOVE      "CC:LIFESTYLE SELECTOR" TO COMSLCT
         RETURN
ICSYSTEM  MOVE      "**CC: IC SYSTEMS **" TO COMSLCT
         RETURN
NOTDMC   WRITE     BADORD,SEQEOF;OLRN," ",DATE
         DISPLAY   *P1:24,*EL,*HON,"NON-TRIPLEX ORDER",*HOFF,*B,*B,*B,*B,*B;
         GOTO      READO
.
NOORD
         DISPLAY     *W2,*R,*P1:24,*EL,"****SPOOLING STOPED!!!!!!":
                   *P20:24,*R,*HON,*EL,"NO RECORD FOUND FOR LR##",OLRN
        KEYIN     *P78:24,*HOFF,STR1;
        GOTO      READO

FIRSTORD
.START PATCH 2.56 - REPLACED LOGIC
.         COMPARE   C33 TO PRTLINES
.         GOTO      printlr IF EQUAL
.         PRINT     *l;
.         ADD       C1 TO PRTLINES
.         GOTO      firstord
.printlr  print     *20,"***** FIRST LR ## ",olrn," *****"
.         add       c1 to prtlines
.cntline  compare   c66 to prtlines
.         return    if equal
.         PRINT     *l;
.         ADD       C1 TO PRTLINES
.         GOTO      cntline
..............
.Patch 2.76 Not Sure
.         PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*N,*20,"***** FIRST LR ## ",olrn," *****",*F;
.Patch 2.76 Not Sure
         return
.END PATCH 2.56 - REPLACED LOGIC
NOMLR
         MOVE      "NO SUCH MAILER" TO MCOMP
         RETURN
.
.patch2.75
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch2.75
         INCLUDE   NOWNIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NCRCIO.inc
         INCLUDE   NRTNIO.inc
         INCLUDE   NSPIIO.inc
.START PATCH 2.4 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 2.4 - ADDED LOGIC
.START PATCH 2.5 - ADDED LOGIC
         INCLUDE   NSPEIO.INC
.END PATCH 2.5 - ADDED LOGIC
         INCLUDE   COMLOGIC.inc
ABORT    DISPLAY   *P1:24,*HON,*BLINKON,*RED,"JOB ABORTED",*B,*B,*B,*W5,*B
EOJ
.START PATCH 2.56 - REPLACED LOGIC
.         COMPARE   C33 TO PRTLINES
.         GOTO      EOJ2 IF EQUAL
.         PRINT     *l;
.         ADD       C1 TO PRTLINES
.         GOTO      EOJ
.eoj2     print     *20,"***** LAST LR ## ",lastlr,"  *****"
.         print     *20,"***** LAST LABEL LR ## ",lastlabl,"  *****"
.         print     *20,"***** TOTAL ORDERS =",spcount," *****"
.         add       c3 to prtlines
.eoj3     compare   c66 to prtlines
.         goto      eoj4 if equal
.         PRINT     *l;
.         ADD       C1 TO PRTLINES
.         GOTO      EOJ3
.............
.Patch 2.74
.         PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*N,*20,"***** FIRST LR ## ",firstlr," *****"
.Endpatch 2.74
.         print     *20,"***** LAST LR ## ",lastlr,"  *****"
.         print     *20,"***** LAST LABEL LR ## ",lastlabl,"  *****"
.         print     *20,"***** TOTAL ORDERS =",spcount," *****"
.PATCH 2.74
          write EMAILFILE,seq;"FIRST LR ## ",FIRSTLR
          write EMAILFILE,seq;"LAST LR ## ",lastlr
          write EMAILFILE,seq;"LAST Label LR ## ",lastlabl
          write EMAILFILE,seq;"Total Orders = ",spcount
                               close    EMAILFILE
.PATCH2.74
.END PATCH 2.56 - REPLACED LOGIC
eoj4
         IFNZ      PC
         FLUSH     TDMCORD
         FLUSH     BADORD
.START PATCH 2.56 REMMED LOGIC
.         FLUSH     LABEL
.END PATCH 2.56 REMMED LOGIC
         FLUSH     NAMFILE
         XIF
         CLOSE     TDMCORD
         WEOF      BADORD,SEQEOF
         CLOSE     BADORD
.START PATCH 2.6 REPLACED LOGIC
.         WEOF      SAVEFILE,SEQEOF
.END PATCH 2.6 REPLACED LOGIC
         CLOSE     SAVEFILE
.was still active code????? 19 Jan 2012 DLH
.         PRINT     *FLUSH;
.         SPLCLOSE
.was still active code????? 19 Jan 2012 DLH
.Patch 2.76 Logic Added
                              prtclose Laser
                              call      Trim using prtname
.HotOrders should not rename first.  This is for regular run.
                              pack      taskname,"\\nins1\e\data\triplex.out"
                              pack      APIFileName,taskname,hexzero
                              call      FindFirstFile
                              if (APIResult <> 0 & APIResult <> hexeight)
                                        erase     taskname
                              endif
.START PATCH 2.79 REPLACED LOGIC
.         pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
         pack    APIFileName,"C:\WORK\faxfile.prn",hexzero
.END PATCH 2.79 REPLACED LOGIC
         pack    APIFileName2,"\\nins1\e\data\TRIPLEX.OUT",hexzero
         call    CopyFile
.Patch 2.76 Logic Added
         MOVE      "-1" TO SEQ
.START PATCH 2.56 REMMED LOGIC
.         WEOF      LABEL,SEQ
.END PATCH 2.56 REMMED LOGIC
         WEOF      NAMFILE,SEQ
         CLOSE     NAMFILE
.START PATCH 2.56 REPLACED LOGIC
.END PATCH 2.96 
         WEOF      SENDFILE,SEQ
         CLOSE     SENDFILE
.START PATCH 2.96 
.         CLOSE     LABEL,EOFSIZE
.Patch 2.76 Comment Out
.         PRTCLOSE  LABEL
.Patch 2.76 Comment Out
.begin patch 2.7
.START PATCH 2.72 REPLACED LOGIC
.               write          Xmlfile,seq;"</nin>"
.               WEOF           XMLFile,seq
.               Close          XMLfile
.               clear          str55
.               pack           str55 from XMLFileName,"PreTouch"
.               clear          str45
.               pack           str45 from XMLFileName,"Touch"
.               rename         str55,str45
.Patch 2.76 Comment Out
          call      CloseXMLFile
.Patch 2.76 Comment Out
.END PATCH 2.72 REPLACED LOGIC
.end patch 2.7
.DREW
.Patch 2.76 Logic Added
         CALL      PRNTLABL
.Patch 2.76 Logic Added
.Patch 2.76 Comment Out and Moved
.         pack      APIFileName,"F:\data\fax\faxfile.prn",hexzero
.          pack    APIFileName,NTWKPATH4,"fax\faxfile.prn",hexzero
.         pack      APIFileName,"C:\WORK\faxfile.prn",hexzero
.         pack      APIFileName2,"\\nins1\e\data\TRIPLEX1.OUT",hexzero
.         call      CopyFile
.Patch 2.76 Comment Out and Moved to Prntlabl
.END PATCH 2.56 REPLACED LOGIC
         WEOF      WEEKFILE,SEQ
         CLOSE     WEEKFILE
.START PATCH 2.5 - NOW READING FROM IO
.         CLOSE     OUTSP
.END PATCH 2.5 - NOW READING FROM IO
         STOP
IO
         TRAPCLR    IO
         NORETURN
         TRAP      IO IF IO
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE:
                   SIX,SEVEN,EIGHT,NINE,TEN,TWELVE
ZERO
         DISPLAY   *P1:24,"UNDEFINED IO ERROR",*W2;
         GOTO      IOEXIT
ONE
//Patch 2.9 altered code to retrieve the error message end
         DISPLAY   *P1:24,"TDMCORD.OUT FILE ERROR ",error,*W2;
//Patch 2.9 altered code to retrieve the error message end  
         GOTO      IOEXIT
TWO
         DISPLAY   *P1:24,"TRIPLEX1.OUT FILE ERROR",*W2;
         GOTO      IOEXIT
THREE
         DISPLAY   *P1:24,"BADORD FILE ERROR",*W2;
         GOTO      IOEXIT
FOUR
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
FIVE
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SIX
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SEVEN
         GOTO      IOEXIT
EIGHT
         DISPLAY   *P1:24," LABEL SPOOL FILE ERROR",*W2;
         KEYIN     *R,*P1:24,"DO YOU WANT ME TO CREATE FILE ",ANS;
         CMATCH    "Y" TO ANS
         GOTO      PREPLABL IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      EIGHT IF NOT EQUAL
         GOTO      IOEXIT
NINE
         DISPLAY   *P1:24,"BAD ORDER FILE ERROR",*W2;
         GOTO      IOEXIT
TEN      DISPLAY   *P1:24,*EL,"ORDER SPOOL FILE ERROR",*W2
         GOTO      IOEXIT
ELEVEN   DISPLAY   *P1:24,*EL,"ORDER FILE ERROR",*W2
         GOTO      IOEXIT
TWELVE   DISPLAY   *P1:24,*EL,"NINSPEC FILE ERROR",*W2
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P60:24,*B,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOEXIT1 IF EQUAL
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT
         GOTO      ZERO
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "ALERT"
PREPLABL
.START PATCH 2.56 REPLACED LOGIC
.         IFNZ      PC
.         PREPARE   LABEL,"TRIPLEX1/OUT:PRINT"
.         XIF
.         IFZ      PC
.         PREPARE   LABEL,"g:\DATA\TRIPLEX.OUT"
.         XIF
.START PATCH 2.64 ADDED LOGIC
.Patch 2.76 Comment Out
.         call      CleanFaxfile
.Patch 2.76 Comment Out
.END PATCH 2.64 ADDED LOGIC
.Patch 2.76 Comment Out
.        PRTOPEN   LABEL,"FAXFILE","FAXFILE.PRN"
.Patch 2.76 Comment Out
.END PATCH 2.56 REPLACED LOGIC
         GOTO      CLOCK
.START PATCH 2.64 ADDED LOGIC
CleanFaxfile
        clear   taskname
          Path      Exist,"c:\windows"
          if over                       .nt/2000
                    append    "!c:\winnt\system32\cmd.exe",taskname
          elseif (osflag = c6)          .XP
                    append    "!c:\windows\system32\cmd.exe",taskname
          else                          .95/98
                    append    "!c:\command.com",taskname
          endif
        append  " /c del ",taskname
.START PATCH 2.79 REPLACED LOGIC
.         append    NTWKPATH4,taskname
.         append    "fax\faxfile.prn",taskname
          append    "C:\WORK\faxfile.prn",taskname
.END PATCH 2.79 REPLACED LOGIC
        reset   taskname
        execute taskname
.
        clear   taskname
          Path      Exist,"c:\windows"
          if over                       .nt/2000
                    append    "!c:\winnt\system32\cmd.exe",taskname
          elseif (osflag = c6)          .XP
                    append    "!c:\windows\system32\cmd.exe",taskname
          else                          .95/98
                    append    "!c:\command.com",taskname
          endif
        append  " /c del \\nins1\e\data\TRIPLEX1.OUT",taskname
        reset   taskname
        execute taskname
        PAUSE   C5
          return
.Patch 2.76 Logic Added
LabelListViewAdd
          labellistview.InsertItem giving N8 using ORTNDTEM
          labellistview.SetItemText giving N1 using N8,TDMCSW,1
          labellistview.SetItemText giving N1 using N8,OFOCODE,2
          labellistview.SetItemText giving N1 using N8,RTCOMP,3
          labellistview.SetItemText giving N1 using N8,OMLRPON,4
          labellistview.SetItemText giving N1 using N8,OFDESC,5
          labellistview.SetItemText giving N1 using N8,O1DES,6
          labellistview.SetItemText giving N1 using N8,O2DES,7
          labellistview.SetItemText giving N1 using N8,QTYOUT,8
          labellistview.SetItemText giving N1 using N8,NSEL2NAME,9
          labellistview.SetItemText giving N1 using N8,OMLRKY,10
          labellistview.SetItemText giving N1 using N8,LBOFR,11
          labellistview.SetItemText giving N1 using N8,LROUT,12
          labellistview.SetItemText giving N1 using N8,OMLRPON,13
          labellistview.SetItemText giving N1 using N8,DIM45b,14
          labellistview.SetItemText giving N1 using N8,STR45,15
          labellistview.SetItemText giving N1 using N8,RTADDR,16
          labellistview.SetItemText giving N1 using N8,RTCITY,17
          labellistview.SetItemText giving N1 using N8,RTSTATE,18
          labellistview.SetItemText giving N1 using N8,RTZIP,19
          return
GetListViewLabels
          labellistview.GetItemText giving ORTNDTEM using num9,0
          labellistview.GetItemText giving TDMCSW using num9,1
          labellistview.GetItemText giving OFOCODE using num9,2
          labellistview.GetItemText giving RTCOMP using num9,3
          labellistview.GetItemText giving OMLRPON using num9,4
          labellistview.GetItemText giving OFDESC using num9,5
          labellistview.GetItemText giving O1DES using num9,6
          labellistview.GetItemText giving O2DES using num9,7
          labellistview.GetItemText giving QTYOUT using num9,8
          labellistview.GetItemText giving NSEL2NAME using num9,9
          labellistview.GetItemText giving OMLRKY using num9,10
          labellistview.GetItemText giving LBOFR using num9,11
          labellistview.GetItemText giving LROUT using num9,12
          labellistview.GetItemText giving OMLRPON using num9,13
          labellistview.GetItemText giving DIM45b using num9,14
          labellistview.GetItemText giving STR45 using num9,15
          labellistview.GetItemText giving RTADDR using num9,16
          labellistview.GetItemText giving RTCITY using num9,17
          labellistview.GetItemText giving RTSTATE using num9,18
          labellistview.GetItemText giving RTZIP using num9,19
          return
.Patch 2.76 Logic Added
.END PATCH 2.64 ADDED LOGIC
.START PATCH 2.53 ADDED LOGIC
         INCLUDE   NCNTIO.inc
.END PATCH 2.53 ADDED LOGIC
.START PATCH 2.89 REMOVED LOGIC
.;START PATCH 2.62 ADDED LOGIC
.         INCLUDE   NFULIO.INC
.;END PATCH 2.62 ADDED LOGIC
.END PATCH 2.89 REMOVED LOGIC
.START PATCH 2.74A ADDED LOGIC
          INCLUDE   NSEL2IO.INC
          INCLUDE   NSEL3IO.INC
          INCLUDE   NADDIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NREFIO.INC
          INCLUDE   NMODIO.INC
.END PATCH 2.74A ADDED LOGIC
.Patch 2.76 Logic Added
          include   prtorderpage1.inc
.Patch 2.76 Logic Added


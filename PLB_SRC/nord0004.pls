;...........................................................................
;
; PROGRAM    : NORD0004
; DATE       : 08/20/92
; AUTHOR     : D.L. HERRICK
. DESCRIPTION: PRODUCES NIN MAILER SUMMARY (Y1).
.
............................................................................
.
PC             EQU        0
               INCLUDE    COMMON.inc
               INCLUDE    CONS.inc
               INCLUDE    NORD2DD.inc
               INCLUDE    NDATDD.inc
               INCLUDE    NXRFDD.inc
               INCLUDE    NXCHDD.inc
               INCLUDE    NXNGDD.inc
.Begin patch 3.85
          include   compdd.inc
          include   cntdd.inc
.               INCLUDE    NMLRDD.inc
.End patch 3.85
               INCLUDE    NOWNDD.INC
               include    nmrgdd.inc
               include    statsdd.inc
.START PATCH 3.6 - ADDED LOGIC
               include    nofrdd.inc
.END PATCH 3.6 - ADDED LOGIC
               include    hp.inc
.START PATCH 3.84 ADDED LOGIC
          INCLUDE   NSELDD.INC
          INCLUDE   NSEL2DD.INC
          INCLUDE   NMODDD.INC
          INCLUDE   NTXTDD.INC
.END PATCH 3.84 ADDED LOGIC
          Include   PrtPagedd.inc
          include winapi.inc

+
release  init      "4.02"          DLH     added Flags= to prtopen with PDF:
reldate   Init      "2014 March 25"
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
.release  init      "4.01"         DLH email the xls file
.RelDate  Init      "2013 May 2"
.release  init      "4.00"         DLH Sunbelt PDF
.RelDate  Init      "2013 April 22"
.release  init      "3.95"         DLH PDF995
.RelDate  Init      "20 December 2012"
.release  init      "3.94"         DLH Printer Name change
.RelDate        Init               "24 January 2012"
.release  init      "3.93"         DLH try and really fix Susan print problem
.RelDate        Init               "12 May 2010"
.release  init      "3.92"         JD 02Jan2008   Added hpreset to help with Print area 5 SA.
.RelDate        Init               "01/02/2008"
.release  init      "3.91"         DLH 27mar2007 Pacific LIsts  uses company from common.inc
.RelDate        Init           "03/27/2007"
.release  init      "3.90"         JD06Apr2006 Upped the array printrec. Wilderness all dates bombing.
.release  init      "3.89"         JD   06Oct2005 Skipped lcrs/pendings, upped the array printrec.
.release  init      "3.88"        DMB   18JUN2005 IP CHANGE for File Manager
.RelDate        Init           "06/18/2005"
.release  init      "3.87"        ASH   14MAR2005 Exchange File Conversion
.RelDate        Init           "03/14/2005"
.release  init      "3.86"        ASH   06AUG2004 Logo Conversion
.RelDate        Init           "08/06/2004"
.release  init      "3.85"        DMB   26MAY2004 Mailer Conversion
.RelDate        Init           "05/26/2004"
.Release        INit           "3.84"  ASH  28JAN04  DATACARD CONVERSION
.RelDate        Init           "01/28/2004"
.Release        INit           "3.83"  DMB  MOVED CHECK FOR LM EXFEE AND RUNCHARGE IMMEDIATELY AFTER READ
.Release        INit           "3.82"  DLH  Array left at 500 at mlr totals ensured PRintRecCount reset to 0 and that we goto done routine even if total was 0
.RelDate        Init           "11/12/2003"
.Release        Init           "3.81"  JD increased print interations 500 to 1000.   12Nov2003 funny looks like 500 ?
.Release        Init           "3.8"  DLH
.RelDate        Init           "07/15/2003"
.release   init       "3.7.4"       ASH 26JUL01 CLEAN UP OF NET NAMES FORMULA
.release   init       "3.7.3"       ASH 19MAR01 NINORD PUT ON FILE MANAGER
.release   init       "3.7.2"       ASH 02OCT2000 NEW SERVER ADDED
.release   init       "3.7.1"       DLH 13Dec99 recompile for new statsdd.inc
.RELEASE   INIT       "3.7"       ASH 08JUN99 NINXCHNG.DAT Y2K FILE CONVERSION
.RELEASE   INIT       "3.6"       ASH 06MAY99 Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.release   init       "3.5"       DLH 12Apr99 recompile for new statsdd.inc
.release  init       "3.4"       ASH fixed printing problems
.release  init       "3.3"       ASH NINORD Y2K, File expansion
.release  init       "3.2"       dlh 06jan99 Banner pages
.release  init       "3.1"       DLH 22Dec98 skip pending orders
.release   init       "3.0"       DLH  14May98 NWF flat file major cleanup.revision
.                                added formulas for cumulative data.
.release   init       "2.9"       JD   20apr98 cleaned up exstat calc.
.release   init       "2.8"      DLH 09Apr98 Cleanup of formula1
.release   init       "2.7"      JD  19Mar98 Added EXFEELST check.
.release   init       "2.6"     DLH 28Jan98 Turn off projections for NWF
.release   init      "2.5"      DLH 23Jul97  Fix display/print problem with no exstat status
.release  init       "2.3"      DLH 08Jul97  replace nwf with generic ninstat file
.release  init       "2.2"      DLH 02Apr97  add NWF stats
.release  init       "2.1"      DLH 25OCt96  removal of unwanted characters at
.                              flat file write
.release  init       "2.0"      DLH 08niv95  add response vars to write
.Release  init       "1.9"      JD 05aug94    mod lotus write.
.Release  init       "1.8"      JD 06Mar94    add CopyRightS.
.Release  init       "1.7"     DLH 17Feb94  break on list select option.(Y2)
.RELEASE  INIT       "1.6"     DLH 20Aug93  compressed print fix.
.
.RELEASE  INIT       "1.5"     DLH 04APR93  ADDED DATACARD kREVDATE & LIST
.                                   OWNER NAME TO LOTUS OUTPUT & inhouse.
.RELEASE INIT           "1.4"     DLH 25SEP92 BUG FIXED WHEN 1ST ORDER CANCELLED
.                             OR RUNNING CHARGE.
.RELEASE  INIT          "1.3"     DLH 16SEP92 MINOR FIXES TO LOTUS FLAT FILE.
.RELEASE  INIT          "1.2"     DLH 03SEP92 ADD PRINT OF MAILDATE YEAR IN HEADER.
.RELEASE  INIT          "1.2"     DLH 27AUG92   ADD DELIMITED OUTPUT FILE OPTION
.                                                 SO FILE CAN BE IMPORTED TO 123.
.RELEASE  INIT          "1.1"     DLH 27AUG92   ADD INHOUSE OPTION
.                                                               PRINTS LIST NUMBER.
.RELEASE  INIT          "1.0"     DLH 20AUG92   NEW
.
.
............................................................................
.
.Start Patch #3.3 - added vars
Num9     form      9
Num9b    form      9
.End Patch #3.3 - added vars
.Start Patch #3.3 - increased file size
.NORDsFILE IFILE     KEYLEN=6,FIXED=294
NORDsFILE IFILE     KEYLEN=6,FIXED=408
.End Patch #3.3 - increased file size
formula1 dim       180
formula2 dim       70
formula3 dim       70
formula4 dim       70
formula5 dim       160
formula6 dim       70
formula7 dim       70
formula8 dim       70
formula9 dim       70
formul10 dim       70
formul11 dim       70
formul12 dim       70
formul13 dim       70
formul14 dim       70
formul15 dim       70
formul16 dim       70
formul17 dim       70
formul18 dim       70
formul19 dim       70
excelrow  dim      5
exbal    form      10
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2

INHFLAG  FORM      1         INHOUSE FLAG 1=NO LIST # 2=LIST NUMBER PRINTED
SRTFLAG  FORM      1         SORT FLAG, 1=MAILER#, 2=MAILER NAME
TOTFLAG  FORM      1         TOTL FLAG, 1=NO TOTALS, 2=TOTALS  NOT USED
BRKFLAG  FORM      1         BROKER FLAG, 1=NO BROKER INVOLVED, 2=BROKER INVLVE
LOTSFLAG FORM      1         DELIMITED LOTUS FILE FLAG, 1=PRINT, 2=FILE
slctflag form      1         break on list select 1=no break 2=break
nwfSflag form      1         1 none, 2=nwf aggregate stats, 3= nwf stats subtot by package.
banrflag form      1      1=no banner page, 2 = banner page
HOLDMLR  INIT      "    "
HOLDCNT  INIT      "   "
HOLDOFR  INIT      "   "
HOLDNAME INIT      "                        "
.START PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
HoldMCOMP dim       75
.END PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
holdlr   dim       6
o3des    dim       35
.START PATCH 3.84 replaced LOGIC
.holdslct dim       35
holdslct dim       75
.END PATCH 3.84 replaced LOGIC
MLRTAB   FORM      5
OFRTAB   FORM      3
ORDOFR   DIM       3         HOLDS ORDER OFFER #.
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
DATEPRT3 DIM       8
NUMMASK  INIT      "Z,ZZZ,ZZZ"
NUMMASKA INIT      "ZZZZZZ"
NUMPRT1  DIM       9
NUMPRT2  DIM       9
NUMPRT3  DIM       9
PDFFlag   dim       1
FileCheck FIle
trapcount form      4

.Start Patch #3.3 - increased vars
.TORDQTY  FORM      9
.TORDQTYE FORM      9         TOTAL EXHANGE
.TORDQTYR FORM      9         TOTAL RENTAL
TORDQTY  FORM      11
TORDQTYE FORM      11        TOTAL EXHANGE
TORDQTYR FORM      11        TOTAL RENTAL
.End Patch #3.3 - increased vars
TORDNUM  FORM      6         TOTAL ORDERS PRINTED
TORDNUME FORM      6         TOTAL EXCHANGE ORDERS PRINTED.
TORDNUMR FORM      6         TOTAL RENTAL ORDERS PRINTED,
TORDNUMS FORM      6         TOTAL SPLIT ORDERS  PRINTED.
GORDQTY  FORM      9
.Start Patch #3.3 - increased vars
.NUMMASKB INIT      "ZZZ,ZZZ,ZZZ"
NUMMASKB INIT      "ZZ,ZZZ,ZZZ,ZZZ"
.End Patch #3.3 - increased vars
NUMPRTA  DIM       6
.Start Patch #3.3 - increased vars
.NUMPRT1B DIM       11
.NUMPRT2B DIM       11
.NUMPRT3B DIM       11
.NUMPRT4B DIM       11
.
NUMPRT1B DIM       14
NUMPRT2B DIM       14
NUMPRT3B DIM       14
NUMPRT4B DIM       14
.EndPatch #3.3 - increased vars
PPMMASK  INIT      "$$$Z.ZZ"
PPMPRT   DIM       7
CANTEXT  DIM       11
ENTTEXT  DIM       3
TAPETEXT DIM       14
EXCHTEXT DIM       10
COMTEXT  DIM       11
NINPPATH FORM      1                 '0' FILE CLOSED '1'=OPEN
B11      INIT      "           "
REURTNS  INIT      "0000-0001"
sysjday  form       5                 Today in Julian
C55      FORM      "55"
N52      FORM      5.2
N42      FORM      4.2
INPUT      Form      5
.Start Patch #3.3 - increased vars
.M1         FORM      8
.M2         FORM      8
.M3         FORM      8
.M4         FORM      8
.M5         FORM      8
.M6         FORM      8
.M7         FORM      8
.M8         FORM      8
.M9         FORM      8
.M10        FORM      8
.M11        FORM      8
.M12        FORM      8
.T1         FORM      8
.T2         FORM      8
.T3         FORM      8
.T4         FORM      8
.T5         FORM      8
.T6         FORM      8
.T7         FORM      8
.T8         FORM      8
.T9         FORM      8
.T10        FORM      8
.T11        FORM      8
.T12        FORM      8
.
M1         FORM      10
M2         FORM      10
M3         FORM      10
M4         FORM      10
M5         FORM      10
M6         FORM      10
M7         FORM      10
M8         FORM      10
M9         FORM      10
M10        FORM      10
M11        FORM      10
M12        FORM      10
T1         FORM      10
T2         FORM      10
T3         FORM      10
T4         FORM      10
T5         FORM      10
T6         FORM      10
T7         FORM      10
T8         FORM      10
T9         FORM      10
T10        FORM      10
T11        FORM      10
T12        FORM      10
.begin patch 3.8
col1           form           "3000"               ;print positions for Monthly info
col2           form           "3625"
col3           form           "4250"
col4           form           "4875"
col5           form           "5500"
col6           form           "6125"
col7           form           "6750"
col8           form           "7375"
col9           form           "8000"
col10          form           "8625"
col11          form           "9250"
col12          form           "9875"
Mpos1          form           4                  ;print column variable is loaded here. IE if start month is jan col1 =Mpos1
Mpos2          form           4                  ;print column variable is loaded here. IE if start month is Feb col1 =Mpos2
Mpos3          form           4                  ;print column variable is loaded here. IE if start month is mar col1 =Mpos3
Mpos4          form           4
Mpos5          form           4
Mpos6          form           4
Mpos7          form           4
Mpos8          form           4
Mpos9          form           4
Mpos10         form           4
Mpos11         form           4
Mpos12         form           4
.end patch 3.8
.End Patch #3.3 - increased vars
RENTEXCH dim         1
PLSTNAME DIM           35
.START PATCH 3.84 replaced LOGIC
.PLSTSLCT DIM           35
PLSTSLCT DIM           75
.END PATCH 3.84 replaced LOGIC
HLIST      DIM       6
EXSTAT   DIM       25
XFLAG    FORM          1
.START PATCH 3.84 REPLACED LOGIC
.TEXT       DIM         46
TEXT       DIM         75
TEXT1      DIM         46
.END PATCH 3.84 REPLACED LOGIC
PERCENT  FORM      4.2
Percent1 form       4.4
CALCPER  FORM      7.4
nordin    form      8
nordinb    form      8
nordoutb   form      8
nordout   form      8
break     dim       1
.START PATCH 3.7 - REPLACED LOGIC
.usage1c   form      9
.usage2c   dim       9
usage1c   form      10
usage2c   dim       10
.END PATCH 3.7 - REPLACED LOGIC
.............................................\
.response vars
rGross   form      7            gross names used
Rperdupe form      2            percentage dupes
Rnetname form      7            net names
rperetn  form      2.2          % returned
rnumret  form      7            number of returns
ravgift  form      3.2          average gift
rincome  form      7            total income
rcost    form      7            total cost
gainloss form      7            diff of above
rcostm   form      3.2          cost per member
rincm    form      4            income per member
rperm    form      4            cost per thousand
rcost$   form      3.2          cost per $ raised
ractual  form      2.2          actual percentage
rprojc   form      3            projected % change
.
.stats goodies
statcnt  form      5            .number of stat records accumulated for list
calcrr   form      7.6          .used to calc current records rr 16Apr98 DLH
.calcrr   form      3.6           used to calc current records rr
statrr   form      3.6          .tot response rate
avgift   form      4.2          .tot  aver gift
calccost form      6.4
calcgift form      6.4
calcostm  form      6.4
Tcpm     form      6.2          .total cost to acquire
TMcst    form     10.2          .total cost to ???
averrr   form      1.6          .averaged response rate
avergft  form      2.2          .averaged average gift
avertcpm form      3.2
averTMcst form      6.2
.these are for stats
lodate    form       5
hidate    form       5
lodate1   dim        10
hidate1   dim        10
.end of these are for stats
.begin patch 3.8         range of dates on report
RepLodate      form       5                           ;PS clear if mlr break
RepHidate      form       5
RepLodate1     dim        10
RepHidate1     dim        10
.end patch 3.8
..........................................................
.begin patch 3.8
................................................................................
PRintRecCount    form           4
.PrintRec        REcord         (300)                                   ;artificial cap check if exceeded and issue warning
.PrintRec        REcord         (500)                                   ;artificial cap check if exceeded and issue warning
.begin patch 3.89
.PrintRec        REcord         (700)                                   ;artificial cap check if exceeded and issue warning
.end patch 3.89
.begin patch 3.90
PrintRec        REcord         (1100)                                   ;artificial cap check if exceeded and issue warning
.end patch 3.90
.START PATCH 3.81 - ADDED LOGIC
.PrintRec        REcord         (2000)                                   ;artificial cap check if exceeded and issue warning
.end PATCH 3.81 - ADDED LOGIC
PrintList       Dim            6              01-06              List number
PrintListName   Dim            35             07-41
PrintM1         FORM          10              42-51
PrintM2         FORM          10              52-61
PrintM3         FORM          10              62-71
PrintM4         FORM          10              72-81
PrintM5         FORM          10              82-91
PrintM6         FORM          10              92-101
PrintM7         FORM          10             102-111
PrintM8         FORM          10             112-121
PrintM9         FORM          10             122-131
PrintM10        FORM          10             132-141
PrintM11        FORM          10             142-151
PrintM12        FORM          10             152-161
.START PATCH 3.84 replaced LOGIC
.PrintSelect     Dim            35            162-196
PrintSelect     Dim            75            162-196
.END PATCH 3.84 replaced LOGIC
PrintPerc       Form           4.2           197-213
PrintExstat     Dim           25             214-238
.START PATCH 3.84 replaced LOGIC
.PrintDCText     Dim            46            239-284              qty for charge used in display and print, populated by compute
PrintDCText     Dim            75            239-284              qty for charge used in display and print, populated by compute
.END PATCH 3.84 replaced LOGIC
PrintRevdate    Dim            10            285-294              Total for charge used in display and print, populated by compute
PrintOwnocpy    Dim            35            295-329
               recordend
+
. to make nice for fiscal reports   IE report runs from June 2002 Thru May 2003
.to keep track:
.if   record month is >= the low month (june) the index = month-(low Month - 1)  for July   7-(6-1)=2
.if   record month is <  the low month (june) the index = (12-(diff between Lm & month)+1           for Jan    12-(5)+1)=8
.                                                                                                   for Feb    12-4+1=9
.end patch 3.8
..............................................................................
MLRBREAK  INIT      "N"
DATE     DIM       8
OUTPUT   FILE
outname1 dim       35              .fix 30Oct2000 DLH common.inc outname is to small.
.

+.........................................................................
.
. DEFAULT OPTIONS ARE : BREAK BY MAILER NUMBER
.                       Produce Inhouse report with owner info
.
. COMMENT CAN MODIFY THE DEFAULTS:  Alpha  :  BREAK BY MAILER Name
.                                   Lotus  :  produce Flat file
.                                   Select :  Break on list select
.
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................
.
         MOVE      "Client Summary (Y)" TO STITLE
         move      c1 to nwfsflag
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NORD0004" TO PROGRAM
         MOVE      "DISKIN" TO INPNAME
         MOVE      "LOCAL" TO PRTNAME
.         MOVE      "Pacific Lists, Inc." TO COMPNME
.begin patch 3.91
.          If        (Company = c2)
.         MOVE      "Pacific Lists, Inc." TO COMPNME
.          else
         MOVE      "Names In The News" TO COMPNME
.          endif
.end patch 3.91

         ENDIF
         CALL      PAINT
         MOVE      C0 TO BRKFLAG
         MOVE      C0 TO srtFLAG
           MOVE        C0 TO INHFLAG
         MOVE      C0 TO NINPPATH
           MOVE        C1 TO LOTSFLAG
           move      c0 to slctflag
          scan     "0170" in prtname
          if        equal
          move      c3 to nwfsflag
.          move      c2 to slctflag               break in list select
          move      c1 to slctflag               break in list select
          endif
         TRAP      EOJ IF F5
         MOVE      "Exit" TO PF5
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         CALL      FUNCDISP
.

         clear     holdslct
         MATCH     "        " TO TODAY
         IF        EQUAL
         GOTO      GETDATE
         ELSE
         IF        EOS
         GOTO      GETDATE
         ENDIF
         ENDIF
         GOTO      BEGIN
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
         unpack    date into mm,str1,dd,str1,yy
         move      "20" to cc
         call       cvtjul
         move       juldays to sysjday

         XIF
.
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.START PATCH 3.7.3 REPLACED LOGIC
.         open      Nordsfile,"ninord"
.>Patch 3.88 Begin
.         open      Nordsfile,"ninord.ISI|20.20.30.103:502"
         open      Nordsfile,"ninord.ISI|NINS1:502"
.>Patch 3.88 End
.END PATCH 3.7.3 REPLACED LOGIC
.
         GOTO      OPTION
OPTGET   MOVE      C1 TO SRTFLAG
           MOVE      C0 TO INHFLAG
           MOVE        C1 TO LOTSFLAG
         move      c0 to banrflag
           move      c0 to slctflag
         RESET     COMMENT
         KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
              *P20:11,"INHOUSE:  PRINT LIST NUMBER":
              *P20:12,"LOTUS :   PRODUCE OUTPUT FILE":
              *P20:13,"Select :  Break on list select":
                   *p20:14,"Banner  : PRINT Banner Page":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         trap      help if f1
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C1 TO SRTFLAG
           MOVE      C0 TO INHFLAG
           MOVE        c1 TO LOTSFLAG
           move      c1 to slctflag
            move     c0 to banrflag
            move    c0 to PDFflag
           RESET     COMMENT
         SETLPTR   COMMENT
          IF        (FUNC = "1")
          CALL       optPDF
          endif
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "ALPHA" IN COMMENT
         CALL      OPTALPH IF EQUAL
         RESET     COMMENT
           SCAN      "INHOUSE" IN COMMENT
           CALL      OPTINH IF EQUAL
           RESET       COMMENT
           SCAN      "LOTUS" IN COMMENT
           CALL      OPTLOTUS IF EQUAL
           RESET       COMMENT
           SCAN      "SELECT" IN COMMENT
           CALL      OPTSLCT IF EQUAL
           RESET       COMMENT
           SCAN      "NWFA" IN COMMENT
           CALL      OPTNWFA IF EQUAL
           RESET       COMMENT
           SCAN      "NWFB" IN COMMENT
           CALL      OPTNWFB IF EQUAL
         SCAN      "BANNER" IN COMMENT
         CALL      optBanr IF EQUAL
         RESET     COMMENT

         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"ALPHA  :  BREAK BY MAILER NAME":
              *P20:11,"INHOUSE:  PRINT LIST NUMBER":
              *P20:12,"LOTUS :   PRODUCE OUTPUT FILE":
              *P20:13,"Select :  Break on list select":
                   *p20:14,"Banner  : PRINT Banner Page":
                   *p20:15,"PDF  : PRINT to PDF":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO SRTFLAG
           MOVE      C1 TO INHFLAG
           MOVE      C1 TO LOTSFLAG
         move      c1 to slctflag
         move      c1 to nwfsflag
         GOTO      INPGET
OPTALPH  MOVE      C2 TO SRTFLAG
         MOVE      C1 TO LOTSFLAG
         RETURN
OPTINH   MOVE      C2 TO INHFLAG
           RETURN
OPTLOTUS MOVE      C2 TO LOTSFLAG
           RETURN
OPTSLCT
           MOVE      C2 TO SLCTFLAG
.         move      c1 to slctflag
           RETURN
optnwfa  move      c2 to nwfsflag
         return
optnwfb  move      c3 to nwfsflag
.         move      c2 to slctflag
         move      c1 to slctflag
         return
optbanr
         MOVE      C2 TO banrFLAG
         return
optPDF
         MOVE      C2 TO PDFFLAG
         return
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
.          pack      str255 from inpname,".dat|NINS1:502"
          pack      str255 from inpname,".dat"
.         OPEN      TESTFILE,INPNAME
         OPEN      TESTFILE,str255
         MOVE      C1 TO NINPPATH
          pack      taskname from "\\nins1\e\data\",inpname,".dat,\\nins1\e\data\",inpname,".srt;3-6,214-248,249-284,7-12"

          Sort      Taskname,SUNDM="NINS1:502"
.          Sort      Taskname
.
          if over
                    alert     Caution,taskname,result
                    alert     caution,S$ERROR$,result
                    return
          endif
         
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
.         MOVE      INPNAME TO NORDNAME
          pack      NORDNAME from inpname,".srt|NINS1:502"
.         MOVE      str255 TO NORDNAME
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PRTNAME
           BRANCH    LOTSFLAG TO PRTOK,OUTPUT
.begin patch 3.93
.PRTOK      SPLOPEN   PRTFILE
PRTOK     Call      PrintPrep
.          Prtopen   Laser,"\\NINs2\Laser8",PRTFILE
.end patch 3.93
         compare   c2 to banrflag
         call      prtbanr if equal
.         DISPLAY   *P15:07,PRTNAME
.          PRINT     033,"M"
.         print     hpland,hpptch,hplin8,hptop,hpbot;
.begin patch 3.93
.begin patch 3.92
.         print     hpreset
.end patch 3.92
       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                   *MarginL=0,*MarginT=0:
                   *Alignment=*Left,*font=prtpg7
.                   *Alignment=*Left,*font=prtpg85
.         print     hpland,HPtmsr17,hplin8,hptop,hpbot;
.end patch 3.93
           GOTO      PRESTART
.begin patch 3.93
.OUTPUT   SPLOPEN   PRTFILE
OUTPUT    Call      PrintPrep
.Prtopen   Laser,"\\NINs2\Laser8",PRTFILE   
         compare   c2 to banrflag
         call      prtbanr if equal
         DISPLAY   *P15:07,PRTNAME
.           clear     outname
           clear     outname1
.START PATCH 3.7.2 REPLACED LOGIC
.           APPEND    "g:\DATA\" TO OUTNAME
.           APPEND    NTWKPATH1 TO OUTNAME
           APPEND    NTWKPATH1 TO OUTNAME1
.END PATCH 3.7.2 REPLACED LOGIC
.           APPEND    "INPNAME" TO OUTNAME           .dlh 17fEB98
.         APPEND    PRTNAME TO OUTNAME
         APPEND    PRTNAME TO OUTNAME1
.           APPEND    ".TMP" TO OUTNAME
           APPEND    ".csv" TO OUTNAME1
           RESET     OUTNAME1
.           RESET     OUTNAME
.           PREPARE   OUTPUT,OUTNAME
           PREPARE   OUTPUT,OUTNAME1
.           prepare   output,"jose.tst"
           DISPLAY   *P01:07,"Output File(s) :":
                     *P15:07,PRTNAME,b1,outname1
.                     *P15:07,PRTNAME,b1,outname
.          PRINT     033,"M"
.         print     hpland,hpptch,hplin8,hptop,hpbot;
.begin patch 3.92
.         print     hpreset
.end patch 3.92
       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                   *MarginL=0,*MarginT=0:
                   *Alignment=*Left,*font=prtpg7
.       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
.                   *MarginL=0,*MarginT=0:
.                   *Alignment=*Left
.end patch 3.93
           GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
+........................................................................
.
PRESTART KEYIN     *P1:24,*T5,STR1;
         CLEAR     PF4
         TRAPCLR   F4
         CALL      FUNCDISP
           MOVE        C0 TO M1
           MOVE        C0 TO M2
           MOVE        C0 TO M3
           MOVE        C0 TO M4
           MOVE        C0 TO M5
           MOVE        C0 TO M6
           MOVE        C0 TO M7
           MOVE        C0 TO M8
           MOVE        C0 TO M9
           MOVE        C0 TO M10
           MOVE        C0 TO M11
           MOVE        C0 TO M12
.begin patch 3.8
           Move           c0 to PRintRecCount
.end patch 3.8
.          PRINT     033,"M"
START    CALL      NORD2SEQ
.         goto start if (olnum = "018710")
         GOTO      EOJ IF OVER
.PATCH3.83
.begin patch 
.begin patch 3.89
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      start IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      start IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      start IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      start IF EQUAL     YES, skip.
.end patch 3.89
.note cancodes also updated to skip cancelled pending orders.
.end patch 
           RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      START IF EQUAL
         RESET     EXFEELST
         SCAN      OLNUM IN EXFEELST
         GOTO      START IF EQUAL
.PATCH3.83
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
.BEGIN PATCH 3.8
               MOVE           OMDTEM to MM
               MOVE           OMDTED to DD
               MOVE           OMDTEY to YY
               call           cvtjul
.ENd PATCH 3.8
               ADD       C1 TO INPUT
               compare   c1 to INput
               if        equal
.BEGIN PATCH 3.8
               move      juldays to RepLodate
               move      juldays to RepHidate
               pack      RepLodate1 from omdtem,slash,omdted,slash,omdtec,omdtey
               pack      RepHidate1 from omdtem,slash,omdted,slash,omdtec,omdtey
.ENd PATCH 3.8
.               match     "0170" to omlrnum
.                              if        equal
.                              move      c3 to nwfsflag
.                              move      c2 to slctflag
.                              endif

               endif
               DISPLAY   *P15:09,INPUT,b1,olrn
.BEGIN PATCH 3.8
         compare   Replodate to juldays
         if        less
         move      Juldays to Replodate
         pack      Replodate1 from omdtem,slash,omdted,slash,omdtec,omdtey
         endif

         compare   juldays to Rephidate
         if         less
         pack      Rephidate1 from omdtem,slash,omdted,slash,omdtec,omdtey
         move      juldays to Rephidate
         endif
.ENd PATCH 3.8
.
CHKRUN
.PATCH3.83          COMMENT OUT  - THIS CHECK DONE ABOVE
.          RESET     RUNCODES
.         SCAN      OLNUM IN RUNCODES
.         GOTO      START IF EQUAL

.         RESET     EXFEELST
.         SCAN      OLNUM IN EXFEELST
.         GOTO      START IF EQUAL
.PATCH3.83          END COMMENT OUT
.begin patch 3.1
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       lcr order ?
         GOTO      Start IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       lcr order ?
         GOTO      Start IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.1
.
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      START IF EQUAL
         ADD       C1 TO N9

         MOVE      olrn TO NMRGFLD
.
SRTCHK   BRANCH    SRTFLAG TO CHKNUM,CHKALPH
CHKNUM   MATCH     OMLRNUM TO HOLDMLR
         GOTO      EXCHK IF EQUAL
         CALL      NEWMLR
         GOTO      EXCHK
CHKALPH  MATCH     ORDCNAME TO HOLDNAME
         CALL      NEWMLR IF NOT EQUAL
.
.
EXCHK    MOVE      B10 TO EXCHTEXT
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      EXCHANGE IF EQUAL
         GOTO      RENT
EXCHANGE
.Start Patch #3.3 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO Num9
         MOVE      OEXQTY TO Num9
         COMPARE   C0 TO Num9
.End Patch #3.3 - replaced var
.         IF        EQUAL
         GOTO      SPLIT IF NOT EQUAL
         ADD       C1 TO TORDNUME
.Start Patch #3.3 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYE
         MOVE      C0 TO Num9
         MOVE      OQTY TO Num9
         ADD       Num9 TO TORDQTYE
.End Patch #3.3 - replaced var
         GOTO      DETAIL
.         ELSE
SPLIT    ADD       C1 TO TORDNUMS
.Start Patch #3.3 - replaced var
.         ADD       N7 TO TORDQTYE
.         SUB       N7 FROM TORDQTYR
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
.
         ADD       Num9 TO TORDQTYE
         SUB       Num9 FROM TORDQTYR
         MOVE      C0 TO Num9
         MOVE      OQTY TO Num9
         ADD       Num9 TO TORDQTYR
.End Patch #3.3 - replaced var
.         ENDIF
         GOTO      DETAIL
RENT     ADD       C1 TO TORDNUMR
.Start Patch #3.3 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TORDQTYR
         MOVE      C0 TO Num9
         MOVE      OQTY TO Num9
         ADD       Num9 TO TORDQTYR
.End Patch #3.3 - replaced var
.
DETAIL     CALL      PROCESS
           GOTO      START
.
PROCESS    MATCH     OLNUM TO HLIST
......................................
           IF          NOT EQUAL          Its a break by list number-
           COMPARE   C1 TO N9                                      |
                        IF        EQUAL                        --
                        MOVE      OLNUM TO HLIST                | 1st record
                        MOVE      HLIST TO NDATFLD              | force goodies
                        CALL      SAVE                          |
                        RETURN                                  |
                        ENDIF                                  --     |

.         call      getstat
          CALL      getexch
         call      calcnet
           CALL      NOBAL
           CALL      SAVE
............................................end of list break
                     ELSE
...........................................its not a break by list #
         compare   c1 to slctflag          break by select wanted?
           if      equal                   no.
             CALL    SAVE
                   else                    yes.
.START PATCH 3.84 replaced LOGIC
.           match   o2des to holdslct
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
          match     NSEL2NAME,holdslct
.END PATCH 3.84 replaced LOGIC
           if      not equal               Break on select.
.           call      getstat    (was not turned on 10/28/97 DLH)
             CALL      getexch
           call      calcnet
             CALL      NOBAL
             CALL      SAVE
                     ELSE
             CALL    SAVE
           endif
           endif
.................................................
         call      getstat
           RETURN                  leave process subroutine
           ENDIF
.................................................
getexch
         MOVE      HLIST TO NDATFLD
         rep       zfill in ndatfld
           MOVE        C1 TO NDATPATH
           CALL        NDATKEY
           if        over
           display   *p1:22,*el,"datacard read failed ",ndatfld,*b,*w2
         endif
         rep       zfill in nownfld
.START PATCH 3.84 REPLACED LOGIC
.           MOVE      OWNNUM TO NOWNFLD
          unpack    OWNNUM,str2,str4
          MOVE      str4,NOWNFLD
.END PATCH 3.84 REPLACED LOGIC
           MOVE      C1 TO NOWNPATH
           CALL      NOWNKEY
.START PATCH 3.84 REPLACED LOGIC
.           parse     textdata into text1 using " ~09",noskip,blankfill
.           UNPACK    TEXT1 INTO TEXT
.;          UNPACK    TEXT1 INTO stR1,TEXT
          if (NDATCONV = "1")
                    pack      NSELFLD1,"01X",LSTNUM
                    pack      NSELFLD2,"02XBASE"
                    move      "D.Load-NSELAIM",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELAIM
                    if not over
                              if (NSELEXC = "2")
                                        pack      str25,"Exchange Only"
                              else
                                        pack      NMODFLD,NSELDESC
                                        rep       zfill,NMODFLD
                                        move      "NMODKEY",Location
                                        pack      KeyLocation,"Key: ",NMODFLD
                                        call      NMODKEY
                                        call      Trim using NMODDESC
                                        move      NSELPRICE,str8
                                        call      Trim using str8
                                        pack      str25,"$",str8,NMODDESC
                              endif
                              call      FormatNumeric using NSELQTY,STR13
                              call      Trim using NSELSNAME
                              move      NSELSNAME,str35
                              pack      text,str13,B1,str35,B1,str25
                    else
                              pack      NTXTFLD,LSTNUM,"1"
                              move      "D.Load-NTXTKEY",Location
                              pack      KeyLocation,"Key: ",NTXTFLD
                              call      NTXTKEY
                              if not over
                                        move      NTXTTEXT,text1
                                        move      text1,text
                              else
                                        clear     text
                              endif
                    endif
          else
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              move      NTXTTEXT,text1
                              move      text1,text
                    else
                              clear     text
                    endif
          endif
.END PATCH 3.84 REPLACED LOGIC
           clear     nxrffld                    .dlh 04feb98
           MOVE      HLIST TO NXRFFLD
           REP       ZFILL IN NXRFFLD
           MOVE        C1 TO NXRFPATH
           CLEAR       EXSTAT
           CLEAR     NXRFMLR
           CALL      NXRFKEY
                   if        over
                   move      b25 to exstat       *make sure we don't print any status
                   move      c0 to exbal
                   return                        *nothing more to do get out    DLH 23Jul97.
                   endif
           MOVE      C1 TO XFLAG
.START PATCH 3.87 REPLACED LOGIC - TEMPORARY PATCH
.           CLEAR     NXNGFLD1
.           APPEND    "01X" TO  NXNGFLD1
.           APPEND    HOLDMLR TO NXNGFLD1
.           RESET     NXNGFLD1
.           CLEAR     NXNGFLD2
.           APPEND    "02X" TO  NXNGFLD2
.           APPEND    NXRFMLR TO NXNGFLD2
.           RESET     NXNGFLD2
.           CALL      NXNGAIM
.                     IF        OVER
.                     MOVE        C2 TO XFLAG
.                     CLEAR     NXNGFLD1
.                     APPEND    "01X" TO  NXNGFLD1
.                     APPEND    NXRFMLR TO NXNGFLD1
.                     RESET     NXNGFLD1
.                     CLEAR     NXNGFLD2
.                     APPEND    "02X" TO  NXNGFLD2
.                     APPEND    HOLDMLR TO NXNGFLD2
.                     RESET     NXNGFLD2
.                     CALL      NXNGAIM
.                               IF        OVER
.                               move      b25 to exstat
.                               move      c0 to exbal
.                               Clear     rentexch
.                               RETURN
.                               ENDIF
.                     ENDIF
.................................
          pack      COMPFLD3,HOLDMLR
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          move      COMPNUM,str6
.
          pack      COMPFLD,NXRFMLR
          move      "2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
.
          CLEAR     NXNGFLD1
          APPEND    "01X" TO  NXNGFLD1
          APPEND    str6 TO NXNGFLD1
          RESET     NXNGFLD1
          CLEAR     NXNGFLD2
          APPEND    "02X" TO  NXNGFLD2
          APPEND    COMPNUM TO NXNGFLD2
          RESET     NXNGFLD2
          CALL      NXNGAIM
          IF OVER
                    MOVE      C2 TO XFLAG
                    CLEAR     NXNGFLD1
                    APPEND    "01X" TO  NXNGFLD1
                    APPEND    COMPNUM TO NXNGFLD1
                    RESET     NXNGFLD1
                    CLEAR     NXNGFLD2
                    APPEND    "02X" TO  NXNGFLD2
                    APPEND    str6 TO NXNGFLD2
                    RESET     NXNGFLD2
                    CALL      NXNGAIM
                    IF OVER
                              move      b25 to exstat
                              move      c0 to exbal
                              Clear     rentexch
                              RETURN
                    ENDIF
          ENDIF
.END PATCH 3.87 REPLACED LOGIC - TEMPORARY PATCH
           MOVE        C1 TO NXCHPATH
           PACK      NXCHFLD1 FROM ACCKEY,ENTRY
           REP       ZFILL IN NXCHFLD1
           CALL      NXCHKEY
           if        over                   lets double check. 21sep93.
                     move      b25 to exstat
                     move      c0 to exbal
                     return
           endif
.          DISPLAY   *P1:16,*EL,NXCHFLD1,B2,USAGE1,B1,USAGE2,*W5
           SUB       USAGE1 FROM USAGE2
           COMPARE   C1 TO XFLAG
                     IF        EQUAL
                     COMPARE   USAGE2 TO C0
                     GOTO        EVEN  IF EQUAL
                             IF        NOT LESS
.                               MULT      SEQ BY USAGE2
                               move      usage2 to usage2c
                               CLEAR     EXSTAT
                               APPEND    "IS OWED " TO EXSTAT
                                rep       "- " in  usage2c
                              APPEND    USAGE2c TO EXSTAT
                               APPEND    " NAMES." TO EXSTAT
                               move      usage2 to exbal
.                               mult      seq by exbal        .DLH 04Feb98
                               Move      "R" to RentExch
                               RESET     EXSTAT
                     ELSE
                               CLEAR     EXSTAT
.                               MULT      SEQ BY USAGE2
                               move      usage2 to usage2c
                               APPEND    "OWES    " TO EXSTAT
                                rep       "- " in  usage2c
                               APPEND    USAGE2c TO EXSTAT
                               APPEND    " NAMES." TO EXSTAT
                               RESET     EXSTAT
                               move      usage2 to exbal
.                               mult      seq by exbal      .DLH 04Feb98
                               Move      "E" to Rentexch
                               ENDIF
          ELSE
                     COMPARE USAGE2 TO C0
                     GOTO      EVEN IF EQUAL
                             IF        NOT LESS
                               MULT      SEQ BY USAGE2
                               move      usage2 to usage2c
                               CLEAR     EXSTAT
                               APPEND    "OWES    " TO EXSTAT
                                rep       "- " in  usage2c
                               APPEND    USAGE2c TO EXSTAT
                               APPEND    " NAMES." TO EXSTAT
                               Move      "E" to Rentexch
                               move      usage2 to exbal
.                               mult      seq by exbal        .DLH 04Feb98
                               RESET     EXSTAT
                     ELSE
                               move      c0 to usage2c
                               move      usage2 to usage2c
                               CLEAR     EXSTAT
                               APPEND    "IS OWED " TO EXSTAT
                                rep       "- " in  usage2c
                               APPEND    USAGE2c TO EXSTAT
                               APPEND    " NAMES." TO EXSTAT
                               Move      "R" to Rentexch
                               RESET     EXSTAT
                               move      usage2 to exbal
                               mult      seq by exbal        .JD20apr98
                               ENDIF
           ENDIF
           RETURN
EVEN     MOVE    "EXCHANGE EVEN" TO EXSTAT
         move      c0 to exbal

           RETURN
.
calcnet  compare   c0 to nordout
         if        equal
         move      c0 to percent
         else
         move      c0 to calcper
         move       nordout to calcper
         DIVIDE    nordin INTO CALCPER
         MULT      "100" BY CALCPER
         MOVE      C0 TO PERCENT
         ADD       CALCPER TO PERCENT
         endif
         return
NOBAL
           CALL      DTL0
           ADD         M1 TO T1
           ADD         M2 TO T2
           ADD         M3 TO T3
           ADD         M4 TO T4
           ADD         M5 TO T5
           ADD         M6 TO T6
           ADD         M7 TO T7
           ADD         M8 TO T8
           ADD         M9 TO T9
           ADD         M10 TO T10
           ADD         M11 TO T11
           ADD         M12 TO T12
           MOVE        C0 TO M1
           MOVE        C0 TO M2
           MOVE        C0 TO M3
           MOVE        C0 TO M4
           MOVE        C0 TO M5
           MOVE        C0 TO M6
           MOVE        C0 TO M7
           MOVE        C0 TO M8
           MOVE        C0 TO M9
           MOVE        C0 TO M10
           MOVE        C0 TO M11
           MOVE        C0 TO M12
           move        c0,nordin
          move         c0,nordout
.         move      nordoutb to nordout
.         move      nordinb to nordin
.         move      c0,nordoutb
.         move      c0,nordinb
           move      c0,calcper
           move      c0,percent
           RETURN
SAVE       MOVE      OLNUM TO HLIST
.START PATCH 3.84 replaced LOGIC
.           move      o2des to holdslct
.           MOVE      O1DES TO PLSTNAME
.           MOVE      O2DES TO PLSTSLCT
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY-2",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
           move      NSEL2NAME to holdslct
           MOVE      O1DES TO PLSTNAME
           MOVE      NSEL2NAME TO PLSTSLCT
.END PATCH 3.84 replaced LOGIC
           MOVE        C0 TO N2
           MOVE      OMDTEM TO N2
           COMPARE   C0 TO N2
           IF        EQUAL         *INVALID MAIL DATE MONTH
           MOVE      OODTEM TO N2        .MOVE ORDER DATE MONTH.
           ENDIF
.Start Patch #3.3 - replaced var
.           MOVE        OQTY TO N7
.           LOAD      N8 FROM N2 OF M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
.           ADD       N7 TO N8
.           STORE     N8 INTO N2 OF M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
..
.         MOVE      HLIST TO N6
.         MOVE      C0  TO    N7
.         MOVE      OQTY TO N7
.         ADD       N7       TO TORDQTY
...
           MOVE        OQTY TO Num9
           LOAD      Num9b FROM N2 OF M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
           ADD       Num9 TO Num9b
           STORE     Num9b INTO N2 OF M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
.
         MOVE      HLIST TO N6
         MOVE      C0  TO    Num9
         MOVE      OQTY TO Num9
         ADD       Num9       TO TORDQTY
.End Patch #3.3 - replaced var
         ADD       C1 TO TORDNUM
.
getmrg    call      nmrgkey
          return    if over
          add       nmrgiqty to nordin
          add       nmrgnet to nordout
          call      getstat
          return
.
getstat
         branch    nwfsflag to nostat,getstata,getstata
getstata
.         display   *p1:24,*el,"getstata ",o1des,hlist,b1,olrn,*w3
.         move      hlist to statfld
.         rep       zfill in statfld
          clear    statfld2
.          append   hlist to statfld
.          reset    statfld
          clear    statfld2
          move     olrn to statfld2
          rep      zfill in statfld2
          clear    holdlr
          move     olrn to holdlr
          rep      zfill in holdlr
          move      c2 to statpath
.          display   *p1:24,*el,"read ready ",olrn,b1,statfld2,*w2
          call      statkey
.          display   *p1:24,*el,"test read ok "
.         goto      getstat1
         goto      nostat if over
.          display   *p1:24,*el,"read ok ",statlr,b1,statfld2,*w2

.           compare c3 to nwfsflag        .select break?
.           if      equal
.           rep     zfill in statlr
.           readtab nordsfile,statlr;*217,o3des
.          display   *p1:24,*el,"statlr ",o3des,hlist,holdslct,*w2
.           rep     lowup in o3des
.           rep     lowup in holdslct
.           match   o3des to holdslct
.           goto    getstat1 if not equal
.           endif

         move      c0 to n3
         clear     str3
         unpack    oodnum into str4,str3
         move      str3 to n3
.         display   *p1:24,*el,"offer ",str3,*w
         branch    n3 of nostat,nostat,nostat,nostat,nostat:
                   nostat,nostat,nostat,nostat,nostat:
                   nostat,nostat,nostat:
                   contrib:          offer 14 contributors
                   basic:            offer 15 basic member (Philantropic)
                   assoc:             offer 16 assoc member  subs/members
                   nostat:             offer 16 assoc member  subs/members
                   nostat:           offer 16 assoc member  subs/members
                   nostat:
                   contrib
                   goto      getstat1
contrib
.         display   *p1:24,*el,"panel = ",statpanel,*w2
         scan      "CONTRI" in statpanel
         goto      calcstat if  equal
         reset     statpanel
         scan      "Contri" in statpanel
         goto      calcstat if  equal
         scan      "CARD" in statpanel
         goto      calcstat if  equal
         scan      "Card" in statpanel
         goto      calcstat if  equal
         scan      "CALENDAR" in statpanel
         goto      calcstat if  equal
.         display   *p1:24,*el,"I know its a contrib ",statpanel
         goto      getstat1
basic    scan      "MEMBER" in statpanel
         goto      calcstat if equal
         reset     statpanel
         scan      "Member" in statpanel
         goto      calcstat if equal
         goto      getstat1
assoc
         goto      calcstat                     .02Feb98 - Nwf naming scheme change
.                                               take all since ID'd by LR
         display   *p1:24,*el,"ASSOC YEAH ",statpanel,*w2
         scan      "STANDARD" in statpanel
         goto      calcstat if equal
         reset     statpanel
         scan      "Standard" in statpanel
         goto      calcstat if equal
         reset     statpanel
.         goto      calcstat if  equal
         reset     statpanel
         scan      "SUBSCRI" in statpanel
         goto      calcstat if equal
         goto      getstat1

getstat1
         call      statks
         goto      nostat if over
           compare c3 to nwfsflag   .select break?
           if      equal
           rep     zfill in statlr
           match   statlr to holdlr
           goto    nostat if not equal
           endif

         match     hlist to statlist
         if        not equal
         display   *p1:23,*el,"List no match ",statfld,b1,hlist,b1,statlist,b1,statcnt
         goto      getstat1
         endif

           compare c3 to nwfsflag   .select break?
           if      equal
           rep     zfill in statlr
           match   "000000" to statlr
           goto    getstat1 if equal
.           readtab nordsfile,statlr;*217,o3des
.           rep     lowup in o3des
.           rep     lowup in holdslct
.          display   *p1:23,*el,"read ks ",o3des,b1,holdslct,*p1:24,statsel,statlr,*w2;
          reset     o3des
          reset     holdslct
.           match   o3des to holdslct
.          goto    getstat1 if not equal
           endif

.lets make sure not destroyed
         move      c0 to n3
         clear     str3
         unpack    oodnum into str4,str3
         move      str3 to n3
.
         branch    n3 of nostat,nostat,nostat,nostat,nostat:
                   nostat,nostat,nostat,nostat,nostat:
                   nostat,nostat,nostat:
                   contrib:          offer 14 contributors
                   basic:            offer 15 basic member (Philantropic)
                   assoc:             offer 16 assoc member  subs/members
                   nostat:             offer 16 assoc member  subs/members
                   nostat:           offer 16 assoc member  subs/members
                   nostat:
                   contrib
         goto      getstat1
calcstat
.do the wild thing
          display   *p1:21,*el,"calcstat ",statcnt
.         compare   c3 to statcnt          .last 3 occurances?
.         goto      nostat if equal       .yes, that's good (for NWF)

          display   *p1:22,*el,"calcstat check qty ",statmqty

.         compare   "5000" to statmqty    .mail 5000 or more names????
         move      "5000" to n4
.         if        (statmqty > n4)
         if        (statmqty < n4)
         goto      getstat1              .no skip record
          display   *p1:20,*el,"qty was good ",statmqty,*b
         endif

          display   *p1:23,*el,"calcstat check weeks out ",statwkso

         compare   c3 to statwkso        .how many weeks out ?
         if        less
         display   *p1:23,*el,"calcstat check weeks out ",statwkso," reject"
         goto      getstat1               .less than 3 skip
         endif
         unpack    statmdate into mm,dd,cc,yy

         display   *p1:24,*el,"calcstat check age",b1,statcnt

         call      cvtjul
         move      sysjday to n5
         sub        juldays from n5
         compare    n5 to "366"
         if         less
         display   *p1:24,*el,"calcstat check age",b1,statcnt," reject"
         goto       getstat1                  .stats older than 1 year, Reject.
         endif

         add       c1 to statcnt
         call      cvtjul
         compare   c1 to statcnt
         if        equal
         move      juldays to lodate
         move      juldays to hidate
         pack      lodate1 from mm,slash,dd,slash,cc,yy
         pack      hidate1 from mm,slash,dd,slash,cc,yy
         endif

         compare   lodate to juldays
         if        less
         move      Juldays to lodate
         pack      lodate1 from mm,slash,dd,slash,cc,yy
         endif

         compare   juldays to hidate
         if         less
         pack      hidate1 from mm,slash,dd,slash,cc,yy
         move      juldays to hidate
         endif
.
         match     "0170" to omlrnum          .Natl Wildlife Fed?
         if         equal                     .Yes
         move      c0 to calcrr
         move      statresp to calcrr          Number of responses from client
         divide    statmqty into calcrr        divided by quantity mailed
         add       calcrr to Statrr            = response rate
         goto      calc1                      .Yes, don't project, use their #'s
         endif                                .26Jan98 DLH per NWF
.
         compare   "11" to statwkso           12 = weeks of returns - no adjust necessary
         if        not less
         move      c0 to calcrr
         move      statresp to calcrr          Number of responses from client
         divide    statmqty into calcrr        divided by quantity mailed
         add       calcrr to Statrr            = response rate
         goto      calc1
         endif

.
         compare   "7" to statwkso           7-11 = weeks of returns - we are at 90-95%
         if        not less                                                         ---
         move      c0 to calcrr
         move      statresp to calcrr        Number of responses from client
         divide    ".95" into calcrr         adjust as we only have est 95%
         divide    statmqty into calcrr      divide by qty mailed= response rate
         add       calcrr to Statrr
         goto      calc1
         endif

         compare   "3" to statwkso           3 = weeks of returns - we are at 50%
         if        equal                                                         ---
         move      c0 to calcrr
         move      statresp to calcrr
         divide    ".50" into calcrr
         divide    statmqty into calcrr
         add       calcrr to Statrr
         goto      calc1
         endif

         compare   "4" to statwkso           4 = weeks of returns - we are at 65%
         if        equal                                                         ---
         move      c0 to calcrr
         move      statresp to calcrr
         divide    ".65" into calcrr
         divide    statmqty into calcrr
         add       calcrr to Statrr
         goto      calc1
         endif

         compare   "5" to statwkso           5 = weeks of returns - we are at 80%
         if        equal                                                         ---
         move      c0 to calcrr
         move      statresp to calcrr
         divide    ".8" into calcrr
         divide    statmqty into calcrr
         add       calcrr to Statrr
         goto      calc1
         endif

         compare   "6" to statwkso           6 = weeks of returns - we are at 80%
         if        equal                                                         ---
         move      c0 to calcrr
         move      statresp to calcrr
         divide    ".8" into calcrr
         divide    statmqty into calcrr
         add       calcrr to Statrr
         goto      calc1
         endif

calc1    move      c0 to calcgift            calc average gift
         move      statrev to calcgift
         divide    statresp into calcgift
         add       calcgift to avgift
.
.         calc sub-total cost in the mail

         move      c0 to calccost
         move      statIMcst to calccost
         mult      statmqty by calccost
         mult      ".001" by calccost
         add       calccost to TMcst     Total Mail cost

.         calc list cost

         move      c0 to calccost
         move      statlcpm to calccost
         mult      statmqty by calccost
         mult      ".001" by calccost
         add       calccost to TMcst     Total Mail cost

.calc cost per member/cost per response

         move      c0 to calcostm
         move      statrev to calcostm
         sub       tmcst from calcostm
         divide    statresp into calcostm
         add       calcostm to Tcpm     cost to aqcuire or cost per new member

         goto      getstat1
         stop
.
nostat   RETURN
.
. IF THERE IS A BROKER INVOLVED, AND ITS A NUMERIC BREAK, READ THE MAILER
. FILE TO GET THE MAILER NAME.
.
CHKCNT   BRANCH    BRKFLAG TO DTL0
         BRANCH    SRTFLAG OF CHKCNT1,DTL0
CHKCNT1  MATCH     OCOBN TO HOLDCNT
         GOTO      DTL0 IF EQUAL
         MOVE      OMLRNUM TO HOLDMLR
         MOVE      OCOBN TO HOLDCNT
.        MOVE      C1 TO BRKFLAG
.        MATCH     B3 TO OCOBN
.        GOTO      DTL0 IF EQUAL
         PACK      MKEY WITH OMLRNUM,OCOBN
.         DISPLAY   *P1:22,"READING MAILER FILE CHKCNT",*B
         CALL      NMLRKEY
         GOTO      MLRNG IF OVER
.START PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
          move      MCOMP,HoldMCOMP
.END PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
.        MOVE      C2 TO BRKFLAG
.
.begin patch 3.8
DTL0
.DTL0     COMPARE   C59 TO PRTLINES
.         CALL        IF NOT LESS
.end patch 3.8
           BRANCH    LOTSFLAG TO DTL1,DTL2
.
.Start Patch 3.4 - corrected printing problems
.Start Patch #3.3 - increased var
..DTL1     PRINT     *N,*01,PLSTNAME:
..                      *47,M1:
..                      *56,M2:
..                      *65,M3:
..                      *74,M4:
..                      *83,M5:
..                      *92,M6:
..                      *101,M7:
..                      *110,M8:
..                      *119,M9:
..                      *129,M10:
..                      *138,M11:
..                      *147,M12:
..                      b1,percent,"%":
..                      *N,*1,PLSTSLCT;
.DTL1     PRINT     *N,*01,PLSTNAME:
.                      *47,M1:
.                      *58,M2:
.                      *69,M3:
.                      *80,M4:
.                      *91,M5:
.                      *102,M6:
.                      *113,M7:
.                      *124,M8:
.                      *135,M9:
.                      *147,M10:
.                      *158,M11:
.                      *169,M12:
.                      b1,percent,"%":
.                      *N,*1,PLSTSLCT;
.begin patch 3.8
DTL1
               Add           c1 to PRintRecCount
               Move           PLstname to PrintRec(PrintRecCount).PrintListName
               Move           M1 to PrintRec(PrintRecCount).PrintM1
               Move           M2 to PrintRec(PrintRecCount).PrintM2
               Move           M3 to PrintRec(PrintRecCount).PrintM3
               Move           M4 to PrintRec(PrintRecCount).PrintM4
               Move           M5 to PrintRec(PrintRecCount).PrintM5
               Move           M6 to PrintRec(PrintRecCount).PrintM6
               Move           M7 to PrintRec(PrintRecCount).PrintM7
               Move           M8 to PrintRec(PrintRecCount).PrintM8
               Move           M9 to PrintRec(PrintRecCount).PrintM9
               Move           M10 to PrintRec(PrintRecCount).PrintM10
               Move           M11 to PrintRec(PrintRecCount).PrintM11
               Move           M12 to PrintRec(PrintRecCount).PrintM12
               Move           Percent to PrintRec(PrintRecCount).PrintPerc
               Move           PLstSLCT to PrintRec(PrintRecCount).PrintSelect

.               PRINT     *N,*01,PLSTNAME:
.                      *46,M1:
.                      *56,M2:
.                      *66,M3:
.                      *76,M4:
.                      *86,M5:
.                      *96,M6:
.                      *106,M7:
.                      *116,M8:
.                      *126,M9:
.                      *136,M10:
.                      *146,M11:
.                      *156,M12:
.                      b1,percent,"%":
.                      *N,*1,PLSTSLCT;
.End Patch #3.3 - increased var
.End Patch 3.4 - corrected printing problems
           BRANCH     INHFLAG TO NOLST,YESLST
NOLST
               Move           lstnum to PrintRec(PrintRecCount).PrintList
               Move           Text to PrintRec(PrintRecCount).PrintDCText
.START PATCH 3.84 REPLACED LOGIC
.               Move           RevDate to PrintRec(PrintRecCount).PrintRevDate
                    unpack    REVDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
               Move           str10 to PrintRec(PrintRecCount).PrintRevDate
.END PATCH 3.84 REPLACED LOGIC

.           PRINT     *N,*1,"## ",LSTNUM,B1,TEXT," Updated: ",REVDATE;
.NOLST      PRINT          *N,*1,TEXT;
           GOTO      DTL1A
YESLST
               Move           lstnum to PrintRec(PrintRecCount).PrintList
               Move           Text to PrintRec(PrintRecCount).PrintDCText
.START PATCH 3.84 REPLACED LOGIC
.               Move           RevDate to PrintRec(PrintRecCount).PrintRevDate
                    unpack    REVDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
               Move           str10 to PrintRec(PrintRecCount).PrintRevDate
.END PATCH 3.84 REPLACED LOGIC
               Move           Ownocpy to PrintRec(PrintRecCount).PrintOwnocpy
.               PRINT     *N,*1,"## ",LSTNUM,B1,TEXT," Updated: ",REVDATE:
.                       "Owner: ",OWNOCPY;
DTL1A
.               ADD         C4 TO PRTLINES
.end patch 3.8
.             B1,TEXT:
.Begin patch 3.8
.           CMATCH      B1 TO EXSTAT
.                     IF          EOS
.                     PRINT     B1
.                     RETURN
.                     ENDIF
.           PRINT       *N,*1,EXSTAT
.           ADD       C1 TO PRTLINES
               Move           Exstat to PrintRec(PrintRecCount).PrintExstat
.end patch 3.8
           RETURN
DTL2       CALL      DTL1
           REP       ",-" IN PLSTNAME         .remove delimiters
           REP       "#"'" IN PLSTNAME         .remove delimiters
           rep       ", " in plstslct
           REP       "#"'" IN PLSTslct     .remove delimiters
           REP       ",-" IN ownocpy           .remove delimiters
           REP       "#"'" IN ownocpy     .remove delimiters
           REP       ",-" IN text         .remove delimiters
           REP       "#"'" IN text         .remove delimiters
.                                                    FILE IMPORT.
.do stats goodies
         compare   c0 to statcnt
         if        equal
.         Display    *p1:24,*el,"I had no stats ",plstname,hlist,*w2
         move         c0 to averrr
         move         c0 to avergft
         move         c0 to avertcpm
         move         c0 to averTMcst
         move      c0 to statrr
         move      c0 to avgift
         move      c0 to tcpm
         move      c0 to tmcst
         move      c0 to lodate
         move      c0 to hidate
         clear     lodate1
         clear     hidate1
         goto      skipstat
         endif
         move      c0 to averrr
         divide    statcnt into statrr
         move      statrr to averrr
.         mult      "100" by averrr
         move      c0 to statrr
.
         move      c0 to avergft
         divide    statcnt into avgift
         move      avgift to avergft
         move      c0 to avgift
.
         move      c0 to avertcpm
         divide    statcnt into tcpm
         move      tcpm to avertcpm
         move      c0 to tcpm
.
         move      c0 to avertmcst
         divide    statcnt into tmcst
         move      tmcst  to avertmcst
         move      c0 to tmcst

         Move      c0 to percent1
         move      percent to percent1
         divide    "100" into percent1
.
skipstat move      c0 to statcnt
.
         compare   c1 to nwfsflag
         if        equal
.START PATCH 3.84 REPLACED LOGIC
.           WRITE       OUTPUT,SEQ;"#"",ownocpy,"#",":
.                   lstnum,",":
.                   "#"",PLSTNAME,"#",":
.                       M1,",":
.                      M2,",":
.                      M3,",":
.                      M4,",":
.                      M5,",":
.                      M6,",":
.                      M7,",":
.                      M8,",":
.                      M9,",":
.                      M10,",":
.                      M11,",":
.                      M12,",":
.                      percent,",":
.                      "#"",PLSTSLCT,"#",":
.                          ",#"",TEXT,"#",","#"",EXSTAT,"#"":
.                          ",#"",REVDATE,"#",#"",OWNOCPY,"#"":
.                      " ,":            gross
.                      " ,":            per dupes
.                      " ,":            net names
.                      ",",averrr:      Response rate
.                      ",",avergft:     average gift
.                      " ,":            income
.                      ",",avertcpm:
.                      ",",avertmcst:
.                      ",",lodate1:
.                      ",",hidate1
                    unpack    REVDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
           WRITE       OUTPUT,SEQ;"#"",ownocpy,"#",":
                   lstnum,",":
                   "#"",PLSTNAME,"#",":
                       M1,",":
                      M2,",":
                      M3,",":
                      M4,",":
                      M5,",":
                      M6,",":
                      M7,",":
                      M8,",":
                      M9,",":
                      M10,",":
                      M11,",":
                      M12,",":
                      percent,",":
                      "#"",PLSTSLCT,"#",":
                          ",#"",TEXT,"#",","#"",EXSTAT,"#"":
                          ",#"",str10,"#",#"",OWNOCPY,"#"":
                      " ,":            gross
                      " ,":            per dupes
                      " ,":            net names
                      ",",averrr:      Response rate
                      ",",avergft:     average gift
                      " ,":            income
                      ",",avertcpm:
                      ",",avertmcst:
                      ",",lodate1:
                      ",",hidate1
.END PATCH 3.84 REPLACED LOGIC
          else
...............................................................................
..Formula1 = net names
..=IF(F17>0,IF(M17>0,F17*M17,F17*N17),IF(M17>0,E17*M17,E17*N17))
.
.. New improved 4Feb98
..=IF(G9>0,IF(N9>0,G9*N9,IF(M9>0,G9*M9,G9*O9)),IF(N9>0,F9*N9,IF(M9>0,F9*M9,F9*O9)))


..F9*M9,F9*O9)))
        clear       formula1
        append      "=if(g",formula1
        append      excelrow to formula1
        append      ">0,",formula1
        append      "IF(n",formula1
        append      excelrow to formula1
        append      ">0,g",formula1
        append      excelrow,formula1
        append      "*n",formula1
        append      excelrow,formula1
..START PATCH 3.7.4 REMOVED LOGIC
..        append      ",if(m",formula1
..        append      excelrow,formula1
..END PATCH 3.7.4 REMOVED LOGIC
        append      ",if(m",formula1
        append      excelrow,formula1
        append      ">0,g",formula1
..START PATCH 3.7.4 ADDED LOGIC
        append      excelrow,formula1
..END PATCH 3.7.4 ADDED LOGIC
        append      "*m",formula1
        append      excelrow,formula1
        append      ",g",formula1
        append      excelrow,formula1
        append      "*O",formula1
        append      excelrow,formula1
.START PATCH 3.7.4 REPLACED LOGIC
.        append      "))),IF(N",formula1
        append      ")),IF(N",formula1
.END PATCH 3.7.4 REPLACED LOGIC
        append      excelrow,formula1
        append      ">0,f",formula1
        append      excelrow,formula1
        append      "*N",formula1
        append      excelrow,formula1
        append      ",IF(M",formula1
        append      excelrow to formula1
        append      ">0,f",formula1
        append      excelrow,formula1
        append      "*m",formula1
        append      excelrow,formula1
        APPEND      ",F",formula1
        APPEND      EXCELROW,FORMULA1
        APPEND      "*O",FORMULA1
        APPEND      EXCELROW,FORMULA1
        append      ")))",formula1
        reset       formula1

..............................................................................
..............................................................................
.Formula2 = Number of returns
.=ROUND(P17*Q17*0.01,0)
        clear       formula2
        append      "=Round(p",formula2
        append      excelrow to formula2
        append      "*Q",formula2
        append      excelrow to formula2
        append      ",0)",formula2
        reset       formula2
..............................................................................
.=S17*R17
.Formula3 = Total Income
        clear       formula3
        append      "=R",formula3
        append      excelrow to formula3
        append      "*S",formula3
        append      excelrow to formula3
        reset       formula3
..............................................................................
.(P17*$S$10/1000+P17*$S$11*$T$11)
.Formula4 = Product Cost
        clear       formula4
        append      "=(P",formula4
        append      excelrow to formula4
        append      "*$R$9/1000+R",formula4
        append      excelrow to formula4
        append      "*$R$10*$S$10)",formula4
        reset       formula4
...............................................................................
..=IF(g17=0,IF(N17>M17,f17*I17*N17/1000,f17*I17*M17/1000),
..IF(N17>M17,g17*I17*N17/1000,g17*I17*M17/1000))
..Formula5 = List Cost
..=IF(G15=0,IF(O15>N15,F15/1000*J15*O15,F15/1000*J15*N15),
........... IF(O15>N15,G15*J15*O15/1000,G15*J15*N15/1000))

.short and simple  Px/1000*Jx

        clear       formula5
        append      "=P",formula5
        append      excelrow to formula5
        append      "/1000*J",formula5
        append      excelrow to formula5
        reset       formula5
............................................................................
.formula6 Total Cost
.=SUM(W17:X17)
        clear       Formula6
        append      "=Sum(V",formula6
        append      excelrow to formula6
        append      ":U",formula6
        append      excelrow to formula6
        append      ")",formula6
        reset       formula6
............................................................................
.=V17-AB17
.formula7 Net +/-
        clear       Formula7
        append      "=T",formula7
        append      excelrow to formula7
        append      "-W",formula7
        append      excelrow to formula7
        reset       formula7
............................................................................
.=IF(P17>0,W7/R17,0)
.formula8 Cost/Gain per Mbr
        clear       Formula8
        append      "=IF(P",formula8
        append      excelrow to formula8
        append      ">0,x",formula8
        append      excelrow to formula8
        append      "/R",formula8
        append      excelrow to formula8
        append      ",0)",formula8
        reset       formula8
............................................................................
.=IF(O17>0,V17/O17*1000,0)
.formula9 Income per M
        clear       Formula9
        append      "=IF(R",formula9
        append      excelrow to formula9
        append      ">0,T",formula9
        append      excelrow to formula9
        append      "/P",formula9
        append      excelrow to formula9
        append      "*1000,0)",formula9
        reset       formula9
............................................................................
.=IF(P17>0,Y17/P17*1000,0)
.formul10 Cost per M
        clear       Formul10
        append      "=IF(P",formul10
        append      excelrow to formul10
        append      ">0,W",formul10
        append      excelrow to formul10
        append      "/P",formul10
        append      excelrow to formul10
        append      "*1000,0)",formul10
        reset       formul10
............................................................................
.=IF(V17>0,AB17/V17,0)
.formul11 Cost per $
        clear       Formul11
        append      "=IF(T",formul11
        append      excelrow to formul11
        append      ">0,W",formul11
        append      excelrow to formul11
        append      "/T",formul11
        append      excelrow to formul11
        append      ",0)",formul11
        reset       formul11
.***************************************************************************
.formula12 . cum vol
        clear      formul12
        if         (excelrow = "17")
        append     "=p17" to formul12
        reset      formul12
        else
        append     "=P" to formul12
        append     excelrow to formul12
        append     "+AC" to formul12
        CALL       SUBROW
        append     excelrow to formul12
        CALL       ADDROW
        reset      formul12
        endif
.***************************************************************************
.formula13 . cum RESP
        clear      formul13
        if         (excelrow = "17")
        append     "=R17" to formul13
        reset      formul13
        else
        append     "=R" to formul13
        append     excelrow to formul13
        append     "+AD" to formul13
        CALL       SUBROW
        append     excelrow to formul13
        CALL       ADDROW
        reset      formul13
        endif
.***************************************************************************
.formula14 . cum RR
        clear      formul14
        append     "=AD" to formul14
        append     excelrow to formul14
        append     "/AC" to formul14
        append     excelrow to formul14
        append     "*100" to formul14
        reset      formul14
.***************************************************************************
.formula15 . cum Rev
        clear      formul15
        if         (excelrow = "17")
        append     "=T17" to formul15
        reset      formul15
        else
        append     "=T" to formul15
        append     excelrow to formul15
        append     "+AF" to formul15
        CALL       SUBROW
        append     excelrow to formul15
        CALL       ADDROW
        reset      formul15
        endif
.***************************************************************************
.formula16 . cum Gift
        clear      formul16
        append     "=AF" to formul16
        append     excelrow to formul16
        append     "/AD" to formul16
        append     excelrow to formul16
        reset      formul16
.***************************************************************************
.formula17 . cum cOST
        clear      formul17
        if         (excelrow = "17")
        append     "=w17" to formul17
        reset      formul17
        else
        append     "=w" to formul17
        append     excelrow to formul17
        append     "+Ah" to formul17
        CALL       SUBROW
        append     excelrow to formul17
        CALL       ADDROW
        reset      formul17
        endif
.***************************************************************************
.formula18 . cum NET
        clear      formul18
        append     "=AF" to formul18
        append     excelrow to formul18
        append     "-AH" to formul18
        append     excelrow to formul18
        reset      formul18
.***************************************************************************
.formula19 . cum cpa
        clear      formul19
        append     "=(AF" to formul19
        append     excelrow to formul19
        append     "-AH" to formul19
        append     excelrow to formul19
        APPEND     ")/AD" TO FORMUL19
        append     excelrow to formul19
        reset      formul19
.***************************************************************************
        if          (averrr = 0)
        move        ".0085" to averrr
        endif
        if           (Percent = 0 )
        move         "75" to percent
        endif
        if            (avergft = 0)
        move          "15" to avergft
        endif
.***************************************************************************
.START PATCH 3.84 REPLACED LOGIC
.                   WRITE       OUTPUT,SEQ;*CDFON,ownocpy:
.                   lstnum:              .list number
.                   PLSTNAME:           .list name
.                      PLSTSLCT:         .select
.                      UNIVERSe:         .list univers
.                      "10000":         .reco qty
.                      "  0":          .qty approved
.                      RENTEXCH:             .r/e
.                      "100":               .list cost
.                      "100":               .actual cost
.                      "   ":           .maildate
.                      exbal:              .exchange status
.                      "00":                 .net requested
.                      "00":                .net approved
.                      percent1:            .average net
.                      formula1:            .Net Names
.                      averrr:             .Response rate
.                       formula2:           .Number of returns
.                       avergft:             .average gift
.                      Formula3:            .Total Income
.                      formula4:            .product cost
.                      formula5:            .List Cost
.                      Formula6:             .Total Cost
.                      Formula7:             .Net +/-
.                      Formula8:              .Cost MBR
.                      Formula9:            .INcome per M
.                      Formul10:           .Cost per M
.                      Formul11:           .Cost Per $
.                      Formul12:                .cumulative Vol
.                      Formul13:                .cumulative REsp
.                      Formul14:              .cumulative RR
.                      Formul15:                .cumulative  rev
.                      Formul16:                .cumulative gift
.                      Formul17:                .cumulative cost
.                      Formul18:                .cumulative net
.                      Formul19:                .cumulative CPA
.                      REVDATE:
.                      "0":                 .usage names
.                      "0":                 .usage orders
.                      "                  ":  .comments
.                      OWNOCPY:
.                      TEXT:
.                      avertcpm:
.                      avertmcst:
.                      lodate1:             .Results
.                      hidate1:             .      .Dates
.                      exstat
.............
                    unpack    REVDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                   WRITE       OUTPUT,SEQ;*CDFON,ownocpy:
                   lstnum:              .list number
                   PLSTNAME:           .list name
                      PLSTSLCT:         .select
                      UNIVERSe:         .list univers
                      "10000":         .reco qty
                      "  0":          .qty approved
                      RENTEXCH:             .r/e
                      "100":               .list cost
                      "100":               .actual cost
                      "   ":           .maildate
                      exbal:              .exchange status
                      "00":                 .net requested
                      "00":                .net approved
                      percent1:            .average net
                      formula1:            .Net Names
                      averrr:             .Response rate
                       formula2:           .Number of returns
                       avergft:             .average gift
                      Formula3:            .Total Income
                      formula4:            .product cost
                      formula5:            .List Cost
                      Formula6:             .Total Cost
                      Formula7:             .Net +/-
                      Formula8:              .Cost MBR
                      Formula9:            .INcome per M
                      Formul10:           .Cost per M
                      Formul11:           .Cost Per $
                      Formul12:                .cumulative Vol
                      Formul13:                .cumulative REsp
                      Formul14:              .cumulative RR
                      Formul15:                .cumulative  rev
                      Formul16:                .cumulative gift
                      Formul17:                .cumulative cost
                      Formul18:                .cumulative net
                      Formul19:                .cumulative CPA
                      STR10:
                      "0":                 .usage names
                      "0":                 .usage orders
                      "                  ":  .comments
                      OWNOCPY:
                      TEXT:
                      avertcpm:
                      avertmcst:
                      lodate1:             .Results
                      hidate1:             .      .Dates
                      exstat
.END PATCH 3.84 REPLACED LOGIC
         endif
         move         c0 to averrr
         move         c0 to avergft
         move         c0 to avertcpm
         move         c0 to averTMcst
         move         c0 to averTMcst
         move      c0 to statrr
         move      c0 to avgift
         move      c0 to tcpm
         move      c0 to tmcst
         move      c0 to lodate
         move      c0 to hidate
         move      c0 to exbal
         clear     lodate1
         clear     hidate1
         move      c0 to statcnt
         move      excelrow to n5      .increment row counter
         add       c1 to n5
         move      n5 to excelrow
         rep       zfill in excelrow
           RETURN
+............................................................................
SUBROW  MOVE       C0 TO N5
        MOVE       EXCELROW TO N5
        SUB        C1 FROM N5
        MOVE       N5 TO EXCELROW
        rep       zfill in excelrow
        RETURN
ADDROW  MOVE       C0 TO N5
        MOVE       EXCELROW TO N5
        ADD        C1 TO N5
        MOVE       N5 TO EXCELROW
        rep       zfill in excelrow
        RETURN
+............................................................................
.
. NEW MAILER. PRINT Details & TOTALS FOR OLD MAILER
.              Clear records etc.
.
NEWMLR
           BRANCH    TOTFLAG TO NEWMLRB
           COMPARE   C0 TO PAGE
           GOTO      NEWMLRB IF EQUAL
           CALL        getexch
           CALL      NOBAL
           CALL      MLRTOTLS
.
NEWMLRB  BRANCH    SRTFLAG TO BYNUM,BYALPH
BYNUM    MOVE      OMLRNUM TO HOLDMLR
         MOVE      OCOBN TO HOLDCNT
         UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
         PACK      MKEY WITH OMLRNUM,OCOBN
         CALL      NMLRKEY
         GOTO      MLRNG IF OVER
.START PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
          move      MCOMP,HoldMCOMP
.END PATCH 3.87 ADDED LOGIC - TEMPORARY PATCH
         GOTO      CHKBRK
BYALPH
.START PATCH 3.87 ADDED LOGIC 7/22/05
          MOVE      OMLRNUM TO HOLDMLR
          MOVE      OCOBN TO HOLDCNT
          PACK      MKEY WITH OMLRNUM,OCOBN
          CALL      NMLRKEY
          GOTO      MLRNG IF OVER
          move      MCOMP,HoldMCOMP
.END PATCH 3.87 ADDED LOGIC 7/22/05
         MOVE      ORDCNAME TO HOLDNAME
         MOVE      ORDCNAME TO MCOMP
         MOVE      ORDMNAME TO MNAME
         UNPACK    OODNUM INTO STR4,ORDOFR      .SET NEW OFFER ALSO
.
CHKBRK   MOVE      C1 TO BRKFLAG
         RESET     MCOMP
         SCAN      "C/O" IN MCOMP
         GOTO      CNTRMLR IF NOT EQUAL
         BUMP      MCOMP BY -1
         APPEND    B3 TO MCOMP
         RESET     MCOMP
         MOVE      C2 TO BRKFLAG
.
CNTRMLR  SETLPTR   MCOMP
         ENDSET    MCOMP
CHKMHEAD CMATCH    B1 TO MCOMP
         GOTO      SETMHEAD IF NOT EQUAL
         BUMP      MCOMP BY -1
         GOTO      CHKMHEAD IF NOT EOS
SETMHEAD MOVEFPTR  MCOMP TO N3
         MOVE      "5500" TO MLRTAB
         SUBTRACT  N3 FROM MLRTAB
         DIVIDE    C2 INTO MLRTAB
         RESET     MCOMP
         SETLPTR   MCOMP
         CALL      HEADING
         RETURN
.begin patch 3.8
MLRTOTLS
.need to get dates for header
.need to get low month for columns
               unpack         RepLoDate1 into str2
               move           str2 into n2
               for            n3 from "1" to "12"
               load           str4 from n3 of col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11,col12
               store          str4 into n2 of Mpos1,Mpos2,Mpos3,Mpos4,Mpos5,Mpos6,Mpos7,Mpos8,Mpos9,Mpos10,Mpos11,Mpos12
               if             (n2 = 12)
               move           c1 to n2
               else
               add            c1 to n2
               endif
               repeat
.print positions loaded lets go
               call           Printsubhead
.ok lets output the goodies
.               For            PrintREcCOunt from "1" to "300"
.               For            PrintREcCOunt from "1" to "500"
.begin patch 3.89
.               For            PrintREcCOunt from "1" to "700"
.end patch 3.89
.START PATCH 3.81 - ADDED LOGIC
.               For            PrintREcCOunt from "1" to "2000"
.end PATCH 3.81 - ADDED LOGIC
.begin patch 3.90
               For            PrintREcCOunt from "1" to "1100"
.end patch 3.90
               if             (PrintRec(PrintRecCount).PrintListName = "" or PrintRec(PrintRecCount).PrintListName ="  ")
               Break
               endif
.begin patch 3.93
          add       sixlpi,Row
          PrtPage   Laser;*Alignment=*Right,*p=1:row,PrintRec(PrintRecCount).PrintListName:
                    *p=Mpos1:row,PrintRec(PrintRecCount).PrintM1:
                    *p=Mpos2:row,PrintRec(PrintRecCount).PrintM2:
                    *p=Mpos3:row,PrintRec(PrintRecCount).PrintM3:
                    *p=Mpos4:row,PrintRec(PrintRecCount).PrintM4:
                    *p=Mpos5:row,PrintRec(PrintRecCount).PrintM5:
                    *p=Mpos6:row,PrintRec(PrintRecCount).PrintM6:
                    *p=Mpos7:row,PrintRec(PrintRecCount).PrintM7:
                    *p=Mpos8:row,PrintRec(PrintRecCount).PrintM8:
                    *p=Mpos9:row,PrintRec(PrintRecCount).PrintM9:
                    *p=Mpos10:row,PrintRec(PrintRecCount).PrintM10:
                    *p=Mpos11:row,PrintRec(PrintRecCount).PrintM11:
                    *p=Mpos12:row,PrintRec(PrintRecCount).PrintM12:
                    *Alignment=*Left:
                    *p=col12:row,b10,b1,PrintRec(PrintRecCount).Printperc,"%"
          add       sixlpi,Row
          PrtPage   Laser;*p=1:row,PrintRec(PrintRecCount).PrintSelect
          add       sixlpi,Row
           if          (PrintRec(PrintRecCount).PrintOwnocpy <> "")               ;if we have processd all valid records - exit
          PrtPage   Laser;*p=1:row,"## ",PrintRec(PrintRecCount).PrintList,B1,PrintRec(PrintRecCount).PrintDCTExt," Updated: ",PrintRec(PrintRecCount).PrintRevDate:
                    " Owner: ",PrintRec(PrintRecCount).PrintOwnoCpy;
.          print              " Owner: ",PrintRec(PrintRecCount).PrintOwnoCpy;
          Else
          PrtPage   Laser;*p=1:row,"## ",PrintRec(PrintRecCount).PrintList,B1,PrintRec(PrintRecCount).PrintDCTExt," Updated: ",PrintRec(PrintRecCount).PrintRevDate
          endif

          CMATCH      B1 TO PrintRec(PrintRecCount).PrintExstat
                     IF          EOS
                    Else          
                    add       sixlpi,Row
                    PrtPage   Laser;*p=1:row,PrintRec(PrintRecCount).PrintExstat
                    add       sixlpi,Row
                    endif
.          PRINT     *N,*01,PrintRec(PrintRecCount).PrintListName:
.                      *Mpos1,PrintRec(PrintRecCount).PrintM1:
.                      *Mpos2,PrintRec(PrintRecCount).PrintM2:
.                      *Mpos3,PrintRec(PrintRecCount).PrintM3:
.                      *Mpos4,PrintRec(PrintRecCount).PrintM4:
.                      *Mpos5,PrintRec(PrintRecCount).PrintM5:
.                      *Mpos6,PrintRec(PrintRecCount).PrintM6:
.                      *Mpos7,PrintRec(PrintRecCount).PrintM7:
.                      *Mpos8,PrintRec(PrintRecCount).PrintM8:
.                      *Mpos9,PrintRec(PrintRecCount).PrintM9:
.                      *Mpos10,PrintRec(PrintRecCount).PrintM10:
.                      *Mpos11,PrintRec(PrintRecCount).PrintM11:
.                      *Mpos12,PrintRec(PrintRecCount).PrintM12:
.                      *col12,b10,b1,PrintRec(PrintRecCount).Printperc,"%":
.                      *N,*1,PrintRec(PrintRecCount).PrintSelect;
.           PRINT     *N,*1,"## ",PrintRec(PrintRecCount).PrintList,B1,PrintRec(PrintRecCount).PrintDCTExt," Updated: ",PrintRec(PrintRecCount).PrintRevDate;
.           if          (PrintRec(PrintRecCount).PrintOwnocpy <> "")               ;if we have processd all valid records - exit
.           print              " Owner: ",PrintRec(PrintRecCount).PrintOwnoCpy;
.           endif
.               ADD         C4 TO PRTLINES
.           CMATCH      B1 TO PrintRec(PrintRecCount).PrintExstat
.                     IF          EOS
.                     PRINT     B1
.                     Else
.               PRINT       *N,*1,PrintRec(PrintRecCount).PrintExstat
.               ADD       C1 TO PRTLINES
.                     ENDIF
               Move           Exstat to PrintRec(PrintRecCount).PrintExstat
.               COMPARE   C59 TO PRTLINES
.               if             Not Less
          If        (Row > 7100)
               call           Heading
               call           PrintSubhead
.              CALL      HEADING IF NOT LESS
               endif
               Repeat
.               COMPARE   C59 TO PRTLINES
.               if             Not Less
          If        (Row > 7100)
               call           Heading
               call           PrintSubhead
.              CALL      HEADING IF NOT LESS
               endif
         MOVE      NUMMASKB TO NUMPRT1B
         EDIT      TORDQTY  TO NUMPRT1B
         MOVE      NUMMASKB TO NUMPRT2B
         MOVE      NUMMASKB TO NUMPRT3B
         EDIT      TORDQTYE TO NUMPRT3B
         MOVE      NUMMASKB TO NUMPRT4B
         EDIT      TORDQTYR TO NUMPRT4B
         COMPARE   C0 TO TORDQTY
.begin patch 3.82
               Goto           TotDone if equal
.end patch 3.82
.         RETURN    IF EQUAL
.end patch 3.8
.NEWTOT1  BRANCH    LOTSFLAG TO NEWTOT1A,NEWTOT2
.Start Patch 3.4 - corrected printing problems
..Start Patch #3.3 - increased vars
..NEWTOT1A PRINT     *N,*rptchar "_":163,*n,*47,T1:
..                      *56,T2:
..                      *65,T3:
..                      *74,T4:
..                      *83,T5:
..                      *92,T6:
..                      *101,T7:
..                      *110,T8:
..                      *119,T9:
..                      *129,T10:
..                      *138,T11:
..                      *147,T12
.NEWTOT1A PRINT     *N,*rptchar "_":163,*n,*47,T1:
.                      *58,T2:
.                      *69,T3:
.                      *80,T4:
.                      *91,T5:
.                      *102,T6:
.                      *113,T7:
.                      *124,T8:
.                      *135,T9:
.                      *147,T10:
.                      *158,T11:
.                      *169,T12
.begin patch 3.8
.NEWTOT1A PRINT     *N,*rptchar "_":173,*n,*mpos1,T1:
.NEWTOT1A PRINT                *N,*rptchar "_":173:
.                              *n,*mpos1,"   JANUARY":
.                              *mpos2,"  FEBRUARY":
.                              *mpos3,"     MARCH":
.                              *mpos4,"     APRIL":
.                              *mpos5,"       MAY":
.                              *mpos6,"      JUNE":
.                              *mpos7,"      JULY":
.                              *mpos8,"    AUGUST":
.                              *mpos9," SEPTEMBER":
.                              *mpos10,"   OCTOBER":
..                              *mpos11,"  NOVEMBER":
.                              *mpos12,"  DECEMBER":
..                              *N,*rptchar "-":173:
.                              *n,*mpos1,T1:
.                              *mpos2,T2:
.                              *mpos3,T3:
.                              *mpos4,T4:
.                              *mpos5,T5:
.                              *mpos6,T6:
.                              *mpos7,T7:
.                              *mpos8,T8:
.                              *mpos9,T9:
.                              *mpos10,T10:
.                              *mpos11,T11:
.                              *mpos12,T12
NewTot1a  add       Sixlpi,row
          Prtpage   Laser;*p=1:row,*rptchar "_":200
          add       Sixlpi,row
          Prtpage   Laser;*Alignment=*Right,*p=mPos1:row,"January":
                    *p=mpos2:row,"February":
                    *p=mpos3:row,"March":
                    *p=mpos4:row,"April":
                    *p=mpos5:row,"May":
                    *p=mpos6:row,"June":
                    *p=mpos7:row,"July":
                    *p=mpos8:row,"August":
                    *p=mpos9:row,"September":
                    *p=mpos10:row,"October":
                    *p=mpos11:row,"November":
                    *p=mpos12:row,"December",*ALIGNMENT=*Left
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,*rptchar "-":500
          add       Sixlpi,row
          Prtpage   Laser;*Alignment=*Right,*p=mpos1:row,T1:
                    *p=mpos2:row,T2:
                    *p=mpos3:row,T3:
                    *p=mpos4:row,T4:
                    *p=mpos5:row,T5:
                    *p=mpos6:row,t6:
                    *p=mpos7:row,T7:
                    *p=mpos8:row,T8:
                    *p=mpos9:row,T9:
                    *p=mpos10:row,T10:
                    *p=mpos11:row,T11:
                    *p=mpos12:row,T12:
                    *Alignment=*Left
          add       Sixlpi,row
.End Patch #3.3 - increased var
.End Patch 3.4 - corrected printing problems
NEWTOT1B  MOVE          C0 TO TORDNUM
            ADD         TORDNUME TO TORDNUM
            ADD         TORDNUMR TO TORDNUM
            ADD         TORDNUMS TO TORDNUM
.         COMPARE   C55 TO PRTLINES
          If        (Row > 7100)
          Call      Heading
          endif
.         CALL      HEADING IF NOT LESS
.            PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL           ORDERS: ",TORDNUMR:
.                      *63,"RENTAL             USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX                  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL ORDERS: ",TORDNUM:
.                      *63,"TOTAL                               USAGE:":
.                      *81,NUMPRT1B
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,"EXCHANGE ORDERS: ",TORDNUME:
                    *p=3750:row,"EXCHANGE USAGE:",*p=5000:row,NUMPRT3B
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,"RENTAL ORDERS: ",TORDNUMR:
                    *p=3750:row,"RENTAL USAGE:",*p=5000:row,NUMPRT4B
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,"RENT/EX ORDERS: ",TORDNUMS
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,"TOTAL ORDERS: ",TORDNUM:
                    *p=3750:row,"TOTAL USAGE:",*p=5000:row,NUMPRT1B
          add       Sixlpi,row
          Prtpage   Laser;*p=1:row,CopyRightS,"1992-2016, Names in the News"
          add       Sixlpi,row
          add       Sixlpi,row
          add       Sixlpi,row
          add       Sixlpi,row
          add       Sixlpi,row
          add       Sixlpi,row
.            PRINT     *N,*1,"EXCHANGE ORDERS: ",TORDNUME:
.                      *63,"EXCHANGE USAGE:",*81,NUMPRT3B:
.                   *N,*1,"RENTAL   ORDERS: ",TORDNUMR:
.                      *63,"RENTAL   USAGE:",*81,NUMPRT4B:
.                   *N,*1,"RENT/EX  ORDERS: ",TORDNUMS:
.                   *N,*1,"TOTAL    ORDERS: ",TORDNUM:
.                      *63,"TOTAL    USAGE:":
.                      *81,NUMPRT1B
.START PATCH 3.86 REPLACED LOGIC
.         print     *1,CopyRightS,"1992-2003, Names in the News/CA"
.         print     *1,CopyRightS,"1992-2007, Names in the News"
.END PATCH 3.86 REPLACED LOGIC
         ADD       C6 TO PRTLINES
           BRANCH    LOTSFLAG TO TOTDONE,NEWTOT2
           GOTO      TOTDONE
NEWTOT2    CLEAR     PLSTNAME
            WRITE     OUTPUT,SEQ;"#"",PLSTNAME,"#",":
                          T1,",":
                      T2,",":
                      T3,",":
                      T4,",":
                      T5,",":
                      T6,",":
                      T7,",":
                      T8,",":
                      T9,",":
                      T10,",":
                      T11,",":
                      T12,","
            MOVE        C0 TO TORDNUM
            ADD         TORDNUME TO TORDNUM
            ADD         TORDNUMR TO TORDNUM
            ADD         TORDNUMS TO TORDNUM
           WRITE       OUTPUT,SEQ;B1
           WRITE     OUTPUT,SEQ;"#"EXCHANGE ORDERS: #",",TORDNUME:
                      ",",TORDQTYE
           WRITE     OUTPUT,SEQ;"#"RENTAL   ORDERS: #",",TORDNUMR:
                      ",",TORDQTYR,","
           WRITE     OUTPUT,SEQ;"#"RENT/EX  ORDERS: #",",TORDNUMS,","
           WRITE     OUTPUT,SEQ;"#"TOTAL    ORDERS: #",",TORDNUM:
                      ",",TORDQTY
         ADD       C5 TO PRTLINES
           GOTO      TOTDONE

TOTDONE  MOVE      C0 TO TORDQTY
         MOVE      C0 TO TORDQTYE
         MOVE      C0 TO TORDQTYR
         MOVE      C0 TO TORDNUM
         MOVE      C0 TO TORDNUMR
         MOVE      C0 TO TORDNUME
         MOVE      C0 TO TORDNUMS
           MOVE        C0 TO T1
           MOVE        C0 TO T2
           MOVE        C0 TO T3
           MOVE        C0 TO T4
           MOVE        C0 TO T5
           MOVE        C0 TO T6
           MOVE        C0 TO T7
           MOVE        C0 TO T8
           MOVE        C0 TO T9
           MOVE        C0 TO T10
           MOVE        C0 TO T11
           MOVE        C0 TO T12
           MOVE        C0 TO PAGE     *RESET PAGE AT MLR BREAK DLH 8/27/92
           move        yes to mlrbreak
           MOVE      C1 TO N9
.begin patch 3.8
.               For            PrintREcCOunt from "1" to "500"
.begin patch 3.89
.               For            PrintREcCOunt from "1" to "700"
.end patch 3.89
.START PATCH 3.81 - ADDED LOGIC
.               For            PrintREcCOunt from "1" to "2000"
.end PATCH 3.81 - ADDED LOGIC
.begin patch 3.90
               For            PrintREcCOunt from "1" to "1100"
.end patch 3.90
               Clear          PrintRec(PrintRecCount).PrintListName
               Move           c0,PrintRec(PrintRecCount).PrintM1
               Move           c0,PrintRec(PrintRecCount).PrintM2
               Move           c0,PrintRec(PrintRecCount).PrintM3
               Move           c0,PrintRec(PrintRecCount).PrintM4
               Move           c0,PrintRec(PrintRecCount).PrintM5
               Move           c0,PrintRec(PrintRecCount).PrintM6
               Move           c0,PrintRec(PrintRecCount).PrintM7
               Move           c0,PrintRec(PrintRecCount).PrintM8
               Move           c0,PrintRec(PrintRecCount).PrintM9
               Move           c0,PrintRec(PrintRecCount).PrintM10
               Move           c0,PrintRec(PrintRecCount).PrintM11
               Move           c0,PrintRec(PrintRecCount).PrintM12
               Move           c0,PrintRec(PrintRecCount).Printperc
               Clear          PrintRec(PrintRecCount).PrintSelect
               Clear          PrintRec(PrintRecCount).PrintList
               Clear          PrintRec(PrintRecCount).PrintDCTExt
               Clear          PrintRec(PrintRecCount).PrintRevDate
               Clear          PrintRec(PrintRecCount).PrintOwnocpy
               Clear          PrintRec(PrintRecCount).PrintExstat
               Repeat
.begin patch 3.82
               MOve           c0 to printreccount
.end patch 3.82
.end patch 3.8
               RETURN
.
.START PATCH 3.6 - REPLACED LOGIC, OODES --> OFDESC
.NEWOFR   MOVE      ORDOFR TO HOLDOFR
.         SETLPTR   OODES
.         ENDSET    OODES
.CHKOHEAD CMATCH    B1 TO OODES
.         GOTO      SETOHEAD IF NOT EQUAL
.         BUMP      OODES BY -1
.         GOTO      CHKOHEAD IF NOT EOS
.SETOHEAD MOVEFPTR  OODES TO N3
.         MOVE      "132" TO OFRTAB
.         SUBTRACT  N3 FROM OFRTAB
.         DIVIDE    C2 INTO OFRTAB
.         RESET     OODES
.         SETLPTR   OODES
.         RETURN
NEWOFR   MOVE      ORDOFR TO HOLDOFR
         SETLPTR   OFDESC
         ENDSET    OFDESC
CHKOHEAD CMATCH    B1 TO OFDESC
         GOTO      SETOHEAD IF NOT EQUAL
         BUMP      OFDESC BY -1
         GOTO      CHKOHEAD IF NOT EOS
SETOHEAD MOVEFPTR  OFDESC TO N3
         MOVE      "132" TO OFRTAB
         SUBTRACT  N3 FROM OFRTAB
         DIVIDE    C2 INTO OFRTAB
         RESET     OFDESC
         SETLPTR   OFDESC
         RETURN
.END PATCH 3.6 - REPLACED LOGIC, OODES --> OFDESC
.
EOJ
.PATCH3.83
          if ((RepLodate = c0) and (REPHiDate = C0))
           display   *p1:22,*el,"No valid records for this Y report "
         shutdown  "cls"
                    STOP
          endif
.ENDPATCH3.83
         CALL      getexch
         call      calcnet
         call      getstat
           CALL        NOBAL
           BRANCH    TOTFLAG TO EOJB
         CALL      MLRTOTLS
.EOJB     BRANCH    LOTSFLAG TO EOJC,EOJD
EOJB
.          print     033,"M","@"                      .reset to defaults.
.         print     hpport,hpreset
         BRANCH    PRTFLAG TO END
          PrtClose  Laser
.patch
.Begin patch  4.01
.          if        (PDFFLAG = "2")
          if        (PDFFLAG = "2" & Lotsflag = "2")
                    WEOF      OUTPUT,SEQ
                    Close     output          
                    Move      outname1,Str55
                    Move      outname1,Str45
                    pack      mailattach from mailattach,";",outname1
                    goto      checkfile
          elseif   (PDFFLAG = "2")          

.begin patch 4.0
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
                    Move      MailAttach,Str55
.end patch 4.0
                    Move      MailAttach,Str45
                    goto      checkfile
           Elseif (Lotsflag = "2")
                    WEOF      OUTPUT,SEQ
                    Close     output          
                    Move      outname1,Str55
                    Move      outname1,Str45
                    pack      mailattach from outname1
.end patch  xxx
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck



.Begin patch  xxx
.          move    "Here is your List Summary PDF File",MailSubjct
          if        (PDFFLAG = "2" & Lotsflag = "2")
          move    "Here is your List Summary PDF & CVS Files",MailSubjct
          elseif    (PDFFLAG = "2" )
          move    "Here is your List Summary PDF File",MailSubjct
          elseif    (lotsFLAG = "2" )
          move    "Here is your List Summary CSV File",MailSubjct
          endif

.end patch  xxx
          Clear     MailBody
.          append    "lstsum.pdf",Mailbody
          append    Mailattach,Mailbody
          append    CRLF,Mailbody
          reset     Mailbody
          pack      Mailto from user,"@nincal.com"
          pack      MailFrom from user,"@nincal.com"
.Begin patch  4.01
.          pack      MailAttach from "c:\work\pdf\",prtname,".pdf"
.end patch  4.01
          call      SendMail
.Clean up afterwards
          pause c7
.begin patch 4.01
          if        (PDFFLAG = "2")
          pack      str45 from "c:\work\pdf\",prtname,".pdf"
          erase     str45
          endif
          if        (LotsFLAG = "2")
          erase     outname1
          endif
.          erase     mailattach
.end patch 4.01
.begin patch 4.0
.          Call      PDF995Auto0
.end patch 4.0
          endif

.         SPLCLOSE
.         CALL      REMVTOF
.begin patch xxx
.         BRANCH    LOTSFLAG TO END,EOJC
         GOTO      END
.EOJC     WEOF      OUTPUT,SEQ
.           GOTO      END
.end patch xxx
.
HEADING  ADD       C1 TO PAGE
.          BRANCH    LOTSFLAG TO HD1,HD2
.START PATCH 3.86 REPLACED LOGIC
.HD1      PRINT     *F,*n,*n,"CONFIDENTIAL":
.                      *68,"NAMES IN THE NEWS CA., INC.":
.                      *161,"DATE: ",TODAY
HD1
.begin patch 3.91
.          If        (Company = c2)
.          PRINT     *F,*n,*n,"CONFIDENTIAL":
.                      *68,"Pacific Lists, Inc.":
.                      *161,"DATE: ",TODAY
.          else
.          PRINT     *F,*n,*n,"CONFIDENTIAL":
.                      *68,"NAMES IN THE NEWS":
.                      *161,"DATE: ",TODAY
.          endif
.          PrtPage   Laser;*NewPage,*Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
          move      c1,row
          if        (page = c1 & MLRBREAK = no)
          PrtPage   Laser;*Pictrect=*off,*PICT=0:800:0:5000:NINLogo:
                   *p=9500:1,"Confidential"
          ElseIf   (page = c1 & MLRBREAK = yes)
          PrtPage   Laser;*NewPage,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo:
                   *p=9500:1,"Confidential"
          else
          PrtPage   Laser;*NewPage,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo:
                   *p=9500:1,"Confidential"
          endif
          add       Sixlpi,row
          PrtPage   Laser;*p=9500:Row,"DATE: ",TODAY
          add       Sixlpi,row
          PrtPage   Laser;*p=9500:Row,"PAGE: ",Page
          move      "750",row
          PrtPage   Laser;*p4125:row,"LISTS ORDERED BY:"
.end patch 3.91
.END PATCH 3.86 REPLACED LOGIC
.                      *141,"DATE: ",TODAY
         BRANCH    BRKFLAG OF HD1MLR,HD1BRK
.
HD1MLR
.Start Patch #3.3 - added century
.         PRINT     *N,*01,"(",HOLDMLR,")":
.                      *70,"LISTS ORDERED BY MAILER:":
.                      *141,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*MLRTAB,MCOMP:
.                       *N,*75,"YEAR: ",OMDTEY;
          add       Sixlpi,Row
.          PrtPage   Laser;*p=mlrtab:row,HoldMcomp                    
          PrtPage   Laser;*p=1:row,"(",HOLDMLR,")",*p=4125:row,HoldMcomp                    
          add       Sixlpi,Row
.         PRINT     *N,*01,"(",HOLDMLR,")":
.                      *70,"LISTS ORDERED BY MAILER:":
.                      *161,"PAGE:    ",PAGE:
..                      *141,"PAGE:    ",PAGE:
.                   *N:
.begin patch 3.8
.START PATCH 3.87 REPLACED LOGIC
.                   *N,*MLRTAB,MCOMP
.                   *N,*MLRTAB,HoldMCOMP
.END PATCH 3.87 REPLACED LOGIC
.                   *N,*MLRTAB,MCOMP:
.                       *N,*75,"YEAR: ",OMDTEC,OMDTEY;
.End Patch #3.3 - added century
.end patch 3.8
.         MOVE      C8 TO PRTLINES
         GOTO      HD1A
.
HD1BRK
.Start Patch #3.3 - added century
.         PRINT     *N:
.                      *68,"LISTS ORDERED THRU BROKER:":
.                      *141,"PAGE:    ",PAGE:
.                   *N:
.                   *N,*MLRTAB,MCOMP:
.                       *N,*75,"YEAR: ",OMDTEY;
          add       Sixlpi,Row
          PrtPage   Laser;*p=1:row,"(",HOLDMLR,")",*p=4125:row,Mcomp                    
          add       Sixlpi,Row
.         PRINT     *N:
.                      *68,"LISTS ORDERED THRU BROKER:":
.                      *161,"PAGE:    ",PAGE:
..                      *141,"PAGE:    ",PAGE:
.                   *N:
.begin patch 3.8
.                   *N,*MLRTAB,MCOMP
.                   *N,*MLRTAB,MCOMP:
.                       *N,*75,"YEAR: ",OMDTEC,OMDTEY;
.End Patch #3.3 - added century
.end patch 3.8
.         MOVE      C7 TO PRTLINES
.
.Start Patch 3.4 - corrected printing problems
.this patch is null as I forgot to save the original (but wrong) values :(
.however, the new ones work fine.  ASH
.Start Patch #3.3 - added century
.HD1A     PRINT     *N,*N:
.                   *N,*06,"LIST ":
.                      *48,"JANUARY":
.                      *56,"FEBRUARY":
.                      *68,"MARCH":
.                      *77,"APRIL":
.                      *88,"MAY":
.                      *96,"JUNE":
.                      *105,"JULY":
.                      *112,"AUGUST":
.                      *119,"SEPTEMBER":
.                      *130,"OCTOBER":
.                      *138,"NOVEMBER":
.                      *147,"DECEMBER":
.                      b1,"NET OUT"
HD1A
.begin patch 3.8
.               PRINT     *N,*N:
.                   *N,*06,"LIST ":
.                      *49,"JANUARY":
.                      *58,"FEBRUARY":
.                      *71,"MARCH":
.                      *81,"APRIL":
.                      *93,"MAY":
.                      *102,"JUNE":
.                      *112,"JULY":
.                      *119,"AUGUST":
.                      *127,"SEPTEMBER":
.                      *139,"OCTOBER":
.                      *148,"NOVEMBER":
.                      *158,"DECEMBER":
.                      b1,"NET OUT"
.end patch 3.8
.End Patch #3.3 - added century
.End Patch 3.4 - corrected printing problems
.         ADD       C3 TO PRTLINES
         UNPACK    OODNUM INTO STR4,ORDOFR
         MOVE      B4 TO STR4
         CALL      NEWOFR
.          GOTO      HDEXIT
           BRANCH    LOTSFLAG TO HDEXIT,HD2
HD2      COMPARE   C1 TO PAGE
           GOTO      HDEXIT IF NOT EQUAL
.START PATCH 3.86 REPLACED LOGIC
.           WRITE     OUTPUT,SEQ;"#"CONFIDENTIAL#",",B12,B12:
.                      "#"NAMES IN THE NEWS CA., INC.#,":
.                      B1,",",B12,"#"DATE: #",",TODAY
.begin patch 3.91
          If        (Company = c2)
           WRITE     OUTPUT,SEQ;"#"CONFIDENTIAL#",",B12,B12:
                      "#"PACIFIC LISTS#,":
                      B1,",",B12,"#"DATE: #",",TODAY                   
          else
           WRITE     OUTPUT,SEQ;"#"CONFIDENTIAL#",",B12,B12:
                      "#"NAMES IN THE NEWS#,":
                      B1,",",B12,"#"DATE: #",",TODAY
          endif
.end patch 3.91

.END PATCH 3.86 REPLACED LOGIC
         BRANCH    BRKFLAG OF HD2MLR,HD2BRK
.
HD2MLR   WRITE         OUTPUT,SEQ;"#"(",HOLDMLR,")#",":
                      "#"LISTS ORDERED BY MAILER:#""
         WRITE     OUTPUT,SEQ;B1,",,,,,,,,,,,,","Cost Assumptions"
         WRITE     OUTPUT,SEQ;B1,",,,,,,,,,,,,","#"Printing#",","232.00"
.START PATCH 3.87 REPLACED LOGIC
.         WRITE     OUTPUT,SEQ;"#"   #",","#"",MCOMP,"#""
         WRITE     OUTPUT,SEQ;"#"   #",","#"",HoldMCOMP,"#""
.END PATCH 3.87 REPLACED LOGIC
         GOTO      HD2A
.
HD2BRK   WRITE     OUTPUT,SEQ;"#"LISTS ORDERED THRU BROKER:#","
         WRITE     OUTPUT,SEQ;B1
         WRITE     OUTPUT,SEQ;B1
         WRITE     OUTPUT,SEQ;"#"  #",","#"",MCOMP,"#""
.
HD2A     WRITE         OUTPUT,SEQ;B1
           WRITE     OUTPUT,SEQ;B1
         compare   c1 to nwfsflag
         if         equal
           WRITE     OUTPUT,SEQ;B6,"#"",str12,str12,"#",","#"",str12,"#"",",#"LIST #",":
                      "#"JANUARY  #",":
                      "#"FEBRUARY #",":
                      "#"MARCH    #",":
                      "#"APRIL    #",":
                      "#"MAY      #",":
                      "#"JUNE     #",":
                      "#"JULY     #",":
                      "#"AUGUST   #",":
                      "#"SEPTEMBER#",":
                      "#"OCTOBER  #",":
                      "#"NOVEMBER #",":
                      "#"DECEMBER #",":
                      "#"NET OUT  #",":
                      "#"SELECT   #",":
                          "#"Text     #",":
                          "#"Comment  #",":
                          "#"EXSTAT   #",":
                          "#"REV. DATE#",":
                          "#"OWNER    #",":
                      "#"Gross    #",":
                      "#"Per Dupes#",":
                      "#"Net Names#",":
                      "#"% Returns#",":
                      "#"Aver Gift#",":
                      "#"Income   #",":
                      "#"Cost/mbr #",":
                      "#"Mail Cost#""
         ELSE
                   write       output,seq;*cdfon," "," "            .08
                   write       output,seq;*cdfon,b1," "             .09
                   write       output,seq;*cdfon,b1,"   "           .10
                   write       output,seq;*cdfon,b1,""              .11
                   write       output,seq;*cdfon,b1," "             .12
                   write       output,seq;*cdfon,b1,"   "           .13
.
                   WRITE       OUTPUT,SEQ;*CDFON,"Owner":
                   "List ##":              .list number
                   "List":           .list name
                   "Select":         .select
                   "Universe":         .list univers
                   "Reco Qty":         .reco qty
                   "Qty Aprvd":          .qty approved
                      "R/E":             .r/e
                      "List Cost":               .list cost
                      "Act Cost":               .actual cost
                      "MailDate":           .maildate
                      "Exch Stat":              .exchange status
                      "Net Req":                 .net requested
                      "Net Appr":                 .net approved
                      "aver Net":            .average net
                      "Net Names":            .Net Names
                      "Resp R":             .Response rate
                       "Rtns":           .Number of returns
                       "Gift":             .average gift
                      "Income":            .Total Income
                      "Prod Cost":            .product cost
                      "Lst Cost":            .List Cost
                      "Tot Cst":             .Total Cost
                      "Net +/-":             .Net +/-
                      "Cost Mbr":              .Cost MBR
                      "Inc  M":            .INcome per M
                      "Cost/M":           .Cost per M
                      "Cost?$":           .Cost Per $
                      "Cum Vol":
                      "Cum Resp":
                      "Cum R/R":
                      "Cum Rev":
                      "Cum Gift":
                      "Cum Cst":
                      "Cum Net":
                      "Cum CPA":
                      "Lst upd":
                      "Usage Names":
                      "Usage ##ORDS":
                      "Comments":
                      "Owner":
                      "list INfo":
                      "avertcpm":
                      "avertmcst":
                      "Results":             .Results
                      "       ":             .      .Dates
                      "exstat"
                   write       output,seq;*cdfon,b1,"   "           .15
                   write       output,seq;*cdfon,b1,"   "           .16
         endif
         move      "17" to excelrow
         UNPACK    OODNUM INTO STR4,ORDOFR
         MOVE      B4 TO STR4
         CALL      NEWOFR

HDEXIT   RETURN
.
MLRNG    KEYIN     *P01:24,*EL,*B,"Invalid mailer number: ",*DV,OMLRNUM:
                                  " in LR: ",*DV,OLRN,*CL,STR1
         GOTO      END
Help     trapclr   f1
.          execute   "c:\progra~1\plus!\micros~1\iexplore.exe file:\\nts2\wc5\http\help.htm"
          if         over                .Failed
.          execute   "c:\progra~1\Intern~1\iexplore.exe file:\\nts2\wc5\http\help.htm"
          endif
          trap      help if f1
          return

prtbanr 
.Print      *l,*rptchar "*":80:
.                    *l,*N:
.                    "User : ",user,*l,*n:
.                    "Program : ",program,*n:
.                    "Date: ",today,*l,*n:
.                    "Input file : ",inpname,*l,*n:
.                    "Copies : ":
.                    *n,*rptchar "*":80:
.                    *f
         return
.begin patch 3.8
PrintSubhead
          add       Sixlpi,row
          PrtPage   Laser;*p=3750:row,"Maildates from :",REpLodate1," to ",REpHiDate1     
          add       Sixlpi,row
          add       Sixlpi,row
          
.               Print          *n,*62,"Maildates from :",REpLodate1," to ",REpHiDate1
.               PRINT     *N,*N:
          Prtpage   Laser;*p=325:row,"LIST":
                    *Alignment=*Right:
                    *p=mPos1:row,"January":
                    *p=mpos2:row,"February":
                    *Alignment=*Right:
                    *p=mpos3:row,"March":
                    *p=mpos4:row,"April":
                    *p=mpos5:row,"May":
                    *p=mpos6:row,"June":
                    *p=mpos7:row,"July":
                    *p=mpos8:row,"August":
                    *p=mpos9:row,"September":
                    *p=mpos10:row,"October":
                    *p=mpos11:row,"November":
                    *p=mpos12:row,"December",*ALIGNMENT=*LEFT:
                    *p=col12:row,"            Net out"

.                   *N,*06,"LIST ":
.                      *mpos1,"   JANUARY":
.                      *mpos2,"  FEBRUARY":
.                      *mpos3,"     MARCH":
.                      *mpos4,"     APRIL":
.                      *mpos5,"       MAY":
.                      *mpos6,"      JUNE":
.                      *mpos7,"      JULY":
.                      *mpos8,"    AUGUST":
.                      *mpos9," SEPTEMBER":
.                      *mpos10,"   OCTOBER":
.                      *mpos11,"  NOVEMBER":
.                      *mpos12,"  DECEMBER":
.                      *col12,"            NET OUT"
.               Add            C1 TO PRTLINES
          add       Sixlpi,row
               return
.end patch 3.8
+.........................................................................
PrintPrep
          call      GetWinVer
          pack      Str45,"\\nins1\e\data\",prtfile                              ."
.begin dh test
                    rep       lowup,USer
                    if        (pdfflag = "2")
.begin patch 4.0
.                              call       GetPDFPath
.                              Call      PDF995Auto
.                              call      SetPDFFlag
                              clear     str45
.                              pack      str45 from prtname,".pdf"
                              pack      str45 from "c:\work\pdf\",prtname,".pdf"
                              pack      Mailattach from str45
.                              PRTOPEN Laser,"PDF995",str45
                              PRTOPEN Laser,"PDF:",str45,Flags=PDF_FLAGS_WIN_ANSI_ENCODING
.end patch 4.0
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Print to PDF"
                              return
                    else

                    if        (user = "SANSTRA")
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Susan's Printer"
                           if    (osflag <> c0)
.                          if (osflag = c2)         .nt
                                  PRTOPEN Laser,"Kyocera FS-1030D",Str45
.                          elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
.                                  PRTOPEN Laser,"Kyocera FS-1030D",Str45
.                          elseif (osflag = c1)         .win 95 98
.                                  PRTOPEN Laser,"KYOCERAS",Str45
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN Laser,"@",Str45
                          endif
                    Elseif        (user = "SMCGUIR")
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Suzie's Printer"
.                          if (osflag = c2)         .nt
                           if    (osflag <> c0)
                                  PRTOPEN Laser,"Kyocera FS-1030D KX",Str45
./                          elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
./                                  PRTOPEN Laser,"Kyocera FS-1030D KX",Str45
./                          elseif (osflag = c1)         .win 95 98
./                                  PRTOPEN Laser,"FSD-1030D",Str45
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN Laser,"@",Str45
                          endif
                    Elseif        (user = "DHERRIC")
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," David's Printer"
.                          if (osflag = c2)         .nt +
.                                  PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
.                          elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                           if    (osflag <> c0)
                                  PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45

.                          elseif (osflag = c1)         .win 95 98
.                                  PRTOPEN Laser,"KYOCERAM",Str45
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN Laser,"@",Str45
                          endif
                    ElseIF (PRTFLAG = "2")  .Sales
.                    if (PRTFLAG = "2")  .Sales
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser3"

                              if (OSFLAG >= C6)
                                        PRTOPEN   Laser,"\\NINs2\Laser3 Blankstock",Str45
.                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
.                                        PRTOPEN   Laser,"Laser3 Blankstock",Str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",Str45
                              endif
                    elseif (PRTFLAG = "3")                            .All others
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser2"
                              if (OSFLAG >= C6)
                                        PRTOPEN   Laser,"\\NINs2\Laser2",Str45
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   Laser,"Laser2",Str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",Str45
                              endif
                  elseif (PRTFlag = 5)     .Susan
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Susan's Printer"
                          if (osflag = c2)         .nt
                                  PRTOPEN Laser,"@KYOCERA FS1030D",Str45
                          elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                  PRTOPEN Laser,"@KYOCERA FS1030D",Str45
                          elseif (osflag = c1)         .win 95 98
                                  PRTOPEN Laser,"@KYOCERAS",Str45
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN Laser,"@",Str45
                          endif
                  elseif (prtflag = 7)     .DH
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," David's Printer"
                          if (osflag = c2 or Osflag = c6)         .nt +
                                  PRTOPEN Laser,"@Kyocera Mita FS-C5016N",Str45
                          elseif (osflag = c1)         .win 95 98
                                  PRTOPEN Laser,"@KYOCERAM",Str45
                          else   .(osflag = c0)         .Don't know prompt for printer
                                  PRTOPEN Laser,"@",Str45
                          endif
                    else
                              DISPLAY   *P15:07,PRTNAME,b1,prtflag," Laser8"
                              if (OSFLAG >= C6)
                                        PRTOPEN   Laser,"\\NINs2\Laser8",Str45
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   Laser,"Laser8",Str45
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   Laser,"-",Str45
                              endif
                    endif
                    endif
          Return

WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"mailplan pdf - ",mailattach
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
.Debug
.               Display        *p1:23,*el,*b,"Input = ",input," array count = ",PrintRecCount," M1 = ",m1," m2 = ",m2
.               Pause          "5"
.               return

               INCLUDE        NORD2IO.inc
.Bebin patch 3.85
          include   compio.inc
          include   cntio.inc
.               INCLUDE        NMLRIO.inc
.End patch 3.85
               INCLUDE        NDATIO.inc
               INCLUDE        NXRFIO.inc
               INCLUDE        NXCHIO.inc
               INCLUDE        NXNGIO.inc
               include        nownio.inc
               include        nmrgio.inc
               include        statsio.inc
.START PATCH 3.6 - ADDED LOGIC
               include        nofrio.inc
.END PATCH 3.6 - ADDED LOGIC
.START PATCH 3.84 ADDED LOGIC
          INCLUDE   NSELIO.INC
          INCLUDE   NSEL2IO.INC
          INCLUDE   NMODIO.INC
          INCLUDE   NTXTIO.INC
.END PATCH 3.84 ADDED LOGIC
               INCLUDE        COMLOGIC.inc
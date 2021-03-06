...............................................................................
.'INVPRT' READS BOTH NAMES IN THE NEWS AND COMPUNAME INVOICE FILES STARTING AT.
.THE BEGINNING IF THE FILE 'ARMST' DOES NOT EXIST OR AT THE FIRST LR NUMBER IN.
.1980 IF IT DOES. AT THE END OF EACH YEAR THE MONTHLY INVOICE, A/R, AND A/P   .
.TOTALS ARE WRITTEN TO 'ARMST'. WHEN THE LAST INVOICE IN THE FILE IS READ, THE.
.TOTALS FROM 'ARMST' ARE PRINTED ON A SERIAL PRINTER.                         .
. UPDATED   01/04/82    DAVID HERRICK    NAMES IN THE NEWS CAL.
. UPDATED   PRINT ON LOCAL PRINTER OR TO SPOOL FILE INVPRT/PRT.
. UPDATED   04/02/84 - MAJOR CALCULATION CORRECTION.
.To tie with other reports this should pull adjustments by date not by LR#
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 3.3
.         INCLUDE   NINVDD.inc
          INCLUDE             ninvdd.inc
          INclude   NInvAcddd.inc
.end patch 3.3
         inc       hp.inc
         INCLUDE   NADJDD.inc
.begin patch 3.0
          INCLUDE          PRTPAGEDD.INC
.end patch 3.0
Release        Init           "3.53"      DH new year,
Reldate   Init      "2016 Jan 4"             
.Release        Init           "3.52"      DH new year,
.Reldate   Init      "2015 Jan 5"             
.Release        Init           "3.51"      DH new year,
.Reldate   Init      "2014 Jan 2"             
.Release        Init           "3.50"      DH Sunbelt PDF
.Reldate   Init      "2013 April 23"             
.Release        Init           "3.41"      DH new year,
.Reldate   Init      "3 Jan 2013"             
.Release        Init           "3.40"      DH new year, Data Manager
.Reldate   Init      "3 Jan 2012"             
.Release        Init           "3.39"      DH new year, Data Manager
.Reldate   Init      "3 Jan 2011"             
.Release        Init           "3.38"      DH new year
.Reldate   Init      "3 Jan 2010"             
.Release        Init           "3.37"      DH turn off seperate NIN/PL   new year
.Reldate   Init      "31 Dec 2009"             
.Release        Init           "3.36"      2009  JD    13Jan2009
.Release        Init           "3.35"      2008 DLH   04Jan2008
.Release        Init           "3.34"      2007   05Jan2007JD
.Release        Init           "3.33"      2006   20Nov2006JD
.Release        Init           "3.32"      2006  06Jan2006JD
.Release        Init           "3.31"     13Jul2005         JD         KEY var expanded for valid "Search"
.Release        Init           "3.3"    02March2005         DLH       Invoice conversion
.Release        Init           "3.23"    07Jan2005          JD        2005 changes.
.Release        Init           "3.22"   06AUG2004 ASH       Logo Conversion
.Release        Init           "3.21"   28May2004 DLH  change to PDF995 printer
.Release        Init           "3.2"   06Jan2003 DLH  extra check for valid key
.Release        Init           "3.1"   10Dec2002 DLH  fix at Load array - check for valid data
.RELEASE  init      "3.0"             08jan02 DLH Rewrite
.                                         PRTPAGE
.                                         New file format - Isam
.RELEASE  init      "2.7"            JD  07jan02 ready for 2002 run.
.RELEASE  init      "2.6"            JD  13jan00 ready for 2000 run.
.RELEASE  init      "2.5"           DLH 26AugY99 NINadj nadjust Y2k
.RELEASE  init      "2.4"           DLH 4MAY99 NINVOICE Y2k
.: started
.RELEASE  init      "2.3"           JD 25JAN99 got ready for 1999 run.
.RELEASE  init      "2.2"          DLH do not reread nincal files for CMP section.
.release   init      "2.1"           JD 20Jan98 for 98.
.RELEASE  INIT      "2.0"           DLH 23MAR92, CONS, NINVXX, NADJXX,
.                                  COMLOGIC
.
. .FILES DECLARATIONS
.
.begin patch 3.0
.OUTPUT   FILE      FIX=58          PHYSICAL OUTPUT FILE 'ARMST'
.begin patch 3.2
.Results   IFILE      Keylen=6,FIX=33          .PHYSICAL OUTPUT FILE 'INVSTATS.isi'
Results   IFILE      Keylen=6,FIX=35          .PHYSICAL OUTPUT FILE 'INVSTATS.isi'
.end patch 3.2
.file layout
resultkey           dim           6         CCYYMM      1-6
.TotINV        form          5                     7-11
.TotAP         Form          9.2                  12-23
.TotAR         Form          9.2                  24-35
.
. ..........................................................................
.RN       FORM      "026"      RECORD NUMBER
holdToday       dim         6
.end patch 3.0
.
.
.
DATE     DIM       8
TIME     DIM       8
.F1       DIM       1         POS 1-1    FILLER
YR       FORM      2         POS 2-3    YEAR
MO       FORM      2         POS 5-6    MONTH
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
MIN      DIM       2         POS 17-18  MINUTE
SEC      DIM       2         POS 20-21  SECOND
.
. .WORK SPACE
.
FINAL    DIM       1         CHECKS END OF PROGRAM
C        DIM       1         CHECKS FOR "0" IN TOTAL
OK       DIM       1
CHECK    DIM       1
.
COUNT    FORM      2
COUNT1   FORM      5         COUNTS INVOICE RECORDS
COUNT2   FORM      5         OUTPUT MONTH COUNT
COUNT3   FORM      5         OUTPUT YEAR COUNT
WORK     FORM      10.4
B14      DIM       14
B17      DIM       17
B20      DIM       20
B23      DIM       23
.
.
. .TOTAL INVOICE WORK FIELDS
.
TOTINV   FORM      5
FININV   FORM      5(3)         TOTAL INVOICES PLACED IN YEAR
. *******CHANGE YEARS IN FOLLOWING LINES*****
.FININVA  DIM       5         TOTAL INVOICES PLACED DURING 1995
.FININVB  DIM       5         TOTAL INVOICES PLACED DURING 1996
.FININVC  DIM       5         TOTAL INVOICES PLACED DURING 1997
FORM5    FORM      5
.
. .ACCOUNTS PAYABLE WORK FIELDS
.
.begin patch 3.0
.TOTAP    FORM      10
.FINAP    FORM      10         TOTAL NAMES INVOICED DURING YEAR
TOTAP    FORM      9.2
FINAP    FORM      9.2(3)         TOTAL NAMES INVOICED DURING YEAR
. *******CHANGE YEARS IN FOLLOWING LINES*****
.FINAPA   DIM       10         TOTAL NAMES INVOICED DURING 1995
.FINAPB   DIM       10         TOTAL NAMES INVOICED DURING 1996
.FINAPC   DIM       10         TOTAL NAMES INVOICED DURING 1997
.end patch 3.0
FORM9P   FORM      10
ACCPAY   FORM      10
FORM9    FORM      10
.
. .ACCOUNTS RECIEVABLE WORK FIELDS
.
.begin patch 3.0
.TOTAR    FORM      10
.FINAR    FORM      10
TOTAR    FORM      9.2
FINAR    FORM      9.2(3)
.FINARA   DIM       10
.FINARB   DIM       10
.FINARC   DIM       10
.end patch 3.0
FINARB1  DIM       10
FORM9R   FORM      10
ACCREC   FORM      10
.EndAR    form      10
EndARA    FORM      9.2
EndARB    FORM      9.2
FORM7    FORM      7
.
. .PERCENTAGE COMPUTATION FIELDS
.
PCT      DIM       3
PCTB     DIM       5         % CHANGE IN 1991
PCTC     DIM       5         % CHANGE IN 1992
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
.
F2       DIM       2         FILLER
.
.V        FORM      "01"      SPLIT SCREEN POSITION
.V1       FORM      "15"      SPLIT SCREEN POSITION
V1       FORM      "14"      SPLIT SCREEN POSITION
.V2       FORM      "26"      SPLIT SCREEN POSITION
V2       FORM      "25"      SPLIT SCREEN POSITION
.
. **********CHANGE THE FOLLOWING INV# NUMBER IN JAN*********
. 000000 FIRST INV# OF 1981
. 001000 FIRST INV# OF 1982
. 015124 FIRST INV# OF 1983
. 032197 FIRST INV# OF 1984
. 052364 FIRST INV# OF 1985
. 075180 FIRST INV# OF 1986
. 098419 FIRST INV# OF 1987
. 121541 FIRST INV# OF 1988
. 145392 FIRST INV# OF 1989
. 172279 FIRST INV# OF 1990
. 197750 FIRST INV# OF 1991
. 218003 FIRST INV# OF 1992
. 241530 FIRST INV# OF 1993
. 262880 FIRST INV# OF 1994
. 282911 FIRST INV# OF 1995
. 302647 FIRST INV# OF 1996
. 321449 FIRST INV# OF 1997
. 341381 FIRST INV# OF 1998
. 362337 FIRST INV# OF 1999
. 385477 FIRST INV# OF 2000
. 385477 FIRST INV# OF 2001
.begin patch 3.2
.FIRSTN   INIT      "433150"  FIRST NIN INV# OF 2002
.FIRSTN   INIT      "477920"  FIRST NIN INV# OF 2004
.begin patch 3.32
.FIRSTN   INIT      "500807"   FIRST NIN INV# OF 2005
.begin patch 3.32
.begin patch 3.34
.FIRSTN   INIT      "520529"   FIRST NIN INV# OF 2006
.begin patch 3.34
.begin patch 3.35
.FIRSTN   INIT      "536416"   FIRST NIN INV# OF 2007
.begin patch 3.35
.begin patch 3.36
.FIRSTN   INIT      "551933"   FIRST NIN INV# OF 2008
.begin patch 3.37
.FirstN    INit      "566458"    FIRST NIN INV# OF 2009
.begin patch 3.38
.FirstN    INit      "577030"    FIRST NIN INV# OF 2010
.begin patch 3.340
.FirstN    INit      "586274"    FIRST NIN INV# OF 2011
.FirstN    INit      "595488"    FIRST NIN INV# OF 2012
.start patch 3.51 every year 
.FirstN    INit      "603740"    FIRST NIN INV# OF 2013
.start patch 3.52 every year 
.FirstN    INit      "612131"    FIRST NIN INV# OF 2014
.FirstN    INit      "620721"    FIRST NIN INV# OF 2015
FirstN    INit      "630497"    FIRST NIN INV# OF 2016
.end patch 3.52 every year 
.end patch 3.51 every year 
.end patch 3.38
.end patch 3.37
.begin patch 3.36
.FIRSTN   INIT      "455522"  FIRST NIN INV# OF 2003
.end patch 3.2
FIRSTC   INIT      "341381"  FIRST CMP INV# OF 
.
YR2      DIM       2         HOLDS YEAR
HOLDYR   FORM      2         HOLDS YEAR
DA2      DIM       2         HOLDS DAY
NUMDA    FORM      2         HOLDS DAY
MO2      DIM       2         HOLDS MONTH
HOLDMO   DIM       2         HOLDS MONTH
NUMMO    FORM      2         HOLDS MONTH
HRSTRT   DIM       2         HOLDS STARTING HOUR
NUMHR    FORM      2         HOLDS HOUR
MINSTRT  DIM       2         HOLDS STARTING MIN
SECSTRT  DIM       2         HOLDS STARTING SEC
MONTH    DIM       9
holdce   dim       2
.
TEST     FORM      "12"
MNTH     FORM      "01"
TAB      FORM      "01"
INDEX    FORM      "01"
ZERO     FORM      "0"
TITLE    INIT      "COMPUNAME        "
.KEY      INIT      "96"
.Begin patch 3.24
KEY      INIT      "096"
.Begin patch 3.24
TEN      FORM      "10"
IND      FORM      2
.
. HOLD TOTAL number INVOICES
.
.Begin patch 3.0
Row1          form          "2175"
Row2          Form          "2550"
Row3          Form          "2925"
Row4          Form          "3300"
Row5          Form          "3675"
Row6          Form          "4050"
Row7          Form          "4425"
Row8          Form          "4800"
Row9          Form          "5175"
Row10         Form          "5550"
Row11         Form          "5925"
Row12         Form          "6300"

ColumnA       form          5
ColumnB       form          5
ColumnC       Form          5             .used for percentages

Column1A      Form          "1000"
Column1B      Form          "2375"

Column2A      Form          "2750"
Column2B      Form          "4125"

Column3A      Form          "4875"
Column3B      Form          "6250"
TrackMonth    Form          2
STARTYEAR     FORM          4
HoldYear      form          4
YEARCOUNT     FORM          1       .WHICH YEAR OF REPORT

oVERfLAG      DIM           1
InvCount      Form          5(3,12)               ..3 years worth of MONTHly data
.JANX     FORM      5
.FEBX     FORM      5
.MARX     FORM      5
.APRX     FORM      5
.MAYX     FORM      5
.JUNX     FORM      5
.JULX     FORM      5
.AUGX     FORM      5
.SEPX     FORM      5
.OCTX     FORM      5
.NOVX     FORM      5
.DECX     FORM      5
.
. .HOLD ACC/PAY
.
Payables   Form          9.2(3,12)               ..3 years worth of MONTHly data
.JAN      FORM      10
.FEB      FORM      10
.MAR      FORM      10
.APR      FORM      10
.MAY      FORM      10
.JUN      FORM      10
.JUL      FORM      10
.AUG      FORM      10
.SEP      FORM      10
.OCT      FORM      10
.NOV      FORM      10
.DEC      FORM      10
.
. .HOLD ACC/REC
.
Receivables   Form          9.2(3,12)               ..3 years worth of MONTHly data
.JANR     FORM      10
.FEBR     FORM      10
.MARR     FORM      10
.APRR     FORM      10
.MAYR     FORM      10
.JUNR     FORM      10
.JULR     FORM      10
.AUGR     FORM      10
.SEPR     FORM      10
.OCTR     FORM      10
.NOVR     FORM      10
.DECR     FORM      10
.end patch 3.0

.
FINALSAV FORM      10.2
SAVE     FORM      10.2
FIN10     FORM      10
WORK14   DIM       14
FORM14   FORM      14
DIM10    DIM       10
.
MASKARA  INIT      "99999999.99"
MASKARB  INIT      "99999999.99"
MASKARC  INIT      "99999999.99"
.Begin patch 3.0
Invmask       init          "ZZ,ZZ9"
MaskIt    INIT      "$$$,$$$,$$Z.99"
.
. AVERAGE INVOICE FIELDS
.
FORM114  FORM      11.4
Average       Form          5.2
MaskAverage   init          "$$,$$9.99"
.AVERAGA  FORM      5.2
.AVERAGB  FORM      5.2
.AVERAGC  FORM      5.2
.hexeight integer 4,"4294967295"
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
        include     winapi.inc
.end patch 3.0
.
.
SPOOL    DIM       1
FMESG    DIM       20
ANS      DIM       1
LASTMOSW DIM       1               HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR
.START PATCH 3.22 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 3.22 ADDED LOGIC
         MOVE      "NINV0005" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "CALCULATE CURRENT INVOICES" TO STITLE
         move      c1 to v
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         TRAP      STOP IF F5
. SPOOLING Y/N
.Begin patch 3.0
.         KEYIN    *P15:10,"DO YOU WANT THE OUTPUT SPOOLED ?":
.                   *T20,SPOOL;
.         CMATCH    "N",SPOOL
.         GOTO      BEGIN IF EQUAL
.         DISPLAY   *P15:12,"SPOOL FILE IS NAMED 'ORDPRT/PRT'";
..         SPLOPEN   "INVPRT/PRT:DR0"
.         IFNZ      PC
.         SPLOPEN   "ORDPRT/PRT:PRINT","Q"
.         XIF
.         IFZ       PC
.         SPLOPEN   "\\nins1\e\data\ORDPRT","Q"
.         XIF
.end patch 3.0
.
BEGIN
.
.THIS ROUTINE CALLS DISPLAY, WHICH DISPLAY THE RECORD AND DATE COUNT ON THE;
.SCREEN AND TIME WHICH READS THE DATE AND TIME FORM 'ARCCLOCK'. THE TIME IS
.THEN STORED AS THE STARTING TIME OF THE REPORT.
         CALL      DISPLAY
         CALL      TIME
         MOVE      HR,HRSTRT
         REP       zfill,HRSTRT
         MOVE      MIN,MINSTRT
         REP       zfill,MINSTRT
         MOVE      SEC,SECSTRT
         MOVE      DA,DA2
         REP       zfill,DA2
.         MOVE      YR,YR2
         unpack    yr into yr2
         MOVE      MO,MO2
         REP       zfill,MO2
         TRAP      START IF IO
.
.THE FOLLOWING ROUTINE OPENS 'ARMST'. IF 'ARMST' DOES NOT EXIST THE TRAP
.CAUSES CONTROL TO BE TRANSFERED TO START, WHICH BEGINS READING THE INVOICE
.FILE FORM THE BEGINNING.  IF THE TRAP IS NOT ENCOUNTERED, THIS MEANS THAT
.'ARMST' ALREADY EXISTS AND THE FIRST READ OF THE INVOICE FILE IS THE FIRST
.LR OF 1980.
.
.Begin patch 3.0
         OPEN      Results,"InvStats|NINS1:502",exclusive
.cleanup old totals before proceeding. This section needs to be updated as time rolls on
.currently we are deleting records from Jan 2002 to date.
.              move          "2003" to n4
.              move          "2004" to n4
.begin patch 3.32
.              move          "2005" to n4
.begin patch 3.32
.begin patch 3.34
.              move          "2006" to n4
.begin patch 3.34
.begin patch 3.35
.              move          "2007" to n4
.begin patch 3.35
.begin patch 3.36
.              move          "2008" to n4
.begin patch 3.37
.              move          "2009" to n4
.begin patch 3.38
.              move          "2010" to n4
.begin patch 3.40
.              move          "2011" to n4
.begin patch 3.41
.              move          "2012" to n4
.begin patch 3.51
.              move          "2013" to n4
.begin patch 3.52
.              move          "2014" to n4
              move          "2016" to n4
.end patch 3.52
.end patch 3.51
.end patch 3.41
.end patch 3.38
.end patch 3.37
.begin patch 3.36
              move          c1 to trackmonth
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
cleanloop
              loop
              while         (trackmonth < 13)
              read          results,resultkey;str6,Totinv,Totap,totar
              goto          increment if over
              Move          C0 to Totinv
              move          c0 to TotAp
              move          C0 to TotAR
              Update        Results;Str6,TotInv,TotAp,TotAr
Increment     add           c1 to trackmonth
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
              repeat
              move          c1 to trackmonth
.
              move          c1 to trackmonth
              add           c1 to n4
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
.              if            (n4 < 2007)
.begin patch 3.32
.              if            (n4 < 2007)
.end patch 3.32
.begin patch 3.34
.              if            (n4 < 2008)
.end patch 3.34
.begin patch 3.35
.              if            (n4 < 2009)
.end patch 3.35
.begin patch 3.36
.begin patch 3.37
.              if            (n4 < 2010)
.begin patch 3.38
.              if            (n4 < 2011)
.begin patch 3.40
.              if            (n4 < 2012)
.begin patch 3.41
.              if            (n4 < 2013)
.begin patch 3.51
.              if            (n4 < 2014)
.begin patch 3.52
.              if            (n4 < 2015)
              if            (n4 < 2017)
.end patch 3.52
.end patch 3.51
.end patch 3.41
.end patch 3.38
.end patch 3.37
.end patch 3.36
              goto          cleanloop
              endif
.end patch 3.0
.
         TRAPCLR   IO
         MOVE      C2 TO NINVPATH           .SET ACCESS TO ISAM BY INV#.
         MOVE      "                    ",FMESG
         MOVE      "Q",CHECK
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      FIRSTN TO NINVFLD
         CALL      NINVKEY
.         CALL      NINVtst
         GOTO      NOINV IF OVER
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
         MOVE       invdtEC,holdce

.         REP       zfill,AP1
.         REP       zfill,AP2
.         REP       zfill,AR
.Begin patch 3.0
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "260",RN       'ADD 13 TO THIS VARIABLE EVERY JAN'
.. /////////////////////////////////////////////////////////////////////////////
         MOVE      YR,HOLDYR
.end patch 3.0
         GOTO      READ2
.
.Begin patch 3.0
..AFTER THE NAMES IN THE NEWS INVOICE FILE IS READ, CONTROL IS TRANSFERRED TO
..THE FOLLOWING ROUTINE.  THIS ROUTINE OPENS THE COMPUNAME INVOICE AND ADJUST-
..MENT FILES, READS THE FIRST LR NUMBER OF 1980 AND SPLITS THE SCREEN TO DISP-
..LAY THE COMPUNAME INVOICE AND DATE TOTALS.
..
.CHECK
.         move      c0 to ninvflg2
.         move      c0 to ninvflag
.         move      c0 to nadjflag
.         move      c2 to ninvpath
.         close     ninvfile
.         close     ninvfil2
.         close     nadjfile
.          MOVE      "CMPINV2" TO NINVNME2
.          MOVE      "CMPINV2" TO NINVNaME
.         CMATCH    "Q",CHECK
.         GOTO      CMP IF NOT EQUAL
.         MOVE      FIRSTC TO NINVFLD
.         CALL      NINVKEY
..         REP       zfill,AP1
..         REP       zfill,AP2
..         REP       zfill,AR
.         MOVE      "COMPUNAME",TITLE
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "260",RN       'ADD 13 TO THIS VARIABLE EVERY JAN'
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "X",FINAL
.         ADD       "42",V
.         ADD       "42",V1
.         ADD       "42",V2
.         MOVE      "30",TAB
.         CALL      ZERO
.         OPEN      OUTPUT,"\\nins1\e\data\text\ARMST.dat"
.         MOVE      YR,HOLDYR
.         GOTO      READ2
.
.end patch 3.0
.THE FOLLOWING ROUTINE IS ONLY ENCOUNTERED IF 'ARMST' DOES NOT EXIST. IT
.CREATES 'ARMST' AND OPENS ALL OF THE NECESSARY INVOICE AND ADJUSTMENT FILES
.
START    TRAPCLR   IO
         NORETURN
.begin patch 3.0
         DISPLAY   *P10:12,*EF,*HON,"InvStats File NOT FOUND. IT WILL BE CREATED."
.end patch 3.0
         BEEP
         MOVE      "NINADJ" TO NADJNAME
         MOVE      "NAMES IN THE NEWS",TITLE
.begin patch 3.0
..begin patch 3.2
.         Prepare   Results,"\\nins1\e\data\text\INVStats.dat","\\nins1\e\data\index\INVStats.isi","6","33"
         Prepare   Results,"\\nins1\e\data\text\INVStats.dat","\\nins1\e\data\index\INVStats.isi","6","35"
..end patch 3.2
.         PREPARE   OUTPUT,"\\nins1\e\data\text\ARMST.dat"
.         GOTO      SCRN1
.end patch 3.0
.
.begin patch 3.0
.CMP
.         move      c0 to ninvflg2
.         move      c0 to ninvflag
.         move      c0 to nadjflag
.         move      c2 to ninvpath
.         close     ninvfile
.         close     ninvfil2
.         close     nadjfile
.          MOVE      "CMPINV2" TO NINVNME2.
.          MOVE      "CMPINV2" TO NINVNaME
.         MOVE      "CMPADJ" TO NADJNAME
.         MOVE      "COMPUNAME",TITLE
.         MOVE      "X",FINAL
.         OPEN      OUTPUT,"\\nins1\e\data\text\ARMST.dat"
.         MOVE      "30",TAB
.         CALL      ZERO
.SCRN1    CMATCH    "X",FINAL
.         GOTO      READ1 IF NOT EQUAL
.         ADD       "42",V
.         ADD       "42",V1
.         ADD       "42",V2
.
.end patch 3.0
......READS INVOICE FILE AND CREATES 'ARMST'
.
READ1    CALL      NINVKS
         GOTO      PrintIt IF OVER
.         REP       zfill,AP1
.         REP       zfill,AR
.         REP       zfill,AP2
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
        MOVE       invdtEC,holdce
         ADD       "1",COUNT1
         DISPLAY   *PV1:6,COUNT1,*PV2:9,MO,"/",DA,"/",YR," ",LRN;
         COMPARE   "1",COUNT1
         GOTO      READ2 IF NOT EQUAL
         MOVE      YR,HOLDYR
.begin patch 3.0
Read2
.READ2    COMPARE   YR,HOLDYR
.         GOTO      NEWYEAR IF NOT EQUAL
.         LOAD      FORM5 FROM MO OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
.                   SEPX,OCTX,NOVX,DECX
.         ADD       "1",FORM5
.         ADD       "1",FININV
.         STORE     FORM5 INTO MO OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
.                   SEPX,OCTX,NOVX,DECX
              packKey       ResultKey from InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              Read          Results,ResultKey;ResultKey,Totinv,TotAP,TotAR
              if            over
              packKey       ResultKey from InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              move          c0 to Totinv
              move          c0 to totap
              move          c0 to totar
.begin patch 3.2
                                                    if                       (resultkey <> "000000" & resultkey <> "      ")
                     write         Results,ResultKey;ResultKey,Totinv,TotAP,TotAR      .prepare record
                  read          results,ResultKey;ResultKey,Totinv,TotAP,TotAR      .prepare record for update
                                                    endif
.end patch 3.2
              endif
              add           c1 to totinv
              add           ar to totar
              add           ap1 to totap
              add           ap2 to totap
..THE FOLLOWING SEQUENCE CHECKS THE AR, AP1, AND AP2 FOR A MINUS OVERPUNCH AND
..CONVERTS THOSE IT FINDS TO VALID NUMERIC DATA.
..
..begin patch 2.4
..         TYPE      AR
..         GOTO      AP1 IF EQUAL
..         ENDSET    AR
..         REP       "}0J1K2L3M4N5O6P7Q8R9",AR
..         RESET     AR
..         MOVE      AR,FORM9
..         SUB       FORM9,ACCREC
..         GOTO      AP1A
.AP1
.         mult      "100" by ar
.         MOVE      AR,ACCREC
..AP1A     TYPE      AP1
..         GOTO      AP2 IF EQUAL
..         ENDSET    AP1
..         REP       "}0J1K2L3M4N5O6P7Q8R9",AP1
..         RESET     AP1
.         mult      "100" by ap1
..         MOVE      AP1,FORM9
..         SUB       FORM9,ACCPAY
..         GOTO      AP2A
.AP2      MOVE      AP1,ACCPAY
..AP2A     TYPE      AP2
..         GOTO      ADD IF EQUAL
..         ENDSET    AP2
..         REP       "}0J1K2L3M4N5O6P7Q8R9",AP2
..         RESET     AP2
..         MOVE      AP2,FORM9
..         SUB       FORM9,ACCPAY
..         GOTO      LOAD
.ADD
..      MOVE      AP2,FORM9
.         mult      "100" by ap2
..         ADD       FORM9,ACCPAY
.         ADD       ap2,ACCPAY
..end patch 2.4
.LOAD     LOAD      FORM9P FROM MO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
.                   NOV,DEC
..         CMATCH    "*",ADJC                   .do it anyway
..         GOTO      LOAD2 IF NOT EQUAL
         MOVE      LRN TO NADJFLD
         rep       zfill in nadjfld
         CALL      NADJKEY
         GOTO      LOAD2 IF OVER
..begin patch 2.5
..         REP       zfill,ASRECADJ
..         REP       zfill,ASPAYAD1
..         REP       zfill,ASPAYAD2
..         TYPE      ASPAYAD1
..         GOTO      NEXT2 IF EQUAL
..NEXT1    BUMP      ASPAYAD1
..         GOTO      NEXT1 IF NOT EOS
..         REP       "}0J1K2L3M4N5O6P7Q8R9",ASPAYAD1
..         RESET     ASPAYAD1
..         MOVE      ASPAYAD1,FORM7
..         SUB       FORM7,FORM9P
..         SUB       FORM7,FINAP
..         GOTO      NEXT3
NEXT2    MOVE      ASPAYAD1,FORM7
.         ADD       FORM7,FORM9P
.         ADD       FORM7,FINAP
              add           form7 to totap
..NEXT3    TYPE      ASPAYAD2
..         GOTO      NEXT5 IF EQUAL
..NEXT4    BUMP      ASPAYAD2
..         GOTO      NEXT4 IF NOT EOS
..         REP       "}0J1K2L3M4N5O6P7Q8R9",ASPAYAD2
..         RESET     ASPAYAD2
..         MOVE      ASPAYAD2,FORM7
..         SUB       FORM7,FORM9P
..         SUB       FORM7,FINAP
..         GOTO      LOAD2
..end patch 2.5
.NEXT5    MOVE      ASPAYAD2,FORM7
          MOVE      ASPAYAD2,FORM7
.         ADD       FORM7,FINAP
.         ADD       FORM7,FORM9P
.LOAD2    ADD       ACCPAY,FORM9P
.         ADD       ACCPAY,FINAP
.         STORE     FORM9P INTO MO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
.                   NOV,DEC
          add           FORM7 to totap
..
.         LOAD      FORM9R FROM MO OF JANR,FEBR,MARR,APRR,MAYR,JUNR,JULR,AUGR:
.                   SEPR,OCTR,NOVR,DECR
..         CMATCH    "*",ADJC                     .04jun96 - dlh check anyway
..         GOTO      LOAD3 IF NOT EQUAL
.         MOVE      LRN TO NADJFLD
.         rep       zfill in nadjfld
.         CALL      NADJKEY
.         GOTO      LOAD3 IF OVER
..begin patch 2.5
..         REP       zfill,ASRECADJ
..         REP       zfill,ASPAYAD1
..         REP       zfill,ASPAYAD2
..
..THE FOLLOWING SEQUENCE CONVERTS MINUS OVERPUNCHES IN THE ADJUSTMENT RECORD
..
..         TYPE      ASRECADJ
..         GOTO      LOOP2 IF EQUAL
..LOOP1    BUMP      ASRECADJ
..         GOTO      LOOP1 IF NOT EOS
..         REP       "}0J1K2L3M4N5O6P7Q8R9",ASRECADJ
..         RESET     ASRECADJ
..         MOVE      ASRECADJ,FORM7
..         SUB       FORM7,FORM9R
..         SUB       FORM7,FINAR
..         GOTO      LOAD3
..end patch 2.5
.LOOP2    MOVE      ASRECADJ,FORM7
.         MOVE                ASRECADJ,FORM7                   changed to form9 7/12/05JD
.              add           form7 to TotAr
          MOVE                ASRECADJ,FORM9
              add           form9 to TotAr
.         ADD       FORM7,FORM9R
.         ADD       FORM7,FINAR
.LOAD3
.         ADD       ACCREC,FORM9R
.         ADD       ACCREC,FINAR
Load2
.begin patch 3.3
          CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
          Move      c0 to form7
               FOR           AcdRecCount,"1","15"
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
               if             (NinvacdCode = "096")
          add       NinvAcdRate to form7
               Break
               endif
          Repeat

.         SEARCH    KEY IN ADDCHG1 TO TEN WITH IND
..         GOTO      LOAD4 IF OVER
.         GOTO      LOAD3 IF OVER
..
..THE FOLLOWING SEQUENCE CHECKS THE ADDITIONAL CHARGES FOR A '96' AND IF ONE
..IS ENCOUNTERED IT:
..
..1) LOADS THE WHOLE ADDITIONAL CHARGE FIELD TO A WORK FIELD
.         LOAD      WORK14 FROM IND OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
.                   ADDCHG6,ADDCHG7,ADDCHG8:
.                   ADDCHG9,ADDCHG10
.         RESET     WORK14,3
.         SETLPTR   WORK14,9
..2) MOVES THE ACTUAL CHARGE AMOUNT TO A NUMERIC WORK FIELD
.         MOVE      WORK14,FORM7
.3) AND ADDS THE CHARGE TO THE CURRENT MONTH AND FINAL ACC/PAY
         ADD       FORM7,FORM9R
.         ADD       FORM7,FINAR
.         ADD       FORM7,Totar
                              if        (form7 >0)
                              call      debugger
                              endif
         RESET     WORK14,14
         RESET     WORK14
.LOAD4    STORE     FORM9R INTO MO OF JANR,FEBR,MARR,APRR,MAYR,JUNR,JULR,AUGR:
.                   SEPR,OCTR,NOVR,DECR
.
Load3
.begin patch 3.2
          if                         (resultkey <> "000000" & resultkey <> "      ")
            Update         Results;ResultKey,Totinv,TotAP,TotAR      .prepare record
          endif
.end patch 3.2
.end patch 3.0
         GOTO      READ1
.
. .....WRITES DATA TO 'ARMST'
.
.begin patch 3.0
PrintIt
.begin patch 4.18
.begin patch 3.50
.          call      pdf995auto
.end patch 3.50
.end patch 4.18

.                        if (osflag = c2)         .nt
                        move    "Invprt" to  str25
.                                PRTOPEN Laser,"",str25
.                                PRTOPEN Laser,"\\NTS0\Laser8","\\nins1\e\data\orderprt.lst"

.                        PRTOPEN Laser,"Acrobat Distiller",str25
.begin patch 3.50
.                        PRTOPEN Laser,"PDF995",str25
                        pack    str55,"c:\work\pdf\",str25,".pdf"
                        PRTOPEN Laser,"PDF:",str55
.                        pack    str55,str25,".pdf"
.end patch 3.50
.                       elseif (osflag = c1)         .win 95 98
.                                PRTOPEN Laser,"","\\nins1\e\data\orderprt.lst"
.                                PRTOPEN Laser,"Laser8","\\nins1\e\data\orderprt.lst"
.                        else   .(osflag = c0)         .Don't know prompt for printer
.                                PRTOPEN Laser,"","\\nins1\e\data\orderprt.lst"
.                        endif
............
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Portrait


.START PATCH 3.22 REPLACED LOGIC
.         prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
.                         *FONT=prtpg24B,*p=2350:50,"Names ":
.                    *font=prtpg24I," in the News":
.                    *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
.                    *FONT=prtpg10,*P=7000:375,"Time: ",Time:
.                    *p=1475:1100,"2002":
.                    *p=3725:1100,"2003":
.                    *p=5725:1100,"2004":
.                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
         prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
              *Pictrect=*off,*PICT=0:1000:2250:8550:NINLogo:
                    *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
                    *FONT=prtpg10,*P=7000:375,"Time: ",Time:
.                    *p=1475:1100,"2002":
.                    *p=3725:1100,"2003":
.                    *p=5725:1100,"2004":
.begin patch 3.32
.                    *p=1475:1100,"2004":
.                    *p=3725:1100,"2005":
.                    *p=5725:1100,"2006":
.begin patch 3.32
.begin patch 3.34
.                    *p=1475:1100,"2005":
.                    *p=3725:1100,"2006":
.                    *p=5725:1100,"2007":
.begin patch 3.34
.begin patch 3.35
.                    *p=1475:1100,"2006":
.                    *p=3725:1100,"2007":
.                    *p=5725:1100,"2008":
.begin patch 3.35
.begin patch 3.36
.begin patch 3.37
.                    *p=1475:1100,"2007":
.                    *p=3725:1100,"2008":
.                    *p=5725:1100,"2009":
.begin patch 3.38
.                    *p=1475:1100,"2008":
.                    *p=3725:1100,"2009":
.                    *p=5725:1100,"2010":
.begin patch 3.40
.                    *p=1475:1100,"2009":
.                    *p=3725:1100,"2010":
.                    *p=5725:1100,"2011":
.begin patch 3.41
.                    *p=1475:1100,"2010":
.                    *p=3725:1100,"2011":
.                    *p=5725:1100,"2012":
.begin patch 3.51
.                    *p=1475:1100,"2011":
.                    *p=3725:1100,"2012":
.                    *p=5725:1100,"2013":
.begin patch 3.52
.                    *p=1475:1100,"2012":
.                    *p=3725:1100,"2013":
.                    *p=5725:1100,"2014":
                    *p=1475:1100,"2014":
                    *p=3725:1100,"2015":
                    *p=5725:1100,"2016":
.end patch 3.52
.end patch 3.51
.end patch 3.41
.end patch 3.38
.end patch 3.37
.begin patch 3.36
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
.END PATCH 3.22 REPLACED LOGIC
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1250,*PENSIZE=10,*line=ColumnB:1250
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1250,*PENSIZE=10,*line=columnb:1250:
                    *font=prtpg10B,*p=1:1800,"Month":
                    *font=prtpg10,*p=COLUMN1A:1800,"Invoices":
                    *font=prtpg10,*p=COLUMN1B:1800,*alignment=*Right,"AR/AP":
                    *p=column1a:1950,*PENSIZE=10,*line=column1b:1950:
                    *font=prtpg10,*p=column2a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column2b:1800,*alignment=*Right,"AR/AP"
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         PrtPage    Laser;*font=prtpg10,*p=columnb:1800,*alignment=*Right,"Chng ":
                    *FONT=prtpg10,*p=column2a:1950,*PENSIZE=10,*line=ColumnB:1950:
                    *font=prtpg10,*p=column3a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column3b:1800,*alignment=*Right,"AR/AP"
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         PrtPage    Laser;*font=prtpg10,*p=columnb:1800,*alignment=*Right,"Chng ":
                    *FONT=prtpg10,*p=column3a:1950,*PENSIZE=10,*line=columnb:1950:
                    *font=prtpg10,*p=1:2175,*alignment=*Left,"January":
                    *font=prtpg10,*p=1:2550,"February":
                    *font=prtpg10,*p=1:2925,"March":
                    *font=prtpg10,*p=1:3300,"April":
                    *font=prtpg10,*p=1:3675,"May":
                    *font=prtpg10,*p=1:4050,"June":
                    *font=prtpg10,*p=1:4425,"July":
                    *font=prtpg10,*p=1:4800,"August":
                    *font=prtpg10,*p=1:5175,"September":
                    *font=prtpg10,*p=1:5550,"October":
                    *font=prtpg10,*p=1:5925,"November":
                    *font=prtpg10,*p=1:6300,"December":
                    *p=column1a:7025,*PENSIZE=10,*line=column1b:7025:
                    *p=column1a:7575,*PENSIZE=10,*line=column1b:7575
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column2a:7575,*PENSIZE=10,*line=columnb:7575
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column3a:7575,*PENSIZE=10,*line=columnb:7575:
                    *font=prtpg10,*p=1:7050,"Totals":
                    *p1:8000,"*** This report includes adjustments done in the same year as the associated invoice ***"



.begin patch 3.2  MUST BE CHANGED EVERY JANUARY!!!!
.begin patch 3.35
.                                move             "2005" to startyear
.                                move             "2006" to startyear
.End patch 3.35
.begin patch 3.36
.begin patch 3.37
.begin patch 3.38
.begin patch 3.40
.begin patch 3.41
.                                 move             "2010" to startyear
.begin patch 3.51
.                                 move             "2011" to startyear
.begin patch 3.52
.                                 move             "2012" to startyear
                                 move             "2014" to startyear
.end patch 3.52
.end patch 3.51
.end patch 3.41
.                                 move             "2009" to startyear
.                                 move             "2008" to startyear
.end patch 3.38
.                                 move             "2007" to startyear
.end patch 3.37
.End patch 3.36
.                                move             "2004" to startyear
.                                move             "2003" to startyear
.                                move             "2002" to startyear
.                                move             "2001" to startyear
.                                move             "2000" to startyear
.end patch 3.2
                                        move                startyear to holdyear
          move                c1 to Yearcount
          move                c1 to Trackmonth
          PACK                resultkey,startyear,trackmonth     .first year to be read
          rep       zfill in resulTkey
          read                results,ResultKey;ResultKey,Totinv,TotAP,TotAR
              call          loadArray
              move          no to overflag
              loop
              until         (overflag = "Y")
              readks        results;ResultKey,Totinv,TotAP,TotAR
              if            over
              move          "Y" to overflag
              endif
              call          LoadArray
              repeat
.print goodies here
              move          c1 to yearcount
              move          c1 to trackmonth
              loop
              until         (yearcount = 4 )
                            loop
                            until         (trackmonth = 13)
                        move          Invcount(yearcount,trackmonth) to TOtinv
                              move          Payables(yearcount,trackmonth) to TotAP
                    move          Receivables(yearcount,trackmonth) to TotAr
                            If            (totinv > 0 or totap > 0 or totar >0)
                    call          printModet
                            endif
                            add           totinv to Fininv(yearcount)
                            add           TOTAP to FinAp(yearcount)
                            add           TOTAR to FinAr(yearcount)
                            add           c1 to trackmonth
                            repeat
              call          PrintYrTot
              add           c1 to yearcount
              MOVE          C1 TO TRACKMONTH
              repeat
              Goto          Stop
.......................................................................................
PrintMoDet
              move          c0 to row
              load          row from trackmonth of row1,row2,row3,row4,row5,row6:
                            row7,row8,row9,row10,row11,row12
              move          c0 to columnA
              move          c0 to columnB
              Load          COLUMNA from yearcount of Column1a,Column2a,Column3a
              Load          COLUMNB from yearcount of Column1b,Column2b,Column3b
          move          INVmask to str6
              edit          TotInv to str6
              move          maskit to Str14
              edit          Totar to Str14
              Prtpage      laser;*font=prtpg10,*p=columna:row,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,Str14
              if            (yearcount = 2)
              move          Receivables(1,trackmonth) to EndARA
              move          Receivables(2,trackmonth) to EndARB
              call          Percent
              move  Column2B to ColumnB
              add             "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif
              if            (yearcount = 3)
              move          startyear to n4
              add           c2 to n4
              pack          str6 from n4,trackmonth
              rep           zfill in str6
              move          Receivables(2,trackmonth) to EndARA
.add code for last month
                        if            (holdToday = str6)          .last month of report
                                       if         (trackmonth = 2)
                                       move       "28" to n2
                                       endif

                                       if         (trackmonth = 1 or trackmonth = 3 or trackmonth = 5 or trackmonth = 7 or trackmonth = 8 or trackmonth = 10  or trackmonth = 12 )
.                                      if         (trackmonth = 1 or trackmonth = 3 or trackmonth = 5 or trackmonth = 7 or trackmonth = 10  or trackmonth = 12 )
                                       move       "31" to n2
                                       endif

                                       if         (trackmonth = 4 or trackmonth = 6 or trackmonth = 9 or trackmonth = 11)
.                                      if         (trackmonth = 4 or trackmonth = 6 or trackmonth = 8 or trackmonth = 9 or trackmonth = 11)
                                       move       "30" to n2
                                       endif
                        unpack          today into mm,str1,dd,str1,yy
.so lets prorate
                        divide          N2 into EndArA
                        move          dd to n2
                        mult          n2 by EndARA
                        MOVE          ENDARA to Receivables(2,trackmonth)         .adjust ytd totals for year 2
                        endif

              move          Receivables(3,trackmonth) to EndARB
              call          Percent
              move  Column3B to ColumnB
              add             "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif

              add           "125" to ROw
              move          maskit to Str14
              edit          TotaP to Str14
              Load          COLUMNB from yearcount of Column1b,Column2b,Column3b
              Prtpage       laser;*font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,Str14,*ALIGNMENT=*Left
              RETURN
.............................................................................................................
PrintYrTot

          move          INVmask to str6
              edit          FinInv(yearcount) to str6
              move          maskit to Str14
              edit          Finar(yearcount) to Str14
              Prtpage      laser;*font=prtpg10,*p=columna:7050,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,Str14
              move          maskit to Str14
              edit          FinaP(yearcount) to Str14
              Prtpage      laser;*font=prtpg10,*p=columnb:7175,*ALIGNMENT=*Right,Str14,*ALIGNMENT=*Left
.do average goodies
              move          Fininv(yearcount) to form5
              move          FinAR(Yearcount) to Form114
              Divide        form5 into Form114
              Move          c0 to Average
              add           Form114 to average
              move          MaskAverage to str9
              edit          AVERAGE to str9
              PRTPAGE       Laser;*font=prtpg10,*p=columna:7300,*ALIGNMENT=*Left,"Average:":
                            *p=columnb:7300,*ALIGNMENT=*Right,str9
              move          Fininv(yearcount) to form5
              move          FinAp(Yearcount) to Form114
              Divide        form5 into Form114
              Move          c0 to Average
              add           Form114 to average
              move          MaskAverage to str9
              edit          AVERAGE to str9
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7425,*ALIGNMENT=*Right,str9,*ALIGNMENT=*Left
              If            (yearcount = 2)
              move          FinAR(1) to EndARA
              move          FinAR(2) to EndARB
              call          percent
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif
              If            (yearcount = 3)
debug1
              move          c0 to endara                .zero it out and we will begin
              move          c1 to n2
              unpack        holdtoday into str4,str2
              move          str2 to trackmonth
              Loop
              until         (n2 = Trackmonth+1)
              add           Receivables(2,n2) to EndARA
              add           c1 to n2
              repeat
              move          FinAR(3) to EndARB
              call          percent
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif
              RETURN
.............................................................................................................

LoadArray
.begin patch 3.1
               clear          str4
               clear          str2
.end patch 3.1
              unpack        RESULTKEY into str4,str2
.begin patch 3.1
              if              (str4 = "" or str2 = "")
              return
              endif
.end patch 3.1
              Move          str4 to n4
              move          str2 to trackmonth
              compare       n4 to Holdyear
              if            Not equal
              move          n4 to Holdyear
              add           c1 to yearcount
              endif
              move          TOTINV to invcount(yearcount,trackmonth)
              move          TOTAP to payables(yearcount,trackmonth)
              move          TOTAr to Receivables(yearcount,trackmonth)
              return
.............................................................................................................
.NEWYEAR  LOAD      TOTINV FROM MNTH OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
.                   SEPX,OCTX,NOVX,DECX
.         LOAD      TOTAP FROM MNTH OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
.                   NOV,DEC
.         LOAD      TOTAR FROM MNTH OF JANR,FEBR,MARR,APRR,MAYR,JUNR,JULR,AUGR:
.                   SEPR,OCTR,NOVR,DECR
..         WRITAB    OUTPUT,RN;*TAB,MNTH,HOLDYR,TOTINV,TOTAP,TOTAR,015,003
.         WRITAB    OUTPUT,RN;*TAB,MNTH,HOLDYR,TOTINV,TOTAP,TOTAR
.         ADD       "1",RN
.         ADD       "1",MNTH
.         COMPARE   "13",MNTH
.         GOTO      NEWYEAR IF NOT EQUAL
..         WRITAB    OUTPUT,RN;*TAB,"19",HOLDYR,FININV,FINAP,FINAR,015,003
.         WRITAB    OUTPUT,RN;*TAB,holdce,HOLDYR,FININV,FINAP,FINAR
.         ADD       "1",RN
.         MOVE      YR,HOLDYR
.
..THE FOLLOWING ROUTINE MOVES ZEROES INTO THE WORK FIELDS IN BETWEEN DOING
..NAMES IN THE NEWS AND COMPUNAME.
..
.ZEROFILL STORE     ZERO INTO INDEX OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
.                   NOV,DEC
.         STORE     ZERO INTO INDEX OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
.                   SEPX,OCTX,NOVX,DECX
.         STORE     ZERO INTO INDEX OF JANR,FEBR,MARR,APRR,MAYR,JUNR,JULR,AUGR:
.                   SEPR,OCTR,NOVR,DECR
.         ADD       "1",INDEX
.         COMPARE   "13",INDEX
.         GOTO      ZEROFILL IF NOT EQUAL
.         MOVE      "1",INDEX
.         MOVE      "0",FININV
.         MOVE      "0",FORM9R
.         MOVE      "0",FORM9P
.         MOVE      "0",FORM7
.         MOVE      "0",FORM9
.         MOVE      "0",ACCREC
.         MOVE      "0",ACCPAY
.         MOVE      "0",FINAP
.         MOVE      "0",FINAR
.         MOVE      "01",MNTH
.. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "273",RN         'ADD 13 TO THIS VARIABLE EVERY JAN.'
..         COMPARE   "290",RN         'ADD 13 TO THIS VARIABLE EVERY JAN.'
... /////////////////////////////////////////////////////////////////////////////
..         GOTO      READ1 IF NOT EQUAL
..
..'ARMST' IS CLOSED
..
.END1     WEOF      OUTPUT,RN
.         CLOSE     OUTPUT
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "234",RN           'ADD 13 TO THIS LITERAL EVERY JAN."
.. /////////////////////////////////////////////////////////////////////////////
.         CMATCH    "X",FINAL
.         GOTO      CHECK IF NOT EQUAL
.         OPEN      OUTPUT,"\\nins1\e\data\text\ARMST.dat"
..
......DISPLAYS TIME, DATE, AND HEADINGS;
.
DISP
         DISPLAY   "Confidential",B7,B7,B7,TITLE,B5,B5,B5,B5,"Date",B2:
                   MO2,"/",DA2,"/",YR2;
 DISPLAY B7,B7,B7,B7,B7,B7,B7,B7,B7,B7,"Time",B2,HRSTRT,":",MINSTRT," ",SECSTRT
. ******CHANGE YEARS IN FOLLOWING LINES******;
.         DISPLAY   B8,B7,"------1996------",B3," --------1997---------":
.                   B4,"---------1998---------";
.         DISPLAY   B1,"Month",B4,B3,B3,"Inv",B4,"AR/AP",B5,B3,"Inv",B4:
.                   "AR/AP",B4,"Chng",B3,B2,"Inv",B5,"AR/AP",B4,"Chng";
..
.. PRINT LOCAL
..         PRINT     *F,033,033,"H,P,FR3;",033,046,"l8C",*FLUSH:
..                   *F,*N:
.         print     hpptch,HPTOP,*f
.         print     *n,*N,"Confidential",B7,B7,B7,TITLE,B5,B5,B5,B5,"Date",B2;
.         PRINT     MO2,"/",DA2,"/",YR2
.         PRINT   *L,B7,B7,B7,B7,B7,B7,B7,B7,B7,B7,"Time",B2,HRSTRT,":";
.         PRINT     MINSTRT," ",SECSTRT
.. ******CHANGE YEARS IN FOLLOWING LINES******
.         PRINT     *L,B4,B4,B7,"------1999------",B3," --------2000---------";
.         PRINT     B4,"---------2001---------"
.         PRINT     B1,"Month",B4,B3,B3,"Inv",B4,"AR/AP",B5,B3,"Inv",B4;
.         PRINT     "AR/AP",B4,"Chng",B3,B2,"Inv",B5,"AR/AP",B4,"Chng"
..
.. .....PRINTS DATA BY MONTH AND YEAR
..
.END2     READTAB   OUTPUT,RN;*TAB,MO,YR,FININVA,FINAPA,FINARA
.         MOVE      MO,NUMMO
.         BRANCH    NUMMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
..
.JAN      MOVE      "January",MONTH
.         GOTO      END3
.FEB      MOVE      "February",MONTH
.         GOTO      END3
.MAR      MOVE      "March",MONTH
.         GOTO      END3
.APR      MOVE      "April",MONTH
.         GOTO      END3
.MAY      MOVE      "May",MONTH
.         GOTO      END3
.JUN      MOVE      "June",MONTH
.         GOTO      END3
.JUL      MOVE      "July",MONTH
.         GOTO      END3
.AUG      MOVE      "August",MONTH
.         GOTO      END3
.SEP      MOVE      "September",MONTH
.         GOTO      END3
.OCT      MOVE      "October",MONTH
.         GOTO      END3
.NOV      MOVE      "November",MONTH
.         GOTO      END3
.DEC      MOVE      "December",MONTH
..
.END3     ADD       "13",RN
.         MOVE      " " TO LASTMOSW
.         READTAB   OUTPUT,RN;*TAB,MO,YR,FININVB,FINAPB,FINARB
.         MOVE      MO2,NUMMO
.         COMPARE   NUMMO,MO
.         GOTO      END3B IF EQUAL
.         GOTO      END4 IF NOT LESS
.         MOVE      FINARB,FIN10
.         ADD       FIN10,FINALSAV
.         GOTO      END4
.END3B    MOVE      FINARB,SAVE
.         DIV       "31",SAVE
.         MOVE      DA2,NUMDA
.         MULT      NUMDA,SAVE
.         ADD       SAVE,FINALSAV
.         MOVE      "Y" TO LASTMOSW
.         MOVE      SAVE TO FINARB1
.END4     ADD       "13",RN
.         READTAB   OUTPUT,RN;*TAB,MO,YR,FININVC,FINAPC,FINARC
..         DISPLAY   015;
.         CALL      PERCENT
.         CMATCH    "X",C
.         GOTO      END5 IF EQUAL
.
..THE FOLLOWING SEQUENCE EDITS THE DATA TO A MORE SUITABLE PRINT FORMAT
..
.         EDIT      FINARA,MASKARA
.         EDIT      FINARB,MASKARB
.         EDIT      FINARC,MASKARC
.         GOTO      END6
.END5     MOVE      "           ",MASKARC
.         EDIT      FINARA,MASKARA
.         EDIT      FINARB,MASKARB
.END6     DISPLAY   MONTH,B5,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB:
.                   "%",B3,FININVC,B1,MASKARC,B2,PCTC,"%";
.         PRINT     MONTH,B5,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB;
.         PRINT     "%",B2,FININVC,B1,MASKARC,B2,PCTC,"%"
.         MOVE      "99999999.99",MASKARA
.         MOVE      "99999999.99",MASKARB
.         MOVE      "99999999.99",MASKARC
.         CMATCH    "X",C
.         GOTO      END7 IF EQUAL
.         EDIT      FINAPA,MASKARA
.         EDIT      FINAPB,MASKARB
.         EDIT      FINAPC,MASKARC
.         GOTO      END8
.END7     MOVE      "           ",MASKARC
.         MOVE      " ",C
.         EDIT      FINAPA,MASKARA
.         EDIT      FINAPB,MASKARB
.END8     DISPLAY   B20,MASKARA,B9,MASKARB,B14,MASKARC;
.         PRINT     B20,MASKARA,B9,MASKARB,B14,MASKARC
.         MOVE      "99999999.99",MASKARA
.         MOVE      "99999999.99",MASKARB
.         MOVE      "99999999.99",MASKARC
.         SUB       "25",RN
.. "25" IS A CONSTANT DO NOT CHANGE
.. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "246",RN          'ADD 13 TO THIS LITERAL EVERY JAN.'
.. /////////////////////////////////////////////////////////////////////////////
.         GOTO      END2 IF NOT EQUAL
..
.. .....PRINTS YEARLY TOTALS
..
..         DISPLAY   015;
.         DISPLAY   B1,"             _____ ___________   _____ _______":
.                   "____ ____   _____ ___________ ____";
..         DISPLAY   015;
.         PRINT     B1,"             _____ ___________   _____ _______";
.         PRINT     "____ ____   _____ ___________ ____"
.         READTAB   OUTPUT,RN;*TAB,F2,YR,FININVA,FINAPA,FINARA
.         ADD       "13",RN
.         READTAB   OUTPUT,RN;*TAB,F2,YR,FININVB,FINAPB,FINARB
.         ADD       "13",RN
.         MOVE      "A",OK
.         READTAB   OUTPUT,RN;*TAB,F2,YR,FININVC,FINAPC,FINARC
.         CALL      PERCENT
.         EDIT      FINARA,MASKARA
.         EDIT      FINARB,MASKARB
.         EDIT      FINARC,MASKARC
.         DISPLAY   B2,B6,B6,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB:
.                   "%",B3,FININVC,B1,MASKARC,B1,PCTC,"%";
.         PRINT     B2,B6,B6,FININVA,B1,MASKARA,B3,FININVB,B1,MASKARB,B1,PCTB;
.         PRINT     "%",B2,FININVC,B1,MASKARC,B1,PCTC,"%"
.         MOVE      "99999999.99",MASKARA
.         MOVE      "99999999.99",MASKARB
.         MOVE      "99999999.99",MASKARC
.         EDIT      FINAPA,MASKARA
.         EDIT      FINAPB,MASKARB
.         EDIT      FINAPC,MASKARC
.         DISPLAY   B20,MASKARA,B9,MASKARB,B14,MASKARC;
.         PRINT     B20,MASKARA,B9,MASKARB,B14,MASKARC
.         MOVE      "99999999.99",MASKARA
.         MOVE      "99999999.99",MASKARB
.         MOVE      "99999999.99",MASKARC
..
..THE FOLLOWING SEQUENCE COMPUTES THE AVERAGE INVOICE OF EACH YEAR
..
.         MOVE      FINARA,FORM102
.         MOVE      FININVA,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGA
.         MOVE      FINARB,FORM102
.         MOVE      FININVB,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGB
.         MOVE      FINARC,FORM102
.         MOVE      FININVC,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGC
.         DISPLAY   B14,"Average: ",AVERAGA,B12,AVERAGB,B17,AVERAGC;
.         PRINT     B14,"Average: ",AVERAGA,B12,AVERAGB,B17,AVERAGC
.         MOVE      FINAPA,FORM102
.         MOVE      FININVA,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGA
.         MOVE      FINAPB,FORM102
.         MOVE      FININVB,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGB
.         MOVE      FINAPC,FORM102
.         MOVE      FININVC,FORM5
.         DIV       FORM5,FORM102
.         DIV       "100",FORM102
.         MOVE      FORM102,AVERAGC
.         DISPLAY   B23,AVERAGA,B12,AVERAGB,B17,AVERAGC;
.         PRINT     B23,AVERAGA,B12,AVERAGB,B17,AVERAGC
.         CALL      TIME
.         REP       zfill,HR
.         REP       zfill,MIN
.         DISPLAY   "*** THIS REPORT INCLUDES ADJUSTMENTS ***",B20:
.                   B4,B3,"End Time",B1,HR,":",MIN," ",SEC
..         DISPLAY   014;
.         PRINT     "*** THIS REPORT INCLUDES ADJUSTMENTS ***",B20;
.         PRINT     B4,B3,"End Time",B1,HR,":",MIN," ",SEC
.         MOVE      "01",TAB
.         MOVE      "NAMES IN THE NEWS",TITLE
.         CLOSE     OUTPUT
.         CMATCH    " ",FINAL
..         STOP      IF EQUAL
.         GOTO      STOP IF EQUAL
.         OPEN      OUTPUT,"\\nins1\e\data\text\ARMST.dat"
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "234",RN        .'ADD 13 TO THIS LITERAL EVERY JAN.'
.. /////////////////////////////////////////////////////////////////////////////
.         MOVE      " ",FINAL
.         MOVE      "0",FINALSAV
.         MOVE      "0",SAVE
.         MOVE      "0",FIN10
.         GOTO      DISP
..
.end patch 3.0
. .....ZERO FILLS TOTAL FIELDS
.
.begin patch 3.0
.ZERO     MOVE      "0",TOTINV
.         MOVE      "0",COUNT1
.         MOVE      "0",COUNT2
.         MOVE      "0",COUNT3
.         MOVE      "0",TOTAP
.         MOVE      "0",FININV
.         MOVE      "0",FINAP
.         MOVE      "0",FININVA
.         MOVE      "0",FININVB
.         MOVE      "0",FININVC
.         MOVE      "0",FINAPA
.         MOVE      "0",FINAPB
.         MOVE      "0",FINAPC
.         MOVE      "0",FINARA
.         MOVE      "0",FINARB
.         MOVE      "0",FINARC
.         RETURN
.end patch 3.0
.
. .....READS TIME AND DATE FROM 'ARCCLOCK'
.
TIME
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
.begin patch 3.0
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
.end patch 3.0
         MOVE      MM TO MO
         MOVE      DD TO DA
         MOVE      YY TO YR
         CLOCK     TIME TO TIME   (DIM 8)
         UNPACK    TIME INTO HR,ANS,MIN,ANS,SEC
         MOVE      HR,NUMHR
         COMPARE   "12",NUMHR
         GOTO      TIME4 IF EQUAL
         GOTO      TIME3 IF LESS
         MOVE      "PM",SEC
         SUB       "12",NUMHR
         MOVE      NUMHR,HR
         RETURN
.TIME3    CLOSE     TIME
TIME3
         MOVE      "AM",SEC
         RETURN
.TIME4    CLOSE     TIME
TIME4
         MOVE      "PM",SEC
         RETURN
.
. .....FIGURES PERCENTAGE CHANGE
.
.begin patch 3.0
.call with   EndARA and EndARB
.return with percent diff in PCTB
PERCENT
.         MOVE      FINARA,FINAR
.         MOVE      FINARB,ENDAR
              If            (EndARA = c0 and EndARB = c0)
              move          b5 to pctb
              clear         PCTB
              return
              endif
         COMPARE   EndARA,ENDARB
         GOTO      PERCENT2 IF NOT LESS
         MOVE      EndARB,WORK
         DIV       EndARA,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "-",PCTB
         APPEND    PCT,PCTB
         append    "%" to PCTB
         RESET     PCTB
         GOTO      PERCENT3
PERCENT2
         MOVE      EndARB TO WORK
         DIV       EndARA INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "+",PCTB
         APPEND    PCT,PCTB
         append    "%" to PCTB
         RESET     PCTB
.
.THE FOLLOWING ROUTINE CHECKS A FIELD CALLED 'OK' FOR AN 'A'. THIS FIELD IS A
.SIGNAL WHICH SIGNIFIES THE END OF THE FILE AND TELLS THE PROGRAM TO MOVE THE
.PREVIOUS YEAR'S TOTAL AT THE SAME POINT TO DATE TO A TOTAL FIELD SO THE ACTUAL
.CHANGE TO DATE CAN BE RECORDED
.
PERCENT3 CMATCH    "A",OK
         GOTO      PERCENT4 IF NOT EQUAL
         MOVE      FINALSAV,EndARA
         MOVE      " ",OK
         GOTO      PERCENT5
PERCENT4 CMATCH    "Y" TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FINARB1 TO EndARA
         MOVE      " " TO LASTMOSW
         GOTO      PERCENTY
PERCENTX MOVE      FINAR(2),EndARA
PERCENTY
         Compare   c0,FINAR(3)
         GOTO      PERCENT7 IF EQUAL
PERCENT5 MOVE      FINAR(3),EndARB
         Compare   C0,FINAR(3)
         GOTO      PERCENT7 IF EQUAL
         COMPARE   EndARA,EndARB
         GOTO      PERCENT6 IF NOT LESS
         MOVE      EndARB,WORK
         DIV       EndARA,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "-",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
         RETURN
PERCENT6
.PERCENT6 MOVE      FINAR,WORK
.         DIV       EndARB,WORK
         MOVE      EndARB TO WORK
         DIV       EndARA INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "+",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
         RETURN
PERCENT7 CLEAR     PCTC
.begin patch 3.0
.         REP       "0 ",FINARC
.         REP       "0 ",FINAPC
.         REP       "0 ",FININVC
.end patch 3.0
         MOVE      "X",C
         RETURN
debugger
                              return
.
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:24,"***** INVOCPRT *****",*R,*R:
.begin patch 3.0
                   *P12:24,"NAMES IN THE NEWS";
.                   *P12:24,"NAMES IN THE NEWS",*P40:24,"|",*P56:24,"COMPUNAME";
.DISPLAY2 DISPLAY   *R,*P40:24,"|";
.         ADD       "1",COUNT
.         COMPARE   "21",COUNT
.         GOTO      DISPLAY2 IF NOT EQUAL
.DISPLAY3 DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
          DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
.         COMPARE   "42",V
.         RETURN    IF EQUAL
.         ADD       "41",V
.         GOTO      DISPLAY3
              return
.end patch 3.0
.
. ...ERROR DISPLAY;
.
IOERROR
         DISPLAY   *P1:24,*EL,*HON," ",FMESG," COULD NOT BE FOUND";
         BEEP
         KEYIN     *P78:24,ANS;
         CMATCH    "Q",ANS
         GOTO      IOERROR IF NOT EQUAL
         GOTO      STOP
.
NOINV
         MOVE      "FIRST INVOICE ## ",FMESG
         DISPLAY   *P1:24,*EL,*HON," ",FMESG,FIRSTN," COULD NOT BE FOUND";
         BEEP
         KEYIN     *P78:24,ANS;
         CMATCH    "Q",ANS
         GOTO      NOINV IF NOT EQUAL
.STOP     PRINT     *FLUSH
.begin patch 3.0
STOP
.STOP     PRINT     *F,033,033,"H,P,FR1;",033,046,"l8C",*FLUSH
.         SPLCLOSE
.         CHAIN     "NORD0011"
              call          time
              PrtPage       laser;*FONT=prtpg10,*P=7000:8000,"Time: ",Time
              PrtClose      Laser

.begin patch 3.50
.                move    C0,N9
.                move    "                                        ",APIFileName
.                clear   APIFileName
..                pack    APIFileName,"C:\WORK\",str55,hexzero
.                pack    APIFileName,"C:\WORK\PDF\",str55,hexzero
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
..                                if (time3 > 5500) .55 Seconds Maximum
.                                if (time3 > 12000) .120 Seconds Maximum
.                                         break
.                                endif
.              repeat
          Pause     "3"
.end patch 3.50

          Move      "Here is your INV history PDF File",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "Dherric@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\pdf\invprt.pdf",MailAttach
          call      SendMail

.         move    "Here is your PDF File",SmtpSubject Subject
..   Set the text message that is send with the attachments
.        move    str55,SmtpTextMessage(1)   Array <Text message >
.        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.;        move    "NTS2",SmtpEmailServer                   Address of email serverc
.        move    "NTS4",SmtpEmailServer                   Address of email serverc
.        clear   smtpemailaddress
..begin patch 3.33
.        append  "JoseDuenas",SmtpEmailAddress
..        append  "DiegoMontoya",SmtpEmailAddress
.        append  "@nincal.com",SmtpEmailAddress
.        reset   smtpemailaddress
..        move    userlogn,SmtpUserName                                User name
.        move    "JoseDuenas",SmtpUserName                                User name
..        move    "DiegoMontoya",SmtpUserName                                User name
..   Set the destinations of the email. Max 100 (Mime spec)
.        move    smtpemailaddress,SmtpDestinations(1,1)
.        MOVe       "DavidHerrick@nincal.com",SmtpDestinations(2,1)
..        move    userlogn,SmtpDestinations(1,2)
.        move    "JoseDuenas",SmtpDestinations(1,2)
.        move    "David Herrick",SmtpDestinations(2,2)
..        move    "DiegoMontoya",SmtpDestinations(1,2)
..end patch 3.33
.        move    "2",SmtpDestIndexLast                          originators UserName
.        move    "Invprt.pdf",SmtpAttachments(1,1)                     Attached file name
.;        move    "c:\work",SmtpAttachments(1,2)           Path to attached file name
.        move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.        move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.        clear   SmtpLogFile                                         'Clear' disables the LogFile
.        move    "1",SmtpProgress                                    Enable progress bars
.        call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.        if not equal
.                pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
.                        "Status Code ",SmtpStatus," - ",SmtpStatusText
.                move    "PDF File not found",SmtpSubject Subject
.                move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.                call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.        endif
          winshow
          pause     "10"
.end patch 3.0
.begin patch 4.18
.begin patch 3.50
.          call      pdf995auto0
.end patch 3.50
.end patch 4.18

.begin patch 3.36
.         CHAIN     "ninv0005N"
.end patch 3.36

         shutdown  "CLS"
         stop
.begin patch 3.3
.         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
          INclude   NInvAcdio.inc
.end patch 3.3
         INCLUDE   NADJIO.inc
.begin patch 3.0
          INCLUDE          PRTPAGEIO.INC
.end patch 3.0
         INCLUDE   COMLOGIC.inc


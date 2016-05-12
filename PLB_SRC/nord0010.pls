...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INC       OSLSTEAM.inc
.         IFZ       PC
.ERROR    DIM       30
.         XIF
...............................................................................
Release  init      "4.33"              DLH    New year
Reldate   Init      "2016 Jan 4"
.Release  init      "4.32"              DLH    New year
.Reldate   Init      "2015 Jan 5"
.Release  init      "4.31"              DLH    New year
.Reldate   Init      "2014 Jan 2"
.Release  init      "4.30"              DLH    Sunbelt PDF
.Reldate   Init      "2013 April 22"
.Release  init      "4.26"              DLH    New year
.Reldate   Init      "03 Jan 2013"
.Release  init      "4.25"              DLH    New year
.Reldate   Init      "03 Jan 2012"
.Release  init      "4.24"              DLH    New year
.Reldate   Init      " Jan 2011"
.Release  init      "4.23"              DLH    Remove Kelly
.Reldate   Init      "25 May 2010"
.Release  init      "4.22"              DLH    turn off seperate NIN/PL New year 09 Jan 10
.Reldate   Init      "09 Jan 2009"
.Release  init      "4.21"              JD    year 2009
.Reldate   Init      "09 Jan 2009"
.Release  init      "4.20"              DLH  09Jan08 send copies to SHerene,Joey
.Release  init      "4.19"              DLH  07Jan08 year 2008
.Release  init      "4.18"              DLH  27Apr07 pdf995 auto check
.Release  init      "4.17"              JD  05Jan07 year 2007
.Release  init      "4.16"              JD  20Nov2006 Update mailsend to JD
.Release  init      "4.15"             DLH 11Aug2006 Convert to Mailsend
.Release  init      "4.14"             JD changes for year 2006.
.Release  init      "4.13"            JD changes for year 2005.
.Release  init      "4.12"            ASH  06AUG2004  Logo Conversion
.Release  init      "4.11"            DLH 28May2004 update to pdf995 printer
.Release  init      "4.1"            changes for year 2004.
.Release  init      "4.0"            DLH  Play with Isam output file
.                                   add array(s)
.                                   convert to PrtPage
.                                   create and Mail PDF file
DATE     DIM       8
TIME     DIM       8
YR       FORM      2         POS 2-3    YEAR
MO       FORM      2         POS 5-6    MONTH
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
MIN      DIM       2         POS 17-18  MINUTE
. F1     DIM       1         POS 19-19  FILLER
SEC      DIM       2         POS 20-21  SECOND
MO1      DIM       2
DA1      DIM       2
YR1      DIM       2
CE       DIM       2
CE1      DIM       2
CE2      DIM       2
HOLDCE   DIM       2
.
TAB      FORM      "01"      TAB POSITIONS
TAB1     FORM      "42"
F2       DIM       2         FILLER
V1       FORM      "15"      SPLIT SCREEN POSITION
V2       FORM      "26"      SPLIT SCREEN POSITION
. ***************CHANGE FIRST' ' NUMBERS IN JAN EVERY YEAR********************
**********. 50000 FIRST LR OF 1981.***********
**********. 50693 FIRST LR OF 1982.***********
**********. 68843 FIRST LR OF 1983.***********
**********. 87861 FIRST LR OF 1984.***********
.**********.109956 FIRST LR OF 1985.***********
.* ********.000100 FIRST LR OF 1986.***********
.**********.024744  FIRST LR OF 1987.***********
.**********.051937  FIRST LR OF 1988.***********
.**********.078953 FIRST LR 1989. **************
.**********.106639 FIRST LR 1990. **************
.**********.135371 FIRST LR 1991. *************
.**********.158020 FIRST LR 1992. *************
.**********.185570 FIRST LR 1993. *************
.**********.211248 FIRST LR 1994. *************
.**********.234738 FIRST LR 1995. *************
.**********.257868 FIRST LR 1996. *************
.**********.280833 FIRST NIN LR OF 1997
.**********.305233 FIRST NIN LR OF 1998
.**********.330753 FIRST NIN LR OF 1999
.**********.362964 FIRST NIN LR OF 2000
.**********.350000 FIRST NIN LR OF 2001
.**********.643810 FIRST NIN LR OF 2009
.**********.686400 FIRST NIN LR OF 2010
.FIRSTN   INIT      "370000"  Start here - what is the firstn lr really?
.begin patch 4.21a -DLH
.FIRSTN   INIT      "500000"  Start here - what is the firstn lr really?
.end patch 4.21a -DLH
FIRSTN   INIT      "686400"  
* *****************************************************************************
.
FINORDA  DIM       5         TOTAL ORDERS PLACED DURING Year 1
FINORDB  DIM       5         TOTAL ORDERS PLACED DURING Year 2
FINORDC  DIM       5         TOTAL ORDERS PLACED DURING Year 3
FINORDD  DIM       5         TOTAL ORDERS PLACED DURING Year 4
FINORDE  DIM       5         TOTAL ORDERS PLACED DURING Year 5
FinordBE Form      5         Total Brokerage Exchange orders for current year
FinordBR Form      5         Total Brokerage Rental orders for current year
FinordLM Form      5         Total List Management orders for current year
FINQTY   FORM      9.2       TOTAL NAMES ORDERED DURING YEAR
ENDQTY   FORM      9.2       PERCENTAGE WORK SPACE
ORDS     FORM      5         WORK FIELD FOR TOTAL LOAD
. *******************CHANGE THE YEARS IN THE FOLLOWING LINES***********
FINQTYA  DIM       9         TOTAL NAMES ORDERED DURING Year 1
FINQTYB  DIM       9         TOTAL NAMES ORDERED DURING Year 2
FINQTYC  DIM       9         TOTAL NAMES ORDERED DURING Year 3
FINQTYD  DIM       9         TOTAL NAMES ORDERED DURING Year 4
FINQTYE  DIM       9         TOTAL NAMES ORDERED DURING Year 5
FinQtyBE Form      9         Total Brokerage Exchange Names for current year
FinQtyBR Form      9         Total Brokerage Rental Names for current year
FinQtyLM Form      9         Total List Management Names for current year
FINQTYD1 Form      9         USED FOR PARTIAL MONTH % CHANGE CALC.
PCT1     DIM       5         % CHANGE IN Year
PCT      DIM       3         PRINTS PERCENTAGE
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
PCT2     DIM       5         % CHANGE IN Year
WORK     FORM      9.4       WORK SPACE
FINALSAV FORM      9
.begin patch 4.0
          INCLUDE          PRTPAGEDD.INC

results       IFile         Keylen=6

resultvars    list
ResultKey     dim           6        1-6   ccyymm          order date
BRcount       form          5        7-11  BROKERAGE rental number of orders
BRQty         form          9       12-20  BROKERAGE rental NUMBER of Names
BEcount       form          5       21-25  BROKERAGE Exchange number of orders
BEQty         form          9       26-34  BROKERAGE Exchange NUMBER of Names
LMRcount      form          5       35-39  List Management rental number of orders
LMRQty        form          9       40-48  List Management rental NUMBER of Names
LMEcount      form          5       49-53  List Management Exchange number of orders
LMEQty        form          9       54-62  List Management Exchange NUMBER of Names
              listend
.
STARTYEAR     FORM          4
HoldYear      form          4
YEARCOUNT     FORM          1       .WHICH YEAR OF REPORT
TrackMonth    form          2

calcord       form          5
ordmask       init          "ZZ,ZZ9"
calcNames     form          9
Namemask      init          "ZZZ,ZZZ,ZZ9"
TOTORD        form          5
TotNames      Form          9
.
.osflag   form   1       .1=win 95,98, 2=NT

.Row           FORM          5
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
Column1B      Form          "2250"

Column2A      Form          "2625"
Column2B      Form          "3875"

Column3A      Form          "4500"
Column3B      Form          "5750"

Column4A      Form          "6375"
Column4B      Form          "7625"

Column5A      Form          "8250"
Column5B      Form          "9500"
..............................................
.Number of Names Array
. year 1 load array
.subsequent years at month breaks compare previous year to current print, replace month in table
.
NamesByMonth    FORM       9(2,12)        two years worth by month Jan to Dec
.
holdToday       dim         6
.NamesJan        form       9
.NamesFeb        form       9
.NamesMar        form       9
.NamesApr        form       9
.NamesMay        form       9
.NamesJun        form       9
.NamesJul        form       9
.NamesAug        form       9
.NamesSep        form       9
.NamesOct        form       9
.NamesNov        form       9
.NamesDec        form       9
......................................................................................................
.Sort Parameters=======================================================
INDAT    init  "ninord.dat"   .File to be sorted
OUTSRT   init  "NORDVAR.DAT"   .Sorted Output file
ClintSrt init  "202-205"                        .Sort by client #
.SORTVAR   INIT     "\\nins1\e\data\salesref.dat,\\nins1\e\data\salesref.srt;28-72,s=1='06'&73='0901'|1='06'&73='0900'"
SORTFLE   dim    70                            .Var to pack file names of sort
.PRTITLE   DIM    18
.PRTNAME1  DIM    11
.PRTDIR    INIT    "C:\WORK\"
.PRTFILE1  DIM    19
HOLDDATE  DIM    4
        include     winapi.inc
.hexeight integer 4,"4294967295"
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16

.end patch 4.0
COUNT    FORM      2
MNTH    FORM      "01"
SALESBR  FORM      2       SALES TEAM USED TO DIFF. BROKERAGE/MANAGEMENT
SALESNUM DIM       2
CODENUM  FORM      1
FORM9    FORM      9
QTY      FORM      10
OK       DIM       1
SAVE     FORM      9
NUMDA    FORM      2
count1   form      9
SPOOL    DIM       1
LASTMOSW DIM       1            HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR.
TEAM1    INIT      "01"         "JEANETTE"
TEAM2    INIT      "02"         "BECKY M."
TEAM3    INIT      "03"         "SUSAN"
TEAM4    INIT      "04"         "ELAINE"
TEAM5    INIT      "05"         "BONNIE"
TEAM6    INIT      "06"         "LIST MGMT"
TEAM7    INIT      "07"          "SUZIE"
TEAM8    INIT      "08"           "COLD CALLS/INES"
.
. 11/82 .....SPOOLING Y/N
.
.START PATCH 4.12 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 4.12 ADDED LOGIC
         MOVE      "EXIT" TO PF5
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ORDER ANALYSIS PROGRAM" TO STITLE
         move      c1 to v
         TRAP      Stop IF F5
         CALL      PAINT
         MOVE      C1 TO NORDPATH        .SET ACCESS TO ISI.

         move      c3 to nordlock
         CALL      FUNCDISP
.....INITIAL TIME ANS SCREEN DISPLAY;
.
BEGIN
          DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
           if         (program <> "NORD0010")
         MOVE      "NORD0010" TO PROGRAM
          clock     date to today
           else
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
           endif
.         CALL      TIME
.
. .....CHECKS TO SEE IF 'QTYMST' EXISTS
.
         TRAP      Nofile GIVING ERROR IF IO
         OPEN      Results,"ordstats|NINS1:502",exclusive
         TRAPCLR   IO
         TRAP      IO GIVING ERROR IF IO
....................................
.         goto      print
................................
.cleanup old totals before proceeding. This section needs to be updated as time rolls on
.currently we are deleting records from Jan 2003 to date.
.              move          "2003" to n4
.              move          "2004" to n4
.              move          "2005" to n4
.              move          "2006" to n4
.              move          "2007" to n4
.begin patch 4.19
.              move          "2008" to n4
.END patch 4.19
.begin patch 4.21
.begin patch 4.22
.              move          "2009" to n4
.begin patch 4.24
.              move          "2010" to n4
.begin patch 4.25
.              move          "2011" to n4
.begin patch 4.26
.              move          "2012" to n4
.begin patch 4.31
.              move          "2013" to n4
.begin patch 4.32
.              move          "2014" to n4
              move          "2016" to n4
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
.END patch 4.21
              move          c1 to trackmonth
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
cleanloop
              loop
              while         (trackmonth < 13)
              read          results,resultkey;str1
              goto          increment if over
              delete        results
Increment     add           c1 to trackmonth
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
              repeat
.
              move          c1 to trackmonth
              add           c1 to n4
              packKey       resultkey from n4,trackmonth
              rep           zfill in resultkey
.              if            (n4 < 2004)
.              if            (n4 < 2005)
.              if            (n4 < 2007)
..begin patch 4.17
.              if            (n4 < 2008)
..end patch 4.17
.begin patch 4.19
.              if            (n4 < 2009)
.end patch 4.19

.begin patch 4.22
.begin patch 4.24
.begin patch 4.25
.              if            (n4 < 2011)
.begin patch 4.26
.              if            (n4 < 2012)
.begin patch 4.31
.              if            (n4 < 2013)
.begin patch 4.32
.              if            (n4 < 2014)
              if            (n4 < 2016)
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
              goto          cleanloop
              endif
................................
         MOVE      FIRSTN TO NORDFLD
         CALL      NORDKEY
         goto      Print if over
         ADD       "1",COUNT1
         clear              str4
         pack               str4 from oodtec,oodtey
         move               c0 to n4
         move               str4 to n4
.         if                 (n4 < 2003)
.         if                 (n4 < 2004)
.         if                 (n4 < 2005)
.         if                 (n4 < 2006)
..begin patch 4.17
.              if            (n4 < 2007)
..end patch 4.17
.begin patch 4.19
.              if            (n4 < 2008)
.end patch 4.19
.begin patch 4.22
.begin patch 4.24
.              if            (n4 < 2010)
.              if            (n4 < 2011)
.begin patch 4.25 ***********
.begin patch 4.26
.              if            (n4 < 2012)
.begin patch 4.31
.              if            (n4 < 2013)
.begin patch 4.32
.              if            (n4 < 2014)
              if            (n4 < 2016)
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
         goto                read1
         endif
         DISPLAY   *PV1:6,COUNT1,*P25:9,OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   B1,OLRN
         goto      read2
.
.
.
READ1    CALL      NORDKS
         GOTO      Print IF OVER
         ADD       "1",COUNT1
         DISPLAY   *PV1:6,COUNT1,*P25:9,OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   B1,OLRN
         clear              str4
         pack               str4 from oodtec,oodtey
         move               c0 to n4
         move               str4 to n4
.         if                 (n4 < 2003)
.         if                 (n4 < 2004)
.         if                 (n4 < 2005)
.         if                 (n4 < 2006)
..begin patch 4.17
.         if                 (n4 < 2007)
..end patch 4.17
.begin patch 4.19
.         if                 (n4 < 2008)
.end patch 4.19
.begin patch 4.22
.begin patch 4.24
.              if            (n4 < 2010)
.begin patch 4.25 ***********
.              if            (n4 < 2011)
.begin patch 4.26
.              if            (n4 < 2012)
.begin patch 4.31
.              if            (n4 < 2013)
.begin patch 4.32
.              if            (n4 < 2014)
              if            (n4 < 2016)
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
         goto                read1
         endif
.
read2
          if        (Ostat <> "0" and OSTAT <> "B")
          goto      read1
          endif
.
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         COMPARE   c0 TO N9
         GOTO      READ1 IF EQUAL
.dh goes wally
               IF             (n9 > 800000 & Ostat <> "B")
               call           SendDaveEmail
               endif
.
         packkey   ResultKey from oodtec,oodtey,oodtem
          read     results,ResultKey;ResultKey,BRcount,BRQty,BEcount:
               BEQty,LMRcount,LMRQty,LMEcount,LMEQty
          if       over
          packkey   ResultKey from oodtec,oodtey,oodtem
              MOVE          C0 TO bRCOUNT
              MOVE          C0 TO bRQTY
              MOVE          C0 TO bECOUNT
              MOVE          C0 TO bEQTY
              MOVE          C0 TO lMRCOUNT
              MOVE          C0 TO lMECOUNT
              MOVE          C0 TO lMRQTY
              MOVE          C0 TO lMEQTY
          Write    results,ResultKey;ResultKey,BRcount,BRQty,BEcount:
               BEQty,LMRcount,LMRQty,LMEcount,LMEQty
          endif
................................................................
.DO EXCHANGE/RENTAL CALCS.
         MOVE      c0 TO SALESBR
         PACK      SALESNUM FROM OSALES10,OSALES
         MOVE      SALESNUM TO SALESBR
...............................................................................
.NOTE THIS TABLE NEEDS TO BE ADJUSTED WHEN EVER SALES PERSONNEL CHANGES.
...............................................................................
.CONVERT SALESPERSONS TO SALES TEAMS.
.
.
.                                       2   5  3  4  4  6    ??  1  8
.   LOAD      SALESNUM FROM SALESBR OF LISA,BO,SA,EM,NP,LSTM,??,JC,MS
.                                       1    2  3  4  5  6   7   8  9
.
.                   1  5  7  3  7   ?   2   ?   1  3  5  3  7
.                   ??,JP,JE,TF,SMM,???,BM,???,BT,MD,LM,LT,SB
.                   10 11 12 13 14  15  16  17 18 19 20 21 22
.TEAM1 JEANETTE
.TEAM2 REBECCA
.TEAM3 SUSAN A
.TEAM4 ELAINE
.TEAM5 BONNIE
.TEAM6 LIST MAN.
.TEAM7 SUSIE MM
.TEAM8 COLD CALLS
.
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM2,TEAM5,TEAM3,TEAM4:
                   TEAM4,TEAM6,TEAM4,TEAM1:
                   TEAM8,TEAM1,TEAM5,TEAM7:
                   TEAM3,TEAM7,TEAM7,TEAM2:
                   TEAM5,TEAM1,TEAM6,TEAM5,TEAM3,TEAM7
         MOVE      SALESNUM TO SALESBR
         MOVE      c0 TO CODENUM
         MOVE      OELCODE TO CODENUM
         BRANCH    CODENUM OF OK,OKEX,OKEX
OK       BRANCH    SALESBR OF BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1:
                   MANAGE1
         GOTO      BRKAGE1
OKEX
         BRANCH    SALESBR OF BRKAGE,BRKAGE,BRKAGE,BRKAGE,BRKAGE,MANAGE
.
.BRKAGE - BROKERAGE SIDE MAY BE RENT EXCHANGE OR SPLIT.
BRKAGE
         MOVE      c0 TO FORM9
         MOVE      OEXQTY TO FORM9
         COMPARE   c0 TO FORM9
         GOTO      OKEXA IF EQUAL
         MOVE      N9 TO FORM9
         MOVE      OEXQTY TO N9
         SUB       N9 FROM FORM9           GET RENTAL PORTION
         MOVE      OEXQTY TO N9
         add       n9 to BEqty
         add       form9 to BRqty
         add       c1 to BRCount
         GOTO      WRite
BRKAGE1
         move      c0 to form9
         move      oqty to form9
         add       form9 to BRqty
         add       c1 to BRCount
         GOTO      write
.
.MANAGE1 - RENTAL MANAGEMENT SIDE.
MANAGE1
         move      c0 to n9
         move      oqty to n9
         ADD       N9,LMRqty
         ADD       C1 TO LMRcount
         GOTO      write
.
.OKEXA EXCHANGE/BROKAGE
OKEXA
         ADD       N9,BEQTY
         ADD       C1 TO BEcount
         GOTO      write
.MANAGE - MANAGEMENT SIDE MAY BE RENT EXCHANGE OR SPLIT.
MANAGE
.
         MOVE      c0 TO FORM9
         MOVE      OEXQTY TO FORM9
         COMPARE   c0 TO FORM9
         GOTO      OKEXB IF EQUAL
         MOVE      N9 TO FORM9
         MOVE      OEXQTY TO N9
         SUB       N9 FROM FORM9           GET RENTAL PORTION
         MOVE      OEXQTY TO N9
         ADD       N9,LMEqty
         ADD       FORM9,LMRqty
         add       c1 to LMRcount
         GOTO      write
.
.OKEXB EXCHANGE/MANAGED
OKEXB
         move      c0 to n9
         move      oqty to n9
         ADD       N9,LMEqty
         ADD       C1 TO LMEcount
         GOTO      write
.
write
          Update   results;ResultKey,BRcount,BRQty,BEcount:
               BEQty,LMRcount,LMRQty,LMEcount,LMEQty

         goto       read1
.................................................................................
.begin patch 4.0
Print
.begin patch 4.18
.begin patch 4.3
.          call      pdf995auto
.end patch 4.3
.end patch 4.18

.Find out system information
               call           Getwinver
.;               getinfo system,str6
.;        unpack  str6 into str1,str1
.;        move    C0,osflag
.;        if (str1 = "3" or str1 = "4")
.;                move    C1,osflag
.;        elseif (str1 = "1" | str1 = "5")
.;                move    C2,osflag
.;        endif
.;debug
.                        if (osflag = c2)         .nt
.                                PRTOPEN Laser,"","\\nins1\e\data\orderprt.lst"
.                                PRTOPEN Laser,"\\NTS0\Laser8","\\nins1\e\data\orderprt.lst"

                        move    "ordprt" to  str25
.;                        PRTOPEN Laser,"Acrobat Distiller",str25
.begin patch 4.3
.                        PRTOPEN Laser,"PDF995",str25
                        pack    str55,"c:\work\pdf\ordprt.pdf"
                        PRTOPEN Laser,"PDF:",str55
.                        pack    str55,str25,".pdf"
                        MOve  str55,Mailattach      
.end patch 4.3
.                       elseif (osflag = c1)         .win 95 98
.                                PRTOPEN Laser,"","\\nins1\e\data\orderprt.lst"
.                                PRTOPEN Laser,"Laser8","\\nins1\e\data\orderprt.lst"
.                        else   .(osflag = c0)         .Don't know prompt for printer
.                                PRTOPEN Laser,"","\\nins1\e\data\orderprt.lst"
.                        endif
............
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
.                   *p=3925:250,*font=prtpg24B,"Names  ":
.                   *font=prtpg24I," in the News":
.                   *PENSIZE=10,*p=3550:550,*Line=7050:550:
.                   *p=3925:650,*font=prtpg10,"C    A    L    I    F    O    R    N    I    A       I    N    C.":
.                   *p=2800:7900,*font=prtpg10,"1300 Clay Street, 11th Floor, ":
.                    "Oakland, CA 94612-1429 ","·":
.                    " 415-989-3350 ","·"," Fax 415-433-7796"

...............

.START PATCH 4.12 REPLACED LOGIC
.         prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
.                         *FONT=prtpg24B,*p=3850:50,"Names ":
.                    *font=prtpg24I," in the News":
.                    *FONT=prtpg10,*P=9500:150,"DATE: ",tODAY:
.                    *p=1475:1100,"2000":
.                    *p=3225:1100,"2001":
.                    *p=4975:1100,"2002":
.                    *p=6725:1100,"2003":
.                    *p=8475:1100,"2004":
.                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
          prtpage   laser;*FONT=prtpg10,*p=1:150,"Confidential":
                    *Pictrect=*off,*PICT=0:1000:3800:10100:NINLogo:
                    *FONT=prtpg10,*P=9500:150,"DATE: ",tODAY:
.                   *p=1475:1100,"2000":
.                   *p=3225:1100,"2001":
.                   *p=4975:1100,"2002":
.                   *p=6725:1100,"2003":
.                   *p=8475:1100,"2004":
.                   *p=1475:1100,"2001":
.                   *p=1475:1100,"2002":
.begin patch 4.17
.                   *p=1475:1100,"2003":
.                   *p=3225:1100,"2004":
.                   *p=4975:1100,"2005":
.                   *p=6725:1100,"2006":
..begin patch 4.17
.                   *p=8475:1100,"2007":
..end patch 4.17
.                   *p=1475:1100,"2004":
.                   *p=3225:1100,"2005":
.                   *p=4975:1100,"2006":
.                   *p=6725:1100,"2007":
.                   *p=8475:1100,"2008":
.end patch 4.19
.begin patch 4.22
.                    *p=1475:1100,"2005":
.                    *p=3225:1100,"2006":
.                    *p=4975:1100,"2007":
.                    *p=6725:1100,"2008":
.                    *p=8475:1100,"2009":
.begin patch 4.22
.                    *p=1475:1100,"2006":
.                    *p=3225:1100,"2007":
.                    *p=4975:1100,"2008":
.                    *p=6725:1100,"2009":
.                    *p=8475:1100,"2010":
.begin patch 4.25
.                    *p=1475:1100,"2007":
.                    *p=3225:1100,"2008":
.                    *p=4975:1100,"2009":
.                    *p=6725:1100,"2010":
.                    *p=8475:1100,"2011":
.begin patch 4.26
.                    *p=1475:1100,"2008":
.                    *p=3225:1100,"2009":
.                    *p=4975:1100,"2010":
.                    *p=6725:1100,"2011":
.                    *p=8475:1100,"2012":
.begin patch 4.31
.                    *p=1475:1100,"2009":
.                    *p=3225:1100,"2010":
.                    *p=4975:1100,"2011":
.                    *p=6725:1100,"2012":
.                    *p=8475:1100,"2013":
.begin patch 4.32
.                    *p=1475:1100,"2010":
.                    *p=3225:1100,"2011":
.                    *p=4975:1100,"2012":
.                    *p=6725:1100,"2013":
.                    *p=8475:1100,"2014":
                    *p=1475:1100,"2012":
                    *p=3225:1100,"2013":
                    *p=4975:1100,"2014":
                    *p=6725:1100,"2015":
                    *p=8475:1100,"2016":
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
.end patch 4.21
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
.END PATCH 4.12 REPLACED LOGIC
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1250,*PENSIZE=10,*line=ColumnB:1250
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1250,*PENSIZE=10,*line=columnb:1250
                    move      Column4B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:1250,*PENSIZE=10,*line=columnb:1250
                    move      Column5B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:1250,*PENSIZE=10,*line=ColumnB:1250:
                    *font=prtpg10B,*p=1:1800,"Month":
                    *font=prtpg10,*p=COLUMN1A:1800,"Orders":
                    *font=prtpg10,*p=COLUMN1B:1800,*alignment=*Right,"Names":
                    *p=1000:1950,*PENSIZE=10,*line=2250:1950:
                    *font=prtpg10,*p=column2a:1800,*alignment=*Left,"Orders":
                    *font=prtpg10,*p=column2b:1800,*alignment=*Right,"Names"
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1950,*PENSIZE=10,*line=ColumnB:1950:
                    *font=prtpg10,*p=column3a:1800,*alignment=*Left,"Orders":
                    *font=prtpg10,*p=column3b:1800,*alignment=*Right,"Names"
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1950,*PENSIZE=10,*line=columnb:1950:
                    *font=prtpg10,*p=column4a:1800,*alignment=*Left,"Orders":
                    *font=prtpg10,*p=column4b:1800,*alignment=*Right,"Names"
                    move      Column4B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:1950,*PENSIZE=10,*line=columnb:1950:
                    *font=prtpg10,*p=column5a:1800,*alignment=*Left,"Orders":
                    *font=prtpg10,*p=column5b:1800,*alignment=*Right,"Names"
                     move     Column5B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:1950,*PENSIZE=10,*line=columnb:1950:
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
                    *p=column1a:7200,*PENSIZE=10,*line=column1b:7200
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column2a:7200,*PENSIZE=10,*line=columnb:7200
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column3a:7200,*PENSIZE=10,*line=columnb:7200
                    move      Column4B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column4a:7200,*PENSIZE=10,*line=columnb:7200
                    move      Column5B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:7025,*PENSIZE=10,*line=columnb: 7025:
                    *p=column5a:7200,*PENSIZE=10,*line=columnb:7200:
                    *font=prtpg10,*p=1:7050,"Totals"

.         move        "1999" to startyear
.         move        "2000" to startyear
.         move        "2002" to startyear
.begin patch 4.19
.         move        "2003" to startyear
.         move        "2004" to startyear
.end patch 4.19
.begin patch 4.21
.         move        "2003" to startyear
.begin patch 4.22
.         move        "2005" to startyear
.begin patch 4.24
.         move        "2006" to startyear
.begin patch 4.25
.         move        "2007" to startyear
.begin patch 4.26
.         move        "2008" to startyear
.begin patch 4.31
.         move        "2009" to startyear
.begin patch 4.32
.         move        "2010" to startyear
         move        "2012" to startyear
.end patch 4.32
.end patch 4.31
.end patch 4.26
.end patch 4.24
.end patch 4.22
.end patch 4.21
         move        startyear to holdyear
         move        c1 to Yearcount
         move        c1 to Trackmonth
         PACK        resultkey,startyear,trackmonth     .first year to be read
         rep               zfill in resulTkey
         read       results,ResultKey;resultvars
         add        BRCOUNT to calcord
         add        BECOUNT to calcord
         add        LMRCOUNT to calcord
         add        LMECOUNT to calcord
         add        BRQTY to calcnames
         add        BeQTY to calcnames
         add        LMrQTY to calcnames
         add        LMeQTY to calcnames
         call       PrintMoDet

PRintLoop
         LOOP
         UNTIL      (YEARCOUNT = 6)
         readks     results;resultvars
         if         over

         call       PrintYrTot
         goto       stop
         endif
         unpack     ResultKey into str4,str2
         move       str2 to trackmonth
         move       c0 to n4
         move       str4 to n4
         if         (N4 = holdyear)
         add        BRCOUNT to calcord
         add        BECOUNT to calcord
         add        LMRCOUNT to calcord
         add        LMECOUNT to calcord
         add        BRQTY to calcnames
         add        BeQTY to calcnames
         add        LMrQTY to calcnames
         add        LMeQTY to calcnames
.
                if      (yearcount = 5)
                add     Brcount to FinOrdBR
                add     BECount to FinordBE
                add     LMRCount to FinordLM
                add     LMECount to FinordLM
                Add     BRqty to FinQtyBR
                add     BEQty to FinQtyBE
                add     LMRQty to FinQtyLm
                add     LMEQty to FinQtyLM
                endif
         call       PrintMoDet
         else
         call       PrintYrTot
         move       BRCOUNT to calcord
         add        BECOUNT to calcord
         add        LMRCOUNT to calcord
         add        LMECOUNT to calcord
         Move       BRQTY to calcnames
         add        BeQTY to calcnames
         add        LMrQTY to calcnames
         add        LMeQTY to calcnames
                if      (yearcount = 5)
                add     Brcount to FinOrdBR
                add     BECount to FinordBE
                add     LMRCount to FinordLM
                add     LMECount to FinordLM
                Add     BRqty to FinQtyBR
                add     BEQty to FinQtyBE
                add     LMRQty to FinQtyLm
                add     LMEQty to FinQtyLM
                endif
         call       PrintMoDet
         endif
         REPEAT

STOP
              prtclose      laser
                move    C0,N9
                move    "                                        ",APIFileName
.begin patch 4.3
.                        pack    str55,str25,".pdf"
.                clear   APIFileName
..                pack    APIFileName,"C:\WORK\",str55,hexzero
.                pack    APIFileName,"C:\WORK\PDF",str55,hexzero
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
..                                if (time3 > 5500)       .55 Seconds Maximum           .....Telecomm is a pud
.                                if (time3 > 12000) .120 Seconds Maximum
.
.                                         break
.                                endif
.              repeat
.end patch 4.3

.
.begin patch 4.15
          Move      "Here is your Order History PDF File",MailSubjct
.begin patch 4.16
.         Move      "DiegoMontoya@nincal.com",MailFrom
.         Move      "DiegoMontoya@nincal.com",MailTo
          Move      "Creques@nincal.com",MailFrom
.begin patch 4.20   
.         Move      "JoseDuenas@nincal.com,Dherric@nincal.com,LynBunch@nincal.com",MailTo
.         Move      "JoseDuenas@nincal.com,Dherric@nincal.com,JackForder@nincal.com,ShereneKelly@nincal.com,JoeyGamache@nincal.com",MailTo
          Move      "Dherric@nincal.com,SusanAnstrand@nincal.com",MailTo
.end patch 4.20     
.end patch 4.16
.         Move      "DavidHerrick@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\pdf\ordprt.pdf",MailAttach
          call      SendMail
.       move    "Here is your PDF File",SmtpSubject Subject
.   Set the text message that is send with the attachments
.        move    str55,SmtpTextMessage(1)   Array <Text message >
.        move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
.        move    "NTS4",SmtpEmailServer                   Address of email serverc
.        clear   smtpemailaddress
..        append  "JoseDuenas",SmtpEmailAddress
.        append  "DiegoMontoya",SmtpEmailAddress
.        append  "@nincal.com",SmtpEmailAddress
.        reset   smtpemailaddress
..        move    userlogn,SmtpUserName                                User name
..        move    "JoseDuenas",SmtpUserName                                User name
.        move    "DiegoMontoya",SmtpUserName                                User name
..   Set the destinations of the email. Max 100 (Mime spec)
.        move    smtpemailaddress,SmtpDestinations(1,1)
..        move    userlogn,SmtpDestinations(1,2)
..        move    "JoseDuenas",SmtpDestinations(1,2)
.        move    "DiegoMontoya",SmtpDestinations(1,2)
.        move    "1",SmtpDestIndexLast                          originators UserName
.        move    "ordprt.pdf",SmtpAttachments(1,1)                     Attached file name
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
         pause     "30"

.end patch 4.0
.begin patch
.begin patch 4.18
.begin patch 4.3
.          call      pdf995auto0
.end patch 4.3
.end patch 4.18

.begin patch 4.22
.;         CHAIN     "NINV0005"
.         CHAIN     "NOrD0010N"
          shutdown
.end patch 4.22
.End patch
         STOP
         STOP
.begin patch 4.0
PrintMoDet
              move          c0 to row
              load          row from trackmonth of row1,row2,row3,row4,row5,row6:
                            row7,row8,row9,row10,row11,row12
              move          c0 to columnA
              move          c0 to columnB
              Load          COLUMNA from yearcount of Column1a,Column2a,Column3a,Column4a,Column5a
              Load          COLUMNB from yearcount of Column1b,Column2b,Column3b,Column4b,Column5b
          move          ordmask to str6
              edit          calcord to str6
              move          namemask to str11
              edit          calcnames to str11
              Prtpage      laser;*font=prtpg10,*p=columna:row,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,str11
              add           calcord to totord
              add           calcnames to totnames
              move          calcnames to NamesbyMonth(2,trackmonth)
              if            (yearcount > 1)
              move          NamesbyMonth(1,trackmonth) to FinQty
              if            (holdToday = resultkey)          .last month of report
                   if         (trackmonth = 2)
                   move       "28" to n2
                   endif

                   if         (trackmonth = 1 or trackmonth = 3 or trackmonth = 5 or trackmonth = 7 or trackmonth = 8 or trackmonth = 10  or trackmonth = 12 )
                   move       "31" to n2
                   endif

.                  if         (trackmonth = 4 or trackmonth = 6 or trackmonth = 8 or trackmonth = 9 or trackmonth = 11)
                   if         (trackmonth = 4 or trackmonth = 6 or trackmonth = 9 or trackmonth = 11)
                   move       "30" to n2
                   endif
              unpack          today into mm,str1,dd,str1,yy
.so lets prorate
              divide          N2 into Finqty
              move          dd to n2
              mult          n2 by Finqty
              move          Finqty to NamesbyMonth(1,trackmonth) .save for Prorated year calc
              endif
.              Load          Finqty from NamesbyMonth(1,trackmonth)
              move          calcnames to Endqty
              call          percent
               move         Columnb to ColumnC
               add          "75" to columnC
               PrtPage      Laser;*p=Columnc:row,*alignment=*left,pct1
              endif
.
.              if            (yearcount = 1)
.              store         calcnames into trackmonth of NamesJan,NamesFeb,NamesMar,NamesApr,NamesMay:
.                                 NamesJun,NamesJul,NamesAug,NamesSep,NamesOct,NamesNov,NamesDec
.              elseif        (yearcount > 1)
.              Load          Finqty from trackmonth of NamesJan,NamesFeb,NamesMar,NamesApr,NamesMay:
.                                 NamesJun,NamesJul,NamesAug,NamesSep,NamesOct,NamesNov,NamesDec
.              move          calcnames to Endqty
.              call          percent
.               move         Columnb to ColumnC
.               add          "75" to columnC
.               PrtPage      Laser;*p=Columnc:row,*alignment=*left,pct1
.               store         calcnames into trackmonth of NamesJan,NamesFeb,NamesMar,NamesApr,NamesMay:
.                                 NamesJun,NamesJul,NamesAug,NamesSep,NamesOct,NamesNov,NamesDec
.               endif
.
              move          c0 to calcord
              move          c0 to calcnames
              add           c1 to trackmonth
              return
PrintYrTot
              store         totord into yearcount of FinordA,FinordB,FinordC,FinordD,FinordE
              store         totNames into yearcount of FinQtyA,FinQtyB,FinQtyC,FinQtyD,FinQtyE
              add           c1 to yearcount
.
              if            (yearcount < 6)
              move          NamesbyMonth(2,1) to NamesbyMonth(1,1)
              move          NamesbyMonth(2,2) to NamesbyMonth(1,2)
              move          NamesbyMonth(2,3) to NamesbyMonth(1,3)
              move          NamesbyMonth(2,4) to NamesbyMonth(1,4)
              move          NamesbyMonth(2,5) to NamesbyMonth(1,5)
              move          NamesbyMonth(2,6) to NamesbyMonth(1,6)
              move          NamesbyMonth(2,7) to NamesbyMonth(1,7)
              move          NamesbyMonth(2,8) to NamesbyMonth(1,8)
              move          NamesbyMonth(2,9) to NamesbyMonth(1,9)
              move          NamesbyMonth(2,10) to NamesbyMonth(1,10)
              move          NamesbyMonth(2,11) to NamesbyMonth(1,11)
              move          NamesbyMonth(2,12) to NamesbyMonth(1,12)
              move          c0 to NamesbyMonth(2,1)
              move          c0 to NamesbyMonth(2,2)
              move          c0 to NamesbyMonth(2,3)
              move          c0 to NamesbyMonth(2,4)
              move          c0 to NamesbyMonth(2,5)
              move          c0 to NamesbyMonth(2,6)
              move          c0 to NamesbyMonth(2,7)
              move          c0 to NamesbyMonth(2,8)
              move          c0 to NamesbyMonth(2,9)
              move          c0 to NamesbyMonth(2,10)
              move          c0 to NamesbyMonth(2,11)
              move          c0 to NamesbyMonth(2,12)
              endif
              move          ordmask to str6
              edit          totord to str6
              move          namemask to str11
              edit          totnames to str11
              Prtpage      laser;*font=prtpg10,*p=columna:7050,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,str11
              move          c0 to totord
              move          c0 to totnames
              MOVE          STR4 TO HOLDYEAR
              if            (yearcount > 2)
                        if            (yearcount = 3)
                        move           Finqtya to finqty
                        move           Finqtyb to endqty
                        call           percent
                        elseif        (yearcount = 4)
                        move           Finqtyb to finqty
                        move           Finqtyc to endqty
                        call           percent
                        elseif        (yearcount = 5)
                        move           Finqtyc to finqty
                        move           Finqtyd to endqty
                        call           percent
                        elseif        (yearcount = 6)
.
                            move          c1 to n2
                            move          c0 to FinqtyD1
                            Loop
                            until          (n2 = trackmonth)
                            Add            NamesByMonth(1,N2) to FinQtyD1
                        add            c1 to n2
                            repeat
                        move           FinqtyD1 to finqty
                        move           Finqtye to endqty
                        call           percent
                endif
               move     Columnb to ColumnC
               add      "100" to columnC
               PrtPage  Laser;*p=Columnc:7050,*alignment=*left,pct1
                      If      (YearCount = 6)
                        move          ordmask to str6
                        edit          FinOrdBR to str6
                        move          namemask to str11
                        edit          FinQtyBR to str11
                        Prtpage      laser;*font=prtpg10,*p=Column4a:7300,*ALIGNMENT=*Left,"Brokerage Rental -":
                                           *p=columna:7300,*ALIGNMENT=*Left,str6:
                                     *font=prtpg10,*p=columnb:7300,*ALIGNMENT=*Right,str11
                        move          ordmask to str6
                        edit          FinOrdBe to str6
                        move          namemask to str11
                        edit          FinQtyBe to str11
                        Prtpage      laser;*font=prtpg10,*p=Column4a:7450,*ALIGNMENT=*Left,"Brokerage Exchange -":
                                           *p=columna:7450,*ALIGNMENT=*Left,str6:
                                     *font=prtpg10,*p=columnb:7450,*ALIGNMENT=*Right,str11
                        move          ordmask to str6
                        edit          FinOrdLM to str6
                        move          namemask to str11
                        edit          FinQtyLM to str11
                        Prtpage      laser;*font=prtpg10,*p=Column4a:7600,*ALIGNMENT=*Left,"List Management -":
                                           *p=columna:7600,*ALIGNMENT=*Left,str6:
                                     *font=prtpg10,*p=columnb:7600,*ALIGNMENT=*Right,str11
                        endif
              endif
                        return
.end patch 4.0
. .....FIGURES PERCENTAGE CHANGE
.
.percent      Enter with Finqty = 1st of two years to be compared
.                        Endqty = 2nd of two years to be compared
.             Exit with  PCT1 & PCT2
PERCENT
         COMPARE   FINQTY,ENDQTY
         GOTO      PERCENT2 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCT1
         APPEND    "-",PCT1
         APPEND    PCT,PCT1
         append    "%" to pct1
         RESET     PCT1
         GOTO      PERCENT3
PERCENT2 MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCT1
         APPEND    "+",PCT1
         APPEND    PCT,PCT1
         append    "%" to pct1
         RESET     PCT1
PERCENT3
.CMATCH    "A",OK
         compare   c6 to yearcount
         GOTO      PERCENT4 IF NOT EQUAL
         MOVE      FINALSAV,FINQTY
         MOVE      b1,OK
         GOTO      PERCENT5
PERCENT4
         CMATCH    "Y" TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FINQTYD1 TO FINQTY
         MOVE      b1 TO LASTMOSW
         GOTO      PERCENTY
PERCENTX
         MOVE      FINQTYD,FINQTY
PERCENTY
         MATCH     "        0",FINQTYC
         GOTO      PERCENT7 IF EQUAL
PERCENT5 MOVE      FINQTYE,ENDQTY
         MATCH     "0",FINQTYE
         GOTO      PERCENT7 IF EQUAL
         COMPARE   FINQTY,ENDQTY
         GOTO      PERCENT6 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCT2
         APPEND    "-",PCT2
         APPEND    PCT,PCT2
         append    "%" to pct2
         RESET     PCT2
         RETURN
PERCENT6
.PERCENT6 MOVE      FINQTY,WORK
.         DIV       ENDQTY,WORK
         MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCT2
         APPEND    "+",PCT2
         APPEND    PCT,PCT2
         append    "%" to pct2
         RESET     PCT2
         RETURN
PERCENT7 CLEAR     PCT2
         MOVE      "   " TO PCT2
         REP       "0 ",FINQTYE
         REP       "0 ",FINORDE
         RETURN
......................................................................................................
ERR      DISPLAY   *P1:1,*ES,"RANGE ERROR :",ERROR;
         KEYIN     *P22:1,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      STOP IF EQUAL
         GOTO      ERR
NOfile
         prepare   results,"\\nins1\e\data\text\Ordstats.dat","\\nins1\e\data\index\Ordstats.isi","6","62",exclusive
.         prepare   results,"c:\work\ordstats.dat","c:\work\ordstats.isi","6","62",exclusive
         return
.
IO       DISPLAY   *P1:24,*EL,"IO ERROR ",ERROR;
         STOP
SendDaveEmail
          Move      "Nord0010 - Alert",MailSubjct
           Clear     MailFrom
           Clear    MailAttach
           If       (user = " " or user = "")
           append   "InformationServices",Mailfrom
           else
                    append    user,MailFrom
                    endif
          append    "@nincal.com",MailFrom
          reset     MailFrom
          Clear     MailBody
          Pack      MailBody from Olrn,b1,"LR ##",CRLF,"Oqty INCOME > 800,000 ":
                    b1,Oqty,CRLF
           Move     "DavidHerrick@NINCAL.com",MailTO
          Call      SendMail   


..             move          "Nord0010 - Alert" to SmtpSubject Subject
.              Clear         SmtpTextMessage(1)   Array <Text message >
.                   append         OLRN,SmtpTextMessage(1)   Array <Text message >
.         append         " ",SmtpTextMessage(1)   Array <Text message >
.         append         "LR ##",SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)
..
.         append         "Oqty INCOME > 800,000 ",SmtpTextMessage(2)   Array <Text message >
.         append         b1,SmtpTextMessage(2)   Array <Text message >
.         append         oqty,SmtpTextMessage(2)   Array <Text message >
.              reset          smtpTextMessage(2)
.
.         Move       "2",SmtpTextIndexLast                               Index to last entry in TextMessage array
.         move       "DHerric" to str45
.         move       "David Herrick" to str55
.         call       Mailmesg
               return
.
.begin patch 4.0
           INCLUDE          PRTPAGEIO.INC
.        pack   SortFle,ntwkpath6,indat,comma,prtdir,outsrt
.        pack   taskname,sortfle,";",clintsrt,comma,"s=16=","'",LST,"'","&202>","'",HOLDDATE,"'"
.        Alert  note,"I am going away to do some work!",result
.        sort   taskname
.end patch 4.0
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


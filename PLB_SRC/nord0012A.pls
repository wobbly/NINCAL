...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
.         INC       OSLSTEAM.inc
         INC       OSLSpern.inc
.         IFZ       PC
.ERROR    DIM       30
.         XIF
...............................................................................
Release  init      "1.10"              DLH  . Sunbelt PDF
reldate   Init      "2013 April 25"
.Release  init      "1.0"              DLH  . start dates for commissionable orders - SS,PM,SK
.reldate   Init      "10 August 09"
.Release  init      "1.10"              DLH  New Year
.reldate   Init      "02 Feb 09"
.Release  init      "1.00"              DLH  10Jan08 
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
.**********. 50000 FIRST LR OF 1981.***********
.**********. 50693 FIRST LR OF 1982.***********
.**********. 68843 FIRST LR OF 1983.***********
.**********. 87861 FIRST LR OF 1984.***********
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

Salescount          form      2
results       IFile         Keylen=8

resultvars    list
ResultKey     dim           8        1-8   salesnum+ccyymm          order date
BRcount       form          5        9-13  BROKERAGE rental number of orders
BRQty         form          9       14-22  BROKERAGE rental NUMBER of Names
BEcount       form          5       23-27  BROKERAGE Exchange number of orders
BEQty         form          9       28-36  BROKERAGE Exchange NUMBER of Names
LMRcount      form          5       37-41  List Management rental number of orders
LMRQty        form          9       42-50  List Management rental NUMBER of Names
LMEcount      form          5       51-55  List Management Exchange number of orders
LMEQty        form          9       56-64  List Management Exchange NUMBER of Names
              listend
.
STARTYEAR     FORM          4
HoldYear      form          4
HoldSales   Form        2
YEARCOUNT     FORM          1       .WHICH YEAR OF REPORT
TrackMonth    form          2

calcord       form          5
ordmask       init          "ZZ,ZZ9"
calcNames     form          9
Namemask      init          "ZZZ,ZZZ,ZZ9"
TOTORD        form          5
TotNames      Form          9
.
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
NamesByMonth    FORM       9(2,12)        each with two years worth by month Jan to Dec
.
holdToday       dim         6
......................................................................................................
.Sort Parameters=======================================================
HOLDDATE  DIM    4
        include     winapi.inc
.timestamp1 dim  16
.timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16

.begin patch 1,1
.FIRSTN   INIT      "500000"  Start here - what is the firstn lr really?
FIRSTN   INIT      "650000"  Start here - what is the firstn lr really?
.end patch 1,1
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
.
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
         MOVE      "NORD012A" TO PROGRAM
         MOVE      "EXIT" TO PF5
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ORDER Sales REp ANALYSIS PROGRAM" TO STITLE
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
          clock     date to today
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
.
         TRAP      Nofile GIVING ERROR IF IO
         OPEN      Results,"ordSLSComm",exclusive
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
.begin patch 1.1  
.begin patch 4.19
.              move          "2008" to n4               .change every year
              move          "2009" to n4               .change every year
.END patch 4.19
.end patch 1.1  
          for       Salescount from "0" to "35" using c1

                    For       Trackmonth from "1" to "12" using C1
                    packKey       resultkey from Salescount,n4,trackmonth
                    rep           zfill in resultkey
                    read          results,resultkey;str1
                    if        Not over
                    Delete    Results
                    endif
                    Repeat
          REpeat                        
.cleanloop
.              loop
.              while         (trackmonth < 13)
.              read          results,resultkey;str1
.              goto          increment if over
.              delete        results
.Increment     add           c1 to trackmonth
.              packKey       resultkey from Salescount,n4,trackmonth
.              rep           zfill in resultkey
.              repeat
..
.              move          c1 to trackmonth
.              add           c1 to n4
.              packKey       resultkey from Salescount,n4,trackmonth
.              rep           zfill in resultkey
..              if            (n4 < 2004)
..              if            (n4 < 2005)
..              if            (n4 < 2007)
...begin patch 4.17
..              if            (n4 < 2008)
...end patch 4.17
..begin patch 4.19
.              if            (n4 < 2009)
..end patch 4.19
.
.              goto          cleanloop
.              endif
.              repeat
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
.begin patch 1.1  
.              if            (n4 < 2008)
              if            (n4 < 2009)
.end patch 1.1  
.end patch 4.19
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
.begin patch 1.1  
.              if            (n4 < 2008)
              if            (n4 < 2009)
.end patch 1.1  
.end patch 4.19
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
          clear     Str2
          pack      str2 from Osales10,osales
          pack      str8 from oodtec,oodtey,oodtem,oodted
          move      str8 to n8
.NIN division
          if        ((str2 = "21" or str2 = "29" or str2 = "27" or str2 = "11") & n8 < "20090720")
          goto      Read1
          endif
.Sherene
          if        (str2 = "06" & n8 < "20090803")
          goto      Read1
          endif

          unpack    olrn,str1,str5
          if        (str1 = "M" & (Osales = "" or Osales = " "))
          move      c2,osales10
          move      c7,osales
          endif
          if        (str1 = "B" & (Osales = "" or Osales = " "))
          move      c3,osales10
          move      c5,osales
          endif
         packkey   ResultKey from Osales10,Osales,oodtec,oodtey,oodtem
         rep        Zfill,ResultKey
          read     results,ResultKey;ResultKey,BRcount,BRQty,BEcount:
               BEQty,LMRcount,LMRQty,LMEcount,LMEQty
          if       over
         packkey   ResultKey from Osales10,Osales,oodtec,oodtey,oodtem
         rep        Zfill,ResultKey
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
...............................................................................
.
LOADOK   
         MOVE      c0 TO CODENUM
         MOVE      OELCODE TO CODENUM
         BRANCH    CODENUM OF OK,OKEX,OKEX
OK       BRANCH    SALESBR OF BRKAGE1,MANAGE1,BRKAGE1,BRKAGE1,BRKAGE1:                      .1-5
                   MANAGE1,BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1:                                 .6-10
                   BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1:                                 .11-15
                   BRKAGE1,BRKAGE1,BRKAGE1,MANAGE1,BRKAGE1:                                 .16-20
                   BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1:                                 .21-25
                   BRKAGE1,MANAGE1,MANAGE1,BRKAGE1,BRKAGE1:                                 .26-30
                   BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1,BRKAGE1                                 .31-35
         GOTO      BRKAGE1
OKEX
         BRANCH    SALESBR OF BRKAGE,MANAGE,BRKAGE,BRKAGE,BRKAGE:                      .1-5
                   MANAGE,BRKAGE,BRKAGE,BRKAGE,BRKAGE:                                 .6-10
                   BRKAGE,BRKAGE,BRKAGE,BRKAGE,BRKAGE:                                 .11-5
                   BRKAGE,BRKAGE,BRKAGE,MANAGE,BRKAGE:                                 .16-20
                   BRKAGE,BRKAGE,BRKAGE,BRKAGE,BRKAGE:                                 .21-25
                   BRKAGE,MANAGE,MANAGE,BRKAGE,BRKAGE:                                 .26-30
                   BRKAGE,BRKAGE,BRKAGE,BRKAGE,BRKAGE                                 .31-35

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
.begin patch 1.10
.          call      pdf995auto
.end patch 1.10
.end patch 4.18

.Find out system information
               call           Getwinver
.;debug

.begin patch 1.10
.                        move    "ordSlscom" to  str25
.                        PRTOPEN Laser,"PDF995",str25
                        move    "c:\work\pdf\ordSlscom.pdf" to  str55
                        PRTOPEN Laser,"PDF:",str55
.                        pack    str55,str25,".pdf"
.end patch 1.10
prtloop   for       Salescount from "0" to "35" using c1

                    if        (salescount = "21" | salescount = "27" | salescount = "29" | salescount = "06" | salescount = "11")  
          move      Osls0,str25
.Pam 21, Shirley 29, their LM 27, SK = 6
          Load      str25 FROM Salescount OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                   OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                   OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                   OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
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
              move          c0 to NamesbyMonth(1,1)
              move          c0 to NamesbyMonth(1,2)
              move          c0 to NamesbyMonth(1,3)
              move          c0 to NamesbyMonth(1,4)
              move          c0 to NamesbyMonth(1,5)
              move          c0 to NamesbyMonth(1,6)
              move          c0 to NamesbyMonth(1,7)
              move          c0 to NamesbyMonth(1,8)
              move          c0 to NamesbyMonth(1,9)
              move          c0 to NamesbyMonth(1,10)
              move          c0 to NamesbyMonth(1,11)
              move          c0 to NamesbyMonth(1,12)
              move          c0 to BRCOUNT
              move          c0 to calcord
              move          c0 to BECOUNT
              move          c0 to LMRCOUNT
              move          c0 to LMECOUNT
              move          c0 to BRQTY
              move          c0 to calcnames
              move          c0 to BeQTY
              move          c0 to LMrQTY
              move          c0 to LMeQTY

............
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
          if        (salescount <> c0)
          PRTPAGE     Laser;*NewPage
          endif
...............

          prtpage   laser;*FONT=prtpg10,*p=1:150,"Confidential":
                    *Pictrect=*off,*PICT=0:1000:3800:10100:NINLogo:
                    *FONT=prtpg10,*alignment=*Left,*P=9500:150,"DATE: ",tODAY:
                    *p=1:400,*alignment=*Left,"Commission History for : ",Str25:
.begin patch 1.1
                    *p=1475:1100,"2005":
                    *p=3225:1100,"2006":
                    *p=4975:1100,"2007":
                    *p=6725:1100,"2008":
                    *p=8475:1100,"2009":
.en.                 *p=1475:1100,"2004":
.                   *p=3225:1100,"2005":
.                   *p=4975:1100,"2006":
.                   *p=6725:1100,"2007":
.                   *p=8475:1100,"2008":
.end patch 1.1
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
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

.begin patch 1.1
.         move        "2004" to startyear
         move        "2005" to startyear
.end patch 1.1
         move        startyear to holdyear
         move        c0 to Yearcount
         move        c0 to Trackmonth
          Call      PrintLoop
                    endif
          REpeat
          goto      Stop

PRintLoop
          For       Yearcount from c1 to c5 using c1
          if        (yearcount = c1)
          Else
          add       c1,startyear
          endif
                    for       trackmonth from "1" to "12" using c1    
                    PACK        resultkey,salescount,startyear,trackmonth     .first year to be read
                    rep       zfill in resultKey
                    read       results,ResultKey;resultvars
                    if        over
                    move      c0,BRCOUNT
                    move      c0,BECOUNT
                    move      c0,LMRCOUNT
                    move      c0,LMECOUNT
                    move      c0,BRQTY 
                    move      c0,BeQTY 
                    move      c0,LMrQTY
                    move      c0,LMeQTY
                    endif
                    call      printdet
                    repeat
          call      PrintYRtot
.         Add       c1,yearcount
          repeat
          move      c0,FinordBR
          move      c0,FinordBE
          move      c0,FinordLM
          move      c0,FinQTYBR
          move      c0,FinQtyBE
          move      c0,FinQtyLM
          REturn
.        
PRintDet
          UNpack    resultkey into N2,str4
         
.         move       str2 to trackmonth
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
.         call       PrintYrTot
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
          REturn    
.           REPEAT
.         call      printyrtot
.         MOve      Salescount,N2
.       
.         REpeat

STOP
              prtclose      laser
.Begin patch 1.10
.                move    C0,N9
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,"C:\WORK\PDF",str55,hexzero
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
.                                if (time3 > 12000) .120 Seconds Maximum
.
.                                         break
.                                endif
.              repeat
                    Pause     "5"
.end patch 1.10

.
          Move      "Here is your Order History by Salesperson PDF",MailSubjct
          Move      "Creques@nincal.com",MailFrom
.         Move      "JoseDuenas@nincal.com,Dherric@nincal.com,ShereneKelly@nincal.com,JoeyGamache@nincal.com",MailTo
          Move      "DavidHerrick@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\pdf\ordSlscom.pdf",MailAttach
         pause     "30"
          call      SendMail
          winshow
         pause     "30"

.Begin patch 1.10
.         call      pdf995auto0
.end patch 1.10

.         CHAIN     "NINV0005"
.         CHAIN     "NOrD0010N"
          shutdown
         STOP
         STOP
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
              move          calcnames to Endqty
              call          percent
               move         Columnb to ColumnC
               add          "75" to columnC
               PrtPage      Laser;*p=Columnc:row,*alignment=*left,pct1
              endif
.
.
              move          c0 to calcord
              move          c0 to calcnames
.              add           c1 to trackmonth
              return
PrintYrTot
              store         totord into yearcount of FinordA,FinordB,FinordC,FinordD,FinordE
              store         totNames into yearcount of FinQtyA,FinQtyB,FinQtyC,FinQtyD,FinQtyE
.              add           c1 to yearcount
.
              if            (yearcount < 5)                           
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
              if            (yearcount > 1)
                        if            (yearcount = 2)
                        move           Finqtya to finqty
                        move           Finqtyb to endqty
                        call           percent
                        elseif        (yearcount = 3)
                        move           Finqtyb to finqty
                        move           Finqtyc to endqty
                        call           percent
                        elseif        (yearcount = 4)
                        move           Finqtyc to finqty
                        move           Finqtyd to endqty
                        call           percent
                        elseif        (yearcount = 5)
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

                      If      (YearCount = 5)
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
Break     move      N2,HoldSales
.reset some stuff
          return
.......................................................     
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
         compare   c5 to yearcount
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
         prepare   results,"\\nins1\e\data\text\OrdSlsComm.dat","\\nins1\e\data\index\OrdSlsComm.isi","8","64",exclusive
         return
.
IO       DISPLAY   *P1:24,*EL,"IO ERROR ",ERROR;
         STOP
SendDaveEmail
          Move      "Nord0012A - Alert",MailSubjct
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


               return
.
           INCLUDE          PRTPAGEIO.INC
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


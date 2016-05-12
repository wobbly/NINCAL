...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INC       OSLSTEAM.inc
...............................................................................
Release   Init      "1.53"           DLH    YEar change
reldate   Init      "2016 January 5"
.Release   Init      "1.52"           DLH    YEar change
.reldate   Init      "2015 January 5"
.Release   Init      "1.51"           DLH    YEar change
.reldate   Init      "2014 January 2"
.Release   Init      "1.5"           DLH    Sunbelt PDF
.reldate   Init      "2013 May 6"
.Release   Init      "1.41"           DLH    YEar change
.reldate   Init      "01 Feb 2013"
.Release   Init      "1.4"           DLH    YEar change
.reldate   Init      "01 Feb 2011"
.Release   Init      "1.3"           DLH    Remove Kelly
.reldate   Init      "25 May 2010"
.Release   Init      "1.2"           DLH    Year change
.reldate   Init      "02 February 2010"
.Release   Init      "1.1"           DLH    Year change
.reldate   Init      "02 March 2009"
.Release   Init      "PRE"           DLH end play replacement for nord0011 ??? currently only LM looks at
.process only nord0010 updates the info
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
Pass      Form      1
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 4.12 ADDED LOGIC
         MOVE      "NORD0011" TO PROGRAM
         MOVE      "EXIT" TO PF5
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ORDER DEPT ANALYSIS PROG" TO STITLE
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
          Move      C1,Pass
.
         TRAP      Nofile GIVING ERROR IF IO
         OPEN      Results,"ordstats|NINS1:502",exclusive
         TRAPCLR   IO
         TRAP      IO GIVING ERROR IF IO
....................................
.         goto      print
................................
Print
.begin patch 1.5
.          call      pdf995auto
               call           Getwinver

                        move    "ordprt" to  str25
.                        PRTOPEN Laser,"PDF995",str25
                        pack    str55,"c:\work\pdf\",str25,".pdf"
                        PRTOPEN Laser,"PDF:",str55
.                        pack    str55,str25,".pdf"
.end patch 1.5
............
          if        (pass = c1)
          move      "List Management",str15
          Else
          Move      "Brokerage",str15
          endif
.Year Change          
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
          prtpage   laser;*FONT=prtpg10,*p=1:150,"Confidential":
                    *Pictrect=*off,*PICT=0:1000:3800:10100:NINLogo:
                    *p=1:250,Str15:
                    *FONT=prtpg10,*P=9500:150,"DATE: ",tODAY:
                    *p=1475:1100,"2010":
                    *p=3225:1100,"2011":
                    *p=4975:1100,"2012":
                    *p=6725:1100,"2013":
                    *p=8475:1100,"2014":
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

.begin patch 1.1 Year Change
.begin patch 1.51 Year Change
.         move        "2009" to startyear
.begin patch 1.52 Year Change
.         move        "2010" to startyear
         move        "2012" to startyear
.end patch 1.52 Year Change
.end patch 1.51 Year Change
.         move        "2004" to startyear
.end patch 1.1
         move        startyear to holdyear
         move        c1 to Yearcount
         move        c1 to Trackmonth
         PACK        resultkey,startyear,trackmonth     .first year to be read
         rep               zfill in resulTkey
         read       results,ResultKey;resultvars
          if        (pass = c1)
         add        LMRCOUNT to calcord
         add        LMECOUNT to calcord
         add        LMrQTY to calcnames
         add        LMeQTY to calcnames
          Else
         add        BRCOUNT to calcord
         add        BECOUNT to calcord
         add        BRQTY to calcnames
         add        BeQTY to calcnames
          endif
         call       PrintMoDet

PRintLoop
         MOve       C0,COUNT1
         add        c2,v1
         LOOP
         UNTIL      (YEARCOUNT = 6)
         readks     results;resultvars
         if         over

         call       PrintYrTot
         goto       stop
         endif
         ADD       "1",COUNT1
         DISPLAY   *PV1:7,COUNT1,b1,resultkey
         
         unpack     ResultKey into str4,str2
         move       str2 to trackmonth
         move       c0 to n4
         move       str4 to n4
         if         (N4 = holdyear)
          if        (pass = c1)
          add        LMRCOUNT to calcord
          add        LMECOUNT to calcord
          add        LMrQTY to calcnames
          add        LMeQTY to calcnames
          Else
          add        BRCOUNT to calcord
          add        BECOUNT to calcord
          add        BRQTY to calcnames
          add        BeQTY to calcnames
          endif
.
          if      (yearcount = 5)
                    if        (pass = c1)
                    add     LMRCount to FinordLM
                    add     LMECount to FinordLM
                    add     LMRQty to FinQtyLm
                    add     LMEQty to FinQtyLM
                    Else
                    add     Brcount to FinOrdBR
                    add     BECount to FinordBE
                        Add     BRqty to FinQtyBR
                    add     BEQty to FinQtyBE

                    endif
          endif
         call       PrintMoDet
         else
         call       PrintYrTot
                              if        (pass = c1)
                              MOve       LMRCOUNT to calcord
                              add        LMECOUNT to calcord
                              Move        LMrQTY to calcnames
                              add        LMeQTY to calcnames
                              Else
                              move       BRCOUNT to calcord
                              Move       BRQTY to calcnames
                              add        BECOUNT to calcord
                              add        BeQTY to calcnames
                              endif

                if      (yearcount = 5)
                    if        (pass = c1)
                    add     LMRCount to FinordLM
                    add     LMECount to FinordLM
                    add     LMRQty to FinQtyLm
                    add     LMEQty to FinQtyLM
                    Else
                    add     Brcount to FinOrdBR
                    add     BECount to FinordBE
                        Add     BRqty to FinQtyBR
                    add     BEQty to FinQtyBE
                    endif
                endif
         call       PrintMoDet
         endif
         REPEAT

STOP
              prtclose      laser
.begin patch 1.5
.                move    C0,N9
.                move    "                                        ",APIFileName
.                clear   APIFileName
..                pack    APIFileName,"C:\WORK\",str55,hexzero
.                pack    APIFileName,"C:\WORK\PDF",str55,hexzero
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
..                        clock   timestamp,timestamp2
                                move    timestamp2,time2
.                                sub     time1,time2,time3
..                                if (time3 > 5500)       .55 Seconds Maximum           .....Telecomm is a pud
.                                if (time3 > 12000) .120 Seconds Maximum
.
.                                         break
.                                endif
.              repeat
          pause     "10"
.end patch 1.5

.
          Move      "Here is your LM Order History PDF File",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "Dherric@nincal.com,SusanAnstrand@nincal.com",MailTo
.         Move      "Dherric@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\pdf\ordprt.pdf",MailAttach
          call      SendMail
          winshow
         pause     "30"

.begin patch 1.5
.          call      pdf995auto0
.end patch 1.5
.         CHAIN     "NINV0005"
          
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
         return
.
IO       DISPLAY   *P1:24,*EL,"IO ERROR ",ERROR;
         STOP
SendDaveEmail
          Move      "Nord0011 - Alert",MailSubjct
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

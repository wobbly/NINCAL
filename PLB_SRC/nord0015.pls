...............................................................................
.Rewrite of Nord0011
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INC       OSLSTEAM.inc
.Five year order volume broken out by Brokerage and Management
...............................................................................
Release   Init      "2.10"           DLH    add YTD for years 2-4
reldate   Init      "2016 May 3"
.Release   Init      "2.02"           DLH    use date from dsinit, year change
.reldate   Init      "2015 July 1"
.Release   Init      "2.01"           DLH    YEar change
.reldate   Init      "2015 January 31"
.Release   Init      "2.00"           DLH    breakout LM Rent/exch qty dump to excel
.reldate   Init      "2014 August xx"
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
.begin patch 2.0
FinordLR Form      5         Total List Management rental orders for current year
FinordLE Form      5         Total List Management exchange orders for current year
.end patch 2.0
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
.begin patch 2.0
FinQtyLR Form      9         Total List Management Rental Names for current year
FinQtyLE Form      9         Total List Management Exchange Names for current year
.end patch 2.0
FINQTYD1 Form      9         USED FOR PARTIAL MONTH % CHANGE CALC.
PCT1     DIM       5         % CHANGE IN Year
PCT      DIM       3         PRINTS PERCENTAGE
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
PCT2     DIM       5         % CHANGE IN Year
WORK     FORM      9.4       WORK SPACE
FINALSAV FORM      9
.Begin patch 2.0
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft                        integer 4,"0xffffefDD"
xlTop                         integer 4,"0xffffefc0"
AlignRight                    integer 4,"0xffffefc8"
xlAlignCenter                 integer 4,"0xffffeff4"
xlBottom                      integer      4,"0xffffeff5"
XlLineStyleDBl                Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range

xlEdgeTop      integer 4,"0x8"                       .8
xlEdgeBottom   integer 4,"0x9"                    .9
xlDouble       integer 4,"0xffffefe9"                 .-4119

VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidthA variant
xlColumnWidthB variant
xlColumnWidthCats variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant

HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_I4                         EQU                  3           .4 byte integer
Zoom70                        VARIANT
ZoomRowMax            form    9      //Max row before a soft page break
ZoomRowMaxPage1   form        9      //Max row before a soft page break on page 1
CellRowCnt          FORM      "46"
CellRowCnt1         FORM      "54"
CurCellNum FORM 5
SheetIndex                              variant
sheetcount          integer   1
Index     Form      5
DimRow1       Dim             5
DimRow2       Dim             5
DimRow3       Dim             5
ExRange1      Dim             7
ExRange2      Dim             7
ExRange3      Dim            16
xlrange1    automation

.          INCLUDE          PRTPAGEDD.INC
NINLogo    Pict
.end patch 2.0

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
.begin patch 2.0
TOTRORD        form          5
TotRNames      Form          9
TOTEORD        form          5
TotENames      Form          9
.end patch 2.0
.
.osflag   form   1       .1=win 95,98, 2=NT

RowA           FORM          5
.begin patch 2.0
Row1          form          "00007"
Row2          Form          "00011"
Row3          Form          "00015"
Row4          Form          "00019"
Row5          Form          "00023"
Row6          Form          "00027"
Row7          Form          "00031"
Row8          Form          "00035"
Row9          Form          "00039"
Row10         Form          "00043"
Row11         Form          "00047"
Row12         Form          "00051"
.Row1          form          "2175"
.Row2          Form          "2550"
.Row3          Form          "2925"
.Row4          Form          "3300"
.Row5          Form          "3675"
.Row6          Form          "4050"
.Row7          Form          "4425"
.Row8          Form          "4800"
.Row9          Form          "5175"
.Row10         Form          "5550"
.Row11         Form          "5925"
.Row12         Form          "6300"
.end patch 2.0

ColumnA       Dim          5
ColumnB       Dim          5
ColumnC       Dim          5             .used for percentages
Column1A      Init          "C"
Column1B      Init          "D"
Column1C      Init          "E"
Column2A      Init          "F"
Column2B      Init          "G"
Column2C      Init          "H"
Column3A      Init          "J"
Column3B      Init          "K"
Column3C      Init          "L"
Column4A      Init          "N"
Column4B      Init          "O"
Column4C      Init          "P"
Column5A      Init          "R"
Column5B      Init          "S"
Column5C      Init          "T"
.ColumnA       form          5
.ColumnB       form          5
.ColumnC       Form          5             .used for percentages
.
.Column1A      Form          "1000"
.Column1B      Form          "2250"
.
.Column2A      Form          "2625"
.Column2B      Form          "3875"
.
.Column3A      Form          "4500"
.Column3B      Form          "5750"
.
.Column4A      Form          "6375"
.Column4B      Form          "7625"
.
.Column5A      Form          "8250"
.Column5B      Form          "9500"
.end patch 2.0
..............................................
.Number of Names Array
. year 1 load array
.subsequent years at month breaks compare previous year to current print, replace month in table
.
.begin patch 2.0  - create arrays for rent and exchange two Years worth each
RNamesByMonth    FORM       9(2,12)        two years worth REntal by month Jan to Dec Number of orders, number of names
ENamesByMonth    FORM       9(2,12)        two years worth Exchange by month Jan to Dec Number of orders, number of names
.end patch 2.0  - create arrays for rent and exchange two Years worth each
NamesByMonth    FORM       9(2,12)        two years worth by month Jan to Dec Number of orders, number of names
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
         MOVE      "NORD0015" TO PROGRAM
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
           if         (program <> "NORD0015")
         MOVE      "NORD0015" TO PROGRAM
          clock     date to today
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
           else
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
           endif


.          move        "04-30-16",today
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
.begin patch 2.0
.                        pack    str55,"c:\work\pdf\",str25,".pdf"
.                        PRTOPEN Laser,"PDF:",str55
.end patch 2.0
.                        pack    str55,str25,".pdf"
.end patch 1.5
............
          if        (pass = c1)
          move      "List Management",str15
          Else
          Move      "Brokerage",str15
          endif
.begin patch 2.0
.Year Change          
.Open Excel application
        create  ex
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
        create   xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
        create   xlShiftUp,VarType=VT_R8,VarValue="-4162"
        create xlRowHeight,VarType=VT_R8,VarValue="8.00"
        create xlColumnWidth,VarType=VT_R8A,VarValue="0.46"
        create xlColumnWidthA,VarType=VT_R8,VarValue="64.00"
        create xlColumnWidthB,VarType=VT_R8,VarValue="12.00"
        create xlColumnWidthCats,VarType=VT_R8a,VarValue="24.00"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
.        setprop ex,*Visible="True"
              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45

              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin
              setprop sheet.range("k2","k2"),*Value="Confidential",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k2:k2").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k2:k2").Font,*Bold="True"
              setprop sheet.range("k3","k3"),*Value=Str15,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k3:k3").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k3:k3").Font,*Bold="True"
              setprop sheet.range("r2","r2"),*Value="Date:",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("r2:r2").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("r2:r2").Font,*Bold="True"
.set format for the date
              setprop sheet.range("s2","s2"),*Value=Today,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("s2:s2").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("s2:s2").Font,*Bold="True"
                      Sheet.Range("s2:t2").Merge       

                      pack       str4 from cc,yy                             .current year
              setprop sheet.range("r5","r5"),*Value=str4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("r5:r5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("r5:r5").Font,*Bold="True"
                      Sheet.Range("r5:t5").Merge       
                      move       str4,n4
                      sub        c1 from n4
                      move       n4,str4
              setprop sheet.range("n5","n5"),*Value=str4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("n5:n5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("n5:n5").Font,*Bold="True"
                      Sheet.Range("n5:p5").Merge       
                      sub        c1 from n4
                      move       n4,str4
              setprop sheet.range("j5","j5"),*Value=str4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("j5:j5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("j5:j5").Font,*Bold="True"
                      Sheet.Range("j5:l5").Merge       
                      sub        c1 from n4
                      move       n4,str4
              setprop sheet.range("f5","f5"),*Value=str4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("f5:f5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("f5:f5").Font,*Bold="True"
                      Sheet.Range("f5:h5").Merge       
                      sub        c1 from n4
                      move       n4,str4
              setprop sheet.range("c5","c5"),*Value=str4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c5:c5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("c5:c5").Font,*Bold="True"
                      Sheet.Range("c5:d5").Merge       
              setprop sheet.range("A6","A6"),*Value="Month",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("A6:A6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("a6:a6").Font,*Bold="True"
              setprop sheet.range("c6","c6"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c6:c6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("c6:c6").Font,*Bold="True"
              setprop sheet.range("d6","d6"),*Value="Names",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d6:d6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("d6:d6").Font,*Bold="True"

              setprop sheet.range("f6","f6"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("f6:f6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("f6:f6").Font,*Bold="True"
              setprop sheet.range("g6","g6"),*Value="Names",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("g6:g6").Font,*Bold="True"
              setprop sheet.range("h6","h6"),*Value="%",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h6:h6").Font,*Bold="True"

              setprop sheet.range("j6","j6"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("j6:j6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("j6:j6").Font,*Bold="True"
              setprop sheet.range("k6","k6"),*Value="Names",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k6:k6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k6:k6").Font,*Bold="True"
              setprop sheet.range("l6","l6"),*Value="%",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l6:l6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("l6:l6").Font,*Bold="True"

              setprop sheet.range("n6","n6"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("n6:n6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("n6:n6").Font,*Bold="True"
              setprop sheet.range("o6","o6"),*Value="Names",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("o6:o6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("o6:o6").Font,*Bold="True"
              setprop sheet.range("p6","p6"),*Value="%",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("p6:p6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("p6:p6").Font,*Bold="True"

              setprop sheet.range("r6","r6"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("r6:r6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("r6:r6").Font,*Bold="True"
              setprop sheet.range("s6","s6"),*Value="Names",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("s6:s6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("s6:s6").Font,*Bold="True"
              setprop sheet.range("t6","t6"),*Value="%",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("t6:t6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("t6:t6").Font,*Bold="True"

.Left headers months, etc
              setprop sheet.range("A7","A7"),*Value="January",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A7:A7").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A7:A7").Font,*Bold="True"
                      Sheet.Range("A7:A9").Merge       
              setprop sheet.range("b7","b7"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b7:b7").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b7:b7").Font,*Bold="True"
              setprop sheet.range("b8","b8"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b8:b8").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b8:b8").Font,*Bold="True"
              setprop sheet.range("b9","b9"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b9:b9").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b9:b9").Font,*Bold="True"
.draw some borders
        sheet.range("B7:T9").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B9:T9").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B9:T9").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B10:B10").Rows,*RowHeight=xlRowHeight
.Feb              
              setprop sheet.range("A11","A11"),*Value="February",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A11:A11").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A11:A11").Font,*Bold="True"
                      Sheet.Range("A11:A13").Merge     
              setprop sheet.range("b11","b11"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b11:b11").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b11:b11").Font,*Bold="True"
              setprop sheet.range("b12","b12"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b12:b12").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b12:b12").Font,*Bold="True"
              setprop sheet.range("b13","b13"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b13:b13").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b13:b13").Font,*Bold="True"
.draw some borders
        sheet.range("B11:T13").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B13:T13").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B13:T13").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B14:B14").Rows,*RowHeight=xlRowHeight
.March              
              setprop sheet.range("A15","A15"),*Value="March",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A15:A15").Font,*Name="Times New Roman", *Size=16
              setprop sheet.range("A15:A15").Font,*Bold="True"
                      Sheet.Range("A15:A17").Merge     
              setprop sheet.range("b15","b15"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b15:b15").Font,*Name="Times New Roman", *Size=16
              setprop sheet.range("b15:b15").Font,*Bold="True"
              setprop sheet.range("b16","b16"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b16:b16").Font,*Name="Times New Roman", *Size=16
              setprop sheet.range("b16:b16").Font,*Bold="True"
              setprop sheet.range("b17","b17"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b17:b17").Font,*Name="Times New Roman", *Size=16
              setprop sheet.range("b17:b17").Font,*Bold="True"
.draw some borders
        sheet.range("B15:T17").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B17:T17").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B17:T17").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B18:B18").Rows,*RowHeight=xlRowHeight
.April            
              setprop sheet.range("A19","A19"),*Value="April",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A19:A19").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A19:A19").Font,*Bold="True"
                      Sheet.Range("A19:A21").Merge     
              setprop sheet.range("b19","b19"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b19:b19").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b19:b19").Font,*Bold="True"
              setprop sheet.range("b20","b20"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b20:b20").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b20:b20").Font,*Bold="True"
              setprop sheet.range("b21","b21"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b21:b21").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b21:b21").Font,*Bold="True"
.draw some borders
        sheet.range("B19:T21").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B21:T21").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B21:T21").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B22:B22").Rows,*RowHeight=xlRowHeight
.May              
              setprop sheet.range("A23","A23"),*Value="May",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A23:A23").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A23:A23").Font,*Bold="True"
                      Sheet.Range("A23:A25").Merge     
              setprop sheet.range("b23","b23"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b23:b23").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b23:b23").Font,*Bold="True"
              setprop sheet.range("b24","b24"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b24:b24").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b24:b24").Font,*Bold="True"
              setprop sheet.range("b25","b25"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b25:b25").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b25:b25").Font,*Bold="True"
.draw some borders
        sheet.range("B23:T25").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B25:T25").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B25:T25").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B26:B26").Rows,*RowHeight=xlRowHeight
.June              
              setprop sheet.range("A27","A27"),*Value="June",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A27:A27").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A27:A27").Font,*Bold="True"
                      Sheet.Range("A27:A29").Merge     
              setprop sheet.range("b27","b27"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b27:b27").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b27:b27").Font,*Bold="True"
              setprop sheet.range("b28","b28"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b28:b28").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b28:b28").Font,*Bold="True"
              setprop sheet.range("b29","b29"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b29:b29").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b29:b29").Font,*Bold="True"
.draw some borders
        sheet.range("B27:T29").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B29:T29").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B29:T29").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B30:B30").Rows,*RowHeight=xlRowHeight
.July              
              setprop sheet.range("A31","A31"),*Value="July",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A31:A31").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A31:A31").Font,*Bold="True"
                      Sheet.Range("A31:A33").Merge     
              setprop sheet.range("b31","b31"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b31:b31").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b31:b31").Font,*Bold="True"
              setprop sheet.range("b32","b32"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b32:b32").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b32:b32").Font,*Bold="True"
              setprop sheet.range("b33","b33"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b33:b33").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b33:b33").Font,*Bold="True"
.draw some borders
        sheet.range("B31:T33").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B33:T33").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B33:T33").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B34:B34").Rows,*RowHeight=xlRowHeight
.August
              setprop sheet.range("A35","A35"),*Value="August",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A35:A35").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A35:A35").Font,*Bold="True"
                      Sheet.Range("A35:A37").Merge     
              setprop sheet.range("b35","b35"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b35:b35").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b35:b35").Font,*Bold="True"
              setprop sheet.range("b36","b36"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b36:b36").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b36:b36").Font,*Bold="True"
              setprop sheet.range("b37","b37"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b37:b37").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b37:b37").Font,*Bold="True"
.draw some borders
        sheet.range("B35:T37").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B37:T37").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B37:T37").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B38:B38").Rows,*RowHeight=xlRowHeight
.Sept
              setprop sheet.range("A39","A39"),*Value="September",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A39:A39").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A39:A39").Font,*Bold="True"
                      Sheet.Range("A39:A41").Merge     
              setprop sheet.range("b39","b39"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b39:b39").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b39:b39").Font,*Bold="True"
              setprop sheet.range("b40","b40"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b40:b40").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b40:b40").Font,*Bold="True"
              setprop sheet.range("b41","b41"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b41:b41").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b41:b41").Font,*Bold="True"
.draw some borders
        sheet.range("B39:T41").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B41:T41").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B41:T41").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B42:B42").Rows,*RowHeight=xlRowHeight
.Oct
              setprop sheet.range("A43","A43"),*Value="October",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A43:A43").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A43:A43").Font,*Bold="True"
                      Sheet.Range("A43:A45").Merge     
              setprop sheet.range("b43","b43"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b43:b43").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b43:b43").Font,*Bold="True"
              setprop sheet.range("b44","b44"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b44:b44").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b44:b44").Font,*Bold="True"
              setprop sheet.range("b45","b45"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b45:b45").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b45:b45").Font,*Bold="True"
.draw some borders
        sheet.range("B43:T45").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B45:T45").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B45:T45").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B46:B46").Rows,*RowHeight=xlRowHeight
.Nov
              setprop sheet.range("A47","A47"),*Value="November",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A47:A47").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A47:A47").Font,*Bold="True"
                      Sheet.Range("A47:A49").Merge     
              setprop sheet.range("b47","b47"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b47:b47").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b47:b47").Font,*Bold="True"
              setprop sheet.range("b48","b48"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b48:b48").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b48:b48").Font,*Bold="True"
              setprop sheet.range("b49","b49"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b49:b49").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b49:b49").Font,*Bold="True"
.draw some borders
        sheet.range("B47:T49").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B49:T49").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B49:T49").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B50:B50").Rows,*RowHeight=xlRowHeight
.Dec
              setprop sheet.range("A51","A51"),*Value="December",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A51:A51").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A51:A51").Font,*Bold="True"
                      Sheet.Range("A51:A53").Merge     
              setprop sheet.range("b51","b51"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b51:b51").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b51:b51").Font,*Bold="True"
              setprop sheet.range("b52","b52"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b52:b52").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b52:b52").Font,*Bold="True"
              setprop sheet.range("b53","b53"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b53:b53").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b53:b53").Font,*Bold="True"
.draw some borders
        sheet.range("B51:T53").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B53:T53").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B53:T53").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B54:B54").Rows,*RowHeight=xlRowHeight
.Totals
              setprop sheet.range("A55","A55"),*Value="Totals",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A55:A55").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A55:A55").Font,*Bold="True"
                      Sheet.Range("A55:A57").Merge     
              setprop sheet.range("b55","b55"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b55:b55").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b55:b55").Font,*Bold="True"
              setprop sheet.range("b56","b56"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b56:b56").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b56:b56").Font,*Bold="True"
              setprop sheet.range("b57","b57"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b57:b57").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b57:b57").Font,*Bold="True"
.draw some borders
        sheet.range("B55:T57").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B57:T57").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B57:T57").Borders(xlEdgeBottom),*LineStyle=xlDouble
.Set row height
               setprop        sheet.range("B58:B58").Rows,*RowHeight=xlRowHeight
.YTD Totals
              setprop sheet.range("A59","A59"),*Value="YTD Totals",*HorizontalAlignment=xlAlignCenter,*VerticalAlignment=xlAlignCenter
              setprop sheet.range("A59:A59").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A59:A59").Font,*Bold="True"
                      Sheet.Range("A59:A61").Merge     
              setprop sheet.range("b59","b59"),*Value="Rent",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b59:b59").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b59:b59").Font,*Bold="True"
              setprop sheet.range("b60","b60"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b60:b60").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b60:b60").Font,*Bold="True"
              setprop sheet.range("b61","b61"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b61:b61").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b61:b61").Font,*Bold="True"
.draw some borders
        sheet.range("B59:T61").BorderAround using *LineStyle=1,*Weight=2
        setprop sheet.range("B61:T61").Borders(xlEdgeTop),*LineStyle=xlDouble
        setprop sheet.range("B61:T61").Borders(xlEdgeBottom),*LineStyle=xlDouble
              

.        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
.          prtpage   laser;*FONT=prtpg10,*p=1:150,"Confidential":
.                    *Pictrect=*off,*PICT=0:1000:3800:10100:NINLogo:
.                    *p=1:250,Str15:
.                    *FONT=prtpg10,*P=9500:150,"DATE: ",tODAY:
.                    *p=1475:1100,"2010":
.                    *p=3225:1100,"2011":
.                    *p=4975:1100,"2012":
.                    *p=6725:1100,"2013":
.                    *p=8475:1100,"2014":
.                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
.                    move      Column2B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1250,*PENSIZE=10,*line=ColumnB:1250
.                    move      Column3B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1250,*PENSIZE=10,*line=columnb:1250
.                    move      Column4B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:1250,*PENSIZE=10,*line=columnb:1250
.                    move      Column5B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:1250,*PENSIZE=10,*line=ColumnB:1250:
.                    *font=prtpg10B,*p=1:1800,"Month":
.                    *font=prtpg10,*p=COLUMN1A:1800,"Orders":
.                    *font=prtpg10,*p=COLUMN1B:1800,*alignment=*Right,"Names":
.                    *p=1000:1950,*PENSIZE=10,*line=2250:1950:
.                    *font=prtpg10,*p=column2a:1800,*alignment=*Left,"Orders":
.                    *font=prtpg10,*p=column2b:1800,*alignment=*Right,"Names"
.                    move      Column2B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1950,*PENSIZE=10,*line=ColumnB:1950:
.                    *font=prtpg10,*p=column3a:1800,*alignment=*Left,"Orders":
.                    *font=prtpg10,*p=column3b:1800,*alignment=*Right,"Names"
.                    move      Column3B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1950,*PENSIZE=10,*line=columnb:1950:
.                    *font=prtpg10,*p=column4a:1800,*alignment=*Left,"Orders":
.                    *font=prtpg10,*p=column4b:1800,*alignment=*Right,"Names"
.                    move      Column4B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:1950,*PENSIZE=10,*line=columnb:1950:
.                    *font=prtpg10,*p=column5a:1800,*alignment=*Left,"Orders":
.                    *font=prtpg10,*p=column5b:1800,*alignment=*Right,"Names"
.                     move     Column5B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:1950,*PENSIZE=10,*line=columnb:1950:
.                    *font=prtpg10,*p=1:2175,*alignment=*Left,"January":
.                    *font=prtpg10,*p=1:2550,"February":
.                    *font=prtpg10,*p=1:2925,"March":
.                    *font=prtpg10,*p=1:3300,"April":
.                    *font=prtpg10,*p=1:3675,"May":
.                    *font=prtpg10,*p=1:4050,"June":
.                    *font=prtpg10,*p=1:4425,"July":
.                    *font=prtpg10,*p=1:4800,"August":
.                    *font=prtpg10,*p=1:5175,"September":
.                    *font=prtpg10,*p=1:5550,"October":
.                    *font=prtpg10,*p=1:5925,"November":
.                    *font=prtpg10,*p=1:6300,"December":
.                    *p=column1a:7025,*PENSIZE=10,*line=column1b:7025:
.                    *p=column1a:7200,*PENSIZE=10,*line=column1b:7200
.                    move      Column2B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:7025,*PENSIZE=10,*line=columnb:7025:
.                    *p=column2a:7200,*PENSIZE=10,*line=columnb:7200
.                    move      Column3B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:7025,*PENSIZE=10,*line=columnb:7025:
.                    *p=column3a:7200,*PENSIZE=10,*line=columnb:7200
.                    move      Column4B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column4a:7025,*PENSIZE=10,*line=columnb:7025:
.                    *p=column4a:7200,*PENSIZE=10,*line=columnb:7200
.                    move      Column5B to ColumnB
.                    add     "500" to ColumnB
.         prTPAGE    LASER;*FONT=prtpg10,*p=column5a:7025,*PENSIZE=10,*line=columnb: 7025:
.                    *p=column5a:7200,*PENSIZE=10,*line=columnb:7200:
.                    *font=prtpg10,*p=1:7050,"Totals"
.
.begin patch 1.1 Year Change
.begin patch 1.51 Year Change
.         move        "2009" to startyear
.         move        "2010" to startyear
         move        "2012" to startyear
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
.begin patch 2.0
                    add     LMRCount to FinordLR
                    add     LMECount to FinordLE
                    add     LMRQty to FinQtyLR
                    add     LMEQty to FinQtyLE
.end patch 2.0
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
.begin patch 2.0
                    add     LMRCount to FinordLR
                    add     LMECount to FinordLE
                    add     LMRQty to FinQtyLR
                    add     LMEQty to FinQtyLE
.end patch 2.0
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
.begin patch 2.0
.              prtclose      laser
                    pack      Exrange1,"A5"
                    pack      Exrange2,"T57"

          sheet.range(EXrange1,EXrange2).Columns.Autofit
         Setprop Sheet.Range("E1"),*ColumnWidth=xlColumnWidth
         Setprop Sheet.Range("I1"),*ColumnWidth=xlColumnWidth
         Setprop Sheet.Range("M1"),*ColumnWidth=xlColumnWidth
         Setprop Sheet.Range("q1"),*ColumnWidth=xlColumnWidth
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse

          pack      taskname,"c:\work\"                     ."
              setprop ex,*DefaultFilePath=taskname
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF

............................................
SaveAsFileNameSelect
          setmode *mcursor=*arrow
.          ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.                              append  ".xls",taskname
              clear   taskname
                              if        (#ver = c1)
                              pack            Taskname,"c:\work\Ordprt.xlsx"
                              else
                              pack            Taskname,"c:\work\Ordprt.xls"
                              endif                  
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapSaveAsObject if Object
.                book.saveas giving N9 using *Filename=taskname
.                trapclr Object
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapSaveAsObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="FALSE"

..............................................................................................................
.CleanUp
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        Destroy xlRowHeight
        Destroy xlColumnWidth
        Destroy xlColumnWidthCats
        Destroy OTRUE
        Destroy OFALSE
        Destroy TopMargin
        Destroy BottomMargin
        Destroy LeftMargin
        Destroy RightMargin
              setprop ex,*DisplayAlerts=OTRUE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.

        ex.quit
        destroy ex

.end patch 2.0
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
.begin patch 2.0
.          Move      "Here is your LM Order History PDF File",MailSubjct
          Move      "Here is your LM Order History report",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "Dherric@nincal.com,SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com,DeniseHubbard@nincal.com",MailTo
.         Move      "Dherric@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\ordprt.xlsx",MailAttach
          call      SendMail
          winshow
.end patch 2.0
         pause     "30"

.begin patch 1.5
.          call      pdf995auto0
.end patch 1.5
.         CHAIN     "NINV0005"
          
.begin patch 2.02
           shutdown
.end patch 2.02
         STOP
         STOP
PrintMoDet
.begin patch 2.0
              move          c0 to row
              load          rowA from trackmonth of row1,row2,row3,row4,row5,row6:
                            row7,row8,row9,row10,row11,row12
              move          c0 to columnA
              move          c0 to columnB
              Load          COLUMNA from yearcount of Column1a,Column2a,Column3a,Column4a,Column5a
              Load          COLUMNB from yearcount of Column1b,Column2b,Column3b,Column4b,Column5b
          move          ordmask to str6
              edit          calcord to str6
.begin patch 2.0
.REnt

           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange1 from ColumnA,DimRow1            
                    pack            Exrange2 from ColumnB,DimRow1            
                    setprop sheet.range(ExRange1,ExRange1),*Value=LMRcount,*NumberFormat="######0"
                    setprop sheet.range(ExRange2,ExRange2),*Value=LMRQty,*NumberFormat="##,####0"
.Exchange
                      add        c1,rowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange1 from ColumnA,DimRow1            
                    pack            Exrange2 from ColumnB,DimRow1            
                    setprop sheet.range(ExRange1,ExRange1),*Value=LMECount,*NumberFormat="######0"
                    setprop sheet.range(ExRange2,ExRange2),*Value=LMEQty,*NumberFormat="##,####0"
.totals
                      add        c1,rowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange1 from ColumnA,DimRow1            
                    pack            Exrange2 from ColumnB,DimRow1            
                    setprop sheet.range(ExRange1,ExRange1),*Value=calcord,*NumberFormat="######0"
                    setprop sheet.range(ExRange2,ExRange2),*Value=calcnames,*NumberFormat="##,####0"



.              move          namemask to str11
.              edit          calcnames to str11
.              Prtpage      laser;*font=prtpg10,*p=columna:row,*ALIGNMENT=*Left,str6:
.                           *font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,str11
         add        LMRCOUNT,TotRord
         add        LMECOUNT,TotEord
         add        LMrQTY,totRnames                   
         add        LMeQTY,totEnames                   


.end patch 2.0
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
.begin patch 2.0
.                     unpack          today into mm,str1,dd,str1,yy
.                     divide          N2 into Finqty
.                     move          dd to n2
.                     mult          n2 by Finqty
.                     move          Finqty to NamesbyMonth(1,trackmonth) .save for Prorated year calc
                      endif
              move          calcnames to Endqty
.              call          percent
.               move         Columnb to ColumnC
.               add          "75" to columnC
              Load          COLUMNC from yearcount of Column1C,Column2C,Column3C,Column4C,Column5C
.let's use a formula here?
              load          rowA from trackmonth of row1,row2,row3,row4,row5,row6:
                            row7,row8,row9,row10,row11,row12

               if     (yearcount = c5)
                      if            (holdToday = resultkey)          .last month of report
                      move       N2,str2              .number of days in month.
                      call       trim using str2
                      unpack          today into mm,str1,dd,str1,yy
                      call       trim using dd
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-((O",DimRow1,"/",str2,")*",dd,"))/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-((O",DimRow1,"/",str2,")*",dd,"))/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-((O",DimRow1,"/",str2,")*",dd,"))/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

                      else

           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      endif

            Elseif    (yearcount = c4)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

            Elseif    (yearcount = c3)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

            Elseif    (yearcount = c2)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
            endif


.               PrtPage      Laser;*p=Columnc:row,*alignment=*left,pct1
.end patch 2.0
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
.begin patch 2.0
              move          Rnamesbymonth(2,1) to Rnamesbymonth(1,1)
              move          Rnamesbymonth(2,2) to Rnamesbymonth(1,2)
              move          Rnamesbymonth(2,3) to Rnamesbymonth(1,3)
              move          Rnamesbymonth(2,4) to Rnamesbymonth(1,4)
              move          Rnamesbymonth(2,5) to Rnamesbymonth(1,5)
              move          Rnamesbymonth(2,6) to Rnamesbymonth(1,6)
              move          Rnamesbymonth(2,7) to Rnamesbymonth(1,7)
              move          Rnamesbymonth(2,8) to Rnamesbymonth(1,8)
              move          Rnamesbymonth(2,9) to Rnamesbymonth(1,9)
              move          Rnamesbymonth(2,10) to Rnamesbymonth(1,10)
              move          Rnamesbymonth(2,11) to Rnamesbymonth(1,11)
              move          Rnamesbymonth(2,12) to Rnamesbymonth(1,12)
              move          c0 to Rnamesbymonth(2,1)
              move          c0 to Rnamesbymonth(2,2)
              move          c0 to Rnamesbymonth(2,3)
              move          c0 to Rnamesbymonth(2,4)
              move          c0 to Rnamesbymonth(2,5)
              move          c0 to Rnamesbymonth(2,6)
              move          c0 to Rnamesbymonth(2,7)
              move          c0 to Rnamesbymonth(2,8)
              move          c0 to Rnamesbymonth(2,9)
              move          c0 to Rnamesbymonth(2,10)
              move          c0 to Rnamesbymonth(2,11)
              move          c0 to Rnamesbymonth(2,12)
              move          Enamesbymonth(2,1) to Enamesbymonth(1,1)
              move          Enamesbymonth(2,2) to Enamesbymonth(1,2)
              move          Enamesbymonth(2,3) to Enamesbymonth(1,3)
              move          Enamesbymonth(2,4) to Enamesbymonth(1,4)
              move          Enamesbymonth(2,5) to Enamesbymonth(1,5)
              move          Enamesbymonth(2,6) to Enamesbymonth(1,6)
              move          Enamesbymonth(2,7) to Enamesbymonth(1,7)
              move          Enamesbymonth(2,8) to Enamesbymonth(1,8)
              move          Enamesbymonth(2,9) to Enamesbymonth(1,9)
              move          Enamesbymonth(2,10) to Enamesbymonth(1,10)
              move          Enamesbymonth(2,11) to Enamesbymonth(1,11)
              move          Enamesbymonth(2,12) to Enamesbymonth(1,12)
              move          c0 to Enamesbymonth(2,1)
              move          c0 to Enamesbymonth(2,2)
              move          c0 to Enamesbymonth(2,3)
              move          c0 to Enamesbymonth(2,4)
              move          c0 to Enamesbymonth(2,5)
              move          c0 to Enamesbymonth(2,6)
              move          c0 to Enamesbymonth(2,7)
              move          c0 to Enamesbymonth(2,8)
              move          c0 to Enamesbymonth(2,9)
              move          c0 to Enamesbymonth(2,10)
              move          c0 to Enamesbymonth(2,11)
              move          c0 to Enamesbymonth(2,12)
.end patch 2.0
              endif
.begin patch 2.0
              move          ordmask to str6
              edit          totord to str6
              move          namemask to str11
              edit          totnames to str11
.Rent
                      MOve       Row12,RowA
                      add        c4,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                      add        c1,rowA
                    Move            RowA,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                    pack            Exrange3 from ColumnC,DimRow1
          
                      Clear      Taskname
                      pack       taskname from "=",ColumnA,"7+",ColumnA,"11+",ColumnA,"15+",ColumnA,"19+",ColumnA,"23+",ColumnA,"27+",ColumnA,"31+":
                                 ColumnA,"35+",ColumnA,"39+",ColumnA,"43+",ColumnA,"47+",ColumnA,"51"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      Clear      Taskname
                      pack       taskname from "=",ColumnB,"7+",ColumnB,"11+",ColumnB,"15+",ColumnB,"19+",ColumnB,"23+",ColumnB,"27+",ColumnB,"31+":
                                 ColumnB,"35+",ColumnB,"39+",ColumnB,"43+",ColumnB,"47+",ColumnB,"51"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
.Exchange
                      MOve       Row12,RowA
                      add        c5,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                      add        c1,rowA
                    Move            RowA,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      Clear      Taskname
                      pack       taskname from "=",ColumnA,"8+",ColumnA,"12+",ColumnA,"16+",ColumnA,"20+",ColumnA,"24+",ColumnA,"28+",ColumnA,"32+":
                                 ColumnA,"36+",ColumnA,"40+",ColumnA,"44+",ColumnA,"48+",ColumnA,"52"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      Clear      Taskname
                      pack       taskname from "=",ColumnB,"8+",ColumnB,"12+",ColumnB,"16+",ColumnB,"20+",ColumnB,"24+",ColumnB,"28+",ColumnB,"32+":
                                 ColumnB,"36+",ColumnB,"40+",ColumnB,"44+",ColumnB,"48+",ColumnB,"52"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
          
.totals
                      MOve       Row12,RowA
                      add        c6,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                      add        c1,rowA
                    Move            RowA,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1


                    setprop sheet.range(ExRange1,ExRange1),*Value=TotORd,*NumberFormat="######0"
.              move          namemask to str11
.              edit          calcnames to str11
                    setprop sheet.range(ExRange2,ExRange2),*Value=Totnames,*NumberFormat="##,####0"
.              Prtpage      laser;*font=prtpg10,*p=columna:7050,*ALIGNMENT=*Left,str6:
.                           *font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,str11
.percentage calcs
              Load          COLUMNC from yearcount of Column1C,Column2C,Column3C,Column4C,Column5C
.let's use a formula here?
                      MOve       Row12,RowA
                      add        c4,RowA

               if     (yearcount = c5)

           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

            Elseif    (yearcount = c4)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

            Elseif    (yearcount = c3)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(K",DimRow1,"-G",DimRow1,")/K",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

            Elseif    (yearcount = c2)
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
                      add c1,rowa
           Move            RowA,DimRow1
                    call            Trim using DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                    pack         taskname from "=(G",DimRow1,"-D",DimRow1,")/G",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
            endif



.YTD
.begin patch 2.10
.previous year(s)
.Year 1
.rent
.
                      move       "C",columnA
                      move       "D",columnB
                      MOve       Row12,RowA
                      add        c8,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
           call       debug      
                      move       c0,n2
                      unpack     today into mm,str6
                      move       mm,n2
                      Clear      Taskname
                      append     "=C",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+C11",taskname
                      elseif     (n5 = c3)
                      append     "+C15",taskname
                      elseif     (n5 = c4)
                      append     "+C19",taskname
                      elseif     (n5 = c5)
                      append     "+C23",taskname
                      elseif     (n5 = c6)
                      append     "+C27",taskname
                      elseif     (n5 = c7)
                      append     "+C31",taskname
                      elseif     (n5 = c8)
                      append     "+C35",taskname
                      elseif     (n5 = c9)
                      append     "+C39",taskname
                      elseif     (n5 = c10)
                      append     "+C43",taskname
                      elseif     (n5 = c11)
                      append     "+C47",taskname
                      elseif     (n5 = "12")
                      append     "+C51",taskname
                      endif
                      repeat
                      reset      taskname                    
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      append     "=D",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+D11",taskname
                      elseif     (n5 = c3)
                      append     "+D15",taskname
                      elseif     (n5 = c4)
                      append     "+D19",taskname
                      elseif     (n5 = c5)
                      append     "+D23",taskname
                      elseif     (n5 = c6)
                      append     "+D27",taskname
                      elseif     (n5 = c7)
                      append     "+D31",taskname
                      elseif     (n5 = c8)
                      append     "+D35",taskname
                      elseif     (n5 = c9)
                      append     "+D39",taskname
                      elseif     (n5 = c10)
                      append     "+D43",taskname
                      elseif     (n5 = c11)
                      append     "+D47",taskname
                      elseif     (n5 = "12")
                      append     "+D51",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
.Exchange
                      MOve       Row12,RowA
                      add        c9,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=C8",taskname
                      elseif     (n5 = c2)
                      append     "+C12",taskname
                      elseif     (n5 = c3)
                      append     "+C16",taskname
                      elseif     (n5 = c4)
                      append     "+C20",taskname
                      elseif     (n5 = c5)
                      append     "+C24",taskname
                      elseif     (n5 = c6)
                      append     "+C28",taskname
                      elseif     (n5 = c7)
                      append     "+C32",taskname
                      elseif     (n5 = c8)
                      append     "+C36",taskname
                      elseif     (n5 = c9)
                      append     "+C40",taskname
                      elseif     (n5 = c10)
                      append     "+C44",taskname
                      elseif     (n5 = c11)
                      append     "+C48",taskname
                      elseif     (n5 = "12")
                      append     "+C52",taskname
                      endif
                      repeat
                      reset      taskname
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=D8",taskname
                      elseif     (n5 = c2)
                      append     "+D12",taskname
                      elseif     (n5 = c3)
                      append     "+D16",taskname
                      elseif     (n5 = c4)
                      append     "+D20",taskname
                      elseif     (n5 = c5)
                      append     "+D24",taskname
                      elseif     (n5 = c6)
                      append     "+D28",taskname
                      elseif     (n5 = c7)
                      append     "+D32",taskname
                      elseif     (n5 = c8)
                      append     "+D36",taskname
                      elseif     (n5 = c9)
                      append     "+D40",taskname
                      elseif     (n5 = c10)
                      append     "+D44",taskname
                      elseif     (n5 = c11)
                      append     "+D48",taskname
                      elseif     (n5 = "12")
                      append     "+D52",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
.Total
                      MOve       Row12,RowA
                      add        c10,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      pack       taskname from "=Sum(c59:c60)"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      clear      taskname
                      pack       taskname from "=Sum(d59:d60)"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
.................................................................................................
.Year 2
.rent
                      move       "F",columnA
                      move       "G",columnB
                      move       "H",columnC
                      MOve       Row12,RowA
                      add        c8,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
           call       debug      
                      move       c0,n2
                      unpack     today into mm,str6
                      move       mm,n2
                      Clear      Taskname
                      append     "=F",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+F11",taskname
                      elseif     (n5 = c3)
                      append     "+F15",taskname
                      elseif     (n5 = c4)
                      append     "+F19",taskname
                      elseif     (n5 = c5)
                      append     "+F23",taskname
                      elseif     (n5 = c6)
                      append     "+F27",taskname
                      elseif     (n5 = c7)
                      append     "+F31",taskname
                      elseif     (n5 = c8)
                      append     "+F35",taskname
                      elseif     (n5 = c9)
                      append     "+F39",taskname
                      elseif     (n5 = c10)
                      append     "+F43",taskname
                      elseif     (n5 = c11)
                      append     "+F47",taskname
                      elseif     (n5 = "12")
                      append     "+F51",taskname
                      endif
                      repeat
                      reset      taskname                    
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"


                      clear      taskname
                      append     "=G",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+G11",taskname
                      elseif     (n5 = c3)
                      append     "+G15",taskname
                      elseif     (n5 = c4)
                      append     "+G19",taskname
                      elseif     (n5 = c5)
                      append     "+G23",taskname
                      elseif     (n5 = c6)
                      append     "+G27",taskname
                      elseif     (n5 = c7)
                      append     "+G31",taskname
                      elseif     (n5 = c8)
                      append     "+G35",taskname
                      elseif     (n5 = c9)
                      append     "+G39",taskname
                      elseif     (n5 = c10)
                      append     "+G43",taskname
                      elseif     (n5 = c11)
                      append     "+G47",taskname
                      elseif     (n5 = "12")
                      append     "+G51",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"59"
                      pack         taskname from "=(g",DimRow1,"-d",DimRow1,")/g",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

.Exchange
                      MOve       Row12,RowA
                      add        c9,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=F8",taskname
                      elseif     (n5 = c2)
                      append     "+F12",taskname
                      elseif     (n5 = c3)
                      append     "+F16",taskname
                      elseif     (n5 = c4)
                      append     "+F20",taskname
                      elseif     (n5 = c5)
                      append     "+F24",taskname
                      elseif     (n5 = c6)
                      append     "+F28",taskname
                      elseif     (n5 = c7)
                      append     "+F32",taskname
                      elseif     (n5 = c8)
                      append     "+F36",taskname
                      elseif     (n5 = c9)
                      append     "+F40",taskname
                      elseif     (n5 = c10)
                      append     "+F44",taskname
                      elseif     (n5 = c11)
                      append     "+F48",taskname
                      elseif     (n5 = "12")
                      append     "+F52",taskname
                      endif
                      repeat
                      reset      taskname
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=g8",taskname
                      elseif     (n5 = c2)
                      append     "+G12",taskname
                      elseif     (n5 = c3)
                      append     "+G16",taskname
                      elseif     (n5 = c4)
                      append     "+G20",taskname
                      elseif     (n5 = c5)
                      append     "+G24",taskname
                      elseif     (n5 = c6)
                      append     "+G28",taskname
                      elseif     (n5 = c7)
                      append     "+G32",taskname
                      elseif     (n5 = c8)
                      append     "+G36",taskname
                      elseif     (n5 = c9)
                      append     "+G40",taskname
                      elseif     (n5 = c10)
                      append     "+G44",taskname
                      elseif     (n5 = c11)
                      append     "+G48",taskname
                      elseif     (n5 = "12")
                      append     "+G52",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"60"
                      pack         taskname from "=(g",DimRow1,"-d",DimRow1,")/g",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"

.Total
                      MOve       Row12,RowA
                      add        c10,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      pack       taskname from "=Sum(f59:f60)"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      clear      taskname
                      pack       taskname from "=Sum(g59:g60)"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"61"
                      pack         taskname from "=(g",DimRow1,"-d",DimRow1,")/g",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.................................................................................................
.Year 3
.rent
                      move       "J",columnA
                      move       "K",columnB
                      move       "L",columnC
                      MOve       Row12,RowA
                      add        c8,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
           call       debug      
                      move       c0,n2
                      unpack     today into mm,str6
                      move       mm,n2
                      Clear      Taskname
                      append     "=J",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+J11",taskname
                      elseif     (n5 = c3)
                      append     "+J15",taskname
                      elseif     (n5 = c4)
                      append     "+J19",taskname
                      elseif     (n5 = c5)
                      append     "+J23",taskname
                      elseif     (n5 = c6)
                      append     "+J27",taskname
                      elseif     (n5 = c7)
                      append     "+J31",taskname
                      elseif     (n5 = c8)
                      append     "+J35",taskname
                      elseif     (n5 = c9)
                      append     "+J39",taskname
                      elseif     (n5 = c10)
                      append     "+J43",taskname
                      elseif     (n5 = c11)
                      append     "+J47",taskname
                      elseif     (n5 = "12")
                      append     "+J51",taskname
                      endif
                      repeat
                      reset      taskname                    
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      append     "=K",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+K11",taskname
                      elseif     (n5 = c3)
                      append     "+K15",taskname
                      elseif     (n5 = c4)
                      append     "+K19",taskname
                      elseif     (n5 = c5)
                      append     "+K23",taskname
                      elseif     (n5 = c6)
                      append     "+K27",taskname
                      elseif     (n5 = c7)
                      append     "+K31",taskname
                      elseif     (n5 = c8)
                      append     "+K35",taskname
                      elseif     (n5 = c9)
                      append     "+K39",taskname
                      elseif     (n5 = c10)
                      append     "+K43",taskname
                      elseif     (n5 = c11)
                      append     "+K47",taskname
                      elseif     (n5 = "12")
                      append     "+K51",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"59"
                      pack         taskname from "=(k",DimRow1,"-g",DimRow1,")/k",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Exchange
                      MOve       Row12,RowA
                      add        c9,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=J8",taskname
                      elseif     (n5 = c2)
                      append     "+J12",taskname
                      elseif     (n5 = c3)
                      append     "+J16",taskname
                      elseif     (n5 = c4)
                      append     "+J20",taskname
                      elseif     (n5 = c5)
                      append     "+J24",taskname
                      elseif     (n5 = c6)
                      append     "+J28",taskname
                      elseif     (n5 = c7)
                      append     "+J32",taskname
                      elseif     (n5 = c8)
                      append     "+J36",taskname
                      elseif     (n5 = c9)
                      append     "+J40",taskname
                      elseif     (n5 = c10)
                      append     "+J44",taskname
                      elseif     (n5 = c11)
                      append     "+J48",taskname
                      elseif     (n5 = "12")
                      append     "+J52",taskname
                      endif
                      repeat
                      reset      taskname
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=K8",taskname
                      elseif     (n5 = c2)
                      append     "+K12",taskname
                      elseif     (n5 = c3)
                      append     "+K16",taskname
                      elseif     (n5 = c4)
                      append     "+K20",taskname
                      elseif     (n5 = c5)
                      append     "+K24",taskname
                      elseif     (n5 = c6)
                      append     "+K28",taskname
                      elseif     (n5 = c7)
                      append     "+K32",taskname
                      elseif     (n5 = c8)
                      append     "+K36",taskname
                      elseif     (n5 = c9)
                      append     "+K40",taskname
                      elseif     (n5 = c10)
                      append     "+K44",taskname
                      elseif     (n5 = c11)
                      append     "+K48",taskname
                      elseif     (n5 = "12")
                      append     "+K52",taskname
                      endif
                      repeat
                      reset      taskname
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"60"
                      pack         taskname from "=(k",DimRow1,"-g",DimRow1,")/k",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Total
                      MOve       Row12,RowA
                      add        c10,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      pack       taskname from "=Sum(j59:j60)"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      clear      taskname
                      pack       taskname from "=Sum(k59:k60)"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"61"
                      pack         taskname from "=(k",DimRow1,"-g",DimRow1,")/k",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.................................................................................................
.Year 4 
.rent
.
                      move       "N",columnA
                      move       "O",columnB
                      move       "P",columnC
                      MOve       Row12,RowA
                      add        c8,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
           call       debug      
                      move       c0,n2
                      unpack     today into mm,str6
                      move       mm,n2
.                      move       today,str8
.                      rep        "-/",str8
                      Clear      Taskname
                      append     "=N",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+N11",taskname
                      elseif     (n5 = c3)
                      append     "+N15",taskname
                      elseif     (n5 = c4)
                      append     "+N19",taskname
                      elseif     (n5 = c5)
                      append     "+N23",taskname
                      elseif     (n5 = c6)
                      append     "+N27",taskname
                      elseif     (n5 = c7)
                      append     "+N31",taskname
                      elseif     (n5 = c8)
                      append     "+N35",taskname
                      elseif     (n5 = c9)
                      append     "+N39",taskname
                      elseif     (n5 = c10)
                      append     "+N43",taskname
                      elseif     (n5 = c11)
                      append     "+N47",taskname
                      elseif     (n5 = "12")
                      append     "+N51",taskname
                      endif
                      repeat
                      reset      taskname
                      
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"

                      clear      taskname
                      append     "=o",taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "7",taskname
                      elseif     (n5 = c2)
                      append     "+o11",taskname
                      elseif     (n5 = c3)
                      append     "+o15",taskname
                      elseif     (n5 = c4)
                      append     "+o19",taskname
                      elseif     (n5 = c5)
                      append     "+o23",taskname
                      elseif     (n5 = c6)
                      append     "+o27",taskname
                      elseif     (n5 = c7)
                      append     "+o31",taskname
                      elseif     (n5 = c8)
                      append     "+o35",taskname
                      elseif     (n5 = c9)
                      append     "+o39",taskname
                      elseif     (n5 = c10)
                      append     "+o43",taskname
                      elseif     (n5 = c11)
                      append     "+o47",taskname
                      elseif     (n5 = "12")
                      append     "+o51",taskname
                      endif
                      repeat
                      reset      taskname

                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"59"
                      pack         taskname from "=(o",DimRow1,"-k",DimRow1,")/o",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Exchange
                      MOve       Row12,RowA
                      add        c9,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=N8",taskname
                      elseif     (n5 = c2)
                      append     "+N12",taskname
                      elseif     (n5 = c3)
                      append     "+N16",taskname
                      elseif     (n5 = c4)
                      append     "+N20",taskname
                      elseif     (n5 = c5)
                      append     "+N24",taskname
                      elseif     (n5 = c6)
                      append     "+N28",taskname
                      elseif     (n5 = c7)
                      append     "+N32",taskname
                      elseif     (n5 = c8)
                      append     "+N36",taskname
                      elseif     (n5 = c9)
                      append     "+N40",taskname
                      elseif     (n5 = c10)
                      append     "+N44",taskname
                      elseif     (n5 = c11)
                      append     "+N48",taskname
                      elseif     (n5 = "12")
                      append     "+N52",taskname
                      endif
                      repeat
                      reset      taskname

.                      pack       taskname from "=(N56/365)*DAYS(#"",str8,"#",#"1/1/14#")"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      clear      taskname
                      for        N5 from c1 to n2 using c1
                      if         (N5 = c1)
                      append     "=O8",taskname
                      elseif     (n5 = c2)
                      append     "+o12",taskname
                      elseif     (n5 = c3)
                      append     "+o16",taskname
                      elseif     (n5 = c4)
                      append     "+o20",taskname
                      elseif     (n5 = c5)
                      append     "+o24",taskname
                      elseif     (n5 = c6)
                      append     "+o28",taskname
                      elseif     (n5 = c7)
                      append     "+o32",taskname
                      elseif     (n5 = c8)
                      append     "+o36",taskname
                      elseif     (n5 = c9)
                      append     "+o40",taskname
                      elseif     (n5 = c10)
                      append     "+o44",taskname
                      elseif     (n5 = c11)
                      append     "+o48",taskname
                      elseif     (n5 = "12")
                      append     "+o52",taskname
                      endif
                      repeat
                      reset      taskname

.                      pack       taskname from "=(O56/365)*DAYS(#"",str8,"#",#"1/1/14#")"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"60"
                      pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Total
                      MOve       Row12,RowA
                      add        c10,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                      clear      taskname
                      pack       taskname from "=Sum(N59:N60)"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      clear      taskname
                      pack       taskname from "=Sum(O59:O60)"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                      pack       ExRange3 from Columnc,"61"
                      pack         taskname from "=(O",DimRow1,"-K",DimRow1,")/O",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.end patch 2.10
.YTD
.Current year
.rent
           call       debug      

                      move       "R",columnA
                      move       "S",columnB
                      move       "T",columnC
                      MOve       Row12,RowA
                      add        c8,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                      clear      taskname
                      pack       taskname from "=R55"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      pack       taskname from "=S55"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Exchange
                      MOve       Row12,RowA
                      add        c9,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                      clear      taskname
                      pack       taskname from "=R56"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      pack       taskname from "=S56"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"
.Total
                      MOve       Row12,RowA
                      add        c10,RowA
           Move            RowA,DimRow1
                    call            Trim using DimRow1
.                   
                    pack            Exrange1 from ColumnA,DimRow1
                    pack            Exrange2 from ColumnB,DimRow1
                    pack            Exrange3 from Columnc,DimRow1 
                      clear      taskname
                      pack       taskname from "=R57"
                      setprop sheet.range(ExRange1,ExRange1),*Formula=taskname,*NumberFormat="######0"
                      pack       taskname from "=S57"
                    setprop sheet.range(ExRange2,ExRange2),*Formula=taskname,*NumberFormat="##,####0"
                    pack         taskname from "=(S",DimRow1,"-O",DimRow1,")/S",DimRow1
                      setprop sheet.range(ExRange3,ExRange3),*Formula=taskname,*NumberFormat="0%;[Red](0%)"




.end patch 2.0
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
.begin patch 2.0
.add code here
.               move     Columnb to ColumnC
.               add      "100" to columnC
.               PrtPage  Laser;*p=Columnc:7050,*alignment=*left,pct1
.end patch 2.0
                      If      (YearCount = 6)
                        move          ordmask to str6
                        edit          FinOrdBR to str6
                        move          namemask to str11
                        edit          FinQtyBR to str11
.begin patch 2.0
.                        Prtpage      laser;*font=prtpg10,*p=Column4a:7300,*ALIGNMENT=*Left,"Brokerage Rental -":
.                                           *p=columna:7300,*ALIGNMENT=*Left,str6:
.                                     *font=prtpg10,*p=columnb:7300,*ALIGNMENT=*Right,str11
.end patch 2.0
                        move          ordmask to str6
                        edit          FinOrdBe to str6
                        move          namemask to str11
                        edit          FinQtyBe to str11
.begin patch 2.0
.                        Prtpage      laser;*font=prtpg10,*p=Column4a:7450,*ALIGNMENT=*Left,"Brokerage Exchange -":
.                                           *p=columna:7450,*ALIGNMENT=*Left,str6:
.                                     *font=prtpg10,*p=columnb:7450,*ALIGNMENT=*Right,str11
.end patch 2.0
                        move          ordmask to str6
                        edit          FinOrdLM to str6
                        move          namemask to str11
                        edit          FinQtyLM to str11
.begin patch 2.0
.                        Prtpage      laser;*font=prtpg10,*p=Column4a:7600,*ALIGNMENT=*Left,"List Management -":
.                                           *p=columna:7600,*ALIGNMENT=*Left,str6:
.                                     *font=prtpg10,*p=columnb:7600,*ALIGNMENT=*Right,str11
.end patch 2.0
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
.begin patch 2.0
TrapSaveAsObject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
.
.
        scan    "Cannot access",str55
        if equal
..Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
        endif
..Send them back to select another File name and try to Save again.
        goto SaveAsFileNameSelect
.end patch 2.0

SendDaveEmail
          Move      "Nord0015 - Alert",MailSubjct
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
.           INCLUDE          PRTPAGEIO.INC
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


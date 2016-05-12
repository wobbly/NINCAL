..............................................................................
PC         EQU       0
           INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
           include  oslspern.inc
           include norddd.inc
           include    compdd.inc
           include         cntdd.inc
...............................................................................
Release  init      "1.00"              DLH  New
reldate   Init      "2014 December 5"
.
.create data file order history by MailDate and dump into excel
* *****************************************************************************
OSLS      dim       25


input      file                       .temp sort in the future

Salescount          form      2
results       IFile         Keylen=10
resultvars    list
ResultKey     dim           10               1-10   Comp#+ccyy          Mail date
ResSls        dim           2               11-12   salesperson      
count01       form          5               13-17   Jan # orders
Vol01         form          9               18-26   Jan Vol Names
count02       form          5               27-31   # orders
Vol02         form          9               32-40   Vol Names
count03       form          5               41-45   # orders
Vol03         form          9               46-54   Vol Names
count04       form          5               55-59   # orders
Vol04         form          9               60-68   Vol Names
count05       form          5               69-74   # orders
Vol05         form          9               75-83   Vol Names
count06       form          5               84-88   # orders
Vol06         form          9               89-97   Vol Names
count07       form          5               98-102   # orders
Vol07         form          9              103-111   Vol Names
count08       form          5              112-116   # orders
Vol08         form          9              117-125  Vol Names
count09       form          5              126-130  # orders
Vol09         form          9              131-139  Vol Names
count10       form          5              140-144   # orders
Vol10         form          9              145-153   Vol Names
count11       form          5              154-158   # orders
Vol11         form          9              159-167   Vol Names
count12       form          5              168-172   # orders
Vol12         form          9              173-181   Vol Names
              listend
.
.
calcord       form          5
calcNames     form          9
TOTord       form          5
TOTNames     form          9
TOTORdA       FOrm    5(12)
TOTNamesA     form          9(12)
.
ColumnA    dim        2
ColumnB    dim        2
ColumnC    dim        2
Formula    Dim        25
.
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"

PROGS    PROGRESS      (2)                  .progress bars
COLORS   COLOR         (3)
 
 
..............................................
.Array

.
StartMonth Dim        2(12),("02"),("03"),("04"),("05"),("06"),("07"),("08"),("09"),("10"),("11"),("12"),("01")
calcrow               Dim        2(12),("10"),("11"),("12"),("13"),("14"),("15"),("16"),("17"),("18"),("19"),("20"),("21")
HoldStartMM           Form       2
HoldStartYr           form       2
Curryy                form       2
currmm                form       2
.
slsfound              form       2
MonthCOunt Form       2
holdToday       dim         6
......................................................................................................
.some excel goodies
.to find version of excel
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end to find version of excel

sheetno    form      2
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
N34     form    3.4

VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
XLAlignLeft                               integer 4,"0xffffefdd"
XlAlignRight                              integer 4,"0xffffefc8"
xlAlignCenter integer 4,"0xffffeff4"
xlBottom  integer      4,"0xffffeff5"
XlLineStyleDBl Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2
xlUnderlineStyleSingle integer 4,"0x2"

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidth2 variant
xlColumnWidth13 variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant

ExRange1      Dim             6
ExRange2      Dim             6
ExRange3      Dim            14
Row1          form            3
Row2          Form            3
Row3          form            3
Row4          Form            3
DimRow1       Dim             3

CurRec        form    5.2
CurVal        form            3
LastVal       form            3

.
         MOVE      "EXIT" TO PF5
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ORDER History Report" TO STITLE
           if         (program <> "NORD0029")
         MOVE      "NORD0029" TO PROGRAM
          clock     date to today
           endif
         move      c1 to v
         TRAP      Stop IF F5
         CALL      PAINT
         CALL      FUNCDISP
.....INITIAL TIME ANS SCREEN DISPLAY;
.
  
           CREATE COLORS(1)=*BLUE
           CREATE COLORS(2)=*RED
           CREATE COLORS(3)=*WHITE
           CREATE PROGS(1)=5:6:10:40:
           BGCOLOR=COLORS(3):
           FGCOLOR=COLORS(1)
           CREATE PROGS(2)=10:11:10:40:
           BGCOLOR=COLORS(3):
           FGCOLOR=COLORS(2)
           ACTIVATE PROGS
  

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
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
        setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
        create         xlColumnWidth,VarType=VT_R8,VarValue="0.0"                     .Default
        create    xlColumnWidth2,VarType=VT_R8,VarValue="2"  
        create    xlColumnWidth13,VarType=VT_R8,VarValue="13"  
          setprop ex,*Visible=OTRUE
              setprop sheet.range("B5:B5").Font,*Bold="True"
          setprop sheet.range("B5:B5"),*Value="Salesperson from Program 34"
              setprop sheet.range("A9:AA10").Font,*Bold="True"
          setprop sheet.range("A9:A9"),*Value="Mailer"
          setprop sheet.range("B9:B9"),*Value="Salesperson"
          setprop sheet.range("c9:c9"),*Value="January",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("C9:D9").Merge       
          setprop sheet.range("c10:c10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("d10:d10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("E9:E9"),*Value="February",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("E9:F9").Merge       
          setprop sheet.range("e10:e10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("f10:f10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("G9:g9"),*Value="March",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("g9:h9").Merge       
          setprop sheet.range("g10:g10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("h10:h10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("i9:i9"),*Value="April",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("i9:j9").Merge       
          setprop sheet.range("i10:i10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("j10:j10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("k9:k9"),*Value="May",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("k9:l9").Merge       
          setprop sheet.range("k10:k10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("l10:l10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("m9:m9"),*Value="June",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("m9:n9").Merge       
          setprop sheet.range("m10:m10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("n10:n10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("o9:o9"),*Value="July",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("o9:p9").Merge       
          setprop sheet.range("o10:o10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("p10:p10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("q9:q9"),*Value="August",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("q9:r9").Merge       
          setprop sheet.range("q10:q10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("r10:r10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("s9:s9"),*Value="September",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("s9:t9").Merge       
          setprop sheet.range("s10:s10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("t10:t10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("u9:u9"),*Value="October",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("u9:v9").Merge       
          setprop sheet.range("u10:u10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("v10:v10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("w9:w9"),*Value="November",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("w9:x9").Merge       
          setprop sheet.range("w10:w10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("x10:x10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("y9:y9"),*Value="December",*HorizontalAlignment=xlAlignCenter
           Sheet.Range("y9:z9").Merge       
          setprop sheet.range("y10:y10"),*Value="Orders",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("z10:z10"),*Value="Names",*HorizontalAlignment=xlAlignCenter
.begin patch xxx
          setprop sheet.range("aa9:aa9"),*Value="Mailer ##",*HorizontalAlignment=xlAlignCenter
.end patch xxx

      
BEGIN
           open       input,"\\nins1\e\data\diskin34.dat|NINS1:502",READ                       ."
           prepare    results,"\\nins1\e\data\text\MLRSMDates.dat","\\nins1\e\data\index\MLRSMDates.isi|nins1:502","10","181"
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
.first pass get data
              move            C0,CurRec
              move            C0,CurVal
              move            C0,LastVal
              positeof Input
              fposit          Input,N10
              calc            howmany=(N10/501)             .501 = 499(Order record length) + 2 bytes for CR/LF
              reposit Input,C0                           .Reposition File Pointer to beginning of file

           Loop
           read       input,seq;ordvars
           until      over
              calc            CurRec=(CurRec+1)
              calc            CurVal=((CurRec/howmany)*100)
              if (CurVal <> LastVal)
               setitem        PROGS(1),0,CurVal
               move           CurVal,LastVal
              endif

           if         (Ostat = "B" or OSTAT = "0")
                      packkey    resultkey using "00",omlrnum,omdtec,omdtey
                      read       results,Resultkey;resultvars
                      if         over
                                 packkey    resultkey using "00",omlrnum,omdtec,omdtey           .restore the key
                                 call       loadres
                                 write      results,resultkey;resultvars
                      else
                                 call       loadres
                                 Update     results;resultvars
                      endif
           endif                      
           
           repeat

.second pass dump to excel
           Close      Input
          Move       "10",Row1
           open       input,"\\nins1\e\data\text\MLRSMDates.dat|nins1:502",READ
              move            C0,CurRec
              move            C0,CurVal
              move            C0,LastVal
              positeof Input
              fposit          Input,N10
              calc            howmany=(N10/183)             .183 = 181( record length) + 2 bytes for CR/LF
              reposit Input,C0                           .Reposition File Pointer to beginning of file

           loop
           read     input,seq;resultvars
           until      over
              calc            CurRec=(CurRec+1)
              calc            CurVal=((CurRec/howmany)*100)
              if (CurVal <> LastVal)
               setitem        PROGS(2),0,CurVal
               move           CurVal,LastVal
              endif
           call       populate
           repeat
           
           Move      Row1,DimRow1
           call      Trim using DimRow1                 

           pack    EXrange1,"A8"
           pack    Exrange2,"AA",Dimrow1
           sheet.range(Exrange1,Exrange2).Columns.Autofit         


           goto       stop




          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=str9
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
           if         (n2 = "12")
           move       c1,N2
           else
           add        c1,n2
           endif
           add        c1,row1

           Move       "23",DimRow1
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Totals"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          pack      ExRange1,"A8"
          pack      ExRange2,"D23"
          sheet.range(ExRange1,ExRange2).BorderAround using *LineStyle=1,*Weight=2

           move       "8",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          pack      Exrange2 from "D",DimRow1
          pack      Exrange3,Exrange1,":",Exrange2
          setprop sheet.range(ExRange1,ExRange1),*Value="Totals",*HorizontalAlignment=xlAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          sheet.range(EXRange3).Merge
           move       "9",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Orders"
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Names"

.
....................................
.set width first
           pack    EXrange1,"A8"
           pack    Exrange2,"AA","23"
           sheet.range(Exrange1,Exrange2).Columns.Autofit         

           setprop xlColumnWidth,VarValue="1.86"
           pack    EXrange1,"D8"
           pack    EXrange2,"D23"
           setprop sheet.range(EXrange1,EXrange2).Columns,*ColumnWidth=xlColumnWidth


           Move       "B",ColumnA
           Move       "C",ColumnB
           For        monthcount from c1 to "12" using c1
           move       TOtordA(Monthcount),TotOrd
           move       TOtNamesA(Monthcount),TotNames
           move       calcrow(Monthcount),row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Totord,*NumberFormat="##,####0_)"
          pack      Exrange2 from ColumnB,DimRow1
          setprop sheet.range(ExRange2,ExRange2),*Value=TOTnames,*NumberFormat="##,####0_)"

           Repeat
           move       "23",Dimrow1
          pack      Exrange1 from ColumnA,DimRow1
           pack       Formula from "=sum(b10:b21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"
          pack      Exrange1 from ColumnB,DimRow1
           pack       Formula from "=sum(C10:C21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"
           
           goto       stop
.
.
...............................................................................
.................................................................................
Populate
           unpack     resultkey into str2,str4
           rep        zfill,str4
           packkey   mkey,str4,z3
           call      nmlrkey
           move       CompContact,ressls

           move       ressls,n2
           Load      OSLS from N2 of OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12:
                      OSLS13,OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22:
                      osls23,osls24,osls25,osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35                                      

           add c1,row1


          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=CompComp
          pack      Exrange2 from "B",DimRow1
          setprop sheet.range(ExRange2,ExRange2),*Value=Osls
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count01,*NumberFormat="##,####0_)"
          pack      Exrange1 from "D",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol01,*NumberFormat="##,####0_)"

          pack      Exrange1 from "E",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count02,*NumberFormat="##,####0_)"
          pack      Exrange1 from "F",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol02,*NumberFormat="##,####0_)"

          pack      Exrange1 from "G",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count03,*NumberFormat="##,####0_)"
          pack      Exrange1 from "H",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol03,*NumberFormat="##,####0_)"

          pack      Exrange1 from "I",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count04,*NumberFormat="##,####0_)"
          pack      Exrange1 from "J",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol04,*NumberFormat="##,####0_)"

          pack      Exrange1 from "k",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count05,*NumberFormat="##,####0_)"
          pack      Exrange1 from "l",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol05,*NumberFormat="##,####0_)"

          pack      Exrange1 from "m",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count06,*NumberFormat="##,####0_)"
          pack      Exrange1 from "n",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol06,*NumberFormat="##,####0_)"

          pack      Exrange1 from "o",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count07,*NumberFormat="##,####0_)"
          pack      Exrange1 from "p",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol07,*NumberFormat="##,####0_)"

          pack      Exrange1 from "q",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count08,*NumberFormat="##,####0_)"
          pack      Exrange1 from "r",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol08,*NumberFormat="##,####0_)"

          pack      Exrange1 from "s",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count09,*NumberFormat="##,####0_)"
          pack      Exrange1 from "t",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol09,*NumberFormat="##,####0_)"

          pack      Exrange1 from "u",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count10,*NumberFormat="##,####0_)"
          pack      Exrange1 from "v",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol10,*NumberFormat="##,####0_)"

          pack      Exrange1 from "w",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count11,*NumberFormat="##,####0_)"
          pack      Exrange1 from "x",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol11,*NumberFormat="##,####0_)"

          pack      Exrange1 from "y",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=count12,*NumberFormat="##,####0_)"
          pack      Exrange1 from "z",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Vol12,*NumberFormat="##,####0_)"
.begin patch xxx
          pack      Exrange1 from "aa",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=str4,*NumberFormat="0000"
.end patch xxx
           
............
           Return
....................................................................................
subhead
           if         (slsfound = c1)
           move       "e",ColumnA
           move       "f",ColumnB
           move       "g",ColumnC
           Elseif     (slsfound = c2)
           move       "h",ColumnA
           move       "i",ColumnB
           move       "j",ColumnC
           Elseif     (slsfound = c3)
           move       "k",ColumnA
           move       "l",ColumnB
           move       "m",ColumnC
           Elseif     (slsfound = c4)
           move       "n",ColumnA
           move       "o",ColumnB
           move       "p",ColumnC
           Elseif     (slsfound = c5)
           move       "q",ColumnA
           move       "r",ColumnB
           move       "s",ColumnC
           Elseif     (slsfound = c6)
           move       "t",ColumnA
           move       "u",ColumnB
           move       "v",ColumnC
           Elseif     (slsfound = c7)
           move       "w",ColumnA
           move       "x",ColumnB
           move       "y",ColumnC
           Elseif     (slsfound = c8)
           move       "z",ColumnA
           move       "aa",ColumnB
           move       "ab",ColumnC
           Elseif     (slsfound = c9)
           move       "ac",ColumnA
           move       "ad",ColumnB
           move       "ae",Columnc
           endif
           move       "8",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          pack      Exrange2 from ColumnC,DimRow1
          pack      Exrange3,Exrange1,":",Exrange2
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          sheet.range(EXRange3).Merge
           move       "9",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Orders"
          pack      Exrange1 from ColumnB,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Names"
          pack      Exrange1 from ColumnC,DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="% Names"
.totals
           move       "23",row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from ColumnA,DimRow1
           pack       Formula from "=sum(",ColumnA,"10:",ColumnA,"21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"

          pack      Exrange1 from ColumnB,DimRow1
           pack       Formula from "=sum(",ColumnB,"10:",ColumnB,"21)"
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="##,####0_)"

          pack      Exrange1 from ColumnC,DimRow1
           pack       formula from "=",ColumnB,dimrow1,"/C",dimrow1
          setprop sheet.range(ExRange1,ExRange1),*Value=formula,*NumberFormat="0.0%"

          pack      ExRange1,columnA,"8"
          pack      ExRange2,columnC,"23"
          sheet.range(ExRange1,ExRange2).BorderAround using *LineStyle=1,*Weight=2

           return
.......................................................................................
STOP
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
           Append    "C:\WORK\test",taskname    
           if        (#ver = c1)
           append  ".xlsx",taskname
           else
           append  ".xls",taskname
           endif
           Reset     Taskname                              
           erase          taskname
           book.saveas giving N9 using *Filename=taskname
           trapclr Object
.
           setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
           destroy        OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
           destroy sheet
           destroy sheets
           destroy book
           destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
           setprop ex,*DisplayAlerts=OFALSE
           destroy        OFALSE
           ex.quit
           destroy ex
.Email new XLS to User



          pause     "10"

.
          Move      "Here is your Order History test",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Move      taskname,MailBody
          MOve      taskname,MailAttach
          call      SendMail
          winshow
         pause     "30"

          shutdown
         STOP
         STOP
.drop into correct bucket
LoadRes
           move       omdtem,n2
           move       oqty,n9
           if         (N2 = c1)   .jan
                      add        c1,count01
                      add        n9,Vol01
           elseif     (N2 = c2)                 
                      add        c1,count02
                      add        n9,Vol02
           elseif     (N2 = c3)                 
                      add        c1,count03
                      add        n9,Vol03
           elseif     (N2 = c4)                 
                      add        c1,count04
                      add        n9,Vol04
           elseif     (N2 = c5)                 
                      add        c1,count05
                      add        n9,Vol05
           elseif     (N2 = c6)                 
                      add        c1,count06
                      add        n9,Vol06
           elseif     (N2 = c7)                 
                      add        c1,count07
                      add        n9,Vol07
           elseif     (N2 = c8)                 
                      add        c1,count08
                      add        n9,Vol08
           elseif     (N2 = c9)                 
                      add        c1,count09
                      add        n9,Vol09
           elseif     (N2 = c10)                 
                      add        c1,count10
                      add        n9,Vol10
           elseif     (N2 = 11)                 
                      add        c1,count11
                      add        n9,Vol11
           elseif     (N2 = 12)                 
                      add        c1,count12
                      add        n9,Vol12
           endif                      
           return
......................................................................................................
ERR      DISPLAY   *P1:1,*ES,"RANGE ERROR :",ERROR;
         KEYIN     *P22:1,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      STOP IF EQUAL
         GOTO      ERR
NOfile
         return
.
IO       DISPLAY   *P1:24,*EL,"IO ERROR ",ERROR;
         STOP

BigDate
           load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                      m07,m08,m09,m010,m011,m012
           return

.
           include  nordio.inc
           include  compio.inc
          include   cntio.inc
           INCLUDE  COMLOGIC.inc


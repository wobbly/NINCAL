PC            EQU             0
              INCLUDE         common.inc
              INCLUDE         cons.inc
              INCLUDE         NDATDD.INC
              INCLUDE         NSHPDD.INC
              INCLUDE         NMRGDD.INC
              INCLUDE         CONSACCT.inc
              Include         nacddd.inc
              INCLUDE         NOWNDD.INC
              INCLUDE         NDAT3DD.INC
              include         compdd.inc
              include         cntdd.inc
              INCLUDE         NORDDD.INC
              INCLUDE         NINVDD.INC
              INCLUDE         NINVACDDD.INC
              INCLUDE         NJSTDD.INC
              INCLUDE         NADJDD.INC
              include         nescdd.inc
              include         NUSEDD.INC
              include         winapi.inc
              INclude         LstIdd.inc
              INClude         INCLDD.inc
              include         PRtpagedd.inc
RElease       Init            "Prerelease"
RElDate       INit            "05/14/2006"
...............................................................................
mrgsw    dim       1
shipsw   dim       1
.
TodayIs       Form            5              .Todays date in Julian
DateofJob     Dim             10             .date job run
TMPQTY        FORM            10
TMPVAR        FORM            10             .Temp calc var
RQTY          FORM            10             .temp holder rental qty
EXQTY         FORM            10             .temp holcer Exch qty
OrdTOTCur     FORM            15             .Exch volume total for current year
EXCHTOTCur    FORM            15             .Exch volume total for current year
RENTTOTCur    FORM            15             .rental volume total for current year
OrdTOTPrev    FORM            15             .Exch volume total for previous year
EXCHTOTPrev   FORM            15             .Exch volume total for previous year
RENTTOTPrev   FORM            15             .rental volume total for previous year
FISCMONTH     FORM            2              .holds month starting fiscal year for client - = 01 if calendar
BegFiscCur    FORM            5
EndFiscCur    FORM            5
BegFiscPrev   FORM            5
EndFiscPrev   FORM            5
AutoFlag      Dim             1              .Holds 'Y' if usual auto run else its manual submission
LYEAR         FORM            4
ForceDay      Dim             4
.
LRArrayPtr    form            5              .pointer for array
LRArray       Record          (8000)               .array to hold valid lrs and related invoice Number
TempLR        Dim             6
TempINv       Dim             6
              REcordend

VolPtr        Form            2
VolArrayPrev  FOrm            15(3,12)         . level 1 total monthly vol, level2 rent, level 3 exch
VolArrayCur   FOrm            15(3,12)         . level 1 total monthly vol, level2 rent, level 3 exch
.........
.output vars
OqtyPrev      FOrm            15
RqtyPRev      FOrm            15
EqtyPrev      FOrm            15
OqtyCur       FOrm            15
RqtyCur       FOrm            15
EqtyCur       FOrm            15
Mon1Label     Dim             3
Mon2Label     Dim             3
Mon3Label     Dim             3
Mon4Label     Dim             3
Mon5Label     Dim             3
Mon6Label     Dim             3
Mon7Label     Dim             3
Mon8Label     Dim             3
Mon9Label     Dim             3
Mon10Label     Dim             3
Mon11Label     Dim             3
Mon12Label     Dim             3
Months        Dim            3(12),("JAN"),("FEB"),("MAR"),("APR"),("MAY"),("JUN"),("JUL"),("AUG"),("SEP"),("OCT"),("NOV"),("DEC")
str36         dim             36
VolHdrCur     Dim             16
VolHdrPrev    Dim             16
.............................................................................................
.some excel goodies
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
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
xlAlignCenter integer 4,"0xffffeff4"
xlBottom  integer 4,"0xffffefc0"            ??????????
xlInsideHorizontal Integer 4,"0x0000000B"
xlInsideVertical Integer 4,"0x0000000C"
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
REportHdr     Dim             250
ExRange1      Dim             5
ExRange2      Dim             5
ExRange3      Dim            12
Row1          form            2
Row2          Form            2
DimRow1       Dim             2
DimRow2       Dim             2
.............................................................................
.Main
.Entry to program check for auto or manual if auto read list file and process all
. If Manual use supplied list # pull info from file & process

              clock timestamp,timestamp
              unpack timestamp,str2,yy,mm,dd
              call            cvtjul
              move juldays  to TODAYIS
              pack DATEofJOB with mm,slash,dd,slash,str2,yy
              PACK            FORCEDAY,str2,YY
              Move            c1 to ndatpath
              move            c1 to nordpath
              move            c1 to ninvpath
.test test test test
              Move            No,AutoFlag
.              call            output
.              goto            eoj

              If              (AutoFlag = YES)
                              Loop
                              CAll           ClearLRArray
                              Call           ClearVolArray
                              call           IncLks
                              Until over
                                             If Not over
                                             packkey Ndatfld,INCList
                                             call Ndatkey
                                             if over
.add alert
                                             endif
                                             MOve           Lmonth,Fiscmonth
                                             call SetDates
                                             call ProcessOrder
                                             CAll ProcessINv
                                             call processAdj
                                             Call OutPut
                                             endif
                              Repeat
                              GOto           Eoj
              Else
                              MOve           "012594",STR6                       .temp testing
                              packkey        IncLFLD,str6
                              call           IncLKey
                              if             OVer
.                              Alert
                              Else
                                             packkey Ndatfld,INCList
                                             call Ndatkey
                                             if over
.add alert
                                             endif

                              endif
                              call SetDates
                              call ProcessOrder
                              CAll ProcessINv
                              call ProcessAdj
                              call Output
                              goto Eoj
              endif

.ProcessOrder We have a valid list # with Criteria get orders as defined
ProcessOrder
              MOVE            C1 TO NORDPATH
              Pack            NORDFLD2,"02L",lSTNUM
              call            nordaim
              IF              NOT OVER
                              Call            OrderCriteria
              Else
.              Alert
              Endif
...............................................................................
              Loop
               call           nordkg
               clear          rqty
               clear          exqty
               until           OVER
               IF NOT OVER
                              call            OrderCriteria

               else
.add code :)

               endif
               Repeat
               Return
.
...............................................................................
.Does this order pass muster?
OrderCriteria
              Call            LoadOrderArray                               .Save LR for Invoice processing
                              IF (INCDATEBY = "M")
                                             MOVE           OMDTEM,MM
                                             MOVE           OMDTEY,YY
                                             MOVE           OMDTED,DD
                                             call           cvtjul
                              else
                                             MOVE           OODTEM,MM
                                             MOVE           OODTEY,YY
                                             MOVE           OODTED,DD
                                             call           cvtjul
                              endif
.previous year?
               if ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
                              If (OSTAT = "B" or OSTAT = "0")
                                             clear TMPVAR
                                             if (oexqty > "0")
                                                            move oexqty to n9
                                                            move           oqty to n10
                                                            sub n9 from n10,TMPVAR
                                                            move           TMPVAR to RQTY
                                                            move           n9 to EXQTY
                                                            add            EXQTY to EXCHTOTPRev
                                                            add            EXQTY to ORDTOTprev
                                                            add            RQTY to RENTTOTprev
                                                            add            RQTY to ORDTOTprev
                                             else
                                                            reset excodes
                                                            scan oelcode in excodes
                                                            if equal
                                                                           move           oqty,EXQTY
                                                                           add            EXQTY,EXCHTOTPrev
                                                                           add            EXQTY,ORDTOTPrev
                                                            else
                                                                           move           oqty,RQTY
                                                                           add            RQTY,RENTTOTPRev
                                                                           add            RQTY,ORDTOTPrev
                                                            endif
                                             endif
                                                            call           cvtgreg
                                                            if (mm = "01")
                                                                           reset          mm
                                                            endif

                                                            clear n2
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Volptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,VolPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  VolPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,Volptr
                             endif
.Total qty
                             MOve            c0,N9
                             Move            Oqty,N9
                             add             N9,VolArrayPrev(1,Volptr)
.Rent Qty
                             MOve            c0,N9
                             Move            Rqty,N9
                             add             N9,VolArrayPrev(2,Volptr)
.Exch Qty
                             MOve            c0,N9
                             Move            Exqty,N9
                             add             N9,VolArrayPrev(3,Volptr)
                             endif
                             endif
.end of previous year order volume
.Current year?
               if ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
                             IF (OSTAT = "B" or OSTAT = "0")
                                             clear TMPVAR
                                             if (oexqty > "0")
                                                            move oexqty to n9
                                                            move           oqty to n10
                                                            sub n9 from n10,TMPVAR
                                                            move           TMPVAR to RQTY
                                                            move           n9 to EXQTY
                                                            add            EXQTY to EXCHTOTCur
                                                            add            EXQTY to ORDTOTCur
                                                            add            RQTY to RENTTOTCur
                                                            add            RQTY to ORDTOTCur
                                             else
                                                            reset excodes
                                                            scan oelcode in excodes
                                                            if equal
                                                                           move           oqty,EXQTY
                                                                           add            EXQTY,EXCHTOTCur
                                                                           add            EXQTY,ORDTOTCur
                                                            else
                                                                           move           oqty,RQTY
                                                                           add            RQTY,RENTTOTCur
                                                                           add            RQTY,ORDTOTCur
                                                            endif
                                             endif
                                                            call           cvtgreg
                                                            if (mm = "01")
                                                                           reset          mm
                                                            endif

                                                            clear n2
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Volptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,VolPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  VolPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,Volptr
                             endif
.Total qty
                             MOve            c0,N9
                             Move            Oqty,N9
                             add             N9,VolArrayCur(1,Volptr)
.Rent Qty
                             MOve            c0,N9
                             Move            Rqty,N9
                             add             N9,VolArrayCur(2,Volptr)
.Exch Qty
                             MOve            c0,N9
                             Move            Exqty,N9
                             add             N9,VolArrayCur(3,Volptr)
                             endif
                             endif
.end of Current year order volume
              Return
...............................................................................
.ProcessINv
ProcessInv
              Return
...............................................................................
.ProcessINv
ProcessAdj
              Return
...........................
.LoadOrderArray - load lrs for invoice reads.
LoadOrderArray
              Move            c1,ninvpath
              packkey         NinvFld,olrn
              call            Ninvkey
              if              not over
              add             c1,LRArrayPtr
              MOve            Olrn,LRArray(LRArrayPtr).TempLR
              MOve            INVnum,LRArray(LRArrayPtr).TempINV
              endif
              return
...............................................................................
.Clear LR array for next run
ClearLRArray
              move            C0,LRArrayptr
              move            b1,str1
              Loop
              add             c1,LRArrayPtr
              MOve            LRArray(LRArrayPtr).TempLR,Str6
              IF              (str6 = "" or Str6 = "      ")
              move            yes,str1
              else
              MOve            B6,LRArray(LRArrayPtr).TempLR
              MOve            B6,LRArray(LRArrayPtr).TempINV
              endif
              Until           (str1=Yes)
              repeat
              Return
...............................................................................
.Clear Vol arrays for next run
ClearVolArray
              move            C1,Volptr
              Loop
              Move            C0,VolArrayCur(1,Volptr)
              Move            C0,VolArrayCur(2,Volptr)
              Move            C0,VolArrayCur(3,Volptr)
              Move            C0,VolArrayPrev(1,Volptr)
              Move            C0,VolArrayPrev(2,Volptr)
              Move            C0,VolArrayPrev(3,Volptr)
              add             c1 to Volptr
              until           (volPtr=13)
              repeat
              Return
...............................................................................
.Output
Output
.
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
        create xlRowHeight,VarType=VT_R8,VarValue="6.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"

..........
.Headers & Labels
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
.
.add switches to build correct headers
.here is fiscal example

              move BegFiscCur to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscCur to juldays
              call            cvtgreg
              pack ReportHdr with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,") vs. "
              pack VolHdrCur with "FISCAL YEAR ",CC,YY
              move BegFiscPrev to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev to juldays
              call            cvtgreg
              Clear           Taskname
              PAck            taskname with ReportHdr,"FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
              pack VolHdrPrev with "FISCAL YEAR ",CC,YY
              reset taskname
.and report title


              clear reporthdr
              If              (IncRep1 = "M")   .Monthly report
              append          "MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A ",ReportHdr
              Elseif          (IncRep1 = "V")   .Monthly report with Variance
              append          "MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A ",ReportHdr
              Elseif          (IncRep1 = "Q")   .Quarterly report
              append          "QUARTERLY LIST INCOME/VOLUME REPORT - REPORTED ON A ",ReportHdr
              endif
              If              (INCLTYPE = "C")
              Append          "CASH BASIS (BY CHECK DATE)",ReportHdr
              Elseif          (INCLTYPE = "I")
              Append          "ACCRUED BASIS (BY INVOICE DATE)",ReportHdr
              ENDIF
              RESET           REportHdr
.Build Month Labels
.lets offset So that Month 1 of array is first month of fiscal or calendar year
              Clear           Str36
              MOve            FiscMonth,n3
              call            debug
              For             N2 from c0 to "11" using "1"
              IF              (n3 > "12")
                              Move c1 to N3
              endif
              Move            Months(N3),str3
              append          str3,str36
              add             c1 to n3
              Repeat
              reset           str36
              Unpack          str36 into Mon1Label,Mon2Label,Mon3Label,Mon4Label,Mon5Label,Mon6Label:
                              Mon7Label,Mon8Label,Mon9Label,Mon10Label,Mon11Label,Mon12Label
.
.              sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11

              setprop         sheet.range("A33:A33").Rows,*RowHeight=xlRowHeight
              setprop         sheet.range("c1:c1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("f1:f1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("k1:k1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="8.38"
              setprop         sheet.range("a1:b1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="14.75"
              setprop         sheet.range("d1:e1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("g1:h1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="18.15"
              setprop         sheet.range("J1:J1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("L1:L1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="5.13"
              setprop         sheet.range("I1:I1").Columns,*ColumnWidth=xlColumnWidth

              setprop sheet.range("B1","B1"),*Value=Olstname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b1:b1").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b1:b1").Font,*Bold="True"
              sheet.range("B1:N1").Merge
              setprop sheet.range("B2","B2"),*Value=ReportHdr,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b2:b2").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("b2:b2").Font,*Bold="True"
              sheet.range("B2:N2").Merge
              setprop sheet.range("B3","B3"),*Value=taskname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b3:b3").Font,*Name="Times New Roman", *Size=11
              setprop sheet.range("b3:b3").Font,*Bold="True"
              sheet.range("B3:N3").Merge
              setprop sheet.range("d6","d6"),*Value=VolHdrCur,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d6:d6").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("d6:d6").Font,*Bold="True"
              sheet.range("d6:E6").Merge
              setprop sheet.range("G6","G6"),*Value=VolHdrpREV,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G6:G6").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("G6:G6").Font,*Bold="True"
              sheet.range("G6:H6").Merge
              setprop sheet.range("J6","J6"),*Value=VolHdrCur,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J6:J6").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("J6:J6").Font,*Bold="True"
              sheet.range("J6:J6").Merge
              setprop sheet.range("L6","L6"),*Value=VolHdrpREV,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("L6:L6").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("L6:L6").Font,*Bold="True"
              sheet.range("L6:L6").Merge

              setprop sheet.range("B9","B9"),*Value=Mon1Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b9:b9").Font,*Bold="True"
              sheet.range("B9:B10").Merge
              setprop sheet.range("B11","B11"),*Value=Mon2Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b11:b11").Font,*Bold="True"
              sheet.range("B11:B12").Merge
              setprop sheet.range("B13","B13"),*Value=Mon3Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b13:b13").Font,*Bold="True"
              sheet.range("B13:B14").Merge
              setprop sheet.range("B15","B15"),*Value=Mon4Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b15:b15").Font,*Bold="True"
              sheet.range("B15:B16").Merge
              setprop sheet.range("B17","B17"),*Value=Mon5Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b17:b17").Font,*Bold="True"
              sheet.range("B17:B18").Merge
              setprop sheet.range("B19","B19"),*Value=Mon6Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b19:b19").Font,*Bold="True"
              sheet.range("B19:B20").Merge
              setprop sheet.range("B21","B21"),*Value=Mon7Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b21:b21").Font,*Bold="True"
              sheet.range("B21:B22").Merge
              setprop sheet.range("B23","B23"),*Value=Mon8Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b23:b23").Font,*Bold="True"
              sheet.range("B23:B24").Merge
              setprop sheet.range("B25","B25"),*Value=Mon9Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b25:b25").Font,*Bold="True"
              sheet.range("B25:B26").Merge
              setprop sheet.range("B27","B27"),*Value=Mon10Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b27:b27").Font,*Bold="True"
              sheet.range("B27:B28").Merge
              setprop sheet.range("B29","B29"),*Value=Mon11Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b29:b29").Font,*Bold="True"
              sheet.range("B29:B30").Merge
              setprop sheet.range("B31","B31"),*Value=Mon12Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b31:b31").Font,*Bold="True"
              sheet.range("B31:B32").Merge
              setprop sheet.range("B34","B34"),*Value="Total",*HorizontalAlignment=xlLeft
              sheet.range("B34:B35").Merge
              setprop sheet.range("D7","D7"),*Value="Placed Volume",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D7:D7").Font,*Bold="True"
              sheet.range("d7:d8").Merge
              setprop sheet.range("D7:D8"),*WrapText=OTRUE
              setprop sheet.range("E7","E7"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E7:E8").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("E7","E7").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("E7","E7"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E8","E8"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E8","E8"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G7","G7"),*Value="Placed Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G7:G7").Font,*Bold="True"
              sheet.range("G7:G8").Merge
              setprop sheet.range("H7","H7"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H7:H8").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("h7","h7").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("H8","H8"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J7","J7"),*Value="Accrued Income",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J7:J7").Font,*Bold="True"
              sheet.range("J7:J8").Merge
              setprop sheet.range("L7","L7"),*Value="Accrued Income",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("L7:L7").Font,*Bold="True"
              sheet.range("L7:L8").Merge
.borders
               sheet.range("B9:b35").BorderAround using *LineStyle=1,*Weight=3
.               SETPROP sheet.range("B9:b35").Borders,*XlBordersIndex=xlInsideHorizontal
               SETPROP sheet.range("B9:b35").Borders,xlInsideHorizontal
               sheet.range("d7:E35").BorderAround using *LineStyle=1,*Weight=3
.               sheet.range("D7:E35").Borders *XlBordersIndex=xlInsideVertical
               sheet.range("G7:H35").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("j7:j35").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("l7:L35").BorderAround using *LineStyle=1,*Weight=3

............
.data
              move            C1,Volptr
              MOve            "9",Row1
              move            "10",row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Loop
              Move            VolArrayPrev(1,VolPtr),OqtyPrev
              Move            VolArrayPrev(2,VolPtr),RqtyPrev
              Move            VolArrayPrev(3,VolPtr),Eqtyprev
              Move            VolArrayCur(1,VolPtr),OqtyCur
              Move            VolArrayCur(2,VolPtr),Rqtycur
              Move            VolArrayCur(3,VolPtr),EQtyCur
              call            debug
              pack            Exrange1 from "D",DimRow1
              pack            Exrange2 from "D",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=OqtyCur,*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "E",DimRow1
              pack            Exrange2 from "E",DimRow2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyCur,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyCur,*NumberFormat="##,####0"
              pack            Exrange1 from "G",DimRow1
              pack            Exrange2 from "G",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=OqtyPrev,*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "H",DimRow1
              pack            Exrange2 from "H",DimRow2
              setprop sheet.range(Exrange1,Exrange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(Exrange2,Exrange2),*Value=RqtyPrev,*NumberFormat="##,####0"
              add             c2,row1
              add             c2,Row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              add             c1 to Volptr
              until           (volPtr=13)
              repeat
.TOTALS
              add             c1,row1
              add             c1,Row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              pack            Exrange1 from "D",DimRow1
              pack            Exrange2 from "D",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=OrdTotCur,*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "E",DimRow1
              pack            Exrange2 from "E",DimRow2
              setprop sheet.range(ExRange1,ExRange1),*Value=ExchTotCur,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RentTotCur,*NumberFormat="##,####0"
              pack            Exrange1 from "G",DimRow1
              pack            Exrange2 from "G",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=OrdTotPrev,*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "H",DimRow1
              pack            Exrange2 from "H",DimRow2
              setprop sheet.range(Exrange1,Exrange1),*Value=ExchTotPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(Exrange2,Exrange2),*Value=RentTotPrev,*NumberFormat="##,####0"

              PACK            STR25,"* As of ",DATEOFJOB
              setprop sheet.range("B37","B37"),*Value=str25,*HorizontalAlignment=xlLeft
.save it
        clear   taskname

        setprop ex,*DefaultFilePath=taskname
        Pack    Str3,"c:\"      ."argh
        pack    taskname,str3,taskname,"LOINC"
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
        endif

.........................................
.CleanUp
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        ex.quit
        destroy ex
        Return
...............................................................................
.SetDates - establish date parameters
SetDates
               move LMONTH to fiscmonth
               MOVE LMONTH TO MM
               MOVE           "01" TO DD

               Move ForceDay to N4
               move n4 to LYEAR
               unpack LYEAR,str2,YY
               call           cvtjul

.If this is a fiscal year report and todayis[report date] is less than latest fiscal date than do not add add another year to it
.i.e. if the fiscal month is jul 04 and the report date is mar 04 then we must sub a year for jul 03 - jun 04 FY
.i.e. if the fiscal month is jul 04 and the report date is aug 04 then we leave alone to create jul 04 - jun 05 FY

               if (Lmonth <> C1)
                              if (todayis < juldays)
                                             sub c1 from n4
                                             move n4,str4
                                             unpack str4 to str2,YY
                                             rep zfill,YY
                                             call           cvtjul
                              endif
               endif

               move juldays to BegFiscCur
               sub            c1 from juldays
               call           cvtgreg
               CLEAR          N2
               MOVE           yy TO N2
               pack str4 with CC,YY
               move str4 to n4
               add c1,n4
               unpack n4,str2,n2
               MOVE N2 TO YY
               CALL CVTJUL
               move juldays to EndFiscCur
Year2
               move begfiscCur to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays to BegFiscPrev
.End
               move endfiscCur to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays,EndFiscPrev
               return
...............................................................................
.EOJ - End of JOB
EOJ
              Stop
...............................................................................
.INCLUDES
              INCLUDE         NORDIO.INC
              INCLUDE         NDATIO.INC
              INCLUDE         NSHPIO.INC
              INCLUDE         NMRGIO.INC
              INCLUDE         COMPUTE.INC
              INCLUDE         NDAT3IO.INC
              INCLUDE         nacdIO.inc
              INCLUDE         NINVACDIO.INC
              INCLUDE         NINVIO.INC
              INCLUDE         NJSTIO.INC
              INCLUDE         NADJIO.INC
              INCLUDE         NUSEIO.INC
              INclude         LstIIO.inc
              INClude         INCLIO.inc
              INCLUDE         \\nts0\c\library\include\COMLOGIC.INC
              INclude         PrtPageio.inc

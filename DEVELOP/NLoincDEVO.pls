PC            EQU             0
              Include         common.inc
              Include         cons.inc
              Include         NDATDD.INC
              Include         NSHPDD.INC
              Include         NMRGDD.INC
              Include         CONSACCT.inc
              Include         nacddd.inc
              Include         NOWNDD.INC
              Include         NDAT3DD.INC
              Include         compdd.inc
              Include         cntdd.inc
              Include         NORDDD.INC
              Include         NINVDD.INC
              Include         NINVACDDD.INC
              Include         NJSTDD.INC
              Include         NADJDD.INC
              Include         nescdd.inc
              Include         NUSEDD.INC
              Include         winapi.inc
              Include         LstIdd.inc
              Include         LIncDD.inc
              Include         PRtpagedd.inc
;Following used only in order to load Search.plf
               include        ncmpdd.inc
               include        nrtndd.inc
RElease       Init            "Prerelease"
RElDate       INit            "05/14/2006"
...............................................................................
mrgsw    dim       1
shipsw   dim       1
adjn2         form            2              .adjustment for loop counter
ProjFlag      Dim             1              .Y if we had projection record
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
AutoFlag      Init            "Y"              .Holds 'Y' if usual auto run else its manual submission
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

InvPtr        Form            2
InvArrayPrev  FOrm            15.2(1,12)         . level 1 total monthly  AP Prev year
InvArrayCur   FOrm            15.2(1,12)         . level 1 total monthly AP  Current Year
PRojArray     Form            9.2(12)           .projections $
PRojArray1     Form            9.2(12)           .projections $ for sorting fiscal's
.........
.output vars
OqtyPrev      FOrm            15              .Order Quantity Previous year
RqtyPRev      FOrm            15              .Rent Quantity Previous year
EqtyPrev      FOrm            15              .Exchange Quantity Previous year
OqtyCur       FOrm            15              .Order Quantity Current year
RqtyCur       FOrm            15              .Rent Quantity Current year
EqtyCur       FOrm            15              .Exchange Quantity Current year
Mon1Label     Dim             3              .Label Month 1
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
VolHdrCur     Dim             16               .header volume Current year
VolHdrPrev    Dim             16               .header Folume Previous Year
APPrev        Form            15.2             .previous year A/P
APCur         Form            15.2             .Current Year A/P
PRojection    form            15.2             .Projected
APOpen        Form            15.2             .Open Payables
starttime     form            17               .self describing :)
endtime       form            17               .self describing :)
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
xlBottom  integer      4,"0xffffeff5"
XlLineStyleDBl Integer 4,"0xffffefe9"                         .line style double
XlShiftToLeft  Integer 4,"0xffffefc1"                         .delete shift to left
xlLandscape integer 4,"0x2"                     .2

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant

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
;............................................................................................................
              rep             lowup,PROGRAM   .case sensitive
              IF           (PRogram = "NLOINC0007")    .loaded from dsprog - Auto mode
              MOve            Yes,AutoFlag
              Else
              MOve            No,AutoFlag
              endif
;Set Vars used for About Box
        move    "NLOInc0007.PLS",Wprognme
        move    "LO Income Reports",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate

Timer   Timer
SRCH           plform         Search
mss1           plform         Error
abt            plform         About
NLoinca        plform         NLOINC0007A
.NLoincb        plform         credit0001B
x              plform         NLOINC0007
               winhide
.;Load Forms, Always load parent form first
               formload       x
               formload       NLOinca,NLOINC0007
               formload       abt
               formload       mss1
               formload       SRCH
.
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,EOJ,RESULT

              clock timestamp,timestamp
              move            timestamp,starttime
              unpack timestamp,str2,yy,mm,dd
              call            cvtjul
              move juldays  to TODAYIS
              pack DATEofJOB with mm,slash,dd,slash,str2,yy
              PACK            FORCEDAY,str2,YY
              Move            c1 to ndatpath
              move            c1 to nordpath
              move            c1 to ninvpath
              move            c3 to nordlock            .no locks
              move            c3 to Ndatlock            .no locks
              move            c3 to Nshplock            .no locks
              move            c3 to Complock            .no locks
              move            c3 to CNCTLOCK            .no locks
.
. If entered from Master Menu -- in manual /  maint mode
.load screens and wait for user
. if loaded from dsprog - Auto mode  load listview and display progress
.
.test test test test
               Move            Yes,AutoFlag
.               Move            No,AutoFlag
.              call            output
.              goto            eoj
              call            LoadRepList

              If              (AutoFlag = YES)
                              SETPROP       NLOINC0007aListView001,ENABLED=0
                              NLOINC0007aListView001.SetItemState using Seq,C2,c2
                              move     SEQ,result
                              Loop
                              CAll           ClearLRArray
                              Call           ClearVolArray
                              call           ClearProjArray
                              call           ClearInvArray
                              call           ScrubVars
                              call           debug
                              move     result,IN9
                              NLOINC0007aListView001.GetNextItem giving result using C2,IN9  // -1 is error code
                              NLOINC0007aListView001.GetItemText giving str6 using result,0
                              Move str6,LincList
                              NLOINC0007aListView001.SetItemState using Result,C0,c2
.                              packkey        LincFld,str6
.                              call           LInckey
.                              Until over
                              NLOINC0007aListView001.GetItemText Giving LincAuto using result,6
                                             IF             (LincAuto <> "N")
                                             NLOINC0007aListView001.GetItemText Giving str10 using Result,2
                                                             IF             (str10 = "Monthly")
                                                             Move           "M" to Lincrep1
                                                             else
                                                             Move           "Q" to Lincrep1
                                                             endif
.                                                                           
                                             NLOINC0007aListView001.GetItemText Giving str10 using Result,3
                                                             if             (str10 = "Mail date")
                                                             move           "M",Lincdateby
                                                             else
                                                             move           "O",Lincdateby
                                                             endif
                                             NLOINC0007aListView001.GetItemText Giving str7 using Result,4
                                                             if             (str7 = "cash")
                                                             move           "C",LincType
                                                             Else
                                                             move           "I",LincType
                                                             endif
                                             NLOINC0007aListView001.GetItemText Giving str2 using Result,7
                                             move           str2 to Lmonth
                                             Endif
                              until (result = SEQ)
.                                             If Not over
                                             IF  (Result <> Seq)
                                             MOve           Lmonth,Fiscmonth
                              call           debug
                                                            IF (LIncREP1 = "M")                      .only ready for monthly so far 6/1/06
                                                            call           Monthly
                                                            else
                                                            call           Quarterly
                                                            endif

                                             endif

                              Repeat
                              GOto           Eoj
              Else

              loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
              repeat
              goto  Eoj
              endif
ManualRun
.add code to use items selected in list view
.1st check and see if any selected
              move     SEQ,result
              loop
              move     result,IN9
              NLOINC0007aListView001.GetNextItem giving result using C2,IN9  // -1 is error code

              until (result = SEQ)
              NLOINC0007aListView001.GetItemText giving str6 using result,0
              NLOINC0007aListView001.SetItemState using Result,C0,c2

.Process your information here
                              packkey        LIncFLD,str6
                              call           LIncKey
                              if             OVer
.                              Alert
                              Return
                              endif

                              IF (LIncREP1 = "M")                      .only ready for monthly so far 6/1/06
                              call           Monthly
                              else
                              call           Quarterly
                              endif
              repeat
.              NLOINC0007aListView001.SetItemState using In9,C0,c2
              setprop   NLOINC0007Button004,Enabled=1
              return

.                              move           "021334",str6
.                              move           "021231",str6
.                              move           "020565",str6
.                              Move           "020411",str6               .nwf
.                              Move           "003942",str6               .npca
.                              Move           "018246",str6
.                              Move           "017865",str6
.                              Move           "015102",str6
.                              Move           "011507",str6
.                              Move           "010298",str6
.                              Move           "005172",str6
                              MOve           "012594",STR6                       .temp testing  SOI
.                              MOve           "002303",STR6                       .temp testing tnc

Monthly
                              packkey Ndatfld,LIncList
                              call Ndatkey
                              if over
.add alert
                              return
                              endif
.
                              call SetDates
                              call ProcessOrder
                              CAll ProcessINv
                              call ProcessAdj
                              call getPRojections
                              call Output
                              return
Quarterly
                              packkey Ndatfld,LIncList
                              call Ndatkey
                              if over
.add alert
                              return
                              endif
                              return

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
               clear          rqty
               clear          exqty
               call           nordkg
               until           OVER
               IF NOT OVER
                              if             (olrn = "607064")
                              call           debug
                              endif
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
                              IF (LINCDATEBY = "M")
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
              MOve            c1,LRArrayPtr
              Loop
              call            UnloadOrderArray
.
              If              (olrn ="")
                              Break
              Return
              endif
.
              call            InvoiceCriteria
              Repeat

InvoiceCriteria
              Move            C0 to Juldays
              packkey         NinvFld,olrn
              call            Ninvkey
              if              not over
              MOve            c0 to N2
                              if (LIncTYPE = "C")      .CHEck Date - Cash Basis
                                             If             (statb <> "P")        .not paid get out
.add code to accrue open payables
                                             call           GetAdjCash
                                             add            AP1,APOpen
                                                            return
                                             endif
.
                                             move           CHK1DTEM to N2
                                             move           CHK1DTEM to MM
                                             move           CHK1DTED to DD
                                             move           CHK1DTEY to YY
                                             If             (N2 = C0)                     .paid but no checkdate hope we find it with adjustments
                                             return
                                             endif
.
                                             call           cvtjul
                              elseIf (LIncTYPE = "I")      .INvoice Date
                                             move           INVDTEM to N2
                                             move           INVDTEM to MM
                                             move           INVDTED to DD
                                             move           INVDTEY to YY
                                             call           cvtjul
                              endif
              Endif
              if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
.              call            debug
                              if (LIncTYPE = "C")      .CHEck Date - Cash Basis
                              call           GetAdjCash
                              endif
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Invptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,InvPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  InvPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,Invptr
                             endif
.A/P
               add             AP1,InvArrayCur(1,INVptr)
              endif
.
              if (Juldays >= BegFiscPrev & Juldays  <=  EndFiscPrev)
                              if (LIncTYPE = "C")      .CHEck Date - Cash Basis
                              call           GetAdjCash
                              endif
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Invptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,InvPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  InvPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,Invptr
                             endif
.A/P
               add             AP1,InvArrayPrev(1,INVptr)
              endif
.end of Current year

              Return
...............................................................................
.GetAdjCash - cash basis lets get the correct payment amount
GetAdjCash
              packkey         Nadjfld,Olrn
              CALL           NADJKEY
              if              Not over
                              add             ASPAYAD1,ap1
              endif
              return

...not sure I will need compute crap
              MOVE            YES TO SUBPPSW
              Move            no,shipsw
              move            no,mrgsw
              MOVE            OLRN to nmrgfld
              REP             ZFILL IN NMRGFLD
              move            c0 to nmrgrqty
              move            c0 to nmrgiqty
              move            c0 to nmrgnet
              move            no to mrgsw
              move            no to shipsw
              CALL            NMRGKEY
              if              not over
                              move      yes to mrgsw
              endif

              MOVE            NordFLD to nshpfld
              REP             ZFILL IN NshpFLD
              CALL            NshpKEY
              if              not over
                                            move      yes to shipsw
              endif
              call            wipecvars
              move            c1 to ndatpath
              move            olnum to ndatfld
              call            ndatkey
              call            NInvAcdRecClear
              CLEAR           NInvAcdfld
              packkey         NInvAcdFld from Invnum
              call            NinvAcdTst
              Call            NInvAcdRecLoad
              CALL            COMPUTE
              move            ap to ap1
              return

...............................................................................
.ProcessAdj - detail adjustments
ProcessAdj
              MOve            c1,LRArrayPtr
              Loop
              call            UnloadOrderArray
.
              If              (olrn ="")
                              Break
              Return
              endif
.
              If              (olrn ="607064")
              call            debug
              endif
              call            adjustRead
              Repeat

AdjustRead
              move             "01" to n2
              move             n2 to str2
              rep              zfill in str2
              CLEAR            NJSTFLD
              PACKkey          NJSTFLD FROM INVNUM,str2
              CALL             NJSTKEY
              if               Not Over
              call            adjustCriteria
              endif
              clear           adjn2
              for             adjn2,"2","9"
              move            adjn2 to str2
              rep             zfill in str2
              CLEAR           NJSTFLD
              PACKKey         NJSTFLD FROM INVNUM,str2
              rep             zfill in njstfld
              CALL            NJSTKEY
              if               Not Over
              call            adjustCriteria
              endif
              repeat
              Return
AdjustCriteria
              if              (LIncTYPE = "C")             .cash basis only care about code 14
              If              (olrn ="607064")
              call            debug
              endif
                              IF             (JSTREASN = "14")
                              unpack          JSTDATE,str2,YY,MM,DD
                                             if              (jstap1 < 0)
                                             mult            seq by jstap1
                                             endif
                              move           jstap1 to ap1
                              else
                              return
                              endif
              Elseif         (LIncTYPE = "I")             .Invoice date basis
                              unpack          JSTDATE,str2,YY,MM,DD
                              IF             (JSTREASN = "14")
                              return
                              else
                              move           jstap1 to ap1
                              endif
              endif
              call            cvtjul
              if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Invptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,InvPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  InvPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,iNVptr
                             endif
.A/P
               add             AP1,InvArrayCur(1,Invptr)
              endif
.
              if (Juldays >= BegFiscPrev & Juldays  <=  EndFiscPrev)
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
.lets offset So that Month 1 of array is first month of fiscal or calendar year
                                            MOVe            MM,N2          .month to adjust
                                            if              (N2 < Fiscmonth)
                                                            CAlc Invptr=N2-fiscmonth+13
                                            ElseIf          (N2 = Fiscmonth)
                                                            Move c1,InvPtr
                                            ElseIf          (N2 > Fiscmonth)
                                                            Calc  InvPtr=N2-Fiscmonth+1
                                            endif
                             Else            .Not Fiscal
                             Move            MM,iNVptr
                             endif
.A/P
               add             AP1,InvArrayPrev(1,Invptr)
              endif
.end of Current year
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
...........................
.UnLoadOrderArray - unload lrs for invoice reads.
UnLoadOrderArray
              MOve            LRArray(LRArrayPtr).TempLR,Olrn
              MOve            LRArray(LRArrayPtr).TempINV,Invnum
              add             c1,LRArrayPtr
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
              move            C0,LRArrayptr
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
              move            C0,volptr
              Return
...............................................................................
.Clear Inv arrays for next run
ClearInvArray
              move            C1,InvPtr
              Loop
              Move            C0,InvArrayCur(1,InvPtr)
              Move            C0,InvArrayPrev(1,InvPtr)
              add             c1 to InvPtr
              until           (InvPtr=13)
              repeat
              move            C0,InvPtr
              Return
...............................................................................
.Clear Projections arrays for next run
ClearProjArray
              move            C1,InvPtr
              Loop
              Move            C0,ProjArray(InvPtr)
              Move            C0,ProjArray1(InvPtr)
              add             c1 to InvPtr
              until           (InvPtr=13)
              repeat
              move            C0,InvPtr
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
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
.        setprop ex,*Visible="True"

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
              If              (LIncRep1 = "M")   .Monthly report
              append          "MONTHLY LIST REPORT - VOLUME ",ReportHdr
                              if (LINCDATEBY = "O")
                                             append "(BY ORDER DATE)/",ReportHdr
                              Elseif (LINCDATEBY = "M")
                                             append "(BY MAIL DATE)/",ReportHdr
                              endif
              Elseif          (LIncRep2 = "Y")   .Monthly report with Variance
              append          "MONTHLY LIST REPORT - VOLUME ",ReportHdr
                              if (LINCDATEBY = "O")
                                             append "(BY ORDER DATE)/",ReportHdr
                              Elseif (LINCDATEBY = "M")
                                             append "(BY MAIL DATE)/",ReportHdr
                              endif
              Elseif          (LIncRep1 = "Q")   .Quarterly report
              append          "QUARTERLY LIST REPORT - VOLUME ",ReportHdr
                              if (LINCDATEBY = "O")
                                             append "(BY ORDER DATE)/",ReportHdr
                              Elseif (LINCDATEBY = "M")
                                             append "(BY MAIL DATE)/",ReportHdr
                              endif
              endif
              If              (LIncTYPE = "C")
              Append          "INCOME REPORTED ON A CASH BASIS (BY CHECK DATE)",ReportHdr
              Elseif          (LIncTYPE = "I")
              Append          "INCOME REPORTED ON AN ACCRUED BASIS (BY INVOICE DATE)",ReportHdr
              ENDIF
              RESET           REportHdr
.Build Month Labels
.lets offset So that Month 1 of array is first month of fiscal or calendar year
              Clear           Str36
              MOve            FiscMonth,n3
.              call            debug
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
.              sheet.range("A1:E1").Merge
              sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45

              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin

              setprop         sheet.range("A35:A35").Rows,*RowHeight=xlRowHeight
              setprop         sheet.range("a1:a1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("c1:c1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("f1:f1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("k1:k1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="12.00"
              setprop         sheet.range("d1:d1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("g1:g1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="14.75"
              setprop         sheet.range("e1:e1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("h1:h1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("J1:O1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="5.13"
              setprop         sheet.range("I1:I1").Columns,*ColumnWidth=xlColumnWidth

              setprop sheet.range("b3","B3"),*Value=Olstname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b3:b3").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b3:b3").Font,*Bold="True"
              setprop sheet.range("B4","B4"),*Value=ReportHdr,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B4:B4").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("B4:B4").Font,*Bold="True"
              setprop sheet.range("B5","B5"),*Value=taskname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B5:B5").Font,*Name="Times New Roman", *Size=11
              setprop sheet.range("B5:B5").Font,*Bold="True"
              setprop sheet.range("D8","D8"),*Value=VolHdrCur,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D8:D8").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("D8:D8").Font,*Bold="True"
              sheet.range("d8:E8").Merge
              setprop sheet.range("G8","G8"),*Value=VolHdrpREV,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G8:G8").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("G8:G8").Font,*Bold="True"
              sheet.range("G8:H8").Merge

              setprop sheet.range("B11","B11"),*Value=Mon1Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b11:b11").Font,*Bold="True"
              sheet.range("B11:B12").Merge
              setprop sheet.range("B13","B13"),*Value=Mon2Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b13:b13").Font,*Bold="True"
              sheet.range("B13:B14").Merge
              setprop sheet.range("B15","B15"),*Value=Mon3Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b15:b15").Font,*Bold="True"
              sheet.range("B15:B16").Merge
              setprop sheet.range("B17","B17"),*Value=Mon4Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b17:b17").Font,*Bold="True"
              sheet.range("B17:B18").Merge
              setprop sheet.range("B19","B19"),*Value=Mon5Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b19:b19").Font,*Bold="True"
              sheet.range("B19:B20").Merge
              setprop sheet.range("B21","B21"),*Value=Mon6Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b21:b21").Font,*Bold="True"
              sheet.range("B21:B22").Merge
              setprop sheet.range("B23","B23"),*Value=Mon7Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b23:b23").Font,*Bold="True"
              sheet.range("B23:B24").Merge
              setprop sheet.range("B25","B25"),*Value=Mon8Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b25:b25").Font,*Bold="True"
              sheet.range("B25:B26").Merge
              setprop sheet.range("B27","B27"),*Value=Mon9Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b27:b27").Font,*Bold="True"
              sheet.range("B27:B28").Merge
              setprop sheet.range("B29","B29"),*Value=Mon10Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b29:b29").Font,*Bold="True"
              sheet.range("B29:B30").Merge
              setprop sheet.range("B31","B31"),*Value=Mon11Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b31:b31").Font,*Bold="True"
              sheet.range("B31:B32").Merge
              setprop sheet.range("B33","B33"),*Value=Mon12Label,*HorizontalAlignment=xlLeft
              setprop sheet.range("b33:b33").Font,*Bold="True"
              sheet.range("B33:B34").Merge
              setprop sheet.range("B36","B36"),*Value="Totals",*HorizontalAlignment=xlLeft
              sheet.range("B36:B37").Merge
              setprop sheet.range("D9","D9"),*Value="Volume",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D9:D9").Font,*Bold="True"
              sheet.range("d9:d10").Merge
              setprop sheet.range("D9:D10"),*WrapText=OTRUE
              setprop sheet.range("E9","E9"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E10:E10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("E9","E9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("E9","E9"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E10","E10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E10","E10"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G9","G9"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G9:G9").Font,*Bold="True"
              sheet.range("G10:G10").Merge
              setprop sheet.range("H9","H9"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H9:H10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("h9","h9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("H10","H10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
.income starts here
              Bump     VolHdrPrev,7                           .let reuse and chop off "FISCAL"
              setprop sheet.range("m8","m8"),*Value=VolHdrPrev,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("m8:m8").Font,*Bold="True"
              Reset           VolHdrPrev
              setprop sheet.range("J9","J9"),*Value="Projected",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J10","J10"),*Value="Income",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J9:J10").Font,*Bold="True"
              If              (LIncTYPE = "C")
              setprop sheet.range("k9","k9"),*Value="Actual",*HorizontalAlignment=xlAlignCenter
              Elseif          (LIncTYPE = "I")
              setprop sheet.range("k9","k9"),*Value="Accrued",*HorizontalAlignment=xlAlignCenter
              ENDIF
              setprop sheet.range("k10","k10"),*Value="Income",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k9:k10").Font,*Bold="True"
              setprop sheet.range("l9","l9"),*Value="Variance",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l9:l9").Font,*Bold="True"
              sheet.range("l9:l10").Merge
              If              (LIncTYPE = "C")
              setprop sheet.range("m9","m9"),*Value="Actual",*HorizontalAlignment=xlAlignCenter
              Elseif          (LIncTYPE = "I")
              setprop sheet.range("m9","m9"),*Value="Accrued",*HorizontalAlignment=xlAlignCenter
              ENDIF
              setprop sheet.range("m10","m10"),*Value="Income",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("m8:m10").Font,*Bold="True"
.borders
.              //LineStyle 1 = Continuous
.              //LineStyle 4 = Dash Dot
.              //Object Viewer Help tells us that xlInsideHorizontal has a value of "12" Decimal
.              //Object Viewer Help tells us that xlInsideVertical has a value of "11" Decimal
.              //Object Viewer Help tells us that xlEdgeright has a value of "10" Decimal
               sheet.range("B11:b37").BorderAround using *LineStyle=1,*Weight=3
               setprop  sheet.Range("B11:b34").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("B36:b37").Borders(12),*LineStyle=1,*Weight=3
               sheet.range("d9:E37").BorderAround using *LineStyle=1,*Weight=3
               setprop  sheet.Range("d9:e34").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("d36:e37").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("d9:e37").Borders(11),*LineStyle=1,*Weight=3
.               sheet.range("D9:E37").Borders *XlBordersIndex=xlInsideVertical
               sheet.range("G9:H37").BorderAround using *LineStyle=1,*Weight=3
               setprop  sheet.Range("g9:H37").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("G9:H37").Borders(11),*LineStyle=1,*Weight=3
               sheet.range("j9:M30").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("j9:m10").BorderAround using *LineStyle=1,*Weight=3
               setprop  sheet.Range("j11:m34").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("j9:M34").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("j36:M37").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("j36:m37").Borders(12),*LineStyle=1,*Weight=3
               sheet.range("j9:m34").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("j36:m37").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("b11:b34").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("b36:b37").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("d9:e34").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("d36:e37").BorderAround using *LineStyle=1,*Weight=3
               setprop sheet.range("L9:L37").Borders(10),*LineStyle=XlLineStyleDBl

...........
.data
.order
              move            C1,Volptr
              MOve            "11",Row1
              move            "12",row2
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
.Invoice
              move            C1,INVptr
              MOve            "11",Row1
              move            "12",row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Loop
              Move            INVArrayPrev(1,INVPtr),APPrev
              Move            INVArrayCur(1,INVPtr),APCur
              Move            PRojArray(INVPtr),PRojection
              pack            Exrange1 from "J",DimRow1
              pack            Exrange2 from "J",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Projection,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "k",DimRow1
              pack            Exrange2 from "k",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=APCur,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "L",DimRow1
              pack            Exrange2 from "L",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              pack            taskname from "=IF(K",Dimrow1,">0,K",DimRow1,"-J",DimRow1,",0"

              setprop sheet.range(ExRange1,ExRange1),*Value=taskname,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "M",DimRow1
              pack            Exrange2 from "M",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=APPrev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              add             c2,row1
              add             c2,Row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              add             c1 to INvptr
              until           (INVPtr=13)
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
              pack            Exrange1 from "J",DimRow1
              pack            Exrange2 from "J",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(J11:J34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "k",DimRow1
              pack            Exrange2 from "k",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(K11:k34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "L",DimRow1
              pack            Exrange2 from "L",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(L11:L34)",*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
              sheet.range(EXRange3).Merge
              pack            Exrange1 from "M",DimRow1
              pack            Exrange2 from "M",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(M11:M34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
              If         (LIncTYPE = "I")             .Invoice date basis
              PACK            STR55,"Income to Owner as of ",DATEOFJOB
              setprop sheet.range("B39","B39"),*Value=str55,*HorizontalAlignment=xlLeft
              Else
              PACK            STR55,"Open Income to Owner as of ",DATEOFJOB
              setprop sheet.range("B39","B39"),*Value=str55,*HorizontalAlignment=xlLeft
              setprop sheet.range("G39","G39"),*Value=APOpen,*NumberFormat="$##,####0.00_)"
              endif
              pack            str55,Copyright," 2005-2006 Names in the News"
              setprop sheet.range("B40","B40"),*Value=Str55,*HorizontalAlignment=xlLeft

              clock timestamp,timestamp
              move            timestamp,endtime
.              setprop sheet.range("B38","B38"),*Value=starttime,*HorizontalAlignment=xlLeft
.              setprop sheet.range("c38","c38"),*Value=endtime,*HorizontalAlignment=xlLeft
              subtract            starttime from endtime
.              setprop sheet.range("d38","d38"),*Value=endtime,*HorizontalAlignment=xlLeft
..............................................................................................................
.Cleanup
              setprop sheet.range("j8:m8").Font,*Name="Times New Roman", *Size=10
              if              (LIncREP2 <> yes)              .no variance delete that column
              Sheet.Range("L1:L65536").Delete *Shift=xlShiftToLeft                 .my syntax is wrong it ignores shift to left
.                                                                                  , by maxing out the row number it works ?
              endif
              if              (ProjFlag <> yes)              .no projection delete that column
              Sheet.Range("J1:J65536").Delete *Shift=xlShiftToLeft
              endif
.in case we did delete columns we are doing cell merges for headers here

              sheet.range("B3:m3").Merge
              sheet.range("B4:m4").Merge
              sheet.range("b5:m5").Merge
              Bump     VolHdrCur,7                           .let reuse and chop off "FISCAL"
              setprop sheet.range("J8","J8"),*Value=VolHdrCur,*HorizontalAlignment=xlAlignCenter
              If              (projflag <> YES & LIncRep2 <> yes)
              sheet.range("J8:j8").Merge
              Elseif          (projflag = YES & LIncRep2 <> yes)
              sheet.range("J8:k8").Merge
              elseif          (projflag = YES & LIncRep2 = yes)
              sheet.range("J8:l8").Merge
              endif
              setprop sheet.range("J8:L8").Font,*Bold="True"
              Reset           VolHdrCur
.save it
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"c:\","work\",LincList,"_",Timestamp
./////
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
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
        Destroy OTRUE
        Destroy OFALSE
        Destroy TopMargin
        Destroy BottomMargin
        Destroy LeftMargin
        Destroy RightMargin
              setprop ex,*DisplayAlerts=OFALSE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        ex.quit
        destroy ex
        Return
...............................................................................
.getPRojections check projection file if records exist pull most current projection
getPRojections
              move EndFiscCur to juldays
              call            cvtgreg
              pack            str7,CC,YY,"001"
              Pack            str4,CC,YY
              Packkey         LSTIFLD,LIncList,str7
              call            Lstikey
              if              Not over                     .we got a live one
              Move            Yes to ProjFlag
              move            LSTIproj,str3
              loop
              call            LstiKS
              until           over
                              if              (LincList <> LSTIlist)        .same list ??
                              Break                                        .nope
                              endif
              move            LSTIproj,str3
              repeat
              Packkey         LSTIFLD,LIncList,str4,str3
              call            Lstikey
              Else
              Move            No to ProjFlag
              return
              endif
.For Fiscal
                              If (FiscMonth <> c1)               .if not equal to one its fiscal
                              Move            lstIm1,ProjArray1(1)
                              Move            lstIm2,ProjArray1(2)
                              Move            lstIm3,ProjArray1(3)
                              Move            lstIm4,ProjArray1(4)
                              Move            lstIm5,ProjArray1(5)
                              Move            lstIm6,ProjArray1(6)
                              Move            lstIm7,ProjArray1(7)
                              Move            lstIm8,ProjArray1(8)
                              Move            lstIm9,ProjArray1(9)
                              Move            lstIm10,ProjArray1(10)
                              Move            lstIm11,ProjArray1(11)
                              Move            lstIm12,ProjArray1(12)
                              move            c1 to n4
                              MOve            FiscMonth,n3
              call            debug
                              For             N2 from c0 to "11" using "1"
                                             IF              (n3 > "12")
                                             Move c1 to N3
                                             endif
                              Move            ProjArray1(N3),ProjArray(n4)
                              add             c1 to n3
                              add            c1 to N4

                              Repeat


                             Else            .Not Fiscal
                             Move            lstIm1,ProjArray(1)
                             Move            lstIm2,ProjArray(2)
                             Move            lstIm3,ProjArray(3)
                             Move            lstIm4,ProjArray(4)
                             Move            lstIm5,ProjArray(5)
                             Move            lstIm6,ProjArray(6)
                             Move            lstIm7,ProjArray(7)
                             Move            lstIm8,ProjArray(8)
                             Move            lstIm9,ProjArray(9)
                             Move            lstIm10,ProjArray(10)
                             Move            lstIm11,ProjArray(11)
                             Move            lstIm12,ProjArray(12)
                             endif
              return
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
.Scrubvars - End of List scrub vars before continueing
ScrubVars
              Move            c0,APOpen
              Move            C0,OrdtotCur
              Move            C0,OrdtotPrev
              Move            C0,ExchTotCur
              Move            C0,ExchTotprev
              Move            C0,RentTotCur
              Move            C0,RentTotprev
              Return
;..........................................................................................
NlOINC0007TabClick
        IF (N1 = C1)
.                getprop Credit001aListView001,visible=N9
.                move    n9 to ListViewNum
.                Deactivate Credita
        else (N1 = C2 )
.                Deactivate Creditb
.        setprop Credit001aListView001,visible=ListViewNum
.        call    CreditSortListView
        Endif
        return

NlOINC0007TabChange
        IF (N1 = C1)
.                move    C1,TabNum
.                Activate Credita
.                setfocus Credit001aListView001
.                setprop Credit001aListView001,visible=ListViewNum
.                call    CreditSortListView
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        else (N1 = C2)
.                getprop Credit001aListView001,visible=N9
.                move    n9 to ListViewNum
.                move    C2,TabNum
.                Activate Creditb
.                setfocus Credit001bEditText003
;Prevent occurance or accumulated events which may place "hidden" objects on wrong form
;ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
;............................................................................................................
LoadNloinc0007view
              packkey        LIncFLD,LincList
              call           LIncKey
.
              SetItem         NLOINC0007EditText001,0,LincList
              SetItem         NLOINC0007StatText002,0,Olstname
              Move            Lmonth,n2
              setitem         NLOINC0007ComboBox001,0,n2
              if              (LINCDATEBY = "M")
              Setitem         NLOINC0007DataList001,0,c1
              ElseIf          (LINCDATEBY = "O")
              Setitem         NLOINC0007DataList001,0,c2
              endif
              if              (LIncTYPE = "C")
              Setitem         NLOINC0007DataList002,0,c1
              ElseIf          (LIncTYPE = "I")
              Setitem         NLOINC0007DataList002,0,c2
              endif
              if              (LIncREP1 = "M")
              Setitem         NLOINC0007DataList003,0,c1
              ElseIf          (LIncREP1 = "Q")
              Setitem         NLOINC0007DataList003,0,c2
              endif
.NOte still need to create and populate projection maintenance screens
              pack            str7,CC,YY,"001"
              Pack            str4,CC,YY
              Packkey         LSTIFLD,LIncList,str7
              call            Lstikey
              if              Not over                     .we got a live one
              Setitem         NLOINC0007DataList004,0,c2
              Else
              Setitem         NLOINC0007DataList004,0,c1
              endif
              if              (LIncAuto = "Y" or LIncAuto = " ")
              Setitem         NLOINC0007DataList005,0,c1
              ElseIf          (LIncAuto = "N")
              Setitem         NLOINC0007DataList005,0,c2
              endif
              Clear           Str4
              Move            LincYear,str4
              Setitem         NLOINC0007EditText002,0,str4
              return
;............................................................................................................
.LoadRepList load list view showing all lists & associated reports
LoadRepList
.first create columns

              NLOINC0007aListView001.InsertColumn using "List",45,1
              NLOINC0007aListView001.InsertColumn using "Name",170,2
              NLOINC0007aListView001.InsertColumn using "Report",70,3
              NLOINC0007aListView001.InsertColumn using "Date Select",65,4
              NLOINC0007aListView001.InsertColumn using "Accrual",50,5
              NLOINC0007aListView001.InsertColumn using "Projections",70,6
              NLOINC0007aListView001.InsertColumn using "Auto",50,7
              NLOINC0007aListView001.InsertColumn using "Fiscal Month",0,8
              NLOINC0007aListView001.SetColumnFormat using 4,1              .set DAYS column justify right
              Setprop NLOINC0007aListView001,Sortorder=2
.load data
                              loop
                              call           LIncks
                              Until over
                                             If Not over
                                             Packkey        Ndatfld,LIncList
                                             call           Ndatkey
                                             NLOINC0007aListView001.InsertItem giving IN9 using LincList
                                             NLOINC0007aListView001.SetItemText using IN9,OLstname,1
                                             if (LIncREP1 = "M")
                                             move           "Monthly",str10
                                             NLOINC0007aListView001.SetItemText using IN9,str10,2
                                             elseIf (LIncREP1 = "Q")
                                             move           "Quarterly",str10
                                             NLOINC0007aListView001.SetItemText using IN9,str10,2
                                             endif
                                             if             (Lincdateby = "M")
                                             move           "Mail date",str10
                                             NLOINC0007aListView001.SetItemText using IN9,str10,3
                                             ElseIf         (Lincdateby = "O")
                                             Move           "Order date",str10
                                             NLOINC0007aListView001.SetItemText using IN9,str10,3
                                             endif
                                             if             (LIncTYPE = "C")
                                             move           "Cash",str7
                                             NLOINC0007aListView001.SetItemText using IN9,str7,4
                                             ElseIf        (LIncTYPE = "I")
                                             move           "Invoice",str7
                                             NLOINC0007aListView001.SetItemText using IN9,str7,4
                                             endif
                                             clear str1                  .projections Yes or no
                                             NLOINC0007aListView001.SetItemText using IN9,str1,5
                                             NLOINC0007aListView001.SetItemText using IN9,LincAuto,6
                                             MOve           Lmonth to str2
                                             NLOINC0007aListView001.SetItemText using IN9,str2,7
                                             endif

                              Repeat
            return
...............................................................................
.EOJ - End of JOB
EOJ
              winshow
              Stop
...............................................................................
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
;BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2
;LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3
;MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
;SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
;BROKER - not an option with this program
              return
        return
SearchLoad2
;LIST
        unpack  Srchstr,str6,str1,str35
        Setitem      NLOINC0007StatText002,0,str35
        return
SearchLoad3
;MAILER- not an option with this program
        return
SearchLoad4
;SHIP-TO - not an option with this program
        return
;
...............................................................................
.IncludeS
              Include         NORDIO.INC
              Include         NDATIO.INC
              Include         NSHPIO.INC
              Include         NMRGIO.INC
              Include         COMPUTE.INC
              Include         NDAT3IO.INC
              Include         nacdIO.inc
              Include         NINVACDIO.INC
              Include         NINVIO.INC
              Include         NJSTIO.INC
              Include         NADJIO.INC
              Include         NUSEIO.INC
              Include         LstIIO.inc
              Include         LIncIO.inc
              Include         COMLOGIC.INC
              Include         PrtPageio.inc
              Include         compIO.inc
              Include         cntIO.inc
              Include         Nownio.inc
;Following used only in order to load Search.plf
              include         ncmpio.inc
              include         nrtnio.inc
              include         searchio.inc      .contains logic for search.plf

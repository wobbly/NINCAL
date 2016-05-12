PC            EQU             1
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
              Include   Hp.inc
              Include         PRtpagedd.inc
.Following used only in order to load Search.plf
               include        ncmpdd.inc
               include        nrtndd.inc
RElease       Init            "1.18"    DLH    Correction clarification on State Date (datetoold)
RElDate       INit            "2015 October 7"
.RElease       Init            "1.17"    DLH    Remove Pia, add dynamic screen resize, add sort by email addy to primary list view
.RElDate       INit            "2013 September 9"
.RElease       Init            "1.16"    DLH    Blind CC Susan
.RElDate       INit            "31 August 2010"
.RElease       Init            "1.15"    DLH     Turn of PLI
.RElDate       INit            "09 March 2010"
.RElease       Init            "1.14"    DLH       Increased array size for UNICEF report
.RElDate       INit            "05 January 2010"
.RElease       Init            "1.13"    DLH       email cleanup
.RElDate       INit            "05 January 2010"
.RElease       Init            "1.12"    DLH       SK does not want to be cc'd on everything
.RElDate       INit            "30 September 2009"
.RElease       Init            "1.11"    DLH       See TEst DH
.RElDate       INit            "2 October 2008"
.RElease       Init            "1.10"   DLH       IF no email address use SHerene
.RElDate       INit            "1 July 2008"
.RElease       Init            "1.09"   DLH       SOme more address checking
.RElDate       INit            "1 June 2008"
.RElease       Init            "1.08"   DLH       Turn on Email of report
.RElDate       INit            "8 May 2008"
.RElease       Init            "1.07"   DLH       Corrected CopyRight
.RElDate       INit            "April 2008"
.RElease       Init            "1.06"   JD        Updated copyrite
.RElDate       INit            "18 Mar 2008"
.RElease       Init            "1.05"   DLH       Add F2 list search
.RElDate       INit            "11 Feb 2008"
.RElease       Init            "1.04"   ASH       19JUN2007 PLI Inclusion
.RElDate       INit            "06/19/2007"
.RElease       Init            "1.03"                     .Listview update in add mode. Delete subroutines finished
.RElDate       INit            "09/26/2006"
.RElease       Init            "1.02"                     .Minor cleanup Auto run
.RElDate       INit            "09/05/2006"
.RElease       Init            "1.01"                     .Minor cleanup manual run
.RElDate       INit            "08/23/2006"
.RElease       Init            "1.0"
.RElDate       INit            "08/02/2006"
.RElease       Init            "Prerelease"
.RElDate       INit            "05/14/2006"
...............................................................................
.begin patch 1.08
trapcount form      4
FileCheck File
.end patch 1.08

mrgsw    dim       1
shipsw   dim       1
adjn2         form            2              .adjustment for loop counter
ProjFlag      Dim             1              .Y if we had projection record
DateOKFlag          Dim       1              .if user change report date and format is not valid = "N"
DateRan   Dim       10        .date Manual job run
AddMode   DIm       1         "T" if in add mode
.
QrtCheck  form      2              .Holds month of report date - in auto mode used to allow or suppress Quarterly reps
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
BegFiscCur    FORM            5              .Year 1
EndFiscCur    FORM            5              .Year 1
BegFiscPrev   FORM            5              .Year 2
EndFiscPrev   FORM            5              .Year 2
BegFiscPrev3   FORM            5              .Year 3
EndFiscPrev3   FORM            5              .Year 3
BegFiscPrev4   FORM            5              .Year 4
EndFiscPrev4   FORM            5              .Year 4
BegFiscPrev5   FORM            5              .Year 5
EndFiscPrev5   FORM            5              .Year 5
BegFiscPrev6   FORM            5              .Year 6
EndFiscPrev6   FORM            5              .Year 6
DateToOld Form      5         .if LIncYEAR has a value convert to Julian do not include records prior
AutoFlag      Init            "Y"              .Holds 'Y' if usual auto run else its manual submission
LYEAR         FORM            4
ForceDay      Dim             4
.
LRArrayPtr    form            5              .pointer for array
.begin patch 1.14
.increased for Unicef 01/05/10  DLH   Error was F02
.LRArray       Record          (8000)               .array to hold valid lrs and related invoice Number
.end patch 1.14
LRArray       Record          (8500)               .array to hold valid lrs and related invoice Number
TempLR        Dim             6
TempINv       Dim             6
              REcordend

VolPtr        Form            2
.monthly report
VolArrayPrev  FOrm            15(2,12)         . level 1  rent, level 2 exch
VolArrayCur   FOrm            15(2,12)         . level 1  rent, level 2 exch
.Quarterly report
VolArrayyear1 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch
VolArrayyear2 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch
VolArrayyear3 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch
VolArrayyear4 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch
VolArrayyear5 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch
VolArrayyear6 FOrm            15(2,4)         . level 1 Quarterly rent vol, level 2  exch

InvPtr        Form            2
.monthly report
InvArrayPrev  FOrm            15.2(1,12)         . level 1 total monthly  AP Prev year
InvArrayCur   FOrm            15.2(1,12)         . level 1 total monthly AP  Current Year
PRojArray     Form            9.2(12)           .projections $
PRojArray1     Form            9.2(12)           .projections $ for sorting fiscal's
.Quarterly report
InvArrayyear1  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
InvArrayyear2  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
InvArrayyear3  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
InvArrayyear4  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
InvArrayyear5  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
InvArrayyear6  FOrm            15.2(1,4)         . level 1 total quarterly  AP 
YearFlag   Form      1
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
HDRYear1      DIm             32               .header for quarterly 6 year rep
HDRYear2      DIm             32               .header for quarterly 6 year rep
HDRYear3      DIm             32               .header for quarterly 6 year rep
HDRYear4      DIm             32               .header for quarterly 6 year rep
HDRYear5      DIm             32               .header for quarterly 6 year rep
HDRYear6      DIm             32               .header for quarterly 6 year rep
APPrev        Form            15.2             .previous year A/P
APCur         Form            15.2             .Current Year A/P
PRojection    form            15.2             .Projected
APOpen        Form            15.2             .Open Payables
starttime     form            17               .self describing :)
endtime       form            17               .self describing :)
Tabnum    form      2           .tab changes
RepLoop   Form      9
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
.XlShiftToLeft  Integer 4,"0xffffefc1"                         .delete shift to left
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
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
Row3          form            2
Row4          Form            2
DimRow1       Dim             2
DimRow2       Dim             2
DimRow3       Dim             2
DimRow4       Dim             2
.START PATCH 1.04 REPLACED LOGIC
TASKNAME2 DIM       200
SReturn        init 0x0a                     ;soft return/line feed
HOLDEXCL  dim       1
AlignCenter integer 4,"0xffffeff4"
.END PATCH 1.04 REPLACED LOGIC

.Set Up Menu Bar
mFile    menu
mHelp    menu
MPrint   menu
.Present Data for Menu Bar
FData   init    "&File;E&xit"
HData   init    "&Help;&About"
SData   init    "&Report;&Excel"

.............................................................................
.Main
.Entry to program check for auto or manual if auto read list file and process all
. If Manual use supplied list # pull info from file & process
.............................................................................................................
              rep             lowup,PROGRAM   .case sensitive
              IF           (PRogram = "NLOINC0007")    .loaded from dsprog - Auto mode
              MOve            Yes,AutoFlag
          Elseif       (InpName = "NLOINC0007")    .loaded from dsprog - Auto mode  until program is bumped up
              MOve            Yes,AutoFlag
              Else
              MOve            No,AutoFlag
              endif
.Set Vars used for About Box
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
NLoincb        plform         NLOINC0007B
x              plform         NLOINC0007
               winhide
.;Load Forms, Always load parent form first
               formload       x
               formload       NLOincb,NLOINC0007
               formload       NLOinca,NLOINC0007
               formload       abt
               formload       mss1
               formload       SRCH

        CREATE  NLOINC0007;MFile,FData
        create  NLOINC0007;Mprint,SData,MFile
        create  NLOINC0007;mHelp,HData,Mprint


        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,EOJ,RESULT
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate MPrint,PrintGo,result
        activate mHelp,HelpGo,result

              clock timestamp,timestamp
              move            timestamp,starttime
              unpack timestamp,str2,yy,mm,dd
              if    (autoflag = yes)
              unpack          today into mm,str1,dd,str1,yy
              move  "20",str2
              endif
              call            cvtjul
              move juldays  to TODAYIS
              pack DATEofJOB with mm,slash,dd,slash,str2,yy
              MOve  mm,Qrtcheck
              pack  DateRan with mm,slash,dd,slash,str2,yy
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
.               Move            Yes,AutoFlag
.               Move            No,AutoFlag
.              call            output
.              goto            eoj
               Deactivate     NLOInca
               Deactivate     NLOIncb
               move           c1 to n1
              call           NlOINC0007TabChange

              call            LoadRepList

              If              (AutoFlag = YES)
                              SetItem NLOINC0007StatText008,0,"Running EOM Reports"
                              SETPROP       NLOINC0007aListView001,ENABLED=0         .disable list view so user cant change
                              NLOINC0007aListView001.SetItemState using Seq,C2,c2
                              move     SEQ,result
                              Loop
                              CAll           ClearLRArray
                              Call           ClearVolArray
                              call           ClearProjArray
                              call           ClearInvArray
                              call           ScrubVars
                              move     result,IN9
                              NLOINC0007aListView001.GetNextItem giving result using C2,IN9  // -1 is error code
                              move      Result,RepLoop                          .may get destroyed save it
                              NLOINC0007aListView001.GetItemText giving str6 using result,0
                              NLOINC0007aListView001.EnsureVisible Using Result,1         
                              Move str6,LincList
                              NLOINC0007aListView001.SetItemState using Result,C0,c2
.
                              NLOINC0007aListView001.GetItemText Giving LincAuto using result,6

.begin patch 1.01
                              until (RepLoop = Seq)           .-1 =  no selected items get out
.end patch 1.01

                           IF             (LincAuto <> "N")
                                             NLOINC0007aListView001.GetItemText Giving olstname using Result,1
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
                                                             if             (str7 = "Cash")
                                                             move           "C",LincType
                                                             Else
                                                             move           "I",LincType
                                                             endif
.dh test
                                        call debug    
                                        Move      c0 to str8
                                        Move      LincYear,str8
                                        Setitem         NLOINC0007EditText002,0,str8
.dh test
                                             NLOINC0007aListView001.GetItemText Giving str2 using Result,7
                                             move           str2 to Lmonth

.                                        NLOINC0007aListView001.GetItemText Giving str8 using In9,8
                                        NLOINC0007aListView001.GetItemText Giving str8 using result,8
                                        Move      c0 to LincYear
                                        Move      Str8 to LincYear                                           
                                        Setitem         NLOINC0007EditText002,0,str4
                                             Endif
.begin patch 1.13
.                                        NLOINC0007aListView001.GetItemText Giving LincRecipient using In9,9
                                        call      debug
                                        Clear     LincRecipient
.                                        NLOINC0007aListView001.GetItemText Giving LincRecipient using In9,10
                                        NLOINC0007aListView001.GetItemText Giving LincRecipient using result,10
.end patch 1.13
.test DH
.                                        pack      Taskname from "Email",LincRecipient
.                                      alert   caution,taskname,result


.         
                                             IF  (Result <> Seq & LincAuto <> "N")            .if report is auto and we are not done
                                        SetItem         NLOINC0007EditText001,0,LincList
                                        SetItem         NLOINC0007StatText002,0,Olstname
.                                                                       
                                        Move            Lmonth,n2
                                        setitem         NLOINC0007ComboBox001,0,n2
                                        if              (LINCDATEBY = "M")
                                        setitem         NLOINC0007ComboBox002,0,c2
.                                       Setitem         NLOINC0007DataList001,0,c2
                                        ElseIf          (LINCDATEBY = "O")
                                        setitem         NLOINC0007ComboBox002,0,c3
.                                       Setitem         NLOINC0007DataList001,0,c3
                                        endif
                                        if              (LIncTYPE = "C")
                                        Setitem         NLOINC0007Combobox004,0,c2
                                        ElseIf          (LIncTYPE = "I")
                                        Setitem         NLOINC0007Combobox004,0,c3
                                        endif
                                        if              (LIncREP1 = "M")
                                        Setitem         NLOINC0007Combobox003,0,c2
                                        ElseIf          (LIncREP1 = "Q")
                                        Setitem         NLOINC0007Combobox003,0,c3
                                        endif
                                        if              (LIncAuto = "Y" or LIncAuto = B1)
                                        Setitem         NLOINC0007Combobox006,0,c1
                                        endif
                                        if              (LIncREP1 = "N" or LIncREP1 = " ")
                                        Setitem         NLOINC0007Combobox007,0,c2
                                        ElseIf          (LIncREP1 = "Y")
                                        Setitem         NLOINC0007Combobox007,0,c3
                                        endif
                                        Setitem       Nloinc0007EditText003,0,LincRecipient
                                                  MOve           Lmonth,Fiscmonth
                                                            IF (LIncREP1 = "M")                      .only ready for monthly so far 6/1/06
                                                            call           Monthly
                                                            elseIF         (QrtCheck = 3 or QrtCheck = 6 or QrtCheck = 9 or QrtCheck =12) 
                                                  call           Quarterly
                                                            endif

                                             endif
.begin patch 1.01
.                   until (RepLoop = SEQ)           .-1 =  no selected items get out
.end patch 1.01
                              call      debug
                              Repeat
                              SetItem NLOINC0007StatText008,0,"EOM Reports Done"

                              GOto           Eoj
              Else

           EVENTREG  X, 17, XRESIZE

              loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
              repeat
              goto  Eoj
              endif
ManualRun
          SetItem NLOINC0007StatText008,0,"Running Report(s)"
              SETPROP       NLOINC0007aListView001,ENABLED=0         .disable list view so user cant change
          
.1st did the user change the report date
                    call      Chkrepdate
                              if        (DateokFLag = Yes)           .user changed report date
                              Unpack              str10,MM,SLASH,DD,SLASH,STR2,YY
                              call            cvtjul
                              move      juldays  to TODAYIS
                              PACK            FORCEDAY,str2,YY
                                        Else
                                        return
                    endif
.2nd check and see if any selected
              move     SEQ,result
              loop

              CAll           ClearLRArray
          Call           ClearVolArray
          call           ClearProjArray
          call           ClearInvArray
          call           ScrubVars

              move     result,IN9
              NLOINC0007aListView001.GetNextItem giving result using C2,IN9  // -1 is error code
              move  Result,RepLoop                          .may get destroyed save it
              NLOINC0007aListView001.EnsureVisible Using Result,1     
              NLOINC0007aListView001.GetItemText giving str6 using result,0
              Move str6,LincList
              NLOINC0007aListView001.SetItemState using Result,C0,c2
              NLOINC0007aListView001.GetItemText Giving LincAuto using result,6
              NLOINC0007aListView001.GetItemText Giving str10 using Result,2
.begin patch 1.01
              until (RepLoop = SEQ)
.end patch 1.01
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
                             if             (str7 = "Cash")
                             move           "C",LincType
                             Else
                             move           "I",LincType
                             endif
.test DH
          call      debug
          getitem         NLOINC0007ComboBox001,0,n2
          Move            n2,Lmonth
          
.              NLOINC0007aListView001.GetItemText Giving str2 using Result,7
.              move           str2 to Lmonth
.         NLOINC0007aListView001.GetItemText Giving str4 using In9,8
          Move      c0 to LincYear
          Getitem         NLOINC0007EditText002,0,str8
                    Move      Str8 to LincYear     
.test DH
                    
          SetItem         NLOINC0007EditText001,0,LincList
          SetItem         NLOINC0007StatText002,0,Olstname
          SetItem         NLOINC0007EditText003,0,LIncRECIPIENT
          
.                                                                      
          Move            Lmonth,n2

              setitem         NLOINC0007ComboBox001,0,n2

              if              (LINCDATEBY = "M")
              setitem         NLOINC0007ComboBox002,0,c2
.              Setitem         NLOINC0007DataList001,0,c2
                    ElseIf          (LINCDATEBY = "O")
              setitem         NLOINC0007ComboBox002,0,c3
.              Setitem         NLOINC0007DataList001,0,c3
              endif

              Setitem         NLOINC0007Combobox004,0,c1

              if              (LIncTYPE = "C")
              Setitem         NLOINC0007Combobox004,0,c2
                    ElseIf          (LIncTYPE = "I")
              Setitem         NLOINC0007Combobox004,0,c3
              endif

              if              (LIncREP1 = "M")
              Setitem         NLOINC0007Combobox003,0,c2
                    ElseIf          (LIncREP1 = "Q")
              Setitem         NLOINC0007Combobox003,0,c3
              endif
          if              (LIncAuto = "Y" or LIncAuto = B1)
          Setitem         NLOINC0007Combobox006,0,c1
          endif
              if              (LIncREP1 = "N" or LIncREP1 = " ")
              Setitem         NLOINC0007Combobox007,0,c2
                    ElseIf          (LIncREP1 = "Y")
              Setitem         NLOINC0007Combobox007,0,c3
              endif


                              IF (LIncREP1 = "M")                     
                              call           Monthly
                              else
                              call           Quarterly
                              endif
.begin patch 1.01
.              until (RepLoop = SEQ)
.end patch 1.01
              repeat
.              NLOINC0007aListView001.SetItemState using In9,C0,c2
              setprop   NLOINC0007aButton001,Enabled=1
          SetItem NLOINC0007StatText008,0,B1
          setprop NLOINC0007aListView001,ENABLED=1                   .turn it back on
          setprop NLOINC0007aEditText001,Enabled=1           .turn it back on
              return


Monthly
                              packkey Ndatfld,LIncList
                              call Ndatkey
                              if over
.add alert
                              return
                              endif
.
.START PATCH 1.04 ADDED LOGIC
                                        move      ELSTCDE,HOLDEXCL
.END PATCH 1.04 ADDED LOGIC
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
.START PATCH 1.04 ADDED LOGIC
                                        move      ELSTCDE,HOLDEXCL
.END PATCH 1.04 ADDED LOGIC
                              call SetDates
                              call ProcessOrder
                              CAll ProcessINv
                              call ProcessAdj
                              call getPRojections
                              call output
                              return

.ProcessOrder We have a valid list # with Criteria get orders as defined
ProcessOrder
              MOVE            C1 TO NORDPATH
              Pack            NORDFLD2,"02L",lSTNUM
              call            nordaim
              IF              NOT OVER
                              Call            OrderCriteria
              Else
                    REturn              .Yikes DH 01 April 2008
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
.                              call           debug
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
.OrderCriteria   two year report - Monthly
OrderCriteria
              Call            LoadOrderArray                               .Save LR for Invoice processing
              IF             (LIncRep1 = "Q")   .Quarterly report
              goto  OrderCriteriaQ
              endif

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
           call debug
           If (DateToOld <> 0)
                    IF        (juldays < DateToOld)
                    goto      ORderExit
                    endif
           endif              
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
.                             MOve            c0,N9
.                             Move            Oqty,N9
.                             add             N9,VolArrayPrev(1,Volptr)
.Rent Qty
                             MOve            c0,N9
                             Move            Rqty,N9
.                             add             N9,VolArrayPrev(2,Volptr)
                             add             N9,VolArrayPrev(1,Volptr)
.Exch Qty
                             MOve            c0,N9
                             Move            Exqty,N9
.                             add             N9,VolArrayPrev(3,Volptr)
                             add             N9,VolArrayPrev(2,Volptr)
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
.                             MOve            c0,N9
.                             Move            Oqty,N9
.                             add             N9,VolArrayCur(1,Volptr)
.Rent Qty
                             MOve            c0,N9
                             Move            Rqty,N9
.                             add             N9,VolArrayCur(2,Volptr)
                             add             N9,VolArrayCur(1,Volptr)
.Exch Qty
                             MOve            c0,N9
                             Move            Exqty,N9
.                             add             N9,VolArrayCur(3,Volptr)
                             add             N9,VolArrayCur(2,Volptr)
                             endif
                             endif
.end of Current year order volume
OrderExit
              Return
...............................................................................
.OrderCriteriaQ - order selection for Quarterly (six year report)
OrderCriteriaQ
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
.previous years?
               if ((Juldays >= BegFiscPrev6) & (Juldays  <=  EndFiscPrev6))
               move c6 to yearflag
               elseIf ((Juldays >= BegFiscPrev5) & (Juldays  <=  EndFiscPrev5))
               move c5 to yearflag
               elseIf ((Juldays >= BegFiscPrev4) & (Juldays  <=  EndFiscPrev4))
               move c4 to yearflag
               elseIf ((Juldays >= BegFiscPrev3) & (Juldays  <=  EndFiscPrev3))
               move c3 to yearflag
               elseIf ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
               move c2 to yearflag
               elseIf ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
               move c1 to yearflag
               Else
               Move c0 to Yearflag
                    If        (dateToOld <> 0)
                              IF        (Juldays < DateToOld)
                              move      C0 to YearFlag              .skip to old
                              endif
                    Endif     
               endif
.test DH
          call      debug
                    If        (dateToOld <> 0)
                              IF        (Juldays < DateToOld)
                              move      C0 to YearFlag              .skip to old
                              endif
                    Endif     
.test DH
               
                              If (OSTAT = "B" or OSTAT = "0")
                                             clear TMPVAR
                                             if (oexqty > "0")
                                                            move oexqty to n9
                                                            move           oqty to n10
                                                            sub n9 from n10,TMPVAR
                                                            move           TMPVAR to RQTY
                                                            move           n9 to EXQTY
                                                            add            EXQTY to EXCHTOTPRev
                                                            add            RQTY to RENTTOTprev
                                             else
                                                            reset excodes
                                                            scan oelcode in excodes
                                                            if equal
                                                                           move           oqty,EXQTY
                                                                           add            EXQTY,EXCHTOTPrev
                                                            else
                                                                           move           oqty,RQTY
                                                                           add            RQTY,RENTTOTPRev
                                                            endif
                                             endif
                            Endif
              IF    (YearFlag = c6)              .year 6
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayyear6(1,1)
                              add Exqty,VolArrayyear6(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayyear6(1,2)
                              add Exqty,VolArrayyear6(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayyear6(1,3)
                              add Exqty,VolArrayyear6(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayyear6(1,4)
                              add Exqty,VolArrayyear6(2,4)
                                        endif
                    Elseif    (YearFlag = c5)               
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayYear5(1,1)
                              add Exqty,VolArrayYear5(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayYear5(1,2)
                              add Exqty,VolArrayYear5(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayYear5(1,3)
                              add Exqty,VolArrayYear5(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayYear5(1,4)
                              add Exqty,VolArrayYear5(2,4)
                                        endif
                    Elseif    (YearFlag = c4)              
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayYear4(1,1)
                              add Exqty,VolArrayYear4(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayYear4(1,2)
                              add Exqty,VolArrayYear4(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayYear4(1,3)
                              add Exqty,VolArrayYear4(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayYear4(1,4)
                              add Exqty,VolArrayYear4(2,4)
                                        endif
                    Elseif    (YearFlag = c3)               
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayYear3(1,1)
                              add Exqty,VolArrayYear3(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayYear3(1,2)
                              add Exqty,VolArrayYear3(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayYear3(1,3)
                              add Exqty,VolArrayYear3(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayYear3(1,4)
                              add Exqty,VolArrayYear3(2,4)
                                        endif
                    Elseif    (YearFlag = c2)               
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayYear2(1,1)
                              add Exqty,VolArrayYear2(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayYear2(1,2)
                              add Exqty,VolArrayYear2(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayYear2(1,3)
                              add Exqty,VolArrayYear2(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayYear2(1,4)
                              add Exqty,VolArrayYear2(2,4)
                                        endif
                    Elseif    (YearFlag = c1)
                              if (Volptr >= c1 & VolPtr <= c3)             .1st quarter
                              add Rqty,VolArrayYear1(1,1)
                              add Exqty,VolArrayYear1(2,1)
                              Elseif (Volptr >= c4 & VolPtr <= c6)         .2nd Quarter
                              add Rqty,VolArrayYear1(1,2)
                              add Exqty,VolArrayYear1(2,2)
                              Elseif (Volptr >= c7 & VolPtr <= c9)    .3rd Quarter
                              add Rqty,VolArrayYear1(1,3)
                              add Exqty,VolArrayYear1(2,3)
                                        Elseif (Volptr >= c10 & VolPtr <= "12") .4th Quarter
                              add Rqty,VolArrayYear1(1,4)
                              add Exqty,VolArrayYear1(2,4)
                                        endif               
                    endif

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
              IF             (LIncRep1 = "Q")   .Quarterly report
              goto  InvoiceCriteriaQ
              endif
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
                    If (DateToOld <> 0)
                              if        (Juldays < DateToOld)
                              goto      INvExit                                .skip
                              endif
                    Endif     
              if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
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
.         if        (INvptr = c1)
.         call      debug
.         endif
               add             AP1,InvArrayPrev(1,INVptr)
              endif
.end of Current year
INvExit
              Return
...............................................................................
.InvoiceCriteriaQ   for Quarterly 6 year report
InvoiceCriteriaQ
              Move            C0 to Juldays
              packkey         NinvFld,olrn
              call            Ninvkey
              if              not over
              MOve            c0 to N2
                              if (LIncTYPE = "C")      .CHEck Date - Cash Basis
                                             If             (statb <> "P")        .not paid get out
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
               if ((Juldays >= BegFiscPrev6) & (Juldays  <=  EndFiscPrev6))
               move c6 to yearflag
               elseIf ((Juldays >= BegFiscPrev5) & (Juldays  <=  EndFiscPrev5))
               move c5 to yearflag
               elseIf ((Juldays >= BegFiscPrev4) & (Juldays  <=  EndFiscPrev4))
               move c4 to yearflag
               elseIf ((Juldays >= BegFiscPrev3) & (Juldays  <=  EndFiscPrev3))
               move c3 to yearflag
               elseIf ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
               move c2 to yearflag
               elseIf ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
               move c1 to yearflag
               Else
               Move c0 to Yearflag
               endif
           If       (DateToOld <> 0)
                    if        (Juldays < DateToOld)
                    move      c0 to yearflag                 .to old don't include
                    endif
           endif    

                              if (LIncTYPE = "C")      .CHEck Date - Cash Basis
                              call           GetAdjCash
                              endif

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

              IF    (YearFlag = c6)              .year 6
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear6(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear6(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear6(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear6(1,4)
                                        endif
                    Elseif    (YearFlag = c5)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear5(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear5(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear5(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear5(1,4)
                                        endif
                    Elseif    (YearFlag = c4)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear4(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear4(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear4(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear4(1,4)
                                        endif
                    Elseif    (YearFlag = c3)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear3(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear3(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear3(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear3(1,4)
                                        endif
                    Elseif    (YearFlag = c2)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear2(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear2(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear2(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear2(1,4)
                                        endif
                    Elseif    (YearFlag = c1)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear1(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear1(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear1(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear1(1,4)
                                        endif
          Endif
          return
...............................................................................
.GetAdjCash - cash basis lets get the correct payment amount
GetAdjCash
              packkey         Nadjfld,Olrn
              CALL           NADJKEY
              if              Not over
                              add             ASPAYAD1,ap1
              endif
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
.              If              (olrn ="452592")
.              call            debug
.              endif
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
              IF             (LIncRep1 = "Q")   .Quarterly report
              goto  AdjustCriteriaQ
              endif

              if              (LIncTYPE = "C")             .cash basis only care about code 14
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
              if    (DateToOld <> 0)
                              if        (JulDays < DateToOld)
                              goto      adjexit
                              endif
                    endif               
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
.         if        (INvptr = c1)
.         call      debug
.         endif
               add             AP1,InvArrayPrev(1,Invptr)
              endif
.end of Current year
AdjExit
              Return
...........................
.AdjustCriteriaQ              .Quarterly six year rep
AdjustCriteriaQ              
              if              (LIncTYPE = "C")             .cash basis only care about code 14
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
               if ((Juldays >= BegFiscPrev6) & (Juldays  <=  EndFiscPrev6))
               move c6 to yearflag
               elseIf ((Juldays >= BegFiscPrev5) & (Juldays  <=  EndFiscPrev5))
               move c5 to yearflag
               elseIf ((Juldays >= BegFiscPrev4) & (Juldays  <=  EndFiscPrev4))
               move c4 to yearflag
               elseIf ((Juldays >= BegFiscPrev3) & (Juldays  <=  EndFiscPrev3))
               move c3 to yearflag
               elseIf ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
               move c2 to yearflag
               elseIf ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
               move c1 to yearflag
               Else
               Move c0 to Yearflag
               endif
               IF   (DateToOld <> 0)
                    if        (JulDays < DateToOld)
                    move      c0 to Yearflag                 .to old skip it
                    endif
               endif          
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
              IF    (YearFlag = c6)              .year 6
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear6(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear6(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear6(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear6(1,4)
                                        endif
                    Elseif    (YearFlag = c5)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear5(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear5(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear5(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear5(1,4)
                                        endif
                    Elseif    (YearFlag = c4)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear4(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear4(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear4(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear4(1,4)
                                        endif
                    Elseif    (YearFlag = c3)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear3(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear3(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear3(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear3(1,4)
                                        endif
                    Elseif    (YearFlag = c2)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear2(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear2(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear2(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear2(1,4)
                                        endif
                    Elseif    (YearFlag = c1)               
                              if (InvPtr >= c1 & InvPtr <= c3)             .1st quarter
                              add AP1,InvArrayyear1(1,1)
                              Elseif (InvPtr >= c4 & InvPtr <= c6)         .2nd Quarter
                              add AP1,InvArrayyear1(1,2)
                              Elseif (InvPtr >= c7 & InvPtr <= c9)    .3rd Quarter
                              add AP1,InvArrayyear1(1,3)
                                        Elseif (InvPtr >= c10 & InvPtr <= "12") .4th Quarter
                              add AP1,InvArrayyear1(1,4)
                                        endif
          Endif
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
.              Move            C0,VolArrayCur(3,Volptr)
              Move            C0,VolArrayPrev(1,Volptr)
              Move            C0,VolArrayPrev(2,Volptr)
.              Move            C0,VolArrayPrev(3,Volptr)
              add             c1 to Volptr
              until           (volPtr=13)
              repeat
              move            C1,Volptr
              Loop
              Move            C0,VolArrayYear1(1,Volptr)
              Move            C0,VolArrayYear2(1,Volptr)
              Move            C0,VolArrayYear3(1,Volptr)
              Move            C0,VolArrayYear4(1,Volptr)
              Move            C0,VolArrayYear5(1,Volptr)
              Move            C0,VolArrayYear6(1,Volptr)
              Move            C0,VolArrayYear1(2,Volptr)
              Move            C0,VolArrayYear2(2,Volptr)
              Move            C0,VolArrayYear3(2,Volptr)
              Move            C0,VolArrayYear4(2,Volptr)
              Move            C0,VolArrayYear5(2,Volptr)
              Move            C0,VolArrayYear6(2,Volptr)
              add             c1 to Volptr
              until           (volPtr=5)
              repeat
              move            C0,volptr
              Return
...............................................................................
.Clear Inv arrays for next run
ClearInvArray
              move            C1,InvPtr
              Loop
              Move            C0,InvArrayCur(1,InvPtr)             .monthly
              Move            C0,InvArrayPrev(1,InvPtr)
              add             c1 to InvPtr
              until           (InvPtr=13)
              repeat
              move            C1,InvPtr
              Loop
              Move            C0,InvArrayyear1(1,InvPtr)            .Quarterly
              Move            C0,InvArrayyear2(1,InvPtr)
              Move            C0,InvArrayyear3(1,InvPtr)
              Move            C0,InvArrayyear4(1,InvPtr)
              Move            C0,InvArrayyear5(1,InvPtr)
              Move            C0,InvArrayyear6(1,InvPtr)
              add             c1 to InvPtr
              until           (InvPtr=5)
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
              If              (LIncRep1 = "Q")   .Quarterly report
              pack            HdrYear1,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              endif
              move BegFiscPrev to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev to juldays
              call            cvtgreg
              Clear           Taskname
              If                (LIncRep1 = "M")   .Monthly report
              PAck            taskname with ReportHdr,"FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
          Elseif          (LIncRep1 = "Q")   .Quarterly report 
          unpack    str10 into str5
              PAck            taskname with ReportHdr,"FISCAL YEAR (",str5," - ",MM,"/",DD,")"
              endif
          pack VolHdrPrev with "FISCAL YEAR ",CC,YY
              reset taskname
.and report title
              If              (LIncRep1 = "Q")   .Quarterly report
              pack            HdrYear2,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              move BegFiscPrev3 to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev3 to juldays
              call            cvtgreg
              pack            HdrYear3,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              move BegFiscPrev4 to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev4 to juldays
              call            cvtgreg
              pack            HdrYear4,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              move BegFiscPrev5 to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev5 to juldays
              call            cvtgreg
              pack            HdrYear5,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              move BegFiscPrev6 to juldays
              call            cvtgreg
              pack            str10 with mm,"/",DD,"/",YY
              move EndFiscPrev6 to juldays
              call            cvtgreg
              pack            HdrYear6,"FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY               
              endif

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
              If              (LIncRep1 = "M")   .Monthly report
.Build Month Labels - Monthly report
.lets offset So that Month 1 of array is first month of fiscal or calendar year
              Clear           Str36
              MOve            FiscMonth,n3
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
.START PATCH 1.04 REPLACED LOGIC
.              sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
.......................
.begin patch 1.15
.                    if (HOLDEXCL = "P")
.                              sheet.Shapes.AddPicture using "\\nins1\e\netutils\pacificlists.jpg",OTRUE,OTRUE,0,0,144,45
.                    
..                             clear taskname2
..                             setprop   xlRowHeight,VarValue="65.0"
..                             setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
..                             setprop sheet.range("A1"),*HorizontalAlignment=AlignCenter
..                             setprop sheet.range("A1").Font,*Name="Times New Roman",*Size=8,*Bold="True"
..                             append    "Pacific Lists, Inc.",taskname2
..                             append    Sreturn,taskname2
..                             append    "1300 Clay St. 11th Floor",taskname2
...                             append    Sreturn,taskname2
..                             append    "Oakland, CA 94612-1429",taskname2
..                             append    Sreturn,taskname2
..                             append    "415-945-9450 · Fax 415-945-9451",taskname2
..                             append    Sreturn,taskname2
..                             append    "A Division of Names in the News",taskname2
..                             reset     taskname2
..                             setprop   sheet.range("E1"),*Value=taskname
..                             setprop sheet.range("EA1").Characters(1,19).Font,*Size=14
..                             sheet.range("A1:E1").Merge
..                             setprop   xlRowHeight,VarValue="2.75"
.                    else
             Sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.                    endif
.end patch 1.15
                    setprop sheet.range("d1:z250").Font,*Name="Times New Roman", *Size=11
.END PATCH 1.04 REPLACED LOGIC
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin

.              setprop         sheet.range("A35:A35").Rows,*RowHeight=xlRowHeight
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
              setprop sheet.range("D9:E10").Font,*Bold="True"
              sheet.range("d9:d10").Merge
              setprop sheet.range("D9:D10"),*WrapText=OTRUE
              setprop sheet.range("E9","E9"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D9:E10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("E9","E9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("E9","E9"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E10","E10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E10","E10"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G9","G9"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G9:H10").Font,*Bold="True"
              sheet.range("G9:G10").Merge
              setprop sheet.range("H9","H9"),*Value="Exchange Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g9:H10").Font,*Name="Times New Roman", *Size=9
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
              setprop sheet.range("J9:k10").Font,*Bold="True"
              setprop sheet.range("l9","l9"),*Value="Variance",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l9:K10").Font,*Bold="True"
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

              Elseif          (LIncRep1 = "Q")   .Quarterly report

.START PATCH 1.04 REPLACED LOGIC
.                   sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
..................
.begin patch 1.15
.                    if (HOLDEXCL = "P")
.                              sheet.Shapes.AddPicture using "\\nins1\e\netutils\pacificlists.jpg",OTRUE,OTRUE,0,0,144,45
..                             clear taskname2
..                             setprop   xlRowHeight,VarValue="65.0"
..                             setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
..                             setprop sheet.range("A1"),*HorizontalAlignment=AlignCenter
..                             setprop sheet.range("A1").Font,*Name="Times New Roman",*Size=8,*Bold="True"
..                             append    "Pacific Lists, Inc.",taskname2
..                             append    Sreturn,taskname2
..                             append    "1300 Clay St. 11th Floor",taskname2
..                             append    Sreturn,taskname2
..                             append    "Oakland, CA 94612-1429",taskname2
..                             append    Sreturn,taskname2
..                             append    "415-945-9450 · Fax 415-945-9451",taskname2
..                             append    Sreturn,taskname2
..                             append    "A Division of Names in the News",taskname2
..                             reset     taskname2
..                             setprop   sheet.range("A1"),*Value=taskname
..                             setprop sheet.range("A1").Characters(1,19).Font,*Size=14
..                             sheet.range("A1:E1").Merge
..                             setprop   xlRowHeight,VarValue="2.75"
.                    else
                              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.                    endif
.end patch 1.15
                    setprop sheet.range("d1:z250").Font,*Name="Times New Roman", *Size=11
.END PATCH 1.04 REPLACED LOGIC
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin
              setprop         sheet.range("a1:a1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("f1:f1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("j1:j1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="12.00"
              setprop         sheet.range("C1:e1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("g1:i1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("k1:m1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="10."
              setprop         sheet.range("B1:B1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("G1:G1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("L1:L1").Columns,*ColumnWidth=xlColumnWidth
              setprop sheet.range("b3","B3"),*Value=Olstname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b3:b3").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b3:b3").Font,*Bold="True"
              sheet.range("b3:m3").Merge
              setprop sheet.range("B4","B4"),*Value=ReportHdr,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B4:B4").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("B4:B4").Font,*Bold="True"
              sheet.range("b4:n4").Merge
              setprop sheet.range("B5","B5"),*Value=taskname,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B5:B5").Font,*Name="Times New Roman", *Size=11
              setprop sheet.range("B5:B5").Font,*Bold="True"
              sheet.range("b5:m5").Merge
.year one
              setprop sheet.range("b8","b8"),*Value=HDRYear1,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b8:b8").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("b8:b8").Font,*Bold="True"
              sheet.range("b8:E8").Merge
              setprop sheet.range("B11","B11"),*Value="1st Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b11:b11").Font,*Bold="True"
              sheet.range("B11:B12").Merge
              setprop sheet.range("B13","B13"),*Value="2nd Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b13:b13").Font,*Bold="True"
              sheet.range("B13:B14").Merge
              setprop sheet.range("B15","B15"),*Value="3rd Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b15:b15").Font,*Bold="True"
              sheet.range("B15:B16").Merge
              setprop sheet.range("B17","B17"),*Value="4th Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b17:b17").Font,*Bold="True"
              sheet.range("B17:B18").Merge
              setprop sheet.range("B19","B19"),*Value="Totals",*HorizontalAlignment=xlLeft
              setprop sheet.range("b19:b19").Font,*Bold="True"
              sheet.range("B19:B20").Merge
              setprop sheet.range("c9","c9"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c10","c10"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c9:E10").Font,*Bold="True"
              setprop sheet.range("c9:e10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("d9","d9"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d9","d9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("d9","d9"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d10","d10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d10","d10"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("e9","e9"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("e9:e10").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("e9","e9"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("e10","e10"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.
.year two
              setprop sheet.range("G8","G8"),*Value=HdrYear2,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G8:G8").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("G8:G8").Font,*Bold="True"
              sheet.range("G8:i8").Merge
              setprop sheet.range("g9","g9"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g10","g10"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g9:I10").Font,*Bold="True"
              setprop sheet.range("g9:I10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("H9","H9"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H9","H9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("H9","H9"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H10","H10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("H10","H10"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("I9","I9"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("I9:I10").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("I9","I9"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("I10","I10"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.
.year 3
              setprop sheet.range("k8","k8"),*Value=HdrYear3,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k8:k8").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("k8:k8").Font,*Bold="True"
              sheet.range("k8:m8").Merge
              setprop sheet.range("k9","k9"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k10","k10"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k9:M10").Font,*Bold="True"
              setprop sheet.range("k9:M10").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("l9","l9"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l9","l9").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("l9","l9"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l10","l10"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l10","l10"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("m9","m9"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("m9:m10").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("m9","m9"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("m10","m10"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.
.year 4
              setprop sheet.range("b24","b24"),*Value=HDRYear4,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b24:b24").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("b24:b24").Font,*Bold="True"
              sheet.range("b24:E24").Merge
              
              setprop sheet.range("B27","B27"),*Value="1st Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b27:b27").Font,*Bold="True"
              sheet.range("B27:B28").Merge
              setprop sheet.range("B29","B29"),*Value="2nd Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b29:b29").Font,*Bold="True"
              sheet.range("B29:B30").Merge
              setprop sheet.range("B31","B31"),*Value="3rd Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b31:b31").Font,*Bold="True"
              sheet.range("B31:B32").Merge
              setprop sheet.range("B33","B33"),*Value="4th Quarter",*HorizontalAlignment=xlLeft
              setprop sheet.range("b33:b33").Font,*Bold="True"
              sheet.range("B33:B34").Merge
              setprop sheet.range("B35","B35"),*Value="Totals",*HorizontalAlignment=xlLeft
              setprop sheet.range("b35:b35").Font,*Bold="True"
              sheet.range("B35:B36").Merge
              setprop sheet.range("c25","c25"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c26","c26"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("c25:E26").Font,*Bold="True"
              setprop sheet.range("c25:E26").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("d25","d25"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d25","d25").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("d25","d25"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d26","d26"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d26","d26"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("e25","e25"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("e25:e26").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("e25","e25"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("e26","e26"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.year 5
              setprop sheet.range("G24","G24"),*Value=HdrYear5,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("G24:G24").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("G24:G24").Font,*Bold="True"
              sheet.range("G24:i24").Merge

              setprop sheet.range("g25","g25"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g26","g26"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g25:I26").Font,*Bold="True"
              setprop sheet.range("G25:I26").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("h25","h25"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h25","h25").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("h25","h25"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h26","h26"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h26","h26"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("i25","i25"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("i25:i26").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("i25","i25"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("i26","i26"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.year 6     
              setprop sheet.range("k24","k24"),*Value=HdrYear6,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k24:k24").Font,*Name="Times New Roman", *Size=10
              setprop sheet.range("k24:k24").Font,*Bold="True"
              sheet.range("k24:m24").Merge
              
              setprop sheet.range("k25","k25"),*Value="Total",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k26","k26"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k25:M26").Font,*Bold="True"
              setprop sheet.range("k25:M26").Font,*Name="Times New Roman", *Size=9
              setprop sheet.range("l25","l25"),*Value="Exch. Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l25","l25").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("l25","l25"),*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l26","l26"),*Value="Rent Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l26","l26"),*HorizontalAlignment=xlAlignCenter
                              if              (LIncTYPE = "i")             .INVOICE BASIS
                              setprop sheet.range("m25","m25"),*Value="Invoiced",*HorizontalAlignment=xlAlignCenter
                    Sheet.Range("m25:m26").Merge
                    elseif              (LIncTYPE = "C")             .cash basis
                    setprop sheet.range("m25","m25"),*Value="Checks",*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("m26","m26"),*Value="Paid",*HorizontalAlignment=xlAlignCenter
                    endif
.borders
.              //LineStyle 1 = Continuous
.              //LineStyle 4 = Dash Dot
.              //Object Viewer Help tells us that xlInsideHorizontal has a value of "12" Decimal
.              //Object Viewer Help tells us that xlInsideVertical has a value of "11" Decimal
.              //Object Viewer Help tells us that xlEdgeright has a value of "10" Decimal
               sheet.range("c9:e10").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("g9:i10").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("k9:m10").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("c25:e26").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("g25:i26").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("k25:m26").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("b11:e20").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("g11:i20").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("k11:m20").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("b27:e36").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("g27:i36").BorderAround using *LineStyle=1,*Weight=3
               sheet.range("k27:m36").BorderAround using *LineStyle=1,*Weight=3
               setprop  sheet.Range("d9:d20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("b10:e20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("c9:e9").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("b10:e20").Borders(11),*LineStyle=1,*Weight=3

               setprop  sheet.Range("h9:h20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g10:i20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g9:i9").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g10:i20").Borders(11),*LineStyle=1,*Weight=3

               setprop  sheet.Range("l9:l20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k10:m20").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k9:m9").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k10:m20").Borders(11),*LineStyle=1,*Weight=3

               setprop  sheet.Range("d25:d36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("b26:e36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("c25:e25").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("b26:e36").Borders(11),*LineStyle=1,*Weight=3

               setprop  sheet.Range("h25:h36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g26:i36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g25:i25").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("g26:i36").Borders(11),*LineStyle=1,*Weight=3

               setprop  sheet.Range("l25:l36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k26:m36").Borders(12),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k25:m25").Borders(11),*LineStyle=1,*Weight=3
               setprop  sheet.Range("k26:m36").Borders(11),*LineStyle=1,*Weight=3
              endif
...........
.data
.order
              If              (LIncRep1 = "M")   .Monthly report
              move            C1,Volptr
              MOve            "11",Row1
              move            "12",row2
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Loop
.              Move            VolArrayPrev(1,VolPtr),OqtyPrev
.              Move            VolArrayPrev(2,VolPtr),RqtyPrev
.              Move            VolArrayPrev(3,VolPtr),Eqtyprev
              Move            VolArrayPrev(1,VolPtr),RqtyPrev
              Move            VolArrayPrev(2,VolPtr),Eqtyprev
          MOve      RqtyPrev,OqtyPrev
          Add       EqtyPrev,OqtyPrev
.              Move            VolArrayCur(1,VolPtr),OqtyCur
.              Move            VolArrayCur(2,VolPtr),Rqtycur
.              Move            VolArrayCur(3,VolPtr),EQtyCur
              Move            VolArrayCur(1,VolPtr),Rqtycur
              Move            VolArrayCur(2,VolPtr),EQtyCur
          MOve      RqtyCur,OqtyCur
          Add       EqtyCur,OqtyCur
              pack            Exrange1 from "D",DimRow1
              pack            Exrange2 from "D",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
          MOve      RqtyCur,OqtyCur
          Add       EqtyCur,OqtyCur
              
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

              Elseif          (LIncRep1 = "Q")   .Quarterly report

              move            C1,Volptr
              MOve            "11",Row1
              move            "12",row2
              MOve            "27",Row3
              move            "28",row4
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Move            Row3,DimRow3
              call            Trim using DimRow3
              Move            Row4,DimRow4
              call            Trim using DimRow4
              Loop
              Move            VolArrayYear1(1,VolPtr),RqtyPrev
              Move            VolArrayYear1(2,VolPtr),Eqtyprev
          MOve      RqtyPRev,OqtyPrev
          Add       EqtyPrev,OqtyPrev
              pack            Exrange1 from "D",DimRow1
              pack            Exrange2 from "D",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.
              Move            VolArrayYear2(1,VolPtr),RqtyPrev
              Move            VolArrayYear2(2,VolPtr),Eqtyprev
          MOve      RqtyPRev,OqtyPrev
          Add       EqtyPrev,OqtyPrev
              pack            Exrange1 from "H",DimRow1
              pack            Exrange2 from "H",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.
              Move            VolArrayYear3(1,VolPtr),RqtyPrev
              Move            VolArrayYear3(2,VolPtr),Eqtyprev
          MOve      RqtyPRev,OqtyPrev
          Add       EqtyPrev,OqtyPrev
              pack            Exrange1 from "L",DimRow1
              pack            Exrange2 from "L",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.
              Move            VolArrayYear4(1,VolPtr),RqtyPrev
              Move            VolArrayYear4(2,VolPtr),Eqtyprev
              pack            Exrange1 from "D",DimRow3
              pack            Exrange2 from "D",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.
              Move            VolArrayYear5(1,VolPtr),RqtyPrev
              Move            VolArrayYear5(2,VolPtr),Eqtyprev
              pack            Exrange1 from "H",DimRow3
              pack            Exrange2 from "H",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.
              Move            VolArrayYear6(1,VolPtr),RqtyPrev
              Move            VolArrayYear6(2,VolPtr),Eqtyprev
              pack            Exrange1 from "L",DimRow3
              pack            Exrange2 from "L",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=EqtyPrev,*NumberFormat="##,####0"
              setprop sheet.range(ExRange1,ExRange1).Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range(ExRange2,ExRange2),*Value=RqtyPrev,*NumberFormat="##,####0"
.

              add             c2,row1
              add             c2,Row2
              add             c2,Row3
              add             c2,Row4
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Move            Row3,DimRow3
              call            Trim using DimRow3
              Move            Row4,DimRow4
              call            Trim using DimRow4
              add             c1 to Volptr
              until           (volPtr=5)
              repeat

              endif
.Invoice
              If          (LIncRep1 = "M")   .Monthly report
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
.
              Elseif          (LIncRep1 = "Q")   .Quarterly report
.
              move            C1,Invptr
              MOve            "11",Row1
              move            "12",row2
              MOve            "27",Row3
              move            "28",row4
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Move            Row3,DimRow3
              call            Trim using DimRow3
              Move            Row4,DimRow4
              call            Trim using DimRow4
              Loop

.year1
              Move            INVArrayyear1(1,INVPtr),APPrev
              pack            Exrange1 from "E",DimRow1
              pack            Exrange2 from "E",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
.year2
              Move            INVArrayyear2(1,INVPtr),APPrev
              pack            Exrange1 from "I",DimRow1
              pack            Exrange2 from "I",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
.year3
              Move            INVArrayyear3(1,INVPtr),APPrev
              pack            Exrange1 from "M",DimRow1
              pack            Exrange2 from "M",DimRow2
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
.year4
              Move            INVArrayyear4(1,INVPtr),APPrev
              pack            Exrange1 from "E",DimRow3
              pack            Exrange2 from "E",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
.year5
              Move            INVArrayyear5(1,INVPtr),APPrev
              pack            Exrange1 from "I",DimRow3
              pack            Exrange2 from "I",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
.year6
              Move            INVArrayyear6(1,INVPtr),APPrev
              pack            Exrange1 from "M",DimRow3
              pack            Exrange2 from "M",DimRow4
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value=Apprev,*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
 
              add             c2,row1
              add             c2,Row2
              add             c2,Row3
              add             c2,Row4
              Move            Row1,DimRow1
              call            Trim using DimRow1
              Move            Row2,DimRow2
              call            Trim using DimRow2
              Move            Row3,DimRow3
              call            Trim using DimRow3
              Move            Row4,DimRow4
              call            Trim using DimRow4
              add             c1 to Invptr
              until           (InvPtr=5)
              repeat
              endif
.TOTALS
              If              (LIncRep1 = "M")   .Monthly report
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
              PACK            STR55,"List Income as of ",DATEOFJOB
              setprop sheet.range("B39","B39"),*Value=str55,*HorizontalAlignment=xlLeft
              Else
              PACK            STR55,"Open List Income as of ",DATEOFJOB
              setprop sheet.range("B39","B39"),*Value=str55,*HorizontalAlignment=xlLeft
              setprop sheet.range("G39","G39"),*Value=APOpen,*NumberFormat="$##,####0.00_)"
              endif
              pack            str55,CopyrightS," 2005-2016 Names in the News"
.              pack            str55,Copyright," 2005-2008 Names in the News"
.START PATCH 1.04 REPLACED LOGIC
.              pack            str55,Copyright," 2005-2007 Names in the News"
.                   if (HOLDEXCL = "P")
..START PATCH 1.06 REPLACED LOGIC
..                            pack      str55,Copyright," 2007 Pacific Lists"
.                             pack      str55,Copyright," 2007-2008 Pacific Lists, A Division of Names in the News"
.                   else
.                             pack      str55,Copyright," 2005-2008 Names in the News"
...                           pack      str55,Copyright," 2005-2007 Names in the News"
...END PATCH 1.06 REPLACED LOGIC
.                   endif
.END PATCH 1.04 REPLACED LOGIC
              setprop sheet.range("B40","B40"),*Value=Str55,*HorizontalAlignment=xlLeft


              clock timestamp,timestamp
              move            timestamp,endtime
.              setprop sheet.range("B38","B38"),*Value=starttime,*HorizontalAlignment=xlLeft
.              setprop sheet.range("c38","c38"),*Value=endtime,*HorizontalAlignment=xlLeft
              subtract            starttime from endtime
.              setprop sheet.range("d38","d38"),*Value=endtime,*HorizontalAlignment=xlLeft

              Elseif          (LIncRep1 = "Q")   .Quarterly report
.year one volume              
              setprop sheet.range("c11","c11"),*Value="=Sum(d11:d12)",*NumberFormat="##,####0"
              sheet.range("c11","c12").Merge
              setprop sheet.range("c13","c13"),*Value="=Sum(d13:d14)",*NumberFormat="##,####0"
              sheet.range("c13","c14").Merge
              setprop sheet.range("c15","c15"),*Value="=Sum(d15:d16)",*NumberFormat="##,####0"
              sheet.range("c15","c16").Merge
              setprop sheet.range("c17","c17"),*Value="=Sum(d17:d18)",*NumberFormat="##,####0"
              sheet.range("c17","c18").Merge
.year two volume              
              setprop sheet.range("g11","g11"),*Value="=Sum(h11:h12)",*NumberFormat="##,####0"
              sheet.range("g11","g12").Merge
              setprop sheet.range("g13","g13"),*Value="=Sum(h13:h14)",*NumberFormat="##,####0"
              sheet.range("g13","g14").Merge
              setprop sheet.range("g15","g15"),*Value="=Sum(h15:h16)",*NumberFormat="##,####0"
              sheet.range("g15","g16").Merge
              setprop sheet.range("g17","g17"),*Value="=Sum(h17:h18)",*NumberFormat="##,####0"
              sheet.range("g17","g18").Merge
.year three volume              
              setprop sheet.range("k11","k11"),*Value="=Sum(l11:l12)",*NumberFormat="##,####0"
              Sheet.Range("k11","k12").Merge
              setprop sheet.range("k13","k13"),*Value="=Sum(l13:l14)",*NumberFormat="##,####0"
              Sheet.Range("k13","k14").Merge
              setprop sheet.range("k15","k15"),*Value="=Sum(l15:l16)",*NumberFormat="##,####0"
              Sheet.Range("k15","k16").Merge
              setprop sheet.range("k17","k17"),*Value="=Sum(l17:l18)",*NumberFormat="##,####0"
              Sheet.Range("k17","k18").Merge
.year four volume              
              setprop sheet.range("c27","c27"),*Value="=Sum(d27:d28)",*NumberFormat="##,####0"
              sheet.range("c27","c28").Merge
              setprop sheet.range("c29","c29"),*Value="=Sum(d29:d30)",*NumberFormat="##,####0"
              sheet.range("c29","c30").Merge
              setprop sheet.range("c31","c31"),*Value="=Sum(d31:d32)",*NumberFormat="##,####0"
              sheet.range("c31","c32").Merge
              setprop sheet.range("c33","c33"),*Value="=Sum(d33:d34)",*NumberFormat="##,####0"
              sheet.range("c33","c34").Merge
.year five volume              
              setprop sheet.range("g27","g27"),*Value="=Sum(h27:h28)",*NumberFormat="##,####0"
              sheet.range("g27","g28").Merge
              setprop sheet.range("g29","g29"),*Value="=Sum(h29:h30)",*NumberFormat="##,####0"
              sheet.range("g29","g30").Merge
              setprop sheet.range("g31","g31"),*Value="=Sum(h31:h32)",*NumberFormat="##,####0"
              sheet.range("g31","g32").Merge
              setprop sheet.range("g33","g33"),*Value="=Sum(h33:h34)",*NumberFormat="##,####0"
              sheet.range("g33","g34").Merge
.year six volume              
              setprop sheet.range("k27","k27"),*Value="=Sum(l27:l28)",*NumberFormat="##,####0"
              Sheet.Range("k27","k28").Merge
              setprop sheet.range("k29","k29"),*Value="=Sum(l29:l30)",*NumberFormat="##,####0"
              Sheet.Range("k29","k30").Merge
              setprop sheet.range("k31","k31"),*Value="=Sum(l31:l32)",*NumberFormat="##,####0"
              Sheet.Range("k31","k32").Merge
              setprop sheet.range("k33","k33"),*Value="=Sum(l33:l34)",*NumberFormat="##,####0"
              Sheet.Range("k33","k34").Merge

              pack            Exrange1 from "C19"
              pack            Exrange2 from "c20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(c11:c18)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "g19"
              pack            Exrange2 from "g20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(g11:g18)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "k19"
              pack            Exrange2 from "k20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(k11:k18)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "c35"
              pack            Exrange2 from "c36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(c27:c34)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "g35"
              pack            Exrange2 from "g36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(g27:g34)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "k35"
              pack            Exrange2 from "k36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(k27:k34)",*NumberFormat="##,####0"
              sheet.range(EXRange3).Merge
              
              setprop sheet.range("D19","D19"),*Value="=+D11+D13+D15+D17",*NumberFormat="##,####0"
              setprop sheet.range("D19","D19").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("h19","h19"),*Value="=+h11+h13+h15+h17",*NumberFormat="##,####0"
              setprop sheet.range("h19","h19").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("l19","l19"),*Value="=+l11+l13+l15+l17",*NumberFormat="##,####0"
              setprop sheet.range("l19","l19").Interior, *ColorIndex=6              .6= yellow

              setprop sheet.range("D20","D20"),*Value="=+D12+D14+D16+D18",*NumberFormat="##,####0"
              setprop sheet.range("h20","h20"),*Value="=+h12+h14+h16+h18",*NumberFormat="##,####0"
              setprop sheet.range("l20","l20"),*Value="=+l12+l14+l16+l18",*NumberFormat="##,####0"

              setprop sheet.range("D35","D35"),*Value="=+D27+D29+D31+D33",*NumberFormat="##,####0"
              setprop sheet.range("D35","D35").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("h35","h35"),*Value="=+h27+h29+h31+h33",*NumberFormat="##,####0"
              setprop sheet.range("h35","h35").Interior, *ColorIndex=6              .6= yellow
              setprop sheet.range("l35","l35"),*Value="=+l27+l29+l31+l33",*NumberFormat="##,####0"
              setprop sheet.range("l35","l35").Interior, *ColorIndex=6              .6= yellow

              setprop sheet.range("D36","D36"),*Value="=+D28+D30+D32+D34",*NumberFormat="##,####0"
              setprop sheet.range("h36","h36"),*Value="=+h28+h30+h32+h34",*NumberFormat="##,####0"
              setprop sheet.range("l36","l36"),*Value="=+l28+l30+l32+l34",*NumberFormat="##,####0"

          pack            Exrange1 from "e19"
              pack            Exrange2 from "e20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(e11:e18)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "i19"
              pack            Exrange2 from "i20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(i11:i18)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "m19"
              pack            Exrange2 from "m20"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(m11:m18)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "e35"
              pack            Exrange2 from "e36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(e27:e34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "i35"
              pack            Exrange2 from "i36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(i27:i34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge

              pack            Exrange1 from "m35"
              pack            Exrange2 from "m36"
              pack            Exrange3,Exrange1,":",Exrange2
              setprop sheet.range(ExRange1,ExRange1),*Value="=Sum(m27:m34)",*NumberFormat="$##,####0.00_)"
              sheet.range(EXRange3).Merge
             

                              If         (LIncTYPE = "I")             .Invoice date basis
                              PACK            STR55,"Income to List as of ",DATEOFJOB
                    setprop sheet.range("B38","B38"),*Value=str55,*HorizontalAlignment=xlLeft
                    Else
                              PACK            STR55,"Open List Income as of ",DATEOFJOB
                              setprop sheet.range("B38","B38"),*Value=str55,*HorizontalAlignment=xlLeft
                              setprop sheet.range("G38","G38"),*Value=APOpen,*NumberFormat="$##,####0.00_)"
                              endif
              pack            str55,Copyrights," 2005-2016 Names in the News"
.              pack            str55,Copyright," 2005-2008 Names in the News"
..START PATCH 1.04 REPLACED LOGIC
..             pack            str55,Copyright," 2005-2007 Names in the News"
.                   if (HOLDEXCL = "P")
..START PATCH 1.06 REPLACED LOGIC
.                             pack      str55,Copyright," 2007-2008 Pacific Lists"
..                            pack      str55,Copyright," 2007 Pacific Lists"
.                   else
.                             pack      str55,Copyright," 2005-2008 Names in the News"
..                            pack      str55,Copyright," 2005-2007 Names in the News"
..END PATCH 1.06 REPLACED LOGIC
.                   endif
.END PATCH 1.04 REPLACED LOGIC
              setprop sheet.range("B39","B39"),*Value=Str55,*HorizontalAlignment=xlLeft
              endif
..............................................................................................................
.Cleanup
              if          (LIncRep1 = "M")   .Monthly report
              setprop sheet.range("j8:m8").Font,*Name="Times New Roman", *Size=10
              if              (LIncREP2 <> yes)              .no variance delete that column
              Sheet.Range("L1:L37").Delete Using xlShiftToLeft                 
.              Sheet.Range("L1:L65536").Delete  *Shift=xlShiftToLeft                 .my syntax is wrong it ignores shift to left
.                                                                                  , by maxing out the row number it works ?
              endif
              if              (ProjFlag <> yes)              .no projection delete that column
              Sheet.Range("J1:J37").Delete  Using xlShiftToLeft
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
              Sheet.Range("a7:z7").Delete Using xlShiftUp                 
.a little cheating cleanup :)                 

              Elseif          (LIncRep1 = "Q")   .Quarterly report
              Sheet.Range("a22:z22").Delete Using xlShiftUp                 
          Sheet.Range("a7:z7").Delete Using xlShiftUp                 
              endif
              Reset           VolHdrCur
                    if (AutoFlag = No)
                              call getuser
                              call trim using nuseuser
                              clear taskname
                              If          (LIncRep1 = "Q")   .Quarterly report
                              pack taskname with "Report requested by ",nuseuser,B1,"on ",DateRan
                              setprop sheet.range("J36","J36"),*Value=taskname,*HorizontalAlignment=xlLeft
                              setprop sheet.range("j36:j36").Font,*Name="Times New Roman", *Size=9
                              Else
                              pack taskname with "Report requested by ",nuseuser,B1,"on ",DateRan
                              setprop sheet.range("J38","J38"),*Value=taskname,*HorizontalAlignment=xlLeft
                              setprop sheet.range("j38:j38").Font,*Name="Times New Roman", *Size=9
                              endif
          endif
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
.SendMail
.make sure file closed etc
.         Pack       MailSubjct,"LOINC - ",LincList
.         Move      "CReques@nincal.com",MailFrom
.         Move      LincRecipient,MailTo
.         Move      Taskname,MailBody
.         MOve      Taskname,MailAttach
.         call      SendMail

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
.begin patch 1.08
.SendMail
.make sure file closed etc
.1st do we do anything - valid email?
.         call      debug
          Clear     MailTO
          Clear     MailCC
          Clear     MailBCC
          call      debug
...testing
.          move      "NINS1.nincal.com",MailServer
.          move      "Xnincal99",mailpass
.          move      "creques",MailUser
.          move      "Y",Mailtrace
...testing
.begin patch 1.16
.begin patch xxx
.          if        (AutoFlag=yes & LincRecipient <> "SusanAnstrand@nincal.com")
.          if        (AutoFlag=yes & LincRecipient <> "PiaPayne@nincal.com")
.          Move      "PiaPayne@nincal.com",Mailcc
..turned off 12/02/2011
..          move      "SusanAnstrand@nincal.com",Mailcc
..end patch xxx
.          endif
.end patch 1.16
.DH 05 jan 10
          call      Trim using LincRecipient
          if        (LincRecipient = "")
          return
          endif
          scan      "@",LincRecipient
..begin patch 1.10 IF no one set to default
          if        not equal
          Clear     LincRecipient
..DH 12/1/09
..          move      "SKelly@nincal.com",LincRecipient
..DH 12/1/09
          Move      "Creques@nincal.com",LincRecipient                        .DH Failing here?  02/07/2012
          Else
          reset     LincRecipient
          endif
.DH 05 jan 10
.         return    if over
.end patch 1.10 let's do some more checking
.begin patch 1.09 let's do some more checking
.          call      Trim using LincRecipientpdf995
.          if        (LincRecipient = "")
.          return
.          endif
.end patch 1.09 let's do some more checking
.begin patch 1.12
.          scan      "Kelly",LincRecipient
.          if        not equal                    .Sherene wants a copy
.          move      "SKelly@nincal.com",MailCC
.          endif
.          reset     LincRecipient
.end patch 1.12
.1st we don't know if it was run under office 03 0r 07
.begin patch
          if        (Linclist = "003942")         .NPCA
          move      "SusanAnstrand@nincal.com",MailCC
           else
           Clear      Mailcc
          endif
          pack      MailAttach from taskname,"*"
          
          FindFile  Mailattach,Name=Str55
          scan      ".xlsx",str55
          if        not equal
          pack      MailAttach from taskname,".xls"
          else
          pack      MailAttach from taskname,".xlsx"
          endif
          Move      C0,Trapcount
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck
.         
          Pack       MailSubjct,"List Income - ",LincList," - ",Olstname
          Move      "CReques@nincal.com",MailFrom
.          move      "dherric@nincal.com",mailbcc
          Pack      Mailto,LincRecipient
.          Move      LincRecipient,MailTo
          clear     Mailbody
          append    "To: ",Mailbody
          append    LincRecipient,Mailbody
          append    CRLF,Mailbody
          Append      MailAttach,MailBody
          reset     Mailbody
          call      SendMail
        Return
WaitForEnd
          TrapClr   IO
.check the error if file does not exist just get out
          add       C1,Trapcount
          pause     c5
          noreturn
          if        (trapcount > 240)   .20 min are you kidding me
          Pack       MailSubjct,"List Income - ",LincList
          Move      "CReques@nincal.com",MailFrom
          Move      LincRecipient,MailTo
          Move      "CReques@nincal.com",MailBCC
          append    MailAttach,MailBody
          append    CRLF,MailBOdy
          append    "I am sorry I could not send you the file",Mailbody
          reset     Mailbody
          Move      B1,Mailattach
          call      SendMail
          return
          endif
          
          goto      checkfile
.end patch 1.08
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
           Move     c0,n4
               Move ForceDay to N4
          if        (N4 <> c0)
               move n4 to LYEAR
               unpack LYEAR,str2,YY
          endif
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
Prev
               move begfiscCur to JULDAYS
               call cvtgreg                                     .paramenters for previous year in Monthly
               pack str4 with CC,YY                         .parameters for previous year 5 in Quarterly
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
           Move     c0 to DateToOld
               if   (LincYear <> 0)
               move Lincyear to str8
               unpack         str8 into str2,yy,mm,dd
.No do not use fiscal unless MM is not there and more checking for the day
               move   c0,n2
               move   mm,N2
               if     (N2 < c1 | N2 > "12")
               move Lmonth to MM
               endif
.               if   (dd = "00" | dd = " 0")     
               if   (dd = "00" | dd = " 0" | dd = " " | dd = " " | dd = "")     
               move "01",DD
               endif     
               call cvtjul
               move juldays,DateToOld
               endif
.               
               IF              (LincREp1 = "Q")          .quarterly 6 year rep
...................................................
           Move     c0 to DateToOld
               if   (LincYear <> 0)
               move Lincyear to str8
               unpack         str8 into str2,yy,mm,dd
               move Lmonth to MM
               if   (dd = "00" | dd = " 0")     
               move "01",DD
               endif     
               call cvtjul
               move juldays,DateToOld
               endif

               move begfiscPrev to JULDAYS               .parameters for year4 of six year rep
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul

               move juldays to BegFiscPrev3
               move endfiscPrev to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays,EndFiscPrev3
......................
               move begfiscPrev3 to JULDAYS               .parameters for year3 of six year rep
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul

               move juldays to BegFiscPrev4
               move endfiscPrev3 to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays,EndFiscPrev4
......................
               move begfiscPrev4 to JULDAYS               .parameters for year2 of six year rep
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul

               move juldays to BegFiscPrev5
               move endfiscPrev4 to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays,EndFiscPrev5
......................
               move begfiscPrev5 to JULDAYS               .parameters for year2 of six year rep
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul

               move juldays to BegFiscPrev6
               move endfiscPrev5 to JULDAYS
               call cvtgreg
               pack str4 with CC,YY
               move str4 to n4
               sub c1,n4
               unpack n4,str2,n2
               move n2 to YY
               rep zfill,YY
               call cvtjul
               move juldays,EndFiscPrev6
              endif
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
...........................................................................................
NlOINC0007TabClick
        IF (N1 = C1)
                Deactivate NLoinca
        else (N1 = C2 )
                Deactivate NLoincb
        Endif
        return


NlOINC0007TabChange
        IF (N1 = C1)
                move    C1,TabNum
                Activate NLoinca
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        else (N1 = C2)
                move    C2,TabNum
                Activate NLoincb
           setprop  NLOINC0007bButtonSave,Visible=0
           setprop  NLOINC0007bButtonNew,Visible=1
           setprop  NLOINC0007bButtonSave,Enabled=0
           setprop  NLOINC0007bButtonNew,Enabled=1
                
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
LoadNloinc0007view
.
              SetItem         NLOINC0007EditText001,0,LincList
              SetItem         NLOINC0007StatText002,0,Olstname
              NLOINC0007aListView001.GetItemText Giving LincAuto using In9,6
              NLOINC0007aListView001.GetItemText Giving str10 using In9,2
              IF             (str10 = "Monthly")
              Move           "M" to Lincrep1
              else
              Move           "Q" to Lincrep1
              endif
.                                                                           
                    NLOINC0007aListView001.GetItemText Giving str10 using In9,3
                    if             (str10 = "Mail date")
                    move           "M",Lincdateby
                    else
                    move           "O",Lincdateby
                    endif
                    NLOINC0007aListView001.GetItemText Giving str7 using In9,4
                    if             (str7 = "Cash")
                    move           "C",LincType
                    Else
                    move           "I",LincType
                    endif
                    NLOINC0007aListView001.GetItemText Giving str2 using In9,7
                    move           str2 to Lmonth
              NLOINC0007aListView001.GetItemText Giving str8 using In9,8
              Move  Str8 to LincYear
              NLOINC0007aListView001.GetItemText Giving str6 using In9,9
              Move  Str6 to LincREcid
              NLOINC0007aListView001.GetItemText Giving LincRecipient using In9,10
              NLOINC0007aListView001.GetItemText Giving LincRep2 using In9,11

 
          Move            Lmonth,n2
              setitem         NLOINC0007ComboBox001,0,n2
              if              (LINCDATEBY = "M")
              setitem         NLOINC0007ComboBox002,0,c2
.              Setitem         NLOINC0007DataList001,0,c2
              ElseIf          (LINCDATEBY = "O")
              setitem         NLOINC0007ComboBox002,0,c3
.              Setitem         NLOINC0007DataList001,0,c3
              endif
              if              (LIncTYPE = "C")
              Setitem         NLOINC0007Combobox004,0,c2
              ElseIf          (LIncTYPE = "I")
              Setitem         NLOINC0007Combobox004,0,c3
              endif
              if              (LIncREP1 = "M")
              Setitem         NLOINC0007Combobox003,0,c2
              ElseIf          (LIncREP1 = "Q")
              Setitem         NLOINC0007Combobox003,0,c3
              endif
              if              (LIncREP2 = "N" or LIncREP2 = " ")
              Setitem         NLOINC0007Combobox007,0,c2
              ElseIf          (LIncREP2 = "Y")
              Setitem         NLOINC0007Combobox007,0,c3
              endif
.NOte still need to create and populate projection maintenance screens
              pack            str7,CC,YY,"001"
              Pack            str4,CC,YY
              Packkey         LSTIFLD,LIncList,str7
              call            Lstikey
              if              Not over                     .we got a live one
              Setitem         NLOINC0007Combobox005,0,c3
              Else
              Setitem         NLOINC0007Combobox005,0,c2
              endif
              if              (LIncAuto = "Y" or LIncAuto = B1)
              Setitem         NLOINC0007Combobox006,0,c1
              ElseIf          (LIncAuto = "N")
              Setitem         NLOINC0007Combobox006,0,c2
              endif
              Clear           Str8
              Move            LincYear,str8
              Setitem         NLOINC0007EditText002,0,str8
              Setitem         NLOINC0007EditText003,0,LIncRECIPIENT
          Call      ClearNLOINC0007b
          
          
.Create list view columns and populate, put headers into stattextfields         
.Build Month Labels - Monthly report
.lets offset So that Month 1 of array is first month of fiscal or calendar year
              Clear           Str36
              MOve            LMonth,n3
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
          setitem   NLOINC0007bStatText001,0,Mon1Label
          setitem   NLOINC0007bStatText002,0,Mon2Label
          setitem   NLOINC0007bStatText003,0,Mon3Label
          setitem   NLOINC0007bStatText004,0,Mon4Label
          setitem   NLOINC0007bStatText005,0,Mon5Label
          setitem   NLOINC0007bStatText006,0,Mon6Label
          setitem   NLOINC0007bStatText007,0,Mon7Label
          setitem   NLOINC0007bStatText008,0,Mon8Label
          setitem   NLOINC0007bStatText009,0,Mon9Label
          setitem   NLOINC0007bStatText010,0,Mon10Label
          setitem   NLOINC0007bStatText011,0,Mon11Label
          setitem   NLOINC0007bStatText012,0,Mon12Label
.List view first create columns
              NLOINC0007bListView001.InsertColumn using Mon1Label,65,1
              NLOINC0007bListView001.InsertColumn using Mon2Label,65,2
              NLOINC0007bListView001.InsertColumn using Mon3Label,65,3
              NLOINC0007bListView001.InsertColumn using Mon4Label,65,4
              NLOINC0007bListView001.InsertColumn using Mon5Label,65,5
              NLOINC0007bListView001.InsertColumn using Mon6Label,65,6
              NLOINC0007bListView001.InsertColumn using Mon7Label,65,7
              NLOINC0007bListView001.InsertColumn using Mon8Label,65,8
              NLOINC0007bListView001.InsertColumn using Mon9Label,65,9
              NLOINC0007bListView001.InsertColumn using Mon10Label,65,10
              NLOINC0007bListView001.InsertColumn using Mon11Label,65,11
              NLOINC0007bListView001.InsertColumn using Mon12Label,65,12
              NLOINC0007bListView001.InsertColumn using "YEAR",40,13
              NLOINC0007bListView001.InsertColumn using "Proj ##",30,14
              NLOINC0007bListView001.InsertColumn using "Date",40,15
.
          packkey   LSTIFLD,LincList
              pack            str7,CC,"00001"                       .start at 2000
              Pack            str4,CC,"00"
              Packkey         LSTIFLD,LIncList,str7
              call            Lstikey
          if        over
          Goto      ProjLoop
          Else
          Move      lstIm1,str9
              NLOINC0007bListView001.InsertItem giving IN9 using str9
          Move      lstIm2,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,1
          Move      lstIm3,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,2
          Move      lstIm4,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,3
          Move      lstIm5,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,4
          Move      lstIm6,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,5
          Move      lstIm7,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,6
          Move      lstIm8,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,7
          Move      lstIm9,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,8
          Move      lstIm10,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,9
          Move      lstIm11,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,10
          Move      lstIm12,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,11
              NLOINC0007bListView001.SetItemText using IN9,LstIYear,12
              NLOINC0007bListView001.SetItemText using IN9,LstIProj,13
              NLOINC0007bListView001.SetItemText using IN9,LstIDate,14
          endif
ProjLoop      loop
              call            LstiKS
              until           over
                              if              (LincList <> LSTIlist)        .same list ??
                              Break                                        .nope
                              endif
          Move      lstIm1,str9
              NLOINC0007bListView001.InsertItem giving IN9 using str9
          Move      lstIm2,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,1
          Move      lstIm3,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,2
          Move      lstIm4,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,3
          Move      lstIm5,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,4
          Move      lstIm6,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,5
          Move      lstIm7,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,6
          Move      lstIm8,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,7
          Move      lstIm9,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,8
          Move      lstIm10,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,9
          Move      lstIm11,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,10
          Move      lstIm12,str9
              NLOINC0007bListView001.SetItemText using IN9,str9,11
              NLOINC0007bListView001.SetItemText using IN9,LstIYear,12
              NLOINC0007bListView001.SetItemText using IN9,LstIProj,13
              NLOINC0007bListView001.SetItemText using IN9,LstIDate,14
              repeat
.         endif
          call      LoadNloinc0007bview
              return
.............................................................................................................
LoadNloinc0007bview
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,0
          setitem NLOINC0007bEditText001,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,1
          setitem NLOINC0007bEditText002,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,2
          setitem NLOINC0007bEditText003,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,3
          setitem NLOINC0007bEditText004,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,4
          setitem NLOINC0007bEditText005,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,5
          setitem NLOINC0007bEditText006,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,6
          setitem NLOINC0007bEditText007,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,7
          setitem NLOINC0007bEditText008,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,8
          setitem NLOINC0007bEditText009,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,9
          setitem NLOINC0007bEditText010,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,10
          setitem NLOINC0007bEditText011,0,str9
              NLOINC0007bListView001.GetItemText giving str9 Using IN9,11
          setitem NLOINC0007bEditText012,0,str9
              NLOINC0007bListView001.GetItemText giving str4 Using IN9,12
          Setitem NLOINC0007bEditText013,0,Str4
          return              
.............................................................................................................
ClearNloinc0007bview
          setitem NLOINC0007bEditText001,0,B1
          setitem NLOINC0007bEditText002,0,B1
          setitem NLOINC0007bEditText003,0,B1
          setitem NLOINC0007bEditText004,0,B1
          setitem NLOINC0007bEditText005,0,B1
          setitem NLOINC0007bEditText006,0,B1
          setitem NLOINC0007bEditText007,0,B1
          setitem NLOINC0007bEditText008,0,B1
          setitem NLOINC0007bEditText009,0,B1
          setitem NLOINC0007bEditText010,0,B1
          setitem NLOINC0007bEditText011,0,B1
          setitem NLOINC0007bEditText012,0,B1
          setitem NLOINC0007bEditText013,0,B1
          Setfocus NLOINC0007bEditText001
          return              

.............................................................................................................
.LoadRepList load list view showing all lists & associated reports
LoadRepList
.first create columns

              NLOINC0007aListView001.InsertColumn using "List",45,1
              NLOINC0007aListView001.InsertColumn using "Name",170,2
              NLOINC0007aListView001.InsertColumn using "Report",70,3
              NLOINC0007aListView001.InsertColumn using "Date Select",65,4
              NLOINC0007aListView001.InsertColumn using "Accrual",50,5
              NLOINC0007aListView001.InsertColumn using "Projections",70,6
              NLOINC0007aListView001.InsertColumn using "Auto",40,7
              NLOINC0007aListView001.InsertColumn using "Fiscal Month",0,8
              NLOINC0007aListView001.InsertColumn using "Start Year",0,9
              NLOINC0007aListView001.InsertColumn using "Record ID",0,10
              NLOINC0007aListView001.InsertColumn using "Email",100,11
              NLOINC0007aListView001.InsertColumn using "Variance",0,12
              NLOINC0007aListView001.SetColumnFormat using 4,1              .set DAYS column justify right
              Setprop NLOINC0007aListView001,Sortorder=3
.load data
                              loop
                              call           LIncks
                              Until over
                                             If Not over
                                             Packkey        Ndatfld,LIncList
                                             call           Ndatkey
                                             
                                             Call LoadListView
                                       endif
                              Repeat
            return
......................................................................................................................
LoadListView
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
                                             clear str4
                                             MOve Lincyear,str8
                                             NLOINC0007aListView001.SetItemText using IN9,str8,8
                                             MOve LincRecID,str6
                                             NLOINC0007aListView001.SetItemText using IN9,str6,9
                                             call Trim using LincRecipient
                                             NLOINC0007aListView001.SetItemText using IN9,LIncRECIPIENT,10
                                             NLOINC0007aListView001.SetItemText using IN9,LIncRep2,11
                                             

            return
...............................................................................
.verifyProj - verify Report info  and Save Record
VerifyRep
              GetItem         NLOINC0007EditText001,0,LincList
            Packkey        Ndatfld,LIncList
            call           Ndatkey
              SetItem         NLOINC0007StatText002,0,Olstname
          Clear     LIncDateBy
          Clear     LIncType
          Clear     LIncRep1
          Clear     LMonth
          Clear     LIncAuto
          sub       Lincyear,LincYear
          
              Getitem         NLOINC0007ComboBox001,0,n2
              Move  N2,Lmonth

              if    (n2 < 1 or N2 > 12)
                    alert   caution,"Start of Fiscal Year required!",result
                    setfocus  NLOINC0007ComboBox001
                    Goto      VerifyRepAbort
                    endif
              
              Getitem         NLOINC0007EditText002,0,str8
              Move  str8,Lincyear
              count N2,str8

              if    (n2 > 0 & N2 < 8)
                    alert   caution,"Starting year format CCYYMMDD",result
                    setfocus  NLOINC0007EditText002
                    Goto      VerifyRepAbort
                    endif
              if      (n2 > 0)
              unpack  str8 into str2,yy,mm,dd
                      if      (str2 <> "19" & str2 <> "20")
                      alert   caution,"Starting year format CCYYMMDD",result
                      setfocus  NLOINC0007EditText002
                      Goto      VerifyRepAbort
                      endif
                      move       mm,n2
                      if      (n2 < "1" | n2 > "12")
                      alert   caution,"Starting year format CCYYMMDD",result
                      setfocus  NLOINC0007EditText002
                      Goto      VerifyRepAbort
                      endif
                      move       dd,n2
                      if      (n2 < "1" | n2 > "31")
                      alert   caution,"Starting year format CCYYMMDD",result
                      setfocus  NLOINC0007EditText002
                      Goto      VerifyRepAbort
                      endif
           endif

              Getitem         NLOINC0007ComboBox002,0,n2
          if        (N2 <> 2 and N2 <> 3)
                    alert     caution,"Mail or Order Date Selection Required",result
.                             setfocus  NLOINC0007DataList001
                              setfocus  NLOINC0007ComboBox002
                    Goto      VerifyRepAbort
          Endif     
              if              (N2 = c2)
              MOve  "M",LINCDATEBY
              Elseif          (N2 = c3)
              Move  "O",LIncDateBy
              endif
          
              Getitem         NLOINC0007Combobox004,0,N2
            IF      (N2 <> 2 and N2 <> 3)
                    alert     caution,"Accrual Selection required",result
                    setfocus  NLOINC0007Combobox004
                    Goto      VerifyRepAbort
                    endif
              IF    (N2 = C2)
              MOVE  "C",LincType
              Elseif          (N2 = C3)
              Move  "I",LincType
              endif

              Getitem         NLOINC0007ComboBox003,0,n2
          If        (N2 = c2)
          Move      "M",LIncRep1
          Elseif    (N2 = c3)
          Move      "Q",LIncRep1
              Else
                    alert     caution,"Report Selection required",result
                    setfocus  NLOINC0007Combobox003
                    Goto      VerifyRepAbort
          endif

          Getitem   NLOINC0007Combobox006,0,N2
          If        (N2 = c2)
          Move      "N",LIncAuto
          Elseif    (N2 = c1)
          Move      "Y",LIncAuto
          Else
          Move      "Y",LIncAuto
          endif

          Getitem   NLOINC0007Combobox007,0,N2
          If        (N2 = c2)
          Move      "N",LIncRep2
          Elseif    (N2 = c3)
          Move      "Y",LIncRep2
          Else
          Move      "N",LIncRep2
          endif
            GetItem         NLOINC0007EditText003,0,LincRecipient
.begin patch xxx
           move       Inits,LincInits
.end patch xxx
           
          packkey   LIncFLD,LincList
          move      c1,n6
RepWrite  move      n6,LincREcID
          rep       Zfill in LincREcID
          packkey   LIncFLD1,LincREcID
          Move      C2,LincPath
          call      LincTst
          if        over
          Move      C1,LincPath
          call      LincWrt
          Else
          add       c1 to n6
          Goto      RepWrite
          endif
          Move      b1,AddMode
          Call      LoadListView
          Return
....................................................................................
.VerifyRepAbort verify failed reset buttons
VerifyRepAbort
          setprop   NLOINC0007aButtonSave,Enabled=1            .save
          setprop   NLOINC0007aButtonSave,Visible=1            .save
          Setprop   NLOINC0007aButtonAdd,Enabled=0            .add 
          setprop   NLOINC0007aButtonAdd,Visible=0            .add 
          setprop   NLOINC0007aButton005,Enabled=1            .quit 
          setprop   NLOINC0007aButton005,Visible=1            .quit 
          Move      b1,AddMode
          REturn
...............................................................................
.verifyProj - verify projections and Save Record
VerifyProj
          Getitem NLOINC0007bEditText013,0,str4
          Count     n2,str4
          if        (n2 <> 4)
                    alert   caution,"Date Must be in CCYY Format",result
                    setFocus NLOINC0007bEditText013
                    return
                    endif

                    Move      str4,LstIYear
          getitem NLOINC0007bEditText001,0,str9
          Move      Str9,lstIm1
              NLOINC0007bListView001.InsertItem giving IN9 using str9
          getitem NLOINC0007bEditText002,0,str9
          Move      Str9,lstIm2
              NLOINC0007bListView001.SetItemText using IN9,str9,1
          getitem NLOINC0007bEditText003,0,str9
          Move      Str9,lstIm3
              NLOINC0007bListView001.SetItemText using IN9,str9,2
          getitem NLOINC0007bEditText004,0,str9
          Move      Str9,lstIm4
              NLOINC0007bListView001.SetItemText using IN9,str9,3
          getitem NLOINC0007bEditText005,0,str9
          Move      Str9,lstIm5
              NLOINC0007bListView001.SetItemText using IN9,str9,4
          getitem NLOINC0007bEditText006,0,str9
          Move      Str9,lstIm6
              NLOINC0007bListView001.SetItemText using IN9,str9,5
          getitem NLOINC0007bEditText007,0,str9
          Move      Str9,lstIm7
              NLOINC0007bListView001.SetItemText using IN9,str9,6
          getitem NLOINC0007bEditText008,0,str9
          Move      Str9,lstIm8
              NLOINC0007bListView001.SetItemText using IN9,str9,7
          getitem NLOINC0007bEditText009,0,str9
          Move      Str9,lstIm9
              NLOINC0007bListView001.SetItemText using IN9,str9,8
          getitem NLOINC0007bEditText010,0,str9
          Move      Str9,lstIm10
              NLOINC0007bListView001.SetItemText using IN9,str9,9
          getitem NLOINC0007bEditText011,0,str9
          Move      Str9,lstIm11
              NLOINC0007bListView001.SetItemText using IN9,str9,10
          getitem NLOINC0007bEditText012,0,str9
          Move      Str9,lstIm12
              NLOINC0007bListView001.SetItemText using IN9,str9,11
              NLOINC0007bListView001.SetItemText using IN9,LstIYear,12
              clock           timestamp,timestamp
          Move      Timestamp,LstIdate              
              NLOINC0007bListView001.SetItemText using IN9,LstIDate,14
          Getitem NLOINC0007EditText001,0,str6
          move      Str6,LstIList
          move      c1 to N3
WriteProj Move      N3 to str3
          rep       Zfill in str3
              pack            str7,LstiYear,str3
              Move  str3 to LstIproj
              Packkey         LSTIFLD,LIncList,str7
              call            LstiTst
              if    OVer
              Call  LstIwrt
              Else
              add   c1 to N3
              goto  WriteProj
          endif

              NLOINC0007bListView001.SetItemText using IN9,LstIProj,13
          return
...............................................................................
.EOJ - End of JOB
EOJ
              winshow
.begin patch 1.02
          if        (Autoflag = Yes)                 
          Shutdown
          endif
.end patch 1.02
          Stop
...............................................................................
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3
.MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER - not an option with this program
              return
        return
SearchLoad2
.LIST
        unpack  Srchstr,str6,str1,str35
        Setitem      NLOINC0007StatText002,0,str35
        Setitem     NLOINC0007EditText001,0,str6
        return
SearchLoad3
.MAILER- not an option with this program
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.
...............................................................................
.check REPORT date
Chkrepdate
          move    No to dateokflag
          move  "          " to str10
          reset str10
          clear str10
          getitem NLOINC0007aEditText001,0,str10
          clear   mm
          clear   dd
          clear   str2
          clear   yy
          call    TRIM using str10
          count   N2,str10
          if (N2 = 10)
                    unpack  str10,MM,str1,DD,str1,STR2,YY
          elseif (N2 = 8)
                    unpack  str10,MM,DD,STR2,YY
          elseif (N2 <> 0)
                    alert   caution,"Date Must be in MMDDCCYY Format",result
                    setFocus NLOINC0007aEditText001
                     goto BadDate
          elseif (N2 = 0)                          .use today's date
.begin patch 1.01
          unpack    DATEofJOB with mm,slash,dd,slash,str2,yy
                    pack      str10,MM,SLASH,DD,SLASH,STR2,YY
          Goto      DateExit
.         return
.end patch 1.01
          endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
               setFocus NLOINC0007aEditText001
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus NLOINC0007aEditText001
                              goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                             setFocus NLOINC0007aEditText001
                               goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    str10,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   str10
        endif
.begin patch 1.01
DateExit        
.end patch 1.01
        setitem NLOINC0007aEditText001,0,str10
          MOve      Yes to DateokFLag
          setFocus  NLOINC0007aListView001
        setprop NLOINC0007aEditText001,Enabled=0                     .don't let them change during run
        return
BadDate
          move    No to dateokflag
              setprop   NLOINC0007aButton001,Enabled=1                     .reset report button
        return        
...............................................................................
GetUser
          move      c1 to nusepath
          clock       port to str3
          unpack    str3 into str2,str1
          pack      str3 from str1,str2
          MOVE      str3 TO NUSEFLD .removed FOR TESTING only
          REP       ZFILL IN NUSEFLD
          CALL      NUSEKEY
          goto      userng if over
          scan      "INVALID" in nuseuser
          goto      userng if equal
          return
userng
          clear     taskname
          append    "I'm sorry I've lost track of who you are,",taskname
          append    NewLine,taskname
          append    "Please leave the program and try again!",taskname
          reset     taskname
          alert     caution,taskname,result
          return
...............................................................................
.Clear for entry
ClearNLOINC0007

          setprop             NLOINC0007AButton004,Enabled=0            .disable Del button
          setprop             NLOINC0007AButton004,Visible=0            .
          setitem   NLOINC0007ComboBox001,0,c0
          setitem   NLOINC0007ComboBox002,0,c0
.              Setitem         NLOINC0007DataList001,0,c1
              Setitem         NLOINC0007Combobox004,0,c1
              Setitem         NLOINC0007Combobox003,0,c1
              Setitem         NLOINC0007Combobox005,0,c1
              Setitem         NLOINC0007Combobox006,0,c1
          Setitem   NLOINC0007EditText001,0,b1
          Setitem   NLOINC0007EditText002,0,b1
          Setitem   NLOINC0007aEditText001,0,b1
          setfocus  NLOINC0007EditText001

          REturn
...............................................................................
.Clear 
ClearNLOINC0007b

              Setitem         NLOINC0007bStatText001,0,b1
              Setitem         NLOINC0007bStatText002,0,B1
              Setitem         NLOINC0007bStatText003,0,B1
              Setitem         NLOINC0007bStatText004,0,B1
              Setitem         NLOINC0007bStatText005,0,B1
              Setitem         NLOINC0007bStatText006,0,B1
              Setitem         NLOINC0007bStatText007,0,B1
              Setitem         NLOINC0007bStatText008,0,B1
              Setitem         NLOINC0007bStatText009,0,B1
              Setitem         NLOINC0007bStatText010,0,B1
              Setitem         NLOINC0007bStatText011,0,B1
              Setitem         NLOINC0007bStatText012,0,B1
          Setitem   NLOINC0007bEditText001,0,b1
          Setitem   NLOINC0007bEditText002,0,b1
          Setitem   NLOINC0007bEditText003,0,b1
          Setitem   NLOINC0007bEditText004,0,b1
          Setitem   NLOINC0007bEditText005,0,b1
          Setitem   NLOINC0007bEditText006,0,b1
          Setitem   NLOINC0007bEditText007,0,b1
          Setitem   NLOINC0007bEditText008,0,b1
          Setitem   NLOINC0007bEditText009,0,b1
          Setitem   NLOINC0007bEditText010,0,b1
          Setitem   NLOINC0007bEditText011,0,b1
          Setitem   NLOINC0007bEditText012,0,b1
              NLOINC0007bListView001.DeleteAllContents Giving Result
          REturn
...............................................................................
DelReports
.routine is not yet called see the button :)
          SetItem NLOINC0007StatText008,0,"Deleting Report(s)"
.            SETPROP       NLOINC0007aListView001,ENABLED=0         .disable list view so user cant change
.2nd check and see if any selected
              move     SEQ,result
              loop


              move     result,IN9
              NLOINC0007aListView001.GetNextItem giving result using C2,IN9  // -1 is error code
              move  Result,RepLoop                          .may get destroyed save it
              NLOINC0007aListView001.EnsureVisible Using Result,1     
.              NLOINC0007aListView001.GetItemText giving str6 using result,0
.              Move str6,LincList
              NLOINC0007aListView001.GetItemText Giving Str6 using Result,9
              MOve  str6,LincRecID
              until (RepLoop = SEQ)

              NLOINC0007aListView001.SetItemState using Result,C0,c2
            Rep     Zfill,LincREcId
.delete from list view
.              NLOINC0007aListView001.DeleteItem using IN9
              NLOINC0007aListView001.DeleteItem using Result

            packkey LIncFLD1,LincREcID
          Move      C2,LincPath
          call      LincTst
          IF        not Over
          call      LincDel
            Else
                      alert   caution,"Record not be Deleted Inform IT!",result
            endif
              until (RepLoop = SEQ)
          Repeat

.            SETPROP       NLOINC0007aListView001,ENABLED=1         .disable list view so user cant change
          Return
XRESIZE
           NLOinc0007.Scale
           RETURN
.......................................................................................................
FileGo
        Goto   EOJ
.....................................................................
HelpGo
        setprop AboutMssg,visible=1
        return
............................................................................................................
PrintGo
           Chain    "NLoinc0009"
           return
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
.Following used only in order to load Search.plf
              include         ncmpio.inc
              include         nrtnio.inc
              include         searchio.inc      .contains logic for search.plf

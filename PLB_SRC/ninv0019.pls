...............................................................................
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
          INCLUDE             ninvdd.inc
          INclude   NInvAcddd.inc
          inCLUDE   oSLSPERN.INC
          inc                 hp.inc
          INCLUDE             NADJDD.inc
          include   Norddd.inc
          INCLUDE          PRTPAGEDD.INC

          include        compdd.inc
          include        cntdd.inc
          include   Nowndd.inc
         INC       CONSacct.inc
         include   nacddd.inc
         include   ndatdd.inc
         include   ndat3dd.inc
         include   nshpdd.inc
          include   Nmrgdd.inc
.begin patch 1.40
           include    Tinvdd.inc
form122    FOrm       12.2
.end patch 1.40
shipsw   dim      1
mrgsw    dim      1



. income by salesperson
.Note should subract Tinv $ from NIN INc  2014
Release   Init      "1.43"      DLH  .Emailt to DH, email to SA,SM june and Dec.
Reldate   Init      "2016 January 19"
.Release   Init      "1.42"      DLH  New Year
.Reldate   Init      "2016 January 5"
.Release   Init      "1.41"      DLH  New Year
.Reldate   Init      "2015 January 5"
.Release   Init      "1.40"      DLH  Added code for Tinv$, Note have no years prior to 2012
.Reldate   Init      "2014 August 15"
.Release   Init      "1.34"      DLH  New Year
.Reldate   Init      "2014 January 2"
.Release   Init      "1.33"      DLH  sunbelt PDF
.Reldate   Init      "2013 April 4"
.Release        Init           "1.32"      DLH  -- New Year
.Reldate   Init      "3 Jan 2013"
.Release        Init           "1.31"      DLH  -- New Year
.Reldate   Init      "3 Jan 2012"
.Release        Init           "1.3"      DLH  -- New Year
.Reldate   Init      "3 Jan 2011"
.Release        Init           "1.2"      DLH  -- USE DSPROG DATE IF CHAINED
.Reldate   Init      "28 mAY 2010"
.Release        Init           "1.1"      DLH  -- change to LR & NIN inc
.Reldate   Init      "24 February 2010"
.Release        Init           "1.02"      DLH  -- New Year
.Reldate   Init      "January 2010"
.Release        Init           "1.01"      DLH  -- New Year
.Reldate   Init      "January 2009"
.Release        Init           "1.00"      DLH  -- used ninv0005 as basis for new version
.Reldate   Init      "October 2008"
Results   IFILE      Keylen=8,FIX=37          .PHYSICAL OUTPUT FILE "lRdetMST.ISI|NINS1:502"
.file layout
resultkey           dim           8         xxCCYYMM      1-8
.xx = sales number
.TotINV        form          5                    9-13
.TotNIN         Form          9.2                 14-25
.TOTLR         Form          9.2                 26-37
.
. ..........................................................................
holdToday       dim         6
.
.
.
DATE     DIM       8
TIME     DIM       8
YR       FORM      2         POS 2-3    YEAR
MO       FORM      2         POS 5-6    MONTH
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
MINIT      DIM       2         POS 17-18  MINUTE
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
FORM5    FORM      5
.
. .ACCOUNTS PAYABLE WORK FIELDS
.
TOTNIN    FORM      9.2
FINNIN    FORM      9.2(3)         TOTAL NAMES INVOICED DURING YEAR

FORM9P   FORM      10
ACCPAY   FORM      10
FORM9    FORM      10
.
. .ACCOUNTS RECIEVABLE WORK FIELDS
.
TOTLR    FORM      9.2
FINLR    FORM      9.2(3)
FINLRB1  DIM       10
FORM9R   FORM      10
ACCREC   FORM      10
EndLRA    FORM      9.2
EndLRB    FORM      9.2
FORM7    FORM      7
.
. .PERCENTAGE COMPUTATION FIELDS
.
PCT      DIM       3
PCTB     DIM       5         % CHANGE IN yr1
PCTC     DIM       5         % CHANGE IN yr2
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
.
F2       DIM       2         FILLER
.
V1       FORM      "14"      SPLIT SCREEN POSITION
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
.FIRSTN   INIT      "433150"  FIRST NIN INV# OF 2002
.FIRSTN   INIT      "455522"  FIRST NIN INV# OF 2003
.FIRSTN   INIT      "477920"  FIRST NIN INV# OF 2004
.FIRSTN   INIT      "500807"   FIRST NIN INV# OF 2005
.FIRSTN   INIT      "520529"   FIRST NIN INV# OF 2006
.FIRSTN   INIT      "536416"   FIRST NIN INV# OF 2007
.FIRSTN   INIT      "551933"   FIRST NIN INV# OF 2008
.FirstN    INit      "566458"    FIRST NIN INV# OF 2009
.FirstN    INit      "577030"    FIRST NIN INV# OF 2010
.begin patch 1.31
.FirstN    INit      "586274"    FIRST NIN INV# OF 2011
.begin patch 1.32
.FirstN    INit      "595488"    FIRST NIN INV# OF 2012
.begin patch 1.34
.FirstN    INit      "603740"    FIRST NIN INV# OF 2013
.begin patch 1.41
.FirstN    INit      "612131"    FIRST NIN INV# OF 2014
.FirstN    INit      "620721"    FIRST NIN INV# OF 2015
FirstN    INit      "630497"    FIRST NIN INV# OF 2016
.begin patch 1.41
.end patch 1.34
.end patch 1.32

slspern  dim       2
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
TITLE    INIT      "Names in the News"
.KEY      INIT      "096"
.IND      FORM      2
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
slsloop   form      2

. .HOLD ACC/PAY
.
Payables   Form          9.2(3,12)               ..3 years worth of MONTHly data
.
. .HOLD ACC/REC
.
Receivables   Form          9.2(3,12)               ..3 years worth of MONTHly data

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
Invmask       init          "ZZ,ZZ9"
MaskIt    INIT      "$$$,$$$,$$Z.99"
.
. AVERAGE INVOICE FIELDS
.
FORM114  FORM      11.4
Average       Form          5.2
MaskAverage   init          "$$,$$9.99"
timestamp2 dim  16
        include     winapi.inc
.end patch 3.0
.
.
SPOOL    DIM       1
FMESG    DIM       20
ANS      DIM       1
LASTMOSW DIM       1               HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"

.begin patch 1.2
          move      "20",CC
          move      "20",str2
          Match    "NINV0019" TO PROGRAM
          if        equal
          Unpack    Today into mm,str1,dd,str1,yy
          Move      Today,Date

          else

          CLOCK     DATE TO DATE
          MOVE      DATE TO TODAY
          UNPACK    DATE INTO MM,STR1,DD,STR1,YY
          unpack        today into mm,str1,dd,str1,yy
          move          "20" to str2

          MOVE      "NINV0019" TO PROGRAM
          endif
          
          pack          holdToday from str2,yy,mm
          MOVE      MM TO MO
          MOVE      DD TO DA
          MOVE      YY TO YR
.end patch 1.2          
         MOVE      "NIN" TO COMPNME
         MOVE      "INVOICES By Salesperson" TO STITLE
         move      c1 to v
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         TRAP      STOP IF F5
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
         MOVE      MINIT,MINSTRT
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
         OPEN      Results,"lRdetMST.ISI|NINS1:502",exclusive

.***********************TESTING
.         goto      PrintIt
.***********************TESTING

.cleanup old totals before proceeding. This section needs to be updated as time rolls on
.currently we are deleting records from Jan 2002 to date.
.begin patch 1.01
.begin patch 1.3
.begin patch 1.31
.              move          "2011" to n4               .change every year
.begin patch 1.32
.              move          "2012" to n4               .change every year
.begin patch 1.34
.          move          "2013" to n4               .change every year
.begin patch 1.41
.          move          "2014" to n4               .change every year
          move          "2016" to n4               .change every year
.end patch 1.41
.end patch 1.34
.end patch 1.32
.              move          "2010" to n4               .change every year
.              move          "2009" to n4               .change every year
.end patch 1.3
.end patch 1.01
          for       SlsLoop from "0" to "35" using c1

                    For       Trackmonth from "1" to "12" using C1
                    packKey       resultkey from Slsloop,n4,trackmonth
                    rep           zfill in resultkey
                    read          results,resultkey;str1
                    if        Not over
                    Delete    Results
                    endif
                    Repeat
          REpeat                        






.
         TRAPCLR   IO
          MOve      C3,NinvLock
         MOVE      C2 TO NINVPATH           .SET ACCESS TO ISAM BY INV#.
         MOVE      "                    ",FMESG
         MOVE      "Q",CHECK
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      FIRSTN TO NINVFLD
         CALL      NINVkey
         GOTO      NOINV IF OVER
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
         MOVE       invdtEC,holdce

         MOVE      YR,HOLDYR
         GOTO      READ2
.
.
START    TRAPCLR   IO
         NORETURN
         DISPLAY   *P10:12,*EF,*HON,"InvStats File NOT FOUND. IT WILL BE CREATED."
         BEEP
         MOVE      "NINADJ" TO NADJNAME
         MOVE      "NAMES IN THE NEWS",TITLE
         Prepare   Results,"\\nins1\e\data\text\LrDetMSt.dat","\\nins1\e\data\index\LrDetMSt.isi","8","37"
......READS INVOICE FILE
.
READ1    CALL      NINVKS
         GOTO      PrintIt IF OVER
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
Read2
          move                c1 to nordpath
          MOVE                LRN TO NORDFLD
          CALL                NORDKEY
.         
          reset     EXFEELST
          scan      Olnum in EXFEELST
          goto      Read1 if equal

          reset     Runcodes
          scan      Olnum in Runcodes
          goto      Read1 if equal
          
          clear               slspern
          pack                slspern from osales10,osales

          call      Debug
              packKey       ResultKey from slspern,InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              Read          Results,ResultKey;ResultKey,Totinv,TotNIN,TOTLR
              if            over
              packKey       ResultKey from slspern,InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              move          c0 to Totinv
              move          c0 to TotNIN
              move          c0 to TOTLR
.begin patch
                    if                        (resultkey <> "00000000" & resultkey <> "        ")
                    write         Results,ResultKey;ResultKey,Totinv,TotNIN,TOTLR      .prepare record
                    read          results,ResultKey;ResultKey,Totinv,TotNIN,TOTLR      .prepare record for update
                    endif
.end patch 
              endif
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
          Call           NInvAcdRecLoad
          move      No,shipsw
          move      no,mrgsw
           move      olrn to nmrgfld
               move      c0 to nmrgrqty
               move      c0 to nmrgiqty
               move      c0 to nmrgnet
               move      no to mrgsw
               move      no to SHIPsw
               call      nmrgkey
               if       not over
               move     yes to mrgsw
               endif
               move      olrn to nshpfld
               call      nshpkey
               if       not over
               move     yes to shipsw
               endif

          CALL      COMPUTE

          call      debug
              add           c1 to totinv
              add           LRinc to TOTLR
.begin patch 1.40
          packkey   Tinvfld,olrn
           move       c0,form122
          call      Tinvkey
           if        not over
           Move      TinvDOLR,form122
           mult       ".01" by form122
           sub       form122 from NINinc
           endif
.end patch 1.40


              add           NININC to TotNIN
          MOVE      LRN TO NADJFLD
          rep       zfill in nadjfld
          CALL      NADJKEY
          GOTO      LOAD2 IF OVER
NEXT2     ADD       ASNININC,TotNIN
          add       ASLRINC,TOTLR
Load2
.
Load3
          if                         (resultkey <> "00000000" & resultkey <> "        ")
            Update         Results;ResultKey,Totinv,TotNIN,TOTLR      .prepare record
          endif
          move          c0 to Totinv
          move          c0 to TotNIN
          move          c0 to TOTLR
               call           WipeCVars
        GOTO      READ1
.
PrintIt
.begin patch 1.33
.          call      pdf995auto

                        move    "c:\work\pdf\LrDetMSt.pdf" to  str55
.                        PRTOPEN Laser,"PDF995",str25
.                        PRTOPEN Laser,"PDF:",str55,SPOOLFILE="c:\work\LRDETMST.SPL"
                        PRTOPEN Laser,"PDF:",str55
.                        pack    str55,str25,".pdf"
.end patch 1.33



                              move                startyear to holdyear
          Move      "00",str2
.Printloop   move       c0 to slsloop
             move       c0 to slsloop
                    move      str2 to slsloop
          for       Slsloop from "0" to "35" using c1
                    if        (slsloop = c1 )          
                    call      debug
                    endif
.  MUST BE CHANGED EVERY JANUARY!!!!
.begin patch 1.01
.         Move             "2006" to startyear
.          move                "2007" to startyear
.          move                "2008" to startyear
.begin patch 1.31
.          move                "2009" to startyear
.begin patch 1.32
.          move                "2010" to startyear
.begin patch 1.34
.          move                "2011" to startyear
.begin patch 1.41
.          move                "2012" to startyear
          move                "2014" to startyear
.end patch 1.41
.end patch 1.34
.end patch 1.01
          move                startyear to holdyear
          MOVE                OSLS0 TO str25
          LOAD                str25 FROM slsloop OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                    OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                    OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                    OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
          DISPLAY   *P14:6,"Sales ##: ",slsloop,b1,str25
          
          move        c0 to Yearcount
          move        c0 to Trackmonth
                    call      Header
          Call      PrintLoop
          REpeat
          goto      Stop

Printloop
          For       Yearcount from c1 to c3 using c1

                    for       trackmonth from "1" to "12" using c1    
                    PACK        resultkey,slsLoop,startyear,trackmonth     .first year to be read
                    rep       zfill in resultKey
                    read       results,ResultKey;REsultkey,Totinv,TotNIN,TOTLR
                    if        over
                    move      c0,Totinv
                    move      c0,TotNIN
                    move      c0,TOTLR
                    endif
                    call      LoadArray
.                   call      printmodet
                    repeat
          add       c1,startyear
.ok array is full lets print
.         call      PrintYRtot
          repeat
          call      Dumparray
          move      c0,Totinv
          move      c0,TotNIN
          move      c0,TOTLR
          REturn


.                   PACK                resultkey,slsloop,startyear,trackmonth
.                   rep       zfill in resulTkey
.         
.         read                results,ResultKey;ResultKey,Totinv,TotNIN,TOTLR
.              call          loadArray
.              move          no to overflag
.              loop
.              until         (overflag = "Y")
.Nope          readks        results;ResultKey,Totinv,TotNIN,TOTLR
.              if            over
.              move          "Y" to overflag
.              endif
.         unpack    resultkey into n2,str6
.         if        (n2 <> slsloop)
.             move          "Y" to overflag
.         endif
.              call          LoadArray
.              repeat
.print goodies here
Dumparray
              move          c1 to yearcount
              move          c1 to trackmonth
              loop
              until         (yearcount = 4 )
                            loop
                            until         (trackmonth = 13)
                                 move          Invcount(yearcount,trackmonth) to TOtinv
                              move          Payables(yearcount,trackmonth) to TotNIN
                      move          Receivables(yearcount,trackmonth) to TOTLR
                                            If            (totinv > 0 or TotNIN > 0 or TOTLR >0)
                                 call          printModet
                                            endif
                            add           totinv to Fininv(yearcount)
                            add           TotNIN to FinNIN(yearcount)
                            add           TOTLR to FinLR(yearcount)
                            add           c1 to trackmonth
                            repeat
              call          PrintYrTot
              add           c1 to yearcount
              MOVE          C1 TO TRACKMONTH
              repeat
          call      ClearArray
.         add       c1,slsloop
.         move      slsloop,str2
          Return

..........................................................................................
ClearArray
              move          c1 to yearcount
              move          c1 to trackmonth
              loop
              until         (yearcount = 4 )
                            loop
                        until         (trackmonth = 13)
                    move          C0 to Invcount(yearcount,trackmonth)
                    move          C0 to Payables(yearcount,trackmonth)
                    move          C0 to Receivables(yearcount,trackmonth)
                    Move          C0 to Fininv(yearcount)
                        Move          C0 to FinNIN(yearcount)
                        Move          C0 to FinLR(yearcount)
                        add           c1 to trackmonth
                        repeat
              add           c1 to yearcount
              MOVE          C1 TO TRACKMONTH
              repeat
              move          c1 to yearcount
              move          c1 to trackmonth

          return
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
              edit          TOTLR to Str14
              Prtpage      laser;*font=prtpg10,*p=columna:row,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,Str14
              if            (yearcount = 2)
              move          Receivables(1,trackmonth) to ENDLRA
              move          Receivables(2,trackmonth) to ENDLRB
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
              move          Receivables(2,trackmonth) to ENDLRA
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
                        divide          N2 into ENDLRA
                        move          dd to n2
                        mult          n2 by ENDLRA
                        MOVE          ENDLRA to Receivables(2,trackmonth)         .adjust ytd totals for year 2
                        endif

              move          Receivables(3,trackmonth) to ENDLRB
              call          Percent
              move  Column3B to ColumnB
              add             "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif

              add           "125" to ROw
              move          maskit to Str14
              edit          TotNIN to Str14
              Load          COLUMNB from yearcount of Column1b,Column2b,Column3b
              Prtpage       laser;*font=prtpg10,*p=columnb:row,*ALIGNMENT=*Right,Str14,*ALIGNMENT=*Left
              RETURN
.............................................................................................................
PrintYrTot

          move          INVmask to str6
              edit          FinInv(yearcount) to str6
              move          maskit to Str14
              edit          FinLR(yearcount) to Str14
              Prtpage      laser;*font=prtpg10,*p=columna:7050,*ALIGNMENT=*Left,str6:
                           *font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,Str14
              move          maskit to Str14
              edit          FinNIN(yearcount) to Str14
              Prtpage      laser;*font=prtpg10,*p=columnb:7175,*ALIGNMENT=*Right,Str14,*ALIGNMENT=*Left
.do average goodies
              move          Fininv(yearcount) to form5
              move          FinLR(Yearcount) to Form114
              Divide        form5 into Form114
              Move          c0 to Average
              add           Form114 to average
              move          MaskAverage to str9
              edit          AVERAGE to str9
              PRTPAGE       Laser;*font=prtpg10,*p=columna:7300,*ALIGNMENT=*Left,"Average:":
                            *p=columnb:7300,*ALIGNMENT=*Right,str9
              move          Fininv(yearcount) to form5
              move          FinNIN(Yearcount) to Form114
              Divide        form5 into Form114
              Move          c0 to Average
              add           Form114 to average
              move          MaskAverage to str9
              edit          AVERAGE to str9
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7425,*ALIGNMENT=*Right,str9,*ALIGNMENT=*Left
              If            (yearcount = 2)
              move          FinLR(1) to ENDLRA
              move          FinLR(2) to ENDLRB
              call          percent
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif
              If            (yearcount = 3)
debug1
              move          c0 to ENDLRA                .zero it out and we will begin
              move          c1 to n2
              unpack        holdtoday into str4,str2
              move          str2 to trackmonth
              Loop
              until         (n2 = Trackmonth+1)
              add           Receivables(2,n2) to ENDLRA
              add           c1 to n2
              repeat
              move          FinLR(3) to ENDLRB
              call          percent
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
              PRTPAGE       Laser;*font=prtpg10,*p=columnb:7050,*ALIGNMENT=*Right,pctb,*ALIGNMENT=*Left
              endif
              RETURN
.............................................................................................................

LoadArray
.begin patch 3.1
          if        (slsloop = "29")
          call      Debug
          endif
.               clear          str4
.               clear          str2
..end patch 3.1
.              unpack        RESULTKEY into slspern,str4,str2
.         call      Trim using str4
.         call      Trim using str2
..begin patch 3.1
.              if              (str4 = "" or str2 = "")
.              return
.              endif
..end patch 3.1
.              Move          str4 to n4
.              move          str2 to trackmonth
.              compare       n4 to Holdyear
.              if            Not equal
.              move          n4 to Holdyear
.              add           c1 to yearcount
.              endif
.         if        (yearcount > 3 | trackmonth > 12)
..error condition
.         return
.         endif
              move          TOTINV to invcount(yearcount,trackmonth)
              move          TotNIN to payables(yearcount,trackmonth)
              move          TOTLR to Receivables(yearcount,trackmonth)
              return
.............................................................................................................
.
DISP
         DISPLAY   "Confidential",B7,B7,B7,TITLE,B5,B5,B5,B5,"Date",B2:
                   MO2,"/",DA2,"/",YR2;
 DISPLAY B7,B7,B7,B7,B7,B7,B7,B7,B7,B7,"Time",B2,HRSTRT,":",MINSTRT," ",SECSTRT
.
.
TIME
.begin patch 1.2
.         CLOCK     DATE TO DATE
.         IFNZ      PC
.         UNPACK    DATE INTO MM,DD,YY
.         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
.         XIF
.         IFZ       PC
.         MOVE      DATE TO TODAY
.         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
.         XIF
..begin patch 3.0
.              unpack        today into mm,str1,dd,str1,yy
.              move          "20" to str2
.              pack          holdToday from str2,yy,mm
.end patch 3.0
.         MOVE      MM TO MO
.         MOVE      DD TO DA
.         MOVE      YY TO YR
.end patch 1.2
         CLOCK     TIME TO TIME   (DIM 8)
         UNPACK    TIME INTO HR,ANS,MINIT,ANS,SEC
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
.call with   ENDLRA and ENDLRB
.return with percent diff in PCTB
PERCENT
.         MOVE      FinLRA,FinLR
.         MOVE      FinLRB,ENDAR
              If            (ENDLRA = c0 and ENDLRB = c0)
              move          b5 to pctb
              clear         PCTB
              return
              endif
         COMPARE   ENDLRA,ENDLRB
         GOTO      PERCENT2 IF NOT LESS
         MOVE      ENDLRB,WORK
         DIV       ENDLRA,WORK
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
         MOVE      ENDLRB TO WORK
         DIV       ENDLRA INTO WORK
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
         MOVE      FINALSAV,ENDLRA
         MOVE      " ",OK
         GOTO      PERCENT5
PERCENT4 CMATCH    "Y" TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FinLRB1 TO ENDLRA
         MOVE      " " TO LASTMOSW
         GOTO      PERCENTY
PERCENTX MOVE      FinLR(2),ENDLRA
PERCENTY
         Compare   c0,FinLR(3)
         GOTO      PERCENT7 IF EQUAL
PERCENT5 MOVE      FinLR(3),ENDLRB
         Compare   C0,FinLR(3)
         GOTO      PERCENT7 IF EQUAL
         COMPARE   ENDLRA,ENDLRB
         GOTO      PERCENT6 IF NOT LESS
         MOVE      ENDLRB,WORK
         DIV       ENDLRA,WORK
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
.PERCENT6 MOVE      FinLR,WORK
.         DIV       ENDLRB,WORK
         MOVE      ENDLRB TO WORK
         DIV       ENDLRA INTO WORK
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
.         REP       "0 ",FinLRC
.         REP       "0 ",FinNINC
.         REP       "0 ",FININVC
.end patch 3.0
         MOVE      "X",C
         RETURN
Header
............
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Portrait

          if        (slsloop > 0)
          prtpage   LASER;*newpage
          endif
          prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
              *Pictrect=*off,*PICT=0:1000:2250:8550:NINLogo:
                    *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
                    *p=750:375,str25:
                    *FONT=prtpg10,*P=7000:375,"Time: ",Time
.begin patch 1.01
.                    *p=1475:1100,"2006":
.                    *p=3725:1100,"2007":
.                    *p=5725:1100,"2008":
.begin patch 1.41
           MOve       Startyear,N4                      
          prTPAGE    LASER;*p=1475:1100,N4
          add         c1,n4
          prTPAGE    LASER;*p=3725:1100,N4
          add         c1,n4
          prTPAGE    LASER;*p=5725:1100,n4:
.end patch 1.41
.end patch 1.01
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:1250,*PENSIZE=10,*line=ColumnB:1250
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:1250,*PENSIZE=10,*line=columnb:1250:
                    *font=prtpg10B,*p=1:1800,"Month":
                    *font=prtpg10,*p=COLUMN1A:1800,"Invoices":
                    *font=prtpg10,*p=COLUMN1B:1800,*alignment=*Right,"LR/NIN":
                    *p=column1a:1950,*PENSIZE=10,*line=column1b:1950:
                    *font=prtpg10,*p=column2a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column2b:1800,*alignment=*Right,"LR/NIN"
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
         PrtPage    Laser;*font=prtpg10,*p=columnb:1800,*alignment=*Right,"Chng ":
                    *FONT=prtpg10,*p=column2a:1950,*PENSIZE=10,*line=ColumnB:1950:
                    *font=prtpg10,*p=column3a:1800,*alignment=*Left,"Invoices":
                    *font=prtpg10,*p=column3b:1800,*alignment=*Right,"LR/NIN"
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
.begin patch 1.33 changes undone after sunbelt fix
                    *p=column1a:7025,*PENSIZE=10,*line=column1b:7025:
                    *p=column1a:7575,*PENSIZE=10,*line=column1b:7575
.                    *p=column1a:6950,*PENSIZE=10,*line=column1b:6950:
.                    *p=column1a:7525,*PENSIZE=10,*line=column1b:7525
.end patch 1.33
                    move      Column2B to ColumnB
                    add     "500" to ColumnB
.begin patch 1.33 changes undone after sunbelt fix
         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column2a:7575,*PENSIZE=10,*line=columnb:7575
.         prTPAGE    LASER;*FONT=prtpg10,*p=column2a:6950,*PENSIZE=10,*line=columnb:6950:
.                    *p=column2a:7525,*PENSIZE=10,*line=columnb:7525
.end patch 1.33
                    move      Column3B to ColumnB
                    add     "500" to ColumnB
         prTPAGE    LASER;*FONT=prtpg10,*p=column3a:7025,*PENSIZE=10,*line=columnb:7025:
                    *p=column3a:7575,*PENSIZE=10,*line=columnb:7575:
                    *font=prtpg10,*p=1:7050,"Totals":
                    *p1:8000,"*** This report includes adjustments done in the same year as the associated invoice ***"
          Return


.
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:01,"***** NINV0019 *****":
.begin patch 3.0
                   *P21:03,"NAMES IN THE NEWS";
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
                move    C0,N9
                move    "                                        ",APIFileName
                clear   APIFileName
                pack    APIFileName,"C:\WORK\PDF\lrdetmst.pdf",hexzero
                clock   timestamp,timestamp1
                move    timestamp1,time1
                loop
                        clock   timestamp,timestamp2
                                move    timestamp2,time2
                                sub     time1,time2,time3
                                if (time3 > 5500) .55 Seconds Maximum
.                                if (time3 > 12000) .120 Seconds Maximum
                                         break
                                endif
              repeat

          Move      "Here is Salesperson INV history Pdf",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          move       mm,n2
          Move      "DavidHerrick@nincal.com",MailTo
          if           (n2 = "6" or n2 = "12")
          Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
          Endif 
          Move      Str55,MailBody
          MOve      "c:\work\pdf\lrdetmst.pdf",MailAttach
          call      SendMail

          winshow
          pause     "5"
.end patch 3.0
.begin patch 1.33
.          call      pdf995auto0
.end patch 1.33


         shutdown  "CLS"
.begin patch 3.3
.         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
          INclude   NInvAcdio.inc
          iNCLUDE   nORDIO.INC
         INCLUDE   NADJIO.inc
         include   nacdio.inc
         include   ndatio.inc
         include   ndat3io.inc
         include   nshpio.inc
          include   Nmrgio.inc
          include        compio.inc
          include        cntio.inc
          include   Nownio.inc
          include   compute.inc
.begin patch 3.0
          INCLUDE          PRTPAGEIO.INC
.end patch 3.0
.begin patch 1.40
           include    TinvIO.inc
.end patch 1.40
         INCLUDE   COMLOGIC.inc


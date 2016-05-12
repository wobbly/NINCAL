...............................................................................
.calculate current year billing and compare to prior two years
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
          INCLUDE             ninvdd.inc
          INclude   NInvAcddd.inc
          include     ndat3dd.inc
         inc       hp.inc
         INCLUDE   NJstDD.inc
         include   nacddd.inc
           include    consacct.inc
           INCLUDE    NORDDD.INC
           INCLUDE    NSHPDD.INC
           INCLUDE    NMRGDD.INC
           INCLUDE    NOWNDD.INC
          include   compdd.inc
          include   cntdd.inc
           INCLUDE    NDATDD.INC
          INCLUDE          PRTPAGEDD.INC
Release   Init      "4.00"      DH make it calculate like the other programs, sort out current year records for invoices and adjustments
.                                see release 3.53 for old code
Reldate   Init      "2016 May 12"             
.Release        Init           "3.53"      DH new year,
.Reldate   Init      "2016 Jan 4"             
.
. .FILES DECLARATIONS
.
Results   IFILE      Keylen=6,FIX=35          .PHYSICAL OUTPUT FILE 'INVSTATS.isi'
.file layout
resultkey           dim           6         CCYYMM      1-6
.
. ..........................................................................
holdToday       dim         6
File       File
mrgsw    dim       1
shipsw   dim       1
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
MINs      DIM       2         POS 17-18  MINUTE
SEC      DIM       2         POS 20-21  SECOND
.
. .WORK SPACE
.
FINAL    DIM       1         CHECKS END OF PROGRAM
C        DIM       1         CHECKS FOR "0" IN TOTAL
OK       DIM       1
CHECK    DIM       1
.
COUNT    FORM      5
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
TOTAP    FORM      9.2
FINAP    FORM      9.2(3)         TOTAL NAMES INVOICED DURING YEAR
FORM9P   FORM      10
ACCPAY   FORM      10
FORM9    FORM      10
.
. .ACCOUNTS RECIEVABLE WORK FIELDS
.
TOTAR    FORM      9.2
FINAR    FORM      9.2(3)
FINARB1  DIM       10
FORM9R   FORM      10
ACCREC   FORM      10
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
V1       FORM      "14"      SPLIT SCREEN POSITION
V2       FORM      "25"      SPLIT SCREEN POSITION
.
. **********CHANGE THE FOLLOWING INV# NUMBER IN JAN*********
.FirstN    INit      "620721"    FIRST NIN INV# OF 2015
FirstN    INit      "630497"    FIRST NIN INV# OF 2016
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
.INDEX    FORM      "01"
ZERO     FORM      "0"
TITLE    INIT      "COMPUNAME        "
KEY      INIT      "096"
TEN      FORM      "10"
IND      FORM      2
.
. HOLD TOTAL number INVOICES
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
.
Payables   Form          9.2(3,12)               ..3 years worth of MONTHly data
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
.
.
SPOOL    DIM       1
FMESG    DIM       20
ANS      DIM       1
LASTMOSW DIM       1               HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
         MOVE      "NINV0005" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "CALCULATE CURRENT INVOICES" TO STITLE
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
         MOVE      MINs,MINSTRT
         REP       zfill,MINSTRT
         MOVE      SEC,SECSTRT
         MOVE      DA,DA2
         REP       zfill,DA2
         unpack    yr into yr2
         MOVE      MO,MO2
         REP       zfill,MO2
         TRAP      START IF IO
.
         OPEN      Results,"InvStats|NINS1:502",exclusive
.cleanup old totals before proceeding. This section needs to be updated as time rolls on
.              move          "2015" to n4
              move          "2016" to n4
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
.              if            (n4 < 2015)
              if            (n4 < 2017)
              goto          cleanloop
              endif
.
         TRAPCLR   IO
.begin patch 4.0
.sort 'NEW' Invoices.
         DISPLAY   *P10:09,"Sorting 'new' invoices: "

          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.New;7-12,S=#"132=#'",Yr2,"'#""
          Sort      Taskname,SunDM="NINS1:502"
.
.sort 'NEW' adjustments
         DISPLAY   *P10:11,"Sorting 'new' adjustments: "
          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127='",yr2,"'#""
          Sort      Taskname,SunDM="NINS1:502"

         MOVE      "                    ",FMESG
         MOVE      "Q",CHECK
         MOVE      "NAMES IN THE NEWS",TITLE
           goto       read1
.end patch 4.0
.
.THE FOLLOWING ROUTINE IS ONLY ENCOUNTERED IF the outpt file DOES NOT EXIST. IT
.CREATES 'ARMST' AND OPENS ALL OF THE NECESSARY INVOICE AND ADJUSTMENT FILES
.
START    TRAPCLR   IO
         NORETURN
         DISPLAY   *P10:12,*EF,*HON,"InvStats File NOT FOUND. IT WILL BE CREATED."
         BEEP
         MOVE      "NAMES IN THE NEWS",TITLE
         Prepare   Results,"\\nins1\e\data\text\INVStats.dat","\\nins1\e\data\index\INVStats.isi","6","35"
Read1
          open      FIle,"\\nins1\e\data\nininv.new|NINS1:502"

           lOOP
READsls   Read      File,seq;Invvars
           until      over
          ADD       C1 TO COUNT
          DISPLAY   *P10:13,"New Sales records PROCESSED: ",COUNT
         move      c0 to mo
         move      c0 to da
         move      c0 to yr
         MOVE      INVDTEM TO MO
         MOVE      INVDTED TO DA
         MOVE      INVDTEY TO YR
        MOVE       invdtEC,holdce
         ADD       "1",COUNT1
         DISPLAY   *PV1:6,COUNT1,*PV2:9,MO,"/",DA,"/",YR," ",LRN;
           if           (COUNT1 = c1)
           MOVE      YR,HOLDYR
           endif  
           MOVE       C3,NORDLOCK
           PACKKEY    NORDFLD FROM LRN
           CALL       NORDKEY
         move      c1 to nownpath
         move      c3 to nownLOCK
         move      olon to nownfld
         call      nownkey
         MOVE      NORDFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL        NMRGKEY
         if      not over
                       move    yes to mrgsw
         endif   
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
.not nec to read ship as already billed
         move      no to shipsw
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
          move        No,SUBPPSW
           call       compute

              packKey       ResultKey from InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              Read          Results,ResultKey;ResultKey,Totinv,TotAP,TotAR
              if            over
              packKey       ResultKey from InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              move          c0 to Totinv
              move          c0 to totap
              move          c0 to totar
                            if   (resultkey <> "000000" & resultkey <> "      ")
                            write         Results,ResultKey;ResultKey,Totinv,TotAP,TotAR      .prepare record
                            endif
              add           c1 to totinv
              add           ar to totar
              add           ap1 to totap
              add           ap2 to totap
              else                                     .not over 
              add           c1 to totinv
              add           ar to totar
              add           ap1 to totap
              add           ap2 to totap
              Update         Results;ResultKey,Totinv,TotAP,TotAR      .prepare record
              endif

           repeat
 
           open      FIle,"\\nins1\e\data\nadjust.new|NINS1:502"
          Move      C0 TO COUNT
           Loop
Readjst   Read      File,seq;jstvars
           until      over       
          ADD       C1 TO COUNT
          DISPLAY   *P10:15,"New adjustment records PROCESSED: ",COUNT
           if       (JSTREASN <> "16")      .skip adv pay to LO
              packKey       ResultKey from InvDteC,InvDteY,InvDteM
              rep           Zfill in ResultKey
              Read          Results,ResultKey;ResultKey,Totinv,TotAP,TotAR
              if            over
                      packKey       ResultKey from InvDteC,InvDteY,InvDteM
                      rep           Zfill in ResultKey
                      Move          JSTAP1 to totap
                      Move          JSTAP2 to totap
                      Move          JSTAR to TotAr
                            if   (resultkey <> "000000" & resultkey <> "      ")
                            write         Results,ResultKey;ResultKey,Totinv,TotAP,TotAR      .prepare record
                            endif
              else
                      rep           Zfill in ResultKey
                      add           JSTAP1 to totap
                      add           JSTAP2 to totap
                      add           JSTAR to TotAr
                      Update         Results;ResultKey,Totinv,TotAP,TotAR      .prepare record
            endif
           ENDIF
           repeat
           goto       printit
.................................................
.
PrintIt
                        move    "Invprt" to  str25
                        pack    str55,"c:\work\pdf\",str25,".pdf"
                        PRTOPEN Laser,"PDF:",str55
............
        PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*Portrait


         prTPAGE    LASER;*FONT=prtpg10,*p=1:150,"Confidential":
              *Pictrect=*off,*PICT=0:1000:2250:8550:NINLogo:
                    *FONT=prtpg10,*P=7000:150,"DATE: ",tODAY:
                    *FONT=prtpg10,*P=7000:375,"Time: ",Time:
                    *p=1475:1100,"2014":
                    *p=3725:1100,"2015":
                    *p=5725:1100,"2016":
                    *p=column1a:1250,*PENSIZE=10,*line=column1b:1250
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
                                 move             "2014" to startyear
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
               clear          str4
               clear          str2
              unpack        RESULTKEY into str4,str2
              if              (str4 = "" or str2 = "")
              return
              endif
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
              unpack        today into mm,str1,dd,str1,yy
              move          "20" to str2
              pack          holdToday from str2,yy,mm
         MOVE      MM TO MO
         MOVE      DD TO DA
         MOVE      YY TO YR
         CLOCK     TIME TO TIME   (DIM 8)
         UNPACK    TIME INTO HR,ANS,MINs,ANS,SEC
         MOVE      HR,NUMHR
         COMPARE   "12",NUMHR
         GOTO      TIME4 IF EQUAL
         GOTO      TIME3 IF LESS
         MOVE      "PM",SEC
         SUB       "12",NUMHR
         MOVE      NUMHR,HR
         RETURN
TIME3
         MOVE      "AM",SEC
         RETURN
TIME4
         MOVE      "PM",SEC
         RETURN
.
. .....FIGURES PERCENTAGE CHANGE
.
.call with   EndARA and EndARB
.return with percent diff in PCTB
PERCENT
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
         MOVE      "X",C
         RETURN
.
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:24,"***** INVOCPRT *****",*R,*R:
                   *P12:24,"NAMES IN THE NEWS";
          DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
              return
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
STOP
              call          time
              PrtPage       laser;*FONT=prtpg10,*P=7000:8000,"Time: ",Time
              PrtClose      Laser

          Pause     "3"

          Move      "Here is your INV history PDF File",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "Dherric@nincal.com",MailTo
          Move      Str55,MailBody
          MOve      "c:\work\pdf\invprt.pdf",MailAttach
          call      SendMail

          winshow
          pause     "10"

         shutdown  "CLS"
         stop
          INCLUDE             ninvio.inc
          INclude   NInvAcdio.inc
         INCLUDE   NJstIO.inc
          INCLUDE          PRTPAGEIO.INC
          include     ndat3io.inc
         include   nacdio.inc
           INCLUDE NORDIO.INC
           INCLUDE    NSHPIO.INC
           INCLUDE    NMRGIO.INC
           INCLUDE    NOWNIO.INC
          include   compIO.inc
          include   cntIO.inc
           INCLUDE    NDATio.INC
           include    compute.inc
         INCLUDE   COMLOGIC.inc


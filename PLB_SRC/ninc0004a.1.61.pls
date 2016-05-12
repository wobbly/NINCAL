...............................................................................
. INCOMEBYMON - CALC'S CLIENT BILLING & ADJUSTMENT FOR CURRENT MONTH.
...............................................................................
.
PC        EQU       0
          INC       COMMON.inc
.
          INC       CONS.inc
          INCLUDE   CONSacct.inc
          INCLUDE   NINVDD.inc
          include   nacddd.inc
          include   nshpdd.inc
          include   compdd.inc
          include   cntdd.inc

          INC       NBILDD.inc
          INCLUDE   NOWNDD.INC
          INCLUDE   NDATDD.inc
          INCLUDE   NORDDD.INC
          INCLUDE   NDAT3DD.INC
          INclude   Ntxtdd.inc.
          INclude   NSeldd.inc.
          INCLUDE   NSEL2DD.INC
          INC       GNXTDD.inc
          include   nmrgdd.inc
          INC       NadjDD.inc
          include   ninvacddd.inc
.begin patch 1.3
               include        nmlddd.inc
.end patch 1.3
.begin patch 1.6
               include        TINVdd.inc
form122   Form      12.2
.end patch 1.6

shipsw    dim       1
mrgsw     dim       1
.
release  init      "1.61"          DLH     added Flags= to prtopen with PDF:
reldate   Init      "2014 March 25"
.release   init      "1.6"          DLH     .break out NIN income and LO payables
.reldate   Init      "2013 June 17"
.release   init      "1.5"          DLH     .Sunbelt PDF
.reldate   Init      "2013 April 25"
.release   init      "1.4"          DLH     .add Mailers Name PEr Susan request
.reldate   Init      "11 March 2013"
.release   init      "1.3"          DLH     .add Maildate for Pia's calcs, force mode to "L"
.reldate   Init      "4 February 2013"
.release   init      "1.2"          DLH     .Pull LM for project vs actual by list report
.reldate   Init      "1 November 2012"
.Release   INit      "1.1"     DLH       quick compare of 2 periods
.reldate   Init      "21 May 2012"
.release   init      "1.01"          DLH     .Add Inga
.reldate   Init      "26 October 2009"
.release   init      "1.00"          DLH     New 
.reldate   Init      "1 September 2009"
.CLOCK    FUNCTION
........................
DATE      DIM       10
SYSMO     DIM       2
SYSDY     DIM       2
SYSYR     DIM       2
SYSDAT    DIM       8                        SYSTEM DATE
.
.FILES.
...............................................................................
.

OUTPUT    FILE     .ordvars of qualifying records
OUTPUT1   IFILE     KEYLEN=10,FIX=240
CSVFile   File      
.
HBILLKEY DIM       8    *FOR MATCH MLR/BILL-TO BREAK?
KEY54    DIM       54   *OUTPUT FILE KEY.
. WORK VARIABLES
.
.
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
PASS     FORM      1
ANS      DIM       1
ADJAR    FORM      7.2
ADJLR    FORM      7.2
ADJAP    FORM      7.2
ZERO     FORM      "0"
ONE      FORM      "1"
BROK     DIM       1
.
COUNT    FORM      5
COUNT1   FORM      5
CO       FORM      1
DATEMASK DIM       10
.
CoName   DIM       45
str54    dim       54
UnGross   FOrm      9.2                 .Unbilled gross AR
Flag      Dim       1                   .records type being processed 'U'billed orders 'B'illed, 'O'pen, 'P'aid
Mode      Init      "M"                        .'M'ailer or 'L'ist
Form52    Form      5.2
net92    form      9.2            holding field while calcing net charges
QTYCHK   FORM      9
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
JUNDATE  FORM      6
JUNEDAT  INIT      "060191"
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
MDATE    FORM      5
lwrobb12 init     "2749-1822-0704-0974-1451-0127-0638"
card$    form      5.2
HoldTeam  Dim       2
N34           FORM            3.4
TEXT1    DIM       47
STR4A     dIM       4

BildAR    FOrm      9.2       .billed ar
BildLR    Form      9.2       .billed rev

PaidAR    Form      9.2       .paid ar
PaidLR    Form      9.2       .paid rev
UNBILINC  FORM      9.2       
UNBILAmt  FORM      9.2
Form102   Form      10.2
GrandAR      FORM      10.2    
GrandLR      FORM      10.2    

TotEords    form      9        .lm exchange
TotEnames   Form      9        .lm exchange
TotEBords   Form      9        .lm exchange
TotEBnames  Form      9        .lm exchange         
TotEBAR     FOrm      9.2        .lm exchange
TotEBLR     FOrm      9.2        .lm exchange
TotEPords   Form      9        .lm exchange
TotEPnames  Form      9          .lm exchange       
TotEPAR     FOrm      9.2        .lm exchange
TOTEPLR     FOrm      9.2        .lm exchange
TotEOords   Form      9        .lm exchange
TotEOnames  Form      9          .lm exchange       
TotEOAR     FOrm      9.2        .lm exchange
TOTEOLR     FOrm      9.2        .lm exchange




Totords   form      9
Totnames  Form      9
Notords   form      9         .calc unbilled tots
Notnames  Form      9         .calc unbilled tots
TotXnames Form      9         .calc unbilled exchange names
TotUnAR   Form      9.2
TotUnLR   Form      9.2
TotBords   Form      9
TotBnames Form     9         
TotXBName Form      9          .Billed exchange names
TotBAR    FOrm      9.2
TotBLR    FOrm      9.2
TotPords   Form      9
TotPnames Form     9 
TotXPName Form      9         .Paid Exchange names
TotPAR    FOrm      9.2
TOTPLR    FOrm      9.2
TotOords   Form      9
TotOnames Form     9         
TotXoName Form      9                   .Exchange
TotOAR    FOrm      9.2
TOTOLR    FOrm      9.2
Pcc       Dim       2                             .paid century
Pyy       dim       2                             .paid year
PMM       dim       2                             .paid month
Pdd       dim       2                             .paid day
DetailFlag          Dim       1

Pdate     Dim       10
Odate     Dim       10
Bdate     Dim       10
MLdate     Dim       10

........................................
.OUTPUT1.
........................................
OutKey    DIm       10           1-8     team+ list or company "mailer" number

Team      Dim       2           1-2
OutID     Dim       6           3-8    list or company "mailer" number
OutID1    Dim       2           9-10     .1=date range one, 2=date range two
OrdCount  Form      5          10-15          
Ordnames  Form      9          16-24 
NotXnames Form      9          25-33          . unbilled exchange names
OrdAr     Form      10.2       34-46    .est a/r
OrdLR     Form      10.2       47-59    .Est LR
BildCount Form      5          60-64
bildNames Form      9          65-73 
BART      FORM      10.2       74-86     billed a/r
BAPT      FORM      10.2       87-99     billed a/P
BLRT      FORM      10.2      100-     billed lr
BNINT     FORM      10.2               billed NIN
Opencount form      5         100-105
Opennames form      9         106-114
OART      FORM      10.2      115-127    Paid      A/R
OLRT      FORM      10.2      128-140   Paid      L/R
paidcount form      5         141-145
paidnames form      9         146-154
PART      FORM      10.2      155-167    Paid      A/R
PLRT      FORM      10.2      168-180   Paid      L/R
LName     Dim       35        181-215     list or mailer name
Mlrname   DIm       35        216-250
.===================================================================================
.Print Vars
prfile     pfile
PSLSFILE   PFILE
.===================================================================================
.Titles
.Title1  init "DATE"
Title2  init "Count"
Title2a init "Receivable"
Title3  init "Names"
Title3a init "Payable"
Title4  init "Revenue"
Title5  init "Income"
Title5a init "Accounts"
Title5b init "Receivable"
Title6  init "Adjusted"
Title6a init "Accounts"
Title6b init "Payable"
Title7 init "Adjusted LR"
Title7a init "Income"
Title8  init "Adjusted"
Title8a init "Total LR"
Title8b init "Income"

.Font
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
font8   font
font9   font
font14   font
.============================================================
.Create Fonts
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
        create  font7,"Times New Roman",size=9
        create  font8,"Times New Roman",size=10
        create  font9,"Times New Roman",size=10,italic
        create  font14,"Times New Roman",size=11
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
.============================================================
.Printed # TOTALS
PAPTOT    FORM      13
PADJARTOT FORM      13
PADJAPTOT FORM      13
PADJLRTOT FORM      13
PADJNINtot form     13
PFINLRTOT form      13
.================================================================================
.GRAND TOTALS
ARGRND     form        13
APGRND     form        13
LRGRND     form        13
ADJARGRND  form        13
ADJAPGRND  form        13
ADJLRGRND  form        13
ADJNINGRND form        13
FINLRGRND  form        13
.=================================================================================
.Pos of Header info
TitleH  form  9
TitleH2 form  9
.Headers for Sales Report
TitleH3 form  9
TitleH4 form  9
.===================================================================================
RowCount form 9         .count of number of entries per pg
PgCnt   form  9         .Page #
NEWPG   FORM 1         .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
Copy     form 4        .# of copies
.===================================================================================
salesinclistview listview
White     color
Black     color
.Listviewobject for Sorting of Teams
Team1   init    "11-21-29"         .NIN div brokerage
Team2   init    "27"         .NIN Div LM
Team3   init    "               "            .AL TEAM  MM(10) added 8/05
Team4   init    "06"            .LM - 
Team5   init    "00-00-00"            .Uncategorized
T1      init    "01"                                                     .ND
T2      init    "02"                                                     .ND LM
T3      init    "03"                                                     .SA
T4      init    "04"                                                     .SK
NAME1   init    "Brokerage"
NAME2   init    "Management"
NAME3   init    "Brokerage"
NAME4   init    "Management"
NAME5   init    "Uncategorized"
seller  dim      2
Stamp   dim      10
holdtm dim       1
SLSARTOT    FORM    13
SLSFINLRTOT form    13
n13         form    13
n13a         form    13
saleskey    dim       46
.vrs for listview
LVARTOT     form    9.2
LVADJLRTOT  form    9.2
LVFINLRTOT  form    9.2
LVLRTOT     form    9.2
LVLRSTR     DIM     13
LVARSTR     DIM     13
LVADJLRSTR  DIM     13
LVFINLRSTR  DIM     13
str46       dim     46
FIRSTN   INIT      "686400"  Start here - what is the firstn lr really?
...................................................
.
BEGINV   DIM       6
specl    dim       1
.
brker    dim       1
sellerv  dim       2
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NINC0004a" TO PROGRAM
         MOVE      "List Management INCOME " TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C2 TO NINVPATH        .SET ACCESS TO ISI BY INV#.
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NMLRPATH
         move      c3 to nmlrlock
         move      c3 to nordlock
.
        move    "100",column
        move    "3400",column1
        move    "4300",column2
        move    "5300",column3
        move    "6300",column4
        move    "7300",column5
        move    "8300",column6
        move    "9300",column7
        move    "10300",column8
        move    "5260",TitleH
        move    "9000",TitleH2
        move    "3800",TitleH3
        move    "6600",TitleH4


.
          cmatch    b1,today
          if        not equal
          UNpack    today into sysmo,str1,sysdy,str1,sysyr
         pack      date from sysmo,slash,sysdy,slash,cc,sysyr
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
          
          else

         clock     timestamp to timestamp
         unpack    timestamp into cc,sysyr,sysmo,sysdy
         pack      date from sysmo,slash,sysdy,slash,cc,sysyr
         pack      today from sysmo,slash,sysdy,slash,sysyr
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
          endif
         call      datetest
.ComLn2 holds last day of month from datetest routine

         move      Comln2 to sysdy
.
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      DATe TO DATEMASK
         CALL      PAINT
          Move      "L", Mode
.**************************TEST         
.          goto      pass3
          
          
          
.
         PACK      STR35,NTWKPATH1,"INCOME4a"
         PACK      STR45,NTWKPATH1,"INCOME4a"
         
.          Prepare   Output,"c:\work\income4a.tmp",exclusive   
          Prepare   Output,"\\nins1\e\data\income4a.tmp|10.10.30.103:502",exclusive   
          PREPARE   OUTPUT1,STR35,STR45,"8","250",exclusive   
          Prepare   CSVFIle,"\\nins1\e\data\income4a.CSV|10.10.30.103:502",exclusive   
          If        (Mode = "M")            .mailer
.begin patch 1.6
          write     CsvFile,seq;*cdfon,"order ID","order qty","Exch Qty","est Rev","Est A/P","est INC","Est N INC","Order date","qty bild","Revenue","Payables","Income","NIN INC":
                    "Inv Date","Mail Date","Qty Open","Rev Open","AP OPEn","Inc Open","NIN INC OPEN","Qty paid","Rev Paid","AP PAID","Inc Paid","NIN PAID","Date Paid":
.end patch 1.6
                    "Mlr ID","ID","List Name","Mlr Name","Division"                    ."
          Else
.begin patch 1.6
          write     CsvFile,seq;*cdfon,"order ID","order qty","Exch Qty","est Rev","Est A/P","est INC","Est N INC","Order date","qty bild","Revenue","Payables","Income","NIN INC":
                    "Inv Date","Mail Date","Qty Open","Rev Open","AP OPEn","Inc Open","NIN INC OPEN","Qty paid","Rev Paid","AP PAID","Inc Paid","NIN PAID","Date Paid":
.                    "Inv Date","Mail Date","Qty Open","Rev Open","Inc Open","Qty paid","Rev Paid","Inc Paid","Date Paid":
.end patch 1.6
                    "List ID","ID","List Name","Mlr Name","Division"                    ."
          endif
          write     CsvFile,seq;*cdfon,B1,B1,B1,B1,B1,B1,B1,B1:
                    B1,B1,B1,B1,B1,B1,B1,B1:
                    B1,B1,B1,b1,B1,b1,b1,b1
          

         MOVE      ONE TO PASS
.

DATEDIS
         KEYIN     *P10:10,*DV,DATEMASK," OK? ",*T05,STR1;
         CMATCH    no TO str1
         GOTO      BEGIN IF Not EQUAL
         KEYIN     *P10:10,*+,*jr,*zf,SYSMO,"/",*jr,*zf,SYSDY,"/",*jr,*zf,cc,*jr,*zf,SYSYR
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
         call      datetest
.ComLn2 holds last day of month from datetest routine
         move      Comln2 to sysdy
.
         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,cc,SYSYR
         pack      today from sysmo,slash,sysdy,slash,sysyr
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      date TO DATEMASK
         GOTO      DATEDIS
BEGIN    
          move      firstn,Nordfld
          move      c1,nordpath
          call      Nordkey

          Loop
          call      Nordks
          until     over
          reset     Runcodes
          scan      Olnum,runcodes
.          
          Goto      Skip if equal


.dh test July 2013
          Reset     EXFEELST
          scan      Olnum,ExFeeLst
          if        equal
          move      "018710",olnum                      .combine them
          endif
.end dh test July 2013
.          Reset     EXFEELST
.          scan      Olnum,ExFeeLst
.          Goto      Skip if equal

          pack      str5,"plxzX"         .skip pending orders and lcrs and cancelled orders with no billing
          scan      Ostat,str5
          Goto      Skip if equal
.begin patch 1.3
           pack         NMLDFLD1,"01X",OLRN
           clear   str8
           pack         str8,"99999999"
          move      "NMLDAIM",Filename
          pack      KeyLocation,"Key: ",NMLDfld1
           call         NMLDAIM
           loop
                  until over
                  if (NMLDDATE < str8)
                           move         NMLDDATE,str8
                  endif
             move   "NMLDKG",Filename
             pack   KeyLocation,"Key: ",NMLDfld1
                  call         NMLDKG
         repeat
         if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
         else
.Use current Mail Date
         endif
.end patch 1.3



.Temp Code to recreate #'s For prior months
.          if        (OOdtem = "07")                .July
.          if        (OOdtem = "08")                .August
.end temp

.begin patch 1.3
.begin patch 2013 jul 16   . match code used in all other programs
          reset    exfeelst
          scan      olnum in exfeelst             .List Management Exchange Fee?
.          if        (olnum = "024593" or Olnum = "018055")
          if        equal
          call      debug
          move      "018710",olnum                      .combine them
          move      c0,Osales10                   .force the figure for managementcommission numbers
          move      C6,Osales
          endif

.          if        (Olnum = "018710")             .List Management Exchange Fee
.          call      debug
.          move      c0,Osales10                   .force the figure for management commission numbers
.          move      C6,Osales
.          endif
.end patch 2013 jul 16   . match code used in all other programs
.begin patch 2013 jul 16  ..running charges are booked to Brokerage
.          reset     Runcodes
.          scan      Olnum,runcodes
.          if        equal
.          call      debug
.          move      c0,Osales10                   .force the figure for management commission numbers
.          move      C6,Osales
.          endif
.end patch 2013 jul 16  ..running charges are booked to Brokerage
.end patch 1.3
          pack      seller,osales10,osales

.begin patch 1.01
                    if        (Seller = "06" or Seller = "27")
.end patch 1.01
                              pack      str8 from oodtec,oodtey,oodtem,oodted
                              move      str8 to n8
.begin patch 1.1
                                        if        (n8 >= "20120101")
.                                        if        (n8 >= "20110601" & N8 <= "20120430")
.                                        if        (n8 >= "20090716")
.end patch 1.1
                                        Move      "L",Mode
                                        call      Write
                                        endif
                    Else                    
                              goto      skip
.                              pack      str8 from oodtec,oodtey,oodtem,oodted
.                              move      str8 to n8
..begin patch 1.1
.                                        if        (n8 >= "20130101")
.                                        goto      skip
.                                        Move      "M",Mode
.                                        call      write
.                                        endif
                    Endif          
.temp
.          else
.          endif
.end temp
Skip
          repeat
          
          weof      output,seq
          goto      pass2
write     
          write     output,seq;ordvars
          return
          
Pass2

.          Open      Output,"c:\work\income4a.tmp",exclusive
          Open      Output,"\\nins1\e\data\income4a.tmp|10.10.30.103:502",exclusive   

          Loop      
          Read      output,seq;ordvars
          until     OVer
          if        (olrn = "722762")
.          call      debug
          endif
         pack      seller,osales10,osales
          Rep       Zfill,Seller
         reset team1
         reset team2
         reset team3
         reset team4
.begin patch 1.1
          pack      str8 from oodtec,oodtey,oodtem,oodted
          move      str8 to n8
          if        (n8 >= "20120101" )
          move      "01",OutID1
          else
          move      "02",OutID1
          endif
.end patch 1.1


.if not below must be Susan
               move t3 to team
...........................
         scan  seller in team1
         if equal
               move t1 to team
          endif
         scan  seller in team2
         if equal
               move t2 to team
               reset seller
         endif
         scan  seller in team4
         if equal
               move t4 to team
               reset seller
         endif
...........................
...........................

          move      c1,ninvpath
          packkey   Ninvfld,Olrn
          rep       Zfill,Ninvfld
.          call      Debug
          CALL      NINVkey
          if        Over
          move      "U",Flag
          RESET     CANCODES               .RESET FORM POINTER.
          SCAN      OSTAT IN CANCODES       .CANCELLED?
                    if        Not equal
                    call      calcUnbild
.                    Move      Yes,DetailFlag      .include in report
.                    call      Update1
                    move      c0,qtybild    
                    move      c0,formar
                    move      C0,lrinc
.begin patch 1.6
                    move      c0,NinInc
.end patch 1.6
                    endif
          
          Else                          .billed lets scope it out
.Temp Code to recreate #'s For July & August
.          if        (invdtem <> "07")  July
.          if        (invdtem <> "08" )  August
.temp
.          move      "U",Flag
.          RESET     CANCODES               .RESET FORM POINTER.
.          SCAN      OSTAT IN CANCODES       .CANCELLED?
.                    if        Not equal
.                    call      calcUnbild
..                    Move      Yes,DetailFlag      .include in report
...                    call      Update1
.                    endif
.          else
.end temp
.do compute stuff
.          call      Debug
          move      "O",flag                                .open until proved otherwise
          MOVE      Olrn to nmrgfld
          REP       ZFILL IN NMRGFLD
          MOVE      Olrn to nshpfld
          REP       ZFILL IN NshpFLD
          move      c0 to nmrgrqty
          move      c0 to nmrgiqty
          move      c0 to nmrgnet
          move      no to mrgsw
          move      no to shipsw
          call      wipecvars
          CALL      NMRGKEY
                    if        not over
                    move      yes to mrgsw
                    endif
          CALL      nshpKEY
                    if        not over
                    move      yes to shipsw
                    endif
          call           NInvAcdRecClear
         
         CLEAR          NInvAcdfld
         packkey           NInvAcdFld from Invnum
         call           NinvAcdTst
         Call           NInvAcdRecLoad 
         CALL      COMPUTE
.build adj key
         pack      Nadjfld,olrn
         call      Nadjkey
                    if        not over
                    add       ASRECADJ,Formar        .ACCOUNTS RECEIVABLE ADJUSTMENT.
                    add       ASLRINC,lrinc         .LIST RENTAL INCOME ADJUSTMENT.
.begin patch 1.6
                    add       ASPAYAD1,AP         .List owner payables
                    add       ASNININC,NINinc         .NIN list RENTAL INCOME ADJUSTMENT.
.end patch 1.6
                    endif
.begin patch 1.6
.need to get INFO GROUP billing and sub out
          packkey   Tinvfld,olrn
          call      Tinvkey
          if        not over
          Move      TinvDOLR,form122
          mult       ".01" by form122
          sub       form122 from NINinc
          endif
          
.end patch 1.6


          cmatch    "P",statb
                    if        equal
.Do we need paid date.?
.Yes include only those Paid in month report is run
                    MOve      "P",Flag
                    Unpack    MLRPAYD,PCC,Pyy,PMM,Pdd
.                    move      CHK1DTEc,Pcc
.                    move      CHK1DTEy,Pyy
.                    move      CHK1DTEm,Pmm
.                    move      CHK1DTED,PDD
                    
                    Pack      Str4 from Pyy,PMM
                    call      trim using str6
                    Move      Str4,N4
                    pack      STr4A from SysYR,SysMo
                    call      trim using str4A
                    Move      Str4A,N5
                              if        (N4 <> N5)
..                              Move      No,DetailFlag      .do not include in report as paid because in different month
                              MOve      "O",Flag      .do not include in report as paid because in different month
.                              ElseIF   (PYY = sysyr & PMM = sysmo)   .paid in current month include in paid else in Billed                
..                              Move      Yes,DetailFlag      .include in report
                              endif
.dh goes wonky  10/01/09
                              if        (n4 <= n5)
                              move      yes,detailflag
                              else
                              move      no,detailflag
                              endif
                              
                    Else      
                    move      "O",Flag                              
                    endif
                    
.                    if        (flag = "P")
.                    add       c1,Paidcount
.                    add       qtybild,Paidnames
.                    add       Formar,part
.                    add       Lrinc,plrt
.                    Else
.                    add       c1,Opencount
.                    add       qtybild,OPennames
.                    add       Formar,Oart
.                    add       Lrinc,Olrt
.                    endif
.
.          add       c1,bildcount
.          add       qtybild,bildnames
.          add       Formar,Bart
.          add       Lrinc,Blrt
.          call      Update1
          Endif
.temp
.          endif
.temp
          call      Update1
Skipper          
          repeat

          Close     Output1
          Goto      Pass3

.=================================================================
.Update1  - write or update summary records
. are we in list or mailer mode?
Update1


          If        (Mode = "M")            .mailer

.fixit 2013 july 16
                    reset     exfeelst
                    scan      olnum,exfeelst
                    if        equal
                    move      "018710",olnum                
                    endif
.fixit 2013 july 16
                    if        (Olnum = "018710")             .List Management Exchange Fee
.                    call      debug
                    packkey   Ndatfld from Olnum
                    call      Ndatkey
                    packkey   outkey from Team,Olnum,outid1
                    move      OLSTNAME,Lname
                    MOve      Olnum,OutId       
                    Else          
                    pack      Mkey from Omlrnum,"000"
                    call      Nmlrkey
                    packkey   outkey from team,compnum,outid1
                    MOve      COmpnum,OutID
                    move      compcomp,Mlrname
.
                    packkey   Ndatfld from Olnum
                    call      Ndatkey
                    move      OLSTNAME,Lname
.
                    endif
          else

.fixit 2013 july 16
                    reset     exfeelst
                    scan      olnum,exfeelst
                    if        equal
                    move      "018710",olnum                
                    endif
.fixit 2013 july 16
          packkey   Ndatfld from Olnum
          call      Ndatkey
          packkey   outkey from Team,Olnum,outid1
          move      OLSTNAME,Lname
          MOve      Olnum,OutId
.
                    pack      Mkey from Omlrnum,"000"
                    call      Nmlrkey
                    move      compcomp,Mlrname
.
          endif
          call      WriteCsv
          read      output1,outkey;str1;
          If        Over
          packkey   Outkey from Team,Outid,outid1
          goto      Write1
          endif
.          if        (detailflag = yes)
          read      output1,outkey;Team,OutID,outid1,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BAPT,BLRT,BNINT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LName,Mlrname

          call      Prepcounter
          
          Update    Output1;Team,OutID,outid1,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BAPT,BLRT,BNINT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LName,Mlrname
.          Endif
          
          call      Cleanup
          return
WriteCsv
.turn back on 2013 july 16
.          reset     Runcodes
.          scan      Olnum,runcodes
.          Return    if equal
.turn back on 2013 july 16

. 2013 july 16
          Reset     EXFEELST
          scan      olnum,exfeelst
          if        equal
          move      "018710",olnum                      .combine them
          endif
. 2013 july 16
.          if        (olnum = "024593" or Olnum = "018055")
.          Return    
.          endif
          
          Move      Team,n2
          load      str35 with n2 from NAME1,NAME2,NAME3,NAME4,Name5
          clear     Pdate
          Clear     Bdate
          Pack      odate from oodtem,"/",oodted,"/",oodtec,oodtey
.new 04feb13
          Clear     mldate
          Pack      mldate from omdtem,"/",omdted,"/",omdtec,omdtey


.dh wonky
          if        (detailflag = yes)
          move       "P",Flag
          endif
          if        (flag = "U")                               .Unbilled
.begin patch 1.6
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,ungross,c0,unbilinc,c0,Odate,c0,c0,c0,c0,c0,Bdate,mldate,c0,c0,c0,c0,c0,c0,c0,c0,c0,c0:
.end patch 1.6
                    Pdate,outid,outid1,lname,Mlrname,str35
          Elseif    (flag = "O")                                  .billed but open                    
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
.begin patch 1.6
.          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,mldate,c0,c0,c0,qtybild,formar,lrinc:
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,AP,Lrinc,NININC,Odate,qtybild,formar,AP,lrinc,NININC,Bdate,mldate,c0,c0,c0,c0,c0,qtybild,c0,c0,c0,c0:
                    Pdate,outid,outid1,lname,Mlrname,str35
.end patch 1.6
          Elseif    (flag = "P")                               .billed and paid
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          Pack      Pdate from Pmm,"/",Pdd,"/",Pcc,Pyy
.begin patch 1.6
.          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,mldate,c0,c0,c0,qtybild,formar,lrinc:
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Ap,Lrinc,NININC,Odate,qtybild,formar,ap,lrinc,nininc,Bdate,mldate,c0,c0,c0,c0,c0,qtybild,formar,AP,lrinc:
                    NININC:
                    Pdate,outid,outid1,lname,Mlrname,str35
.end patch 1.6
          if        (detailflag = yes)
          move       "O",Flag   .restore flag
          move      No,Detailflag
          endif
          endif          
          REturn
          
Write1
.          if        (detailflag = yes)
          call      Prepcounter
          Write     output1,outkey;Team,OutID,outid1,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BAPT,BLRT,BNINT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LName,Mlrname
.          Endif
          call      Cleanup
          return

PrepCounter
          add       c1,ordcount
          move      Oqty,n9
          add       N9,ordnames

          if        (flag = "U")          
          add       UnGross,ordar
          add       Unbilinc,ordlr

          elseif    (flag = "O")
          add       c1,Opencount
          add       QTYBILD,Opennames
          add       Formar,Oart
          add       LRinc,Olrt

          elseif    (flag = "P")
          add       c1,Paidcount
          add       QTYBILD,Paidnames
          add       Formar,Part
          add       LRinc,Plrt
          endif

          if        (flag = "O" | Flag = "P")
          add       c1,bildcount
          add       QTYBILD,Bildnames
          add       Formar,Bart
          add       Lrinc,Blrt
.begin patch 1.6
          add       aP,BaPt
          add       NINinc,BNINt
.end patch 1.6
          endif
          return
          
Cleanup
          Clear     Flag
          Clear     Oqty
          move      c0,qtybild
          move      c0,qtybild
          move      c0,formar
          move      c0,lrinc
          move      c0,ungross
          move      c0,unbilinc
.
          Move      c0,ordcount
          move      c0,ordnames
          move      c0,ordar
          move      c0,ordlr
          move      c0,bildcount
          move      c0,bildNames
          move      c0,Bart
          Move      c0,Blrt
          move      c0,paidcount
          move      c0,paidnames
          move      c0,part
          move      c0,plrt
          move      c0,opencount
          move      c0,opennames
          move      c0,Oart
          move      c0,Olrt
          move      c0,NotXnames
          clear     LName
          clear     MlrName
.begin patch 1.6
          move      c0,Bapt
          move      c0,Bnint
.end patch 1.6
          return
          

.
.end patch 1.9
.
READBLTO
.         CALL      NBILKEY
.         GOTO      SWAP IF OVER
         RETURN
SWAP
.
.         MOVE      MNAME TO BILNAME
         MOVE      MCCTO TO BILNAME
.Patch2.1
.         MOVE      MCOMP TO BILCOMP
          move      mcomp to CONAME
.EndPatch2.1
         MOVE      MADDR TO BILADDR
         MOVE      MCITY TO BILCITY
         MOVE      MSTATE TO BILSTATE
         MOVE      MZIP TO BILZIP
         RETURN
.
.
*............................................................................
.ok we have data lets do something
Pass3
.          call      Debug
.===========================================================================
          Weof      CsvFIle,seq
          Close     Csvfile
          OPen      OUTPUT1,"\\nins1\e\data\income4a",exclusive   

.                              PRTOPEN PrFile,"PDF995","Income4a"
.                              PRTCLose Prfile
.win7 rc is misbehaving :(                              
                              PRTOPEN PrFile,"PDF:","c:\work\pdf\Income4a.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

          move      c0,n8
          move      c1 to RowCount
          clear     PgCnt
          call      Page
.===========================================================================
          move      "0000000000",outkey
          read      output1,outkey;;
          Clear     HoldTeam          

          Loop
          readks    Output1;Team,OutID,outid1,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BAPT,BLRT,BNINT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LName,Mlrname
          Until     over
          if        (holdteam = "")
          call      ClearTots
          move      Team,HOldteam       .prep for break
          endif
          if        (team <> HoldTeam)
          call      TeamBreak
          endif
          
.calc totals for now  add detail option later
          if        (outid = "018710")              .LM ex fee
.          call      debug
          Add       ordcount,Toteords          
          Add       ordnames,Totenames
          add       BildCOunt,ToteBords
          add       BildNames,ToteBnames
          add       BArt,ToteBar
          add       BLRT,TOTeBLR
          add       PaidCOunt,TotePords
          add       PaidNames,TotePnames
          add       PART,TotePar
          add       PLRT,TOTePLR
          add       OpenCOunt,ToteOords
          add       OPenNames,ToteOnames
          add       OART,ToteOar
          add       OLRT,ToteOLR
          else
          Add       ordcount,Totords          
          Add       ordnames,Totnames
          Add       NotXnames,TotXnames
          add       OrdAr,TotUnAR
          add       OrdLR,TotUnLR
          add       BildCOunt,TotBords
          add       BildNames,TotBnames
          add       BArt,TotBAR
          add       BLRT,TotBLR
          add       PaidCOunt,TotPords
          add       PaidNames,TotPnames
          add       PART,TotPAR
          add       PLRT,TOTPLR
          add       OpenCOunt,TotOords
          add       OPenNames,TotOnames
          add       OART,TotOAR
          add       OLRT,TOTOLR
          endif

          Repeat
          call      TeamBreak          
          GOto      Eoj          
TeamBreak
.print the goodes for the team and clear
          Calc      GrandAR = (TotUnAR+TotBAR)
          Calc      GrandLR = (TotUnLR+TotBLR)
.          if        (totPords = c0)                      .totpnames is picking up garbage somewhere
.          move      c0,TotPnames
.          move      c0,TotPAR
.          move      c0,TOTPLR
.          endif
.          calc      N9 = (TotBnames + Totnames)                       .add in billed for the total
          Move      HOldTeam,n2
          load      str35 with n2 from NAME1,NAME2,NAME3,NAME4,Name5
          prtpage   PRfile;*pcolumn:row,*font=font12,*ll,*ALIGNMENT=*Center,str35;
          add       eightlpi,row
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders Placed":
                    *pcolumn3:Row,Totords,*pcolumn4:Row,TotNames,*pcolumn5:Row,GrandAr,*pcolumn6:Row,GrandLR:
                    *pColumn7:row,TotXnames
          add       eightlpi,row
          add       eightlpi,row
          calc      N9 = (totords - totBords)
          move      N9,Notords
          calc      N9 = (Totnames - TotBnames)              take them back out for unbilled
          move      N9,Notnames
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders NOT Billed":
                    *pcolumn3:Row,Notords,*pcolumn4:Row,Notnames,*pcolumn5:Row,TotUnAR,*pcolumn6:Row,TotUnLR
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders Invoiced":
                    *pcolumn3:Row,TotBords,*pcolumn4:Row,TotBnames,*pcolumn5:Row,TotBAR,*pcolumn6:Row,TotBLR
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Open":
                    *pcolumn3:Row,TotOords,*pcolumn4:Row,TotOnames,*pcolumn5:Row,TotOAR,*pcolumn6:Row,TOTOLR
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Paid":
                    *pcolumn3:Row,TotPords,*pcolumn4:Row,TotPnames,*pcolumn5:Row,TotPAR,*pcolumn6:Row,TOTPLR
          add       eightlpi,row
          add       eightlpi,row
          if        (n2 = c4)          .IA List management
          Calc      GrandAR = (totebar)
          Calc      GrandLR = (toteBlr)
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"EXfee Orders Placed":
                    *pcolumn3:Row,Toteords,*pcolumn4:Row,ToteNames,*pcolumn5:Row,GrandAr,*pcolumn6:Row,GrandLR
          add       eightlpi,row
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"EXfee Orders Invoiced":
                    *pcolumn3:Row,ToteBords,*pcolumn4:Row,ToteBnames,*pcolumn5:Row,ToteBAr,*pcolumn6:Row,toteBLR
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Invoices Open":
                    *pcolumn3:Row,TotEOords,*pcolumn4:Row,TotEOnames,*pcolumn5:Row,TotEOAR,*pcolumn6:Row,TotEOLR
          add       eightlpi,row
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Invoices Paid":
                    *pcolumn3:Row,TotEPords,*pcolumn4:Row,TotEPnames,*pcolumn5:Row,TotEPaR,*pcolumn6:Row,TotEPLR
          add       eightlpi,row
          add       eightlpi,row
          endif          

          call      ClearTots
.          call      Debug
          move      Team to N2
          if        (N2 = C3 or n2 = C4)
          call      Page
          endif
          Move      Team,Holdteam
          return

ClearTots
          Move      C0,Totords          
          Move      C0,Totnames
          Move      C0,TotUnAR
          Move      C0,TotUnLR
          Move      C0,TotBords
          Move      C0,TotBnames
          Move      C0,TotBAR
          Move      C0,TotBLR
          Move      C0,TotPords
          Move      C0,TotPnames
          Move      C0,TotPAR
          Move      C0,TOTPLR
          Move      C0,TotOords
          Move      C0,TotOnames
          Move      C0,TotOAR
          Move      C0,TOTOLR
          move      c0,GrandAR
          move      c0,GrandLR
          move      c0,form102
          move      c0,n9
          Move      C0,Toteords          
          Move      C0,Totenames
          Move      C0,TotEBords
          Move      C0,TotEBnames
          Move      C0,TotEBar
          Move      C0,TotEBLR
          Move      C0,TotEPords
          Move      C0,TotEPnames
          Move      C0,TotEPar
          Move      C0,TotEPLR
          Move      C0,TotEOords
          Move      C0,TotEOnames
          Move      C0,TotEOar
          Move      C0,TotEOLR
          move      c0,NotXnames
          move      C0,TotXnames

          return

*............................................................
.
ABORT
         TRAPCLR   F5
          shutdown  "CLS"
         stop
GETFIGURES

.==========================================================================================
.Begin Print Program
START
.===================================================================================
Page
        clear Rowcount
        add c1 to Pgcnt
        if          (pgcnt > c1)
        prtpage prfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
        Else
        prtpage prfile;*UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
        endif
        clear   row
        move    "300",row
        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
        prtpage prfile;*pTitleH:row,*ALIGNMENT=*Center,*font=font12,"Names in the News - Revenue by Division ";
        prtpage prfile;*pTitleH2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clear str10
          pack      str10 from sysmo,slash,sysdy,slash,cc,sysyr
        prtpage prfile;*font=font12,str10;
        add     eightlpi,row
        add     eightlpi,row
          add     eightlpi,row
        add     eightlpi,row
.Headers
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title2,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title4,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title5,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*Right,*font=font14,*boldon,"Exch. Names",*uloff,*boldoff;
       add     eightlpi,row
       add     eightlpi,row

          Return

.=====================================================================
EOJ
          PrtCLose  Prfile
          goto      Finish




             salesinclistview.SortColumn using *Column=0,*Type=3,*Column1=6,*Type1=4
             move c1 to RowCount
             clear PgCnt
                        clear newpg
             move c0 to n9
             salesinclistview.GetItemCount giving result
             sub c1 from result
Headers
        clear Rowcount
        add c1 to Pgcnt
        prtpage pslsfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                *ORIENT=*Portrait;
        clear   row
        move    "300",row
        prtpage pslsfile;*pcolumn:row,*font=font12,"Confidential";
.START PATCH 2.92 REPLACED LOGIC
.        prtpage pslsfile;*pTitleH3:row,*ALIGNMENT=*Center,*font=font12,"Names in the News California Inc - Monthly Billing By Client ";
        prtpage pslsfile;*pTitleH3:row,*ALIGNMENT=*Center,*font=font12,"Names in the News - Monthly Billing By Client ";
.END PATCH 2.92 REPLACED LOGIC
        prtpage pslsfile;*pTitleH4:row,*ALIGNMENT=*Left,*font=font12,"Date:";
.        clock timestamp,str8
.        unpack str8,str2,yy,mm,dd
        clear str10
.        pack  str10,mm,slash,dd,slash,str2,yy
          pack      str10 from sysmo,slash,sysdy,slash,cc,sysyr
        prtpage pslsfile;*font=font12,str10;
        add     eightlpi,row
        add     "50",row
.        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
.Headers
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title8,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
.       add     eightlpi,row
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title2,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title8a,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage pslsfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Client",*ULOFF,*boldoff;
.       prtpage pslsfile;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=font14,*boldon,*ulon,Title1,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,*ulon,Title2a,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,*ulon,Title8b,*uloff,*boldoff;
       add     eightlpi,row
       add     eightlpi,row
       add c1 to RowCount
Salesloop
       loop
         until (n9 > result)
.        for n9,"0",result
                    salesinclistview.GetItemText giving str1 using n9,0
                    compare n9 to c0
                    if equal
                                        move str1 to holdtm
                    endif
                    if (str1 <> holdtm)
                                        move str1 to holdtm
                                        goto SalesLastPage
                    endif
                    move holdtm to n2
         load str25 with n2 to NAME1,NAME2,NAME3,NAME4,Name5
         prtpage pslsfile;*pTitleH3:485,*font=font12,*ll,*ALIGNMENT=*Center,str25;
         salesinclistview.GetItemText giving str45 using n9,2

.IF AR and Adj Total LR Income are both Zero skip
         clear str13
         CLEAR LVARTOT
         salesinclistview.GetItemText giving str13 using n9,3
         clear n13
         call trim using str13
         move str13 to LVARTOT
         add LVARTOT to n13

         clear LVFINLRTOT
         clear n13a
         salesinclistview.GetItemText giving str13 using n9,6
         call trim using str13
         move str13 to LVFINLRTOT
         add LVFINLRTOT to n13a
         if ((n13 = C0)&(n13a = C0))
               add c1 to n9
               goto salesloop
         endif


.==================================================================
.Client
         prtpage pslsfile;*pcolumn:row,*font=font8,*ll,*ALIGNMENT=*Left,str45;
.Datestamp
         prtpage pslsfile;*pcolumn1:row,*font=font8,*ll,*ALIGNMENT=*Left,stamp;
         clear str9
.AR
         clear str13
         CLEAR LVARTOT
         salesinclistview.GetItemText giving str13 using n9,3
         clear n13
         call trim using str13
         move str13 to LVARTOT
         add LVARTOT to n13
.move str13 to n13
         add n13 to SLSARTOT
         prtpage pslsfile;*pcolumn3:row,*font=font8,*ll,*ALIGNMENT=*RIGHT,n13;
.Adj Total LR Income
         clear LVFINLRTOT
         clear str13
         salesinclistview.GetItemText giving str13 using n9,6
         clear n13
         call trim using str13
         move str13 to LVFINLRTOT
         add LVFINLRTOT to n13
.move str13 to n13
         add n13 to SLSFINLRTOT
         prtpage pslsfile;*pcolumn4:row,*font=font8,*ll,*ALIGNMENT=*RIGHT,n13;
         add eightlpi,row
         add "50",row
         add c1 to RowCount
         add c1 to n9
.Reduced Rows on page to make room for Copyright label
.Max # of clients on a page is 46
           if (ROWCOUNT = "47")
           add     eightlpi,row
           add     eightlpi,row
           move "10200",row
           prtpage pslsfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Page# ";
           prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
           goto Headers
           endif
           repeat
SalesLastPage
.Write total on same page if less than 46 rows
       if (ROWCOUNT < "46")
                 goto Salestotals
       else

.Added to correct page # and page label for next to last page
.========================================================================
           move "10200",row
           prtpage pslsfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.========================================================================
                  move c1 to newpg
                  add  c1 to pgcnt
                    prtpage pslsfile;*NEWPAGE:
                                         *UNITS=*HIENGLISH:
                                       *ORIENT=*PORTRAIT;
                  goto SalesTotals
       endif
SalesTotals
.Grand Totals for clients
       if (newpg = c1)
                    move "1380",row
       else
                          add     eightlpi,row
            prtpage pslsfile;*p3400:row,*pensize=10,*line=10300:row;
            add     "30",row
       endif
       prtpage pslsfile;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font9,*boldon,*ulon,"Grand Total",*ALLOFF;
                     move slsartot to str13
                     call trim using str13
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,SLSARTOT;
                     move slsfinlrtot to str13
                     call trim using str13
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,SLSFINLRTOT;
       add     eightlpi,row
       add     eightlpi,row
.Total Records Printed
.       prtpage pslsfile;*pcolumn:row,*ALIGNMENT=*Left,*font=font9,"Records Printed: ";
.       move n8 to str8
.       call trim using str8
.       prtpage pslsfile;*ALIGNMENT=*Left,*font=font9,n8;
.Adds Footer for last page
       move "10200",row
       prtpage pslsfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                     clear slsfinlrtot
                     clear slsartot

       if (n9 > result)
.                      prtclose pslsfile
.                      PRTPLAY "c:\work\salesincome2.lst","\\nins1\Laser8"
.                      PRTPLAY "c:\work\salesincome2.lst","\\nins1\Laser8"
                     else
                                         clear newpg
                     clear pgcnt
                      goto Headers
                     endif
                      prtclose pslsfile
       DISPLAY   *P10:10,*EF,"Sales Job Finished.....Printing on Laser8!!!!!!!!!!!!!"
       PAUSE     c1
       erase "c:\work\salesincome2.lst"







finish

          Shutdown  "CLS"
         STOP
calcUnbild
.          call      Debug
          move      No,LSTMSW
          if        (seller = "06" | seller = "27")
          Move      "L",Mode
          move      Yes,Lstmsw
          Else
          MOve      "M",MOde
          endif
          MOVE      C0 TO FORM92
          MOVE      C0 TO UNBILAMT
          MOVE      C0 TO UNBILINC
          Move      C0 to UnGross
          MOVE      C0 TO FORM52
          MOVE      C0 TO AR
          move      c0 to commper
          Move      c0 to NotXNames
          clear     specl
          PACK      MKEY FROM OMLRNUM,OCOBN
          REP       ZFILL,MKEY
          CALL      NMLRKEY
.
.
READSHIP  Move      C0,squant
          MOVE      OLRN TO NSHPFLD
          CALL      NSHPKEY
          if        Not over
          MOVE      C0 TO N9
          MOVE      SQUANT TO N9
          COMPARE   C0 TO N9
                    IF        NOT EQUAL
                    MOVE      SQUANT TO OQTY
                    ENDIF
         Endif
READMRG
          MOVE      OLRN TO NMRGFLD
          rep       zfill in nmrgfld
          MOVE      C1 TO NMRGPATH
          CALL      NMRGKEY
          IF        Not OVER
          move      nmrgiqty to oqty
          Endif


         CALL      GETCARD
         SUB       FORM92 FROM FORM92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
         move       no to specl


         cmatch    yes to specl
         if        equal
         goto      calcsp       .got price fron special subroutine
         endif
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if not over
               move           NSEL2NAME,O2DES
              else
               unpack         OPPM,str3,str2
               pack           str6,str3,".",str2
               rep            zfill,str6
               move           str6,NSEL2PRICE
              endif

              MOVE            NSEL2PRICE,FORM52
               if             (form52 = 0 & lstmsw <> "Y" & OELCODE <> "3" & OELCODE <> "3")
.               call           debug
               endif
calcsp
         MULT       FORM52 BY FORM92       .base price
         MOVE       FORM92 TO UNBILINC
         move       Form92,UnGross
         RESET      EXCODES
         SCAN       OELCODE IN EXCODES             EXCHANGE ?
         GOTO       OKEX IF EQUAL

          if        (onetfm="M" | onetfm="F")      .discounted order???
          move      onetper to form32              .yes calc it
          mult      ".01" by form32
.
          MOVE      OQTY TO FORM92
          DIV       THOUS INTO FORM92
          mult       form32 by form92               .percentage of billable names
          move      form92 to net92                 .save it
          MULT      FORM52 BY FORM92
          MOVE      FORM92 TO UNBILINC               .base rental
          MOVE       FORM92 TO UNGross
          endif

         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
         MOVE      C0 TO FORM92
         MOVE      OEXQTY TO FORM92
         COMPARE   C0 TO FORM92            PURE EXCHANGE ?
          IF        EQUAL                 YES.
          MOVE      C0 TO QTYCHK
          MOVE      NO TO SPLITSW
          MOVE      OQTY TO QTYCHK
          MOVE      QTYCHK TO FORM92
          MOve      Qtychk,NotXnames
....................

          CMATCH    YES TO LSTMSW
                    IF        EQUAL
                    MOVE      C0 TO UNBILINC
                    Move      C0,UnGross
                    Return
                    ELSE
                     GOTO      GETPRICE
                     ENDIF
....................
          ELSE
          MOVE      YES TO SPLITSW            .split
          CMATCH    YES TO LSTMSW
.............................
                    IF        EQUAL
                    MOVE      C0 TO UNBILAMT
                    GOTO      RENTPART
                    ELSE
                    MOVE      C0 TO QTYCHK
                    MOVE      OEXQTY TO QTYCHK
                    MOVE      QTYCHK TO FORM92
                    MOve      Qtychk,NotXnames
                    GOTO      GETPRICE
                    ENDIF
............................
          ENDIF
GETPRICE

         cmatch    yes to specl
         if        equal
         goto      calce
         endif


         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         GOTO      CALCE
         ENDIF

         MOVE      C8 TO FORM52

CALCE
         DIVIDE    THOUS INTO FORM92
         MULTIPLY  FORM52 BY FORM92
         MOVE      FORM92 TO UNBILAMT
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
         MOVE      FORM92 TO UNBILINC
          MOve      Form92 TO UNGross
          REturn
         ENDIF
.
RENTPART
.
         MOVE      C0 TO FORM92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO FORM92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM FORM92           GET RENTAL PORTION
         MULT      ".001" BY FORM92
         cmatch    yes to specl
         if        equal
         goto      calcsp2
         endif

         compare   c0 to card$
         if        equal
         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         else
         move      card$ to form52
         endif
         MULT      FORM52 BY FORM92
         
          MOve      Form92 TO UNGross
.
          CMATCH    YES TO LSTMSW
          IF        EQUAL
          MULT      ".1" BY FORM92
          ELSE
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,form92
         ENDIF

          ADD       FORM92 TO UNBILAMT
          MOVE      UNBILAMT TO UNBILINC
          RETURN
.
calcsp2  MULT      FORM52 BY FORM92
         ADD       FORM92 TO UNBILAMT
          MOVE      UNBILAMT TO UNBILINC
          MOve      UNBILAMT TO UNGross
         Return
.
GETCARD  MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
              if (NDATCONV = "1")
                              pack           NSELFLD1,"01X",LSTNUM
                              pack           NSELFLD2,"021XBASE"
                              move           "NSELAIM",Location
                              pack           KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call           NSELAIM
                              if not over
                                             if (NSELEXC <> "2")
                                                            move           C0,card$
                                                            move           NSELPRICE,card$
                                             endif
                              else
                                             goto DataCheckText
                              endif
              else
DataCheckText
               pack           NTXTFLD,LSTNUM,"1"
               move           "NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               if not over
                              move           NTXTTEXT,text1
                              SCAN           "EXCHANGE ONLY" IN TEXT1
                              RETURN IF EQUAL                 NO USABLE $ RETURN
                              RESET          TEXT1
                              SCAN           "$" IN TEXT1
                              RETURN IF NOT EQUAL        NO USABLE $ RETURN
                              BUMP           TEXT1 BY 1
                              PACK           STR2 FROM TEXT1
                              move           c0 to card$
                              MOVE           STR2 TO card$
                              SCAN           "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
                              RETURN IF NOT EQUAL            *YES.
                              CLEAR          STR2
                              BUMP           TEXT1 BY 1
                              PACK           STR2 FROM TEXT1       *NO, NOW WE DO!
                              move           c0 to card$
                              MOVE           STR2 TO card$
               else
                              clear          text1
               endif
              endif
         RETURN
RENT
          cmatch    yes to specl
          if        equal
          REturn
          endif

         CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,unbilinc
         ENDIF
.
          RETURN




          INCLUDE   NORDIO.inc
          include   compio.inc
          include   cntio.inc
          Include   ninvacdio.inc
          INCLUDE   NINVIO.inc
          INCLUDE   NBILIO.inc
          INCLUDE   NadjIO.inc
          INCLUDE   GNXTIO.INC
          INCLUDE   COMPUTE.inc
          include   nacdio.inc
          include   nownio.inc
          include   nshpio.inc
          Include   Ndatio.inc
          INCLUDE   NDAT3IO.INC
          INclude   NSelio.inc.
          INCLUDE   NSEL2IO.INC
          INclude   Ntxtio.inc.
          inc       nmrgio.inc
.begin patch 1.3
               include        nmldio.inc
.end patch 1.3
.begin patch 1.6
               include        TINVIO.inc
.end patch 1.6
          INCLUDE   COMLOGIC.inc


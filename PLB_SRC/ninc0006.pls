...............................................................................
. Ninc0006 - YTD TOTAL revenue for Managed lists
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
         include   nmlddd.inc
          include   Commdd.inc

shipsw    dim       1
mrgsw     dim       1
.
release  init      "1.02"          DLH     .added pass2count
reldate   Init      "2016 April 1"
.release  init      "1.01"          DLH     .added discount info onetper,COMMPCT,ppm
.reldate   Init      "2014 December 3"
.release  init      "1.00"          DLH     .New
.reldate   Init      "2014 March 28"
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
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
OUTPUT1   IFILE     KEYLEN=8,FIX=290
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
Flag      Dim       1                   .records type being processed 'U'billed orders 'B'illed, 'O'pen, 'P'aid 'l'ate
.begin patch 1.05                       .'p'aid previous period
.////////////////////////////////////testing list mode July 10 2012



Mode      Init      "L"                        .'M'ailer or 'L'ist








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
STR6A     dIM       6

BildAR    FOrm      9.2       .billed ar
BildLR    Form      9.2       .billed rev

PaidAR    Form      9.2       .paid ar
PaidLR    Form      9.2       .paid rev
UNBILINC  FORM      9.2       
UNBILAmt  FORM      9.2
Form102   Form      10.2
GrandAR      FORM      10.2    
GrandLR      FORM      10.2    
Mask$102   Init      "$$$$,$$$,$$Z.ZZ"
Mask$92    Init      "$$$,$$$,$$Z.ZZ"
Mask9     Init      "ZZZ,ZZZ,ZZZ"
Str11a    Dim       11
Str11b    Dim       11
Str15a    Dim       15
Str16a    Dim       16
.end patch 1.02
OrderCount form       9
ListCount  form       9
Pass2Count form       9

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
.begin patch 1.05
TotEppords   Form      9        .lm exchange - paid prev
TotEppnames  Form      9        .lm exchange - paid prev
TotEppAR     FOrm      9.2        .lm exchange - paid prev
TOTEppLR     FOrm      9.2        .lm exchange - paid prev
.end patch 1.05
.begin patch 1.04
TotELords   Form      9        .lm exchange - Late
TotELnames  Form      9        .lm exchange - Late  
TotELAR     FOrm      9.2        .lm exchange - Late
TOTELLR     FOrm      9.2        .lm exchange - Late
.end patch 1.04
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
.begin patch 1.05
Totppords   Form      9            .paid prev
Totppnames  Form      9            .paid prev
TotppAR    FOrm      9.2            .paid prev
TOTppLR    FOrm      9.2            .paid prev
.end patch 1.05
.begin patch 1.04
TotLords   Form      9            .paid late
TotLnames  Form      9            .paid late
TotLAR    FOrm      9.2            .paid late
TOTLLR    FOrm      9.2            .paid late
.begin patch 1.04
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
DetailFlag          Dim       1                   .If yes include in paid count for PDF report

Pdate     Dim       10
Odate     Dim       10
Bdate     Dim       10
MLDate     Dim       10
........................................
.OUTPUT1.
........................................
OutKey    DIm       8           1-8     team+ list or company "mailer" number
Team      Dim       2           1-2
OutID     Dim       6           3-8    list or company "mailer" number
OrdCount  Form      5           9-13          
Ordnames  Form      9          14-22 
NotXnames Form      9          23-31          . unbilled exchange names
OrdAr     Form      10.2       32-44    .est a/r
OrdLR     Form      10.2       45-57    .Est LR
BildCount Form      5          58-62
bildNames Form      9          63-71 
BART      FORM      10.2       72-84     billed a/r
BLRT      FORM      10.2       85-97     billed lr
Opencount form      5          98-102
Opennames form      9         103-111
OART      FORM      10.2      112-124    OPen      A/R
OLRT      FORM      10.2      125-137    OPEn      L/R
paidcount form      5         138-142
paidnames form      9         143-151
PART      FORM      10.2      152-164    Paid      A/R
PLRT      FORM      10.2      165-177   Paid      L/R
LateCount Form      5         178-182
LateNames form      9         183-191
Lart      Form      10.2      192-204
LLrt      Form      10.2      205-217
PRevCount Form      5         218-222
PRevNames form      9         223-231
ppart      Form      10.2     232-242
PPlrt      Form      10.2     245-255
Name      Dim       35        256-290     list or mailer name
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
Team1   init    "  "         .NIN div brokerage  .
...................test
Team2     Init      "06"
...................test
Team3   init    "               "            .AL TEAM  MM(10) added 8/05
Team4    Init      "  "
Team5   init    "00-00-00"            .Uncategorized
T1      init    "01"                                                     .ND
T2      init    "02"                                                     .ND LM
T3      init    "03"                                                     .SA
T4      init    "04"                                                     .SK
NAME1   init    "Schoevaars New Business"
NAME2   init    "Names in the News Management"
NAME3   init    "Schoevaars New Business"
NAMe4   init    "Names in the News Management"
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
FIRSTN   INIT      "650000"  Start here - what is the firstn lr really?
...................................................
.
BEGINV   DIM       6
specl    dim       1
.
brker    dim       1
sellerv  dim       2

COMMRecCount        form           3
COMMRec             REcord         (50)                                   ;artificial cap
COMMNumRec          Dim            6             01-06             Company number
COMMSTARTRec        FORM           8             07-14              start date ccyymmdd
COMMSTOPRec         FORM           8             15-22              stop date ccyymmdd
                    recordend
RecCount            form           3                        .total number loaded
StartDate           Form      8
StopDate            Form      8
OnTheList           Dim       1
Qualified           Dim       1
DaystoPAy           Form      5
ExFeeArr              dim        6(3)                             .currently 3
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         clock     timestamp to timestamp
         unpack    timestamp into cc,sysyr,sysmo,sysdy
          Match     "NINC0006" TO INPNAME
          if        equal                         .chained from dsinit
          Unpack    Today into sysmo,slash,sysdy,slash,sysyr
          else
         pack      today from sysmo,slash,sysdy,slash,sysyr
          endif
         pack      date from sysmo,slash,sysdy,slash,cc,sysyr
         MOVE      "NINC0006" TO PROGRAM
         MOVE      "Total List Revenue " TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C2 TO NINVPATH        .SET ACCESS TO ISI BY INV#.
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NMLRPATH
         move      c3 to nmlrlock
         move      c3 to nordlock
         move      c3 to nInvlock
         move      c3 to COMMlock
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
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
         call      datetest
.ComLn2 holds last day of month from datetest routine

         move      Comln2 to sysdy
.
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      DATe TO DATEMASK
         CALL      PAINT
.**************************TEST         
.          goto      pass3
          
          
          
.
         PACK      STR35,NTWKPATH1,"INCOME6"
         PACK      STR45,NTWKPATH1,"INCOME6"
         
          Prepare   Output,"c:\work\Income6.tmp",exclusive   
          PREPARE   OUTPUT1,STR35,STR45,"8","290",exclusive   
          Prepare   CSVFIle,"c:\work\Income6.CSV",exclusive   
          write     CsvFile,seq;*cdfon,"order ID","order qty","Exch Qty","est Rev","est INC","Order date","qty bild","Revenue","Income":
                    "Inv Date","Mail Date","Qty Open","Rev Open","Inc Open","Qty paid","Rev Paid","Inc Paid","Date Paid":
                    "Days to Pay","Qualified","List ID","List Name","Mailer","Discount","Comm %","PPM","LM Comm","Full Comm","Diff"                    ."
          

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
...........................Load table here
          move      c4,ndatpath              .read only managed files
          move      "000000",ndatfld
           call       ndattst                    .position at front of file  
          Loop
NewList
          Call      Ndatks
          until     over
           add        c1,listcount
           display    *p10:10,*el,"List count ",listcount," List ##",lstnum
           clear      NORDFLD
           clear      NORDFLD1
           clear      NORDFLD3
           clear      NORDFLD4
           clear      NORDFLD5
           packkey   Nordfld2,"02X",lstnum
           call       Nordaim
           goto       Newlist if over
           add        c1,ordercount
           display    *p10:12,*el,"order count ",ordercount," order ##",olrn
           call       checkit
                      loop
                      call       Nordkg
                      until      over
                      add        c1,ordercount
                      display    *p10:12,*el,"order count ",ordercount," order ##",olrn
                      call       checkit
                      repeat     
                      
          repeat
.get management exfee's
           explode    exfeelst,";",ExFeeArr 
           MOVE       "Management List Exchange Fee",OLSTNAME
           for        n1 from "1" to "3" using c1
           move       Exfeearr(n1),lstnum
           clear      NORDFLD
           clear      NORDFLD1
           clear      NORDFLD3
           clear      NORDFLD4
           clear      NORDFLD5
           packkey   Nordfld2,"02X",lstnum
           call       Nordaim
           if         not over
           add        c1,ordercount
           display    *p10:12,*el,"order count ",ordercount," order ##",olrn
           call       checkit
                      loop
                      call       Nordkg
                      until      over
                      add        c1,ordercount
                      display    *p10:12,*el,"order count ",ordercount," order ##",olrn
                      call       checkit
                      repeat     
           endif
                      
          repeat


          weof      output,seq
          goto      pass2

.................

Checkit

          reset    exfeelst
          scan      olnum in exfeelst             .List Management Exchange Fee?
          if        equal
          call      debug
          move      "018710",olnum                      .combine them
          endif


          pack      str5,"plxzX"         .skip pending orders and lcrs and cancelled orders with no billing
          scan      Ostat,str5
          return      if equal

          move      c1,ninvpath
          packkey   Ninvfld,Olrn
          rep       Zfill,Ninvfld
          CALL      NINVkey
             if       not over
.          return    if Over
.
           pack      str8 from INVDTEc,INVDTEy,INVDTEm,INVDTEd
           move      str8 to n8
                   
                      if        (n8 >= "20140101")
                      call      write
                      endif
           else
           pack      str8 from OmDTEc,OmDTEy,OmDTEm,OmDTEd
           move      str8 to n8
                   
                      if        (n8 >= "20140101")
                      call      write
                      endif
           endif
           return

          weof      output,seq
          goto      pass2

.................
          
write     
          write     output,seq;ordvars
          return
.................
          
Pass2
          Open      Output,"c:\work\Income6.tmp",exclusive

          Loop      
          Read      output,seq;ordvars
          until     OVer
          if        (olrn = "722762")
.          call      debug
          endif
           add        c1,Pass2count
           display    *p10:14,*el,"order count ",Pass2count," order ##",olrn

...........................
          pack      MKEY,OMLRNUM,"000"
          rep       zfill,MKEY
          move      "O.Load5-NMLRKEY",Location
          pack      KeyLocation,"Key: ",MKEY
          call      NMLRKEY

...........................

          move      c1,ninvpath
          packkey   Ninvfld,Olrn
          rep       Zfill,Ninvfld
          CALL      NINVkey
          if        Over
          move      "U",Flag
          RESET     CANCODES               .RESET FORM POINTER.
          SCAN      OSTAT IN CANCODES       .CANCELLED?
                    if        Not equal
                    call      calcUnbild
                    move      c0,qtybild    
                    move      c0,formar
                    move      C0,lrinc
                    endif
          
          Else                          .billed lets scope it out
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
                    endif


          cmatch    "P",statb
                    if        equal
.Do we need paid date.?

.Yes include only those Paid in month report is run
                    if        (olrn = "719443")
.                    call      debug
                    endif
                    MOve      "P",Flag
                    Unpack    MLRPAYD,PCC,Pyy,PMM,Pdd
                    
                    Pack      Str6 from PCC,Pyy,PMM
                    call      trim using str6
                    Move      Str6,N6
                    pack      STr6A from "20",SysYR,SysMo
                    call      trim using str6A
                    Move      Str6A,N7
                              If        (n6 < N7)
                              Move      No,DetailFlag
                              move      "p",Flag                  .paid in previous period
                              ElseIf    (N6 = N7)
                              move      yes,detailflag            .include in PDF report
                              Else
                              Move      No,Detailflag
                              endif
.if paid over 120 from orginal MD -- does not qual
                    pack            NMLDFLD1,"01X",OLRN
                    clear   str8
                    pack            str8,"99999999"
                    call            NMLDAIM
                    loop
                    until over
                    if (NMLDDATE < str8)
                              move           NMLDDATE,str8
                    endif
                    call           NMLDKG
                    repeat
                    if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
                    unpack     str8 into omdtec,omdtey,omdtem,omdted
                    else
.Use current Mail Date
                    endif
                    clear     str8
                    move      c0,n8
                    move      c0,n9
                    rep       Zfill,mlrpayd
                    unpack    MLrpayd into str2,yy,mm,dd
                    call      cvtjul
                    move      Juldays,n9
                    move      omdtey,yy
                    move      omdtem,mm
                    move      omdted,dd
                    call      cvtjul
                    move      Juldays,n8
                    if        (detailflag <> no)                      .current so check
                              if        (Olnum = "018710" or Olnum = "024593")             .List Management Exchange Fee
                                        if        (n9-n8>90)   .contract says 90
                                        move      "l",flag
                                        endif
                              Else
                                        if        (n9-n8>120)   .contract says 120
                                        move      "l",flag
                                        if        (seller = "06")
                                        call      debug
                                        endif
                                        endif
                              ENDIF          
                    Endif
                                        

                    Else      
                    move      "O",Flag                              
                    endif
          Endif
          call      Update1
Skipper          
          repeat

          Close     Output1
          Goto      Pass3

.=================================================================
.Update1  - write or update summary records
Update1

.fixit 2013 july 16
                    reset     exfeelst
                    scan      olnum,exfeelst
                    if        equal
                    move      "018710",olnum                
                    endif
.fixit 2013 july 16
          packkey   Ndatfld from Olnum
          call      Ndatkey
          packkey   outkey from Team,olnum
          move      OLSTNAME,name
          MOve      olnum,OutId

           call      WriteCsv
          read      output1,outkey;str1;
          If        Over
          packkey   Outkey from Team,Outid
          goto      Write1
          endif
          read      output1,outkey;Team,OutID,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BLRT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LateCount,LateNames,Lart,LLrt:
                    prevcount,prevnames,ppart,ppLrt,Name
          call      Prepcounter
          
          Update    Output1;Team,OutID,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BLRT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LateCount,LateNames,Lart,LLrt:
                    prevcount,prevnames,ppart,ppLrt,Name
          
          call      Cleanup
          return
WriteCsv
.          reset     Runcodes
.          scan      Olnum,runcodes
.          Return    if equal
.
.          if        (olnum = "024593" or Olnum = "018055")
.          Return    
.          endif
          
          Move      Team,n2
          load      str35 with n2 from NAME1,NAME2,NAME3,NAME4,Name5
          clear     Pdate
          Clear     Bdate
          Pack      odate from oodtem,"/",oodted,"/",oodtec,oodtey
          Clear     MLDate
          pack      MLdate from omdtem,"/",omdted,"/",omdtec,omdtey
          if        (flag = "U")                               .Unbilled
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,ungross,unbilinc,Odate,c0,c0,c0,Bdate,MLdate,c0,c0,c0,c0,c0,c0:
                    Pdate,DaystoPay,Qualified,outid,name,Mcomp,onetper,COMMPCT,ppm
          Elseif    (flag = "O")                                  .billed but open                    
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,MLdate,qtybild,formar,lrinc,c0,c0,c0:
                    Pdate,DaystoPay,Qualified,outid,name,Mcomp,onetper,COMMPCT,ppm
          Elseif    (flag = "P")                               .billed and paid
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          Pack      Pdate from Pmm,"/",Pdd,"/",Pcc,Pyy
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,MLdate,c0,c0,c0,qtybild,formar,lrinc:
                    Pdate,DaystoPay,Qualified,outid,name,mcomp,onetper,COMMPCT,ppm
          Elseif    (flag = "l")                               .billed and paid late
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          Pack      Pdate from Pmm,"/",Pdd,"/",Pcc,Pyy
          Move      No,Qualified
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,MLdate,c0,c0,c0,qtybild,formar,lrinc:
                    Pdate,DaystoPay,Qualified,outid,name,mcomp,onetper,COMMPCT,ppm
          Elseif    (flag = "p")                               .billed and paid previous
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          Pack      Pdate from Pmm,"/",Pdd,"/",Pcc,Pyy
          Move      No,Qualified
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Lrinc,Odate,qtybild,formar,lrinc,Bdate,MLdate,c0,c0,c0,qtybild,formar,lrinc:
                    Pdate,DaystoPay,Qualified,outid,name,mcomp,onetper,COMMPCT,ppm
          endif 
          REturn
          
Write1
          call      Prepcounter
          Write     output1,outkey;Team,OutID,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BLRT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LateCount,LateNames,Lart,LLrt:
                    prevcount,prevnames,ppart,ppLrt,Name
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
          Elseif    (Flag = "p")          .paid prev
          add       c1,Prevcount
          add       QTYBILD,Prevnames
          add       Formar,PPart
          add       LRinc,PPlrt
          elseif    (flag = "P" & DetailFlag = Yes)
          add       c1,Paidcount
          add       QTYBILD,Paidnames
          add       Formar,Part
          add       LRinc,Plrt

          elseif    (flag = "l" & DetailFlag = Yes)
          add       c1,Latecount
          add       QTYBILD,Latenames
          add       Formar,Lart
          add       LRinc,Llrt

          elseif    (flag = "P" & DetailFlag <> Yes )
          add       c1,Opencount
          add       QTYBILD,Opennames
          add       Formar,Oart
          add       LRinc,Olrt
          endif
          if        (flag = "O" | Flag = "P" | Flag = "l" | Flag = "p")
          add       c1,bildcount
          add       QTYBILD,Bildnames
          add       Formar,Bart
          add       Lrinc,Blrt
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
          move      c0,Latecount
          move      c0,Latenames
          move      c0,Lart
          move      c0,Llrt
          move      c0,prevcount
          move      c0,prevnames
          move      c0,PPart
          move      c0,PPlrt
          clear     Name
          return
          

.
.
READBLTO
         RETURN
SWAP
.
         MOVE      MCCTO TO BILNAME
          move      mcomp to CONAME
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
          display    *p10:15,*el,"Printing! "

          Weof      CsvFIle,seq
          Close     Csvfile
          OPen      OUTPUT1,"\\nins1\e\data\Income6",exclusive   

.begin patch 1.10
                              PRTOPEN PrFile,"PDF:","c:\work\pdf\Income6.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING
.                              PRTOPEN PrFile,"PDF995","Income6"
.                              PRTCLose Prfile
..win7 rc is misbehaving :(                              
.                              PRTOPEN PrFile,"PDF995","Income6"
.end patch 1.10

          move      c0,n8
          move      c1 to RowCount
          clear     PgCnt
          call      Page
.===========================================================================
          move      "00000000",outkey
          read      output1,outkey;;
          Clear     HoldTeam          

          Loop
          readks    Output1;Team,OutID,OrdCount,Ordnames,NotXnames,OrdAr,OrdLR,BildCount,bildNames:
                    BART,BLRT,opencount,opennames,oart,olrt,paidcount,paidnames,PART,PLRT,LateCount,LateNames,Lart,LLrt:
                    prevcount,prevnames,ppart,ppLrt,Name
          Until     over
          if        (holdteam = "")
          call      ClearTots
          move      Team,HOldteam       .prep for break
          endif
          if        (team <> HoldTeam)
          call      TeamBreak
          endif
          
          if        (outid = "018710" or Olnum = "024593")             .List Management Exchange Fee

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
          add       LateCOunt,ToteLords
          add       LateNames,ToteLnames
          add       LART,ToteLar
          add       LLRT,TOTeLLR
          add       prevCOunt,Toteppords
          add       PRevNames,Toteppnames
          add       PPART,Toteppar
          add       PPLRT,TOTeppLR
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
          add       LateCOunt,TotLords
          add       LateNames,TotLnames
          add       LART,TotLAR
          add       LLRT,TOTLLR
          add       prevCOunt,Totppords
          add       PRevNames,Totppnames
          add       PPART,Totppar
          add       PPLRT,TOTppLR
          endif

          Repeat
          call      TeamBreak          
          GOto      Eoj          
TeamBreak
.print the goodes for the team and clear
          Calc      GrandAR = (TotUnAR+TotBAR)
          Calc      GrandLR = (TotUnLR+TotBLR)
          Move      HOldTeam,n2
          load      str35 with n2 from NAME1,NAME2,NAME3,NAME4,Name5
          prtpage   PRfile;*pcolumn:row,*font=font12,*ll,*ALIGNMENT=*Center,str35;
          add       eightlpi,row
          add       eightlpi,row

          Move      Mask9,Str11
          Edit      Totords,Str11
          Move      Mask9,Str11a
          Edit      TotNames,Str11a                  
          Move      Mask9,Str11b
          Edit      TotXNames,Str11b   
          Move      Mask$102,Str16
          edit      GrandAr,str16
          Move      Mask$102,Str16a
          edit      GrandLr,str16a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders Placed":
                    *pcolumn3:Row,STr11,*pcolumn4:Row,str11a,*pcolumn5:Row,str16,*pcolumn6:Row,str16a:
                    *pColumn7:row,str11b
          add       eightlpi,row
          add       eightlpi,row
          calc      N9 = (totords - totBords)
          move      N9,Notords
          calc      N9 = (Totnames - TotBnames)              take them back out for unbilled
          move      N9,Notnames
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      Notords,Str11
          Move      Mask9,Str11a
          Edit      NotNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotUnAr,str15
          Move      Mask$92,Str15a
          edit      TotUNLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders NOT Billed":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,Str15,*pcolumn6:Row,Str15a
.                    *pcolumn3:Row,Notords,*pcolumn4:Row,Notnames,*pcolumn5:Row,TotUnAR,*pcolumn6:Row,TotUnLR
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      TotBords,Str11
          Move      Mask9,Str11a
          Edit      TotBNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotBAr,str15
          Move      Mask$92,Str15a
          edit      TotBLr,str15a

          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Orders Invoiced":
                    *pcolumn3:Row,str11,*pcolumn4:Row,str11a,*pcolumn5:Row,str15,*pcolumn6:Row,str15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      TotOords,Str11
          Move      Mask9,Str11a
          Edit      TotONames,Str11a                  
          Move      Mask$92,Str15
          edit      TotOAr,str15
          Move      Mask$92,Str15a
          edit      TotOLr,str15a

          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Open":
                    *pcolumn3:Row,Str11,*pcolumn4:Row,Str11a,*pcolumn5:Row,Str15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      TotPords,Str11
          Move      Mask9,Str11a
          Edit      TotPNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotPAr,str15
          Move      Mask$92,Str15a
          edit      TotPLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Paid":
                    *pcolumn3:Row,Str11,*pcolumn4:Row,Str11a,*pcolumn5:Row,Str15,*pcolumn6:Row,STR15a
.begin patch 1.05
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      Totppords,Str11
          Move      Mask9,Str11a
          Edit      TotppNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotppAr,str15
          Move      Mask$92,Str15a
          edit      TotppLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Paid Prev":
                    *pcolumn3:Row,Str11,*pcolumn4:Row,Str11a,*pcolumn5:Row,Str15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      TotLords,Str11
          Move      Mask9,Str11a
          Edit      TotLNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotLAr,str15
          Move      Mask$92,Str15a
          edit      TotLLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"Invoices Paid Late":
                    *pcolumn3:Row,Str11,*pcolumn4:Row,Str11a,*pcolumn5:Row,Str15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          add       eightlpi,row
          if        (n2 = c2)          .List management ex fee
          Calc      GrandAR = (totebar)
          Calc      GrandLR = (toteBlr)
          Move      Mask9,Str11
          Edit      TotEords,Str11
          Move      Mask9,Str11a
          Edit      TotENames,Str11a                  
          Move      Mask$102,Str16
          edit      GrandAr,str16
          Move      Mask$102,Str16a
          edit      GrandLr,str16a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"EXfee Orders Placed":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,Str16,*pcolumn6:Row,STR16a
          add       eightlpi,row
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      ToteBords,Str11
          Move      Mask9,Str11a
          Edit      TotEBNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotEBAr,str15
          Move      Mask$92,Str15a
          edit      TotEBLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"EXfee Orders Invoiced":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,STR15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      ToteOords,Str11
          Move      Mask9,Str11a
          Edit      TotEONames,Str11a                  
          Move      Mask$92,Str15
          edit      TotEOAr,str15
          Move      Mask$92,Str15a
          edit      TotEOLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Invoices Open":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,STR15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      TotePords,Str11
          Move      Mask9,Str11a
          Edit      TotEPNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotEPAr,str15
          Move      Mask$92,Str15a
          edit      TotEPLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Invoices Paid":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,STR15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      Toteppords,Str11
          Move      Mask9,Str11a
          Edit      TotEppNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotEppAr,str15
          Move      Mask$92,Str15a
          edit      TotEppLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Inv Paid Prev":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,STR15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          Move      Mask9,Str11
          Edit      ToteLords,Str11
          Move      Mask9,Str11a
          Edit      TotELNames,Str11a                  
          Move      Mask$92,Str15
          edit      TotELAr,str15
          Move      Mask$92,Str15a
          edit      TotELLr,str15a
          PRtpage   Prfile;*pcolumn1:Row,*ALIGNMENT=*Right,*font=font8,"ExFee Inv Paid Late":
                    *pcolumn3:Row,STR11,*pcolumn4:Row,STR11a,*pcolumn5:Row,STR15,*pcolumn6:Row,STR15a
          add       eightlpi,row
          add       eightlpi,row
          endif          

          call      ClearTots
          move      Team to N2
          if        (n2 = c2)
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
          Move      C0,TotLords
          Move      C0,TotLnames
          Move      C0,TotLAR
          Move      C0,TOTLLR
          Move      C0,TotELords
          Move      C0,TotELnames
          Move      C0,TotELar
          Move      C0,TotELLR

          Move      C0,Totppords
          Move      C0,Totppnames
          Move      C0,TotppAR
          Move      C0,TOTppLR
          Move      C0,TotEppords
          Move      C0,TotEppnames
          Move      C0,TotEppar
          Move      C0,TotEppLR

          return

*............................................................
.
ABORT
         TRAPCLR   F5
          Shutdown  "cls"
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
        prtpage pslsfile;*pTitleH3:row,*ALIGNMENT=*Center,*font=font12,"Names in the News - Monthly Billing By Client ";
        prtpage pslsfile;*pTitleH4:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clear str10
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
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title2,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title8a,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage pslsfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Client",*ULOFF,*boldoff;
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

          Shutdown  "cls"
         STOP
calcUnbild
.          call      Debug
          move      No,LSTMSW
.          if        (seller = "06" | seller = "27")
          if        (seller = "06")
          move      Yes,Lstmsw
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



.begin patch 1.04
         include   nmldio.inc
.end patch 1.04

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
          include   Commio.inc
          INCLUDE   COMLOGIC.inc


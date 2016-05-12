...............................................................................
. NINC0004a - find LM Dollars
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
          INC       NjstDD.inc
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
release  init      "2.00"          DLH     Major cleanup
reldate   Init      "2014 July 21"
.release  init      "1.61"          DLH     added Flags= to prtopen with PDF:
.reldate   Init      "2014 March 25"
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
.HBILLKEY DIM       8    *FOR MATCH MLR/BILL-TO BREAK?
KEY54    DIM       54   *OUTPUT FILE KEY.
. WORK VARIABLES
.
.
.
Skip	Dim	1
File      FIle
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
Calc102   Form      10.2
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
          
          
          
.
	pack	taskname from "\\nins1\e\data\income4a",cc,sysyr,sysmo,".CSV|10.10.30.103:502"
          Prepare   CSVFIle,taskname,exclusive   

          write     CsvFile,seq;*cdfon,"order ID","order qty","Exch Qty","est Rev","Est A/P","est INC","Est N INC","Order date","qty bild","Revenue","Payables","Income","NIN INC":
                    "Inv Date","Mail Date","Qty Open","Rev Open","AP OPEn","Inc Open","NIN INC OPEN","Qty paid","Rev Paid","AP PAID","Inc Paid","NIN PAID","Date Paid":
                    "List ID","ID","List Name","Mlr Name","Division"                    ."
          

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
.sort 'NEW' Invoices.
         DISPLAY   *P10:09,"Sorting 'new' invoices: "

          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.New;7-12,S=#"132=#'",sysYr,"#'&134=#'",sysmo,"#'#""
          Sort      Taskname,SunDM="NINS1:502"
.
.sort 'NEW' adjustments
         DISPLAY   *P10:11,"Sorting 'new' adjustments: "
          pack      str4 from Sysyr,sysmo
          rep       zfill in str4
          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127='",str4,"'#""
          Sort      Taskname,SunDM="NINS1:502"
.
.
.
.
          open      FIle,"\\nins1\e\data\nininv.new|NINS1:502"
          move      c1,pass

	lOOP
READsls   Read      File,seq;Invvars
	until 	over
          ADD       C1 TO COUNT
          DISPLAY   *P10:13,"New Sales records PROCESSED: ",COUNT
          call      getdata
	if	(skip = No)

	Call	WriteCSV

	endif
	repeat
 
 	open      FIle,"\\nins1\e\data\nadjust.new|NINS1:502"
          Move      C0 TO COUNT
	move	c2,pass
	Loop
Readjst   Read      File,seq;jstvars
	until	over	
          ADD       C1 TO COUNT
          DISPLAY   *P10:15,"New adjustment records PROCESSED: ",COUNT
          call      getdata
	if	(skip = No)

	Call	WriteCSV

	endif
	repeat




Abort     Weof      CsvFIle,seq
          Close     Csvfile
	shutdown
         
...........................

.=================================================================

WriteCsv
	move	"List Management",str35
          if    (flag = "O")                                  .billed but open                    
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,AP,Lrinc,NININC,Odate,qtybild,formar,AP,lrinc,NININC,Bdate,mldate,c0,c0,c0,c0,c0,qtybild,c0,c0,c0,c0:
                    Pdate,Ndatfld,Flag,Olstname,Mcomp,str35,str45,form122
          Elseif    (flag = "P")                               .billed and paid
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          Pack      Pdate from Pmm,"/",Pdd,"/",Pcc,Pyy
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,FormAr,Ap,Lrinc,NININC,Odate,qtybild,formar,ap,lrinc,nininc,Bdate,mldate,c0,c0,c0,c0,c0,qtybild,formar,AP,lrinc:
                    NININC:
                    Pdate,Ndatfld,Flag,Olstname,Mcomp,str35,str45,form122
          Elseif    (flag = "A")                               .adjustment
	unpack	JSTDATE into INvdtec,invdtey,invdtem,invdted
          Pack      Bdate from INVDTEM,"/",INVDTED,"/",INVDTEC,INVDTEY
          write     CsvFile,seq;*cdfon,Olrn,oqty,NotXnames,JSTAR,JstAp1,JSTLRINC,JSTNININC,Odate,qtybild,JSTAR,JstAp1,JSTLRINC,JSTNININC,Bdate,mldate,c0,c0,c0,c0,c0,qtybild,c0,c0,c0:
                    c0,c0,Ndatfld,flag,Olstname,Mcomp,str35,str45
	endif

          call      Cleanup

	return
.============================================
          
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
          move      c0,Bapt
          move      c0,Bnint
          return
          

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
*............................................................
.==========================================================================================

.=====================================================================
*...................................................................................
GetData
.
	move	No,skip
	if	(pass = c2)
	move	Jstlr,LRN
	endif

	if	(pass = c2 & jstreasn = "16")				.shortpayment skip
	move	yes,skip
	return
	Elseif	(pass = c2 & jstreasn = "14")				.Adv pay tp LO skip
	move	yes,skip
	return
	endif

         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
          reset     Runcodes
          scan      Olnum,runcodes
.          
	if	equal
	move	yes,skip
	return
	endif

          pack      str5,"plxzX"         .skip pending orders and lcrs and cancelled orders with no billing
          scan      Ostat,str5
	if	equal
	move	yes,skip
	return
	endif

               MOVE          OLNUM TO NDAT3FLD
               clear          str6
               CLEAR          NDATTDMC
               rep            zfill in ndat3fld
               CALL           NDAT3KEY
               IF             NOT OVER                  *TDMC BILLING INFO.
               CLEAR          STR10
                              IF             (NDatTDMC = "B")
                              MOVE      "BOTH" TO STR10
                              Elseif         (NDatTDMC = "R")
                              MOVE      "RENT/SPLIT" TO STR10
                              ELseIF         (NDatTDMC = "E")
                              MOVE      "EXCH/Only" TO STR10
                              ENDIF
                              If             (Ndat3cde = b1)
                              MOVE      "TDMC" TO STR8
                              Elseif         (ndat3cde = "F")
                              MOVE      "FIDE" TO STR8
                              Elseif         (ndat3cde = "A")
                              MOVE      "Anacapa" TO STR8
                              Elseif         (ndat3cde = "J")
                              MOVE      "Antares" TO STR8
                              Elseif         (ndat3cde = "P")
                              MOVE      "PIDI" TO STR8
                              Elseif         (ndat3cde = "M")
                              MOVE      "MMI" TO STR8
                              Elseif         (ndat3cde = "R")
                              MOVE      "Frontlin" TO STR8
                              ENDIF
.
                              IF             (ndatdolc = "Y")
                              move       "$/Date" to str6
                              endif

                              clear          str45

                              Append         Str8,str45
                              append         b1,str45
                              append         str10,str45
                              append         b1,str45
                              append         "Chrgs ",str45
                              append         b1,str45
                              append         str6 to str45
                              if             (ndat3exh = yes)
                              append         "+Exch Fee",str45
                              Endif
                              reset          str45
               endif


          Reset     EXFEELST

          scan      Olnum,ExFeeLst
          if        equal
          move      "018710",olnum                      .combine them
          move      c0,Osales10                   .force the figure for management commission numbers
          move      C6,Osales
          endif
	
          pack      seller,osales10,osales
          if        (seller <> "02" & Seller <> "06" & Seller <> "19" & Seller <> "27" & Seller <> "28" & Seller <> "30")
          move	yes,skip
          return
          endif
          
          cmatch    "P",statb
                    if        equal
                    MOve      "P",Flag
                    Unpack    MLRPAYD,PCC,Pyy,PMM,Pdd
		else
		MOve	"O",flag
		clear	pcc
		clear	pyy
		clear	pmm	
		clear	pdd
		endif
	if	(pass = c2)
	move	"A",Flag
	endif


         type      obrknum
         if        equal
         PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         CALL      NBRKKEY
         move      yes to brker
         endif
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.
          pack      NMLDFLD1,"01X",LRN
        clear   str8
          pack      str8,"99999999"
          call      NMLDAIM
          loop
                    until over
                    if (NMLDDATE < str8)
                              move      NMLDDATE,str8
                    endif
                    call      NMLDKG
          repeat
          if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
          else
.Use current Mail Date
          endif
          Clear     mldate
          Pack      mldate from omdtem,"/",omdted,"/",omdtec,omdtey

         MOVE      OLON TO NOWNFLD    .NEED INFO FOR BILLING. 11/94
         CALL      NOWNKEY
.
.
         MOVE      NO TO SUBPPSW                          .do not deduct prepayment
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
         call      wipecvars
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         move      lrn to nshpfld
         call      nshpkey
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
         move      c0 to ar
         move      formar to ar
.why?????????????
.         move      ap to ap1
.
          packkey   Tinvfld,lrn
	move	c0,form122
          call      Tinvkey
          	if        not over
          	Move      TinvDOLR,form122
          	mult       ".01" by form122
          	sub       form122 from NINinc
          	endif

....................
.
.
OUTPUT
         CMATCH    "P" TO STATB
.         MOVE      C0 TO FORM102
.         MOVE      C0 TO FORM102
.         MOVE      ap1 TO FORM102
.         MOVE      C0 TO Calc102
.         MOVE      C0 TO Calc102
.         MOVE      ap1 TO Calc102
.	add	Cacl102,Form102
	add	Ap2,AP
	add	Ap3,Ap
.
          return
*................................................................................................................................



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
          INC       Njstio.inc
.begin patch 1.3
               include        nmldio.inc
.end patch 1.3
.begin patch 1.6
	include        TINVIO.inc
.end patch 1.6
          INCLUDE   COMLOGIC.inc


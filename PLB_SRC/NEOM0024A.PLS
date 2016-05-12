...............................................................................
.NEOM0024 - list owner income summary report
...............................................................................
.
PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
         include   consacct.inc
         include   hp.inc
.begin patch 2.5
.         INC       NINVDD.inc
          INC                 ninvdd.inc
          INCLUDE             NInvAcddd.INC
.end patch 2.5
.patch2.45
                                        include   compdd.inc
                                        include   cntdd.inc
.         INC       NMLRDD.INC
.patch2.45
         INCLUDE   NmtxDD.INC
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         INCLUDE   GNXTDD.INC
         INCLUDE   NDAT3DD.INC
         include   nslsdd.inc
         inc       nmrgdd.inc
         INCLUDE   NLOBDD.INC
.begin patch 1.9
         INCLUDE   NJSTDD.inc
shipsw    dim       1
mrgsw    dim       1
.end patch 1.9
         include   npaydd.inc
         include   nmoadd.inc
.begin patch 1.8
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
.end patch 1.8
.begin patch 2.4
               Include        PrtPagedd.inc
.end patch 2.4
.begin patch 2.51
          INclude   nadjRes.inc
.end patch 2.51

.
...........................................
release  init      "2.51"         DLH         
reldate   Init      "23 September 2008"
.release  init      "2.5"         DLH           2March2005 Invoice Conversion
.release  init      "2.48"    ASH 21MAR2005  Added logic to test for empty Input file 
.release  init      "2.47"         JD           12Jan2005 ready for 2005.
.release  init      "2.46"                  ASH 07SEP2004   Logo Conversion
.release  init      "2.45"                  JD    26MAY2004 Mailer Conversion
.release  init      "2.44"                 JD 23Mar2004 read ownerstm.srt as input
.release  init      "2.43"                JD 06Jan2004 fixed if with exflag check.
.release  init      "2.42"                DB 01MAY2003 added code to create pdf in c:\work
.Release        Init           "2.41"         DLH 7October2002          client requests
.                                            including subtotals & minor heading changes
.release  init      "2.4"              DLH 06Aug2002 prtage
.release  init      "2.31"              DLH 14Feb2002 reason code 37
.release  init      "2.30"              JD21NOV01 added exchange breakout/care spool file.
.release  init      "2.17"              JD 07Sep01 added New adjres codes.
.release  init      "2.16"              JD 10may01 added lob read if no new billing.
.release  init      "2.15"              JD 19mar01 added owner print info on adjustment page.
.release  init      "2.14"             ASH 02OCT2000 NEW SERVER ADDED
.release  init      "2.13"             JD 27sep00   IF 2aps print owner info payment info.
.release  init      "2.12"             JD 13jul00  fixed mask ap2
.release  init      "2.11"             JD 28mar00  changed to new address.
.elease  init      "2.1"              JD 31jan00  temp fix for jan new year check.
.release  init      "2.0"             DLH 25Oct99 convert form to plb
.release  init      "1.8"             DLH 27Apr99 NININV Y2K
.RELEASE  INIT      "1.72"            JD  31mar99 changed dates without cc
.RELEASE  INIT      "1.7"            ASH 30DEC98 NINORD Y2K, File expansion; CONSACCT.INC VAR EXPANSION
.release  init      "1.6"            JD30nov98 compressed mcomp print.
.release         init      "1.5"            JD 24jul98 fixed bug adj total/mtax print.
.release         init      "1.4"            JD 25Feb97   print only option
.release  init      "1.3"            JD 31Jan97  flat file for whitney
.release  init      "1.2"           DLH 27Mar96 cleanup # of break problems.
.release  init      "1.1"          DLH 04Mar96 
.change primary input file to ownerstm.srt - so we don't miss any accounts that
.did not have new billings.
.
.release  init      "1.0"          DLH 12Feb96 
.
.runs for report types as one report per owner/list
.
.primary input file is pareom.own
..............................................................................
.input file  ownerstm.srt created by neom0023 
input    file      uncomp
........................................................................
.parfile - hold all exclusive list owner numbers and flag for a/r info on the 
.          report
parfile  ifile     keylen=4
OUTPUT   FILE      UNCOMP
...............................................................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK INIT      "XX/XX/XX"
.Start Patch #1.7 - expanded var to hold century
DATEPRT1 DIM       8
DATEPRT2 DIM       8
.DATEPRT1 DIM       10
.DATEPRT2 DIM       10
.End Patch #1.7 - expanded var to hold century
AP1OUT   DIM        15
ADJAPOUT DIM        12
.START PATCH #1.7 - EXPANDED VAR
.AP2OUT   DIM       12
AP2OUT   DIM       15
.END PATCH #1.7 - EXPANDED VAR
aplotus  form      10.2
aplotown form      10.2
AP1FORM  FORM      10.2
AP2FORM  FORM      10.2
lstcomm  form      10.2                       ;owner list income
slccomm  form      8.2                        ;owner select income
.slccommd dim       10
slccommd dim       11
.begin patch 2.41
LstcommTot     Form           10.2
SlcCommTot    Form           10.2
GrossoutTot     Form           10.2
BrkComTot     Form           10.2
NINCommTot     Form           10.2
NINIncTot      Form           10.2
.end patch 2.41
aplotmsk INIT      "ZZZZZZZZZZ99-"
TOTMASK  INIT      "$$,$$$,$$9.99-"
APMASK   INIT      "$$,$$$,$$9.99-"
ARMASK   INIT      "$,$$$,$$$,$$9.99-"
grossMSK INIT      "$$$,$$$,$$9.99-"
adjAPMSK inIT      "$$$$,$$9.99-"
.tadjAPMSKinIT      "$$,$$$,$$9.99-"
.START PATCH #1.7 - NEW VAR
APMASK2  INIT      "$$,$$$,$$9.99-"
.END PATCH #1.7 - NEW VAR
TOTOMSK  INIT      "$$,$$$,$$9.99-"
APCHECK  FORM      "000000001"
tadjmask dim       17
APSW    DIM       1
AP2SW    DIM       1
TAX501   FORM      1
form102  form      10.2
incount  form      5
flatflg  dim       1
rflag    dim       1
.comma    init      ","
..............................................................................
.;adjustment reason code descriptions
.;
nadjtext  DIM       35
.adjres1  init      "Adjustment to Quantity"
.adjres2  init      "Shipping"
.adjres3  init      "Selection fee"
.adjres4  init      "Running Charges"
.adjres5  init      "Change in price"
.adjres6  init      " "
.adjres7  init      "Adjust Within A/P "
.adjres8  init      "Adjust A/R & LR "
.adjres9  init      "Adjust A/P & LR"
.adjres10  init      "Cancel entire Bill"
.adjres11  init      "No invoice A/P to Lr "
.adjres12  init      "Late Lo inv LR to A/P "
.adjres13  init      "Adjustment of Income"
.adjres14  init      "Advance Payment to LO"
.adjres15  init      "Adjustment of Tax"
.adjres16  init      "Short Payment"
.adjres17  init      "Commission"
.adjres18  init      "Postage"
.adjres19  init      "Direct Payment to LO"
.adjres20  init      " "
.adjres21  init      "Advance Payment to LO"
.adjres22  init      "Reduction of A/R"
.adjres23  init      "Reduction of A/P (Contra)"
.adjres24  init      "Discount Earned"
.adjres25  init      "Additional Billing"
.adjres26  init      "Write off of A/R"
.adjres27  init      "Prepayment"
.adjres28  init      "Write off of A/P"
.adjres29  init      "                           "
.adjres30  init      "Taking Credit-Original open"
.adjres31  init      "Credit Transfer"
.adjres32  init      "Refund Credit Taken"
.adjres33  init      "Cancelled/Billing Adjusted "
.adjres34  init      "Adj due to Order Change    "
.adjres35  init      "Court Imposed Bankruptcy Charge"
.adjres36  init      "Bankruptcy, Un-collectible A/R "
.Adjres37  Init      "Void Check"
.adjres99  init      "Entry Correction"
.
.FILES.
...............................................................................
.
.
. WORK VARIABLES
..............................................
.
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
TYPIST   DIM       2
.
TOTARp   FORM      10.2       *prepaid
TOTpMOA  FORM      10.2       *MOA Applied to prepaid
TOTAR    FORM      10.2
TOTAP1   FORM      10.2
TOTAP2   FORM      10.2
TOTAP    FORM      10.2
TOTNIN   FORM      10.2
TOTLR    FORM      10.2
TOTSTAX  FORM      10.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
.
.END TDMC.
.
LRMRINC  FORM      10.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      10.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      10.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      10.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      10.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      10.2      UNKNOWN LR INCOME.
LRBBE    FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR    FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
.
ARMR     FORM      10.2      TOTAL MANAGEMENT/RENTAL  A/R
ARME     FORM      10.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM      FORM      10.2      TOTAL MANAGEMENT A/R
ARBR     FORM      10.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE     FORM      10.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN   FORM      10.2      UNKNOWN  A/R.
ARBBE    FORM      10.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR    FORM      10.2      TOTAL BATCH BILL A/R RENT PORTION
.
APMR     FORM      10.2      TOTAL MANAGEMENT/RENTAL A/P
APME     FORM      10.2      TOTAL MANAGEMENT/EXCHANGE A/P
APM      FORM      10.2      TOTAL MANAGEMENT A/P
APBR     FORM      10.2      TOTAL BROKERAGE/RENTAL A/P
APBE     FORM      10.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN   FORM      10.2      UNKNOWN A/P.
APBBE    FORM      10.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR    FORM      10.2      TOTAL BATCH BILL A/P RENT PORTION
.
ppflag   dim       1            'P' if equal else blank
PMASK    DIM       1
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
.Start Patch #1.7 - added vars to handle OQTY increase
FORM9    FORM      9
.End Patch #1.7 - added vars to handle OQTY increase
FORM52   FORM      5.2
.Start Patch #1.7 - DUPLICATE VAR NOW IN CONSACCT.INC
.FORM92   FORM      9.2
.END Patch #1.7 - DUPLICATE VAR NOW IN CONSACCT.INC
FORM11   FORM      11
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5                 total reads input file
COUNTA   FORM      5                 new invoices
COUNTB   FORM      5                 new payments
countC   form      5                 new adjs
countD   form      5                 current open bills 
CO       FORM      1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
SALESNUM DIM       2
TEAM1    INIT      "01"     SUSAN
TEAM2    INIT      "02"    ELAINE
TEAM3    INIT      "03"    LIST MANAGEMENT
.RUNCODES INIT      "005051-009766"
. 
.
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.Start Patch #1.7 - added vars to handle OQTY increase
MASK9    INIT      "ZZZ,ZZZ,ZZZ"
.End Patch #1.7 - added vars to handle OQTY increase
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
.Start Patch #1.7 - increased vars to handle OQTY increase
.m$oqty   dim       9
m$oqty   dim       11
.End Patch #1.7 - increased vars to handle OQTY increase
.begin patch 1.8
.M$QTY    DIM       9
M$QTY    DIM       11
.end patch 1.8
M$AP1    DIM       13
M$AP2    DIM       13
M$STAX   DIM       8
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       13
M$NINC   DIM       13
M$GROSS  DIM       13
.
MT$AR    DIM       15
MT$ARP   DIM       15
MT$pMOA  DIM       15     *prepaid 
MT$AP1    DIM       15
MT$AP2   DIM       15
MT$STAX  DIM       15
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       15
MT$NINC  DIM       15
.
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
innets   dim       1
rectype    dim       1
holdid   dim       1
holdown  form       4
holdlist form       6
holdlst1 dim       35
HOLDREC  DIM       10
newMLR    FORM      4       1-4     MAILER NUMBER.            NIN
newLR     FORM      6       5-10    LIST RENTAL NUMBER.       NIN
newOWN    FORM      4      11-14    LIST OWNER NUMBER.        NIN
newGUAR1  DIM       1      15-15    OUTSIDE GUARANTY          NIN
newCNT    FORM      3      16-18    BROKER NUMBER.            NIN
newLIST   FORM      6      19-24    LIST NUMBER.              NIN
newMDTE   FORM      6      25-30    MAIL DATE.                NIN
newAP1    FORM      7      31-37    ACCOUNT PAYABLE ONE.      NIN
newDJCD   DIM       1      38-38    DOW JONES CODE.           NIN
newADJCD  DIM       1      39-39    ADJUSTMENT CODE.          NIN
newIDTE   FORM      6      40-45    INVOICE DATE.             NIN
newAP2    FORM      9      46-54    ACCOUNTS PAYABLE TWO.     NIN
newLST1   DIM       35      55-89    LIST DESCRIPTION ONE.     NIN
newCODE   DIM       1       90-90    CREDIT/DEBIT CODE,'C or D'NIN
newGUAR   DIM       1       91-91    GUARANTY CODE.            NIN
newAR     DIM       8       92-99    A/R (NO DECIMAL)
newCHKDTE DIM       6      100-105   INV CHECK DATE
newCNAME  DIM      25      106-130   CLIENT NAME
newadjsw  form      1      131-131   adjustment switch 2-adjusted
newxchrg  form      7.2
.
hldarflg dim       1
DATEKEY  DIM       6
TAXPRT   INIT      "       "
APTOTOWN FORM      10.2       DLH
APTOTWNX FORM      10.2       jd
APTOTWNR FORM      10.2       jd
OWNERMSK DIM       17
OWNRMSKX DIM       17
OWNRMSKR DIM       17
arform   form      10.2
arout    dim       17
.begin patch  2.4
Grossmlr       Form           9.2                  ;reflect full charges to mailer for care
HoldRecAdj     Form           10.2
HoldRecAdj1     Form           10.2
HoldLrInc      Form           10.2
HoldNinInc     Form           10.2
.end patch    2.4
grossout dim       15
arflag   dim       1        from parfile if True print ar
pass     form      1        1=new bills, 2=old bills, 3=old bills, 4=adjustments
LISTCHG  DIM       1
CBLSTNUM FORM      6
CBOWNNUM FORM      4
JSTN     FORM      1
manpay   form      10.2
MANMASK  INIT      "$,$$$,$$$,$$9.99-"
OWNBR    DIM       1
PAYCHK   FORM      1
first    dim       1
CHKDATE  DIM       8
APTOTDET FORM      10.2
LSTMASK  DIM       17
.ARMASK   dim       12
APTOTLST FORM      10.2
ARTOTLST form      10.2
PAYKEY   DIM       5
adjap    form      7.2
adjap1   form      7.2
adjap2   form      7.2
aplist   form      10
lastrec  init      "F"      'T'=true
newflag  init      "F"      'T'=true no previous balance
listbrk  init      "F"
thisown  form       4
hit      form       5        0=no records for this owner/list
exflag   init       "F"                     
exerflag init       "N"
splpass  form       "0"
ExpRepFlag  init       "N"               Expanded report flag default = "N"o  
nincomm  form       9.2
.patch2.42
splname  dim        45
.patch2.42
.
.START PATCH 2.46 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 2.46 ADDED LOGIC
         MOVE      "Names in the News" TO COMPNME
.         MOVE      "NEOM0024a" TO PROGRAM
         MOVE      "MONTHLY List Owner Report" TO STITLE
.START PATCH 2.14 REPLACED LOGIC
.         prepare    output,"g:\data\loact.tmp"
.         SPLOPEN   "g:\data\Ownerstm.lst"

                    Move                c3 to NMLRLOCK
                    Move                c3 to NordLOCK
                    Move                c3 to NdatLOCK

         PACK      STR35,NTWKPATH1,"CareOAR.tmp"
         PACK      STR45,NTWKPATH1,"Ownerstm.LST"
         prepare   output,STR35
.patch2.42  - commenting out/moving so prtfile can pick up correct date prefix for filename
.;                  PRTOPEN       Laser,"","test fax"
.;               Move           "T" to HLDARFLG
.begin patch 2.4
.;               call           prtform
.end patch 2.4
.               prtclose      Laser
.               stop
.patch2.42

.         SPLOPEN   STR45
.END PATCH 2.14 REPLACED LOGIC
.begin patch 2.4
.         PRINT     hpreset,hpland:
.                   033,"&l51P":               page length
.                   033,"&l50F":               number lines
.                   033,"&l0E",033,"&a0c0R"     top margin * print position
.end patch 2.4

.         CALL      PRTFORM
CHOOSE
         CALL      PAINT
         move      "E" to str1
         KEYIN     *P25:14,"(E)om processing, (P)rint report":
                   " ",*T60,*RV,str1;
         CMATCH    "P" TO str1
         If        equal
         move      yes to rflag
         else   
         move      no to rflag
         endif
.       
CLOCK    CLOCK     DATE TO DATE
         MOVE      DATE TO DATEMASK
.         MOVE      DATE TO TODAY
         UNPACK    DATE INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         KEYIN     *CL
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
.
         MATCH     "NEOM0024A" TO PROGRAM         .ENTRY FROM DSINIT?
       IF        NOT EQUAL                     . NO.
DATE
         MOVE      YES TO STR1
         KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,STR1
         MOVE      DATEMASK TO TODAY
         CMATCH    YES TO STR1
         GOTO      OPEN IF EQUAL
         GOTO      DATE IF EOS
         KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,SYSMO,"/":
                   *DE,*JR,*ZF,*RV,SYSDY,"/",*DE,*JR,*ZF,*RV,SYSYR
         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
         MOVE      DATE TO DATEMASK
         MOVE      DATEMASK TO TODAY
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      DATE
         endif
OPEN     
.patch2.42
         pack      str8,"care",sysmo,sysyr
         pack      splname,"c:\work\pdf\",str8,".lst"
         PRTOPEN   Laser,"\\NINs2\Laser6",str8,noprint,spoolfile=splname
         Move      "T" to HLDARFLG
         call      prtform
.patch2.42
.         trap      eoj if f5
.         open      parfile,"\\nins1\e\data\index\pareom",read
         open      parfile,"pareom",read
         move      sysmo to n2
         sub       c1 from n2
         compare   c0 to n2
         if        equal          .its jan last month was dec set key accordingly
         move     sysyr to n2
         sub      c1 from n2
         move     n2 to str2
.begin patch 2.47
         pack     DATEKEY from "200412"
.begin patch 2.47
         rep      zfill in datekey
         else
         move     n2 to str2
         pack     DATEKEY from cc,sysyr,str2
         rep      zfill in datekey
         endif
.temp DH
.begin patch 2.44
           open      input,"ownerstm.srt"
.         open      input,"c:\work\ownerstm.care",read
.end patch 2.44
.temp DH
         move      c1 to nslspath
.begin patch 1.8
         move      c1 to ndatpath
.end patch 1.8
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
BEGIN
         trap      eoj if f5
.
         move      c0 to holdown
        
input  
.
READsls  
         read      input,seq;rectype,slsvars
         if        over
.START PATCH 2.48 ADDED LOGIC
.Test to see if Input file was empty
                    if (count = C0)
                              shutdown
                    endif
.END PATCH 2.48 ADDED LOGIC
         move      "T" to lastrec
         GOTO      TOTold
         endif
         add       c1 to count
         DISPLAY   *P10:10,"Input records PROCESSED: ",COUNT,b1,slsown,b1,slslist

.begin patch 2.44
         compare   "5457" to slsown
         goto       readsls if not equal
.end patch 2.44
.begin patch 2.30
         compare   c1 to splpass
         goto      chkcare if equal
         compare   "5457" to slsown
         if        equal
         call      totold
.         splclose
         PACK      STR45,NTWKPATH1,"CAREOAR.jd"
.         SPLOPEN   STR45
.         PRINT     hpreset,hpland:
.                   033,"&l51P":               page length
.                   033,"&l50F":               number lines
.                   033,"&l0E",033,"&a0c0R"     top margin * print position

.begin patch 2.4
.         CALL      PRTFORM2
         CALL      PRTFORM
.end patch 2.4
         move      yes to ExpRepFlag                          ;turn on expanded report option
         move      c1 to splpass
         goto      processr
         endif

         call      prtform
.
chkcare
         compare   "5457" to slsown
         if        not equal
         cmatch    yes to ExpRepFlag
         if        equal
         call      totold
         move      no to ExpRepFlag
.         splclose
         PACK      STR45,NTWKPATH1,"Ownerstm.JD"
.         SPLOPEN   STR45,"Q"
         endif
         endif
.end patch 2.30
processr
         move      no to exflag
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES                    ;exchange ?
         IF        EQUAL                                 ;yes
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9                              ;split rent/exch ? if so <> zero
.start patch 2.43
.         IF        EQUAL
         GOTO      SPLITOK IF NOT EQUAL
         move      yes to exflag                         ;Pure exchange
.         goto      readsls
.         endif
.end patch 2.43
         endif
.         endif
SPLITOK
         clear     str4
         move      slsown to str4
         rep       zfill in str4
         read      parfile,str4;str4,arflag                  .on the list????
         goto      readsls if over                   .no
         match     "5457" to str4
         if        equal
         move      yes to flatflg
         else
         move      no to flatflg
         endif
         add       c1 to incount
         compare   c1 to incount
         if        equal
         move      slsown to holdown
         move      slslist to holdlist
         move      rectype to holdid
         move      c1 to pass
         endif
.
         compare   slsown to holdown            .break ?
         if        not equal
         call      totold
         move      slsown to holdown
         move      slslist to holdlist
         move      rectype to holdid
         endif
.
         compare   slslist to holdlist
         goto      nobreak if equal
         move      c4 to pass
         call      totold
         move      slslist to holdlist
         move      rectype to holdid
.
nobreak   MOVE      B1 TO RUNFLAG
          cmatch    "A" to rectype
          goto      readslsa if equal
          cmatch    "B" to rectype
          goto      readpaid if equal
          cmatch    "C" to rectype
          goto      readadj if equal
          cmatch    "D" to rectype
          goto       readsls1 if equal          
          display   *p1:24,*el,*b,"Invalid record type",*b,*w4
          goto      readsls

readslsa ADD       C1 TO COUNTA      
         DISPLAY   *P10:12,"New Sales records PROCESSED: ",COUNTA,b1,slsown,b1,slslist
.
         add       c1 to hit

         COMPARE   C0 TO SLSAP2
         IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE     
         MOVE     YES TO AP2SW
         ENDIF
.
         compare   c1 to hit
         call      preplob if equal
.
.
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      getmore
...............................................................................
.readsls1 - read sales file and get all  ninsls.old 4th pass
READsls1  
         move      c4 to pass
         MOVE      B1 TO RUNFLAG
         ADD       C1 TO COUNTd
         DISPLAY   *P10:15,"NUMBER OF Sales records PROCESSED: ",COUNTD,b1,slsown,b1,slslist
         add       c1 to hit

         compare   c1 to countd     .1st open invoice?
         if        equal            .yes
         compare   countc to c0      .were there adjustments
            if        less
            call      totadj        .yes go print total
            move      c4 to pass
            goto      redsls1a
            endif
            compare   countb to c0   .were there payments?
              if    less
              call      totpaid        .yes
              move      c4 to pass
              goto      redsls1a
              endif
                compare  counta to c0    .no, were there new bills
                call     totbill if less    .yes
                move     c4 to pass
                endif
.
redsls1a COMPARE   C0 TO SLSAP2
         IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE     
         MOVE     YES TO AP2SW
         ENDIF
.
         compare   c1 to hit
         call      preplob if equal
.
.
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      getmore
.............................................................................
readpaid
         move     c2 to pass
         add       c1 to hit
         ADD       C1 TO countB
         DISPLAY   *P10:13,"New payment records PROCESSED: ",countB,b1,slsown,b1,slslist
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         CALL      READPAY    
         compare   c1 to countb
         if        equal
         compare   counta to c0         .need to print new billing totals?
         if         less      .yes   
         call      totbill
         move      c2 to pass
         else
         move      c2 to pass
         call      header
         endif
         endif
.
.
PROCES2
         COMPARE   C0 TO SLSAP2
         IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE     
         MOVE     YES TO AP2SW
         ENDIF

         PACK      MKEY FROM SLSMLR,Z3
         REP       zfill IN MKEY
         CALL      NMLRKEY
         IF        OVER
         PACK      MKEY FROM Z3,Z3,Z3
         CALL      NMLRKEY
         ENDIF
         CALL      READTAX
     
.
         MOVE      "99/99/99" TO DATEPRT1
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsMDTe TO cc,str2,str4
         pack       str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
         MOVE      "99/99/99" TO DATEPRT2
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsiDTe TO cc,str2,str4
         pack      str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT2
         PACK      CHKDATE FROM CHK1dTEM,SLASH,CHK1DTED,SLASH,CHK1DTEY
         move      c0  to invdate
         unpack    slsidte into STR2,YY,mm,dd
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO INVDATE
         MOVE      SLSAP1 TO AP1FORM
         MOVE      YES TO APSW
         COMPARE   APCHECK TO AP1form
         IF          NOT GREATER
         MOVE      NO TO APSW
         ENDIF
         mult      "100" by ap1form
        MOVE    APMASK TO AP1OUT
        EDIT    AP1FORM TO AP1OUT
        ADD     SLSAP1 TO APTOTDET
        MATCH   YES TO AP2SW
        IF      EQUAL
        ADD     SLSAP2 TO APTOTOWN
        ELSE
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
        ADD     SLSAP1 TO APTOTOWN
         ENDIF
        ENDIF
        MATCH   yes TO AP2SW
        IF     EQUAL
         move   slsap2 to ap2form
         mult      "100" by ap2form
        MOVE    APMASK TO AP2OUT
        EDIT    AP2FORM TO AP2OUT
        ENDIF

         MATCH     YES TO AP2SW
         IF        EQUAL
         ADD       SLSAP2 TO APTOTLST
         ELSE      
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         ADD       SLSAP1 TO APTOTLST
         ENDIF
         ENDIF
          MATCH     YES TO AP2SW
         IF         EQUAL
         GOTO      print
         ELSE
         COMPARE    C0 TO PAYCHK
         IF         NOT EQUAL
         GOTO      readsls                    .sales read
         ENDIF
         ENDIF
         MOVE      SLSLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.         RESET     EXCODES
.         SCAN      OELCODE IN EXCODES
.         goto      readsls if equal
         goto      print
.
.............................................................................
readadj
         move      c3 to pass
         add       c1 to countc
         MOVE      SLSLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         CALL      READPAY    
         move      slslr to nordfld
         rep       zfill in nordfld
         call      nordkey
         DISPLAY   *P10:14,"Adjustment records PROCESSED: ",countc,b1,olon,b1,olnum
.
         compare   c1 to countc       .1st adj?
         if        equal
                compare   countb to c0   .yes,  were there payments?
                if    less
                call      totpaid        .yes
                MOVE      C3 TO PASS
                GOTO      READADJ1
                ENDIF
           compare  counta to c0    .no, were there new bills
.           call     totbill if less    .yes
               if       less 
               call     totbill     .yes
               goto     readadj1
               endif 
          call       header
         MOVE       C3 TO PASS
         endif
.
READADJ1 compare   c1 to hit
         if        equal
         call      preplob 
         endif
.
         move      SLSlr to nINVfld
         rep       zfill in nINVfld
         call      nINVkey
         PACK      NJSTFLD FROM INVNUM,SLSADJSW
         REP       ZFILL IN NJSTFLD
         CALL      NJSTKEY
         move       c0 to adjap1
         add        jstap1 to adjap1
.
         compare    c0 to adjap1
         if         not equal
         compare    c0 to ap2
         goto       readsls if not equal
         endif

         move       c0 to adjap2
         add        jstap2 to adjap2
         compare    c0 to adjap2          .if both ap adjs 0 skip
         if         equal
         compare    c0 to adjap1
         goto       readsls if equal
         endif
         compare   c0 to adjap2
         if        not equal
         move      yes to ap2sw
         MOVE    apmask TO AP2OUT
         EDIT    adjap2 TO AP2OUT
         else
         move     no to ap2sw
         MOVE    adjAPMSK TO adjAPOUT
         EDIT     adjap1 TO adjAPOUT
         compare  c0 to adjap1
         goto     readsls if equal
         endif
         move     jstreasn to n2
         clear   nadjtext
         load     nadjtext from n2 of adjres1,adjres2,adjres3,adjres4,adjres5:
                 adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                 adjres13,adjres14,adjres15,adjres16,adjres17,adjres18,adjres19:
                 adjres20,adjres21,adjres22,adjres23,adjres24,adjres25,adjres26:
                 adjres27,adjres28,adjres29,adjres30,adjres31,adjres32,adjres33:
                 adjres34,adjres35,adjres36,adjres37
         compare  "99" to n2
         if      equal
         move    adjres99 to nadjtext
         endif
.
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
         goto      print
.
.............................................................................
getmore
         MOVE      slsLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         MOVE      slsLR TO NinvFLD
         REP       ZFILL IN NinvFLD
         call      ninvkey
         call      readpay
         move      paytn to paychk
         compare   c0 to page
         call      header if equal
.
         REP       ZFILL IN olnum
         MATCH     olnum TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
         MOVE       C0 TO CMPT92
         MOVE       QTYbild TO cmpt92
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
.
...............................................................................
.NOTE THIS TABLE NEEDS TO BE ADJUSTED WHEN EVER SALES PERSONNEL CHANGES.
...............................................................................
.CONVERT SALESPERSONS TO SALES TEAMS.
         MOVE      C0 TO SALESBR
         PACK      SALESNUM FROM OSALES10,OSALES
         MOVE      SALESNUM TO SALESBR
         COMPARE   C0 TO SALESBR
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      LOADOK IF NOT EQUAL
         MOVE      C1 TO SALESBR
         MOVE      C2 TO OELCODE
         GOTO      PROCESS
         ENDIF
.
.                                       2   5  3  4  4  6    ??  1  1
.   LOAD      SALESNUM FROM SALESBR OF LISA,BO,SA,EM,NP,INES,??,JC,TM
.                   1  5  7  3  7   ?   2   ?   7  6  5  3
.                   GH,JP,JE,MG,SMM,???,BM,???,BT,MD,LM,LT
.
...............................................................................
.
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM1,TEAM1,TEAM2:
                   TEAM2,TEAM3,TEAM2,TEAM1:
                   TEAM1,TEAM1,TEAM1,TEAM2:
                   TEAM1,TEAM2,TEAM2,TEAM1:
                   TEAM2,TEAM2,TEAM3,TEAM1,TEAM1,TEAM1
         MOVE      SALESNUM TO SALESBR
...............................................................................
.
PROCESS
         MOVE      PPM TO CMPT92
         MOVE      CMPT92 TO FORM32
.
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
.
.
         MOVE      SLSAP1 TO AP1FORM
         MOVE      SLSAP2 TO AP2FORM

         MOVE      YES TO APSW
         COMPARE   APCHECK TO AP1form
         IF          NOT GREATER
         MOVE      NO TO APSW
         ENDIF

        MOVE    APMASK TO AP1OUT
        mult      "100" by ap1form
        EDIT    AP1FORM TO AP1OUT
.

        MOVE    APMASK TO AP2OUT
        mult      "100" by ap2form
        EDIT    AP2FORM TO AP2OUT
.        MOVE    APMASK TO AP2OUT
.        EDIT    slsap2 TO AP2OUT

         COMPARE   C0 TO SLSAP2
         IF      EQUAL
         MOVE    NO TO AP2SW
         ELSE     
         MOVE     YES TO AP2SW
         ENDIF
.
         move      c2 to tdmcflag
         MOVE      NORDFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
.begin patch 1.8
         move      lrn to nshpfld
         move      no to shipsw
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         move      olnum to ndatfld
         call      ndatkey
         call      wipecvars
.end patch 1.8
         CALL      COMPUTE
.
*......................................................................
.
PRINT     if       (row > 7100)
          call      Header
          endif
.               COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        equal
         move      b1 to innets
         else
         move      "*" to innets
         endif
         MOVE      "99/99/99" TO DATEPRT1
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    SlsMDTe TO cc,str2,str4
         pack       str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
         MOVE      PPM TO CMPT92
         MOVE      MASK32 TO M$PPM
        MOVE      CMPT92 TO FORM32
         EDIT      FORM32 TO M$PPM
        MOVE      QTYbild TO FORM9
         MOVE      MASK9 TO M$QTY
         EDIT      FORM9 TO M$QTY
         MOVE      oQTY TO FORM9
         MOVE      MASK9 TO M$oQTY
         EDIT      FORM9 TO M$oQTY
         move        c0 to arform
         move        slsar to arform
         branch    pass of dt1,dt2,dt3,dt4
..............................................................................
.dt1 - new billing detail print section |
.........................................
dt1    
        clear      str40
        move       mcomp to str40
.        cmatch     yes to exflag
.        if         equal
.        call       exprint
.        add        "200" to Row           
.        add        c1 to lines
.        goto       readsls
.        endif
.begin patch    2.4
               IF             (HldArFlg = "T")
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*left,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1:
                   *p=5500:row,*Alignment=*right,M$Oqty:
                   *p=6025:row,M$qty,*Alignment=*left
        else
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*left,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3175:row,DatePrt1:
                   *p=5500:row,*Alignment=*right,M$Oqty:
                   *p=6025:row,M$qty,*Alignment=*left:
                   *p=6750:row,"@",m$ppm
         endif
         MATCH     YES TO AP2SW
         IF        EQUAL
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
         move      slsap2 to aplotus
         ADD     SLSAP2 TO APTOTOWN
         ELSE
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
         move      slsap1 to aplotus
         ADD       SLSAP1 TO APTOTOWN
                   If (exflag <> Yes)
                   ADD       SLSAP1 TO APTOTwnr
                   endif
         else
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
         move      slsap2 to aplotus
         ADD       SLSAP2 TO APTOTOWN
                   If (exflag <> Yes)
                   ADD       SLSAP2 TO APTOTwnr
                   endif
         ENDIF
         ENDIF
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,oqty:                    
                      comma,qtybild:
                      comma,m$ppm:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1
         endif
.
chkdet
         cmatch      "T" to hldarflg
         if          equal
.         move        armask to arout
.         EDIT        gross TO AROUT

         move         gross to nincomm
.         MULTIPLY  ".10" BY nincomm
         Mult        ".30" by NINComm
         move         gross to brkcom
         MULTIPLY  ".20" BY brkcom
         Sub         Brkcom from NinComm
         move        gross to lstcomm
         sub         brkcom from lstcomm
         sub         nincomm from lstcomm
         move        slsap1 to slccomm
         sub         lstcomm from slccomm
.begin patch 2.4
               Move           c0 to Grossmlr
               add            Gross to Grossmlr
               add            Slccomm to grossmlr
               add            nininc to grossmlr
.end patch 2.4
         move        grossmsk to grossout
.begin patch 2.4
.         EDIT        gross TO grossout
         EDIT        grossMlr TO grossout
.end patch 2.4
.test dlh
               Move        SlcComm to SlcCommd
               call        trim using slccommd
.test dlh


.         EDIT        arform TO AROUT
.         PRINT       hpt825,arout:
.test dlh
.         PRINT       hpt600,hpdtch85,lstcomm,slccomm,hpt700,hpdtch85,hpuprght,grossout,hpt740,hpdtch85,hpuprght,brkcom:
.                     hpt800,hpdtch85,hpuprght,nincomm,hpt850,hpdtch85,hpuprght,nininc,hpt950,hpdtch85,hpuprght,TAXPRT
.begin patch 2.4
.    PrtPage     Laser;*p=6000:row,*Alignment=*Right,lstcomm," /",Slccommd:
               If             (exflag = Yes)      ;        cmatch     yes to exflag
                              If        (Grossmlr = slcComm+lstcomm+brkcom+nincomm+nininc)
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                                             else
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,*boldon,*ulon,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left,*boldoff,*uloff
                                             endif
               ADD            SlcComm TO APTOTWNX
               add            SlcComm to SlcCommTot
               add            GrossMlr to GrossoutTot
               endif
               IF             (Exflag <> Yes)
                               If        (Grossmlr = slcComm+lstcomm+brkcom+nincomm+nininc)
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8750:row,*Alignment=*Right,BrkCom:
                                             *p=9250:row,*Alignment=*Right,NINComm:
                                             *p=9800:row,*Alignment=*Right,NINinc:
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                                             else
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,*boldon,*ulon,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8750:row,*Alignment=*Right,BrkCom:
                                             *p=9250:row,*Alignment=*Right,NINComm:
                                             *p=9800:row,*Alignment=*Right,NINinc:
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left,*boldoff,*uloff
                              endif
                endif
.begin patch 2.41
               add            Lstcomm to LstcommTot
               add            SlcComm to SlcCommTot
               add            GrossMlr to GrossoutTot
               add            Brkcom to BrkComTot
               add            NINComm to NINCommTot
               add            NINInc  to NINIncTot
.end patch 2.41
.          PRINT       hpt600,hpdtch85,lstcomm,hpbon,"/",hpboff,slccommd,hpt700,grossout,hpt750,brkcom:
.                     hpt825,nincomm,hpt875,nininc,hpt950,hpprop,hpdtch85,hpuprght,TAXPRT
.test dlh
.                     hpt950,hpdtch85,hpuprght,TAXPRT
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
         else
    PrtPage           Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.         PRINT       hpt825:
.                      hpt950,TAXPRT
         endif
         add       "200" to Row
.         add       c1 to lines
.end patch 2.4
         goto      readsls
.............................................................................
.dt4 - detail print for previously billed/open invoices |
.........................................................
dt4
        clear      str40
        move       mcomp to str40
.

.        cmatch     yes to exflag
.        if         equal
.        call       exprint
.        add        "200" to row
.        goto       readsls
.        endif
.
        cmatch      "T" to hldarflg
         if          equal
.begin patch 2.4
         PrtPage    Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*Left,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1:
                   *p=5500:row,*Alignment=*right,M$Oqty:
                   *p=6025:row,M$qty,*Alignment=*left
.        PRINT     *3,hpdtch70,hpuprght,str40,*flush:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1:
.                   hpt510,m$oqty,hpt540,m$qty;
        else
        PrtPage               Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*Right,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1:
                   *p=5250:row,*Alignment=*right,M$Oqty:
                   *p=6025:row,M$qty:
                   *p=6750:row,"@",m$ppm,*Alignment=*Left
.        PRINT     *3,hpdtch70,hpuprght,str40,*flush:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1:
.                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm;
         endif
.end patch 2.4
.         PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1;
.per sa/lp 25mar96 DLH
.                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm;
         MATCH     YES TO AP2SW
         IF        EQUAL
         ADD     SLSAP2 TO APTOTOWN
                   If (exflag <> Yes)
                   ADD       SLSAP2 TO APTOTwnr
                   endif
        PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
.         PRINT     hpt400,AP2OUT;
         move      slsap2 to aplotus
         else
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         ADD       SLSAP1 TO APTOTOWN
                   If (exflag <> Yes)
                   ADD       SLSAP1 TO APTOTwnr
                   endif
.begin patch 2.4
         PrtPage       Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
.         PRINT        hpt400,AP1OUT;
.end patch 2.4
         move      slsap1 to aplotus
.begin patch 1.8
.         div       hund into aplotus
.end patch 1.8
.         move      slsap1 to aplotus
         else
         ADD       SLSAP2 TO APTOTOWN
                   If (exflag <> Yes)
                   ADD       SLSAP2 TO APTOTwnr
                   endif
.begin patch 2.4
               PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
.         PRINT     hpt400,AP2OUT;
.end patch 2.4
         move      slsap2 to aplotus
.begin patch 1.8
.         div       hund into aplotus
.end patch 1.8
.         move     slsap2 to aplotus
         ENDIF
         ENDIF

         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"O",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1
          endif

          cmatch      "T" to hldarflg
          if          equal
.         move        armask to arout
.         EDIT        gross TO AROUT
               If             (ExpRepFlag = Yes)               
               packkey        NinvFld from SLSLR
               rep            zfill in Ninvfld
               move           c1 to ninvpath
               call           Ninvkey
.               PackKey        Nadjfld from SLSLR
.               rep            zfill in Nadjfld
               Move           c0 to Holdrecadj
               Move           c0 to Holdrecadj1
               move           c0 to Holdlrinc
               move           c0 to HoldNinInc
               MOVE           C1 TO N2
.               If             (slslr = "450633" or SLSlr = "473589")
               If             (SLSlr = "450468")
               call           debug
               endif

DETADJ4  
               PACKkey      NJSTFLD FROM INVNUM,n2
               REP       ZFILL IN NJSTFLD
               CALL      NJSTKEY
               GOTO      DetAdj4x IF OVER
               If             (JstReasn <> "27" & JstReasn <> "16")        .not prepayment or shortpay
               add            JstAr to HoldRecAdj
               add            JstLRInc to HoldLrInc
               add            JstNINInc to HoldNinInc
               endif
               If             (JstReasn = "03")        .Select
               add            JstAr to HoldREcadj1
               endif

               ADD       C1 TO N2
               GOTO      DETADJ4
               ENDIF         
Detadj4x
.               Add            HoldRECADJ to Gross
               endif
.begin patch 2.4

               If             (Ostat <> "Q")             ;billed and cancelled?
               move         gross to nincomm             ;no calc commissions
.              MULTIPLY  ".10" BY nincomm
               Mult        ".30" by NINComm
               move         gross to brkcom
               MULTIPLY  ".20" BY brkcom
               Sub         Brkcom from NinComm
               Add         HoldRECADJ to Gross
.dave goes way south 12/2/02
               Sub         HoldRECADJ1 from Gross
.end dave goes way south 12/2/02
               move        gross to lstcomm
               Else
               move           C0 to lstcomm
               move           C0 to NINComm             ;Yes, No commissions
               move           C0 to BrKCom
               endif
.end patch 2.4
         sub         brkcom from lstcomm
         sub         nincomm from lstcomm
         move        slsap1 to slccomm
         sub         lstcomm from slccomm
.begin patch 2.4
.               If             (ExpRepFlag = Yes)
.               add            HoldNinInc to Nininc
.               endif

               Move           c0 to Grossmlr
               If             (Ostat = "Q")             ;billed and cancelled?
               add            Slccomm to grossmlr
               add            HoldNinInc to Nininc
               add            nininc to grossmlr
               else
               add            HoldLRinc to NINComm
               add            HoldNinInc to Nininc
.dave goes wild 11/04/2002
.                     if             (nincomm < 0)
                     add            HoldLRinc to GrossMlr         .if list manager ate cost do not reflect in gross billings
.                     endif
.end dave goes wild 11/04/2002
               add            Gross to Grossmlr
               add            Slccomm to grossmlr
               add            Nininc to grossmlr
               endif

.end patch 2.4
         move        grossmsk to grossout
.begin patch 2.4
.         EDIT        gross TO grossout
         EDIT        grossMlr TO grossout
.end patch 2.4

.test DLH
         Move        slccomm to slccommd
         call        trim using slccommd
.test DLH


.         EDIT        arform TO AROUT
.         PRINT       hpt825,arout:
.test dlh
.         PRINT       hpt600,hpdtch85,lstcomm,slccomm,hpt700,hpdtch85,hpuprght,grossout,hpt740,hpdtch85,hpuprght,brkcom:
.                     hpt800,hpdtch85,hpuprght,nincomm,hpt850,hpdtch85,hpuprght,nininc,hpt950,hpdtch85,hpuprght,TAXPRT
.begin patch 2.4


               If             (exflag = Yes)      ;        cmatch     yes to exflag
                              If        (Grossmlr = slcComm+lstcomm+brkcom+nincomm+nininc)
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                                             else
                              PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,*boldon,*ulon,lstcomm:
                                             *p=7250:Row,*Alignment=*Right,SlcCommd:
                                             *p=8125:Row,*Alignment=*Right,grossout:
                                             *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left,*boldoff,*uloff
                                             endif
               ADD            SlcComm TO APTOTWNX
               add            SlcComm to SlcCommTot
               add            GrossMlr to GrossoutTot
               endif
               IF             (HldArFlg = "T" & Exflag <> Yes)
.               cmatch      "T" to hldarflg
.               if          equal
                              If        (Grossmlr = slcComm+lstcomm+brkcom+nincomm+nininc)
               PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,lstcomm:
                              *p=7250:Row,*Alignment=*Right,SlcCommd:
                              *p=8125:Row,*Alignment=*Right,grossout:
                              *p=8750:row,*Alignment=*Right,BrkCom:
                              *p=9250:row,*Alignment=*Right,NINComm:
                              *p=9800:row,*Alignment=*Right,NINinc:
                              *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                              else
               PrtPage     Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85,*boldon,*ulon,lstcomm:
                              *p=7250:Row,*Alignment=*Right,SlcCommd:
                              *p=8125:Row,*Alignment=*Right,grossout:
                              *p=8750:row,*Alignment=*Right,BrkCom:
                              *p=9250:row,*Alignment=*Right,NINComm:
                              *p=9800:row,*Alignment=*Right,NINinc:
                              *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left,*boldoff,*uloff
                              endif
.begin patch 2.41
               add            Lstcomm to LstcommTot
               add            SlcComm to SlcCommTot
               add            GrossMlr to GrossoutTot
               add            Brkcom to BrkComTot
               add            NINComm to NINCommTot
               add            NINInc  to NINIncTot
.end patch 2.41
.         PRINT       hpt600,hpdtch85,lstcomm,hpbon,"/",hpboff,slccommd,hpt700,grossout,hpt750,brkcom:
.                     hpt825,nincomm,hpt875,nininc,hpt950,hpprop,hpdtch85,hpuprght,TAXPRT
.test dlh
.                     hpt950,hpdtch85,hpuprght,TAXPRT
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
         else
.    PrtPage           Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.         PRINT       hpt825:
.                      hpt950,TAXPRT
.end patch 2.4
         endif
.begin patch 2.4
.         add       c1 to lines
               add            "200" to Row
.end patch 2.4
         goto      readsls
..............................................................................
.dt2 - detail print section for new payments made.|
...................................................
dt2 
         match      no to apsw
         goto       prt3a2 if equal
        clear      str40
        move       mcomp to str40
          add            "200" to row
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1
.         PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,slslr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1;
GOODONE2
         MATCH     YES TO AP2SW
         IF        EQUAL
.         print     hpt400,ap2out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.begin patch 2.4
          PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
.         PRINT     hpt400,AP2OUT;
.end patch 2.4
         MOVE      CHKN2 TO CHKN1
         move      slsap2 to aplotus
.begin patch 1.8
.         div       hund into aplotus
.end patch 1.8
.         move      slsap2 to aplotus
         ELSE
.         print     hpt400,ap1out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.begin patch 2.4
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
.         PRINT     hpt400,AP1OUT;
.end patch 2.4
         move      slsap1 to aplotus
.begin patch 1.8
.         div       hund into aplotus
.end patch 1.8
.         move      slsap1 to aplotus
         endif
         move        armask to arout
         add         arform to artotlst
         EDIT        arform TO AROUT
         PrtPage        Laser;*p=7500:row,*Alignment=*Left,ChkDate:
                        *p=8600:row,*Alignment=*Right,chkn1,*Alignment=*Left
.         PRINT       hpt750,CHKDATE,b1,chkn1;                    CHKN1;
.         cmatch      "T" to hldarflg
.         if          equal
.         PRINT       hpt700,hpdtch85,hpuprght,lrinc,hpt775,hpdtch85,hpuprght,brkcom:
.                     hpt825 hpdtch85,hpuprght,nininc,hpt875,hpdtch85,hpuprght,arout,hpt950,hpdtch85,hpuprght,TAXPRT
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
.         else
    PrtPage             Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.         PRINT       hpt875:
.                      hpt950,TAXPRT
.end patch 2.4
.         endif
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,chkdate:
                      comma,chkn1
         endif
prt3a2
.begin patch 2.4
.              ADD       C1 TO lines
.               add            "200" to Row
.end patch 2.4
         MOVE      C1 TO JSTN
         MOVE      SLSLR TO NINVFLD
         CALL      NINVKEY
DETADJ2  
         PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      readsls IF OVER
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO JSTN
         GOTO      DETADJ2
         ENDIF         
.begin patch 1.9
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        add        form92 to aptotown            *new
.        mult       seq by form92
.        div        hund into form92
        move       c0 to adjap
.        add        form92 to adjap
         add        jstap1 to adjap
         add        jstap1 to aptotown
.end patch 1.9

        move       adjapmsk to adjapout
         move      adjap to aplotus
.         div       hund into aplotus
.        move       adjap to aplotus
        edit       adjap to adjapout
        CMATCH     YES TO AP2SW
        IF          EQUAL
        GOTO       CONVAP2
        ENDIF
        COMPARE    C0 TO ADJAP
        IF         NOT EQUAL
        ADD        ADJAP TO MANPAY
        move       adjap to aplist
        mult       hund into aplist
        add        aplist to aptotlst
        ENDIF
CONVAP2
.begin patch 1.9
.       MOVE       C0 TO CVTFLD
.        move       jstap2 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        add        form92 to aptotown            *new
.        div        hund into form92
.        mult       seq by form92
..START PATCH #1.7 - INCREASED VAR
..        move       c0 to form72
..        add        form92 to form72
..        move       apmask to ap2out
..        edit       form72 to ap2out
..         move      form72 to aplotus
..         div       hund into aplotus
...        move       form72 to aplotus
..        cmatch     yes to ap2sw
..        if         equal
..        compare    c0 to form72
..        goto       readsls if equal
..        endif
..        ADD        FORM72 TO MANPAY
..        move       c0 to adjap
..        add        form72 to adjap
...
        move       c0 to CMPT92
.        add        form92 to CMPT92
         add        jstap2 to cmpt92
.end patch 1.9

        move       apmask2 to ap2out    .APMASK2 - NEW VAR WHICH WILL HOLD NEW VALUE OF CMPT92
        edit       CMPT92 to ap2out
         move      CMPT92 to aplotus
.         div       hund into aplotus
.        move       CMPT92 to aplotus
        cmatch     yes to ap2sw
        if         equal
        compare    c0 to CMPT92
        goto       readsls if equal
        endif
        ADD        CMPT92 TO MANPAY
        move       c0 to adjap
        add        CMPT92 to adjap    
.END PATCH #1.7 - INCREASED VAR
        COMPARE    C0 TO ADJAP
        IF         NOT EQUAL
        move       adjap to aplist
        mult       hund into aplist
        add        aplist to aptotlst
        ENDIF
         if        (row > 7100)
         call       Header
         endif
.        COMPARE   "44" to lines
.         CALL      HEADer IF NOT LESS
              add            "200" to row
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2000:row,*Alignment=*right,*font=prtpg85,slslr:
                   *p=2500:row,Invnum:
                   *p=3200:row,DatePrt1,*Alignment=*Left
.
.             PRINT     *3,hpdtch85,hpuprght,str40:
.                  hpt200,hpdtch85,hpuprght,b1,slslr:
.                  hpt250,b2,INVNUM:
.                  hpt325,b4,DATEPRT1;
.START PATCH #1.7 - INCREASED VAR
.         COMPARE   C0 TO form72
         COMPARE   C0 TO CMPT92
.END PATCH #1.7 - INCREASED VAR
         IF        NOT EQUAL
.         print     hpt400,ap2out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.begin patch 2.4
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
.         PRINT     hpt400,AP2OUT;
.end patch 2.4
         MOVE      "MANUAL" TO CHKN1
         ELSE
.         print     hpt400,ap1out,hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm; 
.begin patch 2.4
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
.         PRINT     hpt400,AP1OUT;
.end patch 2.4
         ENDIF
         MOVE      "MANUAL" TO CHKN1
         UNPACK    JSTDATE INTO MM,DD,YY
         PACK      CHKDATE FROM MM,SLASH,DD,SLASH,YY
         move        c0 to arform
         move        slsar to arform
         move        armask to arout
        add         arform to artotlst
         EDIT        arform TO AROUT
.begin patch 2.4

.              PRINT       hpt750,CHKDATE,b1,CHKN1;
         cmatch      "T" to hldarflg
         if          equal

         PrtPage               Laser;*p=6000:row,*Alignment=*Right,*font=prtpg85,lstcomm," /",Slccommd:
                              *p=7000:Row,Lrinc:
                              *p=7750:row,BrkCom:
                              *p=8250:row,NININc:
                              *p=8750:row,ArOut:
                              *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.
.         PRINT       hpt700,hpdtch85,hpuprght,lrinc,hpt775,hpdtch85,hpuprght,brkcom:
.                     hpt825 hpdtch85,hpuprght,nininc,hpt875,hpdtch85,hpuprght,arout,hpt950,hpdtch85,hpuprght,TAXPRT
.         PRINT       hpt850,hpdtch85,hpuprght,arout:
.                      hpdtch85,hpuprght,hpt950,TAXPRT
         else
    PrtPage            Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.         PRINT       hpt875:
.                      hpt950,TAXPRT
.end patch 2.4
          endif
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,chkdate:
                      comma,chkn1
         endif
         add       "200" to row
.        ADD       C1 TO lines
         GOTO      readsls
.
*............................................................
.DT3  - ADJUSTMENTS FOR THE MONTH      |
........................................
dt3
        clear      str40
        move       mcomp to str40
.begin patch 2.4
        PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*left,*font=prtpg85,jstlr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1:
                   *p=7500:row,*Alignment=*left,nadjtext
.        PRINT     *3,hpdtch70,hpuprght,str40:
.                   hpt200,hpdtch85,hpuprght,b1,jstlr:
.                   hpt250,b2,INVNUM:
.                   hpt325,b4,DATEPRT1:
.                   hpdtch85,hpuprght,hpt750,nadjtext,hpdtch85,hpuprght;
.;                   hpt525,m$oqty,hpt600,m$qty,hpt675,"@",m$ppm:
.;                   hpdtch85,hpuprght,hpt750,nadjtext,hpdtch85,hpuprght;
.end patch 2.4
         MATCH     YES TO AP2SW
         IF        EQUAL
.         MOVE    apmask TO AP2OUT
.         edit    slsap2 to ap2out
.begin patch 2.4
               PrtPage       Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,Adjap2,*Alignment=*Left
.         PRINT     hpt400,Adjap2
.end patch 2.4
         move     adjap2 TO APlotus
.         move      slsap2 to aplotus
.         move     c0 to form92
.         move     adjap2 to form92
.         mult     "100" by form92
.         ADD     form92 TO APTOTOWN
         ADD     SLSAP2 TO APTOTOWN
         ELSE
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
.         move     c0 to form92
.         move     adjap1 to form92
.         mult     "100" by form92
.begin patch 1.9
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92         *added 7/24/98 jd

.        add        form92 TO APTOTOWN
         add     jstap1 to aptotown
.end patch 1.9

.        ADD     SLSAP1 TO APTOTOWN        *turned off 7/24/98 jd
.         MOVE    apmask TO AP1OUT
.         edit    slsap1 to ap1out
.begin patch 2.4
         PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,ADjAPOUT,*Alignment=*Left
.         PRINT     hpt400,AdjAPOUT;
.end patch 2.4
         move     adjap1 TO APlotus
.         move     slsap1 to aplotus
         else
.         PRINT        hpt400,AP1OUT;
.         ADD     SLSAP2 TO APTOTOWN
         ENDIF
         ENDIF
.         PRINT       hpt875:
.                      hpt950,TAXPRT
         PrtPage               Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.end patch 2.4
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"A",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b6:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1:
                      comma,nadjtext
         endif
         add       "200" to row
.        add       c1 to lines
         goto      readsls
.............................................................................
HEADER
         ADD       C1 TO PAGE
         branch    pass of hd1a,hd2a,hd3a,hd4a 
*............................................................
hd1a     
.        MOVE      c10 TO LINES
               Move           "1300" to row
.         MOVE      c5 TO LINES
.begin patch 2.0
.         PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"New billings for list:":
..                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.begin patch 2.4
.               PRINT     *f;  
               PrtPage        Laser;*newpage
.               CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         call       prtform2
.         else
.         call      prtform
.         endif
         call      prtform
.
               call           PrtOwner
                PrtPage       Laser;*alignment=*Right,*p=6875:725,"New Billing for List:",*alignment=*left

.               print      033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"New billings for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.end patch 2.4
.end patch 2.0
         CMATCH    "T" TO HLDARFLG
         IF        EQUAL
.begin patch 2.4
.               PrtPage        Laser;*p=7774:1235,*font=prtpg9bi,"Billed to":
.                              *p=7774:1355,*font=prtpg9bi,"Mailer":
.begin release 2.41
.               PrtPage        Laser;*p=7649:1235,*font=prtpg9bi,"Billed to":
.                              *p=7649:1355,*font=prtpg9bi,"Mailer":
               PrtPage        Laser;*p=7649:1235,*font=prtpg9bi,"Gross":
                              *p=7649:1355,*font=prtpg9bi,"Billing":
.end release 2.41
                              *p=8375:1610,"------------List Costs-----------":
                                 *p=8500:1235,"Brk":
                                 *p=8500:1365,"Comm":
                                 *p=9000:1235,"Lstm":
                                 *p=9000:1365,"Comm":
                                 *p=9600:1365,"Slct":
                                 *PENSIZE=10,*p=7475:1250,*Line=7475:8000:             ;extra 
                                 *PENSIZE=10,*p=8225:1250,*Line=8225:8000              ;around grossmlr
 
 
.;               PRINT    033,"(8U",033,"(s1p07.00v1s+2b5T",hpt715,"Billings",hpt750,"Brk Comm",hpt820,"Lstm Comm",HPT875,"Slct Income":
.                   hpboff,hpuprght,*flush;
         else
.         print     hpboff,hpuprght,*flush;
.end patch 2.4
         endif
         compare   c1 to page
         if        equal
         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund by aplotown
.         move      c0 to aptotown
.1625
.begin patch 2.4
               Move           "1625" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
               add            "400" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
.               Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk,*n,*n,*3,"New Billing:":
.                   hpdtch85,hpuprght,*flush
.         add       c5 to lines
               add            "200" to Row              s
.end patch 2.4
               cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         endif
.
         compare   c1 to page
         if        not equal
.begin patch 2.4
               add            "400" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
.         Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"New Billing: ":
.                   hpdtch85,hpuprght,*flush
              add            "200" to row
 ;         add       c3 to lines
.end patch 2.4
         endif
         RETURN
*............................................................
hd4a 
               Move           "1300" to row
.              MoVE      c10 TO LINES
.begin patch 2.0
.          PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"Open Invoices as of end of month:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.begin patch 2.4
.print     *f;
               PRtPage        Laser;*Newpage
.          CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         call       prtform2
.         else
.         call      prtform
.         endif
               call           PrtForm
               call           PrtOwner
               PrtPage       Laser;*Alignment=*Right,*p=6875:725,"Current Open Invoices:",*Alignment=*Left
               add            "200" to row
.               PRINT     033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"Open Invoices as of end of month:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.end patch 2.0
.         CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         PRINT     HPT900,"Billed":
.                   hpboff,hpuprght,*flush;
.         else
.         print     hpboff,hpuprght,*flush;
.         endif

         CMATCH    "T" TO HLDARFLG
         IF        EQUAL
.begin patch 2.4
               PRTPAGE     Laser;*p=5100:1235,*font=prtpg9bi,"Order":
                           *p=5100:1365," Qty":
                           *p=5600:1235,*font=prtpg9bi,"Billed":
                           *p=5600:1365," Qty":
                           *p=6350:1365,"  List/Select":
                           *p=6350:1235,"Owner Income"
.begin patch 2.4
.               PrtPage        Laser;*p=7774:1235,*font=prtpg9bi,"Billed to":
.                              *p=7774:1355,*font=prtpg9bi,"Mailer":
.begin release 2.41
.               PrtPage        Laser;*p=7649:1235,*font=prtpg9bi,"Billed to":
.                              *p=7649:1355,*font=prtpg9bi,"Mailer":
               PrtPage        Laser;*p=7649:1235,*font=prtpg9bi,"Gross":
                              *p=7649:1355,*font=prtpg9bi,"Billings":
.end release 2.41
                              *p=8375:1610,"------------List Costs-----------":
                                 *p=8500:1235,"Brk":
                                 *p=8500:1365,"Comm":
                                 *p=9000:1235,"Lstm":
                                 *p=9000:1365,"Comm":
                                 *p=9600:1365,"Slct":
                                 *PENSIZE=10,*p=7475:1250,*Line=7475:8000:             ;extra 
                                 *PENSIZE=10,*p=8225:1250,*Line=8225:8000              ;around grossmlr
.         PRINT    033,"(8U",033,"(s1p07.00v1s+2b5T",hpt715,"Billings",hpt750,"Brk Comm",hpt820,"Lstm Comm",HPT875,"Slct Income":
.                   hpboff,hpuprght,*flush;
         else
.         print     hpboff,hpuprght,*flush;
         endif
.end patch 2.4
         compare   c0 to counta
         if        equal
         compare   c1 to page
         if        equal 
         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund by aplotown
.         move      c0 to aptotown
.begin patch 2.4
               Move           "1625" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,Ownermsk
               add            "500" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
               add            "200" to row
. Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"
.         add       c4 to lines
.end patch 2.4


         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         endif
         endif

         compare   c0 to countb
         if        equal
         add        "500" to Row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
.         print      *n,*3,"No new Payments this month"
         add        "200" to Row
 ;        add       c2 to lines
         endif

         compare   c0 to countc
         if        equal
         add        "200" to Row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"No Adjustments to Payments this month.",*font=prtpg9
         add        "400" to Row
.        add       c2 to lines
.         print     *n,*3,"No Adjustments to payment this month":
.                   hpdtch85,hpuprght,*flush
         endif

         add        "500" to Row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Open invoices as of end of month.",*font=prtpg9
         add        "200" to Row
.         Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Open invoices as of end of month : ":
.                   hpdtch85,hpuprght,*flush
         RETURN
*............................................................
hd2a
               Move           "1300" to row
.              MOVE      c10 TO LINES
.begin patch 2.0
.          PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"New payments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon,"Check date & Number";
.Start Patch 2.13
           MATCH   YES TO AP2SW
           IF      EQUAL
           call    nownkey
           endif
. turned off 7/31/01 need to check out timing.
.         pack      nlobfld from slsown,slslist,DATEKEY
.         rep       zfill in nlobfld
.         display   *p1:24,*el,*b,"at prepok",nlobfld,*b,*w10
.         move      slsown to holdown
.         move      slslist to holdlist
.         move      slsown to nownfld
.         move      slslst1 to holdlst1
.         move      slslr to ninvfld
.         move      arflag to hldarflg
.         call      ninvkey
.         CALL      READPAY
.         move      c0 to lobbal
.                 call      nlobkey
.                 if        over
.                 move      "T" to newflag
.                 else
.                 move      "F" to newflag
.                 endif
.End Patch 2.13
.begin patch 2.4
 ;         PRINT     *f;
               prtPage        Laser;*newpage
                call      prtform
                call          PrtOwner
                PrtPage       Laser;*alignment=*Right,*p=6875:725,"New Payments for List:",*Alignment=*Left:
                              *p=7500:1300,*font=prtpg9bi,"Check date & Number"
.                print     033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"New payments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon,"Check date & Number";
.end patch 2.4
.end patch 2.0
.         CMATCH    "T" TO HLDARFLG
.         IF        EQUAL
.         PRINT     HPT900,"Billed":
.                   hpboff,hpuprght,*flush;
.         else
.begin patch 2.4
.         print     hpboff,hpuprght,*flush;
.end patch 2.4
.         endif

         compare   c0 to counta
         if        equal
         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund by aplotown
.         move      c0 to aptotown
.begin patch 2.4
               Move           "1625" to row               
.add            "500" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,Ownermsk
               add            "500" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
               add            "200" to Row
.Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"

.         add       c4 to lines
.end patch 2.4

         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         endif
               add            "500" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"Payments during the month:",*font=prtpg9

.         print     *n,*n,*3,hpdtch85,hpuprght,"Payments during the month:",hpdtch85,hpuprght
         RETURN
*............................................................
hd3a
               Move           "1300" to row
.              MOVE      c10 TO LINES
.begin patch 2.0
.         PRINT     *f,*n,hpOWNST1,hpdtch85,hpuprght,hpt1000,TODAY:
.                     *L,hpt1000,PAGE:
.                   *l,*11,OWNLON:
.                   *L,*11,OWNLONM,hpt700,"Payment adjustments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.         print     hpboff,hpuprght,*flush;
.Start Patch 2.15
           MATCH   YES TO AP2SW
           IF      EQUAL
           call    nownkey
           endif

.End Patch 2.15
               PrtPage        Laser;*Newpage
.              PRINT     *f;
         call       prtform
         call       PrtOwner
         PrtPage       Laser;*alignment=*Right,*p=6875:725,"Payment adjustments for list:",*Alignment=*left
.         print      033,"&a0c0R",*n,hpdtch85,hpuprght,hpt1000,TODAY:
.                   *l,*11,OWNLON,hpt1000,PAGE:
.                   *L,*11,OWNLONM,hpt700,"Payment adjustments for list:":
.                   *L,*11,ownocpy,hpt700,holdlst1:
.                   *L,*11,OWNLOSA,hpt700,holdlist:
.                   *L,*11,OWNLOCTY,*28,OWNLOS,*32,OWNLOZC:
.                   *N,*n,hpt750,hpdtch85,hpuprght,hpitalic,hpbon;
.         print     hpboff,hpuprght,*flush;

.end patch 2.0
         compare   c0 to counta
         if        equal
         compare   c1 to page
         if        equal
         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund by aplotown
.         move      c0 to aptotown
.begin patch 2.4
               Move            "1625" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,Ownermsk
               add            "500" to row
               PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
               add            "200" to row
. Print     hpdtch85,hpuprght,*n,*n:
.                   *3,"Opening Balance : ",hpt400,ownermsk:
.                   *n,*3,"No new Billing this month"
.         add       c4 to lines
.end patch 2.4

         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b4:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         endif
         endif

         compare   c0 to countb
         if        equal
.begin patch 2.4
         add            "500" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
.         print     *n,*3,"No new Payments this month"
.         add       c2 to lines
         add            "500" to row
         endif
         add            "200" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Payment Adjustments :",*font=prtpg9
.         print     *n:
.                   *n,*3,"Payment Adjustments :",hpdtch85,hpuprght,*flush
         add            "200" to row
.end patch 2.4
         RETURN
.
*............................................................
.begin patch 1.9
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.end patch 1.9
..........................................................................
.
.totbILL - done with new invoices let's get new payments
totbiLL  compare   c0 to countA             -did we have any new?
         if        equal                -no
         move      c0 to aptotown
         move      c0 to page
         move      c2 to pass
         return
         endif
         move      aptotown to form102
.         mult      ".01" by form102
         add       form102 to lobbal
         MOVE      TOTOMSK TO OWNERMSK
         mult      "100" by aptotown
         EDIT      APTOTOWN TO OWNERMSK
         MOVE      TOTOMSK TO OWNRMSKx
         mult      "100" by aptotwnx
         EDIT      APTOTWNx TO OWNRMSKx

         MOVE      TOTOMSK TO OWNRMSKR
         mult      "100" by aptotwnr
         EDIT      APTOTWNr TO OWNRMSKr
         if        (row > 7100)
         call       Header
         endif

         compare   c0 to aptotwnx
         if        not equal
         add            "200" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total New Income +",*p=4000:row,OwnerMsk:
                              *p=6750:row,*Alignment=*Right,*font=prtpg85,lstcommTot:
                              *p=7250:Row,*Alignment=*Right,SlcCommTot:
                              *p=8125:Row,*Alignment=*Right,grossoutTot:
                              *p=8750:row,*Alignment=*Right,BrkComTot:
                              *p=9250:row,*Alignment=*Right,NINCommTot:
                              *p=9800:row,*Alignment=*Right,NINincTot,*Alignment=*Left
           call               ZeroTotals    
         add            "200" to row
        PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
                       *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
                       add            "200" to row
         PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Rental",*p=4000:row,OwnerMsk:
                        *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left,*font=prtpg9
         else
         add            "200" to row
.begin patch 2.41
.         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total New Income +",*p=4000:row,OwnerMsk
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total New Income +",*p=4000:row,OwnerMsk:
                              *p=6750:row,*Alignment=*Right,*font=prtpg85,lstcommTot:
                              *p=7250:Row,*Alignment=*Right,SlcCommTot:
                              *p=8125:Row,*Alignment=*Right,grossoutTot:
                              *p=8750:row,*Alignment=*Right,BrkComTot:
                              *p=9250:row,*Alignment=*Right,NINCommTot:
                              *p=9800:row,*Alignment=*Right,NINincTot,*Alignment=*Left
           call               ZeroTotals    
.end patch 2.41
.         print     *n,*02,"Total New Income +",hpt400,ownermsk
.end patch 2.4
         endif
               IF             (HldArFlg = "T")
         add            "225" to row
         PrtPage        Laser;*p=5000:row,*font=prtpg9b,"Income Amount = the sum of Owner(List + Select)."
         add            "225" to row
.begin release 2.41
.         PrtPage        Laser;*p=5000:row,"Billed to Mailer = the sum of Income amount + List costs."
         PrtPage        Laser;*p=5000:row,"Gross Billings = the sum of Income amount + List costs."
.end release 2.41
                    endif
         move      c0 to aptotown
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         compare   c1 to pass
         if        equal 
         move      c2 to pass
         endif
         call      header
         return
...........................................................................
.begin patch 2.41
ZeroTotals     
               move           c0 to LstCommTot
               move           C0 to SLcCommTot
               move           c0 to GrossOutTot
               move           C0 to BrkComTot
               move           C0 to NinCOmmTot
               move           C0 to NinIncTot
               return
.end patch 2.41
...........................................................................
.totold - done with old invoices - done
totold
         compare   c0 to countd                .no opens????
         if        equal
         compare   countc to c0         .need to print adj totals?
         if         less      .yes   
         call      totadj
         else
         compare   countb to c0         .need to print paid totals?
         if         less      .yes   
         move      c4 to pass
         call      totpaid
         endif
.         
         endif
         endif
         move      aptotown to form102
.         mult      ".01" by form102
         cmatch    "T" to newflag
         if        equal
         move       form102 to lobbal           .no prev. bal
         endif
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      c0 to aptotown
         goto      totals
..........................................................................
.totpaid - done with newly paid invoices let's get all adjustments
totpaid
         compare   c0 to countB
         if        equal
         move      c0 to aptotown
         move      c3 to pass
         move      c0 to page
         return
         endif
         move      c0 to form102
         move      aptotown to form102
.         mult      ".01" by form102
         sub       form102 to lobbal
         MOVE      TOTOMSK TO OWNERMSK
         mult      "100" by aptotown
         EDIT      APTOTOWN TO OWNERMSK
         if        (row > 7100)
         call      header
         endif
         add            "500" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total Paid Income -",*p=4000:row,OwnerMsk
         move      c0 to aptotown
       
         compare   c4 to pass
         if        not equal 
         move      c3 to pass
         endif
         call      header
         return
..........................................................................
.totadj   - done with all adjustments lets do the next account
totadj 
         compare   c0 to countC
         if equal
         move      c0 to aptotown
         move      c4 to pass
         move      c0 to page
         return
         endif
         move      c0 to form102
         mult      "100" by aptotown
         move      aptotown to form102
         mult      ".01" by form102
         add       form102 to lobbal
         MOVE      totomsk TO tadjMaSK
         EDIT      APTOTOWN TO tadjMaSK
.         COMPARE   "44" TO LINES
         IF        (row > 7100)
         call      Header
         endif
.         CALL      HEADER IF not less
         add            "500" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Total adjustments to Income +/-",*p=4000:row,tadjmask
.         print     *n,*02,"Total adjustments to Income +/-",hpt400,tadjmask
         move      c0 to aptotown
         move      c4 to pass
         call      header
         return
..................................................................         
totals
.         branch    pass of totbill,totpaid,totadj
.         compare   c3 to pass
.         if        equal
.         call      totadj
.         endif
         move      lobbal to form102
         mult      "100" by form102
         move      form102 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         move      form102 to aplotown
.         div       hund into aplotown
         scan      "-" in ownermsk
         if        equal
         move      c0 to aptotown
         MOVE      TOTOMSK TO OWNERMSK
         EDIT      APTOTOWN TO OWNERMSK
         endif
         MOVE      TOTOMSK TO OWNRMSKx
         mult      "100" by aptotwnx
         EDIT      APTOTWNx TO OWNRMSKx
         MOVE      TOTOMSK TO OWNRMSKR
         mult      "100" by aptotwnr
         EDIT      APTOTWNr TO OWNRMSKr
.        COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
         IF        (row > 7100)
         call      Header

         endif
         compare   c0 to aptotwnx
         if        not equal
.         add            "500" to row
         add            "400" to row
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Ending Balance",*p=4000:row,OwnerMsk:
                              *p=6750:row,*Alignment=*Right,*font=prtpg85,lstcommTot:
                              *p=7250:Row,*Alignment=*Right,SlcCommTot:
                              *p=8125:Row,*Alignment=*Right,grossoutTot:
                              *p=8750:row,*Alignment=*Right,BrkComTot:
                              *p=9250:row,*Alignment=*Right,NINCommTot:
                              *p=9800:row,*Alignment=*Right,NINincTot,*Alignment=*Left
           call               ZeroTotals    
         add            "200" to row
        PrtPage        laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
                       *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
.                       *p=6500:row,ownrmskx
         add            "200" to row
          PrtPage       Laser;*p=5150:row,"Total Rental":
                        *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left

         else
         add            "400" to row
.begin patch 2.41
.        PrtPage        laser;*p=125:row,*font=prtpg9b,"Ending Balance",*p=4000:row,ownermsk
         PrtPage        laser;*p=125:row,*font=prtpg9b,"Ending Balance",*p=4000:row,ownermsk:
                              *p=6750:row,*Alignment=*Right,*font=prtpg85,lstcommTot:
                              *p=7250:Row,*Alignment=*Right,SlcCommTot:
                              *p=8125:Row,*Alignment=*Right,grossoutTot:
                              *p=8750:row,*Alignment=*Right,BrkComTot:
                              *p=9250:row,*Alignment=*Right,NINCommTot:
                              *p=9800:row,*Alignment=*Right,NINincTot,*Alignment=*Left
           call               ZeroTotals    
.end patch 2.41

.         print     *n,*02,"Ending Balance",hpt400,ownermsk
         endif
               IF             (HldArFlg = "T")
         add            "200" to row
         PrtPage        Laser;*p=5000:row,*font=prtpg9b,"Income Amount = the sum of Owner(List + Select)."
         add            "200" to row
.begin release 2.41
.         PrtPage        Laser;*p=5000:row,"Billed to Mailer = the sum of Income amount + List costs."
         PrtPage        Laser;*p=5000:row,"Gross Billings = the sum of Income amount + List costs."
.end release 2.41
                    endif
endingb
         move      c0 to aptotwnx
         move      c0 to aptotwnr
         add       "500" to row
.         add       c2 to lines
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"E",comma,b25:          
                      comma,b6:                         
                      comma,b6:                         
                      comma,b8:                      
                      comma,b9:                    
                      comma,b9:                    
                      comma,b7:                    
                      comma,aplotown:
                      comma,b7,comma,holdlst1
         endif
         call      mtaxprt
         pack      nlobfld from holdown,holdlist,cc,sysyr,sysmo
         move      holdown to loblon
         move      holdlist to loblist
         move      cc to lobcc
         move      sysyr to lobyy
         move      sysmo to lobmm      
         rep       zfill in nlobfld
         rep       zfill in loblon
         rep       zfill in loblist
         cmatch    yes to rflag
         if        not equal
         call      nlobwrt
         endif
         cmatch    "T" to lastrec
         goto      eoj if equal
         move      c0 to lobbal
         move      slsown to holdown
         move      slslist to holdlist
         move      slslst1 to holdlst1
         move      arflag to hldarflg
         move      slslr to ninvfld
         rep       zfill in ninvfld
.add goodies get next beg bal
         pack      nlobfld from slsown,slslist,DATEKEY
         rep       zfill in nlobfld
         move       slsown to nownfld
         move      c0 to aptotown
.         move      c0 to aptotwnx
.         move      c0 to aptotwnr
         move       c0 to lobbal
         call       nlobkey
.                 call      nlobkey
                 if        over
                 move      "T" to newflag
                 else 
                 move      "F" to newflag
                 endif
.         display   *p1:24,*el,*b,"Beg bal",nlobfld,b1,newflag,*b,*w10
         move       c0 to page
         move       c0 to counta
         move       c0 to countb
         move       c0 to countc
         move       c0 to countd
         move       c1 to pass
         PACK      MKEY FROM SLSMLR,Z3
         rep       zfill in mkey
         CALL      NMLRKEY
         call      readtax
toteoj   
         return
..........................................................................
READPAY  MOVE      SLSOWN TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      PAYTN TO str1
         MOVE      PAYTN TO PAYCHK
         PACK      PAYKEY FROM NOWNFLD,str1
         REP       ZFILL IN PAYKEY
         MOVE      PAYKEY TO NPAYFLD
         REP       ZFILL IN NPAYFLD
         CLEAR     PCOMP                   ;PCBUS DOES NOT CLEAR ON OVER.
         CLEAR      PNAME                  ; So be safe and clear them
         CLEAR      PSTREET
         CLEAR      PCITY 
         CLEAR      PSTATE
         CLEAR      PZIP
         CALL      NPAYKEY
         CALL      NOWNKEY
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         CMATCH    YES TO AP2SW
         IF        NOT EQUAL
         MOVE      PCOMP TO ownocpy
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ENDIF
         endif
         endif
         RETURN
...............................................................................
preplob  branch    pass to prepok
.         pack      nlobfld from slsown,slslist,cc,sysyr,sysmo
.         rep       zfill in nlobfld
.         call      nlobtst
.         if        over                   *it really is new
.         goto      prepok
.         else                             *all ready done it
.         move      c0 to hit
.         noreturn
.         branch    pass of error,readpaid,readadj,readsls1
.         endif
.error    display   *p1:24,*el,*b,*b,*red,*blinkon,"ERROR AT PREPLOB",*b,*w10
.         stop
prepok   pack      nlobfld from slsown,slslist,DATEKEY
         rep       zfill in nlobfld
.         display   *p1:24,*el,*b,"at prepok",nlobfld,*b,*w10
         move      slsown to holdown
         move      slslist to holdlist
         move      slsown to nownfld
         move      slslst1 to holdlst1
         move      slslr to ninvfld
         move      arflag to hldarflg
         call      ninvkey
         CALL      READPAY    
         move      c0 to lobbal
                 call      nlobkey
                 if        over
                 move      "T" to newflag
                 else 
                 move      "F" to newflag
                 endif
         call      header
         return
.

......................................................................
readtax
.         MOVE      SLSMLR TO nmtxfld
         MOVE      compnum TO nmtxfld
         CLEAR     TAXPRT
         MOVE      C0 TO MTXC501
         REP       ZFILL IN nmtxfld
         CALL      NMTXKEY
         MOVE      C0 TO TAX501
         MOVE      MTXC501,TAX501
         BRANCH    TAX501 OF C0,C0,C3,C4,C5,C6
C0
         RETURN
C3       MOVE      "501C-3" TO TAXPRT
         RETURN
C4       MOVE      "501C-4" TO TAXPRT
         RETURN
C5       MOVE      "501C-5" TO TAXPRT
         RETURN
C6       MOVE      "501C-6" TO TAXPRT
         RETURN
mtaxprt
.         compare   n4 to pass
.         if        equal
         if        (row > 7800)
         call      header
         endif
.         COMPARE   "44" TO LINES
.         CALL      HEADER IF not less
mtaxprt2
.         print     *l;
.         add       c1 to lines
.         compare   "44" to lines
.         goto      mtaxprt2 if not equal
.         print     *3,"Mailers's tax status is provided as a service though":
.                   " its accuracy cannot be guaranteed."
               PrtPage        Laser;*p=1:7800,"Mailers's tax status is provided as a service though":
                   " its accuracy cannot be guaranteed."               
.print     033,"*p2438.0y0.0X",*3,"Mailers's tax status is provided as a service though":
.                   " its accuracy cannot be guaranteed."               

.         endif
         return
                    
.
EOJ
.patch2.42
        PrtCLose             Laser
        PRTPLAY splname,"PDF995"
.patch2.42
.               splclose
         shutdown
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
.begin patch 2.0
................................................................................................
.prtform
.begin patch 2.4
.prtform
.//print horiz line top of page     .note pattern 2G-horiz line 1G  vert line
.//                         pattern   height in dots (length)   terminate in A for horiz. B for vert
.//                         /           /                  vert width in dots      *cxA would be hor width
.//                        /           /                  /         vert position in dots
.//                       /           /                  /         /
.//                 033,"*c2G",033,"*c5825.2000A",033,"*c3B",033,"*p63.40y2.50X":
.//                 033,"*c0P";                                            \
.                           \                                               Horiz pos in dots
.                            start printing in black
.
.
.         print   033,"&l1E",033,"&a0c0R":
.                   033,"*p1030x75Y":
.                   033,"(8U",033,"(s1p12.00v0s+3b5T","Names":
.                   b2,033,"(8U",033,"(s1p12.00v1s-3b5T","in the News":
.                   033,"*p993x95Y",033,"*c525a02b0p":                         .LINE
.                   033,"*p1030x135Y":
.               033,"(8U",033,"(s1p06.00v0s-2b5T","C  A  L  I  F  O  R  N  I  A        I  N  C .":
.               033,"*p1085.0x189Y":
.               033,"(8U",033,"(s1p06.00v0s-2b5T","1300 Clay St., 11th Floor":
.;              033,"(8U",033,"(s1p06.00v0s-2b5T","One Bush Street, Suite 300":
.               033,"*p1060x226.5Y":
.               033,"(8U",033,"(s1p06.00v0s-2b5T","   Oakland, CA  94612-1429":
.;              033,"(8U",033,"(s1p06.00v0s-2b5T","San Francisco, CA 94104-4447":
.               033,"*p1030x264.0Y":
.               " 415-989-3350 ",bullet," Fax 415-433-7796":
.               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p0.0y1650.0X",033,"*c0P":            .box top
.               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p75.0y1650.0X",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y1650.0X",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y2400.0X",033,"*c0P":            .box line right
.               033,"*p1665x55Y": 
.               033,"(8U",033,"(s1p13.00v1s+3b5T","OWNER ACTIVITY REPORT":
.               033,"*p1x40Y": 
.               033,"(8U",033,"(s1p13.00v0s+3b5T","CONFIDENTIAL":
.               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p375.0y2.5X",033,"*c0P":            .horizontal lines header top
.               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p450.0y2.5X",033,"*c0P":            .horizontal lines header bottm
.               033,"*c2G",033,"*c562.5A",033,"*c3B",033,"*p2400.0y2.5X",033,"*c0P":            .horizontal lines bottom of form col 1
.               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y587.5X",033,"*c0P":            .horizontal lines bottom of form col 2
.               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y765.625X",033,"*c0P":            .horizontal lines bottom of form col 3
.               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y943.75X",033,"*c0P":            .horizontal lines bottom of form col 4
.               033,"*c2G",033,"*c375.0A",033,"*c3B",033,"*p2400.0y1121.875X",033,"*c0P":            .horizontal lines bottom of form col 5
.               033,"*c2G",033,"*c1237.0A",033,"*c3B",033,"*p2400.0y1534.375X",033,"*c0P":            .horizontal lines bottom of form col 6
.               033,"*c2G",033,"*c337.0A",033,"*c3B",033,"*p2400.0y2809.375X",033,"*c0P":            .horizontal lines bottom of form col 7
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2.5X",033,"*c0P":            . 1st vert line left
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y587.5X",033,"*c0P":            . 2nd vert line 
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y765.625X",033,"*c0P":            . 3rd vert line 
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y943.75X",033,"*c0P":            . 4th vert line 
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1121.875X",033,"*c0P":            . 5th vert line 
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1534.375X",033,"*c0P":            . 6th vert line 
.               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2809.375X",033,"*c0P":            . 7th vert line 
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p375.0y3182.5X",033,"*c0P":            . last vert line end header bar
.               033,"*p175x425.0Y":                                                          .position for header lettering
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer":                              . bold & italicised
.               033,"*p624.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","LR##":                              . bold & italicised
.               033,"*p814.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Inv##":                              . bold & italicised
.               033,"*p955.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Mail Date":                              . bold & italicised
.               033,"*p1195.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Income Amount":                              . bold & italicised
.               033,"*p1609.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Order Qty        Billed Qty       Price/m":                              . bold & italicised
.               033,"*p2900.0x425.0Y":
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer Tax":                              . bold & italicised
.              033,"*p0.0x0.0Y"                                .reset position (nec?)
.;end patch 2.4
               return
.begin patch 2.4
.prtform2
PrtForm
.end patch 2.4
.//print horiz line top of page     .note pattern 2G-horiz line 1G  vert line
.//                         pattern   height in dots (length)   terminate in A for horiz. B for vert
.//                         /           /                  vert width in dots      *cxA would be hor width
.//                        /           /                  /         vert position in dots
.//                       /           /                  /         /
.//                 033,"*c2G",033,"*c5825.2000A",033,"*c3B",033,"*p63.40y2.50X":
.//                 033,"*c0P";                                            \
.                           \                                               Horiz pos in dots
.                            start printing in black
.
.
.START PATCH 2.46 REPLACED LOGIC
.       PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
.                   *MarginL=0,*MarginT=0:           
.                   *p=3433:250,*font=prtpg12B,"Names  ":
.                   *font=prtpg12I,"  in the News":
.                   *PENSIZE=10,*p=3305:425,*Line=4940:425:
.                   *p=3413:450,*font=prtpg9,"C  a  l  i  f  o  r  n  i  a    I  n  c .":
.                   *p=3616:630,*font=prtpg6,"1300 Clay Street, 11th Floor":
.                   *p=3586:755, "Oakland, CA 94612-1429":
.                   *p=3516:880, " 415-989-3350 ","·"," Fax 415-433-7796":
.                   *RECT=1:250:5500:7250:
.                   *p=5550:1,*font=prtpg12B,"Owner Activity Report":
.                   *p=1:1,"Confidential":
.                   *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
.                   *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
.                   *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
.                   *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
.                   *PENSIZE=10,*p=2575:1250,*Line=2575:8000:             ;3rd
.                   *PENSIZE=10,*p=3150:1250,*Line=3150:8000:             ;4th
.                   *PENSIZE=10,*p3725:1250,*Line=3725:8000:              ;5th
.                   *PENSIZE=10,*p4975:1250,*Line=4975:8000:              ;6th
.                   *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;7th
.                   *PENSIZE=10,*p=1:7990,*Line=1875:7990:                    ;bottom lines   1
.                   *PENSIZE=10,*p=2000:7990,*Line=2500:7990:                    ;bottom lines 2
.                   *PENSIZE=10,*p=2575:7990,*Line=3025:7990:                    ;bottom lines   3
.                   *PENSIZE=10,*p=3150:7990,*Line=3650:7990:                    ;bottom lines   4
.                   *PENSIZE=10,*p=3725:7990,*Line=4850:7990:                    ;bottom lines   5
.                   *PENSIZE=10,*p=4975:7990,*Line=9800:7990:                    ;bottom lines   6
.                   *PENSIZE=10,*p=9875:7990,*Line=10400:7990:                    ;bottom lines   7
.                   *p=583:1300,*font=prtpg9bi,"Mailer":
.                   *p=2125:1300,*font=prtpg9bi,"LR##":
.                   *p=2700:1300,*font=prtpg9bi,"Inv##":
.                   *p=3200:1300,*font=prtpg9bi,"Mail Date":
.                   *p=3850:1300,*font=prtpg9bi,"Income Amount":
.                   *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
          PRTPAGE   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                    *MarginL=0,*MarginT=0:
                    *Alignment=*Left:
                    *Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
                    *RECT=1:250:5500:7250:
                    *p=5550:1,*font=prtpg12B,"Owner Activity Report":
                    *p=1:1,"Confidential":
                    *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
                    *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
                    *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
                    *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
                    *PENSIZE=10,*p=2575:1250,*Line=2575:8000:             ;3rd
                    *PENSIZE=10,*p=3150:1250,*Line=3150:8000:             ;4th
                    *PENSIZE=10,*p3725:1250,*Line=3725:8000:              ;5th
                    *PENSIZE=10,*p4975:1250,*Line=4975:8000:              ;6th
                    *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;7th
                    *PENSIZE=10,*p=1:7990,*Line=1875:7990:                    ;bottom lines   1
                    *PENSIZE=10,*p=2000:7990,*Line=2500:7990:                    ;bottom lines 2
                    *PENSIZE=10,*p=2575:7990,*Line=3025:7990:                    ;bottom lines   3
                    *PENSIZE=10,*p=3150:7990,*Line=3650:7990:                    ;bottom lines   4
                    *PENSIZE=10,*p=3725:7990,*Line=4850:7990:                    ;bottom lines   5
                    *PENSIZE=10,*p=4975:7990,*Line=9800:7990:                    ;bottom lines   6
                    *PENSIZE=10,*p=9875:7990,*Line=10400:7990:                    ;bottom lines   7
                    *p=583:1300,*font=prtpg9bi,"Mailer":
                    *p=2125:1300,*font=prtpg9bi,"LR##":
                    *p=2700:1300,*font=prtpg9bi,"Inv##":
                    *p=3200:1300,*font=prtpg9bi,"Mail Date":
                    *p=3850:1300,*font=prtpg9bi,"Income Amount":
                    *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
.END PATCH 2.46 REPLACED LOGIC
         IF        (HLDARFLG = "T")
.print detail goodies
               PRTPAGE     Laser;*p=5100:1235,*font=prtpg9bi,"Order":
                           *p=5100:1365," Qty":
                           *p=5600:1235,*font=prtpg9bi,"Billed":
                           *p=5600:1365," Qty":
                           *p=6350:1365,"  List/Select":
                           *p=6350:1235,"Owner Income"
          else
.print regular goodies
               PRTPAGE     Laser;*p=5100:11300,*font=prtpg9bi,"Order Qty"
.               033,"(8U",033,"(s1p07.00v1s+2b5T","Order Qty        Billed Qty       Price/m":                              . bold & italicised
         endif


         return
..THIS LOGIC IS NEVER ACCESSED DUE TO RETURN ABOVE!!!!  9/24/2004  LOGO DOES *NOT* NEED CONVERSION!!!!
         print   033,"&l1E",033,"&a0c0R":
                   033,"*p1030x75Y":
                   033,"(8U",033,"(s1p12.00v0s+3b5T","Names":
                   b2,033,"(8U",033,"(s1p12.00v1s-3b5T","in the News":
                   033,"*p993x95Y",033,"*c525a02b0p":                         .LINE
                   033,"*p1030x135Y":
               033,"(8U",033,"(s1p06.00v0s-2b5T","C  A  L  I  F  O  R  N  I  A        I  N  C .":
               033,"*p1085.0x189Y":
               033,"(8U",033,"(s1p06.00v0s-2b5T","1300 Clay St., 11th Floor":
.              033,"(8U",033,"(s1p06.00v0s-2b5T","One Bush Street, Suite 300":
               033,"*p1060x226.5Y":
               033,"(8U",033,"(s1p06.00v0s-2b5T","   Oakland, CA  94612-1429":
.              033,"(8U",033,"(s1p06.00v0s-2b5T","San Francisco, CA 94104-4447":
               033,"*p1030x264.0Y":
               " 415-989-3350 ",bullet," Fax 415-433-7796":
               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p0.0y1650.0X",033,"*c0P":            .box top
               033,"*c2G",033,"*c750.0A",033,"*c3B",033,"*p75.0y1650.0X",033,"*c0P":            .box bottom
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y1650.0X",033,"*c0P":            .box line left
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p0.0y2400.0X",033,"*c0P":            .box line right
               033,"*p1665x55Y": 
               033,"(8U",033,"(s1p13.00v1s+3b5T","OWNER ACTIVITY REPORT":
               033,"*p1x40Y": 
               033,"(8U",033,"(s1p13.00v0s+3b5T","CONFIDENTIAL":
               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p375.0y2.5X",033,"*c0P":            .horizontal lines header top
               033,"*c2G",033,"*c3180.0A",033,"*c3B",033,"*p450.0y2.5X",033,"*c0P":            .horizontal lines header bottm
               033,"*c2G",033,"*c562.5A",033,"*c3B",033,"*p2400.0y2.5X",033,"*c0P":            .horizontal lines bottom of form col 1
               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y587.5X",033,"*c0P":            .horizontal lines bottom of form col 2
               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y765.625X",033,"*c0P":            .horizontal lines bottom of form col 3
               033,"*c2G",033,"*c150.0A",033,"*c3B",033,"*p2400.0y943.75X",033,"*c0P":            .horizontal lines bottom of form col 4
               033,"*c2G",033,"*c375.0A",033,"*c3B",033,"*p2400.0y1121.875X",033,"*c0P":            .horizontal lines bottom of form col 5
               033,"*c2G",033,"*c1237.0A",033,"*c3B",033,"*p2400.0y1534.375X",033,"*c0P":            .horizontal lines bottom of form col 6
               033,"*c2G",033,"*c337.0A",033,"*c3B",033,"*p2400.0y2809.375X",033,"*c0P":            .horizontal lines bottom of form col 7
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2.5X",033,"*c0P":            . 1st vert line left
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y587.5X",033,"*c0P":            . 2nd vert line 
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y765.625X",033,"*c0P":            . 3rd vert line 
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y943.75X",033,"*c0P":            . 4th vert line 
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1121.875X",033,"*c0P":            . 5th vert line 
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y1534.375X",033,"*c0P":            . 6th vert line 
               033,"*c1G",033,"*c2025.00B",033,"*c3A",033,"*p375.0y2809.375X",033,"*c0P":            . 7th vert line 
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p375.0y3182.5X",033,"*c0P":            . last vert line end header bar
               033,"*p175x425.0Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer":                              . bold & italicised
               033,"*p624.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","LR##":                              . bold & italicised
               033,"*p814.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","Inv##":                              . bold & italicised
               033,"*p955.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","Mail Date":                              . bold & italicised
               033,"*p1195.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","Income Amount":                              . bold & italicised
               033,"*p1580.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","Order Qty    Billed Qty      List/Select Income":                              . bold & italicised
               033,"*p2900.0x425.0Y":
               033,"(8U",033,"(s1p07.00v1s+2b5T","Mailer Tax":                              . bold & italicised
              033,"*p0.0x0.0Y"                                .reset position (nec?)
               return
.begin patch 2.4
PrtOWner        call          trim using Ownlocty
                                                         prtpage       Laser;*p=500:375,Ownlon,*alignment=*right,*p10000:500,Page:
                              *p=10000:375,Today,*alignment=*left:
                              *p=500:575,OwnLonm:
                              *p=500:725,Ownocpy,*p=7000:725,Holdlst1:
                              *p=500:875,Ownlosa,*p=7000:875,Holdlist:
                              *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc
               return
 ;end patch 2.4
exprint
.               call           debug
               PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                   *p=2125:row,*Alignment=*left,*font=prtpg85,slslr:
                   *p=2690:row,Invnum:
                   *p=3195:row,DatePrt1:
                   *p=5500:row,*Alignment=*right,M$Oqty:
                   *p=6025:row,M$qty,*Alignment=*left:
                   *p=8500:row,"Exchange List Recovery"
         MATCH     YES TO AP2SW
         IF        EQUAL
         move      slsap2 to aplotus
         ADD     SLSAP2 TO APTOTOWN
         ADD     SLSAP2 TO APTOTWNX
         ELSE
         COMPARE   C0 TO PAYCHK
                        IF        EQUAL
                        PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out:
                                             *p=8125:Row,*Alignment=*Right,Ap1Out:
                                             *p=7250:Row,*Alignment=*Right,Ap1Out:
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                        move      slsap1 to aplotus
                        ADD       SLSAP1 TO APTOTOWN
                        ADD       SLSAP1 TO APTOTWNX
                        else
                        PrtPage               Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left:
                                             *p=8125:Row,*Alignment=*Right,Ap2Out:
                                             *p=7250:Row,*Alignment=*Right,Ap2Out:
                                             *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                        move      slsap2 to aplotus
                        ADD       SLSAP2 TO APTOTOWN
                        ADD     SLSAP2 TO APTOTWNX
                        ENDIF
         ENDIF
.
         cmatch    yes to flatflg
         if        equal
         WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
                      comma,slslr:
                      comma,invnum:                         
                      comma,dateprt1:                      
                      comma,oqty:                    
                      comma,qtybild:
                      comma,m$ppm:                    
                      comma,aplotus:
                      comma,taxprt:
                      comma,holdlst1
         endif

.         PrtPage            Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left

         return
................................................................................................
.end patch 2.0
         STOP
.patch2.45
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.45
         INCLUDE   NORDIO.INC
 ;begin patch 2.5
.        INCLUDE   NINVIO.INC
          INCLUDE             ninvio.inc
          INCLUDE             NInvAcdIO.INC
 ;end patch 2.5
        include   nownio.inc
         INCLUDE   NmtxIO.INC
         INCLUDE   NDAT3IO.INC
         INCLUDE   GNXTIO.INC
         include   comlogic.inc
         include   nmoaio.inc
         include   nmrgio.inc
         INCLUDE   NLOBIO.INC
         include   nslsio.inc
.begin patch 1.9
         INCLUDE   NJSTIO.inc
.end patch 1.9
.begin patch 1.8
.begin patch 2.5
.         include   compute.inc
          include             compute.inc
.end patch 2.5
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
.end patch 1.8
.begin patch 2.4
.end patch 2.4
               include   npayio.inc


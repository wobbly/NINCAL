pc       equ       0
         include   common.inc
         include   cons.inc                   
         INCLUDE   NBILDD.inc
         include   consacct.inc
         include   cvtdd.inc
         include   hp.inc
         include   norddd.inc
.START PATCH 1.5 REPLACED LOGIC
.         include   nmlrdd.inc
.         include   nbrkdd.inc
         include   compdd.inc
         include   cntdd.inc
.END PATCH 1.5 REPLACED LOGIC
         include   nmrgdd.inc
         include   nowndd.inc
         include   nshpdd.inc
         include   nacddd.inc
         include   ndatdd.inc
;begin patch 1.6
	Include	ninvdd.inc
	INclude	NINvAcddd.inc
;         include   ninvdd.inc
;end patch 1.6
         include   ndat3dd.inc
         include   nxcgdd.inc
         include   tinvdd.inc

RELEASE  	INIT      	"1.6"              DLH 10March2005 Invoice CONVERSION
;RELEASE  INIT      "1.5"              ASH 26MAY2004 MAILER CONVERSION
.RELEASE  INIT      "1.4"              ASH 04OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.3"              ASH 17JUN99 DUPLICATE VARS NOW FOUND IN CONS.INC
.RELEASE  INIT      "1.2"              ASH 26JAN99  CONSACCT.INC VAR EXPANSION
.release  init      "1.10"             ASH 30Oct98  TDMCINV Y2K, File expansion
.release  init      "1.00"             DLH 17June98 
output   file     
lines    form      2
page     form      2
eop      form      "58"
skipcnt  form      5
Count    FORM        5
selectd  form       5
Countin  FORM        5
TOTARp   FORM      9.2       *prepaid
TOTpMOA  FORM      9.2       *MOA Applied to prepaid
TOTAR    FORM      9.2
TOTAP1   FORM      9.2
TOTAP2   FORM      9.2
TOTAP    FORM      9.2
TOTNIN   FORM      9.2
TOTLR    FORM      9.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
LManage  init      "018710"    List management exchange fees only
TDMCAMT  FORM       8.2
tottdmc  form       9.2
MT$tdmc DIM       15
TDMChrg  dim        1           (Y) = tdmc related charges
runchrg  form       9.2          =Running charges
grunrlr  form       9.2          =Total NINCA income on triplex billing
grunrar  form       9.2          =Total NINCA adc billing (TDMC)
grunrflat  form       9.2        =total ninca rental flat billing
grunrpass  form       9.2        =total ninca rental pass through billing  RC, Selects
TDMCFLAT   form       9.2        =estimated triplex flat charges shipping, Mag tape....
IncRFlat   form       9.2        =Calced NINCA income from flat charges.
PRCflat    form       3          =percent cost associated to flat charges
PRCPASS    form       3          =percent cost associated to /m charges
PRCLRflat  form       3          =percent income associated to flat charges
prcLRPASS  form       3          =percent income associated to /m charges
.
.END TDMC.
.
LRMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      9.2      UNKNOWN LR INCOME.
LREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
LRBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
.
ARMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
AREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM      FORM      9.2      TOTAL MANAGEMENT A/R
ARBR     FORM      9.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN   FORM      9.2      UNKNOWN  A/R.
ARBBE      FORM        9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR      FORM        9.2      TOTAL BATCH BILL A/R RENT PORTION
.
APMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
APM      FORM      9.2      TOTAL MANAGEMENT A/P
APBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN   FORM      9.2      UNKNOWN A/P.
APBBE      FORM        9.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR      FORM        9.2      TOTAL BATCH BILL A/P RENT PORTION
.
ppflag   dim       1            'P' if equal else blank
PMASK    DIM       1
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
FORM11   FORM      11
form122  form      12.2
innets   dim       1
exfeflag dim       1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
mrgsw    dim       1
shipsw   dim       1 
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
.START PATCH 1.3 - DUPLICATE VARS
.MASK32   INIT      "ZZZ.ZZ-"
.END PATCH 1.3 - DUPLICATE VARS
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.START PATCH 1.3 - DUPLICATE VARS
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.END PATCH 1.3 - DUPLICATE VARS
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
.START PATCH #1.2 - INCREASED VAR
.M$AR     DIM       13
M$AR     DIM       15
.END PATCH #1.2 - INCREASED VAR
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
.START PATCH #1.2 - INCREASED VAR
.M$AP1    DIM       13
.M$AP2    DIM       13
.M$STAX   DIM       8
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
.END PATCH #1.2 - INCREASED VAR
M$CTAX   DIM       8
M$POST   DIM       6
.START PATCH #1.2 - INCREASED VAR
.M$LRINC  DIM       13
.M$NINC   DIM       13
.M$GROSS  DIM       13
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.END PATCH #1.2 - INCREASED VAR
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
DATEMASK DIM       8
.
............................................................................
         move      "TDMC Billing Analysis" to stitle
         move      "TINV0004" to program 
         move      "Names in the News Ca" to compnme
         move       c1 to nordpath
         move       c1 to ninvpath
         
         clock      date to today
         call       paint
.START PATCH 1.4 REPLACED LOGIC
.         splopen   "g:\data\tdmcreg.lst"
         PACK      STR35,NTWKPATH1,"tdmcreg.lst"
         splopen   str35
.END PATCH 1.4 REPLACED LOGIC
         trap       oops if f5
         move       "Abort" to pf5
         call       funcdisp
.START PATCH 1.4 REPLACED LOGIC
.         prepare    output,"g:\data\TDMCINV.out"
         PACK       STR35,NTWKPATH1,"TDMCINV.OUT"
         prepare    output,STR35
.end PATCH 1.4 REPLACED LOGIC
         call       header
.         goto       pass3


loop     call       tinvks
         goto       pass2 if over
         add        c1 to count
         display    *p12:10,"records Read, In :",count,b1,countin
         move       tinvlr to ninvfld
.Start patch #1.10 - remmed and replaced lines
.         unpack      tinvdate into mm,dd,yy
         unpack      tinvdate into str2,yy,mm,dd
.End patch #1.10 - remmed and replaced lines
         match      "03" to yy
         goto       loop if not equal
;         if         (mm = "01")
;         goto       loop IF NOT EQUAL
;         endif
.         match      "01" to mm
.         goto       loop if not equal
         add        c1 to countin
         move       tinvlr to nordfld
         rep        zfill in nordfld
         call       nordkey
         write      output,seq;tinvvars,omlrnum,olon,olnum
         goto       loop
pass2
         weof       output,seq
.Start Patch #1.10 - remmed and replaced line
.         execute    "f:\netutils\sort g:\data\tdmcinv.out g:\data\tdmcinv.srt /s(70,4,N,a,74,6,N,a,1,6,n,a) w(c:) verbose"
.START PATCH 1.4 REPLACED LOGIC
.         execute    "f:\netutils\sort g:\data\tdmcinv.out g:\data\tdmcinv.srt /s(72,4,N,a,76,6,N,a,1,6,n,a) w(c:) verbose"
         PACK       TASKNAME,NTWKPATH2,"sort ",NTWKPATH1,"tdmcinv.out ",NTWKPATH1,"tdmcinv.srt /s(72,4,N,a,76,6,N,a,1,6,n,a) w(c:) verbose"
         execute    TASKNAME
.END PATCH 1.4 REPLACED LOGIC
.End Patch #1.10 - remmed and replaced line
.START PATCH 1.4 REPLACED LOGIC
.pass3    open       output,"g:\data\tdmcinv.srt"
pass3    PACK       STR35,NTWKPATH1,"TDMCINV.SRT"
         open       output,STR35
.END PATCH 1.4 REPLACED LOGIC

loop3    read       output,seq;tinvvars,omlrnum,olon,olnum
         goto       eoj if over
         move       tinvlr to ninvfld        
         move       tinvlr to nordfld
         move       tinvlr to nmrgfld
         move       tinvlr to nshpfld         
         move       olon to nownfld
         call       ninvkey
         call       nordkey
        
         MOVE      C0 TO FORM7
         MOVE      qtybild TO FORM7
.
.START PATCH #1.2 - INCREASED VAR
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
         MOVE      PPM TO CMPT92
         DIVIDE    HUND INTO CMPT92
         MOVE      CMPT92 TO FORM32
.END PATCH #1.2 - INCREASED VAR
.
.
         CALL      READBILL
.
.
         move      c1 to nownpath
         move      olon to nownfld
         call      nownkey
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      olrn to nshpfld
         move      no to shipsw
         CALL      NshpKEY                    ;on line ship data
         if        not over
         move      yes to shipsw
         endif
         move      no to mrgsw
         CALL      NMRGKEY                    ;on line merge data
         if        not over
         move      yes to mrgsw
         endif
         call       nownkey
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL IN MKEY
         CALL      NMLRKEY
         move      c2 to tdmcflag     .force compute to calc tdmc goodies
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
MASKIT   
.START PATCH #1.2 - INCREASED VAR
.         MOVE      MASK72 TO M$GROSS
.         EDIT      GROSS TO M$GROSS
..
.         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
..
.         MOVE      MASK72 TO M$AR
.         EDIT      FORMAR TO M$AR
.         ADD       FORMAR TO TOTAR
..
.         MOVE      MASK72 TO M$AP1
.         EDIT      AP TO M$AP1
.         ADD       AP TO TOTAP1
..
.         MOVE      MASK72 TO M$AP2
.         EDIT      FORMAP2 TO M$AP2
.         ADD       FORMAP2 TO TOTAP2
.         COMPARE   C0 TO FORMAP2
.         IF        EQUAL
.         CLEAR     M$AP2
.         endif
..
.         MOVE      MASK72 TO M$LRINC
.         EDIT      LRINC TO M$LRINC
.         ADD       LRINC TO TOTLR
..
.         MOVE      MASK72 TO M$NINC
.         EDIT      NININC TO M$NINC
.         ADD       NININC TO TOTNIN
..
.         MOVE      MASK42 TO M$STAX
.         EDIT      TAXES TO M$STAX
.         ADD       TAXES TO TOTSTAX
....
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
.
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
         ADD       FORMAR TO TOTAR
.
         MOVE      MASK92 TO M$AP1
         EDIT      AP TO M$AP1
         ADD       AP TO TOTAP1
.
         MOVE      MASK92 TO M$AP2
         EDIT      FORMAP2 TO M$AP2
         ADD       FORMAP2 TO TOTAP2
         COMPARE   C0 TO FORMAP2
         IF        EQUAL
         CLEAR     M$AP2
         endif
.
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
         ADD       LRINC TO TOTLR
.
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
         ADD       NININC TO TOTNIN
.
         MOVE      MASK52 TO M$STAX
         EDIT      TAXES TO M$STAX
         ADD       TAXES TO TOTSTAX
.END PATCH #1.2 - INCREASED VAR
.
         MOVE      C0 TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
.
*****************************************************************************
         MOVE      C0 TO RENTSW
         MOVE      OELCODE TO RENTSW
............................................................................
.  BATCH BILLING SECTION. 25MAR93
           PACK      MKEY FROM omlrnum,OCOBN
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
.          DISPLAY   *P10:16,*EL,*P10:18,*EL 
           CALL      NMLRKEY
.           CMATCH    "B" TO MCODE          .BATCH BILL ?
.         goto      bdate if equal
.           CMATCH    "A" TO MCODE          .BATCH BILL ?
.         goto      bdate if equal
         goto      slstype 
..............................................................................
slstype   MATCH     OLNUM TO Lmanage
          IF        EQUAL
          MOVE      YES TO EXFEFLAG      
          goto      ManEXch               .Management exchange fee
          ENDIF
          MOVE      NO TO EXFEFLAG
          BRANCH    SALESBR OF BROKER,BROKER,MANAGE         
          goto      broker
.
.MANEXCH - LIST MANAGEMENT EXCHANGE FEE.

MANEXCH
         ADD       LRINC TO LREXFEE
         ADD       FORMAR TO AREXFEE
         ADD       AP TO APEXFEE
         ADD       FORMAP2 TO APEXFEE
         GOTO      PRINT

MANAGE
         BRANCH   RENTSW OF MRENT,MEXCH,MEXCH
MRENT
         ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
         ADD       FORMAR TO ARMR
         ADD       AP TO APMR
         ADD       FORMAP2 TO APMR
         GOTO      PRINT
MEXCH    
         ADD       LRINC TO LRMEINC        *EXCH/MANAGEMENT INCOME
         ADD       FORMAR TO ARME
         ADD       AP TO APME
         ADD       FORMAP2 TO APME
         GOTO      PRINT
.
BROKER   BRANCH    BATCHBR OF BROKER2
.
BROKER1  BRANCH    RENTSW OF BRENT,BEXCH,BEXCH
           GOTO      BRENT
BROKER2  BRANCH    RENTSW OF BRENTB,BEXCHB,BEXCHB
           GOTO      BRENTB
BRENT
         ADD       LRINC TO LRBRINC        *RENTAL/BROKERAGE INCOME
         ADD       FORMAR TO ARBR
         ADD       AP TO APBR
         ADD       FORMAP2 TO APBR
         GOTO      PRINT
BEXCH
         ADD       LRINC TO LRBEINC        *EXCH/BROKERAGE INCOME
         ADD       FORMAR TO ARBE
         ADD       AP TO APBE
         ADD       FORMAP2 TO APBE
         GOTO      PRINT
BRENTB
         ADD       LRINC TO LRBBR        *RENTAL/BROKERAGE INCOME
         ADD       FORMAR TO ARBBR
         ADD       AP TO APBBR
         ADD       FORMAP2 TO APBBR
         GOTO      PRINT
BEXCHB
         ADD       LRINC TO LRBBE        *EXCH/BROKERAGE INCOME
         ADD       FORMAR TO ARBBE
         ADD       AP TO APBBE
         ADD       FORMAP2 TO APBBE
*****************************************************************************************
DET
Print
         ADD         C1 TO selectd
         move       tinvdolr to form122
         mult       ".01" by form122
         move        c0 to TDMCAMT
         add         form122 to TDMCAMT
         move       c0 to runchrg

         move      tinvlr to nxcgfld
         rep       zfill in nxcgfld
         call      nxcgkey
         goto      runexit if over
         move       yes to tdmchrg
         add        nxcgar to runchrg
runloop  call       nxcgks
         goto       runexit if over       
         match      nxcglr to nxcgfld
         goto       runexit if not equal
         add        nxcgar to runchrg
         goto       runloop
runexit         

.det
.PRINT
         COMPARE   "56" TO LINES
         CALL      HEADER IF not less
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        equal
         move      b1 to innets
         else
         move      "*" to innets
         endif
.
         if        (runchrg > 0)
         add       runchrg to runrar             .add exchange charges.
         endif
.                                                .prog 19 charges
         move      runrar to runrlr
         sub       tdmcamt from runrlr
..................
.         
         move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         sub       runrpass,tdmcflat           .what part of tdmc charge is flat charges
         if        (tdmcflat < 0 )   
         move      c0 to tdmcflat
         endif
         
.         
         calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         calc      prcpass=runrpass/(runrpass+tdmcflat)*100
         calc      prcLRPASS=(RUNRLR/runrar)*100
         
         if        (runrlr < 0 | incRflat < 0)
.LETS ONLY PRINT REJECTS
         COMPARE   C1 TO CMREFLAG
         IF        EQUAL
         MATCH     YES TO EXFEFLAG
         IF        EQUAL
         PRINT     HPBON,*2,HPUNON,OMLRNUM,HPUNOFF,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPBOFF,HPUNOFF
         ELSE
         PRINT     HPBON,*2,OMLRNUM,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPBOFF
         ENDIF
         ELSE
         MATCH     YES TO EXFEFLAG
         IF        EQUAL
         PRINT     *2,HPuNoN,OMLRNUM,HPUNOFF,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPUNOFF
         ELSE
         PRINT     *2,OMLRNUM,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMP,*64,M$AP2," ",PMASK
         ENDIF
         ENDIF
         ADD       c2 TO LINES
         ENDIF
.02Jun98 DLH begin

         MOVE      MASK92 TO Mt$AP1
         MOVE      MASK92 TO Mt$AP2
         MOVE      MASK92 TO Mt$AR
         MOVE      MASK92 TO Mt$lrinc
         EDIT      RUNRAR TO Mt$AR
         EDIT      RUNRPASS TO Mt$AP1
         EDIT      RUNRflat TO Mt$AP2
         EDIT      RUNRlr TO Mt$lrinc
.tdmcamt = $ from Triplex invoice file
.Runrar = total receivable $ from triplex related additional charges
.runrpass = receivables from Run Charge, commisionable selects
.runrflat = receivables from Flat charges ie Mag tape, Shipping, etc.
.IncRFLat   = Profit from flat charges (tdmcamt - runrpass = tdmc flat charges 'TDMCFLAT'; 
.runrflat - tdmcflat = IncRflat)         
         if        (runrlr < 0 | incRflat < 0)        
         print     hpunon,hpbon,*2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                   prcpass,"%":
                   *89,mt$lrinc,B1,PRCLRPASS,"%":
                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                   *91,INCrFLAT,B2,PRCLRFLAT,"%",hpboff,hpunoff
         ADD       C2 TO LINES
         else          
..LETS ONLY PRINT REJECTS
.;         print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
.;                   prcpass,"%":
.;                   *89,mt$lrinc,B1,PRCLRPASS,"%":
.;                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
.;                   *91,INCrFLAT,B2,PRCLRFLAT,"%"
.;         ADD       C1 TO LINES
         endif
         clear     tdmchrg      
         add        runrar to grunrar
         add        runrpass to grunrpass
         add        runrflat to grunrflat    
         add        runrlr to grunrlr
         move       c0 to runrar
         move       c0 to runrlr
         move       c0 to runrpass
         move       c0 to runrflat

           DISPLAY   *ef,*P10:14,"RECORDS SKIPPED : ",skipcnt
           DISPLAY   *ef,*P10:12,"RECORDS selected: ",selectd
           goto      loop3
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch,hpdupl,*f          .compressed, duplex
.         print      hp17ptch,hpdupl,*f,*f          .compressed, duplex
         endif
         PRINT     *f,*29,"***  N I N   T R I P L E X   ":
                   *60,"B I L L I N G   A N A L Y S I S  ***":
                   *116,"DATE ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"MAILER",*11,"LR":
                   *17,"INVOICE":
                   *26,"MAILER BILL-TO":
                   *52,"--------ACCOUNTS--------":
                   *79,"-------COMMISSIONS------":
                   *108,"------TAXES----",*128,"OUR":
                   *L,*1,"NUMBER",*9,"NUMBER":
                   *18,"NUMBER",*26,"NAME AND THRU":
                   *53,"RECEIVABLE":
                   *69,"PAYABLE",*81,"NIN INCOME":
                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
                   *126,"POSTAGE",*L
         MOVE      c6 TO LINES
         RETURN
eoj
*......................................................................
.
TOTAL    
.         COMPARE   "59" TO LINES
         COMPARE   "40" TO LINES
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$AR
         EDIT      TOTAR TO MT$AR
         MOVE      MASK92 TO MT$ARp
         mult      seq by totarp
         EDIT      TOTARp TO MT$ARp
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP1 TO MT$AP1
         MOVE      MASK92 TO MT$AP2
         EDIT      TOTAP2 TO MT$AP2
         MOVE      MASK92 TO MT$NINC
         EDIT      TOTNIN TO MT$NINC
         MOVE      MASK92 TO MT$LRINC
         EDIT      TOTLR TO MT$LRINC
         MOVE      MASK92 TO MT$STAX
         EDIT      TOTSTAX TO MT$STAX
         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTCTAX TO MT$CTAX
         MOVE      MASK52 TO MT$POST
         EDIT      TOTPOST TO MT$POST
         PRINT     *1,"## INVOICES ",COUNT:
                   *26,"*** TOTALS",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC:
                   *115,MT$CTAX:
                   *L,*62,MT$AP2,*77,MT$NINC,*103,MT$STAX,*124,MT$POST:
                   *L,*26,"Code 96 ",*48,MT$ARP
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
         print     *26,"total Moa debit",*48,MT$pmoa
         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP TO MT$AP1
         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,MT$AP1
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBRINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBR TO MT$AR
         EDIT      APBR TO MT$AP1
         PRINT     *L,*L,*5,"TOTAL BROKERAGE/RENTAL     ",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBEINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBE TO MT$AR
         EDIT      APBE TO MT$AP1
         PRINT     *5,"TOTAL BROKERAGE/EXCHANGE   ",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LRMRINC TO LRMINC
         ADD       LRMEINC TO LRMINC
         EDIT      LRMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       ARMR TO ARM
         ADD       ARME TO ARM
         ADD       APMR TO APM
         ADD       APME TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
         PRINT     *5,"TOTAL MANAGEMENT    ",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
.
         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRMINC
         ADD       LREXFEE TO LRMINC
         EDIT      LRMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       AREXFEE TO ARM
         ADD       APEXFEE TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
         PRINT     *5,"TOTAL MANAGEMENT EXCH FEE   ",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBR TO MT$AR
         EDIT      APBBR TO MT$AP1
         PRINT     *L,*L,*5,"TOTAL BROKERAGE/RENT BATCH Prev. Month",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBE TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBE TO MT$AR
         EDIT      APBBE TO MT$AP1
         PRINT     *5,"TOTAL BROKERAGE/EXCH BATCH Prev. Month",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
..............................................................................
.TRIPLEX  EXCHANGES
         MOVE      MASK92 TO MT$LRINC
         EDIT      RUNLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      RUNAR TO MT$AR
         EDIT      RUNPASS TO MT$AP1
         EDIT      RUNFLAT TO MT$AP2
         PRINT     *5,"TRIPLEX BREAKOUT IS FOR TEST PURPOSES ONLY!!!!!!!!":
                   *L,*5,"--------------------------------------------------":
                   *L
         PRINT     *5,"Running charge Ivoices","##",RUNCOUNT,*48,MT$AR:
                   *62,MT$AP1:
                   *89,MT$LRINC:
                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2
.
.TRIPLEX  prog 19 entries for month
         MOVE      MASK92 TO MT$LRINC
         EDIT      GRUNRLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      GRUNRAR TO MT$AR
         EDIT      GRUNRPASS TO MT$AP1
         EDIT      GRUNRFLAT TO MT$AP2
         PRINT     *5,"Rental TDMC & Prog 19","##",RUNRCNT,*48,MT$AR:
                   *62,MT$AP1:
                   *89,MT$LRINC:
                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2
.
         MOVE      MASK92 TO MT$TDMC
         EDIT      TOTTDMC TO MT$TDMC
         PRINT     *l,*5,hpbon,"TRIPLEX Billed us  Charges","##",*48,MT$TDMC,hpboff

.         MOVE      MASK92 TO MT$LRINC
.         EDIT      CMREDOLR TO MT$LRINC
.         PRINT     *L,*L,HPBON:
.                    *5,"10 %COMMISION RECAPTURE    ":
.                   *89,MT$LRINC,HPBOFF
.
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      LRMEINC TO MT$LRINC
.         MOVE      MASK92 TO MT$AP1
.         MOVE      MASK92 TO MT$AR
.         EDIT      ARME TO MT$AR
.         EDIT      APME TO MT$AP1
.         PRINT     *5,"TOTAL MANAGEMENT/EXCHANGE  ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.
         PRINT     *FLUSH
.         COMPARE   C0 TO LRUNKN
.         GOTO      EOJ IF EQUAL
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      LRUNKN TO MT$LRINC
.         MOVE      MASK92 TO MT$AP1
.         MOVE      MASK92 TO MT$AR
.         EDIT      ARUNKN TO MT$AR
.         EDIT      APUNKN TO MT$AP1
.         PRINT     *5,"TOTAL UNKOWN   ",*48,MT$AR,*62,MT$AP1:
.                   *89,MT$LRINC
.         PRINT     *FLUSH
.         .execute "F:\PUBLIC\NPRINT g:\DATA\tdmcbill.LST Q=LASER2 NT NA=GS_JL f=0 S=NTS0_fpnw C=2"
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop


READBILL
.
.         PACK      NBILFLD FROM MLRN,COBN,BILLTN
.         CALL      NBILKEY
.         CALL      READMLR IF OVER
.         RETURN
.READMLR 
READMLR  PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
         clear     bilname
.         MOVE      MCCTO TO BILNAME
           CLEAR     BRCOMP
           CLEAR     BRaddr
           CLEAR     BRcity
           CLEAR     BRstate
           CLEAR     BRzip
           CLEAR     NBRKFLD
           PACK      NBRKFLD FROM iBRKNUM,iBRKCNT
           CMATCH    B1 TO NBRKFLD
           return    IF EOS
         call      nbrkkey
         IF        NOT OVER
         move      mcomp to bilCOMP
           move      BRCOMP to bilNAME
           ENDIF
         RETURN
.  
oops     display    *p1:24,*el,*blinkon,"Now you've Done it!!!!!!!",*b,*b,*b,*w10
         stop         
............................................................................
         include  tinvio.inc
         include  nordio.inc
.START PATCH 1.5 REPLACED LOGIC
.         include   nmlrio.inc
.         include   nbrkio.inc
         include   compio.inc
         include   cntio.inc
.END PATCH 1.5 REPLACED LOGIC
         include  nownio.inc
;begin patch 1.6
;         include  ninvio.inc
	Include	ninvio.inc
	Include	NInvAcdio.inc
;         include  compute.inc
         	include  	compute.inc
;end patch 1.6
         include  nbilio.inc
         include  nmrgio.inc
         include  nshpio.inc
         include  ndat3io.inc
         include  nacdio.inc
         include  ndatio.inc
         include  nxcgio.inc 
         include  cvt.inc       
         include  comlogic.inc         


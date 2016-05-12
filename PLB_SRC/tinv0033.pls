pc       equ       0
         include   common.inc
         include   cons.inc                   
         include   norddd.inc
.START PATCH 1.9 REPLACED LOGIC
.         include   nbrkdd.inc
.         include   nmlrdd.inc
	include	compdd.inc
	include	cntdd.inc
.END PATCH 1.9 REPLACED LOGIC
         include   tinvdd.inc
;begin patch 1.2
         INCLUDE   NBILDD.inc
         include   consacct.inc
         include   cvtdd.inc
         include   hp.inc
         include   nmrgdd.inc
         include   ndatdd.inc
         include   nowndd.inc
         include   nshpdd.inc
         include   nacddd.inc
;begin patch 2.0         
;         include   ninvdd.inc
         	include   	ninvdd.inc
         	INclude	NInvAcddd.inc
;end patch 2.0         
         include   ndat3dd.inc
         include   nxcgdd.inc
;end patch 1.2

RELEASE  INIT      "2.1"            DMB  11MAY2005	Added code to account for new triplex billing file
;RELEASE  	INIT      	"2.0"            DLH 10March2005	Invoice CONVERSION
;RELEASE  INIT      "1.9"            ASH 26MAY2004	MAILER CONVERSION
.RELEASE  INIT      "1.8"            DLH 12Jul2002 Use GetWinVer
;RELEASE  INIT      "1.7"            ASH 04OCT2000 NEW SERVER ADDED
;RELEASE  INIT      "1.6"            ASH 17JAN99 DUPLICATE VARS NOW FOUND IN CONS.INC
;RELEASE  INIT      "1.5"            ASH 26JAN99 CONSACCT.INC VAR EXPANSION
;RELEASE  init      "1.4"            DLH 21Jan99 .change flag to only
;                                   bold and underline (neg)
;                                   * list management orders
;release  init      "1.3"            ASH 09Nov98 TDMCINV Y2K, File expansion
;Release  init      "1.2"            DLH 24Sep98 Add verification
;release  init      "1.1"            jd14nov97  added detail report.
;release  init      "1.00"
input    file     
lines    form      2
page     form      2
eop      form      "58"
skipcnt  form      5
APPLIED    FORM        5
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
; TRIPLEX BILLING VARIABLES.
;
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
;
;END TDMC.
LRMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      9.2      UNKNOWN LR INCOME.
LREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
LRBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
;
ARMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
AREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM      FORM      9.2      TOTAL MANAGEMENT A/R
ARBR     FORM      9.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN   FORM      9.2      UNKNOWN  A/R.
ARBBE      FORM        9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR      FORM        9.2      TOTAL BATCH BILL A/R RENT PORTION
;
APMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
APM      FORM      9.2      TOTAL MANAGEMENT A/P
APBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN   FORM      9.2      UNKNOWN A/P.
APBBE      FORM        9.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR      FORM        9.2      TOTAL BATCH BILL A/P RENT PORTION
;
ppflag   dim       1            'P' if equal else blank
PMASK    DIM       1
;
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
;FORM92   FORM      9.2
FORM11   FORM      11
form122  form      12.2
innets   dim       1
exfeflag dim       1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
mangflag dim       1       " " = brokerage order "*"=list management
mrgsw    dim       1
shipsw   dim       1 
;..............................................................................
;PRINT MASK VARIABLES
;
MASK22   INIT      "ZZ.ZZ"
;START PATCH 1.6 - DUPLICATE VARS
;MASK32   INIT      "ZZZ.ZZ-"
;END PATCH 1.6 - DUPLICATE VARS
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
;START PATCH 1.6 - DUPLICATE VARS
;MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
;END PATCH 1.6 - DUPLICATE VARS
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
;
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
;START PATCH #1.5 - INCREASED VAR
;M$AR     DIM       13
M$AR     DIM       15
;END PATCH #1.5 - INCREASED VAR
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
;START PATCH #1.5 - INCREASED VAR
;M$AP1    DIM       13
;M$AP2    DIM       13
;M$STAX   DIM       8
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
;END PATCH #1.5 - INCREASED VAR
M$CTAX   DIM       8
M$POST   DIM       6
;START PATCH #1.5 - INCREASED VAR
;M$LRINC  DIM       13
;M$NINC   DIM       13
;M$GROSS  DIM       13
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
;END PATCH #1.5 - INCREASED VAR
;
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
;
;...........................................................................
.Patch 2.1
mask12      init    "ZZZZZZZZZ.99"         ;formatting vars
Dim12a      dim     12	    ;formatting vars
n92	form 9.2
.Patch 2.1
         move      "TDMC Billing info" to stitle
         move      "TINV0003" to program 
         move      "Names in the News Ca" to compnme
         clock      date to today
         call       paint
;START PATCH 1.7 REPLACED LOGIC
;         splopen   "g:\data\tdmcbill.lst"
         PACK      STR35,NTWKPATH1,"TDMCBILL.LST"
         splopen   STR35
;END PATCH 1.7 REPLACED LOGIC
         trap       oops if f5
         move       "Abort" to pf5
         call       funcdisp
         open       input,"TDMCINV.IN",exclusive
         call       header
         MOVE      C1 TO NmlrPATH       *SET ACCESS TYPE.
         MOVE      C1 TO NORDPATH       *SET ACCESS TYPE.
         MOVE      C1 TO NinvPATH       *SET ACCESS TYPE.

loop
         read       input,seq;*CDFON,TINVLR,TINVDATE,TINVINV,TINVDOLR      
         goto       eoj if over
.Patch 2.1
         type      TINVLR
         goto      loop if not equal
         move 	   mask12 to dim12a
         move      tinvdolr to n92
         edit      n92 to dim12a         
         squeeze   dim12a,dim12a,"."
         call trim using dim12a
         move      dim12a to tinvdolr
         call      zfillit using tinvdolr
         call      trim using tinvdolr
.Patch 2.1
;Start Patch #1.3 - added logic to compensate for Triplex format change
.Patch 2.1 This is no longer necessary with new triplex billing file
.         unpack     tinvdate,mm,dd,cc,yy
.         pack       tinvdate,cc,yy,mm,dd                                      
.Patch 2.1         
;End Patch #1.3 - added logic to compensate for Triplex format change         
         add        c1 to n4
         display    *p12:10,"records in :",n4
         move       tinvlr to tinvfld
         rep        zfill in tinvfld
.           CALL        tinvTST
.           IF          OVER
.           .CALL        tinvWRT
.           ADD         C1 TO APPLIED
.             ..ELSE
.         .move      "Already in File!!" to mcomp
.         .add       c1 to skipcnt
.         endif
         move      tinvlr to nordfld
         move      "Order not found!!" to mcomp
         clear     str2
         clear     mangflag             .management
         MOVE      C1 TO NORDPATH       *SET ACCESS TYPE.
         call      nordkey
         if        not over
         pack      str2 from osales10,osales
         move      str2 to n2
         if        (n2 = 6)
         move      "*" to mangflag
         endif
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL IN MKEY
         CALL      NMLRKEY
         endif
det   
;begin patch 1.2
         move       tinvlr to ninvfld        
         move       tinvlr to nmrgfld
         move       tinvlr to nshpfld         
         call       ninvkey
         move       olon to nownfld
         MOVE      C0 TO FORM7
         MOVE      QTYbild TO FORM7
;
;START PATCH #1.5 - INCREASED VAR
;         MOVE      PPM TO FORM72
;         DIVIDE    HUND INTO FORM72
;         MOVE      FORM72 TO FORM32
         MOVE      PPM TO CMPT92
         DIVIDE    HUND INTO CMPT92
         MOVE      CMPT92 TO FORM32
;END PATCH #1.5 - INCREASED VAR
;
;
         CALL      READMLR
;
;
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
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
MASKIT   
;START PATCH #1.5 - INCREASED VAR
;         MOVE      MASK72 TO M$GROSS
;         EDIT      GROSS TO M$GROSS
;.
;         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
;.
;         MOVE      MASK72 TO M$AR
;         EDIT      FORMAR TO M$AR
;         ADD       FORMAR TO TOTAR
;.
;         MOVE      MASK72 TO M$AP1
;         EDIT      AP TO M$AP1
;         ADD       AP TO TOTAP1
;.
;         MOVE      MASK72 TO M$AP2
;         EDIT      FORMAP2 TO M$AP2
;         ADD       FORMAP2 TO TOTAP2
;         COMPARE   C0 TO FORMAP2
;         IF        EQUAL
;         CLEAR     M$AP2
;         endif
;.
;         MOVE      MASK72 TO M$LRINC
;         EDIT      LRINC TO M$LRINC
;         ADD       LRINC TO TOTLR
;.
;         MOVE      MASK72 TO M$NINC
;         EDIT      NININC TO M$NINC
;         ADD       NININC TO TOTNIN
;.
;         MOVE      MASK42 TO M$STAX
;         EDIT      TAXES TO M$STAX
;         ADD       TAXES TO TOTSTAX
;...
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
;
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
;
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
         ADD       FORMAR TO TOTAR
;
         MOVE      MASK92 TO M$AP1
         EDIT      AP TO M$AP1
         ADD       AP TO TOTAP1
;
         MOVE      MASK92 TO M$AP2
         EDIT      FORMAP2 TO M$AP2
         ADD       FORMAP2 TO TOTAP2
         COMPARE   C0 TO FORMAP2
         IF        EQUAL
         CLEAR     M$AP2
         endif
;
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
         ADD       LRINC TO TOTLR
;
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
         ADD       NININC TO TOTNIN
;
         MOVE      MASK52 TO M$STAX
         EDIT      TAXES TO M$STAX
         ADD       TAXES TO TOTSTAX
;END PATCH #1.5 - INCREASED VAR
;
         MOVE      C0 TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
;
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
;
*****************************************************************************
         MOVE      C0 TO RENTSW
         MOVE      OELCODE TO RENTSW
;...........................................................................
;  BATCH BILLING SECTION. 25MAR93
           PACK      MKEY FROM omlrnum,OCOBN
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
;          DISPLAY   *P10:16,*EL,*P10:18,*EL 
           CALL      NMLRKEY
;           CMATCH    "B" TO MCODE          .BATCH BILL ?
;         goto      bdate if equal
;           CMATCH    "A" TO MCODE          .BATCH BILL ?
;         goto      bdate if equal
         goto      slstype 
;.............................................................................
slstype   MATCH     OLNUM TO Lmanage
          IF        EQUAL
          MOVE      YES TO EXFEFLAG      
          goto      ManEXch               .Management exchange fee
          ENDIF
          MOVE      NO TO EXFEFLAG
          BRANCH    SALESBR OF BROKER,BROKER,MANAGE         
          goto      broker
;
;MANEXCH - LIST MANAGEMENT EXCHANGE FEE.

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
;
BROKER   BRANCH    BATCHBR OF BROKER2
;
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
;DET
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

;det
;PRINT
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
;
         if        (runchrg > 0)
         add       runchrg to runrar             .add exchange charges.
         endif
;                                                .prog 19 charges
         move      runrar to runrlr
         sub       tdmcamt from runrlr
;.................
;         
         move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         sub       runrpass,tdmcflat           .what part of tdmc charge is flat charges
         if        (tdmcflat < 0 )   
         move      c0 to tdmcflat
         endif
         
;         
         calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         calc      prcpass=runrpass/(runrpass+tdmcflat)*100
         calc      prcLRPASS=(RUNRLR/runrar)*100
         
         if        (runrlr < 0 | incRflat < 0)
;LETS ONLY PRINT REJECTS
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
         PRINT     *1,mangflag,*2,HPuNoN,OMLRNUM,HPUNOFF,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*50,M$AR,hpbon,ppflag,hpboff,*64,M$AP1," ":
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMP,*64,M$AP2," ",PMASK,HPUNOFF
         ELSE
         PRINT     *1,mangflag,*2,OMLRNUM,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
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
;02Jun98 DLH begin

         MOVE      MASK92 TO Mt$AP1
         MOVE      MASK92 TO Mt$AP2
         MOVE      MASK92 TO Mt$AR
         MOVE      MASK92 TO Mt$lrinc
         EDIT      RUNRAR TO Mt$AR
         EDIT      RUNRPASS TO Mt$AP1
         EDIT      RUNRflat TO Mt$AP2
         EDIT      RUNRlr TO Mt$lrinc
;tdmcamt = $ from Triplex invoice file
;Runrar = total receivable $ from triplex related additional charges
;runrpass = receivables from Run Charge, commisionable selects
;runrflat = receivables from Flat charges ie Mag tape, Shipping, etc.
;IncRFLat   = Profit from flat charges (tdmcamt - runrpass = tdmc flat charges 'TDMCFLAT'; 
;runrflat - tdmcflat = IncRflat)         

;let print it all
;         if        (runrlr < 0 | incRflat < 0)        
;
         if        (prclrflat < 0)
         print     hpunon,hpbon,*2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                   prcpass,"%":
                   *89,mt$lrinc,B1,PRCLRPASS,"%":
                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                   *91,INCrFLAT,B2,PRCLRFLAT,"%",hpboff,hpunoff
         else
         print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                   prcpass,"%":
                   *89,mt$lrinc,B1,PRCLRPASS,"%":
                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                   *91,INCrFLAT,B2,PRCLRFLAT,"%"
         endif
         ADD       C2 TO LINES
;         else          
;.LETS ONLY PRINT REJECTS
;;         print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
;;                   prcpass,"%":
;;                   *89,mt$lrinc,B1,PRCLRPASS,"%":
;;                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
;;                   *91,INCrFLAT,B2,PRCLRFLAT,"%"
;;         ADD       C1 TO LINES
;         endif
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
           goto      loop
;............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch,hpdupl,*f          .compressed, duplex
         endif
         PRINT     *f,*29,"***  N E W   T R I P L E X   ":
                   *60,"B I L L I N G   A N A L Y S I S  ***":
                   *116,"DATE ",today:
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

;end patch 1.2
;
;begin patch 1.2
;        PRINT     *1,tinvlr,*16,mcomp,*45,tinvinv,*59,tinvdolr
;         add       c1 to lines
;         compare   eop to lines
;         call      header if not less
;           DISPLAY   *ef,*P10:14,"RECORDS SKIPPED : ",skipcnt
;           DISPLAY   *ef,*P10:12,"RECORDS APPLIED : ",APPLIED
;           goto      loop
;end patch 1.2
;begin patch 1.2
;HEADER
;         ADD       C1 TO PAGE
;         PRINT     *F,*N
;         PRINT     *1,today,*21,"* * * NEW TDMC BILLING INFO * * *";
;         PRINT     *70,"Page: ",PAGE
;         PRINT     *N
;         PRINT     *1,"LR ##",*16,"MAILER",*45,"TDMC INV",*59,"AMOUNT"
;         move       c6 to lines
;         RETURN
READMLR  PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
         clear     bilname
;         MOVE      MCCTO TO BILNAME
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

;end patch 1.2
eoj
         compare   eop to lines
         call      header if not less
         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
.Patch 2.1
         PRINT     *L,*10,"NUMBER OF RECORDS SKIPPED: ",skipcnt         
.Patch 2.1           
         splclose
;begin patch 1.8
                    call                GetWinVer
;         path      exist,"c:\windows"
;         if        not over
;START PATCH 1.7 REPLACED LOGIC
;         execute  "!c:\command.com /c copy g:\DATA\tdmcbill.LST \\nts0\LASER2 "
;         execute  "!c:\command.com /c copy g:\DATA\tdmcbill.LST \\nts0\LASER2 "
;        else
;         execute  "!c:\winnt\system32\cmd.exe /c copy g:\DATA\tdmcbill.LST \\nts0\LASER2 "
;         execute  "!c:\winnt\system32\cmd.exe /c copy g:\DATA\tdmcbill.LST \\nts0\LASER2 "
;
                    If                  (osflag = c3 | osflag = c4)
         PACK     TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
                    ElseIf             (osflag = c1 | osflag = c5)
;        else
         PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
;END PATCH 1.7 REPLACED LOGIC
                    ElseIf             (osflag = c6)
         PACK     TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\nts0\LASER2 "
         execute  TASKNAME
;end patch 1.8
         endif
;         .execute "F:\PUBLIC\NPRINT g:\DATA\tdmcbill.LST Q=LASER2 NT NA=GS_JL f=0 S=NTS0_fpnw C=2"
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop
oops     display    *p1:24,*el,*blinkon,"Now you've Done it!!!!!!!",*b,*b,*b,*w10
         stop         
;...........................................................................
         include  tinvio.inc
         include  nordio.inc
.START PATCH 1.9 REPLACED LOGIC
.         include   nbrkio.inc
.         include   nmlrio.inc
	include	compio.inc
	include	cntio.inc
.END PATCH 1.9 REPLACED LOGIC
;begin patch 1.2
         include  nownio.inc
;begin patch 2.0         
;         include  ninvio.inc
         	include  	ninvio.inc
         	INclude	NInvAcdio.inc
;         include  compute.inc
         	include  	compute.inc
;end patch 2.0         
         include  nbilio.inc
         include  nmrgio.inc
         include  nshpio.inc
         include  ndat3io.inc
         include  nxcgio.inc 
         include  nacdio.inc
         include  ndatio.inc
         include  cvt.inc       
;end patch 1.2
         include  comlogic.inc         

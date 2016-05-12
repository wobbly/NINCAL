...............................................................................
.Ninv0006 - DAILY INVOICE REGISTER PROGRAM
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
.begin patch 2.8
.;begin patch 3.1
.;              INCLUDE   NINVDD.inc
               Include        ninvdd.inc
               Include        NInvAcddd.inc
.;end patch 3.1
         INC       CONSacct.inc
         include   nacddd.inc
         include   ndatdd.inc
         include   nshpdd.inc
shipsw   dim      1
mrgsw    dim      1
.end patch 2.8
         include   hp.inc
         include   Tinvdd.inc
;patch3.0
                              include        compdd.inc
                              include        cntdd.inc
.        INCLUDE   NMLRDD.inc
;patch3.0
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         include   nmrgdd.inc
;patch3.0
.         INCLUDE   NBRKDD.INC
;patch3.0
         include   ndat3dd.inc
         include   nmoadd.inc
.START PATCH 3.3 REMOVED LOGIC
.         include NFULDD.INC
.END PATCH 3.3 REMOVED LOGIC
         include   nxcgdd.inc
.
Release   Init      "3.33"    DLH       29NOv2007    add Xfoot check
.Release  Init      "3.32"    DLH       15May2007    Pacific Lists - brokerage side
.release         init "3.31"      JD  27NOV2006 Print bolded if AR/AP is negative.
.release         init               "3.3"            DMS                  21JUNE2006     Fulfillment Conversion
.release        init           "3.2"        DLH              07JUne2006     error checking
.release        init           "3.1"        DLH             8March2005     Invoice Conversion
.release  init      "3.0"        DMB         26MAY2004      Mailer Conversion
.RELEASE  INIT      "2.95"          JD  09DEC02 totaling # of outside/rentals.
.RELEASE  INIT      "2.91"          JD 19Sep00 added bolding to missing inv.
.RELEASE  INIT      "2.9"          DLH 13Sep00 MOA check at end of job
.RELEASE  INIT      "2.8"          DLH 5May99 NININV Y2K
.RELEASE  INIT      "2.7"          ASH 26JAN99 CONSACCT.INC VAR EXPANSION
.RELEASE  INIT      "2.6"          DLH 27May98 TRACK Triplex
.release  init      "2.5"         DLH 17may95 added check for moa entry on prepays
.RELEASE  INIT      "2.4"          jd  30apr95  added nmrgdd for compute chgs.
.release  init      "2.3"         DLH 14NOV94 CONSACCT.INC SPLITS.
.RELEASE  INIT      "2.2"          JD  27jun94 print to laser
.RELEASE  INIT      "2.1"         03AUG93   TYPISTS INITIALS TO 3 BYTES.
.RELEASE  INIT      "2.0"        DLH   20MAR92   NINVXX, NMLRXX, NBILXX,
.
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
.FILES.
...............................................................................
.
.
.begin patch 2.8
.NININVS  IFILE     KEYLEN=6,FIXED=305
NININVS  IFILE     KEYLEN=6,FIXED=403
.end patch 2.8
.begin patch 3.33
XfootFlag Dim       1                  ."Y" if any invoices did not balance
.end patch 3.33

.
.
. TRIPLEX BILLING VARIABLES.
.
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
LManage  init      "018710"    List management exchange fees only
.
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
.lrplus     form       9.2        =NINCA Billed amount > tdmcmant.
.END TDMC.
.
.
. WORK VARIABLES
LRMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      9.2      UNKNOWN LR INCOME.
LREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
form122  form      12.2
TDMCAMT  FORM       8.2
AREXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARM      FORM      9.2      TOTAL MANAGEMENT A/R
APM      FORM      9.2      TOTAL MANAGEMENT A/P
APEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
.
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.begin patch 3.2
HoldAR             form           10.2      ACCOUNTS RECEIVABLE (A/R),  X,XXX,XXX.XX
HoldAP1            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XX
HoldAP2            form           10.2      2ND ACCOUNTS PAYABLE (A/P2), X,XXX,XXX.XX
HoldAP3            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XXHOldAr
.end patch 3.2
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
MO       DIM       2
DY       DIM       2
YR       DIM       2
LOCAL    INIT      "LOCAL"
PRTFLAG  DIM       1
rntflg  dim       1
ANS      DIM       1
TYPIST   DIM       2
.
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
TOTTDMC  form      9.2

.Begin patch 3.32
SALESBR  FORM      2
SALESNUM DIM       2
TEAM1    INIT      "01"     Brokerage            .2007
TEAM2    INIT      "02"    Brokerage    .2007
TEAM3    INIT      "03"    LIST MANAGEMENT
TOTPLARp   FORM      9.2       *prepaid
TOTPLpMOA  FORM      9.2       *MOA Applied to prepaid
TOTPLAR    FORM      9.2
TOTPLAP1   FORM      9.2
TOTPLAP2   FORM      9.2
TOTPLAP    FORM      9.2
TOTPLNIN   FORM      9.2
TOTPLLR    FORM      9.2
TOTPLSTAX  FORM      9.2
TOTPLCTAX  FORM      6.2
TOTPLPOST  FORM      5.2
ninPLMRinc form      9.2      Total NIN ACD MAnAGEMENT RENTAL inc
ninPLMEinc form      9.2      Total NIN ACD MAnAGEMENT exch inc
ninPLBRinc form      9.2      Total NIN ACD Brokerage RENTAL inc
ninPLBEinc form      9.2      Total NIN ACD Brokerarge Exch inc
LRPLMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRPLMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRPLMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRPLBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRPLBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRPLUNKN   FORM      9.2      UNKNOWN LR INCOME.
LRPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
LRPLBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRPLBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
ARPLMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
ARPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
ARPLME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARPLM      FORM      9.2      TOTAL MANAGEMENT A/R
ARPLBR     FORM      9.2      TOTAL BROKERAGE/RENTAL  A/R
ARPLBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARPLUNKN   FORM      9.2      UNKNOWN  A/R.
ARPLBBE      FORM        9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARPLBBR      FORM        9.2      TOTAL BATCH BILL A/R RENT PORTION
.
APPLMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APPLME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APPLEXfee  form      9.2      Total Management Exchange fee - start 1/1/98
APPLM      FORM      9.2      TOTAL MANAGEMENT A/P
APPLBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APPLBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APPLUNKN   FORM      9.2      UNKNOWN A/P.
APPLBBE      FORM        9.2      TOTAL BATCH BILL A/P EXCH PORTION
APPLBBR      FORM        9.2      TOTAL BATCH BILL A/P RENT PORTION
COLDPLLm   FORM        9.2      New Biz LM
COLDPLBR   FORM        9.2      New Biz BR
COLDPLBE   FORM        9.2      New Biz BE
COUNTPL  FORM      5

.end patch 3.32
.
ZERO     FORM      "0"
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
.START PATCH #2.7 - VAR NOW FOUND IN CONSACCT.INC
.FORM92   FORM      9.2
.END PATCH #2.7 - VAR NOW FOUND IN CONSACCT.INC
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
CO       FORM      1
.
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
.START PATCH #2.7 - INCREASED VAR
.M$AR     DIM       13
.M$ARp    DIM       13     *prepaid
M$AR     DIM       15
M$ARp    DIM       15     *prepaid
.END PATCH #2.7 - INCREASED VAR
M$PPM    DIM       6
M$QTY    DIM       9
.START PATCH #2.7 - INCREASED VAR
.M$AP1    DIM       13
.M$AP2    DIM       13
.M$STAX   DIM       8
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
.END PATCH #2.7 - INCREASED VAR
M$CTAX   DIM       8
M$POST   DIM       6
.START PATCH #2.7 - INCREASED VAR
.M$LRINC  DIM       13
.M$NINC   DIM       13
.M$GROSS  DIM       13
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.END PATCH #2.7 - INCREASED VAR
.
MT$AR    DIM       15
MT$ARp   DIM       15     *prepaid
MT$pMOA  DIM       15     *prepaid
MT$AP1   DIM       15
MT$AP2   DIM       15
MT$STAX  DIM       15
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       15
MT$NINC  DIM       15
MT$TDMC  dim       15
.
NEW      FORM      5
neWout   FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
.begin release 2.9
moabadflag Init     "N"          Y = missing moa
.end release 2.9
.
         CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         MOVE      DATEMASK TO TODAY
         XIF
         IFZ       PC
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         XIF
         MATCH     "NINV0006" TO PROGRAM         .ENTRY FROM DSINIT?
         IF        NOT EQUAL                     . NO.
         MOVE      "NINV0006" TO PROGRAM           .SET DEFAULTS
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "NINVPRT2" TO INPNAME
         move      "slsdreg" to prtname
         ENDIF
         MOVE      "DAILY INVOICE REGISTER" TO STITLE
         CALL      PAINT
         move      c1 to nordpath
         move      c1 to ndatpath
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File   : ":
                   *P01:07,"Print File   : ":
                   *P01:08,"NIN Input Count  : ":
                   *P01:09,"PLI Input Count  : ":
                   *P01:10,"Today's Date : ",*p15:09,today
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      NININVS,INPNAME
          TRAP      IOMssg giving error IF IO
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      INPUT IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      INPUT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
         MOVE      ZERO TO PAGE
.         OPEN      NININVS,"NINVPRT",EXCLUSIVE
.
INPUT    COMPARE   ZERO TO PAGE
         CALL      HEADER IF EQUAL
.
.begin patch 3.32
.         ADD       c1 TO COUNT
.         DISPLAY   *P15:08,COUNT,b1,invnum,b1,lrn
.end patch 3.32
.
READINV  READKS    NININVS;INVVARS
         GOTO      TOTAL IF OVER
.begin patch 2.8
.         REP       zfill IN AR
.         REP       zfill IN AP1
.         REP       zfill IN AP2
.end patch 2.8
.begin patch 3.2
              Move            c0,holdar
              Move            c0,holdap1
              Move            c0,holdap2
              Move            c0,holdap3
              Move            Ar,HOldar
              move            ap1,holdap1
              move            ap2,holdap2
              move            ap3,holdap3
.end patch 3.2
.
         MATCH     LRN TO STR6
         GOTO      INPUT IF EQUAL
         MOVE      LRN TO STR6
.
         CMATCH    "R" TO STATB
         GOTO      REPRINT IF EQUAL
         ADD       c1 TO NEW
MISSCHK  COMPARE   NEW TO c1
         IF        EQUAL
         MOVE      INVNUM TO HOLDREC
         ENDIF
         MOVE      INVNUM TO NEXTREC          *CHECK
         COMPARE   NEXTREC TO HOLDREC         * FOR MISSING
         GOTO      MISSING IF NOT EQUAL        *INVOICE
         ADD       c1 TO HOLDREC
         GOTO      PROCESS
REPRINT  ADD       c1 TO REPRINT
         GOTO      INPUT
.
PROCESS
.begin patch 2.8
.               MOVE      ZERO TO FORM7
.         MOVE      QTYSHP TO FORM7
.end patch 2.8
.
.START PATCH #2.7 - INCREASED VAR
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
.begin patch 2.8
.         MOVE      PPM TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
.end patch 2.8
.END PATCH #2.7 - INCREASED VAR
.
.
         CALL      READBILL
.
.begin patch 3.32
         move      lrn to nordfld
         call      nordkey
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
          Else
.

.See Oslspern.inc for details.
...............................................................................
.
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM3,TEAM1,Team1,team1:                 .1-5
                   TEAM3,Team1,TEAM1,TEAM1,TEAM1:                            .6-10        
                   TEAM1,Team1,TEAM1,Team1,Team1:                            .11-15       
                   TEAM1,Team1,Team1,TEAM3,TEAM1:                            .16-20       
                   TEAM1,TEAM1,team1,team1,team1:                            .21-25       
.                   TEAM1,TEAM1,team1,team1,team3:                                     .26-30       
                   TEAM1,TEAM3,team3,team1,team3:                            .26-30       
                   TEAM1,TEAM1,team1,team1,team1                             .31-35       
         MOVE      SALESNUM TO SALESBR
         ENDIF
         
          if        (OcompId = "P" & SalesBr = c1)                              
          ADD       C1 TO COUNTPL
          elseif (OcompId2 = "P" & SalesBr = c3)                      
          ADD       C1 TO COUNTPL
          else
          ADD       C1 TO COUNT
          endif
         DISPLAY   *P15:08,COUNT,b5,lrn,b1,invnum:
                   *P15:09,COUNTPL,b5,lrn,b1,invnum
.end patch 3.32
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
.
...........................................................................
.DLH 02Jun98 begin
         move       no to tdmchrg
         move       c0 to runchrg

         move      lrn to nxcgfld
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
.DLH 02Jun98 - end
...........................................................................


.get triplex info
         move        c0 to TDMCAMT
         move       lrn to tinvfld
         call       tinvkey
         if         not over
         move       yes to tdmchrg
         move       tinvdolr to form122
         mult       ".01" by form122
         move        c0 to TDMCAMT
         add         form122 to TDMCAMT
moretdmc call       tinvks
         goto       tdmcexit if over
         match      tinvfld to tinvlr
         goto       tdmcexit if not equal
         move       c0 to form122
         move       tinvdolr to form122
         mult       ".01" by form122
         add         form122 to TDMCAMT
         goto       moretdmc
         endif
tdmcexit
         add        tdmcamt to tottdmc

.DLH 28MAy98
         move      c2 to tdmcflag     .force compute to calc tdmc goodies
.
.
.begin patch 2.8
         move      olon to nownfld
         call      nownkey
.
         move      olnum to ndatfld
         call      ndatkey
chcker   CMATCH    "C" TO ELSTCDE
         IF        not equal
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         if        equal
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9
         if        equal
          goto     wipegoods
          else
          add      c1 to newout
          move     yes to rntflg
         goto      wipegoods
         endif
         endif
;
renter   add       c1,newout
         move      yes to rntflg
         goto       wipegoods
         endif
;
wipegoods
         call      wipecvars
.end patch 2.8

               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE

.
MASKIT
.START PATCH #2.7 - INCREASED VAR
.         MOVE      MASK72 TO M$GROSS
.         EDIT      GROSS TO M$GROSS
..
.         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
.         clear     m$arp
.         compare   c0 to prepay
.         if        not equal
.         MOVE      MASK72 TO M$ARp
.         edit      prepay to m$arp
.         endif
.
.         MOVE      MASK72 TO M$AP1
.         EDIT      AP TO M$AP1
.         ADD       AP TO TOTAP1
..
.         MOVE      MASK72 TO M$AP2
.         EDIT      FORMAP2 TO M$AP2
.         ADD       FORMAP2 TO TOTAP2
.         COMPARE   ZERO TO FORMAP2
.         CALL      ZEROAP2 IF EQUAL
..
.         MOVE      MASK72 TO M$AR
.         EDIT      FORMAR TO M$AR
.         ADD       FORMAR TO TOTAR
..
.         MOVE      MASK72 TO M$LRINC
.         EDIT      LRINC TO M$LRINC
.         ADD       LRINC TO TOTLR
..
.         MOVE      MASK72 TO M$NINC
.         EDIT      NININC TO M$NINC
.         ADD       NININC TO TOTNIN
..
.         MOVE      MASK52 TO M$STAX
.         EDIT      TAXES TO M$STAX
.         ADD       TAXES TO TOTSTAX
...
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
         clear     m$arp
         compare   c0 to prepay
         if        not equal
         MOVE      MASK92 TO M$ARp
         edit      prepay to m$arp
         endif
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
         COMPARE   ZERO TO FORMAP2
         CALL      ZEROAP2 IF EQUAL
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
.END PATCH #2.7 - INCREASED VAR
.
         MOVE      ZERO TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
.
         GOTO      PRINT
.
ZEROAP2  CLEAR     M$AP2
         RETURN
.
.READBIL
READBILL
.
.         PACK      NBILFLD FROM MLRN,COBN,BILLTN
.         CALL      NBILKEY
.         CALL      READMLR IF OVER
. .        RETURN
READMLR  PACK      MKEY FROM MLRN,COBN
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
.READMLR  PACK      MKEY FROM MLRN,COBN
.         CALL      NMLRKEY
.         MOVE      MCOMP TO BILCOMP
.         MOVE      MCCTO TO BILNAME
.         RETURN
.
.
*......................................................................
.
PRINT
.         COMPARE   "59" TO LINES
         COMPARE   "56" TO LINES
.         CALL      HEADER IF EQUAL
         CALL      HEADER IF GREATER
         compare    c0 to prepay
         if         not equal
         print      *9,hpbon,lrn,*18,invnum,hpboff;
         else
         print      *9,lrn,*18,invnum;
         endif
.START PATCH 3.31
                   if         (holdap1 < zero|holdar < zero)
         print      hpbon,*26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                    *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                    *L,*1,COBN,"-",BILLTN,hpboff;
                  else
         print      *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                    *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                    *L,*1,COBN,"-",BILLTN;
                  endif
.         print      *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
.                    *79,M$NINC,*91,M$LRINC,*107,M$STAX:
.                   *117,M$CTAX,*127,M$POST:
.                    *L,*1,COBN,"-",BILLTN;
.End PATCH 3.31
         cmatch     yes to rntflg
         if         equal
         print      *17,hpbon,INVDTEM,"/",INVDTED,"/",INVDTEY,hpboff;
         else
         print      *17,INVDTEM,"/",INVDTED,"/",INVDTEY;
         endif
         print       *26,BILCOMP,*50,hpbon,m$arp,hpboff,*64,M$AP2
         ADD       c2 TO LINES
         move      no to rntflg
.begin patch 3.33

          IF        (ar <> ap1+ap2+ap3+lrinc+nininc+xninc & ar+Prepay <> ap1+ap2+ap3+lrinc+nininc+xninc)
          call      debug
          Print     *l,hpbon,"******** OUT OF BALANCE **********",HpBoff         .
          Move      Yes,Xfootflag
          add       c2,Lines
          endif
.end patch 3.33
JD
         if        (tdmchrg = "Y" | runrar > 0)
         add        c2 to lines
.
         if        (runchrg > 0)
         add       runchrg to runrar             .add exchange charges.
         endif
.         add       runchrg to runrpass
.         move      c0 to form92
.         move      runchrg to form92
.         sub       tdmcamt,form92
.         add       form92 to runrlr
.         endif
.
         move      runrar to runrlr
         sub       tdmcamt from runrlr
.
         move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         sub       runrpass,tdmcflat           .what part of tdmc charge is flat charges
         if        (tdmcflat < 0 )
         move      c0 to tdmcflat
         endif

.         move      c0 to IncRflat
.         move      RunRFlat to incRflat           .income on flat charges
.         sub       tdmcflat,IncRFlat
.
         calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         calc      prcpass=runrpass/(runrpass+tdmcflat)*100
.         calc      prcLRflat=INCrflat/(incrflat+runrlr)*100
         calc      prcLRPASS=(RUNRLR/runrar)*100
.         move      c0 to LRPlus
.         calc      LRPlus=runrar-tdmcamt-runrlr-incrflat

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
                   *91,INCrFLAT,B2,PRCLRFLAT,"%",hpboff,hpunoff;
         else
         print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                   prcpass,"%":
                   *89,mt$lrinc,B1,PRCLRPASS,"%":
                   *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                   *91,INCrFLAT,B2,PRCLRFLAT,"%";
         endif
         clear     tdmchrg
         add        runrar to grunrar
         add        runrpass to grunrpass
         add        runrflat to grunrflat
         add        runrlr to grunrlr
         endif
.begin patch 3.33
          move      c0,onAmount
.end patch 3.33

         cmatch    yes to ppsw
         if        equal
         move      lrn to nmoafld5
         move      c5 to  nmoapath
         call      nmoakey
                if        over
                print     hpbon,*2,"No MOA entry found for this invoice!!!!!!",hpboff,*l
.begin  release 2.9
         move      yes to moabadflag
.end  release 2.9
                add       c2 to lines
         goto      moaexit
                endif
moaloop  compare   "18" to reason
                if        equal
         print     *2,"MOA Prepayment ",*52,onamount,*l
         add       onamount to totpmoa       .total prepay moa
         add       c2 to lines
         goto      moaexit
         else
         call      nmoaks
                   if        over
         print     hpbon,*2,"No MOA Prepayment entry found for this invoice!!!!!!",hpboff,*l
.begin  release 2.9
         move      yes to moabadflag
.end  release 2.9
         add       c2 to lines
         goto      moaexit
                   else
         goto      moaloop
                   endif
                endif
         endif
         print     b1
         add       c1 to lines
moaexit
.begin patch 3.2
              IF              (Formar <> HoldAr & PPSW <> Yes)
              PRINT     *L,*1,hpbon,"Computed Ar does not Match  ",HOLDAR,hpboff
              Elseif          (Formar-Prepay <> Holdar & PPSW = Yes)
              PRINT     *L,*1,hpbon,"Computed Ar does not Match  ",HOLDAR,hpboff
              endif
              IF              (ap <> HoldAp1)
              PRINT     *L,*1,hpbon,"Computed Ap1 does not Match  ",HOLDAp1,hpboff
              endif
              IF              (Formap2 <> HoldAp2)
              PRINT     *L,*1,hpbon,"Computed Ap2 does not Match  ",HOLDAp2,hpboff
              endif
              IF              (ap3 <> HoldAp3)
              PRINT     *L,*1,hpbon,"Computed Ap3 does not Match  ",HOLDAp3,hpboff
              endif
.end patch 3.2
         move       c0 to runrar
         move       c0 to runrlr
         move       c0 to runrpass
         move       c0 to runrflat
         GOTO      INPUT
*......................................................................
.
TOTAL    COMPARE   "59" TO LINES
         CALL      HEADER IF EQUAL
         MOVE      MASK92 TO MT$AR
         EDIT      TOTAR TO MT$AR
         mult      seq by totarp
         MOVE      MASK92 TO MT$ARp
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
         PRINT     *26,"***DAILY TOTAL",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC:
                   *115,MT$CTAX:
                   *l,*62,MT$AP2,*77,MT$NINC,*103,MT$STAX,*124,MT$POST:
                   *L,*26,"Code 96 ",*48,MT$ARP
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
         print     *26,"Moa prepay debit",*48,MT$pmoa
.begin release 2.9
         cmatch    yes to moabadflag
         if        equal
         print     hpbon,*2,"MOA Prepayment entries are MISSING!!!!!!",hpboff,*l
         endif
.end release 2.9
         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP TO MT$AP1
         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,MT$AP1
         PRINT     *L,*L,*26,"REPRINTS  : ",REPRINT:
                   *L,*22,"NEW INVOICES  : ",new,b2,"NEW OUTSIDE RENTALS :",NEWout
..............................................................................
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

..............................................................................
.TRIPLEX  running charge invoices for month
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
         PRINT     *5,"TRIPLEX RUN Charge ","##",RUNCOUNT,*48,MT$AR:
                   *62,MT$AP1:
                   *89,MT$LRINC:
                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2
.
.TRIPLEX  prog 19 entries for month
         MOVE      MASK92 TO MT$LRINC
         EDIT      gRUNRLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      gRUNrAR TO MT$AR
         EDIT      gRUNRPASS TO MT$AP1
         EDIT      gRUNRFLAT TO MT$AP2
         PRINT     *5,"TRIPLEX   Charges","##",RUNRCNT,*48,MT$AR:
                   *62,MT$AP1:
                   *89,MT$LRINC:
                   *L,*5,"FLAT PASS THROUGH    ",*62,MT$AP2

         MOVE      MASK92 TO MT$TDMC
         EDIT      TOTTDMC TO MT$TDMC
         PRINT     *5,"TRIPLEX Billed us  Charges","##",*48,MT$TDMC

.
.         MOVE      MASK92 TO MT$LRINC
.         EDIT      CMREDOLR TO MT$LRINC
.         PRINT     *L,*L,HPBON:
.                    *5,"10 %COMMISION RECAPTURE    ":
.                   *89,MT$LRINC,HPBOFF
.begin patch 3.75
          add       c2,lines
         COMPARE   "59" TO LINES
         CALL      HEADER IF EQUAL
         MOVE      MASK92 TO MT$AR
         EDIT      TOTPLAR TO MT$AR
         MOVE      MASK92 TO MT$ARp
         mult      seq by totPLarp
         EDIT      TOTPLARp TO MT$ARp
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTPLAP1 TO MT$AP1
         MOVE      MASK92 TO MT$AP2
         EDIT      TOTPLAP2 TO MT$AP2
         MOVE      MASK92 TO MT$NINC
         EDIT      TOTPLNIN TO MT$NINC
         MOVE      MASK92 TO MT$LRINC
         EDIT      TOTPLLR TO MT$LRINC
         MOVE      MASK92 TO MT$STAX
         EDIT      TOTPLSTAX TO MT$STAX
         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTPLCTAX TO MT$CTAX
         MOVE      MASK52 TO MT$POST
         EDIT      TOTPLPOST TO MT$POST
              PRint           *l,*5,"## PL INVOICES ",COUNTPL,hpboff
.         
         PRINT     *26,"***PL DAILY TOTAL",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC:
                   *115,MT$CTAX:
                   *l,*62,MT$AP2,*77,MT$NINC,*103,MT$STAX,*124,MT$POST:
                   *L,*26,"Code 96 ",*48,MT$ARP
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
         print     *26,"Moa prepay debit",*48,MT$pmoa


         ADD       TOTPLAP1 TO TOTPLAP
         ADD       TOTPLAP2 TO TOTPLAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTPLAP TO MT$AP1
         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,MT$AP1
         PRINT     *L,*L,*26,"REPRINTS  : ",REPRINT:
                   *L,*22,"NEW INVOICES  : ",new,b2,"NEW OUTSIDE RENTALS :",NEWout

         MOVE      MASK92 TO MT$LRINC
         MOVE      C0 TO LRPLMINC
         ADD       LRPLEXFEE TO LRMINC
         EDIT      LRPLMINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      C0 TO ARM
         MOVE      C0 TO APM
         ADD       ARPLEXFEE TO ARM
         ADD       APPLEXFEE TO APM
         EDIT      ARM TO MT$AR
         EDIT      APM TO MT$AP1
         PRINT     *5,"TOTAL MANAGEMENT EXCH FEE   ",*48,MT$AR,*62,MT$AP1:
                   *89,MT$LRINC
.
..............................................................................
.end patch 3.75

.
        GOTO      EOJ
.............................................................................
HEADER
         ADD       c1 TO PAGE
         compare    c1 to page
         if         equal
;         print      hpreset,hp17ptch,hpdupl,*f          .compressed, duplex
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
         PRINT     *f,*31,"***  N I N   D A I L Y   ":
                   *57,"I N V O I C E   R E G I S T E R  ***":
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
         MOVE      "6" TO LINES
         RETURN
.
*............................................................
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
.         MULTIPLY  seq   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
MISSING  PRINT     *L,*1,hpbon,"********* MISSING INVOICE ******  ",HOLDREC,hpboff;
         PRINT     *1,hpbon,"********* MISSING INVOICE ******  ",HOLDREC,hpboff:
                   *FLUSH
         ADD       c1 TO HOLDREC
         add       c3 to lines
         GOTO      MISSCHK
........................
.
.
EOJ
          if        (XfootFlag = Yes)
          PRint     *l,HPBon,"OUT OF BALANCE INVOICE IN REGISTER",HPBoff
          endif
         shutdown  "cls"
         STOP
.begin patch 2.8
;begin patch 3.1
               Include        compute.inc
               include        NInvAcdio.inc
               include        ninvio.inc
;INCLUDE   COMPUTE.inc
;end patch 3.1
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
         include   nownio.inc
.end patch 2.8
;patch3.0
                              include        compio.inc
                              include        cntio.inc
.        INCLUDE   NMLRIO.inc
;patch3.0
         INCLUDE   NBILIO.inc
         include   nordio.inc
         INCLUDE   NDAT3IO.INC
         include   nmoaio.inc
;patch3.0
.        INCLUDE   NBRKIO.INC
;patch3.0
         INCLUDE   NMRGIO.INC
         include   tinvio.inc
         include   nxcgio.inc
.START PATCH 3.3 REMOVED LOGIC
.         include NFULIO.inc
.END PATCH 3.3 REMOVED LOGIC
         INCLUDE   COMLOGIC.inc

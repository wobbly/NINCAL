...............................................................................
.NINV0666 - INVoice checking
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   consacct.inc
         include   hp.inc
              INC             ninvdd.inc
              Include         NInvAcddd.inc
                              include        compdd.inc
                              include        cntdd.inc
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         INCLUDE   GNXTDD.inc
         INCLUDE   NDAT3DD.inc
        include winapi.inc
         inc       nmrgdd.inc
         include   nslsdd.inc
         include   nmoadd.inc
         include   nxcgdd.inc
         include   tinvdd.inc
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
           include nprjdd.inc
           include nrevdd.inc
          
.
...........................................
release        init           "1.00"        DLH              20JUly2006     error checking  
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
time          Dim             8
.FILES.
...............................................................................
SLSFILE  FILE
.
DUPEOWN  IFILE     KEYLEN=4
OWNKEY   DIM       4    *DUPE OWNER FILE.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
.
.
.  KEY VARIABLES
.............................................
.
BEGINV   DIM       6    *STARTING INVOICE NUMBER FOR MONTH
TAB      FORM      "37"
.
. WORK VARIABLES
..............................................
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
ANS      DIM       1
TYPIST   DIM       2
HoldAR             form           10.2      ACCOUNTS RECEIVABLE (A/R),  X,XXX,XXX.XX
HoldAP1            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XX
HoldAP2            form           10.2      2ND ACCOUNTS PAYABLE (A/P2), X,XXX,XXX.XX
HoldAP3            Form           10.2     LIST OWNER AMOUNT (A/P1), XX,XXX.XXHOldAr        

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
.begin patch 2.9
ninMRinc form      9.2      Total NIN ACD MAnAGEMENT RENTAL inc
ninMEinc form      9.2      Total NIN ACD MAnAGEMENT exch inc
ninBRinc form      9.2      Total NIN ACD Brokerage RENTAL inc
ninBEinc form      9.2      Total NIN ACD Brokerarge Exch inc
.end patch 2.9
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
FORM9A    FORM      9
FORM52   FORM      5.2
FORM11   FORM      11
form122  form      12.2
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      6
CO       FORM      1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
SALESNUM DIM       2
TEAM1    INIT      "01"     SUSAN
TEAM2    INIT      "02"    ELAINE
TEAM3    INIT      "03"    LIST MANAGEMENT
DIM9     DIM       9
dim9B    dim       9
DIM9A    DIM       9
AMOUNT1  DIM       10
FORM102  FORM      10.2
FORM102W FORM      10.2
newfld   dim       10
MINUS    INIT      "-"
FORM9B    FORM      9
FORM9    FORM      9
apsw     dim       1
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
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       15
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       15
M$AP2    DIM       15
M$STAX   DIM       10
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
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
lastinv  dim       6
EXFEFLAG DIM       1          ."Y" = EXCHANGE MAN FEE "N" DEFAULT
mrgsw    dim       1
shipsw   dim       1
moabadflag Init     "N"          Y = missing moa
check    form      1
PRFILE        pfile
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
Column8  form    9
Column9  form    9
Column10 form    9
Column11 form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
check1   form      5
check2   form      5 
today1   form      5
revdat   form      5
COLDLm   FORM        9.2      New Biz LM
COLDBR   FORM        9.2      New Biz BR
COLDBE   FORM        9.2      New Biz BE
newbiz    init      "N"
OLDTOT   FORM    10.2
typer    dim      1
srcr     dim      1
cidr     dim      6
mmrep    dim       2
yyrep    dim       2
ccrep    dim       2
. 385477 FIRST INV# OF 2000

.............................................................................................
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
              move    "4250" to Header1
              move    "4150" to Title1       
              move    "5450" to Title2
              move    "6750" to Title3       
        move    "200",column
        move    "575",column1
        move    "1000",column2
        move    "1500",column3
        move    "3200",column4
        move    "3600",column5
        move    "4400",column6
        move    "5400",column7
        move    "6250",column8
        move    "6800",column9
        move    "7300",column10
        move    "4000",column4R
        move    "4700",column5R
        move    "5400",column6R
        move    "6100",column7R
        move    "6600",column8R
        move    "7100",column9R
        move    "7700",column10R
        
.
	MOVE      "Names in the News" TO COMPNME
         MOVE      "NINV0666" TO PROGRAM
         MOVE      "INVOICE Verification REGISTER" TO STITLE
              call            "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
              "Parameters":
              "ProcessPDF":
              "\\nts0\c\apps\plb\code\pdftest.bat":
              result
              if (result = C0)
.Prepare Flag file
               prep           tempfile,"c:\progra~1\pdf995\flag.dat"
               write          tempfile,SEQ;"flag set"
               close          tempfile
              endif
. Logic Addition for PDF Quality Control                                       
.
               PRTOPEN prfile,"PDF995",PRTNAME
               PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                                              *Duplex=2;                                                 
         MATCH     B8 TO TODAY
         IF        EQUAL
         goto      clock
                   ELSE
         IF        EOS
         goto      clock
         ENDIF
         ENDIF
         goto      date1
.       
CLOCK    CLOCK     DATE TO TODAY
DATE1    MOVE      TODAY TO DATEMASK
         UNPACK    TODAY INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
         DISPLAY   *P01:06,"Input File  :NININV ":
                   *P01:07,"Eom Date : "
         clear     str45
.
OPEN      TRAP      IO GIVING ERROR IF IO
          MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
          MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
          move      c3 to nordlock
          move      c3 to nShplock
          TRAPCLR   IO
.
INPUT    COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
	Move	c0 to holdrec
         move      c1 to ndatpath
         move      c3 to ndatlock
         move      c3 to Tinvlock
.FIRSTN   INIT      "500807"   FIRST NIN INV# OF 2005
.                    520530	2006
.         packkey	ninvfld,"385477"
.         packkey	ninvfld,"520530"  Last of 2005
         packkey	ninvfld,"537587"  Last of 1/07
	call	Ninvkey
.
.
READINV  CALL      NINVKS
         GOTO      TOTAL IF OVER
         MOVE      B1 TO RUNFLAG
         ADD       C1 TO COUNT
.         .triplex running charge list?
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum:
                   *P15:07,today
              Move            c0,holdar
              Move            c0,holdap1
              Move            c0,holdap2
              Move            c0,holdap3
              Move            Ar,HOldar
              move            ap1,holdap1
              move            ap2,holdap2
              move            ap3,holdap3
.lets not check until at least the year 2000
MISSCHK  
	if	(invdtec = "20" & invdtey >= "00")
			if	(holdrec = c0)
			move	invnum to holdrec               .1st instance
			endif
	MOVE      INVNUM TO NEXTREC          *CHECK
         	COMPARE   NEXTREC TO HOLDREC         * FOR MISSING
.         	GOTO      MISSING IF NOT EQUAL        *INVOICE
         	ADD       C1 TO HOLDREC
	endif
         	MOVE      LRN TO NORDFLD
         	REP       ZFILL IN NORDFLD
         	CALL      NORDKEY
         MATCH     OLNUM TO "015030"
         goto      readinv if not equal
.
         DISPLAY   *P10:13,"                              ",b5,b5,invdtem,slash,invdted,slash,invdtec,invdtey
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST          .triplex running charge list?
         IF         EQUAL                     .Yes
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
         MOVE       C0 TO CMPT92
         MOVE       QTYbild TO FORM82
         MOVE       FORM82 TO CMPT92    
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
         MULT       ".01" BY FORM92
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
...........................................................................
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
         MOVE      C0 TO FORM9A
         MOVE       QTYbild TO FORM9a
          move      ppm to form32
.
.
         CALL      READBILL
.
.
         move      c1 to nownpath
         move      olon to nownfld
         call      nownkey
         move      c2 to tdmcflag
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

...........................................................................
         move      c2 to tdmcflag     .force compute to calc tdmc goodies
............................................................................         
         move      olnum to ndatfld
         call      ndatkey
         move      lrn to nshpfld
         move      no to shipsw

         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               

         CALL      COMPUTE
.
MASKIT   
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
.
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
         ADD       FORMAR TO TOTAR
         move      c0 to form102w
         add       formar to form102w
.
         MOVE      MASK92 TO M$AP1
         EDIT      AP TO M$AP1
         ADD       AP TO TOTAP1
.
         MOVE      MASK92 TO M$AP2
         EDIT      FORMAP2 TO M$AP2
         ADD       FORMAP2 TO TOTAP2
         COMPARE   C0 TO FORMAP2
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
.
         MOVE      C0 TO TAXES
         MOVE      MASK42 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
*****************************************************************************
         MOVE      C0 TO RENTSW
         MOVE      OELCODE TO RENTSW
............................................................................
           PACK      MKEY FROM MLRN,z3
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
           CALL      NMLRKEY
           CMATCH    "B" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
           CMATCH    "A" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
         goto      slstype 
bdate
         MOVE      C0 TO N2
         MOVE      OMDTEM TO N2           .we will use the month instead
bdatedebug
         compare   c0 to n2
         goto     slstype if equal
         move     c0 to n4
         clear     str4
         pack      str4 from omdtec,omdtey     .maildate century & year
         move      str4 to n4
         clear     str4
         pack      str4 from cc,sysyr          .current century & year
         move      c0 to n5
         move       str4 to n5
        sub        n4 from n5
          if       (n5 > 0 )                  .from previous year
          move     c1 to batchbr              .so set flag  on          
          goto      slstype
           ENDIF
          if       (n5 < 0 )                  .from future year
          move     c0 to batchbr              .so set flag off
          goto      slstype
           ENDIF
MONTHCK    MOVE      C0 TO N2
           MOVE      OMDTEM TO N2 
           MOVE      SYSMO TO N3
           SUB       N2 FROM N3
           if        (n3 > 0)                   maildate is from a previous month
bdatedebug1
             MOVE      C1 TO BATCHBR
             ENDIF
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
         move      "M",src
         move      b1,type
         move      OLNUM to cid
         BRANCH   RENTSW OF MRENT,MEXCH,MEXCH
MRENT
        move       no to newbiz
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldlm
                              move      no to newbiz
                              else
                              ADD       LRINC TO LRMRINC        *RENT/MANAGEMENT INCOME
                             move       no to newbiz
                              endif

         ADD       NININC TO NINMRINC        *RENT/MANAGEMENT INCOME
         ADD       FORMAR TO ARMR
         ADD       AP TO APMR
         ADD       FORMAP2 TO APMR
         GOTO      PRINT
MEXCH    
         ADD       LRINC TO LRMEINC        *EXCH/MANAGEMENT INCOME
         ADD       NININC TO NINMEINC        *EXCH/MANAGEMENT INCOME
         ADD       FORMAR TO ARME
         ADD       AP TO APME
         ADD       FORMAP2 TO APME
         GOTO      PRINT
.
BROKER 
       
         BRANCH    BATCHBR OF BROKER2
.
BROKER1  BRANCH    RENTSW OF BRENT,BEXCH,BEXCH
           GOTO      BRENT
BROKER2  BRANCH    RENTSW OF BRENTB,BEXCHB,BEXCHB
           GOTO      BRENTB
BRENT
         move      "B",src
         move      "R",type
                              pack      cid,"00",mlrn
        move       no to newbiz
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldbr
                            move       no to newbiz
                              else
                              add       lrinc to lrbrinc
                              move      no to newbiz
                              endif
         ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
         ADD       FORMAR TO ARBR
         ADD       AP TO APBR
         ADD       FORMAP2 TO APBR
         GOTO      PRINT
BEXCH
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
        move       no to newbiz
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldbe
                             move       no to newbiz
                              else
                              add        lrinc to lrbeinc
                              move      no to newbiz
                              endif
         ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
         ADD       FORMAR TO ARBE
         ADD       AP TO APBE
         ADD       FORMAP2 TO APBE
         GOTO      PRINT
BRENTB
         move      "B",src
         move      "R",type
                              pack      cid,"00",mlrn
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldbr
                              else
                              add       lrinc to lrbbr
                              move      no to newbiz
                              endif
         ADD       NININC TO NINBRINC        *RENTAL/BROKERAGE INCOME
         ADD       FORMAR TO ARBBR
         ADD       AP TO APBBR
         ADD       FORMAP2 TO APBBR
         GOTO      PRINT
BEXCHB
         move      "B",src
         move      "E",type
                              pack      cid,"00",mlrn
         call      newbus
                              if        (newbiz = yes)
                              add       lrinc to coldbE
                              else
                              add       lrinc to lrbbe
                              move      no to newbiz
                              endif
         ADD       NININC TO NINBEINC        *EXCH/BROKERAGE INCOME
         ADD       FORMAR TO ARBBE
         ADD       AP TO APBBE
         ADD       FORMAP2 TO APBBE
         GOTO      PRINT
.
ZEROAP2  CLEAR     M$AP2
         RETURN
.
.READBIL
READBILL
.READMLR 
READMLR
         PACK      MKEY FROM MLRN,z3
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
         clear     bilname
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
;begin patch 3.7
NEWBUS
                match     "M",src
                goto       projsav if equal
       PACK      MKEY FROM mlrn,"000"
       call        NMLRKEY
       move       cc to ccrep
       move       mm to mmrep
       move       yy to yyrep
      unpack    COMPCNTDATE INTO cc,yy,mm,dd
       move      cid to cidr
       move      compnum to cid
       move      type,typer
       move      src,srcr
       move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           usage in last year
         if LESS
                              move       yes to newbiz
                              return
                              endif
                              endif
         move      ccrep to cc
         move      mmrep to mm
         move      yyrep to yy
         goto     projread
.
projsav
          move    type,typer
                   move    src,srcr
                   move    cid,cidr
projread
          pack    nPrjfld with TYPE,SRC,cid,"200601"
                 call    nprjkey
                 if      (projlr > 0)                                                 
                 return
                 else  
                 pack    nrevfld,typer,srcr,cidr,"2005"
                 call    nrevkey
                 call    swaptype if over
                                add JANLR,OLDTOT
                                add FEBLR,OLDTOT
                                add MARLR,OLDTOT
                                add APRLR,OLDTOT
                                add MAYLR,OLDTOT
                                add JUNLR,OLDTOT
                                add JULLR,OLDTOT
                                add AUGLR,OLDTOT
                                add SEPLR,OLDTOT
                                add OCTLR,OLDTOT
                                add NOVLR,OLDTOT
                                add DECLR,OLDTOT
                           if (OLDTOT = C0) 
                 move        yes to newbiz
                 move   c0,oldtot
                 return
                 endif
                 endif
                 move   c0,oldtot
      return                                     
*......................................................................
.
PRINT    
.              IF              (ar = HoldAr & ap1 = HoldAp1 & ap2 = HoldAp2 & ap3 = HoldAp3)
.              
	if	(ppsw = yes & FormAr-prepay=holdar & ap1 = HoldAp1 & ap2 = HoldAp2 & ap3 = HoldAp3)
	goto	Readinv
	endif
.	
	call	debug
              	COMPARE  "9900" to ROW
        	CALL      HEADER IF not less
	clear     ppflag
	cmatch    yes to ppsw

	if        equal 
	move      "P" to ppflag
	endif
         	move      c0 to n2
         	move      onetper to n2
         	compare   c0 to n2                    .net name order?
         	if        equal
                       	move      b1 to innets
         	else
         	              move      "*" to innets
         	endif
         	
                    COMPARE   C1 TO CMREFLAG
         	IF        EQUAL
                    MATCH     YES TO EXFEFLAG
                    	IF        EQUAL
                              		prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,*ulon,MLRN,*uloff,*boldoff;
;innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INNETS,*boldoff;                   
                              prtpage prfile;*ll,*boldon,LRN,*boldoff;             
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVNUM,*boldoff;                   
;RUNFLAG      
                              prtpage prfile;*ll,*boldon,RUNFLAG,*boldoff; 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILNAME,*boldoff;   
;GUARPAY       
                              prtpage prfile;*ll,*boldon," ",GUARPAY,*boldoff;                                          
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AR,*boldoff;    
;PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP1,*boldoff;                  
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$NINC,*boldoff;                 
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$LRINC,*boldoff;                
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$STAX,*boldoff;          
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$CTAX,*boldoff;          
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$POST,*boldoff;
                       add     eightlpi,row        
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff; 
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                   
                   
                       ELSE
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,MLRN,*boldoff;
;innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INNETS,*boldoff;                   
                              prtpage prfile;*boldon,LRN,*boldoff;                 
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVNUM,*boldoff;                   
;RUNFLAG      
                              prtpage prfile;*ll,*boldon,RUNFLAG,*boldoff; 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILNAME,*boldoff;   
;GUARPAY       
                              prtpage prfile;*ll,*boldon," ",GUARPAY,*boldoff;
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AR,*boldoff;    
;PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP1,*boldoff;                  
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$NINC,*boldoff;                 
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$LRINC,*boldoff;                
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$STAX,*boldoff;          
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$CTAX,*boldoff;          
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$POST,*boldoff;
                       add     eightlpi,row        
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,COBN,"-",BILLTN,*boldoff;                          
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,INVDTEM,"/",INVDTED,"/",INVDTEY,*boldoff;                 
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,*boldon,BILCOMP,*boldoff;           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,*boldon,M$AP2," ",PMASK,*boldoff;   
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                       ENDIF
         ELSE
                       MATCH     YES TO EXFEFLAG
                       IF        EQUAL
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,*ulon,MLRN,*uloff;
;innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,INNETS;                     
                              prtpage prfile;LRN;                   
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVNUM;                     
;RUNFLAG      
                              prtpage prfile;RUNFLAG;       
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILNAME;     
;GUARPAY       
                              prtpage prfile;" ",GUARPAY;                                 
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AR;      
;PPFLAG       
.                             prtpage prfile;*boldon,PPFLAG,*boldoff;      
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                                                                           
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP1;                    
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$NINC;                   
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$LRINC;                  
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$STAX;            
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$CTAX;            
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$POST;
                       add     eightlpi,row        
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;  
                       add     eightlpi,row        
                       add     eightlpi,row                                               
                       ELSE
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,MLRN;
;innets       
                              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,INNETS;                     
                              prtpage prfile;LRN;                   
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVNUM;                     
;RUNFLAG      
                              prtpage prfile;RUNFLAG;       
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILNAME;     
;GUARPAY       
                              prtpage prfile;" ",GUARPAY;                                 
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AR;      
;PPFLAG       
                              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,*boldon,PPFLAG,*boldoff;                              
.                             prtpage prfile;*boldon,PPFLAG,*boldoff;                     
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP1;                    
                              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$NINC;                   
                              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$LRINC;                  
                              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$STAX;            
                              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$CTAX;            
                              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$POST;
                       add     eightlpi,row        
                              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*ll,COBN,"-",BILLTN;                            
                              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,INVDTEM,"/",INVDTED,"/",INVDTEY;                   
                              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,BILCOMP;             
                              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*font=font8,*ll,M$AP2," ",PMASK;    
                       ENDIF
         ENDIF
         ADD       c2 TO LINES
         if        (tdmchrg = "Y" | runrar > 0)
         add        c2 to lines
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
         clear     tdmchrg      
         add        runrar to grunrar
         add        runrpass to grunrpass
         add        runrflat to grunrflat    
         add        runrlr to grunrlr
         endif
...........................................................................         
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal
        CALL      HEADER IF not less         


........................................
         cmatch    yes to ppsw
         if        equal                                           1
         move      lrn to nmoafld5                                 1
         rep       zfill in nmoafld5                               1
         move      c5 to  nmoapath                                 1   
         call      nmoakey                                         1
         if        over                                     2 
        add     eightlpi,row                                               
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal
        CALL      HEADER IF not less
        
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
         move      yes to moabadflag
         add       c2 to lines                                     2
         goto      moaexit                                         2  
                endif                                              2
moaloop  compare   "18" to reason                                  1 
                if        equal                                    2
        add     eightlpi,row                                               
              COMPARE  "9900" to ROW
        CALL      HEADER IF equal                                  2
        CALL      HEADER IF not less                               2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"MOA Prepayment ";
              prtpage prfile;*p3900:row,*ALIGNMENT=*Right,*ll,onamount;    
         add       onamount to totpmoa       .total prepay moa     2
         add       c2 to lines                                     2
         goto      moaexit                                         2
                endif                                              1
         call      nmoaks                                          2
                if        over                                     2
        add     eightlpi,row                                               
              COMPARE  "9900" to ROW
         CALL      HEADER IF equal                                 2
         CALL      HEADER IF not less                              2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"No MOA entry found for this invoice!!!!!!",*boldoff;
         move      yes to moabadflag
         add       c2 to lines                                     2
         goto      moaexit                                         2
                else                                                 
         goto      moaloop                                         2a 
                endif                                              2a
         endif                                                     1
         add       c1 to lines
moaexit  CLEAR     PMASK
              IF              (Formar <> HoldAr)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*p3900:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed Ar does not Match!!!!!!",HoldAr,*boldoff;
              endif
              IF              (ap <> HoldAp1)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*p3900:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP1 does not Match!!!!!!",HoldAP1,*boldoff;
              endif
              IF              (Formap2 <> HoldAp2)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*p3900:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP2 does not Match!!!!!!",HoldAP2,*boldoff;
              endif
              IF              (ap3 <> HoldAp3)
              add     eightlpi,row  
              COMPARE  "9900" to ROW
              CALL      HEADER IF equal                                 2
              CALL      HEADER IF not less                              2
              prtpage prfile;*p3900:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Computed AP3 does not Match!!!!!!",HoldAP3,*boldoff;
              endif
        add     eightlpi,row                                               
        add     eightlpi,row                                               

WRTSLS
         GOTO      ReadINv
*......................................................................
.
TOTAL    
              COMPARE  "6000" to ROW
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
.         
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"## INVOICES ",COUNT,*boldoff;
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Totals",*boldoff;                              
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff; 
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                            
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$CTAX,*boldoff;
        add     eightlpi,row           
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;               
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$NINC,*boldoff;                             
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$STAX,*boldoff;                             
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$POST,*boldoff;        
        add     eightlpi,row                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Code 96 ",*boldoff;                     
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$ARP,*boldoff;                              
        add     eightlpi,row                                
         MOVE      MASK92 TO MT$pmoa
         EDIT      TOTpmoa TO MT$pmoa
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Total MOA Debit",*boldoff;      
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$PMOA,*boldoff;                             
        add     eightlpi,row                 
         cmatch    yes to moabadflag
         if        equal
                      add     eightlpi,row                                 
                      add     eightlpi,row   
               prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"****MOA Prepayment entries are MISSING!!!!!!****",*boldoff;   
                      add     eightlpi,row                                 
                      add     eightlpi,row                                                        
         endif

         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      MASK92 TO MT$AP1
         EDIT      TOTAP TO MT$AP1
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL ACCOUNTS PAYABLE",*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;               
        add     eightlpi,row                 
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBRINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBR TO MT$AR
         EDIT      APBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/RENTAL",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBEINC TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBE TO MT$AR
         EDIT      APBE TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKERAGE/EXCHANGE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
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
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            Coldbr to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Rent",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            Coldbe to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New BROKERAGE/Exch",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              Move            Mask92 to Mt$lrinc
              edit            ColdLm to Mt$Lrinc
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New List management",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"New LR Income excluded in TOTALS",*boldoff;                    
        add     eightlpi,row  
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
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL MANAGEMENT EXCH FEE",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBR TO MT$AR
         EDIT      APBBR TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/RENT BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      LRBBE TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         EDIT      ARBBE TO MT$AR
         EDIT      APBBE TO MT$AP1
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"TOTAL BROKERAGE/EXCH BATCH Prev Months",*boldoff;                    
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;         
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/RENT ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         EDIT      NINBEINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL BROKER/EXCH ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
.
         MOVE      MASK92 TO MT$LRINC
         add       ninmeinc to ninmrinc
         EDIT      NINMRINC TO MT$LRINC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"TOTAL MANAGEMENT ACD",*boldoff;                    
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                           
        add     eightlpi,row  
        add     eightlpi,row         
        add     eightlpi,row         
.
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
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Triplex Breakout",*ULOFF,*boldoff;                    
        add     eightlpi,row  
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Running Charge Invoices","##",RUNCOUNT,*boldoff;                            
        add     eightlpi,row  
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;                                              
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                                          
        add     eightlpi,row                 
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"FLAT PASS THROUGH",*boldoff;                                                                  
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;                                                     
        add     eightlpi,row                 
        add     eightlpi,row                         
         MOVE      MASK92 TO MT$LRINC
         EDIT      GRUNRLR TO MT$LRINC
         MOVE      MASK92 TO MT$AP1
         MOVE      MASK92 TO MT$AR
         MOVE      MASK92 TO MT$AP2
         EDIT      GRUNRAR TO MT$AR
         EDIT      GRUNRPASS TO MT$AP1
         EDIT      GRUNRFLAT TO MT$AP2
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Rental TDMC & Prog 19","##",RUNRCNT,*boldoff; 
        add     eightlpi,row  
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AR,*boldoff;                                              
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP1,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$LRINC,*boldoff;                                                          
        add     eightlpi,row                 
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"FLAT PASS THROUGH",*boldoff;                                                                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$AP2,*boldoff;                                                     
        add     eightlpi,row  
        add     eightlpi,row          
.
         MOVE      MASK92 TO MT$TDMC
         EDIT      TOTTDMC TO MT$TDMC
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"TRIPLEX Billed us  Charges",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,MT$TDMC,*boldoff;                                                           
         GOTO      EOJ
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
              if not equal
                                             PRTPAGE prfile;*NEWPAGE:
                                             *UNITS=*HIENGLISH:
                        *ORIENT=*PORTRAIT:
                                             *Duplex=2                        
              endif
        MOVE      c6 TO LINES 
              clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
.        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"NIN Monthly Sales Register",*boldoff;  
        add     eightlpi,row        
        add     eightlpi,row        
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
              prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Accounts",*boldoff;                               
              prtpage prfile;*pTitle2:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Commissions",*boldoff;                                          
              prtpage prfile;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font8,*boldon,*ll,"Taxes",*boldoff;                                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,"Mailer Bill-To",*boldoff;                                        
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Mailer",*ULOFF,*boldoff;           
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"LR    ",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Invoice",*ULOFF,*boldoff;                
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Name and Thru",*ULOFF,*boldoff;                                        
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Receivable",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Payable",*ULOFF,*boldoff;                             
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"NIN Income",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"LR Income",*ULOFF,*boldoff;                           
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"State",*ULOFF,*boldoff;         
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"City",*ULOFF,*boldoff;          
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Postage",*ULOFF,*boldoff;                     
        add     eightlpi,row  
        add     eightlpi,row  

         RETURN
.
*........................
MISSING  
              COMPARE  "9900" to ROW

        CALL      HEADER IF equal
        CALL      HEADER IF not less
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"********* MISSING INVOICE ******",HOLDREC,*ULOFF,*boldoff;          
        add     eightlpi,row  
              clear str6
              move holdrec to str6
              append str6,taskname
              append crlf,taskname
         DISPLAY   *P10:13,"MISSING INVOICE: ",HOLDREC
         ADD       C1 TO HOLDREC
         add       c3 to lines
         GOTO      MISSCHK
swaptype
                 IF      (srcr = "M")
                 return
                 endif
.                 
                 match   "R" to typer
                 if      equal
                 pack    nrevfld from "EB","00",mlrn,"2005"
                 else
                 pack    nrevfld from "RB","00",mlrn,"2005"
                 endif
                 call    nrevkey
                 return
.
EOJ
              prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
              pack            APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
              loop
               call           FindFirstFile
               until (APIResult = 0 | APIResult = hexeight)
               pause          "1"
              repeat
              pause           "2"
              erase           "c:\progra~1\pdf995\flag.dat"                
         MOVE      NO to str1
fini
.........................................................................................................................
         SHUTDOWN  "cls"
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         SHUTDOWN  "cls"
         STOP

                              include        compio.inc
                              include        cntio.inc
         INCLUDE   NORDIO.INC
              include         compute.inc
              INCLUDE         ninvio.inc
              Include         NInvAcdio.inc
         include   nownio.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         include   cvt.inc
         INCLUDE   GNXTIO.inc
         include   nmoaio.inc
         include   nmrgio.inc
         include   nslsio.inc
         include   tinvio.inc
         include   nxcgio.inc
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
        include nrevio.inc
        include nprjio.inc
         INCLUDE    COMLOGIC.inc



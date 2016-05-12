...............................................................................
.NEOM0666
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   consacct.inc
         include   hp.inc
;begin patch 1.2         
         include   nshpdd.inc
         include   nacddd.inc
;         INC       NINVDD.inc
	Include	ninvdd.inc
	Include	NinvAcddd.inc
;end patch 1.2         
.START PATCH 1.1 REPLACED LOGIC
.         INC       NMLRDD.inc
.         INCLUDE   NBRKDD.INC
         INCLUDE   COMPDD.INC
	include	cntdd.inc
.END PATCH 1.1 REPLACED LOGIC
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.inc
         include   nowndd.inc
         include    tinvdd.inc
           INCLUDE   GNXTDD.inc
         INCLUDE   NDAT3DD.inc
         inc       nmrgdd.inc
         include   nslsdd.inc
         include   nmoadd.inc
          
.
...........................................
RELEASE  INIT      "1.2"       DLH 10march2005	Invoice CONVERSION   .this guy is missing some goodies to work ?
;RELEASE  INIT      "1.1"       ASH 26MAY2004	MAILER CONVERSION
.RELEASE  INIT      "1.0"       DLH11Sep97
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
.RECNUM   FILE      FIX=42
tdmcfILE  FILE
.
. 172279 FIRST INV# OF 1990
. 197750 FIRST INV# OF 1991
. 218003 FIRST INV# OF 1992
. 241530 FIRST INV# OF 1993
. 262880 FIRST INV# OF 1994
. 302647 First Inv# of 1996.
. 321449 First Inv# of 1997.
FIRSTN   INIT      "302647" 
DUPEOWN  IFILE     KEYLEN=4
OWNKEY   DIM       4    *DUPE OWNER FILE.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
form122  form      12.2
TDMCAMT  FORM       8.2
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
.
.END TDMC.
.
LRMRINC  FORM      9.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC  FORM      9.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC   FORM      9.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC  FORM      9.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC  FORM      9.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN   FORM      9.2      UNKNOWN LR INCOME.
LRBBE    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR    FORM      9.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
.
ARMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL  A/R
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
;FORM92   FORM      9.2
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
CO       FORM      1
BATCHBR  FORM      1       "0" =NO, "1" = YES.
RENTSW   FORM      1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR  FORM      2
SALESNUM DIM       2
TEAM1    INIT      "01"     SUSAN
TEAM2    INIT      "02"    ELAINE
TEAM3    INIT      "03"    LIST MANAGEMENT
.RUNCODES INIT      "005051-009766"
DIM9     DIM       9
dim8     dim       8
DIM7     DIM       7
AMOUNT1  DIM       10
FORM102  FORM      10.2
newfld   dim       10
MINUS    INIT      "-"
TOMOV    INIT      "0}1J2K3L4M5N6O7P8Q9R"
FORM8    FORM      8
FORM9    FORM      9
apsw     dim       1
. 
.
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
;MASK32   INIT      "ZZZ.ZZ-"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
;MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
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
lastinv  dim       6
mrgsw    dim       1
shipsw   dim       1 
.
         MOVE      "Names in the News Ca" TO COMPNME
         MOVE      "tinv0005" TO PROGRAM
         MOVE      "Triplex Reconciliation" TO STITLE
         OPEN          DUPEOWN,"DUPEOWN",READ
         PREPARE   tdmcFILE,"\\nins1\e\data\tdmcreco.tmp"
.       
CLOCK    CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO SYSMO,SYSDY,SYSYR
         PACK      DATEMASK FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
         MOVE      DATEMASK TO TODAY
         XIF
         IFZ       PC
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         XIF
         MOVE      C0 TO PAGE
         CALL      PAINT
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
.
DATE     MOVE      YES TO ANS
.         KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,ANS
.         CMATCH    YES TO ANS
.         GOTO      OPEN IF EQUAL
.         GOTO      DATE IF EOS
.         KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,SYSMO,"/":
.                   *DE,*JR,*ZF,*RV,SYSDY,"/",*DE,*JR,*ZF,*RV,SYSYR
.         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
.         MOVE      DATE TO DATEMASK
.         MOVE      DATEMASK TO TODAY
.         CALL      PAINT
.         CALL      FUNCDISP
.         GOTO      DATE
OPEN     TRAP      SHAREINV GIVING ERROR IF IO
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
.         DISPLAY   *P1:23,*EL,"OPENING NININV2 READ ONLY";
.         OPEN      NINVFILE,"NININV2",READ
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
.         OPEN      RECNUM,"INVNUM"
         TRAPCLR   IO
         GOTO      BEGIN
SHAREINV TRAPCLR   IO
         DISPLAY   *P1:23,*EL,"NININV2 READ ONLY FAILED, FILE SHARED";
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
.         OPEN      NINVFILE,"NININV2",SHARE
         GOTO      OPENREST
BEGIN
.          READTAB   RECNUM,C0;*TAB,BEGINV

.         CLOSE     RECNUM
         MOVE      firstn TO NINVFLD
          
.           MOVE      "NINVLAST" TO GNXTFLD
.           CALL      GNXTKEY
.           MOVE      GNXTNUM TO NINVFLD
.           MOVE      GNXTNUM TO HOLDREC
.           REP       ZFILL IN GNXTFLD
         CALL      NINVTST
.         MOVE      BEGINV TO HOLDREC
         ADD       C1 TO HOLDREC
.
INPUT
.    COMPARE   C0 TO PAGE
.         CALL      HEADER IF EQUAL
.
.
READINV  CALL      NINVKS
         GOTO      TOTAL IF OVER
         MOVE      B1 TO RUNFLAG
.         MATCH     INVDTEM TO SYSMO
.         GOTO      TOTAL IF NOT EQUAL
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT,b5,lrn,b1,invnum
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.
MISSCHK  MOVE      INVNUM TO NEXTREC          *CHECK
.         COMPARE   NEXTREC TO HOLDREC         * FOR MISSING
.         GOTO      MISSING IF NOT EQUAL        *INVOICE
.         ADD       C1 TO HOLDREC
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.
.calc on running charges for exchanges
.need to add rate changes and commission on regular orders
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
         MOVE       C0 TO FORM72
         MOVE       squant TO FORM82
         MOVE       FORM82 TO FORM72    
.pre 7/19/96
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90      = 2.34 per thousand to TDMC
         MULT       ".00156" BY FORM72          40%  COMMISSION ON 3.90  =1.56 per thousand to US
.pre 1/7/97
.         mult
.
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        FORM72 TO RUNLR         LR INC PORTION
         ADD        FORM72 TO FORM82        TOTAL RUNNING CHARGE
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
         MULT       ".01" BY FORM92
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
. ...
         move       lrn to tinvfld
         call       tinvkey
         if         not over
         move       tinvdolr to form122
         mult       ".01" by form122
         move        c0 to TDMCAMT
         add         form122 to TDMCAMT
distdmc  display    *P35:24,"Triplex Billed ",TDMCAMT:
                    *p50:12,"TDMC ## ",tinvinv    
         call       tinvks
         goto       tdmcexit if over           
         match      tinvfld to tinvlr
         goto       tdmcexit if not equal
         move       c0 to form122
         move       tinvdolr to form122
         mult       ".01" by form122
         add         form122 to TDMCAMT
         goto       distdmc
         endif
tdmcexit
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
PROCESS  MOVE      C0 TO FORM7
         MOVE      squant TO FORM7
.
         MOVE      PPM TO FORM72
         DIVIDE    HUND INTO FORM72
         MOVE      FORM72 TO FORM32
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
           CALL        NMRGKEY
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
.
MASKIT   MOVE      MASK72 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         ADD       PREPAY TO TOTARP      .TOTAL PREPAID ORDERS.
.
         MOVE      MASK72 TO M$AR
         EDIT      FORMAR TO M$AR
         ADD       FORMAR TO TOTAR
.
         MOVE      MASK72 TO M$AP1
         EDIT      AP TO M$AP1
         ADD       AP TO TOTAP1
.
         MOVE      MASK72 TO M$AP2
         EDIT      FORMAP2 TO M$AP2
         ADD       FORMAP2 TO TOTAP2
         COMPARE   C0 TO FORMAP2
         CALL      ZEROAP2 IF EQUAL
.
         MOVE      MASK72 TO M$LRINC
         EDIT      LRINC TO M$LRINC
         ADD       LRINC TO TOTLR
.
         MOVE      MASK72 TO M$NINC
         EDIT      NININC TO M$NINC
         ADD       NININC TO TOTNIN
.
         MOVE      MASK42 TO M$STAX
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
.  BATCH BILLING SECTION. 25MAR93
           PACK      MKEY FROM MLRN,COBN
           REP       ZFILL IN MKEY
           MOVE      C0 TO BATCHBR
.          DISPLAY   *P10:16,*EL,*P10:18,*EL 
           CALL      NMLRKEY
           CMATCH    "B" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
           CMATCH    "A" TO MCODE          .BATCH BILL ?
         goto      bdate if equal
         goto      slstype 
bdate    MOVE      C0 TO N2
         MOVE      OMDTEY TO N2
         MOVE      SYSYR TO N3
         SUB       N2 FROM N3
         COMPARE   C1 TO N3
         IF        EQUAL
           MOVE      C0 TO N2
           MOVE      OMDTEM TO N2 
           MOVE      SYSMO TO N3
.          DISPLAY   *P10:16,"BATCH BILL"
           SUB       N2 FROM N3
             COMPARE   "-11" TO N3           .MAILDATE FROM PREVIOUS YEAR?
             IF        EQUAL
             MOVE      C1 TO BATCHBR
.            DISPLAY   *P10:18,"MONTH HIT"
           goto      slstype
             ENDIF
           ENDIF
MONTHCK    MOVE      C0 TO N2
           MOVE      OMDTEM TO N2 
           MOVE      SYSMO TO N3
.          DISPLAY   *P10:16,"BATCH BILLER"
           SUB       N2 FROM N3
             COMPARE   C1 TO N3           .MAILDATE FROM LAST MONTH?
             IF        EQUAL
             MOVE      C1 TO BATCHBR
.            DISPLAY   *P10:18,"MONTH HIT"
             ENDIF
..............................................................................
slstype   BRANCH    SALESBR OF BROKER,BROKER,MANAGE
.         ADD       LRINC TO LRUNKN
.         ADD       FORMAR TO ARUNKN
.         ADD       AP TO APUNKN
.         ADD       FORMAP2 TO APUNKN
.         GOTO      PRINT
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
.         RETURN
.READMLR 
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
.  
*......................................................................
.
PRINT    COMPARE   "56" TO LINES
.         CALL      HEADER IF EQUAL
.         CALL      HEADER IF not less
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
         clear      str8
         pack       str8 from invdtem,invdted,cc,invdtey
         write      tdmcfile,seq;*cdfon,lrn,invnum,bilcomp,innets,runflag,str8,m$ar,M$ap1,m$ap2,m$lrinc:
                    runar,runlr,runpass,runflat,tdmcamt,o1des
          move     c0 to runar
          move     c0 to runlr
          move     c0 to runpass
          move     c0 to runflag
          move     c0 to tdmcamt
         GOTO      INPUT
*......................................................................
.
TOTAL    
         GOTO      EOJ
.............................................................................
*............................................................
.
CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
         RESET     MPCHARS
         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
         GOTO      CVTMP IF EQUAL                YES.
         RESET     CVTFLD                        NO.
         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
         RETURN    IF EQUAL                      ITS OK.
FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
         RETURN                                POP THE STACK.
CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
         RESET     CVTFLD
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
..........................
MISSING  
.        COMPARE   "56" TO LINES
.        CALL      HEADER IF equal
.        CALL      HEADER IF not less
.         PRINT     *1,"********* MISSING INVOICE ******  ",HOLDREC;
.         PRINT     *1,"********* MISSING INVOICE ******  ",HOLDREC,*l,*l:
.                   *FLUSH
.         DISPLAY   *P10:13,"MISSING INVOICE: ",HOLDREC
.         ADD       C1 TO HOLDREC
.         add       c3 to lines
         GOTO      MISSCHK
........................
.KMINUS - CONVERT TO MINUS OVERPUNCH. ENTER WITH AMOUNT1 EXIT AMOUNT1
KMINUS
         RESET     AMOUNT1
         MOVE      AMOUNT1 TO NEWFLD
         REP       "-0" IN NEWFLD
         ENDSET    NEWFLD
         REP       TOMOV IN NEWFLD         *MINUS OVERPUNCH CONVERT
         RESET     NEWFLD
         CLEAR     AMOUNT1
         MOVE      NEWFLD TO AMOUNT1
         RETURN
.
EOJ
.         splclose
         WEOF      tdmcFILE,SEQ
         CLOSE     tdmcfILE,EOFSIZE
.         MOVE      NO to str1
.         KEYIN     *P1:24,*EL,*B,*B,*B,"OK TO UPDATE NINVLAST NOW  ?",*RV,*t120,STR1;
.         CMATCH    YES TO STR1
.         IF         EQUAL
.         MOVE      "NINVNXT" TO GNXTFLD
.           CALL      GNXTKEY
.           MOVE      GNXTNUM TO lastinv
.         MOVE     "NINVLAST" TO GNXTFLD
.            CALL     GNXTKEY
.            MOVE     lastinv TO GNXTNUM
.            CALL     GNXTUPD
.         keyin   *p1:24,*el,"Log Number Then hit Enter  ",*p20:24,*DV,lastinv,*p27:24,str1
.         endif
         SHUTDOWN  "cls"
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         SHUTDOWN  "cls"
         STOP
.START PATCH 1.1 REPLACED LOGIC
.         INC       NMLRIO.inc
.         INCLUDE   NBRKIO.INC
         INCLUDE   COMPIO.INC
	include	cntio.inc
.END PATCH 1.1 REPLACED LOGIC
         INCLUDE   NORDIO.inc
;begin patch 1.2         
;         INCLUDE   NINVIO.inc
	Include	ninvio.inc
	Include	NInvAcdio.inc
;         include   compute.INC
         	include   	compute.inc
;end patch 1.2         
         include   nownio.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
           INCLUDE   GNXTIO.inc
          include  tinvio.inc
         include   nmoaio.inc
         include   nmrgio.inc
         include   nslsio.inc
          include  nshpio.inc
         include  nacdio.inc
        INCLUDE    COMLOGIC.inc


...............................................................................
.NEOM0020 -  commission register for epsilon
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   CONSacct.inc
         include   hp.inc
;begin patch 2.6
;         INC       NINVDD.inc
         	INC       	ninvdd.inc
	Include	Ninvacddd.inc
;end patch 2.6
.patch2.57
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.inc
.patch2.57
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   GNXTDD.inc
         INCLUDE   NDAT3DD.inc
.patch2.57
.         INCLUDE   NBRKDD.INC
.patch2.57
         include   nowndd.inc
.begin patch 2.4
         include   nmrgdd.inc
         include   ndatdd.inc
         include   nacddd.inc
         include   nshpdd.inc
mrgsw    dim        1
shipsw   dim        1
.end patch 2.4
.
...........................................
release  init      "2.61"         JD   27May2005  Do not include orders after 5/19/05
;release  	init      	"2.6"         DLH   02March2005  Invoice Conversion
;release  init      "2.58"         JD   04Jun2004  Use NINCLAST for starting inv #.
;release  init      "2.57"        DMB	26MAY2004	Mailer Conversion
.RELEASE  init      "2.56"             JD  04May04 changed font.
;RELEASE  INIT      "2.55"          JD  07FEB2003 using epsimlrs variable.
;RELEASE  INIT      "2.5"          ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "2.4"          DLH  27APR99 NININV Y2k
.RELEASE  INIT      "2.31"          JD  08mar99 added new client #4417.
.RELEASE  INIT      "2.3"          JD  04mar99 added new client #93.
.RELEASE  INIT      "2.3"         ASH 25JAN99 CONSACCT.INC VAR EXPANSION
.RELEASE  INIT      "2.2"          jd 30oct97 added dsprog.dat
.RELEASE  INIT      "2.1"          jd 22oct96 revised to use acs #24/#6319
.                                   LWV/VOLVO 2000.
.RELEASE  INIT      "2.0"           JD 19DEC95 READ BROKER FILE.
.RELEASE  INIT      "1.9"          jd 30nov95 revised to use acs #2702. live again
.RELEASE  INIT      "1.8"         DLH 14NOV94 SPLITS,
.RELEASE  INIT     "1.7"         JD  16aug94 modified for dawson/epsilon comm.
.RELEASE  INIT     "1.6"         JD  18JAN94 CORRECTED DATE CHECK ON BATCH BILL
.RELEASE  INIT     "1.5"         DLH 25MAR93 ACCRUAL OF BATCH BILLING.
.RELEASE  INIT      "1.4"        DLH 16FEB93 GNXTxx.INC
.RELEASE  INIT      "1.3"        DLH  30JUN92  BREAKOUT TDMC CHARGES.
.
.RELEASE  INIT      "1.2"       DLH  23MAR92  NORDXX, NINVXX, NBILXX
.RELEASE  INIT      "1.1"       DLH 01/31/92  COMBINED LM TOTALS &
.                              CHANGED ORDER TO MATCH OTHER EOM JOBS.
.RELEASE  INIT      "1.0"       91
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
detail   form      5
ANS      DIM       1
TYPIST   DIM       2
.
TOTAR    FORM      9.2
TOTAP1   FORM      9.2
TOTAP2   FORM      9.2
TOTAP    FORM      9.2
TOTNIN   FORM      9.2
TOTLR    FORM      9.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
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
ARBBE    FORM      9.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR    FORM      9.2      TOTAL BATCH BILL A/R RENT PORTION
.
APMR     FORM      9.2      TOTAL MANAGEMENT/RENTAL A/P
APME     FORM      9.2      TOTAL MANAGEMENT/EXCHANGE A/P
APM      FORM      9.2      TOTAL MANAGEMENT A/P
APBR     FORM      9.2      TOTAL BROKERAGE/RENTAL A/P
APBE     FORM      9.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN   FORM      9.2      UNKNOWN A/P.
APBBE    FORM      9.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR    FORM      9.2      TOTAL BATCH BILL A/P RENT PORTION
.
PMASK    DIM       1
commamt  FORM      7.2
.
CALCAMT  FORM      9.2
FORM2    FORM      2
FORM22   FORM      2.2
.begin patch 2.4
.FORM7    FORM      7
.end patch 2.4
.FORM72   FORM      7.2
FORM52   FORM      5.2
.FORM82   FORM      8.2
.START PATCH #2.3 - DUPLICATE VAR IN CONSACCT.INC
.FORM92   FORM      9.2
.END PATCH #2.3 - DUPLICATE VAR IN CONSACCT.INC
FORM11   FORM      11
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
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
argh94x  form      9.4
.RUNCODES INIT      "005051-009766"
.
.
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
.begin patch 2.4
.MASK32   INIT      "ZZZ.ZZ-"
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.end patch 2.4
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       13
M$AP2    DIM       13
M$STAX   DIM       8
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       15
M$NINC   DIM       13
M$GROSS  DIM       13
.
MT$AR    DIM       15
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
notecomm init      "Commission Already Taken !"
commnt   dim       30
.begin patch 2.4
.commish  init      "N"
.end patch 2.4
.
.         DISPLAY   *P1:1,*EF," MONTHLY INVOICE REGISTER PRINT PROGRAM"
         KEYIN     *CL
         MOVE      "NINCAL" TO COMPNME
         MOVE      "NEOM0020" TO PROGRAM
         MOVE      "MONTHLY EPSILON COMMISSION" TO STITLE
         IFZ       PC
.START PATCH 2.5 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\COMMREGM.lst"
         PACK      STR35,NTWKPATH1,"COMMREGM.lst"
         SPLOPEN   STR35
.END PATCH 2.5 REPLACED LOGIC
         XIF
         KEYIN     *CL
         TRAP      EXIT1 IF F5
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
CLOCK
         CLOCK     DATE TO today
DATE1    MOVE      today TO DATEMASK
         UNPACK    today INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         CALL      FUNCDISP
.
         DISPLAY   *P01:06,"Input File  :NININV ":
                   *P01:08,"Invoices Processed: ":
                   *P01:09,"Eom Date : "
.
OPEN     TRAP      SHAREINV GIVING ERROR IF IO
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
.         DISPLAY   *P1:23,*EL,"OPENING NININV2 READ ONLY";
.         OPEN      NINVFILE,"NININV2",READ
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
         move      c1 to ndatpath
OPENREST TRAP      IO GIVING ERROR IF IO
.         OPEN      RECNUM,"INVNUM"
         TRAPCLR   IO
         GOTO      BEGIN
SHAREINV TRAPCLR   IO
         DISPLAY   *P1:23,*EL,"NININV2 READ ONLY FAILED, FILE SHARED";
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
        MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
        move       c1 to nbrkpath
.         OPEN      NINVFILE,"NININV2",SHARE
         GOTO      OPENREST
BEGIN
.        READTAB   RECNUM,C0;*TAB,BEGINV
.         CLOSE     RECNUM
.         MOVE      BEGINV TO NINVFLD
.         MOVE      "NINVLAST" TO GNXTFLD
.Patch2.58
         MOVE      "NINCLAST" TO GNXTFLD
.Patch2.58
         CALL      GNXTKEY
         MOVE      GNXTNUM TO NINVFLD
         MOVE      GNXTNUM TO HOLDREC
         REP       ZFILL IN GNXTFLD
         CALL      NINVTST
.         MOVE      BEGINV TO HOLDREC
         ADD       C1 TO HOLDREC
.
INPUT    COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
.
.
READINV  CALL      NINVKS
         GOTO      TOTAL IF OVER
         MOVE      B1 TO RUNFLAG
         MATCH     INVDTEM TO SYSMO
         GOTO      TOTAL IF NOT EQUAL
         ADD       C1 TO COUNT
         DISPLAY   *P20:08,count:
                   *P15:09,today
.begin patch 2.4
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 2.4
.
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.
.start patch 2.61
.
         move      C5 to mm          .may 19
         move      "19" to dd         . last day
         move      "05" to yy         . be
         call      cvtjul                .fore
         move      juldays to n6         .bill new way
         move      invdtem to mm           .code
         move      invdted to dd           .check
         move      invdtey to yy           .it
         call      cvtjul                .out
         sub       juldays from n6       .what is diff?
         compare   c0 to n6
         if        not less              .do new way
         reset     epsimlrs
         match     "0192" to obrknum
         goto       readinv if not equal
         scan       mlrn in epsimlrs
         if         equal
;         match     "0192" to obrknum
;         goto       readinv if not equal
         reset     epsimlrs
         scan       omlrnum in epsimlrs
         goto       ok if equal
.         match     "0904" to omlrnum
.         goto       ok if  equal
.         match     "1762" to omlrnum
.         goto       ok if equal
.         match     "1604" to omlrnum
.         goto       ok if equal
.         match     "2702" to omlrnum
.         goto       ok if equal
.         match     "0024" to omlrnum
.         goto       ok if equal
.         match     "6319" to omlrnum
.         goto       ok if equal
.         match     "3083" to omlrnum
.         goto       ok if equal
.         match     "3852" to omlrnum
.         goto       ok if equal
.         match     "0093" to omlrnum
.         goto       ok if equal
.         match     "4417" to omlrnum
.         goto       ok if equal
         endif
			endif
.end patch 2.61
   goto       readinv
OK       CALL      READMLR
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         PACK      NBRKFLD FROM iBRKNUM,iBRKCNT
         CMATCH    B1 TO NBRKFLD
         goto      goon IF EOS
         call      nbrkkey
         IF        NOT OVER
         move      mcomp to bilCOMP
         move      BRCOMP to bilNAME
         ENDIF
.
goon    if         (commpct < "20")
        goto       readinv
        endif
.goon
.     match     "20" to commpct
.         goto      readinv if not equal
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
.START PATCH #2.3 - INCREASED VARS
.         MOVE       C0 TO FORM72
.         MOVE       QTYSHP TO FORM82
.         MOVE       FORM82 TO FORM72
.         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
.         MULT       ".00156" BY FORM72          40%  COMMISSION ON 3.90
.         ADD        FORM82 TO RUNPASS       TDMC PORTION
.         ADD        FORM72 TO RUNLR         LR INC PORTION
.         ADD        FORM72 TO FORM82        TOTAL RUNNING CHARGE
...
         MOVE       C0 TO CMPT92
.begin patch 2.4
.         MOVE       QTYSHP TO FORM82
         MOVE       QTYbild TO FORM82
.end patch 2.4
         MOVE       FORM82 TO CMPT92
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90
         ADD        FORM82 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORM82        TOTAL RUNNING CHARGE
.END PATCH #2.3 - INCREASED VARS
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
.begin patch 2.4
.         MULT       ".01" BY FORM92
.end patch 2.4
         ADD        FORM92 TO RUNAR
         SUB        FORM82 FROM FORM92      FIND FLAT FEE PORTION
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
.
.
PROCESS
.begin patch 2.4
.          MOVE      C0 TO FORM7

.         MOVE      QTYSHP TO FORM7
.end patch 2.4
.
.START PATCH #2.3 - INCREASED VARS
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
         MOVE      PPM TO CMPT92
.begin patch 2.4
.         DIVIDE    HUND INTO CMPT92
.end patch 2.4
         MOVE      CMPT92 TO FORM32
.END PATCH #2.3 - INCREASED VARS
.
.
.         CALL      READBILL
.
.
.begin patch 2.4
         move      lrn to nshpfld
         move      no to shipsw
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         move      olnum to ndatfld
         call      ndatkey
         MOVE      LRN TO NmrgFLD
         REP       ZFILL IN NmrgFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet


         move      no to mrgsw
         CALL      NMRGKEY
         if       not over
         move      yes to mrgsw
         endif
         call      wipecvars
.end patch 2.4
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
.
.         move      lrinc to calcamt
.         if        (commpct >= "20" )
.         move      commpct to form74
.         mult      ".01" by form74
.         divide    form74 into calcamt
.         mult      ".01" by calcamt            .calculated epsilon com
.         else
.         move      c0 to calcamt
.         endif
.
MASKIT
         cmatch    yes to ans
.         if        equal
.         move      amount to calcamt
.         div       calcamt by c2
         if        (commpct >= "20" )
         move      c0 to calcamt
         move      gross to calcamt
         mult      ".10" by calcamt
         else
         move      c0 to calcamt
         endif
         MOVE      MASK92 TO M$LRINC
         EDIT      calcamt TO M$LRINC
.begin patch 2.4
         if        (nacd1flag = 1)
         move      notecomm to commnt
         else
         clear     commnt
         ADD       calcamt TO TOTLR
         endif
.         cmatch    no to commish
.         if        equal
.         ADD       calcamt TO TOTLR
.         endif
         goto      print
.end patch 2.4
.         goto      input
.
.READBIL
READBILL
.
         PACK      NBILFLD FROM MLRN,COBN,BILLTN
         CALL      NBILKEY
         CALL      READMLR IF OVER
         RETURN
.READMLR
READMLR  PACK      MKEY FROM MLRN,COBN
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
.         MOVE      MCCTO TO BILNAME
         RETURN
.
.begin patch 2.4
.COMPUTE
.         MOVE      C0 TO SELECT
.         MOVE      C0 TO SHIP
.         CLEAR     PREPAYSW
.         CLEAR     PPSW
.         MOVE      C0 TO FORMAP2
.         MOVE      C0 TO SAVEAP
.         MOVE      C0 TO TAXES
.         MOVE      C0 TO BRKCOM
.         MOVE      C0 TO CMPCOM
.         MOVE      C0 TO LRINC
.         MOVE      C0 TO AP
.         MOVE      C0 TO SVECOM
.         MOVE      C0 TO PREPAY
.         MOVE      C0 TO POST
.         MOVE      C0 TO GROSS
.         MOVE      C0 TO FORMAR
.         MOVE      C0 TO PRICE
.         MOVE      C0 TO PRICEx
.         MOVE      C0 TO AMOUNT
.         MOVE      C0 TO AMOUNTx
.         MOVE      C0 TO SVEACR
..START PATCH #2.3 - INCREASED VARS
..         MOVE      C0 TO FORM72
.         MOVE      C0 TO CMPT92
..END PATCH #2.3 - INCREASED VARS
.         MOVE      C0 TO FORM72x
.         MOVE      C0 TO FORM73
..START PATCH #2.3 - INCREASED VARS
..         MOVE      C0 TO FORM74
..         MOVE      C0 TO FORM74x
.         MOVE      C0 TO CMPT94
.         MOVE      C0 TO CMPT94x
..END PATCH #2.3 - INCREASED VARS
.         MOVE      C0 TO FORM32
..
..START PATCH #2.3 - INCREASED VARS
..         MOVE      QTYSHP TO FORM74
..         COMPARE   C0 TO FORM74
..         GOTO      FNINCD IF EQUAL
..         DIVIDE    THOUS INTO FORM74
..         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
...
..         MOVE      iRexqty TO FORM74x              .split qty
..         DIVIDE    THOUS INTO FORM74x
..         MOVE      FORM74x TO AMOUNTX            .split QUANTITY BILLED
...
..         MOVE      C0 TO FORM72
..         MOVE      PPM TO FORM72
..         COMPARE   C0 TO FORM72
..         GOTO      FNINCD IF EQUAL
..         DIVIDE    HUND INTO FORM72
..         MOVE      FORM72 TO PRICE               PRICE PER M
.....
.         MOVE      QTYSHP TO CMPT94
.         COMPARE   C0 TO CMPT94
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    THOUS INTO CMPT94
.         MOVE      CMPT94 TO AMOUNT              QUANTITY BILLED
..
.         MOVE      iRexqty TO CMPT94x              .split qty
.         DIVIDE    THOUS INTO CMPT94x
.         MOVE      CMPT94x TO AMOUNTX            .split QUANTITY BILLED
..
.         MOVE      C0 TO CMPT92
.         MOVE      PPM TO CMPT92
.         COMPARE   C0 TO CMPT92
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO PRICE               PRICE PER M
..END PATCH #2.3 - INCREASED VARS
..
.         MOVE      C0 TO FORM72x                  exchange
.         MOVE      iexppm TO FORM72x              portion
.         DIVIDE    HUND INTO FORM72x              of the
.         MOVE      FORM72x TO PRICEX              split PRICE PER M
..
..START PATCH #2.3 - INCREASED VARS
..         MULT      PRICE BY FORM74
..         MOVE      FORM74 TO GROSS               GROSS BILLING.
..         move      form74 to calcamt
..         MOVE      FORM74 TO FORMAR                  ACCOUNTS RECEIVABLE.
..         MOVE      FORM74 TO SVEACR              WORKING AR.
...
..         MULT      PRICEx BY FORM74x             .split portion
..         add       FORM74x TO GROSS               GROSS BILLING.
..         add       FORM74x TO SVEACR              WORKING AR.
.....
.         MULT      PRICE BY CMPT94
.         MOVE      CMPT94 TO GROSS               GROSS BILLING.
.         move      CMPT94 to calcamt
.         MOVE      CMPT94 TO FORMAR                  ACCOUNTS RECEIVABLE.
.         MOVE      CMPT94 TO SVEACR              WORKING AR.
..
.         MULT      PRICEx BY CMPT94x             .split portion
.         add       CMPT94x TO GROSS               GROSS BILLING.
.         add       CMPT94x TO SVEACR              WORKING AR.
..END PATCH #2.3 - INCREASED VARS
..
.FNINCD   MOVE      C0 TO N1                      CLEAR BRANCH VAR.
.         MOVE      PAYCODE TO N1                     PAYABLE CODE
.         BRANCH    N1 TO CHKCHRGS,FNINCD2,CHKCHRGS,FNINCD4
.FNINCD2  MOVE      C0 TO FORM32
.         MOVE      COMMPCT TO FORM32
.         COMPARE   c0 TO FORM32                    C0 COMMISSION?
.         GOTO      CHKCHRGS IF EQUAL
.         DIVIDE    HUND INTO FORM32
.         MOVE      FORMAR TO LRINC
.         MULT      FORM32 BY LRINC               LR INCOME.
.         MOVE      LRINC TO CMPCOM
.         GOTO      CHKCHRGS
.FNINCD4  MOVE      FORMAR TO LRINC
..
.CHKCHRGS MOVE      c0 TO INDEX
.NEXTCHRG ADD       c1 TO INDEX
.         CLEAR     PREPAYSW
.         COMPARE   "11" TO INDEX
.         goto      endchrgs if equal
.         LOAD      STR14 FROM INDEX OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
.                   ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10
.         UNPACK    STR14 INTO ADDCODE,STR12
.         UNPACK    STR12 TO STR7,AEXTCD,STR3,STR1
.         MATCH     "  " TO ADDCODE
.         goto      endchrgs if equal
..boogie for this program only
.         match     "01" to addcode
.         if         equal
.         move      yes to commish
.         move      notecomm to commnt
.          endif
.         MOVE      ADDCODE TO ADDKEY
..
..
.         MOVE      C0 TO ACAMT
.         MOVE      C0 TO ACCMPR
.         MOVE      C0 TO ANINCD
..
..START PATCH #2.3 - INCREASED VAR
..         MOVE      STR7 TO FORM72
..         DIVIDE    HUND BY FORM72
..         MOVE      FORM72 TO ACAMT
.         MOVE      STR7 TO CMPT92
.         DIVIDE    HUND BY CMPT92
.         MOVE      CMPT92 TO ACAMT
..END PATCH #2.3 - INCREASED VAR
..
.         MOVE      STR3 TO FORM32
.         DIVIDE    HUND BY FORM32
.         MOVE      C0 TO ACCMPR
.         MOVE      FORM32 TO ACCMPR
..
.         MOVE      C0 TO ANINCD             CLEAR BRANCH
.         MOVE      STR1 TO ANINCD
..
.         MOVE      ADDCODE TO CODENUM
.         BRANCH    CODENUM TO CD01:      01    BROKER COMMISION
.                              CD02:      02    BROKER COMMISSION
.                              CD03:      03    Broker commssion   5/94
.                              CD04:      04    TAXES
.                              CD05:      05    TAXES
.                              CD06:      06    TAXES
.                              CD07:      07    TAXES
.                              CD08:      08    TAXES
.                              CD09:      09    TAXES
.                              CD10:      10    TAXES
.                              CD11:      11    TAXES
.                              SLCTPERM:  12    INDUSTRY SELECT (NY=BROKER COMM)
.                              SLCTPERM:  13    SELECTIONS
.                              SLCTPERM:  14    SELECTIONS
.                              SLCTPERM:  15    SELECTIONS
.                              SLCTPERM:  16    SELECTIONS
.                              SLCTPERM:  17    SELECTIONS
.                              SLCTPERM:  18    SELECTIONS
.                              SLCTPERM:  19    SELECTIONS
.                              SLCTPERM:  20    SELECTIONS
.                              SLCTPERM:  21    SELECTIONS
.                              SLCTPERM:  22    SELECTIONS
.                              SLCTPERM:  23    SELECTIONS
.                              SLCTPERM:  24    SELECTIONS
.                              SLCTPERM:  25    SELECTIONS
.                              SLCTPERM:  26    SELECTIONS
.                              SLCTPERM:  27    SELECTIONS
.                              SLCTPERM:  28    SELECTIONS
.                              SLCTPERM:  29    SELECTIONS
.                              SLCTPERM:  30    SELECTIONS
.                              SLCTPERM:  31    SELECTIONS
.                              SLCTPERM:  32    SELECTIONS
.                              SLCTPERM:  33    SELECTIONS
.                              SLCTPERM:  34    SELECTIONS
.                              SLCTPERM:  35    SELECTIONS
.                              SLCTPERM:  36    SELECTIONS
.                              SLCTPERM:  37    SELECTIONS
.                              SLCTPERM:  38    SELECTIONS
.                              SLCTPERM:  39    SELECTIONS
.                              SLCTPERM:  40    SELECTIONS
.                              SLCTPERM:  41    SELECTIONS
.                              SLCTPERM:  42    SELECTIONS
.                              SLCTPERM:  43    SELECTIONS
.                              SLCTPERM:  44    SELECTIONS
.                              SLCTPERM:  45    SELECTIONS
.                              SLCTPERM:  46    SELECTIONS
.                              SHIPPERM:  47    SHIPPING
.                              SHIPPERM:  48    SHIPPING
.                              SLCTPERM:  49    SELECTIONS
.                              SLCTPERM:  50    SELECTIONS
.                              SLCTFLAT:  51    MIN FLAT. (NY=GROSS BILLING)
.                              SLCTFLAT:  52    SELECTIONS
.                              SLCTFLAT:  53    SELECTIONS
.                              SHIPFLAT:  54    SHIPPING
.                              SLCTFLAT:  55    SELECTIONS
.                              SLCTFLAT:  56    SELECTIONS
.                              SLCTFLAT:  57    SELECTIONS
.                              TAXFLAT:   58    TAXES
.                              SLCTFLAT:  59    SELECTIONS
.                              SHIPFLAT:  60    SHIPPING
.                              SLCTFLAT:  61    SELECTIONS
.                              SHIPFLAT:  62    SHIPPING
.                              SLCTFLAT:  63    SELECTIONS
.                              SLCTFLAT:  64    SELECTIONS
.                              SLCTFLAT:  65    SELECTIONS
.                              SLCTFLAT:  66    SELECTIONS
.                              SLCTFLAT:  67    EXCHANGE FEE
.                              SLCTFLAT:  68    SELECTIONS
.                              SHIPFLAT:  69    SHIPPING
.                              SLCTFLAT:  70    SELECTIONS
.                              SHIPFLAT:  71    SHIPPING
.                              CD72:      72    OUR POSTAGE
.                              SHIPFLAT:  73    SHIPPING
.                              SHIPFLAT:  74    SHIPPING
.                              SHIPFLAT:  75    SHIPPING
.                              SHIPFLAT:  76    SHIPPING
.                              SHIPFLAT:  77    SHIPPING
.                              SHIPFLAT:  78    SHIPPING
.                              SHIPFLAT:  79    SHIPPING
.                              SHIPFLAT:  80    SHIPPING
.                              SLCTFLAT:  81    SELECTIONS
.                              SHIPFLAT:  82    SHIPPING
.                              SHIPFLAT:  83    SHIPPING
.                              SLCTFLAT:  84    SELECTIONS
.                              SLCTFLAT:  85    SELECTIONS
.                              SLCTFLAT:  86    SELECTIONS
.                              SLCTFLAT:  87    SELECTIONS
.                              SLCTFLAT:  88    SELECTIONS
.                              SLCTFLAT:  89    SELECTIONS
.                              SLCTFLAT:  90    SELECTIONS
.                              SLCTFLAT:  91    SELECTIONS
.                              TAXFLAT:   92    TAXES
.                              TAXFLAT:   93    TAXES
.                              TAXFLAT:   94    TAXES
.                              CD95:      95    BROKER COMMISSION
.                              CD96:      96    PRE-PAYMENTS
.                              CD97:      97    BROKER COMMISSION
.                              SLCTFLAT:  98    SELECTIONS
.                              CREDIT     99    CREDIT
.         GOTO      CD00
..
.SLCTPERM
..START PATCH #2.3 - INCREASED VAR
..         MOVE      QTYSHP TO FORM74
..         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
...
.         MOVE      QTYSHP TO CMPT94
.         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
..END PATCH #2.3 - INCREASED VAR
.         ADD       AMOUNT TO SELECT
..lets handle a split
.         CMATCH    YES TO MCOPIES                .REGIONAL
.         GOTO      SLCTPRMX IF NOT EQUAL         .NO
.         RESET     FULHOUSE
.         SCAN      OWNCTN IN FULHOUSE
.         if        equal                         .yes, lets check for affected selects.
.         compare   "15" to codenum               .sex select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "17" to codenum               .key  select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "19" to codenum               .split  select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "34" to codenum               .recency  select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "42" to codenum               .ps labels  select?
.         goto       slctprmx if equal            .yes, calc it
.         endif
.         goto      sumamt               .none of the above - exit.
.slctprmx
.         RESET     FULHOUSE
.         SCAN      OWNCTN IN FULHOUSE
.         if        equal                         .yes, lets check for affected selects.
..START PATCH #2.3 - INCREASED VAR
..         move      c0 to form74x
..         MOVE      iRexqty TO FORM74x
..         DIVIDE    THOUS BY FORM74x
..         MULTIPLY  ACAMT BY FORM74x
..         add       FORM74x TO AMOUNT
..         ADD       form74x TO SELECT
...
.         move      c0 to CMPT94x
.         MOVE      iRexqty TO CMPT94x
.         DIVIDE    THOUS BY CMPT94x
.         MULTIPLY  ACAMT BY CMPT94x
.         add       CMPT94x TO AMOUNT
.         ADD       CMPT94x TO SELECT
..END PATCH #2.3 - INCREASED VAR
.         endif
.         GOTO      SUMAMT
................................................................................
..
.SHIPPERM
..START PATCH #2.3 - INCREASED VAR
..         MOVE      QTYSHP TO FORM74
..         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
..         ADD       AMOUNT TO SHIP
...lets handle a split
..         MOVE      iRexqty TO FORM74x
..         DIVIDE    THOUS BY FORM74x
..         MULTIPLY  ACAMT BY FORM74x
..         add       FORM74x TO AMOUNT
..         ADD       form74x TO ship
....
.         MOVE      QTYSHP TO CMPT94
.         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
.         ADD       AMOUNT TO SHIP
..lets handle a split
.         MOVE      iRexqty TO CMPT94x
.         DIVIDE    THOUS BY CMPT94x
.         MULTIPLY  ACAMT BY CMPT94x
.         add       CMPT94x TO AMOUNT
.         ADD       CMPT94x TO ship
..END PATCH #2.3 - INCREASED VAR
..
.         GOTO      SUMAMT
................................................................................
.SLCTFLAT MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SELECT
.         GOTO      SUMAMT
..
.SHIPFLAT MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SHIP
.         GOTO      SUMAMT
..
.TAXFLAT  MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CREDIT
.         MOVE      ACAMT TO AMOUNT
.         MULT      SEQ BY AMOUNT
.         GOTO      SUMAMT
..
..
.. ACAMT IS NOT IN CORRECT FORM, IT SHOULD HAVE 3 DEC PLACES.
.. USE ITS ALPHA EQUIVLENT FROM THE UNPACK ABOVE.
..
.CD00     MATCH     "0331" TO MLRN
.         GOTO      CD00CONT IF NOT EQUAL
.         PACK      STR4 WITH INVDTEY,INVDTEM
.         MATCH     "8601" TO STR4
.         GOTO      NEXTCHRG IF EQUAL
.CD00CONT MOVE      STR7 TO FORM73
.         DIVIDE    THOUS BY FORM73
.         MULTIPLY  PRICE BY FORM73
.         MULTIPLY  ACCMPR BY FORM73
.         MULTIPLY  "-1" BY FORM73
.         MOVE      FORM73 TO AMOUNT
.         ADD       AMOUNT TO FORMAR
.         ADD       AMOUNT TO LRINC
.         GOTO      ADDAMT
..
.CD01     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".10" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD02     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".20" BY AMOUNT
.         MULTIPLY  ".05" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD03     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".20" BY AMOUNT
.         ADD       AMOUNT TO brkcom
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.
.         GOTO      SUMAMT
..
..CD04     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".0400" BY AMOUNT
..         ADD       AMOUNT TO TAXES
..         GOTO      SUMAMT
..
.CD04     MOVE      SVEACR TO AMOUNT
.         MOVE      AMOUNT TO CANUSE        *CANADIAN USE TAX
.         SUB       LRINC FROM CANUSE
.         MULT      ".10" BY CANUSE
.         MULT      "-1" BY CANUSE
.         MOVE      CANUSE TO AMOUNT
.         GOTO      SUMAMT
..
.CD05     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0500" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD06     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0600" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD07     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0800" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD08     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0700" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD09     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0625" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD10     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0725" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD11     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0825" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD12     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".15" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD51     MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO GROSS
.         GOTO      SUMAMT
..
.CD72     MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SHIP
.         ADD       AMOUNT TO FLOAMT
.         ADD       AMOUNT TO POST
.         GOTO      SUMAMT
..
.CD95     ADD       ACAMT TO BRKCOM
.         MOVE      ACAMT TO AMOUNT
.         MULTIPLY  "-1" BY AMOUNT
.         GOTO      SUMAMT
..
.CD96     MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO PREPAY
.         MOVE      "Y" TO PREPAYSW
.         MOVE      YES TO PPSW
.         GOTO      NEXTCHRG
..         GOTO      SUMAMT
..
.CD97     ADD       ACAMT TO BRKCOM
.         MOVE      ACAMT TO AMOUNT
.         MULTIPLY  "-1" BY AMOUNT
.         GOTO      SUMAMT
..
.SUMAMT   MOVE      AMOUNT TO SVECOM
.         COMPARE   "0" TO ACCMPR
.         GOTO      ADDAMT IF EQUAL
.         MULTIPLY  ACCMPR BY SVECOM
.         ADD       SVECOM TO LRINC
..START PATCH #2.3 - INCREASED VARS
..         MOVE      AMOUNT TO FORM72
..         SUB       SVECOM FROM FORM72
..         MOVE      FORM72 TO SVECOM
.         MOVE      AMOUNT TO CMPT92
.         SUB       SVECOM FROM CMPT92
.         MOVE      CMPT92 TO SVECOM
..END PATCH #2.3 - INCREASED VARS
.******
..
.ADDAMT   BRANCH    ANINCD TO ANINCD1,ANINCD2
..ANINCD0 - ADD. CHG CODE = ' ', ADD. CHG. TO AR.
.ANINCD0  ADD       AMOUNT TO FORMAR
.         GOTO      NEXTCHRG
..ANINCD1 - ADD. CHG. CODE - '1', ADD. CHG. TO A/P
.ANINCD1  CMATCH    "2" TO PAYCODE
.         GOTO      ADDCOM IF EQUAL
.         ADD       AMOUNT TO AP
.         GOTO      NEXTCHRG
.ADDCOM   ADD       SVECOM TO AP
.         GOTO      NEXTCHRG
.ANINCD2  ADD       AMOUNT TO FORMAR
.         GOTO      ANINCD1
..
..
.ENDCHRGS MOVE      C0 TO N1              CLEAR BRANCH VAR.
.         MOVE      PAYCODE TO N1
.         BRANCH    N1 TO PAYCD1,PAYCD2,PAYCD3
..PAYCD0 - PAYCODE = '0' OR ' ', SO GROSS=A/P
.PAYCD0   MOVE      FORMAR TO AP
.         SUBTRACT  LRINC FROM AP
.         SUBTRACT  TAXES FROM AP
..         SUBTRACT  PREPAY FROM FORMAR
..START PATCH #2.3 - INCREASED VARS
..         add       form74x to lrinc
..         add       form74x to formar
.         add       CMPT94x to lrinc
.         add       CMPT94x to formar
..END PATCH #2.3 - INCREASED VARS
.         RETURN
..PAYCD1 - PAYCODE = '1', NININCOME = GROSS - A/P - TAXES.
.PAYCD1   MOVE      FORMAR TO NININC
.         SUBTRACT  AP FROM NININC
.         SUBTRACT  TAXES FROM NININC
.         SUBTRACT  POST FROM NININC
..         SUBTRACT  PREPAY FROM FORMAR
..START PATCH #2.3 - INCREASED VARS
..         add       form74x to lrinc
..         add       form74x to formar
.         add       CMPT94x to lrinc
.         add       CMPT94x to formar
..END PATCH #2.3 - INCREASED VARS
.         RETURN
..PAYCD2 - PAYCODE = '2', 2- A/P'S,
..         A/P'S AS DEFINED IN INVOICE RECORD.
.PAYCD2
..START PATCH #2.3 - INCREASED VARS
..         MOVE      C0 TO FORM72
..         MOVE      AP1 TO FORM72
..         DIV       HUND INTO FORM72
..         MOVE      FORM72 TO SAVEAP
..         MOVE      SAVEAP TO AP
..         MOVE      FORMAR TO FORM72
..         SUBTRACT  AP FROM FORM72
..         MOVE      AP2 TO FORMAP2
..         DIV       HUND INTO FORMAP2
..         SUB       FORMAP2 FROM FORM72
...         SUBTRACT  PREPAY FROM FORMAR
..         MOVE      FORM72 TO LRINC
..         SUB       TAXES FROM LRINC
..         SUB       POST FROM LRINC
..         add       form74x to lrinc
..         add       form74x to formar
....
.         MOVE      C0 TO CMPT92
.         MOVE      AP1 TO CMPT92
.         DIV       HUND INTO CMPT92
.         MOVE      CMPT92 TO SAVEAP
.         MOVE      SAVEAP TO AP
.         MOVE      FORMAR TO CMPT92
.         SUBTRACT  AP FROM CMPT92
.         MOVE      AP2 TO FORMAP2
.         DIV       HUND INTO FORMAP2
.         SUB       FORMAP2 FROM CMPT92
..         SUBTRACT  PREPAY FROM FORMAR
.         MOVE      CMPT92 TO LRINC
.         SUB       TAXES FROM LRINC
.         SUB       POST FROM LRINC
.         add       CMPT94x to lrinc
.         add       CMPT94x to formar
..END PATCH #2.3 - INCREASED VARS
.         RETURN
.PAYCD3
..START PATCH #2.3 - INCREASED VARS
..         MOVE      C0 TO FORM72
..         MOVE      AP1 TO FORM72
..         DIVIDE    HUND INTO FORM72
..         MOVE      C0 TO AP
..         MOVE      FORM72 TO AP
..         MOVE      FORMAR TO LRINC
..         SUBTRACT  AP FROM LRINC
...         MOVE      AR TO CMPCOM
...        SUBTRACT  FORM72 FROM CMPCOM
..         SUBTRACT  TAXES FROM LRINC
..         SUBTRACT  POST FROM LRINC
...        SUBTRACT  PREPAY FROM FORMAR
..         add       form74x to lrinc
..         add       form74x to formar
....
.         MOVE      C0 TO CMPT92
.         MOVE      AP1 TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      C0 TO AP
.         MOVE      CMPT92 TO AP
.         MOVE      FORMAR TO LRINC
.         SUBTRACT  AP FROM LRINC
..         MOVE      AR TO CMPCOM
..        SUBTRACT  CMPT92 FROM CMPCOM
.         SUBTRACT  TAXES FROM LRINC
.         SUBTRACT  POST FROM LRINC
..        SUBTRACT  PREPAY FROM FORMAR
.         add       CMPT94x to lrinc
.         add       CMPT94x to formar
..END PATCH #2.3 - INCREASED VARS
.         RETURN
..
.end patch 2.4
*......................................................................
PRINT    COMPARE   "59" TO LINES
         CALL      HEADER IF not less
         PRINT     *2,MLRN,*9,LRN,*18,INVNUM,RUNFLAG:
                  *26,BILNAME," ",GUARPAY,*50,M$lrinc,b1,commnt:
                   *L,*1,COBN,"-",BILLTN:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMp,*l
         ADD       c3 TO LINES
         add       c1 to detail
         CLEAR     PMASK
         clear     commnt
         CLEAR     BILNAME
         CLEAR     BILcomp
.begin patch 2.4
.         move      no to commish
.end patch 2.4
         move      no to ans
         GOTO      INPUT
*......................................................................
.
TOTAL    COMPARE   "59" TO LINES
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$lrinc
         EDIT      TOTlr TO MT$lrinc
         PRINT     *1,"## INVOICES ",detail:
                   *26,"*** TOTALS",*48,MT$lrinc
.
         PRINT     *FLUSH
         GOTO      EOJ
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
.         print      hp17ptch,hpdupl,*f,*f          .compressed, duplex
.begin patch 2.56
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
.end patch 2.56
         PRINT     *31,"***  N I N   M O N T H L Y   ":
                   *60,"COMM   R E G I S T E R  ***":
                   *116,"DATE ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"MAILER",*11,"LR":
                   *17,"INVOICE":
                   *26,"MAILER BILL-TO":
                   *52,"-------COMMISSIONS------":
                   *L,*1,"NUMBER",*9,"NUMBER":
                   *18,"NUMBER",*26,"NAME AND THRU",*l
         MOVE      c6 TO LINES
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
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
........................
.
EOJ
         display   *p2:23,*blinkon,"Please wait I'm PRINTING !!!!!",*blinkoff
.         execute "f:\public\NPRINT g:\DATA\commregm.LST Q=LASER2 NT NB f=0 S=Nts0_fpnw"
         DISPLAY   *P1:4,*Ef,*P10:12,"JOB DONE!!!!!!",*W2
EXIT1
         shutdown  "cls"
         STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
.patch2.57
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch2.57
         INCLUDE   NORDIO.inc
 ;begin patch 2.6
;        INCLUDE   NINVIO.inc
;         include   compute.inc
        	INCLUDE   	ninvio.inc
	Include	NInvAcdio.inc
         	include   	compute.inc
 ;end patch 2.6
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         INCLUDE   GNXTIO.inc
.patch2.57
.         INCLUDE   NBRKIO.INC
.patch2.57
.begin patch 2.4
         include   ndatio.inc
         include   nacdio.inc
         include   nmrgio.inc
         include   nshpio.inc
.end patch 2.4
         INCLUDE    COMLOGIC.inc


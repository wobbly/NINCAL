...............................................................................
.NEOM0022 -
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   hp.inc
;begin patch 2.0
;         INC       NINVDD.inc
         	INC       	ninvdd.inc
	Include	NInvAcddd.inc
;end patch 2.0
.patch1.92
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.INC
.patch1.92
.begin patch 1.9
         include   consacct.inc
         include   nadjdd.inc
         include   nshpdd.inc
         include   nmrgdd.inc
         include   nacddd.inc
         include   nowndd.inc
mrgsw    dim       1
shipsw   dim       1
.end patch 1.9
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   GNXTDD.inc
         include   ndatdd.inc
         INCLUDE   NDAT3DD.inc
.
...........................................
.test for epsilon - pull all spec olympics/Farber/Natl ms 
.use invrep01 as input sorted by mlr(3-6),invdate(97-102)
release  	init      	"2.0"        DLH 02March2005	Invoice Conversion
;release  init      "1.93"        DLH	08June2004	Datacard Conversion Cleanup
;release  init      "1.92"        DMB	26MAY2004	Mailer Conversion
.RELEASE  INIT      "1.91"         ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.9"         DLH  23Aug99 NINadj nadjust  Y2k
.RELEASE  INIT     "1.8"         DLH  27APR99 NININV Y2k
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
.begin patch 1.9
.ADDKEY   DIM       2
.THOUS    FORM      "1000"
.HUND     FORM      "100"
.INDEX    FORM      2
.AMOUNT   FORM      7.4
.GROSS    FORM      7.2
.CANUSE   FORM      7.4            CANADIAN USE TAX
.SHIP     FORM      7.2
.POST     FORM      3.2
..SELECT   FORM      7.2
.TAXES    FORM      7.2
.PRICE    FORM      3.2
.FORMAR   FORM      7.2
.AP       FORM      7.2
.FORMAP2  FORM      7.2
.SAVEAP   FORM      7.2
.LRINC    FORM      7.2
.ADDCODE  DIM       2
.STR14    DIM       14
.ACCMPR   FORM      3.2
.AEXTCD   DIM       1
.ANINCD   FORM      1
.CODENUM  FORM      2
.FORM73   FORM      7.3
.FORM32   FORM      3.2
.SVEACR   FORM      7.2
.CMPCOM   FORM      7.2
.BRKCOM   FORM      7.2
.FLOAMT   FORM      7.2
.SVECOM   FORM      7.4
.NININC   FORM      7.2
.PREPAY   FORM      7.2
. TRIPLEX BILLING VARIABLES.
.CALCRUN   FORM     9.2
..===================================================
..THESE ARE EXCHANGE PORTION OF TDMC RUNNING CHARGES.
..===================================================
.RUNAR    FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
.RUNLR    FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
.RUNPASS  FORM      9.2      TOTAL PASSED THROUGH TO TDMC.
.RUNFLAT  FORM      9.2      ALL NON-RUN CHARGES. IE MAG TAPE.
.RUNCOUNT FORM      5
..===================================================
..THESE ARE RENTAL/SPLIT PORTION OF TDMC RUNNING CHARGES.
..===================================================
.RUNRAR   FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
.RUNRLR   FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
.RUNRPASS FORM      9.2      TOTAL PASSED THROUGH TO TDMC.
.RUNRFLAT FORM      9.2      ALL NON-RUN CHARGES. IE MAG TAPE.
.RUNRCNT  FORM      5
.
.PREPAYSW DIM       1            =Y IF PREPAY $
.ppsw     dim       1

........................................................
.. WORK VARIABLES
..............................................
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.RUNFLAG   DIM      1         HOLDS STAR IF TDMC RUNNING CHARGES INVOLVED
commflag form      1
cancprnt dim       10
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
detail   form      5
HOLDMLR  DIM       4
HOLDPO   DIM       12
calcamt  FORM      7.4
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
TDMCLIST INIT      "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
.
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
.ACAMT    FORM      7.2
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
.FORM72   FORM      7.2
.FORM74   FORM      7.4
FORM52   FORM      5.2
.FORM82   FORM      8.2
.FORM92   FORM      9.2
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
. 
.
...............................................................................
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
.MASK32   INIT      "ZZZ.ZZ-"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
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
M$LRINC  DIM       13
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
oopsflag dim       1       .' ' if ok, '*' if datacard 10% comm.
.
paydate  dim       8
infile   file      fixed=294
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
notecomm init      "Commission Already Taken !"
commnt   dim       30
comalrdy  init      "N"
.
.         DISPLAY   *P1:1,*EF," MONTHLY INVOICE REGISTER PRINT PROGRAM"
         MOVE      "NINCAL" TO COMPNME
         MOVE      "NEOM0022" TO PROGRAM
         MOVE      "Epsilon COMMISSION REG" TO STITLE
         move      c1 to ndatpath
         IFZ       PC
.START PATCH 1.91 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\COMMREG.lst"
         PACK      STR35,NTWKPATH1,"COMMREG.lst"
         SPLOPEN   STR35
.END PATCH 1.91 REPLACED LOGIC
         XIF
.       
CLOCK    
         TRAP      EXIT1 IF F5
         CLOCK     DATE TO DATE
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
         CALL      FUNCDISP
.
DATE     MOVE      YES TO ANS
         KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,ANS
         CMATCH    YES TO ANS
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
.         GOTO      OPENREST
BEGIN
.        READTAB   RECNUM,C0;*TAB,BEGINV
.         CLOSE     RECNUM
.         MOVE      BEGINV TO NINVFLD
         MOVE      "NINVLAST" TO GNXTFLD
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
         GOTO      eoj IF OVER
         MOVE      B1 TO RUNFLAG
         MATCH     INVDTEM TO SYSMO
         GOTO      eoj IF NOT EQUAL
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT
.begin patch 1.9
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 1.9
.
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
.
         COMPARE   C1 TO COUNT
         IF        EQUAL
         MOVE      MLRN TO HOLDMLR
         MOVE      OMLRPON TO HOLDPO
         ENDIF
         clear     cancprnt
         reset     cancodes
         scan      ostat in cancodes
         if        equal
         move      "Cancelled"  to cancprnt
         move      c1 to commflag            .cancelled- no commission
         endif
         MATCH     MLRN TO HOLDMLR
         CALL      TOTAL IF NOT EQUAL
         MATCH     OMLRPON TO HOLDPO
         CALL      TOTAL IF NOT EQUAL
         match     "0192" to obrknum
         goto       readinv if not equal
         match     "0904" to omlrnum
         goto       ok if  equal
         match     "1762" to omlrnum
         goto       ok if equal
         match     "1604" to omlrnum
         goto       ok if equal
         goto       readinv
.         
ok
;	match     "20" to commpct
	If	(commpct = "20")
	move	c2 to commflag
	else
	move	c1 to commflag
	endif
;.         goto      readinv if not equal  
;         if        not equal
;         move      c1 to commflag                   .no commission
;         else 
;         move      c2 to commflag
;         endif
.
         move      olnum to ndatfld
         rep       zfill in ndatfld
         call      ndatkey
         move      c0 to n3                   .lets double check comm
;begin patch 1.93
               call           trim using commper
               move           commper to n3
;end patch 1.93
         compare   "20" to n3
         if        not equal
         move     star to oopsflag
         endif
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
         MOVE       C0 TO FORM82
         MOVE       C0 TO FORM72
         MOVE       QTYbild TO FORM82
         MOVE       FORM82 TO FORM72    
         MULT       ".00234" BY FORM82          60% TRIPLEX ON 3.90
         MULT       ".00156" BY FORM72          40%  COMMISSION ON 3.90 
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
.
.
PROCESS  MOVE      C0 TO FORM7
         MOVE      qtybild TO FORM7
.
         MOVE      PPM TO FORM72
         DIVIDE    HUND INTO FORM72
         MOVE      FORM72 TO FORM32
.
.
         CALL      READBILL
.
.begin patch 1.9
        call       wipecvars
        move      olon to nownfld
        move      c1 to nownpath
        call      nownkey
        move       lrn to nshpfld
        move       lrn to nmrgfld
        move       no to shipsw
        move       no to mrgsw
        call       nmrgkey
        if         not over
        move       yes to mrgsw
        endif
        call       nshpkey
        if         not over
        move       yes to shipsw
        endif
.end patch 1.9
.
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad

         CALL      COMPUTE
.
CHKADJ   MOVE      LRN TO NADJFLD
         REP       ZFILL IN NADJFLD
         CALL      NADJKEY
         GOTO      maskit IF OVER
.
.
.begin patch 1.9
.         MOVE      ASRECADJ TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
.         ADD       FORM82 TO FORMAR
         add       asrecadj to formar
. 
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM72
.         MOVE      CVTFLD TO FORM72
.         MULTIPLY  ".01"  BY FORM72
.         ADD       FORM72 TO LRINC
.         MULT      SEQ BY FORM72

         CMATCH    STAR TO OOPSFLAG           .NON 20% LIST ?
         IF        EQUAL                     .MOST LIKELEY WE TOOK 20 ON A 10%
.         COMPARE   FORM72 TO CALCAMT         .adjusment = calcamnt?
         COMPARE   aslrinc TO CALCAMT         .adjusment = calcamnt?
             IF        EQUAL                  .YES
             MOVE      C1 TO COMMFLAG          .SORRY NO COMMIS
             ENDIF
         ENDIF
.
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP
         add       aspayad1 to ap
.
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO FORMAP2
         add        aspayad1 to formap2
.end patch
.
MASKIT
.         cmatch    yes to ans
.         if        equal
.         move      amount to calcamt
.         div       calcamt by c2
.         mult      ".10" by calcamt
         MOVE      MASK72 TO M$LRINC
         EDIT      calcamt TO M$LRINC
         MOVE      MASK72 TO M$AR
         EDIT      FORMAR TO M$AR
         cmatch    no to comalrdy
         if        equal
            compare   c2 to commflag
            if        equal
               ADD       calcamt TO TOTLR
            endif
         endif
         goto      print
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
         MOVE      MCCTO TO BILNAME
         RETURN
.  
.begin patch 1.9
.COMPUTE  MOVE      C0 TO SELECT
.         MOVE      C0 TO SHIP
.         CLEAR     PREPAYSW
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
.         MOVE      C0 TO AMOUNT
.         MOVE      C0 TO SVEACR
.         MOVE      C0 TO FORM72
.         MOVE      C0 TO FORM73
.         MOVE      C0 TO FORM74
.         MOVE      C0 TO FORM32
..
.         MOVE      qtybild TO FORM74
.         COMPARE   C0 TO FORM74
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    THOUS INTO FORM74
.         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
..
.         MOVE      C0 TO FORM72
.         MOVE      PPM TO FORM72
.         COMPARE   C0 TO FORM72
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO PRICE               PRICE PER M
..
.         MULT      PRICE BY FORM74
.         MOVE      FORM74 TO GROSS               GROSS BILLING.
.         move      form74 to calcamt
.         mult      ".10" by calcamt            .calculated epsilon com
.         MOVE      FORM74 TO FORMAR                  ACCOUNTS RECEIVABLE.
.         MOVE      FORM74 TO SVEACR              WORKING AR.
..
.FNINCD   MOVE      C0 TO N1                      CLEAR BRANCH VAR.
.         MOVE      PAYCODE TO N1                     PAYABLE CODE
.         BRANCH    N1 TO CHKCHRGS,FNINCD2,CHKCHRGS,FNINCD4
.FNINCD2  MOVE      C0 TO FORM32
.         MOVE      COMMPCT TO FORM32
.         COMPARE   "0" TO FORM32                    C0 COMMISSION?
.         GOTO      CHKCHRGS IF EQUAL
.         DIVIDE    HUND INTO FORM32
.         MOVE      FORMAR TO LRINC
.         MULT      FORM32 BY LRINC               LR INCOME.
.         MOVE      LRINC TO CMPCOM
.         GOTO      CHKCHRGS
.FNINCD4  MOVE      FORMAR TO LRINC
..
.CHKCHRGS MOVE      "0" TO INDEX
.NEXTCHRG ADD       "1" TO INDEX
.         CLEAR     PREPAYSW
.         COMPARE   "11" TO INDEX
.         goto      endchrgs if equal
.         LOAD      STR14 FROM INDEX OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
.                   ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10
.         UNPACK    STR14 INTO ADDCODE,STR12
.         UNPACK    STR12 TO STR7,AEXTCD,STR3,STR1
.         MATCH     "  " TO ADDCODE
.         goto      endchrgs if equal
.         match     "01" to addcode
.         if         equal
.         move      yes to comalrdy
.         move      notecomm to commnt
.          endif
.         MOVE      ADDCODE TO ADDKEY
..
..
.         MOVE      C0 TO ACAMT
.         MOVE      C0 TO ACCMPR
.         MOVE      C0 TO ANINCD
..
.         MOVE      STR7 TO FORM72
.         DIVIDE    HUND BY FORM72
.         MOVE      FORM72 TO ACAMT
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
.SLCTPERM MOVE      qtybild TO FORM74
.         DIVIDE    THOUS BY FORM74
.         MULTIPLY  ACAMT BY FORM74
.         MOVE      FORM74 TO AMOUNT
.         ADD       AMOUNT TO SELECT
.         GOTO      SUMAMT
..
.SHIPPERM MOVE      qtybild TO FORM74
.         DIVIDE    THOUS BY FORM74
.         MULTIPLY  ACAMT BY FORM74
.         MOVE      FORM74 TO AMOUNT
.         ADD       AMOUNT TO SHIP
.         GOTO      SUMAMT
..
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
.         MOVE      AMOUNT TO FORM72
.         SUB       SVECOM FROM FORM72
.         MOVE      FORM72 TO SVECOM
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
.         RETURN
..PAYCD1 - PAYCODE = '1', NININCOME = GROSS - A/P - TAXES.
.PAYCD1   MOVE      FORMAR TO NININC
.         SUBTRACT  AP FROM NININC
.         SUBTRACT  TAXES FROM NININC
.         SUBTRACT  POST FROM NININC
..         SUBTRACT  PREPAY FROM FORMAR
.         RETURN
..PAYCD2 - PAYCODE = '2', 2- A/P'S,
..         A/P'S AS DEFINED IN INVOICE RECORD.
.PAYCD2   MOVE      C0 TO FORM72
.         MOVE      AP1 TO FORM72
.         DIV       HUND INTO FORM72
.         MOVE      FORM72 TO SAVEAP
.         MOVE      SAVEAP TO AP
.         MOVE      FORMAR TO FORM72
.         SUBTRACT  AP FROM FORM72
.         MOVE      AP2 TO FORMAP2
.         DIV       HUND INTO FORMAP2
.         SUB       FORMAP2 FROM FORM72
..         SUBTRACT  PREPAY FROM FORMAR
.         MOVE      FORM72 TO LRINC
.         SUB       TAXES FROM LRINC
.         SUB       POST FROM LRINC
.         RETURN
.PAYCD3   MOVE      C0 TO FORM72
.         MOVE      AP1 TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      C0 TO AP
.         MOVE      FORM72 TO AP
.         MOVE      FORMAR TO LRINC
.         SUBTRACT  AP FROM LRINC
..         MOVE      AR TO CMPCOM
..        SUBTRACT  FORM72 FROM CMPCOM
.         SUBTRACT  TAXES FROM LRINC
.         SUBTRACT  POST FROM LRINC
..        SUBTRACT  PREPAY FROM FORMAR
.         RETURN
..
..         move      c0 to n2
..         move      addcode to n2
..         compare   c3 to n2
..         MATCH     "03" TO ADDCODE
..         GOTO      nextchrg IF not EQUAL
..         move      c0 to form72
..         MOVE      STR7 TO FORM72
..         DIVIDE    HUND BY FORM72
..         MOVE      FORM72 TO ACAMT
..       MOVE      STR3 TO FORM32
..       DIVIDE    HUND BY FORM32
..       MOVE      c0 TO ACCMPR
..       MOVE      FORM32 TO ACCMPR
..CD03     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".20" BY AMOUNT
..       ADD       AMOUNT TO BRKCOM
..       move      yes to ans
..
..
*......................................................................
PRINT    COMPARE   "59" TO LINES
         CALL      HEADER IF EQUAL
         CALL      HEADER IF not less
         compare   c1 to commflag
         if        equal
         MOVE      MASK72 TO M$LRINC
         MOVE      C0 TO FORM72
         EDIT      FORM72 TO M$LRINC
         endif
         clear     paydate
         type      mlrpayd
         if        equal
         move      "XX/XX/XX" to paydate
         edit      mlrpayd to paydate
         endif
         PRINT     *2,MLRN,*9,LRN,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*60,M$ar,b1,commnt:
                   *91,m$lrinc,oopsflag,*110,paydate:
                   *L,*1,COBN,"-",BILLTN,*7,hpbon,cancprnt,*7,cancprnt,hpboff:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *26,BILCOMp,"Using - ",o1des,*l
         ADD       c3 TO LINES
         move     b1 to oopsflag
         add       c1 to detail
         CLEAR     PMASK
         clear     commnt
         move      no to comalrdy
         move      no to ans
         GOTO      INPUT
*......................................................................
.
TOTAL    COMPARE   "59" TO LINES
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$lrinc
         EDIT      TOTlr TO MT$lrinc
         PRINT     HPBON,HPITALIC,*1,detail," INVOICES ","For: ",holdpo,b1:
                   "*** TOTAL --> ",*91,MT$lrinc,HPBOFF,HPUPRGHT
.
         PRINT     *FLUSH
         MOVE      C0 TO DETAIL
         MOVE      C0 TO totlr
         MOVE      MLRN TO HOLDMLR
         MOVE      OMLRPON TO HOLDPO
         MOVE      C0 TO PAGE        
         CALL      HEADER
         RETURN
         GOTO      EOJ
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch         .compressed
         endif
         PRINT     *f,HPBON,*27,"***  N I N   ",hpitalic,"E P S I L O N   ":
                   hpuprght,"C O M M I S S I O N   R E G I S T E R  ***":
                   *116,"DATE: ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"MAILER",*11,"LR":
                   *17,"INVOICE":
                   *26,"MAILER BILL-TO":
                   *52,"---------TOTAL---------":
                   *80,"------COMMISSION-------":
                   *104,"-------PAYMENT--------":
                   *L,*1,"NUMBER",*9,"NUMBER":
                   *18,"NUMBER",*26,"NAME AND THRU":
                   *52,"---------BILLED--------":
                   *80,"----------DUE----------":
                   *104,"-------RECEIVED-------":
                   *L,HPBOFF
         MOVE      c6 TO LINES
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
........................
.
EOJ      call      total
EXIT1    STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
.patch1.92
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.92
         INCLUDE   NORDIO.inc
;begin patch 2.0
;         INCLUDE   NINVIO.inc
         	INCLUDE   	ninvio.inc
	Include	NInvAcdio.inc
;end patch 2.0
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         INCLUDE   GNXTIO.inc
.begin patch 1.9
         include   nshpio.inc
         include   nmrgio.inc
         include   nadjio.inc
;begin patch 2.0
;         include   compute.inc
         	include   	compute.inc
;end patch 2.0
         include   nacdio.inc
         include   nownio.inc
.end patch 1.9
         include   ndatio.inc
         INCLUDE    COMLOGIC.inc


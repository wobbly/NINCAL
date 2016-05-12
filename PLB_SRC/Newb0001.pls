...............................................................................
.NEwb0001 - 
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   hp.inc
.begin patch 1.2
;begin patch 1.7         
;         INC       NINVDD.inc
          INclude   ninvdd.inc          
          Include   NinvAcddd.inc
;end patch 1.7         
         include   nmrgdd.inc
         include   nacddd.inc
         include   nowndd.inc
         include   nshpdd.inc
.end patch 1.2
;Patch1.6
                              include   compdd.inc
                              include   cntdd.inc
.         INC       NMLRDD.inc
;Patch1.6
         include   nadjdd.inc
         INCLUDE   NBILDD.inc
         INCLUDE   NORDDD.inc
         include   ndatdd.inc
         INCLUDE   GNXTDD.inc
;Patch1.6
.         INCLUDE   NBRKDD.INC
;Patch1.6
         INCLUDE   NDAT3DD.inc
.Start patch #1.1 - CONSACCT.INC has been included
         INCLUDE   CONSACCT.inc
.END patch #1.1 - CONSACCT.INC has been included
.
...........................................
.
.sort diskin 
.by list & order date   16-21,177-178,173-174,175-176
.
.........................................................................
release   init                "1.7"        DLH    10March2005         Invoice Conversion 
.
.release  init      "1.6"        DMB    26MAY2004 Mailer Conversion 
.release  init      "1.5"       20feb20040 ASH DATACARD CONVERSION
.release  init      "1.4"       02OCT2000 ASH NEW SERVER ADDED
.release  init      "1.3"       20Aug99 DLH NINAdj,nadjust y2k file expansion
.release  init      "1.2"       29APR99 DLH NININV Y2K, File expansion
.release  init      "1.1"       30Dec98 ASH NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"       17oct97
.CLOCK    FUNCTION
........................
.Start patch #1.1 - Following vars have been deleted as CONSACCT.INC has been included.
....DELETED AND INCREASED!!!
.FORM82  - UNVIVERSALLY MOVED TO FORMNINETWO2
.FORMAR  
.AP      
.FORMAP2 
.LRINC   
.ACAMT   
.SVECOM  
.SAVEAP  
.CMPCOM  
.NININC  
.AMOUNT  
.CANUSE  
.BRKCOM  
.SELECT  
.SHIP    
.taxes   
.SVEACR  
.GROSS   
.FLOAMT  
.PREPAY    
....DELETED!!!
.ADDKEY
.RUNFLAG
.THOUS
.HUND
.INDEX
.POST
.PRICE
.CALCRUN
.RUNAR
.RUNLR
.RUNPASS
.RUNFLAT
.RUNCOUNT
.RUNRAR
.RUNRLR
.RUNRPASS
.RUNRFLAT
.RUNRCNT
.ADDCODE
.STR14
.ACCMPR
.AEXTCD
.ANINCD
.CODENUM
.FORM73
.FORM32
.PREPASW
.PPSW
.FORM92
.End patch #1.1 - Following vars have been deleted as CONSACCT.INC has been included.
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
.
.  KEY VARIABLES
.............................................
.
.
.Start Patch #1.1 - vars now found in CONSACCT.INC
.ADDKEY   DIM       2
.end Patch #1.1 - vars now found in CONSACCT.INC
. WORK VARIABLES
..............................................
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.begin patch 1.3
mrgsw    dim       1
shipsw   dim       1
.end patch 1.3
.Start Patch #1.1 - vars now found in CONSACCT.INC.
.RUNFLAG   DIM      1         HOLDS STAR IF TDMC RUNNING CHARGES INVOLVED
.End Patch #1.1 - vars now found in CONSACCT.INC
commflag form      1
cancprnt dim       10
.
.Start Patch #1.1 - vars now found in CONSACCT.INC
.THOUS    FORM      "1000"
.HUND     FORM      "100"
.INDEX    FORM      2
.End Patch #1.1 - vars now found in CONSACCT.INC
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
detail   form      5
HOLDList  DIM       6
calcamt  FORM      7.4
.Start Patch #1.1 - vars now found in CONSACCT.INC
.AMOUNT   FORM      7.4
.GROSS    FORM      7.2
.CANUSE   FORM      7.4            CANADIAN USE TAX
.SHIP     FORM      7.2
.POST     FORM      3.2
.SELECT   FORM      7.2
.TAXES    FORM      7.2
.PRICE    FORM      3.2
.FORMAR   FORM      7.2
.AP       FORM      7.2
.FORMAP2  FORM      7.2
.SAVEAP   FORM      7.2
.LRINC    FORM      7.2
.End Patch #1.1 - vars now found in CONSACCT.INC
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
. TRIPLEX BILLING VARIABLES.
.Start Patch #1.1 - vars now found in CONSACCT.INC
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
.end Patch #1.1 - vars now found in CONSACCT.INC
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
.Start Patch #1.1 - vars now found in CONSACCT.INC
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
.PREPAYSW DIM       1            =Y IF PREPAY $
.ppsw     dim       1
.End Patch #1.1 - vars now found in CONSACCT.INC
PMASK    DIM       1
commamt  FORM      7.2
.Start Patch #1.1 - vars now found in CONSACCT.INC
.ACAMT    FORM      7.2
.End Patch #1.1 - vars now found in CONSACCT.INC
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
.Start Patch #1.1 - vars now found in CONSACCT.INC
.FORM72   FORM      7.2
.FORM74   FORM      7.4
.FORM82   FORM      8.2
.FORM92   FORM      9.2
.End Patch #1.1 - vars now found in CONSACCT.INC
FORM52   FORM      5.2
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
.begin patch 1.2
.MASK32   INIT      "ZZZ.ZZ-"
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.end patch 1.2
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
.Start Patch #1.1 - increase var
.M$AR     DIM       13
M$AR     DIM       15
.end Patch #1.1 - increase var
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       13
M$AP2    DIM       13
M$STAX   DIM       8
M$CTAX   DIM       8
M$POST   DIM       6
.Start Patch #1.1 - increase var
.M$LRINC  DIM       13
M$LRINC  DIM       15
.end Patch #1.1 - increase var
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
.Start Patch #1.1 - increased file size
.infile   file      fixed=294
infile   file      fixed=408
.End Patch #1.1 - increased file size
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
commnt   dim       30
bstatus  init       "          "
invsw    dim       1
lstmexsw dim       1      list management exchange sw
.comalrdy  init      "N"
.START PATCH 1.4 REPLACED LOGIC
.sort     init      "f:\netutils\sort.exe g:\data\"
.sorta    init      ".dat g:\data\newb.dat /s (16,6,c,a,177,2,c,a,173,2,c,a,175,2,c,a) F(tab) verbose"
sort     dim       45
sorta    dim       100
         pack      sort,ntwkpath2,"sort.exe ",ntwkpath1
         pack      sorta,".dat ",NTWKPATH1,"newb.dat /s (16,6,c,a,177,2,c,a,173,2,c,a,175,2,c,a) F(tab) verbose"
.END PATCH 1.4 REPLACED LOGIC
sortstring dim     130
.

         MOVE      "NINCAL" TO COMPNME
         MOVE      "NEwb0001" TO PROGRAM
         MOVE      "New business REG" TO STITLE
         move      c1 to ndatpath
         keyin     *p10:12,*el,"enter filename ",str8
         pack      sortstring from sort,str8,sorta
         execute   sortstring
         IFZ       PC
.START PATCH 1.4 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\NewbREG.lst"
         PACK      STR35,NTWKPATH1,"NewbREG.lsT"
         SPLOPEN   STR35
.END PATCH 1.4 REPLACED LOGIC
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
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         GOTO      BEGIN
SHAREINV TRAPCLR   IO
        move       c1 to nbrkpath
         GOTO      OPENREST
BEGIN
.START PATCH 1.4 REPLACED LOGIC
.         open      infile,"g:\data\newb"
         PACK      STR35,NTWKPATH1,"Newb"
         open      infile,STR35
.END PATCH 1.4 REPLACED LOGIC
         ADD       C1 TO HOLDREC
.
INPUT
.
.
READINV  read      infile,seq;ordvars
         GOTO      eoj IF OVER
         ADD       C1 TO COUNT
         COMPARE   C1 TO COUNT
         call      header If EQUAL
         MOVE      B1 TO RUNFLAG
         move      olrn to ninvfld
         move      yes to invsw
        move       no to lstmexsw       
         call      ninvkey
         if        over
         move      no to invsw
         clear     str2
         pack      str2 from osales10,osales
         move      c0 to n2
         move      str2 to n2
         compare   c6 to n2
         if        equal
         if        (oelcode = "2" | oelcode = "3")
         move      yes to lstmexsw
         endif
         endif
         endif
.         goto      readinv if over
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT
.begin patch 1.2
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 1.2
.
         COMPARE   C1 TO COUNT
         IF        EQUAL
         MOVE      olnum TO HOLDlist
         ENDIF
         move      c2 to commflag
         clear     cancprnt
         reset     cancodes
         scan      ostat in cancodes
         if        equal
         move      "Cancelled"  to cancprnt
         move      c1 to commflag            .cancelled- no commission
         endif
         MATCH     olnum TO HOLDlist
         CALL      TOTAL IF NOT EQUAL
.         
ok
         CALL      READMLR
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         PACK      NBRKFLD FROM oBRKNUM,oBRKCNT
         CMATCH    B1 TO NBRKFLD
         goto      goon IF EOS
         call      nbrkkey
         IF        NOT OVER
         move      mcomp to bilCOMP
         move      BRCOMP to bilNAME
         ENDIF
goon     
.         match     "20" to commpct
.         if        not equal
.         move      c1 to commflag                   .no commission
.         else 
.         move      c2 to commflag
.         endif
.
         move      olnum to ndatfld
         rep       zfill in ndatfld
         call      ndatkey
         REP       ZFILL IN OLNUM
         MATCH     OLNUM TO TDMCLIST
         IF         EQUAL
         ADD        C1 TO RUNCOUNT
         MOVE       STAR TO RUNFLAG
.Start Patch #1.1 - replaced logic
.         MOVE       C0 TO Form82
.         MOVE       C0 TO Form72
.         cmatch     yes to invsw
.         if       equal
.         MOVE       QTYSHP TO Form82
.         else
.         move       oqty to Form82
.         endif
.         MOVE       Form82 TO Form72    
.         MULT       ".00234" BY Form82          60% TRIPLEX ON 3.90
.         MULT       ".00156" BY Form72          40%  COMMISSION ON 3.90 
.         ADD        Form82 TO RUNPASS       TDMC PORTION
.         ADD        Form72 TO RUNLR         LR INC PORTION
.         ADD        Form72 TO Form82        TOTAL RUNNING CHARGE
.         MOVE       C0 TO FORM92
.         MOVE       AR TO FORM92             TOTAL BILLED
.         MULT       ".01" BY FORM92
.         ADD        FORM92 TO RUNAR
.         SUB        Form82 FROM FORM92      FIND FLAT FEE PORTION
.
         MOVE       C0 TO FORMNINETWO2
         MOVE       C0 TO CMPT92
         cmatch     yes to invsw
         if       equal
.begin patch 1.2
.         MOVE       QTYSHP TO FORMNINETWO2
         MOVE       QTYbild TO FORMNINETWO2
.end patch 1.2
         else
         move       oqty to FORMNINETWO2
         endif
         MOVE       FORMNINETWO2 TO CMPT92    
         MULT       ".00234" BY FORMNINETWO2          60% TRIPLEX ON 3.90
         MULT       ".00156" BY CMPT92          40%  COMMISSION ON 3.90 
         ADD        FORMNINETWO2 TO RUNPASS       TDMC PORTION
         ADD        CMPT92 TO RUNLR         LR INC PORTION
         ADD        CMPT92 TO FORMNINETWO2        TOTAL RUNNING CHARGE
         MOVE       C0 TO FORM92
         MOVE       AR TO FORM92             TOTAL BILLED
         MULT       ".01" BY FORM92
         ADD        FORM92 TO RUNAR
         SUB        FORMNINETWO2 FROM FORM92      FIND FLAT FEE PORTION
.end Patch #1.1 - replaced logic
         ADD        FORM92 TO RUNFLAT        SAVE IT.
         ELSE
         MOVE       B1 TO RUNFLAG
         ENDIF
.
.
PROCESS
.begin patch 1.2
.        MOVE      C0 TO FORM7
.         MOVE      QTYSHP TO FORM7
.end patch 1.2
.
.Start Patch #1.1 - replaced logic
.         MOVE      PPM TO Form72
.         DIVIDE    HUND INTO Form72
.         MOVE      Form72 TO FORM32
.
.begin patch 1.2
.         MOVE      PPM TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
         move       ppm to form32
.end patch 1.2
.end Patch #1.1 - replaced logic
.
.
.         CALL      READmlr
.
.
         cmatch    yes to invsw
         if        equal
.begin patch 1.2
         move      olon to nownfld
         call      nownkey
         move      olnum to ndatfld
         call      ndatkey
         move      lrn to nshpfld
         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
         MOVE      LRN TO NmrgFLD
         REP       ZFILL IN NmrgFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         call      wipecvars
.end patch 1.2
         CALL      COMPUTE 
         move      cmpcom to lrinc           .we want net only.
         sub       brkcom from lrinc         .no pass thru, etc.
         endif
.
CHKADJ   cmatch    yes to invsw
         if        equal
         MOVE      LRN TO NADJFLD
         REP       ZFILL IN NADJFLD
         CALL      NADJKEY
         GOTO      maskit IF OVER
         else
         goto      maskit
         endif
.
.
.begin patch 1.3
.          MOVE      ASRECADJ TO CVTFLD
.         CALL      CVT
.end patch 1.3
.Start Patch #1.1 - replaced logic
.         MOVE      c0 TO Form82
.         MOVE      CVTFLD TO Form82
.         MULTIPLY  ".01"  BY Form82
.         ADD       Form82 TO FORMAR
.. 
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO Form72
.         MOVE      CVTFLD TO Form72
.         MULTIPLY  ".01"  BY Form72
.         ADD       Form72 TO LRINC
.         MULT      SEQ BY Form72
..         CMATCH    STAR TO OOPSFLAG           .NON 20% LIST ?
..         IF        EQUAL                     .MOST LIKELEY WE TOOK 20 ON A 10%
..         COMPARE   Form72 TO CALCAMT         .adjusment = calcamnt?
..             IF        EQUAL                  .YES
..             MOVE      C1 TO COMMFLAG          .SORRY NO COMMIS
..             ENDIF
..         ENDIF
..
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT  
.         MOVE      c0 TO Form82
.         MOVE      CVTFLD TO Form82
.         DIV       HUND INTO Form82
.         ADD       Form82 TO AP
..
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO Form82
.         MOVE      CVTFLD TO Form82
.         DIV       HUND INTO Form82
.         ADD       Form82 TO FORMAP2
......
.         MOVE      c0 TO FORMNINETWO2
.         MOVE      CVTFLD TO FORMNINETWO2
.         MULTIPLY  ".01"  BY FORMNINETWO2
.begin patch 1.3
          add      ASRECADJ TO formar
.         ADD       FORMNINETWO2 TO FORMAR
. 
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO CMPT92
.         MOVE      CVTFLD TO CMPT92
.         MULTIPLY  ".01"  BY CMPT92
.         ADD       CMPT92 TO LRINC
         add       aslrinc to lrinc
         MULT      SEQ BY CMPT92        .? I left it what is it dlh 1.3
.         CMATCH    STAR TO OOPSFLAG           .NON 20% LIST ?
.         IF        EQUAL                     .MOST LIKELEY WE TOOK 20 ON A 10%
.         COMPARE   CMPT92 TO CALCAMT         .adjusment = calcamnt?
.             IF        EQUAL                  .YES
.             MOVE      C1 TO COMMFLAG          .SORRY NO COMMIS
.             ENDIF
.         ENDIF
.
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORMNINETWO2
.         MOVE      CVTFLD TO FORMNINETWO2
.         DIV       HUND INTO FORMNINETWO2
.         ADD       FORMNINETWO2 TO AP
         add       aspayad1 to ap
.
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      c0 TO FORMNINETWO2
.         MOVE      CVTFLD TO FORMNINETWO2
.         DIV       HUND INTO FORMNINETWO2
.         ADD       FORMNINETWO2 TO FORMAP2
          add       aspayad2 to formap2
.End Patch #1.1 - replaced logic
.end patch 1.3
.
MASKIT
.         cmatch    yes to ans
.         if        equal
.         move      amount to calcamt
.         div       calcamt by c2
.         mult      ".10" by calcamt
.START PATCH #1.1 - INCREASED VAR
.         MOVE      MASK72 TO M$LRINC
.         EDIT      lrinc TO M$LRINC
.         MOVE      MASK72 TO M$AR
.         EDIT      FORMAR TO M$AR
.
         MOVE      MASK92 TO M$LRINC
         EDIT      lrinc TO M$LRINC
         MOVE      MASK92 TO M$AR
         EDIT      FORMAR TO M$AR
.END PATCH #1.1 - INCREASED VAR
.         cmatch    no to comalrdy
.         if        equal
            compare   c2 to commflag
            if        equal
              ADD       lrinc TO TOTLR
            endif
.         endif
         goto      print
.         goto      input
.
.READMLR 
READMLR  PACK      MKEY FROM omlrnum,z3
         CALL      NMLRKEY
         MOVE      MCOMP TO BILCOMP
.         MOVE      MCCTO TO BILNAME
         RETURN
.  
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
..Start Patch #1.1 - replaced logic
..         MOVE      C0 TO Form72
..         MOVE      C0 TO FORM73
..         MOVE      C0 TO FORM74
.         MOVE      C0 TO CMPT92
.         MOVE      C0 TO FORM73
.         MOVE      C0 TO CMPT94
..End Patch #1.1 - replaced logic
.         MOVE      C0 TO FORM32
..
..Start Patch #1.1 - replaced logic
..         MOVE      QTYSHP TO FORM74
..         COMPARE   C0 TO FORM74
..         GOTO      FNINCD IF EQUAL
..         DIVIDE    THOUS INTO FORM74
..         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
...
..         MOVE      C0 TO Form72
..         MOVE      PPM TO Form72
..         COMPARE   C0 TO Form72
..         GOTO      FNINCD IF EQUAL
..         DIVIDE    HUND INTO Form72
..         MOVE      Form72 TO PRICE               PRICE PER M
...
..         MULT      PRICE BY FORM74
..         MOVE      FORM74 TO GROSS               GROSS BILLING.
..         move      FORM74 to calcamt
..
.         MOVE      QTYSHP TO CMPT94
.         COMPARE   C0 TO CMPT94
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    THOUS INTO CMPT94
.         MOVE      CMPT94 TO AMOUNT              QUANTITY BILLED
..
.         MOVE      C0 TO CMPT92
.         MOVE      PPM TO CMPT92
.         COMPARE   C0 TO CMPT92
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO PRICE               PRICE PER M
..
.         MULT      PRICE BY CMPT94
.         MOVE      CMPT94 TO GROSS               GROSS BILLING.
.         move      CMPT94 to calcamt
..End Patch #1.1 - replaced logic
..
.         move      c0 to form32
.         MOVE      COMMPCT TO FORM32
.         DIVIDE    HUND INTO FORM32
.         MULT      FORM32 BY calcamt               LR INCOME.
..
..Start Patch #1.1 - replaced logic
..         MOVE      FORM74 TO FORMAR                  ACCOUNTS RECEIVABLE.
..         MOVE      FORM74 TO SVEACR              WORKING AR.
.         MOVE      CMPT94 TO FORMAR                  ACCOUNTS RECEIVABLE.
.         MOVE      CMPT94 TO SVEACR              WORKING AR.
..end Patch #1.1 - replaced logic
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
..         move      yes to comalrdy
.          endif
.         MOVE      ADDCODE TO ADDKEY
..
..
.         MOVE      C0 TO ACAMT
.         MOVE      C0 TO ACCMPR
.         MOVE      C0 TO ANINCD
..
..Start Patch #1.1 - replaced logic
..         MOVE      STR7 TO Form72
..         DIVIDE    HUND BY Form72
..         MOVE      Form72 TO ACAMT
..
.         MOVE      STR7 TO CMPT92
.         DIVIDE    HUND BY CMPT92
.         MOVE      CMPT92 TO ACAMT
..End Patch #1.1 - replaced logic
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
..Start Patch #1.1 - replaced logic
..SLCTPERM MOVE      QTYSHP TO FORM74
..         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
..         ADD       AMOUNT TO SELECT
..         GOTO      SUMAMT
...
..SHIPPERM MOVE      QTYSHP TO FORM74
..         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
..         ADD       AMOUNT TO SHIP
..         GOTO      SUMAMT
.SLCTPERM MOVE      QTYSHP TO CMPT94
.         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
.         ADD       AMOUNT TO SELECT
.         GOTO      SUMAMT
..
.SHIPPERM MOVE      QTYSHP TO CMPT94
.         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
.         ADD       AMOUNT TO SHIP
.         GOTO      SUMAMT
..End Patch #1.1 - replaced logic
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
..Start Patch #1.1 - replaced logic
..         MOVE      AMOUNT TO Form72
..         SUB       SVECOM FROM Form72
..         MOVE      Form72 TO SVECOM
...
.         MOVE      AMOUNT TO CMPT92
.         SUB       SVECOM FROM CMPT92
.         MOVE      CMPT92 TO SVECOM
..End Patch #1.1 - replaced logic
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
..Start Patch #1.1 - replaced logic
..PAYCD2   MOVE      C0 TO Form72
..         MOVE      AP1 TO Form72
..         DIV       HUND INTO Form72
..         MOVE      Form72 TO SAVEAP
..         MOVE      SAVEAP TO AP
..         MOVE      FORMAR TO Form72
..         SUBTRACT  AP FROM Form72
..         MOVE      AP2 TO FORMAP2
..         DIV       HUND INTO FORMAP2
..         SUB       FORMAP2 FROM Form72
...         SUBTRACT  PREPAY FROM FORMAR
..         MOVE      Form72 TO LRINC
..         SUB       TAXES FROM LRINC
..         SUB       POST FROM LRINC
..         RETURN
..PAYCD3   MOVE      C0 TO Form72
..         MOVE      AP1 TO Form72
..         DIVIDE    HUND INTO Form72
..         MOVE      C0 TO AP
..         MOVE      Form72 TO AP
..         MOVE      FORMAR TO LRINC
..         SUBTRACT  AP FROM LRINC
...         MOVE      AR TO CMPCOM
...        SUBTRACT  Form72 FROM CMPCOM
..
.PAYCD2   MOVE      C0 TO CMPT92
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
.         RETURN
.PAYCD3   MOVE      C0 TO CMPT92
.         MOVE      AP1 TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      C0 TO AP
.         MOVE      CMPT92 TO AP
.         MOVE      FORMAR TO LRINC
.         SUBTRACT  AP FROM LRINC
..         MOVE      AR TO CMPCOM
..        SUBTRACT  CMPT92 FROM CMPCOM
..End Patch #1.1 - replaced logic
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
..Start Patch #1.1 - replaced logic
...         move      c0 to Form72
...         MOVE      STR7 TO Form72
...         DIVIDE    HUND BY Form72
...         MOVE      Form72 TO ACAMT
....
..         move      c0 to CMPT92
..         MOVE      STR7 TO CMPT92
..         DIVIDE    HUND BY CMPT92
..         MOVE      CMPT92 TO ACAMT
..End Patch #1.1 - replaced logic
..        MOVE      STR3 TO FORM32
..        DIVIDE    HUND BY FORM32
..        MOVE      c0 TO ACCMPR
..        MOVE      FORM32 TO ACCMPR
..CD03     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".20" BY AMOUNT
..        ADD       AMOUNT TO BRKCOM
..        move      yes to ans
..
..
.*......................................................................
PRINT    COMPARE   "59" TO LINES
         CALL      HEADER IF EQUAL
         CALL      HEADER IF not less
         compare   c1 to commflag
         if        equal
.Start Patch #1.1 - replaced logic
         MOVE      MASK72 TO M$LRINC
.         MOVE      C0 TO Form72
.         EDIT      Form72 TO M$LRINC
.         endif
.         cmatch   no to invsw
.         if        equal
.         MOVE      MASK72 TO M$AR
.         MOVE      C0 TO Form72
.         EDIT      Form72 TO M$AR
.         MOVE      MASK72 TO M$LRINC
.         MOVE      C0 TO Form72
.         EDIT      Form72 TO M$LRINC
..
         MOVE      MASK92 TO M$LRINC
         MOVE      C0 TO CMPT92
         EDIT      CMPT92 TO M$LRINC
         endif
         cmatch   no to invsw
         if        equal
         MOVE      MASK92 TO M$AR
         MOVE      C0 TO CMPT92
         EDIT      CMPT92 TO M$AR
         MOVE      MASK92 TO M$LRINC
         MOVE      C0 TO CMPT92
         EDIT      CMPT92 TO M$LRINC
.End Patch #1.1 - replaced logic
         endif
         clear     paydate
         type      mlrpayd
         if        equal
         move      "XX/XX/XX" to paydate
         edit      mlrpayd to paydate
         endif
         clear     bstatus
         cmatch    no to invsw
         if        equal
         clear     invnum
        
         move      "Not Billed" to bstatus
         endif
         clear     str25
         cmatch    yes to lstmexsw
         if        equal
         move      "List management Exchange" to str25
         endif
         PRINT     *2,omlrNum,*9,oLRN,*18,INVNUM,RUNFLAG:
                   *26,BILNAME," ",GUARPAY,*60,M$ar,b1,commnt:
                   *91,m$lrinc,oopsflag,*110,paydate:
                   *L,*1,COBN,"-",BILLTN,*7,hpbon,cancprnt,*7,cancprnt,hpboff:
                   *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                   b1,mcomp,b1,hpbon,hpitalic,bstatus,b1,str25,hpboff,hpuprght:
                   *l
         ADD       c3 TO LINES
         move     b1 to oopsflag
         add       c1 to detail
         CLEAR     PMASK
         clear     commnt
.         move      no to comalrdy
         move      no to ans
         GOTO      INPUT
*......................................................................
.
TOTAL    move      holdlist to ndatfld
         move      c1 to ndatpath
         call      ndatkey
.START PATCH 1.5 REPLACED LOGIC
.         move      c0 to n9
.         move      universe to n9
.
.         compare   "10000" to n9
.         if        less 
.         move      ".00" to form32
.         goto      total1
.         endif
.
.         compare   "100000" to n9
.         if        less 
.         move      ".05" to form32
.         goto      total1
.         endif
.         
.         compare   "1000000" to n9
.................
         move      c0 to n10
         move      universe to n10

         compare   "10000" to n10
         if        less 
         move      ".00" to form32
         goto      total1
         endif

         compare   "100000" to n10
         if        less 
         move      ".05" to form32
         goto      total1
         endif
         
         compare   "1000000" to n10
.END PATCH 1.5 REPLACED LOGIC
         if        less 
         move      ".08" to form32
         goto      total1
         endif

         move      ".10" to form32

total1   COMPARE   "59" TO LINES
         CALL      HEADER IF not less
         MOVE      MASK92 TO MT$lrinc
         EDIT      TOTlr TO MT$lrinc
         PRINT     HPBON,HPITALIC,*1,detail," INVOICES ",b1:
                   "*** Total Income--> ",*91,MT$lrinc
         mult      form32 by totlr
         MOVE      MASK92 TO MT$lrinc
         EDIT      TOTlr TO MT$lrinc
.START PATCH 1.5 REPLACED LOGIC
.         print     "List Universe ",n9," *** Bonus @ ",form32,"% -->  ",*91,MT$lrinc,HPBOFF,HPUPRGHT
         print     "List Universe ",N10," *** Bonus @ ",form32,"% -->  ",*91,MT$lrinc,HPBOFF,HPUPRGHT
.END PATCH 1.5 REPLACED LOGIC
.
         PRINT     *FLUSH
         MOVE      C0 TO DETAIL
         MOVE      C0 TO totlr
         MOVE      olnum TO HOLDlist
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
         PRINT     *f,HPBON,*27,"***  N I N   ",hpitalic,"New Business   ":
                   hpuprght,"C O M M I S S I O N   R E G I S T E R  ***":
                   *116,"DATE: ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",b3,o1des,*116,"PAGE ",PAGE:
                   *L,*L,*1,"Mailer",*11,"Lr":
                   *17,"Invoice":
                   *26,"Mailer Bill-to":
                   *52,"---------Total---------":
                   *80,"------List Rental------":
                   *104,"-------Payment--------":
                   *L,*1,"Number",*9,"Number":
                   *18,"Number",*26,"Name and thru":
                   *52,"---------Billed--------":
                   *80,"---------Income--------":
                   *104,"-------Received-------":
                   *L,HPBOFF
         MOVE      c6 TO LINES
         RETURN
.
.begin patch 1.3
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
.........................
.end patch 1.3
.
EOJ      call      total
EXIT1    STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
;Patch1.6
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
;Patch1.6
         INCLUDE   NORDIO.inc
;begin patch 1.7         
;         INCLUDE   NINVIO.inc
          Include   ninvio.inc
          Include   NInvAcdio.inc
;end patch 1.7         
         INCLUDE   NBILIO.inc
         INCLUDE   NDAT3IO.inc
         INCLUDE   GNXTIO.inc
         include   nadjio.inc
;Patch1.6
.         INCLUDE   NBRKIO.INC
;Patch1.6
         include   ndatio.inc
.begin patch 1.2
;begin patch 1.7         
;         include   compute.inc
          Include   compute.inc
;end patch 1.7         
         include   nmrgio.inc
         include   nacdio.inc
         include   nownio.inc
         include   nshpio.inc
.end patch 1.2

         INCLUDE    COMLOGIC.inc


...............................................................................
.NEOM0028  -  comsumer direct accrual estimator
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         include   hp.inc
;begin patch 2.5
;         INC       NINVDD.inc
	INC       	ninvdd.inc
	include	NinvAcddd.inc
;end patch 2.5
.patch2.45
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.INC
.patch2.45
         include   nowndd.inc
         include   nord2dd.inc
         include   ndatdd.inc
.START PATCH 1.6 REMOVED LOGIC
..START PATCH 1.4 ADDED LOGIC
.	include	nfuldd.inc
.END PATCH 1.4 ADDED LOGIC
.END PATCH 1.6 REMOVED LOGIC
...........................................
release  init	   "1.6"	 DMS	21JUN2006	Fulfillment Conversion
.release  init      "1.5"         DLH	7March2005	Invoice Conversion
;release  init      "1.45"         JD	26MAY2004	Mailer Conversion
;RELEASE  INIT      "1.4"         ASH 04FEB2002	NINFUL CONVERSION
.RELEASE  INIT      "1.3"         ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.2"         DLH  28APR99 NININV Y2K, File expansion
.RELEASE  INIT      "1.1"         ASH  14JAN99 NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"         DLH  20Nov95 

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
..............................................
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
RUNFLAG   DIM      1         HOLDS STAR IF TDMC RUNNING CHARGES INVOLVED
commflag form      1
cancprnt dim       5
.
THOUS    FORM      "1000"
HUND     FORM      "100"
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
INDEX    FORM      2
detail   form      5
HOLDMLR  DIM       4
HOLDPO   DIM       12
calcamt  FORM      7.4
AMOUNT   FORM      7.4
GROSS    FORM      7.2
CANUSE   FORM      7.4            CANADIAN USE TAX
SHIP     FORM      7.2
POST     FORM      3.2
ANS      DIM       1
TYPIST   DIM       2
SELECT   FORM      7.2
TAXES    FORM      7.2
PRICE    FORM      3.2
FORMAR   FORM      7.2
AP       FORM      7.2
FORMAP2  FORM      7.2
SAVEAP   FORM      7.2
LRINC    FORM      7.2
form84   form      8.4
.
totcd    form      9.2      .total consumer direcdt
TOTAR    FORM      9.2
TOTAP1   FORM      9.2
TOTAP2   FORM      9.2
TOTAP    FORM      9.2
TOTNIN   FORM      9.2
TOTLR    FORM      9.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
tapesw   dim       1
. TRIPLEX BILLING VARIABLES.
CALCRUN   FORM     9.2
.===================================================
.THESE ARE EXCHANGE PORTION OF TDMC RUNNING CHARGES.
.===================================================
RUNAR    FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
RUNLR    FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
RUNPASS  FORM      9.2      TOTAL PASSED THROUGH TO TDMC.               
RUNFLAT  FORM      9.2      ALL NON-RUN CHARGES. IE MAG TAPE.
RUNCOUNT FORM      5
.===================================================
.THESE ARE RENTAL/SPLIT PORTION OF TDMC RUNNING CHARGES.
.===================================================
RUNRAR   FORM      9.2      TOTAL ADDITIONAL TDMC RUNNING CHARGES AR.
RUNRLR   FORM      9.2      TOTAL COMMISION FROM RUNNING CHARGES.
RUNRPASS FORM      9.2      TOTAL PASSED THROUGH TO TDMC.               
RUNRFLAT FORM      9.2      ALL NON-RUN CHARGES. IE MAG TAPE.
RUNRCNT  FORM      5
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
ADDCODE  DIM       2
.START PATCH 1.4 REMOVED LOGIC - DUPLICATE VAR IN CONS.INC
.STR14    DIM       14
.END PATCH 1.4 REMOVED LOGIC - DUPLICATE VAR IN CONS.INC
ACCMPR   FORM      3.2
AEXTCD   DIM       1
ANINCD   FORM      1
CODENUM  FORM      2
FORM73   FORM      7.3
FORM32   FORM      3.2
SVEACR   FORM      7.2
CMPCOM   FORM      7.2
BRKCOM   FORM      7.2
FLOAMT   FORM      7.2
SVECOM   FORM      7.4
NININC   FORM      7.2
PREPAY   FORM      7.2
PMASK    DIM       1
PREPAYSW DIM       1            =Y IF PREPAY $
ppsw     dim       1
commamt  FORM      7.2
ACAMT    FORM      7.2
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM72   FORM      7.2
FORM74   FORM      7.4
FORM52   FORM      5.2
FORM82   FORM      8.2
FORM92   FORM      9.2
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
M$CD     DIM       13
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
MT$cd    DIM       15
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
.START PATCH #1.1 - REPLACED LOGIC
.ALTHOUGH THIS FILE IS NEVER OPENED!!
.infile   file      fixed=294
infile   file      fixed=408
.END PATCH #1.1 - REPLACED LOGIC
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
notecomm init      "Commission Already Taken !"
commnt   dim       30
comalrdy  init      "N"
.
.         DISPLAY   *P1:1,*EF," MONTHLY INVOICE REGISTER PRINT PROGRAM"
         MOVE      "Names in the News Ca" TO COMPNME
         MOVE      "NEOM0028" TO PROGRAM
         MOVE      "Consumer Direct Accrual" TO STITLE
         move      c1 to ndatpath
.START PATCH 1.3 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\COMMdrct.lst"
         PACK      STR35,NTWKPATH1,"COMMdrct.lst"
         SPLOPEN   STR35
.END PATCH 1.3 REPLACED LOGIC
         PRINT     hp17ptch,hpdupl,hptop,*F                
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
OPEN
         move      c1 to nownpath
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         GOTO      BEGIN
BEGIN
         move      "consdrct" to nordname
.
INPUT    COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
.
.
READINV  call      nord2seq
         GOTO      eoj IF OVER
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"NUMBER OF Records PROCESSED: ",COUNT
.
         move      c1 to ninvpath
         cmatch    b1 to olrn
         goto      readinv if eos
         MOVE      oLRN TO NinvFLD
         REP       ZFILL IN NinvFLD
         CALL      NinvKEY
         if        over
         reset     cancodes
         scan      ostat in cancodes
         goto      readinv if equal
.begin patch 1.2
         move      oqty to qtybild
         clear     invdtec
.Start Patch #1.1 - replaced logic until NININV.DAT is converted
.         move      oqty to qtyshp
.         count     n1,oqty
.         if (n1 = 9)
.                   BUMP      oqty,C2
.                   move      oqty to qtyshp
.                   RESET     oqty
.         elseif (n1 = 8)
.                   BUMP      oqty,C1
.                   move      oqty to qtyshp
.                   RESET     oqty
.         else         .n1 = 7
.                   move      oqty to qtyshp
.         endif
..End Patch #1.1 - replaced logic until NININV.DAT is converted
.end patch 1.2
         clear     invnum
         clear     mlrpayd
         clear     invdtem
         clear     invdted
         clear     invdtey
         endif
.
         clear     cancprnt
         reset     cancodes
         scan      ostat in cancodes
         if        equal
         move      "Cncld"  to cancprnt
         move      c1 to commflag            .cancelled- no commission
         endif
.         
ok
         move      olon to nownfld
         rep       zfill in ndatfld
         call      nownkey
         move       yes to tapesw
.START PATCH 1.4 REPLACED LOGIC
.         scan      "TDMC" in ownctn
.         if         equal 
.         move       no to tapesw
.         endif
	call	Trim using OWNCTN
.START PATCH 1.6 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"ok-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.	if (NFULFLD = "0026")
.		move	no,tapesw
.	else
.		scan	"TDMC",NFULCOMP
	if (OWNCTN <> "")
		pack	COMPFLD6, OWNCTN
		rep	zfill,COMPFLD6
		move	C1,COMPPATH
		move	"ok-COMPKEY6",Location
		pack	KeyLocation,COMPFLD6
		call	COMPKEY6
		if over
			clear 	COMPFLD6
			clear	COMPCOMP
		else
			if (COMPSVBFLG <> "T")
				clear 	COMPFLD6
				clear	COMPCOMP
			endif
		endif
	else	// OWNCTN = ""
		clear 	COMPFLD6
		clear	COMPCOMP
	endif
	if (COMPFLD6 = "0026")
			move	no,tapesw
		else
		scan	"TDMC",COMPCOMP
.END PATCH 1.6 REPLACED LOGIC
		if equal 
			move	no,tapesw
		endif
	endif
.END PATCH 1.4 REPLACED LOGIC
         goto       readinv if equal
         if         equal 
         move       no to tapesw
         endif
        
.
.
PROCESS  MOVE      C0 TO FORM7
.begin patch 1.2
.         MOVE      QTYSHP TO FORM84
         MOVE      QTYbild TO FORM84
.end patch 1.2
         div       "1000" into form84
.
         MOVE      "4.80" TO FORM72
         MOVE      FORM72 TO FORM32
.
         mult      form32 by form84
         cmatch    yes to tapesw
         if        equal 
         add       "25.00" to form84
         endif
.
.
MASKIT
         sub       formar from formar
         add       form84 to formar
         MOVE      MASK72 TO M$cd
         EDIT      FORMAR TO M$cd
         add       formar to totcd
*......................................................................
PRINT    COMPARE   "59" TO LINES
         CALL      HEADER IF EQUAL
         CALL      HEADER IF not less
         clear     paydate
         type      mlrpayd
         if        equal
.begin patch 1.2
         unpack    mlrpayd into cc,yy,mm,dd
.         move      "XX/XX/XX" to paydate
.         edit      mlrpayd to paydate
         move       "/" to str1
         pack       paydate from mm,str1,dd,str1,yy
         endif
.         PRINT     *2,MLRN,*9,LRN,*18,INVNUM,RUNFLAG:
.                   *26,ordcname," ",*60,M$cd,*80,qtyshp:
.                   *110,paydate:
.                   *L,*1,hpbon,cancprnt,*1,cancprnt,hpboff:
.                   *7,"Using - ",o1des;
         PRINT     *2,MLRN,*9,LRN,*18,INVNUM,RUNFLAG:
                   *26,ordcname," ",*60,M$cd,*80,qtybild:
                   *110,paydate:
                   *L,*1,hpbon,cancprnt,*1,cancprnt,hpboff:
                   *7,"Using - ",o1des;
.end patch 1.2
         cmatch    yes to tapesw
         if        equal
.START PATCH 1.4 REPLACED LOGIC
.         print     *60,ownctn,*80,"Tape",*l
.                  else
.         print    *60,ownctn,*l       
.
	call	Trim using OWNCTN
.START PATCH 1.6 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"PRINT-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.         print     *60,NFULCOMP,*80,"Tape",*l
.                  else
.         print    *60,NFULCOMP,*l       
	if (OWNCTN <> "")
		pack	COMPFLD6, OWNCTN
		rep	zfill,COMPFLD6
		move	C1,COMPPATH
		move	"PRINT-COMPKEY6",Location
		pack	KeyLocation,COMPFLD6
		call	COMPKEY6
		if over
			clear 	COMPFLD6
			clear	COMPCOMP
		else
			if (COMPSVBFLG <> "T")
				clear 	COMPFLD6
				clear	COMPCOMP
			endif
		endif
	else	// OWNCTN = ""
		clear 	COMPFLD6
		clear	COMPCOMP
	endif
	print     *60,COMPCOMP,*80,"Tape",*l
                  else
        print    *60,COMPCOMP,*l 
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.4 REPLACED LOGIC
         endif                
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
         MOVE      MASK92 TO MT$cd
         EDIT      TOTcd TO MT$cd
         PRINT     HPBON,HPITALIC,*1,detail," Records ",b1:
                   "*** TOTAL --> ",*60,MT$CD,HPBOFF,HPUPRGHT
.
         PRINT     *FLUSH
         MOVE      C0 TO DETAIL
         MOVE      C0 TO PAGE        
         RETURN
.
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
.         print      hp17ptch         .compressed
         endif
         PRINT     *f,*n,HPBON,*27,"***  N I N   ",hpitalic,"C o n s u m e r ":
                   hpuprght,"  D i r e c t   A c c r u a l  ***":
                   *116,"DATE: ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"MAILER",*11,"LR":
                   *17,"INVOICE":
                   *26,"MAILER BILL-TO":
                   *52,"------Estimated--------":
                   *80,"------Quantity-------":
                   *104,"----Mlr-PAYMENT-------":
                   *L,*1,"NUMBER",*9,"NUMBER":
                   *18,"NUMBER",*26,"NAME AND THRU":
                   *52,"-------Charges---------":
                   *104,"-------RECEIVED-------":
                   *L,HPBOFF
         MOVE      c6 TO LINES
         RETURN
.
.
EOJ     call       total
EXIT1    STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
.patch2.45
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.45
         INCLUDE   NORD2IO.inc
;begin patch 2.5
;         INCLUDE   NINVIO.inc
         	INCLUDE   	ninvio.inc
	INClude	NInvAcdio.inc
;end patch 2.5
         include   ndatio.inc
         include   nownio.inc
.START PATCH 1.6 REMOVED LOGIC
..START PATCH 1.4 ADDED LOGIC
.	include	nfulio.inc
..END PATCH 1.4 ADDED LOGIC
.END PATCH 1.6 REMOVED LOGIC
         INCLUDE    COMLOGIC.inc


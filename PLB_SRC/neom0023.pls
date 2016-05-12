...............................................................................
.NEOM0023 - list owner income summary report input file prep
...............................................................................
.
PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
         include   consacct.inc
         include   hp.inc
;begin patch 1.6
;         INC       NINVDD.inc
          INC                 ninvdd.inc
;end patch 1.6
.patch1.5
                                        include   compdd.inc
                                        include   cntdd.inc
.         INC       NMLRDD.INC
.patch1.5
         INCLUDE   NmtxDD.INC
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         INCLUDE   GNXTDD.INC
         INCLUDE   NDAT3DD.INC
         include   nslsdd.inc
         INCLUDE   NLOBDD.INC
.begin patch 1.3
         INCLUDE   NJSTDD.inc
.end patch 1.3

         include   npaydd.inc
         include   nmoadd.inc
.
...........................................
release   init      "1.71"        DLH use dsinit
Redate    Init      "2016 May 2"
.release   init      "1.7"        DLH reinstated use of Pareom, add data manager to file opens
.Redate    Init      "01 December 2010"
.release   init                "1.6"        DLH 02MArch2005  Invoice Conversion
.Redate    Init      "02 March 2005"
.release  init      "1.5"        DMB    26MAY2004 Mailer Conversion
.RELEASE  INIT      "1.4"          ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.3"          DLH 24Aug99  NINadj nadjust Y2K, File expansion
.RELEASE  INIT      "1.2"          DLH 27APR99  NININV Y2K, File expansion
.RELEASE  INIT      "1.1"          ASH 20JAN99  NINORD Y2K, File expansion
.release  init      "1.0"          DLH 07Mar96 
.

.runs for report types as one report per owner/list
.pass1 all new billings within the month
.pass2 all billings paid within the month
.pass3 all adjustments to bills within the month
.pass4 all billings still open
.
.primary output file is ownerstm.dat
.needs to be sorted to ownerstm.srt for neom0024
.sorted 12,4,c,a,20,6,c,a,1,1,c,a,6,6,c,a     owner/list/recid/lr
.
..............................................................................
.input file  ninsls.own created by neom0004 with sales register|
........................................................................
Input    file      .;superset of ninsls for current month 
..............................................................................
jstfile  file                        ."nadjust.own",read
outfile  file      uncomp
DUPEOWN  IFILE     KEYLEN=4
OWNKEY   DIM       4    *DUPE OWNER FILE.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
...............................................................................
.nadjust.own - all detail adjustments for the month
.Sorted
.            .inc (75,2,c,ne,"current year",and,71,2,c,ne,"current month")
.           /s (7,6,c,a
...............................................................................
.4th file - ninsls.old   |
..........................
.sales file used is  copy of ninsls 
..............................................................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
DATEPRT1 DIM       8
DATEPRT2 DIM       8
AP1OUT   DIM        12
AP2OUT   DIM       12
AP1FORM  FORM      9
TOTMASK  INIT      "$$$$,$$9.99-"
APMASK   INIT      "$$$$,$$9.99-"
TOTOMSK  INIT      "$$,$$$,$$9.99-"
APCHECK  FORM      "000000001"
tadjmask dim       14
APSW    DIM       1
AP2SW    DIM       1
TAX501   FORM      1
form102  form      10.2
recid    dim       1
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
ppflag   dim       1            'P' if equal else blank
PMASK    DIM       1
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
.START PATCH #1.1 - DUPLICATE VAR IN CONSACCT.INC
.FORM92   FORM      9.2
.END PATCH #1.1 - DUPLICATE VAR IN CONSACCT.INC
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
COUNT2   FORM      5
count3   form      5
count4   form      5
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
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
m$oqty   dim       9
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
APTOTOWN FORM      10       DLH
OWNERMSK DIM       13
arform   form      9
arout    dim       12
arflag   dim       1        from parfile if True print ar
pass     form      1        1=new bills, 2=old bills, 3=old bills, 4=adjustments
LISTCHG  DIM       1
CBLSTNUM FORM      6
CBOWNNUM FORM      4
JSTN     FORM      1
manpay   form      10.2
MANMASK  INIT      "$$,$$$,$$9.99-"
OWNBR    DIM       1
PAYCHK   FORM      1
first    dim       1
CHKDATE  DIM       8
APTOTDET FORM      9
LSTMASK  DIM       13 
ARMASK   dim       12
APTOTLST FORM      9
ARTOTLST form      9
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
.cleanup 12/01/2010
parfile   IFILE     keylen=4
.
.
         MOVE      "Names in the News" TO COMPNME
         MATCH     "NEOM0023" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         MOVE      TODAY TO DATE
         Else  
         MOVE      "NEOM0023" TO PROGRAM
         CLOCK     DATE TO DATE
         ENDIF

         MOVE      "MONTHLY L O  Report Prep" TO STITLE
.START PATCH 1.4 REPLACED LOGIC
.         prepare   outfile,"g:\data\Ownerstm.dat"
.          pack      str35,"\\nins1\e\data\Ownerstm,dat"
          pack      str55,"e:\data\Ownerstm.dat|NINS1:502"
         DISPLAY   *P1:24,*EL,"Preparing Ownerstm.dat ",*W10;
         prepare   outfile,STR55,exclusive
.END PATCH 1.4 REPLACED LOGIC
         move      "ninsls.own|NINS1:502" to nslsname (payables first pass)
         open      jstfile,"nadjust.own|NINS1:502",read
         OPEN      DUPEOWN,"DUPEOWN|NINS1:502",READ
         Open      parfile,"Pareom|NINS1:502"

.       
.CLOCK    CLOCK     DATE TO DATE
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO SYSMO,STR1,SYSDY,STR1,SYSYR
         MOVE      C0 TO PAGE
         CALL      PAINT
         KEYIN     *CL
         move      "Exit" to pf5
         trap      eoj if f5
         CALL      FUNCDISP
.
OPEN     
         trap      eoj if f5
         open      input,"ninsls.own|NINS1:502",read
         move      c1 to nslspath
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
BEGIN
         trap      eoj if f5
.
        
input
.readsls   1st pass
READsls  
         read      input,seq;slsvars
         if        over
         move      c0 to nslsflag
         move      "ninsls.pad|NINS1:502" to nslsname 
         goto      readpaid
         endif
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"New Sales records PROCESSED: ",COUNT,b1,slsown,b1,slslist
.begin patch xxx
         Clear     str4
         Move      slsown to str4
         Rep       zfill in str4
         Read      parfile,str4;str4,arflag                  .on the list????
         Goto      readsls if over                   .no
.end patch xxx

         move      "A" to recid
         write     outfile,seq;recid,slsvars
         goto      readsls
.
.............................................................................
.readpaid - 2nd pass
readpaid
         call      nslsseq
         goto      readadj if over      
         ADD       C1 TO count3
         DISPLAY   *P10:13,"New payment records PROCESSED: ",count3,b1,slsown,b1,slslist
         move      "B" to recid
.begin patch xxx
         Clear     str4
         Move      slsown to str4
         Rep       zfill in str4
         Read      parfile,str4;str4,arflag                  .on the list????
         Goto      readpaid if over                   .no
.end patch xxx
         write     outfile,seq;recid,slsvars
         goto      readpaid
.............................................................................
.readadj - 3rd pass
readadj
         read      jstfile,seq;jstvars
         if        over
         close     nslsfile
         move      c0 to nslsflag
         move      "ninsls.old|NINS1:502" to nslsname
         goto      readsls1
         endif
         add       c1 to count4
         move      jstlr to nordfld
         rep       zfill in nordfld
         call      nordkey
         DISPLAY   *P10:14,"Adjustment records PROCESSED: ",count4,b1,olon,b1,olnum
.begin patch xxx
         Clear     str4
         Move      olon to str4
         Rep       zfill in str4
         Read      parfile,str4;str4,arflag                  .on the list????
         Goto      readadj if over                   .no
.end patch xxx

         move      jstlr to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         move      "C" to recid
         move       jstmlr to slsmlr
         move       jstlr to slslr
OWNPREP  MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF
         move       olon to slsown
         move       olnum to slslist
         move       cobn to slscnt
         move       obrkguar to slsguar
.Start patch #1.1 - increased var
.         move       c0 to str6
.         pack       str6 from omdtem,omdted,omdtey
.         move       str6 to slsmdte
         MOVE       C0,STR8
         pack       STR8 from omdtec,omdtey,omdtem,omdted
         MOVE       STR8,SLSMDTE
.End patch #1.1 - increased var
         move       wsjpc to slsdjcd
         move       adjc to slsadjcd
         move       jstcd to slscode
         move       c0 to str6
         pack       str6 from chk1dtem,chk1dted,chk1dtey
         move       str6 to slschkdte
.
.Start patch #1.1 - increased var
.         move       c0 to str6
.         pack       str6 from invdtem,invdted,invdtey
.         move       str6 to slsidte
         move       c0 to str8
         pack       str8 from CC,INVDTEY,invdtem,invdted
         move       str8 to slsidte
.END patch #1.1 - increased var
         clear     slscname
         move      JSTSUBNO to slsadjsw          .is this var big enough????
         move      c0 to slsxchrg
.begin patch 1.3

.         move      jstap1 to cvtfld
.         call       cvt
         move       c0 to slsap1
.         move       cvtfld to slsap1
         move       jstap1 to slsap1
.         
.         move       c0 to cvtfld
.         move      jstap2 to cvtfld
.         call       cvt
         move       c0 to slsap2
.         move       cvtfld to slsap2
         move       jstap2 to slsap2
.
.         move       c0 to cvtfld
.         move      jstar to cvtfld
.         call       cvt
         move       c0 to slsar
.         move       cvtfld to slsar
         move        jstar to slsar
.end patch 1.3

.
         move       o1des to slslst1
.
         COMPARE   C0 TO SLSAP1
         IF        EQUAL
         COMPARE   C0 TO SLSAP2
         GOTO      READADJ IF EQUAL
         ENDIF
.
         write      outfile,seq;recid,slsvars
         goto      readadj
...............................................................................
.readsls1 - read sales file and get all  ninsls.old 4th pass
READsls1  
         CALL      Nslsseq
         if        over
         weof      outfile,seq
         close     outfile
          Shutdown  "cls"
         stop
         endif
         DISPLAY   *P10:15,"NUMBER OF Sales records PROCESSED: ",COUNT2,b1,slsown,b1,slslist
.begin patch xxx
         Clear     str4
         Move      slsown to str4
         Rep       zfill in str4
         Read      parfile,str4;str4,arflag                  .on the list????
         Goto      readsls1 if over                   .no
.end patch xxx
         move     "D" to recid              .open
         write    outfile,seq;recid,slsvars 
         ADD       C1 TO COUNT2
         goto     readsls1
*............................................................
.begin patch 1.3
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
.end patch 1.3
..........................................................................
EOJ      Shutdown
          STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
.begin patch 1.2
.         include   compute.inc
.end patch 1.2
.patch1.5
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.5
         INCLUDE   NORDIO.INC
;begin patch 1.6
;         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
;end patch 1.6
         include   nownio.inc
         INCLUDE   NmtxIO.inc
         INCLUDE   NDAT3IO.inc
         INCLUDE   GNXTIO.inc
         include   nmoaio.inc
         INCLUDE   NLOBIO.INC
         include   nslsio.inc
.begin patch 1.3
         INCLUDE   NJSTIO.inc
.end patch 1.3
         include   npayio.inc
         INCLUDE   COMLOGIC.inc


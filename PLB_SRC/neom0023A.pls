...............................................................................
.NEOM0023a - Rerun of list owner income summary report input file prep
...............................................................................
.
PC        EQU       0
          INC       COMMON.INC
          INCLUDE   CONS.INC
          include   consacct.inc
          include   hp.inc
          INC       ninvdd.inc
          include   compdd.inc
          include   cntdd.inc
         INCLUDE   NmtxDD.INC
         INCLUDE   NORDDD.INC
         include   nowndd.inc
         INCLUDE   GNXTDD.INC
         INCLUDE   NDAT3DD.INC
         include   nslsdd.inc
         INCLUDE   NLOBDD.INC
         INCLUDE   NJSTDD.inc

         include   npaydd.inc
         include   nmoadd.inc
.
         INCLUDE   NADJDD.inc
         include   nmrgdd.inc
          Include   NInvAcddd.inc
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
         include   nmlddd.inc



...........................................
release   init                "1.0"        DLH New allow adhoc run for any date period
Reldate   Init      "01 February 2010"
.
SLSFILE  FILE

.runs for report types as one report per owner/list
.pass1 all new billings within the month
.pass2 all billings paid within the month
.pass3 all adjustments to bills within the month
.pass4 all billings still open
.
.primary output file is ownerstm.adhoc
.needs to be sorted to ownerstm.srt for neom0024
.sorted 12,4,c,a,20,6,c,a,1,1,c,a,6,6,c,a     owner/list/recid/lr
.
..............................................................................
.First sort out records desired
........................................................................
Input    file      ;superset of ninsls for current month 
..............................................................................
jstfile  file                        ."nadjust.own",read
outfile  file      uncomp
File      FIle
DUPEOWN  IFILE     KEYLEN=4
OWNKEY   DIM       4    *DUPE OWNER FILE.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
...............................................................................
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
brker    dim       1
FORM9A    FORM      9
Branch    Form      1
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
CountOP   form      6         .open invoice count      
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
mrgsw    dim       1
shipsw   dim       1
adjflag   dim       1           default " " or "Y"  for new invoices = "N"
.
         MOVE      "Names in the News" TO COMPNME
.         MOVE      "NEOM023A" TO PROGRAM
         MOVE      "ADHOC L O  Report Prep" TO STITLE
         PACK      STR35,NTWKPATH1,"Ownerstm.adhoc"
         prepare   outfile,STR35,exclusive
         OPEN      DUPEOWN,"DUPEOWN",READ
.       
CLOCK    
          if        (INPNAME <> "NEOM0023A")      
          CLOCK     DATE TO DATE
          else
          move      today to Date
          endif
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
.         open      parfile,"pareom"
         move      c1 to nslspath
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
         move       c3,nordlock         .turn off filepi's
         move      c3,ninvlock
         move       c3,complock
OPENREST TRAP      IO GIVING ERROR IF IO
         TRAPCLR   IO
         MOVE       C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
BEGIN
         trap      eoj if f5
.sort 'NEW' Invoices.
         DISPLAY   *P10:09,"Sorting 'new' invoices: "

          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.New;7-12,S=#"132=#'",sysYr,"#'&134=#'",sysmo,"#'#""
          Sort      Taskname,SunDM="NINS1:502"

.sort 'PAID' Invoices.
         DISPLAY   *P10:10,"Sorting 'paid' invoices: "
          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.paid;7-12,S=#"217=#'",sysYr,"#'&219=#'",sysmo,"#'#""
          Sort      Taskname,SunDM="NINS1:502"

.sort 'NEW' adjustments
         DISPLAY   *P10:11,"Sorting 'new' adjustments: "
          pack      str4 from Sysyr,sysmo
          rep       zfill in str4
.          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127=#'",sysYr,"#'&129=#'",sysmo,"#'#""
          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127='",str4,"'#""
          Sort      Taskname,SunDM="NINS1:502"
.        
.for opens  read the master invoice file invoice date must be <= selected MM YY  paid date must be > selected MM YY
         DISPLAY   *P10:12,"Looking for 'open' invoices: "
         pack       str4 from sysyr,sysmo
.          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\nininv.open;7-12,S=#"217=<",STR4,"'&219>#'",sysmo,"#'#""
.          Sort      Taskname,SunDM="NINS1:502"
         move       c0,n9
         move       c0,n8
         move       c2,NINVPATH
          packkey      Ninvfld,"000000"
          call      Ninvtst
          Prepare   SlsFile,"\\nins1\e\data\ninsls.open|NINS1:502",exclusive
Loop      Loop
          call      Ninvks
          until     over
          add       c1,CountOP 
          DISPLAY   *P40:12,CountOP,b1,invnum
.          if        (invnum = "576649")
.          call      debug
.          endif
          pack      str4 from Sysyr,sysmo
          rep       Zfill in str4
          move      c0,n4
          move      str4,n4
          pack      str4 from INvdtey,invdtem
          rep       Zfill in str4
          move      c0,n5
          move      str4,n5
          pack      str4 from CHK1DTEY,CHK1DTEM
          rep       Zfill in str4
          move      c0,n6
          move      str4,n6
.check dates  if valid the rest of info and call the write
          if        ((n5 <= N4 & n4 < n6) or (n5 <= N4 & n6 = C0))
          Call      Getdata
.                    if        (Olon = "7189" or Olon = "6018" or Olon = "0390")
.4352 2136 student conserv                                                                                               

.                    if        (Olon = "7189" or Olon = "5716" or Olon = "5799" or Olon = "5851" or Olon = "4352" or Olon = "2136" or OLNUM = "028834")
                    CMATCH    YES TO APSW
                              if        equal
                              call      Prepsls
.                              call      debug
                              write     slsfile,seq;slsvars
                              add       c1,n8
                              DISPLAY   *P68:12,n8
                              endif                  
.                    endif                    
          endif
          if        (CountOP = "999999")
          Break
          endif
          Repeat      

         WEOF      SLSFILE,SEQ
         CLOSE     SLSFILE
.then create the files


input
.1st pass
          open      FIle,"\\nins1\e\data\nininv.new|NINS1:502"
          move      c1,pass
READSls   Read      File,seq;Invvars
          if        over
          add       c1,pass
          open      FIle,"\\nins1\e\data\nininv.paid|NINS1:502"
.reset flag
          move      yes,adjflag
         goto      readpaid
         endif

         ADD       C1 TO COUNT
         DISPLAY   *P10:13,"New Sales records PROCESSED: ",COUNT,b1,slsown,b1,slslist
.do not take adjustments for these
          move      No,adjflag
          call      getdata
          call      prepsls
         move      "A" to recid
         write     outfile,seq;recid,slsvars
         goto      readsls
.
.............................................................................
.readpaid - 2nd pass
readpaid
          Read      File,seq;Invvars         
.          call      nslsseq
          if        over
          open      FIle,"\\nins1\e\data\nadjust.new|NINS1:502"
         goto      readadj
         endif
         ADD       C1 TO count3
         DISPLAY   *P10:14,"New payment records PROCESSED: ",count3,b1,slsown,b1,slslist
          call      getdata
          call      prepsls
         move      "B" to recid
         write     outfile,seq;recid,slsvars
         goto      readpaid
.............................................................................
.readadj - 3rd pass
readadj
          Read      File,seq;jstvars
.         read      jstfile,seq;jstvars
         if        over
          Open      slsFIle,"\\nins1\e\data\ninsls.open|NINS1:502"
         goto      readsls1
         endif

         add       c1 to count4
         move      jstlr to nordfld
         rep       zfill in nordfld
         call      nordkey
         DISPLAY   *P10:14,"Adjustment records PROCESSED: ",count4,b1,olon,b1,olnum
         move      jstlr to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         move      "C" to recid
         move       jstmlr to slsmlr
         move       jstlr to slslr

          MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF
         move       olon to slsown
         move       olnum to slslist
         move       cobn to slscnt
         move       obrkguar to slsguar
         MOVE       C0,STR8
         pack       STR8 from omdtec,omdtey,omdtem,omdted
         MOVE       STR8,SLSMDTE
         move       wsjpc to slsdjcd
         move       adjc to slsadjcd
         move       jstcd to slscode
         move       c0 to str6
         pack       str6 from chk1dtem,chk1dted,chk1dtey
         move       str6 to slschkdte
.
         move       c0 to str8
         pack       str8 from CC,INVDTEY,invdtem,invdted
         move       str8 to slsidte
         clear     slscname
         move      JSTSUBNO to slsadjsw          .is this var big enough????
         move      c0 to slsxchrg
         move       c0 to slsap1
         move       jstap1 to slsap1
.         
         move       c0 to slsap2
         move       jstap2 to slsap2
         move       c0 to slsar
         move        jstar to slsar
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
.          call      debug
         Read       slsfile,seq;slsvars
         if        over
         weof      outfile,seq
         close     outfile
          Shutdown
         stop
         endif
         move     "D" to recid              .open
         write    outfile,seq;recid,slsvars 
         ADD       C1 TO COUNT2
         DISPLAY   *P10:15,"NUMBER OF Sales records PROCESSED: ",COUNT2,b1,slsown,b1,slslist
         goto     readsls1
*.--------------------------------------------------------------------------------------------------
GetData
         MOVE      C0 TO FORM9A
         MOVE      QTYbild TO FORM9A
         move      ppm to form32
.
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         type      obrknum
         if        equal
         PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         CALL      NBRKKEY
         move      brcomp,sLSCNAME
         move      yes to brker
         endif
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.
          pack      NMLDFLD1,"01X",LRN
        clear   str8
          pack      str8,"99999999"
          call      NMLDAIM
          loop
                    until over
                    if (NMLDDATE < str8)
                              move      NMLDDATE,str8
                    endif
                    call      NMLDKG
          repeat
          if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
          else
.Use current Mail Date
          endif

         MOVE      OLON TO NOWNFLD    .NEED INFO FOR BILLING. 11/94
         CALL      NOWNKEY
.
.
         MOVE      YES TO SUBPPSW
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
         call      wipecvars
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         move      lrn to nshpfld
         call      nshpkey
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
         move      c0 to ar
         move      formar to ar
         move      ap to ap1
.
OWNPREP  MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF

         MOVE      C0 TO BRANCH
         MOVE      B1 TO guarpay
         MOVE      GUARCODE TO BRANCH
         BRANCH    BRANCH OF OGUAR1,OGUAR2,OGUAR3,prepayd,prepayd,prepayd:
                   prepayd,prepayd,prepayd
         GOTO      chkadj
OGUAR1   MOVE      STAR TO GUARpay
         GOTO      chkadj
OGUAR2   MOVE      STAR TO GUARpay
         GOTO      chkadj
OGUAR3   MOVE      STAR TO GUARpay
.
prepayd
          if        (pass = c1)
          goto     output
          endif
.chkadj --- need to read details and only pull if prior or in date range          
CHKADJ
.          MOVE      LRN TO NADJFLD
.         REP       ZFILL IN NADJFLD
.         CALL      NADJKEY
.         GOTO      OUTPUT IF OVER
..
..
.          add      asrecadj to ar
...
.         add        aslrinc to lrinc
.
.         add       aspayad1 to ap1
..
.         add       aspayad2 to ap2
          if        (adjflag <> no)
          move      c1,n2
DETADJ2  
          if        (invnum = "566236")
          call      debug
          endif
         clear     njstfld
          move      N2,str2
         rep       zfill in str2
          PACKkey      NJSTFLD FROM INVNUM,str2
         CALL      NJSTKEY
         GOTO      Output if over
          pack      str4 from Sysyr,sysmo
          rep       Zfill in str4
          move      str4,n4
          unpack    JSTDATE into str2,str4,str2
          rep       Zfill in str4
          move      str4,n5          
.check dates  
          if        (n5 <= N4)
          add        jstap1 to ap1
          add        jstap2 to ap2
          add       Jstar to ar
          add       Jstlrinc to lrinc
          endif
          add       c1,n2
        goto       Detadj2
          endif
.
.
OUTPUT
         CMATCH    "P" TO STATB
         if        equal
         move      "P" to ascrdb
         else
         move      "O" to ascrdb
         endif
         MOVE      C0 TO FORM102
         MOVE      C0 TO FORM102
         MOVE      ap1 TO FORM102

         MOVE      NO TO APSW
         COMPARE   C0 TO FORM102
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
          cmatch    yes to brker
          if        not equal
          unpack    mslsper into osales10,osales
          else
                              if       (brsales= "06" or brsales = "27" or brsales = "28")
          unpack    brsales into osales10,osales
                              else
          unpack    mslsper into osales10,osales
                              endif
          endif
.
          match     "00" to mslsper
          if        equal
          unpack    brsales into osales10,osales
          endif
         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
         if         equal            .yes
         move      "    " to obrknum
         move      "   " to obrkcnt            03/31/05 JD
         endif
.
         move      no to brker
         move      Mcomp,sLSCNAME
          return
*............................................................
PrepSls
          move      co,slsmlr
          clear     Slslr
          move      co,slsown
          clear     slsguar1
          clear     slscnt
          move      c0,slslist
          move      c0,slsmdte
          move      c0,slsap1
          clear     slsdjcd
          clear     slsadjcd
          move      c0,slsidte
          move      c0,slsap2
          clear     slslst1
          clear     slscode
          clear     slsguar
          move      c0,slsar
          clear     slschkdte
          clear     slscname
          move      c0,slsadjsw
          move      c0,slsxchrg
.
          Move      mlrn,slsmlr
          move      lrn,slslr
          move      olon,slsown
          move      Obrkguar,slsguar1
          move      cobn,slscnt          
          move      olnum,slslist
          clear     str8
          pack      str8 from OMDTEC,OMDTEY,OMDTEM,OMDTED
          move      str8,slsmdte
          move      ap1,slsap1
          move      wsjpc,slsdjcd
          move      adjc,slsadjcd
          clear     str8
          pack      str8 from invdtec,INVDTEY,INVDTEM,INVDTED
          move      str8,slsIdte
.          move      Form102,slsap2
          move        ap2,slsap2 
          move      o1des,slslst1
          move      ascrdb,slscode
          move      guarpay,slsguar
          move      ar,slsar
          clear     str8
          pack      str8 from chk1dtec,chk1dtey,chk1dtem,chk1dted          
          move      str8,slschkdte
.
.          Move     CHKN1
.


          return
*............................................................
WriteSls            WRITE     FILE,SEQ;MLRN:            ..001-004
                   LRN:                          ..005-010
                   OLON:                         ..011-014
                   OBRKGUAR:                     ..015-015
                   COBN:                         ..016-018
                   OLNUM:                        ..019-024
                   OMDTEC,OMDTEY,OMDTEM,OMDTED:  ..025-032
                   *ZF,ap1:                       ..033-045
                   WSJPC:                        ..046-046
                   ADJC:                         ..047-047
                   invdtec,INVDTEY,INVDTEM,INVDTED:  ..048-055
                   *ZF,form102:                      ..056-068
                   O1DES:                            ..069-103
                   ASCRDB:                           ..104-104
                   GUARPAY:                          ..105-105
                   ar:                               ..106-118
                   chk1dtec:                         ..119-120
                   chk1dtey:                     .     121-122
                   chk1dtem:                    .      123-124
                   chk1dted:                       .   125-126
                   SLSCNAME                     .      127-151
          Return
..........................................................................
EOJ       Shutdown
          STOP
IO       TRAPCLR   IO
         DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
         STOP
          include   compio.inc
          include   cntio.inc
         INCLUDE   NORDIO.INC
          INCLUDE             ninvio.inc
         include   nownio.inc
         INCLUDE   NmtxIO.inc
         INCLUDE   NDAT3IO.inc
         INCLUDE   GNXTIO.inc
         include   nmoaio.inc
         INCLUDE   NLOBIO.INC
         include   nslsio.inc
         INCLUDE   NJSTIO.inc
          include  compute.inc
         INCLUDE   NADJio.inc
         include   nmrgio.inc
          Include   NInvAcdio.inc
         include   nshpio.inc
         include   ndatio.inc
         include   nacdio.inc
         include   nmldio.inc
         include   npayio.inc
         INCLUDE   COMLOGIC.inc


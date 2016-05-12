;..............................................................................
;NEOM024W - list owner income summary report
;
;  Andrew Harkins
;  December 17, 2004
;
;  This program is a copy of NEOM0024.PLS.  This copy creates single copies
;  of the Owner Activity Report breaking on Owner/List.  These individual copies
;  will then be placed on our website.
;
;  This program does not do the updates or printing options available in NEOM0024!!
;
;  This program runs alongside NEOM24AW.PLS.  However, this program MUST run first as it cleans up the destination directory!!
;       Assumed sequence of program execution:
;               NEOM0024
;               NEOM024A
;               NEOM024W
;               NEOM24AW
;..............................................................................
;
PC       EQU       0
          include   COMMON.INC
          include   CONS.INC
          include   consacct.inc
          include   hp.inc
          include   NINVDD.inc
          include   compdd.inc
          include   cntdd.inc
          include   NmtxDD.INC
          include   NORDDD.INC
          include   nowndd.inc
          include   GNXTDD.INC
          include   NDAT3DD.INC
          include   nslsdd.inc
          include   nmrgdd.inc
          include   NLOBDD.INC
          include   NJSTDD.inc
shipsw    dim       1
mrgsw     dim       1
          include   npaydd.inc
          include   nmoadd.inc
          include   nshpdd.inc
          include   ndatdd.inc
          include   nacddd.inc
          Include   PrtPagedd.inc
          include   ninvacddd.inc
release       init            "1.2"           26JAN2006 DMB added code for data folder restructure
.release  init      "1.1"     ASH 25FEB2005  Added logic to test for empty Input file
.release  init      "1.0"          ASH 14DEC2004  New version of NEOM0024 which breaks by Owner+List
;
;primary input file is pareom.own
;.............................................................................
input     file      uncomp
;.......................................................................
;parfile - hold all exclusive list owner numbers and flag for a/r info on the
;          report
parfile   ifile     keylen=4
.OUTPUT   FILE      UNCOMP
;..............................................................................
;CLOCK    FUNCTION
;.......................
DATE      DIM       8
SYSMO     DIM       2
SYSDY     DIM       2
SYSYR     DIM       2
;
DATEMASK INIT       "XX/XX/XX"
DATEPRT1 DIM        8
DATEPRT2 DIM        8
AP1OUT    DIM       15
ADJAPOUT DIM        12
AP2OUT    DIM       15
aplotus   form      10.2
aplotown form       10.2
AP1FORM   FORM      10.2
AP2FORM   FORM      10.2
aplotmsk INIT       "ZZZZZZZZZZ99-"
TOTMASK   INIT      "$$,$$$,$$9.99-"
APMASK    INIT      "$$,$$$,$$9.99-"
ARMASK    INIT      "$,$$$,$$$,$$9.99-"
adjAPMSK inIT       "$$$$,$$9.99-"
APMASK2   INIT      "$$,$$$,$$9.99-"
TOTOMSK   INIT      "$$,$$$,$$9.99-"
APCHECK   FORM      "000000001"
tadjmask dim        17
APSW      DIM       1
AP2SW     DIM       1
TAX501    FORM      1
form102   form      10.2
incount   form      5
.flatflg  dim       1
;.............................................................................
;adjustment reason code descriptions
;
nadjtext DIM       35
adjres1   init      "Adjustment to Quantity"
adjres2   init      "Shipping"
adjres3   init      "Selection fee"
adjres4   init      "Running Charges"
adjres5   init      "Change in price"
adjres6   init      " "
adjres7   init      "Adjust Within A/P "
adjres8   init      "Adjust A/R & LR "
adjres9   init      "Adjust A/P & LR"
adjres10 init       "Cancel entire Bill"
adjres11 init       "No invoice A/P to Lr "
adjres12 init       "Late Lo inv LR to A/P "
adjres13 init       "Adjustment of Income"
adjres14 init       "Advance Payment to LO"
adjres15 init       "Adjustment of Tax"
adjres16 init       "Short Payment"
adjres17 init       "Commission"
adjres18 init       "Postage"
adjres19 init       "Direct Payment to LO"
adjres20 init       " "
adjres21 init       "Advance Payment to LO"
adjres22 init       "Reduction of A/R"
adjres23 init       "Reduction of A/P (Contra)"
adjres24 init       "Discount Earned"
adjres25 init       "Additional Billing"
adjres26 init       "Write off of A/R"
adjres27 init       "Prepayment"
adjres28 init       "Write off of A/P"
adjres29 init       "                           "
adjres30 init       "Taking Credit-Original open"
adjres31 init       "Credit Transfer"
adjres32 init       "Refund Credit Taken"
adjres33 init       "Cancelled/Billing Adjusted "
adjres34 init       "Adj due to Order Change    "
adjres35 init       "Court Imposed Bankruptcy Charge"
adjres36 init       "Bankruptcy, Un-collectible A/R "
Adjres37 Init       "Void Check"

adjres99 init       "Entry Correction"
;
;FILES.
;..............................................................................
;
;
; WORK VARIABLES
;.............................................
;
XFootFlag Init      "Y"       ;if set to No account did not balance
;
ELEVEN    FORM      "11"
FIFTY1    FORM      "51"
TYPIST    DIM       2
;
TOTARp    FORM      10.2      *prepaid
TOTpMOA   FORM      10.2      *MOA Applied to prepaid
TOTAR     FORM      10.2
TOTAP1    FORM      10.2
TOTAP2    FORM      10.2
TOTAP     FORM      10.2
TOTNIN    FORM      10.2
TOTLR     FORM      10.2
TOTSTAX   FORM      10.2
TOTCTAX   FORM      6.2
TOTPOST   FORM      5.2
; TRIPLEX BILLING VARIABLES.
;
TDMCLIST INIT       "005051"  LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
;
;END TDMC.
;
LRMRINC   FORM      10.2      TOTAL MANAGEMENT/RENTAL LR INCOME
LRMEINC   FORM      10.2      TOTAL MANAGEMENT/EXCHANGE LR INCOME
LRMINC    FORM      10.2      TOTAL MANAGEMENT LR INCOME.
LRBRINC   FORM      10.2      TOTAL BROKERAGE/RENTAL LR INCOME
LRBEINC   FORM      10.2      TOTAL BROKERAGE/EXCHANGE LR INCOME
LRUNKN    FORM      10.2      UNKNOWN LR INCOME.
LRBBE     FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH EXCH.
LRBBR     FORM      10.2      TOTAL BATCH BILL LR FROM PREV MONTH RENT.
;
ARMR      FORM      10.2      TOTAL MANAGEMENT/RENTAL  A/R
ARME      FORM      10.2      TOTAL MANAGEMENT/EXCHANGE  A/R
ARM       FORM      10.2      TOTAL MANAGEMENT A/R
ARBR      FORM      10.2      TOTAL BROKERAGE/RENTAL  A/R
ARBE      FORM      10.2      TOTAL BROKERAGE/EXCHANGE  A/R
ARUNKN    FORM      10.2      UNKNOWN  A/R.
ARBBE     FORM      10.2      TOTAL BATCH BILL A/R EXCH PORTION
ARBBR     FORM      10.2      TOTAL BATCH BILL A/R RENT PORTION
;
APMR      FORM      10.2      TOTAL MANAGEMENT/RENTAL A/P
APME      FORM      10.2      TOTAL MANAGEMENT/EXCHANGE A/P
APM       FORM      10.2      TOTAL MANAGEMENT A/P
APBR      FORM      10.2      TOTAL BROKERAGE/RENTAL A/P
APBE      FORM      10.2      TOTAL BROKERAGE/EXCHANGE A/P
APUNKN    FORM      10.2      UNKNOWN A/P.
APBBE     FORM      10.2      TOTAL BATCH BILL A/P EXCH PORTION
APBBR     FORM      10.2      TOTAL BATCH BILL A/P RENT PORTION
;
ppflag    dim       1         'P' if equal else blank
PMASK     DIM       1
;
FORM2     FORM      2
FORM22    FORM      2.2
FORM7     FORM      7
FORM9     FORM      9
FORM52    FORM      5.2
FORM11    FORM      11
NUM10     FORM      10
COUNT     FORM      5         total reads input file
COUNTA    FORM      5         new invoices
COUNTB    FORM      5         new payments
countC    form      5         new adjs
countD    form      5         current open bills
CO        FORM      1
BATCHBR   FORM      1         "0" =NO, "1" = YES.
RENTSW    FORM      1         "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR   FORM      2
SALESNUM DIM        2
TEAM1     INIT      "01"      SUSAN
TEAM2     INIT      "02"      ELAINE
TEAM3     INIT      "03"      LIST MANAGEMENT
;..............................................................................
;PRINT MASK VARIABLES
;
MASK22    INIT      "ZZ.ZZ"
MASK42    INIT      "Z,ZZZ.ZZ-"
MASK72    INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52    INIT      "ZZ,ZZZ.ZZ-"
MASK62    INIT      "ZZZ,ZZZ.ZZ-"
MASK7     INIT      "Z,ZZZ,ZZZ"
MASK9     INIT      "ZZZ,ZZZ,ZZZ"
;
M$RTAX    DIM       5         *RETURN-TO TAX PERCENT
M$AR      DIM       13
M$ARp     DIM       13        *prepaid
M$PPM     DIM       6
m$oqty    dim       11
M$QTY     DIM       11
M$AP1     DIM       13
M$AP2     DIM       13
M$STAX    DIM       8
M$CTAX    DIM       8
M$POST    DIM       6
M$LRINC   DIM       13
M$NINC    DIM       13
M$GROSS   DIM       13
;
MT$AR     DIM       15
MT$ARP    DIM       15
MT$pMOA   DIM       15        *prepaid
MT$AP1    DIM       15
MT$AP2    DIM       15
MT$STAX   DIM       15
MT$CTAX   DIM       10
MT$POST   DIM       9
MT$LRINC DIM        15
MT$NINC   DIM       15
;
NEW       FORM      5
REPRINT   FORM      5
PAGE      FORM      4
innets    dim       1
rectype   dim       1
holdid    dim       1
holdown   form      4
holdlist form       6
holdlst1 dim        35
HOLDREC   DIM       10
newMLR    FORM      4       1-4     MAILER NUMBER.            NIN
newLR     FORM      6       5-10    LIST RENTAL NUMBER.       NIN
newOWN    FORM      4      11-14    LIST OWNER NUMBER.        NIN
newGUAR1 DIM        1      15-15    OUTSIDE GUARANTY          NIN
newCNT    FORM      3      16-18    BROKER NUMBER.            NIN
newLIST   FORM      6      19-24    LIST NUMBER.              NIN
newMDTE   FORM      6      25-30    MAIL DATE.                NIN
newAP1    FORM      7      31-37    ACCOUNT PAYABLE ONE.      NIN
newDJCD   DIM       1      38-38    DOW JONES CODE.           NIN
newADJCD DIM        1      39-39    ADJUSTMENT CODE.          NIN
newIDTE   FORM      6      40-45    INVOICE DATE.             NIN
newAP2    FORM      9      46-54    ACCOUNTS PAYABLE TWO.     NIN
newLST1   DIM       35     55-89    LIST DESCRIPTION ONE.     NIN
newCODE   DIM       1      90-90    CREDIT/DEBIT CODE,'C or D'NIN
newGUAR   DIM       1      91-91    GUARANTY CODE.            NIN
newAR     DIM       8      92-99    A/R (NO DECIMAL)
newCHKDTE DIM       6     100-105   INV CHECK DATE
newCNAME DIM        25    106-130   CLIENT NAME
newadjsw form       1     131-131   adjustment switch 2-adjusted
newxchrg form       7.2
;
hldarflg dim        1
DATEKEY   DIM       6
TAXPRT    INIT      "       "
APTOTOWN FORM       10.2       DLH
APTOWNck FORM       10.2       DLH
APTOTWNX FORM       10.2       jd
APTOTWNR FORM       10.2       jd
OWNERMSK DIM        17
OWNRMSKX DIM        17
OWNRMSKR DIM        17
arform    form      10.2
arout     dim       17
arflag    dim       1         from parfile if True print ar
pass      form      1         1=new bills, 2=old bills, 3=old bills, 4=adjustments
LISTCHG   DIM       1
CBLSTNUM FORM       6
CBOWNNUM FORM       4
JSTN      FORM      1
manpay    form      10.2
MANMASK   INIT      "$,$$$,$$$,$$9.99-"
OWNBR     DIM       1
PAYCHK    FORM      1
first     dim       1
CHKDATE   DIM       8
APTOTDET FORM       10.2
LSTMASK   DIM       17
APTOTLST FORM       10.2
ARTOTLST form       10.2
PAYKEY    DIM       5
adjap     form      7.2
adjap1    form      7.2
adjap2    form      7.2
aplist    form      10
lastrec   init      "F"       'T'=true
newflag   init      "F"       'T'=true no previous balance
listbrk   init      "F"
thisown   form      4
hit       form      5         0=no records for this owner/list
exflag    init      "F"
exerflag init       "N"
splpass   form      "0"
carebrk   init      "N"
splname   dim       45
FirstRec form       1

.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          MOVE      "Names in the News" TO COMPNME
          MOVE      "MONTHLY OAR for Website" TO STITLE
.         PACK      STR35,"c:\work\loact.tmp"
.         PACK      STR45,"c:\work\Ownerstm.LST"
.         prepare   output,STR35
CHOOSE
          CALL      PAINT
;
CLOCK
          CLOCK     DATE TO DATE
          MOVE      DATE TO DATEMASK
          UNPACK    DATE INTO SYSMO,STR1,SYSDY,STR1,SYSYR
          MOVE      C0 TO PAGE
          CALL      PAINT
          KEYIN     *CL
          move      "Exit" to pf5
          trap      eoj if f5
          CALL      FUNCDISP
          MOVE      DATEMASK TO TODAY
.
          loop
                    MOVE      YES TO STR1
                    KEYIN     *P20:12,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,STR1
                    if (str1 = YES)
                              break
                    endif
                    KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,SYSMO,"/":
                              *DE,*JR,*ZF,*RV,SYSDY,"/",*DE,*JR,*ZF,*RV,SYSYR
                    PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,SYSYR
                    MOVE      DATE TO DATEMASK
                    MOVE      DATEMASK TO TODAY
                    CALL      PAINT
                    CALL      FUNCDISP
          repeat
OPEN
..............................................
.         pack      str8,"own",sysmo,sysyr
.         pack      splname,"c:\work\",str8,".lst"
.         PRTOPEN   Laser,"\\SRV2008a\laser6",str8,noprint,spoolfile=splname
.         CALL      PRTFORM
..............................................
          open      parfile,"pareom"
          move      sysmo to n2
          sub       c1 from n2
          compare   c0 to n2
          if equal          .its jan last month was dec set key accordingly
                    move      sysyr to n2
                    sub       c1 from n2
                    move      n2 to str2
.
                    pack      str4,CC,SYSYR
                    move      str4,N4
                    sub       C1,N4
                    move      N4,str4
.
                    pack      DATEKEY,str4,"12"
          else
                    move      n2 to str2
                    pack      DATEKEY from cc,sysyr,str2
          endif
          rep       zfill in datekey
.For Testing!!
.         open      input,"c:\work\ownerstm.srt"
.START PATCH 4/1/2005 REPLACED LOGIC ASH
.         open      input,"ownerstm.srt"
          open      input,"\\nins1\E\DATA\ownerstm.srt"
.END PATCH 4/1/2005 REPLACED LOGIC ASH
          move      c1 to nslspath
          move      c1 to ndatpath
          MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
          MOVE      C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
OPENREST
          TRAP      IO GIVING ERROR IF IO
          TRAPCLR   IO
          MOVE      C1 TO NORDPATH     .SET ACCESS TO ISI BY LR#.
BEGIN
          trap      eoj if f5
          move      c0 to holdown
input
READsls
          read      input,seq;rectype,slsvars
          if over
.START PATCH 1.1 ADDED LOGIC
.Test to see if Input file was empty
                    if (count = C0)
                              shutdown
                    endif
.END PATCH 1.1 ADDED LOGIC
                    move      "T" to lastrec
                    GOTO TOTold
EOJ
                    call      CloseFile
                    pause     "180"
.Clean up and then copy over all the new PDF files
                    clear     taskname
                    call      getwinver   ;make sure we have osflag
.This program runs alongside NEOM024AW.PLS.  However, this program MUST run first as it cleans up the destination directory!!
                    If (osflag = c1 | osflag =C5)
                              append    "!c:\winnt\system32\cmd.exe",taskname
                    elseif              (osflag = C3 | osflag =C4)
                              append    "!c:\command.com",taskname
                    elseif              (Osflag = C6)
                              append    "!c:\windows\system32\cmd.exe",taskname
                    endif
.Patch 1.2                    
.                   append    " /c del \\nins1\e\data\weboar\*.pdf",taskname
                    append    " /c del \\nins1\e\storage\weboar\*.pdf",taskname
.Patch 1.2                    
                    reset     taskname
                    execute   taskname
.
                    clear     taskname
                    If (osflag = c1 | osflag =C5)
                              append    "!c:\winnt\system32\cmd.exe",taskname
                    elseif              (osflag = C3 | osflag =C4)
                              append    "!c:\command.com",taskname
                    elseif              (Osflag = C6)
                              append    "!c:\windows\system32\cmd.exe",taskname
                    endif
                    append    " /c XCOPY c:\work\pdf\*",taskname
                    pack      str6,CC,SYSYR,SYSMO
                    append    str6,taskname
                    append    ".pdf ",taskname
.Patch 1.2                                        
.                   append    "\\nins1\e\data\weboar\",taskname
                    append    "\\nins1\e\storage\weboar\",taskname
.Patch 1.2                                        
                    reset     taskname
                    execute   taskname
.
                    clear     taskname
                    If (osflag = c1 | osflag =C5)
                              append    "!c:\winnt\system32\cmd.exe",taskname
                    elseif              (osflag = C3 | osflag =C4)
                              append    "!c:\command.com",taskname
                    elseif              (Osflag = C6)
                              append    "!c:\windows\system32\cmd.exe",taskname
                    endif
                    append    " /c del c:\work\pdf\*",taskname
                    append    str6,taskname
                    append    ".pdf",taskname
                    reset     taskname
                    execute   taskname
                    shutdown
          endif
          add       c1 to count
          DISPLAY   *P10:10,"Input records PROCESSED: ",COUNT,b1,slsown,b1,slslist
          Compare   "5457" to Slsown
          goto Readsls if equal            ;CARE is processed by Neom24AW
processr
          move      no to exflag
          MOVE      SLSLR TO NORDFLD
          REP       ZFILL IN NORDFLD
          CALL      NORDKEY
          RESET     EXCODES
          SCAN      OELCODE IN EXCODES
          IF EQUAL
                    MOVE      C0 TO N9
                    MOVE      OEXQTY TO N9
                    COMPARE   C0 TO N9
                    GOTO SPLITOK IF NOT EQUAL
                    move      yes to exflag
          endif
SPLITOK
          clear     str4
          move      slsown to str4
          rep       zfill in str4
          read      parfile,str4;str4,arflag                  .on the list????
          goto readsls if over                   .no
.         match     "4020" to str4
.         if equal
.                   move      yes to flatflg
.         else
.                   move      no to flatflg
.         endif
          add       c1 to incount
          compare   c1 to incount
          if equal
                    move      slsown to holdown
                    move      slslist to holdlist
                    move      rectype to holdid
                    move      c1 to pass
                    call      OpenFile
          endif
;
          if (slsown <> holdown | slslist <> holdlist)
                    if (slslist <> holdlist)
                              move      C4,pass
                    endif
.break
                    call      totold
                    call      CloseFile
                    move      slsown to holdown
                    move      slslist to holdlist
                    move      rectype to holdid
                    call      OpenFile
          endif
;
nobreak
          MOVE      B1 TO RUNFLAG
          cmatch    "A" to rectype
          goto readslsa if equal
          cmatch    "B" to rectype
          goto readpaid if equal
          cmatch    "C" to rectype
          goto readadj if equal
          cmatch    "D" to rectype
          goto readsls1 if equal
          display   *p1:24,*el,*b,"Invalid record type",*b,*w4
          goto readsls
readslsa
          ADD       C1 TO COUNTA
          DISPLAY   *P10:12,"New Sales records PROCESSED: ",COUNTA,b1,slsown,b1,slslist
;
          add       c1 to hit
          COMPARE   C0 TO SLSAP2
          IF EQUAL
                    MOVE      NO TO AP2SW
          ELSE
                    MOVE      YES TO AP2SW
          ENDIF
;
          compare   c1 to hit
          call      preplob if equal
;
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "readslsa-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "readslsa-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          call      readtax
          goto      getmore
;..............................................................................
;readsls1 - read sales file and get all  ninsls.old 4th pass
READsls1
          move      c4 to pass
          MOVE      B1 TO RUNFLAG
          ADD       C1 TO COUNTd
          DISPLAY   *P10:15,"NUMBER OF Sales records PROCESSED: ",COUNTD,b1,slsown,b1,slslist
          add       c1 to hit
          compare   c1 to countd     .1st open invoice?
          if equal            .yes
                    compare   countc to c0      .were there adjustments
                    if less
                              call      totadj        .yes go print total
                              move      c4 to pass
                              goto redsls1a
                    endif
                    compare   countb to c0   .were there payments?
                    if less
                              call      totpaid        .yes
                              move      c4 to pass
                              goto redsls1a
                    endif
                    compare   counta to c0    .no, were there new bills
                    call      totbill if less    .yes
                    move      c4 to pass
          endif
;
redsls1a
          COMPARE   C0 TO SLSAP2
          IF EQUAL
                    MOVE      NO TO AP2SW
          ELSE
                    MOVE      YES TO AP2SW
          ENDIF
;
          compare   c1 to hit
          call      preplob if equal
;
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "redslsa-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "redslsa-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          call      readtax
          goto      getmore
;............................................................................
readpaid
          move      c2 to pass
          add       c1 to hit
          ADD       C1 TO countB
          DISPLAY   *P10:13,"New payment records PROCESSED: ",countB,b1,slsown,b1,slslist
          MOVE      SLSLR TO NINVFLD
          REP       ZFILL IN NINVFLD
          CALL      NINVKEY
          CALL      READPAY
          compare   c1 to countb
          if equal
                    compare   counta to c0         .need to print new billing totals?
                    if less      .yes
                              call      totbill
                              move      c2 to pass
                    else
                              move      c2 to pass
                              call      header
                    endif
          endif
;
PROCES2
          COMPARE   C0 TO SLSAP2
          IF EQUAL
                    MOVE      NO TO AP2SW
          ELSE
                    MOVE      YES TO AP2SW
          ENDIF
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "PROCES2-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "PROCES2-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
.Unsure of this forced over so I am leaving it here for reference
.         PACK      MKEY FROM SLSMLR,Z3
.         REP       zfill IN MKEY
.         CALL      NMLRKEY
.         IF OVER
.                   PACK      MKEY FROM Z3,Z3,Z3
.                   CALL      NMLRKEY
.         ENDIF
          CALL      READTAX
;
          MOVE      "99/99/99" TO DATEPRT1
          MOVE      "99/99/99" TO DATEPRT1
          CLEAR     STR6
          clear     str4
          clear     str2
          UNPACK    SlsMDTe TO cc,str2,str4
          pack      str6 from str4,str2
          RESET     STR6                         .MMDDYY
          EDIT      STR6 TO DATEPRT1
          MOVE      "99/99/99" TO DATEPRT2
          CLEAR     STR6
          clear     str4
          clear     str2
          UNPACK    SlsiDTe TO cc,str2,str4
          pack      str6 from str4,str2
          RESET     STR6                         .MMDDYY
          EDIT      STR6 TO DATEPRT2
          PACK      CHKDATE FROM CHK1dTEM,SLASH,CHK1DTED,SLASH,CHK1DTEY
          move      c0  to invdate
          unpack    slsidte into STR2,YY,mm,dd
          CALL      CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
          MOVE      JULDAYS TO INVDATE
          MOVE      SLSAP1 TO AP1FORM
          MOVE      YES TO APSW
          COMPARE   APCHECK TO AP1form
          IF NOT GREATER
                    MOVE      NO TO APSW
          ENDIF
          mult      "100" by ap1form
          MOVE      APMASK TO AP1OUT
          EDIT      AP1FORM TO AP1OUT
          ADD       SLSAP1 TO APTOTDET
          MATCH     YES TO AP2SW
          IF EQUAL
                    ADD       SLSAP2 TO APTOTOWN
        ELSE
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              ADD       SLSAP1 TO APTOTOWN
                    ENDIF
          ENDIF
          MATCH     yes TO AP2SW
          IF EQUAL
                    move      slsap2 to ap2form
                    mult      "100" by ap2form
                    MOVE      APMASK TO AP2OUT
                    EDIT      AP2FORM TO AP2OUT
          ENDIF
          MATCH     YES TO AP2SW
          IF EQUAL
                    ADD       SLSAP2 TO APTOTLST
          ELSE
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              ADD       SLSAP1 TO APTOTLST
                    ENDIF
          ENDIF
          MATCH     YES TO AP2SW
          IF EQUAL
                    GOTO print
          ELSE
                    COMPARE   C0 TO PAYCHK
                    IF NOT EQUAL
                              GOTO readsls                    .sales read
                    ENDIF
          ENDIF
          MOVE      SLSLR TO NORDFLD
          REP       ZFILL IN NORDFLD
          CALL      NORDKEY
          goto      print
;
;............................................................................
readadj
          move      c3 to pass
          add       c1 to countc
          MOVE      SLSLR TO NINVFLD
          REP       ZFILL IN NINVFLD
          CALL      NINVKEY
          CALL      READPAY
          move      slslr to nordfld
          rep       zfill in nordfld
          call      nordkey
          DISPLAY   *P10:14,"Adjustment records PROCESSED: ",countc,b1,olon,b1,olnum
;
          compare   c1 to countc       .1st adj?
          if equal
                    compare   countb to c0   .yes,  were there payments?
                    if less
                              call      totpaid        .yes
                              MOVE      C3 TO PASS
                              GOTO READADJ1
                    ENDIF
                    compare   counta to c0    .no, were there new bills
                    if less
                              call      totbill     .yes
                              goto readadj1
                    endif
                    call      header
                    MOVE      C3 TO PASS
          endif
;
READADJ1
          compare   c1 to hit
          if equal
                    call      preplob
          endif
;
          move      SLSlr to nINVfld
          rep       zfill in nINVfld
          call      nINVkey
          PACK      NJSTFLD FROM INVNUM,SLSADJSW
          REP       ZFILL IN NJSTFLD
          CALL      NJSTKEY
          move      c0 to adjap1
          add       jstap1 to adjap1
;
          compare   c0 to adjap1
          if not equal
                    compare   c0 to ap2
                    goto readsls if not equal
          endif
          move      c0 to adjap2
          add       jstap2 to adjap2
          compare   c0 to adjap2          .if both ap adjs 0 skip
          if equal
                    compare   c0 to adjap1
                    goto readsls if equal
          endif
          compare   c0 to adjap2
          if not equal
                    move      yes to ap2sw
                    MOVE      apmask TO AP2OUT
                    EDIT      adjap2 TO AP2OUT
          else
                    move      no to ap2sw
                    MOVE      adjAPMSK TO adjAPOUT
                    EDIT      adjap1 TO adjAPOUT
                    compare   c0 to adjap1
                    goto readsls if equal
          endif
          move      jstreasn to n2
          clear     nadjtext
          load      nadjtext from n2 of adjres1,adjres2,adjres3,adjres4,adjres5:
                    adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                    adjres13,adjres14,adjres15,adjres16,adjres17,adjres18,adjres19:
                    adjres20,adjres21,adjres22,adjres23,adjres24,adjres25,adjres26:
                    adjres27,adjres28,adjres29,adjres30,adjres31,adjres32,adjres33:
                    adjres34,adjres35,adjres36,adjres37
          compare   "99" to n2
          if equal
                    move      adjres99 to nadjtext
          endif
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "READADJ1-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "READADJ1-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          call      readtax
          goto      print
;............................................................................
getmore
          MOVE      slsLR TO NORDFLD
          REP       ZFILL IN NORDFLD
          CALL      NORDKEY
          MOVE      slsLR TO NinvFLD
          REP       ZFILL IN NinvFLD
          call      ninvkey
          call      readpay
          move      paytn to paychk
          compare   c0 to page
          call      header if equal
;
          REP       ZFILL IN olnum
          MATCH     olnum TO TDMCLIST
          IF EQUAL
                    ADD       C1 TO RUNCOUNT
                    MOVE      STAR TO RUNFLAG
                    MOVE      C0 TO FORM82
                    MOVE      C0 TO CMPT92
                    MOVE      QTYbild TO cmpt92
                    MULT      ".00234" BY FORM82          60% TRIPLEX ON 3.90
                    MULT      ".00156" BY CMPT92          40%  COMMISSION ON 3.90
                    ADD       FORM82 TO RUNPASS       TDMC PORTION
                    ADD       CMPT92 TO RUNLR         LR INC PORTION
                    ADD       CMPT92 TO FORM82        TOTAL RUNNING CHARGE
                    MOVE      C0 TO FORM92
                    MOVE      AR TO FORM92             TOTAL BILLED
                    ADD       FORM92 TO RUNAR
                    SUB       FORM82 FROM FORM92      FIND FLAT FEE PORTION
                    ADD       FORM92 TO RUNFLAT        SAVE IT.
          ELSE
                    MOVE      B1 TO RUNFLAG
          ENDIF
;..............................................................................
;NOTE THIS TABLE NEEDS TO BE ADJUSTED WHEN EVER SALES PERSONNEL CHANGES.
;..............................................................................
;CONVERT SALESPERSONS TO SALES TEAMS.
          MOVE      C0 TO SALESBR
          PACK      SALESNUM FROM OSALES10,OSALES
          MOVE      SALESNUM TO SALESBR
          COMPARE   C0 TO SALESBR
          IF EQUAL
                    RESET     RUNCODES
                    SCAN      OLNUM IN RUNCODES
                    GOTO LOADOK IF NOT EQUAL
                    MOVE      C1 TO SALESBR
                    MOVE      C2 TO OELCODE
                    GOTO PROCESS
          ENDIF
;
;                                       2   5  3  4  4  6    ??  1  1
;   LOAD      SALESNUM FROM SALESBR OF LISA,BO,SA,EM,NP,INES,??,JC,TM
;                   1  5  7  3  7   ?   2   ?   7  6  5  3
;                   GH,JP,JE,MG,SMM,???,BM,???,BT,MD,LM,LT
;
;..............................................................................
;
LOADOK
          LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM1,TEAM1,TEAM2:
                    TEAM2,TEAM3,TEAM2,TEAM1:
                    TEAM1,TEAM1,TEAM1,TEAM2:
                    TEAM1,TEAM2,TEAM2,TEAM1:
                    TEAM2,TEAM2,TEAM3,TEAM1,TEAM1,TEAM1
          MOVE      SALESNUM TO SALESBR
;..............................................................................
;
PROCESS
          MOVE      PPM TO CMPT92
          MOVE      CMPT92 TO FORM32
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "PROCESS-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "PROCESS-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          call      readtax
;
          MOVE      SLSAP1 TO AP1FORM
          MOVE      SLSAP2 TO AP2FORM
          MOVE      YES TO APSW
          COMPARE   APCHECK TO AP1form
          IF NOT GREATER
                    MOVE      NO TO APSW
          ENDIF

          MOVE      APMASK TO AP1OUT
          mult      "100" by ap1form
          EDIT      AP1FORM TO AP1OUT
;
          MOVE      APMASK TO AP2OUT
          mult      "100" by ap2form
          EDIT      AP2FORM TO AP2OUT

          COMPARE   C0 TO SLSAP2
          IF EQUAL
                    MOVE      NO TO AP2SW
          ELSE
                    MOVE      YES TO AP2SW
          ENDIF
;
          move      c2 to tdmcflag
          MOVE      NORDFLD to nmrgfld
          REP       ZFILL IN NMRGFLD
          move      c0 to nmrgrqty
          move      c0 to nmrgiqty
          move      c0 to nmrgnet
          move      no to mrgsw
          CALL      NMRGKEY
          if not over
                    move      yes to mrgsw
          endif
          move      lrn to nshpfld
          move      no to shipsw
          call      nshpkey
          if not over
                    move      yes to shipsw
          endif
          move      olnum to ndatfld
          call      ndatkey
          call      wipecvars
          CALL      COMPUTE
*......................................................................
;
PRINT
          if (row > 7100)
                    call      Header
          endif
          move      c0 to n2
          move      onetper to n2
          compare   c0 to n2                    .net name order?
          if equal
                    move      b1 to innets
          else
                    move      "*" to innets
          endif
          MOVE      "99/99/99" TO DATEPRT1
          MOVE      "99/99/99" TO DATEPRT1
          CLEAR     STR6
          clear     str4
          clear     str2
          UNPACK    SlsMDTe TO cc,str2,str4
          pack      str6 from str4,str2
          RESET     STR6                         .MMDDYY
          EDIT      STR6 TO DATEPRT1
          MOVE      PPM TO CMPT92
          MOVE      MASK32 TO M$PPM
          MOVE      CMPT92 TO FORM32
          EDIT      FORM32 TO M$PPM
          MOVE      QTYbild TO FORM9
          MOVE      MASK9 TO M$QTY
          EDIT      FORM9 TO M$QTY
          MOVE      oQTY TO FORM9
          MOVE      MASK9 TO M$oQTY
          EDIT      FORM9 TO M$oQTY
          move      c0 to arform
          move      slsar to arform
          branch    pass of dt1,dt2,dt3,dt4
;.............................................................................
;dt1 - new billing detail print section |
;........................................
dt1
          clear     str40
          move      COMPCOMP to str40
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                    *p=2125:row,*font=prtpg85,slslr:
                    *p=2690:row,Invnum:
                    *p=3195:row,DatePrt1:
                    *p=5750:row,*Alignment=*right,M$Oqty:
                    *p=6525:row,M$qty:
                    *p=6850:row,"@":
                    *p=7250:row,m$ppm,*Alignment=*Left
          MATCH     YES TO AP2SW
          IF EQUAL
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                    move      slsap2 to aplotus
                    ADD       SLSAP2 TO APTOTOWN
          ELSE
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
                              move      slsap1 to aplotus
                              ADD       SLSAP1 TO APTOTOWN
                              If (exflag <> Yes)
                                        ADD       SLSAP1 TO APTOTwnr
                              else
                                        ADD       SLSAP1 TO APTOTwnx
                              endif
                    else
                              PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                              move      slsap2 to aplotus
                              ADD       SLSAP2 TO APTOTOWN
                              If (exflag <> Yes)
                                        ADD       SLSAP2 TO APTOTwnr
                              else
                                        ADD       SLSAP2 TO APTOTwnx
                              endif
                    ENDIF
          ENDIF
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"N",comma,str40:
.                             comma,slslr:
.                             comma,invnum:
.                             comma,dateprt1:
.                             comma,oqty:
.                             comma,qtybild:
.                             comma,m$ppm:
.                             comma,aplotus:
.                             comma,taxprt:
.                             comma,holdlst1
.         endif
;
          cmatch    "T" to hldarflg
          if equal
                    move      armask to arout
                    EDIT      arform TO AROUT
                    If (exflag = Yes)      ;        cmatch     yes to exflag
                              PrtPage   Laser;*font=prtpg85:
                                        *p=8125:Row,*Alignment=*Right,ARout:
                                        *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                        *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    else
                              PrtPage   Laser;*font=prtpg85:
                                        *p=8125:Row,*Alignment=*Right,ARout:
                                        *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    endif
         else
                    If        (exflag = Yes)      ;        cmatch     yes to exflag
                    PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left:
                              *p=8500:row,*Alignment=*Left,"Exchange List Recovery"
                    else
                              PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    endif
          endif
          add       "200" to Row
          goto readsls
;............................................................................
;dt4 - detail print for previously billed/open invoices |
;........................................................
dt4
          clear     str40
          move      COMPCOMP to str40
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                    *p=2125:row,*Alignment=*Left,*font=prtpg85,slslr:
                    *p=2690:row,Invnum:
                    *p=3195:row,DatePrt1:
                    *p=5750:row,*Alignment=*right,M$Oqty:
                    *p=6525:row,M$qty:
                    *p=6850:row,"@":
                    *p=7250:row,m$ppm,*Alignment=*Left
          MATCH     YES TO AP2SW
          IF EQUAL
                    ADD       SLSAP2 TO APTOTOWN
                    If (exflag <> Yes)
                              ADD       SLSAP2 TO APTOTwnr
                    else
                              ADD       SLSAP2 TO APTOTwnx
                    endif
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                    move      slsap2 to aplotus
          else
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              ADD       SLSAP1 TO APTOTOWN
                              If (exflag <> Yes)
                                        ADD       SLSAP1 TO APTOTwnr
                              else
                                        ADD       SLSAP1 TO APTOTwnx
                              endif
                              PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
                              move      slsap1 to aplotus
                    else
                              ADD       SLSAP2 TO APTOTOWN
                              If (exflag <> Yes)
                                        ADD       SLSAP2 TO APTOTwnr
                              else
                                        ADD       SLSAP2 TO APTOTwnx
                              endif
                              PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                              move      slsap2 to aplotus
                    ENDIF
          ENDIF
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"O",comma,str40:
.                             comma,slslr:
.                             comma,invnum:
.                             comma,dateprt1:
.                             comma,b9:
.                             comma,b9:
.                             comma,b6:
.                             comma,aplotus:
.                             comma,taxprt:
.                             comma,holdlst1
.         endif
          cmatch    "T" to hldarflg
          if equal
                    move      armask to arout
                    EDIT      arform TO AROUT
                    If (exflag = Yes)      ;        cmatch     yes to exflag
                              PrtPage   Laser;*p=6750:row,*Alignment=*Right,*font=prtpg85:
                                        *p=8125:Row,*Alignment=*Right,Arout:
                                        *p=8500:row,*Alignment=*Left,"Exchange List Recovery":
                                        *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    else
                              PrtPage   Laser;*font=prtpg85:
                                        *p=8125:Row,*Alignment=*Right,ARout:
                                        *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    endif
          else
                    If (exflag = Yes)      ;        cmatch     yes to exflag
                              PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left:
                                        *p=8500:row,*Alignment=*Left,"Exchange List Recovery"
                    else
                              PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
                    endif
          endif
          add       "200" to row
          goto readsls
;.............................................................................
;dt2 - detail print section for new payments made.|
;..................................................
dt2
          match     no to apsw
          goto prt3a2 if equal
          clear     str40
          move      COMPCOMP to str40
          add       "200" to row
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                    *p=2125:row,*font=prtpg85,slslr:
                    *p=2690:row,Invnum:
                    *p=3195:row,DatePrt1
GOODONE2
          MATCH     YES TO AP2SW
          IF EQUAL
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                    MOVE      CHKN2 TO CHKN1
                    move      slsap2 to aplotus
          ELSE
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
                    move      slsap1 to aplotus
          endif
          move      armask to arout
          add       arform to artotlst
          EDIT      arform TO AROUT
          PrtPage   Laser;*p=7500:row,*Alignment=*Left,ChkDate:
                    *p=8600:row,*Alignment=*Right,chkn1,*Alignment=*Left
          cmatch    "T" to hldarflg
          if equal
                    PrtPage   Laser;*p=7500:row,*Alignment=*Left,ChkDate:
                              *p=8600:row,*Alignment=*Right,chkn1,*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
          else
                    PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
          endif
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
.                             comma,slslr:
.                             comma,invnum:
.                             comma,dateprt1:
.                             comma,b9:
.                             comma,b9:
.                             comma,b6:
.                             comma,aplotus:
.                             comma,taxprt:
.                             comma,holdlst1:
.                             comma,chkdate:
.                             comma,chkn1
.         endif
prt3a2
          MOVE      C1 TO JSTN
          MOVE      SLSLR TO NINVFLD
          CALL      NINVKEY
DETADJ2
          PACK      NJSTFLD FROM INVNUM,JSTN
          CALL      NJSTKEY
          GOTO      readsls IF OVER
          MATCH     "14" TO JSTREASN
          IF NOT EQUAL
                    ADD       C1 TO JSTN
                    GOTO DETADJ2
          ENDIF
          move      c0 to adjap
          add       jstap1 to adjap
          add       jstap1 to aptotown
          move      adjapmsk to adjapout
          move      adjap to aplotus
          edit      adjap to adjapout
          CMATCH    YES TO AP2SW
          IF EQUAL
                    GOTO CONVAP2
          ENDIF
          COMPARE   C0 TO ADJAP
          IF NOT EQUAL
                    ADD       ADJAP TO MANPAY
                    move      adjap to aplist
                    mult      hund into aplist
                    add       aplist to aptotlst
          ENDIF
CONVAP2
          move      c0 to CMPT92
          add       jstap2 to cmpt92
          move      apmask2 to ap2out    .APMASK2 - NEW VAR WHICH WILL HOLD NEW VALUE OF CMPT92
          edit      CMPT92 to ap2out
          move      CMPT92 to aplotus
          cmatch    yes to ap2sw
          if equal
                    compare   c0 to CMPT92
                    goto readsls if equal
          endif
          ADD       CMPT92 TO MANPAY
          move      c0 to adjap
          add       CMPT92 to adjap
          COMPARE   C0 TO ADJAP
          IF NOT EQUAL
                    move      adjap to aplist
                    mult      hund into aplist
                    add       aplist to aptotlst
          ENDIF
          if (row > 7100)
                    call      Header
          endif
          add       "200" to row
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                    *p=2000:row,*Alignment=*right,*font=prtpg85,slslr:
                    *p=2500:row,Invnum:
                    *p=3200:row,DatePrt1,*Alignment=*Left
          COMPARE   C0 TO CMPT92
          IF NOT EQUAL
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP2Out,*Alignment=*Left
                    MOVE      "MANUAL" TO CHKN1
          ELSE
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,AP1Out,*Alignment=*Left
          ENDIF
          MOVE      "MANUAL" TO CHKN1
          UNPACK    JSTDATE INTO MM,DD,YY
          PACK      CHKDATE FROM MM,SLASH,DD,SLASH,YY
          move      c0 to arform
          move      slsar to arform
          move      armask to arout
          add       arform to artotlst
          EDIT      arform TO AROUT
          PRINT     hpt750,CHKDATE,b1,CHKN1;
          cmatch    "T" to hldarflg
          if equal
                    PrtPage   Laser;*p=8750:row,*Alignment=*Right,*font=prtpg85,arout:
                              *p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
          else
                    PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
          endif
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"P",comma,str40:
.                             comma,slslr:
.                             comma,invnum:
.                             comma,dateprt1:
.                             comma,b9:
.                             comma,b9:
.                             comma,b6:
.                             comma,aplotus:
.                             comma,taxprt:
.                             comma,holdlst1:
.                             comma,chkdate:
.                             comma,chkn1
.         endif
          add       "200" to row
          GOTO readsls
;
*............................................................
;DT3  - ADJUSTMENTS FOR THE MONTH      |
;.......................................
dt3
          clear     str40
          move      COMPCOMP to str40
          PrtPage   Laser;*Alignment=*LEFT,*p=125:row,*font=prtpg7,str40:
                    *p=2125:row,*Alignment=*left,*font=prtpg85,jstlr:
                    *p=2690:row,Invnum:
                    *p=3195:row,DatePrt1:
                    *p=7500:row,*Alignment=*left,nadjtext
          MATCH     YES TO AP2SW
          IF EQUAL
                    PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,Adjap2,*Alignment=*Left
                    move      adjap2 TO APlotus
                    ADD       SLSAP2 TO APTOTOWN
          ELSE
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              add       jstap1 to aptotown
                              PrtPage   Laser;*p=4750:row,*Alignment=*Right,*font=prtpg85,ADjAPOUT,*Alignment=*Left
                              move      adjap1 TO APlotus
                    else
                    ENDIF
          ENDIF
          PrtPage   Laser;*p=9950:row,*Alignment=*Left,TaxPrt,*Alignment=*Left
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"A",comma,str40:
.                             comma,slslr:
.                             comma,invnum:
.                             comma,dateprt1:
.                             comma,b9:
.                             comma,b9:
.                             comma,b6:
.                             comma,aplotus:
.                             comma,taxprt:
.                             comma,holdlst1:
.                             comma,nadjtext
.         endif
          add       "200" to row
          goto readsls
;............................................................................
HEADER
          ADD       C1 TO PAGE
          branch    pass of hd1a,hd2a,hd3a,hd4a
*............................................................
hd1a
          Move      "1300" to row
          if (FirstRec <> C0)
                    PrtPage   Laser;*newpage
          endif
          move      C1,FirstRec
          call      prtform
          call      PrtOwner
          call      prtsubHead
          PrtPage   Laser;*alignment=*right,*p=6875:725,"New Billing for List:",*Alignment=*Left
          CMATCH    "T" TO HLDARFLG
          IF EQUAL
                    PrtPage   Laser;*p=8125:1235,*Alignment=*Right,*font=prtpg9bi,"Gross":
                              *p=8125:1355,*font=prtpg9bi,"Billed",*Alignment=*Left
          else
          endif
          compare   c1 to page
          if        equal
                    move      lobbal to form102
                    mult      "100" by form102
                    move      form102 to aptotown
                    MOVE      TOTOMSK TO OWNERMSK
                    EDIT      APTOTOWN TO OWNERMSK
                    move      form102 to aplotown
                    Move      "1625" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
                    add       "400" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
                    add       "200" to Row              s
.                   cmatch    yes to flatflg
.                   if equal
.                             WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:
.                                       comma,b6:
.                                       comma,b6:
.                                       comma,b8:
.                                       comma,b9:
.                                       comma,b9:
.                                       comma,b4:
.                                       comma,aplotown:
.                                       comma,b7,comma,holdlst1
.                   endif
                    move      c0 to aptotown
                    move      c0 to aptotwnx
                    move      c0 to aptotwnr
          endif
          compare   c1 to page
          if not equal
                    add       "400" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"New Billing:",*font=prtpg9
                    add       "200" to row
          endif
          RETURN
*............................................................
hd4a
          Move      "1300" to row
          if (FirstRec <> C0)
                    PrtPage   Laser;*newpage
          endif
          move      C1,FirstRec
          call      prtform
          call      PrtOwner
          call      prtsubHead
          PrtPage   Laser;*alignment=*right,*p=6875:725,"Current Open Invoices:",*alignment=*Left
          add       "200" to row
          CMATCH    "T" TO HLDARFLG
          IF EQUAL
                    PrtPage   Laser;*p=8125:1235,*Alignment=*Right,*font=prtpg9bi,"Gross":
                              *p=8125:1355,*font=prtpg9bi,"Billed",*Alignment=*Left
          endif
          compare   c0 to counta
          if equal
                    compare   c1 to page
                    if equal
                              move      lobbal to form102
                              mult      "100" by form102
                              move      form102 to aptotown
                              MOVE      TOTOMSK TO OWNERMSK
                              EDIT      APTOTOWN TO OWNERMSK
                              move      form102 to aplotown
                              Move      "1625" to row
                              PrtPage   laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                                        *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
                              add       "500" to row
                              PrtPage   laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
                              add       "200" to row
.                             cmatch    yes to flatflg
.                             if equal
.                                       WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:
.                                                 comma,b6:
.                                                 comma,b6:
.                                                 comma,b8:
.                                                 comma,b9:
.                                                 comma,b9:
.                                                 comma,b4:
.                                                 comma,aplotown:
.                                                 comma,b7,comma,holdlst1
.                             endif
                              move      c0 to aptotown
                              move      c0 to aptotwnx
                              move      c0 to aptotwnr
                    endif
          endif
          compare   c0 to countb
          if equal
                    add       "500" to Row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
                    add       "200" to Row
          endif
          compare   c0 to countc
          if equal
                    add       "200" to Row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"No Adjustments to Payments this month.",*font=prtpg9
                    add       "400" to Row
          endif
          add       "500" to Row
          PrtPage   laser;*p=125:row,*font=prtpg9b,"Open invoices as of end of month.",*font=prtpg9
          add       "200" to Row
          RETURN
*............................................................
hd2a
          Move      "1300" to row
          MATCH     YES TO AP2SW
          IF EQUAL
                    call      nownkey
          endif
          if (FirstRec <> C0)
                    PrtPage   Laser;*newpage
          endif
          move      C1,FirstRec
          call      prtform
          call      PrtOwner
          PrtPage   Laser;*alignment=*right,*p=6875:725,"New Payments for List:",*alignment=*Left:
                    *p=7500:1300,*font=prtpg9bi,"Check date & Number"
          compare   c0 to counta
          if equal
                    move      lobbal to form102
                    mult      "100" by form102
                    move      form102 to aptotown
                    MOVE      TOTOMSK TO OWNERMSK
                    EDIT      APTOTOWN TO OWNERMSK
                    move      form102 to aplotown
                    Move      "1625" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                              *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
                    add       "500" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
                    add       "200" to Row
.                   cmatch    yes to flatflg
.                   if equal
.                             WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:
.                                       comma,b6:
.                                       comma,b6:
.                                       comma,b8:
.                                       comma,b9:
.                                       comma,b9:
.                                       comma,b4:
.                                       comma,aplotown:
.                                       comma,b7,comma,holdlst1
.                   endif
                    move      c0 to aptotown
                    move      c0 to aptotwnx
                    move      c0 to aptotwnr
          endif
          add       "500" to row
          PrtPage   laser;*p=125:row,*font=prtpg9b,"Payments during the month:",*font=prtpg9
          RETURN
*............................................................
hd3a
          Move      "1300" to row
          MATCH     YES TO AP2SW
          IF EQUAL
                    call      nownkey
          endif
          if (FirstRec <> C0)
                    PrtPage   Laser;*newpage
          endif
          move      C1,FirstRec
          call      prtform
          call      PrtOwner
          PrtPage   Laser;*alignment=*right,*p=6875:725,"Payment adjustments for list:",*alignment=*Left
          compare   c0 to counta
          if equal
                    compare   c1 to page
                    if equal
                              move      lobbal to form102
                              mult      "100" by form102
                              move      form102 to aptotown
                              MOVE      TOTOMSK TO OWNERMSK
                              EDIT      APTOTOWN TO OWNERMSK
                              move      form102 to aplotown
                              Move      "1625" to row
                              PrtPage   laser;*p=125:row,*font=prtpg9b,"Opening Balance :":
                                        *font=prtpg9,*p=4750:row,*Alignment=*Right,Ownermsk,*Alignment=*Left
                              add       "500" to row
                              PrtPage   laser;*p=125:row,*font=prtpg9b,"No new Billing this month.",*font=prtpg9
                              add       "200" to row
.                             cmatch    yes to flatflg
.                             if equal
.                                       WRITE     OUTPUT,SEQ;B5,comma,"B",comma,b25:
.                                       comma,b6:
.                                       comma,b6:
.                                       comma,b8:
.                                       comma,b9:
.                                       comma,b9:
.                                       comma,b4:
.                                       comma,aplotown:
.                                       comma,b7,comma,holdlst1
.                             endif
                              move      c0 to aptotown
                              move      c0 to aptotwnx
                              move      c0 to aptotwnr
                    endif
          endif
          compare   c0 to countb
          if        equal
                    add       "500" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"No new Payments this month.",*font=prtpg9
                    add       "200" to row
          endif
          add       "500" to row
          PrtPage   laser;*p=125:row,*font=prtpg9b,"Payment Adjustments :",*font=prtpg9
          add       "200" to row
          RETURN
;
*............................................................
totbiLL
          compare   c0 to countA             -did we have any new?
          if equal                -no
                    move      c0 to aptotown
                    move      c0 to page
                    move      c2 to pass
                    return
          endif
          move      aptotown to form102
          add       form102 to lobbal
          MOVE      TOTOMSK TO OWNERMSK
          mult      "100" by aptotown
          EDIT      APTOTOWN TO OWNERMSK
          MOVE      TOTOMSK TO OWNRMSKx
          mult      "100" by aptotwnx
          EDIT      APTOTWNx TO OWNRMSKx
          MOVE      TOTOMSK TO OWNRMSKR
          mult      "100" by aptotwnr
          EDIT      APTOTWNr TO OWNRMSKr
          if (row > 7100)
                    call      Header
          endif
          compare   c0 to aptotwnx
          if not equal
                    add       "200" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Total New Income +",*Alignment=*right,*p=4750:row,OwnerMsk:
                          *Alignment=*Left
                    add       "200" to row
                    PrtPage   laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
                              *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
                    add       "200" to row
                    PrtPage   laser;*p=5150:row,*font=prtpg9b,"Total Rental":
                              *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left,*font=prtpg9
          else
                    add       "200" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Total New Income +",*Alignment=*right,*p=4750:row,OwnerMsk:
                              *Alignment=*Left
          endif
          move      c0 to aptotown
          move      c0 to aptotwnx
          move      c0 to aptotwnr
          compare   c1 to pass
          if equal
                    move      c2 to pass
          endif
          call      header
          return
;..........................................................................
;totold - done with old invoices - done
totold
          compare   c0 to countd                .no opens????
          if equal
                    compare   countc to c0         .need to print adj totals?
                    if less      .yes
                              call      totadj
                    else
                              compare   countb to c0         .need to print paid totals?
                              if less      .yes
                                        move      c4 to pass
                                        call      totpaid
                              endif
                    endif
          endif
          move      aptotown to form102
          cmatch    "T" to newflag
          if equal
                    move      form102 to lobbal           .no prev. bal
          endif
          MOVE      TOTOMSK TO OWNERMSK
          EDIT      APTOTOWN TO OWNERMSK
          move      c0 to aptownck
          move      aptotown to aptownck
          move      c0 to aptotown
          goto totals
;.........................................................................
;totpaid - done with newly paid invoices let's get all adjustments
totpaid
          compare   c0 to countB
          if equal
                    move      c0 to aptotown
                    move      c3 to pass
                    move      c0 to page
                    return
          endif
          move      c0 to form102
          move      aptotown to form102
          sub       form102 to lobbal
          MOVE      TOTOMSK TO OWNERMSK
          mult      "100" by aptotown
          EDIT      APTOTOWN TO OWNERMSK
          if (row > 7100)
                    call      header
          endif
          add       "400" to row
          PrtPage   laser;*p=125:row,*font=prtpg9b,"Total Paid Income -",*Alignment=*right,*p=4750:row,OwnerMsk:
                    *Alignment=*Left
          move      c0 to aptotown
          compare   c4 to pass
          if not equal
                    move      c3 to pass
          endif
          call      header
          return
;.........................................................................
;totadj   - done with all adjustments lets do the next account
totadj
          compare   c0 to countC
          if equal
                    move      c0 to aptotown
                    move      c4 to pass
                    move      c0 to page
                    return
          endif
          move      c0 to form102
          mult      "100" by aptotown
          move      aptotown to form102
          mult      ".01" by form102
          add       form102 to lobbal
          MOVE      totomsk TO tadjMaSK
          EDIT      APTOTOWN TO tadjMaSK
          IF (row > 7100)
                    call      Header
          endif
          add       "400" to row
          PrtPage   laser;*p=125:row,*font=prtpg9b,"Total adjustments to Income +/-",*Alignment=*right,*p=4750:row,tadjmask:
                    *Alignment=*Left
          move      c0 to aptotown
          move      c4 to pass
          call      header
          return
;.................................................................
totals
          compare   lobbal to aptownck
          if not equal
                    Move      No to XFootFlag
          endif
          move      lobbal to form102
          mult      "100" by form102
          move      form102 to aptotown
          MOVE      TOTOMSK TO OWNERMSK
          EDIT      APTOTOWN TO OWNERMSK
          move      form102 to aplotown
          scan      "-" in ownermsk
          if equal
                    move      c0 to aptotown
                    MOVE      TOTOMSK TO OWNERMSK
                    EDIT      APTOTOWN TO OWNERMSK
          endif
          MOVE      TOTOMSK TO OWNRMSKx
          mult      "100" by aptotwnx
          EDIT      APTOTWNx TO OWNRMSKx
          MOVE      TOTOMSK TO OWNRMSKR
          mult      "100" by aptotwnr
          EDIT      APTOTWNr TO OWNRMSKr
          IF (row > 7100)
                    call      Header
          endif
          compare   c0 to aptotwnx
          if not equal
                    add       "400" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Ending Balance",*Alignment=*right,*p=4750:row,OwnerMsk:
                    *Alignment=*left
                    IF (row > 7100)
                              call      Header
                    endif
                    add       "200" to row
                    PrtPage   laser;*p=5150:row,*font=prtpg9b,"Total Exchange List Recovery":
                              *p=7250:row,*Alignment=*Right,ownrmskx,*Alignment=*Left
                    add       "200" to row
                    PrtPage   Laser;*p=5150:row,"Total Rental":
                              *p=7250:row,*Alignment=*Right,ownrmskr,*Alignment=*Left
          else
                    add       "400" to row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,"Ending Balance",*Alignment=*right,*p=4750:row,ownermsk:
                              *Alignment=*Left
          endif
          if (XFootFlag = No)
                    add       "200" to Row
                    PrtPage   laser;*p=125:row,*font=prtpg9b,*boldon,*ulon,"Ending Balance Does NOT Xfoot  ":
                    Lobbal," <> ",Aptownck,*boldoff,*uloff
                    Move      Yes to XFootFlag                   ;reset
          endif
endingb
          move      c0 to aptotwnx
          move      c0 to aptotwnr
.         cmatch    yes to flatflg
.         if equal
.                   WRITE     OUTPUT,SEQ;B5,comma,"E",comma,b25:
.                             comma,b6:
.                             comma,b6:
.                             comma,b8:
.                             comma,b9:
.                             comma,b9:
.                             comma,b7:
.                             comma,aplotown:
.                             comma,b7,comma,holdlst1
.         endif
          call      mtaxprt
          pack      nlobfld from holdown,holdlist,cc,sysyr,sysmo
          move      holdown to loblon
          move      holdlist to loblist
          move      cc to lobcc
          move      sysyr to lobyy
          move      sysmo to lobmm
          rep       zfill in nlobfld
          rep       zfill in loblon
          rep       zfill in loblist
          cmatch    "T" to lastrec
          goto eoj if equal
          move      c0 to lobbal
          move      slsown to holdown
          move      slslist to holdlist
          move      slslst1 to holdlst1
          move      arflag to hldarflg
          move      slslr to ninvfld
          rep       zfill in ninvfld
;add goodies get next beg bal
          pack      nlobfld from slsown,slslist,DATEKEY
          rep       zfill in nlobfld
          move      slsown to nownfld
          move      c0 to aptotown
          move      c0 to lobbal
          call      nlobkey
          if over
                    move      "T" to newflag
          else
                    move      "F" to newflag
          endif
          move      c0 to page
          move      c0 to counta
          move      c0 to countb
          move      c0 to countc
          move      c0 to countd
          move      c1 to pass
.........................
..Eventual code
.         move      SLSMLR,COMPFLD
.         rep       zfill,COMPFLD
.         if (COMPFLD = "")
..Force an over
.                   pack      COMPFLD,"))))))"
.         endif
.         move      "endingb-COMPKEY",Location
.         pack      KeyLocation,"Key: ",COMPFLD
.         call      COMPKEY
.........................
          move      SLSMLR,COMPFLD3
          rep       zfill,COMPFLD3
          if (COMPFLD3 = "")
.Force an over
                    pack      COMPFLD3,"))))))"
          endif
          move      "endingb-COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          call      readtax
toteoj
          return
;.........................................................................
READPAY
          MOVE      SLSOWN TO NOWNFLD
          REP       ZFILL IN NOWNFLD
          MOVE      PAYTN TO str1
          MOVE      PAYTN TO PAYCHK
          PACK      PAYKEY FROM NOWNFLD,str1
          REP       ZFILL IN PAYKEY
          MOVE      PAYKEY TO NPAYFLD
          REP       ZFILL IN NPAYFLD
          CLEAR     PCOMP                   *PCBUS DOES NOT CLEAR ON OVER.
          CLEAR     PNAME
          CLEAR     PSTREET
          CLEAR     PCITY
          CLEAR     PSTATE
          CLEAR     PZIP
          CALL      NPAYKEY
          CALL      NOWNKEY
          CMATCH    B1 TO PCOMP
          IF NOT EOS
                    COMPARE   C0 TO PAYCHK
                    IF EQUAL
                              CMATCH    YES TO AP2SW
                              IF NOT EQUAL
                                        MOVE      PCOMP TO ownocpy
                                        MOVE      PNAME TO OWNLONM
                                        MOVE      PSTREET TO OWNLOSA
                                        MOVE      PCITY TO OWNLOCTY
                                        MOVE      PSTATE TO OWNLOS
                                        MOVE      PZIP TO OWNLOZC
                              ENDIF
                    endif
          endif
          RETURN
;..............................................................................
preplob
          branch    pass to prepok
prepok
          pack      nlobfld from slsown,slslist,DATEKEY
          rep       zfill in nlobfld
          move      slsown to holdown
          move      slslist to holdlist
          move      slsown to nownfld
          move      slslst1 to holdlst1
          move      slslr to ninvfld
          move      arflag to hldarflg
          call      ninvkey
          CALL      READPAY
          move       c0 to lobbal
          call      nlobkey
          if over
                    move      "T" to newflag
          else
                    move      "F" to newflag
          endif
          call      header
          return
;.....................................................................
readtax
          MOVE      compnum TO nmtxfld
          CLEAR     TAXPRT
          MOVE      C0 TO MTXC501
          REP       ZFILL IN nmtxfld
          CALL      NMTXKEY
          MOVE      C0 TO TAX501
          MOVE      MTXC501,TAX501
          BRANCH    TAX501 OF C0,C0,C3,C4,C5,C6
C0
          RETURN
C3
          MOVE      "501C-3" TO TAXPRT
          RETURN
C4
          MOVE      "501C-4" TO TAXPRT
          RETURN
C5
          MOVE      "501C-5" TO TAXPRT
          RETURN
C6
          MOVE      "501C-6" TO TAXPRT
          RETURN

mtaxprt
          if (row > 7800)
                    call      header
          endif
mtaxprt2
          PrtPage   Laser;*p=1:7800,"Mailers's tax status is provided as a service though":
                    " its accuracy cannot be guaranteed."
          return
;

IO
          TRAPCLR   IO
          DISPLAY   *P1:24,*EL,"I/O ERROR ",ERROR,*W10;
;...............................................................................................
prtform
          PRTPAGE   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE:
                    *MarginL=0,*MarginT=0:
                    *Alignment=*Left:
                    *Pictrect=*off,*PICT=250:1050:3000:8000:NINLogo:
                    *RECT=1:250:5500:7250:
                    *p=5550:1,*font=prtpg12B,"Owner Activity Report":
                    *p=1:1,"Confidential":
                    *PENSIZE=10,*p=1:1250,*Line=10750:1250:               ;top line
                    *PENSIZE=10,*p=1:1500,*Line=10750:1500:               ;top line
                    *PENSIZE=10,*p=1:1250,*Line=1:8000:                   ;first vert line
                    *PENSIZE=10,*p=2000:1250,*Line=2000:8000:             ;2nd
                    *PENSIZE=10,*p=2575:1250,*Line=2575:8000:             ;3rd
                    *PENSIZE=10,*p=3150:1250,*Line=3150:8000:             ;4th
                    *PENSIZE=10,*p3725:1250,*Line=3725:8000:              ;5th
                    *PENSIZE=10,*p4975:1250,*Line=4975:8000:              ;6th
                    *PENSIZE=10,*p9875:1250,*Line=9875:8000:              ;7th
                    *PENSIZE=10,*p=1:7990,*Line=1875:7990:                    ;bottom lines   1
                    *PENSIZE=10,*p=2000:7990,*Line=2500:7990:                    ;bottom lines 2
                    *PENSIZE=10,*p=2575:7990,*Line=3025:7990:                    ;bottom lines   3
                    *PENSIZE=10,*p=3150:7990,*Line=3650:7990:                    ;bottom lines   4
                    *PENSIZE=10,*p=3725:7990,*Line=4850:7990:                    ;bottom lines   5
                    *PENSIZE=10,*p=4975:7990,*Line=9800:7990:                    ;bottom lines   6
                    *PENSIZE=10,*p=9875:7990,*Line=10400:7990:                    ;bottom lines   7
                    *p=583:1300,*font=prtpg9bi,"Mailer":
                    *p=2125:1300,*font=prtpg9bi,"LR##":
                    *p=2700:1300,*font=prtpg9bi,"Inv##":
                    *p=3140:1300,*font=prtpg9bi,"Mail Date":
                    *p=3850:1300,*font=prtpg9bi,"Income Amount":
                    *p=9950:1300,*font=prtpg9bi,"Mlr Tax"
          return
PrtOWner
          call      trim using Ownlocty
          prtpage   Laser;*p=500:375,Ownlon,*alignment=*right,*p10000:500,Page:
                    *p=10000:375,Today,*alignment=*left:
                    *p=500:575,OwnLonm:
                    *p=500:725,Ownocpy,*p=7000:725,Holdlst1:
                    *p=500:875,Ownlosa,*p=7000:875,Holdlist:
                    *p=500:1025,*ll,Ownlocty,*pl,",  ",Ownlos,"  ",Ownlozc
          return
;...............................................................................................
PrtSubHEad
          PRTPAGE   Laser;*p=5750:1300,*font=prtpg9bi,*Alignment=*right,"Order Qty":
                    *p=6725:1300,*font=prtpg9bi,"Billed Qty":
                    *p=7250:1300,"Price/m"
          Return
;...............................................................................................
OpenFile
          pack      str6,CC,SYSYR,SYSMO
          clear     str25
.Adjacency is expecting a 6 byte Owner number so give them this until we convert file
          pack      str25,"00",holdown,holdlist,str6
          rep       zfill,str25
          PRTOPEN   laser,"PDF995",str25
          pack      str25,str25,".pdf"
          move      C0,FirstRec
          return

CloseFile
          PRTclose laser
          return

          include   compio.inc
          include   cntio.inc
          INCLUDE   NORDIO.INC
          INCLUDE   NINVIO.INC
          include   nownio.inc
          INCLUDE   NmtxIO.INC
          INCLUDE   NDAT3IO.INC
          INCLUDE   GNXTIO.INC
          include   comlogic.inc
          include   nmoaio.inc
          include   nmrgio.inc
          INCLUDE   NLOBIO.INC
          include   nslsio.inc
          INCLUDE   NJSTIO.inc
          include   compute.inc
          include   ndatio.inc
          include   nacdio.inc
          include   nshpio.inc
          include   npayio.inc

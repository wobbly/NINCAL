* *****************************************************************************
* UNBILLED
* NAMES IN THE NEWS.  UNBILLED REPORT PROGRAM      26FEB91
*
* CREATED FROM  AMOUNTDUE & NIN201/TEXT
* *****************************************************************************
*
PC       EQU       0
         INCLUDE   COMMON.inc
.
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NSHPDD.inc
         include        compdd.inc
         include        cntdd.inc
         INCLUDE   NDATDD.inc
         include   nmrgdd.inc
         include   nmobdd.inc
         include   nmoadd.inc
         include   hp.inc
         include   nmlddd.inc
         INCLUDE   NOFRDD.INC
              INCLUDE         NSELDD.INC
              INCLUDE         NSEL2DD.INC
              INCLUDE         NTXTDD.INC
.begin patch 3.95
           include nprjdd.inc
           include nrevdd.inc
.end patch 3.95

Release   Init      "3.96"         DLH .New year
Reldate   INit      "2016 January 5"
.Release   Init      "3.95"         DLH .Break out cold new biz - does not seem to be done yet 2015Feb 24
.Reldate   INit      "2015 January 12"
.Release   Init      "3.94"         DLH .USe new order comselect code
.Reldate   INit      "2015 January 8"
.Release   Init      "3.93"         DLH .tweak to make sure orders not included on report details are added to executive excel totals
..                                           fix on nselfld2 prep, turned off more filepi's 
.Reldate   INit      "2014 April 24"
.Release   Init      "3.92"         DLH Getprice use exhange price from order if present
.Reldate   INit      "2014 January 7"
.Release   Init      "3.91"         DLH New Year
.Reldate   INit      "2014 January 2"
.Release   Init      "3.9"         DLH Breakout AR - AP for weekly reporting to excel
.Reldate   INit      "20 January 2011"
.Release   Init      "3.8"         DLH Don't break out PL
.Reldate   INit      "24 February 2010"
.Release   Init      "3.79"         DLH New Year
.Reldate   INit      "4 January 2010"
.Release   Init      "3.78"         DLH add code for Elstcde = "P" also flagged as exclusive
.Reldate   INit      "14 December 2009"
.Release   Init      "3.77"         DLH add code for M orders and Incorrectly placed orders
.Reldate   INit      "15 July 2008"
.Release        Init                "3.76"         DLH 24 June 2008 More fine tuning 
.Release        Init                "3.75"         DLH 26Sept2007 New PL lm #'s
.Release        Init                "3.74"         DLH 05June2007 oslspern
.for details prior to 3.55 check arcived version
.Release        Init                "3.73"         DLH 16March2007 PL, Sendmail
.Release        Init                "3.72"         DMB 13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release        Init                "3.71"         DLH 18Jul2006 Make LM use Return date like nord013d
.Release        Init                "3.7"         DMS 21Jun2006 Fulfillment Conversion
.RELEASE       INIT           "3.62"     DLH 12JUL2006 Removed Epsilon
.RELEASE      INIT           "3.61"     JD30JUN2006 Removed Nut Act 5.00/m pricing.
.Release       Init            "3.6"         DLH 13Feb2006 write to excel
.Release       Init            "3.55"         DLH 7Feb2006 breakout by source 30,60,90+
.release  init      "3.54"         DMB     18JUN2005  FM IP Chg
.release  init      "3.53"         ASH     07APR2005  COMMPER CONVERSION
.release  init      "3.52"         JD      14Jan2005  Updated starting LR
.release  init      "3.51"        DLH           08JUn2004  Refix required trim of datacard var Commper
.release  init      "3.50"        JD            04JUn2004  check dsprog for differents runs.
.release  init      "3.46"        DMB        26MAY2004      Mailer Conversion
.RELEASE  INIT      "3.45"      29JAN2004 ASH DATACARD CONVERSION
.RELEASE  INIT      "3.44"      06AUG2002 JD added lw robbins 12/m mailers.
.RELEASE  INIT      "3.43"     04FEB2002 ASH NINFUL CONVERSION
.RELEASE  INIT      "3.42"      08Mar02    added read of new mail date revision file.
.RELEASE  INIT      "3.41"     30Jan02    removed WS/added NWF 5.00/m pricing.
.RELEASE  INIT      "3.4"     15Oct01    DLH for reuse Orders with Blank return dates include in report instead of defaulting to maildate.
.                                       also email totals to DH
.RELEASE  INIT      "3.3"     23July01    DLH quick little patch to email a message
.RELEASE  INIT      "3.2"     19MAR01    ASH NINORD MOVED TO FILE MANAGER
.RELEASE  INIT      "3.1a"     04Jan00 jd  skip cancelled lcr's.
.RELEASE  INIT      "3.1"     08Oct99 DLH Added some more invalid rtn date checks.
.                            was skipping reuse orders?
.RELEASE  INIT      "3.0"     06MAY99 ASH Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.Release  init      "2.9"     06JAN99 ASH NINORD Y2K, File expansion
.Release  init      "2.81"     12jan99 jd changed start lr# search.
.Release  init      "2.8"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.Release   init      "2.7"             DLH 29jul98  starting lr 98.
.Release   init      "2.6"             DLH 09Jul98 commission on brk rents
.release  init      "2.51"            DLH 07May98 turn off locks on major reads
.release  init      "2.5"             JD  09FEB98  UPDATED STARTING LR#.
.release  init      "2.4"             JD  12DEC96 added more discount break.
.release  init      "2.3"             DLH 23Oct96 Add net code for straight rentals
.Release  init      "2.2"             JD  30mar95 added dawson brk # getprice
.RELEASE  INIT      "2.1"            JD  29JUN94 print to laser.
.RELEASE  INIT      "2.0"           JD  28SEP93 SPLC FLAT CHRGE $7.00 EXCHANGE.
.RELEASE  INIT      "1.9"          DLH 20MAY93 FLAG MAILDATE >= 2WKS OLD.
.RELEASE  INIT      "1.8"          DLH 26MAR93 ADDED BATCH BILLING BREAKOUT.
.RELEASE  INIT       "1.7"          DLH 27APR92  ADD CHAINING FROM DSINIT
.RELEASE   INIT      "1.6"         DLH 08APR92  LIFESTYLE & IC SYSTEMS.
.RELEASE  INIT      "1.5"         D. HERRICK 15JAN92  CONVERT MOST INCLUDES ADD EXCLUSIVE FLAG (LIST).
.RELEASE  INIT      "1.5"         D. HERRICK  SEP91    ALLOW DATE CHANGE.
.RELEASE  INIT      "1.4"         D. HERRICK 12SEP91    ADD BREAK OUT OF 30DAY INCOME INTO BRKAGE XCHN/RENT, LIST MANAG.
.RELEASE  INIT      "1.3"         D. HERRICK 10SEP91    CHANGE AGEING ON (A)LL
.                                REPORT TO MATCH STANDARD..
.RELEASE  INIT      "1.2"         D. HERRICK 06AUG91    ADDED ZERO LR INC ON
.                                LIST MANAGEMENT EXCHANGES.
.RELEASE  INIT      "1.1"         D. HERRICK 01AUG91
.                                ADDED AGEING OF ESTIMATED INCOME ON ALL REP.
.                                ADDED INCLUDE COMLOGIC.inc
.
.RELEASE  INIT      "1.0"        DLH. 26FEB91 WRITTEN.
. .............................................................................
.
.
. FILES DESCRIPTIONS
. ..................
. ...................................
NFULCOMP  DIM       55
. .............................................................................
.
.dave goes bad
printdetails          dim        1                     .if outside date criteruia suppress printing but add to totals
.end
N34           FORM            3.4
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
PRTFLAG  FORM      1
THOUS    FORM      "1000"
HUND     FORM      "100"
PDATE    DIM       8
JOBBR    FORM      1               BRANCH FOR JOB TYPE SEPERATE,TOTAL
TOTALDOL FORM      7.2
YR       DIM       2
GROSS    FORM      7.2
TOTAL    FORM      9.2
AR       FORM      9.2
CODENUM  FORM      2
EXCLPRT  DIM       4
.
PROGNAME DIM       8
CHKJUL   FORM      5
chkdate  form      6
chkdate2 dim       6
UNBILAMT FORM      9.2
FORM92   FORM      9.2
net92    form      9.2            holding field while calcing net charges
form32   form      3.2
FORM52   FORM      5.2
card$    form      5.2
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.
UNBILINC FORM      9.2
UNLNC30  FORM      9.2
UNLNC60  FORM      9.2
UNLNC90  FORM      9.2
UNLNC90P FORM      9.2
.begin release 3.55
UNLNC60BR     FORM      9.2
UNLNC60BE     FORM      9.2
UNLNC60LM     FORM      9.2
UNLNC90BR     FORM      9.2
UNLNC90BE     FORM      9.2
UNLNC90LM     FORM      9.2
UNLNC90PBR    FORM      9.2
UNLNC90PBE    FORM      9.2
UNLNC90PLM    FORM      9.2
.end release 3.55
.begin patch 3.73         subtotals for PL
UNPLINC   FORM      9.2
UNPL30    FORM      9.2
UNPL60    FORM      9.2
UNPL90    FORM      9.2
UNPL90P   FORM      9.2
UNPL60BR     FORM      9.2
UNPL60BE     FORM      9.2
UNPL60LM     FORM      9.2
UNPL90BR     FORM      9.2
UNPL90BE     FORM      9.2
UNPL90LM     FORM      9.2
UNPL90PBR    FORM      9.2
UNPL90PBE    FORM      9.2
UNPL90PLM    FORM      9.2
UNBILPLTOT FORM      12.2

UNPL30LM  FORM      9.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
UNPL30BE FORM      9.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
UNPL30BR FORM      9.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
COUNTONIN1  FORM      5                  NUMBER OF ORDERS CALCULATED.
TOTALNIN    FORM      9.2
TOTALNINU   FORM      9.2
UNNININC  FORM      9.2
UNNIN30   FORM      9.2
UNNIN60   FORM      9.2
UNNIN90   FORM      9.2
UNNIN90P  FORM      9.2
UNNIN60BR     FORM      9.2
UNNIN60BE     FORM      9.2
UNNIN60LM     FORM      9.2
UNNIN90BR     FORM      9.2
UNNIN90BE     FORM      9.2
UNNIN90LM     FORM      9.2
UNNIN90PBR    FORM      9.2
UNNIN90PBE    FORM      9.2
UNNIN90PLM    FORM      9.2
UNBILNINTOT         FORM      12.2
UNNIN30LM   FORM      9.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
UNNIN30BE   FORM      9.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
UNNIN30BR   FORM      9.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
COUNTOPL1   FORM      5                  NUMBER OF ORDERS CALCULATED.
TOTALPL     FORM      9.2
TOTALPLU    FORM      9.2
UNBILTOT    FORM      12.2

UN30LM    FORM      9.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
UN30BE    FORM      9.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
UN30BR    FORM      9.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
LRBBE     FORM      11.2      TOTAL BATCH BILL LR EXCH PORTION
LRBBR     FORM      11.2      TOTAL BATCH BILL LR RENT PORTION
LRPLBBE             FORM      11.2      TOTAL BATCH BILL LR EXCH PORTION
LRPLBBR             FORM      11.2      TOTAL BATCH BILL LR RENT PORTION
LRNINBBE    FORM      11.2      TOTAL BATCH BILL LR EXCH PORTION
LRNINBBR    FORM      11.2      TOTAL BATCH BILL LR RENT PORTION
WRIT      FORM      1
.begin patch 3.9
CalcAr    Form      9.2            .A/R
CalcAp    Form      9.2            .A/R

TAR       Form      9.2
TAP       Form      9.2

AR30      Form      9.2
AR60      Form      9.2
AR90      Form      9.2
AR90P     Form      9.2               .over 90

AR30BR    Form      9.2
AR60BR    Form      9.2
AR90BR    Form      9.2
AR90PBR   Form      9.2

AR30BE    Form      9.2
AR60BE    Form      9.2
AR90BE    Form      9.2
AR90PBE   Form      9.2

AR30LM    Form      9.2
AR60LM    Form      9.2
AR90LM    Form      9.2
AR90PLM   Form      9.2

AP30      Form      9.2
AP60      Form      9.2
AP90      Form      9.2
AP90P     Form      9.2

AP30BR    Form      9.2
AP60BR    Form      9.2
AP90BR    Form      9.2
AP90PBR   Form      9.2

AP30BE    Form      9.2
AP60BE    Form      9.2
AP90BE    Form      9.2
AP90PBE   Form      9.2

AP30LM    Form      9.2
AP60LM    Form      9.2
AP90LM    Form      9.2
AP90PLM   Form      9.2

.end patch 3.9
.
BATCHBR  FORM      1       "0" =NO, "1" = YES.
.
AGEFLAG  FORM      1
KEY      DIM       28
Searching      Init           "S-E-A-R-C-H-I-N-G"
.Begin patch 3.95
typer    dim      1
srcr     dim      1
cidr     dim      6
mmrep    dim       2
yyrep    dim       2
ccrep    dim       2
check1   form      5
check2   form      5 
today1   form      5
revdat   form      5
newbiz    init      "N"
OLDTOT   FORM    10.2
OLDTOT1   FORM    10.2
AR30NewBR    Form      9.2
AR60NewBR    Form      9.2
AR90NewBR    Form      9.2
AR90PNewBR    Form      9.2
AR30NewBE    Form      9.2
AR60NewBE    Form      9.2
AR90NewBE    Form      9.2
AR90PNewBE    Form      9.2
AR30NewLM    Form      9.2
AR60NewLM    Form      9.2
AR90NewLM    Form      9.2
AR90PNewLM    Form      9.2
AP30NewBR    Form      9.2
AP60NewBR    Form      9.2
AP90NewBR    Form      9.2
AP90PNewBR    Form      9.2
AP30NewBE    Form      9.2
AP60NewBE    Form      9.2
AP90NewBE    Form      9.2
AP90PNewBE    Form      9.2
AP30NewLM    Form      9.2
AP60NewLM    Form      9.2
AP90NewLM    Form      9.2
AP90PNewLM    Form      9.2
.end patch 3.95
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
ANS      DIM       1
DATE     DIM       8
TIME     DIM       8
MDATE    FORM      5
JUNDATE  FORM      6
QTYCHK   FORM      9
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
+ *****************************************************************************
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
PASS     FORM      3                  FOR JOBBR = 1 PASS = 1 = TRIPLEX
.                                     FOR JOBBR = 1 PASS = 2 = NOT TRIPLEX
.                                     FOR JOBBR = 2 NOT  USED.
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
COMPMLR  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "59"
SHIPPED  DIM       9
EXCHANGE DIM       9
COMSLCT  DIM       9
COUNTO   FORM      6                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
TOTALU   FORM      9.2
TDMCSW   DIM       1                  TRIPLEX INDICATOR Y=TRIPLEX, N=NOT
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
.SW30     DIM       1                  DUE FOR BILLING IN 30 DAYS = "Y"
SYSJDATE FORM      5
specl    dim       1
lwrobb12 init     "2749-1822-0704-0974-1451-0127-0638"
TEXT1    DIM       47
.............................................................................................
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MATCH     "NORD0013" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         MOVE      TODAY TO PDATE
         UNPACK    PDATE INTO MM,STR1,DD,STR1,YY
         MOVE      FUNC TO STR1
         MOVE      FUNC TO JOBBR
         goto      input
         ENDIF

         MOVE      "NORD0013" TO PROGRAM
         MOVE      "NIN   " TO COMPNME
         MOVE      "LOCAL" TO PRTNAME
         MOVE      C0 TO JOBBR
         IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
         CLOCK     DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    PDATE INTO MM,ANS,DD,ANS,YY
         XIF

input
         Scan      "NINORD", inpname
         IF        not equal
         MOVE      INPNAME TO NORDNME1
          Pack      Nordname from Inpname,"|NINS1:502"
              else
         move      "NINORD.ISI|NINS1:502" to nordname
         endif
         CLOCK     TIME TO TIME
         MOVE      "UNBILLED REPORT" TO STITLE
         MOVE      "DATE" TO PF3
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      PRTGET
NOTHING  RETURN
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     "LOCAL"  TO PRTNAME
         GOTO      TRAPS IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         print     hptop,hpdupl,hp17ptch,*f
         DISPLAY   *P01:06,"Input File  :",*P15:06,Nordname
         DISPLAY   *P01:07,"Print File  :",*P15:07,PRTNAME
         GOTO      TRAPS
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET

KEYDATE
         KEYIN     *P10:12,"ENTER DATE ",*ZF,*JR,*EL,*+,MM,*DV,SLASH,DD:
                   *DV,SLASH,YY
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         CALL      PAINT
         CALL      FUNCDISP
         RETURN
TRAPS    TRAP      IO GIVING ERROR NORESET IF IO
         TRAP      KEYDATE IF F3
         DISPLAY   *P1:24,*EL,"OPENING FILES";
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "                    " TO FERROR
         KEYIN    *P1:24,*EL,"FILES OPEN 30 SEC'S TO CHANGE DATE",*T30,STR1;
          Display   *p1:24,*el;
         TRAP      NOTHING IF F3
         MOVE      B1 TO PF3
         CALL      FUNCDISP
         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         TRAP      ABORT IF F5
.**************************************************
.******** FIRST LR # OF ????   *******************
.         MOVE      "600000" TO NORDFLD
.         MOVE      "550000" TO NORDFLD
         MOVE      "686400" TO NORDFLD                     .1st lr of 2009
....................................turn of all locks on reads
         move      c3 to nordlock           order file
         move      c3 to nmlrlock           mailer file
         move      c3 to ndatlock           datacard file
         move      c3 to nownlock           Owner file
         move      c3 to Complock           company file
         move      c3 to CNCTlock           contact file
         move      c3 to nshplock           Shipping file        
         move      c3 to nMOAlock           MOA detail file        
         move      c3 to nmldlock           mail date changge file        
         move      c3 to nofrlock           offer file        
         move      c3 to Nsellock           Select file
         move      c3 to NSel2lock          Select file
         move      c3 to Ntxtlock           datacard text file

............................................................................
         MOVE      C1 TO NORDPATH
         CALL      NORDTST
.
START    MOVE      C0 TO LINES
         MOVE      C0 TO TOTALDOL
         BRANCH    JOBBR TO DEF,ALL        .IF FROM DSINIT WE HAVE VALUE.
         MOVE      "D" TO STR1
         KEYIN     *P28:09,"(D)efault or (A)ll ",*T30,*RV,STR1
         REP       "D1A2" IN STR1
         MOVE      STR1 TO JOBBR
         BRANCH    JOBBR OF DEF,ALL         >1 OR 2 ?
         GOTO      START                            NO!
DEF     DISPLAY    *P28:09,*EL,"DEFAULTS SELECTED"
        GOTO  START1
.
ALL     DISPLAY    *P28:09,*EL,"ALL SELECTED"
        GOTO  START1
.
START1
         MOVE      C1 TO PASS
         SUB       TOTALU FROM TOTALU
         SUB       TOTAL FROM TOTAL
         MOVE      C0 TO UNLNC90PBR
         MOVE      C0 TO UNLNC90BR
         MOVE      C0 TO UNLNC60BR
         MOVE      C0 TO UNLNC90PBE
         MOVE      C0 TO UNLNC90BE
         MOVE      C0 TO UNLNC60BE
         MOVE      C0 TO UNLNC90PLM
         MOVE      C0 TO UNLNC90LM
         MOVE      C0 TO UNLNC60LM
         MOVE      C0 TO UNLNC90P
         MOVE      C0 TO UNLNC90
         MOVE      C0 TO UNLNC60
         MOVE      C0 TO UNLNC30
         MOVE      C0 TO UNPL90PBR
         MOVE      C0 TO UNPL90BR
         MOVE      C0 TO UNPL60BR
         MOVE      C0 TO UNPL90PBE
         MOVE      C0 TO UNPL90BE
         MOVE      C0 TO UNPL60BE
         MOVE      C0 TO UNPL90PLM
         MOVE      C0 TO UNPL90LM
         MOVE      C0 TO UNPL60LM
         MOVE      C0 TO UNPL90P
         MOVE      C0 TO UNPL90
         MOVE      C0 TO UNPL60
         MOVE      C0 TO UNPL30
         MOVE      C0 TO UNNIN90PBR
         MOVE      C0 TO UNNIN90BR
         MOVE      C0 TO UNNIN60BR
         MOVE      C0 TO UNNIN90PBE
         MOVE      C0 TO UNNIN90BE
         MOVE      C0 TO UNNIN60BE
         MOVE      C0 TO UNNIN90PLM
         MOVE      C0 TO UNNIN90LM
         MOVE      C0 TO UNNIN60LM
         MOVE      C0 TO UNNIN90P
         MOVE      C0 TO UNNIN90
         MOVE      C0 TO UNNIN60
         MOVE      C0 TO UNNIN30
.Begin patch 3.95
         MOVE      C0 TO AR30NewBR    
         MOVE      C0 TO AR60NewBR    
         MOVE      C0 TO AR90NewBR    
         MOVE      C0 TO AR90PNewBR   
         MOVE      C0 TO AR30NewBE    
         MOVE      C0 TO AR60NewBE    
         MOVE      C0 TO AR90NewBE    
         MOVE      C0 TO AR90PNewBE   
         MOVE      C0 TO AR30NewLM    
         MOVE      C0 TO AR60NewLM    
         MOVE      C0 TO AR90NewLM    
         MOVE      C0 TO AR90PNewLM   
         MOVE      C0 TO AP30NewBR    
         MOVE      C0 TO AP60NewBR    
         MOVE      C0 TO AP90NewBR    
         MOVE      C0 TO AP90PNewBR   
         MOVE      C0 TO AP30NewBE    
         MOVE      C0 TO AP60NewBE    
         MOVE      C0 TO AP90NewBE    
         MOVE      C0 TO AP90PNewBE   
         MOVE      C0 TO AP30NewLM    
         MOVE      C0 TO AP60NewLM    
         MOVE      C0 TO AP90NewLM    
         MOVE      C0 TO AP90PNewLM   
.end patch 3.95
         SUB       AR FROM AR
         SUB       UNBILINC FROM UNBILINC
         SUB       COUNTO FROM COUNTO
         SUB       COUNTO1 FROM COUNTO1
         SUB       COUNTI FROM COUNTI
         SUB       COUNTI1 FROM COUNTI1
.
GETREC         MOVELPTR       sEARCHING TO N2
               IF             (N2 = 17)
               SETLPTR        SEARCHING TO C1
               ELSE
               ADD            C1 TO N2
               SETLPTR        SEARCHING TO N2
               ENDIF
               DISPLAY        *P15:24,*EL,*HON,Searching,*HOFF;
.
         CALL      NORDKS
         GOTO      EXIT IF OVER



              if            (olrn = "7985840" or olrn = "798370" or olrn = "797330")
.              call          debug
              endif
         Display              *p1:10,"Current LR ",*p15:10,Olrn
         ADD       C1 TO COUNTO
         DISPLAY   *P1:24,COUNTO;
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       *YES
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      Getrec IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
         RESET     CANCODES               *RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       *CANCELLED?
         GOTO      GETREC IF EQUAL
.testing
          Scan      "M",Olrn
.         call      Debug if equal
          reset     Olrn
.testing
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if not over
               move           NSEL2NAME,O2DES
              else
               unpack         OPPM,str3,str2
               pack           str6,str3,".",str2
               rep            zfill,str6
               move           str6,NSEL2PRICE
              endif
              pack            NMLDFLD1,"01X",OLRN
        clear   str8
              pack            str8,"99999999"
              call            NMLDAIM
              loop
               until over
               if (NMLDDATE < str8)
                              move           NMLDDATE,str8
               endif
               call           NMLDKG
              repeat
              if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
              else
.Use current Mail Date
              endif
.
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
         MOVE      NO TO LSTMSW
.begin patch 3.75         
.         If        (str2 = "02" or Str2 = "06" or str2 = "19")
.begin patch 3.77
.begin patch 3.8
          Cmatch    "M",Olrn
          if        equal
          move      yes,lstmsw
.          move      "P",Ocompid2
          endif
          MOve      B1,Ocompid                .force to NIN
          MOve      B1,Ocompid2                .force to NIN
.end patch 3.8
          if        (Ocompid = "P" & str2 = "06")
          Clear     OCompid        .can't be both will PRESUME LM did place the order      
          endif
.end patch 3.77
          If        (str2 = "02" or Str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
.end patch 3.75         
          Move      yes,lstmsw
.begin patch 3.74
.         ElseIf    (str2 = "00")
          ElseIf    (str2 = "00" & Ocompid <> "P")
.end patch 3.74
          reset     Runcodes
          scan      Olnum,runcodes
                    If        Not Equal
                    Move      Yes,Lstmsw
                    endif
          Endif               
         MOVE      NO TO OVER
.
PREPOWN  MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
         COMPARE   C1 TO JOBBR               *WHICH JOB TYPE?
         GOTO      CONTIN IF NOT EQUAL         *ALL REPORT REPORT
          call            Trim using OFULLFIL             
          if (OFULLFIL <> "")
                              pack            COMPFLD,OFULLFIL
                              call      zfillit using COMPFLD
                              move            C1,COMPPATH
                              move            "PREPOWN-COMPKEY",Location
                              pack            KeyLocation,COMPFLD
                              call            COMPKEY
                              if over
                    clear COMPFLD
                    clear NFULCOMP
                              else
                    move      COMPCOMP,NFULCOMP
                              endif
          else      // OWNCTN = ""
                              clear          COMPFLD
                              clear          NFULCOMP
          endif
                    if (COMPFLD = "009406")
               BRANCH    PASS OF TDMCYES,GETREC      *TRIPLEX PASS, OR NON-TDMC
TDMCYES
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
.if no return date check for a reuse
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
.                            call          usemd
                            goto          usemd
                            endif
              endif
.if no return date check for a reuse
              if            (chkdate2 = b6 or chkdate2 = "" or chkdate2 = b1)
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
.                            call          usemd
                            goto          usemd
                            endif
              endif

         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         MOVE      ORTNDTEc TO cc
         CALL      CVTJUL
         GOTO      CHKDAYS

USEMD    CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         MOVE      OMDTEc TO CC
         CALL      CVTJUL
CHKDAYS  MOVE      SYSJDATE TO CHKJUL
         ADD       C7 TO CHKJUL              PLUS 7 DAYS
         SUB       JULDAYS FROM CHKJUL
         COMPARE    CHKJUL TO C0                DATE GREATER THAN 7 DAYS AFTER
.dh goes bad
                      if         not less
.         GOTO      GETREC IF NOT LESS               YES
                    MOVe         Yes to printdetails
                    GOTO      CONTIN                        NO,PROCESS
                      endif      
           
         ELSE
.dh goes bad
         MOVe         no to printdetails
.         BRANCH    PASS OF GETREC,TDMCNO
         BRANCH    PASS OF Contin,TDMCNO
         ENDIF
.
TDMCNO   CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         MOVE      SYSJDATE TO CHKJUL
         SUB       JULDAYS FROM CHKJUL
         COMPARE    CHKJUL TO SEQ              DATE GREATER THAN TODAY?
.         GOTO      GETREC IF NOT LESS             YES
.dh goes bad
           if         not less
         MOVe         no to printdetails
         else
         MOVe         Yes to printdetails
           endif

.
CONTIN   CALL      DETAIL
         GOTO      GETREC               *ADDITIONAL CRITERIA FAILED GET NEXT RE
.
DETAIL
         MOVE      C0 TO FORM92
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
.BEGIN PATCH 3.9
          moVE      c0,CalcAR
          Move      C0,CalcAP
.END PATCH 3.9
          
         MOVE      C0 TO AR
         move      c0 to commper
         clear     specl
         CALL      MLRREAD
         CALL      READSHIP            *ORDER SHIPPED????
         CALL      READMRG
         CALL      GETCARD
         SUB       FORM92 FROM FORM92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
         call      special
         cmatch    yes to specl
         if        equal
         goto      calcsp       .got price fron special subroutine
         endif
              MOVE            NSEL2PRICE,FORM52
               if             (form52 = 0 & lstmsw <> "Y" & OELCODE <> "3" & OELCODE <> "3")
.               call           debug
               endif
               if             (Omlrnum = "9615")
.               call           debug
               endif
calcsp
         MULT      FORM52 BY FORM92       .base price
         MOVE      FORM92 TO UNBILINC
.begin patch 3.9
          Move      Unbilinc,CalcAR               .save receivables
.end patch 3.9
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      OKEX IF EQUAL

         if        (onetfm="M" | onetfm="F")      .discounted order???
         move      onetper to form32              .yes calc it
         mult      ".01" by form32
.
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
         mult       form32 by form92               .percentage of billable names
         move      form92 to net92                 .save it
         MULT      FORM52 BY FORM92
         MOVE      FORM92 TO UNBILINC               .base rental
         endif

         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
         MOVE      C0 TO FORM92
         MOVE      OEXQTY TO FORM92
         COMPARE   C0 TO FORM92            PURE EXCHANGE ?
         IF        EQUAL                 YES.
          MOVE      C0 TO QTYCHK
          MOVE      NO TO SPLITSW
          MOVE      OQTY TO QTYCHK
          MOVE      QTYCHK TO FORM92

          CMATCH    YES TO LSTMSW
....................
                      IF        EQUAL
                      MOVE      C0 TO UNBILINC
.begin patch 3.9
           Move      C0,CalcAR
.end patch 3.9
                      GOTO      OK
                      ELSE
                      GOTO      GETPRICE
                      ENDIF
....................
          ELSE
          MOVE      YES TO SPLITSW            .split
          CMATCH    YES TO LSTMSW
.............................
                      IF        EQUAL
                      MOVE      C0 TO UNBILAMT
.begin patch 3.9
           Move      C0,CalcAR
.end patch 3.9
                      GOTO      RENTPART
                      ELSE
                      MOVE      C0 TO QTYCHK
                      MOVE      OEXQTY TO QTYCHK
                      MOVE      QTYCHK TO FORM92
                      GOTO      GETPRICE
                      ENDIF
............................
          ENDIF
GETPRICE

         cmatch    yes to specl
         if        equal
         goto      calce
         endif
.begin patch 3.92
              MOVE            NSEL2PRICE,FORM52
               if             (form52 <> 0 & lstmsw <> "Y" & OELCODE = "3")
                    goto      calce
               Elseif         (form52 <> 0 & lstmsw <> "Y" & OELCODE = "2" & SPLITSW <> "Y")     
                    goto      calce
               endif
.end patch 3.92

         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    *SAVE RESULT
         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           *CONVERT TODAY'S  DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    *SAVE RESULT

         COMPARE   JUNDATE TO MDATE
         IF        NOT GREATER
         MOVE      C8 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         GOTO      CALCE
         ENDIF

         MOVE      C8 TO FORM52

CALCE
.Start Patch #3.44
        reset      lwrobb12
        match      "0638" to obrknum             .lwrobb
        if         equal
        scan       omlrnum in lwrobb12
          if         equal
          move       "12" to form52
          endif
        endif
         DIVIDE    THOUS INTO FORM92
         MULTIPLY  FORM52 BY FORM92
         MOVE      FORM92 TO UNBILAMT
.begin patch 3.9
          Move      UnBilinc,Calcar
.end patch 3.9
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
         MOVE      FORM92 TO UNBILINC
.begin patch 3.9
          Move      UnBilinc,CalcAR
.end patch 3.9
         GOTO      OK
         ENDIF
.
RENTPART
.
         MOVE      C0 TO FORM92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO FORM92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM FORM92           GET RENTAL PORTION
         MULT      ".001" BY FORM92
         cmatch    yes to specl
         if        equal
         goto      calcsp2
         endif

         compare   c0 to card$
         if        equal
         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         else
         move      card$ to form52
         endif
         MULT      FORM52 BY FORM92
.begin patch 3.9
          Add      Form92,CalcAR
.end patch 3.9

.
        CMATCH    YES TO LSTMSW
         IF        EQUAL
         MULT      ".1" BY FORM92
         ELSE
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,form92
         ENDIF

addamt   ADD       FORM92 TO UNBILAMT
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
calcsp2  MULT      FORM52 BY FORM92
         ADD       FORM92 TO UNBILAMT
.begin patch 3.9
          Add      Form92,CalcAR
.end patch 3.9
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
GETCARD  MOVE      OLNUM TO NDATFLD
         CLEAR     EXCLPRT
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
.         CMATCH    "C" TO ELSTCDE
.         IF        EQUAL
          if        (elstcde = "C" or Elstcde = "P")
         MOVE      "EXCL" TO EXCLPRT
         ENDIF
              if (NDATCONV = "1")
                              pack           NSELFLD1,"01X",LSTNUM
                              pack           NSELFLD2,"02XBASE"
                              move           "NSELAIM",Location
                              pack           KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call           NSELAIM
                              if not over
                                             if (NSELEXC <> "2")
                                                            move           C0,card$
                                                            move           NSELPRICE,card$
                                             endif
                              else
                                             goto DataCheckText
                              endif
              else
DataCheckText
               pack           NTXTFLD,LSTNUM,"1"
               move           "NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               if not over
                              move           NTXTTEXT,text1
                              SCAN           "EXCHANGE ONLY" IN TEXT1
                              RETURN IF EQUAL                 NO USABLE $ RETURN
                              RESET          TEXT1
                              SCAN           "$" IN TEXT1
                              RETURN IF NOT EQUAL        NO USABLE $ RETURN
                              BUMP           TEXT1 BY 1
                              PACK           STR2 FROM TEXT1
                              move           c0 to card$
                              MOVE           STR2 TO card$
                              SCAN           "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
                              RETURN IF NOT EQUAL            *YES.
                              CLEAR          STR2
                              BUMP           TEXT1 BY 1
                              PACK           STR2 FROM TEXT1       *NO, NOW WE DO!
                              move           c0 to card$
                              MOVE           STR2 TO card$
               else
                              clear          text1
               endif
              endif
         RETURN
RENT
         cmatch    yes to specl
         if        equal
         goto      ok
         endif
.begin patch 3.9
          Move      Unbilinc,CalcAR
.end patch 3.9

         CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,unbilinc
         ENDIF
.
OK
          IF        (LstMsw = YES)
          GOto      TDMCYES1                .Its list management use Return date if avail.
          Else
          GOTO      USEMD1
          Endif
TDMCYES1
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
         compare   c0 to chkdate
.if no return date check for a reuse
              if            equal
                            if            (ortnnum = "0001")
                            goto          bill30                   .take it now it is a reuse
              else
.                            call          usemd1
                            goto          usemd1
                            endif
              endif
         MATCH     b6 TO chkdate2
.if no return date check for a reuse
              if            equal
                            if            (ortnnum = "0001")
                            goto          bill30                   .take it now it is a reuse
                            else
.                            call          usemd1
                            goto          usemd1
                            endif
              endif
         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      Ortndtec TO cc
         MOVE      ORTNDTEY TO YY
         CALL      CVTJUL
         GOTO      CHKDAY1
USEMD1   CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         MOVE      OMDTEC TO CC
         CALL      CVTJUL
.
CHKDAY1  MOVE      SYSJDATE TO CHKJUL
.
.begin patch 3.9
          Move      CalcAR,CalcAP
          Sub      Unbilinc from CalcAP
          Add       CalcAr,TAR
          Add       CalcAP,TAP
.end patch 3.9
         add       c1 to chkjul             .5/1/ dlh plus 1 day
         SUBTRACT  chkjul from JULdays
.         MOVE      NO TO SW30
.
         COMPARE   c0 TO JULDAYS
         GOTO      BILL30 IF LESS          *SHOULD BE BILLED THIS MONTH.
.
         COMPARE   C31 TO JULDAYS
         GOTO      BILL60 IF LESS
.
         COMPARE   "61" TO JULDAYS         *
         GOTO      BILL90 IF LESS
         ADD       UNBILINC TO UNLNC90P                 .total  90 p
                               if        (OcompId = "P" or (Ocompid2 = "P" & LSTMSW = YES))
                               add       Unbilinc,unPL90p
                               else
                               add       Unbilinc,UnNIN90p
.begin patch 3.9
                               Add       CalcAR,AR90P
                               Add       CalcAP,AP90P
.end patch 3.9
                               endif
              IF              (LSTMSW = YES)
              ADD             UNBILINC TO UNLNC90Plm        .total lm 90p
.begin patch 3.73
                    if        (OcompId2 = "P")                        
                    ADD         UNBILINC TO UNPL90Plm       .total PL lm 90p
                    else
                    ADD         UNBILINC TO UNNIN90Plm      .total PL lm 90p
.begin patch 3.9
.begin patch 3.95
                               if           (newbiz <> Yes)
                    Add       CalcAR,AR90PLM
                    Add       CalcAP,AP90PLM
                               Else
                      Add       CalcAR,AR90PNewLM
                      Add       CalcAP,AP90PNewLM
                               endif
.end patch 3.95
.end patch 3.9
                    endif

              ELSE

              Reset           Excodes
              SCAN            oELCODE IN EXCODES
                        IF              EQUAL
                        ADD             UNBILINC TO UNLNC90Pbe         .total exchange 90p
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL90Pbe       .total PL BE 90p
                              else
                              ADD         UNBILINC TO UNNIN90Pbe      .total NIN BE 90p
.begin patch 3.9
.begin patch 3.95
                    if        (newbiz <> Yes)  
                              Add       CalcAR,AR90PBE
                              Add       CalcAP,AP90PBE
                               Else
                      Add       CalcAR,AR90PNewLM
                      Add       CalcAP,AP90PNewLM
                               endif
.end patch 3.95
.end patch 3.9
                              endif
                         ElsE
                         ADD             UNBILINC TO UNLNC90Pbr        .total rent 90P
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL90Pbr       .total PL BR 90p
                              else
                              ADD         UNBILINC TO UNNIN90Pbr      .total NIN BR 90p
.begin patch 3.9
.begin patch 3.95
                    if        (newbiz <> Yes)  
                              Add       CalcAR,AR90PBR
                              Add       CalcAP,AP90PBR
                               Else
                                 Add       CalcAR,AR90PNewBR
                                 Add       CalcAP,AP90PNewBR
                               endif
.end patch 3.9
                              endif
.end patch 3.73
                         endif
.end patch 3.95              
              ENDIF
         GOTO      OK1
         
BILL30   ADD       UNBILINC TO UNLNC30               .total 30
                    if        (OcompId = "P" or (Ocompid2 = "P" & LSTMSW = YES))
.                    call      debug
                    add       Unbilinc,unPL30         .pl 30
                    else
                    add       Unbilinc,UnNIN30    .nin 30
.begin patch 3.9
                    Add       CalcAR,AR30
                    Add       CalcAP,AP30
.end patch 3.9
                    endif
.end patch 3.73               
...............
              IF              (LSTMSW = YES)
              ADD             UNBILINC TO UN30lm                   .total lm 30
                    if        (OcompId2 = "P")                        
                    ADD         UNBILINC TO UNPL30lm        .total PL lm 30
                    else
                    ADD         UNBILINC TO UNNIN30lm       .total NIN lm 30
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR30LM
                              Add       CalcAP,AP30LM
                              Else
                              Add       CalcAR,AR30NewLM
                              Add       CalcAP,AP30NewLM
                              endif
.end patch 3.95
.end patch 3.9
                    endif

              ELSE                              .brokerage
              Reset           Excodes
              SCAN            oELCODE IN EXCODES
                              IF              EQUAL                .exchange
                              ADD             UNBILINC TO UN30be            .total Exchange 30
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL30be        .total PL BE 30
                              else
                              ADD         UNBILINC TO UNNIN30be       .total NIN BE 30
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR30BE
                              Add       CalcAP,AP30BE
                               else  
                              Add       CalcAR,AR30NewBE
                              Add       CalcAP,AP30NewBE
                              endif
.end patch 3.95
.end patch 3.9
                              endif

                              if        (batchBr = c1)
                                        ADD     UNBILINC TO LRBBE
.begin patch 3.73
                                        if        (Ocompid = "P")
                                                  ADD       UNBILINC TO LRPLBBE
                                                  Else
                                                  ADD       UNBILINC TO LRNINBBE
                                                  endif
                              endif
                              ElsE                                    .rental                                 
                              ADD             UNBILINC TO UN30br      .total rent 30
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL30br        .total PL BR 30
                              else
                              ADD         UNBILINC TO UNNIN30br       .total NIN BR 30
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR30BR
                              Add       CalcAP,AP30BR
                               else  
                              Add       CalcAR,AR30NewBR
                              Add       CalcAP,AP30NewBR
                              endif
.end patch 3.95
.end patch 3.9
                              endif
                              if        (batchBr = c1)
                                        ADD     UNBILINC TO LRBBR
.begin patch 3.73
                                        if        (Ocompid = "P")
                                                  ADD       UNBILINC TO LRPLBBR
                                                  Else
                                                  ADD       UNBILINC TO LRNINBBR
                                                  endif
                              endif
.end patch 3.73
                              endif
              ENDIF
         GOTO      OK1

...................
.         MOVE      YES TO SW30
.         GOTO      OK1
.
BILL60   ADD       UNBILINC TO UNLNC60               .total 60
.Begin patch 3.73             
                    if        (OcompId = "P" or (Ocompid2 = "P" & LSTMSW = YES))
                    add       Unbilinc,unPL60
                    else
                    add       Unbilinc,UnNIN60
.begin patch 3.9
                    Add       CalcAR,AR60
                    Add       CalcAP,AP60
.end patch 3.9
                    endif
.end patch 3.73               

              IF              (LSTMSW = YES)
              ADD             UNBILINC TO UNLNC60lm                   .total lm 60
                    if        (OcompId2 = "P")                        
                    ADD         UNBILINC TO UNPL60lm        .total PL lm 60p
                    else
                    ADD         UNBILINC TO UNNIN60lm       .total PL lm 60p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                    Add       CalcAR,AR60LM
                    Add       CalcAP,AP60LM
                               else  
                              Add       CalcAR,AR60NewLM
                              Add       CalcAP,AP60NewLM
                              endif
.end patch 3.95
.end patch 3.9
                    endif

              ELSE
              Reset           Excodes
              SCAN            oELCODE IN EXCODES
                              IF              EQUAL
                              ADD             UNBILINC TO UNLNC60be            .total Exchange 60
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL60be        .total PL BE 60p
                              else
                              ADD         UNBILINC TO UNNIN60be       .total NIN BE 60p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR60be
                              Add       CalcAP,AP60be
                               else  
                              Add       CalcAR,AR60NewBE
                              Add       CalcAP,AP60NewBE
                              endif
.end patch 3.95
.end patch 3.9
                              endif
                              ElsE
                              ADD             UNBILINC TO UNLNC60br   .total rent 60
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL60br        .total PL BR 60p
                              else
                              ADD         UNBILINC TO UNNIN60br       .total NIN BR 60p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR60br
                              Add       CalcAP,AP60br
                               else  
                              Add       CalcAR,AR60NewBR
                              Add       CalcAP,AP60NewBR
                              endif
.end patch 3.95
.end patch 3.9
                              endif
.end patch 3.73
                              endif
              ENDIF
         GOTO      OK1
.
BILL90   ADD       UNBILINC TO UNLNC90
                    if        (OcompId = "P" or (Ocompid2 = "P" & LSTMSW = YES))
                    add       Unbilinc,unPL90
                    else
                    add       Unbilinc,UnNIN90
.begin patch 3.9
                    Add       CalcAR,AR90
                    Add       CalcAP,AP90
.end patch 3.9
                    endif
.end patch 3.73               
              IF              (LSTMSW = YES)
              ADD             UNBILINC TO UNLNC90lm
.begin patch 3.73             
                    if        (OcompId2 = "P")                        
                    ADD         UNBILINC TO UNPL90lm        .total PL lm 90p
                    else
                    ADD         UNBILINC TO UNNIN90lm       .total NIN lm 90p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                    Add       CalcAR,AR90LM
                    Add       CalcAP,AP90LM
                               else  
                              Add       CalcAR,AR90NewLM
                              Add       CalcAP,AP90NewLM
                              endif
.end patch 3.95
.end patch 3.9
                    endif
              ELSE
              Reset           Excodes
              SCAN            oELCODE IN EXCODES
                              IF              EQUAL
                              ADD             UNBILINC TO UNLNC90be
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL90be        .total PL BE 90p
                              else
                              ADD         UNBILINC TO UNNIN90be       .total PL BE 90p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR90be
                              Add       CalcAP,AP90be
                               else  
                              Add       CalcAR,AR90NewBe
                              Add       CalcAP,AP90NewBE
                              endif
.end patch 3.95
.end patch 3.9
                              endif
                              ElsE
                              ADD             UNBILINC TO UNLNC90br
                              if        (OcompId = "P")                         
                              ADD         UNBILINC TO UNPL90br        .total PL bR 90p
                              else
                              ADD         UNBILINC TO UNNIN90br       .total PL BR 90p
.begin patch 3.9
.begin patch 3.95
                              if            (newbiz <> Yes)
                              Add       CalcAR,AR90BR
                              Add       CalcAP,AP90BR
                               else  
                              Add       CalcAR,AR90NewBR
                              Add       CalcAP,AP90NewBR
                              endif
.end patch 3.95
.end patch 3.9
                              endif
.end patch 3.73
                              endif
              ENDIF
         GOTO      OK1
.
OK1      MOVE      UNBILINC TO FORM92
         ADD       FORM92 TO TOTALU
         ADD       FORM92 TO TOTAL
         ADD       C1 TO COUNTO1
         MOVE      FORM92 TO AR
.begin patch 3.73
          if        (Ocompid = "P" or (Ocompid2 = "P" & LSTMSW = YES))
          add       Form92,TotalPLU
          add       Form92,TotalPL
          add       c1,countoPL1
          else
          add       Form92,TotalNINU
          add       Form92,TotalNIN
          add       c1,countoNIN1
          endif
.end patch 3.73     
.
CHECK1 
.         MATCH     YES TO SW30               .DUE IN 30?
.         IF        EQUAL                     .        YES
.         MATCH     YES TO LSTMSW            .LIST MANAGEMENT?
.         IF        EQUAL
.         ADD       UNBILINC TO UN30LM       .YES
..begin patch 3.73
.         if        (Ocompid2 = "P")
.         add       UnBilinc,UNPL30LM
.         else
.         add       UnBilinc,UNNIN30LM
.         endif
..end patch 3.73
.         ELSE
.         RESET     EXCODES                        NO
.         SCAN      OELCODE IN EXCODES           OK ITS BROKERAGE, EXCHANGE?
.         IF        EQUAL
.         ADD       UNBILINC TO UN30BE          YES.
..begin patch 3.73
.         if        (Ocompid = "P")
.         add       UnBilinc,UNPL30BE
.         else
.         add       UnBilinc,UNNIN30BE
.         endif
..end patch 3.73
.         COMPARE   C1 TO BATCHBR                 .BATCH ?
.           IF      EQUAL                         .YES
.           ADD     UNBILINC TO LRBBE
..begin patch 3.73
.                   if        (Ocompid = "P")
.                             ADD       UNBILINC TO LRPLBBE
.                             Else
.                             ADD       UNBILINC TO LRNINBBE
.                             endif
..end patch 3.73       
.           ENDIF
.         ELSE
.         ADD       UNBILINC TO UN30BR          NO ITS RENTAL.
..begin patch 3.73
.         if        (Ocompid = "P")
.         add       UnBilinc,UNPL30BR
.         else
.         add       UnBilinc,UNNIN30BR
.         endif
..end patch 3.73
.         COMPARE   C1 TO BATCHBR                 .BATCH ?
.           IF      EQUAL                         .YES
.           ADD     UNBILINC TO LRBBR
..begin patch 3.73
.                   if        (Ocompid = "P")
.                             ADD       UNBILINC TO LRPLBBR
.                             Else
.                             ADD       UNBILINC TO LRNINBBR
.                             endif
..end patch 3.73       
.           ENDIF
.         ENDIF
.         ENDIF
.         ENDIF
         RESET     EXCODES
         if        (unbilinc > 3000)
.Start patch 3.73
          Move      "Nord0013 - output",MailSubjct
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
          Append    OLRN,MailBody
          Append    B1,MailBody
          Append    "LR ##",MailBody
          Append    CRLF,MailBOdy
.
          Append    "LR INcome > 3000 ",MailBody
          Append    UnBilinc,MailBody
          Append    CRLF,MailBOdy
.
          Append    "Order Quantity = ",MailBody
          Append    Oqty,MailBody
          Append    CRLF,MailBOdy
          Reset     MailBody
.               move          "Nord0013 - output" to SmtpSubject Subject
.               Clear         SmtpTextMessage(1)   Array <Text message >
.              append         OLRN,SmtpTextMessage(1)   Array <Text message >
.              append         b1,SmtpTextMessage(1)   Array <Text message >
.              append         "LR ##",SmtpTextMessage(1)   Array <Text message >
.               reset          smtpTextMessage(1)
.
.               Clear         SmtpTextMessage(2)   Array <Text message >
.              append         "LR INCOME > 3000",SmtpTextMessage(2)   Array <Text message >
.              append         b1,SmtpTextMessage(2)   Array <Text message >
.              append         Unbilinc,SmtpTextMessage(2)   Array <Text message >
.               reset          smtpTextMessage(2)

.               Clear         SmtpTextMessage(3)   Array <Text message >
.              append         "Order Quantity = ",SmtpTextMessage(3)   Array <Text message >
.              append         Oqty,SmtpTextMessage(3)   Array <Text message >
.               reset          smtpTextMessage(3)

.              Move       "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
          move      c3,MailType
          Call      SendMail
.end patch 3.73
              winshow
         endif
         SCAN      OELCODE IN EXCODES
         GOTO      CHECKOK IF EQUAL
         CLEAR     EXCHANGE
         GOTO      DISSCOM
CHECKOK
         MOVE      "EXCHANGE" TO EXCHANGE
DISSCOM
         CLEAR     COMSLCT
         CMATCH    "C",OCOMSLCT
         IF        EQUAL
         MOVE      "COMSELECT" TO COMSLCT
        ENDIF
         CMATCH    "L",OCOMSLCT
         IF        EQUAL
         MOVE      "LIFESTYLE" TO COMSLCT
         ENDIF
         CMATCH    "I",OCOMSLCT
         IF        EQUAL
         MOVE      "IC SYSTEMS" TO COMSLCT
         ENDIF
         NORETURN
.check for maildate age if >= 2wks flag it.   dlh 20may93.
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         MOVE      C0 TO AGEFLAG
         MOVE      SYSJDATE TO CHKJUL
         SUBTRACT  JULDAYS FROM CHKJUL
         COMPARE   "13" TO CHKJUL
         IF        GREATER
         MOVE      C1 TO AGEFLAG
         ENDIF
         GOTO      PRINT
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         MOVE      C0 TO BATCHBR
         CALL      NMLRKEY
         CMATCH    "B" TO MCODE          .BATCH BILL ?
         IF        EQUAL                 .YES
           MOVE      C1 TO BATCHBR
           ELSE
         CMATCH    "A" TO MCODE          .BATCH BILL ?
         IF        EQUAL                 .YES
           MOVE      C1 TO BATCHBR
           else
           CLEAR   MCODE                   .MCODE WITH 'B' PRINTED ELSE IGNORE
           ENDIF
           endif
.begin patch 3.95
           if         (lstMSW <> Yes)  is this a brokerage client?
           call       debug
                  Move      "B",src    
                  move       cc to ccrep
                  move       mm to mmrep
                  move       yy to yyrep
                 unpack    COMPCNTDATE INTO cc,yy,mm,dd
                  move      compnum to cid
                  move      cid to cidr
                  Move      "R",type     
                  move      type,typer
                  move      src,srcr
                  move     "31" to dd
                    type       yy
                    if        equal
                    CALL      CVTJUL
                    MOVE      juldays TO revdat
                    move      revdat to check1
                    move      today1 to check2
                    SUB       check1 FROM CHECK2
                    compare   "365" to check2           usage in last year
                    if LESS
                                         move       yes to newbiz
                                         return
                                         endif
                                         endif
                    move      ccrep to cc
                    move      mmrep to mm
                    move      yyrep to yy
           eLSE
                  Move      "M",src    
                  move      OLNUM,cid
                  move      cid to cidr
                  move      b1,type
                  move      type,typer
                  move      OLNUM,cid
           endif
           call     projread
.end patch 3.95
         RETURN
.
.
READSHIP CLEAR     SHIPPED
         MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
         RETURN    IF OVER
         MOVE      "*SHIPPED*" TO SHIPPED
.Start Patch #2.9 - replaced var
.PREPARING FOR FUTURE INCREASE OF SQUANT
.         MOVE      C0 TO N7
.         MOVE      SQUANT TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO N9
         MOVE      SQUANT TO N9
         COMPARE   C0 TO N9
.END Patch #2.9 - replaced var
         IF        NOT EQUAL
         MOVE      SQUANT TO OQTY
         ENDIF
         RETURN
READMRG
         MOVE      OLRN TO NMRGFLD
         rep       zfill in nmrgfld
         MOVE      C1 TO NMRGPATH
         CALL      NMRGKEY
         RETURN    IF OVER
         move      nmrgiqty to oqty
         RETURN
special
.         move       no to specl
.         MATCH     "0020" TO OMLRNUM
.         IF         EQUAL
.         MOVE       C5 TO FORM52         *30jan02 turned off. full pricing 2/01.
.         move       yes to specl
.         return
.         ENDIF
.
         move       no to specl
         MATCH     "0170" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         MATCH     "0173" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

.Start patch 3.61
.         MATCH     "0179" TO OMLRNUM
.         IF         EQUAL
.         MOVE       C5 TO FORM52
.         move       yes to specl
.         return
.         ENDIF
.End patch 3.61

         MATCH     "0188" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         match      "0171" to obrknum
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF
.
         return
.......................................................................
BALAN
.MONEY ON ACCOUNT ?     *ADD IN THE FUTURE??????
         pack      nmoafld4 from obrknum,omlrnum
.         MOVE      MKEY TO CHKMLR
.         RESET     MKEY TO 4
.         APPEND    Z3 TO MKEY
.         RESET     MKEY
         move      c2 to nmobpath
         call      nmobkey
.         READ      ACCOUNT1,MKEY;MKEY,BALANCE
         COMPARE   C0 TO BALANCE
         GOTO      BALAN1 IF EQUAL
         GOTO      BALAN1 IF NOT LESS
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
BALAN1
          goto      print
.         READKS    ACCOUNT1;MKEY,BALANCE
.         GOTO      PRINT IF OVER
         MOVE      MKEY TO COMPMLR
         MATCH     CHKMLR TO COMPMLR
         GOTO      BALAN1 IF NOT EQUAL
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
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
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",OLRN
.         NORETURN                                POP THE STACK.
.         GOTO      GETREC                       GO BACK TO READ.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  SEQ BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
..
*......................................................................
HEADER   ADD       C1 TO PAGE
         PRINT     *F,*1,"CONFIDENTIAL",*32,"N I N   U N B I L L":
                   " E D   O R D E R S   A S   O F",*84,"DATE:",PDATE:
                   *L,*1,PROGRAM,*121,"PAGE ## ",PAGE:
                   *L,*L,*7,"ORDER DATE",*23,"MLR##",*32,"LR":
                   *40,"MAILER NAME/OFFER",*68,"LIST OWNER##":
                   *87,"LIST OWNER NAME",*114,"MAIL DTE",*124:
                   "RETURN DT"
         MOVE      C4 TO LINES
         RETURN
*......................................................................
PRINT
         ADD       UNBILINC TO UNBILTOT
.begin patch 3.73
.begin patch 3.76
.         if        (OCompId = "P" or OCompID2 = "P")
          if        (OcompId = "P" or (Ocompid2 = "P" & LSTMSW = YES))
.end patch 3.76
          add       Unbilinc,UnbilPLTot
          else
          add       Unbilinc,UnbilNINTot
          endif
.end patch 3.73

.dave goes bad
           if         (printdetails = No)
           goto       getrec
           endif
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
.
.Start Patch #2.9 - add century
.         PRINT     *40,MCCTO:
.                   *L,*7,OODTEM,SLASH,OODTED,SLASH,OODTEY:
.                   *18,OMLRNUM,SLASH,OCOBN,*27,MCODE,*27,MCODE,*29,OLRN:
.                   *40,MCOMP,*74,OLON,*86,OWNOCPY,*114,OMDTEM,SLASH:
.                   OMDTED,SLASH,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
.                   SLASH,ORTNDTEY;
         PRINT     *40,MCCTO:
                   *L,*7,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                   *18,OMLRNUM,SLASH,OCOBN,*27,MCODE,*27,MCODE,*29,OLRN:
                   *40,MCOMP,*74,OLON,*86,OWNOCPY,*114,OMDTEM,SLASH:
                   OMDTED,SLASH,OMDTEC,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
                   SLASH,ORTNDTEC,ORTNDTEY;
.end Patch #2.9 - add century
         COMPARE   C1 TO AGEFLAG
         IF        EQUAL
.Start Patch #2.9 - add century
.         PRINT     *113,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY,STAR;
         PRINT     *113,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,STAR;
.END Patch #2.9 - add century
         add       c1 to lines
         ENDIF
.START PATCH 3.43 ADDED LOGIC
.         PRINT     *L,*8,EXCHANGE,*22,SHIPPED,*40,OODES,B1,EXCLPRT:
.                   *86,OWNCTN,B3,FORM52,"/M",onetper,"%",*119,UNBILINC
         PRINT     *L,*8,EXCHANGE,*22,SHIPPED,*40,OODES,B1,EXCLPRT:
                   *86,NFULCOMP,B3,FORM52,"/M",onetper,"%",*119,UNBILINC
.END PATCH 3.43 ADDED LOGIC
         IFZ       PC

        Clear   str6
                if      (OComMod = "1")
                move    "/M",str6
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,"@",OComPrc,str6,*109,"QUANTITY ";
                elseif  (OComMod = "2") 
                move    "/Flat",str6
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,"@",OComPrc,str6,*109,"QUANTITY ";
                else                            .all others     
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
                endif

.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
.Start Patch #2.9 - replaced var
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     *118,STR7;
.         PRINT     *118,STR7;
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     "/",STR7:
.                   *L,*1,*RPTCHAR,DASH:132
.         XIF
.         IFNZ      PC
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     STR7;
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     "/",STR7:
.                   *L,*1,"------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------"
..
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
         PRINT     *118,STR9;
         PRINT     *118,STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PRINT     "/",STR9:
                   *L,*1,*RPTCHAR,DASH:132
         XIF
         IFNZ      PC
        Clear   str6
                if      (OComMod = "1")
                move    "/M",str6
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,"@",OComPrc,str6,*109,"QUANTITY ";
                elseif  (OComMod = "2") 
                move    "/Flat",str6
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,"@",OComPrc,str6,*109,"QUANTITY ";
                else                            .all others     
                      PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
                endif


.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
         PRINT     STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PRINT     "/",STR9:
                   *L,*1,"------------------------------":
                   "------------------------------":
                   "------------------------------":
                   "------------------------------":
                   "------------"
.END Patch #2.9 - replaced var
         XIF
         ADD       C7 TO LINES
         GOTO      GETREC
* ***************************************************************************
*  EXIT
* ****************************************************************************
.
EXIT     BRANCH    JOBBR OF EXIT1,EXIT2
EXIT1    BRANCH    PASS OF EXIT1A,EXIT1B
EXIT1A   MOVE      C2 TO PASS
         DISPLAY   *P01:23,"PASS TWO"
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A RETURN DATE GREATER THAN *7* DAYS AFTER TO THE":
                   " REPORT DATE.",*L:
                   *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
.                   " WITH A RETURN DATE GREATER THAN *3* DAYS PRIOR TO THE":
.                   " REPORT DATE.",*L:
.                  *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         SUB       COUNTO FROM COUNTO
         PRINT     *F
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
*****************
*for testing only*************************************************
*****************
         MOVE      "151000" TO NORDFLD
         CALL      NORDTST
         GOTO      GETREC
EXIT1B   COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A MAIL DATE GREATER THAN THE REPORT DATE."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         PRINT     *F
         GOTO      EXIT3
EXIT2    DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"      TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"TO BE BILLED WITHIN 30 DAYS -    ",UNLNC30:
                   *60,"BROKERAGE RENTAL 30 DAY   - ",UN30BR:
                   *L,*1,"TO BE BILLED WITHIN 60 DAYS -    ",UNLNC60:
                   *60,"BROKERAGE EXCHANGE 30 DAY - ",UN30BE:
                   *L,*1,"TO BE BILLED WITHIN 90 DAYS -    ",UNLNC90:
                   *60,"LIST MANAGEMENT 30 DAY    - ",UN30LM:
                   *L,*1,"TO BE BILLED WITHIN 90+ DAYS-    ",UNLNC90P:
                   *L,*1,"30 DAY BROKERAGE RENTAL BATCH -  ",LRBBR:
.begin patch 3.73
.                   *L,*1,"30 DAY BROKERAGE EXCHANGE BATCH- ",LRBBE:
                   *L,*1,"30 DAY BROKERAGE EXCHANGE BATCH- ",LRBBE
          add       c7,lines                   
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*L,*1,"  NIN TOTAL UNBILLED INCOME - ",UNBILNINTOT:
                   *L,*1,"TO BE BILLED WITHIN 30 DAYS -    ",UNNIN30:
                   *60,"BROKERAGE RENTAL 30 DAY   - ",UNNIN30BR:
                   *L,*1,"TO BE BILLED WITHIN 60 DAYS -    ",UNNIN60:
                   *60,"BROKERAGE EXCHANGE 30 DAY - ",UNNIN30BE:
                   *L,*1,"TO BE BILLED WITHIN 90 DAYS -    ",UNNIN90:
                   *60,"LIST MANAGEMENT 30 DAY    - ",UNNIN30LM:
                   *L,*1,"TO BE BILLED WITHIN 90+ DAYS-    ",UNNIN90P
          add       c7,lines                   
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*L,*1,"  PL TOTAL UNBILLED INCOME - ",UNBILPLTOT:
                   *L,*1,"TO BE BILLED WITHIN 30 DAYS -    ",UNPL30:
                   *60,"BROKERAGE RENTAL 30 DAY   - ",UNPL30BR:
                   *L,*1,"TO BE BILLED WITHIN 60 DAYS -    ",UNPL60:
                   *60,"BROKERAGE EXCHANGE 30 DAY - ",UNPL30BE:
                   *L,*1,"TO BE BILLED WITHIN 90 DAYS -    ",UNPL90:
                   *60,"LIST MANAGEMENT 30 DAY    - ",UNPL30LM:
                   *L,*1,"TO BE BILLED WITHIN 90+ DAYS-    ",UNPL90P:
.end patch 3.73
                   *L,*1,"THIS REPORT INCLUDES ALL UNBILLED ORDERS":
                   *L,*1,"ASSUMPTIONS:   ":
                   *L,*1,"LIST MANAGEMENT RENTAL COMMISSION 10%,":
                   " BROKERAGE RENTAL COMMISSION 20%":
                   *L,*1,"IF SPLIT RENTAL/EXCHANGE $/M PULLED FROM DATACARD,":
                   " IF $/M NOT ON DATACARD $65/M IS USED.":
                   *L,*1,"EXCHANGE VOLUME RATES ARE ACCOUNTED FOR":
                   *L,*1,"SHIPPED QUANTITY USED IF AVAILABLE",*L:
                   *L,*1,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,*1,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         PRINT     *F
.Start patch 3.73
          Move      "Nord0013 - Totals output",MailSubjct
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
.begin patch 3.9
          append      "Total Unbilled AR             ",Mailbody  
          append      TAR,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "Total Unbilled AP             ",Mailbody  
          append      TAP,Mailbody 
          Append      CRLF,MailBOdy
.
          Move      ", ",str2
          append      "30, 60,90, 90+ AR Brokerage rental    ",Mailbody  
          append      AR30BR,MailBody
          append      Str2,Mailbody
          append      AR60BR,Mailbody
          append      str2,Mailbody
          append      AR90BR,Mailbody
          append     str2,Mailbody
          append     AR90PBR,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "30, 60,90, 90+ AR Brokerage Exchange  ",Mailbody  
          append      AR30BE,MailBody
          append      Str2,Mailbody
          append      AR60BE,Mailbody
          append      str2,Mailbody
          append      AR90BE,Mailbody
          append     str2,Mailbody
          append     AR90PBE,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "30, 60,90, 90+ day AR List Management     ",Mailbody  
          append      AR30LM,MailBody
          append      Str2,Mailbody
          append      AR60LM,Mailbody
          append      str2,Mailbody
          append      AR90LM,Mailbody
          append     str2,Mailbody
          append     AR90PLM,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "30, 60,90, 90+ AP Brokerage rental    ",Mailbody  
          append      AP30BR,MailBody
          append      Str2,Mailbody
          append      AP60BR,Mailbody
          append      str2,Mailbody
          append      AP90BR,Mailbody
          append     str2,Mailbody
          append     AP90PBR,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "30, 60,90, 90+ AP Brokerage Exchange  ",Mailbody  
          append      AP30BE,MailBody
          append      Str2,Mailbody
          append      AP60BE,Mailbody
          append      str2,Mailbody
          append      AP90BE,Mailbody
          append     str2,Mailbody
          append     AP90PBE,Mailbody 
          Append      CRLF,MailBOdy
.
          append      "30, 60,90, 90+ day AP List Management     ",Mailbody  
          append      AP30LM,MailBody
          append      Str2,Mailbody
          append      AP60LM,Mailbody
          append      str2,Mailbody
          append      AP90LM,Mailbody
          append     str2,Mailbody
          append     AP90PLM,Mailbody 
          Append      CRLF,MailBOdy

.end patch 3.9
            append      "Total Unbilled income       ",Mailbody  
            append      Unbiltot,Mailbody 
          Append    CRLF,MailBOdy
.
            append      "Total 30 day                ",Mailbody
          append      Unlnc30,Mailbody
          Append    CRLF,MailBOdy
.
            append         "30 day Brokerage rental     ",Mailbody   
            append         UN30BR,Mailbody   
          Append    CRLF,MailBOdy
.
            append         "30 day Brokerage Exchange   ",Mailbody   
            append         UN30BE,Mailbody   
          Append    CRLF,MailBOdy
.
            append         "30 day List Management      ",Mailbody   
            append         UN30LM,Mailbody   
          Append    CRLF,MailBOdy
.
              append         "60 day Total                ",Mailbody   
            Append         UNLNC60,Mailbody   
          Append    CRLF,MailBOdy
 
            Append         "90 day Total                ",Mailbody   
            Append         UNLNC90,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90+ day Total               ",Mailbody   
            Append         UNLNC90P,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "30 Batch Brokerage Rental   ",Mailbody   
            Append         LRBBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "30 Batch Brokerage Exchange ",Mailbody   
            Append         LRBBE,Mailbody   
          Append    CRLF,MailBOdy
.
.
            Append         "60  Brokerage Rent          ",Mailbody   
            Append         UNLNC60Br,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "60  Brokerage Exchange      ",Mailbody   
            Append         UNLNC60BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "60  Management              ",Mailbody   
            Append         UNLNC60LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90  Brokerage Rent          ",Mailbody   
            Append         UNLNC90BR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90  Brokerage Exchange      ",Mailbody   
            Append         UNLNC90BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90  Management              ",Mailbody   
            Append         UNLNC90LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90+  Brokerage Rent         ",Mailbody   
            Append         UNLNC90PBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90+  Brokerage Exchange     ",Mailbody   
            Append         UNLNC90PBE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "90+ Management              ",Mailbody   
            Append         UNLNC90PLM,Mailbody   
          Append    CRLF,MailBOdy
            Append         "                       ",Mailbody   
          Append    CRLF,MailBOdy

            Append         "Total PL Unbilled income       ",Mailbody   
            Append         UnbilPLtot,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL Total 30 day                ",Mailbody   
            Append         UnPL30,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 30 day Brokerage rental     ",Mailbody   
            Append         UNPL30BR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 30 day Brokerage Exchange   ",Mailbody   
            Append         UNPL30BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 30 day List Management      ",Mailbody   
            Append         UNPL30LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 60 day Total                ",Mailbody   
            Append         UNPL60,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90 day Total                ",Mailbody   
            Append         UNPL90,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90+ day Total               ",Mailbody   
            Append         UNPL90P,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 30 Batch Brokerage Rental   ",Mailbody   
            Append         LRPLBBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 30 Batch Brokerage Exchange ",Mailbody   
            Append         LRPLBBE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 60  Brokerage Rent          ",Mailbody   
            Append         UNPL60Br,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 60  Brokerage Exchange      ",Mailbody   
            Append         UNPL60BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 60  Management              ",Mailbody   
            Append         UNPL60LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90  Brokerage Rent          ",Mailbody   
            Append         UNPL90BR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90  Brokerage Exchange      ",Mailbody   
            Append         UNPL90BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90  Management              ",Mailbody   
            Append         UNPL90LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90+  Brokerage Rent         ",Mailbody   
            Append         UNPL90PBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90+  Brokerage Exchange     ",Mailbody   
            Append         UNPL90PBE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "PL 90+ Management              ",Mailbody   
            Append         UNPL90PLM,Mailbody   
          Append    CRLF,MailBOdy

            Append         "                       ",Mailbody   
          Append    CRLF,MailBOdy

            Append         "Total NIN Unbilled income       ",Mailbody   
            Append         UnbilNINtot,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN Total 30 day                ",Mailbody   
            Append         UnNIN30,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 30 day Brokerage rental     ",Mailbody   
            Append         UNNIN30BR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 30 day Brokerage Exchange   ",Mailbody   
            Append         UNNIN30BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 30 day List Management      ",Mailbody   
            Append         UNNIN30LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 60 day Total                ",Mailbody   
            Append         UNNIN60,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90 day Total                ",Mailbody   
            Append         UNNIN90,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90+ day Total               ",Mailbody   
            Append         UNNIN90P,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 30 Batch Brokerage Rental   ",Mailbody   
            Append         LRNINBBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 30 Batch Brokerage Exchange ",Mailbody   
            Append         LRNINBBE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 60  Brokerage Rent          ",Mailbody   
            Append         UNNIN60Br,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 60  Brokerage Exchange      ",Mailbody   
            Append         UNNIN60BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 60  Management              ",Mailbody   
            Append         UNNIN60LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90  Brokerage Rent          ",Mailbody   
            Append         UNNIN90BR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90  Brokerage Exchange      ",Mailbody   
            Append         UNNIN90BE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90  Management              ",Mailbody   
            Append         UNNIN90LM,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90+  Brokerage Rent         ",Mailbody   
            Append         UNNIN90PBR,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90+  Brokerage Exchange     ",Mailbody   
            Append         UNNIN90PBE,Mailbody   
          Append    CRLF,MailBOdy
.
            Append         "NIN 90+ Management              ",Mailbody   
            Append         UNNIN90PLM,Mailbody   
          Append    CRLF,MailBOdy

.              move          "Nord0013 - Totals output" to SmtpSubject Subject
.
.              Clear          SmtpTextMessage(1)
.              append         "Total Unbilled income       ",SmtpTextMessage(1)   Array <Text message >
.              append         Unbiltot,SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)
.
.              Clear          SmtpTextMessage(2)
.              append         "Total 30 day                ",SmtpTextMessage(2)   Array <Text message >
.              append         Unlnc30,SmtpTextMessage(2)   Array <Text message >
.              reset          smtpTextMessage(2)
.
.              Clear          SmtpTextMessage(3)
.              append         "30 day Brokerage rental     ",SmtpTextMessage(3)   Array <Text message >
.              append         UN30BR,SmtpTextMessage(3)   Array <Text message >
.              reset          smtpTextMessage(3)
.
.              Clear          SmtpTextMessage(4)
.              append         "30 day Brokerage Exchange   ",SmtpTextMessage(4)   Array <Text message >
.              append         UN30BE,SmtpTextMessage(4)   Array <Text message >
.              reset          smtpTextMessage(4)
.
.              Clear          SmtpTextMessage(5)
.              append         "30 day List Management      ",SmtpTextMessage(5)   Array <Text message >
.              append         UN30LM,SmtpTextMessage(5)   Array <Text message >
.              reset          smtpTextMessage(5)
..
.              Clear          SmtpTextMessage(6)
.              append         "60 day Total                ",SmtpTextMessage(6)   Array <Text message >
.              append         UNLNC60,SmtpTextMessage(6)   Array <Text message >
.              reset          smtpTextMessage(6)
..
.              Clear          SmtpTextMessage(7)
.              append         "90 day Total                ",SmtpTextMessage(7)   Array <Text message >
.              append         UNLNC90,SmtpTextMessage(7)   Array <Text message >
.              reset          smtpTextMessage(7)
..
.              Clear          SmtpTextMessage(8)
.              append         "90+ day Total               ",SmtpTextMessage(8)   Array <Text message >
.              append         UNLNC90P,SmtpTextMessage(8)   Array <Text message >
.              reset          smtpTextMessage(8)
..
.              Clear          SmtpTextMessage(9)
.              append         "30 Batch Brokerage Rental   ",SmtpTextMessage(9)   Array <Text message >
.              append         LRBBR,SmtpTextMessage(9)   Array <Text message >
.              reset          smtpTextMessage(9)
..
.              Clear          SmtpTextMessage(10)
.              append         "30 Batch Brokerage Exchange ",SmtpTextMessage(10)   Array <Text message >
.              append         LRBBE,SmtpTextMessage(10)   Array <Text message >
.              reset          smtpTextMessage(10)
..
.;begin patch 3.55
..
.              Clear          SmtpTextMessage(11)
.              append         "60  Brokerage Rent          ",SmtpTextMessage(11)   Array <Text message >
.              append         UNLNC60Br,SmtpTextMessage(11)   Array <Text message >
.              reset          smtpTextMessage(11)
..
.              Clear          SmtpTextMessage(12)
.              append         "60  Brokerage Exchange      ",SmtpTextMessage(12)   Array <Text message >
.              append         UNLNC60BE,SmtpTextMessage(12)   Array <Text message >
.              reset          smtpTextMessage(12)
..
.              Clear          SmtpTextMessage(13)
.              append         "60  Management              ",SmtpTextMessage(13)   Array <Text message >
.              append         UNLNC60LM,SmtpTextMessage(13)   Array <Text message >
.              reset          smtpTextMessage(13)
..
.              Clear          SmtpTextMessage(14)
.              append         "90  Brokerage Rent          ",SmtpTextMessage(14)   Array <Text message >
.              append         UNLNC90BR,SmtpTextMessage(14)   Array <Text message >
.              reset          smtpTextMessage(14)
..
.              Clear          SmtpTextMessage(15)
.              append         "90  Brokerage Exchange      ",SmtpTextMessage(15)   Array <Text message >
.              append         UNLNC90BE,SmtpTextMessage(15)   Array <Text message >
.              reset          smtpTextMessage(15)
..
.              Clear          SmtpTextMessage(16)
.              append         "90  Management              ",SmtpTextMessage(16)   Array <Text message >
.              append         UNLNC90LM,SmtpTextMessage(16)   Array <Text message >
.              reset          smtpTextMessage(16)
..
.              Clear          SmtpTextMessage(17)
.              append         "90+  Brokerage Rent         ",SmtpTextMessage(17)   Array <Text message >
.              append         UNLNC90PBR,SmtpTextMessage(17)   Array <Text message >
.              reset          smtpTextMessage(17)
..
.              Clear          SmtpTextMessage(18)
.              append         "90+  Brokerage Exchange     ",SmtpTextMessage(18)   Array <Text message >
.              append         UNLNC90PBE,SmtpTextMessage(18)   Array <Text message >
.              reset          smtpTextMessage(18)
..
.              Clear          SmtpTextMessage(19)
.              append         "90+ Management              ",SmtpTextMessage(19)   Array <Text message >
.              append         UNLNC90PLM,SmtpTextMessage(19)   Array <Text message >
.              reset          smtpTextMessage(19)
..begin patch 3.73
.              Clear          SmtpTextMessage(20)
.              append         "                       ",SmtpTextMessage(20)   Array <Text message >
.              reset          smtpTextMessage(20)
.
.              Clear          SmtpTextMessage(21)
.              append         "Total PL Unbilled income       ",SmtpTextMessage(21)   Array <Text message >
.              append         UnbilPLtot,SmtpTextMessage(21)   Array <Text message >
.              reset          smtpTextMessage(21)
..
.              Clear          SmtpTextMessage(22)
.              append         "PL Total 30 day                ",SmtpTextMessage(22)   Array <Text message >
.              append         UnPL30,SmtpTextMessage(22)   Array <Text message >
.              reset          smtpTextMessage(22)
..
.              Clear          SmtpTextMessage(23)
.              append         "PL 30 day Brokerage rental     ",SmtpTextMessage(23)   Array <Text message >
.              append         UNPL30BR,SmtpTextMessage(23)   Array <Text message >
.              reset          smtpTextMessage(23)
..
.              Clear          SmtpTextMessage(24)
.              append         "PL 30 day Brokerage Exchange   ",SmtpTextMessage(24)   Array <Text message >
.              append         UNPL30BE,SmtpTextMessage(24)   Array <Text message >
.              reset          smtpTextMessage(24)
..
.              Clear          SmtpTextMessage(25)
.              append         "PL 30 day List Management      ",SmtpTextMessage(25)   Array <Text message >
.              append         UNPL30LM,SmtpTextMessage(25)   Array <Text message >
.              reset          smtpTextMessage(25)
..
.              Clear          SmtpTextMessage(26)
.              append         "PL 60 day Total                ",SmtpTextMessage(26)   Array <Text message >
.              append         UNPL60,SmtpTextMessage(26)   Array <Text message >
.              reset          smtpTextMessage(26)
..
.              Clear          SmtpTextMessage(27)
.              append         "PL 90 day Total                ",SmtpTextMessage(27)   Array <Text message >
.              append         UNPL90,SmtpTextMessage(27)   Array <Text message >
.              reset          smtpTextMessage(27)
..
.              Clear          SmtpTextMessage(28)
.              append         "PL 90+ day Total               ",SmtpTextMessage(28)   Array <Text message >
.              append         UNPL90P,SmtpTextMessage(28)   Array <Text message >
.              reset          smtpTextMessage(28)
..
.              Clear          SmtpTextMessage(29)
.              append         "PL 30 Batch Brokerage Rental   ",SmtpTextMessage(29)   Array <Text message >
..              append         LRBBR,SmtpTextMessage(29)   Array <Text message >
.              reset          smtpTextMessage(29)
..
.              Clear          SmtpTextMessage(30)
.              append         "PL 30 Batch Brokerage Exchange ",SmtpTextMessage(30)   Array <Text message >
..              append         LRBBE,SmtpTextMessage(30)   Array <Text message >
.              reset          smtpTextMessage(30)
..
.              Clear          SmtpTextMessage(31)
.              append         "PL 60  Brokerage Rent          ",SmtpTextMessage(31)   Array <Text message >
.              append         UNPL60Br,SmtpTextMessage(31)   Array <Text message >
.              reset          smtpTextMessage(31)
..
.              Clear          SmtpTextMessage(32)
.              append         "PL 60  Brokerage Exchange      ",SmtpTextMessage(32)   Array <Text message >
.              append         UNPL60BE,SmtpTextMessage(32)   Array <Text message >
.              reset          smtpTextMessage(32)
..
.              Clear          SmtpTextMessage(33)
.              append         "PL 60  Management              ",SmtpTextMessage(33)   Array <Text message >
.              append         UNPL60LM,SmtpTextMessage(33)   Array <Text message >
.              reset          smtpTextMessage(33)
..
.              Clear          SmtpTextMessage(34)
.              append         "PL 90  Brokerage Rent          ",SmtpTextMessage(34)   Array <Text message >
.              append         UNPL90BR,SmtpTextMessage(34)   Array <Text message >
.              reset          smtpTextMessage(34)
..
.              Clear          SmtpTextMessage(35)
.              append         "PL 90  Brokerage Exchange      ",SmtpTextMessage(35)   Array <Text message >
.              append         UNPL90BE,SmtpTextMessage(35)   Array <Text message >
.              reset          smtpTextMessage(35)
..
.              Clear          SmtpTextMessage(36)
.              append         "PL 90  Management              ",SmtpTextMessage(36)   Array <Text message >
.              append         UNPL90LM,SmtpTextMessage(36)   Array <Text message >
.              reset          smtpTextMessage(36)
..
.              Clear          SmtpTextMessage(37)
.              append         "PL 90+  Brokerage Rent         ",SmtpTextMessage(37)   Array <Text message >
.              append         UNPL90PBR,SmtpTextMessage(37)   Array <Text message >
.              reset          smtpTextMessage(37)
..
.              Clear          SmtpTextMessage(38)
.              append         "PL 90+  Brokerage Exchange     ",SmtpTextMessage(38)   Array <Text message >
.              append         UNPL90PBE,SmtpTextMessage(38)   Array <Text message >
.              reset          smtpTextMessage(38)
..
.              Clear          SmtpTextMessage(39)
.              append         "PL 90+ Management              ",SmtpTextMessage(39)   Array <Text message >
.              append         UNPL90PLM,SmtpTextMessage(39)   Array <Text message >
.              reset          smtpTextMessage(39)
.
..begin patch 3.73
.              Clear          SmtpTextMessage(40)
.              append         "                       ",SmtpTextMessage(40)   Array <Text message >
.              reset          smtpTextMessage(40)
.
.              Clear          SmtpTextMessage(41)
.              append         "Total NIN Unbilled income       ",SmtpTextMessage(41)   Array <Text message >
.              append         UnbilNINtot,SmtpTextMessage(41)   Array <Text message >
.              reset          smtpTextMessage(41)
..
.              Clear          SmtpTextMessage(42)
.              append         "NIN Total 30 day                ",SmtpTextMessage(42)   Array <Text message >
.              append         UnNIN30,SmtpTextMessage(42)   Array <Text message >
.              reset          smtpTextMessage(42)
..
.              Clear          SmtpTextMessage(43)
.              append         "NIN 30 day Brokerage rental     ",SmtpTextMessage(43)   Array <Text message >
.              append         UNNIN30BR,SmtpTextMessage(43)   Array <Text message >
.              reset          smtpTextMessage(43)
..
.              Clear          SmtpTextMessage(44)
.              append         "NIN 30 day Brokerage Exchange   ",SmtpTextMessage(44)   Array <Text message >
.              append         UNNIN30BE,SmtpTextMessage(44)   Array <Text message >
.              reset          smtpTextMessage(44)
..
.              Clear          SmtpTextMessage(45)
.              append         "NIN 30 day List Management      ",SmtpTextMessage(45)   Array <Text message >
.              append         UNNIN30LM,SmtpTextMessage(45)   Array <Text message >
.              reset          smtpTextMessage(45)
..
.              Clear          SmtpTextMessage(46)
.              append         "NIN 60 day Total                ",SmtpTextMessage(46)   Array <Text message >
.              append         UNNIN60,SmtpTextMessage(46)   Array <Text message >
.              reset          smtpTextMessage(46)
..
.              Clear          SmtpTextMessage(47)
.              append         "NIN 90 day Total                ",SmtpTextMessage(47)   Array <Text message >
.              append         UNNIN90,SmtpTextMessage(47)   Array <Text message >
.              reset          smtpTextMessage(47)
..
.              Clear          SmtpTextMessage(48)
.              append         "NIN 90+ day Total               ",SmtpTextMessage(48)   Array <Text message >
.              append         UNNIN90P,SmtpTextMessage(48)   Array <Text message >
.              reset          smtpTextMessage(48)
..
.              Clear          SmtpTextMessage(49)
.              append         "NIN 30 Batch Brokerage Rental   ",SmtpTextMessage(49)   Array <Text message >
..              append         LRBBR,SmtpTextMessage(49)   Array <Text message >
.              reset          smtpTextMessage(49)
..
.              Clear          SmtpTextMessage(50)
.              append         "NIN 30 Batch Brokerage Exchange ",SmtpTextMessage(50)   Array <Text message >
..              append         LRBBE,SmtpTextMessage(50)   Array <Text message >
.              reset          smtpTextMessage(50)
..
.              Clear          SmtpTextMessage(51)
.              append         "NIN 60  Brokerage Rent          ",SmtpTextMessage(51)   Array <Text message >
.              append         UNNIN60Br,SmtpTextMessage(51)   Array <Text message >
.              reset          smtpTextMessage(51)
..
.              Clear          SmtpTextMessage(52)
.              append         "NIN 60  Brokerage Exchange      ",SmtpTextMessage(52)   Array <Text message >
.              append         UNNIN60BE,SmtpTextMessage(52)   Array <Text message >
.              reset          smtpTextMessage(52)
..
.              Clear          SmtpTextMessage(53)
.              append         "NIN 60  Management              ",SmtpTextMessage(53)   Array <Text message >
.              append         UNNIN60LM,SmtpTextMessage(53)   Array <Text message >
.              reset          smtpTextMessage(53)
..
.              Clear          SmtpTextMessage(54)
.              append         "NIN 90  Brokerage Rent          ",SmtpTextMessage(54)   Array <Text message >
.              append         UNNIN90BR,SmtpTextMessage(54)   Array <Text message >
.              reset          smtpTextMessage(54)
..
.              Clear          SmtpTextMessage(55)
.              append         "NIN 90  Brokerage Exchange      ",SmtpTextMessage(55)   Array <Text message >
.              append         UNNIN90BE,SmtpTextMessage(55)   Array <Text message >
.              reset          smtpTextMessage(55)
..
.              Clear          SmtpTextMessage(56)
.              append         "NIN 90  Management              ",SmtpTextMessage(56)   Array <Text message >
.              append         UNNIN90LM,SmtpTextMessage(56)   Array <Text message >
.              reset          smtpTextMessage(56)
..
.              Clear          SmtpTextMessage(57)
.              append         "NIN 90+  Brokerage Rent         ",SmtpTextMessage(57)   Array <Text message >
.              append         UNNIN90PBR,SmtpTextMessage(57)   Array <Text message >
.              reset          smtpTextMessage(57)
..
.              Clear          SmtpTextMessage(58)
.              append         "NIN 90+  Brokerage Exchange     ",SmtpTextMessage(58)   Array <Text message >
.              append         UNNIN90PBE,SmtpTextMessage(58)   Array <Text message >
.              reset          smtpTextMessage(58)
..
.              Clear          SmtpTextMessage(59)
.              append         "NIN 90+ Management              ",SmtpTextMessage(59)   Array <Text message >
.              append         UNNIN90PLM,SmtpTextMessage(59)   Array <Text message >
.              reset          smtpTextMessage(59)
.
.;              Move       "10",SmtpTextIndexLast                               Index to last entry in TextMessage array
..              Move       "19",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              Move       "59",SmtpTextIndexLast                               Index to last entry in TextMessage array
.end patch 3.73
.begin patch 3.55
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
          Reset     Mailbody
          move      c3,MailType
          call      sendmail
              winshow
.begin patch 3.6
.Open Excel application
OhMy
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
              Create  ex
              pack  str4,cc,yy
              rep   zfill,str4
.              pack            Taskname,"\\nins1\d\users\dherric\monday2009.xls"
.begin patch 3.79
.              pack            Taskname,"\\nins1\d\accounting\monday2009.xls"
.          if        (yy = "13")
.              pack            Taskname,"\\nins1\d\accounting\monday",str4,".xls"       
.          else
              pack            Taskname,"\\nins1\d\accounting\monday2016.xlsx"       
.              endif
.end patch 3.79
              getprop ex,*Workbooks=books
              Books.Open using *Filename=taskname
              books.item giving book using 1
              getprop book,*Sheets=sheets
.              getprop book,*workSheets=sheets
.Reset Default of Worksheets found in a Workbook         .eight
.should try getting the property here and reseting it when done.
        getprop ex,*SheetsInNewWorkbook=NumberofSheets
.Create Workbooks collection
.        clock   timestamp,str8
.        unpack  str8,str2,yy,mm,dd
        UNPACK    PDATE INTO MM,STR1,DD,STR1,YY
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        clock   time to time
        move      mm to sheetno
              if              (sheetno = c1)
              sheets.item giving sheet using 1
              Elseif          (sheetno = c2)
              sheets.item giving sheet using 2
              Elseif          (sheetno = c3)
              sheets.item giving sheet using 3
              Elseif          (sheetno = c4)
              sheets.item giving sheet using 4
              Elseif          (sheetno = c5)
              sheets.item giving sheet using 5
              Elseif          (sheetno = c6)
              sheets.item giving sheet using 6
              Elseif          (sheetno = c7)
              sheets.item giving sheet using 7
              Elseif          (sheetno = c8)
              sheets.item giving sheet using 8
              Elseif          (sheetno = c9)
              sheets.item giving sheet using 9
              Elseif          (sheetno = 10)
              sheets.item giving sheet using 10
              Elseif          (sheetno = 11)
              sheets.item giving sheet using 11
              Elseif          (sheetno = 12)
              sheets.item giving sheet using 12
              endif
        pack    RowNumber,"C","14"
        setprop sheet.range(RowNumber),*Value=str10
        pack    RowNumber,"D","14"
        setprop sheet.range(RowNumber),*Value=Time
        pack    RowNumber,"A","14"
        setprop sheet.range(RowNumber),*Value="Unbilled Total"
        pack    RowNumber,"B","14"
        setprop sheet.range(RowNumber),*Value=UnbilTot,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","14"
        setprop sheet.range(RowNumber),*Value=UnbilNINTot,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","14"
        setprop sheet.range(RowNumber),*Value=UnbilPLTot,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","15"
        setprop sheet.range(RowNumber),*Value="30 day Total"
        pack    RowNumber,"B","15"
        setprop sheet.range(RowNumber),*Value=Unlnc30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","15"
        setprop sheet.range(RowNumber),*Value=UnNIN30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","15"
        setprop sheet.range(RowNumber),*Value=UnPl30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","16"
        setprop sheet.range(RowNumber),*Value="60 day Total"
        pack    RowNumber,"B","16"
        setprop sheet.range(RowNumber),*Value=UnLnc60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","16"
        setprop sheet.range(RowNumber),*Value=UnNIN60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","16"
        setprop sheet.range(RowNumber),*Value=UnPL60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","17"
        setprop sheet.range(RowNumber),*Value="90 day Total"
        pack    RowNumber,"B","17"
        setprop sheet.range(RowNumber),*Value=uNLNC90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","17"
        setprop sheet.range(RowNumber),*Value=uNNIN90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","17"
        setprop sheet.range(RowNumber),*Value=uNPL90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","18"
        setprop sheet.range(RowNumber),*Value="90+ day Total"
        pack    RowNumber,"B","18"
        setprop sheet.range(RowNumber),*Value=Unlnc90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","18"
        setprop sheet.range(RowNumber),*Value=UnNIN90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","18"
        setprop sheet.range(RowNumber),*Value=UnPl90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","19"
        setprop sheet.range(RowNumber),*Value="30 Brokerage rent"
        pack    RowNumber,"B","19"
        setprop sheet.range(RowNumber),*Value=UN30BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","19"
        setprop sheet.range(RowNumber),*Value=UNNIN30BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","19"
        setprop sheet.range(RowNumber),*Value=UNPL30BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","20"
        setprop sheet.range(RowNumber),*Value="30 Brokerage Exch"
        pack    RowNumber,"B","20"
        setprop sheet.range(RowNumber),*Value=UN30BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","20"
        setprop sheet.range(RowNumber),*Value=UNNIN30BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","20"
        setprop sheet.range(RowNumber),*Value=UNPL30BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","21"
        setprop sheet.range(RowNumber),*Value="30 List Management"
        pack    RowNumber,"B","21"
        setprop sheet.range(RowNumber),*Value=UN30LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","21"
        setprop sheet.range(RowNumber),*Value=UNNIN30LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","21"
        setprop sheet.range(RowNumber),*Value=UNPL30LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","22"
        setprop sheet.range(RowNumber),*Value="60 Brokerage rent"
        pack    RowNumber,"B","22"
        setprop sheet.range(RowNumber),*Value=Unlnc60br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","22"
        setprop sheet.range(RowNumber),*Value=UnNIN60br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","22"
        setprop sheet.range(RowNumber),*Value=UnPl60br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","23"
        setprop sheet.range(RowNumber),*Value="60 Brokerage Exch"
        pack    RowNumber,"B","23"
        setprop sheet.range(RowNumber),*Value=Unlnc60be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","23"
        setprop sheet.range(RowNumber),*Value=UnNIN60be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","23"
        setprop sheet.range(RowNumber),*Value=UnPl60be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","24"
        setprop sheet.range(RowNumber),*Value="60 List Management"
        pack    RowNumber,"B","24"
        setprop sheet.range(RowNumber),*Value=Unlnc60lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","24"
        setprop sheet.range(RowNumber),*Value=UnNIN60lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","24"
        setprop sheet.range(RowNumber),*Value=UnPl60lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","25"
        setprop sheet.range(RowNumber),*Value="90 Brokerage rent"
        pack    RowNumber,"B","25"
        setprop sheet.range(RowNumber),*Value=Unlnc90br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","25"
        setprop sheet.range(RowNumber),*Value=UnNIN90br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","25"
        setprop sheet.range(RowNumber),*Value=UnPl90br,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","26"
        setprop sheet.range(RowNumber),*Value="90 Brokerage Exch"
        pack    RowNumber,"B","26"
        setprop sheet.range(RowNumber),*Value=Unlnc90be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","26"
        setprop sheet.range(RowNumber),*Value=UnNIN90be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","26"
        setprop sheet.range(RowNumber),*Value=UnPl90be,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","27"
        setprop sheet.range(RowNumber),*Value="90 List Management"
        pack    RowNumber,"B","27"
        setprop sheet.range(RowNumber),*Value=Unlnc90lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","27"
        setprop sheet.range(RowNumber),*Value=UnNIN90lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","27"
        setprop sheet.range(RowNumber),*Value=UnPl90lm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","28"
        setprop sheet.range(RowNumber),*Value="90+ Brokerage rent"
        pack    RowNumber,"B","28"
        setprop sheet.range(RowNumber),*Value=UNLNC90PBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","28"
        setprop sheet.range(RowNumber),*Value=UNNIN90PBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","28"
        setprop sheet.range(RowNumber),*Value=UNPL90PBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","29"
        setprop sheet.range(RowNumber),*Value="90+ Brokerage Exch"
        pack    RowNumber,"B","29"
        setprop sheet.range(RowNumber),*Value=Unlnc90pbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","29"
        setprop sheet.range(RowNumber),*Value=UnNIN90pbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","29"
        setprop sheet.range(RowNumber),*Value=UnPl90pbe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","30"
        setprop sheet.range(RowNumber),*Value="90+ List Management"
        pack    RowNumber,"B","30"
        setprop sheet.range(RowNumber),*Value=Unlnc90plm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73
        pack    RowNumber,"E","30"
        setprop sheet.range(RowNumber),*Value=UnNIN90plm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","30"
        setprop sheet.range(RowNumber),*Value=UnPl90plm,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","31"
        setprop sheet.range(RowNumber),*Value="30 day batch Rent"
        pack    RowNumber,"B","31"
        setprop sheet.range(RowNumber),*Value=LrbBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73 ----- variables not save or calced yet
        pack    RowNumber,"E","31"
        setprop sheet.range(RowNumber),*Value=LRNINBBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","31"
        setprop sheet.range(RowNumber),*Value=LRPLBBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
        pack    RowNumber,"A","32"
        setprop sheet.range(RowNumber),*Value="30 day batch Exch"
        pack    RowNumber,"B","32"
        setprop sheet.range(RowNumber),*Value=LrbBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.73 ----- variables not save or calced yet
        pack    RowNumber,"E","32"
        setprop sheet.range(RowNumber),*Value=LrNINbBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","32"
        setprop sheet.range(RowNumber),*Value=LrPLbBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.73
.begin patch 3.9
        pack    RowNumber,"A","63"
        setprop sheet.range(RowNumber),*Value="Total Unbilled AR"
        pack    RowNumber,"B","63"
        setprop sheet.range(RowNumber),*Value=TAR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","64"
        setprop sheet.range(RowNumber),*Value="Total Unbilled AP"
        pack    RowNumber,"B","64"
        setprop sheet.range(RowNumber),*Value=TAP,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","65"
        setprop sheet.range(RowNumber),*Value="Brkr Rent AR 30, 60, 90,90+ "
        pack    RowNumber,"B","65"
        setprop sheet.range(RowNumber),*Value=AR30BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","65"
        setprop sheet.range(RowNumber),*Value=AR60BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","65"
        setprop sheet.range(RowNumber),*Value=AR90BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","65"
        setprop sheet.range(RowNumber),*Value=AR90PBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","66"
        setprop sheet.range(RowNumber),*Value="Brkr Exch AR 30, 60, 90, 90+ "
        pack    RowNumber,"B","66"
        setprop sheet.range(RowNumber),*Value=AR30BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","66"
        setprop sheet.range(RowNumber),*Value=AR60BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","66"
        setprop sheet.range(RowNumber),*Value=AR90BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","66"
        setprop sheet.range(RowNumber),*Value=AR90PBE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","67"
        setprop sheet.range(RowNumber),*Value="Management AR 30, 60, 90, 90+ "
        pack    RowNumber,"B","67"
        setprop sheet.range(RowNumber),*Value=AR30LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","67"
        setprop sheet.range(RowNumber),*Value=AR60LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","67"
        setprop sheet.range(RowNumber),*Value=AR90LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","67"
        setprop sheet.range(RowNumber),*Value=AR90PLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","68"
        setprop sheet.range(RowNumber),*Value="Brkr Rent AP 30, 60, 90,90+ "
        pack    RowNumber,"B","68"
        setprop sheet.range(RowNumber),*Value=AP30BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","68"
        setprop sheet.range(RowNumber),*Value=AP60BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","68"
        setprop sheet.range(RowNumber),*Value=AP90BR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","68"
        setprop sheet.range(RowNumber),*Value=AP90PBR,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","69"
        setprop sheet.range(RowNumber),*Value="Brkr Exch AP 30, 60, 90, 90+ "
        pack    RowNumber,"B","69"
        setprop sheet.range(RowNumber),*Value=AP30BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","69"
        setprop sheet.range(RowNumber),*Value=AP60BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","69"
        setprop sheet.range(RowNumber),*Value=AP90BE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","69"
        setprop sheet.range(RowNumber),*Value=AP90PBE,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","70"
        setprop sheet.range(RowNumber),*Value="Management AP 30, 60, 90, 90+ "
        pack    RowNumber,"B","70"
        setprop sheet.range(RowNumber),*Value=AP30LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","70"
        setprop sheet.range(RowNumber),*Value=AP60LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","70"
        setprop sheet.range(RowNumber),*Value=AP90LM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","70"
        setprop sheet.range(RowNumber),*Value=AP90PLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.begin patch 3.95
.           call       debug
        pack    RowNumber,"A","86"
        setprop sheet.range(RowNumber),*Value="NEW BRK Rent AR 30, 60, 90, 90+ "
        pack    RowNumber,"B","86"
        setprop sheet.range(RowNumber),*Value=AR30NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","86"
        setprop sheet.range(RowNumber),*Value=AR60NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","86"
        setprop sheet.range(RowNumber),*Value=AR90NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","86"
        setprop sheet.range(RowNumber),*Value=AR90PNewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","87"
        setprop sheet.range(RowNumber),*Value="NEW BRK Rent AP 30, 60, 90, 90+ "
        pack    RowNumber,"B","87"
        setprop sheet.range(RowNumber),*Value=Ap30NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","87"
        setprop sheet.range(RowNumber),*Value=Ap60NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","87"
        setprop sheet.range(RowNumber),*Value=Ap90NewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","87"
        setprop sheet.range(RowNumber),*Value=Ap90PNewBr,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","88"
        setprop sheet.range(RowNumber),*Value="NEW BRK Exch AR 30, 60, 90, 90+ "
        pack    RowNumber,"B","88"
        setprop sheet.range(RowNumber),*Value=AR30NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","88"
        setprop sheet.range(RowNumber),*Value=AR60NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","88"
        setprop sheet.range(RowNumber),*Value=AR90NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","88"
        setprop sheet.range(RowNumber),*Value=AR90PNewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","89"
        setprop sheet.range(RowNumber),*Value="NEW BRK Exch AP 30, 60, 90, 90+ "
        pack    RowNumber,"B","89"
        setprop sheet.range(RowNumber),*Value=Ap30NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","89"
        setprop sheet.range(RowNumber),*Value=Ap60NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","89"
        setprop sheet.range(RowNumber),*Value=Ap90NewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","89"
        setprop sheet.range(RowNumber),*Value=Ap90PNewBe,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","90"
        setprop sheet.range(RowNumber),*Value="NEW LM  AR 30, 60, 90, 90+ "
        pack    RowNumber,"B","90"
        setprop sheet.range(RowNumber),*Value=Ar30NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","90"
        setprop sheet.range(RowNumber),*Value=Ar60NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","90"
        setprop sheet.range(RowNumber),*Value=Ar90NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","90"
        setprop sheet.range(RowNumber),*Value=Ar90PNewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

        pack    RowNumber,"A","91"
        setprop sheet.range(RowNumber),*Value="NEW LM  AP 30, 60, 90, 90+ "
        pack    RowNumber,"B","91"
        setprop sheet.range(RowNumber),*Value=Ap30NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"C","91"
        setprop sheet.range(RowNumber),*Value=Ap60NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"D","91"
        setprop sheet.range(RowNumber),*Value=Ap90NewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","91"
        setprop sheet.range(RowNumber),*Value=Ap90PNewLM,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 3.95


.end patch 3.9
.        trap    TrapObject if Object
        book.save giving N9
        trapclr Object
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
.end patch 3.6

EXIT3    BEEP
         BRANCH    PRTFLAG OF STOP,EXIT4
EXIT4    SPLCLOSE
STOP     SHUTDOWN  "CLS"
.         STOP
ABORT    TRAPCLR   F5
         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT3
* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - RANGE ERROR"
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - FORMAT ERROR"
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - PARITY ERROR"
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      PARITY
.begin patch 3.95
projread
. Projection Section Update Yearly!! checking for current year projections
           Move       No,Newbiz
          packkey    nPrjfld with TYPE,SRC,cid,"201601"
                 call    nprjkey
                    If        (PrjAR > 0)                    
                    MOve      No,Newbiz
                 return
                 else  
.                    call      Debug
.needs previous year if $ its not new
                 pack    nrevfld,typer,srcr,cidr,"2015" ......<<<<<<<<Yearly??????????????????????????
                 call    nrevkey
                 call    swaptype if over

                              Calc      OldTot1=(JanLR+FebLR+MarLR+AprLR+MayLR+JunLR+JulLR+AugLR+SepLR+OctLR+NovLR+DecLR)
                              Calc      OldTot=(JanAR+FebAR+MarAR+AprAR+MayAR+JunAR+JulAR+AugAR+SepAR+OctAR+NovAR+DecAR)

                    if (OLDTOT = C0 & OldTot1 = c0) 
                    move        yes to newbiz
                    move   c0,oldtot
                    return
                    endif
          endif
          move   c0,oldtot
          return                
swaptype
                 IF      (srcr = "M")
                 return
                 endif
.CHANGE ANNUALLY                 
                 match   "R" to typer
                 if      equal
.needs previous year if $ its not new
                 pack    nrevfld from "EB",srcr,cidr,"2015"            ....YEARLY
                 else
                 pack    nrevfld from "RB",srcr,cidr,"2015"
                 endif
                 call    nrevkey
                 return
.end patch 3.95          
         INCLUDE   NDATIO.inc
.patch3.46
                                             include        compio.inc
                                             include        cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.46
         INCLUDE   NSHPIO.inc
         INCLUDE   NORDIO.inc
         include   nmrgio.inc
         include   nmobio.inc
         include   nmoaio.inc
.START PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOWNIO.inc
.START PATCH 3.7 REMOVED LOGIC
..START PATCH 3.43 ADDED LOGIC
.              INCLUDE         NFULIO.INC
..END PATCH 3.43 ADDED LOGIC
.END PATCH 3.7 REMOVED LOGIC
.START PATCH 3.42 ADDED LOGIC
         include   nmldio.inc
.END PATCH 3.42 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
              INCLUDE         NSELIO.INC
              INCLUDE         NSEL2IO.INC
              INCLUDE         NTXTIO.INC
.END PATCH 3.45 ADDED LOGIC
.begin patch 3.95
           include nprjio.inc
           include nrevio.inc
.end patch 3.95
         INCLUDE   COMLOGIC.inc

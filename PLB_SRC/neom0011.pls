. PROGRAM    : NEOM0011
. DATE       : 04/05/88
. AUTHOR     : E.W. LAKE
. DESCRIPTION: PRODUCES NIN STATEMENTS & STATEMENT EDITS AGED BY MAIL DATE.
.
............................................................................
.
PC             EQU            0
               INCLUDE        COMMON.inc
+
         INCLUDE   CONS.inc
+
         INCLUDE   GTBLDD.inc    TABLE
+
         INCLUDE   NSTEDD.inc    STATEMENT FILE
+
.patch3.72
          include   compdd.inc
          include   cntdd.inc
.         INCLUDE   NMLRDD.inc    MAILER
.patch3.72
+
         INCLUDE   NBILDD.inc    BILL-TO
+
         INCLUDE   NPGEDD.inc
.patch3.72
.         include   nbrkdd.inc
.patch3.72
.begin patch 3.8
.         include   ninvdd.inc
          include     ninvdd.inc
.end patch 3.8
         include   norddd.inc
         include   hp.inc
         include   nmobdd.inc
         include   nmoadd.inc
.begin patch 3.4
         INCLUDE   NJSTDD.inc
.end patch 3.4
         include   nowndd.inc
         INCLUDE   OSLSPERN.inc
.Begin patch 3.6
               Include        DtpDD.inc
.End patch 3.6
release init    "3.93"        DLH Pull current moa balance at eoj2 - to fix R6 (and others) end of job totals
Reldate        Init           "2014 March 11"
.release init    "3.92"        DLH use PLB pdf with func=4
.Reldate        Init           "2014 January 30"
.release init    "3.91"        More totals on bad debt report
.Reldate        Init           "2013 December 04"
.release init    "3.90"        Only suppress using Obildirct on statements show consultant on internal reports
.Reldate        Init           "01 February 2013"
.release init    "3.89"                  DLH add payables totals to Bad Debt (report 5)
.Reldate        Init           "19 January 2011"
.release init    "3.88"                  DLH Correct JD code from patch 3.0
.Reldate        Init           "1 September 2010"
.release init    "3.87"                  DLH Change to copyfile
.Reldate        Init           "10 March 2010"
.release init    "3.86"                  DLH No more PL logo
.Reldate        Init           "11 February 2010"
.release init    "3.85"                  DLH Finished adding zero ar report
.Reldate        Init           "3 December 2009"
.release init    "3.84"                  06 Dec 2007  DLH    Company breakout
.Reldate        Init           "06 December 2007"
.release init    "3.83"                 20 May 2007  DLH    PLI
.Reldate        Init           "23 May 2007"
.release init    "3.82"                 08Mar2007  DLH      Oslspern.inc expansion
.Reldate        Init           "08 March 2007"
.release            init                "3.81"       JD     Totals % prc60 fixed.
.release            init                "3.8"        DLH    Invoice Conversion
.release  init      "3.77"        ASH   10DEC2004 Page File Conversion - DaysToPay File Conversion
.Reldate        Init           "09/060/2005"
.Reldate        Init           "03/02/2005"
.release  init      "3.76.1"       ASH  06JAN2005 Update logic around Inactive Contacts
.release  init      "3.76"        DMB   10SEP2004 Logo Conversion Update
.release  init      "3.75"        JD    10SEP2004 Logo Conversion
.release  init      "3.72"        DMB   26MAY2004 Mailer Conversion
.RELEASE  init      "3.71      JD  04May04 changed font."
.Reldate        Init           "09/13/2004"
.Release        Init          "3.7"            15Mar2004 Change to PrtPage for statements (funcbr = 4), print "$" column 1 if shortpays
.Reldate        Init           "03/15/2004"
.Release        Init          "3.61"           12Mar2004 proposed font change/flag shortpays.
   
.Release        Init          "3.6"           28January2004 DLH add days to pay   
.Reldate        Init           "01/28/2004"
.release  init       "3.53"      02OCT2000 ASH NEW SERVER ADDED
.release  init       "3.52"      16Jun00 JD PRINT GORE 2000 GELAC BILTO
.release  init       "3.51"      19Apr00 JD updated header from 1900 to 2000.
.release  init       "3.5"      18Feb00 DLH on the fly form  and the new address
.release  init       "3.4"      27APR99 DLH NINadj nadjust Y2k
.release  init       "3.3"      27APR99 DLH NININV.dat Y2k
.release  init       "3.2"      26Feb99 DLH remove mailer name from total line
.RELEASE  INIT      "3.1"        20JAN99 ASH NINORD Y2K, File expansion
.RELEASE  INIT      "3.0"        30NOV98jd comparing zero's to stebrk.
.RELEASE  INIT      "2.9"        26jun96 jd if pre 95 order use old address.
.RELEASE  INIT      "2.8"       31MAR95 DLH DO NOT BREAK ON MLR CONTACT
.release  init      "2.7"       23mar95 DLH more detail on adv pay rep
.release  init      "2.6"       10mar95 dlh flag adjusted invoices
.                              -printed in bold.
.release  init      "2.5"       01jan95 added bil direct.
.release  init      "2.4"      pending dlh new report adv pay to LO  by LO
.
.RELEASE  INIT      "2.3"       28JUN94 DLH NEW REPORT TYPE - OPEN PREPAID
.                              (money in house)  ORDERS.
.
.release  init      "2.2"       26jun94 DLH new report type open with LO payment.
.
.RELEASE  INIT      "2.1"      18APR94 DLH USE LASER FORM ON STATEMENT CARDS.
.
.RELEASE  INIT      "2.0"      01OCT93   ADD BROKER/CONSULTANT BREAK & TOTALS.
.RELEASE  INIT      "1.9.1"      JD 30DEC92  
.                             FIXED PRINT OF THRU NAME 60-90 REPORT, FIXED
.                             GUARANTEE MESSAGE ON STATEMENT FORM, AS WELL AS
.                             CORRECT MLR#/CNT# MAILER BREAK CONTINUATION.
.
.RELEASE  INIT      "1.9"      JD 21OCT92  
.                             PRINTING MAIL DATE INSTEAD OF INV DATE ON
.                             STATEMENTS
.RELEASE         INIT      "1.8"      DLH 8JUN92  
.                             NEW REPORT TYPE - EDIT BAD DEBT. INV 365 DAYS
.                             OLD.
.RELEASE    INIT     "1.7"     D.L. HERRICK 13MAY92 
.                             FLAG GUARANTEED ORDERS ON STATEMENT CARDS &
.                             STATEMENT EDIT.
.RELEASE   INIT     "1.6"     D.L. HERRICK 12MAY92
.                             ADD SALESPERSON ON STATEMENT EDIT.
.RELEASE  INIT      "1.5"     D.L. HERRICK 21APR92.
.                            ADD NO-TOTAL OPTION, PRINT NINSALES PERSON ON 60-
.                            90.
.RELEASE  INIT      "1.4"    D.L. HERRICK 09AUG91
.                           CONVERT FOR NINCAL USE.
.RELEASE  INIT      "1.3"    E.W. LAKE   07/09/91
.                           EXCLUDE NEGATIVE AMOUNTS FROM STATEMENT CARDS.
.
.RELEASE INIT      "1.2"    E.W. LAKE   08/10/88
.                           CHOP LIST NAME @ 17 BYTES.
.
.RELEASE INIT      "1.1"    E.W. LAKE   07/14/88
.                           PCBUS CONVERSION
.
.RELEASE INIT      "1.0"    E.W. LAKE   04/05/88
.                           INITIAL RELEASE
.
.Begin patch 3.92
FileCheck           FIle
trapcount           form      4
.end patch 3.92
.Begin Patch 3.73
OLDBRKNEWCOMP external "COMP001A;OldBrktoNewComp"
OLDBRKNEWCONTACT external "COMP001A;OldBrktoNewContact"
HOLD      DIM       500
HOLD2     DIM       304
.End Patch 3.73
.START PATCH 3.77 ADDED **TEMPORARY** LOGIC
.This logic is temporary until NINSTE has had it's Mailer and Broker fields converted.
GetNewMlrBrk external "NEOM011A;GetNewMlrBrk"
GetNewMlr external "NEOM011A;GetNewMlr"
.END PATCH 3.77 ADDED **TEMPORARY** LOGIC
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
.START PATCH 3.77 ADDED LOGIC
HoldMlrKey          dim       6
HoldBrkKey          dim       25
.END PATCH 3.77 ADDED LOGIC
PRTLINES FORM      2
DATE     DIM       8
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
R1       INIT      "Over 60 Day Statement Edit"
R2       INIT      "   Full Statement Edit    "
R3       INIT      "Full Statement Edit Invdte"
R4       INIT      "Statements                "
R5       INIT      " Bad Debt Statement Edit  "
R6       INIT      "Open Advance Pay to LO"
R7       INIT      "Open Prepaid Orders"
R8       INIT      "Open/Advance Pay to/by LO"
R9       INIT      "Open Zero Receivables"
RPTTYPE  DIM       26
MON1     INIT      " JANUARY 20?? "
MON2     INIT      "FEBRUARY 20?? "
MON3     INIT      "  MARCH 20??  "
MON4     INIT      "  APRIL 20??  "
MON5     INIT      "   MAY 20??   "
MON6     INIT      "  JUNE 20??   "
MON7     INIT      "  JULY 20??   "
MON8     INIT      " AUGUST 20??  "
MON9     INIT      "SEPTEMBER 20??"
MON10    INIT      " OCTOBER 20?? "
MON11    INIT      "NOVEMBER 20?? "
MON12    INIT      "DECEMBER 20?? "
MONTH    DIM       14
guar1    init      "30 day"
guar2    init      "45 day"
guar3    init      "60 day"
guar4    init      "open guar"
guar5    init      "unknown"
guar6    init      "prepaid"
guar7    init      "prepaid 30day"
guar8    init      "prepaid 45day"
guar9    init      "prepaid 60day"
guardes  dim       14 
guapflag form      1              2=paid for guar, 1=other
SYSDAYS  FORM      5              AGEING DATE CONVERTED TO JULIAN
HOLDBRL  INIT      "    "         CHECK FOR BRK/CONS BREAK
HOLDMLR  INIT      "    "         CHECK FOR MAILER BREAK
.HOLDCNT  INIT      "   "          CHECK FOR CONTACT BREAK   dlh 31mar95
holdown  dim        4              CHECK FOR Owner BREAK   dlh 31mar95
fARBRK    FORM      10.2
fApBRK    FORM      10.2
fAR       FORM      10.2
fAP       FORM      10.2           LIST OWNER A/P
fAP1      FORM      10.2           NON-LIST OWNER A/P
advap     form      10.2           advanced lo a/p
DY00TO30 FORM      10.2            DETAIL
DY31TO60 FORM      10.2
DY61TO90 FORM      10.2
DYOVER90 FORM      10.2
BALDUE   FORM      10.2
CL00TO30 FORM      10.2            CLIENT TOTALS
CL31TO60 FORM      10.2
CL61TO90 FORM      10.2
CLOVER90 FORM      10.2
BR00TO30 FORM      10.2            BROKER/CONSULTANT TOTALS
BR31TO60 FORM      10.2
BR61TO90 FORM      10.2
BROVER90 FORM      10.2
GR00TO30 FORM      10.2            GRAND TOTALS
GR31TO60 FORM      10.2
GR61TO90 FORM      10.2
GROVER90 FORM      10.2
GR30to90 FORM      10.2
.
DYapTO30 FORM      10.2            DETAIL
DYapTO60 FORM      10.2
DYapTO90 FORM      10.2
DYapOR90 FORM      10.2
CLAPTO30 FORM      10.2            CLIENT TOTALS
CLAPTO60 FORM      10.2
CLAPTO90 FORM      10.2
CLApOR90 FORM      10.2
BRAPTO30 FORM      10.2            BROKER/CONSULTANT TOTALS
BRAPTO60 FORM      10.2
BRAPTO90 FORM      10.2
BRAPOR90 FORM      10.2
GRAPTO30 FORM      10.2            GRAND TOTALS
GRAPTO60 FORM      10.2
GRAPTO90 FORM      10.2
GRAPOR90 FORM      10.2
GRAP3t90 FORM      10.2
guarap   form      10.2           paid on guarantees
guarothr form      10.2           paid ?????

.begin patch 3.84
NNAPTO30 FORM      10.2            NIN GRAND TOTALS
NNAPTO60 FORM      10.2
NNAPTO90 FORM      10.2
NNAPOR90 FORM      10.2
NNAP3t90 FORM      10.2
NNguarap   form      10.2           paid on guarantees
NNguarothr form      10.2           paid ?????
NNAR     FORM      10.2
NNAP     FORM      10.2
NNAP1    FORM      10.2           NON-LIST OWNER A/P
NNAP2    FORM      10.2           advance pay to lo.
NNRAP3    FORM      10.2           intra company ap

PLAPTO30 FORM      10.2            PL GRAND TOTALS
PLAPTO60 FORM      10.2
PLAPTO90 FORM      10.2
PLAPOR90 FORM      10.2
PLAP3t90 FORM      10.2
PLguarap   form      10.2           paid on guarantees
PLguarothr form      10.2           paid ?????
PLAR     FORM      10.2
PLAP     FORM      10.2
PLAP1    FORM      10.2           NON-LIST OWNER A/P
PLAP2    FORM      10.2           advance pay to lo.
PLAP3    FORM      10.2           intra company ap
.end patch 3.84
.
GRAR     FORM      10.2
GRAP     FORM      10.2
GRAP1    FORM      10.2           NON-LIST OWNER A/P
GRAP2    FORM      10.2           advance pay to lo.
PRC30    FORM      10.2
PRC60    FORM      10.2
PRC90    FORM      10.2
PRCOV    FORM      10.2
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
DATEMASK INIT      "99/99/99"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
DIFFDAYS FORM      5
C35      FORM      "35"
C36      FORM      "36"
C37      FORM      "37"
C38      FORM      "38"
c58      form      "58"
c57      form      "57"
NUMMASK  INIT      "Z,ZZZ,ZZZ,ZZZ.99-"
COL1     DIM       17
COL2     DIM       17
COL3     DIM       17
COL4     DIM       17
COL5     DIM       17
COL6     DIM       17
edittot  DIM       17
LNAME17  DIM       17
OVER60   DIM       1             Y=YES, USED FOR PAGES & MLROVER90 WRITES
SLS      DIM       8             USED TO PRINT SALESPERSON ON 60-90.
TOTFLAG  FORM      1         TOTL FLAG, 1=NO TOTALS, 2=TOTALS
GUAFLAG  FORM      1         NIN GUARANTY FLAG 1=GUARANTIED
GUAR     DIM       1         NIN GUAR FLAG YES= MAILER BREAK RECORD GUARANTEED
GUAFLAGo FORM      1        OUTSIDE GUARANTY FLAG 1=GUARANTIED
GUARo    DIM       1        OUTSIDE  GUAR FLAG YES= MLR BREAK REC. GUARANTEED
OUTGUAR  DIM        1        HOLD '@' IF DETAIL HAS OUTSIDE GUAR
FIRST    INIT      "Y"
brkbrk   dim        4
holdbrk  init       "0000"
moamlr   dim        4
firstbk  init      "N"
brokerFlag   form       "0"
hbrkcnt  dim        3
chklr    dim        6
careof   dim        3
DOLLAR   INIT      "$$,$$$,$$$.99"
moaamt   dim        13
moa$     form       8.2
CHANGE   FORM      10.2         CHANGE TO BE APPLIED TO BALANCE.
.str20    dim       20
AP2SW    DIM       1
APMASK   INIT      "$,$$$,$$$,$$9.99-"
AP1OUT   DIM        17
form92   form      9.2
form72   form      7.2
adjap    form      10.2
AP2OUT   DIM       15
JSTN     FORM      2
HUND     FORM      "100"
n2sls    dim       2
.begin Patch 3.6
DTPDesc        Dim            25
DTPInfoFlag    Dim            1
.end Patch 3.6
steCompID Dim       1
AgeCheck  Dim       8
.
. DEFAULT OPTIONS ARE : TOTALS
.
.
. COMMENT CAN MODIFY THE DEFAULTS:  NOTOT  :  NO TOTALS
............................................................................
MLRCRED  FILE      FIXED=7
brkcRED  IFILE      FIXED=7,keylen=4
............................................................................
.begin patch 3.7
VertPos        Form           5
GreyFIll       Color
colornum       form 24
font1     font
Font4     font
font5     font
font08         font
Font09I   font
Font09BI  font
Font010   font
Font010n  font
Font010B  font
Font012B  font
Font012BI font
Font014   Font
Font014B  font
Font014BI font
Font018I  font
Font07    font
Font07dot5          font
Font07dot5B         font
Font07dot5I         font
Font07dot5BI        font
Font018B  font
Font018BI font
PRTPG24B  font
PRTPG24I  font
PRTPG10             font
sevenfive form      "7.5"
Laser     PFILE

          create    font1,"Times New Roman",size=14,bold
          create    font08,"Times New Roman",size=8
          create    font5,"Times New Roman",size=11
          create    Font09I,"Times New Roman",size=9,Italic
          create    Font09BI,"Times New Roman",size=9,Bold,Italic
          create    Font010,"Times New Roman",size=10
          create    Font010n,"Courier New",size=11
          create    Font010B,"Times New Roman",size=10,Bold
          create    Font012B,"Times New Roman",size=12,Bold
          create    Font012BI,"Times New Roman",size=12,Bold,Italic
          create    Font014,"Times New Roman",size=14
          create    Font014B,"Times New Roman",size=14,Bold
          create    Font014BI,"Times New Roman",size=14,Bold,Italic
          create    Font018I,"Times New Roman",size=18,Italic
          create    Font07,"Times New Roman",size=7
          create    Font07dot5,"Times New Roman",size=sevenfive
          create    Font07dot5I,"Times New Roman",size=sevenfive,Italic
          create    Font07dot5b,"Times New Roman",size=sevenfive,Bold
          create    Font07dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
          create    Font018B,"Times New Roman",size=18,Bold
          create    Font018BI,"Times New Roman",size=18,Bold,Italic
.
          create    PRTpg24B,"Times New Roman",size=24,Bold
          create    PRTpg24I,"Times New Roman",size=24,Italic
          create    PRTpg10,"Times New Roman",size=10
               Create         GreyFill=224:224:224

.end patch 3.7
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
+...........................................................................
.
.
.        splopen   "c:\work\test3.prn"
.        call      PrtARForm
.        splclose
.        stop
.begin patch 3.7
               Call           GetwinVer
.end patch 3.7
         MOVE      FUNC TO FUNCBR       
         move      c2 to nmobpath
         MOVE      C1 TO NordPATH
         MOVE      C1 TO NINVPATH
         SCAN      "NOTOT" IN COMMENT
         IF        EQUAL 
         MOVE      C1 TO TOTFLAG
         ELSE
         MOVE      C2 TO TOTFLAG
         ENDIF
         MATCH     "        " TO TODAY
         IF        EQUAL 
         GOTO      GETDATE
         ELSE
         IF        EOS
         GOTO      GETDATE
         ENDIF
         ENDIF
         GOTO      BEGIN
.
GETDATE  
          match     "NEOM0011",Program
          if        equal
          move      Today,Date
          Else
          CLOCK     DATE TO DATE
          endif
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
BEGIN    
.******temp
.         MOve      "NININV.ISI",Ninvname
.         OPen      NinvFile,"c:\work\Nininv"
.         move      c1,Ninvflag
.Begin patch 3.7
.temp temp temp
.               MOve           c4 to func
.               MOve           c4 to funcBR
.               Move           "02-29-04" to date
.               Move           "02-29-04" to today
.               Move           "NINSTE.dat" to inpname
.               Move           "TestSTE.lst" to prtname
.               MOVE      "Statements" TO STITLE
.               MOve           "NEOM0011" to program
.temp temp temp
.end patch 3.7

         CMATCH    B1 TO PROGRAM
         IF        EOS
         MOVE      "NEOM0011 " TO PROGRAM
.         MOVE      "NINSTE" TO INPNAME
        MOVE       "LOCAL" TO PRTNAME
         ENDIF
         
         MOVE      "Statements" TO STITLE
         CALL      PAINT
.
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
.
         DISPLAY   *P01:06,"Ageing Date : ":
                   *P01:07,"Report Type : ":
                   *P01:08,"Input File  : ":
                   *P01:10,"Print File  : ":
                   *P01:11,"Input Count : ":
                   *P01:12,"Invoice Date: ":
                   *P01:13,"Index File  : ":
                   *P01:14,"Options     : ",COMMENT
.
         UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         pack       Agecheck from "20",yy,MM,DD
.
DATEGET  DISPLAY   *P15:06,MM,SLASH,DD,SLASH,YY
         KEYIN     *P1:24,*EL,"DATE OK ? ",*T05,STR1;
         CMATCH    NO TO STR1
         GOTO      DATEBAD IF EQUAL
         CALL      DATETEST
         BRANCH    DATEFLAG TO DATENG
.         MOVE      YY TO NYY
.         CALL      JULIAN
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSDAYS
.         MOVE      STR5 TO SYSDAYS
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM MON1,MON2,MON3,MON4,MON5,MON6:
                   MON7,MON8,MON9,MON10,MON11,MON12
         SCAN      "??" IN MONTH
         GOTO      PRTGET IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
         GOTO      TYPEGET
DATEBAD  KEYIN     *P01:24,*EL,"The ageing date is invalid.":
                   *P15:06,*DV,*HON,MM,*DV,SLASH,*DV,DD,*DV,SLASH,*DV,YY:
                   *P15:06,*RV,*+,*JR,MM:
                   *P18:06,*RV,*+,*JR,DD:
                   *P21:06,*RV,*-,*JR,YY,*HOFF;
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         GOTO      DATEGET
.
TYPEGET  COMPARE   C0 TO FUNCBR
         GOTO      TYPENG IF LESS
         GOTO      TYPENG IF EQUAL
         COMPARE   FUNCBR TO C9
         GOTO      TYPENG IF LESS
         LOAD      RPTTYPE USING FUNCBR FROM R1,R2,R3,R4,R5,R6,r7,r8,R9
         DISPLAY   *P15:07,FUNCBR,SLASH,RPTTYPE
         COMPARE   C4 TO FUNCBR
         IF        NOT EQUAL
           COMPARE   FUNCBR TO C1
           IF        EQUAL
           MOVE      C6 TO INDNUM
           IFNZ      PC
           PREPARE   MLRCRED,"MLROVER90/TEXT:PRINT"
           XIF
           IFZ       PC
.START PATCH 3.53 REPLACED LOGIC
.           PREPARE   MLRCRED,"g:\DATA\MLROVR90"
.           PREPARE   brkCRED,"g:\DATA\brkovr90","g:\DATA\brkovr90","4","7"
           PACK      STR35,NTWKPATH1,"MLROVR90"
           PACK      STR45,NTWKPATH1,"brkovr90"
           PACK      STR50,NTWKPATH1,"brkovr90"
           PREPARE   MLRCRED,STR35
           PREPARE   brkCRED,STR45,STR50,"4","7"
.END PATCH 3.53 REPLACED LOGIC
           XIF
           ENDIF

.
           COMPARE   FUNCBR TO C2
           IF        EQUAL
           MOVE      C1 TO INDNUM
           ENDIF
.
           COMPARE   FUNCBR TO C3
           IF        EQUAL
           MOVE      C7 TO INDNUM
           ENDIF
.
           COMPARE   FUNCBR TO C5
           IF        EQUAL
           MOVE      C8 TO INDNUM
           ENDIF
.
           COMPARE   FUNCBR TO C6
           IF        EQUAL
           MOVE      C9 TO INDNUM
           ENDIF
.
           COMPARE   FUNCBR TO C7
           IF        EQUAL
           MOVE      C10 TO INDNUM
           ENDIF
.
           COMPARE   FUNCBR TO C8
           IF        EQUAL
           MOVE      C11 TO INDNUM
           ENDIF
.
         CLEAR     NPGENAME
         APPEND    "PAGE" TO NPGENAME
         APPEND    INDNUM TO NPGENAME
         RESET     NPGENAME
         REP       ZFILL IN NPGENAME
         DISPLAY   *P15:13,NPGENAME
         ENDIF
         GOTO      INPGET
TYPENG   KEYIN     *P01:24,*EL,"The Report Type is invalid.":
                   *P15:07,FUNCBR;
         GOTO      TYPEGET
.
INPGET   TRAP      INPNG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:08,INPNAME
         MOVE      INPNAME TO NSTENAME
.         BRANCH    FUNCBR OF PAGE,PAGE,PAGE,PRTGET,PAGE,page,page
         BRANCH    FUNCBR OF PAGE,PAGE,PAGE,PRTGET,PAGE,page,page,page,prtget
PAGE
         clear     str45
         pack      str45 from ntwkpath1,npgename
.START PATCH 3.77 REPLACED LOGIC
.         PREPARE   NPGEFILE,str45
.         MOVE      C1 TO NPGEFLAG
          PREPARE   NPGEFLE2,str45,"\\nins1\E\DATA\INDEX\page","3-8","66"
          MOVE      C1 TO NPGEFLG2
.END PATCH 3.77 REPLACED LOGIC
.Begin patch 3.6
.         GOTO      PRTGET
         GOTO      SetDtp
.End patch 3.6
INPNG    NORETURN
         KEYIN     *P01:24,*EL,"The Input File is not on-line. ":
                   *P15:08,INPNAME;
         GOTO      INPGET
.Begin patch 3.6
SetDtp         Call           DTPOpen        ;if file present sets flag to one else flag null and file ignored
.end patch 3.6
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         DISPLAY   *P15:10,PRTNAME
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      OMIT IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE FROM pdrive,PRTNAME
         trap      prtng if spool
.Begin patch 3.7
               If             (FuncBr <> 4)
               SPLOPEN        PRTFILE
               ElseIf         (FuncBr = 4)
.   PRTOPEN   Laser,"\\NTS0\Laser8",str8,noprint,spoolfile=prtname
.         PRTOPEN   Laser,"\\nts0\laser8","Statements",SPoolfile="\\nins1\e\data\testSTE.lst"
.begin patch 3.92
.          PRTOPEN   Laser,"Faxfile","FaxFile.prn"
                    pack    str55,"c:\work\pdf\",Prtname,".pdf"
                    PRTOPEN Laser,"PDF:",str55
.end patch 3.92
.         PRTOPEN Laser,"Faxfile",""
.         PRTOPEN   Laser,"","Statements"
               getitem        GreyFill,0,colornum
               endif
.end patch 3.7
         DISPLAY   *P15:10,PRTNAME
         trapclr   spool
         GOTO      OMIT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:10,PRTNAME;
         trapclr   spool
         GOTO      PRTGET
.
. THE 'NNOSTMNT' ENTRY IN THE TABLE FILE CONTAINS MAILER NUMBERS THAT DO NOT
. NEED STATEMENT CARDS TO BE PRINTED.
.
OMIT     DISPLAY   *P1:24,*EL;
               if             (FuncBr <> 4)
               PRINT          hpreset,hpport:
                              033,"&l66P":               page length
                              033,"&l65F":               number lines
                              033,"&l0E",033,"&a0c0R";     top margin * print position
               endif
         BRANCH    FUNCBR TO START,START,START,OMIT1,START,start,start,start,start
OMIT1
.         PRINT     HPuniv10
         MOVE      "NNOSTMNT" TO GTBLFLD
         CALL      GTBLKEY
.
START    CALL      NSTESEQ
.START PATCH 3.77 REPLACED LOGIC
.         GOTO      EOJ IF OVER
          if over
                    if (FUNCBR <> 4)
                              call      PageUpdate
                    endif
                    GOTO EOJ
          endif
          if        (stear = c0)
          call      debug
          endif
.begin patch 3.86
.          Unpack    STECODE into str1,SteCompid                    ..get company ID
          move      B1,steCompId
.end patch 3.86
.END PATCH 3.77 REPLACED LOGIC
         ADD       C1 TO N6
         DISPLAY   *P15:11,N6
.         move      stelr to chklr
         compare   c6 to funcbr                .adv pay selected is this one?
         goto      guachck if equal
         compare   c7 to funcbr                .prepay selected is this one?
         goto      guachck if equal
         compare   c8 to funcbr                .adv pay selected is this one?
         goto      guachck if equal
         goto      guaexit
.         if        equal                        .adv pay selected is this one?
.         move      stelr to ninvfld
.         call      ninvkey
.         match     b2 to chkdtem
.         goto      start if equal              .no
.         move      stelr to nordfld
.         rep        zfill in nordfld
.         call      nordkey
.         move      b1 to guardes
.         move      c0 to n1
.         move      guarcode to n1 
.         compare   c0 to n1
.         if        not equal
.         load     guardes from n1 of guar1,guar2,guar3,guar4,guar5,guar6:
.                  guar7,guar8,guar9
.         move     c2 to guapflag
.         else
.         move     c1 to guapflag
.         endif
.         endif
.         compare   c7 to funcbr
.         if        equal                        .prepaid ?
guachck 
         compare   c6 to funcbr
         if        equal                        .adv pay selected is this one?
         move      stelr to ninvfld
         call      ninvkey
.begin patch 3.3
.         match     b2 to chkdtem
         match     b2 to chk1dtem
.end patch 3.3
         goto      start if equal              .no
         endif
         compare   c8 to funcbr
         if        equal                        .adv pay selected is this one?
         move      stelr to ninvfld
         call      ninvkey
.begin patch 3.3
.         match     b2 to chkdtem
         match     b2 to chk1dtem
.end patch 3.3
         goto      start if equal              .no
         PACK      MKEY FROM STEMLR,STECNT         
.Begin Patch 3.73
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.START PATCH 3.76.1 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 3.76.1 ADDED LOGIC
.End Patch 3.73
         endif
         move      stelr to nordfld
         rep        zfill in nordfld
         call      nordkey
         move      b1 to guardes
         move      c0 to n1
         move      guarcode to n1 
         compare   c0 to n1
         if        not equal
         load     guardes from n1 of guar1,guar2,guar3,guar4,guar5,guar6:
                  guar7,guar8,guar9
         move     c2 to guapflag
         else
         move     c1 to guapflag
         endif
guaexit  compare   c7 to funcbr
         goto      start2 if not equal
         branch    n1 of start,start,start,start,start,strt1,strt1,strt1,strt1
         goto      start
.        endif
strt1    MOVE      NO TO GUAR
         MOVE      NO TO GUARo                .outside guar 21apr93
         MATCH     STAR TO STEGRNTE
         IF        EQUAL 
         MATCH     STEMLR TO HOLDMLR
           IF      EQUAL
.         MATCH     STECNT TO HOLDCNT            .dlh 31mar95
.                IF        EQUAL
                 MOVE      C1 TO GUAFLAG
.                ENDIF
         ENDIF
         MOVE      YES TO GUAR
         ENDIF
         MOVE      C0 TO N1                .OUTSIDE GUAR 21APR93
         MOVE      STEGUAR TO N1          .OUTSIDE GUAR 21APR93
         CLEAR     OUTGUAR
.        COMPARE   C0 TO N1                .OUTSIDE GUAR 21APR93
.        IF        NOT EQUAL               .        "
         branch    n1 to start1,start1,start1,start1
         goto      start2
.        CLEAR     OUTGUAR
.        GOTO      START1                  .        "
.        ELSE                              .        "
start1   MATCH     STEMLR TO HOLDMLR       .        "
         IF       EQUAL              .        "
.         MATCH     STECNT TO HOLDCNT
.           IF      EQUAL
        MOVE     C1 TO GUAFLAGo     .        "
.       ENDIF 
       ENDIF                       .        "
         MOVE      YES TO GUARo            .        "
.        ENDIF                             .        "
.               
.        move      c1 to guaflAgo
         
start2   
.
         MOVE      C0 TO DY00TO30
         MOVE      C0 TO DY31TO60
         MOVE      C0 TO DY61TO90
         MOVE      C0 TO DYapTO30
         MOVE      C0 TO DYapTO60
         MOVE      C0 TO DYapTO90
         MOVE      C0 TO fAR
         MOVE      C0 TO fAP
         MOVE      C0 TO fAP1
         MOVE      C0 TO DYOVER90
         MOVE      C0 TO DYapOR90
         MOVE      B12 TO COL1
         MOVE      B12 TO COL2
         MOVE      B12 TO COL3
         MOVE      B12 TO COL4
.         move      stemlr to holdmlr
.
. PUT DECIMAL POINT INTO AR & AP.
.
.         MOVE      STEAR TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO fAR
.         MULTIPLY  ".01" BY fAR
.
.begin patch 3.3
          move      stear to far
.end patch 3.3.
. DO NOT INCLUDE ZERO AMOUNTS ON STATEMENT CARDS.
.
.begin patch 3.85
.         BRANCH    FUNCBR TO CVTAP,CVTAP,CVTAP,NOCVTAP,CVTAP,cvtap,cvtap,cvtap
         BRANCH    FUNCBR TO CVTAP,CVTAP,CVTAP,NOCVTAP,CVTAP,cvtap,cvtap,cvtap,NoCVTAR
NOCVTAP  COMPARE   C0 TO fAR
         GOTO      START IF EQUAL
.dh goes bad
.       GOTO      START IF LESS
..dh goes bad   July 16 2010
 
          goto      cvtap
NOCVTAR  COMPARE   C0 TO fAR
         GOTO      START IF Not EQUAL
.end patch 3.85
.
CVTAP
.begin patch 3.3
.          MOVE      STEAP1 TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO fAP
.         MULTIPLY  ".01" BY fAP
         move      steap1 to fap
.         MATCH     "000000000" TO STEAP2
         compare   c0 to steap2
         if        equal
         move      no to ap2sw
         goto      age
         else
         move      yes to ap2sw
         endif
.         GOTO      AGE IF EQUAL
.         MOVE      STEAP2 TO CVTFLD
         MOVE      fAP TO fAP1             *SAVE NON LIST OWNER PORTION.
.         CALL      CVT
.         MOVE      CVTFLD TO fAP
.         MULTIPLY  ".01" BY fAP
         move       steap2 to fap
.end patch 3.3
.
. AGE THE INVOICE ACCORDING TO MAIL DATE FOR REPORTS 1,2, & 4
.BY INVOICE DATE FOR REPORT 3.
.SUPPRESS ALL BUT 1YEAR & + ON 5, AND AGED BY INVDATE.
.prepay age on invdate
AGE      MOVE      NO TO OVER60
         BRANCH    FUNCBR OF AGE1,AGE1,AGE2,AGE1,AGE2,age2,age1,age2,age1
AGE1
.Start patch #3.1 - increased var
.         MATCH     "000000" TO STEMLDDT
.         IF        EQUAL
.         UNPACK    STEINVDT TO MM,DD,YY
.         ELSE
.         UNPACK    STEMLDDT TO MM,DD,YY
.
         MATCH     "00000000" TO STEMLDDT
         IF        EQUAL
         UNPACK    STEINVDT TO STR2,YY,MM,DD
         ELSE
         UNPACK    STEMLDDT TO str2,YY,MM,DD
.End patch #3.1 - increased var
         ENDIF
.         GOTO      AGE2        DLH 7/2/92  ?????
         GOTO      AGE4
AGE2
.Start patch #3.1 - increased var
.         UNPACK    STEINVDT TO MM,DD,YY
         UNPACK    STEINVDT TO STR2,YY,MM,DD
.END patch #3.1 - increased var
         GOTO      AGE4
age3
.begin patch 3.3
.        move      chkdtem to mm      .adv pay to lo a/p aging
.         move      chkdted to dd
.         move      chkdtey to yy
         move      chk1dtem to mm      .adv pay to lo a/p aging
         move      chk1dted to dd
         move      chk1dtey to yy
.end patch 3.3
         CALL      CVTJUL
         MOVE      SYSDAYS TO DIFFDAYS
         SUBTRACT  JULDAYS FROM DIFFDAYS
         goto      chk90ap         
.
AGE4     CALL      CVTJUL
         MOVE      SYSDAYS TO DIFFDAYS
         SUBTRACT  JULDAYS FROM DIFFDAYS
         BRANCH    FUNCBR OF CHK90,CHK90,CHK90,CHK90,CHK365,chk90,chk90,chk90,chk90
CHK365   COMPARE   "365" TO DIFFDAYS
         GOTO       START IF LESS
         MOVE      fAR TO DYOVER90
         MOVE      YES TO OVER60
          if        (Funcbr = c5)
          move      FAp to DYapOR90
          endif
         GOTO      ageexit
CHK90    COMPARE   "90" TO DIFFDAYS
         GOTO      CHK60 IF LESS
         GOTO      CHK60 IF EQUAL
         MOVE      fAR TO DYOVER90
         MOVE      YES TO OVER60
          if        (Funcbr = c5)
          move      FAp to DYapOR90
          endif
         GOTO      ageexit
CHK60    COMPARE   "60" TO DIFFDAYS
         GOTO      CHK30 IF LESS
         GOTO      CHK30 IF EQUAL
         MOVE      fAR TO DY61TO90
         MOVE      YES TO OVER60
          if        (Funcbr = c5)
          move      FAp to DYapTO90
          endif
         GOTO      ageexit
CHK30    BRANCH    FUNCBR TO START           PASS UP FOR OVER 60.
         COMPARE   "30" TO DIFFDAYS
         GOTO      CHK00 IF LESS
         GOTO      CHK00 IF EQUAL
         MOVE      fAR TO DY31TO60
         GOTO      ageexit
CHK00    MOVE      fAR TO DY00TO30
          
.
ageexit  branch    funcbr of chkmlr,chkmlr,chkmlr,chkmlr,chkmlr,age3,chkmlr,age3,chkmlr
         goto      chkmlr
.
CHK90ap  
         MOVE      C1 TO JSTN
         call      detadj2
         branch    guapflag to guarothr,guarap
guarothr add       adjap to guarothr
         goto      chk90apa         
guarap   add       adjap to guarap

chk90apa cOMPARE   "90" TO DIFFDAYS
         GOTO      CHK60ap IF LESS
         GOTO      CHK60ap IF EQUAL
         MOVE      adjAp TO DYapOR90
         GOTO      chkmlr
CHK60ap  COMPARE   "60" TO DIFFDAYS
         GOTO      CHK30ap IF LESS
         GOTO      CHK30ap IF EQUAL
         MOVE      adjAp TO DYaptO90
         GOTO      chkmlr
CHK30ap  COMPARE   "30" TO DIFFDAYS
         GOTO      CHK00ap IF LESS
         GOTO      CHK00ap IF EQUAL
         MOVE      adjap TO DYapTO60
         GOTO      chkmlr
CHK00ap  MOVE      adjap TO DYapTO30
.
CHKMLR   BRANCH    FUNCBR TO CHKMLR2,CHKMLR2,CHKMLR2,CHKMLR1,CHKMLR2:
                   chkmlr2,chkmlr2,chkown,chkmlr2
CHKMLR1  RESET     TBLTEXT
         SCAN      STEMLR IN TBLTEXT        MAILER TO OMIT?
         GOTO      START IF EQUAL
CHKMLR2
.test dh sep 1 2010
          Rep       Zfill,stebrk
.         match     "    " to stebrk
.         goto      skip if equal
.start of patch 3.0
.         match     "0000" to stebrk
.         goto      skip if equal
.end of patch 3.0
.test dh sep 1 2010
         compare   c1 to brokerFlag
         goto      samebk if equal
         goto      fbrkr 
samebk   match     stebrk to holdbrk
         if        not equal
         move      yes to brkbrk
         goto      mlrtot
         endif
         goto      skip
.         
chkown   match     steown to holdown
         goto      owntot if not equal
         goto      detail2
.
skip     MATCH     STEMLR TO HOLDMLR
         GOTO      MLRTOT IF NOT EQUAL
.         BRANCH    FUNCBR TO CHKCNT,CHKCNT,CHKCNT,DETAIL2
.   NINCAL  19AUG91 DLH.
.CHKCNT   MATCH     STECNT TO HOLDCNT
.         GOTO      MLRTOT IF NOT EQUAL
.
DETAIL2  MOVE      DATEMASK TO DATEPRT1
.Start patch #3.1 - increased var
.         EDIT      STEMLDDT TO DATEPRT1
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    STEMLDDT TO cc,str2,str4
.         APPEND    STR4,STR6                    .MMDD
.         UNPACK    STEMLDDT TO cc,str2
.         APPEND    STR2,STR6                    .yy
         pack       str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
.End patch #3.1 - increased var
         MOVE      DATEMASK TO DATEPRT2
.Start patch #3.1 - increased var
.         EDIT      STEINVDT TO DATEPRT2
         CLEAR     STR6
         clear     str4
         clear     str2
         UNPACK    STEinvDT TO cc,str2,str4
.         APPEND    STR4,STR6                    .MMDD
.         UNPACK    STEinvDT TO cc,str2
.         APPEND    STR2,STR6                    .yy
         pack      str6 from str4,str2
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT2
.END patch #3.1 - increased var
         MOVE      STELNAME TO LNAME17
.
         ADD       DY00TO30 TO CL00TO30     CLIENT TOTALS
         ADD       DY31TO60 TO CL31TO60
         ADD       DY61TO90 TO CL61TO90
         ADD       DYOVER90 TO CLOVER90

         
         ADD       DY00TO30 TO BR00TO30     BROKER TOTALS
         ADD       DY31TO60 TO BR31TO60
         ADD       DY61TO90 TO BR61TO90
         ADD       DYOVER90 TO BROVER90
.
         ADD       DYapTO30 TO CLapTO30     CLIENT TOTALS
         ADD       DYapTO60 TO CLapTO60
         ADD       DYapTO90 TO CLapTO90
         ADD       DYapOR90 TO CLapOR90

         
         ADD       DYapTO30 TO BRapTO30     BROKER TOTALS
         ADD       DYapTO60 TO BRapTO60
         ADD       DYapTO90 TO BRapTO90
         ADD       DYapOR90 TO BRapOR90
.
         ADD       fAR       TO GRAR
         ADD       fAP       TO GRAP
         ADD       fAP1      TO GRAP1
.
.begin patch 3.85
.         BRANCH    FUNCBR TO DTL1,DTL2,DTL2,DTL3,DTL1,dtl6,dtl2,dtl6
         BRANCH    FUNCBR TO DTL1,DTL2,DTL2,DTL3,DTL1,dtl6,dtl2,dtl6,dtl2
DTL1     COMPARE   c59 TO PRTLINES
         CALL      HEADING IF NOT LESS

.Begin Patch 3.73
         move      CNCTFNAME to str20   
.         move      mname to str20
.End Patch 3.73
         match     "2" to steadjsw
.         compare   c2 to steadjsw
         if        equal
         PRINT        *02,hpbon,"$":
                      *04,STEGRNTE:
                      *06,str20:
                      *42,DY61TO90:
                      *52,DYOVER90:
                      *66,DATEPRT1:
                      *75,STELR:
                      *82,STEMLRPO:
                      *90,STEINVNO:
                      *97,DATEPRT2:
                      *102,fAP:
                      *116,LNAME17,hpboff
         else
         PRINT        *02,PERIOD:
                      *04,STEGRNTE:
                      *06,str20:
                      *42,DY61TO90:
                      *52,DYOVER90:
                      *66,DATEPRT1:
                      *75,STELR:
                      *82,STEMLRPO:
                      *90,STEINVNO:
                      *97,DATEPRT2:
                      *102,fAP:
                      *116,LNAME17
         endif
.
         ADD       C1 TO PRTLINES
.         ADD       DY00TO30 TO CL00TO30     CLIENT TOTALS
.         ADD       DY31TO60 TO CL31TO60
.         ADD       DY61TO90 TO CL61TO90
.         ADD       DYOVER90 TO CLOVER90
.
.         ADD       fAR       TO GRAR
.         ADD       fAP       TO GRAP
         GOTO      START
.
DTL2     COMPARE   c59 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      C0 TO N1                .OUTSIDE GUAR 21APR93
         MOVE      STEGUAR TO N1          .OUTSIDE GUAR 21APR93
         move      b1 to outguar
         branch    n1 of dtl2a,dtl2a,dtl2a,dtl2a
         goto      dtl2b
.        COMPARE   C0 TO N1                .OUTSIDE GUAR 21APR93
.        IF        NOT EQUAL               .        "
.        MOVE      B1 TO OUTGUAR                  .        "
.        ELSE                              .        "
dtl2a    MOVE      "@" TO OUTGUAR
.        ENDIF
dtl2b    
.Begin Patch 3.73
         move      CNCTFNAME to str20   
.         move      mname to str20
.End Patch 3.73
         match     "2" to steadjsw
.         compare   c2 to steadjsw
         if        equal
         PRINT        *02,hpbon,"$":
                      *03,OUTGUAR:
                      *04,STEGRNTE:
                      *06,str20:
                      *22,DY00TO30:
                      *32,DY31TO60:
                      *42,DY61TO90:
                      *52,DYOVER90:
                      *66,DATEPRT1:
                      *75,STELR:
                      *82,STEMLRPO:
                      *90,STEINVNO:
                      *97,DATEPRT2:
                      *102,fAP:
                      *116,LNAME17,hpboff
         else
         PRINT        *02,PERIOD:
                      *03,OUTGUAR:
                      *04,STEGRNTE:
                      *06,str20:
                      *22,DY00TO30:
                      *32,DY31TO60:
                      *42,DY61TO90:
                      *52,DYOVER90:
                      *66,DATEPRT1:
                      *75,STELR:
                      *82,STEMLRPO:
                      *90,STEINVNO:
                      *97,DATEPRT2:
                      *102,fAP:
                      *116,LNAME17
         endif
.
         ADD       C1 TO PRTLINES
.         ADD       DY00TO30 TO CL00TO30     CLIENT TOTALS
.         ADD       DY31TO60 TO CL31TO60
.         ADD       DY61TO90 TO CL61TO90
.         ADD       DYOVER90 TO CLOVER90
.
.         ADD       fAR       TO GRAR
.         ADD       fAP       TO GRAP
         GOTO      START
.
DTL3
.Begin patch 3.7
.         COMPARE   "60" TO PRTLINES 
.         COMPARE   C38 TO PRTLINES
.         CALL      CONTINUE IF NOT LESS
.*******************************************
              if              (Vertpos >= 10125)
              call            Continue
              endif
.*********************************************************************************************************
.end patch 3.7
.
         COMPARE   C0 TO DY00TO30
         GOTO      CHKCOL2 IF EQUAL
         MOVE      NUMMASK  TO COL1
         EDIT      DY00TO30 TO COL1
CHKCOL2  COMPARE   C0 TO DY31TO60
         GOTO      CHKCOL3 IF EQUAL
         MOVE      NUMMASK  TO COL2
         EDIT      DY31TO60 TO COL2
CHKCOL3  COMPARE   C0 TO DY61TO90
         GOTO      CHKCOL4 IF EQUAL
         MOVE      NUMMASK  TO COL3
         EDIT      DY61TO90 TO COL3
CHKCOL4  COMPARE   C0 TO DYOVER90
         GOTO      DTL3B IF EQUAL
         MOVE      NUMMASK  TO COL4
         EDIT      DYOVER90 TO COL4
.
DTL3B    MOVE      NUMMASK TO COL5
         EDIT      fAR      TO COL5
         MOVE      C0 TO N1                .OUTSIDE GUAR 21APR93
         MOVE      STEGUAR TO N1          .OUTSIDE GUAR 21APR93
         move      b1 to outguar
.        COMPARE   C0 TO N1                .OUTSIDE GUAR 21APR93
.        IF        NOT EQUAL               .        "
.        MOVE      B1 TO OUTGUAR                  .        "
.        ELSE                              .        "
         branch    n1 to dtl3c,dtl3c,dtl3c,dtl3c
         goto      dtl3d
dtl3c    MOVE      "@" TO OUTGUAR
.        ENDIF
dtl3d
.Begin patch 3.7
.         match     "2" to steadjsw
.          compare   c2 to steadjsw
.          if       equal
.          PrtPage             Laser;*Font=Font010,*p=125:vertpos,SteInvno,b2,OutGuar:
.                              *p=850:vertpos,dateprt1:
.                              *p=1625:vertpos,Stelr,b1,stegrnte:
.                              *p=2300:vertpos,SteMLrPO:
.;                              *ALIGNMENT=*right:
.                              *p=4875:vertpos,col1:
.                              *p=5875:vertpos,col2:
.                              *p=6875:vertpos,col3:
.                              *p=7875:vertpos,col4:
.                              *ALIGNMENT=*Left
.
.          PRINT        hpbon,b5,STEINVNO:
.                      b2,hpdtch10,hpuprght,OUTGUAR:
.                      hpt100,DATEPRT1:
.                      hpt175,b3,STELR:
.                      b1,STEGRNTE:
.                      hpt250,STEMLRPO:
.                      hpt400,COL1:
.                      hpt500,COL2:
.                      hpt600,COL3:
.                      hpt700,COL4,hpboff
.;                      COL5
.          else
          PrtPage             Laser;*Font=Font010,*p=125:vertpos,SteInvno,b2,OutGuar:
                              *p=850:vertpos,dateprt1:
                              *p=1625:vertpos,Stelr,b1,stegrnte:
                              *p=2300:vertpos,SteMLrPO:
                              *ALIGNMENT=*right:
                              *p=4875:vertpos,col1:
                              *p=5875:vertpos,col2:
                              *p=6875:vertpos,col3:
                              *p=7875:vertpos,col4:
                              *ALIGNMENT=*Left
.         PRINT        b5,hpdtch10,hpuprght,STEINVNO:
.;;         PRINT        "!R! SFNT 'TimesNewRoman' , 10, 1001, 277, .9, .3;exit;"
.;;;         PRINT        b5,steinvno:
.                      b2,OUTGUAR:
.                      hpt100,DATEPRT1:
.                      hpt175,b3,STELR:
.                      b1,STEGRNTE:
.                      hpt250,STEMLRPO:
.                      hpt400,COL1:
.                      hpt500,COL2:
.                      hpt600,COL3:
.                      hpt700,COL4
.                      COL5
.         endif
.
.         ADD       C1 TO PRTLINES
               Add            "125" to vertPos
.end patch 3.7
         ADD       fAR TO BALDUE
         GOTO      START
DTL6     COMPARE   c58 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      C0 TO N1                .OUTSIDE GUAR 21APR93
         MOVE      C1 TO JSTN
         MOVE      STEGUAR TO N1          .OUTSIDE GUAR 21APR93
         move      b1 to outguar
         branch    n1 of dtl6a,dtl6a,dtl6a,dtl6a
         goto      dtl6b
dtl6a    MOVE      "@" TO OUTGUAR
.         call      detadj2
         goto      dtl6b
.dtl6b
detadj2  
         PACK      NJSTFLD FROM INVNUM,JSTN
         rep       zfill in njstfld
         CALL      NJSTKEY
         if        over
         noreturn
         GOTO      start
         endif
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO JSTN
         GOTO      DETADJ2
         ENDIF  
......****TEMP
.prior to report date - take it else skip
          MOve      Jstdate,N8
          move      AgeCheck,N9

          IF        (N8 > N9)
          ADD       C1 TO JSTN
          GOTO      DETADJ2
          ENDIF  
          
......****TEMP
.begin patch 3.4
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        mult       seq by form92
.        div        hund into form92
        move       c0 to adjap
.        add        form92 to adjap
        add        jstap1 to adjap

        move       apmask to ap1out
        edit       adjap to ap1out
        add        adjap to grap2
.        MOVE       C0 TO CVTFLD
.        move       jstap2 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        div        hund into form92
.        mult       seq by form92
        move       c0 to form72
.        add        form92 to form72
        add         jstap2 to form72
        move       apmask to ap2out
        edit       form72 to ap2out
        add        form72 to grap2
        return
dtl6b
.Begin Patch 3.73
         move      CNCTFNAME to str20   
.         move      mname to str20
.End Patch 3.73
        compare   c8 to funcbr
        if        equal
.Begin  Patch 3.73
        move      COMPCOMP to str20
.        move      mcomp to str20
.End Patch 3.73
        endif
         PRINT        *02,PERIOD:
                      *03,OUTGUAR:
                      *04,STEGRNTE:
                      *06,str20:
                      *22,DY00TO30:
                      *32,DY31TO60:
                      *42,DY61TO90:
                      *52,DYOVER90:
                      *66,DATEPRT1:
                      *75,STELR:
                      *82,STEMLRPO:
                      *90,STEINVNO:
                      *97,DATEPRT2;
         COMPARE   C0 TO form72
         IF        NOT EQUAL
         PRINT     *99,ap2out;
         ELSE
         PRINT      *99,ap1out;
         ENDIF
.                      *102,fAP:
         display     *p1:22,grap2
         COMPARE   c59 TO PRTLINES
         CALL      HEADING IF NOT LESS
.begin patch 3.3
.        PRINT       *116,LNAME17:
.                     *n,*25,hpbon,guardes,hpboff:
.                     *53,"CHECK Info : ",chkdtem,slash,chkdted,slash,chkdtey:
.                     b1,"##",chkn1
         PRINT       *116,LNAME17:
                      *n,*25,hpbon,guardes,hpboff:
                      *53,"CHECK Info : ",chk1dtem,slash,chk1dted,slash,chk1dtey:
                      b1,"##",chkn1        
.end patch 3.3
.
         ADD       C3 TO PRTLINES
         GOTO      START
.
.. MAILER TOTAL.
.
.
MLRTOT
         COMPARE     C4 TO FUNCBR
         IF        NOT EQUAL
         move      holdmlr to moamlr
         MOVE      STEMLR TO HOLDMLR
.         MOVE      STECNT TO HOLDCNT
         ENDIF
         COMPARE   C0 TO PAGE
         GOTO      MLRTOT2 IF NOT EQUAL
         CALL      HEADING
         GOTO      MLRTOT3
owntot   move      steown to holdown
         compare   c0 to page
         goto      owntot2 if not equal
         call      heading
         goto      owntot2
owntot2  compare   c0 to page
         if        equal
         call      heading
         goto      owntot3
         endif
         compare   c57 to prtlines
         call      heading if not less
         move      c0 to gr30to90
         add       cl00tO30 to gr30to90
         add       cl31to60 to gr30to90
         add       cl61tO90 to gr30to90
         add       clover90 to gr30to90
.
         move      c0 to grap3t90
         add       claptO30 to grap3t90
         add       clapto60 to grap3t90
         add       claptO90 to grap3t90
         add       clapor90 to grap3t90
.
         MOVE      NUMMASK TO edittot
         EDIT      gr30to90 TO edittot
         PRINT        *02,PERIOD:
                   *N,*01,ownocpy:
                      *22,CL00TO30:
                      *32,CL31TO60:
                      *42,CL61TO90:
                      *52,CLOVER90,b1,"TOTAL ",edittot
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         MOVE      NUMMASK TO edittot
         EDIT      grap3t90 TO edittot
         print     *01,"[Paid out]":
                      *22,CLapTO30:
                      *32,CLapTO60:
                      *42,CLapTO90:
                      *52,CLapOR90,b1,"TOTAL ",edittot:
                   *N,*flush
         ADD       C3 TO PRTLINES
owntot3  move      steown to holdown
         move      holdown to nownfld
         call      nownkey
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         MOVE      C0 TO GR30TO90
.
         MOVE      C0 TO CLapTO30
         MOVE      C0 TO CLapTO60
         MOVE      C0 TO CLapTO90
         MOVE      C0 TO CLapOR90
         MOVE      C0 TO GRap3T90
         PRINT     *N,*01,ownocpy:
                   *N,*02,PERIOD
         ADD       C3 TO PRTLINES
         CALL      PAGES
         GOTO      DETAIL2
.
.begin patch 3.85
.MLRTOT2  BRANCH    FUNCBR TO TOT1,TOT2,TOT2,TOT3,TOT1,tot2,tot2,tot8
.BEGIN Patch 3.89
.MLRTOT2  BRANCH    FUNCBR TO TOT1,TOT2,TOT2,TOT3,TOT1,tot2,tot2,tot8,tot2
MLRTOT2  BRANCH    FUNCBR TO TOT1,TOT2,TOT2,TOT3,TOT5,tot2,tot2,tot8,tot2
.END Patch 3.89
.end patch 3.85
TOT1     COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
.Patch 3.73
         PRINT        *02,PERIOD:
                   *N,*01,COMPCOMP:
                      *42,CL61TO90:
                      *52,CLOVER90,b5,"<MOA BALANCE> ",moaamt:
                   *N
.         PRINT        *02,PERIOD:
.                   *N,*01,MCOMP:
.                      *42,CL61TO90:
.                      *52,CLOVER90,b5,"<MOA BALANCE> ",moaamt:
.                   *N
.End Patch 3.73
         ADD       C3 TO PRTLINES
         cmatch    yes to firstbk
         if        equal
         move      no to firstbk
         move      no to brkbrk
         goto      zeromlr1
         endif
.                  
         cmatch    yes to brkbrk
         if        equal
         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch 3.73
                                        call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                                        if (hold <> "")
                                                  reset hold to 7
                                                  move hold to COMPCOMP
                                        endif
.         call      nbrkkey
.         MOVE      brcomp TO MCOMP
.End Patch 3.73
          move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk,slash,hbrkcnt:
                      *22,BR00TO30:
                      *32,BR31TO60:
                      *42,BR61TO90:
                      *52,BROVER90,b1,"GRAND TOTAL ",COL5
.         PRINT     *N,*01,*25,BR00TO30,*35,BR31TO60,*42,BR61TO90,*55,BROVER90
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
         MOVE      STEBRK TO HOLDBRK
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
         move      no to brkbrk
         ADD       C3 TO PRTLINES
         endif
.
zeromlr1 ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         GOTO      MLRTOT3
.
TOT2     COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         CALL      BALREAD
         move      c0 to gr30to90
         add       cl00tO30 to gr30to90
         add       cl31to60 to gr30to90
         add       cl61tO90 to gr30to90
         add       clover90 to gr30to90
.
         move      c0 to grap3t90
         add       claptO30 to grap3t90
         add       clapto60 to grap3t90
         add       claptO90 to grap3t90
         add       clapor90 to grap3t90
.
         MOVE      NUMMASK TO edittot
         EDIT      gr30to90 TO edittot
.begin patch 3.2
.         PRINT        *02,PERIOD:
.                   *N,*01,MCOMP:
.end patch 3.2
         PRINT        *02,PERIOD:
                      *22,CL00TO30:
                      *32,CL31TO60:
                      *42,CL61TO90:
                      *52,CLOVER90,b1,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT:
                   *N
         ADD       C3 TO PRTLINES

          if        (funcBr = c6)
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         MOVE      NUMMASK TO edittot
         EDIT      grap3t90 TO edittot
         print     *01,"[Paid out]":
                      *22,CLapTO30:
                      *32,CLapTO60:
                      *42,CLapTO90:
                      *52,CLapOR90,b1,"TOTAL ",edittot:
                   *N,*flush
         ADD       C3 TO PRTLINES
         endif

         cmatch    yes to firstbk
         if        equal
         move      no to firstbk
         move      no to brkbrk
         goto      zeromlr2
         endif
.                  
         cmatch    yes to brkbrk
         if        equal
         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch 3.73
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                              endif
.         call      nbrkkey
.         MOVE      brcomp TO MCOMP
          move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk,slash,hbrkcnt:
                      *22,BR00TO30:
                      *32,BR31TO60:
                      *42,BR61TO90:
                      *52,BROVER90,b1,"GRAND TOTAL ",COL5
.         PRINT     *N,*01,*25,BR00TO30,*35,BR31TO60,*45,BR61TO90,*55,BROVER90
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
.
                    if        (funcBr = c6)
                    move      c0 to fapBRK
                   add       braptO30 to fapBRK
                   add       brapto60 to fapBRK
                   add       braptO90 to fapBRK
                   add       brapor90 to fapBRK
                   MOVE      NUMMASK TO COL5
                   EDIT      fApBRK TO COL5
                   COMPARE   c57 TO PRTLINES
                   CALL      HEADING IF NOT LESS
                   call      balread
                   PRINT     *N;
                   PRINT     *N,*01,"<BROKER TOTAL> ## [paid]":
                                *22,BRapTO30:
                                *32,BRapTO60:
                                *42,BRapTO90:
                                *52,BRapOR90,b1,"GRAND TOTAL ",COL5
                   ADD       C3 TO PRTLINES
                   MOVE      C0 TO BRapTO30
                   MOVE      C0 TO BRapTO60
                   MOVE      C0 TO BRapTO90
                   MOVE      C0 TO BRapOR90
                   move      c0 to fapBRK
                   endif
         MOVE      STEBRK TO HOLDBRK
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
         move      no to brkbrk
         ADD       C3 TO PRTLINES
         endif

         goto      zeromlr2
.
.BEGIN Patch 3.89
TOT5     COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         CALL      BALREAD
         move      c0 to gr30to90
         add       cl00tO30 to gr30to90
         add       cl31to60 to gr30to90
         add       cl61tO90 to gr30to90
         add       clover90 to gr30to90
.
         move      c0 to grap3t90
         add       claptO30 to grap3t90
         add       clapto60 to grap3t90
         add       claptO90 to grap3t90
         add       clapor90 to grap3t90
.
         MOVE      NUMMASK TO edittot
         EDIT      gr30to90 TO edittot
         PRINT        *02,PERIOD:
                      *42,CL61TO90:
                      *52,CLOVER90,b1,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT:
                   *N
         ADD       C3 TO PRTLINES


         cmatch    yes to firstbk
         if        equal
         move      no to firstbk
         move      no to brkbrk
         goto      zeromlr2
         endif
.                  
         cmatch    yes to brkbrk
         if        equal
         pack      nbrkfld from holdbrk,hbrkcnt
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                              endif
          move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk,slash,hbrkcnt:
                      *42,BR61TO90:
                      *52,BROVER90,b1,"GRAND TOTAL ",COL5
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
.
         MOVE      STEBRK TO HOLDBRK
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
         move      no to brkbrk
         ADD       C3 TO PRTLINES
         endif

         goto      zeromlr2

.END Patch 3.89
.
TOT8     COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         CALL      BALREAD
         move      c0 to gr30to90
         add       cl00tO30 to gr30to90
         add       cl31to60 to gr30to90
         add       cl61tO90 to gr30to90
         add       clover90 to gr30to90
.
         move      c0 to grap3t90
         add       claptO30 to grap3t90
         add       clapto60 to grap3t90
         add       claptO90 to grap3t90
         add       clapor90 to grap3t90
.
         MOVE      NUMMASK TO edittot
         EDIT      gr30to90 TO edittot
         PRINT        *02,PERIOD:
                   *N,*01,ownocpy:
                      *22,CL00TO30:
                      *32,CL31TO60:
                      *42,CL61TO90:
                      *52,CLOVER90,b1,"TOTAL ",edittot
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS      
         MOVE      NUMMASK TO edittot
         EDIT      grap3t90 TO edittot
         print     *01,"[Paid out]":
                      *22,CLapTO30:
                      *32,CLapTO60:
                      *42,CLapTO90:
                      *52,CLapOR90,b1,"TOTAL ",edittot:
                   *N,*flush
         ADD       C3 TO PRTLINES
         move      steown to holdown
         move      holdown to nownfld
         call      nownkey
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         MOVE      C0 TO GR30TO90
.
         MOVE      C0 TO CLapTO30
         MOVE      C0 TO CLapTO60
         MOVE      C0 TO CLapTO90
         MOVE      C0 TO CLapOR90
         MOVE      C0 TO GRap3T90
         PRINT     *N,*01,ownocpy:
                   *N,*02,PERIOD
         ADD       C3 TO PRTLINES
         CALL      PAGES
.
zeromlr2 ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         MOVE      C0 TO GR30TO90
.
         ADD       CLapTO30 TO GRapTO30     GRAND TOTALS
         ADD       CLapTO60 TO GRapTO60
         ADD       CLapTO90 TO GRapTO90
         ADD       CLapOR90 TO GRapOR90
         
.
         MOVE      C0 TO CLapTO30
         MOVE      C0 TO CLapTO60
         MOVE      C0 TO CLapTO90
         MOVE      C0 TO CLapOR90
         MOVE      C0 TO GRap3T90
         GOTO      MLRTOT3
.
TOT3    
.Begin patch 3.7
.         COMPARE   C35 TO PRTLINES
.         COMPARE   "57" TO PRTLINES
.         GOTO      TOT3B IF EQUAL
.         COMPARE   C38 TO PRTLINES
.         COMPARE   "60" TO PRTLINES
.         IF        NOT LESS        
.         CALL      CONTINUE
.         ENDIF
.         PRINT     *N;
.         ADD       C1 TO PRTLINES
.         GOTO      TOT3
TOT3B    MOVE      NUMMASK TO COL5
         EDIT      BALDUE  TO COL5
         COMPARE   C1 TO GUAFLAGo               .OUTSIDE GUARANTEES?
         IF        EQUAL
               iF             (VERTPOS >= 10000)
               CALL           cONTINUE
               ENDIF
               PrtPage        Laser;*p=125:10125,"(@) You guaranteed payment on these items. ":
                              "Please remit payment by agreed upon date."
               Add            "125" to Vertpos
.         PRINT     *N,hpt075,"(@) You guaranteed payment on these items. ":
.                    "Please remit payment by agreed upon date.";
.         ELSE
.         PRINT     *N;
         ENDIF
         COMPARE    C1 TO GUAFLAG               .NIN GUARANTEES?
         IF         EQUAL
               PrtPage        Laser;*p=125:10250,"(*) Items guaranteed for payment on your ":
                              "behalf. Please remit by agreed upon date."
.               PRINT     *N,hpt225,"(*) Items guaranteed for payment on your ":
.                    "behalf. Please remit by agreed upon date.";
.         ELSE
.         PRINT     *N;
         ENDIF
               PrtPage        Laser;*ALIGNMENT=*right:
                              *p=4875:10335,CL00TO30:
                              *p=5875:10335,CL31TO60:
                              *p=6875:10335,CL61TO90:
                              *p=7875:10335,CLOver90:
                              *ALIGNMENT=*Left:
                              *Fill=*On:
                              *Bgcolor=Colornum:
                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
                              *Fill=*Off:
                              *p=850:10335,*font=Font012BI,"Total Due:":
                              *ALIGNMENT=*right:
                              *p=3425:10335,Col5:
                              *ALIGNMENT=*Left

.         PRINT     *N,hpt400,CL00TO30,hpt500,CL31TO60,hpt600,CL61TO90:
.                   hpt700,CLOVER90
.          print     033,"*p785.5x3168.75Y",col5
.;1        PRINT     033,"*p255x3118.75Y",col5
.;                   *N,hpt250,col5:
.;                   *l,*l,*l,hpt250,col5:
.         PRINT     *N:
.                   *N:
.                   *N
         cmatch    yes to firstbk
         if        equal
         move      no to firstbk
         move      no to brkbrk
         goto      zeromlr
         endif
.                  
         cmatch    yes to brkbrk
         if        equal
         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch 3.73
                                        call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                                        if (hold <> "")
                                                  reset hold to 7
                                                  move hold to COMPCOMP
                                        endif
.Begin Patch3.73
.         call      nbrkkey
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                                        reset hold to 62
                                        move hold to COMPADDR
                                        reset hold to 132
                                        move hold to COMPCITY
                                        reset hold to 162
                                        move hold to COMPSTATE
                                        reset hold to 164
                                        move hold to COMPZIP
                                        reset hold
                                        call OLDBRKNEWCONTACT using NBRKFLD,HOLD2
                                        reset hold2 to 10
                                        move hold2 to       CNCTFNAME
                                        reset hold2 to 288
                                        move hold2 to       CNCTSALES
                              endif
statchek
                              if (cnctsales <> "06")
                                                  goto      nxtbrk
                                        endif
.         MOVE      brcntct TO MNAME
.         MOVE      brcomp TO MCOMP
.         MOVE      braddr TO MADDR
.         MOVE      brcity TO MCITY
.         MOVE      brstate TO MSTATE
.         MOVE      BRZIP TO MZIP             

.                                if (brsales <> "06")
.                                                 goto      nxtbrk
.                                       endif
.                             endif
.End Patch 3.73
         call           statehd

brk3 
.COMPARE   "57" TO PRTLINES
.;         COMPARE   C35 TO PRTLINES
.         GOTO      brkB IF EQUAL
.;         COMPARE   C35 TO PRTLINES
.         COMPARE   "60" TO PRTLINES
.         IF        NOT LESS
.         CALL      CONTINUE
.         ENDIF
.         PRINT     *N;
.         ADD       C1 TO PRTLINES
.         GOTO      brk3
BRKB
         move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
.         PRINT     *N,*N;
.         PRINT     *N;
.         PRINT     *n,*n,hpt400,BR00TO30,hpt500,BR31TO60,hpt600,BR61TO90:
.                   hpt700,BROVER90:
.                   *N,hpt250,col5:
.                   *N
               PrtPage        Laser;*ALIGNMENT=*right:
                              *p=4875:10335,BR00TO30:
                              *p=5875:10335,BR31TO60:
                              *p=6875:10335,BR61TO90:
                              *p=7875:10335,BROver90:
                              *ALIGNMENT=*Left:
                              *Fill=*On:
                              *Bgcolor=Colornum:
                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
                              *Fill=*Off:
                              *p=850:10335,*font=Font012BI,"Total Due:":
                              *ALIGNMENT=*right:
                              *p=3425:10335,Col5:
                              *ALIGNMENT=*Left
.end patch 3.7
nxtbrk
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
         MOVE      STEBRK TO HOLDBRK
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
         move      no to brkbrk
         endif
zeromlr  MOVE      C0 TO BALDUE
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         MOVE      C0 TO CLapTO30
         MOVE      C0 TO CLapTO60
         MOVE      C0 TO CLapTO90
         MOVE      C0 TO CLapOR90
         MOVE      C0 TO GUAFLAG
         MOVE      C0 TO GUAFLAGo
         MOVE      STEMLR TO HOLDMLR
.         MOVE      STECNT TO HOLDCNT
         MOVE      STEBRK TO HOLDBRK
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
         MATCH     YES TO GUAR               .NIN GUAR
         IF        EQUAL
         MOVE      C1 TO GUAFLAG
         MOVE      NO TO GUAR
         ENDIF
         MATCH      YES TO GUARo              .OUTSIDE GUAR?
         IF         EQUAL
         MOVE       C1 TO GUAFLAGo
         MOVE       NO TO GUARo
         ENDIF
.
mlrtot3
.patch 3.73    added 6/2/05 JD
.         ADD       C3 TO PRTLINES
                              move      c0 to str1
         cmatch    yes to firstbk
         if        equal
         move      no to firstbk
         move      no to brkbrk
         goto      zeromlr1
         endif
.                  
         cmatch    yes to brkbrk
         if        equal
         pack      nbrkfld from holdbrk,hbrkcnt
.
..         MOVE      C0 TO STR1
.         cmatch    yes to brkbrk
.         if        equal
.         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch 3.73
.         call      nbrkkey
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                                        reset hold to 62
                                        move hold to COMPADDR
                                        reset hold to 132
                                        move hold to COMPCITY
                                        reset hold to 162
                                        move hold to COMPSTATE
                                        reset hold to 164
                                        move hold to COMPZIP
                                        reset hold
                                        call OLDBRKNEWCONTACT using NBRKFLD,HOLD2
                                        reset hold2 to 10
                                        move hold2 to       CNCTFNAME
                              endif

.         MOVE      brcntct TO MNAME
.         MOVE      brcomp TO MCOMP
.         MOVE      braddr TO MADDR
.         MOVE      brcity TO MCITY
.         MOVE      brstate TO MSTATE
.         MOVE      BRZIP TO MZIP
.End Patch 3.73
         GOTO      PRTMLR
         endif
         PACK      NBILFLD WITH STEMLR,STECNT,STEBILTO
         PACK      MKEY FROM STEMLR,STECNT         *MUST READ MAILER EVERY
.                                                    TO GET SALESPERSON #.
                              clear     HOLD
                              clear HOLD2
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         move      b3 to careof
         PACK      NBRKFLD FROM stebrk,stebrkct
         CMATCH    B1 TO NBRKFLD
         goto      GOON IF EOS
.Begin Patch 3.73
.           call      nbrkkey
.           goto      goon if over
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              goto      goon if (hold = "")
.           call      nmlrkey
                              pack                compfld3 from mkey
                              call                COMPKEY3
.START PATCH 3.76.1 ADDED LOGIC
                              if (CNCTINACTIVE = "T")
                                        clear     cnctfname
                              endif
.END PATCH 3.76.1 ADDED LOGIC
.         cmatch    "T" to COMPBDRCTFLG            .bill direct?  11/2 dlh
.         if        equal
.begin patch 3.90
                              if        (obildrct = yes & funcbr = c4)
                              goto      prtmlr
                              endif
.                              cmatch     yes to obildrct
.                              goto       prtmlr if equal
.end patch 3.90
.                             endif
.         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
.End Patch 3.73
.Begin Patch 3.73

         reset hold to 7
                              move hold to CNCTFNAME
                              reset hold to 62
                              move hold to COMPADDR
                              reset hold to 132
                              move hold to COMPCITY
                              reset hold to 162
                              move hold to COMPSTATE
                              reset hold to 164
                              move hold to COMPZIP
                              reset hold
.         move      mcomp to mcomp
.         move      BRCOMP to mname
.         move     BRaddr to maddr
.         move      BRcity to mcity
.         move     BRstate to mstate
.         move     BRzip to mzip
         move     "C/O" to careof
.End Patch 3.73
         goto      prtmlr
GOON    
.Begin Patch 3.73
.           call      nmlrkey
                                        pack                compfld3 from mkey
                                        call                COMPKEY3
.START PATCH 3.76.1 ADDED LOGIC
                                        if (CNCTINACTIVE = "T")
                                                  clear     cnctfname
                                        endif
.END PATCH 3.76.1 ADDED LOGIC
.End Patch 3.73
         MATCH     "7364",STEMLR
         GOTO      BILLER IF EQUAL
         move      "04" to mm
         move      "01" to dd
         move      "95" to yy
         call      cvtjul
         move      juldays to str5
.Start patch #3.1 - increased var
.         unpack    steinvdt into mm,dd,yy
         unpack    steinvdt into STR2,YY,mm,dd
.END patch #3.1 - increased var
         call      cvtjul
         move      str5 to n5
         sub       juldays from n5
         goto      prtmlr if less
.         IF        LESS
.         MATCH     "7364",STEMLR
.         IF        EQUAL
.         GOTO      BILLER
.         ELSE
.         GOTO      PRTMLR
.         ENDIF
.         ENDIF
biller   clear     nbilfld
         PACK      NBILFLD WITH STEMLR,STECNT,STEBILTO
         CALL      NBILKEY
         GOTO      GETMLR IF OVER
.Begin Patch 3.73
         MOVE    BILNAME TO CNCTFNAME
         move    BILCOMP to COMPCOMP
         move    BILADDR to COMPADDR
         move    BILCITY to COMPCITY
         move    BILSTATE to COMPSTATE
         move    BILZIP to COMPZIP
.         MOVE      BILNAME TO MNAME
.         MOVE      BILCOMP TO MCOMP
.         MOVE      BILADDR TO MADDR
.         MOVE      BILCITY TO MCITY
.         MOVE      BILSTATE TO MSTATE
.         MOVE      BILZIP TO MZIP
.End Patch 3.73
         GOTO      PRTMLR
GETMLR
.Begin Patch 3.73
.not sure if this is still valid - will leave for now
         MOVE      MCCTO TO MNAME
.End Patch 3.73
.         CALL      PARSEMLR
PRTMLR   
         move      stelr to nordfld
         rep        zfill in nordfld
         call      nordkey
.Begin patch 3.6
          If             (DTPFlag = "1")       .file exists and is opem
.START PATCH 3.77 REPLACED LOGIC
.               packKey        DTPFld from SteBRK,Stemlr
.Temporary patch, above logic will be restored when NSTEBRK/NSTEMLR are using NEW Company numbers!!
 call     GetNewMlrBrk using STEMLR,STEBRK,DTPFLD
.END PATCH 3.77 REPLACED LOGIC
.                   rep       Zfill in Dtpfld
                    call      DtpKey
                    If over
                              MOVe      "Aging Not Avail." to DTPDesc
                              Move      NO to DTPInfoFLag
                    Else
                              Move      Yes to DTPInfoFLag
                              MOVe      "Days to Pay: " to DTPDesc
                              IF (DTP3yrcount > 0 & Dtp3yrdays > 0)
                                        Divide    DTP3Yrcount into DTP3yrdays
                              else
                                        Move      c0 to dtp3yrdays
                              endif
                              IF (DTP1yrcount > 0 & Dtp1yrdays > 0)
                                        Divide    DTP1Yrcount into DTP1yrdays
                              else
                                        Move      c0 to dtp1yrdays
                              endif
                    endif
          Endif
.end patch 3.6
          if (STECompID = "P")
                    move      " (PLI)",str6
          else
                    clear     str6
          endif
        BRANCH    FUNCBR TO MLR1,MLR1,MLR2,MLR3,MLR1,mlr1,mlr1
MLR1     COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE       C0 TO N2
         clear      n2sls
         CLEAR      SLS
.         MOVE       MSLSPER TO N2
         pack       n2sls from stesls10,stesales
         move       n2sls to n2
         LOAD       SLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                    OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                    OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20:
                    OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.Begin patch 3.6
.         PRINT     *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT,B2,"(",SLS,")":
               IF             (dtp3yrdays >= 90 or dtp1yrdays >= 90)
.Begin Patch 3.73
.               PRINT          *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT,B2,"(",SLS,") ",HPBON,DtpDesc,dtp3yrdays," 3yr Avr, ",Dtp1yrdays," 1 yr Avr":
               PRINT          *N,*01,COMPCOMP,str6,B1,STEMLR,SLASH,steCNT,B2,"(",SLS,") ",HPBON,DtpDesc,dtp3yrdays," 3yr Avr, ",Dtp1yrdays," 1 yr Avr":
                              HPBOff,*N,*02,PERIOD
.End Patch 3.73
                   Else
.Begin Patch 3.73
.               PRINT          *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT,B2,"(",SLS,") ",DtpDesc,dtp3yrdays," 3yr Avr, ",Dtp1yrdays," 1 yr Avr":
               PRINT          *N,*01,COMPCOMP,str6,B1,STEMLR,SLASH,steCNT,B2,"(",SLS,") ",DtpDesc,dtp3yrdays," 3yr Avr, ",Dtp1yrdays," 1 yr Avr":
                              *N,*02,PERIOD
.End Patch 3.73
                   endif
.end patch 3.6
         ADD       C3 TO PRTLINES
         CALL      PAGES
         GOTO      DETAIL2
MLR2     COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
.Begin patch 3.6
.         PRINT     *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT:
               IF             (dtp3yrdays >= 90 or dtp1yrdays >= 90)
.Begin Patch 3.73
.               PRINT          *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT,HPBON,DtpDesc,dtp3yrdays," 3yr, ",Dtp1yrdays," 1 yr":
               PRINT          *N,*01,COMPCOMP,B1,STEMLR,SLASH,steCNT,HPBON,DtpDesc,dtp3yrdays," 3yr, ",Dtp1yrdays," 1 yr":
                              HPBoff,*N,*02,PERIOD
.End Patch 3.73
                   Else
.Begin Patch 3.73
.               PRINT          *N,*01,MCOMP,B1,STEMLR,SLASH,steCNT,DtpDesc,dtp3yrdays," 3yr, ",Dtp1yrdays," 1 yr":
               PRINT          *N,*01,COMPCOMP,B1,STEMLR,SLASH,steCNT,DtpDesc,dtp3yrdays," 3yr, ",Dtp1yrdays," 1 yr":
                              *N,*02,PERIOD
.End Patch 3.73
                   endif
.end patch 3.6
         ADD       C3 TO PRTLINES
         CALL      PAGES
         GOTO      DETAIL2
MLR3     CALL      STATEHD
         GOTO      DETAIL2
.
.
.
.begin patch 3.85
.EOJ      BRANCH    FUNCBR TO EOJ1,EOJ2,EOJ2,EOJ3,EOJ1,eoj2,eoj2,eoj2
.begin Patch 3.89
.EOJ      BRANCH    FUNCBR TO EOJ1,EOJ2,EOJ2,EOJ3,EOJ1,eoj2,eoj2,eoj2,eoj2
EOJ      BRANCH    FUNCBR TO EOJ1,EOJ2,EOJ2,EOJ3,EOJ5,eoj2,eoj2,eoj2,eoj2
.end Patch 3.89
.end patch 3.85
.
EOJ1     BRANCH    TOTFLAG OF EOJ1X,EOJ1A
EOJ1A    COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         CALL      BALREAD
.Begin Patch 3.73
         PRINT       *02,PERIOD:
                   *N,*01,COMPCOMP:
                      *42,CL61TO90:
                      *52,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT:
                   *N
.         PRINT       *02,PERIOD:
.                   *N,*01,MCOMP:
.                      *42,CL61TO90:
.                      *52,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT:
.                   *N
.End Patch 3.73
         ADD       C3 TO PRTLINES
.
         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch3.73
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                              endif
.         call      nbrkkey
.         MOVE      brcomp TO MCOMP
.End Patch 3.73
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk,slash,hbrkcnt:
                      *22,BR00TO30:
                      *32,BR31TO60:
                      *42,BR61TO90:
                      *52,BROVER90
.                      *55,BROVER90
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL3
         EDIT      GR61TO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GROVER90 TO COL4
         PRINT     *N,"PROCESSED : ",N6:
                   *N:
                   *N,"TOTALS:":
                      *58,"61 - 90":
                      *73,"OVER 90":
                   *N,*56,"---------":
                      *71,"---------":
                   *N,*54,COL3:
                      *69,COL4
EOJ1X    BRANCH    FUNCBR OF EOJ1Y
         GOTO      EXIT
EOJ1Y    WEOF      MLRCRED,SEQ
         CLOSE     MLRCRED,EOFSIZE
         GOTO      EXIT
.
EOJ2     BRANCH    TOTFLAG OF EOJ2A,EOJ2A
EOJ2A    COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         CALL      BALREAD
.begin patch 3.2
.         PRINT        *02,PERIOD:
.                   *N,*01,MCOMP:
.end patch 3.2
.Begin patch 3.93
         CLEAR     moaamt
         clear     nmoafld4
         PACK      NMOAFLD4 FROM Nbrkfld,moamlr
         REP       ZFILL IN NMOAFLD4
         move      no to over
         CALL      NMOBKEY
         cmatch    "Y" to over
         if         equal
         move      "0.00" to balance
         endif
        MOVE      DOLLAR TO moaamt
         MULT      "-1" BY BALANCE
         MOVE      BALANCE TO moa$
         EDIT      moa$ TO moaamt
.End patch 3.93
         PRINT        *02,PERIOD:
                      *22,CL00TO30:
                      *32,CL31TO60:
                      *42,CL61TO90:
                      *52,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT:
                   *N
         ADD       C3 TO PRTLINES
.
         pack      nbrkfld from holdbrk,hbrkcnt
.Start patch3.73
.         call      nbrkkey
.         MOVE      brcomp TO MCOMP
                              call      OLDBRKNEWCOMP using NBRKFLD,HOLD
                              if (hold <> "")
                                        reset hold to 7
                                        move hold to COMPCOMP
                              endif
.End patch3.73
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk,slash,hbrkcnt:
                      *22,BR00TO30:
                      *32,BR31TO60:
                      *42,BR61TO90:
                      *52,BROVER90
         BRANCH    TOTFLAG OF EXIT,EOJ2B
EOJ2B
          if        (funcBr = c6)
          move      c0 to fapBRK
         add       braptO30 to fapBRK
         add       brapto60 to fapBRK
         add       braptO90 to fapBRK
         add       brapor90 to fapBRK
         MOVE      NUMMASK TO COL5
         EDIT      fApBRK TO COL5
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         call      balread
         PRINT     *N;
         PRINT     *N,*01,"<BROKER TOTAL> ## [paid]":
                      *22,BRapTO30:
                      *32,BRapTO60:
                      *42,BRapTO90:
                      *52,BRapOR90,b1,"GRAND TOTAL ",COL5
         ADD       C3 TO PRTLINES
         MOVE      C0 TO BRapTO30
         MOVE      C0 TO BRapTO60
         MOVE      C0 TO BRapTO90
         MOVE      C0 TO BRapOR90
         move      c0 to fapBRK
         endif


         ADD       CL00TO30 TO GR00TO30
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GR00TO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GR31TO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GR61TO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GROVER90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAR     TO COL5
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP     TO COL6
.
         MOVE      GR00TO30 TO PRC30
         DIVIDE    GRAR   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GR31TO60 TO PRC60
.start patch 3.81
         DIVIDE    GRAR   INTO PRC60      turned back on 9/20/05 JD
.end patch 3.81
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GR61TO90 TO PRC90
         DIVIDE    GRAR   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GROVER90 TO PRCOV
         DIVIDE    GRAR   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
.
.         compare   c6 to funcbr
.         if        equal
.         MOVE      NUMMASK  TO COL6
.         EDIT      GRAP2    TO COL6
.         endif
. moved up before grand totals, col totals getting overwritten
.         compare   c6 to funcbr
.         if        equal
.          move      c0 to fapBRK
.         add       braptO30 to fapBRK
.         add       brapto60 to fapBRK
.         add       braptO90 to fapBRK
.         add       brapor90 to fapBRK
.         MOVE      NUMMASK TO COL5
.         EDIT      fApBRK TO COL5
.         COMPARE   c57 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.         call      balread
.         PRINT     *N;
.         PRINT     *N,*01,"<BROKER TOTAL> ## [paid]":
.                      *22,BRapTO30:
.                      *32,BRapTO60:
.                      *42,BRapTO90:
.                      *52,BRapOR90,b1,"GRAND TOTAL ",COL5
.         ADD       C3 TO PRTLINES
.         MOVE      C0 TO BRapTO30
.         MOVE      C0 TO BRapTO60
.         MOVE      C0 TO BRapTO90
.         MOVE      C0 TO BRapOR90
.         move      c0 to fapBRK
.         endif
.
.
         PRINT     *N,"PROCESSED : ",N6:
                   *N:
                   *N,"TOTALS:":
                      *29,"0 - 30":
                      *43,"31 - 60":
                      *58,"61 - 90":
                      *73,"OVER 90":
                      *88,"TOTL AR":
                      *103,"TOTL AP":
                   *N,*26,"---------":
                      *41,"---------":
                      *56,"---------":
                      *71,"---------":
                      *86,"---------":
                      *101,"---------":
                   *N,*24,COL1:
                      *39,COL2:
                      *54,COL3:
                      *69,COL4:
                      *84,COL5:
                      *99,COL6
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP1    TO COL6
         PRINT     *22,PRC30,PRC:
                      *40,PRC60,PRC:
                      *52,PRC90,PRC:
                      *70,PRCOV,PRC:
                      *99,COL6
         ADD       C7 TO PRTLINES
         BRANCH    FUNCBR OF EXIT,EXIT,EXIT,EXIT,EXIT,EOJ2P,Exit,EOJ2P
EOJ2P
         ADD       CLapTO30 TO GRapTO30     GRAND TOTALS
         ADD       CLapTO60 TO GRapTO60
         ADD       CLapTO90 TO GRapTO90
         ADD       CLapOR90 TO GRapOR90
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GRapTO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GRapTO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GRapTO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GRapOR90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAp2    TO COL5
.
         MOVE      GRapTO30 TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GRapTO60 TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GRapTO90 TO PRC90
         DIVIDE    GRAp2   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GRapOR90 TO PRCOV
         DIVIDE    GRAp2   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
         print          *N:
                   *N,"A/P TOTALS:":
                      *29,"0 - 30":
                      *43,"31 - 60":
                      *58,"61 - 90":
                      *73,"OVER 90":
                      *88,"TOTL AP":
                   *N,*26,"---------":
                      *41,"---------":
                      *56,"---------":
                      *71,"---------":
                      *86,"---------":
                   *N,*24,COL1:
                      *39,COL2:
                      *54,COL3:
                      *69,COL4:
                      *84,COL5
         PRINT     *22,PRC30,PRC:
                      *40,PRC60,PRC:
                      *52,PRC90,PRC:
                      *70,PRCOV,PRC:
                      *99,COL6
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      Guarap TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      guarothr TO COL2
         MOVE      guarap TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      guarothr TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         print          *N:
                   *N,"Guaranteed:":
                      *29,col1,b2,prc30,prc:
                   *N,"Other:":
                      *29,col2,b2,prc60,prc
         GOTO      EXIT
.
EOJ3     
.Begin patch 3.7
.;         COMPARE   C37 TO PRTLINES
.         COMPARE   "60" TO PRTLINES
.         GOTO      EOJ3B IF EQUAL
.         PRINT     *N;
.         ADD       C1 TO PRTLINES
.         GOTO      EOJ3
EOJ3B    MOVE      NUMMASK TO COL5
         EDIT      BALDUE  TO COL5
         COMPARE   C1 TO GUAFLAGo               .OUTSIDE GUARANTEES?
         IF        EQUAL
               iF             (VERTPOS >= 10000)
               CALL           cONTINUE
               ENDIF
               PrtPage        Laser;*p=125:10125,"(@) You guaranteed payment on these items. ":
                              "Please remit payment by agreed upon date."
               Add            "125" to Vertpos
         ENDIF
         COMPARE    C1 TO GUAFLAG               .NIN GUARANTEES?
         IF         EQUAL
               PrtPage        Laser;*p=125:10250,"(*) Items guaranteed for payment on your ":
                              "behalf. Please remit by agreed upon date."
         ENDIF
               PrtPage        Laser;*ALIGNMENT=*right:
                              *p=4875:10335,CL00TO30:
                              *p=5875:10335,CL31TO60:
                              *p=6875:10335,CL61TO90:
                              *p=7875:10335,CLOver90:
                              *ALIGNMENT=*Left:
                              *Fill=*On:
                              *Bgcolor=Colornum:
                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
                              *Fill=*Off:
                              *p=850:10335,*font=Font012BI,"Total Due:":
                              *ALIGNMENT=*right:
                              *p=3425:10335,Col5:
                              *ALIGNMENT=*Left
.         PRINT     hpt400,CL00TO30,hpt500,CL31TO60,hpt600,CL61TO90:
.                   hpt700,CLOVER90:
.;                   *N,hpt225,COL5:
.                   033,"*p785.5x3168.75Y",col5:
.                   *N:
.                   *N:
.                   *N
         pack      nbrkfld from holdbrk,hbrkcnt
.Begin Patch 3.73
.         call      nbrkkey
.Patch 3.76 Comment Out
.                             call      OLDBRKNEWCOMP using NBRKFLD,HOLD
.                             if (hold <> "")
.                                       reset hold to 7
.                                       move hold to COMPCOMP
.                                       reset hold to 62
.                                       move hold to COMPADDR
.                                       reset hold to 132
.                                       move hold to COMPCITY
.                                       reset hold to 162
.                                       move hold to COMPSTATE
.                                       reset hold to 164
.                                       move hold to COMPZIP
.                                       reset hold
.                                       call OLDBRKNEWCONTACT using NBRKFLD,HOLD2
.                                       reset hold2 to 10
.                                       move hold2 to       CNCTFNAME
.                             endif
.Patch 3.76 Comment Out
.         MOVE      brcntct TO MNAME
.         MOVE      brcomp TO MCOMP
.         MOVE      braddr TO MADDR
.         MOVE      brcity TO MCITY
.         MOVE      brstate TO MSTATE
.         MOVE      BRZIP TO MZIP
.End patch3.73

               call           statehd
eoj3c    
.;         COMPARE   "57" TO PRTLINES
.;        COMPARE   C35 TO PRTLINES
.;         GOTO      eoj3d IF EQUAL
.         COMPARE   "60" TO PRTLINES
.          goto      eoj3d if equal
.;         IF        NOT LESS
.;         CALL      CONTINUE
.;         ENDIF
.         PRINT     *N;
.         ADD       C1 TO PRTLINES
.         GOTO      eoj3c
eoj3d
         move      c0 to farbrk
         add       br00tO30 to farbrk
         add       br31to60 to farbrk
         add       br61tO90 to farbrk
         add       brover90 to farbrk
         MOVE      NUMMASK TO COL5
         EDIT      farbrk  TO COL5
.;         PRINT     *N,*N;
.;         PRINT     *N;
.         PRINT     hpt400,BR00TO30,hpt500,BR31TO60,hpt600,BR61TO90:
.                   hpt700,BROVER90:
.;                   *N,hpt225,COL5:
.                   033,"*p785.5x3168.75Y",col5:
.                   *N:
.                   *N:
.                   *N
               PrtPage        Laser;*ALIGNMENT=*right:
                              *p=4875:10335,BR00TO30:
                              *p=5875:10335,BR31TO60:
                              *p=6875:10335,BR61TO90:
                              *p=7875:10335,BROver90:
                              *ALIGNMENT=*Left:
                              *Fill=*On:
                              *Bgcolor=Colornum:
                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
                              *Fill=*Off:
                              *p=850:10335,*font=Font012BI,"Total Due:":
                              *ALIGNMENT=*right:
                              *p=3425:10335,Col5:
                              *ALIGNMENT=*Left
               move      c0 to farbrk
.Begin Patch 3.89
                    Goto      Exit
EOJ5     
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         CALL      BALREAD

         PRINT       *02,PERIOD:
                   *N,*01,COMPCOMP:
                      *42,CL61TO90:
                      *52,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT:
                   *N
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90

         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL3
         EDIT      GR61TO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GROVER90 TO COL4
         PRINT     *N,"PROCESSED : ",N6:
                   *N:
                   *N,"TOTALS:":
                      *58,"61 - 90":
                      *73,"OVER 90":
                   *N,*56,"---------":
                      *71,"---------":
                   *N,*54,COL3:
                      *69,COL4
         COMPARE   c57 TO PRTLINES
         CALL      HEADING IF NOT LESS



         MOVE      NUMMASK TO COL5
          EDIT      BALDUE  TO COL5


         ADD       CLapTO30 TO GRapTO30     GRAND TOTALS
         ADD       CLapTO60 TO GRapTO60
         ADD       CLapTO90 TO GRapTO90
         ADD       CLapOR90 TO GRapOR90
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GRapTO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GRapTO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GRapTO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GRapOR90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAp2    TO COL5
.
         MOVE      GRapTO30 TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GRapTO60 TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GRapTO90 TO PRC90
         DIVIDE    GRAp2   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GRapOR90 TO PRCOV
         DIVIDE    GRAp2   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
         print          *N:
                   *N,"A/P TOTALS:":
                      *58,"61 - 90":
                      *73,"OVER 90":
                      *88,"TOTL AP":
                   *N,*56,"---------":
                      *71,"---------":
                      *86,"---------":
                   *N,*54,COL3:
                      *69,COL4:
                      *84,COL5
         PRINT        *52,PRC90,PRC:
                      *70,PRCOV,PRC:
                      *99,COL6
         COMPARE   C57 TO PRTLINES
         CALL      HEADING IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      Guarap TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      guarothr TO COL2
         MOVE      guarap TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      guarothr TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         print          *N:
                   *N,"Guaranteed:":
                      *29,col1,b2,prc30,prc:
                   *N,"Other:":
                      *29,col2,b2,prc60,prc


.               PrtPage        Laser;*ALIGNMENT=*right:
.                              *p=6875:10335,CL61TO90:
.                              *p=7875:10335,CLOver90:
.                              *ALIGNMENT=*Left:
.                              *Fill=*On:
.                              *Bgcolor=Colornum:
.                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
.                              *Fill=*Off:
.                              *p=850:10335,*font=Font012BI,"Total Due:":
.                              *ALIGNMENT=*right:
.                              *p=3425:10335,Col5:
.                              *ALIGNMENT=*Left
.         pack      nbrkfld from holdbrk,hbrkcnt
.
.               call           statehd

.         add       br00tO30 to farbrk
.         add       br31to60 to farbrk
.         add       br61tO90 to farbrk
.         add       brover90 to farbrk
.         MOVE      NUMMASK TO COL5
.         EDIT      farbrk  TO COL5
.               PrtPage        Laser;*ALIGNMENT=*right:
.                              *p=6875:10335,BR61TO90:
.                              *p=7875:10335,BROver90:
.                              *ALIGNMENT=*Left:
.                              *Fill=*On:
.                              *Bgcolor=Colornum:
.                              *RECT=10325:10575:725:3550:                                ;top:bot:left:right
.                              *Fill=*Off:
.                              *p=850:10335,*font=Font012BI,"Total Due:":
.                              *ALIGNMENT=*right:
.                              *p=3425:10335,Col5:
.                              *ALIGNMENT=*Left
               move      c0 to farbrk

.end Patch 3.89



.
EXIT  
         BRANCH    PRTFLAG TO END
               if             (funcbr <> 4)
               SPLCLOSE
               Elseif         (funcbr = 4)
               prtclose       Laser
.begin patch 3.92
.               Pack           str45,"C:\WORK\FAXFILE.PRN"
..begin patch 3.87
...               Pack           str45,"\\nts0\d\data\fax\FAXFILE.PRN"
..               If                  (osflag = c1 | osflag = c5)
..               append  "!c:\winnt\system32\cmd.exe",taskname
..               ElseIf                  (osflag = c3 | osflag = c4)
..               append  "!c:\command.com",taskname
..               ElseIf                  (osflag = c6)
..               append  "!c:\windows\system32\cmd.exe",taskname
..               endif
..                append        " /c copy ",taskname
..                append        str45,taskname
..                append        B1,taskname
.                append        NTWKPATH1,taskname
.                append        Prtname,taskname
.                reset         taskname
..                execute       taskname
.                    Copyfile  str45,taskname
          Clear     Mailbody
          append    "This is a message from the Statement Print Program",Mailbody
          Append    CRLF,MailBody                                               
          Append    "Your PDF file was created!",Mailbody
          Append    CRLF,MailBody                                               
          append    "Location:  c:\work\pdf\ ",mailbody
          Append    CRLF,MailBody                                               
          append    "filename:  ",mailbody
          append    prtname,mailbody
          Append    CRLF,MailBody                                               
          Reset     MailBody
                    
          pack      mailattach,"c:\work\pdf\",Prtname,".pdf"         
CheckFile
          trap      WaitForEnd giving error if IO
          open      FileCheck,Mailattach,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      Mailto,USER,"@nincal.com"
          pack      Mailfrom,USER,"@nincal.com"
          move      "Here is your PDF File",Mailsubjct
          call      SendMail
.end patch 3.92
.end patch 3.87
               erase          str45
               endif
.end patch 3.7
         CALL      REMVTOF
         BRANCH    FUNCBR OF PGEEOF,PGEEOF,PGEEOF,END,PGEEOF,pgeeof,pgeeof:
                   pgeeof
.         CALL      NPGEEOF
         GOTO      END
PGEEOF
.START PATCH 3.77 REMOVED LOGIC
.         CALL       NPGEEOF
.END PATCH 3.77 REMOVED LOGIC
         GOTO       END
.
+...........................................................................
.
HEADING  ADD       C1 TO PAGE
.begin patch 3.85
.         BRANCH    FUNCBR TO HD1,HD2,HD2,NOHD,HD1,hd2,hd2,hd2
         BRANCH    FUNCBR TO HD1,HD2,HD2,NOHD,HD1,hd2,hd2,hd2,hd2
.end patch 3.85
NOHD     RETURN
HD1
         compare    c1 to page
         if         equal
.         print      hp17ptch,hpdups,hptop,*f
.         print      hp17ptch,hpdupl,hptop,*f
.begin patch 3.71
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
.end patch 3.71
.START PATCH 3.76 REPLACED LOGIC - ASH
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,RPTTYPE:
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
.                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
.                      *112,"A/P":
.                      *116,"LIST NAME":
.                   *N,*46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
          IF        (STECompID = "P")
          PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
                    *54,"Pacific Lists Inc.":
                    *119,"DATE: ",TODAY:
                    *N,*52,RPTTYPE:
                    *119,"PAGE:    ",PAGE:
                    *N,*58,MONTH:
                    *N:
                    *N,*48,"61 - 90":
                    *58,"OVER 90":
                    *66,"MAILDATE":
                    *78,"LR##":
                    *83,"MLR PO":
                    *92,"INV##":
                    *97,"INV.DATE":
                    *112,"A/P":
                    *116,"LIST NAME":
                    *N,*46,"---------":
                    *56,"---------":
                    *66,"--------":
                    *75,"------":
                    *82,"-------":
                    *90,"------":
                    *97,"--------":
                    *106,"---------":
                    *116,"-----------------"
          Else
          PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
                    *54,"NAMES IN THE NEWS":
                    *119,"DATE: ",TODAY:
                    *N,*52,RPTTYPE:
                    *119,"PAGE:    ",PAGE:
                    *N,*58,MONTH:
                    *N:
                    *N,*48,"61 - 90":
                    *58,"OVER 90":
                    *66,"MAILDATE":
                    *78,"LR##":
                    *83,"MLR PO":
                    *92,"INV##":
                    *97,"INV.DATE":
                    *112,"A/P":
                    *116,"LIST NAME":
                    *N,*46,"---------":
                    *56,"---------":
                    *66,"--------":
                    *75,"------":
                    *82,"-------":
                    *90,"------":
                    *97,"--------":
                    *106,"---------":
                    *116,"-----------------"
          Endif               
.END PATCH 3.76 REPLACED LOGIC - ASH
         MOVE      C8 TO PRTLINES
         RETURN
.
HD2  
         compare    c1 to page
         if         equal
.         print      hp17ptch,hpdups,hptop,*f
.         print      hp17ptch,hpdupl,hptop,*f
. begin patch 3.71
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
.end patch 3.71
.START PATCH 3.76 REPLACED LOGIC - ASH
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,RPTTYPE:
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*29,"0 - 30":
.                      *38,"31 - 60":
.                      *48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
.                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
.                      *112,"A/P":
.                      *116,"LIST NAME":
.                   *N,*26,"---------":
.                      *36,"---------":
.                      *46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
          IF        (SteCompId = "P")
          PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
                    *54,"Pacific Lists Inc.":
                    *119,"DATE: ",TODAY:
                    *N,*52,RPTTYPE:
                    *119,"PAGE:    ",PAGE:
                    *N,*58,MONTH:
                    *N:
                    *N,*29,"0 - 30":
                    *38,"31 - 60":
                    *48,"61 - 90":
                    *58,"OVER 90":
                    *66,"MAILDATE":
                    *78,"LR##":
                    *83,"MLR PO":
                    *92,"INV##":
                    *97,"INV.DATE":
                    *112,"A/P":
                    *116,"LIST NAME":
                    *N,*26,"---------":
                    *36,"---------":
                    *46,"---------":
                    *56,"---------":
                    *66,"--------":
                    *75,"------":
                    *82,"-------":
                    *90,"------":
                    *97,"--------":
                    *106,"---------":
                    *116,"-----------------"
          Else
          PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
                    *54,"NAMES IN THE NEWS":
                    *119,"DATE: ",TODAY:
                    *N,*52,RPTTYPE:
                    *119,"PAGE:    ",PAGE:
                    *N,*58,MONTH:
                    *N:
                    *N,*29,"0 - 30":
                    *38,"31 - 60":
                    *48,"61 - 90":
                    *58,"OVER 90":
                    *66,"MAILDATE":
                    *78,"LR##":
                    *83,"MLR PO":
                    *92,"INV##":
                    *97,"INV.DATE":
                    *112,"A/P":
                    *116,"LIST NAME":
                    *N,*26,"---------":
                    *36,"---------":
                    *46,"---------":
                    *56,"---------":
                    *66,"--------":
                    *75,"------":
                    *82,"-------":
                    *90,"------":
                    *97,"--------":
                    *106,"---------":
                    *116,"-----------------"
          Endif               
.END PATCH 3.76 REPLACED LOGIC - ASH
         MOVE      C9 TO PRTLINES
         RETURN
.
.Begin patch 3.7
.CONTINUE PRINT     *N:
.                      hpt250,"CONTINUED":
.                   *N:
.                   *N:
.                   *N
.CONTINUE PRINT     *n,hpt250,"CONTINUED"
CONTINUE
. PRINT     *l,*l,hpt250,"CONTINUED"
.          PRINT     033,"*p251x5000.75Y""
               PRtpage        Laser;*p=775:10335,*font=Font012BI,"Continued:"
.                              *Fill=*On:
.                              *Bgcolor=Colornum:
.                              *RECT=10325:10575:975:3800:                                ;top:bot:left:right
.                              *Fill=*Off:
.                              *p=1025:10335,*font=Font012BI,"Total Due:"
.          print     033,"*p785.5x3168.75Y","CONTINUED"
.
.end patch 3.7
STATEHD  MATCH     YES TO FIRST
         IF        EQUAL
         MOVE      STEMLR TO HOLDMLR
.         MOVE      STECNT TO HOLDCNT
         ENDIF
         cmatch    yes to brkbrk
         if        equal
         move      holdbrk to holdmlr
.         move      hbrkcnt to holdcnt
         endif
.begin patch 3.5
.         PRINT     *f,hpstmnt:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N,hpdtch10,hpuprght,*15,MNAME:
.                   *N,*15,MCOMP,hpt600,TODAY:
.                   *N,*15,MADDR,hpt600,"##",HOLDMLR:
.                   *N,*15,MCITY,B1,MSTATE,B1,MZIP:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N
.Begin patch 3.7
.         call      PrtARForm
               call           TestForm
.         PRINT     *N,*n:
.         PRINT        "!R! SFNT 'ITC-Bookman-LightSWA' , 10;exit;"
.         PRINT        "!R! SFNT 'TimesNewRoman' , 10;exit;"
.         PRINT        "!R! FSET 1p##v0s0b4101t;
.Patch 3.73
                                                  call trim using compcity
               PrtPage        Laser;*p=750:1500,*font=Font010B,CNCTFNAME:
                              *p=750:1625,COMPCOMP:
                              *p=6000:1625,Today:
                              *p=750:1750,COMPAddr:
                              *p=6000:1750,"##",HoldMLr:
                              *p=750:1875,*ll,COMPCITY,COMMA,b1,COMPSTATE,B1,COMPZIP
.               PrtPage        Laser;*p=750:1500,*font=Font010B,MName:
.                              *p=750:1625,MComp:
.                              *p=6000:1625,Today:
.                              *p=750:1750,MAddr:
.                              *p=6000:1750,"##",HoldMLr:
.                              *p=750:1875,MCITY,B1,MSTATE,B1,MZIP
.Patch 3.73

               Move           "3360" to Vertpos
.         PRINT     *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N,hpdtch10,hpuprght,*15,MNAME:
.                   *N,*15,MNAME:
.                   *N,*15,MCOMP,hpt600,TODAY:
.                   *N,*15,MADDR,hpt600,"##",HOLDMLR:
.                   *N,*15,MCITY,B1,MSTATE,B1,MZIP:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N:
.                   *N
.end patch 3.7

.;;;;;;                   *N,*15,MADDR,hpt600,"##",HOLDMLR,SLASH,HOLDCNT: dlh 31mar95
         MOVE      "20" TO PRTLINES
         MOVE      NO TO FIRST
         RETURN
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  MOVE      "0000000000" TO CVTFLD
.         RETURN
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.
.PARSEMLR BRANCH    FUNCBR TO PRSE1,PRSE1,PRSE1,PRSE3,PRSE1,prse1,prse1
PARSEMLR RETURN
.NINCAL 19AUG91 TURNED OFF NOT USED NINCAL. DLH.
PRSE1    
                              SCAN      "C/O" IN MCOMP
         GOTO      PRSE1B IF NOT EQUAL
         BUMP      MCOMP BY -1
         APPEND    B3 TO MCOMP
         RESET     MCOMP
         SETLPTR   MCOMP
         RESET     MNAME TO 19
         APPEND    B6 TO MNAME
         RESET     MNAME
         RETURN
PRSE1B   PACK      MNAME WITH B10,B10,B5
         RETURN
PRSE3    SCAN      "C/O" IN MCOMP
         RETURN    IF NOT EQUAL
         BUMP      MCOMP BY -1
         APPEND    B3 TO MCOMP
         RESET     MCOMP
         SETLPTR   MCOMP
         PACK      MNAME WITH B10,B10,B5
         RETURN
.
.begin patch 3.85
.PAGES    BRANCH    FUNCBR OF CHKAGE,WRTPAGE,WRTPAGE,NOPAGE,CHKAGE,wrtpage,wrtpage:
.                   wrtpage
PAGES    BRANCH    FUNCBR OF CHKAGE,WRTPAGE,WRTPAGE,NOPAGE,CHKAGE,wrtpage,wrtpage:
                   wrtpage,NOPage
.end patch 3.85
NOPAGE   RETURN
.
CHKAGE   CMATCH    YES TO OVER60
         RETURN    IF NOT EQUAL
.
WRTPAGE  MOVE      PAGE TO N4
         MOVE      N4 TO INDPAGE
.START PATCH 3.77 MOVED LOGIC
.         MOVE      STEMLR TO INDMNUM
.END PATCH 3.77 MOVED LOGIC
.Begin Patch 3.73
         move      CNCTFNAME to INDNAME 
.         move      mname to str20
.         MOVE      MNAME TO INDNAME
                              move COMPCOMP to INDCOMP
.                             MOVE      MCOMP TO INDCOMP
.End Patch 3.73
.START PATCH 3.77 MOVED LOGIC
.Temporary patch, above logic will be restored when NSTEBRK/NSTEMLR are using NEW Company numbers!!
.Above logic is patch just under WRTPAGE label
          call      GetNewMlr using STEMLR,INDMNUM
.END PATCH 3.77 MOVED LOGIC
         compare   c8 to funcbr
         if        equal
         move      ownocpy to indcomp
         clear     indname
         move      holdown to indnum
         endif
.START PATCH 3.77 REPLACED LOGIC
.         CALL      NPGEWRT
          move      INDMNUM,NPGEFLD
          move      "NPGEWRT",Location
          pack      KeyLocation,"Key: ",NPGEFLD
          CALL      NPGEWRT
          call      PageUpdate
.END PATCH 3.77 ADDED LOGIC
         BRANCH    FUNCBR OF NOCREDIT
         RETURN
NOCREDIT WRITE     MLRCRED,SEQ;STEMLR,Z3
         match     b4 to holdbrk
         return    if eos
         read      brkcred,holdbrk;;
         return    if not over
         write     brkcred,holdbrk;holdbrk,z3
         RETURN
.
FBRKR
         move      stebrk to holdbrk
         MOVE      STEmlr TO moamlr
         move      stebrkct to hbrkcnt
.         move      stemlr to holdmlr
         move      yes to firstbk
         move      c1 to brokerFlag
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         goto      mlrtot
         
balread
         CLEAR     moaamt
         clear     nmoafld4
         PACK      NMOAFLD4 FROM holdbrk,moamlr
.         PACK      NMOAFLD4 FROM holdbrk,holdmlr
         REP       ZFILL IN NMOAFLD4
         move      no to over
         CALL      NMOBKEY
         cmatch    "Y" to over
         if         equal
         move      "0.00" to balance
         endif
        MOVE      DOLLAR TO moaamt
         MULT      "-1" BY BALANCE
         MOVE      BALANCE TO moa$
         EDIT      moa$ TO moaamt
         return

+...........................................................................
.begin patch 3.5
................................................................................................
.PrtARForm
.09/24/2004 THE FOLLOWING ROUTINE IS *NEVER* CALLED SO LOGO CONVERSION IS UNNECESSARY!!!!   ASH
PrtARForm
.//print horiz line top of page     .note pattern 2G-horiz line 1G  vert line
.//                         pattern   height in dots (length)   terminate in A for horiz. B for vert
.//                         /           /                  vert width in dots      *cxA would be hor width
.//                        /           /                  /         vert position in dots
.//                       /           /                  /         /
.//                 033,"*c2G",033,"*c5825.2000A",033,"*c3B",033,"*p63.40y2.50X":
.//                 033,"*c0P";                                            \
.                           \                                               Horiz pos in dots
.                            start printing in black
.
         print    *F,033,"&l1E",033,"&a0c0R":          .top of form, set top margin 0 lines, position to 0.0
                   033,"*p880x75Y":
                   033,"(8U",033,"(s1p18.00v0s+3b5T","Names":
                   b2,033,"(8U",033,"(s1p18.00v1s-3b5T","in the News":
                   033,"*p760x95Y",033,"*c900a02b0p":
                   033,"*p880x135Y":
               033,"(8U",033,"(s1p09.00v0s-2b5T","C  A  L  I  F  O  R  N  I  A        I  N  C .":
               033,"*p1040.0x264Y":
               033,"(8U",033,"(s1p07.00v0s-2b5T","180 Grand Ave., Suite 1365":
               033,"*p1040x301.5Y":
               033,"(8U",033,"(s1p07.00v0s-2b5T","Oakland, CA 94612-3716":
               033,"*p980x339.0Y":
               " 415-989-3350 ",bullet," Fax 415-433-7796":
.               033,"*p1085.0x189Y":
.               033,"(8U",033,"(s1p06.00v0s-2b5T","1300 Clay St., 11th Floor":
.               033,"*p1060x226.5Y":
.               033,"(8U",033,"(s1p06.00v0s-2b5T","Oakland, CA 94612-1429":
.               033,"*p1030x264.0Y":
.               " 415-989-3350 ",bullet," Fax 415-433-7796":
.               033,"*c2G",033,"*c825.0A",033,"*c3B",033,"*p825.0x639.0y",033,"*c0P":            .box top
.               033,"*c2G",033,"*c825.0A",033,"*c3B",033,"*p825.0x714Y",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p825.0x639.0Y",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1650.0x639.0Y",033,"*c0P":            .box line right
               033,"*c2G",033,"*c600.0A",033,"*c3B",033,"*p918.75x760.25y",033,"*c0P":            .box top
               033,"*c2G",033,"*c600.0A",033,"*c3B",033,"*p918.75x835.25Y",033,"*c0P":            .box bottom
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p918.75x760.25Y",033,"*c0P":            .box line left
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1518.75.x760.25Y",033,"*c0P":            .box line right
               033,"*p939x812.5Y":
               033,"(8U",033,"(s1p12.00v1s+3b5T","Statement of Payment Due":
               033,"*c2G",033,"*c1100.0A",033,"*c3B",033,"*p2.5x901.5Y",033,"*c0P":            .horizontal lines header top
               033,"*c2G",033,"*c1350.0A",033,"*c3B",033,"*p1100.5x901.5Y",033,"*c0P":         .horizontal lines header top
               033,"*c2G",033,"*c1156.75A",033,"*c3B",033,"*p2.5x957.75Y",033,"*c0P":            .horizontal lines header bottm
.               033,"*c1G",033,"*c2136.5B",033,"*c3A",033,"*p2.5x901.5Y",033,"*c0P":            . 1st vert line left
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p2.5x901.5Y",033,"*c0P":            . 1st vert line left
               033,"*c1G",033,"*c2099.0B",033,"*c3A",033,"*p237.5x957.75Y",033,"*c0P":            . 2nd vert line
               033,"*c1G",033,"*c2099.0B",033,"*c3A",033,"*p482.5x957.75Y",033,"*c0P":            . 3rd vert line
               033,"*c1G",033,"*c2099.0B",033,"*c3A",033,"*p733.75x957.75Y",033,"*c0P":            . 4th vert line
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p1156.75x901.5Y",033,"*c0P":            . 5th vert line
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p1456.75x901.5Y",033,"*c0P":            . 6th vert line
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p1756.75x901.5Y",033,"*c0P":            . 7th vert line
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p2056.75x901.5Y",033,"*c0P":            . 8th vert line
               033,"*c1G",033,"*c2155.25B",033,"*c3A",033,"*p3150.5x901.5Y",033,"*c0P":            . last vert line end header bar
               033,"*c2G",033,"*c210.0A",033,"*c3B",033,"*p2.5x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 1
               033,"*c2G",033,"*c220.0A",033,"*c3B",033,"*p237.5x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 2
               033,"*c2G",033,"*c220.0A",033,"*c3B",033,"*p482.5x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 3
               033,"*c2G",033,"*c395.0A",033,"*c3B",033,"*p733.75x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 4
               033,"*c2G",033,"*c280.0A",033,"*c3B",033,"*p1156.75x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 5
               033,"*c2G",033,"*c280.0A",033,"*c3B",033,"*p1456.75x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 6
               033,"*c2G",033,"*c280.0A",033,"*c3B",033,"*p1756.75x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 7
               033,"*c2G",033,"*c450.0A",033,"*c3B",033,"*p2056.75x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 8
               033,"*c2G",033,"*c375.0A",033,"*c3B",033,"*p3150.5x3056.75Y",033,"*c0P":            .horizontal lines bottom of form col 8
.               033,"*p295x815.0Y":                                                          .position for header lettering
               033,"*p295x946.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+1b5T","Billing Description":                              . bold & italicised
.               033,"*p1135x825.0Y":                                                          .position for header lettering
               033,"*p1210x946.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+1b5T","0-30 days":                              . bold & italicised
               033,"*p1493x946.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+1b5T","60 days":                              . bold & italicised
               033,"*p1820x946.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+3b5T","90 days":                              . bold & italicised
               033,"*p2142x946.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+3b5T","Over 90":                              . bold & italicised
               033,"*p27.5x1006.25Y":                                                          .position for header lettering
               033,"(8U",033,"(s1p08.00v1s+1b5T","Invoice No.":                              . bold & italicised
               033,"*p277.0x1006.25Y":
               033,"(8U",033,"(s1p08.00v1s+1b5T","Maildate":                              . bold & italicised
               033,"*p500.0x1006.25Y":
               033,"(8U",033,"(s1p08.00v1s+1b5T","Our LR No.":                              . bold & italicised
               033,"*p785.5x1006.25Y":
               033,"(8U",033,"(s1p08.00v1s+1b5T","Your Order No.":                              . bold & italicised
               033,"*c2G",033,"*c900.0A",033,"*c3B",033,"*p237.5x3070.0Y",033,"*c0P":            .Total box top
               033,"*c2G",033,"*c900.0A",033,"*c3B",033,"*p237.5x3145.0Y",033,"*c0P":            .Total  bottom
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p237.5x3070.0Y",033,"*c0P":            .Total line left
               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1137.5.x3070.0Y",033,"*c0P":            .Total  line right
               033,"*p237.5x3070Y":  position cursor
               033,"*c900A":      width
               033,"*c75B":       height
               033,"*c2G":            area fill%
               033,"*c2P":             print it  pattern shaded fill
               033,"*p250x3118.75Y":
               033,"(8U",033,"(s1p12.00v1s+3b5T","Total Due:":
               033,"*p1180.0x500.0Y":
               033,"&l0E",033,"*p0.0x0.0Y",033,"&l0E",033,"&a0c0R"                                .reset position (nec?)
               return
.BEGIN PATCH 3.7
TestFOrm
         move      stelr to nordfld
         rep        zfill in nordfld
         call      nordkey
.Patch 3.76 Patch Commented back in 
               PRTPAGE        Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0:
                              *NewPage;
.Patch 3.76 Commented back in 3.76
.                              *p=3250:125,*font=Font018b,"Names  ":             
.                              *font=Font018I,"in the News":
.                              *PENSIZE=10,*p=2652:400,*Line=5652:400:
.                              *p=3250:450,*font=Font010,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
.                              *p=3207:693,*font=Font07,"1300 Clay Street, 11th Floor, Oakland, CA 94612-1429":
.               PRTPAGE        Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
.                              *p=3489:793,"415-989-3350 ","·"," Fax 415-433-7796":
.BEGIN PATCH 3.75
        prtpage Laser;*UNITS=*HIENGLISH;
.begin patch 3.83
          IF        (STEcompID = "P")
          prtpage   Laser;*p=1:25,*font=font018b,"Pacific Lists, Inc.":
                    *p=451:343,*font=font07,"180 Grand Ave. Suite 1365":
                    *p=451:443,"Oakland, CA 94612-3716":
                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
                    *p=317:643,"A Division of Names in the News"
          Else        
          prtpage   Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          Endif
.end patch 3.83
.end PATCH 3.75
               PRTPAGE        Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *RECT=2500:2750:3000:5075:                                ;top:bot:left:right
                              *p=3075:2510,*font=Font012BI,"Statement of Payment Due":
                              *p=1:3000,*font=Font08,*line=8000:3000:                      ;top Hori line
                              *p=1500:3010,*font=Font09BI,"Billing Description":
                              *p=4125:3010,*font=Font09BI,"0-30 Days":
                              *p=5125:3010,*font=Font09BI,"60 Days":
                              *p=6125:3010,*font=Font09BI,"90 Days":
                              *p=7125:3010,*font=Font09BI,"Over 90":
                              *p=1:3225,*font=Font08,*line=3875:3225:                      ;2nd Hori line
                              *p=125:3235,*font=Font09BI,"Invoice":
                              *p=970:3235,*font=Font09BI,"Maildate":
                              *p=1600:3235,*font=Font09BI,"NIN LR":
                              *p=2350:3235,*font=Font09BI,"Your Order No.":
                              *p=1:3000,*Line=1:10250:                                      ;Left most vert line
                              *p=725:3225,*Line=725:10250:                                  ;2nd Left most vert line
                              *p=1475:3225,*Line=1475:10250:                                  ;3rd Left most vert line
                              *p=2225:3225,*Line=2225:10250:                                  ;4th Left most vert line
                              *p=3875:3000,*Line=3875:10250:                                  ;5th Left most vert line
                              *p=4875:3000,*Line=4875:10250:                                  ;6th Left most vert line
                              *p=5875:3000,*Line=5875:10250:                                  ;7th Left most vert line
                              *p=6875:3000,*Line=6875:10250:                                  ;8th Left most vert line
                              *p=1:10250,*font=Font08,*line=650:10250:                      ;Left most bottom Hori line
                              *p=725:10250,*font=Font08,*line=1400:10250:                      ;2nd bottom Hori line
                              *p=1475:10250,*font=Font08,*line=2150:10250:                      ;3rd  bottom Hori line
                              *p=2225:10250,*font=Font08,*line=3800:10250:                      ;4rd  bottom Hori line
                              *p=3875:10250,*font=Font08,*line=4800:10250:                      ;4rd  bottom Hori line
                              *p=4875:10250,*font=Font08,*line=5800:10250:                      ;4rd  bottom Hori line
                              *p=5875:10250,*font=Font08,*line=6800:10250:                      ;4rd  bottom Hori line
                              *p=6875:10250,*font=Font08,*line=8000:10250                      ;4rd  bottom Hori line
               Return
.end patch 3.7
................................................................................................
.end patch 3.5
.START PATCH 3.77 ADDED LOGIC
PageUpdate
.Update previous record!!
          call      Trim using HoldMlrKey
          if (HoldMlrKey <> "")
                    pack      NPGEFLD,HoldMlrKey
.Re-pack immediately!!!
                    move      INDMNUM,HoldMlrKey
.
                    move      HoldBrkKey,str35
                    call      Trim using str35
                    move      INDNAME,HoldBrkKey
.
                    move      "NPGEKEY",Location
                    pack      KeyLocation,"Key: ",NPGEFLD
                    CALL      NPGEKEY
                    loop
                              until over
                              until (INDMNUM <> NPGEFLD)
                              call      Trim using INDNAME
                              if (str35 = INDNAME)
                                        move      PAGE,N4
                                        MOVE      N4 TO INDPAGE2
                                        move      "NPGEUPD",Location
                                        pack      KeyLocation,"Key: ",NPGEFLD
                                        CALL      NPGEUPD
                                        break
                              endif
                              move      "NPGEKS",Location
                              pack      KeyLocation,"Key: ",NPGEFLD
                              CALL      NPGEKS
                    repeat
          else
                    move      INDMNUM,HoldMlrKey
                    move      INDNAME,HoldBrkKey
          endif
          return
.END PATCH 3.77 ADDED LOGIC
.Begin patch 3.92
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"statement PDF - ",str25
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    prtname,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 3.92
+...........................................................................
.
         INCLUDE   GTBLIO.inc    TABLE
+
         INCLUDE   NSTEIO.inc    STATEMENT FILE
+
.patch3.72
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc    MAILER
.patch3.72
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NPGEIO.inc
+
.patch3.72
.         include   nbrkio.inc
.patch3.72
.Begin patch 3.6
               Include        DtpIO.inc
.End patch 3.6

.begin patch 3.8
.         include   ninvio.inc
          include             ninvio.inc
.end patch 3.8
         include   nordio.inc
         INCLUDE   NJSTIO.inc
         include   nmobio.inc
         include   nownio.inc
         INCLUDE   COMLOGIC.inc

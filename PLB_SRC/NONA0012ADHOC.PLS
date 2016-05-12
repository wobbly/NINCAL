...............................................................................
PC        EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NMOADD.inc
          INCLUDE   NMOBDD.inc
          include   compdd.inc
          include   cntdd.inc
          INCLUDE   NPGEDD.inc
          include   hp.inc
          INCLUDE   NDATDD.inc
          include   gnxtdd.inc
           Include        MOANotesDD.inc
          include nescdd.inc
        include winapi.inc    
.
release  init      "1.00"           DLH  .for eom recon
reldate    Init       "2014 November 12"
.find active accounts  for EOM
str10a     dim        10
str10b     dim        10
ONE      FORM      "1"
TWO      FORM      "2"
ZERO     FORM      "0"
ANS      DIM       1
BR       FORM      2
DD1      DIM       2
HOLDMM   DIM       2           USED TO CHECK FOR CURRENT MONTH TRANSACTION.
HOLDYY   DIM       2           USED TO CHECK FOR CURRENT YEAR TRANSACTION.
V2       FORM      2           VERTICAL DISPLAY VARIABLE
V3       FORM      2           VERTICAL DISPLAY VARIABLE
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
balrec   dim       1
SELECT   FORM      2           BRANCH FOR REASON
SAMEMLR  DIM       4           USED TO VERIFY STILL READING CORRECT ACCOUNT
samebrk  dim       4           broker break
DATE     DIM       8
PAGE     FORM      4           PAGE NUMBER
LINES    FORM      2           LINE NUMBER
MONTH    DIM       10
YEAR     DIM       4
NINETEEN INIT      "19"
JAN      INIT      "JANUARY"
FEB      INIT      "FEBRUARY"
MAR      INIT      "MARCH"
APR      INIT      "APRIL"
MAY      INIT      "MAY"
JUN      INIT      "JUNE"
JUL      INIT      "JULY"
AUG      INIT      "AUGUST"
SEP      INIT      "SEPTEMBER"
OCT      INIT      "OCTOBER"
NOV      INIT      "NOVEMBER"
DEC      INIT      "DECEMBER"
DOLLAR   INIT      "$$,$$$,$$$.99"
DOLLAR1  INIT      "$$$,$$$,$$$.99-"
AMNTAPP  DIM       13
AMNTRECD DIM       13
SUBAPP   FORM      8.2
SUBRECD  FORM      8.2
SUBAPPb  FORM      8.2
SUBRECDb FORM      8.2
MTDAPP   FORM      8.2
MTDRECD  FORM      8.2
MTDBAPP   FORM      8.2
MTDBRECD  FORM      8.2
MTDCHNG  FORM      9.2
MTDTAPP  FORM      8.2      M-T-D TOTAL APP
MTDTRECD FORM      8.2      M-T-D TOTAL REC'D
MTDTCHNG FORM      9.2      M-T-D TOTAL CHANGE
MTDTNINAPP  FORM      8.2      M-T-D TOTAL APP
MTDTNINRECD FORM      8.2      M-T-D TOTAL REC'D
MTDTPLIAPP  FORM      8.2      M-T-D TOTAL APP
MTDTPLIRECD FORM      8.2      M-T-D TOTAL REC'D
TOTAPP   FORM      9.2
TOTRECD  FORM      9.2
TOTAMASK DIM       14
TOTRMASK DIM       14
SUBAMASK DIM       13
SUBRMASK DIM       13
MTDRMASK DIM       13
MTDAMASK DIM       13
totlMASK DIM       15
MTDTMASK DIM       15
TRANDTE   DIM      10
trancnt  form      5
tranNINcnt  form      5
tranPLIcnt  form      5
DINVDTE  DIM       10
FIRSTPAS INIT      "Y"
TOTALB   FORM      9.2
TOTAL    FORM      9.2
XFOOT    FORM      9.2       USED TO VERF. DETAIL ENTRIES TO BALANCE
DETAIL$  FORM      8.2
TOTDOLL  INIT      "$$$,$$$,$$$.99"
TOTDOLL1 INIT      "$$$,$$$,$$$.99-"
COUNT    FORM      5
LBRACKET DIM       1      USED FOR '<'
RBRACKET DIM       1      USED FOR '>'
BEGNUM   FORM      7      *HOLD BEGINNING TRANSACTION NUMBER.
ENDNUM   FORM      7
HoldMlrKey          dim       6
HoldBrkKey          dim       25
MNUM2     DIM       6         .Temporary var!!
PRFILE    pfile
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
OldFlag   Init      "N"                 .default is don't print old details
DateChk   DIm       8
clnmoa     File
fakefile  file
Output     File
Output1    File
Input     Ifile     Keylen=14
SysDay    Dim       2
SysMO     Dim       2
Sysyr     Dim       2
acctOut   Form      6                  .accounts written out
DetOut    FOrm      6                   .detail recors out
.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
          move    "4250" to Header1
          move    "1400" to Title1      
          move    "5450" to Title2
          move    "4400" to Title3      
        move    "200",column
        move    "900",column1
        move    "1600",column2
        move    "2300",column3
        move    "2500",column4
        move    "3600",column5
        move    "4400",column6
        move    "5400",column7
        move    "6250",column8
        move    "6800",column9
        move    "7300",column10
        move    "3000",column4R
        move    "4400",column5R
        move    "5100",column6R
        move    "5800",column7R
        move    "7200",column8R
        move    "7200",column9R
        move    "7900",column10R
        
COMPCOMPKEY external "COMP001A;COMPCOMPKEY"
holda     dim       500       .length of Company record - used by independent routines to hold Company record
NOESCFLAG FORM      1
BrokeFlag FORM    1
.
+..............................................................................
          MOVE      "Names in the News" TO COMPNME
          MOVE      "PULL MOA DETAILS" TO STITLE
          MATCH     "NONA0012ADHOC" TO INPNAME         .ENTRY FROM DSINIT?
          IF NOT EQUAL                     . NO.
                    CLOCK     DATE TO DATE
                    CLOCK     DATE TO today
             shutdown
             stop
          endif
          Unpack    Today into Sysmo,str1,Sysday,str1,Sysyr
          trap      done if f5
          move      "ABORT" to pf5
          CALL      PAINT
          call      funcdisp
          move      "07" to v
          Clock     Timestamp,Datechk
          MOVE      C2 TO NMObPATH
          MOVE      C4 TO NMOAPATH
         clear     str45
         append    "\\nins1\d\accounting\gl\clnmoa" to str45
         append    sysyr,str45
         append    sysmo,str45
         append    ".csv" to str45
         reset      str45
         PREPARE   clnmoa,str45
         write     clnmoa,seq;*cdfon,"Mailer ##","CNT ##","Entry ##","Billto##","Transaction Date","Control##","Inv ##","LR##","INV Date","AMT Out":
                   "Date Entered","AMT IN","Reason Code","Reason","LIST ##","CHECK ##","Transaction ##","BRK ##","Typist"


Prep      DISPLAY   *P10:11,"Sorting 'new' records: "
          pack      str4 from Sysyr,sysmo
          rep       zfill in str4
          Pack      Taskname from "\\nins1\e\data\text\Ninmoa.dat,\\nins1\e\data\Ninmoa.tmp;1-4,S=#"56='",str4,"'#""
          Sort      Taskname,SunDM="nins1:502"
          open      FakeFIle,"\\nins1\e\data\ninmoa.tmp|nins1:502"
            Loop      
            read      fakefile,seq;moavars  
           until      over
           unpack     trandate into cc,yy,mm,dd
           pack       str10 from mm,"/",dd,"/",cc,yy              .trandate
           unpack     invdate into cc,yy,mm,dd
           pack       str10a from mm,"/",dd,"/",cc,yy              .inv date
           unpack     recdate into cc,yy,mm,dd
           pack       str10b from mm,"/",dd,"/",cc,yy              .recorded date
          CLEAR     RDESC
           if         (reason = "99")
           MOVE      "ENTRY CORRECTION" TO RDESC
           else
         LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22,REAS23
           endif      
          WRITE       clnmoa,SEQ;*cdfon,MLR:
                                 MCNT:
                                 ENTRY:
                                 MBILLTO:
                                 str10:                           .trandate
                                 CONTROL:
                                 INVOICE:
                                 LRNUM:
                                 str10a:                          .inv date
                                 ONAMOUNT:                .AMOUNT OUT
                                 str10b:                          .date recorded
                                 INAMOUNT:             .AMOUNT IN
                                 Reason:
                                 RDesc:
                                 LIST: 
                                 CHECKNUM:
                                 TRANSNUM:
                                 NMOABRK: 
                                 NMOAINIT 

          repeat
         WEOF      clnMoa,SEQ
         CLOSE     clnMoa,EOFSIZE

Done      shutdown
          stop
          include   compio.inc
          include   cntio.inc
           Include        MOANotesIO.inc
          INCLUDE   NMOAIO.inc
          INCLUDE   NMOBIO.inc
          INCLUDE   NDATIO.inc
          INCLUDE   NPGEIO.inc
          include nescio.inc
          include   gnxtio.inc
          INCLUDE   COMLOGIC.inc
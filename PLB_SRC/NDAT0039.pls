PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
         include  nowndd.inc

RELEASE  INIT      "1.1"      ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "R1.0"      DLH 21Feb97
....................................................................
inPUT    FILE      
OUTPUT   FILE      
OUTPUT2  FILE      
OUTPUT3  FILE      
....................................................................
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
. 
...............................................................................
.misc VARIABLES
akey2    init       "02R"
sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
DATMO    DIM       2           .used to 
DATDAY   DIM       2           .hold the
DATYR    DIM       2           .datacard revised date.
ORDDATE  FORM      5           .order date in julian
CARDATE  FORM      5           .datacard date in julian
date     DIM       8
today1   FORM      5 
date1    dim       8
CHECK    FORM      5
check2   form      5
BLNK3    DIM       3
COUNT    FORM      5
count1   form      5
count2   form      5
USED     FORM      5
NOTUSED  FORM      5
OPTFLAG  FORM      1
....................................................................
         MOVE      "Ndat0039" TO PROGRAM
         MOVE      "Find Used Exclusive DATACARDS" TO STITLE
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         clock     date to today
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
. 
         TRAP      ABORT IF F5
. 
. 
....................................................................
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO SYSMO,str1,SYSDAY,str1,SYSYR
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      "01" TO MM
         MOVE      "01" TO DD
         MOVE      "96" TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
. 
....................................................................
....................................................................
BEGIN
.START PATCH 1.1 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\DataBLO"
.         open      input,"g:\data\nindat.me",exclusive
         PACK      STR35,NTWKPATH1,"DATABLO"
         PACK      STR45,NTWKPATH1,"NINDAT.ME"
         PREPARE   OUTPUT,STR35
         open      input,STR45,exclusive
.END PATCH 1.1 REPLACED LOGIC
.         call       ndattst
         GOTO      A100
         MOVE      C1 TO NDATFLAG
         GOTO      A100
....................................................................
. 
. GET NEXT DATA CARD
. 
A100
         read      input,seq;datvars
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
         add       c1 to count
        display   *p10:13,"records read ",count
         cmatch    "C" to elstcde        .exclusive ?
         goto      a100 if not equal         .no, skip
. 
CHKRUN   RESET     RUNCODES
         SCAN      lstnum IN RUNCODES         .running charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
         CMATCH    "W"  TO status        .withdrawn ?
         GOTO      A100 IF EQUAL         .yes skip
         reset     mlstname
.

ORDINFO  REP       zfill IN LSTNUM
         PACK      NORDFLD2,AKEY2,LSTNUM
         CLEAR     NORDFLD1
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
. 
         display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
         CALL      NORDAIM
         GOTO      CARDNG IF OVER
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         Compare   check to CHECK2           .usage in time period?
         goto      c100 if less              .yes
         GOTO      B100         .no DEAL, TRY AGAIN.

CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
         ADD       c1 TO NOTUSED
         DISPLAY   *P12:20,"NUMBER OF LISTS WITH NO USAGE : ",NOTUSED
         GOTO      b101
. 
. COMPUTE ORDER USAGE
. 
B100
        display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
         CALL      NORDKG
         GOTO      a100 IF OVER
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         Compare   check to CHECK2           .usage in time period?
         goto      c100 if less              .yes
         GOTO      B100         .no  DEAL, TRY AGAIN.
B101   
. 
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
.  
C100
         write     output,seq;*+,datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
D100
         GOTO      A100
. 
. 
. ABORT - OPERATOR ABORTED JOB. RESULTS NOT VALID
. 
ABORT
         DISPLAY   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
. 
. CLOSE FILE AND EXIT
. 
Z900
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
. 
        shutdown   "cls"
        STOP
. 
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         include   nownio.inc
         INCLUDE   COMLOGIC.inc


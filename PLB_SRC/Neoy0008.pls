PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
OUTPUT   FILE      
release  init      "2.4"       DLH USE data manager for output
reldate	init	"2014 October 21"
.release  init      "2.3"       DMB 18JUN2005 Changed IP address of File Manager
.release  init      "2.2"       ASH 19MAR01      GOTO FILE MANAGER FOR NINORD
.release  init      "2.1"       ASH 28DEC98    NINORD Y2K, file expansion
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.
AKEY1    INIT      "01R"
AKEY2    INIT      "02R"
ALKEY    DIM       9           LISTKEY
AMKEY    DIM       7
NAMES    FORM      10
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
ZERO     FORM      "0"
ONE      FORM      "1"
EIGHT    FORM      "8"
.Start Patch #2.1 - increased var for OQTY increase
.OQTY1    FORM      7
OQTY1    FORM      9
.End Patch #2.1 - increased var for OQTY increase
ORDDATE  FORM       5
date     DIM       8
today1   FORM      5 
date1    dim       8
CHECK    FORM      5
check2   form      5
sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
. 
.OUTPUT FILE.
..............
.STATUS   DIM       1           1-1
.LSTNUM   DIM       6           2-7
.BLNK3    DIM       3           8-10
.MLSTNAME DIM       55         11-65
.QTY      FORM      5          66-70
.NAMES    FORM      10         71-80
...............................................................................
.PRINT VARIABLES
PAGE     FORM      3
LINES    FORM      2
SIXTY2   FORM      "62"
ANS      DIM       1
EIGHTY8  INIT      "88"
EIGHTY9  INIT      "89"
NINETY   INIT      "90"
NINETY1  INIT      "91"
NINETY2  INIT      "92"
NINETY3  INIT      "93"
MOMATCH  FORM      "11"
ORDMO    FORM      2
PRNTBR   FORM      1
SIX      FORM      "6"
BLNK3    DIM       3
COUNT    FORM      5
USED     FORM      5
NOTUSED  FORM      5
         MOVE      "NEOY0008" TO PROGRAM
         MOVE      "CARDS USED last 2 years twice or more" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
. 
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO SYSMO,str1,SYSDAY,str1,SYSYR
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      SYSMO TO MM
         MOVE      SYSDAY TO DD
         MOVE      SYSYR TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
. OPEN INPUT FILES
OPEN     TRAP      ABORT IF F5
. 
. 
. PREP OUTPUT FILE
. 
.begin patch 2.4
.         IFZ       PC
.         PREPARE   OUTPUT,"c:\work\CARDANY.CAL",exclusive
         PREPARE   OUTPUT,"\\nins1\e\data\CARDANY.CAL|nins1:502",exclusive
.         XIF
.end patch 2.4
         move      c3 to ndatlock
         move      c3 to nordlock
         trap      nodatfile if io
         open      ndatfile,"nindat",read
         move      c1 to ndatflag            .got it
nodatfile 
         trapclr   io
         trap      noordfile if io
.>Patch 2.3 Begin
.START PATCH 2.2 REPLACED LOGIC
.         open      nordfle2,"ninord",read
.         open      nordfle2,"c:\work\dbaca\ninorddb.AAM",read
.         open      nordfle2,"ninord.AAM|20.20.30.103:502",read
         open      nordfle2,"ninord.AAM|NINS1:502",read
.>Patch 2.3 End         
.END PATCH 2.2 REPLACED LOGIC
         move      c1 to nordflg2            .got it
noordfile
         move      "000000" to ndatfld
         call      ndattst
. 
. GET NEXT DATA CARD
. 
A100
         CALL      NDATKS
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
. 
         CMATCH     b1  TO status
         GOTO      A100 IF NOT EQUAL

         REP       zfill IN LSTNUM
         PACK      NORDFLD2,AKEY2,LSTNUM
         CLEAR     NORDFLD1
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
. 
         CALL      NORDAIM
         GOTO      CARDNG IF OVER
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
         MOVE      OODTEM TO ORDMO
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "730" to check2
         IF        LESS
         GOTO      B101             .ITs GOOD
         ELSE
         GOTO      B100A            .DATE OUT OF RANGE
         ENDIF
CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
         ADD       ONE TO NOTUSED
         DISPLAY   *P12:20,"NUMBER OF LISTS WITH NO USAGE : ",NOTUSED
.         GOTO      C100
         GOTO      A100
. 
. COMPUTE ORDER USAGE
. 
B100
.         MOVE      "Z,ZZZ,ZZZ,ZZ9",MASK
.         EDIT      NAMES,MASK
.  
.         DISPLAY   *P1:9,"ORDERS = ",QTY:
.                   *P1:10," NAMES = ",MASK
. 
B100A    CALL      NORDKG
         GOTO      C100 IF OVER
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
. 
         MATCH     LSTNUM,OLNUM
         GOTO      C100 IF NOT EQUAL
.
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "730" to check2
         IF        LESS
         GOTO      B101             .ITs GOOD
         ELSE
         GOTO      B100A            .DATE OUT OF RANGE
         ENDIF
B101     MOVE      OQTY TO OQTY1
         ADD       OQTY1,NAMES
         ADD       C1,QTY
         COMPARE   C2 TO QTY
         GOTO      C100 IF not less
         GOTO      B100A
. 
.         GOTO      B100
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
.  
C100
         COMPARE   C1,QTY
         GOTO      C101 IF EQUAL
         COMPARE   ZERO TO NAMES
         GOTO      C101 IF EQUAL
. 
         move      lstnum to ndatfld
         call      ndatkey
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal

         write     output,seq;datvars 
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
. 
C101     MOVE      ZERO,NAMES
         MOVE      ZERO,QTY
. 
. GO DO IT AGAIN
. 
D100
         GOTO      A100
. 
.list name begins with 'A '
FixitA
        bump      Mlstname by 2
        clear     str55
        append    mlstname to str55
        append    "|A" to str55
        reset     str55
        clear     Mlstname
        move      str55 to mlstname
        return
.
.list name begins with 'The '
FixitThe
        bump      Mlstname by 4
        clear     str55
        append    mlstname to str55
        append    "|The" to str55
        reset     str55
        clear     Mlstname
        move      str55 to mlstname
        return

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
         shutdown
.        STOP
. 
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


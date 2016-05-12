. .............................................................................
.
.nsch0001/TXT -- MAIL DATE SCHEDULING PROGRAM
.
. .............................................................................
PC       EQU       0

         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
         INCLUDE   NPASDD.inc
         INCLUDE   NDATEDD.inc
         INCLUDE   NXRFDD.inc
         INCLUDE   NSCHDD.inc
.Patch2.9
          include   compdd.inc
          include   cntdd.inc
.          INCLUDE   NMLRDD.INC
.Patch2.9
           INCLUDE   NMDLDD.inc
.         INC       SCHEDULE.inc
..............................................................................
RELEASE  INIT     "2.9"       DMB       26MAY2004 Mailer Conversion
.RELEASE  INIT     "2.86"     ASH 29JAN2004 DATACARD CONVERSION
.RELEASE  INIT     "2.85"       ;jd 12aug95 restoring list# on add but no add.
.RELEASE  INIT     "2.80"      ;jd 13sep95 totals by week.
.RELEASE  INIT     "2.73"      ;jd 15may95 move schkey to nmdlkey for write/upd
.RELEASE  INIT     "2.72"     DLH 23Feb95 new NMDLDD vars
.RELEASE  INIT     "2.71"     DLH 06JUL94 EDIT/RV ON BOOKING INST.
.RELEASE  INIT     "2.7"      DLH 09JUN93 ADD TwO MORE INSTRUCTION LINES.
.                                  and numerous minor friendly enhancements.
.
.RELEASE  INIT     "2.6"      DLH 20APR 93 ALLOW USE OF MAILER NUMBER IF NOT LIST
.                                  EXISTS.
.RELEASE  INIT        "2.5"      DLH 05APR93  PROGRAM FLOW, RETAIN "Y" AS DEFAULT
.                                  ON OK?.
.
.RELEASE   INIT     "2.4"      DLH 01MAR93    EDIT ON INSTRUCTION LINES. 
.RELEASE  INIT     "2.3"      DLH 08FEB93   PCBUS REQUIRED KEYS TO BE LEFT
.                                  JUSTIFIED INSTEAD OF RIGHT TO WORK PROPERLY.
.
.RELEASE  INIT         "2.2"     DLH 26JUN92   NSCHXX & NMDLXX INCLUDES.
.
.RELEASE  INIT      "2.1"      DLH 11MAR92
.                            UPDATED NXRFXX TO INCLUDE ACCESS BY MLR KEY.
.                            ADDED LISTHELP.inc.
.
.RELEASE  INIT      "2.0"              DLH 26FEB92.
.                           updated ndatxx,npasxx includes.
.                           added nxrfxx, cons, comlogic includes.
.                           updated ndatedd & ndateio to use cons vars.
.
. MODIFIED MARCH 3 1986 - TWO NEW CODES IN SCHEDULE INSTRUCTION FILE:
.                         1 - SCHEDULE CODE;  ' ' = CAN BE SCHEDULED
.                                             'N' = DO NOT SCHEDULE.
.                         2 - LCR CODE;       ' ' = OK TO LCR
.                                             'N' = DO NOT ALLOW LCR'S
.                       CAN BE BYPASSED WITH PASSWORD IN LCR PROGRAM: NINCLEAR.
.
.
. CREATED JULY 1983 NINCAL. - DLH
.
...............................................................................
+ .............................................................................
.
. PROGRAM VARIABLES
. .............................................................................
.
.
.CENTURY  DIM       2       ASSUMED TO BE "19" IF NOT SPECIFIED.
HOLDLIST    DIM       6       LIST NUMBER
.
. KEY FIELDS FOR NSCHFILE
. ...................
.
K1       INIT      "01L"
K2       INIT      "02L"
K3       INIT      "03L"
QUES     INIT      "????"
.
.
C        INIT      "G"     PROGRAM CODE   - FOR NPASKEY BUILD.
Q        INIT      "Q"     PROGRAM CODE LIST INST  - FOR NPASKEY BUILD.
.
..ADDITIONAL  LISTHELP VARS
WSW     DIM   1              WITHDRAWN SWITCH
KEYCOUNT FORM      1
AKEY1    DIM       3
. KEYIN AND DISPLAY VARIABLES.;
. ...........................
.
TAB127   FORM      "127"
TAB457   FORM      "457"
NUM      FORM      1       BRANCHING CONSTANT.
ITEM     FORM      2
SELECT   FORM      1       BRANCHING CONSTANT (1=ADD MODE),(2=UPDATE MODE).
DATE2    DIM       8       FORMATTED DATE MM/DD/YY.
HOLDLINE DIM       70      SAVE LIST INSTRUCTIONS DURING MODIFY.
MON      FORM      1       BRANCHING CONSTANT
MON1     FORM      2       DATE 1ST MONDAY OF THE MONTH
MON2     FORM      2       DATE 2ND MONDAY OF THE MONTH
MON3     FORM      2       DATE 3RD MONDAY OF THE MONTH
MON4     FORM      2       DATE 4TH MONDAY OF THE MONTH
MON5     FORM      2       DATE 5ND MONDAY OF THE MONTH
NWORK1   FORM      5       NUMERIC WORK VARIABLE.
NWORK2   FORM      5       NUMERIC WORK VARIABLE.
NWORK3   FORM      5       NUMERIC WORK VARIABLE.
.START PATCH 2.86 REPLACED LOGIC
.NWORK9   FORM      9       NUMERIC WORK FIELD.
.OVERBOOK FORM      9       HOLDS TOTAL BOOKING FOR LIST/WEEK
.TOTBOOK  FORM      9       HOLDS TOTAL BOOKING FOR LIST/MONTH
NWORK9   FORM      10       NUMERIC WORK FIELD.
OVERBOOK FORM      10       HOLDS TOTAL BOOKING FOR LIST/WEEK
TOTBOOK  FORM      10       HOLDS TOTAL BOOKING FOR LIST/MONTH
.END PATCH 2.86 REPLACED LOGIC
JDAYWORK FORM      3
DAYWORK  FORM      2
MONWORK  FORM      2
YEARWORK FORM      2
sysmm    form      2
.SIX      FORM      "6"     VALUE OF SIX
.SEVEN    FORM      "7"     VALUE OF SEVEN
.EIGHT    FORM      "8"
.NINE     FORM      "9"     VALUE OF NINE
EIGHTY   FORM      "80"    VALUE OF EIGHTY
DAYFEB   FORM      "28"    DAYS IN FEBUARY
DAY30    FORM      "30"
DAY31    FORM      "31"
BAR      INIT      006
SDESC    DIM       7       SAVE MAILER DESCRIPTION DURING MODIFY.
SQUANT   DIM       7       SAVE QUANTITY DURING MODIFY
ENTRYOK  DIM       1       HOLDS 'N' IF ENTRY WOULD BE UNATHORIZED OVERBOOK.
PLIST1   DIM       6       PICKOFF LIST NUMBER
NEWNAME  DIM       8       USED TO BUILD FILE FOR PICK OFF OUTPUT.
FILENUM  FORM      2       USED TO BUILD FILE FOR PICK OFF OUTPUT.
F2       DIM       2
RECNAME  DIM       17      USED TO BUILD FILE FOR PICK OFF OUTPUT.
EXT      INIT      "/TEXT:PRINT"
ADDSW    DIM       1       HOLDS '*' IF BOOKING INST. NOT ON FILE/NO BOOKING.
ANS      DIM       1
.
. TABLE VARIABLES FOR SCREEN POSITIONING.
. .......................................
.
. ACTUAL VERTICAL STARTING POSITION IS "07"
V1       FORM      "09"
V2       FORM      "09"
V3       FORM      "09"
V4       FORM      "09"
V5       FORM      "09"
.
H1       FORM      "02"
H2       FORM      "17"
H3       FORM      "33"
H4       FORM      "49"
H5       FORM      "65"
.
.
.
. TABLE VARIABLES FOR OVERBOOKING TOTALS
. ......................................
.
.START PATCH 2.86 REPLACED LOGIC
.TOT1     FORM      9
.TOT2     FORM      9
.TOT3     FORM      9
.TOT4     FORM      9
.TOT5     FORM      9
TOT1     FORM      10
TOT2     FORM      10
TOT3     FORM      10
TOT4     FORM      10
TOT5     FORM      10
.END PATCH 2.86 REPLACED LOGIC
.
. SCALER DATE CONVERSION VARIABLES.
. .................................
.
START    FORM      "30318"      STARTING DATE JAN 3 83 (MONDAY).
SCALER   FORM      5
SAVSCLR  FORM      5
NWORK    FORM      5.4
DECIMAL  FORM      .4
DAYS     FORM      3
HOLD     DIM       8
TERMS    FORM      3
TAB      INIT      "000031059090120151181212243273304334"
MODE     FORM      1           PROGRAM MODE - ADD = 1, LIST = 2, UPDATE = 3,
.                                             MAINT= 4, END = 5
.
. .............................................................................
. PROGRAM  MAIN
. .............................................................................
.
. SET TRAPS OPEN FILES.
. .....................
         TRAP      EXIT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NSCH0001" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "MAIL DATE SCHEDULE" TO STITLE
         clock     date to today
         unpack    today into mm,str6
         move      mm to sysmm
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NXRFPATH         .SET ACCESS TO LIST KEY.
         TRAP      IO GIVING ERROR IF IO
.         DISPLAY   *P1:24,*EL,"OPENING MAILDATE FILE";
.         OPEN      NSCHFILE,"MAILDATE"
.         DISPLAY   *P1:24,*EL,"MAILDATE FILE OPENED";
.         DISPLAY   *P1:24,*EL,"OPENING INSTRUCTION FILE";
.         OPEN      NMDLFILE,"NINMDLST"
.         DISPLAY   *P1:24,*EL,"INSTRUCTION FILE OPENED";
         TRAPCLR   IO
.
start     DISPLAY   *P1:1,*ES,*P30:01,"MAIL DATE  SCHEDULE":
                   *P32:6,"LIST##: ";
         CALL      CARDREAD
         CALL      READSPEC

. DISPLAY MENU;
. ............
MENU
. RESET TABLES
         MOVE      "09" TO V1
         MOVE      "09" TO V2
         MOVE      "09" TO V3
         MOVE      "09" TO V4
         MOVE      "09" TO V5
         MOVE      "000000000" TO TOT1
         MOVE      "000000000" TO TOT2
         MOVE      "000000000" TO TOT3
         MOVE      "000000000" TO TOT4
         MOVE      "000000000" TO TOT5
         MOVE      "000000000" TO overbook
         MOVE      "000000000" TO TOTBOOK
         MOVE      C1 TO NPASKEY           *REMOVE ANY PASSWORD
.
         DISPLAY   *P1:8,*EF,*P30:01,"MAIL DATE  SCHEDULE":
                   *P30:09,"(A)dd or review":
                   *P30:11,"(L)ist instructions":
                   *P30:13,"(U)pdate schedule":
                   *P30:15,"(M)aintenance":
                     *P30:17,"(N)ew List":
                   *P30:19,"(F)inished";
         KEYIN     *P27:21,"Your choice ",*uc,STR1,*lc;
         REP       "A1L2U3M4F5N6" IN STR1
         MOVE      STR1 TO MODE
         BRANCH    MODE OF ADD,LIST,UPDATE,MAINT,EXIT,START
         GOTO      MENU
.
.
. ADD MODIFY REVIEW MAILDATE INFO.
. ................................
ADD
         MOVE      C1 TO SELECT
.         DISPLAY   *P1:1,*ES,*P30:01,"MAIL DATE  SCHEDULE":
.                   *P32:6,"LIST##: ";
.         CALL      CARDREAD
.         CALL      READSPEC
         CMATCH    "*",ADDSW
         CALL      ADDPASS IF EQUAL          *PASSWORD REQUIRED.
         CMATCH    " " TO MDLcode           *BOOKING ALLOWED ?
         GOTO      KMONTH IF EQUAL           *YES
         GOTO      KMONTH IF EOS             *YES
         CMATCH    yes TO MDLcode           *BOOKING ALLOWED ?
         GOTO      KMONTH IF EQUAL           *YES
         DISPLAY   *P1:24,*EL,"BOOKING NOT ALLOWED",*B,*W3;
         GOTO      MENU                      *NO
. .............................................................................
. ADDPASS - CHECK PASSWORD LIST HAS NOT BEEN PREVIOUSLY SCHEDULED.
ADDPASS
         CLEAR     NPASKEY
         DISPLAY   *P1:23,*EF,"THIS LIST HAS NOT BEEN PREVIOUSLY BOOKED":
                   " BOOKING INSTRUCTIONS NEEDED."
         GOTO      KEYPASS
.
.MODPASS -  4/85 LIST INSTRUCTION MODIFICATION DESIRED.
MODPASS  CLEAR     NPASKEY
         DISPLAY   *P1:23,*EF,"PASSWORD REQUIRED FOR MODIFICATION OF":
                   " INSTRUCTIONS.";
KEYPASS
         KEYIN     *P1:24,*EL,"Enter password ",*EOFF,NPASKEY;
         PACK      NPASFLD FROM Q,NPASKEY
         CALL      NPASKEY
         GOTO      MENU IF OVER
         DISPLAY   *P1:23,*EF;
         RETURN
...............................................................................
. KEYIN EDIT AND READ DATA DATACARD INFO.;
CARDREAD
         KEYIN     *P39:06,*JR,*ZF,SCHKEY;
         SCAN      "?" IN SCHKEY
         if        equal
         clear     lstnum
         CALL      LISTHELP
.       display  *P39:07,lstnum,*w10
         type      LSTNUM          .RETURN WITH #?
         if        eos 
         clear     schkey
         goto      cardread
         else
         MOVE      LSTNUM to SCHKEY       .YES
         rep       zfill in schkey
           endif
           endif
         MOVE      SCHKEY TO HOLDLIST
         MATCH     "00000*",SCHKEY         ** EXIT
         GOTO      CARDEXIT IF EQUAL
         GOTO      CARDERR IF EOS            ** EXIT IF NULL
           SCAN      "M" IN SCHKEY           *USEING MAILER NUMBER?
           GOTO      USEMLR IF EQUAL
           RESET     SCHKEY
         TYPE      SCHKEY
.         display  *P39:07,SCHKEY,*w10
         GOTO      CARDERR IF EOS            ** LIST NUMBER MUST BE NUMERIC
         GOTO      CARDERR IF NOT EQUAL      ** LIST NUMBER MUST BE NUMERIC
         GOTO      USECARD
USEMLR     BUMP      SCHKEY 2
           CLEAR     MKEY
           APPEND    SCHKEY TO MKEY
           APPEND    "000" TO MKEY
           RESET     MKEY
           RESET     SCHKEY
           CALL      NMLRKEY
           MOVE      MCOMP TO OLSTNAME
           MOVE      C0 TO UNIVERSE
           CLEAR     ELSTCDE
           CLEAR     STATUS
           GOTO      USE
.
USECARD  MOVE      C1 TO NDATPATH
         MOVE      SCHKEY TO NDATFLD
         rep       zfill in ndatfld
           MOVE      C0 TO UNIVERSE
         CALL      NDATKEY
         GOTO      NOLIST IF OVER
USE      MOVE      SCHKEY TO NXRFFLD
.         move      schkey to holdlist                
         rep       zfill in nxrffld
         CALL      NXRFKEY
           IF        OVER
           MOVE      B4 TO NXRFMLR
           ENDIF
.          move      nxrflist to schkey
.          move      nxrflist to holdlist
         DISPLAY   *P08:02,*EF,"MLR## ",NXRFMLR:
                   *P20:02,"LIST## ",SCHKEY,"  ",OLSTNAME,*P72:02,UNIVERSE;
         MATCH     "         ",UNIVERSE
         CALL      UNIVBLNK IF EQUAL
         DISPLAY   *P01:03,"         "
         CMATCH    "C" TO ELSTCDE
         CALL      EXCL IF EQUAL
         CMATCH    "W",STATUS
         CALL      WITHDRWN IF EQUAL
           MOVE        YES TO STR1
         KEYIN     *P50:06,"OK? ",*RV,*uc,STR1,*lc,*P50:06,*EL;
         CMATCH    YES,STR1
         RETURN    IF EQUAL
         DISPLAY   *P39:06,"______",*P20:02,*EL,*HON,"OOPS WRONG LIST.",*B:
                   *P32:6,*HOFF,"LIST##",*W:
                   *P20:02,*EL;
         GOTO      CARDREAD
.
UNIVBLNK
         DISPLAY   *P72:02,*HON,"NOT AVAIL.",*HOFF;
         RETURN
.
EXCL
         DISPLAY   *P01:03,*HON,"EXCLUSIVE",*HOFF;
         RETURN
.
. WITHDRWN - ORDER IS NOT CURRENTLY AVAILABLE.
. ............................................
WITHDRWN
         DISPLAY   *P20:05,*EF,"This list is",*HON," WITHDRAWN ":
                   *HOFF,"booking not allowed.",*B,*W,*B;
         KEYIN     *P70:5,"Enter",STR1;
         goto      withexit
.         BRANCH    MODE OF WTHDRWN1,WITHEXIT,WTHDRWN1,WTHDRWN1,WTHDRWN1
WTHDRWN1 NORETURN
         DISPLAY   *P20:02,*EF;
         DISPLAY   *P32:06,"LIST##: ";
         GOTO      CARDREAD
WITHEXIT
         RETURN
.
. CARDERR - LIST NUMBER MUST BE NUMERIC.
. .....................................
CARDERR
         BEEP
         DISPLAY   *P23:08,*EL,"List number must be NUMERIC !!!",*W,*W,*W,*W,*W
         DISPLAY   *P23:08,*EL;
         GOTO      CARDREAD
.
. CARDEXIT - EXIT SUBROUTINE.
. ...........................
CARDEXIT
         MOVE      "*",STR1              *SET SWITCH FOR PICK OFF
         COMPARE   C4 TO SELECT       *
         BEEP
         NORETURN
         GOTO      MENU
NOLIST
         BEEP
         DISPLAY   *P23:08,*EL,"I CAN'T FIND A LIST WITH THAT NUMBER !?";
         PAUSE     "5"
         DISPLAY   *P23:08,*EL;
         GOTO      CARDREAD
. END SUBROUTINE.
. ............................................................................
KMONTH
         KEYIN     *p1:9,*ef,*P32:12,*EL,"MONTH: ",*JR,*ZF,NMM;
         COMPARE   C0,NMM
         GOTO      MENU IF EQUAL
         COMPARE   C1,NMM
         GOTO      KMONTH IF LESS
         COMPARE   "13",NMM
         GOTO      KMONTH IF NOT LESS
         CALL      MMNAM
         DISPLAY   *P30:12,*EL,"SELECTED MONTH ",MMNAM;
           MOVE      YES TO STR1
         KEYIN     *P50:12,"OK? ",*RV,*uc,STR1,*lc,*P50:12,*EL;
         CMATCH    YES,STR1
         GOTO      KYEAR IF EQUAL
         GOTO      KMONTH
KYEAR
         CLOCK     YEAR TO YY                 **GET YEAR FROM SYSTEM CLOCK
         MOVE      YY TO NYY
.         compare   nmm to c6               * should we use next year?
.         if        not less
         compare   nmm to sysmm
         goto      yrdis if equal
         if        not less
         add       c1 to nyy                *yes.
         endif
.         endif
yrdis    DISPLAY   *P32:14,*EL,"YEAR: ",NYY;
           MOVE        YES TO STR1
         KEYIN     *P50:14,"OK? ",*RV,*uc,STR1,*lc,*P50:12,*EL;
         CMATCH    YES,STR1
         GOTO      DATEXIT IF EQUAL
         KEYIN     *P38:14,*ZF,*JR,*RV,NYY;
           MOVE      YES TO STR1
         KEYIN     *P50:14,"OK? ",*RV,*uc,STR1,*lc,*P50:12,*EL;
         CMATCH    YES,STR1
         GOTO      DATEXIT IF EQUAL
         GOTO      KYEAR
DATEXIT
         BRANCH    SELECT OF KYADD,KYMOD,KYMAIN
. KYADD -  RETURN TO ADD REVIEW SUBROUTINE.
KYADD
         GOTO      READATE
. KYMOD - RETURN TO MODIFY DELETE SUBROUTINE.
KYMOD
         GOTO      READATE
. KYMAIN - RETURN TO MAINTANCE SUBROUTINE.
KYMAIN
         GOTO      MAINT1
.
. READ MASTER MAILDATE FILE
. .........................
READATE
         CALL      DAYSMO
         CALL      CALENDAR
         CLEAR     NSCHFLD
         CLEAR     NSCHFLD2
         CLEAR     NSCHFLD3
         MOVE      NMM,MM
         REP       ZFILL,MM
         MOVE      NYY,YY
         REP       ZFILL,YY
         PACK      NSCHFLD FROM K1,SCHKEY
         PACK      NSCHFLD2 FROM K2,YY
         PACK      NSCHFLD3 FROM K3,MM
         DISPLAY   *P1:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
           CALL        NSCHAIM
         GOTO      NODATE IF OVER
         GOTO      WHATPOS
READKG
         DISPLAY   *P1:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
           CALL        NSCHKG
.         READKG    NSCHFILE;SCHKEY,SCHYEAR,SCHMONTH,SCHDAY,SCHMLR,SCHQUANT
         GOTO      NOMORE IF OVER
.
. WHATPOS - GET HORIZONTAL POSITION
. .................................
.
WHATPOS
         MOVE      SCHDAY TO DAYWORK
         COMPARE   DAYWORK TO MON1
         GOTO      COLUMN1 IF EQUAL
         COMPARE   DAYWORK TO MON2
         GOTO      COLUMN2 IF EQUAL
         COMPARE   DAYWORK TO MON3
         GOTO      COLUMN3 IF EQUAL
         COMPARE   DAYWORK TO MON4
         GOTO      COLUMN4 IF EQUAL
         COMPARE   DAYWORK TO MON5
         GOTO      COLUMN5 IF EQUAL
COLUMN1
         MOVE      C1 TO NUM
         GOTO      DISDATE
COLUMN2
         MOVE      C2 TO NUM
         GOTO      DISDATE
COLUMN3
         MOVE      C3 TO NUM
         GOTO      DISDATE
COLUMN4
         MOVE      C4 TO NUM
         GOTO      DISDATE
COLUMN5
         MOVE      C5 TO NUM
         GOTO      DISDATE
.
. DISDATE - GET SCREEN POSITIONING AND DISPLAY RECORD;
. ...................................................
.
DISDATE
.        TABLE OF HORIZONTAL POSITIONS.
         LOAD      H FROM NUM OF H1,H2,H3,H4,H5
.
.        TABLE OF VERTICAL POSITIONS
         LOAD      V FROM NUM OF V1,V2,V3,V4,V5
         ADD       C1 TO V
.
         COMPARE   "24" TO V
         GOTO      MORENT IF EQUAL
        GOTO        STOREM
MORENT   KEYIN     *P1:24,*EL,"  There are more entries do you want to ":
                   "see them (Y/N): ",*T254,ANS;
         CMATCH    YES TO ANS
         GOTO      MENU IF NOT EQUAL
         MOVE      C9 TO V1
         MOVE      C9 TO  V2
         MOVE      C9 TO  V3
         MOVE      C9 TO V4
         MOVE      C9 TO V5
         CALL      CALENDAR
         GOTO      DISCAL
STOREM   STORE     V INTO NUM OF V1,V2,V3,V4,V5
.
DISCAL   DISPLAY   *PH:V,SCHMLR,SCHQUANT;
.
. ADD QUANTITIES TO OVERBOOK TOTALS.
. ..................................
.
         LOAD      OVERBOOK FROM NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
         MOVE      SCHQUANT TO NWORK9
         ADD       NWORK9 TO OVERBOOK
         ADD       NWORK9 TO TOTBOOK
         STORE     OVERBOOK IN NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
.
.
. CHECK FOR MODE SELECTION.
. .........................
.
         BRANCH    SELECT OF ADDINQ,UPDTE
ADDINQ
         GOTO      READKG
UPDTE
         KEYIN     *P1:24,*EL,"Is this the record you want to mod/del?":
                   " (*) to exit ",*uc,STR1,*lc;
         CMATCH    YES,STR1
         GOTO      UPDATE1 IF EQUAL
         CMATCH    NO,STR1
         GOTO      READKG IF EQUAL
         CMATCH    "*",STR1
         GOTO      MENU IF EQUAL
         GOTO      UPDTE
.
. NODATE - NO RECORDS FOUND FOR GIVEN LIST/MONTH/YEAR
. ...................................................
.
NODATE
         DISPLAY   *P1:24,*EL,"NO RECORDS FOUND",*B,*W4;
         BRANCH    SELECT OF ADDATE,MENU,MENU
         GOTO      MENU
.
.
. NOMORE - ALL RECORDS FOUND AND DISPLAYED FOR GIVEN LIST/MONTH/YEAR.;
. ...................................................................
NOMORE
.         DISPLAY   *P1:24,*EL,"NO MORE RECORDS FOUND":
.                  *P64:24,"Total:",TOTBOOK;
         DISPLAY   *P1:24,*b,"NO MORE RECORDS FOUND",*w2
         display   *p1:24,*el,*P06:24,tot1:
                   *p22:24,tot2:
                   *p39:24,tot3:
                   *p54:24,tot4,*p64:24, "Total:",TOTBOOK;
         BEEP
         KEYIN     *P30:23,"Hit ENTER to continue",STR1;
         BRANCH    SELECT OF ADDATE
         GOTO      MENU
.
. ........................
. ADDATE - ADD A NEW ENTRY
. ........................
ADDATE
         KEYIN     *P1:24,*EL,"Add on what day (1-5), (*) to exit. ":
                   *P64:24,"Total:",*DV,TOTBOOK,*P37:24,STR1;
.         keyin     *el,*P1:24,"week1",tot1:
.                   *p11:24,"week2",tot2,*p64:1, "Total:",*DV,TOTBOOK,*P37:24,STR1;
         CMATCH    "*",STR1
         if         equal
         move       holdlist to schkey
         goto       menu
         endif
.         GOTO      MENU IF EQUAL
         TYPE      STR1            *MUST BE NUMERIC
         GOTO      ADDATE IF EOS
         MOVE      STR1 TO NUM
         COMPARE   c6 TO NUM
         GOTO      ADDATE IF NOT LESS       *MUST BE 1-5
.
. GET DAY(DATE) AND LOAD VARIABLE (DAY)
         LOAD      DAYWORK FROM NUM OF MON1,MON2,MON3,MON4,MON5
         MOVE      DAYWORK TO SCHDAY
         REP       ZFILL,SCHDAY
         COMPARE   C0 TO DAYWORK
         GOTO      ADDATE IF EQUAL         *MUST HAVE BE VALID DATE
.
. GET KEYIN POSTIONS FROM TABLES.;
.         TABLE     OF HORIZONTAL POSTIONS.
         LOAD      H FROM NUM OF H1,H2,H3,H4,H5
.
.         TABLE     OF VERTICAL POSITIONS.
         LOAD      V FROM NUM OF V1,V2,V3,V4,V5
         ADD       C1 TO V
         STORE     V INTO NUM OF V1,V2,V3,V4,V5
.
ADKEYIN
         KEYIN     *PH:V,"MAILER,QUANT  ",*PH:V,*EDIT,SCHMLR
         KEYIN     *PH:V,*DV,SCHMLR,*JR,SCHQUANT
         DISPLAY   *PH:V,SCHMLR,SCHQUANT
         CMATCH    " " TO SCHMLR
         GOTO      ADKEYIN IF EQUAL        *MUST HAVE MAILER DESCRIPTION.;
         GOTO      ADKEYIN IF EOS          *MUST HAVE MAILER DESCRIPTION.;
         CMATCH    "*" TO SCHMLR
         GOTO      ADRESET IF EQUAL        *ESCAPE FROM ENTRY BY USER.
         SCAN      "*" IN SCHQUANT
         GOTO      ADRESET IF EQUAL        *ESCAPE FROM ENTRY BY USER.
         TYPE      SCHQUANT
         GOTO      ADKEYIN IF NOT EQUAL    *QUANTITY MUST BE NUMERIC;
         MOVE      SCHQUANT TO NWORK9
.
. GET OVERBOOK TOTAL FOR CORRECT DAY FROM TABLE.
. ..............................................
         LOAD      OVERBOOK FROM NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
         ADD       NWORK9 TO OVERBOOK        *ADD QTY TO RUNNING BAL.
         ADD       NWORK9 TO TOTBOOK
           MOVE      YES TO STR1
         KEYIN     *P1:24,*EL,"OK ? ",*RV,*uc,STR1,*lc,*P1:24,*EL;
         CMATCH    YES,STR1
         GOTO      ADWRITE IF EQUAL
         DISPLAY   *PH:V,"******,******",*B;
         GOTO      ADDQUES
ADDQUES
         SUBTRACT  NWORK9 FROM OVERBOOK       *RESET RUNNING BAL.
         SUBTRACT  NWORK9 FROM TOTBOOK
         BEEP
         KEYIN     *P1:24,*EL,"ENTRY NOT ADDED. TRY AGAIN ? ",*uc,STR1,*lc;
         CMATCH    YES,STR1
         GOTO      ADKEYIN IF EQUAL
ADRESET
. RESET TABLE IF ENTRY IS NOT TO HAPPEN.
         DISPLAY   *PH:V,"******,******",*B;
         RESET     SCHQUANT
         SUBTRACT  C1 FROM V
         STORE     V INTO NUM OF V1,V2,V3,V4,V5
         GOTO      ADDATE
.
. ADWRITE - WRITE NEW ENTRY TO FILE
ADWRITE
         GOTO      OVERBOOK              *CHECK FOR OVERBOOKING.
ADDOK
         CMATCH    NO,ENTRYOK
         GOTO      ADDQUES IF EQUAL        *OVERBOOKING NOT AUTHORIZED.
. STORE NEW BOOKING TOTAL INTO TABLE.
         STORE     OVERBOOK INTO NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
         MOVE      NYY,SCHYEAR
         MOVE      NMM,SCHMONTH
         REP       ZFILL IN SCHYEAR
         REP       ZFILL IN SCHMONTH
         MOVE      HOLDLIST TO SCHKEY        *RESTORE LIST #
           CALL      NSCHWRT
         GOTO      ADDATE
.
.
. ..........................................
. OVERBOOK - CHECK FOR OVER BOOKING ON LIST.
. ..........................................
OVERBOOK
         MOVE      YES TO ENTRYOK
.START PATCH 2.86 REPLACED LOGIC
.         MOVE      UNIVERSE TO NWORK9
.         CMATCH    " " TO UNIVERSE
.         GOTO      OBEXIT IF EOS       *EXIT IF UNIVERSE QUANTITY IS NULL.
.         MATCH     "         ",UNIVERSE
.
         MOVE      UNIVERSE TO NWORK9
         CMATCH    " " TO UNIVERSE
         GOTO      OBEXIT IF EOS       *EXIT IF UNIVERSE QUANTITY IS NULL.
         MATCH     "          ",UNIVERSE
.END PATCH 2.86 REPLACED LOGIC
         GOTO      OBEXIT IF EQUAL
         COMPARE   NWORK9 TO OVERBOOK
         GOTO      OBEXIT IF EQUAL
         GOTO      YESOVER IF NOT LESS
         GOTO      OBEXIT
YESOVER
         KEYIN     *P1:24,*EL,*B,*B,"YOU HAVE OVERBOOKED THIS LIST!! ":
                   "DO YOU HAVE AUTHORIZATION (Y/N) ? ",*uc,STR1,*lc;
         CMATCH    YES,STR1
         GOTO      OVERPASS IF EQUAL
         CMATCH    NO,STR1
         GOTO      ENTRYNO IF EQUAL         *NOT AUTHORIZED TO OVERBOOK.
OVERPASS
         KEYIN     *P1:24,*EL,"Enter password",*EOFF,NPASKEY;
         PACK      NPASFLD FROM C,NPASKEY
         CALL      NPASKEY
         GOTO      ENTRYNO IF OVER          *PASSWORD NOT VALID.
         MOVE      YES,ENTRYOK              *PASSWORD VALID.
         GOTO      OBEXIT
ENTRYNO
         MOVE      NO TO ENTRYOK           *NOT ALLOWED TO OVERBOOK.
         DISPLAY   *P1:24,*EL,*B,*B,"ENTRY NOT ALLOWED !",*W2,*P1:24,*EL;
         GOTO      OBEXIT
OBEXIT
         BRANCH    SELECT OF ADDOK,MODOK    *RETURN TO ENTRY POINT.
. .........................................
. UPDATE - MODIFY OR DELETE A SINGLE ENTRY.
. .........................................
.
UPDATE
         MOVE      C2 TO SELECT          **SET BRANCHING CONSTANT.
.         DISPLAY   *P1:1,*ES,*P30:1,"MAIL DATE SCHEDULE":
.                   *P32:6,"LIST##: ";
.         CALL      CARDREAD
.         CALL      READSPEC
.
         CMATCH    "*",ADDSW
         CALL      ADDPASS IF EQUAL
         CMATCH    " " TO MDLcode        *BOOKING OK?
         GOTO      KMONTH IF EQUAL        *YES
         GOTO      KMONTH IF EOS          *YES
         CMATCH    yes TO MDLcode        *BOOKING OK?
         GOTO      KMONTH IF EQUAL        *YES
         DISPLAY   *P1:24,*EL,"NO BOOKING ALLOWED!,",*B,*W3;
         GOTO      MENU                   *NO
UPDATE1
         KEYIN     *P1:24,*EL,"(M)odify or (D)elete? ",*uc,STR1,*lc;
         CMATCH    "M",STR1
         GOTO      MODDATE IF EQUAL
         CMATCH    "D",STR1
         GOTO      DELDATE IF EQUAL
         GOTO      READKG
.
. MODDATE - MODIFY SCHEDULE ENTRY.
. ................................
MODDATE
         MOVE      SCHMLR,SDESC            *SAVE MAILER DESC
         MOVE      SCHQUANT,SQUANT           *SAVE QUANTITY
         MOVE      SCHQUANT TO NWORK9
         LOAD      OVERBOOK FROM NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
         SUBTRACT  NWORK9 FROM OVERBOOK
         SUBTRACT  NWORK9 FROM TOTBOOK
         KEYIN     *PH:V,"MAILER,QUANT  ",*PH:V,SCHMLR,*JR,SCHQUANT
         MOVE      SCHQUANT TO NWORK9
         ADD       NWORK9 TO OVERBOOK
         ADD       NWORK9 TO TOTBOOK
         KEYIN     *P1:24,*EL,"OK ?",*uc,STR1,*lc,*P1:24,*EL;
         CMATCH    YES,STR1
         GOTO      MODDATE1 IF EQUAL
MODNG
         MOVE      SDESC TO SCHMLR         *RESTORE MAILER
         MOVE      SQUANT TO SCHQUANT        *RESTORE QUANTITY.
         DISPLAY   *PH:V,SCHMLR,SCHQUANT;
         SUBTRACT  NWORK9 FROM OVERBOOK
         SUBTRACT  NWORK9 FROM TOTBOOK
         MOVE      SCHQUANT TO NWORK9
         ADD       NWORK9 TO OVERBOOK     *RESTORE RUNNING TOTAL.
         ADD       NWORK9 TO TOTBOOK
         GOTO      MODQUES
.
MODDATE1
         GOTO      OVERBOOK               *CHECK FOR OVERBOOKING.
MODOK
         CMATCH    NO,ENTRYOK
         GOTO      MODNG IF EQUAL         *ENTRY NOT ALLOWED (OVERBOOKED)
         STORE     OVERBOOK INTO NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
           CALL        NSCHUPD
         GOTO      READKG
.
MODQUES
         KEYIN     *P1:24,*EL,"TRY AGAIN ? ",*uc,STR1,*lc;
         CMATCH    YES,STR1
         GOTO      MODDATE IF EQUAL
         GOTO      READKG
.
.
. DELDATE - DELETE SCHEDULE ENTRY
. ...............................
.
DELDATE
         KEYIN     *P1:24,*EL,*B,"ARE YOU SURE? ",*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      READKG IF EQUAL
         CMATCH    YES,STR1
         GOTO      DELDATE1 IF EQUAL
         GOTO      DELDATE
DELDATE1
         DISPLAY   *PH:V,"              ";
           CALL      NSCHDEL
.
. CORRECT OVERBOOK TABLE.
.
         MOVE      SCHQUANT TO NWORK9
         LOAD      OVERBOOK FROM NUM OF TOT1,TOT2,TOT3,TOT4,TOT5
         SUBTRACT  NWORK9 FROM OVERBOOK
         SUBTRACT  NWORK9 FROM TOTBOOK
         LOAD      OVERBOOK INTO NUM OF  TOT1,TOT2,TOT3,TOT4,TOT5
.
. CORRECT VERTICAL POSTION TABLE.
.
         SUBTRACT  C1 FROM V
         STORE     V INTO NUM OF V1,V2,V3,V4,V5
         GOTO      READKG
. .............................
.
. MAINTENANCE (DELETE OLD DATA)
. .............................
MAINT
         DISPLAY   *P01:01,*ES,*P30:01,"MAIL DATE SCHEDULE":
                   *P30:02,"**MAINTENANCE";
         DISPLAY   *P1:24,*B," THIS WILL KILL ALL RECORDS FOR MONTH GIVEN":
                   *B,*W5;
         MOVE      C3 TO SELECT       **SET BRANCHING CONSTANT.
         KEYIN     *P1:24,*EL,"ENTER PASSWORD FOR MAINTENANCE ",*EOFF,NPASKEY;
         match     "COSMO" to npaskey
         goto      nopass if not equal
.         PACK      NPASFLD FROM C,NPASKEY
.         CALL      NPASKEY
.         GOTO      NOPASS IF OVER         ** PASSWORD INVALID.
         DISPLAY   *P1:24,*EL,"PASSWORD ACCEPTED.";
         GOTO      KMONTH                **GO GET DATE.
         GOTO      MAINT1
NOPASS
         DISPLAY   *P1:24,*EL,*B,*B,"PASSWORD INVALID",*W2;
         GOTO      MENU
MAINT1
         CLEAR     NSCHFLD
         CLEAR     NSCHFLD2
         MOVE      NMM TO MM
         REP       ZFILL,MM
         MOVE      NYY TO YY
         REP       ZFILL,YY
         PACK      NSCHFLD2 FROM K2,YY
         PACK      NSCHFLD3 FROM K3,MM
           CALL      NSCHAIM
         GOTO      NOMAINT IF OVER
         GOTO      MAINDEL
MAINT1A  CALL          NSCHKG
         GOTO      MAINEXIT IF OVER
MAINDEL   CALL         NSCHDEL
         GOTO      MAINT1A
. NOMAINT - NO RECORDS FOR TIME PERIOD SPECIFIED.
NOMAINT
         DISPLAY   *P1:24,*EL,"NO RECORDS FOUND ",*W3;
         GOTO      MENU
. MAINEXIT - MAINTENANCE COMPLETE.
MAINEXIT
         DISPLAY   *P1:24,*EL,*B,"MAINTENANCE COMPLETED",*W3;
         GOTO      MENU
.
. LIST SPECIAL INSTRUCTIONS UPDATE.
. .................................
LIST
         DISPLAY   *P1:1,*ES,*P30:01,"MAIL DATE  SCHEDULE":
                    *P28:04,"LIST INSTRUCTION UPDATE":
                    *P32:06,"LIST##: ";
.         CALL      CARDREAD
.         CALL      READSPEC
. PASSWORD REQUIRED TO ADD NEW LIST INSTRUCTIONS.
         CMATCH    "*",ADDSW
         CALL      ADDPASS IF EQUAL
         CALL      MODPASS IF NOT EQUAL
         CALL      MODPASS IF EOS
.
WHATLINE 
.START PATCH 2.86 REPLACED LOGIC
.         DISPLAY   *P01:03,*Ef,*cyan,"1)",*white,MDLCODE:
.                   *P25:03,*cyan,"2)",*white,MDLLCRCD:
.                   *P46:03,*EL,*cyan,"3)",*white,"PLANNER: ",MDLPLAN:
.                   *cyan,"  4)",*white,"CALLER: ",MDLCALL:
.                   *P1:06,*EL,*cyan,"5)",*white,MDLtext:
.                  *P69:24,*cyan,revdate,*white
          unpack    REVDATE,str4,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,str4
          DISPLAY   *P01:03,*Ef,*cyan,"1)",*white,MDLCODE:
                    *P25:03,*cyan,"2)",*white,MDLLCRCD:
                    *P46:03,*EL,*cyan,"3)",*white,"PLANNER: ",MDLPLAN:
                    *cyan,"  4)",*white,"CALLER: ",MDLCALL:
                    *P1:06,*EL,*cyan,"5)",*white,MDLtext:
                    *P69:24,*cyan,str10,*white
.END PATCH 2.86 REPLACED LOGIC
.          DISPLAY   *P01:03,*EL,*cyan,"1)",*white,MDLLINE1:
.                   *P01:04,*EL,*cyan,"2)",*white,MDLLINE2:
.                   *P01:05,*EL,*cyan,"3)",*white,MDLcode:
.                   *P25:05,*cyan,"4)",*white,mdllcrCD:
.                   *P46:05,*EL,*cyan,"5)",*white,"PLANNER: ",MDLplan:
.                    *cyan,"  6)",*white,"CALLER: ",MDLcall:
.                      *P1:06,*EL,*cyan,"7)",*white,MDLLINE3:
.                      *P1:07,*EL,*cyan,"8)",*white,MDLLINE4
         CMATCH    NO TO MDLcode
         CALL      NBOOK IF EQUAL
         CALL      YBOOK IF NOT EQUAL
         CALL      YBOOK IF EOS
         CMATCH    NO TO mdllcrCD
         CALL      NLCR IF EQUAL
         CALL      YLCR IF NOT EQUAL
         CALL      YLCR IF EOS
.         DISPLAY   *P01:03,"1)",*P01:04,"2)":
.                   *P01:05,"3)",*P25:05,"4)":
.                     *P01:06,"5)":
.                      *P01:07,"7)":
.                      *P01:08,"8)"
         KEYIN     *P1:24,*EL,"Change line number: ",*HON,"(*)",*HOFF:
                   " Done, (9)No changes,":
                   " (10)to delete :",STR2;
.         SCAN      "*",STR2
.         GOTO      LSTWRITE IF EQUAL
         goto      menu
         GOTO      WHATLINE IF EOS
           RESET     STR2
         TYPE      STR2
         GOTO      WHATLINE IF EOS
         MOVE      STR2 TO ITEM
         COMPARE   "9" TO ITEM
         GOTO      MENU IF EQUAL
         COMPARE   "10",ITEM
         GOTO      EDITING IF NOT EQUAL
         KEYIN     *P1:24,*EL,*B,*B,*HON,"ARE YOU SURE!! THIS RECORD WILL":
                       " BE GONE FOREVER",*uc,STR1,*lc,*HOFF;
         CMATCH    YES,STR1
           GOTO      WHATLINE IF NOT EQUAL
         KEYIN     *P1:24,*EL,*B,*B,*HON,"ARE YOU REALLY SURE!!",*uc,STR1,*lc,*HOFF;
           CMATCH     YES TO STR1
         GOTO      DELSPEC IF EQUAL
         GOTO      WHATLINE
EDITING  BRANCH    ITEM OF EDITONE,EDITTWO,EDITHREE,EDITFOUR:
                   EDITFIVE,EDITSIX,EDIT7,EDIT8
         GOTO      WHATLINE
EDITONE
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE";
.         MOVE      MDLLINE1,HOLDLINE
.         KEYIN     *P03:03,*EL,*DV,MDLLINE1,*P03:03,*EDIT,*RV,MDLLINE1;
.         MATCH     B1,MDLLINE1
         GOTO      SAVEDONE IF EQUAL
         GOTO      SAVEDONE IF EOS
.         CMATCH    ">",MDLLINE1
         GOTO      SAVEDONE IF EQUAL
         GOTO      SAVEDONE IF EOS
.         DISPLAY   *P01:03,*EL,"1)",MDLLINE1;
         GOTO      WHATLINE
SAVEDONE
.         MOVE      HOLDLINE TO MDLLINE1
.         DISPLAY   *P01:03,*EL,"1)",MDLLINE1;
         GOTO      WHATLINE
EDITTWO
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE";
.         MOVE      MDLLINE2,HOLDLINE
.         KEYIN     *P03:04,*EL,*DV,MDLLINE2,*P03:04,*EDIT,*RV,MDLLINE2;
.         MATCH     " ",MDLLINE2
         GOTO      SAVEDTWO IF EOS
.         CMATCH    ">",MDLLINE2
         GOTO      SAVEDTWO IF EQUAL
         GOTO      SAVEDTWO IF EOS
.         DISPLAY   *P01:04,*EL,"2)",MDLLINE2;
         GOTO      WHATLINE
SAVEDTWO
.         MOVE      HOLDLINE TO MDLLINE2
.         DISPLAY   *P01:04,*EL,"2)",MDLLINE2;
         GOTO      WHATLINE
.
EDITHREE
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE":
                   " 'N' FOR NO BOOKING, ' ' FOR BOOKING OK.";
         MOVE      MDLcode,HOLDLINE
         KEYIN     *P03:05,"                     ",*P03:05,MDLcode;
         MATCH     " ",MDLcode
         GOTO      SAVEDTHR IF EOS
         CMATCH    ">",MDLcode
         GOTO      SAVEDTHR IF EQUAL
         GOTO      SAVEDTHR IF EOS
         DISPLAY   *P01:05,*EL,"3)",MDLcode;
         GOTO      THREXIT
SAVEDTHR
         MOVE      HOLDLINE TO MDLcode
         DISPLAY   *P01:05,*EL,"3)",MDLcode;
         GOTO      THREXIT
THREXIT
         CMATCH    NO TO MDLcode
         GOTO      NOBOOK IF EQUAL
         DISPLAY   *P04:05,"-BOOKING ALLOWED";
         GOTO      WHATLINE
NOBOOK   DISPLAY   *P04:05,"-NO BOOKING ALLOWED";
         GOTO      WHATLINE
EDITFOUR
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE":
                   " 'N' FOR NO LCR'S, ' ' FOR LCR'S OK.";
         MOVE      mdllcrCD,HOLDLINE
         KEYIN     *P27:05,"                     ",*P27:05,*EDIT,*RV,mdllcrCD;
         MATCH     " ",mdllcrCD
         GOTO      SAVEDFR IF EOS
         CMATCH    ">",mdllcrCD
         GOTO      SAVEDFR IF EQUAL
         GOTO      SAVEDFR IF EOS
         DISPLAY   *P25:05,*EL,"3)",mdllcrCD;
         GOTO      FOUREXIT
SAVEDFR
         MOVE      HOLDLINE TO mdllcrCD
         DISPLAY   *P25:05,*EL,"3)",mdllcrCD;
         GOTO      FOUREXIT
FOUREXIT
         CMATCH    NO TO mdllcrCD
         GOTO      NOLCR IF EQUAL
         DISPLAY   *P28:05,"-LCR'S ALLOWED";
         GOTO      WHATLINE
NOLCR   DISPLAY   *P28:05,"-NO LCR'S ALLOWED";
         GOTO      WHATLINE
.
EDITFIVE
         MOVE      MDLplan TO HOLDLINE
         KEYIN     *P57:05,MDLplan
         CMATCH    ">" TO MDLplan
         GOTO      SAVEFV IF EOS
         GOTO      SAVEFV IF EQUAL
FIVEXIT  DISPLAY   *P57:05,"  ",*P57:05,MDLplan
         GOTO      WHATLINE
SAVEFV   MOVE      HOLDLINE TO MDLplan
         GOTO      FIVEXIT
.
EDITSIX
         MOVE      MDLcall TO HOLDLINE
         KEYIN     *P72:05,*EDIT,*RV,MDLcall
         CMATCH    ">" TO MDLcall
         GOTO      SAVESX IF EOS
         GOTO      SAVESX IF EQUAL
SIXEXIT  DISPLAY   *P72:05,"  ",*P72:05,MDLcall
         GOTO      WHATLINE
SAVESX   MOVE      HOLDLINE TO MDLcall
         GOTO      SIXEXIT
.
EDIT7
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE";
.         MOVE      MDLLINE3,HOLDLINE
.         KEYIN     *P03:06,*EL,*DV,MDLLINE3,*P03:06,*EDIT,*RV,MDLLINE3;
.         CMATCH    ">",MDLLINE3
         GOTO      SAVE7 IF EQUAL
         GOTO      SAVE7 IF EOS
.         DISPLAY   *P01:06,*EL,"7)",MDLLINE3;
         GOTO      WHATLINE
.
SAVE7
.    MOVE      HOLDLINE TO MDLLINE3
.         DISPLAY   *P01:06,*EL,"7)",MDLLINE3;
         GOTO      WHATLINE
.
EDIT8
         DISPLAY   *P01:24,*EL,"ENTER '>' FOR NO CHANGE";
.         MOVE      MDLLINE4,HOLDLINE
.         KEYIN     *P03:07,*EL,*DV,MDLLINE4,*P03:07,*EDIT,*RV,MDLLINE4;
.         CMATCH    ">",MDLLINE4
         GOTO      SAVE8 IF EQUAL
         GOTO      SAVE8 IF EOS
.         DISPLAY   *P01:07,*EL,"8)",MDLLINE4;
         GOTO      WHATLINE
SAVE8
.         MOVE      HOLDLINE TO MDLLINE4
.         DISPLAY   *P01:07,*EL,"8)",MDLLINE4;
         GOTO      WHATLINE
.
.
. READSPEC - READ LIST INSTRUCTIONS.
. ..................................
READSPEC
.         CLEAR     MDLLINE1
.         CLEAR     MDLLINE2
         CLEAR     MDLcode
         CLEAR     mdllcrCD
.          CLEAR     MDLLINE3
.          CLEAR     MDLLINE4
           CLEAR       MDLplan
           CLEAR     MDLcall
         MOVE      " ",ADDSW           *CLEAR SWITCH
           MOVE      SCHKEY TO NMDLFLD
         REP       ZFILL IN NMDLFLD
           CALL      NMDLKEY
         GOTO      SPECOVER IF OVER
           CMATCH     B1 TO SCHKEY
           GOTO       SPECOVER IF EOS
.          DISPLAY   *P01:03,*EL,*cyan,"1)",*white,MDLLINE1:
.                   *P01:04,*EL,*cyan,"2)",*white,MDLLINE2:
.                   *P01:05,*EL,*cyan,"3)",*white,MDLcode:
.                   *P25:05,*cyan,"4)",*white,mdllcrCD:
.                   *P46:05,*EL,*cyan,"5)",*white,"PLANNER: ",MDLplan:
.                    *cyan,"  6)",*white,"CALLER: ",MDLcall:
.                      *P1:06,*EL,*cyan,"7)",*white,MDLLINE3:
.                      *P1:07,*EL,*cyan,"8)",*white,MDLLINE4
.START PATCH 2.86 REPLACED LOGIC
.         DISPLAY   *P01:03,*Ef,*cyan,"1)",*white,MDLCODE:
.                   *P25:03,*cyan,"2)",*white,MDLLCRCD:
.                   *P46:03,*EL,*cyan,"3)",*white,"PLANNER: ",MDLPLAN:
.                   *cyan,"  4)",*white,"CALLER: ",MDLCALL:
.                   *P1:06,*EL,*cyan,"5)",*white,MDLtext:
.                  *P69:24,*cyan,revdate,*white
          unpack    REVDATE,str4,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,str4
          DISPLAY   *P01:03,*Ef,*cyan,"1)",*white,MDLCODE:
                    *P25:03,*cyan,"2)",*white,MDLLCRCD:
                    *P46:03,*EL,*cyan,"3)",*white,"PLANNER: ",MDLPLAN:
                    *cyan,"  4)",*white,"CALLER: ",MDLCALL:
                    *P1:06,*EL,*cyan,"5)",*white,MDLtext:
                    *P69:24,*cyan,str10,*white
.END PATCH 2.86 REPLACED LOGIC
         CMATCH    NO TO MDLcode
         CALL      NBOOK IF EQUAL
         CALL      YBOOK IF NOT EQUAL
         CALL      YBOOK IF EOS
         CMATCH    NO TO mdllcrCD
         CALL      NLCR IF EQUAL
         CALL      YLCR IF NOT EQUAL
         CALL      YLCR IF EOS
         RETURN
.
NBOOK    DISPLAY   *P04:05,"-NO BOOKING ALLOWED",*B;
         RETURN
YBOOK    DISPLAY   *P04:05,"-BOOKING ALLOWED";
         RETURN
NLCR     DISPLAY   *P28:05,"-NO LCR'S ",*B;
         RETURN
YLCR     DISPLAY   *P28:05,"-LCR'S OK";
         RETURN
.
SPECOVER
         MOVE      "*" TO ADDSW            * ADDITION OF RECORDS NOT ALLOWED
         DISPLAY   *P03:03,*HON,*EL,"NO INSTRUCTIONS FOUND ",*B,*HOFF;
         MOVE      HOLDLIST TO SCHKEY
         RETURN
. ...................................
.
. LSTWRITE - WRITE SPECIAL INSTRUCTIONS
. ......................................
LSTWRITE
.         FILEPI    4;NMDLFILE
.         MOVE      SCHKEY TO HOLDLIST           *SAVE KEY
           MOVE      SCHKEY TO NMDLFLD
.         READ      NMDLFILE,SCHKEY;;
           CALL      NMDLTST
         GOTO      NEWSPEC IF OVER
         GOTO      OLDSPEC
NEWSPEC
         MOVE      nmdlfld TO mdlkey
           CALL      NMDLWRT
.         WRITE     NMDLFILE,SCHKEY;SCHKEY,MDLLINE1,MDLLINE2,MDLcode,mdllcrCD:
.                   MDLplan,MDLcall
         GOTO       MENU
OLDSPEC
          MOVE      nmdlfld TO mdlkey
          CALL         NMDLUPD 
.         UPDATE    NMDLFILE;SCHKEY,MDLLINE1,MDLLINE2,MDLcode,mdllcrCD:
.                   MDLplan,MDLcall
.         GOTO      LIST
           goto      menu
. .....................................
. DELSPEC - DELETE SPECIAL INSTRUCTIONS
. .....................................
.
DELSPEC  MOVE      SCHKEY TO NMDLFLD
           CALL      NMDLTST
           CALL      NMDLDEL
.         FILEPI    2;NMDLFILE
.         READ      NMDLFILE,SCHKEY;;
.         DELETE    NMDLFILE,SCHKEY
         GOTO      menu
. ................................................................
         INCLUDE   NDATEIO.inc
.
. CALENDAR - DISPLAY MONTH ON SCREEN.;
. ...................................
.
CALENDAR
         DISPLAY   *P38:08,*EF,*cyan,mmnam,*white;
         CALL      GETMONDY
           IFNZ      PC
         DISPLAY   *P03:09,*HON,MMNAM,*HOFF," THE ",MON1,"   ",BAR:
                   *P19:09,"2ND THE ",MON2,"   ",BAR:
                   *P35:09,"3RD THE ",MON3,"   ",BAR:
                   *P51:09,"4TH THE ",MON4,"   ",BAR:
                   *P67:09,"5TH THE ",MON5,"  ",BAR:
               *P16:09,BAR,*P32:09,BAR,*P48:09,BAR,*P64:09,BAR,*P79:09,BAR:
               *P16:10,BAR,*P32:10,BAR,*P48:10,BAR,*P64:10,BAR,*P79:10,BAR:
               *P16:11,BAR,*P32:11,BAR,*P48:11,BAR,*P64:11,BAR,*P79:11,BAR:
               *P16:12,BAR,*P32:12,BAR,*P48:12,BAR,*P64:12,BAR,*P79:12,BAR:
               *P16:13,BAR,*P32:13,BAR,*P48:13,BAR,*P64:13,BAR,*P79:13,BAR:
               *P16:14,BAR,*P32:14,BAR,*P48:14,BAR,*P64:14,BAR,*P79:14,BAR:
               *P16:15,BAR,*P32:15,BAR,*P48:15,BAR,*P64:15,BAR,*P79:15,BAR:
               *P16:16,BAR,*P32:16,BAR,*P48:16,BAR,*P64:16,BAR,*P79:16,BAR:
               *P16:17,BAR,*P32:17,BAR,*P48:17,BAR,*P64:17,BAR,*P79:17,BAR:
               *P16:18,BAR,*P32:18,BAR,*P48:18,BAR,*P64:18,BAR,*P79:18,BAR:
               *P16:19,BAR,*P32:19,BAR,*P48:19,BAR,*P64:19,BAR,*P79:19,BAR:
               *P16:20,BAR,*P32:20,BAR,*P48:20,BAR,*P64:20,BAR,*P79:20,BAR:
               *P16:21,BAR,*P32:21,BAR,*P48:21,BAR,*P64:21,BAR,*P79:21,BAR:
               *P16:22,BAR,*P32:22,BAR,*P48:22,BAR,*P64:22,BAR,*P79:22,BAR:
               *P16:23,BAR,*P32:23,BAR,*P48:23,BAR,*P64:23,BAR,*P79:23,BAR:
               *P01:09,C1,*P01:10,C2,*P1:11,C3,*P01:12,C4,*P01:13:
               C5:
               *P01:14,c6,*P01:15,c7,*P01:16,c8,*P01:17,c9,*P01:18:
               C1,C0:
               *P01:19,C1,C1,*P01:20,C1,C2,*P01:21,C1,C3,*P01:22:
               C1,C4:
               *P01:23,C1,C5;
           XIF
           IFZ      PC
         DISPLAY   *P03:09,*yellow,*white,"1ST THE ",MON1,"   ",*yellow,I179,*white:
                   *P19:09,"2ND THE ",MON2,"   ",*yellow,I179,*white:
                   *P35:09,"3RD THE ",MON3,"   ",*yellow,I179,*white:
                   *P51:09,"4TH THE ",MON4,"   ",*yellow,I179,*white:
                   *P67:09,"5TH THE ",MON5,"  ",*yellow,I179,*white:
               *P16:10,*yellow,I179,*white,*P32:10,*yellow,I179,*white,*P48:10:
               *yellow,I179,*white,*P64:10,*yellow,I179,*white,*P79:10,*yellow,I179,*white:
               *P16:11,*yellow,I179,*white,*P32:11,*yellow,I179,*white,*P48:11:
               *yellow,I179,*white,*P64:11,*yellow,I179,*white,*P79:11,*yellow,I179,*white:
               *P16:12,*yellow,I179,*white,*P32:12,*yellow,I179,*white,*P48:12:
                 *yellow,I179,*white,*P64:12,*yellow,I179,*white,*P79:12,*yellow,I179,*white:
               *P16:13,*yellow,I179,*white,*P32:13,*yellow,I179,*white,*P48:13:
               *yellow,I179,*white,*P64:13,*yellow,I179,*white,*P79:13,*yellow,I179,*white:
               *P16:14,*yellow,I179,*white,*P32:14,*yellow,I179,*white,*P48:14:
               *yellow,I179,*white,*P64:14,*yellow,I179,*white,*P79:14,*yellow,I179,*white:
               *P16:15,*yellow,I179,*white,*P32:15,*yellow,I179,*white,*P48:15:
                 *yellow,I179,*white,*P64:15,*yellow,I179,*white,*P79:15,*yellow,I179,*white:
               *P16:16,*yellow,I179,*white,*P32:16,*yellow,I179,*white,*P48:16:
                 *yellow,I179,*white,*P64:16,*yellow,I179,*white,*P79:16,*yellow,I179,*white:
               *P16:17,*yellow,I179,*white,*P32:17,*yellow,I179,*white,*P48:17:
                 *yellow,I179,*white,*P64:17,*yellow,I179,*white,*P79:17,*yellow,I179,*white:
               *P16:18,*yellow,I179,*white,*P32:18,*yellow,I179,*white,*P48:18:
                 *yellow,I179,*white,*P64:18,*yellow,I179,*white,*P79:18,*yellow,I179,*white:
               *P16:19,*yellow,I179,*white,*P32:19,*yellow,I179,*white,*P48:19:
                 *yellow,I179,*white,*P64:19,*yellow,I179,*white,*P79:19,*yellow,I179,*white:
               *P16:20,*yellow,I179,*white,*P32:20,*yellow,I179,*white,*P48:20:
                 *yellow,I179,*white,*P64:20,*yellow,I179,*white,*P79:20,*yellow,I179,*white:
               *P16:21,*yellow,I179,*white,*P32:21,*yellow,I179,*white,*P48:21:
                 *yellow,I179,*white,*P64:21,*yellow,I179,*white,*P79:21,*yellow,I179,*white:
               *P16:22,*yellow,I179,*white,*P32:22,*yellow,I179,*white,*P48:22:
                 *yellow,I179,*white,*P64:22,*yellow,I179,*white,*P79:22,*yellow,I179,*white:
               *P16:23,*yellow,I179,*white,*P32:23,*yellow,I179,*white,*P48:23:
                 *yellow,I179,*white,*P64:23,*yellow,I179,*white,*P79:23,*yellow,I179,*white:
               *P01:10,*cyan,C1,*P01:11,C2,*P1:12,C3,*P01:13,C4,*P01:14:
               C5:
               *P01:15,c6,*P01:16,c7,*P01:17,c8,*P01:18,c9,*P01:19:
               C1,C0:
               *P01:20,C1,C1,*P01:21,C1,C2,*P01:22,C1,C3,*P01:23:
               C1,C4,*white;
           XIF
         RETURN
.
. GETMONDY - GET DATES OF MONDAYS OF CURRENT MONTH.
. .................................................
GETMONDY
         MOVE      C1 TO   DAYWORK
         MOVE      C1 TO NDD
         MOVE      NMM TO MONWORK
         MOVE      NYY TO YEARWORK
.         MOVE      CC TO ??
         CALL      SCALER
TRYAGAIN
         BRANCH    NUM OF ADD1,ADD2,ADD3,ADD4,ADD5,ADD6
. ADD0 - THE FIRST DAY OF THE MONTH IS MONDAY
ADD0     GOTO      BRMONDY
. ADD1 - THE FIRST DAY OF THE MONTH IS TUESDAY
ADD1
         ADD       c6 TO DAYWORK
         GOTO      BRMONDY
. ADD2 - THE FIRST DAY OF THE MONTH IS WEDNESDAY
ADD2     ADD       C5  TO DAYWORK
         GOTO      BRMONDY
. ADD3 - THE FIRST DAY OF THE MONTH IS THURSDAY
ADD3     ADD       C4 TO DAYWORK
         GOTO      BRMONDY
. ADD4 - THE FIRST DAY OF THE MONTH IS FRIDAY
ADD4     ADD       C3 TO DAYWORK
         GOTO      BRMONDY
. ADD5 - THE FIRST DAY OF THE MONTH IS SATURDAY
ADD5     ADD       C2 TO DAYWORK
         GOTO      BRMONDY
. ADD6 - THE FIRST DAY OF THE MONTH IS SUNDAY
ADD6     ADD       C1 TO DAYWORK
         GOTO      BRMONDY
BRMONDY
         ADD       C1 TO MON
         BRANCH    MON OF MON1,MON2,MON3,MON4,MON5
         GOTO      BYEMONDY
MON1
         COMPARE   DAYWORK TO DAYSMO             DATE > DAYS IN MONTH?
         GOTO      LMON1 IF NOT LESS             OK MOVE DATE TO DISPLAY;
         MOVE      C0 TO MON1                  DATE BEYOND MONTH END.
         GOTO      BYEMONDY
LMON1    MOVE      DAYWORK TO MON1
         GOTO      ADDWEEK
MON2
         COMPARE   DAYWORK TO DAYSMO             DATE > DAYS IN MONTH?
         GOTO      LMON2 IF NOT LESS             OK MOVE DATE TO DISPLAY;
         MOVE      C0 TO MON2                  DATE BEYOND MONTH END.
         GOTO      BYEMONDY
LMON2    MOVE      DAYWORK TO MON2
         GOTO      ADDWEEK
MON3
         COMPARE   DAYWORK TO DAYSMO             DATE > DAYS IN MONTH?
         GOTO      LMON3 IF NOT LESS             OK MOVE DATE TO DISPLAY;
         MOVE      C0 TO MON3                  DATE BEYOND MONTH END.
         GOTO      BYEMONDY
LMON3    MOVE      DAYWORK TO MON3
         GOTO      ADDWEEK
MON4
         COMPARE   DAYWORK TO DAYSMO             DATE > DAYS IN MONTH?
         GOTO      LMON4 IF NOT LESS             OK MOVE DATE TO DISPLAY;
         MOVE      C0 TO MON4                  DATE BEYOND MONTH END.
         GOTO      BYEMONDY
LMON4    MOVE      DAYWORK TO MON4
         GOTO      ADDWEEK
MON5
         COMPARE   DAYWORK TO DAYSMO             DATE > DAYS IN MONTH?
         GOTO      LMON5 IF NOT LESS             OK MOVE DATE TO DISPLAY;
         MOVE      C0 TO MON5                  DATE BEYOND MONTH END.
         GOTO      BYEMONDY
LMON5    MOVE      DAYWORK TO MON5
         GOTO      BYEMONDY
.
. ADDWEEK - ADD SEVEN TO THE FIRST MONDAYS DATE FOR FOLLOWING MONDAY DATE.
. .........................................................................
ADDWEEK
         ADD       c7 TO DAYWORK
         GOTO      BRMONDY
         RETURN
.
.
. BYEMONDY - EXIT GETMONDY ROUTINE.
. .................................
.
BYEMONDY
         MOVE      C0 TO MON
         RETURN
.
. SCALER - SCALER DATE CONVERSION
. ...............................
. ENTERS WITH     NDD - 01
.                 NMM - MONTH REQUESTED
.                 NYY - YEAR REQUESTED
.
. EXITS WITH      NUM = DAY OF THE WEEK (0=MONDAY,6=SUNDAY)
.
SCALER
.
. CONVERT MONTH
         MOVE      NMM TO NWORK
         MULT      C3,NWORK
         RESET     TAB,NWORK
         LENSET    TAB
         SUB       C2,NWORK
         RESET     TAB,NWORK
         MOVE      TAB TO SCALER
. ADD DAY
.
         MOVE      NDD TO NWORK
         ADD       NWORK TO SCALER
.
         MOVE      NYY TO NWORK
         MULT      "365",NWORK
         ADD       NWORK TO SCALER
.
         MOVE      NYY TO NWORK
         COMPARE   C0 TO NWORK
         GOTO      WAS1900 IF EQUAL
         DIV       C4,NWORK
.
WAS1900  MOVE      NWORK TO HOLD
         RESET     HOLD,5
         LENSET    HOLD
         RESET     HOLD
         MOVE      HOLD TO NWORK
         ADD       NWORK TO SCALER
         RESET     HOLD,8
         LENSET    HOLD
         RESET     HOLD,7
         MOVE      HOLD TO NWORK
         COMPARE   "0000" TO NWORK
         GOTO      DONE IF NOT EQUAL
         MOVE      NMM TO NWORK
         COMPARE   C3 TO NWORK
         GOTO      DONE IF NOT LESS
         SUB       C1,SCALER
.
DONE
         MOVE      SCALER,NWORK
         SUB       START,NWORK
         DIVIDE    c7 INTO NWORK
         MOVE      NWORK TO DECIMAL
         MOVE      DECIMAL TO NWORK
         MULT      c7,NWORK
         MOVE      NWORK TO NUM
         RETURN
.
.
. .............................................................................
. PICK - PICK OFF RECORD FOR PRINTING. FILE CREATED IS MDATEXX/TXT:DR1
PICK
         MOVE      C4 TO SELECT              *SET BRANCHING CONSTANT.
.
         DISPLAY   *P1:1,*ES,*P30:01,"MAIL DATE SCHEDULE":
                   *P28:04,"PICK OFF":
                   *P1:24,*EL,"Enter (*) to exit.";
.
         KEYIN     *P10:06,"ALL LISTS ? ",*uc,STR1,*lc;
         CMATCH    "*",STR1
         GOTO      MENU IF EQUAL
         CMATCH    YES,STR1
         GOTO      KYEAR IF EQUAL
.
.
PLIST1   DISPLAY   *P32:06,"LIST##: ";
         CALL      CARDREAD
         CMATCH    "*" TO STR1
         GOTO      MENU IF EQUAL         *EXIT
         MOVE      SCHKEY TO PLIST1
         GOTO      KYEAR
.
.
. ..............................
. PMONTH - SELECT MONTHS WANTED.
. .............................
PMONTH
         DISPLAY   *P1:24,*EL,"Enter (13) for all months";
         KEYIN     *P32:12,*EL,"MONTH: ",*JR,*ZF,NMM;
         COMPARE   C0 TO NMM
         GOTO      PMONTH IF EQUAL
         COMPARE   C1 TO NMM
         GOTO      PMONTH IF LESS
         COMPARE   "13" TO NMM
         GOTO      ALLMON IF EQUAL
         GOTO      PMONTH IF NOT LESS
         CALL      MMNAM
         DISPLAY   *P30:12,*EL,"SELECTED MONTH ",MMNAM;
         KEYIN     *P50:12,"OK? ",*uc,STR1,*lc,*P50:12,*EL;
         CMATCH    YES,STR1
         GOTO      PICKREAD IF EQUAL
         GOTO      PMONTH
.
. ALLMON - ALL MONTHS SELECTED FOR PICK.
. ......................................
.
ALLMON
         GOTO      MENU
.
PICKREAD
         MOVE      C1 TO FILENUM
FILENAME
         CLEAR     NEWNAME
         APPEND    "MDATE",NEWNAME
         MOVE      FILENUM TO F2
         REP       ZFILL,F2
         APPEND    F2,NEWNAME
         RESET     NEWNAME,8
         RESET     NEWNAME
         TRAP      GOODFILE IF IO
.         OPEN      RECMST,NEWNAME
         ADD       C1 TO FILENUM
         GOTO      FILENAME
GOODFILE
         TRAPCLR   IO
         CLEAR     RECNAME
         PACK      RECNAME FROM NEWNAME,EXT
.         PREPARE   RECMST,RECNAME
         DISPLAY   *P1:24,*EL,"The output file for this search is ":
                   NEWNAME;
         CLEAR     NSCHFLD
         CLEAR     NSCHFLD2
         CLEAR     NSCHFLD3
         MOVE      NMM,MM
         REP       ZFILL,MM
         MOVE      NYY,YY
         REP       ZFILL,YY
         PACK      NSCHFLD FROM K1,SCHKEY
         PACK      NSCHFLD2 FROM K2,YY
         PACK      NSCHFLD3 FROM K3,MM
.
           CALL        NSCHAIM
         GOTO      NOPICK IF OVER
         GOTO      PICKOUT
.
PREADKG    CALL         NSCHAIM  
. READKG    NSCHFILE;SCHKEY,SCHYEAR,SCHMONTH,SCHDAY,SCHMLR,SCHQUANT
         GOTO      PICKDONE IF OVER
         GOTO      PICKOUT
.
PICKOUT
         GOTO      PREADKG
.
PICKDONE
.         WEOF      RECMST,SEQ
.         CLOSE     RECMST
         KEYIN     *P1:24,*EL,"PICK OFF IS DONE ",*B,*B,STR1;
         GOTO      MENU
.
NOPICK
.NOPICK   WEOF      RECMST,SEQ
.         CLOSE     RECMST
         DISPLAY   *P1:24,*EL,"NO RECORDS FOUND!!!!!",*B,*B;
.         PREPARE   RECMST,RECNAME
.         CLOSE     RECMST
         GOTO      MENU
.
. PRINT - PRINT SELECTED OUTPUT FILE.
. ....................................
PRINT
         GOTO      MENU
.
*..............................................................................
.
. EXIT - CLOSE FILES STOP PROGRAM.
. ...............................
EXIT     CLOSE     NMDLFILE
         CLOSE     NSCHFILE
         STOP
IO       BEEP
         DISPLAY   *P1:24,*R,*EL,*HON,"ALL MASTER FILES NOT FOUND !!!",*HOFF;
         PAUSE     "5"
         GOTO      EXIT
         INCLUDE   NDATIO.inc
         INCLUDE   NPASIO.inc
         INCLUDE   NXRFIO.inc
           INCLUDE   NSCHIO.inc
           INCLUDE   NMDLIO.inc
.Patch2.9
          include   compio.inc
          include   cntio.inc
.          INCLUDE   NMLRIO.inc
.Patch2.9
         INCLUDE   LISTHELP.inc
         INCLUDE   COMLOGIC.inc
...............................................................................


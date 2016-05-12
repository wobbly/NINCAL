..............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.patch1.2
				include	compdd.inc
				include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch1.2
         INCLUDE   NORDDD.inc
.
release  init      "1.2"        DMB	26MAY2004	Mailer Conversion
.RELEASE  INIT      "1.1"        ASH  05JAN99  NINORD Y2K, File expansion
.RELEASE  INIT      "1.0"               JD31mar93 new.
LINES    FORM      2
PAGE     FORM      "000"
DATE     DIM       8
DATEBR   FORM      1
TYPBR    FORM      1
REVDATE  DIM       8
LSTSTRG  INIT      "011328-010957-012379-015015-000191-000081"
.
.Start Patch #1.1 - increased file size
.File size has not been updated in some time!!
.WRKFILE  FILE      VAR=312,COMP
WRKFILE  FILE      VAR=486,COMP
.End Patch #1.1 - increased file size
WRKNAME  DIM       25
WRKNAM1  INIT      "DISKIN66"
WRKFLD   DIM       9
.
OMITBRCH FORM      1         if TYPIST omits were requested,0=yes,1=no
REVPICK  DIM       112       chosen dates
REVPICK2 DIM       112       chosen dates only when mm/dd/yy is used
REVCOUNT FORM      2         screen line counter
YEAR     DIM       2         keyed in year and year from datmst
MO       DIM       2         keyed in month and month from datmst
MOYR     DIM       4         month & year from datmst
DAY      DIM       2         keyed in day and day from datmst
REVBRCH  FORM      1         used to determine if/how dates should
.                             be checked. 0=no,1=yy,2=mmyy,3=mmddyy
.V        FORM      2         vertical screen coordinate
H1       FORM      2         horizontal screen coordinate
H2       FORM      2                     "
H3       FORM      2                     "
H4       FORM      2                     "
H5       FORM      2                     "
.
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
RECJUL   FORM      5         julian order date from NINORD3
.
PICK     DIM       1         KEYED IN PICK OFF CHOICE.
NUMPICK  FORM      1         NUMERIC PICK OFF CHOICE
ANS      DIM       1
FIRSTN   INIT      "185570"  FIRST NIN LR OF 1993
..............................................................................
         MOVE      "NORD0033" TO PROGRAM
         MOVE      "Names in the News CA" TO COMPNME
         MOVE      "EPSILON WEEKLY RETURN-TO PICK" TO STITLE
         CALL       PAINT
         MOVE       "EXIT" TO PF5
         CALL       FUNCDISP
         TRAP       STOP IF F5
         MOVE      C1 TO NORDPATH
         CLOCK     DATE TO DATE
.         UNPACK    DATE INTO MM,DD,YY
.         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         PACK      WRKNAME FROM pdrive,WRKNAM1
.
.
CHOICE
         KEYIN    *P15:6,"Enter current week's days for records you want?:":
                   *P30:10,"(R)eturn Date?":
                   *P30:11,"(F)inished?":
                   *P19:13,"Choose one: ",PICK;
         CMATCH    "F",PICK
         GOTO      CHOICE IF EOS
         STOP      IF EQUAL
.
         MOVE      PICK,ANS
         REPLACE   "T1R2F3",ANS
         MOVE      C0 TO NUMPICK
         MOVE      ANS,NUMPICK
         COMPARE   C0 TO NUMPICK
         GOTO      CHOICE IF EQUAL
         COMPARE   "4" TO NUMPICK
         GOTO      CHOICE IF NOT LESS
         MOVE      C0 TO N6
         PACK      WRKNAME FROM pdrive,WRKNAM1
         PREPARE   WRKFILE,WRKNAME
.  RETURN DATE SELECTION ROUTINE
.   CAN SELECT BY YEAR, MONTH/YEAR OR MONTH/DAY/YEAR.
.   OTHER PICK-OFFS WILL GO TO DATESEC TO SEE IF THE USER WANTS A
.    SECONDARY DATE PICK-OFF.
.
DATEPICK DISPLAY   *P1:4,*EF,*P17:4,"***R E T U R N   D A T E   P I C K":
                   " - O F F***";
         GOTO      DATECHCE
DATESEC  MOVE      C0 TO REVBRCH
         KEYIN    *P1:24,*EL,*P15:24,"Do you want a secondary search by ":
                   "DATE? ",ANS;
         CMATCH    "Y",ANS
         GOTO      DATESEC IF EOS
         GOTO      DATESEC2 IF EQUAL
         CMATCH    "N",ANS
         GOTO      DATESEC IF NOT EQUAL
         GOTO      INPUT
DATESEC2 DISPLAY   *P8:5,"***S E C O N D A R Y   R E V I S E   D A T E   ":
                   "P I C K - O F F***";
DATECHCE KEYIN    *P1:11,*EF,"How would you like to choose the revise dates:":
                   *P49:11,"1. by YEAR?":
                   *P49:12,"2. by MONTH/YEAR?":
                   *P49:13,"3. by MONTH/DAY/YEAR?":
                   *P71:13,"Choose: ",REVBRCH;
         MOVE     C1 TO REVCOUNT
         MOVE      "14",V
         BRANCH    REVBRCH OF YRSEL,MOSEL,DYSEL
         GOTO      DATECHCE
.
. SELECT BY YEAR... 2 POSITION YEAR FIELDS ARE APPENDED TO REVPICK.
.
YRSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
YRROW    BRANCH    N1 OF YRROW1,YRROW2,YRROW3,YRROW4,ENDYR
YRROW1   MOVE      "03" TO H1
         MOVE      "16" TO H2
         GOTO      YRPICK
YRROW2   MOVE      "21" TO H1
         MOVE      "35" TO H2
         GOTO      YRPICK
YRROW3   MOVE      "40" TO H1
         MOVE      "54" TO H2
         GOTO      YRPICK
YRROW4   MOVE      "59" TO H1
         MOVE      "73" TO H2
YRPICK   KEYIN     *PH1:V,*DV,REVCOUNT,") 19",*ZF,*JR,YEAR;
         MATCH     "0*",YEAR
         GOTO      ENDYR IF EQUAL
         GOTO      YRPICK IF EOS
         TYPE      YEAR
         GOTO      YRPICK IF NOT EQUAL
YROK     KEYIN     *PH2:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      YRPICK IF EQUAL
         GOTO      YROK IF EOS
         CMATCH    "Y",ANS
         GOTO      YROK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    YEAR,REVPICK
         ADD      C1,V
         COMPARE   "21",V
         GOTO      YRPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      YRROW
ENDYR    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      INPUT
.
. SELECT BY MONTH/YEAR... 4 POSITION MONTH/YEAR FIELDS ARE APPENDED
.                         TO REVPICK.
.
MOSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
MOROW    BRANCH    N1 OF MOROW1,MOROW2,MOROW3,MOROW4,ENDMO
MOROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "16" TO H4
         GOTO      MOPICK
MOROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "35" TO H4
         GOTO      MOPICK
MOROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "47" TO H3
         MOVE      "54" TO H4
         GOTO      MOPICK
MOROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "73" TO H4
MOPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDMO IF EQUAL
         GOTO      MOPICK IF EOS
         TYPE      MO
         GOTO      MOPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      MOPICK IF EOS
         GOTO      MOPICK IF NOT EQUAL
MOOK     KEYIN     *PH4:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      MOPICK IF EQUAL
         GOTO      MOOK IF EOS
         CMATCH    "Y",ANS
         GOTO      MOOK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    MO,REVPICK
         APPEND    YEAR,REVPICK
         ADD      C1,V
         COMPARE   "21",V
         GOTO      MOPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      MOROW
ENDMO    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      INPUT
.
. SELECT BY MONTH/DAY/YEAR...  8 POSITION MO/DY/YR FIELDS ARE APPENDED
.                              TO REVPICK & REVPICK2 IF NEEDED.
.
DYSEL    CLEAR     REVPICK
         CLEAR     REVPICK2
         MOVE     C1 TO N1
DYROW    BRANCH    N1 OF DYROW1,DYROW2,DYROW3,DYROW4,ENDDY
DYROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "13" TO H4
         MOVE      "16" TO H5
         GOTO      DYPICK
DYROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "31" TO H4
         MOVE      "35" TO H5
         GOTO      DYPICK
DYROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "48" TO H3
         MOVE      "50" TO H4
         MOVE      "54" TO H5
         GOTO      DYPICK
DYROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "69" TO H4
         MOVE      "73" TO H5
DYPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /  /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR:
                   DAY,*PH4:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDDY IF EQUAL
         GOTO      DYPICK IF EOS
         TYPE      MO
         GOTO      DYPICK IF NOT EQUAL
         TYPE      DAY
         GOTO      DYPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      DYPICK IF NOT EQUAL
DYOK     KEYIN     *PH5:V,"OK?",ANS;
         CMATCH    "N",ANS
         GOTO      DYPICK IF EQUAL
         GOTO      DYOK IF EOS
         CMATCH    "Y",ANS
         GOTO      DYOK IF NOT EQUAL
         PACK      DATE WITH MO,SLASH,DAY,SLASH,YEAR
         BRANCH    N1 TO DYAPP,DYAPP,DYAPP2,DYAPP2
DYAPP    APPEND    DATE TO REVPICK
         GOTO      DYCOUNT
DYAPP2   APPEND    DATE TO REVPICK2
DYCOUNT  ADD      C1,REVCOUNT
         ADD      C1,V
         COMPARE   "21",V
         GOTO      DYPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      DYROW
ENDDY    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         MOVE      C1 TO NORDPATH
         MOVE      FIRSTN TO NORDFLD
INPUT    CALL      NORDKEY
         GOTO      DONE IF OVER
         ADD       C1 TO N5
         MATCH     "1102" TO ORTNNUM
         GOTO      SETDY IF NOT EQUAL
         DISPLAY   *P1:23,"RECORDS READ: ",N5;
SETDATE  BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MAJOR PICK-OFF BY DATE.
         GOTO      WRTPREP
*
. BY YEAR RETRIEVAL...
.
SETYR    RESET     REVPICK                       GET FP READY FOR SCAN.
         SCAN      ORTNDTEY IN REVPICK               IS IT IN THE STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         GOTO      INPUT                      NO.
*
. BY MONTH RETRIEVAL...
.
SETMO    RESET     REVPICK                       GET FP READY FOR SCAN.
         PACK      MOYR WITH ORTNDTEM,ORTNDTEY   SET UP SEARCH STRING.
         SCAN      MOYR IN REVPICK               IS IT IN THE STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         GOTO      INPUT                      NO.
*
. BY DAY RETRIEVAL...
.
SETDY    CALL      NORDKS
         GOTO      DONE IF OVER
         ADD       C1 TO N5
         DISPLAY   *P1:23,"RECORDS READ: ",N5;
         MATCH     "1102" TO ORTNNUM
         GOTO      SETDY IF NOT EQUAL
         RESET     LSTSTRG
         SCAN      OLNUM IN LSTSTRG
         GOTO      SETDY IF NOT EQUAL
         RESET     REVPICK                       GET FP READY FOR SCAN.
         RESET     REVPICK2                               "
         PACK      REVDATE FROM ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEY
         SCAN      REVDATE IN REVPICK            IS IT IN 1ST STRING?
         GOTO      WRTPREP IF EQUAL             YES.
         SCAN      REVDATE IN REVPICK2           NO. HOWBOUT 2ND STRING?
         GOTO      WRTPREP IF EQUAL                 YES.
         GOTO      SETDY
*
.
WRTPREP 
         CALL      WRTWRK
         ADD       C1 TO N6
         DISPLAY   *P25:23,"RECORDS SELECTED ",N6;
         GOTO      SETDY
..............................................................................
WRTWRK   WRITE     WRKFILE,SEQ;ORDVARS
         RETURN
.
DONE     WEOF      WRKFILE,SEQ
         CLOSE     WRKFILE,EOFSIZE
         STOP
STOP 
         STOP
.
.patch1.2
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.2
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


. .............................................................................
. 
. *****INCLUSION NINDATE -- SUBROUTINES TO ENTER END EDIT DATES.
. 
. .............................................................................
. 
. SUBROUTINE DESCRIPTIONS.
. 
.      KEYDATE           KEY A DATE IN MMDDYY FORMAT AND EDIT CHECK IT.
.      DAYSMO            DETERMINE THE NO OF DAYS IN THE MONTH.
.      LEAPYR            DETERMINE IF THE YEAR IS A LEAP YEAR.
.      ANDAT             ANALYZE THE DATE INTO COMPONENTS MM,DD,YY.
.      SYNDAT            COMBINE MM,DD,YY INTO A SINGLE COMPONENT.
.      YYMMDD            CONVERT MMDDYY INTO YYMMDD FORMAT.
.      MMNAM             CREATE MONTH NAME FROM MM.
.      NEXMON            CHANGE MM,CC,YY TO NEXT MONTH.
.      NEXYR             CHANGE CC,YY TO NEXT YEAR.
.      LASMON            CHANGE MM,CC,YY TO LAST MONTH.
.      LASYR             CHANGE CC,YY TO LAST YEAR.
. .............................................................................
.  
. VARIABLES REQUIRED.
. 
.     H        FORM 2    HORIZONTAL SCREEN POSITION
.     V        FORM 2    VERTICAL SCREEN POSITION
.     HTEMP    FORM 2    CALC VAR FOR HORIZONTAL  SCREEN POSITION.
.     MESSG1   FORM 2    VERTICAL SCREEN POSITION FOR MESSAGES.
. 
.     OTHER VARIABLES INCLUDED IN NINVDATE/TXT .
. 
. ............................................................................
. 
+ 
. .............................................................................
. *****KEYDATE -- KEYIN AND EDIT DATE.
. 
KEYDATE
         MOVE      "00",NMM
         MOVE      "00",NDD
         MOVE      "00",NYY
         MOVE      "N",KEYDAT      **VALID DATE NOT INPUT YET
         KEYIN     *PH:V,"MMDDYY",*PH:V,DATE
         RESET     DATE,0
         BUMP      DATE
         RETURN    IF EOS           **IF 'ENTER' ONLY RETURN WITH KEYDAT = 'N'
         RESET     DATE,6
         GOTO      KEYD90 IF EOS    ** VALID DATE MUST HAVE 6 BYTES.
         RESET     DATE
         TYPE      DATE             ** MUST BE NUMERIC.
         GOTO      KEYD90 IF NOT EQUAL
         CALL      ANDAT
         CALL      DAYSMO
         COMPARE   "1",NMM
         GOTO      KEYD90 IF LESS
         COMPARE   "13",NMM
         GOTO      KEYD90 IF NOT LESS
         COMPARE   "1",NDD
         GOTO      KEYD90 IF LESS
         COMPARE   NDD,DAYSMO
         GOTO      KEYD90 IF LESS
         MOVE      "Y",KEYDAT       ** VALID DATE. RETURN.
         RETURN
KEYD90
         BEEP                        ** DATE NO GOOD.
         GOTO      KEYDATE
. .............................................................................
+ 
. .............................................................................
. 
. ** DAYSMO
. 
DAYSMO
         MOVE      "0",DAYSMO
         BRANCH    NMM OF DAY31,DAYFEB,DAY31,DAY30,DAY31,DAY30:
                   DAY31,DAY31,DAY30,DAY31,DAY30,DAY31
         RETURN
DAY31    MOVE      "31",DAYSMO
         RETURN
DAY30    MOVE      "30",DAYSMO
         RETURN
DAYFEB   CALL      LEAPYR
         MATCH     "Y",LEAPYR
         GOTO      DAY29 IF EQUAL
         GOTO      DAY28
DAY29    MOVE      "29",DAYSMO
         RETURN
DAY28    MOVE      "28",DAYSMO
         RETURN
. 
. .............................................................................
+ 
. .............................................................................
. 
. **LEAPYR
. 
LEAPYR
         COMPARE   "0",NYY           IS THIS A CENTURY YEAR ?
         GOTO      LEAP20 IF EQUAL
LEAP10   MOVE      NYY,DATEM1        NOT A CENTURY YEAR TEST YY.
         GOTO      LEAP30
LEAP20   MOVE      CC,DATEM1         A CENTURY YEAR TEST CENTURY.
         GOTO      LEAP30
LEAP30   MOVE      DATEM1,DATEM2     DIVIDE BY FOUR AND COMPUTE REMAINDER.
         DIV       "4",DATEM1
         MULT      "4",DATEM1
         SUB       DATEM1,DATEM2
         COMPARE   "0",DATEM2        IS THE REMAINDER ZERO?
         GOTO      LEAP50 IF EQUAL
. 
LEAP40   MOVE      "N",LEAPYR        NOT A LEAP YEAR
         GOTO      LEAPXX
LEAP50   MOVE      "Y",LEAPYR        LEAP YEAR
.  
LEAPXX   RETURN
. .............................................................................
+ 
. .............................................................................
. 
. ** ANDAT -- ANALYZE DATE INTO NMM, NDD, NYY.
. 
ANDAT
         MOVE      "00",NMM        IF DATE NOT NUMERIC VARIABLE HOLDS ZERO
         MOVE      "00",NDD        IF DATE NOT NUMERIC VARIABLE HOLDS ZERO
         MOVE      "00",NYY        IF DATE NOT NUMERIC VARIABLE HOLDS ZERO
         MOVE      MM,NMM
         MOVE      DD,NDD
         MOVE      YY,NYY
         RETURN
. .............................................................................
. 
+ 
. .............................................................................
. 
. ** SYNDAT -- SYNTHESIZE DATE FROM COMPONENTS MM,DD,YY.
. 
SYNDAT
         CLEAR     DATE
         MOVE      NMM,MM
         MOVE      NDD,DD
         MOVE      NYY,YY
         PACK      DATE FROM MM,DD,YY
         RETURN
. .............................................................................
+ 
. .............................................................................
. 
. ** YYMMDD -- REVERSE DATE FROM MMDDYY TO YYMMDD FORMAT.
. 
YYMMDD
         UNPACK    DATE TO MM,DD,YY
         PACK      DATE FROM YY,MM,DD
         RETURN
. 
. .............................................................................
+ 
. .............................................................................
. 
. ** MMNAM -- CREATE MONTH NAME 
. 
MMNAM
         BRANCH    NMM OF MMNAM01,MMNAM02,MMNAM03,MMNAM04,MMNAM05,MMNAM06:
                   MMNAM07,MMNAM08,MMNAM09,MMNAM10,MMNAM11,MMNAM12
         MOVE      "   ",MMNAM
         RETURN
MMNAM01  MOVE      "JAN",MMNAM
         RETURN
MMNAM02  MOVE      "FEB",MMNAM
         RETURN
MMNAM03  MOVE      "MAR",MMNAM
         RETURN
MMNAM04  MOVE      "APR",MMNAM
         RETURN
MMNAM05  MOVE      "MAY",MMNAM
         RETURN
MMNAM06  MOVE      "JUN",MMNAM
         RETURN
MMNAM07  MOVE      "JUL",MMNAM
         RETURN
MMNAM08  MOVE      "AUG",MMNAM
         RETURN
MMNAM09  MOVE      "SEP",MMNAM
         RETURN
MMNAM10  MOVE      "OCT",MMNAM
         RETURN
MMNAM11  MOVE      "NOV",MMNAM
         RETURN
MMNAM12  MOVE      "DEC",MMNAM
         RETURN
. .............................................................................
+ 
. .............................................................................
. 
. ** NEXMON
. 
NEXMON
         COMPARE   "12",NMM
         GOTO      NEXM10 IF LESS
         CALL      NEXYR
         MOVE      "00",NMM
NEXM10   ADD       "1",NMM
         RETURN
. 
. .............................................................................
. 
. ** NEXYR
. 
NEXYR
         COMPARE   "99",NYY
         GOTO      NEXY10 IF LESS
         ADD       "1",CC
NEXY10
         ADD       "1",NYY
         RETURN
. .............................................................................
. 
. ** LASMON
. 
LASMON   COMPARE   "2",NMM
         GOTO      LASM10 IF NOT LESS
         CALL      LASYR
         MOVE      "13",NMM
LASM10   SUB       "1",NMM
         RETURN
. 
. .............................................................................
. 
. ** LASYR
LASYR    COMPARE   "00",NYY
         GOTO      LASY10 IF EQUAL
         SUB       "1",CC
         MOVE      "99",NYY
         GOTO      LASYXX
LASY10   SUB       "1",NYY
LASYXX   RETURN
. .............................................................................
. **** END OF INCLUSION NINDATE
. .............................................................................


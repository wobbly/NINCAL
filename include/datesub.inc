***************************************************************************
*                         COMPUTER TRANSACTION SYSTEMS                    *
*                                   PO Box 56                             *
*                               15 Bayview Street                         *
*                            North Weymouth, MA  02191                    *
*                             (617) 331-6968  Office                      *
*                             (617) 331-6969  Fax                         *
***************************************************************************

. DATESUB.INC  -  SUBROUTINE TO GET THE TIME, DATES AND JULIAN
. -----------
.
. "GETDATE" will CLOCK the TIMESTAMP to THECDATE variable (ccyymmdd)
.  =======  
.
. "EXPDATE" is used to Expand a Selected Date in the following Formats
.  =======  the Variable THECDATE must be stored with the date 
.           in Format (ccyymmdd).  This routine also performs a Date 
.           Validation and returns DATERR=1 if Invalid or DATERR=0 if OK.
.           
. 
. Variables on Exit from GETDATE, EXPDATE and JULDATE
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.
.                 VARIABLE     FORMAT              DATA TYPE
.                 ========     ======              =========  
.                 DD           "dd"                   FORM     
.                 MM           "mm"                   FORM
.                 CC           "cc"                   FORM
.                 YY           "yy"                   FORM
.
.                 MMM          "mm"                   DIM
.                 DDD          "dd"                   DIM      
.                 CCC          "cc"                   DIM
.                 YYY          "yy"                   DIM
.
.                 YYYY         "ccyy"                 FORM
.                 CCYY         "ccyy"                 DIM
.
.                 MMDDYY       "mmddyy"               DIM
.                 MMDDYYYY     "mmddccyy"             DIM

.                 TODAY        "mm-dd-yy"             DIM      GETDATE Only
.                 TODAYC       "mm-dd-ccyy"           DIM      GETDATE Only
.                 TODAYA       "January 01,1999"      DIM      GETDATE Only
.
.                 THECDATE     "ccyymmdd"             DIM
.                 THEDATE      "mm-dd-yy"             DIM      EXPDATE Only
.                 THEDATEC     "mm-dd-ccyy"           DIM      EXPDATE Only
.                 THEDATEA     "January 01,1999"      DIM      EXPDATE Only
.                 DAYNUM       "n"                    FORM     
.                 DAYNAME      "dayofweek"            DIM      
.                 MTHNAME      "monthname"            DIM      
.
.                 CDAY         "nnn"                  DIM      
.                 CYEAR        "yyyy"                 DIM      
.
.                 JDAY         "nnn"                  DIM      
.                 JULIAN       "yyyynnn"              DIM      
.
.                 TIME         "hh:mm:ss"             DIM      GETDATE Only
.                 THETIME      "hh:mm:ss"             DIM      EXPDATE Only
.
.                 DATERR       0 =Date Valid          FORM
.                              1 =Date Invalid
...............................................................................
. To get a Calendar Date from a Julian Date, use one the following Routine.
. The resulting Output will be the same Variables from the List above.
.
.        Routine           Input Field       Input Format 
.        =======           ============      ============ 
.        JULDATE           JULIAN            yyyyddd 
.
.
...............................................................................
. To get a Julian Date for a Specific Date, use one of the following Routines.
.
.        Routine           Input Format      Output Format
.        =======           ============      =============
.        REG               mm-dd-ccyy        ccyyddd
.        REGDIM8           mmddccyy          ccyyddd
.        REGDIM8C          ccyymmdd          ccyyddd
.
.
...............................................................................
. Julian Date Range Calculations - Number of Days between 2 Julian Dates
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.
.    NOTE: Before using JULDAYS the Variables FROMJDAT & THRUJDAT must
.    ~~~~  have a Valid Julian date calculated with the REG, REGDIM8
.          or REGDIM8C routines and moving variable JULIAN into them.
.
.
.        Routine      Input Field   Format       Output Field  Format
.        =======      ===========   =======      ============  ====== 
.        JULDAYS      FROMJDAT      ccyyddd      TOTJDAYS      ddddd
.                     THRUJDAT      ccyyddd 
.
...............................................................................



*******************************************************************************
. PUT THE CURRENT DATE INTO DATE VARIABLES
.
GETDATE
         CLOCK    TIMESTAMP,THECDATE         ...Format = ccyymmdd         
GETDATE1
         UNPACK   THECDATE INTO CCYY,MMM,DDD
         UNPACK   CCYY INTO CC,YY
         UNPACK   CCYY INTO CCC,YYY
         MOVE     CCYY,YYYY
         MOVE     CCYY,CYEAR
         MOVE     MMM,MM
         MOVE     DDD,DD
         PACK     MMDDYY WITH MMM,DDD,YYY
         PACK     MMDDYYYY WITH MMM,DDD,YYYY

         CLOCK    DAY TO CDAY                ...Julian Day
         MOVE     CDAY,JDAY

         CLOCK    WEEKDAY,WORK01
         MOVE     WORK01,DAYNUM
         LOAD     DAYNAME FROM DAYNUM OF SUN,MON,TUE,WED,THU,FRI,SAT

         LOAD     MTHNAME FROM MM OF JANMTH,FEBMTH,MARMTH,APRMTH,MAYMTH,JUNMTH:
                                     JULMTH,AUGMTH,SEPMTH,OCTMTH,NOVMTH,DECMTH

         PACK     JULIAN WITH CCYY,CDAY

         PACK     TODAY WITH MMM,HYP,DDD,HYP,YYY
         PACK     TODAYC WITH MMM,HYP,DDD,HYP,CCYY
         PACK     TODAYA WITH MTHNAME," ",DDD,",",CCYY

         CLOCK    TIME,TIME

         MOVE     ZERO,DATERR                ...DATE in Correct Format
         RETURN
 

*******************************************************************************
. EXPRESS A SELECTED DATE INTO DATE VARIABLES
.
EXPDATED
         MOVE     ONE,EXPSWT        ...for Display Rtn's ONLY - Ignore any Date
         GOTO     EXPDATE0          ...Range Error when reading date from file.

EXPDATE
         MOVE     ZERO,EXPSWT       ...Clear Swt to allow Verify of Date Ranges

EXPDATE0
         REPLACE  " 0",THECDATE
         UNPACK   THECDATE INTO YYYY,MMM,DDD
         RESET    THECDATE

         MATCH    "000000",THECDATE          ...Do NOT add Century to Zero Date
         GOTO     EXPDATE2 IF EQUAL

         MATCH    "19000000",THECDATE        ...Do NOT add Century to Zero Date
         IF       EQUAL
                  MOVE     BLANKS,THECDATE
                  GOTO     EXPDATE0
         ENDIF

         IF       (YYYY>=50 & YYYY<=99)      ...Calc Century for Yrs 1950-1999
                  ADD      "1900",YYYY
         ENDIF

         IF       (YYYY>=0 & YYYY<=99)       ...Calc Century for Yrs 2000-2089
                  ADD      "2000",YYYY
         ENDIF
EXPDATE2
         PACK     THECDATE WITH YYYY,MMM,DDD ...Re-Build with Century & Year
         REPLACE  " 0",THECDATE
EXPDATE3
         MOVE     YYYY,CCYY
         REPLACE  " 0",CCYY
         UNPACK   CCYY INTO CC,YY
         RESET    CCYY

         UNPACK   CCYY INTO CCC,YYY
         RESET    CCYY

         MOVE     CCYY,CYEAR
         MOVE     CCYY,YYYY
         MOVE     MMM,MM
         MOVE     DDD,DD

         CALL     REGDIM8C                   ...Calc Julian Day (JDAY)
.                                            ...Calc Julian Date(JULIAN)
         CALL     DAYOFWK
         MOVE     TJULDAYS,DAYNUM
         LOAD     DAYNAME FROM DAYNUM OF SUN,MON,TUE,WED,THU,FRI,SAT

         LOAD     MTHNAME FROM MM OF JANMTH,FEBMTH,MARMTH,APRMTH,MAYMTH,JUNMTH:
                                     JULMTH,AUGMTH,SEPMTH,OCTMTH,NOVMTH,DECMTH

         LOAD     MTHDAYS FROM MM OF JANDAY,FEBDAY,MARDAY,APRDAY,MAYDAY,JUNDAY:
                                     JULDAY,AUGDAY,SEPDAY,OCTDAY,NOVDAY,DECDAY

         UNPACK   JULIAN INTO CYEAR,CDAY

         PACK     MMDDYY WITH MMM,DDD,YYY
         PACK     MMDDYYYY WITH MMM,DDD,CCC,YYY

         PACK     THEDATE WITH MMM,HYP,DDD,HYP,YYY
         PACK     THEDATEC WITH MMM,HYP,DDD,HYP,CCYY
         PACK     THEDATEA WITH MTHNAME," ",DDD,",",CCYY

         CLOCK    TIME,THETIME

         MATCH    "00000000",THECDATE
         IF       EQUAL
                  MOVE     BLANKS,THECDATE
                  GOTO     EXPDATEX
         ENDIF

         MATCH    "19000000",THECDATE
         IF       EQUAL
                  MOVE     BLANKS,THECDATE
                  REPLACE  " 0",THECDATE
                  GOTO     EXPDATEX
         ENDIF

         IF       (MM<1 | MM>12)             ...Validate Month
                  GOTO     EXPDATER
         ENDIF

         IF       (DD<1 | DD>MTHDAYS)        ...Validate Day
                  GOTO     EXPDATER
         ENDIF

         IF       (YYYY<1950 | YYYY>2099)    ...Validate Year
                  GOTO     EXPDATER
         ENDIF

EXPDATEX
         UNPACK   ZERO INTO DATERR,EXPSWT    ...DATE in Correct Format
         RETURN                              ...Reset Swt's

EXPDATER
         BRANCH   EXPSWT OF EXPDATEX
         MOVE     ONE,DATERR                 ...DATE Incorrect
         DISPLAY  *P1:23,*EL,*DION,"Date ",MMM,HYP,DDD,HYP,CCYY:
         "  is Not within Systems Valid Date Range - Please Re-Enter":
                  *DIOFF,*B,*W3,*C,*EL;
         RETURN

*******************************************************************************
. REG DATE TO JULIAN ROUTINE  from Date Format "mm-dd-ccyy"
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REG   
         REPLACE   " 0" IN INVDATE
         UNPACK    INVDATE INTO HLDMTH,WORK01,HLDDAY,WORK01,HLDYR

         GOTO      REG05
*................................................................
. REG DIM8 DATE TO JULIAN ROUTINE  from Date Format "mmddccyy"
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REGDIM8
         REPLACE   " 0" IN MMDDYYYY
         UNPACK    MMDDYYYY INTO HLDMTH,HLDDAY,HLDYR

         GOTO      REG05
*................................................................
. REG DIM6 DATE TO JULIAN ROUTINE  from Date Format "ccyymmdd"
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
REGDIM8C
         REPLACE   " 0" IN THECDATE
         UNPACK    THECDATE INTO HLDYR,HLDMTH,HLDDAY

*................................................................
. CHECK FOR LEAP YEAR
. ~~~~~~~~~~~~~~~~~~~
REG05
         IF       (HLDMTH<>"02")
                  GOTO     REG10
         ENDIF

         MOVE      "28" TO NFEB
         MOVE      HLDDAY TO FORM02

         MOVE      HLDYR,S09V02
         DIVIDE    "4" INTO S09V02
         MOVE      S09V02 TO S00V02

         IF       (S00V02=ZERO)
                  MOVE     "29",NFEB
         ENDIF

         IF       (FORM02>NFEB) 
                  NORETURN
                  GOTO     EXPDATER
         ENDIF
*................................................................
. CONVERT TO JULIAN
. ~~~~~~~~~~~~~~~~~
REG10  
         MOVE      ZERO TO JDAY
         MOVE      ONE TO MM
         MOVE      HLDMTH TO NDAY
REG20  
         COMPARE   MM TO NDAY
         GOTO      REG30 IF EQUAL
         LOAD      FORM02 FROM MM OF N31,NFEB,N31,N30,N31,N30,N31,N31:
                   N30,N31,N30,N31
         ADD       FORM02 TO JDAY
         ADD       ONE TO MM
         GOTO      REG20
REG30  
         MOVE      HLDDAY TO NDAY
         ADD       NDAY TO JDAY
 
         PACK      JULIAN WITH HLDYR,JDAY
         REPLACE   " 0" IN JULIAN

         RETURN

*******************************************************************************
. Convert Julian Date into a Calandar Date 
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

JULDATE 
         UNPACK    JULIAN INTO CYEAR,CDAY
         MOVE      CDAY TO JDAY
         MOVE      ZERO TO MM        
         MOVE      CYEAR TO YYYY
*
         COMPARE   ONE TO JDAY       
         RETURN    IF LESS
*................................................................
. MAKE SURE FEBRUARY IS HANDLED PROPERLY ON LEAP YEARS
.
         MOVE      ZERO,MM
         MOVE      "28" TO NFEB
         MOVE      YYYY TO S09V02
         DIVIDE    "4" INTO S09V02
         MOVE      S09V02 TO S00V02
         COMPARE   ZERO TO S00V02
         GOTO      MDLOOP IF NOT EQUAL       ...NO, LEAVE NFEB = 28.
         MOVE      "29" TO NFEB              ...YES, SET NFEB = 29.
*................................................................
. COMPUTE THE MONTH
.
MDLOOP  
         ADD       ONE TO MM
         LOAD      FORM02 FROM MM OF N31,NFEB,N31:   JAN/FEB/MAR
                                      N30,N31,N30:    APR/MAY/JUN
                                      N31,N31,N30:    JUL/AUG/SEP
                                      N31,N30,N31     OCT/NOV/DEC
         SUBTRACT  FORM02 FROM JDAY
         GOTO      MDL1 IF EQUAL       ...SUBTRACT DAYS OF THE MONTH
         GOTO      MDLOOP IF NOT LESS  ...UNTIL MONTH FOUND
MDL1    
         ADD       FORM02 TO JDAY      ...UNBIAS FROM LAST SUBTRACT
         MOVE      JDAY TO DD          ...TO GET DAY OF THE MONTH
*................................................................
. PUT THE DATE INTO mm/dd/yy FORMAT
.
         PACK      THECDATE WITH YYYY,MM,DD
         REPLACE   " 0",THECDATE
         CALL      EXPDATE

         RETURN

*******************************************************************************
. CALCULATE DAYS BETWEEN JULIAN DATES
. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
JULDAYS
         MOVE      FROMJDAT,JULIAN
         UNPACK    JULIAN INTO FROMJYR,FROMJDAY
         MOVE      FROMJYR,FJULYEAR
         MOVE      FROMJDAY,FJULDAYS

         MOVE      THRUJDAT,JULIAN
         UNPACK    JULIAN INTO THRUJYR,THRUJDAY
         MOVE      THRUJYR,TJULYEAR
         MOVE      THRUJDAY,TJULDAYS

         SUB       FJULYEAR,TJULYEAR
         COMPARE   ZERO,TJULYEAR
         GOTO      JULDAYS5 IF EQUAL

         CALC      TOTJDAYS=(((365*TJULYEAR)-FJULDAYS+TJULDAYS))
         GOTO      JULDAYSX
JULDAYS5
         CALC      TOTJDAYS=(THRUJDAT-FROMJDAT+1)
JULDAYSX
         RETURN

*******************************************************************************
. WHAT DAY OF THE WEEK IS IT
. ~~~~~~~~~~~~~~~~~~~~~~~~~~
.      FORMULA IS AS FOLLOWS:
.      365.25 * YY = XY
.      DDD + XY = XY
.      IF TWO DECIMAL POSITIONS = ZERO,
.             SUBTRACT 1 FROM XY
.
. XY (WHOLE NUMBER ONLY) = MOD(7)
. REMAINDER INDICATES DAY OF WEEK ..
. SUNDAY = 0, SATURDAY = 6
..........................................

DAYOFWK 
         MOVE      YY TO S09V02
         COMPARE   ZERO TO S09V02
         GOTO      DAY10 IF NOT LESS
         MOVE      ZERO TO TJULDAYS
         RETURN
DAY10   
         MOVE      JDAY TO TJULDAYS
         COMPARE   ZERO TO TJULDAYS
         RETURN    IF EQUAL
         GOTO      DAY20 IF NOT LESS
         MOVE      ZERO TO TJULDAYS
         RETURN
DAY20   
         COMPARE   "367" TO TJULDAYS
         GOTO      FSTDAY IF LESS
         MOVE      ZERO TO TJULDAYS
         RETURN
........................................
FSTDAY   
         MOVE      YY TO S09V02
         MULT      "365.25" TO S09V02

         ADD       TJULDAYS TO S09V02        ...ADD DAYS TO SUM OF YEARS
         MOVE      S09V02 TO S00V02          ...TRUNCATE TO DECIMAL

         COMPARE   ZERO TO S00V02            ...FRACTIONS = ZERO?
         GOTO      SUBFRAC IF NOT EQUAL      ... YES ...
         SUB       ONE FROM S09V02           ... SO SUBTRACT 1
SUBFRAC  
         SUB       S00V02 FROM S09V02      REDUCE TO WHOLE NUMBER
*..............................................................
. WE HAVE NO MODULO-7 ROUTINE, AND DIVIDING GIVES FUNNY RESULTS, SO
. SET UP A SUBTRACTIVE LOOP TO PREVENT FRACTIONAL RESULTS
.
.SUB70006 COMPARE   "7000000" TO S09V02
.         GOTO      SUB70005 IF LESS
.         SUB       "7000000",S09V02
.         GOTO      SUB70006
.SUB70005 COMPARE   "700000" TO S09V02
.         GOTO      SUB70000 IF LESS
.         SUB       "700000",S09V02
.         GOTO      SUB70005
SUB70000
         COMPARE   "70000" TO S09V02
         GOTO      SUB7000 IF LESS
         SUB       "70000",S09V02
         GOTO      SUB70000
SUB7000 
         COMPARE   "7000" TO S09V02
         GOTO      SUB700 IF LESS
         SUB       "7000",S09V02
         GOTO      SUB7000
SUB700  
         COMPARE   "700" TO S09V02
         GOTO      SUB70 IF LESS
         SUB       "700",S09V02
         GOTO      SUB700
SUB70   
         COMPARE   "70" TO S09V02
         GOTO      SUB7 IF LESS
         SUB       "70",S09V02
         GOTO      SUB70
SUB7    
         COMPARE   "7" TO S09V02
         GOTO      SUBOUT IF LESS
         SUB       "7",S09V02
         GOTO      SUB7
SUBOUT  
         MOVE      S09V02 TO TJULDAYS    
         ADD       ONE TO TJULDAYS
         COMPARE   "51",YY
         GOTO      SUBOUT1 IF NOT LESS
         SUB       ONE,TJULDAYS
         COMPARE   ZERO,TJULDAYS
         GOTO      SUBOUT1 IF NOT EQUAL
         MOVE      "7",TJULDAYS
SUBOUT1
         LOAD      DAYNAME FROM TJULDAYS OF SUN,MON,TUE,WED,THU,FRI,SAT
         RETURN
*..............................................................
.        ERROR    =0
.        SUNDAY   =1
.        MONDAY   =2
.        TUESDAY  =3
.        WEDNESDAY=4
.        THURSDAY =5
.        FRIDAY   =6
.        SATURDAY =7

*..............................................................
. END OF SUBROUTINE
...............................................................



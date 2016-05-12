PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INC       NPAYDD.inc
         INC       HP.inc
RELEASE  INIT      "2.1"       ASH   22MAY98   YEAR 2000 - Will print century
.RELEASE  INIT     "2.0"       DLH   29APR92   NPAYXX, CONS, COMLOGIC.
.
SYSDATE  DIM       8
.Start Patch 2.1 - added var
SYSDATE1 DIM       10
.End Patch 2.1 - added var
DATEMASK INIT      "99/99/99"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
ZERO     FORM      "0"
ONE      FORM      "1"
FOUR     FORM      "4"
TEN      FORM      "10"
COUNT    FORM      5
TABLE    FORM      1
.
RTNUM1   DIM       4      1-4    PAY-TO NUMBER. **KEY**
RTNUM1A  DIM       1
.Start patch 2.1 - increased length
.RTCNTCT1 DIM       25     5-29   PAY-TO CONTACT NAME.
.RTCOMP1  DIM       25    30-54   PAY-TO COMPANY NAME
RTCNTCT1 DIM       45     5-29   PAY-TO CONTACT NAME.
RTCOMP1  DIM       45    30-54   PAY-TO COMPANY NAME.
.End patch 2.1 - increased length
RTADDR1  DIM       25    55-79   PAY-TO ADDRESS
RTCITY1  DIM       15    80-94   PAY-TO CITY.
RTSTATE1 DIM       2     95-96   PAY-TO STATE.
RTZIP1   DIM       10    97-106  PAY-TO ZIP. LEFT JUST.
RTNAME1  DIM       10   107-116  PASSWORD NAME
.Start Patch 2.1 - increased length
.RTREVDT1 DIM       6    117-122  REVISED DATE.
RTREVDT1 DIM       8    117-122  REVISED DATE.
.End Patch 2.1 - increased length
. 
RTNUM2   DIM       4      1-4    PAY-TO NUMBER. **KEY**
RTNUM2A  DIM       1
.Start patch 2.1 - increased length
.RTCNTCT2 DIM       25     5-29   PAY-TO CONTACT NAME.
.RTCOMP2  DIM       25    30-54   PAY-TO COMPANY NAME
RTCNTCT2 DIM       45     5-29   PAY-TO CONTACT NAME.
RTCOMP2  DIM       45    30-54   PAY-TO COMPANY NAME.
.End patch 2.1 - increased length
RTADDR2  DIM       25    55-79   PAY-TO ADDRESS
RTCITY2  DIM       15    80-94   PAY-TO CITY.
RTSTATE2 DIM       2     95-96   PAY-TO STATE.
RTZIP2   DIM       10    97-106  PAY-TO ZIP. LEFT JUST.
RTNAME2  DIM       10   107-116  PASSWORD NAME
.Start Patch 2.1 - increased length
.RTREVDT2 DIM       6    117-122  REVISED DATE
RTREVDT2 DIM       8    117-122  REVISED DATE
.End Patch 2.1 - increased length
. 
RTNUM3   DIM       4      1-4    PAY-TO NUMBER. **KEY**
RTNUM3A  DIM       1
.Start patch 2.1 - increased length
.RTCNTCT3 DIM       25     5-29   PAY-TO CONTACT NAME.
.RTCOMP3  DIM       25    30-54   PAY-TO COMPANY NAME.
RTCNTCT3 DIM       45     5-29   PAY-TO CONTACT NAME.
RTCOMP3  DIM       45    30-54   PAY-TO COMPANY NAME.
.End patch 2.1 - increased length
RTADDR3  DIM       25    55-79   PAY-TO ADDRESS
RTCITY3  DIM       15    80-94   PAY-TO CITY.
RTSTATE3 DIM       2     95-96   PAY-TO STATE.
RTZIP3   DIM       10    97-106  PAY-TO ZIP. LEFT JUST.
RTNAME3  DIM       10   107-116  PASSWORD NAME
.Start Patch 2.1 - increased length
.RTREVDT3 DIM       6    117-122  REVISED DATE
RTREVDT3 DIM       8    117-122  REVISED DATE
.End Patch 2.1 - increased length
. 
RTNUM4   DIM       4      1-4    PAY-TO NUMBER. **KEY**
RTNUM4A  DIM       1
.Start patch 2.1 - increased length
.RTCNTCT4 DIM       25     5-29   PAY-TO CONTACT NAME.
.RTCOMP4  DIM       25    30-54   PAY-TO COMPANY NAME.
RTCNTCT4 DIM       45     5-29   PAY-TO CONTACT NAME.
RTCOMP4  DIM       45    30-54   PAY-TO COMPANY NAME.
.End patch 2.1 - increased length
RTADDR4  DIM       25    55-79   PAY-TO ADDRESS
RTCITY4  DIM       15    80-94   PAY-TO CITY.
RTSTATE4 DIM       2     95-96   PAY-TO STATE.
RTZIP4   DIM       10    97-106  PAY-TO ZIP. LEFT JUST.
RTNAME4  DIM       10   107-116  PASSWORD NAME
.Start Patch 2.1 - increased length
.RTREVDT4 DIM       6    117-122  REVISED DATE
RTREVDT4 DIM       8    117-122  REVISED DATE
.End Patch 2.1 - increased length
. 
.Start Patch 2.1 - increased length
.RTDATE1  DIM       8
.RTDATE2  DIM       8
.RTDATE3  DIM       8
.RTDATE4  DIM       8
RTDATE1  DIM       10
RTDATE2  DIM       10
RTDATE3  DIM       10
RTDATE4  DIM       10
.End Patch 2.1 - increased length
.
         IFNZ      PC
         OPEN      NPAYFLE3,"NINPAY/SORT",EXCLUSIVE
         XIF
         IFZ       PC
         OPEN      NPAYFLE3,"NINPAY.SRT",EXCLUSIVE
         XIF
.
         MOVE      C3 TO NPAYPATH
         MOVE      C1 TO NPAYFLG3
         CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      DATEMASK TO SYSDATE
         EDIT      DATE TO SYSDATE
         XIF
         IFZ      PC
         MOVE     DATE TO SYSDATE
. Start Patch 2.1
         UNPACK   SYSDATE TO MM,STR1,DD,STR1,YY
         PACK     SYSDATE1 WITH MM,STR1,DD,STR1,CC,YY
. End Patch 2.1         
         XIF
         MOVE     SYSDATE TO TODAY
         MOVE     "NPay0002" TO PROGRAM
         MOVE      "NINCAL" TO PROGRAM
         MOVE     "MASTER PAY-TO PRINT" TO STITLE
         CMATCH    B1 TO PRTNAME
         IF        EOS
         MOVE      "LOCAL" TO PRTNAME
         ELSE
         IFZ       PC
         pack      prtfile from pdrive,prtname
         SPLOPEN   PRTfile
         XIF
         IFNZ       PC
         SPLOPEN   "NINBILL/PRT:PRINT"
         XIF
         ENDIF
.start test
         print     hpdupl,hp17ptch,*f
.end test
         CALL     PAINT
.
         CALL      HEADER
.
LOOP     CALL      NPAYSEQ      
. 
         GOTO      EOJ IF OVER
         ADD       ONE TO COUNT
         ADD       ONE TO TABLE
         DISPLAY   *P10:12,*EL,"RECORDS PROCESSED : ",COUNT
.
         BRANCH    TABLE OF ONE,TWO,THREE,FOUR
.         CALL      PRINT
.         MOVE      ZERO TO TABLE
         DISPLAY   *P1:23,*BLINKON,*HON,"OOOPS",*B
         GOTO      LOOP
.
ONE      MOVE      POWNER TO RTNUM1
         MOVE      PAYNUM TO RTNUM1A
         MOVE      PNAME TO RTCNTCT1
         MOVE      PCOMP TO RTCOMP1
         MOVE      PSTREET TO RTADDR1
         MOVE      PCITY TO RTCITY1
         MOVE      PSTATE TO RTSTATE1
         MOVE      PZIP TO RTZIP1
         MOVE      PPASS TO RTNAME1
         MOVE      PDATE TO RTREVDT1
.
         GOTO      LOOP
.
TWO      MOVE      POWNER TO RTNUM2
         MOVE      PAYNUM TO RTNUM2A
         MOVE      PNAME TO RTCNTCT2
         MOVE      PCOMP TO RTCOMP2
         MOVE      PSTREET TO RTADDR2
         MOVE      PCITY TO RTCITY2
         MOVE      PSTATE TO RTSTATE2
         MOVE      PZIP TO RTZIP2
         MOVE      PPASS TO RTNAME2
         MOVE      PDATE TO RTREVDT2
         GOTO      LOOP
.
THREE    MOVE      POWNER TO RTNUM3
         MOVE      PAYNUM TO RTNUM3A
         MOVE      PNAME TO RTCNTCT3
         MOVE      PCOMP TO RTCOMP3
         MOVE      PSTREET TO RTADDR3
         MOVE      PCITY TO RTCITY3
         MOVE      PSTATE TO RTSTATE3
         MOVE      PZIP TO RTZIP3
         MOVE      PPASS TO RTNAME3
         MOVE      PDATE TO RTREVDT3
.
         GOTO      LOOP
.
FOUR     MOVE      POWNER TO RTNUM4
         MOVE      PAYNUM TO RTNUM4A
         MOVE      PNAME TO RTCNTCT4
         MOVE      PCOMP TO RTCOMP4
         MOVE      PSTREET TO RTADDR4
         MOVE      PCITY TO RTCITY4
         MOVE      PSTATE TO RTSTATE4
         MOVE      PZIP TO RTZIP4
         MOVE      PPASS TO RTNAME4
         MOVE      PDATE TO RTREVDT4
.
         CALL      PRINT
         MOVE      ZERO TO TABLE
         GOTO      LOOP
.
PRINT    COMPARE   "64" TO LINES
         CALL      HEADER IF GREATER
         CALL      HEADER IF EQUAL
         PRINT     *1,"## ",RTNUM1,"-",RTNUM1A,*33,"## ",RTNUM2:
                   "-",RTNUM2A:
                   *65,"## ",RTNUM3,"-",RTNUM3A,*97,"## ",RTNUM4:
                   "-",RTNUM4A:
                   *N:
                   *1,RTCNTCT1,*33,RTCNTCT2,*65,RTCNTCT3,*97,RTCNTCT4:
                   *N:
                   *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
                   *FLUSH;
         PRINT     *1,RTCOMP1,*33,RTCOMP2,*65,RTCOMP3,*97,RTCOMP4:
                   *N:
                   *1,RTADDR1,*33,RTADDR2,*65,RTADDR3,*97,RTADDR4:
                   *N:
                   *1,RTCITY1,"  ",RTSTATE1," ",RTZIP1:
                   *33,RTCITY2,"  ",RTSTATE2," ",RTZIP2:
                   *65,RTCITY3,"  ",RTSTATE3," ",RTZIP3:
                   *97,RTCITY4,"  ",RTSTATE4," ",RTZIP4
.Start Patch 2.1
.         MOVE      DATEMASK TO RTDATE1
.         MOVE      DATEMASK TO RTDATE2
.         MOVE      DATEMASK TO RTDATE3
.         MOVE      DATEMASK TO RTDATE4
.         EDIT      RTREVDT1 TO RTDATE1
.         EDIT      RTREVDT2 TO RTDATE2
.         EDIT      RTREVDT3 TO RTDATE3
.         EDIT      RTREVDT4 TO RTDATE4         
         UNPACK    RTREVDT1 TO CC,YY,MM,DD
         PACK      RTDATE1  WITH MM,SLASH,DD,SLASH,CC,YY
         UNPACK    RTREVDT2 TO CC,YY,MM,DD         
         PACK      RTDATE2  WITH MM,SLASH,DD,SLASH,CC,YY         
         UNPACK    RTREVDT3 TO CC,YY,MM,DD
         PACK      RTDATE3  WITH MM,SLASH,DD,SLASH,CC,YY         
         UNPACK    RTREVDT4 TO CC,YY,MM,DD
         PACK      RTDATE4  WITH MM,SLASH,DD,SLASH,CC,YY         
.End Patch 2.1
         PRINT     *1,"UPDATED : ",RTDATE1:
                   *33,"UPDATED : ",RTDATE2:
                   *65,"UPDATED : ",RTDATE3:
                   *97,"UPDATED : ",RTDATE4:
                   *N:
                   *1,"By ",RTNAME1:
                   *33,"By ",RTNAME2:
                   *65,"By ",RTNAME3:
                   *97,"By ",RTNAME4,*L,*L
.
         ADD       TEN TO LINES
.
         CLEAR     RTNUM1
         CLEAR     RTNUM2
         CLEAR     RTNUM3
         CLEAR     RTNUM4
.
         CLEAR     RTCNTCT1
         CLEAR     RTCNTCT2
         CLEAR     RTCNTCT3
         CLEAR     RTCNTCT4
.
         CLEAR     RTCOMP1
         CLEAR     RTCOMP2
         CLEAR     RTCOMP3
         CLEAR     RTCOMP4
.
         CLEAR     RTADDR1
         CLEAR     RTADDR2
         CLEAR     RTADDR3
         CLEAR     RTADDR4
.
         CLEAR     RTCITY1
         CLEAR     RTCITY2
         CLEAR     RTCITY3
         CLEAR     RTCITY4
.
         CLEAR     RTSTATE1
         CLEAR     RTSTATE2
         CLEAR     RTSTATE3
         CLEAR     RTSTATE4
.
         CLEAR     RTZIP1
         CLEAR     RTZIP2
         CLEAR     RTZIP3
         CLEAR     RTZIP4
.
         CLEAR     RTNAME1
         CLEAR     RTNAME2
         CLEAR     RTNAME3
         CLEAR     RTNAME4
.
         CLEAR     RTREVDT1
         CLEAR     RTREVDT2
         CLEAR     RTREVDT3
         CLEAR     RTREVDT4
.
         RETURN
.
HEADER
         MOVE      FOUR TO LINES
         ADD       ONE TO PAGE
         PRINT     *F,*1,"CONFIDENTIAL":
                   *26,"* * *   N I N   M A S T E R   ":
                   "P A Y - T O   F I L E   * * *":
                   *119,"DATE: ",SYSDATE1:
                   *N,*119,"PAGE: ",PAGE,*L                   
         RETURN
EOJ      MATCH     "          " TO RTCOMP1
         GOTO      EOJ1 IF EOS
         GOTO      EOJ1 IF EQUAL
         CALL      PRINT
EOJ1     PRINT     hpport,hpdupoff,hpreset,*FLUSH
        shutdown  "cls"       
          STOP
         INCLUDE    NPAYIO.inc
         INCLUDE    COMLOGIC.inc         


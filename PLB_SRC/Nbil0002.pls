PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE    NBILDD.inc
RELEASE  INIT      "1.2"       ASH 04OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.1"       DLH  23MAR92   NBILXX
.RELEASE   INIT        "1"    *CREATED 29JUL91   DLH.
.
.
SYSDATE  DIM       8
DATEMASK INIT      "99/99/99"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
COUNT    FORM      5
TABLE    FORM      1
.
BLNUM1   DIM       4      1-4    BILLTO NUMBER. **KEY**
BLNUM1A  DIM       3
BLNUM1B  DIM       1
BLCNTCT1 DIM       25     5-29   BILLTO CONTACT NAME.
BLCOMP1  DIM       25    30-54   BILLTO COMPANY NAME.
BLADDR1  DIM       25    55-79   BILLTO ADDRESS
BLCITY1  DIM       15    80-94   BILLTO CITY.
BLSTATE1 DIM       2     95-96   BILLTO STATE.
BLZIP1   DIM       10    97-106  BILLTO ZIP. LEFT JUST.
BLNAME1  DIM       10   107-116  PASSWORD NAME
BLREVDT1 DIM       6    117-122  REVISED DATE.
. 
BLNUM2   DIM       4      1-4    BILLTO NUMBER. **KEY**
BLNUM2A  DIM       3
BLNUM2B  DIM       1
BLCNTCT2 DIM       25     5-29   BILLTO CONTACT NAME.
BLCOMP2  DIM       25    30-54   BILLTO COMPANY NAME.
BLADDR2  DIM       25    55-79   BILLTO ADDRESS
BLCITY2  DIM       15    80-94   BILLTO CITY.
BLSTATE2 DIM       2     95-96   BILLTO STATE.
BLZIP2   DIM       10    97-106  BILLTO ZIP. LEFT JUST.
BLNAME2  DIM       10   107-116  PASSWORD NAME
BLREVDT2 DIM       6    117-122  REVISED DATE.
. 
BLNUM3   DIM       4      1-4    BILLTO NUMBER. **KEY**
BLNUM3A  DIM       3
BLNUM3B  DIM       1
BLCNTCT3 DIM       25     5-29   BILLTO CONTACT NAME.
BLCOMP3  DIM       25    30-54   BILLTO COMPANY NAME.
BLADDR3  DIM       25    55-79   BILLTO ADDRESS
BLCITY3  DIM       15    80-94   BILLTO CITY.
BLSTATE3 DIM       2     95-96   BILLTO STATE.
BLZIP3   DIM       10    97-106  BILLTO ZIP. LEFT JUST.
BLNAME3  DIM       10   107-116  PASSWORD NAME
BLREVDT3 DIM       6    117-122  REVISED DATE.
. 
BLNUM4   DIM       4      1-4    BILLTO NUMBER. **KEY**
BLNUM4A  DIM       3
BLNUM4B  DIM       1
BLCNTCT4 DIM       25     5-29   BILLTO CONTACT NAME.
BLCOMP4  DIM       25    30-54   BILLTO COMPANY NAME.
BLADDR4  DIM       25    55-79   BILLTO ADDRESS
BLCITY4  DIM       15    80-94   BILLTO CITY.
BLSTATE4 DIM       2     95-96   BILLTO STATE.
BLZIP4   DIM       10    97-106  BILLTO ZIP. LEFT JUST.
BLNAME4  DIM       10   107-116  PASSWORD NAME
BLREVDT4 DIM       6    117-122  REVISED DATE.
. 
.
BLDATE1  DIM       8
BLDATE2  DIM       8
BLDATE3  DIM       8
BLDATE4  DIM       8
.
.
         CLOCK     DATE TO DATE
         IFZ       PC
         MOVE      DATE TO TODAY
         MOVE      DATE TO SYSDATE
         MOVE      "NINBILL.SRT" TO NBILNAME
         XIF
         IFNZ      PC 
         MOVE      "NINBILL/SORT" TO NBILNAME
         MOVE      DATEMASK TO SYSDATE
         EDIT      DATE TO SYSDATE
         MOVE      SYSDATE TO TODAY
         XIF
         CMATCH    B1 TO PRTNAME
         IF        EOS
         MOVE      "LOCAL" TO PRTNAME
         ELSE
         IFZ       PC
.START PATCH 1.2 REPLACED LOGIC
.         SPLOPEN   "\\nts0\d\DATA\NINBILL.lst"
         PACK      STR35,ntwkpath1,"NINBILL.LST"
         SPLOPEN   STR35
.END PATCH 1.2 REPLACED LOGIC
         XIF
         IFNZ       PC
         SPLOPEN   "NINBILL/PRT:PRINT"
         XIF
         ENDIF
         MOVE      "NBIL0002" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "MASTER BILL-TO PRINT" TO STITLE
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         TRAP      EOJ1 IF F5
         CALL      FUNCDISP
         MOVE      C3 TO NBILPATH     .SET ACCESS TO SEQ FLAT FILE.
.
         CALL      HEADER
.
LOOP     CALL      NBILSEQ
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNT
         ADD       C1 TO TABLE
         DISPLAY   *P10:12,*EL,"RECORDS PROCESSED : ",COUNT
.
         BRANCH    TABLE TO ONE,TWO,THREE,FOUR
         DISPLAY   *P1:24,*BLINKON,*HON,"OOOPS",*B;
         GOTO      LOOP
.
ONE
         MOVE      BILMLR TO BLNUM1
         MOVE      BILCNT TO BLNUM1A
         MOVE      BILCODE TO BLNUM1B
         MOVE      BILNAME TO BLCNTCT1
         MOVE      BILCOMP TO BLCOMP1
         MOVE      BILADDR TO BLADDR1
         MOVE      BILCITY TO BLCITY1
         MOVE      BILSTATE TO BLSTATE1
         MOVE      BILZIP TO BLZIP1
         MOVE      BILUSER TO BLNAME1
         MOVE      BILRVDTE TO BLREVDT1
         GOTO      LOOP
.
TWO      MOVE      BILMLR TO BLNUM2
         MOVE      BILCNT TO BLNUM2A
         MOVE      BILCODE TO BLNUM2B
         MOVE      BILNAME TO BLCNTCT2
         MOVE      BILCOMP TO BLCOMP2
         MOVE      BILADDR TO BLADDR2
         MOVE      BILCITY TO BLCITY2
         MOVE      BILSTATE TO BLSTATE2
         MOVE      BILZIP TO BLZIP2
         MOVE      BILUSER TO BLNAME2
         MOVE      BILRVDTE TO BLREVDT2
.
         GOTO      LOOP
.
THREE    MOVE      BILMLR TO BLNUM3
         MOVE      BILCNT TO BLNUM3A
         MOVE      BILCODE TO BLNUM3B
         MOVE      BILNAME TO BLCNTCT3
         MOVE      BILCOMP TO BLCOMP3
         MOVE      BILADDR TO BLADDR3
         MOVE      BILCITY TO BLCITY3
         MOVE      BILSTATE TO BLSTATE3
         MOVE      BILZIP TO BLZIP3
         MOVE      BILUSER TO BLNAME3
         MOVE      BILRVDTE TO BLREVDT3
.
         GOTO      LOOP
.
FOUR     MOVE      BILMLR TO BLNUM4
         MOVE      BILCNT TO BLNUM4A
         MOVE      BILCODE TO BLNUM4B
         MOVE      BILNAME TO BLCNTCT4
         MOVE      BILCOMP TO BLCOMP4
         MOVE      BILADDR TO BLADDR4
         MOVE      BILCITY TO BLCITY4
         MOVE      BILSTATE TO BLSTATE4
         MOVE      BILZIP TO BLZIP4
         MOVE      BILUSER TO BLNAME4
         MOVE      BILRVDTE TO BLREVDT4
.
         CALL      PRINT
         MOVE      C0 TO TABLE
         GOTO      LOOP
.
PRINT    COMPARE   "64" TO LINES
         CALL      HEADER IF GREATER
         CALL      HEADER IF EQUAL
         PRINT     *1,"## ",BLNUM1,SLASH,BLNUM1A,DASH,BLNUM1B:
                   *33,"## ",BLNUM2,SLASH,BLNUM2A,DASH,BLNUM2B:
                   *65,"## ",BLNUM3,SLASH,BLNUM3A,DASH,BLNUM3B:
                   *97,"## ",BLNUM4,SLASH,BLNUM4A,DASH,BLNUM4B:
                   *N:
                   *1,BLCNTCT1,*33,BLCNTCT2,*65,BLCNTCT3,*97,BLCNTCT4:
                   *N:
                   *1,BLCOMP1,*33,BLCOMP2,*65,BLCOMP3,*97,BLCOMP4:
                   *FLUSH;
         PRINT     *1,BLCOMP1,*33,BLCOMP2,*65,BLCOMP3,*97,BLCOMP4:
                   *N:
                   *1,BLADDR1,*33,BLADDR2,*65,BLADDR3,*97,BLADDR4:
                   *N:
                   *1,BLCITY1,"  ",BLSTATE1," ",BLZIP1:
                   *33,BLCITY2,"  ",BLSTATE2," ",BLZIP2:
                   *65,BLCITY3,"  ",BLSTATE3," ",BLZIP3:
                   *97,BLCITY4,"  ",BLSTATE4," ",BLZIP4
         MOVE      DATEMASK TO BLDATE1
         MOVE      DATEMASK TO BLDATE2
         MOVE      DATEMASK TO BLDATE3
         MOVE      DATEMASK TO BLDATE4
         EDIT      BLREVDT1 TO BLDATE1
         EDIT      BLREVDT2 TO BLDATE2
         EDIT      BLREVDT3 TO BLDATE3
         EDIT      BLREVDT4 TO BLDATE4
         PRINT     *1,"UPDATED : ",BLDATE1:
                   *33,"UPDATED : ",BLDATE2:
                   *65,"UPDATED : ",BLDATE3:
                   *97,"UPDATED : ",BLDATE4:
                   *N:
                   *1,"By ",BLNAME1:
                   *33,"By ",BLNAME2:
                   *65,"By ",BLNAME3:
                   *97,"By ",BLNAME4,*L,*L
.
         ADD       C10 TO LINES
.
         CLEAR     BLNUM1
         CLEAR     BLNUM2
         CLEAR     BLNUM3
         CLEAR     BLNUM4
.
         CLEAR     BLCNTCT1
         CLEAR     BLCNTCT2
         CLEAR     BLCNTCT3
         CLEAR     BLCNTCT4
.
         CLEAR     BLCOMP1
         CLEAR     BLCOMP2
         CLEAR     BLCOMP3
         CLEAR     BLCOMP4
.
         CLEAR     BLADDR1
         CLEAR     BLADDR2
         CLEAR     BLADDR3
         CLEAR     BLADDR4
.
         CLEAR     BLCITY1
         CLEAR     BLCITY2
         CLEAR     BLCITY3
         CLEAR     BLCITY4
.
         CLEAR     BLSTATE1
         CLEAR     BLSTATE2
         CLEAR     BLSTATE3
         CLEAR     BLSTATE4
.
         CLEAR     BLZIP1
         CLEAR     BLZIP2
         CLEAR     BLZIP3
         CLEAR     BLZIP4
.
         CLEAR     BLNAME1
         CLEAR     BLNAME2
         CLEAR     BLNAME3
         CLEAR     BLNAME4
.
         CLEAR     BLREVDT1
         CLEAR     BLREVDT2
         CLEAR     BLREVDT3
         CLEAR     BLREVDT4
.
         RETURN
.
HEADER
         MOVE      C4 TO LINES
         ADD       C1 TO PAGE
         PRINT     *F,*1,"CONFIDENTIAL":
                   *26,"* * *   N I N   M A S T E R   ":
                   "B I L L - T O   F I L E   * * *":
                   *119,"DATE: ",SYSDATE:
                   *N,*119,"PAGE: ",PAGE,*L
         RETURN
EOJ      MATCH     "          " TO BLCOMP1
         GOTO      EOJ1 IF EOS
         GOTO      EOJ1 IF EQUAL
         CALL      PRINT
EOJ1     PRINT     *FLUSH
         STOP
         INCLUDE   NBILIO.inc
         INCLUDE   COMLOGIC.inc


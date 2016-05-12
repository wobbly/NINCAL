PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INCLUDE   NMLRDD.inc
         INCLUDE   NXNGDD.inc
         INCLUDE   NXCHDD.inc
         INCLUDE   HP.inc
;*#**#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#**#*#*#*#*#*#**#**#*#*#*#*#*#*#*#*#*#*#*#*#*
release  init     "1.0"     26August2003 DLH 
. .............................................................................
. 
. FILES
. ...................
. 
. ACCOUNT FILE
. ...................
. 
MLR1     DIM       4
MLR2     DIM       4     MAILER NUMBERS
AKEY1   INIT       "01R"
AKEY2    INIT      "02R"
. 
Output         File
. .............................................................................
. 
. WORK FIELDS
. ....................
. 
.Start Patch #1.5 - expanded fields to reflect expansion of MCOMP
.MDES1    DIM       25
.MDES2    DIM       25
MDES1    DIM       45
MDES2    DIM       45
.End Patch #1.5 - expanded fields to reflect expansion of MCOMP
ENTRY1   FORM      5        ENTRY NUMBER
COUNT    FORM      7            NUMBER OF ACCOUNT RECORDS READ
OKCOUNT  FORM      7
BADCOUNT FORM      5            NUMBER OF DETAIL RECORDS NOT FOUND
PAGE     FORM      "0000"
LINE     FORM      "00"
TOTAL1   FORM      9
TOTAL2   FORM      9
VAR1     FORM      9
VAR2     FORM      9
STOP     FORM      5
BRANCH   FORM      1
ZERO5    FORM      "00000"
DETSW    DIM       1         *Y=PERFORM TOTALS CHECK < 1000, N=NO CHECK
NUM      FORM      1         BRANCH INDICATOR FOR MODE.
HENTRY   DIM       5         HOLD ENTRY FOR OUTPUT
HKEY     DIM       8         HOLD KEY FOR MATCH
HKEY1    DIM       5
OLDKEY   DIM       8
TYPE1    DIM       6
TYPE2    DIM       6
DATE     DIM       8
TIME     DIM       8
DIFF     FORM      10
DIFF1    FORM      10
THOUS    FORM      "1000"
. .............................................................................
. 
. PROGRAM BODY
. .............................................................................
. 
*DEBUG
         MOVE      "Names In The News Ca Inc" TO COMPNME
         MOVE      "NXCH" TO PROGRAM
         MOVE      "Integral EXCHANGE ACCOUNTS" TO STITLE
         CLOCK     DATE TO TODAY
         MOVE      TODAY TO DATE
         MOVE      "ABORT" TO PF5
         CALL       PAINT
         CALL       FUNCDISP
.         DISPLAY   *P01:01,*ES,*P14:01," EXCHANGE TEST PROGRAM "
         TRAP      IO IF IO
         TRAP      EOJ IF F5
. 
         CALL      FUNCDISP
         Prepare              Output,"c:\work\IntegralXch.dat"
BEGIN
         DISPLAY   *P01:24,*EL;
. 
         DISPLAY   *P5:12,*EL,"READING ACCOUNT FILE NUMBER OF RECORDS READ":
                   " EQUALS : "
START
         MOVE      C0 TO TOTAL1
         MOVE      C0 TO TOTAL2
         DISPLAY   *P57:12,*EL,COUNT
         call      nxngseq
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNT
         CLEAR     NXCHFLD1
         PACK      NXCHFLD1 FROM acckey,ENtry
         MOVE      C0 TO STOP
         MOVE      ENTRY TO STOP
         REP       ZFILL,NXCHFLD1
         MOVE      "NO BALANCE " TO ERROR
         move      c1 to nxchpath
         call      nxchkey
         IF        OVER
.        GOTO      START
         endif      
         write     Output,seq;ACCKEY:
                              nxngdate:
                              ENTRY:
                              Flag:
                              Usage1:
                              Usage2
          goto     start
EOJ      
               Weof           Output,seq
               close          Output
               STOP
IO       DISPLAY   *P10:20,*EL,"FILE NOT FOUND",*B,*W5
         STOP
         INCLUDE   NMLRIO.inc
         INCLUDE   NXNGIO.inc
         INCLUDE   NXCHIO.inc
         INCLUDE   COMLOGIC.inc


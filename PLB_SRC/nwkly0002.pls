PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
OUTPUT   FILE      
release  init      "1.0"       DLH
Reldate   Init      "2013 May 16"
date     DIM       8
today1   FORM      5                              .today in julian
today2   FORM      5                              .six months ago in julian

sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
...............................................................................
.PRINT VARIABLES
COUNT    FORM      5
USED     FORM      5

         MOVE      "NWKLY0002" TO PROGRAM
         MOVE      "Update last 6 mos" TO STITLE
         MOVE      "Names In The News." TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
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
         sub        "180" from Juldays
         move       Juldays to Today2
. 
OPEN     TRAP      ABORT IF F5
          move      "000000",ndatfld
          call      ndattst
. 
. 
. PREP OUTPUT FILE
. 
         PREPARE   OUTPUT,"\\nins1\e\data\datawkly.dat|NINS1:502",exclusive
         move      c3 to ndatlock
. GET DATA CARDs
. 
A100
          loop
          CALL      NDATKS
          until     over
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
. 
          unpack    revdate into cc,yy,mm,dd
         CALL      CVTJUL
          if        (juldays >= Today2 & Juldays <= today1 & (elstcde = "C" or ELSTCDE = "P"))
          call      writeout
          endif
          repeat
          goto      z900
..................................................
writeout


.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal

         write     output,seq;datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS output ",USED
          return
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
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
. 
         shutdown   "cls"
. 
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


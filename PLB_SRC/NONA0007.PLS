PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
ONAKEY   DIM       8
brk      dim       4
CHANGE   FORM      7.2
SAMEMLR  DIM       7
.needs to be cleaned up  - sigh  DH 2013 june 24

RELEASE  INIT      "1.2"      JD 19AUG92 ADDED NMOA,NMOB.inc'S
.RELEASE  INIT      "1.1"      DLH 12MAR92 CONVERT TO PCBUS
...............................................................................
         MOVE      "NONA0007" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "FIX MOA BALANCE" TO STITLE
         move       c1 to nmoapath
         move       c2 to nmobpath
         CALL      PAINT
LOOP     KEYIN     *P10:10,*ef,"BRk/MLR",*JR,*ZF,ONAKEY
         SCAN     "*" TO ONAKEY
         STOP      IF EQUAL
.         GOTO      LOOP IF EOS
           RESET     ONAKEY
         cmatch    b1 to onakey
         GOTO      LOOP IF EOS
         UNPACK    ONAKEY INTO brk,MLR
         move      z3 to nmobMcnt
         MOVE      ONAKEY TO NMOAFLD4
         CALL      NMOBKEY
         GOTO      NOGOOD IF over
         KEYIN     *P1:12,*DV,balance," NEW BALANCE ",*RV,change
.         pack      nmoafld4 from nmobbrk,mlr
         pack      nmobfld from mlr
         move      z3 to mcnt
         DISPLAY   *P1:24,*EL,"GOING TO DELETE ",*B,*W;         
         MOVE      C2 TO NMOBPATH
         CALL      NMOBDEL
         DISPLAY   *P1:24,*EL,"DELETE DONE",*B,*W;         
         DISPLAY   *P1:24,*EL,"GOING TO WRITE ",*B,*W;         
.         move      brk to nmobbrk
         move      c0 to nmobmcnt
         unpack    nmoafld4 into nmoabrk,mlr
         CALL      NMOBWRT
         DISPLAY   *P1:24,*EL,"WRITE DONE",*B,*W;         
         GOTO      LOOP
NOGOOD   DISPLAY   *P1:24,*EL,"NO SUCH ENTRY",*B,*B;
.         KEYIN     *P1:12," NEW BALANCE ",*RV,BALANCE
.         CALL      NMOBWRT
         GOTO      LOOP
         STOP
...............................................................................
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         INCLUDE   COMLOGIC.inc
         STOP


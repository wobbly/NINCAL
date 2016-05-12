PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
;begin patch 2.2
;         INCLUDE   NINVDD.inc
               INCLUDE        ninvdd.inc
;end patch 2.2
RELEASE        INIT           "2.2"      DLH   08March2005 Invoice Conversion
;RELEASE  INIT      "2.1"      DLH   5MAY99 NININV Y2K
.RELEASE  INIT      "2.0"      DLH   20MAR92
PASS     DIM       5
PASSWORD INIT      "COSMO"
. 
. 
START    MOVE      "NINV0014" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "FIX INVOICE DATE" TO STITLE
         MOVE      C2 TO NINVPATH     .SET ACCESS TO ISI BY INV#.
         CALL      PAINT
         MOVE      "EXIT" TO PF5
         CALL      FUNCDISP
         KEYIN     *P1:4,*EL,"PASSWORD ",*EOFF,PASS
         MATCH     PASS,PASSWORD
         GOTO      STOP IF NOT EQUAL
KEY
         KEYIN     *P10:10,"ENTER INVOICE NUMBER ",*ZF,*JR,NINVFLD
         MATCH     "00000*",NINVFLD
         GOTO      STOP IF EQUAL
         TYPE      NINVFLD
         GOTO      KEY IF EOS
         KEYIN     *P10:20,*EL,"OK?",STR1
         CMATCH    "Y",STR1
         GOTO      KEY IF NOT EQUAL
         CALL      NINVKEY
         GOTO      NOREC IF OVER
         DISPLAY   *P10:12,"DATE = ",INVDTEM,"/",INVDTED,"/",invdtec,INVDTEY
         KEYIN     *P10:24,*EL,"CHANGE IT ?",STR1;
         CMATCH    "Y",STR1
         GOTO      KEY IF NOT EQUAL
         GOTO      UPDATE
UPDATE
         KEYIN     *P10:14,*+,"NEW DATE __/__/____",*P19:14,*de,*+,INVDTEM:
                   *P22:14,INVDTED,*P25:14,invdtec,INVDTEY,*-
         KEYIN     *P10:24,*EL,"OK?",STR1;
         CMATCH    "Y",STR1
         GOTO      UPDATE IF NOT EQUAL
         CALL      NINVUPD
         GOTO      KEY
STOP     STOP
.
NOREC    DISPLAY   *P1:24,*EL,*B,"********** NO SUCH RECORD ***********";
         GOTO      KEY
 ;begin patch 2.2
;        INCLUDE   NINVIO.inc
               INCLUDE        ninvio.inc
 ;end patch 2.2
        INCLUDE   COMLOGIC.inc


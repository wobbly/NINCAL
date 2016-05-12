PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
S6       DIM       6
LR       DIM       6
S124     DIM       124
S30      DIM       30
MO       DIM       2
DY       DIM       2
YR       DIM       2
HLDMO    DIM       2
HLDYR    DIM       2

RELEASE  INIT      "1.4"      ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.3"      ASH 29DEC98 NINORD Y2K,File expansion
.RELEASE  INIT      "1.2"      DLH 18MAR92
         MOVE      "NORD0019" TO PROGRAM
         MOVE      "Names In The News Ca Inc" TO COMPNME
         MOVE      "CHECK FOR BAD ORDER DATES" TO STITLE
         MOVE      "ABORT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
         KEYIN     *P1:8,*HON,"DO YOU WANT SPOOLING ? ",*uc,STR1
         CMATCH    YES,STR1
         GOTO      SPOOL IF EQUAL
         TRAP      ABORT IF F5
BEGIN    KEYIN     *P1:10,"ENTER ORDER FILE NAME: ",*uc,NORDNAME
         KEYIN     *P1:5,"TURN ON THE SERIAL PRINTER, HIT ENTER. ",STR1
         PRINT     *F,*L,*L,*10,"SEARCH FOR BAD ORDER DATES PROGRAM"
KEY      KEYIN     *P1:12,*EL,"STARTING LR##, ENTER FOR ENTIRE FILE",*JR,*ZF:
                   NORDFLD,*P60:12,"OK?",*uc,STR1,*P60:12,*EL
         CMATCH    YES TO STR1
         GOTO      KEY IF NOT EQUAL
         CMATCH    B1 TO NORDFLD
         GOTO      LOOP IF EOS
         GOTO      LOOP IF EQUAL
START    CALL      NORDKEY
         GOTO      EOJ IF OVER
         GOTO      CHECK
LOOP     CALL      NORDKS
         GOTO      EOJ IF OVER
CHECK    MATCH     OODTEY,HLDYR
         GOTO      YRCHNG IF NOT EQUAL
         MATCH     OODTEM,HLDMO
         GOTO      LOOP IF EQUAL
.Start Patch #1.3 - remmed and replaced logic
.         DISPLAY   *P1:24,"MONTH CHANGE...LR ",OLRN,"  DATE=":
.                   OODTEM,OODTED,OODTEY
.         PRINT     *L,*24,"MONTH CHANGE...LR",OLRN,"  DATE=":
.                   OODTEM,OODTED,OODTEY
         DISPLAY   *P1:24,"MONTH CHANGE...LR ",OLRN,"  DATE=":
                   OODTEM,OODTED,OODTEC,OODTEY
         PRINT     *L,*24,"MONTH CHANGE...LR",OLRN,"  DATE=":
                   OODTEM,OODTED,OODTEC,OODTEY
.End Patch #1.3 - remmed and replaced logic
         MOVE      OODTEM,HLDMO
         GOTO      LOOP
YRCHNG   
.Start Patch #1.3 - remmed and replaced logic
.         DISPLAY   *P1:24,"YEAR  CHANGE...LR ",OLRN,"  DATE=":
.                   OODTEM,OODTED,OODTEY
.         PRINT     *L,*24,"YEAR  CHANGE...LR",OLRN,"  DATE=":
.                   OODTEM,OODTED,OODTEY
         DISPLAY   *P1:24,"YEAR  CHANGE...LR ",OLRN,"  DATE=":
                   OODTEM,OODTED,OODTEC,OODTEY
         PRINT     *L,*24,"YEAR  CHANGE...LR",OLRN,"  DATE=":
                   OODTEM,OODTED,OODTEC,OODTEY
.End Patch #1.3 - remmed and replaced logic
         MOVE      OODTEY,HLDYR
         GOTO      LOOP
ABORT    DISPLAY   *P1:24,*EL,"JOB ABORTED",*B
EOJ      KEYIN     *P1:24,*R,"JOB DONE",STR1
         STOP
SPOOL    IFNZ      PC
         SPLOPEN   "ORDERDTE/PRT"
         XIF
         IFZ        PC
.START PATCH 1.4 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\ORDERDTE.lst"
         PACK      STR35,NTWKPATH1,"ORDERDTE.lst"
         SPLOPEN   STR35
.END PATCH 1.4 REPLACED LOGIC
         XIF
         GOTO      BEGIN
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


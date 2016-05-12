* *****************************************************************************
* NDAT0015 - DATAWEIRD
*
* REVISIONS:    OCT 20 1986    CHANGE TO USE ORDER FILE INCLUDE
*               FEB    1987    CONVERT TO RMS
*               JUNE 22 1987   ADD LIST/ STATUS DISPLAY ON LOOK UP
*
*
* *****************************************************************************
*
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NDATDD.inc
. .............................................................................
RELEASE  INIT      "1.1"      ASH 29OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.0"      DLH 12MAR92     UPDATE INCLUDES
.RELEASE  INIT      "0.8"
.
DATWEIRD FILE      VAR=2813
.
. .............................................................................
.
. PROGRAM VARIABLES
. .................
.
KMAILER  DIM       4                  KEYIN FIELD FOR MAILER##;
KLIST    DIM       6                  KEYIN FIELD FOR LIST##;
QUES     INIT      "????"
.ANS      DIM       1
VTAB     FORM      2
VTAB1    FORM      2
VTAB2    FORM      2
VALKEY   FORM      1
HIT      FORM      1
DATE     FORM      1
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
WORK02   DIM       2
MO       DIM       2                  DATE DISPLAY;
DAY      DIM       2                   "      "
YEAR     DIM       2                   "      "
PASS     FORM      3                  NUMBER OF RECORD DISPLAYS;
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
AKEY1    DIM       3                  USED TO BUILD MAILER & LIST FILE AIM KEY.
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
KEYCOUNT FORM      2                  USED TO CHECK AIM KEY LENGTH, FREE FLOAT
TEN      FORM      "10"
SIX      FORM      "6"
VARIN    FORM      2
ONE      FORM      "1"
ZERO     FORM      "0"
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F5
         TRAP      IOPEN GIVING ERROR NORESET IF IO
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "                    " TO FERROR
.         TRAPCLR   IO
         TRAP      IO GIVING ERROR NORESET IF IO
         MOVE      "NDAT0015" TO PROGRAM
         MOVE      "DATA WEIRD PICK" TO STITLE
         MOVE      "Names in the News CA" TO COMPNME
         CALL      PAINT
         TRAP      PREP IF IO
         IFNZ      PC
         OPEN      DATWEIRD,"DATAWEIRD"
         XIF
         IFZ       PC
         OPEN      DATWEIRD,"DATAWERD"
         XIF
WHATDO   KEYIN     *P12:10,*EF,"ADD TO EXISTING OUTPUT FILE ? ",STR1
         CMATCH    YES TO STR1
         GOTO      SEQ3 IF EQUAL
         CMATCH    NO TO STR1
         GOTO      WHATDO IF NOT EQUAL
         CLOSE     DATWEIRD
         GOTO      PREP
SEQ3     MOVE      "-3" TO SEQ
         GOTO      START
PREP     TRAPCLR   IO
         IFNZ      PC
         PREPARE   DATWEIRD,"DATAWEIRD/TEXT:PRINT"
         XIF
         IFZ       PC
.START PATCH 1.1 REPLACED LOGIC
.         PREPARE   DATWEIRD,"g:\DATA\DATAWERD"
         PACK      TASKNAME,NTWKPATH1,"DATAWERD"
         PREPARE   DATWEIRD,TASKNAME
.END PATCH 1.1 REPLACED LOGIC
         XIF
.
START
         DISPLAY   *P28:11,"2) LIST##:   ______":
                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
                   " or (?) FOR HELP",*HOFF;
.
KEYIN2
         CLEAR     KLIST
         KEYIN     *P40:11,*ZF,*JR,*T60,KLIST
         DISPLAY   *P40:11,KLIST
         MATCH     "00000*" TO KLIST
         GOTO      EXIT IF EQUAL
         MATCH     "00000?" TO KLIST
         GOTO      LISTHELP IF EQUAL
         GOTO      EXIT
.
LISTHELP DISPLAY   *SETSWALL  12:24:40:80;
         KEYIN     *P1:1,*ES:
                   *P1:12,*EL,"I WILL HELP YOU FIND THE LIST NUMBER",*R:
                   *P1:12,*EL,"THAT YOU WANT":
                   *R,*P1:12:
                   *R,*P1:12,"DO YOU WANT A FREE FLOAT SEARCH ? ,":
                   *R,*P1:12,"ie: FIND 'AMERICAN' ANYWHERE IN LIST ":
                   *R,*P1:12,"NAME ",*T60,STR1;
         CMATCH    YES TO STR1
         GOTO      FREFLOT1 IF EQUAL
         MOVE      "02L" TO AKEY1
KEYLIST  KEYIN     *P1:1,*ES,"TYPE IN LIST NAME : ":
                   *P1:2,*T60,MLSTNAME
         KEYIN     *P1:2,*DV,MLSTNAME,*P1:3,"OK ? ",*T254,STR1:
                   *P1:3,*EL;
         CMATCH    NO TO STR1
         GOTO      KEYLIST IF EQUAL
         CMATCH    B1 TO MLSTNAME
         GOTO      NTENOUGH IF EQUAL
         GOTO      NTENOUGH IF EOS
         MATCH    "02L" TO AKEY1
         GOTO      PACKL1 IF EQUAL
         PACK      NDATFLD2 FROM AKEY1,MLSTNAME
         CALL      CHKKEY1
         GOTO      LSTREADA
PACKL1   PACK      NDATFLD2 FROM AKEY1,MLSTNAME,QUES
         CALL      CHKKEY1
         GOTO      LSTREADA
FREFLOT1 MOVE      "02F" TO AKEY1
         GOTO      KEYLIST
LSTREADA
         CLEAR     LSTNUM
         MOVE      "****NO LIST FOUND*****" TO MLSTNAME
         CLEAR     NDATFLD1
         CALL      NDATAIM
         GOTO      LSTAEXIT IF OVER
         GOTO      DISLSTA
CHKKEY1  MOVELPTR  NDATFLD2 TO KEYCOUNT
         COMPARE   SIX TO KEYCOUNT
         GOTO      NTENOUGH IF LESS
         RETURN
LSTRED1A CLEAR     LSTNUM
         MOVE      "****NO LIST FOUND*****" TO MLSTNAME
         CALL      NDATKG
         GOTO      LSTAEXIT IF OVER
         GOTO      DISLSTA
LSTAEXIT
         IFNZ      PC
         FLUSH     DATWEIRD
         XIF
         CALL      LSTREAD1
         DISPLAY   *RESETSW
         GOTO      KEYIN2
DISLSTA  DISPLAY   *P1:11,*EL,"##",LSTNUM,B1,MLSTNAME,*P1:12,*EL
         CMATCH    "W" TO STATUS
         CALL      WITHD IF EQUAL
         WRITE     DATWEIRD,SEQ;DATVARS
         GOTO      LSTRED1A
WITHD    DISPLAY   *P1:12,*EL,*HON,"WITHDRAWN",*HOFF
         RETURN
.
NTENOUGH NORETURN
.
NOGOOD
         DISPLAY   *P1:24,*HON,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON!!!":
                   *RESETSW:
                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
                   " or (?) FOR HELP",*HOFF;
         PAUSE     "3"
         GOTO      START
LSTREAD
         MOVE      LSTNUM TO NDATFLD
         CALL      NDATKEY
         CALL      LSTREAD1 IF OVER
         RETURN
.
LSTREAD1
         MOVE      "NOTE: NO LIST FOUND              ",MLSTNAME
         RETURN
.
* ***************************************************************************
*  EXIT AND FERROR SUBROUTINES
* ****************************************************************************
.
EXIT     DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         WEOF      DATWEIRD,SEQ
         CLOSE     DATWEIRD
EXIT1    BEEP
         STOP
IOPEN    TRAPCLR   IO
         NORETURN
IODIS    DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      IODIS
IO       TRAPCLR   IO
         TRAP      IO GIVING ERROR NORESET IF IO
         NORETURN
IODIS1   DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,STR1,*RESETSW;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      IODIS1
RANGE
         TRAPCLR   RANGE
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         NORETURN
RANGED   DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGED
         STOP
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
         STOP
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
         STOP
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


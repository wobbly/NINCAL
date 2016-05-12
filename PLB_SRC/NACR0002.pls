. 
. 
. 
PC        EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
. 
. 
. 
. 
. 
. 
. 
        IFZ       PC
ACROMST  AFILE    var=48
        XIF
        IFNZ       PC
ACROMST  AFILE     var=48
         XIF
. 
. 
. 
. 
LSNO     DIM       6
NAME     DIM       35
ACRONYM  DIM       7
DATE1    DIM       6
MLRNUM   DIM       4
. 
. 
. 
.MM       DIM       2
.DD       DIM       2
.YY       DIM       2
. 
. 
. 
ANS      DIM       1
.SEQ      FORM      "-1"
SYSDATE  DIM       8
DATE     DIM       8
PAGECNTR FORM      "00"
LINECNTR FORM      2
PAGENUM  FORM      2
LISTMASK DIM       6
LETTER   INIT      "A"
LINECHK  FORM      "56"
ZEROS    FORM      "000000"
.SLASH    INIT      "/"
KEY1     INIT      "01X"
KEY2     INIT      "02L"
KEY3     INIT      "03L"
AKEY1    DIM       9
AKEY2    DIM       38
AKEY3    DIM       10
THIRTEEN INIT      "............."
TWO      INIT      "  "
STRING   INIT      " 00112233445566778899AABBCCDDEEFFGGHHIIJJKKLLMMNNO"
STRING1  INIT      "OPPQQRRSSTTUUVVWWXXYYZ"
BREAK    DIM       1
QUES     INIT      "????"
BLANKS   INIT      "      "
LISTNUM  FORM      6
CLIENT   DIM       35
ACRONIM  DIM       7
.
RELEASE INIT	"1.1"		DS25April2006  replaced references to "G" drive
.RELEASE INIT       "1.0"                JD26OCT92 COMLOGIC,CONS.
. 
. 
. 
         CLOCK     DATE TO SYSDATE
         IFNZ      PC
         UNPACK    SYSDATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC 
         UNPACK    SYSDATE INTO MM,STR1,DD,STR1,YY
         MOVE      SYSDATE TO TODAY
         XIF
         REP       " 0",MM
         REP       " 0",DD
         REP       " 0",YY
         PACK      SYSDATE FROM MM,SLASH,DD,SLASH,YY
. 
. 
. 
OPENMST
         OPEN      ACROMST,"ACRONYM"
         MOVE      "NACR0002" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "ACRONYM PRINT PROGRAM" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
         TRAP      ABORT IF F5
         KEYIN     *P1:23,*EL,"OUTPUT TO BE SPOOLED ?",ANS
         CMATCH    "N" TO ANS
         GOTO      GO IF EQUAL
         IFNZ      PC
         SPLOPEN   "ACROPRT/PRT:PRINT","Q"
         XIF
         IFZ      PC
.START PATCH 	1.1 	REPLACED LOGIC
.        SPLOPEN   "g:\DATA\ACROPRT.LST"
  	PACK taskname, NTWKPATH1, "ACROPRT.LST"
  	SPLOPEN taskname
.END PATCH 	1.1 	REPLACED LOGIC
         XIF
GO       CALL      HEADINGS
         CALL      PREP
         GOTO      DETAIL
. 
. 
. 
PERFORM  DISPLAY   *P1:23,*EL,*HON,"P R I N T I N G "
         READKG    ACROMST;LSNO,NAME,ACRONYM,DATE1,MLRNUM
         GOTO      BREAK IF OVER
         MATCH     LSNO TO BLANKS
         CALL      ZEROS IF EQUAL
         GOTO      DETAIL
. 
. 
. 
*  HEADINGS ROUTINE  *
. 
. 
. 
HEADINGS MOVE      "00" TO LINECNTR
         ADD       "1" TO PAGENUM
         MOVE      SYSDATE TO DATE
         PRINT     *F,*20,"MAILDATE MASTER LISTING       DATE ",DATE
         PRINT     *N,*N
         RETURN
. 
. 
. 
DETAIL   MOVE      LSNO TO LISTNUM
         MOVE      NAME TO CLIENT
         MOVE      ACRONYM TO ACRONIM
         COMPARE   LISTNUM TO ZEROS
         GOTO      EDIT2 IF EQUAL
         GOTO      EDIT1 
CONTINU  ADD       "1" TO LINECNTR
         CMATCH    LETTER TO NAME
         GOTO      BREAK IF NOT EQUAL
         CALL      PRINTDET
. 
. 
. 
ENDPAGE  COMPARE   LINECNTR TO LINECHK
         GOTO      PERFORM IF NOT EQUAL
         GOTO      PRINTP
. 
. 
. 
PRINTP   PRINT     *N,*36,"PAGE - ",PAGENUM
         CALL      HEADINGS
         GOTO      PERFORM
. 
. 
. 
. 
. 
. 
BREAK    
         CMATCH    "Z" TO LETTER
         GOTO      LASTPAGE IF EQUAL
         REP       STRING1  IN LETTER
         REP       STRING  IN LETTER
         COMPARE   "0" TO LINECNTR
         GOTO      SKIP2 IF EQUAL
         GOTO      BRAKLINE
. 
. 
. 
BREAKP   PRINT     *N,*36,"PAGE - ",PAGENUM
         CALL      HEADINGS
         CALL      PREP
         GOTO      DETAIL
. 
. 
BRAKLINE ADD       "1" TO LINECNTR
         PRINT     *C
         COMPARE   LINECNTR TO LINECHK
         GOTO      BREAKP IF EQUAL
         GOTO      BRAKLINE
. 
. 
. 
. 
BREAK1   NORETURN
         GOTO      SKIP
. 
. 
PREP
         PACK      AKEY2 FROM KEY2,LETTER,QUES
         CLEAR     AKEY1
         CLEAR     AKEY3
         READ      ACROMST,AKEY1,AKEY2,AKEY3;LSNO,NAME,ACRONYM,DATE1,MLRNUM
         GOTO      BREAK1 IF OVER
         RETURN
. 
. 
LASTLINE ADD       "1" TO LINECNTR
         PRINT     *C
         COMPARE   LINECNTR TO LINECHK
         GOTO      THATSIT IF EQUAL
         GOTO      LASTLINE
. 
. 
PRINTDET PRINT     *2,LISTMASK,"  ",CLIENT,TWO,THIRTEEN," ",ACRONIM,"  ",DATE1
         RETURN
. 
. 
. 
THATSIT  PRINT     *N,*36,"PAGE - ",PAGENUM
         GOTO      EOJ
. 
. 
. 
. 
ZEROS    MOVE      ZEROS TO LSNO
         RETURN
. 
. 
. 
SKIP     REP       STRING1 IN LETTER
         REP       STRING IN LETTER
         CALL      PREP
         GOTO      DETAIL
. 
. 
. 
SKIP2    CALL      PREP
         GOTO      DETAIL
. 
. 
LASTPAGE GOTO      LASTLINE
. 
. 
EDIT1    MOVE      "ZZZZZ9" TO LISTMASK
         EDIT      LISTNUM TO LISTMASK
         GOTO      CONTINU
. 
. 
EDIT2    MOVE      "ZZZZZZ" TO LISTMASK
         EDIT      LISTNUM TO LISTMASK
         GOTO      CONTINU
. 
.  
. 
. 
EOJ                
         CLOSE     ACROMST
         SPLCLOSE  
         DISPLAY   *P1:1,*ES,*P10:12,"JOB DONE!!",*W2
         STOP
. 
. 
. 
ABORT    BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED BY USER !!!",*W3
         GOTO      EOJ
.
         INCLUDE   COMLOGIC.inc


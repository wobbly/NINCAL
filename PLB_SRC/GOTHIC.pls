.PROGRAM GOTHIC   PRINTS GOTHIC LETTERS    LOUIS J BOOKBINDER  16 FEB 83
.C1       DIM       1
IN       IFILE     VAR=130,KEYLEN=3
KEY      DIM       3
LINE     DIM       120
LNO      FORM      2
ONE      FORM      " 1"
PTR      DIM       5
SP       INIT      " "
SPLNAME  DIM       13
T1       DIM       1
TEXT     DIM       40
XPTR     INIT      "lpt1:"
prfile  pfile

          include   cons.inc
.---------------------------------
         OPEN      IN,"GOTHIC",READ
         KEYIN     *ES,*H 30,"GOTHIC PRINT PROGRAM":
                   *P1:5,"ENTER THE NAME OF THE PRINTER YOU WILL USE ":
                   "(L if local): ",PTR
         CMATCH    "L" WITH PTR
         GOTO      NOSPOOL IF EOS
         GOTO      NOSPOOL IF EQUAL
.         PACK      SPLNAME WITH XPTR,PTR
         PACK      SPLNAME WITH XPTR
         TRAP      BADPTR IF SPOOL
.         splopen   "\\nins1\e\data\gothic.lst"
.         Splopen   "Laser6"
.         SPLOPEN   SPLNAME,"Q"
NOSPOOL  
.          PRINT     *F
         PRTOPEN PrFile,"","GOTHIC"
         prtpage prfile;*UNITS=*HIENGLISH,*ORIENT=*LANDSCAPE;

ASK      KEYIN     *P1:10,"ENTER THE TEXT YOU WISH TO PRINT: ":
                   "________________________________________":
                   *HA -40,*IT,*RV,TEXT,*IN
         GOTO      SCAN IF NOT EOS
         CMATCH    "L" WITH PTR
         GOTO      EXIT IF EOS
         GOTO      EXIT IF EQUAL
.         SPLCLOSE
          prtclose  prfile
EXIT     CLOSE     IN
.         splclose
.         release
          prtclose  prfile
         STOP
.
BADPTR   DISPLAY   *P1:24,*B,"NO SUCH PRINTER: ",PTR
         CLOSE     IN
         STOP
.
SCAN     MOVE      TEXT TO str1
         PACK      KEY WITH str1,ONE
         CALL      PRINT
         BUMP      TEXT
         GOTO      SCAN IF NOT EOS
         RESET     TEXT
.         PRINT     *FLUSH
         GOTO      ASK
.
. print a letter
.
PRINT    
.          PRINT     *N,*N
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row

         CMATCH    SP WITH str1
         GOTO      PRSP IF EQUAL
         READ      IN,KEY;T1,LNO,LINE
         GOTO      NOSUCH IF OVER
PRINT1   
.PRINT     LINE
         prtpage    prfile;*p1:row,Line
         READKS    IN;T1,LNO,LINE
         RETURN    IF OVER
        add     eightlpi,row
         CMATCH    str1 WITH T1
         GOTO      PRINT1 IF EQUAL
         RETURN
.
. print a space
.
PRSP
.         PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*N,*N
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
         RETURN
.
NOSUCH   DISPLAY   *P1:24,"NO SUCH CHARACTER: ",str1;
         RETURN
VER
         CMATCH    str1 WITH T1
         GOTO      PRINT1 IF EQUAL
         RETURN
.
. print a space
.
PRSP
.PRINT     *N,*N,*N,*N,*N,*N,*N,*N,*N,*N,*N
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
         RETURN
.
NOSUCH   DISPLAY   *P1:24,"NO SUCH CHARACTER: "

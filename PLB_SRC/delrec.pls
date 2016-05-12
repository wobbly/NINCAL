PC         EQU         0
         INCLUDE   COMMON.inc
           INCLUDE   CONS.inc
Release	Init	"2.0"	.DLH add file manager
reldate	Init	"2014 March 18"
.release  init      "it works"
FILE1    IFILE     KEYLEN=1
FILE2    IFILE     KEYLEN=2
FILE3    IFILE     KEYLEN=3
FILE4    IFILE     KEYLEN=4
FILE5    IFILE     KEYLEN=5
FILE6    IFILE     KEYLEN=6,comp,var=3002
FILE7    IFILE     KEYLEN=7
FILE8    IFILE     KEYLEN=8
FILE9    IFILE     KEYLEN=9
FILE10   IFILE     KEYLEN=10
FILE11    IFILE     KEYLEN=11
FILE12    IFILE     KEYLEN=12
FILE13   IFILE    KEYLEN=13
FILE14   IFILE     KEYLEN=14
FILE15    IFILE     KEYLEN=15
FILE16    IFILE     KEYLEN=16
FILE17    IFILE     KEYLEN=17
INFILE   IFILE     KEYLEN=6
FILE18   IFILE     KEYLEN=18,VAR=500
FILE19    IFILE     KEYLEN=19
FILE20    IFILE     KEYLEN=20
FILE21    IFILE     KEYLEN=21
FILE22   IFILE     KEYLEN=22
FILE29   IFILE     KEYLEN=29
KEY1     DIM       1
KEY2     DIM       2
KEY3     DIM       3
KEY4     DIM       4
KEY5     DIM       5
KEY6     DIM       6
KEY7     DIM       7
KEY8     DIM       8
KEY9     DIM       9
KEY10    DIM       10
KEY11    DIM       11
KEY12    DIM       12
KEY13    DIM       13
KEY14    DIM       14
KEY15    DIM       15
KEY16    DIM       16
KEY17    DIM       17
KEY18    DIM       18
KEY19    DIM       19
KEY20    DIM       20
KEY21    DIM       21
KEY22    DIM       22
KEY29    DIM       29
PASS     DIM       5
NAME     DIM       50
manager	 Init	   ".isi|NINS1:502"		
KEYLEN   FORM      2
ANS      DIM       1
KEY      DIM       30
lr       dim       6
dim2     dim        2
PASSCD   move      "DELREC" TO PROGRAM
	winshow
           MOVE      "NAMES IN THE NEWS" TO COMPNME
           MOVE      "DELETE RECORDS" TO STITLE
           CALL      PAINT
.          DISPLAY   *P1:1,*EF,"RECORD DELETION PROGRAM"
         KEYIN     *P1:4,"PLEASE ENTER PASSWORD: ",*EOFF,PASS,*EON
         RESET     PASS TO 4
         RESET     PASS
         MATCH     "COSMO",PASS
         GOTO      END1 IF NOT EQUAL
         TRAP      END1 IF INT
         TRAP      RMS IF F3
WHTFILE  KEYIN     *P1:4,*EF,"WHAT FILE DO YOU WANT TO DELETE RECORDS":
                   " FROM? ",NAME
         MATCH     "*",NAME
         GOTO      QUES IF EOS
         GOTO      QUES IF EQUAL
	pack	name from name,manager
.         TRAP      NOFILE GIVING ERROR IF IO
.         OPEN      INFILE,NAME
. 
KEYLENTH KEYIN     *P1:5,*EF,"HOW LONG IS THE INDEX KEY? ":
                   *DE,*ZF,*JR,KEYLEN
         DISPLAY   *P1:7,"NUMBER:":
                      *P1:8,"(this number WILL NOT be zero-filled or right":
                      "-justified....enter exact key)",*EF
         BRANCH    KEYLEN OF K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13:
                   K14,K15,K16,K17,K18,K19,k20,k21,k22,keylenth,keylenth,keylenth,keylenth,keylenth:
                   keylenth,k29
         COMPARE   "00",KEYLEN
         GOTO      QUES IF EQUAL
         GOTO      KEYLENTH
K1       KEYIN     *P8:7,KEY1
         BUMP      KEY BY 18
         DISPLAY   *P21:7,"AFTER BUMP,KEY=",KEY
         MOVE      KEY1,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE1,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE1
         READ      FILE1,KEY;;
         GOTO      KEY1 IF OVER
         DELETE    FILE1,KEY
         TRAPCLR   IO
         CLOSE     FILE1
         GOTO      QUES
KEY1     RESET     KEY
         BUMP      KEY BY 18
         MOVE      KEY1 TO KEY
         FILEPI    1;FILE1
         DELETEK   FILE1,KEY
         TRAPCLR   IO
         CLOSE     FILE1
         GOTO      NOREC IF OVER
K2       KEYIN     *P8:7,KEY2
         BUMP      KEY BY 17
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY2,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE2,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE2
         READ      FILE2,KEY;;
         GOTO      KEY2 IF OVER
         DELETE   FILE2,KEY
         TRAPCLR   IO
         CLOSE     FILE2
         GOTO      QUES
KEY2     RESET     KEY
         BUMP      KEY BY 17
         MOVE      KEY2 TO KEY
         FILEPI    1;FILE2
         DELETEK   FILE2,KEY
         TRAPCLR   IO
         CLOSE     FILE2
         GOTO      NOREC IF OVER
K3       KEYIN     *P8:7,KEY3
         BUMP      KEY BY 16
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY3,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE3,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE3
         READ      FILE3,KEY;;
         GOTO      KEY3 IF OVER
         DELETE   FILE3,KEY
         TRAPCLR   IO
         CLOSE     FILE3
         GOTO      QUES
KEY3     RESET     KEY
         BUMP      KEY BY 16
         MOVE      KEY3 TO KEY
         FILEPI    1;FILE3
         DELETEK   FILE3,KEY
         TRAPCLR   IO
         CLOSE     FILE3
         GOTO      NOREC IF OVER
         GOTO      QUES
K4       KEYIN     *P8:7,KEY4
         BUMP      KEY BY 15
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY4,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE4,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE4
         READ      FILE4,KEY;;
         GOTO      KEY4 IF OVER
         DELETE   FILE4,KEY
         TRAPCLR   IO
         CLOSE     FILE4
         GOTO      QUES
KEY4     RESET     KEY
         BUMP      KEY BY 15
         MOVE      KEY4 TO KEY
         FILEPI    1;FILE4
         DELETEK   FILE4,KEY
         TRAPCLR   IO
         CLOSE     FILE4
         GOTO      NOREC IF OVER
         GOTO      QUES
K5       KEYIN     *P8:7,KEY5
         BUMP      KEY BY 14
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY5,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE5,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE5
         READ      FILE5,KEY;;
         GOTO      KEY5 IF OVER
         DELETE   FILE5,KEY
         TRAPCLR   IO
         CLOSE     FILE5
         GOTO      QUES
KEY5     RESET     KEY
         BUMP      KEY BY 14
         MOVE      KEY5 TO KEY
         FILEPI    1;FILE5
         DELETEK   FILE5,KEY
         TRAPCLR   IO
         CLOSE     FILE5
         GOTO      NOREC IF OVER
         GOTO      QUES
K6       KEYIN     *P8:7,KEY6
         BUMP      KEY BY 13
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY6,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE6,NAME
         TRAP      KEY6 GIVING ERROR IF IO
         FILEPI    3;FILE6
         READ      FILE6,KEY;;
         GOTO      KEY6 IF OVER
         DELETE   FILE6,KEY
         TRAPCLR   IO
         CLOSE     FILE6
         GOTO      QUES
KEY6     DISPLAY   *P1:24,*EL,*HON,"NO RECORD I'LL DELETE ISAM KEY",*B,*W:
                   *HOFF,*P1:24,*EL;
         TRAP      IO GIVING ERROR IF IO
         RESET     KEY
         BUMP      KEY BY 13
         MOVE      KEY6 TO KEY
         FILEPI    1;FILE6
         DELETEK   FILE6,KEY
         TRAPCLR   IO
         CLOSE     FILE6
         GOTO      NOREC IF OVER
         GOTO      QUES
K7       KEYIN     *P8:7,KEY7
         BUMP      KEY BY 12
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY7,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE7,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    1;FILE7
         READ      FILE7,KEY;;
         GOTO      key7 if over
         filepi    1;file7
         DELETE   FILE7,KEY
         TRAPCLR   IO
         CLOSE     FILE7
         GOTO      QUES
KEY7     RESET     KEY
         BUMP      KEY BY 12
         MOVE      KEY7 TO KEY
         FILEPI    1;FILE7
         DELETEK   FILE7,KEY
         TRAPCLR   IO
         CLOSE     FILE7
         GOTO      NOREC IF OVER
         GOTO      QUES
K8       KEYIN     *P8:7,KEY8
         BUMP      KEY BY 11
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY8,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
          pack      str255 from name,"|NINS1:502"
         OPEN      FILE8,str255
.         GOTO      KEY8
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE8
         READ      FILE8,KEY;;
         GOTO      KEY8 IF OVER
         DELETE    FILE8,KEY
         TRAPCLR   IO
         CLOSE     FILE8
         GOTO      QUES
KEY8     RESET     KEY
         BUMP      KEY BY 11
         MOVE      KEY8 TO KEY
         FILEPI    1;FILE8
         DELETEK   FILE8,KEY
         TRAPCLR    IO
         CLOSE     FILE8
         GOTO      NOREC IF OVER
         GOTO      QUES
K9       KEYIN     *P8:7,KEY9
         BUMP      KEY BY 10
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY9,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K10      KEYIN     *P8:7,KEY10
         BUMP      KEY BY 9
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY10,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE10,NAME
.         GOTO      KEY8
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE10
         READ      FILE10,KEY;;
         GOTO      KEY10 IF OVER
         DELETE    FILE10,KEY
         TRAPCLR   IO
         CLOSE     FILE10
         GOTO      QUES
KEY10    RESET     KEY
         BUMP      KEY BY 9
         MOVE      KEY10 TO KEY
         FILEPI    1;FILE10
         DELETEK   FILE10,KEY
         TRAPCLR    IO
         CLOSE     FILE10
         GOTO      NOREC IF OVER
         GOTO      QUES
K11      KEYIN     *P8:7,KEY11
         BUMP      KEY BY 8
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY11,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K12      KEYIN     *P8:7,KEY12
         BUMP      KEY BY 7
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY12,KEY
         DISPLAY   *P20:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K13      KEYIN     *P8:7,KEY13
         BUMP      KEY BY 6
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY13,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE13,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE13
         READ      FILE13,KEY;;
         GOTO      KEY13 IF OVER
         DELETE    FILE13,KEY
         CLOSE     FILE13
         TRAPCLR   IO
         GOTO      QUES
KEY13    RESET     KEY
         BUMP      KEY BY 6
         MOVE      KEY13 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE13
         DELETEK    FILE13,KEY
         CLOSE     FILE13
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K14      KEYIN     *P8:7,KEY14
         BUMP      KEY BY 5
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY14,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE14,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE14
         READ      FILE14,KEY;;
         GOTO      KEY14 IF OVER
         DELETE    FILE14,KEY
         CLOSE     FILE14
         TRAPCLR   IO
         GOTO      QUES
KEY14    RESET     KEY
         BUMP      KEY BY 5
         MOVE      KEY14 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE14
         DELETEK    FILE14,KEY
         CLOSE     FILE14
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K15      KEYIN     *P8:7,KEY15
         BUMP      KEY BY 4
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY15,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K16      KEYIN     *P8:7,KEY16
         BUMP      KEY BY 3
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY16,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K17      KEYIN     *P8:7,KEY17
         BUMP      KEY BY 2
.         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY17,KEY
         DISPLAY   *P20:7,"AFTER MOVE, KEY=",KEY
         GOTO      DELETE
K18      KEYIN     *P8:7,KEY18
         BUMP      KEY BY 1
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY18,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE18,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE18
         READ      FILE18,KEY;;
         GOTO      KEY18 IF OVER
         DELETE    FILE18,KEY
         CLOSE     FILE18
         TRAPCLR   IO
         GOTO      QUES
KEY18    RESET     KEY
         BUMP      KEY BY 1
         MOVE      KEY18 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE18
         DELETEK    FILE18,KEY
         CLOSE     FILE18
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K19      KEYIN     *P8:7,KEY19
         MOVE      KEY19,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
DELETE
         TRAP      IO GIVING ERROR IF IO
         OPEN      INFILE,NAME
         READ      INFILE,KEY;;
         GOTO      NOREC IF OVER
         DELETE    INFILE,KEY
         TRAPCLR   IO
         CLOSE     INFILE
         GOTO      QUES
K20      KEYIN     *P8:7,KEY20
         BUMP      KEY BY 1
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY20,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE20,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE20
         READ      FILE20,KEY;;
         GOTO      KEY20 IF OVER
         DELETE    FILE20,KEY
         CLOSE     FILE20
         TRAPCLR   IO
         GOTO      QUES
KEY20    RESET     KEY
         BUMP      KEY BY 1
         MOVE      KEY20 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE20
         DELETEK    FILE20,KEY
         CLOSE     FILE20
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K21      KEYIN     *P8:7,KEY21
         BUMP      KEY BY 1
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY21,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE21,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE21
         READ      FILE21,KEY;;
         GOTO      KEY21 IF OVER
         DELETE    FILE21,KEY
         CLOSE     FILE21
         TRAPCLR   IO
         GOTO      QUES
KEY21    RESET     KEY
         BUMP      KEY BY 1
         MOVE      KEY21 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE21
         DELETEK    FILE21,KEY
         CLOSE     FILE21
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K22      KEYIN     *P8:7,KEY22
         BUMP      KEY BY 1
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY22,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE22,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE22
         READ      FILE22,KEY;;
         GOTO      KEY22 IF OVER
         DELETE    FILE22,KEY
         CLOSE     FILE22
         TRAPCLR   IO
         GOTO      QUES
KEY22    RESET     KEY
         BUMP      KEY BY 1
         MOVE      KEY22 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE22
         DELETEK    FILE22,KEY
         CLOSE     FILE22
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
K29      KEYIN     *P8:7,KEY29
         BUMP      KEY BY 1
         DISPLAY   *P20:7,"AFTER BUMP, KEY=",KEY
         MOVE      KEY29,KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         TRAP      NOFILE GIVING ERROR IF IO
         OPEN      FILE29,NAME
         TRAP      IO GIVING ERROR IF IO
         FILEPI    3;FILE29
         READ      FILE29,KEY;;
         GOTO      KEY29 IF OVER
         DELETE    FILE29,KEY
         CLOSE     FILE29
         TRAPCLR   IO
         GOTO      QUES
KEY29    RESET     KEY
         BUMP      KEY BY 1
         MOVE      KEY29 TO KEY
         DISPLAY   *P50:7,"AFTER MOVE, KEY=",KEY
         DISPLAY   *P1:24,*EL,"NO RECORD WILL DELETE KEY",*B;
         FILEPI    1;FILE29
         DELETEK    FILE29,KEY
         CLOSE     FILE29
         GOTO      NOREC IF OVER
         TRAPCLR   IO
         GOTO      QUES
NOREC    BEEP
         BEEP
         DISPLAY   *P1:24,*EL,"NO RECORD WITH THIS KEY ON FILE !!!!!",*W;
         DISPLAY   *W,*W;
         GOTO      QUES
NOFILE   BEEP
         BEEP
         DISPLAY   *P1:23,*EL,"ERROR IS",ERROR
         DISPLAY   *P1:24,*EL,"FILE NOT ONLINE !!",*W,*W,*W;
         TRAPCLR   IO
         GOTO      WHTFILE
IO
         BEEP
         BEEP
         DISPLAY   *P1:23,*EL,"ERROR IS",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROr !!",*W,*W,*W;
         TRAPCLR   IO
         GOTO      WHTFILE
. 
QUES     DISPLAY   *P1:13,*EL,"1 - NEW FILE NAME....2 - NEW INDEX ":
                      "LENGTH....3 - ANOTHER RECORD....4 - END JOB"
         KEYIN     *P80:13,ANS
         CMATCH    "1",ANS
         GOTO      WHTFILE IF EQUAL
         CMATCH    "2",ANS
         GOTO      KEYLENTH IF EQUAL
         CMATCH    "3",ANS
         GOTO      C4 IF NOT EQUAL
         BRANCH    KEYLEN OF K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13:
                   K14,K15,K16,K17,K18,K19,k20,k21,k22,keylenth,keylenth,keylenth,keylenth,keylenth:
                   keylenth,k29
C4       GOTO      END1
. 
END1      STOP
. 
.>DOS      SHUTDOWN  "FREE"
RMS      STOP
           include     comlogic.inc


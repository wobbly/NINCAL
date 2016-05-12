.; Do a fancy clock or counter program
 
A        EQU       185
B        EQU       186
C        EQU       187                 
D        EQU       188
E        EQU       200
F        EQU       201
G        EQU       202
H        EQU       203
I        EQU       204
J        EQU       205
K        EQU       206
L        EQU       191
M        EQU       192
N        EQU       217
O        EQU       218
P        EQU       179
$        EQU       32
.W1UL     FORM
TIME     DIM     8
LASTTIME INIT    "  :  :  "
INDEX    FORM    1
DIM1     INIT    " "
RIGHT    FORM    2
LEFT     FORM    2
ALARM    INIT      "  :00:00"
ALARMX   FORM      1
.         PARSE     S$CMDLIN,ALARM,"09::"
         SETLPTR   ALARM
.         TRAP      STOP IF ESCAPE
         TRAP      RED NORESET IF F1
         TRAP      GREEN NORESET IF F2
         TRAP      BLUE NORESET IF F3
         TRAP      YELLOW NORESET IF F4
         TRAP      MAGENTA NORESET IF F5
         TRAP      CYAN NORESET IF F6
         TRAP      WHITE NORESET IF F7
         TRAP      BLACK NORESET IF F8
         TRAP      REDX NORESET IF F9
         TRAP      GREENX NORESET IF F10
.         TRAP      BLUEX NORESET IF F13
.         TRAP      YELLOWX NORESET IF F14
.         TRAP      MAGENTAX NORESET IF F15
.         TRAP      CYANX NORESET IF F16
.         TRAP      WHITEX NORESET IF F17
.         TRAP      BLACKX NORESET IF F18
START
         MOVE      "  :  :  ",LASTTIME
         NORETURN
         DISPLAY   *RESETSW,*ES:
                       *SETSWTB 3:15:
                       *P26:3,O,L:
                       *P26:4,P,P:
                       *P26:5,M,N:
                       *P26:9,O,L:
                       *P26:10,P,P:
                       *P26:11,M,N:
                       *P54:3,O,L:
                       *P54:4,P,P:
                       *P54:5,M,N:
                       *P54:9,O,L:
                       *P54:10,P,P:
                       *P54:11,M,N;
TIMELOOP
         CLOCK   TIME,TIME
         MATCH   TIME,LASTTIME
         GOTO    TIMELOOP IF EQUAL
         MATCH     ALARM,TIME
         GOTO      TIMELOOP1 IF NOT EQUAL
         MOVE      "1",ALARMX
TIMELOOP1
         RESET   TIME,8
         RESET   LASTTIME,8
         CMOVE   TIME,LASTTIME
         MOVE    "71",LEFT
         MOVE    "79",RIGHT
         CALL    PUTCHAR
.
         RESET   TIME,7
         RESET   LASTTIME,7
         CMATCH  TIME,LASTTIME
         GOTO    ENDLOOP IF EQUAL
         CMOVE   TIME,LASTTIME
         MOVE    "58",LEFT
         MOVE    "66",RIGHT
         CALL    PUTCHAR
.
         RESET   TIME,5
         RESET   LASTTIME,5
         CMATCH  TIME,LASTTIME
         GOTO    ENDLOOP IF EQUAL
         CMOVE   TIME,LASTTIME
         MOVE    "43",LEFT
         MOVE    "51",RIGHT
         CALL    PUTCHAR
.
         RESET   TIME,4
         RESET   LASTTIME,4
         CMATCH  TIME,LASTTIME
         GOTO    ENDLOOP IF EQUAL
         CMOVE   TIME,LASTTIME
         MOVE    "30",LEFT
         MOVE    "38",RIGHT
         CALL    PUTCHAR
.
         RESET   TIME,2
         RESET   LASTTIME,2
         CMATCH  TIME,LASTTIME
         GOTO    ENDLOOP IF EQUAL
         CMOVE   TIME,LASTTIME
         MOVE    "15",LEFT
         MOVE    "23",RIGHT
         CALL    PUTCHAR
.
         RESET   TIME,1
         RESET   LASTTIME,1
         CMATCH  TIME,LASTTIME
         GOTO    ENDLOOP IF EQUAL
         CMOVE   TIME,LASTTIME
         MOVE    "2",LEFT
         MOVE    "10",RIGHT
         CALL    PUTCHAR
ENDLOOP
         RESET   LASTTIME
         PERFORM   ALARMX,ALARM
         GOTO    TIMELOOP
ALARM
         BEEP
         RETURN
STOP
         STOP
PUTCHAR
         CMOVE   TIME,DIM1
         MOVE    DIM1,INDEX
         DISPLAY *P1:1,*SETSWLR LEFT:RIGHT;
         BRANCH  INDEX,ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE
zero     DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
one      DISPLAY $,$,F,J,C,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 J,J,J,J,G,J,J,J,J;
         RETURN
two      DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 F,J,J,J,J,J,J,J,D,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 E,J,J,J,J,J,J,J,J;
         RETURN
three    DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,J,J,J,J,J,J,A,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
four     DISPLAY B,$,$,$,$,$,$,B,$,*N:
                 B,$,$,$,$,$,$,B,$,*N:
                 B,$,$,$,$,$,$,B,$,*N:
                 B,$,$,$,$,$,$,B,$,*N:
                 B,$,$,$,$,$,$,B,$,*N:
                 B,$,$,$,$,$,$,B,$,*N:
                 E,J,J,J,J,J,J,K,J,*N:
                 $,$,$,$,$,$,$,B,$,*N:
                 $,$,$,$,$,$,$,B,$,*N:
                 $,$,$,$,$,$,$,B,$,*N:
                 $,$,$,$,$,$,$,B,$,*N:
                 $,$,$,$,$,$,$,B,$,*N:
                 $,$,$,$,$,$,$,B,$;
         RETURN
five     DISPLAY F,J,J,J,J,J,J,J,J,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 E,J,J,J,J,J,J,J,C,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
six      DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 B,$,$,$,$,$,$,$,$,*N:
                 I,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
seven    DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,F,J,J,J,D,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$,*N:
                 $,$,$,$,B,$,$,$,$;
         RETURN
eight    DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 I,J,J,J,J,J,J,J,A,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
nine     DISPLAY F,J,J,J,J,J,J,J,C,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,A,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 $,$,$,$,$,$,$,$,B,*N:
                 B,$,$,$,$,$,$,$,B,*N:
                 E,J,J,J,J,J,J,J,D;
         RETURN
RED
         DISPLAY   *DION,*RED;
         GOTO      START
GREEN
         DISPLAY   *DION,*GREEN;
         GOTO      START
BLUE
         DISPLAY   *DION,*BLUE;
         GOTO      START
YELLOW
         DISPLAY   *DION,*YELLOW;
         GOTO      START
MAGENTA
         DISPLAY   *DION,*MAGENTA;
         GOTO      START
CYAN
         DISPLAY   *DION,*CYAN;
         GOTO      START
WHITE
         DISPLAY   *DION,*WHITE;
         GOTO      START
BLACK
         DISPLAY   *DION,*BLACK;
         GOTO      START
REDX
         DISPLAY   *HON,*RED,*HOFF;
         GOTO      START
GREENX
         DISPLAY   *HON,*GREEN,*HOFF;
         GOTO      START
BLUEX
         DISPLAY   *HON,*BLUE,*HOFF;
         GOTO      START
YELLOWX
         DISPLAY   *HON,*YELLOW,*HOFF;
         GOTO      START
MAGENTAX
         DISPLAY   *HON,*MAGENTA,*HOFF;
         GOTO      START
CYANX
         DISPLAY   *HON,*CYAN,*HOFF;
         GOTO      START
WHITEX
         DISPLAY   *HON,*WHITE,*HOFF;
         GOTO      START
BLACKX
         DISPLAY   *HON,*BLACK,*HOFF;
         GOTO      START


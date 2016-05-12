         INC       COMMON.INC
.N1       FORM      *16
N1       FORM      16
F1       FILE
S1       DIM       80
N2       FORM      "00"
N3       FORM      "00"
S2       INIT      "TALKTEX.DAT"
N4       FORM      "-1"
+
         TRAP      X1 IF IO
         OPEN      F1,S2
         TRAPCLR   IO
         DISPLAY   *ES,*P1:22,"ENTER WAIT TIME (IN SECONDS) BETW":
                   "EEN EACH DEFINITION :"
         DISPLAY   *P1:23,"(F5) to exit"
X2       KEYIN     *P58:22,*EL,N2
         COMPARE   "1",N2
         GOTO      X2 IF LESS
         COMPARE   "61",N2
         GOTO      X2 IF NOT LESS
         DISPLAY   *ES
X3       CLOSE     F1
         OPEN      F1,S2
X6       TRAP      EOJ IF F5
         READ      F1,N4;S1
         GOTO      X3 IF OVER
         CMATCH    ".",S1
         GOTO      X4 IF NOT ZERO
         MOVE      N2,N3
X5       DISPLAY   *W
         SUB       "1",N3
         GOTO      X5 IF NOT ZERO
         DISPLAY   *ES
         GOTO      X6
X4       DISPLAY   *P1:24,*R,S1
         GOTO      X6
X1       BEEP
         DISPLAY   *ES,*P1:22,"TEXAS TALK NOT AVAILABLE ===> ",S2:
                   " NOT FOUND",*W,*W,*W,*W,*W,*W,*W,*W,*W,*W,*ES
EOJ      CHAIN     "MASTER"

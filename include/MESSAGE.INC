.
.MESSAGE/INC - FIND UPCOMING HOLIDAY'S.
.
MESSG
         DISPLAY   *P1:1,*ES
.	 DISPLAY   *P10:12,*RED,"DISCLAIMER: ":
.		    *N,*RED,"THE FOLLOWING MESSAGES ARE NOT MEANT ":
.	           *N,*YELLOW,"TO IMPLY SUPPORT FOR,  ":
.		   *N,*GREEN,"DAYS OFF GIVEN BECAUSE OF, OR ":
.	           *N,*BROWN,"CORRECT DATES FOR THE HOLIDAYS MENTIONED":
.		   *N,*CYAN,"JUST HAVE FUN WITH IT!",*W,*B,*R,*R,*R,*R,*R,*R
.         RETURN
	 CALL      PAINT
         MOVE      MM TO NMM
         MOVE      DD TO NDD
         BRANCH    NMM OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
         DISPLAY   *P1:24,*EL,"I'M LOST AS FAR AS HOLIDAY'S???",*W4,*B
         RETURN
JAN      return
      COMPARE   "20" TO NDD
         GOTO      FEB1 IF NOT LESS
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," MARTIN LUTHER KING DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
FEB      return
         COMPARE   "17" TO NDD
         GOTO      MAR3 IF NOT LESS
FEB1     return
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," PRESIDENT'S DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
MAR      return
         COMPARE   "17" TO NDD
         GOTO      MAR3 IF LESS
         COMPARE   "28" TO NDD
         GOTO      MAR2 IF NOT LESS
MAR1     return
         DISPLAY   *P15:24,*EL,*HON," GOOD FRIDAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
MAR2     return
         DISPLAY   *P15:24,*EL,*HON," EASTER is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
MAR3     return
         COMPARE   "2" TO NMM
         GOTO      MAR3A IF EQUAL
         COMPARE   "17" TO NDD
         GOTO      MAR1 IF NOT LESS
MAR3A    return
         DISPLAY   *P15:24,*EL,*HON," St. Patricks day is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
APR      return
         COMPARE   "24" TO NDD
         GOTO      MAY IF NOT LESS
         DISPLAY   *P15:24,*EL,"Passover is coming up soon."
         GOTO      MOMDAY
         
MAY      return
         COMPARE   "4" TO NMM
         GOTO      MOMDAY IF EQUAL
         COMPARE   "11" TO NDD
         GOTO      MAY1 IF NOT LESS
MOMDAY   DISPLAY   *P15:22,*R,*EL,*HON," Mother's day is coming soon.":
                   *HOFF
         RETURN
MAY1     COMPARE   "26" TO NDD
         GOTO      FLAGDAY IF NOT LESS
MEMDAY   DISPLAY   *P15:24,*EL,*HON," Memorial day is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
JUN      return
         COMPARE   "14" TO NDD
         GOTO      DADDAY IF NOT LESS
FLAGDAY  DISPLAY   *P15:24,*EL,*HON," Flag day is coming up":
                   " soon.",*B,*W,*HOFF
DADDAY   COMPARE   "15" TO NDD
         GOTO      JUL4 IF NOT LESS
         DISPLAY   *P15:24,*EL,*HON," Father's day is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
JUL      return
         COMPARE   "4" TO NDD
         GOTO      AUG IF NOT LESS
JUL4     DISPLAY   *P15:24,*EL,*HON," THE FOURTH is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
AUG      return
         DISPLAY   *P1:1,*ES,*P15:24,*HON,"LABOR DAY is coming soon!",*B,*W:
                   *HOFF
         RETURN
SEP      return
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," COLUMBUS DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
OCT      COMPARE   "10" TO NDD
         GOTO      OCT1 IF NOT LESS
         return
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," COLUMBUS DAY is coming up":
                   " soon.",*B,*W,*R
         DISPLAY   *P15:24,*EL,*HON,"  YOM KIPPUR is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
OCT1     return
. JACK-O-LANTERN.
	 DISPLAY   *P1:1,*Bgcolor *BLACK,*ES
	 DISPLAY   *RED
         DISPLAY   *P34:3,*HON," ",*P35:3," ",*P36:4," "," ",*RPTCHAR " ":4:
                   *P32:5,*RPTCHAR " ":15,*P30:6,*RPTCHAR " ":20:
                   *P29:7,*RPTCHAR " ":22,*P28:8,*RPTCHAR " ":25:
                   *P28:9,*RPTCHAR " ":25:
                   *P27:10,*RPTCHAR " ":27,*P27:11,*RPTCHAR " ":27:
                   *P27:12,*RPTCHAR " ":26,*P29:13,*RPTCHAR " ":24:
                   *P30:14,*RPTCHAR " ":22:
                   *P32:15,*RPTCHAR " ":19,*P36:16,*RPTCHAR " ":11:
                   *P35:7,*HOFF," ",*P44:7," ",*P34:8,*cyan,*dion,b1,004,b1,*P43:8,b1,004,b1,*dioff,*red:
                   *P39:9," ",*P38:10,"   ":
                   *P34:13,"  ",*P44:13,"  ",*P36:14,"        ",*W2
         DISPLAY   *P1:1,*RD,*RD,*RD,*RD,*SCRRIGHT COL24
         DISPLAY   *SCRRIGHT COL24,*SCRRIGHT COL24
         DISPLAY   *RD,*RD,*RD,*RD,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24,*R,*SCRRIGHT COL24
         DISPLAY   *P1:1,*ES,*HOFF,*Bgcolor *BLUE,*WHITE
         RETURN
NOV      return
         COMPARE   "11" TO NDD
         GOTO      NOV1 IF NOT LESS
. 
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," VETERANS DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
NOV1
         COMPARE   "27" TO NDD
         GOTO      DEC1 IF NOT LESS
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," THANKSGIVING DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
DEC      return
         COMPARE   "25" TO NDD
         GOTO      DEC2 IF NOT LESS
DEC1     DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," CHRISTMAS DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
DEC2
         DISPLAY   *P1:1,*ES
         DISPLAY   *P15:24,*EL,*HON," NEW YEARS DAY is coming up":
                   " soon.",*B,*W,*HOFF
         RETURN
.


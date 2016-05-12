pc       equ       0
         INC       COMMON.inc
	 include   cons.inc
release  init      "OK" 
time     init      "mm:dd:ss"
S1       INIT      "+"
S2       INIT      "***"
S3       INIT      "**O**"
S4       INIT      "*O***O*"
S5       INIT      "*****O*****"
S6       INIT      "**O***O**"
S7       INIT      "*****O*****"
S8       INIT      "**O*******O**"
S9       INIT      "******O***O******"
S10      INIT      "***O***O***O***"
S11      INIT      "******O***O******"
S12      INIT      "****O****O****O****"
S13      INIT      "********O*****O********"
S14      INIT      "****O*****O*****O****"
S15      INIT      "********O*****O********"
S16      INIT      "****O*******O*******O****"
S17      INIT      "*********O*******O*********"
S18      INIT      "********O****O***O****O********"
S19      INIT      "**********O*******O**********"
S20      INIT      "*******O*******O*******O*******"
S21      INIT      "*****O******O*******O******O*****"
S22      INIT      "**********O******O*******O*********"
S23      INIT      "***************************************"
S24      INIT      "III"
s25      init      0001
.S25      INIT      "O"
S26      INIT      " "
.LINE     INIT      007
.BAR      INIT      006
.CTL      INIT      002
.CTR      INIT      003
.CBL      INIT      004
.CBR      INIT      005
block     init      0260
+
	 keyin     *p1:24,*cl;
         TRAP      EOJ IF F5
         call      paint
         DISPLAY   *P1:1,*dioff,*BGcolor=7,*ES
.         display   *HON,*BLUE,I218,*RPTCHAR I196:78,I191:
.                   *P1:2,I179,*P80:2,I179:
.                   *P1:3,I179,*P80:3,I179:
.                   *P1:4,I179,*P80:4,I179:
.                   *P1:5,I179,*P80:5,I179:
.                   *P1:6,I179,*P80:6,I179:
.                   *P1:7,I179,*P80:7,I179:
.                   *P1:8,I179,*P80:8,I179:
.                   *P1:9,I179,*P80:9,I179:
.                   *P1:10,I179,*P80:10,I179:
.                   *P1:11,I179,*P80:11,I179:
.                   *P1:12,I179,*P80:12,I179:
.                   *P1:13,I179,*P80:13,I179:
.                   *P1:14,I179,*P80:14,I179:
.                   *P1:15,I179,*P80:15,I179:
.                   *P1:16,I179,*P80:16,I179:
.                   *P1:17,I179,*P80:17,I179:
.                   *P1:18,I179,*P80:18,I179:
.                   *P1:19,I179,*P80:19,I179:
.                   *P1:20,I179,*P80:20,I179:
.                   *P1:21,I179,*P80:21,I179:
.                   *P1:22,I179,*P80:22,I179:
.                   *P1:23,I179,*P80:23,I179:
.                   *P1:24,I192,*RPTCHAR I196:78,I217,*HOFF:
         display   *hon,*border=block
         display   *P2:22,"Use F5 key":
                   *P2:23,"to exit",*Bgcolor=2:
                   *P36:1,*blinkon,*YELLOW,S1,*blinkoff:
                   *WHITE,*P35:2,S2,*P34:3,S3,*P33:4,S4,*P31:5:
                   S5,*P32:6,S6,*P31:7,S7,*P30:8,S8,*P28:9,S9,*P29:10:
                   S10,*P28:11,S11,*P27:12,S12,*P25:13,S13,*P26:14:
                   S14,*P25:15,S15,*P24:16,S16,*P23:17,S17,*P21:18:
                   S18,*P22:19,S19,*P21:20,S20,*P20:21,S21,*P19:22:
                   S22,*P17:23,S23,*P35:24,*dioff,*bgcolor=6,S24,*bgcolor=2;
X1       DISPLAY   *dion,*P32:8,S26,*P36:14,S26,*P25:21,*green,S25,*white,*P44:20,S26:
                   *P28:16,S26,*P36:3,S26,*P38:6,S26,*P36:14,*color n2,S25,*white:
                   *color n2,s25,*white,*P44:16,S26:
                   *P25:21,S26,*P29:18,S26,*P36:12,S26,*P44:20,*color n2,s25:
                   *white:
                   *P34:18,S26,*P32:10,S26,*P36:3,*color n2,s25,*white,*P33:13,S26:
                   *P28:20,*color n2,s25,*white:
                   *P36:22,*color n2,s25,*white,*P44:22,S26,*P25:21,*color n2,s25,*white:
                   *P29:22,*green,S25,*white:
                   *P44:16,*color n2,s25,*white,*P38:4,S26,*P34:18,*color n2,s25,*white:
                   *P36:20,S26:
                   *P44:22,S26,*P32:10,*color n2,s25,*white,*P43:18,S26,*P38:4,S25:
                   *P29:22,S26:
                   *P36:12,*color n2,s25,*white,*P29:22,S26,*P36:16,S26,*P40:10,*green,S25,*white:
                   *P42:14,S26,*P33:13,*color n2,s25,*white,*P38:6,*color n2,s25,*white,*P34:4,S26:
                   *P40:8,S26,*P44:22,*color n2,s25,*white,*P47:21,S26,*P29:18,*green,S25,*white:
                   *P29:22,*color n2,s25,*white,*P28:16,*color n2,s25,*white:
                   *P29:22,S26,*P34:11,S26,*P34:4,*color n2,s25,*white,*P34:6,S26:
                   *P47:21,*color n2,s25,*white,*P32:19,S26,*P42:14,*color n2,s25,*white,*P33:15,S26:
                   *P36:16,*color n2,s25,*white,*P40:21,S26,*P36:20,*color n2,s25,*white,*P41:12,S26:
                   *P34:6,*color n2,s25,*white,*P36:7,S26,*P36:10,S26,*P33:15,*green,S25,*white:
                   *P40:21,*color n2,s25,*white,*P36:22,*color n2,s25,*white,*P43:18,*color n2,s25,*white,*P38:18,S26:
                   *P29:22,S26,*P36:22,S26:
                   *P36:5,S26,*P40:8,*color n2,s25,*white,*P34:9,S26,*P41:12,*green,S25,*white:
                   *P36:22,*color n2,s25,*white,*P30:14,S26,*P40:19,S26,*P32:19,*green,S25,*white:
                   *P29:22,*green,S25,*white:
                   *P34:9,*color n2,s25,*white,*P39:13,S26,*P32:17,S26,*P36:5,*green,S25,*white:
                   *P36:10,*color n2,s25,*white,*P40:19,*color n2,s25,*white,*P28:20,S26,*P32:21,S26:
                   *P36:7,*color n2,s25,*white,*P39:15,S26,*P40:17,S26,*P34:11,*green,S25,*white:
                   *P36:14,S26,*P32:8,S26,*P28:16,S26,*P38:11,S26:
                   *P31:12,S26,*P38:9,S26,*P32:21,*color n2,s25,*white,*P32:17,*green,S25,*white:
                   *P38:18,*color n2,s25,*white,*P40:17,*color n2,s25,*white,*P38:9,*color n2,s25,*white,*P39:15,*green,S25,*white:
                   *P30:14,*color n2,s25,*white,*P31:12,*color n2,s25,*white,*P39:13,*color n2,s25,*white,*P38:11,*green,S25,*white
.repeat before color change
X2       DISPLAY   *dion,*P32:8,S26,*P36:14,S26,*P25:21,*color n2,s25,*white:
                   *P44:20,S26:
                   *P28:16,S26,*P36:3,S26,*P38:6,S26,*P36:14,*color n2,S25:
                   *white:
                   *color n2,s25,*white,*P44:16,S26:
                   *P25:21,S26,*P29:18,S26,*P36:12,S26,*P44:20:
                   *color n2,s25,*white:
                   *P34:18,S26,*P32:10,S26,*P36:3,*color n2,s25,*white:
                   *P33:13,S26:
                   *P28:20,*color n2,s25,*white,*P36:22,*color n2,s25,*white,*P44:22,S26,*P25:21,*green,s25,*white:
                   *P29:22,*green,s25,*white:
                   *P44:16,*color n2,s25,*white,*P38:4,S26,*P34:18,*color n2,s25,*white,*P36:20,S26:
                   *P44:22,S26,*P32:10,*color n2,s25,*white,*P43:18,S26,*P38:4,*green,s25,*white:
                   *P29:22,S26:
                   *P36:12,*color n2,s25,*white,*P29:22,S26,*P36:16,S26,*P40:10,*green,s25,*white:
                   *P42:14,S26,*P33:13,*color n2,s25,*white,*P38:6,*color n2,s25,*white,*P34:4,S26:
                   *P40:8,S26,*P44:22,*color n2,s25,*white,*P47:21,S26,*P29:18,*green,s25,*white:
                   *P29:22,*green,s25,*white:
                   *P29:22,S26,*P34:11,S26,*P34:4,*color n2,s25,*white,*P34:6,S26:
                   *P47:21,*color n2,s25,*white,*P32:19,S26,*P42:14,*color n2,s25,*white,*P33:15,S26:
                   *P36:16,*color n2,s25,*white,*P40:21,S26,*P36:20,*color n2,s25,*white,*P41:12,S26:
                   *P34:6,*color n2,s25,*white,*P36:7,S26,*P36:10,S26,*P33:15,*green,s25,*white:
                   *P40:21,*color n2,s25,*white,*P36:22,*color n2,s25,*white,*P43:18,*color n2,s25,*white,*P38:18,S26:
                   *P29:22,S26,*P36:22,S26:
                   *P36:5,S26,*P40:8,*color n2,s25,*white,*P34:9,S26,*P41:12,*green,s25,*white:
                   *P36:22,*color n2,s25,*white,*P30:14,S26,*P40:19,S26,*P32:19,*green,s25,*white:
                   *P29:22,*color n2,s25,*white,*P28:16,*color n2,s25,*white:
                   *P34:9,*color n2,s25,*white,*P39:13,S26,*P32:17,S26,*P36:5,*green,s25,*white:
                   *P36:10,*color n2,s25,*white,*P40:19,*color n2,s25,*white,*P28:20,S26,*P32:21,S26:
                   *P36:7,*color n2,s25,*white,*P39:15,S26,*P40:17,S26,*P34:11,*green,s25,*white:
                   *P36:14,S26,*P32:8,S26,*P28:16,S26,*P38:11,S26:
                   *P31:12,S26,*P38:9,S26,*P32:21,*color n2,s25,*white,*P32:17,*green,s25,*white:
                   *P38:18,*color n2,s25,*white,*P40:17,*color n2,s25,*white,*P38:9,*color n2,s25,*white,*P39:15,*green,s25,*white:
                   *P30:14,*color n2,s25,*white,*P31:12,*color n2,s25,*white,*P39:13,*color n2,s25,*white,*P38:11,*green,s25,*white:
                   *P2:22,"Use F5 key":
                   *P2:23,"to exit"
.get next color
         clock     time to time
         unpack    time into str2,str2,str2,str1,str1
         move      c0 to n2
         move      str1 to n2
.
         GOTO      X1
EOJ      STOP
         include   comlogic.inc


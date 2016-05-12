PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
	 INCLUDE   GNXTDD.inc
RELEASE  INIT      "1.2"           16FEB93 DLH   GNXGxx.INC
.RELEASE  INIT      "1.1"
.PI       FILE      FIXED=12
FILL     INIT      "*     "
ZERO     FORM      "0"
         MOVE      "NDAT0019" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "MAKE DATACARD PI FILE BUSY" TO STITLE
         CALL      PAINT
.         OPEN      PI,"NINPI"
.         FILEPI    2;PI
.         WRITAB    PI,ZERO;FILL
	 MOVE      "NINPI" TO GNXTFLD
	 CALL	   GNXTKEY
	 MOVE	   FILL TO GNXTNUM
	 CALL      GNXTUPD
         IFNZ      PC
         DISPLAY   *P12:12,*HON,"DONE !",*HOFF,*B
         XIF
         IFZ       PC
         DISPLAY   *P12:12,*HON,*RED,*BLINKON,"DONE !",*HOFF,*B,*BLINKOFF
         XIF
         STOP
	 INCLUDE   GNXTIO.inc
         INCLUDE   COMLOGIC.inc



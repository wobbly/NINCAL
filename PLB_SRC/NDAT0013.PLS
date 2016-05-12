PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
           INCLUDE   GNXTDD.inc
RELEASE  INIT      "1.2"        16FEB93  DLH GNXTxx.INC
.RELEASE  INIT      "1.1"
.PI       FILE      FIXED=12
FILL     INIT      "            "
ZERO     FORM      "0"
         MOVE      "NDAT0013" TO PROGRAM
         MOVE      "Names in the News " TO COMPNME
         MOVE      "FIX BUSY DATACARD PI FILE" TO STITLE
         CALL      PAINT
.         OPEN      PI,"NINPI"
.         FILEPI    2;PI
.         WRITAB    PI,ZERO;FILL
           MOVE      "NINPI" TO GNXTFLD
           CALL      GNXTKEY
           MOVE      B6 TO GNXTNUM
           REP       ZFILL IN GNXTNUM
           CALL      GNXTUPD
         IFNZ      PC
         DISPLAY   *P12:12,*HON,"DONE !",*B
         XIF
         IFZ       PC
         DISPLAY   *P12:12,*HON,*RED,*BLINKON,"DONE !",*B,*HOFF,*BLINKOFF
         XIF
         STOP
           INCLUDE   GNXTIO.inc
         INCLUDE   COMLOGIC.inc


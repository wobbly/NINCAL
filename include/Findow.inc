+..............................................................................
.
.COMPUTE THE DAY OF THE WEEK
.
.  ENTER WITH:  JDAYWORK = JULIAN DATE
.               YEARWORK = YEAR
.
.  EXITS WITH:  NWORK2   = DAY OF THE WEEK (1=SUNDAY, 7=SATURDAY)
.
. BASE THE DOW ON THE FACT THAT 01/01/80 WAS A TUESDAY
.DLH 10 Jan 00     fix for 2000.
.use cvtjul cut out extra code
.
FINDOW
.dave gone bad
         PACK      str8 FROM mm,dd,yy    .save incomeing vars
         MOVE      "01" TO mm
         MOVE      "01" TO dd
         MOVE      eighty TO yy
         CALL      cvtjul
         MOVE      juldays TO n5
         UNPACK    str8 into mm,dd,yy
         CALL      cvtjul
         MOVE      juldays TO nwork2
         SUB       n5 FROM nwork2
.         MOVE      YEARWORK,NWORK2
.         SUB       EIGHTY,NWORK2
.         ADD       JDAYWORK,NWORK2   OFFSET 1 DAY/YEAR
.end dave
         ADD       TWO,NWORK2        01/01/80 WAS A TUESDAY
*
.ALLOW 1 DAY/YEAR FOR EACH LEAP YEAR
.
.more bad dave
.         MOVE      YEAR,NWORK1
.         SUB       "77",NWORK1
.
FINDOW1
. SUB       FOUR,NWORK1
.         GOTO      FINDOW2 IF LESS
.         ADD       ONE,NWORK2
.         GOTO      FINDOW1
.end more bad dave
*
.DIVIDE BY 7 TILL AND USE THE REMAINDER FOR THE DOW
.
FINDOW2  SUB       SEVEN,NWORK2
         GOTO      FINDOW3 IF ZERO
         GOTO      FINDOW2 IF NOT LESS
.
FINDOW3  ADD       SEVEN,NWORK2
         RETURN

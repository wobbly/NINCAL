*...........................................................
*KEYNOTE - ADD A NEW ORDER NOTE.
* required = KEY (list rental number)
*...........................................................
KEYNOTE  
         TRAPCLR   F3
         TRAPCLR   F4
	 display   *savesw 
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO Mm,STR1,Dd,STR1,Yy
         REP       ZFILL,Dd
         REP       ZFILL,Mm
         PACK      NDATE FROM Mm,Dd,Yy,CC
         MOVE      KEY TO NOTEKEY
         CLOCK     TIME TO str8
         UNPACK    str8 INTO HH,STR1,MN
         PACK      NTIME FROM HH,MN
         MOVE      TYPINIT TO NINITS
         CLEAR     NLINE1
         CLEAR     NLINE2
         CLEAR     NLINE3
         CLEAR     NLINE4
         CLEAR     NLINE5
         CLEAR     NLINE6
.
         DISPLAY   *P1:16,*EF,*HON,"ENTER NOTE FOR LR:",*HOFF,*P1:17:
                   "LR : ",NOTEKEY:
                   *P1:18,NLINE1:
                   *P1:19,NLINE2:
                   *P1:20,NLINE3:
                   *P1:21,NLINE4:
                   *P1:22,NLINE5:
                   *P1:23,NLINE6
NLINE1   MOVE      NLINE1 TO SAVE
         KEYIN     *P1:18,*RV,*+,NLINE1
         SCAN      STAR IN NLINE1
         GOTO      EXNOTES IF EQUAL
         SCAN      LBRAK IN NLINE1
         GOTO      EXNOTES IF EQUAL
         SCAN      RBRAK IN NLINE1
         GOTO      NLINE1X IF EQUAL
         DISPLAY   *P1:18,NLINE1
         GOTO      NLINE2    
NLINE1X  MOVE      SAVE TO NLINE1
         DISPLAY   *P1:18,NLINE1
.    
NLINE2   MOVE      NLINE2 TO SAVE
         KEYIN     *P1:19,*RV,*+,NLINE2
         SCAN      STAR IN NLINE2
         GOTO      EXNOTES IF EQUAL
         GOTO      KEYNOTE1 IF EOS
         SCAN      LBRAK IN NLINE2
         GOTO      NLINE1 IF EQUAL
         SCAN      RBRAK IN NLINE2
         GOTO      NLINE2X IF EQUAL
         DISPLAY   *P1:19,NLINE2
         GOTO      NLINE3    
NLINE2X  MOVE      SAVE TO NLINE2
         DISPLAY   *P1:19,NLINE2
.
NLINE3   MOVE      NLINE3 TO SAVE
         KEYIN     *P1:20,*RV,*+,NLINE3
         SCAN      STAR IN NLINE3
         GOTO      EXNOTES IF EQUAL
         GOTO      KEYNOTE1 IF EOS
         SCAN      LBRAK IN NLINE3
         GOTO      NLINE2 IF EQUAL
         SCAN      RBRAK IN NLINE3
         GOTO      NLINE3X IF EQUAL
         DISPLAY   *P1:20,NLINE3
         GOTO      NLINE4    
NLINE3X  MOVE      SAVE TO NLINE3
         DISPLAY   *P1:20,NLINE3
.
NLINE4   MOVE      NLINE4 TO SAVE
         KEYIN     *P1:21,*RV,*+,NLINE4
         SCAN      STAR IN NLINE4
         GOTO      KEYNOTE1 IF EOS
         GOTO      EXNOTES IF EQUAL
         SCAN      LBRAK IN NLINE4
         GOTO      NLINE3 IF EQUAL
         SCAN      RBRAK IN NLINE4
         GOTO      NLINE4X IF EQUAL
         DISPLAY   *P1:21,NLINE4
         GOTO      NLINE5    
NLINE4X  MOVE      SAVE TO NLINE4
         DISPLAY   *P1:21,NLINE4
.
NLINE5   MOVE      NLINE5 TO SAVE
         KEYIN     *P1:22,*RV,*+,NLINE5
         SCAN      STAR IN NLINE5
         GOTO      EXNOTES IF EQUAL
         GOTO      KEYNOTE1 IF EOS
         SCAN      LBRAK IN NLINE5
         GOTO      NLINE4 IF EQUAL
         SCAN      RBRAK IN NLINE5
         GOTO      NLINE5X IF EQUAL
         DISPLAY   *P1:22,NLINE5
         GOTO      NLINE6    
NLINE5X  MOVE      SAVE TO NLINE5
         DISPLAY   *P1:22,NLINE5
NLINE6   MOVE      NLINE6 TO SAVE
         KEYIN     *P1:23,*RV,*+,NLINE6
         SCAN      STAR IN NLINE6
         GOTO      EXNOTES IF EQUAL
         GOTO      KEYNOTE1 IF EOS
         SCAN      LBRAK IN NLINE6
         GOTO      NLINE5 IF EQUAL
         SCAN      RBRAK IN NLINE6
         GOTO      NLINE6X IF EQUAL
         DISPLAY   *P1:23,NLINE6
         GOTO      KEYNOTE1    
NLINE6X  MOVE      SAVE TO NLINE6
         DISPLAY   *P1:23,NLINE6
.
KEYNOTE1 KEYIN     *P1:24,*EL,*P20:24,"ALL OK ? ",*-,STR1;
         CMATCH    STAR IN STR1
         GOTO      EXNOTES IF EQUAL
         CMATCH    NO TO STR1
         GOTO      NLINE1 IF EQUAL
         MOVE      key TO NONOFLD
         CALL      NONOWRT
EXNOTES
.	 display   *scrnrst 1
         display   *restsw
         TRAP      keynote if F4
         TRAP      dissnote if F3
         return
*...........................................................
*DISSNOTE - DISPLAY ANY ORDER NOTES.
* REQUIRED: KEY   (LR NUMBER FROM ORDER OR INVOICE PROG.)
*...........................................................
DISSNOTE 
.         TRAPCLR   F2
         TRAPCLR   F3
         TRAPCLR   F4
.	 display   *scrnsave 1
         display   *savesw
         TRAP      DISSNOTE IF F3
         MOVE      KEY TO NONOFLD
         CALL      NONOKEY
	 MATCH     KEY TO NOTEKEY         *CORRECT RECORD?
	 GOTO      NONOTE IF NOT EQUAL    * NO
DISNOTE1 UNPACK    NDATE INTO Mm,dd,yy,CC
         UNPACK    NTIME INTO HH,MN
         DISPLAY   *P1:17,*EF:
                   "LR : ",NOTEKEY,*P14:17,"DATED : ",Mm,"/",Dd,"/",CC,Yy:
                   "  ",HH,":",MN," By ",NINITS:
                   *P1:18,NLINE1:
                   *P1:19,NLINE2:
                   *P1:20,NLINE3:
                   *P1:21,NLINE4:
                   *P1:22,NLINE5:
                   *P1:23,NLINE6
         KEYIN     *P20:24,"Enter to continue, (P)rior Note",STR1,*P20:24,*EL;
         CMATCH    STAR IN STR1
         GOTO      NONOTE1 IF EQUAL
         CMATCH    "P" IN STR1
         GOTO      PRENOTE IF EQUAL
         CALL      NONOKS
	 MATCH     KEY TO NOTEKEY
	 GOTO      DISNOTE1 IF EQUAL
         GOTO      NONOTE
.
PRENOTE  CALL      NONOKP
	 MATCH     KEY TO NOTEKEY
	 GOTO      DISNOTE1 IF EQUAL
.........
NONOTE   DISPLAY   *P20:23,*EL,"NO (MORE) NOTES FOUND :",*B,*W2:
                   *P20:23,*EL;
NONOTE1  
.	 display   *scrnrst 1
         display   *restsw
         TRAP      keynote if F4
         TRAP      dissnote if F3
	 return
.............................................................


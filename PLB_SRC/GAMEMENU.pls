. RESTORE DATASHARE COMMON, BY CHAINING ANSWER/DBC AT END.
         INCLUDE   COMMON.inc
. 
.ARCCLOCK FILE      HOLDS SYSTEM DATE
SEQ      FORM      "-1"
YR       FORM      2
DAY      FORM      2
MO       FORM      2
WKDAY    FORM      1
HR       FORM      2
MIN      FORM      2
SEC      FORM      2
JUNK     DIM       1
FIVE     FORM      "17"
PASS     INIT      "COSMO"
ANS      DIM       5
COLON    INIT      ":"
TIME     DIM       8
HH       DIM       2
. 
S1       FORM      2             COMMON DATA DEFINITIONS MAY BE WRONG!!
.         OPEN      ARCCLOCK,"ARCCLOCK"
.         READ      ARCCLOCK,SEQ;JUNK,YR,JUNK,MO,JUNK,DAY,JUNK,JUNK,WKDAY:
.                   JUNK,HR,JUNK,MIN,JUNK,SEC
.         CLOSE     ARCCLOCK
.         PACK      TIME FROM HR,COLON,MIN,COLON,SEC
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO HH
         MOVE      HH TO HR
         SUBTRACT  HR,FIVE
         GOTO      START IF ZERO
         GOTO      START IF LESS
         SUBTRACT  FIVE,HR
         GOTO      START IF ZERO
         GOTO      BAD IF LESS
         GOTO      START IF OVER
         GOTO      BAD
+
BAD      DISPLAY   *ES,*P1:3,"I'm so sorry but it is time for work not ":
                   "PLAY!!!!!! ":
                   *P1:23,"TIME : ",TIME
         BEEP
         KEYIN     *P2:5,*EOFF:
                   "Do you have a password so you can play when you should be":
                   " working?",*T30,*JR,ANS
         MATCH     PASS,ANS
         GOTO      START IF EQUAL
         BEEP
         stop
START    DISPLAY   *ES,"GAMES",*P1:3,"( 1) Hunt the wumpus (if you can)":
                   *P1:4,"( 2) Find the enemy submarine",*P1:5:
                   "( 3) The old card game favorite",*P1:6,"( 4) St":         
                   "ar Trek at its best",*P1:7,"( 5) Battlestar":
                   " ",*P1:8,*HON,"( 6) DESTROY",*HOFF,*P1:9,"( 7) Bior":
                   "ythm, (comp room only)",*P1:10,"( 8) Blackjack ":
                   *P1:11,"(9) Theory":
                   *P1:12,"(10) Monopoly":
                   *P1:13,"(11) Torpedos":
                   *P1:14,"(12) Flush":
                   *P1:15,"(13) Fly":
                   *P1:16,"(14) Xmastree":
                   *P1:17,"(15) Talktexan"
X728     KEYIN     *P1:21,*EL,"Selection by number",*P41:21,"Enter ":
                   "(99) to leave this menu.",*P25:21,"__",*P25:21,S1
         COMPARE   "1",S1                                                     
         GOTO      X728 IF LESS                                               
..............................................................................
         COMPARE   "99",S1                                                    
         GOTO      STOP IF ZERO
..............................................................................
         COMPARE   "16",S1
         GOTO      X728 IF NOT LESS                                           
..............................................................................
         TRAP      X874 IF CFAIL                                              
         BRANCH    S1 OF ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT,NINE:
                   TEN,ELEVEN,TWELVE,THRTEEN,FRTEEN,FIFTEEN,SIXTEEN,SEVNTEEN:
                   EIGHTEEN,NINETEEN,TWENTY
..............................................................................
         GOTO      X728                                                       
******************************************************************************
X874     DISPLAY   *P1:23,*EL,"SORRY PROGRAM NOT ON LINE",*B,*W,*P1:23,*EL
         RETURN
******************************************************************************
ONE     CHAIN     "WUMPUS"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
TWO     CHAIN     "SUBHUNT"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
THREE     CHAIN     "MEELBORN"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
FOUR     CHAIN     "STARTREK"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
FIVE     CHAIN     "BATTLESTAR"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
SIX     CHAIN     "DESTROY"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
SEVEN     CHAIN     "BIOCAL"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
EIGHT    CHAIN     "BJACK"
******************************************************************************
         GOTO      X728                                                       
******************************************************************************
NINE    CHAIN     "THEORY"
******************************************************************************
         GOTO      X728
******************************************************************************
TEN    CHAIN     "MONOPOLY"
*******************************************************************************
         GOTO      X728
******************************************************************************
ELEVEN    CHAIN     "TORPEDOS"
*******************************************************************************
         GOTO      X728
*******************************************************************************
TWELVE    CHAIN     "TOILET"
*******************************************************************************
         GOTO      X728
*******************************************************************************
THRTEEN    CHAIN     "FLY"
*******************************************************************************
         GOTO      X728
*******************************************************************************
FRTEEN   CHAIN     "XMASTREE"
         GOTO      X728
*******************************************************************************
FIFTEEN  CHAIN     "TALKTEXAN"
         GOTO      X728
*******************************************************************************
SIXTEEN  CHAIN     "??"
         GOTO      X728
*******************************************************************************
SEVNTEEN CHAIN     "FLY"
         GOTO      X728
*******************************************************************************
EIGHTEEN CHAIN      "XMASTREE"
         GOTO      X728
*******************************************************************************
NINETEEN CHAIN     "TALKTEXAN"
         GOTO      X728
*******************************************************************************
TWENTY   CHAIN     "ANSWER"
         GOTO      X728
*******************************************************************************
.STOP     CHAIN     "ANSWER"
STOP     STOP
******************************************************************************

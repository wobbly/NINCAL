+
. PROGRAM: MEELBORN
. 
. PURPOSE: GAME	OF MILLE-BORNES, A FRENCH TOURING CARD GAME.
.
DISC	 FILE
VERSION	 INIT	   "13.3"
RELEASE	 INIT	   "10 Oct 84"
MEELBORN INIT	   "MEELSTRT"
CAR	 INIT	   "^"
.
MID	 EQU	   39
.
ALINE	 FORM	   "13"
ACOL	 FORM	   "01"
BLINE	 FORM	   "13"
BCOL	 FORM	   "41"
CFSWT	 FORM	   3
DSCNT	 FORM	   2
CARDS	 INIT	   "ROQJQRSIQISPSMDREPSSJISFRCDGGFBFEHJCHARRBIFJPJQSJQJ":
		   "FSJHFSLJNGJGHROPEPHQPHPRJOPGPQOPSIPJQPQDRGAJQKJEIR"
DECK	 DIM	   101
LETTERA	 INIT	   "A"
LETTERB	 INIT	   "B"
LETTERC	 INIT	   "C"
LETTERD	 INIT	   "D"
LETTERE	 INIT	   "E"
LETTERF	 INIT	   "F"
LETTERG	 INIT	   "G"
LETTERH	 INIT	   "H"
LETTERI	 INIT	   "I"
LETTERJ	 INIT	   "J"
LETTERK	 INIT	   "K"
LETTERL	 INIT	   "L"
LETTERM	 INIT	   "M"
LETTERN	 INIT	   "N"
LETTERO	 INIT	   "O"
LETTERP	 INIT	   "P"
LETTERQ	 INIT	   "Q"
LETTERR	 INIT	   "R"
LETTERS	 INIT	   "S"
LETTERY	 INIT	   "Y"
DRAW	 FORM	   3
*
. CARD VALUES
.................
. HAZARDS
.
A	 INIT	   "OUT	OF GAS"
CNTA	 FORM	   "2"
B	 INIT	   "FLAT TIRE"
CNTB	 FORM	   "2"
C	 INIT	   "ACCIDENT"
CNTC	 FORM	   "2"
D	 INIT	   "SPEED LIMIT	50"
CNTD	 FORM	   "3"
E	 INIT	   "STOP!"
CNTE	 FORM	   "4"
*
. REMEDIES
.
F	 INIT	   "GASOLINE"
CNTF	 FORM	   "6"
G	 INIT	   "SPARE TIRE"
CNTG	 FORM	   "6"
H	 INIT	   "REPAIRS"
CNTH	 FORM	   "6"
I	 INIT	   "END	OF LIMIT"
CNTI	 FORM	   "6"
J	 INIT	   "ROLL !..."
CNTJ	 FORM	   "14"
*
. SAFETIES
.
K	 INIT	   "EXTRA TANK"
CNTK	 FORM	   "1"
L	 INIT	   "PUNCTURE-PROOF"
CNTL	 FORM	   "1"
M	 INIT	   "DRIVING ACE"
CNTM	 FORM	   "1"
N	 INIT	   "RIGHT OF WAY"
CNTN	 FORM	   "1"
*
. DISTANCE CARDS
.
O	 INIT	   "200	MILESTONES"
CNTO	 FORM	   "4"
P	 INIT	   "100	MILESTONES"
CNTP	 FORM	   "12"
Q	 INIT	   " 75	MILESTONES"
CNTQ	 FORM	   "10"
R	 INIT	   " 50	MILESTONES"
CNTR	 FORM	   "10"
S	 INIT	   " 25	MILESTONES"
CNTS	 FORM	   "10"
.
KEEPCNT	 FORM	   "1"
.
OPER	 FORM	   1
FAST	 DIM	   1
PORT	 FORM	   1
NAME	 DIM	   10(2)
.
ACARDS	 DIM	   7
SPA	 INIT	   "					    "
BCARDS	 DIM	   7
SCORE1	 FORM	   6
SCORE2	 FORM	   6
SCORE3	 FORM	   6
SCORE4	 FORM	   6
SCORET1	 FORM	   6
SCORET2	 FORM	   6
XCARDS	 DIM	   7
PLAYER	 FORM	   1
RETRY	 INIT	   "N"
PLAY2	 DIM	   1
.
CONDA	 INIT	   "E"
CONDB	 INIT	   "E"
CONDC	 INIT	   "E"
CONDD	 INIT	   "E"
LIMITA	 FORM	   "200"
LIMITB	 FORM	   "200"
LIMITC	 FORM	   "200"
LIMITD	 FORM	   "200"
OLIMA	 FORM	   "200"
OLIMB	 FORM	   "200"
OLIMC	 FORM	   "200"
OLIMD	 FORM	   "200"
SLA	 INIT	   "I"
SLB	 INIT	   "I"
SLC	 INIT	   "I"
SLD	 INIT	   "I"
SAFE1	 INIT	    "	 "
SAFE2	 INIT	    "	 "
SAFE3	 INIT	    "	 "
SAFE4	 INIT	    "	 "
A200	 FORM	   1
B200	 FORM	   1
X200	 FORM	   1
DISTA	 FORM	   4
DISTB	 FORM	   4
DISTC	 FORM	   4
DISTD	 FORM	   4
HAZARD	 INIT	   "ABCE"
REMEDY	 INIT	   "FGHJ"
SAFETY	 INIT	   "KLMN"
MILES	 FORM	   3
HZ	 FORM	   2
HI	 FORM	   2
EXT	 INIT	   "NO"
GOAL	 FORM	   " 700"
MESSAGE	 DIM	   32
*
. WORKING STORAGE
.
DIM1	 DIM	   1
DIM2	 DIM	   2
DIM2A	 DIM	   2
DIM3	 DIM	   3
DIM4	 DIM	   4
DIM10	 DIM	   10
DIM15	 DIM	   15
TIME1	 DIM	   8
FORM1	 FORM	   1
FORM2	 FORM	   2
FORM3	 FORM	   3
FORM4	 FORM	   4
FORM10	 FORM	   10
QUES	 DIM	   3
BRNCH	 FORM	   2
CNA	 FORM	   "7"
CNB	 FORM	   "7"
CN	 FORM	   "7"
CARDIN	 DIM	   15
NUM2A	 FORM	   2
REVR	 FORM	   "0"
*
. CONSTANTS
.
$0$	 FORM	   "0"
$1$	 FORM	   "1"
$2$	 FORM	   "2"
$3$	 FORM	   "3"
$4$	 FORM	   "4"
$6$	 FORM	   "6"
$7$	 FORM	   "7"
$8$	 FORM	   "8"
$9$	 FORM	   "9"
$10$	 FORM	   "10"
$11$	 FORM	   "11"
$12$	 FORM	   "12"
$13$	 FORM	   "13"
$16$	 FORM	   "16"
$19$	 FORM	   "19"
$25$	 FORM	   "25"
$27$	 FORM	   "27"
$40$	 FORM	   "40"
$42$	 FORM	   "42"
$50$	 FORM	   "50"
$100$	 FORM	   "100"
$101$	 FORM	   "101"
$102$	 FORM	   "102"
$200$	 FORM	   "200"
$300$	 FORM	   "300"
$1000$	 FORM	   "1000"
$5000$	 FORM	   "5000"
$SPACE$	 INIT	   " "
$4SPACE$ INIT	   "	"
$7SPACE$ INIT	   "	   "
$AST$	 INIT	   "*"
$TM$	 INIT	   "TM"
*
. RANDOM NUMBER	GENERATOR REQUIRES THESE VARIABLES
.
RND	 FORM	   "1234567890"
RX	 FORM	   0.3
RND100	 INIT	   "07849375984839875984893758947893785894798378578584":
		   "39401203984903871734930201848598604932727664584940"
RHH	 FORM	   2
RMM	 FORM	   2
+
. PROGRAM ENTRY	POINT.
.
MLBRNCPM
	 CLOCK	   TIME,TIME1
	 UNPACK	   TIME1,DIM2,DIM1,DIM2A
	 MOVE	   DIM2,RHH
	 MOVE	   DIM2A,RMM
*
. CHECK	FOR PROPER RELEASE.
.
TRAP
	 TRAP	   PREP	IF IO
	 OPEN	   DISC,"MEELDATA"
*
. ATTEMPT TO READ LAST SHUFFLED	DECK
.
	 READ	   DISC,$1$;CARDS
	 MATCH	   SPA,CARDS
	 CALL	   PREP	IF EQUAL
	 MOVE	   CARDS,DECK
	 CALL	   INIT
*
. DISPLAY MENU
.
KEY
	 DISPLAY   *ES,*P20:2,"MILLE BORNES - THE 1000 MILESTONE GAME.":
		   *P5:4,"1. AUTOMATIC PLAYING OF BOTH HANDS (DEMO).":
		   *P5:5,"2. COMPUTER IS YOUR OPPONENT":
		   *P5:6,"3. EXIT":
		   *P15:9,"SELECTION : "
*
. FIND OUT WHAT	WILL BE	DONE
.
WHATGAME
	 KEYIN	   *P27:9,*+,*DE,OPER
	 BRANCH	   OPER	OF WHATGAM2,WHATGAM2,EXIT
	 GOTO	   WHATGAME
*
. USER ENTERED '1' or '2'. LET HIM PLAY.
.
WHATGAM2
	 CALL	   SEED
	 ADD	   FORM10,RND
*
. GET PERSONS NAME IF AUTOMATIC	PLAY OF	BOTH HANDS NOT SPECIFIED.
.
	 COMPARE   $1$,OPER
	 GOTO	   WHATNAME IF NOT EQUAL
	 MOVE	   "A. J. FOYT",NAME(1)
	 MOVE	   "M ANDRETTI",NAME(2)
WHATSPED
	 KEYIN	   *P5:10,"FAST	OR SLOW	GAME (F/S) ? ",*+,*UC,FAST;
	 CMATCH	   "F",FAST
	 GOTO	   AUTO	IF EQUAL
	 CMATCH	   "S",FAST
	 GOTO	   AUTO	IF EQUAL
	 GOTO	   WHATSPED
WHATNAME
	 KEYIN	   *P5:10,"WHAT	IS YOUR	NAME ? ",NAME(1)
	 CALL	   SEED
	 ADD	   FORM10,RND
	 CMATCH	   " ",NAME(1)
	 GOTO	   WHATNAME IF EOS
	 MOVE	   "COMPUTER",NAME(2)
	 MOVE	   $1$,PORT
	 GOTO	   AUTO
*
. SEMI RANDOM NUMBER GENERATOR.
.
SEED
	 COMPARE   $0$,RMM
	 GOTO	   SEED1 IF NOT	EQUAL
	 MOVE	   "17",RMM
SEED1
	 RESET	   RND100,RMM
	 MOVE	   RND100,DIM2
	 MOVE	   DIM2,FORM10
	 ADD	   $25$,FORM10
	 MOVE	   FORM10,RMM
	 RETURN
*
AUTO
	 CALL	   SEED
	 MOVE	   "MEELBORN",MEELBORN
	 MULT	   FORM10,RND
	 CALL	   SEED
	 ADD	   FORM10,RND
*
. DISPLAY BOARD	AT THIS	TIME.
.
	 DISPLAY   *ES,*P60:21,"MILLE	BORNES":
		   *P01:02,"---PLAYER ## 1",*RPTCHAR "-":27:
		   *P41:02,"---PLAYER ## 2",*RPTCHAR "-":27:
		   *P01:03,"CONDITION: ",E,*PMID:03,"| CONDITION: ",E:
		   *P01:04,"MILESTONES:	   0",*PMID:04,"| MILESTONES:	 0":
		   *PMID:05,"|":
		   *P01:06,"/-SAFETY  AREA-\",*PMID:06,"| /-SAFETY  AREA-\":
		   *PMID:07,"|":
		   *PMID:08,"|":
		   *PMID:09,"|":
		   *PMID:10,"|":
	      *P01:17,"/---HAZARDS-------REMEDIES-------SAFETIES-------\":
		   *N,"| 2 OUT OF GAS  6 GASOLINE     1	EXTRA TANK     |":
		   *N,"| 2 FLAT	TIRE   6 SPARE TIRE   1	PUNCTURE-PROOF |":
		   *N,"| 2 ACCIDENT    6 REPAIRS      1	DRIVING	ACE    |":
		   *N,"| 3 SPEED LIMIT 6 END OF	LIMIT 1	RIGHT OF WAY   |":
		   *N,"| 4 STOP	      14 ROLL		  #"   #"   #"	  |":
		  *n,"+",*RPTCHAR "-":16,"DISTANCE CARDS",*RPTCHAR "-":17,"+":
		   *N,"\----200(4)  100(12)  75(10)  50(10)  25(10)----/":
		   *P60:23,"VERSION : ",VERSION:
		   *P60:24,"RELEASE : ",RELEASE;
	 MOVE	   $19$,HZ
	 CALL	   DIS2
	 ADD	   $40$,HZ
	 COMPARE   $1$,OPER
	 CALL	   DIS2	IF EQUAL
OPL3
	 DISPLAY   *P1:12,*EL,"NOW SHUFFLING THE DECK. PLEASE HAVE":
		   " PATIENCE...",*P1:11,*EL;
	 PACK	   DECK,SPA,SPA,SPA
	 GOTO	   NEWCRD
*
. SUBROUTINE TO	DISPLAY	CARD LINE NUMBERS ON THE SCREEN.
.
DIS2
	 DISPLAY   *PHZ:03,"	       CARDS":
		   *PHZ:04,"1.":
		   *PHZ:05,"2.":
		   *PHZ:06,"3.":
		   *PHZ:07,"4.":
		   *PHZ:08,"5.":
		   *PHZ:09,"6.":
		   *PHZ:10,"7."
	 RETURN
*
. RANDOM NUMBER	GENERATOR RETURNS .NNN IF RX FORM 0.3
. REQUIRES INITIAL NON-ZERO SEED RND
.
RANDOM	 MULT	   "357353",RND
	 ADD	   $1$,RND
	 MOVE	   RND,DIM15
	 RESET	   DIM15,6
	 MOVE	   DIM15,DIM4
	 CMOVE	   ".",DIM4
	 MOVE	   DIM4,RX
	 RETURN
*
. GET ANOTHER CARD FROM	THE PILE.
.
NEWCRD
	 ADD	   $1$,DRAW
CRND
	 CALL	   RANDOM
	 MOVE	   $102$,FORM10
	 MULT	   RX,FORM10
	 COMPARE   $1$,FORM10
	 GOTO	   CRND	IF LESS
	 COMPARE   $102$,FORM10
	 GOTO	   CRND	IF NOT LESS
	 RESET	   DECK,FORM10
ISITX
	 CMATCH	   " ",DECK
	 GOTO	   EMPT	IF EQUAL
	 BUMP	   DECK
	 GOTO	   ISITX IF NOT	EOS
	 RESET	   DECK
	 GOTO	   ISITX
EMPT
	 CMOVE	   CARDS,DECK
	 BUMP	   CARDS,2
	 GOTO	   CDSW	IF NOT EOS
	 RESET	   CARDS,2
CDSW
	 DISPLAY   $AST$;
	 COMPARE   "80",DRAW
	 GOTO	   NEWCRD IF LESS
	 RESET	   DECK
FILL
	 CMATCH	   " ",DECK
	 GOTO	   BUMP	IF NOT EQUAL
	 ADD	   $1$,DRAW
	 DISPLAY   $AST$;
	 CMOVE	   CARDS,DECK
	 BUMP	   CARDS,2
BUMP
	 BUMP	   DECK
	 GOTO	   FILL	IF NOT EOS
	 RESET	   CARDS
	 RESET	   DECK
	 DISPLAY   *P1:11,"SHUFFLED",*EL,*P1:12,*EL
*
. CUT THE DECK
.
	 CALL	   SEED
	 CLEAR	   CARDS
	 RESET	   DECK,FORM10
	 APPEND	   DECK,CARDS
	 RESET	   DECK
	 APPEND	   DECK,CARDS
	 RESET	   CARDS
	 MOVE	   CARDS,DECK
	 CLEAR	   ACARDS
	 CLEAR	   BCARDS
	 MOVE	   $3$,FORM3
	 MOVE	   $0$,DRAW
	 MOVE	   $0$,FORM2
	 MOVE	   $SPACE$,DIM1
	 DISPLAY   *P1:12,"CUT.",*EL
	 CALL	   INIT
*
. DEAL THE CARDS - STORE HANDS
.
DEAL	 ADD	   $1$,FORM2
	 COMPARE   $13$,FORM2
	 GOTO	   DEALT IF EQUAL
	 ADD	   $1$,DRAW
	 RESET	   DECK,DRAW
	 CMOVE	   DECK,DIM1
	 COMPARE   $7$,FORM2
	 GOTO	   THEIRS IF NOT LESS
	 APPEND	   DIM1,ACARDS
	 GOTO	   DEAL
THEIRS	 APPEND	   DIM1,BCARDS
	 GOTO	   DEAL
DEALT	 DISPLAY   *P1:11,*EL,*N,*EL
	 EXTEND	   ACARDS
	 EXTEND	   BCARDS
	 RESET	   ACARDS
	 RESET	   BCARDS
	 MOVE	   $7$,CNA
	 MOVE	   $7$,CNB
	 MOVE	   $7$,CN
SHOW
	 MOVE	   $3$,FORM3
CDR
	 MOVE	   ACARDS,DIM1
	 CALL	   DETER
	 ADD	   $1$,FORM3
	 DISPLAY   *P22:FORM3,DIM15
	 BUMP	   ACARDS
	 COMPARE   $9$,FORM3
	 GOTO	   CDR IF LESS
	 COMPARE   $1$,OPER
	 GOTO	   PLAY	IF NOT EQUAL
PR2IS
	 RESET	   BCARDS
	 MOVE	   $3$,FORM3
PR2X
	 MOVE	   BCARDS,DIM1
	 CALL	   DETER
	 ADD	   $1$,FORM3
	 DISPLAY   *P62:FORM3,DIM15
	 BUMP	   BCARDS
	 COMPARE   $9$,FORM3
	 GOTO	   PR2X	IF LESS
	 DISPLAY   *P1:11,*EL
	 GOTO	   PLAY
*
. SET UP DISK FILE TO SAVE THE CARD DECK FROM ONE GAME TO THE NEXT.
.
PREP 
	 DISPLAY   *ES,*P10:10,"INITIALIZING WORK FILE."
	 MOVE	   CARDS,DECK
	 PREP	   DISC,"MEELDATA"
	 WRITE	   DISC,$0$;RELEASE
	 WEOF	   DISC,$2$
INIT
	 WRITE	   DISC,$1$;DECK
	 RETURN
*
. READY	TO START GAME -	YOU GO FIRST.
.
XPLAY2
	 COMPARE   $1$,PLAYER
	 GOTO	   PLAY2X IF EQUAL
*
. RESTORE PLAYER 2 B
.
. MOVE 4-1  3-2
.
	 MOVE	   SCORE4,SCORE1
	 MOVE	   SCORE3,SCORE2
	 MOVE	   CONDD,CONDA
	 MOVE	   CONDC,CONDB
	 MOVE	   LIMITD,LIMITA
	 MOVE	   LIMITC,LIMITB
	 MOVE	   OLIMD,OLIMA
	 MOVE	   OLIMC,OLIMB
	 MOVE	   SLD,SLA
	 MOVE	   SLC,SLB
	 RESET	   SAFE4
	 MOVE	   SAFE4,SAFE1
	 RESET	   SAFE3
	 MOVE	   SAFE3,SAFE2
	 MOVE	   DISTD,DISTA
	 MOVE	   DISTC,DISTB
	 RESET	   XCARDS
	 MOVE	   XCARDS,BCARDS
	 MOVE	   X200,B200
	 MOVE	   CN,CNB
*
. MOVE 1-3   2-4
.
PLAY
	 RESET	   ACARDS
	 MOVE	   ACARDS,XCARDS
	 RESET	   SAFE1
	 RESET	   SAFE2
	 MOVE	   SAFE1,SAFE3
	 MOVE	   SAFE2,SAFE4
	 MOVE	   SCORE1,SCORE3
	 MOVE	   SCORE2,SCORE4
	 MOVE	   CONDA,CONDC
	 MOVE	   CONDB,CONDD
	 MOVE	   LIMITA,LIMITC
	 MOVE	   LIMITB,LIMITD
	 MOVE	   OLIMA,OLIMC
	 MOVE	   OLIMB,OLIMD
	 MOVE	   SLA,SLC
	 MOVE	   SLB,SLD
	 MOVE	   A200,X200
	 MOVE	   CNA,CN
	 MOVE	   DISTA,DISTC
	 MOVE	   DISTB,DISTD
	 MOVE	   $1$,PLAYER
	 MOVE	   $2$,PLAY2
	 MOVE	   $1$,HI
	 GOTO	   XDRAW
PLAY2X
	 MOVE	   $2$,PLAYER
	 MOVE	   $1$,PLAY2
*
. RESTORE PLAYER 1 A
.
. MOVE 3-1  4-2
.
	 RESET	   XCARDS
	 MOVE	   XCARDS,ACARDS
	 MOVE	   X200,A200
	 MOVE	   CN,CNA
	 MOVE	   SCORE3,SCORE1
	 MOVE	   SCORE4,SCORE2
	 MOVE	   CONDC,CONDA
	 MOVE	   CONDD,CONDB
	 MOVE	   LIMITC,LIMITA
	 MOVE	   LIMITD,LIMITB
	 MOVE	   OLIMC,OLIMA
	 MOVE	   OLIMD,OLIMB
	 MOVE	   SLC,SLA
	 MOVE	   SLD,SLB
	 RESET	   SAFE3
	 MOVE	   SAFE3,SAFE1
	 RESET	   SAFE4
	 MOVE	   SAFE4,SAFE2
	 MOVE	   DISTC,DISTA
	 MOVE	   DISTD,DISTB
*
. SET UP PLAYER	2 B
.
. MOVE 2-3   1-4
.
	 MOVE	   "41",HI
	 RESET	   BCARDS
	 MOVE	   BCARDS,XCARDS
	 MOVE	   B200,X200
	 MOVE	   CNB,CN
	 MOVE	   SCORE2,SCORE3
	 MOVE	   SCORE1,SCORE4
	 MOVE	   CONDB,CONDC
	 MOVE	   CONDA,CONDD
	 MOVE	   LIMITA,LIMITD
	 MOVE	   LIMITB,LIMITC
	 MOVE	   OLIMA,OLIMD
	 MOVE	   OLIMB,OLIMC
	 MOVE	   SLA,SLD
	 MOVE	   SLB,SLC
	 RESET	   SAFE1
	 MOVE	   SAFE1,SAFE4
	 RESET	   SAFE2
	 MOVE	   SAFE2,SAFE3
	 MOVE	   DISTA,DISTD
	 MOVE	   DISTB,DISTC
*
. DRAW A CARD FROM THE DECK
.
XDRAW
	 ADD	   $1$,CFSWT
	 BRANCH	   REVR	OF ENDGAME1,ENDGAME2,END
	 RESET	   XCARDS,CN
	 COMPARE   $101$,DRAW
	 GOTO	   DRW IF LESS
	 MOVE	   $0$,FORM1
	 RESET	   XCARDS
CNT
	 CMATCH	   " ",XCARDS
	 GOTO	   BLAN	IF EQUAL
	 ADD	   $1$,FORM1
BLAN
	 BUMP	   XCARDS
	 GOTO	   CNT IF NOT EOS
	 MOVE	   $27$,HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:3,FORM1," CARDS"
	 GOTO	   PLAYX
DRW
	 ADD	   $1$,DRAW
	 MOVE	   DRAW,FORM3
	 MOVE	   $11$,HZ
	 COMPARE   "51",FORM3
	 GOTO	   ALRIT IF LESS
	 SUB	   $50$,FORM3
	 ADD	   $1$,HZ
ALRIT
	 COMPARE   $50$,DRAW
	 GOTO	   HALVES IF NOT EQUAL
	 DISPLAY   *P54:11,"HALF DECK DEALT."
HALVES
	 COMPARE   $101$,DRAW
	 GOTO	   ALRI	IF NOT EQUAL
	 DISPLAY   *P54:11,*EL,*P54:12,"ALL 101	CARDS DEALT."
ALRI
	 DISPLAY   *PFORM3:HZ,"X",*P52:HZ,"<"
	 RESET	   DECK,DRAW
	 MOVE	   DECK,DIM1
	 CMOVE	   DIM1,XCARDS
	 COMPARE   $1$,OPER
	 GOTO	   FTH IF EQUAL
	 COMPARE   PORT,PLAYER
	 GOTO	   PLAYX IF NOT	EQUAL
*
. SHOW CARD DRAWN
.
FTH
	 CALL	   DETER
	 MOVE	   CN,FORM3
	 ADD	   $3$,FORM3
	 MOVE	   "21",HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:FORM3,DIM15
PLAYX
	 RESET	   XCARDS
	 RESET	   BCARDS
	 RESET	   ACARDS
	 MOVE	   LETTERN,RETRY
	 CLOCK	   TIME,TIME1
	 MATCH	   $7SPACE$,BCARDS
	 GOTO	   JOHN	IF NOT EQUAL
	 MATCH	   $7SPACE$,ACARDS
	 GOTO	   ENDGAME IF EQUAL
JOHN
	 MATCH	   $7SPACE$,XCARDS
	 GOTO	   XPLAY2 IF EQUAL
	 COMPARE   $1$,OPER
	 GOTO	   OLMN	IF NOT EQUAL
	 MOVE	   PLAYER,PORT
OLMN
	 DISPLAY   *PHI:1,SPA,*PHI:1,NAME(PLAYER),*P1:11,TIME1
	 RESET	   SAFE3,4
	 CMATCH	   "N",SAFE3
	 GOTO	   LIMX	IF NOT EQUAL
	 CMATCH	   "J",CONDC
	 GOTO	   LIMX	IF EQUAL
	 CMATCH	   "E",CONDC
	 GOTO	   LIMX	IF LESS
	 CMATCH	   "I",CONDC
	 GOTO	   LIMX	IF NOT LESS
	 MOVE	   LETTERJ,CONDC
	 MOVE	   $11$,HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:3,J,$7SPACE$
*
. PLAY A CARD
.
LIMX
	 MOVE	   $10$,HZ
	 ADD	   HI,HZ
	 BRANCH	   OPER	OF APLAY,GETIN
GETIN
	 COMPARE   $2$,PLAYER
	 GOTO	   APLAY IF EQUAL
KE3
	 KEYIN	   *PHZ:1," - CARD NUMBER: ",*+,FORM1;
	 COMPARE   $1$,FORM1
	 GOTO	   KE3 IF LESS
	 GOTO	   GOTCRD
*
. PROCESS CARD INTENDED	TO BE PLAYED
.
GOTCRD
	 COMPARE   $8$,FORM1
	 GOTO	   LIMX	IF NOT LESS
	 MOVE	   FORM1,CN
	 RESET	   XCARDS,CN
	 CMATCH	   " ",XCARDS
	 GOTO	   PLAYX IF EQUAL
	 MOVE	   XCARDS,DIM1
	 CALL	   DETER
	 MOVE	   DIM15,CARDIN
*
. GO TO	SPECIFIC CARD ROUTINES
.
	 BRANCH	   NUM2A OF ACOM,BCOM,CCOM,DCOM,ECOM,FCOM,GCOM:
			    HCOM,ICOM,JCOM,KCOM,KCOM,KCOM,KCOM:
			    OCOM,OCOM,OCOM,OCOM,OCOM
ACOM
	 RESET	   SAFE4
	 CMATCH	   SAFE4,"K"
	 GOTO	   INV IF EQUAL
	 GOTO	   TAG
BCOM
	 RESET	   SAFE4,2
	 CMATCH	   "L",SAFE4
	 GOTO	   INV IF EQUAL
	 GOTO	   TAG
CCOM
	 RESET	   SAFE4,3
	 CMATCH	   "M",SAFE4
	 GOTO	   INV IF EQUAL
	 GOTO	   TAG
ECOM
	 RESET	   SAFE4,4
	 CMATCH	   "N",SAFE4
	 GOTO	   INV IF EQUAL
*
. CHANGE OPPONENT'S CONDITION
.
TAG
	 CMATCH	   "J",CONDD
	 GOTO	   INV IF NOT EQUAL
	 MOVE	   DIM1,CONDD
	 MOVE	   $0$,CFSWT
	 MOVE	   "53",HZ
	 SUB	   HI,HZ
	 DISPLAY   *PHZ:3,CARDIN
*
. REMOVE CARD FROM YOUR	HAND
.
DISCRD
	 RESET	   XCARDS,CN
	 CMOVE	   " ",XCARDS
	 COMPARE   $1$,KEEPCNT
	 CALL	   UPDCARD IF EQUAL
	 COMPARE   $101$,DRAW
	 GOTO	   DISCRD1 IF LESS
	 ADD	   $1$,DSCNT
DISCRD1
	 COMPARE   PORT,PLAYER
	 GOTO	   DISCRD2 IF NOT EQUAL
	 MOVE	   CN,FORM3
	 ADD	   $3$,FORM3
	 MOVE	   "20",HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:FORM3,$7SPACE$,$4SPACE$,$4SPACE$
DISCRD2
	 CMATCH	   "Y",RETRY
	 GOTO	   XPLAY2 IF NOT EQUAL
	 GOTO	   XDRAW
*
. CARD CHOSEN CAN'T BE PLAYED
.
INV
	 DISPLAY   *PHI:1,SPA,*PHI:1,CARDIN," DISCARD ";
	 MOVE	   HI,HZ
	 ADD	   "24",HZ
	 COMPARE   $1$,OPER
	 GOTO	   DISCRD IF EQUAL
	 COMPARE   PORT,PLAYER
	 GOTO	   DSCRD IF EQUAL
	 COMPARE   $2$,PLAYER
	 GOTO	   DISCRD IF EQUAL
DSCRD
	 KEYIN	   *PHZ:1,"? ",*+,*UC,DIM1;
	 CMATCH	   "N",DIM1
	 GOTO	   PLAYX IF EQUAL
	 CMATCH	   "Y",DIM1
	 GOTO	   DSCRD IF NOT	EQUAL
	 GOTO	   DISCRD
DCOM
	 MOVE	   $50$,MILES
	 RESET	   SAFE4,4
	 CMATCH	   "N",SAFE4
	 GOTO	   INV IF EQUAL
	 MATCH	   LETTERD,SLD
	 GOTO	   INV IF EQUAL
	 COMPARE   MILES,LIMITD
	 GOTO	   INV IF LESS
	 MOVE	   LETTERD,SLD
	 MOVE	   $0$,CFSWT
	 MOVE	   MILES,LIMITD
	 MOVE	   "42",HZ
	 SUB	   HI,HZ
	 DISPLAY   *PHZ:5,"SPEED LIMIT:	",LIMITD
	 GOTO	   DISCRD
FCOM
	 CMATCH	   "A",CONDC
	 GOTO	   FIXA	IF EQUAL
	 GOTO	   INV
GCOM
	 CMATCH	   "B",CONDC
	 GOTO	   FIXA	IF EQUAL
	 GOTO	   INV
HCOM
	 CMATCH	   "C",CONDC
	 GOTO	   FIXA	IF EQUAL
	 GOTO	   INV
JCOM
	 CMATCH	   "E",CONDC
	 GOTO	   INV IF LESS
	 CMATCH	   "I",CONDC
	 GOTO	   INV IF NOT LESS
*
. CHANGE YOUR CONDITION
.
FIXA
	 MOVE	   DIM1,CONDC
	 MOVE	   $11$,HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:3,CARDIN
	 RESET	   SAFE3,4
	 CMATCH	   "N",SAFE3
	 GOTO	   DISCRD IF NOT EQUAL
	 MOVE	   LETTERJ,CONDC
	 DISPLAY   *PHZ:3,J,"	  "
	 GOTO	   DISCRD
ICOM
	 MATCH	   LETTERI,SLC
	 GOTO	   INV IF EQUAL
	 COMPARE   LIMITC,OLIMC
	 GOTO	   INV IF LESS
	 DISPLAY   *PHI:5,*RPTCHAR " ":16
	 MOVE	   LETTERI,SLC
	 MOVE	   $0$,FORM2
EOL
	 MOVE	   OLIMC,LIMITC
	 CALL	   LIMIT
	 GOTO	   DISCRD
KCOM
	 MOVE	   $1$,FORM2
	 MOVE	   LETTERY,RETRY
	 RESET	   SAFETY
	 ADD	   $100$,SCORE3
MATS
	 CMATCH	   DIM1,SAFETY
	 GOTO	   DISSA IF EQUAL
	 ADD	   $1$,FORM2
	 BUMP	   SAFETY
	 GOTO	   MATS	IF NOT EOS
	 GOTO	   INV
DISSA
	 RESET	   HAZARD,FORM2
	 CMATCH	   CONDC,HAZARD
	 GOTO	   COOS	IF NOT EQUAL
	 MOVE	   LETTERJ,CONDC
	 MOVE	   $11$,HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:3,J,*RPTCHAR " ":6
	 GOTO	   COO
COOS
	 MATCH	   LETTERN,DIM1
	 GOTO	   DISER IF NOT	EQUAL
	 MATCH	   LETTERD,SLC
	 GOTO	   DISER IF NOT	EQUAL
	 MOVE	   OLIMC,LIMITC
	 CALL	   LIMIT
COO
	 BEEP
	 COMPARE   $1$,CFSWT
	 GOTO	   DISER IF NOT	EQUAL
	 DISPLAY   *PHI:1,SPA,*PHI:1,*+,NAME(PLAYER):
		    " SCORES A	COUP FOURRE !!!",*W
	 ADD	   $300$,SCORE3
DISER
	 RESET	   SAFE3,FORM2
	 CMOVE	   DIM1,SAFE3
	 COMPARE   $4$,FORM2
	 GOTO	   CD IF LESS
	 MOVE	   LETTERI,SLC
	 DISPLAY   *PHI:10,N:
		   *PHI:5,*RPTCHAR " ":16
	 GOTO	   EOL
CD
	 ADD	   $6$,FORM2
	 DISPLAY   *PHI:FORM2,CARDIN
	 GOTO	   DISCRD
*
. PROCESS MILESTONE CARDS
.
OCOM
	 MOVE	   CARDIN,DIM3
	 MOVE	   DIM3,MILES
	 CMATCH	   "J",CONDC
	 GOTO	   INV IF NOT EQUAL
	 COMPARE   MILES,LIMITC
	 GOTO	   INV IF LESS
	 MOVE	   DISTC,FORM10
	 ADD	   MILES,FORM10
	 COMPARE   FORM10,GOAL
	 GOTO	   INV IF LESS
	 COMPARE   $200$,MILES
	 GOTO	   REGAD IF NOT	EQUAL
	 COMPARE   $2$,X200
	 GOTO	   INV IF NOT LESS
	 ADD	   $1$,X200
REGAD
	 MOVE	   FORM10,DISTC
	 COMPARE   $1$,PLAYER
	 GOTO	   MILESTN1 IF NOT EQUAL
	 DISPLAY   *PACOL:ALINE,DIM3
	 ADD	   $1$,ALINE
	 COMPARE   $16$,ALINE
	 GOTO	   NEWDIST IF LESS
	 MOVE	   $13$,ALINE
	 ADD	   $4$,ACOL
	 GOTO	   NEWDIST
MILESTN1
	 DISPLAY   *PBCOL:BLINE,DIM3
	 ADD	   $1$,BLINE
	 COMPARE   $16$,BLINE
	 GOTO	   NEWDIST IF LESS
	 MOVE	   $13$,BLINE
	 ADD	   $4$,BCOL
NEWDIST
	 MOVE	   $12$,HZ
	 ADD	   HI,HZ
	 DISPLAY   *PHZ:4,DISTC
	 CALL	   LIMIT
	 COMPARE   GOAL,DISTC
	 GOTO	   DISCRD IF LESS
	 COMPARE   $1000$,GOAL
	 GOTO	   ENDGAME IF EQUAL
	 COMPARE   $1$,OPER
	 GOTO	   EXT2	IF NOT EQUAL
	 MOVE	   LETTERY,EXT
	 GOTO	   EXT3
EXT2
	 COMPARE   $2$,OPER
	 GOTO	   APR28 IF NOT	EQUAL
	 COMPARE   $2$,PLAYER
	 GOTO	   APR28 IF NOT	EQUAL
	 COMPARE   $0$ TO DISTD
	 GOTO	   MSS1	IF EQUAL
	 COMPARE   $100$,DRAW
	 GOTO	   MAY5	IF LESS
MSS1
	 MOVE	   LETTERN,EXT
	 GOTO	   EXT3
MAY5
	 DISPLAY   *P41:1,*EL,"COMPUTER	WISHES AN EXTENSION !",*W
	 MOVE	   LETTERY,EXT
	 GOTO	   EXT3
APR28
XEXT
	 KEYIN	   *P1:1,*EL,"DO YOU WISH AN EXTENSION TO 1000 M. ? ":
		   *+,*UC,EXT
EXT3
	 CMATCH	   "Y",EXT
	 GOTO	   EXTY	IF EQUAL
	 CMATCH	   "N",EXT
	 GOTO	   XEXT	IF NOT EQUAL
	 GOTO	   ENDGAME
EXTY
EXT31
	 APPEND	   PLAYER,EXT
	 RESET	   EXT
	 MOVE	   $1000$,GOAL
	 MOVE	   $200$,OLIMC
	 MOVE	   $200$,OLIMD
	 MOVE	   $50$,LIMITC
	 MOVE	   $50$,LIMITD
	 MATCH	   LETTERD,SLC
	 GOTO	   CANTOO IF NOT EQUAL
	 DISPLAY   *PHI:5,"SPEED LIMIT:	",LIMITC
	 GOTO	   CANT
CANTOO
	 MOVE	   $200$,LIMITC
	 DISPLAY   *PHI:5,*RPTCHAR " ":16
CANT
	 CALL	   LIMIT
	 MATCH	   LETTERD,SLD
	 GOTO	   DISCRD IF EQUAL
	 MOVE	   $200$,LIMITD
	 MOVE	   $42$,HZ
	 SUB	   HI,HZ
	 DISPLAY   *PHZ:5,*RPTCHAR " ":16
	 GOTO	   DISCRD
+
. END OF GAME  -  COMPUTE SCORES
.
ENDGAME
	 MOVE	   $7SPACE$,XCARDS
	 MOVE	   $7SPACE$,ACARDS
	 MOVE	   $7SPACE$,BCARDS
	 DISPLAY   *ES," C A L C U L A T I N G	 S C O R E S   A T   T H I S":
		   "   T I M E"
	 MOVE	   $1$,REVR
	 COMPARE   $1$,PLAYER
	 GOTO	   XPLAY2 IF NOT EQUAL
ENDGAME1
	 CALL	   SCORE
	 MOVE	   SCORET1,FORM10
	 MOVE	   SCORET2,SCORET1
	 MOVE	   FORM10,SCORET2
	 ADD	   $1$,REVR
	 GOTO	   XPLAY2
ENDGAME2
	 CALL	   SCORE
	 MOVE	   SCORET1,FORM10
	 MOVE	   SCORET2,SCORET1
	 MOVE	   FORM10,SCORET2
	 ADD	   $1$,REVR
	 GOTO	   XPLAY2
*
. DISPLAY WINNING PLAYER'S NAME
.
END
	 MOVE	   $1$,FORM1
	 COMPARE   SCORET1,SCORET2
	 GOTO	   ENDTIE IF EQUAL
	 GOTO	   ENDP2 IF NOT	LESS
	 DISPLAY   *P1:12,*+,NAME(1);
	 COMPARE   $5000$,SCORET1
	 GOTO	   TOURNOVR IF NOT LESS
	 GOTO	   TOURNNEW
ENDTIE
	 DISPLAY   *P1:12,*+,"** TIE **";
	 GOTO	   TOURNEW1
ENDP2
	 DISPLAY   *P1:12,*+,NAME(2);
	 COMPARE   $5000$,SCORET2
	 GOTO	   TOURNOVR IF NOT LESS
	 MOVE	   $2$,FORM1
TOURNNEW
	 DISPLAY   *P12:12," IS	WINNING	THE TOURNAMENT!";
TOURNEW1
	 KEYIN	   *P41:12,"CONTINUE THIS TOURNAMENT (Y/N) ? ",*+,*UC,DIM1;
	 CMATCH	   "N",DIM1
	 GOTO	   TOURNOVR IF EQUAL
	 CMATCH	   "Y",DIM1
	 GOTO	   TOURNEW1 IF NOT EQUAL
	 CALL	   CLEAR
	 GOTO	   AUTO
TOURNOVR
	 DISPLAY   *B,*P12:12,*EL," WON	THE TOURNAMENT!!!!!"
TOURNOVX
	 KEYIN	   *P41:12,"PLAY ANOTHER GAME? ",*+,DIM1
	 CMATCH	   "Y",DIM1
	 GOTO	   EXIT	IF NOT EQUAL
	 TRAP	   TOURNCF IF CFAIL
	 CLOSE	   DISC
	 CHAIN	   "MEELBORN"
TOURNCF
	 DISPLAY   *N,"PROGRAM NOT FOUND"
	 GOTO	   SHUTDOWN
EXIT
	 CLOSE	   DISC
SHUTDOWN
	 SHUTDOWN
*
. DETERMINE CARD TYPE -	LOAD IN	DIM15
.
DETER
	 MOVE	   $19$,FORM2
	 SEARCH	   DIM1	IN LETTERA TO FORM2 USING NUM2A
	 LOAD	   DIM15     FROM      NUM2A OF	A,B,C,D,E,F,G,H,I,J:
				       K,L,M,N,O,P,Q,R,S
	 RETURN
*
. CHECK	SPEED LIMIT
.
LIMIT
	 COMPARE   $2$,X200
	 GOTO	   NSL IF LESS
	 MOVE	   $100$,OLIMC
	 COMPARE   $101$,LIMITC
	 GOTO	   NSL IF LESS
	 MOVE	   $100$,LIMITC
NSL
	 MOVE	   GOAL,FORM10
	 SUB	   DISTC,FORM10
	 COMPARE   LIMITC,FORM10
	 RETURN	   IF NOT LESS
	 MOVE	   FORM10,LIMITC
	 MOVE	   FORM10,OLIMC
	 DISPLAY   *PHI:5,"DIST. TO GO:	",LIMITC
	 RETURN
+
. SCORING ROUTINE
.
SCORE
	 DISPLAY   *PHI:2,"Safeties":
		   *PHI:3,"All Safeties":
		   *PHI:4,"Trip	Completed":
		   *PHI:5,"Bonus for Extension":
		   *PHI:6,"Delayed Action":
		   *PHI:7,"Safe	Trip":
		   *PHI:8,"Shut-out":
		   *PHI:9,"Milestones":
		   *PHI:10,"GAME TOTALS":
		   *PHI:11,"TOURNAMENT TOTALS";
.
	 MOVE	   HI,FORM2
	 ADD	   $19$,FORM2
	 COMPARE   $1$,SCORE3
	 GOTO	   SCRALLS IF LESS
	 DISPLAY   *PFORM2:2,SCORE3;
*
. CHECK	FOR ALL	SAFETIES.
.
SCRALLS	
	 RESET	   SAFETY
	 RESET	   SAFE3
	 MATCH	   SAFETY,SAFE3
	 GOTO	   SCR1000 IF NOT EQUAL
	 DISPLAY   *PFORM2:3,"	 300";
	 ADD	   $300$,SCORE3
*
. CHECK	FOR COMPLETED TRIP.
.
SCR1000
	 COMPARE   GOAL,DISTC
	 GOTO	   SCREXT IF NOT EQUAL
	 DISPLAY   *PFORM2:4,"	 400";
	 ADD	   "400",SCORE3
*
. CHECK	FOR EXTENSION.
.
SCREXT
	 CMATCH	   "N",EXT
	 GOTO	   SCRDELAY IF EQUAL
	 BUMP	   EXT
	 MOVE	   PLAYER,DIM1
	 CMATCH	   DIM1,EXT
	 GOTO	   SCREXT1 IF EQUAL
	 COMPARE   $1000$,DISTD
	 GOTO	   SCRDELAY IF EQUAL
	 GOTO	   SCREXT2
SCREXT1
	 COMPARE   $1000$,DISTC
	 GOTO	   SCRDELAY IF NOT EQUAL
SCREXT2
	 DISPLAY   *PFORM2:5,"	 200";
	 ADD	   $200$,SCORE3
*
. CHECK	FOR DELAYED GAME.
.
SCRDELAY
	 COMPARE   $101$,DRAW
	 GOTO	   SCRSAFE IF LESS
	 COMPARE   GOAL,DISTC
	 GOTO	   SCRSAFE IF LESS
	 DISPLAY   *PFORM2:6,"	 300";
	 ADD	   $300$,SCORE3
*
. CHECK	FOR SAVE TRIP.
.
SCRSAFE
	 COMPARE   $1$,X200
	 GOTO	   SCRSHUT IF NOT LESS
	 COMPARE   GOAL,DISTC
	 GOTO	   SCRSHUT IF LESS
	 DISPLAY   *PFORM2:7,"	 300";
	 ADD	   $300$,SCORE3
*
. CHECK	FOR SHUTOUT.
.
SCRSHUT
	 COMPARE   $1$,DISTD
	 GOTO	   SCRDIST IF NOT LESS
	 DISPLAY   *PFORM2:8,"	 500";
	 ADD	   "500",SCORE3
*
. ADD IN DISTANCE SCORES.
.
SCRDIST
	 DISPLAY   *PFORM2:9,"	",DISTC;
	 ADD	   DISTC,SCORE3
	 ADD	   SCORE3,SCORET1
	 DISPLAY   *PFORM2:10,SCORE3:
		   *PFORM2:11,SCORET1;
	 RETURN
+
. FIRST	LINE OF	AUTOMATIC PLAY
. DETERMINE WHICH CARD TO PLAY
.
APLAY
	 COMPARE   $1$,OPER
	 GOTO	   APLAY1 IF NOT EQUAL
	 CMATCH	   "F",FAST
	 GOTO	   APLAY1 IF EQUAL
	 DISPLAY   *W,*W;
APLAY1
	 COMPARE   $3$,DSCNT
	 GOTO	   DISCARDQ IF NOT LESS
APLAYZ
	 CMATCH	   "D",SLC
	 GOTO	   LO9 IF NOT EQUAL
	 MOVE	   "N01",QUES
	 GOTO	   HAND
LO9
	 CMATCH	   "J",CONDD
	 GOTO	   LO8 IF NOT EQUAL
	 RESET	   SAFE4,4
	 CMATCH	   "N",SAFE4
	 GOTO	   LO8 IF EQUAL
	 MOVE	   "E02",QUES
	 GOTO	   HAND
LO8
	 RESET	   XCARDS
	 RESET	   HAZARD
	 RESET	   SAFETY
	 RESET	   REMEDY
	 CMATCH	   "J",CONDC
	 GOTO	   OPPO	IF EQUAL
*
. IF YOU HAVE A	HAZARDOUS CONDITION - CHECK FOR	SAFETY OR REMEDY
.
HAZ
	 CMATCH	   CONDC,HAZARD
	 GOTO	   HAZ1	IF EQUAL
	 CMATCH	   CONDC,REMEDY
	 GOTO	   ROLLCR IF EQUAL
GVB
	 BUMP	   HAZARD
	 BUMP	   SAFETY
	 BUMP	   REMEDY
	 GOTO	   HAZ IF NOT EOS
	 GOTO	   OPPO
ROLLCR
	 MOVE	   "J03",QUES
	 GOTO	   HAND
*
. HAZARD CONDITION
.
HAZ1
	 MOVE	   SAFETY,QUES
	 RESET	   QUES
	 APPEND	   "04",QUES
	 GOTO	   HAND
HA04
	 MOVE	   REMEDY,QUES
	 RESET	   QUES
	 APPEND	   "05",QUES
	 GOTO	   HAND
*
. NO REMEDY FOR	YOUR HAZARD
. CHECK	FOR PLAYING HAZARD AGAINST OPPONENT
.
OPPO
	 CMATCH	   "J",CONDD
	 GOTO	   ATA IF EQUAL
	 CMATCH	   "J",CONDC
	 GOTO	   MILEST IF EQUAL
	 GOTO	   DISCARD
ATA
	 RESET	   XCARDS
	 RESET	   HAZARD
	 RESET	   SAFETY
	 RESET	   SAFE4
FR1
	 MOVE	   $1$,FORM1
HZD
	 CMATCH	   HAZARD,XCARDS
	 GOTO	   L44 IF NOT EQUAL
	 CMATCH	   SAFETY,SAFE4
	 GOTO	   GOTCRD IF NOT EQUAL
L44
	 BUMP	   XCARDS
	 ADD	   $1$,FORM1
	 GOTO	   HZD IF NOT EOS
	 RESET	   XCARDS
	 BUMP	   SAFETY
	 BUMP	   SAFE4
	 BUMP	   HAZARD
	 GOTO	   FR1 IF NOT EOS
	 CMATCH	   "D",SLD
	 GOTO	   RETES IF EQUAL
	 MOVE	   "D06",QUES
	 GOTO	   HAND
RETES
	 CMATCH	   "J",CONDC
	 GOTO	   MILEST IF EQUAL
*
. NO PLAY
.
DISCARD
	 MOVE	   $0$,FORM1
*
. TRY TO PLAY SPEED LIMIT
.
	 CMATCH	   "D",SLD
	 GOTO	   RESETXX IF EQUAL
	 RESET	   SAFE4,4
	 CMATCH	   "N",SAFE4
	 GOTO	   RESETXX IF EQUAL
	 MOVE	   "D07",QUES
	 GOTO	   HAND
RESETXX
	 CMATCH	   "D",SLC
	 GOTO	   JUN3	IF EQUAL
	 RESET	   SAFE3,4
	 CMATCH	   "N",SAFE3
	 GOTO	   RZER	IF NOT EQUAL
*
. TRY TO PLAY END OF LIMIT
.
JUN3
	 MOVE	   "I08",QUES
	 GOTO	   HAND
RZER
	 RESET	   HAZARD
*
. THROW	AWAY HAZARDS WHICH HAVE	BEEN PREVENTED BY OPPONENT'S SAFETY CARD
.
	 RESET	   SAFE4
	 RESET	   SAFE3,4
	 CMATCH	   "N",SAFE3
	 GOTO	   BNM IF NOT EQUAL
	 MOVE	   "J09",QUES
	 GOTO	   HAND
BNM
	 CMATCH	   " ",SAFE4
	 GOTO	   BNMP	IF EQUAL
	 MOVE	   HAZARD,QUES
	 RESET	   QUES
	 APPEND	   $10$,QUES
	 GOTO	   HAND
BNMP
	 BUMP	   HAZARD
	 BUMP	   SAFE4
	 GOTO	   BNM IF NOT EOS
	 GOTO	   KI8
*
. CHECK	FOR REMEDIES MATCHING SAFETIES IN YOUR HAND
.
SAF
	 RESET	   SAFETY
	 RESET	   REMEDY
	 RESET	   XCARDS
	 MOVE	   $0$,FORM2
JUN3B
	 ADD	   $1$,FORM2
	 COMPARE   $8$,FORM2
	 GOTO	   JUN3A IF NOT	LESS
	 RESET	   XCARDS,FORM2
JUN3D
	 CMATCH	   XCARDS,SAFETY
	 GOTO	   JUN3C IF EQUAL
	 BUMP	   SAFETY
	 BUMP	   REMEDY
	 GOTO	   JUN3D IF NOT	EOS
	 RESET	   SAFETY
	 RESET	   REMEDY
	 GOTO	   JUN3B
*
. I HAVE A SAFETY - CHECK FOR MATCHING REMEDY
.
JUN3C
	 MOVE	  REMEDY,QUES
	 RESET	   QUES
	 APPEND	   $27$,QUES
	 GOTO	   HAND
*
. IF NO	REMEDY - CHECK FOR ANOTHER SAFETY
. RETURNS TO JUN3B - NEXT CARD LOOP
.
*
. PLAY SAFETIES
.
JUN3A
	 RESET	   SAFETY
	 COMPARE   "600",DISTC
	 GOTO	   DUPS	IF LESS
JU1
	 MOVE	   SAFETY,QUES
	 RESET	   QUES
	 APPEND	   $11$,QUES
	 GOTO	   HAND
BUMSS
	 BUMP	   SAFETY
	 GOTO	   JU1 IF NOT EOS
	 RESET	   SAFE3,4
	 CMATCH	   "N",SAFE3
	 GOTO	   DUPS	IF NOT EQUAL
	 MOVE	   "I12",QUES
	 GOTO	   HAND
KI8
	 RESET	   SAFE3
	 RESET	   SAFETY
	 RESET	   REMEDY
*
. THROW	AWAY REMEDIES FOR YOUR SAFETIES
.
HY6
	 CMATCH	   SAFETY,SAFE3
	 GOTO	   HY7 IF NOT EQUAL
	 MOVE	   REMEDY,QUES
	 RESET	   QUES
	 APPEND	   $13$,QUES
	 GOTO	   HAND
HY7
	 BUMP	   SAFE3
	 BUMP	   SAFETY
	 BUMP	   REMEDY
	 GOTO	   HY6 IF NOT EOS
	 GOTO	   SAF
*
. THROW	AWAY DUPLICATE CARDS
.
DUPS
	 MOVE	   $0$,FORM1
	 COMPARE   $2$,X200
	 GOTO	   RESETER IF LESS
	 MOVE	   "O26",QUES
	 GOTO	   HAND
RESETER
	 ADD	   $1$,FORM1
	 RESET	   XCARDS,FORM1
	 GOTO	   DISCARDX IF EOS
	 MOVE	   XCARDS,DIM1
	 CMATCH	   " ",DIM1
	 GOTO	   RESETER IF EQUAL
	 CMATCH	   "O",DIM1
	 GOTO	   RESETER IF NOT LESS
CKL
	 BUMP	   XCARDS
	 GOTO	   RESETER IF EOS
	 CMATCH	   DIM1,XCARDS
	 GOTO	   GOTCRD IF EQUAL
	 GOTO	   CKL
DISCARDX
	 RESET	   HAZARD
	 RESET	   XCARDS
	 MOVE	   X200,FORM1
*
. THROW	AWAY 200 MILESTONE IF TOO MANY IN HAND (ALREADY	PLAYED)
.
Q20
	 CMATCH	   "O",XCARDS
	 GOTO	   Q21 IF NOT EQUAL
	 ADD	   $1$,FORM1
	 COMPARE   $3$,FORM1
	 GOTO	   Q21 IF LESS
	 MOVE	   "O25",QUES
	 GOTO	   HAND
Q21
	 BUMP	   XCARDS
	 GOTO	   Q20 IF NOT EOS
*
. THROW	AWAY SMALLEST MILESTONE
.
Q22
	 COMPARE   $25$,LIMITC
	 GOTO	   NO25	IF EQUAL
	 MOVE	   "S20",QUES
	 GOTO	   HAND
NO25
	 MOVE	   "R21",QUES
	 COMPARE   $50$,LIMITC
	 GOTO	   NO50	IF EQUAL
	 GOTO	   HAND
NO50
	 MOVE	   "Q22",QUES
	 COMPARE   "75",LIMITC
	 GOTO	   NO75	IF EQUAL
	 GOTO	   HAND
NO75
	 MOVE	   "P23",QUES
	 GOTO	   HAND
NO100
	 COMPARE   $2$,X200
	 GOTO	   JU2 IF LESS
	 MOVE	   "O24",QUES
	 GOTO	   HAND
JU2 
	 MOVE	   HAZARD,QUES
	 RESET	   QUES
	 APPEND	   "14",QUES
	 GOTO	   HAND
JU3 
	 BUMP	   HAZARD
	 GOTO	   JU2 IF NOT EOS
*
. THROW	AWAY ANY CARD
.
DISCARDQ 
	 ADD	   $1$,CN
	 COMPARE   $8$,CN
	 GOTO	   OKR IF LESS
	 MOVE	   $1$,CN
OKR
	 RESET	   XCARDS,CN
	 CMATCH	   " ",XCARDS
	 GOTO	   DISCARDQ IF EQUAL
	 MOVE	   CN,FORM1
	 MOVE	   $1$,HZ
	 ADD	   HI,HZ
	 GOTO	   GOTCRD
*
. PLAY MILESTONES
.
MILEST
	 CMATCH	   "J",CONDC
	 GOTO	   OPPO	IF NOT EQUAL
MS100 
	 MOVE	   "P16",QUES
	 COMPARE   $100$,LIMITC
	 GOTO	   MS75	IF LESS
	 GOTO	   HAND
MS75 
	 MOVE	   "Q17",QUES
	 COMPARE   "75",LIMITC
	 GOTO	   MS50	IF LESS
	 GOTO	   HAND
MS50 
	 MOVE	   "R18",QUES
	 COMPARE   $50$,LIMITC
	 GOTO	   MS25	IF LESS
	 GOTO	   HAND
MS25  
	 MOVE	   "S19",QUES
	 COMPARE   $25$,LIMITC
	 GOTO	   MS200 IF LESS
	 GOTO	   HAND
MS200  
	 MOVE	   "O15",QUES
*
. CHECK	FOR THIS CARD IN HAND
.
HAND  
	 RESET	   QUES
	 RESET	   XCARDS
	 MOVE	   $1$,FORM1
HAVE  
	 CMATCH	   QUES,XCARDS
	 GOTO	   GOTCRD IF EQUAL
	 ADD	   $1$,FORM1
	 BUMP	   XCARDS
	 GOTO	   HAVE	IF NOT EOS
	 RESET	   QUES,2
	 MOVE	   QUES,DIM2
	 MOVE	   DIM2,BRNCH
	 BRANCH	   BRNCH OF LO9,LO8,GVB,HA04,OPPO,RETES,RESETXX:
		    RZER,BNM,BNMP,BUMSS,DUPS,HY7,JU3:
		    MS100,MS75,MS50,MS25,DISCARD:
		    NO25,NO50,NO75,NO100,JU2,Q22:
		    RESETER,JUN3B
CLEAR  
	 MOVE	   $0$,FORM1
	 MOVE	   FORM1,SCORE1
	 MOVE	   FORM1,SCORE2
	 MOVE	   FORM1,SCORE3
	 MOVE	   FORM1,SCORE4
	 MOVE	   LETTERN,RETRY
	 CLEAR	   PLAY2
	 MOVE	   LETTERE,CONDA
	 MOVE	   LETTERE,CONDB
	 MOVE	   LETTERE,CONDC
	 MOVE	   LETTERE,CONDD
	 MOVE	   $200$,LIMITA
	 MOVE	   $200$,LIMITB
	 MOVE	   $200$,LIMITC
	 MOVE	   $200$,LIMITD
	 MOVE	   $200$,OLIMA
	 MOVE	   $200$,OLIMB
	 MOVE	   $200$,OLIMC
	 MOVE	   $200$,OLIMD
	 MOVE	   LETTERI,SLA
	 MOVE	   LETTERI,SLB
	 MOVE	   LETTERI,SLC
	 MOVE	   LETTERI,SLD
	 MOVE	   $4SPACE$,SAFE1
	 MOVE	   $4SPACE$,SAFE2
	 MOVE	   $4SPACE$,SAFE3
	 MOVE	   $4SPACE$,SAFE4
	 MOVE	   FORM1,A200
	 MOVE	   FORM1,B200
	 MOVE	   FORM1,X200
	 MOVE	   FORM1,DISTA
     	 MOVE	   FORM1,DISTB
	 MOVE	   FORM1,DISTC
	 MOVE	   FORM1,DISTD
	 MOVE	   FORM1,MILES
	 MOVE	   "700",GOAL
	 MOVE	   FORM1,REVR
	 MOVE	   $1$,FORM1
	 READ	   DISC,FORM1;DECK
	 MOVE	   DECK,CARDS
	 MOVE	   $0$,DRAW
	 MOVE	   PORT,PLAYER
	 MOVE	   $7$,CNA
	 MOVE	   $7$,CNB
	 MOVE	   LETTERN,EXT
	 MOVE	   $13$,ALINE
	 MOVE	   ALINE,BLINE
	 MOVE	   $1$,ACOL
	 MOVE	   "41",BCOL
	 MOVE	   $2$,CNTA
	 MOVE	   $2$,CNTB
	 MOVE	   $2$,CNTC
	 MOVE	   $3$,CNTD
	 MOVE	   $4$,CNTE
	 MOVE	   $6$,CNTF
	 MOVE	   $6$,CNTG
	 MOVE	   $6$,CNTH
	 MOVE	   $6$,CNTI
	 MOVE	   "14",CNTJ
	 MOVE	   $1$,CNTK
	 MOVE	   $1$,CNTL
	 MOVE	   $1$,CNTM
	 MOVE	   $1$,CNTN
	 MOVE	   $4$,CNTO
	 MOVE	   $12$,CNTP
	 MOVE	   $10$,CNTQ
	 MOVE	   $10$,CNTR
	 MOVE	   $10$,CNTS
	 MOVE	   $0$,DSCNT
	 RETURN
*
. DISPLAY UPDATED CARD COUNT ON	THE SCREEN.
.
UPDCARD	 
	 BRANCH	   NUM2A OF UPDCARDA,UPDCARDB,UPDCARDC,UPDCARDD,UPDCARDE:
		   UPDCARDF,UPDCARDG,UPDCARDH,UPDCARDI,UPDCARDJ,UPDCARDK:
		   UPDCARDL,UPDCARDM,UPDCARDN,UPDCARDO,UPDCARDP,UPDCARDQ:
		   UPDCARDR,UPDCARDS
	 RETURN
UPDCARDA 
	 SUB	   $1$,CNTA
	 DISPLAY   *P3:18,CNTA;
	 RETURN
UPDCARDB 
	 SUB	   $1$,CNTB
	 DISPLAY   *P3:19,CNTB;
	 RETURN
UPDCARDC 
	 SUB	   $1$,CNTC
	 DISPLAY   *P3:20,CNTC;
	 RETURN
UPDCARDD 
	 SUB	   $1$,CNTD
	 DISPLAY   *P3:21,CNTD;
	 RETURN
UPDCARDE 
	 SUB	   $1$,CNTE
	 DISPLAY   *P3:22,CNTE;
	 RETURN
UPDCARDF 
	 SUB	   $1$,CNTF
	 DISPLAY   *P17:18,CNTF;
	 RETURN
UPDCARDG 
	 SUB	   $1$,CNTG
	 DISPLAY   *P17:19,CNTG;
	 RETURN
UPDCARDH 
	 SUB	   $1$,CNTH
	 DISPLAY   *P17:20,CNTH;
	 RETURN
UPDCARDI 
	 SUB	   $1$,CNTI
	 DISPLAY   *P17:21,CNTI;
	 RETURN
UPDCARDJ 
	 SUB	   $1$,CNTJ
	 DISPLAY   *P16:22,CNTJ;
	 RETURN
UPDCARDK 
	 SUB	   $1$,CNTK
	 DISPLAY   *P32:18,CNTK;
	 RETURN
UPDCARDL 
	 SUB	   $1$,CNTL
	 DISPLAY   *P32:19,CNTL;
	 RETURN
UPDCARDM 
	 SUB	   $1$,CNTM
	 DISPLAY   *P32:20,CNTM;
	 RETURN
UPDCARDN 
	 SUB	   $1$,CNTN
	 DISPLAY   *P32:21,CNTN;
	 RETURN
UPDCARDO 
	 SUB	   $1$,CNTO
	 DISPLAY   *P10:24,CNTO;
	 RETURN
UPDCARDP 
	 SUB	   $1$,CNTP
	 DISPLAY   *P18:24,CNTP;
	 RETURN
UPDCARDQ 
	 SUB	   $1$,CNTQ
	 DISPLAY   *P26:24,CNTQ;
	 RETURN
UPDCARDR 
	 SUB	   $1$,CNTR
	 DISPLAY   *P34:24,CNTR;
	 RETURN
UPDCARDS 
	 SUB	   $1$,CNTS
	 DISPLAY   *P42:24,CNTS;
	 RETURN

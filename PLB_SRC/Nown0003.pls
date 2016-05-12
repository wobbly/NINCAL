*.............................................................................
.THIS PROGRAM READ THE NINOWN FILE & CHECKS THE REVISION DATE AGAINST THE 
.OPERATOR SELECTED DATE AND WRITES OUT SELECTED RECORDS TO CALOWN/TEXT.
*.............................................................................
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NOWNDD.INC
CALOWN   FILE      FIXED=170,STATIC=6
.............................
RELEASE  INIT      "2.2"        DMB 08SEP2003	Added code to grab dates from dsinit
;RELEASE  INIT      "2.1"        ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "2.0"        DLH 13MAR92
RMM      DIM       2
Rcc      DIM       2
RDD      DIM       2
RYY      DIM       2
ANS      DIM       1
FILE     DIM       8
COUNT    FORM      5
HITS     FORM      5
KYY      DIM       2
KDD      DIM       2
KMM      DIM       2
.
RMM1     FORM      2
RDD1     FORM      2
RYY1     FORM      2
ONE      FORM      "1"
oWNdate  form      5
today1   form      5
.
.MAIN
         TRAP      EOJ IF F5
.
  

;patch2.2
	rep	lowup,PROGRAM
	match	"NOWN0003",PROGRAM   .case sensitive
	if not equal
;patch2.2
		MOVE      "NOWN0003" TO PROGRAM
		MOVE      "NINCAL" TO COMPNME
		MOVE      "OWNER REVISION PICKOFF" TO STITLE
		CALL      PAINT
START  
		KEYIN     *P1:24,"PICKS OFF OWNERS FROM ENTERED DATE TO PRESENT":
		*P01:12,"ENTER BEGINNING PICKOFF DATE: MM/DD/YY":
		*P31:12,KMM
		REPLACE   " 0" IN KMM
		KEYIN     *P31:12,*DV,KMM,"/",*P34:12,KDD
		REPLACE   " 0" IN KDD
		KEYIN     *P34:12,*DV,KDD,*P37:12,KYY
		REPLACE   " 0" IN KYY
		KEYIN     *P37:12,*DV,KYY:
	                   *P40:12,"OK ? ",ANS,*P40:12,*EL
	else
		MOVE      "NINCAL" TO COMPNME
		MOVE      "OWNER REVISION PICKOFF" TO STITLE
		CALL      PAINT
		DISPLAY     *P1:24,"PICKS OFF OWNERS FROM ENTERED DATE TO PRESENT":
		*P01:12,"ENTER BEGINNING PICKOFF DATE: MM/DD/YY"
		move	FUNC to KMM
		REPLACE   " 0" IN KMM
		move	INITS to KDD
		REPLACE   " 0" IN KDD
		move	comment to KYY
		REPLACE   " 0" IN KYY
		MOVE	YES to ANS
		KEYIN     *P31:12,*DV,KMM,"/",*P34:12,*DV,KDD,"/",*P37:12,*DV,KYY:
	                   *P40:12,"OK ? ",*t10,ANS,*P40:12,*EL
	endif
;patch2.2



         CMATCH    "N" TO ANS
         GOTO      START  IF EQUAL
         MOVE      KMM TO MM
         MOVE      KDD TO DD
         MOVE      KYY TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
         TRAP      NOFILE GIVING ERROR IF IO
         MOVE      "CALOWN" TO FILE
         DISPLAY   *P1:24,*EL,"OPENING ",FILE;
         IFNZ      PC
         PREPARE   CALOWN,"CALOWN/CAL:PRINT"
         XIF
         IFZ       PC
.START PATCH 2.1 REPLACED LOGIC
.         PREPARE   CALOWN,"g:\DATA\CALOWN.CAL"
         PACK   STR35,NTWKPATH1,"CALOWN.CAL"
         PREPARE   CALOWN,STR35
.END PATCH 2.1 REPLACED LOGIC
         XIF
         MOVE      "CALOWN" TO FILE
         DISPLAY   *P1:24,*EL,"OPENING ",FILE;
         DISPLAY   *P1:24,*EL,"FILES OPEN";
         DISPLAY   *P20:13,*EL,"RECORDS READ ="
READOWN  CALL      NOWNSEQ
         GOTO      EOJ IF OVER
         ADD       ONE TO COUNT
         DISPLAY   *P35:13,COUNT
         UNPACK    OWNRDTE INTO MM,DD,RCC,YY
         MATCH     B2,YY
         GOTO      READOWN IF EQUAL
         CALL      CVTJUL
         MOVE      juldays TO OwnDATE
         compare   today1 to owndate
         goto      readown if less
.         MOVE      RMM TO RMM1
.         MOVE      RDD TO RDD1
.         MOVE      RYY TO RYY1
.         COMPARE   NYY TO RYY1
.         GOTO      READOWN IF LESS
.         GOTO      MM IF EQUAL
.         GOTO      WRITE IF NOT LESS
.MM       COMPARE   NMM TO RMM1
.         GOTO      READOWN IF LESS
.         GOTO      DD IF EQUAL
.         GOTO      WRITE IF NOT LESS
.DD       COMPARE   NDD TO RDD1
.         GOTO      READOWN IF LESS
WRITE    WRITE      CALOWN,SEQ;OWNLOC:
                               OWNBLK:
                               OWNLON:
                               OWNLONM:
                               OWNOCPY:
                               OWNLOSA:
                               OWNLOCTY:
                               OWNLOS:
                               OWNLOZC:
                               OWNNEC:
                               OWNCTN:
                               OWNTELE:
                               OWNPASS:
                               OWNRDTE:
                               OWNgally:
                               OWNTAXID:
                               ownfax:
                               ownfax2:
                               ownstat
         ADD       ONE TO HITS
         DISPLAY   *P20:14,"RECORDS FOUND =",HITS
         GOTO      READOWN
...............................................................................
EOJ      COMPARE   ONE TO HITS
         CALL      NOHITS IF LESS
         WEOF      CALOWN,SEQ
         CLOSE     CALOWN
.         KEYIN     *P1:23,*EL,*B,"PICK DONE HIT (ENTER)",*T10,ANS
         shutdown  "cls"
			STOP
...............................................................................
NOHITS   WRITE     CALOWN,SEQ;"                                 "
         RETURN
...............................................................................
NOFILE   TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,"ERROR"
         STOP
         INCLUDE   NOWNIO.INC
         INCLUDE   COMLOGIC.INC


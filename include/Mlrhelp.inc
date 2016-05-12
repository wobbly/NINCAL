;    Last change:  ML   21 Oct 1998    2:53 pm
.............................................................................
.MLRHELP - AIM SEARCH FOR MAILER.
.
.ENTER WITH - TOPWIND,BOTWIN,LWIND,RWIND -  TO DEFINE WINDOW.
.EXIT  WITH - MNUM, MCOMP,  NEED TO MOVE TO OTHER VARIABLES AND RETURN AT   .
.                           EXIT PARAGRAPH.
.
. 19oct98 change chkkey to chkkeym, changed ntenof to ntenofM
. to avoide dupe paragraph names
. REVISED TO HANDLE SMALLER WINDOW & USE NMLRIO.INC - JAN/FEB 92 DLH.
.............................................................................
MLRHELP  CLEAR     STR2
	 MOVE      TOPWIND TO STR2
	 TYPE      STR2    *VALID NUMBER?
	 CALL      MSETWIN IF NOT EQUAL
	 CALL      MSETWIN IF EOS
	 COMPARE   C0 TO TOPWIND
	 CALL      MSETWIN IF NOT GREATER
	 DISPLAY   *SETSWALL  TOPWIND:BOTWIND:LWIND:RWIND;
         move      yes to str1
	 KEYIN     *P1:1,*ES:
		   *P1:6,*EL,"I WILL HELP YOU FIND THE MAILER NUMBER",*R:
		   *P1:6,*EL,"THAT YOU WANT":
		   *R,*P1:6:
		   *R,*P1:6,"DO YOU WANT A FREE FLOAT SEARCH ? ,":
		   *R,*P1:6,"ie: FIND 'AMERICAN' ANYWHERE IN MAILER ":
		   *R,*P1:6,"NAME ",*T60,*RV,*UC,STR1;
	 CMATCH    YES TO STR1
	 GOTO      FREFLOAT IF EQUAL
	 MOVE      "01L" TO str3
KEYMLR   KEYIN     *P1:1,*ES,"TYPE IN MAILER NAME : ":
		   *P1:2,*DV,MCOMP:
		   *P1:2,*T60,*EDIT,MCOMP
	 MOVE      MCOMP TO STR24
	 CMATCH    "*" IN MCOMP
	 GOTO      MLRAEXT1 IF EQUAL
	 KEYIN     *P1:2,*DV,MCOMP,*P1:3,"OK ? ",*T60,*uc,STR1:
		   *P1:3,*EL;
	 CMATCH    NO TO STR1
	 GOTO      KEYMLR IF EQUAL
	 CMATCH    B1 TO MCOMP
	 GOTO      ntenofM IF EQUAL
	 GOTO      ntenofM IF EOS
	 MATCH     "01L" TO str3
	 GOTO      PACKM1 IF EQUAL
	 PACK      MLRAKEY FROM str3,MCOMP
	 CALL      CHKKEYM
	 GOTO      MLREADA
PACKM1   PACK      MLRAKEY FROM str3,MCOMP,Question,Question,Question
	 CALL      CHKKEYM
	 GOTO      MLREADA
FREFLOAT MOVE      "01F" TO str3
	 GOTO      KEYMLR
MLREADA
	 CLEAR     MNUM
	 MOVE      "*****NO MNUM FOUND*****" TO MCOMP
	 CALL      NMLRAIM
	 IF        OVER
	 MOVE      STR24 TO MCOMP
	 GOTO      MLRAEXIT
	 ENDIF
	 MOVE      MNUM TO STR4
	 GOTO      DISMLRA
CHKKEYM  MOVELPTR  MLRAKEY TO n2
	 GOTO      ntenofM IF ZERO
	 COMPARE   C6 TO n2
	 GOTO      ntenofM IF LESS         *NOT ENOUGH INFO, LET THEM KNOW
	 RETURN                             *OK, RETURN
MLREADA1 CLEAR     MNUM
	 MOVE      "*****NO MNUM FOUND*****" TO MCOMP
	 CALL      NMLRKG
	 IF        OVER
	 MOVE      STR24 TO MCOMP
         move      b1 to mfax
	 GOTO      MLRAEXIT
	 ENDIF
	 MATCH     MNUM TO STR4
	 GOTO      MLREADA1 IF EQUAL
	 MOVE      MNUM TO STR4
	 GOTO      DISMLRA
MLREADA2 CLEAR     MNUM
	 MOVE      "*****NO MNUM FOUND*****" TO MCOMP
	 IFZ       PC
	 CALL      NMLRKGP
	 XIF
	 IF        OVER
	 MOVE      STR24 TO MCOMP
	 GOTO      MLRAEXIT
	 ENDIF
	 MATCH     MNUM TO STR4
	 GOTO      MLREADA2 IF EQUAL
	 MOVE      MNUM TO STR4
	 GOTO      DISMLRA
DISMLRA  DISPLAY   *P1:11,*EL,"##",MNUM,"  ",MCOMP;
	 ENDSET    MCOMP
	 IFZ       PC
	 KEYIN     *P1:6,"ENTER TO CONTINUE, (P)rior  (*) TO END",*T254,*uc,STR1;
	 XIF
	 IFNZ      PC
	 KEYIN     *P1:6,"ENTER TO CONTINUE  (*) TO END",*T254,*uc,STR1;
	 XIF
	 CMATCH    STAR TO STR1
	 GOTO      MLRAEXT1 IF EQUAL
	 CMATCH    "P" TO STR1
	 GOTO      MLREADA2 IF EQUAL
	 GOTO      MLREADA1
.............................................................................
ntenofM   DISPLAY   *P1:24,*BLINKON,*HON,*EL,"NOT ENOUGH INFO TO SEARCH",*w,*b:
		   *HOFF;
	 GOTO      MLRHELP
.............................................................................
MLRAEXIT 
	 clear     mnum
	 MOVE      YES TO OVER
MLRAEXT1 DISPLAY   *P1:1,*ES,*RESETSW:
		   *P20:24,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
		   " or (?) FOR HELP",*HOFF;
.         MOVE      STR24 TO MCOMP
	 RETURN
................
MSETWIN  MOVE      "18" TO TOPWIND    NO VALUES GIVEN,SET TO DEFAULTS
	 MOVE      C24 TO BOTWIND
	 MOVE      "40" TO LWIND
	 MOVE      C80 TO RWIND
	 RETURN
............................................................................

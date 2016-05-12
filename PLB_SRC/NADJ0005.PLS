PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NADJDD.inc
RELEASE INIT	   "1.0"        JD   30mar93
.
PASSWORD DIM       5
. ...........
         MOVE      "NADJ0005" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "MOD ADJUSTMENT AMEND ##" TO STITLE
         CALL      PAINT
	 KEYIN     *P16:10,"Enter Password to Add, Hit Enter for Inquiry":
                   *EOFF,PASSWORD,*EON;
         MATCH     "COSMO",PASSWORD
         GOTO      KEYLR IF EQUAL
         STOP
KEYLR   KEYIN     *P31:4," FIX ADJUSTMENTS",*P1:12,"PLEASE ENTER 'LR ##' ":
                  "OR '*' TO END.",*P56:12,*JR,*ZF,NADJFLD;
         SCAN      "*" IN NADJFLD
         STOP      IF EQUAL
        TYPE NADJFLD
        GOTO READADJ IF EQUAL
        GOTO KEYLR IF EOS
         GOTO      KEYLR IF NOT EQUAL
.
READADJ CALL      NADJKEY
        GOTO      BUSYADJ IF NOT OVER
        BEEP
        DISPLAY   *P1:24,*EL,"ADJUSTMENT RECORD OF 'LR ##' ",NADJFLD:
                  " NOT FOUND",*W;
        GOTO      KEYLR
BUSYADJ  CMATCH    "*",ASCODE
         GOTO      RECBUSY IF EQUAL
BUSY	 MOVE      "*" TO ASCODE
	 GOTO      UPD
REMOVE   MOVE	   B1 TO ASCODE
UPD	 CALL      NADJUPD
         TRAPCLR   IO
What     Keyin    *p10:15,*ef,"Fix (A)mendment # or (T)otals?",str1
	if	(str1 = "T")
	goto	Totals
	ElseIf	(str1 = "A")
	goto	Keynum
	endif
	goto	What
KEYNUM   KEYIN    *P10:15,*EL,"AMENDMENT ## IS  ",*DV,ASAMNUM:
		  b1,"new num ",*rv,asamnum,*P75:15:
                   "OK ? ",STR1;
          CMATCH    NO TO STR1
         GOTO       KEYlr IF EQUAL
        MOVE      "J",ASCODE
        CALL      NADJUPD
MORINPT KEYIN     *P1:24,*EL,"MORE  INPUT ? ",STR1;
        CMATCH    YES,STR1
        GOTO      KEYLR IF EQUAL
        CMATCH    NO,STR1
        GOTO      MORINPT IF NOT EQUAL
.
        STOP
Totals
	Display	*p10:14,*ef:
		*p10:15,"AR     ",ASRECADJ:
		*p10:16,"AP     ",ASPAYAD1:
		*p10:17,"AP2    ",ASPAYAD2:
		*p10:18,"AP3=*LR",ASPAYAD3:
		*p10:19,"LR     ",ASLRINC:
		*p10:20,"NIN    ",ASNININC:
		*p10:21,"*NIN   ",ASXNINC
	KEyin	*p10:14,"Make your Changes":
		*p10:15,"AR     ",*RV,*JR,ASRECADJ:
		*p10:16,"AP     ",*RV,*JR,ASPAYAD1:
		*p10:17,"AP2    ",*RV,*JR,ASPAYAD2:
		*p10:18,"AP3=*LR",*RV,*JR,ASPAYAD3:
		*p10:19,"LR     ",*RV,*JR,ASLRINC:
		*p10:20,"NIN    ",*RV,*JR,ASNININC:
		*p10:21,"*NIN   ",*RV,*JR,ASXNINC
	Display	*p10:14,*ef,"OK ???":
		*p10:15,"AR     ",ASRECADJ:
		*p10:16,"AP     ",ASPAYAD1:
		*p10:17,"AP2    ",ASPAYAD2:
		*p10:18,"AP3=*LR",ASPAYAD3:
		*p10:19,"LR     ",ASLRINC:
		*p10:20,"NIN    ",ASNININC:
		*p10:21,"*NIN   ",ASXNINC
	 KEYIN     *P1:24,*EL,"Save Changes ? ",STR1;
	 if	(str1 = "Y" or str1 = "y")
	 move	"J",ASCODE
	 call	NADJUPD
	 Endif
	 goto	KeyLR
		
		
		
RECBUSY BEEP
        Keyin   *P1:24,*EL,"THE REQUESTED RECORD IS IN USE. PL":
                  "EASE TRY LATER (R) to release it",str1
	if	(str1 = "R")
	 move	"J",ASCODE
	 call	NADJUPD
	 endif
        GOTO      KEYLR
	 INCLUDE   NADJIO.inc
	 INCLUDE   COMLOGIC.inc


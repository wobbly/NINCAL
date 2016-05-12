;    Last change:  JS    6 Oct 1999   12:30 pm
..............................................................................
.
. NADJCLIO INCLUSION
. NIN ADJUSTMENT APPLICATION ROUTINE
.
. APPLIES ADJUSTMENT AMOUNTS TO COMPUTED AMOUNTS FROM THE NINVCALC
. ROUTINE IN THE NINVCLIO INCLUSION.
.
..............................................................................
.
NADJCALC
.         MOVE      ASRECADJ TO CVTFLD
.        display   *p01:25,"adjar"
.         CALL      CVT
         MOVE      ARAMT TO ARJ
         ADD       asrecadj TO ARamt
.         MOVE      ARJ TO ARAMT
.         ADD       NUM102 TO ARWOPP
.
.         MOVE      ASPAYAD1 TO CVTFLD
.        display   *p01:25,"adjap1"
.         CALL      CVT
.         MOVE      APAMT TO AP1J
.        ADD       NUM102 TO AP1J
.         MOVE      AP1J TO APAMT
         ADD       aspayad1 TO APamt
.
.         MOVE      ASPAYAD2 TO CVTFLD
.        display   *p01:25,"adjap2"
.         CALL      CVT
.         MOVE      AP2AMT TO AP2J
.         ADD       NUM102 TO AP2J
.         MOVE      AP2J TO AP2AMT
         ADD       aspayad2 TO AP2amt
.begin PLI Patch
.  	add 	ASNININC,NInInc
	add	ASXninc,XNIncamt
  	ADD       aspayad3 TO AP3amt
.end PLI Patch
.
.         MOVE      ASLRINC TO CVTFLD
.        display   *p01:25,"adjlrinc"
.         CALL      CVT
         ADD       aslrinc TO LRINC
.
.         MOVE      ADJSTAX TO CVTFLD
.        display   *p01:25,"adjstax"
.         CALL      CVT
.         ADD       NUM102 TO STAX
.         ADD       NUM102 TO TAXES
..
.         MOVE      ADJCTAX TO CVTFLD
.|        display   *p01:25,"adjctax"
.|         CALL      CVT
.         ADD       NUM102 TO CTAX
.         ADD       NUM102 TO TAXES
.
.         MOVE      ADJPOST TO CVTFLD
.        display   *p01:25,"adjpost"
.         CALL      CVT
.         ADD       NUM102 TO POST
.
         RETURN
.
CVT
.         ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         GOTO      CVTOK IF EQUAL                ITS OK.
FORMERR
.  DISPLAY   *P01:24,*EL,*B,"Format error in NADJCALC. ":
.                   "CVTFLD=",CVTFLD,*W9
.         MOVE      B10 TO CVTFLD
.         RETURN
CVTMP
.    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
CVTOK    MOVE      CVTFLD TO NUM102
         MULTIPLY  ".01"  BY NUM102
         RETURN

;    Last change:  JD   15 Sep 1999    3:41 pm
..............................................................................
.
. NJSTCLIO INCLUSION
. NIN DETAIL ADJUSTMENT APPLICATION ROUTINE
.
. APPLIES ADJUSTMENT AMOUNTS TO COMPUTED AMOUNTS FROM THE NINVCALC
. ROUTINE IN THE NINVCLIO INCLUSION.
.
.patch 1.1 PLI Sep 2007
..............................................................................
.
NJSTCALC
.         MOVE      JSTAR TO CVTFLD
.         CALL      CVT
         ADD       jstar TO FAR
         ADD       jstar TO ARWOPP
.
.         MOVE      JSTAP1 TO CVTFLD
.         CALL      CVT
         ADD       jstap1 TO FAP1
         ADD       jstap1 TO AP
.
.         MOVE      JSTAP2 TO CVTFLD
.         CALL      CVT
         ADD       jstap2 TO FAP2
         ADD       jstap2 TO AP
.begin patch 1.1
         ADD       jstap3 TO FAP3
         ADD       jstap3 TO AP
         Add 	    JstXninc,Xninc		
.end patch 1.1
.
.        MOVE      JSTLRINC TO CVTFLD
.        CALL      CVT
         ADD       jstlrinc to lRINC
         ADD       jstNINinc to NINC
.
.         MOVE      JSTSTAX TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO STAX
.         ADD       NUM102 TO TAXES
.
.         MOVE      JSTCTAX TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO CTAX
.         ADD       NUM102 TO TAXES
.
.         MOVE      JSTPOST TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO POST
.
          add       jststax to taxes
          add       jststax to stax
.
.         MOVE      JSTCTAX TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO CTAX
.         ADD       NUM102 TO TAXES
          add       jstctax to taxes
          add       jstctax to ctax
.
.         MOVE      JSTPOST TO CVTFLD
.         CALL      CVT
.         ADD       NUM102 TO POST
         add        jstpost to post
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
.FORMERR  DISPLAY   *P01:24,*EL,*B,"Format error in NJSTCALC. ",*W9
.         MOVE      B10 TO CVTFLD
.         RETURN
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.CVTOK    MOVE      CVTFLD TO NUM102
.         MULTIPLY  ".01"  BY NUM102
         RETURN
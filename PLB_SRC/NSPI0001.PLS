.
.
.
. ***************************************************************************
. *  THIS PROGRAM IS FOR SPEC INST MASTER MAINTENANCE TO CMP OR NIN FILES
. ***************************************************************************
.
.
.
PC	 EQU       0
         INCLUDE   COMMON.inc
	 INCLUDE   CONS.inc
	 INCLUDE   NSPIDD.inc
.
RELEASE  INIT	   "2.0"          DLH   20FEB92
.
SWITCH  FORM  1
LIN     FORM  2
CO      FORM  1
.
START   MOVE	   "NSPI0001" TO PROGRAM
	 MOVE      "NINCAL" TO COMPNME
	 MOVE      "ORDER/LCR SPECIAL INSTRUCTIONS" TO STITLE
	 CALL	    PAINT
A       KEYIN      *P17:5,*EL,"(N)AMES IN THE NEW'S  OR (F)INISHED",STR1
        REP        "N2A1",STR1
	 CMATCH     "F" TO STR1
	 STOP       IF EQUAL
.
A1      DISPLAY *P1:6,*EF,*P35:6,"1 - ADD",*P35:7,"2 - DELETE",*P35:8:
        "3 - REVIEW",*P35:9,"4 - UPDATE",*P35:10,"5 - TERMINATE",*P28:11:
        "ENTER FUNCTION TO BE RUN";
A15     KEYIN   *P53:11,*JR,*ZF,*it,*lc,NSPIFLD;
	MOVE    NSPIFLD TO SPINO
	move    spino to n3
        BRANCH  n3 OF A2,A7,A5,A9,C2
. A2=ADD, A7=DELETE, A5=REVIEW, A9=UPDATE, C2=TERMINATE
.
        GOTO    A15
.
. ******* ADDITION TO THE FILE ROUTINE *******
.
A2     KEYIN         *P1:4,*EF,"ENTER NEW SPEC INST NUMBER: ",*JR,NSPIFLD
       MATCH         "00",NSPIFLD
       GOTO          A2 IF EQUAL
       GOTO          A1 IF EOS
       MATCH         "99",NSPIFLD
       GOTO          A2 IF EQUAL
        MOVE       "0",SWITCH
	MOVE       NSPIFLD TO SPINO 
        rep        zfill in spino
	 CALL	    NSPITST
        GOTO       C3 IF NOT OVER
        KEYIN      *P1:7,*EL,"ENTER SPEC INST DESCRIPTION:   ",*rv,*edit,INST1:
                     *P1:9,*EL,"ENTER 2ND LINE:                ",*rv,*edit,INST2;
A211    CALL	    NSPIWRT
.
A3      KEYIN      *EL,*P1:24,"ADD ANOTHER INSTRUCTION? ",STR1;
        CMATCH     "Y",STR1
        GOTO       A2 IF EQUAL
        CMATCH     "N",STR1
        GOTO       A1 IF EQUAL
        GOTO       A3
.
. ******* REVIEW A RECORD ROUTINE *******
.
A5      KEYIN      *P1:4,*EF,"ENTER SPEC INST NUMBER: ",*ZF,*JR,NSPIFLD;
        CMATCH     " ",NSPIFLD
        GOTO       A1 IF EOS
        MOVE       "1",SWITCH
	 MOVE	   NSPIFLD TO SPINO
	 CALL      NSPIKEY
        GOTO       C1 IF OVER
        DISPLAY    *P1:7,*EF,"SPECIAL INSTRUCTION LINE #1:     ",INST1;
        DISPLAY    *P1:9,"                    LINE #2:     ",INST2;
.
A6      KEYIN      *P1:24,*EL,"REVIEW ANOTHER SPEC INST? ",STR1;
        CMATCH     "Y",STR1
        GOTO       A5 IF EQUAL
        CMATCH     "N",STR1
        GOTO       A1 IF EQUAL
        GOTO       A6
.
. ******* DELETE A RECORD ROUTINE *******
A7     DISPLAY       *P1:4,*EF,"DELETION IS NOT PERMITTED AT THIS TIME.";
       GOTO          A1
.
. ******* UPDATE A RECORD ROUTINE *******
.
A9      KEYIN      *P1:4,*EF,"ENTER SPEC INST NUMBER: ",*ZF,*JR,NSPIFLD;
        CMATCH     " ",NSPIFLD
        GOTO       A1 IF EOS
        MOVE       "3",SWITCH
	 MOVE       NSPIFLD TO SPINO
	 CALL	    NSPIKEY
        GOTO       C1 IF OVER
        DISPLAY    *P1:7,*EF,"SPECIAL INSTRUCTION LINE #1:    ",INST1:
                     *P1:9,"                    LINE #2:    ",INST2;
        GOTO       A95
.
.
A95     KEYIN      *P1:24,*EL,"IS THIS THE SPEC INST RECORD THAT YOU WISH":
                   " TO UPDATE? ",STR1;
        CMATCH     "Y",STR1
        GOTO       A122 IF EQUAL
        CMATCH     "N",STR1
        GOTO       A9 IF EQUAL
        GOTO       A95
.
A122    DISPLAY     *P1:24,*EL,"ENTER LINE NUMBER OF ITEM TO BE UPDATED: ":
                      "                 ENTER 99 TO END UPDATE";
        KEYIN     *P42:24,*ZF,*JR,LIN;
        COMPARE    "99",LIN
        GOTO       A13 IF EQUAL
        BRANCH     LIN OF A121,A123
.
A121    KEYIN      *P32:7,*rv,*edit,INST1;
        DISPLAY    *P32:7,INST1;
        GOTO       A122
A123    KEYIN      *P32:9,*rv,*edit,INST2;
        DISPLAY    *P32:9,INST2;
        GOTO       A122
.
A13     CALL	   NSPIUPD
.
A14     KEYIN      *P1:24,*EL,"ANY MORE UPDATES? ",STR1;
        CMATCH     "Y",STR1
        GOTO       A9 IF EQUAL
        CMATCH     "N",STR1
        GOTO       A1 IF EQUAL
        GOTO       A14
.
C1       BEEP
   DISPLAY *P1:12,*EL,*P15:12,"****** NO SUCH INSTRUCTION IN THE FILE ******":
                   *W,*W;
        BRANCH     SWITCH OF A2,A5,A7,A9
. ******** TERMINATE THE PROGRAM *******
.
C2      STOP
.
C3       BEEP
  DISPLAY *P1:12,*EL,*P15:12,"****** INSTRUCTION NUMBER ALREADY IN USE *****":
         *W,*W;
        GOTO       A2
	 INCLUDE    NSPIIO.inc
	 INCLUDE    COMLOGIC.inc


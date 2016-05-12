*NXRF0001 - ADD LISTMLR RECORDS.
*WRITTEN    19JUN92
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NXRFDD.INC
MODE     FORM      1
holdlist dim       6
BRANCH	 FORM	   1
.
RELEASE  INIT      "1.2"            08Mar2001 DLH Delete function
.RELEASE  INIT      "1.1"            25jul96 if over put list# back.
.RELEASE  INIT      "1.00"
.
	 MATCH     "NDAT0001" TO PROGRAM     *CHAINED FROM NINV0001 ?
	 IF	    EQUAL                        *YES
	 MOVE       C1 TO BRANCH
         goto       main
         endif
         move      c0 to branch
main     MOVE      "NAMES IN THE NEWS CA INC" TO COMPNME
         MOVE      "NXRF0001" TO PROGRAM
         MOVE      "LIST/MLR X-REF MAINT" TO STITLE
         MOVE      "EXIT" TO PF5
         TRAP      STOP IF F5
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C1 TO NXRFPATH
.
KEY1     MOVE      C1 TO MODE
         KEYIN     *P10:12,*EL,"LIST ##: ",*ZF,*JR,NXRFLIST
         SCAN      STAR IN NXRFLIST
         GOTO      STOP IF EQUAL
         GOTO      KEY1 IF EOS
         MOVE      NXRFLIST TO NXRFFLD
         move      nxrflist to holdlist
         CALL      NXRFkey
         GOTO      ALREADY IF NOT OVER
KEY2     KEYIN     *P10:14,"MAILER ##: ",*ZF,*JR,NXRFMLR
         move      holdlist to nxrflist
         MOVE      NXRFMLR TO NXRFFLD2
         CALL      NXRFWRT
         GOTO      KEY1
ALREADY  DISPLAY   *P10:14,"MAILER ##: ",NXRFMLR
	 DISPLAY   *P1:24,*EL,"ALL READY ON FILE ",*B,*B,*W2,*P1:24,*EL;
         KEYIN     *P1:24,"UPDATE ? ",STR1;
         CMATCH    YES TO STR1
         GOTO      UPD IF EQUAL
.begin patch 1.2
         CMATCH    "D" TO STR1
         GOTO      DEL IF EQUAL
.end patch 1.2
         GOTO      KEY1
UPD      MOVE      C2 TO MODE
         MOVE      NXRFfld TO NXRFlist
         CALL      NXRFDEL
         GOTO      KEY2
.begin patch 1.2
DEl      MOVE      C2 TO MODE
         MOVE      NXRFfld TO NXRFlist
         CALL      NXRFDEL
         display   *p1:24,*el,"record deleted",*b
         goto      key1
.end patch 1.2
STOP
         SHUTDOWN  "CLS"
         STOP
EOJX     
         stop
         INCLUDE   NXRFIO.INC
         INCLUDE   COMLOGIC.INC


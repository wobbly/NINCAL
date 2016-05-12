. 'SCMFIX' CHECKS THE FIRST BYTE OF THE ORDER AND INVOICE FILE FROM THE KEYED
. IN LR NUMBER. IT THEN UPDATES THE FIRST BYTE TO 'S' IN THE CASE OF AN ORDER
. AND 'F' IN THE CASE OF AN INVOICE.
. 06/22/83 ALSO CHECKS SECOND BYTE OF ADJUSTMENT FILE AND UPDATES TO 'J'.
PC	 EQU	   0
         INCLUDE   COMMON.inc
	 INCLUDE   CONS.inc
	 INCLUDE   NORDDD.inc
; 	 INCLUDE   NINVDD.inc
 	 INCLUDE   ninvdd.inc
	 INCLUDE   NADJDD.inc
.
RELEASE  INIT      "2.1"        DLH 14MAR2005    INvoice Conversion
;RELEASE  INIT      "2.0"        DLH 18MAR92    INCLUDES. ET AL.
.
CODENG   DIM       1         INVALID LR CODE
COM      DIM       1         C,N
.
	 MOVE	   "NORD0021" TO PROGRAM
	 MOVE 	   "NINCAL" TO COMPNME
	 MOVE	   "SUPER FIX BUSY" TO STITLE
	 MOVE      C1 TO NORDPATH	
 	 MOVE	   C1 TO NINVPATH
	 CALL      PAINT
.
KEYCOMP KEYIN  *P15:5,"(N)ames In The News or (*)Finished: ",COM
         CMATCH    "*",COM
         STOP      IF EQUAL
         GOTO      KEYCOMP IF EOS
         CMATCH    "N",COM
         GOTO      KEYCOMP IF NOT EQUAL
         GOTO      ENTERLR
ENTERLR  MOVE      " ",CODENG
         KEYIN     *EF,*P10:8,"ENTER LR NUMBER ('<' FOR NEW COMPANY OR '*' TO":
                   " STOP): ",*EF,*JR,*T60,*ZF,NORDFLD;
         MATCH     "00000<",NORDFLD
         GOTO      KEYCOMP IF EQUAL
         GOTO      ENTERLR IF EOS
         MATCH     "00000*",NORDFLD
         GOTO      STOP IF EQUAL
         TYPE      NORDFLD
         GOTO      ENTERLR IF NOT EQUAL
.         READ      ORDER,KEY;CODEO,STATO,MLRO,LRO
	 CALL      NORDKEY
         GOTO      NULLKEYO IF OVER
.         UPDATAB   ORDER;*1,"S"
         DISPLAY   *P17:12,"CURRENT CODE IN ORDER FILE IS '",ORCODE:
		   "' NEW CODE IS 'S'";
	 MOVE      "S" TO ORCODE
	 CALL	   NORDUPD
	 MOVE      NORDFLD TO NINVFLD
INV	 CALL      NINVKEY
.INV      READ      INVOICE,KEY;CODEI,STATI,MLRI,LRI
         GOTO      NULLKEYI IF OVER
.         UPDATAB   INVOICE;*1,"F"
         DISPLAY   *P17:15,"CURRENT CODE IN INVOICE FILE IS '",CODE:
                   "' NEW CODE IS 'F'";
	 MOVE	   "F" TO CODE
	 CALL      NINVUPD
ADJUST	 MOVE      NORDFLD TO NADJFLD
	 CALL      NADJKEY
.         READ      ADJUST,KEY;CODEA,STATA,MLRA,LRA
         GOTO      NULLKEYA IF OVER
.         UPDATAB   ADJUST;*1,"0"
         DISPLAY   *P17:17,"CURRENT CODE IN ADJUSTMENT FILE IS '",ASCODE:
                   "' NEW CODE IS 'J'";
	 MOVE       "J" TO ASCODE
	 CALL	    NADJUPD
CHECK    KEYIN     *P25:24,"DO YOU WANT TO CONTINUE? ",*EL,STR1;
         CMATCH    "Y",STR1
         GOTO      CHECK IF EOS
         GOTO      ENTERLR IF EQUAL
         CMATCH    "N",STR1
         GOTO      CHECK IF NOT EQUAL
         GOTO      STOP
NULLKEYO DISPLAY   *P26:12,*B,"THIS ORDER IS NOT ON FILE",*W2;
         MOVE      "O",CODENG
         GOTO      INV
NULLKEYI DISPLAY   *P26:15,*B,"THIS INVOICE IS NOT ON FILE",*W2;
         CMATCH    "O",CODENG
         GOTO      NULLKYIA IF EQUAL
         MOVE      "O",CODENG
         GOTO      ADJUST
NULLKYIA MOVE      "P",CODENG
         GOTO      ADJUST
NULLKEYA DISPLAY   *P26:17,*B,"THIS ADJUSTMENT IS NOT ON FILE",*W2;
         CMATCH    "O",CODENG
         GOTO      CHECK IF EQUAL
         CMATCH    "P",CODENG
         GOTO      NOMATCH IF EQUAL
         GOTO      CHECK
NOMATCH  DISPLAY   *P1:24,*B,".........INVALID LR !!",*W;
         GOTO      ENTERLR
STOP	 STOP
;	 INCLUDE   NINVIO.inc
	 INCLUDE   NORDIO.inc
	 INCLUDE   ninvio.inc
	 INCLUDE   NADJIO.inc	
	 INCLUDE   COMLOGIC.inc
	

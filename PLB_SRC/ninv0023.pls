PC	 EQU	   0
         INCLUDE   COMMON.inc
	 INCLUDE   CONS.inc
         include   consacct.inc
;begin patch 1.1
;       	 INCLUDE   NINVDD.inc
       	 INCLUDE              ninvdd.inc
;end patch 1.1

RELEASE  INIT      "1.1"       9Mar05   Invoice Conversion
;RELEASE  INIT      "1.0"       11Mar04   created for adding loinvn only.
SAVELR	 DIM 	   6	
ITEM     FORM      2
	 MOVE	   "NINV0023" TO PROGRAM
	 MOVE      "Names In The News Ca Inc" TO COMPNME
	 MOVE	   "ADD L/O INV # DATA" TO STITLE
	 CALL 	   PAINT
	 MOVE      C1 TO NINVPATH        SET ACCESS TO ISI BY LR#.
.
START    CALL      WIPEVAR
	 KEYIN     *P1:3,*EF,"WHAT LR? ",*ZF,*JR,NINVFLD
	 SCAN     "N" IN NINVFLD
	 GOTO     START1 IF EQUAL
	 MOVE     C1 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
	 CALL	   NINVKEY
         GOTO      START IF OVER
	 GOTO	   START3
START1 	 CALL      WIPEVAR
 	 KEYIN     *P1:3,*EF,"WHAT INV?",*ZF,*JR,NINVFLD
	 MOVE      C2 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
	 CALL	   NINVKEY
         GOTO      START IF OVER
START3   DISPLAY   *P1:5,"(1) INVOICE NUMBER: ",INVNUM:
                   *P1:6,"(2) 1ST CHECK NUMBER: ",CHKN1:
                   *P1:7,"(3) 2ND CHECK NUMBER: ",CHKN2:
                   *p1:8,"(12)mlrs check number:",imlrchk:
                   *p1:9,"(17) Quantity:",qtybild:
;                   *p35:10,"(18) Mlr Number        ",mlrn:
                   *p1:10,"(18) L/O INV#        ",LOINVN:
                   *P1:11,"(4) CHECK DATE: ",CHK1DTEM,CHK1DTED,chk1dtec,CHK1DTEY:
                   *P1:12,"(5) INVOICE DATE: ",INVDTEM,INVDTED,invdtec,INVDTEY:
                   *P1:13,"(6) RCV CHK DATE: ",MLRPAYD:
                   *P1:14,"(7) A/R AMOUNT: ",AR:
                   *P1:15,"(8) A/P 1     : ",AP1:
                   *P1:16,"(9) A/P 2     : ",AP2:
	           *P1:17,"(10) LR NUMBER: ",LRN:
	           *p1:18,"(11) broker/cnt:",ibrknum,slash,ibrkcnt:
	           *p1:19,"(13) split/qty:",irexqty,"      Guar Letter": ",wsjpc:
	           *p1:20,"(14) split/$ppm:",iexppm:
	           *p1:21,"(15) net name %:   ",irnetper:
	           *p1:22,"(16) net run charge: ",inetrc
         KEYIN     *P1:24,*EL,"IS THIS THE INVOICE YOU WANT? ",STR1;
         CMATCH    YES,STR1
         GOTO      START IF NOT EQUAL
GETITEM  KEYIN     *P1:24,*EL,"WHAT ITEM NUMBER DO YOU WANT TO CHANGE? ",ITEM;
         COMPARE   "99",ITEM
         GOTO      UPDATE IF EQUAL
         BRANCH    ITEM OF T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,t11,t12:
                   t13,t14,t15,t16,t17,t18,t19
         GOTO      GETITEM
T1
          goto      getitem
T2
          goto      getitem
T3
          goto      getitem
T4
          goto      getitem
T5
          goto      getitem
T6
          goto      getitem
T7
          goto      getitem
T8
          goto      getitem
T9
         GOTO      GETITEM
T10
	 GOTO      GETITEM
T11
         GOTO      GETITEM
T12
         GOTO      GETITEM
T13
         GOTO      GETITEM
T14
         GOTO      GETITEM
T15
         GOTO      GETITEM
T16
         GOTO      GETITEM
T17
         GOTO      GETITEM
T18
      KEYIN     *P 2:10,*jr,LOINVN
         GOTO      GETITEM
T19
         GOTO      GETITEM
UPDATE   CALL      NINVUPD
         GOTO      START
EOJ      STOP
WIPEVAR  CLEAR          CODE
         CLEAR		STATB
	 CLEAR		MLRN
	 CLEAR		LRN
	 CLEAR          SAVELR
	 RETURN
BUGGED   DISPLAY    *P1:24,*EL,*HON,*B,"B U G G E D !!!!!"
	 STOP
;begin patch 1.1
;	 INCLUDE    NINVIO.inc
	 INCLUDE      ninvio.inc
;end patch 1.1
	 INCLUDE   COMLOGIC.inc


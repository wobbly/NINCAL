.H.PROGRAM THAT ALLOWS CHANGING OF INVOICE FIELDS THAT ARE NORMALLY RESTRICTED.
PC         EQU         0
               INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
               include   consacct.inc
;begin patch 2.4
;                   INCLUDE   NINVDD.inc
                    INCLUDE        ninvdd.inc
;end patch 2.4
release	init      "2.51"      DLH update print file if record is present
Reldate	Init	"20140812"
.release        init           "2.5"           20October2008  DLH Chkn3 keying
.release        init           "2.4"           08March2005  DLH Invoice Conversion
;release  init      "2.3"           22Apr99 DLH see ninv0001 release 9.0
.RELEASE  INIT      "2.2"           29jan97 jd added keyin mlr #.
.RELEASE  INIT      "2.1"          13oct95 jd added keyin for net info.
.RELEASE  INIT      "2.0"         20MAR92   DLH
SAVELR     DIM         6      
ITEM     FORM      2
           MOVE        "NINV0015" TO PROGRAM
           MOVE      "Names In The News Ca Inc" TO COMPNME
           MOVE        "FIX BAD INVOICE DATA" TO STITLE
           CALL        PAINT
           MOVE      C1 TO NINVPATH        SET ACCESS TO ISI BY LR#.
.
START    CALL      WIPEVAR
           KEYIN     *P1:3,*EF,"WHAT LR? ",*ZF,*JR,NINVFLD
           SCAN     "N" IN NINVFLD
           GOTO     START1 IF EQUAL
           MOVE     C1 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
           CALL        NINVKEY
         GOTO      START IF OVER
           GOTO        START3
START1     CALL      WIPEVAR
           KEYIN     *P1:3,*EF,"WHAT INV?",*ZF,*JR,NINVFLD
           MOVE      C2 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
           CALL        NINVKEY
         GOTO      START IF OVER
START3   DISPLAY   *P1:5,"(1) INVOICE NUMBER: ",INVNUM,"      Status (O)pen or (P)aid:",statb:
                   *P1:6,"(2) 1ST CHECK NUMBER: ",CHKN1:
                   *P1:7,"(3) 2ND CHECK NUMBER: ",CHKN2:
                   *P30:7,"(22) 3RD CHECK NUMBER: ",CHKN3:
                   *p1:8,"(12)mlrs check number:",imlrchk:
                   *p1:9,"(17) Quantity:",qtybild:
                   *p35:10,"(18) Mlr Number        ",mlrn:
                   *p1:10,"(19) Quantity in        ",QTYIN:
                   *P1:11,"(4) CHECK DATE: ",CHK1DTEM,CHK1DTED,chk1dtec,CHK1DTEY:
                   *P1:12,"(5) INVOICE DATE: ",INVDTEM,INVDTED,invdtec,INVDTEY:
                   *P1:13,"(6) RCV CHK DATE: ",MLRPAYD:
                   *P1:14,"(7) A/R AMOUNT: ",AR:
                   *P30:14,"(8) A/P 1     : ",AP1:
                   *P1:15,"(9) A/P 2     : ",AP2:
                   *P30:15,"(20) A/P 3     : ",AP3:
                   *P1:16,"(21) *LR     : ",Xninc:
                     *P1:17,"(10) LR NUMBER: ",LRN:
                     *p1:18,"(11) broker/cnt:",ibrknum,slash,ibrkcnt:
                     *p1:19,"(13) split/qty:",irexqty,"      Guar Letter": ",wsjpc:
                     *p1:20,"(14) split/$ppm:",iexppm:
                     *p1:21,"(15) net name %:   ",irnetper:
                     *p1:22,"(16) net run charge: ",inetrc:
                     *p1:23,"(23) MLRpay:   ",MlrpayR:
                     *p45:23,"(24) Credit Letter code: ",LET90D
         KEYIN     *P1:24,*EL,"IS THIS THE INVOICE YOU WANT? ",STR1;            ."
         CMATCH    YES,STR1
         GOTO      START IF NOT EQUAL
GETITEM  KEYIN     *P1:24,*EL,"WHAT ITEM NUMBER DO YOU WANT TO CHANGE? ",ITEM;
         COMPARE   "99",ITEM
         GOTO      UPDATE IF EQUAL
         BRANCH    ITEM OF T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,t11,t12:
                   t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24
         GOTO      GETITEM
T1
.          goto      getitem
          move      invnum to ninvfld
.         move      c2 to ninvpath
.         call      ninvkey
.         call      ninvdel
         KEYIN     *P21:5,*zf,*jr,INVNUM
         move      lrn to ninvfld
         move      c1 to ninvpath
         call      ninvupd
                DISPLAY   *P1:23,*EL,*B,"KEY ",*HON," CHANGED",*hoff,*B
         GOTO      GETITEM
T2       KEYIN     *P23:6,CHKN1
         GOTO      GETITEM
T3       KEYIN     *P23:7,CHKN2
         GOTO      GETITEM
T4       KEYIN     *p35:11,"format mmddccyy":
                       *P17:11,*-,*ZF,*JR,CHK1DTEM,CHK1DTED,chk1dtec,CHK1DTEY,*+
         GOTO      GETITEM
T5       KEYIN     *p35:12,"format mmddccyy":
                       *P19:12,*-,*ZF,*JR,INVDTEM,INVDTED,invdtec,INVDTEY,*+
         GOTO      GETITEM
T6       KEYIN     *P19:13,MLRPAYD
         GOTO      GETITEM
T7       KEYIN     *P17:14,AR
         GOTO      GETITEM
T8       KEYIN     *P47:14,AP1
         GOTO      GETITEM
T9       KEYIN     *P17:15,AP2
         GOTO      GETITEM
T20       KEYIN     *P47:15,AP3
         GOTO      GETITEM
T21       KEYIN     *P17:16,Xninc
         GOTO      GETITEM
.start patch 2.5
T22       KEYIN     *P30:7,chkn3
         GOTO      GETITEM
T23       KEYIN     *P30:23,Mlrpayr
         GOTO      GETITEM
T24       KEYIN     *P61:23,Let90D
         GOTO      GETITEM
         
         
.end patch 2.5         
T10        MOVE        LRN TO SAVELR
           REP         ZFILL IN SAVELR
           KEYIN     *P17:17,*ZF,*JR,LRN,*DV,B1,*DV,SAVELR
           MOVE      SAVELR TO NINVFLD
           MOVE     C1 TO NINVPATH
           CMATCH   B1 TO SAVELR
           GOTO     T10A IF EOS       .OLD LR WAS NULL
           MATCH    "000000" TO SAVELR
           GOTO     T10A IF EQUAL
           CALL     NINVTST
           FILEPI    1;NINVFILE
           DELETEK   NINVFILE,SAVELR
T10A       MOVE      C2 TO NINVPATH
           MOVE      INVNUM TO NINVFLD
           REP         ZFILL IN NINVFLD
           CALL      NINVTST
           GOTO      BUGGED IF OVER
           MOVE      C1 TO NINVPATH
           REP         ZFILL IN LRN
           DISPLAY   *P1:24,*EL,"NEW LR EQUALS ",LRN,*W5
           BRANCH    NINVFLAG TO T10B
           CALL      NINVOPEN
T10B       FILEPI    1;NINVFILE
           INSERT    NINVFILE,LRN
           MOVE      LRN TO NINVFLD
           CALL      NINVTST
           IF          NOT OVER
           DISPLAY   *P1:23,*EL,*B,"KEY CHANGED",*B
           ELSE
           DISPLAY   *P1:23,*EL,*B,"KEY ",*HON,"NOT",*HOFF," CHANGED",*B
           ENDIF
           GOTO      GETITEM
T11      KEYIN     *P17:18,*jr,*zf,ibrknum,*dv,slash,*jr,*zf,Ibrkcnt
         GOTO      GETITEM
T12      KEYIN     *P23:18,imlrchk
         GOTO      GETITEM
T13      KEYIN     *P23:19,*jr,irexqty
         GOTO      GETITEM
T14      KEYIN     *P23:20,*jr,iexppm
         GOTO      GETITEM
T15      KEYIN     *P23:21,*jr,irnetper
         GOTO      GETITEM
T16      KEYIN     *P23:22,*jr,*zf,inetrc
         GOTO      GETITEM
T17      KEYIN     *P23:09,*jr,qtybild
         GOTO      GETITEM
T18      KEYIN     *P 2:10,*jr,*zf,mlrn
         GOTO      GETITEM
T19      KEYIN     *P25:10,*jr,qtyin
         GOTO      GETITEM
UPDATE   CALL      NINVUPD
.add print files goodies here
	call	PINVKEY				.in print file?
	goto	start if over			.nope
	call	Ninvkey				.reread the live record to get changes
	call	Pinvupd				.Update the print record
.end of uddate print files goodies here
	
         GOTO      START
EOJ      STOP
WIPEVAR  CLEAR          CODE
         CLEAR                STATB
           CLEAR              MLRN
           CLEAR              LRN
           CLEAR          SAVELR
           RETURN
BUGGED   DISPLAY    *P1:24,*EL,*HON,*B,"B U G G E D !!!!!"
           STOP
;begin patch 2.4
;          INCLUDE    NINVIO.inc
           INCLUDE      ninvio.inc
;end patch 2.4
           INCLUDE   COMLOGIC.inc


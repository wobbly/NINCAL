PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NJSTDD.inc
RELEASE  INIT      "2.1"        JD  15SEP99    inv/adj Y2K.
;RELEASE  INIT      "2.0"       DLH 19MAR92    CONS, COMLOGIC, NJSTDD
ADJPRT   FILE      FIXED=180
READCNT  FORM      4
;
         MOVE      "NADJ0003" TO PROGRAM
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "ADJUSTMENT PRINT PREP" TO STITLE
         MOVE      "NINPADJ" TO NJSTNAME
;         OPEN      NINPADJ,"NINPADJ",SHARE
         OPEN      ADJPRT,"NINPADJ2",EXCLUSIVE
         CALL      PAINT
;begin patch 2.2
         call      njstopen
; 
READ
;     CALL      NJSTKS
         read     njstfile,seq;jstvars,typinit
;end patch 2.2

; 
         GOTO      EOJ IF OVER
         ADD       C1 TO READCNT
         DISPLAY   *P10:12,"NUMBER OF ADJUSTMENTS READ : ",READCNT
;.............................................................................
;
;
         FILEPI    1;ADJPRT
;begin patch 2.2
;         WRITE     ADJPRT,SEQ;JSTvars
         WRITE     ADJPRT,SEQ;JSTvars,typinit
;end patch 2.2
;                                     001-001   BUSY BYTE
;                                JSTSTAT:      002-002   STATUS

;                                JSTMLR:       003-006   MAILER CODE
;.                                JSTLR:        007-012   LR#
;                                JSTBILTO:     013-013   BILL-TO CODE
;                                JSTPAYTO:     014-014   PAY-TO CODE
;                                JSTAR:        015-021   A/R  ADJUSTMENT
;                                JSTAP1:       022-028   A/P1 ADJUSTMENT
;                                JSTAP2:       029-035   A/P2 ADJUSTMENT
;                                JSTFIL1:      036-036
;                                JSTLRINC:     037-042   LR INCOME ADJUSTMENT
;                                JSTREUSE:     043-043   RE-USE/RUN CHARGE CODE
;                                JSTCD:        044-044   CREDIT/DEBIT CODE
;                                JSTCRCT:      045-045   CORRECT/ADDITIONAL BILL
;                                JSTSTAX:      046-050   STATE TAX ADJUSTMENT
;                                JSTPOST:      051-054   POSTAGE ADJUSTMENT
;                                JSTCTAX:      055-059   CITY ADJUSTMENT
;                                JSTREASN:     060-061   REASON CODE
;                                JSTCNT:       062-064   CONTACT CODE
;                                JSTINVNO:     065-070   INVOICE NUMBER
;                                JSTDATE:      071-076   ADJUSTMENT DATE
;.                                JSTINVDT:     077-082   INVOICE DATE
;                                JSTSUBNO:     083-083   ADJUSTMENT AMENDMENT NU
; .                               JSTISTAT:     084-084   INVOICE STATUS
;                                JSTFIL6:       085-090
;                                jstqty:
; .                               jstqrsn
;...............................................................................
         PACK      NJSTFLD FROM JSTINVNO,JSTSUBNO
         REP       " 0" IN NJSTFLD
;         FILEPI    1;NINPADJ
;         DELETE    NINPADJ,KEY
         CALL      NJSTDEL
         GOTO      READ
EOJ      WEOF      ADJPRT,SEQ
	 IFNZ	   PC
         FLUSH     ADJPRT
	 XIF
         CLOSE     ADJPRT,EOFSIZE
         STOP
         INCLUDE   NJSTIO.inc
         INCLUDE    COMLOGIC.inc


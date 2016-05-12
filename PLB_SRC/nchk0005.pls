...............................................................................
.CHECKINFO - CREATED AUG 88
.
.PURPOSE : ALLOW UPDATING OF INVOICE CHECK & CHECK DATE FIELDS IN THE EVENT OF
.          MANUAL: VOIDS, CONTRAS, CHECK REISSUES.
...............................................................................
.8=8=8=8=8=8=8=8 NEED TO CALL * CLEAR ALL VARS BEFORE EACH READ.******
.*******************************************************************
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NPASDD.inc
;begin patch 1.5         
 ;        INCLUDE   NINVDD.inc
         
         	INCLUDE   	ninvdd.inc
;end patch 1.5         
	 INCLUDE   NCKIDD.inc
RELEASE  	INIT      	"1.51"      13Nov2007 JD Allow for PLI orders
;RELEASE  	INIT      	"1.5"       10March2005 dlh NININV Conversion
;RELEASE  INIT      "1.4"       26aPR99 dlh NININV Y2K
.RELEASE  INIT      "1.3"        added option to update chk#2 manual open inv.
.RELEASE  INIT      "1.2"       DLH 26MAR92    CONS, NPASXX, NINVXX, COMLOGIC,
.                               ETC.
.
.
AKEY     INIT      "01R"
AKEY1    INIT      "02R"
AKEY2    INIT      "03R"
LR       DIM       6
.
...............................................................................
DATE     DIM       8
.
.
MODE     FORM      1
INQ      FORM      1
NUM      FORM      1
.ANS      DIM       1
.
         CLOCK     DATE TO DATE
.begin patch 1.4
         clock     timestamp to timestamp
         unpack    timestamp into cc           .get century
.end patch 1.4
         MOVE      "NCHK0005" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "CHECK MAINTENANCE" TO STITLE
         MOVE      C1 TO NINVPATH
	 MOVE      C2 TO NCKIPATH
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK   DATE INTO MM,STR1,DD,STR1,YY
         MOVE     DATE TO TODAY
         XIF
.begin patch 1.4
.         PACK     NCKIDATE FROM MM,DD,YY
         PACK     NCKIDATE FROM CC,YY,MM,DD
.end patch 1.4
         CALL      PAINT
         KEYIN    *P12:10,"Please Enter Password ",*EOFF,NPASKEY,*EON;
         APPEND    "a" TO NPASFLD
         APPEND    NPASKEY TO NPASFLD
         RESET     NPASFLD
         CALL      NPASKEY
         GOTO      GOOD IF NOT OVER
         DISPLAY   *P17:11,*B,"Password NOT VALID.....Only Inquiry Allowed";
         MOVE      C1 TO INQ
         STOP
GOOD
         MOVE      C0 TO INQ
.
         CALL      PAINT
MENU     KEYIN     *P33:06,*EF,"(S)top/void payment & reissue":
                   *P33:08,"Void - (C)ontra":
                   *P33:10,"(V)oid a computer check":
                   *P33:12,"(M)anual check on paid invoice":
                   *P33:14,"Manual check on (O)pen invoice":
                   *P33:16,"(I)quire":
                   *P33:18,"(E)xit":
                   *P33:20,*ESOFF,STR1
         BRANCH    INQ OF INQUIRE
         REP       "S1C2V3M4I5O6E7" IN STR1
         MOVE      STR1 TO MODE
         MOVE      STR1 TO NCKISTAT
         BRANCH    MODE OF VOIDRE,VOIDCONT,VOID,MANUAL,INQUIRE:
                   MANOPEN,STOP
         GOTO      MENU
INQUIRE 
         KEYIN     *P33:06,*EF,"INQUIRE BY:   ":
                   *P33:08,"(I)nvoice number":
                   *P33:10,"(O)ld check number":
                   *P33:12,"(N)ew check number":
                   *P33:14,"(E)xit":
                   *P33:16,*ESOFF,STR1
         REP       "I1O2N3E4" IN STR1
         MOVE      STR1 TO MODE
         BRANCH    MODE OF INQINV,INQOLD,INQNEW,MENU
INQINV   KEYIN     *P1:08,*EF:
                   *P39:10,"  INVOICE : ",*JR,*ZF,NCKIINV
         SCAN      "*" IN NCKIINV
         GOTO      MENU IF EQUAL
         TYPE      NCKIINV
         GOTO      INQINV IF NOT EQUAL
         PACK      NCKIFLD FROM AKEY,NCKIINV
         CLEAR     NCKIFLD2
         CLEAR     NCKIFLD3
         CALL      INQHEAD
	 CALL      CLEARVAR
         CALL      NCKIAIM
	 GOTO       INQDONE IF OVER
         CALL      DISINFO
	 CALL      CLEARVAR
         CALL      NCKIKG
         CALL      DISINFO IF NOT over
         GOTO      INQDONE
INQOLD   KEYIN     *P1:08,*EF:
                   *P39:10,"  CHECK : ",*JR,*ZF,NCKIONUM
         SCAN      "*" IN NCKIONUM
         GOTO      MENU IF EQUAL
         TYPE      NCKIONUM
         GOTO      INQOLD IF NOT EQUAL
         PACK      NCKIFLD2 FROM AKEY1,NCKIONUM
         CLEAR     NCKIFLD
         CLEAR     NCKIFLD3
         CALL      INQHEAD
	 CALL      CLEARVAR
         CALL      NCKIAIM
	 GOTO      INQDONE IF OVER
         CALL      DISINFO
	 CALL      CLEARVAR
         CALL      NCKIKG
         CALL      DISINFO IF NOT over
         GOTO      INQDONE
INQNEW   KEYIN     *P1:08,*EF:
                   *P39:10,"  CHECK : ",*JR,*ZF,NCKINNUM
         SCAN      "*" IN NCKINNUM
         GOTO      MENU IF EQUAL
         TYPE      NCKINNUM
         GOTO      INQNEW IF NOT EQUAL
         PACK      NCKIFLD3 FROM AKEY2,NCKINNUM
         CLEAR     NCKIFLD2
         CLEAR     NCKIFLD
         CALL      INQHEAD
	 CALL      CLEARVAR
         CALL      NCKIAIM
	 GOTO      INQDONE IF OVER
         CALL      DISINFO
	 CALL      CLEARVAR
         CALL      NCKIKG
         CALL      DISINFO IF NOT over
         GOTO      INQDONE
INQHEAD  DISPLAY   *P1:1,*ES:
                   *P01:01,"INVOICE",*P10:01,"STATUS",*P20:01,"OLD CHECK":
                   *P30:01,"ORIG DATE",*P40:01,"NEW CHECK",*P50:01,"NEW DATE":
		   *P60:01,"A/P"
         DISPLAY   *SETSWTB 2:24
         RETURN
DISINFO
         DISPLAY   *P1:01,*EL,*P01:01,NCKIINV,*P20:01,NCKIONUM,*P40:01,NCKINNUM:
		   *P60:01,NCKIAP1;
	 CLEAR     MM
	 CLEAR     DD
	 CLEAR     YY
.begin patch 1.4
.         UNPACK    NCKIDTE INTO MM,DD,YY
.         DISPLAY   *P30:01,MM,"/",DD,"/",YY;
         UNPACK    NCKIDTE INTO CC,YY,MM,DD
         DISPLAY   *P30:01,MM,"/",DD,"/",CC,YY;
.end patch 1.4
	 CLEAR     MM
	 CLEAR     DD
	 CLEAR     YY
.begin patch 1.4
.         UNPACK    NCKIDATE INTO MM,DD,YY
.         DISPLAY   *P50:01,MM,"/",DD,"/",YY;
         UNPACK    NCKIDATE INTO CC,YY,MM,DD
         DISPLAY   *P50:01,MM,"/",DD,"/",YY;
.end patch 1.4
         BRANCH    NCKISTAT OF VR,VC,V,M,DISINFOX,M
         GOTO      DISINFOX
VR       DISPLAY   *P10:01,"VOID/RE"
         GOTO      DISINFOX
VC       DISPLAY   *P10:01,"VOID/CONTRA"
         GOTO      DISINFOX
V        DISPLAY   *P10:01,"VOID"
         GOTO      DISINFOX
M        DISPLAY   *P10:01,"MANUAL"
DISINFOX
         DISPLAY   *P1:1,*RD,*RD
         RETURN
INQDONE  KEYIN     *P1:24,*EL,"THAT'S ALL",STR1,*RESETSW
         DISPLAY   *P1:1,*ES
         GOTO      MENU
...............................................................................
VOID
         CALL      GETINV
         CALL      DISINV
         KEYIN     *P20:12,"CHECK ## BEING VOIDED   ",*JR,*ZF,NCKIONUM
.begin patch 1.4
.         UNPACK    NCKIDATE INTO MM,DD,YY
         UNPACK    NCKIDATE INTO CC,YY,MM,DD
.end patch 1.4
         MATCH     NCKIONUM TO CHKN1
         GOTO      CHK1 IF NOT EQUAL
         MOVE      "*VOID*" TO CHKN1
         GOTO      VOID1
CHK1     MATCH     NCKIONUM TO CHKN2
         GOTO      INVCHKN IF NOT EQUAL
         MOVE      "*VOID*" TO CHKN2
VOID1    GOTO      WRITE
...............................................................................
VOIDRE
         CALL      GETINV
         CALL      DISINV
         KEYIN     *P20:12,"CHECK ## BEING VOIDED   ",*JR,*ZF,NCKIONUM
VOIDRE1  KEYIN     *P20:14,"NEW CHECK NUMBER        ",*JR,*ZF,NCKINNUM
         KEYIN     *P20:16,"NEW CHECK DATE MM/DD/CCYY ",*JR,*ZF,MM,"/",*JR,*ZF:
                   DD,"/",*JR,*ZF,CC,*JR,*ZF,YY
         TYPE      MM
         GOTO      DTENG IF NOT EQUAL
         TYPE      DD 
         GOTO      DTENG IF NOT EQUAL
         TYPE      YY
         GOTO      DTENG IF NOT EQUAL
         if        (cc > 20 | cc < 19 )
         GOTO      DTENG
         endif
         MOVE      MM TO N2
         COMPARE   "13" TO N2
         GOTO      DTENG IF NOT LESS
         MOVE      DD TO N2
         COMPARE   "32" TO N2
         GOTO      DTENG IF NOT LESS
.begin patch 1.4
.         PACK      NCKIDTE FROM CHKDTEM,CHKDTED,CHKDTEY
         PACK      NCKIDTE FROM chk1dtec,chk1dtey,CHK1DTEM,CHK1DTEd
         MOVE      MM TO CHK1DTEM
         MOVE      DD TO CHK1DTED
         MOVE      YY TO CHK1DTEy
         MOVE      CC TO CHK1DTEc
.end patch 1.4
         MATCH     NCKIONUM TO CHKN1
         GOTO      CHK2 IF NOT EQUAL
         MOVE      NCKINNUM TO CHKN1
         GOTO      VOIDR1
CHK2     MATCH     NCKIONUM TO CHKN2
         GOTO      INVCHKN IF NOT EQUAL
         MOVE      NCKINNUM TO CHKN2
VOIDR1   GOTO      WRITE
...............................................................................
VOIDCONT
         CALL      GETINV
         CALL      DISINV
         KEYIN     *P20:12,"CHECK ## BEING VOIDED   ",*JR,*ZF,NCKIONUM
.begin patch 1.4
.         UNPACK    NCKIDATE INTO MM,DD,YY
         UNPACK    NCKIDATE INTO CC,YY,MM,DD
.end patch 1.4
         MATCH     NCKIONUM TO CHKN1
         GOTO      CHK3 IF NOT EQUAL
         MOVE      "CONTRA" TO CHKN1
         GOTO      VOIDC1
CHK3     MATCH     NCKIONUM TO CHKN2
         GOTO      INVCHKN IF NOT EQUAL
         MOVE      "CONTRA" TO CHKN2
VOIDC1   GOTO      WRITE
...............................................................................
MANUAL
         CALL      GETINV
         CALL      DISINV
         KEYIN     *P20:12,"CHECK ## BEING REPLACED ",*JR,*ZF,NCKIONUM
         KEYIN     *P20:14,"NEW CHECK NUMBER        ",*JR,*ZF,NCKINNUM
         KEYIN     *P20:16,"NEW CHECK DATE MM/DD/CCYY ",*JR,*ZF,MM,"/",*JR,*ZF:
                   DD,"/",*JR,*ZF,CC,*JR,*ZF,YY
         TYPE      MM
         GOTO      DTENG IF NOT EQUAL
         TYPE      DD 
         GOTO      DTENG IF NOT EQUAL
         TYPE      YY
         GOTO      DTENG IF NOT EQUAL
         if        (cc > 20 | cc < 19 )
         GOTO      DTENG
         endif
         MOVE      MM TO N2
         COMPARE   "13" TO N2
         GOTO      DTENG IF NOT LESS
         MOVE      DD TO N2
         COMPARE   "32" TO N2
         GOTO      DTENG IF NOT LESS
.begin patch 1.4
.         PACK      NCKIDTE FROM CHKDTEM,CHKDTED,CHKDTEY
         PACK      NCKIDTE FROM chk1dtec,chk1dtey,CHK1DTEM,CHK1DTEd
         MOVE      MM TO CHK1DTEM
         MOVE      DD TO CHK1DTED
         MOVE      CC TO CHK1DTEc
         MOVE      YY TO CHK1DTEY
.end patch 1.4
         MATCH     NCKIONUM TO CHKN1
         GOTO      CHK4 IF NOT EQUAL
         MOVE      NCKINNUM TO CHKN1
         GOTO      MANUAL1
CHK4     MATCH     NCKIONUM TO CHKN2
         GOTO      INVCHKN IF NOT EQUAL
         MOVE      NCKINNUM TO CHKN2
MANUAL1  GOTO      WRITE
...............................................................................
MANOPEN
         CALL      GETINV
         CALL      DISINV
         CLEAR     NCKIONUM
KEYCHK   KEYIN     *P20:14," HIT ENTER CHECK#1/ (2) CHECK#2  ",NUM
         GOTO      CHEK1 IF EOS
         BRANCH    NUM OF CHEK1,CHEK2
CHEK1    KEYIN     *P20:14,*EL,"CHECK NUMBER #1      ",*JR,*ZF,NCKINNUM
         KEYIN     *P20:16,"CHECK DATE MM/DD/CCYY ",*JR,*ZF,MM,"/",*JR,*ZF:
                   DD,"/",*JR,*ZF,CC,*JR,*ZF,YY
         move      c1 to NUM
         GOTO      VERDATE
CHEK2    KEYIN     *P20:14,*EL,"ARE YOU SURE YOU WANT TO UPDATE CHECK #2 ",str1
         GOTO      CHEK2 IF EOS
         CMATCH    YES TO str1
         GOTO      MANOPEN IF NOT EQUAL
         KEYIN     *P20:14,*EL,"CHECK NUMBER #2      ",*JR,*ZF,NCKINNUM
         KEYIN     *P20:16,"CHECK DATE MM/DD/CCYY ",*JR,*ZF,MM,"/",*JR,*ZF:
                   DD,"/",*JR,*ZF,CC,*JR,*ZF,YY
VERDATE  TYPE      MM
         GOTO      DTENG IF NOT EQUAL
         TYPE      DD 
         GOTO      DTENG IF NOT EQUAL
         if        (cc > 20 | cc < 19 )
         GOTO      DTENG
         endif
         TYPE      YY
         GOTO      DTENG IF NOT EQUAL
         MOVE      MM TO N2
         COMPARE   "13" TO N2
         GOTO      DTENG IF NOT LESS
         MOVE      DD TO N2
         COMPARE   "32" TO N2
         GOTO      DTENG IF NOT LESS
         CLEAR     NCKIDTE
.begin patch 1.4
         MOVE      MM TO CHK1DTEM
         MOVE      DD TO CHK1DTED
         MOVE      CC TO CHK1DTEc
         MOVE      YY TO CHK1DTEY
.end patch 1.4
         COMPARE   C1 TO NUM
         IF        EQUAL
         MOVE      NCKINNUM TO CHKN1
         ELSE   
         MOVE      NCKINNUM TO CHKN2
         ENDIF
         GOTO      WRITE
...............................................................................
NOINV
         DISPLAY   *P1:23,*EL,"NO SUCH INVOICE",*B,*W3,*B
         GOTO      MENU
...............................................................................
DISINV
         DISPLAY   *P10:10,*EF,"INVOICE ## ",NCKIINV,"  CHECK 1 ",CHKN1:
                   "  CHECK 2 ",CHKN2
         RETURN
...............................................................................
GETINV   
         KEYIN     *P1:10,*EF:
                   *P39:10," LR## OF INVOICE TO BE MARKED: ",*JR,*ZF,NINVFLD
         SCAN      "*" IN NINVFLD
         GOTO      MENU IF EQUAL
.start patch 1.51
.         TYPE      NINVFLD
.         GOTO      GETINV IF NOT EQUAL
.end patch 1.51
         REP       ZFILL IN NINVFLD
         CALL      NINVKEY
         IF        OVER
         DISPLAY   *P1:24,*EL,"NO SUCH INVOICE",*B,*W4;
         GOTO       NOINV
         ENDIF
         MOVE      INVNUM TO NCKIINV
         RETURN
..............................................................................
CLEARVAR CLEAR     NCKIINV
	 MOVE      C0 TO NCKISTAT
	 CLEAR     NCKIONUM
	 CLEAR     NCKIDTE
	 CLEAR     NCKINNUM
	 CLEAR     NCKIDATE
	 CLEAR     NCKIAP1
	 CLEAR     NCKIAP2
	 RETURN
...............................................................................
DTENG    DISPLAY   *P1:24,*EL,*B,*B,"INVALID DATE !!!!!",*W3,*P1:24,*EL;
         GOTO      MENU
...............................................................................
INVCHKN  DISPLAY   *P1:24,*EL,*B,*B,"INVALID CHECK NUMBER !!!!",*W3:
                   *P1:24,*EL;
         GOTO      MENU
...............................................................................
WRITE
.         FILEPI    3;CHKINFO
.         WRITE     CHKINFO;INVNO,STAT,CHKNUM,OLDATE,NCHKNUM,MM,DD,YY
	 CALL      NCKIWRT
         CALL      NINVUPD
         GOTO      MENU
...............................................................................
NOREC    DISPLAY   *P1:24,*EL,"NO RECORDS FOR THAT KEY!!!!!",*B,*W4:
                   *P1:24,*EL;
         GOTO      MENU
...............................................................................
STOP      STOP
;begin patch 1.5         
;         INCLUDE   NINVIO.inc
         INCLUDE   ninvio.inc
;end patch 1.5         
         INCLUDE   NPASIO.inc
	 INCLUDE   NCKIIO.INC
         INCLUDE   COMLOGIC.inc


PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NORDDD.inc
         inc       ninvdd.inc
			include   nadjdd.inc
         INCLUDE   NJSTDD.inc
         INCLUDE   NSPEDD.INC
.
RELEASE  INIT      "1.2"       ASH   13JUL99   NINSPE FILE CONVERSION
.RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
TDMCORD  FILE      
ADJPRT   FILE      FIXED=180
INFILE   FILE      VAR=7
TDMCOUT  IFILE     KEYLEN=6,VAR=288
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
writupd  FORM      4
writCNT  FORM      4
spec1    form      1
LR       DIM       6
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
.START PATCH 1.2 - ADDED VARS
DESC0L1  DIM       47       7-53      LINE 1 OF 00 INSTRUCTION
DESC0L2  DIM       47      54-100     LINE 2 OF 00 INSTRUCTION
DESC991  DIM       47     101-147     LINE 1 OF 99 INSTRUCTION
DESC992  DIM       47     148-194     LINE 2 OF 99 INSTRUCTION
DESC981  DIM       47     195-241     LINE 1 OF 98 INSTRUCTION
DESC982  DIM       47     242-288     LINE 2 OF 98 INSTRUCTION
.END PATCH 1.2 - ADDED VARS
.
.
         MOVE      "Nadj<TR></TR>0095" TO PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
         MOVE      "Fix broker #'s" TO STITLE
         move      c1 to nordpath
         move      c1 to ninvpath
         CALL      PAINT
         OPEN      ADJPRT,"c:\work\NINadj3",EXCLUSIVE
.         OPEN      TDMCORD,"e:\data\diskin3.brk"
.
READ
MainLoop
         read     ADJPRT,seq;adjvars
.         read      tdmcord,seq;ordvars
         goto      eoj if over
	move	aslrnum,NADJFLD
     	call	NADJWRT
     	goto     eoj
.         if        (Ostat <> "0" or OSTAT <> "B")
;         CMATCH    "p" TO OSTAT       Pending order ?
;         GOTO      read IF EQUAL     YES, skip.
;         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
;         GOTO      read IF EQUAL     YES, skip.
;         CMATCH    "l" TO OSTAT      lcr order ?
;         GOTO      read IF EQUAL     YES, skip.
;         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
;         GOTO      read IF EQUAL     YES, skip.
         ADD       ONE TO READCNT
         DISPLAY   *P10:20,"NUMBER OF ORDERS READ : ",READCNT
         PACK      NJSTFLD FROM JSTINVNO,JSTSUBNO
         REP       " 0" IN NJSTFLD
;         FILEPI    1;NINPADJ
;         DELETE    NINPADJ,KEY
         CALL      NJSTWRT
		move	"J",ASCODE
.Test with existing Invoice Number
		move	jstlr,NADJFLD
		move	"AdjustSave-NADJTST",Location
		pack	KeyLocation,"Key: ",NADJFLD
		call	NADJTST
		if over
			move	C0,ASTATUS
			move	Jstmlr,ASMLRNUM
			move	jstlr,ASLRNUM
			move	JSTAR,ASRECADJ
			move	JSTAP1,ASPAYAD1
			move	JSTAP2,ASPAYAD2
			move	JSTAP3,ASPAYAD3
			move	JSTLRINC,ASLRINC
			move	JSTNININC,ASNININC
			move	JSTREUSE,ASREUSE
			move	JSTCD,ASCRDB
			move	JSTCRCT,ASADD
			move	JSTSTAX,ASSTAX
			move	JSTPOST,ASPOST
			move	JSTCTAX,ASCITY
			move	JSTCNT,ASCNTCT
			move	INVNUM,ASINVNO
			move	JSTDATE,ASCRDTE
			pack	ASINDTE,jstinvdt
			move	"01",ASAMNUM
			move	c0,ASTAT
.
			move	Jstlr,NADJFLD
			rep	ZFILL,NADJFLD
			move	"AdjustSave-NADJWRT",Location
			pack	KeyLocation,"Key: ",NADJFLD
			call	NADJWRT
			endif
.        call       nordupd
         ADD       ONE TO writcnt
         DISPLAY   *P10:22,"NUMBER OF orders updated  : ",writcnt
         GOTO      READ
.
eoj         CLOSE     ADJPRT
         shutdown   "cls"
         INCLUDE   COMLOGIC.inc
         include   ninvio.inc
        include    nordio.inc
         INCLUDE   NJSTIO.INC
			include   nadjio.inc

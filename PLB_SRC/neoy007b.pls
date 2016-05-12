.  s. CARDCNT - NAMES IN THE NEWS CALIFORNIA VERSION 10/29/85
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INCLUDE   NDATDD.inc
INPUT    FILE
OUTPUT   FILE      UNCOMP

RELEASE  INIT      "2.12"       ASH 07APR05    COMMPER CONVERSION
.RELEASE  INIT      "2.11"       JD  09apr01    updated unc on open/prep.
.RELEASE  INIT      "2.1"       ASH 30DEC98    NINORD Y2K, File expansion
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.
AKEY1    INIT      "01R"
AKEY2    INIT      "02R"
ALKEY    DIM       9           LISTKEY
AMKEY    DIM       7
NAMES    FORM      10
QTY      FORM      5
PackData DataList
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
ZERO     FORM      "0"
ONE      FORM      "1"
EIGHT    FORM      "8"
.Start Patch #2.1 - increase var to handle OQTY increase
.OQTY1    FORM      7
OQTY1    FORM      9
.End Patch #2.1 - increase var to handle OQTY increase
.
.OUTPUT FILE.
..............
.STATUS   DIM       1           1-1
.LSTNUM   DIM       6           2-7
.BLNK3    DIM       3           8-10
.MLSTNAME DIM       55         11-65
.QTY      FORM      5          66-70
.NAMES    FORM      10         71-80
...............................................................................
.PRINT VARIABLES
PAGE     FORM      3
LINES    FORM      2
SIXTY2   FORM      "62"
ANS      DIM       1
EIGHTY8  INIT      "88"
EIGHTY9  INIT      "89"
NINETY   INIT      "90"
NINETY1  INIT      "91"
NINETY2  INIT      "92"
NINETY3  INIT      "93"
MOMATCH  FORM      "03"
ORDMO    FORM      2
PRNTBR   FORM      1
SIX      FORM      "6"
BLNK3    DIM       3
check    form      5
check2   form      5
orddate  form      5
today1   form      5
COUNT    FORM      5
USED     FORM      5
NOTUSED  FORM      5
.taskname dim      120
.
         MOVE      "NEOY0007" TO PROGRAM
         MOVE      "CARD COUNT E-O-Y" TO STITLE
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         move      c3 to ndatlock
         CLOCK     DATE TO today
         UNPACK    today INTO mm,str1,dd,str1,yy
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         KEYIN     *P12:12,"(P)rint or (C)alc 'default' ",*T60,ANS
         CMATCH    "P" TO ANS
         GOTO      OPEN IF EOS
.         GOTO      print1 IF EQUAL
.
. OPEN INPUT FILES
OPEN     TRAP      ABORT IF F5
.
.
. PREP OUTPUT FILE
.
         IFNZ      PC
         PREP      OUTPUT,"CARDUSE/CAL:PRINT"
         XIF
         IFZ       PC
         PREPARE   OUTPUT,"\\nins1\e\DATA\CARDUSE.CAL",exclusive
         XIF
         move      "000000" to ndatfld
         call       ndattst
.
. GET NEXT DATA CARD
.
A100
         CALL      NDATKS
         GOTO      Z900 IF OVER
.
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
.
         CMATCH     b1  TO status
         GOTO      A100 IF NOT EQUAL
			cmatch    b1 to ndatoff
			goto      a100 if not equal
						if (ELSTCDE = "C" )
							unpack	REVDATE,CC,YY,MM,DD
							call	CVTJUL
							sub	JULDAYS,TODAY1,howmany
							if (howmany >= 180)
;								insertitem PackData,999999,OLNUM
;								write	output,seq;b1,lstnum,ownnum,nlstcde,elstcde,commper,hotline,newdate,revdate,password,mlstname,universe
.START PATCH 2.12 ADDED LOGIC
								move	COMMPER,OLDCOMMPER
.END PATCH 2.12 ADDED LOGIC
								write	output,seq;datvars
								add	C1,USED
								display	*P12:18,"NUMBER OF LISTS WITH USAGE ",USED
							endif
							endif
							goto a100
ABORT
         DISPLAY   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
.
. CLOSE FILE AND EXIT
.
Z900
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
			INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc


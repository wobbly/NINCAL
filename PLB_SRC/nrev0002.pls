.*nrev0001.pls makes sure ever projected record has a revenue record, even if no revenue has bee realized yet
.
.....................................
pc       equ      0
         include  common.inc
         include  cons.inc
          include   compdd.inc
          include   cntdd.inc
	include	ndatdd.inc
Release   Init      "1.0"     DLH 
Reldate   Init      "2014 August 13"

INPUT    FILE     VAR=178
SOURCE  DIM     1            2-2
.id num         6            3-8
owner   dim     4
.CC
YEAR    DIM     2           11-12
MONTH   FORM    2            9-10
.dd
mohold  form    3
FILL1   DIM     1           13-13
AR      FORM    10.2         14-49
AP      FORM    10.2         50-60
LR      FORM    10.2         61-71
NINTOT  form    10.2
QTY     FORM    8           72-79
ADJAR   FORM    10.2         80-90
ADJAP   FORM    10.2         91-101
ADJLR   FORM    10.2        102-112
ADJNINTOT  form    10.2
unbilinc form      8.2     113-123
JUNK    DIM     2
...........................
           include nprjdd.inc
          Include   Nrevdd.inc


UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    10.2
MO       FORM    2
.
.
        MOVE     "NREV0002" TO PROGRAM
         MOVE    "UPDATE REVENUE FILE" TO STITLE
         MOVE    "Names In The News" TO COMPNME
         CALL     PAINT
        display   *p1:24,*el,"Projection file";

	Clock	Date to today
	unpack	Today into mm,str1,dd,str1,yy
	clear	NPRJFLD1
	clear	NPRJFLD2
	clear	NPRJFLD3
	clear	NPRJFLD4
	clear	NPRJFLD5
	pack	str4 from cc,yy
	rep	zfill in str4
	packkey	Nprjfld4 from "04X",str4
	
	call	nprjaim
	if	not over			.should not be
        	SUB      SEQ FROM INCOUNT
        	DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
	call	checkrev
	endif
	loop
	call	nprjkg
	until 	over
	SUB      SEQ FROM INCOUNT
          DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
	call	checkrev
	repeat	
	shutdown "cls"
.
Checkrev	
        PACK    nrevfld FROM PrjTYPE,PrjSRC,prjclient,prjyr
        call      Nrevtst         
        GOTO    WRITE IF OVER
	return	


.
WRITE   
	move	prjtype,TYPE
	move	prjsrc,SRC
	move	prjclient,CID
	move	prjyr,YR0
	if	(SRC = "B")
          packkey    compfld,cid
          call      compkey
          move      compcomp,Client
	elseif	(SRC = "M")
          packkey    ndatfld,cid
         call       ndatkey
         move      olstname to Client
	else
	clear	CLIENT
	endif
        MOVE    C0 TO JANLR
        MOVE    C0 TO FEBLR
        MOVE    C0 TO MARLR
        MOVE    C0 TO APRLR
        MOVE    C0 TO MAYLR
        MOVE    C0 TO JUNLR
        MOVE    C0 TO JULLR
        MOVE    C0 TO AUGLR
        MOVE    C0 TO SEPLR
        MOVE    C0 TO OCTLR
        MOVE    C0 TO NOVLR
        MOVE    C0 TO DECLR

        MOVE    C0 TO JANNIN
        MOVE    C0 TO FEBNIN
        MOVE    C0 TO MARNIN
        MOVE    C0 TO APRNIN
        MOVE    C0 TO MAYNIN
        MOVE    C0 TO JUNNIN
        MOVE    C0 TO JULNIN
        MOVE    C0 TO AUGNIN
        MOVE    C0 TO SEPNIN
        MOVE    C0 TO OCTNIN
        MOVE    C0 TO NOVNIN
        MOVE    C0 TO DECNIN

        MOVE    C0 TO JanAR
        MOVE    C0 TO FebAR
        MOVE    C0 TO MarAR
        MOVE    C0 TO AprAR
        MOVE    C0 TO MayAR
        MOVE    C0 TO JunAR
        MOVE    C0 TO JulAR
        MOVE    C0 TO AugAR
        MOVE    C0 TO SepAR
        MOVE    C0 TO OctAR
        MOVE    C0 TO NOVAR
        MOVE    C0 TO DecAR

        MOVE    C0 TO JanAP
        MOVE    C0 TO FebAP
        MOVE    C0 TO MarAP
        MOVE    C0 TO AprAP
        MOVE    C0 TO MayAP
        MOVE    C0 TO JunAP
        MOVE    C0 TO JulAP
        MOVE    C0 TO AugAP
        MOVE    C0 TO SepAp
        MOVE    C0 TO OctAP
        MOVE    C0 TO NovAP
        MOVE    C0 TO DecAP

        move    c0 to REVunbld

          move      C0,REvAR
          move      C0,REvAP
          Move      C0,RevAR
          Move      C0,RevAP

          call      Nrevwrt
.
         ADD     C1 TO  WRTCOUNT
         DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT


	return
	
          Include   Nrevio.inc
           include nprjio.inc
          include   compio.inc
          include   cntio.inc
	include	ndatio.inc
         INCLUDE  COMLOGIC.INC


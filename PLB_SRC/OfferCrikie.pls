pc	equ	0
	include	Common.inc
	include	cons.inc
	include	Nofrdd.inc
release	INit 	"Not"	
FIle	FIle	
HoldMLr	DIm	6	
holdofnum	form	3	
	
.	ofrvars  list
.	OFCODE   DIM       1         1-1     'R'
.	OFMLR    DIM       6         2-7     MAILER NUMBER      KEY.
.	OFNUM    DIM       3         8-10     OFFER NUMBER       KEY.
.	OFDESC   DIM       40        11-50    OFFER DESCRIPTION.
.	OFNAME   DIM       10       51-60    LAST UPDATED BY.
.	.OFDATE   DIM       6        59-64    DATE LAST UPDATED.
.	OFDATE   DIM       8        61-68    DATE LAST UPDATED.
	
	OPen	File,"c:\work\ninofr.bad",read

Looper
	read	File,seq;ofrvars
	goto	EOj if over
	if	(ofmlr <> Holdmlr)
	GOTO	Break
	endif
	move	ofnum,n3
	if	(n3 <> HOldofnum)
	GOTO	MIAoffer
	else	
	add	c1 to HOldofnum
	endif
	goto	looper
	
	
	
.break	New mailer is the first offer offer #1??? 
Break	
	IF	(OFMLR = "004209")
	CALL	DEBUG
	ENDIF
	move	ofnum,n3
	if	(n3 = c1)   we are good :)
	move	ofnum,Holdofnum
	add	c1 to HOldofnum
	move	ofmlr,holdmlr	
	goto	Looper
	else
.lets create one	
	MOVe	"P",Ofcode
	move	"001",ofnum
	MOVe	"20070921",OfDate
	Move	"PLi Conv",Ofname
	Clear	OFDESC
	pack	NOFRFLD from ofmlr,ofnum
	rep	zfill,ofnum
	rep	zfill,nofrfld
	call	Nofrtst1
		if	over
		call	Nofrwrt
		IF	(n3 = C2)     SWE WERE ONLY MISSING #1
		move	n3,holdofnum
		ADD	C1,HOLDOFNUM
		move	ofmlr,holdmlr	
		GOTO	LOOPER
		ELSE
		move	n3,Holdofnum
		add	c1 to HOldofnum
		move	ofmlr,holdmlr	
		GOTO	LOOPER
		endif
		else
		Goto	Looper
		endif
	endif
	return
.
MiaOffer
	MOVe	"20070921",OfDate
	Move	"PLi Conv",Ofname
	Clear	OFDESC
	pack	NOFRFLD from ofmlr,holdofnum
	move	holdofnum,ofnum
	rep	zfill,ofnum
	rep	zfill,nofrfld
	MOVe	"P",Ofcode
	call	Nofrtst1
	if	over
	call	Nofrwrt
.	IF	(N3 = HOLDOFNUM)
	add	c1 to HOldofnum
	GOTO	lOOPER
	else
	Goto	Looper
	stop
	endif
	return
EOJ
	Stop


	include	Nofrio.inc
	include	comlogic.inc
	
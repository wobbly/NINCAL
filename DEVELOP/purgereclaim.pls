.....................................
.NLCR0050
.
.LCR PURGE PROGRAM - USED FOR LCRS WITH MAIL DATES OVER 6 MONTHS OLD
.
.Running this program will update the following files, which should be backed up ahead of time:
.	NINORD
.	NINORD4
.	NINORD5
.	NINORD6
.	NINSPEC2
.	NINPRINT
.	NINPRINTL
.
. DAILYJD.EXE should be run afterwards, to clean up above files and re-index.
.
.FOR TESTING PURPOSES FOLLOW LOGIC AROUND ".DREWTEST" LABELS.  THESE
.WILL DENOTE LINES WHERE FILES ARE UPDATED, AND WHICH SHOULD BE UPDATED FOR TESTING.
.
.....................................
	include	common.inc
	include	cons.inc
	include	norddd.inc

release  init      "1.32"        ASH	   05JUN2006	Added patch to allow dynamic use of Mail Dates in Program 1

output	file		.used for testing purposes

	move	C1,NORDPATH
.Set time from which we will start cancelling LCRs
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	move	C0,N5
	move	C0,JULDAYS
	call	CVTJUL
.DREWTEST
	sub	"183",JULDAYS,N5	.6 Months ago from today
	call	Paint
	move	"c:\work\lcrreclaim.dat",str35
	erase	str35
	prepare	output,str35
	display	*P10:10,*EL,"Mail Date: ",str10
	move	"LCRORD.AAM",str45
	trap	IOMssg Giving Error if IO
	open	NORDFLE2,str45
	pack	str4,"01Xz"
	move	"Aamread",Location
	pack	KeyLocation,"AAMKEY"
	read	NORDFLE2,str4;ORDVARS
	loop
		until over
		trapclr	IO
		add	C1,howmany
		display	*P10:12,*EL,"Current Record:    ",OLRN
		display	*P10:13,*EL,"Record Count:   ",howmany
		display	*P10:14,*EL,"LCR Count:     ",N10
		pack	str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
		call	Trim using str8
		move	OODTEC,CC
		move	OODTEY,YY
		move	OODTEM,MM
		move	OODTED,DD
		move	C0,JULDAYS
		call	CVTJUL
		if (JULDAYS <> C0 & JULDAYS >= N5)
			write	output,SEQ;OLRN,B1,OMDTEM,slash,OMDTED,SLASH,OMDTEC,SLASH,OMDTEY,B1,OODTEM,slash,OODTED,SLASH,OODTEC,SLASH,OODTEY
			ADD	C1,N10
		endif
		move	"AamKGread",Location
		pack	KeyLocation,"AAMKEY"
		trap	IOMssg Giving Error if IO
		readkg	NORDFLE2;ORDVARS
	repeat
	shutdown

	include	nordio.inc
	include	comlogic.iNC
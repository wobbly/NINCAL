.Purge Program Version which only writes out number of records currently a candidate
.for purging from NINPRINTL.DAT.
.Based on Mail Date, and current system date.
..............................................................
	include	common.inc
	include	cons.inc
	include	norddd.inc
	include	nordpdd.inc

release	init	"1.0"	ASH	INITIAL RELEASE

output	file		.used for testing purposes

	move	C1,NORDPATH
.Set time from which we will start cancelling LCRs
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	move	C0,N5
	move	C0,JULDAYS
	call	CVTJUL	
	sub	"183",JULDAYS,N5	.6 Months ago from today
	call	Paint
	pack	str10,MM,SLASH,DD,SLASH,CC,YY
.
	prepare	output,"c:\work\lcrPURGE.dat"
	display	*P10:10,*EL,"Mail Date: ",str10
	move	"NORDPSEQ",Location
	pack	KeyLocation,"SEQ"
	loop
		call	NORDPSEQ
		until over
		add	C1,howmany
		display	*P10:12,*EL,"Current Record:    ",OLRN
		display	*P10:13,*EL,"Record Count:   ",howmany
		move	OMDTEC,CC
		move	OMDTEY,YY
		move	OMDTEM,MM
		move	OMDTED,DD
		move	C0,JULDAYS
		call	CVTJUL
		if (JULDAYS <> C0 & JULDAYS < N5)
			add	C1,N9
		endif
	repeat
	write	output,SEQ;"NINPRINTL Purge Preliminary Report"
	write	output,SEQ;
	move	howmany,str9
	call	FormatNumeric using str9,str11
	write	output,SEQ;"Totals Records in NINPRINTL:  ",str11
	move	N9,str9
	call	FormatNumeric using str9,str11
	write	output,SEQ;"Eligible Purge Records:       ",str11
	shutdown

	include	nordio.inc
	include	nordpio.inc
	include	comlogic.iNC
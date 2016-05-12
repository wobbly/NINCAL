pc	equ		0
	include		common.inc
	include		cons.inc
	include		compdd.inc
          include cntdd.inc
	include		Ncmpxowndd.inc
release	init	"temp"	


	loop
loop	call	compks
	until	over
	type	COMPOLDOWN
	goto  	loop if not equal
	packkey	NCXOFLD2,COMPOLDOWN
	move	c2,NCXOPath
	call	NCXOTST
	if	over
	move	compnum,NCXOComp
	move	COMPOLDOWN,NCXOOwn
	call	NCXOWrt
	goto	loop
	endif
	repeat
	stop

	include		compio.inc
          include cntio.inc
	include		Ncmpxownio.inc
	include		comlogic.inc
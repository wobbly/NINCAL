	include	common.inc
	include	cons.inc
	include	nmdldd.inc

PC	EQU	0

release	init	"temp"
input	file
output	file

	open	input,"c:\work\ninmdlst2.dat"
	erase	"c:\work\booking.dat"
	prepare	output,"c:\work\booking.dat"
.
	loop
		read	input,SEQ;NMDLFLD
		until over
		add	C1,N9
		display	*p10:12,"records processed : ",N9
		CALL	NMDLKEY
		if not over
			move	YES,mdllcrcd2
			call	NMDLUPD
		else
			write	output,SEQ;MDLVARS
		endif
	repeat
	stop

	include	nmdlio.inc
	include	comlogic.inc

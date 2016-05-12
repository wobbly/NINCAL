	include	common.inc
	include	cons.inc
	include	npkgdd.inc
	include	ncmpdd.inc
	include	norddd.inc

PC	EQU	0

release	init	"temp"
input	file
output	file
output2	file

	erase	"c:\work\datacheck13.dat"
	erase	"c:\work\datacheck13a.dat"
	prepare	output,"c:\work\datacheck13.dat"
	prepare	output2,"c:\work\datacheck13a.dat"
.
	open	input,"c:\work\mlrtest.dat"
	move	C1,NPKGPATH
	move	C1,NCMPPATH
	loop
		read	input,SEQ;str4
		until over
		add	C1,N9
		display	*p10:12,"records processed : ",N9
		pack	NPKGFLD1,"01X",str4
		call	NPKGAIM
		if not over
			write	output,SEQ;NPKGVARS
		endif
		pack	NCMPFLD2,"02X",str4
		call	NCMPAIM
		if not over
			write	output2,SEQ;NCMPVARS
		endif
	repeat
	stop

	include	ncmpio.inc
	include	npkgio.inc
	include	nordio.inc
	include	comlogic.inc

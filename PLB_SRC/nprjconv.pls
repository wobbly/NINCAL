	include	common.inc
	include	cons.inc
	include	f:\library\include\nprjdd.inc
	include	compdd.inc
	include	cntdd.inc

release	init	"1.0"

tempfile1	file

	call	Paint
.	goto GetNextLoop
.
	erase	"c:\work\prjerror.dat"
	erase	"c:\work\projdolr.dat"
.
	prepare	tempfile,"c:\work\prjerror.dat"
	prepare	tempfile1,"c:\work\projdolr.dat"
	loop
		move	"NPRJSEQ",Location
		pack	KeyLocation,"Key: SEQ"
		call	NPRJSEQ
		until over
		if (ProjSrc = "B")
			move	"COMPKEY3",Location
			unpack	ProjClient,str2,str4
			pack	COMPFLD3,str4
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
			if over
				pack	str55,ProjClient," does not exist in Company file!!       "
				goto	WriteError
			elseif (COMPMLRFLG <> "T")
				pack	str55,ProjClient," is not a valid Mailer in Company file!!"
				goto	WriteError
			else
				move	COMPNUM,ProjClient
			endif
		endif
		add	C1,howmany
		display	*p10:10,"records ",howmany
		write	tempfile1,SEQ;PROJVARS
EndOLoop
	repeat
	shutdown

WriteError
	write	tempfile,SEQ;str55,PROJVARS
	goto EndOLoop
	return

	include	nprjio.inc
	include	compio.inc
	include	cntio.inc
	include	comlogic.inc

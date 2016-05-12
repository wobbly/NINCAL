	include 	common.inc
	include	cons.inc
Release	Init	"not"
PC	Equ	0
Laser	pfile
SpoolFL2	INit	"c:\work\faxfile.lst"
.		PRTOPEN Laser,"PDF995","",NoPRint,SpoolFile="faxfile"

                       	PRTOPEN 	Laser,"PDF995","faxfile.test"

.	call	debug
.                       	PRTClose 	Laser
		PrtPlay     Spoolfl2,"PDF995"

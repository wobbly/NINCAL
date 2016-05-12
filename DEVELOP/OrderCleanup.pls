	include	common.inc
	include	cons.inc
	include	norddd.inc
.	include	nord4dd.inc
.	include	nord5dd.inc
.	include	nord6dd.inc
	include	gnxtdd.inc

release	init	"temp"
tempfile2	file

.	ERASE	"C:\WORK\ORDERR.DAT"
.	prep	tempfile2,"C:\WORK\ORDERR.DAT",exclusive
.
	move	C1,NORDPATH
	open	tempfile,"\\NTS1\E\DATA\NPRINT.ah2",exclusive
	loop
		read	tempfile,SEQ;ORDVARS
		until	over
		pack	NORDFLD,OLRN
		CALL	NORDTST
		if over
			if (OSTAT = "R")
				move	"0",OSTAT
			endif
			call	NORDWRT
		else
			if (OSTAT = "R")
				move	"0",OSTAT
			endif
			call	NORDUPD
		endif
	repeat
	CLOSE	TEMPFILE
	shutdown
.
test
	open	tempfile,"\\NTS1\E\DATA\NINPRINTL.DAT",exclusive
	loop
		read	tempfile,SEQ;ORDVARS
		until	over
		if (OLRN > "575870")
			pack	NORDFLD,OLRN
			CALL	NORDTST
			if over
				call	NORDWRT
			else
//NORDIO.INC has been hardcoded so that str1 receives value of OSTAT when calling NORDTST!!
				if (OSTAT <> str1)
					move	"oops",str4
				else
					clear	str4
				endif
					//Write to error file for hand viewing!!
					WRITE	TEMPFILE2,SEQ;str4,OLRN
.				else
.					call	NORDUPD
.				endif
			endif
		endif
	repeat
	CLOSE	TEMPFILE
.
	move	"NORDNXT",GNXTFLD
	move	"GNXTKEY",Location
	pack	KeyLocation,"Key: ",GNXTFLD
	call	GNXTKEY
	move	C0,N6
	move	GNXTNUM,N6
	for howmany,"1","1000"
		move	N6,NORDFLD
		rep	zfill,NORDFLD
		call	NORDTST
		if over
			//Write to error file for research!!
			WRITE	TEMPFILE2,SEQ;NORDFLD
		endif
		sub	C1,N6
	repeat
.
	shutdown

	include	gnxtio.inc
	include	f:\library\develop\nordio.inc
.	include	nordio4.inc
.	include	nordio5.inc
.	include	nordio6.inc
	include	comlogic.inc
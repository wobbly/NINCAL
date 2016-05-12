	include	common.inc
	include	cons.inc
	include	f:\library\develop\adjacencysourcecode\src\user.io
	include	f:\library\develop\adjacencysourcecode\src\guidgen.inc

release	init	"1.0"	ASH	Initial Release

	open	tempfile,"c:\work\adjacency\develop\text\user.dat",exclusive
	loop
		read	tempfile,SEQ;fUserIO
		until over
		if (fUserIO.userType = "1" | fUserIO.userType = "2" | fUserIO.userType = "3")
			move	str35,fUserIO.fName
			move	str45,fUserIO.lName
			call	generateGUID using fUserIO.userID
			move	COMPCOMP,fUserIO.Organization
			clear	fUserIO.compFunction
			count	N2,fUserIO.username
			call	RandomPassword using fUserIO.password
			move	str1,fUserIO.userType
		endif
	repeat
	close	tempfile
	shutdown


	include	comlogic.inc
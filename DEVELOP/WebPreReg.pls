	include	common.inc
	include	cons.inc
	include	compdd.inc
	include	cntdd.inc
	include	f:\library\develop\adjacencysourcecode\src\user.io
	include	f:\library\develop\adjacencysourcecode\src\guidgen.inc

release	init	"1.0"	ASH	Initial Release

DimPtr	dim	^

RandomPassword external	"Random;RandomPassword"
tempfile2	file

	clock	timestamp,timestamp
	erase	"c:\work\adjacency\develop\text\user.dat"
	prep	tempfile2,"c:\work\adjacency\develop\text\user.dat",exclusive
	open	tempfile,"c:\work\webregistrants.csv",exclusive
	loop
		read	tempfile,SEQ;*CDFON,str35,str45,COMPFLD,fUserIO.Email
		until over
		call	Trim using str35
		if (str35 <> "")
			call	Trim using fUserIO.Email
			if (fUserIO.Email <> "")
				call	Trim using COMPFLD
				if (COMPFLD <> "")
					call	ZFillIt using COMPFLD
					call	COMPKEY
					if not over
						call	Trim using str45
						move	str35,fUserIO.fName
						move	str35,str1
						move	str45,fUserIO.lName
						call	generateGUID using fUserIO.userID
						move	COMPCOMP,fUserIO.Organization
						move	COMPADDR,fUserIO.Address
						move	COMPCITY,fUserIO.City
						move	COMPSTATE,fUserIO.State
						move	COMPZIP,fUserIO.Zip
						call	Trim using COMPPHONE
						if (COMPPHONE = "")
							move	"0000000000",fUserIO.Telephone
						else
							move	COMPPHONE,fUserIO.Telephone
						endif
						call	Trim using COMPFAX
						if (COMPFAX = "")
							move	"0000000000",fUserIO.Fax
						else
							move	COMPFAX,fUserIO.Fax
						endif
						clear	fUserIO.compFunction
						clear	fUserIO.Area
						move	fUserIO.fName,str1
						pack	fUserIO.username,str1,fUserIO.lName
						count	N2,fUserIO.username
						if (N2 < 5)
							pack	fUserIO.username,fUserIO.fName,fUserIO.lName
							count	N2,fUserIO.username
							if (N2 < 5)
								pack	fUserIO.username,fUserIO.fName,fUserIO.lName,"XXXXX"
							endif
						endif
						pause	"1"
						call	RandomPassword using fUserIO.password
						if (COMPCLRFLG = "T")
							move	"O",str1
						elseif (COMPMLRFLG = "T")
							move	"C",str1
						elseif (COMPBRKFLG = "T")
							move	"B",str1
						else
							move	"P",str1
						endif
						move	str1,fUserIO.userType
						move	"1",fUserIO.tos
						unpack	timestamp,fUserIO.dateTime
						move	COMPNUM,fUserIO.companyNumber
						move	YES,fUserIO.invoiceFlag
						write	tempfile2,SEQ;fUserIO
					endif
				endif
			endif
		endif
	repeat
	close	tempfile
	close	tempfile2
	shutdown


	include	compio.inc
	include	cntio.inc
	include	comlogic.inc
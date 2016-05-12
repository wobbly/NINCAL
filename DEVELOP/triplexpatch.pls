	include	common.inc
	include	cons.inc
	include	norddd.inc
	include	nsel2dd.inc

release	init	"temp"
compm    dim       25
qtyout2  dim       9
DESC     DIM       12
DESC2     DIM      3
tempfile2 file
tempfile3 file
tempfile4 file

	call	Paint
	move	C1,NORDPATH
	erase	"c:\work\triplex\listing.dat"
	erase	"c:\work\triplex\other.dat"
	prep	tempfile3,"c:\work\triplex\listing.dat",exclusive
	prep	tempfile4,"c:\work\triplex\other.dat",exclusive
	open	tempfile,"c:\work\triplex\lst.lst",exclusive
	loop
		read	tempfile,SEQ;str25
		until over
		add	C1,N5
		display	*p10:10,"File Number: ",N5
		display	*p10:12,"File Name: ",str25
		call	Trim using str25
		if (str25 <> "")
			pack	str55,"\\nts1\e\data\",str25	."
			open	tempfile2,str55
			loop
				read	tempfile2,SEQ;OLRN:
					B2:
					COMPM:
					B2:
					O1DES:
					B1:
					NSEL2NAME:
					B3:
					QTYOUT2:
					B5:
					DESC:
					B5:
					str9:
					desc2
				until over
				if (DESC = "            ")
					move	O1DES,str45
					pack	NORDFLD,OLRN
					call	NORDKEY
					if not over
						if (OMLRKY <> DESC)
							call	Trim using OMLRKY
							if (OMLRKY <> "")
								count	result,OMLRKY
								if (result < 7)
									type	OMLRKY
									if equal
										move	OMLRKY,str7
										call	ZFillIt using str7
										move	str7,OMLRKY
									endif
								endif
							endif
							move	OMLRKY,DESC
.
							write	tempfile3,SEQ;OLRN:
								B2:
								COMPM:
								B2:
								O1DES:
								B1:
								NSEL2NAME:
								B3:
								QTYOUT2:
								B5:
								DESC:
								B5:
								str9:
								desc2
						else
							write	tempfile4,SEQ;OLRN:
								B2:
								COMPM:
								B2:
								O1DES:
								B1:
								NSEL2NAME:
								B3:
								QTYOUT2:
								B5:
								DESC:
								B5:
								str9:
								desc2
						endif
					else
						write	tempfile4,SEQ;OLRN:
							B2:
							COMPM:
							B2:
							O1DES:
							B1:
							NSEL2NAME:
							B3:
							QTYOUT2:
							B5:
							DESC:
							B5:
							str9:
							desc2
					endif
				else
					write	tempfile4,SEQ;OLRN:
						B2:
						COMPM:
						B2:
						O1DES:
						B1:
						NSEL2NAME:
						B3:
						QTYOUT2:
						B5:
						DESC:
						B5:
						str9:
						desc2
				endif
			repeat
			close	tempfile2
		endif
	repeat
	shutdown

	include	nsel2io.inc
	include	nordio.inc
	include	comlogic.inc
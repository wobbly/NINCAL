	include	common.inc
	include	cons.inc
	include	norddd.inc
	include	nowndd.inc
	include	nfuldd.inc

PC	EQU	0

release	init	"temp"
output	file
ServiceNum dim	4(33),("0001"),("0002"),("0003"),("0004"),("0005"):
		("0006"),("0007"),("0008"),("0009"),("0010"),("0011"):
		("0012"),("0013"),("0014"),("0015"),("0016"),("0017"):
		("0018"),("0019"),("0020"),("0021"),("0022"),("0023"):
		("0024"),("0025"),("0026"),("0027"),("0028"),("0029"):
		("0030"),("0031"),("0032"),("0033")

ServiceName dim	25(33),("FIDE"),("JETSON"),("ANACAPA"),("ANTARES"),("SMALL PUB"):
		("DTM"),("PIDI"),("EPSILON"),("LIST MAINTENANC"),("KABLE"),("ENERTEX MKTG"):
		("NATL SUBSC.FUL."),("METRO SVC"),("NATL CONVSN SYS"),("DYNAMARK"),("IMI/INFO.MGT."),("PACIF.COAST DIR"):
		("INTERACTIVE SYS"),("DRCT ACCESS MKTG"),("METRO DIRECT"),("L & E MERIDIAN"),("ARG/RICHIE G."),("FFA/CATHERINE"):
		("BLAEMIRE COMM."),("ANNA M./ATC"),("TDMC"),("MKTG SERVICES"),("KABLE"),("FINC/LORI BRANSON MALM"):
		("FRONTLINE DATAGROUP"),("TARGET ANALYSIS GROUP"),("DMI/DATA MANAGEMENT"),("TRYLON")

OrderQty form	15(35)
OrderNum form	9(35)
SEQEOF2	form	"-4"	

	prepare	output,"c:\work\datacheck7.dat"
	clock	timestamp,timestamp
	move	timestamp,str8
	move	str8,str9
	move	C1,NORDPATH
	move	C1,NOWNPATH
	call	Paint
	CALL	NORDOPEN
	READ	NORDFILE,SEQEOF;ORDVARS
	loop
		READ	NORDFILE,SEQEOF2;ORDVARS
		until over
		add	C1,N9
		display	*p10:12,"records processed : ",N9
.		if (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X")
		if (OSTAT = "0" | OSTAT = "B")
			pack	str8,OODTEC,OODTEY,OODTEM,OODTED
			if (str8 > "20011231" AND str8 <= str9)
				pack	NOWNFLD,OLON
				rep	zfill,NOWNFLD
				call	NOWNKEY
				if not over
					call	Trim using OWNCTN
					if (OWNCTN <> "")
						for N2,"1","33"
							if (ServiceNum(N2) = OWNCTN)
								move	N2,result
								goto AfterLoop
							endif
						repeat
						move	"35",result
					else
						move	"34",result
					endif
AfterLoop
					add	C1,OrderNum(result)
					move	C0,howmany
					move	OQTY,howmany
					add	howmany,OrderQty(result)
				endif
			elseif (str8 < "20001231")
				break
			endif
		endif
	repeat
andrew
	write	Output,seq;"Live/Billed Orders from 1/1/2002 - Present, and their Fulfillment Houses"
	write	Output,seq;
	for result,"1","33"
		write	Output,seq;ServiceName(result)
		move	OrderNum(result),str9
		call	FormatNumeric using str9,str11
		call	Trim using str11
		move	OrderQty(result),str15
		call	FormatNumeric using str15,str25
		call	Trim using str25
		write	Output,seq;"          Orders: ",str11
		write	Output,seq;"        Quantity: ",str25
		write	Output,seq;
	repeat
	write	Output,seq;"No Service Bureau"
	move	OrderNum(34),str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	move	OrderQty(34),str15
	call	FormatNumeric using str15,str25
	call	Trim using str25
	write	Output,seq;"          Orders: ",str11
	write	Output,seq;"        Quantity: ",str25
	write	Output,seq;
.
	write	Output,seq;"Invalid Service Bureau!!"
	move	OrderNum(35),str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	move	OrderQty(35),str15
	call	FormatNumeric using str15,str25
	call	Trim using str25
	write	Output,seq;"          Orders: ",str11
	write	Output,seq;"        Quantity: ",str25
	write	Output,seq;
	stop

	include	nordio.inc
	include	nownio.inc
	include	nfulio.inc
	include	comlogic.inc

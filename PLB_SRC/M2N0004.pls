Pc	Equ	0
	Include	Common.inc
	INclude	COns.inc
RElease	Init	"0"
	INclude	M2nDD.inc
	INclude	M2nLoDD.inc
countIN	form	6
COuntWRt	form	6
LName	Dim	55
Name	Dim	25
ADDr	Dim	25
City	DIm	15
State	Dim	12
Zip	Dim	15
File	FIle
	Move	c1 to M2Npath
.	Open	FIle,"c:\work\min2nin.upd",read
.	Open	FIle,"c:\work\m2nupd3.txt",read
	OPen	FIle,"c:\work\Min\nin090.csv",read

	Loop
lOOP	Read	File,seq;*cdfon,M2NLoNin,Name,Addr,City,State,Zip,M2NLoMIN	
	until 	over		
	add	c1,countin
	display	*p5:10,"records in ",countin
	call	Debug
	call	Trim using M2NLomin
	count	n2,M2NLomin
		if	(N2 = 6)
		packkey	M2NLoFld,M2NLoMIN
		Elseif	(N2 = 5)
		packkey	M2NLoFld,c0,M2NLoMIN
		Elseif	(N2 = 4)
		packkey	M2NLoFld,c0,c0,M2NLoMIN
		Elseif	(N2 = 3)
		packkey	M2NLoFld,c0,c0,c0,M2NLoMIN
		Elseif	(N2 = 2)
		packkey	M2NLoFld,c0,c0,c0,c0,M2NLoMIN
		Elseif	(N2 = 1)
		packkey	M2NLoFld,c0,c0,c0,c0,c0,M2NLoMIN
		Elseif	(N2 = 0)
		GOTO	lOOP
		endif
	Move	M2nLomin,M2nLofld		
	packkey	M2NLoFld,M2NLoMIN

	call	Trim using M2NLoNin
	count	n2,M2NLonin
		if	(N2 = 6)
		packkey	Str6,M2NLonIN
		Elseif	(N2 = 5)
		packkey	str6,c0,M2NLoNin
		Elseif	(N2 = 4)
		packkey	str6,c0,c0,M2NLoNin
		Elseif	(N2 = 3)
		packkey	str6,c0,c0,c0,M2NLoNin
		Elseif	(N2 = 2)
		packkey	str6,c0,c0,c0,c0,M2NLoNin
		Elseif	(N2 = 1)
		packkey	str6,c0,c0,c0,c0,c0,M2NLoNin
		Elseif	(N2 = 0)
		GOTO	lOOP
		endif
	Move	Str6,M2NLoNIN

	call	M2NLoTst
	if	over
	call	M2NLoWrt
	add	c1,countwrt
	display	*p5:12,"records out ",countwrt

	endif
	repeat

	CLose	FIle
	goto	EOj                     .skip list matches

	OPen	FIle,"c:\work\Min\nin010.csv",read

	Loop
lOOP2	Read	File,seq;*cdfon,M2NNin,LName,M2NMIN	
	until 	over		
	add	c1,countin
	display	*p5:10,"records in ",countin
	call	Debug
	call	Trim using M2Nmin
	count	n2,m2nmin
		if	(N2 = 6)
		packkey	M2NFld,M2nMIN
		Elseif	(N2 = 5)
		packkey	M2NFld,c0,M2nMIN
		Elseif	(N2 = 4)
		packkey	M2NFld,c0,c0,M2nMIN
		Elseif	(N2 = 3)
		packkey	M2NFld,c0,c0,c0,M2nMIN
		Elseif	(N2 = 2)
		packkey	M2NFld,c0,c0,c0,c0,M2nMIN
		Elseif	(N2 = 1)
		packkey	M2NFld,c0,c0,c0,c0,c0,M2nMIN
		Elseif	(N2 = 0)
		GOTO	lOOP2
		endif
	IF	(M2nFld = "000000")
	goto	Loop2
	endif
	MOve	M2nFld,M2NMin	
	
	call	Trim using M2NNin
	count	n2,m2nnin
		if	(N2 = 6)
		packkey	Str6,M2nnIN
		Elseif	(N2 = 5)
		packkey	str6,c0,M2NNin
		Elseif	(N2 = 4)
		packkey	str6,c0,c0,M2NNin
		Elseif	(N2 = 3)
		packkey	str6,c0,c0,c0,M2NNin
		Elseif	(N2 = 2)
		packkey	str6,c0,c0,c0,c0,M2NNin
		Elseif	(N2 = 1)
		packkey	str6,c0,c0,c0,c0,c0,M2NNin
		Elseif	(N2 = 0)
		GOTO	lOOP2
		endif
	if	(str6 = "000000" or str6 = "")		
	goto	Loop2
	endif
	Move	Str6,M2nNIN
	call	M2nTst
	if	over
	call	M2NWrt
	add	c1,countwrt
	display	*p5:12,"records out ",countwrt

	endif
	repeat

	CLose	FIle

Eoj
	stop
	INClude	M2nio.inc
	INClude	M2nLoio.inc
	INClude	COmlogic.inc
	
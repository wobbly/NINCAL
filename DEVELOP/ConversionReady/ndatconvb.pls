	include	common.inc
	include	cons.inc
	include	F:\LIBRARY\DEVELOP\CONVERSIONREADY\ndatdd.inc
	include	F:\LIBRARY\DEVELOP\CONVERSIONREADY\nseldd.inc

release	init	"1.0"

DimPtr	dim	^
DimPtr1	dim	^
DimPtr2	dim	^
FilePtr	file	^

startfp	form	9
endfp	form	9
str46	dim	46
	
CreateSelects Routine DimPtr,DimPtr1,FilePtr
.DimPtr  = LSTNUM
.DimPtr1 = TEXTDATA
.FilePtr = NINSEL.DAT
.Prep Base/Secondary Base information
	move	C1,startfp
	move	C1,N4
	loop
		until (startfp > "2256")
		clear	NSELVARS
		clear	str46
		clear	str6
		move	"47",str46
		movefptr DimPtr1,startfp
		parse	DimPtr1 into str46 using " ~09",noskip,blankfill,truncate
		movefptr DimPtr1,endfp
		move	endfp,N5
		sub	startfp,N5
		compare	"2256",startfp
		if not equal
			if (N5 > 46)				.wordwrap, no new line char
				cmatch	B1,str46
				if eos
					move	B55,str46
				endif
				movelptr str46,N2
				if (N2 = 0)			.if empty blank fill
					move	B55,str46
				endif
			endif
		endif
.
		call	Trim using str46
		until (str46 = "")
.
		rep	lowup,str46
		scan	"EXCHANGE ONLY",str46
		if equal
			move	"2",NSELEXC
		else
			reset	str46
			scan	"EXCH ONLY",str46
			if equal
				move	"2",NSELEXC
			else
				reset	str46
				scan	"RENTAL ONLY",str46
				if equal
					move	"3",NSELEXC
				else
.						move	"0",NSELEXC
.					endif
					reset	str46
					scan	"EXCH",str46		.push text away from any Base Select Dollar Selects, and go straight to Price
					if not equal
						move	"3",NSELEXC
					else
						move	"1",NSELEXC
					endif
					scan	"$",str46
					if equal
						bump	str46
						scan	"$",str46
						if equal
							bump	str46
							move	str46,str6
							for result,"6","1",SEQ
								type	str6
								if equal
									break
								else
									setlptr	str6,result
								endif
							repeat
						else
							move	str46,str6
							for result,"6","1",SEQ
								type	str6
								if equal
									break
								else
									setlptr	str6,result
								endif
							repeat
						endif
.							movefptr str46,N9
					endif
					call	Trim using str6
					if (str6 = "")		.No Pricing Information, clear Exchange/Rent Flag
						move	"1",NSELEXC
					else
						move	str46,str15
					endif
				endif
			endif
		endif
		reset	str46
		if (str6 <> "")
			move	C0,NSELPRICE
			move	str6,NSELPRICE
			call	Trim using str15
			scan	SLASH,str15
			if equal
				bump	str15
			endif
			if (str15 = "M")
				move	"001",NSELDESC
			elseif (str15 = "LIST" | str15 = "LST")
				move	"003",NSELDESC
			elseif (str15 = "FLAT FEE" | str15 = "FLAT")
				move	"002",NSELDESC
			elseif (str15 = "INQUIRE")
				move	"009",NSELDESC
			elseif (str15 = "SEE BELOW")
				move	"1",NSELNOTES
				move	"008",NSELDESC
			elseif (str15 <> "EXCHANGE ONLY" & str15 = "RENTAL ONLY")	.Already taken care of
.				write	output1,SEQ;str46
			endif
		endif
		move	DimPtr,NSELLIST
		move	N4,NSELNUM
		rep	zfill,NSELNUM
		move	NSELNUM,NSELINDEX
.
		reset	str46
		setlptr	str46
		call	Trim using str46
test1
		scan	B1,str46
		if equal
			movefptr str46,N8
			reset	str46
			setlptr	str46,N8
			move	str46,str25
			call	Trim using str25
			call	RemoveChar using str25,COMMA
			type	str25
			if equal
				move	C0,N10
				move	str25,N10
				move	N10,NSELQTY
				rep	zfill,NSELQTY
				reset	str46,N8
			elseif (str25 = "INQUIRE")
				reset	str46,N8
			else
				reset	str46
			endif
		endif
		setlptr	str46
		move	str46,str55
		call	Trim using str55
test2
		move	"  ",str2
		scan str2,str55
		if equal
			movefptr str55,N9
			reset	str55
			setlptr	str55,N9
			move	str55,str46
		else
			scan	STAR,str55
			if equal
				movefptr str55,N9
				reset	str55
				setlptr	str55,N9
				move	str55,str46
			else
				move	str55,str46
			endif
		endif
		move	str46,NSELSNAME
.
		if (startfp = 1)
			move	"BASE",NSELBASE
		else
			move	"SEC.",NSELBASE
		endif
		clock	timestamp,timestamp
		move	timestamp,NSELDATE
.
		write	FilePtr,SEQ;NSELVARS
		add	"47",startfp
		add	C1,N4
		reset	DimPtr1,startfp
	repeat
	reset	DimPtr1
	return

	include	F:\LIBRARY\DEVELOP\CONVERSIONREADY\ndatio.inc
	include	F:\LIBRARY\DEVELOP\CONVERSIONREADY\nselio.inc
	include	comlogic.inc
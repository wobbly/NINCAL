.LIST USAGE CREATION SUPPLEMENTAL PROGRAM
........................................
. Program:	NUSG0001.PLS
. Function:	List Usage Creation Supplemental Program (Requires NUSG0001)
. Author:	Andrew Harkins
. Orig.	Date:	July 22,2004
. Release:	1.0
........................................
PC       EQU       1

	include	common.inc
	include	cons.inc
.	include	f:\library\develop\backups\norddd.inc
	include	norddd.inc
.START PATCH 1.2 ADDED LOGIC
	include	ndatdd.inc
	include	nsel2dd.inc
.END PATCH 1.2 ADDED LOGIC

release  init      "1.31"       DLH 23Sep07	PLI
.release  init      "1.3"       ASH 31JAN05	VERSION USING CRITERIA ESTABLISHED BY NP 1/31/2005
.release  init      "1.2"       ASH 20JAN05	VERSION USING CRITERIA ESTABLISHED BY NP 1/20/2005
.release  init      "1.0"       ASH 21JUL04	Initial Release

N18Mos		form	5
N12Mos		form	5
N6Mos		form	5
.START PATCH 1.3 ADDED LOGIC
N24Mos		form	5
Case1A		form	1
Case1B		form	1
Case2A		form	1
Case2B		form	1
Case3A		form	1
Case3B		form	1
Case4A		form	1
Case4B		form	1
.END PATCH 1.3 ADDED LOGIC
TestFlag	form	1
DimPtr		dim	^
DimPtr1		dim	^
FrmPtr		form	^

.START PATCH 1.1 ADDED LOGIC
mss1	plform	Error

FindUsageLoadErrorWindow Routine
	formload mss1
	return
.END PATCH 1.1 ADDED LOGIC

FindUsage Routine DimPtr,DimPtr1,FrmPtr
.Determines if valid Usage exists between List & Mailer
.DimPtr  = List Number
.DimPtr1 = Mailer Number
.FrmPtr  = Return Flag, 1=yes
.
	move	C0,FrmPtr	Initialize Return Variable
	move	C0,N2		Initialize Flag for 2 non-entire Orders in 12 months
	move	C0,N3		Initialize Flag for Order in last 6 months COUPLED with...
	move	C0,N4		Initialize Flag for Order in last 18 months
	move	C0,N7		Initialize Flag to test Date for Test Orders
	move	C0,TestFlag	Initialize Second Part of flag to test for Test Orders
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	call	CVTJUL
	sub	"365",JULDAYS,N12Mos
	sub	"183",JULDAYS,N6Mos
	sub	"568",JULDAYS,N18Mos
	sub	"730",JULDAYS,N6	Outer limit of records to search - 2 years
.
	move	C3,NORDLOCK
	pack	NORDFLD1,"01X",DimPtr1
	pack	NORDFLD2,"02X",DimPtr
	move	"AIM-readlast",Location
	pack	KeyLocation,"Key: ",NORDFLD1,NORDFLD2
.	call	NORDAIM
	branch	NORDFLG2 TO NORDlast9
	call	NORDOPN2
NORDlast9
	readlast NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
	loop
		until over
		move	OODTEC,CC
		move	OODTEY,YY
		move	OODTEM,MM
		move	OODTED,DD
		call	CVTJUL
.begin patch 1.31a
.begin patch 1.31
		clear	str1
		unpack      olrn,str1,str5 
.		until (JULDAYS < N6 & str1 <> "B" & STR1 <> "M")
.		until (JULDAYS < N6 & Ocompid <> "P" & Ocompid2 <>"P")
.end patch 1.31a
.		until (JULDAYS < N6)
.end patch 1.31
		if (OSTAT = "0" or OSTAT = "B")
			if (JULDAYS > N7)
				move	JULDAYS,N7
.				if (OTOCODE = "1" | OTOCODE = "2")	1=Test, 2=Retest
				if (OTOCODE = "1")			1=Test, 2=Retest
					move	C1,TestFlag
				else
					move	C0,TestFlag
				endif
			endif
			if (FrmPtr <> C1)	.Skip if already determined
				if (JULDAYS >= N12Mos)
					if (OELCODE =  "1" | OELCODE =  "3")
						move	C1,FrmPtr
					else
						move	C0,howmany
						call	Trim using OQTY
						move	OQTY,howmany
						if (howmany >= "50000")
							move	C1,FrmPtr
						else
							add	C1,N2
							if (N2 >= C2)
								move	C1,FrmPtr
							endif
						endif
						if (FrmPtr <> C1)
							call	LastTest
						endif
					endif
				else
					call	LastTest
				endif
			endif
		endif
		move	"NORDKGP",Location
		call	NORDKGP
	repeat
.If last Order was a Test negate possibility of usage
	if (TestFlag = C1)
		move	C0,FrmPtr
	endif
	return

LastTest
	if (JULDAYS >= N6Mos)
		if (N3 > 0)
			move	C1,N4
		else
			move	C1,N3
		endif
	elseif (JULDAYS >= N18Mos)
		move	C1,N4
	endif
	if (N3 > 0 AND N4 > 0)
		move	C1,FrmPtr
	endif
	return

.START PATCH 1.2 ADDED LOGIC
FindUsage2 Routine DimPtr,DimPtr1,FrmPtr
.Determines if valid Usage exists between List & Mailer
.DimPtr  = List Number
.DimPtr1 = Mailer Number
.FrmPtr  = Return Flag, 1=yes
.
	move	C0,FrmPtr	Initialize Return Variable
.START PATCH 1.3 REPLACED LOGIC
.	move	C0,N2
.	move	C0,N3
.	move	C0,N4
.	move	C0,N7
........................
	move	C0,Case1A
	move	C0,Case1B
	move	C0,Case2A
	move	C0,Case2B
	move	C0,Case3A
	move	C0,Case3B
	move	C0,Case4A
	move	C0,Case4B
.END PATCH 1.3 REPLACED LOGIC
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	call	CVTJUL
	sub	"365",JULDAYS,N12Mos
.START PATCH 1.3 REPLACED LOGIC
.	sub	"568",JULDAYS,N18Mos
.	sub	"730",JULDAYS,N6	Outer limit of records to search - 2 years
	sub	"730",JULDAYS,N24Mos
	sub	"547",JULDAYS,N18Mos
	sub	"1095",JULDAYS,N6	Outer limit of records to search - 3 years
.END PATCH 1.3 REPLACED LOGIC
	move	C1,NDATPATH
.START PATCH 1.3 ADDED LOGIC
	call	GetListUniverse
.END PATCH 1.3 ADDED LOGIC
.
	move	C3,NORDLOCK
	pack	NORDFLD1,"01X",DimPtr1
	pack	NORDFLD2,"02X",DimPtr
	move	"AIM-readlast",Location
	pack	KeyLocation,"Key: ",NORDFLD1,NORDFLD2
.	call	NORDAIM
	branch	NORDFLG2 TO NORDlast9b
	call	NORDOPN2
NORDlast9b
	readlast NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
	loop
		until over
		until (FrmPtr = C1)
		move	OODTEC,CC
		move	OODTEY,YY
		move	OODTEM,MM
		move	OODTED,DD
		call	CVTJUL
.begin patch 1.31a
.begin patch 1.31
		clear	str1
		unpack      olrn,str1,str5 
.		until (JULDAYS < N6 & str1 <> "B" & STR1 <> "M")
.		until (JULDAYS < N6 & Ocompid <> "P" & Ocompid2 <>"P")
.end patch 1.31a
.		until (JULDAYS < N6)
.end patch 1.31
		if (OSTAT = "0" or OSTAT = "B")
.START PATCH 1.2 REPLACED LOGIC
.			if (N4 > 0)	.18 Month clause
.				if (JULDAYS >= N12Mos)
.					move	C0,howmany
.					call	Trim using OQTY
.					move	OQTY,howmany
.					if (howmany >= "5000")
.						move	C1,FrmPtr
.					else
..Determine List/Select Universe
.						clear	str10
.						packkey	NSEL2FLD,"1",OLRN
.						move	"NSEL2KEY",Location
.						pack	KeyLocation,"Key: ",NSEL2FLD
.						call	NSEL2KEY
.						if not over
.							if (NSEL2QTY > 0)
.								move	NSEL2QTY,str10
.							elseif (NSEL2QTY2 > 0)
.								move	NSEL2QTY2,str10
.							else
.								call	GetListUniverse
.							endif
.						else
.							call	GetListUniverse
.						endif
.						move	C0,N10
.						call	Trim using str10
.						move	str10,N10
.						if (N10 < "5000")
.							move	C1,FrmPtr
.						endif
.					endif
.				endif
.			elseif (JULDAYS >= N18Mos)
.				move	C1,N4
.			endif
................................................................
.START PATCH 1.3 REPLACED LOGIC
.			if (JULDAYS >= N12Mos)
.				if (N4 = 1)
.					move	C1,FrmPtr
.				else
.					move	C0,howmany
.					call	Trim using OQTY
.					move	OQTY,howmany
.					if (howmany >= "5000")
.						if (N4 > 0)
.							move	C1,FrmPtr
.						else
.							move	C1,N4
.						endif
.					else
..Determine List/Select Universe
.						clear	str10
.						packkey	NSEL2FLD,"1",OLRN
.						move	"NSEL2KEY",Location
.						pack	KeyLocation,"Key: ",NSEL2FLD
.						call	NSEL2KEY
.						if not over
.							if (NSEL2QTY > 0)
.								move	NSEL2QTY,str10
.							elseif (NSEL2QTY2 > 0)
.								move	NSEL2QTY2,str10
.							else
.								call	GetListUniverse
.							endif
.						else
.							call	GetListUniverse
.						endif
.						move	C0,N10
.						call	Trim using str10
.						move	str10,N10
.						if (N10 < "5000")
.							if (N4 > 0)
.								move	C1,FrmPtr
.							else
.								move	C1,N4
.							endif
.						endif
.					endif
.				endif
.			elseif (JULDAYS >= N18Mos)
.				if (N4 = 1)
.					move	C1,FrmPtr
.				else
.					move	C2,N4
.				endif
.			endif
.................................................................
			if (N10 <= 20000)
.Case 1 = If List Universe <= 20,000, 1 order in last 12 months, any size, plus 1 add'l order 0-18 mos
				if (JULDAYS >= N12Mos)
					if (Case1A > 0)
						move	C1,Case1B
					else
						move	C1,Case1A
					endif
					if (Case1A = C1 & Case1B = C1)
						move	C1,FrmPtr
					endif
				elseif (JULDAYS >= N18Mos)
					move	C1,Case1B
					if (Case1A = C1 & Case1B = C1)
						move	C1,FrmPtr
					endif
				endif
			elseif (N10 > 20000)
.Case 2 = If List Universe > 20,000, (1 order in last 12 mos >=10m, AND 1 add'l order  >=  5m 0-18 mos)
.Case 3 = If List Universe > 20,000, (1 order in last 12 mos >= 5m, AND 2 add'l orders >=  5m 0-18 mos)
.Case 4 = If List Universe > 20,000, (1 order in last 12 mos >=50m, AND 1 add'l order  >= 10m 0-24 mos)
				move	C0,howmany
				call	Trim using OQTY
				move	OQTY,howmany
				if (howmany >= "50000")
					call	TestCase4A	.Includes test to TestCase4B
					call	TestCase3
					call	TestCase2A	.Includes test to TestCase2B
				elseif (howmany >= "10000")
					call	TestCase4B
					call	TestCase3
					call	TestCase2A	.Includes test to TestCase2B
				elseif (howmany >= "5000")
					call	TestCase3
					call	TestCase2B
				endif
			endif
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
		endif
		move	"NORDKGP",Location
		call	NORDKGP
	repeat
	return

.START PATCH 1.3 ADDED LOGIC
TestCase4A
.Required:  Qty 50K
.Case 4A = 1 order in last 12 mos >=50m, AND 1 add'l order  >= 10m 0-24 mos
.Includes test to TestCase4B
	if (JULDAYS >= N12Mos)
		if (Case4A > 0)
			move	C1,Case4B
		else
			move	C1,Case4A
		endif
		if (Case4A = C1 & Case4b = C1)
			move	C1,FrmPtr
		endif
	elseif (JULDAYS >= N24Mos)
		move	C1,Case4B
		if (Case4A = C1 & Case4B = C1)
			move	C1,FrmPtr
		endif
	endif
	return

TestCase4B
.Required:  Qty 10K
.Case 4B = 1 add'l order  >= 10m 0-24 mos
	if (JULDAYS >= N24Mos)
		move	C1,Case4B
		if (Case4A = C1 & Case4B = C1)
			move	C1,FrmPtr
		endif
	endif
	return

TestCase2A
.Required:  Qty 10K
.Case 2A = 1 order in last 12 mos >=10m, AND 1 add'l order  >=  5m 0-18 mos
.Includes test to TestCase2B
	if (JULDAYS >= N12Mos)
		if (Case2A > 0)
			move	C1,Case2B
		else
			move	C1,Case2A
		endif
		if (Case2A = C1 & Case2B = C1)
			move	C1,FrmPtr
		endif
	elseif (JULDAYS >= N18Mos)
		move	C1,Case2B
		if (Case2A = C1 & Case2B = C1)
			move	C1,FrmPtr
		endif
	endif
	return

TestCase2B
.Required:  Qty 5K
.Case 2B = 1 add'l order  >=  5m 0-18 mos
	if (JULDAYS >= N18Mos)
		move	C1,Case2B
		if (Case2A = C1 & Case2B = C1)
			move	C1,FrmPtr
		endif
	endif
	return

TestCase3
.Required:  Qty 5K
.Case 3 = 1 order in last 12 mos >= 5m, AND 2 add'l orders >=  5m 0-18 mos
	if (JULDAYS >= N12Mos)
		if (Case3A = 1)
			add	C1,Case3B
		else
			move	C1,Case3A
		endif
		if (Case3B >= C2 & Case3A = C1)
			move	C1,FrmPtr
		endif
	elseif (JULDAYS >= N18Mos)
		add	C1,Case3B
		if (Case3B >= C2 & Case3A = C1)
			move	C1,FrmPtr
		endif
	endif
	return
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.2 ADDED LOGIC
GetListUniverse
.START PATCH 1.2 ADDED LOGIC
	move	C0,N10
.END PATCH 1.2 ADDED LOGIC
	pack	NDATFLD,OLNUM
	move	"NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATPATH
	call	NDATKEY
	if not over
.START PATCH 1.2 REPLACED LOGIC
.		move	UNIVERSE,str10
		call	Trim using UNIVERSE
		move	UNIVERSE,N10
.END PATCH 1.2 REPLACED LOGIC
	endif
	return
.END PATCH 1.2 ADDED LOGIC

.START PATCH 1.2 ADDED LOGIC
	include	ndatio.inc
	include	nsel2io.inc
.END PATCH 1.2 ADDED LOGIC
	include	nordio.inc
	include	comlogic.inc

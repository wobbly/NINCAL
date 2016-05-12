	include	common.inc
	include	cons.inc
	include	f:\library\include\ndatdd.inc
	include	nadddd.inc
	include	narrdd.inc
	include	ntxtdd.inc
	include	NSLTdd.inc
	include	nsrcdd.inc
	include	nmeddd.inc
	include	ncatdd.inc
	include	nowndd.inc
	include	nfuldd.inc

.pc	equ	1

release	init	"1.0"	initial release

CreateSelects external "ndatconvb;CreateSelects"

NewOWNNUM	DIM	6	.Increased to 6 bytes
DATFUL		DIM	6	.New
DATMAN		DIM	6	.New
DATMLR		DIM	6	.New
NewNEWDATE	DIM	8	.Reformatted from MMDDCCYY to CCYYMMDD
NewREVDATE	DIM	8	.Reformatted from MM-DD-CCYY to CCYYMMDD
NewMLSTNAME	DIM	75	.Increased to 75 bytes
NewCLEANCDE	DIM	4	.Increase 1 byte
NewNETNAME	DIM	4	.Increase 1 byte
NewDELCODE	DIM	4	.Increase 1 byte
NewSAMPLE	DIM	4	.Increase 1 byte
DATPAY		DIM	6	.New
NDATCONV	DIM	1
NDATEXCH	DIM	1	.1 = EXCHANGE ONLY - NOT USED AS OF MARCH 17, 2004!!!
NDATWEB	        DIM	1
NDATOFF	        DIM	1
NDATFILL2	DIM	101	.FILLER
UNIVERSE2	DIM	10

NADDPRICETemp	FORM	5.2
NADDDESCTemp	DIM	3

TEXTData2	DIM	2370	.Size of TEXTDATA (2256) + enough to Hold "RETURN-TO INFO" + Carriage Return + following fields:
TEXTA		DIM	47	.Temporary storage of individual lines in TextData
.RLINE1   DIM       18          469-486  RETURN-TO INFO.
.RLINE2   DIM       18          487-504  RETURN-TO INFO.
.RLINE3   DIM       18          505-522  RETURN-TO INFO.
.RLINE4   DIM       18          523-540  RETURN-TO INFO.
.RLINE5   DIM       18          541-558  RETURN-TO INFO.
RFlag		form	1	.Test for instances where RLINE Fields are written to Text fields
drew	form	"0"

input	ifile
output	file
output2	file
output3	file
output4	file
output5	file
output6	file
output7	file
output8	file

DimPtr	dim	^
DimPtr2	dim	^

TextArray dim	500(5)	.2256
CounterVar form	9
	
	call	Paint

	move	C1,NDATPATH
	open	input,"c:\work\nincatref.isi",read
	prepare	output,"c:\work\ninDAT.dat",exclusive
	prepare	output2,"c:\work\niNSLTexc.dat",exclusive
	prepare	output3,"c:\work\ninadddup.dat",exclusive
	prepare	output4,"c:\work\ninarrdup.dat",exclusive
	prepare	output5,"c:\work\nincatdup.dat",exclusive
	prepare	output6,"c:\work\ninsrcdup.dat",exclusive
	prepare	output7,"c:\work\ninsltdup.dat",exclusive
	prepare	output8,"c:\work\ninsel.dat",exclusive
.
	move	C3,NDATLOCK
	loop
		move	"NDATSEQ",Location
		call	NDATSEQ
		until over
		add	C1,CounterVar
		display    *p10:10,"records ",CounterVar
.Modify certain fields
		pack	NewOWNNUM,"00",OWNNUM
.
		clear	DATFUL		.Initialize it
		pack	NOWNFLD,OWNNUM
		move	"NOWNKEY",Location
		pack	KeyLocation,"Key: ",NOWNFLD
		call	NOWNKEY
		if not over
			call	Trim using OWNCTN
			if (OWNCTN <> "")
				pack	NFULFLD,OWNCTN
				rep	zfill,NFULFLD
				move	C1,NFULPATH
				move	"NFULKEY",Location
				pack	KeyLocation,NFULFLD
				call	NFULKEY
				if not over
					move	NFULNUM,DATFUL
				endif
			endif
		endif
.
		clear	DATMAN
		clear	DATMLR
.
		unpack	NEWDATE,MM,DD,CC,YY
		pack	NewNEWDATE,CC,YY,MM,DD
.
		unpack	REVDATE,MM,str1,DD,str1,CC,YY
		pack	NewREVDATE,CC,YY,MM,DD
.
		move	MLSTNAME,NewMLSTNAME
		scan	"OFFICE USE",MLSTNAME
		if equal
			move	C1,NDATOFF
		else
			clear	NDATOFF
		endif
.
		unpack	CLEANCDE,str1,str2
		move	C0,N2
		move	str2,N2
		if (N2 = 21)	.Quarterly has a dupe
			move	"03",str2
		endif
		pack	NewCLEANCDE,str1,"0",str2
		rep	zfill,NewCLEANCDE
.
		unpack	NETNAME,str1,str2
		pack	NewNETNAME,str1,"0",str2
		rep	zfill,NewNETNAME
.
		unpack	DELCODE,str1,str2
		pack	NewDELCODE,str1,"0",str2
		rep	zfill,NewDELCODE
.
		unpack	SAMPLE,str1,str2
		pack	NewSAMPLE,str1,"0",str2
		rep	zfill,NewSAMPLE
.Exchange Only Field
.		clear	NDATEXCH
.		parse	textdata into text1 using " ~09",noskip,blankfill
.		rep	lowup,TEXT1
.		scan	"EXCHANGE ONLY",TEXT1
.		if equal
.			move	C2,NDATEXCH
.		else
..			reset	text1
..			scan	"EXCH/",TEXT1
..			if equal
..				move	C2,NDATEXCH
..			endif
.		endif
.		reset	textdata
......................................................
.		clear	NDATEXCH
.		parse	textdata into text1 using " ~09",noskip,blankfill
.		rep	lowup,TEXT1
.		scan	"EXCHANGE ONLY",TEXT1
.		if equal
.			move	"2",NDATEXCH
.		else
.			reset	TEXT1
.			scan	"EXCH ONLY",TEXT1
.			if equal
.				move	"2",NDATEXCH
.			else
.				reset	TEXT1
.				scan	"RENTAL ONLY",TEXT1
.				if equal
.					move	"3",NDATEXCH
.				endif
.			endif
.		endif
.		reset	textdata
.Call routine to create Select File
		call	CreateSelects using LSTNUM,TEXTDATA,output8
.What are we gonna do about the following field???
		clear	DATPAY
.
		call	Trim using UNIVERSE
		if (UNIVERSE <> "")
			move	C0,N10
			move	UNIVERSE,N10
			move	N10,UNIVERSE2
			rep	zfill,UNIVERSE2
		else
			move	"           ",UNIVERSE2
		endif
.
		move	"Write to New File",Location
		pack	KeyLocation,"Key: ",NDATFLD
		write	output,SEQ;STATUS:
				LSTNUM:
				NewOWNNUM:
				DATFUL:
				DATMAN:
				DATMLR:
				NLSTCDE:
				ELSTCDE:
				COMMPER:
				HOTLINE:
				NewNEWDATE:
				NewREVDATE:
				PASSWORD:
				NewMLSTNAME:
				OLSTNAME:
				NewCLEANCDE:
				CLNINFO:
				NewNETNAME:
				NETINFO:
				NewDELCODE:
				NewSAMPLE:
				SEX:
				MIN:
				UNIVERSE2:
				DATPAY:
				NDATCONV:
				NDATEXCH:
				unitdata:
				NDATWEB:
				NDATOFF:
				NDATFILL2
.Peel out information from RLINE Fields
		move	C0,RFlag
		call	Trim using RLINE1
		call	Trim using RLINE2
		call	Trim using RLINE3
		call	Trim using RLINE4
		call	Trim using RLINE5
		if (RLINE1 <> "" | RLINE2 <> "" | RLINE3 <> "" | RLINE4 <> "" | RLINE5 <> "")
			if (RLINE1 <> "EMAIL @ $50 Flat" OR RLINE2 <> "FTP   @ $50 Flat" OR RLINE3 <> "CDROM @ $50 Flat" OR RLINE4 <> "3480/90 Cartridge" OR (RLINE5 <> "$25 Flat" AND RLINE5 <> "$30 Flat" AND RLINE5 <> "$40 Flat"))
				rep	lowup,RLINE1
				rep	lowup,RLINE2
				rep	lowup,RLINE3
				rep	lowup,RLINE4
				rep	lowup,RLINE5
				if (RLINE1 <> "DO NOT RETURN" OR RLINE2 <> "MAG TAPE/CARTRIDGE" OR RLINE4 <> "IS REQUIRED ON" OR RLINE5 <> "RENTALS." OR (RLINE3 <> "A $25.00 FLAT FEE" & RLINE3 <> "A $30.00 FLAT FEE" & RLINE3 <> "A $40.00 FLAT FEE"))
					if (RLINE1 <> "DO NOT RETURN TAPE" OR RLINE3 <> "IS REQUIRED ON" OR RLINE4 <> "RENTALS." OR RLINE5 <> "" OR (RLINE2 <> "A $25.00 FLAT FEE" & RLINE2 <> "A $30.00 FLAT FEE" & RLINE2 <> "A $40.00 FLAT FEE"))
						if (RLINE1 <> "DO NOT RETURN TAPE" OR RLINE3 <> "IS REQUIRED." & RLINE3 <> "IS REQUIRED" OR RLINE4 <> "" OR RLINE5 <> "" OR (RLINE2 <> "A $25.00 FLAT FEE" & RLINE2 <> "A $30.00 FLAT FEE" & RLINE2 <> "A $40.00 FLAT FEE"))
							if (STATUS <> "W")
								unpack	REVDATE,MM,str1,DD,str1,CC,YY
								pack	str4,CC,YY
								move	C0,N4
								move	str4,N4
								if (N4 > "1992")
.Flag to have records appended to Text information
									move	C1,RFlag
								endif
							endif
						else
.Process records which contain:
."DO NOT RETURN TAPE"							.Disregard - this is the standard these days
."A $25.00 FLAT FEE" OR "A $30.00 FLAT FEE" OR "A $40.00 FLAT FEE"     .Disregard as per DH - Perhaps have a "Generic Tape" Entry in Addressing to apply this fee to.
."IS REQUIRED." & RLINE3 <> "IS REQUIRED"
						endif
					else
.Process records which contain:
."DO NOT RETURN TAPE"							.Disregard - this is the standard these days
."A $25.00 FLAT FEE" OR "A $30.00 FLAT FEE" OR "A $40.00 FLAT FEE"	.Perhaps have a "Generic Tape" Entry in Addressing to apply this fee to.
."IS REQUIRED ON"							.Disregard as per DH
."RENTALS."								.Disregard
					endif
				else
.Process records which contain:
."DO NOT RETURN"				.Disregard - this is the standard these days
.
."MAG TAPE/CARTRIDGE"
."A $25.00 FLAT FEE" OR "A $30.00 FLAT FEE" OR "A $40.00 FLAT FEE"
					move	C0,NADDPRICE
					if (ELSTCDE = "C")	.Exclusive Only
						move	"40",NADDPRICE
					else
						if (RLINE3 = "A $25.00 FLAT FEE")
							move	"25",NADDPRICE
						elseif (RLINE3 = "A $30.00 FLAT FEE")
							move	"30",NADDPRICE
						elseif (RLINE3 = "A $40.00 FLAT FEE")
							move	"40",NADDPRICE
						endif
					endif
					pack	NADDNUM,"021"
					pack	NADDLIST,LSTNUM
					move	"002",NADDDESC
					pack	NADDFLD,NADDLIST,NADDNUM
					move	"NADDTST5",Location
					pack	KeyLocation,"Key: ",NADDFLD
					call	NADDTST
					if over
						move	"NADDWRT5",Location
						call	NADDWRT
					else
						write	output3,SEQ;LSTNUM,NADDVARS
					endif
."IS REQUIRED ON"				.Disregard
."RENTALS."					.Disregard
.Drop in Addressing
				endif
			else
.Drop All in Addressing
.Process records which contain:
."EMAIL @ $50 Flat"
				pack	NADDNUM,"025"
				pack	NADDLIST,LSTNUM
				move	"50",NADDPRICE
				move	"002",NADDDESC
				pack	NADDFLD,NADDLIST,NADDNUM
				move	"NADDTST1",Location
				pack	KeyLocation,"Key: ",NADDFLD
				call	NADDTST
				if over
					move	"NADDWRT1",Location
					call	NADDWRT
				else
					write	output3,SEQ;LSTNUM,NADDVARS
				endif
."FTP   @ $50 Flat"
				pack	NADDNUM,"027"
				pack	NADDLIST,LSTNUM
				move	"50",NADDPRICE
				move	"002",NADDDESC
				pack	NADDFLD,NADDLIST,NADDNUM
				move	"NADDTST2",Location
				pack	KeyLocation,"Key: ",NADDFLD
				call	NADDTST
				if over
					move	"NADDWRT2",Location
					call	NADDWRT
				else
					write	output3,SEQ;LSTNUM,NADDVARS
				endif
."CDROM @ $50 Flat"
				pack	NADDNUM,"028"
				pack	NADDLIST,LSTNUM
				move	"50",NADDPRICE
				move	"002",NADDDESC
				pack	NADDFLD,NADDLIST,NADDNUM
				move	"NADDTST3",Location
				pack	KeyLocation,"Key: ",NADDFLD
				call	NADDTST
				if over
					move	"NADDWRT3",Location
					call	NADDWRT
				else
					write	output3,SEQ;LSTNUM,NADDVARS
				endif
."3480/90 Cartridge"
."$30 Flat"
				move	C0,NADDPRICE
				if (ELSTCDE = "C")	.Exclusive Only
					move	"40",NADDPRICE
				else
					if (RLINE5 = "$25 Flat")
						move	"25",NADDPRICE
					elseif (RLINE5 = "$30 Flat")
						move	"30",NADDPRICE
					elseif (RLINE5 = "$40 Flat")
						move	"40",NADDPRICE
					endif
				endif
				pack	NADDNUM,"021"
				pack	NADDLIST,LSTNUM
				move	"002",NADDDESC
				pack	NADDFLD,NADDLIST,NADDNUM
				move	"NADDTST4",Location
				pack	KeyLocation,"Key: ",NADDFLD
				call	NADDTST
				if over
					move	"NADDWRT4",Location
					call	NADDWRT
				else
					write	output3,SEQ;LSTNUM,NADDVARS
				endif
			endif
		endif
.Update Subsidiary files
.Addressing
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE1
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE2
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE3
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE4
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE5
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE6
		clear	NADDPRICETemp
		clear	NADDDESCTemp
		call	WriteToAddressing using ADDCDE7
.Arrangement
		call	WriteToArrangement using ARRCODE1
		call	WriteToArrangement using ARRCODE2
		call	WriteToArrangement using ARRCODE3
.Category
		call	WriteToCategory using CATCDE1
		call	WriteToCategory using CATCDE2
		call	WriteToCategory using CATCDE3
		call	WriteToCategory using CATCDE4
		call	WriteToCategory using CATCDE5
		call	WriteToCategory using CATCDE6
		call	WriteToCategory using CATCDE7
		call	WriteToCategory using CATCDE8
		call	WriteToCategory using CATCDE9
		call	WriteToCategory using CATCDE10
.Media
		call	WriteToMedia using MAGSPEC1
		call	WriteToMedia using MAGSPEC2
		call	WriteToMedia using MAGSPEC3
.Source
		call	WriteToSource using SCODE1,SCODEP1
		call	WriteToSource using SCODE2,SCODEP2
		call	WriteToSource using SCODE3,SCODEP3
		call	WriteToSource using SCODE4,SCODEP4
		call	WriteToSource using SCODE5,SCODEP5
		call	WriteToSource using SCODE6,SCODEP6
		call	WriteToSource using SCODE7,SCODEP7
.Select
		call	WriteToSelect using SELCDE1,SEL1M
		call	WriteToSelect using SELCDE2,SEL2M
		call	WriteToSelect using SELCDE3,SEL3M
		call	WriteToSelect using SELCDE4,SEL4M
		call	WriteToSelect using SELCDE5,SEL5M
		call	WriteToSelect using SELCDE6,SEL6M
		call	WriteToSelect using SELCDE7,SEL7M
		call	WriteToSelect using SELCDE8,SEL8M
		call	WriteToSelect using SELCDE9,SEL9M
		call	WriteToSelect using SELCDE10,SEL10M
.Text
		if (RFlag = C1)
			call	Trim using TEXTData
			pack	TEXTData2,TEXTData,newline,"Format Charges:",RLINE1,newline,RLINE2,newline,RLINE3,newline,RLINE4,newline,RLINE5
			call	WriteToText using TEXTData2
		else
			call	WriteToText using TEXTData
		endif
.Add Email if not already there
		pack	NADDNUM,"025"
		pack	NADDLIST,LSTNUM
		clear	NADDPRICE
		clear	NADDDESC
		pack	NADDFLD,NADDLIST,NADDNUM
		move	"NADDTST-email",Location
		pack	KeyLocation,"Key: ",NADDFLD
		call	NADDTST
		if over
			move	"NADDWRT-email",Location
			call	NADDWRT
		endif
	repeat
	display    *p10:10,"records ",CounterVar,"  DONE!!"
	STOP

WriteToAddressing LRoutine DimPtr
	unpack	DimPtr,str1,str2
	call	Trim using str2
	if (str2 = "")
		return
	endif
	move	C0,N2
	move	str2,N2
	move	N2,str2
	rep	zfill,str2
	pack	NADDNUM,"0",str2
	pack	NADDLIST,LSTNUM
	clear	NADDPRICE
	clear	NADDDESC
	pack	NADDFLD,NADDLIST,NADDNUM
	move	"NADDKEY",Location
	pack	KeyLocation,"Key: ",NADDFLD
	call	NADDKEY
	if over
.Make sure entry was not created from RLINE fields
		pack	NADDNUM,"0",str2
		pack	NADDLIST,LSTNUM
		move	NADDPRICETemp,NADDPRICE
		move	NADDDESCTemp,NADDDESC
		pack	NADDFLD,NADDLIST,NADDNUM
		move	"NADDWRT",Location
		call	NADDWRT
	else
		if (NADDPRICE = C0)
			move	NADDPRICETemp,NADDPRICE
			move	NADDDESCTemp,NADDDESC
			move	"NADDUPD",Location
			call	NADDUPD
		endif
	endif
	return

WriteToArrangement LRoutine DimPtr
	unpack	DimPtr,str1,str2
	call	Trim using str2
	if (str2 = "")
		return
	endif
	move	C0,N2
	move	str2,N2
	move	N2,str2
	rep	zfill,str2
.Filter specific Arrangement Codes to Selection
	if (N2 = 10)
		move	"3",N2
		move	N2,str2
		rep	zfill,str2
		pack	str3,"L",str2
		call	WriteToSelect using str3
		return
	endif
	pack	NARRNUM,"0",str2
	pack	NARRLIST,LSTNUM
	pack	NARRFLD,NARRLIST,NARRNUM
	move	"NARRTST",Location
	pack	KeyLocation,"Key: ",NARRFLD
	call	NARRTST
	if over
		move	"NARRWRT",Location
		call	NARRWRT
	else
		write	output4,SEQ;LSTNUM,NARRVARS
	endif
	return

WriteToCategory LRoutine DimPtr
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
	read	input,DimPtr;str3,str2
	if over
		return
	endif
	unpack	str3,NCATCODE
	move	C0,N2
	move	str2,N2
	move	N2,NCATNUM
	rep	zfill,NCATNUM
	pack	NCATLIST,LSTNUM
	pack	NCATFLD,NCATLIST,NCATCODE,NCATNUM
	move	"NCATTST",Location
	pack	KeyLocation,"Key: ",NCATFLD
	call	NCATTST
	if over
		move	"NCATWRT",Location
		call	NCATWRT
	else
		write	output5,SEQ;LSTNUM,NCATVARS
	endif
	return

WriteToMedia LRoutine DimPtr
	unpack	DimPtr,str1,str2
	call	Trim using str2
	if (str2 = "")
		return
	endif
	move	C0,N2
	move	str2,N2
.Filter all Media Codes to Addressing
	if (N2 = 2)
		move	"29",N2
	elseif (N2 = 3)
		move	"30",N2
	elseif (N2 = 4)
		move	"31",N2
	elseif (N2 = 5)
		move	"32",N2
	elseif (N2 = 6)
		move	"33",N2
	elseif (N2 = 7)
		move	"34",N2
	elseif (N2 = 0)
		return
	endif
	move	N2,str2
	rep	zfill,str2
	pack	str3,"A",str2
	clear	NADDPRICETemp
	clear	NADDDESCTemp
	call	WriteToAddressing using str3
.	pack	NMEDNUM,"0",str2
.	pack	NMEDLIST,LSTNUM
.	pack	NMEDFLD,NMEDLIST,NMEDNUM
.	move	"NMEDWRT",Location
.	pack	KeyLocation,"Key: ",NMEDFLD
.	call	NMEDWRT
	return

WriteToSource LRoutine DimPtr,DimPtr2
	unpack	DimPtr,str1,str2
	call	Trim using str2
	if (str2 = "")
		return
	endif
	move	C0,N2
	move	str2,N2
	move	N2,str2
	rep	zfill,str2
.Filter specific Source Codes and skip them
	if (N2 = 61)		.Modem - does not make sense
		return
	elseif (N2 = 59)	.CD ROM - does not make sense
		return
	endif
	pack	NSRCNUM,"0",str2
	pack	NSRCLIST,LSTNUM
	pack	NSRCPER,DimPtr2
	pack	NSRCFLD,NSRCLIST,NSRCNUM
	move	"NSRCTST",Location
	pack	KeyLocation,"Key: ",NSRCFLD
	call	NSRCTST
	if over
		move	"NSRCWRT",Location
		call	NSRCWRT
	else
		write	output6,SEQ;LSTNUM,NSRCVARS
	endif
	return

WriteToSelect LRoutine DimPtr,DimPtr2
	unpack	DimPtr,str1,str2
	call	Trim using str2
	if (str2 = "")
		return
	endif
.Clear Percentage Var and Percentage Descriptor Var
	clear	NSLTPRICE
	clear	NSLTDESC
	call	Trim using DimPtr2
	if (DimPtr2 <> "")
		call	FilterIt using DimPtr,DimPtr2
	endif
	if (NSLTPRICE = C0 & NSLTDESC = "")
		return
	endif
	move	C0,N2
	move	str2,N2
	move	N2,str2
	rep	zfill,str2
.Filter specific Select Codes to Addressing
	if (N2 = 6)		.P.S. Labels
		move	"6",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 59)	.Diskette
		move	"16",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 53)	.5 1/4 diskette
		move	"17",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 54)	.3 1/2 diskette
		move	"18",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 65)	.Modem
		move	"24",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 66)	.Email
		move	"25",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 67)	.ASCII COMMA DELIMITED
		move	"26",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 68)	.FTP
		move	"27",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 55)	.Cartridge
		move	"21",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 56)	.1-up p.s.
		move	"15",N2
		call	WritetoAddressing2
		return
	elseif (N2 = 57)	.4-up p.s.
		move	"35",N2
		call	WritetoAddressing2
		return
	endif
	pack	NSLTNUM,"0",str2
	pack	NSLTLIST,LSTNUM
	pack	NSLTFLD,NSLTLIST,NSLTNUM
	move	"NSLTTST",Location
	pack	KeyLocation,"Key: ",NSLTFLD
	call	NSLTTST
	if over
		move	"NSLTWRT",Location
		call	NSLTWRT
	else
		write	output7,SEQ;LSTNUM,NSLTVARS
	endif
	return

WritetoAddressing2
	move	NSLTPRICE,NADDPRICETemp
	move	NSLTDESC,NADDDESCTemp
	move	N2,str2
	rep	zfill,str2
	pack	str3,"A",str2
	call	WriteToAddressing using str3
	return

FilterIt Routine DimPtr,DimPtr2
	move	DimPtr2,str7
	call	RemoveChar using DimPtr2,B1
	call	RemoveChar using DimPtr2,star
	move	"$",str1	
	call	RemoveChar using DimPtr2,str1
	rep	lowup,DimPtr2
	movelptr DimPtr2,N7
	movefptr DimPtr2,N6
	unpack	DimPtr2,str1
	type	str1
	if equal
		scan	SLASH,DimPtr2
		if equal
.Move this variable to Description variable
			movefptr DimPtr2,result
			move	result,N6
			sub	C1,result
			setlptr	DimPtr2,result
		endif
		reset	DimPtr2
		type	DimPtr2
		if not equal
.Write to exception file
			write	output2,SEQ;LSTNUM,DimPtr,str7
		else
			move	DimPtr2,str8
			scan	period,DimPtr2
			if equal
				movelptr DimPtr2,howmany	.Save it for later
				movefptr DimPtr2,result
				move	result,N9
				sub	C1,result
				setlptr	DimPtr2,result
				reset	DimPtr2
.Test first section of decimal var for length over 3 digits
				count	result,DimPtr2
				if (result > 3)
.Assume it is incorrect and write to Exception file
					write	output2,SEQ;LSTNUM,DimPtr,str7
				else
.Test fractional section of decimal var for length over 2 digits
					add	C1,N9
					reset	DimPtr2,N9
					setlptr DimPtr2,howmany
					count	result,DimPtr2
					if (result > 2)
.Assume it is incorrect and write to Exception file
						write	output2,SEQ;LSTNUM,DimPtr,str7
					else
						move	str8,NSLTPRICE
.
						reset	DimPtr2,N6
						setlptr	DimPtr2,N7
						call	TestVerbage
					endif
				endif
			else
				count	result,DimPtr2
				if (result > 3)
.Assume it is incorrect and write to Exception file
					write	output2,SEQ;LSTNUM,DimPtr,str7
				else
					move	str8,NSLTPRICE
.
					reset	DimPtr2,N6
					setlptr	DimPtr2,N7
					call	TestVerbage
				endif
			endif
		endif
	elseif (str1 = PERIOD)
		scan	SLASH,DimPtr2
		if equal
.Move this variable to Description variable
.			move	DimPtr2,?Var
			movefptr DimPtr2,result
			move	result,N6
			sub	C1,result
			setlptr	DimPtr2,result
		endif
		reset	DimPtr2
		bump	DimPtr2		.Make up for the Radix point
		type	DimPtr2
		if not equal
.Write to exception file
			write	output2,SEQ;LSTNUM,DimPtr,str7
		else
			move	DimPtr2,str8
.Test fractional section of decimal var for length over 2 digits
			count	result,DimPtr2
			if (result > 2)
.Assume it is incorrect and write to Exception file
				write	output2,SEQ;LSTNUM,DimPtr,str7
			else
				move	str8,NSLTPRICE
.
				reset	DimPtr2,N6
				setlptr	DimPtr2,N7
				call	TestVerbage
			endif
		endif
	else
.All Verbage - test for following values, and write to exception file if not found
		if (DimPtr2 = "N/A" | DimPtr2 = "N/C" | DimPtr2 = "SEEBASE" | DimPtr2 = "SEEDESC" | DimPtr2 = "INQUIRE" | DimPtr2 = "INQ" | DimPtr2 = "INQ.")
.Write to Descriptor Var
			if (DimPtr2 = "N/A")
				move	"005",NSLTDESC
			elseif (DimPtr2 = "N/C")
				move	"006",NSLTDESC
			elseif (DimPtr2 = "SEEBASE")
				move	"007",NSLTDESC
			elseif (DimPtr2 = "SEEDESC")
				move	"008",NSLTDESC
			elseif (DimPtr2 = "INQUIRE" | DimPtr2 = "INQ" | DimPtr2 = "INQ.")
				move	"009",NSLTDESC
			endif
		else
.Write to exception file
			write	output2,SEQ;LSTNUM,DimPtr,str7
		endif
	endif
	return

TestVerbage
.All Verbage - test for following values, and write to exception file if not found
	if (DimPtr2 = "/M" | DimPtr2 = "/FLT" | DimPtr2 = "/F" | DimPtr2 = "/FLAT" | DimPtr2 = "/FLATT" | DimPtr2 = "/FL" | DimPtr2 = "/EACH" | DimPtr2 = "EACH" | DimPtr2 = "/EA" | DimPtr2 = "/MIN")
.Write to Descriptor Var
		if (DimPtr2 = "/M")
			move	"001",NSLTDESC
		elseif (DimPtr2 = "/FLT" | DimPtr2 = "/F" | DimPtr2 = "/FLAT" | DimPtr2 = "/FLATT" | DimPtr2 = "/FL")
			move	"002",NSLTDESC
		elseif (DimPtr2 = "/EACH" | DimPtr2 = "EACH" | DimPtr2 = "/EA")
			move	"003",NSLTDESC
		elseif (DimPtr2 = "/MIN")
			move	"004",NSLTDESC
		endif
	else
.Write to exception file
		write	output2,SEQ;LSTNUM,DimPtr,str7
		clear	NSLTDESC
		clear	NSLTPRICE
	endif
	return

WriteToText LRoutine DimPtr
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
.Take out all the right padded spaces
	clear	TEXT1
	clear	TEXT2
	clear	TEXT3
	clear	TEXT4
	clear	TEXT5
	clear	TEXT6
	clear	TEXT7
	clear	TEXT8
	clear	TEXT9
	clear	TEXT10
	clear	TEXT11
	clear	TEXT12
	clear	TEXT13
	clear	TEXT14
	clear	TEXT15
	clear	TEXT16
	clear	TEXT17
	clear	TEXT18
	clear	TEXT19
	clear	TEXT20
	clear	TEXT21
	clear	TEXT22
	clear	TEXT23
	clear	TEXT24
	clear	TEXT25
	clear	TEXT26
	clear	TEXT27
	clear	TEXT28
	clear	TEXT29
	clear	TEXT30
	clear	TEXT31
	clear	TEXT32
	clear	TEXT33
	clear	TEXT34
	clear	TEXT35
	clear	TEXT36
	clear	TEXT37
	clear	TEXT38
	clear	TEXT39
	clear	TEXT40
	clear	TEXT41
	clear	TEXT42
	clear	TEXT43
	clear	TEXT44
	clear	TEXT45
	clear	TEXT46
	clear	TEXT47
	clear	TEXT48
	for howmany,"1","48"
		movefptr DimPtr,N9
		movelptr DimPtr,N8
		until (N8 = N9)
		call	PARSITUP using TEXTA,DimPtr,C1
		call	RemoveChar using TEXTA,newline
		call	RTrim using TEXTA
		pack	TEXTA,TEXTA,newline
		store	TEXTA,howmany,TEXT1,TEXT2,TEXT3,TEXT4,TEXT5,TEXT6,TEXT7,TEXT8,TEXT9,TEXT10:
			TEXT11,TEXT12,TEXT13,TEXT14,TEXT15,TEXT16,TEXT17,TEXT18,TEXT19,TEXT20:
			TEXT21,TEXT22,TEXT23,TEXT24,TEXT25,TEXT26,TEXT27,TEXT28,TEXT29,TEXT30:
			TEXT31,TEXT32,TEXT33,TEXT34,TEXT35,TEXT36,TEXT37,TEXT38,TEXT39,TEXT40:
			TEXT41,TEXT42,TEXT43,TEXT44,TEXT45,TEXT46,TEXT47,TEXT48
	repeat
	pack	DimPtr,TEXT1,TEXT2,TEXT3,TEXT4,TEXT5,TEXT6,TEXT7,TEXT8,TEXT9,TEXT10:
		TEXT11,TEXT12,TEXT13,TEXT14,TEXT15,TEXT16,TEXT17,TEXT18,TEXT19,TEXT20:
		TEXT21,TEXT22,TEXT23,TEXT24,TEXT25,TEXT26,TEXT27,TEXT28,TEXT29,TEXT30:
		TEXT31,TEXT32,TEXT33,TEXT34,TEXT35,TEXT36,TEXT37,TEXT38,TEXT39,TEXT40:
		TEXT41,TEXT42,TEXT43,TEXT44,TEXT45,TEXT46,TEXT47,TEXT48
	move	C0,N1
	clear	TextArray(1)
	clear	TextArray(2)
	clear	TextArray(3)
	clear	TextArray(4)
	clear	TextArray(5)
	unpack	DimPtr,TextArray(1),TextArray(2),TextArray(3),TextArray(4),TextArray(5)
	for	N9,C1,C5
		call	Trim using TextArray(N9)
		if (TextArray(N9) <> "")
			move	TextArray(N9),NTXTTEXT
			add	C1,N1
			move	N1,NTXTNUM
			rep	zfill,NTXTNUM
			pack	NTXTLIST,LSTNUM
			pack	NTXTFLD,NTXTLIST,NTXTNUM
			move	"NTXTWRT",Location
			pack	KeyLocation,"Key: ",NTXTFLD
			call	NTXTWRT
		endif
	repeat
	return

DREWTEST
	MOVE	STR1,STR1
	RETURN

	include	f:\library\include\ndatio.inc
	include	naddio.inc
	include	narrio.inc
	include	ntxtio.inc
	include	NSLTio.inc
	include	nsrcio.inc
	include	nmedio.inc
	include	ncatio.inc
	include	nownio.inc
	include	nfulio.inc
	include	comlogic.inc
PC	EQU	1
.......................................................
.......................................................
.	INT001B.PLS
.	Program1/Integral Upload Program
.	Andrew Harkins
.	September 13, 2004
.
.	Takes records updated in Program 1 (and others),
.	and notifies Integral of changes.
.......................................................
.......................................................
	include	common.inc
	include	cons.inc
.	include	f:\library\develop\backups\norddd.inc
.	include	f:\library\develop\backups\nloldd.inc
.	include	f:\library\develop\backups\nsel2dd.inc
	include	norddd.inc
	include	nloldd.inc
	include	nsel2dd.inc
	include	ncmpdd.inc
	include	ncmp2dd.inc
	include	nlol2dd.inc
.	include	nmlrdd.inc
	include	nowndd.inc
	include	nofrdd.inc
.	include	nbrkdd.inc
	include	nord4dd.inc
	include	nord5dd.inc
	include	npnddd.inc
	include	ninvdd.inc
	include	nmrgdd.inc
	include	nadddd.inc
	include	nsltdd.inc
	include	nrefdd.inc
	include	nmoddd.inc
.	include	f:\library\develop\backups\statsdd.inc
	include	statsdd.inc
	include	nsel3dd.inc
	include	nxchdd.inc
	include	nxngdd.inc
	include	compdd.inc
	include	cntdd.inc
	include	nprcdd.inc
	include	winapi.inc

release		init	"1.0"	13SEP2004	ASH	Initial Release

GetNetLast external "NORDTEST;OrderGetNetLast"
.
outfile		file
outfile1	file
outfile2	file
outfile3	file
DimPtr		dim	^
DimPtr1		dim	^
NMODDESChold	dim	20
PERCENT	form	4.2
CALCPER	form	7.4
nordin	form	8
nordout	form	8
DupeRate dim	6
timestamp2 dim	14	'yyyymmddhhmmss'
.hexeight integer 4,"4294967295"

mss1	plform	Error
	formload mss1

	call	GetWinVer
	call	Paint
.Create work var
	move	C1,NORDPATH
	move	C1,NLOLPATH
	move	C1,NCMPPATH
	move	C1,NSEL2PATH
	move	C1,NCMP2PATH
	move	C1,NLOL2PATH
.Clean-up:  We keep files for today and yesterday.  All others are deleted.  I actually delete for at least 5 days back, in case we run into an extended holiday.
	display	*P10:12,*EL,"Cleaning up Previous files"
	clear   str55
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe /c ",str55
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe /c ",str55
	else			.95/98
		append	"!c:\command.com /c ",str55
	endif
	append  "del \\nins1\e\data\integral\",str55	."
	reset	str55
.
	call	Trim using COMMENT
	if (COMMENT = "")
.START TESTING LOGIC
.		alert	note,"using timestamp",result
.END TESTING LOGIC
		clock	timestamp,timestamp2
	else
		move	COMMENT,timestamp2
	endif
.START TESTING LOGIC
.	pack	taskname,"\\nins1\e\data\integral\int",timestamp2,".dat"
.	prepare	outfile,taskname,exclusive
.	WRITE	OUTFILE,SEQ;"CAMPAIGN AND INTEGRAL FILE"
.	CLOSE	OUTFILE
..
.	pack	taskname,"\\nins1\e\data\integral\prc",timestamp2,".dat"
.	prepare	outfile,taskname,exclusive
.	WRITE	OUTFILE,SEQ;"PRICING FILE"
.	CLOSE	OUTFILE
..
.	pack	taskname,"\\nins1\e\data\integral\pkg2",timestamp2,".dat"
.	prepare	outfile,taskname,exclusive
.	WRITE	OUTFILE,SEQ;"Projection FILE"
.	CLOSE	OUTFILE
..
.	pack	taskname,"\\nins1\e\data\integral\exch",timestamp2,".dat"
.	prepare	outfile,taskname,exclusive
.	WRITE	OUTFILE,SEQ;"exchange stats FILE"
.	CLOSE	OUTFILE
.	shutdown "cls"
.END TESTING LOGIC
	unpack	timestamp2,CC,YY,MM,DD
	move	C0,N2
	move	DD,N2
	if (N2 < 7)
		if (MM <> "01")
			move	C0,N2
			move	MM,N2
			sub	C1,N2
			move	N2,MM
			rep	zfill,MM
		else
			move	C0,N2
			move	YY,N2
			sub	C1,N2
			move	N2,YY
			rep	zfill,YY
			move	"12",MM
		endif
		for N2,"31","23",SEQ
			pack	str45,CC,YY,MM,N2,STAR,".dat"
			rep	zfill,str45
			pack	taskname,str55,"int",str45
			execute	taskname
.			pack	taskname,str55,"Prc",str45
.			execute	taskname
.			pack	taskname,str55,"Pkg2",str45
.			execute	taskname
			pack	taskname,str55,"Exch",str45
			execute	taskname
		repeat
.Refresh the vars
		unpack	timestamp2,CC,YY,MM,DD
	endif
.
	move	C0,howmany
	move	DD,howmany
	sub	C2,howmany
	sub	C5,howmany,result
	for N2,howmany,result,SEQ
		until (N2 = C0)
		pack	str45,CC,YY,MM,N2,STAR,".dat"
		rep	zfill,str45
		pack	taskname,str55,"int",str45
		execute	taskname
.		pack	taskname,str55,"Prc",str45
.		execute	taskname
.		pack	taskname,str55,"Pkg2",str45
.		execute	taskname
.		pack	taskname,str55,"Exch",str45
.		execute	taskname
	repeat
.No backups are kept at all of these 2 files
	pack	taskname,str55,"Pkg2*.dat"
	execute	taskname
	pack	taskname,str55,"Exch*.dat"
	execute	taskname
.
	display	*P10:12,*EL,"Running Campaign Update"
.Campaign Cross Reference
	pack	taskname,"\\nins1\e\data\integral\int",timestamp2,".dat"
	prepare	outfile,taskname,exclusive
	clear	NCMP2FLD2
	clear	NCMP2FLD3
	pack	NCMP2FLD4,"03X1"	.Only look for updated records
	move	"NCMP2AIM",Location
	pack	KeyLocation,"Key: ",NCMP2FLD4
	call	NCMP2AIM
	loop
		until over
.Write out record for Integral
		pack	NCMPFLD,NCMP2NUM
		move	"NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if not over
			write	outfile,SEQ;"C":
					NCMP2INUM:
					NCMP2UDATE:
					NCMP2NUM:
					NCMPCNAME:
					NCMPMLR:
					NCMPSHIPTO:
					NCMPCNT:
					NCMPPLANNER:
					NCMPQTY:
					NCMPNETQTY:
					NCMPMDATE:
					NCMPRDATE:
					NCMPCDATE:
					NCMPCOMMENT
		endif
.Update Cross Reference File
		move	C0,NCMP2Upd
		move	"NCMP2UPD",Location
		pack	KeyLocation,"Key: ",NCMP2FLD4
		call	NCMP2UPD
.
  		move	"NCMP2KG",Location
		call	NCMP2KG
	repeat
	display	*P10:12,*EL,"Running Detail Update"
.Detail Cross Reference
..Create Other Files
.	pack	taskname,"\\nins1\e\data\integral\Prc",timestamp2,".dat"
.	prepare	outfile1,taskname,exclusive
.	pack	taskname,"\\nins1\e\data\integral\Pkg2",timestamp2,".dat"
.	prepare	outfile2,taskname,exclusive
	clear	NLOL2FLD1
	clear	NLOL2FLD2
	clear	NLOL2FLD3
	clear	NLOL2FLD4
	pack	NLOL2FLD5,"05X1"	.Only look for updated records
	move	"NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD5
	call	NLOL2AIM
	loop
		until over
.I immediately Update the record in case someone updates it while we are searching for other information.  This will prevent missed updates.
.Update Cross Reference File
		if (NLOL2Type2 = "3")	.Delete the record
.Deletion of the record here will screw up the Aim Read, so I will Delete after this run
.I update Deleted records here as I want to circumvent the problem where someone Deletes a record after it has
.been processed in this loop.
			move	"4",NLOL2Type2
		endif
.Need to read via Key info
		pack	NLOL2FLD,NLOL2Num,NLOL2INum,NLOL2Type
		move	"NLOL2TST",Location
		pack	KeyLocation,"Key: ",NLOL2FLD
		call	NLOL2TST
.
		move	C0,NLOL2Upd
		move	"NLOL2UPD",Location
		pack	KeyLocation,"Key: ",NLOL2FLD5
		call	NLOL2UPD
...Get Select information
		if (NLOL2Type = "0")		.LOL Record
			pack	NSEL2FLD,"2",NLOL2NUM
		else				.LCR/LR Record
			pack	NSEL2FLD,"1",NLOL2NUM
		endif
		move	"NSEL2KEY",Location
		pack	KeyLocation,"Key: ",NSEL2FLD
		call	NSEL2KEY
		if over
.This actually should never happen!!
			move	C0,NSEL2PRICE
			move	C0,NSEL2SPRICE
			if (NLOL2Type = "0")
				move	NLOLSelect,NSEL2NAME
			else
				move	OPPM,str3
				bump	OPPM BY	3
				move	OPPM,str2
				if (str2 = " 0"	OR str2	= "  ")
					pack	str2,"00"
				endif
				pack	str6,str3,PERIOD,str2
				type	str6
				if not equal
					clear	str6
				endif
				move	str6,NSEL2PRICE
				move	O2DES,NSEL2NAME
			endif
			move	"/m",NMODDESChold
			move	"XXXX",NSEL2NUM
		else
			pack	NMODFLD,NSEL2DESC
			rep	zfill,NMODFLD
			move	"NMODKEY",Location
			pack	KeyLocation,"Key: ",NMODFLD
			call	NMODKEY
			if over
				move	"/m",NMODDESChold
			else
				move	NMODDESC,NMODDESChold
			endif
		endif
...Do actual Write
		if (NLOL2Type = "0")		.LOL Record
			if (NLOL2Type2 = "4")
				move	"3",str1
			else
				move	"0",str1
			endif
			pack	NLOLFLD,NLOL2NUM
			move	"NLOLKEY",Location
			pack	KeyLocation,"Key: ",NLOLFLD
			call	NLOLKEY
			if (str1 <> "3" | NLOL2INUM <> "******************")		.Do not write out if a New record for Integral and it is already Deleted!!!
.Following Not Yet implemented!!!
.				if (str1 <> "3")
.					call	GetOtherFields using NLOLLOL,str1
.				else	.clear all those fields
.					call	ClearFields
.				endif
				write	outfile,SEQ;str1:
						NLOL2INUM:
						NLOL2NUM:
						NLOL2UDATE:
						NSEL2NAME:
						NSEL2NUM:
						NSEL2PRICE:
						NSEL2SPRICE:
						NMODDESChold:
						NLOLCNum:
						NLOLList:
						NLOLOwner:
						NLOLQty:
						NLOLNetQty:
						NLOLTest:
						NLOLRent:
						NLOLNet:
						NLOLNetReq:
						NLOLNetApp:
						NLOLMDate:
						NLOLComment
			endif
		else				.LCR/LR Record
			if (NLOL2Type2 = "4")
				move	"3",str1
			else
				move	"1",str1
			endif
			pack	NORDFLD,NLOL2NUM
			move	"NORDKEY",Location
			pack	KeyLocation,"Key: ",NORDFLD
			call	NORDKEY
			if (str1 <> "3" | NLOL2INUM <> "******************")		.Do not write out if a New record for Integral and it is already Deleted!!!
				if (str1 <> "3")
					call	GetOtherFields using OLRN,str1
				else	.clear all those fields
					call	ClearFields
				endif
				write	outfile,SEQ;str1:
						NLOL2INUM:
						NLOL2NUM:
						NLOL2UDATE:
						NSEL2NAME:
						NSEL2NUM:
						NSEL2PRICE:
						NSEL2SPRICE:
						NMODDESChold:
						OSTAT:
						OMLRNUM:
						OLNUM:
						OLON:
						OMLRPON:
						OQTY:
						OMLRKY:
						ORTNDTEC:
						ORTNDTEY:
						ORTNDTEM:
						ORTNDTED:
						OMDTEC:
						OMDTEY:
						OMDTEM:
						OMDTED:
						OELCODE:
						ONETQTY:
						OCAMP:
						OXPPM:
						ORTNNUM:
						OCOCODE:
						OCO2CODE:
						OODTEC:
						OODTEY:
						OODTEM:
						OODTED:
						OEXQTY:
						OBRKNUM:
						OBRKCNT:
						onetper:
						MCOMP:
						OWNOCPY:
						BRCOMP:
						str10:
						DupeRate:
						NPNDDESC:
						nmrgiqty:
						nmrgnet:
						AR:
						OTOCODE
			endif
		endif
.
  		move	"NLOL2KG",Location
		call	NLOL2KG
	repeat
	close	outfile
	close	outfile1
	close	outfile2
.Clean up Detail File
	display	*P10:12,*EL,"Running Detail Delete"
	clear	NLOL2FLD1
	clear	NLOL2FLD2
	clear	NLOL2FLD3
	pack	NLOL2FLD4,"04X4"	.Only look for records marked to be deleted
	clear	NLOL2FLD5
	move	"2-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD4
	call	NLOL2AIM
	loop
		until over
.
		pack	NLOL2FLD,NLOL2Num,NLOL2INum,NLOL2Type
		move	"2-NLOL2KEY",Location
		pack	KeyLocation,"Key: ",NLOL2FLD
		call	NLOL2KEY
.
		move	"2-NLOL2DEL",Location
		pack	KeyLocation,"Key: ",NLOL2FLD4
		call	NLOL2DEL
.
  		move	"NLOL2KG",Location
		call	NLOL2KG
	repeat
.Exchange File
.This logic should only run once a day.  This is governed
.by Exchlog, which holds one record containing:  YYYYMMDD
.To get this logic to happen more than once a day, just delete
.Exchlog.
	pack	taskname,"\\nins1\e\data\integral\Exchlog.dat"
	pack	APIFileName,taskname,hexzero
	call	FindFirstFile
	if (APIResult <> 0 & APIResult <> hexeight)
		open	outfile3,taskname,exclusive
		read	outfile3,SEQ;str9
		if not over
			unpack	timestamp2,str8
			if (str8 <> str9)
CreateExchangeFile
				close	outfile3
				erase	"\\nins1\e\data\integral\Exchlog.dat"
				pack	taskname,"\\nins1\e\data\integral\Exchlog.dat"
				prep	outfile3,taskname,exclusive
				unpack	timestamp2,str8
				write	outfile3,SEQ;str8
				close	outfile3
.Create Exchange File
				pack	taskname,"\\nins1\e\data\integral\Exch",timestamp2,".dat"
				prepare	outfile3,taskname,exclusive
				display	*P10:10,*EL,"Creating ExchFile.dat"
				move	C0,howmany
				loop
					pack	Location,"NXNGSEQ"
					call	NXNGSEQ
					until over
					add	C1,howmany
					display	*p10:12,"Exchange records ",howmany
					clear	NXCHFLD1
					pack	NXCHFLD1,ACCKEY,ENTRY
					rep	ZFILL,NXCHFLD1
					move	"NO BALANCE ",ERROR
					move	C1,NXCHPATH
					pack	Location,"NXCHKEY"
					call	NXCHKEY
.					if not over
						write	outfile3,SEQ;ACCKEY:
							nxngdate:
							ENTRY:
							Flag:
							Usage1:
							Usage2
.					endif
				repeat
				close	outfile3
			endif
		endif
	else
		goto CreateExchangeFile
	endif
	shutdown "cls"

GetOtherFields LRoutine DimPtr,DimPtr1
.DimPtr  = LOL/LR Number
.DimPtr1 = "0" = LOL, "1" = LR, "3" = Deleted record
.Option to call for additional fields for LOL record currently NOT implemented!!!
.It would seem silly to do all the extra calculation for a temporary record.
	if (DimPtr1 = "3")
.Should never happen!!!  Calling routine should filter this value!!!!
		call	ClearFields
		return
	elseif (DimPtr1 = "0")	.LOL
.Not yet implemented!!!!
	else			.LR
		pack	MKEY,OMLRNUM,"000"
		rep	zfill,MKEY
		move	"NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY
		if over
			clear	MCOMP
		endif
.
		call	Trim using OLON
		if (OLON <> "")
			pack	NOWNFLD,OLON
			rep	zfill,NOWNFLD
			move	"NOWNKEY",Location
			pack	KeyLocation,"Key: ",NOWNFLD
			call	NOWNKEY
		else
			clear	OWNOCPY
		endif
.
		bump	OODNUM,4
		pack	NOFRFLD,OMLRNUM,OODNUM
		rep	zfill in NOFRFLD
		move	"NOFRKEY",Location
		pack	KeyLocation,"Key: ",NOFRFLD
		call	NOFRKEY
		if over
			clear	OFDESC
		endif
.
		pack	NBRKFLD,OBRKNUM,OBRKCNT
		rep	zfill in NBRKFLD
		if (NBRKFLD <> "0000000")
			move	"NBRKKEY",Location
			pack	KeyLocation,"Key: ",NBRKFLD
			call	NBRKKEY
			if over
				clear	BRCOMP
			endif
		else
			clear	BRCOMP
		endif
.
		call	CalcNet
		clear	DupeRate
		clear	DupeRate
		call	GetNetLast using OMLRNUM,OLNUM,DupeRate
		clear	NPNDDESC	.Initialize value
		if (OSTAT = "p" | OSTAT = "x")
			move	OLRN,NORD4FLD
			if (NORD4FLD <>	"")
				rep	zfill in NORD4FLD
				clear	NORD4STAT
				move	"NORD4KEY",Location
				pack	KeyLocation,"Key: ",NORD4FLD
				call	NORD4KEY		.get Pending Order info
				if not over
					if (OSTAT = "p"	| OSTAT	= "x")
						move	"p",str2
					endif
					pack	NPNDFLD	from str2,NORD4STAT
					rep	zfill in NPNDFLD
					move	"NPNDKEY",Location
					pack	KeyLocation,"Key: ",NPNDFLD
					call	NPNDKEY
					if over
						clear	NPNDDESC
					endif
				endif
			endif
		elseif (OSTAT = "l" | OSTAT = "z")
			move	OLRN,NORD5FLD
			if (NORD5FLD <>	"")
				rep	zfill in NORD5FLD
				clear	NORD5STAT
				move	"NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY		.get Pending Order info
				if not over
					move	"l",str2
					pack	NPNDFLD	from str2,NORD5STAT
					rep	zfill in NPNDFLD
					move	"NPNDKEY",Location
					pack	KeyLocation,"Key: ",NPNDFLD
					call	NPNDKEY
					if over
						clear	NPNDDESC
					endif
				endif
			endif
		endif
.
		packkey	NINVFLD,OLRN
		rep	zfill,NINVFLD
		move	c1,NINVPATH
		move	"NINVKEY",Location
		pack	KeyLocation,"Key: ",NINVFLD
		call	NINVKEY
	endif
.	call	GetOtherFiles using DimPtr,DimPtr1
	return

ClearFields LRoutine DimPtr
	clear	onetper
	clear	MCOMP
	clear	OWNOCPY
	clear	BRCOMP
	clear	str10
	clear	DupeRate
	clear	NPNDDESC
	clear	nmrgiqty
	clear	nmrgnet
	clear	AR
	return

GetOtherFiles LRoutine
.DimPtr  = LOL/LR Number
.DimPtr1 = "0" = LOL, "1" = LR, "3" = Deleted record
.Pulls Package and Price information
	if (DimPtr1 = "3")
.Safety check!!  Should never get here.
		return
	elseif (DimPtr1 = "0")		.LOL
		pack	NSEL3FLD1,"01X2",DimPtr
		pack	STAT2FLD2,"01X",DimPtr
		pack	STAT2FLD3,"02X1"
		pack	NCMPFLD,NLOLCNum
	else				.LR
		pack	NSEL3FLD1,"01X1",DimPtr
		pack	STAT2FLD2,"01X",DimPtr
		pack	STAT2FLD3,"02X0"
		pack	NCMPFLD,OCAMP
	endif
	move	"NSEL3AIM",Location
	pack	KeyLocation,"Key: ",NSEL3FLD1
	call	NSEL3AIM
	loop
		until over
		if (NSEL3CODE = "A")
			pack	NADDFLD,OLNUM,NSEL3NUM
			move	"NADDKEY",Location
			pack	KeyLocation,"Key: ",NADDFLD
			call	NADDKEY
			if not over
				pack	NREFFLD,"A",NADDNUM
				pack	NMODFLD,NADDDESC
			endif
		elseif (NSEL3CODE = "L")
			pack	NSLTFLD,OLNUM,NSEL3NUM
			move	"NSLTKEY",Location
			pack	KeyLocation,"Key: ",NSLTFLD
			call	NSLTKEY
			if not over
				pack	NREFFLD,"L",NSLTNUM
				pack	NMODFLD,NSLTDESC
			endif
		endif
		move	"NREFKEY",Location
		pack	KeyLocation,"Key: ",NREFFLD
		call	NREFKEY
.
		rep	zfill,NMODFLD
		move	"2-NMODKEY",Location
		pack	KeyLocation,"Key: ",NMODFLD
		call	NMODKEY
		if over
			move	"/m",NMODDESC
		endif
		write	outfile1,SEQ;DimPtr1:
				NSEL3LR:
				NSEL3CODE:
				NSEL3NUM:
				NSEL3PRICE:
				NREFDESC:
				NMODDESC
		move	"NSEL3KG",Location
		call	NSEL3KG
	repeat
	move	C1,STATPATH
	move	"STAT2AIM",Location
	pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
	call	STAT2AIM
	loop
		until over
		move	"NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if not over
			if (NCMPRPT = "3")
.NWF template uses last	Price Total always
				move	C0,N6
				move	C0,STATPCKM
.Cannot	use N5 as that is what CVTJUL uses!!
				pack	NPRCFLD1,"01X",NCMPMLR
				pack	NPRCFLD2,"02X",STATPCKNUM
				move	"NPRCAIM",Location
				pack	KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
				call	NPRCAIM
				loop
					until over
					move	C0,JULDAYS
					call	Trim using NPRCDATE
					if (NPRCDATE <>	"")
						unpack	NPRCDATE,CC,YY,MM,DD
						call	cvtjul
					endif
					if (JULDAYS > N6)
						move	JULDAYS,N6
						move	NPRCTOTAL,STATPCKM
					endif
					move	"NPRCKG",Location
					call	NPRCKG
				repeat
			else
				move	C0,JULDAYS
				unpack	STATPCKM,str5,str3
				call	Trim using str5
				move	str5,JULDAYS
				call	CVTGREG
				if (YY < "80")
					move	"20",CC
				else
					move	"19",CC
				endif
				move	C1,NPRCPATH
				pack	NPRCFLD,NCMPMLR,STATPCKNUM,CC,YY,MM,DD
				move	"NPRCKEY",Location
				pack	KeyLocation,"Key: ",NPRCFLD
				call	NPRCKEY
				if not over
					move	NPRCTOTAL,STATPCKM
				else
					move	C0,STATPCKM
				endif
			endif
			write	outfile2,SEQ;DimPtr1:
					statlr:
					STATPCKNUM:
					STATPCKM
		endif
		move	"STAT2KG",Location
		call	STAT2KG
	repeat
	return

CalcNet
	clear	str10
	move	C0,nordin
	move	C0,nordout
	move	C0,calcper
	move	C0,percent
	move	OLRN,NMRGFLD
	move	"NMRGKEY",Location
	pack	KeyLocation,"Key: ",NMRGFLD
	call	NMRGKEY
	if not over
		add	NMRGIQTY,nordin
		add	NMRGNET,nordout
		compare	C0,nordout
		if not equal
			move	C0,CALCPER
			move	nordout,CALCPER
			divide	nordin,CALCPER
			mult	"100",CALCPER
			move	C0,PERCENT
			add	CALCPER,PERCENT
			move	PERCENT,str10
		endif
	endif
	return

	include nordio.inc
	include nlolio.inc
	include nsel2io.inc
	include ncmpio.inc
	include ncmp2io.inc
	include nlol2io.inc
.	include	nmlrio.inc
	include	nownio.inc
	include	nofrio.inc
.	include	nbrkio.inc
	include	nord4io.inc
	include	nord5io.inc
	include	npndio.inc
	include	ninvio.inc
	include	nmrgio.inc
	include	naddio.inc
	include	nsltio.inc
	include	nrefio.inc
	include	nmodio.inc
	include	statsio2.inc
	include	nsel3io.inc
	include	nxchio.inc
	include	nxngio.inc
	include	compio.inc
	include	cntio.inc
	include	nprcio.inc
	include comlogic.inc

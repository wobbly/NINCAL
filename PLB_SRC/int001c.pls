.......................................................
.......................................................
.	INT001C.PLS
.	Program1/Integral Download Program
.	Andrew Harkins
.	September 13, 2004
.
.	Takes input file from Integral of records submitted by
.       INT001B.PLS that need to have an Integral ID assigned.
.
.       Input file will contain new Integral IDs.  Updates Cross
.       Reference records.
.
.       Updates Integral if records are:  Invalid, Dupes, Deleted
.......................................................
.......................................................
	include	common.inc
	include	cons.inc
.	include	f:\library\develop\backups\nloldd.inc
.	include	f:\library\develop\backups\norddd.inc
	include	nloldd.inc
	include	norddd.inc
	include	ncmpdd.inc
	include	nlol2dd.inc
	include	ncmp2dd.inc
	include	winapi.inc

Release	init	"1.0" ASH	Initial Release
.
outfile	file	.Used to flag Integral when we need to bounce back a record
.
IntIDVars	list
IntIDType	dim	1
IntIDNIN	dim	6
IntIDInt	dim	18
		listend
DimPtr		dim	^
.hexeight integer 4,"4294967295"
timestamp2 dim	14	'yyyymmddhhmmss'
.
	call	GetWinVer
	call	Paint
.
	move	C1,NORDPATH
	move	C1,NLOLPATH
	move	C1,NCMPPATH
	move	C2,NCMP2PATH
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
	append  "del \\nins1\e\data\integral\",str55
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
			pack	taskname,str55,"idint",str45
			execute	taskname
			pack	taskname,str55,"rejint",str45
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
		pack	taskname,str55,"idint",str45
		execute	taskname
		pack	taskname,str55,"rejint",str45
		execute	taskname
	repeat
.
	display	*P10:12,*EL,"Preparing Integral ID Update File"
	pack	APIFileName,"\\nins1\e\data\integral\idint.dat",hexzero
	call	FindFirstFile
	if (APIResult <> 0 & APIResult <> hexeight)
		pack	taskname,"\\nins1\e\data\integral\rejint",timestamp2,".dat"
		prepare	outfile,taskname
.
		pack	taskname,"\\nins1\e\data\integral\idint",timestamp2,".dat"
		rename	"\\nins1\e\data\integral\idint.dat",taskname
.START TESTING LOGIC
.	write	outfile,SEQ;"Invalid ID"
.	CLOSE	OUTFILE
.	SHUTDOWN "CLS"
.END TESTING LOGIC
		display	*P10:12,*EL,"Reading Integral ID Update File"
		open	tempfile,taskname
		loop
			read	tempfile,SEQ;IntIDVars
			until over
			call	Trim using IntIDInt
			if (IntIDInt = "******************" | IntIDInt = "")
				write	outfile,SEQ;IntIDVars:
					"Invalid ID"
			else
				if (IntIDType = "C")
					pack	NCMPFLD,IntIDNIN
					move	"NCMPTST",Location
					pack	KeyLocation,NCMPFLD
					call	NCMPTST
					if over
						call	WriteDelete
					else
						move	C2,NCMP2PATH
						pack	NCMP2FLD1,IntIDInt
						move	"NCMP2KEY",Location
						pack	KeyLocation,NCMP2FLD1
						call	NCMP2KEY
						if not over
							call	WriteDupe using NCMP2INum
						else
							call	UpdateRecord
						endif
					endif
				elseif (IntIDType = "0")	.LOL
					pack	NLOLFLD,IntIDNIN
					move	"NLOLKEY",Location
					pack	KeyLocation,NLOLFLD
					call	NLOLKEY
					if over
						call	WriteDelete
					else
						clear	NLOL2FLD1
						clear	NLOL2FLD3
						pack	NLOL2FLD2,"02X",IntIDInt
						move	"NLOL2AIM",Location
						pack	KeyLocation,NLOL2FLD2
						call	NLOL2AIM
						if not over
							call	WriteDupe using NLOL2INum
						else
							call	UpdateRecord
						endif
					endif
				elseif (IntIDType = "1")	.LR
					pack	NORDFLD,IntIDNIN
					move	"NORDKEY",Location
					pack	KeyLocation,NORDFLD
					call	NORDKEY
					if over
						call	WriteDelete
					else
						clear	NLOL2FLD1
						clear	NLOL2FLD3
						pack	NLOL2FLD2,"02X",IntIDInt
						move	"2-NLOL2AIM",Location
						pack	KeyLocation,NLOL2FLD2
						call	NLOL2AIM
						if not over
							call	WriteDupe using NLOL2INum
						else
							call	UpdateRecord
						endif
					endif
				endif
			endif
		repeat
	endif
	close	outfile
	close	tempfile
	shutdown "cls"

WriteDelete
	write	outfile,SEQ;IntIDVars:
		"Delete"
	return

WriteDupe Lroutine DimPtr
	write	outfile,SEQ;IntIDVars:
		"Duplicate":
		DimPtr
	return

UpdateRecord
	if (IntIDType = "C")
		move	C1,NCMP2PATH
		pack	NCMP2FLD,IntIDNIN
		move	"2-NCMP2KEY",Location
		pack	KeyLocation,NCMP2FLD1
		call	NCMP2KEY
		if over
.Currently do not allow MM Campaigns to be established by Program 1
			write	outfile,SEQ;IntIDVars:
				"Delete-Invalid Campaign"
		else
			move	IntIDInt,NCMP2INum
			move	"NCMP2UPD",Location
			call	NCMP2UPD
		endif
	elseif (IntIDType = "0" | IntIDType = "1")	.LOL/LR
		pack	NLOL2FLD1,"01X",IntIDNIN
		clear	NLOL2FLD2
		pack	NLOL2FLD3,"03X",IntIDType
		move	"3-NLOL2AIM",Location
		pack	KeyLocation,NLOL2FLD1,COMMA,NLOL2FLD3
		call	NLOL2AIM
		if over
			write	outfile,SEQ;IntIDVars:
				"Delete-Invalid LOL/LR"
		else
			pack	NLOL2FLD,NLOL2Num,NLOL2INum,NLOL2Type
			move	"NLOL2TST",Location
			pack	KeyLocation,NLOL2FLD
			call	NLOL2TST
.
			move	IntIDInt,NLOL2INum
			move	"NLOL2UPD",Location
			call	NLOL2UPD
		endif
	endif
	return

	include	nordio.inc
	include	nlolio.inc
	include	ncmpio.inc
	include	nlol2io.inc
	include	ncmp2io.inc
	include	comlogic.inc

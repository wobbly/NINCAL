PC	EQU	1
.Include Files
	include common.inc
	include F:\LIBRARY\DEVELOP\DStrahan\catcodes\NCATCODEDD.inc
	include cons.inc


Release		INIT	"1.0"
holdVar		dim	528
tempfile2	file
namefill	dim	73
basefill	dim	40
typehold	dim	12
broadmar	dim	27
submar		dim	43
environ		dim	33
polit		dim	33
health		dim	29
progr		dim	33

mss1	plform	Error
	formload mss1
.
	erase	"c:\work\CategoryNEW.dat"
	erase	"c:\work\Category.prn"
	copyfile "f:\library\develop\listmarketsproject\mkt desig 6-16-06_2.prn","c:\work\Category.prn"
	prep	tempfile2,"c:\work\CategoryNEW.dat",exclusive
	open	tempfile,"c:\work\Category.prn",exclusive
	call	Paint
	loop
dave		read	tempfile,SEQ;holdVar
		until over
.		clear 	vars and get data!
		add	C1,howmany
		display	*p10:12,"records processed : ",howmany
		unpack	holdvar,NCATCODENUM,namefill,basefill,typehold,broadmar,submar,environ,polit,health,progr,STR15
		call TRIM using typehold
		if (typehold = "Byrs/Subs")
			move "B", NCATCODETYPE
		elseif (typehold = "Dnrs/Mbrs")
			move "D", NCATCODETYPE
		elseif (typehold = "Compiled")
			move "C", NCATCODETYPE
		elseif (typehold = "Misc.")
			move "M", NCATCODETYPE
		else 
			clear NCATCODETYPE
		endif
		move broadmar,	NCATCODEMARKET
		move submar, 	NCATCODESUBMARK
		move environ, 	NCATCODEENV
		move polit, 	NCATCODEPOLIT
		move health,	NCATCODEHEALTH
		move progr,	NCATCODEPROG
		call TRIM using str15
		if (str15 = "Christian")
			move "C", NCATCODEFOCUS
		elseif (str15 = "Veterans")
			move "V", NCATCODEFOCUS
		elseif (str15 = "Pets")
			move "P", NCATCODEFOCUS
		elseif (str15 = "Women")
			move "W", NCATCODEFOCUS
		elseif (str15 = "Children")
			move "K", NCATCODEFOCUS
		elseif (str15 = "Seniors")
			move "S", NCATCODEFOCUS
		elseif (str15 = "Gay/Lesbian")
			move "G", NCATCODEFOCUS
		elseif (str15 = "Jewish")
			move "J", NCATCODEFOCUS
		else
			clear NCATCODEFOCUS
		endif
		call TRIM using NCATCODENUM
		call zfillit using NCATCODENUM
		write	tempfile2,SEQ;NCATCODEVARS
	repeat

	close 	tempfile
	close 	tempfile2
	shutdown
.
	include	comlogic.inc
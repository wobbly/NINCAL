	include	common.inc
	include	cons.inc

release		init	"1.1"	09March2006	DMS	ADDED mung by name and domain
.release	init	"1.0"	05JAN2006 ASH	INITIAL RELEASE

taskname2	dim	1000
taskname3	dim	1000
DimPtr		dim	^
DimPtr1		dim	^
hold		dim	100
hold2		dim	100
name		dim	500 to hold munged name with formatting
.
.START PATCH 1.1 ADDED LOGIC
white 	color
grey 	color
	create	white=*white
	create	grey=220:220:220
.END PATCH 1.1 ADDED LOGIC

MaxValue	const	"100"
MungARR		dim	3(MaxValue,2),("32"),(" "):
				("33"),("!"):
				("36"),("$"):
				("37"),("%"):
				("38"),("&"):
				("39"),("'"):
				("40"),("("):
				("41"),(")"):
				("42"),("*"):
				("43"),("+"):
				("44"),(","):
				("45"),("-"):
				("46"),("."):
				("47"),("//"):
				("48"),("0"):
				("49"),("1"):
				("50"),("2"):
				("51"),("3"):
				("52"),("4"):
				("53"),("5"):
				("54"),("6"):
				("55"),("7"):
				("56"),("8"):
				("57"),("9"):
				("58"),(":"):
				("59"),(";"):
				("60"),("<"):
				("61"),("="):
				("62"),(">"):
				("63"),("?"):
				("64"),("@"):
				("65"),("A"):
				("66"),("B"):
				("67"),("C"):
				("68"),("D"):
				("69"),("E"):
				("70"),("F"):
				("71"),("G"):
				("72"),("H"):
				("73"),("I"):
				("74"),("J"):
				("75"),("K"):
				("76"),("L"):
				("77"),("M"):
				("78"),("N"):
				("79"),("O"):
				("80"),("P"):
				("81"),("Q"):
				("82"),("R"):
				("83"),("S"):
				("84"),("T"):
				("85"),("U"):
				("86"),("V"):
				("87"),("W"):
				("88"),("X"):
				("89"),("W"):
				("90"),("Z"):
				("91"),("["):
				("92"),("\"):
				("93"),("]"):
				("94"),("^"):
				("95"),("_"):
				("96"),("`"):
				("97"),("a"):
				("98"),("b"):
				("99"),("c"):
				("100"),("d"):
				("101"),("e"):
				("102"),("f"):
				("103"),("g"):
				("104"),("h"):
				("105"),("i"):
				("106"),("j"):
				("107"),("k"):
				("108"),("l"):
				("109"),("m"):
				("110"),("n"):
				("111"),("o"):
				("112"),("p"):
				("113"),("q"):
				("114"),("r"):
				("115"),("s"):
				("116"),("t"):
				("117"),("u"):
				("118"),("v"):
				("119"),("w"):
				("120"),("x"):
				("121"),("y"):
				("122"),("z"):
				("123"),("{"):
				("124"),("|"):
				("125"),("}"):
				("126"),("~")
."
x	plform	Mung
	winhide
	formload x
.START PATCH 1.1 ADDED LOGIC
	call EnableUpper	// set up for initial address mung
	call DisableLower
	MungWebBrowser.Navigate2 USING "about:blank"
	erase	"c:\work\Mung.html"
.END PATCH 1.1 ADDED LOGIC
	loop
		eventwait
	repeat

MungRetrieve
	move	B55,taskname2
	clear	taskname2
	
.START PATCH 1.1 ADDED LOGIC
	clear 	name
	getitem MungCheckDomain, 0, N1
	if ( N1 = 1 )  // get name and domain fields
		setitem	MungEditNewEmail,0,""  // clear
		getitem MungEditName,0,hold
		call 	TRIM	using hold
		getitem MungEditDomain,0,hold2
		call 	TRIM	using hold2
		pack taskname, hold, "@",hold2
		call   	RemoveChar 	using taskname,B1
		call 	TRIM 	using taskname
		// taskname if full name for link
		// hold contains name with formatting
		// now, mung "hold", and store in name
		move	B1,str1
		count	howmany,hold
		for result,"1",howmany
			reset	hold,result
			cmove	hold,str1
			call	MungConvert using str1,str5
			if (str5 <> "")
				append	"&##",name
				append	str5,name
				append	";",name
			endif
		repeat
		reset	name	
	else //   use complete address (top input box)
.END PATCH 1.1 ADDED LOGIC
		setitem	MungEditNewEmail,0,""
		getitem	MungEditOldEmail,0,taskname
		call	Trim using taskname
.START PATCH 1.1 ADDED LOGIC
	endif  
.END PATCH 1.1 ADDED LOGIC
dave	if (taskname <> "")
.		append	"<A HREF=/"",taskname	."
		move	B1,str1
		count	howmany,taskname
		for result,"1",howmany
			reset	taskname,result
			cmove	taskname,str1
			call	MungConvert using str1,str5
back
			if (str5 <> "")
				append	"&##",taskname2
				append	str5,taskname2
				append	";",taskname2
			endif
		repeat
		reset	taskname2
		getitem	MungCheckLink,0,N1
		if (N1 = 1)
			clear	taskname3
			append	"<A HREF=#"&##109;&##97;&##105;&##108;&##116;&##111;&##58;",taskname3	."
			append	taskname2,taskname3
			append	"#">",taskname3								."
.START PATCH 1.1 ADDED LOGIC
			getitem MungCheckDomain, 0, N1  // if user entered name and domain
			if (N1 = 1)
				append name, taskname3  // append munged, formatted name
			else
.END PATCH 1.1 ADDED LOGIC
				append	taskname2,taskname3
.START PATCH 1.1 ADDED LOGIC
			endif
.END PATCH 1.1 ADDED LOGIC
			append	"</A>",taskname3
			reset	taskname3
			setitem	MungEditNewEmail,0,taskname3
		else
		
.START PATCH 1.1 ADDED LOGIC
			getitem MungCheckDomain, 0, N1  // if user entered name and domain
			if (N1 = 1)
				setitem MungEditNewEmail,0,name
			else		
.END PATCH 1.1 ADDED LOGIC
			setitem	MungEditNewEmail,0,taskname2
.START PATCH 1.1 ADDED LOGIC
			endif
.END PATCH 1.1 ADDED LOGIC
		endif
	endif
.START PATCH 1.1 ADDED LOGIC
	
	getitem	MungEditNewEmail,0,taskname2
	erase	"c:\work\Mung.html"
	prep	tempfile,"c:\work\Mung.html"
	write	tempfile,SEQ;taskname2
	close	tempfile
	MungWebBrowser.Navigate2 USING "c:\work\Mung.html"
daveq	reset taskname
	clear taskname2
	getitem MungCheckLink, 0,N1
	if (N1 = 1)
		pack taskname2, "mailto:",taskname
	else 
		pack taskname2, taskname
	endif
	setprop	MungStatStatusBar,text=TASKNAME2
.END PATCH 1.1 ADDED LOGIC
	return
	

MungConvert Routine DimPtr,DimPtr1
	clear	DimPtr1
	for N3,C1,MaxValue
		if (MungARR(N3,2) = DimPtr)
			move	MungARR(N3,1),DimPtr1
			break
		endif
	repeat
	return

.START PATCH 1.1 ADDED LOGIC

EnableUpper
	setitem MungEditOldEmail,0,""
	setprop MungEditOldEmail, enabled=1,bgcolor=white
	setprop MungCheckLink, enabled=1
	return

DisableUpper
	setprop MungEditOldEmail, enabled=0, bgcolor=grey
	setprop MungCheckLink, enabled=0
	return
EnableLower
	setitem MungCheckLink, 0,C1  // lower will always be a linked address
	setitem MungEditName,0,""
	setprop MungEditName, enabled=1, bgcolor=white
	setprop MungEditDomain, enabled=1, bgcolor=white
	return

DisableLower
	setitem MungCheckLink, 0,C0  // uncheck link
	setprop MungEditName, enabled=0,bgcolor=grey
	setprop MungEditDomain, enabled=0,bgcolor=grey
	return

.END PATCH 1.1 ADDED LOGIC

	include	comlogic.inc
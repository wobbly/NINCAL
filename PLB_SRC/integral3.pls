.SELECT RECONCILIATION PROGRAM FOR INTEGRAL
........................................
. Program:	Integral3.PLS
. Function:	Uses algorithm to try to match Selects under a particular List that may be spelled a little differently
.		This program can be stand alone, but will be called by another application (INTEGRAL.PLS)
.		Assumes Input file has been properly created
.		Sorts Input file before running routine
.		Creates Export file called:  c:\work\lrfile.new
. Author:	Andrew Harkins
. Orig.	Date:	February 22, 2005
. Release:	1.0
........................................
	include	common.inc
	include	cons.inc

release	init	"1.0"		ASH	Initial Release
revdate	init	"February 22, 2005"

tempfile2 file
result1	form	9
str75	dim	75
str75A	dim	75
str35a	dim	35
str35b	dim	35
str35c	dim	35
.Length is Total for LRFile (446) plus a bit extra
Hold	dim	500
HoldA	dim	501
HoldB	dim	446
Hold2	dim	366
TestCode dim	1
.
KeyValue dim	6
.
DimPtr	dim	^
DimPtr2	dim	^
DimPtr3	dim	^
DimPtr4	dim	^
DataPtr	Datalist ^
.
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE	variant
OFALSE	variant
.
wdWindowStateMinimize integer 4,"0x00000002"
wdDoNotSaveChanges integer 4,"0x00000000"
wdReplaceAll integer 4,"0x00000002"
.
.MS Word logic slows down unexpectedly, commenting it all out 6/3/05 ASH
.WordObj	  Automation
.DocsObj	  Automation
.DocObj	  Automation
.RangeObj  Automation
.RangeObj2 Automation
.
PackData DataList
PackData2 DataList
PackData3 DataList
ListHold dim	6
howmany5 form	9
howmany6 form	9
howmany7 form	9

Integral3Run Routine
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
	call	Paint
	display	*P10:10,*EL,"Running INTEGRAL3.PLS - to clean up Select Names!"
	display	*P10:12,*EL,"Sorting LRFile.dat"
.For testing
.	goto secondtest
.Sort Input file created by INTEGRAL.PLS - Sort by List Number
	pack	taskname,"c:\work\lrfile.dat,c:\work\lrfile.tmp;12-17"
	sort	taskname
.Open newly created file
	open	tempfile,"c:\work\lrfile.tmp",exclusive
	erase	"c:\work\lrfile.new"
	prepare	tempfile2,"c:\work\lrfile.new",exclusive
.First Loop is for Substitutions/cleanup
	move	C0,N11
	display	*P10:12,*EL,"First cleanup Loop"
	loop
.*******************************************
.***************LRFile.dat******************
.*******************************************
.Order Status			1	Text
.Mailer Number			4	Numeric
.LR Number (Key)		6	Numeric		 6-11
.List Number			6	Numeric		12-17
.List Owner Number		4	Numeric
.Mailer Purchase Order Number	12	Text
.Order Quantity			9	Numeric
.Mailer Key			12	Text
.Return Date			8	Numeric
.Mail Date			8	Numeric
.Entire/Rent/Exchange Code	1	Numeric
.Order Net Quantity		9	Numeric
.Order Campaign Number		6	Numeric
.*Exchange Price per Thousand	5	Numeric
.Return To Company Number	4	Numeric
.NINCA Contact Code		2	Numeric
.NINCA Caller Code		2	Numeric
.Order Date			8	Numeric
.Order Exchange Quantity	9	Numeric
.Broker Number			4	Numeric
.Broker Contact Number		3	Numeric
.Net Name Percentage		2	Numeric
.Mailer Name			45	Text
.Owner Name			25	Text
.Broker Name			45	Text
.Merge Percentage		10	Numeric
.Last Merge % for Mlr/List	6	Numeric
.LCR/Pending Order SubStatus	45	Text
.Gross Billed Quantity		8	Numeric
.Net Merge Quantity		8	Numeric
.Total Billed (AR)		10.2	Numeric
.Base Price			5.2	Numeric
.Select Price			5.2	Numeric
.Price Calculator		20	Text
.Select Number			4	Numeric		367-370
.Select Name			75	Text		371-445
.Test/Continuation Code		1	Numeric
...........................
		read	tempfile,SEQ;hold
		until over
.
		add	C1,N11
		display	*P10:13,*EL,N11
.
		unpack	hold,Hold2,str4,str75,TestCode
		unpack	hold,str5,KeyValue,str6
		display	*P10:14,*EL,"List: ",str6
		display	*P10:15,*EL,"Select: ",str75
.Ferret out Actual Selects
.		type	str4
.		if not equal
			call	Trim using str75
			if (str75 <> "")
.Make Substitutions
				call	Substitute using str75
			endif
.		endif
.Write to File
		write	tempfile2,SEQ;Hold2,str4,str75,TestCode
	repeat
	close	tempfile
	close	tempfile2
.Skip this logic for the time being.  It takes too long to run.
.	GOTO EndPart
SecondTest
.Second Loop is to try to catch Selects where wording is transposed
.MS Word logic slows down unexpectedly, commenting it all out 6/3/05 ASH
..Create your background instance of Word
.	create	WordObj,Class="!Word.Application"
.	setprop	WordObj,*WindowState=wdWindowStateMinimize
.	getprop WordObj,*Documents=DocsObj
.	DocsObj.Add Giving DocObj
.	DocObj.Activate
.	getprop DocObj,*Content=RangeObj
.
	move	C0,N11
	display	*P10:12,*EL,"Second cleanup Loop"
.Create work var
	create	PackData=1:1:1:1,Sorted=1
.Work vars to try to field out Selects where Word order is different, but Selects are really identical
	create	PackData2=1:1:1:1,Sorted=1
	create	PackData3=1:1:1:1,Sorted=1
	open    tempfile,"c:\work\lrfile.new",exclusive
	erase	"c:\work\lrfile.tmp"
	prepare	tempfile2,"c:\work\lrfile.tmp",exclusive
	loop
                read    tempfile,SEQ;hold
		until over
.
		add	C1,N11
		move	N11,str11
		call	Trim using str11
		pack	taskname,"Record Number: ",str11,"-Currently processing block of Lists before this number!"
		display	*P10:13,*EL,taskname
.
		unpack	hold,Hold2,str4,str75
.Ferret out Actual Selects
.		type	str4
.		if not equal
.KeyValue=LR Number, str6=List Number
			unpack	hold,str5,KeyValue,str6
			display	*P10:14,*EL,"Current List: ",ListHold
			display	*P10:20,*EL,"Next List: ",str6
			display	*P10:21,*EL,"Next Select: ",str75
			call	Trim using str75
			if (str6 <> ListHold)
				if (ListHold <> "")	.NOT first record
					call	SortSelectWordPrelim
.Refresh DataList
					deleteitem PackData,0
				endif
				move	str6,ListHold	.Initialize new List Number
			endif
.Start loading up with newest values
			insertitem PackData,0,hold
.		endif
	repeat
.Write out last one
	call	SortSelectWordPrelim
.MS Word logic slows down unexpectedly, commenting it all out 6/3/05 ASH
..Destroy all objects, preventing any erroneous message boxes - including SaveAs dialog box
.        setprop WordObj,*DisplayAlerts=OFALSE
.	destroy DocsObj
.	destroy DocObj
.	destroy RangeObj
.	WordObj.quit Using *SaveChanges=wdDoNotSaveChanges
.        destroy WordObj
.
	destroy	PackData2
	destroy	PackData3
EndPart
	destroy	PackData
	close	tempfile
	close	tempfile2
	display	*P10:12,*EL,"Final Sort of LRFile"
.Commented logic would be used if Second Loop was instated.
	pack	taskname,"c:\work\lrfile.tmp,c:\work\lrfile.dat;2-5,6-11"
.This logic used if only First Loop is used.
.	pack	taskname,"c:\work\lrfile.new,c:\work\lrfile.dat;2-5,6-11"
	sort	taskname
	if over
		move	"INTEGRAL3.PLS Error",SmtpSubject Subject
		move	"0",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		move	"NTS4",SmtpEmailServer			 Address of email serverc
		move	"InformationServices@nincal.com",SmtpEmailAddress
		move	"Information Services",SmtpUserName				  User name
		move	"Information Services",SmtpUserFullName		    User Full Name
		move	"InformationServices@nincal.com",SmtpDestinations(1,1)
		move	"1",SmtpDestIndexLast				    Index to last entry	in Dest	array
		move	"Copying over of LRFile.dat failed!",SmtpTextMessage(1)   Array <Text message >
		move	"1",SmtpTextIndexLast
		clear	SmtpLogFile					    'Clear' disables the LogFile
		call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	else
        	erase   "c:\work\lrfile.new"
	        erase   "c:\work\lrfile.tmp"
	endif
	return

Substitute Routine DimPtr
.Create temporary work variable
	move	DimPtr,str75A
	lowercase str75A		.Create temporary var as SCAN verb is case-sensitive
.
.Some Prelim Replacements First.
.
.These are done first in order to circumvent problems which may arise in String Replacements later on
.
	move	"DTP; ",str45
	move	"DTP/",str55
	call	ReplaceIt using DimPtr,str45,str55
	move	"DTP;",str45
	call	ReplaceIt using DimPtr,str45,str55
.
	move	"COA; ",str45
	move	"COA/",str55
	call	ReplaceIt using DimPtr,str45,str55
	move	"COA;",str45
	call	ReplaceIt using DimPtr,str45,str55
.
	move	";",str45
	move	" ",str55
	call	ReplaceIt using DimPtr,str45,str55
.
	move	":",str45
	move	" ",str55
	call	ReplaceIt using DimPtr,str45,str55
.
	loop
		scan	".00",DimPtr
		until not equal
		sdelete	DimPtr,C3
		reset	DimPtr
	repeat
.Reset as above routines may have change variable.
	move	DimPtr,str75A
	lowercase str75A		.Create temporary var as SCAN verb is case-sensitive
.
.Start String Replacements
.
.I will use the first example as template for documentation
.ReplaceItWord is used here as I do not want to change the case of the entire Master string,
.because mixed-case is preferred and any words that are NOT scanned would have their case
.changed.  Doing this would mean every single word that warranted mixed-case would have to be
.added, including those that are not having there spelling/syntax alter, ie: "National", "Multi",etc.
.This would be a nightmare.  We will assume that User entered these will case sensitivity to their liking.
.
	move	"Subscribers",str55	.str55 will hold the Standard value
	scan	"subscribers",str75A	.Search for Standard value
	if not equal
		reset	str75A 		.Always reset after a SCAN
		scan	"subscriber",str75A	.First possible variation
		if not equal
			reset	str75A		.Always reset after a SCAN
			scan	"subs",str75A	.Second possible variation
			if not equal
				reset	str75A		.Always reset after a SCAN
				scan	"sub",str75A 	.Third possible variation
				if equal
.Correct third possible variation
					move	"sub",str45 	.Reset will happen before next 'section' of scanning!
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
.Any more variations would be nested, starting here...
.				else....
.
				endif
			else
.Correct second possible variation
				move	"subs",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
.Correct first possible variation
			move	"subscriber",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
.If Standard value is found, run ReplaceItWord anyway, in case the Case in Master string is screwy.  This is the opportunity to set the case.
		move	"subscribers",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A	.Always reset after a SCAN
	move	"Members",str55
	scan	"members",str75A
	if not equal
		reset	str75A
		scan	"member",str75A
		if not equal
			reset	str75A
			scan	"mbrs",str75A
			if not equal
				reset	str75A
				scan	"mbr",str75A
				if equal
					move	"mbr",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"mbrs",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"member",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"members",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Donors",str55
	scan	"donors",str75A
	if not equal
		reset	str75A
		scan	"donor",str75A
		if not equal
			reset	str75A
			scan	"dnrs",str75A
			if not equal
				reset	str75A
				scan	"dnr",str75A
				if equal
					move	"dnr",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"dnrs",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"donor",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"donors",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Buyers",str55
	scan	"buyers",str75A
	if not equal
		reset	str75A
		scan	"buyer",str75A
		if not equal
			reset	str75A
			scan	"byrs",str75A
			if not equal
				reset	str75A
				scan	"byr",str75A
				if equal
					move	"byr",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"byrs",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"buyer",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"buyers",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Paid",str55
	scan	"paid",str75A
	if not equal
		reset	str75A
		scan	"pd",str75A
		if equal
			move	"pd",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"paid",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Expires",str55
	scan	"expires",str75A
	if not equal
		reset	str75A
		scan	"expire",str75A
		if not equal
			reset	str75A
			scan	"expr",str75A
			if not equal
				reset	str75A
				scan	"exp",str75A
				if equal
					move	"exp",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"expr",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"expire",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"expires",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Quarterly",str55
	scan	"quarterly",str75A
	if not equal
		reset	str75A
		scan	"qtrly",str75A
		if equal
			move	"qtrly",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"quarterly",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Women",str55
	scan	"women",str75A
	if not equal
		reset	str75A
		scan	"wom",str75A
		if equal
			move	"wom",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"women",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	scan	"monthly",str75A
	if not equal
		reset	str75A
		move	"Mos",str55
		scan	"mos",str75A
		if not equal
			reset	str75A
			scan	"months",str75A
			if not equal
				reset	str75A
				scan	"month",str75A
				if not equal
					reset	str75A
					scan	"mo ",str75A
					if not equal
						reset	str75A
						scan	"mo(",str75A
						if not equal
							reset	str75A
							scan	"mo)",str75A
							if not equal
								reset	str75A
								scan	"mo/",str75A
								if equal
									move	"mo/",str45
									call	ReplaceIt2 using DimPtr,str45,str55,str75A
								endif
							else
								move	"mo)",str45
								call	ReplaceIt2 using DimPtr,str45,str55,str75A
							endif
						else
							move	"mo)",str45
							call	ReplaceIt2 using DimPtr,str45,str55,str75A
						endif
					else
						move	"mo ",str45
						call	ReplaceIt2 using DimPtr,str45,str55,str75A
					endif
				else
					move	"month",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"months",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"mos",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"Monthly",str55
		move	"monthly",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Inquires",str55
	scan	"inquires",str75A
	if not equal
		reset	str75A
		scan	"inquire",str75A
		if not equal
			reset	str75A
			scan	"inqs",str75A
			if not equal
				reset	str75A
				scan	"inq",str75A
				if equal
					move	"inq",str45
					call	ReplaceIt2 using DimPtr,str45,str55,str75A
				endif
			else
				move	"inqs",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"inquire",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"inquires",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
	reset	str75A
.
	reset	str75A
	move	"Home",str55
	scan	"home",str75A
	if not equal
		reset	str75A
		scan	"hme",str75A
		if not equal
			reset	str75A
			scan	"hm",str75A
			if equal
				move	"hm",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"hme",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"home",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Adrs",str55
	scan	"adrs",str75A
	if not equal
		reset	str75A
		scan	"address",str75A
		if not equal
			reset	str75A
			scan	"adr",str75A
			if equal
				move	"adr",str45
				call	ReplaceIt2 using DimPtr,str45,str55,str75A
			endif
		else
			move	"address",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"adrs",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"Active",str55
	scan	"active",str75A
	if not equal
		reset	str75A
		scan	"act",str75A
		if equal
			move	"act",str45
			call	ReplaceIt2 using DimPtr,str45,str55,str75A
		endif
	else
		move	"active",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"DMS",str55
	scan	"direct mail sold",str75A
	if equal
		move	"direct mail sold",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.
	reset	str75A
	move	"DTP",str55
	scan	"direct to publisher",str75A
	if equal
		move	"direct to publisher",str45
		call	ReplaceIt2 using DimPtr,str45,str55,str75A
	endif
.Other Replacements w/o ReplaceIt2
	reset	DimPtr
	move	" SUP ",str55
	scan	str55,DimPtr
	if not equal
		reset	DimPtr
		scan	" SUP",DimPtr
		if not equal
			reset	DimPtr
			scan	"SUP ",DimPtr
			if equal
				reset	DimPtr
				move	"SUP ",str45
				call	ReplaceIt using DimPtr,str45,str55
			endif
		else
			reset	DimPtr
			move	" SUP",str45
			call	ReplaceIt using DimPtr,str45,str55
		endif
	endif
.
	reset	DimPtr
	move	" $",str55
	scan	str55,DimPtr
	if not equal
		reset	DimPtr
		scan	"-$",DimPtr
		if not equal
			reset	DimPtr
			scan	"/$",DimPtr
			if not equal
				reset	DimPtr
				scan	"($",DimPtr
				if not equal
					reset	DimPtr
					scan	")$",DimPtr
					if not equal
						reset	DimPtr
						scan	"$",DimPtr
						if equal
							reset	DimPtr
							move	"$",str45
							call	ReplaceIt using DimPtr,str45,str55
						endif
					endif
				endif
			endif
		endif
	endif
	reset	DimPtr
.
.Reconcile any non-matching Parantheses
	move	C0,result	.Count for "(" characters
	move	C0,howmany	.Count for ")" characters
	loop
		scan	"(",DimPtr
		until not equal
		add	C1,result
		bump	DimPtr
		until EOS
	repeat
	reset	DimPtr
	loop
		scan	")",DimPtr
		until not equal
		add	C1,howmany
		bump	DimPtr
		until EOS
	repeat
	reset	DimPtr
	if (result > howmany)
.Pad extra right parans to end of String
		movelptr DimPtr,N10
		add	C1,howmany
		for N9,howmany,result
			reset	DimPtr,N10
			append	")",DimPtr
			add	C1,N10
		repeat
		reset	DimPtr
		setlptr DimPtr,N10
	elseif (howmany > result)
.Remove furthest most right parans until counts match
		movelptr DimPtr,N9
		move	N9,N8
		move	")",str1
		move	"|",str2	.Use a character I know should NEVER be included!!
		sub	result,howmany,N10
		loop
			reset	DimPtr,N9
			scan	str1,DimPtr
			if equal
				bump	DimPtr,SEQ
				append	"|",DimPtr
				sub	C1,N10
			endif
			until (N10 < C1)
			sub	C1,N9
			until (N9 < C1)		.Safety Check
		repeat
		reset	DimPtr
		setlptr DimPtr,N8
		call	RemoveChar using DimPtr,str2
	endif
	reset	DimPtr
.Blank Pad Parantheses
.Remove all blank-padded parans
.Note:  Attempting to blank pad parans MAY take considerable time
	reset	DimPtr
	move	" (",str45
	loop
		scan	str45,DimPtr
		until not equal
		movefptr DimPtr,N9
		reset	DimPtr
		move	"(",str55
		call	ReplaceIt using DimPtr,str45,str55
		reset	DimPtr,N9
	repeat
.
	reset	DimPtr
	move	") ",str45
	loop
		scan	str45,DimPtr
		until not equal
		movefptr DimPtr,N9
		reset	DimPtr
		move	")",str55
		call	ReplaceIt using DimPtr,str45,str55
		reset	DimPtr,N9
	repeat
.................................
..Following is original logic which padded parans with spaces - but had problems.
.	reset	DimPtr
.	move	" (",str55
.	scan	str55,DimPtr
.	if not equal
.		reset	DimPtr
.		scan	"(",DimPtr
.		if equal
..First, test to make sure this is not the first character
.			movefptr DimPtr,result
.			if (result <> 1)
.				reset	DimPtr
.				move	"(",str45
.				call	ReplaceIt using DimPtr,str45,str55
.			endif
.		endif
.	endif
..
.	reset	DimPtr
.	move	") ",str55
.	scan	str55,DimPtr
.	if not equal
.		reset	DimPtr
.		scan	")",DimPtr
.		if equal
..First test to make sure this is not the last character
.			movefptr DimPtr,result
.			reset	DimPtr
.			movelptr DimPtr,howmany
.			if (result <> howmany)
.				move	")",str45
.				call	ReplaceIt using DimPtr,str45,str55
.			endif
.		endif
.	endif
.....................................
..Following is second attempt to try to blank pad parans, but still flawed
.	movelptr DimPtr,N10
.	loop
.		scan	"(",DimPtr
.		until not equal
..First, test to make sure this is not the first character
.		movefptr DimPtr,result
.		if (result <> 1)
..Second, test to make sure that the preceding character is not another "("
.			bump	DimPtr,SEQ
.			move	DimPtr,str1
.			if (str1 <> "(")
.				append	"|",DimPtr
.			endif
.		endif
.		bump	DimPtr
.		until EOS
.	repeat
.	reset	DimPtr
.	setlptr DimPtr,N10
.	move	"|",str45
.	move	" (",str55
.	call	ReplaceIt using DimPtr,str45,str55
..
.	movelptr DimPtr,N10
.	loop
.		scan	")",DimPtr
.		until not equal
..First, test to make sure this is not the last character
.		movefptr DimPtr,result
.		if (result <> N10)
..Second, test to make sure that the preceding character is not another "("
.			bump	DimPtr
.			move	DimPtr,str1
.			if (str1 <> ")")
.				append	"|",DimPtr
.			endif
.		endif
.		bump	DimPtr
.		until EOS
.	repeat
.	reset	DimPtr
.	setlptr DimPtr,N10
.	move	"|",str45
.	move	") ",str55
.	call	ReplaceIt using DimPtr,str45,str55
...................................
.
	reset	DimPtr
	move	"&",str55
	loop
		scan	" & ",DimPtr
		until not equal
		reset	DimPtr
		move	" & ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	"& ",DimPtr
		until not equal
		reset	DimPtr
		move	"& ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	" &",DimPtr
		until not equal
		reset	DimPtr
		move	" &",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
.
	reset	DimPtr
	move	"-",str55
	loop
		scan	" - ",DimPtr
		until not equal
		reset	DimPtr
		move	" - ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	"- ",DimPtr
		until not equal
		reset	DimPtr
		move	"- ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	" -",DimPtr
		until not equal
		reset	DimPtr
		move	" -",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
.
	pack	str2,"\/"
	rep	str2,DimPtr
.
	reset	DimPtr
	move	"/",str55
	loop
		scan	" / ",DimPtr
		until not equal
		reset	DimPtr
		move	" / ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	"/ ",DimPtr
		until not equal
		reset	DimPtr
		move	"/ ",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
	loop
		scan	" /",DimPtr
		until not equal
		reset	DimPtr
		move	" /",str45
		call	ReplaceIt using DimPtr,str45,str55
	repeat
.Finally, clear out extra blank spaces
.
	move	"  ",str45
	move	" ",str55
	call	ReplaceIt using DimPtr,str45,str55
.
	move	"   ",str45
	move	" ",str55
	call	ReplaceIt using DimPtr,str45,str55
.
	move	"    ",str45
	move	" ",str55
	call	ReplaceIt using DimPtr,str45,str55
.
	return

.ReplaceItWord Routine DimPtr,DimPtr2,DimPtr3
..DimPtr  - Master String
..DimPtr2 - Replacing Word
..DimPtr3 - Word to Replace
..
..This Routine will allow us to Replace without having to worry about modifying the Case on the Master String
..
..Clear out previous data
.	setProp RangeObj,*Text=""
..
.	reset	DimPtr
.	call	Trim using DimPtr
.	call	Trim using DimPtr2
.	call	Trim using DimPtr3
.	if (DimPtr <> "" & DimPtr2 <> "" & DimPtr3 <> "")
..Set text in Range object AND spell check it!
.		setprop RangeObj,*Text=DimPtr
..Run Find/Replace Routine
.		RangeObj.Find.Execute using *FindText=DimPtr2,*ReplaceWith=DimPtr3,*Replace=wdReplaceAll,*MatchCase=OFALSE
..Take back spell checked text and cut off funky control character that Word adds to it
..Note that you MUST reset the Range Text BEFORE retrieving it!!
..Otherwise the funky control character that Word adds will mess up the variable that holds the Text.
..
..I am using the Count property of the Characters collection because if my Find/Replace replaces the entire source string,
..the End property returns a '0', which crashes the program
.		getprop	DocObj.Characters,*Count=result
.		sub	C1,result
.		setprop	RangeObj,*End=result
..This step must follow resetting of End of Range
.		getprop RangeObj,*Text=DimPtr
.	endif
.	return

SortSelectWordPrelim
.Master routine for Sorting/Matching Select Names
.Assumes Input file has been sorted by List Number
.This Routine is called when a List Number changes.
.
.Notes:  This Prelim Routine tests to see if any of the Selects
.are not identical.  If not identical, it will go to the workhorse 'SortSelectWord'.
.If identical, then it will mark all subsequent matching Selects with an '*'
.until a non-match is encountered.
.'SortSelectWord' will start over at the beginning.  Any '*' record is skipped.
.As it updates subsequent Selects, it also marks them with a '*' so that loop will
.skip them later.
.
.'*' Records are not processed!!  This GREATLY speeds up processing time.
.
	PackData.GetCount giving howmany
	if (howmany > C0)
		for result,"1",howmany
.This first loop is just a prelim, in case all the Selects are already identical
			getitem	PackData,result,HoldB
			unpack	HoldB,Hold2,str4,str75
			if (result = C1)
				move	str75,str75A
			else
				if (str75 <> str75A)
.Attempt to sort them out via MS Word
					call	SortSelectWord
					break		.IMPORTANT!!
				else
					pack	HoldA,HoldB,STAR
					setitem	PackData,result,HoldA
				endif
			endif
		repeat
	endif
.Take values and write to new file
hmmm
	PackData.GetCount giving howmany
	if (howmany > C0)
		for result,"1",howmany
			getitem	PackData,result,HoldB
			write	tempfile2,SEQ;HoldB
		repeat
	endif
	return

SortSelectWord
.Takes all Selects from a List and Compares 'words' via MS Word, looking for Selects that are composed differently but actually identical
	PackData.GetCount giving howmany
	if (howmany > C0)
.Nested loop to test if Select Names are semantically identical, if not grammatically.
.If identical, will reconcile the Select Names
		for result1,"1",howmany
.Get 'Master' record
.Remove all 'punctuation'
			getitem	PackData,result1,HoldA
			unpack	HoldA,Hold2,str4,str75,str1
			unpack	HoldA,HoldB,str1
			display	*P10:15,*EL,"Current Select in Sorted Datalist: ",str75
			display	*P10:16,*EL,"Select Record in Sorted DataList: ",result1
			if (str1 <> STAR)	.Select was not already processed
				call	SortSelectWord2 using str75,PackData2
.
				add	C1,result1,howmany2
				for howmany3,howmany2,howmany
.Get 'Comparison' record
					getitem	PackData,howmany3,HoldA
					unpack	HoldA,Hold2,str4,str75
					unpack	HoldA,str5,KeyValue
					unpack	HoldA,HoldB,str1
					display	*P10:17,*EL,"Comparing to Select: ",str75
					display	*P10:18,*EL,"Compared Select Record: ",howmany3
					if (str1 <> STAR)	.Select was not already processed
						call	SortSelectWord2 using str75,PackData3
.Do actual comparison
						move	C0,N1	.Flag to determine if a Replacement was made
						PackData2.GetCount giving howmany4
						PackData3.GetCount giving N9
						if (howmany4 = N9)
							for N9,"1",howmany4
								getitem	PackData2,N9,str45
								getitem	PackData3,N9,str55
								call	Trim using str45
								call	Trim using str55
								if (str45 <> str55)
									move	C1,N1	.Trip flag to prevent update
									break
								endif
							repeat
						else
							move	C1,N1
						endif
.
						if (N1 = C0)	.A Replacement can be made!
.Update the Second record
.Get refreshed first record - remember that this routine strips all 'punctuation' so we must refresh
							getitem	PackData,result1,HoldA
							unpack	HoldA,Hold2,str4,str75A
.Get second record to be refreshed
							getitem	PackData,howmany3,HoldA
							unpack	HoldA,Hold2,str4,str75,TestCode
.Reload with everything - EXCEPT replace Select Name!
.'*' Indicates this record has already been adjusted by a preceding Select Record.
.This means we can skip it in the future, as any iteration following would have been
.taken care of when preceding Select record was run.
							pack	HoldA,Hold2,str4,str75A,TestCode,STAR
							setitem	PackData,howmany3,HoldA
						endif
					else
.						beep
					endif
				repeat
			else
.				beep
			endif
		repeat
	endif
	return

SortSelectWord2 Routine DimPtr,DataPtr
.DimPtr  - Master String
.DataPtr - DataList
.
.This is the Routine which strips punctuation from Select line,
.dumps it into MS Word and then breaks it down into sorted 'words'.
.'Words' are then compared to see if Select is semantically identical, if not literally.
.
.Clear out previous data
	deleteitem DataPtr,0
.MS Word logic slows down unexpectedly, commenting it all out 6/3/05 ASH
.	setProp RangeObj,*Text=""
........................
.Pull out some info, and reformat it
	move	"/ ",str2
	rep	str2,DimPtr
	move	"( ",str2
	rep	str2,DimPtr
	move	") ",str2
	rep	str2,DimPtr
	move	"; ",str2
	rep	str2,DimPtr
	move	"- ",str2
	rep	str2,DimPtr
	move	", ",str2
	rep	str2,DimPtr
	lowercase DimPtr
.
	call	Trim using DimPtr
	if (DimPtr <> "")
.MS Word logic slows down unexpectedly, commenting it all out 6/3/05 ASH
..Set text in Range object AND spell check it!
.		setprop RangeObj,*Text=DimPtr
..Pick out 'Words'
.		getprop	RangeObj.Words,*Count=howmany6
..MS Word returns control character as it's own 'word', so skip it.
.		sub	C1,howmany6
.		for howmany5,C1,howmany6
.			getprop	RangeObj.Words(howmany5),*Text=str35
.			call	Trim using str35
..			insertitem DataPtr,0,str35
...................................................
.			if (str35 = "$")
..append "$" to next Word
.				move	str35,str35a
.			elseif (str35 = "+")
..attach "+" to preceding Word
.				DataPtr.GetCount giving N7
.				for N6,C1,N7
.					getitem	DataPtr,N6,str35c
.					if (str35c = str35b)
.						pack	str35b,str35b,str35
.						setitem DataPtr,N6,str35b
.						goto BackToLoop
.					endif
.				repeat
.				insertitem DataPtr,0,str35
.			elseif (str35a <> "")
.				pack	str35a,str35a,str35
.				insertitem DataPtr,0,str35a
.				clear	str35a
.			else
.				insertitem DataPtr,0,str35
.			endif
.BackToLoop
.			move	str35,str35b
.		repeat
............................................................................
		movefptr DimPtr,howmany5
		movelptr DimPtr,howmany6
		loop
			scan	B1,DimPtr
			until not equal
			movefptr DimPtr,howmany7
			reset	DimPtr,howmany5
			setlptr	DimPtr,howmany7
			move	DimPtr,str35
			call	Trim using str35
			insertitem DataPtr,0,str35
			add	C1,howmany7,howmany5
			reset	DimPtr,howmany5
			setlptr	DimPtr,howmany6
		repeat
.Get last one
		reset	DimPtr,howmany5
		setlptr	DimPtr,howmany6
		move	DimPtr,str35
		call	Trim using str35
		insertitem DataPtr,0,str35
        endif
        return

ReplaceIt2 Routine DimPtr,DimPtr2,DimPtr3,DimPtr4
.DimPtr  - Master String
.DimPtr2 - Replacing Word
.DimPtr3 - Word to Replace
.DimPtr4 - Version of Master String in Lowercase - for scanning purposes!
.
.This Routine will allow us to Replace without having to worry about modifying the Case on the Master String
.This Routine assumes that the Master String has NOT been reset!!
.
	call	Trim using DimPtr
.	call	Trim using DimPtr2	.Sometimes I need to use blank characters!!
	call	Trim using DimPtr3
.Do not alter DimPtr4!!  It is holding the place of the Replacing word.
	if (DimPtr <> "" & DimPtr2 <> "" & DimPtr3 <> "")
.Determine how many characters we are dealing with
		count	result,DimPtr2
.Set Length Pointer to 'Select' the REAL spelling(case sensitive) in Master String
.Form Pointer is already set.  See Notes above!!
		movefptr DimPtr4,howmany
		calc	result=((result-1)+howmany)
.Capture REAL, case-sensitive version of Replacing Word
		movelptr DimPtr,N9	.Hold original length pointer
		reset	DimPtr,howmany
		setlptr	DimPtr,result
.Dump this value in Replacing Word var
		remove	DimPtr,DimPtr2
.Reset Master String
		reset	DimPtr
		setlptr	DimPtr,N9
		if (DimPtr2 <> DimPtr3)
.Do actual replacement
			call	ReplaceIt using DimPtr,str45,str55
		endif
.Important Step!!!
.Temp version of Master String will need to be refreshed now.
		move	DimPtr,DimPtr4
		lowercase DimPtr4
        endif
        return

	include	comlogic.inc
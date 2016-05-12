PC         EQU       0
	include	common.inc
	include	cons.inc
	include	nprjdd.inc
.	include	nmlrdd.inc
	include	ndatdd.inc
	include	compdd.inc
	include	cntdd.inc

release	init	"1.0"
errors	file
errors2	file
errors3	file
books	automation
book	automation
sheets	automation
sheet	automation
ex	automation	class="Excel.Application"
sheetnum integer 4
percentage form	3.4
DimPtr	dim	^
.	FORM	11
n114	FORM	11.4
..Booleans
..PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
..Formatting vars needed
..These constants were found in the Object Browser in Excel under the Help topic for the
..HorizontalAlignment property of the Range object.
.AlignRight integer 4,"0xffffefc8"
.AlignCenter integer 4,"0xffffeff4"
.SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
.xlUnderlineStyleSingle integer 4,"0x2"
.FirstRec form   8
	Prepare	Errors,"c:\work\Errors.dat"
	Prepare	Errors2,"c:\work\Errors2.dat"
	Prepare	Errors3,"c:\work\Errors3.dat"
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0

	clock	timestamp,timestamp
	move	C1,NDATPATH
	move	C1,NMLRPATH
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.	setprop ex.CommandBars("Standard"),*Visible="True"
.	setprop ex.CommandBars("Formatting"),*Visible="True"
.	setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
..Reset Default of Worksheets found in a Workbook
.        getprop ex,*SheetsInNewWorkbook=SheetsDefault
.        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
	books.open using "e:\data\brrent.xls"
.	getprop books.Item(1),*Sheets=sheets
.	for sheetnum,C2,"8"
      MOVE      C1 TO SHEETNUM
		if (sheetnum <> 5)
			sheets.item giving sheet using sheetnum
.			move	C6,howmany
    			loop
				move	howmany,str9
				call	Trim using str9
				pack	str11,"B",str9
				if (SheetNum = 4 | SheetNum = 8)	.List Management
					getprop	sheet.range(str11),*Value=str6
					call	Trim using str6
					if (str6 = "")
						goto EndLoop
					endif
					call	ZFillIt using str6
					pack	NDATFLD,str6
					pack	KeyLocation,"Key: ",NDATFLD
					move	"NDATKEY",Location
					call	NDATKEY
					if over
						Write	Errors,seq;"pass ",sheetnum," List" ,str6
						goto BreakIt
					endif
					move	NDATFLD,ProjClient
				else					.Brokerage
.					getprop	sheet.range(str11),*Value=str4
					call	Trim using str4
					if (str4 = "")
						goto EndLoop
					endif
					call	ZFillIt using str4
					pack	MKEY,str4,"000"
					pack	KeyLocation,"Key: ",MKEY
					move	"NMLRKEY",Location
					call	NMLRKEY
					if over
						Write	Errors,seq;"pass ",sheetnum," Mailer" ,str4
						goto BreakIt
					endif
				endif
 		if (ProjSrc = "B")
			move	"COMPKEY3",Location
			unpack	ProjClient,str2,str4
			pack	COMPFLD3,str4
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
			if over
				pack	str55,ProjClient," does not exist in Company file!!       "
				goto	WriteError
			elseif (COMPMLRFLG <> "T")
				pack	str55,ProjClient," is not a valid Mailer in Company file!!"
				goto	WriteError
			else
				move	COMPNUM,ProjClient
			endif
		endif
.					pack	ProjClient,"00",MKEY
.				endif
				move	"2005",ProjYr
				move	"01",ProjKey
				if (SheetNum = 1)
.Brokerage LR Rental
					move	"R",ProjType
					move	"B",ProjSrc
				elseif (SheetNum = 3)
.Brokerage LR Exchange
					move	"E",ProjType
					move	"B",ProjSrc
				elseif (SheetNum = 4)
.List Management LR
					move	" ",ProjType
					move	"M",ProjSrc
				elseif (SheetNum = 6)
.Brokerage NIN Rental
					move	"R",ProjType
					move	"B",ProjSrc
				elseif (SheetNum = 7)
.Brokerage NIN Exchange
					move	"E",ProjType
					move	"B",ProjSrc
				elseif (SheetNum = 8)
.List Management NIN
					move	" ",ProjType
					move	"M",ProjSrc
				endif
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
				call	NPRJKEY
				if not over
					if (SheetNum < 5)
						if (SheetNum = 4)
							Write	Errors2,seq;"pass ",sheetnum," List" ,str6
						else
							Write	Errors2,seq;"pass ",sheetnum," Mailer" ,str4
						endif
					else
						call	LoadIt
						if (str2 = YES)
							move	"NPRJUPD",Location
							call	NPRJUPD
						endif
					endif
				else
					unpack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
					move	timestamp,ProjDate
.
					call	LoadIt
					if (str2 = YES)
						move	"NPRJWRT",Location
						call	NPRJWRT
					endif
				endif

BReakIt
				add	C1,howmany
			repeat
		endif
EndLoop
.     	repeat
	destroy sheet
	destroy sheets
	destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
	setprop ex,*DisplayAlerts=OFALSE
	destroy OTRUE
	destroy OFALSE
	ex.quit
	destroy ex
	stop

LoadIt
	clear	str2
	pack	str11,"P",str9
	getprop	sheet.range(str11),*Value=str25
	call	Trim using str25
	call	RemoveChar using str25,comma
	move	"$",str1
	call	RemoveChar using str25,str1
	type	str25
	if not equal
		Write	Errors3,seq;"pass ",sheetnum," Mailer" ,str4," Total ",str25
	else
		move	C0,N11
		move	str25,N11
		move	N11,str25
		call	Trim using str25
		move	YES,str2
		if (SheetNum < 5)
			move	str25,ProjLR
		else
			move	str25,ProjNin
		endif
		move	"D",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRJan
		else
			move	str3,projNINJan
		endif
		move	"E",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRFeb
		else
			move	str3,projNINFeb
		endif
		move	"F",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRMar
		else
			move	str3,projNINMar
		endif
		move	"G",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRApr
		else
			move	str3,projNINApr
		endif
		move	"H",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRMay
		else
			move	str3,projNINMay
		endif
		move	"I",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRJun
		else
			move	str3,projNINJun
		endif
		move	"J",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRJul
		else
			move	str3,projNINJul
		endif
		move	"K",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRAug
		else
			move	str3,projNINAug
		endif
		move	"L",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRSep
		else
			move	str3,projNINSep
		endif
		move	"M",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLROct
		else
			move	str3,projNINOct
		endif
		move	"N",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRNov
		else
			move	str3,projNINNov
		endif
		move	"O",str1
		call	FindPercentage using str1
		if (SheetNum < 5)
			move	str3,projLRDec
		else
			move	str3,projNINDec
		endif
	endif
	return

FindPercentage LRoutine DimPtr
	clear	str3
	pack	str11,DimPtr,str9
	getprop	sheet.range(str11),*Value=str25
	call	Trim using str25
	call	RemoveChar using str25,comma
	move	"$",str1
	call	RemoveChar using str25,str1
	type	str25
	if not equal
		Write	Errors3,seq;"pass ",sheetnum," Mailer" ,str4," ",str9," ",str25
	else
		call	CalcIt
	endif
	return

CalcIt
	move	C0,N114
	move	C0,percentage
	move	str25,N114
	if (SheetNum < 5)
		calc	percentage=(N114/ProjLR)
	else
		calc	percentage=(N114/ProjNin)
	endif
	mult	"100",percentage
	move	percentage,N3
	move	N3,str3
	return

.EndOLoop
.	repeat
.	shutdown

WriteError
	write	tempfile,SEQ;str55,PROJVARS
.	goto EndOLoop
	return

.bReakIt
.				add	C1,howmany
.			repeat
.		endif

	include	nprjio.inc
.	include	nmlrio.inc
	include	ndatio.inc
	include	compio.inc
	include	cntio.inc
	include comlogic.inc

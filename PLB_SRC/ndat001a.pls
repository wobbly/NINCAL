.NDAT001A.PLS - SUPPLEMENTAL CODE FOR ORGANIZING SELECT NAMES PULLED TOGETHER FROM SELECT PARTITIONS
	include	common.inc
	include	cons.inc
.START PATCH 1.1 ADDED LOGIC
	include	nseldd.inc
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.3 ADDED LOGIC
	include	ndatdd.inc
.END PATCH 1.3 ADDED LOGIC

release	init	"1.4"	ASH	14OCT2004	ADDED ROUTINE TO DO OUTSIDE READ OF DATACARD TO DETERMINE IF EXCLUSIVE
.release	init	"1.3"	ASH	17MAR2004	ADDED ROUTINE TO CHECK DATACARD
.release	init	"1.2"	ASH	11NOV2003	COMMENTED OUT LOGIC WHICH IS NO LONGER ACCESSED
.release	init	"1.1"	ASH	08APR2003	ADDED LOGIC TO TEST FOR DUPLICATE OF SELECT BASES
.release	init	"1.0"	ASH	24MAR2003	INITIAL RELEASE
DataScreen	Window
DataListView	ListView
DimPtr		Dim	^
.START PATCH 1.1 ADDED LOGIC
DimPtr1		Dim	^
DimPtr2		Dim	^
DimPtr3		Dim	^
FrmPtr		form	^
taskname2	dim	200
.END PATCH 1.1 ADDED LOGIC
OrdArray	form	1(9)
#N1		form	1

.START PATCH 1.2 COMMENTED LOGIC
.DataInitializeListView Routine
.	destroy	DataScreen
.	create	DataScreen=0:0:0:0
..	destroy	DataListView
.	create	DataScreen;DataListView=0:0:0:0,SortHeader=1,SortOrder=1
.        DataListView.InsertColumn using "",0,1
.        DataListView.InsertColumn using "",0,2
.	return

.DataLoadListView Routine DimPtr
.	unpack	DimPtr,NPRTVARS
.	move	C0,result
.	DataListView.GetItemCount giving howmany
.	sub	C1,howmany
.	for N9,C0,howmany
.		DataListView.GetItemText giving str3 using N9,1
.		if (str3 = NPRTNUM)
.			move	C1,result
.			break
.		endif
.	repeat
..Only add the item if its not already there
.	if (result = C0)
.		pack	str4,NPRTORD,NPRTNUM
.		DataListView.InsertItem giving result using str4
.		DataListView.SetItemText using result,DimPtr,1
.	endif
.	return

.DataListViewExtract Routine DimPtr
..Organize the concatenation of these items
.	clear	DimPtr
.	clear	OrdArray
.	clear	str2
.	DataListView.GetItemCount giving howmany
.	sub	C1,howmany
.	for N1,C0,"9"		.Directly refers to Ordinal values
.		add	C1,N1,#N1
.		move	#N1,str1
.		for N9,C0,howmany
.			DataListView.GetItemText giving str55 using N9,1
.			unpack	str55,NPRTVARS
.			if (NPRTORD = str1)
.				call	Trim using NPRTPNAME
.				if (DimPtr = "")
.					append	NPRTPNAME,DimPtr
.				else
.					if (NPRTORD = "1")		.TYPE - "Subs/Dnrs"
.						if (OrdArray(#N1) = 1)
.							append	SLASH,DimPtr
.						endif
.					elseif (NPRTORD = "2")		.RECENCY - "24 Mos"
.						if (OrdArray(2) = 1)	.Should really never happen - 2 Recencys do not make sense!
.							append	SLASH,DimPtr
.						else
.							if (OrdArray(1) = 1)
.								append	"(",DimPtr
.								move	YES,str2
.							endif
.						endif
.					elseif (NPRTORD = "3")		.DOLLAR - "$10+"
.						if (OrdArray(1) = 1)
.							if (OrdArray(2) = 0)
.								append	"(",DimPtr
.								move	YES,str2
.							else
.								append	SLASH,DimPtr
.							endif
.						elseif (OrdArray(2) = 1)	.Implied that TYPE does not exist
.							append	SLASH,DimPtr
.						endif
.					elseif (NPRTORD = "4")		.GENDER - "Men"
.						if (OrdArray(1) = 1)
.							if (OrdArray(2) = 0 & OrdArray(3) = 0)
.								append	"(",DimPtr
.								move	YES,str2
.							elseif (str2 = YES)
.								append	")",DimPtr
.								move	NO,str2
.							endif
.						else
.							append	"(",DimPtr
.							move	YES,str2
.						endif
.					elseif (NPRTORD = "5")		.OTHER - "DMS"
.						if (str2 = YES)
.							append	")",DimPtr
.							clear	str2
.						elseif (str2 <> NO)
.							append	"(",DimPtr
.							move	YES,str2
.						elseif (DimPtr <> ")")
.							append	";",DimPtr
.						endif
.					endif
.					append	NPRTPNAME,DimPtr
.				endif
.				move	C1,OrdArray(#N1)
.			endif
.		repeat
.	repeat
.	if (str2 = YES)
.		append	")",DimPtr
.	endif
.	if (DimPtr <> "")
.		reset	DimPtr
.	endif
.	return
.END PATCH 1.2 COMMENTED LOGIC

.START PATCH 1.1 ADDED LOGIC
SelectTestBase Routine DimPtr,DimPtr1,DimPtr2
.Routine queries Select File to determine if there is another Select already marked as Base,
.and updates if necessary, to allow New Select to assume Base Status (done by calling program)
.DimPtr   = List Number
.DimPtr1  = Select Number
.DimPtr2  = Flag to determine if calling program can Update Select Number with Base
	clear	DimPtr2
	pack	NSELFLD1,"01X",DimPtr
	pack	NSELFLD2,"02XBASE"
	move	"NSELAIM",Location
	pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
	call	NSELAIM
	if over
		move	YES,DimPtr2
	else
		loop
			if (NSELNUM <> DimPtr1)
				pack	taskname,"There is already a Base Select for this List!",newline,"Do you want Select ",DimPtr1," to be the Base instead?"
				alert	plain,taskname,result
				if (result = 1)
					move	"   ",NSELBASE
					move	"NSELUPD",Location
					call	NSELUPD
					move	YES,DimPtr2
				else
					move	NO,DimPtr2
				endif
				break
			endif
			move	"NSELKG",Location
			call	NSELKG
			until over
		repeat
	endif
	return

SelectTestBase2 Routine DimPtr,DimPtr1,DimPtr2,DimPtr3
.Routine queries Select File to determine if there is a Select marked as Base/Secondary Base.
.DimPtr   = List Number
.DimPtr1  = Select Number
.DimPtr2  = Flag to determine if calling program can use Associated Base Select Number
	move	NO,DimPtr2	.Initialize
	pack	NSELFLD,DimPtr,DimPtr1
	move	"NSELKEY",Location
	pack	KeyLocation,"Key: ",NSELFLD
	call	NSELKEY
	if not over
		loop
			until (NSELNUM <> DimPtr1)
			if (NSELBASE = "BASE" | NSELBASE = "SEC.")
				move	YES,DimPtr2
				break
			endif
			move	"NSELKS",Location
			call	NSELKS
			until over
		repeat
	endif
	if (DimPtr2 = NO)
		call	Trim using DimPtr3
		if (DimPtr3 <> "")
.Try to locate a possible suggestion
			move	DimPtr3,taskname2
			clear	DimPtr3		.Initialize it
			pack	NSELFLD1,"01X",DimPtr
			clear	NSELFLD2
			move	"2-NSELAIM",Location
			pack	KeyLocation,"Key: ",NSELFLD1
			call	NSELAIM
			loop
				until over
				if (NSELBASE = "BASE" | NSELBASE = "SEC.")
					scan	NSELSNAME,taskname2
					if equal
						move	NSELNUM,DimPtr3
						break
					endif
				endif
				move	"2-NSELKG",Location
				pack	KeyLocation,"Key: ",NSELFLD1
				call	NSELKG
			repeat
		endif
	endif
	return

.START PATCH 1.2 COMMENTED LOGIC
.SelectTestBase3 Routine DimPtr,DimPtr1,DimPtr2
..Routine queries Select File to determine if associated Base is Primary or Secondary
..DimPtr   = List Number
..DimPtr1  = Base Select Number
..DimPtr2  = Flag to determine if Base is Primary or Secondary
.	clear	DimPtr2	.Initialize
.	pack	NSELFLD,DimPtr,DimPtr1
.	move	"3-NSELKEY",Location
.	pack	KeyLocation,"Key: ",NSELFLD
.	call	NSELKEY
.	if not over
.		if (NSELBASE = "BASE")
.			move	"1",DimPtr2
.		elseif (NSELBASE = "SEC.")
.			move	"2",DimPtr2
.		endif
.	endif
.	return
.END PATCH 1.2 COMMENTED LOGIC
.START PATCH 1.1 ADDED LOGIC
SelectTestBase4 Routine DimPtr,DimPtr1,FrmPtr
.Routine queries Select File to determine Base Price
.DimPtr   = List Number
.DimPtr1  = Base Select Number
.FrmPtr   = Base Price
	move	C0,FrmPtr	.Initialize
	pack	NSELFLD,DimPtr,DimPtr1
	move	"4-NSELKEY",Location
	pack	KeyLocation,"Key: ",NSELFLD
	call	NSELKEY
	if not over
		move	NSELPRICE,FrmPtr
	endif
	return
.END PATCH 1.1 ADDED LOGIC

SelectGetNextIndex Routine DimPtr,DimPtr1
.Retrieves next logical Index Value for New Select Records
.DimPtr  = List Number
.DimPtr1 = New Index Value (returned)
.
	call	Trim using DimPtr
	if (DimPtr = "")	.New Datacard, not yet established!
		move	"0001",DimPtr1
		return
	endif
	move	C0,N4
	pack	NSELFLD1,"01X",DimPtr
	clear	NSELFLD2
	move	"3-NSELAIM",Location
	pack	KeyLocation,"Key: ",NSELFLD1
	call	NSELAIM
	loop
		until over
		call	Trim using NSELINDEX
		move	C0,howmany
		move	NSELINDEX,howmany
		if (howmany > N4)
			move	howmany,N4
		endif
		move	"3-NSELKG",Location
		call	NSELKG
	repeat
	add	C1,N4
	move	N4,DimPtr1
	rep	zfill,DimPtr1
	return

.START PATCH 1.3 ADDED LOGIC
SelectGetExchange Routine DimPtr,DimPtr1,DimPtr2
.Retrieves default value of NSELEXC from Select Base
.DimPtr  = List Number
.DimPtr1 = Base Number
.DimPtr2 = Exchange Status (returned)
	clear	DimPtr2			.Initialize value
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
	move	C1,NSELPATH
	pack	NSELFLD,DimPtr,DimPtr1
	move	"NSELKEY",Location
	pack	KeyLocation,"Key: ",NSELFLD
	call	NSELKEY
	if not over
		move	NSELEXC,DimPtr2
	endif
	return

SelectSetExchange Routine DimPtr,DimPtr1,DimPtr2
.Allows dynamic updating of Exchange Status for all Select records when a Base/Sec. Base is saved.
.DimPtr  = List Number
.DimPtr1 = Select Number
.DimPtr2 = Exchange Status of Base
	call	Trim using DimPtr
	call	Trim using DimPtr1
	if (DimPtr = "" | DimPtr1 = "")
		return
	endif
	move	C0,result
	pack	NSELFLD1,"01X",DimPtr
	clear	NSELFLD2
	move	"NSELAIM",Location
	pack	KeyLocation,"Key: ",NSELFLD1
	call	NSELAIM
	loop
		until over
		if (NSELBASE = DimPtr1)
			if (NSELEXC <> DimPtr2)
				if (result = C0)
					pack	taskname,"There are Selects under this Base with different Exchange Statuses.",newline,"Do you want to update any/all Selects off this Base?"
					alert	plain,taskname,result
				endif
				if (result = 1)
					move	DimPtr2,NSELEXC
					move	"NSELUPD",Location
					pack	KeyLocation,"Key: ",NSELFLD1
					call	NSELUPD
				else
					break
				endif
			endif
		endif
		move	"NSELKG",Location
		pack	KeyLocation,"Key: ",NSELFLD1
		call	NSELKG
	repeat
	return
.END PATCH 1.3 ADDED LOGIC

.START PATCH 1.4 ADDED LOGIC
GetDatacardExclusive Routine DimPtr,DimPtr1
.DimPtr  = Datacard Number
.DimPtr1 = Exclusive Byte
	pack	NDATFLD,DimPtr
	move	"NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	if over
.This should never happen
		move	STAR,DimPtr1
	else
		move	ELSTCDE,DimPtr1
	endif
	return
.END PATCH 1.4 ADDED LOGIC

.START PATCH 1.1 ADDED LOGIC
	include	nselio.inc
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.3 ADDED LOGIC
	include	ndatio.inc
.END PATCH 1.3 ADDED LOGIC
	include	comlogic.inc
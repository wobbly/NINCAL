PC	EQU	1
.......................................................
.......................................................
.	NINT001.PLS
.	Integral Campaign/Detail Cross Reference File Maintenance Program
.	Andrew Harkins
.	September 13, 2004
.
.	Allows Maintenance of Cross Reference files
.       used by Merge Manager and Program 1
.......................................................
.......................................................
	include	common.inc
	include	cons.inc
	include	nlol2dd.inc
	include	ncmp2dd.inc
	include	norddd.inc
.	include	f:\library\develop\backups\norddd.inc
	include	ncmpdd.inc
	include	nloldd.inc
	include	compdd.inc
	include	cntdd.inc
.	include	f:\library\develop\backups\nloldd.inc
.	include	f:\library\develop\backups\compdd.inc
.	include	f:\library\develop\backups\cntdd.inc

release	init	"1.1"	ASH	01DEC2004 Campaign file conversion
.release	init	"1.0"

ExitFlag init	"Y"
ReturnFlag init	"N"
NewFlag	init	"N"
DimPtr	dim	^
FrmPtr	form	^
FrmPtr1	form	^
TabNum	form	"1"
hold	dim	500
colordim dim	8

.Colors
white	color
grey	color
red	color
black	color

Timer	Timer

mss1	plform	Error
abt	plform	About

x	plform	nint0001
Nint001a plform	Nint001a
Nint001b plform	Nint001b
	winhide

.Load Forms, Always load parent	form first
	formload x
	formload Nint001a,nint0001
	formload Nint001b,nint0001
	formload mss1
	formload abt

.Create	Colors for EditText Inquiry
	create	white=*white
	create	grey=220:220:220
	create	red=*red
	create	black=*black

	CREATE	TIMER,18000	.30 minutes
	ACTIVATE TIMER,Timeout,RESULT

.Load ListView Objects
	IntegralListView.InsertColumn using "Number",50,0
	IntegralListView.InsertColumn using "Name",80,1
	IntegralListView.InsertColumn using "Integral ID",80,2
	IntegralListView.InsertColumn using "Mailer",75,3
	IntegralListView.InsertColumn using "Update",50,4
	IntegralListView.InsertColumn using "Details",0,5
.
	IntegralListViewDetail.InsertColumn using "Number",50,0
	IntegralListViewDetail.InsertColumn using "",100,1
	IntegralListViewDetail.InsertColumn using "Integral ID",100,2
	IntegralListViewDetail.InsertColumn using "Update",50,3
	IntegralListViewDetail.InsertColumn using "Type",200,4
	IntegralListViewDetail.InsertColumn using "Details",100,5
	IntegralListViewDetail.InsertColumnFgClr using *Index=6
.	IntegralListViewDetail.InsertColumnBgClr using *Index=7
.
	Integral2ListView.InsertColumn using "Number",50,0
	Integral2ListView.InsertColumn using "",100,1
	Integral2ListView.InsertColumn using "Integral ID",80,2
	Integral2ListView.InsertColumn using "Update",50,3
	Integral2ListView.InsertColumn using "Type",75,4
	Integral2ListView.InsertColumn using "Campaign",75,5
	Integral2ListView.InsertColumn using "Integral ID",80,6
	Integral2ListView.InsertColumn using "Mailer",75,7
	Integral2ListView.InsertColumn using "Details",0,8
.
	move	C1,NORDPATH
	move	C1,NCMPPATH
	call	IntegralDisableCamp1
	call	IntegralDisableCamp2
	call	Integral2Disable
	clock	timestamp,timestamp
	loop
		waitevent
		deactivate TIMER
		ACTIVATE TIMER,Timeout,RESULT
	repeat

Timeout
	beep
	beep
	beep
	shutdown
	return

FileGo
FileGo2
	if (ExitFlag = YES)
		shutdown
	endif
	return

IntegralDisableCamp1
	move	NO,NewFlag
	setprop	IntegralEditCampNum,enabled=0,bgcolor=grey
	setprop	IntegralEditIntegralID,enabled=0,bgcolor=grey
	setprop	IntegralEditIntegralDate,enabled=0,bgcolor=grey
	setprop	IntegralCheckUpdate,enabled=0
	return

IntegralEnableCamp1
	setprop	IntegralEditIntegralID,enabled=1,bgcolor=white
	setprop	IntegralEditIntegralDate,enabled=1,bgcolor=white
	setprop	IntegralCheckUpdate,enabled=1
	return

IntegralDisableCamp2
	move	NO,NewFlag
	setprop	IntegralEditDetIntegralID,enabled=0,bgcolor=grey
	setprop	IntegralEditDetIntegralDate,enabled=0,bgcolor=grey
	setprop	IntegralCheckDetUpdate,enabled=0
	setprop	IntegralComboDetType,enabled=0,bgcolor=grey
	return

IntegralEnableCamp2
	setprop	IntegralEditDetIntegralID,enabled=1,bgcolor=white
	setprop	IntegralEditDetIntegralDate,enabled=1,bgcolor=white
	setprop	IntegralCheckDetUpdate,enabled=1
	setprop	IntegralComboDetType,enabled=1,bgcolor=white
	return

IntegralDisableCamp1Buttons
	if (ExitFlag = NO)
		alert	note,"You have already opened a file for modification!",result
		noreturn
		return
	else
		move	NO,ExitFlag
	endif
	setprop	IntegralQuit,enabled=1
	setprop	IntegralSave,enabled=1
IntegralDisableCamp1ButtonsB
	setprop	IntegralListView,enabled=0
	setprop	IntegralOK,enabled=0
	setprop	IntegralNew,enabled=0
	setprop	IntegralModify,enabled=0
	return

IntegralEnableCamp1Buttons
	setprop	IntegralQuit,enabled=0
	setprop	IntegralSave,enabled=0
	setprop	IntegralDelete,enabled=0
	setprop	IntegralNew,enabled=1
	setprop	IntegralOK,enabled=1
	setprop	IntegralListView,enabled=1
	move	YES,ExitFlag
	return

IntegralDisableCamp2Buttons
	if (ExitFlag = NO)
		alert	note,"You have already opened a file for modification!",result
		noreturn
		return
	else
		call	IntegralDisableCamp1ButtonsB
		move	NO,ExitFlag
	endif
	setprop	IntegralListView,enabled=0
	setprop	IntegralListViewDetail,enabled=0
	setprop	IntegralOK,enabled=0
	setprop	IntegralDetNew,enabled=0
	setprop	IntegralDetModify,enabled=0
	setprop	IntegralDetQuit,enabled=1
	setprop	IntegralDetSave,enabled=1
	return

IntegralEnableCamp2Buttons
	call	IntegralEnableCamp1Buttons
	setprop	IntegralDetQuit,enabled=0
	setprop	IntegralDetSave,enabled=0
	setprop	IntegralDetDelete,enabled=0
	setprop	IntegralOK,enabled=1
	setprop	IntegralListView,enabled=1
	setprop	IntegralListViewDetail,enabled=1
	move	YES,ExitFlag
	return

Integral2Disable
	setprop	Integral2EditIntegralID,enabled=0,bgcolor=grey
	setprop	Integral2EditIntegralDate,enabled=0,bgcolor=grey
	setprop	Integral2CheckUpdate,enabled=0
	setprop	Integral2EditDetIntegralID,enabled=0,bgcolor=grey
	setprop	Integral2EditDetIntegralDate,enabled=0,bgcolor=grey
	setprop	Integral2CheckDetUpdate,enabled=0
	setprop	Integral2ComboDetType,enabled=0,bgcolor=grey
	return

Integral2Enable
	setprop	Integral2EditDetIntegralID,enabled=1,bgcolor=white
	setprop	Integral2EditDetIntegralDate,enabled=1,bgcolor=white
	setprop	Integral2CheckDetUpdate,enabled=1
	setprop	Integral2ComboDetType,enabled=1,bgcolor=white
	return

Integral2DisableButtons
	if (ExitFlag = NO)
		alert	note,"You have already opened a file for modification!",result
		noreturn
		return
	else
		move	NO,ExitFlag
	endif
	setprop	Integral2ListView,enabled=0
	setprop	Integral2OK,enabled=0
	setprop	Integral2DetModify,enabled=0
	setprop	Integral2DetQuit,enabled=1
	setprop	Integral2DetSave,enabled=1
	return

Integral2EnableButtons
	setprop	Integral2DetQuit,enabled=0
	setprop	Integral2DetSave,enabled=0
	setprop	Integral2DetDelete,enabled=0
	setprop	Integral2OK,enabled=1
	setprop	Integral2ListView,enabled=1
	move	YES,ExitFlag
	return

IntegralClearCamp1
	setitem	IntegralEditCampNum,0,""
	setitem	IntegralStatCampName2,0,""
	setitem	IntegralEditIntegralID,0,""
	setitem	IntegralEditIntegralDate,0,""
	setitem	IntegralCheckUpdate,0,0
	return

IntegralClearCamp2
	setitem	IntegralStatDetNum2,0,""
	setitem	IntegralStatDetOstat2,0,""
	setitem	IntegralEditDetIntegralID,0,""
	setitem	IntegralEditDetIntegralDate,0,""
	setitem	IntegralCheckDetUpdate,0,""
	setitem	IntegralComboDetType,0,0
	return

IntegralClearDetail
	Integral2ListView.DeleteAllItems
	setitem	Integral2StatRec,0,""
	setitem	Integral2StatCampNum2,0,""
	setitem	Integral2StatCampName2,0,""
	setitem	Integral2EditIntegralID,0,""
	setitem	Integral2EditIntegralDate,0,""
	setitem	Integral2CheckUpdate,0,0
	setitem	Integral2StatDetNum2,0,""
	setitem	Integral2StatDetOstat2,0,""
	setitem	Integral2EditDetIntegralID,0,""
	setitem	Integral2EditDetIntegralDate,0,""
	setitem	Integral2CheckDetUpdate,0,0
	setitem	Integral2ComboDetType,0,1
	return

IntegralLoadCamp1
	setitem	IntegralEditCampNum,0,NCMP2Num
	setitem	IntegralStatCampName2,0,NCMPCName
	setitem	IntegralEditIntegralID,0,NCMP2INum
	call	Trim using NCMP2UDate
	if (NCMP2UDate <> "")
		unpack	NCMP2UDate,CC,YY,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	IntegralEditIntegralDate,0,str10
	move	C0,N1
	move	NCMP2Upd,N1
	setitem	IntegralCheckUpdate,0,N1
	call	IntegralLoadCampaignScreenLower using NCMP2NUM
	return

IntegralLoadCamp2
	setitem	IntegralStatDetNum2,0,NLOL2Num
	setitem	IntegralEditDetType,0,NLOL2Type
	call	IntegralGetOSTAT using str25
	setitem	IntegralStatDetOstat2,0,str25
	setitem	IntegralEditDetIntegralID,0,NLOL2INum
	call	Trim using NLOL2UDate
	if (NLOL2UDate <> "")
		unpack	NLOL2UDate,CC,YY,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	IntegralEditDetIntegralDate,0,str10
	move	C0,N1
	move	NLOL2Upd,N1
	setitem	IntegralCheckDetUpdate,0,N1
	move	C1,N1
	move	NLOL2Type2,N1
	setitem	IntegralComboDetType,0,N1
	call	Trim using NLOL2FLD
	if (NLOL2FLD <> "")
		move	"LoadC.2-NLOL2TST",Location
		pack	KeyLocation,"Key: ",NLOL2FLD
		call	NLOL2TST
		if over
			setprop	IntegralDetNew,enabled=1
		else
			setprop	IntegralDetModify,enabled=1
		endif
	else
		setprop	IntegralDetNew,enabled=1
	endif
	return

IntegralLoadDetail
	call	Trim using NCMP2Num
	if (NCMP2Num = "")
		call	Trim using NCMPFLD
		if (NCMPFLD = "")
			setprop	Integral2GroupBox001,title="Record without a Campaign.",fgcolor=red
		else
			setprop	Integral2GroupBox001,title="Campaign without Cross Reference.",fgcolor=red
		endif
	else
		setprop	Integral2GroupBox001,title="Campaign",fgcolor=black
	endif
	setitem	Integral2StatCampNum2,0,NCMPFLD
	setitem	Integral2StatCampName2,0,NCMPCName
	setitem	Integral2EditIntegralID,0,NCMP2INum
	call	Trim using NCMP2UDate
	if (NCMP2UDate <> "")
		unpack	NCMP2UDate,CC,YY,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	Integral2EditIntegralDate,0,str10
	move	C0,N1
	move	NCMP2Upd,N1
	setitem	Integral2CheckUpdate,0,N1
.
	setitem	Integral2StatDetNum2,0,NLOL2Num
	call	IntegralGetOSTAT using str25
	setitem	Integral2StatDetOstat2,0,str25
	setitem	Integral2EditDetIntegralID,0,NLOL2INum
	call	Trim using NLOL2UDate
	if (NLOL2UDate <> "")
		unpack	NLOL2UDate,CC,YY,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	Integral2EditDetIntegralDate,0,str10
	move	C0,N1
	move	NLOL2Upd,N1
	setitem	Integral2CheckDetUpdate,0,N1
	move	C1,N1
	move	NLOL2Type2,N1
	setitem	Integral2ComboDetType,0,N1
	setprop	Integral2DetModify,enabled=1
	return

IntegralGetOSTAT LRoutine DimPtr
.DimPtr  = Return value - Status of Record
	if (NLOL2Type = "0")	.LOL Record
		move	"LOL Record",DimPtr
	else
		clear	DimPtr
.
		move	C1,NORDPATH
		pack	NORDFLD,NLOL2Num
		move	"NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		if not over
			if (OSTAT = "l")
				move	"LCR Record",DimPtr
			elseif (OSTAT = "p")
				move	"Pending Order Record",DimPtr
			elseif (OSTAT = "z")
				move	"Cancelled LCR",DimPtr
			elseif (OSTAT = "x")
				move	"Cancelled Pending Order",DimPtr
			elseif (OSTAT = "0")
				move	"Live Order",DimPtr
			elseif (OSTAT = "X")
				move	"Cancelled Order",DimPtr
			elseif (OSTAT = "B")
				move	"Billed Order",DimPtr
			elseif (OSTAT = "Q")
				move	"Cancelled Billed Order",DimPtr
			endif
		endif
	endif
	return

IntegralLoadCampaignScreen LRoutine FrmPtr,DimPtr
.FrmPtr  = Update Type:  '0' = Campaign Number Search only, '1' = Include Update byte Search
.DimPtr  = Campaign Number
.You must have a valid value in at least one of the parameters.  Calling routine is responsible
.to test for validity.
	IntegralListView.DeleteAllItems
.
	clear	NCMP2FLD3
	call	Trim using DimPtr
	if (DimPtr = "")
		clear	NCMP2FLD2
	else
		pack	NCMP2FLD2,"01X",DimPtr
	endif
.
	if (FrmPtr = C0)	.Look up via Campaign Number Only
		clear	NCMP2FLD4
	else
		pack	NCMP2FLD4,"03X1"
	endif
.
	move	"Load-NCMP2AIM",Location
	pack	KeyLocation,"Key: ",NCMP2FLD2,COMMA,NCMP2FLD4
	call	NCMP2AIM
	loop
		until over
		call	IntegralLoadCampaignListView
		move	"Load-NCMP2KG",Location
		pack	KeyLocation,"Key: ",NCMP2FLD2,COMMA,NCMP2FLD4
		call	NCMP2KG
	repeat
	IntegralListView.GetItemCount giving howmany
	move	howmany,str9
	call	Trim using str9
	call	FormatNumeric using str9,str11
	pack	str45,str11," Record(s) found."
	setitem	IntegralStatCampRec,0,str45
	IntegralListView.SetItemState giving N9 using 0,2,2
	IntegralListView.EnsureVisible using 0,0
	call	Click_IntegralListView
	return

IntegralLoadCampaignListView
	IntegralListView.InsertItem giving N9 using NCMP2Num
	pack	NCMPFLD,NCMP2Num
	move	"LoadLV-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	IntegralListView.SetItemText using N9,NCMPCName,1
	IntegralListView.SetItemText using N9,NCMP2INum,2
.START PATCH 1.1 REPLACED LOGIC
.	move	NCMPMlr,COMPFLD3
.	move	"LoadLV-COMPKEY3",Location
.	pack	KeyLocation,"Key: ",COMPFLD3
.	call	COMPKEY3
	move	NCMPMlr,COMPFLD
	move	"LoadLV-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
.END PATCH 1.1 REPLACED LOGIC
	IntegralListView.SetItemText using N9,COMPCOMP,3
	if (NCMP2Upd = "1")
		move	YES,str1
	else
		clear	str1
	endif
	IntegralListView.SetItemText using N9,str1,4
	pack	hold,NCMP2VARS,NCMPCName,COMPCOMP
	IntegralListView.SetItemText using N9,hold,5
	return

IntegralLoadCampaignScreenLower LRoutine DimPtr
.DimPtr  = Campaign Number
	IntegralListViewDetail.DeleteAllItems
	move	DimPtr,NORDFLDC		.Must load this way before actual call as DimPtr will become corrupted
	move	DimPtr,NLOLFLD1
	move	C2,NLOLPATH
	move	"LoadLower-NLOLKEY",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	call	NLOLKEY
	if over
		goto IntegralLoadCampaignScreenLower2
	endif
	move	"LoadLower-NLOLKS",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	loop
		until (NLOLCNUM	<> NLOLFLD1)
		call	IntegralLoadDetailListView using NLOLLOL,C0
		call	NLOLKS
		until over
	repeat
IntegralLoadCampaignScreenLower2
	move	C4,NORDPATH
	move	"LoadLower-NORDKEY",Location
	pack	KeyLocation,"Key: ",NORDFLDC
	call	NORDKEY
	if not over
		move	"LoadLower-NORDKS",Location
		pack	KeyLocation,"Key: ",NORDFLDC
		loop
			until (OCAMP <>	NORDFLDC)
			call	IntegralLoadDetailListView using OLRN,C1
.Need to reestablish NORDPATH at each iteration	as above routines call other routines which MAY	reset NORDPATH to C1
			move	C4,NORDPATH
			call	NORDKS
			until over
		repeat
	endif
.Make sure at least one	record is here
	IntegralListViewDetail.GetItemCount giving result
	move	result,str9
	call	Trim using str9
	call	FormatNumeric using str9,str11
	pack	str45,str11," Record(s) Found."
	setitem	IntegralStatDetailRec,0,str45
	IntegralListViewDetail.SetItemState giving N9 using 0,2,2
	IntegralListViewDetail.EnsureVisible using 0,0
	call	Click_IntegralListViewDetail
	return

IntegralLoadDetailListView LRoutine DimPtr,FrmPtr
.DimPtr  = LOL/LR Number
.FrmPtr  = Record Type:  '0'=LOL, '1'=LR
	pack	NLOL2FLD1,"01X",DimPtr
	pack	NLOL2FLD3,"03X",FrmPtr
	rep	zfill,NLOL2FLD3
	clear	NLOL2FLD2
	clear	NLOL2FLD4
	clear	NLOL2FLD5
	move	"LoadDLV-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
	call	NLOL2AIM
	if over
		move	DimPtr,NLOL2NUM
		move	FrmPtr,NLOL2Type
		pack	NLOL2INum,B55
		pack	NLOL2Type2,B55
		pack	NLOL2Upd,B55
		pack	NLOL2UDate,B55
		pack	NLOL2Filler,B55
		move	"0xFF0000",colordim		.Red
	else
		move	"0x000000",colordim		.Black
	endif
.
	IntegralListViewDetail.InsertItem giving N9 using NLOL2Num
	call	IntegralGetOSTAT using str25
	IntegralListViewDetail.SetItemText using N9,str25,1
	IntegralListViewDetail.SetItemText using N9,NLOL2INum,2
	if (NLOL2Upd = "1")
		move	YES,str1
	else
		clear	str1
	endif
	IntegralListViewDetail.SetItemText using N9,str1,3
	if (NLOL2Type2 = "2")
		move	"LOL to LR Conversion",str25
	elseif (NLOL2Type2 = "3")
		move	"Deletion",str25
	else
		clear	str25
	endif
	IntegralListViewDetail.SetItemText using N9,str25,4
	pack	hold,NLOL2VARS
	IntegralListViewDetail.SetItemText using N9,hold,5
	IntegralListViewDetail.SetItemText using N9,colordim,6
	return

IntegralVerifyCampaign
	if (NewFlag = YES)
		getitem	IntegralEditCampNum,0,NCMP2Num
		call	Trim using NCMP2Num
		if (NCMP2Num = "")
			alert	note,"Valid Campaign Number Required!!",result
			setfocus IntegralEditCampNum
			move	YES,ReturnFlag
			return
		endif
		pack	NCMPFLD,NCMP2Num
		move	C1,NCMPPATH
		move	"Verify-NCMPTST",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPTST
		if over
			alert	note,"Valid Campaign Number Required!!",result
			setfocus IntegralEditCampNum
			move	YES,ReturnFlag
			return
		endif
		pack	NCMP2FLD,NCMP2Num
		move	C1,NCMP2PATH
		move	"Verify-NCMP2TST",Location
		pack	KeyLocation,"Key: ",NCMP2FLD
		call	NCMP2TST
		if not over
			alert	note,"A Cross Reference Record for this Campaign already exists!!",result
			setfocus IntegralEditCampNum
			move	YES,ReturnFlag
			return
		endif
	endif
	getitem	IntegralEditIntegralID,0,NCMP2INum
	call	Trim using NCMP2INum
	if (NCMP2INum = "")
		alert	note,"Valid Integral ID Required!!",result
		setfocus IntegralEditIntegralID
		move	YES,ReturnFlag
		return
	endif
	getitem	IntegralEditIntegralDate,0,str10
	call	Trim using str10
	call	RemoveChar using str10,SLASH
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	NCMP2UDate,CC,YY,MM,DD
	else
		clear	NCMP2UDate
	endif
	getitem	IntegralCheckUpdate,0,N1
	if (N1 = C1)
		move	C1,NCMP2Upd
	else
		clear	NCMP2Upd
	endif
	return

IntegralVerifyDetail
	getitem	IntegralEditDetIntegralID,0,NLOL2INum
	call	Trim using NLOL2INum
	if (NLOL2INum = "")
		alert	note,"Valid Integral ID Required!!",result
		setfocus IntegralEditDetIntegralID
		move	YES,ReturnFlag
		return
	endif
.
	if (NewFlag = YES)
		getitem	IntegralEditDetType,0,NLOL2Type
.
		pack	NLOL2FLD,NLOL2Num,NLOL2INum,NLOL2Type
		move	"Ver.Det.-NLOL2TST",Location
		clear	KeyLocation
		call	NLOL2TST
		if not over
			alert	note,"Detail Cross Reference Record Already Exists!!",result
			setfocus IntegralEditDetIntegralID
			move	YES,ReturnFlag
			return
		endif
	endif
	getitem	IntegralEditDetIntegralDate,0,str10
	call	Trim using str10
	call	RemoveChar using str10,SLASH
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	NLOL2UDate,CC,YY,MM,DD
	else
		clear	NLOL2UDate
	endif
	getitem	IntegralCheckDetUpdate,0,N1
	if (N1 = C1)
		move	C1,NLOL2Upd
	else
		clear	NLOL2Upd
	endif
	getitem	IntegralComboDetType,0,N1
	if (N1 > C1)
		move	N1,NLOL2Type2
	else
		clear	NLOL2Type2
	endif
	return

IntegralLoadDetailScreen LRoutine DimPtr,FrmPtr,FrmPtr1
.DimPtr  = Record Number
.FrmPtr  = Update Flag:  '0' = LOL/LR Number Search only, '1' = Include Update byte Search
.FrmPtr1 = Update Type:  '2' = Conversion from LOL to LR, '3' = Marked for Deletion in Merge Manager
.You must have a valid value in at least one of the parameters.  Calling routine is responsible
.to test for validity.
	Integral2ListView.DeleteAllItems
.
	clear	NLOL2FLD2
	clear	NLOL2FLD3
	call	Trim using DimPtr
	if (DimPtr = "")
		clear	NLOL2FLD1
	else
		pack	NLOL2FLD1,"01X",DimPtr
	endif
.
	if (FrmPtr = C0)	.Look up via LOL/LR Number Only
		clear	NLOL2FLD5
	else
		pack	NLOL2FLD5,"05X1"
	endif
.
	if (FrmPtr1 = C1)	.Look up via Update Type
		clear	NLOL2FLD4
	else
		move	FrmPtr1,N1
		pack	NLOL2FLD4,"04X",N1
		rep	zfill,NLOL2FLD4
	endif
.
	move	"LoadD-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD4,COMMA,NLOL2FLD5
	call	NLOL2AIM
	loop
		until over
		call	Integral2LoadListView
		move	"LoadD-NLOL2KG",Location
		pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD4,COMMA,NLOL2FLD5
		call	NLOL2KG
	repeat
	Integral2ListView.GetItemCount giving howmany
	move	howmany,str9
	call	Trim using str9
	call	FormatNumeric using str9,str11
	pack	str45,str11," Record(s) found."
	setitem	Integral2StatRec,0,str45
	Integral2ListView.SetItemState giving N9 using 0,2,2
	Integral2ListView.EnsureVisible using 0,0
	call	Click_Integral2ListView
	return

Integral2LoadListView
	Integral2ListView.InsertItem giving N9 using NLOL2Num
	call	IntegralGetOSTAT using str25
	Integral2ListView.SetItemText using N9,str25,1
	Integral2ListView.SetItemText using N9,NLOL2INum,2
	if (NLOL2Upd = "1")
		move	YES,str1
	else
		clear	str1
	endif
	Integral2ListView.SetItemText using N9,str1,3
	if (NLOL2Type2 = "2")
		move	"LOL to LR Conversion",str25
	elseif (NLOL2Type2 = "3")
		move	"Deletion",str25
	else
		clear	str25
	endif
	Integral2ListView.SetItemText using N9,str25,4
	if (NLOL2Type = "0")	.LOL Record
		move	C1,NLOLPATH
		pack	NLOLFLD,NLOL2Num
		move	"2LoadLV-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
		move	NLOLCNum,NCMPFLD
	else			.LR/LCR Record
		move	C1,NORDPATH
		pack	NORDFLD,NLOL2Num
		move	"2LoadLV-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		move	OCAMP,NCMPFLD
	endif
	call	Trim using NCMPFLD
	if (NCMPFLD <> "")
		move	C1,NCMPPATH
		move	"2LoadLV-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
.
		pack	NCMP2FLD,NCMPFLD
		move	C1,NCMP2PATH
		move	"2LoadLV-NCMP2KEY",Location
		pack	KeyLocation,"Key: ",NCMP2FLD
		call	NCMP2KEY
		if over
			unpack	B55,NCMP2VARS
		endif
.
.START PATCH 1.1 REPLACED LOGIC
.		move	NCMPMlr,COMPFLD3
.		move	"2LoadLV-COMPKEY3",Location
.		pack	KeyLocation,"Key: ",COMPFLD3
.		call	COMPKEY3
		move	NCMPMlr,COMPFLD
		move	"2LoadLV-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
.END PATCH 1.1 REPLACED LOGIC
	else
		move	B55,NCMPFLD
		unpack	B55,NCMP2VARS
		pack	NCMPCNAME,B55,B55
		clear	COMPVARS
	endif
	Integral2ListView.SetItemText using N9,NCMPFLD,5
	Integral2ListView.SetItemText using N9,NCMP2INum,6
	Integral2ListView.SetItemText using N9,COMPCOMP,7
	pack	hold,NLOL2VARS,NCMP2VARS,NCMPFLD,NCMPCNAME
	Integral2ListView.SetItemText using N9,hold,8
	return	return

Integral2Verify
	getitem	Integral2EditDetIntegralID,0,NLOL2INum
	call	Trim using NLOL2INum
	if (NLOL2INum = "")
		alert	note,"Valid Integral ID Required!!",result
		setfocus Integral2EditDetIntegralID
		move	YES,ReturnFlag
		return
	endif
	getitem	Integral2EditDetIntegralDate,0,str10
	call	Trim using str10
	call	RemoveChar using str10,SLASH
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	NLOL2UDate,CC,YY,MM,DD
	else
		clear	NLOL2UDate
	endif
	getitem	Integral2CheckDetUpdate,0,N1
	if (N1 = C1)
		move	C1,NLOL2Upd
	else
		clear	NLOL2Upd
	endif
	getitem	Integral2ComboDetType,0,N1
	if (N1 > C1)
		move	N1,NLOL2Type2
	else
		clear	NLOL2Type2
	endif
	return
.......................................................................
.........................GUI HOUSEKEEPING..............................
.......................................................................
IntegralSwitchTab LRoutine FrmPtr
	if (TabNum <> FrmPtr)
		move	TabNum,N2
		call	IntegralTabClick
		move	FrmPtr,N2
		call	IntegralTabChange
		setitem	IntegralTabControl,0,FrmPtr
	endif
	return

IntegralTabClick
	if (N2 = C1)
		Deactivate Nint001a
	elseif (N2 = C2)
		Deactivate Nint001b
	endif
	return

IntegralTabChange
	move	N2,TabNum
	if (N2 = C1)
		Activate Nint001a
		setfocus IntegralSearchKey
	elseif (N2 = C2)
		Activate Nint001b
		setfocus Integral2EditSearch
	endif
	return


	include	nlol2io.inc
	include	ncmp2io.inc
	include	ncmpio.inc
	include	nordio.inc
	include	nlolio.inc
	include	compio.inc
	include	cntio.inc
	include	comlogic.inc

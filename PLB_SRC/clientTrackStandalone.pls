PC	EQU	1
	include	common.inc
	include cons.inc
	include cntdd.inc
	include compdd.inc
	include nexcdd.inc

release	init	"1.0"	7JULY2006	DMS


hold6		dim	226
ExitFlag6	init	"Y"
ReturnValue 	form 	1
ReturnFlag6	init	"N"
NewFlag6	init 	"N"
NEXCFLDBack	dim	10
NEXCFLDBack2	dim	10
startJulDate	dim	5
endJulDate	dim	5
font2	font
white	color
grey	color
	create 	font2,"Arial",size=8,bold=0
	create	white=*white
	create	grey=220:220:220
.
mss1	plform	Error
screen	plform	COMP001f
	formload	mss1
	formload	screen
	winhide
.Load List View Columns
	setfocus Comp001fEditInput
	COMP001fListViewDisplay.InsertColumn using "Client Name",150,0
	COMP001fListViewDisplay.InsertColumn using "Type",100,1
	COMP001fListViewDisplay.InsertColumn using "Start Date",100,2
	COMP001fListViewDisplay.InsertColumn using "End Date",100,3
	COMP001fListViewDisplay.InsertColumn using "Client Number",95,4
	COMP001fListViewDisplay.InsertColumn using "hold vars",0,5
	COMP001fListViewDisplay.InsertColumn using "jul start",0,6
	COMP001fListViewDisplay.InsertColumn using "jul end",0,7
. set up initial screen
	call	CltTrackDisableButtons
	call	CltTrackDisableFields
	call	CltTrackDisableList
	loop
		eventwait
	repeat
.  below are the routines
COMP001fAAMRead
	move	"Read-NEXCAIM",Location
	pack	KeyLocation,"Key: ",NEXCFLD1
	call	NEXCAIM
	loop
		until over
		pack	hold6,NEXCVARS
		call	CltTrackLoadListView
		move	"Read-NEXCKG",Location
		call	NEXCKG
	repeat
	call	CltTrackLoadRecord
	return
.
CltTrackLoadListView
	move	NEXCCLIENT,COMPFLD
	move	"CltTrackLoadListView-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	call	Trim using COMPCOMP
	COMP001fListViewDisplay.InsertItem giving result using COMPCOMP // creates new row and assigns value to 1st col
	if (NEXCTYPE="0")
		pack	str15,"Brokerage"
	elseif (NEXCTYPE="1")
		pack	str15,"List Management"
	else
		clear	str15
	endif
	COMP001fListViewDisplay.SetItemText using result, str15,1
	call	Trim using NEXCSDATE
	if (NEXCSDATE <> "")
		unpack	NEXCSDATE,cc,yy,mm,dd
		call	cvtjul
		move	juldays, startJulDate
		pack	str10,mm,SLASH,dd,SLASH,cc,yy
	else
		clear	str10
		move	C0,startJulDate
	endif
	COMP001fListViewDisplay.SetItemText using result, str10,2
	call	TRIM using NEXCEDATE
	if (NEXCEDATE <> "")
		unpack	NEXCEDATE,cc,yy,mm,dd
		call	cvtjul
		move	juldays, endJulDate
		pack	str10,mm,SLASH,dd,SLASH,cc,yy
		COMP001fListViewDisplay.SetItemText using result, str10,3
	else
		clear	str10
		move	C0,endJulDate
	endif
	COMP001fListViewDisplay.SetItemText using result, NEXCCLIENT,4
	COMP001fListViewDisplay.SetItemText using result, hold6,5
	COMP001fListViewDisplay.SetItemText using result, startJulDate,6
	COMP001fListViewDisplay.SetItemText using result, endJulDate,7
	return
.
CltTrackLoadRecord
	COMP001fListViewDisplay.GetItemCount giving result
	move	result, str9
	call	Trim using str9
	call	FormatNumeric using str9, str11
	pack	str35,str11," records found"
	setitem	COMP001fStatRecFnd,0,str35
	if (result <> 0)
		COMP001fListViewDisplay.SetItemState giving result using 0,2,2 // select listview obj 1
		call	Click_COMP001fListViewDisplay
	else
		call	CltTrackClearRecord
		call	CltTrackDisableList
		call	CltTrackDisableButtons
	endif
	return
.
CltTrackPopulateFields
	COMP001fListViewDisplay.GetNextItem giving result using C2
	COMP001fListViewDisplay.GetItemText giving hold6 using result,5 // populate hold6 with full record
	if (result = SEQ)  // no entries in LV
		call	CltTrackDisableButtons
		call	CltTrackDisableFields
		call	CltTrackEnableUpper
		call	CltTrackClearRecord
		setfocus COMP001fEditInput
		//call Click_COMP001fButtonOk
		return
	endif
	unpack	hold6,NEXCVARS
	setitem	COMP001fEditNumber,0, NEXCCLIENT
	if (NEXCTYPE="0")  // brokerage
		setitem	COMP001fComboType,0,3
	elseif (NEXCTYPE="1")  // list management
		setitem	COMP001fComboType,0,2
	else  // blank
		setitem	COMP001fComboType,0,1
	endif
	setitem	COMP001fEditID,0, NEXCNUM
	Call	TRIM using NEXCCLIENT
	call	Trim using NEXCSDATE
	if (NEXCSDATE <> "")
		unpack	NEXCSDATE,cc,yy,mm,dd
		pack	str10,mm,SLASH,dd,SLASH,cc,yy
	else
		clear	str10
	endif
	setitem	COMP001fEditStartDate,0,str10
	call	TRIM using NEXCEDATE
	if (NEXCEDATE <> "")
		unpack	NEXCEDATE,cc,yy,mm,dd
		pack	str10,mm,SLASH,dd,SLASH,cc,yy
	else
		clear	str10
	endif
	setitem	COMP001fEditEndDate,0,str10
	call	TRIM using NEXCNOTES
	setitem	COMP001fEditNotes,0,NEXCNOTES
	move	NEXCCLIENT,COMPFLD
	move	"CltTrackPopulateFields-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	call	Trim using COMPCOMP
	setitem	COMP001fStatName,0,COMPCOMP
.because record is found, do the following:
	call	CltTrackDisableButtons
	call	CltTrackDisableFields
	call	CltTrackEnableList // be able to click in lv
	call	CltTrackEnableUpper
	setprop	COMP001fButtonModify, enabled=1
	return
.
CltTrackDisableList
	setprop	COMP001fListViewDisplay, enabled=0
	return
.
CltTrackEnableList
	setprop	COMP001fListViewDisplay, enabled=1
	return
.
CltTrackDisableFields
	setprop	COMP001fEditNumber,enabled=0,bgcolor=grey,readonly=1
	setprop	COMP001fComboType,enabled=0,bgcolor=grey
	setprop	COMP001fEditID,enabled=0,bgcolor=grey
	setprop	COMP001fEditStartDate,enabled=0,bgcolor=grey
	setprop	COMP001fEditEndDate,enabled=0,bgcolor=grey
	setprop	COMP001fEditNotes,enabled=0,bgcolor=grey
	return
.
CltTrackEnableFields
	setprop	COMP001fEditNumber,enabled=1,bgcolor=white
	setprop	COMP001fComboType,enabled=1,bgcolor=white
	setprop	COMP001fEditID,enabled=1,bgcolor=white
	setprop	COMP001fEditStartDate,enabled=1,bgcolor=white
	setprop	COMP001fEditEndDate,enabled=1,bgcolor=white
	setprop	COMP001fEditNotes,enabled=1,bgcolor=white
	return
.
CltTrackClearRecord
	setitem	COMP001fEditNumber,0,""
	setitem	COMP001fComboType,1,""
	setitem	COMP001fEditID,0,""
	setitem	COMP001fStatName,0,""
	setitem	COMP001fEditStartDate,0,""
	setitem	COMP001fEditEndDate,0,""
	setitem	COMP001fEditNotes,0,""
	return
.
CltTrackDisableButtons
	setprop	COMP001fButtonModify,enabled=0
	setprop	COMP001fButtonDelete,enabled=0
	setprop	COMP001fButtonSave,enabled=0
	setprop	COMP001fButtonQuit,enabled=0
	return
.
CltTrackEnableButtons
	setprop	COMP001fButtonSave,enabled=1
	setprop	COMP001fButtonQuit,enabled=1
	return
.
CltTrackEnableUpper
	move	"Y", ExitFlag6
	setprop COMP001fButtonNew,enabled=1
	setprop Comp001fEditInput,enabled=1,bgcolor=white
	setprop COMP001fButtonOK,enabled=1
	setprop COMP001fButtonExit,enabled=1
	return
.
CltTrackDisableUpper
	move	"N",ExitFlag6
	setprop COMP001fButtonNew,enabled=0
	setprop COMP001fButtonModify,enabled=0
	setprop	Comp001fEditInput,enabled=0,bgcolor=grey
	setprop	COMP001fButtonOK,enabled=0
	setprop	COMP001fButtonExit,enabled=0
	return
.
CltTrackButtonQuit
	call	CltTrackPopulateFields
	return
.
CltTrackButtonModify
	call	CltTrackDisableUpper
	call	CltTrackDisableList
	call	CltTrackEnableFields
	call	CltTrackEnableButtons
	setprop	COMP001fButtonDelete, enabled=1
	setfocus COMP001fEditNumber
	return
.
CltTrackButtonSave
	move	"N",ReturnFlag6
	call	CltTrackVerifyInput
	if (ReturnFlag6 = "Y")
		return
	endif
	if (NewFlag6 = "Y")
		pack	NEXCFLD,NEXCCLIENT,NEXCTYPE,NEXCNUM
		move	"T.SaveNew-NEXCTST",Location
		pack	KeyLocation,"Key: NEXCFLD"
		call	NEXCTST
		if not over
			alert	note,"That record already exists!", result
			call	CltTrackButtonQuit
			return
		endif
		move	"T.SaveNew-NEXCWRT",Location
		pack	KeyLocation,"Key: NEXCWRT"
		call	NEXCWRT
		move	"N",NewFlag6
	else
		pack	NEXCFLD,NEXCCLIENT,NEXCTYPE,NEXCNUM
		move	"T.SaveMod.-NEXCTST",Location
		pack	KeyLocation,"Key: NEXCFLD"
		call	NEXCTST
		if over
			// should never happen
			alert	note,"Record no longer exists!", result
			call	CltTrackButtonQuit
			return
		endif
		move	"CltTrackButtonSave-NEXCUPD",Location
		pack	KeyLocation,"Key: NEXCFLD"
		call	NEXCUPD  // needs previous key read
	endif
	call	Click_COMP001fButtonOK
	return
.
CltTrackVerifyInput
	getitem	COMP001fEditNumber,0,NEXCCLIENT
	call 	TRIM using NEXCCLIENT
	if (NewFlag6 = YES)
		if (NEXCCLIENT = "")
			alert	caution,"Valid 6 digit Client Number required!",result
			setfocus COMP001fEditNumber
			move	"Y",ReturnFlag6
			return
		else
			call	ZFillIt using NEXCCLIENT
			setitem	COMP001fEditNumber,0,NEXCCLIENT
			move	NEXCCLIENT,COMPFLD
			move	"T.Verify-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			if over
				alert	caution,"Valid 6 digit Client Number required!",result
				setfocus COMP001fEditNumber
				move	"Y",ReturnFlag6
				return
			endif
		endif
	endif
.
	getitem COMP001fComboType,0,N1
	if (N1 <= C1)
		alert	caution,"Client Type required!", result
		setfocus COMP001fComboType
		move	"Y",ReturnFlag6
		return
	elseif (N1 = C2)
		pack	NEXCTYPE,"1"
	elseif (N1 = C3)
		pack	NEXCTYPE,"0"
	endif
.
	getitem COMP001fEditID,0,NEXCNUM
	call	TRIM using NEXCNUM
	if (NewFlag6 <> YES)
		if (NEXCNUM = "")
			pack	taskname,"3 digit Unique Id required!",newline,"Please contact I.S."
			alert	caution,taskname,result
			setfocus COMP001fEditID
			move	"Y",ReturnFlag6
			return
		endif
	else
		move	C0,N3
		loop
			add	C1,N3
			if (N3 >= "998")
				pack	taskname,"3 digit Unique Id required!",newline,"Please contact I.S."
				alert	caution,taskname,result
				setfocus COMP001fEditID
				move	"Y",ReturnFlag6
				return
			endif
			move	N3,NEXCNUM
			rep	zfill,NEXCNUM
			pack	NEXCFLD,NEXCCLIENT,NEXCTYPE,NEXCNUM
			move	"T.Ver.New-NEXCTST",Location
			pack	KeyLocation,"Key: NEXCFLD"
			call	NEXCTST
			until over
		repeat
	endif
.
	getitem COMP001fEditStartDate,0,str10
	call 	RemoveChar,str10,SLASH
	call	TRIM using str10
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	NEXCSDATE,CC,YY,MM,DD
	else
		clear	NEXCSDATE
	endif
.
	getitem COMP001fEditEndDate,0,str10
	call 	RemoveChar,str10,SLASH
	call	TRIM using str10
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	NEXCEDATE,CC,YY,MM,DD
	else
		clear	NEXCEDATE
	endif
.
	getitem	COMP001fEditNotes,0,NEXCNOTES
	return
.
CltTrackButtonDelete
	alert	plain,"Are you sure you want to delete the selected record(s)?",result
	if (result = 1)  // yes
		move	SEQ,result
		move	result,N9
		loop
			move	result,N9
			COMP001fListViewDisplay.GetNextItem giving result using C2,N9  // -1 is error code
			until (result = SEQ)
			COMP001fListViewDisplay.GetItemText giving hold6 using result,5
			unpack	hold6,NEXCVARS
			pack	NEXCFLD,NEXCCLIENT,NEXCTYPE,NEXCNUM
			move	"CltTrackButtonDelete-NEXCTST",Location
			pack	KeyLocation,"Key: NEXCFLD"
			call	NEXCTST	// give valid read
			if over
				alert	note,"no valid read", result
				return
			endif
			move	"CltTrackButtonDelete-NEXCDEL",Location
			pack	KeyLocation,"Key: NEXCFLD"
			call	NEXCDEL
		repeat
		call	CLICK_COMP001fButtonOK  // same as previous ok click
	else
		call	CltTrackButtonQuit
		return
	endif
	return
.
CltTrackButtonNew
	call	CltTrackClearRecord
	call	CltTrackDisableUpper
	call	CltTrackDisableList
	call	CltTrackEnableFields
	call	CltTrackEnableButtons
	move	"Y",NewFlag6
	setprop	COMP001fEditNumber,readonly=0
	setfocus COMP001fEditNumber
	return
.
FileExit
	if (ExitFlag6 = "Y")  // yes, it's ok to shutdown
		shutdown
	endif
	return
.
	include	cntio.inc
	include	compio.inc
	include	nexcio.inc
	include	comlogic.inc
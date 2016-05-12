PC	EQU	1
........................................
. Program:      WebProcFileMaint.PLS
. Function:     File Management for website processes
. Author:       David Strahan
. Date:         January 18,2006
. Release:      1.0
. Notes:
........................................
.
.Include Files
	include common.inc
        include cons.inc
        include       NWEBDD2.INC
.
Release 	Init	"1.0"
.
DimPtr		dim	^
hold		dim	160	// holding space for NINWEB2
NewFlag 	init	"N"	// always N unless user clicks the new button
ReturnFlag 	init	"N"	// always N unless verifyData finds incomplete record
ExitFlag	init	"N"	// if New or Modify, should not be able to exit- save or quit
.

font2   font
.
white	color
grey	color
        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220
.
x	plform	WebProcFileMaint
mss1	plform	Error
	winhide
	formload x
	formload mss1
.
.Load ListView Columns
	WebListView.InsertColumn using "ID",50,0
	WebListView.InsertColumn using "Name",250, 1
	WebListView.InsertColumn using "Location",225,2
	WebListView.InsertColumn using "Complete Record",0,3
	setfocus WebEditSearch
	call WebDisableLower	// initialize to disable
	setprop WebNew, enabled=1 //allow new
.
	loop

		eventwait
	repeat
.
fileExit
	if (ExitFlag != "Y")	// Y means we're in middle of save or modify
		shutdown
	endif
	return
.
WebSeqRead
	CLOSE	NWEB2FLIST
	MOVE	c0,nweb2flag
	loop
		move	"WebOK-NWEB2SEQ",Location
		pack	KeyLocation,"Key: SEQ"
		call NWEB2SEQ
		until over
		call WebLoadListView
	repeat
	call WebLoadRecord
	return
.
WebXSeqRead
	CLOSE	NWEB2FLIST
	MOVE	c0,nweb2flag
	move	"WebOK-NWEB2EOF",Location
	pack	KeyLocation,"Key: SEQ"
	call NWEB2EOF
	loop
		move	"WebOK-NWEB2BAK",Location
		pack	KeyLocation,"Key: SEQ"
		call NWEB2BAK
		until over
		call WebLoadListView
	repeat
	call WebLoadRecord
	return
.
WebISIRead Routine DimPtr
.Calling routine has already trimmed passed variable
	move	DimPtr,NWEB2FLD
	call	ZFillIt using NWEB2FLD
	move	"WebOK-NWEB2KEY",Location
	pack	KeyLocation,"Key: ",NWEB2FLD
	call	NWEB2KEY
	if not over
		call WebLoadListView	// load ListViewObject
	endif
	call WebLoadRecord
	return
.
WebAAMRead Routine DimPtr
.Calling routine has already trimmed passed variable
	pack	NWEB2FLD1,"01F",DimPtr
	clear	NWEB2FLD2
	move	"WebOK-NWEB2AIM",Location
	pack	KeyLocation,"Key: ",NWEB2FLD1,comma,NWEB2FLD2
	call 	NWEB2AIM
	loop
		until over
		call	WebLoadListView
		call	NWEB2KG
	repeat
	call WebLoadRecord
	return
.
WebLoadListView	// insert ListView item
	WebListView.InsertItem giving result using NWEB2RECORD.NWEB2CODE //creates new row and assigns value to 1st col
	call	Trim using NWEB2RECORD.NWEB2NAME
	WebListView.SetItemText using result,NWEB2RECORD.NWEB2NAME,1 // col 2
	call	Trim using NWEB2RECORD.NWEB2LOCATION
	WebListView.SetItemText using result,NWEB2RECORD.NWEB2LOCATION,2 //col 3
	pack	hold, NWEB2RECORD.NWEB2CODE, NWEB2RECORD.NWEB2NAME, NWEB2RECORD.NWEB2LOCATION
	WebListView.SetItemText using result,hold,3 // col 4
	return
.
WebLoadRecord
.	display ListView objects and selects first obj
.	load first obj fields into variables
.
.
	WebListView.GetItemCount giving result
	compare result, c0
	if zero
		SetItem WebBlankRecords, 0, "0 records found"
		call WebDisableLower	// disable all lower
		//call WebEnableUpper	// to get new access
		setprop WebNew, enabled=1 // except new
		return
	endif
	WebListView.SetItemState giving result using 0,2,2 // select listview obj 1
	WebListView.EnsureVisible using 0,0 // scroll to it if necessary


	WebListView.GetNextItem giving result using C2  // loads top object into variables
	WebListView.GetItemText giving NWEB2RECORD.NWEB2CODE using result
	WebListView.GetItemText giving NWEB2RECORD.NWEB2NAME using result, 1
	WebListView.GetItemText giving NWEB2RECORD.NWEB2LOCATION using result, 2
.
	call WebLoadCurrentLV //original
	call WebDisableLower2
	call 	WebPutRecFound
	return
.
WebPutRecFound	// output # records found
	WebListView.GetItemCount giving n11
	move	n11, str11
	call	Trim using str11
	pack	str45,str11," records found"
	SetItem WebBlankRecords, 0, str45
	return
.
WebEnableUpper
	setprop WebOk, enabled=1
	setprop WebExit, enabled=1
	setprop WebEditSearch, enabled=1, bgcolor=white
	setprop WebListView, enabled=1
	move "N", ExitFlag
	return
.
WebDisableUpper
	move "Y", ExitFlag
	setprop WebOK, enabled=0
	setprop WebExit, enabled=0
	setprop WebEditSearch, enabled=0, bgcolor=grey
	setprop WebListView, enabled=0
	return
.
WebDisableLower
	setprop WebNew, enabled=0
	setprop WebModify, enabled=0
	setprop WebDelete, enabled=0
	setprop WebSave, enabled=0
	setprop WebQuit, enabled=0
	setprop WebEditNumber, enabled=0, bgcolor=grey
	setprop WebEditName, enabled=0, bgcolor=grey
	setprop WebEditLocation, enabled=0, bgcolor=grey
	move "N", NewFlag
	return
.
WebDisableLower2	// new, modify, delete remain enabled
	setprop WebNew, enabled=1
	setprop WebModify, enabled=1
	setprop WebDelete, enabled=1
	setprop WebSave, enabled=0
	setprop WebQuit, enabled=0
	setprop WebEditNumber, enabled=0, bgcolor=grey
	setprop WebEditName, enabled=0, bgcolor=grey
	setprop WebEditLocation, enabled=0, bgcolor=grey
	move "N", NewFlag
	return
.
WebEnableLower	// leave new, WebEditNumber, modify as disabled
	setprop WebNew, enabled=0
	setprop WebModify, enabled=0
	setprop WebDelete, enabled=1
	setprop WebSave, enabled=1
	setprop WebQuit, enabled=1
	setprop WebEditName, enabled=1, bgcolor=white
	setprop WebEditLocation, enabled=1, bgcolor=white
	return
.
WebLoadCurrentLV
	setitem	WebEditNumber,0,NWEB2RECORD.NWEB2CODE
	setitem	WebEditName,0,NWEB2RECORD.NWEB2NAME
	setitem	WebEditLocation,0,NWEB2RECORD.NWEB2LOCATION
	return
.
WebNewButton
	move "Y", NewFlag
	call WebDisableUpper
	call WebEnableLower
	setprop WebDelete, enabled=0
.
	call GetNextId
		if (NWEB2FLD="void")
			call	WebQuitButton
			return	// no space for data - exit routine
		endif
	setitem WebEditNumber, 0, str4
	return
.
WebQuitButton
.	undo changes
	call WebEnableUpper
	call WebDisableLower
	//setprop WebNew, enabled=1	// allow new
	setprop WebModify, enabled=1	// allow modify
	call CLICK_WebOK	// same as previous WebOK click
	return
.
WebDeleteButton
	alert plain, "Are you sure you want to delete this record?", result
	if (result = 1)	// yes
		move NWEB2RECORD.NWEB2CODE, NWEB2FLD
		call NWEB2TST	// give valid read
		if over
		endif
		call NWEB2DEL	// needs to be preceeded by valid read
		call WebEnableUpper
		call WebDisableLower2
		call CLICK_WebOK	// same as previous WebOK click
		return
	endif
	if (result = 2)	// no
		call WebQuitButton
		return
	endif

	if (result = 3)	// cancel
		call WebQuitButton
		return
	endif
.
WebClearRecord
	WebEditNumber.SelectAll
	WebEditNumber.Clear
	WebEditName.SelectAll
	WebEditName.Clear
	WebEditLocation.SelectAll
	WebEditLocation.Clear
	return
.

WebSaveButton
	call WebVerifyData
	if (ReturnFlag = "Y")
		return
	endif
	if (NewFlag = "Y")	// it's a new record
		call WebDisableLower	// has to be called here - changes value of new flag
		move	NWEB2RECORD.NWEB2CODE,NWEB2FLD
		call NWEB2WRT	// write the record
	elseif (NewFlag = "N") // it's a modify
		call WebDisableLower	// has to be called here - changes value of new flag

		//call WebListViewClick2  // new

		call NWEB2TST	// gives read
		if over
		//Should never happen
			alert	note,"Record no longer Exists!",result
			call WebQuitButton
			return
		endif
		call NWEB2UPD // needs previous key read
	endif
	call WebEnableUpper
	//move	nweb2fld,str4
	call	Click_WebOK //old
.
	WebListView.FindItem giving result using 0, NWEB2FLD
	WebListView.SetItemState using result,2,2
	WebListView.EnsureVisible using result, 0
	call	Click_WebListView
	return
.
WebVerifyData
	getitem WebEditNumber,0,NWEB2RECORD.NWEB2CODE
	call    TRIM using NWEB2RECORD.NWEB2CODE
	count   HowMany,NWEB2RECORD.NWEB2CODE
	if (howmany = 0)
		alert caution,"Number Required!",result
		setfocus WebEditNumber
		move "Y", ReturnFlag
		return
	endif
	call	ZFillIt using NWEB2RECORD.NWEB2CODE
.
	getitem WebEditName,0,NWEB2RECORD.NWEB2NAME
	call    TRIM using NWEB2RECORD.NWEB2NAME
	count   HowMany,NWEB2RECORD.NWEB2NAME
	if (howmany = 0)
		alert caution,"Name Required!",result
		setfocus WebEditName
		move "Y", ReturnFlag
		return
	endif
.
	getitem WebEditLocation,0,NWEB2RECORD.NWEB2LOCATION
        call    TRIM using NWEB2RECORD.NWEB2LOCATION
        count   HowMany,NWEB2RECORD.NWEB2LOCATION
        if (howmany = 0)
            	alert caution,"Location Required!",result
                setfocus WebEditLocation
                move "Y", ReturnFlag
                return
	endif
        move "N", ReturnFlag	// record is complete!
        return
.
WebModifyButton
	move	NWEB2RECORD.NWEB2CODE,NWEB2FLD
	call	NWEB2TST
	call WebDisableUpper
	call WebEnableLower
	return
.
GetNextId
	move	C1,N4
	loop
		move	N4, NWEB2FLD
		rep	zfill,NWEB2FLD
		call	NWEB2TST
		until over
		add	C1,N4
		until (N4 = "9999")
	repeat

	if (N4 = "9999")
		move	"void",NWEB2FLD
		pack	taskname,"Maximum number of records has been exceeded.",newline,"Please contact Information Services!"
		alert	note,taskname,result
		return
	endif
	move	n4, str4
	rep	zfill, str4
	return

	include	nwebio2.inc
	include	comlogic.inc
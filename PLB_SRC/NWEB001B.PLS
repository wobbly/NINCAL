PC	EQU	1
........................................
. Program:      NWEB001b.PLS
. Function:     File Management for Web2site processes
. Author:       David Strahan
. Date:         January 18,2006
. Release:      1.0
. Notes:
........................................
.
.Include Files
	include common.inc
        include cons.inc
        include	NWebDD2.INC
        include	npasdd.inc
.
Release 	Init	"1.0"
.
DimPtr		dim	^
WindPtr		Window 	^
hold2		dim	160	// holding space for NINWeb2
NewFlag2 	init	"N"	// always N unless user clicks the new button
ReturnFlag2 	init	"N"	// always N unless verifyData finds incomplete record
ExitFlag	dim	1	// dave
PassFlag1	init	"N"
.

	SHUTDOWN	// requires calling program
.
LoadListViewColumns routine WindPtr
font2   font

white	color
grey	color
        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220

x	plform	NWEB001B
mss1	plform	Error
pass	plform	Passwrd
	winhide
	formload x, WindPtr
	formload mss1
	formload pass
	Web2ListView.InsertColumn using "ID",50,0
	Web2ListView.InsertColumn using "Name",250, 1
	Web2ListView.InsertColumn using "Location",225,2
	Web2ListView.InsertColumn using "Complete Record",0,3
	setfocus Web2EditSearch
	call Web2DisableLower	// initialize to disable
	setprop Web2New, enabled=1 //allow new
	loop

		eventwait
	repeat
	return
.
ActivateForm routine	// allow parent to activate screen
	activate x
	return
.
DeactivateForm routine  // allow parent to deactivate screen
	deactivate x
	return
.
fileExit
	call "NWEB0001;fileExit" // call exit in parent program
	return
.
Web2SeqRead
	CLOSE	NWeb2FLIST
	MOVE	c0,nWeb2flag
	loop
		move	"Web2SeqRead-NWeb2SEQ",Location
		pack	KeyLocation,"Key: SEQ"
		call NWeb2SEQ
		until over
		call Web2LoadListView
	repeat
	call Web2LoadRecord
	return
.
Web2XSeqRead
	CLOSE	NWeb2FLIST
	MOVE	c0,nWeb2flag
	move	"Web2XSeqRead-NWeb2EOF",Location
	pack	KeyLocation,"Key: SEQ"
	call NWeb2EOF
	loop
		move	"Web2XSeqRead-NWeb2BAK",Location
		pack	KeyLocation,"Key: SEQ"
		call NWeb2BAK
		until over
		call Web2LoadListView
	repeat
	call Web2LoadRecord
	return
.
Web2ISIRead Routine DimPtr
.Calling routine has already trimmed passed variable
	move	DimPtr,NWeb2FLD
	call	ZFillIt using NWeb2FLD
	move	"Web2ISIRead-NWeb2KEY",Location
	pack	KeyLocation,"Key: ",NWeb2FLD
	call	NWeb2KEY
	if not over
		call Web2LoadListView	// load ListViewObject
	endif
	call Web2LoadRecord
	return
.
Web2AAMRead Routine DimPtr
.Calling routine has already trimmed passed variable
	pack	NWeb2FLD1,"01F",DimPtr
	clear	NWeb2FLD2
	move	"Web2AAMRead-NWeb2AIM",Location
	pack	KeyLocation,"Key: ",NWeb2FLD1,comma,NWeb2FLD2
	call 	NWeb2AIM
	loop
		until over
		call	Web2LoadListView
		move	"Web2AAMRead-NWeb2KG",Location
		pack	KeyLocation,"Key: ",NWeb2FLD1,comma,NWeb2FLD2
		call	NWeb2KG
	repeat
	call Web2LoadRecord
	return
.
Web2LoadListView	// insert ListView item
	Web2ListView.InsertItem giving result using NWeb2RECORD.NWeb2CODE //creates new row and assigns value to 1st col
	call	Trim using NWeb2RECORD.NWeb2NAME
	Web2ListView.SetItemText using result,NWeb2RECORD.NWeb2NAME,1 // col 2
	call	Trim using NWeb2RECORD.NWeb2LOCATION
	Web2ListView.SetItemText using result,NWeb2RECORD.NWeb2LOCATION,2 //col 3
	pack	hold2, NWeb2RECORD.NWeb2CODE, NWeb2RECORD.NWeb2NAME, NWeb2RECORD.NWeb2LOCATION
	Web2ListView.SetItemText using result,hold2,3 // col 4
	return
.
Web2LoadRecord
.	display ListView objects and selects first obj
.	load first obj fields into variables
.
.
	Web2ListView.GetItemCount giving result
	compare result, c0
	if zero
		SetItem Web2BlankRecords, 0, "0 records found"
		call Web2DisableLower	// disable all lower
		setprop Web2New, enabled=1 // except new
		return
	endif
	Web2ListView.SetItemState giving result using 0,2,2 // select listview obj 1
	Web2ListView.EnsureVisible using 0,0 // scroll to it if necessary
.
	Web2ListView.GetNextItem giving result using C2  // loads top object into variables
	Web2ListView.GetItemText giving NWeb2RECORD.NWeb2CODE using result
	Web2ListView.GetItemText giving NWeb2RECORD.NWeb2NAME using result, 1
	Web2ListView.GetItemText giving NWeb2RECORD.NWeb2LOCATION using result, 2
.
	call Web2LoadCurrentLV
	call Web2DisableLower2
	call 	Web2PutRecFound
	return
.
Web2PutRecFound	// output # records found
	Web2ListView.GetItemCount giving n11
	move	n11, str11
	call	Trim using str11
	pack	str45,str11," records found"
	SetItem Web2BlankRecords, 0, str45
	return
.
Web2EnableUpper
	setprop Web2Ok, enabled=1
	call "NWEB0001;WebEnableExit"  // ExitFlag2=N in parent, enable exit
	setprop Web2EditSearch, enabled=1, bgcolor=white
	setprop Web2ListView, enabled=1
	return
.
Web2DisableUpper
	setprop Web2OK, enabled=0
	call "NWEB0001;WebDisableExit" // ExitFlag2=Y in parent, disable exit
	setprop Web2EditSearch, enabled=0, bgcolor=grey
	setprop Web2ListView, enabled=0
	return
.
Web2DisableLowerMain
.Called by:  Web2DisableLower, Web2DisableLower2
.Never called by itself
	setprop Web2Save, enabled=0
	setprop Web2Quit, enabled=0
	setprop Web2EditNumber, enabled=0, bgcolor=grey
	setprop Web2EditName, enabled=0, bgcolor=grey
	setprop Web2EditLocation, enabled=0, bgcolor=grey
	move "N", NewFlag2
	return
.
Web2DisableLower
	setprop Web2New, enabled=0
	setprop Web2Modify, enabled=0
	setprop Web2Delete, enabled=0
	call	Web2DisableLowerMain
	return
.
Web2DisableLower2	// new, modify, delete remain enabled
	setprop Web2New, enabled=1
	setprop Web2Modify, enabled=1
	setprop Web2Delete, enabled=1
	call	Web2DisableLowerMain
	return
.
Web2EnableLower	// leave new, modify as disabled
	setprop Web2New, enabled=0
	setprop Web2Modify, enabled=0
	setprop Web2Delete, enabled=1
	setprop Web2Save, enabled=1
	setprop Web2Quit, enabled=1
	setprop Web2EditName, enabled=1, bgcolor=white
	setprop Web2EditLocation, enabled=1, bgcolor=white
	return
.
Web2LoadCurrentLV
	setitem	Web2EditNumber,0,NWeb2RECORD.NWeb2CODE
	setitem	Web2EditName,0,NWeb2RECORD.NWeb2NAME
	setitem	Web2EditLocation,0,NWeb2RECORD.NWeb2LOCATION
	return
.
Web2NewButton
	if (PassFlag1 = NO)
		move	"W",progcode
		setitem PasswordStatMssg1,0,"Tracking Records"
		setprop PasswordStatMssg1,visible=1,alignment=1
		setitem PasswordStatMssg,0,"Enter Password to Delete or Modify,"
		setprop PasswordStatMssg,alignment=1
		setitem PasswordEdit,0,""
		setfocus PasswordEdit
		clear	NPASFLD
		setprop	Passwrd,visible=1
		reset NPASFLD
		if (PassFlag =	NO)
			return
		endif
		move	YES,PassFlag1
	endif
.
	move "Y", NewFlag2
	call Web2DisableUpper
	call Web2EnableLower
	setprop Web2Delete, enabled=0
.
	call Web2GetNextId
		if (NWeb2FLD="void")
			call	Web2QuitButton
			return	// no space for data - exit routine
		endif
	setitem Web2EditNumber, 0, str4
	return
.
Web2QuitButton
.	undo changes
	call Web2EnableUpper
	call Web2DisableLower
	setprop Web2Modify, enabled=1	// allow modify
	call CLICK_Web2OK	// same as previous Web2OK click
	return
.
Web2DeleteButton
	if (PassFlag1 = NO)
		move	"W",progcode
		setitem PasswordStatMssg1,0,"Tracking Records"
		setprop PasswordStatMssg1,visible=1,alignment=1
		setitem PasswordStatMssg,0,"Enter Password to Delete or Modify,"
		setprop PasswordStatMssg,alignment=1
		setitem PasswordEdit,0,""
		setfocus PasswordEdit
		clear	NPASFLD
		setprop	Passwrd,visible=1
		reset NPASFLD
		if (PassFlag =	NO)
			return
		endif
		move	YES,PassFlag1
	endif
.
	alert plain, "Are you sure you want to delete this record?", result
	if (result = 1)	// yes
		move NWeb2RECORD.NWeb2CODE, NWeb2FLD
		move	"Web2DButton-NWeb2TST",Location
		pack	KeyLocation,"Key: NWeb2FLD"
		call NWeb2TST	// give valid read
		if over
		endif

		move	"Web2DButton-NWeb2DEL",Location
		pack	KeyLocation,"Key: NWeb2FLD"
		call NWeb2DEL	// needs to be preceeded by valid read
		call Web2EnableUpper
		call Web2DisableLower2
		call CLICK_Web2OK	// same as previous Web2OK click
		return
	endif
	if (result = 2)	// no
		call Web2QuitButton
		return
	endif

	if (result = 3)	// cancel
		call Web2QuitButton
		return
	endif
.
Web2ClearRecord
	Web2EditNumber.SelectAll
	Web2EditNumber.Clear
	Web2EditName.SelectAll
	Web2EditName.Clear
	Web2EditLocation.SelectAll
	Web2EditLocation.Clear
	return
.

Web2SaveButton
	call Web2VerifyData
	if (ReturnFlag2 = "Y")
		return
	endif
	if (NewFlag2 = "Y")	// it's a new record
		call Web2DisableLower	// has to be called here - changes value of new flag
		move	NWeb2RECORD.NWeb2CODE,NWeb2FLD
		call NWeb2WRT	// write the record
	elseif (NewFlag2 = "N") // it's a modify
		call Web2DisableLower	// has to be called here - changes value of new flag
		move	"Web2Save-NWeb2TST",Location
		pack	KeyLocation,"Key: NWeb2FLD"
		call NWeb2TST	// gives read
		if over
		//Should never happen
			alert	note,"Record no longer Exists!",result
			call Web2QuitButton
			return
		endif
.
		move	"Web2Save-NWeb2UPD",Location
		pack	KeyLocation,"Key: NWeb2FLD"
		call NWeb2UPD // needs previous key read
	endif
	call Web2EnableUpper
	call	Click_Web2OK
.
	Web2ListView.FindItem giving result using 0, NWeb2FLD
	Web2ListView.SetItemState using result,2,2
	Web2ListView.EnsureVisible using result, 0
	call	Click_Web2ListView
	return
.
Web2VerifyData
	getitem Web2EditNumber,0,NWeb2RECORD.NWeb2CODE
	call    TRIM using NWeb2RECORD.NWeb2CODE
	count   HowMany,NWeb2RECORD.NWeb2CODE
	if (howmany = 0)
		alert caution,"Number Required!",result
		setfocus Web2EditNumber
		move "Y", ReturnFlag2
		return
	endif
	call	ZFillIt using NWeb2RECORD.NWeb2CODE
.
	getitem Web2EditName,0,NWeb2RECORD.NWeb2NAME
	call    TRIM using NWeb2RECORD.NWeb2NAME
	count   HowMany,NWeb2RECORD.NWeb2NAME
	if (howmany = 0)
		alert caution,"Name Required!",result
		setfocus Web2EditName
		move "Y", ReturnFlag2
		return
	endif
.
	getitem Web2EditLocation,0,NWeb2RECORD.NWeb2LOCATION
        call    TRIM using NWeb2RECORD.NWeb2LOCATION
        count   HowMany,NWeb2RECORD.NWeb2LOCATION
        if (howmany = 0)
            	alert caution,"Location Required!",result
                setfocus Web2EditLocation
                move "Y", ReturnFlag2
                return
	endif
        move "N", ReturnFlag2	// record is complete!
        return
.
Web2ModifyButton
	if (PassFlag1 = NO)
		move	"W",progcode
		setitem PasswordStatMssg1,0,"Tracking Records"
		setprop PasswordStatMssg1,visible=1,alignment=1
		setitem PasswordStatMssg,0,"Enter Password to Delete or Modify,"
		setprop PasswordStatMssg,alignment=1
		setitem PasswordEdit,0,""
		setfocus PasswordEdit
		clear	NPASFLD
		setprop	Passwrd,visible=1
		reset NPASFLD
		if (PassFlag =	NO)
			return
		endif
		move	YES,PassFlag1
	endif
.
	move	NWeb2RECORD.NWeb2CODE,NWeb2FLD
	move	"Web2Modify-NWeb2TST",Location
	pack	KeyLocation,"Key: NWeb2FLD"
	call	NWeb2TST
	call Web2DisableUpper
	call Web2EnableLower
	return
.
Web2GetNextId
	move	C1,N4
	loop
		move	N4, NWeb2FLD
		rep	zfill,NWeb2FLD
		call	NWeb2TST
		until over
		add	C1,N4
		until (N4 = "9999")
	repeat

	if (N4 = "9999")
		move	"void",NWeb2FLD
		pack	taskname,"Maximum number of records has been exceeded.",newline,"Please contact Information Services!"
		alert	note,taskname,result
		return
	endif
	move	n4, str4
	rep	zfill, str4
	return

	include	nWebio2.inc
	include	npasio.inc
	include	comlogic.inc
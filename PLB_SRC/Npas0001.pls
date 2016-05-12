************************************************************
*   PASSMOD IS MAINTENANCE PROGRAM FOR NIN PASSWORD FILE   *
************************************************************
.
PC       EQU       1
.
         INCLUDE   COMMON.inc
         INCLUDE   NPASDD.inc
         INCLUDE   CONS.inc


release  init	    "1.6"	     24FEB2006 DMS ADDED GUI
.str20		dim	20
hold		dim 	86
CODE		dim	1
PW		dim	5
SLASHDATE	dim	10
DimPtr		dim	^
AddFlag		init	"N"
ReturnFlag	init	"N"
ModifyFlag	init 	"N"
ISFlag		init	"N" 	// is user Information Services employee?
RESULT2		FORM	2

font2	font
white	color
grey 	color
	create font2, "Arial", size=8, bold=0
	create white=*white
	create grey=220:220:220
.
PCode	plform	npas001b
MainScreen	plform	npas0001
initialScreen	plform	npas001a
mss1	plform	Error
.
	formload initialScreen
	formload MainScreen
	formload PCode
	formload mss1
	winhide
.
	move C3, NPASLOCK // don't bother with any locking
.
	setfocus InitPassEditPW
.Load ListView Cols
	ProgCodeListView.InsertColumn using "Code", 50,0
	ProgCodeListView.InsertColumn using "Program Description", 150,1
	ProgCodeListView.SortColumn using 0,1 // alpha asc sort on prog code
	ProgCodeListView.SetItemState giving result using 0,2,2 // select 1st object

.
	NPasListView.InsertColumn using "Code",30,0
	NPasListView.InsertColumn using "Password",70,1
	NPasListView.InsertColumn using "UserID",100,2
	NPasListView.InsertColumn using "Full Name",100,3
	NPasListView.InsertColumn using "Program Description",100,4
	NPasListView.InsertColumn using "Date",75,5 // with slashes
	NPasListView.InsertColumn using "Full record",0,6
	NPasListView.InsertColumn using "Raw Date", 0,7  // no formatting
.
	setfocus NPasEditSearch
	call NPasNoResults
.loop starts here
	loop
		eventwait
	repeat
.
FileClose
	if (AddFlag =  "N" & ModifyFlag = "N")  // Y is ok to exit
		shutdown
	endif
	return
.
NPasInsertLVItem
	unpack NPASKEY, CODE,PW
	unpack NPASDATE, cc, yy, mm, dd
	pack SLASHDATE, mm, "/", dd, "/", cc, yy
	pack hold, NPASKEY, CODE, PW, NPASUSER, NPASNAM1, NPASDESC, SLASHDATE, NPASDATE // b4 I trim
	NPasListView.InsertItem giving result using CODE // creates new row and assigns value to 1st col
	call Trim using PW
	NPasListView.SetItemText using result, PW, 1 // col2
	call Trim using NPASUSER
	NPasListView.SetItemText using result, NPASUSER, 2 // col3
	call Trim using NPASNAM1
	NPasListView.SetItemText using result, NPASNAM1, 3 // col 4
	call Trim using NPASDESC
	NPasListView.SetItemText using result, NPASDESC, 4 // col5
	NPasListView.SetItemText using result, SLASHDATE, 5 // col 6
	NPasListView.SetItemText using result, hold, 6 // col7
	NPasListView.SetItemText using result, NPASDATE, 7 // col8
	return
.
NPasSeqRead
	CLOSE	NPASFLIST	// close so I can do multiple seq reads
	MOVE	c0,NPASFLAG
	loop
		move	"NPASOK-NPASSEQ",Location
		pack	KeyLocation,"Key: SEQ"
		call NPASSEQ
		until over
		call NPASInsertLVItem
	repeat
	call NPasCommonRead
	return
.
NPasISIRead Routine DimPtr
. calling routine has already trimmed passed variable
	move DimPtr, NPASFLD1
	move "NPasISIRead-NPASKEY1", Location
	pack KeyLocation, "Key:   ",NPASFLD1
	call NPASKEY1
	if not over
		loop
			unpack NPASKEY, str1
			until over
			if (str1 <> NPasFld1)
				break
			endif
		call NPASInsertLVItem
		call NPASKS1
		repeat
	endif
	call NPasCommonRead
	return
.
NPasAAMRead Routine DimPtr
. calling routine has already trimmed passed variable
	if (ISFlag = "Y")
		pack NPASFLD2, "01F", DimPtr
	else
		pack NPASFLD2, "01X", DimPtr  // search on pw, want it exact
	endif
	move "NPasOK-NPASAIM", Location
	pack KeyLocation,"Key:   ",NPASFLD2
	call NPASAIM
	if not over
		call NPASInsertLVItem
		loop
			call NPASKG
			until over
			call NPASInsertLVItem
		repeat
	endif
	call NPASCommonRead
	return
.
NPasCommonRead
	NPASListView.GetItemCount giving result
	move result, str9
	call TRIM using str9
	call FormatNumeric using str9, str11
	pack str35, str11, " records found"
	setItem NPasTextRecFnd, 0, str35
	if (result <>0)
		NPasListView.SetItemState giving result using 0,2,2 // select first object
		call Click_NPasListView // call to NPasRetrieveLVRecord
		call NPasFoundResults  // does enable/disables for found results
	else
		call NPasNoResults  // enables/disables for no results
	endif
	return
NPasRetrieveLVRecord	// load highlighted listview entry
	NPASListView.GetItemCount giving result
	if (result <> 0)
		NPasListView.GetNextItem giving result using C2
		NPasListView.GetItemText giving hold using result, 6
		unpack hold,NPASKEY, CODE,PW,NPASUSER,NPASNAM1,NPASDESC,SLASHDATE, NPASDATE
		call NPasLoadCurrentLV
		call NPasFoundResults
	else
		call NPasNoResults
	endif
	return
NPasLoadCurrentLV
	setitem NPasEditCode, 0, CODE
	setitem NPasEditPassword, 0, PW
	setitem NPasEditUserId, 0, NPASUSER
	setitem NPasEditUserName, 0, NPASNAM1
	setitem NPasEditProgramDesc, 0, NPASDESC
	setitem NPasEditDateMod, 0, SLASHDATE
	return
.
NPasClearRecord // clear GUI fields
	setitem NPasEditCode, 0, ""
	setitem NPasEditPassword, 0, ""
	setitem NPasEditUserId, 0, ""
	setitem NPasEditUserName, 0, ""
	setitem NPasEditProgramDesc, 0, ""
	setitem NPasEditDateMod, 0, ""
	setitem NPasTextRecFnd, 0, ""
	return
.
NPasFoundResults
	setprop NPasEditSearch, enabled=1, bgcolor=white
	setprop NPasQuit, enabled=0
	setprop NPasAdd, enabled=1
	setprop NPasModify, enabled=1
	setprop NPasDelete, enabled=0
	setprop NPasExit, enabled=1
	setprop NPasOK, enabled=1
	setprop NPasSave, enabled=0
	setprop NPasListView, enabled=1
	call NPasDisableRecordFields
	setprop NPasRadioProgCode, enabled=1
	setprop NPasRadioPassword, enabled=1
	return
.
NPasNoResults
	setprop NPasRadioProgCode, enabled=1
	setprop NPasRadioPassword, enabled=1
	setprop NPasEditSearch, enabled=1, bgcolor=white
	setprop NPasQuit, enabled=0
	setprop NPasAdd, enabled=1
	setprop NPasModify, enabled=0
	setprop NPasExit, enabled=1
	setprop NPasOK, enabled=1
	setprop NPasDelete, enabled=0
	setprop NPasSave, enabled=0
	call NPasDisableRecordFields
	return
.
SetUserLimits
	setprop NPasEditSearch, visible=0
	setprop NPasOK, visible=0
	setprop NPasRadioProgCode, visible=0
	setprop NPasStatSearch, visible=0
	setprop NPasRadioPassword, visible=0
	setprop NPasAdd, visible=0
	setprop NPasDelete, visible=0
	call NPASAAMREAD using taskname
	return
NPasEnableRecordFields	// date always disabled
	if (ISFLAG = "Y")  // only IS can change the following fields
		setprop NPasEditCode, enabled=1, bgcolor=white
		setprop NPasEditUserId,  enabled=1, bgcolor=white
		setprop NPasEditUserName, enabled=1, bgcolor=white
		setprop NPasEditProgramDesc,  enabled=1, bgcolor=white
	endif
	setprop NPasEditPassword,  enabled=1, bgcolor=white  // everyone can change their password
	return
.
NPasDisableRecordFields  // date always disabled
	setprop NPasEditCode, enabled=0, bgcolor=grey
	setprop NPasEditPassword,  enabled=0, bgcolor=grey
	setprop NPasEditUserId,  enabled=0, bgcolor=grey
	setprop NPasEditUserName, enabled=0, bgcolor=grey
	setprop NPasEditProgramDesc,  enabled=0, bgcolor=grey
	setprop NPasEditDateMod, enabled=0, bgcolor=grey
	return
.
NPasModifyProps  // for both modify and add
	setprop NPasRadioProgCode, enabled=0
	setprop NPasRadioPassword, enabled=0
	setprop NPasEditSearch, enabled=0, bgcolor=grey
	setprop NPasQuit, enabled=1
	setprop NPasAdd, enabled=0
	setprop NPasModify, enabled=0
	setprop NPasExit, enabled=0
	setprop NPasOK, enabled=0
	if (AddFlag = "Y")
		setprop NPasDelete, enabled=0
	else
		setprop NPasDelete, enabled=1
	endif
	setprop NPasSave, enabled=1
	setprop NPasListView, enabled=0
	call NPasEnableRecordFields
	setfocus NPasEditCode
	return
.
NPasQuit
. undos whatever changes user might have been making (no save)
	call NPasRetrieveLVRecord
	move "N", ModifyFlag
	move "N", AddFlag
	return
.
NPasModify
	call NPasModifyProps   // good for add and modify
	move "Y", ModifyFlag
	return
.
NPasAdd
	move "Y", Addflag
	call NPasClearRecord
	call NPasModifyProps  // good for add and modify
	clock date, str10
	move "-/", str2
	replace str2, str10
	call removechar using str10, slash
	call TRIM using str10
	unpack str10, mm,dd,yy
	pack str10, mm,"/",dd,"/","20",yy  // have to change 20 when century changes
	setitem NPasEditDateMod, 0, str10
	return
.
NPasDelete
	alert plain, "Are you sure you wish to delete the selected record(s)", result
	if (result=1) // yes
		move	SEQ, result
		move result, N9
		loop
			move result, N9
			NPasListView.GetNextItem giving result using C2, N9 // -1 is error code
			until (result = SEQ)
			NPasListView.GetItemText giving hold using result, 6 // populate hold w/ full record
			unpack hold,NPASKEY, CODE,PW,NPASUSER,NPASNAM1,NPASDESC,SLASHDATE, NPASDATE
			pack NPasFld, CODE, PW
			call NPasTST  // give valid read
			if over
				alert note, "no valid read", result
				return
			endif
			call NPASDEL
		repeat
		move "N", ModifyFlag  // could put under NPASFOUNDRESULTS and NPASNORESULTS
	else	.if (result=2)  // no
		call NPASQUIT
	endif
	call CLICK_NPASOK  // same as previous NPASOK click
	return
.
NPasVerifyData   // make sure all fields are filled in - move code and pw to NPASKEY
	getitem NPasEditCode, 0, str1
	count HowMany, str1
	if (HowMany = 0)
		alert caution, "Program Code Required!", result
		setfocus NPasEditCode
		move "Y", returnFlag
		return
	endif
	getitem NPasEditPassword, 0, str5
	count HowMany, str5
	if (HowMany = 0)
		alert caution, "Password Required!", result
		setfocus NPasEditPassword
		move "Y", returnFlag
		return
	elseif (HowMany < 3)
		alert caution, "Password must be at least three characters long.", result
		setfocus NPasEditPassword
		move "Y", returnFlag
		return
	endif
	pack str6, str1, str5
	move str6, NPASKEY
	getitem NPasEditUserId, 0, NPASUSER
	count HowMany, NPASUSER
	if (HowMany = 0)
		alert caution, "User ID Required!", result
		setfocus NPasEditUserId
		move "Y", returnFlag
		return
	endif
	getitem NPasEditUserName, 0, NPASNAM1
	count HowMany, NPASNAM1
	if (HowMany = 0)
		alert caution, "User Name Required!", result
		setfocus NPasEditUserName
		move "Y", returnFlag
		return
	endif
	getitem NPasEditProgramDesc, 0, NPASDESC
	count HowMany, NPASDESC
	if (HowMany = 0)
		alert caution, "Program Description Required!", result
		setfocus NPasEditProgramDesc
		move "Y", returnFlag
		return
	endif
	CLEAR STR10
 	getitem NPasEditDateMod, 0, str10
	call removechar using str10, SLASH
	call TRIM using str10
	unpack str10, mm,dd,cc,yy
	pack NPASDATE, cc,yy,mm,dd
	call trim using npasdate
	call ZFILLIT using npasdate
	move "N", returnFlag
	return
.
NPasSave
	if (ModifyFlag = "Y")  // update record
		move	PW, str20  // save copy of original PW - might be changed
		call TRIM using str20
		call NPasVerifyData  // verify pw >=3 chars  // newly entered pw(if one) now in str5, NPASKEY updated
		if (ReturnFlag = "Y")
			return
		endif
.
		if (ISFlag = "N")  // changing password for non-I.S. user
			if (str5 = PW)  // pw is what was unpacked into record box (from hold)
				alert note, "Password is not changed.", result
				setfocus NPASEditPassword
				return
			endif
			// is desired password already in use?
			pack NPASFLD2, "01X", STR5
			move "NPasOK-NPASAIM", Location
			pack KeyLocation,"Key:   ",NPASFLD2
			call NPASAIM  // does someone else have that PW?
			if not over  // YES, SOMEONE DOES
				alert note, "That PW already exists.  Choose another.", result
				return
			endif
			call ChangeAllPWs
			NPASListView.deleteallitems giving result
			call NPASAAMREAD using STR5  // display all instances of new PW
		endif
DAVE		if (ISFlag = "Y")  // I.S. employee
			if (STR5 = str20)   // NO CHANGE TO PW FIELD
					call NPASTESTROUTINE
					if over // means not used
						// NEED VALID READ
						call NPASTestRoutineOldValues
						if over // should never happen
							alert note, "Record no longer exists!", result
							call NPasQuit
							return
						endif
						call NPASUPD // no change to pw field, will update single record's other fields
					else   // CODE+PW COMBO IS USED
						if (CODE = STR1) // here we wish to update existing
						// STR1 IS WHAT NEW CODE IS
							  CALL NPASTESTROUTINEOLDVALUES
							call NPASUPD  // update b/c no changes to pw and code
						else 	// can't update to an existing key!
							alert note, "Program code + Password combination already used, try again.",result
							call NPasQuit
							setfocus NPasEditCode
							RETURN
						endif
					endif
			else  // make changes to selected record and prompt to change every pw
				alert plain, "Do you want to change every instance of this password?", result
				if (result = 1)   // yes - change every record
					call NPasTestRoutine
					if over // highlighted entry is not a duplicate
						// now need valid read
						call NPASTestRoutineOldValues
						if over // should never happen
							alert note, "Record no longer exists!", result
							call NPasQuit
							return
						endif
						call NPASUPD  // changes the loaded  record
						// now change the rest load searchtext, load list view
						NPASListView.deleteallitems giving result
						call	NPasAAMRead using str20 // get entries with old pw
						call ChangeAllPWs
					else  // higlighted entry is a duplicate
						NPASListView.deleteallitems giving result
						call	NPasAAMRead using str20 // get entries with old pw
						call ChangeAllPWs
					endif
				endif
				if (result = 2)   // no - just change one -prob. can be dup
					call NPASTestRoutine
					if over // means not used
						// now need valid read
						call NPASTestRoutineOldValues
						if over // should never happen
							alert note, "Record no longer exists!", result
							call NPasQuit
						return
						endif
						call NPASUPD  // changes the loaded  record
					else
						alert note, "Program code + PW combo is taken, try again.", result					call NPasQuit
						setfocus NPasEditCode
						return
					endif
				endif
				if (result = 3)   // cancel
					call NPASQuit
					return
				endif
			endif
		endif

	else	// we have a new record!
		call NPasVerifyData
		if (ReturnFlag = "Y")
			return
		endif
		call NPASTestRoutine
		if over // means not used
			move	"NPASSave-NPASWRT",Location
			pack	KeyLocation,"Key: NPASFLD"
			move NPASKEY, NPASFLD
			call NPASWRT
		else
			alert note, "That Program Code and PW combination is used", result
			return
		endif
	endif
. change search edit text box for listview refresh
. to include any changes we just made on search key - IS personnel only
	if (ISFLAG = "Y")
		getitem	NPasRadioProgCode, 0, result
			if (result=1)
				getitem NPasEditCode, 0, taskname
				call TRIM using taskname
				setitem NPasEditSearch, 0, taskname
			endif
		getitem NPasRadioPassword, 0, result
			if (result=1)
				getitem NPASEditPassword, 0, taskname
				call TRIM using taskname
				setitem NPasEditSearch, 0, taskname
			endif

		call CLICK_NPASOK

	endif
	// BACK TO CODE FOR BOTH I.S., NON-I.S.
	move "N", AddFlag
	move "N", ModifyFlag
	return
.
ChangeAllPWs
	move SEQ, result
	move result, N9
	loop
		move	result, N9
		NPasListView.GetNextItem giving result using C0, N9 // -1 is error code
		until (result = SEQ)
		NPasListView.GetItemText giving hold using result, 6 // populate hold with full record
		unpack hold,NPASKEY, CODE,PW,NPASUSER,NPASNAM1,NPASDESC,SLASHDATE, NPASDATE
		call NPASTestRoutine
		if over // should never happen
			alert note, "Record no longer exists!", result
			call NPasQuit
			return
		endif
		IF (ISFLAG="N")
			getitem NPASEditPassword, 0,PW
			call TRIM using PW
			PACK NPASKEY, CODE, PW
		else // I.S.
			PACK NPASKEY, CODE, STR5
		endif
		// if code+pw combo exists, don't do update
		call NPASTestRoutine
		if over
			//NEED valid read
			call NPASTestRoutineOldValues
			move	"NPASSave-NPASUPD",Location
			pack	KeyLocation,"Key: NPASFLD"
			call NPASUPD
		else
			alert note, "Code + Password combo already exists, at least one record not changed.", RESULT2
			// I created and used Result2 b/c result being used in loop
		endif
	repeat
	return
NPASTestRoutineOldValues
	move	"NPASSave-NPASTST",Location
	pack	KeyLocation,"Key: NPASFLD"
	pack NPASFLD, CODE, str20   // old values
	call NPASTST
	return
.
NPASTestRoutine
	move	"NPASSave-NPASTST",Location
	pack	KeyLocation,"Key: NPASFLD"
	move NPASKEY, NPASFLD
	call NPASTST
	return
.
	INCLUDE   NPASIO.inc
        INCLUDE    COMLOGIC.inc


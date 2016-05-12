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
DimPtr	dim	^
hold	dim	160	// holding space for NINWEB2
.
x	plform	WebProcFileMaint
mss1	plform	Error
	winhide
	formload x
	formload mss1
.
.Load ListView Columns
.
	WebListView.InsertColumn using "ID",50,0
	WebListView.InsertColumn using "Name",250, 1
	WebListView.InsertColumn using "Location",225,2
	WebListView.InsertColumn using "Complete Record",0,3
.
	setfocus WebEditSearch
.
	loop
		eventwait
	repeat
.
.
fileExit
	shutdown
	return
.

WebSeqRead
	CLOSE	NWEB2FLIST
	MOVE	c0,nweb2flag
	loop
		call NWEB2SEQ
		until over
		call WebLoadListView
	repeat
	return
.

WebXSeqRead
	call NWEB2EOF
	loop
		call NWEB2BAK
		until over
		call WebLoadListView
	repeat
	return





WebISIRead Routine DimPtr
.Calling routine has already trimmed passed variable
	move	DimPtr,NWEB2FLD
	call	ZFillIt using NWEB2FLD
.
	call	NWEB2KEY
	if not over
.
		call WebLoadListView	// load ListViewObject

		// want to see if listview is empty or not - if not empty, call Webload REcord
	endif
.
	WebListView.GetItemCount giving result
	compare result, c0
	if zero
		//alert note, "Empty Listview!", result
		SetItem WebBlankRecords, 0, "0 records found"
		return
	endif
.
	WebListView.SetItemState giving result using 0,2,2
	WebListView.EnsureVisible using 0,0
	call WebLoadRecord
	call 	WebPutRecFound
	return
.
return
.
WebAAMRead Routine DimPtr
.Calling routine has already trimmed passed variable
	pack	NWEB2FLD1,"01F",DimPtr
	clear	NWEB2FLD2
	call 	NWEB2AIM
	loop
		until over
		call	WebLoadListView
		call	NWEB2KG
	repeat
	WebListView.GetItemCount giving result
	compare result, c0
	if zero
		//alert note, "Empty Listview!", result
		SetItem WebBlankRecords, 0, "0 records found"
		return
	endif
.
	WebListView.SetItemState giving result using 0,2,2
	WebListView.EnsureVisible using 0,0
	call WebLoadRecord
	call 	WebPutRecFound

	return
.
WebLoadListView
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
	//alert plain, "Entering WebLoadRecord!", result
	WebListView.GetNextItem giving result using C2
	WebListView.GetItemText giving NWEB2RECORD.NWEB2CODE using result
	WebListView.GetItemText giving NWEB2RECORD.NWEB2NAME using result, 1
	WebListView.GetItemText giving NWEB2RECORD.NWEB2LOCATION using result, 2
.
	setitem	WebEditNumber,0,NWEB2RECORD.NWEB2CODE
	setitem	WebEditName,0,NWEB2RECORD.NWEB2NAME
	setitem	WebEditLocation,0,NWEB2RECORD.NWEB2LOCATION
	return
.
WebPutRecFound
	WebListView.GetItemCount giving n11
	move	n11, str11
	call	Trim using str11
	pack	str45,str11," records found"
	SetItem WebBlankRecords, 0, str45
	return
.
	include	nwebio2.inc
	include	comlogic.inc


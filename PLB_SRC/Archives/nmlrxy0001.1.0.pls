.MAILER/MAILER PROGRAM
........................................
. Program:	Nmlrxy0001.PLS
. Function:	Mailer to Mailer Program
. Author:	Andrew Harkins
. Orig.	Date:	May 24,2004
. Release:	1.0
........................................
PC	EQU	1
	include	common.inc
	include	cons.inc
	include	nmlrxydd.inc
	include	compdd.inc
	include	cntdd.inc
.	include	nmlrdd.inc
	include	nusedd.inc
.Must be included for Search.plf
.	include	nbrkdd.inc
	include	nowndd.inc
	include	ndatdd.inc
	include	ncmpdd.inc
	include	nrtndd.inc
	include	norddd.inc
	include	ncntdd.inc

release	init	"1.0"	25MAY2004	ASH INITIAL RELEASE
	move	"May 25, 2004",Wreldate
.
	move	"Nmlrxy0001.PLS",Wprognme
	move	"Mailer to Mailer Notes Program",Wfunction
	move	"Andrew	Harkins",Wauthor
	move	release,Wrelease
.
.EXTERNAL ROUTINES FROM	INFO.PLC
MailerLoadForm external "INFO;LoadForm"
MailerDisplayMailer external "INFO;DisplayMailer"
.EXTERNAL ROUTINES FROM NMLRXY0002.PLC
CreateMailerMailer external "NMLRXY0002;CreateMailerMailer"

str6a	dim	6
.FLAGS
ExitFlag init	"Y"
ReturnFlag init	"Y"
NewFlag	init	"N"
MlrEditFlag form 1
Timer	Timer
hold	dim	1068
holdkey	dim	12
NoteHold dim	750
userlogn dim	7
MouseForm form	10
T1	form	4
L1	form	4
FrmPtr	form	^
EditPtr EditText ^
.Vars used for Report Screen
RptCan  dim     1
ObjectColl	Collection
StatTextBoxes	StatText (2)
EditTextBoxes	EditText (1)
Buttons		Button (3)
ListViews	ListView (1)

.Colors
white	color
grey	color
.Set Up	Menu Bar
mFile	menu
mEdit	menu
mOptions menu
mReports menu
mHelp	menu
.Menu Bar for Report Screen
mRSearch menu
.Set Up	SubMenu	for Options
sSearch	submenu
 
.Present Data for Menu Bar
FData	init	"&File;E&xit"
EData	init	"&Edit;<1&Undo;-;<2&Cut	Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
OData	init	"&Options;&Search-F2"
RData	init	"&Reports"
HData	init	"&Help;&About"
.Present Data for SubMenu
SData	init	";&Mailer"

brk	plform	Search
rpt2	plform	Report2
mss1	plform	Error
abt	plform	About
x	plform	Nmlrxy0001
	winhide
	formload x
	formload brk
	formload rpt2
	formload mss1
	formload abt
.
	call	MailerLoadForm
.Timer creation
	CREATE	TIMER,18000	.30 minutes
	ACTIVATE TIMER,Timeout,RESULT
.Create	Menus
	create	Nmlrxy0001;mFile,FData
	create	Nmlrxy0001;mEdit,EData,mFile
	create	Nmlrxy0001;mOptions,OData,mEdit
	create	Nmlrxy0001;mReports,RData,mOptions
	create	Nmlrxy0001;mHelp,HData,mReports
.Create	SubMenu
	create	Nmlrxy0001;sSearch,SData,mOptions,1
 
.Activate Menus
.FileGo	leads to stop
	activate mFile,FileGo,result
.Need this when	it works
	activate mEdit,EditGo,result
.Only a	SubMenu	under this one
	activate mOptions
	activate mReports,ReportGo,result
	activate mHelp,HelpGo,result	
.Activate SubMenus
	activate sSearch,SearchGo,result
.Create	Colors for EditText Inquiry
	create	white=*white
	create	grey=220:220:220
.
	MailerXYListView.InsertColumn using "Key1",0,0
	MailerXYListView.InsertColumn using "Mailer1",50,1
	MailerXYListView.InsertColumn using "Mailer1 Name",150,2
	MailerXYListView.InsertColumn using "Mailer2",50,3
	MailerXYListView.InsertColumn using "Mailer2 Name",150,4
	MailerXYListView.InsertColumn using "Notes",200,5
	MailerXYListView.InsertColumn using "Details",0,6
.
	call	GetWinVer
.
	move	C0,NUSEFLD
	move	C1,NUSEPATH
	move	PORTN,NUSEFLD
	rep	zfill,NUSEFLD
	call	NUSEKEY
	call	Trim using NUSEUSER
	move	NUSEUSER,str1
	loop
		bump	NUSEUSER,1
		cmatch	B1,NUSEUSER
		until equal
		until eos
	repeat
	if not eos
		bump	NUSEUSER,1
		move	NUSEUSER,str6
		clear	userlogn
		pack	userlogn,str1,str6
	endif
	reset	NUSEUSER

.
	clock	timestamp,timestamp
	call	MailerDisableLower
	setfocus MailerXYEditMailer1
	loop
		waitevent
		deactivate TIMER
		ACTIVATE TIMER,Timeout,RESULT
	repeat
 
Timeout
.Test to make sure nothing is left open	in Modify Mode
	getprop	MailerXYEditMailer1a,enabled=N9
	if (N9 = 1)
		call	Click_MailerXYQuit
	endif
	beep
	beep
	beep
	shutdown

FileGo
.Flag set to "N" if in Modify or New mode
	branch result to FileGo2
FileGo2	       
	if (ExitFlag = "Y")
		winshow
		stop
	endif
	return
EditGo
	return
HelpGo
	branch	result to HelpGo1
HelpGo1
	setprop	AboutMssg,visible=1
	return	     
 
SearchGo
	branch	result to SearchGo3
SearchGo1
.BROKER
	return
SearchGo2	 
.LIST
	return
SearchGo3 Routine
.MAILER
	move	C3,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
SearchGo4
.SHIP-TO
	return
SearchGo5
.CAMPAIGN
	return
	
SearchGo6
.OWNER
	return

SearchLoad
.Called	by SearchDataList_DoubleClick
.Only load if in modify	mode
	getprop	MailerXYEditMailer1a,enabled=howmany
	branch SrchFlag	to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER
	return
SearchLoad2
.LIST
	return
SearchLoad3
.MAILER
	unpack	Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
	if (howmany = C1)
		if (MlrEditFlag = 3)
			setitem	MailerXYEditMailer1a,0,str6
			setitem	MailerXYStatMlrName1a,0,str45
			setfocus MailerXYEditMailer1a
		else
			setitem	MailerXYEditMailer2a,0,str6
			setitem	MailerXYStatMlrName2a,0,str45
			setfocus MailerXYEditMailer2a
		endif
	else
		if (MlrEditFlag = 1)
			setitem	MailerXYEditMailer1,0,str6
			setitem	MailerXYStatMlrName1,0,str45
			setfocus MailerXYEditMailer1
		else
			setitem	MailerXYEditMailer2,0,str6
			setitem	MailerXYStatMlrName2,0,str45
			setfocus MailerXYEditMailer2
		endif
	endif
	return
SearchLoad4
.SHIP-TO
	return
SearchLoad5
.CAMPAIGN
	return
SearchLoad6
.OWNER
	return
ReportGo
	return

MailerClearScreen
	setitem	MailerXYEditMailer1a,0,""
	setitem	MailerXYEditMailer2a,0,""
	setitem	MailerXYEditNotesa,0,""
	setitem	MailerXYEditNotesb,0,""
	setitem	MailerXYEditRecDate,0,""
	setitem	MailerXYEditUpdDate,0,""
	setitem	MailerXYStatMlrName1a,0,""
	setitem	MailerXYStatMlrName2a,0,""
	setitem	MailerXYStatUpdate,0,""
	return

MailerEnableUpper
	setprop	MailerXYEditMailer1,enabled=1,bgcolor=white
	setprop	MailerXYEditMailer2,enabled=1,bgcolor=white
	setprop	MailerXYListView,enabled=1,bgcolor=white
	setprop	MailerXYOK,enabled=1
	setprop	MailerXYNew,enabled=1
	setprop	MailerXYExit,enabled=1
	return

MailerDisableUpper
	move	NO,ExitFlag
	setprop	MailerXYEditMailer1,enabled=0,bgcolor=grey
	setprop	MailerXYEditMailer2,enabled=0,bgcolor=grey
	setprop	MailerXYListView,enabled=0,bgcolor=grey
	setprop	MailerXYOK,enabled=0
	setprop	MailerXYModify,enabled=0
	setprop	MailerXYNew,enabled=0
	setprop	MailerXYDelete,enabled=0
	setprop	MailerXYExit,enabled=0
	return

MailerEnableLower
	setprop	MailerXYEditMailer1a,enabled=1,bgcolor=white
	setprop	MailerXYEditMailer2a,enabled=1,bgcolor=white
	setprop	MailerXYEditNotesa,readonly=0,bgcolor=white
	setprop	MailerXYEditNotesb,readonly=0,bgcolor=white
	setprop	MailerXYEditRecDate,enabled=1,bgcolor=white
	setprop	MailerXYEditUpdDate,enabled=1,bgcolor=white
	setprop	MailerXYQuit,enabled=1
	setprop	MailerXYSave,enabled=1
	move	YES,ExitFlag
	return

MailerDisableLower
	move	"N",NewFlag
	setprop	MailerXYEditMailer1a,enabled=0,bgcolor=grey,readonly=1
	setprop	MailerXYEditMailer2a,enabled=0,bgcolor=grey,readonly=1
	setprop	MailerXYEditNotesa,readonly=1,bgcolor=grey
	setprop	MailerXYEditNotesb,readonly=1,bgcolor=grey
	setprop	MailerXYEditRecDate,enabled=0,bgcolor=grey
	setprop	MailerXYEditUpdDate,enabled=0,bgcolor=grey
	setprop	MailerXYQuit,enabled=0
	setprop	MailerXYSave,enabled=0
	return

MailerLoadScreen
	unpack	NMLRXYMLR1,str6
	call	MailerLostFocus using C3
.
	unpack	NMLRXYMLR2,str6
	call	MailerLostFocus using C4
.
	setitem	MailerXYEditNotesa,0,NMLRXYNOTE
.
	setitem	MailerXYEditNotesb,0,NMLRXYNOTE2
.
	call	Trim using NMLRXYDATE
	if (NMLRXYDATE <> "")
		unpack	NMLRXYDATE,str4,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,str4
	else
		clear	str10
	endif
	setitem	MailerXYEditRecDate,0,str10
.

	call	Trim using NMLRXYUDATE
	if (NMLRXYUDATE <> "")
		unpack	NMLRXYUDATE,str4,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,str4
	else
		clear	str10
	endif
	setitem	MailerXYEditUpdDate,0,str10
.
	setitem	MailerXYStatUpdate,0,NMLRXYINITS
	return

MailerLostFocus Routine FrmPtr
	call	Trim using str6
	if (str6 = "")
		clear	COMPNUM
		clear	COMPCOMP
	else
		pack	COMPFLD,str6
		move	"Load-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if over
			clear	COMPNUM
			clear	COMPCOMP
		elseif (COMPMLRFLG <> "T")
			clear	COMPNUM
			clear	COMPCOMP
		endif
	endif
	if (FrmPtr = 1)
		setitem	MailerXYEditMailer1,0,COMPNUM
		setitem	MailerXYStatMlrName1,0,COMPCOMP
	elseif (FrmPtr = 2)
		setitem	MailerXYEditMailer2,0,COMPNUM
		setitem	MailerXYStatMlrName2,0,COMPCOMP
	elseif (FrmPtr = 3)
		setitem	MailerXYEditMailer1a,0,COMPNUM
		setitem	MailerXYStatMlrName1a,0,COMPCOMP
	elseif (FrmPtr = 4)
		setitem	MailerXYEditMailer2a,0,COMPNUM
		setitem	MailerXYStatMlrName2a,0,COMPCOMP
	endif
	return

MailerVerifyData
	getitem	MailerXYEditMailer1a,0,str6
	call	Trim using str6
	if (str6 = "")
		alert	caution,"Valid First Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer1a
		return
	endif
	call	ZFillIt using str6
	pack	COMPFLD,str6
	move	"Verify1-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if over
		alert	caution,"Valid First Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer1a
		return
	elseif (COMPMLRFLG <> "T")
		alert	caution,"Valid First Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer1a
		return
	endif
	pack	NMLRXYMLR1,str6
.
	getitem	MailerXYEditMailer2a,0,str6
	call	Trim using str6
	if (str6 = "")
		alert	caution,"Valid Second Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer2a
		return
	endif
	pack	COMPFLD,str6
	move	"Verify2-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if over
		alert	caution,"Valid Second Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer2a
		return
	elseif (COMPMLRFLG <> "T")
		alert	caution,"Valid Second Mailer is Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer2a
		return
	endif
	pack	NMLRXYMLR2,str6
.Test for validity
	pack	NMLRXYFLD,NMLRXYMLR2,NMLRXYMLR1			.Test using opposite format first
	move	"NMLRXYTST",Location
	pack	KeyLocation,"Key: ",NMLRXYFLD
	call	NMLRXYTST
	if over
		pack	NMLRXYFLD,NMLRXYMLR1,NMLRXYMLR2		.Test using opposite format first
		move	"NMLRXYTST-2",Location
		pack	KeyLocation,"Key: ",NMLRXYFLD
		call	NMLRXYTST
		if over
			if (NewFlag <> YES)
.Safety measure for Existing records - Modify Button should already have tested for this.
				alert	note,"This record no longer exists!",result
				goto Click_MailerXYQuit
			endif
		elseif (NewFlag = YES)
			pack	taskname,"This record already exists!",newline,"Duplicates are not allowed."
			alert	note,taskname,result
			move	YES,ReturnFlag
			setfocus MailerXYEditMailer1a
			return
		endif
	elseif (NewFlag = YES)
		pack	taskname,"This record already exists!",newline,"Duplicates are not allowed."
		alert	note,taskname,result
		move	YES,ReturnFlag
		setfocus MailerXYEditMailer1a
		return
	endif
.
	getitem	MailerXYEditNotesa,0,NMLRXYNOTE
	call	Trim using NMLRXYNOTE
	if (NMLRXYNOTE = "")
		alert	caution,"Valid Notes are Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditNotesa
		return
	endif
.Safety measure which prevents entry of all CarriageReturns
	move	NMLRXYNOTE,NoteHold
	pack	str2,newline,B1
	rep	str2,NoteHold
	call	Trim using NoteHold
	if (NoteHold = "")
		alert	caution,"Valid Notes are Required!",result
		move	YES,ReturnFlag
		setfocus MailerXYEditNotesa
		return
	endif
.
	getitem	MailerXYEditNotesb,0,NMLRXYNOTE2
.
.	getitem	MailerXYEditRecDate,0,str10
.	call	Trim using str10
.	call	RemoveChar using str10,SLASH
.	unpack	str10,MM,DD,YY,str2
.	pack	NMLRXYDATE,str2,YY,MM,DD
	if (NewFlag = YES)
		move	timestamp,NMLRXYDATE
	endif
.
	getitem	MailerXYEditUpdDate,0,str10
	call	Trim using str10
	call	RemoveChar using str10,SLASH
	unpack	str10,MM,DD,str4
	pack	NMLRXYUDATE,str4,MM,DD
.
	move	userlogn,NMLRXYINITS
	return

MailerReadRecords
	if (str6 <> "" & str6A <> "")
.Attempt to read Index
		call	ZFILLIT using str6
		call	ZFILLIT using str6a
		pack	NMLRXYFLD,str6,str6a
		move	"NMLRXYKEY-1",Location
		pack	KeyLocation,"Key: ",NMLRXYFLD
		call	NMLRXYKEY
		if not over
			call	MailerLoadListView
		else
			pack	NMLRXYFLD,str6a,str6
			move	"NMLRXYKEY-2",Location
			pack	KeyLocation,"Key: ",NMLRXYFLD
			call	NMLRXYKEY
			if not over
				call	MailerLoadListView
			else
				alert	note,"No records found that meet your criteria!",result
			endif
		endif
.Otherwise try Aamdex values
	elseif (str6 <> "")
		call	ZFILLIT using str6
		pack	NMLRXYFLD1,"01F",str6
		move	"NMLRXYAIM-1",Location
		pack	KeyLocation,"Key: ",NMLRXYFLD1
		call	NMLRXYAIM
		if not over
			loop
				until over
				if (str6 = NMLRXYMLR1 | str6 = NMLRXYMLR2)
					call	MailerLoadListView
				endif
				move	"NMLRXYKG-1",Location
				pack	KeyLocation,"Key: ",NMLRXYFLD1
				call	NMLRXYKG
			repeat
		else
			alert	note,"No records found that meet your criteria!",result
		endif
	else	.str6a <> ""
		call	ZFILLIT using str6a
		pack	NMLRXYFLD1,"01F",str6a
		move	"NMLRXYAIM-2",Location
		pack	KeyLocation,"Key: ",NMLRXYFLD1
		call	NMLRXYAIM
		if not over
			loop
				until over
				if (str6a = NMLRXYMLR1 | str6a = NMLRXYMLR2)
					call	MailerLoadListView
				endif
				move	"NMLRXYKG-2",Location
				pack	KeyLocation,"Key: ",NMLRXYFLD1
				call	NMLRXYKG
			repeat
		else
			alert	note,"No records found that meet your criteria!",result
		endif
	endif
	MailerXYListView.GetItemCount giving result
        if (result > 0)
                move    result,str9
                call    Trim using str9
                pack    str55,str9," Record(s) Found."
	else
		clear	str55
        endif
        setitem MailerXYStatRecords,0,str55
	MailerXYListView.SetItemState giving N9 using 0,2,2
	MailerXYListView.EnsureVisible using 0,0
	call	Click_MailerXYListView
	return

MailerLoadListView
	pack	hold,NMLRXYVARS
	pack	COMPFLD,NMLRXYMLR1
	move	"LoadLV-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
        MailerXYListView.InsertItem giving N9 using COMPCOMP
        MailerXYListView.SetItemText using N9,COMPNUM,1
        MailerXYListView.SetItemText using N9,COMPCOMP,2
	pack	COMPFLD,NMLRXYMLR2
	move	"LoadLV2-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
        MailerXYListView.SetItemText using N9,COMPNUM,3
        MailerXYListView.SetItemText using N9,COMPCOMP,4
        MailerXYListView.SetItemText using N9,NMLRXYNOTE,5
        MailerXYListView.SetItemText using N9,hold,6
	return

CalcPseudoMouseForm LRoutine EditPtr
.Function used to give a psuedo	Mouse_Down_Event in order to display OrderInfo
.by hitting the	F3 key while sitting on	certain	Edit Text Boxes.
	getprop	EditPtr,top=T1,left=L1,height=N8,width=N9
	calc	MouseForm=((N9*10000)+N8)
	return

MailerSetPrintOptions
.Allows	selection of Mailer/Mailer records for printing
.Called	by File menu & Print Button
	call	Report2DestroyObjects
	move    PORTN,NCNTFLD1
	rep     zfill,NCNTFLD1
	move    C3,NCNTPATH
	move    "Print-NCNTKEY",Location
	pack    KeyLocation,"Key: ",NCNTFLD1
	call    NCNTKEY
	setprop	Report2,title="NIN Mailer/Mailer Print Screen"
	move	NO,RptCan
	create	Report2;StatTextBoxes(1)=25:45:5:65,"Mlr Number",""
	create	Report2;EditTextBoxes(1)=25:45:65:135,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;Buttons(2)=25:45:135:160,"+"
	create	Report2;Buttons(3)=25:45:160:185,"-"
	create	Report2;ListViews(1)=45:170:5:320,MultiSelect=1,HideColHdr=1,FullRow=1
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	activate StatTextBoxes(1)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
	eventreg EditTextBoxes(1),10,MailerSetListKeyPress,RESULT=N9
	activate EditTextBoxes(1),MailerSetListEditChange,result
.Register LostFocus Event on EditTextBoxes so that corresponding "+/-" Buttons will have default
	eventreg EditTextBoxes(1),9,MailerSetButton2
	activate Buttons(1),MailerSetPrintOK,result
	activate Buttons(2),MailerSetPrintAdd,result
	activate Buttons(3),MailerSetPrintRemove,result
.Register LostFocus Event on ListViews so that corresponding "+/-" Buttons will have default
	eventreg ListViews(1),9,MailerSetButton3
.Register LostFocus Event on ListViews so that OK Button will have default
	eventreg ListViews(1),11,MailerSetButton4
.Register Double-Click Event on ListViews
	eventreg ListViews(1),6,MailerSetPrintRemove
	activate ListViews(1)
	ListViews(1).InsertColumn using "",50,0
	ListViews(1).InsertColumn using "",250,1
	listins	ObjectColl,StatTextBoxes(1),EditTextBoxes(1),Buttons(1),Buttons(2),Buttons(3),ListViews(1)
	setfocus EditTextBoxes(1)
	setprop	Report2,visible=1
	return

MailerSetListEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(1),0,str6
		call	Trim using str6
		move	C0,N6
		move	str6,N6
		move	N6,str6
		rep	zfill,str6
	endif
	return
MailerSetButton2
.Got Focus
	setprop	Buttons(2),default=1
	return
MailerSetButton3
.Got Focus
	setprop	Buttons(3),default=1
	return
MailerSetButton4
.Lost Focus
	setprop	Buttons(1),default=1
	return
MailerSetPrintOK
	call	MailerSetPrintAdd
	setprop	Report2,visible=0
.
	setmode	*mcursor=*wait
	call	CreateMailerMailer using ListViews(1)
	setmode	*mcursor=*arrow
.
	return

MailerSetPrintAdd
	getitem	EditTextBoxes(1),0,str6
	call	Trim using str6
	if (str6 <> "")
		call	ZFillIt using str6
		packkey	COMPFLD,str6
		move	"MSPAdd-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if not over
			if (COMPMLRFLG = "T")
				ListViews(1).GetItemCount giving howmany
				sub	C1,howmany
				for result,C0,howmany
					ListViews(1).GetItemText giving str6 using result
					if (str6 = COMPFLD)
						return
					endif
				repeat
				ListViews(1).InsertItem giving N9 using COMPFLD,9999
				ListViews(1).SetItemText using N9,COMPCOMP,1
			endif
		endif
	endif
	setitem	EditTextBoxes(1),0,""
	setfocus EditTextBoxes(1)
	return

MailerSetPrintRemove
.Test first to make sure there are records highlighted
	move	SEQ,result
	move	result,N9
	ListViews(1).GetNextItem giving result using C2,N9
	if (result = SEQ)
		return
	endif
	move	SEQ,result
	loop
		move	result,N9
		ListViews(1).GetNextItem giving result using C2,N9
 		until (result = SEQ)
		ListViews(1).DeleteItem using result
		sub	C1,result
	repeat
	return

MailerSetListKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo3
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return

Report2DestroyObjects
	destroy	ObjectColl
	return

	include	nmlrxyio.inc
	include	compio.inc
	include	cntio.inc
.	include	nmlrio.inc
	include	nuseio.inc
.Must be included for Search.plf
.	include	nbrkio.inc
	include	nownio.inc
	include	ndatio.inc
	include	ncmpio.inc
	include	nrtnio.inc
	include	ncntio.inc
	include	searchio.inc	  .contains logic for search.plf
	include	comlogic.inc
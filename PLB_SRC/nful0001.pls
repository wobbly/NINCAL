PC	EQU	1
	include	common.inc
	include	cons.inc
	include	winapi.inc
	include	nfuldd.inc
.START PATCH 1.1 ADDED LOGIC
	include	npasdd.inc
.END PATCH 1.1 ADDED LOGIC
................................................
release  init      "1.11"       16May05	DLH	bandaid add email
;release  init      "1.1"       19JUL02	ASH	ADDED PASSWORD FEATURE
.release  init      "1.0"       20DEC01	ASH	INITIAL RELEASE

hold	dim	300	.holds complete record plus extra
holdkey	dim	4
ExitFlag init	"Y"
ReturnFlag dim	1
NewFlag	dim	1
TIMER	timer
DimPtr	dim	^
FrmPtr	form	^
ColHeads automation
ColHead	automation
ListIts	automation
ListIt	automation
SubIt	automation
IntIndex integer 4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant

.Colors
white   color
grey    color

.Set Up Menu Bar
mFile   menu
mEdit   menu
mHelp   menu
 
.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"
 
.Set Vars used for About Box
        move    "NFUL0001.PLS",Wprognme
        move    "Fulfillment File Maintenance Program",Wfunction
        move    "Andrew Harkins",Wauthor
        move    release,Wrelease
        move    "December 20, 2001",Wreldate

mss1    plform  Error
abt     plform  About
.START PATCH 1.1 ADDED LOGIC
pss     plform  Passwrd
.END PATCH 1.1 ADDED LOGIC
x       plform  Nful0001
	winhide
	formload x
	formload abt
	formload mss1
.START PATCH 1.1 ADDED LOGIC
	formload pss
.END PATCH 1.1 ADDED LOGIC
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
.Create variant objects
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Timer creation
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
 
.Create Menus
	create  Nful0001;mFile,FData
	create  Nful0001;mEdit,EData,mFile
	create  Nful0001;mHelp,HData,mEdit
 
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
        activate mHelp,HelpGo,result
.
        getprop FulListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="Num.",*Width=45
	ColHeads.Add using *Index=3,*Key="two",*Text="Company",*Width=90
	ColHeads.Add using *Index=4,*Key="three",*Text="Contact",*Width=90
	ColHeads.Add using *Index=5,*Key="four",*Text="Fax Number",*Width=85
	ColHeads.Add using *Index=6,*Key="five",*Text="Complete Record",*Width=0
.
	setprop	FulListView,*HideColumnHeaders=OFALSE
	setprop	FulListView,*HideSelection=OFALSE
	setprop	FulListView,*FullRowSelect=OTRUE
	setprop	FulListView,*Sorted=OTRUE
	setprop	FulListView,*SortOrder=1
	setprop	FulListView,*SortKey=2
	setprop	FulListView,*AllowColumnReorder=OTRUE
	setprop	FulListView,*LabelEdit=1
	setprop	FulListView,*View=3
        getprop FulListView,*ListItems=ListIts
.        
	clock	timestamp,timestamp
.
	call	FulDisableLower
	setfocus FulEditSearch
.START PATCH 1.1 ADDED LOGIC
        move    "O",progcode
        move    "N",PassFlag
.END PATCH 1.1 ADDED LOGIC
.
	loop
		waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
.Test to make sure nothing is left open in Modify Mode
        beep
        beep
        beep
        shutdown

FileGo
.ExitFlag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo2

FileGo1
	return

FileGo2
	if (ExitFlag = NO)
		return
	endif
	winshow
	stop
EditGo
	return
HelpGo
        branch  result to HelpGo1
HelpGo1
        setprop AboutMssg,visible=1
        return

FulClearScreen
	setitem	FulEditNumber,0,""
	setitem FulEditCompany,0,""
	setitem FulEditContact,0,""
	setitem FulEditFax,0,""
;begin patch 1.11	
	setitem FulEditEmail,0,""
;begin patch 1.11	
	setitem	FulEditNotes,0,""
	setitem	FulEditDate,0,""
	setitem	FulStatMod,0,""
	return

FulDisableUpper
	setprop	FulOK,enabled=0
	setprop	FulListView,*enabled=0
	return

FulEnableUpper
	setprop	FulOK,enabled=1
	setprop	FulListView,*enabled=1
	return

FulDisableButtons
	setprop	FulNew,enabled=0
	setprop	FulModify,enabled=0
	return

FulEnableButtons
	setprop	FulNew,enabled=1
	getitem	FulEditNumber,0,str4
	if (str4 = "")
		setprop	FulModify,enabled=0
	else
		setprop	FulModify,enabled=1
	endif
	return

FulDisableButtons2
	setprop	FulQuit,enabled=0
	setprop	FulSave,enabled=0
	setprop	FulDelete,enabled=0
	setprop	FulExit,enabled=1
	return

FulEnableButtons2
	setprop	FulQuit,enabled=1
	setprop	FulSave,enabled=1
	setprop	FulExit,enabled=0
	return

FulDisableLower
	setprop	FulEditNumber,enabled=0,bgcolor=grey
	setprop	FulEditCompany,enabled=0,bgcolor=grey
	setprop	FulEditContact,enabled=0,bgcolor=grey
	setprop	FulEditFax,enabled=0,bgcolor=grey
;begin patch 1.11
	setprop	FulEditEmail,enabled=0,bgcolor=grey
;end patch 1.11
	setprop	FulEditNotes,enabled=0,bgcolor=grey
	setprop	FulEditDate,enabled=0,bgcolor=grey
	move	YES,ExitFlag
	return

FulEnableLower
	move	NO,ExitFlag
	setprop	FulEditNumber,enabled=1,bgcolor=white
	setprop	FulEditCompany,enabled=1,bgcolor=white
	setprop	FulEditContact,enabled=1,bgcolor=white
	setprop	FulEditFax,enabled=1,bgcolor=white
;begin patch 1.11
	setprop	FulEditEmail,enabled=1,bgcolor=white
;end patch 1.11
	setprop	FulEditNotes,enabled=1,bgcolor=white
	setprop	FulEditDate,enabled=1,bgcolor=white
	return

FulReadISAM LRoutine DimPtr
	packkey	NFULFLD,DimPtr
	move	C1,NFULPATH
	move	"ReadISAM-NFULKEY",Location
	pack	KeyLocation,"Key: ",NFULFLD
	call	NFULKEY
	if over
		pack	taskname,"Record not found using '",NFULFLD,"' as Fulfillment Number!",newline,"Try again."
		alert	caution,taskname,result
		setfocus FulEditSearch
		return
	else
		call	FulLoadScreen
	endif
	return

FulReadAAM LRoutine DimPtr,FrmPtr
	clear	NFULFLD1
	clear	NFULFLD2
	clear	NFULFLD3
	if (FrmPtr = 1)
		pack	NFULFLD1,"01F",DimPtr
	elseif (FrmPtr = 2)
		pack	NFULFLD2,"02F",DimPtr
	elseif (FrmPtr = 3)
		pack	NFULFLD3,"03F",DimPtr
	endif
	move	C1,NFULPATH
	move	"ReadAAM-NFULAIM",Location
	if (FrmPtr = 1)
		pack	KeyLocation,"Key: ",NFULFLD1
	elseif (FrmPtr = 2)
		pack	KeyLocation,"Key: ",NFULFLD2
	elseif (FrmPtr = 3)
		pack	KeyLocation,"Key: ",NFULFLD3
	endif
	call	NFULAIM
	loop
		until over
		call	FulLoadListView
		move	"ReadAAM-NFULKG",Location
		call	NFULKG
	repeat
	getprop	ListIts,*Count=N9
	if (N9 <> C0)
		setprop	ListIts(1),*Selected=C1
		call	Click_FulListView
	endif
	return

FulLoadListView
	pack	hold,NFULVARS
	ListIts.Add giving ListIt using *Index=1,*Text=""
	setprop ListIt,*SubItems(1)=NFULNUM
	setprop ListIt,*SubItems(2)=NFULCOMP
	setprop ListIt,*SubItems(3)=NFULCNT
	call	Trim using NFULFAX
	if (NFULFAX <> "")
		unpack	NFULFAX,str3,str2,str1,str4
		pack	str13,"(",str3,")",str2,str1,DASH,str4
	else
		clear	str13
	endif
	setprop ListIt,*SubItems(4)=str13
	setprop ListIt,*SubItems(5)=hold
	return

FulLoadScreen
	setitem	FulEditNumber,0,NFULNUM
	setitem FulEditCompany,0,NFULCOMP
	setitem FulEditContact,0,NFULCNT
	call	Trim using NFULFAX
	if (NFULFAX <> "")
		unpack	NFULFAX,str3,str2,str1,str4
		pack	str13,"(",str3,")",str2,str1,DASH,str4
	else
		clear	str13
	endif
	setitem FulEditFax,0,str13
	setitem	FulEditNotes,0,NFULNOTES
	call	Trim using NFULDATE
	if (NFULDATE <> "")
		unpack	NFULDATE,str4,MM,DD
		pack	str10,MM,SLASH,DD,SLASH,str4
	else
		clear	str10
	endif
	setitem	FulEditDate,0,str10
;begin patch 1.11
	setitem	FulEditEmail,0,NFulEmail
;end patch 1.11
	clear	str35
	call	Trim using NFULMODDATE
	call	Trim using NFULINITS
	if (NFULMODDATE <> "" | NFULINITS <> "")
		append	"Last Modified ",str35
		if (NFULMODDATE <> "")
			unpack	NFULMODDATE,str4,MM,DD
			pack	str11,MM,SLASH,DD,SLASH,str4,B1
			append	str11,str35
		endif
		if (NFULINITS <> "")
			append	"by ",str35
			append	NFULINITS,str35
		endif
		reset	str35
	endif
	setitem	FulStatMod,0,str35
	return

FulVerifyData
	if (NewFlag = YES)
		move	C0,N4
		loop
			add	C1,N4
			if (N4 >= 9998)
				alert	caution,"Maximum Number of Records Exceeded!!",result
				move	YES,ReturnFlag
				return
			endif
			move	N4,NFULFLD
			rep	zfill,NFULFLD
			move	C1,NFULPATH
			move	"Verify-NFULTST",Location
			pack	KeyLocation,"Key: ",NFULFLD
			call	NFULTST
			until over
		repeat
		setitem	FulEditNumber,0,NFULFLD
		move	NFULFLD,NFULNUM
	else
		getitem	FulEditNumber,0,NFULNUM
	endif
.
	getitem FulEditCompany,0,NFULCOMP
	call	Trim using NFULCOMP
	if (NFULCOMP = "")
		alert	caution,"Valid Company Name Required!",result
		setfocus FulEditCompany
		move	YES,ReturnFlag
		return
	endif
.
	getitem FulEditContact,0,NFULCNT
.
	getitem FulEditFax,0,str13
	move	"(",str1
	call	RemoveChar using str13,str1
	move	")",str1
	call	RemoveChar using str13,str1
	call	RemoveChar using str13,DASH
	call	Trim using str13
	count	result,str13
	if (result <> 10)
		alert	caution,"Valid 10 digit Fax Number Required!",result
		setfocus FulEditFax
		move	YES,ReturnFlag
		return
	endif
	move	str13,NFULFAX
;end patch 1.11	
	getitem	FulEditEmail,0,NFulEmail
;end patch 1.11	
.
	getitem	FulEditNotes,0,NFULNOTES
	return
..........................
	include	nfulio.inc
.START PATCH 1.1 ADDED LOGIC
	include	npasio.inc
.END PATCH 1.1 ADDED LOGIC
	include	comlogic.inc

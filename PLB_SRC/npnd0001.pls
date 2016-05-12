........................................
. Program:      NPND0001.PLS
. Function:     LCR/Pending Order Sub-Status Description File Maintenance Program
. Author:       Andrew Harkins
. Orig. Date:   June 3, 2002
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
	include	npnddd.inc
        include winapi.inc

release init    "1.0"   ASH 03JUN2002 ORIGINAL RELEASE

wdWindowStateMinimize integer 4,"0x00000002"
.
Timer   Timer
.Flags
ExitFlag init   "Y"
ReturnFlag init "N"
NewFlag init    "N"
.Length of records
hold    dim     48
key     dim     3
.hexeight integer 4,"4294967295"
.Colors
white   color
grey    color

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"

.Set Vars used for About Box
        move    "NPND0001.PLS",Wprognme
        move    "LCR/Pending Sub-Status File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    "1.0",Wrelease
        move    "June 03, 2002",Wreldate

.Declare forms, Always declare child forms first
mss1    plform  Error
abt     plform  About
x       plform  npnd0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload mss1

.Load OrderInfo
.
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  npnd0001;mFile,FData
        create  npnd0001;mEdit,EData,mFile
        create  npnd0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mEdit,EditGo,result
        activate mHelp,HelpGo,result

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220

.Load up Objects
.ListViews
        PendListView.InsertColumn using "Sort Key",0,1
        PendListView.InsertColumn using "Status Type",150,2
        PendListView.InsertColumn using "Status Code",90,3
        PendListView.InsertColumn using "Description",350,4
        PendListView.InsertColumn using "Hold",0,5
.Load the Object
	call	PendLoadListView
.Main Loop
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.Reset Screen
        call    PendDisableLower
        setfocus PendListView
.
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
        beep
        beep
        beep
        stop

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
FileGo2
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return

PendDisableUpper
        setprop PendListView,enabled=0,bgcolor=grey
	setprop	PendModify,enabled=0
	setprop	PendNew,enabled=0
	setprop	PendExit,enabled=0
        return

PendEnableUpper
        setprop PendListView,enabled=1,bgcolor=white
	setprop	PendModify,enabled=1
	setprop	PendNew,enabled=1
	setprop	PendExit,enabled=1
	move	NO,NewFlag
	move	YES,ExitFlag
        return

PendDisableLower
        setprop PendComboType,enabled=0,bgcolor=grey
        setprop PendEditCode,enabled=0,bgcolor=grey
        setprop PendEditDescription,enabled=0,bgcolor=grey
	setprop	PendQuit,enabled=0
	setprop	PendSave,enabled=0
	setprop	PendDelete,enabled=0
        return

PendEnableLower
	move	NO,ExitFlag
        setprop PendComboType,bgcolor=white
        setprop PendEditCode,bgcolor=white
        setprop PendEditDescription,enabled=1,bgcolor=white
	setprop	PendQuit,enabled=1
	setprop	PendSave,enabled=1
        return

PendClear
        setitem PendComboType,0,1
        setitem PendEditCode,0,""
        setitem PendEditDescription,0,""
        return

PendLoadListView
	PendListView.DeleteAllItems giving result
	if (NPNDFLAG = 1)
		read	NPNDFILE,SEQEOF;;
	endif
	loop
		move	"NPNDSEQ",Location
		pack	KeyLocation,"Key: Sequential"
		call	NPNDSEQ
		until over
		if (NPNDCODE = "p")
			move	"Pending Order",str35
		elseif (NPNDCODE = "l")
			move	"LCR",str35
		elseif (NPNDCODE = "x")
			move	"Cancelled Record",str35
		endif
		pack	str45,str35,NPNDSTAT
		pack	hold,NPNDVARS
	        PendListView.InsertItem giving N9 using str45
	        PendListView.SetItemText using N9,str35,1
	        PendListView.SetItemText using N9,NPNDSTAT,2
	        PendListView.SetItemText using N9,NPNDDESC,3
	        PendListView.SetItemText using N9,hold,4
	repeat
	PendListView.SetItemState giving result using 0,2,2
	call	Click_PendListView
	return

PendLoadScreens
	move	C1,N2
	if (NPNDCODE = "p")
		move	C2,N2
	elseif (NPNDCODE = "l")
		move	C3,N2
	elseif (NPNDCODE = "x")
		move	C4,N2
	endif
        setitem PendComboType,0,N2
        setitem PendEditCode,0,NPNDSTAT
        setitem PendEditDescription,0,NPNDDESC
        return

.Verify Data Entry
PendVerifyData
        getitem PendComboType,0,N2
	if (N2 = 2)
		move	"p",NPNDCODE
	elseif (N2 = 3)
		move	"l",NPNDCODE
	elseif (N2 = 4)
		move	"x",NPNDCODE
	else
		alert	caution,"Valid Status Type Required!",result
		move	YES,ReturnFlag
		setfocus PendComboType
		return
	endif
.
        getitem PendEditCode,0,NPNDSTAT
	call	Trim using NPNDSTAT
	move	C0,N2
	move	NPNDSTAT,N2
	if (N2 = 0)
		alert	caution,"Valid Status Code Required!",result
		move	YES,ReturnFlag
		setfocus PendEditCode
		return
	endif
	move	N2,NPNDSTAT
	rep	zfill,NPNDSTAT
	if (NewFlag = YES)
		pack	NPNDFLD,NPNDCODE,NPNDSTAT
		move	"Verify-NPNDTST",Location
		pack	KeyLocation,"Key: ",NPNDFLD
		call	NPNDTST
		if not over
			pack	taskname,"This Record Already Exists!",newline,"Choose another Type/Code Combination."
			alert	caution,taskname,result
			move	YES,ReturnFlag
			setfocus PendEditCode
			return
		endif
	endif
.
        getitem PendEditDescription,0,NPNDDESC
	call	Trim using NPNDDESC
	if (NPNDDESC = "")
		alert	caution,"Valid Status Description Required!",result
		move	YES,ReturnFlag
		setfocus PendEditDescription
		return
	endif
        return

.Include IO file
        include npndio.inc
        include comlogic.inc

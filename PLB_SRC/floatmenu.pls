PC EQU	0

	include common.inc
	include cons.inc

release	init 		"1.1" 	27JUL2004	ASH	Removed logic to clear Body so that Signature will appear
.release	init 		"1.0" 	15SEP2003	Added This Routine to be able to start outlook mailitem by right clicking, displaying listview and selecting

;External Routines
CompanyContactSearch	External	"CompSearchForm;CompanyContactSearch"
.Objects and Variables Required
;OutLook Vars
TO DIM 45
SUBJECT DIM 45
BODY    DIM 200
Session automation
Note    automation
Mes     automation      class="Outlook.Application"
.Formatting vars needed
olbyvalue integer 4,"0x1"
olFlagStatus integer 4,"0x2"
;Vars for Rt Click ListView
InfoString dim  47
ScrRight form   4
ScrBottom form  4 
clickptr form  10
FarRight form   4
FarBottom form  4
T1      form    4
L1      form    4
;str for email
str256	DIM	256
;Form that I will grab coordinates from
ParentForm	WINDOW	^
;Object I clicked
StatTextPtr	StatText	^
;Object I will Extract or Put Data
EditTextPtr     EditText	^
;StatText to point to which object to fill with search result usually a description
StatTextPtr2	StatText ^
;position of click on stattextptr
MouseClickPtr	FORM	^

CCB	FORM	^
COTYPE	FORM	^
CCBFORM FORM	1
COTYPEFORM	FORM	1
MOUSECLICKFORM	FORM	9
;Routine to call
FloatMenuRoutine	FORM	^
LoadFloatMenu Routine
FMenu	plform  FloatMenu
	formload FMenu
	return

;ParentCode-Comp0001
;			call	EmailFloatDisplay using COMP0001,Comp2StatEmail,Comp2EditEmail,MouseClick,FloatMenuItem
Looper


	LOOP	
	  EVENTWAIT	
	REPEAT	
EmailFloatDisplay Routine ParentForm,StatTextPtr,EditTextPtr,MouseClickPtr,FloatMenuRoutine
	getprop	FloatMenu,visible=n1
	if (n1 = c1)	
		Goto	FloatMenuClose
	Else
		FloatMenuListView.deleteallcontents
		FloatMenuListView.insertcolumn using "1",75,0
		FloatMenuListView.insertitem using "Email Contact",1 
		Call	showonscreen
		Goto	Looper
	endif
ParentCoFloatDisplay Routine	ParentForm,StatTextPtr,EditTextPtr,CCB,COTYPE,MouseClickPtr,FloatMenuRoutine
	getprop	FloatMenu,visible=n1
	if (n1 = c1)	
		Goto	FloatMenuClose
	Else
		FloatMenuListView.deleteallcontents
		FloatMenuListView.insertcolumn using "1",75,0
		FloatMenuListView.insertitem using "Company Search",1 
		move	CCB to CCBFORM
		MOVE	COTYPE to CCBFORM
		move	mouseclickptr to mouseclickform
		Call	showonscreen
		Goto	Looper
	endif





FloatMenuRoutines	Branch	FloatMenuRoutine,FloatEmailMessage,FloatSearchCompany

FloatEmailMessage
		getitem	EditTextPtr,0,str256
;Open Outlook application
	create  mes
;Create Message
	mes.createitem giving Note using 0
;Recipient
	setprop note,*to=str256
;Subject
	setprop note,*subject=""
;Body
.START PATCH 1.1 REMOVED LOGIC - CLEARING OF BODY CLEAR SIGNATURE
.	setprop note,*body=""
.END PATCH 1.1 REMOVED LOGIC - CLEARING OF BODY CLEAR SIGNATURE
;	setprop note,*flagstatus=olFlagStatus
	caLL	FloatMenuClose Routine
	Note.display
	return
FloatSearchCompany
	caLL	FloatMenuClose Routine
	Call	CompanyContactSearch using	Floatmenu,StatTextPtr,EditTextPtr,CCBFORM,CoTypeFORM,MOUSECLICKFORM
	Return
ShowOnScreen
.LOGIC in this section broken down into following generalized equation:
.
.OrderInfo_Top=(TopCoordinateOfMouseClick + TopCoordinateOfObjectWhereClickOccurred + Cushion + TopCoordinateOfProgram1Screen
.If ((OrderInfo_Top + OrderInfo_Height) > ScreenHeight)
.       OrderInfo_Top=(OrderInfo_Top - TopCoordinateOfMouseClick - Cushion
.Endif
.
.OrderInfo_Left=(LeftCoordinateOfMouseClick + LeftCoordinateOfObjectWhereClickOccurred + Cushion + LeftCoordinateOfProgram1Screen
.If ((OrderInfo_Left + OrderInfo_Width) > ScreenWidth)
.       OrderInfo_Left=(OrderInfo_Left - LeftCoordinateOfMouseClick - Cushion
.Endif
.
.
.Getinfo
.This is done each time in case the user changes their screen dimensions in the middle of
.using this program
	clear t1
	clear l1
.Get info from edittextptr
        getprop StatTextPtr,top=T1,left=L1
	clear   str25
	getinfo system,str25
	bump    str25,12
	move    str25,str4
	move    str4,ScrRight
	bump    str25,4
	move    str25,str4
	move    str4,ScrBottom
	setprop Fmenu,winpos=1
        getprop Parentform,top=H,left=V
;        getprop ParentForm,top=H,left=V
.Break down mouse coordinates - figured in terms of object where mouse was clicked
.clickptr established at MouseDown_Event
        move    "10000",N7
        div     N7,mouseclickptr,N9 .N9=left
        mult    N9,N7            
        sub     N7,mouseclickptr,N8 .N8=top
.Add to STATIC coordinates of object where mouse was clicked
.T1/L1 established at MouseDown_Event
        add     N9,L1           .L1=left
        add     N8,T1           .T1=top
.Calulate totals for positions
        add     T1,H
        add     "44",H          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible 
        add     L1,V
        add     "7",V           .Compensate to allow second click to make invisible
.Test to see if object will fit on page
        Getprop floatmenu,height=N9,width=N8
        add     N9,H,FarBottom
        if (FarBottom > ScrBottom)
                sub     N9,H
                sub     "20",H  .Compensate to allow second click to make invisible
        endif
        add     N8,V,FarRight
        if (FarRight > ScrRight)
                sub     N8,V
                sub     "10",V  .Compensate to allow second click to make invisible
        endif
        setprop fmenu,top=H,left=V
        setprop FloatMenuListView,top=0,left=0
	setprop	 FMenu,Height=20
        setprop FloatMenu,width=110
	setprop	 FloatMenuListView,Height=20
	setprop	 FloatMenuListView,Width=110
	setprop fmenu,visible=1
        return   
FloatMenuClose Routine
        setprop FloatMenu,visible=0
        setprop FloatMenu,winpos=3
        return


Exit
	include comlogic.inc

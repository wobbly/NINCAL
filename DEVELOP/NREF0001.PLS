	include	common.inc
	include	cons.inc
	include	nrefdd.inc
	include	ncatdd.inc

PC      EQU     1

release	INIT	"1.1"	DLH 08/15/2006 Add Min COdes
.release	INIT	"1.0"	ASH 08/05/2002 INITIAL RELEASE

NewFlag init	"N"
ReturnFlag init	"N"
hold	dim	81	.Length of NREFVARS
holdkey	dim	4
.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
OBOOL	variant
IntIndex integer 4
IntIndex2 integer 4
IntIndex3 integer 4
ColHeads automation
ColHead	automation
ListIts	automation
ListIt	automation
SubIt	automation
font2   font
.
white	color
grey	color

        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220

err     plform  Error
x	plform	nref0001
	winhide
	formload x
	formload err

.Create Column Headers for PackageListView
        getprop RefListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="Code",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="Code",*Width=150
	ColHeads.Add using *Index=3,*Key="two",*Text="Num",*Width=40
	ColHeads.Add using *Index=4,*Key="three",*Text="Description",*Width=175
	ColHeads.Add using *Index=5,*Key="four",*Text="Sub-Code",*Width=150
	ColHeads.Add using *Index=6,*Key="five",*Text="",*Width=0
	ColHeads.Add using *Index=7,*Key="six",*Text="MIN",*Width=40
.Set some properties for ListView object
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  OBOOL,VarType=VT_BOOL,VarValue=0
.
	setprop	RefListView,*HideColumnHeaders=OFALSE
	setprop	RefListView,*HideSelection=OFALSE
.	setprop	RefListView,*HotTracking=OTRUE
	setprop	RefListView,*FullRowSelect=OTRUE
.	setprop	RefListView,*MultiSelect=OTRUE
	setprop	RefListView,*Sorted=OTRUE
	setprop	RefListView,*SortOrder=0
.	setprop	RefListView,*AllowColumnReorder=OTRUE
	setprop	RefListView,*LabelEdit=1
	setprop	RefListView,*View=3
	setprop	RefListView,*Font=font2
.
        getprop RefListView,*ListItems=ListIts

.Main Loop
	call	RefDisableLower
	loop
		eventwait
	repeat

RefClearScreen
	setitem	RefComboCode,0,1
	setitem	RefComboSubCode,0,1
	setitem	RefEditDescription,0,""
	setitem	RefEditNumber,0,""
	setitem	NRefEditText001,0,""
	return

RefEnableLower
	setprop	RefComboCode,bgcolor=white
	setprop	RefComboSubCode,bgcolor=white
	setprop	RefEditDescription,enabled=1,bgcolor=white
	setprop	RefEditNumber,bgcolor=white
	setprop	NRefEditText001,enabled=1,bgcolor=white
	return

RefDisableLower
	setprop	RefComboCode,enabled=0,bgcolor=grey
	setprop	RefComboSubCode,enabled=0,bgcolor=grey
	setprop	RefEditDescription,enabled=0,bgcolor=grey
	setprop	RefEditNumber,enabled=0,bgcolor=grey
	setprop	NRefEditTExt001,enabled=0,bgcolor=grey
	move	NO,NewFlag
	return

RefDisableUpper
.	setprop	RefListView,enabled=0,bgcolor=grey
	setprop	RefComboSearchCode,enabled=0,bgcolor=grey
	return

RefEnableUpper
.	setprop	RefListView,enabled=1,bgcolor=white
	setprop	RefComboSearchCode,enabled=1,bgcolor=white
	return

RefDisableUpperButtons
	setprop	RefOK,enabled=0
	setprop	RefNew,enabled=0
	setprop	RefModify,enabled=0
RefDisableUpperButtons2
	setprop	RefQuit,enabled=0
	setprop	RefSave,enabled=0
	setprop	RefDelete,enabled=0
	return

RefEnableUpperButtons
	setprop	RefOK,enabled=1
	setprop	RefNew,enabled=1
	return

RefEnableUpperButtons2
	setprop	RefQuit,enabled=1
	setprop	RefSave,enabled=1
	return

RefLoadListView
	pack	hold,NREFVARS
	ListIts.Add giving ListIt using *Index=1,*Text=NREFCODE
	setprop ListIt,*SubItems(1)=str55
	setprop ListIt,*SubItems(3)=NREFDESC
	clear	str45
	if (NREFCODE = "T")	.Category Code - pull sub-code
		unpack	NREFNUM,str1,str2
		move	str2,NREFNUM
		move	str1,str2
		for N3,C1,"50"
			getitem	RefComboSubCode,N3,str1
			if (str1 = str2)
				getitem	RefComboSubCode,N3,str50
				unpack	str50,str4,str45
				break
			endif
		repeat
	endif
	setprop ListIt,*SubItems(2)=NREFNUM
	setprop ListIt,*SubItems(4)=str45
	setprop ListIt,*SubItems(5)=hold
	setprop ListIt,*SubItems(6)=NRefMin
	return

RefLoadScreen
	move	C1,N2
	for N3,C1,"50"
		getitem	RefComboCode,N3,str1
		if (str1 = NREFCODE)
			move	N3,N2
			break
		endif
	repeat
	setitem	RefComboCode,0,N2
.
	move	C1,N2
	if (NREFCODE = "T")	.Category Code - pull sub-code
		unpack	NREFNUM,str1,str3
		move	str1,str2
		for N3,C1,"50"
			getitem	RefComboSubCode,N3,str1
			if (str1 = str2)
				move	N3,N2
				break
			endif
		repeat
	else
		move	NREFNUM,str3
	endif
	setitem	RefComboSubCode,0,N2
	setitem	RefEditDescription,0,NREFDESC
	setitem	RefEditNumber,0,str3
	setitem	NrefEditText001,0,NrefMin
	
	return

RefVerifyData
	getitem	RefComboCode,0,result
	if (result = 1)
		alert	caution,"Code is Required!",result
		setfocus RefComboCode
		move	YES,ReturnFlag
		return
	endif
	getitem	RefComboCode,result,NREFCODE
.
	if (NREFCODE = "T")
		getitem	RefComboSubCode,0,result
		if (result = 1)
			alert	caution,"Category Codes require a Sub-Code!",result
			setfocus RefComboSubCode
			move	YES,ReturnFlag
			return
		endif
		getitem	RefComboSubCode,result,NCATCODE
	endif
	getitem	NRefEditText001,0,NREFMIN
.
	getitem	RefEditDescription,0,NREFDESC
	return

RefCheckNumber
	getitem	RefComboSubCode,0,result
	if (result > 1)
		getitem	RefEditNumber,0,str3
		move	C0,N3
		move	str3,N3
		if (N3 > 99)
			alert	caution,"Category Codes require a digit number!",result
			setfocus RefEditNumber
		endif
	endif
	return

	include	nrefio.inc
	include	comlogic.inc

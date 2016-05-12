	include	common.inc
	include	cons.inc
	INclude	Nacddd.inc
PC      EQU     1

release	INIT	"1.0"	DLH 08/15/2006 Add Min COdes
Reldate	Init	"30 August 2007"

NewFlag init	"N"
ReturnFlag init	"N"
hold	dim	81	.Length of VARS
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

;Set Up Menu Bar
mFile    menu
mHelp    menu
;Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
;
;Present Data for Menu Bar
FData   init    "&File;E&xit"
HData   init    "&Help;&About"
File	FIle	


        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220

err     plform  Error
abt            plform         About
x	plform	nACD0001
        move    "NACD0666.PLS",Wprognme
        move    "INVOICE Addition Charges",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate




	winhide
	formload x
	formload err
                    formload       abt

        CREATE  NACD0001;MFile,FData
        create  NACd0001;mHelp,HData,MFIle

;FileGo leads to stop
        activate mFile,FileGo,result
        activate mHelp,HelpGo,result


.Create Column Headers for PackageListView
        getprop AcdListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="Code",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="Code",*Width=150
	ColHeads.Add using *Index=3,*Key="two",*Text="Num",*Width=40
	ColHeads.Add using *Index=4,*Key="three",*Text="Description",*Width=175
	ColHeads.Add using *Index=5,*Key="four",*Text="Sub-Code",*Width=150
.Set some properties for ListView object
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  OBOOL,VarType=VT_BOOL,VarValue=0
.
	setprop	AcdListView,*HideColumnHeaders=OFALSE
	setprop	AcdListView,*HideSelection=OFALSE
.	setprop	AcdListView,*HotTracking=OTRUE
	setprop	AcdListView,*FullRowSelect=OTRUE
.	setprop	AcdListView,*MultiSelect=OTRUE
	setprop	AcdListView,*Sorted=OTRUE
	setprop	AcdListView,*SortOrder=0
.	setprop	AcdListView,*AllowColumnReorder=OTRUE
	setprop	AcdListView,*LabelEdit=1
	setprop	AcdListView,*View=3
	setprop	AcdListView,*Font=font2
.
        getprop AcdListView,*ListItems=ListIts
.Load the listview
.	Loop	
.	call	Nacdseq
.	until	over
.	call	AcdLoadListView
	
.	repeat

	Open	File,"c:\work\ninacd.dat"
	Loop	
	read	File,seq;NacdVars
	until	over
	if	(Nacdkey="115")
	move	Nacdkey,Nacdfld
	call	Nacdtst
	call	Nacdupd 
	elseif	(Nacdkey="156")
	move	Nacdkey,Nacdfld
	call	Nacdtst
	call	Nacdupd 
	Elseif	(Nacdkey = "157" or Nacdkey = "158")
	move	Nacdkey,Nacdfld
	call	NAcdtst
		call Nacdwrt if over
	endif
	
	repeat
	stop		


.Main Loop
	call	AcdDisableLower
	loop
		eventwait
	repeat

AcdClearScreen
	setitem	acdEditDescription,0,""
	setitem	acdEditNumber,0,""
	setitem	acdEditText001,0,""
	return

AcdEnableLower
	setprop	AcdEditDescription,enabled=1,bgcolor=white
	setprop	AcdEditNumber,bgcolor=white
	setprop	AcdEditText001,enabled=1,bgcolor=white
	return

AcdDisableLower
	setprop	AcdEditDescription,enabled=0,bgcolor=grey
	setprop	AcdEditNumber,enabled=0,bgcolor=grey
	setprop	AcdEditTExt001,enabled=0,bgcolor=grey
	move	NO,NewFlag
	return

AcdDisableUpper
.	setprop	AcdListView,enabled=0,bgcolor=grey
	return

AcdEnableUpper
.	setprop	AcdListView,enabled=1,bgcolor=white
	return

AcdDisableUpperButtons
	setprop	AcdNew,enabled=0
	setprop	AcdModify,enabled=0
AcdDisableUpperButtons2
	setprop	AcdQuit,enabled=0
	setprop	AcdSave,enabled=0
	return

AcdEnableUpperButtons
	setprop	AcdNew,enabled=1
	return

AcdEnableUpperButtons2
	setprop	AcdQuit,enabled=1
	setprop	AcdSave,enabled=1
	return

AcdLoadListView
	pack	hold,NAcdVARS
	ListIts.Add giving ListIt using *Index=1,*Text=NAcdKey
	setprop ListIt,*SubItems(1)=str55
	setprop ListIt,*SubItems(3)=NAcdText
	setprop ListIt,*SubItems(2)=NAcdKey
	setprop ListIt,*SubItems(4)=Nacdtype
.	setprop ListIt,*SubItems(5)=NacdType


	return

AcdLoadScreen
.
	setitem	AcdEditDescription,0,Nacdtext
	setitem	AcdEditNumber,0,str3
	
	return

AcdVerifyData
	getitem	AcdEditNumber,result,Nacdkey
.
.
	getitem	AcdEditDescription,0,Nacdtext

	Getitem	ACDEditText001,0,Nacdtype
	return

AcdCheckNumber
	return
;......................................................................................................
FileGo
                winshow
                stop
...........................................................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return
..
	include	nacdio.inc
	include	comlogic.inc

........................................
. Program:      NDMX0001.PLS
. Function:     DMExchange File(s) Maintenance
. Author:       Andrew Harkins
. Orig. Date:   July 1,2005
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        	include common.inc
        	include cons.inc
        	include norddd.inc
	include	media.inc
        	include nshpdd.inc
        	include	compdd.inc
        	include	cntdd.inc
	include	nsel2dd.inc
	include	dmexdd.inc
	include	oslspern.inc

release init    "1.1"		08Mar2007  DLH	Oslspern.inc expansion
Reldate	Init	"08 Mar 2007"
.release init    "1.0"   ASH 01JUL2005 ORIGINAL RELEASE
.	move	"July 1, 2005",Wreldate
	MOVe	Reldate,Wreldate

.Files
DMXFILE	IFILE	KEYL=6
DMXFILE2 	IFILE	KEYL=6
.
Timer   Timer
ExitFlag init   "Y"
ReturnFlag init "N"
NewFlag init    "N"
hold    dim     500	.ORDVARS(408) + Shipping vars(78) + some buffer space
holdkey dim     6
.
SearchLR dim	6
SearchShip form	1
SearchOrd form	1
DMXLoop	form	1
TypeFlag form	1
CurRec	form	5.2
CurVal	form	3
LastVal	form	3
.
FrmPtr	form	^
.
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

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"

.Set Vars used for About Box
	move    "NDMX0001.PLS",Wprognme
	move    "DMExchange File(s) Maintenance",Wfunction
	move    "Andrew Harkins",Wauthor

.Declare forms, Always declare child forms first
mss1    plform  Error
abt     plform  About
x       plform  Ndmx0001
        winhide
.Load Forms, Always load parent form first
	formload x
	formload abt
	formload mss1

	CREATE  TIMER,18000     .30 minutes
	ACTIVATE TIMER,Timeout,RESULT
.Create Menus
	create  Ndmx0001;mFile,FData
	create  Ndmx0001;mEdit,EData,mFile
	create  Ndmx0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
	activate mFile,FileGo,result
.Need this when it works
	activate mEdit,EditGo,result
.Only a SubMenu under this one
	activate mHelp,HelpGo,result

        getprop DMXListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="Type",*Width=65
	ColHeads.Add using *Index=3,*Key="two",*Text="LR",*Width=50
	ColHeads.Add using *Index=4,*Key="three",*Text="Status",*Width=65
	ColHeads.Add using *Index=5,*Key="four",*Text="Mailer",*Width=75
	ColHeads.Add using *Index=6,*Key="five",*Text="Broker",*Width=75
	ColHeads.Add using *Index=7,*Key="six",*Text="PO",*Width=75
	ColHeads.Add using *Index=8,*Key="seven",*Text="List",*Width=75
	ColHeads.Add using *Index=9,*Key="eight",*Text="Select",*Width=75
	ColHeads.Add using *Index=10,*Key="nine",*Text="Mail Date",*Width=75
	ColHeads.Add using *Index=11,*Key="ten",*Text="Order Date",*Width=75
	ColHeads.Add using *Index=12,*Key="eleven",*Text="Qty",*Width=70,*Alignment=1
	ColHeads.Add using *Index=13,*Key="twelve",*Text="Media",*Width=75
	ColHeads.Add using *Index=14,*Key="thirteen",*Text="Shipping",*Width=75
	ColHeads.Add using *Index=15,*Key="fourteen",*Text="Ship Qty",*Width=70,*Alignment=1
	ColHeads.Add using *Index=16,*Key="fifteen",*Text="Ship Date",*Width=75
	ColHeads.Add using *Index=17,*Key="sixteen",*Text="Tracking",*Width=75
	ColHeads.Add using *Index=18,*Key="seventeen",*Text="Salesperson",*Width=100
	ColHeads.Add using *Index=19,*Key="eighteen",*Text="Complete Record",*Width=0
.Sort Fields
	ColHeads.Add using *Index=20,*Key="nineteen",*Text="Mail Date Sort",*Width=0
	ColHeads.Add using *Index=21,*Key="twenty",*Text="Order Date Sort",*Width=0
	ColHeads.Add using *Index=22,*Key="twentyone",*Text="Qty Sort",*Width=0
	ColHeads.Add using *Index=23,*Key="twentytwo",*Text="Ship Qty Sort",*Width=0
	ColHeads.Add using *Index=24,*Key="twentythree",*Text="Ship Date Sort",*Width=0
.Set some properties for ListView object
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
.
	setprop	DMXListView,*HideColumnHeaders=OFALSE
	setprop	DMXListView,*HideSelection=OFALSE
	setprop	DMXListView,*FullRowSelect=OTRUE
	setprop	DMXListView,*MultiSelect=OTRUE
	setprop	DMXListView,*Sorted=OTRUE
	setprop	DMXListView,*SortOrder=0
	setprop	DMXListView,*SortKey=2
	setprop	DMXListView,*AllowColumnReorder=OTRUE
	setprop	DMXListView,*LabelEdit=1
	setprop	DMXListView,*View=3
        getprop DMXListView,*ListItems=ListIts
.
	clock	timestamp,timestamp
.
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
	call	DMXDisableLower
	setfocus DMXEditSearchLR
.Open files
	trap    IOMssg if IO
	open    DMXFILE,"shipfax|10.10.30.103:502"
	open    DMXFILE2,"DMXFILE|10.10.30.103:502"
	trapclr IO
	move	C1,NORDPATH
.Main Loop
	clock   timestamp,timestamp
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
        branch result to FileGo1
FileGo1
        if (ExitFlag = "Y" & DMXLoop = C0)
                winshow
                stop
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return


.Disable Upper Screen
DMXDisableUpper
	setprop DMXEditSearchLR,enabled=0,bgcolor=grey
	setprop DMXCheckShip,enabled=0
	setprop DMXCheckOrder,enabled=0
	setprop DMXOK,enabled=0
	setprop DMXExit,enabled=0
	setprop DMXNew,enabled=0
	setprop DMXDelete,enabled=0
	setprop DMXListView,*enabled=0
        return

.Enable Upper Screen
DMXEnableUpper
.Allow Exit
        move    "Y",ExitFlag
	setprop DMXEditSearchLR,enabled=1,bgcolor=white
	setprop DMXCheckShip,enabled=1
	setprop DMXCheckOrder,enabled=1
	setprop DMXOK,enabled=1
	setprop DMXExit,enabled=1
	setprop DMXNew,enabled=1
	setprop DMXListView,*enabled=1
        return

.Disable Lower Screen
DMXDisableLower
	setprop DMXEditLR,enabled=0,bgcolor=grey
	setprop DMXComboRecType,enabled=0
	setprop DMXQuit,enabled=0
	setprop DMXSAve,enabled=0
        return

.Enable Lower Screen
DMXEnableLower
	move    "N",ExitFlag
	setprop DMXEditLR,enabled=1,bgcolor=white
	setprop DMXComboRecType,enabled=1
	setprop DMXQuit,enabled=1
	setprop DMXSave,enabled=1
        return

DMXClearScreen
.Clear all Text Fields
        setitem DMXEditLR,0,""
        setitem DMXComboRecType,0,1
        setitem DMXStatStatus1,0,""
        setitem DMXStatMailer1,0,""
        setitem DMXStatBroker1,0,""
        setitem DMXStatPO1,0,""
        setitem DMXStatList1,0,""
        setitem DMXStatSelect1,0,""
        setitem DMXStatShipVia1,0,""
        setitem DMXStatTracking1,0,""
        setitem DMXStatMailDate1,0,""
        setitem DMXStatOrderDate1,0,""
        setitem DMXStatOrderQty1,0,""
        setitem DMXStatMedia1,0,""
        setitem DMXStatShipDate1,0,""
        setitem DMXStatShipQty1,0,""
        setitem DMXStatSalesperson1,0,""
        return

DMXReadFiles
	if (SearchLR = "")
		move	C0,howmany
		move	C0,result
		if (SearchShip = 1)
			open	tempfile,"shipfax"
			positeof tempfile
			fposit	tempfile,N10
			calc	howmany=(N10/501)	.'501 = 499(shipfax record length) + 2 bytes for CR/LF
			close	tempfile
		endif
		if (SearchOrd = 1)
			open	tempfile,"DMXFILE"
			positeof tempfile
			fposit	tempfile,N10
			calc	result=(N10/410)	.'410 = 408(DMXFILE record length) + 2 bytes for CR/LF
			close	tempfile
		endif
		add	result,howmany
	endif
.
	move	C1,DMXLoop
	setprop	DMXQuit,enabled=1
	if (SearchShip = 1)
.LR mode
		close	DMXFILE
		trap    IOMssg if IO
		open	DMXFILE,"shipfax|10.10.30.103:502"
		trapclr IO
		if (SearchLR <> "")
			read	DMXFILE,SearchLR;ORDVARS
			loop
				until over
				until (SearchLR <> OLRN)
				eventcheck
				if (DMXLoop = C0)
					goto DMXLoopBreak
				endif
				call	DMXLoadListView using C1
				readks	DMXFILE;ORDVARS
			repeat
		else
.Read ShipFax.dat mode
			loop
				read	DMXFILE,SEQ;ORDVARS
				until over
				eventcheck
				if (DMXLoop = C0)
					goto DMXLoopBreak
				endif
				call	DMXLoadListView using C1
				call	NDMXUpdateProgressBar
			repeat
		endif
	endif
	if (SearchOrd = 1)
.LR mode
		close	DMXFILE2
		trap	IOMssg if IO
		open	DMXFILE2,"DMXFILE|10.10.30.103:502"
		trapclr IO
		if (SearchLR <> "")
			read	DMXFILE2,SearchLR;ORDVARS
			loop
				until over
				until (SearchLR <> OLRN)
				eventcheck
				if (DMXLoop = C0)
					goto DMXLoopBreak
				endif
				call	DMXLoadListView using C2
				readks	DMXFILE2;ORDVARS
			repeat
		else
.Read DMXFile.dat
			loop
				read	DMXFILE2,SEQ;ORDVARS
				until over
				eventcheck
				if (DMXLoop = C0)
					goto DMXLoopBreak
				endif
				call	DMXLoadListView using C2
				call	NDMXUpdateProgressBar
			repeat
		endif
	endif
DMXLoopBreak
	setprop	DMXQuit,enabled=0
	getprop	ListIts,*Count=N9
	if (N9 <> C0)
		if (N9 = 1)
			pack	taskname,"1 Record Found."
		else
			move	N9,str9
			call	FormatNumeric using str9,str11
			pack	taskname,str11," Records Found."
		endif
		setitem	DMXStatRecords,0,taskname
		for IntIndex,"1",N9
			setprop	ListIts(IntIndex),*Selected=C0
		repeat
		setprop	ListIts(1),*Selected=C1
		call	Click_DMXListView
		setprop	DMXDelete,enabled=1
	else
		setitem	DMXStatRecords,0,"No Records Found."
		setprop	DMXDelete,enabled=0
	endif
	move	C0,DMXLoop
	return

DMXLoadListView Routine FrmPtr
	ListIts.Add giving ListIt using *Index=1,*Text=""
.LR Number
	setprop ListIt,*SubItems(2)=OLRN
.LR Status
	if (OSTAT = "l")
		pack	str45,"LCR"
	elseif (OSTAT = "z")
		pack	str45,"Cancelled LCR"
	elseif (OSTAT = "p")
		pack	str45,"Pending Order"
	elseif (OSTAT =	"x")
		pack	str45,"Cancelled Pending"
	elseif (OSTAT =	"0")
		pack	str45,"Live Order"
	elseif (OSTAT =	"B")
		pack	str45,"Billed Order"
	elseif (OSTAT =	"Q")
		pack	str45,"Cancelled-Billed"
	elseif (OSTAT =	"X")
		pack	str45,"Cancelled Order"
	else
		clear	str45
	endif
	setprop ListIt,*SubItems(3)=str45
.Mailer Name
	pack	COMPFLD3,OMLRNUM
	move	"Load-COMPKEY3",Location
	pack	KeyLocation,COMPFLD3
	call	COMPKEY3
	setprop ListIt,*SubItems(4)=COMPCOMP
.Broker Name
	call	Trim using OBRKNUM
	if (OBRKNUM <> "")
		pack	COMPFLD4,OBRKNUM
		move	"Load-COMPKEY2",Location
		pack	KeyLocation,COMPFLD4
		call	COMPKEY2
	else
		clear	COMPCOMP
	endif
	setprop ListIt,*SubItems(5)=COMPCOMP
.PO Number
	setprop ListIt,*SubItems(6)=OMLRPON
.List Name
	setprop ListIt,*SubItems(7)=O1DES
.Select
	packkey	NSEL2FLD,"1",OLRN
	move	"Load-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
	setprop ListIt,*SubItems(8)=NSEL2NAME
.Mail Date
	call	Trim using OMDTEC
	if (OMDTEC <> "")
		pack	str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	else
		clear	str10
	endif
	setprop ListIt,*SubItems(9)=str10
.Mail Date Sort
	move	OMDTEC,CC
	move	OMDTEY,YY
	move	OMDTEM,MM
	move	OMDTED,DD
	call	CVTJUL
	move	JULDAYS,str5
	setprop ListIt,*SubItems(19)=str5
two
.Order Date
	call	Trim using OODTEC
	if (OODTEC <> "")
		pack	str10,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	else
		clear	str10
	endif
	setprop ListIt,*SubItems(10)=str10
.Order Date Sort
one
	move	OODTEC,CC
	move	OODTEY,YY
	move	OODTEM,MM
	move	OODTED,DD
	call	CVTJUL
	move	JULDAYS,str5
	setprop ListIt,*SubItems(20)=str5
three
.Order Qty
	call	Trim using OQTY
	if (OQTY <> "")
		move	OQTY,str9
		call	FormatNumeric using str9,str11
	else
		clear	str11
	endif
	setprop ListIt,*SubItems(11)=str11
.Order Qty Sort
	move	C0,N9
	move	OQTY,N9
	move	N9,str9
	rep	zfill,str9
	setprop ListIt,*SubItems(21)=str9
.Media
	clear	MEDIA
	call	Trim using OFOCODE
	if (OFOCODE <> "")
		move	C0,N2
		move	OFOCODE,N2
		move	MED0,MEDIA
		load	MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
			MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
			MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
			MED23,MED24,MED25,med26,med27,med28,med29
	endif
	setprop ListIt,*SubItems(12)=MEDIA
.Salesperson
	clear	taskname
	pack	str2,OSALES10,OSALES
	call	Trim using str2
	if (str2 <> "")
		move	C0,N2
		move	str2,N2
		if (N2 > 0)
			move	osls0,str45
			load	str45 from N2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
				osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
				osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
			osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
			move	N2,str2
			rep	zfill,str2
			pack	taskname,str2," - ",str45
		endif
	endif
	setprop ListIt,*SubItems(17)=taskname
...Shipping Record...
	if (FrmPtr = C1)
.Type of Record
		setprop ListIt,*SubItems(1)="Shipping"
.Shipping Via
		pack	NSHPFLD,OLRN
		rep	zfill,NSHPFLD
		move	"NSHPKEY",Location
		pack	KeyLocation,"Key: ",NSHPFLD
		call	NSHPKEY
		if not over
.Because we may Trim variables later, I am loading hold here!!
			pack	hold,ORDVARS,"S",SINFO,SQUANT,SDATE,STRACK
			setprop ListIt,*SubItems(13)=SINFO
.Ship Qty
			call	Trim using SQUANT
			if (SQUANT <> "")
				move	SQUANT,str9
				call	FormatNumeric using str9,str11
			else
				clear	str11
			endif
			setprop ListIt,*SubItems(14)=str11
.Order Qty Sort
			move	C0,N9
			move	SQUANT,N9
			move	N9,str9
			rep	zfill,str9
			setprop ListIt,*SubItems(22)=str9
.Ship Date
			call	Trim using SDATE
			if (SDATE <> "")
				unpack	SDATE,str4,MM,DD
				pack	str10,MM,SLASH,DD,SLASH,str4
			else
				clear	str10
			endif
			setprop ListIt,*SubItems(15)=str10
.Ship Date Sort
			unpack	str10,MM,str1,DD,str1,CC,YY
			call	CVTJUL
			move	JULDAYS,str5
			setprop ListIt,*SubItems(23)=str5
.Tracking
			setprop ListIt,*SubItems(16)=STRACK
		else
			pack	hold,ORDVARS,"S"
			clear	str11
			clear	str10
		endif
.Complete Record
		setprop ListIt,*SubItems(18)=hold
...New Order Record...
	elseif (FrmPtr = C2)
.Type of Record
		setprop ListIt,*SubItems(1)="New Order"
.Complete Record
		pack	hold,ORDVARS,"O"
		setprop ListIt,*SubItems(18)=hold
	endif
        return

DMXLoadScreen
	call	Trim using OLRN
	if (OLRN = "")
		call	DMXClearScreen
		return
	endif
        setitem DMXEditLR,0,OLRN
	if (str1 = "S")
        	setitem DMXComboRecType,0,2
.Shipping
		setitem DMXStatShipVia1,0,SINFO
.Tracking
		setitem DMXStatTracking1,0,STRACK
.Ship Date
		call	Trim using SDATE
		if (SDATE <> "")
			unpack	SDATE,str4,MM,DD
			pack	str10,MM,SLASH,DD,SLASH,str4
		else
			clear	str10
		endif
		setitem DMXStatShipDate1,0,str10
.Ship Qty
		call	Trim using SQUANT
		if (SQUANT <> "")
			move	SQUANT,str9
			call	FormatNumeric using str9,str11
		else
			clear	str11
		endif
		setitem DMXStatShipQty1,0,str11
        else
		if (str1 = "O")
			setitem DMXComboRecType,0,3
		else
			setitem DMXComboRecType,0,1
        	endif
.Shipping
		setitem DMXStatShipVia1,0,""
		setitem DMXStatTracking1,0,""
		setitem DMXStatShipDate1,0,""
		setitem DMXStatShipQty1,0,""
        endif
.LR Status
	if (OSTAT = "l")
		pack	str45,"LCR"
	elseif (OSTAT = "z")
		pack	str45,"Cancelled LCR"
	elseif (OSTAT = "p")
		pack	str45,"Pending Order"
	elseif (OSTAT =	"x")
		pack	str45,"Cancelled Pending"
	elseif (OSTAT =	"0")
		pack	str45,"Live Order"
	elseif (OSTAT =	"B")
		pack	str45,"Billed Order"
	elseif (OSTAT =	"Q")
		pack	str45,"Cancelled-Billed"
	elseif (OSTAT =	"X")
		pack	str45,"Cancelled Order"
	else
		clear	str45
	endif
        setitem DMXStatStatus1,0,str45
.Mailer
	pack	COMPFLD3,OMLRNUM
	move	"LoadS-COMPKEY3",Location
	pack	KeyLocation,COMPFLD3
	call	COMPKEY3
	if not over
		pack	taskname,COMPNUM,B1,DASH,B1,COMPCOMP
	else
		clear	taskname
	endif
	setitem DMXStatMailer1,0,taskname
.Broker
	call	Trim using OBRKNUM
	if (OBRKNUM <> "")
		pack	COMPFLD4,OBRKNUM
		move	"LoadS-COMPKEY2",Location
		pack	KeyLocation,COMPFLD4
		call	COMPKEY2
		if not over
			pack	taskname,COMPNUM,B1,DASH,B1,COMPCOMP
		else
			clear	taskname
		endif
	else
		clear	taskname
	endif
	setitem DMXStatBroker1,0,taskname
.PO
	setitem DMXStatPO1,0,OMLRPON
.List
	pack	taskname,OLNUM,B1,DASH,B1,O1DES
	setitem DMXStatList1,0,taskname
.Select
	packkey	NSEL2FLD,"1",OLRN
	move	"LoadS-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
		move	"XXXX",NSEL2NUM
	endif
	pack	taskname,NSEL2NUM,B1,DASH,B1,NSEL2NAME
	setitem DMXStatSelect1,0,taskname
.Mail Date
	call	Trim using OMDTEC
	if (OMDTEC <> "")
		pack	str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	else
		clear	str10
	endif
	setitem DMXStatMailDate1,0,str10
.Order Date
	call	Trim using OODTEC
	if (OODTEC <> "")
		pack	str10,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	else
		clear	str10
	endif
	setitem DMXStatOrderDate1,0,str10
.Order Qty
	call	Trim using OQTY
	if (OQTY <> "")
		move	OQTY,str9
		call	FormatNumeric using str9,str11
	else
		clear	str11
	endif
	setitem DMXStatOrderQty1,0,str11
.Media
	clear	MEDIA
	call	Trim using OFOCODE
	if (OFOCODE <> "")
		move	C0,N2
		move	OFOCODE,N2
		move	MED0,MEDIA
		load	MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
			MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
			MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
			MED23,MED24,MED25,med26,med27,med28,med29
	endif
	setitem DMXStatMedia1,0,MEDIA
.Salesperson
	clear	taskname
	pack	str2,OSALES10,OSALES
	call	Trim using str2
	if (str2 <> "")
		move	C0,N2
		move	str2,N2
		if (N2 > 0)
			move	osls0,str45
			load	str45 from N2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
				osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
				osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
			osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
			move	N2,str2
			rep	zfill,str2
			pack	taskname,str2," - ",str45
		endif
	endif
	setitem DMXStatSalesperson1,0,taskname
	return

.Verify Data Entry
DMXVerifyData
.LR
	getitem DMXEditLR,0,OLRN
	call	Trim using OLRN
	if (OLRN = "")
		alert	note,"Valid LR Number required!",result
		setfocus DMXEditLR
		move	YES,ReturnFlag
		return
	endif
	move	OLRN,NORDFLD
	call	ZFillIt using NORDFLD
	setitem DMXEditLR,0,NORDFLD
	move	"Verify-NORDKEY",Location
	pack	KeyLocation,"Key: ",NORDFLD
	call	NORDKEY
	if over
		alert	note,"Valid LR Number required!",result
		setfocus DMXEditLR
		move	YES,ReturnFlag
		return
	endif
.Type
	getitem DMXComboRecType,0,TypeFlag
	if (TypeFlag <> 2 & TypeFlag <> 3)
		alert	note,"You must select a Record Type!",result
		setfocus DMXComboRecType
		move	YES,ReturnFlag
		return
	endif
.DMXFile
	if (TypeFlag = 2)
.Open just in call not already open
		move	"Verify-shipfax",Location
		open    DMXFILE,"shipfax|10.10.30.103:502"
		read	DMXFILE,NORDFLD;;
.Open just in call not already open
	elseif (TypeFlag = 3)
		move	"Verify-DMXFILE",Location
		open    DMXFILE2,"DMXFILE|10.10.30.103:502"
		read	DMXFILE2,NORDFLD;;
	endif
	if not over
		alert	note,"This LR already exists in DMX File!",result
		setfocus DMXEditLR
		move	YES,ReturnFlag
		return
	endif
.Check Shipping File
	pack	NSHPFLD,OLRN
	rep	zfill,NSHPFLD
	move	"Verify-NSHPKEY",Location
	pack	KeyLocation,"Key: ",NSHPFLD
	call	NSHPKEY
	if over
		if (TypeFlag = 2)	.Shipping
			alert	note,"There is no Shipping Record for this LR!",result
			setfocus DMXComboRecType
			move	YES,ReturnFlag
			return
		endif
	else
		if (TypeFlag = 3)	.New Order
			alert	note,"You cannot create a 'New Order' record for an LR that has already Shipped!",result
			setfocus DMXComboRecType
			move	YES,ReturnFlag
			return
		endif
	endif
	return

NDMXUpdateProgressBar
	calc	CurRec=(CurRec+1)
	calc	CurVal=((CurRec/howmany)*100)
	if (CurVal <> LastVal)
		setitem	DMXProgressBar,0,CurVal
		move	CurVal,LastVal
	endif
	return
NDMXInitProgressBar
	move	C0,CurRec
	move	C0,CurVal
	move	C0,LastVal
	return

.Include IO file
        include nordio.inc
        include nshpio.inc
	include	compio.inc
	include	cntio.inc
	include	nsel2io.inc
        include comlogic.inc
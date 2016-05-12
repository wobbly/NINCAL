PC	EQU	1
        include common.inc
        include cons.inc
	include	norddd.inc
.	include f:\library\develop\backups\norddd.inc
        include ncmpdd.inc
	include	nusedd.inc

release init    "1.0" 		23APR02	ASH	NEW RELEASE
.EXTERNAL ROUTINES FROM	NORDTEST.PLC
OrderSetShipToInfo external "NORDTEST;OrderSetShipToInfo"
.EXTERNAL ROUTINES FROM	NCMP0002.PLC
OrderCreateCampaign external "NCMP0002;CreateCampaign"
OrderCreateCampaignA external "NCMP002A;CreateCampaignA"
OrderCreateCampaignB external "NCMP002B;CreateCampaignB"

userlogn	dim	7
EditTextBoxes	EditText (2)
Buttons		Button	(2)
ComboBoxes	ComboBox (3)
StatTextBoxes	StatText (5)
ComboPtr	ComboBox ^
.Vars used for Report Screen
RptCan	dim	1
RED	color

rpt2	plform	Report2
mss1	plform	Error
	winhide
	formload rpt2
	formload mss1

	create	RED=*RED
.Locate User Information
	clock	port,str3
	unpack	str3,str2,str1
	pack	str3,str1,str2
	move	str3,PORTN
.
	move	C0,NUSEFLD
	move	C1,NUSEPATH
	move	PORTN,NUSEFLD
	rep	zfill,NUSEFLD
	call	NUSEKEY
	scan	"INVALID",NUSEUSER
	reset	NUSEUSER
	call	Trim using NUSEUSER
	scan	"BILLING",NUSEUSER	 
	if not equal
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
	endif
	reset	NUSEUSER
.Load ComboBox
	create	ComboBoxes(3)=1:10:1:10,"",""
	open	tempfile,"c:\work\combo.dat"
	for N9,C0,"49"
		read	tempfile,SEQ;str55
		call	Trim using str55
		insertitem ComboBoxes(3),N9,str55
	repeat
	close	tempfile,delete
.Get Campaign
	call	OrderSetCampaignReports
	setprop	Report2,visible=1
	if (RptCan = NO)
.NCMPKEY already called	in Report2OK routine
		getitem	ComboBoxes(1),0,N9
		if (N9 = 1)	.Need File Name.
			 getitem ComboBoxes(2),0,N8
			 if (N8	= 1)
				 call	 OrderCreateCampaign using NCMPFLD,ComboBoxes(3),userlogn
			 elseif	(N8 = 2)
				 call	 OrderCreateCampaignA using NCMPFLD,ComboBoxes(3),userlogn
			 elseif	(N8 = 3)
				 call	 OrderCreateCampaignB using NCMPFLD,ComboBoxes(3),userlogn
			 endif
		endif
	endif
	shutdown

OrderSetCampaignReports
.Allows	selection of Campaign for printing
.Called	by Reports menu, Campaign submenu
	setprop	Report2,title="NINCA Campaign Report"
	move	NO,RptCan
.	create	Report2;mRSearch,"&Search;&Campaign"
	create	Report2;StatTextBoxes(1)=50:70:10:110,"Campaign",""
	create	Report2;StatTextBoxes(2)=70:90:10:110,"Printer",""
	create	Report2;StatTextBoxes(5)=90:110:10:310,"Report Type",""
	create	Report2;StatTextBoxes(3)=130:150:10:310,"",""
	create	Report2;StatTextBoxes(4)=150:170:10:310,"","",fgcolor=red
	create	Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;ComboBoxes(1)=70:91:80:310,"",";P)rint to File;)Laser 3;).PDF File;)Laser 2"
	create	Report2;ComboBoxes(2)=90:111:80:310,"",";S)tandard;)TNC;)NWF//UNICEF"
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:190,"&Finish",enabled=0
.	activate mRSearch,Report2SearchGo,result
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate StatTextBoxes(5)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
	eventreg EditTextBoxes(1),10,OrderCampaignKeyPress,RESULT=N9
	activate EditTextBoxes(1),OrderSetCampaignEditChange,result
	activate ComboBoxes(1)
	activate ComboBoxes(2)
	activate Buttons(1),OrderSetCampaignOK,result
	activate Buttons(2),OrderSetCampaignFinish,result
	setfocus EditTextBoxes(1)
	return
.Report2SearchGo
.	goto SearchGo5
OrderSetCampaignEditChange
	if (result = C2)	.Lost Focus + Change
		setprop	Buttons(2),enabled=0
	endif
	return
OrderSetCampaignOK
	getitem	EditTextBoxes(1),0,str6
	call	Trim using str6
	if (str6 = "")
		setitem	StatTextBoxes(4),0,"Not	a valid	Campaign Number!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	else
		move	str6,NCMPFLD
		call	ZFILLIT	using NCMPFLD,C0
		setitem	StatTextBoxes(4),0,NCMPFLD
		move	C1,NCMPPATH
		move	"Order7Move-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if over
			setitem	StatTextBoxes(4),0,"Not	a valid	Campaign Number!!"
			setprop	Buttons(2),enabled=0
			setfocus EditTextBoxes(1)
		else
			pack	str25,"Campaign: ",NCMPNUM
			setitem	StatTextBoxes(3),0,str25
			setitem	StatTextBoxes(4),0,NCMPCNAME
			setprop	Buttons(2),enabled=1
			setfocus Buttons(2)
		endif
	endif
	return
OrderSetCampaignFinish
	setprop	Report2,visible=0
	return
OrderCampaignKeyPress
.	if (N9 = 113) .F2 Key calls Search Function
..Virtual Key Value
.		goto SearchGo5
.	elseif (N9 = 120)     .F9 Key closes Search Function
.		setprop	Search,visible=0
.	endif
	return
        
        include ncmpio.inc
	include	nordio.inc
	include	nuseio.inc
        include comlogic.inc

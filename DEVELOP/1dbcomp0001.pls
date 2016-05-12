	include	common.inc
	include	cons.inc
;patchconversion  3.0
.	include	mlrnbrkdd.inc
	include	compdd.inc
	include	cntdd.inc
;patchconversion	  3.0
	include	nowndd.inc
.	include	nmlrdd.inc
	include	nsmpdd.inc
	include	npasdd.inc
	include	npaydd.inc
	include	nusedd.inc
	include	ncntdd.inc
   include  nmtxdd.inc
   include  stabbdd.inc
	include	ndatdd.inc
	include	nxrfdd.inc
	include	nofrdd.inc
	include	winapi.inc
	include	gnxtdd.inc
	include	compnotesdd.inc
	INCLUDE	OSLSPERN.INC
.patch1.8
	include	norddd.inc
.endpatch1.8
Release	init		"3.0"	14MAY2004	DMB	CODE MODIFICATION FOR MAILER/COMPANY CONVERSION
;Release	init		"2.1"	12APR2004	DMB	ADDED CODE to Allow Credit Status Not Accepting Broker Guars
;Release	init		"2.0"	04APR2004	DMB	ADDED CODE to Allow LM modification of contacts
;Release	init		"1.9"	22DEC2003	DMB	ADDED CODE to Allow Modification of Notes without Verfing other fields
.Release	init		"1.8"	22DEC2003	DMB	ADDED CODE to Search Order File to create new (old file)broker contacts that do not have order history.
.Release	init		"1.7"	18NOV2003	DMB	ADDED CODE TO AUTOMATICALLY FILL IN LAST NAME BASED ON B1 ON CONTACT SCREEN (COMP1B)
.Release	init		"1.6"	08OCT2003	DMB	Corrected Code for Adding consultant contacts
.Release	init		"1.5"	29SEP2003	DMB	Added Logic to Search routine
.Release	init		"1.4"	25SEP2003	DMB	Allow Modifcation of Broker Notes by anyone per DH
.Release	init		"1.3"	24SEP2003	DMB	Fixed Bug that did not add correct cnctcnt to new broker contact.
.Release	init		"1.2"	22SEP2003	DMB	Minor Fixes and Enhancements
.Release	init		"1.0"	27SEP2002	ASH	INITIAL RELEASE
.Patch1.9 Determines whether or not user is allowed to save broker information w/o going thru verification routine.
BrkNoteFlag	DIM	1
.Endpatch1.9
;SampleViewer
.Items used by Sample Screen
Preview form    1
Default form    1
Select  form    1
dcxpath dim     35
SmpPage form    5
SmpPage2 form   5
SmpFile pfile
SmpScale form   3
;Sample
;
.Patch1.2
CREDIT	IFILE	KEYLEN=6
.Endpatch1.2
.EXTERNAL ROUTINES FROM	COMP001A.PLC
OrderDisplayMessage external "INFO;DisplayMessage"
OrderInfoClose external "INFO;InfoClose"
MailerLoadForm external "INFO;LoadForm"
COMPCOMPKEY external "COMP001A;COMPCOMPKEY"
CNCTCNCTKEY external "COMP001A;CNCTCNCTKEY"
;patch3.0
ComptoOldMlr1 external "Comp001A;CompToOldMlr"
;patch3.0
;Patch2.0
;External Routine to check for duplicate contact names
CNCTCNCTNameKEY external "COMP001A;CNCTCNCTNameKEY"
;Patch2.0
;patch1.04
COMPOLDBRKGNXTKEY External "COMP001B;COMPOLDBRKGNXTKEY"
;patch1.04
;patch1.13
LoadFloatMenu external "FLOATMENU;LoadFloatMenu"
;Called in Click_Comp2StatEmail,MouseDn_Comp2StatEmail
EmailFloatDisplay external "FLOATMENU;EmailFloatDisplay"
;Also Called in Change_CompTabControl001
FloatMenuClose	external	"FLOATMENU;FloatMenuClose"
;patch1.14
ParentCoFloatDisplay	External	"FLOATMENU;ParentCoFloatDisplay"
LoadSearchForm		External	"CompSearchForm;LoadSearchForm"
CompanyContactSearchMenu		External	"CompSearchForm;CompanyContactSearchMenu"
CCB	Form	1
COMPTYPE	FORM	1
CompSearchString	DIM	109
CSNUM	FORM	2
;patch1.2
SearchExitVar	DIM	1
;patch1.2
;patch1.14
;Vars to see position where clicked
MouseClick	FORM	9
FloatMenuItem	FORM	3
;patch1.13
.EXTERNAL ROUTINES FROM NPKG0001.PLC
MailerPackageLoadListViews external "NPKG0001;PackageLoadListViews"
MailerPackageDisableForm external "NPKG0001;PackageDisableForm"
MailerPackageEnableForm external "NPKG0001;PackageEnableForm"
MailerPackageOKClick external "NPKG0001;PackageOKClick"

NewFlag	init		"N"
NewFlag2 init		"N"
NewFlag4Category init	"N"
NewFlag4Offer init	"N"
ReturnFlag init		"N"
ReturnFlag2 init	"N"
ReturnFlag4Offer init	"N"
ReturnFlag4Category init "N"
ExitFlag init	"Y"
ExitFlag2 init	"Y"
.This flag is actually used for Screen 5 - NPKG0001.PLC, will need to update when this program goes live
ExitFlag4 dim	%1
	move	"Y",ExitFlag4
ExitFlag4A init	"Y"
ExitFlag4B init	"Y"
HoldFlag init	"N"
;Patch3.0  Offer Password Prompt
HoldFlag2 init	"N"
;patch3.0
hold	dim	500	.length of Company record
holda	dim	500	.length of Company record - used by independent routines to hold Company record
;patch1.1
;hold2	dim	300	.length of Contact record
hold2	dim	304	.length of Contact record
;patch1.07
holdcredit	DIM	2
;patch1.2
ConvCreditstr	INIT	"1 2*3B4N5P6W7M899G"
RevCreditstr	INIT	" 1*2B3N4P5W6M798G9"
BADCREDIT	INIT	"*BP9"
;patch1.2
;patch1.07
;patch1.1
;patch1.04
COMP0001OLDBRK	DIM	4
COMP0001OLDMLR	DIM	4
;patch1.04

;patch1.05
str27	DIM	27
;patch1.05
.
areacode dim	3
taskname2 dim	200
.
;
yesno1	integer	1,"0x000004"
LPAREN	INIT	"("
RPAREN	INIT	")"
bslash	INIT	"\"
COMPHOLD	DIM                 6
.COMPHOLD4	DIM                 6
COMPHOLD4	DIM                 6
;
TabNum	form	9
DimPtr	dim	^
FrmPtr	form	^
FrmPtr1	form	^
height	form	7.4
width	form	7.4
MaxHeight form	"610.0000"
.MaxHeight form	"733.0000"
.MaxHeight form	"804.0000"
MinHeight form	"438.0000"
MaxWidth form	"905.0000"
MinWidth form	"642.0000"
N74	form	7.4
VerTab	form	4
HorTab	form	4
.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
filler  init    "0000"
.hexeight integer 4,"4294967295"
.
VT_BOOL	EQU 11
OTRUE	variant
OFALSE	variant
OBOOL	variant
.
VScrollBar1 VScrollBar
HScrollBar1 HScrollBar
.Colors
white	color
grey	color
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
;patch1.0
msecurity menu
;patch1.0
mHelp    menu
.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&View Samples;Searc&h"
HData   init    "&Help;&About"
;patch1.01
SEData   init    "&Security;Administrator Mode"
revtyps init    "DH AH DM DB JD	"	     ALLOWED REVISION ?
SECFLAG init    "N"
;patch1.01
;patch1.07 Security for Credit
CreditFlag	init	"N"
;patch1.07
Timer   Timer
;patch1.06
CRLF	INTEGER	1,"0x07F"
;patch1.06
BMAINTFLAG	INIT	"N"
OLDMLR	DIM	4
.Set Vars used for About Box
	move    "COMP0001.PLS",Wprognme
	move    "Company File Maintenance",Wfunction
	move    "Andrew Harkins",Wauthor
	move    Release,Wrelease
	move    "September 27, 2002",Wreldate
.
;CompSrch	plform	CompSearchForm
pass	plform  Passwrd
abt	plform	About
rpt	plform	Report
smp	plform	NORD001F
COMP001A plform  COMP001A
x	plform  COMP0001
COMP001B plform  COMP001B
COMP001C plform  COMP001C
COMP001D plform  COMP001D
	winhide
.Load Forms, Always load parent form first
	formload x
	formload COMP001A,COMP0001
	formload COMP001B,COMP0001
	formload COMP001C,COMP0001
	formload COMP001D,COMP0001
	formload pass
	formload abt
	formload rpt
	formload smp
;	formload CompSrch
;patch1.13
	CALL	LoadFloatMenu
;patch1.14
	CALL	LoadSearchForm
;patch1.14
;patch1.13
	call	MailerLoadForm
	setmode	*GRAYSCALE=0
	pack	dcxpath,NTWKPATH1,"\samples\"
;Open CompCredit
	OPEN	CREDIT,"COMPCREDIT"



.Prep some stuff for the Sample Viewing form
.	pack	dcxpath,NTWKPATH1,"\samples\"
.
.Set up ScrollBar linking.  There is no way to access information from a ScrollBar embedded in a form, so we
.must declare another, link it, then register the Change event, and then determine values from this phantom object.
	getprop	COMP0001,LINKVSCROLL=VScrollBar1,LINKHSCROLL=HScrollBar1
	eventreg VScrollBar1,C3,VScrollChange
	eventreg HScrollBar1,C3,HScrollChange
	setprop	VScrollBar1,Min=0,Max=20,Shift=10
	setprop	HScrollBar1,Min=0,Max=20,Shift=10
.
	call	MailerPackageLoadListViews using COMP0001,C5
.
	CompListViewSearch.InsertColumn using "Key",0,1
	CompListViewSearch.InsertColumn using "Number",50,2
	CompListViewSearch.InsertColumn using "Name",280,3
	CompListViewSearch.InsertColumn using "Details",0,4
.
	Comp1ListViewCnt.InsertColumn using "Key",0,1
	Comp1ListViewCnt.InsertColumn using "Contact",225,2
	Comp1ListViewCnt.InsertColumn using "Number",50,3
	Comp1ListViewCnt.InsertColumn using "Type",100,3
	Comp1ListViewCnt.SetColumnFormat using 2,1
.
	Comp2ListView.InsertColumn using "Key",0,1
	Comp2ListView.InsertColumn using "Company",75,2
	Comp2ListView.InsertColumn using "Contact",50,3
	Comp2ListView.InsertColumn using "Type",75,4
	Comp2ListView.InsertColumn using "Name",280,4
	Comp2ListView.InsertColumn using "Other Detail",0,5
	Comp2ListView.SetColumnFormat using 1,1
	Comp2ListView.SetColumnFormat using 2,1
.
	Comp4ListViewOffer.InsertColumn using "Offer",75,1
	Comp4ListViewOffer.InsertColumn using "Description",300,2
	Comp4ListViewOffer.InsertColumn using "Other Detail",0,3
	Comp4ListViewOffer.SetColumnFormat using 0,1
.
	Comp4ListViewCategory.InsertColumn using "Category",75,1
	Comp4ListViewCategory.InsertColumn using "Description",300,2
	Comp4ListViewCategory.InsertColumn using "Other Detail",0,3
	Comp4ListViewCategory.SetColumnFormat using 0,1
.
	Comp4ListViewList.InsertColumn using "List",75,1
	Comp4ListViewList.InsertColumn using "List Name",270,2
	Comp4ListViewList.SetColumnFormat using 0,1
.
	Comp4ListViewSample.InsertColumn using "Sample",75,1
	Comp4ListViewSample.InsertColumn using "Description",270,2
	Comp4ListViewSample.SetColumnFormat using 0,1
.Load Contact &	Caller ComboBox
	move	C1,N2
	move	"  ",str45
	deleteitem Comp1ComboContact,0
;patch1.08
	for n2,"1","23"
		load	str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
			osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
			osls17,osls18,osls19,osls20,osls21,osls22
			move	n2 to str2
		rep	zfill,str2
		pack	str27,str25,str2
;		call	trim using str27
		insertitem Comp1ComboContact,9999,str27
;	insertitem Comp1ComboContact,N2,str45
;	loop
;		move	C1,NCNTPATH
;		move	"Load-NCNTSEQ",Location
;		call	NCNTSEQ
;		until over
;		if (CNTCNT = "1")
;			pack	str45,CNTNAME,B1,CNTNUM
;			add	C1,N2
;			insertitem Comp1ComboContact,N2,str45
;		endif
	repeat
;patch1.08
	setitem	Comp1ComboContact,0,0
.Clear WebBrowser ComboBox
	deleteitem Comp3ComboAddress,0
.
.Create	Colors for EditText Inquiry
	create	white=*white
	create	grey=220:220:220
.
	create	TIMER,18000     .30 minutes
	activate TIMER,Timeout,RESULT
.Create Menu Items
        create  COMP0001;mFile,FData
        create  COMP0001;mEdit,EData,mFile
        create  COMP0001;mOptions,OData,mEdit

;patch1.01
        reset   revtyps
;	scan	INITS,revtyps
;	if equal
        create	COMP0001;mSecurity,SEData,mOptions
        create	COMP0001;mHelp,HData,mSecurity
;else
;	create	COMP0001;mHelp,HData,mOptions
;	endif
;        create  COMP0001;mHelp,HData,mOptions
;patch1.01

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mEdit,EditGo,result
        activate mOptions,OptionsGo,result
;patch1.01
        activate mSecurity,SecurityGo,result
;patch1.01
        activate mHelp,HelpGo,result
.
	call	Comp1DisableLower
	call	Comp2DisableLower
	call	Comp4DisableLower
.
	move	C3,COMPLOCK
.
	Comp3WebBrowser.Navigate2 USING "about:blank"
	eventreg Comp3WebBrowser,"252",WebTest,FastEvent
 	eventreg Comp3WebBrowser,"259",WebTest2,FastEvent
.Set some properties for Browser object
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	OBOOL,VarType=VT_BOOL,VarValue=0
.
	move	"D",progcode
.
        trap    StateFileError if IO
        open    NSTFILE,NSTNAME
        trapclr IO
.
;patch1.05
	deleteitem Comp2ComboSalesPerson,c0
	for n2,"1","23"
		load	str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
			osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
			osls17,osls18,osls19,osls20,osls21,osls22
			move	n2 to str2
		rep	zfill,str2
		pack	str27,str25,str2
;		call	trim using str27
			insertitem Comp2ComboSalesPerson,9999,str27
	repeat
;patch1.05
;break
	setfocus CompSearchList
	loop
		waitevent
		setitem timer,0,18000   .reset to 30 minutes
	repeat

WebTest
	clear	taskname
	getprop	*Arg2,VarValue=taskname
	for result,C1,C10
		getitem	Comp3ComboAddress,result,taskname2
		scan	taskname2,taskname
		if equal
			setitem	Comp3ComboAddress,0,result
			break
		endif
	repeat
	reset	taskname
	return
WebTest2
.	clear	taskname
.	getprop	*Arg2,VarValue=taskname
	return

Timeout
	if (ExitFlag = NO)
		call	Click_CompQuit
	endif
	if (ExitFlag2 = NO)
.		call	Click_Comp2Quit
	endif
	if (ExitFlag4A = NO)
		call	Click_Comp4QuitOffer
	endif
	if (ExitFlag4B = NO)
		call	Click_Comp4QuitCategory
	endif
        beep
        beep
        beep
        winshow
        stop

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1
FileGo1
	if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")
		winshow
		stop
	endif
	RETURN
OptionsGo
	branch result to OptionsGo1,OptionsGo2
OptionsGo1
	Comp4ListViewSample.GetItemCount giving result
	if (result > 0)
		getitem	Comp1EditOldMailer,0,str6
		setitem	SamplesEditMailer,0,str6
		Comp4ListViewSample.GetNextItem giving N9 using C2
		Comp4ListViewSample.GetItemText giving str3 using N9,0
		setitem	SamplesEditSample,0,str3
		Activate NORD001F
		if (str6 <> "" AND str3 <> "")
			goto Click_SamplesOK
		endif
	else
		alert note,"There are no samples to view",result,"No Samples"
	endif
	return
OptionsGo2
	Getitem	CompTabControl001,0,n2
	add	c1 to n2
;patch1.14
		retcount CSNUM
		ADD c1 to CSNUM
		CALL	CompanyContactSearchMenu	USING	CCB,COMPTYPE,CompSearchString,CSNUM,SearchExitVar
.patch1.2
		if (SearchExitVar <>	YES)
			CALL	CompSearchLoad
		Endif
.patch1.2
;patch1.14
;	Setitem	CompSearchComboSearch1,0,n2
;	Setitem	CompSearchComboType,0,c3
;	Call	CompSearchVisible
;	setprop	CompSearchForm,visible=c1

	Return
EditGo
	Return
SecurityGo
	branch	result to SecurityGo1
SecurityGo1
;patch1.07
	move	"(",progcode
;patch1.07
	move	NO,SecFlag
	pack	str55,"	          To Enter Administrator mode"
	setitem	PasswordStatMssg1,0,str55
	setprop	PasswordStatMssg1,visible=1
	setitem	PasswordEdit,0,""
	setfocus	PasswordEdit
	clear	NPASFLD
	setprop	Passwrd,visible=1
	if (NPASFLD <> "(COSMO")
		return
	endif
	move	YES to SECFLAG
;patch1.07
	move	YES to CREDITFLAG
;patch1.07
	return
HelpGo
	setprop AboutMssg,visible=1
	return

CompEnableUpper
	move	NO,NewFlag
;patch1.02
;	setprop	CompOK,enabled=1
;patch1.02
	setprop	CompNew,enabled=1
.patch3.0
.	setprop	CompListViewSearch,enabled=1
.patch3.0
	setprop	CompExit,enabled=1
	move	YES,ExitFlag
;patch1.02
	call	EnableOK
;patch1.02
	return

CompEnableUpperButtons
	setprop	CompSave,enabled=1
	setprop	CompQuit,enabled=1
	return

CompDisableUpper
	move	NO,ExitFlag
	setprop	CompExit,enabled=0
	setprop	CompOK,enabled=0
	setprop	CompNew,enabled=0
	setprop	CompModify,enabled=0
	setprop	CompListViewSearch,enabled=0
	setprop	CompPrint,enabled=0
	setprop	CompSearch,enabled=0
;Patch1.4
	return
;EndPatch1.4
CompDisableUpperButtons
	setprop	CompSave,enabled=0
	setprop	CompQuit,enabled=0
	return

Comp1EnableLower
	setprop	Comp1CheckBroker,enabled=1
	setprop	Comp1CheckConsultant,enabled=1
;patch3.0
	getitem	Comp1CheckMailer,0,N1                 ;Mailer?
	if (N1 = C1)
		call Comp1EnableLowerBroker
	else
		getitem	Comp1CheckConsultant,0,N1                 ;Mailer?
		if (N1 = C1)
			call Comp1EnableLowerBroker
		endif
	endif
;patch3.0
	setprop	Comp1CheckInactive,enabled=1
	setprop	Comp1CheckReset,enabled=1
	setprop	Comp1ComboContact,enabled=1,bgcolor=white
	setprop	Comp1ComboStatus,enabled=1,bgcolor=white
	setprop	Comp1EditAcctFax,enabled=1,bgcolor=white
	setprop	Comp1EditAddress1,enabled=1,bgcolor=white
	setprop	Comp1EditAddress2,enabled=1,bgcolor=white
	setprop	Comp1EditCity,enabled=1,bgcolor=white
	setprop	Comp1EditCountry,enabled=1,bgcolor=white
	setprop	Comp1EditCountryCode,enabled=1,bgcolor=white
	setprop	Comp1EditEmail,enabled=1,bgcolor=white
	setprop	Comp1EditFax,enabled=1,bgcolor=white
	setprop	Comp1EditName,enabled=1,bgcolor=white
	setprop	Comp1EditNotes,enabled=1,bgcolor=white,readonly=0
	setprop	Comp1EditParent,enabled=1,bgcolor=white
	setprop	Comp1EditPhone,enabled=1,bgcolor=white
	setprop	Comp1EditState,enabled=1,bgcolor=white
	setprop	Comp1EditWebSite,enabled=1,bgcolor=white
	setprop	Comp1EditZip,enabled=1,bgcolor=white

;;;patch3.0
		getitem	Comp1CheckMailer,0,N1                 ;Mailer?
		if (N1 = C1)
			call Comp1EnableLowerMailer
		endif
		setprop	Comp1CheckMailer,enabled=1
;;;patch3.0


	if	(SECFLAG = YES)
		setprop	Comp1CheckDiscount,enabled=1
		setprop	Comp1ComboAccount,enabled=1,bgcolor=white
		setprop	Comp1ComboBillCode,enabled=1,bgcolor=white
		setprop	Comp1CheckGalley,enabled=1
		setprop	Comp1CheckManager,enabled=1
		setprop	Comp1CheckOwner,enabled=1
		setprop	Comp1CheckServiceB,enabled=1
		setprop	Comp1ComboBrokerRank,enabled=1,bgcolor=white
		setprop	Comp1EditManager,enabled=1,bgcolor=white
		setprop	Comp1EditManagerCnt,enabled=1,bgcolor=white
;		setprop	Comp1EditNumber,enabled=1,bgcolor=white
		setprop	Comp1EditOldBroker,enabled=1,bgcolor=white
		setprop	Comp1EditOldMailer,enabled=1,bgcolor=white
		setprop	Comp1EditOldOwner,enabled=1,bgcolor=white
		setprop	Comp1EditOldServiceB,enabled=1,bgcolor=white
		setprop	Comp1EditTaxID,enabled=1,bgcolor=white
		setprop	Comp1EditServiceB,enabled=1,bgcolor=white
		setprop	Comp1EditServiceBCnt,enabled=1,bgcolor=white
		call	Comp1EnableLowerMailer
		call	Comp1EnableLowerBroker
	endif
	return

Comp1DisableLower
.	setprop	Comp1CheckBillDirect,enabled=0
	setprop	Comp1CheckBroker,enabled=0
	setprop	Comp1CheckConsultant,enabled=0
.	setprop	Comp1CheckDiscount,enabled=0
	setprop	Comp1CheckExchange,enabled=0
.	setprop	Comp1CheckFaxBroker,enabled=0
.	setprop	Comp1CheckFaxMailer,enabled=0
.	setprop	Comp1CheckForProfit,enabled=0
	setprop	Comp1CheckGalley,enabled=0
.	setprop	Comp1CheckGuarantees,enabled=0
	setprop	Comp1CheckInactive,enabled=0
	setprop	Comp1CheckMailer,enabled=0
	setprop	Comp1CheckManager,enabled=0
	setprop	Comp1CheckOwner,enabled=0
.	setprop	Comp1CheckRegional,enabled=0
	setprop	Comp1CheckReset,enabled=0
	setprop	Comp1CheckServiceB,enabled=0
.PATCH1.8 NEW STATEMENTS CHECK BOX
.	setprop	Comp1CheckStatements,enabled=0
.PATCH1.8
.	setprop	Comp1CheckUsage,enabled=0
.	setprop	Comp1ComboAccount,enabled=0,bgcolor=grey
.	setprop	Comp1ComboBillCode,enabled=0,bgcolor=grey
	setprop	Comp1ComboBrokerRank,enabled=0,bgcolor=grey
	setprop	Comp1ComboContact,enabled=0,bgcolor=grey
.	setprop	Comp1ComboExempt,enabled=0,bgcolor=grey
	setprop	Comp1ComboStatus,enabled=0,bgcolor=grey
.	setprop	Comp1Edit501C,enabled=0,bgcolor=grey
.	setprop	Comp1EditAccountYear,enabled=0,bgcolor=grey
	setprop	Comp1EditAcctFax,enabled=0,bgcolor=grey
	setprop	Comp1EditAddress1,enabled=0,bgcolor=grey
	setprop	Comp1EditAddress2,enabled=0,bgcolor=grey
.	setprop	Comp1EditBroker,enabled=0,bgcolor=grey
.	setprop	Comp1EditBrokerCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditCity,enabled=0,bgcolor=grey
.	setprop	Comp1EditConsultant,enabled=0,bgcolor=grey
.	setprop	Comp1EditConsultantCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditCountry,enabled=0,bgcolor=grey
	setprop	Comp1EditCountryCode,enabled=0,bgcolor=grey
	setprop	Comp1EditEmail,enabled=0,bgcolor=grey
	setprop	Comp1EditFax,enabled=0,bgcolor=grey
.	setprop	Comp1EditInvoice,enabled=0,bgcolor=grey
.	setprop	Comp1EditInvoiceCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditManager,enabled=0,bgcolor=grey
	setprop	Comp1EditManagerCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditName,enabled=0,bgcolor=grey
	setprop	Comp1EditNotes,enabled=0,bgcolor=grey,readonly=1
	setprop	Comp1EditNumber,enabled=0,bgcolor=grey
	setprop	Comp1EditOldBroker,enabled=0,bgcolor=grey
	setprop	Comp1EditOldMailer,enabled=0,bgcolor=grey
	setprop	Comp1EditOldOwner,enabled=0,bgcolor=grey
	setprop	Comp1EditOldServiceB,enabled=0,bgcolor=grey
	setprop	Comp1EditParent,enabled=0,bgcolor=grey
.	setprop	Comp1EditPermit,enabled=0,bgcolor=grey
	setprop	Comp1EditPhone,enabled=0,bgcolor=grey
	setprop	Comp1EditServiceB,enabled=0,bgcolor=grey
	setprop	Comp1EditServiceBCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditState,enabled=0,bgcolor=grey
	setprop	Comp1EditTaxID,enabled=0,bgcolor=grey
	setprop	Comp1EditWebSite,enabled=0,bgcolor=grey
	setprop	Comp1EditZip,enabled=0,bgcolor=grey
	Call	Comp1DisableLowerMailer
	Call	Comp1DisableLowerBroker
	return

Comp1ClearScreen
	setitem	CompStatNumber,0,""
.Screen1
	setitem	Comp1CheckBillDirect,0,0
	setitem	Comp1CheckBroker,0,0
	setitem	Comp1CheckConsultant,0,0
	setitem	Comp1CheckDiscount,0,0
	setitem	Comp1CheckExchange,0,0
	setitem	Comp1CheckFaxBroker,0,0
	setitem	Comp1CheckFaxMailer,0,0
	setitem	Comp1CheckForProfit,0,0
	setitem	Comp1CheckGalley,0,0
	setitem	Comp1CheckGuarantees,0,0
	setitem	Comp1CheckInactive,0,0
	setitem	Comp1CheckMailer,0,0
	setitem	Comp1CheckManager,0,0
	setitem	Comp1CheckOwner,0,0
	setitem	Comp1CheckRegional,0,0
	setitem	Comp1CheckReset,0,0
	setitem	Comp1CheckServiceB,0,0
.PATCH1.8 NEW STATEMENTS CHECK BOX
	setitem	Comp1CheckStatements,0,0
.PATCH1.8
	setitem	Comp1CheckUsage,0,0
	setitem	Comp1ComboAccount,0,1
	setitem	Comp1ComboBillCode,0,1
	setitem	Comp1ComboBrokerRank,0,1
	setitem	Comp1ComboContact,0,0
	setitem	Comp1ComboExempt,0,1
	setitem	Comp1ComboStatus,0,1
	setitem	Comp1Edit501C,0,""
	setitem	Comp1EditAccountYear,0,""
	setitem	Comp1EditAcctFax,0,""
	setitem	Comp1EditAddress1,0,""
	setitem	Comp1EditAddress2,0,""
	setitem	Comp1EditBroker,0,""
	setitem	Comp1EditBrokerCnt,0,""
	setitem	Comp1EditCity,0,""
	setitem	Comp1EditConsultant,0,""
	setitem	Comp1EditConsultantCnt,0,""
	setitem	Comp1EditCountry,0,""
	setitem	Comp1EditCountryCode,0,""
	setitem	Comp1EditEmail,0,""
	setitem	Comp1EditFax,0,""
	setitem	Comp1EditInvoice,0,""
	setitem	Comp1EditInvoiceCnt,0,""
	setitem	Comp1EditManager,0,""
	setitem	Comp1EditManagerCnt,0,""
	setitem	Comp1EditName,0,""
	setitem	Comp1EditNotes,0,""
	setitem	Comp1EditNumber,0,""
	setitem	Comp1EditOldBroker,0,""
	setitem	Comp1EditOldMailer,0,""
	setitem	Comp1EditOldOwner,0,""
	setitem	Comp1EditOldServiceB,0,""
	setitem	Comp1EditParent,0,""
	setitem	Comp1EditPermit,0,""
	setitem	Comp1EditPhone,0,""
	setitem	Comp1EditServiceB,0,""
	setitem	Comp1EditServiceBCnt,0,""
	setitem	Comp1EditState,0,""
	setitem	Comp1EditTaxID,0,""
	setitem	Comp1EditWebSite,0,""
	setitem	Comp1EditZip,0,""
	setitem	Comp1StatBrokerName,0,""
	setitem	Comp1StatConsultantName,0,""
	setitem	Comp1StatCreate,0,""
	setitem	Comp1StatInvoiceName,0,""
	setitem	Comp1StatManagerName,0,""
	setitem	Comp1StatParentName,0,""
	setitem	Comp1StatRevision,0,""
	setitem	Comp1StatServiceBName,0,""
	setitem	Comp1StatStateName,0,""
	Comp1ListViewCnt.DeleteAllItems giving N9
	setitem	Comp1StatCnt,0,""
	return

.Screen 2
Comp2DisableUpper
	setprop	Comp2Modify,enabled=0
	setprop	Comp2New,enabled=0
;Patch1.02
	setprop	CompOK,enabled=0
	setprop	Comp2ListView,enabled=0
;Patch3.0
	setprop CompListViewSearch,enabled=0
;patch3.0
	move	NO,ExitFlag2
;Patch1.02
	return
Comp2DisableUpper2
	setprop	Comp2Quit,enabled=0
	setprop	Comp2Save,enabled=0
	return
Comp2EnableUpper
;Patch1.02
	move	NO,NewFlag2
;	if	(ExitFlag  = YES)
;		setprop	CompOK,enabled=1
;	endif
;Patch1.02
	setprop	Comp2ListView,enabled=1
	move	YES,ExitFlag2
;Patch1.02
	call	EnableOK
;Patch1.02
	setprop	Comp2Modify,enabled=1
	setprop	Comp2New,enabled=1
	return
Comp2EnableUpper2
	setprop	Comp2Quit,enabled=1
	setprop	Comp2Save,enabled=1
	return

Comp2DisableLower
	setprop	Comp2CheckBroker,enabled=0
	setprop	Comp2CheckClient,enabled=0
	setprop	Comp2CheckDMA,enabled=0
	setprop	Comp2CheckDatacard,enabled=0
	setprop	Comp2CheckHidden,enabled=0
	setprop	Comp2CheckHoliday,enabled=0
	setprop	Comp2CheckInactive,enabled=0
	setprop	Comp2CheckNewsletter,enabled=0
	setprop	Comp2CheckParty,enabled=0
	setprop	Comp2CheckPromo,enabled=0
	setprop	Comp2CheckWeekly,enabled=0
	setprop	Comp2ComboType,enabled=0,bgcolor=grey
	setprop	Comp2EditComp,enabled=0,bgcolor=grey
	setprop	Comp2EditCountryCode,enabled=0,bgcolor=grey
	setprop	Comp2EditEmail,enabled=0,bgcolor=grey
	setprop	Comp2EditFName,enabled=0,bgcolor=grey
	setprop	Comp2EditFax,enabled=0,bgcolor=grey
	setprop	Comp2EditLName,enabled=0,bgcolor=grey
	setprop	Comp2EditNumber,enabled=0,bgcolor=grey
;patch1.1
	setprop	Comp2EditOldComp,enabled=0,bgcolor=grey
	setprop	Comp2EditOldNumber,enabled=0,bgcolor=grey
;patch1.1
	setprop	Comp2EditPhone,enabled=0,bgcolor=grey
	setprop	Comp2EditPhone2,enabled=0,bgcolor=grey
	setprop	Comp2ComboSalesPerson,enabled=0,bgcolor=grey
	setprop	Comp2EditSalutation,enabled=0,bgcolor=grey
	setprop	Comp2EditTitle,enabled=0,bgcolor=grey
	return
Comp2EnableLower
	setprop	Comp2CheckBroker,enabled=1
	setprop	Comp2CheckClient,enabled=1
	setprop	Comp2CheckDMA,enabled=1
	setprop	Comp2CheckDatacard,enabled=1
	setprop	Comp2CheckHidden,enabled=1
	setprop	Comp2CheckHoliday,enabled=1
	setprop	Comp2CheckInactive,enabled=1
	setprop	Comp2CheckNewsletter,enabled=1
	setprop	Comp2CheckParty,enabled=1
	setprop	Comp2CheckPromo,enabled=1
	setprop	Comp2CheckWeekly,enabled=1

;
	setprop	Comp2EditCountryCode,enabled=1,bgcolor=white
	setprop	Comp2EditEmail,enabled=1,bgcolor=white
;patch1.02&1.03
	if	(NEWFLAG2 = YES)
		setprop	Comp2EditFName,enabled=1,bgcolor=white
		setprop	Comp2EditLName,enabled=1,bgcolor=white
		setprop	Comp2ComboType,enabled=1,bgcolor=white
	elseif	(SECFLAG = YES)
		setprop	Comp2EditFName,enabled=1,bgcolor=white
		setprop	Comp2EditLName,enabled=1,bgcolor=white
		setprop	Comp2ComboType,enabled=1,bgcolor=white
		setprop	Comp2EditOldComp,enabled=1,bgcolor=white
		setprop	Comp2EditOldNumber,enabled=1,bgcolor=white
;Patch3.0
.	elseif (NEWFLAG = NO & N1 = "1")
.		setprop	Comp2EditFName,enabled=1,bgcolor=white
.		setprop	Comp2EditLName,enabled=1,bgcolor=white
.Patch3.0
	endif
;patch1.02&1.03
	setprop	Comp2EditFax,enabled=1,bgcolor=white
	setprop	Comp2EditPhone,enabled=1,bgcolor=white
	setprop	Comp2EditPhone2,enabled=1,bgcolor=white
	setprop	Comp2ComboSalesPerson,enabled=1,bgcolor=white
	setprop	Comp2EditSalutation,enabled=1,bgcolor=white
	setprop	Comp2EditTitle,enabled=1,bgcolor=white
;patch1.03
;	if	(SECFLAG = YES)
;		setprop	Comp2EditComp,enabled=1,bgcolor=white
;		setprop	Comp2EditNumber,enabled=1,bgcolor=white
;	endif
;patch1.03
	return
Comp2ClearScreen
	setitem	Comp2CheckBroker,0,0
	setitem	Comp2CheckClient,0,0
	setitem	Comp2CheckDMA,0,0
	setitem	Comp2CheckDatacard,0,0
	setitem	Comp2CheckHidden,0,0
	setitem	Comp2CheckHoliday,0,0
	setitem	Comp2CheckInactive,0,0
	setitem	Comp2CheckNewsletter,0,0
	setitem	Comp2CheckParty,0,0
	setitem	Comp2CheckPromo,0,0
	setitem	Comp2CheckWeekly,0,0
	setitem	Comp2ComboType,0,1
	setitem	Comp2EditComp,0,""
	setitem	Comp2EditCountryCode,0,""
	setitem	Comp2EditEmail,0,""
	setitem	Comp2EditFName,0,""
	setitem	Comp2EditFax,0,""
	setitem	Comp2EditLName,0,""
	setitem	Comp2EditNumber,0,""
	setitem	Comp2EditOldComp,0,""
	setitem	Comp2EditOldNumber,0,""
	setitem	Comp2EditPhone,0,""
	setitem	Comp2EditPhone2,0,""
	setitem	Comp2ComboSalesPerson,0,0
	setitem	Comp2EditSalutation,0,""
	setitem	Comp2EditTitle,0,""
	setitem	Comp2StatCreate,0,""
	setitem	Comp2StatRevision,0,""
;patch1.03 may remove for new comp button
;may remove
	if (NewFlag = YES)
		Comp2ListView.DeleteAllItems giving N9
	endif
;may remove
;patch1.03 may remove
	return

.Screen 4
Comp4DisableUpper
	call	Comp4DisableUpperCategory
	call	Comp4DisableUpperOffer
	return

Comp4DisableUpperCategory
	setprop	Comp4NewCategory,enabled=0
	setprop	Comp4ModifyCategory,enabled=0
	return
Comp4DisableUpperOffer
	setprop	Comp4NewOffer,enabled=0
	setprop	Comp4ModifyOffer,enabled=0
	setprop	Comp4ListViewOffer,enabled=0
;Patch3.0
	setprop CompListViewSearch,enabled=0
	setprop CompOK,enabled=0
;Patch3.0

	return

Comp4EnableUpper
	call	Comp4EnableUpperCategory
	call	Comp4EnableUpperOffer
	return

Comp4EnableUpperCategory
	setprop	Comp4ModifyCategory,enabled=1
	setprop	Comp4NewCategory,enabled=1
	move	YES,ExitFlag4B
	return
Comp4EnableUpperOffer
	setprop	Comp4ModifyOffer,enabled=1
	setprop	Comp4NewOffer,enabled=1
	setprop	Comp4ListViewOffer,enabled=1
	move	YES,ExitFlag4A
	move	NO,NewFlag4Offer
;Patch3.0
	call	EnableOK
;Patch3.0

	return

Comp4DisableUpper2
	call	Comp4DisableUpperCategory2
	call	Comp4DisableUpperOffer2
	return

Comp4DisableUpperCategory2
	setprop	Comp4SaveCategory,enabled=0
	setprop	Comp4QuitCategory,enabled=0
	return
Comp4DisableUpperOffer2
	setprop	Comp4SaveOffer,enabled=0
	setprop	Comp4QuitOffer,enabled=0
	return

Comp4EnableUpper2
	call	Comp4EnableUpperCategory2
	call	Comp4EnableUpperOffer2
	return

Comp4EnableUpperCategory2
	setprop	Comp4SaveCategory,enabled=1
	setprop	Comp4QuitCategory,enabled=1
	return
Comp4EnableUpperOffer2
	setprop	Comp4SaveOffer,enabled=1
	setprop	Comp4QuitOffer,enabled=1
	return

Comp4DisableLower
	call	Comp4DisableLowerCategory
	call	Comp4DisableLowerOffer
	return
Comp4DisableLowerCategory
	move	NO,NewFlag4Category
	setprop	Comp4EditCompCategory,enabled=0,bgcolor=grey
	setprop	Comp4EditNumberCategory,enabled=0,bgcolor=grey
	setprop	Comp4EditCategoryCategory,enabled=0,bgcolor=grey
	setprop	Comp4EditCategoryNameCategory,enabled=0,bgcolor=grey
	return

Comp4DisableLowerOffer
.	move	NO,NewFlag4Offer
	setprop	Comp4EditCompOffer,enabled=0,bgcolor=grey
	setprop	Comp4EditNumberOffer,enabled=0,bgcolor=grey
	setprop	Comp4EditOfferNameOffer,enabled=0,bgcolor=grey
	setprop	Comp4EditOfferOffer,enabled=0,bgcolor=grey
	return

Comp4EnableLower
	call	Comp4EnableLowerCategory
	call	Comp4EnableLowerOffer
	return

Comp4EnableLowerCategory
	move	NO,ExitFlag4B
	setprop	Comp4EditCompCategory,enabled=1,bgcolor=white
	setprop	Comp4EditNumberCategory,enabled=1,bgcolor=white
	setprop	Comp4EditCategoryCategory,enabled=1,bgcolor=white
	setprop	Comp4EditCategoryNameCategory,enabled=1,bgcolor=white
	return

Comp4EnableLowerOffer
	move	NO,ExitFlag4A
.	setprop	Comp4EditCompOffer,enabled=1,bgcolor=white
.	setprop	Comp4EditNumberOffer,enabled=1,bgcolor=white
	setprop	Comp4EditOfferNameOffer,enabled=1,bgcolor=white
.	setprop	Comp4EditOfferOffer,enabled=1,bgcolor=white
	return

Comp4ClearScreen
	call	Comp4ClearCategory
	call	Comp4ClearOffer
	return

Comp4ClearCategory
	setitem	Comp4EditCompCategory,0,""
	setitem	Comp4EditNumberCategory,0,""
	setitem	Comp4EditCategoryCategory,0,""
	setitem	Comp4EditCategoryNameCategory,0,""
	setitem	Comp4StatRevisionCategory,0,""
	return

Comp4ClearOffer
	setitem	Comp4EditCompOffer,0,""
	setitem	Comp4EditNumberOffer,0,""
	setitem	Comp4EditOfferNameOffer,0,""
	setitem	Comp4EditOfferOffer,0,""
	setitem	Comp4StatRevisedOffer,0,""
	return

CompLoadListView
	pack	hold,COMPVARS
	CompListViewSearch.InsertItem giving N9 using COMPCOMP
	CompListViewSearch.SetItemText using N9,COMPNUM,1
	CompListViewSearch.SetItemText using N9,COMPCOMP,2
	CompListViewSearch.SetItemText using N9,hold,3
	return

CompLoadScreen
	setitem	CompStatNumber,0,COMPNUM
.SCREEN 1
	setitem	Comp1EditNumber,0,COMPNUM
.
	move	C1,N2		.Default - Credit Okay
	if (COMPCREDIT = "*")		;"*" = ON HOLD.
		move	"2",N2
;	elseif (COMPCREDIT = "I")	;"I" = INACTIVE,	;not used db091803
;		move	"3",N2
	elseif (COMPCREDIT = "B")	;"B" = CREDIT RISK.  -      reset nightly if released
		move	"3",N2
	elseif (COMPCREDIT = "N")	;"N" =   NEW MAILER.
		move	"4",N2
	elseif (COMPCREDIT = "P")	;"P" = POLITICAL MAILER.  - reset nightly if released
		move	"5",N2
	elseif (COMPCREDIT = "W")	;"W" = Warning - read note
		move	"6",N2
	elseif (COMPCREDIT = "M")	;"M" = Must Prepay
		move	"7",N2
	elseif (COMPCREDIT = "9")	;"9" = On hold until over 90s paid
		move	"8",N2
	elseif (COMPCREDIT = "G")	;"G" = Guarantees are always required
		move	"9",N2
	elseif (COMPCREDIT = "g")	;"G" = Guarantees not Accepted
		move	"10",N2
	endif
	setitem	Comp1ComboStatus,0,N2
.
	setitem	Comp1EditName,0,COMPCOMP
.
	setitem	Comp1EditAddress1,0,COMPADDR
.
	setitem	Comp1EditAddress2,0,COMPADDR2
.
	setitem	Comp1EditCity,0,COMPCITY
.
	setitem	Comp1EditState,0,COMPSTATE
.
        call    trim using COMPSTATE
        count   N2,COMPSTATE
        if (N2 = "0")
                move    "",STNAME
        else
                pack    NSTFLD,COMPSTATE
                trap    StateFileError if IO
                read    NSTFILE,NSTFLD;STVARS
                trapclr IO
                if over
                        move    "Unknown State",STNAME
                endif
        endif
	setitem	Comp1StatStateName,0,STNAME
.
	setitem	Comp1EditZip,0,COMPZIP
.
	setitem	Comp1EditCountry,0,COMPCNTRY
.
	setitem	Comp1EditCountryCode,0,COMPCNTRYCDE
.
	count	result,COMPPHONE
	if (result = 10)
		unpack	COMPPHONE,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
	elseif (result = 0)
		clear	str15
	else
		unpack	COMPPHONE,str3,str4
		pack	str15,str3,DASH,str4
	endif
	setitem	Comp1EditPhone,0,str15
.
	count	result,COMPFAX
	if (result = 10)
		unpack	COMPFAX,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
	elseif (result = 0)
		clear	str15
	else
		unpack	COMPFAX,str3,str4
		pack	str15,str3,DASH,str4
	endif
	setitem	Comp1EditFax,0,str15
.
	count	result,COMPACCTFAX
	if (result = 10)
		unpack	COMPACCTFAX,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
	elseif (result = 0)
		clear	str15
	else
		unpack	COMPACCTFAX,str3,str4
		pack	str15,str3,DASH,str4
	endif
	setitem	Comp1EditAcctFax,0,str15
.
	setitem	Comp1EditEmail,0,COMPEMAIL
.
	setitem	Comp1EditWebSite,0,COMPFTP
.
	move	C2,N2		.Must start with first legitimate entry!!
	clear	str2
;patch1.09
	move	COMPCONTACT to str2
	call	zfillit using str2
	move	str2 to n2
	loop
		getitem	Comp1ComboContact,N2,str27
		unpack	str27,str25,str2
		if (str2 = "" |	str2 = " " | str2 = "  ")
			move	C1,N2
			break
		endif
	until (str2 = COMPCONTACT)
		add	C1,N2
;Extra protection
		if (N2 >= 98)
			move	C1,N2
			break
		endif
	repeat
	setitem	Comp1ComboContact,0,N2
;patch1.09
.
	setitem	Comp1EditParent,0,COMPMAIN
.
	call	CompCompKey using COMPMAIN,holda
	unpack	holda,str6,str55
	setitem	Comp1StatParentName,0,str55
.
	call	Trim using COMPACCTD
	if (COMPACCTD <> "")
		unpack	COMPACCTD,MM,YY
		pack	str5,MM,SLASH,YY
	else
		clear	str5
	endif
	setitem	Comp1EditAccountYear,0,str5
.
	if (COMPACCTM = "C")
		move	C2,N1
	elseif (COMPACCTM = "A")
		move	C3,N1
	else
		move	C1,N1
	endif
	setitem	Comp1ComboAccount,0,N1
.
	if (COMPBILLCDE = "B")
		move	C2,N1
	elseif (COMPBILLCDE = "A")
		move	C3,N1
	else
		move	C1,N1
	endif
	setitem	Comp1ComboBillCode,0,N1
.
	if (COMPINACTIVE = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckInactive,0,N1
.
	if (COMPDISCFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckDiscount,0,N1
.
	setitem	Comp1EditTaxID,0,COMPTAXID
.Mailer Fields
	if (COMPMLRFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckMailer,0,N1
.
	setitem	Comp1EditOldMailer,0,COMPOLDMLR
.
	setitem	Comp1EditBroker,0,COMPBROKER
.
	setitem	Comp1EditBrokerCnt,0,COMPBROKER1
.
	call	Trim using COMPBROKER
	if (COMPBROKER <> "")
		call	CompCompKey using COMPBROKER,holda
		unpack	holda,str6,str55
	else
		clear	str55
	endif
	setitem	Comp1StatBrokerName,0,str55
.
	setitem	Comp1EditConsultant,0,COMPCONSULT
.
	setitem	Comp1EditConsultantCnt,0,COMPCONSULT1
.
	call	Trim using COMPCONSULT
	if (COMPCONSULT <> "")
		call	CompCompKey using COMPCONSULT,holda
		unpack	holda,str6,str55
	else
		clear	str55
	endif
	setitem	Comp1StatConsultantName,0,str55
.
	setitem	Comp1EditInvoice,0,COMPINVSGO
.
	setitem	Comp1EditInvoiceCnt,0,COMPINVSGO1
.
	call	Trim using COMPINVSGO
	if (COMPINVSGO <> "")
		call	CompCompKey using COMPINVSGO,holda
		unpack	holda,str6,str55
	else
		clear	str55
	endif
	setitem	Comp1StatInvoiceName,0,str55
.
	if (COMPFAXFLAG1 = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckFaxMailer,0,N1
.
	if (COMPREGCDE = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckRegional,0,N1
.
	if (COMPMUSAGE <> "T")
		move	C0,N1
	else
		move	C1,N1
	endif
	setitem	Comp1CheckUsage,0,N1
.
	if (COMPBDRCTFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckBillDirect,0,N1
.
	if (COMPEXCHANGE = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckExchange,0,N1
.PATCH1.8  NEW RECEIVE STATEMENT FLAG
	if (COMPSTATEMENT <> "T")
		move	C0,N1
	else
		move	C1,N1
	endif
	setitem	Comp1CheckStatements,0,N1
.PATCH1.8
.
;Patch3.0
	move    B1,MTXCODE
	move    COMPNUM,NMTXFLD
	rep     ZFILL,NMTXFLD
	move	"Load-NMTXKEY",Location
	pack	KeyLocation,"Key: ",NMTXFLD
	call    NMTXKEY
	if over
		clear   MTXEXMPT
		clear   MTXC501
		move    B1,MTXCODE
		clear   MTXPROFT
	endif
.
	if (MTXPROFT = "Y")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckForProfit,0,N1
.
	setitem	Comp1Edit501C,0,MTXC501
.
	setitem	Comp1EditPermit,0,MTXEXMPT
.
	rep     lowup,MTXCODE
	if (MTXCODE = B1)
		move	C1,N1
	elseif (MTXCODE = "N")
		move	C2,N1
	elseif (MTXCODE = "T")
		move	C3,N1
	elseif (MTXCODE = "D")
		move	C4,N1
	elseif (MTXCODE = "R")
		move	C5,N1
	else
		move	C1,N1
	endif
	setitem	Comp1ComboExempt,0,N1
;Patch3.0
.Owner Fields
	if (COMPOWNFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckOwner,0,N1
.
	setitem	Comp1EditOldOwner,0,COMPOLDOWN
.
	setitem	Comp1EditManager,0,COMPMANAGER
.
	setitem	Comp1EditManagerCnt,0,COMPMANAGER1
.
	call	Trim using COMPMANAGER
	if (COMPMANAGER <> "")
		call	CompCompKey using COMPMANAGER,holda
		unpack	holda,str6,str55
	else
		clear	str55
	endif
	setitem	Comp1StatManagerName,0,str55
.
	setitem	Comp1EditServiceB,0,COMPSRVB
.
	setitem	Comp1EditServiceBCnt,0,COMPSRVB1
.
	call	Trim using COMPSRVB
	if (COMPSRVB <> "")
		call	CompCompKey using COMPSRVB,holda
		unpack	holda,str6,str55
	else
		clear	str55
	endif
	setitem	Comp1StatServiceBName,0,str55
.
	if (COMPGALLEY = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckGalley,0,N1
.Broker/Consultant Fields
	if (COMPBRKFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckBroker,0,N1
.
	if (COMPCLRFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckConsultant,0,N1
.
	setitem	Comp1EditOldBroker,0,COMPOLDBRK
.
	if (COMPPBRKFLG = "1")
		move	C1,N1
	elseif (COMPPBRKFLG = "2")
		move	C2,N1
	elseif (COMPPBRKFLG = "3")
		move	C3,N1
	else
		move	C1,N1
	endif
	setitem	Comp1ComboBrokerRank,0,N1
.
	if (COMPFAXFLAG2 = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckFaxBroker,0,N1
.
	if (COMPACCEPT <> "Y")
		move	C0,N1
	else
		move	C1,N1
	endif
	setitem	Comp1CheckGuarantees,0,N1
.Service Bureau Fields
	if (COMPSVBFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckServiceB,0,N1
.
	setitem	Comp1EditOldServiceB,0,COMPOLDSVB
.Manager
	if (COMPMNGFLG = "T")
		move	C1,N1
	else
		move	C0,N1
	endif
	setitem	Comp1CheckManager,0,N1
.Creation Fields
	pack	taskname,B55,B55,B55
	clear	taskname
	call	Trim using COMPUSER
	call	Trim using COMPDTE
	if (COMPDTE <> "" AND COMPUSER <> "")
		append	"Record Created ",taskname
		if (COMPDTE <> "")
			unpack	COMPDTE,CC,YY,MM,DD
			pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
			append	str15,taskname
		endif
		if (COMPUSER <> "")
			append	"by ",taskname
			append	COMPUSER,taskname
		endif
	endif
	reset	taskname
	setitem	Comp1StatCreate,0,taskname
.Revision Fields
	pack	taskname,B55,B55,B55
	clear	taskname
	call	Trim using COMPRUSER
	call	Trim using COMPRDTE
	if (COMPRDTE <> "" AND COMPRUSER <> "")
		append	"Record Modified ",taskname
		if (COMPRDTE <> "")
			unpack	COMPRDTE,CC,YY,MM,DD
			pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
			append	str15,taskname
		endif
		if (COMPRUSER <> "")
			append	"by ",taskname
			append	COMPRUSER,taskname
		endif
	endif
	reset	taskname
	setitem	Comp1StatRevision,0,taskname

;.Notes
	move	COMPNUM to COMPNOTEFLD
	move	"LoadScreen-COMPNOTEKEY",Location
	pack	KeyLocation,"Key: ",COMPNOTEFLD
	call	COMPNOTEKEY
	if over
		clear	COMPNOTES
	endif
	setitem	Comp1EditNotes,0,COMPNOTES
.Contacts
.SCREEN 2
;Patch1.02
	call	Comp2LoadListview
;	Comp1ListViewCnt.DeleteAllItems giving N9
;	Comp2ListView.DeleteAllItems giving N9
;	pack	CNCTFLD2,"01X",COMPNUM
;        move    "LoadScreen-CNTAIM",Location
;        pack    KeyLocation,"Key: ",CNCTFLD2
;	call	CNCTAIM
;	loop
;		until over
;.Contact Fields on Screen 1
;		Comp1ListViewCnt.InsertItem giving N9 using CNCTID
;		Comp1ListViewCnt.SetItemText using N9,CNCTFNAME,1
;		Comp1ListViewCnt.SetItemText using N9,CNCTID,2
;		if (CNCTTYPE = "1")
;			move	"Mailer",str15
;		elseif (CNCTTYPE = "2")
;			move	"Broker",str15
;		elseif (CNCTTYPE = "3")
;			move	"List Owner",str15
;		elseif (CNCTTYPE = "4")
;			move	"Service Bureau",str15
;		elseif (CNCTTYPE = "5")
;			move	"Consultant",str15
;		elseif (CNCTTYPE = "6")
;			move	"Manager",str15
;		else
;			clear	str15
;		endif
;		Comp1ListViewCnt.SetItemText using N9,str15,3
;.Contact Fields on Screen 2
;		pack	hold2,CNCTVARS
;		Comp2ListView.InsertItem giving N9 using CNCTID
;		Comp2ListView.SetItemText using N9,CNCTCODE,1
;		Comp2ListView.SetItemText using N9,CNCTID,2
;		Comp2ListView.SetItemText using N9,str15,3
;		Comp2ListView.SetItemText using N9,CNCTFNAME,4
;		Comp2ListView.SetItemText using N9,hold2,5
;		call	CNCTKG
;	repeat
;	Comp1ListViewCnt.GetItemCount giving result
;	if (result > 0)
;		Comp1ListViewCnt.SetItemState giving N9 using 0,2,2
;		call	Click_Comp1ListViewCnt
;	else
;		setitem	Comp1StatCnt,0,""
;	endif
;	Comp2ListView.GetItemCount giving result
;	if (result > 0)
;		Comp2ListView.SetItemState giving N9 using 0,2,2
;		call	Click_Comp2ListView
;	else
;		call	Comp2ClearScreen
;	endif
;Patch1.02
.SCREEN 3
	call	Trim using COMPFTP
	if (COMPFTP = "")
		Comp3WebBrowser.Navigate2 USING "about:blank"
	else
		Comp3WebBrowser.Navigate2 USING COMPFTP
	endif
	Comp4ListViewOffer.DeleteAllItems giving N9
	Comp4ListViewList.DeleteAllItems giving N9
	Comp4ListViewCategory.DeleteAllItems giving N9
	Comp4ListViewSample.DeleteAllItems giving N9
	call	Comp4ClearScreen
	if (COMPMLRFLG = "T")
.		call	CompViewSecondaryScreens
.SCREEN 4
.Offers
test
		call	Comp4LoadScreenOffers using COMPOLDMLR
.		call	Comp4LoadScreenOffers using COMPNUM
.Lists
		call	Comp4LoadScreenLists using COMPOLDMLR
.Categories
		call	Comp4LoadScreenCategories
.Samples
		call	Comp4LoadScreenSamples using COMPOLDMLR
.SCREEN 5
		call	MailerPackageOKClick using COMPOLDMLR
	else
.		call	CompHideSecondaryScreens
	endif
	return

StateFileError
	move    "Unknown State",STNAME
	return

Comp2LoadScreen
	setitem	Comp2EditComp,0,CNCTCODE
.
	setitem	Comp2EditNumber,0,CNCTID
.
	move	C1,N1
	move	CNCTTYPE,N1
	setitem	Comp2ComboType,0,N1
.
	setitem	Comp2EditFName,0,CNCTFNAME
.
	setitem	Comp2EditLName,0,CNCTLNAME
.
;patch1.1
	clear	str4
	clear	str3
	unpack	CNCTCNT,str4,str3
	setitem	Comp2EditOldComp,0,str4
	setitem	Comp2EditOldNumber,0,str3
;patch1.1
	setitem	Comp2EditSalutation,0,CNCTSAL
.
	setitem	Comp2EditTitle,0,CNCTTITLE
.
	setitem	Comp2EditEmail,0,CNCTEMAIL
.
	setitem	Comp2EditCountryCode,0,CNCTCNTRY
.
	call	Trim using CNCTPHONE
	if (CNCTPHONE <> "")
		count	N2,CNCTPHONE
		if (N2 <> 10)
			unpack	CNCTPHONE,str3,str4
			pack	str15,str3,DASH,str4
		else
			unpack	CNCTPHONE,areacode,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		endif
	else
		clear	str15
	endif
	setitem	Comp2EditPhone,0,str15
.
	call	Trim using CNCTPHONE1
	if (CNCTPHONE1 <> "")
		count	N2,CNCTPHONE1
		if (N2 <> 10)
			unpack	CNCTPHONE1,str3,str4
			pack	str15,str3,DASH,str4
		else
			unpack	CNCTPHONE1,areacode,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		endif
	else
		clear	str15
	endif
	setitem	Comp2EditPhone2,0,str15
.
	call	Trim using CNCTFAX
	if (CNCTFAX <> "")
		count	N2,CNCTFAX
		if (N2 <> 10)
			unpack	CNCTFAX,str3,str4
			pack	str15,str3,DASH,str4
		else
			unpack	CNCTFAX,areacode,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		endif
	else
		clear	str15
	endif
	setitem	Comp2EditFax,0,str15
;Contact
;	clear	str2
;	clear	n2
;	move CNCTSALES to str2
;	call zfillit using str2
;	move str2 to n2
;	setitem	Comp2ComboSalesPerson,0,n2
;;
	move	C2,N2		.Must start with first legitimate entry!!
	clear	str2
;patch1.09
	move	CNCTSALES to str2
	call	zfillit using str2
	move	str2 to n2
	loop
		getitem	Comp2ComboSalesPerson,N2,str27
		unpack	str27,str25,str2
		if (str2 = "" |	str2 = " " | str2 = "  ")
			move	C1,N2
			break
		endif
	until (str2 = CNCTSALES)
		add	C1,N2
;Extra protection
		if (N2 >= 98)
			move	C1,N2
			break
		endif
	repeat
	setitem	Comp2ComboSalesPerson,0,N2
;patch1.09

.
	if (CNCTINACTIVE = "T")
		setitem	Comp2CheckInactive,0,1
	else
		setitem	Comp2CheckInactive,0,0
	endif
.
	if (CNCTORDDISP = "T")
		setitem	Comp2CheckHidden,0,1
	else
		setitem	Comp2CheckHidden,0,0
	endif
.
	if (CNCTPWKLYFLG = "T")
		setitem	Comp2CheckWeekly,0,1
	else
		setitem	Comp2CheckWeekly,0,0
	endif
.
	if (CNCTPROMOFLG = "T")
		setitem	Comp2CheckPromo,0,1
	else
		setitem	Comp2CheckPromo,0,0
	endif
.
	if (CNCTPBRKFLG = "T")
		setitem	Comp2CheckBroker,0,1
	else
		setitem	Comp2CheckBroker,0,0
	endif
.
	if (CNCTPCLNTFLG = "T")
		setitem	Comp2CheckClient,0,1
	else
		setitem	Comp2CheckClient,0,0
	endif
.
	if (CNCTPPRTYFLG = "T")
		setitem	Comp2CheckParty,0,1
	else
		setitem	Comp2CheckParty,0,0
	endif
.
	if (CNCTPDMAFLG = "T")
		setitem	Comp2CheckDMA,0,1
	else
		setitem	Comp2CheckDMA,0,0
	endif
.
	if (CNCTPDTAFLG = "T")
		setitem	Comp2CheckDatacard,0,1
	else
		setitem	Comp2CheckDatacard,0,0
	endif
.
	if (CNCTHLYDYFLG = "T")
		setitem	Comp2CheckHoliday,0,1
	else
		setitem	Comp2CheckHoliday,0,0
	endif
.
	if (CNCTMRKTNEWS = "T")
		setitem	Comp2CheckNewsletter,0,1
	else
		setitem	Comp2CheckNewsletter,0,0
	endif
.
	call	Trim using CNCTUSER
	call	Trim using CNCTDATE
	move	b1,taskname
	clear	taskname
	if (CNCTDATE <> "" AND CNCTUSER <> "")
		append	"Record Created ",taskname
		if (CNCTDATE <> "")
			unpack	CNCTDATE,CC,YY,MM,DD
			pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
			append	str15,taskname
		endif
		if (CNCTUSER <> "")
			append	"by ",taskname
			append	CNCTUSER,taskname
		endif
	endif
	reset	taskname
	setitem	Comp2StatCreate,0,taskname
.
	call	Trim using CNCTUSER2
	call	Trim using CNCTDATE2
	move	b1,taskname
	clear	taskname
	if (CNCTDATE2 <> "" AND CNCTUSER2 <> "")
		append	"Record Modified ",taskname
		if (CNCTDATE2 <> "")
			unpack	CNCTDATE2,CC,YY,MM,DD
			pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
			append	str15,taskname
		endif
		if (CNCTUSER2 <> "")
			append	"by ",taskname
			append	CNCTUSER2,taskname
		endif
	endif
	reset	taskname
	setitem	Comp2StatRevision,0,taskname
	return

Comp4LoadScreenOffers Routine DimPtr
	pack	NOFRFLD1,"01X",DimPtr
	move    "C.LoadOffers-NOFRAIMKEY",Location
	pack	KeyLocation,"Key: ",NOFRFLD1
	call    NOFRAIM
	loop
		until over
		move	"C.LoadOffers-NOFRKG",Location
		pack	taskname,OFRVARS
		Comp4ListViewOffer.InsertItem giving N9 using OFNUM
		Comp4ListViewOffer.SetItemText using N9,OFDESC,1
		Comp4ListViewOffer.SetItemText using N9,taskname,2
		call	NOFRKG
	repeat
	Comp4ListViewOffer.EnsureVisible giving N10 using 0,0
	Comp4ListViewOffer.GetItemCount giving result
	if (result > C0)
		Comp4ListViewOffer.SetItemState giving N9 using 0,2,2
		call	Click_Comp4ListViewOffer
	endif
	return

Comp4LoadOffer
	setitem	Comp4EditCompOffer,0,OFMLR
.	setitem	Comp4EditNumberOffer,0,""		.IN USE?????????????????
	setitem	Comp4EditOfferNameOffer,0,OFDESC
	setitem	Comp4EditOfferOffer,0,OFNUM
	call	Trim using OFNAME
	call	Trim using OFDATE
	if (OFNAME <> "" AND OFDATE <> "")
		append	"Record Updated ",taskname
		if (OFDATE <> "")
			unpack	OFDATE,CC,YY,MM,DD
			pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
			append	str15,taskname
		endif
		if (OFNAME <> "")
			append	"by ",taskname
			append	OFNAME,taskname
		endif
	endif
	setitem	Comp4StatRevisedOffer,0,str55
	return

Comp4LoadScreenLists LRoutine DimPtr
	move	C2 to NXRFPATH
	move	DimPtr,NXRFFLD2
	move	"C.LoadList-NXRFKEY",Location
	pack	KeyLocation,"Key: ",NXRFFLD2
	call	NXRFKEY
	if over
		move	"000000",NXRFLIST
	endif
	loop
		move	NXRFLIST,NDATFLD
		move	C1,NDATPATH
		move	"C.LoadList-NDATKEY",Location
		pack	KeyLocation,"Key: ",NDATFLD
		call	NDATKEY
		if not over
		        Comp4ListViewList.InsertItem giving N9 using LSTNUM
		        Comp4ListViewList.SetItemText using N9,OLSTNAME,1
		endif
		move	"C.LoadList-NXRFKS",Location
		pack	KeyLocation,"Key: ",NXRFFLD2
		call	NXRFKS
		until over
		match	DimPtr,NXRFMLR
		until not equal
	repeat
	Comp4ListViewList.EnsureVisible giving N10 using 0,0
	return

Comp4LoadScreenCategories LRoutine DimPtr
.	move	C0,N9
.	pack	NOFRFLD1,"01X",DimPtr
.	move    "C.LoadOffers-NOFRAIMKEY",Location
.	pack	KeyLocation,"Key: ",NOFRFLD1
.	call    NOFRAIM
.	loop
.		until over
.		move	"C.LoadOffers-NOFRKG",Location
.		move	OFRVARS,taskname
.		Comp4ListViewCategory.InsertItem giving N9 using OFNUM
.		Comp4ListViewCategory.SetItemText using N9,OFDESC,1
.		Comp4ListViewCategory.SetItemText using N9,taskname,2
.		call	NOFRKG
.	repeat
.	Comp4ListViewCategory.EnsureVisible giving N10 using N9,0
.	Comp4ListViewCategory.GetItemCount giving result
.	if (result > C0)
.		Comp4ListViewCategory.SetItemState giving N9 using 0,2,2
.		call	Click_Comp4ListViewCategory
.	endif
	Comp4ListViewCategory.EnsureVisible giving N10 using 0,0
	return

Comp4LoadCategory
	setitem	Comp4EditCompCategory,0,""
	setitem	Comp4EditNumberCategory,0,""
	setitem	Comp4EditCategoryCategory,0,""
	setitem	Comp4EditCategoryNameCategory,0,""
	setitem	Comp4StatRevisionCategory,0,""
	return

Comp4LoadScreenSamples LRoutine DimPtr
.THIS ROUTINE CURRENTLY USES OLD FORMAT.  EVENTUALLY ALL THE .DCX FILES WILL NEED TO BE CONVERTED TO NEW COMPANY NUMBER!!!!!!!!!!!!!!
.Called	by:  CompLoadScreen
.This is used to dynamically change the	Samples	when the Mailer	is changed!!!
.Load Samples ListView
	pack	NSMPFLD1,"01X",DimPtr
	move	"C.LoadSamples-NSMPAIM",Location
	pack	KeyLocation,"Key: ",NSMPFLD1
	call	NSMPAIM
	loop
		until over
	        Comp4ListViewSample.InsertItem giving N9 using NSMPNUM
	        Comp4ListViewSample.SetItemText using N9,NSMPDES1,1
		move	"C.LoadSamples-NSMPKG",Location
		call	NSMPKG
	repeat
	Comp4ListViewSample.EnsureVisible giving N10 using 0,0
	return

Comp1VerifyData
.	getitem	Comp1CheckReset,0,0	.????????????????????????
	return

CompViewSecondaryScreens
.	setprop CompTabControl001,TabLabel="Company;Contact;Website;Offers/Lists/Cat./Samples;Packages"
	return

CompHideSecondaryScreens
.	if (TabNum > 3)
.		call	CompSwitchTab using C1
.	endif
.	setprop CompTabControl001,TabLabel="Company;Contact;Website"
	return

CompSwitchTab LRoutine FrmPtr
        if (TabNum <> FrmPtr)
                move    TabNum,N2
                call    CompTabClick
                move    FrmPtr,N2
                call    CompTabChange
                setitem CompTabControl001,0,FrmPtr
        endif
        return
CompTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
        if (N2 = C1)
                Deactivate COMP001A
        elseif (N2 = C2)
                Deactivate COMP001b
        elseif (N2 = C3)
		Deactivate COMP001c
        elseif (N2 = C4)
		Deactivate COMP001D
        elseif (N2 = C5)
		call	MailerPackageDisableForm
        elseif (N2 = C6)
        elseif (N2 = C7)
        else    N2 = C8
        endif
        return

CompTabChange
        move    N2,TabNum
;patch1.13
	call FloatMenuClose
;patch1.13
.
        if (N2 = C1)
                Activate COMP001A
        elseif (N2 = C2)
		Activate COMP001b
        elseif (N2 = C3)
		Activate COMP001c
		Comp3WebBrowser.Refresh
.Strange work-around - zorder for this object is somehow lost when DEACTIVATE/ACTIVATE is used on Child Form!!!
		getprop	Comp3WebBrowser,zorder=result
		setprop	Comp3WebBrowser,zorder=result
        elseif (N2 = C4)
		Activate COMP001D
        elseif (N2 = C5)
		call	MailerPackageEnableForm
        elseif (N2 = C6)
        elseif (N2 = C7)
        else   .N2 = C8
        endif
        return

VScrollChange
	getitem	VScrollBar1,0,N3			.Find where the ScrollBar currently sits.
	getprop	COMP0001,height=result			.Find the current Height
.Calculation:  ((MaxHeight - Current Height) / MaxIncrements of ScrollBar * Current ScrollBar Index)
	calc	howmany=((MaxHeight-result)/20*N3)
	setprop	COMP0001,WINOFFSETV=howmany
	return

HScrollChange
	getitem	HScrollBar1,0,N3			.Find where the ScrollBar currently sits.
	getprop	COMP0001,width=result			.Find the current Width
.Calculation:  ((MaxWidth - Current Width) / MaxIncrements of ScrollBar * Current ScrollBar Index)
	calc	howmany=((MaxWidth-result)/20*N3)
	setprop	COMP0001,WINOFFSETH=howmany
	return
Comp1GatherFields

.SCREEN 1
	clear 	CompVars
	getitem	Comp1EditNumber,0,COMPNUM               ;company number
.Patch1.9
	Goto COMPNOTESONLY if (BrkNoteFlag = YES)
.Patch1.9

	getitem	Comp1ComboStatus,0,N2                   ;credit status
	if (n2 = 1)	;" " = Credit OK
		move	" ",COMPCREDIT
	elseif (n2 = 2)	;"*" = ON HOLD.
		move	"*",COMPCREDIT
;	elseif (n2 = 3)	;"I" = INACTIVE,	                     may get rid of???? Yes
;		move	"I",COMPCREDIT
	elseif (n2 = 3)	;"B" = Credit Risk,
		move	"B",COMPCREDIT
	elseif (n2 = 4)	;"N" = New Mailer,
		move	"N",COMPCREDIT
	elseif (n2 = 5)	;"P" = POLITICAL MAILER.  - reset nightly if released
		move	"P",COMPCREDIT
	elseif (n2 = 6)	;"W" = "W" = Warning - read note
		move	"W",COMPCREDIT
	elseif (n2 = 7)	;"M" = Must Prepay
		move	"M",COMPCREDIT
	elseif (n2 = 8)	;"9" = On hold until over 90s paid
		move	"9",COMPCREDIT
	elseif (n2 = 9)	;"G" = Guarantees are always required
		move	"G",COMPCREDIT
	elseif (n2 = 10)	;"g" = Guarantees not Accepted
		move	"g",COMPCREDIT
	endif

	getitem Comp1EditName,0,COMPCOMP
                    call    TRIM using COMPCOMP
                    count   HowMany,COMPCOMP
                    if      (HowMany = 0)
		alert	caution,"Company Name Required!",result
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		return
                    elseif  (COMPCOMP = "")
		alert	caution,"Company Name Required!",result
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		return
                    endif
;Looking for Comp1CheckReset?  It's one of last checked objects.


	getitem	Comp1EditAddress1,0,COMPADDR
;
	getitem	Comp1EditAddress2,0,COMPADDR2
;
	getitem	Comp1EditCity,0,COMPCITY
;
	getitem	Comp1EditState,0,COMPSTATE
;
                    call	trim using COMPSTATE
                    count	N2,COMPSTATE
                    if (N2 = "0")
                             move	"",STNAME
                    else
                             pack	NSTFLD,COMPSTATE
                             trap	StateFileError if IO
                             read	NSTFILE,NSTFLD;STVARS
                             trapclr	IO
                             if over
		alert	caution,"Invalid State Abbreviation",result,"State Abbreviation"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditState
		return
                             else
		move                   stabb to compstate
		setitem                Comp1EditState,0,compstate
		setitem                Comp1StatStateName,0,STNAME
                             endif
                    endif
;
	getitem	Comp1EditZip,0,COMPZIP
;
	getitem	Comp1EditCountry,0,COMPCNTRY
;
	getitem	Comp1EditCountryCode,0,COMPCNTRYCDE
;
	getitem	Comp1EditPhone,0,str14
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
                                        move 	str14 to COMPPHONE
		unpack	str14,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		setitem	Comp1EditPhone,0,str15
	elseif (result = 0)
		clear	str15
		clear	COMPPHONE
	elseif (result = 7)
 		alert type=yesno1," Is this phone number local? (if yes, I will add 510 area code)",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp1EditPhone,0,str15
			pack	COMPPHONE,areacode,str3,str4
		elseif (n1=7)    . 6 = yes , 7 = no
			alert    caution,"Please add area code to phone number",result,"Area Code"
			setfocus Comp1EditPhone
		                    return
		endif
                    elseif (result < 7)
		alert	caution,"Phone Number Must Be at Least Seven Digits",result,"Incomplete Phone Number"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditPhone
                                        return
                    elseif (result > 10)
		alert	caution,"Phone Number Must Be No More than Ten Digits",result,"Phone Number too long"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditPhone
                                        return
                    else
		alert	caution,"Please Check format of phone number",result,"Phone Number invalid"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditPhone
                                        return
	endif
;
	getitem	Comp1EditFax,0,str14
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
      move 	str14 to COMPFAX
		unpack	str14,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		setitem	Comp1EditFax,0,str15
	elseif (result = 0)
		clear	str15
		clear	COMPFAX
;patch1.07
		alert caution,"Fax Number Must Be Entered!",result,"Required Field"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditFax
		return
;patch1.07
	elseif (result = 7)
		alert type=yesno1," Is this fax number local(if yes, I will add 510 area code)?",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp1EditFax,0,str15
			pack	COMPFAX,areacode,str3,str4
		elseif (n1=7)    . 6 = yes , 7 = no
			alert	caution,"Please add area code to phone number",result,"Area Code"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus	Comp1EditFax
		                    return
		endif
                    elseif (result < 7)
		alert	caution,"Phone Number Must Be at Least Seven Digits",result,"Incomplete Phone Number"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditFax
                                        return
                    elseif (result > 10)
		alert	caution,"Phone Number Must Be No More than Ten Digits",result,"Phone Number too long"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditFax
                                        return
                    else
		alert	caution,"Please Check format of phone number",result,"Phone Number invalid"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditFax
                                        return
	endif
;
	getitem	Comp1EditAcctFax,0,str14
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
		move 	str14 to COMPACCTFAX
		unpack	str14,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		setitem	Comp1EditAcctFax,0,str15
	elseif (result = 0)
		clear	str15
		clear	COMPACCTFAX
	elseif (result = 7)
 		alert type=yesno1," Is this phone number local(if yes, I will add 510 area code)?",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp1EditAcctFax,0,str15
			pack	COMPACCTFAX,areacode,str3,str4
		elseif (n1=7)    . 6 = yes , 7 = no
			alert	caution,"Please add area code to phone number",result,"Area Code"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus	Comp1EditAcctFax
		                    return
		endif
                    elseif (result < 7)
		alert	caution,"Phone Number Must Be at Least Seven Digits",result,"Incomplete Phone Number"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditAcctFax
                                        return
                    elseif (result > 10)
		alert	caution,"Phone Number Must Be No More than Ten Digits",result,"Phone Number too long"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditAcctFax
                                        return
                    else
		alert	caution,"Please Check format of phone number",result,"Phone Number invalid"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditAcctFax
                                        return
	endif

	getitem	Comp1EditEmail,0,COMPEMAIL
;
	getitem	Comp1EditWebSite,0,COMPFTP
;
                    clear 	n2
	getitem	Comp1ComboContact,N1,N2	;Internal Contact Information
	getitem	Comp1ComboContact,N2,str27	;Internal Contact Information
	unpack	str27,str25,str2
	if (str2 = "" |	str2 = " " | str2 = "  ")		;Did not pick a contact
		alert	caution,"Company Needs a Valid In-house Contact Person",Result,"Contact Person Needed"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1ComboContact
		return
	else
;patch1.08
		move	n2 to COMPCONTACT
		rep	zfill,COMPCONTACT

;patch1.08
;	                    clear 	str2
;		getitem	Comp1ComboContact,N2,str45
;		unpack	str45,str35,str1,str2
;		move 	str2,COMPCONTACT
;		rep 	zfill,COMPCONTACT
	endif
;
	getitem	Comp1EditParent,0,COMPMAIN             ;if this is a satelite or subsidiary office it holds id # of main (parent) corp record
        clear	Howmany
        count	HowMany,COMPMAIN
        if	(HowMany = 0)	                   ;Do nothing
	else
;		rep 	zfill,COMPMAIN
		call	zfillit using COMPMAIN
		call	CompCompKey using COMPMAIN,holda
		if over
			alert	caution,"Invalid Parent Company",result,"Error"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus	Comp1EditParent
			return
		else
			unpack	holda,str6,str55
			setitem	Comp1EditParent,0,str6
			setitem	Comp1StatParentName,0,str55
		endif
	endif
;
	getitem   Comp1EditAccountYear,0,str5   ;Fiscal Date MMDD
	call   RemoveChar  using str5,slash
	call   RemoveChar  using str5,bslash
	call   TRIM  using str5
	move   str5,str4
	count  N2,str4
	if (N2 = 0)
		clear   MM
		clear   DD
		clear   COMPACCTD
	elseif (N2 <> 4)
		alert caution,"Fiscal Date Must be in MMDD Format",result,"Error in Date"
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus	Comp1EditAccountYear
		return
	elseif (N2 = 4)
		unpack  str4,MM,DD
		move    MM,N2
		if ((N2 > "12")|(N2 < c1))
			alert	caution,"Invalid Month!",result
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus	Comp1EditAccountYear
			Return
		else
			move    DD,N2
			if ((N2 > "31")|(N2 < c1))
				alert caution,"Invalid Day!",result
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				setfocus	Comp1EditAccountYear
				Return
			endif
		endif
		pack   COMPACCTD with MM,DD
		pack	str5,MM,SLASH,DD
		setitem	Comp1EditAccountYear,0,str5
	endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	clear n1
	clear n2
	getitem	Comp1ComboAccount,N1,N2                 ;Acccounting Method
	clear  str35
	getitem	Comp1ComboAccount,N2,str35
	if (str35 <> "")
		match	"Cash",str35
		if equal
			move "C" to COMPACCTM
		endif
		reset str35
		Match "Accrual",str35
		if equal
			move "A" to COMPACCTM
		endif
		reset str35
	else
		clear COMPACCTM
	endif
 	clear n1		                    ;batch billing info
 	clear n2
	getitem	Comp1ComboBillCode,N1,N2
                    clear 	str35
	getitem	Comp1ComboBillCode,N2,str35
	if (str35 <> "")
                    	Match	"Batch Bill",str35
		if 	equal
			move "B" to COMPBILLCDE
		endif
		reset 	str35
                    	Match	"Adjust Batch Bill",str35
		if equal
			move "A" to COMPBILLCDE
		endif
		reset 	str35
	else
		clear COMPBILLCDE
	endif
;
	clear n1
	getitem	Comp1CheckInactive,0,N1                 ;Inactive Vendor
	if (N1 = C1)
		move	"T",COMPINACTIVE
	else
		move	"F",COMPINACTIVE
	endif
;
	clear n1
	getitem	Comp1CheckDiscount,0,N1                 ;Discount Vendor
	if (N1 = C1)
		move	"T",COMPDISCFLG
	else
		move	"F",COMPDISCFLG
	endif
;
	getitem	Comp1EditTaxID,0,COMPTAXID
;Mailer Fields
	clear n1
	getitem	Comp1CheckMailer,0,N1                 ;Mailer?
	if (N1 = C1)
		move	"T",COMPMLRFLG
	else
		move	"F",COMPMLRFLG
	endif
;;;Waiting till after Broker File Conversion for now just grabbing variables

	getitem	Comp1EditOldMailer,0,COMPOLDMLR       ;OLDMailer
	rep	zfill,COMPOLDMLR
t
	getitem	Comp1EditBroker,0,COMPBROKER
	if (COMPBROKER <> "")
		call zfillit using COMPBROKER
		call	CompCompKey using COMPBROKER,holda
		unpack	holda,str6,str55
		if (holda <> "")
			reset holda to 228
			move holda to str1
			if (str1 <> "T")
				alert caution,COMPBROKER,result,"Not A Valid Company for this field"
				reset holda
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				setfocus Comp1EditBroker
				return
			endif
			getitem	Comp1EditBrokerCnt,0,COMPBROKER1
			call zfillit using COMPBROKER1
			pack	str9,COMPBROKER,COMPBROKER1
			call	CnctCnctKey using str9,hold2
			if (hold2 = "")
				if (COMPBROKER1 <> "000")
					alert caution,str9,result,"Not A Valid Broker Contact"
					call	Comp1EnableLower
					call	CompEnableUpperButtons
					setfocus Comp1EditBrokerCnt
					return
				endif
			endif
			setitem	Comp1EditBroker,0,COMPBROKER
			setitem	Comp1StatBrokerName,0,str55
			setitem	Comp1EditBrokerCnt,0,COMPBROKER1
		else
			alert caution,COMPBROKER,result,"Not A Valid Broker"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus Comp1EditBroker
			return
		endif
	else
		clear COMPBROKER
		clear	str55
		clear COMPBROKER1
		setitem	Comp1EditBroker,0,COMPBROKER
		setitem	Comp1StatBrokerName,0,str55
		setitem	Comp1EditBrokerCnt,0,COMPBROKER1
	endif

	getitem	Comp1EditConsultant,0,COMPCONSULT
	if (COMPCONSULT <> "")
		call zfillit using COMPCONSULT
		call	CompCompKey using COMPCONSULT,holda
		unpack	holda,str6,str55
		if (holda <> "")
			reset holda to 231
			move holda to str1
			if (str1 <> "T")
				alert caution,COMPCONSULT,result,"Not A Valid Company for this field"
				reset holda
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				setfocus Comp1EditConsultant
				return
			endif
			getitem	Comp1EditConsultantCnt,0,COMPCONSULT1
			call zfillit using COMPCONSULT1
			pack	str9,COMPCONSULT,COMPCONSULT1
			call	CnctCnctKey using str9,hold2
			if (hold2 = "")
				if (COMPCONSULT1 <> "000")
					alert caution,str9,result,"Not A Valid Consultant Contact"
					call	Comp1EnableLower
					call	CompEnableUpperButtons
					setfocus Comp1EditConsultantCnt
					return
				endif
			endif
			setitem	Comp1EditConsultant,0,COMPCONSULT
			setitem	Comp1StatConsultantName,0,str55
			setitem	Comp1EditConsultant,0,COMPCONSULT1
		else
			alert caution,COMPCONSULT,result,"Not A Valid Consultant"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus Comp1EditConsultant
			return
		endif
	else
		clear	str55
		clear COMPCONSULT
		clear COMPCONSULT1
		setitem	Comp1EditConsultant,0,COMPCONSULT
		setitem	Comp1StatConsultantName,0,str55
		setitem	Comp1EditConsultantCnt,0,COMPCONSULT1
	endif
	getitem	Comp1EditInvoice,0,COMPINVSGO
	if (COMPINVSGO <> "")
		call zfillit using COMPINVSGO
		call	CompCompKey using COMPINVSGO,holda
		unpack	holda,str6,str55
		if (holda <> "")
			getitem	Comp1EditInvoiceCnt,0,COMPINVSGO1
			call zfillit using COMPINVSGO1
			pack	str9,COMPINVSGO,COMPINVSGO1
			call	CnctCnctKey using str9,hold2
			if (hold2 = "")
				if (COMPINVSGO1 <> "000")
					alert caution,str9,result,"Not A Valid Invoice Contact"
					call	Comp1EnableLower
					call	CompEnableUpperButtons
					setfocus Comp1EditInvoiceCnt
					return
				endif
			endif
			setitem	Comp1EditInvoice,0,COMPINVSGO
			setitem	Comp1StatInvoiceName,0,str55
			setitem	Comp1EditInvoiceCnt,0,COMPINVSGO1
		else
			alert caution,COMPINVSGO,result,"Not A Valid Invoice Contact"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus Comp1EditInvoice
			return
		endif
	else
		clear	str55
		clear COMPINVSGO
		clear COMPINVSGO1
		setitem	Comp1EditInvoice,0,COMPINVSGO
		setitem	Comp1StatInvoiceName,0,str55
		setitem	Comp1EditInvoiceCnt,0,COMPINVSGO1
	endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
	clear n1
	getitem	Comp1CheckFaxMailer,0,N1                 ;Fax Order Confirmations for Mailer
	if (N1 = C1)
		move	"T",COMPFAXFLAG1
	else
		move	"F",COMPFAXFLAG1
	endif
.
	clear n1
	getitem	Comp1CheckRegional,0,N1                 ;'T' if regional
	if (N1 = C1)
		move	"T",COMPREGCDE
	else
		move	"F",COMPREGCDE
	endif

.
	clear n1
	getitem	Comp1CheckUsage,0,N1                 ;'T' if Usage Sharing Allowed
	if (N1 = C1)
		move	"T",COMPMUSAGE
	else
		move	"F",COMPMUSAGE
	endif
;.
	clear n1
	getitem	Comp1CheckBillDirect,0,N1                 ;'T' Mailer Bill Direct
	if (N1 = C1)
		move	"T",COMPBDRCTFLG
	else
		move	"F",COMPBDRCTFLG
	endif
;.
	clear n1
	getitem	Comp1CheckExchange,0,N1                 ;'T' Exchange Allowed
	if (N1 = C1)
		move	"T",COMPEXCHANGE
	else
		move	"F",COMPEXCHANGE
	endif
.PATCH1.8  NEW RECEIVE STATEMENT FLAG
	clear n1
	getitem	Comp1CheckStatements,0,N1
	if (N1 = C1)
		move	"T",COMPSTATEMENT
	else
		move	"F",COMPSTATEMENT
	endif

.PATCH1.8

;.
	getitem	Comp1CheckInactive,0,N1                 ;Inactive Vendor
	if (N1 = C1)
		move	"T",COMPINACTIVE
	else
		move	"F",COMPINACTIVE
	endif
;;Patch4.0 Mailer Conversion Code

	getitem	Comp1CheckForProfit,0,N1
	if (N1 = C1)
		MOVE "Y" to MTXPROFT
	else
		move	" " to MTXPROFT
	endif
	getitem	Comp1Edit501C,0,MTXC501
;;
	clear n2
	call	TRIM using MTXC501
   count	N2,MTXC501
   if	(N2 > C0)
	   cmatch	YES,MTXPROFT
		if equal
			alert   caution,"Not Possible for a For-Profit!",result
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			setfocus Comp1Edit501C
		endif
	endif
  	if (MTXC501 <> B1 AND (MTXC501 = "0" OR MTXC501 = "1"))
		alert	caution,"501C Code Must Be '2-9'!",result
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus Comp1Edit501C
		return
	endif
;;
	clear n2
	getitem	Comp1EditPermit,0,MTXEXMPT
   call    TRIM using MTXEXMPT
   count   N2,MTXEXMPT
	clear n1
	getitem	Comp1ComboExempt,0,N1
   if (n1 <= C2 AND N2 <> C0)
		alert caution,"Specify Number Type!",result
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus Comp1ComboExempt
		return
	elseif (n1 > C2 AND N2 = C0)
		alert caution,"Permit Number Required!",result
		call	Comp1EnableLower
		call	CompEnableUpperButtons
		setfocus Comp1EditPermit
		return
	endif
.first line is blank add 1
	if (N1 = c1)
		move	MTXCODE,B1
	elseif (n1 = c2)
      move  "N",MTXCODE
	elseif (n1 = c3)
      move  "T",MTXCODE
	elseif (n1 = c4)
      move  "D",MTXCODE
	elseif (n1 = c5)
      move  "R",MTXCODE
	else
      move  B1,MTXCODE
	endif
;May have to update from here but not sure-will comment out for now
.	move    B1,MTXCODE
.	move    COMPNUM,NMTXFLD
.	rep     ZFILL,NMTXFLD
.	move	"Load-NMTXKEY",Location
.	pack	KeyLocation,"Key: ",NMTXFLD
.	call    NMTXKEY
.	if over
.		clear   MTXEXMPT
.		clear   MTXC501
.		move    B1,MTXCODE
.		clear   MTXPROFT
.	endif
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Patch4.0
.Owner Fields
	clear n1
	getitem	Comp1CheckOwner,0,N1                 ;'T' Exchange Allowed
	if (N1 = C1)
		move	"T",COMPOWNFLG
	else
		move	"F",COMPOWNFLG
	endif
.
	getitem	Comp1EditOldOwner,0,COMPOLDOWN
	rep	zfill,COMPOLDOWN
.
	getitem	Comp1EditManager,0,COMPMANAGER
	rep	zfill,COMPMANAGER
.
	getitem	Comp1EditManagerCnt,0,COMPMANAGER1
	rep	zfill,COMPMANAGER1
.
;	call	Trim using COMPMANAGER
;	if (COMPMANAGER <> "")
;		call	CompCompKey using COMPMANAGER,holda
;		unpack	holda,str6,str55
;	else
;		clear	str55
;	endif
;	setitem	Comp1StatManagerName,0,str55
.
;
	getitem	Comp1EditServiceB,0,COMPSRVB
	rep	zfill,COMPSRVB
;
	getitem	Comp1EditServiceBCnt,0,COMPSRVB1
	rep	zfill,COMPSRVB1
.
;	call	Trim using COMPSRVB
;	if (COMPSRVB <> "")
;		call	CompCompKey using COMPSRVB,holda
;		unpack	holda,str6,str55
;	else
;		clear	str55
;	endif
;	setitem	Comp1StatServiceBName,0,str55
.

	clear n1		                  ;T = List Owner prefers Galley Listing
	getitem	Comp1CheckGalley,0,N1
	if (N1 = C1)
		move	"T",COMPGALLEY
	else
		move	"F",COMPGALLEY
	endif
.Broker/Consultant Fields
;
	clear n1		                  ;'T' Consultant
	getitem	Comp1CheckConsultant,0,N1
	if (N1 = C1)
		move	"T",COMPCLRFLG
	else
		move	"F",COMPCLRFLG
	endif
;

	clear n1		                  ;'T' Broker
	getitem	Comp1CheckBroker,0,N1
	if (N1 = C1)
		move	"T",COMPBRKFLG
	else
		move	"F",COMPBRKFLG
		if (COMPCLRFLG = "F")
			if (COMPMLRFLG = "F")
				pack taskname,"New Company Must Be marked a Broker or Consultant or Mailer",crlf,"Please Check one of the boxes below"
				alert caution,taskname,result,"New Broker or Consultant or Mailer"
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				return
			endif
		endif
	endif
testo
;patch3.0
		if ((COMPCLRFLG = "T" & COMPBRKFLG = "T") or (COMPCLRFLG = "T" & COMPMLRFLG = "T"))
				pack taskname,"New Company MUST Be marked as a Broker OR Consultant OR Mailer",crlf,"Please Uncheck one of the boxes "
				alert caution,taskname,result,"New Broker or Consultant or Mailer"
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				return
		elseif ((COMPMLRFLG = "T" & COMPBRKFLG = "T") or (COMPMLRFLG = "T" & COMPCLRFLG = "T"))
				pack taskname,"New Company MUST Be marked as a Broker OR Consultant OR Mailer",crlf,"Please Uncheck one of the boxes "
				alert caution,taskname,result,"New Broker or Consultant or Mailer"
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				return
		elseif ((COMPBRKFLG = "T" & COMPMLRFLG = "T") or (COMPBRKFLG = "T" & COMPCLRFLG = "T"))
				pack taskname,"New Company  MUST Be marked as a Broker OR Consultant OR Mailer",crlf,"Please Uncheck one of the boxes "
				alert caution,taskname,result,"New Broker or Consultant or Mailer"
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				return
		endif
;patch3.0
.		if 	(n2 <> c1)
.			pack taskname,"New Company Must Be marked a Broker or Consultant",crlf,"Please Check one of the boxes below"
.			alert caution,taskname,result,"New Broker or Consultant"
.			call	Comp1EnableLower
.			call	CompEnableUpperButtons
.			setfocus	Comp1CheckBroker
.			return
.			endif
.Patch4.0

;patch1.06
.	endif
	getitem	Comp1EditOldBroker,0,COMPOLDBRK       ;OLD Broker
	rep	zfill,COMPOLDBRK
;
	getitem	Comp1ComboBrokerRank,0,N2             ;Broker Rank
	move	C1,N2		.Default
	if (n2 = 1)	;
		move	"1",COMPPBRKFLG
	elseif (n2 = 2)	;
		move	"2",COMPPBRKFLG
	elseif (n2 = 3)	;
		move	"3",COMPPBRKFLG
	endif
;
	clear n1		                  ;Broker Fax Order Confirmations
	getitem	Comp1CheckFaxBroker,0,N1
	if (N1 = C1)
		move	"T",COMPFAXFLAG2
	else
		move	"F",COMPFAXFLAG2
	endif
;
	clear n1		                  ;Broker Accept Guarantees
	getitem	Comp1CheckGuarantees,0,N1
	if (N1 = C1)
		move	"T",COMPACCEPT
	else
		move	"F",COMPACCEPT
	endif
.Service Bureau Fields
	clear n1		                  ;Broker Accept Guarantees
	getitem	Comp1CheckServiceB,0,N1
	if (N1 = C1)
		move	"T",COMPSVBFLG
	else
		move	"F",COMPSVBFLG
	endif
;
	getitem	Comp1EditOldServiceB,0,COMPOLDSVB     ;OLD Service Bureau
	rep 	zfill,COMPOLDSVB
.Manager
	clear n1		                  ;'T' List Manager
	getitem	Comp1CheckManager,0,N1
	if (N1 = C1)
		move	"T",COMPMNGFLG
	else
		move	"F",COMPMNGFLG
	endif

;patch1.08
	getitem	Comp1CheckReset,0,n1
.patch1.2
	if (NEWFLAG = NO)
		if (n1 = c1)
			trap	IOMssg if IO
			filepi	1;credit
			read	CREDIT,COMPNUM;;
			if over
				write CREDIT,COMPNUM;COMPNUM,HOLDCREDIT
			else
				Update	CREDIT;COMPNUM,HOLDCREDIT
			endif
			trapclr	IO
			setitem	Comp1CheckReset,0,0
		endif
	endif
.patch1.2
;patch1.08
testing
.Creation Fields
	if (NEWFLAG = YES)
		move	NPASUSER,COMPUSER
		clock	timestamp,str8
		move	str8 to COMPDTE
;		pack	taskname,B55,B55,B55
		move	b1,taskname
		clear	taskname
		call	Trim using COMPUSER
		call	Trim using COMPDTE
		if (COMPDTE <> "" AND COMPUSER <> "")
			append	"Record Created ",taskname
			if (COMPDTE <> "")
				unpack	COMPDTE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
				append	str15,taskname
			endif
			if (COMPUSER <> "")
				append	"by ",taskname
				append	COMPUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp1StatCreate,0,taskname
		move	NPASUSER,COMPRUSER
		clock	timestamp,str8
		move	str8 to COMPRDTE
		move	b1,taskname
;		pack	taskname,B55,B55,B55
		clear	taskname
		call	Trim using COMPRUSER
		call	Trim using COMPRDTE
		if (COMPRDTE <> "" AND COMPRUSER <> "")
			append	"Record Modified ",taskname
			if (COMPRDTE <> "")
				unpack	COMPRDTE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
				append	str15,taskname
			endif
			if (COMPRUSER <> "")
				append	"by ",taskname
				append	COMPRUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp1StatRevision,0,taskname
	else
.Revision Fields
		call	CompCompKey using COMPNUM,holda
		clear	str7
		clear	str8
		unpack	holda,b55,b55,b55,b55,b55,b55,b12,b6,str7,str8
		move	str7 to COMPUSER
		if	(COMPUSER = b7)
			move "Unknown" to COMPUSER
		endif
		move	str8 to COMPDTE
		if	(COMPDTE = b8)
			move "00000000" to COMPDTE
		endif
		clear	taskname
		call	Trim using COMPUSER
		call	Trim using COMPDTE
		if (COMPDTE <> "" AND COMPUSER <> "")
			append	"Record Created ",taskname
			if (COMPDTE <> "")
				unpack	COMPDTE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
				append	str15,taskname
			endif
			if (COMPUSER <> "")
				append	"by ",taskname
				append	COMPUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp1StatCreate,0,taskname
		move	NPASUSER,COMPRUSER
		clock	timestamp,str8
		move	str8 to COMPRDTE
		move	b1,taskname
;		pack	taskname,B55,B55,B55
		clear	taskname
		call	Trim using COMPRUSER
		call	Trim using COMPRDTE
		if (COMPRDTE <> "" AND COMPRUSER <> "")
			append	"Record Modified ",taskname
			if (COMPRDTE <> "")
				unpack	COMPRDTE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,b1
				append	str15,taskname
			endif
			if (COMPRUSER <> "")
				append	"by ",taskname
				append	COMPRUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp1StatRevision,0,taskname
	endif
;.Attempt an ISAM read
;Comp1Write

	if (NEWFLAG = NO)
		move	COMPNUM,COMPFLD
		rep	zfill,COMPFLD
		move	"CompOK-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPTST
		if not over
;patch1.04
;patch1.16
			if 	(COMPBRKFLG = "T" OR COMPCLRFLG = "T")
;			if 	(COMPBRKFLG = "T")
;patch1.16
;				move COMPFLD to COMPMAIN
				call COMPCOMPKEY using COMPFLD,holda
				unpack holda,taskname,str35,str1,COMP0001OLDBRK
				if (COMP0001OLDBRK="    ")
					move	"Save-GNXTKEY",Location
					move	"NBRKNXT",GNXTFLD
					call	GNXTKEY
					call	trim using gnxtnum
;					bump	GNXTNUM,2
					move	GNXTNUM,n4
					loop
						add	C1,n4
						move	n4,BRKNUM
						rep	zfill,BRKNUM
						pack	COMPFLD4,BRKNUM
						rep	zfill,COMPFLD4
						call	COMPTST2
					until over
					repeat
					clear	GNXTNUM
					move	COMPFLD4,GNXTNUM
					move	COMPFLD4,COMPOLDBRK
					reset	GNXTNUM
					rep	zfill,GNXTNUM
					move	"Save-GNXTUPD",Location
					call	GNXTUPD
				endif
;refresh
				move	COMPNUM,COMPFLD
				rep	zfill,COMPFLD
				move	"CompOK-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPTST
				if over
					alert	caution,"Error Modifying Record",result,"Call IS - Could not find record"
					call	Comp1EnableLower
					call	CompEnableUpperButtons
					return
				endif
			elseif (COMPMLRFLG = "T")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				call COMPCOMPKEY using COMPFLD,holda
				unpack holda,taskname,str30,str2,COMP0001OLDMLR
				if (COMP0001OLDMLR="    ")
					move	"Save-GNXTKEY",Location
					move	"NMLRNXT",GNXTFLD
					call	GNXTKEY
					call	trim using gnxtnum
;					bump	GNXTNUM,2
					move	GNXTNUM,n4
					loop
						add	C1,n4
						move	n4,MNUM
						rep	zfill,MNUM
						pack	COMPFLD3,MNUM
						rep	zfill,COMPFLD3
						call	COMPTST3
					until over
					repeat
					clear	GNXTNUM
					move	COMPFLD3,GNXTNUM
					move	COMPFLD3,COMPOLDMLR
					reset	GNXTNUM
					rep	zfill,GNXTNUM
					move	"Save-GNXTUPD",Location
					call	GNXTUPD
				endif
;refresh
				move	COMPNUM,COMPFLD
				rep	zfill,COMPFLD
				move	"CompOK-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPTST
				if over
					alert	caution,"Error Modifying Record",result,"Call IS - Could not find record"
					call	Comp1EnableLower
					call	CompEnableUpperButtons
					return
				endif
			endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.			endif
;patch1.04
			call	COMPUPD
			Call MailerTaxUpdate
COMPNOTESONLY
;NotesUpdate
			getitem	Comp1EditNotes,0,COMPNOTES
;patch1.04
			if	(COMPNOTES <> "")
;patch1.04
				move	COMPNUM to COMPNOTEFLD
				move	COMPNUM to COMPNOTECOMP
				move	"Save-COMPNOTEUPD",Location
				pack	KeyLocation,"Key: ",COMPNOTEFLD
				call	COMPNOTETST
				if	not over
					call	COMPNOTEUPD
;					setitem	CompSearchList,0,COMPNUM
;					call	Click_CompOK
				else
					move	"Save-COMPNOTEUPD",Location
					pack	KeyLocation,"Key: ",COMPNOTEFLD
					call	COMPNOTEWRT
;					alert	caution,"Error Modifying Notes Record",result,"Call IS - Could not find record"
				endif
;					setitem	CompSearchList,0,COMPNUM
;					call	Click_CompOK
.patch1.4
			else
				move	COMPNUM to COMPNOTEFLD
				move	COMPNUM to COMPNOTECOMP
				move	"Save-COMPNOTEUPD",Location
				pack	KeyLocation,"Key: ",COMPNOTEFLD
				call	COMPNOTETST
				if	not over
					call	COMPNOTEUPD
				endif
.patch1.4
			endif

		else
				alert	caution,"Error Modifying Record",result,"Call IS - Could not find record"
				call	Comp1EnableLower
				call	CompEnableUpperButtons
				return
		endif
	else
		move	"COMPANY ",GNXTFLD
		call	GNXTKEY
		if over
;Need to add error code
			alert	caution,"Can't Find Next Company Number",result,"Call IS"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			return
		endif
		move    	GNXTNUM,N6
		loop
			add	C1,N6
			move	N6,COMPFLD
			rep	zfill,COMPFLD
			call	COMPTST
		until over
		repeat
		move	COMPFLD to COMPNUM
		Call	zfillit using COMPNUM
		rep	zfill using COMPNUM
;Modify GNXT.DAT if CLient is NEW
		clear	GNXTNUM
		move	COMPFLD,GNXTNUM
		reset	GNXTNUM
		rep	zfill,GNXTNUM
		move	"Save-GNXTUPD",Location
		call	GNXTUPD
;Test for Duplicates first
		move	"Save-COMPTST",Location
		call	COMPTST
		if over
;patch1.04
;patch1.3
			if 	(COMPBRKFLG = "T" OR COMPCLRFLG = "T")
;			if	(COMPBRKFLG = "T")
;patch1.3
				move	"Save-GNXTKEY",Location
				move	"NBRKNXT",GNXTFLD
				call	GNXTKEY
				call	trim using gnxtnum
;				bump	GNXTNUM,2
				move	GNXTNUM,n4
				loop
					add	C1,n4
					move	n4,BRKNUM
					rep	zfill,BRKNUM
					pack	COMPFLD4,BRKNUM
					rep	zfill,COMPFLD4
					call	COMPTST2
				until over
				repeat
;				move	CNCTFLD4 to COMP0001OLDBRK
;				clear	DIMPTR2
				clear	GNXTNUM
				move	COMPFLD4,GNXTNUM
				move	COMPFLD4,COMPOLDBRK
				reset	GNXTNUM
				rep	zfill,GNXTNUM
				move	"Save-GNXTUPD",Location
				call	GNXTUPD
;				call	COMPOLDBRKGNXTKEY using COMP001BOLDBRK
;				move	COMP001BOLDBRK to COMPOLDBRK
;				rep	zfill,COMPOLDBRK
;				if 	(COMPOLDBRK = "0000")
;					alert caution,"You may need to add old Broker Manually",result,"Call I.S."
;				else
;
;				endif
;patch4.0
			elseif 	(COMPMLRFLG = "T")
				move	"Save-GNXTKEY",Location
				move	"NMLRNXT",GNXTFLD
				call	GNXTKEY
				call	trim using gnxtnum
				move	GNXTNUM,n4
				loop
					add	C1,n4
					move	n4,MNUM
					rep	zfill,MNUM
					pack	COMPFLD3,MNUM
					rep	zfill,COMPFLD3
					call	COMPTST3
				until over
				repeat
				clear	GNXTNUM
				move	COMPFLD3,GNXTNUM
				move	COMPFLD3,COMPOLDMLR
				reset	GNXTNUM
				rep	zfill,GNXTNUM
				move	"Save-GNXTUPD",Location
				call	GNXTUPD
;patch4.0
			endif
;patch1.04
			move	"Save-COMPWRT",Location
			call	COMPWRT
;Patch3.0
			move COMPNUM,NMTXFLD
			rep	zfill,NMTXFLD
			if ((MTXCODE <> B1 OR MTXPROFT = YES OR (MTXC501 <> B1 AND MTXC501 <> "")) AND MTXCODE <> NO)
				call	NMTXTST
				if over
					move	COMPNUM,MTXNUM
					move	"MailerUpdate-NMTXWRT",Location
					call	NMTXWRT
				endif
			endif
;Patch3.0
;NotesWrite
;patch3.0
			getitem	Comp1EditNotes,0,COMPNOTES
			if	(COMPNOTES <> "")
;patch1.04
				move	COMPNUM to COMPNOTEFLD
				move	COMPNUM to COMPNOTECOMP
				move	"Save-COMPNOTEUPD",Location
				pack	KeyLocation,"Key: ",COMPNOTEFLD
				call	COMPNOTETST
				if	 over
					move	"Save-COMPNOTEUPD",Location
					pack	KeyLocation,"Key: ",COMPNOTEFLD
					call	COMPNOTEWRT
				endif
			endif
;patch3.0 Comment Out
.			move	COMPNUM to COMPNOTECOMP
.			move	COMPNUM to COMPNOTEFLD
.			move	"LoadScreen-COMPNOTEWRT",Location
.			pack	KeyLocation,"Key: ",COMPNOTEFLD
.			call	COMPNOTEWRT
;patch3.0
;{this should not happen!!!}
		else
			alert	caution,"Duplicate Key, Cannot Add!",result,"Call IS"
			call	Comp1EnableLower
			call	CompEnableUpperButtons
			return
		endif
	endif
	setitem	CompSearchList,0,COMPNUM
	call	Click_CompOK
	return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Comp2GatherFields
	getitem	Comp2EditComp,0,CNCTCODE	;Company
.
	getitem	Comp2EditNumber,0,CNCTID	;Contact Number
.
	getitem	Comp2ComboType,0,N1		;1-mailer 2-broker 3-listowner 4-SB 5-Consultant 6-Manager

;patch 1.17	added to check if contact being created had appropriately be designated mlr,broker,listowner,etc with the company
		if (CNCTCODE <> "")
			rep zfill,CNCTCODE
			call zfillit using CNCTCODE
			call	CompCompKey using CNCTCODE,holda
			if (N1 = c1)	.Mailer
				if (holda <> "")
					unpack	holda,b55,b55,b55,b55,b10,str1
					if (str1 = "T")		;if Mailerflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a Mailer ",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			elseif	(N1=C2) .Broker
				if (holda <> "")
					unpack	holda,b55,b55,b55,b55,b10,b1,str1
					move	" " to b1
					if (str1 = "T")		;if Brokerflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a Broker",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			elseif	(N1=C3) .ListOwner
				if (holda <> "")
					unpack	holda,b55,b55,b55,b55,b10,b2,str1
					if (str1 = "T")		;if Listownerflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a ListOwner",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			elseif	(N1=C4) .ServiceBureau
				if (holda <> "")
				unpack	holda,b55,b55,b55,b55,b10,b3,str1
					if (str1 = "T")		;if ServiceBflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a Service Bureau",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			elseif	(N1=C5) .Consultant
				if (holda <> "")
					unpack	holda,b55,b55,b55,b55,b10,b4,str1
					if (str1 = "T")		;if Consultantflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a Consultant",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			elseif	(N1=C6) .Manager
				if (holda <> "")
					unpack	holda,b55,b55,b55,b55,b10,b5,str1
					if (str1 = "T")		;if Managerflg = True
						move	n1 to CNCTTYPE
					else
						alert caution,"The company for this contact must have already been designated as a Manager",Result,""
						call	Comp2DisableUpper
						call	Comp2EnableLower
						call	Comp2EnableUpper2
						return
					endif
				endif
			endif
		else
			move	n1 to CNCTTYPE
		endif

;patch1.17
.
;patch1.1
	clear	str4
	clear	str3
	getitem	Comp2EditOldComp,0,str4
	getitem	Comp2EditOldNumber,0,str3
	call	zfillit using str3
	call	zfillit using str4
	pack	CNCTCNT with str4,str3

;patch1.1
	getitem	Comp2EditFName,0,CNCTFNAME              ;FullName
	if 	(CNCTFNAME = "")
		alert caution,"Contact Needs a Name!!!!",result,"Name Required"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditFName
		return
;Patch2.0
	else
		if (NEWFLAG2 = YES)
			move CNCTCODE to str6
			clear hold2
			call CNCTCNCTNameKEY using CNCTFNAME,hold2,str6
			if (hold2 <> "")
				alert caution,"Contact already exists for this client!!!!",result,"Duplicate Name"
				call	Comp2DisableUpper
				call	Comp2EnableLower
				call	Comp2EnableUpper2
				setfocus	Comp2EditFName
				clear	hold2
				return
			endif
		endif
;patch2.0
	endif
.
	getitem	Comp2EditLName,0,CNCTLNAME              ;Last Name
.
	getitem	Comp2EditSalutation,0,CNCTSAL           ;Salutation
.
	getitem	Comp2EditTitle,0,CNCTTITLE              ;Title
.
	getitem	Comp2EditEmail,0,CNCTEMAIL              ;Email Address
.
	getitem	Comp2EditCountryCode,0,CNCTCNTRY        ;Country Code
.
	getitem	Comp2EditPhone,0,str14
;;;;
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
;patch1.15
		getitem	Comp2EditComp,0,str6
		if (str6 <> "")
			rep zfill,str6
			call zfillit using str6
			call	CompCompKey using str6,holda
			if (holda <> "")
				clear	str10
				unpack	holda,b55,b55,b55,b25,b5,str10	;CompFax
				match	str10,str14
				if equal
					Clear	CNCTPHONE
				else
					move 	str14 to CNCTPHONE
					unpack	str14,areacode,str3,str4
					pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
					setitem	Comp2EditPhone,0,str15
				endif
			else
				move 	str14 to CNCTPHONE
				unpack	str14,areacode,str3,str4
				pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
				setitem	Comp2EditPhone,0,str15
			endif
		else
			move 	str14 to CNCTPHONE
			unpack	str14,areacode,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp2EditPhone,0,str15
		endif
;patch1.15

	elseif (result = 0)
		clear	str15
		clear	CNCTPHONE
	elseif (result = 7)
 		alert type=yesno1," Is this phone number local(if yes, I will add 510 area code)?",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str14 with areacode,str3,str4
;patch1.15
			getitem	Comp2EditComp,0,str6
			if (str6 <> "")
				rep zfill,str6
				call zfillit using str6
				call	CompCompKey using str6,holda
				if (holda <> "")
					clear	str10
					unpack	holda,b55,b55,b55,b25,b5,str10	;CompFax
					match	str10,str14
					if equal
						Clear	CNCTPHONE
					else
						move 	str14 to CNCTPHONE
						unpack	str14,areacode,str3,str4
						pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
						setitem	Comp2EditPhone,0,str15
					endif
				else
					move 	str14 to CNCTPHONE
					unpack	str14,areacode,str3,str4
					pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
					setitem	Comp2EditPhone,0,str15
				endif
			else
				move 	str14 to CNCTPHONE
				unpack	str14,areacode,str3,str4
				pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
				setitem	Comp2EditPhone,0,str15
			endif
;patch1.15
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp2EditPhone,0,str15
			pack	CNCTPHONE,areacode,str3,str4
		elseif (n1=7)    . 6 = yes , 7 = no
			alert	caution,"Please add area code to phone number",result,"Area Code"
			call	Comp2DisableUpper
			call	Comp2EnableLower
			call	Comp2EnableUpper2
			setfocus	Comp2EditPhone
		                    return
		endif
                    elseif (result < 7)
		alert	caution,"Phone Number Must Be at Least Seven Digits",result,"Incomplete Phone Number"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone
                                        return
                    elseif (result > 10)
		alert	caution,"Phone Number Must Be No More than Ten Digits",result,"Phone Number too long"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone
                                        return
                    else
		alert	caution,"Please Check format of phone number",result,"Phone Number invalid"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone
                                        return
	endif
;
	getitem	Comp2EditPhone2,0,str14
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
                                        move 	str14 to CNCTPHONE1
		unpack	str14,areacode,str3,str4
		pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
		setitem	Comp2EditPhone2,0,str15
	elseif (result = 0)
		clear	str15
		clear	CNCTPHONE1
	elseif (result = 7)
 		alert type=yesno1," Is this phone number local(if yes, I will add 510 area code)?",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp2EditPhone2,0,str15
			pack	CNCTPHONE1,areacode,str3,str4
		elseif (n1=7)    . 6 = yes , 7 = no
			alert	caution,"Please add area code to phone number",result,"Area Code"
			call	Comp2DisableUpper
			call	Comp2EnableLower
			call	Comp2EnableUpper2
			setfocus	Comp2EditPhone2
			return
		endif
	elseif (result < 7)
		alert	caution,"Phone Number Must Be at Least Seven Digits",result,"Incomplete Phone Number"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone2
                                        return
	elseif (result > 10)
		alert	caution,"Phone Number Must Be No More than Ten Digits",result,"Phone Number too long"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone2
		return
	else
		alert	caution,"Please Check format of phone number",result,"Phone Number invalid"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditPhone2
		return
	endif
.
	getitem	Comp2EditFax,0,str14
  	call   	RemoveChar 	using str14,dash
  	call   	RemoveChar 	using str14,lparen
  	call   	RemoveChar 	using str14,rparen
  	call   	RemoveChar 	using str14,b1
	count	result,str14
	if (result = 10)
;patch1.15
		getitem	Comp2EditComp,0,str6
		if (str6 <> "")
			rep zfill,str6
			call zfillit using str6
			call	CompCompKey using str6,holda
			if (holda <> "")
				clear	str10
				unpack	holda,b55,b55,b55,b25,b15,str10	;CompFax
				match	str10,str14
				if equal
					clear	CNCTFAX
				else
					move 	str14 to CNCTFAX
					unpack	str14,areacode,str3,str4
					pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
					setitem	Comp2EditFax,0,str15
				endif
			else
				move 	str14 to CNCTFAX
				unpack	str14,areacode,str3,str4
				pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
				setitem	Comp2EditFax,0,str15
			endif
		else
			move 	str14 to CNCTFAX
			unpack	str14,areacode,str3,str4
			pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
			setitem	Comp2EditFax,0,str15
		endif
;patch1.15
	elseif (result = 0)
;patch1.15	;if no fax number entered assume that the company fax is to be used.
;		alert caution,"Fax Number Must Be Entered!",result,"Required Field"
		clear	str15
		clear	CNCTFAX
;patch1.07
;		call	Comp2DisableUpper
;		call	Comp2EnableLower
;		call	Comp2EnableUpper2
;		setfocus	Comp2EditFax
;		return
;patch1.15
;patch1.07
	elseif (result = 7)
		alert type=yesno1," Is this fax number local(if yes, I will add 510 area code)?",n1
		if (n1=6)    . 6 = yes , 7 = no
			move 	"510" to areacode      ;Alameda County Area Code
			unpack	str14,str3,str4
			pack	str14	with areacode,str3,str4
;			pack	CNCTFAX,areacode,str3,str4
;patch1.15
			getitem	Comp2EditComp,0,str6
			if (str6 <> "")
				rep zfill,str6
				call zfillit using str6
				call	CompCompKey using str6,holda
				if (holda <> "")
					clear	str10
					unpack	holda,b55,b55,b55,b25,b15,str10	;CompFax
					match	str10,str14
					if equal
						clear	CNCTFAX
					else
						pack	CNCTFAX,areacode,str3,str4
						pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
						setitem	Comp2EditFax,0,str15
					endif
				else
					pack	CNCTFAX,areacode,str3,str4
					pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
					setitem	Comp2EditFax,0,str15
				endif
			else
				pack	CNCTFAX,areacode,str3,str4
				pack	str15,lparen,areacode,rparen,b1,str3,DASH,str4
				setitem	Comp2EditFax,0,str15
			endif

;patch1.15

		elseif (n1=7)    . 6 = yes , 7 = no
			alert	caution,"Please add area code to fax number",result,"Area Code"
			call	Comp2DisableUpper
			call	Comp2EnableLower
			call	Comp2EnableUpper2
			setfocus	Comp2EditFax
			return
		endif
	elseif (result < 7)
		alert	caution,"Fax Number Must Be at Least Seven Digits",result,"Incomplete Fax Number"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditFax
		return
	elseif (result > 10)
		alert	caution,"Fax Number Must Be No More than Ten Digits",result,"Fax Number too long"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditFax
		return
	else
		alert	caution,"Please Check format of fax number",result,"Fax Number invalid"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2EditFax
		return
	endif
;
;patch1.05
                    clear 	n2
	getitem	Comp2ComboSalesPerson,N1,N2	;Internal Contact Information
	getitem	Comp2ComboSalesPerson,N2,str27	;Internal Contact Information
	unpack	str27,str25,str2
	if (str2 = "" |	str2 = " " | str2 = "  ")		;Did not pick a contact
		alert	caution,"Need a Salesperson for this contact",result,"Salesperson Needed"
		call	Comp2DisableUpper
		call	Comp2EnableLower
		call	Comp2EnableUpper2
		setfocus	Comp2ComboSalesPerson
		return
	else
		move	n2 to CNCTSALES
		rep	zfill,cnctsales
	endif
;patch1.05
	getitem	Comp2CheckInactive,0,N1
	if (N1 = C1)
		move	"T",CNCTINACTIVE
	else
		move	"F",CNCTINACTIVE
	endif
.
	getitem	Comp2CheckHidden,0,N1
	if (N1 = C1)
		move	"T",CNCTORDDISP
	else
		move	"F",CNCTORDDISP
	endif
.
	getitem	Comp2CheckWeekly,0,N1
	if (N1 = C1)
		move	"T",CNCTPWKLYFLG
	else
		move	"F",CNCTPWKLYFLG
	endif
.
	getitem	Comp2CheckPromo,0,N1
	if (N1 = C1)
		move	"T",CNCTPROMOFLG
	else
		move	"F",CNCTPROMOFLG
	endif
;
	getitem	Comp2CheckBroker,0,N1
	if (N1 = C1)
		move	"T",CNCTPBRKFLG
	else
		move	"F",CNCTPBRKFLG
	endif
;.
	getitem	Comp2CheckClient,0,N1
	if (N1 = C1)
		move	"T",CNCTPCLNTFLG
	else
		move	"F",CNCTPCLNTFLG
	endif
.
	getitem	Comp2CheckParty,0,N1
	if (N1 = C1)
		move	"T",CNCTPPRTYFLG
	else
		move	"F",CNCTPPRTYFLG
	endif
;.
	getitem	Comp2CheckDMA,0,N1
	if (N1 = C1)
		move	"T",CNCTPDMAFLG
	else
		move	"F",CNCTPDMAFLG
	endif
;.
	getitem	Comp2CheckDatacard,0,N1
	if (N1 = C1)
		move	"T",CNCTPDTAFLG
	else
		move	"F",CNCTPDTAFLG
	endif
;
	getitem	Comp2CheckHoliday,0,N1
	if (N1 = C1)
		move	"T",CNCTHLYDYFLG
	else
		move	"F",CNCTHLYDYFLG
	endif
.
	getitem	Comp2CheckNewsletter,0,N1
	if (N1 = C1)
		move	"T",CNCTMRKTNEWS
	else
		move	"F",CNCTMRKTNEWS
	endif
.
;
	if (NEWFLAG2 = YES)
		move	NPASUSER,CNCTUSER
		clock	timestamp,str8
		move	str8 to CNCTDATE
;		pack	taskname,B55,B55,B55
		move	b1 to taskname
		clear	taskname
		call	Trim using CNCTUSER
		call	Trim using CNCTDATE
		if (CNCTDATE <> "" AND CNCTUSER <> "")
			append	"Record Created ",taskname
			if (CNCTDATE <> "")
				unpack	CNCTDATE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,B1
				append	str15,taskname
			endif
			if (CNCTUSER <> "")
				append	"by ",taskname
				append	CNCTUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp2StatCreate,0,taskname
		move	NPASUSER,CNCTUSER2
		clock	timestamp,str8
		move	str8 to CNCTDATE2
;		pack	taskname,B55,B55,B55
		move	b1 to taskname
		clear	taskname
		call	Trim using CNCTUSER2
		call	Trim using CNCTDATE2
		if (CNCTDATE2 <> "" AND CNCTUSER2 <> "")
			append	"Record Modified ",taskname
			if (CNCTDATE2 <> "")
				unpack	COMPRDTE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,B1
				append	str15,taskname
			endif
			if (CNCTUSER2 <> "")
				append	"by ",taskname
				append	COMPRUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp2StatRevision,0,taskname
	else
.Revision Fields
		getitem	Comp2EditComp,0,str6	;Company
		getitem	Comp2EditNumber,0,str3	;Contact Number
		pack	str9,str6,str3
		call	CnctCnctKey using str9,hold2
		clear	str9
		clear	str8
		unpack	hold2,b55,b55,b55,b55,b25,b7,str8,str9
			move "       ",b7
		move	str8 to CNCTUSER
		if	(CNCTUSER = b8)
			move "Unknown" to CNCTUSER
		endif
		clear	str8
		move	str9 to str8
		move	str8 to CNCTDATE
		if	(CNCTDATE = b8)
			move "00000000" to CNCTDATE
		endif
		clear	taskname
		move	b1,taskname
		call	Trim using CNCTUSER
		call	Trim using CNCTDATE
		if (COMPDTE <> "" AND CNCTUSER <> "")
			append	"Record Created ",taskname
			if (CNCTDATE <> "")
				unpack	CNCTDATE,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,B1
				append	str15,taskname
			endif
			if (CNCTUSER <> "")
				append	"by ",taskname
				append	CNCTUSER,taskname
			endif
		endif
		reset	taskname
		setitem	Comp2StatCreate,0,taskname
		move	NPASUSER,CNCTUSER2
		clock	timestamp,str8
		move	str8 to CNCTDATE2
;		pack	taskname,B55,B55,B55
		move	b1 to taskname
		clear	taskname
		call	Trim using CNCTUSER2
		call	Trim using CNCTDATE2
		if (CNCTDATE2 <> "" AND CNCTUSER2<> "")
			append	"Record Modified ",taskname
			if (CNCTDATE2 <> "")
				unpack	CNCTDATE2,CC,YY,MM,DD
				pack	str15,MM,SLASH,DD,SLASH,CC,YY,B1
				append	str15,taskname
			endif
			if (CNCTUSER2 <> "")
				append	"by ",taskname
				append	CNCTUSER2,taskname
			endif
		endif
		reset	taskname
		setitem	Comp2StatRevision,0,taskname
	endif


Comp2Write
	if (NEWFLAG2 = NO)
		pack	CNCTFLD,CNCTCODE,CNCTID
		call	zfillit,CNCTFLD
		move	"Comp2Write-CNCTKEY",Location
		pack	KeyLocation,"Key: ",CNCTFLD
		call	CNCTTST
		if not over
			call	CNCTUPD
			call	Comp2LoadListView
			call	Comp2OKEndOfRead
		else
			alert	caution,"Error Modifying Record",result,"Call IS - Could not find record"
			call	Comp2EnableLower
			call	Comp2EnableUpper2
			return
		endif
	else
		clear	str3
		move	C0 TO N3
		move	N3 to str3
		rep	zfill,str3
;		call	zfillit using str3
		pack	CNCTFLD,CNCTCODE,str3
		call	zfillit using CNCTFLD
		loop
			call	CNCTTST
		until over
			add	C1,N3
			move	N3 to str3
			rep	zfill,str3
;			call	zfillit using str3
			pack	CNCTFLD,CNCTCODE,str3
		repeat
		unpack	CNCTFLD,b6,str3
		move	str3 to CNCTID
		call	zfillit using CNCTID
;Test for Duplicates first
		pack	CNCTFLD,CNCTCODE,CNCTID
		move	"Save-CNCTTST",Location
		call	CNCTTST
		if over
;patch1.04
;STARTPATCH 1.06
			if ((CNCTTYPE = "2")&(COMPBRKFLG = "T")|(CNCTTYPE = "5")&(COMPCLRFLG = "T"))
;ENDPATCH 1.06
				clear	str4
				clear	str3
				move	c0 to n3
				getitem	Comp2EditOldComp,0,str4
				loop
;					getitem	Comp2EditOldNumber,0,str3
;					call	zfillit using str3
					call	zfillit using str4
					rep 	zfill using str4
					move	n3 to str3
					rep 	zfill using str3
					pack	CNCTFLD4 with str4,str3
					call	CNCTTST2
				until over
					add	c1 to n3
				repeat
.patch1.8
		      Clear NORDFLD1
   		   Clear NORDFLD2
      		Clear NORDFLD3
	      	Clear NORDFLD4
	   	   PACK  NORDFLD4 FROM "04L",str4
   	   	MOVE  C2 TO NORDPATH
	   	   CALL  NORDAIM
   	   	GOTO  WriteContact if over
	      	MATCH OBRKCNT,str3
		      GOTO  TryAnotherContact if equal
				loop
					call   NORDKG
				until	over
			      MATCH  OBRKCNT,str3
			      if equal
						loop
TryAnotherContact
							add	c1 to n3
							move	n3 to str3
							rep 	zfill using str3
							pack	CNCTFLD4 with str4,str3
							call	CNCTTST2
						until over
						repeat
			   	   CALL  NORDAIM
		 			endif
				repeat
WriteContact
.Patch1.8
				pack	CNCTCNT from CNCTFLD4
;STARTPATCH 1.06
				move	"Save-CNCTWRT",Location
				call	CNCTWRT
				call	Comp2LoadListView
				call	Comp2OKEndOfRead
;ENDPATCH 1.06
;Patch3.0
			elseif (CNCTTYPE = "1" & COMPMLRFLG = "T")

				clear cnctcnt
				packkey cnctcnt,cnctcnt
;STARTPATCH 1.06
				move	"Save-CNCTWRT",Location
				call	CNCTWRT
				call	Comp2LoadListView
				call	Comp2OKEndOfRead
			endif
;Patch3.0
.			endif
;patch1.04

;{this should not happen!!!}
		else
			alert	caution,"Duplicate Key, Cannot Add!",result,"Call IS"
			call	Comp2EnableLower
			call	Comp2EnableUpper2
			return
		endif
	endif
	return
Comp2LoadListView
.Contacts
.SCREEN 2
	Comp1ListViewCnt.DeleteAllItems giving N9
	Comp2ListView.DeleteAllItems giving N9
;patch1.02
	getitem	Comp1EditNumber,0,str6
;patch1.02
	clear	CNCTFLD3
	pack	CNCTFLD2,"01X",str6
	move	"Comp2LoadListView-CNTAIM",Location
	pack	KeyLocation,"Key: ",CNCTFLD2
	call	CNCTAIM
	loop
		until over
.Contact Fields on Screen 1
		Comp1ListViewCnt.InsertItem giving N9 using CNCTID
		Comp1ListViewCnt.SetItemText using N9,CNCTFNAME,1
		Comp1ListViewCnt.SetItemText using N9,CNCTID,2
		if (CNCTTYPE = "1")
			move	"Mailer",str15
		elseif (CNCTTYPE = "2")
			move	"Broker",str15
		elseif (CNCTTYPE = "3")
			move	"List Owner",str15
		elseif (CNCTTYPE = "4")
			move	"Service Bureau",str15
		elseif (CNCTTYPE = "5")
			move	"Consultant",str15
		elseif (CNCTTYPE = "6")
			move	"Manager",str15
		else
			clear	str15
		endif
		Comp1ListViewCnt.SetItemText using N9,str15,3
.Contact Fields on Screen 2
		pack	hold2,CNCTVARS
		Comp2ListView.InsertItem giving N9 using CNCTID
		Comp2ListView.SetItemText using N9,CNCTCODE,1
		Comp2ListView.SetItemText using N9,CNCTID,2
		Comp2ListView.SetItemText using N9,str15,3
		Comp2ListView.SetItemText using N9,CNCTFNAME,4
		Comp2ListView.SetItemText using N9,hold2,5
		call	CNCTKG
	repeat
	Comp1ListViewCnt.GetItemCount giving result
	if (result > 0)
		Comp1ListViewCnt.SetItemState giving N9 using 0,2,2
		call	Click_Comp1ListViewCnt
	else
		setitem	Comp1StatCnt,0,""
	endif
	Comp2ListView.GetItemCount giving result
;patch1.05
	if (result > 0)
		Comp2ListView.SetItemState giving N9 using 0,2,2
		call	Click_Comp2ListView
	else
		call	Comp2ClearScreen
	endif
;patch1.05


	return
EnableOK
	if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")
		setprop	CompOK,enabled=1
		setprop	CompListViewSearch,enabled=1
		return
	else
		return
	endif
CLearAllScreens
	call	Comp1ClearScreen
	call	Comp2ClearScreen
	Comp2ListView.DeleteAllItems giving N9
	Comp3WebBrowser.Navigate2 USING "about:blank"
	call	Comp4ClearScreen
	call	Comp4ClearCategory
	call	Comp4ClearOffer
	return
;Routine Called during save button of screen 2
;patch1.2

;This Routine is called by CompSearchForm
CompSearchLoad
;	Branch	CompSearchFlag,CompSearchComp,CompSearchCnt,CompSearchCompCnt

;CompSearchComp
;	if (ExitFlag = "N" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
	if (CCB = c1)	;Company
		if (ExitFlag = "N")      ;if in modify mode  must me for parent
			Getitem	CompTabControl001,0,n2
			if (n2 = c1)    ;if focus on first page
				getprop	Comp1EditParent,enabled=n1
				if (n1 = c1)     ;if edit parent is enabled
					Unpack	CompSearchString,str6,str55,str3,str45
					setitem	Comp1EditParent,0,str6
					setitem	Comp1StatParentName,0,str55
.patch1.5
					setfocus	Comp1EditParent
.patch1.5
					return
				endif
			endif
			Return
;Else if nothing is being modified must be a initial company search
		elseif (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
			Unpack	CompSearchString,str6,str55,str3,str45
			setitem	CompSearchList,0,str6
			call	Click_CompOK
			Getitem	CompTabControl001,0,n2
			Call	CompTabClick
			move	C1 to N2
			CALL	CompTabChange
			Setitem	CompTabControl001,0,c1
			RETURN
		endif
	elseif (CCB = c2)	;Contact
		if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
			Unpack	CompSearchString,str6,str55,b1,str3,str45	;CompanyNumber,Co Name,Cnt Num,Cnt Name
			move	" ",b1
			setitem	CompSearchList,0,str6
			call	Click_CompOK				;Call Up new Company
			Comp2ListView.GetItemCount giving result
			Unpack	CompSearchString,str6,str55,str3,str45
			move	str3 to n3
			if (result > 0)				        ;If there is a valid record in LV switch to cnt pg and highlight owner
				Getitem	CompTabControl001,0,n2
				Call	CompTabClick
				move	C2 to N2
				CALL	CompTabCHange
				Setitem	CompTabControl001,0,c2
				Comp2ListView.SetItemState giving N9 using n3,2,2
				Comp2ListView.EnsureVisible	USING n3,0
				Comp1ListViewCnt.SetItemState giving n9 using n3,2,2
				Comp1ListViewCnt.EnsureVisible	giving result USING n3,0
				call	Click_Comp2ListView
				Return
			endif
		else
			alert caution,"Cannot Search For Company/Contact While modifying",result,""
			Return
		endif
.Patch1.5
	elseif (CCB = c3)	;Company & Contact
		if (ExitFlag = "N")      ;if in modify mode must me for parent
			Getitem	CompTabControl001,0,n2
			if (n2 = c1)    ;if focus on first page
				getprop	Comp1EditParent,enabled=n1
				if (n1 = c1)     ;if edit parent is enabled
					Unpack	CompSearchString,str6,str55,str3,str45
					setitem	Comp1EditParent,0,str6
					setitem	Comp1StatParentName,0,str55
					setfocus	Comp1EditParent
					return
				endif
			endif
		Elseif (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
			Unpack	CompSearchString,str6,str55,b1,str3,str45	;CompanyNumber,Co Name,Cnt Num,Cnt Name
			move	" ",b1
			Call trim using str3
			If (Str3 = "")	;if str3 is empty they must be looking for company
				setitem	CompSearchList,0,str6
				call	Click_CompOK
				Getitem	CompTabControl001,0,n2
				Call	CompTabClick
				move	C1 to N2
				CALL	CompTabChange
				Setitem	CompTabControl001,0,c1
				RETURN
			else
				setitem	CompSearchList,0,str6	          .Looking for contact i presume
				call	Click_CompOK				;Call Up new Company
				Comp2ListView.GetItemCount giving result
				Unpack	CompSearchString,str6,str55,str3,str45
				move	str3 to n3
				if (result > 0)				        ;If there is a valid record in LV switch to cnt pg and highlight owner
					Getitem	CompTabControl001,0,n2
					Call	CompTabClick
					move	C2 to N2
					CALL	CompTabCHange
					Setitem	CompTabControl001,0,c2
					Comp2ListView.SetItemState giving N9 using n3,2,2
					Comp2ListView.EnsureVisible	USING n3,0
					Comp1ListViewCnt.SetItemState giving n9 using n3,2,2
					Comp1ListViewCnt.EnsureVisible	giving result USING n3,0
					call	Click_Comp2ListView
					Return
				Endif
			Endif
		else
			alert caution,"Cannot Search For Contact\Company While modifying",result,""
			Return
		Endif
.Patch1.5
	endif
	Return

;Patch3.0
MailerTaxUpdate
	move COMPNUM,NMTXFLD
	rep	zfill,NMTXFLD
	if ((MTXCODE <> B1 OR MTXPROFT = YES OR (MTXC501 <> B1 AND MTXC501 <> "")) AND MTXCODE <> NO)
		call	NMTXTST
		if over
			move	COMPNUM,MTXNUM
			move	"MailerUpdate-NMTXWRT",Location
			call	NMTXWRT
		else
			move	"MailerUpdate-NMTXUPD",Location
			call	NMTXUPD
		endif
	else
		call	NMTXTST
		if not over
			move	"MailerUpdate-NMTXDEL",Location
			call	NMTXDEL
		endif
	endif
	return
;Patch3.0

;Patch3.0
Comp1EnableLowerMailer
	setprop	Comp1CheckBillDirect,enabled=1
	setprop	Comp1CheckExchange,enabled=1
	setprop	Comp1CheckFaxMailer,enabled=1
	setprop	Comp1CheckForProfit,enabled=1
	setprop	Comp1CheckRegional,enabled=1
	setprop	Comp1CheckStatements,enabled=1
	setprop	Comp1CheckUsage,enabled=1
	setprop	Comp1ComboAccount,enabled=1,bgcolor=white
	setprop	Comp1ComboBillCode,enabled=1,bgcolor=white
	setprop	Comp1ComboExempt,enabled=1,bgcolor=white
	setprop	Comp1Edit501C,enabled=1,bgcolor=white
	setprop	Comp1EditAccountYear,enabled=1,bgcolor=white
	setprop	Comp1EditBroker,enabled=1,bgcolor=white
	setprop	Comp1EditBrokerCnt,enabled=1,bgcolor=white
	setprop	Comp1EditConsultant,enabled=1,bgcolor=white
	setprop	Comp1EditConsultantCnt,enabled=1,bgcolor=white
	setprop	Comp1EditInvoice,enabled=1,bgcolor=white
	setprop	Comp1EditInvoiceCnt,enabled=1,bgcolor=white
	setprop	Comp1EditPermit,enabled=1,bgcolor=white
	return
Comp1DisableLowerMailer
	setprop	Comp1CheckBillDirect,enabled=0
	setprop	Comp1CheckDiscount,enabled=0
	setprop	Comp1CheckExchange,enabled=0
	setprop	Comp1CheckFaxMailer,enabled=0
	setprop	Comp1CheckForProfit,enabled=0
	setprop	Comp1CheckRegional,enabled=0
	setprop	Comp1CheckStatements,enabled=0
	setprop	Comp1CheckUsage,enabled=0
	setprop	Comp1ComboAccount,enabled=0,bgcolor=grey
	setprop	Comp1ComboBillCode,enabled=0,bgcolor=grey
	setprop	Comp1ComboExempt,enabled=0,bgcolor=grey
	setprop	Comp1Edit501C,enabled=0,bgcolor=grey
	setprop	Comp1EditAccountYear,enabled=0,bgcolor=grey
	setprop	Comp1EditBroker,enabled=0,bgcolor=grey
	setprop	Comp1EditBrokerCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditConsultant,enabled=0,bgcolor=grey
	setprop	Comp1EditConsultantCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditInvoice,enabled=0,bgcolor=grey
	setprop	Comp1EditInvoiceCnt,enabled=0,bgcolor=grey
	setprop	Comp1EditPermit,enabled=0,bgcolor=grey
	return
Comp1EnableLowerBroker
	setprop	Comp1CheckFaxBroker,enabled=1
	setprop	Comp1CheckGuarantees,enabled=1
	return
Comp1DisableLowerBroker
	setprop	Comp1CheckFaxBroker,enabled=0
	setprop	Comp1CheckGuarantees,enabled=0
	return

;Patch3.0
Comp4VerifyOffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if (NewFlag4Offer = YES)
		Call ComptoOldMlr1	using COMPHOLD4,OLDMLR
		move	COMPHOLD4 to OFMLR
		CALL ZFILLIT USING OFMLR
		clear n3
		loop
			add c1 to n3
			move n3 to str3
			move str3 to ofnum
			rep zfill,OFNUM
			call zfillit using ofnum
			CLEAR     NOFRFLD
.			PACK      NOFRFLD FROM OFMLR,OFNUM
			PACK      NOFRFLD FROM OLDMLR,OFNUM
			REP       ZFILL,NOFRFLD
			call zfillit	using nofrfld
			CALL      NOFRTST
		until over
		repeat
		MOVE	"R" TO OFCODE
		MOVE	NPASUSER TO OFNAME
		clock	timestamp,str8
		move	str8 to OFDATE
		getitem	Comp4EditOfferNameOffer,0,OFDESC
		call	Trim Using OFDESC
		CALL	NOFRWRT
	else
		MOVE	"R" TO OFCODE
		clear	OLDMLR
		getitem	Comp4EditCompOffer,0,OFMLR
		Call ComptoOldMlr1	using OFMLR,OLDMLR
.		getitem	Comp4EditNumberOffer,0,Comp4EditNumberOffer
		getitem	Comp4EditOfferOffer,0,OFNUM
		rep zfill,OFNUM
		call zfillit using ofnum
		getitem	Comp4EditOfferNameOffer,0,OFDESC
		call	Trim Using OFDESC
		MOVE	NPASUSER TO OFNAME
		clock	timestamp,str8
		move	str8 to OFDATE
		CLEAR	NOFRFLD
.		PACK	NOFRFLD FROM OFMLR,OFNUM
		PACK	NOFRFLD FROM OLDMLR,OFNUM
		call zfillit	using nofrfld
		CALL	NOFRTST
		if not over
			CALL	NOFRUPD
		endif
	endif
	return

;;
.	Getitem	CompTabControl001,0,n2
.		if (n2 = c1)    ;if focus on first page
.			if (ExitFlag = "N")      ;if in modify mode
.				getprop	Comp1EditParent,enabled=n1
.				if (n1 = c1)     ;if edit parent is enabled
.					Unpack	CompSearchString,str6,str55,str3,str45
.					setitem	Comp1EditParent,0,str6
.					setitem	Comp1StatParentName,0,str55
.				endif
.			else
.				if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
.					Unpack	CompSearchString,str6,str55,str3,str45
.					setitem	CompSearchList,0,str6
.					call	Click_CompOK
.				else
.					alert caution,"Cannot Search For Contact While modifying",result,""
.				endif
.			endif
.		else
.			if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
.				Unpack	CompSearchString,str6,str55,b1,str3,str45	;CompanyNumber,Co Name,Cnt Num,Cnt Name
.				setitem	CompSearchList,0,str6
.				call	Click_CompOK				;Call Up new Company
.				Comp2ListView.GetItemCount giving result
.				Unpack	CompSearchString,str6,str55,str3,str45
.				move	str3 to n3
.				if (result > 0)				        ;If there is a valid record in LV switch to cnt pg and highlight owner
.					Getitem	CompTabControl001,0,n2
.					Call	CompTabClick
.					move	C2 to N2
.					CALL	CompTabCHange
.					Setitem	CompTabControl001,0,c2
.					Comp2ListView.SetItemState giving N9 using n3,2,2
.					Comp2ListView.EnsureVisible	USING n3,0
.					Comp1ListViewCnt.SetItemState giving n9 using n3,2,2
.					Comp1ListViewCnt.EnsureVisible	giving result USING n3,0
.					call	Click_Comp2ListView
.				endif
.			else
.				alert caution,"Cannot Search For Contact While modifying",result,""
.			endif
.		endif
.		RETURN

;		Unpack	CompSearchString,str6,str55,str3,str45
;		setitem	Comp1EditParent,0,str6
;		setitem	Comp1StatParentName,0,str55
;;		call	Click_CompOK
;		Getitem	CompTabControl001,0,n2
;		Call	CompTabClick
;		move	C1 to N2
;		CALL	CompTabCHange
;		Setitem	CompTabControl001,0,c1
;		RETURN
;	endif
;CompSearchCnt
;	if (ExitFlag = "Y" AND ExitFlag2 = "Y" AND ExitFlag4A = "Y" AND ExitFlag4B = "Y")	;If nothing is Being Modified
;		Unpack	CompSearchStr,str6,str55,b1,str3,str45	;CompanyNumber,Co Name,Cnt Num,Cnt Name
;		setitem	CompSearchList,0,str6
;		call	Click_CompOK				;Call Up new Company
;		Comp2ListView.GetItemCount giving result
;		Unpack	CompSearchStr,str6,str55,str3,str45
;		move	str3 to n3
;		if (result > 0)				        ;If there is a valid record in LV switch to cnt pg and highlight owner
;			Getitem	CompTabControl001,0,n2
;			Call	CompTabClick
;			move	C2 to N2
;			CALL	CompTabCHange
;			Setitem	CompTabControl001,0,c2
;			Comp2ListView.SetItemState giving N9 using n3,2,2
;			Comp2ListView.EnsureVisible	USING n3,0
;			Comp1ListViewCnt.SetItemState giving n9 using n3,2,2
;			Comp1ListViewCnt.EnsureVisible	giving result USING n3,0
;			call	Click_Comp2ListView
;		endif
;	else
;		alert caution,"Cannot Search For Contact While modifying",result,""
;	endif
;	RETURN
;ECompSearchCompCnt
;	RETURN
;patch1.2
;	include	compio.inc
;	include	f:\library\develop\cntio.inc
	include	nownio.inc
	include	nsmpio.inc
.	include	nmlrio.inc
	include  npasio.inc
	include	npayio.inc
	include	nuseio.inc
.Patch1.4
	include	ncntio.inc
.Patch1.4
	include	cntio.inc
	include  nmtxio.inc
	include	ndatio.inc
	include	nxrfio.inc
	include	gnxtio.inc
;patch conversion
.	include	nbrkio.inc
;patchconversion
	include	compnotesio.inc
.patch1.8
	include	nordio.inc
.endpatch1.8
	include	nofrio.inc
	include	compio.inc
	include	comlogic.inc

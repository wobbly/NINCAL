	include	common.inc
	include	cons.inc

release	init	"1.1"	ASH	29AUG2005	Added logic to thwart inadvertant calls to SpellCheck.  Work Orders: 398, 922
.release	init	"1.0"

.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OFALSE  variant
        create  OFALSE,VarType=VT_BOOL,VarValue=0

wdWindowStateMinimize integer 4,"0x00000002"
wdDoNotSaveChanges integer 4,"0x00000000"
.
WordObj	  Automation
DocsObj	  Automation
DocObj	  Automation
RangeObj  Automation

EditPtr EditText        ^
DimPtr  Dim             ^
FrmPtr	Form		^
WdCharacterCase integer	4

SpellCheck Routine EditPtr,DimPtr
.START PATCH 1.1 ADDED LOGIC
	pack	taskname,"Invoking Spell Check may take a few seconds.",newline,"Are you sure you wish to continue?"
	alert	plain,taskname,result
	if (result <> 1)
		return
	endif
.END PATCH 1.1 ADDED LOGIC
.Extract the text to spell check
        getitem	EditPtr,0,DimPtr
	call	Trim using DimPtr
	if (DimPtr <> "")
.Create your background instance of Word
		create	WordObj,Class="!Word.Application"
		setprop	WordObj,*WindowState=wdWindowStateMinimize
		setprop WordObj.Options,*IgnoreUppercase=OFALSE
		getProp WordObj,*Documents=DocsObj
		DocsObj.Add Giving DocObj
		DocObj.Activate
		getProp DocObj,*Content=RangeObj
.Set text in Range object AND spell check it!
		setProp RangeObj,*Text=DimPtr
		RangeObj.CheckSpelling
.Take back spell checked text and cut off funky control character that Word adds to it
		getProp RangeObj,*Text=DimPtr
		movelptr DimPtr,N9
		sub	C1,N9
		setlptr	DimPtr,N9
.Place text back in Edit Text
		setitem	EditPtr,0,DimPtr
.Destroy all objects, preventing any erroneous message boxes - including SaveAs dialog box
	        setprop WordObj,*DisplayAlerts=OFALSE
		destroy DocsObj
		destroy DocObj
		destroy RangeObj
        	WordObj.quit Using *SaveChanges=wdDoNotSaveChanges
	        destroy WordObj
        endif
        return

CaseChange Routine EditPtr,DimPtr,FrmPtr
.Extract the text to spell check
        getitem	EditPtr,0,DimPtr
	call	Trim using DimPtr
	if (DimPtr <> "")
		move	FrmPtr,WdCharacterCase
.Create your background instance of Word
		create	WordObj,Class="!Word.Application"
		setprop	WordObj,*WindowState=wdWindowStateMinimize
.		setprop WordObj.Options,*IgnoreUppercase=OFALSE
		getprop WordObj,*Documents=DocsObj
		DocsObj.Add Giving DocObj
		DocObj.Activate
		getprop DocObj,*Content=RangeObj
.Set text in Range object AND spell check it!
		setprop RangeObj,*Text=DimPtr
		setprop RangeObj,*Case=WdCharacterCase
.Take back spell checked text and cut off funky control character that Word adds to it
		getprop RangeObj,*Text=DimPtr
		movelptr DimPtr,N9
		sub	C1,N9
		setlptr	DimPtr,N9
.Place text back in Edit Text
		setitem	EditPtr,0,DimPtr
.Destroy all objects, preventing any erroneous message boxes - including SaveAs dialog box
	        setprop WordObj,*DisplayAlerts=OFALSE
		destroy DocsObj
		destroy DocObj
		destroy RangeObj
        	WordObj.quit Using *SaveChanges=wdDoNotSaveChanges
	        destroy WordObj
        endif
        return
	include	comlogic.inc
.
. Code to spell check an EditText Box using MSWord and OLE.
. 
. 
. 
. OLE is saving me loads of time and extending our capabilities.
. 
. 
. 
. Thanks Sunbelt!
. 
. 
. 
. Andrew Harkins
. 
. Names In The News, CA
. 
. 
. 
...................................
WordObj	Automation
DocsObj	Automation
DocObj	Automation
RangeObj Automation

str100	dim	1000
C1      form    "1"
N9      form    9
form	plform	"spellchk2.plf"
	formload	form

.                             PL/B does not have a Boolean datatype, so we
.                             have to create our own.
VT_BOOL EQU 11
OFALSE  variant
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.
.                                            Extract the text to spell check
	loop
		waitevent
	repeat
	stop
				
spell	
	getitem	EditText001,0,str100
	if (str100 <> "")
.                                            Create your background instance
.                                            of Word
           create    WordObj,Class="Word.Application"
           getProp   WordObj,*Documents=DocsObj
           DocsObj.Add Giving DocObj
           DocObj.Activate
           getProp   DocObj,*Content=RangeObj
.                                            Set text in Range object AND 
.                                            spell check it!
           setProp   RangeObj,*Text=str100
           RangeObj.CheckSpelling
.                                            Take back spell checked text and
.                                            cut off funky control character
.                                            that Word adds to it possibly an eof?
           getProp   RangeObj,*Text=str100
           movelptr  str100,N9
           sub       C1,N9
           setlptr   str100,N9
.                                            Place text back in Edit Text
           setitem   EditText001,0,str100
.                                            Destroy all objects, preventing
.                                            any erroneous message boxes.
           setprop   WordObj,*DisplayAlerts=OFALSE
           destroy   DocsObj
           destroy   DocObj
           destroy   RangeObj
.                                            Cheating here and using OFALSE for
.                                            wdDoNotSaveChanges, which has same
.                                            VarValue of "0".
           WordObj.quit Using *SaveChanges=OFALSE
           destroy   WordObj
	endif
	return
.

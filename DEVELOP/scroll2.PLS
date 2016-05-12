	include	common.inc
	include	cons.inc

release	init	"1.0"

VSCROLL1	VSCROLLBAR	
FONT1	FONT	
STEXT1	STATTEXT	
STEXT2	STATTEXT	
BUTTON1	BUTTON	
RETVAL	FORM	9
VALUE	FORM	1
STRING	DIM	60
MEANING	DIM	40(7),("Line Down"):
  ("Line Up"),("Page Down"):
  ("Page Up"),("Slide box moved"):
  ("Ignored - Less than {min} value"):
  ("Ignored - Greater than {max} value")
*		
x	plform	scroll2
.scrolla	plform	scrolla
	winhide
	formload	x
.	formload	scrolla,scroll

.Create the Objects
.		
 CREATE	scroll2;VSCROLL1=20:100:200:210,0,100,10
 CREATE	FONT1,"Arial",SIZE=10
 CREATE	scroll2;STEXT1=90:130:300:500,"",FONT1
 CREATE	scroll2;STEXT2=130:170:300:800,"",FONT1
 CREATE	scroll2;BUTTON1=170:200:350:420,"Exit"
*		
.Activate the Objects
.		
 ACTIVATE	VSCROLL1,STATUS,RETVAL
 ACTIVATE	STEXT1
 ACTIVATE	STEXT2
 ACTIVATE	BUTTON1,EXIT,N1
*		


	loop
		waitevent
	repeat

EXIT
	STOP
	RETURN
.Scroll Bar Activated
.		
STATUS		
 UNPACK	RETVAL INTO VALUE,N8
 PACK	STRING WITH "Current Position: ",N8
 SETITEM	STEXT1,0,STRING
.		
 PACK	STRING WITH "Status Code: ",VALUE:
  " - ",MEANING(VALUE)
 SETITEM	STEXT2,0,STRING
.		
 RETURN	
	include	comlogic.inc


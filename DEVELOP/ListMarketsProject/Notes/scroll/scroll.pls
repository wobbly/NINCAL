VSCROLL1        VSCROLLBAR
FONT1   FONT
STEXT1  STATTEXT
STEXT2  STATTEXT
BUTTON1 BUTTON
RESULT  FORM    1
RETVAL  FORM    9
VALUE   FORM    1
POSITION        FORM    8
STRING  DIM     60
MEANING DIM     40(7),("Line Down"),("Line Up"),("Page Down"):
                  ("Page Up"),("Slide box moved"):
                  ("Ignored - Less than {min} value"):
                  ("Ignored - Greater than {max} value")

*
.Create the Objects
.
        CREATE  VSCROLL1=2:10:20:210,0,100,10
        CREATE  FONT1,"Arial",SIZE=10
        CREATE  STEXT1=9:10:30:50,"",FONT1
        CREATE  STEXT2=11:12:30:80,"",FONT1
        CREATE  BUTTON1=16:17:35:42,"Exit"
*
.Activate the Objects
.
        ACTIVATE        VSCROLL1,STATUS,RETVAL
        ACTIVATE        STEXT1
        ACTIVATE        STEXT2
        ACTIVATE        BUTTON1,EXIT,RESULT
*
.Wait for an Event to Occur
.
        LOOP

  WAITEVENT
        REPEAT
*
.Scroll Bar Activated
.
STATUS
        UNPACK  RETVAL INTO VALUE,POSITION
        PACK    STRING WITH "Current Position: ",POSITION
        SETITEM STEXT1,0,STRING
.
        PACK    STRING WITH "Status Code: ",VALUE:
                  " - ",MEANING(VALUE)
        SETITEM STEXT2,0,STRING
.
        RETURN
*
.Exit Selected
.
EXIT
        STOP

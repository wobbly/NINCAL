*
.TabControl Demonstration Program
.
.First Define the Forms.  The forms for	the labels are defined as Object Only
. and must preceed the definition of the main form.  This is because the 
. main form references the label forms and the compiler	does not allow forward
. references.
.
LAB1	 PLFORM	   LABEL1
LAB2	 PLFORM	   LABEL2
MAIN	 PLFORM	   MAIN
*
.Load the Forms.  The forms for	the labels are loaded specifying the main
. form's Object	Name.
.
	 FORMLOAD  MAIN
	 FORMLOAD  LAB1,MainForm
	 FORMLOAD  LAB2,MainForm
*
.Wait for an Event to Occur.  Examine the code in the main form	under the
. TabControl001	object and the Click and Change	Events.	
.
	 LOOP
	 WAITEVENT
	 REPEAT

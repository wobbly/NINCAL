button1	button
result	form	9

	create	button1=10:12:10:15,"O&K"
.	activate button1
	activate button1,OKEvent,result

	loop
		waitevent
	repeat

OKEvent
	alert	note,"Event Processed",result
	return
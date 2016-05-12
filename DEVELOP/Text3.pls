people	xfile
person	xfile
person	dim	25
id	dim	10
name	dim	50
newaddress	dim	50
seq	form	"-1"

	move	"1556 8th avenue",newaddress
	open	people,"people.xml"
.	open	person,"people.xml"
	read	people,SEQ;person=person
	read	person,SEQ;personid=id,name=name
	update	person;address=newaddress
	CLOSE	person
	close	people
	stop
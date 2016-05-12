result	form	9
Tabs	automation
Tab	automation

tabTest3 plform	tabtest3
tabtest2 plform	tabtest2
tabtest1 plform	tabtest1
x	plform	tabtest
	winhide

	formload x
	formload tabtest1,tabtest
	formload tabtest2,tabtest
	formload tabtest3,tabtest

	getprop	TabStrip,*Tabs=Tabs
	Tabs.Add
	Tabs.Add
	setprop	Tabs(1),*Caption="one"
	setprop	Tabs(2),*Caption="two"
	setprop	Tabs(3),*Caption="three"
	setprop	Tabs(3),*Highlighted="True"
	setprop	Tabs(3),*Highlighted="False"
	setprop	Tabs(3),*Selected="True"
	setprop	Tabs(3),*Selected="False"
	loop
		eventwait
	repeat
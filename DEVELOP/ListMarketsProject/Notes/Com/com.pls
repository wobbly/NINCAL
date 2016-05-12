PlbCom		Automation
Name		Dim		20
Phone		Dim		20

		Display		*ES, *HD
		Create		PlbCom,Class="Plbwin.Program"
		EventReg	PlbCom, 1, ShowData, ARG1=Name, ARG2=Phone
		EventReg	PlbCom, 2, AllDone
		EventReg	PlbCom, 11, ProgComplete

		PlbCom.Run	Using "comdrew"
		PlbCom.EventSend	Using 1,"Smith"
		Loop
		EventWait
		Repeat

ShowData
		Display		"Name: ", Name, " Phone: ", Phone
		Return

AllDone	
		Display		"All records read"
		PlbCom.EventSend 	Using 2
		Return

ProgComplete	Display		"Program terminated"
		Keyin		Name
		Destroy		PlbCom
		Stop

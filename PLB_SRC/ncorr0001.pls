	include common.inc
	include cons.inc

PC	EQU 0
TO DIM 45
SUBJECT DIM 45
BODY    DIM 200
;TASKNAME  DIM 100
ANS	DIM	1

Release	init "1.0"
Session automation
Note    automation
Mes     automation      class="Outlook.Application"
Mailobject automation  
attachments automation
attachment automation

.Formatting vars needed
olbyvalue integer 4,"0x1"
olFlagStatus integer 4,"0x2"
	trap Exit if F5
	call Paint
	display *P5:12,"Please enter corrections and/or cancellations through the NIN Intranet Site"
	pause c5
	stop
.Open Excel application
.        create  mes

.        KEYIN   *P10:12,"Co(R)rection Update Or Cancel(L)ation? ",*t50,ANS
	if (ANS = "R")
;Create Message
		mes.CreateItemFromTemplate giving note using "\\nins1\e\data\form\correction.oft"
		setprop note,*flagstatus=olFlagStatus
		Note.display
	elseif (ANS="L")
;		KEYIN   *P10:16,"Cancel(L)ation ? ",ANS
;		if (ANS = "Y")
			mes.CreateItemFromTemplate giving note using "\\nins1\e\data\form\cancel.oft"
			setprop note,*flagstatus=olFlagStatus
			Note.display
;		else
	else
		stop
;		endif
	endif

   
Exit
	stop
	include	comlogic.inc

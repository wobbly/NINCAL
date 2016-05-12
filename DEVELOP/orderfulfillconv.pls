	include common.inc
	include cons.inc
	include norddd.inc
	include nordpdd.inc	
	include ndatdd.inc
	include compdd.inc
	include cntdd.inc
	include nowndd.inc
	
Orderfile	file
Ordererr	file

Orderfilep	file
Ordererrp	file

Orderfilepl	file
Ordererrpl	file

err	form     6
update form      6
left	form      6
release init "1.0"

	erase "c:\work\orderfulfill.dat"
	erase "c:\work\ordererr.dat"	
	
	erase "c:\work\orderfulfillp.dat"
	erase "c:\work\ordererrp.dat"

	erase "c:\work\orderfulfillpl.dat"
	erase "c:\work\ordererrpl.dat"	

	copyfile "\\nts1\e\data\text\ninord.dat","\\nts1\e\data\text\ninord.1012" 
	copyfile "\\nts1\e\data\NINPRINT.dat","\\nts1\e\data\NINPRINT.1012" 	
	copyfile "\\nts1\e\data\NINPRINTL.dat","\\nts1\e\data\ninprintl.1012" 	
	
	prepare orderfile,"c:\work\orderfulfill.dat"
	prepare ordererr,"c:\work\ordererr.dat"	
	
	
	prepare orderfilep,"c:\work\orderfulfillp.dat"
	prepare ordererrp,"c:\work\ordererrp.dat"	

	prepare orderfilepl,"c:\work\orderfulfillpl.dat"
	prepare ordererrpl,"c:\work\ordererrpl.dat"		

	Move "NINORD" to NORDNAME
	DISPLAY   *P10:8,"Working On: ",NORDNAME	
	Move C1 to NORDPATH
	loop
		Call Nordseq
	until over 
		Move OLON,NOWNFLD
		Call NOWNKEY
		If over
			write	Ordererr,seq;ordvars
			write   Orderfile,seq;ordvars	
			DISPLAY   *P10:10,"Working On LR : ",OLRN			 					
			add c1 to err
			DISPLAY   *P10:12,"Error with : ",OLRN," ","Count: ", ERR
		else
			Clear 	COMPFLD6
			Clear 	COMPNUM
			call	trim using ownctn
			pack	COMPFLD6,OWNCTN
			if (COMPFLD6 = "    " or COMPFLD6 = "")
				pack	COMPFLD6,"////"	.FORCE AN OVER
			else
				rep	zfill,COMPFLD6
			endif
			call	COMPKEY6
			if not over
				PACK OFULLFIL,COMPNUM
				Call zfillit using OFULLFIL				
				write orderfile,seq;ordvars				
				add c1 to update				
				DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
			else
				clear OFULLFIL
				write orderfile,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat
Test2	
.NINPRINT

	clear err
	clear update
	clear left
	Move "NINPRINT" to NORDPNAME
	DISPLAY   *P10:8,"Working On: ",NORDPNAME	
	Move C1 to NORDPPATH
	loop
		Call Nordpseq
	until over 
		Move OLON,NOWNFLD
		Call NOWNKEY
		If over
			write	Ordererrp,seq;ordvars
			write   Orderfilep,seq;ordvars	
			DISPLAY   *P10:10,"Working On LR : ",OLRN			 					
			add c1 to err
			DISPLAY   *P10:12,"Error with : ",OLRN," ","Count: ", ERR
		else
			Clear 	COMPFLD6
			Clear 	COMPNUM
			call	trim using ownctn
			pack	COMPFLD6,OWNCTN
			if (COMPFLD6 = "    " or COMPFLD6 = "")
				pack	COMPFLD6,"////"	.FORCE AN OVER
			else
				rep	zfill,COMPFLD6
			endif
			call	COMPKEY6
			if not over
				PACK OFULLFIL,COMPNUM
				Call zfillit using OFULLFIL				
				write orderfilep,seq;ordvars				
				add c1 to update				
				DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
			else
				clear OFULLFIL
				write orderfilep,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat	
	
.NINPRINTL
Test1
	close nordpfile
	clear nordpflag
	clear err
	clear update
	clear left
	Move "NINPRINTL" to NORDPNAME
	DISPLAY   *P10:8,"Working On: ",NORDPNAME	
	Move C1 to NORDPPATH
	loop
		Call Nordpseq
	until over 
		Move OLON,NOWNFLD
		Call NOWNKEY
		If over
			write	Ordererrpl,seq;ordvars
			write   Orderfilepl,seq;ordvars	
			DISPLAY   *P10:10,"Working On LR : ",OLRN			 					
			add c1 to err
			DISPLAY   *P10:12,"Error with : ",OLRN," ","Count: ", ERR
		else
			Clear 	COMPFLD6
			Clear 	COMPNUM
			call	trim using ownctn
			pack	COMPFLD6,OWNCTN
			if (COMPFLD6 = "    " or COMPFLD6 = "")
				pack	COMPFLD6,"////"	.FORCE AN OVER
			else
				rep	zfill,COMPFLD6
			endif
			call	COMPKEY6
			if not over
				PACK OFULLFIL,COMPNUM
				Call zfillit using OFULLFIL				
				write orderfilepl,seq;ordvars				
				add c1 to update				
				DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
			else
				clear OFULLFIL
				write orderfilepl,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat		
	
	
	stop
	include nownio.inc
	include cntio.inc	
	include nordio.inc
	include ndatio.inc
	include compio.inc	
	include nordpio.inc		
	include comlogic.inc
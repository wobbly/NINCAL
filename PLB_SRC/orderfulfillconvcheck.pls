	include common.inc
	include cons.inc
	include norddd.inc
	include nordpdd.inc	
	include ndatdd.inc
	include compdd.inc
	include cntdd.inc
	include nowndd.inc
	
Orderfile	file      fixed=408
Ordererr	file

Orderfilep	file
Ordererrp	file

Orderfilepl	file
Ordererrpl	file

NonMatch	file



input           file      fixed=408
err	form     6
update form      6
left	form      6
release init "1.0"

	erase "c:\work\orderfulfill.dat"
.	erase "\\nins1\e\data\orderfulfill.dat"
	erase "c:\work\ordererr.dat"	
	
	erase "c:\work\orderfulfillp.dat"
	erase "c:\work\ordererrp.dat"

	erase "c:\work\orderfulfillpl.dat"
	erase "c:\work\ordererrpl.dat"	

	copyfile "\\nins1\e\data\text\ninord.dat","c:\work\ninord.dat" 
.	copyfile "\\nins1\e\data\NINPRINT.dat","\\nins1\e\data\NINPRINT.1017" 	
.	copyfile "\\nins1\e\data\NINPRINTL.dat","\\nins1\e\data\ninprintl.1017" 	
	
	prepare orderfile,"\\nins1\e\data\orderfulfill.dat|NINS1:502"
        prepare ordererr,"c:\work\ordererr.dat"	
	
	
	prepare orderfilep,"c:\work\orderfulfillp.dat"
	prepare ordererrp,"c:\work\ordererrp.dat"	

	prepare orderfilepl,"c:\work\orderfulfillpl.dat"
	prepare ordererrpl,"c:\work\ordererrpl.dat"		

	prepare nonmatch,"c:\work\nonmatch.dat"
	

	Move "NINORD" to NORDNAME
        open input,"c:\work\ninord.dat",read
	DISPLAY   *P10:8,"Working On: ",NORDNAME	


	Move C1 to NORDPATH
	move c1 to nownpath
	move c1 to comppath
	
	
	loop
.	Call Nordseq
        	read input,seq;ordvars
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
		  	if (OWNCTN <> "")
				pack	COMPFLD6,OWNCTN
				rep	zfill,COMPFLD6
				call	COMPKEY6
				if not over
..added code for double check				
					If (OFULLFIL <> COMPNUM)
						write nonmatch,seq;olrn
					Endif
				
					PACK OFULLFIL,COMPNUM
					Call zfillit using OFULLFIL				
					write orderfile,seq;ordvars				
					add c1 to update				
					DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
				endif
			
		 	else
				clear OFULLFIL
				write orderfile,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat
	
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
		  	if (OWNCTN <> "")
				pack	COMPFLD6,OWNCTN
				rep	zfill,COMPFLD6
				call	COMPKEY6
				if not over
..added code for double check				
					If (OFULLFIL <> COMPNUM)
						write nonmatch,seq;olrn
					Endif				
				
					PACK OFULLFIL,COMPNUM
					Call zfillit using OFULLFIL				
					write orderfilep,seq;ordvars				
					add c1 to update				
					DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
				Endif
			else
				clear OFULLFIL
				write orderfilep,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat	
NINPRINTL
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
		  	if (OWNCTN <> "")
				pack	COMPFLD6,OWNCTN
				rep	zfill,COMPFLD6
				call	COMPKEY6
				if not over
..added code for double check				
					If (OFULLFIL <> COMPNUM)
						write nonmatch,seq;olrn
					Endif				
					PACK OFULLFIL,COMPNUM
					Call zfillit using OFULLFIL				
					write orderfilepl,seq;ordvars				
					add c1 to update				
					DISPLAY   *P10:16,"Updated with : ",OLRN," ","Count: ", update				
				endif
			else
				clear OFULLFIL
				write orderfilepl,seq;ordvars
				add c1 to left
				DISPLAY   *P10:18,"Untouched : ",OLRN," ","Count: ", left
			Endif
		Endif
	repeat		
exit1	
	
	stop
	include nownio.inc
	include cntio.inc	
	include nordio.inc
	include ndatio.inc
	include compio.inc	
	include nordpio.inc		
	include comlogic.inc
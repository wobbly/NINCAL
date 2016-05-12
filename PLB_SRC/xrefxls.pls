	include common.inc
	include cons.inc	
	include nxrfdd.inc
	include ndatdd.inc	
	include compdd.inc
	include cntdd.inc	



release init "1.0"	
OldtoNewMlr external "COMP001A;OldtoNewMlr"		
Temp	file
Temp2   file

	open temp,"c:\work\book.csv"
	prepare temp2,"C:\work\xref.csv"	
	



	loop
		Read temp,Seq;Str4
	until over
		clear str6
		call zfillit using str4
    		Call OldtoNewMlr using str4,str6
//make list for associated list to company
		move	C2 to NXRFPATH
		move	Str6,NXRFFLD2
		move	"C.LoadList-NXRFKEY",Location
		pack	KeyLocation,"Key: ",NXRFFLD2
		call	NXRFKEY
		if Not over
			loop
.				move	"000000",NXRFLIST
				move	NXRFLIST,NDATFLD
				move	C1,NDATPATH
				move	"C.LoadList-NDATKEY",Location
				pack	KeyLocation,"Key: ",NDATFLD
				call	NDATKEY
				if not over
					write temp2,seq;*CDFON,str4,NDATFLD,MLSTNAME
				endif					
				move	"C.LoadList-NXRFKS",Location
				pack	KeyLocation,"Key: ",NXRFFLD2
				call	NXRFKS
			until over
				match	str6,NXRFMLR
			until not equal		
			repeat
		else
			write temp2,seq;*CDFON,str4,"","NO XREF"		
		endif
	repeat
	stop
	include nxrfio.inc
	include compio.inc
	include cntio.inc
	include ndatio.inc		
	include comlogic.inc
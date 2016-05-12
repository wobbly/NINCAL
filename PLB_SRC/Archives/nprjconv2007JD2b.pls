PC         EQU       0
	include	common.inc
	include	cons.inc
	include	\\nins1\e\library\include\nprjdd.inc
	include	compdd.inc
	include	cntdd.inc

release	init	"1.0"

tempfile1	file
LRjan     form  1.4
LRfeb     dim   25
LRmar     dim   25
LRapr     dim   25
LRmay     dim   25
LRjun     dim   25
LRjul     dim   25
LRaug     dim   25
LRsep     dim   25
LRoct	  dim   25
LRnov     dim   25
lrdec     dim   25
TOTALLR	Dim	25

n111	FORM	1.11
n112  form  1.2
proj   form     1.4
proj2  dim  12
projlrJant   dim 3
projlrFebt   dim 3
projlrMart   dim 3
projlrAprt   dim 3
projlrMayt   dim 3
projlrJunt   dim 3
projlrJult   dim 3
projlrAugt   dim 3
projlrSept	 dim 3
projlrOctt   dim 3
projlrNovt   dim 3
projlrDect   dim 3
projclient2  dim 6

JD1    form "000"
lrproj form 11
ninproj form 11



	call	Paint
.	goto GetNextLoop
.
	erase	"c:\work\prjerror.dat"
.	erase	"c:\work\projdolr.dat"
.
	prepare	tempfile,"c:\work\prjerror.dat"
	OPEN   	tempfile1,"\\nins1\e\DATA\LM07LR.CSV"
.	OPEN   	tempfile1,"\\nins1\e\DATA\BE07LR.CSV"
.	OPEN   	tempfile1,"\\nins1\e\DATA\BR07LR.CSV"
.	loop
.		move	"NPRJSEQ",Location
.		pack	KeyLocation,"Key: SEQ"
.		call	NPRJSEQ
Loop
       ReAD   TEMPFILE1,SEQ;*CDFON,PROJCLIENT,STR45,LRJAN,LRFEB,LRMAR,LRAPR,LRMAY,LRJUN,LRJUL,LRAUG:
		       LRSEP,LROCT,LRNOV,LRDEC,Totallr
.		       LRSEP,LROCT,LRNOV,LRDEC,projlr
		goto    eoj if over
.cleanup client number
		call	Trim,ProjCLient
		call	Trim,TotalLR
		CALL	REMOVEcHAR,tOTALLR,COMMA
		MOve	c0,ProjLR
		move	TotalLR,ProjLR
		clear	str6
		count	n1,Projclient
		if	(n1=1)
		append	"00000",str6
		append	projclient,str6
		reset	STR6
		elseif	(n1=2)
		append	"0000",str6
		append	projclient,str6
		reset	STR6
		elseif	(n1=3)
		append	"000",str6
		append	projclient,str6
		reset	STR6
		elseif	(n1=4)
		append	"00",str6
		append	projclient,str6
		reset	STR6
		elseif	(n1=5)
		append	"0",str6
		append	projclient,str6
		reset	STR6
		elseif	(n1=6)
		move	ProjClient,str6
		endif
		move	Str6,Projclient

		move    lrjan to proj
		call    calcit
		move    str25 to projlrjan
		move    lrfeb to proj
		call    calcit
		move    str25 to projlrfeb
		move    lrmar to proj
		call    calcit
		move    str25 to projlrmar
		move    lrapr to proj
		call    calcit
		move    str25 to projlrapr
		move    lrmay to proj
		call    calcit
		move    str25 to projlrmay
		move    lrjun to proj
		call    calcit
		move    str25 to projlrjun
		move    lrjul to proj
		call    calcit
		move    str25 to projlrjul
		move    lraug to proj
		call    calcit
		move    str25 to projlraug
		move    lrsep to proj
		call    calcit
		move    str25 to projlrsep
		move    lroct to proj
		call    calcit
		move    str25 to projlroct
		move    lrnov to proj
		call    calcit
		move    str25 to projlrnov
		move    lrdec to proj
		call    calcit
		move    str25 to projlrdec

		move	projclient,COMPFLD
		rep	zfill,COMPFLD
		move	"CompOK-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
;			pack	COMPFLD3,str4
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			if over
				pack	str55,ProjClient," does not exist in Company file!!       "
.				goto	WriteError
			elseif (COMPMLRFLG <> "T")
				pack	str55,ProjClient," is not a valid Mailer in Company file!!"
.				goto	WriteError
			else
				move	COMPNUM,ProjClient
			endif
.		endif
				move	"2007",ProjYr
				move	"01",ProjKey
				move  projlr to lrproj
				move  projclient to projclient2
				move   projlrjan to projlrjant
				move   projlrfeb to projlrfebt
				move   projlrmar to projlrmart
				move   projlrapr to projlraprt
				move   projlrmay to projlrmayt
				move   projlrjun to projlrjunt
				move   projlrjul to projlrjult
				move   projlraug to projlraugt
				move   projlrsep to projlrsept
				move   projlroct to projlroctt
				move   projlrnov to projlrnovt
				move   projlrdec to projlrdect
.Brokerage LR Rental
.					move	"R",ProjType
.					move	"B",ProjSrc
.Brokerage LR Exchange
.					move	"E",ProjType
.LM LR 
					move	" ",ProjType
					move	"M",ProjSrc
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
projrite
            call	NPRJkey
				if not over
				move   lrproj to projlr
				move   projlrjant to projlrjan
				move   projlrfebt to projlrfeb
				move   projlrmart to projlrmar
				move   projlraprt to projlrapr
				move   projlrmayt to projlrmay
				move   projlrjunt to projlrjun
				move   projlrjult to projlrjul
				move   projlraugt to projlraug
				move   projlrsept to projlrsep
				move   projlroctt to projlroct
				move   projlrnovt to projlrnov
				move   projlrdect to projlrdec
				move  yes to projmast
				call	NPRJUPD
				else
			move	"2007",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
.					move	"R",ProjType
.					move	"B",ProjSrc
.Brokerage LR Exchange
.					move	"E",ProjType
.LM LR 
					move	" ",ProjType
					move	"M",ProjSrc
					move  projclient2 to projclient
					move  lrproj to projlr
				move   projlrjant to projlrjan
				move   projlrfebt to projlrfeb
				move   projlrmart to projlrmar
				move   projlraprt to projlrapr
				move   projlrmayt to projlrmay
				move   projlrjunt to projlrjun
				move   projlrjult to projlrjul
				move   projlraugt to projlraug
				move   projlrsept to projlrsep
				move   projlroctt to projlroct
				move   projlrnovt to projlrnov
				move   projlrdect to projlrdec
				move  yes to projmast
					call  nprjwrt
				endif
		  goto    loop
calcit
.       move   proj to str25
		move    c0 to n112
	        move   proj to n112
        
.        move   percnt6 to percnt104
.        move   percnt104 to percnt102
.        move   percnt102 to str25
.   	call    removeChar using str25,period
.     
.		 return
        move    n112 to str25
   	call    removeChar using str25,period
.   	add     n111 to n3
		return

EOJ
      shutdown

	include	nprjio.inc
	include	compio.inc
	include	cntio.inc
	include	comlogic.inc

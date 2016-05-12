PC         EQU       0
	include	common.inc
	include	cons.inc
	include	\\nins1\e\library\include\nprjdd.inc
	include	compdd.inc
	include	cntdd.inc

release	init	"1.0"

tempfile1	file
LRjan     dim   25
LRfeb     dim   25
LRmar     dim   25
LRapr     dim   25
LRmay     dim   25
LRjun     dim   25
LRjul     dim   25
LRaug     dim   25
LRsep     dim   25
LRoct	    dim   25
LRnov     dim   25
lrdec     dim   25
percentage form	3.4
n111	FORM	1.11
n112  form  1.2
proj   dim 11
proj2  dim  12
JD1    form "000"
projlrJant   dim 25
projlrFebt   dim 25
projlrMart   dim 25
projlrAprt   dim 25
projlrMayt   dim 25
projlrJunt   dim 25
projlrJult   dim 25
projlrAugt   dim 25
projlrSept	 dim 25
projlrOctt   dim 25
projlrNovt   dim 25
projlrDect   dim 25
projclient2  dim 6
lrproj form 11
projlrt dim 11





	call	Paint
.	goto GetNextLoop
.
	erase	"c:\work\prjerror.dat"
.	erase	"c:\work\projdolr.dat"
.
	prepare	tempfile,"c:\work\prjerror.dat"
	OPEN   	tempfile1,"\\nins1\e\DATA\lmlrjd2008.CSV"
.	loop
.		move	"NPRJSEQ",Location
.		pack	KeyLocation,"Key: SEQ"
.		call	NPRJSEQ
Loop
       ReAD   TEMPFILE1,SEQ;*CDFON,PROJCLIENT,STR45,LRJAN,LRFEB,LRMAR,LRAPR,LRMAY,LRJUN,LRJUL,LRAUG:
		       LRSEP,LROCT,LRNOV,LRDEC,PROJLRt
		goto    eoj if over
		move    projlrt,projlr
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

				move	"2008",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	b1,ProjType
					move	"M",ProjSrc
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
projrite
				move   projlr    to lrproj
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
			move	"2008",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	B1,ProjType
					move	"M",ProjSrc
					move  yes to projmast
	               			move  lrproj to projlr		
					move  projclient2 to projclient
					move  projlrt to projlr
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

	               			call  nprjwrt

				endif
		  goto    loop
calcit
       move   proj to str25
		 return
.      move    c0 to n111
.		move    c0 to n112
.		move    lrsep to proj
		scan   star in proj
.		move    lrmay to proj
		movefptr proj,n9
		sub     c1,n9
		setlptr  proj,n9
		reset   proj
		move    proj,proj2
		reset    proj2
		setlptr proj2,n9
		move    proj2 to n111
		move    n111 to n112
		move    n112 to str25
   	call    removeChar using str25,period
		return

EOJ
      shutdown

	include	nprjio.inc
	include	compio.inc
	include	cntio.inc
	include	comlogic.inc

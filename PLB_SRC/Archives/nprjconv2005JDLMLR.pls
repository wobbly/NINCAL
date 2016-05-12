PC         EQU       0
	include	common.inc
	include	cons.inc
	include	f:\library\include\nprjdd.inc
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
projLRJant   dim 3
projLRFebt   dim 3
projLRMart   dim 3
projLRAprt   dim 3
projLRMayt   dim 3
projLRJunt   dim 3
projLRJult   dim 3
projLRAugt   dim 3
projLRSept	 dim 3
projLROctt   dim 3
projLRNovt   dim 3
projLRDect   dim 3

JD1    form "000"
lrproj form 11



	call	Paint
.	goto GetNextLoop
.
	erase	"c:\work\prjerror.dat"
.	erase	"c:\work\projdolr.dat"
.
	prepare	tempfile,"c:\work\prjerror.dat"
	OPEN   	tempfile1,"E:\DATA\LMLR.CSV"
.	loop
.		move	"NPRJSEQ",Location
.		pack	KeyLocation,"Key: SEQ"
.		call	NPRJSEQ
Loop
       ReAD   TEMPFILE1,SEQ;*CDFON,PROJCLIENT,STR45,LRJAN,LRFEB,LRMAR,LRAPR,LRMAY,LRJUN,LRJUL,LRAUG:
		       LRSEP,LROCT,LRNOV,LRDEC,projlr
		goto    eoj if over
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

				move	"2005",ProjYr
				move	"01",ProjKey
				move  projlr to lrproj
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
				call	NPRJUPD
				else
			move	"2005",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	" ",ProjType
					move	"M",ProjSrc
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
					call  nprjwrt
				endif
		  goto    loop
calcit
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

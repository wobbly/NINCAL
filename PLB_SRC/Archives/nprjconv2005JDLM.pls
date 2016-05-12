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
JD1    form "000"



	call	Paint
.	goto GetNextLoop
.
	erase	"c:\work\prjerror.dat"
.	erase	"c:\work\projdolr.dat"
.
	prepare	tempfile,"c:\work\prjerror.dat"
	OPEN   	tempfile1,"E:\DATA\LMNIN.CSV"
.	loop
.		move	"NPRJSEQ",Location
.		pack	KeyLocation,"Key: SEQ"
.		call	NPRJSEQ
Loop
       ReAD   TEMPFILE1,SEQ;*CDFON,PROJCLIENT,STR45,LRJAN,LRFEB,LRMAR,LRAPR,LRMAY,LRJUN,LRJUL,LRAUG:
		       LRSEP,LROCT,LRNOV,LRDEC,PROJNIN
		goto    eoj if over
		move    lrjan to proj
		call    calcit
		move    str25 to projninjan
		move    lrfeb to proj
		call    calcit
		move    str25 to projninfeb
		move    lrmar to proj
		call    calcit
		move    str25 to projninmar
		move    lrapr to proj
		call    calcit
		move    str25 to projninapr
		move    lrmay to proj
		call    calcit
		move    str25 to projninmay
		move    lrjun to proj
		call    calcit
		move    str25 to projninjun
		move    lrjul to proj
		call    calcit
		move    str25 to projninjul
		move    lraug to proj
		call    calcit
		move    str25 to projninaug
		move    lrsep to proj
		call    calcit
		move    str25 to projninsep
		move    lroct to proj
		call    calcit
		move    str25 to projninoct
		move    lrnov to proj
		call    calcit
		move    str25 to projninnov
		move    lrdec to proj
		call    calcit
		move    str25 to projnindec
				move	"2005",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	" ",ProjType
					move	"M",ProjSrc
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
projrite
				call	NPRJtst
				if not over
				call	NPRJUPD
				else
			move	"2005",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	" ",ProjType
					move	"M",ProjSrc
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

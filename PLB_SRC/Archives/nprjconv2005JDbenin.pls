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
projninJant   dim 3
projninFebt   dim 3
projninMart   dim 3
projninAprt   dim 3
projninMayt   dim 3
projninJunt   dim 3
projninJult   dim 3
projninAugt   dim 3
projninSept	 dim 3
projninOctt   dim 3
projninNovt   dim 3
projninDect   dim 3
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
	OPEN   	tempfile1,"E:\DATA\benin.CSV"
.	loop
.		move	"NPRJSEQ",Location
.		pack	KeyLocation,"Key: SEQ"
.		call	NPRJSEQ
Loop
       ReAD   TEMPFILE1,SEQ;*CDFON,PROJCLIENT,STR45,LRJAN,LRFEB,LRMAR,LRAPR,LRMAY,LRJUN,LRJUL,LRAUG:
		       LRSEP,LROCT,LRNOV,LRDEC,projnin
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

			move	"COMPKEY3",Location
			unpack	ProjClient,str2,str4
			pack	COMPFLD3,str4
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
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
				move	"2005",ProjYr
				move	"01",ProjKey
				move  projnin to ninproj
				move  projclient to projclient2
				move   projninjan to projninjant
				move   projninfeb to projninfebt
				move   projninmar to projninmart
				move   projninapr to projninaprt
				move   projninmay to projninmayt
				move   projninjun to projninjunt
				move   projninjul to projninjult
				move   projninaug to projninaugt
				move   projninsep to projninsept
				move   projninoct to projninoctt
				move   projninnov to projninnovt
				move   projnindec to projnindect
.Brokerage LR Rental
					move	"E",ProjType
					move	"B",ProjSrc
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
projrite
            call	NPRJkey
				if not over
				move   ninproj to projnin
				move   projninjant to projninjan
				move   projninfebt to projninfeb
				move   projninmart to projninmar
				move   projninaprt to projninapr
				move   projninmayt to projninmay
				move   projninjunt to projninjun
				move   projninjult to projninjul
				move   projninaugt to projninaug
				move   projninsept to projninsep
				move   projninoctt to projninoct
				move   projninnovt to projninnov
				move   projnindect to projnindec
				call	NPRJUPD
				else
			move	"2005",ProjYr
				move	"01",ProjKey
.Brokerage LR Rental
					move	"E",ProjType
					move	"B",ProjSrc
					move  projclient2 to projclient
					move  ninproj to projnin
				move   projninjant to projninjan
				move   projninfebt to projninfeb
				move   projninmart to projninmar
				move   projninaprt to projninapr
				move   projninmayt to projninmay
				move   projninjunt to projninjun
				move   projninjult to projninjul
				move   projninaugt to projninaug
				move   projninsept to projninsep
				move   projninoctt to projninoct
				move   projninnovt to projninnov
				move   projnindect to projnindec
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

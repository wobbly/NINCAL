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
	OPEN   	tempfile1,"c:\work\projdolr.jd"
Looper
      
.Loop
       ReAD   TEMPFILE1,SEQ;PROJvars
		goto    eoj if over
		match     "2005", projyr
		if      equal
		goto      projrite
		else
		goto      looper
		endif
projrite
		move	"01",ProjKey
				pack	NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,ProjKey
				move	"NPRJKEY",Location
				pack	KeyLocation,"Key: ",NPRJFLD
            call	NPRJkey
				if not over
				move  "20050204" to projdate
				move  yes to projmast
				call	NPRJUPD
				endif
		  goto    looper
EOJ
      shutdown

	include	nprjio.inc
	include	compio.inc
	include	cntio.inc
	include	comlogic.inc

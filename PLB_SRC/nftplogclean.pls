.nftplogclean
pc	equ	0
	inc	common.inc	
	inc	cons.inc
	inc	nftplogdd.inc
Release	init	"test"
Reldate	Init	"2014 March7"
NewFIle	Afile	Name="NINFTPLOG1.aam|NINS1:502"
Key1	    DIM	12                 .AAM Key FileName                                 .Company Number
Key2	    DIM	103                .AAM Key FileName                                 .Number
Key3	    DIM	42	
	
	Open	newfile,"NINFTPLOG1.aam|NINS1:502"
	open	NFTPLOGFILE,"ninftplog.dat|NINS1:502"
	move	c1,NFTPLOGFLAG
	loop
	call	nftplogseq
	until   over
	packkey	key1,"01X",NFTPLOGCOMP,NFTPLOGCOMPID
	packkey	key2,"02X",NFTPLOGFileName
	packkey	key3,"03X",NFTPLOGDATE,NFTPLOGSize
	
	read	newfile,key1,key2,key3;;
	if over
	write	newfile;NFTPLOGVARS
	endif
	
	repeat
	stop
	
	
	
	
	inc	nftplogio.inc
	inc	comlogic.inc
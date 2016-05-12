..........................................................
.
. Author:  Andrew Harkins
. Date:    June 19, 2006
.
..........................................................

PC       EQU       0
	INCLUDE   COMMON.inc
	INCLUDE   CONS.inc
	include	compdd.inc
	include	cntdd.inc
	include	compnotesdd.inc
	include	nfuldd.inc
	include	gnxtdd.inc
	include	ndatdd.inc

release  init      "TEMP"
tempfile1 file
tempfile2 file
errfile	file
ndataam	afile	FIXED=600,Name="c:\work\nindat.AAM"
mhold	dim	6

.Some prep work
	aamdex	"#"\\nins1\e\data\text\nindat.dat#" c:\work\nindat.aam -14-19"
	open	ndataam,"c:\work\nindat.AAM"
.
	move	C1,COMPPATH
	ERASE	"\\nins1\e\library\develop\fulfillmentconversion\COMPANY.dat"
	ERASE	"\\nins1\e\library\develop\fulfillmentconversion\CONTACTS.dat"
	ERASE	"\\nins1\e\library\develop\fulfillmentconversion\compnotes.dat"
	ERASE	"\\nins1\e\library\develop\fulfillmentconversion\errfile.dat"
	copyfile "\\nins1\e\data\text\company.dat","\\nins1\e\library\develop\fulfillmentconversion\COMPANY.dat"
	copyfile "\\nins1\e\data\text\CONTACTS.dat","\\nins1\e\library\develop\fulfillmentconversion\CONTACTS.dat"
	copyfile "\\nins1\e\data\text\compnotes.dat","\\nins1\e\library\develop\fulfillmentconversion\compnotes.dat"
	copyfile "\\nins1\e\data\text\gnxt.dat","\\nins1\e\library\develop\fulfillmentconversion\gnxt.dat"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\gnxt -r"
	execute taskname
	open	tempfile,"\\nins1\e\library\develop\fulfillmentconversion\COMPANY.dat",EXCLUSIVE
	open	tempfile1,"\\nins1\e\library\develop\fulfillmentconversion\CONTACTS.dat",EXCLUSIVE
	open	tempfile2,"\\nins1\e\library\develop\fulfillmentconversion\compnotes.dat",EXCLUSIVE
	prep	errfile,"\\nins1\e\library\develop\fulfillmentconversion\errfile.dat",EXCLUSIVE
.Some more prep work
	clock	timestamp,timestamp
	call	paint
	//We have to position ALL the files to the EOF pointer
	loop
		read	tempfile,SEQ;str1
		until over
		add	c1 to n7
		display	*p10:12,"Reading Company record : ",n7
	repeat
	move	C0,N7
	display	*p10:12,"                                        "
	loop
		read	tempfile1,SEQ;str1
		until over
		add	c1 to n7
		display	*p10:12,"Reading Contact record : ",n7
	repeat
	move	C0,N7
	display	*p10:12,"                                        "
	loop
		read	tempfile2,SEQ;str1
		until over
		add	c1 to n7
		display	*p10:12,"Reading Notes record : ",n7
	repeat
.
	move	C0,N7
	display	*p10:12,"                                        "
	loop
		move	"NFULSEQ",Location
		pack	KeyLocation,"Key: SEQ"
		call	NFULSEQ
		until over
		add	c1 to n7
		display	*p10:12,"records processed : ",n7
.
.Get new Company Number
		//Clear all vars first
		clear	COMPVARS
.
		move	"COMPANY",GNXTFLD
		move	"GNXTKEY",Location
		pack    KeyLocation,"Key: ",GNXTFLD
	        call    GNXTKEY
        	move    GNXTNUM,N6
		loop
			add	C1,N6
        		move    N6,GNXTNUM
		        rep     zfill,GNXTNUM
			move	"GNXTUPD",Location
			pack    KeyLocation,"Key: ",GNXTFLD
        		call    GNXTUPD
		        move    N6,COMPFLD
        		rep     zfill,COMPFLD
			move	"COMPTST",Location
			pack    KeyLocation,"Key: ",COMPFLD
			call	COMPTST
			until over
		repeat
		move	COMPFLD,COMPNUM
.
		move	"T",COMPSVBFLG
		move	NFULNUM,COMPOLDSVB
		move	NFULCOMP,COMPCOMP
		move	NFULFAX,COMPFAX
		move	NFULDATE,COMPDTE
		move	NFULMODDATE,COMPRDTE
		move	NFULMODDATE,COMPDTE 		
		move	NFULINITS,COMPRUSER
		move	NFULINITS,COMPUSER
		
		move	NFULEmail,COMPEMAIL	.This is duplicated in Contacts file - if there is a Contact
		//Write Record
		write	tempfile,SEQ;COMPVARS
.Contacts File
		call	Trim using NFULCNT
		if (NFULCNT <> "")
			pack	CNCTFLD2,"01x",COMPNUM
			move	"CNCTAIM",Location
			pack	KeyLocation,"Key: ",CNCTFLD2
			call	CNCTAIM
			if not over
				//Write error message
				pack	taskname,"Contact already exists for ",COMPNUM
				write	errfile,SEQ;taskname
			else
				clear	CNCTVARS
				move	COMPNUM,CNCTCODE
				move	"001",CNCTID
				move	NFULCNT,CNCTFNAME
				loop
					scan	B1,NFULCNT
					until not equal
					bump	NFULCNT
				repeat
				if (NFULCNT <> "")
					move	NFULCNT,CNCTLNAME
				endif
				move	NFULEmail,CNCTEMAIL
				move	NFULFAX,CNCTFAX
				move	"4",CNCTTYPE
				move	"I.S.",CNCTUSER
				move	timestamp,CNCTDATE
.
				write	tempfile1,SEQ;CNCTVARS
			endif
		endif
.Notes File
		call	Trim using NFULNOTES
		if (NFULNOTES <> "")
			pack	COMPNOTEFLD,COMPNUM
			move	"COMPNOTEKEY",Location
			pack	KeyLocation,"Key: ",COMPNOTEFLD
			call	COMPNOTEKEY
			if not over
				//Write error message
				pack	taskname,"Notes already exist for ",COMPNUM
				write	errfile,SEQ;taskname
			else
				move	COMPNOTEFLD,COMPNOTECOMP
				move	NFULNOTES,COMPNOTES
				write	tempfile2,SEQ;COMPNOTEVARS
			endif
		endif
.Datacard File
one
		pack	str9,"01x",NFULNUM,"  "
		read	ndataam,str9;DATVARS
		loop
			until over
			move	COMPNUM,DATFUL
			update	NDATAAM;DATVARS
			readkg	ndataam;DATVARS
		repeat
	repeat
	close	tempfile
	close	tempfile1
	close	tempfile2
	copyfile "\\nins1\e\data\text\company.dat","\\nins1\e\data\text\company.ful"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5".	
	copyfile "\\nins1\e\library\develop\fulfillmentconversion\COMPANY.dat","\\nins1\e\data\text\company.dat"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\company -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\companyb -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\companym -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\companyo -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\companyr -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunaamdx \\nins1\E\DATA\TEXT\COMPANY.DAT \\nins1\E\DATA\INDEX\COMPANY.AAM L500 -u,7-61,227,228,229,230,231"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunaamdx \\nins1\e\data\index\company2 -r"
	execute taskname

	copyfile "\\nins1\e\data\text\CONTACTS.dat","\\nins1\e\data\text\CONTACTS.ful"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"
	copyfile "\\nins1\e\library\develop\fulfillmentconversion\CONTACTS.dat","\\nins1\e\data\text\CONTACTS.dat"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\CONTACTS -r"
	execute taskname
	pack	taskname,"!\\nins1\e\apps\plb\code\sunaamdx \\nins1\e\data\index\CONTACTS -r"
	execute taskname

	copyfile "\\nins1\e\data\text\compnotes.dat","\\nins1\e\data\text\compnotes.ful"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"

	copyfile "\\nins1\e\library\develop\fulfillmentconversion\compnotes.dat","\\nins1\e\data\text\compnotes.dat"
	//Copyfile sucks.  will not release destination file immediately!!
	pause	"5"
	pack	taskname,"!\\nins1\e\apps\plb\code\sunindex \\nins1\e\data\index\compnotes -r"
	execute taskname
.
	display	*p10:23,n7,*b,*w5
	stop

	include	compio.inc
	include	cntio.inc
	include	compnotesio.inc
	INCLUDE	nfulio.inc
	include	gnxtio.inc
	include	ndatio.inc
	include	comlogic.INC

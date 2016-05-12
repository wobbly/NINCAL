PC	equ	0
	Include	Common.inc	
	Include	Cons.inc
	INClude	MDc090DD.inc
	INClude	M2nDD.inc
	include	Nowndd.inc
               INCLUDE        PRTPAGEDD.INC
.
Release	Init	"00.01"	DLH 07November2006
Reldate	INit	"07November2006"
File	File
Page	FOrm	5
MinCount	Form	6
NinCount	Form	6
MinActive	Form	6
.
.SOrt input
	clear	Taskname
	pack	taskname,"\\nins1\e\data\text\mdc_090.dat,\\nins1\e\data\Mdc_090.srt -6-30,31-55"
               Sort           Taskname
               If             over
               Alert          Caution,Taskname,result
               alert          caution,S$ERROR$,result
               return
               endif
               
               trap            PrintErr if SPOOL
               PRTOPEN         Laser,"",WPrognme
               prtpage         Laser;*UNITS=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT;
               
               call	Header
               move	c1,M2nPath
	OPen	File,"\\nins1\e\data\mdc_090.srt",read
PRocess	Loop	
	read	File,seq;M090VARS
	until	over
	add	c1 to Mincount
		if	(GDlt = "A")
		add	c1,MinActive
		endif
	clear	M2nFld
	packkey	M2nFld,c0,Gkey
	clear	Nownfld
	call	M2nKey
		if	Not over
		add	c1 to Nincount
		unpack	M2NNIN,str2,Nownfld
		call	Nownkey                  .should not be over
		endif
			
	if	(Row >= 9500)
	call	Header
	endif
	call	Trim using Gcty
	call	Trim using Ownlocty
	PRtPage	Laser;*p50:row,Gkey,b1,Gdlt,*p4500:row,ownlon,b1,ownstat
	add	"150",row
	PRtPage	Laser;*p100:Row,Gnam,*p4550:row,OWNOCPY
	add	"150",row
	PRtPage	Laser;*p100:Row,gad1,*p4550:row,OWNLOSA
	add	"150",row
	PRtPage	Laser;*p100:Row,*ll,Gcty,", ",Gstt," ",Gzip,*p4550:row,*ll,Ownlocty,", ",OWNLOS," ",ownlozc
	add	"150",row
	PRtPage	Laser;*p100:Row,gCnt,*p4550:row,OWNLONM
	add	"150",row
	PRtPage	Laser;*p100:Row,gPH1,*p4550:row,OWNTele
	add	"300",row
	Clear	Ownlon
	clear	Ownstat
	clear	OwnLosa
	clear	Ownocpy
	clear	ownlonm
	clear	ownlos
	clear	ownlocty
	clear	ownlozc
	clear	owntele



	repeat

	if	(Row >= 9500)
	call	Header
	endif

	add	"300",row
	PRtPage	Laser;*p100:Row,MinCOunt," Min Owners",*p4550:row,NINCOunt,"NIN Owners Matched"
	add	"150",row
	PRtPage	Laser;*p100:Row,MinActive," Active Min Owners"

	PRtCLose	Laser
	stop

Header	if	(page > 0)
                PRTPAGE	  Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon,*newpage
	else
                PRTPAGE	  Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon
.                                *p=3500:250,ReportTitle,*p=7200:250,Today
	endif
                Move            "800" to row

                add             c1 to page
	Return
;PrintErr     ----   Printing cancelled or error
Printerr
               Trapclr        Spool
               alert          Caution,"Printing Cancelled or Error!",result
               Noreturn
               Return
	stop

	INclude	comlogic.inc
	INClude	M2nio.inc
	include	nownio.inc
	include	mdc090io.inc
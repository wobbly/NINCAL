PC       EQU       0
.
	INCLUDE   	COMMON.inc
         	INCLUDE   	CONS.inc
         	INCLUDE   	CompDD.inc
	include	Cntdd.inc
	Include	BrptDD.inc
	INclude	norddd.inc
	Include	NMLDdd.inc
	include	nINVDD.INC
Release	INit	"2.0"	DLH total rewrite
Reldate	Init	"24 June 2008"
.Note before turned on we need to start sending out letters of impending credit holds
.LM has agreed we can turn of credit for their clients at 75 days
.Must read "bancruptcy" file and not lock out for those LR's
.envision it being run at least weekly
.
.read invoice file - open items only if not in bancruptcy file and meet other criteria lock out
.create file for impending and credit hold letters
. need to look at bill direct info (for Brokerage). File should have sufficent data to produce letter to correct address with
.information regarding which orders need to be able to sort by broker and mailer
.

.RELEASE  INIT      "1.6"         DLH 23MAR92    NMLRXX,
.RELEASE  INIT      "1.5"        D.L. HERRICK    19AUG91
.              ' '=OK,
.             "*" = ON HOLD.
.             "I" = INACTIVE,
.             "B" = CREDIT RISK.  -      reset nightly if released
.             "N" =   NEW MAILER.
.             "P" = POLITICAL MAILER.  - reset nightly if released
.             "W" = Warning - read note                     ; 21Dec2000
.             "M" = Must Prepay                                            ; 05Mar2002
.             "9" = On hold until over 90s paid             ; 05Mar2002
.             "G" = Guarantees are always required          ; 05Mar2002
.             "g" = Guarantees No longer accepted           ; 06Apr2004
.
.
. FILE DESCRIPTIONS.
. ..................
Letter    	FILE        .orders we are going to send a warning letter to brokers about 60 & +
Lock	FIle	.orders that are causing LM Mailers to go on credit Hold  75 & + also get a letter

......................................................................................................................................
.
.
. PROGRAM VARIABLES
. .................
Count	Form 	5
READNUM  FORM      5       NUMBER OF RECORDS READ.
UPDNUM   FORM      5       NUMBER OF RECORD UPDATED.
CONTNUM  FORM      3
TodayIs     Form            5              .Todays date in Julian
InvDteIS	Form            5              .Invoice date in Julian
MLDateIS	Form            5              .Mail date in Julian
.
.
         MOVE      "NEOM0008" TO PROGRAM
         MOVE      "Names In The News" TO COMPNME
         MOVE      "CHECK CLIENT CREDIT" TO STITLE
         CALL      PAINT
         CALL      FUNCDISP
PassOne
	move	c1,Brptpath
	Prepare	Letter,"c:\work\Letter,dat",Exclusive
	Prepare	Lock,"c:\work\Lock,dat",Exclusive
	MOve	c3,NORDLOCK              
	MOve	c1,NORDPath
	clock 	timestamp,timestamp
            unpack 	timestamp,str2,yy,mm,dd
            call  	cvtjul
            move 	juldays,TODAYIS
	Loop
InputInv	Call	NinvSEQ
	UNtil	Over

	if	(STATB = "P")
	goto	InputINv
	endif
.check bankruptcy file

	packkey	Brptfld,LRN
	call	BrptKey
	If	Not over
	goto	InputInv 
	endif

	scan	"CSH",CHKN1	          .is it in a control?
	goto	InputINv if equal
	Reset	CHKN1
.Check Invdate if its less than 20 days forget it we had a bill issue skipit
	move	INVDTEY to yy
            move        InvDted to dd
            move        Invdtem to mm
            call        cvtjul
	Move	Juldays,InvDteIs

	IF	(invdteIs +20 <= TodayIS)
	goto	InputINv
	endif

	PackKey  	NordFld from LRN
	Call	Nordkey
.check for original MD
            pack	NMLDFLD1,"01X",LRN
            clear   str8
	pack	str8,"99999999"
	call	NMLDAIM
	loop
		until over
		if (NMLDDATE < str8)
			move	NMLDDATE,str8
		endif
		call	NMLDKG
	repeat
	if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
               unpack     str8 into omdtec,omdtey,omdtem,omdted
	else
.Use current Mail Date
	endif
	move           omdtey to yy
            move           omdted to dd
            move           omdtem to mm
            call           cvtjul
	Move	Juldays,MLDateIs
.if 75 0r more past maildate = potential hold
	if	(todayIS - 75 >= MlDateis)       .oops old
		If	(LET90D = "1" & LET90D <> "2")     .already got warning put on hold
		Write	Lock,Seq;OrdVars
		endif
	ElseIF	(todayIS - 60 >= MlDateis & (LET90D = B1 or LET90D = ""))       .no notice yet & over 60
	Write	Letter,Seq;OrdVars
	endif		
	
	Repeat
.........................................................................................................................
.Sort the files
	Weof	Lock,seq
	Weof	Letter,Seq
	close	Lock
	CLose	Letter
.
	Display 	*p1:22,*el,"sorting Lock"
	Pack	Taskname from "c:\work\Lock.Dat \\nins1\e\data\Lock.Dat -303-306,307-309,3-6,7-12"
	Sort	Taskname
.
	Display 	*p1:22,*el,"sorting Letter"
	Pack	Taskname from "c:\work\Letter.Dat \\nins1\e\data\Letter.Dat -303-306,307-309,3-6,7-12"
	Sort	Taskname
.Letters
PassTwo
	Move	C0,Count
	Open	Letter,"\\nins1\e\data\letter,dat",exclusive
	Loop
	Read	Letter,seq;Ordvars
	Until	OVer
	If	(count = c1)
.set break goodies	
	endif
.decide break goodies
.how many per page etc
.ok we added to letter mark the invoice
	Move	C1,Ninvpath
	PackKey	Ninvfld,Olrn
	call	Ninvkey
	Move	c1,Let90D
	call	NinvUpd
	repeat

.Lock
PassThree
	Move	C0,Count
	Open	Lock,"\\nins1\e\data\lock,dat",exclusive
	Loop
	Read	Lock,seq;Ordvars
	Until	OVer
	add	c1,count
	If	(count = c1)
.set break goodies	
	endif
.decide break goodies
.how many per page etc
.ok we added to letter mark the invoice
	Move	C1,Ninvpath
	PackKey	Ninvfld,Olrn
	call	Ninvkey
	Move	c2,Let90D
	call	NinvUpd

	repeat




	Packkey 	COmpfld from Compnum
.
         	CALL      CompKEY
.         	GOTO      READ IF OVER
	If	(compCredit <> " ")
	return                           .status is already set
	endif
         	MOVE      "*" TO CompCredit
         	CALL      CompUPD
.
EOJ
         	DISPLAY   *P1:24,"JOB DONE / RETURNING TO CHAIN",*W5;
         	STOP
.
EXIT
         	STOP
	Include	BrptIO.inc
         	INCLUDE   	COMPIO.inc
	INclude	Cntio.inc
	INclude	nordIO.inc
	Include	NMLDIO.inc
	include	nINVIO.INC
         	INCLUDE   	COMLOGIC.inc

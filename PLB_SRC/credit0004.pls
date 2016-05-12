PC       EQU       0
.
	INCLUDE   	COMMON.inc
         	INCLUDE   	CONS.inc
	include	nINVDD.INC
Release	INit	"1.0"	DLH Clear let90 status
Reldate	Init	"27 October 2008"
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

......................................................................................................................................
.
.
. PROGRAM VARIABLES
. .................
Count	Form 	5
READNUM  	FORM      	7       NUMBER OF RECORDS READ.
UPDNUM   	FORM      	5       NUMBER OF RECORD UPDATED.
CONTNUM  	FORM      	3
LockCount	Form	5
TodayIs     Form            5              .Todays date in Julian
InvDteIS	Form            5              .Invoice date in Julian
MLDateIS	Form            5              .Mail date in Julian
HoldBrk	dim	7	.brk & contact #'s
HoldMlr	dim	7	.MLR & contact #'s
PdfFName	Dim	35
Laser	pfile
LetterType	Dim	7
faxname	dim	45
faxtele	dim	10
faxattn	dim	45
FaxFlag	Form	1
FileCheck	FIle
trapcount	form	4
LttrEmail	Dim	75
EmailFlag	Dim	1
TimesNew11	font
QTYMSK	INIT	"ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT	DIM	11         *USED FOR ORDER PRINT
QTYNUM	FORM	9         *USED FOR ORDER PRINT, QTY FORMATING.
NINLogo	PICT
Longdist	Dim	1
Pass	Form	1
dlFiles 	datalist
DLresult 	form 9
DLndx 	form 9
dmFileName 	dim 80
SJdate	Form	5            starting credit hold date in Julian
	CREATE	NINLogo=3:13:30:50:
		"\\nins1\e\netutils\NIN logo black outline.jpg"
.
.
         	MOVE      	"Credit0004" TO PROGRAM
         	MOVE      	"Names In The News" TO COMPNME
         	MOVE      	"Reset CREDIT LEtter " TO STITLE
         	CALL      	PAINT
         	CALL      	FUNCDISP
	create	TimesNew11,"Times New Roman",size=11
	move 	"300" to Column1
	move 	"1200" to Column2
	move 	"2500" to Column3
	move 	"4000" to Column4
	move 	"4500" to Column5
	move 	"5000" to Column5

............................................................................................................
.Main
Main
         	Keyin   	*P15:12,"Lr ## to be Updated: ",str6
	MOve	c1,NInvPath                
	Rep	Zfill,Str6
	packkey	Ninvfld from Str6
	Scan	star in str6
	goto	Eoj if over
	call	Ninvkey	
	if	OVer
	Display 	*p1:23,*ef,"Not found",*b,*W4
	goto	Main
	endif
Status
         	DISPLAY   *P45:12,"Letter Status: ",Let90D
         	Keyin	*p15:13,"Change Status to: ",str1
	if	(str1 = b1 | str1 = "1" | str1 = "2")
	else
	Display 	*p1:23,*ef,"Invalid",*b,*W4
		if	(str1 = star)
		goto	Eoj
		endif
	goto	Status	
	endif
	Move	Str1,Let90D
	call	ninvupd
	Keyin 	*p1:23,*ef,"More ?",str1
	if	(Str1 = "y" | str1 = "Y")
	goto	Main
	endif
	
.
EOJ	
 
EXIT
 	Shutdown	"cls"
         	STOP
	include	nINVIO.INC
         	INCLUDE   	COMLOGIC.inc

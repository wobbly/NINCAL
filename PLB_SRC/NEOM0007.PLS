PC       	EQU       	0
         	INCLUDE   	COMMON.inc
         	INCLUDE   	CONS.inc
	INClude	compdd.inc
	INclude	Cntdd.inc

Release	Init	"2.0"	DLH total rewrite
Reldate	INit	"24 June 2008"
.
.
. ..................
.
.
. PROGRAM VARIABLES
. .................
READNUM  	FORM      5       NUMBER OF RECORDS READ.
UPDNUM   	FORM      5       NUMBER OF RECORD UPDATED.
DATE     	DIM       8
.
.
         	TRAP      	EXIT IF F5
	 MOVE      	"ABORT " TO PF5
         	MOVE      	"NEOM0007" TO PROGRAM
         	MOVE      	"NINCAL" TO COMPNME
         	MOVE      	"CLEAR CREDIT Status" TO STITLE
         	MOVE      	"99/99/99" TO TODAY
         	CLOCK     	DATE TO DATE
	IFNZ	   PC
         	MOVE      	DATE TO N6
         	EDIT      	N6 TO TODAY
	XIF
	IFZ	   PC
	MOVE      	DATE TO TODAY
	XIF
	CALL	   PAINT
	CALL      	FUNCDISP
	move	c1,comppath
         	DISPLAY   	*P15:12,"NUMBER OF Records READ    : ":
                   	*P15:14,"NUMBER OF Records UPDATED : ";
.
...............................................................................
READ  	
         	CALL      COMPSEQ
         	GOTO      DONE IF OVER
	ADD       C1 TO READNUM
         	DISPLAY   *P44:12,READNUM;
.if mailer  check status
	if	(COMPMLRFLG = "T")
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
		if	(COMPCREDIT <> "*")
		goto	REad
		endif
	Packkey 	COmpfld from Compnum
	ADD       	C1 TO UPDNUM
.
         	CALL      	CompKEY
         	GOTO      	READ IF OVER
         	MOVE      	" " TO CompCredit
         	call	Debug
         	CALL      	CompUPD
         	DISPLAY   	*P44:14,UPDNUM;
	Move	"dherric@nincal.com",Mailto
	move	"Creques@nincal.com",Mailfrom
	move	"credit Released",Mailsubjct
	append	"Company ## ",mailBody
	append	Compnum,Mailbody
	Append	CRLF,Mailbody
	append	Compcomp,Mailbody
	Append	CRLF,Mailbody
	reset	Mailbody
	call 	SendMail
         	GOTO      	READ
	Else

	GOto	REad
	endif
.
DONE
         DISPLAY   *P1:24,"JOB DONE / RETURNING TO CHAIN",*W5;
         STOP
EXIT
         TRAPCLR   F5
         STOP
         	INCLUDE   	CompIO.inc
	INclude	CntIO.inc
         	INCLUDE   	COMLOGIC.inc


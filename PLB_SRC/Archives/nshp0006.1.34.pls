PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
;Patch1.3
			include	compdd.inc
			include	cntdd.inc
.         INCLUDE   NMLRDD.inc
;Patch1.3
         INCLUDE   NSHPDD.inc
         INCLUDE   NOWNDD.inc
         include   hp.inc
         include   nofrdd.inc
.START PATCH 1.32 REMOVED LOGIC
..START PATCH 1.21 ADDED LOGIC
.	INCLUDE	NFULDD.INC
..END PATCH 1.21 ADDED LOGIC
.END PATCH 1.32 REMOVED LOGIC
.START PATCH 1.23 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 1.23 ADDED LOGIC
.
. .............................................................................
Release	init	"1.34"		        DMB	12OCT2006	Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release	init	"1.33"		JD	15SEP2006	No date check, include TDMC
.Release	init	"1.32"		DMS	22JUN2006	Fulfillment Conversion
.Release	init	"1.31"		JD	22MAY2006	Created print file
.Release	init	"1.3"		DMB	26MAY2004	Mailer Conversion
.release	init	"1.23"		ASH	29JAN2004  DATACARD CONVERSION
Reldate	init	"OCTOBER 12, 2006"
.reldate	init	"JANUARY 29, 2004"
.Release   init     "1.22"          		18Mar2002 added extra date check, only include orders that are past due 3 days.
.Release   init     "1.21"          	ASH	05FEB2002 ASH NINFUL CONVERSION
.Release   init     "1.2"          	DLH	27Sep2001 DLH attempt to elim phantom reuse orders
.release  init     "1.1"          	DLH	06Feb2001 DLH fax, email, or as last resort print the reports.
.RELEASE INIT      "1.0"          	DLH	22January2001 DLH  as report
* NAMES IN THE NEWS CALIF. Nightly List Management SHIPPING Info request REPORT PROGRAM
. .................................................................................................................
.
. WORK VARIABLES
.
PDATE    DIM       8
TELEMASK INIT      "(999)999-9999"
ORDMASK INIT       "ZZZ,ZZ9,999"
ORDQTY   DIM       11
QTYNUM   FORM      9
SYSJDATE FORM      5
TELE1    DIM       13
fax1    DIM       13
fax2    DIM       5
CODENUM  FORM      2
.
PROGNAME DIM       8
.
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
HOLDOWN  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "46"
COUNTO   FORM      8                  NUMBER OF ORDERS READ.
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
Output   File
.
.START PATCH 1.32 ADDED LOGIC
NFULCOMP	DIM	55
.END PATCH 1.32 ADDED LOGIC
. .............................................................................
*******************************************************************************
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F3
         MOVE      "EXIT" TO PF3
         MOVE      "NSHP0006" TO PROGRAM
         MOVE      "Nightly SHIPPING REPORT" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         move      c2 to nshplock
;START PATCH 1.2 REPLACED LOGIC
         pack      str35,NTWKPATH1,"weekship.lst"
         splopen   STR35
         clear     str35
*****************
*****************
.
PassOne
	  move     "500000" to nordfld
          CLOCK     DATE TO PDATE
          move      pdate to date
          UNPACK    DATE INTO MM,str1,DD,str1,YY
          CALL      CVTJUL
          MOVE      JULDAYS TO SYSJDATE
          move      juldays to hidate
          move      juldays to lodate
          sub       c3 from lodate
          move     c1 to nordpath
          call       nordtst
          PACK   STR35,NTWKPATH1,"DISKIN71.tmp"
          prepare  output,str35,exclusive
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..8 = Windows CE
        if       (str1 = "3" or str1 = "4")
        move     c1 to osflag
        endif
        if       (str1 = "1" or str1 = "5")
        move     c2 to osflag
        endif
.        goto        passtwo
Pass1Loop call     nordks
          goto     eoj1 if over
          add      c1 to counto
          DISPLAY   *P15:10,"RECORDS IN = ",COUNTO;
          RESET     CANCODES               *RESET FORM POINTER.
          SCAN      OSTAT IN CANCODES       *CANCELLED?
          GOTO      Pass1Loop IF EQUAL
          move      "pxlz" to str4
          scan      ostat in str4
          goto      Pass1Loop if equal
          RESET     RUNCODES
          SCAN      OLNUM IN RUNCODES
          GOTO      Pass1Loop IF EQUAL
          CMatch    "B" to ostat
          goto      PASS1LOOP if equal            .already billed skip.
.begin patch 1.2
          match      "0001",ortnnum       .REUSE
          goto       pass1loop if equal
          move       c0 to n4
          move       ortnnum to n4
          branch     n4 of pass1loop
.end patch 1.2

          clear     str2
          pack      str2 from OSALES10,osales
          move      c0 to n2
          move      str2 to n2
          if        (n2 = 6 or N2 = 19)           .List Management?
          goto      checkdate
          else
          goto      pass1loop
          endif
.
CheckDate
.start patch 1.33
          goto      checktwo
.end patch 1.33
.return date past or within the next 3 days?
          if        (ortndtem > "0" & ortndtem < "13")
          move      ORTNDTEM  to mm
          move      ORTNDTEd  to dd
          move      ORTNDTEy  to yy
          move      ORTNDTEc  to cc
          else
          move      OMDTED  to mm
          move      OmdTEd  to dd
          move      OmdTEy  to yy
          move      OmdTEc  to cc
          endif
          call      cvtjul
.          if        (juldays <= sysjdate)
          if        (juldays >= lodate & juldays <= hidate)
          goto      checktwo
          else
          call      debug
          goto      pass1Loop
          endif
.
checktwo
          MOVE      OLRN TO NSHPFLD
          rep       zfill in nshpfld
          CALL      NSHPKEY
          goto      pass1loop if not over         .already shipped
          MOVE      OLON TO NOWNFLD
          REP       ZFILL IN NOWNFLD
          CALL      NOWNKEY
.START PATCH 1.21 REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	call	Trim using OWNCTN
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 1.32 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"checktwo-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.	scan	"TDMC",NFULCOMP
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"checktwo-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear	NFULCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear	NFULCOMP
.			else
.				move	COMPCOMP,NFULCOMP
.			endif
.		endif
.	else	//  OWNCTN = ""
.		clear	COMPFLD6
.		clear	NFULCOMP
.	endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	Call Trim Using OFULLFIL
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit usin COMPFLD
		move	C1,COMPPATH
		move	"checktwo-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	NFULCOMP
		else
			move	COMPCOMP,NFULCOMP
		endif
	else	//  OFULLFIL = ""
		clear	COMPFLD
		clear	NFULCOMP
	endif
.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.END PATCH 1.32 REPLACED LOGIC
.END PATCH 1.21 REPLACED LOGIC
.start patch 1.33
.	scan	"TDMC",NFULCOMP
.          GOTO      Pass1Loop IF EQUAL            .triplex we get info a diff way
.end patch 1.33
          write     Output,seqeof;ordvars
          goto      Pass1Loop
eoj1
.close - sort
         weof       output,seqeof
         DISPLAY   *P15:23,*EL,"Sorting records = ";
         close      nordfile
         close      output
;        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,214-247"
        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,66-73"
        sort    taskname
        if over
                move    s$error$,error
	         DISPLAY   *P15:23,*EL,"Sorting Error = ",Error,*w5,*b,*w5;
                stop
        endif
.passtwo -
passtwo
         move       C0 TO COUNTO
         CALL      FUNCDISP
        clear   taskname
         move       c0 to nordflag
         PACK   STR35,NTWKPATH1,"DISKIN71.dat"
         open      output,str35,exclusive
;START PATCH 1.31 REPLACED LOGIC
         pack      str35,NTWKPATH1,"weekship.lst"
         splopen   STR35
.         PACK   STR35,NTWKPATH1,"DISKIN71.LST"

.before we open print file we need to see if we can fax or email by reading the 1st record.

.         if        (osflag = c2)         .nt
.         splopen   "\\NTS0\Laser6","A"
.         splopen   "\\NTS0\Laser8","A"
.         endif
.         if        (osflag = c1)         .win 95 98
.         splopen   "Laser6","A"
.         endif
.         if        (osflag = c0)         .Don't know prompt for printer
.         splopen   "","A"
.         endif

.         SPLOPEN   STR35
;End PATCH 1.31 REPLACED LOGIC
         PRINT     hp17ptch,hptop                .compressed
         MOVE      "                    " TO FERROR
.
GETREC   DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
.
         read      output,seq;ordvars
         GOTO      EXIT IF OVER
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
         ADD       C1 TO COUNTO
         DISPLAY   *P15:12,"Printing Record = ",COUNTO;
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
.START PATCH 1.21 REPLACED LOGIC
.Start Patch 1.34 REPLACE CODE Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.End Patch 1.34 REPLACE CODE Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
.START PATCH 1.32 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"GETREC-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"GETREC-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear	NFULCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear	NFULCOMP
.			else
.				move	COMPCOMP,NFULCOMP
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear	COMPFLD6
.		clear	NFULCOMP
.	endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD		
		move	C1,COMPPATH
		move	"GETREC-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	NFULCOMP
		else
			move	COMPCOMP,NFULCOMP
		endif
	else	// OFULLFIL = ""
		clear	COMPFLD
		clear	NFULCOMP
	endif

.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.END PATCH 1.32 REPLACED LOGIC
.END PATCH 1.21 REPLACED LOGIC
         MATCH     NOWNFLD TO HOLDOWN
         CALL      BREAK IF NOT EQUAL
         CALL      MLRREAD
         GOTO      PRINT
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CALL      OVER IF OVER
         RETURN
.
.
*......................................................................
HEADER   ADD       C1 TO PAGE
         MOVE      TELEMASK TO TELE1
         EDIT      OWNTELE TO TELE1
         clear     fax1
         clear     fax2
         if        (ownfax <> " " & ownfax <> "")
         move      telemask to fax1
         edit      ownfax to fax1
         move      "Fax ##" to fax2
         endif
.START PATCH 1.21 REPLACED LOGIC
.         PRINT     *F,*n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
.                   PDATE:
.                   *L,*40,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                   *119,"PAGE ## ",PAGE:
.                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
.                   *L,*21,OWNLONM,tele1:
.                   *L,*21,OWNOCPY,fax1,b1,fax2:
.                   *L,*21,OWNCTN
         PRINT     *F,*n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
                   PDATE:
                   *L,*40,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
                   *119,"PAGE ## ",PAGE:
                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
                   *L,*21,OWNLONM,tele1:
                   *L,*21,OWNOCPY,fax1,b1,fax2:
                   *L,*21,NFULCOMP
.END PATCH 1.21 REPLACED LOGIC
;         PRINT     *N,*50,"RTN/MAIL",*60,"QUANTITY",*72,"SHIPPED":
         PRINT     *N,*50,"MAIL/RTN",*60,"QUANTITY",*72,"SHIPPED":
                   *81,"POSTAGE":
                   *93,"SHIPPING Method":
                   *L,*1,"LR ##",*10,"MAILER/OFFER/LIST",*52,"DATE":
                   *60,"ORD/SHIP",*74,"DATE",*81,"AMOUNT":
                   *93,"Tracking Number"
         MOVE      C10 TO LINES
         RETURN
*......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO ORDQTY
DETAIL
.START PATCH 1.23 REPLACED LOGIC
.         PRINT     *L,*1,OLRN,*10,MCOMP,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                   *58,ORDQTY,*71,"__/__/____",*82,"$______",*91,"______________________________":
.                   *L,*10,OFDESC,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *59,"__________",*91,"______________________________":
.                   *L,*1,Hpbon,*10,O1DES,hpboff,b3,O2DES:
.                   *L
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
         PRINT     *L,*1,OLRN,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
                   *58,ORDQTY,*71,"__/__/____",*82,"$______",*91,"______________________________":
                   *L,*10,OFDESC,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
                   *59,"__________",*91,"______________________________":
                   *L,*1,Hpbon,*10,O1DES,hpboff,b3,NSEL2NAME:
                   *L
;         PRINT     *L,*1,OLRN,*10,MCOMP,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
;                   *58,ORDQTY,*71,"__/__/____",*82,"$______",*91,"______________________________":
;                   *L,*10,OFDESC,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
;                   *59,"__________",*91,"______________________________":
;                   *L,*1,Hpbon,*10,O1DES,hpboff,b3,NSEL2NAME:
;                   *L
.END PATCH 1.23 REPLACED LOGIC
         ADD       C4 TO LINES
         GOTO      GETREC
BREAK
         CALL      NOWNKEY
.START PATCH 1.21 REPLACED LOGIC
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
.START PATCH 1.32 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"BREAK-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"BREAK-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear	NFULCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear	NFULCOMP
.			else
.				move	COMPCOMP,NFULCOMP
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear	COMPFLD6
.		clear	NFULCOMP
.	endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"BREAK-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	NFULCOMP
		else
			move	COMPCOMP,NFULCOMP
		endif
	else	// OFULLFIL = ""
		clear	COMPFLD
		clear	NFULCOMP
	endif
.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.END PATCH 1.32 REPLACED LOGIC
.END PATCH 1.21 REPLACED LOGIC
         move      c0 to page
         CALL      HEADER
         MOVE      NOWNFLD TO HOLDOWN

*.................add code to create fax codes or email goodies

         RETURN
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
          SPLCLOSE
          shutdown
          STOP
EXIT1     shutdown
          STOP
* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,Str1;
         CMATCH    "Q",Str1
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
.debug    return
..........................................................................
         INCLUDE   NSHPIO.inc
         INCLUDE   NOWNIO.inc
;Patch1.3
			include	compio.inc
			include	cntio.inc
.         INCLUDE   NMLRIO.inc
;Patch1.3
         INCLUDE   NORDIO.inc
         include   nofrio.inc
.START PATCH 1.32 REMOVED LOGIC
..START PATCH 1.21 ADDED LOGIC
.	INCLUDE	NFULIO.INC
..END PATCH 1.21 ADDED LOGIC
.END PATCH 1.32 REMOVED LOGIC
.START PATCH 1.23 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 1.23 ADDED LOGIC
        INCLUDE   COMLOGIC.inc

PC       EQU       0
	INCLUDE   COMMON.inc
         	INCLUDE   CONS.inc
         	INCLUDE   NORDDD.inc
.Patch1.4
	include	compdd.inc
	include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.Patch1.4
.Begin Patch 1.73
	Include	Ndatdd.inc
.end Patch 1.73
         	INCLUDE   NSHPDD.inc
         	INCLUDE   NOWNDD.inc
         	include   hp.inc
.         include   nofrdd.inc
.START PATCH 1.32 REPLACED LOGIC
.        include fulfill.inc
.START PATCH 1.56 REMOVED LOGIC
.	INCLUDE	NFULDD.INC
.END PATCH 1.56 REMOVED LOGIC
.END PATCH 1.32 REPLACED LOGIC
.
. .................................................................................................................
Release   init     "1.73"            DLH Pull all exclusives
Reldate	Init	"19 March  2008"
.Release   init     "1.72"            DLH Whitney
.Reldate	Init	"15 February  2008"
..Release   init     "1.71"            DLH Facsys (nts3)
.Reldate	Init	" October 2007"
.Release   init     "1.70"            DLH Fix sort, file name, break, etc
.Reldate	Init	"14 August 2007"
.Release   init     "1.60"            24 May 2007 DLH skip  Alaska Conservation Foundation
.Reldate	Init	"18 June 2007"
.Release   init     "1.59"            24 May 2007 DLH PLI
.Reldate	Init	"24 May 2007"
.Release   init     "1.58"            12OCT2006 DMB Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release   init     "1.57"            14AUG2006 JD Added file path for open
.Release   init     "1.56"           22JUN2006 DMS Fulfillment Conversion
.Release   init     "1.55"           23MAY2006 JD Added Secondary file check for list exclusion, new starting LR#
.Release   init     "1.53"           14JUN2005 ASH RETIRED PCL2PDF - Note that this program
.					is not really using PCL2PDF.  Logic looks like test
.					logic only!!
.Release   init     "1.52"           09DEC2004 ASH FAXFILE.PRN
.Release   init     "1.51"           14SEP2004 JD new starting lr.
.Release   init     "1.5"           30AUG2004 DMB	New Logo
.Release   init     "1.4.1"        07JUL2004 DMB Add code to skip Kerry for Pres. per BS
.Release   init     "1.4"           26MAY2004 DMB	Mailer Conversion
.Release   init     "1.35"           28feb2003 DMB Added code to skip Ntl urban league per JN
.Release   init     "1.34"          05Feb2003 DMB Added code to skip african wildlife foundation
.Release   init     "1.33"          21Jun2002 DMB  Patched shipping request for emagazine
.Release   init     "1.32"          20APR2002 ASH	NINFUL CONVERSION
.Release   init     "1.31"          09Apr2002 skipping records for list Animal Place.
.Release   init     "1.3"           20Mar2002 using fulcnt10a instead of fulcnt10 e magazine.
.Release   init     "1.2"           27Sep2001 DLH attempt to elim phantom reuse orders
.release   init     "1.1"           06Feb2001 DLH fax, email, or as last resort print the reports.
.RELEASE   INIT     "1.0"           22January2001 DLH  as report
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
.osflag   form   1          1=win 95,98, 2=NT
outflag  form   1          1=print, 2=fax, 3= email, 4= fax & email
LONGDIST DIM       1
. .............................................................................
.START PATCH 1.56 ADDED LOGIC
NFULCOMP	DIM	55
NFULCNT	DIM	55
NFULFAX	DIM	10
.begin patch 1.73
NFULEMAIL	Dim	50
.end patch 1.73
.END PATCH 1.56 ADDED LOGIC
.Print Variables
PrintDoc	pfile
.START PATCH 1.5 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
Column8 form 8
Column9 form 8
	move "300" to Column1
	move "500" to Column2
	move "1200" to Column3
	move "1500" to Column4
	move "4000" to Column5
	move "4050" to Column6
	move "5050" to Column7
	move "6050" to Column8
	move "6800" to Column9
CopyVar2 dim	5000
TimesNew10	font
.begin patch 1.7
.tempfle	dim 40
tempfle	dim 	100
.end patch 1.7
	create	TimesNew10,"Times New Roman",size=10
                    move "180" to eightlpi
.END PATCH 1.5 ADDED LOGIC
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
.begin patch 1.70
HOLDFUL  DIM       6
.end patch 1.70
HOLDOWN  DIM       4
LINES    FORM      2
PAGE     FORM      5
.Patch1.5 Var Modified to protext the innocent
PBREAK   FORM      "9180"
.PBREAK   FORM      "46"
.Patch1.5 Var Modified
COUNTO   FORM      8                  NUMBER OF ORDERS READ.
Output   File
.START PATCH 1.55
FAXLIST  File
.END PATCH 1.55
save     dim       47
fhandle  dim        4                   .use to create fax files.
spoolfle DIM       40                   .order  SPOOL FILEs
faxname  dim       45
faxtele  dim       10
faxattn  dim       45
.begin patch 1.59
FontO7		font
FontO18B	font

	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
.end patch 1.59
.
. .............................................................................
*******************************************************************************
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F3
         MOVE      "EXIT" TO PF3
         MOVE      "NSHP0007" TO PROGRAM
         MOVE      "Nightly SHIPPING REPORT" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         move      c2 to nshplock	 .record locking
			move	    c3,nordlock		 .no locking
*****************
*****************
.

PassOne
.Patch1.55
          move     "550000" to nordfld
.Patch1.55
.Patch1.51
.          move     "500000" to nordfld
.Patch1.51
.          move     "400000" to nordfld
.START PATCH 1.55
.	  OPEN      FAXLIST,"FAXLIST"
	  OPEN      FAXLIST,"\\nins1\e\data\text\FAXLIST"
.END PATCH 1.55	 
          CLOCK     DATE TO PDATE
          move      pdate to date
          UNPACK    DATE INTO MM,str1,DD,str1,YY
          CALL      CVTJUL
          MOVE      JULDAYS TO SYSJDATE
          add       c3 to sysjdate
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
        if       (str1 = "1" or str1 = "5" or str1 ="6")
        move     c2 to osflag
        endif
.For testing
.                    goto passtwo
Pass1Loop call     nordks
          goto     eoj1 if over
          add      c1 to counto
          DISPLAY   *P15:10,"RECORDS IN = ",COUNTO;
          if        (olrn = "414521")
          call      debug
          endif
.
          match     "019539",olnum                .animal place
          goto      pass1loop if equal
.

.start patch 1..60
          match     "014900",olnum     .Alaska Conservation Foundation
          goto      pass1loop if equal
.end patch 1.60

.patch1.4.1
          match     "021334",olnum     .Kerry for Pres
          goto      pass1loop if equal
.patch1.4.1
.patch1.34
          match     "002312",olnum     .Nat'l African Wildlife skip
          goto      pass1loop if equal
.endpatch1.34
.patch1.35
          match     "007823",olnum     .Ntl urban league per SKIP
          goto      pass1loop if equal
.endpatch1.35
.patch1.36
          match     "020565",olnum     .WaterKeeper Allianace  per JN SKIP
          goto      pass1loop if equal
.endpatch1.36
.START PATCH 1.55
.Begin Patch 1.73
	move	c1,ndatpath
	packkey	Ndatfld,Olnum
	call	Ndatkey
.end Patch 1.73


NOFAX
	  READ      FAXLIST,seq;STR6
	  if        not over
	     	if        (str6 = olnum)
	     	goto      pass1loop if equal
	     	else
	     	goto      NOFAX
	     	endif
	  else
	  endif
.END PATCH 1.55
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
          match      "0001",ortnnum       .REUSE
          goto       pass1loop if equal
.begin patch 1.2
          move       c0 to n4
          move       ortnnum to n4
          branch     n4 of pass1loop
.end patch 1.2
          clear     str2
          pack      str2 from OSALES10,osales
          move      c0 to n2
          move      str2 to n2
.          if        (n2 = 6 or N2 = 19)           .List Management?
.begin patch 1.73
.          if        (n2 = 6 or N2 = 19 or N2 = 27 or N2 = 28 or )           .List Management?
          if        (n2 = 6 or N2 = 19 or N2 = 27 or N2 = 28 or ELSTCDE = "N" or ELSTCDE = "P")           .List Management or exclusive? (some overlap on logic)
.end patch 1.73
          goto      checkdate
          else
          goto      pass1loop
          endif
.
CheckDate
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
          if        (juldays <= sysjdate)
          goto      checktwo
          else
.          call      debug
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
.START PATCH 1.32 REPLACED LOGIC
.          SCAN      "TDMC" IN OWNCTN
.dave goes bad for E mag
.Start Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.//.                   if        (OWNLON = "4017")       ."E Magazine
.//;START PATCH 1.32 REPLACED LOGIC
.//;                   move       ful10 to ownctn
.//.
.//.			move	"0010",ownctn	.KABLE
.//;END PATCH 1.32 REPLACED LOGIC
.//.                   endif
.//.	call	Trim using OWNCTN
.End Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.Start Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.begin patch 1.73   not our file forget about it
.	if      (OLNUM = "015003")       .E (The Environmental Magazine) List
.		Move "009390",OFULLFIL	.Still KABLE
.	endif
.end patch 1.73   not our file forget about it
	call	Trim using OFULLFIL
.End Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 1.56 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"checktwo-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULVARS
.	endif
.	scan	"TDMC",NFULCOMP
.Start Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"checktwo-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear 	NFULCOMP
.			clear	NFULCNT
.			clear	NFULFAX
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear 	NFULCOMP
.				clear	NFULCNT
.				clear	NFULFAX
.			else
.				move	COMPCOMP,NFULCOMP
.				move	CNCTFNAME,NFULCNT
.				move	COMPFAX,NFULFAX
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear	COMPFLD6
.		clear 	NFULCOMP
.		clear	NFULCNT
.		clear	NFULFAX
.	endif
.	scan	"TDMC",NFULCOMP
.END PATCH 1.56 REPLACED LOGIC
.;END PATCH 1.32 REPLACED LOGIC
.          GOTO      Pass1Loop IF EQUAL            .triplex we get info a diff way
.End Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.Start Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"checktwo-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear 	NFULCOMP
			clear	NFULCNT
			clear	NFULFAX
.begin patch 1.73
			clear	NFULEMAIL
.end patch 1.73
		else
.Grab the active contact full name if there is one
			Packkey CNCTFLD2 to "01X",COMPNUM
			Call	CNCTAIM
			loop
			until over
			until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
				call	CNCTKG
			repeat
			move	COMPCOMP,NFULCOMP
			move	CNCTFNAME,NFULCNT
			move	COMPFAX,NFULFAX
.begin patch 1.73
			Move	CompEmail,NFULEMAIL

.end patch 1.73
		endif
	else	// OFULLFIL = ""
		clear	COMPFLD
		clear 	NFULCOMP
		clear	NFULCNT
		clear	NFULFAX
.begin patch 1.73
		clear	NFULEMAIL
.end patch 1.73
	endif
	
	Goto	Pass1Loop If (OFULLFIL = "009406")	.Donnelley/Triplex/TDMC
	
.End Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.END PATCH 1.56 REPLACED LOGIC

          write     Output,seqeof;ordvars
          goto      Pass1Loop
eoj1
.close - sort
         weof       output,seqeof
         DISPLAY   *P15:23,*EL,"Sorting records = ";
         close      nordfile
         close      output
         
.begin patch 1.7
.        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,214-247"
        pack    taskname,"\\nins1\e\data\diskin71.tmp,\\nins1\e\data\diskin71.dat;22-25,329-334,214-247"
.end patch 1.7
        sort    taskname
        if over
                move    s$error$,error
	         DISPLAY   *P15:23,*EL,"Sorting Error = ",Error,*w5,*b,*w5;
                stop
        endif

.passtwo -
passtwo
         move       C0 TO COUNTo
         CALL      FUNCDISP
         clear   taskname
         clear   holdown
.begin patch 1.7
	Clear 	HoldFul         
.end patch 1.7
         move   c0 to nordflag
         PACK   STR35,NTWKPATH1,"DISKIN71.dat"
         open      output,str35,exclusive
         MOVE      "                    " TO FERROR
.
GETREC   DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
.
         read      output,seq;ordvars
         GOTO      EXIT IF OVER
         ADD       C1 TO COUNTO
         DISPLAY   *P15:12,"Printing Record = ",COUNTO;
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
.START PATCH 1.32 REPLACED LOGIC
.Patch1.33
.StartEnd Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
//.;DB goes Crazy for E mag
//.;SendShipping to KABLE
//.                   if        (OWNLON = "4017")       ."E Magazine
//.                             move "0010",ownctn      .KABLE
//.                   endif
//.;End Subpatch1.33
//.                    call	Trim using OWNCTN
.End Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.Start Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                                       
.SendShipping to KABLE
.begin patch 1.73   not our file forget about it
.	if      (OLNUM = "015003")       .E (The Environmental Magazine) List
.		Move "009390",OFULLFIL	.Still KABLE
.	endif
.end patch 1.73   not our file forget about it
	call	Trim using OFULLFIL
.End Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.START PATCH 1.56 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"GETREC-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULVARS
.	endif
.Start Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"GETREC-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear 	NFULCOMP
.			clear	NFULCNT
.			clear	NFULFAX
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear 	NFULCOMP
.				clear	NFULCNT
.				clear	NFULFAX
.			else
.				move	COMPCOMP,NFULCOMP
.				move	CNCTFNAME,NFULCNT
.				move	COMPFAX,NFULFAX
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear	COMPFLD6
.		clear 	NFULCOMP
.		clear	NFULCNT
.		clear	NFULFAX
.	endif
.End Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.Start Patch 1.58 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                           	
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"GETREC-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear 	NFULCOMP
			clear	NFULCNT
			clear	NFULFAX
		else
.Grab the active contact full name if there is one
			Packkey CNCTFLD2 to "01X",COMPNUM
			Call	CNCTAIM
			loop
			until over
			until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
				call	CNCTKG
			repeat		
			move	COMPCOMP,NFULCOMP
			move	CNCTFNAME,NFULCNT
			move	COMPFAX,NFULFAX
		endif
	else	// NFULLFIL = ""
		clear	COMPFLD
		clear 	NFULCOMP
		clear	NFULCNT
		clear	NFULFAX
	endif
.End Patch 1.58 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	                   
.END PATCH 1.56 REPLACED LOGIC
.END PATCH 1.32 REPLACED LOGIC
         MATCH     NOWNFLD TO HOLDOWN
         CALL      BREAK IF NOT EQUAL
.begin patch 1.70
	if	(OFullfil <> HOldFul)
	call	Break
	endif
.end patch 1.70
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
         move      c0 to n10
         move      faxtele to n10
         compare   c0 to n10
         if        not equal
         type      faxtele
                if  equal
	         move      faxtele to ownfax
        	 endif
         endif
.         if        (ownfax <> " " & ownfax <> "")
         move      telemask to fax1
.         edit      ownfax to fax1
         edit      faxtele to fax1

.         move      "Fax ##" to fax2
.         endif
.Patch 1.5 Commented Out
.         if         (page = c1)
.Patch 1.5 Commented Out
.Patch 1.5 New Logic
         if         (page <> c1)
        prtpage PrintDoc;*NEWPAGE:
        	       			*UNITS=*HIENGLISH:
                        *ORIENT=*PORTRAIT;
			endif
.Patch 1.5 New Logic
.         PRINT     HPDTCH10,hpfixed,*n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
.         PRINT     *n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
.                   PDATE:
.START PATCH 1.32 REPLACED LOGIC
.         PRINT     *n,hpt700,"DATE:":
.                   PDATE:
.                   *l,hpt700,"PAGE ## ",PAGE:
..                   *L,*40,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                   *n,*n,*n,*n,*n:
.                    hpt225,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
..                   *119,"PAGE ## ",PAGE:
..                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
..                   *L,*21,OWNLONM,tele1:
..                   *L,*21,OWNOCPY,fax1,b1,fax2:
..                   *l,*21,faxattn:
..                   *L,*21,OWNOCPY:
..                   *L,*21,OWNCTN," Via - Fax ",fax1
.                   *n,*l,hpt125,faxattn:
.                   *L,hpt125,OWNOCPY:
.                   *L,hpt125,OWNCTN," Via - Fax ",fax1
..............
.Comment Out print statements Patch1.5
.         PRINT     *n,hpt700,"DATE:":
.                   PDATE:
.                   *l,hpt700,"PAGE ## ",PAGE:
.                   *n,*n,*n,*n,*n:
.                    hpt225,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                   *n,*l,hpt125,faxattn:
.                   *L,hpt125,OWNOCPY:
.                   *L,hpt125,NFULCOMP," Via - Fax ",fax1
.Comment Out print statements Patch1.5
.END PATCH 1.32 REPLACED LOGIC
.         else
.         PRINT     *F,HPDTCH10,hpfixed,*n,*50,"N A M E S   I N   T H E   N E W S",*119,"DATE:":
.START PATCH 1.32 REPLACED LOGIC
.         PRINT     *F,*n,hpt700,"DATE:":
.                   PDATE:
.                   *l,hpt700,"PAGE ## ",PAGE:
.                   *n,*n,*n,*n,*n:
.                   hpt225,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
..                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
..                   *L,*21,OWNLONM,tele1:
..                   *L,*21,OWNOCPY,fax1,b1,fax2:
.                   *n,*l,hpt125,faxattn:
.                   *L,hpt125,OWNOCPY:
.                   *L,hpt125,OWNCTN," Via - Fax ",fax1
...................
.Patch1.5 Comment Out
.         PRINT     *F,*n,hpt700,"DATE:":
.                   PDATE:
.                   *l,hpt700,"PAGE ## ",PAGE:
.                   *n,*n,*n,*n,*n:
.                   hpt225,"S h i p m e n t   I n f o r m a t i o n   R e q u e s t":
.                   *n,*l,hpt125,faxattn:
.                   *L,hpt125,OWNOCPY:
.                   *L,hpt125,NFULCOMP," Via - Fax ",fax1
.Patch1.5 Comment Out
.Patch1.5 Replaced Logic
.begin patch 1.59
	If	(Ocompid2 = "P")            .pacific lists mangement list order
	prtpage	PrintDoc;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=451:343,*font=fontO7,"1300 Clay St. 11th Floor":
		*p=451:443,"Oakland, CA 94612-1492":
		*p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=317:643,"A Division of Names in the News"

	else
	prtpage	PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
	endif
	
				
.				prtpage	PrintDoc;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1000:400:8400:NINLogo
.end patch 1.59
				move "300" to row
				Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"DATE:";
				Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PDATE;
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"PAGE ## ";
				Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,PAGE;
				add eightlpi to ROW
				add "600" to Row
				Prtpage PrintDoc;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=TimesNew10,*ll,		"S h i p m e n t   I n f o r m a t i o n   R e q u e s t";
				add eightlpi to ROW
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,faxattn;
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ownocpy;
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn4:row,*ALIGNMENT=*LEFT,*font=TimesNew10,nfulcomp;
				Prtpage PrintDoc;*font=TimesNew10,"Via  - Fax ";
				Prtpage PrintDoc;*font=TimesNew10,fax1;
				add eightlpi to ROW
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Rtn/Mail/";
				Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Order/Ship";
				Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Tracking";
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"LR ##";
				Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"List/Mailer";
				Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shp Date";
				Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Quantities";
				Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Number";
				add eightlpi to ROW
.Patch1.5 Replaced logic
.Patch1.5 End Comment out
.END PATCH 1.32 REPLACED LOGIC
.         endif
.         PRINT     *N,*50,"RTN/MAIL",*60,"QUANTITY",*72,"SHIPPED":
.Patch1.5 Comment Out
.         PRINT     *N,hpt400,"Rtn/Mail/",hpt500,"Order/Ship",hpt600,"Tracking":
.                   *115,"POSTAGE":
.                   *81,"SHIPPING Method":
.                   *L,*1,"LR ##",*10,"MAILER/LIST",*52,"DATE":
.                   *L,*1,"LR ##",hpt075,"List/Mailer",hpt400,"Shp Date":
.                   *60,"ORD/SHIP",*74,"DATE",*81,"AMOUNT":
.                   hpt400,"ORD/SHIP",*74,"DATE",*115,"AMOUNT":
.                   hpt500,"Quantities",hpt600,"Number"
.                   *81,"Tracking Number"
.         MOVE      "12" TO LINES
.Patch1.5 Comment Out

         RETURN
*......................................................................
PRINT
.Patch 1.5 Commented Out
.         COMPARE   PBREAK TO LINES
.         CALL      HEADER IF NOT LESS
.         COMPARE   C0 TO LINES
.         CALL      HEADER IF EQUAL
.         MOVE      ORDMASK TO ORDQTY
.         MOVE      OQTY TO QTYNUM
.         EDIT      QTYNUM TO ORDQTY
.Patch 1.5 Commented Out
.Patch 1.5 Logic Added
         COMPARE   PBREAK TO ROW
         CALL      HEADER IF NOT LESS
.         COMPARE   "300" TO ROW
.         CALL      HEADER IF EQUAL
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO ORDQTY
.Patch 1.5 Logic Added
.
.         PRINT     *L,*1,hpbon,HPDTCH12,hpfixed,OLRN,HPDTCH10,hpfixed:
.Patch 1.5 Comment Out
.         PRINT     *L,*1,hpbon,OLRN,hpt075,O1DES,hpboff:
.         	    hpt400,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                    hpt500,ORDQTY,Hpt600,"______________________________":
.         	    hpboff,*10,MCOMP,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                    *l,hpt075,MCOMP:
.		    hpt400,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *58,ORDQTY,*71,"__/__/____",*82,"$______",*91,"______________________________":
.                   hpt400,ORDQTY,*71,"__/__/____",*82,"______________________________",*115,"$__________":
.                   *59,"__________",*91,"______________________________":
.                   hpt400:
.                   *L,*1,Hpbon,*10,O1DES,hpboff,b3,O2DES:
.                   *l,*3,"Shipped Via: ","____________________________________________":
.                   hpt400,"__/__/____",hpt500,"__________",hpt600,"Ship Cost: ","$__________"
.         ADD       C4 TO LINES
.Patch 1.5 Comment Out
.Patch 1.5 Replaced Logic
		add eightlpi to ROW
		Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,OLRN;
		Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*boldon,O1DES,*boldoff;
		PACK STR10 WITH ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
		Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
		Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,ORDQTY;
		Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"______________________________";
		add eightlpi to ROW
		Prtpage PrintDoc;*pcolumn3:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,MCOMP;
		PACK STR10 WITH OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
		Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,STR10;
		add eightlpi to ROW
		Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipped Via:";
		Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"____________________________________________";
		Prtpage PrintDoc;*pcolumn6:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__/__/____";
		Prtpage PrintDoc;*pcolumn7:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"__________";
		Prtpage PrintDoc;*pcolumn8:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Shipping Cost: ";
		Prtpage PrintDoc;*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"$__________";
		add eightlpi to ROW
.Patch1.5 Replaced Logic
         GOTO      GETREC
BREAK
         call       sendit
.

         move      c0 to outflag              .reset flag
.dave goes bad for E mag
.                   if        (OWNLON = "4017")       ."E Magazine
.START PATCH 1.32 REPLACED LOGIC
.                   move       ful10 to ownctn
.			move	"0010",ownctn	.KABLE
.END PATCH 1.32 REPLACED LOGIC
.                   endif
.
.START PATCH 1.32 REPLACED LOGIC
.         if        (ownctn = " " or ownctn = "" or ownctn = "               " )   .no cc to?
.         call      debug
.         MOVE      C0 TO fulnum                    .reset array pointer
.         clear     faxtele
.         goto      chkful2x                 .correct
.         endif
.         move       c1 to fulnum
..
.CHKFUL2  clear     str15
.         clear     faxtele
.         LOAD      STR15 FROM fulnum OF FUL1,FUL2,FUL3,FUL4,FUL5:
.                    FUL6,FUL7,FUL8,FUL9,FUL10,FUL11,ful12,FUL13,ful14,ful15:
.                    ful16,ful17,ful18,ful19,ful20,ful21,ful22,ful23,ful24,ful25,ful26
..make sure all caps for match
.         clear      save
.         move       ownctn to save
.         rep        LowUp in save
.         reset      save
..
.         match      save in STR15     .fax this one?
.	         if         not equal
.	         add       c1 to fulnum
.	        	 if        (fulnum > 27)
.        		 goto       chkful2x
.	                 endif
.	         else
.	         goto       chkful2x
.        	 endif
.         goto      chkful2
.
..
.chkful2x
.         move       nownfld to fhandle
.         move      "           " to faxname
.         clear     faxattn
.         LOAD      FAXattn FROM fulnum OF FULcnt1,FULcnt2,FULcnt3,FULcnt4,FULcnt5:
.                   FULcnt6,FULcnt7,FULcnt8,FULcnt9,FULcnt10,FULcnt11,fulcnt12:
.                   FULcnt13,fulcnt14,fulcnt15,fulcnt16,fulcnt17,fulcnt18,fulcnt19:
.                   fulcnt20,fulcnt21,fulcnt22,fulcnt23,fulcnt24,fulcnt25,fulcnt26
.         if        (faxattn = " " or faxattn = "")
.         move      "Order Fulfillment" to faxattn
.         endif
.         move      faxattn to faxname
..note if its 10 check for which list
......................................
.	 clear     faxtele
.	 LOAD      FAXTELE FROM fulnum OF FULTEL1,FULTEL2,FULTEL3,FULTEL4,FULTEL5:
.	           FULTEL6,FULTEL7,FULTEL8,FULTEL9,FULTEL10,FULTEL11,fultel12:
.	           FULTEL13,fultel14,fultel15,fultel16,fultel17,fultel18,fultel19:
.	           fultel20,fultel21,fultel22,fultel23,fultel24,fultel25,fultel26
..insert check for email here!!!!!!!!!!!!!!!!!
.	 move      c0 to n10
.	 if        (fulnum >= 27 or fulnum  = 0 )
.	 move      ownfax to faxtele
.	 endif
.........................................
.begin patch 1.73
.		ADD code here to do Email instead of fax - so would create PDF and email the request
.	make print subroutine called so can use for both faxing and email.

	goto	SkippEmail
	Move	"Order Fulfillment",MailSubct
	Move	NFAXEmail,MailTO
	MOve	"JoeyGamache@nincal.com",MailFrom
	
	
SkippEmail	
.end patch 1.73
.........................................
	clear	faxtele
	clear	faxattn
	move	NOWNFLD,fhandle
	move	"           ",faxname
	call	Trim using NFULCNT
	if (NFULCNT = "")
		move	"Order Fulfillment",faxattn
	else
		move	NFULCNT,faxattn
	endif
	move	faxattn,faxname
	call	Trim using NFULFAX
	move	C0,N10
	move	NFULFAX,N10
	if (N10 > 0)
		move	NFULFAX,faxtele
	else
.begin patch 1.72 exception for Sherene   02/15/2008
		if	(Olnum <> "017432")   Whitney museum & One Source productions
		move	ownfax,faxtele
		endif
.end patch 1.72 exception for Sherene
	endif
	move	C0,N10
.END PATCH 1.32 REPLACED LOGIC

         move      faxtele to n10
.
         clear     spoolfle
         append    "\\nts0\d\data\fax\nSHP",spoolfle
         append    nownfld to spoolfle
.begin patch 1.7
	IF	(OFullfil <> "")
 	Append	OFullFil,spoolfle        
 	else	
 	Append	"000000",Spoolfle
 	endif
.end patch 1.7
         APPEND    ".LST" TO spoolfle
         reset     spoolfle
.patch 1.5
         clear     tempfle
         append    "\\nts0\d\data\fax\nSHP",tempfle
         append    nownfld to tempfle
.begin patch 1.7
	IF	(OFullfil <> "")
 	Append	OFullFil,tempfle        
 	else	
 	Append	"000000",tempfle
 	endif
.end patch 1.7
         APPEND    ".TMP" TO tempfle
         reset     tempfle
			PRTOPEN PrintDoc,"faxfile","Faxfile.prn"
			PRTPAGE PrintDoc;*UNITS=*HIENGLISH:
			*ORIENT=*Portrait:
			*MarginL=1;
.         SPLOPEN    spoolfle,"Q"
.Patch1.5
         compare   c0 to n10
         if        equal
.printing  not faxing - no or invalid fax number
.         PRINT     hp17ptch,hptop                .compressed
.dave testing
.patch1.5
.         print     *n,032,hpreset:
.                   hpttray:
.                   hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F"
.         call       PortraitLTRHEAD
.patch1.5
.end of test
         	bump      spoolfle by 16                 .for next fax.
	         reset     spoolfle
   	      move      c1 to outflag                  .will be copied to the printer
         ELSE
      	   move      c2 to outflag                  .will be copied to fax
.START PATCH 1.53 REMOVED LOGIC
.Note that OLON is the Owner Number and "002691" is a List Number!!
.;dave goes bad for AFSC at Metro
.            if        (OLON = "002691")       ."American Friends Services at Metro
.            	move      c4 to outflag
.            endif
.END PATCH 1.53 REMOVED LOGIC
	         COUNT     N2,faxtele
            COMPARE   C10 TO N2
            IF        EQUAL
   	         MOVE      C1 TO LONGDIST
      	      UNPACK    faxtele INTO STR3,STR7
         	   MATCH     "510" TO STR3           .LOCAL ?
	            IF         EQUAL
		            MOVE       STR7 TO faxtele
	            	CLEAR      LONGDIST
            	else
		            MATCH      B3 TO STR3           .LOCAL ?
		            IF         EQUAL
			            MOVE       STR7 TO faxtele
		   	         CLEAR      LONGDIST
		            ENDIF
	            ENDIF
            ENDIF
.                  print     "^[D","14154337796","^[N",faxname,"^]":
.Patch1.5
.for testing
.                    move "4154337796" to faxtele
.for testing
			pack	taskname,"c:\work\hdrfile.prn"
			splopen	taskname
			print   hpreset,Hpcour,"^[D",longdist,faxtele,"^[N",faxname,"^]",Hpfixed;
			splclose
.For testing
.			move c1 to outflag
.Patch1.5 Comment Out
.                  print     "^[D",longdist,faxtele,"^[N",faxname,"^]":
.                   *n,032,hpreset,hpttray,hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F"
.				        call       PortraitLTRHEAD
.Patch1.5
.end of test
         endif

         move      c0 to page
         CALL      HEADER
breakexit
         move     nownfld to holdown
.begin patch 1.7
	mOVE	ofullfIL,hoLDfuL         
.end patch 1.7
         RETURN
sendit
         if         (counto <> c1)
.Patch 1.5 Logic Added
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Please fax Shipping information back to (510)628-8313.";
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"If you did not receive a listed order, Please let us know immediately.";
				add eightlpi to ROW
				add eightlpi to ROW
				Prtpage PrintDoc;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,"Names in the News - List Management (415)989-3350.";
				prtclose PrintDoc
.START PATCH 1.52 REPLACED LOGIC
.				Rename "\\nts0\d\data\fax\faxfile.prn",SPOOLFLE
				Pause	"5"               .make sure done with file - sigh my machine is to fast dh
				Rename "C:\WORK\faxfile.prn",SPOOLFLE
.END PATCH 1.52 REPLACED LOGIC
.Patch 1.5 Logic Added
.Patch1.5 Comment Out
.      	   print     *n,*l,"Please fax Shipping information back to (510)628-8313.":
.                   *n,*1,"If you did not receive a listed order, Please let us know immediately.":
.                   *n,*n,*1,"Names in the News California - List Management (415)989-3350."
.         splclose
.Patch 1.5 Comment Out
.here check outflag and copy to fax, printer, acrobat/email
         clear     taskname
.Patch 1.5 Comment Out
.testing
.	move c1 to outflag
         if        (outflag = 1)   .to printer
.	           if        (osflag = c1)         .win 95 98
.                   append    "!c:\command.com",taskname
.                   endif
.	           if        (osflag = c2)         .winnt or win 2000
.                   append    "c:\winnt\system32\cmd.exe",taskname
.                   endif
.         append    " /c copy ",taskname
.         append    spoolfle to taskname
.         append    " \\nts0\Laser8",taskname
.         reset     taskname
.         execute   taskname
.Patch 1.5 Comment Out
.Patch 1.5 Logic Updated
			Call GETWINVER
			clear	copyvar2
			if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
				append	"!c:\winnt\system32\cmd.exe",CopyVar2
				append  " /c copy ",CopyVar2
			elseif (OSFLAG = "6")  .XP
				append	"!c:\windows\system32\cmd.exe",CopyVar2
				append  " /c copy ",CopyVar2
			else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
				append	"!c:\command.com",CopyVar2
				append  " /c copy ",CopyVar2
			endif
         append    spoolfle to CopyVar2
			append  " \\nts0\laser8",CopyVar2
			reset   CopyVar2
			execute CopyVar2
.			erase	SPOOFLE
			erase "c:\work\hdrfile.prn"
.Patch 1.5 Logic Updated
         pause     c2
	         if        not over            .execute was ok
                 call      debug
.begin patch 1.7
	call	Trim using Spoolfle
   	     MOVELPTR  Spoolfle,n2 
                 scan      period in spoolfle
                 lenset    spoolfle
                 clear     str40
                 reset     spoolfle
                 append    spoolfle to str40
                 append    "SNT" to str40
                 reset     str40
.                 reset     spoolfle to 40
                 reset     spoolfle,n2
.end patch 1.7
                 reset     spoolfle
	       	 rename    spoolfle,str40
	         endif
         endif
.
         if        (outflag = 2 or outflag = 4)    .to fax
.Patch 1.5 Comment Out
.	           if        (osflag = c1)         .win 95 98
.                   append    "!c:\command.com",taskname
.                   endif
.	           if        (osflag = c2)         .winnt or win 2000
.                   append    "!c:\winnt\system32\cmd.exe",taskname
.                   endif
.Patch 1.5 Comment Out
.Patch 1.5 Logic Updated
				Call GETWINVER
				clear	copyvar2
				if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
					append	"!c:\winnt\system32\cmd.exe",CopyVar2
					append  " /c copy ",CopyVar2
				elseif (OSFLAG = "6")  .XP
					append	"!c:\windows\system32\cmd.exe",CopyVar2
					append  " /c copy ",CopyVar2
				else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
					append	"!c:\command.com",CopyVar2
					append  " /c copy ",CopyVar2
				endif
.Patch 1.5 Logic Updated
.Patch 1.5 Comment Out
.         append    " /c copy ",taskname
.         append    spoolfle to taskname
.         append    " \\NTS2\FAX",taskname
.         reset     taskname
.         execute   taskname
.Patch 1.5 Comment Out
.Patch 1.5 Logic Updated
				append  "c:\work\hdrfile.prn /b + ",CopyVar2
				append  SPOOLFLE,CopyVar2
				append	" /b ",CopyVar2
				append  TEMPFLE,CopyVar2
.				append	" /b ",CopyVar2
.				add	C1,howmany
.				move	howmany,str9
.				call	Trim using str9
.				pack	taskname,NTWKPATH1,"datafax",str9,".fax"
.				append  taskname,CopyVar2

				reset   CopyVar2
				execute CopyVar2
				clear   CopyVar2
				erase	SPOOLFLE
				rename TEMPFLE,SPOOLFLE
				if (OSFLAG = "1" or OSFLAG = "5")  .NT4,NT5
					append	"!c:\winnt\system32\cmd.exe",CopyVar2
				elseif (OSFLAG = "6")  .XP
					append	"!c:\windows\system32\cmd.exe",CopyVar2
				else (OSFLAG = "3" or OSFLAG = "4")  .WIN95,98
					append	"!c:\command.com",CopyVar2
				endif
				append  " /c copy ",CopyVar2
				append  SPOOLFLE,CopyVar2
				append  " \\nts3\fax",CopyVar2
				reset   CopyVar2
				execute CopyVar2
				erase "c:\work\hdrfile.prn"
.Patch 1.5 Logic Updated

         pause     c2
	         if        not over            .execute was ok
                 call      debug
.begin patch 1.7
	call	Trim using Spoolfle
   	     MOVELPTR  Spoolfle,n2 
                 
                 scan      period in spoolfle
                 lenset    spoolfle
                 clear     str40
                 reset     spoolfle
                 append    spoolfle to str40
                 append    "SNT" to str40
                 reset     str40
.                 reset     spoolfle to 40
                 reset     spoolfle,n2
.end patch 1.7
                 reset     spoolfle
	       	 rename    spoolfle,str40
	         endif
         endif
.
         if        (outflag = 3 or outflag = 4)    .to email
.START PATCH 1.53 REMOVED LOGIC
.Note that OLON is the Owner Number and "002691" is a List Number!!
.;dave goes bad for AFSC at Metro
.                   if        (OLON = "002691")       ."American Friends Services at Metro
.                   clear     taskname
.                   append    "\\nts0\c\apps\pcl2pdf\pcl2pdf32 ",taskname
.                   append    str40 to taskname
.                   append    " \\nts0\d\data\fax\nshp5260.pdf /S",taskname
.
.	           reset     taskname
.        	   execute   taskname
.	           pause     c5
.
.                   endif
.END PATCH 1.53 REMOVED LOGIC
         endif
.
         endif

        return
        
        
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
          call       sendit
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
.Patch1.4
			include	compio.inc
			include	cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch1.4
.Begin Patch 1.73
	Include	Ndatio.inc
.end Patch 1.73
         INCLUDE   NORDIO.inc
         include   hpio.inc
.START PATCH 1.56 REMOVED LOGIC
.;START PATCH 1.32 REPLACED LOGIC
.	INCLUDE	NFULIO.INC
.;END PATCH 1.32 REPLACED LOGIC
.END PATCH 1.56 REMOVED LOGIC
         INCLUDE   COMLOGIC.inc


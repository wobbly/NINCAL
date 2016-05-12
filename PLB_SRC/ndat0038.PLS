PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
         include  nowndd.inc
.START PATCH 1.4 REPLACED LOGIC
..START PATCH 1.2 ADDED LOGIC
.	include	nfuldd.inc
..END PATCH 1.2 ADDED LOGIC
	include	   compdd.inc
	include	   cntdd.inc
.END PATCH 1.4 REPLACED LOGIC
release	 init	    "1.41"	   JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.RELEASE  INIT	   "1.4"      DMS 21JUN06  FULFILLMENT CONVERSION
.RELEASE  INIT      "1.3"      ASH 30JAN04  DATACARD CONVERSION
.RELEASE  INIT      "1.2"      ASH 04FEB2002	NINFUL CONVERSION
.RELEASE  INIT      "1.1"      ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "R1.0"      DLH 21Feb97
....................................................................
inPUT    FILE      
OUTPUT   FILE      
OUTPUT2  FILE      
OUTPUT3  FILE      
....................................................................
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
. 
...............................................................................
.misc VARIABLES
akey2    init       "02R"
sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
DATMO    DIM       2           .used to 
DATDAY   DIM       2           .hold the
DATYR    DIM       2           .datacard revised date.
ORDDATE  FORM      5           .order date in julian
CARDATE  FORM      5           .datacard date in julian
date     DIM       8
today1   FORM      5 
date1    dim       8
CHECK    FORM      5
check2   form      5
BLNK3    DIM       3
COUNT    FORM      5
count1   form      5
count2   form      5
USED     FORM      5
NOTUSED  FORM      5
OPTFLAG  FORM      1
....................................................................
         MOVE      "Ndat0038" TO PROGRAM
         MOVE      "Find Unused Exclusive DATACARDS" TO STITLE
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         clock     date to today
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
. 
         TRAP      ABORT IF F5
. 
. 
....................................................................
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO SYSMO,str1,SYSDAY,str1,SYSYR
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      "01" TO MM
         MOVE      "01" TO DD
         MOVE      "96" TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
. 
....................................................................
....................................................................
BEGIN
.START PATCH 1.1 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\Dataold"
.         prepare   output2,"g:\data\dataold1"
.         prepare   output3,"g:\data\datantd"
.         open      input,"g:\data\nindat.me",exclusive
.
         PACK      STR25,NTWKPATH1,"Dataold"
         PACK      STR35,NTWKPATH1,"Dataold1"
         PACK      STR45,NTWKPATH1,"datantd"
         PACK      STR50,NTWKPATH1,"nindat.me"
         PREPARE   OUTPUT,STR25
         prepare   output2,STR35
         prepare   output3,STR45
         open      input,STR50,exclusive
.END PATCH 1.1 REPLACED LOGIC
.         call       ndattst
         GOTO      A100
         MOVE      C1 TO NDATFLAG
         GOTO      A100
....................................................................
. 
. GET NEXT DATA CARD
. 
A100
         read      input,seq;datvars
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
         add       c1 to count
        display   *p10:13,"records read ",count
         cmatch    "C" to elstcde        .exclusive ?
         goto      a100 if not equal         .no, skip
         scan      "OFFICE USE" in mlstname
         goto      a100 if equal
         scan      "Office Use" in mlstname
         goto      a100 if equal
. 
CHKRUN   RESET     RUNCODES
         SCAN      lstnum IN RUNCODES         .running charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
         CMATCH    "W"  TO status        .withdrawn ?
         GOTO      A100 IF EQUAL         .yes skip
         reset     mlstname
.

ORDINFO  REP       zfill IN LSTNUM
         PACK      NORDFLD2,AKEY2,LSTNUM
         CLEAR     NORDFLD1
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
. 
         display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
         CALL      NORDAIM
         GOTO      CARDNG IF OVER
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         Compare   check to CHECK2           .usage in time period?
         goto      a100 if less
         GOTO      B100         .BIG DEAL, TRY AGAIN.

CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
         ADD       c1 TO NOTUSED
         DISPLAY   *P12:20,"NUMBER OF LISTS WITH NO USAGE : ",NOTUSED
         GOTO      b101
. 
. COMPUTE ORDER USAGE
. 
B100
        display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
         CALL      NORDKG
         GOTO      C100 IF OVER
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         Compare   check to CHECK2           .usage in time period?
         goto      a100 if less
         GOTO      B100         .BIG DEAL, TRY AGAIN.
B101   
. 
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
.  
C100
.START PATCH 1.3 REPLACED LOGIC
.         move        ownnum to nownfld
	unpack	ownnum,str2,nownfld
.END PATCH 1.3 REPLACED LOGIC
         rep         zfill in nownfld
         call        nownkey

         clear      mm
         clear      dd
         move       "19" to cc
         clear      yy
.START PATCH 1.3 REPLACED LOGIC
.         unpack     revdate into mm,dd,cc,yy
         unpack     revdate,cc,yy,mm,dd
.END PATCH 1.3 REPLACED LOGIC
         CALL      CVTJUL
         MOVE      juldays TO orddate
         MOVE      ORDDATE TO CHECK
         move      today1 to check2
         Compare   check to CHECK2           .revised in time period?
         goto      c110 if not less

.START PATCH 1.2 REPLACED LOGIC
.         scan        "TDMC" in ownctn
.         if           equal
.         goto        c130
.         else
.         goto        c120  
.         endif
.
	call	Trim using OWNCTN
.START PATCH 1.4 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"C100-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
..


.begin patch "1.41"

.	scan	"TDMC",NFULCOMP
.	if (OWNCTN <> "")
.		pack 	COMPFLD6,OWNCTN
.		rep	zfill, COMPFLD6
.		move	C1,COMPPATH
.		move	"C100-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear COMPFLD6
.			clear COMPCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear COMPFLD6
.				clear COMPCOMP
.			endif
.		endif
.	else 	// OWNCTN = ""
.		clear COMPFLD6
.		clear COMPCOMP
.	endif
		if (DATFUL <> "")
			pack	COMPFLD, DATFUL
			rep	zfill, COMPFLD
			move           C1,COMPPATH
			move	"Verify-COMPKEY",Location
			pack	KeyLocation,COMPFLD
			call	COMPKEY
			if over
			clear 	COMPCOMP
			else
				if (COMPSVBFLG <> "T")
               			clear 	COMPCOMP
				endif
			endif
		else	// datful = ""
			clear 	COMPCOMP
		endif
.end patch "1.41"
       if (COMPNUM = "009406")
.	scan	"TDMC",COMPCOMP
.END PATCH 1.4 REPLACED LOGIC
.end patch "1.41"
.	if equal 
		goto c130
	else
		goto c120
	endif
.END PATCH 1.2 REPLACED LOGIC
         goto        c130
.
c110 
. updated but not used
         write     output2,seq;*+,datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
c120
.not triplex not updated or used
         write     output3,seq;*+,datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
c130
.old triplex not updated or used
         write     output,seq;*+,datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
D100
         GOTO      A100
. 
. 
. ABORT - OPERATOR ABORTED JOB. RESULTS NOT VALID
. 
ABORT
         DISPLAY   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
. 
. CLOSE FILE AND EXIT
. 
Z900
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
         WEOF      OUTPUT2,SEQ
         CLOSE     OUTPUT2
         WEOF      OUTPUT3,SEQ
         CLOSE     OUTPUT3
. 
        shutdown   "cls"
        STOP
. 
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         include   nownio.inc
.START PATCH 1.4 REPLACED LOGIC
..START PATCH 1.2 ADDED LOGIC
.	include	nfulio.inc
..END PATCH 1.2 ADDED LOGIC
	include compio.inc
	include cntio.inc
.END PATCH 1.4 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc


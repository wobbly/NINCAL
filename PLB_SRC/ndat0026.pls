pc       equ       0
         INCLUDE   COMMON.inc
         INCLUDE   ndatdd.inc
         INCLUDE   nowndd.inc
         include   cons.inc
.START PATCH 1.2 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.	include	nfuldd.inc
	include compdd.inc
	include cntdd.inc
.END PATCH 1.4 REPLACED LOGIC
.END PATCH 1.2 ADDED LOGIC
NUM      FORM      1

release	 init	    "1.41"	   JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release	 init	   "1.4"	DMS 21JUN2006  FULFILLMENT CONVERSION
.release  init      "1.3"        ASH 28JAN2004  DATACARD CONVERSION
.release  init      "1.2"        ASH 04FEB2002	NINFUL CONVERSION
.release  init      "1.1"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.0"
OUTFILE  FILE      VAR=3002
input    file   
.
         DISPLAY   *P1:1,*ES
         TRAP      EOJ IF F5
         MOVE      "Abort" TO PF5
         move     c1 to ndatpath
         CALL      PAINT
         CALL      FUNCDISP
KEYIN    KEYIN    *P12:14,"1)EXCLUSIVES OR 2)ALL LISTS ",NUM
         GOTO      KEYIN IF EOS
;         move      "DATA100" to ndatnme1
.			open      input,"e:\data\data100"
.START PATCH 1.1 REPLACED LOGIC
.PREP     PREPARE    OUTFILE,"g:\data\datatdmc"
PREP     PACK       TASKNAME,NTWKPATH1,"datatdmc"
        PREPARE    OUTFILE,TASKNAME			
.END PATCH 1.1 REPLACED LOGIC
         
INPUT
.          read      input,seq;datvars
         call      ndatseq
         GOTO      EOJ IF OVER
         cmatch    "W" to status
         goto      input if equal
         add       c1 to n6
         display   *p10:15,"records read = ",n6
         branch    num of check,nocheck
CHECK
.         CMATCH    "C" TO ELSTCDE
.         goto      input if not equal
nocheck
.START PATCH 1.3 REPLACED LOGIC
.         MOVE      Ownnum TO nownfld
	unpack	OWNNUM,str2,str4
	move	str4,nownfld
.END PATCH 1.3 REPLACED LOGIC
         rep       zfill in nownfld
          call     nownkey
.START PATCH 1.2 - REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
..         SCAN      "FIDE" IN OWNCTN
..         SCAN      "ANACAPA" IN OWNCTN
.         goto      write if not equal
.         reset     ownctn
.         goto       input
...........................
.	call	Trim using OWNCTN
.START PATCH 1.4 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"KEYLST-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
..
..	if (NFULFLD = "0026"| nfulfld= "0001"|nfulfld="0003")
..		goto input
..	else
..		scan	"TDMC-FIDE-ANACAP",NFULCOMP
..		if  equal
..			goto input
..		endif
..	endif
..
.check2
.
.       if (NFULFLD = "0001")
. 	    goto write
.		 else
.		 goto input
.	endif
..	else
..		scan	"FIDE",NFULCOMP
..		if equal 
..			goto input
..		endif
..	endif
...
..	if (NFULFLD = "0004")
..		goto input
..	else
..		scan	"ANTARES",NFULCOMP
..		if equal 
..			goto input
..		endif
..	endif
...
..	if (NFULFLD = "0003")
..		goto input
..	else
..		scan	"ANACAPA",NFULCOMP
..		if equal 
..			goto input
..		endif
..	endif
.	reset	NFULCOMP
.END PATCH 1.2 - REPLACED LOGIC
. 
.begin patch "1.41"
.	if (OWNCTN <> "")
.		PACK	COMPFLD6,OWNCTN
.		rep	zfill, COMPFLD6
.		move	C1,COMPPATH
.		move	"KEYLST-COMPKEY6",Location
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
.	else	// OWNCTN = ""
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

check2
       if (COMPNUM = "009410")
 	    goto write
		 else
		 goto input
	endif
	reset	COMPCOMP
.END PATCH 1.4 REPLACED LOGIC         
WRITE    WRITE      OUTFILE,SEQ;datvars
         add       c1 to n5
         display   *p10:16,"records written = ",n5
         GOTO      INPUT
EOJ
         WEOF      OUTFILE,SEQ
         CLOSE     OUTFILE
         shutdown
.         STOP
         include   ndatio.inc
         include   nownio.inc         
.START PATCH 1.2 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.	include	nfulio.inc
	include compio.inc
	include cntio.inc
.END PATCH 1.4 REPLACED LOGIC
.END PATCH 1.2 ADDED LOGIC
         include   comlogic.inc


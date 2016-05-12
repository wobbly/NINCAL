OldtoNewMlr1 external "COMP001A;OldtoNewMlr"
#NUM0	DIM	3
.#MLR0	DIM	4
#MLR0	DIM	6
#COMP0 DIM	6
..............................................................................
.
. NOFRIO INCLUSION
. NIN OFFER IO INCLUSION
.
. FILE NAME : NINOFR
. REC LENGTH: 64 FIXED
. INDEX KEY : 2-8 (4 POSITION MLR#, 3 POSITION OFFER#)
.
. RELEASE	1.2	2014 October 10	DLH old mailer number now 6 bytes
. RELEASE	1.1	17OCT02	ASH ADDED AIM FILE FUNCTIONALITY
..............................................................................
.
NOFRKEY  BRANCH    NOFRFLAG OF NOFR1
         CALL      NOFROPEN
NOFR1    
.patch1.0
			call		zfillit using NOFRFLD
.begin patch 1.2
.			bump		NOFRFLD,2
.end patch 1.2
			unpack	NOFRFLD,#MLR0,#NUM0
			call	   OldtoNewMlr1 using #MLR0,#COMP0
			if (#COMP0 = "")
				setflag over
				return
			endif
			pack	   NOFRFLD,#COMP0,#NUM0

.patch1.0
			trap      IOMssg giving Error if IO
         branch    NOFRLOCK to NOFR1L,NOFR1R,NOFR1N
NOFR1L   FILEPI    1;NOFRFILE
         READ      NOFRFILE,NOFRFLD;OFRVARS
         TRAPCLR   IO
         RETURN
NOFR1R   READLK    NOFRFILE,NOFRFLD;OFRVARS
         TRAPCLR   IO
         RETURN
NOFR1N   READ      NOFRFILE,NOFRFLD;OFRVARS
         TRAPCLR   IO
         RETURN      
..............................................................................
.
. ENTRY POINT : NOFRTST
. REQUIRED    : NOFRFLD
. RETURNED    : 
. DESCRIPTION : TEST KEY
.
NOFRTST  BRANCH    NOFRFLAG TO NOFR2
         CALL      NOFROPEN
NOFR2    
.patch1.0
.begin patch 1.2
.			bump		NOFRFLD,2
.end patch 1.2
			unpack	NOFRFLD,#MLR0,#NUM0
			call	   OldtoNewMlr1 using #MLR0,#COMP0
			if (#COMP0 = "")
				setflag over
				return
			endif
			pack	   NOFRFLD,#COMP0,#NUM0
.patch1.0

			trap      IOMssg giving Error if IO
         branch    NOFRLOCK to NOFR2L,NOFR2R,NOFR2N
NOFR2L   FILEPI    1;NOFRFILE
         READ      NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN
NOFR2R   READLK    NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN
NOFR2N   READ      NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN      
..............................................................................
..............................................................................
.
. ENTRY POINT : NOFRTST1
. REQUIRED    : NOFRFLD
. RETURNED    : 
. DESCRIPTION : TEST KEY
.
NOFRTST1  BRANCH    NOFRFLAG TO NOFR2A
         CALL      NOFROPEN
NOFR2A    
	trap      IOMssg giving Error if IO
         branch    NOFRLOCK to NOFR2La,NOFR2Ra,NOFR2Na
NOFR2La   FILEPI    1;NOFRFILE
         READ      NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN
NOFR2Ra   READLK    NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN
NOFR2Na   READ      NOFRFILE,NOFRFLD;;
         trapclr   IO
         RETURN      
..............................................................................
.
. ENTRY POINT : NOFRKS
. REQUIRED    : 
. RETURNED    : OFFER RECORD
. DESCRIPTION : KEY SEQUENTIAL OFFER FILE READ
.
NOFRKS   BRANCH    NOFRFLAG TO NOFR3
         CALL      NOFROPEN
NOFR3    ;patchrap      IOMssg giving Error if IO      branch    NOFRLOCK to NOFR3L,NOFR3R,NOFR3N
NOFR3L   FILEPI    1;NOFRFILE
         READKS    NOFRFILE;OFRVARS
         trapclr   IO
         RETURN
NOFR3R   READKSLK  NOFRFILE;OFRVARS
         trapclr   IO
         RETURN
NOFR3N   READKS    NOFRFILE;OFRVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NOFRSEQ
. REQUIRED    :
. RETURNED    : OFFER RECORD
. DESCRIPTION : SEQUENTIAL OFFER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NOFRSEQ  BRANCH    NOFRFLAG TO NOFR4
         CALL      NOFROPEN
NOFR4    trap      IOMssg giving Error if IO
         branch    NOFRLOCK to NOFR4L,NOFR4R,NOFR4N
NOFR4L   FILEPI    1;NOFRFILE
         READ      NOFRFILE,SEQ;OFRVARS
         trapclr   IO
         RETURN
NOFR4R   READLK    NOFRFILE,SEQ;OFRVARS
         trapclr   IO
         RETURN
NOFR4N   READ      NOFRFILE,SEQ;OFRVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NOFRWRT
. REQUIRED    : 'NOFRFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NOFRWRT  BRANCH    NOFRFLAG TO NOFR5
         GOTO      NOFR5 IF EQUAL
         CALL      NOFROPEN
NOFR5    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NOFRFILE
.         WRITE     NOFRFILE,NOFRFLD;OFRVARS
         FILEPI    1;NOFRFLIST
         WRITE     NOFRFLIST;OFRVARS
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN    
..............................................................................
.
. ENTRY POINT : NOFRUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE OFFER FILE
.
NOFRUPD  BRANCH    NOFRFLAG TO NOFR6
         CALL      NOFROPEN
NOFR6    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NOFRFILE
.         UPDATE    NOFRFILE;OFRVARS
         FILEPI    1;NOFRFLIST
         UPDATE    NOFRFLIST;OFRVARS
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NOFRDEL
. REQUIRED    : 'NOFRFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NOFRDEL  BRANCH    NOFRFLAG TO NOFR7
         CALL      NOFROPEN
NOFR7    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NOFRFILE
.         DELETE    NOFRFILE,NOFRFLD
         FILEPI    1;NOFRFLIST
         DELETE    NOFRFLIST
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN
.START PATCH 1.1 ADDED LOGIC
;.............................................................................
;
; ENTRY POINT : NOFRAIM   
; REQUIRED    : NOFRFld1
; RETURNED    : OFFER RECORD
; DESCRIPTION : AIM OFFER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NOFRAIM
	BRANCH	NOFRFLAG,NOFR9
	CALL	NOFROPEN
NOFR9
;patch1.0
	unpack	NOFRFLD1,#NUM0,#MLR0
	call	   OldtoNewMlr1 using #MLR0,#COMP0
	if (#COMP0 = "")
		setflag over
		return
	endif
	pack	   NOFRFLD1,#NUM0,#COMP0
;patch1.0
	TRAP	IOMssg Giving Error if IO
	BRANCH	NOFRLOCK,NOFR9L,NOFR9R,NOFR9N

NOFR9L
	FILEPI	1;NOFRFLE2
	READ	NOFRFLE2,NOFRFld1;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR9R 
	READLK	NOFRFLE2,NOFRFld1;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR9N
	READ	NOFRFLE2,NOFRFld1;OFRVARS
	TRAPCLR	IO
	RETURN
.;.............................................................................
.;
.; ENTRY POINT : NOFRKG
.; REQUIRED    : VALID PREVIOUS AIM READ
.; RETURNED    : OFFER RECORD
.; DESCRIPTION : AIM KEY GENEREIC OFFER FILE READ
.;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.;
NOFRKG
	BRANCH	NOFRFLAG,NOFR10
	CALL	NOFROPEN
NOFR10
	TRAP	IOMssg Giving Error if IO
	BRANCH	NOFRLOCK,NOFR10L,NOFR10R,NOFR10N
NOFR10L
	FILEPI	1;NOFRFLE2
	READKG	NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR10R 
	READKGLK NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR10N
	READKG	NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
.;.............................................................................
.;
.; ENTRY POINT : NOFRKGP
.; REQUIRED    : PREVIOUS VALID AIM READ
.; RETURNED    : OFFER RECORD
.; DESCRIPTION : KEY GENERIC PRIOR OFFER FILE READ
.;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.;
NOFRKGP
	BRANCH	NOFRFLAG,NOFR11
	CALL	NOFROPEN
NOFR11
	TRAP	IOMssg Giving Error if IO
	BRANCH	NOFRLOCK TO NOFR11L,NOFR11R,NOFR11N
NOFR11L
	FILEPI	1;NOFRFLE2
	READKGP	NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR11R
	READKGPLK NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
NOFR11N
	READKGP	NOFRFLE2;OFRVARS
	TRAPCLR	IO
	RETURN
.;.............................................................................

.END PATCH 1.1 ADDED LOGIC
...............................................................................
.
. ENTRY POINT : NOFROPEN
. REQUIRED    : 'NOFRFLAG'
. RETURNED    : 'NOFRFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OFFER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NOFROPEN TRAP      NOFRGONE IF IO
.START PATCH 1.1 REPLACED LOGIC
.         OPEN      NOFRFILE,NOFRNAME
         OPEN      NOFRFLIST
.END PATCH 1.1 REPLACED LOGIC
         TRAPCLR   IO
         MOVE      C1 TO NOFRFLAG
         RETURN
.
NOFRGONE MOVE      NOFRNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

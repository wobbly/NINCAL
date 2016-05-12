..............................................................................
.
. NPNDIO INCLUSION
. NIN ORDER PENDING FILE I/O ROUTINES
.
. CREATED 11DEC98
.
. FILE NAME : NINPND
. REC LENGTH: 48 FIXED
. INDEX KEY : 1-3 (1 BYTE STATUS TYPE + 2 BYTE STATUS CODE)
.
..............................................................................
.
. ENTRY POINT : NPNDKEY
. REQUIRED    : 'NPNDFLD'
. RETURNED    : ORDER PENDING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPNDKEY  BRANCH    NPNDFLAG TO NPND1
         CALL      NPNDOPEN
NPND1    trap      IOMSSG GIVING ERROR if IO
         BRANCH    NPNDLOCK TO NPND1L,NPND1R,NPND1N
.NPND1L - file locks
NPND1L
         FILEPI    1;NPNDFILE
         READ      NPNDFILE,NPNDFLD;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND1R - record locks
NPND1R
         READLK    NPNDFILE,NPNDFLD;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND1N - no locks
NPND1N
         READ      NPNDFILE,NPNDFLD;NPNDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPNDTST
. REQUIRED    : NPNDFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPNDTST  BRANCH    NPNDFLAG TO NPND2
         CALL      NPNDOPEN
NPND2    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;NPNDFILE
         READ      NPNDFILE,NPNDFLD;STR1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPNDKS
. REQUIRED    :
. RETURNED    : ORDER PENDING RECORD
. DESCRIPTION : KEY SEQUENTIAL ORDER PENDING FILE READ
.
NPNDKS   BRANCH    NPNDFLAG TO NPND3
         CALL      NPNDOPEN
NPND3    trap      IOMSSG GIVING ERROR if IO
         BRANCH    NPNDLOCK TO NPND3L,NPND3R,NPND3N

.NPND3L - file locks
NPND3L
         FILEPI    1;NPNDFILE
         READKS    NPNDFILE;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND3R - record locks
NPND3R
         READKSLK  NPNDFILE;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND3N - no locks
NPND3N
         READKS    NPNDFILE;NPNDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPNDSEQ
. REQUIRED    :
. RETURNED    : ORDER PENDING RECORD
. DESCRIPTION : SEQUENTIAL ORDER PENDING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPNDSEQ  BRANCH    NPNDPATH TO NPND4A,NPNDGONE,NPND4C
NPND4A   BRANCH    NPNDFLAG TO NPND4B
         CALL      NPNDOPEN

NPND4B   trap      IOMSSG GIVING ERROR if IO
         BRANCH    NPNDLOCK TO NPND4BL,NPND4BR,NPND4BN

.NPND4BL - file locks
NPND4BL
         FILEPI    1;NPNDFILE
         READ      NPNDFILE,SEQEOF2;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND4BR - record locks
NPND4BR
         READLK    NPNDFILE,SEQEOF2;NPNDVARS
         TRAPCLR   IO
         RETURN

.NPND4BN - no locks
NPND4BN
         READ      NPNDFILE,SEQEOF2;NPNDVARS
         TRAPCLR   IO
         RETURN

NPND4C   
.START SECTION REMMED UNTIL NPDNFLE3 IMPLEMENTED, IF EVER
.         BRANCH    NPNDFLG3 TO NPND4D
.         CALL      NPNDOPEN
.NPND4D   trap      IOMSSG GIVING ERROR if IO
.         BRANCH    NPNDLOCK TO NPND4DL,NPND4DR,NPND4DN
.
..NPND4DL - file locks
.NPND4DL
.         FILEPI    1;NPNDFLE3
.         READ      NPNDFLE3,SEQ;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND4DR - record locks
.NPND4DR
.         READLK    NPNDFLE3,SEQ;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND4DN - no locks
.NPND4DN
.         READ      NPNDFLE3,SEQ;NPNDVARS
.         TRAPCLR   IO
.END SECTION REMMED UNTIL NPDNFLE3 IMPLEMENTED, IF EVER
         RETURN
..............................................................................
.
. ENTRY POINT : NPNDWRT
. REQUIRED    : 'NPNDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPNDWRT  MOVE      C1 TO NPNDPATH
NPNDWRT1 COMPARE   NPNDPATH TO NPNDFLAG
         GOTO      NPND5 IF EQUAL
         CALL      NPNDOPEN
NPND5    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;NPNDFILE
         WRITE     NPNDFILE,NPNDFLD;NPNDVARS
         TRAPCLR   IO
.START SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
.         call      NPNDwrt2
.END SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
         RETURN
.......................
.START SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
.NPNDWRT2 BRANCH    NPNDFLG2 TO NPND5B
.         CALL      NPNDOPN2
.NPND5B   TRAP      IOMSSG GIVING ERROR IF IO
.         FILEPI    1;NPNDFLE2
.         INSERT    NPNDFLE2
.         TRAPCLR   IO
.         RETURN
.END SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
..............................................................................
.
. ENTRY POINT : NPNDUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER PENDING FILE
.
NPNDUPD  BRANCH    NPNDFLAG TO NPND6
         CALL      NPNDOPEN
NPND6    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;NPNDFILE
         UPDATE    NPNDFILE;NPNDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPNDDEL
. REQUIRED    : 'NPNDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPNDDEL  BRANCH    NPNDFLAG TO NPND7
         CALL      NPNDOPEN
NPND7    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;NPNDFILE
         DELETE    NPNDFILE,NPNDFLD
         TRAPCLR   IO
         RETURN

.START SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
...............................................................................
..
.. ENTRY POINT : NPNDNAIM
.. REQUIRED    : NPNDFLD2
.. RETURNED    : ORDER PENDING RECORD
.. DESCRIPTION : AIM ORDER PENDING FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
.NPNDAIM  BRANCH    NPNDFLG2 TO NPND9
.         CALL      NPNDOPN2
.NPND9    trap      IOMSSG GIVING ERROR if IO
.         BRANCH    NPNDLOCK TO NPND9L,NPND9R,NPND9N
.
..NPND9L - file locks
.NPND9L
.         FILEPI    1;NPNDFLE2
.         READ      NPNDFLE2,NPNDFLD2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND9R - record locks
.NPND9R
.         READLK    NPNDFLE2,NPNDFLD2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND9N - no locks
.NPND9N
.         READ      NPNDFLE2,NPNDFLD2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
...............................................................................
..
.. ENTRY POINT : NPNDKG
.. REQUIRED    : VALID PREVIOUS AIM READ
.. RETURNED    : ORDER PENDING RECORD
.. DESCRIPTION : AIM KEY GENEREIC ORDER PENDING FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
.NPNDKG   BRANCH    NPNDFLG2 TO NPND10
.         CALL      NPNDOPN2
.NPND10    trap      IOMSSG GIVING ERROR if IO
.         BRANCH    NPNDLOCK TO NPND10L,NPND10R,NPND10N
.
..NPND10L - file locks
.NPND10L
.         FILEPI    1;NPNDFLE2
.         READKG    NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND10R - record locks
.NPND10R
.         READKGLK  NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND10N - no locks
.NPND10N
.         READKG    NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
...............................................................................
..
.. ENTRY POINT : NPNDKGP
.. REQUIRED    : PREVIOUS VALID AIM READ
.. RETURNED    : ORDER PENDING RECORD
.. DESCRIPTION : KEY GENERIC PRIOR ORDER PENDING FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
.NPNDKGP  BRANCH    NPNDFLG2 TO NPND11
.         CALL      NPNDOPN2
.NPND11    trap      IOMSSG GIVING ERROR if IO
.         BRANCH    NPNDLOCK TO NPND11L,NPND11R,NPND11N
.
..NPND11L - file locks
.NPND11L
.         FILEPI    1;NPNDFLE2
.         READKGP   NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND11R - record locks
.NPND11R
.         READKGPLK NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.
..NPND11N - no locks
.NPND11N
.         READKGP   NPNDFLE2;NPNDVARS
.         TRAPCLR   IO
.         RETURN
.END SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
..............................................................................
.
. ENTRY POINT : NPNDOPEN
. REQUIRED    : 'NPNDFLAG'
. RETURNED    : 'NPNDFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN ORDER PENDING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPNDOPEN BRANCH    NPNDPATH OF NPNDOPN1,NPNDOPN2,NPNDOPN3
NPNDOPN1 TRAP      NPNDGONE IF IO
         BRANCH    NPNDLOCK TO NPNDOP,NPNDOPR,NPNDOP
.
NPNDOP
         OPEN      NPNDFILE,NPNDNAME
	 READ      NPNDFILE,SEQEOF;;
         TRAPCLR   IO
         MOVE      C1 TO NPNDFLAG
         RETURN
NPNDOPR
         OPEN      NPNDFILE,NPNDNAME,LOCKMANUAL,SINGLE
	 READ      NPNDFILE,SEQEOF;;
         TRAPCLR   IO
         MOVE      C1 TO NPNDFLAG
         RETURN
.
NPNDOPN2 
.START SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
.         TRAP      NPNDGONE IF IO
.         BRANCH    NPNDLOCK TO NPNDOP2,NPNDOPR2,NPNDOP2
..
.NPNDOP2
.         OPEN      NPNDFLE2,NPNDNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NPNDFLG2
.         RETURN
.NPNDOPR2
.         OPEN      NPNDFLE2,NPNDNAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NPNDFLG2
.END SECTION REMMED UNTIL NPDNFLE2 IMPLEMENTED, IF EVER
         RETURN
.
NPNDOPN3
.START SECTION REMMED UNTIL NPDNFLE3 IMPLEMENTED, IF EVER
.         TRAP      NPNDGONE IF IO
.         BRANCH    NPNDLOCK TO NPNDOP3,NPNDOPR3,NPNDOP3
..
.NPNDOP3
.         OPEN      NPNDFLE3,NPNDNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NPNDFLG3
.         RETURN
.NPNDOPR3
.         OPEN      NPNDFLE3,NPNDNAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NPNDFLG3
.END SECTION REMMED UNTIL NPDNFLE3 IMPLEMENTED, IF EVER
         RETURN
.
NPNDGONE MOVE      NPNDNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
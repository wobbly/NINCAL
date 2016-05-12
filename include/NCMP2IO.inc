.........................................................
.
. NCMP2IO INCLUSION
. NIN CAMPAIGN FILE I/O ROUTINES
.
. FILE NAME :   NINCMP2
. REC LENGTH:   33 FIXED
. INDEX KEY :   (1)     001-006 (Campaign Name)
.               (2)     007-024 (Integral Scenario ID)
.
.
.........................................................
. ENTRY POINT : NCMP2KEY
. REQUIRED    : 'NCMP2PATH', 'NCMP2FLD', or 'NCMP2FLD1'
. RETURNED    : CAMPAIGN CROSS REFERENCE VARS
. DESCRIPTION : EXACT ISAM KEY READ
.
NCMP2KEY
	branch	NCMP2PATH to NCMP21a,NCMP21C
NCMP21a
	branch	NCMP2flag to NCMP21b
	CALL	NCMP2OPEN
NCMP21B
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP21bL,NCMP21bR,NCMP21bN
NCMP21bL
	FILEPI	1;NCMP2FILE
	READ	NCMP2FILE,NCMP2FLD;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP21bR  
	READLK	NCMP2FILE,NCMP2FLD;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP21bN
	READ	NCMP2FILE,NCMP2FLD;NCMP2VARS
	TRAPCLR	IO
	RETURN

NCMP21C
	branch	NCMP2flag to NCMP21D
	CALL	NCMP2OPEN
NCMP21D
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP21DL,NCMP21DR,NCMP21DN
NCMP21DL
	FILEPI	1;NCMP2FLE1
	READ	NCMP2FLE1,NCMP2FLD1;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP21DR  
	READLK	NCMP2FLE1,NCMP2FLD1;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP21DN
	READ	NCMP2FLE1,NCMP2FLD1;NCMP2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2TST
. REQUIRED    : 'NCMP2PATH', 'NCMP2FLD', or 'NCMP2FLD1'
. RETURNED    : CAMPAIGN CROSS REFERENCE VARS
. DESCRIPTION : TEST KEY
.
NCMP2TST
	branch	NCMP2PATH to NCMP22a,NCMP22C
NCMP22a
	branch	NCMP2flag to NCMP22b
	CALL	NCMP2OPEN
NCMP22B
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP22bL,NCMP22bR,NCMP22bN
NCMP22bL
	FILEPI	1;NCMP2FILE
	READ	NCMP2FILE,NCMP2FLD;;
	TRAPCLR	IO
	RETURN
NCMP22bR  
	READLK	NCMP2FILE,NCMP2FLD;;
	TRAPCLR	IO
	RETURN
NCMP22bN
	READ	NCMP2FILE,NCMP2FLD;;
	TRAPCLR	IO
	RETURN

NCMP22C
	branch	NCMP2flag to NCMP21D
	CALL	NCMP2OPEN
NCMP22D
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP22DL,NCMP22DR,NCMP22DN
NCMP22DL
	FILEPI	1;NCMP2FLE1
	READ	NCMP2FLE1,NCMP2FLD1;;
	TRAPCLR	IO
	RETURN
NCMP22DR  
	READLK	NCMP2FLE1,NCMP2FLD1;;
	TRAPCLR	IO
	RETURN
NCMP22DN
	READ	NCMP2FLE1,NCMP2FLD1;;
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2KS
. REQUIRED    : 'NCMP2PATH'
. RETURNED    : CAMPAIGN VARS
. DESCRIPTION : KEY SEQUENTIAL CAMPAIGN CROSS REFERENCE FILE READ
.
NCMP2KS
	branch	NCMP2PATH to NCMP23a,NCMP23C
NCMP23a
	branch	NCMP2flag to NCMP23b
	CALL	NCMP2OPEN
NCMP23B
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP23bL,NCMP23bR,NCMP23bN
NCMP23bL
	FILEPI	1;NCMP2FILE
	READKS	NCMP2FILE;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP23bR  
	READKSLK NCMP2FILE;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP23bN
	READKS	NCMP2FILE;NCMP2VARS
	TRAPCLR	IO
	RETURN

NCMP23C
	branch	NCMP2flag to NCMP23D
	CALL	NCMP2OPEN
NCMP23D
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP23DL,NCMP23DR,NCMP23DN
NCMP23DL
	FILEPI	1;NCMP2FLE1
	READKS	NCMP2FLE1;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP23DR  
	READKSLK NCMP2FLE1;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP23DN
	READKS	NCMP2FLE1;NCMP2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2SEQ
. REQUIRED    :
. RETURNED    : CAMPAIGN CROSS REFERENCE VARS
. DESCRIPTION : SEQUENTIAL CAMPAIGN CROSS REFERENCE FILE READ
.
NCMP2SEQ
	BRANCH	NCMP2FLAG TO NCMP24
	CALL	NCMP2OPEN
NCMP24
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP24L,NCMP24R,NCMP24N
NCMP24L
	FILEPI	1;NCMP2FILE
	READ	NCMP2FILE,SEQ;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP24R
	READLK	NCMP2FILE,SEQ;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP24N
	READ	NCMP2FILE,SEQ;NCMP2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2WRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT.
.
NCMP2WRT
	BRANCH	NCMP2FLAG,NCMP25
	CALL	NCMP2OPEN
NCMP25
	trap	IOMssg giving Error if IO
	FILEPI	1;NCMP2FLIST
	WRITE	NCMP2FLIST;NCMP2VARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2UPD
. REQUIRED    : A PREVIOUS KEY READ,
. RETURNED    :
. DESCRIPTION : KEY UPDATE CAMPAIGN CROSS REFERENCE FILE
.
NCMP2UPD
	BRANCH	NCMP2FLAG,NCMP26
	CALL	NCMP2OPEN
NCMP26
	trap	IOMssg giving Error if IO
	FILEPI	1;NCMP2FLIST
	UPDATE	NCMP2FLIST;NCMP2VARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2DEL                                             
. REQUIRED    : 'NCMP2PATH', 'NCMP2FLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ALL KEYS. PATH=2/3/4 DELETES KEY ONLY.
.
NCMP2DEL
	BRANCH	NCMP2FLAG,NCMP27
	CALL	NCMP2OPEN
NCMP27
	trap	IOMssg giving Error if IO
	FILEPI	1;NCMP2FLIST
	DELETE	NCMP2FLIST
	trapclr	IO
	RETURN
.
..............................................................................
.
. ENTRY POINT : NCMP2KP
. REQUIRED    : 'NCMP2PATH'
. RETURNED    : CAMPAIGN RENCMP2
. DESCRIPTION : KEY SEQUENTIAL CAMPAIGN FILE READ
.
NCMP2KP   COMPARE   NCMP2PATH TO NCMP2FLAG
         GOTO      NCMP28 IF EQUAL
         CALL      NCMP2OPEN
NCMP28    TRAP      IOMssg Giving Error if IO
         Branch    NCMP2lock to NCMP28L,NCMP28R,NCMP28N
NCMP28L   FILEPI    1;NCMP2FILE
         READKP    NCMP2FILE;NCMP2VARS
         TRAPCLR   IO
         RETURN
NCMP28R
         READKPLK    NCMP2FILE;NCMP2VARS
         TRAPCLR     IO
         RETURN
NCMP28N
         READKP    NCMP2FILE;NCMP2VARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCMP2AIM
. REQUIRED    : NCMP2FLD1,NCMP2FLD2
. RETURNED    : CAMPAIGN CROSS REFERENCE RECORD
. DESCRIPTION : AIM CAMPAIGN CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCMP2AIM
	BRANCH	NCMP2FLAG TO NCMP29
	CALL	NCMP2OPEN
NCMP29
	TRAP	IOMssg Giving Error if IO
	Branch	NCMP2lock to NCMP29L,NCMP29R,NCMP29N
NCMP29L
	FILEPI	1;NCMP2FLE2
	READ	NCMP2FLE2,NCMP2FLD2,NCMP2FLD3,NCMP2FLD4;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP29R
	READLK	NCMP2FLE2,NCMP2FLD2,NCMP2FLD3,NCMP2FLD4;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP29N
	READ	NCMP2FLE2,NCMP2FLD2,NCMP2FLD3,NCMP2FLD4;NCMP2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : CAMPAIGN CROSS REFERENCE RECORD
. DESCRIPTION : AIM KEY GENERIC CAMPAIGN CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCMP2KG
	BRANCH	NCMP2FLAG TO NCMP210
	CALL	NCMP2OPEN
NCMP210
	TRAP	IOMssg Giving Error if IO
	Branch	NCMP2lock to NCMP210L,NCMP210R,NCMP210N
NCMP210L
	FILEPI	1;NCMP2FLE2
	READKG	NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP210R
	READKGLK NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP210N
	READKG	NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NCMP2KGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : CAMPAIGN CROSS REFERENCE RECORD
. DESCRIPTION : Aim KEY GENERIC PRIOR CAMPAIGN CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCMP2KGP
	BRANCH	NCMP2FLAG TO NCMP211
	CALL	NCMP2OPEN
NCMP211
	TRAP	IOMssg Giving Error if IO
	branch	NCMP2lock to NCMP211L,NCMP211R,NCMP211N
NCMP211L
	FILEPI	1;NCMP2FLE2
	READKGP	NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP211R
	READKGPLK NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
NCMP211N
	READKGP	NCMP2FLE2;NCMP2VARS
	TRAPCLR	IO
	RETURN
...............................................................................
...............................................................................
.
. ENTRY POINT : NCMP2OPEN
. REQUIRED    : 'NCMP2PATH'
. RETURNED    : 'NCMP2FLAG' SET TO THE PATH NUMBER: 'NCMP2PATH', IF OPENNED.
. DESCRIPTION : OPEN NIN CAMPAIGN CROSS REFERENCE FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCMP2OPEN
	TRAP	NCMP2GONE giving Error if IO
.	OPEN	NCMP2FLIST,EXCLUSIVE
	OPEN	NCMP2FLIST
	trapclr	IO
	MOVE	C1,NCMP2FLAG
	RETURN
.
NCMP2GONE
	MOVE	NCMP2NAME,FILENAME
	CALL	FILEGONE
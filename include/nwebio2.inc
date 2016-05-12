..............................................................................
.
. NWEB2IO INCLUSION
. NIN WEBSITE PROCESS DEFINITION FILE I/O ROUTINES
.
. FILE NAME : NINWEB2
. REC LENGTH: 160
. INDEX KEY:      1-4	NWEB22CODE
. AIMDEX KEY:
.
..............................................................................
.
. ENTRY POINT : NWEB2KEY
. REQUIRED    : 'NWEB2FLD'
. RETURNED    : WEBSITE PROCESS DEFINITION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
...................................................................................

NWEB2KEY
        BRANCH  NWEB2FLAG,NWEB21
        CALL    NWEB2OPEN
NWEB21
        trap    IOMssg giving Error if IO
        READ    NWEB2FILE,NWEB2FLD;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2TST
. REQUIRED    : NWEB2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NWEB2TST
        BRANCH  NWEB2FLAG,NWEB22
        CALL    NWEB2OPEN
NWEB22
        trap    IOMssg giving Error if IO
        READ    NWEB2FILE,NWEB2FLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2KS
. REQUIRED    :
. RETURNED    : WEBSITE PROCESS DEFINITION RECORD
. DESCRIPTION : KEY SEQUENTIAL WEBSITE USER TRACKING FILE READ
.
NWEB2KS
        BRANCH  NWEB2FLAG,NWEB23
        CALL    NWEB2OPEN
NWEB23
        trap    IOMssg giving Error if IO
        READKS  NWEB2FILE;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2SEQ
. REQUIRED    :
. RETURNED    : WEBSITE PROCESS DEFINITION RECORD
. DESCRIPTION : SEQUENTIAL PROCESS DEFINITION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEB2SEQ
        BRANCH  NWEB2FLAG,NWEB24
        CALL    NWEB2OPEN
NWEB24
        trap    IOMssg giving Error if IO
        READ    NWEB2FILE,SEQ;NWEB2RECORD
        trapclr IO
        RETURN

...................................................................................
. ENTRY POINT : NWEB2EOF
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : reposition fp to eof
.
.
...................................................................................
NWEB2EOF
	BRANCH  NWEB2FLAG,NWEB31
	CALL    NWEB2OPEN
NWEB31
	trap    IOMssg giving Error if IO
	READ    NWEB2FILE,FPEOF;NWEB2RECORD
	trapclr IO
	 RETURN

...................................................................................
. ENTRY POINT : NWEB2BAK
. REQUIRED    :
. RETURNED    : WEBSITE PROCESS DEFINITION RECORD
. DESCRIPTION : read sequentially backwards through file
.
.
...................................................................................
NWEB2BAK
        BRANCH  NWEB2FLAG,NWEB30
        CALL    NWEB2OPEN
NWEB30
        trap    IOMssg giving Error if IO
        READ    NWEB2FILE,XSEQ;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2WRT
. REQUIRED    : 'NWEB2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NWEB2WRT
        BRANCH  NWEB2FLAG,NWEB25
        CALL    NWEB2OPEN
NWEB25
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEB2FLIST
        WRITE   NWEB2FLIST;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NWEB2UPD
        BRANCH  NWEB2FLAG,NWEB26
        CALL    NWEB2OPEN
NWEB26
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEB2FLIST
        UPDATE  NWEB2FLIST;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2DEL
. REQUIRED    : 'NWEB2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NWEB2DEL
        BRANCH  NWEB2FLAG,NWEB27
        CALL    NWEB2OPEN
NWEB27
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEB2FLIST
        DELETE  NWEB2FLIST
        trapclr IO
        RETURN

...............................................................................
. ENTRY POINT : NWEB2AIM
. REQUIRED    : NWEB2FLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEB2AIM
        BRANCH  NWEB2FLAG,NWEB28
        CALL    NWEB2OPEN
NWEB28
        trap    IOMssg giving Error if IO
        READ    NWEB2FLE2,NWEB2FLD1,NWEB2FLD2;NWEB2RECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEB2KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEB2KG
        BRANCH  NWEB2FLAG,NWEB210
        CALL    NWEB2OPEN
NWEB210
        trap    IOMssg giving Error if IO
        READKG  NWEB2FLE2;NWEB2RECORD
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NWEB2OPEN
. REQUIRED    : 'NWEB2FLAG'
. RETURNED    : 'NWEB2FLAG' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN WEBSITE PROCESS DEFINITION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NWEB2OPEN
        TRAP    NWEB2GONE giving Error if IO
        OPEN    NWEB2FLIST       .,EXCLUSIVE
        trapclr IO
        MOVE    C1,NWEB2FLAG
        RETURN
.
NWEB2GONE
        MOVE    NWEB2NAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
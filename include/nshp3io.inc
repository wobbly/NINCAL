..............................................................................
.
. NSHP3IO INCLUSION
. NIN WEBSITE USER TRACKING FILE I/O ROUTINES
.
. FILE NAME : NINSHP3
. REC LENGTH: 336
. INDEX KEY:    001-006	NSHP3LR   	
. AIMDEX KEY:   007-015	NSHP3RQTY 	
.		016-023	NSHP3Date 	
.		024-032	NSHP3DQTY 	
.		032-286	NSHP3Comments	
.		287-336	NSHP3FILLER	

.
..............................................................................
.
. ENTRY POINT : NSHP3KEY
. REQUIRED    : 'NSHP3FLD'
. RETURNED    : WEBSITE Inventory/LOL RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSHP3KEY
        BRANCH  NSHP3FLAG,NSHP31
        CALL    NSHP3OPEN
NSHP31
        trap    IOMssg giving Error if IO
        READ    NSHP3FILE,NSHP3FLD;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3TST
. REQUIRED    : NSHP3FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSHP3TST
        BRANCH  NSHP3FLAG,NSHP32
        CALL    NSHP3OPEN
NSHP32
        trap    IOMssg giving Error if IO
        READ    NSHP3FILE,NSHP3FLD;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3KS
. REQUIRED    :
. RETURNED    : WEBSITE USER TRACKING RECORD
. DESCRIPTION : KEY SEQUENTIAL Inventory/LOL FILE READ
.
NSHP3KS
        BRANCH  NSHP3FLAG,NSHP33
        CALL    NSHP3OPEN
NSHP33
        trap    IOMssg giving Error if IO
        READKS  NSHP3FILE;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3SEQ
. REQUIRED    :
. RETURNED    : WEBSITE USER TRACKING RECORD
. DESCRIPTION : SEQUENTIAL Inventory/LOL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSHP3SEQ
        BRANCH  NSHP3FLAG,NSHP34
        CALL    NSHP3OPEN
NSHP34
        trap    IOMssg giving Error if IO
        READ    NSHP3FILE,SEQ;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3WRT
. REQUIRED    : 'NSHP3FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSHP3WRT
        BRANCH  NSHP3FLAG,NSHP35
        CALL    NSHP3OPEN
NSHP35
        trap    IOMssg giving Error if IO
        FILEPI  1;NSHP3FLIST
        WRITE   NSHP3FLIST;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NSHP3UPD
        BRANCH  NSHP3FLAG,NSHP36
        CALL    NSHP3OPEN
NSHP36
        trap    IOMssg giving Error if IO
        FILEPI  1;NSHP3FLIST
        UPDATE  NSHP3FLIST;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3DEL
. REQUIRED    : 'NSHP3FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSHP3DEL
        BRANCH  NSHP3FLAG,NSHP37
        CALL    NSHP3OPEN
NSHP37
        trap    IOMssg giving Error if IO
        FILEPI  1;NSHP3FLIST
        DELETE  NSHP3FLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NSHP3AIM
. REQUIRED    : NSHP3FLD1,NSHP3FLD2,NSHP3FLD3,NSHP3FLD4,NSHP3FLD5,NSHP3FLD6,NSHP3FLD7
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSHP3AIM
        BRANCH  NSHP3FLAG,NSHP38
        CALL    NSHP3OPEN
NSHP38
        trap    IOMssg giving Error if IO
.        READ    NSHP3FLE2,NSHP3FLD1,NSHP3FLD2,NSHP3FLD3,NSHP3FLD4,NSHP3FLD5,NSHP3FLD6,NSHP3FLD7;NSHP3VARS
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NSHP3AIMT
. REQUIRED    : NSHP3FLD1,NSHP3FLD2,NSHP3FLD3,NSHP3FLD4,NSHP3FLD5,NSHP3FLD6,NSHP3FLD7
. RETURNED    : OVER FLAG
. DESCRIPTION : AIM FILE TEST READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSHP3AIMT
        BRANCH  NSHP3FLAG,NSHP39
        CALL    NSHP3OPEN
NSHP39
        trap    IOMssg giving Error if IO
.        READ    NSHP3FLE2,NSHP3FLD1,NSHP3FLD2,NSHP3FLD3,NSHP3FLD4,NSHP3FLD5,NSHP3FLD6,NSHP3FLD7;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NSHP3KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSHP3KG
        BRANCH  NSHP3FLAG,NSHP310
        CALL    NSHP3OPEN
NSHP310
        trap    IOMssg giving Error if IO
.        READKG  NSHP3FLE2;NSHP3VARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NSHP3OPEN
. REQUIRED    : 'NSHP3FLAG'
. RETURNED    : 'NSHP3FLAG' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN Inventory/LOL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSHP3OPEN
        TRAP    NSHP3GONE giving Error if IO
        OPEN    NSHP3FLIST       .,EXCLUSIVE
.       OPEN    NSHP3FLIST
        trapclr IO
        MOVE    C1,NSHP3FLAG
        RETURN
.
NSHP3GONE
        MOVE    NSHP3NAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
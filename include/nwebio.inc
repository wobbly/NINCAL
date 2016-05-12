..............................................................................
.
. NWEBIO INCLUSION
. NIN WEBSITE USER TRACKING FILE I/O ROUTINES
.
. FILE NAME : NINWEB
. REC LENGTH: 150
. INDEX KEY:      1-52	NWEBID + NWEBTIME
. AIMDEX KEY:     1-36	NWEBID
.		 53-64	NWEBUSERNAME
.		 65-65	NWEBUSERTYPE
.		 66-71	NWEBCOMP
.		 72-75	NWEBPROCESS
.		 76-135	NWEBUSEREMAIL
.
..............................................................................
.
. ENTRY POINT : NWEBKEY
. REQUIRED    : 'NWEBFLD'
. RETURNED    : WEBSITE USER TRACKING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEBKEY
        BRANCH  NWEBFLAG,NWEB1
        CALL    NWEBOPEN
NWEB1
        trap    IOMssg giving Error if IO
        READ    NWEBFILE,NWEBFLD;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBTST
. REQUIRED    : NWEBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NWEBTST
        BRANCH  NWEBFLAG,NWEB2
        CALL    NWEBOPEN
NWEB2
        trap    IOMssg giving Error if IO
        READ    NWEBFILE,NWEBFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBKS
. REQUIRED    :
. RETURNED    : WEBSITE USER TRACKING RECORD
. DESCRIPTION : KEY SEQUENTIAL WEBSITE USER TRACKING FILE READ
.
NWEBKS
        BRANCH  NWEBFLAG,NWEB3
        CALL    NWEBOPEN
NWEB3
        trap    IOMssg giving Error if IO
        READKS  NWEBFILE;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBSEQ
. REQUIRED    :
. RETURNED    : WEBSITE USER TRACKING RECORD
. DESCRIPTION : SEQUENTIAL WEBSITE USER TRACKING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEBSEQ
        BRANCH  NWEBFLAG,NWEB4
        CALL    NWEBOPEN
NWEB4
        trap    IOMssg giving Error if IO
        READ    NWEBFILE,SEQ;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBWRT
. REQUIRED    : 'NWEBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NWEBWRT
        BRANCH  NWEBFLAG,NWEB5
        CALL    NWEBOPEN
NWEB5
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEBFLIST
        WRITE   NWEBFLIST;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NWEBUPD
        BRANCH  NWEBFLAG,NWEB6
        CALL    NWEBOPEN
NWEB6
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEBFLIST
        UPDATE  NWEBFLIST;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBDEL
. REQUIRED    : 'NWEBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NWEBDEL
        BRANCH  NWEBFLAG,NWEB7
        CALL    NWEBOPEN
NWEB7
        trap    IOMssg giving Error if IO
        FILEPI  1;NWEBFLIST
        DELETE  NWEBFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NWEBAIM
. REQUIRED    : NWEBFLD1,NWEBFLD2,NWEBFLD3,NWEBFLD4,NWEBFLD5,NWEBFLD6,NWEBFLD7
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEBAIM
        BRANCH  NWEBFLAG,NWEB8
        CALL    NWEBOPEN
NWEB8
        trap    IOMssg giving Error if IO
        READ    NWEBFLE2,NWEBFLD1,NWEBFLD2,NWEBFLD3,NWEBFLD4,NWEBFLD5,NWEBFLD6,NWEBFLD7;NWEBRECORD
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NWEBAIMT
. REQUIRED    : NWEBFLD1,NWEBFLD2,NWEBFLD3,NWEBFLD4,NWEBFLD5,NWEBFLD6,NWEBFLD7
. RETURNED    : OVER FLAG
. DESCRIPTION : AIM FILE TEST READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEBAIMT
        BRANCH  NWEBFLAG,NWEB9
        CALL    NWEBOPEN
NWEB9
        trap    IOMssg giving Error if IO
        READ    NWEBFLE2,NWEBFLD1,NWEBFLD2,NWEBFLD3,NWEBFLD4,NWEBFLD5,NWEBFLD6,NWEBFLD7;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NWEBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWEBKG
        BRANCH  NWEBFLAG,NWEB10
        CALL    NWEBOPEN
NWEB10
        trap    IOMssg giving Error if IO
        READKG  NWEBFLE2;NWEBRECORD
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NWEBOPEN
. REQUIRED    : 'NWEBFLAG'
. RETURNED    : 'NWEBFLAG' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN WEBSITE USER TRACKING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NWEBOPEN
        TRAP    NWEBGONE giving Error if IO
        OPEN    NWEBFLIST       .,EXCLUSIVE
.       OPEN    NWEBFLIST
        trapclr IO
        MOVE    C1,NWEBFLAG
        RETURN
.
NWEBGONE
        MOVE    NWEBNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
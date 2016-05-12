..............................................................................
.
. NINTIO INCLUSION
. INTEGRAL CLIENT RELATIONSHIP FILE I/O ROUTINES
.
. FILE NAME : NININT
. REC LENGTH: 6
. INDEX KEY : 1-6 (6 BYTE CLIENT NUMBER)
.
..............................................................................
.
. ENTRY POINT : NINTKEY
. REQUIRED    : 'NINTFLD'
. RETURNED    : INTEGRAL CLIENT RELATIONSHIP RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NINTKEY
        BRANCH  NINTFLAG,NINT1
        CALL    NINTOPEN
NINT1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NINTFILE
        READ    NINTFILE,NINTFLD;NINTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTTST
. REQUIRED    : NINTFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NINTTST
        BRANCH  NINTFLAG,NINT2
        CALL    NINTOPEN
NINT2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NINTFILE
        READ    NINTFILE,NINTFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTKS
. REQUIRED    :
. RETURNED    : INTEGRAL CLIENT RELATIONSHIP RECORD
. DESCRIPTION : KEY SEQUENTIAL INTEGRAL CLIENT RELATIONSHIP FILE READ
.
NINTKS
        BRANCH  NINTFLAG,NINT3
        CALL    NINTOPEN
NINT3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NINTFILE
        READKS  NINTFILE;NINTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTSEQ
. REQUIRED    :
. RETURNED    : INTEGRAL CLIENT RELATIONSHIP RECORD
. DESCRIPTION : SEQUENTIAL INTEGRAL CLIENT RELATIONSHIP FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NINTSEQ
        BRANCH  NINTFLAG,NINT4
        CALL    NINTOPEN
NINT4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NINTFILE
        READ    NINTFILE,SEQ;NINTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTWRT
. REQUIRED    : 'NINTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NINTWRT
        BRANCH  NINTFLAG,NINT5
        CALL    NINTOPEN
NINT5
        trap    IOMssg giving Error if IO
        FILEPI  1;NINTFLIST
        WRITE   NINTFLIST;NINTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NINTUPD
        BRANCH  NINTFLAG,NINT6
        CALL    NINTOPEN
NINT6
        trap    IOMssg giving Error if IO
        FILEPI  1;NINTFLIST
        UPDATE  NINTFLIST;NINTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NINTDEL
. REQUIRED    : 'NINTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NINTDEL
        BRANCH  NINTFLAG,NINT7
        CALL    NINTOPEN
NINT7
        trap    IOMssg giving Error if IO
        FILEPI  1;NINTFLIST
        DELETE  NINTFLIST
        trapclr IO
        RETURN

...............................................................................
.. ENTRY POINT : NINTAIM
.. REQUIRED    : NINTFLD2
.. RETURNED    : RECORD
.. DESCRIPTION : AIM FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
.NINTAIM
.        BRANCH  NINTFLAG,NINT8
.        CALL    NINTOPEN
.NINT8
.        trap    IOMssg giving Error if IO
.        READ    NINTFLE2,NINTFLD1;NINTVARS
.        trapclr IO
.        RETURN
.
...............................................................................
..
.. ENTRY POINT : NINTKG
.. REQUIRED    : VALID PREVIOUS AIM READ
.. RETURNED    : RECORD
.. DESCRIPTION : AIM KEY GENEREIC FILE READ
..               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
..
.NINTKG
.        BRANCH  NINTFLAG,NINT10
.        CALL    NINTOPEN
.NINT10
.        trap    IOMssg giving Error if IO
.        READKG  NINTFLE2;NINTVARS
.        trapclr IO
.        RETURN
.
...............................................................................
.
. ENTRY POINT : NINTOPEN
. REQUIRED    : 'NINTFLAG'
. RETURNED    : 'NINTFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN INTEGRAL CLIENT RELATIONSHIP FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NINTOPEN
        TRAP    NINTGONE giving Error if IO
        OPEN    NINTFLIST       .,EXCLUSIVE
.       OPEN    NINTFLIST
        trapclr IO
        MOVE    C1,NINTFLAG
        RETURN
.
NINTGONE
        MOVE    NINTNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
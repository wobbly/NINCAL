..............................................................................
.
. NCONTIO INCLUSION
. NIN DATACARD ADDRESSING FILE I/O ROUTINES
.
. FILE NAME : NINCONT
. REC LENGTH: 20 VAR
. INDEX KEY : 1-20 (9 BYTE MASTER LR + 9 BYTE CONTINUATION LR + 2 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NCONTKEY
. REQUIRED    : 'NCONTFLD'
. RETURNED    : LR CONTINUATION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCONTKEY
        BRANCH  NCONTFLAG,NCONT1
        CALL    NCONTOPEN
NCONT1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NCONTFILE
        READ    NCONTFILE,NCONTFLD;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTTST
. REQUIRED    : NCONTFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCONTTST
        BRANCH  NCONTFLAG,NCONT2
        CALL    NCONTOPEN
NCONT2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NCONTFILE
        READ    NCONTFILE,NCONTFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTKS
. REQUIRED    :
. RETURNED    : LR CONTINUATION RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ
.
NCONTKS
        BRANCH  NCONTFLAG,NCONT3
        CALL    NCONTOPEN
NCONT3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NCONTFILE
        READKS  NCONTFILE;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTSEQ
. REQUIRED    :
. RETURNED    : LR CONTINUATION RECORD
. DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCONTSEQ
        BRANCH  NCONTFLAG,NCONT4
        CALL    NCONTOPEN
NCONT4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NCONTFILE
        READ    NCONTFILE,SEQ;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTWRT
. REQUIRED    : 'NCONTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCONTWRT
        BRANCH  NCONTFLAG,NCONT5
        CALL    NCONTOPEN
NCONT5
        trap    IOMssg giving Error if IO
        FILEPI  1;NCONTFLIST
        WRITE   NCONTFLIST;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NCONTUPD
        BRANCH  NCONTFLAG,NCONT6
        CALL    NCONTOPEN
NCONT6
        trap    IOMssg giving Error if IO
        FILEPI  1;NCONTFLIST
        UPDATE  NCONTFLIST;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTDEL
. REQUIRED    : 'NCONTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCONTDEL
        BRANCH  NCONTFLAG,NCONT7
        CALL    NCONTOPEN
NCONT7
        trap    IOMssg giving Error if IO
        FILEPI  1;NCONTFLIST
        DELETE  NCONTFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NCONTAIM
. REQUIRED    : NCONTFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCONTAIM
        BRANCH  NCONTFLAG,NCONT8
        CALL    NCONTOPEN
NCONT8
        trap    IOMssg giving Error if IO
        READ    NCONTFLE2,NCONTFLD1,NCONTFLD2;NCONTVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NCONTKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCONTKG
        BRANCH  NCONTFLAG,NCONT10
        CALL    NCONTOPEN
NCONT10
        trap    IOMssg giving Error if IO
        READKG  NCONTFLE2;NCONTVARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NCONTOPEN
. REQUIRED    : 'NCONTFLAG'
. RETURNED    : 'NCONTFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN LR CONTINUATION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCONTOPEN
        TRAP    NCONTGONE giving Error if IO
        OPEN    NCONTFLIST       .,EXCLUSIVE
.       OPEN    NCONTFLIST
        trapclr IO
        MOVE    C1,NCONTFLAG
        RETURN
.
NCONTGONE
        MOVE    NCONTNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
..............................................................................
.
. NPRGIO INCLUSION
. NIN INFORMATION SERVICES IN-HOUSE PROGRAM FILE I/O ROUTINES
.
. FILE NAME : NINPRG
. REC LENGTH: 5 VAR
. INDEX KEY : 1-5 NPRGNUM
.
..............................................................................
.
. ENTRY POINT : NPRGKEY
. REQUIRED    : 'NPRGFLD'
. RETURNED    : INFORMATION SERVICES IN-HOUSE PROGRAM RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRGKEY
        BRANCH  NPRGFLAG,NPRG1
        CALL    NPRGOPEN
NPRG1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NPRGFILE
        READ    NPRGFILE,NPRGFLD;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGTST
. REQUIRED    : NPRGFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPRGTST
        BRANCH  NPRGFLAG,NPRG2
        CALL    NPRGOPEN
NPRG2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NPRGFILE
        READ    NPRGFILE,NPRGFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGKS
. REQUIRED    :
. RETURNED    : INFORMATION SERVICES IN-HOUSE PROGRAM RECORD
. DESCRIPTION : KEY SEQUENTIAL INFORMATION SERVICES IN-HOUSE PROGRAM FILE READ
.
NPRGKS
        BRANCH  NPRGFLAG,NPRG3
        CALL    NPRGOPEN
NPRG3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NPRGFILE
        READKS  NPRGFILE;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGSEQ
. REQUIRED    :
. RETURNED    : INFORMATION SERVICES IN-HOUSE PROGRAM RECORD
. DESCRIPTION : SEQUENTIAL INFORMATION SERVICES IN-HOUSE PROGRAM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRGSEQ
        BRANCH  NPRGFLAG,NPRG4
        CALL    NPRGOPEN
NPRG4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NPRGFILE
        READ    NPRGFILE,SEQ;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGSEQ2
. REQUIRED    :
. RETURNED    : INFORMATION SERVICES IN-HOUSE PROGRAM RECORD
. DESCRIPTION : BACKWARD SEQUENTIAL INFORMATION SERVICES IN-HOUSE PROGRAM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRGSEQ2
        BRANCH  NPRGFLAG,NPRG42
        CALL    NPRGOPEN
NPRG42
        trap    IOMssg giving Error if IO
.       FILEPI  1;NPRGFILE
        READ    NPRGFILE,NPRGSEQ2;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGWRT
. REQUIRED    : 'NPRGFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPRGWRT
        BRANCH  NPRGFLAG,NPRG5
        CALL    NPRGOPEN
NPRG5
        trap    IOMssg giving Error if IO
        FILEPI  1;NPRGFLIST
        WRITE   NPRGFLIST;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NPRGUPD
        BRANCH  NPRGFLAG,NPRG6
        CALL    NPRGOPEN
NPRG6
        trap    IOMssg giving Error if IO
        FILEPI  1;NPRGFLIST
        UPDATE  NPRGFLIST;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGDEL
. REQUIRED    : 'NPRGFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPRGDEL
        BRANCH  NPRGFLAG,NPRG7
        CALL    NPRGOPEN
NPRG7
        trap    IOMssg giving Error if IO
        FILEPI  1;NPRGFLIST
        DELETE  NPRGFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NPRGAIM
. REQUIRED    : NPRGFLD1, NPRGFLD2, NPRGFLD3, NPRGFLD4, NPRGFLD5, NPRGFLD6
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRGAIM
        BRANCH  NPRGFLAG,NPRG8
        CALL    NPRGOPEN
NPRG8
        trap    IOMssg giving Error if IO
        READ    NPRGFLE2,NPRGFLD1,NPRGFLD2,NPRGFLD3,NPRGFLD4,NPRGFLD5,NPRGFLD6;NPRGVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NPRGKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRGKG
        BRANCH  NPRGFLAG,NPRG10
        CALL    NPRGOPEN
NPRG10
        trap    IOMssg giving Error if IO
        READKG  NPRGFLE2;NPRGVARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NPRGOPEN
. REQUIRED    : 'NPRGFLAG'
. RETURNED    : 'NPRGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN INFORMATION SERVICES IN-HOUSE PROGRAM FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPRGOPEN
        TRAP    NPRGGONE giving Error if IO
        OPEN    NPRGFLIST       .,EXCLUSIVE
        trapclr IO
        MOVE    C1,NPRGFLAG
        RETURN
.
NPRGGONE
        MOVE    NPRGNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
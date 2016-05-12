..............................................................................
.
. NEXCIO INCLUSION
. NIN DATACARD EXCRESSING FILE I/O ROUTINES
.
. FILE NAME : NINEXC
. REC LENGTH: 226 VAR
. INDEX KEY : 1-10 (6 BYTE LIST NUMBER + 1 BYTE TYPE + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NEXCKEY
. REQUIRED    : 'NEXCFLD'
. RETURNED    : CLIENT EXCLUSIVITY TRACKING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NEXCKEY
        BRANCH  NEXCFLAG,NEXC1
        CALL    NEXCOPEN
NEXC1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NEXCFILE
        READ    NEXCFILE,NEXCFLD;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCTST
. REQUIRED    : NEXCFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NEXCTST
        BRANCH  NEXCFLAG,NEXC2
        CALL    NEXCOPEN
NEXC2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NEXCFILE
        READ    NEXCFILE,NEXCFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCKS
. REQUIRED    :
. RETURNED    : CLIENT EXCLUSIVITY TRACKING RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD EXCRESSING FILE READ
.
NEXCKS
        BRANCH  NEXCFLAG,NEXC3
        CALL    NEXCOPEN
NEXC3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NEXCFILE
        READKS  NEXCFILE;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCSEQ
. REQUIRED    :
. RETURNED    : CLIENT EXCLUSIVITY TRACKING RECORD
. DESCRIPTION : SEQUENTIAL DATACARD EXCRESSING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NEXCSEQ
        BRANCH  NEXCFLAG,NEXC4
        CALL    NEXCOPEN
NEXC4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NEXCFILE
        READ    NEXCFILE,SEQ;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCWRT
. REQUIRED    : 'NEXCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NEXCWRT
        BRANCH  NEXCFLAG,NEXC5
        CALL    NEXCOPEN
NEXC5
        trap    IOMssg giving Error if IO
        FILEPI  1;NEXCFLIST
        WRITE   NEXCFLIST;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NEXCUPD
        BRANCH  NEXCFLAG,NEXC6
        CALL    NEXCOPEN
NEXC6
        trap    IOMssg giving Error if IO
        FILEPI  1;NEXCFLIST
        UPDATE  NEXCFLIST;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCDEL
. REQUIRED    : 'NEXCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NEXCDEL
        BRANCH  NEXCFLAG,NEXC7
        CALL    NEXCOPEN
NEXC7
        trap    IOMssg giving Error if IO
        FILEPI  1;NEXCFLIST
        DELETE  NEXCFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NEXCAIM
. REQUIRED    : NEXCFLD1, NEXCFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NEXCAIM
        BRANCH  NEXCFLAG,NEXC8
        CALL    NEXCOPEN
NEXC8
        trap    IOMssg giving Error if IO
        READ    NEXCFLE2,NEXCFLD1,NEXCFLD2;NEXCVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NEXCKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NEXCKG
        BRANCH  NEXCFLAG,NEXC10
        CALL    NEXCOPEN
NEXC10
        trap    IOMssg giving Error if IO
        READKG  NEXCFLE2;NEXCVARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NEXCOPEN
. REQUIRED    : 'NEXCFLAG'
. RETURNED    : 'NEXCFLAG' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN CLIENT EXCLUSIVITY TRACKING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NEXCOPEN
        TRAP    NEXCGONE giving Error if IO
        OPEN    NEXCFLIST       .,EXCLUSIVE
.       OPEN    NEXCFLIST
        trapclr IO
        MOVE    C1,NEXCFLAG
        RETURN
.
NEXCGONE
        MOVE    NEXCNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
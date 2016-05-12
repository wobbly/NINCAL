..............................................................................
.
. NADDIO INCLUSION
. NIN DATACARD ADDRESSING FILE I/O ROUTINES
.
. FILE NAME : NINADD
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NADDKEY
. REQUIRED    : 'NADDFLD'
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NADDKEY
        BRANCH  NADDFLAG,NADD1
        CALL    NADDOPEN
NADD1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NADDFILE
        READ    NADDFILE,NADDFLD;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDTST
. REQUIRED    : NADDFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NADDTST
        BRANCH  NADDFLAG,NADD2
        CALL    NADDOPEN
NADD2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NADDFILE
        READ    NADDFILE,NADDFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDKS
. REQUIRED    :
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ
.
NADDKS
        BRANCH  NADDFLAG,NADD3
        CALL    NADDOPEN
NADD3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NADDFILE
        READKS  NADDFILE;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDSEQ
. REQUIRED    :
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NADDSEQ
        BRANCH  NADDFLAG,NADD4
        CALL    NADDOPEN
NADD4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NADDFILE
        READ    NADDFILE,SEQ;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDWRT
. REQUIRED    : 'NADDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NADDWRT
        BRANCH  NADDFLAG,NADD5
        CALL    NADDOPEN
NADD5
        trap    IOMssg giving Error if IO
        FILEPI  1;NADDFLIST
        WRITE   NADDFLIST;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NADDUPD
        BRANCH  NADDFLAG,NADD6
        CALL    NADDOPEN
NADD6
        trap    IOMssg giving Error if IO
        FILEPI  1;NADDFLIST
        UPDATE  NADDFLIST;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDDEL
. REQUIRED    : 'NADDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NADDDEL
        BRANCH  NADDFLAG,NADD7
        CALL    NADDOPEN
NADD7
        trap    IOMssg giving Error if IO
        FILEPI  1;NADDFLIST
        DELETE  NADDFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NADDAIM
. REQUIRED    : NADDFLD2  -- This documentation should say NADDFLD1 RVW 09/09/15
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NADDAIM
        BRANCH  NADDFLAG,NADD8
        CALL    NADDOPEN
NADD8
        trap    IOMssg giving Error if IO
        READ    NADDFLE2,NADDFLD1;NADDVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NADDKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NADDKG
        BRANCH  NADDFLAG,NADD10
        CALL    NADDOPEN
NADD10
        trap    IOMssg giving Error if IO
        READKG  NADDFLE2;NADDVARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NADDOPEN
. REQUIRED    : 'NADDFLAG'
. RETURNED    : 'NADDFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD ADDRESSING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NADDOPEN
        TRAP    NADDGONE giving Error if IO
        OPEN    NADDFLIST       .,EXCLUSIVE
.       OPEN    NADDFLIST
        trapclr IO
        MOVE    C1,NADDFLAG
        RETURN
.
NADDGONE
        MOVE    NADDNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
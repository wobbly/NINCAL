//..............................................................................

// NFTP22IO INCLUSION
// NIN DATACARD ADDRESSING FILE I/O ROUTINES

// FILE NAME : NINFTP2
// REC LENGTH: 974
// INDEX KEY : 1-9 (6 BYTE COMP NUMBER + 3 BYTE RECORD #), 1-12(6 BYTE COMP NUMBER + 3 BYTE FTP RECORD #+ 3 BYTE RECORD #)

//..............................................................................

// ENTRY POINT : NFTP2KEY
// REQUIRED    : 'NFTP2FLD'
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : EXACT ISAM KEY READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTP2KEY
        BRANCH  NFTP2FLAG,NFTP21
        CALL    NFTP2OPEN
NFTP21
        trap    IOMssg giving Error if IO
        READ    NFTP2FILE1,NFTP2FLD;NFTP2VARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2TST
// REQUIRED    : NFTP2FLD
// RETURNED    :
// DESCRIPTION : TEST KEY

NFTP2TST
        BRANCH  NFTP2FLAG,NFTP22
        CALL    NFTP2OPEN
NFTP22
        trap    IOMssg giving Error if IO
        READ    NFTP2FILE1,NFTP2FLD;;
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2KS
// REQUIRED    :
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ

NFTP2KS
        BRANCH  NFTP2FLAG,NFTP23
        CALL    NFTP2OPEN
NFTP23
        trap    IOMssg giving Error if IO
        READKS  NFTP2FILE1;NFTP2VARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2SEQ
// REQUIRED    :
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTP2SEQ
        BRANCH  NFTP2FLAG,NFTP24
        CALL    NFTP2OPEN
NFTP24
        trap    IOMssg giving Error if IO
        READ    NFTP2FILE,SEQ;NFTP2VARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2WRT
// REQUIRED    : 'NFTP2FLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT

NFTP2WRT
        BRANCH  NFTP2FLAG,NFTP25
        CALL    NFTP2OPEN
NFTP25
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTP2FLIST
        WRITE   NFTP2FLIST;NFTP2VARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2UPD
// REQUIRED    : A PREVIOUS KEY READ
// RETURNED    :
// DESCRIPTION : KEY UPDATE RECORD NUMBER FILE

NFTP2UPD
        BRANCH  NFTP2FLAG,NFTP26
        CALL    NFTP2OPEN
NFTP26
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTP2FLIST
        UPDATE  NFTP2FLIST;NFTP2VARS
        trapclr IO
        RETURN

//.............................................................................

// ENTRY POINT : NFTP2DEL
// REQUIRED    : 'NFTP2FLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NFTP2DEL
        BRANCH  NFTP2FLAG,NFTP27
        CALL    NFTP2OPEN
NFTP27
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTP2FLIST
        DELETE  NFTP2FLIST
        trapclr IO
        RETURN

//..............................................................................
// ENTRY POINT : NFTP2AIM
// REQUIRED    : NFTP2FLD1
// RETURNED    : RECORD
// DESCRIPTION : AIM FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFTP2AIM
        BRANCH  NFTP2FLAG,NFTP28
        CALL    NFTP2OPEN
NFTP28
        trap    IOMssg giving Error if IO
        READ    NFTP2FLE2,NFTP2FLD1;NFTP2VARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTP2KG
// REQUIRED    : VALID PREVIOUS AIM READ
// RETURNED    : RECORD
// DESCRIPTION : AIM KEY GENEREIC FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTP2KG
        BRANCH  NFTP2FLAG,NFTP210
        CALL    NFTP2OPEN
NFTP210
        trap    IOMssg giving Error if IO
        READKG  NFTP2FLE2;NFTP2VARS
        trapclr IO
        RETURN

//...............................................................................
// ENTRY POINT : NFTP2OPEN
// REQUIRED    : 'NFTP2FLAG'
// RETURNED    : 'NFTP2FLAG' SET TO '1' IF OPENNED
// DESCRIPTION : OPEN NIN DATACARD ADDRESSING FILE
//               DISPLAY ERROR AND ABORT IF NOT ON-LINE.

NFTP2OPEN
        TRAP    NFTP2GONE giving Error if IO
        OPEN    NFTP2FLIST       .,EXCLUSIVE
        trapclr IO
        MOVE    C1,NFTP2FLAG
        RETURN
.
NFTP2GONE
        MOVE    NFTP2NAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
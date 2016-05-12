//..............................................................................

// NFTPLOG2IO INCLUSION
// NIN DATACARD ADDRESSING FILE I/O ROUTINES

// FILE NAME : NINFTPLOG
// REC LENGTH: 346 VAR
// INDEX KEY : 6 (6 BYTE COMP NUMBER), 100 File NAME(AAM)

//..............................................................................

// ENTRY POINT : NFTPLOGKEY
// REQUIRED    : 'NFTPLOGFLD'
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : EXACT ISAM KEY READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

//NFTPLOGKEY
//        BRANCH  NFTPLOGFLAG,NFTPLOG1
//        CALL    NFTPLOGOPEN
//NFTPLOG1
//        trap    IOMssg giving Error if IO
//        READ    NFTPLOGFILE,NFTPLOGFLD;NFTPLOGVARS
//        trapclr IO
//        RETURN

//..............................................................................
//
// ENTRY POINT : NFTPLOGTST
// REQUIRED    : NFTPLOGFLD
// RETURNED    :
// DESCRIPTION : TEST KEY
//NFTPLOGTST
//        BRANCH  NFTPLOGFLAG,NFTPLOG2
//        CALL    NFTPLOGOPEN
//NFTPLOG2
//        trap    IOMssg giving Error if IO
//        READ    NFTPLOGFILE,NFTPLOGFLD;;
//        trapclr IO
//        RETURN

//..............................................................................
// ENTRY POINT : NFTPLOGKS
// REQUIRED    :
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ

//NFTPLOGKS
//        BRANCH  NFTPLOGFLAG,NFTPLOG3
//        CALL    NFTPLOGOPEN
//NFTPLOG3
//        trap    IOMssg giving Error if IO
//        READKS  NFTPLOGFILE;NFTPLOGVARS
//        trapclr IO
//        RETURN

//..............................................................................

// ENTRY POINT : NFTPLOGSEQ
// REQUIRED    :
// RETURNED    : DATACARD ADDRESSING RECORD
// DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTPLOGSEQ
        BRANCH  NFTPLOGFLAG,NFTPLOG4
        CALL    NFTPLOGOPEN
NFTPLOG4
        trap    IOMssg giving Error if IO
        READ    NFTPLOGFILE,SEQ;NFTPLOGVARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTPLOGWRT
// REQUIRED    : 'NFTPLOGFLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
NFTPLOGWRT
        BRANCH  NFTPLOGFLAG,NFTPLOG5
        CALL    NFTPLOGOPEN
NFTPLOG5
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPLOGFLIST
        WRITE   NFTPLOGFLIST;NFTPLOGVARS
        trapclr IO
        RETURN

//.............................................................................

// ENTRY POINT : NFTPLOGUPD
// REQUIRED    : A PREVIOUS KEY READ
// RETURNED    :
// DESCRIPTION : KEY UPDATE RECORD NUMBER FILE

NFTPLOGUPD
        BRANCH  NFTPLOGFLAG,NFTPLOG6
        CALL    NFTPLOGOPEN
NFTPLOG6
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPLOGFLIST
        UPDATE  NFTPLOGFLIST;NFTPLOGVARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTPLOGDEL
// REQUIRED    : 'NFTPLOGFLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
//
NFTPLOGDEL
        BRANCH  NFTPLOGFLAG,NFTPLOG7
        CALL    NFTPLOGOPEN
NFTPLOG7
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPLOGFLIST
        DELETE  NFTPLOGFLIST
        trapclr IO
        RETURN

//..............................................................................
// ENTRY POINT : NFTPLOGAIM
// REQUIRED    : NFTPLOGFLD1,NFTPLOGFLD2
// RETURNED    : RECORD
// DESCRIPTION : AIM FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
NFTPLOGAIM
        BRANCH  NFTPLOGFLAG,NFTPLOG8
        CALL    NFTPLOGOPEN
NFTPLOG8
        trap    IOMssg giving Error if IO
        READ    NFTPLOGFLE2,NFTPLOGFLD1,NFTPLOGFLD2;NFTPLOGVARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTPLOGKG
// REQUIRED    : VALID PREVIOUS AIM READ
// RETURNED    : RECORD
// DESCRIPTION : AIM KEY GENEREIC FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTPLOGKG
        BRANCH  NFTPLOGFLAG,NFTPLOG10
        CALL    NFTPLOGOPEN
NFTPLOG10
        trap    IOMssg giving Error if IO
        READKG  NFTPLOGFLE2;NFTPLOGVARS
        trapclr IO
        RETURN

//..............................................................................
//
// ENTRY POINT : NFTPLOGOPEN
// REQUIRED    : 'NFTPLOGFLAG'
// RETURNED    : 'NFTPLOGFLAG' SET TO '1' IF OPENNED
// DESCRIPTION : OPEN NIN DATACARD ADDRESSING FILE
//               DISPLAY ERROR AND ABORT IF NOT ON-LINE.

NFTPLOGOPEN
        TRAP    NFTPLOGGONE giving Error if IO
        OPEN    NFTPLOGFLIST       .,EXCLUSIVE
        trapclr IO
        MOVE    C1,NFTPLOGFLAG
        RETURN
NFTPLOGGONE
        MOVE    NFTPLOGNAME,FILENAME
        CALL    FILEGONE
//..............................................................................
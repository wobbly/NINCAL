.//..............................................................................
.//
.// NFTPIO INCLUSION
.// NIN DATACARD ADDRESSING FILE I/O ROUTINES
.//
.// FILE NAME : NINFTP
.// REC LENGTH: 974 VAR
.// INDEX KEY : 
.// 2013 November 26  after update or write use copyfile for winbatch jobs
.//.............................................................................
.//
.// ENTRY POINT : NFTPKEY
.// REQUIRED    : 'NFTPFLD'
.// RETURNED    : DATACARD ADDRESSING RECORD
.// DESCRIPTION : EXACT ISAM KEY READ
.//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTPKEY
        BRANCH  NFTPFLAG,NFTP1
        CALL    NFTPOPEN
NFTP1
        trap    IOMssg giving Error if IO
        READ    NFTPFILE1,NFTPFLD;NFTPVARS
        trapclr IO
        RETURN

.//..............................................................................
.//
.// ENTRY POINT : NFTPTST
.// REQUIRED    : NFTPFLD
.// RETURNED    :
.// DESCRIPTION : TEST KEY
.//
NFTPTST
        BRANCH  NFTPFLAG,NFTP2
        CALL    NFTPOPEN
NFTP2
        trap    IOMssg giving Error if IO
        READ    NFTPFILE1,NFTPFLD;;
        trapclr IO
        RETURN

.//..............................................................................
.
.// ENTRY POINT : NFTPKS
.// REQUIRED    :
.// RETURNED    : DATACARD ADDRESSING RECORD
.// DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ
.
.//NFTPKS
.//        BRANCH  NFTPFLAG,NFTP3
.//        CALL    NFTPOPEN
.//NFTP3
.//        trap    IOMssg giving Error if IO
.//        READKS  NFTPFILE1;NFTPVARS
.//        trapclr IO
.//        RETURN
.
.//..............................................................................
.
.// ENTRY POINT : NFTPSEQ
.// REQUIRED    :
.// RETURNED    : DATACARD ADDRESSING RECORD
.// DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
.//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFTPSEQ
        BRANCH  NFTPFLAG,NFTP4
        CALL    NFTPOPEN
NFTP4
        trap    IOMssg giving Error if IO
        READ    NFTPFILE,SEQ;NFTPVARS
        trapclr IO
        RETURN

//..............................................................................

// ENTRY POINT : NFTPWRT
// REQUIRED    : 'NFTPFLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT

NFTPWRT
        BRANCH  NFTPFLAG,NFTP5
        CALL    NFTPOPEN
NFTP5
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPFLIST
        WRITE   NFTPFLIST;NFTPVARS
        trapclr IO
.begin patch
          copyfile  "\\nins1\e\data\text\ninftp.dat","\\nins1\e\data\text\ninftp.batch"
.end patch
        RETURN

//..............................................................................

// ENTRY POINT : NFTPUPD
// REQUIRED    : A PREVIOUS KEY READ
// RETURNED    :
// DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NFTPUPD
        BRANCH  NFTPFLAG,NFTP6
        CALL    NFTPOPEN
NFTP6
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPFLIST
        UPDATE  NFTPFLIST;NFTPVARS
        trapclr IO
.begin patch
          copyfile  "\\nins1\e\data\text\ninftp.dat","\\nins1\e\data\text\ninftp.batch"
.end patch
        RETURN

//..............................................................................

// ENTRY POINT : NFTPDEL
// REQUIRED    : 'NFTPFLD'
// RETURNED    :
// DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE

NFTPDEL
        BRANCH  NFTPFLAG,NFTP7
        CALL    NFTPOPEN
NFTP7
        trap    IOMssg giving Error if IO
        FILEPI  1;NFTPFLIST
        DELETE  NFTPFLIST
        trapclr IO
.begin patch
          copyfile  "\\nins1\e\data\text\ninftp.dat","\\nins1\e\data\text\ninftp.batch"
.end patch
        RETURN

//.............................................................................
// ENTRY POINT : NFTPAIM
// REQUIRED    : NFTPFLD2
// RETURNED    : RECORD
// DESCRIPTION : AIM FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTPAIM
        BRANCH  NFTPFLAG,NFTP8
        CALL    NFTPOPEN
NFTP8
        trap    IOMssg giving Error if IO
        READ    NFTPFLE2,NFTPFLD1;NFTPVARS
        trapclr IO
        RETURN
//.............................................................................
// ENTRY POINT : NFTPKG
// REQUIRED    : VALID PREVIOUS AIM READ
// RETURNED    : RECORD
// DESCRIPTION : AIM KEY GENEREIC FILE READ
//               APPLICATION'S RESPONSIBILITY TO TEST FLAGS

NFTPKG
        BRANCH  NFTPFLAG,NFTP10
        CALL    NFTPOPEN
NFTP10
        trap    IOMssg giving Error if IO
       READKG  NFTPFLE2;NFTPVARS
        trapclr IO
        RETURN
//...............................................................................
.
// ENTRY POINT : NFTPOPEN
// REQUIRED    : 'NFTPFLAG'
// RETURNED    : 'NFTPFLAG' SET TO '1' IF OPENNED
// DESCRIPTION : OPEN NIN DATACARD ADDRESSING FILE
//               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
NFTPOPEN
        TRAP    NFTPGONE giving Error if IO
        OPEN    NFTPFLIST       .,EXCLUSIVE
        trapclr IO
        MOVE    C1,NFTPFLAG
        RETURN
.
NFTPGONE
        MOVE    NFTPNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
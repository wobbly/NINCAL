******************************************************
*  SRDSSLTDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS SRDS FILE.
* ****************************************************
.
.      FILE:      NISRDSSLT
.    LENGTH:      51
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6         SRDSSLTLIST
..............................................................................
.
. ENTRY POINT : SRDSSLTKEY
. REQUIRED    : 'SRDSSLTFLD'
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSSLTKEY
          BRANCH    SRDSSLTFLAG,SRDSSLT1
          CALL      SRDSSLTOPEN
SRDSSLT1
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFILE
          READ      SRDSSLTFILE,SRDSSLTFLD;SRDSSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTTST
. REQUIRED    : SRDSSLTFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSSLTTST
          BRANCH    SRDSSLTFLAG,SRDSSLT2
          CALL      SRDSSLTOPEN
SRDSSLT2
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFILE
          READ      SRDSSLTFILE,SRDSSLTFLD;;
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTKS
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SELECTION FILE READ
.
SRDSSLTKS
          BRANCH    SRDSSLTFLAG,SRDSSLT3
          CALL      SRDSSLTOPEN
SRDSSLT3
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFILE
          READKS    SRDSSLTFILE;SRDSSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTKS
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SELECTION FILE READ
.
SRDSSLTKS1
          BRANCH    SRDSSLTFLAG,SRDSSLT3A
          CALL      SRDSSLTOPEN
SRDSSLT3A
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFILE
          READKS    SRDSSLTFILE;SRDSSLTLIST
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTSEQ
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : SEQUENTIAL DATACARD SELECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSSLTSEQ
          BRANCH    SRDSSLTFLAG,SRDSSLT4
          CALL      SRDSSLTOPEN
SRDSSLT4
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFILE
          READ      SRDSSLTFILE,SEQ;SRDSSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTWRT
. REQUIRED    : 'SRDSSLTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSSLTWRT
          BRANCH    SRDSSLTFLAG,SRDSSLT5
          CALL      SRDSSLTOPEN
SRDSSLT5
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFLIST
          WRITE     SRDSSLTFLIST;SRDSSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
SRDSSLTUPD
          BRANCH    SRDSSLTFLAG,SRDSSLT6
          CALL      SRDSSLTOPEN
SRDSSLT6
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFLIST
          UPDATE    SRDSSLTFLIST;SRDSSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTDEL
. REQUIRED    : 'SRDSSLTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSSLTDEL
          BRANCH    SRDSSLTFLAG,SRDSSLT7
          CALL      SRDSSLTOPEN
SRDSSLT7
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSSLTFLIST
          DELETE    SRDSSLTFLIST
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSSLTOPEN
. REQUIRED    : 'SRDSSLTFLAG'
. RETURNED    : 'SRDSSLTFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN srds DATACARD SELECTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSSLTOPEN
          TRAP      SRDSSLTGONE giving Error if IO
          OPEN      SRDSSLTFLIST 
          trapclr   IO
          MOVE      C1,SRDSSLTFLAG
          RETURN
.
SRDSSLTGONE
          MOVE      SRDSSLTNAME,FILENAME
          CALL      FILEGONE
.
..............................................................................

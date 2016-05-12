................................................................................
.                                                                             .
.NLRA2IO.INC - IO MODULE
...............................................................................
. RELEASE 1.0 
.
. FILENAME:     NINLRA2
. LENGTH:       84 FIXED
. ACCESS:       ISI
. KEY   :       1-9    (List NUMBER + LRA Num)
.               
...............................................................................
.
. ENTRY POINT : NLRA2KEY
. REQUIRED    : 'NLRA2FLD'
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLRA2KEY  BRANCH    NLRA2FLAG TO NLRA21
         CALL      NLRA2OPEN
NLRA21    trap      IOMssg giving Error if IO
         branch    NLRA2LOCK to NLRA21L,NLRA21R,NLRA21N
NLRA21L   FILEPI    1;NLRA2FILE
         READ      NLRA2FILE,NLRA2FLD;NLRA2VARS
         trapclr   IO
         RETURN
NLRA21R   READLK    NLRA2FILE,NLRA2FLD;NLRA2VARS
         trapclr   IO
         RETURN
NLRA21N   READ      NLRA2FILE,NLRA2FLD;NLRA2VARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRA2TST
. REQUIRED    : NLRA2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NLRA2TST  BRANCH    NLRA2FLAG TO NLRA22
         CALL      NLRA2OPEN
NLRA22

	trap      IOMssg giving Error if IO
         branch    NLRA2LOCK to NLRA22L,NLRA22R,NLRA22N
NLRA22L   FILEPI    1;NLRA2FILE
         READ      NLRA2FILE,NLRA2FLD;;
         trapclr   IO
         RETURN
NLRA22R   READLK    NLRA2FILE,NLRA2FLD;;
         trapclr   IO
         RETURN
NLRA22N   READ      NLRA2FILE,NLRA2FLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRA2KS
. REQUIRED    :
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NLRA2KS   BRANCH    NLRA2FLAG TO NLRA23
         CALL      NLRA2OPEN
NLRA23    trap      IOMssg giving Error if IO
         branch    NLRA2LOCK to NLRA23L,NLRA23R,NLRA23N
NLRA23L   FILEPI    1;NLRA2FILE
         READKS    NLRA2FILE;NLRA2VARS
         trapclr   IO
         RETURN
NLRA23R   READKSLK  NLRA2FILE;NLRA2VARS
         trapclr   IO
         RETURN
NLRA23N   READKS    NLRA2FILE;NLRA2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLRA2SEQ
. REQUIRED    :
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLRA2SEQ  BRANCH    NLRA2FLAG TO NLRA24
         CALL      NLRA2OPEN
NLRA24    trap      IOMssg giving Error if IO
         branch    NLRA2LOCK to NLRA24L,NLRA24R,NLRA24N
NLRA24L   FILEPI    1;NLRA2FILE
         READ      NLRA2FILE,SEQ;NLRA2VARS
         trapclr   IO
         RETURN
NLRA24R   READLK    NLRA2FILE,SEQ;NLRA2VARS
         trapclr   IO
         RETURN
NLRA24N   READ      NLRA2FILE,SEQ;NLRA2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLRA2WRT
. REQUIRED    : 'NLRA2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NLRA2WRT  BRANCH    NLRA2FLAG TO NLRA25      C1 TO NLRA2PATH
         CALL      NLRA2OPEN
NLRA25    trap      IOMssg giving Error if IO
	FILEPI    1;NLRA2FLIST
	WRITE     NLRA2FLIST;NLRA2VARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NLRA2UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NLRA2UPD  BRANCH     NLRA2FLAG TO NLRA26
         CALL       NLRA2OPEN
NLRA26    trap       IOMssg giving Error if IO
         FILEPI     1;NLRA2FLIST
         UPDATE     NLRA2FLIST;NLRA2VARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRA2DEL
. REQUIRED    : 'NLRA2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NLRA2DEL  BRANCH    NLRA2FLAG TO NLRA27
         CALL      NLRA2OPEN
NLRA27    trap      IOMssg giving Error if IO
         FILEPI    1;NLRA2FLIST
         DELETE    NLRA2FLIST
         trapclr   IO
         RETURN
.............................................................................
.
. ENTRY POINT : NLRA2OPEN
. REQUIRED    : 'NLRA2FLAG'
. RETURNED    : 'NLRA2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN NINLRA_2 FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLRA2OPEN
         TRAP      NLRA2GONE IF IO
         OPEN      NLRA2FLIST
         TRAPCLR   IO
         MOVE      C1 TO NLRA2FLAG
         RETURN
.
NLRA2GONE MOVE      NLRA2NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................


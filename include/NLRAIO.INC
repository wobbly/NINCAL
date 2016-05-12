................................................................................
.                                                                             .
.NLRAIO.INC - IO MODULE
...............................................................................
. RELEASE 1.0 
.
. FILENAME:     NINLRA
. LENGTH:       60 FIXED
. ACCESS:       ISI
. KEY   :       1-6    (List NUMBER)
.               7-12   (Mailer NUMBER)
...............................................................................
.
. ENTRY POINT : NLRAKEY
. REQUIRED    : 'NLRAFLD'
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLRAKEY  BRANCH    NLRAFLAG TO NLRA1
         CALL      NLRAOPEN
NLRA1    	Branch    NLraPath to NLRA1a,NLRA1b
NLRA1a    trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA1aL,NLRA1aR,NLRA1aN
NLRA1aL   FILEPI    1;NLRAFILE
         READ      NLRAFILE,NLRAFLD;NLRAVARS
         trapclr   IO
         RETURN
NLRA1aR   READLK    NLRAFILE,NLRAFLD;NLRAVARS
         trapclr   IO
         RETURN
NLRA1aN   READ      NLRAFILE,NLRAFLD;NLRAVARS
         trapclr   IO
         RETURN
NLRA1b    trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA1bL,NLRA1bR,NLRA1bN
NLRA1bL   filePI    1;NLRAfil1
         READ      NLRAfil1,NLRAFLD1;NLRAVARS
         trapclr   IO
         RETURN
NLRA1bR   READLK    NLRAfil1,NLRAFLD1;NLRAVARS
         trapclr   IO
         RETURN
NLRA1bN   READ      NLRAfil1,NLRAFLD1;NLRAVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRATST
. REQUIRED    : NLRAFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NLRATST  BRANCH    NLRAFLAG TO NLRA2
         CALL      NLRAOPEN
NLRA2    	Branch    NLraPath to NLRA2a,NLRA2b

NLRA2a   trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA2L,NLRA2R,NLRA2N
NLRA2L   FILEPI    1;NLRAFILE
         READ      NLRAFILE,NLRAFLD;;
         trapclr   IO
         RETURN
NLRA2R   READLK    NLRAFILE,NLRAFLD;;
         trapclr   IO
         RETURN
NLRA2N   READ      NLRAFILE,NLRAFLD;;
         trapclr   IO
         RETURN
NLRA2b   trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA2bL,NLRA2bR,NLRA2bN
NLRA2bL   FILEPI    1;NLRAFILE
         READ      NLRAFIL1,NLRAFLD1;;
         trapclr   IO
         RETURN
NLRA2bR   READLK    NLRAFIL1,NLRAFLD1;;
         trapclr   IO
         RETURN
NLRA2bN   READ      NLRAFIL1,NLRAFLD1;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRAKS
. REQUIRED    :
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NLRAKS   BRANCH    NLRAFLAG TO NLRA3
         CALL      NLRAOPEN
NLRA3    	Branch    NLraPath to NLRA3a,NLRA3b

NLRA3a    trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA3aL,NLRA3aR,NLRA3aN
NLRA3aL   FILEPI    1;NLRAFILE
         READKS    NLRAFILE;NLRAVARS
         trapclr   IO
         RETURN
NLRA3aR   READKSLK  NLRAFILE;NLRAVARS
         trapclr   IO
         RETURN
NLRA3aN   READKS    NLRAFILE;NLRAVARS
         trapclr   IO
         RETURN
NLRA3b    trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA3bL,NLRA3bR,NLRA3bN
NLRA3bL   filePI    1;NLRAfil1
         READKS    NLRAfil1;NLRAVARS
         trapclr   IO
         RETURN
NLRA3bR   READKSLK  NLRAfil1;NLRAVARS
         trapclr   IO
         RETURN
NLRA3bN   READKS    NLRAfil1;NLRAVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLRASEQ
. REQUIRED    :
. RETURNED    : LRA RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLRASEQ  BRANCH    NLRAFLAG TO NLRA4
         CALL      NLRAOPEN
NLRA4    trap      IOMssg giving Error if IO
         branch    NLRALOCK to NLRA4L,NLRA4R,NLRA4N
NLRA4L   FILEPI    1;NLRAFILE
         READ      NLRAFILE,SEQ;NLRAVARS
         trapclr   IO
         RETURN
NLRA4R   READLK    NLRAFILE,SEQ;NLRAVARS
         trapclr   IO
         RETURN
NLRA4N   READ      NLRAFILE,SEQ;NLRAVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLRAWRT
. REQUIRED    : 'NLRAFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NLRAWRT  BRANCH    NLRAFLAG TO NLRA5      C1 TO NLRAPATH
         CALL      NLRAOPEN
NLRA5    trap      IOMssg giving Error if IO
	FILEPI    1;NLRAFLIST
	WRITE     NLRAFLIST;NLRAVARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NLRAUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NLRAUPD  BRANCH     NLRAFLAG TO NLRA6
         CALL       NLRAOPEN
NLRA6    trap       IOMssg giving Error if IO
         FILEPI     1;NLRAFLIST
         UPDATE     NLRAFLIST;NLRAVARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLRADEL
. REQUIRED    : 'NLRAFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NLRADEL  BRANCH    NLRAFLAG TO NLRA7
         CALL      NLRAOPEN
NLRA7    trap      IOMssg giving Error if IO
         FILEPI    1;NLRAFLIST
         DELETE    NLRAFLIST
         trapclr   IO
         RETURN
.............................................................................
.
. ENTRY POINT : NLRAOPEN
. REQUIRED    : 'NLRAFLAG'
. RETURNED    : 'NLRAFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN NINLRAR FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLRAOPEN
         TRAP      NLRAGONE IF IO
         OPEN      NLRAFLIST
         TRAPCLR   IO
         MOVE      C1 TO NLRAFLAG
         RETURN
.
NLRAGONE MOVE      NLRANAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................


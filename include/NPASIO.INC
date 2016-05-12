..............................................................................
.
. NPASIO INCLUSION
. NIN PASSWORD FILE I/O ROUTINES
.
.
. FILE NAME : NINPASS
. REC LENGTH: 68 VARIABLE
. INDEX KEY : 1-6 .
..............................................................................
.
. ENTRY POINT : NPASKEY
. REQUIRED    : 'NPASFLD'
. RETURNED    : PASSWORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPASKEY  BRANCH    NPASFLAG TO NPAS1
         CALL      NPASOPEN
NPAS1    trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS1L,NPAS1R,NPAS1N
NPAS1L   FILEPI    1;NPASFILE
         READ      NPASFILE,NPASFLD;NPASLIST
         trapclr   IO
         RETURN
NPAS1R   READLK    NPASFILE,NPASFLD;NPASLIST
         trapclr   IO
         RETURN
NPAS1N   READ      NPASFILE,NPASFLD;NPASLIST
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASKEY1
. REQUIRED    : 'NPASFLD1'
. RETURNED    : PASSWORD RECORD
. DESCRIPTION : EXACT ISAM SECONDARY KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPASKEY1 BRANCH    NPASFLAG TO NPAS1A
         CALL      NPASOPEN
NPAS1A   trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS1AL,NPAS1AR,NPAS1AN
NPAS1AL  FILEPI    1;npasfle1
         read	   npasfle1, npasfld1;npaslist
         trapclr   IO
         RETURN
NPAS1AR  readlk    npasfle1, npasfld1;npaslist
         trapclr   IO
         RETURN
NPAS1AN	 read      npasfle1, npasfld1;npaslist
         trapclr   IO
         RETURN

...................................................................................
.
. ENTRY POINT : NPASTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPASTST  BRANCH    NPASFLAG TO NPAS2
         CALL      NPASOPEN
NPAS2    trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS2L,NPAS2R,NPAS2N
NPAS2L   FILEPI    1;NPASFILE
         READ      NPASFILE,NPASFLD;STR1
         trapclr   IO
         RETURN
NPAS2R   READLK    NPASFILE,NPASFLD;STR1
         trapclr   IO
         RETURN
NPAS2N   READ      NPASFILE,NPASFLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASKS
. REQUIRED    :
. RETURNED    : PASSWORD RECORD
. DESCRIPTION : KEY SEQUENTIAL PASSWORD FILE READ
.
NPASKS   BRANCH    NPASFLAG TO NPAS3
         CALL      NPASOPEN
NPAS3    trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS3L,NPAS3R,NPAS3N
NPAS3L   FILEPI    1;NPASFILE
         READKS    NPASFILE;NPASLIST
         trapclr   IO
         RETURN
NPAS3R   READKSLK  NPASFILE;NPASLIST
         trapclr   IO
         RETURN
NPAS3N   READKS    NPASFILE;NPASLIST
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASKS1
. REQUIRED    :
. RETURNED    : PASSWORD RECORD
. DESCRIPTION : KEY SEQUENTIAL PASSWORD FILE READ
.
NPASKS1  BRANCH    NPASFLAG TO NPAS3A
         CALL      NPASOPEN
NPAS3A   trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS3aL,NPAS3AR,NPAS3AN
NPAS3AL  FILEPI    1;NPASFLE1
         READKS    NPASFLE1;NPASLIST
         trapclr   IO
         RETURN
NPAS3AR  READKSLK  NPASFLE1;NPASLIST
         trapclr   IO
         RETURN
NPAS3AN  READKS    NPASFLE1;NPASLIST
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASSEQ
. REQUIRED    :
. RETURNED    : PASSWORD RECORD
. DESCRIPTION : SEQUENTIAL PASSWORD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPASSEQ  BRANCH    NPASFLAG TO NPAS4
         CALL      NPASOPEN
NPAS4    trap      IOMssg giving Error if IO
         branch    NPASLOCK to NPAS4L,NPAS4R,NPAS4N
NPAS4L   FILEPI    1;NPASFILE
         READ      NPASFILE,SEQ;NPASLIST
         trapclr   IO
         RETURN
NPAS4R   READLK    NPASFILE,SEQ;NPASLIST
         trapclr   IO
         RETURN
NPAS4N   READ      NPASFILE,SEQ;NPASLIST
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASAIM
. REQUIRED    : 'NPASFLD2'
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPASAIM		BRANCH 	NPASFLAG, NPAS30
		CALL 	NPASOPEN
NPAS30		TRAP 	IOMssg giving Error if IO
		READ	NPASFLE2, NPASFLD2; NPASLIST
		TRAPCLR	IO
		RETURN
.
..............................................................................
.
. ENTRY POINT : NPASKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPASKG	BRANCH	NPASFLAG, NPAS35
	CALL	NPASOPEN

NPAS35	TRAP 	IOMssg giving Error if IO
	READKG	NPASFLE2; NPASLIST
	TRAPCLR	IO
	RETURN
............................................................................................................................................................
.
. ENTRY POINT : NPASWRT
. REQUIRED    : 'NPASFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPASWRT  BRANCH    NPASFLAG OF NPAS5
         CALL      NPASOPEN
NPAS5    trap      IOMssg giving Error if IO
         FILEPI    1;NPASFILE
         //WRITE     NPASFILE,NPASFLD;NPASLIST   //  DMS 03/01/06
         WRITE     NPASFLIST;NPASLIST
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PASSWORD FILE
.
NPASUPD
        BRANCH  NPASFLAG,NPAS40
        CALL    NPASOPEN
NPAS40
        trap    IOMssg giving Error if IO
        FILEPI  1;NPASFLIST
        UPDATE  NPASFLIST;NPASLIST
        trapclr IO
        RETURN
..............................................................................
.
. ENTRY POINT : NPASDEL
. REQUIRED    : 'NPASFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPASDEL  BRANCH    NPASFLAG TO NPAS7
         CALL      NPASOPEN
NPAS7    trap      IOMssg giving Error if IO
         FILEPI    1;NPASFILE
         //DELETE    NPASFILE,NPASFLD
         DELETE    NPASFLIST
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPASOPEN
. REQUIRED    : 'NPASFLAG'
. RETURNED    : 'NPASFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PASSWORD FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPASOPEN TRAP      NPASGONE GIVING ERROR IF IO
         //OPEN      NPASFILE,NPASNAME
         OPEN      NPASFLIST		.,EXCLUSIVE
         TRAPCLR   IO
         MOVE      C1 TO NPASFLAG
         RETURN
.
NPASGONE
	MOVE      NPASNAME TO FILENAME
        CALL      FILEGONE
.
..............................................................................

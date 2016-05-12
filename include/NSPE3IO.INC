..............................................................................
.
. NSPE3IO INCLUSION
.
. FILE NAME : NINSPEC3
. REC LENGTH: 3006
. INDEX KEY : 1-6 
.
..............................................................................
.
. ENTRY POINT : NSPE3KEY
. REQUIRED    : 'NSPE3FLD'
. RETURNED    : PLI SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPE3KEY
	 call      Trim using NSPE3FLD
	 if (NSPE3FLD = "")
		   flagrestore	"0001"
		   return
	 endif
         BRANCH    NSPE3FLAG TO NSPE31
         CALL      NSPE3OPEN
         
NSPE31   trap      IOMssg giving Error if IO
         branch    NSPE3lock of NSPE31L,NSPE31R,NSPE31N
.default FIlepi's active
NSPE31L  FILEPI    1;NSPE3FILE
         READ      NSPE3FILE,NSPE3FLD;NSPE3VARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
NSPE31R
         READLK    NSPE3FILE,NSPE3FLD;NSPE3VARS
         trapclr  IO
         RETURN
.No locks
NSPE31N
         READ      NSPE3FILE,NSPE3FLD;NSPE3VARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3TST
. REQUIRED    : NSPE3FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSPE3TST BRANCH    NSPE3FLAG TO NSPE32
         CALL      NSPE3OPEN
NSPE32   trap      IOMssg giving Error if IO
         FILEPI    1;NSPE3FILE
         READ      NSPE3FILE,NSPE3FLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3KS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NSPE3KS  BRANCH    NSPE3FLAG TO NSPE33
         CALL      NSPE3OPEN
NSPE33   trap      IOMssg giving Error if IO
         BRANCH    NSPE3LOCK OF NSPE33L,NSPE33R,NSPE33N

NSPE33L  FILEPI    1;NSPE3FILE
         READKS    NSPE3FILE;NSPE3VARS
         trapclr   IO
         RETURN
NSPE33R
         READKSLK  NSPE3FILE;NSPE3VARS
         trapclr   IO
         RETURN
NSPE33N
         READKS    NSPE3FILE;NSPE3VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3SEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPE3SEQ BRANCH    NSPE3FLAG TO NSPE34
         CALL      NSPE3OPEN
NSPE34   trap      IOMssg giving Error if IO
         BRANCH    NSPE3LOCK OF NSPE34L,NSPE34R,NSPE34N
NSPE34L  FILEPI    1;NSPE3FILE
         READ      NSPE3FILE,SEQ;NSPE3VARS
         trapclr   IO
         RETURN
NSPE34R
         READLK    NSPE3FILE,SEQ;NSPE3VARS
         trapclr   IO
         RETURN
NSPE34N
         READ      NSPE3FILE,SEQ;NSPE3VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3WRT
. REQUIRED    : NSPE3FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSPE3WRT BRANCH    NSPE3FLAG OF NSPE35
         CALL      NSPE3OPEN
NSPE35   trap      IOMssg giving Error if IO
         BRANCH    NSPE3LOCK TO NSPE35L,NSPE35R
NSPE35L  FILEPI    1;NSPE3FILE
         WRITE     NSPE3FILE,NSPE3FLD;NSPE3VARS
         trapclr   IO
         RETURN
NSPE35R
         WRITE     NSPE3FILE,NSPE3FLD;NSPE3VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3DEL
. REQUIRED    : NSPE3FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSPE3DEL BRANCH    NSPE3FLAG TO NSPE37
         CALL      NSPE3OPEn
NSPE37   trap      IOMssg giving Error if IO
         FILEPI    1;NSPE3FILE
         DELETE    NSPE3FILE,NSPE3FLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE3OPEN
. REQUIRED    : 'NSPE3FLAG'
. RETURNED    : 'NSPE3FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSPE3OPEN TRAP     NSPE3GONE IF IO
         BRANCH    NSPE3LOCK TO NSPE3OPNL,NSPE3OPNR

NSPE3OPNL OPEN     NSPE3FILE,NSPE3NAME
         TRAPCLR   IO
         MOVE      C1 TO NSPE3FLAG
         RETURN
NSPE3OPNR OPEN     NSPE3FILE,NSPE3NAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NSPE3FLAG
         RETURN

.
NSPE3GONE MOVE     NSPE3NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

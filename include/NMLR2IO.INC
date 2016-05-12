..............................................................................
.
. NMLR2IO INCLUSION
.
. FILE NAME : NINMLR2
. REC LENGTH: 754
. INDEX KEY : 1-4 
..............................................................................
.
. ENTRY POINT : NMLR2KEY
. REQUIRED    : 'NMLR2FLD'
. RETURNED    : MAILER NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLR2KEY BRANCH    NMLR2FLAG TO NMLR21
         CALL      NMLR2OPEN
         
NMLR21   trap      IOMssg giving Error if IO
         branch    nMLR2lock of nMLR21L,NMLR21R,NMLR21N
.default FIlepi's active
NMLR21L  FILEPI    1;NMLR2FILE
         READ      NMLR2FILE,NMLR2FLD;NMLR2VARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
NMLR21R
         READLK    NMLR2FILE,NMLR2FLD;NMLR2VARS
         trapclr  IO
         RETURN
.No locks
NMLR21N
         READ      NMLR2FILE,NMLR2FLD;NMLR2VARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2TST
. REQUIRED    : NMLR2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMLR2TST BRANCH    NMLR2FLAG TO NMLR22
         CALL      NMLR2OPEN
NMLR22   trap      IOMssg giving Error if IO
         FILEPI    1;NMLR2FILE
         READ      NMLR2FILE,NMLR2FLD;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2KS
. REQUIRED    :
. RETURNED    : MAILER NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL MLRCIAL INSTRUCTION FILE READ
.
NMLR2KS  BRANCH    NMLR2FLAG TO NMLR23
         CALL      NMLR2OPEN
NMLR23   trap      IOMssg giving Error if IO
         BRANCH    NMLR2LOCK OF NMLR23L,NMLR23R,NMLR23N

NMLR23L  FILEPI    1;NMLR2FILE
         READKS    NMLR2FILE;NMLR2VARS
         trapclr   IO
         RETURN
NMLR23R
         READKSLK  NMLR2FILE;NMLR2VARS
         trapclr   IO
         RETURN
NMLR23N
         READKS    NMLR2FILE;NMLR2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2SEQ
. REQUIRED    :
. RETURNED    : MAILER NOTES RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLR2SEQ BRANCH    NMLR2FLAG TO NMLR24
         CALL      NMLR2OPEN
NMLR24   trap      IOMssg giving Error if IO
         BRANCH    NMLR2LOCK OF NMLR24L,NMLR24R,NMLR24N
NMLR24L  FILEPI    1;NMLR2FILE
         READ      NMLR2FILE,SEQ;NMLR2VARS
         trapclr   IO
         RETURN
NMLR24R
         READLK    NMLR2FILE,SEQ;NMLR2VARS
         trapclr   IO
         RETURN
NMLR24N
         READ      NMLR2FILE,SEQ;NMLR2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2WRT
. REQUIRED    : NMLR2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMLR2WRT BRANCH    NMLR2FLAG OF NMLR25
         CALL      NMLR2OPEN
NMLR25   FILEPI    1;NMLR2FILE
         WRITE     NMLR2FILE,NMLR2FLD;NMLR2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2UPD
. REQUIRED    : PREVIOUS READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI UPDATE
.
NMLR2UPD BRANCH    NMLR2FLAG OF NMLR26
         CALL      NMLR2OPEN
NMLR26   FILEPI    1;NMLR2FILE
         UPDATE    NMLR2FILE;NMLR2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2DEL
. REQUIRED    : NMLR2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMLR2DEL BRANCH    NMLR2FLAG TO NMLR27
         CALL      NMLR2OPEn
NMLR27   trap      IOMssg giving Error if IO
         FILEPI    1;NMLR2FILE
         DELETE    NMLR2FILE,NMLR2FLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLR2OPEN
. REQUIRED    : 'NMLR2FLAG'
. RETURNED    : 'NMLR2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMLR2OPEN TRAP     NMLR2GONE IF IO
         BRANCH    NMLR2LOCK TO NMLR2OPNL,NMLR2OPNR

NMLR2OPNL OPEN     NMLR2FILE,NMLR2NAME
         TRAPCLR   IO
         MOVE      C1 TO NMLR2FLAG
         RETURN
NMLR2OPNR OPEN     NMLR2FILE,NMLR2NAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NMLR2FLAG
         RETURN

.
NMLR2GONE MOVE     NMLR2NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

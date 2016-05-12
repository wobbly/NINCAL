..............................................................................
.
. NBRK2IO INCLUSION
.
. FILE NAME : NINBRK2
. REC LENGTH: 754
. INDEX KEY : 1-4 
..............................................................................
.
. ENTRY POINT : NBRK2KEY
. REQUIRED    : 'NBRK2FLD'
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRK2KEY BRANCH    NBRK2FLAG TO NBRK21
         CALL      NBRK2OPEN
         
NBRK21   trap      IOMssg giving Error if IO
         branch    nBRK2lock of nBRK21L,NBRK21R,NBRK21N
.default FIlepi's active
NBRK21L  FILEPI    1;NBRK2FILE
         READ      NBRK2FILE,NBRK2FLD;NBRK2VARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
NBRK21R
         READLK    NBRK2FILE,NBRK2FLD;NBRK2VARS
         trapclr  IO
         RETURN
.No locks
NBRK21N
         READ      NBRK2FILE,NBRK2FLD;NBRK2VARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2TST
. REQUIRED    : NBRK2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NBRK2TST BRANCH    NBRK2FLAG TO NBRK22
         CALL      NBRK2OPEN
NBRK22   trap      IOMssg giving Error if IO
         FILEPI    1;NBRK2FILE
         READ      NBRK2FILE,NBRK2FLD;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2KS
. REQUIRED    :
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL BRKCIAL INSTRUCTION FILE READ
.
NBRK2KS  BRANCH    NBRK2FLAG TO NBRK23
         CALL      NBRK2OPEN
NBRK23   trap      IOMssg giving Error if IO
         BRANCH    NBRK2LOCK OF NBRK23L,NBRK23R,NBRK23N

NBRK23L  FILEPI    1;NBRK2FILE
         READKS    NBRK2FILE;NBRK2VARS
         trapclr   IO
         RETURN
NBRK23R
         READKSLK  NBRK2FILE;NBRK2VARS
         trapclr   IO
         RETURN
NBRK23N
         READKS    NBRK2FILE;NBRK2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2SEQ
. REQUIRED    :
. RETURNED    : BROKER NOTES RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBRK2SEQ BRANCH    NBRK2FLAG TO NBRK24
         CALL      NBRK2OPEN
NBRK24   trap      IOMssg giving Error if IO
         BRANCH    NBRK2LOCK OF NBRK24L,NBRK24R,NBRK24N
NBRK24L  FILEPI    1;NBRK2FILE
         READ      NBRK2FILE,SEQ;NBRK2VARS
         trapclr   IO
         RETURN
NBRK24R
         READLK    NBRK2FILE,SEQ;NBRK2VARS
         trapclr   IO
         RETURN
NBRK24N
         READ      NBRK2FILE,SEQ;NBRK2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2WRT
. REQUIRED    : NBRK2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NBRK2WRT BRANCH    NBRK2FLAG OF NBRK25
         CALL      NBRK2OPEN
NBRK25   FILEPI    1;NBRK2FILE
         WRITE     NBRK2FILE,NBRK2FLD;NBRK2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2UPD
. REQUIRED    : PREVIOUS READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI UPDATE
.
NBRK2UPD BRANCH    NBRK2FLAG OF NBRK26
         CALL      NBRK2OPEN
NBRK26   FILEPI    1;NBRK2FILE
         UPDATE    NBRK2FILE;NBRK2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2DEL
. REQUIRED    : NBRK2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NBRK2DEL BRANCH    NBRK2FLAG TO NBRK27
         CALL      NBRK2OPEn
NBRK27   trap      IOMssg giving Error if IO
         FILEPI    1;NBRK2FILE
         DELETE    NBRK2FILE,NBRK2FLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NBRK2OPEN
. REQUIRED    : 'NBRK2FLAG'
. RETURNED    : 'NBRK2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NBRK2OPEN TRAP     NBRK2GONE IF IO
         BRANCH    NBRK2LOCK TO NBRK2OPNL,NBRK2OPNR

NBRK2OPNL OPEN     NBRK2FILE,NBRK2NAME
         TRAPCLR   IO
         MOVE      C1 TO NBRK2FLAG
         RETURN
NBRK2OPNR OPEN     NBRK2FILE,NBRK2NAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NBRK2FLAG
         RETURN

.
NBRK2GONE MOVE     NBRK2NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

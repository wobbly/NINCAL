.    Last change:       DMB  14 Aug 2001   8:45  pm     Turning Live
.                       DMB  17 Jul 2001   10:53 pm
. ..............................................................................
.
. NRCHGIO INCLUSION
.
. FILE NAME : EXCHARGE.DAT
. REC LENGTH: 67
. INDEX KEY: 1-6 LR
..............................................................................
.
. ENTRY POINT : NRCHGKEY
. REQUIRED    : 'NRCHGFLD'
. RETURNED    : LR
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NRCHGKEY BRANCH    NRCHGFLAG TO NRCHG1
        CALL      NRCHGOPEN

NRCHG1   trap      IOMssg giving Error if IO
        branch    NRCHGlock of NRCHG1L,NRCHG1R,NRCHG1N
.default FIlepi's active
NRCHG1L  FILEPI    1;EXCHARGE
        READ      EXCHARGE,NRCHGFLD;NRCHGVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
NRCHG1R
         READLK    EXCHARGE,NRCHGFLD;NRCHGVARS
         trapclr  IO
         RETURN
.No locks
NRCHG1N
         READ      EXCHARGE,NRCHGFLD;NRCHGVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NRCHGTST
. REQUIRED    : NRCHGFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NRCHGTST BRANCH    NRCHGFLAG TO NRCHG2
        CALL      NRCHGOPEN
NRCHG2   trap      IOMssg giving Error if IO
        FILEPI    1;EXCHARGE
        READ      EXCHARGE,NRCHGFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGKS
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
NRCHGKS  BRANCH    NRCHGFLAG TO NRCHG3
        CALL      NRCHGOPEN
NRCHG3   trap      IOMssg giving Error if IO
        BRANCH    NRCHGLOCK OF NRCHG3L,NRCHG3R,NRCHG3N

NRCHG3L  FILEPI    1;EXCHARGE
        READKS    EXCHARGE;NRCHGVARS
        trapclr   IO
        RETURN
NRCHG3R
        READKSLK  EXCHARGE;NRCHGVARS
        trapclr   IO
        RETURN
NRCHG3N
        READKS    EXCHARGE;NRCHGVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGSEQ
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SEQUENTIAL CHECK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NRCHGSEQ BRANCH    NRCHGFLAG TO NRCHG4
.        CALL      NRCHGOPEN
.NRCHG4   trap      IOMssg giving Error if IO
.        BRANCH    NRCHGLOCK OF NRCHG4L,NRCHG4R,NRCHG4N
.NRCHG4L  FILEPI    1;NRCHGFLE
.        READ      NRCHGFLE,SEQ;NRCHGVARS
.        trapclr   IO
.        RETURN
.NRCHG4R
.        READLK    NRCHGFLE,SEQ;NRCHGVARS
.        trapclr   IO
.        RETURN
.NRCHG4N
.        READ      NRCHGFLE,SEQ;NRCHGVARS
.        trapclr   IO
.        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGWRT
. REQUIRED    : NRCHGFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NRCHGWRT BRANCH    NRCHGFLAG OF NRCHG5
        CALL      NRCHGOPEN
NRCHG5   trap      IOMssg giving Error if IO
        FILEPI    1;EXCHARGE
        WRITE     EXCHARGE;NRCHGVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
NRCHGUPD  COMPARE   C1 TO NRCHGFLAG
         GOTO      NRCHG6 IF EQUAL
         CALL      NRCHGOPEN
NRCHG6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;EXCHARGE
         UPDATE    EXCHARGE;NRCHGVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NRCHGDEL
. REQUIRED    : NRCHGFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NRCHGDEL BRANCH    NRCHGFLAG TO NRCHG7
        CALL      NRCHGOPEn
NRCHG7   trap      IOMssg giving Error if IO
        FILEPI    1;EXCHARGE
        DELETE    EXCHARGE
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGAIM
. REQUIRED    : NRCHGFLD1,
. RETURNED    : NINRCHG RECORD
. DESCRIPTION : AIM NINRCHG FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NRCHGAIM  BRANCH    NRCHGFLAG TO NRCHG9
.         CALL      NRCHGOPEN
.NRCHG9    TRAP      IOMssg Giving Error if IO
.         Branch    NRCHGLOCK to NRCHG9L,NRCHG9R,NRCHG9N
.NRCHG9L   FILEPI    1;NNRCHGFLE2
.         READ      NNRCHGFLE2,NRCHGFLD1;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
.NRCHG9R
.         READLK    NNRCHGFLE2,NRCHGFLD1;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
.NRCHG9N
.         READ      NNRCHGFLE2,NRCHGFLD1;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NRCHGKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : NINRCHG RECORD
. DESCRIPTION : AIM KEY GENEREIC NINRCHG FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NRCHGKG   BRANCH    NRCHGFLAG TO NRCHG10
.         CALL      NRCHGOPEN
.NRCHG10   TRAP      IOMssg Giving Error if IO
.         Branch    NRCHGlock to NRCHG10L,NRCHG10R,NRCHG10N
.NRCHG10L  FILEPI    1;NNRCHGFLE2
.         READKG    NNRCHGFLE2;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
.NRCHG10R
.         READKGLK    NNRCHGFLE2;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
.NRCHG10N
.         READKG    NNRCHGFLE2;NRCHGVARS
.         TRAPCLR   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NRCHGOPEN
. REQUIRED    : 'NRCHGFLAG'
. RETURNED    : 'NRCHGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN EXCHARGE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NRCHGOPEN
        TRAP      NRCHGGONE IF IO
        BRANCH    NRCHGLOCK TO NRCHGOPNL,NRCHGOPNR

NRCHGOPNL

        OPEN      EXCHARGE,NRCHGNAME
        TRAPCLR   IO
        MOVE      C1 TO NRCHGFLAG
        RETURN
NRCHGOPNR
        OPEN      EXCHARGE,NRCHGNAME,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NRCHGFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : NRCHGOPN2
. REQUIRED    : 'NRCHGFLG2'
. RETURNED    : 'NRCHGFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN CONTROL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
.NRCHGOPN2
.        TRAP      NRCHGGONE IF IO
.        BRANCH    NRCHGLOCK TO NRCHGOPN2L,NRCHGOPN2R

.NRCHGOPN2L
.        OPEN      NRCHGFLE,NRCHGNAME
.        TRAPCLR   IO
.        MOVE      C1 TO NRCHGFLG2
.        RETURN
.NRCHGOPN2R
.        OPEN      NRCHGFLE,NRCHGNAME
.        TRAPCLR   IO
.        MOVE      C1 TO NRCHGFLG2
.        RETURN
.
NRCHGGONE
         MOVE     NRCHGNAME TO FILENAME
         CALL     FILEGONE
.
..............................................................................


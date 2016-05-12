..............................................................................
.
. NCHKIO INCLUSION
.
. FILE NAME : NINCHK
. REC LENGTH: 77
. INDEX KEY : 1-9 CONTROL NUMBER + CHECK NUMBER
. AAMDEX KEY: 1-3 CONTROL NUMBER
.             4-9 CHECK NUMBER
..............................................................................
.
. ENTRY POINT : NCHKKEY
. REQUIRED    : 'NCHKFLD'
. RETURNED    : CHECK RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCHKKEY BRANCH    NCHKFLAG TO NCHK1
        CALL      NCHKOPEN
         
NCHK1   trap      IOMssg giving Error if IO
        branch    nCHKlock of nCHK1L,NCHK1R,NCHK1N
.default FIlepi's active
NCHK1L  FILEPI    1;NCHKFILE
        READ      NCHKFILE,NCHKFLD;NCHKVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
NCHK1R
         READLK    NCHKFILE,NCHKFLD;NCHKVARS
         trapclr  IO
         RETURN
.No locks
NCHK1N
         READ      NCHKFILE,NCHKFLD;NCHKVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCHKTST
. REQUIRED    : NCHKFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCHKTST BRANCH    NCHKFLAG TO NCHK2
        CALL      NCHKOPEN
NCHK2   trap      IOMssg giving Error if IO
        FILEPI    1;NCHKFILE
        READ      NCHKFILE,NCHKFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCHKKS
. REQUIRED    :
. RETURNED    : CHECK RECORD
. DESCRIPTION : KEY SEQUENTIAL CONTROL FILE READ
.
NCHKKS  BRANCH    NCHKFLAG TO NCHK3
        CALL      NCHKOPEN
NCHK3   trap      IOMssg giving Error if IO
        BRANCH    NCHKLOCK OF NCHK3L,NCHK3R,NCHK3N

NCHK3L  FILEPI    1;NCHKFILE
        READKS    NCHKFILE;NCHKVARS
        trapclr   IO
        RETURN
NCHK3R
        READKSLK  NCHKFILE;NCHKVARS
        trapclr   IO
        RETURN
NCHK3N
        READKS    NCHKFILE;NCHKVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCHKSEQ
. REQUIRED    :
. RETURNED    : CHECK RECORD
. DESCRIPTION : SEQUENTIAL CHECK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCHKSEQ BRANCH    NCHKFLAG TO NCHK4
        CALL      NCHKOPEN
NCHK4   trap      IOMssg giving Error if IO
        BRANCH    NCHKLOCK OF NCHK4L,NCHK4R,NCHK4N
NCHK4L  FILEPI    1;NCHKFILE
        READ      NCHKFILE,SEQ;NCHKVARS
        trapclr   IO
        RETURN
NCHK4R
        READLK    NCHKFILE,SEQ;NCHKVARS
        trapclr   IO
        RETURN
NCHK4N
        READ      NCHKFILE,SEQ;NCHKVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCHKWRT
. REQUIRED    : NCHKFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCHKWRT BRANCH    NCHKFLAG OF NCHK5
        CALL      NCHKOPEN
NCHK5   trap      IOMssg giving Error if IO
        FILEPI    1;NCHKLIST
        WRITE     NCHKLIST;NCHKVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCHKUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
NCHKUPD  COMPARE   C1 TO NCHKFLAG
         GOTO      NCHK6 IF EQUAL
         CALL      NCHKOPEN
NCHK6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NCHKLIST
         UPDATE    NCHKLIST;NCHKVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCHKDEL
. REQUIRED    : NCHKFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCHKDEL BRANCH    NCHKFLAG TO NCHK7
        CALL      NCHKOPEn
NCHK7   trap      IOMssg giving Error if IO
        FILEPI    1;NCHKLIST
        DELETE    NCHKLIST
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCHKAIM
. REQUIRED    : NCHKFLD1,NCHKFLD2
. RETURNED    : NINCHK RECORD
. DESCRIPTION : AIM NINCHK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCHKAIM  BRANCH    NCHKFLAG TO NCHK9
         CALL      NCHKOPEN
NCHK9    TRAP      IOMssg Giving Error if IO
         Branch    NCHKLOCK to NCHK9L,NCHK9R,NCHK9N
NCHK9L   FILEPI    1;NCHKFLE2
         READ      NCHKFLE2,NCHKFLD1,NCHKFLD2,NCHKFLD3,NCHKFLD4;NCHKVARS
         TRAPCLR   IO
         RETURN
NCHK9R
         READLK    NCHKFLE2,NCHKFLD1,NCHKFLD2,NCHKFLD3,NCHKFLD4;NCHKVARS
         TRAPCLR   IO
         RETURN
NCHK9N
         READ      NCHKFLE2,NCHKFLD1,NCHKFLD2,NCHKFLD3,NCHKFLD4;NCHKVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCHKKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : NINCHK RECORD
. DESCRIPTION : AIM KEY GENEREIC NINCHK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCHKKG   BRANCH    NCHKFLAG TO NCHK10
         CALL      NCHKOPEN
NCHK10   TRAP      IOMssg Giving Error if IO
         Branch    NCHKlock to NCHK10L,NCHK10R,NCHK10N
NCHK10L  FILEPI    1;NCHKFLE2
         READKG    NCHKFLE2;NCHKVARS
         TRAPCLR   IO
         RETURN
NCHK10R
         READKGLK    NCHKFLE2;NCHKVARS
         TRAPCLR   IO
         RETURN
NCHK10N
         READKG    NCHKFLE2;NCHKVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCHKOPEN
. REQUIRED    : 'NCHKFLAG'
. RETURNED    : 'NCHKFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN CONTROL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCHKOPEN
        TRAP      NCHKGONE IF IO
        BRANCH    NCHKLOCK TO NCHKOPNL,NCHKOPNR

NCHKOPNL
        OPEN      NCHKLIST
        TRAPCLR   IO
        MOVE      C1 TO NCHKFLAG
        RETURN
NCHKOPNR
        OPEN      NCHKLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NCHKFLAG
        RETURN
.
NCHKGONE
        MOVE     NCHKNAME TO FILENAME
        CALL     FILEGONE
.
..............................................................................


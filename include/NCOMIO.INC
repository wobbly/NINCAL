.    Last change:  DMB  11/14/2001
. ..............................................................................
.
. NCOMIO INCLUSION
.
. FILE NAME : NINCOMM.DAT
. REC LENGTH: 32
. INDEX KEY:  1-6 LR
..............................................................................
.
. ENTRY POINT : NCOMKEY
. REQUIRED    : NCOMFLD
. RETURNED    : NCOMVARS
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCOMKEY BRANCH    NCOMFLAG TO NCOM1
        CALL      NCOMOPEN

NCOM1   trap      IOMssg giving Error if IO
        branch    NCOMlock of NCOM1L,NCOM1R,NCOM1N
.Default Filepi's active
NCOM1L  FILEPI    1;NCOMIFILE
        READ      NCOMIFILE,NCOMFLD;NCOMVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
NCOM1R
         READLK    NCOMIFILE,NCOMFLD;NCOMVARS
         trapclr  IO
         RETURN
.No locks
NCOM1N
         READ      NCOMIFILE,NCOMFLD;NCOMVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCOMTST
. REQUIRED    : NCOMFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCOMTST BRANCH    NCOMFLAG TO NCOM2
        CALL      NCOMOPEN
NCOM2   trap      IOMssg giving Error if IO
        FILEPI    1;NCOMIFILE
        READ      NCOMIFILE,NCOMFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMKS
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
NCOMKS  BRANCH    NCOMFLAG TO NCOM3
        CALL      NCOMOPEN
NCOM3   trap      IOMssg giving Error if IO
        BRANCH    NCOMLOCK OF NCOM3L,NCOM3R,NCOM3N

NCOM3L  FILEPI    1;NCOMIFILE
        READKS    NCOMIFILE;NCOMVARS
        trapclr   IO
        RETURN
NCOM3R
        READKSLK  NCOMIFILE;NCOMVARS
        trapclr   IO
        RETURN
NCOM3N
        READKS    NCOMIFILE;NCOMVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMSEQ
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SEQUENTIAL CHECK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCOMSEQ BRANCH    NCOMFLG2 TO NCOM4
        CALL      NCOMOPN2
NCOM4   trap      IOMssg giving Error if IO
        BRANCH    NCOMLOCK OF NCOM4L,NCOM4R,NCOM4N
NCOM4L  FILEPI    1;NCOMFLE
        READ      NCOMFLE,SEQ;NCOMVARS
        trapclr   IO
        RETURN
NCOM4R
        READLK    NCOMFLE,SEQ;NCOMVARS
        trapclr   IO
        RETURN
NCOM4N
        READ      NCOMFLE,SEQ;NCOMVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMWRT
. REQUIRED    : NCOMFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCOMWRT BRANCH    NCOMFLG2 OF NCOM5
.NCOMWRT BRANCH    NCOMFLAG OF NCOM5
        CALL      NCOMOPN2
.        CALL      NCOMOPEN
NCOM5   trap      IOMssg giving Error if IO
        FILEPI    1;NCOMFLE2
        WRITE     NCOMFLE2;NCOMVARS
.        FILEPI    1;NCOMIFILE
.        WRITE     NCOMIFILE;NCOMVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
NCOMUPD  COMPARE   C1 TO NCOMFLG2
         GOTO      NCOM6 IF EQUAL
         CALL      NCOMOPN2
NCOM6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NCOMFLE2
         UPDATE    NCOMFLE2;NCOMVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCOMDEL
. REQUIRED    : NCOMFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCOMDEL BRANCH    NCOMFLG2 TO NCOM7
        CALL      NCOMOPN2
NCOM7   trap      IOMssg giving Error if IO
        FILEPI    1;NCOMFLE2
        DELETE    NCOMFLE2
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMAIM
. REQUIRED    : NCOMFLD1,NCOMFLD2,NCOMFLD3
. RETURNED    : NINCOM RECORD
. DESCRIPTION : AIM NINCOM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCOMAIM  BRANCH    NCOMFLG2 TO NCOM9
         CALL      NCOMOPN2
NCOM9    TRAP      IOMssg Giving Error if IO
         Branch    NCOMLOCK to NCOM9L,NCOM9R,NCOM9N
NCOM9L   FILEPI    1;NCOMFLE2
         READ      NCOMFLE2,NCOMFLD1,NCOMFLD2,NCOMFLD3,NCOMFLD4;NCOMVARS
         TRAPCLR   IO
         RETURN
NCOM9R
         READLK    NCOMFLE2,NCOMFLD1,NCOMFLD2,NCOMFLD3,NCOMFLD4;NCOMVARS
         TRAPCLR   IO
         RETURN
NCOM9N
         READ      NCOMFLE2,NCOMFLD1,NCOMFLD2,NCOMFLD3,NCOMFLD4;NCOMVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCOMKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : NINCOM RECORD
. DESCRIPTION : AIM KEY GENEREIC NINCOM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCOMKG   BRANCH    NCOMFLG2 TO NCOM10
         CALL      NCOMOPN2
NCOM10   TRAP      IOMssg Giving Error if IO
         Branch    NCOMlock to NCOM10L,NCOM10R,NCOM10N
NCOM10L  FILEPI    1;NCOMFLE2
         READKG    NCOMFLE2;NCOMVARS
         TRAPCLR   IO
         RETURN
NCOM10R
         READKGLK    NCOMFLE2;NCOMVARS
         TRAPCLR   IO
         RETURN
NCOM10N
         READKG    NCOMFLE2;NCOMVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCOMOPEN
. REQUIRED    : 'NCOMFLAG'
. RETURNED    : 'NCOMFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN REVENUE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCOMOPEN
        TRAP      NCOMGONE IF IO
        BRANCH    NCOMLOCK TO NCOMOPNL,NCOMOPNR

NCOMOPNL

        OPEN      NCOMIFILE,NCOMNAME
        TRAPCLR   IO
        MOVE      C1 TO NCOMFLAG
        RETURN
NCOMOPNR
        OPEN      NCOMIFILE,NCOMNAME,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NCOMFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : NCOMOPN2
. REQUIRED    : 'NCOMFLG2'
. RETURNED    : 'NCOMFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : 
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCOMOPN2
        TRAP      NCOMGONE IF IO
        BRANCH    NCOMLOCK TO NCOMOPN2L,NCOMOPN2R

NCOMOPN2L
        OPEN      NCOMFLE2,NCOMNAME
        TRAPCLR   IO
        MOVE      C1 TO NCOMFLG2
        RETURN
NCOMOPN2R
        OPEN      NCOMFLE2,NCOMNAME
        TRAPCLR   IO
        MOVE      C1 TO NCOMFLG2
        RETURN

NCOMGONE
         MOVE     NCOMNAME TO FILENAME
         CALL     FILEGONE
.
..............................................................................

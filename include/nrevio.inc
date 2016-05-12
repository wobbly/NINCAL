.    Last change:  DMB  11/14/2001
. ..............................................................................
.
. NREVIO INCLUSION
.
. FILE NAME : REVENUE.DAT
. REC LENGTH: 380
. INDEX KEY:  1-12 TYPE,SRC,CLIENT#,CCYY
..............................................................................
.
. ENTRY POINT : NREVKEY
. REQUIRED    : 'NREVFLD'
. RETURNED    : REVVARS
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREVKEY BRANCH    NREVFLAG TO NREV1
        CALL      NREVOPEN

NREV1   trap      IOMssg giving Error if IO
        branch    NREVlock of NREV1L,NREV1R,NREV1N
.Default Filepi's active
NREV1L  FILEPI    1;NREVIFILE
        READ      NREVIFILE,NREVFLD;NREVVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
NREV1R
         READLK    NREVIFILE,NREVFLD;NREVVARS
         trapclr  IO
         RETURN
.No locks
NREV1N
         READ      NREVIFILE,NREVFLD;NREVVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NREVTST
. REQUIRED    : NREVFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NREVTST BRANCH    NREVFLAG TO NREV2
        CALL      NREVOPEN
NREV2   trap      IOMssg giving Error if IO
        FILEPI    1;NREVIFILE
        READ      NREVIFILE,NREVFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NREVKS
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
NREVKS  BRANCH    NREVFLAG TO NREV3
        CALL      NREVOPEN
NREV3   trap      IOMssg giving Error if IO
        BRANCH    NREVLOCK OF NREV3L,NREV3R,NREV3N

NREV3L  FILEPI    1;NREVIFILE
        READKS    NREVIFILE;NREVVARS
        trapclr   IO
        RETURN
NREV3R
        READKSLK  NREVIFILE;NREVVARS
        trapclr   IO
        RETURN
NREV3N
        READKS    NREVIFILE;NREVVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NREVSEQ
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SEQUENTIAL CHECK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREVSEQ BRANCH    NREVFLG2 TO NREV4
        CALL      NREVOPN2
NREV4   trap      IOMssg giving Error if IO
        BRANCH    NREVLOCK OF NREV4L,NREV4R,NREV4N
NREV4L  FILEPI    1;NREVFLE
        READ      NREVFLE,SEQ;NREVVARS
        trapclr   IO
        RETURN
NREV4R
        READLK    NREVFLE,SEQ;NREVVARS
        trapclr   IO
        RETURN
NREV4N
        READ      NREVFLE,SEQ;NREVVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NREVWRT
. REQUIRED    : NREVFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NREVWRT BRANCH    NREVFLAG OF NREV5
        CALL      NREVOPEN
NREV5   trap      IOMssg giving Error if IO
        FILEPI    1;NREVIFILE
        WRITE     NREVIFILE;NREVVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NREVUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
NREVUPD  COMPARE   C1 TO NREVFLAG
         GOTO      NREV6 IF EQUAL
         CALL      NREVOPEN
NREV6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NREVIFILE
         UPDATE    NREVIFILE;NREVVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NREVDEL
. REQUIRED    : NREVFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NREVDEL BRANCH    NREVFLAG TO NREV7
        CALL      NREVOPEn
NREV7   trap      IOMssg giving Error if IO
        FILEPI    1;NREVIFILE
        DELETE    NREVIFILE
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NREVAIM
. REQUIRED    : NREVFLD1,
. RETURNED    : NINREV RECORD
. DESCRIPTION : AIM NINREV FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NREVAIM  BRANCH    NREVFLAG TO NREV9
.         CALL      NREVOPEN
.NREV9    TRAP      IOMssg Giving Error if IO
.         Branch    NREVLOCK to NREV9L,NREV9R,NREV9N
.NREV9L   FILEPI    1;NNREVFLE2
.         READ      NNREVFLE2,NREVFLD1;NREVVARS
.         TRAPCLR   IO
.         RETURN
.NREV9R
.         READLK    NNREVFLE2,NREVFLD1;NREVVARS
.         TRAPCLR   IO
.         RETURN
.NREV9N
.         READ      NNREVFLE2,NREVFLD1;NREVVARS
.         TRAPCLR   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NREVKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : NINREV RECORD
. DESCRIPTION : AIM KEY GENEREIC NINREV FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NREVKG   BRANCH    NREVFLAG TO NREV10
.         CALL      NREVOPEN
.NREV10   TRAP      IOMssg Giving Error if IO
.         Branch    NREVlock to NREV10L,NREV10R,NREV10N
.NREV10L  FILEPI    1;NNREVFLE2
.         READKG    NNREVFLE2;NREVVARS
.         TRAPCLR   IO
.         RETURN
.NREV10R
.         READKGLK    NNREVFLE2;NREVVARS
.         TRAPCLR   IO
.         RETURN
.NREV10N
.         READKG    NNREVFLE2;NREVVARS
.         TRAPCLR   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NREVOPEN
. REQUIRED    : 'NREVFLAG'
. RETURNED    : 'NREVFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN REVENUE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NREVOPEN
        TRAP      NREVGONE IF IO
        BRANCH    NREVLOCK TO NREVOPNL,NREVOPNR

NREVOPNL

        OPEN      NREVIFILE,NREVNAME
        TRAPCLR   IO
        MOVE      C1 TO NREVFLAG
        RETURN
NREVOPNR
        OPEN      NREVIFILE,NREVNAME,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NREVFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : NREVOPN2
. REQUIRED    : 'NREVFLG2'
. RETURNED    : 'NREVFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN CONTROL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NREVOPN2
        TRAP      NREVGONE IF IO
        BRANCH    NREVLOCK TO NREVOPN2L,NREVOPN2R

NREVOPN2L
        OPEN      NREVFLE,NREVNAME
        TRAPCLR   IO
        MOVE      C1 TO NREVFLG2
        RETURN
NREVOPN2R
        OPEN      NREVFLE,NREVNAME
        TRAPCLR   IO
        MOVE      C1 TO NREVFLG2
        RETURN

NREVGONE
         MOVE     NREVNAME TO FILENAME
         CALL     FILEGONE
.
..............................................................................


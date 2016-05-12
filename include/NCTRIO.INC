..............................................................................
.
. NCTRIO INCLUSION
.
. FILE NAME : CONTROLS
. REC LENGTH: 42
. INDEX KEY : 1-11 
..............................................................................
.
. ENTRY POINT : NCTRKEY
. REQUIRED    : 'NCTRFLD'
. RETURNED    : CONTROL RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRKEY BRANCH    NCTRFLAG TO NCTR1
        CALL      NCTROPEN
         
NCTR1   trap      IOMssg giving Error if IO
        branch    nCTRlock of nCTR1L,NCTR1R,NCTR1N
.default FIlepi's active
NCTR1L  FILEPI    1;NCTRFILE
        READ      NCTRFILE,NCTRFLD;NCTRVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
NCTR1R
         READLK    NCTRFILE,NCTRFLD;NCTRVARS
         trapclr  IO
         RETURN
.No locks
NCTR1N
         READ      NCTRFILE,NCTRFLD;NCTRVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTRTST
. REQUIRED    : NCTRFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCTRTST BRANCH    NCTRFLAG TO NCTR2
        CALL      NCTROPEN
NCTR2   trap      IOMssg giving Error if IO
        FILEPI    1;NCTRFILE
        READ      NCTRFILE,NCTRFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCTRKS
. REQUIRED    :
. RETURNED    : CONTROL RECORD
. DESCRIPTION : KEY SEQUENTIAL CONTROL FILE READ
.
NCTRKS  BRANCH    NCTRFLAG TO NCTR3
        CALL      NCTROPEN
NCTR3   trap      IOMssg giving Error if IO
        BRANCH    NCTRLOCK OF NCTR3L,NCTR3R,NCTR3N

NCTR3L  FILEPI    1;NCTRFILE
        READKS    NCTRFILE;NCTRVARS
        trapclr   IO
        RETURN
NCTR3R
        READKSLK  NCTRFILE;NCTRVARS
        trapclr   IO
        RETURN
NCTR3N
        READKS    NCTRFILE;NCTRVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCTRSEQ
. REQUIRED    :
. RETURNED    : CONTROL RECORD
. DESCRIPTION : SEQUENTIAL CONTROL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRSEQ BRANCH    NCTRFLAG TO NCTR4
        CALL      NCTROPEN
NCTR4   trap      IOMssg giving Error if IO
        BRANCH    NCTRLOCK OF NCTR4L,NCTR4R,NCTR4N
NCTR4L  FILEPI    1;NCTRFILE
        READ      NCTRFILE,SEQ;NCTRVARS
        trapclr   IO
        RETURN
NCTR4R
        READLK    NCTRFILE,SEQ;NCTRVARS
        trapclr   IO
        RETURN
NCTR4N
        READ      NCTRFILE,SEQ;NCTRVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCTRWRT
. REQUIRED    : NCTRFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCTRWRT BRANCH    NCTRFLAG OF NCTR5
        CALL      NCTROPEN
NCTR5   trap      IOMssg giving Error if IO
        FILEPI    1;NCTRLIST
        WRITE     NCTRLIST;NCTRVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCTRUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
NCTRUPD  COMPARE   C1 TO NCTRFLAG
         GOTO      NCTR6 IF EQUAL
         CALL      NCTROPEN
NCTR6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NCTRLIST
         UPDATE    NCTRLIST;NCTRVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTRDEL
. REQUIRED    : NCTRFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCTRDEL BRANCH    NCTRFLAG TO NCTR7
        CALL      NCTROPEn
NCTR7   trap      IOMssg giving Error if IO
        FILEPI    1;NCTRLIST
        DELETE    NCTRLIST
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : NCTRAIM
. REQUIRED    : NCTRFLD1
. RETURNED    : CONTROLS RECORD
. DESCRIPTION : AIM CONTROLS FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRAIM  BRANCH    NCTRFLAG TO NCTR9
         CALL      NCTROPEN
NCTR9    TRAP      IOMssg Giving Error if IO
         Branch    NCTRLOCK to NCTR9L,NCTR9R,NCTR9N
NCTR9L   FILEPI    1;NCTRFLE2
         READ      NCTRFLE2,NCTRFLD1;NCTRVARS
         TRAPCLR   IO
         RETURN
NCTR9R
         READLK    NCTRFLE2,NCTRFLD1;NCTRVARS
         TRAPCLR   IO
         RETURN
NCTR9N
         READ      NCTRFLE2,NCTRFLD1;NCTRVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTRKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : CONTROLS RECORD
. DESCRIPTION : AIM KEY GENEREIC CONTROLS FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRKG   BRANCH    NCTRFLAG TO NCTR10
         CALL      NCTROPEN
NCTR10   TRAP      IOMssg Giving Error if IO
         Branch    NCTRlock to NCTR10L,NCTR10R,NCTR10N
NCTR10L  FILEPI    1;NCTRFLE2
         READKG    NCTRFLE2;NCTRVARS
         TRAPCLR   IO
         RETURN
NCTR10R
         READKGLK    NCTRFLE2;NCTRVARS
         TRAPCLR   IO
         RETURN
NCTR10N
         READKG    NCTRFLE2;NCTRVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTRAIMA
. REQUIRED    : NCTRFLD1
. RETURNED    :
. DESCRIPTION : AIM CONTROLS FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRAIMA BRANCH    NCTRFLAG TO NCTR11
         CALL      NCTROPEN
NCTR11   TRAP      IOMssg Giving Error if IO
         Branch    NCTRLOCK to NCTR11L,NCTR11R,NCTR11N
NCTR11L  FILEPI    1;NCTRFLE2
         READ      NCTRFLE2,NCTRFLD1;*4,str8
         TRAPCLR   IO
         RETURN
NCTR11R
         READLK    NCTRFLE2,NCTRFLD1;*4,str8
         TRAPCLR   IO
         RETURN
NCTR11N
         READ      NCTRFLE2,NCTRFLD1;*4,str8
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTRKGA
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : CONTROL DATE LOADED INTO STR8
. DESCRIPTION : AIM KEY GENEREIC CONTROLS FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCTRKGA  BRANCH    NCTRFLAG TO NCTR12
         CALL      NCTROPEN
NCTR12   TRAP      IOMssg Giving Error if IO
         Branch    NCTRlock to NCTR12L,NCTR12R,NCTR12N
NCTR12L  FILEPI    1;NCTRFLE2
         READKG    NCTRFLE2;*4,str8
         TRAPCLR   IO
         RETURN
NCTR12R
         READKGLK  NCTRFLE2;*4,str8
         TRAPCLR   IO
         RETURN
NCTR12N
         READKG    NCTRFLE2;*4,str8
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCTROPEN
. REQUIRED    : 'NCTRFLAG'
. RETURNED    : 'NCTRFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN CONTROL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCTROPEN
        TRAP      NCTRGONE IF IO
        BRANCH    NCTRLOCK TO NCTROPNL,NCTROPNR

NCTROPNL
        OPEN      NCTRLIST
        TRAPCLR   IO
        MOVE      C1 TO NCTRFLAG
        RETURN
NCTROPNR
        OPEN      NCTRLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NCTRFLAG
        RETURN

.
NCTRGONE
        MOVE     NCTRNAME TO FILENAME
        CALL      FILEGONE
.
..............................................................................


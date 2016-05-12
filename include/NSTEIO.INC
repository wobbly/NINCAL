..............................................................................
.
. NSTEIO INCLUSION
. NIN STATEMENT FILE IO ROUTINES
.
. FILE NAME : NINSTE
. REC LENGTH: 122
. INDEX KEY :
.
..............................................................................
.
. ENTRY POINT : NSTESEQ
. REQUIRED    :
. RETURNED    : STATEMENT RECORD
. DESCRIPTION : SEQUENTIAL READ
.
NSTESEQ  BRANCH    NSTEFLAG TO NSTE3
         CALL      NSTEOPEN
NSTE3    FILEPI    1;NSTEFILE
         READ      NSTEFILE,SEQ;STEvars
         RETURN
..............................................................................
.
. ENTRY POINT : NSTEOPEN
. REQUIRED    : 'NSTEFLAG'
. RETURNED    : 'NSTEFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN STATEMENT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSTEOPEN TRAP      NSTEGONE IF IO
         OPEN      NSTEFILE,NSTENAME
         TRAPCLR   IO
         MOVE      C1 TO NSTEFLAG
         RETURN
NSTEGONE MOVE      NSTENAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

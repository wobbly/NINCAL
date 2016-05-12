..............................................................................
.
. INCLIO INCLUSION
. NIN income report FILE I/O ROUTINES
.
.
..............................................................................
.
. ENTRY POINT : INCLKEY
. REQUIRED    : 'INCLFLD'
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
INCLKEY  BRANCH    INCLFLAG TO INCL1
         CALL      INCLOPEN
INCL1    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;INCLFILE
         READ      INCLFILE,INCLFLD;INCLVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : INCLTST
. REQUIRED    : INCLFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
INCLTST  BRANCH    INCLFLAG TO INCL2
         CALL      INCLOPEN
INCL2    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;INCLFILE
         READ      INCLFILE,INCLFLD;STR1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : INCLKS
. REQUIRED    :
. DESCRIPTION : KEY SEQUENTIAL RETURN-TO FILE READ
.
INCLKS   BRANCH    INCLFLAG TO INCL3
         CALL      INCLOPEN
INCL3    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;INCLFILE
         READKS    INCLFILE;INCLVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : INCLSEQ
. REQUIRED    :
. RETURNED    : RETURN-TO RECORD
. DESCRIPTION : SEQUENTIAL RETURN-TO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
INCLSEQ  BRANCH    INCLFLAG TO INCL4
         CALL      INCLOPEN

INCL4    trap      IOMSSG GIVING ERROR if IO

         FILEPI    1;INCLFILE
         READ      INCLFILE,SEQ;INCLVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : INCLWRT
. REQUIRED    : 'INCLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
INCLWRT  Branch    INCLFLAG to INCL5
         CALL      INCLOPEN
INCL5    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;INCLFILE
         WRITE     INCLFILE,INCLFLD;INCLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : INCLUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RETURN-TO FILE
.
INCLUPD  BRANCH    INCLFLAG TO INCL6
         CALL      INCLOPEN
INCL6    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;INCLFILE
         UPDATE    INCLFILE;INCLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : INCLDEL
. REQUIRED    : 'INCLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
INCLDEL  BRANCH    INCLFLAG TO INCL7
         CALL      INCLOPEN
INCL7    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;INCLFILE
         DELETE    INCLFILE,INCLFLD
         TRAPCLR   IO
         RETURN
...............................................................................
..............................................................................
.
. ENTRY POINT : INCLOPEN
. REQUIRED    : 'INCLFLAG'
. RETURNED    : 'INCLFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
INCLOPEN  TRAP      INCLGONE IF IO
.
INCLOP
         OPEN      INCLFILE,INCLNAME
         TRAPCLR   IO
         MOVE      C1 TO INCLFLAG
         RETURN
..............................................................................

INCLGONE MOVE      INCLNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

..............................................................................
.
. LSTIIO LSTIUSION
.
.INcome by List projections data descriptor
.used for List management income reporting to Owners
..............................................................................
.
. ENTRY POINT : LSTIKEY
. REQUIRED    : 'LSTIFLD'
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
LSTIKEY  BRANCH    LSTIFLAG TO LSTI1
         CALL      LSTIOPEN
LSTI1    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LSTIFILE
         READ      LSTIFILE,LSTIFLD;LSTIVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : LSTITST
. REQUIRED    : LSTIFLD
. DESCRIPTION : TEST KEY
.
LSTITST  BRANCH    LSTIFLAG TO LSTI2
         CALL      LSTIOPEN
LSTI2    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LSTIFILE
         READ      LSTIFILE,LSTIFLD;STR1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LSTIKS
. REQUIRED    :
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
LSTIKS   BRANCH    LSTIFLAG TO LSTI3
         CALL      LSTIOPEN
LSTI3    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LSTIFILE
         READKS    LSTIFILE;LSTIVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LSTISEQ
. REQUIRED    :
. DESCRIPTION : SEQUENTIAL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
LSTISEQ  
         BRANCH    LSTIFLAG TO LSTI4
         CALL      LSTIOPEN

LSTI4    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LSTIFILE
         READ      LSTIFILE,SEQ;LSTIVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : LSTIWRT
. REQUIRED    : 'LSTIFLD'
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
LSTIWRT   Branch   LSTIflag to LSTI5
         CALL      LSTIOPEN
LSTI5    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LSTIFILE
         WRITE     LSTIFILE,LSTIFLD;LSTIVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LSTIUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RETURN-TO FILE
.
LSTIUPD  BRANCH    LSTIFLAG TO LSTI6
         CALL      LSTIOPEN
LSTI6    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LSTIFILE
         UPDATE    LSTIFILE;LSTIVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LSTIDEL
. REQUIRED    : 'LSTIFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
LSTIDEL  BRANCH    LSTIFLAG TO LSTI7
         CALL      LSTIOPEN
LSTI7    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LSTIFILE
         DELETE    LSTIFILE,LSTIFLD
         TRAPCLR   IO
         RETURN
...............................................................................
.
. ENTRY POINT : LSTIOPEN
. REQUIRED    : 'LSTIFLAG'
. RETURNED    : 'LSTIFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
LSTIOPEN 
         TRAP      LSTIGONE IF IO
.
         OPEN      LSTIFILE,LSTINAME
         TRAPCLR   IO
         MOVE      C1 TO LSTIFLAG
         RETURN
.
LSTIGONE MOVE      LSTINAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

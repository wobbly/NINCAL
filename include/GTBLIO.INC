..............................................................................
.
. GTBLIO INCLUSION
. TABLE FILE I/O ROUTINES
.
. FILE NAME : TABLEFLE
. REC LENGTH: 79 FIXED
. INDEX KEY : 1-8 (TABLE CODE)
.
..............................................................................
.
. ENTRY POINT : GTBLKEY
. REQUIRED    : 'GTBLFLD'
. RETURNED    : TABLE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
GTBLKEY  BRANCH    GTBLFLAG TO GTBL1
         CALL      GTBLOPEN
GTBL1    FILEPI    1;GTBLFILE
         READ      GTBLFILE,GTBLFLD;TBLCODE:      001-008   TABLE CODE
                                    TBLTEXT       009-079   TABLE TEXT
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLTST
. REQUIRED    : GTBLFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
GTBLTST  BRANCH    GTBLFLAG TO GTBL2
         CALL      GTBLOPEN
GTBL2    FILEPI    1;GTBLFILE
         READ      GTBLFILE,GTBLFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLKS
. REQUIRED    :
. RETURNED    : ORDER RECORD
. DESCRIPTION : KEY SEQUENTIAL ORDER FILE READ
.
GTBLKS   BRANCH    GTBLFLAG TO GTBL3
         CALL      GTBLOPEN
GTBL3    FILEPI    1;GTBLFILE
         READKS    GTBLFILE;TBLCODE:      001-008   TABLE CODE
                            TBLTEXT       009-079   TABLE TEXT
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLSEQ
. REQUIRED    :
. RETURNED    : ORDER RECORD
. DESCRIPTION : SEQUENTIAL ORDER FILE READ
.
GTBLSEQ  BRANCH    GTBLFLAG TO GTBL4
         CALL      GTBLOPEN
GTBL4    FILEPI    1;GTBLFILE
         READ      GTBLFILE,SEQ;TBLCODE:      001-008   TABLE CODE
                                TBLTEXT       009-079   TABLE TEXT
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLWRT
. REQUIRED    : 'GTBLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
GTBLWRT  BRANCH    GTBLFLAG TO GTBL5
         CALL      GTBLOPEN
GTBL5    FILEPI    1;GTBLFILE
         WRITE     GTBLFILE,GTBLFLD;TBLCODE:      001-008   TABLE CODE
                                    TBLTEXT       009-079   TABLE TEXT
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE TABLE FILE
.
GTBLUPD  BRANCH    GTBLFLAG TO GTBL6
         CALL      GTBLOPEN
GTBL6    FILEPI    1;GTBLFILE
         UPDATE    GTBLFILE;TBLCODE:      001-008   TABLE CODE
                            TBLTEXT       009-079   TABLE TEXT
         RETURN
..............................................................................
.
. ENTRY POINT : GTBLDEL
. REQUIRED    : 'GTBLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
GTBLDEL  BRANCH    GTBLFLAG TO GTBL7
         CALL      GTBLOPEN
GTBL7    FILEPI    1;GTBLFILE
         DELETE    GTBLFILE,GTBLFLD
         RETURN
...............................................................................
.
. ENTRY POINT : GTBLOPEN
. REQUIRED    : 'GTBLFLAG'
. RETURNED    : 'GTBLFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN TABLE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
GTBLOPEN TRAP      GTBLGONE giving error IF IO
         OPEN      GTBLFILE,GTBLNAME
         TRAPCLR   IO
         MOVE      C1 TO GTBLFLAG

         RETURN
.......................................................................
GTBLGONE MOVE      GTBLNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


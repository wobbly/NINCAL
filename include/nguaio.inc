..............................................................................
.
. NGUAIO INCLUSION
. NIN Guaranty FILE I/O ROUTINES
.
.
.  FILE NAME : NINGUAR
. REC LENGTH: 14 FIXED
. INDEX KEY : 1-6
..............................................................................
.
. ENTRY POINT : NGUAKEY
. REQUIRED    : 'NGUAFLD'
. RETURNED    : Guaranty RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NGUAKEY  BRANCH    NGUAFLG1 TO NGUA1
         CALL      NGUAOPEN
NGUA1    FILEPI    1;NGUAFILE
         READ      NGUAFILE,NGUAFLD;GUAvars
         RETURN
..............................................................................
.
. ENTRY POINT : NGUATST
. REQUIRED    : NGUAFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NGUATST  BRANCH    NGUAFLG1 TO NGUA2
         CALL      NGUAOPEN
NGUA2    FILEPI    1;NGUAFILE
         READ      NGUAFILE,NGUAFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NGUAKS
. REQUIRED    :
. RETURNED    : Guaranty RECORD
. DESCRIPTION : KEY SEQUENTIAL Guaranty FILE READ
.
NGUAKS   BRANCH    NGUAFLG1 TO NGUA3
         CALL      NGUAOPEN
NGUA3    FILEPI    1;NGUAFILE
         READKS    NGUAFILE;GUAvars
         RETURN
..............................................................................
. ENTRY POINT : NGUASEQ
. REQUIRED    :
. RETURNED    : GUAR RECORD
. DESCRIPTION : SEQUENTIAL Guaranty FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NGUASEQ
	 BRANCH    NGUAFLG1 TO NGUA4
         CALL      NGUAOPEN
NGUA4    FILEPI    1;NGUAFILE
         READ      NGUAFILE,SEQ;GUAvars
         RETURN
..............................................................................
.
. ENTRY POINT : NGUAWRT
. REQUIRED    : 'NGUAFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NGUAWRT  BRANCH    NGUAFLG1 TO NGUA5
         CALL      NGUAOPEN
NGUA5    FILEPI    1;NGUAFILE
         WRITE     NGUAFILE,NGUAFLD;GUAvars
         RETURN
..............................................................................
.
. ENTRY POINT : NGUAUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE Guaranty FILE
.
NGUAUPD  BRANCH    NGUAFLG1 TO NGUA6
         CALL      NGUAOPEN
NGUA6    FILEPI    1;NGUAFILE
         UPDATE    NGUAFILE;GUAvars
         RETURN
..............................................................................
.
. ENTRY POINT : NGUADEL
. REQUIRED    : 'NGUAFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NGUADEL  BRANCH    NGUAFLG1 TO NGUA7
         CALL      NGUAOPEN
NGUA7    FILEPI    1;NGUAFILE
         DELETE    NGUAFILE,NGUAFLD
         RETURN
...............................................................................
. 
. ENTRY POINT : NGUAOPEN
. REQUIRED    : 'NGUAFLG1' 'NGUAPATH'
. RETURNED    : 'NGUAFLG1' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN Guaranty FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NGUAOPEN
NGUAOPN1 TRAP      NGUAGONE IF IO
         OPEN      NGUAFILE,NGUANAME
         TRAPCLR   IO
         MOVE      C1 TO NGUAFLG1
         RETURN
..............................................................................
NGUAGONE MOVE      NGUANAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


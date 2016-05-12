..............................................................................
.
. GNXTIO INCLUSION
. NIN RECORD NUMBER IO INCLUSION
.
. FILE NAME : NIGNXT
. REC LENGTH: 14 FIXED
. INDEX KEY : 1-8 
.
..............................................................................
.
GNXTKEY  BRANCH    GNXTFLAG OF GNXT1
	 CALL      GNXTOPEN
GNXT1    FILEPI    1;GNXTFILE
	 READ      GNXTFILE,GNXTFLD;GNXTKEY:
                   GNXTNUM
         RETURN
..............................................................................
.
. ENTRY POINT : GNXTTST
. REQUIRED    : GNXTFLD
. RETURNED    : 
. DESCRIPTION : TEST KEY
.
GNXTTST  BRANCH    GNXTFLAG TO GNXT2
         CALL      GNXTOPEN
GNXT2    FILEPI    1;GNXTFILE
         READ      GNXTFILE,GNXTFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : GNXTKS
. REQUIRED    : 
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
GNXTKS   BRANCH    GNXTFLAG TO GNXT3
         CALL      GNXTOPEN
GNXT3    FILEPI    1;GNXTFILE
         READKS    GNXTFILE;GNXTKEY:
                   GNXTNUM
	 RETURN
..............................................................................
.
. ENTRY POINT : GNXTSEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
GNXTSEQ  BRANCH    GNXTFLAG TO GNXT4
         CALL      GNXTOPEN
GNXT4    FILEPI    1;GNXTFILE
         READ      GNXTFILE,SEQ;GNXTKEY:
                   GNXTNUM
	 RETURN
..............................................................................
.
. ENTRY POINT : GNXTWRT
. REQUIRED    : 'GNXTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
GNXTWRT  BRANCH    GNXTFLAG TO GNXT5
         GOTO      GNXT5 IF EQUAL
         CALL      GNXTOPEN
GNXT5    FILEPI    1;GNXTFILE
         WRITE     GNXTFILE,GNXTFLD;GNXTKEY:
                   GNXTNUM
	 RETURN
..............................................................................
.
. ENTRY POINT : GNXTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
GNXTUPD  BRANCH    GNXTFLAG TO GNXT6
         CALL      GNXTOPEN
GNXT6    FILEPI    1;GNXTFILE
         UPDATE    GNXTFILE;GNXTKEY:
                   GNXTNUM
	 RETURN
..............................................................................
.
. ENTRY POINT : GNXTDEL
. REQUIRED    : 'GNXTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
GNXTDEL  BRANCH    GNXTFLAG TO GNXT7
         CALL      GNXTOPEN
GNXT7    FILEPI    1;GNXTFILE
         DELETE    GNXTFILE,GNXTFLD
         RETURN
...............................................................................
.
. ENTRY POINT : GNXTOPEN
. REQUIRED    : 'GNXTFLAG'
. RETURNED    : 'GNXTFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
GNXTOPEN TRAP      GNXTGONE IF IO
         OPEN      GNXTFILE,GNXTNAME
         TRAPCLR   IO
         MOVE      C1 TO GNXTFLAG
         RETURN
.
GNXTGONE MOVE      GNXTNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

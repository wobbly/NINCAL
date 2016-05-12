..............................................................................
.
. NXRFIO INCLUSION
. NIN LIST- MAILER XREF FILE I/O ROUTINES
.
. FILE NAME : LISTMLR
. REC LENGTH: 10 FIXED
. INDEX KEY : 1-6 (6 POSITION LIST#)
. INDEX KEY : 7-10 (4 POSITION MLR#, DUPLICATES)
.
..............................................................................
.
. ENTRY POINT : NXRFKEY
. REQUIRED    : 'NXRFFLD', NXRFPATH
. RETURNED    : LIST- MAILER XREF RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXRFKEY BRANCH     NXRFPATH  TO NXRF1,NXRF1A
NXRF1    BRANCH    NXRFFLAG TO NXRF1B
         CALL      NXRFOPEN
NXRF1B    FILEPI    1;NXRFFILE
         READ      NXRFFILE,NXRFFLD;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
.
NXRF1A   BRANCH    NXRFFLG2 TO NXRF1C
         CALL      NXRFOPEN
NXRF1C    FILEPI    1;NXRFFIL2
         READ      NXRFFIL2,NXRFFLD2;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFTST
. REQUIRED    : NXRFFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NXRFTST  BRANCH    NXRFPATH OF NXRF2,NXRF2A
NXRF2    BRANCH    NXRFFLAG TO NXRF2B
         CALL      NXRFOPEN
NXRF2B    FILEPI    1;NXRFFILE
         READ      NXRFFILE,NXRFFLD;;
         RETURN
.
NXRF2A   BRANCH    NXRFFLG2 TO NXRF2C
         CALL      NXRFOPEN
NXRF2C    FILEPI    1;NXRFFIL2
         READ      NXRFFIL2,NXRFFLD2;;
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFKS
. REQUIRED    :
. RETURNED    : LIST- MAILER XREF RECORD
. DESCRIPTION : KEY SEQUENTIAL LIST- MAILER XREF FILE READ
.
NXRFKS   BRANCH    NXRFPATH OF NXRF3,NXRF3A
NXRF3    BRANCH    NXRFFLAG TO NXRF3B
         CALL      NXRFOPEN
NXRF3B    FILEPI    1;NXRFFILE
         READKS    NXRFFILE;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
.
NXRF3A   BRANCH    NXRFFLG2 TO NXRF3C
         CALL      NXRFOPEN
NXRF3C    FILEPI    1;NXRFFIL2
         READKS    NXRFFIL2;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFSEQ
. REQUIRED    :
. RETURNED    : LIST- MAILER XREF RECORD
. DESCRIPTION : SEQUENTIAL LIST- MAILER XREF FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXRFSEQ  BRANCH    NXRFPATH TO NXRF4,NXRF4A
NXRF4    BRANCH    NXRFFLAG TO NXRF4B
         CALL      NXRFOPEN
NXRF4B   FILEPI    1;NXRFFILE
         READ      NXRFFILE,SEQ;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
.
NXRF4A   BRANCH    NXRFFLG2 TO NXRF4C
         CALL      NXRFOPEN
NXRF4C   FILEPI    1;NXRFFIL2
         READ      NXRFFIL2,SEQ;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFWRT
. REQUIRED    : 'NXRFFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NXRFWRT  BRANCH    NXRFFLAG TO NXRF5
         CALL      NXRFOPEN
.NXRF5    FILEPI    1;NXRFFILE
NXRF5    FILEPI    1;NXRFFlst
.         WRITE     NXRFFILE,NXRFFLD;NXRFLIST:  1-6       LIST NUMBER KEY
.                                NXRFMLR        7-10      MAILER NUMBER
         WRITE     NXRFFLst;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
.         BRANCH    NXRFFLG2 TO NXRF5A
.         MOVE      C2 TO NXRFPATH
.NXRF5A   MOVE      NXRFMLR TO NXRFFLD2
.         CALL      NXRFOPEN
.         FILEPI    1;NXRFFIL2
.         INSERT    NXRFFIL2,NXRFFLD2
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST- MAILER XREF FILE
.
NXRFUPD  BRANCH    NXRFFLAG TO NXRF6
         CALL      NXRFOPEN
NXRF6    FILEPI    1;NXRFFlst
         UPDATE    NXRFFlst;NXRFLIST:  1-6       LIST NUMBER KEY
                                NXRFMLR        7-10      MAILER NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NXRFDEL
. REQUIRED    : 'NXRFFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NXRFDEL  BRANCH    NXRFFLAG TO NXRF7
         CALL      NXRFOPEN
NXRF7    FILEPI    1;NXRFFLst
         DELETE    NXRFFlst
         RETURN
...............................................................................
.
. ENTRY POINT : NXRFOPEN
. REQUIRED    : 'NXRFPATH'
. RETURNED    : 'NXRFFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN LIST- MAILER XREF FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NXRFOPEN
. BRANCH    NXRFPATH TO NXRFOPN1,NXRFOPN2
         MOVE      NXRFNAM1 TO NXRFNAME
         TRAP      NXRFGONE IF IO
         OPEN      NXRFFlst
         TRAPCLR   IO
         MOVE      C1 TO NXRFFLAG
         MOVE      C1 TO NXRFFLG2
         RETURN


NXRFOPN1 MOVE      NXRFNAM1 TO NXRFNAME
         TRAP      NXRFGONE IF IO
         OPEN      NXRFFILE,NXRFNAME
         TRAPCLR   IO
         MOVE      C1 TO NXRFFLAG
         RETURN
.
NXRFOPN2 MOVE      NXRFNAM2 TO NXRFNAME
         TRAP      NXRFGONE IF IO
         OPEN      NXRFFIL2,NXRFNAME
         TRAPCLR   IO
         MOVE      C1 TO NXRFFLG2
         RETURN
.
NXRFGONE MOVE      NXRFNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

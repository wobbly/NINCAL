..............................................................................
.
. NMRGIO INCLUSION
. NIN MERGE FILE I/O ROUTINES
.
. FILE NAME : NINMERGE
. REC LENGTH: 277 COMPRESSED
. INDEX KEY : (1)  1-6       (LR#)
.DLH 08Feb2007  file server and filelist
..............................................................................
.
. ENTRY POINT : NMRGKEY
. REQUIRED    : 'NMRGPATH', 'NMRGFLD'
. RETURNED    : MERGE RECPRD
. DESCRIPTION : EXACT ISAM KEY READ
.
NMRGKEY  BRANCH        NMRGFLAG TO NMRG1
         CALL      NMRGOPEN
NMRG1    FILEPI    1;NMRGFILE
         READ      NMRGFILE,NMRGFLD;NMRGVAR
         RETURN
..............................................................................
.
. ENTRY POINT : NMRGTST
. REQUIRED    : NMRGFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMRGTST  BRANCH        NMRGFLAG TO NMRG2
         CALL      NMRGOPEN
NMRG2    FILEPI    1;NMRGFILE
         READ      NMRGFILE,NMRGFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NMRGKS
. REQUIRED    : 'NMRGPATH'
. RETURNED    : MERGE RENMRG
. DESCRIPTION : KEY SEQUENTIAL MERGE FILE READ
.
NMRGKS   BRANCH    NMRGFLAG TO NMRG3
         CALL      NMRGOPEN
NMRG3    FILEPI    1;NMRGFILE
         READKS    NMRGFILE;NMRGVAR
         RETURN
..............................................................................
.
. ENTRY POINT : NMRGSEQ
. REQUIRED    :
. RETURNED    : MERGE RECORD
. DESCRIPTION : SEQUENTIAL MERGE FILE READ
.
NMRGSEQ  BRANCH    NMRGFLAG TO NMRG4
         CALL      NMRGOPEN
NMRG4    FILEPI    1;NMRGFILE
         READ      NMRGFILE,SEQ;NMRGVAR
         RETURN
..............................................................................
.
. ENTRY POINT : NMRGWRT
. REQUIRED    : 'NMRGFLD', 'NMRGPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., ISI 2/3 INSERTS.
.
NMRGWRT  BRANCH    NMRGFLAG TO NMRG5
         CALL      NMRGOPEN
NMRG5    FILEPI    1;NMRGFILE
         WRITE     NMRGFILE,NMRGFLD;NMRGVAR
         RETURN
.
..............................................................................
.
. ENTRY POINT : NMRGUPD
. REQUIRED    : Previous Valid Read
. RETURNED    :
. DESCRIPTION : Update record
.
NMRGUPD  BRANCH    NMRGFLAG TO NMRG6
         CALL      NMRGOPEN
NMRG6    FILEPI    1;NMRGFILE
         Update     NMRGFILE;NMRGVAR
         RETURN
.
..............................................................................
.
. ENTRY POINT : NMRGDEL
. REQUIRED    : 'NMRGPATH', NMRGDEL
. RETURNED    :
. DESCRIPTION :  Deletes record and KEy
.
NMRGDEL  BRANCH    NMRGFLAG TO NMRG7
.
         CALL      NMRGOPEN
NMRG7    FILEPI    1;NMRGFILE
         DELETE    NMRGFILE,NMrgfld
         RETURN
.
...............................................................................
.
. ENTRY POINT : NMRGOPEN
. REQUIRED    : 'NMRGPATH'
. RETURNED    : 'NMRGFLAG' SET TO THE PATH NUMBER: 'NMRGPATH', IF OPENNED.
. DESCRIPTION : OPEN NIN MERGE FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMRGOPEN
         TRAP      NMRGGONE IF IO
         OPEN      NMRGFILE,NMRGNAME
         TRAPCLR   IO
         MOVE      C1 TO NMRGFLAG
         RETURN
NMRGGONE MOVE      NMRGNAME TO FILENAME
         CALL      FILEGONE
.
...............................................................................


;    Last change:  DH   14 May 1999    9:55 pm
..............................................................................
.
. NMDLIO INCLUSION
. NIN MAILDATE SCHEDULE FILE I/O ROUTINES
.
. FILE NAME : MAILDATE
. REC LENGTH: 26 FIXED
. INDEX KEY : 1-6 (6 POSITION LIST#)
.
..............................................................................
.
. ENTRY POINT : NMDLKEY
. REQUIRED    : 'NMDLFLD'
. RETURNED    : SCHEDULE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMDLKEY  BRANCH    NMDLFLAG TO NMDL1
         CALL      NMDLOPEN
NMDL1    FILEPI    1;NMDLFILE
         READ      NMDLFILE,NMDLFLD;mdlVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NMDLTST
. REQUIRED    : NMDLFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMDLTST  BRANCH    NMDLFLAG TO NMDL2
         CALL      NMDLOPEN
NMDL2    FILEPI    1;NMDLFILE
         READ      NMDLFILE,NMDLFLD;;
         RETURN
..............................................................................
.
. ENTRY POINT : NMDLKS
. REQUIRED    :
. RETURNED    : mdlEDULE RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NMDLKS   BRANCH    NMDLFLAG TO NMDL3
         CALL      NMDLOPEN
NMDL3    FILEPI    1;NMDLFILE
         READKS    NMDLFILE;mdlVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NMDLSEQ
. REQUIRED    :
. RETURNED    : SCHEDULE RECORD
. DESCRIPTION : SEQUENTIAL MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMDLSEQ	 BRANCH    NMDLFLAG TO NMDL4
         CALL      NMDLOPEN
NMDL4    FILEPI    1;NMDLFILE
         READ      NMDLFILE,SEQ;mdlvars

         RETURN
..............................................................................
.
. ENTRY POINT : NMDLWRT
. REQUIRED    : 'NMDLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMDLWRT  BRANCH    NMDLFLAG TO NMDL5
         CALL      NMDLOPEN
NMDL5    FILEPI    1;NMDLFILE
         WRITE     NMDLFILE,NMDLFLD;mdlvars

         RETURN
..............................................................................
.
. ENTRY POINT : NMDLUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SCHEDULE FILE
.
NMDLUPD  BRANCH    NMDLFLAG TO NMDL6
         CALL      NMDLOPEN
NMDL6    FILEPI    1;NMDLFILE
         UPDATE    NMDLFILE;mdlvars

         RETURN
..............................................................................
.
. ENTRY POINT : NMDLDEL
. REQUIRED    : 'NMDLFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMDLDEL  BRANCH    NMDLFLAG TO NMDL7
         CALL      NMDLOPEN
NMDL7    FILEPI    1;NMDLFILE
         DELETE    NMDLFILE,NMDLFLD
         RETURN
...............................................................................
.
. ENTRY POINT : NMDLOPEN
. REQUIRED    : 'NMDLFLAG'
. RETURNED    : 'NMDLFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMDLOPEN TRAP      NMDLGONE IF IO
         OPEN      NMDLFILE,NMDLNAME
         TRAPCLR   IO
         MOVE      C1 TO NMDLFLAG
         RETURN
.
NMDLGONE MOVE      NMDLNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


..............................................................................
.
. NSPIIO INCLUSION
. NIN SPECIAL INSTRUCTION I/O.
.
. FILE NAME : NINSPI
. REC LENGTH: 96 FIXED
. INDEX KEY : 1-2 
..............................................................................
.
. ENTRY POINT : NSPIKEY
. REQUIRED    : 'NSPIFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPIKEY  BRANCH    NSPIFLAG TO NSPI1
         CALL      NSPIOPEN
NSPI1    FILEPI    1;NSPIFILE
         READ      NSPIFILE,NSPIFLD;SPINO:   1-2      SPECIAL INTRUCTION NUMBER
				INST1:       3-49     SPECIAL INSTRUCTION #1
				INST2       50-96     SPECIAL INSTRUCTION #2
         RETURN
..............................................................................
.
. ENTRY POINT : NSPITST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSPITST  BRANCH    NSPIFLAG TO NSPI2
         CALL      NSPIOPEN
NSPI2    FILEPI    1;NSPIFILE
         READ      NSPIFILE,NSPIFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NSPIKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NSPIKS   BRANCH    NSPIFLAG TO NSPI3
         CALL      NSPIOPEN
NSPI3    FILEPI    1;NSPIFILE
         READKS    NSPIFILE;SPINO:   1-2      SPECIAL INTRUCTION NUMBER
				INST1:       3-49     SPECIAL INSTRUCTION #1
				INST2       50-96     SPECIAL INSTRUCTION #2
         RETURN
..............................................................................
.
. ENTRY POINT : NSPISEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPISEQ  BRANCH    NSPIFLAG TO NSPI4
         CALL      NSPIOPEN
NSPI4    FILEPI    1;NSPIFILE
         READ      NSPIFILE,SEQ;SPINO:   1-2      SPECIAL INTRUCTION NUMBER
				INST1:       3-49     SPECIAL INSTRUCTION #1
				INST2       50-96     SPECIAL INSTRUCTION #2
         RETURN
..............................................................................
.
. ENTRY POINT : NSPIWRT
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSPIWRT  BRANCH    NSPIFLAG OF NSPI5
         CALL      NSPIOPEN
NSPI5    FILEPI    1;NSPIFILE
         WRITE     NSPIFILE,NSPIFLD;SPINO:   1-2      SPECIAL INTRUCTION NUMBER
				INST1:       3-49     SPECIAL INSTRUCTION #1
				INST2       50-96     SPECIAL INSTRUCTION #2
         RETURN
..............................................................................
.
. ENTRY POINT : NSPIUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
NSPIUPD  BRANCH    NSPIFLAG TO NSPI6
         CALL      NSPIOPEN
NSPI6    FILEPI    1;NSPIFILE
         UPDATE    NSPIFILE;SPINO:   1-2      SPECIAL INTRUCTION NUMBER
				INST1:       3-49     SPECIAL INSTRUCTION #1
				INST2       50-96     SPECIAL INSTRUCTION #2
         RETURN
..............................................................................
.
. ENTRY POINT : NSPIDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSPIDEL  BRANCH    NSPIFLAG TO NSPI7
         CALL      NSPIOPEN
NSPI7    FILEPI    1;NSPIFILE
         DELETE    NSPIFILE,NSPIFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NSPIOPEN
. REQUIRED    : 'NSPIFLAG'
. RETURNED    : 'NSPIFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSPIOPEN TRAP      NSPIGONE IF IO
         OPEN      NSPIFILE,NSPINAME
         TRAPCLR   IO
         MOVE      C1 TO NSPIFLAG
         RETURN
.
NSPIGONE MOVE      NSPINAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

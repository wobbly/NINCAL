................................................................................
.                                                                             .
.NLOLSIO.INC - IO MODULE FOR List of Lists Sample IDENTIFICATION.           .
..............................................................................
.CREATED 2016 March 23.
...............................................................................
.
. ENTRY POINT : NLOLSKEY
. REQUIRED    : 'NLOLSFLD'
. RETURNED    : List of Lists SAMPLE RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLSKEY  BRANCH    NLOLSFLAG TO NLOLS1
         CALL      NLOLSOPEN
NLOLS1    trap      IOMssg giving Error if IO
         branch    NLOLSLOCK to NLOLS1L,NLOLS1R,NLOLS1N
NLOLS1L   FILEPI    1;NLOLSFILE
         READ      NLOLSFILE,NLOLSFLD;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS1R   READLK    NLOLSFILE,NLOLSFLD;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS1N   READ      NLOLSFILE,NLOLSFLD;NLOLSVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLOLSTST
. REQUIRED    : NLOLSFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NLOLSTST  BRANCH    NLOLSFLAG TO NLOLS2
         CALL      NLOLSOPEN
NLOLS2    trap      IOMssg giving Error if IO
         branch    NLOLSLOCK to NLOLS2L,NLOLS2R,NLOLS2N
NLOLS2L   FILEPI    1;NLOLSFILE
         READ      NLOLSFILE,NLOLSFLD;;
         trapclr   IO
         RETURN
NLOLS2R   READLK    NLOLSFILE,NLOLSFLD;;
         trapclr   IO
         RETURN
NLOLS2N   READ      NLOLSFILE,NLOLSFLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLOLSKS
. REQUIRED    :
. RETURNED    : List of Lists SAMPLE RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NLOLSKS   BRANCH    NLOLSFLAG TO NLOLS3
         CALL      NLOLSOPEN
NLOLS3    trap      IOMssg giving Error if IO
         branch    NLOLSLOCK to NLOLS3L,NLOLS3R,NLOLS3N
NLOLS3L   FILEPI    1;NLOLSFILE
         READKS    NLOLSFILE;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS3R   READKSLK  NLOLSFILE;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS3N   READKS    NLOLSFILE;NLOLSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLSSEQ
. REQUIRED    :
. RETURNED    : List of Lists SAMPLE RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLSSEQ  BRANCH    NLOLSFLAG TO NLOLS4
         CALL      NLOLSOPEN
NLOLS4    trap      IOMssg giving Error if IO
         branch    NLOLSLOCK to NLOLS4L,NLOLS4R,NLOLS4N
NLOLS4L   FILEPI    1;NLOLSFILE
         READ      NLOLSFILE,SEQ;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS4R   READLK    NLOLSFILE,SEQ;NLOLSVARS
         trapclr   IO
         RETURN
NLOLS4N   READ      NLOLSFILE,SEQ;NLOLSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLSWRT
. REQUIRED    : 'NLOLSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NLOLSWRT  BRANCH    NLOLSFLAG TO NLOLS5      C1 TO NLOLSPATH
         CALL      NLOLSOPEN
NLOLS5    trap      IOMssg giving Error if IO
           FILEPI    1;NLOLSFILE
           WRITE     NLOLSFILE;NLOLSVARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NLOLSUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NLOLSUPD  BRANCH     NLOLSFLAG TO NLOLS6
         CALL       NLOLSOPEN
NLOLS6    trap       IOMssg giving Error if IO
         FILEPI     1;NLOLSFILE
         UPDATE     NLOLSFILE;NLOLSVARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLOLSDEL
. REQUIRED    : 'NLOLSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NLOLSDEL  BRANCH    NLOLSFLAG TO NLOLS7
         CALL      NLOLSOPEN
NLOLS7    trap      IOMssg giving Error if IO
         FILEPI    1;NLOLSFILE
         DELETE    NLOLSFILE
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NLOLSOPEN
. REQUIRED    : 'NLOLSFLAG'
. RETURNED    : 'NLOLSFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLOLSOPEN
         TRAP      NLOLSGONE IF IO
         OPEN      NLOLSFILE,NLOLSNAME
         TRAPCLR   IO
         MOVE      C1 TO NLOLSFLAG
         RETURN
.
NLOLSGONE MOVE      NLOLSNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................


................................................................................
.                                                                             .
.NORDSIO.INC - IO MODULE FOR DATABUS PORT/SMPR IDENTIFICATION.           .
..............................................................................
.CREATED 2014 AUGUST 21.
...............................................................................
.
. ENTRY POINT : NORDSKEY
. REQUIRED    : 'NORDSFLD'
. RETURNED    : ORDER SAMPLE RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDSKEY  BRANCH    NORDSFLAG TO NORDS1
         CALL      NORDSOPEN
NORDS1    trap      IOMssg giving Error if IO
         branch    NORDSLOCK to NORDS1L,NORDS1R,NORDS1N
NORDS1L   FILEPI    1;NORDSFILE
         READ      NORDSFILE,NORDSFLD;NORDSVARS
         trapclr   IO
         RETURN
NORDS1R   READLK    NORDSFILE,NORDSFLD;NORDSVARS
         trapclr   IO
         RETURN
NORDS1N   READ      NORDSFILE,NORDSFLD;NORDSVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NORDSTST
. REQUIRED    : NORDSFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NORDSTST  BRANCH    NORDSFLAG TO NORDS2
         CALL      NORDSOPEN
NORDS2    trap      IOMssg giving Error if IO
         branch    NORDSLOCK to NORDS2L,NORDS2R,NORDS2N
NORDS2L   FILEPI    1;NORDSFILE
         READ      NORDSFILE,NORDSFLD;;
         trapclr   IO
         RETURN
NORDS2R   READLK    NORDSFILE,NORDSFLD;;
         trapclr   IO
         RETURN
NORDS2N   READ      NORDSFILE,NORDSFLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NORDSKS
. REQUIRED    :
. RETURNED    : ORDER SAMPLE RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NORDSKS   BRANCH    NORDSFLAG TO NORDS3
         CALL      NORDSOPEN
NORDS3    trap      IOMssg giving Error if IO
         branch    NORDSLOCK to NORDS3L,NORDS3R,NORDS3N
NORDS3L   FILEPI    1;NORDSFILE
         READKS    NORDSFILE;NORDSVARS
         trapclr   IO
         RETURN
NORDS3R   READKSLK  NORDSFILE;NORDSVARS
         trapclr   IO
         RETURN
NORDS3N   READKS    NORDSFILE;NORDSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDSSEQ
. REQUIRED    :
. RETURNED    : ORDER SAMPLE RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDSSEQ  BRANCH    NORDSFLAG TO NORDS4
         CALL      NORDSOPEN
NORDS4    trap      IOMssg giving Error if IO
         branch    NORDSLOCK to NORDS4L,NORDS4R,NORDS4N
NORDS4L   FILEPI    1;NORDSFILE
         READ      NORDSFILE,SEQ;NORDSVARS
         trapclr   IO
         RETURN
NORDS4R   READLK    NORDSFILE,SEQ;NORDSVARS
         trapclr   IO
         RETURN
NORDS4N   READ      NORDSFILE,SEQ;NORDSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDSWRT
. REQUIRED    : 'NORDSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NORDSWRT  BRANCH    NORDSFLAG TO NORDS5      C1 TO NORDSPATH
         CALL      NORDSOPEN
NORDS5    trap      IOMssg giving Error if IO
	FILEPI    1;NORDSFILE
	WRITE     NORDSFILE;NORDSVARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NORDSUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NORDSUPD  BRANCH     NORDSFLAG TO NORDS6
         CALL       NORDSOPEN
NORDS6    trap       IOMssg giving Error if IO
         FILEPI     1;NORDSFILE
         UPDATE     NORDSFILE;NORDSVARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NORDSDEL
. REQUIRED    : 'NORDSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NORDSDEL  BRANCH    NORDSFLAG TO NORDS7
         CALL      NORDSOPEN
NORDS7    trap      IOMssg giving Error if IO
         FILEPI    1;NORDSFILE
         DELETE    NORDSFILE
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NORDSOPEN
. REQUIRED    : 'NORDSFLAG'
. RETURNED    : 'NORDSFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NORDSOPEN
         TRAP      NORDSGONE IF IO
         OPEN      NORDSFILE,NORDSNAME
         TRAPCLR   IO
         MOVE      C1 TO NORDSFLAG
         RETURN
.
NORDSGONE MOVE      NORDSNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................


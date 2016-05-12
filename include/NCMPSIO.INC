................................................................................
.                                                                             .
.NCMPSIO.INC - IO MODULE FOR Campaign Sample IDENTIFICATION.           .
..............................................................................
.CREATED 2016 March 23.
...............................................................................
.
. ENTRY POINT : NCMPSKEY
. REQUIRED    : 'NCMPSFLD'
. RETURNED    : Campaign SAMPLE RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCMPSKEY  BRANCH    NCMPSFLAG TO NCMPS1
         CALL      NCMPSOPEN
NCMPS1    trap      IOMssg giving Error if IO
         branch    NCMPSLOCK to NCMPS1L,NCMPS1R,NCMPS1N
NCMPS1L   FILEPI    1;NCMPSFILE
         READ      NCMPSFILE,NCMPSFLD;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS1R   READLK    NCMPSFILE,NCMPSFLD;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS1N   READ      NCMPSFILE,NCMPSFLD;NCMPSVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NCMPSTST
. REQUIRED    : NCMPSFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCMPSTST  BRANCH    NCMPSFLAG TO NCMPS2
         CALL      NCMPSOPEN
NCMPS2    trap      IOMssg giving Error if IO
         branch    NCMPSLOCK to NCMPS2L,NCMPS2R,NCMPS2N
NCMPS2L   FILEPI    1;NCMPSFILE
         READ      NCMPSFILE,NCMPSFLD;;
         trapclr   IO
         RETURN
NCMPS2R   READLK    NCMPSFILE,NCMPSFLD;;
         trapclr   IO
         RETURN
NCMPS2N   READ      NCMPSFILE,NCMPSFLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NCMPSKS
. REQUIRED    :
. RETURNED    : Campaign SAMPLE RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NCMPSKS   BRANCH    NCMPSFLAG TO NCMPS3
         CALL      NCMPSOPEN
NCMPS3    trap      IOMssg giving Error if IO
         branch    NCMPSLOCK to NCMPS3L,NCMPS3R,NCMPS3N
NCMPS3L   FILEPI    1;NCMPSFILE
         READKS    NCMPSFILE;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS3R   READKSLK  NCMPSFILE;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS3N   READKS    NCMPSFILE;NCMPSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCMPSSEQ
. REQUIRED    :
. RETURNED    : Campaign SAMPLE RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCMPSSEQ  BRANCH    NCMPSFLAG TO NCMPS4
         CALL      NCMPSOPEN
NCMPS4    trap      IOMssg giving Error if IO
         branch    NCMPSLOCK to NCMPS4L,NCMPS4R,NCMPS4N
NCMPS4L   FILEPI    1;NCMPSFILE
         READ      NCMPSFILE,SEQ;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS4R   READLK    NCMPSFILE,SEQ;NCMPSVARS
         trapclr   IO
         RETURN
NCMPS4N   READ      NCMPSFILE,SEQ;NCMPSVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCMPSWRT
. REQUIRED    : 'NCMPSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCMPSWRT  BRANCH    NCMPSFLAG TO NCMPS5      C1 TO NCMPSPATH
         CALL      NCMPSOPEN
NCMPS5    trap      IOMssg giving Error if IO
           FILEPI    1;NCMPSFILE
           WRITE     NCMPSFILE;NCMPSVARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NCMPSUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NCMPSUPD  BRANCH     NCMPSFLAG TO NCMPS6
         CALL       NCMPSOPEN
NCMPS6    trap       IOMssg giving Error if IO
         FILEPI     1;NCMPSFILE
         UPDATE     NCMPSFILE;NCMPSVARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NCMPSDEL
. REQUIRED    : 'NCMPSFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCMPSDEL  BRANCH    NCMPSFLAG TO NCMPS7
         CALL      NCMPSOPEN
NCMPS7    trap      IOMssg giving Error if IO
         FILEPI    1;NCMPSFILE
         DELETE    NCMPSFILE
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NCMPSOPEN
. REQUIRED    : 'NCMPSFLAG'
. RETURNED    : 'NCMPSFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCMPSOPEN
         TRAP      NCMPSGONE IF IO
         OPEN      NCMPSFILE,NCMPSNAME
         TRAPCLR   IO
         MOVE      C1 TO NCMPSFLAG
         RETURN
.
NCMPSGONE MOVE      NCMPSNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................


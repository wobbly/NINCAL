..............................................................................
.
. NORD2IO INCLUSION
. NIN ORDER FILE  (CREATED FROM THE ORDERPIC PROGRAM) I/O ROUTINES
.
. FILE NAME : ?
. REC LENGTH: 277 VAR
.
..............................................................................
.
. ENTRY POINT : NORD2SEQ
. REQUIRED    :
. RETURNED    : ORDER RECORD
. DESCRIPTION : SEQUENTIAL ORDER FILE READ
.
NORD2SEQ BRANCH    NORDFLAG TO NORD24
         CALL      NORD2opn
NORD24   BRANCH    NORDLock TO NORD24L,NORD24R,NORD24N

NORD24L  FILEPI    1;NORDFILE
         READ      NORDFILE,SEQ;ORD2VARS
         RETURN
Nord24R
         READLK    NORDFILE,SEQ;ORD2VARS
         RETURN
Nord24N
         READ      NORDFILE,SEQ;ORD2VARS
         RETURN
..............................................................................
.
. ENTRY POINT : NORD2WRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SEQUENTIAL WRITE
.
NOR2DWRT BRANCH    NORDFLAG TO NORD25
         CALL      NORD2opn
NORD25   FILEPI    1;NORDFILE
         WRITE     NORDFILE,SEQ;ORD2VARS
         RETURN
..............................................................................
.
. ENTRY POINT : NORD2OPN
. REQUIRED    : 'NORDFLAG'
. RETURNED    : 'NORDFLAG' SET TO '1' IF OPENNED.
. DESCRIPTION : OPEN NIN ORDER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NORD2OPN TRAP      NORD2GON IF IO
         OPEN      NORDFILE,NORDNAME
         TRAPCLR   IO
         MOVE      C1 TO NORDFLAG
         RETURN
NORD2GoN MOVE      NORDNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


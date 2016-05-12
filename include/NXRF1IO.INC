..............................................................................
.
. NXRF1IO INCLUSION
. NIN BRK- MAILER XREF FILE I/O ROUTINES
.
. FILE NAME : BRK_MLR
. REC LENGTH:  FIXED
. INDEX KEY : 1-5 (ID & 4 POSITION BRK#)
. INDEX KEY : 6 9 (4 POSITION MLR#)
.
..............................................................................
.
. ENTRY POINT : NXRF1KEY
. REQUIRED    : 'NXRF1FLD', NXRF1PATH
. RETURNED    : BROKER- MAILER XREF RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXRF1KEY BRANCH    NXRF1FLG TO NXRF11
          CALL      NXRF1OPN
NXRF11    FILEPI    1;NXRF1FLE
         READ      NXRF1FLE,NXRF1FLD;NXRF1ID,NXRF1BRK:  1-4       BROKER NUMBER KEY
                                NXRF1MLR        7-10      MAILER NUMBER
         RETURN
.
..............................................................................
.
. ENTRY POINT : NXRF1TST
. REQUIRED    : NXRF1FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NXRF1TST  BRANCH    NXRF1FLG TO NXRF12
          CALL      NXRF1OPN
NXRF12    FILEPI    1;NXRF1FLE
         READ      NXRF1FLE,NXRF1FLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NXRF1KS
. REQUIRED    :
. RETURNED    : BROKER- MAILER XREF RECORD
. DESCRIPTION : KEY SEQUENTIAL BROKER- MAILER XREF FILE READ
.
NXRF1KS  BRANCH    NXRF1FLG TO NXRF13
         CALL      NXRF1OPN
NXRF13    FILEPI    1;NXRF1FLE
         READKS    NXRF1FLE;NXRF1ID,NXRF1BRK:  1-6       BROKER NUMBER KEY
                                NXRF1MLR        7-10      MAILER NUMBER
         RETURN
.
..............................................................................
.
. ENTRY POINT : NXRF1WRT
. REQUIRED    : 'NXRF1FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NXRF1WRT  BRANCH    NXRF1FLG TO NXRF15
         CALL      NXRF1OPN
NXRF15    FILEPI    1;NXRF1FLE
         WRITE     NXRF1FLE,NXRF1FLD;NXRF1ID,NXRF1BRK:  1-6       BROKER NUMBER KEY
                                NXRF1MLR        7-10      MAILER NUMBER
         RETURN
..............................................................................
.
. ENTRY POINT : NXRF1OPN
. REQUIRED    : 'NXRF1PTH'
. RETURNED    : 'NXRF1FLG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN BROKER- MAILER XREF FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NXRF1OPN MOVE      NXRF1NAM TO NXRF1NME
         TRAP      NXRF1GNE IF IO
         OPEN      NXRF1FLE,NXRF1NME
         TRAPCLR   IO
         MOVE      C1 TO NXRF1FLG
         RETURN
.
NXRF1GNE MOVE      NXRF1NAM TO FILENAME
         CALL      FILEGONE
.
..............................................................................


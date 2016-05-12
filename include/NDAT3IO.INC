..............................................................................
.
. NDAT3IO INCLUSION
. NIN DATACARD TRIPLEX CHARGES BILLING FILE I/O ROUTINES
.
. FILE NAME : NINDAT3
. REC LENGTH: 30 FIXED
. INDEX KEY : (1)  1-6       (LIST#)
.
.Jan 03 2008  add Rate Field
..............................................................................
.
. ENTRY POINT : NDAT3KEY
. REQUIRED    : 'NDATPATH', 'NDAT3FLD'
. RETURNED    : DATACARD 
. DESCRIPTION : EXACT ISAM KEY READ
.
NDAT3KEY  BRANCH   NDAT3FLG TO NDAT31
         CALL      NDAT3OPN
NDAT31    FILEPI    1;NDAT3FLE
         READ      NDAT3FLE,NDAT3FLD;Ndat3Var
.
              RETURN
..............................................................................
.
. ENTRY POINT : NDAT3TST
. REQUIRED    : NDAT3FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NDAT3TST  BRANCH   NDAT3FLG TO NDAT32
         CALL      NDAT3OPN
NDAT32    FILEPI    1;NDAT3FLE
         READ      NDAT3FLE,NDAT3FLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NDAT3KS
. REQUIRED    : 'NDAT3FLG'
. RETURNED    : DATACARD 
. DESCRIPTION : KEY SEQUENTIAL DATACARD FILE READ
.
NDAT3KS   BRANCH   NDAT3FLG OF NDAT33
         CALL      NDAT3OPN
NDAT33    FILEPI    1;NDAT3FLE
         READKS    NDAT3FLE;NDat3Var
.
         RETURN
..............................................................................
.
. ENTRY POINT : NDAT3SEQ
. REQUIRED    :
. RETURNED    : 
. DESCRIPTION : SEQUENTIAL DATACARD FILE READ
.
NDAT3SEQ  BRANCH    NDAT3FLG TO NDAT34
         CALL      NDAT3OPN
NDAT34    FILEPI    1;NDAT3FLE
         READ      NDAT3FLE,SEQ;NDAT3var
         RETURN
..............................................................................
.
. ENTRY POINT : NDAT3WRT
. REQUIRED    : 'NDAT3FLD'. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT.
.
NDAT3WRT  BRANCH    NDAT3FLG TO NDAT35
         CALL      NDAT3OPN
NDAT35    FILEPI    1;NDAT3FLE
         WRITE     NDAT3FLE,NDAT3FLD;NDat3var
.
         RETURN
..............................................................................
.
. ENTRY POINT : NDAT3UPD
. REQUIRED    : 'PREVIOUS VALID READ'
. DESCRIPTION : EXACT ISAM KEY TXT/ISI UPDATE.
.
NDAT3UPD  BRANCH    NDAT3FLG TO NDAT36
         CALL      NDAT3OPN
NDAT36    FILEPI    1;NDAT3FLE
          UPDATE     NDAT3FLE;NDat3Var
.
         RETURN
..............................................................................
.
. ENTRY POINT : NDAT3DEL
. REQUIRED    :  'NDAT3FLD'
. RETURNED    :
. DESCRIPTION : DELETES TXT & ISAM KEYS.
.
NDAT3DEL  BRANCH    NDAT3FLG TO NDAT37
.
         CALL      NDAT3OPN
NDAT37    CALL      NDAT3TST
         RETURN    IF OVER
         FILEPI    1;NDAT3FLE
         DELETE    NDAT3FLE,NDAT3FLD
         RETURN
.
..............................................................................
.
. ENTRY POINT : NDAT3OPN
. REQUIRED    : 'NDAT3FLG'
. RETURNED    : 'NDAT3FLG' SET TO THE '1' IF OPENNED.
. DESCRIPTION : OPEN NIN DATACARD TDMC BILLING FILE.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NDAT3OPN TRAP      NDAT3GNE IF IO
         OPEN      NDAT3FLE,NDAT3NME
         TRAPCLR   IO
           MOVE      C1 TO NDAT3FLG
         RETURN
NDAT3GNE MOVE      NDAT3NME TO FILENAME
         CALL      FILEGONE
.
...............................................................................


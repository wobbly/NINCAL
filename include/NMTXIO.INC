;patch1.0 prepatch for conversion of company file 
* NMTXIO/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER MAILER TAX FILE.
* *****************************************************************************
.
.    FILE:      NINMTAX
.  LENGTH:      33
.COMPRESS:      NONE
.    TYPE:      ISAM,FIXED
.     KEY:      NMTXFLD
...............................................................................
.
NMTXKEY  BRANCH    NMTXFLAG TO NMTX1
         CALL      NMTXOPEN
NMTX1    FILEPI    1;NMTXFILE
         READ      NMTXFILE,NMTXFLD;MTXvars
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRTST
. REQUIRED    : NMTXFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMTXTST  BRANCH    NMTXFLAG TO NMTX2
         CALL      NMTXOPEN
NMTX2    FILEPI    1;NMTXFILE
         READ      NMTXFILE,NMTXFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRWRT
. REQUIRED    : NMTXFLD
. RETURNED    :
. DESCRIPTION : KEY WRITE
.
NMTXWRT  BRANCH    NMTXFLAG TO NMTX5
         CALL      NMTXOPEN
NMTX5    FILEPI    1;NMTXFILE
         WRITE     NMTXFILE,NMTXFLD;MTXvars
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRUPD
. REQUIRED    : PREVIOUS VALID ISAM READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE
.
NMTXUPD  BRANCH    NMTXFLAG TO NMTX6
         CALL      NMTXOPEN
NMTX6    FILEPI    1;NMTXFILE
        UPDATE     NMTXFILE;MTXvars
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRDEL
. REQUIRED    : NMTXFLD
. RETURNED    :
. DESCRIPTION : KEY RECORD DELETE
.
NMTXDEL  BRANCH    NMTXFLAG TO NMTX7
         CALL      NMTXOPEN
NMTX7    FILEPI    1;NMTXFILE
         DELETE     NMTXFILE,NMTXFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NMTXOPEN
. REQUIRED    : 'NMTXFLAG'
. RETURNED    : 'NMTXFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER TAX FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMTXOPEN TRAP      NMTXGONE IF IO
         OPEN      NMTXFILE,NMTXNAME
         TRAPCLR   IO
         MOVE      C1 TO NMTXFLAG
         RETURN
NMTXGONE MOVE      NMTXNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
;patch1.0
..............................................................................
.
. ENTRY POINT : NMTXKS
. REQUIRED    : Previous Key Read Required
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL
.
NMTXKS  BRANCH    NMTXFLAG TO NMTX8
         CALL      NMTXOPEN
NMTX8    FILEPI    1;NMTXFILE
         READKS      NMTXFILE;MTXVARS
         RETURN
..............................................................................
;patch1.0


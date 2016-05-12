*        NTYPIO/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER TYPIST ANALISYS FILE.
* *****************************************************************************
.
.    FILE:      TYPOUT
.  LENGTH:      59
.COMPRESS:      NONE
.    TYPE:      ISAM
.     KEY:      IDNUM,TYPE
...............................................................................
.
. ENTRY POINT : NTYPKEY
. REQUIRED    : 'NTYPFLD'
. RETURNED    : TYPIST DATA RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NTYPKEY  
              BRANCH    NTYPFLAG TO NTYP1
              BRANCH    NTYPFLAG TO NTYP1
              CALL      NTYPOPEN
NTYP1         FILEPI    1;NTYPFILE
              READ            NTYPFILE,NTYPFLD;NTypDet
              RETURN
...............................................................................
.
. ENTRY POINT : NTYPTST
. REQUIRED    : NTYPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NTYPTST  BRANCH    NTYPFLAG TO NTYP2
         CALL      NTYPOPEN
NTYP2    FILEPI    1;NTYPFILE
         READ      NTYPFILE,NTYPFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NTYPKS
. REQUIRED    :
. RETURNED    : TYPIST DATE RECORD
. DESCRIPTION : KEY SEQUENTIAL TYPIST FILE READ
.
NTYPKS
              BRANCH    NTYPFLAG TO NTYP3
              CALL      NTYPOPEN
NTYP3         FILEPI    1;NTYPFILE
              READKS          NTYPFILE;NTypDet
              RETURN
...............................................................................
.
. ENTRY POINT : NTYPWRT
. REQUIRED    : 'NTYPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NTYPWRT
              BRANCH    NTYPFLAG TO NTYP5 
              CALL      NTYPOPEN
NTYP5         FILEPI    1;NTYPFILE
              WRITE      NTYPFILE,NTYPFLD;NTypDet
              RETURN
...............................................................................
.
. ENTRY POINT : NTYPUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE TYPIST FILE
.
NTYPUPD
              BRANCH    NTYPFLAG TO NTYP6
              CALL      NTYPOPEN
NTYP6         FILEPI    1;NTYPFILE
              UPDATE    NTYPFILE;NTypDet
              RETURN
...............................................................................
.
. ENTRY POINT : NTYPDEL
. REQUIRED    : 'NTYPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NTYPDEL  BRANCH    NTYPFLAG TO NTYP7
         CALL      NTYPOPEN
NTYP7    FILEPI    1;NTYPFILE
         DELETE    NTYPFILE,NTYPFLD
         RETURN
...............................................................................
.
. ENTRY POINT : NTYPOPEN
. REQUIRED    : 'NTYPFLAG'
. RETURNED    : 'NTYPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NTYPOPEN TRAP      NTYPGONE IF IO
         OPEN      NTYPFILE,NTYPNAME
         TRAPCLR   IO
         MOVE      C1 TO NTYPFLAG
         RETURN
.
NTYPGONE MOVE      NTYPNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


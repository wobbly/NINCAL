;    Last change:  ML   12 Oct 1998    7:52 am
..............................................................................
.
. NSHPIO INCLUSION
. NIN SHIPMENT FILE I/O ROUTINES
.
. REVISED 24JUL91 TO USE CA'S VARIABLES. 
.
. FILE NAME : NINSHP
. REC LENGTH: 77 FIXED
. INDEX KEY : 1-6 (LR NUMBER)
.
.added
.            07Oct98 DLH shpvars, Tracking number
.            15Jul98 DLH add option file lock, record lock, no lock
..............................................................................
.
. ENTRY POINT : NSHPKEY
. REQUIRED    : 'NSHPFLD'
. RETURNED    : SHIPPING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
NSHPKEY  BRANCH    NSHPFLAG TO NSHP1
         CALL      NSHPOPEN
NSHP1   branch    nshplock to nshp1l,nshp1r,nshp1n
.with file locks
NSHP1L   FILEPI    1;NSHPFILE
         READ      NSHPFILE,NSHPFLD;shpvars
         RETURN
.with record locks . note file open must have had flag set
NSHP1R
         READLK   NSHPFILE,NSHPFLD;shpvars

         RETURN
.no locks
NSHP1N
         READ   NSHPFILE,NSHPFLD;shpvars
         RETURN
..............................................................................
.
. ENTRY POINT : NSHPTST
. REQUIRED    : 'NSHPFLD'
. RETURNED    :
. DESCRIPTION : ISAM KEY TEST READ
.
NSHPTST  BRANCH    NSHPFLAG TO NSHP2
         CALL      NSHPOPEN
NSHP2    branch    nshplock to nshp2l,nshp2r,nshp2n
NSHP2L   FILEPI    1;NSHPFILE
         READ      NSHPFILE,NSHPFLD;STR1
         RETURN
NSHP2R
         READLK      NSHPFILE,NSHPFLD;STR1
         RETURN
NSHP2N
         READ      NSHPFILE,NSHPFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NSHPKS
. REQUIRED    :
. RETURNED    : SHIPPING RECORD
. DESCRIPTION : KEY SEQUENTIAL SHIPPING FILE READ
.
NSHPKS   BRANCH    NSHPFLAG TO NSHP3
         CALL      NSHPOPEN
NSHP3    branch    nshplock to nshp3l,nshp3r,nshp3n

NSHP3L   FILEPI    1;NSHPFILE
         READKS    NSHPFILE;SHPVARS
         RETURN
NSHP3R
         READKSLK  NSHPFILE;SHPVARS
         RETURN
NSHP3N
         READKS    NSHPFILE;SHPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NSHPSEQ
. REQUIRED    :
. RETURNED    : SHIPPING RECORD
. DESCRIPTION : SEQUENTIAL SHIPPING FILE READ
.
NSHPSEQ  BRANCH    NSHPFLAG TO NSHP4
         CALL      NSHPOPEN
NSHP4    branch    nshplock to nshp4l,nshp4r,nshp4n

NSHP4L   FILEPI    1;NSHPFILE
         READ      NSHPFILE,SEQ;SHPVARS
        RETURN
NSHP4R
         READLK      NSHPFILE,SEQ;SHPVARS
        RETURN
NSHP4N
         READ      NSHPFILE,SEQ;SHPVARS
        RETURN
..............................................................................
.
. ENTRY POINT : NSHPWRT
. REQUIRED    : 'NSHPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSHPWRT  BRANCH    NSHPFLAG TO NSHP5
         CALL      NSHPOPEN
NSHP5    branch    nshplock to nshp5l,nshp5r
NSHP5L   FILEPI    1;NSHPFILE
         WRITE     NSHPFILE,NSHPFLD;SHPVARS
         RETURN
NSHP5R
         WRITE     NSHPFILE,NSHPFLD;SHPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NSHPUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST FILE
.
NSHPUPD  BRANCH    NSHPFLAG TO NSHP6
         CALL      NSHPOPEN
NSHP6    branch    nshplock to nshp6l,nshp6r
NSHP6L    FILEPI    1;NSHPFILE
          UPDATE    NSHPFILE;SHPVARS
         RETURN
NSHP6R
          UPDATE    NSHPFILE;SHPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NSHPDEL
. REQUIRED    : 'NSHPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSHPDEL  BRANCH    NSHPFLAG TO NSHP7
         CALL      NSHPOPEN
NSHP7    FILEPI    1;NSHPFILE
         DELETE    NSHPFILE,NSHPFLD
         RETURN
...............................................................................
.
. ENTRY POINT : NSHPOPEN
. REQUIRED    : 'NSHPFLAG'
. RETURNED    : 'NSHPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SHIPPING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSHPOPEN TRAP      NSHPGONE IF IO
         BRANCH    NshpLOCK TO NshpOPENl,NSHPOPENr
NSHPOPENl OPEN      NSHPFILE,NSHPNAME
          TRAPCLR   IO
          MOVE      C1 TO NSHPFLAG
          RETURN
NSHPOPENR OPEN      NSHPFILE,NSHPNAME,LOCKMANUAL,SINGLE
          TRAPCLR   IO
          MOVE      C1 TO NSHPFLAG
          RETURN
NSHPGONE MOVE      NSHPNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


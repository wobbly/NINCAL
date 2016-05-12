**************************************************************************************************************************************************************
* NCRCIO.INC - VARIABLE INCLUDE FOR NINORD3 - CORRECTION/CANCELATION FILE.
*******************************************************************************
* LENGTH:    18
*
* ACCESS:    ISI
* KEY    :   1-6     NCRCKEY
***********************
.
..............................................................................
.
. ENTRY POINT : NCRCKEY
. REQUIRED    : 'NCRCFLD'
. RETURNED    : REVISION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCRCKEY  branch    ncrcpath to ncrc1a,ncrc1c
ncrc1a   bRANCH    NCRCFLAG TO NCRC1b
         CALL      NCRCOPEN
NCRC1b   
.        READ      NCRCFILE,NCRCFLD;NCRCKEY:      1-6       ORDER LR NUMBER.
.                               NCRCCODE:         7-7       STATUS 'R'=REVISED 'C'=CANCELLED.
.                               NCRCMM:           8-9       DATE OF REVISION.
.                               NCRCDD:          10-11      DATE OF REVISION.
.                               NCRCYY:          12-13      DATE OF REVISION.
.                               NCRCTYP          14-16      WHO REVISED.
         BRANCH    NCRCLOCK TO NCRC1BL,NCRC1BR,NCRC1BN
NCRC1BL  FILEPI    1;NCRCFILE
         read      ncrcfile,ncrcfld;ncrcvars
         goto      ncrc1x
NCRC1BR  
         readLK    ncrcfile,ncrcfld;ncrcvars
         goto      ncrc1x
NCRC1BN 
         read      ncrcfile,ncrcfld;ncrcvars
         goto      ncrc1x
.
ncrc1c   bRANCH    NCRCFLG2 TO NCRC1d
         CALL      NCRCOPeN
NCRC1d   BRANCH    NCRCLOCK TO NCRC1DL,NCRC1DR,NCRC1DN

NCRC1DL  FILEPI    1;NCRCFLE2
         read      ncrcfle2,ncrcfld2;ncrcvars
         goto      ncrc1x
NCRC1DR 
         readLK      ncrcfle2,ncrcfld2;ncrcvars
         goto      ncrc1x
NCRC1DN  
         read      ncrcfle2,ncrcfld2;ncrcvars
         goto      ncrc1x

ncrc1x   RETURN

..............................................................................
.
. ENTRY POINT : NCRCTST
. REQUIRED    : NCRCFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCRCTST  BRANCH    NCRCPATH TO NCRC2A,NCRC2C

NCRC2A   BRANCH    NCRCFLAG TO NCRC2B
         CALL      NCRCOPEN
NCRC2B   FILEPI    1;NCRCFILE
         READ      NCRCFILE,NCRCFLD;STR1;
         RETURN
NCRC2C   BRANCH    NCRCFLG2 TO NCRC2C
         CALL      NCRCOPEN
NCRC2D   FILEPI    1;NCRCFLE2
         READ      NCRCFLE2,NCRCFLD2;STR1;
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCKS
. REQUIRED    :
. RETURNED    : REVISION RECORD
. DESCRIPTION : KEY SEQUENTIAL REVISION FILE READ
.
NCRCKS   BRANCH    NCRCPATH TO NCRC3A,NCRC3C

NCRC3A   BRANCH    NCRCFLAG TO NCRC3B
         CALL      NCRCOPEN
NCRC3B   branch    ncrclock to ncrc3bL,ncrc3bR,ncrc3bN
ncrc3bL  FILEPI    1;NCRCFILE
         READKS    NCRCFILE;NCRCVARS
         RETURN
ncrc3bR  
         READKSLK  NCRCFILE;NCRCVARS
         RETURN
ncrc3bN
         READKS    NCRCFILE;NCRCVARS
         RETURN
NCRC3C   BRANCH    NCRCFLG2 TO NCRC3D
         CALL      NCRCOPEN
NCRC3D   branch    ncrclock to ncrc3DL,ncrc3DR,ncrc3DN
ncrc3dL  FILEPI    1;NCRCFLE2
         READKS    NCRCFLE2;NCRCVARS
         RETURN
ncrc3dR 
         READKSLK  NCRCFLE2;NCRCVARS
         RETURN
ncrc3dN
         READKS    NCRCFLE2;NCRCVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCSEQ
. REQUIRED    :
. RETURNED    : REVISION RECORD
. DESCRIPTION : SEQUENTIAL REVISION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCRCSEQ  BRANCH    NCRCPATH TO NCRC4A,NCRC4C
NCRC4A   BRANCH    NCRCFLAG TO NCRC4B
         CALL      NCRCOPEN
NCRC4B   BRANCH    NCRCLOCK TO NCRC4BL,NCRC4BR,NCRC4BN
NCRC4BL  FILEPI    1;NCRCFILE
         READ      NCRCFILE,SEQ;NCRCVARS
         RETURN
NCRC4BR
         READLK      NCRCFILE,SEQ;NCRCVARS
         RETURN
NCRC4BN
         READ      NCRCFILE,SEQ;NCRCVARS
         RETURN
NCRC4C   BRANCH    NCRCFLAG TO NCRC4D
         CALL      NCRCOPEN
NCRC4D   BRANCH    NCRCLOCK TO NCRC4DL,NCRC4DR,NCRC4DN
NCRC4DL  FILEPI    1;NCRCFLE2
         READ      NCRCFLE2,SEQ;NCRCVARS
         RETURN
NCRC4DR
         READLK      NCRCFLE2,SEQ;NCRCVARS
         RETURN
NCRC4DN  
         READ      NCRCFLE2,SEQ;NCRCVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCWRT
. REQUIRED    : NCRCFLD, NCRCFLD2 
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCRCWRT  BRANCH    NCRCFLAG TO NCRC5
         CALL      NCRCOPEN
NCRC5    FILEPI    1;NCRCFILE
         WRITE     NCRCFILE,NCRCFLD;NCRCVARS
         COMPARE   C2 TO NCRCFLG2
         IF        NOT EQUAL
         MOVE      C2 TO NCRCPATH
         CALL      NCRCOPEN
         ENDIF
         FILEPi    1;NCRCFLE2
         INSERT    NCRCFLE2,NCRCFLD2
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE REVISION FILE
.
NCRCUPD  BRANCH    NCRCFLAG TO NCRC6
         CALL      NCRCOPEN
NCRC6    BRANCH    NCRCLOCK TO NCRC6L,NCRC6R
NCRC6L   FILEPI    1;NCRCFILE
         UPDATE    NCRCFILE;NCRCVARS
         RETURN
NCRC6R 
         UPDATE  NCRCFILE;NCRCVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCDEL
. REQUIRED    : 'NCRCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCRCDEL  BRANCH    NCRCFLAG TO NCRC7
         CALL      NCRCOPEN
NCRC7    BRANCH    NCRCLOCK TO NCRC7L,NCRC7R
NCRC7L   FILEPI    1;NCRCFILE
         DELETED   NCRCFILE,NCRCFLD
         RETURN
NCRC7R
         DELETED   NCRCFILE,NCRCFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NCRCOPEN
. REQUIRED    : 'NCRCFLAG'
. RETURNED    : 'NCRCFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN REVISION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCRCOPEN branch    ncrcpath to ncrcopn1,ncrcopn2

ncrcopn1 
         MOVE      NCRCNAME TO FILENAME
         TRAP      NCRCGONE IF IO
         COMPARE   C2 TO NCRCLOCK
         IF        EQUAL              .RECORD LOCKING
         OPEN      NCRCFILE,NCRCNAME,LOCKMANUAL,SINGLE
         ELSE                         
         OPEN      NCRCFILE,NCRCNAME
         ENDIF         
         TRAPCLR   IO
         MOVE      C1 TO NCRCFLAG
         RETURN
ncrcopn2 
         MOVE      NCRCNME2 TO FILENAME
         TRAP      NCRCGONE IF IO
         COMPARE   C2 TO NCRCLOCK
         IF        EQUAL              .RECORD LOCKING
         OPEN      NCRCFLE2,NCRCNME2,LOCKMANUAL,SINGLE
         ELSE
         OPEN      NCRCFLE2,NCRCNME2
         ENDIF
         TRAPCLR   IO
         MOVE      C1 TO NCRCFLG2
         RETURN
.
NCRCGONE 
         CALL      FILEGONE
.
..............................................................................


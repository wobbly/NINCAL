..............................................................................
.
. NWFXio INCLUSION
. NWFX Stats FILE DEFINITION
.
. FILE NAME : NWFXref
. REC LENGTH:  FIXED
. INDEX KEY : 
..............................................................................
.
..............................................................................
.
. ENTRY POINT : NWFXKEY
. REQUIRED    : 'NWFXFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWFXKEY  BRANCH    NWFXFLAG TO NWFX1
         CALL      NWFXOPEN
NWFX1    FILEPI    1;NWFXFILE
         READ      NWFXFILE,NWFXFLD;NWFXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NWFXTST  BRANCH    NWFXFLAG TO NWFX2
         CALL      NWFXOPEN
NWFX2    FILEPI    1;NWFXFILE
         READ      NWFXFILE,NWFXFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NWFXKS   BRANCH    NWFXFLAG TO NWFX3
         CALL      NWFXOPEN
NWFX3    FILEPI    1;NWFXFILE
         READKS    NWFXFILE;NWFXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWFXSEQ  BRANCH    NWFXFLAG TO NWFX4
         CALL      NWFXOPEN
NWFX4    FILEPI    1;NWFXFILE
         READ      NWFXFILE,SEQ;NWFXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NWFXWRT  BRANCH    NWFXFLAG OF NWFX5
         CALL      NWFXOPEN
NWFX5    FILEPI    1;NWFXFILE
         WRITE     NWFXFILE,NWFXFLD;NWFXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
NWFXUPD  BRANCH    NWFXFLAG TO NWFX6
         CALL      NWFXOPEN
NWFX6    FILEPI    1;NWFXFILE
         UPDATE    NWFXFILE;NWFXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NWFXDEL  BRANCH    NWFXFLAG TO NWFX7
         CALL      NWFXOPEN
NWFX7    FILEPI    1;NWFXFILE
         DELETE    NWFXFILE,NWFXFLD
         RETURN
..............................................................................
.
. ENTRY POINT : NWFXOPEN
. REQUIRED    : 'NWFXFLAG'
. RETURNED    : 'NWFXFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NWFXOPEN TRAP      NWFXGONE IF IO
         OPEN      NWFXFILE,NWFXNAME
         TRAPCLR   IO
         MOVE      C1 TO NWFXFLAG
         RETURN
.
NWFXGONE MOVE      NWFXNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
.END OF INCLUDE


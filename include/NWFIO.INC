..............................................................................
.
. NWFio INCLUSION
. NWF Stats FILE DEFINITION
.
. FILE NAME : NWFref
. REC LENGTH:  FIXED
. INDEX KEY : 
..............................................................................
.
..............................................................................
.
. ENTRY POINT : NWFKEY
. REQUIRED    : 'NWFFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWFKEY   branch    nwfpath to nwf1a,nwf1c
nwf1a    BRANCH    NWFFLAG TO NWF1b
         CALL      NWFOPEN
NWF1b   FILEPI    1;NWFFILE
         READ      NWFFILE,NWFFLD;NWFVARS
         RETURN
nwf1c    BRANCH    NWFFLAG2 TO NWF1d
         CALL      NWFOPEN2
NWF1d   FILEPI    1;NWFFLE2
         READ      NWFFLE2,NWFFLD2;NWFVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NWFTST   branch    nwfpath to nwf2a,nwf2b
nwf2a    BRANCH    NWFFLAG TO NWF2b
         CALL      NWFOPEN
NWF2b    FILEPI    1;NWFFILE
         READ      NWFFILE,NWFFLD;STR1
         RETURN
nwf2c    BRANCH    NWFFLAG2 TO NWF2d
         CALL      NWFOPEN2
NWF2d    FILEPI    1;NWFFLE2
         READ      NWFFLE2,NWFFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NWFKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NWFKs    branch    nwfpath to nwf3a,nwf3c
NWF3a    BRANCH    NWFFLAG TO NWF3b
         CALL      NWFOPEN
NWF3b    FILEPI    1;NWFFILE
         READKS    NWFFILE;NWFVARS
         RETURN
NWF3c    BRANCH    NWFFLAG2 TO NWF3d
         CALL      NWFOPEN2
NWF3d    FILEPI    1;NWFFLE2
         READKS    NWFFLE2;NWFVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NWFSEQ  BRANCH    NWFFLAG TO NWF4
         CALL      NWFOPEN
NWF4    FILEPI    1;NWFFILE
         READ      NWFFILE,SEQ;NWFVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NWFWRT  BRANCH    NWFFLAG OF NWF5
         CALL      NWFOPEN
NWF5    FILEPI    1;NWFFILE
         WRITE     NWFFILE,NWFFLD;NWFVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
NWFUPD  BRANCH    NWFFLAG TO NWF6
         CALL      NWFOPEN
NWF6    FILEPI    1;NWFFILE
         UPDATE    NWFFILE;NWFVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NWFDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NWFDEL   BRANCH    NWFpath TO NWF7a,nwf7c
nwf7a    BRANCH    NWFFLAG TO NWF7b
         CALL      NWFOPEN
NWF7b    FILEPI    1;NWFFILE
         DELETE    NWFFILE,NWFFLD
         RETURN
nwf7c    BRANCH    NWFFLAG2 TO NWF7d
         CALL      NWFOPEN2
NWF7d    FILEPI    1;NWFFLE2
         DELETE    NWFFLE2,NWFFLD2
         RETURN
..............................................................................
.
. ENTRY POINT : NWFOPEN
. REQUIRED    : 'NWFFLAG'
. RETURNED    : 'NWFFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NWFOPEN TRAP      NWFGONE IF IO
         OPEN      NWFFILE,NWFNAME
         TRAPCLR   IO
         MOVE      C1 TO NWFFLAG
         RETURN
.
NWFOPEN2 TRAP      NWFGONE IF IO
         OPEN      NWFFLE2,NWFNME2
         TRAPCLR   IO
         MOVE      C1 TO NWFFLAG2
         RETURN
.
NWFGONE MOVE      NWFNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
.END OF INCLUDE


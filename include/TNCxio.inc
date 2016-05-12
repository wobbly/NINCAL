..............................................................................
.
. TNCXio INCLUSION
. TNCX Stats FILE DEFINITION
.
. FILE NAME : TNCXref
. REC LENGTH:  FIXED
. INDEX KEY : 


.need to add second key and package file goodies

..............................................................................
.
..............................................................................
.
. ENTRY POINT : TNCXKEY
. REQUIRED    : 'TNCXFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
TNCXKEY  BRANCH    tncxpath to tncx1a,tncx1b,tncx1c
tncx1a   BRANCH    TNCXFLAG TO TNCX1d
         CALL      TNCXOPEN
TNCX1d   FILEPI    1;TNCXFILE
         READ      TNCXFILE,TNCXFLD;TNCXVARS
         RETURN
tncx1b   BRANCH    TNCXFLG1 TO TNCX1e
         CALL      TNCXOPEN
TNCX1e   FILEPI    1;TNCXFLE1
         READ      TNCXFLE1,TNCXFLD1;TNCXVARS
         RETURN
tncx1c   BRANCH    TNCXFLG2 TO TNCX1f
         CALL      TNCXOPEN
TNCX1f   FILEPI    1;TNCXFLE2
         READ      TNCXFLE2,TNCXFLD2;TNCXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
TNCXTST  BRANCH    TNCXFLAG TO TNCX2
         CALL      TNCXOPEN
TNCX2    FILEPI    1;TNCXFILE
         READ      TNCXFILE,TNCXFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
TNCXKS   BRANCH    TNCXFLAG TO TNCX3
         CALL      TNCXOPEN
TNCX3    FILEPI    1;TNCXFILE
         READKS    TNCXFILE;TNCXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
TNCXSEQ  BRANCH    TNCXFLAG TO TNCX4
         CALL      TNCXOPEN
TNCX4    FILEPI    1;TNCXFILE
         READ      TNCXFILE,SEQ;TNCXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
TNCXWRT  BRANCH    tncxpath of tncx5a,tncx5b,tncx5c
tncx5a   BRANCH    TNCXFLAG OF TNCX5d
         CALL      TNCXOPEN
TNCX5d   FILEPI    1;TNCXFILE
         WRITE     TNCXFILE,TNCXFLD;TNCXVARS
         RETURN
tncx5b   BRANCH    TNCXFLG1 OF TNCX5e
         CALL      TNCXOPEN
TNCX5e   FILEPI    1;TNCXFLE1
         WRITE     TNCXFLE1,TNCXFLD1;TNCXVARS
         RETURN
tncx5c   BRANCH    TNCXFLG2 OF TNCX5f
         CALL      TNCXOPEN
TNCX5f   FILEPI    1;TNCXFLE2
         WRITE     TNCXFLE2,TNCXFLD2;TNCXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
TNCXUPD  BRANCH    TNCXFLAG TO TNCX6
         CALL      TNCXOPEN
TNCX6    FILEPI    1;TNCXFILE
         UPDATE    TNCXFILE;TNCXVARS
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
TNCXDEL  BRANCH    TNCXFLAG TO TNCX7
         CALL      TNCXOPEN
TNCX7    FILEPI    1;TNCXFILE
         DELETE    TNCXFILE,TNCXFLD
         RETURN
..............................................................................
.
. ENTRY POINT : TNCXOPEN
. REQUIRED    : 'TNCXFLAG'
. RETURNED    : 'TNCXFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
TNCXOPEN TRAP      TNCXGONE IF IO
         BRANCH    tncxpath OF tncxopn,tncxopn1,tncxopn2

tncxopn  OPEN      TNCXFILE,TNCXNAME
         TRAPCLR   IO
         MOVE      C1 TO TNCXFLAG
         RETURN
tncxopn1  OPEN      TNCXFLE1,TNCXNME1
         TRAPCLR   IO
         MOVE      C1 TO TNCXFLG1
         RETURN
tncxopn2  OPEN      TNCXFLE2,TNCXNME2
         TRAPCLR   IO
         MOVE      C1 TO TNCXFLG2
         RETURN
.
TNCXGONE MOVE      TNCXNAME TO FILENAME
         CALL      FILEGONE
.
.
..............................................................................
. FILE NAME : TNCpackage
. REC LENGTH:  FIXED
. INDEX KEY :  3
.
..............................................................................
.
. ENTRY POINT : tncpKEY
. REQUIRED    : 'tncpFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
tncpKEY  BRANCH    tncpFLAG TO tncp1
         CALL      tncpOPEN
tncp1    FILEPI    1;tncpFILE
         READ      tncpFILE,tncpFLD;tncpVARS
         RETURN
         RETURN
..............................................................................
.
. ENTRY POINT : tncpTST
. REQUIRED    : MKEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
tncpTST  BRANCH    tncpFLAG TO tncp2
         CALL      tncpOPEN
tncp2    FILEPI    1;tncpFILE
         READ      tncpFILE,tncpFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : tncpKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
tncpKS   BRANCH    tncpFLAG TO tncp3
         CALL      tncpOPEN
tncp3    FILEPI    1;tncpFILE
         READKS    tncpFILE;tncpVARS
         RETURN
..............................................................................
.
. ENTRY POINT : tncpSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
tncpSEQ  BRANCH    tncpFLAG TO tncp4
         CALL      tncpOPEN
tncp4    FILEPI    1;tncpFILE
         READ      tncpFILE,SEQ;tncpVARS
         RETURN
..............................................................................
.
. ENTRY POINT : tncpWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
tncpWRT
         BRANCH    tncpFLAG OF tncp5
         CALL      tncpOPEN
tncp5    FILEPI    1;tncpFILE
         WRITE     tncpFILE,tncpFLD;tncpVARS
         RETURN
         RETURN
..............................................................................
.
. ENTRY POINT : tncpUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
tncpUPD  BRANCH    tncpFLAG TO tncp6
         CALL      tncpOPEN
tncp6    FILEPI    1;tncpFILE
         UPDATE    tncpFILE;tncpVARS
         RETURN
..............................................................................
.
. ENTRY POINT : tncpDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
tncpDEL  BRANCH    tncpFLAG TO tncp7
         CALL      tncpOPEN
tncp7    FILEPI    1;tncpFILE
         DELETE    tncpFILE,tncpFLD
         RETURN
..............................................................................
.
. ENTRY POINT : tncpOPEN
. REQUIRED    : 'tncpFLAG'
. RETURNED    : 'tncpFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
tncpOPEN TRAP      tncpGONE IF IO

         OPEN      tncpFILE,tncpNAME
         TRAPCLR   IO
         MOVE      C1 TO tncpFLAG
         RETURN
.
tncpGONE MOVE      tncpNAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE


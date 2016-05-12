..............................................................................
.
. CMPIO INCLUSION
. NIN PROSPECT FILE I/O ROUTINES
.
.
.  FILE NAME : CMP
. REC LENGTH: 199 FIXED
. INDEX KEY : 1-5
..............................................................................
.
. ENTRY POINT : CMPKEY
. REQUIRED    : 'CMPFLD'
. RETURNED    : COMPANY RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
CMPKEY  BRANCH    CMPFLAG TO CMP1
         CALL      CMPOPEN
CMP1    FILEPI    1;CMPFILE
         READ      CMPFILE,CMPFLD;CMPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : CMPTST
. REQUIRED    : CMPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
CMPTST  BRANCH    CMPFLAG TO CMP2
         CALL      CMPOPEN
CMP2    FILEPI    1;CMPFILE
         READ      CMPFILE,CMPFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : CMPKS
. REQUIRED    :
. RETURNED    : PROSPECT RECORD
. DESCRIPTION : KEY SEQUENTIAL PROSPECT FILE READ
.
CMPKS   BRANCH    CMPFLAG TO CMP3
         CALL      CMPOPEN
CMP3    FILEPI    1;CMPFILE
         READKS    CMPFILE;CMPVARS
         RETURN
..............................................................................
. ENTRY POINT : CMPSEQ
. REQUIRED    :
. RETURNED    : PROSPECT RECORD
. DESCRIPTION : SEQUENTIAL PROSPECT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
CMPSEQ	 BRANCH    CMPFLAG TO CMP4B
         CALL      CMPOPEN
CMP4B   FILEPI    1;CMPFILE
        READ      CMPFILE,SEQ;CMPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : CMPWRT
. REQUIRED    : 'CMPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
CMPWRT  BRANCH    CMPFLAG TO CMP5
         CALL      CMPOPEN
CMP5    FILEPI    1;CMPFILE
         WRITE     CMPFILE,CMPFLD;CMPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : CMPUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PROSPECT FILE
.
CMPUPD  BRANCH    CMPFLAG TO CMP6
         CALL      CMPOPEN
CMP6    FILEPI    1;CMPFILE
         UPDATE    CMPFILE;CMPVARS
         RETURN
..............................................................................
.
. ENTRY POINT : CMPDEL
. REQUIRED    : 'CMPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
CMPDEL  BRANCH    CMPFLAG TO CMP7
         CALL      CMPOPEN
CMP7    FILEPI    1;CMPFILE
         DELETE    CMPFILE,CMPFLD
         RETURN
..............................................................................
. 
. ENTRY POINT : CMPOPEN
. REQUIRED    : 'CMPFLAG' 'CMPPATH'
. RETURNED    : 'CMPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PROSPECT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
CMPOPEN  TRAP      CMPGONE IF IO
         OPEN      CMPFILE,CMPNAME
         TRAPCLR   IO
         MOVE      C1 TO CMPFLAG
         RETURN
..............................................................................
CMPGONE MOVE      CMPNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

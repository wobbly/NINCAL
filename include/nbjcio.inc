..............................................................................
.
. NBJCIO INCLUSION
. NIN NBJCER FILE I/O ROUTINES
.
.
..............................................................................
.
. ENTRY POINT : NBJCKEY
. REQUIRED    : 'NBJCFLD'
. RETURNED    : NBJCER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NBJCKEY  BRANCH    NBJCFLG1 TO NBJC1
         CALL      NBJCOPEN
NBJC1    FILEPI    1;NBJCFLE1
         READ      NBJCFLE1,NBJCFLD;NBJCvars
         RETURN
..............................................................................
.
. ENTRY POINT : NBJCTST
. REQUIRED    : NBJCFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NBJCTST  BRANCH    NBJCFLG1 TO NBJC2
         CALL      NBJCOPEN
NBJC2    FILEPI    1;NBJCFLE1
         READ      NBJCFLE1,NBJCFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NBJCKS
. REQUIRED    :
. RETURNED    : NBJCER RECORD
. DESCRIPTION : KEY SEQUENTIAL NBJCER FILE READ
.
NBJCKS   BRANCH    NBJCFLG1 TO NBJC3
         CALL      NBJCOPEN
NBJC3    FILEPI    1;NBJCFLE1
         READKS    NBJCFLE1;NBJCvars
         RETURN
..............................................................................
. ENTRY POINT : NBJCSEQ
. REQUIRED    :
. RETURNED    : NBJCER RECORD
. DESCRIPTION : SEQUENTIAL NBJCER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NBJCSEQ  BRANCH    NBJCPATH OF NBJC4A,NBJCGONE
NBJC4A     BRANCH    NBJCFLG1 TO NBJC4B
         CALL      NBJCOPEN
NBJC4B   FILEPI    1;NBJCFLE3
         READ      NBJCFLE3,SEQ;NBJCvars
         RETURN
..............................................................................
.
. ENTRY POINT : NBJCWRT
. REQUIRED    : 'NBJCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NBJCWRT  BRANCH    NBJCFLG1 TO NBJC5
         CALL      NBJCOPEN
NBJC5    FILEPI    1;NBJCFLE1
         WRITE     NBJCFList;NBJCvars
         RETURN
..............................................................................
.
. ENTRY POINT : NBJCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE NBJCER FILE
.
NBJCUPD  BRANCH    NBJCFLG1 TO NBJC6
         CALL      NBJCOPEN
NBJC6
         UPDATE    NBJCFList;NBJCvars
         RETURN
..............................................................................
.
. ENTRY POINT : NBJCDEL
. REQUIRED    : 'NBJCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NBJCDEL  BRANCH    NBJCFLG1 TO NBJC7
         CALL      NBJCOPEN
NBJC7    FILEPI    1;NBJCFLE1
         DELETE    NBJCFList
         RETURN
..............................................................................
. 
. ENTRY POINT : NBJCOPEN
. REQUIRED    : 'NBJCFLG1' 'NBJCPATH'
. RETURNED    : 'NBJCFLG1' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN NBJCER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NBJCOPEN 
         TRAP      NBJCGONE giving error IF IO
         OPEN      NBJCFList
         OPEn      NBJCFLE3,NBJCNAME
         TRAPCLR   IO
         MOVE      C1 TO NBJCFLG1
         RETURN
..............................................................................
NBJCGONE MOVE      NBJCNAME TO FILENAME
         CALL      FILEGONE
.

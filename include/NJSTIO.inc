..............................................................................
.
. NJSTIO INCLUSION
. NIN DETAIL ADJUSTMENT FILE I/O ROUTINES
.
. FILE NAME : NADJUST
. REC LENGTH: 90 FIXED
. INDEX KEY : 65-70,83 (INVOICE#, SUB#)
.
.PATCH 1.1 ADDED AAMFILE AND FILELIST I/O 
.
..............................................................................
.
. ENTRY POINT : NJSTKEY
. REQUIRED    : 'NJSTFLD'
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
NJSTKEY  BRANCH    NJSTFLAG TO NJST1
         CALL      NJSTOPEN
NJST1    FILEPI    1;NJSTFILE
         READ      NJSTFILE,NJSTFLD;JSTvars
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTTST
. REQUIRED    : 'NJSTFLD'
. RETURNED    :
. DESCRIPTION : ISAM KEY TEST READ
.
NJSTTST  BRANCH    NJSTFLAG TO NJST2
         CALL      NJSTOPEN
NJST2    FILEPI    1;NJSTFILE
         READ      NJSTFILE,NJSTFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTKS
. REQUIRED    :
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : KEY SEQUENTIAL ADJUSTMENT FILE READ
.
NJSTKS   BRANCH    NJSTFLAG TO NJST3
         CALL      NJSTOPEN
NJST3    FILEPI    1;NJSTFILE
         READKS    NJSTFILE;jstvars
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTSEQ
. REQUIRED    :
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : SEQUENTIAL ADJUSTMENT FILE READ
.
NJSTSEQ  BRANCH    NJSTFLAG TO NJST4,NJST4B
         CALL      NJSTOPSQ
         GOTO      NJST4B
NJST4    FILEPI    1;NJSTFILE
         READ      NJSTFILE,SEQ;JSTvars
         RETURN
NJST4B   FILEPI    1;NJSTFLSQ
         READ      NJSTFLSQ,SEQ;JSTvars
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTWRT
. REQUIRED    : 'NJSTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
.START PATCH 1.1 REPLACED LOGIC
.NJSTWRT  BRANCH    NJSTFLAG TO NJST5
.         CALL      NJSTOPEN
.NJST5    FILEPI    1;NJSTFILE
.         WRITE     NJSTFILE,NJSTFLD;JSTvars
.         RETURN
NJSTWRT  BRANCH    NJSTFLAG TO NJST5
         CALL      NJSTOPEN
NJST5    FILEPI    1;NJSTLIST
         WRITE     NJSTLIST;JSTvars
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : NJSTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST FILE
.
.START PATCH 1.1 REPLACED LOGIC
.NJSTUPD  BRANCH    NJSTFLAG TO NJST6
.         CALL      NJSTOPEN
.NJST6    FILEPI    1;NJSTFILE
.         UPDATE    NJSTFILE;JSTvars
.         RETURN
NJSTUPD  BRANCH    NJSTFLAG TO NJST6
         CALL      NJSTOPEN
NJST6    FILEPI    1;NJSTLIST
         UPDATE    NJSTLIST;JSTvars
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : NJSTDEL
. REQUIRED    : 'NJSTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
.START PATCH 1.1 REPLACED LOGIC
.NJSTDEL  BRANCH    NJSTFLAG TO NJST7
.         CALL      NJSTOPEN
.NJST7    FILEPI    1;NJSTFILE
.         DELETE    NJSTFILE,NJSTFLD
.         RETURN
NJSTDEL  BRANCH    NJSTFLAG TO NJST7
         CALL      NJSTOPEN
NJST7    FILEPI    1;NJSTLIST
         DELETE    NJSTLIST
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : NJSTKP
. REQUIRED    :
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : KEY PRIOR SEQUENTIAL ADJUSTMENT FILE READ
.
NJSTKP   BRANCH    NJSTFLAG TO NJST8
         CALL      NJSTOPEN
NJST8    FILEPI    1;NJSTFILE
         READKP    NJSTFILE;JSTvars
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTAIM
. REQUIRED    : NJSTFLD1
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NJSTAIM  BRANCH    NJSTFLG2 TO NJST9
         CALL      NJSTOPEN
NJST9    TRAP      IOMssg Giving Error if IO
	 FILEPI    1;NJSTFLE2
         READ      NJSTFLE2,NJSTFLD1;JSTVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NJSTKG
. REQUIRED    : PREVIOUS VALID AAM READ
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NJSTKG   BRANCH    NJSTFLG2 TO NJST10
         CALL      NJSTOPEN
NJST10   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;NJSTFLE2
         READKG    NJSTFLE2;JSTVARS
         TRAPCLR   IO
         RETURN

...............................................................................
.
. ENTRY POINT : NJSTOPEN
. REQUIRED    : 'NJSTFLAG','NJSTFLG2'
. RETURNED    : 'NJSTFLAG'/'NJSTFLG2' SET TO '1' IF OPENNED ISI, SET TO '2' IF OPENNED TXT.
. DESCRIPTION : OPEN NIN ADJUSTMENT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
.START PATCH 1.1 REPLACED LOGIC
.NJSTOPEN TRAP      NJSTGONE IF IO
.         OPEN      NJSTFILE,NJSTNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NJSTFLAG
.         RETURN
.NJSTOPSQ TRAP      NJSTGONE IF IO
.         OPEN      NJSTFLSQ,NJSTNAME
.         TRAPCLR   IO
.         MOVE      C2 TO NJSTFLAG
.         RETURN
.NJSTGONE MOVE      NJSTNAME TO FILENAME
.         CALL      FILEGONE
.
.NJSTOPN2 TRAP      NJSTGONE IF IO
.         OPEN      NJSTFLE2,NJSTNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NJSTFLG2
.         RETURN

NJSTOPEN
        TRAP      NJSTGONE giving Error if IO
.        BRANCH    NJSTLOCK TO NJSTOPNL,NJSTOPNR

NJSTOPNL
        OPEN      NJSTLIST
        TRAPCLR   IO
        MOVE      C1 TO NJSTFLAG
        MOVE      C1 TO NJSTFLG2
        RETURN
NJSTOPNR
        OPEN      NJSTLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NJSTFLAG
        MOVE      C1 TO NJSTFLG2
        RETURN
NJSTOPSQ TRAP      NJSTGONE IF IO
.>Patch 1.2
.         OPEN      NJSTFLSQ,NJSTNAME
         OPEN      NJSTFLSQ,NJSTNAMESEQ
.>Patch 1.2         
         TRAPCLR   IO
         MOVE      C2 TO NJSTFLAG
         RETURN
.END PATCH 1.1 REPLACED LOGIC
NJSTGONE MOVE      NJSTNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


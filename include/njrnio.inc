...............................................................................
.
. njrnIO inclusion
. NINCA JOURNAL FILE 
.
. FILE NAME  :  JOURNAL1
. REC LENGTH :  68 (FIXED)
. AIMDEX KEY :  INVOICE NUMBER 1-6  
.
.patch 2.0       use jtnvars
...............................................................................
NJRNSEQ  COMPARE   NJRNPATH TO NJRNFLAG
         GOTO      NJRN4 IF EQUAL
         CALL      NJRNOPEN
NJRN4    trap      IOMssg giving Error if IO
         branch    NJRNLOCK to NJRN4L,NJRN4R,NJRN4N
NJRN4L   FILEPI    1;NJRNFLE2
.begin patch 2.0
.         READ      NJRNFLE2,SEQ;JINVN,JTRANS,JLRNUM,JMLRN,JCOBN,JREASON:
.                   JAMOUNT,JINVN2,NJRNCMNT,JDATE
         READ      NJRNFLE2,SEQ;JTNVARS
.end patch 2.0
         trapclr   IO
         RETURN
NJRN4R   READLK    NJRNFLE2,SEQ;JTNVARS
         trapclr   IO
         RETURN
NJRN4N   READ      NJRNFLE2,SEQ;Jtnvars
         trapclr   IO
         RETURN
...............................................................................
NJRNWRT  BRANCH    NJRNFLAG TO NJRN5
         CALL      NJRNOPEN
NJRN5    FILEPI    1;NJRNFILE
         trap      IOMssg giving Error if IO
         WRITE     NJRNFILE;JTNVARS
         trapclr   IO
         RETURN
..............................................................................
NJRNOPEN BRANCH    NJRNPATH TO NJRNOPN1,NJRNOPN2
NJRNOPN1 TRAP      NJRNGONE IF IO
         OPEN      NJRNFILE,NJRNNAME
         TRAPCLR   IO
         MOVE      C1 TO NJRNFLAG
         RETURN
NJRNOPN2 TRAP      NJRNGONE IF IO
         OPEN      NJRNFLE2,NJRNNAME
         TRAPCLR   IO
         MOVE      C2 TO NJRNFLAG
         RETURN
..............................................................................
NJRNGONE MOVE      NJRNNAME TO FILENAME 
         CALL      FILEGONE
..............................................................................

..............................................................................
.
. NSLSIO INCLUSION
.
. FILE NAME : NINSLS
. REC LENGTH: 163    SPACE
. INDEX KEY : 4
.
..............................................................................
.
. ENTRY POINT : NSLSSEQ
. REQUIRED    :
. RETURNED    : SALES RECORD
. DESCRIPTION : SEQUENTIAL SALES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSLSSEQ  COMPARE    NSLSPATH TO NSLSFLAG
         GOTO      NSLS4 IF EQUAL
         CALL      NSLSOPEN
NSLS4    FILEPI    1;NSLSFILE
         READ      NSLSFILE,SEQ;SLSvars       1-4     MAILER NUMBER.
         RETURN
..............................................................................
.
. ENTRY POINT : NSLSWRT
. REQUIRED    : 
. RETURNED    :
. DESCRIPTION :  SEQ TXT WRITE
.
NSLSWRT  BRANCH    NSLSFLAG TO NSLS5
         CALL      NSLSOPEN
NSLS5    FILEPI    1;NSLSFILE
         WRITE     NSLSFILE,SEQ;SLsvars
         RETURN
.
NSLSKEY COMPARE    NSLSPATH TO NSLSFLAG
        GOTO       NSLS1 IF  EQUAL
         CALL      NSLSOPEN
NSLS1    FILEPI    1;NSLSFLE2
         READ      NSLSFLE2,nslsfld;SLSvars
         RETURN
..............................................................................
NSLSKS   COMPARE   NSLSPATH TO NSLSFLAG
         GOTO      NSLS3 IF EQUAL
         CALL      NSLSOPEN
NSLS3    FILEPI    1;NSLSFLE2
         READKS    NSLSFLE2;SLSvars
         RETURN
..............................................................................
.
. ENTRY POINT : NSLSOPEN
. REQUIRED    : 'NSLSFLAG'
. RETURNED    : 'NSLSFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSLSOPEN TRAP      NSLSGONE IF IO
         BRANCH    NSLSPATH OF NSLSOPN1,NSLSOPN2
NSLSOPN1 OPEN      NSLSFILE,NSLSNAME
         TRAPCLR   IO
         MOVE      C1 TO NSLSFLAG
         RETURN
NSLSOPN2 OPEN      NSLSFLE2,NSLSNAME
         TRAPCLR   IO
         MOVE      C2 TO NSLSFLAG
         RETURN
.
NSLSGONE MOVE      NSLSNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


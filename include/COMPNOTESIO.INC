..............................................................................
.
. COMPNOTEIO INCLUSION
.
. FILE NAME : COMPNOTES
. REC LENGTH: 756
. INDEX KEY : 1-6 
..............................................................................
.
. ENTRY POINT : COMPNOTEKEY
. REQUIRED    : 'COMPNOTEFLD'
. RETURNED    : COMPANY NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
COMPNOTEKEY BRANCH    COMPNOTEFLAG TO COMPNOTE1
         	CALL      COMPNOTEOPEN
         
COMPNOTE1   trap      IOMssg giving Error if IO
         	Branch    COMPNOTElock of COMPNOTE1L,COMPNOTE1R,COMPNOTE1N
.default FIlepi's active
COMPNOTE1L  FILEPI    1;COMPNOTEFILE
         	READ      COMPNOTEFILE,COMPNOTEFLD;COMPNOTEVARS
         	trapclr   IO
         	RETURN
.with record locks . note file open must have had flag set
COMPNOTE1R
         	READLK    COMPNOTEFILE,COMPNOTEFLD;COMPNOTEVARS
         	trapclr  IO
         	RETURN
.No locks
COMPNOTE1N
         	READ      COMPNOTEFILE,COMPNOTEFLD;COMPNOTEVARS
         	trapclr  IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTETST
. REQUIRED    : COMPNOTEFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
COMPNOTETST BRANCH    COMPNOTEFLAG TO COMPNOTE2
         	CALL      COMPNOTEOPEN
COMPNOTE2   trap      IOMssg giving Error if IO
         	FILEPI    1;COMPNOTEFILE
         	READ      COMPNOTEFILE,COMPNOTEFLD;;
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTEKS
. REQUIRED    :
. RETURNED    : COMPANY NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL COMPANY NOTES FILE READ
.
COMPNOTEKS  BRANCH    COMPNOTEFLAG TO COMPNOTE3
         	CALL      COMPNOTEOPEN
COMPNOTE3   trap      IOMssg giving Error if IO
         	BRANCH    COMPNOTELOCK OF COMPNOTE3L,COMPNOTE3R,COMPNOTE3N

COMPNOTE3L  FILEPI    1;COMPNOTEFILE
         	READKS    COMPNOTEFILE;COMPNOTEVARS
         	trapclr   IO
         	RETURN
COMPNOTE3R
         	READKSLK  COMPNOTEFILE;COMPNOTEVARS
         	trapclr   IO
         	RETURN
COMPNOTE3N
         	READKS    COMPNOTEFILE;COMPNOTEVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTESEQ
. REQUIRED    :
. RETURNED    : COMPANY NOTES RECORD
. DESCRIPTION : SEQUENTIAL COMPANY NOTES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
COMPNOTESEQ BRANCH    COMPNOTEFLAG TO COMPNOTE4
         	CALL      COMPNOTEOPEN
COMPNOTE4   trap      IOMssg giving Error if IO
         	BRANCH    COMPNOTELOCK OF COMPNOTE4L,COMPNOTE4R,COMPNOTE4N
COMPNOTE4L  FILEPI    1;COMPNOTEFILE
         	READ      COMPNOTEFILE,SEQ;COMPNOTEVARS
         	trapclr   IO
         	RETURN
COMPNOTE4R
         	READLK    COMPNOTEFILE,SEQ;COMPNOTEVARS
         	trapclr   IO
         	RETURN
COMPNOTE4N
         	READ      COMPNOTEFILE,SEQ;COMPNOTEVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTEWRT
. REQUIRED    : COMPNOTEFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
COMPNOTEWRT BRANCH    COMPNOTEFLAG OF COMPNOTE5
         	CALL      COMPNOTEOPEN
COMPNOTE5   FILEPI    1;COMPNOTEFILE
         	WRITE     COMPNOTEFILE,COMPNOTEFLD;COMPNOTEVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTEUPD
. REQUIRED    : PREVIOUS READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI UPDATE
.
COMPNOTEUPD BRANCH    COMPNOTEFLAG OF COMPNOTE6
         	CALL      COMPNOTEOPEN
COMPNOTE6   FILEPI    1;COMPNOTEFILE
         	UPDATE    COMPNOTEFILE;COMPNOTEVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTEDEL
. REQUIRED    : COMPNOTEFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
COMPNOTEDEL BRANCH    COMPNOTEFLAG TO COMPNOTE7
         	CALL      COMPNOTEOPEN
COMPNOTE7   trap      IOMssg giving Error if IO
         	FILEPI    1;COMPNOTEFILE
         	DELETE    COMPNOTEFILE,COMPNOTEFLD
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : COMPNOTEOPEN
. REQUIRED    : 'COMPNOTEFLAG'
. RETURNED    : 'COMPNOTEFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
COMPNOTEOPEN TRAP     COMPNOTEGONE IF IO
         	BRANCH    COMPNOTELOCK TO COMPNOTEOPNL,COMPNOTEOPNR

COMPNOTEOPNL OPEN     COMPNOTEFILE,COMPNOTENAME
         	TRAPCLR   IO
         	MOVE      C1 TO COMPNOTEFLAG
         	RETURN
COMPNOTEOPNR OPEN     COMPNOTEFILE,COMPNOTENAME,LOCKMANUAL,SINGLE
         	TRAPCLR   IO
         	MOVE      C1 TO COMPNOTEFLAG
         	RETURN

.
COMPNOTEGONE MOVE     COMPNOTENAME TO FILENAME
         	CALL      FILEGONE
.
..............................................................................

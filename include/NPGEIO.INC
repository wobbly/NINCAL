..............................................................................
.
. NPGEIO INCLUSION
. NIN PAGE.SRT FILE I/O ROUTINES
.
. FILE NAME : PAGE.SRT
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NPGEKEY
. REQUIRED    : 'NPGEFLD'
. RETURNED    : PAGE.SRT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPGEKEY
	BRANCH	NPGEFLG2,NPGE1
	CALL	NPGEOPN2
NPGE1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NPGEFLE2
	READ	NPGEFLE2,NPGEFLD;NPGEVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGETST
. REQUIRED    : NPGEFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPGETST
	BRANCH	NPGEFLG2,NPGE2
	CALL	NPGEOPN2
NPGE2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NPGEFLE2
	READ	NPGEFLE2,NPGEFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGEKS
. REQUIRED    :
. RETURNED    : PAGE.SRT RECORD
. DESCRIPTION : KEY SEQUENTIAL PAGE.SRT FILE READ
.
NPGEKS
	BRANCH	NPGEFLG2,NPGE3
	CALL	NPGEOPN2
NPGE3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NPGEFLE2
	READKS	NPGEFLE2;NPGEVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGESEQ
. REQUIRED    :
. RETURNED    : PAGE.SRT RECORD
. DESCRIPTION : SEQUENTIAL PAGE.SRT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPGESEQ
	BRANCH	NPGEFLAG,NPGE4
	CALL	NPGEOPEN
NPGE4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NPGEFILE
	READ	NPGEFILE,SEQ;NPGEVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGEWRT
. REQUIRED    : 'NPGEFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPGEWRT
	BRANCH	NPGEFLG2,NPGE5
	CALL	NPGEOPN2
NPGE5
	trap	IOMssg giving Error if IO
	FILEPI	1;NPGEFLE2
	WRITE	NPGEFLE2;NPGEVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGEUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NPGEUPD
	BRANCH	NPGEFLG2,NPGE6
	CALL	NPGEOPN2
NPGE6
	trap	IOMssg giving Error if IO
	FILEPI	1;NPGEFLE2
	UPDATE	NPGEFLE2;NPGEVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGEDEL
. REQUIRED    : 'NPGEFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPGEDEL
	BRANCH	NPGEFLG2,NPGE7
	CALL	NPGEOPN2
NPGE7
	trap	IOMssg giving Error if IO
	FILEPI	1;NPGEFLE2
	DELETE	NPGEFLE2
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NPGEEOF
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SETS RECORD POINTER TO END OF FILE
.
NPGEEOF  
	 WEOF      NPGEFILE,SEQEOF
         RETURN

...............................................................................
.
. ENTRY POINT : NPGEOPN2
. REQUIRED    : 'NPGEFLG2'
. RETURNED    : 'NPGEFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PAGE.SRT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPGEOPN2
	TRAP	NPGEGONE giving Error if IO
	OPEN	NPGEFLE2,NPGENAME
	trapclr	IO
	MOVE	C1,NPGEFLG2
	RETURN
...............................................................................
.
. ENTRY POINT : NPGEOPEN
. REQUIRED    : 'NPGEFLAG'
. RETURNED    : 'NPGEFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PAGE.SRT FILE FOR SEQUENTIAL PROCESSING
.
.
NPGEOPEN
	TRAP	NPGEGONE giving Error if IO
	OPEN	NPGEFILE,NPGENAME
	MOVE	C1,NPGEFLAG
	trapclr	IO
	RETURN
.
NPGEGONE
	MOVE	NPGENAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

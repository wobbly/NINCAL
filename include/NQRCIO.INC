..............................................................................
.
. NQRCIO INCLUSION
. NIN DATACARD Quick Reco FILE I/O ROUTINES
.
. FILE NAME : NINQRC
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 1 BYTE CATCODE + 2 BYTE CAT#)
.
..............................................................................
.
. ENTRY POINT : NQRCKEY
. REQUIRED    : 'NQRCFLD'
. RETURNED    : DATACARD Quick Reco RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NQRCKEY
	BRANCH	NQRCFLAG,NQRC1
	CALL	NQRCOPEN
NQRC1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCFILE
	READ	NQRCFILE,NQRCFLD;NQRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCTST
. REQUIRED    : NQRCFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NQRCTST
	BRANCH	NQRCFLAG,NQRC2
	CALL	NQRCOPEN
NQRC2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCFILE
	READ	NQRCFILE,NQRCFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCKS
. REQUIRED    :
. RETURNED    : DATACARD Quick Reco RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD Quick Reco FILE READ
.
NQRCKS
	BRANCH	NQRCFLAG,NQRC3
	CALL	NQRCOPEN
NQRC3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCFILE
	READKS	NQRCFILE;NQRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCSEQ
. REQUIRED    :
. RETURNED    : DATACARD Quick Reco RECORD
. DESCRIPTION : SEQUENTIAL DATACARD Quick Reco FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NQRCSEQ
	BRANCH	NQRCFLAG,NQRC4
	CALL	NQRCOPEN
NQRC4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCFILE
	READ	NQRCFILE,SEQ;NQRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCWRT
. REQUIRED    : 'NQRCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NQRCWRT
	BRANCH	NQRCFLAG,NQRC5
	CALL	NQRCOPEN
NQRC5
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCFLIST
	WRITE	NQRCFLIST;NQRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NQRCUPD
	BRANCH	NQRCFLAG,NQRC6
	CALL	NQRCOPEN
NQRC6
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCFLIST
	UPDATE	NQRCFLIST;NQRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDEL
. REQUIRED    : 'NQRCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NQRCDEL
	BRANCH	NQRCFLAG,NQRC7
	CALL	NQRCOPEN
NQRC7
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCFLIST
	DELETE	NQRCFLIST
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDKEY
. REQUIRED    : 'NQRCDFLD'
. RETURNED    : DATACARD Quick Reco Description RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NQRCDKEY
	BRANCH	NQRCDFLAG,NQRCD1
	CALL	NQRCDOPEN
NQRCD1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCDFILE
	READ	NQRCDFILE,NQRCDFLD;NQRCDVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDTST
. REQUIRED    : NQRCDFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NQRCDTST
	BRANCH	NQRCDFLAG,NQRCD2
	CALL	NQRCDOPEN
NQRCD2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCDFILE
	READ	NQRCDFILE,NQRCDFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDKS
. REQUIRED    :
. RETURNED    : DATACARD Quick Reco RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD Quick Reco FILE READ
.
NQRCDKS
	BRANCH	NQRCDFLAG,NQRCD3
	CALL	NQRCDOPEN
NQRCD3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCDFILE
	READKS	NQRCDFILE;NQRCDVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDSEQ
. REQUIRED    :
. RETURNED    : DATACARD Quick Reco RECORD
. DESCRIPTION : SEQUENTIAL DATACARD Quick Reco FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NQRCDSEQ
	BRANCH	NQRCDFLAG,NQRCD4
	CALL	NQRCDOPEN
NQRCD4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NQRCDFILE
	READ	NQRCDFILE,SEQ;NQRCDVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDWRT
. REQUIRED    : 'NQRCDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NQRCDWRT
	BRANCH	NQRCDFLAG,NQRCD5
	CALL	NQRCDOPEN
NQRCD5
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCDFLIST
	WRITE	NQRCDFLIST;NQRCDVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NQRCDUPD
	BRANCH	NQRCDFLAG,NQRCD6
	CALL	NQRCDOPEN
NQRCD6
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCDFLIST
	UPDATE	NQRCDFLIST;NQRCDVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NQRCDDEL
. REQUIRED    : 'NQRCDFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NQRCDDEL
	BRANCH	NQRCDFLAG,NQRCD7
	CALL	NQRCDOPEN
NQRCD7
	trap	IOMssg giving Error if IO
	FILEPI	1;NQRCDFLIST
	DELETE	NQRCDFLIST
	trapclr	IO
	RETURN


...............................................................................
.
. ENTRY POINT : NQRCOPEN
. REQUIRED    : 'NQRCFLAG'
. RETURNED    : 'NQRCFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD Quick Reco FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NQRCOPEN
	TRAP	NQRCGONE giving Error if IO
	OPEN	NQRCFLIST	.,EXCLUSIVE
.	OPEN	NQRCFLIST
	trapclr	IO
	MOVE	C1,NQRCFLAG
	RETURN
.
NQRCGONE
	MOVE	NQRCNAME,FILENAME
	CALL	FILEGONE
.
...............................................................................
.
. ENTRY POINT : NQRCDOPEN
. REQUIRED    : 'NQRCDFLAG'
. RETURNED    : 'NQRCDFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD Quick Reco FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NQRCDOPEN
	TRAP	NQRCDGONE giving Error if IO
	OPEN	NQRCDFLIST	.,EXCLUSIVE
.	OPEN	NQRCDFLIST
	trapclr	IO
	MOVE	C1,NQRCDFLAG
	RETURN
.
NQRCDGONE
	MOVE	NQRCDNAME,FILENAME
	CALL	FILEGONE
..............................................................................

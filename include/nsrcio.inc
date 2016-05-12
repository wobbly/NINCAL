..............................................................................
.
. NSRCIO INCLUSION
. NIN DATACARD SOURCE FILE I/O ROUTINES
.
. FILE NAME : NINSRC
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NSRCKEY
. REQUIRED    : 'NSRCFLD'
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSRCKEY
	BRANCH	NSRCFLAG,NSRC1
	CALL	NSRCOPEN
NSRC1
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFILE
	READ	NSRCFILE,NSRCFLD;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCTST
. REQUIRED    : NSRCFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSRCTST
	BRANCH	NSRCFLAG,NSRC2
	CALL	NSRCOPEN
NSRC2
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFILE
	READ	NSRCFILE,NSRCFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCKS
. REQUIRED    :
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SOURCE FILE READ
.
NSRCKS
	BRANCH	NSRCFLAG,NSRC3
	CALL	NSRCOPEN
NSRC3
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFILE
	READKS	NSRCFILE;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCSEQ
. REQUIRED    :
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : SEQUENTIAL DATACARD SOURCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSRCSEQ
	BRANCH	NSRCFLAG,NSRC4
	CALL	NSRCOPEN
NSRC4
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFILE
	READ	NSRCFILE,SEQ;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCWRT
. REQUIRED    : 'NSRCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSRCWRT
	BRANCH	NSRCFLAG,NSRC5
	CALL	NSRCOPEN
NSRC5
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFLIST
	WRITE	NSRCFLIST;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NSRCUPD
	BRANCH	NSRCFLAG,NSRC6
	CALL	NSRCOPEN
NSRC6
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFLIST
	UPDATE	NSRCFLIST;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCDEL
. REQUIRED    : 'NSRCFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSRCDEL
	BRANCH	NSRCFLAG,NSRC7
	CALL	NSRCOPEN
NSRC7
	trap	IOMssg giving Error if IO
	FILEPI	1;NSRCFLIST
	DELETE	NSRCFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NSRCAIM
. REQUIRED    : NSRCFLD1
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSRCAIM
	BRANCH	NSRCFLAG,NSRC8
	CALL	NSRCOPEN
NSRC8   
	trap	IOMssg giving Error if IO
	READ	NSRCFLE2,NSRCFLD1;NSRCVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSRCKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSRCKG
	BRANCH	NSRCFLAG,NSRC10
	CALL	NSRCOPEN
NSRC10
	trap	IOMssg giving Error if IO
	READKG	NSRCFLE2;NSRCVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NSRCOPEN
. REQUIRED    : 'NSRCFLAG'
. RETURNED    : 'NSRCFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD SOURCE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSRCOPEN
	TRAP	NSRCGONE giving Error if IO
	OPEN	NSRCFLIST	.,EXCLUSIVE
.	OPEN	NSRCFLIST
	trapclr	IO
	MOVE	C1,NSRCFLAG
	RETURN
.
NSRCGONE
	MOVE	NSRCNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

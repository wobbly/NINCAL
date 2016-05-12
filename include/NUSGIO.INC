..............................................................................
.
. NUSGIO INCLUSION
. DATACARD USAGE FILE I/O ROUTINES
.
. FILE NAME :  NINUSG
. REC LENGTH:  44 VAR
. INDEX KEY:   1-12	NUSGLIST,NUSGMLR
. AIMDEX KEY:  1-6	NUSGLIST
.	       7-12	NUSGMLR
.
..............................................................................
.
. ENTRY POINT : NUSGKEY
. REQUIRED    : 'NUSGFLD'
. RETURNED    : DATACARD USAGE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSGKEY
	BRANCH	NUSGFLAG,NUSG1
	CALL	NUSGOPEN
NUSG1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NUSGFILE
	READ	NUSGFILE,NUSGFLD;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGTST
. REQUIRED    : NUSGFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NUSGTST
	BRANCH	NUSGFLAG,NUSG2
	CALL	NUSGOPEN
NUSG2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NUSGFILE
	READ	NUSGFILE,NUSGFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGKS
. REQUIRED    :
. RETURNED    : DATACARD USAGE RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD USAGE FILE READ
.
NUSGKS
	BRANCH	NUSGFLAG,NUSG3
	CALL	NUSGOPEN
NUSG3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NUSGFILE
	READKS	NUSGFILE;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGSEQ
. REQUIRED    :
. RETURNED    : DATACARD USAGE RECORD
. DESCRIPTION : SEQUENTIAL DATACARD USAGE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSGSEQ
	BRANCH	NUSGFLAG,NUSG4
	CALL	NUSGOPEN
NUSG4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NUSGFILE
	READ	NUSGFILE,SEQ;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGWRT
. REQUIRED    : 'NUSGFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NUSGWRT
	BRANCH	NUSGFLAG,NUSG5
	CALL	NUSGOPEN
NUSG5
	trap	IOMssg giving Error if IO
	FILEPI	1;NUSGFLIST
	WRITE	NUSGFLIST;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NUSGUPD
	BRANCH	NUSGFLAG,NUSG6
	CALL	NUSGOPEN
NUSG6
	trap	IOMssg giving Error if IO
	FILEPI	1;NUSGFLIST
	UPDATE	NUSGFLIST;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGDEL
. REQUIRED    : 'NUSGFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NUSGDEL
	BRANCH	NUSGFLAG,NUSG7
	CALL	NUSGOPEN
NUSG7
	trap	IOMssg giving Error if IO
	FILEPI	1;NUSGFLIST
	DELETE	NUSGFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NUSGAIM
. REQUIRED    : NUSGFLD1, NUSGFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSGAIM
	BRANCH	NUSGFLAG,NUSG8
	CALL	NUSGOPEN
NUSG8   
	trap	IOMssg giving Error if IO
	READ	NUSGFLE2,NUSGFLD1,NUSGFLD2;NUSGVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NUSGKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENERIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSGKG
	BRANCH	NUSGFLAG,NUSG10
	CALL	NUSGOPEN
NUSG10
	trap	IOMssg giving Error if IO
	READKG	NUSGFLE2;NUSGVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NUSGOPEN
. REQUIRED    : 'NUSGFLAG'
. RETURNED    : 'NUSGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD USAGE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NUSGOPEN
	TRAP	NUSGGONE giving Error if IO
.	OPEN	NUSGFLIST,EXCLUSIVE
	OPEN	NUSGFLIST
	trapclr	IO
	MOVE	C1,NUSGFLAG
	RETURN
.
NUSGGONE
	MOVE	NUSGFNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

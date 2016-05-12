...............................................................................
.                                                                             .
.NUSEIO.INC - IO MODULE FOR DATABUS PORT/USER IDENTIFICATION.           .
...............................................................................
. FILENAME:     NINUSERS
. LENGTH:       45 FIXED
. ACCESS:       ISI
. KEY   :       1-3    (PORT NUMBER)
.              29-31   (TYPIST INITIALS)
..............................................................................
. patch 1.1 06SEP2005  ASH  Added FileList logic - Rewrote ALL routines
.add  call to trim plb 8.6 appears to have a problem with trailing blanks on nt4 machines
.REVISED  04FEB92
.CREATED 13 JUNE 90.
...............................................................................
.
. ENTRY POINT : NUSEKEY
. REQUIRED    : 'NUSEFLD'
. RETURNED    : USERS RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSEKEY
	BRANCH	NUSEPATH,NUSE1A,NUSE1C
NUSE1A
	BRANCH	NUSEFLAG,NUSE1B
	CALL	NUSEOPEN
NUSE1B
.	FILEPI	1;NUSEFILE
	READ	NUSEFILE,NUSEFLD;NUSEVARS
	RETURN
NUSE1C
	BRANCH	NUSEFLAG,NUSE1D
	CALL	NUSEOPEN
NUSE1D
.	FILEPI	1;NUSEFLE2
	READ	NUSEFLE2,NUSEFLD2;NUSEVARS
	RETURN
...............................................................................
.
. ENTRY POINT : NUSETST
. REQUIRED    : NUSEFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NUSETST
	BRANCH	NUSEPATH,NUSE2A,NUSE2C
NUSE2A
	BRANCH	NUSEFLAG,NUSE2B
	CALL	NUSEOPEN
NUSE2B
.	FILEPI	1;NUSEFILE
	READ	NUSEFILE,NUSEFLD;;
	RETURN
NUSE2C
	BRANCH	NUSEFLAG,NUSE2D
	CALL	NUSEOPEN
NUSE2D
.	FILEPI	1;NUSEFLE2
	READ	NUSEFLE2,NUSEFLD2;;
	RETURN
...............................................................................
.
. ENTRY POINT : NUSEKS
. REQUIRED    :
. RETURNED    : USERS RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NUSEKS
	BRANCH	NUSEPATH,NUSE3A,NUSE3C
NUSE3A
	BRANCH	NUSEFLAG,NUSE3B
	CALL	NUSEOPEN
NUSE3B
.	FILEPI	1;NUSEFILE
	READKS	NUSEFILE;NUSEVARS
	RETURN
NUSE3C
	BRANCH	NUSEFLAG,NUSE3D
	CALL	NUSEOPEN
NUSE3D
.	FILEPI    1;NUSEFLE2
	READKS    NUSEFLE2;NUSEVARS
	RETURN
..............................................................................
.
. ENTRY POINT : NUSESEQ
. REQUIRED    :
. RETURNED    : USERS RECORD
. DESCRIPTION : SEQUENTIAL USER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NUSESEQ
	BRANCH	NUSEFLAG,NUSE4
	CALL	NUSEOPEN
NUSE4
.	FILEPI	1;NUSEFILE
	READ	NUSEFILE,SEQ;NUSEVARS
	RETURN
..............................................................................
.
. ENTRY POINT : NUSEWRT
. REQUIRED    : 'NUSEFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NUSEWRT
	BRANCH	NUSEFLAG,NUSE5
	CALL	NUSEOPEN
NUSE5
	FILEPI	1;NUSEFLIST
	WRITE	NUSEFLIST;NUSEVARS
	RETURN
...............................................................................
.
. ENTRY POINT : NUSEUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE USER FILE
.
NUSEUPD
	BRANCH	NUSEFLAG,NUSE6
	CALL	NUSEOPEN
NUSE6
	FILEPI	1;NUSEFLIST
	UPDATE	NUSEFLIST;NUSEVARS
	RETURN
...............................................................................
.
. ENTRY POINT : NUSEDEL
. REQUIRED    : 'NUSEFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NUSEDEL
	BRANCH	NUSEFLAG,NUSE7
	CALL	NUSEOPEN
NUSE7
	FILEPI	1;NUSEFLIST
	DELETE	NUSEFLIST
	RETURN
...............................................................................
.
. ENTRY POINT : NUSEOPEN
. REQUIRED    : 'NUSEFLAG'
. RETURNED    : 'NUSEFLAG' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN NINUSER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NUSEOPEN
	TRAP	NUSEGONE giving Error if IO
	OPEN	NUSEFLIST
	trapclr	IO
	MOVE	C1,NUSEFLAG
	RETURN
.
NUSEGONE
	MOVE	NUSENAME TO FILENAME
	CALL	FILEGONE
.
..............................................................................
.........................................................
.
. NLOL2IO INCLUSION
. NIN LOL FILE I/O ROUTINES
.
. FILE NAME :   NINLOL2
. REC LENGTH:   34 FIXED
. INDEX KEY :   (1)     001-025 (LOL Num + Integral LOL ID + Record Type Byte)
.
. AAMDEX KEY:   (1)     001-006 (LOL Num)
.               (2)     007-024 (Integral LOL ID)
.
.
.........................................................
. ENTRY POINT : NLOL2KEY
. REQUIRED    : 'NLOL2FLD'
. RETURNED    : LOL CROSS REFERENCE VARS
. DESCRIPTION : EXACT ISAM KEY READ
.
NLOL2KEY
	branch	NLOL2flag to NLOL21a
	CALL	NLOL2OPEN
NLOL21A
	TRAP	IOMssg Giving Error if IO
	branch	NLOL2lock to NLOL21AL,NLOL21AR,NLOL21AN
NLOL21AL
	FILEPI	1;NLOL2FILE
	READ	NLOL2FILE,NLOL2FLD;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL21AR  
	READLK	NLOL2FILE,NLOL2FLD;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL21AN
	READ	NLOL2FILE,NLOL2FLD;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2TST
. REQUIRED    : NLOL2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NLOL2TST
	branch	NLOL2flag to NLOL22a
	CALL	NLOL2OPEN
NLOL22A
	TRAP	IOMssg Giving Error if IO
	branch	NLOL2lock to NLOL22AL,NLOL22AR,NLOL22AN
NLOL22AL
	FILEPI	1;NLOL2FILE
	READ	NLOL2FILE,NLOL2FLD;;
	TRAPCLR	IO
	RETURN
NLOL22AR  
	READLK	NLOL2FILE,NLOL2FLD;;
	TRAPCLR	IO
	RETURN
NLOL22AN
	READ	NLOL2FILE,NLOL2FLD;;
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2KS
. REQUIRED    : 'NLOL2FLAG'
. RETURNED    : LOL CROSS REFERENCE VARS
. DESCRIPTION : KEY SEQUENTIAL LOL CROSS REFERENCE FILE READ
.
NLOL2KS
	branch	NLOL2FLAG to NLOL23A
	CALL	NLOL2OPEN
NLOL23A
	TRAP	IOMssg Giving Error if IO
	branch	NLOL2lock to NLOL23AL,NLOL23AR,NLOL23AN
NLOL23AL
	FILEPI	1;NLOL2FILE
	READKS	NLOL2FILE;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL23AR
	READKSLK NLOL2FILE;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL23AN
	READKS	NLOL2FILE;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2SEQ
. REQUIRED    :
. RETURNED    : LOL CROSS REFERENCE VARS
. DESCRIPTION : SEQUENTIAL LOL CROSS REFERENCE FILE READ
.
NLOL2SEQ
	BRANCH	NLOL2FLAG TO NLOL24
	CALL	NLOL2OPEN
NLOL24
	TRAP	IOMssg Giving Error if IO
	branch	NLOL2lock to NLOL24L,NLOL24R,NLOL24N
NLOL24L
	FILEPI	1;NLOL2FILE
	READ	NLOL2FILE,SEQ;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL24R
	READLK	NLOL2FILE,SEQ;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL24N
	READ	NLOL2FILE,SEQ;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2WRT
. REQUIRED    : 'NLOL2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT.
.
NLOL2WRT
	BRANCH	NLOL2FLAG,NLOL25
	CALL	NLOL2OPEN
NLOL25
	trap	IOMssg giving Error if IO
	FILEPI	1;NLOL2FLIST
	WRITE	NLOL2FLIST;NLOL2VARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LOL FILE, APPLICATIONS RESPONSIBILITY TO UPDATE OTHER FILES!!!
.
NLOL2UPD
	BRANCH	NLOL2FLAG,NLOL26
	CALL	NLOL2OPEN
NLOL26
	trap	IOMssg giving Error if IO
	FILEPI	1;NLOL2FLIST
	UPDATE	NLOL2FLIST;NLOL2VARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2DEL                                             
. REQUIRED    : 'NLOL2PATH'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ALL KEYS.
.
NLOL2DEL
	BRANCH	NLOL2FLAG,NLOL27
	CALL	NLOL2OPEN
NLOL27
	trap	IOMssg giving Error if IO
	FILEPI	1;NLOL2FLIST
	DELETE	NLOL2FLIST
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2KP
. REQUIRED    : 'NLOL2FLAG'
. RETURNED    : LOL CROSS REFERENCE RECORD
. DESCRIPTION : KEY SEQUENTIAL LOL CROSS REFERENCE FILE READ
.
NLOL2KP
	Branch	NLOL2FLAG TO NLOL28
	CALL	NLOL2OPEN
NLOL28
	TRAP	IOMssg Giving Error if IO
	Branch	NLOL2lock to NLOL28L,NLOL28R,NLOL28N
NLOL28L
	FILEPI	1;NLOL2FILE
	READKP	NLOL2FILE;NLOL2VARS
	TRAPCLR   IO
	RETURN
NLOL28R
	READKPLK NLOL2FILE;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL28N
	READKP	NLOL2FILE;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2AIM
. REQUIRED    : NLOL2FLD1,NLOL2FLD2
. RETURNED    : LOL CROSS REFERENCE RECORD
. DESCRIPTION : AIM LOL CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOL2AIM
	BRANCH	NLOL2FLAG TO NLOL29
	CALL	NLOL2OPEN
NLOL29
	TRAP	IOMssg Giving Error if IO
	Branch	NLOL2lock to NLOL29L,NLOL29R,NLOL29N
NLOL29L
	FILEPI	1;NLOL2FLE1
	READ	NLOL2FLE1,NLOL2FLD1,NLOL2FLD2,NLOL2FLD3,NLOL2FLD4,NLOL2FLD5;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL29R
	READLK	NLOL2FLE1,NLOL2FLD1,NLOL2FLD2,NLOL2FLD3,NLOL2FLD4,NLOL2FLD5;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL29N
	READ	NLOL2FLE1,NLOL2FLD1,NLOL2FLD2,NLOL2FLD3,NLOL2FLD4,NLOL2FLD5;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : LOL CROSS REFERENCE RECORD
. DESCRIPTION : AIM KEY GENERIC LOL CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOL2KG
	BRANCH	NLOL2FLAG TO NLOL210
	CALL	NLOL2OPEN
NLOL210
	TRAP	IOMssg Giving Error if IO
	Branch	NLOL2lock to NLOL210L,NLOL210R,NLOL210N
NLOL210L
	FILEPI	1;NLOL2FLE1
	READKG	NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL210R
	READKGLK NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL210N
	READKG	NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NLOL2KGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : LOL CROSS REFERENCE RECORD
. DESCRIPTION : Aim KEY GENERIC PRIOR LOL CROSS REFERENCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOL2KGP
	BRANCH	NLOL2FLAG TO NLOL211
	CALL	NLOL2OPEN
NLOL211
	TRAP	IOMssg Giving Error if IO
	branch	NLOL2lock to NLOL211L,NLOL211R,NLOL211N
NLOL211L
	FILEPI	1;NLOL2FLE1
	READKGP	NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL211R
	READKGPLK NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
NLOL211N
	READKGP	NLOL2FLE1;NLOL2VARS
	TRAPCLR	IO
	RETURN
...............................................................................
.
. ENTRY POINT : NLOL2OPEN
. REQUIRED    :
. RETURNED    : 'NLOL2FLAG' SET TO THE PATH NUMBER: 'NLOL2PATH', IF OPENNED.
. DESCRIPTION : OPEN NIN LOL FILE (HIDDEN KEY), DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLOL2OPEN
	TRAP	NLOL2GONE giving Error if IO
.	OPEN	NLOL2FLIST,EXCLUSIVE
	OPEN	NLOL2FLIST
	trapclr	IO
	MOVE	C1,NLOL2FLAG
	RETURN
.
NLOL2GONE
	MOVE	NLOL2NAME,FILENAME
	CALL	FILEGONE

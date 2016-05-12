..............................................................................
.
. NModBIO INCLUSION
. NIN DATACARD PRICE MODIFIER FILE I/O ROUTINES
.
. FILE NAME : NINModB
. REC LENGTH: 42 VAR
. INDEX KEY : 1-3
.
..............................................................................
.
. ENTRY POINT : NModBKEY
. REQUIRED    : 'NModBFLD'
. RETURNED    : DATACARD PRICE MODIFIER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NModBKEY
	BRANCH	NModBFLAG,NModB1
	CALL	NModBOPEN
NModB1
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFILE
	READ	NModBFILE,NModBFLD;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBTST
. REQUIRED    : NModBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NModBTST
	BRANCH	NModBFLAG,NModB2
	CALL	NModBOPEN
NModB2
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFILE
	READ	NModBFILE,NModBFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBKS
. REQUIRED    :
. RETURNED    : DATACARD PRICE MODIFIER RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD PRICE MODIFIER FILE READ
.
NModBKS
	BRANCH	NModBFLAG,NModB3
	CALL	NModBOPEN
NModB3
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFILE
	READKS	NModBFILE;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBSEQ
. REQUIRED    :
. RETURNED    : DATACARD PRICE MODIFIER RECORD
. DESCRIPTION : SEQUENTIAL DATACARD PRICE MODIFIER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NModBSEQ
	BRANCH	NModBFLAG,NModB4
	CALL	NModBOPEN
NModB4
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFILE
	READ	NModBFILE,SEQ;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBWRT
. REQUIRED    : 'NModBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NModBWRT
	BRANCH	NModBFLAG,NModB5
	CALL	NModBOPEN
NModB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFLIST
	WRITE	NModBFLIST;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NModBUPD
	BRANCH	NModBFLAG,NModB6
	CALL	NModBOPEN
NModB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFLIST
	UPDATE	NModBFLIST;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBDEL
. REQUIRED    : 'NModBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NModBDEL
	BRANCH	NModBFLAG,NModB7
	CALL	NModBOPEN
NModB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NModBFLIST
	DELETE	NModBFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NModBAIM
. REQUIRED    : NModBFLD1
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NModBAIM
	BRANCH	NModBFLAG,NModB8
	CALL	NModBOPEN
NModB8   
	trap	IOMssg giving Error if IO
	READ	NModBFLE2,NModBFLD1;NModBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NModBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NModBKG
	BRANCH	NModBFLAG,NModB10
	CALL	NModBOPEN
NModB10
	trap	IOMssg giving Error if IO
	READKG	NModBFLE2;NModBVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NModBOPEN
. REQUIRED    : 'NModBFLAG'
. RETURNED    : 'NModBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD PRICE MODIFIER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NModBOPEN
	TRAP	NModBGONE giving Error if IO
.	OPEN	NModBFLIST,EXCLUSIVE
	OPEN	NModBFLIST
	trapclr	IO
	MOVE	C1,NModBFLAG
	RETURN
.
NModBGONE
	MOVE	NModBNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

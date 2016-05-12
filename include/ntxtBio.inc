..............................................................................
.
. NTxtBIO INCLUSION
. NIN DATACARD TEXT FILE I/O ROUTINES
.
. FILE NAME : NINTxtB
. REC LENGTH: 510 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NTxtBKEY
. REQUIRED    : 'NTxtBFLD'
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NTxtBKEY
	BRANCH	NTxtBFLAG,NTxtB1
	CALL	NTxtBOPEN
NTxtB1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NTxtBFILE
	READ	NTxtBFILE,NTxtBFLD;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBTST
. REQUIRED    : NTxtBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NTxtBTST
	BRANCH	NTxtBFLAG,NTxtB2
	CALL	NTxtBOPEN
NTxtB2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NTxtBFILE
	READ	NTxtBFILE,NTxtBFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBKS
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD TEXT FILE READ
.
NTxtBKS
	BRANCH	NTxtBFLAG,NTxtB3
	CALL	NTxtBOPEN
NTxtB3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NTxtBFILE
	READKS	NTxtBFILE;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBSEQ
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : SEQUENTIAL DATACARD TEXT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NTxtBSEQ
	BRANCH	NTxtBFLAG,NTxtB4
	CALL	NTxtBOPEN
NTxtB4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NTxtBFILE
	READ	NTxtBFILE,SEQ;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBWRT
. REQUIRED    : 'NTxtBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NTxtBWRT
	BRANCH	NTxtBFLAG,NTxtB5
	CALL	NTxtBOPEN
NTxtB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NTxtBFLIST
	WRITE	NTxtBFLIST;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NTxtBUPD
	BRANCH	NTxtBFLAG,NTxtB6
	CALL	NTxtBOPEN
NTxtB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NTxtBFLIST
	UPDATE	NTxtBFLIST;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBDEL
. REQUIRED    : 'NTxtBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NTxtBDEL
	BRANCH	NTxtBFLAG,NTxtB7
	CALL	NTxtBOPEN
NTxtB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NTxtBFLIST
	DELETE	NTxtBFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NTxtBAIM
. REQUIRED    : NTxtBFLD1,NTxtBFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NTxtBAIM
	BRANCH	NTxtBFLAG,NTxtB8
	CALL	NTxtBOPEN
NTxtB8   
	trap	IOMssg giving Error if IO
	READ	NTxtBFLE2,NTxtBFLD1,NTxtBFLD2;NTxtBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NTxtBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NTxtBKG
	BRANCH	NTxtBFLAG,NTxtB10
	CALL	NTxtBOPEN
NTxtB10
	trap	IOMssg giving Error if IO
	READKG	NTxtBFLE2;NTxtBVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NTxtBOPEN
. REQUIRED    : 'NTxtBFLAG'
. RETURNED    : 'NTxtBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD TEXT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NTxtBOPEN
	TRAP	NTxtBGONE giving Error if IO
	OPEN	NTxtBFLIST	.,EXCLUSIVE
.	OPEN	NTxtBFLIST
	trapclr	IO
	MOVE	C1,NTxtBFLAG
	RETURN
.
NTxtBGONE
	MOVE	NTxtBNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

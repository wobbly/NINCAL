..............................................................................
.
. NArrBIO INCLUSION
. NIN DATACARD ARRANGEMENT FILE I/O ROUTINES
.
. FILE NAME : NINArrB
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NArrBKEY
. REQUIRED    : 'NArrBFLD'
. RETURNED    : DATACARD ARRANGEMENT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NArrBKEY
	BRANCH	NArrBFLAG,NArrB1
	CALL	NArrBOPEN
NArrB1
	trap	IOMssg giving Error if IO
.	FILEPI	1;NArrBFILE
	READ	NArrBFILE,NArrBFLD;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBTST
. REQUIRED    : NArrBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NArrBTST
	BRANCH	NArrBFLAG,NArrB2
	CALL	NArrBOPEN
NArrB2
	trap	IOMssg giving Error if IO
.	FILEPI	1;NArrBFILE
	READ	NArrBFILE,NArrBFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBKS
. REQUIRED    :
. RETURNED    : DATACARD ARRANGEMENT RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD ARRANGEMENT FILE READ
.
NArrBKS
	BRANCH	NArrBFLAG,NArrB3
	CALL	NArrBOPEN
NArrB3
	trap	IOMssg giving Error if IO
.	FILEPI	1;NArrBFILE
	READKS	NArrBFILE;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBSEQ
. REQUIRED    :
. RETURNED    : DATACARD ARRANGEMENT RECORD
. DESCRIPTION : SEQUENTIAL DATACARD ARRANGEMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NArrBSEQ
	BRANCH	NArrBFLAG,NArrB4
	CALL	NArrBOPEN
NArrB4
	trap	IOMssg giving Error if IO
.	FILEPI	1;NArrBFILE
	READ	NArrBFILE,SEQ;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBWRT
. REQUIRED    : 'NArrBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NArrBWRT
	BRANCH	NArrBFLAG,NArrB5
	CALL	NArrBOPEN
NArrB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NArrBFLIST
	WRITE	NArrBFLIST;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NArrBUPD
	BRANCH	NArrBFLAG,NArrB6
	CALL	NArrBOPEN
NArrB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NArrBFLIST
	UPDATE	NArrBFLIST;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBDEL
. REQUIRED    : 'NArrBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NArrBDEL
	BRANCH	NArrBFLAG,NArrB7
	CALL	NArrBOPEN
NArrB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NArrBFLIST
	DELETE	NArrBFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NArrBAIM
. REQUIRED    : NArrBFLD1
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NArrBAIM
	BRANCH	NArrBFLAG,NArrB8
	CALL	NArrBOPEN
NArrB8   
	trap	IOMssg giving Error if IO
	READ	NArrBFLE2,NArrBFLD1;NArrBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NArrBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NArrBKG
	BRANCH	NArrBFLAG,NArrB10
	CALL	NArrBOPEN
NArrB10
	trap	IOMssg giving Error if IO
	READKG	NArrBFLE2;NArrBVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NArrBOPEN
. REQUIRED    : 'NArrBFLAG'
. RETURNED    : 'NArrBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD ARRANGEMENT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NArrBOPEN
	TRAP	NArrBGONE giving Error if IO
	OPEN	NArrBFLIST	.,EXCLUSIVE
.	OPEN	NArrBFLIST
	trapclr	IO
	MOVE	C1,NArrBFLAG
	RETURN
.
NArrBGONE
	MOVE	NArrBNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

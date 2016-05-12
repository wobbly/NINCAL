..............................................................................
.
. NSrcBIO INCLUSION
. NIN DATACARD SOURCE FILE I/O ROUTINES
.
. FILE NAME : NINSrcB
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NSrcBKEY
. REQUIRED    : 'NSrcBFLD'
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSrcBKEY
	BRANCH	NSrcBFLAG,NSrcB1
	CALL	NSrcBOPEN
NSrcB1
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFILE
	READ	NSrcBFILE,NSrcBFLD;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBTST
. REQUIRED    : NSrcBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSrcBTST
	BRANCH	NSrcBFLAG,NSrcB2
	CALL	NSrcBOPEN
NSrcB2
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFILE
	READ	NSrcBFILE,NSrcBFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBKS
. REQUIRED    :
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SOURCE FILE READ
.
NSrcBKS
	BRANCH	NSrcBFLAG,NSrcB3
	CALL	NSrcBOPEN
NSrcB3
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFILE
	READKS	NSrcBFILE;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBSEQ
. REQUIRED    :
. RETURNED    : DATACARD SOURCE RECORD
. DESCRIPTION : SEQUENTIAL DATACARD SOURCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSrcBSEQ
	BRANCH	NSrcBFLAG,NSrcB4
	CALL	NSrcBOPEN
NSrcB4
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFILE
	READ	NSrcBFILE,SEQ;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBWRT
. REQUIRED    : 'NSrcBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSrcBWRT
	BRANCH	NSrcBFLAG,NSrcB5
	CALL	NSrcBOPEN
NSrcB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFLIST
	WRITE	NSrcBFLIST;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NSrcBUPD
	BRANCH	NSrcBFLAG,NSrcB6
	CALL	NSrcBOPEN
NSrcB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFLIST
	UPDATE	NSrcBFLIST;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBDEL
. REQUIRED    : 'NSrcBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSrcBDEL
	BRANCH	NSrcBFLAG,NSrcB7
	CALL	NSrcBOPEN
NSrcB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NSrcBFLIST
	DELETE	NSrcBFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NSrcBAIM
. REQUIRED    : NSrcBFLD1
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSrcBAIM
	BRANCH	NSrcBFLAG,NSrcB8
	CALL	NSrcBOPEN
NSrcB8   
	trap	IOMssg giving Error if IO
	READ	NSrcBFLE2,NSrcBFLD1;NSrcBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSrcBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSrcBKG
	BRANCH	NSrcBFLAG,NSrcB10
	CALL	NSrcBOPEN
NSrcB10
	trap	IOMssg giving Error if IO
	READKG	NSrcBFLE2;NSrcBVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NSrcBOPEN
. REQUIRED    : 'NSrcBFLAG'
. RETURNED    : 'NSrcBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD SOURCE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSrcBOPEN
	TRAP	NSrcBGONE giving Error if IO
	OPEN	NSrcBFLIST	.,EXCLUSIVE
.	OPEN	NSrcBFLIST
	trapclr	IO
	MOVE	C1,NSrcBFLAG
	RETURN
.
NSrcBGONE
	MOVE	NSrcBNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

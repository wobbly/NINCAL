..............................................................................
.
. NSltBIO INCLUSION
. NIN DATACARD SELECTION FILE I/O ROUTINES
.
. FILE NAME : NINSltB
. REC LENGTH: 29 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NSltBKEY
. REQUIRED    : 'NSltBFLD'
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSltBKEY
	BRANCH	NSltBFLAG,NSltB1
	CALL	NSltBOPEN
NSltB1
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFILE
	READ	NSltBFILE,NSltBFLD;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBTST
. REQUIRED    : NSltBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSltBTST
	BRANCH	NSltBFLAG,NSltB2
	CALL	NSltBOPEN
NSltB2
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFILE
	READ	NSltBFILE,NSltBFLD;;
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBKS
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SELECTION FILE READ
.
NSltBKS
	BRANCH	NSltBFLAG,NSltB3
	CALL	NSltBOPEN
NSltB3
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFILE
	READKS	NSltBFILE;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBSEQ
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : SEQUENTIAL DATACARD SELECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSltBSEQ
	BRANCH	NSltBFLAG,NSltB4
	CALL	NSltBOPEN
NSltB4
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFILE
	READ	NSltBFILE,SEQ;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBWRT
. REQUIRED    : 'NSltBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSltBWRT
	BRANCH	NSltBFLAG,NSltB5
	CALL	NSltBOPEN
NSltB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFLIST
	WRITE	NSltBFLIST;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NSltBUPD
	BRANCH	NSltBFLAG,NSltB6
	CALL	NSltBOPEN
NSltB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFLIST
	UPDATE	NSltBFLIST;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBDEL
. REQUIRED    : 'NSltBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSltBDEL
	BRANCH	NSltBFLAG,NSltB7
	CALL	NSltBOPEN
NSltB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NSltBFLIST
	DELETE	NSltBFLIST
	trapclr	IO
	RETURN

..............................................................................
. ENTRY POINT : NSltBAIM
. REQUIRED    : NSltBFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSltBAIM
	BRANCH	NSltBFLAG,NSltB8
	CALL	NSltBOPEN
NSltB8   
	trap	IOMssg giving Error if IO
	READ	NSltBFLE2,NSltBFLD1;NSltBVARS
	trapclr	IO
	RETURN

..............................................................................
.
. ENTRY POINT : NSltBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSltBKG
	BRANCH	NSltBFLAG,NSltB10
	CALL	NSltBOPEN
NSltB10
	trap	IOMssg giving Error if IO
	READKG	NSltBFLE2;NSltBVARS
	trapclr	IO
	RETURN

...............................................................................
.
. ENTRY POINT : NSltBOPEN
. REQUIRED    : 'NSltBFLAG'
. RETURNED    : 'NSltBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD SELECTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSltBOPEN
	TRAP	NSltBGONE giving Error if IO
	OPEN	NSltBFLIST	.,EXCLUSIVE
.	OPEN	NSltBFLIST
	trapclr	IO
	MOVE	C1,NSltBFLAG
	RETURN
.
NSltBGONE
	MOVE	NSltBNAME,FILENAME
	CALL	FILEGONE
.
..............................................................................

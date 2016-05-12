;..............................................................................
;.
;. CompSFIO INCLUSION
;. NIN Master CompSFany  FILE I/O ROUTINES
;.
;..............................................................................
;.
;. ENTRY POINT : CompSFKEY
;. REQUIRED    : 'CompSFFLD'
;. RETURNED    : CompSFany RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;.
CompSFKEY
	BRANCH	CompSFFLAG,CompSF1
	CALL	CompSFOPEN
CompSF1
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF1L,CompSF1R,CompSF1N
CompSF1L
	FILEPI	1;CompSFFILE
	READ	CompSFFILE,CompSFFLD;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF1R
	READLK	CompSFFILE,CompSFFLD;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF1N
	READ	CompSFFILE,CompSFFLD;CompSFVARS
   TRAPCLR   IO
	RETURN
;Broker
;..............................................................................
;.
;. ENTRY POINT : CompSFKEY2
;. REQUIRED    : 'CompSFFLD2'
;. RETURNED    : CompSFany RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
CompSFKEY2
	BRANCH	CompSFFLAG,CompSF2
	CALL	CompSFOPEN
CompSF2
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF2L,CompSF2R,CompSF2N
CompSF2L
	FILEPI	1;CompSFFILE2
	READ	CompSFFILE2,CompSFFLD2;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF2R
	READLK	CompSFFILE2,CompSFFLD2;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF2N
	READ	CompSFFILE2,CompSFFLD2;CompSFVARS
   TRAPCLR   IO
	RETURN
;..............................................................................
;.
;.
;. ENTRY POINT : CompSFTST
;. REQUIRED    : CompSFFLD
;. RETURNED    :
;. DESCRIPTION : TEST KEY
;.
CompSFTST
	BRANCH	CompSFFLAG,CompSF2T
	CALL	CompSFOPEN
CompSF2T
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF2LT,CompSF2RT,CompSF2NT
CompSF2LT
	FILEPI	1;CompSFFILE
	READ	CompSFFILE,CompSFFLD;;
   TRAPCLR   IO
	RETURN
CompSF2RT
	READLK	CompSFFILE,CompSFFLD;;
   TRAPCLR   IO
	RETURN
CompSF2NT
	READ	CompSFFILE,CompSFFLD;;
   TRAPCLR   IO
	RETURN
;. ENTRY POINT : CompSFTST2
;. REQUIRED    : CompSFFLD4
;. RETURNED    :
;. DESCRIPTION : Broker TEST KEY
;.
CompSFTST2
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFFLAG,CompSF2T2
	CALL	CompSFOPEN
CompSF2T2
	BRANCH	CompSFLOCK,CompSF2LT2,CompSF2RT2,CompSF2NT2
CompSF2LT2
	FILEPI	1;CompSFFILE2
	READ	CompSFFILE2,CompSFFLD2;;
   TRAPCLR   IO
	RETURN
CompSF2RT2
	READLK	CompSFFILE2,CompSFFLD2;;
   TRAPCLR   IO
	RETURN
CompSF2NT2
	READ	CompSFFILE2,CompSFFLD2;;
   TRAPCLR   IO
	RETURN
;------------------------------------------------------------------------------
;..............................................................................
;.
;. ENTRY POINT : CompSFKS
;. REQUIRED    :
;. RETURNED    : CompSFany RECORD
;. DESCRIPTION : KEY SEQUENTIAL CompSFany FILE READ
;.
CompSFKS
	BRANCH	CompSFFLAG,CompSF3A
	CALL	CompSFOPEN
CompSF3A
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF3AL,CompSF3AR,CompSF3AN
CompSF3AL
	FILEPI	1;CompSFFILE
	READKS	CompSFFILE;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF3AR
	READKSLK	CompSFFILE;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF3AN
	READKS	CompSFFILE;CompSFVARS
   TRAPCLR   IO
	RETURN
;Patch1.0
;..............................................................................
;.
;. ENTRY POINT : CompSFKS2
;. REQUIRED    :
;. RETURNED    : Broker/CompSFany RECORD
;. DESCRIPTION : KEY SEQUENTIAL CompSFany FILE READ
;.
CompSFKS2
	BRANCH	CompSFFLAG,CompSF3A2
	CALL	CompSFOPEN
CompSF3A2
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF3AL2,CompSF3AR2,CompSF3AN2
CompSF3AL2
	FILEPI	1;CompSFFILE2
	READKS	CompSFFILE2;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF3AR2
	READKSLK	CompSFFILE2;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF3AN2
	READKS	CompSFFILE2;CompSFVARS
   TRAPCLR   IO
	RETURN
;..............................................................................
;..............................................................................
;. ENTRY POINT : CompSFSEQ
;. REQUIRED    :
;. RETURNED    : CompSFany RECORD
;. DESCRIPTION : SEQUENTIAL CompSFany FILE READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
;.
CompSFSEQ
	BRANCH	CompSFFLaG,CompSF4
	CALL	CompSFOPEN
CompSF4
	TRAP	IOMssg Giving Error if IO
	BRANCH	CompSFLOCK,CompSF4L,CompSF4R,CompSF4N
CompSF4L
	FILEPI	1;CompSFFILE
	READ	CompSFFILE,SEQ;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF4r
	READLK	CompSFFILE,SEQ;CompSFVARS
   TRAPCLR   IO
	RETURN
CompSF4N
	READ	CompSFFILE,SEQ;CompSFVARS
   TRAPCLR   IO
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CompSFWRT
;. REQUIRED    : 'CompSFFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
;.
CompSFWRT
	BRANCH	CompSFFLAG,CompSF5
	CALL	CompSFOPEN
CompSF5
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CompSFFLIST
	WRITE	CompSFFLIST;CompSFVARS
   TRAPCLR   IO
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CompSFUPD
;. REQUIRED    : A PREVIOUS KEY READ
;. RETURNED    :
;. DESCRIPTION : KEY UPDATE CompSFany FILE
;.
CompSFUPD
	BRANCH	CompSFFLAG,CompSF6
	CALL	CompSFOPEN
CompSF6
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CompSFFLIST
	UPDATE	CompSFFLIST;CompSFVARS
   TRAPCLR   IO
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CompSFDEL
;. REQUIRED    : 'CompSFFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
;.
CompSFDEL
	BRANCH	CompSFFLAG,CompSF7
	CALL	CompSFOPEN
CompSF7
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CompSFFLIST
	DELETE	CompSFFLIST
	RETURN
;.............................................................................
;.
;. ENTRY POINT : CompSFOPEN
;. REQUIRED    : 'CompSFFLAG' 'CompSFPATH'
;. RETURNED    : 'CompSFFLAG' SET TO '1' IF OPENNED
;. DESCRIPTION : OPEN NIN CompSFany FILE
;.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
;.
CompSFOPEN
	TRAP	CompSFGONE IF IO
	OPEN	CompSFFLIST
	TRAPCLR	IO
	MOVE	C1,CompSFFLAG
	RETURN
;..............................................................................
CompSFGONE
	MOVE	CompSFNAME,FILENAME
	CALL	FILEGONE
;..............................................................................
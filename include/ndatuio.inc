..............................................................................
.
. NDatUIO INCLUSION
. NIN DATACARD FILE I/O ROUTINES
.
. FILE NAME : NINDatU
. REC LENGTH: 50 FIXED
. INDEX KEY : (1)  1-6      #
. AIMDEX KEYS:(1) 7-31      NAME
. patch 1.0 17October2006 DLH - Created
.
..............................................................................
.
. ENTRY POINT : NDatUKEY
. REQUIRED    : 'NDatUPATH', 'NDatUFLD'
. RETURNED    : DATACARD
. DESCRIPTION : EXACT ISAM KEY READ
.
NDatUKEY
	BRANCH	NDatUPATH,NDatU1,NDatU1X,NDatU1B
NDatU1
	BRANCH	NDatUFLAG,NDatU1A
	CALL	NDatUOPEN
NDatU1A
	BRANCH	NDatULOCK,NDatU1Al,NDatU1AR,NDatU1AN
.with file locks
NDatU1Al
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFILE
	READ	NDatUFILE,NDatUFLD;DatUvars
	GOTO NDatU1x
.with record locks . note file open must have had flag set
NDatU1AR
	trap	IOMssg giving Error if IO
	READLK	NDatUFILE,NDatUFLD;DatUvars
	GOTO NDatU1X
.with NO locks
NDatU1AN
	trap	IOMssg giving Error if IO
	READ	NDatUFILE,NDatUFLD;DatUvars
	GOTO NDatU1X
NDatU1B
	BRANCH	NDatUFLAG,NDatU1C
	CALL	NDatUOPEN
NDatU1C
	BRANCH	NDatULOCK,NDatU1Cl,NDatU1CR,NDatU1CN
NDatU1CL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFile
	READ	NDatUFile,NDatUFLD;DatUvars
	GOTO NDatU1X
.with record locks . note file open must have had flag set
NDatU1CR
	trap	IOMssg giving Error if IO
	READLK	NDatUFile,NDatUFLD;DatUvars
	GOTO NDatU1X
.with NO locks
NDatU1CN
	trap	IOMssg giving Error if IO
	READ	NDatUFile,NDatUFLD;DatUvars
	GOTO NDatU1X
NDatU1X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUTST
. REQUIRED    : NDatUFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NDatUTST
	BRANCH	NDatUPATH,NDatU2,NDatU2X,NDatU2B
NDatU2
	BRANCH	NDatUFLAG,NDatU2A
	CALL	NDatUOPEN
NDatU2A
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFILE
	READ	NDatUFILE,NDatUFLD;;
	GOTO NDatU2X
NDatU2B
	BRANCH	NDatUFLAG,NDatU2C
	CALL	NDatUOPEN
NDatU2C
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFle1
	READ	NDatUFle1,NDatUFLD1;;
NDatU2X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUKS
. REQUIRED    : 'NDatUPATH'
. RETURNED    : DATACARD
. DESCRIPTION : KEY SEQUENTIAL DATACARD FILE READ
.
NDatUKS
	BRANCH	NDatUPATH,NDatU3A,NDatU3X,NDatU3B
NDatU3
	BRANCH	NDatUFLAG,NDatU3A
	CALL	NDatUOPEN
NDatU3A
	BRANCH	NDatULOCK,NDatU3Al,NDatU3AR,NDatU3AL
NDatU3Al
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFILE
	READKS	NDatUFILE;DatUvars
	GOTO NDatU3X
NDatU3AR
	trap	IOMssg giving Error if IO
	READKSLK NDatUFILE;DatUvars
	GOTO NDatU3X
NDatU3AN
	trap	IOMssg giving Error if IO
	READKS	NDatUFILE;DatUvars
	GOTO NDatU3X
NDatU3B
	BRANCH	NDatUFLAG,NDatU3C
	CALL	NDatUOPEN
NDatU3C
	BRANCH	NDatULOCK,NDatU3CL,NDatU3CR,NDatU3CN
NDatU3CL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFile
	READKS	NDatUFile;DatUvars
	GOTO NDatU3X
NDatU3CR
	trap	IOMssg giving Error if IO
	READKSLK NDatUFile;DatUvars
	GOTO NDatU3X
NDatU3CN
	trap	IOMssg giving Error if IO
	READKS	NDatUFile;DatUvars
	GOTO NDatU3X
NDatU3X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUSEQ
. REQUIRED    :
. RETURNED    : DATACARD
. DESCRIPTION : SEQUENTIAL DATACARD FILE READ
.
NDatUSEQ
	BRANCH	NDatUFLAG,NDatU4
	CALL	NDatUOPEN
NDatU4
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFILE
	READ	NDatUFILE,SEQ;DatUvars
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUWRT
. REQUIRED    : 'NDatUFLD', 'NDatUPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., AIM INSERTS.
.
NDatUWRT
	BRANCH	NDatUFLAG,NDatU5
	CALL	NDatUOPEN
NDatU5
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFLIST
	WRITE	NDatUFLIST;DatUvars
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD
.
NDatUUPD
	BRANCH	NDatUFLAG,NDatU6
	CALL	NDatUOPEN
NDatU6
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFLIST
	UPDATE	NDatUFLIST;DatUvars
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUDEL
. REQUIRED    : 'NDatUPATH', 'NDatUFLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ISAM KEYS.
.
NDatUDEL
	BRANCH	NDatUFLAG,NDatU7
	CALL	NDatUOPEN
NDatU7
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFLIST
	DELETE	NDatUFLIST
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUKP
. REQUIRED    : 'NDatUPATH'
. RETURNED    : DATACARD DATa
. DESCRIPTION : KEY SEQUENTIAL prior DATACARD FILE READ
.
NDatUKP
	BRANCH	NDatUPATH,NDatU8,NDatU8X,NDatU8B
NDatU8
	BRANCH	NDatUFLAG,NDatU8A
	CALL	NDatUOPEN
NDatU8A
	BRANCH	NDatULOCK,NDatU8AL,NDatU8AR,NDatU8AN
NDatU8AL
	trap	IOMssg giving Error if IO
	FilePI	1;NDatUFile
	READKP	NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8AR
	trap	IOMssg giving Error if IO
	READKPLK NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8AN
	trap	IOMssg giving Error if IO
	READKP	NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8B
	BRANCH	NDatUFLAG,NDatU8C
	CALL	NDatUOPEN
NDatU8C
	BRANCH	NDatULOCK,NDatU8CL,NDatU8CR,NDatU8CN
NDatU8CL
	trap	IOMssg giving Error if IO
	FilePI	1;NDatUFile
	READKP	NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8CR
	trap	IOMssg giving Error if IO
	READKPlk NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8CN
	trap	IOMssg giving Error if IO
	READKP	NDatUFile;DatUvars
	GOTO NDatU8X
NDatU8X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUAIM
. REQUIRED    : NDatUFLD1,NDatUFLD2,NDatUFLD4,NDatUFLD5,NDatUFLD6,NDatUFLD7
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatUAIM
	BRANCH	NDatUFLAG,NDatU9
	CALL	NDatUOPEN
NDatU9
	BRANCH	NDatULOCK,NDatU9l,NDatU9R,NDatU9N
NDatU9L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFle1
	READ	NDatUFle1,NDatUFLD1;DatUvars
	trapclr	IO
	RETURN
NDatU9r
	trap	IOMssg giving Error if IO
	READLK	NDatUFle1,NDatUFLD1;DatUvars
	trapclr	IO
	RETURN
NDatU9N
	trap	IOMssg giving Error if IO
	READ	NDatUFle1,NDatUFLD1;DatUvars
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatUKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM KEY GENEREIC DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatUKG
	BRANCH	NDatUFLAG,NDatU10
	CALL	NDatUOPEN
NDatU10
	BRANCH	NDatULOCK,NDatU10L,NDatU10R,NDatU10N
NDatU10L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFle1
	READKG	NDatUFle1;DatUvars
	trapclr	IO
	RETURN
NDatU10R
	trap	IOMssg giving Error if IO
	READKGLK NDatUFle1;DatUvars
	trapclr	IO
	RETURN
NDatU10N
	trap	IOMssg giving Error if IO
	READKG	NDatUFle1;DatUvars
	trapclr	IO
	RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NDatUKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : KEY GENERIC PRIOR DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatUKGP
	BRANCH	NDatUFLAG,NDatU11
	CALL	NDatUOPEN
NDatU11
	BRANCH    NDatULOCK,NDatU11L,NDatU11R,NDatU11N
NDatU11L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatUFle1
	READKGP	NDatUFle1;DatUvars
	trapclr	IO
	RETURN
NDatU11R
	trap	IOMssg giving Error if IO
	READKGPLK NDatUFle1;DatUvars
	trapclr	IO
	RETURN
NDatU11N
	trap	IOMssg giving Error if IO
	READKGP	NDatUFle1;DatUvars
	trapclr	IO
	RETURN
.         XIF
...............................................................................
.
. ENTRY POINT : NDatUOPEN
. REQUIRED    : 'NDatUPATH', 'NDatUFLAG'
. RETURNED    : 'NDatUFLAG' SET TO THE PATH NUMBER: 'NDatUPATH', IF OPENED.
. DESCRIPTION : OPEN NIN DATACARD FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NDatUOPEN
	TRAP	NDatUGONE IF IO
	BRANCH	NDatULOCK,NDatUOPENl,NDatUOPENr
NDatUOPENl
	OPEN	NDatUFLIST
	TRAPCLR	IO
	MOVE	C1,NDatUFLAG
	RETURN
NDatUOPENr
	OPEN	NDatUFLIST,LOCKMANUAL,SINGLE
	TRAPCLR	IO
	MOVE	C1,NDatUFLAG
	RETURN
NDatUGONE
	MOVE	NDatUNAME,FILENAME
	CALL	FILEGONE

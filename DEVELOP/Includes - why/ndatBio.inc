..............................................................................
.
. NDatBIO INCLUSION
. NIN DATACARD FILE I/O ROUTINES
.
. FILE NAME : NINDatB
. REC LENGTH: 600 FIXED
. INDEX KEY : (1)  2-7       LIST#
. INDEX KEY : (2)  2-7,601-617       LIST# & Timestamp saved
. AIMDEX KEYS:(1) 58-132     LIST NAME
.             (2) 64-138     MLSTNAME
.             (3) 14-19      DATFUL
.	      (4)  1-1	     STATUS
.	      (5) 32-32	     NEW BYTE
.	      (6) 33-33      EXCLUSIVE BYTE

.
.
..............................................................................
.
. ENTRY POINT : NDatBKEY
. REQUIRED    : 'NDatBPATH', 'NDatBFLD'
. RETURNED    : DATACARD
. DESCRIPTION : EXACT ISAM KEY READ
.
NDatBKEY
	BRANCH	NDatBPATH,NDatB1,NDatB1X,NDatB1B
NDatB1
	BRANCH	NDatBFLAG,NDatB1A
	CALL	NDatBOPEN
NDatB1A
	BRANCH	NDatBLOCK,NDatB1Al,NDatB1AR,NDatB1AN
.with file locks
NDatB1Al
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFILE
	READ	NDatBFILE,NDatBFLD;DATVARS
	GOTO NDatB1x
.with record locks . note file open must have had flag set
NDatB1AR
	trap	IOMssg giving Error if IO
	READLK	NDatBFILE,NDatBFLD;DATVARS
	GOTO NDatB1X
.with NO locks
NDatB1AN
	trap	IOMssg giving Error if IO
	READ	NDatBFILE,NDatBFLD;DATVARS
	GOTO NDatB1X
NDatB1B
	BRANCH	NDatBFLAG,NDatB1C
	CALL	NDatBOPEN
NDatB1C
	BRANCH	NDatBLOCK,NDatB1Cl,NDatB1CR,NDatB1CN
NDatB1CL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE3
	READ	NDatBFLE3,NDatBFLD3;DATVARS
	GOTO NDatB1X
.with record locks . note file open must have had flag set
NDatB1CR
	trap	IOMssg giving Error if IO
	READLK	NDatBFLE3,NDatBFLD3;DATVARS
	GOTO NDatB1X
.with NO locks
NDatB1CN
	trap	IOMssg giving Error if IO
	READ	NDatBFLE3,NDatBFLD3;DATVARS
	GOTO NDatB1X
NDatB1X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBTST
. REQUIRED    : NDatBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NDatBTST
	BRANCH	NDatBPATH,NDatB2,NDatB2X,NDatB2B
NDatB2
	BRANCH	NDatBFLAG,NDatB2A
	CALL	NDatBOPEN
NDatB2A
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFILE
	READ	NDatBFILE,NDatBFLD;;
	GOTO NDatB2X
NDatB2B
	BRANCH	NDatBFLAG,NDatB2C
	CALL	NDatBOPEN
NDatB2C
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE3
	READ	NDatBFLE3,NDatBFLD3;;
NDatB2X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBKS
. REQUIRED    : 'NDatBPATH'
. RETURNED    : DATACARD
. DESCRIPTION : KEY SEQUENTIAL DATACARD FILE READ
.
NDatBKS
	BRANCH	NDatBPATH,NDatB3A,NDatB3X,NDatB3B
NDatB3
	BRANCH	NDatBFLAG,NDatB3A
	CALL	NDatBOPEN
NDatB3A
	BRANCH	NDatBLOCK,NDatB3Al,NDatB3AR,NDatB3AL
NDatB3Al
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFILE
	READKS	NDatBFILE;DATVARS
	GOTO NDatB3X
NDatB3AR
	trap	IOMssg giving Error if IO
	READKSLK NDatBFILE;DATVARS
	GOTO NDatB3X
NDatB3AN
	trap	IOMssg giving Error if IO
	READKS	NDatBFILE;DATVARS
	GOTO NDatB3X
NDatB3B
	BRANCH	NDatBFLAG,NDatB3C
	CALL	NDatBOPEN
NDatB3C
	BRANCH	NDatBLOCK,NDatB3CL,NDatB3CR,NDatB3CN
NDatB3CL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE3
	READKS	NDatBFLE3;DATVARS
	GOTO NDatB3X
NDatB3CR
	trap	IOMssg giving Error if IO
	READKSLK NDatBFLE3;DATVARS
	GOTO NDatB3X
NDatB3CN
	trap	IOMssg giving Error if IO
	READKS	NDatBFLE3;DATVARS
	GOTO NDatB3X
NDatB3X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBSEQ
. REQUIRED    :
. RETURNED    : DATACARD
. DESCRIPTION : SEQUENTIAL DATACARD FILE READ
.
NDatBSEQ
	BRANCH	NDatBFLAG,NDatB4
	CALL	NDatBOPEN
NDatB4
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFILE
	READ	NDatBFILE,SEQ;DATVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBWRT
. REQUIRED    : 'NDatBFLD', 'NDatBPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., AIM INSERTS.
.
NDatBWRT
	BRANCH	NDatBFLAG,NDatB5
	CALL	NDatBOPEN
NDatB5
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLIST
	WRITE	NDatBFLIST;DATVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD
.
NDatBUPD
	BRANCH	NDatBFLAG,NDatB6
	CALL	NDatBOPEN
NDatB6
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLIST
	UPDATE	NDatBFLIST;DATVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBDEL
. REQUIRED    : 'NDatBPATH', 'NDatBFLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ISAM KEYS.
.
NDatBDEL
	BRANCH	NDatBFLAG,NDatB7
	CALL	NDatBOPEN
NDatB7
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLIST
	DELETE	NDatBFLIST
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBKP
. REQUIRED    : 'NDatBPATH'
. RETURNED    : DATACARD DATa
. DESCRIPTION : KEY SEQUENTIAL prior DATACARD FILE READ
.
NDatBKP
	BRANCH	NDatBPATH,NDatB8,NDatB8X,NDatB8B
NDatB8
	BRANCH	NDatBFLAG,NDatB8A
	CALL	NDatBOPEN
NDatB8A
	BRANCH	NDatBLOCK,NDatB8AL,NDatB8AR,NDatB8AN
NDatB8AL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFILE
	READKP	NDatBFILE;DATVARS
	GOTO NDatB8X
NDatB8AR
	trap	IOMssg giving Error if IO
	READKPLK NDatBFILE;DATVARS
	GOTO NDatB8X
NDatB8AN
	trap	IOMssg giving Error if IO
	READKP	NDatBFILE;DATVARS
	GOTO NDatB8X
NDatB8B
	BRANCH	NDatBFLAG,NDatB8C
	CALL	NDatBOPEN
NDatB8C
	BRANCH	NDatBLOCK,NDatB8CL,NDatB8CR,NDatB8CN
NDatB8CL
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE3
	READKP	NDatBFLE3;DATVARS
	GOTO NDatB8X
NDatB8CR
	trap	IOMssg giving Error if IO
	READKPlk NDatBFLE3;DATVARS
	GOTO NDatB8X
NDatB8CN
	trap	IOMssg giving Error if IO
	READKP	NDatBFLE3;DATVARS
	GOTO NDatB8X
NDatB8X
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBAIM
. REQUIRED    : NDatBFLD1,NDatBFLD2,NDatBFLD4,NDatBFLD5,NDatBFLD6,NDatBFLD7
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatBAIM
	BRANCH	NDatBFLAG,NDatB9
	CALL	NDatBOPEN
NDatB9
	BRANCH	NDatBLOCK,NDatB9l,NDatB9R,NDatB9N
NDatB9L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE2
	READ	NDatBFLE2,NDatBFLD1,NDatBFLD2,NDatBFLD4,NDatBFLD5,NDatBFLD6,NDatBFLD7;DATVARS
	trapclr	IO
	RETURN
NDatB9r
	trap	IOMssg giving Error if IO
	READLK	NDatBFLE2,NDatBFLD1,NDatBFLD2,NDatBFLD4,NDatBFLD5,NDatBFLD6,NDatBFLD7;DATVARS
	trapclr	IO
	RETURN
NDatB9N
	trap	IOMssg giving Error if IO
	READ	NDatBFLE2,NDatBFLD1,NDatBFLD2,NDatBFLD4,NDatBFLD5,NDatBFLD6,NDatBFLD7;DATVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : NDatBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM KEY GENEREIC DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatBKG
	BRANCH	NDatBFLAG,NDatB10
	CALL	NDatBOPEN
NDatB10
	BRANCH	NDatBLOCK,NDatB10L,NDatB10R,NDatB10N
NDatB10L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE2
	READKG	NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
NDatB10R
	trap	IOMssg giving Error if IO
	READKGLK NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
NDatB10N
	trap	IOMssg giving Error if IO
	READKG	NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NDatBKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : KEY GENERIC PRIOR DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NDatBKGP
	BRANCH	NDatBFLAG,NDatB11
	CALL	NDatBOPEN
NDatB11
	BRANCH    NDatBLOCK,NDatB11L,NDatB11R,NDatB11N
NDatB11L
	trap	IOMssg giving Error if IO
	FILEPI	1;NDatBFLE2
	READKGP	NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
NDatB11R
	trap	IOMssg giving Error if IO
	READKGPLK NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
NDatB11N
	trap	IOMssg giving Error if IO
	READKGP	NDatBFLE2;DATVARS
	trapclr	IO
	RETURN
.         XIF
..............................................................................
.
. ENTRY POINT : NDatBOPEN
. REQUIRED    : 'NDatBPATH', 'NDatBFLAG'
. RETURNED    : 'NDatBFLAG' SET TO THE PATH NUMBER: 'NDatBPATH', IF OPENED.
. DESCRIPTION : OPEN NIN DATACARD FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NDatBOPEN
	LOAD	NDatBNAME USING NDatBPATH FROM NDatBNME1,NDatBNME2
	TRAP	NDatBGONE IF IO
	BRANCH	NDatBLOCK,NDatBOPENl,NDatBOPENr
NDatBOPENl
	OPEN	NDatBFLIST
	TRAPCLR	IO
	MOVE	C1,NDatBFLAG
	RETURN
NDatBOPENr
	OPEN	NDatBFLIST,LOCKMANUAL,SINGLE
	TRAPCLR	IO
	MOVE	C1,NDatBFLAG
	RETURN
NDatBGONE
	MOVE	NDatBNAME,FILENAME
	CALL	FILEGONE
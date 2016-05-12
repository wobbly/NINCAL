..............................................................................
.
. 
. NIN datacard / Min CNT info FILE I/O ROUTINES
.
.
.  FILE NAME : NINdatCnt
. REC LENGTH: 120 FIXED
..............................................................................
.
. ENTRY POINT : NdatCntKEY
. REQUIRED    : 'NdatCntFLD'
. RETURNED    : OWNER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NdatCntKEY  	BRANCH    NdatCntFlag TO NdatCnt1
         	CALL      NdatCntOPEN
NdatCnt1    	FILEPI    1;NdatCntFLE1
         	READ      NdatCntFLE1,NdatCntFLD;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntTST
. REQUIRED    : NdatCntFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NdatCntTST  	BRANCH    NdatCntFlag TO NdatCnt2
         	CALL      NdatCntOPEN
NdatCnt2    	FILEPI    1;NdatCntFLE1
         	READ      NdatCntFLE1,NdatCntFLD;STR1
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntKS
. REQUIRED    :
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY SEQUENTIAL OWNER FILE READ
.
NdatCntKS   	BRANCH    NdatCntFlag TO NdatCnt3
         	CALL      NdatCntOPEN
NdatCnt3    	FILEPI    1;NdatCntFLE1
         	READKS    NdatCntFLE1;NDatCntvars
         	RETURN
..............................................................................
. ENTRY POINT : NdatCntSEQ
. REQUIRED    :
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NdatCntSEQ
	BRANCH    NdatCntFlag TO NdatCnt4
         	CALL      NdatCntOPEN
NdatCnt4   	FILEPI    1;NdatCntFLE1
         	READ      NdatCntFLE1,SEQ;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntWRT
. REQUIRED    : 'NdatCntFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NdatCntWRT  	BRANCH    NdatCntFlag TO NdatCnt5
         	CALL      NdatCntOPEN
NdatCnt5    	FILEPI    1;NdatCntFLE1
         	WRITE     NDatCntFList;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE OWNER FILE
.
NdatCntUPD  	BRANCH    NdatCntFlag TO NdatCnt6
         	CALL      NdatCntOPEN
NdatCnt6
         	UPDATE    NDatCntFList;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntDEL
. REQUIRED    : 'NdatCntFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NdatCntDEL  	BRANCH    NdatCntFlag TO NdatCnt7
         	CALL      NdatCntOPEN
NdatCnt7    	FILEPI    1;NdatCntFLE1
         	DELETE    NDatCntFList
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntAIM
. REQUIRED    : NdatCntFLD2
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM OWNER FILE READ
.               APPLICA TION'S RESPONSIBILITY TO TEST FLGS
.
NdatCntAIM  	BRANCH    NdatCntFlag TO NdatCnt8
         	CALL      NdatCntOpen
NdatCnt8    	FILEPI    1;NdatCntFLE2
         	READ      NdatCntFLE2,NdatCntFLD2;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM KEY GENEREIC OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NdatCntKG   	BRANCH    NdatCntFlag TO NdatCnt10
         	CALL      NdatCntOpen
NdatCnt10   	FILEPI    1;NdatCntFLE2
         	READKG    NdatCntFLE2;NDatCntvars
         	RETURN
..............................................................................
.
. ENTRY POINT : NdatCntKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY GENERIC PRIOR OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NdatCntKGP  	BRANCH    NdatCntFlag TO NdatCnt11
         	CALL      NdatCntOpen
NdatCnt11   	FILEPI    1;NdatCntFLE2
         	READKGP   NdatCntFLE2;NDatCntvars
         	RETURN
...............................................................................
. 
. ENTRY POINT : NdatCntOPEN
. REQUIRED    : 'NdatCntFlag' 'NdatCntPATH'
. RETURNED    : 'NdatCntFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OWNER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NdatCntOPEN 	
	TRAP      NdatCntGONE giving error IF IO
         	OPEN      NDatCntFList
         	TRAPCLR   IO
         	MOVE      C1 TO NdatCntFlag
         	RETURN
..............................................................................
NdatCntGONE 	MOVE      NdatCntNAME TO FILENAME
         	CALL      FILEGONE
.
..............................................................................


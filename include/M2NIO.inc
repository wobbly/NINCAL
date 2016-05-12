..............................................................................
.******************************************************
.*  M2N List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* M2N DATACARD FILES.
.* ****************************************************
.
.      FILE:      MIN2NIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
...................
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing M2N, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : M2NKEY
. REQUIRED    : 'M2NFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M2NKEY  	Branch	M2nPath,M2n1a,M2n1c
M2n1a	BRANCH    M2NFlag TO M2N1B
	CALL      M2NOpen
M2N1B    	FILEPI    1;M2NFile
        	READ      M2NFile,M2NFld;M2NVars	
        	RETURN
M2n1C	BRANCH    M2NFlag TO M2N1D
	CALL      M2NOpen
M2N1D    	FILEPI    1;M2NFile2
        	READ      M2NFile2,M2NFld2;M2NVars	
        	RETURN
..............................................................................
.
. ENTRY POINT : M2NTST
. REQUIRED    : M2NFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
M2NTST   	Branch	M2nPath,M2n2a,M2N2C
M2n2a	BRANCH    M2NFlag TO M2N2B
         	CALL      M2NOpen
M2N2B    	FILEPI    1;M2NFile
        	READ      M2NFile,M2NFld;STR1
         	RETURN
M2n2c	BRANCH    M2NFlag TO M2N2D
         	CALL      M2NOpen
M2N2d    	FILEPI    1;M2NFile2
        	READ      M2NFile2,M2NFld2;STR1
         	RETURN
..............................................................................
.
. ENTRY POINT : M2NKS
. REQUIRED    :
. RETURNED    : M2N Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
M2NKS   	Branch	M2NPath,M2N3a,M2N3C
M2N3A	BRANCH    M2NFlag TO M2N3B
         	CALL      M2NOpen
M2N3B    	FILEPI    1;M2NFile
         	READKS    M2NFile;M2NVars	
         	RETURN
M2N3C	BRANCH    M2NFlag TO M2N3D
         	CALL      M2NOpen
M2N3D    	FILEPI    1;M2NFile2
         	READKS    M2NFile2;M2NVars	
         	RETURN
..............................................................................
. ENTRY POINT : M2NSEQ
. REQUIRED    :
. RETURNED    : M2N Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
M2NSEQ  	   BRANCH    M2NFlag TO M2N4
         	CALL      M2NOpen
M2N4   	FILEPI    1;M2NFile
        	 READ      M2NFile,SEQ;M2NVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : M2NWRT
. REQUIRED    : 'M2NFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
M2NWRT  BRANCH    M2NFlag TO M2N5
         CALL      M2NOpen
M2N5    FILEPI    1;M2NFlist
         WRITE     M2NFlist;M2NVars	
         RETURN
..............................................................................
.
. ENTRY POINT : M2NUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
M2NUPD  BRANCH    M2NFlag TO M2N6
         CALL      M2NOpen
M2N6
   	FILEPI    1;M2NFlist
         UPDATE    M2NFlist;M2NVars	
         RETURN
..............................................................................
.
. ENTRY POINT : M2NDEL
. REQUIRED    : 'M2NFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
M2NDEL  	BRANCH    M2NFlag TO M2N7
         	CALL      M2NOpen
M2N7    	FILEPI    1;M2NFlist
	DELETE    M2NFList
         	RETURN
..............................................................................
. 
. ENTRY POINT : M2NOpen
. REQUIRED    : 'M2NFlag' 'M2NPATH'
. RETURNED    : 'M2NFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
M2NOpen	TRAP      M2NGONE giving error IF IO
         	OPEN      M2NFList
         	TRAPCLR   IO
         	MOVE      C1 TO M2NFlag
         	RETURN
..............................................................................
M2NGONE  MOVE      M2NNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


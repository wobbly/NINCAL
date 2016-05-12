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
. ENTRY POINT : M2NLoKEY
. REQUIRED    : 'M2NLoFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M2NLoKEY  	Branch	M2NLoPath,M2NLo1a,M2NLo1c
M2NLo1a	BRANCH    M2NLoFlag TO M2NLo1B
	CALL      M2NLoOpen
M2NLo1B    	FILEPI    1;M2NLoFile
        	READ      M2NLoFile,M2NLoFld;M2NLoVars	
        	RETURN
M2NLo1C	BRANCH    M2NLoFlag TO M2NLo1D
	CALL      M2NLoOpen
M2NLo1D    	FILEPI    1;M2NLoFile2
        	READ      M2NLoFile2,M2NLoFld2;M2NLoVars	
        	RETURN
..............................................................................
.
. ENTRY POINT : M2NLoTST
. REQUIRED    : M2NLoFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
M2NLoTST   	Branch	M2NLoPath,M2NLo2a,M2NLo2C
M2NLo2a	BRANCH    M2NLoFlag TO M2NLo2B
         	CALL      M2NLoOpen
M2NLo2B    	FILEPI    1;M2NLoFile
        	READ      M2NLoFile,M2NLoFld;STR1
         	RETURN
M2NLo2c	BRANCH    M2NLoFlag TO M2NLo2D
         	CALL      M2NLoOpen
M2NLo2d    	FILEPI    1;M2NLoFile2
        	READ      M2NLoFile2,M2NLoFld2;STR1
         	RETURN
..............................................................................
.
. ENTRY POINT : M2NLoKS
. REQUIRED    :
. RETURNED    : M2NLo Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
M2NLoKS   	Branch	M2NLoPath,M2NLo3a,M2NLo3C
M2NLo3A	BRANCH    M2NLoFlag TO M2NLo3B
         	CALL      M2NLoOpen
M2NLo3B    	FILEPI    1;M2NLoFile
         	READKS    M2NLoFile;M2NLoVars	
         	RETURN
M2NLo3C	BRANCH    M2NLoFlag TO M2NLo3D
         	CALL      M2NLoOpen
M2NLo3D    	FILEPI    1;M2NLoFile2
         	READKS    M2NLoFile2;M2NLoVars	
         	RETURN
..............................................................................
. ENTRY POINT : M2NLoSEQ
. REQUIRED    :
. RETURNED    : M2NLo Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
M2NLoSEQ  	   BRANCH    M2NLoFlag TO M2NLo4
         	CALL      M2NLoOpen
M2NLo4   	FILEPI    1;M2NLoFile
        	 READ      M2NLoFile,SEQ;M2NLoVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : M2NLoWRT
. REQUIRED    : 'M2NLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
M2NLoWRT  BRANCH    M2NLoFlag TO M2NLo5
         CALL      M2NLoOpen
M2NLo5    FILEPI    1;M2NLoFlist
         WRITE     M2NLoFlist;M2NLoVars	
         RETURN
..............................................................................
.
. ENTRY POINT : M2NLoUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
M2NLoUPD  BRANCH    M2NLoFlag TO M2NLo6
         CALL      M2NLoOpen
M2NLo6
   	FILEPI    1;M2NLoFlist
         UPDATE    M2NLoFlist;M2NLoVars	
         RETURN
..............................................................................
.
. ENTRY POINT : M2NLoDEL
. REQUIRED    : 'M2NLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
M2NLoDEL  	BRANCH    M2NLoFlag TO M2NLo7
         	CALL      M2NLoOpen
M2NLo7    	FILEPI    1;M2NLoFlist
	DELETE    M2NLoFList
         	RETURN
..............................................................................
. 
. ENTRY POINT : M2NLoOpen
. REQUIRED    : 'M2NLoFlag' 'M2NLoPATH'
. RETURNED    : 'M2NLoFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
M2NLoOpen	TRAP      M2NLoGONE giving error IF IO
         	OPEN      M2NLoFList
         	TRAPCLR   IO
         	MOVE      C1 TO M2NLoFlag
         	RETURN
..............................................................................
M2NLoGONE  MOVE      M2NLoNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


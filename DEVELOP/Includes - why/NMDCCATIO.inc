..............................................................................
.******************************************************
.*  NMDCC List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* NMDCC DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_035
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam 
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing NMDCC, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : NMDCCKEY
. REQUIRED    : 'NMDCCFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMDCCKEY  BRANCH    NMDCCFlag TO NMDCC1
         CALL      NMDCCOpen
NMDCC1    FILEPI    1;NMDCCFile
         READ      NMDCCFile,NMDCCFld;NMDCCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : NMDCCTST
. REQUIRED    : NMDCCFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMDCCTST    	BRANCH    NMDCCFlag TO NMDCC2
         CALL      NMDCCOpen
NMDCC2    FILEPI    1;NMDCCFile
         READ      NMDCCFile,NMDCCFld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NMDCCKS
. REQUIRED    :
. RETURNED    : NMDCC Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
NMDCCKS   BRANCH    NMDCCFlag TO NMDCC3
         CALL      NMDCCOpen
NMDCC3    FILEPI    1;NMDCCFile
         READKS    NMDCCFile;NMDCCVars	
         RETURN
..............................................................................
. ENTRY POINT : NMDCCSEQ
. REQUIRED    :
. RETURNED    : NMDCC Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NMDCCSEQ  	   BRANCH    NMDCCFlag TO NMDCC4
         	CALL      NMDCCOpen
NMDCC4   	FILEPI    1;NMDCCFile
        	 READ      NMDCCFile,SEQ;NMDCCVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : NMDCCWRT
. REQUIRED    : 'NMDCCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMDCCWRT  BRANCH    NMDCCFlag TO NMDCC5
         CALL      NMDCCOpen
NMDCC5    FILEPI    1;NMDCCFlist
         WRITE     NMDCCFLIST;NMDCCVars
         RETURN
..............................................................................
.
. ENTRY POINT : NMDCCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
NMDCCUPD  BRANCH    NMDCCFlag TO NMDCC6
         CALL      NMDCCOpen
NMDCC6
   	FILEPI    1;NMDCCFList
         UPDATE    NMDCCFList;NMDCCVars
         RETURN
..............................................................................
.
. ENTRY POINT : NMDCCDEL
. REQUIRED    : 'NMDCCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMDCCDEL  BRANCH    NMDCCFlag TO NMDCC7
         CALL      NMDCCOpen
NMDCC7    FILEPI    1;NMDCCFList
         DELETE    NMDCCFList
         RETURN
..............................................................................
. 
. ENTRY POINT : NMDCCOpen
. REQUIRED    : 'NMDCCFlag' 'NMDCCPATH'
. RETURNED    : 'NMDCCFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMDCCOpen	TRAP      NMDCCGONE giving error IF IO
         	OPEN      NMDCCFList
         	TRAPCLR   IO
         	MOVE      C1 TO NMDCCFlag
         	RETURN
..............................................................................
NMDCCGONE MOVE      NMDCCNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


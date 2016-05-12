..............................................................................
.******************************************************
.*  MISC List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* NMSC DATACARD FILES.
.* ****************************************************
.
.      FILE:      
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam 
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing NMSC, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : NMSCKEY
. REQUIRED    : 'NMSCFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMSCKEY  BRANCH    NMSCFlag TO NMSC1
         CALL      NMSCOpen
NMSC1    FILEPI    1;NMSCFile
         READ      NMSCFile,NMSCFld;NMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : NMSCTST
. REQUIRED    : NMSCFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMSCTST    	BRANCH    NMSCFlag TO NMSC2
         CALL      NMSCOpen
NMSC2    FILEPI    1;NMSCFile
         READ      NMSCFile,NMSCFld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NMSCKS
. REQUIRED    :
. RETURNED    : NMSC Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
NMSCKS   BRANCH    NMSCFlag TO NMSC3
         CALL      NMSCOpen
NMSC3    FILEPI    1;NMSCFile
         READKS    NMSCFile;NMSCVars	
         RETURN
..............................................................................
. ENTRY POINT : NMSCSEQ
. REQUIRED    :
. RETURNED    : NMSC Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NMSCSEQ  	   BRANCH    NMSCFlag TO NMSC4
         	CALL      NMSCOpen
NMSC4   	FILEPI    1;NMSCFile
        	 READ      NMSCFile,SEQ;NMSCVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : NMSCWRT
. REQUIRED    : 'NMSCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMSCWRT  BRANCH    NMSCFlag TO NMSC5
         CALL      NMSCOpen
NMSC5    FILEPI    1;NMSCFile
         WRITE     NMSCFile,NMSCFld;NMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : NMSCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
NMSCUPD  BRANCH    NMSCFlag TO NMSC6
         CALL      NMSCOpen
NMSC6
   	FILEPI    1;NMSCFile
         UPDATE    NMSCFile;NMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : NMSCDEL
. REQUIRED    : 'NMSCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMSCDEL  BRANCH    NMSCFlag TO NMSC7
         CALL      NMSCOpen
NMSC7    FILEPI    1;NMSCFile
         DELETE    NMSCFile,NMSCFld
         RETURN
..............................................................................
. 
. ENTRY POINT : NMSCOpen
. REQUIRED    : 'NMSCFlag' 'NMSCPATH'
. RETURNED    : 'NMSCFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMSCOpen	TRAP      NMSCGONE giving error IF IO
         	OPEN      NMSCFile,NMSCName
         	TRAPCLR   IO
         	MOVE      C1 TO NMSCFlag
         	RETURN
..............................................................................
NMSCGONE MOVE      NMSCNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


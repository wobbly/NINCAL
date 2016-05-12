..............................................................................
.******************************************************
.*  MISC List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MMSC DATACARD FILES.
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
.MDSMSCDD - Misc addressing MMSC, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : MMSCKEY
. REQUIRED    : 'MMSCFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MMSCKEY  BRANCH    MMSCFlag TO MMSC1
         CALL      MMSCOpen
MMSC1    FILEPI    1;MMSCFile
         READ      MMSCFile,MMSCFld;MMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MMSCTST
. REQUIRED    : MMSCFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MMSCTST    	BRANCH    MMSCFlag TO MMSC2
         CALL      MMSCOpen
MMSC2    FILEPI    1;MMSCFile
         READ      MMSCFile,MMSCFld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : MMSCKS
. REQUIRED    :
. RETURNED    : MMSC Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
MMSCKS   BRANCH    MMSCFlag TO MMSC3
         CALL      MMSCOpen
MMSC3    FILEPI    1;MMSCFile
         READKS    MMSCFile;MMSCVars	
         RETURN
..............................................................................
. ENTRY POINT : MMSCSEQ
. REQUIRED    :
. RETURNED    : MMSC Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
MMSCSEQ  	   BRANCH    MMSCFlag TO MMSC4
         	CALL      MMSCOpen
MMSC4   	FILEPI    1;MMSCFile
        	 READ      MMSCFile,SEQ;MMSCVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : MMSCWRT
. REQUIRED    : 'MMSCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
MMSCWRT  BRANCH    MMSCFlag TO MMSC5
         CALL      MMSCOpen
MMSC5    FILEPI    1;MMSCFile
         WRITE     MMSCFile,MMSCFld;MMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MMSCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
MMSCUPD  BRANCH    MMSCFlag TO MMSC6
         CALL      MMSCOpen
MMSC6
   	FILEPI    1;MMSCFile
         UPDATE    MMSCFile;MMSCVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MMSCDEL
. REQUIRED    : 'MMSCFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
MMSCDEL  BRANCH    MMSCFlag TO MMSC7
         CALL      MMSCOpen
MMSC7    FILEPI    1;MMSCFile
         DELETE    MMSCFile,MMSCFld
         RETURN
..............................................................................
. 
. ENTRY POINT : MMSCOpen
. REQUIRED    : 'MMSCFlag' 'MMSCPATH'
. RETURNED    : 'MMSCFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
MMSCOpen	TRAP      MMSCGONE giving error IF IO
         	OPEN      MMSCFile,MMSCName
         	TRAPCLR   IO
         	MOVE      C1 TO MMSCFlag
         	RETURN
..............................................................................
MMSCGONE MOVE      MMSCNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


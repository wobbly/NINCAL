..............................................................................
.******************************************************
.*  M060 List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* M060 DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_Seg
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam 
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing M060, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC060DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : M060KEY
. REQUIRED    : 'M060Fld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M060KEY  BRANCH    M060Flag TO M0601
         CALL      M060Open
M0601    FILEPI    1;M060File
         READ      M060File,M060Fld;M060Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M060TST
. REQUIRED    : M060Fld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
M060TST    	BRANCH    M060Flag TO M0602
         CALL      M060Open
M0602    FILEPI    1;M060File
         READ      M060File,M060Fld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : M060KS
. REQUIRED    :
. RETURNED    : M060 Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
M060KS   BRANCH    M060Flag TO M0603
         CALL      M060Open
M0603    FILEPI    1;M060File
         READKS    M060File;M060Vars	
         RETURN
..............................................................................
. ENTRY POINT : M060SEQ
. REQUIRED    :
. RETURNED    : M060 Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
M060SEQ  	   BRANCH    M060Flag TO M0604
         	CALL      M060Open
M0604   	FILEPI    1;M060File
        	 READ      M060File,SEQ;M060Vars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : M060WRT
. REQUIRED    : 'M060Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
M060WRT  BRANCH    M060Flag TO M0605
         CALL      M060Open
M0605    FILEPI    1;M060File
         WRITE     M060File,M060Fld;M060Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M060UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
M060UPD  BRANCH    M060Flag TO M0606
         CALL      M060Open
M0606
   	FILEPI    1;M060File
         UPDATE    M060File;M060Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M060DEL
. REQUIRED    : 'M060Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
M060DEL  BRANCH    M060Flag TO M0607
         CALL      M060Open
M0607    FILEPI    1;M060File
         DELETE    M060File,M060Fld
         RETURN
..............................................................................
. 
. ENTRY POINT : M060Open
. REQUIRED    : 'M060Flag' 'M060PATH'
. RETURNED    : 'M060Flag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
M060Open	TRAP      M060GONE giving error IF IO
         	OPEN      M060File,M060Name
         	TRAPCLR   IO
         	MOVE      C1 TO M060Flag
         	RETURN
..............................................................................
M060GONE MOVE      M060NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


..............................................................................
.******************************************************
.*  M035 List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* M035 DATACARD FILES.
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
.MDSMSCDD - Misc addressing M035, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : M035KEY
. REQUIRED    : 'M035Fld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M035KEY  BRANCH    M035Flag TO M0351
         CALL      M035Open
M0351    FILEPI    1;M035File
         READ      M035File,M035Fld;M035Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M035TST
. REQUIRED    : M035Fld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
M035TST    	BRANCH    M035Flag TO M0352
         CALL      M035Open
M0352    FILEPI    1;M035File
         READ      M035File,M035Fld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : M035KS
. REQUIRED    :
. RETURNED    : M035 Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
M035KS   BRANCH    M035Flag TO M0353
         CALL      M035Open
M0353    FILEPI    1;M035File
         READKS    M035File;M035Vars	
         RETURN
..............................................................................
. ENTRY POINT : M035SEQ
. REQUIRED    :
. RETURNED    : M035 Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
M035SEQ  	   BRANCH    M035Flag TO M0354
         	CALL      M035Open
M0354   	FILEPI    1;M035File
        	 READ      M035File,SEQ;M035Vars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : M035WRT
. REQUIRED    : 'M035Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
M035WRT  BRANCH    M035Flag TO M0355
         CALL      M035Open
M0355    FILEPI    1;M035Flist
         WRITE     M035FLIST;M035Vars
         RETURN
..............................................................................
.
. ENTRY POINT : M035UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
M035UPD  BRANCH    M035Flag TO M0356
         CALL      M035Open
M0356
   	FILEPI    1;M035FList
         UPDATE    M035FList;M035Vars
         RETURN
..............................................................................
.
. ENTRY POINT : M035DEL
. REQUIRED    : 'M035Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
M035DEL  BRANCH    M035Flag TO M0357
         CALL      M035Open
M0357    FILEPI    1;M035FList
         DELETE    M035FList
         RETURN
..............................................................................
. 
. ENTRY POINT : M035Open
. REQUIRED    : 'M035Flag' 'M035PATH'
. RETURNED    : 'M035Flag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
M035Open	TRAP      M035GONE giving error IF IO
         	OPEN      M035FList
         	TRAPCLR   IO
         	MOVE      C1 TO M035Flag
         	RETURN
..............................................................................
M035GONE MOVE      M035NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


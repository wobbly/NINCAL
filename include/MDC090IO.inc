..............................................................................
.******************************************************
.*  M090 List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* M090 DATACARD FILES.
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
.MDSMSCDD - Misc addressing M090, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : M090KEY
. REQUIRED    : 'M090Fld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M090KEY  BRANCH    M090Flag TO M0901
         CALL      M090Open
M0901    FILEPI    1;M090File
         READ      M090File,M090Fld;M090Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M090TST
. REQUIRED    : M090Fld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
M090TST    	BRANCH    M090Flag TO M0902
         CALL      M090Open
M0902    FILEPI    1;M090File
         READ      M090File,M090Fld;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : M090KS
. REQUIRED    :
. RETURNED    : M090 Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
M090KS   BRANCH    M090Flag TO M0903
         CALL      M090Open
M0903    FILEPI    1;M090File
         READKS    M090File;M090Vars	
         RETURN
..............................................................................
. ENTRY POINT : M090SEQ
. REQUIRED    :
. RETURNED    : M090 Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
M090SEQ  	   BRANCH    M090Flag TO M0904
         	CALL      M090Open
M0904   	FILEPI    1;M090File
        	 READ      M090File,SEQ;M090Vars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : M090WRT
. REQUIRED    : 'M090Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
M090WRT  BRANCH    M090Flag TO M0905
         CALL      M090Open
M0905    FILEPI    1;M090File
         WRITE     M090Flist;M090Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M090UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
M090UPD  BRANCH    M090Flag TO M0906
         CALL      M090Open
M0906
   	FILEPI    1;M090File
         UPDATE    M090Flist;M090Vars	
         RETURN
..............................................................................
.
. ENTRY POINT : M090DEL
. REQUIRED    : 'M090Fld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
M090DEL  BRANCH    M090Flag TO M0907
         CALL      M090Open
M0907    FILEPI    1;M090File
         DELETE    M090Flist
         RETURN
..............................................................................
.
. ENTRY POINT : M090AIM
. REQUIRED    : M090FLD2
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM OWNER FILE READ
.               APPLICA TION'S RESPONSIBILITY TO TEST FLGS
.
M090AIM  BRANCH    M090Flag TO M0908
         CALL      M090Open
M0908    FILEPI    1;M090File2
         READ      M090File2,M090FLD2;M090vars
         RETURN
..............................................................................
.
. ENTRY POINT : M090KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM KEY GENEREIC OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M090KG   BRANCH    M090Flag TO M09010
         CALL      M090Open
M09010   FILEPI    1;M090File2
         READKG    M090File2;M090vars
         RETURN
..............................................................................
.
. ENTRY POINT : M090KGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY GENERIC PRIOR OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
M090KGP  BRANCH    M090Flag TO M09011
         CALL      M090Open
M09011   FILEPI    1;M090File2
         READKGP   M090File2;M090vars
         RETURN
..............................................................................
. 
. ENTRY POINT : M090Open
. REQUIRED    : 'M090Flag' 'M090PATH'
. RETURNED    : 'M090Flag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
M090Open	TRAP      M090GONE giving error IF IO
         	OPEN      M090FList
         	TRAPCLR   IO
         	MOVE      C1 TO M090Flag
         	RETURN
..............................................................................
M090GONE MOVE      M090NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


..............................................................................
.******************************************************
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_MAIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam & AAM
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : MInKEY
. REQUIRED    : 'MInFLD'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MInKEY  BRANCH    MInFlag TO MIn1
         CALL      MInOPEN
MIn1    FILEPI    1;MInFile
         READ      MInFile,MInFLD;MinMainVARS	
         RETURN
..............................................................................
.
. ENTRY POINT : MInTST
. REQUIRED    : MInFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MInTST  BRANCH    MInFlag TO MIn2
         CALL      MInOPEN
MIn2    FILEPI    1;MInFile
         READ      MInFile,MInFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : MInKS
. REQUIRED    :
. RETURNED    : Min Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
MInKS   BRANCH    MInFlag TO MIn3
         CALL      MInOPEN
MIn3    FILEPI    1;MInFile
         READKS    MInFile;MinMainVARS	
         RETURN
..............................................................................
. ENTRY POINT : MInSEQ
. REQUIRED    :
. RETURNED    : Min Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
MInSEQ  	   BRANCH    MInFlag TO MIn4
         	CALL      MInOPEN
MIn4   	FILEPI    1;MInFile
        	 READ      MInFile,SEQ;MinMainVARS	
        	 RETURN
..............................................................................
.
. ENTRY POINT : MInWRT
. REQUIRED    : 'MInFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
MInWRT  BRANCH    MInFlag TO MIn5
         CALL      MInOPEN
MIn5    FILEPI    1;MInFile
         WRITE     MinFlist;MinMainVARS	
         RETURN
..............................................................................
.
. ENTRY POINT : MInUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
MInUPD  BRANCH    MInFlag TO MIn6
         CALL      MInOPEN
MIn6
.    FILEPI    1;MInFile
.         UPDATE    MInFile;MinMainVARS	
         UPDATE    MinFlist;MinMainVARS	
         RETURN
..............................................................................
.
. ENTRY POINT : MInDEL
. REQUIRED    : 'MInFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
MInDEL  BRANCH    MInFlag TO MIn7
         CALL      MInOPEN
MIn7    FILEPI    1;MInFile
         DELETE    MinFlist
.         DELETE    MInFile,MInFLD
         RETURN
..............................................................................
.
. ENTRY POINT : MInAIM
. REQUIRED    : MInFLD2
. RETURNED    : Min Main Record
. DESCRIPTION : AIM MDC MainFILE READ
.               APPLICA TION'S RESPONSIBILITY TO TEST FLGS
.
MInAIM  BRANCH    MInFlag TO MIn8
         CALL      MInOpen
MIn8    FILEPI    1;MInFile2
         READ      MInFile2,MInFLD2;MinMainVARS	
         RETURN
..............................................................................
.
. ENTRY POINT : MInKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : Min Main Record
. DESCRIPTION : AIM KEY GENEREIC MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MInKG   BRANCH    MInFlag TO MIn10
         CALL      MInOpen
MIn10   FILEPI    1;MInFile2
         READKG    MInFile2;MinMainVARS	
         RETURN
..............................................................................
.
. ENTRY POINT : MInKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : Min Main Record
. DESCRIPTION : KEY GENERIC PRIOR MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MInKGP  BRANCH    MInFlag TO MIn11
         CALL      MInOpen
MIn11   FILEPI    1;MInFile2
         READKGP   MInFile2;MinMainVARS	
         RETURN
...............................................................................
. 
. ENTRY POINT : MInOPEN
. REQUIRED    : 'MInFlag' 'MInPATH'
. RETURNED    : 'MInFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
MInOPEN	TRAP      MInGONE giving error IF IO
         	OPEN      MinFlist
         	TRAPCLR   IO
         	MOVE      C1 TO MInFlag
         	RETURN
..............................................................................
MInGONE MOVE      MInNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


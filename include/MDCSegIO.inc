..............................................................................
.******************************************************
.*  MSeg List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MSeg DATACARD FILES.
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
.MDSMSCDD - Misc addressing MSeg, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : MSegKEY
. REQUIRED    : 'MSegFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MSegKEY  	Branch    MsegPath to Mseg1a,Mseg1c

MSeg1a	BRANCH    MSegFlag TO MSeg1b
         CALL      MSegOpen
MSeg1b    FILEPI    1;MSegFile
         READ      MSegFile,MSegFld;MSegVars	
         RETURN
MSeg1c	BRANCH    MSegFlag TO MSeg1d
         CALL      MSegOpen
MSeg1d    FILEPI    1;MSegFile2
         READ      MSegFile2,MSegFld2;MSegVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MSegTST
. REQUIRED    : MSegFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MSegTST     Branch    MSegpath to Mseg2a,mSeg2c

Mseg2a	BRANCH    MSegFlag TO MSeg2b
         CALL      MSegOpen
MSeg2b    FILEPI    1;MSegFile
         READ      MSegFile,MSegFld;STR1
         RETURN
Mseg2c	BRANCH    MSegFlag TO MSeg2e
         CALL      MSegOpen
MSeg2e    FILEPI    1;MSegFile2
         READ      MSegFile2,MSegFld2;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : MSegKS
. REQUIRED    :
. RETURNED    : MSeg Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
MSegKS   BRANCH    MSegFlag TO MSeg3
         CALL      MSegOpen
MSeg3    FILEPI    1;MSegFile
         READKS    MSegFile;MSegVars	
         RETURN
..............................................................................
. ENTRY POINT : MSegSEQ
. REQUIRED    :
. RETURNED    : MSeg Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
MSegSEQ  	   BRANCH    MSegFlag TO MSeg4
         	CALL      MSegOpen
MSeg4   	FILEPI    1;MSegFile
        	 READ      MSegFile,SEQ;MSegVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : MSegWRT
. REQUIRED    : 'MSegFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
MSegWRT  BRANCH    MSegFlag TO MSeg5
         CALL      MSegOpen
MSeg5    FILEPI    1;MSegFlist
         WRITE     MSegFList;MSegVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MSegUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
MSegUPD  BRANCH    MSegFlag TO MSeg6
         CALL      MSegOpen
MSeg6
   	FILEPI    1;MSegFlist
         UPDATE    MSegFlist;MSegVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MSegDEL
. REQUIRED    : 'MSegFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
MSegDEL  BRANCH    MSegFlag TO MSeg7
         CALL      MSegOpen
MSeg7    FILEPI    1;MSegFList
         DELETE    MSegFList
         RETURN
..............................................................................
. 
. ENTRY POINT : MSegOpen
. REQUIRED    : 'MSegFlag' 'MSegPATH'
. RETURNED    : 'MSegFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
MSegOpen	TRAP      MSegGONE giving error IF IO
         	OPEN      MSegFList
         	TRAPCLR   IO
         	MOVE      C1 TO MSegFlag
         	RETURN
..............................................................................
MSegGONE MOVE      MSegNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


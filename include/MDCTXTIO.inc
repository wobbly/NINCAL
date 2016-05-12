..............................................................................
.******************************************************
.*  MTXT List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MTXT DATACARD FILES.
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
.MDSMSCDD - Misc addressing MTXT, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - MDC Main???? data no supplied
.MDCTXTDD - Owner
.MDCTXTDD - Text
...............................................
. ENTRY POINT : MTXTKEY
. REQUIRED    : 'MTXTFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MTXTKEY  	Branch    MTXTPATH to MTXT1a,Mtxt1c
MTXT1a	BRANCH    MTXTFlag TO MTXT1b
         CALL      MTXTOpen
MTXT1b    FILEPI    1;MTXTFile
         READ      MTXTFile,MTXTFld;MTXTVars	
         RETURN
MTXT1c	BRANCH    MTXTFlag TO MTXT1d
         CALL      MTXTOpen
MTXT1d    FILEPI    1;MTXTFile2
         READ      MTXTFile2,MTXTFld2;MTXTVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MTXTTST
. REQUIRED    : MTXTFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MTXTTST    	Branch	MTXTPATH to MTXT2a,Mtxt2c	
MTXT2a	BRANCH    MTXTFlag TO MTXT2b
         CALL      MTXTOpen
MTXT2b    FILEPI    1;MTXTFile
         READ      MTXTFile,MTXTFld;STR1
         RETURN
MTXT2c	BRANCH    MTXTFlag TO MTXT2d
         CALL      MTXTOpen
MTXT2d    FILEPI    1;MTXTFile2
         READ      MTXTFile2,MTXTFld2;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : MTXTKS
. REQUIRED    :
. RETURNED    : MTXT Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
MTXTKS   	Branch	MTXTPATH to MTXT3a,MTXT3c

MTXT3a	BRANCH    MTXTFlag TO MTXT3b
         CALL      MTXTOpen
MTXT3b    FILEPI    1;MTXTFile
         READKS    MTXTFile;MTXTVars	
         RETURN
MTXT3c	BRANCH    MTXTFlag TO MTXT3d
         CALL      MTXTOpen
MTXT3d    FILEPI    1;MTXTFile2
         READKS    MTXTFile2;MTXTVars	
         RETURN
..............................................................................
. ENTRY POINT : MTXTSEQ
. REQUIRED    :
. RETURNED    : MTXT Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
MTXTSEQ  	   BRANCH    MTXTFlag TO MTXT4
         	CALL      MTXTOpen
MTXT4   	FILEPI    1;MTXTFile
        	 READ      MTXTFile,SEQ;MTXTVars	
        	 RETURN
..............................................................................
.
. ENTRY POINT : MTXTWRT
. REQUIRED    : 'MTXTFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
MTXTWRT  BRANCH    MTXTFlag TO MTXT5
         CALL      MTXTOpen
MTXT5    FILEPI    1;MTXTFList
         WRITE     MTXTFList;MTXTVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MTXTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
MTXTUPD  BRANCH    MTXTFlag TO MTXT6
         CALL      MTXTOpen
MTXT6
   	FILEPI    1;MTXTFList
         UPDATE    MTXTFList;MTXTVars	
         RETURN
..............................................................................
.
. ENTRY POINT : MTXTDEL
. REQUIRED    : 'MTXTFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
MTXTDEL  BRANCH    MTXTFlag TO MTXT7
         CALL      MTXTOpen
MTXT7    FILEPI    1;MTXTFile
         DELETE    MTXTFList
         RETURN
..............................................................................
.
. ENTRY POINT : MTXTKP
. REQUIRED    : 'MTXTPATH'
. RETURNED    : MIN DATACARD text
. DESCRIPTION : KEY SEQUENTIAL prior DATACARD FILE READ
.
MTXTKP
	BRANCH	MTXTFLAG,MTXT8
	CALL	MTXTOPEN
MTXT8
	trap	IOMssg giving Error if IO
	FILEPI	1;MTXTFILE
	READKP	MTXTFILE;MTXTVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : MTXTAIM
. REQUIRED    : MTXTFLD1,MTXTFLD2,MTXTFLD4,MTXTFLD5,MTXTFLD6,MTXTFLD7
. RETURNED    : MIN DATACARD TEXT
. DESCRIPTION : AIM DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MTXTAIM
	BRANCH	MTXTFLAG,MTXT9
	CALL	MTXTOPEN
MTXT9
	trap	IOMssg giving Error if IO
	FILEPI	1;MTXTFILE3
	READ	MTXTFILE3,MTXTFLD3;MTXTVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : MTXTKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : DATACARD RECORD
. DESCRIPTION : AIM KEY GENEREIC DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MTXTKG
	BRANCH	MTXTFLAG,MTXT10
	CALL	MTXTOPEN
MTXT10
	FILEPI	1;MTXTFILE3
	READKG	MTXTFILE3;MTXTVARS
	trapclr	IO
	RETURN
..............................................................................
.
. ENTRY POINT : MTXTKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : MIN DATACARD TEXT
. DESCRIPTION : KEY GENERIC PRIOR DATACARD FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MTXTKGP
	BRANCH	MTXTFLAG,MTXT11
	CALL	MTXTOPEN
MTXT11
	trap	IOMssg giving Error if IO
	FILEPI	1;MTXTFILE3
	READKGP	MTXTFILE3;MTXTVARS
	trapclr	IO
	RETURN
       
..............................................................................
. 
. ENTRY POINT : MTXTOpen
. REQUIRED    : 'MTXTFlag' 'MTXTPATH'
. RETURNED    : 'MTXTFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
MTXTOpen	TRAP      MTXTGONE giving error IF IO
         	OPEN      MTXTFList
         	TRAPCLR   IO
         	MOVE      C1 TO MTXTFlag
         	RETURN
..............................................................................
MTXTGONE MOVE      MTXTNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


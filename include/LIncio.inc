..............................................................................
.
. LIncIO LIncUSION
. NIN income report FILE I/O ROUTINES
.
.
..............................................................................
.
. ENTRY POINT : LIncKEY
. REQUIRED    : 'LIncFLD'
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
LIncKEY  BRANCH    LIncFLAG TO LInc1
         CALL      LIncOPEN
LInc1    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LIncFILE
         READ      LIncFILE,LIncFLD;LIncVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : LIncTST
. REQUIRED    : LIncFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
LIncTST  BRANCH    LIncFLAG TO LInc2
         CALL      LIncOPEN
LInc2    	Branch    LincPath to Linc2a,Linc2b

Linc2a 	trap      IOMSSG GIVING ERROR if IO
         	FILEPI    1;LIncFILE
         	READ      LIncFILE,LIncFLD;STR1
         	TRAPCLR   IO
         	RETURN
Linc2b 	trap      IOMSSG GIVING ERROR if IO
         	FILEPI    1;LIncFILE1
         	READ      LIncFILE1,LIncFLD1;STR1
         	TRAPCLR   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : LIncKS
. REQUIRED    :
. DESCRIPTION : KEY SEQUENTIAL RETURN-TO FILE READ
.
LIncKS   BRANCH    LIncFLAG TO LInc3
         CALL      LIncOPEN
LInc3    trap      IOMSSG GIVING ERROR if IO
         FILEPI    1;LIncFILE
         READKS    LIncFILE;LIncVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : LIncSEQ
. REQUIRED    :
. RETURNED    : RETURN-TO RECORD
. DESCRIPTION : SEQUENTIAL RETURN-TO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
LIncSEQ  BRANCH    LIncFLAG TO LInc4
         CALL      LIncOPEN

LInc4    trap      IOMSSG GIVING ERROR if IO

         FILEPI    1;LIncFILE
         READ      LIncFILE,SEQ;LIncVARS
         TRAPCLR   IO
         RETURN

..............................................................................
.
. ENTRY POINT : LIncWRT
. REQUIRED    : 'LIncFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
LIncWRT  Branch    LIncFLAG to LInc5
         CALL      LIncOPEN
LInc5    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LIncFILE
         WRITE     LIncFList;LIncVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LIncUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RETURN-TO FILE
.
LIncUPD  BRANCH    LIncFLAG TO LInc6
         CALL      LIncOPEN
LInc6    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LIncFILE
         UPDATE    LIncFList;LIncVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : LIncDEL
. REQUIRED    : 'LIncFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
LIncDEL  BRANCH    LIncFLAG TO LInc7
         CALL      LIncOPEN
LInc7    TRAP      IOMSSG GIVING ERROR IF IO
         FILEPI    1;LIncFILE
         DELETE    LIncFList
         TRAPCLR   IO
         RETURN
...............................................................................
..............................................................................
.
. ENTRY POINT : LIncOPEN
. REQUIRED    : 'LIncFLAG'
. RETURNED    : 'LIncFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
LIncOPEN  TRAP      LIncGONE IF IO
.
LIncOP
         OPEN      LIncFLIst
         TRAPCLR   IO
         MOVE      C1 TO LIncFLAG
         RETURN
..............................................................................

LIncGONE MOVE      LIncNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

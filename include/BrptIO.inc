.
. BrptDD INCLUSION
. Bankruptcy FILE IO DEFINITION
.
. FILE NAME : BankRuptcy
. REC LENGTH: 12 FIXED
. INDEX KEY : 1-6,7-12 (LR# ,INVOICE#)
.
.Patch 1.0  NEW    July 2008
..............................................................................
.
. ENTRY POINT : BrptKEY
. REQUIRED    : 'BrptFLD'
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
BrptKEY  BRANCH    BrptFLAG TO Brpt1
         CALL      BrptOPEN
Brpt1    FILEPI    1;BrptFILE
         READ      BrptFILE,BrptFLD;BrptVars
         RETURN
..............................................................................
.
. ENTRY POINT : BrptTST
. REQUIRED    : 'BrptFLD'
. RETURNED    :
. DESCRIPTION : ISAM KEY TEST READ
.
BrptTST  BRANCH    BrptFLAG TO Brpt2
         CALL      BrptOPEN
Brpt2    FILEPI    1;BrptFILE
         READ      BrptFILE,BrptFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : BrptKS
. REQUIRED    :
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : KEY SEQUENTIAL ADJUSTMENT FILE READ
.
BrptKS   BRANCH    BrptFLAG TO Brpt3
         CALL      BrptOPEN
Brpt3    FILEPI    1;BrptFILE
         READKS    BrptFILE;BrptVars
         RETURN
..............................................................................
.
. ENTRY POINT : BrptSEQ
. REQUIRED    :
. RETURNED    : ADJUSTMENT RECORD
. DESCRIPTION : SEQUENTIAL ADJUSTMENT FILE READ
.
BrptSEQ  BRANCH    BrptFLAG TO Brpt4,Brpt4B
         CALL      BrptOPSQ
         GOTO      Brpt4B
Brpt4    FILEPI    1;BrptFILE
         READ      BrptFILE,SEQ;BrptVars
         RETURN
Brpt4B   FILEPI    1;BrptFLSQ
         READ      BrptFLSQ,SEQ;BrptVars
         RETURN
..............................................................................
.
. ENTRY POINT : BrptWRT
. REQUIRED    : 'BrptFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
BrptWRT  BRANCH    BrptFLAG TO Brpt5
         CALL      BrptOPEN
Brpt5    FILEPI    1;BrptLIST
         WRITE     BrptLIST;BrptVars
         RETURN
..............................................................................
.
. ENTRY POINT : BrptUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST FILE
.
BrptUPD  BRANCH    BrptFLAG TO Brpt6
         CALL      BrptOPEN
Brpt6    FILEPI    1;BrptLIST
         UPDATE    BrptLIST;BrptVars
         RETURN
..............................................................................
.
. ENTRY POINT : BrptDEL
. REQUIRED    : 'BrptFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
BrptDEL  BRANCH    BrptFLAG TO Brpt7
         CALL      BrptOPEN
Brpt7    FILEPI    1;BrptLIST
         DELETE    BrptLIST
         RETURN
.
. ENTRY POINT : BrptOPEN
. REQUIRED    : 'BrptFLAG'
. RETURNED    : 'BrptFLAG'
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.

BrptOPEN
        TRAP      BrptGONE giving Error if IO

        OPEN      BrptLIST
        TRAPCLR   IO
        MOVE      C1 TO BrptFLAG
        RETURN

BrptOPSQ TRAP      BrptGONE IF IO
         OPEN      BrptFLSQ,BrptNAMESEQ
         TRAPCLR   IO
         MOVE      C2 TO BrptFLAG
         RETURN

BrptGONE MOVE      BrptNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


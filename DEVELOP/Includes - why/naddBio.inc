..............................................................................
.
. NAddBIO INCLUSION
. NIN DATACARD ADDRESSING FILE I/O ROUTINES
.
. FILE NAME : NINAddB
. REC LENGTH: 9 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NAddBKEY
. REQUIRED    : 'NAddBFLD'
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NAddBKEY
        BRANCH  NAddBFLAG,NAddB1
        CALL    NAddBOPEN
NAddB1
        trap    IOMssg giving Error if IO
.       FILEPI  1;NAddBFILE
        READ    NAddBFILE,NAddBFLD;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBTST
. REQUIRED    : NAddBFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NAddBTST
        BRANCH  NAddBFLAG,NAddB2
        CALL    NAddBOPEN
NAddB2
        trap    IOMssg giving Error if IO
.       FILEPI  1;NAddBFILE
        READ    NAddBFILE,NAddBFLD;;
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBKS
. REQUIRED    :
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD ADDRESSING FILE READ
.
NAddBKS
        BRANCH  NAddBFLAG,NAddB3
        CALL    NAddBOPEN
NAddB3
        trap    IOMssg giving Error if IO
.       FILEPI  1;NAddBFILE
        READKS  NAddBFILE;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBSEQ
. REQUIRED    :
. RETURNED    : DATACARD ADDRESSING RECORD
. DESCRIPTION : SEQUENTIAL DATACARD ADDRESSING FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NAddBSEQ
        BRANCH  NAddBFLAG,NAddB4
        CALL    NAddBOPEN
NAddB4
        trap    IOMssg giving Error if IO
.       FILEPI  1;NAddBFILE
        READ    NAddBFILE,SEQ;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBWRT
. REQUIRED    : 'NAddBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NAddBWRT
        BRANCH  NAddBFLAG,NAddB5
        CALL    NAddBOPEN
NAddB5
        trap    IOMssg giving Error if IO
        FILEPI  1;NAddBFLIST
        WRITE   NAddBFLIST;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NAddBUPD
        BRANCH  NAddBFLAG,NAddB6
        CALL    NAddBOPEN
NAddB6
        trap    IOMssg giving Error if IO
        FILEPI  1;NAddBFLIST
        UPDATE  NAddBFLIST;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBDEL
. REQUIRED    : 'NAddBFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NAddBDEL
        BRANCH  NAddBFLAG,NAddB7
        CALL    NAddBOPEN
NAddB7
        trap    IOMssg giving Error if IO
        FILEPI  1;NAddBFLIST
        DELETE  NAddBFLIST
        trapclr IO
        RETURN

..............................................................................
. ENTRY POINT : NAddBAIM
. REQUIRED    : NAddBFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NAddBAIM
        BRANCH  NAddBFLAG,NAddB8
        CALL    NAddBOPEN
NAddB8
        trap    IOMssg giving Error if IO
        READ    NAddBFLE2,NAddBFLD1;NAddBVARS
        trapclr IO
        RETURN

..............................................................................
.
. ENTRY POINT : NAddBKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NAddBKG
        BRANCH  NAddBFLAG,NAddB10
        CALL    NAddBOPEN
NAddB10
        trap    IOMssg giving Error if IO
        READKG  NAddBFLE2;NAddBVARS
        trapclr IO
        RETURN

...............................................................................
.
. ENTRY POINT : NAddBOPEN
. REQUIRED    : 'NAddBFLAG'
. RETURNED    : 'NAddBFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD ADDRESSING FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NAddBOPEN
        TRAP    NAddBGONE giving Error if IO
        OPEN    NAddBFLIST       .,EXCLUSIVE
.       OPEN    NAddBFLIST
        trapclr IO
        MOVE    C1,NAddBFLAG
        RETURN
.
NAddBGONE
        MOVE    NAddBNAME,FILENAME
        CALL    FILEGONE
.
..............................................................................
******************************************************
*  SRDSxSLTIO INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS SRDSx FILE.
* ****************************************************
.
.      FILE:      NISRDSxSLT
.    LENGTH:      51
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6         SRDSxSLTLIST
..............................................................................
.
. ENTRY POINT : SRDSxSLTKEY
. REQUIRED    : 'SRDSxSLTFLD'
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSxSLTKEY
          BRANCH    SRDSxSLTFLAG,SRDSxSLT1
          CALL      SRDSxSLTOPEN
SRDSxSLT1
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFILE
          READ      SRDSxSLTFILE,SRDSxSLTFLD;SRDSxSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTTST
. REQUIRED    : SRDSxSLTFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSxSLTTST
          BRANCH    SRDSxSLTFLAG,SRDSxSLT2
          CALL      SRDSxSLTOPEN
SRDSxSLT2
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFILE
          READ      SRDSxSLTFILE,SRDSxSLTFLD;;
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTKS
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD SELECTION FILE READ
.
SRDSxSLTKS
          BRANCH    SRDSxSLTFLAG,SRDSxSLT3
          CALL      SRDSxSLTOPEN
SRDSxSLT3
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFILE
          READKS    SRDSxSLTFILE;SRDSxSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTSEQ
. REQUIRED    :
. RETURNED    : DATACARD SELECTION RECORD
. DESCRIPTION : SEQUENTIAL DATACARD SELECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSxSLTSEQ
          BRANCH    SRDSxSLTFLAG,SRDSxSLT4
          CALL      SRDSxSLTOPEN
SRDSxSLT4
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFILE
          READ      SRDSxSLTFILE,SEQ;SRDSxSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTWRT
. REQUIRED    : 'SRDSxSLTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSxSLTWRT
          BRANCH    SRDSxSLTFLAG,SRDSxSLT5
          CALL      SRDSxSLTOPEN
SRDSxSLT5
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFLIST
          WRITE     SRDSxSLTFLIST;SRDSxSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
SRDSxSLTUPD
          BRANCH    SRDSxSLTFLAG,SRDSxSLT6
          CALL      SRDSxSLTOPEN
SRDSxSLT6
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFLIST
          UPDATE    SRDSxSLTFLIST;SRDSxSLTVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTDEL
. REQUIRED    : 'SRDSxSLTFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSxSLTDEL
          BRANCH    SRDSxSLTFLAG,SRDSxSLT7
          CALL      SRDSxSLTOPEN
SRDSxSLT7
          trap      IOMssg giving Error if IO
          FILEPI    1;SRDSxSLTFLIST
          DELETE    SRDSxSLTFLIST
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : SRDSxSLTOPEN
. REQUIRED    : 'SRDSxSLTFLAG'
. RETURNED    : 'SRDSxSLTFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN SRDSx DATACARD SELECTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSxSLTOPEN
          TRAP      SRDSxSLTGONE giving Error if IO
          OPEN      SRDSxSLTFLIST 
          trapclr   IO
          MOVE      C1,SRDSxSLTFLAG
          RETURN
.
SRDSxSLTGONE
          MOVE      SRDSxSLTNAME,FILENAME
          CALL      FILEGONE
.
..............................................................................

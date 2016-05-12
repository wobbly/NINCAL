..............................................................................
.******************************************************
.*  Srds to NIN List-Owner FILE DESCRIPTORS.
.. ....................................................
...............................................
. ENTRY POINT : S2NLoKEY
. REQUIRED    : 'S2NLoFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
S2NLoKEY            Branch    S2NLoPath,S2NLo1a,S2NLo1c
S2NLo1a   BRANCH    S2NLoFlag TO S2NLo1B
          CALL      S2NLoOpen
S2NLo1B             FILEPI    1;S2NLoFile
          READ      S2NLoFile,S2NLoFld;S2NLoVars  
          RETURN
S2NLo1C   BRANCH    S2NLoFlag TO S2NLo1D
          CALL      S2NLoOpen
S2NLo1D             FILEPI    1;S2NLoFile2
          READ      S2NLoFile2,S2NLoFld2;S2NLoVars          
          RETURN
..............................................................................
.
. ENTRY POINT : S2NLoTST
. REQUIRED    : S2NLoFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
S2NLoTST            Branch    S2NLoPath,S2NLo2a,S2NLo2C
S2NLo2a   BRANCH    S2NLoFlag TO S2NLo2B
          CALL      S2NLoOpen
S2NLo2B             FILEPI    1;S2NLoFile
          READ      S2NLoFile,S2NLoFld;STR1
          RETURN
S2NLo2c   BRANCH    S2NLoFlag TO S2NLo2D
          CALL      S2NLoOpen
S2NLo2d             FILEPI    1;S2NLoFile2
          READ      S2NLoFile2,S2NLoFld2;STR1
          RETURN
..............................................................................
.
. ENTRY POINT : S2NLoKS
. REQUIRED    :
. RETURNED    : S2NLo Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
S2NLoKS             Branch    S2NLoPath,S2NLo3a,S2NLo3C
S2NLo3A   BRANCH    S2NLoFlag TO S2NLo3B
          CALL      S2NLoOpen
S2NLo3B             FILEPI    1;S2NLoFile
          READKS    S2NLoFile;S2NLoVars 
          RETURN
S2NLo3C   BRANCH    S2NLoFlag TO S2NLo3D
          CALL      S2NLoOpen
S2NLo3D             FILEPI    1;S2NLoFile2
          READKS    S2NLoFile2;S2NLoVars          
          RETURN
..............................................................................
. ENTRY POINT : S2NLoSEQ
. REQUIRED    :
. RETURNED    : S2NLo Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
S2NLoSEQ               BRANCH    S2NLoFlag TO S2NLo4
          CALL      S2NLoOpen
S2NLo4    FILEPI    1;S2NLoFile
           READ      S2NLoFile,SEQ;S2NLoVars      
           RETURN
..............................................................................
.
. ENTRY POINT : S2NLoWRT
. REQUIRED    : 'S2NLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
S2NLoWRT  BRANCH    S2NLoFlag TO S2NLo5
         CALL      S2NLoOpen
S2NLo5    FILEPI    1;S2NLoFlist
         WRITE     S2NLoFlist;S2NLoVars 
         RETURN
..............................................................................
.
. ENTRY POINT : S2NLoUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
S2NLoUPD  BRANCH    S2NLoFlag TO S2NLo6
         CALL      S2NLoOpen
S2NLo6
          FILEPI    1;S2NLoFlist
         UPDATE    S2NLoFlist;S2NLoVars 
         RETURN
..............................................................................
.
. ENTRY POINT : S2NLoDEL
. REQUIRED    : 'S2NLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
S2NLoDEL            BRANCH    S2NLoFlag TO S2NLo7
          CALL      S2NLoOpen
S2NLo7              FILEPI    1;S2NLoFlist
          DELETE    S2NLoFList
          RETURN
..............................................................................
. 
. ENTRY POINT : S2NLoOpen
. REQUIRED    : 'S2NLoFlag' 'S2NLoPATH'
. RETURNED    : 'S2NLoFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
S2NLoOpen TRAP      S2NLoGONE giving error IF IO
          OPEN      S2NLoFList
          TRAPCLR   IO
          MOVE      C1 TO S2NLoFlag
          RETURN
..............................................................................
S2NLoGONE  MOVE      S2NLoNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


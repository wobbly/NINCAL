..............................................................................
.******************************************************
.*  SRDSSEL List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDSSEL DATACARD FILES.
.* ****************************************************
.
.      FILE:      SRDS_SEL
.    LENGTH:      Fixed=140
.  COMPRESS:      NONE
.      TYPE:      Isam 
......................................................
...............................................
. ENTRY POINT : SRDSSELKEY
. REQUIRED    : 'SRDSSELFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSSELKEY          Branch    SRDSSELPath to SRDSSEL1a,SRDSSEL1c

SRDSSEL1a           BRANCH    SRDSSELFlag TO SRDSSEL1b
                    CALL      SRDSSELOpen
SRDSSEL1b           FILEPI    1;SRDSSELFile
                    READ      SRDSSELFile,SRDSSELFld;SRDSSELVars      
                    RETURN
SRDSSEL1c           BRANCH    SRDSSELFlag TO SRDSSEL1d
                    CALL      SRDSSELOpen
SRDSSEL1d           FILEPI    1;SRDSSELFile2
                    READ      SRDSSELFile2,SRDSSELFld2;SRDSSELVars    
                    RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELTST
. REQUIRED    : SRDSSELFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSSELTST     Branch    SRDSSELpath to SRDSSEL2a,SRDSSEL2c

SRDSSEL2a    BRANCH    SRDSSELFlag TO SRDSSEL2b
         CALL      SRDSSELOpen
SRDSSEL2b    FILEPI    1;SRDSSELFile
         READ      SRDSSELFile,SRDSSELFld;STR1
         RETURN
SRDSSEL2c    BRANCH    SRDSSELFlag TO SRDSSEL2e
         CALL      SRDSSELOpen
SRDSSEL2e    FILEPI    1;SRDSSELFile2
         READ      SRDSSELFile2,SRDSSELFld2;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELKS
. REQUIRED    :
. RETURNED    : SRDSSEL Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
SRDSSELKS   BRANCH    SRDSSELFlag TO SRDSSEL3
         CALL      SRDSSELOpen
SRDSSEL3    FILEPI    1;SRDSSELFile
         READKS    SRDSSELFile;SRDSSELVars    
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELKS
. REQUIRED    :
. RETURNED    : SRDSSEL VALID READ OR NO
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
SRDSSELKS1   BRANCH    SRDSSELFlag TO SRDSSEL3A
            CALL      SRDSSELOpen
SRDSSEL3A    FILEPI    1;SRDSSELFile
            READKS    SRDSSELFile;SRDSSELLIST
            RETURN
..............................................................................
. ENTRY POINT : SRDSSELSEQ
. REQUIRED    :
. RETURNED    : SRDSSEL Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
SRDSSELSEQ      BRANCH    SRDSSELFlag TO SRDSSEL4
          CALL      SRDSSELOpen
SRDSSEL4     FILEPI    1;SRDSSELFile
           READ      SRDSSELFile,SEQ;SRDSSELVars        
           RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELWRT
. REQUIRED    : 'SRDSSELFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSSELWRT  BRANCH    SRDSSELFlag TO SRDSSEL5
         CALL      SRDSSELOpen
SRDSSEL5    FILEPI    1;SRDSSELFlist
         WRITE     SRDSSELFList;SRDSSELVars   
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
SRDSSELUPD  BRANCH    SRDSSELFlag TO SRDSSEL6
         CALL      SRDSSELOpen
SRDSSEL6
          FILEPI    1;SRDSSELFlist
         UPDATE    SRDSSELFlist;SRDSSELVars   
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSSELDEL
. REQUIRED    : 'SRDSSELFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSSELDEL          BRANCH    SRDSSELFlag TO SRDSSEL7
                    CALL      SRDSSELOpen
SRDSSEL7            FILEPI    1;SRDSSELFList
                    DELETE    SRDSSELFList
                    RETURN
..............................................................................
. 
. ENTRY POINT : SRDSSELOpen
. REQUIRED    : 'SRDSSELFlag' 'SRDSSELPATH'
. RETURNED    : 'SRDSSELFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSSELOpen         TRAP      SRDSSELGONE giving error IF IO
                    OPEN      SRDSSELFList
                    TRAPCLR   IO
                    MOVE      C1 TO SRDSSELFlag
                              RETURN
..............................................................................
SRDSSELGONE         MOVE      SRDSSELNAME TO FILENAME
                    CALL      FILEGONE
.
..............................................................................


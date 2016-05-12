..............................................................................
.******************************************************
.*  SRDSTXT List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDSTXT DATACARD FILES.
.* ****************************************************
.
...............................................
. ENTRY POINT : SRDSTXTKEY
. REQUIRED    : 'SRDSTXTFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSTXTKEY   Branch SRDSTXTPATH TO SRDSTXT1a,SRDSTXT1c

SRDSTXT1a   BRANCH    SRDSTXTFlag TO SRDSTXT1b
             CALL      SRDSTXTOpen
SRDSTXT1b     FILEPI    1;SRDSTXTFile
            READ      SRDSTXTFile,SRDSTXTFld;SRDSTXTVars        
            RETURN
SRDSTXT1c   BRANCH    SRDSTXTFlag TO SRDSTXT1d
             CALL      SRDSTXTOpen
SRDSTXT1d     FILEPI    1;SRDSTXTFle1
             READ      SRDSTXTFle1,SRDSTXTFld1;SRDSTXTVars        
             RETURN
..............................................................................
.
. ENTRY POINT : SRDSTXTTST
. REQUIRED    : SRDSTXTFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSTXTTST   Branch SRDSTXTPATH TO SRDSTXT2a,SRDSTXT2c

SRDSTXT2a       BRANCH    SRDSTXTFlag TO SRDSTXT2b
               CALL      SRDSTXTOpen
SRDSTXT2b      FILEPI    1;SRDSTXTFile
               READ      SRDSTXTFile,SRDSTXTFld;STR1
               RETURN
SRDSTXT2c       BRANCH    SRDSTXTFlag TO SRDSTXT2d
               CALL      SRDSTXTOpen
SRDSTXT2d      FILEPI    1;SRDSTXTFle1
               READ      SRDSTXTFle1,SRDSTXTFld1;STR1
               RETURN
..............................................................................
.
. ENTRY POINT : SRDSTXTKS
. REQUIRED    :
. RETURNED    : SRDSTXT Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
SRDSTXTKS   Branch SRDSTXTPATH TO SRDSTXT3a,SRDSTXT3c

SRDSTXT3a   BRANCH    SRDSTXTFlag TO SRDSTXT3b
            CALL      SRDSTXTOpen
SRDSTXT3b     FILEPI    1;SRDSTXTFile
              READKS    SRDSTXTFile;SRDSTXTVars     
              RETURN
SRDSTXT3c   BRANCH    SRDSTXTFlag TO SRDSTXT3b
            CALL      SRDSTXTOpen
SRDSTXT3d     FILEPI    1;SRDSTXTFle1
              READKS    SRDSTXTFle1;SRDSTXTVars     
              RETURN
..............................................................................
. ENTRY POINT : SRDSTXTSEQ
. REQUIRED    :
. RETURNED    : SRDSTXT Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
SRDSTXTSEQ       BRANCH    SRDSTXTFlag TO SRDSTXT4
          CALL      SRDSTXTOpen
SRDSTXT4      FILEPI    1;SRDSTXTFile
           READ      SRDSTXTFile,SEQ;SRDSTXTVars          
           RETURN
..............................................................................
.
. ENTRY POINT : SRDSTXTWRT
. REQUIRED    : 'SRDSTXTFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSTXTWRT  BRANCH    SRDSTXTFlag TO SRDSTXT5
         CALL      SRDSTXTOpen
SRDSTXT5    FILEPI    1;SRDSTXTFlist
         WRITE     SRDSTXTFlist;SRDSTXTVars     
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSTXTUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
SRDSTXTUPD  BRANCH    SRDSTXTFlag TO SRDSTXT6
         CALL      SRDSTXTOpen
SRDSTXT6
          FILEPI    1;SRDSTXTFlist
         UPDATE    SRDSTXTFlist;SRDSTXTVars     
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSTXTDEL
. REQUIRED    : 'SRDSTXTFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSTXTDEL    BRANCH    SRDSTXTFlag TO SRDSTXT7
          CALL      SRDSTXTOpen
SRDSTXT7      FILEPI    1;SRDSTXTFlist
          DELETE    SRDSTXTFList
          RETURN
..............................................................................
. 
. ENTRY POINT : SRDSTXTOpen
. REQUIRED    : 'SRDSTXTFlag' 'SRDSTXTPATH'
. RETURNED    : 'SRDSTXTFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSTXTOpen   TRAP      SRDSTXTGONE giving error IF IO
          OPEN      SRDSTXTFList
          TRAPCLR   IO
          MOVE      C1 TO SRDSTXTFlag
          RETURN
..............................................................................
SRDSTXTGONE  MOVE      SRDSTXTNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


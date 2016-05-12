..............................................................................
.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* S2N DATACARD FILES.
.* ****************************************************
.
.      FILE:      SRDS2NIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
...................
......................................................
...............................................
. ENTRY POINT : S2NKEY
. REQUIRED    : 'S2NFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
S2NKEY    Branch    S2NPath,S2N1a,S2N1c
S2N1a     BRANCH    S2NFlag TO S2N1B
          CALL      S2NOpen
S2N1B     FILEPI    1;S2NFile
          READ      S2NFile,S2NFld;S2NVars        
          RETURN
S2N1C     BRANCH    S2NFlag TO S2N1D
          CALL      S2NOpen
S2N1D     FILEPI    1;S2NFile2
          READ      S2NFile2,S2NFld2;S2NVars      
          RETURN
..............................................................................
.
. ENTRY POINT : S2NTST
. REQUIRED    : S2NFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
S2NTST    Branch    S2NPath,S2N2a,S2N2C
S2N2a     BRANCH    S2NFlag TO S2N2B
          CALL      S2NOpen
S2N2B     FILEPI    1;S2NFile
          READ      S2NFile,S2NFld;STR1
          RETURN
S2N2c     BRANCH    S2NFlag TO S2N2D
          CALL      S2NOpen
S2N2d     FILEPI    1;S2NFile2
          READ      S2NFile2,S2NFld2;STR1
          RETURN
..............................................................................
.
. ENTRY POINT : S2NKS
. REQUIRED    :
. RETURNED    : S2N Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
S2NKS     Branch    S2NPath,S2N3a,S2N3C
S2N3A     BRANCH    S2NFlag TO S2N3B
          CALL      S2NOpen
S2N3B     FILEPI    1;S2NFile
          READKS    S2NFile;S2NVars     
          RETURN
S2N3C     BRANCH    S2NFlag TO S2N3D
          CALL      S2NOpen
S2N3D     FILEPI    1;S2NFile2
          READKS    S2NFile2;S2NVars    
          RETURN
..............................................................................
. ENTRY POINT : S2NSEQ
. REQUIRED    :
. RETURNED    : S2N Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
S2NSEQ       BRANCH    S2NFlag TO S2N4
          CALL      S2NOpen
S2N4      FILEPI    1;S2NFile
           READ      S2NFile,SEQ;S2NVars          
           RETURN
..............................................................................
.
. ENTRY POINT : S2NWRT
. REQUIRED    : 'S2NFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
S2NWRT  BRANCH    S2NFlag TO S2N5
         CALL      S2NOpen
S2N5    FILEPI    1;S2NFlist
         WRITE     S2NFlist;S2NVars     
         RETURN
..............................................................................
.
. ENTRY POINT : S2NUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
S2NUPD  BRANCH    S2NFlag TO S2N6
         CALL      S2NOpen
S2N6
          FILEPI    1;S2NFlist
         UPDATE    S2NFlist;S2NVars     
         RETURN
..............................................................................
.
. ENTRY POINT : S2NDEL
. REQUIRED    : 'S2NFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
S2NDEL    BRANCH    S2NFlag TO S2N7
          CALL      S2NOpen
S2N7      FILEPI    1;S2NFlist
          DELETE    S2NFList
          RETURN
..............................................................................
. 
. ENTRY POINT : S2NOpen
. REQUIRED    : 'S2NFlag' 'S2NPATH'
. RETURNED    : 'S2NFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
S2NOpen   TRAP      S2NGONE giving error IF IO
          OPEN      S2NFList
          TRAPCLR   IO
          MOVE      C1 TO S2NFlag
          RETURN
..............................................................................
S2NGONE  MOVE      S2NNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


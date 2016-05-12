..............................................................................
.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:      MIN2NIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
...................
......................................................
. ENTRY POINT : SRDSLoKEY
. REQUIRED    : 'SRDSLoFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSLoKEY        
          BRANCH    SRDSLoFlag TO SRDSLo1
          CALL      SRDSLoOpen
SRDSLo1   FILEPI    1;SRDSLoFile
          READ      SRDSLoFile,SRDSLoFld;SRDSLoVars         
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoTST
. REQUIRED    : SRDSLoFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSLoTST   
          BRANCH    SRDSLoFlag TO SRDSLo2
          CALL      SRDSLoOpen
SRDSLo2   FILEPI    1;SRDSLoFile
          READ      SRDSLoFile,SRDSLoFld;STR1
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoKS
. REQUIRED    :
. RETURNED    : SRDSLo Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
SRDSLoKS            
          BRANCH    SRDSLoFlag TO SRDSLo3
          CALL      SRDSLoOpen
SRDSLo3   FILEPI    1;SRDSLoFile
          READKS    SRDSLoFile;SRDSLoVars         
          RETURN
          RETURN
..............................................................................
. ENTRY POINT : SRDSLoSEQ
. REQUIRED    :
. RETURNED    : SRDSLo Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
SRDSLoSEQ              BRANCH    SRDSLoFlag TO SRDSLo4
          CALL      SRDSLoOpen
SRDSLo4             FILEPI    1;SRDSLoFile
           READ      SRDSLoFile,SEQ;SRDSLoVars    
           RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoWRT
. REQUIRED    : 'SRDSLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSLoWRT  BRANCH    SRDSLoFlag TO SRDSLo5
         CALL      SRDSLoOpen
SRDSLo5    FILEPI    1;SRDSLoFlist
         WRITE     SRDSLoFlist;SRDSLoVars         
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
SRDSLoUPD  BRANCH    SRDSLoFlag TO SRDSLo6
         CALL      SRDSLoOpen
SRDSLo6
          FILEPI    1;SRDSLoFlist
         UPDATE    SRDSLoFlist;SRDSLoVars         
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoDEL
. REQUIRED    : 'SRDSLoFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSLoDEL  BRANCH    SRDSLoFlag TO SRDSLo7
          CALL      SRDSLoOpen
SRDSLo7             FILEPI    1;SRDSLoFlist
          DELETE    SRDSLoFList
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSLoAIM   
. REQUIRED    : BRKAKEY
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSLoAIM  BRANCH    SRDSLoFlag TO SRDSLo9
         CALL      SRDSLoOpen
SRDSLo9    TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLoLOCK TO SRDSLo9L,SRDSLo9R,SRDSLo9N

.SRDSLo9L - file locks
SRDSLo9L   FILEPI    1;SRDSLoFile2
         READ      SRDSLoFile2,SRDSLoFLD2;SRDSLoVARS
         TRAPCLR   IO
         RETURN

.SRDSLo9R - record locks
SRDSLo9R 
         READLK    SRDSLoFile2,SRDSLoFLD2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN         

.SRDSLo9N - no locks
SRDSLo9N   
         READ      SRDSLoFile2,SRDSLoFLD2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN
         
..............................................................................
.
. ENTRY POINT : SRDSLoKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM KEY GENEREIC BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSLoKG   BRANCH    SRDSLoFlag TO SRDSLo10
         CALL      SRDSLoOpen
SRDSLo10   TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLoLOCK TO SRDSLo10L,SRDSLo10R,SRDSLo10N

.SRDSLo10L - file locks
SRDSLo10L  FILEPI    1;SRDSLoFile2
         READKG    SRDSLoFile2;SRDSLoVARS                               BRKCNT:       5-7    BROKER CONTACT NUMBER. 
         TRAPCLR   IO
         RETURN

.SRDSLo10R - record locks
SRDSLo10R 
         READKGLK  SRDSLoFile2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN         

.SRDSLo10N - no locks
SRDSLo10N   
         READKG    SRDSLoFile2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
. 
. ENTRY POINT : SRDSLoKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : KEY GENERIC PRIOR BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSLoKGP  BRANCH    SRDSLoFlag TO SRDSLo11
         CALL      SRDSLoOpen
SRDSLo11   TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLoLOCK TO SRDSLo11L,SRDSLo11R,SRDSLo11N

.SRDSLo11L - file locks
SRDSLo11L  FILEPI    1;SRDSLoFile2
         READKGP   SRDSLoFile2;SRDSLoVARS
         TRAPCLR   IO
         RETURN
   
.SRDSLo11R - record locks
SRDSLo11R 
         READKGPLK SRDSLoFile2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN         

.SRDSLo11N - no locks
SRDSLo11N   
         READKGP   SRDSLoFile2;SRDSLoVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
..............................................................................
. 
. ENTRY POINT : SRDSLoOpen
. REQUIRED    : 'SRDSLoFlag' 'SRDSLoPATH'
. RETURNED    : 'SRDSLoFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSLoOpen          TRAP      SRDSLoGONE giving error IF IO
          OPEN      SRDSLoFList
          TRAPCLR   IO
          MOVE      C1 TO SRDSLoFlag
          RETURN
..............................................................................
SRDSLoGONE  MOVE      SRDSLoNAME TO FILENAME
          CALL      FILEGONE
.
..............................................................................


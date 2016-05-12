..............................................................................
.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
...............................................
. ENTRY POINT : SRDSKEY
. REQUIRED    : 'SRDSFld'
. RETURNED    :  RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSKEY    
          BRANCH    SRDSFlag TO SRDS1
          CALL      SRDSOpen
SRDS1     FILEPI    1;SRDSFile
          READ      SRDSFile,SRDSFld;SRDSVars        
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSTST
. REQUIRED    : SRDSFld
. RETURNED    :
. DESCRIPTION : TEST KEY
.
SRDSTST
          BRANCH    SRDSFlag TO SRDS2
          CALL      SRDSOpen
SRDS2     FILEPI    1;SRDSFile
          READ      SRDSFile,SRDSFld;STR1
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSKS
. REQUIRED    :
. RETURNED    : SRDS Main Record
. DESCRIPTION : KEY SEQUENTIAL MDC MainFILE READ
.
SRDSKS
          BRANCH    SRDSFlag TO SRDS3
          CALL      SRDSOpen
SRDS3     FILEPI    1;SRDSFile
          READKS    SRDSFile;SRDSVars     
          RETURN
..............................................................................
. ENTRY POINT : SRDSSEQ
. REQUIRED    :
. RETURNED    : SRDS Main Record
. DESCRIPTION : SEQUENTIAL MDC MainFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
SRDSSEQ       BRANCH    SRDSFlag TO SRDS4
          CALL      SRDSOpen
SRDS4      FILEPI    1;SRDSFile
           READ      SRDSFile,SEQ;SRDSVars          
           RETURN
..............................................................................
.
. ENTRY POINT : SRDSWRT
. REQUIRED    : 'SRDSFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
SRDSWRT  BRANCH    SRDSFlag TO SRDS5
         CALL      SRDSOpen
SRDS5    FILEPI    1;SRDSFlist
         WRITE     SRDSFlist;SRDSVars     
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MDC MainFILE
.
SRDSUPD  BRANCH    SRDSFlag TO SRDS6
         CALL      SRDSOpen
SRDS6
          FILEPI    1;SRDSFlist
         UPDATE    SRDSFlist;SRDSVars     
         RETURN
..............................................................................
.
. ENTRY POINT : SRDSDEL
. REQUIRED    : 'SRDSFld'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
SRDSDEL    BRANCH    SRDSFlag TO SRDS7
          CALL      SRDSOpen
SRDS7      FILEPI    1;SRDSFlist
          DELETE    SRDSFList
          RETURN
..............................................................................
.
. ENTRY POINT : SRDSAIM   
. REQUIRED    : BRKAKEY
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSAIM  BRANCH    SRDSFlag TO SRDS9
         CALL      SRDSOpen
SRDS9    TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLOCK TO SRDS9L,SRDS9R,SRDS9N

.SRDS9L - file locks
SRDS9L   FILEPI    1;SRDSFile2
         READ      SRDSFile2,SRDSFLD2;SRDSVARS
         TRAPCLR   IO
         RETURN

.SRDS9R - record locks
SRDS9R 
         READLK    SRDSFile2,SRDSFLD2;SRDSVARS 
         TRAPCLR   IO
         RETURN         

.SRDS9N - no locks
SRDS9N   
         READ      SRDSFile2,SRDSFLD2;SRDSVARS 
         TRAPCLR   IO
         RETURN
         
..............................................................................
.
. ENTRY POINT : SRDSKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : AIM KEY GENEREIC BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSKG   BRANCH    SRDSFlag TO SRDS10
         CALL      SRDSOpen
SRDS10   TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLOCK TO SRDS10L,SRDS10R,SRDS10N

.SRDS10L - file locks
SRDS10L  FILEPI    1;SRDSFile2
         READKG    SRDSFile2;SRDSVARS                               BRKCNT:       5-7    BROKER CONTACT NUMBER. 
         TRAPCLR   IO
         RETURN

.SRDS10R - record locks
SRDS10R 
         READKGLK  SRDSFile2;SRDSVARS 
         TRAPCLR   IO
         RETURN         

.SRDS10N - no locks
SRDS10N   
         READKG    SRDSFile2;SRDSVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
. 
. ENTRY POINT : SRDSKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : BROKER RECORD
. DESCRIPTION : KEY GENERIC PRIOR BROKER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
SRDSKGP  BRANCH    SRDSFlag TO SRDS11
         CALL      SRDSOpen
SRDS11   TRAP      IOMssg Giving Error if IO
         BRANCH    SRDSLOCK TO SRDS11L,SRDS11R,SRDS11N

.SRDS11L - file locks
SRDS11L  FILEPI    1;SRDSFile2
         READKGP   SRDSFile2;SRDSVARS
         TRAPCLR   IO
         RETURN
   
.SRDS11R - record locks
SRDS11R 
         READKGPLK SRDSFile2;SRDSVARS 
         TRAPCLR   IO
         RETURN         

.SRDS11N - no locks
SRDS11N   
         READKGP   SRDSFile2;SRDSVARS 
         TRAPCLR   IO
         RETURN
..............................................................................
..............................................................................
. 
. ENTRY POINT : SRDSOpen
. REQUIRED    : 'SRDSFlag' 'SRDSPATH'
. RETURNED    : 'SRDSFlag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MDC MainFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
SRDSOpen   TRAP      SRDSGONE giving error IF IO
          OPEN      SRDSFList
          TRAPCLR   IO
          MOVE      C1 TO SRDSFlag
          RETURN
..............................................................................
SRDSGONE  MOVE      SRDSNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


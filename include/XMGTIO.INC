..............................................................................
.
. xmgtIO INCLUSION
.
. FILE NAME : exchmgmt
..............................................................................
.
. ENTRY POINT : xmgtKEY
. REQUIRED    : 'xmgtFLD', xmgtPATH
. RETURNED    : 
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
xmgtKEY 
         BRANCH    xmgtFLAG TO xmgt1
         CALL      xmgtOPEN
xmgt1    FILEPI    1;xmgtFILE
         READ      xmgtFILE,xmgtFLD;xmgtvars
         RETURN
.
..............................................................................
.
. ENTRY POINT : xmgtTST
. REQUIRED    : xmgtFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
xmgtTST 
         BRANCH    xmgtFLAG TO xmgt2
         CALL      xmgtOPEN
xmgt2     FILEPI    1;xmgtFILE
          READ      xmgtFILE,xmgtFLD;STR1
          RETURN
.
..............................................................................
.
. ENTRY POINT : xmgtKS
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL LIST- 
.
xmgtKS  
         BRANCH    xmgtFLAG TO xmgt3
         CALL      xmgtOPEN
xmgt3    FILEPI    1;xmgtFILE
         READKS    xmgtFILE;xmgtvars
         RETURN
.
..............................................................................
.
. ENTRY POINT : xmgtSEQ
. REQUIRED    :
. RETURNED    : LIST- 
. DESCRIPTION : SEQUENTIAL LIST-
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
xmgtSEQ  
         BRANCH    xmgtFLAG TO xmgt4
         CALL      xmgtOPEN
xmgt4    FILEPI    1;xmgtFILE
         READ      xmgtFILE,SEQ;xmgtVARS
         RETURN
.
..............................................................................
.
. ENTRY POINT : xmgtWRT
. REQUIRED    : 'xmgtFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
xmgtWRT  BRANCH    xmgtFLAG TO xmgt5
         CALL      xmgtOPEN
xmgt5    FILEPI    1;xmgtFILE
         WRITE     xmgtFILE,xmgtFLD;xmgtVARS
         RETURN
..............................................................................
.
. ENTRY POINT : xmgtUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST- 
.
xmgtUPD  BRANCH    xmgtFLAG TO xmgt6
         CALL      xmgtOPEN
xmgt6    FILEPI    1;xmgtFILE
         UPDATE    xmgtFILE;xmgtVARS
         RETURN
..............................................................................
.
. ENTRY POINT : xmgtDEL
. REQUIRED    : 'xmgtFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
xmgtDEL  BRANCH    xmgtFLAG TO xmgt7
         CALL      xmgtOPEN
xmgt7    FILEPI    1;xmgtFILE
         DELETE    xmgtFILE,xmgtFLD
         RETURN
...............................................................................
.
. ENTRY POINT : xmgtOPEN
. REQUIRED    : 'xmgtPATH'
. RETURNED    : 'xmgtFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN 
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
xmgtOPEN 
         TRAP      xmgtGONE GIVING ERROR IF IO
         OPEN      xmgtFILE,xmgtNAME
         TRAPCLR   IO
         MOVE      C1 TO xmgtFLAG
         RETURN
.
.
xmgtGONE MOVE      xmgtNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


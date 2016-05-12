..............................................................................
.
. NXCHIO INCLUSION
. NIN EXCHANGE DETAIL FILE I/O ROUTINES
.
.
. FILE NAME : NINXCHNG, NINLRXNG
. REC LENGTH: 200 FIXED
. INDEX KEY : 1-17 (1-6 POSITION MLR#, 7-12 MLR#, 13-17 ENTRY#)
.           : 21-26  LR#
. AAMDEX KEY: 1-6, 7-12
.
.             ASH  05APR2005 Complete rewrite - File Conversion
..............................................................................
.
. ENTRY POINT : NXCHKEY
. REQUIRED    : 'NXCHFLD1' OR 'NXCHFLD2'
. RETURNED    : EXCHANGE DETAIL RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXCHKEY
          BRANCH    NXCHPATH TO NXCH1A,NXCH1C
NXCH1A
          COMPARE   C1,NXCHFLAG
          GOTO      NXCH1B IF EQUAL
          CALL      NXCHOPEN
NXCH1B    trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch1bL,Nxch1bR,Nxch1bN

Nxch1bL   FILEPI    1;NXCHFILE
          READ      NXCHFILE,NXCHFLD1;NXCHVARS
          trapclr   IO
          RETURN
Nxch1bR   
          READLK      NXCHFILE,NXCHFLD1;NXCHVARS
          trapclr   IO
          RETURN
Nxch1bN
          READ      NXCHFILE,NXCHFLD1;NXCHVARS
          trapclr   IO
          RETURN
NXCH1C
          COMPARE   C1,NXCHFLAG
          GOTO NXCH1D IF EQUAL
          CALL      NXCHOPEN
NXCH1D
          trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch1dL,Nxch1dR,Nxch1dN

NXCH1DL   FILEPI    1;NXCHFLE2
          READ      NXCHFLE2,NXCHFLD2;NXCHVARS
          trapclr   IO
          RETURN
NXCH1DR
          READLK      NXCHFLE2,NXCHFLD2;NXCHVARS
          trapclr   IO
          RETURN
NXCH1DN
          READ      NXCHFLE2,NXCHFLD2;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHTST
. REQUIRED    : VALID KEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NXCHTST
          BRANCH    NXCHPATH TO NXCH2A,NXCH2C
NXCH2A
          COMPARE   C1,NXCHFLAG
          GOTO NXCH2B IF EQUAL
          CALL      NXCHOPEN
NXCH2B
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFILE
          READ      NXCHFILE,NXCHFLD1;;
          trapclr   IO
          RETURN
NXCH2C
          COMPARE   C1,NXCHFLAG
          GOTO NXCH2D IF EQUAL
          CALL      NXCHOPEN
NXCH2D
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFLE2
          READ      NXCHFLE2,NXCHFLD2;;
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHKS
. REQUIRED    :
. RETURNED    : EXCHANGE DETAIL RECORD
. DESCRIPTION : KEY SEQUENTIAL EXCHANGE DETAIL FILE READ
.
NXCHKS
          BRANCH    NXCHPATH TO NXCH3A,NXCH3C
NXCH3A
          COMPARE   C1,NXCHFLAG
          GOTO NXCH3B IF EQUAL
          CALL      NXCHOPEN
NXCH3B
          trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch3bL,Nxch3bR,Nxch3bN
NXCH3BL   FILEPI    1;NXCHFILE
          READKS    NXCHFILE;NXCHVARS
          trapclr   IO
          RETURN
NXCH3BR   FILEPI    1;NXCHFILE
          READKSLK    NXCHFILE;NXCHVARS
          trapclr   IO
          RETURN
NXCH3BN
          READKS    NXCHFILE;NXCHVARS
          trapclr   IO
          RETURN
NXCH3C
          COMPARE   C1,NXCHFLAG
          GOTO NXCH3D IF EQUAL
          CALL      NXCHOPEN
NXCH3D
          trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch3dL,Nxch3dR,Nxch3dN
NXCH3DL   FILEPI    1;NXCHFLE2
          READKS    NXCHFLE2;NXCHVARS
          trapclr   IO
          RETURN
NXCH3DR
          READKSlk    NXCHFLE2;NXCHVARS
          trapclr   IO
          RETURN
NXCH3DN
          READKS    NXCHFLE2;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHSEQ
. REQUIRED    :
. RETURNED    : EXCHANGE DETAIL RECORD
. DESCRIPTION : SEQUENTIAL EXCHANGE DETAIL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXCHSEQ
          BRANCH    NXCHPATH TO NXCH4A,NXCH4C
NXCH4A
          BRANCH    NXCHFLAG,NXCH4B
          CALL      NXCHOPEN
NXCH4B
          trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch4bL,Nxch4bR,Nxch4bN
NXCH4BL   FILEPI    1;NXCHFILE
          READ      NXCHFILE,SEQ;NXCHVARS
          trapclr   IO
          RETURN
NXCH4BR
          READLK      NXCHFILE,SEQ;NXCHVARS
          trapclr   IO
          RETURN
NXCH4BN
          READ      NXCHFILE,SEQ;NXCHVARS
          trapclr   IO
          RETURN
NXCH4C
          BRANCH    NXCHFLAG,NXCH4D
          CALL      NXCHOPEN
NXCH4D
          trap      IOMssg giving Error if IO
          Branch    NxchLock to Nxch4dL,Nxch4dR,Nxch4dN
NXCH4DL   FILEPI    1;NXCHFLE2
          READ      NXCHFLE2,SEQ;NXCHVARS
          trapclr   IO
          RETURN
NXCH4DR
          READLK      NXCHFLE2,SEQ;NXCHVARS
          trapclr   IO
          RETURN
NXCH4DN
          READ      NXCHFLE2,SEQ;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHWRT
. REQUIRED    : 'VALID RECORD & KEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NXCHWRT
          BRANCH    NXCHFLAG,NXCH5
          CALL      NXCHOPEN
NXCH5
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFLIST
          WRITE     NXCHFLIST;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE EXCHANGE DETAIL FILE
.
NXCHUPD
          BRANCH    NXCHFLAG,NXCH6
          CALL      NXCHOPEN
NXCH6
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFLIST
          UPDATE    NXCHFLIST;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHDEL
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NXCHDEL
          BRANCH    NXCHFLAG,NXCH7
          CALL      NXCHOPEN
NXCH7
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFLIST
          DELETE    NXCHFLIST
          trapclr   IO
          RETURN
...............................................................................
..............................................................................
.
. ENTRY POINT : NXCHKP
. REQUIRED    :
. RETURNED    : EXCHANGE DETAIL RECORD
. DESCRIPTION : KEY PRIOR SEQUENTIAL EXCHANGE DETAIL FILE READ
.
NXCHKP
          BRANCH    NXCHPATH,NXCH8A,NXCH8C
          CALL      NXCHOPEN
NXCH8A
          COMPARE   C1,NXCHFLAG
          GOTO NXCH8B IF EQUAL
          CALL      NXCHOPEN
NXCH8B
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFILE
          READKP    NXCHFILE;NXCHVARS
          trapclr   IO
          RETURN
NXCH8C
          COMPARE  C1,NXCHFLAG
          GOTO NXCH8D IF EQUAL
          CALL      NXCHOPEN
NXCH8D
          trap      IOMssg giving Error if IO
          FILEPI    1;NXCHFLE2
          READKP    NXCHFLE2;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
. ENTRY POINT : NXCHAIM
. REQUIRED    : NXCHFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXCHAIM
          BRANCH    NXCHFLAG,NXCH9
          CALL      NXCHOPEN
NXCH9
          trap      IOMssg giving Error if IO
          READ      NXCHFLE3,NXCHFLD3,NXCHFLD4;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXCHKG
          BRANCH    NXCHFLAG,NXCH10
          CALL      NXCHOPEN
NXCH10
          trap      IOMssg giving Error if IO
          READKG    NXCHFLE3;NXCHVARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : NXCHOPEN
. REQUIRED    : 'NXCHFLAG'
. RETURNED    : 'NXCHFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN EXCHANGE DETAIL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NXCHOPEN
          TRAP      NXCHGONE giving Error IF IO
          OPEN      NXCHFLIST
          TRAPCLR   IO
          MOVE      C1,NXCHFLAG
          RETURN
.
NXCHGONE MOVE      NXCHNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

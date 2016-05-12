..............................................................................
.
. NREFIO INCLUSION
. NIN DATACARD REFERANCE FILE I/O ROUTINES
.
. FILE NAME : NINREF
. REC LENGTH: 81 VAR
. INDEX KEY : 1-3 (1 POSITION REFCODE, 2 POSITION REF#)
.
.because nreffle3 & 4 are depreciated - no more file locking
. Patched May 2011  SRDS addditional isi file
..............................................................................
.
. ENTRY POINT : NREFKEY
. REQUIRED    : 'NREFFLD'
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREFKEY
          Branch    Nrefpath,Nref1a,nref1c,Nref1e
          
Nref1a    BRANCH    NREFFLAG,NREF1b
          CALL      NREFOPEN
NREF1b
          trap      IOMssg giving Error if IO
          
          BRANCH     nrefLOCK to nref1bL,nref1bR,nref1bN
NREf1bL                                       
          FILEPI    1;NREFFILE
          READ      NREFFILE,NREFFLD;NREFVARS
          trapclr   IO
          RETURN
NREf1bR                                                
          READLK      NREFFILE,NREFFLD;NREFVARS
          trapclr   IO
          RETURN
NREf1bN                                                
          READ      NREFFILE,NREFFLD;NREFVARS
          trapclr   IO
          RETURN

Nref1c    BRANCH    NREFFLAG,NREF1d
          CALL      NREFOPEN
NREF1d
          trap      IOMssg giving Error if IO
.          FILEPI    1;NREFFLE3
          READ      NREFFLE3,NREFFLD3;NREFVARS
          trapclr   IO
          RETURN
Nref1e    BRANCH    NREFFLAG,NREF1f
          CALL      NREFOPEN
NREF1f
          trap      IOMssg giving Error if IO
.          FILEPI    1;NREFFLE4
          READ      NREFFLE4,NREFFLD4;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFTST
. REQUIRED    : NREFFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NREFTST
          Branch    Nrefpath,Nref2a,nref2c,Nref2e
          
Nref2a    BRANCH    NREFFLAG,NREF2b
          CALL      NREFOPEN
NREF2B
          trap      IOMssg giving Error if IO
          BRANCH     nrefLOCK,nref2bL,nref2bR,nref2bN
NREF2BL
          FILEPI    1;NREFFILE
          READ      NREFFILE,NREFFLD;;
          trapclr   IO
          RETURN
NREF2BR
          READLK      NREFFILE,NREFFLD;;
          trapclr   IO
          RETURN
NREF2BN
          READ      NREFFILE,NREFFLD;;
          trapclr   IO
          RETURN
          
Nref2c    BRANCH    NREFFLAG,NREF2d
          CALL      NREFOPEN
NREF2d
          trap      IOMssg giving Error if IO
          READ      NREFFLE3,NREFFLD3;;
          trapclr   IO
          RETURN
Nref2e    BRANCH    NREFFLAG,NREF2f
          CALL      NREFOPEN
NREF2f
          trap      IOMssg giving Error if IO
          READ      NREFFLE4,NREFFLD4;;
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFKS
. REQUIRED    :
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD REFERANCE FILE READ
.
NREFKS
          Branch    Nrefpath,Nref3a,nref3c,nref3e
          
Nref3a    BRANCH    NREFFLAG,NREF3b
          CALL      NREFOPEN
NREF3b
          trap      IOMssg giving Error if IO
          BRANCH     nrefLOCK,nref3bL,nref3bR,nref3bN
NREF3bL
          FILEPI    1;NREFFILE
          READKS    NREFFILE;NREFVARS
          trapclr   IO
          RETURN
NREF3bR
          READKSLK    NREFFILE;NREFVARS
          trapclr   IO
          RETURN
NREF3bN
          READKS    NREFFILE;NREFVARS
          trapclr   IO
          RETURN

Nref3c    BRANCH    NREFFLAG,NREF3d
          CALL      NREFOPEN
NREF3d
          trap      IOMssg giving Error if IO
          FILEPI    1;NREFFLE3
          READKS    NREFFLE3;NREFVARS
          trapclr   IO
          RETURN

Nref3e    BRANCH    NREFFLAG,NREF3f
          CALL      NREFOPEN
NREF3f
          trap      IOMssg giving Error if IO
          FILEPI    1;NREFFLE4
          READKS    NREFFLE4;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFSEQ
. REQUIRED    :
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : SEQUENTIAL DATACARD REFERANCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREFSEQ
          BRANCH    NREFFLAG,NREF4
          CALL      NREFOPEN
NREF4
          trap      IOMssg giving Error if IO
          BRANCH     nrefLOCK,nref4L,nref4R,nref4N
NREF4L
          FILEPI    1;NREFFILE
          READ      NREFFILE,SEQ;NREFVARS
          trapclr   IO
          RETURN
NREF4R
          READLK      NREFFILE,SEQ;NREFVARS
          trapclr   IO
          RETURN
NREF4N
          READ      NREFFILE,SEQ;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFSEQ2
. REQUIRED    :
. RETURNED    : DATACARD REFERANCE RECORD
. DESCRIPTION : SEQUENTIAL DATACARD REFERANCE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NREFSEQ2
.         BRANCH    NREFFLAG,NREF6
.         CALL      NREFOPEN
.NREF6
.         FILEPI    1;NREFFLE1
.         READ      NREFFLE1,SEQ;NREFVARS
.         RETURN

..............................................................................
.
. ENTRY POINT : NREFWRT
. REQUIRED    : 'NREFFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NREFWRT
          BRANCH    NREFFLAG,NREF5
          CALL      NREFOPEN
NREF5
          trap      IOMssg giving Error if IO
          FILEPI    1;NREFFLIST
          WRITE     NREFFLIST;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NREFUPD
          BRANCH    NREFFLAG,NREF6
          CALL      NREFOPEN
NREF6
          trap      IOMssg giving Error if IO
          FILEPI    1;NREFFLIST
          UPDATE    NREFFLIST;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFDEL
. REQUIRED    : 'NREFFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NREFDEL
          BRANCH    NREFFLAG,NREF7
          CALL      NREFOPEN
NREF7
          trap      IOMssg giving Error if IO
          FILEPI    1;NREFFLIST
          DELETE    NREFFLIST
          trapclr   IO
          RETURN

..............................................................................
. ENTRY POINT : NREFAIM
. REQUIRED    : NREFFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREFAIM
          BRANCH    NREFFLAG,NREF8
          CALL      NREFOPEN
NREF8   
          trap      IOMssg giving Error if IO          
          BRANCH     nrefLOCK,nref8L,nref8R,nref8N
NREF8L
          FILEPI    1;NREFFLE2
          READ      NREFFLE2,NREFFLD1,Nreffld5;NREFVARS
          trapclr   IO
          RETURN
NREF8R
          READLK     NREFFLE2,NREFFLD1,Nreffld5;NREFVARS
          trapclr   IO
          RETURN
NREF8N
          READ      NREFFLE2,NREFFLD1,Nreffld5;NREFVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NREFKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NREFKG
          BRANCH    NREFFLAG,NREF10
          CALL      NREFOPEN
NREF10
          trap      IOMssg giving Error if IO
          BRANCH     nrefLOCK,nref10L,nref10R,nref10N
NREF10L
          FILEPI    1;NREFFLE2
          READKG    NREFFLE2;NREFVARS
          trapclr   IO
          RETURN
NREF10R
          READKGLK    NREFFLE2;NREFVARS
          trapclr   IO
          RETURN
NREF10N
          READKG    NREFFLE2;NREFVARS
          trapclr   IO
          RETURN

...............................................................................
.
. ENTRY POINT : NREFOPEN
. REQUIRED    : 'NREFFLAG'
. RETURNED    : 'NREFFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD REFERANCE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NREFOPEN
          TRAP      NREFGONE giving Error if IO
          OPEN      NREFFLIST
          trapclr   IO
          MOVE      C1,NREFFLAG
          RETURN
.
NREFGONE
          MOVE      NREFNAME,FILENAME
          CALL      FILEGONE
.
..............................................................................

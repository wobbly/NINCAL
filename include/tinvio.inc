..............................................................................
.
. TINVIO INCLUSION
. TDMC INvoice FILE I/O ROUTINES
.
.
.
. ENTRY POINT : TINVKEY
. REQUIRED    : 'TINVFLD'
. RETURNED    : invoice RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
TINVKEY  BRANCH    TINVFLaG TO TINV1
         CALL      TINVOPEN
TINV1    trap      IOMssg giving Error if IO
         branch    TINVLOCK to TINV1L,TINV1R,TINV1N
TINV1L   FILEPI    1;TINVFiLE
         READ      TINVFiLE,TINVFLD;TINVVARS
         trapclr   IO
         RETURN
TINV1R   READLK    TINVFiLE,TINVFLD;TINVVARS
         trapclr   IO
         RETURN
TINV1N   READ      TINVFiLE,TINVFLD;TINVVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TINVTST
. REQUIRED    : TINVFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
TINVTST  BRANCH    TINVflag TO TINV2
         CALL      TINVOPEN
TINV2    trap      IOMssg giving Error if IO
         branch    TINVLOCK to TINV2L,TINV2R,TINV2N 
TINV2L   FILEPI    1;TINVfile
         READ      TINVfile,TINVFLD;STR1
         trapclr   IO
         RETURN
TINV2R   READLK    TINVfile,TINVFLD;STR1
         trapclr   IO
         RETURN
TINV2N   READ      TINVfile,TINVFLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TINVKS
. REQUIRED    :
. RETURNED    : tdmc invoice RECORD
. DESCRIPTION : KEY SEQUENTIAL tdmc invoice FILE READ
.
TINVKS   BRANCH    TINVflag TO TINV3
         CALL      TINVOPEN
TINV3    trap      IOMssg giving Error if IO
         branch    TINVLOCK to TINV3L,TINV3R,TINV3N
TINV3L   FILEPI    1;TINVfile
         READKS    TINVfile;tinvvars
         trapclr   IO
         RETURN
TINV3R   READKSLK  TINVfile;tinvvars
         trapclr   IO
         RETURN
TINV3N   READKS    TINVfile;tinvvars
         trapclr   IO
         RETURN
..............................................................................
. ENTRY POINT : TINVSEQ
. REQUIRED    :
. RETURNED    : tdmc invoice RECORD
. DESCRIPTION : SEQUENTIAL tdmc invoice FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
TINVSEQ  
TINV4A   BRANCH    TINVflag TO TINV4B
         CALL      TINVOPEN
TINV4B   trap      IOMssg giving Error if IO
         branch    TINVLOCK to TINV4BL,TINV4BR,TINV4BN
TINV4BL  FILEPI    1;TINVfile
         READ      TINVfile,SEQ;tinvvars
         trapclr   IO
         RETURN
TINV4BR  READLK    TINVfile,SEQ;tinvvars
         trapclr   IO
         RETURN
TINV4BN  READ      TINVfile,SEQ;tinvvars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TINVWRT
. REQUIRED    : 'TINVFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
TINVWRT  BRANCH    TINVflag TO TINV5
         CALL      TINVOPEN
TINV5    trap      IOMssg giving Error if IO
         FILEPI    1;TINVfile
         WRITE     TINVfile,TINVFLD;tinvvars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TINVUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE tdmc invoice FILE
.
TINVUPD  BRANCH    TINVflag TO TINV6
         CALL      TINVOPEN
TINV6    trap      IOMssg giving Error if IO
         FILEPI    1;TINVfile
         UPDATE    TINVfile;tinvvars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : TINVDEL
. REQUIRED    : 'TINVFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
TINVDEL  BRANCH    TINVflag TO TINV7
         CALL      TINVOPEN
TINV7    trap      IOMssg giving Error if IO
         FILEPI    1;TINVfile
         DELETE    TINVfile,TINVFLD
         trapclr   IO
         RETURN
..............................................................................
. 
. ENTRY POINT : TINVOPEN
. REQUIRED    : 'TINVflag' 'TINVPATH'
. RETURNED    : 'TINVflag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  tdmc invoice FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
TINVOPEN 
TINVOPN1 TRAP      TINVGONE IF IO
         OPEN      TINVfile,TINVNAME
         TRAPCLR   IO
         MOVE      C1 TO TINVflag
         RETURN
..............................................................................
..............................................................................
TINVGONE MOVE      TINVNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


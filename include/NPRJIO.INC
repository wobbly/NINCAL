..............................................................................
.
. NPRJIO INCLUSION
. NINCAL PROJECTION FILE DEFINITION
.
. FILE NAME : PROJDOLR.DAT
.
. REC LENGTH:       350 FIXED
. INDEX KEY :       001-014   Type/Source/Mailer/Year/Key
. AAMKEY: 003-008   Mailer
.                   009-012   Year
..............................................................................
.NPRJKEY - Isam key read
.
. ENTRY POINT : NPRJKEY
. REQUIRED    : 'NPRJFLD'
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJKEY  BRANCH    NPRJFLAG TO NPRJ1
         CALL      NPRJOPEN
NPRJ1    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJFILE
         READ      NPRJFILE,NPRJFLD;PrjVars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJTST
. REQUIRED    : 'NPRJFLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPRJTST  BRANCH    NPRJFLAG TO NPRJ2
         CALL      NPRJOPEN
NPRJ2    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJFILE
         READ      NPRJFILE,NPRJFLD;;
           trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJKS
. REQUIRED    :
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : KEY SEQUENTIAL PROJECTION FILE READ
.
NPRJKS   BRANCH    NPRJFLAG TO NPRJ3
         CALL      NPRJOPEN
NPRJ3    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJFILE
         READKS    NPRJFILE;PrjVars
           trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJSEQ
. REQUIRED    :
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : SEQUENTIAL PROJECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJSEQ  BRANCH    NPRJFLG2 TO NPRJ4
         CALL      NPRJOPN2
NPRJ4    trap      IOMssg giving Error if IO
         READ      NPRJFLSQ,SEQ;PrjVars
           trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPRJWRT
         BRANCH    NPRJFLAG OF NPRJ5
         CALL      NPRJOPEN
NPRJ5    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJLIST
         WRITE     NPRJLIST;PrjVars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PROJECTION FILE
.
NPRJUPD  BRANCH    NPRJFLAG TO NPRJ6
         CALL      NPRJOPEN
NPRJ6    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJLIST
         UPDATE    NPRJLIST;PrjVars
           trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJDEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPRJDEL  BRANCH    NPRJFLAG TO NPRJ7
         CALL      NPRJOPEN
NPRJ7    trap      IOMssg giving Error if IO
           FILEPI    1;NPRJFILE
         DELETE    NPRJLIST
           trapclr   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NPRJAIM
. REQUIRED    : NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : AIM PROJECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJAIM  BRANCH    NPRJFLAG TO NPRJ9
         CALL      NPRJOPEN
NPRJ9    trap      IOMssg giving Error if IO
         branch    NPRJLOCK to NPRJ9L,NPRJ9R,NPRJ9N
NPRJ9L   FILEPI    1;NPRJFLE2
         READ      NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;PrjVars
         trapclr   IO
         RETURN
NPRJ9R   
         READLK    NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;PrjVars
         trapclr   IO
         RETURN
NPRJ9N
         READ      NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;PrjVars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJAIMT
. REQUIRED    : NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4
. RETURNED    : NADA
. DESCRIPTION : AIM PROJECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJAIMT BRANCH    NPRJFLAG TO NPRJ9T
         CALL      NPRJOPEN
NPRJ9T   trap      IOMssg giving Error if IO
         branch    NPRJLOCK to NPRJ9TL,NPRJ9TR,NPRJ9TN
NPRJ9TL  FILEPI    1;NPRJFLE2
         READ      NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;;
         trapclr   IO
         RETURN
NPRJ9TR   
         READLK    NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;;
         trapclr   IO
         RETURN
NPRJ9TN
         READ      NPRJFLE2,NPRJFLD1,NPRJFLD2,NPRJFLD3,NPRJFLD4,NPRJFLD5;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : AIM KEY GENEREIC PROJECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJKG   BRANCH    NPRJFLAG TO NPRJ10
         CALL      NPRJOPEN
NPRJ10   trap      IOMssg giving Error if IO
         branch    NPRJLOCK to NPRJ10L,NPRJ10R,NPRJ10N
NPRJ10L  FILEPI    1;NPRJFLE2
         READKG    NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
NPRJ10R  READKGLK  NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
NPRJ10N  READKG    NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : PROJECTION RECORD
. DESCRIPTION : KEY GENERIC PRIOR PROJECTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRJKGP  BRANCH    NPRJFLAG TO NPRJ11
         CALL      NPRJOPEN
NPRJ11   trap      IOMssg giving Error if IO
         branch    NPRJLOCK to NPRJ11L,NPRJ11R,NPRJ11N
NPRJ11L  FILEPI    1;NPRJFLE2
         READKGP   NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
NPRJ11R  READKGPLK NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
NPRJ11N  READKGP   NPRJFLE2;PrjVars
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRJOPEN
. REQUIRED    : 'NPRJFLAG'
. RETURNED    : 'NPRJFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PROJECTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPRJOPEN TRAP      NPRJGONE giving Error IF IO
           BRANCH    NPRJLOCK TO NPRJOPNL,NPRJOPNR

NPRJOPNL OPEN          NPRJLIST
         TRAPCLR   IO
         MOVE      C1 TO NPRJFLAG
         RETURN
NPRJOPNR
        OPEN      NPRJLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NPRJFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : NPRJOPN2
. REQUIRED    : 'NPRJFLG2'
. RETURNED    : 'NPRJFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PROJECTION SEQUENTIAL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPRJOPN2
          TRAP      NPRJGONE giving Error IF IO
          BRANCH    NPRJLOCK TO NPRJOPN2L,NPRJOPN2R

NPRJOPN2L
          OPEN        NPRJFLSQ,NPRJNAME
        TRAPCLR   IO
        MOVE      C1 TO NPRJFLG2
        RETURN
NPRJOPN2R
        OPEN      NPRJFLSQ,NPRJNAME,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NPRJFLG2
        RETURN
.
NPRJGONE MOVE      NPRJNAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE

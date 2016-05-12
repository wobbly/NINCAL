..............................................................................
.
. NPRCIO INCLUSION
. NINCAL PRICE FILE DEFINITION
.
. FILE NAME : NINPRC.DAT
.
. REC LENGTH:	355 FIXED
. INDEX KEY :	001-018	Mailer/PRICE Number/Price Date
. AAMKEY:	001-004	Mailer
.		005-010	PRICE Number
..............................................................................
.NPRCKEY - Isam key read
.
. ENTRY POINT : NPRCKEY
. REQUIRED    : 'NPRCFLD'
. RETURNED    : PRICE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRCKEY  BRANCH    NPRCFLAG TO NPRC1
         CALL      NPRCOPEN
NPRC1    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCFILE
         READ      NPRCFILE,NPRCFLD;NPRCVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCTST
. REQUIRED    : 'NPRCFLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPRCTST  BRANCH    NPRCFLAG TO NPRC2
         CALL      NPRCOPEN
NPRC2    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCFILE
         READ      NPRCFILE,NPRCFLD;;
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCKS
. REQUIRED    :
. RETURNED    : PRICE RECORD
. DESCRIPTION : KEY SEQUENTIAL PRICE FILE READ
.
NPRCKS   BRANCH    NPRCFLAG TO NPRC3
         CALL      NPRCOPEN
NPRC3    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCFILE
         READKS    NPRCFILE;NPRCVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCSEQ
. REQUIRED    :
. RETURNED    : PRICE RECORD
. DESCRIPTION : SEQUENTIAL PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NPRCSEQ  BRANCH    NPRCFLAG TO NPRC4
.         CALL      NPRCOPEN
.NPRC4    trap      IOMssg giving Error if IO
.	 FILEPI    1;NPRCFILE
.         READ      NPRCFLE3,SEQ;NPRCVARS
.	 trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NPRCWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPRCWRT
         BRANCH    NPRCFLAG OF NPRC5
         CALL      NPRCOPEN
NPRC5    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCLIST
         WRITE     NPRCLIST;NPRCVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PRICE FILE
.
NPRCUPD  BRANCH    NPRCFLAG TO NPRC6
         CALL      NPRCOPEN
NPRC6    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCLIST
         UPDATE    NPRCLIST;NPRCVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCDEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPRCDEL  BRANCH    NPRCFLAG TO NPRC7
         CALL      NPRCOPEN
NPRC7    trap      IOMssg giving Error if IO
	 FILEPI    1;NPRCFILE
         DELETE    NPRCLIST
	 trapclr   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NPRCAIM
. REQUIRED    : NPRCFLD1,NPRCFLD2
. RETURNED    : PRICE RECORD
. DESCRIPTION : AIM PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRCAIM  BRANCH    NPRCFLAG TO NPRC9
         CALL      NPRCOPEN
NPRC9    trap      IOMssg giving Error if IO
         branch    NPRCLOCK to NPRC9L,NPRC9R,NPRC9N
NPRC9L   FILEPI    1;NPRCFLE2
         READ      NPRCFLE2,NPRCFLD1,NPRCFLD2;NPRCVARS
         trapclr   IO
         RETURN
NPRC9R   
         READLK    NPRCFLE2,NPRCFLD1,NPRCFLD2;NPRCVARS
         trapclr   IO
         RETURN
NPRC9N
         READ      NPRCFLE2,NPRCFLD1,NPRCFLD2;NPRCVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : PRICE RECORD
. DESCRIPTION : AIM KEY GENEREIC PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRCKG   BRANCH    NPRCFLAG TO NPRC10
         CALL      NPRCOPEN
NPRC10   trap      IOMssg giving Error if IO
         branch    NPRCLOCK to NPRC10L,NPRC10R,NPRC10N
NPRC10L  FILEPI    1;NPRCFLE2
         READKG    NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
NPRC10R  READKGLK  NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
NPRC10N  READKG    NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : PRICE RECORD
. DESCRIPTION : KEY GENERIC PRIOR PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRCKGP  BRANCH    NPRCFLAG TO NPRC11
         CALL      NPRCOPEN
NPRC11   trap      IOMssg giving Error if IO
         branch    NPRCLOCK to NPRC11L,NPRC11R,NPRC11N
NPRC11L  FILEPI    1;NPRCFLE2
         READKGP   NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
NPRC11R  READKGPLK NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
NPRC11N  READKGP   NPRCFLE2;NPRCVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRCOPEN
. REQUIRED    : 'NPRCFLAG'
. RETURNED    : 'NPRCFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PRICE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPRCOPEN TRAP      NPRCGONE giving Error IF IO
	 BRANCH    NPRCLOCK TO NPRCOPNL,NPRCOPNR

NPRCOPNL OPEN 	   NPRCLIST
         TRAPCLR   IO
         MOVE      C1 TO NPRCFLAG
         RETURN
NPRCOPNR
        OPEN      NPRCLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NPRCFLAG
        RETURN
.
NPRCGONE MOVE      NPRCNAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE

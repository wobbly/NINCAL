..............................................................................
.
. NFULIO INCLUSION
. NINCAL FULFILLMENT FILE DEFINITION
.
. FILE NAME : NINFUL.DAT
.
. REC LENGTH:	273 FIXED
. INDEX KEY :	001-004	Fulfillment Number
. AAMKEY:	005-049	Company Name
.		050-094	Contact Name
.		095-104 Telephone Number
..............................................................................
.NFULKEY - Isam key read
.
. ENTRY POINT : NFULKEY
. REQUIRED    : 'NFULFLD'
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULKEY  BRANCH    NFULFLAG TO NFUL1
         CALL      NFULOPEN
NFUL1    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULFILE
         READ      NFULFILE,NFULFLD;NFULVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULTST
. REQUIRED    : 'NFULFLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NFULTST  BRANCH    NFULFLAG TO NFUL2
         CALL      NFULOPEN
NFUL2    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULFILE
         READ      NFULFILE,NFULFLD;;
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULKS
. REQUIRED    :
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : KEY SEQUENTIAL FULFILLMENT FILE READ
.
NFULKS   BRANCH    NFULFLAG TO NFUL3
         CALL      NFULOPEN
NFUL3    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULFILE
         READKS    NFULFILE;NFULVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULSEQ
. REQUIRED    :
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : SEQUENTIAL FULFILLMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULSEQ  BRANCH    NFULFLG2 TO NFUL4
         CALL      NFULOPN2
NFUL4    trap      IOMssg giving Error if IO
         READ      NFULFLSQ,SEQ;NFULVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NFULWRT
         BRANCH    NFULFLAG OF NFUL5
         CALL      NFULOPEN
NFUL5    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULLIST
         WRITE     NFULLIST;NFULVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE FULFILLMENT FILE
.
NFULUPD  BRANCH    NFULFLAG TO NFUL6
         CALL      NFULOPEN
NFUL6    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULLIST
         UPDATE    NFULLIST;NFULVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULDEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NFULDEL  BRANCH    NFULFLAG TO NFUL7
         CALL      NFULOPEN
NFUL7    trap      IOMssg giving Error if IO
	 FILEPI    1;NFULFILE
         DELETE    NFULLIST
	 trapclr   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NFULAIM
. REQUIRED    : NFULFLD1,NFULFLD2,NFULFLD3
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : AIM FULFILLMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULAIM  BRANCH    NFULFLAG TO NFUL9
         CALL      NFULOPEN
NFUL9    trap      IOMssg giving Error if IO
         branch    NFULLOCK to NFUL9L,NFUL9R,NFUL9N
NFUL9L   FILEPI    1;NFULFLE2
         READ      NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;NFULVARS
         trapclr   IO
         RETURN
NFUL9R   
         READLK    NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;NFULVARS
         trapclr   IO
         RETURN
NFUL9N
         READ      NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;NFULVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULAIMT
. REQUIRED    : NFULFLD1,NFULFLD2,NFULFLD3
. RETURNED    : NADA
. DESCRIPTION : AIM FULFILLMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULAIMT BRANCH    NFULFLAG TO NFUL9T
         CALL      NFULOPEN
NFUL9T   trap      IOMssg giving Error if IO
         branch    NFULLOCK to NFUL9TL,NFUL9TR,NFUL9TN
NFUL9TL  FILEPI    1;NFULFLE2
         READ      NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;;
         trapclr   IO
         RETURN
NFUL9TR   
         READLK    NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;;
         trapclr   IO
         RETURN
NFUL9TN
         READ      NFULFLE2,NFULFLD1,NFULFLD2,NFULFLD3;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : AIM KEY GENEREIC FULFILLMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULKG   BRANCH    NFULFLAG TO NFUL10
         CALL      NFULOPEN
NFUL10   trap      IOMssg giving Error if IO
         branch    NFULLOCK to NFUL10L,NFUL10R,NFUL10N
NFUL10L  FILEPI    1;NFULFLE2
         READKG    NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
NFUL10R  READKGLK  NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
NFUL10N  READKG    NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : FULFILLMENT RECORD
. DESCRIPTION : KEY GENERIC PRIOR FULFILLMENT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NFULKGP  BRANCH    NFULFLAG TO NFUL11
         CALL      NFULOPEN
NFUL11   trap      IOMssg giving Error if IO
         branch    NFULLOCK to NFUL11L,NFUL11R,NFUL11N
NFUL11L  FILEPI    1;NFULFLE2
         READKGP   NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
NFUL11R  READKGPLK NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
NFUL11N  READKGP   NFULFLE2;NFULVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NFULOPEN
. REQUIRED    : 'NFULFLAG'
. RETURNED    : 'NFULFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN FULFILLMENT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NFULOPEN TRAP      NFULGONE giving Error IF IO
	 BRANCH    NFULLOCK TO NFULOPNL,NFULOPNR

NFULOPNL OPEN 	   NFULLIST
         TRAPCLR   IO
         MOVE      C1 TO NFULFLAG
         RETURN
NFULOPNR
        OPEN      NFULLIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NFULFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : NFULOPN2
. REQUIRED    : 'NFULFLG2'
. RETURNED    : 'NFULFLG2' SET TO '1' IF OPENED
. DESCRIPTION : OPEN NIN FULFILLMENT SEQUENTIAL FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NFULOPN2
	TRAP      NFULGONE giving Error IF IO
	BRANCH    NFULLOCK TO NFULOPN2L,NFULOPN2R

NFULOPN2L
	OPEN	  NFULFLSQ,NFULNAME
        TRAPCLR   IO
        MOVE      C1 TO NFULFLG2
        RETURN
NFULOPN2R
        OPEN      NFULFLSQ,NFULNAME,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NFULFLG2
        RETURN
.
NFULGONE MOVE      NFULNAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE

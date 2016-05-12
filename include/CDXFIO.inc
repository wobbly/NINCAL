..............................................................................
.
. CDXFIO INCLUSION
. NIN Data Transfer FILE DEFINITION
.
. FILE NAME : CompCDXF.DAT
.
. REC LENGTH:	437 FIXED
. INDEX KEY :	001-005	Company Number
..............................................................................
.CDXFKEY - Isam key read
.
. ENTRY POINT : CDXFKEY
. REQUIRED    : 'CDXFFLD'
. RETURNED    : Data Transfer RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
CDXFKEY  	BRANCH    CDXFFLAG TO CDXF1
         	CALL      CDXFOPEN
CDXF1    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFILE
         	READ      CDXFFILE,CDXFFLD;CDXFVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFTST
. REQUIRED    : 'CDXFFLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
CDXFTST  	BRANCH    CDXFFLAG TO CDXF2
         	CALL      CDXFOPEN
CDXF2    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFILE
         	READ      CDXFFILE,CDXFFLD;;
	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFKS
. REQUIRED    :
. RETURNED    : Data Transfer RecORD
. DESCRIPTION : KEY SEQUENTIAL Data TransferFILE READ
.
CDXFKS   	BRANCH    CDXFFLAG TO CDXF3
         	CALL      CDXFOPEN
CDXF3    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFILE
         	READKS    CDXFFILE;CDXFVARS
	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFSEQ
. REQUIRED    :
. RETURNED    : Data Transfer RecORD
. DESCRIPTION : SEQUENTIAL Data TransferFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
CDXFSEQ  	BRANCH    CDXFFLaG TO CDXF4
         	CALL      CDXFOPeN
CDXF4    	trap      IOMssg giving Error if IO
         	READ      CDXFFILE,SEQ;CDXFVARS
	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
CDXFWRT
         	BRANCH    CDXFFLAG OF CDXF5
         	CALL      CDXFOPEN
CDXF5    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFLIST
         	WRITE     CDXFFLIST;CDXFVARS
         	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE Data TransferFILE
.
CDXFUPD  	BRANCH    CDXFFLAG TO CDXF6
         	CALL      CDXFOPEN
CDXF6    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFLIST
         	UPDATE    CDXFFLIST;CDXFVARS
	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFDEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
CDXFDEL  	BRANCH    CDXFFLAG TO CDXF7
         	CALL      CDXFOPEN
CDXF7    	trap      IOMssg giving Error if IO
	FILEPI    1;CDXFFILE
         	DELETE    CDXFFLIST
	trapclr   IO
         	RETURN
..............................................................................
.
. ENTRY POINT : CDXFOPEN
. REQUIRED    : 'CDXFFLAG'
. RETURNED    : 'CDXFFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN Data TransferFILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
CDXFOPEN 	TRAP      CDXFGONE giving Error IF IO
	BRANCH    CDXFLOCK TO CDXFOPNL,CDXFOPNR

CDXFOPNL 	OPEN      CDXFFLIST
         	TRAPCLR   IO
         	MOVE      C1 TO CDXFFLAG
         	RETURN
CDXFOPNR
        	OPEN      CDXFFLIST,LOCKMANUAL,SINGLE
        	TRAPCLR   IO
        	MOVE      C1 TO CDXFFLAG
        	RETURN
..............................................................................
.
.
CDXFGONE 	MOVE      CDXFNAME TO FILENAME
         	CALL      FILEGONE
..............................................................................
.END OF INCLUDE

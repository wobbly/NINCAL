..............................................................................
.
. NSPE2IO INCLUSION
.
. FILE NAME : NINSPEC2
. REC LENGTH: 1006
. INDEX KEY : 1-6 
.
. Last Update Added logic to thwart I44 errors for defunct Key values - forcing setting of OVER flag - ASH 5/20/04
..............................................................................
.
. ENTRY POINT : NSPE2KEY
. REQUIRED    : 'NSPE2FLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPE2KEY
.START PATCH 5/20/2004 ADDED LOGIC - ASH
	 call      Trim using NSPE2FLD
	 if (NSPE2FLD = "")
		   flagrestore	"0001"
		   return
	 endif
.END PATCH 5/20/2004 ADDED LOGIC - ASH
         BRANCH    NSPE2FLAG TO NSPE21
         CALL      NSPE2OPEN
         
NSPE21   trap      IOMssg giving Error if IO
         branch    nspe2lock of nspe21L,Nspe21R,Nspe21N
.default FIlepi's active
Nspe21L  FILEPI    1;NSPE2FILE
         READ      NSPE2FILE,NSPE2FLD;NSPE2VARS
         trapclr   IO
         RETURN
.with record locks . note file open must have had flag set
Nspe21R
         READLK    NSPE2FILE,NSPE2FLD;NSPE2VARS
         trapclr  IO
         RETURN
.No locks
Nspe21N
         READ      NSPE2FILE,NSPE2FLD;NSPE2VARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2TST
. REQUIRED    : NSPE2FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSPE2TST BRANCH    NSPE2FLAG TO NSPE22
         CALL      NSPE2OPEN
NSPE22   trap      IOMssg giving Error if IO
         FILEPI    1;NSPE2FILE
         READ      NSPE2FILE,NSPE2FLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2KS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
NSPE2KS  BRANCH    NSPE2FLAG TO NSPE23
         CALL      NSPE2OPEN
NSPE23   trap      IOMssg giving Error if IO
         BRANCH    NSPE2LOCK OF NSPE23L,NSPE23R,NSPE23N

NSPE23L  FILEPI    1;NSPE2FILE
         READKS    NSPE2FILE;NSPE2VARS
         trapclr   IO
         RETURN
NSPE23R
         READKSLK  NSPE2FILE;NSPE2VARS
         trapclr   IO
         RETURN
NSPE23N
         READKS    NSPE2FILE;NSPE2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2SEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSPE2SEQ BRANCH    NSPE2FLAG TO NSPE24
         CALL      NSPE2OPEN
NSPE24   trap      IOMssg giving Error if IO
         BRANCH    NSPE2LOCK OF NSPE24L,NSPE24R,NSPE24N
NSPE24L  FILEPI    1;NSPE2FILE
         READ      NSPE2FILE,SEQ;NSPE2VARS
         trapclr   IO
         RETURN
NSPE24R
         READLK    NSPE2FILE,SEQ;NSPE2VARS
         trapclr   IO
         RETURN
NSPE24N
         READ      NSPE2FILE,SEQ;NSPE2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2WRT
. REQUIRED    : NSPE2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSPE2WRT BRANCH    NSPE2FLAG OF NSPE25
         CALL      NSPE2OPEN
NSPE25   trap      IOMssg giving Error if IO
         BRANCH    NSPE2LOCK TO NSPE25L,NSPE25R
NSPE25L  FILEPI    1;NSPE2FILE
         WRITE     NSPE2FILE,NSPE2FLD;*+,NSPE2VARS
         trapclr   IO
         RETURN
NSPE25R
         WRITE     NSPE2FILE,NSPE2FLD;*+,NSPE2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2DEL
. REQUIRED    : NSPE2FLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSPE2DEL BRANCH    NSPE2FLAG TO NSPE27
         CALL      NSPE2OPEn
NSPE27   trap      IOMssg giving Error if IO
         FILEPI    1;NSPE2FILE
         DELETE    NSPE2FILE,NSPE2FLD
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSPE2OPEN
. REQUIRED    : 'NSPE2FLAG'
. RETURNED    : 'NSPE2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSPE2OPEN TRAP     NSPE2GONE IF IO
         BRANCH    NSPE2LOCK TO NSPE2OPNL,NSPE2OPNR

NSPE2OPNL OPEN     NSPE2FILE,NSPE2NAME
         TRAPCLR   IO
         MOVE      C1 TO NSPE2FLAG
         RETURN
NSPE2OPNR OPEN     NSPE2FILE,NSPE2NAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NSPE2FLAG
         RETURN

.
NSPE2GONE MOVE     NSPE2NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................


..............................................................................
.
. NPRC2IO INCLUSION
. NINCAL OTHER PRICE FILE DEFINITION
.
. FILE NAME : NINPRC2.DAT
.
. REC LENGTH:	105 FIXED
. INDEX KEY :	001-021	Mailer/Package Number/Other Price Date/Other Code
. AAMKEY:	001-004	Mailer Number
.		005-010	Package Number
.		011-018	Other Price Date
..............................................................................
.NPRC2KEY - Isam key read
.
. ENTRY POINT : NPRC2KEY
. REQUIRED    : 'NPRC2FLD'
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRC2KEY BRANCH    NPRC2FLAG TO NPRC21
         CALL      NPRC2OPEN
NPRC21   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2FILE
         READ      NPRC2FILE,NPRC2FLD;NPRC2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2TST
. REQUIRED    : 'NPRC2FLD'
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPRC2TST BRANCH    NPRC2FLAG TO NPRC22
         CALL      NPRC2OPEN
NPRC22   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2FILE
         READ      NPRC2FILE,NPRC2FLD;;
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2KS
. REQUIRED    :
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : KEY SEQUENTIAL OTHER PRICE FILE READ
.
NPRC2KS  BRANCH    NPRC2FLAG TO NPRC23
         CALL      NPRC2OPEN
NPRC23   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2FILE
         READKS    NPRC2FILE;NPRC2VARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2SEQ
. REQUIRED    :
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : SEQUENTIAL OTHER PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.NPRC2SEQ  BRANCH    NPRC2FLAG TO NPRC24
.         CALL      NPRC2OPEN
.NPRC24    trap      IOMssg giving Error if IO
.	 FILEPI    1;NPRC2FILE
.         READ      NPRC2FLE3,SEQ;NPRC2VARS
.	 trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2WRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPRC2WRT
         BRANCH    NPRC2FLAG OF NPRC25
         CALL      NPRC2OPEN
NPRC25   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2LIST
         WRITE     NPRC2LIST;NPRC2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE OTHER PRICE FILE
.
NPRC2UPD BRANCH    NPRC2FLAG TO NPRC26
         CALL      NPRC2OPEN
NPRC26   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2LIST
         UPDATE    NPRC2LIST;NPRC2VARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2DEL
. REQUIRED    : VALID FILE READ
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPRC2DEL BRANCH    NPRC2FLAG TO NPRC27
         CALL      NPRC2OPEN
NPRC27   trap      IOMssg giving Error if IO
	 FILEPI    1;NPRC2FILE
         DELETE    NPRC2LIST
	 trapclr   IO
         RETURN

..............................................................................
.
. ENTRY POINT : NPRC2AIM
. REQUIRED    : NPRC2FLD1,NPRC2FLD2,NPRC2FLD3
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : AIM OTHER PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRC2AIM BRANCH    NPRC2FLAG TO NPRC29
         CALL      NPRC2OPEN
NPRC29   trap      IOMssg giving Error if IO
         branch    NPRC2LOCK to NPRC29L,NPRC29R,NPRC29N
NPRC29L  FILEPI    1;NPRC2FLE2
         READ      NPRC2FLE2,NPRC2FLD1,NPRC2FLD2,NPRC2FLD3;NPRC2VARS
         trapclr   IO
         RETURN
NPRC29R   
         READLK    NPRC2FLE2,NPRC2FLD1,NPRC2FLD2,NPRC2FLD3;NPRC2VARS
         trapclr   IO
         RETURN
NPRC29N
         READ      NPRC2FLE2,NPRC2FLD1,NPRC2FLD2,NPRC2FLD3;NPRC2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : AIM KEY GENEREIC OTHER PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRC2KG  BRANCH    NPRC2FLAG TO NPRC210
         CALL      NPRC2OPEN
NPRC210  trap      IOMssg giving Error if IO
         branch    NPRC2LOCK to NPRC210L,NPRC210R,NPRC210N
NPRC210L FILEPI    1;NPRC2FLE2
         READKG    NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
NPRC210R READKGLK  NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
NPRC210N READKG    NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2KGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : OTHER PRICE RECORD
. DESCRIPTION : KEY GENERIC PRIOR OTHER PRICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPRC2KGP BRANCH    NPRC2FLAG TO NPRC211
         CALL      NPRC2OPEN
NPRC211  trap      IOMssg giving Error if IO
         branch    NPRC2LOCK to NPRC211L,NPRC211R,NPRC211N
NPRC211L FILEPI    1;NPRC2FLE2
         READKGP   NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
NPRC211R READKGPLK NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
NPRC211N READKGP   NPRC2FLE2;NPRC2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPRC2OPEN
. REQUIRED    : 'NPRC2FLAG'
. RETURNED    : 'NPRC2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OTHER PRICE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPRC2OPEN 
	 TRAP      NPRC2GONE giving Error IF IO
	 BRANCH    NPRC2LOCK TO NPRC2OPNL,NPRC2OPNR

NPRC2OPNL
	 OPEN 	   NPRC2LIST
         TRAPCLR   IO
         MOVE      C1 TO NPRC2FLAG
         RETURN
NPRC2OPNR
        OPEN      NPRC2LIST,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO NPRC2FLAG
        RETURN
.
NPRC2GONE
	 MOVE      NPRC2NAME TO FILENAME
         CALL      FILEGONE
..............................................................................
.END OF INCLUDE

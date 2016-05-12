...................................................
.
. NCMPDD INCLUSION
. NIN supplimental (Campaign) ORDER FILE DEFINITION
.          one record per campaign
. FILE NAME : NINCMP.DAT
. REC LENGTH: 535 FIXED
. INDEX KEY :   (1)     001-006 (Campaign Number)
.
. AAM KEYS  :   (1)     007-051 (Campaign Name)
.               (2)     052-057 (Mailer Number)
.               (3)     058-066 (Broker Number/Cnt)
.               (4)     067-078 (P.O. Number)
.               (5)     079-079 (Campaign Status)
.
. Patch 1.2 DMB 06/18/2005  Changed IP of File Manager
. Patch 1.1 ASH 11/23/2004  Increased Mailer/Broker and converted to Company Number
.			    Increased Ship-to to prep for future conversion
.
NCMPNAME        DIM     8
NCMPNME1        INIT    "NINCMP"
.START PATCH 1.1 REPLACED LOGIC
.NCMPFILE        IFILE   KEYLEN=6,FIXED=535
.NCMPFLE2        AFILE   FIXED=535
.NCMPNME2        INIT    "NINCMP"
.>Patch 1.2 Begin
NCMPFLIST	FileList
.NCMPFILE        IFILE   KEYLEN=6,FIXED=535,uncomp,Name="NINCMP.ISI"
.NCMPFLE2        AFILE   FIXED=535,uncomp,Name="NINCMP.AAM"
.NCMPFILE        IFILE   KEYLEN=6,FIXED=535,uncomp,Name="NINCMP.ISI|20.20.30.104:502"
NCMPFILE        IFILE   KEYLEN=6,FIXED=535,uncomp,Name="NINCMP.ISI|10.10.30.104:502"
.NCMPFLE2        AFILE   FIXED=535,uncomp,Name="NINCMP.AAM|20.20.30.104:502"
NCMPFLE2        AFILE   FIXED=535,uncomp,Name="NINCMP.AAM|10.10.30.104:502"
		FileListEnd
.>Patch 1.2 End		
.END PATCH 1.1 REPLACED LOGIC
NCMPFLD         DIM     6       Campaign Number(ISAM)
NCMPFLD1        DIM     48      Campaign Name(AAM)
.START PATCH 1.1 REPLACED LOGIC
.NCMPFLD2        DIM     7       Mailer Number(AAM)
.NCMPFLD3        DIM     10      Broker Number/Cnt(AAM)
NCMPFLD2        DIM     9       Mailer Number(AAM)
NCMPFLD3        DIM     12      Broker Number/Cnt(AAM)
.END PATCH 1.1 REPLACED LOGIC
NCMPFLD4        DIM     15      P.O. Number(AAM)
NCMPFLD5        DIM     5       Contact(AAM)
NCMPFLAG        FORM    1
.START PATCH 1.1 REMOVED LOGIC
.NCMPFLG2        FORM    1
.END PATCH 1.1 REMOVED LOGIC
NCMPPATH        FORM    1
NCMPLOCK        FORM    1       0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NCMPVARS        LIST
NCMPNum         DIM     6       001-006 Campaign Number(Key)
NCMPCName       DIM     45      007-051 Campaign Name(AAMKey)
.START PATCH 1.1 REPLACED LOGIC
.NCMPMlr         DIM     4       052-055 Mailer Number(AAMKey)
.NCMPBrk         DIM     4       056-059 Broker Number(AAMKey)
NCMPMlr         DIM     6       052-057 Mailer Number(AAMKey)
NCMPBrk         DIM     6       058-063 Broker Number(AAMKey)
.END PATCH 1.1 REPLACED LOGIC
NCMPBrkCnt      DIM     3       064-066 Broker Contact(AAMKey)
NCMPPO          DIM     12      067-078 P.O. Number(AAMKey)
NCMPStat        DIM     1       079-079 Campaign Status
.START PATCH 1.1 REPLACED LOGIC
.NCMPShipTo      DIM     4       080-083 Ship To Number
NCMPShipTo      DIM     6       080-085 Ship To Number
.END PATCH 1.1 REPLACED LOGIC
NCMPDate        DIM     8       086-093 Campaign Date
NCMPModDate     DIM     8       094-101 Modification Date
NCMPInits       DIM     3       102-104 Modification Initials
NCMPCnt         DIM     2       105-106 Contact(AAMKey)
NCMPPlanner     DIM     2       107-108 Planner
NCMPQty         DIM     13      109-121 Gross Campaign Qty
NCMPNetQty      DIM     13      122-134 Net Campaign Qty
NCMPRate        DIM     6       135-140 Response Rate Change%
NCMPGift        DIM     6       141-146 Gift Change%
.----------MASTER FIELDS-----------
NCMPOffer       DIM     3       147-149 Master Offer
NCMPMedia       DIM     2       150-151 Master Media
NCMPSample      DIM     3       152-154 Master Sample
NCMPShip        DIM     2       155-156 Master Shipping Method
NCMPKey         DIM     12      157-168 Master Key Info
NCMPMDate       DIM     8       169-176 Master Mail Date
NCMPRDate       DIM     8       177-184 Master Return Date
NCMPCDate       DIM     8       185-192 Master Cut-off Date
.
NCMPComment     DIM     300     193-492 Special Instructions
NCMPCode        DIM     1       493-493 Busy Byte
NCMPBill        DIM     1       494-494 Bill Direct Code
NCMPRpt		DIM	1	495-495 Projection Report Default
.START PATCH 1.1 REPLACED LOGIC
.NCMPFiller      DIM     46      490-535 Filler
NCMPFiller      DIM     40      496-535 Filler
.END PATCH 1.1 REPLACED LOGIC
                listend

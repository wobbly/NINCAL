...................................................
.
. NPRC2DD INCLUSION
. NIN PACKAGE OTHER PRICE FILE DEFINITION
. FILE NAME : NINPRC2.DAT
. REC LENGTH: 105 FIXED
. INDEX KEY :   (1)     001-023 (Mailer + Package Number + Price Date + Other Code)
.
. AAM KEYS  :   (1)     001-006 (Mailer Number)
.		(2)     007-012 (Package Number)
.		(3)	013-021 (Price Date)
.
.PATCH 1.1	ASH	13OCT2004	INCREASED MAILER NUMBER SIZE
.
NPRC2NAME       INIT    "NINPRC2"
NPRC2LIST	FILELIST
NPRC2FILE       IFILE   NAME="NINPRC2.ISI"
NPRC2FLE2       AFILE   NAME="NINPRC2.AAM"
		FILELISTEND
.START PATCH 1.1 REPLACED LOGIC
.NPRC2FLD        DIM     21	Mailer + Package Number + Price Date + Other Code(ISAM)
.NPRC2FLD1       DIM     7       Mailer Number(AAM)
NPRC2FLD        DIM     23	Mailer + Package Number + Price Date + Other Code(ISAM)
NPRC2FLD1       DIM     9       Mailer Number(AAM)
.END PATCH 1.1 REPLACED LOGIC
NPRC2FLD2       DIM     9       Package Number(AAM)
NPRC2FLD3       DIM     11      Price Date(AAM)
NPRC2FLAG       FORM    1
NPRC2PATH       FORM    1
NPRC2LOCK       FORM    1       0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.	  
NPRC2VARS	LIST
.START PATCH 1.1 REPLACED LOGIC
.NPRC2Mlr	DIM     4	001-004	Mailer Number(ISAM/AAM)
.NPRC2Num        DIM     6	005-010	Package Number(ISAM/AAM)
.NPRC2Date	DIM	8	011-018 Price Date 'CCYYMMDD' (ISAM/AAM) 
.NPRC2Code	DIM	3	019-021	Code (ISAM)
.NPRC2OtherName	DIM	50	022-071	Other Price Name
.NPRC2Other	FORM	6.2	072-080	Other Price
.NPRD2Filler	DIM	25	081-105	Filler
NPRC2Mlr	DIM     6	001-006	Mailer Number(ISAM/AAM)
NPRC2Num        DIM     6	007-012	Package Number(ISAM/AAM)
NPRC2Date	DIM	8	013-020 Price Date 'CCYYMMDD' (ISAM/AAM) 
NPRC2Code	DIM	3	021-023	Code (ISAM)
NPRC2OtherName	DIM	50	024-073	Other Price Name
NPRC2Other	FORM	6.2	074-082	Other Price
NPRD2Filler	DIM	23	083-105	Filler
.END PATCH 1.1 REPLACED LOGIC
                LISTEND

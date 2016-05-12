...................................................
.
. NLOL2DD INCLUSION
. NIN/Integral cross reference file for LOL file
. FILE NAME : NINLOL2.DAT
. REC LENGTH: 55 FIXED
. INDEX KEY :   (1)     001-025 (LOL Num + Integral LOL ID + Record Type Byte)
.
. AAMDEX KEY:   (1)     001-006 (LOL Num)
.               (2)     007-024 (Integral LOL ID)
.               (3)     025-025 (Record Type)
.               (4)     026-026 (Record Type 2)
.               (5)     027-027 (Update Byte)
.
.
NLOL2NAME	INIT	"NINLOL2"

NLOL2FLIST	Filelist
NLOL2FILE	IFILE   KEYLEN=25,FIXED=55,Name="NINLOL2"
NLOL2FLE1	AFILE   FIXED=55,Name="NINLOL2"
.NLOL2FILE	IFILE   KEYLEN=6,FIXED=55,Name="NINLOL2|20.20.30.103:502"
.NLOL2FLE1	AFILE   FIXED=55,Name="NINLOL2A|20.20.30.103:502"
		filelistend

NLOL2FLD	DIM	25	LOL Num + Integral LOL ID + Record Type Byte (ISAM)
NLOL2FLD1	DIM	9	LOL Num (AAM)
NLOL2FLD2	DIM	21	Integral LOL ID (AAM)
NLOL2FLD3	DIM	4	Record Type (AAM)
NLOL2FLD4	DIM	4	Record Type2 (AAM)
NLOL2FLD5	DIM	4	Update Byte (AAM)
NLOL2FLAG	FORM	1
NLOL2PATH	FORM	1
NLOL2LOCK	FORM	1	0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NLOL2VARS	LIST
NLOL2Num	DIM	6	001-006 LR/LOL Number(Key)
NLOL2INum	DIM	18	007-024 Integral LOL ID(Key)
NLOL2Type	DIM	1	025-025 Record Type:   '0'=LOL, '1'=LR/LCR (Key)
NLOL2Type2	DIM	1	026-026 Record Type 2: '2' = Conversion from LOL to LR, '3' = Deletion of LOL/Moving of LR from active Campaign
NLOL2Upd	DIM	1	027-027 Update Byte:  '0'=No Update necessary, '1'=Need to Update Integral
NLOL2UDate	DIM	8	028-035 Update Date
NLOL2Filler	DIM	20	036-055 Update Date
		listend
NLOL2Keyhold	DIM	25	Used for Conversion from LOL to LR records

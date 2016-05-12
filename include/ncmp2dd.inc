...................................................
.
. NCMP2DD INCLUSION
. NIN/Integral cross reference file for Campaign file
. FILE NAME : NINCMP2.DAT
. REC LENGTH: 53 FIXED
. INDEX KEY :   (1)     001-006 (Campaign Num)
.               (2)     007-024 (Integral Scenario ID)
. AAMDEX KEY:   (1)     001-006 (Campaign Num)
.               (2)     007-024 (Integral Campaign ID)
.               (3)     025-025 (Update Byte)
.
.
NCMP2NAME INIT      "NINCMP2 "
NCMP2NME1 INIT      "NINCMP2A"

NCMP2FLIST          Filelist
NCMP2FILE IFILE   KEYLEN=6,FIXED=53,Name="NINCMP2.isi|NINS1:502"
NCMP2FLE1 IFILE   KEYLEN=18,FIXED=53,Name="NINCMP2A.isi|NINS1:502"
NCMP2FLE2 AFILE   FIXED=53,Name="NINCMP2.aam|NINS1:502"
.NCMP2FILE          IFILE   KEYLEN=6,FIXED=53,Name="NINCMP2|20.20.30.103:502"
.NCMP2FLE1          IFILE   KEYLEN=18,FIXED=53,Name="NINCMP2A|20.20.30.103:502"
.NCMP2FLE2          AFILE   FIXED=53,Name="NINCMP2|20.20.30.103:502"
                    filelistend

NCMP2FLD  DIM       6         Campaign Number(ISAM)
NCMP2FLD1 DIM       18        Integral Scenario ID(ISAM)
NCMP2FLD2 DIM       9         Campaign Number(AAM)
NCMP2FLD3 DIM       21        Integral Scenario ID(AAM)
NCMP2FLD4 DIM       4         Update Byte(AAM)
NCMP2FLAG FORM      1
NCMP2PATH FORM      1
NCMP2LOCK FORM      1         0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NCMP2VARS LIST
NCMP2Num  DIM       6         001-006 Campaign Number(Key)
NCMP2INum DIM       18        007-024 Integral Scenario ID(Key)
NCMP2Upd  DIM       1         025-025 Update Byte:  '0'=No Update necessary, '1'=Need to Update Integral
NCMP2UDate          DIM       8         026-033 Update Date
NCMP2Filler         DIM       20        034-053 Update Date
                    listend

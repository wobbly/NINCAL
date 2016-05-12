******************************************************
*  NCATDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  CATEGORY FILE DESCRIPTOR.
* ****************************************************
* NAMES IN THE NEWS MASTER CATEGORY FILE.
* ****************************************************
.patch 1.1   2005 June 18	DMB - Changed IP of File Manager
.
.      FILE:      NINCAT
.    LENGTH:      9
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NCATLIST + NCATCODE + NCATNUM
.AIMDEX KEY:      1-6	NCATLIST
.
.This Category file will read NINREF, which is a Description Reference file.
.As part of it's key it will use hard-coded "T" (for 'CATEGORY'), and then
.the 3 byte key value used for this file.
.This key value, unlike all the other files that read NINREF, uses a sub-code
.as part of it's key.  The others use a 3 byte number.  This one uses a 1 byte
.sub-code paired with a 2 byte number.  ASH 8/1/02
......................................................
NCATNAME	INIT	"NINCAT"
.>Patch 1.1 Begin
NCATFLIST	FILELIST
.NCATFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINCAT.isi"
.NCATFLE2	AFILE	FIXED=9,Name="NINCAT.aam"
.NCATFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINCAT.isi|20.20.30.104:502"
NCATFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINCAT.isi|10.10.30.104:502"
.NCATFLE2	AFILE	FIXED=9,Name="NINCAT.aam|20.20.30.104:502"
NCATFLE2	AFILE	FIXED=9,Name="NINCAT.aam|10.10.30.104:502"
		FILELISTEND
.>Patch 1.1 End		
.
NCATLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NCATFLD		DIM	9
NCATFLD1	DIM	9
NCATFLAG	FORM	1
NCATPATH	FORM	1
.
NCATVARS	LIST
NCATLIST	DIM	6	1-6	CATEGORY LIST NUMBER
NCATCODE	DIM	1	7-7	CATEGORY SUB-CODE
.					'B' = BUSINESS
.					'C' = CONSUMER
.					'E' = ENHANCED SELECT.
NCATNUM		DIM	2	8-9	CATEGORY NUMBER
		LISTEND

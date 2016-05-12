******************************************************
*  NARRDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ARRANGEMENT CODE FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER ARRANGEMENT CODE FILE.
* ****************************************************
;   release         1.1                2005 June 18	DMB - Changed IP of File Manager
.
.      FILE:      NINARR
.    LENGTH:      9
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NARRLIST + NARRNUM
.AIMDEX KEY:      1-6	NARRLIST
......................................................
NARRNAME	INIT	"NINARR"
.>Patch 1.1 Begin
NARRFLIST	FILELIST
.NARRFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINARR.isi"
.NARRFLE2	AFILE	FIXED=9,Name="NINARR.aam"
.NARRFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINARR.isi|20.20.30.103:502"
NARRFILE	IFILE	KEYLEN=9,FIXED=9,Name="NINARR.isi|NINS1:502"
.NARRFLE2	AFILE	FIXED=9,Name="NINARR.aam|20.20.30.103:502"
NARRFLE2	AFILE	FIXED=9,Name="NINARR.aam|NINS1:502"
		FILELISTEND
.>Patch 1.1 End		
.
NARRLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NARRFLD		DIM	9
NARRFLD1	DIM	9
NARRFLAG	FORM	1
NARRPATH	FORM	1
.
NARRVARS	LIST
NARRLIST	DIM	6	1-6	LIST NUMBER
NARRNUM		DIM	3	7-9	ARRANGEMENT CODE NUMBER
		LISTEND

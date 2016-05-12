******************************************************
*  NArrBDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ARRANGEMENT CODE FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER ARRANGEMENT CODE FILE.
* ****************************************************
;   release         1.1                2005 June 18	DMB - Changed IP of File Manager
.
.      FILE:      NINArrB
.    LENGTH:      9
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NArrBLIST + NArrBNUM
.AIMDEX KEY:      1-6	NArrBLIST
......................................................
NArrBNAME	INIT	"NINArrB"
.>Patch 1.1 Begin
NArrBFLIST	FILELIST
NArrBFILE	IFILE	KEYLEN=9,FIXED=26,Name="NINArrB.isi|NINS1:502"
NArrBFLE2	AFILE	FIXED=26,Name="NINArrB.aam|NINS1:502"
		FILELISTEND
.>Patch 1.1 End		
.
NArrBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NArrBFLD		DIM	9
NArrBFLD1	DIM	9
NArrBFLAG	FORM	1
NArrBPATH	FORM	1
.
NArrBVARS	LIST
NArrBLIST	DIM	6	1-6	LIST NUMBER
NArrBNUM	DIM	3	7-9	ARRANGEMENT CODE NUMBER
NArrBStamp	Dim	17        .backup time
		LISTEND

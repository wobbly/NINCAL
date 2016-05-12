******************************************************
*  NSrcBDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  SOURCE CODE FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER SOURCE CODE FILE.
* ****************************************************
.
.      FILE:      NINSrcB
.    LENGTH:      29
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NSrcBLIST + NSrcBNUM
.AIMDEX KEY:      1-6	NSrcBLIST
......................................................
NSrcBNAME	INIT	"NINSrcB"
NSrcBFLIST	FILELIST
NSrcBFILE	IFILE	KEYLEN=9,FIXED=29,Name="NINSrcB.isi|NINS1:502"
NSrcBFLE2	AFILE	FIXED=29,Name="NINSrcB.aam|NINS1:502"

		FILELISTEND
.
NSrcBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NSrcBFLD		DIM	9
NSrcBFLD1	DIM	9
NSrcBFLAG	FORM	1
NSrcBFLG2	FORM	1
NSrcBPATH	FORM	1
.
. 
NSrcBVARS	LIST
NSrcBLIST	DIM	6	1-6	LIST NUMBER
NSrcBNUM		DIM	3	7-9	SOURCE CODE NUMBER
NSrcBPER		DIM	3      10-12	SOURCE CODE PERCENTAGE
NSrcBStamp	Dim	17
		LISTEND

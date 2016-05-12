******************************************************
*  NTxtBDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  DATACARD TEXT FILE
* ****************************************************
* NAMES IN THE NEWS MASTER MEDIA CODE FILE.
* ****************************************************
.
.      FILE:      NINTxtB
.    LENGTH:      525
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-8	NTxtBLIST + NTxtBNUM
.AIMDEX KEY:      1-6	NTxtBLIST
.		  8-507 NTxtBTXT
......................................................
NTxtBNAME	INIT	"NINTxtB"
.
NTxtBFLIST	Filelist
NTxtBFILE	IFILE	KEYLEN=9,FIXED=525,Name="NINTxtB.isi|NINS1:502"
NTxtBFLE2	AFILE	FIXED=525,Name="NINTxtB.aam|NINS1:502"
		filelistend
.
NTxtBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NTxtBFLD		DIM	8
NTxtBFLD1	DIM	9
NTxtBFLD2	DIM	503
NTxtBFLAG	FORM	1
NTxtBPATH	FORM	1
.
.
NTxtBVARS	LIST
NTxtBLIST		DIM	6	1-6	LIST NUMBER
NTxtBNUM		DIM	2	7-8	TEXT NUMBER
NTxtBTEXT		DIM	500     9-508   TEXT
NTxtBStamp	Dim	17    509-525
 		LISTEND

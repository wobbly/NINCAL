******************************************************
*  DATACARD "Upper Case" FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER DATACARD FILE.
* ****************************************************
.
.      FILE:      NINDatU
.    LENGTH:      50
.  COMPRESS:      NONE
.      TYPE:      Aim,ISAM,MANAGED
. INDEX KEY:      1-6     DatUNUM
.AIMDEX KEY:      7-25    DatUName 
......................................................
. LAST MODIFIED
. patch 1.0 17October2006 DLH - Created
......................................................
NDatUNAME	INIT	"NINDatU                             "
NDatUNME1	INIT	"NINDatU                             "
NDatUFLIST	FILELIST
NDatUFILE	IFILE	KEYLEN=6,FIXED=50,Name="NINDatU.ISI|NINS1:502"
NDatUFLE1	AFILE	FIXED=50,Name="NINDatU.AAM|NINS1:502"
	FILELISTEND
	
NDatULOCK	FORM	1		0 OR 1=filepi,2=recordlock, 3=no lock
NDatUFLD		DIM	6
NDatUFLD1	DIM	9

NDatUFLAG	FORM	1
NDatUPATH	FORM	1
.
DATUVARS	LIST
DatUNUM	DIM	6       1-6    ZERO FILLED KEY.
DatUNAME	DIM	25      7-31
DatUFill	Dim	19     32-50	
	 LISTEND

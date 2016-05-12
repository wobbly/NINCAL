******************************************************
*  NModBDD INCLUSION
*  PRICE MODIFIER FILE FOR DATACARD SUBSIDIARY FILES
*  PRICE MODIFIER FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER PRICE MODIFIER FILE.
* ****************************************************
.
.      FILE:      NINModB
.    LENGTH:      60
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-3	NModBNUM
.AIMDEX KEY:      4-23	NModBDESC
......................................................
NModBNAME	INIT	"NINModB"
NModBFLIST	FILELIST
NModBFILE	IFILE	KEYLEN=9,FIXED=60,Name="NINModB.isi|NINS1:502"
NModBFLE2	AFILE	FIXED=60,Name="NINModB.aam|NINS1:502"
		FILELISTEND
.
NModBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NModBFLD		DIM	3
NModBFLD1	DIM	23
NModBFLAG	FORM	1
NModBPATH	FORM	1
.
NModBVARS	LIST
NModBNUM		DIM	3	1-3	PRICE MODIFIER NUMBER
NModBDESC	DIM	20      4-23	PRICE MODIFIER DESCRIPTION
.	"   " OR "000"	NO DESCRIPTION/CALCULATION
.	"001"		/M
.	"002"		/FLAT
.	"003"		/EACH
.	"004"		/MIN
.	"005"		N/A
.	"006"		N/C
.	"007"		SEEBASE
.	"008"		SEEDESC
.	"009"		INQUIRE
NModBCALC	DIM	20     24-43	PRICE MODIFIER CALCULATION
.	"   " OR "000"	NO DESCRIPTION/CALCULATION
.	"001"		/1000
.	"002"
.	"003"		*1
.	"004"
.	"005"
.	"006"		*0
.	"007"
.	"008"
.	"009"
NModBStamp	Dim	17       44-60
		LISTEND

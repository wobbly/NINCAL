******************************************************
*  NAddBDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER ADDRESSING FILE.
* ****************************************************
;   release         1.1                2005 June 18	DMB - Changed IP of File Manager
.
.      FILE:      NINAddB
.    LENGTH:      20
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NAddBLIST + NAddBNUM
.AIMDEX KEY:      1-6	NAddBLIST
......................................................
NAddBNAME	INIT	"NINAddB"
.>Patch 1.1 Begin    
NAddBFLIST	FILELIST
NAddBFILE	IFILE	KEYLEN=9,FIXED=37,Name="NINAddB.isi|NINS1:502"
NAddBFLE2	AFILE	FIXED=37,Name="NINAddB.aam|NINS1:502"
		FILELISTEND
.>Patch 1.1 End		
.
NAddBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NAddBFLD		DIM	9
NAddBFLD1	DIM	9
NAddBFLAG	FORM	1
NAddBPATH	FORM	1
. 
NAddBVARS	LIST
NAddBLIST	DIM	6	1-6	LIST NUMBER
NAddBNUM		DIM	3	7-9	ADDRESS CODE NUMBER
NAddBPRICE	FORM	5.2    10-17	SELECTION CODE PRICE
NAddBDESC	DIM	3      18-20	SELECTION CODE DESCRIPTION
NAddBStamp	Dim	17        stamp of when backed up
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
		LISTEND

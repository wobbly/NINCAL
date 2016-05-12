******************************************************
*  NSltBDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  SELECTION CODE FILE DESCRIPTOR.
* ****************************************************
* NAMES IN THE NEWS MASTER SELECTION CODE FILE.
* ****************************************************
.
.      FILE:      NINSltB
.    LENGTH:      29
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NSltBLIST + NSltBNUM
.AIMDEX KEY:      1-6	NSltBLIST
......................................................
NSltBNAME	INIT	"NINSltB"
.
NSltBFLIST	Filelist
NSltBFILE	IFILE	KEYLEN=9,FIXED=46,Name="NINSltB.isi|NINS1:502"
NSltBFLE2	AFILE	FIXED=46,Name="NINSltB.aam|NINS1:502"

         filelistend
.
NSltBLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NSltBFLD		DIM	9
NSltBFLD1	DIM	9
NSltBFLAG	FORM	1
NSltBPATH	FORM	1
.
. 
NSltBVARS	LIST
NSltBLIST	DIM	6	1-6	LIST NUMBER
NSltBNUM		DIM	3	7-9	SELECTION CODE NUMBER
NSltBPRICE	FORM	5.2    10-17	SELECTION CODE PRICE
NSltBDESC	dim	3      18-20	SELECTION CODE PRICE MODIFIER
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
NSltBQTY		DIM	9      21-29	SELECTION QUANTITY
NSltBStamp	Dim	17     30-46
		LISTEND

******************************************************
*  NADDDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER ADDRESSING FILE.
* ****************************************************
;   release         1.1                2005 June 18	DMB - Changed IP of File Manager
.
.      FILE:      NINADD
.    LENGTH:      20
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9	NADDLIST + NADDNUM
.AIMDEX KEY:      1-6	NADDLIST
......................................................
NADDNAME	INIT	"NINADD"
.>Patch 1.1 Begin    
NADDFLIST	FILELIST
.NADDFILE	IFILE	KEYLEN=9,FIXED=20,Name="NINADD.isi"
.NADDFLE2	AFILE	FIXED=20,Name="NINADD.aam"
.NADDFILE	IFILE	KEYLEN=9,FIXED=20,Name="NINADD.isi|20.20.30.103:502"
NADDFILE	IFILE	KEYLEN=9,FIXED=20,Name="NINADD.isi|NINS1:502"
.NADDFLE2	AFILE	FIXED=20,Name="NINADD.aam|20.20.30.103:502"
NADDFLE2	AFILE	FIXED=20,Name="NINADD.aam|NINS1:502"
		FILELISTEND
.>Patch 1.1 End		
.
NADDLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NADDFLD		DIM	9
NADDFLD1	DIM	9
NADDFLAG	FORM	1
NADDPATH	FORM	1
. 
NADDVARS	LIST
NADDLIST	DIM	6	1-6	LIST NUMBER
NADDNUM	DIM	3	7-9	ADDRESS CODE NUMBER
NADDPRICE	FORM	5.2    10-17	SELECTION CODE PRICE
NADDDESC	DIM	3      18-20	SELECTION CODE DESCRIPTION
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

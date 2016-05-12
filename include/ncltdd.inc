******************************************************
*  NCLTDD INCLUSION
*  CONSULTANT/CLIENT CROSS REFERENCE FILE (FOR WEBSITE PURPOSES)
*  NOTE:  THIS DD MAY BE TEMPORARY!!!
. ....................................................
* ****************************************************
* NAMES IN THE NEWS CONSULTANT/CLIENT CROSS REFERENCE FILE.
* ****************************************************
.   release         1.0                2005 December 12	ASH	Original Release
.
.      FILE:      NINCLT
.    LENGTH:      50
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-14	NCLTCONSULT + NCLTCLIENT + NCLTNUM
.AIMDEX KEY:      1-6	NCLTCONSULT
.		  7-12  NCLTCLIENT
......................................................
NCLTNAME	INIT	"NINCLT"
NCLTFLIST	FILELIST
NCLTFILE	IFILE	KEYLEN=9,FIXED=50,Name="NINCLT.isi|NINS1:502"
NCLTFLE2	AFILE	FIXED=50,Name="NINCLT.aam|NINS1:502"
		FILELISTEND
NCLTLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NCLTFLD		DIM	145
NCLTFLD1	DIM	9
NCLTFLD2	DIM	9
NCLTFLAG	FORM	1
NCLTPATH	FORM	1
.
NCLTVARS	LIST
NCLTCONSULT	DIM	6	1-6	Consultant Number (ISAM/AAM)
NCLTCLIENT	DIM	6	7-12	Client Number (ISAM/AAM)
NCLTNUM		DIM	2      13-14	Unique Number (Allow up to 99 new entries) (ISAM)
NCLTSDATE	DIM	8      15-22	Relationship Start Date
NCLTEDATE	DIM	8      23-30	Relationship End Date
NCLTTYPE	DIM	1      31-31	Type of Consultant:  " "=Regular, "1"=Consultant may not have their name on Orders, therefore allow them to view Orders for their Clients where the Broker field is blank (this can be dangerous!)
NCLTFILLER	DIM	19     32-50	Filler
		LISTEND

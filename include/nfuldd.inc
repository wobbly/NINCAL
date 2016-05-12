............................................................
.
. NFULDD INCLUSION
. NIN FULFILLMENT FILE DEFINITION
.
. FILE NAME : NINFUL
. REC LENGTH: 273 FIXED
. INDEX KEY : NFULNUM
. AAMDEX KEY: NFULCOMP
.	      NFULCNT
.	      NFULTELE
.18 JUN 2005 DMB IP Changed for File Manager
.16 May 2005 DLH add email
............................................................
NFULLIST  Filelist
.NFULFILE IFILE     KEYLEN=4,FIX=273,UNCOMP,Name="NINFUL.ISI|20.20.30.103:502"
NFULFILE IFILE     KEYLEN=4,FIX=273,UNCOMP,Name="NINFUL.ISI|NINS1:502"
.NFULFLE2 AFILE     FIX=273,Name="NINFUL.AAM|20.20.30.103:502"
NFULFLE2 AFILE     FIX=273,Name="NINFUL.AAM|NINS1:502"
	Filelistend
NFULFLSQ FILE
.NFULNAME INIT      "NINFUL|20.20.30.103:502       "
NFULNAME INIT      "NINFUL|NINS1:502       "
NFULFLD  DIM       4
NFULFLD1 DIM       48
NFULFLD2 DIM       48
NFULFLD3 DIM       13
NFULPATH FORM      1
NFULFLAG FORM      1
NFULFLG2 FORM      1
NFULLOCK FORM      1
.
NFULVARS	list
NFULNUM		dim	4	  1-4	       	.Record Num(ISAM)
NFULCOMP	dim	45	  5-49		.Company Name(AAM)
NFULCNT		dim	45	 50-94		.Contact Name(AAM)
NFULFAX		dim	10	 95-104		.Phone Number(AAM)
NFULDATE	dim	8	105-112		.Creation Date
NFULMODDATE	dim	8	113-120		.Modification Date
NFULINITS	dim	3	121-123		.Initials
NFULNOTES	dim	100	124-223		.Notes
NFULEmail	dim	50	224-273
;NFULFill	dim	50	224-273
		listend
............................................................

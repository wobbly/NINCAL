******************************************************
*  MIN List FILE datacard x ref DESCRIPTORS.
. ....................................................
* ****************************************************
* MIN DATACARD FILES.
* ****************************************************
.
.      FILE:      Min2NIN
.    LENGTH:      20
.  COMPRESS:      NONE
.      TYPE:      Isam
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
;Secondary FIles See
......................................................
MinXNAME	DIM	8
MinXFILE	IFILE	KeyLen=6,FIXED=20,Name="Min2NIN.ISI|NINS1:502"
MinXFILE2	IFILE	KEYlen=6,FIXED=20,Name="Min2NIN2.ISI|NINS1:502"

MinXFLd	Dim	6             .NIN list number	1
MinXFLD	Dim	6             .min List number
MinXFLAG	FORM	1
MinXPATH	FORM	1
.
MinVARS		LIST                   
MInxNIN	DIM	6	  1-6     datacard number
MinxFill	Dim	1	  7-7
MinxMIN	Dim	6	  8-13    Min #
minXFIll2	Dim	7	 14-20
	 LISTEND

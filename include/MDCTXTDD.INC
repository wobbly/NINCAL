.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_MTXT
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      FLAT Comma Delimited
......................................................
. LAST MODIFIED
.Patch 1.1  18 March 2008 DLH add Aam 
. patch 1.0 14 August 2006 DLH New
.Main file
.MDCMaindd
.Secondary FIles See
.MDCSEGDD - Segment info universe & $
.MDSMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDCTXTDD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDCTXTDD - Owner
.MDCTXTDD - Text
......................................................
MTXTNAME	INIT	"MDC_TXT.Isi|NINS1:502"
MTXTNAME2	INIT	"MDC_TXT2.Isi|NINS1:502"
MTXTNAME3	INIT	"MDC_TXT.AAM|NINS1:502"

MTXTFLIST	FileList
MTXTFILE	IFILE	Name="MDC_TXT.Isi|NINS1:502"
MTXTFILE2	IFILE	Name="MDC_TXT2.Isi|NINS1:502"
MTXTFILE3	AFILE	Fixed=62,Name="MDC_TXT.AAM|NINS1:502"
	FileListEnd
MTXTSFILE	FILE	
.MTXTSFILE	FILE	Name="MDC_TXT.csv"

MTXTFLAG	FORM	1
MTXTPATH	FORM	1
MTXTFLD	Dim	6
MTXTFLD2	Dim	10         card + Seq
MTXTFLD3	Dim	53         TExt plus key info
.
MTXTVARS		LIST                   .for ease I am using Min's names
TXDCNO	Dim	6	 1-6    Card#
TGSQNO	Form	4	 7-10   Seq #
TXTTYP	DIM	1	 11-11  TYpe of text code
TXTLIN	Dim	50	 12-62       TEXT
	ListEnd
MTXTVARS1		LIST                   .for ease I am using Min's names
CTXDCNO	Dim	6	 1-6    Card#
CTGSQNO	dim	4	 7-10   Seq #
CTXTTYP	DIM	1	 11-11  TYpe of text code
CTXTLIN	Dim	50	        TEXT
	ListEnd
	
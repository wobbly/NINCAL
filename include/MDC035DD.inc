.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_M035
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      FLAT Comma Delimited
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Main file
.MDCMaindd
.Secondary FIles See
.MDCSEGDD - Segment info universe & $
.MDSMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
......................................................
M035NAME	Init	"MDC_035.Isi|NINS1:502"

M035FLIST 	FileList
M035FILE	IFILE	Name="MDC_035.Isi|NINS1:502"
M035FILE2	IFILE	Name="MDC_0352.Isi|NINS1:502"
	FileListEnd
M035SFILE	FILE	
.M035SFILE	FILE	Name="MDC_035.csv"

M035FLAG	FORM	1
M035PATH	FORM	1
M035Fld	Dim	6
M035Fld2	Dim	8
.
M035VARS		LIST                   .for ease I am using Min's names
CADCNO	Dim	6	 1-6    Card#
CASQNO	Form	2	 7-8    Category Seq#
CATC	Dim	3	 9-11   Category COde
CAT$	FOrm	5.2                 12-19   Category DOllar
CATCHR	dim	4	20-23   Category option -char 
CATR	Dim	1	24-24   Rate M/L
CATDSC	Dim	25          29-49   Description
	ListEnd
M035VARS1		LIST                   .for ease I am using Min's names
CCADCNO	Dim	6	 1-6    Card#
CCASQNO	Dim	2	 7-8    Category Seq#
CCATC	Dim	3	 9-11   Category COde
CCAT$	Dim	7                 12-19   Category DOllar
CCATCHR	dim	4	20-23   Category option -char 
CCATR	Dim	1	24-24   Rate M/L
CCATDSC	Dim	25                  29-49   Description
	ListEnd
	
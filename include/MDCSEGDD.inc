.******************************************************
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_SEG
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
MSEgNAME	Init	"MDC_Seg.Isi|NINS1:502"
MSegFlist	Filelist
MSegFILE	IFILE	Name="MDC_Seg.Isi|NINS1:502"
MSegFILE2	IFILE	Name="MDC_Seg2.Isi|NINS1:502"
	FileListEnd
MSegSFILE	FILE	
.MSegSFILE	FILE	Name="MDC_Seg.csv"

MSegFLAG	FORM	1
MSegPATH	FORM	1
MSegFld	Dim	6
MSegFld2	Dim	8
.
MSegVARS		LIST                   .for ease I am using Min's names
SGDCNO	Dim	6	 1-6    Card#
SGSQNO	Form	2	 7-8    Seq#
SGPT	Dim	1	 9-9    Seg pricing Type
SGC	Form	10	10-19   Segment COunt
SGD	Dim	25	20-44   Segment Description
SGP	Form	5.2	45-52   Segment PRice
SGR	Dim	1	53-53   Rate
SEGCAT	Dim	3	54-56   Cat code
SEGTYP	Dim	1	57-57   Seg TYpe
	ListEnd
MSegVARS1		LIST                   .csvfile
CSGDCNO	Dim	6	 1-6    Card#
CSGSQNO	Dim	2	 7-8    Seq#
CSGPT	Dim	1	 9-9    Seg pricing Type
CSGC	Dim	10	10-19   Segment COunt
CSGD	Dim	25	20-44   Segment Description
CSGP	Dim	7	45-52   Segment PRice
CSGR	Dim	1	53-53   Rate
CSEGCAT	Dim	3	54-56   Cat code
CSEGTYP	Dim	1	57-57   Seg TYpe
	ListEnd
	
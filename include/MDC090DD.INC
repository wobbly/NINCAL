.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_M090
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
.MDC090DD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
......................................................
M090NAME	Init	"MDC_090.isi|NINS1:502"
M090Flist	FileLIst
M090FILE	IFILE	Name="MDC_090.isi|NINS1:502"
M090FILE2	AFILE	Name="MDC_090.aam|NINS1:502"
	FileListEnd
M090SFILE	FILE	
.M090SFILE	FILE	Name="MDC_090.csv"

M090Fld	Dim	5
M090Fld2	Dim	28
M090FLAG	FORM	1
M090PATH	FORM	1
.
M090VARS	LIST                   .for ease I am using Min's names
GKEY	Dim	5	  1-5    LO Code#
GNAM	Dim	25	  6-30       Name
GAD1	Dim	25	 31-55       Address 1
GAD2	Dim	25	 56-80        Address 2
GCTY	Dim	20	 81-100        City
GSTT	Dim	2           101-102        State
GZIP	Dim	10	103-112        Zip
GCTR	Dim	25	113-137        Country
GCNT	Dim	25	138-162        Contact
GPH1	Dim	11	163-173        Phone
GPH2	Dim	11	174-184        Phone
GPHF	Dim	11	185-195        FAX
GPHM	Dim	11	196-206        Modem
GDLT	Dim	1	207-207        'A'ctive/'I'nactive
	ListEnd
	
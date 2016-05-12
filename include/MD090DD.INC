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
M090NAME	DIM	12
M090FILE	IFILE	Name="MDC_090C.isi|NINS1:502"
M090SFILE	FILE	
.M090SFILE	FILE	Name="MDC_090.csv"

M090FLd	Dim	5
M090FLAG	FORM	1
M090PATH	FORM	1
.
M090VARS		LIST                   .for ease I am using Min's names
GKEY	Dim	5	 1-5    LO Code#
GNAM	Dim	25	        Name
GAD1	Dim	25	        Address 1
GAD2	Dim	25	        Address 2
GCTY	Dim	20	        City
GSTT	Dim	2	        State
GZIP	Dim	10	        Zip
GCTR	Dim	25	        Country
GCNT	Dim	25	        Contact
GPH1	Dim	11	        Phone
GPH2	Dim	11	        Phone
GPHF	Dim	11	        FAX
GPHM	Dim	11	        Modem
GDLT	Dim	1	        'A'ctive/'I'nactive
	ListEnd
	
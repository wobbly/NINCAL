.******************************************************
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_MSC
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
.MDCMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
......................................................
MMSCNAME	Init	"MDC_MSC.Isi|NINS1:502"
MMSCFILE	IFILE	Name="MDC_MSC.Isi|NINS1:502"
MMSCSFILE	FILE	
.MMSCSFILE	FILE	Name="MDC_MSC.csv"

MMscFld	Dim	6
MMSCFLAG	FORM	1
MMSCPATH	FORM	1
.
MMSCVARS		LIST                   .for ease I am using Min's names
DDCNO	Dim	6	 1-6    Card#
DAD1	Dim	30	 7-36   Addressing 1
DAD2	Dim	30	37-66   Addressing 2
DMO1	Dim	25	67-91   Min order line 1
DMO2	Dim	25	92-116  Min order line 2
DUS1	Dim	25         117-141  UNit of Sale line 1 
DUS2	Dim	25         142-166  UNit of Sale line 2
DSX1	Dim	25         167-191  Gender Line 1 	
DSX2	Dim	25         192-216  Gender Line 2 	
DMD1	Dim	25         217-241  Media Line 1 	
DMD2	Dim	25         242-266  Media Line 2 	
DRS1	Dim	25         267-291  Net Name Line 1 	
DRS2	Dim	25         292-316        Net Name Line 2 	
DDLT	Dim	1          317-317        'A'ctive/'I'nactive
	ListEnd
	
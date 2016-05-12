.******************************************************
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      NMDC_MSC
.    LENGTH:      Fixed
.  COMPRESS:      NONE
.      TYPE:      Isam
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
NMSCNAME	Init	"NMDC_MSC.Isi|NINS1:502"
NMSCFILE	IFILE	Name="NMDC_MSC.Isi|NINS1:502"
NMSCSFILE	FILE	

NMscFld	Dim	6
NMSCFLAG	FORM	1
NMSCPATH	FORM	1
.
NMSCVARS		LIST                   .for ease I am using Min's names
NDDCNO	Dim	6	 1-6    NIN Card#
NDAD1	Dim	30	 7-36   Addressing 1
NDAD2	Dim	30	37-66   Addressing 2
NDMO1	Dim	25	67-91   Min order line 1
NDMO2	Dim	25	92-116  Min order line 2
NDUS1	Dim	25         117-141  UNit of Sale line 1 
NDUS2	Dim	25         142-166  UNit of Sale line 2
NDSX1	Dim	25         167-191  Gender Line 1 	
NDSX2	Dim	25         192-216  Gender Line 2 	
NDMD1	Dim	25         217-241  Media Line 1 	
NDMD2	Dim	25         242-266  Media Line 2 	
NDRS1	Dim	25         267-291  Net Name Line 1 	
NDRS2	Dim	25         292-316        Net Name Line 2 	
NDDLT	Dim	1          317-317        'A'ctive/'I'nactive
	ListEnd
	
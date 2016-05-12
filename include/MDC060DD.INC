.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_M060
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
.MDC060DD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDC060DD - Owner
.MDCTXTDD - Text
......................................................
M060NAME	Init	"MDC_060.isi|NINS1:502"
M060FILE	IFILE	Name="MDC_060.isi|NINS1:502"
M060SFILE	FILE	
.M060SFILE	FILE	Name="MDC_060.csv"

M060Fld	Dim	3
M060FLAG	FORM	1
M060PATH	FORM	1
.
M060VARS	LIST                   .for ease I am using Min's names
FC	Dim	3	  1-3    cat Code#
FDSC	Dim	25	  4-28       Desc
	ListEnd
	
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      NINMDCCAT
.    LENGTH:      
.  COMPRESS:      NONE
.      TYPE:      
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
......................................................
NMDCCNAME	Init	"NINMDCCAT.Isi|NINS1:502"

NMDCCFLIST 	FileList
NMDCCFILE	IFILE	Name="NINMDCCAT.Isi|NINS1:502"
NMDCCFILE2	IFILE	Name="NINMDCCAT2.Isi|NINS1:502"
	FileListEnd
NMDCCSFILE	FILE	

NMDCCFLAG	FORM	1
NMDCCPATH	FORM	1
NMDCCFld	Dim	6
NMDCCFld2	Dim	3
.
NMDCCVARS		LIST                   
NMDCCnum	Dim	6	 1-6    Card#                     Key
NMDCCSQN	Form	2	 7-8    Category Seq#
NMDCCCATC	Dim	3	 9-11   Category COde             Key
NMDCCCAT	FOrm	5.2                 12-19   Category DOllar
NMDCCCATCHR	dim	4	20-23   Category option -char 
NMDCCCATR	Dim	1	24-24   Rate M/L
NMDCCCATDSC	Dim	25          29-49   Description
	ListEnd
	
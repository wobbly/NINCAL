.*  MIN 2 NIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MIN2NINLO
.    LENGTH:      Fixed
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
......................................................
. LAST MODIFIED
. patch 1.0           20 November 2006 DLH New
.Main file
.MDCMaindd
.Secondary FIles See
.MDCSEGDD - Segment info universe & $
.MDSMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDCTXTDD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDCTXTDD - Owner
.MDCTXTDD - Text
.M2nLodd  - list owner xref
......................................................
M2NLONAME	INit	"Min2NinLO       "
M2nLOFlist	Filelist
M2NLOFILE	IFILE	Name="Min2ninLo.Isi|NINS1:502"
M2NLOFILE2	IFILE	Name="Min2ninLo2.Isi|NINS1:502"
	FileListEnd
M2NLOFLAG	FORM	1
M2NLOPATH	FORM	1
M2NLOFld	Dim	6
M2NLOFld2	Dim	6
.
M2NLOVARS		LIST                   .
M2NLOMin	Dim	6	 1-6    right justified 5 byte owner code
M2NLOFill1	dim	1	 7-7    Not USed
M2NLONIN	DIM	6	 8-13   Owner #
	ListEnd
	
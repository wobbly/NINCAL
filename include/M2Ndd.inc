.*  MIN 2 NIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MIN2NIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
......................................................
. LAST MODIFIED
. patch 1.1           20 November 2006 DLH Move owner info to its own file
. patch 1.0           14 August 2006 DLH New
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
M2NNAME	INit	"Min2Nin"
M2nFlist	Filelist
M2NFILE	IFILE	Name="Min2nin.Isi|NINS1:502"
M2NFILE2	IFILE	Name="Min2nin2.Isi|NINS1:502"
	FileListEnd
M2NFLAG	FORM	1
M2NPATH	FORM	1
M2NFld	Dim	6
M2NFld2	Dim	6
.
M2NVARS		LIST                   .
M2NMin	Dim	6	 1-6    6 byte Min LIst code or right justified 5 byte owner code
M2NStatus	dim	1	 7-7    "*"  = Special do not update
. special pricing -- need to print min list for counts etc - argh
M2NNIN	DIM	6	 8-13   NIN List #
	ListEnd
	
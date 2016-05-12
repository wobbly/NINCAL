.company - salesforce xref
.companySF.dat
COMPSFFLIST     Filelist
COMPSFFILE      IFILE           keylen=6,fix=21,uncomp,Name="COMPANYSF.ISI|NINS1:502"
COMPSFFILE2     IFILE           keylen=15,fix=21,uncomp,Name="COMPANYSF2.isi|NINS1:502"
	 Filelistend	 
COMPSFNAME      INIT            "COMPANYSF     "
COMPSFNAM2      INIT            "COMPANYSF2"

COMPSFPATH      FORM            1
COMPSFFLAG      FORM            1
COMPSFFLD       DIM             6
COMPSFFLD2      DIM             15
COMPSFLOCK      FORM            1

COMPSFVARS      LIST
CompSFNUm	Dim	6	1-6	company #
CompSlsF	Dim	15	7-21	salesforce#
	ListENd
	
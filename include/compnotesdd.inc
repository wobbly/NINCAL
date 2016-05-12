..............................................................................
.
. COMPNOTESDD INCLUSION
. COMPANY NOTES FILE DEFINITION
.
. FILE NAME : COMPNOTES
. REC LENGTH: 756
. INDEX KEY : 1-6 
.
.DMB				03JUN2003 - FILE CREATED
..............................................................................
COMPNOTEFILE	IFILE    KEYLEN=6,VAR=756,NODUPLICATES,Name="COMPNOTES.ISI|NINS1:502"
COMPNOTENAME	INIT      "COMPNOTES.ISI|NINS1:502"
COMPNOTEFLD		DIM       6
COMPNOTEFLAG	FORM      1
COMPNOTELOCK	FORM      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................
COMPNOTEVARS	list
COMPNOTECOMP	DIM	6			1-6			COMPANY
COMPNOTES		DIM	750			7-756			COMPANY NOTES
		listend

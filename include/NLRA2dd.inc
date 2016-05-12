.NLRA2DD.INC
......................................................................
NLRA2FLIST	FILELIST
NLRA2file	ifile	keylen=9,FIXED=84,Name="NINLRA_2.ISI|NINS1:502"
	FILELISTEND
NLRA2name	init	"NINLRA_2     "
NLRA2FLD		DIM	9	.LIst NUMBER _ LRANUM.
NLRA2FLAG	FORM	1
NLRA2Path	FORM	1
NLRA2LOCK	FORM	1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NLRA2VARS	LIST
NLRA2LIST		Dim	6	1-6 	List Number	
NLRA2NUM		DIM	3	7-9       LRA ID NUM 
NLRA2Desc		DIM	55       10-64   	Description of LRA
NLRA2FILLER	DIM	20       65-84 	FILLER
         LISTEND

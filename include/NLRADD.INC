.NLRADD.INC
......................................................................
NLRAFLIST	FILELIST
nLRAfile	ifile	keylen=6,FIXED=60,Name="NINLRA.ISI|NINS1:502"
nLRAfil1	ifile	keylen=12,FIXED=60,Name="NINLRA2.ISI|NINS1:502"
	FILELISTEND
nLRAname	init	"NINLRA     "
NLRAFLD		DIM	6	.LIst NUMBER.
NLRAFLD1		DIM	12	.LIst NUMBER + Mailer
NLRAFLAG	FORM	1
NLRAPath	FORM	1
NLRALOCK	FORM	1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NLRAVARS	LIST
NLRALIST		Dim	6	1-6 	List Number	
NLRAMailer	DIM	6       	7-12   	MAILER NUMBER
NLRADTE		DIM	8      	13-20  DATE OF LRA ccyymmdd  .signed/period  date
NLRADATE		DIM	8      	21-28  DATE ENTERED
NLRAUSER		DIM	9      	29-37  UPDATED BY
NLRANUM		DIM	3	38-40  LRA ID NUM - see NLRA2DD
NLRAFILLER	DIM	20     	41-60 FILLER
         LISTEND
....used in nordtest to not destroy in use variables
NlraS12	dim	12
Nlras10	dim	10
Nlras7	dim	7
Nlras6	dim	6
Nlras4	dim	4
Nlras2	dim	2
.
.******************************************************
.*  MIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      MDC_MAIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      FLAT Comma Delimited - Note all these files come CSV from Min if there is not a hit
.                 on our system they get put into an indexed/aimdexed file for review
......................................................
. LAST MODIFIED
. patch 1.0 14 August 2006 DLH New
.Secondary FIles See
.MDCSEGADD - Segment info universe & $
.MDSMSCDD - Misc addressing Min, UOS,Gender,Media,net
.MDC035DD - CAtegory
.MDC091DD - Owner ???? data no supplied
.MDC090DD - Owner
.MDCTXTDD - Text
......................................................
MinNAME	Init	"MDC_Main.Isi|NINS1:502"
.MinFILE	FILE	Name="MDCMain.ISI|NINS1:502"
.
MinFlist	Filelist
MinFILE	IFILE	fixed=438,Name="MDC_Main.Isi|NINS1:502"
MinFILE2	AFILE	fixed=438,Name="MDC_Main.AAm|NINS1:502"
	FilelistEnd
MinSFile	File	
.MinSFile	File	Name="MDCMain.csv|NINS1:502"

MinFLAG	FORM	1
MinPATH	FORM	1
MinFLd	Dim	6
Minfld2	Dim	53                  .aim key
MInCSVSTR	DIM	800
.
MinMainVARS	LIST                   .for ease I am using Min's names
DDCNOa	Dim	6	  1-6     datacard number         .'a' cause dupe from MDC_MSC file
LSType	Dim	1	  7-7     card Type
DTTL	Dim	50	  8-57    Title               .'a' cause dupe from MDC_MSC file
DMKEY	Dim	5	 58-62    Customer key
DMDCNO	Dim	6	 63-68    CHAR Rep Datacard # (is this the old #?)
DLOW	Dim	5	 69-73    List Owner Code
GNAMa	DIM	25	 74-98    List Owner Name          .'a' because dupe from MDC090 file
DRET	Dim	5	 99-103   Tape REturn Code
BUCO	Dim	1	104-104   Business / Consumer
NTNLT	Dim	1	106-105   Nationality
DDTE8	dim	8	106-113   Datacard date    CCYYMMDD
GPH1a	Dim	11	114-124	Owner phone                    .'a' because dupe from MDC090 file
GPHFa	Dim	11	125-135	Owner Fax 	        .'a' because dupe from MDC090 file
DCNT	Dim	25 	136-160	Owner COntact	        
LOEML	Dim	50	161-210	Owner Email		        
COMMMIn	Form	3.2 	211-216	Commission  3.2               .'Min - cause dupe
DDNU8	dim	8	217-224	next update
DDTR8	dim	8	225-232	Revised date
DDTC8	dim	8	233-240	Confirm date
DDDT8	dim	8	241-248	Update date
FNAM	DIM	25	249-273	Return Tape Name
FemPer	Form	3.2	274-279	percent female 3.2
MalPer	Form	3.2	280-285	percent male    3.2
UYESNO	Dim	7	286-292	UNit of sale yes/no
UOSLOW	Form	5.2	292-300             Average UOS Low    5.2
UOSHigh	Form	5.2	301-308             Average UOS High   5.2
UOSAvG	Form	5.2	309-316             Average UOS average   5.2
UNIV	Dim	9	317-325	Universe
MinDoll	form	5.2	326-333	Minimum dollar order    5.2
MinNME	Dim	9	334-342	Minimum Names order
MinNet	Dim	9	343-351	Minimum Net Names order
KEYCDE	Form	5.2	352-359	Key code charges        5.2
NETNAMEa	DIM	7	360-366	net name yes/no                      'a' appended dupes our name
NMEPER	form	3.2	367-372             Net Name percentage      3.2
RUNCHG	form	5.2	373-380	running charges          5.2
RUNUNITS	Dim	1	381-381	run units l/m
WBSTE	Dim	50	382-431	LO Website
DDLTa	Dim	1	432-432	"A"ctive "I"nactive         .'a' cause dupe from MDC_MSC file
JLSTSTS	DIm	1	433-433	List Status S,E,T,I,A
JPARENT	Dim	5	434-438
	 LISTEND
MinMainVARS1	LIST                   .CSV FIle
CDDCNOa	Dim	6	  1-6     datacard number         .'a' cause dupe from MDC_MSC file
CLSType	Dim	1	  7-7     card Type
CDTTL	Dim	50	  8-57    Title               .'a' cause dupe from MDC_MSC file
CDMKEY	Dim	5	 58-62    Customer key
CDMDCNO	Dim	6	 63-68    CHAR Rep Datacard # (is this the old #?)
CDLOW	Dim	5	 69-73    List Owner Code
CGNAMa	DIM	25	 74-98    List Owner Name          .'a' because dupe from MDC090 file
CDRET	Dim	5	 99-103   Tape REturn Code
CBUCO	Dim	1	104-104   Business / Consumer
CNTNLT	Dim	1	106-105   Nationality
CDDTE8	dim	8	106-113   Datacard date    CCYYMMDD
CGPH1a	Dim	11	114-124	Owner phone                    .'a' because dupe from MDC090 file
CGPHFa	Dim	11	125-135	Owner Fax 	        .'a' because dupe from MDC090 file
CDCNT	Dim	25 	136-160	Onwer COntact	        
CLOEML	Dim	50	161-210	Owner Email		        
CCOMMMIn	dIM	5 	211-216	Commission  3.2               .'Min - cause dupe
CDDNU8	dim	8	217-224	next update
CDDTR8	dim	8	225-232	Revised date
CDDTC8	dim	8	233-240	Confirm date
CDDDT8	dim	8	241-248	Update date
CFNAM	DIM	25	249-273	Return Tape Name
CFemPer	dIM	5	274-279	percent female 3.2
CMalPer	dIM	5	280-285	percent male    3.2
CUYESNO	Dim	7	286-292	UNit of sale yes/no
CUOSLOW	dIM	7	292-300             Average UOS Low    5.2
CUOSHigh	DIM	7	301-308             Average UOS High   5.2
CUOSAvG	DIM	7	309-316             Average UOS average   5.2
CUNIV	Dim	9	317-325	Universe
CMinDoll	dIM	7	326-333	Minimum dollar order    5.2
CMinNME	Dim	9	334-342	Minimum Names order
CMinNet	Dim	9	343-351	Minimum Net Names order
CKEYCDE	dIM	7	352-359	Key code charges        5.2
CNETNAME	DIM	7	360-366	net name yes/no
CNMEPER	dIM	5	367-372             Net Name percentage      3.2
CRUNCHG	dIM	7	373-380	running charges          5.2
CRUNUNITS	Dim	1	381-381	run units l/m
CWBSTE	Dim	50	382-431	LO Website
CDDLTa	Dim	1	432-432	"A"ctive "I"nactive         .'a' cause dupe from MDC_MSC file
CJLSTSTS	DIm	1	433-433	List Status S,E,T,I,A
CJPARENT	Dim	5	434-438
	 LISTEND

..............................................................................
.
. NdatCntDD INCLUSION
. NIN Dat/Min contactFILE DEFINITION
.
. FILE NAME : NINdatCnt
. REC LENGTH: 120 FIXED
. INDEX KEY : 2-6 (List #)
. AIM KEY   : 30-546
..26Oct2006 New
..............................................................................
.
NdatCntNAME 	INIT      "NINdatCnt.dat|NINS1:502"
NdatCntFLD   	DIM       6

NDatCntFList 	Filelist
NdatCntFLE1  	IFILE     KEYLEN=4,FIXED=120,Name="NINdatCnt.isi|NINS1:502"
NdatCntFLE2  	AFILE     FIXED=120,Name="NINdatCnt.AAM|NINS1:502"
             	filelistend

NdatCntFLD2  	DIM       28
NdatCntFLaG 	FORM      1
NdatCntPATH 	FORM      1
.
NDatCntvars  	list
NDatCntNum	dim	6	 1-6            NIN List # - Key
NdatCntPhn	Dim	11	 7-18	Manager Phone
NdatCntFax	Dim	11	19-29	Manager fax
NDatCnt	Dim	25	30-54	Contact        - key
NDatCntEml	Dim	50	55-104
NDatCntFill	Dim	16	105-120        not used
         listend
..............................................................................*


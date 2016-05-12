******************************************************
*  SRDSxSLTDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS SRDSx FILE.
* ****************************************************
.
.      FILE:      NISRDSxSLT
.    LENGTH:      51
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6         SRDSxSLTLIST
......................................................
SRDSxSLTNAME  INIT      "SRDSx_SLT.ISI|NINS1:502"


SRDSxSLTFLIST FILELIST


SRDSxSLTFILE  IFILE     KEYLEN=30,FIXED=60,Name="SRDSxSLT.isi|NINS1:502"
                    FILELISTEND
.
SRDSxSLTLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
SRDSxSLTFLD   DIM       30
SRDSxSLTFLAG  FORM      1
SRDSxSLTPATH  FORM      1
. 
SRDSxSLTVARS         LIST
SRDSxSLT     DIM       30         1-30       SRDS Select Desc
SRDSxSLTDESC DIM       30        31-60        NIN Standard SELECTION CODE DESCRIPTION
                    LISTEND

.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:   SRDS_TXT   
.    LENGTH:      
.  COMPRESS:      NONE
.      TYPE:      XML from SRDS if there is not a hit
.                 on our system they get put into an indexed/aimdexed file for review
......................................................
. LAST MODIFIED
. patch 1.0 14 September  2011 DLH New
......................................................
.
SRDSTXTFList        Filelist
SRDSTXTFILE         IFILE     fixed=58,Name="SRDS_TXT.Isi|NINS1:502"
SRDSTXTFLE1         IFILE     fixed=58,Name="SRDS_TXT1.Isi|NINS1:502"
                    FilelistEND

SRDSTXTNAME  Init      "SRDS_TXT|NINS1:502"
SRDSTXTFLAG   FORM      1
SRDSTXTPATH   FORM      1
SRDStxtLock   FORM      1
SRDStxtFLd    Dim       6
SRDStxtFLd1   Dim       8
.
SRDStxtVARS         LIST                   .
SRDSTXTLSTNUM       DIM       6       1-6    ZERO FILLED KEY.
SRDSTXTNUM          DIM       2       7-8    ZERO FILLED KEY.
SRDSTxt             DIM       50      9-58  misc unformated pricing info
.SRDSTxt             DIM       250     9-258  misc unformated pricing info
.                                      will be prepended to data card text? from a new datacard file 
                    LISTEND


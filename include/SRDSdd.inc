.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:      
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      XML from SRDS if there is not a hit
.                 on our system they get put into an indexed/aimdexed file for review
......................................................
. LAST MODIFIED
. patch 1.0 13 April  2011 DLH New
......................................................
.
SRDSFlist  Filelist
SRDSFILE   IFILE     fixed=707,Name="SRDS_Main.Isi|NINS1:502"
SRDSFILE2   AFILE     fixed=707,Name="SRDS_Main.AAm|NINS1:502"
          FilelistEnd

SRDSNAME  Init      "SRDS_Main|NINS1:502"
SRDSFLAG   FORM      1
SRDSPATH   FORM      1
SRDSLock   FORM      1
SRDSFLd    Dim       6
SRDSfld2   Dim       53                  .aim key
SRDSCSVSTR DIM       800
.
SRDSVARS         LIST                   .
SRDSLSTNUM    DIM       6       1-6    ZERO FILLED KEY.
SRDSOWNNUM    DIM       6       7-12   OWNER NUMBER (KEY FOR NINOWN  FILES)
SRDSDATFUL    DIM       6      13-18   FULFILLMENT NUMBER
SRDSDATMAN    DIM       6      19-24   MANAGER
SRDSPRICE     Form      4.2    25-31   Base Price
SRDSNLSTCDE   DIM       1      32-32   NEW LIST CODE (Y or N).
SRDSHOTLINE   DIM       1      33-33   HOTLINE CODE    (Y or N).
SRDSNEWDATE   DIM       8      34-41   DATE CARD PUT UP CCYYMMDD FORMAT
SRDSREVDATE   DIM       8      42-49   REVISION DATE CCYYMMDD FORMAT - PRINTED ON DATACARD  data date (use last clean date)
SRDSPASSWORD  DIM       10     50-59   WHOM LAST UPDATED CARD  - PRINTED ON DATACARD
SRDSMLSTNAME  DIM       75     60-134  MASTER LIST NAME.
SRDSCLEANCDE  DIM       4     135-138  CLEANED CODE (Cxxx).
SRDSCLNINFO   DIM       38    139-176  CLEAN INFORMATION.
SRDSNETNAME   DIM       4     177-180  NET NAME CODE (Nxxx).
SRDSNETINFO   DIM       38    181-218  NET NAME INFORMATION.
SRDSDELCODE   DIM       4     219-222  DELIVERY CODE (Dxxx).
SRDSSAMPLE    DIM       4     223-226  SAMPLE CODE (Pxxx).
SRDSSEX       DIM       15    227-241  SEX TEXT.   OFF 1 BYTE
SRDSMIN       DIM       11    242-252  MINIMUM TEXT.
SRDSUNIVERSE  DIM       10    253-262  UNIVERSE QUANTITY.
SRDSUNITDATA  DIM       188   263-450
SRDSNDATUPDDATE         DIM       8     451-458  UPDATE DATE          touched date
SRDSCOMMPER   DIM       6     459-467  COMMISSION.
SRDSNDATVerf  Dim       8     468-475  Date info last confirmed data 
SRDSNdatMen   form      3.2   476-481       percent men
SRDSNdatFem   Form      3.2   482-487       percent female
SRDScntct     Dim       50    488-537              
SRDScphone     RECORD          538-547      
SRDScphone1           Dim     3
SRDScphone2           Dim     3
SRDScphone3           Dim     4
              REcordend
SRDScFax       Record          548-557      
SRDScFax1             dim       3    
SRDScFax2             dim       3   
SRDScFax3             dim       4   
                      RecordEnd
SRDScEmail    Dim       50    558-607
SRDSStatus    Dim        1    608-608  
SRDSNDatFill  Dim       99   609-707      
           LISTEND


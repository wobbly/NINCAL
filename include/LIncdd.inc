..............................................................................
.
. LIncDD LIncUSION
. NIN Income report Data Descriptors
.
. FILE NAME : ListIncome.dat
. REC LENGTH: 533 FIXED
. INDEX KEY : (1)  1-6       (List #) dupes allowed
. Record ID   (2)  7-12       record ID no dupes
.INcome by List data descriptor
.used for List management income reporting to Owners
.
.
.
LincFLIST     Filelist
LIncFILE  IFILE     KEYLEN=6,FIXED=533,Name="ListIncome.ISI|NINS1:502"
LIncFILE1 IFILE     KEYLEN=6,FIXED=533,noduplicates,Name="ListIncome1.ISI|NINS1:502"
          Filelistend

LIncNAME  INIT      "ListIncome.ISI|NINS1:502"
LIncNAME1           INIT      "ListIncome1.ISI|NINS1:502"
LIncFLD   DIM       6
LIncFLD1   DIM       6
LIncFLAG  FORM      1
LincPath  Form      1
LIncVARS       LIST
LIncList       DIM             6                 1-6     .list number
LincREcID      Dim             6                 7-12    .unique record ID
LINCDATEBY     DIM             1                13-13   'M' by Mail date, 'O' by Order date
LIncTYPE       DIM             1                14-14   Basis 'C' Cash, 'I' Invoice   
.                                                     2 Records required if  Both Cash & Accrual 
LMONTH         FORM            2                15-16  Fiscal Month '1-12'
LIncREP1       DIM             1                17-17  Report option 1 Type 'M' Monthly, 'Q' Quarterly,
.                                                     2 Records required if  Both monthly & quarterly
LIncREP2       DIM             1                18-18  Report option 2 report with projections 'Y' = Include Variance
LIncREP3       DIM             1                19-19  Report option 3 Not used              
.LIncYEAR       FORM            4                20-23  Beginning year for report (drop data prior to being our list)
LIncYEAR       FORM            8                20-27  Beginning year for report (drop data prior to being our list) ccyymmdd
LIncRECIPIENT  DIM             255              28-282   Email address of recipient    
LIncAuto       DIm             1               283-283  "Y" if report runs auto
LIncInits      dim             3               284-285   Last updated by
LIncCOMMENTS   DIM             247             286-533  not used   
              listend
                       

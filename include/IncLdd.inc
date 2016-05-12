..............................................................................
.
. IncLDD INCLUSION
. NIN Income report Data Descriptors
.
. FILE NAME : IncLists.dat
. REC LENGTH: 527 FIXED
. INDEX KEY : (1)  1-6       (List #)
.
.INcome by List data descriptor
.used for List management income reporting to Owners
.
.
.
IncLNAME INIT      "IncLists.ISI|NINS1:502"
IncLFILE IFILE     KEYLEN=6,FIXED=527
IncLFLD  DIM       6
IncLFLAG FORM      1

INCLVARS       LIST
INCList       DIM             6                 1-6
INCDATEBY     DIM             1                 7-7   'M' by Mail date, 'O' by Order date
INCLTYPE      DIM             1                 8-8   Basis 'C' Cash, 'I' Invoice   
.                                                     2 Records required if  Both Cash & Accrual 
LMONTH        FORM            2                 9-10  Fiscal Month '1-12'
IncREP1       DIM             1                11-11  Report option 1 Type 'M' Monthly, 'Q' Quarterly, 'V' Monthly with Variance 
.                                                     2 Records required if  Both monthly & quarterly
IncREP2       DIM             1                12-12  Report option 2 report with projections 'Y' = Include Variance
IncREP3       DIM             1                13-13  Report option 3 Not used              
IncLYEAR      FORM            4                14-17  Beginning year for report (drop data prior to being our list)
IncRECIPIENT  DIM             255              18-272  Email address of recipient    
IncCOMMENTS   DIM             255             273-527  not used   
              listend
                       

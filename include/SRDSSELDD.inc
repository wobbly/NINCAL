.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:      SRDS_SEL
.    LENGTH:      FIXED=140
.  COMPRESS:      NONE
.      TYPE:      
......................................................
. LAST MODIFIED
. 10 MAy 2011 DLH New
......................................................
SRDSSELNAME  Init      "SRDS_SEL.Isi|NINS1:502"
SRDSSELFlist Filelist
SRDSSELFILE  IFILE     Keylen=6,Fixed=140,Name="SRDS_SEL.Isi|NINS1:502"
SRDSSELFiLE2  IFILE     Keylen=10,Fixed=140,Name="SRDS_SEL2.Isi|NINS1:502"
          FileListEnd
SRDSSELSFILE FILE      

SRDSSELFLAG  FORM      1
SRDSSELPATH  FORM      1
SRDSSELFld   Dim       6                .list number
SRDSSELFld2  Dim       10               .list number & select number
.
SRDSSELVARS        List
SRDSSELLIST  DIM       6       1-6         LIST NUMBER          
SRDSSELNUM   DIM       4       7-10        SELECT NUMBER
SRDSSELSNAME DIM       75     11-85        SELECT NAME
SRDSSELQTY   DIM       10     86-95        SELECT QUANTITY
SRDSSELPRICE FORM      5.2    96-103       SELECT CODE PRICE
SRDSSELPCOMM DIM       1     104-104       SELECT PRICE IS COMMISSIONABLE
SRDSSELDESC  DIM       3     105-107       SELECT PRICE MODIFIER
.         "   " OR "000"      NO DESCRIPTION/CALCULATION
.         "001"               /M
.         "002"               /FLAT
.         "003"               /EACH
.         "004"               /MIN
.         "005"               N/A
.         "006"               N/C
.         "007"               SEEBASE
.         "008"               SEEDESC
.         "009"               INQUIRE
SRDSSELINACTIVE        DIM       1     108-108       SELECT INACTIVE?  1 = YES
SRDSSELSTATUS          DIM       1     109-109       SELECT STATUS
.  1 = SPECIAL(DOES NOT PRINT ON DATACARD)
.  2 = OFFICE USE ONLY
.  3 = 
.
SRDSSELNOTES DIM       1     110-110       1 = INDICATES USER SHOULD VIEW NOTES ABOUT THIS SELECT
SRDSSELEXC   DIM       1     111-111       1  = Exc/Rent, 2 = Exchange Only, 3 = Rental Only
SRDSSELBASE  DIM       4     112-115       SELECT NUMBER OF ASSOCIATED BASE - BASE RECORDS WILL HOLD BLANK VALUE!!
. BASE   = BASE - ONLY ONE PER DATACARD
. SEC.   = SECONDARY BASE
. Nxxx   = NUMBER OF BASE/SECONDARY BASE
. "    " = NEITHER BASE NOR SELECT OFF OF BASE
SRDSSELINDEX DIM       4     116-119       INDEX FOR PRINTING/DISPLAY
SRDSSELDATE  DIM       8     120-127       SELECT DATE
SRDSSELINIT  DIM       3     128-130       USER INITIALS
SRDSSELFILLER          DIM       10    131-140       FILLER
                    LISTEND
          
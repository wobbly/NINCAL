******************************************************
*  NSELDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  SELECT FILE.
* ****************************************************
* NAMES IN THE NEWS MASTER LIST SELECT FILE.
* ****************************************************
.
.      FILE:      NINSEL
.    LENGTH:      141
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-10        NSELLIST + NSELNUM
.AIMDEX KEY:      1-6         NSELLIST
.                   112-115 NSELBASE
.                    11-85    NSELSNAME
......................................................
NSELNAME  INIT      "NINSEL"
.
NSELFLIST Filelist
.NSELFILE IFILE     KEYLEN=10,FIXED=141,Name="NINSEL.isi"
.NSELFLE2 AFILE     FIXED=141,Name="NINSEL.aam"
.NSELFILE IFILE     KEYLEN=10,FIXED=141,Name="NINSEL.isi|20.20.30.103:502"
NSELFILE  IFILE     KEYLEN=10,FIXED=141,Name="NINSEL.isi|NINS1:502"
.NSELFLE2 AFILE     FIXED=141,Name="NINSEL.aam|20.20.30.103:502"
NSELFLE2  AFILE     FIXED=141,Name="NINSEL.aam|NINS1:502"
                    filelistend
.
NSELLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NSELFLD   DIM       10
NSELFLD1  DIM       9
NSELFLD2  DIM       7
NSELFLD3  DIM       78
NSELFLAG  FORM      1
NSELPATH  FORM      1
.
. 
NSELVARS  LIST
NSELLIST  DIM       6         1-6       LIST NUMBER
NSELNUM   DIM       4         7-10      SELECT NUMBER
NSELSNAME DIM       75     11-85        SELECT NAME
NSELQTY   DIM       10     86-95        SELECT QUANTITY
NSELPRICE FORM      5.2    96-103       SELECT CODE PRICE
NSELPCOMM DIM       1     104-104       SELECT PRICE IS COMMISSIONABLE
NSELDESC  DIM       3     105-107       SELECT PRICE MODIFIER
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
NSELINACTIVE        DIM       1     108-108       SELECT INACTIVE?  1 = YES
NSELSTATUS          DIM       1     109-109       SELECT STATUS
.  1 = SPECIAL(DOES NOT PRINT ON DATACARD)
.  2 = OFFICE USE ONLY
.  3 = 
.
NSELNOTES DIM       1     110-110       1 = INDICATES USER SHOULD VIEW NOTES ABOUT THIS SELECT
NSELEXC             DIM       1     111-111       1  = Exc/Rent, 2 = Exchange Only, 3 = Rental Only
NSELBASE  DIM       4     112-115       SELECT NUMBER OF ASSOCIATED BASE - BASE RECORDS WILL HOLD BLANK VALUE!!
. BASE   = BASE - ONLY ONE PER DATACARD
. SEC.   = SECONDARY BASE
. Nxxx   = NUMBER OF BASE/SECONDARY BASE
. "    " = NEITHER BASE NOR SELECT OFF OF BASE
NSELINDEX DIM       4     116-119       INDEX FOR PRINTING/DISPLAY
NSELDATE  DIM       8     120-127       SELECT DATE
NSELINIT  DIM       3     128-130       USER INITIALS
NSELFILLER          DIM       11    131-141       FILLER
                    LISTEND

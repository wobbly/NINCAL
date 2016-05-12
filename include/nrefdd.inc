* NREFDD.INC
* *****************************************************************************
* NAMES IN THE NEWS MASTER DATACARD REFERENCE FILE.
* *****************************************************************************
.
.    FILE:      NINREF
.  LENGTH:      100
.    TYPE:      ISAM/AAM
.     KEY:      1-1   NREFCODE
.               2-4   NREFNUM
.  AAMKEY:          1-1   NREFCODE
.
.patch 2016 May 11 filepi,record lock, no lock options
.patch 3 2014 August 8  add description to the aam key
.Patch MAY 2011    - Add SRDS and expand
.Patch August 2006 - Add Min codes
...............................................................................
NREFNAME  INIT      "NINREF"
NREFFLIST FILELIST
NREFFILE  IFILE     KEYLEN=4,FIXED=100,Name="NINREF.isi|NINS1:502"
NREFFLE3  IFILE     KEYLEN=3,FIXED=100,Name="NINREFMin.isi|NINS1:502"      .min key
NREFFLE2  AFILE     FIXED=100,Name="NINREF.aam|NINS1:502"
NREFFLE4  IFILE     KEYLEN=35,FIXED=100,Name="NINREFSRDS.isi|NINS1:502"      .SRDS Key
.NREFFILE  IFILE     KEYLEN=4,FIXED=81,Name="NINREF.isi|NINS1:502"
.NREFFLE3  IFILE     KEYLEN=3,FIXED=81,Name="NINREFMin.isi|NINS1:502"      .min key
.NREFFLE2  AFILE     FIXED=81,Name="NINREF.aam|NINS1:502"
.NREFFLE4  IFILE     KEYLEN=28,FIXED=81,Name="NINREFSRDS.isi|NINS1:502"      .SRDS Key

                    FILELISTEND
NREFFLAG  FORM      1
NREFFLD   DIM       4            .main Isam key
NREFFLD1  DIM       4            .aam key 1
NrefFld3  Dim       3            .Min
NrefFld4  Dim       35                      .srds
.begin patch 3.0
NrefFld5  Dim       49                      .aam key 2 description
.end patch 3.0
NREFPATH  FOrm      1
NrefLock  Form        1
.
NREFVARS  LIST
NREFCODE  DIM       1       1-1     REFERENCE CODE TYPE
.TYPE OF CODES
.1         "A"         ADDRESSING CODE
.2         "C"         CLEANED CODE
.3         "D"         DELIVERY CODE
.4         "L"         SELECTION CODE
.5         "M"         MEDIA CODE
.6         "N"         NET NAME CODE
.7         "P"         SAMPLE CODE
.8         "R"         ARRANGEMENT CODE
.9         "S"         SOURCE CODE
.10        "T"         CATEGORY CODE
NREFNUM   DIM       3       2-4     REFERENCE #
NREFDESC  DIM       46      5-50    DESCRIPTION   aam key fld5
NrefMin   Dim       3      51-53    MIN code  key
NrefSRDS  Dim       35     54-88    SRDS  - KEY       
NREFFILL  DIM       12     89-100   FILLER
.NREFFILL DIM       28     54-81    FILLER
.NREFFILL DIM       31     52-81    FILLER
                    LISTEND
NREFSTR             DIM       150

.Min code descriptions need to be added.
.first pass
.1 "B" Business
.2 "C" Consumer
.3 "A"  AGE
.4 "D"  Dollar select
.5 "E"  Enhance select
.6 "F"  FSA
.7 "G"  Geographic ??
.8 "H"
.9 "L"   Media
.10 "M" 
.11 "P"
.12 "R"
.13 "T"
.14 "U"  UNinverse
.15 "X"  special pricing / commission ??
.16 "W"  web related ??

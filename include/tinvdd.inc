..............................................................................
.
. TINVDD INCLUSION
. TDMC INVOICE FILE DEFINITION
.
. FILE NAME : TDMCINV
. REC LENGTH: 91 FIXED
. INDEX KEY : 1-6 (lr dupes allowed)
..............................................................................
.
TINVFILE IFILE     KEYLEN=6,dup,Name="TDMCINV.isi|NINS1:502"

TINVNAME INIT      "TDMCINV|NINS1:502"
TINVFLD  DIM       6
TINVFLAG FORM      1
TINVLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=No locks
.
Tinvvars list
TINVLR   DIM       6         001-006    NIN LR #
TINVB1   DIM       4         007-010    BLANK
.TINVdate DIM       6         011-016   Date
TINVdate DIM       8         011-018    Date     ccyymmdd   SB INVOICE DATE
TINVdesc DIM       30        019-048    NAME
.TinvINV  DIM       6         049-054   TDMC ID NUM
TinvINV  DIM       11        049-059   TDMC ID NUM
TinvDOLR DIM       12        060-071    $$$$
.begin patch
tinvOrd    dim        11         072-082    infogroup order #
TinvFiller DIM     9          083-091    
.TinvFiller DIM     20        072-091    
         listend
.tempory hold vars         
TinvDolrH   DIM       12            $$$$       
TINVdateH   DIM       8             Date     ccyymmdd
TinvINVH  DIM       11        049-059   TDMC ID NUM
.New file format
.TinvACt  DIM       6         001-006   TDMC Account ID 
.tinvb2   dim       2         007-008
.tinvid   dim       6         009-014
.TINVLR   DIM       6         015-020   NIN LR #
.TINVB1   DIM       4         021-064    BLANK
.TINVdate DIM       6         025-030   Date
.TINVdesc DIM       30        031-060   NAME
.TinvDOLR DIM       12        0??????   $$$$
..............................................................................*


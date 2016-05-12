*        NXCHDD/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER EXCHANGE ACCOUNT DETAIL FILE.
* *****************************************************************************
.
.    FILE:      NINXCHNG, NINLRXNG
.  LENGTH:      200
.COMPRESS:      NONE
.    TYPE:      ISAM/AAM
.     KEY:      EXKEY,LR
.
.         DLH       08 Feb 2010   nxchlock
.               ASH  05APR2005 Complete rewrite - File Conversion
...............................................................................
NXCHNAME  INIT      "NINXCHNG|NINS1:502 "
.
NXCHFLIST FILELIST
NXCHFILE  IFILE     KEYLEN=17,FIXED=200,Name="NINXCHNG.isi|NINS1:502"
NXCHFLE2  IFILE     KEYLEN=6,FIXED=200,Name="NINLRXNG.isi|NINS1:502"
NXCHFLE3  AFILE     FIXED=200,Name="NINXCHNG.aam|NINS1:502"
.
                    FILELISTEND
.
NXCHFLD1  DIM       17
NXCHFLD2  DIM       6
NXCHFLD3  DIM       9         .AamDex Mlr 1
NXCHFLD4  DIM       9         .AamDex Mlr 2
NXCHPATH  FORM      1         .1=mlr,mlr,entry;2=lr
NXCHFLAG  FORM      1
NXCHLock  FORM      1          0/1 filepi, 2 record locks, 3 no locks
.
nxchvars  list
EXKEY               DIM       17        1-17      MLR1,MLR2,ENTRY
LR2                 DIM        3       18-20
LR                  DIM       6        21-26      LR #
USAGE1              FORM      10       27-36      MAILER1 USAGE
USAGE2              FORM      10       37-46      MAILER2 USAGE
QTYfill             FORM      3        47-49      ORDER QTY expansion
QTY                 FORM      9        50-58      ORDER QTY
LIST                DIM       6        59-64      LIST #
DAT                 DIM       8        65-72      Order DATE CCYYMMDD
STAT                DIM       1        73-73      exchange STATUS
.                                                     'C' =CANCELLED ADJUSTED, 'R'=RENTAL,
.                                                     'X'=CANCELLED NOT ADJUSTED.
MLRSW               DIM       1        74-74      1 IF MLR1, 2 IF MLR2
TYPE                DIM       2        75-76      TYPIST INITALS
XCHCOMNT            DIM       100      77-176     KEYED IN COMMENTS
.begin patch 11 Feb 09
Date1               Dim       8         177-184            .order date or date of manual entry
DateM               Dim       8         185-192            .Mail date of manual entry
.xchfiller dim       24      177-200
xchfiller           dim       8         193-200
.end patch 11 Feb 09
                    listend
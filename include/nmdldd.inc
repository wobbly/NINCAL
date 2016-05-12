;    Last change:  DH   13 Aug 1999    8:51 am
..............................................................................
.
. NMDLDD INCLUSION
. NIN BOOKING SCHEDULE FILE DEFINITION
.
. FILE NAME : NINMDLST
. REC LENGTH: 558 FIXED
. INDEX KEY : 1-7 (LIST#/MAILER# + Type Indicator)
. LAST REVISED:
.          04Oct04 ASH Increased mdltext
.        13Aug99 DLH change text to edittext box add lcr code 1
.        09JUN93 DLH LINES 3 & 4.
..............................................................................
.START PATCH 04OCT04 REPLACED LOGIC
.NMDLFILE IFILE     KEYLEN=7,FIX=333
NMDLFILE IFILE     KEYLEN=7,FIX=558,Name="NINMDLST.isi|NINS1:502"
.START PATCH 04OCT04 REPLACED LOGIC
.
NMDLFLAG FORM      1
NMDLNAME INIT          "NINMDLST.isi|NINS1:502"
NMDLFLD    DIM         7
.
. NINMDLST FILE
. .............
.
MDLVARS   List
mdlkey    DIM       6       1-6    .KEY - LIST NUMBER                 1-6
mdltype   dim       1       7-7    ." " =  List, "1" = Mailer
mdlCODE   DIM       1       8-8    .SCHEDULING ALLOWED = ' '          7-7
mdlLCRCD  DIM       1       9-9    .LCR'S ALLOWED = ' '               8-8
mdllcrcd1 dim       1      10-10   ."1"=lcr,"2"=galley 1,"3"=galley 2 9-9
mdlPLAN   DIM       3      11-13   .PLANNERS INITIALS                10-12
mdlCALL   DIM       3      14-16   .CALLERS INITIALS                 13-15
.START PATCH 04OCT04 REPLACED LOGIC
.mdltext  dim            283    17-299  .'4 70 byte LINES plus room FORM 3 CR 16-298
..ADDED FOLLOWING VARS ON APRIL 27 - ASH - INCREASED RECORD SIZE!!!!!
.mdldate  dim            8     300-307  .Date of last update
.mdlinits       dim            10    308-317  .Initials of person who last updated
.mdllcrcd2      dim            1     318-318  ."Y" = In-House LCR Clearance Process is required for this List
.mdlfiller      dim            23    319-333  .Filler
mdltext   dim       500    17-516  .'4 70 byte LINES plus room FORM 3 CR 16-298
mdldate   dim       8     517-524  .Date of last update
mdlinits  dim       10    525-534  .Initials of person who last updated
mdllcrcd2 dim       1     535-535  ."Y" = In-House LCR Clearance Process is required for this List
mdlfiller dim       23    536-558  .Filler
.END PATCH 04OCT04 REPLACED LOGIC
               ListEND

.MDLVARS   List
.mdlkey   DIM        6      KEY - LIST NUMBER           1-6
.mdlLINE1 DIM       70      LINE ONE DESCRIPTION.       7-76
.mdlLINE2 DIM       68      LINE TWO DESCRIPTION.      77-144
.mdlCODE  DIM       1       SCHEDULING ALLOWED = ' '  145-145
.mdlLCRCD DIM       1       LCR'S ALLOWED = ' '       146-146
.mdlPLAN  DIM       3       PLANNERS INITIALS         147-149
.mdlCALL  DIM       3       CALLERS INITIALS          150-152
.mdlLINE3 DIM       70      LINE ONE DESCRIPTION.     153-222
.mdlLINE4 DIM       70      LINE TWO DESCRIPTION.     223-292
.         ListEND

.old variable names
.sCHKEY   DIM        6      KEY - LIST NUMBER           1-6
.SCHLINE1 DIM       70      LINE ONE DESCRIPTION.       7-76
.SCHLINE2 DIM       68      LINE TWO DESCRIPTION.      77-144
.SCHCODE  DIM       1       SCHEDULING ALLOWED = ' '  145-145
.SCHLCRCD DIM       1       LCR'S ALLOWED = ' '       146-146
.SCHPLAN  DIM       3       PLANNERS INITIALS         147-149
.SCHCALL  DIM       3       CALLERS INITIALS          150-152
.SCHLINE3 DIM       70      LINE ONE DESCRIPTION.     153-222
.SCHLINE4 DIM       70      LINE TWO DESCRIPTION.     223-292
.

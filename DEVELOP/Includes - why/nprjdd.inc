..............................................................................
.
. NPRJDD INCLUSION
. NIN PrjECTION FILE DEFINITION
.27 Sep 2010  convert to AR & AP
. 16January2003 -- allow for spread by month.
. ASH 17JAN2003 Added monthly break-out of NIN income (DLH added monthly break-out of LR income earlier)
.
. FILE NAME : Prjdolr
. REC LENGTH: 350 FIXED                 .
.               302 FIXED OLD
. INDEX KEY : PrjType
.               PrjSrc 
.               PrjID  
.               PrjYr  
.               PrjKey 
.
..............................................................................
NPRJLIST  Filelist
NPRJFILE  IFILE     KEYLEN=14,FIX=350,UNCOMP,Name="ProjREC.ISI"
NPRJFLE2  AFILE     FIX=350,Name="ProjREC.AAM"
                    Filelistend
NPRJFLSQ  FILE
NPRJNAME  INIT      "ProjREC       "
NPRJFLD             DIM       14
NPRJFLD1  DIM       4
NPRJFLD2  DIM       4
NPRJFLD3  DIM       9
NPRJFLD4  DIM       7
NPRJFLD5  DIM       5
NPRJPATH  FORM      1
NPRJFLAG  FORM      1
NPRJFLG2  FORM      1
NPRJLOCK  FORM      1
.
PRJVARS   list
PrjType   dim       1           1-1               .Rent/Exchange(ISAM/AAM)  --- don't care
PrjSrc    dim       1           2-2               .Sales/LM(ISAM/AAM)
PrjClient dim       6           3-8             .Client Number (ISAM/AAM) Will be 4 byte Mailer OR 6 byte List if LM!
PrjYr     dim       4           9-12              .Prjection Year (ISAM/AAM)
PrjKey    dim       2          13-14              .Allows multiple Prjections for a particular year (ISAM/AAM)
PrjDate   dim       8          15-22              .Date Projection record was created CCYYMMDD
PrjMod    dim       8          23-30              .Date Prjection record was last modified CCYYMMDD
PRJAR     form      11         31-41              .Prjected AR Income for particular year
PRJAP     form      11         42-52              .Prjected AP Income for particular year
PrjNotes  dim       200        53-252             .Notes for particular record
PrjMast   dim       1         253-253             .Indicates whether this record is current Prjection for this client/this year
PRJARJan  dim       3         254-256   .AR percent for month  ..note if all Prjmonths are ZERO and we have Prjection for year
PRJARFeb  dim       3         257-260   .AR percent for month  ..spread it equally across the year
PRJARMar  dim       3         261-263         .AR percent for month
PRJARApr  dim       3         264-266         .AR percent for month
PRJARMay  dim       3         267-269         .AR percent for month
PRJARJun  dim       3         270-272         .AR percent for month
PRJARJul  dim       3         273-275         .AR percent for month
PRJARAug  dim       3         276-277         .AR percent for month
PRJARSep  dim       3         278-280         .AR percent for month
PRJAROct  dim       3         281-283         .AR percent for month
PRJARNov  dim       3         284-286         .AR percent for month
PRJARDec  dim       3         287-289         .AR percent for month
PRJAPJan  dim       3         290-292   .AP percent for month  ..note if all Prjmonths are ZERO and we have Prjection for year
PRJAPFeb  dim       3         293-295   .AP percent for month  ..spread it equally across the year
PRJAPMar  dim       3         296-298         .AP percent for month
PRJAPApr  dim       3         299-301         .AP percent for month
PRJAPMay  dim       3         302-304         .AP percent for month
PRJAPJun  dim       3         305-307         .AP percent for month
PRJAPJul  dim       3         308-310         .AP percent for month
PRJAPAug  dim       3         311-313         .AP percent for month
PRJAPSep  dim       3         314-316         .AP percent for month
PRJAPOct  dim       3         317-319         .AP percent for month
PRJAPNov  dim       3         320-322         .AP percent for month
PRJAPDec  dim       3         323-325         .AP percent for month
PrjFill   dim       25        326-350             .Filler for future fields
          listend
.

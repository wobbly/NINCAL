..............................................................................
.
. NPRJDD INCLUSION
. NIN ProjECTION FILE DEFINITION
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
.NPRJFILE  IFILE     KEYLEN=14,FIX=350,UNCOMP,Name="ProjREC.ISI"
NPRJFILE  IFILE     KEYLEN=14,FIX=445,UNCOMP,Name="ProjDolr.ISI"
NPRJFLE2  AFILE     FIX=445,Name="ProjDolr.AAM"
                    Filelistend
NPRJFLSQ  FILE
NPRJNAME  INIT      "ProjDolr      "
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
PRJVARS             list
PrjType             dim       1           1-1               .Rent/Exchange(ISAM/AAM)  --- don't care
PrjSrc              dim       1           2-2               .Sales/LM(ISAM/AAM)
PrjClient           dim       6           3-8             .Client Number (ISAM/AAM) Will be 4 byte Mailer OR 6 byte List if LM!
PrjYr               dim       4           9-12              .Projection Year (ISAM/AAM)
PrjKey              dim       2          13-14              .Allows multiple Pr0jections for a particular year (ISAM/AAM)
PrjDate             dim       8          15-22              .Date Projection record was created CCYYMMDD
PrjMod              dim       8          23-30              .Date Prjection record was last modified CCYYMMDD
PrjLR              form      11         31-41              .Projected LR Income for particular year
PrjNin             form      11         42-52              .Projected NIN Income for particular year
PrjNotes           dim       200        53-252             .Notes for particular record
PrjMast            dim       1         253-253             .Indicates whether this record is current projection for this client/this year
prjLRJan           dim       3         254-256   .LR percent for month  ..note if all projmonths are ZERO and we have projection for year
prjLRFeb           dim       3         257-260   .LR percent for month  ..spread it equally across the year
prjLRMar           dim       3         261-263         .LR percent for month
prjLRApr           dim       3         264-266         .LR percent for month
prjLRMay           dim       3         267-269         .LR percent for month
prjLRJun           dim       3         270-272         .LR percent for month
prjLRJul           dim       3         273-275         .LR percent for month
prjLRAug           dim       3         276-277         .LR percent for month
prjLRSep           dim       3         278-280         .LR percent for month
prjLROct           dim       3         281-283         .LR percent for month
prjLRNov           dim       3         284-286         .LR percent for month
prjLRDec           dim       3         287-289         .LR percent for month
.START PATCH 17JAN2003
prjNINJan          dim       3         290-292   .NIN percent for month  ..note if all projmonths are ZERO and we have projection for year
prjNINFeb          dim       3         293-295   .NIN percent for month  ..spread it equally across the year
prjNINMar          dim       3         296-298         .NIN percent for month
prjNINApr          dim       3         299-301         .NIN percent for month
prjNINMay          dim       3         302-304         .NIN percent for month
prjNINJun          dim       3         305-307         .NIN percent for month
prjNINJul          dim       3         308-310         .NIN percent for month
prjNINAug          dim       3         311-313         .NIN percent for month
prjNINSep          dim       3         314-316         .NIN percent for month
prjNINOct          dim       3         317-319         .NIN percent for month
prjNINNov          dim       3         320-322         .NIN percent for month
prjNINDec          dim       3         323-325         .NIN percent for month
.begin patch
PRJAR               form      11        326-336        .Prjected AR Income for particular year
PRJAP               form      11        337-347        .Prjected AP Income for particular year
.Note numbering  is off
PRJARJan            dim       3         349-351         .AR percent for month  ..note if all Prjmonths are ZERO and we have Prjection for year
PRJARFeb            dim       3         352-354         .AR percent for month  ..spread it equally across the year
PRJARMar            dim       3         355-357         .AR percent for month
PRJARApr            dim       3         358-360         .AR percent for month
PRJARMay            dim       3         361-363         .AR percent for month
PRJARJun            dim       3         364-366         .AR percent for month
PRJARJul            dim       3         367-369         .AR percent for month
PRJARAug            dim       3         370-372         .AR percent for month
PRJARSep            dim       3         373-375         .AR percent for month
PRJAROct            dim       3         376-378         .AR percent for month
PRJARNov            dim       3         379-381         .AR percent for month
PRJARDec            dim       3         382-384         .AR percent for month
PRJAPJan            dim       3         385-387   .AP percent for month  ..note if all Prjmonths are ZERO and we have Prjection for year
PRJAPFeb            dim       3         388-390   .AP percent for month  ..spread it equally across the year
PRJAPMar            dim       3         391-393         .AP percent for month
PRJAPApr            dim       3         394-396         .AP percent for month
PRJAPMay            dim       3         397-399         .AP percent for month
PRJAPJun            dim       3         400-402         .AP percent for month
PRJAPJul            dim       3         403-405         .AP percent for month
PRJAPAug            dim       3         406-408         .AP percent for month
PRJAPSep            dim       3         409-411         .AP percent for month
PRJAPOct            dim       3         412-414         .AP percent for month
PRJAPNov            dim       3         415-417         .AP percent for month
PRJAPDec            dim       3         418-420         .AP percent for month
PrjFill             dim       26        425-446             .Filler for future fields
          listend
.         

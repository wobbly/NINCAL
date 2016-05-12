..............................................................................
.
. NBJCDD INCLUSION
. NIN OWNER FILE DEFINITION
.
. FILE NAME : NINBJC   - batch job control QUEUE file
. REC LENGTH: 400 FIXED
. INDEX KEY : 1-11
. INDEX KEY2 : 2-11
..............................................................................
.

NBJCName  Init      "NINBJC.Dat|NINS1:502"
NBJCFList Filelist
NBJCFLE1  IFILE     KEYLEN=11,FIXED=700,Name="NINBJC.isi|NINS1:502"
NBJCFLE2  IFILE     KEYLEN=10,FIXED=700,Name="NINBJC2.isi|NINS1:502"
          filelistend
NBJCFLE3   FILE     FIXED=700

NBJCFLD   DIM       11          .Status & ID        
NBJCFLD2  DIM       10           .ID
NBJCFLG1  FORM      1
NBJCPATH  FORM      1
.
NBJCvars   list
NBJCStat   DIM       1         001-001   .record status  'H'=Hold, ' '=LIVe
NBJCKEY    DIM       10        002-011   .record ID Number
NBJCSrvr   DIM       15        012-026   .If present = Machine to be submitted to else local
NBJCrecvd  Dim       12        027-038   .submitted to this Queue ccyymmddhhmm
NBJCTask   DIM       255       039-294   .Task Details
NBJCWPAth  Dim       254       307-548   .work path
NBJCPRIO   Form     1          549-549   .priority    
NBJCNstart Dim      8          550-557    .next start time
NBJCNDate  Dim      8          558-565    .next start Date
NBJCCDOW   DIm      9          566-574    .next start Day of week
NBJCTIME   dim      8          575-582     
NBJChrs    DIM      2          583-584    .hours
NBJCmins   Dim      3          585-587    .minutes
NBJCAMPM   DIM      3          588-590    
nbjcSun    dim      1          591-591     .run in "T" or "F"
nbjcMon    dim      1          592-591     .run in "T" or "F"
nbjcTue    dim      1          593-591     .run in "T" or "F"
nbjcWed    dim      1          594-591     .run in "T" or "F"
nbjcThu    dim      1          595-591     .run in "T" or "F"
nbjcFri    dim      1          596-591     .run in "T" or "F"
nbjcSat    dim      1          597-591     .run in "T" or "F"
nbjcDay1   dim      1          598-591     .run in "T" or "F"
nbjcDay2   dim      1          599-591     .run in "T" or "F"
nbjcDay3   dim      1          600-600     .run in "T" or "F"
nbjcDay4   dim      1          601-600     .run in "T" or "F"
nbjcDay5   dim      1          602-591     .run in "T" or "F"
nbjcDay6   dim      1          603-591     .run in "T" or "F"
nbjcDay7   dim      1          604-591     .run in "T" or "F"
nbjcDay8   dim      1          605-591     .run in "T" or "F"
nbjcDay9   dim      1          606-591     .run in "T" or "F"
nbjcDay10   dim      1         607-591     .run in "T" or "F"
nbjcDay11   dim      1         608-591     .run in "T" or "F"
nbjcDay12   dim      1         609-591     .run in "T" or "F"
nbjcDay13   dim      1         610-591     .run in "T" or "F"
nbjcDay14   dim      1         611-591     .run in "T" or "F"
nbjcDay15   dim      1         612-591     .run in "T" or "F"
nbjcDay16   dim      1         613-591     .run in "T" or "F"
nbjcDay17   dim      1         614-591     .run in "T" or "F"
nbjcDay18   dim      1         615-591     .run in "T" or "F"
nbjcDay19   dim      1         616-591     .run in "T" or "F"
nbjcDay20   dim      1         617-591     .run in "T" or "F"
nbjcDay21   dim      1         618-591     .run in "T" or "F"
nbjcDay22   dim      1         619-591     .run in "T" or "F"
nbjcDay23   dim      1         620-591     .run in "T" or "F"
nbjcDay24   dim      1         621-591     .run in "T" or "F"
nbjcDay25   dim      1         622-591     .run in "T" or "F"
nbjcDay26   dim      1         623-591     .run in "T" or "F"
nbjcDay27   dim      1         624-591     .run in "T" or "F"
nbjcDay28   dim      1         625-591     .run in "T" or "F"
nbjcDay29   dim      1         626-591     .run in "T" or "F"
nbjcDay30   dim      1         627-591     .run in "T" or "F"
nbjcDay31   dim      1         628-591     .run in "T" or "F"
NBJCDBEOM   form     2         629-630    .run x days before eom
NBJCFill   DIM       70        631-700   .Not USed
           listend

.*Notes: Windows 7 task scheduler - no way to pause all jobs, now way to keep jobs from running simultaniously.
.to build our own scheduler hould have to add:
.months to run
.days to run.
.hours to run
.repeatitions
.single instance? can run with other jobs
.Holidays?
.run last day of month
.split into two files, Queue and LOg
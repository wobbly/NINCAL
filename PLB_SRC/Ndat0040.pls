pc       equ       0
         INCLUDE   COMMON.INC
         include   cons.inc
         INCLUDE   ndatdd.INC
output   ifile      keyl=6
release  init      "1.03"         02OCT2000 ASH NEW SERVER ADDED
.release  init      "1.02"         25Mar1999 JD writing out master listname instead of olstname
.release  init      "1.01"        17Mar1998 DLH change to no locks on datacard read
.release  init      "1"           30Oct97 DLH
. OPEN INPUT FILES
OPEN     TRAP      stop IF F5
         move      "ndat0003" to program
         move      "Names in the News CA" to compnme
         move      "Convert datacards " to stitle
         move      "Abort" to pf5
         clock     date to today
         unpack    today into mm,str1,dd,str1,yy
         call      paint
         call      funcdisp
. 
         move      c1 to ndatpath
         move      c3 to ndatlock        .no locks added 17Mar1998 dlh
.START PATCH 1.03 REPLACED LOGIC
.         PREPARE    OUTPUT,"g:\data\dl\ndatdl.txt","g:\data\dl\ndatdl","6","66",exclusive
         PACK       STR35,NTWKPATH1,"dl\ndatdl.txt"
         PACK       STR45,NTWKPATH1,"dl\ndatdl.isi"
         PREPARE    OUTPUT,str35,str45,"6","66",exclusive
.END PATCH 1.03 REPLACED LOGIC
         move      "******" to lstnum
         write     output,lstnum;"LISTNUMBER",",LISTNAME                          "
looper   
         CALL      ndatseq
         goto      stop if over
         call      rotdial
         rep       zfill in lstnum
         read      output,lstnum;;
         goto      looper if not over
         rep       "#"'" in mlstname
         rep       ", " in mlstname
         write     output,lstnum;*cdfon,lstnum,mlstname
         add       c1 to n6
         display   *p10:12,"Records out : ",n6
         goto      looper                
STOP     
         close     output
         STOP
         include    ndatio.inc
         include    comlogic.inc

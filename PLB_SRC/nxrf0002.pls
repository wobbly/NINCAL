*NXRF0002 - print LISTMLR RECORDS.
*WRITTEN    21aug95
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NXRFDD.INC
;patch1.04
.         include   nmlrdd.inc
	include	compdd.inc
	include	cntdd.inc
;patch1.04
         include   ndatdd.inc
         include   hp.inc
.Start Patch #1.01 - key expanded to reflect change to MCOMP
.output   ifile     keylen=50,dup
.key50    dim       50
.begin patch 1.03
.output   ifile     keylen=70,dup
.key70    dim       70
.START PATCH 1.05 REPLACED LOGIC
.output   ifile     keylen=80,dup
.key80    dim       80
output   ifile     keylen=90,dup
key90    dim       90
.END PATCH 1.05 REPLACED LOGIC
.end patch 1.03
.End Patch #1.01 - key expanded to reflect change to MCOMP
page     form      "00"
lines    form      2
.START PATCH 1.05 REPLACED LOGIC
.holdmlr  dim       4
holdmlr  dim       6
.END PATCH 1.05 REPLACED LOGIC
recsin   form      4
recsout  form      4
.
release  init      "1.05"  ASH	19MAY2005	LISTMLR Conversion
.release  init      "1.04"  DMB	26MAY2004	Mailer Conversion
.release  init      "1.03"  DLH 24OCT2000 file size
.release  init      "1.02"  ASH 04OCT2000 NEW SERVER ADDED
.release  init      "1.01"  ASH 22SEP98 NINMLR Y2K File expansion
.RELEASE  INIT      "1.00"
.
         MOVE      "NAMES IN THE NEWS CA INC" TO COMPNME
         MOVE      "NXRF0002" TO PROGRAM
         MOVE      "LIST/MLR X-REF print" TO STITLE
         MOVE      "EXIT" TO PF5
         TRAP      STOP IF F5
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C2 TO NXRFPATH
.         move      c1 to nmlrpath
         move      c1 to ndatpath
.Start Patch #1.01 - key  and file expanded to reflect change to MCOMP
.         prepare   output,"g:\data\nxrfout","g:\data\nxrfout","50","60"
.START PATCH 1.2 REPLACED LOGIC
.         prepare   output,"g:\data\nxrfout","g:\data\nxrfout","70","80"
         PACK      STR35,NTWKPATH1,"NXRFOUT"
         PACK      STR45,NTWKPATH1,"NXRFOUT"
.         prepare   output,STR35,STR45,"70","80"
.begin patch 1.03
.START PATCH 1.05 REPLACED LOGIC
.         prepare   output,STR35,STR45,"80","90"
         prepare   output,STR35,STR45,"90","100"
.END PATCH 1.05 REPLACED LOGIC
.end patch 1.03
.END PATCH 1.2 REPLACED LOGIC
.End Patch #1.01 - key  and file expanded to reflect change to MCOMP
         display   *p10:12,"Pass one"
.
read
         CALL      NXRFks
         GOTO      pass2 IF  OVER
         add       c1 to recsin
         display   *p10:14,"records in ",recsin
.START PATCH 1.05 REPLACED LOGIC
.         pack      mkey from nxrfmlr,z3
.         clear     mcomp
.         call      nmlrkey
	pack	COMPFLD,nxrfmlr
	clear	COMPCOMP
	move	"read-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
.END PATCH 1.05 REPLACED LOGIC
         clear     olstname
         pack      ndatfld from nxrflist
         call      ndatkey
.Start Patch #1.01 - key expanded to reflect change to MCOMP
.         pack      key50 from mcomp,olstname
.         write     output,key50;key50,nxrflist,nxrfmlr
.begin patch 1.03
.         pack      key70 from mcomp,olstname
.         write     output,key70;key70,nxrflist,nxrfmlr
.START PATCH 1.05 REPLACED LOGIC
.         pack      key80 from mcomp,olstname
.         write     output,key80;key80,nxrflist,nxrfmlr
         pack      key90 from COMPcomp,olstname
         write     output,key90;key90,nxrflist,nxrfmlr
.END PATCH 1.05 REPLACED LOGIC
.end patch 1.03
.End Patch #1.01 - key expanded to reflect change to MCOMP
         goto      read
pass2
         display   *p10:12,"Pass two"
         close     output
.START PATCH 1.2 REPLACED LOGIC
.         open      output,"g:\data\nxrfout"
.         splopen   "g:\data\nxrfout"
         PACK      STR35,NTWKPATH1,"NXRFOUT"
         open      output,STR35
         splopen   STR35
.END PATCH 1.2 REPLACED LOGIC
         print     hpdupl,*f
         call      header
.begin patch 1.03
.prtloop  readks    output;mcomp,str25,nxrflist,nxrfmlr
prtloop
.START PATCH 1.05 REPLACED LOGIC
.         readks    output;mcomp,str35,nxrflist,nxrfmlr
         readks    output;COMPcomp,str35,nxrflist,nxrfmlr
.END PATCH 1.05 REPLACED LOGIC
.end patch 1.03
         goto      stop if over
         add       c1 to recsout
         display   *p10:16,"records out ",recsout
         compare   "57" to lines
         call      header if not less
         match     nxrfmlr to holdmlr
         if        not equal
         move      nxrfmlr to holdmlr
.START PATCH 1.05 REPLACED LOGIC
.         print     *1,hpbon,Nxrfmlr,b1,mcomp,hpboff
         print     *1,hpbon,Nxrfmlr,b1,COMPCOMP,hpboff
.END PATCH 1.05 REPLACED LOGIC
         add       c1 to lines
         endif
         compare   "57" to lines
         if         not less
         call       header
.START PATCH 1.05 REPLACED LOGIC
.         print     *1,hpbon,Nxrfmlr,b1,mcomp,hpboff," (Continued)"
         print     *1,hpbon,Nxrfmlr,b1,COMPCOMP,hpboff," (Continued)"
.END PATCH 1.05 REPLACED LOGIC
         add       c1 to lines
         endif
.begin patch 1.3
.         print     *20,str25,b1,nxrflist
         print     *20,str35,b1,nxrflist
.end patch 1.3
         add       c1 to lines
         goto      prtloop
STOP     STOP
header   add       c1 to page
         clock     date to today
         print     *f,*l,*1,today,*30,"X-Ref listing" *40,"Page ",page,*l,*l
         move      c4 to lines
         print     *6,"Mailer",*20,"Associated lists",*l,*l
         add       c3 to lines
         return
         INCLUDE   NXRFIO.INC
;patch1.04
;         include   nmlrio.inc
		include	compio.inc
		include	cntio.inc
;patch1.04
         include   ndatio.inc
         INCLUDE   COMLOGIC.INC


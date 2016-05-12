PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   norddd.inc
        
.
release  init      "1.3"         02OCT2000 ASH NEW SERVER ADDED
.release  init      "1.2"         05jan99   ASH NINORD Y2K, File expansion
.release  init      "1.1"         21Dec1998 DLH make extra copy for testing.
.RELEASE  INIT      "1.0"           jdjul1996 new
.
.Start Patch #1.2 - increased file size
.faxship  IFILE     KEYLEN=6,fixed=344
faxship  IFILE     KEYLEN=6,fixed=498
.end Patch #1.2 - increased file size
INFILE   FILE      
.Start Patch #1.2 - increased file size
.shipout   IFILE     KEYLEN=6,fixed=344
..begin patch 1.1
.tempout   file     fixed=344
..end patch 1.1
shipout   IFILE     KEYLEN=6,fixed=498
.begin patch 1.1
tempout   file     fixed=498
.end patch 1.1
.end Patch #1.2 - increased file size
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
LR       DIM       6
.
         MOVE      "Nshp0005" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "COPY shipping info -> SHIPFAXt" TO STITLE
         CALL      PAINT
         OPEN      INFILE,"shipfax",SHARE
         OPEN      faxship,"shipfax"
         OPEN      shipOUT,"shipfaxt"
.begin patch 1.1
.START PATCH 1.3 REPLACED LOGIC
.         prepare   tempout,"g:\data\shipfaxb.bak"
         pack      str35,NTWKPATH1,"shipfaxb.bak"
         prepare   tempout,STR35
.END PATCH 1.3 REPLACED LOGIC
.end patch 1.1
.
READ     FILEPI    1;INFILE
         READ      INFILE,SEQ;ordvars
         GOTO      EOJ IF OVER
         ADD       ONE TO READCNT
         DISPLAY   *P10:12,"NUMBER OF shipping  READ : ",READCNT
WRITE    FILEPI    3;shipOUT
         read      shipout,olrn;;
         goto      del if not over
         WRITE     shipOUT,oLRn;ordvars
.begin patch 1.1
         write     tempout,seq;ordvars
.end patch 1.1
del      MOVE      oLRn TO KEY
         REP       " 0" IN KEY
         FILEPI    2;faxship
         READ      faxship,KEY;;
         DELETE    faxship,KEY
         GOTO      READ
EOJ
.dlh 21Dec98 not needed on isam writes
.       WEOF      shipOUT,SEQ
.begin patch 1.1
         weof      tempout,seq
.end patch 1.1
         IFNZ      PC
         FLUSH     mrgOUT
         XIF
         CLOSE     shipOUT,EOFSIZE
.begin patch 1.1
         close     tempout
.end patch 1.1
         CLOSE     faxship
         CLOSE     INFILE
         shutdown  "cls"
         include   nordio.inc
         INCLUDE   COMLOGIC.inc


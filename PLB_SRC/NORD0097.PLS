PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NORDDD.inc
         inc       ninvdd.inc
         inc       ndatdd.inc
         INCLUDE   NSPEDD.INC
.
RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
TDMCORD  FILE      
JDout    file
INFILE   FILE      VAR=7
TDMCOUT  IFILE     KEYLEN=6,VAR=288
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
writupd  FORM      4
writCNT  FORM      4
spec1    form      1
LR       DIM       6
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
DESC0L1  DIM       47       7-53      LINE 1 OF 00 INSTRUCTION
DESC0L2  DIM       47      54-100     LINE 2 OF 00 INSTRUCTION
DESC991  DIM       47     101-147     LINE 1 OF 99 INSTRUCTION
DESC992  DIM       47     148-194     LINE 2 OF 99 INSTRUCTION
DESC981  DIM       47     195-241     LINE 1 OF 98 INSTRUCTION
DESC982  DIM       47     242-288     LINE 2 OF 98 INSTRUCTION
.
.
         MOVE      "NORD0097" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "Fix mailer #'s" TO STITLE
         move      c1 to nordpath
         move      c1 to ninvpath
         move      c1 to ndatpath
         CALL      PAINT
....change input
         OPEN      TDMCORD,"\\nins1\e\data\diskin24.dat"
.
READ
MainLoop read      tdmcord,seq;ordvars
         goto      eoj if over
         ADD       ONE TO READCNT
         DISPLAY   *P10:20,"NUMBER OF ORDERS READ : ",READCNT

         move       olrn to nordfld
         call       nordtst


....change Mailer ##
.        move       "2798",omlrnum
.        move       "2739",omlrnum                  .HUman rights watch
.        move       "0911",omlrnum                  .Foreign Affairs
.        move       "1584",omlrnum                  .SPCA international
.        move       "4019",omlrnum                  .
        move       "7299",omlrnum                  .
          call       nordupd
         DISPLAY   *P10:22,"NUMBER OF orders written  : ",writcnt
.
         GOTO      READ
.
eoj         CLOSE     TDMCORD
         shutdown   "cls"
         INCLUDE   COMLOGIC.inc
         include   ninvio.inc
           include   ndatio.inc
        include    nordio.inc
         INCLUDE   NSPEIO.INC


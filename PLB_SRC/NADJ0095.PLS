PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NORDDD.inc
         inc       ninvdd.inc
          include   nadjdd.inc
         INCLUDE   NJSTDD.inc
         INCLUDE   NSPEDD.INC
.
RELEASE  INIT      "1.3"       DLH      copy missing adjustment detail from print file to master
.RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
ADJPRT   FILE      FIXED=193
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
writupd  FORM      4
writCNT  FORM      4
spec1    form      1
LR       DIM       6
.
.
         MOVE      "Nadj<TR></TR>0095" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "Fix missing records" TO STITLE
         move      c1 to nordpath
         move      c1 to ninvpath
         CALL      PAINT
         OPEN      ADJPRT,"NINPADJ",EXCLUSIVE
.         OPEN      TDMCORD,"e:\data\diskin3.brk"
.
READ
MainLoop
         read     ADJPRT,seq;jstvars,typinit
         goto      eoj if over
         ADD       ONE TO READCNT
         DISPLAY   *P10:20,"NUMBER OF ORDERS READ : ",READCNT
         PACK      NJSTFLD FROM JSTINVNO,JSTSUBNO
         REP       " 0" IN NJSTFLD
         call       NJsttst
         if         over
         CALL      NJSTWRT
         ADD       ONE TO writcnt
          endif
         DISPLAY   *P10:22,"NUMBER OF orders updated  : ",writcnt
         GOTO      READ
.
eoj         CLOSE     ADJPRT
         shutdown   "cls"
         INCLUDE   COMLOGIC.inc
         include   ninvio.inc
        include    nordio.inc
         INCLUDE   NJSTIO.INC
                              include   nadjio.inc

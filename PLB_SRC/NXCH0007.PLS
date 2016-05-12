PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NXRFDD.inc
;patch1.2
			include	compdd.inc
			include	cntdd.inc
.         INCLUDE   NMLRDD.inc
;patch1.2
         INCLUDE   NDATDD.inc
RELEASE  INIT      "1.2"        ASH 19MAY2005 LISTMLR Conversion
.RELEASE  INIT      "1.1"        ASH 03OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "TEST"        DLH 12MAR92
.
OUTPUT   FILE
.
         MOVE      "NXCH0007" TO PROGRAM
         MOVE      "CHECK LISTMLR" TO STITLE
         CALL      PAINT
         IFNZ      PC
         PREP      OUTPUT,"WHATISIT:PRINT"
         XIF
         IFZ      PC
.START PATCH 1.1 REPLACED LOGIC
.         PREP      OUTPUT,"g:\DATA\WHATISIT"
         PACK      STR35,NTWKPATH1,"WHATISIT"
         PREP      OUTPUT,STR35
.END PATCH 1.1 REPLACED LOGIC
         XIF
         MOVE      C1 TO NDATPATH
         MOVE      C1 TO NXRFPATH
.
LOOP     CALL      NXRFSEQ
         GOTO      DONE IF OVER
         ADD       C1 TO N5
         DISPLAY   *P10:15,"RECORDS READ ",N5
.
.START PATCH 1.2 REPLACED LOGIC
.         PACK      MKEY FROM NXRFMLR,Z3
.         CALL      NMLRKEY
         PACK      COMPFLD,NXRFMLR
         CALL      COMPKEY
.END PATCH 1.2 REPLACED LOGIC
.
         MOVE      NXRFLIST   TO NDATFLD
         CALL      NDATKEY
.START PATCH 1.2 REPLACED LOGIC
.         WRITE     OUTPUT,SEQ;OLSTNAME," ",MCOMP
         WRITE     OUTPUT,SEQ;OLSTNAME," ",COMPCOMP
.END PATCH 1.2 REPLACED LOGIC
         ADD       C1 TO N6
         DISPLAY   *P15:16,"RECORDS WRITTEN ",N6
         GOTO      LOOP
.
DONE     WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
         STOP
;patch1.2
			include	compio.inc
			include	cntio.inc
.         INCLUDE   NMLRIO.inc
;Patch1.2
         INCLUDE   NDATIO.inc
         INCLUDE   NXRFIO.inc
         INCLUDE   COMLOGIC.inc

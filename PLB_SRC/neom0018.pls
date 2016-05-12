.NEOM0018 - FIND OUTSTANDING PREPAYS
.checks to see if open & we have cut a check to List owner.
.Does not check to see if LO paid in full, so will include disputes.
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
;begin patch 1.3
;         INCLUDE   NINVDD.inc
         	INCLUDE   	ninvdd.inc
;end patch 1.3
.
. 
. 
. 
release  init      "1.3"      DLH 02March2005 Invoice COnversion
;release  init      "1.2"      ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.1"      DLH 27APR99  NININV Y2K
.ELEASE  INIT      "1.00"     DLH 30MAR94 NEW.
. 
.begin patch 1.1
.invREP   FILE       FIX=294 OUTPUT FILE FOR OUTSTANDING PREPAYS
INVREP   FILE       FIX=400 OUTPUT FILE FOR OUTSTANDING PREPAYS
.end patch 1.1
FILEIN   DIM       8
KEY      DIM       6
COUNT    FORM      5
out      form      5
SEL      DIM       1
BRANCH   FORM      1
. 
. 
*PROGRAM MAIN************MAIN
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NEOM0018" TO PROGRAM
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "NININV" TO INPNAME
         ENDIF
         MOVE      "PREPARE FOR PREPAY STATEMENT" TO STITLE
         TRAP      DUPFILE IF IO
         MOVE      C2 TO NINVPATH     .SET ACCESS TO invoice ISI.
         DISPLAY   *p1:06,"Input File: ",*P15:06,INPNAME
         CALL      PAINT
         move      "ABORT" to pf5
         trap      abort if f5
         call      funcdisp
.START PATCH 1.2 REPLACED LOGIC
.         PREP      INVREP,"g:\data\INVREP02",create
         PACK      STR35,NTWKPATH1,"INVREP02"
         PREP      INVREP,STR35,create
.END PATCH 1.2 REPLACED LOGIC
         DISPLAY   *P1:13,"RECORDS READ :"
INPGET   TRAP      INPNG GIVING ERROR IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         close     testfile
         DISPLAY   *p1:06,"Input File: ",*P15:06,INPNAME
         DISPLAY   *p1:07,"Output File: ",*P15:07,"INVREP02"
         GOTO      select
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:   
                   *P15:06,INPNAME
         GOTO      INPGET
.
         DISPLAY   *P1:13,"RECORDS READ :"
SELECT   move      "241529" to ninvfld     .last invoice of 1992.
         call       ninvkey
.         
READ1    CALL      NINVks
         GOTO      EOJ IF OVER
         ADD       c1 TO COUNT
         DISPLAY   *P16:13,COUNT
OPEN     CMATCH    "P" TO STATB
         GOTO      READ1 IF EQUAL
.begin patch 1.1
.         MATCH     B2 TO CHKDTEM
         MATCH     B2 TO CHK1DTEM
.end patch 1.1
         GOTO      READ1 IF EQUAL
. 
WRITE    WRITE     INVREP,SEQ;INVVARS
         add       c1 to out
         display   *p10:15,"Records out ",out
         GOTO      READ1
IO       DISPLAY   *P1:24,*EL,"NO SUCH FILE!!!!!!!!!!",*B,*W3
         GOTO      INpget
abort    display   *p1:24,*el,*hon,"Job ABORTED",*b
EOJ      WEOF      INVREP,SEQ
         CLOSE     INVREP
         STOP
DUPFILE  KEYIN     *P1:23,*EL,"OUTPUT FILE ALREADY EXISTS!!!":
                   *R,*P1:23,*EL,"OVERWRITE ? ",str1
         CMATCH    "Y" TO str1
         STOP      IF NOT EQUAL
.START PATCH 1.2 REPLACED LOGIC
.         OPEN      INVREP,"g:\data\INVREP02",EXCLUSIVE
         PACK      STR35,NTWKPATH1,"INVREP02"
         OPEN      INVREP,STR35,EXCLUSIVE
.END PATCH 1.2 REPLACED LOGIC
         TRAPCLR   IO
         NORETURN
         GOTO      INPget
;begin patch 1.3
;         INCLUDE   NINVIO.inc
         	INCLUDE   	ninvio.inc
;end patch 1.3
         INCLUDE   COMLOGIC.inc


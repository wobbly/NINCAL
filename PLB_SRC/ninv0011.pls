PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 2.2
.begin patch 2.1
;         INCLUDE   NINVDD.inc
               INCLUDE        ninvdd.inc
.end patch 2.1
.end patch 2.1
         INCLUDE   NORD2DD.inc
. 
. 
. 
. 
RELEASE        INIT           "2.2"     DLH 08March2005 Invoice Conversion
;RELEASE  INIT      "2.12"     ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "2.11"     JD  04jun99 added exclusive to prepare.
.RELEASE  INIT      "2.1"     DLH 5May99 NININV Y2K
.RELEASE  INIT      "2.00"     JD  30oct96 added step to automate paid/payable.
.RELEASE  INIT      "1.52"    DLH 05JAn95 INCREASE RECORD SIZE TO MATCH INVOICE.
.RELEASE  INIT      "1.51"    DLH 30MAR94 INCREASE RECORD SIZE TO MATCH INVOICE.
.RELEASE  INIT      "1.5"     DLH 20MAR92
. 
.begin patch 2.1
.INVREP   FILE       FIX=302 OUTPUT FILE FOR PAYABLES REPORT
INVREP   FILE       FIX=300 OUTPUT FILE FOR PAYABLES REPORT
.end patch 2.1
FILEIN   DIM       8
KEY      DIM       6
COUNT    FORM      5
out      form      5
SEL      DIM       1
BRANCH   FORM      1
.START PATCH 2.12 REPLACED LOGIC
.DR       INIT      "g:\data\"         FORCE OUTPUT FILES TO BE BUILD
.END PATCH 2.12 REPLACED LOGIC
TXT      INIT      ".DAT"
F2       DIM       2
F3       FORM      2
WORKNAME DIM       35                                                       ****
. 
. 
*PROGRAM MAIN************MAIN
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "Ninv0011" TO PROGRAM
         MOVE      "Names In The News Ca" TO COMPNME
         ENDIF
         MOVE      "PREPARE FILE FOR PAYABLES" TO STITLE
         TRAP      DUPFILE IF IO
         MOVE      C1 TO NINVPATH     .SET ACCESS TO LR ISI.
         DISPLAY   *p1:06,"Input File: ",*P15:06,INPNAME
         CALL      PAINT
         move      "ABORT" to pf5
         trap      abort if f5
         call      funcdisp
         unpack    inpname into str6,str2
.         bump      inpname by 7
         move      str2 to f2
.         reset     inpname
INVREP
         CLEAR     WORKNAME
.START PATCH 2.12 REPLACED LOGIC
.         APPEND    dr TO WORKNAME
         APPEND    NTWKPATH1 TO WORKNAME
.END PATCH 2.12 REPLACED LOGIC
         APPEND    "invrep" TO WORKNAME
FILELOOP REP       ZFILL,F2
         APPEND    F2,WORKNAME
         APPEND    txt,WORKNAME
.         RESET     WORKNAME to 8
         reset     workname
         TRAP      FILEOK GIVING ERROR IF IO
         OPEN      invrep,WORKNAME
         CLOSE     invrep
ADDFILE  MOVE      F2 TO F3
         ADD       C1 TO F3
         MOVE      F3 TO F2
         CLEAR     WORKNAME
.START PATCH 2.12 REPLACED LOGIC
.         APPEND    dr TO WORKNAME
         APPEND    NTWKPATH1 TO WORKNAME
.END PATCH 2.12 REPLACED LOGIC
         APPEND    "INVREP" TO WORKNAME
         GOTO      FILELOOP
FILEOK   TRAPCLR   IO
         NORETURN
.         TRAP      IO GIVING ERROR IF IO
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
         RESET     ERROR
         SCAN      "I * Y" IN ERROR
         GOTO      ADDFILE IF EQUAL
         RESET     ERROR
         MOVE      B1 TO ERROR
         DISPLAY   *P1:24,*EL,"BUILDING WORK FILE :",WORKNAME,*W;
         PREPARE   invrep,WORKNAME,exclusive
         TRAPCLR   IO
.         PREP      INVREP,"g:\data\INVREP01",create
         DISPLAY   *P1:13,"RECORDS READ :"
INPGET   TRAP      INPNG GIVING ERROR IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         close     testfile
         DISPLAY   *p1:06,"Input File: ",*P15:06,INPNAME
         MOVE      INPNAME TO NORDNAME
         GOTO      select
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:   
                   *P15:06,INPNAME
         GOTO      INPGET
.
         DISPLAY   *P1:13,"RECORDS READ :"
SELECT 
         clear     sel
         move      func to sel
         rep       "1P2O3B" in sel
         display   *P10:14,*EL,"Select (P)aid or (O)pen invoices, ":
                   "or (B)oth ? ",SEL
         REP       "P1O2B3" IN SEL
         MOVE      SEL TO BRANCH
         branch    branch of sel1,sel2,sel3
sel1     display   *p1:14,*El,"Report Type: PAID"
         goto      selx         
sel2     display   *p1:14,*el,"Report Type: OPEN"
         goto      selx         
sel3     display   *p1:14,*el,"Report Type: BOTH"
         goto      selx         
selx     BRANCH    BRANCH OF READ1,READ1,READ1
         GOTO      SELECT
. 
         
READ1    CALL      NORD2SEQ
         GOTO      EOJ IF OVER
         ADD       c1 TO COUNT
         DISPLAY   *P16:13,COUNT
         MOVE      OLRN TO NINVFLD
         CMATCH    "B" TO OSTAT
         GOTO      READINV IF EQUAL
         CMATCH    "Q" TO OSTAT
         GOTO      READINV IF EQUAL
         GOTO      READ1              *NOT BILLED GET NEXT REC.
READINV  CALL      NINVKEY
         GOTO      READ1 IF OVER
         BRANCH    BRANCH OF PAID,open,WRITE
OPEN     CMATCH    "P" TO STATB
         GOTO      WRITE IF NOT EQUAL
         GOTO      READ1
PAID     CMATCH    "P" TO STATB
         GOTO      WRITE IF EQUAL
         GOTO      READ1
. 
WRITE
.begin patch 2.1
.  WRITE     INVREP,SEQ;INVVARS
         WRITE     INVREP,SEQeof;INVVARs
.end patch 2.1
         add       c1 to out
         display   *p10:15,"Records out ",out
         GOTO      READ1
IO       DISPLAY   *P1:24,*EL,"NO SUCH FILE!!!!!!!!!!",*B,*W3
         GOTO      INpget
abort    display   *p1:24,*el,*hon,"Job ABORTED",*b
EOJ      WEOF      INVREP,SEQ
         CLOSE     INVREP
         shutdown  "cls"
         STOP
DUPFILE  KEYIN     *P1:23,*EL,"OUTPUT FILE ALREADY EXISTS!!!":
                   *R,*P1:23,*EL,"OVERWRITE ? ",str1
         CMATCH    "Y" TO str1
.         STOP      IF NOT EQUAL
         if        not equal
         shutdown  "cls"
         endif
.START PATCH 2.12 REPLACED LOGIC
.         OPEN      INVREP,"g:\data\INVREP01",EXCLUSIVE
         pack      str35,NTWKPATH1,"INVREP01"
         OPEN      INVREP,STR35,EXCLUSIVE
.END PATCH 2.12 REPLACED LOGIC
         TRAPCLR   IO
         NORETURN
         GOTO      INPget
;*---------------------------------------------------------------------------------------------------------------------------
;begin patch 2.2
;INCLUDE   NINVIO.inc
               Include        ninvio.inc
;end patch 2.2
         INCLUDE   NORD2IO.inc
         INCLUDE   COMLOGIC.inc


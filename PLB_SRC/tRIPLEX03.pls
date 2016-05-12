...............................................................................
.triplex03- ...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   consacct.inc
         include   hp.inc
         include   njstdd.inc
.         include   nmrgdd.inc
.         include   ninvdd.inc
         INCLUDE   tINVDD.inc
         include   ndatdd.inc
         include   ndat3dd.inc
.START PATCH 1.3 REPLACED LOGIC
.         include   nmlrdd.inc
         include   compdd.inc
         include   cntdd.inc
.END PATCH 1.3 REPLACED LOGIC
.         include   nshpdd.inc
         INCLUDE   NORDDD.INC
         include   nowndd.inc
.
RELEASE  INIT      "1.3"            ASH 26MAY2004 MAILER CONVERSION
.RELEASE  INIT      "1.2"            ASH 17JAN99 DUPLICATE VARS NOW FOUND IN CONS.INC
.RELEASE  INIT      "1.1"            ASH 17JAN99 DUPLICATE VARS NOW FOUND IN CONS.INC
.release  init      "1.0"         DLH 22Aug97 Created to study triplex
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
.FILES.
...............................................................................
.
.
.
.
.
.
. WORK VARIABLES
.
.
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.
form102  form      10.2
form122  form      12.2
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
MO       DIM       2
DY       DIM       2
YR       DIM       2
LOCAL    INIT      "LOCAL"
PRTFLAG  DIM       1
ANS      DIM       1
TYPIST   DIM       2
.
TOTARp   FORM      9.2       *prepaid
TOTpMOA  FORM      9.2       *MOA Applied to prepaid
TOTAR    FORM      9.2
TOTAP1   FORM      9.2
TOTAP2   FORM      9.2
TOTAP    FORM      9.2
TOTNIN   FORM      9.2
TOTLR    FORM      9.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
.
ZERO     FORM      "0"
.
FORM2    FORM      2
FORM22   FORM      2.2
FORM7    FORM      7
FORM52   FORM      5.2
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      7
counto   form      6      records output
CO       FORM      1
. 
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
.START PATCH 1.1 - DUPLICATE VARS
.MASK32   INIT      "ZZZ.ZZ-"
.END PATCH 1.1 - DUPLICATE VARS
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.START PATCH 1.1 - DUPLICATE VARS
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.END PATCH 1.1 - DUPLICATE VARS
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
M$ARp    DIM       13     *prepaid 
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       13
M$AP2    DIM       13
M$STAX   DIM       8
M$CTAX   DIM       8
M$POST   DIM       6
M$LRINC  DIM       13
M$NINC   DIM       13
M$GROSS  DIM       13
.
MT$AR    DIM       15
MT$ARp   DIM       15     *prepaid 
MT$pMOA  DIM       15     *prepaid 
MT$AP1   DIM       15
MT$AP2   DIM       15
MT$STAX  DIM       15
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       15
MT$NINC  DIM       15
.
NEW      FORM      5
REPRINT  FORM      5
PAGE     FORM      4
LINES    FORM      2
output   file
.
. **********CHANGE THE FOLLOWING INV# NUMBER IN JAN*********
. 000000 FIRST INV# OF 1981
. 001000 FIRST INV# OF 1982
. 015124 FIRST INV# OF 1983
. 032197 FIRST INV# OF 1984
. 052364 FIRST INV# OF 1985
. 075180 FIRST INV# OF 1986
. 098419 FIRST INV# OF 1987
. 121541 FIRST INV# OF 1988
. 145392 FIRST INV# OF 1989
. 172279 FIRST INV# OF 1990
. 197750 FIRST INV# OF 1991
. 218003 FIRST INV# OF 1992
. 241530 FIRST INV# OF 1993
. 262880 FIRST INV# OF 1994
. 282911 FIRST INV# OF 1995
. 302647 FIRST INV# OF 1996
. 321449 FIRST INV# OF 1997
. 341381 FIRST INV# OF 1998
FIRSTN   INIT      "341381"  FIRST NIN INV# OF 1998
.
         CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         MOVE      DATEMASK TO TODAY
         XIF
         IFZ       PC
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         XIF
           MATCH     "triplex03" TO PROGRAM         .ENTRY FROM DSINIT?
           IF          NOT EQUAL                     . NO.  
         MOVE      "Triplex03" TO PROGRAM           .SET DEFAULTS
         MOVE      "Names In The News Ca" TO COMPNME
           move      "Commreg" to prtname
           ENDIF
         MOVE      "triplex stuff " TO STITLE
         CALL      PAINT
         move      c1 to nordpath
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File   : ":
                   *P01:07,"Print File   : ":
                   *P01:08,"Input Count  : ":
                   *P01:09,"Today's Date : ",*p15:09,today:
                   *p01:10,"Output Count : "
         MOVE      C3 TO NORDLOCK
         MOVE      C3 TO NDATLOCK
         MOVE      C3 TO NMLRLOCK
.
.START PATCH 1.2 REPLACED LOGIC
.          prepare  output,"g:\data\tdmcrega.dat"
          PACK     STR35,NTWKPATH1,"TDMCREGA.DAT"
          prepare  output,STR35
.END PATCH 1.2 REPLACED LOGIC
INPGET   
.         TRAP      FILENG IF IO
.         OPEN      TESTFILE,INPNAME
.         TRAPCLR   IO
         DISPLAY   *P15:06,"nadjust"
.         CLOSE     TESTFILE
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      INPUT IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
.         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      INPUT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
         MOVE      ZERO TO PAGE
.         move      c2 to ninvpath
.         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c3 to nmlrlock
         move      c3 to ndatlock
         move      c1 to ndatpath
.
INPUT    
.         move      c2 to ninvpath
.         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c1 to ndatpath
.         call      ninvkey
.         goto      testinv

          
.
READINV  call      njstseq
         goto      eoj if over
        
testinv  
         ADD       c1 TO COUNT
         DISPLAY   *P15:08,COUNT
         unpack     jstdate into mm,dd,yy
.         match      "97" to yy
.         goto       testinv1 if equal
         match      "98" to yy
         goto       readinv if not equal
testinv1
.         move      lrn to tinvfld
.         clear     tinvdate
.         clear     tinvinv
.         clear     tinvdolr
.         call      tinvkey
         move      jstlr to nordfld
         call      nordkey
.         move      lrn to nmrgfld
.         call      nmrgkey

         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY

         MOVE      OLON TO NOWNFLD
         REP         ZFILL IN NOWNFLD
         call      nownkey       
         pack      mkey from omlrnum,z3

         call      nmlrkey

.         call      compute
        
PROCESS  
.
.
         clear     str2
         pack     str2 from osales10,osales
         move     str2 to n2
         compare  c6 to n2
         if       equal
         move     "M" to str1
         Else
         Move      "B" to str1
         endif
         clear     str8
         unpack     jstdate into mm,dd,yy
         pack      str8 from mm,dd,cc,yy
         move     c0 to form102
         move     c0 to form122
         move     c0 to formar
         move     c0 to lrinc
         move     c0 to saveap
         move     c0 to formap2
.         move     form122 to form102
         move     c0 to cvtfld
         move     jstar to cvtfld
         call     cvt
         move     c0 to form122
         move     c0 to formar
         move     cvtfld to form122
         mult     ".01" by form122
         add     form122 to formar

         move     c0 to cvtfld
         move     jstap1 to cvtfld
         call     cvt
         move     c0 to form122
         move     c0 to saveap
         move     cvtfld to form122
         mult     ".01" by form122
         add     form122 to saveap

         move     c0 to cvtfld
         move     jstap2 to cvtfld
         call     cvt
         move     c0 to form122
         move     cvtfld to form122
         move     c0 to formap2
         mult     ".01" by form122
         add     form122 to formap2

         move     c0 to cvtfld
         move     jstlrinc to cvtfld
         call     cvt
         move     c0 to form122
         move     c0 to lrinc
         move     cvtfld to form122
         mult     ".01" by form122
         add     form122 to lrinc

.         move     c0 to gross
         write     output,seq;*CDFON,jstlr,mcomp,o1des,"00",formar,jstreasn,saveap,formap2,lrinc:
                   str8,str1:
                   tinvdate,tinvinv,form102
         ADD       c1 TO COUNTo
         DISPLAY   *P15:10,COUNTo
         GOTO      readinv
*......................................................................
.
CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
         RESET     MPCHARS
         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
         GOTO      CVTMP IF EQUAL                YES.
         RESET     CVTFLD                        NO.
         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
         RETURN    IF EQUAL                      ITS OK.
FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",jstLR
         RETURN                                POP THE STACK.
CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
         RESET     CVTFLD
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
*.........................
*......................................................................
.
TOTAL    GOTO      EOJ
.............................................................................
.
.
EOJ      
         weof       output,seq
         close      output
         shutdown  "cls"
         STOP
.         include   ninvio.inc
         include   nordio.inc
         include   ndatio.inc
         include   njstio.inc 
        include   nownio.inc
         include   ndat3io.inc
.START PATCH 1.3 REPLACED LOGIC
.         include   nmlrio.inc
         include   compIO.inc
         include   cntIO.inc
.END PATCH 1.3 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc



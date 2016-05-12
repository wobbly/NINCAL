...............................................................................
.Triplex01 - ...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   consacct.inc
         include   hp.inc
         include   nmrgdd.inc
;begin patch 1.4         
;         include   ninvdd.inc
	Include	ninvdd.inc
	Include	NInvacddd.inc
;end patch 1.4         
         INCLUDE   tINVDD.inc
         include   ndatdd.inc
         include   ndat3dd.inc
.START PATCH 1.3 REPLACED LOGIC
;.         include   nmlrdd.inc
         include   compdd.inc
         include   cntdd.inc
.END PATCH 1.3 REPLACED LOGIC
         include   nshpdd.inc
         INCLUDE   NORDDD.INC
         include   nacddd.inc
         include   nowndd.inc
.
RELEASE  	INIT      	"1.4"            DLH 10March2005 Invoice CONVERSION
;RELEASE  INIT      "1.3"            ASH 26MAY2004 MAILER CONVERSION
.RELEASE  INIT      "1.2"            ASH 04OCT2000 NEW SERVER ADDED
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
mrgsw    dim       1
SHIPSW   DIM       1
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
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      7
counto   form      6      records output
CO       FORM      1
tipe     dim       1
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
. 321449 FIRST INV# OF 1997
. 341381 FIRST INV# OF 1998
. 362337 FIRST INV# OF 1999
FIRSTN   INIT      "362337"  FIRST NIN INV# OF 1999
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
           MATCH     "TRIPLEX01" TO PROGRAM         .ENTRY FROM DSINIT?
           IF          NOT EQUAL                     . NO.  
         MOVE      "TRIPLEX01" TO PROGRAM           .SET DEFAULTS
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
         MOVE      C3 TO NMLRLOCK
         MOVE      C3 TO NDATLOCK
.
.START PATCH 1.2 REPLACED LOGIC
.          prepare  output,"g:\data\tdmcreg.dat"
          PACK     STR35,NTWKPATH1,"TDMCREG.DAT"
          prepare  output,STR35
.END PATCH 1.2 REPLACED LOGIC
INPGET   
.         TRAP      FILENG IF IO
.         OPEN      TESTFILE,INPNAME
.         TRAPCLR   IO
         DISPLAY   *P15:06,"NININV"
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
         move      c2 to ninvpath
         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c1 to ndatpath
.
INPUT    
         move      c2 to ninvpath
         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c1 to ndatpath
         call      ninvkey
         goto      testinv

          
.
ReADINV  call      ninvks
         goto      eoj if over
testinv  
         ADD       c1 TO COUNT
         DISPLAY   *P15:08,COUNT
         move      lrn to tinvfld
         rep       zfill in tinvfld
         clear     tinvdate
         clear     tinvinv
         clear     tinvdolr
         call      tinvkey
         goto      readinv if over
         move      lrn to nordfld
         call      nordkey
         move      b1 to tipe
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      RENTAL IF not EQUAL
         MOVE      "E" TO TIPE
         GOTO      NEXT
RENTAL   MOVE      "R" TO TIPE

next     REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
.begin patch
         move      no to mrgsw
         move      no to shipsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
.end patch

         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY

         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         call      nownkey       
         pack      mkey from omlrnum,z3

         call      nmlrkey
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         call      compute
        
PROCESS  
.
.
         clear     str2
         move     c0 to n2
         pack     str2 from osales10,osales
         move     str2 to n2
         compare  c6 to n2
         if       equal
         move     "M" to str1
         Else
         Move      "B" to str1
         endif
         clear     str8
         clear     str10
         unpack    tinvdate into str2,yy,mm,dd
         pack      str10 from mm,slash,dd,slash,str2,yy
         move     c0 to form122
         move     tinvdolr to form122
         mult     ".01" by form122
         move     c0 to form102
         move     form122 to form102
         write     output,seq;*CDFON,lrn,mcomp,o1des,gross,formar,commpct,saveap,formap2,lrinc:
                   str1,tipe:
                   tinvdate,tinvinv,form102
         ADD       c1 TO COUNTo
         DISPLAY   *P15:10,COUNTo
         GOTO      readinv
*......................................................................
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
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
;begin patch 1.4         
;         include   ninvio.inc
;         include   compute.inc
	Include	ninvio.inc
	Include	NinvAcdio.inc
	INclude	compute.inc
;         include   compute.inc
;end patch 1.4         
         include   nordio.inc
         include   nmrgio.inc
          include  tinvio.inc
         include   nshpio.inc
         include   ndatio.inc
         include   nownio.inc
         include   ndat3io.inc
.START PATCH 1.3 REPLACED LOGIC
.         include   nmlrio.inc
         include   compio.inc
         include   cntio.inc
.END PATCH 1.3 REPLACED LOGIC
         include   nacdio.inc
         INCLUDE   COMLOGIC.inc



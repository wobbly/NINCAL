...............................................................................
.Nontriplex- ...............................................................................
.
:Quick and dirty to calc lost revenue from lists that moved away from triplex.

PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   consacct.inc
         include   hp.inc
         include   njstdd.inc
         include   nmrgdd.inc
         include   ninvdd.inc
         INCLUDE   tINVDD.inc
         include   ndatdd.inc
         include   ndat3dd.inc
         include   compdd.inc
         include   cntdd.inc
         INCLUDE   NORDDD.INC
         include   nowndd.inc
.
release  init      "1.0"         DLH 19Oct04 
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
ListList       Init           "016130-019539-020429-018919-003252-009923-016535-019072-003831-000995-020824-014706-011507-015102-011702-016169-020711-021691-006019-019610-010540-":
                              "020474-020693-018228-001569-021500-16464-015849-021415-021485-21285-017633-015003-016724-019336-005122-010875-019702-021228-009222-021431-007823-011065-":
                              "012781-010632-001106-018847-021611-020053-021423-010540-020636-014276-020565-020310"
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
COUNT    FORM      7
counto   form      6      records output
CO       FORM      1
. 
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
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
. 362337 FIRST INV# OF 1999
. 385477 FIRST INV# OF 2000
. 385477 FIRST INV# OF 2001
;FIRSTN   INIT      "433150"  FIRST NIN INV# OF 2002
FIRSTN   INIT      "455522"  FIRST NIN INV# OF 2003
StateFlag      Init           "N"
GEnderFlag     Init           "N"
ZipFlag        Init           "N"
ScfFlag        Init           "N"
FTPFlag        Init           "N"
DolrFlag        Init           "N"
PSFlag         Init           "N"
TapeFlag        Init           "N"
CalcNIN        Form           7.4
MissNIN        FOrm           7.2
AddCount       FOrm           2
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
           MATCH     "nontriplex" TO PROGRAM         .ENTRY FROM DSINIT?
           IF          NOT EQUAL                     . NO.  
         MOVE      "NonTriplex" TO PROGRAM           .SET DEFAULTS
         MOVE      "Names In The News" TO COMPNME
           move      "Commreg" to prtname
           ENDIF
         MOVE      "Non Triplex stuff " TO STITLE
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
          PACK     STR35,NTWKPATH1,"NonTDMCREG.DAT"
          Pack     Str35 from  "c:\work\NonTDMCREG.DAT"
          prepare  output,STR35,exclusive
INPGET   
         DISPLAY   *P15:06,Inpname
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
         DISPLAY   *P15:07,PRTNAME
         GOTO      INPUT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
         MOVE      ZERO TO PAGE
         move      c2 to ninvpath
         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c3 to nmlrlock
         move      c3 to ndatlock
         move      c1 to ndatpath
.
INPUT    
         move      c2 to ninvpath
         move      firstn to ninvfld
         move      c1 to nmlrpath
         move      c1 to ndatpath
         call      ninvtst
               Call           NinvKS
         goto      testinv

          
.
READINV        call           nINVKS
         goto      eoj if over
        
testinv  
         ADD       c1 TO COUNT
               DISPLAY         *P15:08,COUNT,b1,LRN,b1,invnum,b1,Invdtey
;               IF             (INVDTEY = "04" or Invdtey = "03" or Invdtey = "02")              
               IF             (INVDTEY = "04")              
               goto           Testinv1
               Else
               goto           readinv
               endif
testinv1
               Packkey        Nordfld from LRN
               Call           Nordkey
               reset          ListList
               scan           Olnum in ListList
               goto           Readinv if not equal
;               Match          "003252" to OLnum
;               goto           Readinv if not equal
;               move           lrn to nmrgfld
;               call           nmrgkey

         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY

;         MOVE      OLON TO NOWNFLD
;         REP         ZFILL IN NOWNFLD
;         call      nownkey       
;         pack      mkey from omlrnum,z3

;         call      nmlrkey
;Check for selects

;         call      compute
        
PROCESS  
.              
               Move           c0 to Missnin
               Move           No to stateflag
               move           No to scfflag
               move           no to GenderFLag
               Move           No to ZipFlag
               Move           No to GenderFlag
               Move           No to FtpFlag
               Move           No to TapeFlag
               Move           No to DolrFlag
               For            AddCount,"1" to "10"

               load           str15 from addcount of ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5,ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10

               UNpack         str15,str3,str7
               if             ((Str3 = "016" or Str3 = "030" or str3 = "034") & DolrFlag = "N") ;Rec/Dollar per m
;               call           Debug
               move           str7 to cmpt92
               DIVIDE         HUND BY CMPT92      
               move           c0 to calcNIN
                              If             (cmpt92 = "5")
                              move           "1.0" to calcNIN
                              Elseif         (cmpt92 = "7.5")
                              move           "1.5" to calcNIN
                              Elseif         (cmpt92 = "10")
                              move           "2.0" to calcNIN
                              Elseif         (cmpt92 = "20")
                              move           "4.0" to calcNIN
                              endif
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to Dolrflag
               endif

               UNpack         str15,str3
               if             (Str3 = "042" & PSFlag = "N") ;PS per m
               move           c0 to calcNIN
               move           "3.0" to calcNIN
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to PSflag
               endif

               UNpack         str15,str3
               if             ((Str3 = "089" or Str3 = "089") & TapeFlag = "N") ;Tape per m
               move           c0 to calcNIN
               move           "15.0" to calcNIN
               add            Calcnin to MissNIN
               MOve           yes to Tapeflag
               endif

               UNpack         str15,str3
               if             ((Str3 = "101" or Str3 = "111") & FtpFlag = "N") ;Tape per m
               move           c0 to calcNIN
               move           "20.0" to calcNIN
               add            Calcnin to MissNIN
               MOve           yes to Ftpflag
               endif

               UNpack         str15,str3
               if             (Str3 = "020" & StateFlag = "N") ;state per m
               move           c0 to calcNIN
               move           "3.5" to calcNIN
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to Stateflag
               endif

               UNpack         str15,str3
               if             (Str3 = "013" & SCFFlag = "N") 
               move           c0 to calcNIN
               move           "2.5" to calcNIN
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to SCFflag
               endif

               UNpack         str15,str3
               if             (Str3 = "015" & GenderFlag = "N") 
               move           c0 to calcNIN
               move           "2.5" to calcNIN
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to Genderflag
               endif

               UNpack         str15,str3
               if             (Str3 = "018" & ZipFlag = "N") 
               move           c0 to calcNIN
               move           "1.00" to calcNIN
               Mult           Qtyin by calcnin
               Mult           ".001",Calcnin
               add            Calcnin to MissNIN
               MOve           yes to Zipflag
               endif
               Repeat
;
         write     output,seq;*CDFON,Olrn,mcomp,Olnum,o1des,"00",Qtyin,MissNin

         ADD       c1 TO COUNTo
         DISPLAY   *P15:10,COUNTo
         GOTO      readinv
*......................................................................
.
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
         include   ninvio.inc
         include   nordio.inc
         include   nMrgio.inc
         include   ndatio.inc
         include   njstio.inc 
         include   nownio.inc
         include   ndat3io.inc
         include   compIO.inc
         include   cntIO.inc
         INCLUDE   COMLOGIC.inc



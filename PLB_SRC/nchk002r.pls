. ...............................................................................
.
PC       EQU       0
.
          INC       COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   CONSACCT.inc
          INC       NCSHDD.inc
          INC       ninvdd.inc
          Include   NInvAcddd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
          INC       NBILDD.inc
          INC       NORDDD.inc
          INC       NOWNDD.inc
          INC       NPAYDD.inc
          INC       NADJDD.inc
          inc       njstdd.inc
          INCLUDE   NACDDD.inc
          INCLUDE   NDATDD.INC
          include   nshpdd.inc
          INC       NMTXDD.inc
          INCLUDE   NDAT3DD.INC
          INCLUDE   NESCDD.INC
          include   nmrgdd.inc
          INCLUDE   NRTNDD.inc
          INCLUDE   NSEL2DD.INC
          inc       hp.inc

.
RELEASE   INIT     "1.00"             DLH Print signature on missprinted checks
Reldate   Init      "05 November 2012"
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       6
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
chk1date dim       8
.FILES.
...............................................................................
.
DATEFILE FILE                             
.*DATE ENTERED FOR CHECK CONTROL.
CHKMST   FILE      FIXED=78
.
. WORK VARIABLES
.
.
.
.
.
shipsw   dim        1
mrgsw    dim        1
XQTY     FORM      9
NUM      FORM      1
CENTS    FORM      2
AMT      DIM       17
FORM7    FORM      7
COUNT    FORM      5
COUNTP   FORM      5
.
.CHKMST FILE VARIABLES
CHECK    DIM       6
CHKCODE  DIM       1
FILL71   DIM       71
.
.
.PRINT MASK VARIABLES
.
MASK92b   INIT      "$,***,***,***.99-"
MASK72   INIT      "$***,***.99-"
M$AP1    DIM       17
M$AP2    DIM       17
M$AP3    DIM       17
CHKNUM   FORM      6
STAT501  DIM       6
TAX501   FORM      1
APAMT    FORM      9
REFERENC DIM       12
SPACES   INIT      "           "
REF      INIT      "REFERENCE:##"
APCHECK  FORM      "000000001"
AP3SW    DIM       1
Intraflag Dim       1                      .set to Y if ap3 or xninc > 0 and not Cextcd "I"
.                                      adds AP1 to check to PL or NIN for inter comp transfer
AP3AMT   FORM      10.2
AP2SW    DIM       1
ADJSW    DIM       1 
APSW     DIM       1
DIM1     DIM       1
TIPE     DIM       8
STAT     INIT      "P"
NOCHK    INIT      "NO CHK"
ESCROW   INIT      "ESCROW"
ESCFLAG  DIM       1
LOWN     FORM      4
REP      DIM       1
febdat  form      5
feb      dim       1 
mrgy     dim       1
prtlines form      2
RTNTAB   FORM      3
PERCENT  FORM      4.2
CALCmPER  FORM      7.4
TOTNCOA  FORM      8
TOTBILL  FORM      8
invqty1  form      7
invqty2  form      7
Num26    form      "26"
PayToSW   dim       1
ap2save  form      10.2
ap1save  form      10.2
ap3save  form      10.2                           .*LR                                                                                      
Xnincsave  form      10.2     .*NIN
ap1amt   form      10.2
qtybild2 form      9
.
mrgnetsv form      8
CHKNflag  Dim       1         .flag for check ## file
         MOVE      "NCHK002R" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      C1 TO NINVPATH
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NPAYPATH
         MOVE      C1 TO NMLRPATH
         MOVE      C1 TO NCSHPATH
         move      c2 to nescpath
         move      c1 to ncshflag
         MOVE       "02" TO MM
         MOVE       "09" TO DD
         MOVE       "96" TO YY
         CALL      CVTJUL
         move      juldays to febdat
         MOVE      "Signature PRINT PROGRAM" TO STITLE
         CALL      PAINT
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         UNPACK    DATE INTO SYSMO,SYSDY,SYSYR
         UNPACK    DATE INTO MM,DD,YY
OK      
         splopen        "\\NINs2\Laser8","R"
.
INPUT     
          Keyin     *p10:12,"number of copies needed",str3
          move      str3,n3
          loop
          until     (n3 = count)
          call      Detail
          ADD       C1 TO COUNT
          DISPLAY   *P10:12,"NUMBER OF CHECK ENTRIES PROCESSED: ",COUNT
          repeat
          shutdown  "cls"
DETAIL
          PRINT     *F
          print      "!R!CALLSA,4.50,9.30; EXIT;"
          RETURN

EOJ      
          SPLCLOSE
          shutdown  "cls"
          STOP
.
          INCLUDE   NMTXIO.inc
          INCLUDE   NORDIO.inc
          INCLUDE   NADJIO.inc
          include   njstio.inc
          INCLUDE   NPAYIO.inc
          INCLUDE   NOWNIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
          INCLUDE   NCSHIO.inc
          INCLUDE             ninvio.inc
          Include   NInvAcdIO.inc
          INCLUDE             compute.inc
          INCLUDE   NDAT3IO.INC
          INCLUDE   NESCIO.INC
          include   nmrgio.inc
          INCLUDE   NRTNIO.inc
          INCLUDE   NACDIO.INC
          INCLUDE   NDATIO.INC
          include   nshpio.inc
          INCLUDE   NSEL2IO.INC
          INCLUDE   COMLOGIC.inc


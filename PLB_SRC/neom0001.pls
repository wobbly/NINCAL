...............................................................................
.neom0001
. once known as NINP43 - END-OF-MONTH STATEMENT PREP. PROG.
...............................................................................
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INCLUDE   CONSACCT.inc
         INCLUDE   NORDDD.INC
.Patch2.6
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
.patch2.6
         INCLUDE   NOWNDD.INC
         INCLUDE   NDAT3DD.INC
.begin patch 2.1
         INCLUDE   NADJDD.inc
         INCLUDE   NJSTDD.inc
.end patch 2.1
         include   nmrgdd.inc
.Patch2.6
.         include   nbrkdd.inc
.Patch2.6
.begin patch 2.0
         INCLUDE   NSTEDD.inc
         INCLUDE   NSLSDD.inc
.begin patch 2.7
.         INCLUDE   NINVDD.inc
          INCLUDE             ninvdd.inc
          Include   NInvAcddd.inc
.begin patch 2.7
         include   nshpdd.inc
         include   ndatdd.inc
         include   nacddd.inc
         include   nmlddd.inc
mrgsw    dim       1
shipsw   dim       1
.end patch 2.0
Release   Init      "2.75"   DLH Use Data Manager
Reldate   Init      "2014 January 08"
.Release   Init      "2.74"   DLH turn off patch 2.71, handle elsewhere as it screwsup reports not having the number
.Reldate   Init      "01 February 2013"

.Release   INIT      "2.73"    DLH No more breakout seperate files for PL reporting
.Reldate   Init      "03 February 10"
.Release   INIT      "2.72"    DLH breakout seperate files for PL reporting
.Reldate   Init      "07 December 07"
.Release INIT "2.71"                JD 31Mar2005   Moved zero's to obrkcnt field. obildirct.
.Release  INIT      "2.7"     DLH 2005March02 Invoice COnversion
.Release INIT "2.6" DMB 26MAY2004       Mailer COnversion
.RELEASE  INIT      "2.5"         18mar04 writing steadjsw "1) if adjusted/"2) if shortpay.
.RELEASE  INIT      "2.4"        29jan04 Use client salesperson instead of brker sales.
.RELEASE  INIT      "2.3"       30juL03 write out guarcode to statement files.
.RELEASE  INIT      "2.2"      08Mar02    added read of new mail date revision file.
.RELEASE  INIT       "2.13"       JD   09oct00 fixed bug with adding ap adjustment.
.RELEASE  INIT       "2.12"       ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT       "2.11"       JD   10Apr00 use date from dsprog.
.RELEASE  INIT       "2.1"       DLH  23AUg99 NINadj nadjust Y2K
.RELEASE  INIT       "2.0"       DLH  26APR99 NININV Y2K
.RELEASE  INIT       "1.9"       ASH  30DEC98 NINORD Y2K, File expansion; CONSACCT.INC VAR EXPANSION
.RELEASE  INIT       "1.8"        JD  30oct96 added step to automate paid/payable.
.RELEASE  INIT      "1.7"        JD  22mar95 write out if 0 ap adjustment.
.RELEASE  INIT      "1.6"        JD  15mar95 if bil direct do not write brk #
.RELEASE  INIT      "1.5"       DLH 15NOV94 NEW compute.INC, CONSACCT.INC SPLITS.
.RELEASE  INIT      "1.4"        jd read mlr for sales person.
.RELEASE  INIT      "1.3"       DLH 29JUN94 ADDED OWNER & CHECK DATE TO STATEMENTS.
.RELEASE         INIT      "1.2"       DLH 19APR93 ADDED OUTSIDE GUARANTEES.
.RELEASE  INIT      "1.1"       DLH 23MAR92      NORDXX, NADJXX,
.RELEASE  INIT      "1.0"      D. HERRICK  07NOV90
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
TIME     DIM       8
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
.START PATCH #1.9 - INCREASED FILESIZE
.reads INVVARS and this was increased some time ago but this was never updated
.INVREP   FILE      FIXED=294
INVREP   FILE      FIXED=302
.END PATCH #1.9 - INCREASED FILESIZE
.
.
SLSFILE  FILE
.
DUPEOWN  IFILE     KEYLEN=4
..............................................................................
. ISAM KEY VARIABLES
OWNKEY   DIM       4    *DUPE OWNER FILE.
...............................................................................
.
.
.DUPEOWN FILE.
..............
.OWNKEY   DIM       4    1-4
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
.
. WORK VARIABLES
.
.STR3     DIM       3
.
...MINUS OVERPUNCH VARS.
NEWFP    FORM      1
TOMOV    INIT      "0}1J2K3L4M5N6O7P8Q9R"
AMOUNT1  DIM       10
NEWFLD   DIM       10
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
SYSDATE  FORM      5         *HOLDS EOM DATE IN JULIAN
INVJDATE FORM      5       *HOLDS INVOICE DATE IN JULIAN.
ANS      DIM       1
REP      DIM       1
TYPIST   DIM       2
.
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
.
FORM2    FORM      2
FORM22   FORM      2.2
.START PATCH #1.9 - REPLACED VAR
.FORM7    FORM      7
FORM9A    FORM      9
.END PATCH #1.9 - REPLACED VAR
FORM52   FORM      5.2
.START PATCH #1.9 - REPLACED VAR
.FORM8    FORM      8
FORM9B    FORM      9
.END PATCH #1.9 - REPLACED VAR
FORM11   FORM      11
.START PATCH #1.9 - REPLACED VAR
.DIM7     DIM       7
.DIM8     DIM       8
.begin patch 2.0
.DIM9A    DIM       9
.DIM9B    DIM       9
.FORM9    FORM      9
.DIM9     DIM       9
.end patch 2.0
.END PATCH #1.9 - REPLACED VAR
APSW     DIM       1
FORM10   FORM      10
FORM102  FORM      10.2
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
BLANK    INIT      " "
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
CO       FORM      1
. 
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.begin patch 2.0
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.MASK32   INIT      "ZZZ.ZZ-"
.end patch 2.0
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       13
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
MT$AP1    DIM       15
MT$AP2   DIM       15
MT$STAX  DIM       15
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       15
MT$NINC  DIM       15
.
PAGE     FORM      4
LINES    FORM      2
MINUS    INIT      "-"
brker    dim       1
.START PATCH #1.9 - DUPLICATE VAR NOW IN CONSACCT.INC
.form92   form      9.2
.END PATCH #1.9 - DUPLICATE VAR NOW IN CONSACCT.INC
adjap    form      7.2
JSTN     dim      2
.START PATCH 2.12 REPLACED LOGIC
.DR       INIT      "g:\data\"         FORCE OUTPUT FILES TO BE BUILD
.END PATCH 2.12 REPLACED LOGIC
TXT      INIT      ".DAT"
F2       DIM       3
F3       FORM      3
WORKNAME DIM       24                                                       ****
WORKNAM2 DIM       24                                                       ****
BRANCH   FORM      1
dadjsw   dim       1
.
.         DISPLAY   *P1:1,*EF," E-O-M STATEMENT PREP. PROGRAM"
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "Names In The News" TO COMPNME
         MOVE      "NEOM0001" TO PROGRAM
         CLOCK     DATE TO DATE
         endif

         MOVE      "STATEMENT PREPARATION" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
         TRAP      F5EXIT IF F5
         MOVE      C1 TO NORDPATH       .SET ACCESS TO ISI LR#.
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NINVPATH
         bump      inpname by 6
         move      inpname to f2
         reset     inpname
         clear     rep
         move      func to rep
.         move      c3 to rep

         rep       "1R2P3E4C" in rep
MODE     DISPLAY   *P10:12,*EL,"(R)EQUEST)/(P)AID/(E)ND OF MONTH/(C)omb",REP
         CMATCH    "R" TO REP
         GOTO      INVREP IF EQUAL
         CMATCH    "P" TO REP
         GOTO      INVREP IF EQUAL
         CMATCH    "C" TO REP
         GOTO      INVREP IF EQUAL
         CMATCH    "E" TO REP
         GOTO      MODE IF NOT EQUAL
         GOTO      MODE IF EOS
.begin patch 2.75
         TRAP      SHAREINV IF IO
.         goto      shareinv       .DLH 27Mar98
.         DISPLAY   *P1:23,*EL,"OPENING NININV READ ONLY";
         DISPLAY   *P1:23,*EL,"OPENING NININV1";
.         OPEN      NINVFILE,"NININV1",READ
.         OPEN      NINVFILE,"NININV1"
.make sure to open the temporar isi in e:\data   . DH 2014 August 1
         OPEN      NINVFILE,"\\nins1\e\data\NININV1|NINS1:502"
         TRAPCLR   IO
         MOVE      C1 TO NINVFLAG
         GOTO      ORDMST
shareinv 
         OPEN      NINVFILE,"NININV1"
         DISPLAY   *P1:24,*EL,"NININV1 data Manager OPEN FAILED, FILE opened WO DM";
         MOVE      C1 TO NINVFLAG      .SET FLAG TO FILE OPEN.
         TRAPCLR   IO
         GOTO      ORDMST
INVREP
          trap       norep if io
          OPEN      INVREP,inpname,EXCLUSIVE
.ORDMST   OPEN      NINORD,"NINORD",SHARE
ORDMST 
.adjustment file is managed by default patch 2.75
.          TRAP      SHAREADJ IF IO
.         DISPLAY   *P1:23,*EL,"OPENING NINADJ READ ONLY";
.         OPEN      NADJFILE,"NINADJ",share
..         OPEN      NADJFILE,"NINADJ",exclusive
.         DISPLAY   *P1:23,*EL,"NINADJ READ ONLY ok";
.         TRAPCLR   IO
.         GOTO      ORDMST1
.SHAREADJ
.         TRAPCLR   IO
.         OPEN      NADJFILE,"NINADJ",SHARE
.         DISPLAY   *P1:24,*EL,"NINADJ READ ONLY FAILED, FILE SHARED";
.
.ORDMST1  MOVE      C1 TO NADJFLAG            .FILE OPEN
ORDMST1  
.end patch 2.75
         OPEN      DUPEOWN,"DUPEOWN|NINS1:502",READ
         cmatch    "E" to rep
         if        equal
.START PATCH 2.12 REPLACED LOGIC
.         PREPARE   SLSFILE,"g:\data\slstemp.dat"
.         PREPARE   NSTEFILE,"g:\data\stetemp.dat"
         PACK      STR35,NTWKPATH1,"SLSTEMP.DAT"
         PACK      STR45,NTWKPATH1,"STETEMP.DAT"
         PREPARE   SLSFILE,STR35
         PREPARE   NSTEFILE,STR45
         PACK      STR45,NTWKPATH1,"STEPTEMP.DAT"
         PREPARE   NSTEPFILE,STR45
.END PATCH 2.12 REPLACED LOGIC
         goto      DATE
         endif
         CLEAR     WORKNAME
.START PATCH 2.12 REPLACED LOGIC
.         APPEND    dr TO WORKNAME
         APPEND    NTWKPATH1 TO WORKNAME
.END PATCH 2.12 REPLACED LOGIC
         APPEND    "slstmp" TO WORKNAME
         append    f2 to workname
         reset     workname
         CLEAR     WORKNAM2
.START PATCH 2.12 REPLACED LOGIC
.         APPEND    dr TO WORKNAM2
         APPEND    NTWKPATH1 TO WORKNAM2
.END PATCH 2.12 REPLACED LOGIC
         APPEND    "stetmp" TO WORKNAM2
         append    f2 to worknam2
         reset     worknam2
         PREPARE   SLSFILE,workname,Exclusive
         PREPARE   NSTEFILE,worknam2,Exclusive
.START PATCH 2.12 REPLACED LOGIC
         CLEAR     WORKNAM2
         APPEND    "steptmp" TO WORKNAM2
         append    f2 to worknam2
         reset     worknam2
         PREPARE   NSTEPFILE,worknam2,Exclusive
.
.         PACK      STR45,NTWKPATH1,"STEPTEMP.DAT"
.         PREPARE   NSTEPFILE,STR45
.END PATCH 2.12 REPLACED LOGIC
DATE
         UNPACK    today INTO SYSMO,ANS,SYSDY,ANS,SYSYR
         MOVE      today TO DATEMASK
       
.         KEYIN     *P10:12,*EL,"DATE : ",*DV,DATEMASK," OK?",*T20,ANS
         display   *P10:12,*EL,"DATE : ",DATEMASK," OK?",yes
         CMATCH    NO TO ANS
         GOTO      INPUT IF NOT EQUAL
         KEYIN     *P10:12,*EL,"DATE : ",*+,SYSMO,"/",SYSDY,"/",SYSYR,*-
         PACK      DATE FROM SYSMO,DASH,SYSDY,DASH,SYSYR
         GOTO      DATE
.
INPUT
.         CLOCK     TIME TO TIME
         MOVE      SYSMO TO MM
         MOVE      SYSDY TO DD
         MOVE      SYSYR TO YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSDATE
*******************************************************************************
+READINV      -   DATABUS  ISAM READ FOR NININV/TEXT.
*******************************************************************************
.         MOVE      "118939" TO KEY
.         READ      NININV,KEY;;
.                             move      "410000" to ninvfld
.                             call      ninvtst
READINV
         CMATCH    "E" TO REP
         IF        NOT EQUAL
.         .FILEPI    1;INVREP      no reason to pi as we have exclusive
         READ    INVREP,SEQ;INVVARS
         ENDIF
         CMATCH    "E" TO REP
         IF        EQUAL
         CALL      NINVKS
.         CALL      NINVseq
         ENDIF
         GOTO      EOJ IF OVER
         ADD       "1" TO COUNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT
         MOVE      INVDTEM TO MM
         MOVE      INVDTED TO DD
         MOVE      INVDTEY TO YY
         CALL      CVTJUL
         MOVE      JULDAYS TO INVJDATE
         COMPARE   INVJDATE TO SYSDATE      *FOWARD DATED INVOICE?
         GOTO      READINV IF LESS          *YES
.
         CMATCH    "C" TO REP
         IF        EQUAL
         goto      takeit
         endif
         CMATCH    "P" TO REP
         IF        EQUAL
         GOTO      TAKEIT
         ENDIF
         CMATCH    "P" TO STATB
         GOTO      READINV IF EQUAL
.
TAKEIT
.begin patch 2.0
.         REP       ZFILL IN AR
.         REP       ZFILL IN AP1
.         REP       ZFILL IN AP2
.end patch 2.0
.
.START PATCH #1.9 - INCREASED VAR
.         MOVE      C0 TO FORM7
.         MOVE      QTYSHP TO FORM7
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
         MOVE      C0 TO FORM9A
.begin patch 2.0
.         MOVE      QTYshp TO FORM9A
         MOVE      QTYbild TO FORM9A
.         MOVE      PPM TO CMPT92
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
         move      ppm to form32
.end patch 2.0
.END PATCH #1.9 - INCREASED VAR
.
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         type      obrknum
         if        equal
         PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         CALL      NBRKKEY
         move      brcomp,stecname
         move      brcomp,sLSCNAME
         move      yes to brker
         endif
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.
          pack      NMLDFLD1,"01X",LRN
        clear   str8
          pack      str8,"99999999"
          call      NMLDAIM
          loop
                    until over
                    if (NMLDDATE < str8)
                              move      NMLDDATE,str8
                    endif
                    call      NMLDKG
          repeat
          if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
          else
.Use current Mail Date
          endif

         MOVE      OLON TO NOWNFLD    .NEED INFO FOR BILLING. 11/94
         CALL      NOWNKEY
.
.
         MOVE      YES TO SUBPPSW
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
.begin patch 2.1
         move      no to mrgsw
         move      no to shipsw
.end patch 2.1
         CALL      NMRGKEY
.begin patch 2.1
         if        not over
         move      yes to mrgsw
         endif
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
.end  patch 2.1
.begin patch 2.0
         call      wipecvars
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         move      lrn to nshpfld
         call      nshpkey
.end patch 2.0
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
         move      c0 to ar
         move      formar to ar
.start  patch 2.12
         move      ap to ap1
.end  patch 2.12
.
OWNPREP  MOVE      OLON TO OWNKEY
         REP       ZFILL IN OWNKEY
         READ      DUPEOWN,OWNKEY;OWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO OLON
         ENDIF

.begin patch 2.2
         MOVE      C0 TO BRANCH
         MOVE      B1 TO guarpay
         MOVE      GUARCODE TO BRANCH
         BRANCH    BRANCH OF OGUAR1,OGUAR2,OGUAR3,prepayd,prepayd,prepayd:
                   prepayd,prepayd,prepayd
         GOTO      chkadj
OGUAR1   MOVE      STAR TO GUARpay
         GOTO      chkadj
OGUAR2   MOVE      STAR TO GUARpay
         GOTO      chkadj
OGUAR3   MOVE      STAR TO GUARpay
.
prepayd
.end patch 2.2
CHKADJ   MOVE      LRN TO NADJFLD
         REP       ZFILL IN NADJFLD
         CALL      NADJKEY
         GOTO      OUTPUT IF OVER
.
.
.begin patch 2.1
.         MOVE      ASRECADJ TO CVTFLD
.         CALL      CVT
.         MOVE      C0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
.         ADD       FORM82 TO AR
          add      asrecadj to ar
.
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
..START PATCH #1.9 - INCREASED VAR
..         MOVE      C0 TO FORM72
..         MOVE      CVTFLD TO FORM72
..         MULTIPLY  ".01"  BY FORM72
..         ADD       FORM72 TO LRINC
.         MOVE      C0 TO CMPT92
.         MOVE      CVTFLD TO CMPT92
.         MULTIPLY  ".01"  BY CMPT92
.         ADD       CMPT92 TO LRINC
..END PATCH #1.9 - INCREASED VAR
..
         add        aslrinc to lrinc

.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.         MOVE      C0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP1
         add       aspayad1 to ap1
.
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      C0 TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.        ADD       FORM82 TO AP2
         add       aspayad2 to ap2
.end patch 2.1

.
.OUTPUT - OUTPUT TO REC. & PAY STATEMENT FILES.
OUTPUT
         CMATCH    "P" TO STATB
         if        equal
         move      "P" to ascrdb
         else
         move      "O" to ascrdb
         endif
         MOVE      C0 TO FORM102
.begin patch 2.0
.         ADD       FORMAR TO FORM102
.         MULT      HUND BY FORM102
.end patch 2.0
.START PATCH #1.9 - INCREASED VAR
.         MOVE      C0 TO FORM8
.         ADD       FORM102 TO FORM8    *A/R
.begin patch 2.0
.         MOVE      C0 TO FORM9B
.         ADD       FORM102 TO FORM9B    *A/R
.end patch 2.0
.END PATCH #1.9 - INCREASED VAR
.
.begin patch 2.0
.         MOVE      C0 TO FORM102
.         MOVE      AP TO FORM102
.         MULT      HUND BY FORM102
.end patch 2.0

.START PATCH #1.9 - INCREASED VAR
.         MOVE      C0 TO FORM7
.         ADD       FORM102 TO FORM7
.begin patch 2.0
.         MOVE      C0 TO FORM9A
.         ADD       FORM102 TO FORM9A
.end patch 2.0

.END PATCH #1.9 - INCREASED VAR
.
.begin patch 2.0
         MOVE      C0 TO FORM102
         MOVE      FORMAP2 TO FORM102
.         MULT      HUND INTO FORM102
.         MOVE      C0 TO FORM9
.         ADD       FORM102 TO FORM9
.end patch 2.0

         MOVE      NO TO APSW
.START PATCH #1.9 - INCREASED VAR
.         COMPARE   C0 TO FORM7
.begin patch 2.0
.         COMPARE   C0 TO FORM9A
         COMPARE   C0 TO FORM102
.end patch 2.0
.END PATCH #1.9 - INCREASED VAR
.         IF        NOT EQUAL
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
.begin patch 2.0
.         COMPARE   C0 TO FORM9
.end patch 2.0
.         COMPARE   C0 TO ap
.start  patch 2.12
         COMPARE   C0 TO ap1
.end  patch 2.12
.         IF        NOT EQUAL
         IF        GREATER
         MOVE      YES TO APSW
         ENDIF
.START PATCH #1.9 - REPLACED VAR
.         MOVE      FORM7 TO DIM7
.         SCAN      MINUS IN DIM7
.         IF        EQUAL
.         RESET     DIM7
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,C0,C0,DIM7
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 4
.         CLEAR     DIM7
.         APPEND    AMOUNT1 TO DIM7
.         ENDIF
..
.         MOVE      FORM8 TO DIM8
.         SCAN      MINUS IN DIM8
.         IF        EQUAL
.         RESET     DIM8
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,C0,DIM8
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 3
.         CLEAR     DIM8
.         APPEND    AMOUNT1 TO DIM8
.         RESET     DIM8
.         ENDIF
...
.begin patch 2.0
.         MOVE      FORM9A TO DIM9A
.         SCAN      MINUS IN DIM9A
.         IF        EQUAL
.         RESET     DIM9A
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,DIM9A
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 2
.         CLEAR     DIM9A
.         APPEND    AMOUNT1 TO DIM9A
.         ENDIF
.end patch 2.0
.
..begin patch 2.0
.         MOVE      FORM9B TO DIM9B
.         SCAN      MINUS IN DIM9B
.         IF        EQUAL
.         RESET     DIM9B
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,DIM9B
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 2
.         CLEAR     DIM9B
.         APPEND    AMOUNT1 TO DIM9B
.         RESET     DIM9B
.         ENDIF
.end patch 2.0
.END PATCH #1.9 - REPLACED VAR

.
.begin patch 2.0
.         MOVE      FORM9 TO DIM9
.         SCAN      MINUS IN DIM9
.         IF        EQUAL
.         RESET     DIM9
.         CLEAR     AMOUNT1
.         PACK      AMOUNT1 FROM C0,DIM9
.         CALL      KMINUS
.         RESET     AMOUNT1 TO 2
.         CLEAR     DIM9
.         APPEND    AMOUNT1 TO DIM9
.         ENDIF
.end patch 2.0
.
.START PATCH #1.9 - REPLACED VAR
.         RESET     DIM7
.         RESET     DIM8
.begin patch 2.0
.         RESET     DIM9A
.         RESET     DIM9B
.end patch 2.0
.END PATCH #1.9 - REPLACED VAR
.begin patch 2.0
.         RESET     DIM9
.end patch 2.0
.START PATCH #1.9 - REPLACED VAR
.         REP       ZFILL IN DIM7     .A/P1
.         REP       ZFILL IN DIM8     .A/R
.begin patch 2.0
.         REP       ZFILL IN DIM9A     .A/P1
.         REP       ZFILL IN DIM9B     .A/R
.end patch 2.0
.END PATCH #1.9 - REPLACED VAR
.begin patch 2.0
.         REP       ZFILL IN DIM9     .A/P2
.end patch 2.0
.
.start patch 2.4
          cmatch    yes to brker
          if        not equal
          move      mcomp  to stecname
          unpack    mslsper into osales10,osales
          else
                              if       (brsales= "06" or brsales = "27" or brsales = "28")
          unpack    brsales into osales10,osales
                              else
          unpack    mslsper into osales10,osales
                              endif
          endif
.
          match     "00" to mslsper
          if        equal
          unpack    brsales into osales10,osales
          endif
.end patch 2.4
.
.begin patch 2.74
.         cmatch    yes to mbildrct            
.         if         equal            .yes
..         move      "0000" to obrknum
..begin patch 2.71
.         move      "    " to obrknum
.         move      "   " to obrkcnt            03/31/05 JD
..begin patch 2.71
.         endif
.end patch 2.74
         MOVE      C1 TO n2
.begin patch 2.5
         move      b1 to steadjsw
DETADJ2  move      n2 to jstn
         clear     njstfld
         rep       zfill in jstn
          PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      chkshort if over
         move      c1 to steadjsw
         MATCH     "14" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO n2
         GOTO      DETADJ2
         ENDIF         
.end patch 2.5
.begin patch 2.1
.         move       c0 to cvtfld
.         move      jstap1 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        mult       seq by form92
.        div        hund into form92
        move       c0 to adjap
        add        jstap1 to adjap
.        add        form92 to adjap
.begin patch 2.12
        mult       seq by adjap
.end patch 2.12
        compare    c0 to adjap
        if         greater
        move       yes to apsw
        endif
.        MOVE       C0 TO CVTFLD
.        move       jstap2 to cvtfld
.        call       cvt
.        move       c0 to form92
.        move       cvtfld to form92
.        div        hund into form92
.        mult       seq by form92
..START PATCH #1.9 - INCREASED VAR
..        move       c0 to form72
..        add        form92 to form72
..        compare    c0 to form72
        move       c0 to CMPT92
        add        jstap2 to cmpt92
.        add        form92 to CMPT92
.begin patch 2.12
        mult       seq by cmpt92
.end patch 2.12
        compare    c0 to CMPT92
.END PATCH #1.9 - INCREASED VAR
.end patch 2.1
        if         greater
        move       yes to apsw
        endif
.
.begin patch 2.5
chkshort  MOVE      C1 TO n2
.
shortp
         move      n2 to jstn
         clear     njstfld
         rep       zfill in jstn
          PACK      NJSTFLD FROM INVNUM,JSTN
         CALL      NJSTKEY
         GOTO      writste IF OVER
         MATCH     "16" TO JSTREASN
         IF        NOT EQUAL
         ADD       C1 TO n2
         GOTO      shortp
         else
         move      "2" to steadjsw
         ENDIF         
.end patch 2.5
writste 
.Start patch #1.9 - added century var
.        WRITE     NSTEFILE,SEQ;"F0":          .001-002 stecode 
.                   MLRN:                       .003-006 mailer
.                   LRN:                        .007-012 lr
.                   BILLTN:                     .013-013 bill to
.                   INVNUM:                     .014-019 invoice number
.                   INVDTEM,INVDTED,INVDTEY:    .020-025 invoice date
.                   *ZF,DIM8:                   .026-033 a/r no dec.
.                   COBN:                       .034-036 mailer cnt
.                   OMLRPON:                    .037-048 mailer po
.                   *ZF,DIM7:                   .049-055 a/p1
.                   *ZF,DIM9:                   .056-064 a/p2
.                   OMDTEM,OMDTED,OMDTEY:       .065-070 maildate
.                   GUARPAY:                    .071-071 guar (*)
.                   O1DES:                      .072-106 list name
.                   OBRKGUAR:                   .107-107 outside guar
.                   STEFIL3:                    .108-110 fill
.                   stecname:                   .111-135   CLIENT NAME stecname(FOR ALPHA SORTING)
.                   oBRKNUM:                    .116-119
.                   oBRKCNT:                    .120-122
.                   osales10:                   .123-124
.                   osales:                   .123-124
.                   olon:                      .125-128
.                   chkdtem:                    .129-
.                   chkdted:
.                   chkdtey                     .   -134
.begin patch 2.0
.        WRITE     NSTEFILE,SEQ;"F0":          .001-002 stecode
.                   MLRN:                       .003-006 mailer
.                   LRN:                        .007-012 lr
.                   BILLTN:                     .013-013 bill to
.                   INVNUM:                     .014-019 invoice number
.                   CC,INVDTEY,INVDTEM,INVDTED: .020-025 invoice date
.                   *ZF,DIM9B:                   .026-033 a/r no dec.
.                   COBN:                       .034-036 mailer cnt   .
.                   OMLRPON:                    .037-048 mailer po
.                   *ZF,DIM9A:                   .049-055 a/p1
.                   *ZF,DIM9:                   .056-064 a/p2
.                   OMDTEC,OMDTEY,OMDTEM,OMDTED:       .065-070 maildate
.                   GUARPAY:                    .071-071 guar (*)
.                   O1DES:                      .072-106 list name
.                   OBRKGUAR:                   .107-107 outside guar
.                   STEFIL3:                    .108-110 fill
.                   stecname:                   .111-135   CLIENT NAME stecname(FOR ALPHA SORTING)
.                   oBRKNUM:                    .116-119
.                   oBRKCNT:                    .120-122
.                   osales10:                   .123-124
.                   osales:                   .123-124
.                   olon:                      .125-128
.                   chkdtem:                    .129-
.                   chkdted:
.                   chkdtey                     .   -134
..End patch #1.9 - added century var
.add checking here which company
.begin patch 2.72
.begin patch 2.73
          clear     str2
          Pack      Str2 from Osales10,Osales
          clear     str1
.          unpack    LRn into Str1,str5
.          IF        (Ocompid = "P" or (Ocompid2 = "P" & (str2 = "27" or Str2 = "28"))or Str1 = "B" or STR1 = "M")                .billable thru pacific lists
.          WRITE               NSTEPFILE,SEQ;"FP":                     ..001-002 stecode
.                    MLRN:                                 ..003-006 mailer
.                    LRN:                                  ..007-012 lr
.                    BILLTN:                               ..013-013 bill to
.                    INVNUM:                               ..014-019 invoice number
.                    invdtec,INVDTEY,INVDTEM,INVDTED:      ..020-027 invoice date
.                    *ZF,ar:                               ..028-040 a/r 10.2
.                    COBN:                                 ..041-043 mailer cnt
.                    OMLRPON:                              ..044-055 mailer po
.                    *ZF,ap1:                              ..056-068 a/p1
.                    *ZF,form102:                          ..069-081 a/p2
.                    OMDTEC,OMDTEY,OMDTEM,OMDTED:          ..082-089 maildate
.                    GUARPAY:                              ..090-090 guar (*)
.                    O1DES:                                ..091-125 list name
.                    OBRKGUAR:                             ..126-126 outside guar
.                    STEFIL3:                              ..127-129 fill
.                    stecname:                             ..130-154   CLIENT NAME stecname(FOR ALPHA SORTING)
.                    oBRKNUM:                              ..155-158
.                    oBRKCNT:                              ..159-161
.                    osales10:                             ..162-162
.                    osales:                               ..163-163
.                    olon:                                 ..164-167
.                    chk1dtec:                             ..168-169
.                    chk1dtey:                             ..170-171
.                    chk1dtem:                             ..172-173
.                    chk1dted:                              ..174-175
.                    steadjsw                                    ..176 steadjsw "used???"
.          Else
        WRITE     NSTEFILE,SEQ;"F0":                     ..001-002 stecode
                   MLRN:                                 ..003-006 mailer
                   LRN:                                  ..007-012 lr
                   BILLTN:                               ..013-013 bill to
                   INVNUM:                               ..014-019 invoice number
                   invdtec,INVDTEY,INVDTEM,INVDTED:      ..020-027 invoice date
                   *ZF,ar:                               ..028-040 a/r 10.2
                   COBN:                                 ..041-043 mailer cnt
                   OMLRPON:                              ..044-055 mailer po
                   *ZF,ap1:                              ..056-068 a/p1
                   *ZF,form102:                          ..069-081 a/p2
                   OMDTEC,OMDTEY,OMDTEM,OMDTED:          ..082-089 maildate
                   GUARPAY:                              ..090-090 guar (*)
                   O1DES:                                ..091-125 list name
                   OBRKGUAR:                             ..126-126 outside guar
                   STEFIL3:                              ..127-129 fill
                   stecname:                             ..130-154   CLIENT NAME stecname(FOR ALPHA SORTING)
                   oBRKNUM:                              ..155-158
                   oBRKCNT:                              ..159-161
                   osales10:                             ..162-162
                   osales:                               ..163-163
                   olon:                                 ..164-167
                   chk1dtec:                             ..168-169
                   chk1dtey:                             ..170-171
                   chk1dtem:                             ..172-173
                   chk1dted:                              ..174-175
                   steadjsw                                    ..176 steadjsw "used???"
.          Endif
.end patch 2.73
.end patch 2.72
.end patch 2.0
         move      no to brker
         CMATCH    YES TO APSW
         GOTO      READINV IF NOT EQUAL
WRTSLS
         move      Mcomp,sLSCNAME
.Start patch #1.9 - added century var
.          WRITE     SLSFILE,SEQ;MLRN:
.                   LRN:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   OMDTEM,OMDTED,OMDTEY:
.                   *ZF,DIM7:
.                   WSJPC:
.                   ADJC:
.                   INVDTEM,INVDTED,INVDTEY:
.                   *ZF,DIM9:
.                   O1DES:
.                   ASCRDB:
.                   GUARPAY:
.                   DIM8:
.                   chkdtem:                        -100
.                   chkdted:
.                   chkdtey:
.                   SLSCNAME                     .   -105
.begin patch 2.0
.          WRITE     SLSFILE,SEQ;MLRN:
.                   LRN:
.                   OLON:
.                   OBRKGUAR:
.                   COBN:
.                   OLNUM:
.                   OMDTEC,OMDTEY,OMDTEM,OMDTED:
.                   *ZF,DIM9A:
.                   WSJPC:
.                   ADJC:
.                   CC,INVDTEY,INVDTEM,INVDTED:
.                   *ZF,DIM9:
.                   O1DES:
.                   ASCRDB:
.                   GUARPAY:
.                   DIM9B:
.                   chkdtem:                        -100
.                   chkdted:
.                   chkdtey:
.                   SLSCNAME                     .   -105
..End patch #1.9 - added century var
          WRITE     SLSFILE,SEQ;MLRN:            ..001-004
                   LRN:                          ..005-010
                   OLON:                         ..011-014
                   OBRKGUAR:                     ..015-015
                   COBN:                         ..016-018
                   OLNUM:                        ..019-024
                   OMDTEC,OMDTEY,OMDTEM,OMDTED:  ..025-032
                   *ZF,ap1:                       ..033-045
                   WSJPC:                        ..046-046
                   ADJC:                         ..047-047
                   invdtec,INVDTEY,INVDTEM,INVDTED:  ..048-055
                   *ZF,form102:                      ..056-068
                   O1DES:                            ..069-103
                   ASCRDB:                           ..104-104
                   GUARPAY:                          ..105-105
                   ar:                               ..106-118
                   chk1dtec:                         ..119-120
                   chk1dtey:                     .     121-122
                   chk1dtem:                    .      123-124
                   chk1dted:                       .   125-126
                   SLSCNAME                     .      127-151

.end patch 2.0
         GOTO      READINV
.
         RETURN
.
*......................................................................
.begin patch 2.1
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         RETURN                                POP THE STACK.
..CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.end patch 2.1
*.........................
norep    TRAPclr    IO
         DISPLAY   *P1:23,*EL,"no ",inpname," file";
         goto       eoj
         
F5EXIT   DISPLAY   *P1:23,*HON,*BLINKON,"OPERATOR ABORTED",*B,*B,*B,*B:
                   *B,*B,*B,*B,*B,*B
         KEYIN     ANS
.
EOJ      IFNZ      PC      
         FLUSH     NSTEFILE
         FLUSH     SLSFILE
         XIF
         WEOF      NSTEFILE,SEQ
         CLOSE     NSTEFILE,EOFSIZE
         WEOF      SLSFILE,SEQ
         CLOSE     SLSFILE,EOFSIZE
         DISPLAY   *P1:23,*EL,"START TIME : ",TIME
         CLOCK     TIME TO TIME
         DISPLAY   *P1:24,*R,*EL,"STOP TIME : ",TIME
         shutdown  "cls"
         STOP
.KMINUS - CONVERT TO MINUS OVERPUNCH. ENTER WITH AMOUNT1 EXIT AMOUNT1
KMINUS
         RESET     AMOUNT1
         MOVE      AMOUNT1 TO NEWFLD
         REP       "-0" IN NEWFLD
         ENDSET    NEWFLD
         REP       TOMOV IN NEWFLD         *MINUS OVERPUNCH CONVERT
         RESET     NEWFLD
         CLEAR     AMOUNT1
         MOVE      NEWFLD TO AMOUNT1
         RETURN
         INCLUDE   NORDIO.INC
         INCLUDE   NADJIO.inc
.begin patch 2.7
.         INCLUDE   NINVIO.inc
          INCLUDE             ninvio.inc
          Include   NInvAcdio.inc
.         INCLUDE   COMPUTE.inc
          INCLUDE             compute.inc
.end patch 2.7
.Patch2.6
          include   compio.inc
          include   cntio.inc
.         include   nmlrio.inc
.Patch2.6
         INCLUDE   NOWNIO.INC
         INCLUDE   NDAT3IO.INC
.Patch2.6
.         include   nbrkio.inc
.Patch2.6
         INCLUDE   NJSTIO.inc
         include   nmrgio.inc
.begin patch 2.0
         include   ndatio.inc
         include   nacdio.inc
.         INCLUDE   COMPUTE.INc
         include   nshpio.inc
.end patch 2.0
         include   nmldio.inc
         INCLUDE   COMLOGIC.inc

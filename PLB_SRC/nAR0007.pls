...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   nshpdd.inc
         INCLUDE   NINVDD.inc
         INCLUDE   CONSACCT.inc
         include   nacddd.inc
          include   compdd.inc
          include   cntdd.inc
         INCLUDE   NBILDD.inc
         INCLUDE   NJSTDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NOWNDD.inc
         include   ndatdd.inc
         INCLUDE   NDAT3DD.INC
         include   nmrgdd.inc
         include   nrtndd.inc
         INCLUDE   GNXTDD.inc
          INCLUDE   NSELDD.INC
          INCLUDE   NTXTDD.INC
          include   nsel2dd.inc
          include   nmoddd.inc
          include   ninvacddd.inc
mrgsw    dim       1
shipsw   dim       1

Release   INit      "1.00"   DLH  USE AR & AP
.converted from Ninc0007
.Release   INit      "2.85"   DLH  sort out invoices
.Release   INit      "2.84A"   DLH  temp fix  
.RELEASE   INIT      "2.84"          JD30JUN2006  Removed WS/Nut Act 5.00/m pricing.
.release  init      "2.83"          ASH 07APR2005  COMMPER CONVERSION
.release  init      "2.82"          JD  09JUn2004  Fix required trim of datacard var Commper
.release  init      "2.81"         JD   04Jun2004  Use NINCLAST for starting inv #.
.release  init      "2.8"        DMB    26MAY2004 Mailer Conversion
.RELEASE   INIT     "2.7"          ASH 28JAN2004 DATACARD CONVERSION
.RELEASE   INIT     "2.6"          ASH 02OCT2000 NEW SERVER ADDED
.RELEASE   INIT     "2.5"          DLH 04APR99 NININV Y2K,
.NOTE TEMP STARTS IN 91 TO CREATE NEW REVENUE FILE
.RELEASE   INIT     "2.4"          ASH 05JAN99 NINORD Y2K, File expansion; CONSACCT.INC VAR EXPANSION
.release   init     "2.3"          DLH 22Sep98 income name from 25 to 45 bytes add 1999
.release   init     "2.2"          Aug98 DLH Cleanup of key
.release   init     "2.11"         9Apr98 DLH change locking & some unbilled (pass3)
.                                 fixes
.release  init      "2.1"          7feb97 DLH new datacard format
.release  init      "2.0"          DLH 05sep95 include unbilled.
.RELEASE  INIT      "1.5"          jd  30apr95  added nmrgdd for compute chgs.
.RELEASE  INIT      "1.4"        DLH 15NOV94 NEW COMPUTE, CONSACCT.INC
.RELEASE   INIT      "1.3"
.RELEASE  INIT      "1.2"         DLH 17FEB93 GNXTxx.INC
.RELEASE  INIT      "1.1"         DLH 24MAR92    NORDXX, NINVXX, NMLRXX,
.                                NOWNXX, NBILXX, NJSTXX
.RELEASE  INIT      "1.0"
.
.
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       10
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
SYSDAT   DIM       8                        SYSTEM DATE
.
.FILES.
...............................................................................
.
.
.START PATCH #2.4 - INCREASED VARS
.output    ifile     keylen=15,fix=151
.output    ifile     keylen=15,fix=163
.output    ifile     keylen=15,fix=163
.begin patch 2.5
.output    ifile     keylen=15,fix=190
output    ifile     keylen=15,fix=191
File      FIle
.end patch 2.5
.END PATCH #1.5 - INCREASED VARS
.output    ifile     keylen=15,fix=131
.OUTPUT   IFILE     KEYLEN=17,FIX=131
DUPEOWN  IFILE     KEYLEN=4
. ISAM KEY VARIABLES
.
HBILLKEY DIM       8    *FOR MATCH MLR/BILL-TO BREAK?
key15    dim       15   *OUTPUT FILE KEY.
.KEY17    DIM       17   *OUTPUT FILE KEY.
key6     dim       6
.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
. WORK VARIABLES
.START PATCH 2.83 ADDED LOGIC
N34       FORM      3.4
.END PATCH 2.83 ADDED LOGIC
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
PASS     FORM      1
ANS      DIM       1
TIPE     DIM       1
CHECK    FORM      1
SOURCE   DIM       1
M        INIT      "M"
B        INIT      "B"
R        INIT      "R"
.E        INIT      "E"
SIX      FORM      "6"
SALESNUM FORM      2
SALENUM  DIM       2
ADJAR    FORM      7.2
ADJLR    FORM      7.2
ADJNIN   FORM      7.2
.Start Patch #2.4 - increase var
.ADJAP    FORM      7.2
ADJAP    FORM      9.2
.end Patch #2.4 - increase var
ZERO     FORM      "0"
ONE      FORM      "1"
.
FORM7    FORM      7
NINETEEN INIT      "19"
TAB      FORM      "37"
.begin patch 2.6
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
.begin patch 2.6
NUM10    FORM      10
COUNT    FORM      5
COUNT1   FORM      5
CO       FORM      1
DATEMASK DIM       10
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
MDATE    FORM      5
CHKJUL   FORM      5
JUNDATE  FORM      6
.Start Patch #2.4 - increase var
.QTYCHK   FORM      6
QTYCHK   FORM      9
.end Patch #2.4 - increase var
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
SYSJDATE FORM      5
.
........................................
.OUTPUT.
........................................
.TIPE     DIM       1          1-1       R OR E       -KEY
.SOURCE   DIM       1          2-2       B OR M       -KEY
.ID#      DIM       4          3-6      MLR OR OWNER  -KEY
.ID#      DIM       4          3-8      MLR OR list#  -KEY  as of may98
.MONTH    DIM       2          7-8                    -KEY
.YEAR     DIM       2          9-10                   -KEY
owner    dim       4
INCNAME  DIM       45        11-55
.START PATCH #2.4 - INCREASED VARS
.ARTOT    FORM      8.2        36-46
.APTOT    FORM      8.2        47-57
.LRTOT    FORM      8.2        58-68
.QTYTOT   FORM      8          69-76
.ADJARTOT FORM      8.2        78-87
.ADJAPTOT FORM      8.2        88-98
.ADJLRTOT FORM      8.2        99-109
.

ARTOT    FORM      10.2        56-68
APTOT    FORM      10.2        69-81
LRTOT    FORM      10.2        82-94
.begin patch 2.5
NINTOT    FORM      10.2       95-107
.end patch 2.5
QTYTOT   FORM      8          108-115
ADJARTOT FORM      10.2       116-128
ADJAPTOT FORM      10.2       129-141
ADJLRTOT FORM      10.2       142-154
.begin patch 2.5
ADJNINTOT FORM      10.2      155-167
.end patch 2.5
.END PATCH #2.4 - INCREASED VARS
.Start Patch #2.4 - increase var
unbilinc form      8.2       110-120
.end Patch #2.4 - increase var
.
unbill   form      8.2        168-190
.
.Start Patch #2.4 - increase var
.UNBILAMT FORM      7.2
UNBILAMT FORM      9.2
.end Patch #2.4 - increase var
form52   form      5.2
specl    dim       1
card$    form      5.2
.Start Patch #2.4 - increase var
.net72    form      7.2            holding field while calcing net charges
net92    form      9.2            holding field while calcing net charges
.end Patch #2.4 - increase var
.START PATCH 2.7 - ADDED LOGIC
TEXT1    DIM       47
.END PATCH 2.7 - ADDED LOGIC
lastinv  dim       6
...................................................
.
BEGINV   DIM       6
.
         MOVE      "NINC0007" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "CLIENT/OWNER-RENT/EXCH INCOME Y-T-D" TO STITLE
         CALL      PAINT
         MOVE      C1 TO NORDPATH      .SET ACCESS TO ISI BY LR#.
         MOVE      C2 TO NINVPATH      .SET ACCESS TO ISI BY INV#.
         MOVE      C1 TO NMLRPATH
         MOVE      C1 TO NOWNPATH
         move      c3 to nmlrlock
         move      c1 to ndatpath
         move      c3 to ndatlock
         move      c3 to nordlock
.
.        IFNZ      PC
.        OPEN      DUPEOWN,"DUPEOWNERS",READ
.        XIF
        IFZ      PC
        OPEN      DUPEOWN,"DUPEOWN",READ
        XIF
.         OPEN      RECNUM,"INVNUM",SHARE
.         READTAB   RECNUM,ZERO;*TAB,BEGINV
.         CLOSE     RECNUM
.         MOVE      "NINVLAST" TO GNXTFLD
.Patch2.81
         MOVE      "NINCLAST" TO GNXTFLD
.Patch2.81
         CALL       GNXTKEY
         MOVE       GNXTNUM TO NINVFLD
         REP        ZFILL TO NINVFLD
.temp dave
.       goto       unbilled
.        MOVE      "197750" TO GNXTNUM
.
.temp redo 1999
.FirstN    INit      "577030"    FIRST NIN INV# OF 2010
. 579315  April 2010
.          Move      "579315",Gnxtnum
.          Move      "579315",ninvfld
.        MOVE      "362336" TO GNXTNUM
.        MOVE      "362336" TO ninvfld
.        MOVE      "577030" TO GNXTNUM
.        MOVE      "577030" TO ninvfld
         display    *p1:22,*el,"starting with ",gnxtnum
.
. 172279 FIRST INV# OF 1990
. 197750 FIRST INV# OF 1991
.        MOVE      "197750" TO BEGINV
.        MOVE      "197750" TO ninvfld
         display    *p1:22,*el,"starting with ",ninvfld
.temp
.START PATCH #2.4 - INCREASED VARS
.         PREPARE   OUTPUT,"g:\DATA\INCOME7","g:\DATA\INCOME7","15","151"
.         PREPARE   OUTPUT,"g:\DATA\INCOME7","g:\DATA\INCOME7","15","163"
.begin patch 2.5
.START PATCH 2.6 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\INCOME7","g:\DATA\INCOME7","15","190",EXCLUSIVE
         PACK      STR35,NTWKPATH1,"INCOME7"
         PACK      STR45,NTWKPATH1,"INCOME7"
.         PREPARE   OUTPUT,STR35,STR45,"15","190",EXCLUSIVE
         PREPARE   OUTPUT,STR35,STR45,"15","191",EXCLUSIVE
.END PATCH 2.6 REPLACED LOGIC
.end patch 2.5
.END PATCH #2.4 - INCREASED VARS
.         PREPARE   OUTPUT,"g:\DATA\INCOME7","g:\DATA\INCOME7","17","131"
.         PREPARE   OUTPUT,"c:\work\INCOME7","c:\work\INCOME7","17","131"
         MOVE      ONE TO PASS
         goto      datechk
unbilled
.temp dh
.         goto       eoj
.temp dh
         move       c0 TO N7
         DISPLAY   *P10:17,"Unbilled IN ",N7
.START PATCH 2.6 REPLACED LOGIC
.          open   OUTPUT,"g:\data\INCOME7"
          PACK      STR35,NTWKPATH1,"INCOME7"
          open   OUTPUT,STR35
.END PATCH 2.6 REPLACED LOGIC
          MOVE      c3 TO PASS

.end.
datechk   TRAP      EOJ IF F5
.         CLOCK     DATE TO DATE
         clock     timestamp to timestamp
         unpack    timestamp into cc,sysyr,sysmo,sysdy
         pack      date from sysmo,sysdy,cc,sysyr
         pack     date from sysmo,slash,sysdy,slash,cc,sysyr
         pack     today from sysmo,slash,sysdy,slash,sysyr
         move      sysdy to dd
         move     sysmo to mm
         move      sysyr to yy
         call      datetest
.cheat  n2 now holds last day of the month
         move      n2 to sysdy
.
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      DATe TO DATEMASK
         CALL      PAINT
DATEDIS
         MOVE      "Y" TO ANS
         KEYIN     *P10:10,*DV,DATEMASK," OK? ",*RV,*T05,ANS
         CMATCH    "Y" TO ANS
         GOTO      BEGIN IF EQUAL
         KEYIN     *P10:10,*+,*jr,*zf,SYSMO,"/",*jr,*zf,SYSDY,"/",*jr,*zf,cc,*jr,*zf,SYSYR
         move      sysdy to dd
         move     sysmo to mm
         move      sysyr to yy
         call      datetest
.cheat  n2 now holds last day of the month
         move      n2 to sysdy
.
         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,cc,SYSYR
         pack     today from sysmo,slash,sysdy,slash,sysyr
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      date TO DATEMASK
         GOTO      DATEDIS
.
BEGIN
         compare   c3 to pass
         if        equal
         goto      getord                    .more temp
         endif
.begin patch 2.85
.sort 'NEW' Invoices
.INVDTEc        DIM            2     130-131  INVOICE DATE (YEAR)
.INVDTEY        DIM            2     132-133  INVOICE DATE (YEAR)
.INVDTEM        DIM            2     134-135  INVOICE DATE (MONTH)
.INVDTED        DIM            2     136-137  INVOICE DATE (DAY)
         DISPLAY   *P10:11,"Sorting 'new' Invoices: "
          pack      str4 from Sysyr,sysmo
          rep       zfill in str4
          Pack      Taskname from "\\nins1\e\data\text\Nininv.dat,\\nins1\e\data\Nininv.New;12-17,S=#"132='",str4,"'#""
          Sort      Taskname,SunDM="10.10.30.103:502"
          open      FIle,"\\nins1\e\data\nininv.new|10.10.30.103:502"

.         CALL      NINVTST
.
INPUT    
.         CALL      NINVKS
          read      File,seq;invvars
.         GOTO      GETADJ IF OVER
          If        Over
          close     FIle
          goto      Getadj
          endif
.end patch 2.85
         ADD       C1 TO N7
         DISPLAY   *P10:14,"INVOICES IN ",N7,b1,invnum
         MATCH     SYSMO TO INVDTEM
         GOTO      INPUT IF NOT EQUAL

         MATCH     SYSYR TO INVDTEY
         GOTO      INPUT IF NOT EQUAL
         MOVE      LRN TO NORDFLD
         CALL      READ
         GOTO      PROCESS
READ     CALL      NORDKEY
         MOVE      OLON TO NOWNFLD
         move      olnum to ndatfld
         rep       zfill in ndatfld
         CALL      NOWNKEY
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.         MATCH     " " TO OELCODE
.         GOTO      RENTAL IF EQUAL
.         MOVE      ZERO TO CHECK
.         MOVE      OELCODE TO CHECK
.         COMPARE    CHECK TO ONE
.         GOTO      RENTAL IF EQUAL
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      RENTAL IF not EQUAL
         MOVE      "E" TO TIPE
         GOTO      NEXT
RENTAL   MOVE      R TO TIPE
NEXT     CLEAR     SALENUM
         PACK      SALENUM FROM OSALES10,OSALES
         MOVE      "0" TO SALESNUM
         MOVE      SALENUM TO SALESNUM
         MOVE      MCOMP TO incname
         clear     owner
         RETURN
.
PROCESS
.begin patch 2.5
.  REP       ZFILL IN AR
.         REP       ZFILL IN AP1
.         REP       ZFILL IN AP2
.end patch 2.5
.
         ADD       "1" TO COUNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED: ",COUNT:
                   *P10:13,"STARTED WITH INVOICE ## ",GNXTNUM
.
.begin patch 2.84a
          Cmatch    "B",LRN
          goto      B1 if equal
.         scan      "B-M" in LRN
.         call      debugger if equal
         goto      B1 if equal
.end patch 2.84a
         COMPARE   SIX TO SALESNUM
         GOTO      MANAGE IF EQUAL
         COMPARE   "19" TO SALESNUM
         GOTO      MANAGE IF EQUAL
         COMPARE   "27" TO SALESNUM
         GOTO      MANAGE IF EQUAL
         COMPARE   "28" TO SALESNUM
         GOTO      MANAGE IF EQUAL
         match     olnum to exfeelst
         if        equal
         move      "E" to tipe
         goto      manage
         endif
.         scan      "B-M" in LRN
.         goto      B1 if equal
         COMPARE   C0 TO SALESNUM
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      "E" TO TIPE
         ELSE
         GOTO      MANAGE
         ENDIF
         ENDIF
B1
         MOVE      B TO SOURCE
         PACK      NBILFLD FROM MLRN,COBN,BILLTN
         MATCH     NBILFLD TO HBILLKEY
         CALL      READBLTO IF NOT EQUAL
         GOTO      CONTIN
MANAGE
         MOVE      M TO SOURCE
         CALL      OWNPREP
         CALL      READOWN
.
CONTIN

         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
.begin patch 2.6
         call     wipecvars
         move     no to mrgsw
         move     no to shipsw
.end patch 2.6
         move      c0 to nmrgnet
         CALL      NMRGKEY
.begin patch 2.6
         if       not over
         move     yes to mrgsw
         endif
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         call     nshpkey
         if       not over
         move     yes to shipsw
         endif
.end patch 2.6

               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
.
         GOTO      WRITEREC
.
READBLTO
         CALL      NBILKEY
         IF        OVER
         GOTO      READMLR
         ELSE
.         MOVE      BILMLR TO MNUM
         move       "00" to str2
         pack       key6 from str2,bilmlr
         RETURN
         ENDIF
.
READMLR
.
         MOVE      MCOMP TO incname
         move       "00" to str2
         pack       key6 from str2,mnum
         RETURN
.
READOWN
         move       olnum to ndatfld
         call       ndatkey
         move      olstname to INCname
         MOVE      OLnum TO key6
         MOVE      M TO SOURCE
         move      olon to owner
         RETURN
.
*............................................................................
GETADJ
         MOVE      "2" TO PASS
.sort 'NEW' adjustments
         DISPLAY   *P10:11,"Sorting 'new' adjustments: "
          pack      str4 from Sysyr,sysmo
          rep       zfill in str4
.          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127=#'",sysYr,"#'&129=#'",sysmo,"#'#""
          Pack      Taskname from "\\nins1\e\data\text\NAdjust.dat,\\nins1\e\data\Nadjust.New;7-12,S=#"127='",str4,"'#""
          Sort      Taskname,SunDM="10.10.30.103:502"
          open      FIle,"\\nins1\e\data\nadjust.new|10.10.30.103:502"
.
Getadj1
          read      file,seq;JstVars
.         CALL      NJSTSEQ
.         GOTO      eoj IF OVER
         GOTO      getord IF OVER
         MOVE      JSTLR TO NORDFLD
         CALL      READ
.
.begin patch 2.6
.         UNPACK    JSTDATE INTO INVDTEM,INVDTED,INVDTEY
         UNPACK    JSTDATE INTO invdtec,INVDTEY,INVDTEM,INVDTED
.end patch 2.6
         MATCH     SYSMO TO INVDTEM
         GOTO      GETADJ1 IF NOT EQUAL
         MATCH     SYSYR TO INVDTEY
         GOTO      GETADJ1 IF NOT EQUAL
         ADD       "1" TO COUNT1
         DISPLAY   *P10:14,*EL,"NUMBER OF ADJUSTMENTS PROCESSED ",COUNT1
.
.begin patch 2.6
.         MOVE      JSTAR TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO ADJAR
.         MOVE      CVTFLD TO ADJAR
.         MULTIPLY  ".01"  BY ADJAR
          move      jstar to adjar
.
.         MOVE      JSTLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO ADJLR
.         MOVE      CVTFLD TO ADJLR
.         MULTIPLY  ".01"  BY ADJLR
          move      jstlrinc to adjlr
          move      JSTNININC to adjNIN
.
.         MOVE      JSTAP1 TO CVTFLD
..        CALL      CVT
         MOVE      ZERO TO ADJAP
.         MOVE      CVTFLD TO ADJAP
.         DIV       HUND INTO ADJAP
          move      jstap1 to adjap
.
.         MOVE      JSTAP2 TO CVTFLD
.         CALL      CVT
..Start patch #2.4 - replaced var
..         MOVE      ZERO TO FORM72
..         MOVE      CVTFLD TO FORM72
..         DIV       HUND INTO FORM72
..         ADD       FORM72 TO ADJAP
..
.         MOVE      ZERO TO CMPT92
.         MOVE      CVTFLD TO CMPT92
.         DIV       HUND INTO CMPT92
.         ADD       CMPT92 TO ADJAP
..end patch #2.4 - replaced var
         add      jstap2 to adjap
.end patch 2.6
.
         COMPARE   SIX TO SALESNUM
         GOTO      MANAGE1 IF EQUAL
         COMPARE   "19" TO SALESNUM
         GOTO      MANAGE1 IF EQUAL
         COMPARE   C0 TO SALESNUM
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      "E" TO TIPE
         ELSE
         GOTO      MANAGE1
         ENDIF
         ENDIF
         MOVE      B TO SOURCE
         PACK      NBILFLD FROM JSTMLR,JSTCNT,JSTBILTO
         CALL      READBLTO
         GOTO      PROCESS2
MANAGE1
         MOVE      M TO SOURCE
         CALL      OWNPREP
         CALL      READOWN
.
PROCESS2 MOVE      ZERO TO FORMAR
         MOVE      ZERO TO AP
         MOVE      ZERO TO LRINC
.
         MOVE      ADJAR TO FORMAR
         MOVE      ADJAP TO AP
         MOVE      ADJLR TO LRINC
         MOVE      ADJNIN TO NININC
.
*......................................................................
.
WRITEREC MOVE      CO TO STR1
         MOVE      " " TO ANS
         PACK      KEY15 WITH TIPE,SOURCE,key6,cc,invdtey,INVDTEM,ANS
         READ      OUTPUT,KEY15;;
         GOTO      WRITE IF OVER
.begin patch 2.5
.         READ      OUTPUT,KEY15;KEY15,incname,owner,ARTOT,APTOT,LRTOT,QTYTOT:
.                   ADJARTOT,ADJAPTOT,ADJLRTOT,unbill
         READ      OUTPUT,KEY15;KEY15,incname,owner,ARTOT,APTOT,LRTOT,NINTOT,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,unbill
.end patch 2.5
         BRANCH    PASS OF ADDINV,ADDADJ,ADDUNBIL
ADDINV
         ADD       FORMAR TO ARTOT
         ADD       AP TO APTOT
         ADD       LRINC TO LRTOT
.begin patch 2.5
.         MOVE      QTYSHP TO FORM7
.         ADD       FORM7 TO QTYTOT
         ADD       NININC TO NINtot
         MOVE      QTYbild TO n9
         ADD       n9 TO QTYTOT
.end patch 2.5
         GOTO      UPDATE
ADDADJ
         ADD       FORMAR TO ADJARTOT
         ADD       AP TO ADJAPTOT
         ADD       LRINC TO ADJLRTOT
         ADD       NININC TO NINtot
         GOTO      UPDATE
.
addunbil add       unbilinc to unbill
.
UPDATE
.begin patch 2.5
.        UPDATe    OUTPUT;KEY15,incname,owner,ARTOT,APTOT,LRTOT,QTYTOT,ADJARTOT:
.                   ADJAPTOT,ADJLRTOT,unbill
         UPDATe    OUTPUT;KEY15,incname,owner,ARTOT,APTOT,LRTOT,nintot,QTYTOT,ADJARTOT:
                   ADJAPTOT,ADJLRTOT,adjnintot,unbill
.end patch 2.5
         CALL      WIPER
         BRANCH    PASS OF INPUT,GETADJ1,getrec
WRITE    call      wiper
         MOVE      B1 TO ANS
         BRANCH    PASS OF ADDINV1,ADDADJ1,ADUNBIL1
ADDINV1
         MOVE      FORMAR TO ARTOT
         MOVE      AP TO APTOT
         MOVE      LRINC TO LRTOT
.begin patch 2.5
.         MOVE      QTYSHP TO FORM7
.         MOVE      FORM7 TO QTYTOT
         MOVE      NININC TO NINTOT
         MOVE      QTYbild TO n9
         MOVE      n9 TO QTYTOT
.end patch 2.5
         GOTO      WRITE1
ADDADJ1
         MOVE      FORMAR TO ADJARTOT
         MOVE       AP TO ADJAPTOT
         MOVE       LRINC TO ADJLRTOT
         GOTO       WRITE1

ADUNBIL1 move       unbilinc to unbill
.
WRITE1
.begin patch 2.5
.       WRITE     OUTPUT,KEY15;KEY15,incname,owner,ARTOT,APTOT,LRTOT,QTYTOT:
.                   ADJARTOT,ADJAPTOT,ADJLRTOT,unbill
        WRITE     OUTPUT,KEY15;KEY15,incname,owner,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,unbill
.end patch 2.5
         CALL      WIPER
         BRANCH    PASS OF INPUT,GETADJ1,getrec
*............................................................
.
.begin patch 2.6
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         NORETURN                                POP THE STACK.
.         BRANCH    PASS OF INPUT,GETADJ
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.end patch 2.6
.
.
WIPER
         MOVE      C0 TO ARTOT
         MOVE      C0 TO APTOT
         MOVE      C0 TO LRTOT
.begin patch 2.5
         MOVE      C0 TO adjninTOT
         MOVE      C0 TO NINTOT
.end patch 2.5
         MOVE      C0 TO QTYTOT
         MOVE      C0 TO ADJARTOT
         MOVE      C0 TO ADJAPTOT
         MOVE      C0 TO ADJLRTOT
         move      c0 to unbilinc
         move      c0 to unbill
         RETURN
getord
.         close     nordfile
         MOVE      c3 TO PASS
         MOVE      "unbilled" TO NORDNAME
         open      nordfile,nordname
         move      c1 to nordflag
         move      "12" to mm
         move      "31" to dd
         move      sysyr  to yy
         call      cvtjul
         move      juldays to sysjdate
getrEC   DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
.
         CALL      NORDSEQ
         GOTO      eoj IF OVER
         call      rotdial
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       *YES
         RESET     CANCODES               *RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       *CANCELLED?
         GOTO      GETREC IF EQUAL
.         MATCH     " " TO OELCODE
.         GOTO      RENTAL2 IF EQUAL
.         MOVE      ZERO TO CHECK
.         MOVE      OELCODE TO CHECK
.         COMPARE    CHECK TO ONE
.         GOTO      RENTAL2 IF EQUAL
         ADD       C1 TO N7
         DISPLAY   *P10:17,"Unbilled IN ",N7
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      RENTAL2 IF NOT EQUAL
         MOVE      "E" TO TIPE
         GOTO      NEXT2
RENTAL2   MOVE      R TO TIPE
next2
         CLEAR     SALENUM
         PACK      SALENUM FROM OSALES10,OSALES
         MOVE      "0" TO SALESNUM
         MOVE      SALENUM TO SALESNUM
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      C1 TO NOWNPATH
         CALL      NOWNKEY
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
.
         MOVE      NO TO LSTMSW
          MOVE      C1 TO Nbrkpath
         PACK      nbrkfld FROM Obrknum,obrkcnt
         REP       ZFILL,nbrkfld
         clear     brsales
         CALL      nbrkkey
         move      c0 to n2
         move      brsales to n2
         compare   c6 to n2
         if        equal
         move       yes to lstmsw
         CLEAR      MCODE         .CLEAR VAR.
         ELSE
         MATCH     "00" TO STR2
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        NOT EQUAL
         MOVE      YES TO LSTMSW
         ENDIF
         ENDIF
         ENDIF
.         ENDIF
         MOVE      NO TO OVER
PREPOWN  MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
         CALL      MLRREAD
         cmatch    yes to lstmsw
         goto      pass2 if equal
pass1
         cmatch    "B" to mcode
         goto      pass1a if equal
         cmatch    "A" to mcode
         goto      pass1a if equal
         goto      pass3
pass1a   MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
.test dlh 03dec96 use maildate
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         MATCH     "00" TO ORTNDTEY
         call       USEMD IF EQUAL
         CALL      CVTJUL
         GOTO      CHKDAYS
pass2    clear     mcode
         MOVE      ORTNDTEM TO MM                           .if batch
         MOVE      ORTNDTED TO DD                           .dlh/jd 01jun94
         MOVE      ORTNDTEY TO YY
         MATCH     "00" TO ORTNDTEY
         call       USEMD IF EQUAL
         CALL      CVTJUL
         goto       chkdays
.
pass3
.         MOVE      ORTNDTEM TO MM
.         MOVE      ORTNDTED TO DD
.         MOVE      ORTNDTEY TO YY
.         MATCH     "00" TO ORTNDTEY
.         call       USEMD IF EQUAL
         clear     mm
         clear     dd
         clear     yy
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         GOTO      CHKDAYS
.
CHKDAYS
         MOVE      SYSJDATE TO CHKJUL
         SUB       JULDAYS FROM CHKJUL
         COMPARE    CHKJUL TO C0                to be billed this year?
         GOTO      GETREC IF NOT LESS            No
         GOTO      CONTIN1                        yes ,PROCESS
.
USEMD    clear     mm
         clear     dd
         clear     yy
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         return
.
CONTIN1
         CALL      DETAIL
         GOTO      getrec              *ADDITIONAL CRITERIA FAILED GET NEXT RE
.
.Start patch #2.4 - replaced var
.DETAIL   MOVE      C0 TO FORM72
DETAIL   MOVE      C0 TO CMPT92
.end patch #2.4 - replaced var
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
         MOVE      C0 TO AR
         move      C0 to commper
         clear     specl               *09Apr98 DLH clear for default
         CALL      MLRREAD
         CALL      READSHIP            *ORDER SHIPPED????
         CALL      READMRG
         CALL      GETCARD
         CALL      READRTN             *different from nord0013
.Start patch #2.4 - replaced var
.         SUB       FORM72 FROM FORM72
.         SUB       FORM52 FROM FORM52
.         MOVE      OQTY TO FORM72
.         DIV       THOUS INTO FORM72
.
         SUB       CMPT92 FROM CMPT92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO CMPT92
         DIV       THOUS INTO CMPT92
.End patch #2.4 - replaced var
         call      special                             ,check to see if special pricing
         cmatch    yes to specl
         if        equal
         goto      calcsp
         endif
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      OPPM TO FORM52
.         DIV       HUND INTO FORM52
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    unpack    OPPM,str3,str2
                    pack      str6,str3,".",str2
                    rep       zfill,str6
                    move      str6,NSEL2PRICE
.                   move      "/m",NMODDESC
.         else
.                   pack      NMODFLD,NSEL2DESC
.                   rep       zfill,NMODFLD
.                   move      "NMODKEY",Location
.                   pack      KeyLocation,"Key: ",NMODFLD
.                   call      NMODKEY
.                   if over
.                             move      "/m",NMODDESC
.                   endif
          endif
          move      NSEL2PRICE,FORM52
.         call      Trim using NMODDESC
.END PATCH 2.7 REPLACED LOGIC
.Start patch #2.4 - replaced var
.calcsp   MULT      FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILINC
calcsp   MULT      FORM52 BY CMPT92
         MOVE      CMPT92 TO UNBILINC
.end patch #2.4 - replaced var
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      OKEX IF EQUAL
         if        (onetfm="M" | onetfm="F")      .discounted order???
         move      onetper to form32              .yes calc it
         mult      ".01" by form32
.Start patch #2.4 - replaced var
.         MOVE      OQTY TO FORM72
.         DIV       THOUS INTO FORM72
.         mult       form32 by form72               .percentage of billable names
.         move      form72 to net72                 .save it
.         MULT      FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILINC               .base rental
.
         MOVE      OQTY TO CMPT92
         DIV       THOUS INTO CMPT92
         mult       form32 by CMPT92               .percentage of billable names
         move      CMPT92 to net92                 .save it
         MULT      FORM52 BY CMPT92
         MOVE      CMPT92 TO UNBILINC               .base rental
.end patch #2.4 - replaced var
         endif
         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
.Start patch #2.4 - replaced var
.         MOVE      C0 TO FORM72
.         MOVE      OEXQTY TO FORM72
.         COMPARE   C0 TO FORM72            PURE EXCHANGE ?
         MOVE      C0 TO CMPT92
         MOVE      OEXQTY TO CMPT92
         COMPARE   C0 TO CMPT92            PURE EXCHANGE ?
.end patch #2.4 - replaced var
         IF        EQUAL                 YES.
         MOVE      C0 TO QTYCHK
         MOVE      NO TO SPLITSW
         MOVE      OQTY TO QTYCHK
.Start patch #2.4 - replaced var
.         MOVE      QTYCHK TO FORM72
         MOVE      QTYCHK TO CMPT92
.end patch #2.4 - replaced var
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MOVE      C0 TO UNBILINC
         GOTO      OK
         ELSE
         GOTO      GETPRICE
         ENDIF
         ELSE
         MOVE      YES TO SPLITSW
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MOVE      C0 TO UNBILAMT
         GOTO      RENTPART
         ELSE
         MOVE      C0 TO QTYCHK
         MOVE      OEXQTY TO QTYCHK
.Start patch #2.4 - replaced var
.         MOVE      QTYCHK TO FORM72
         MOVE      QTYCHK TO CMPT92
.end patch #2.4 - replaced var
         GOTO      GETPRICE
         ENDIF
         ENDIF
.
GETPRICE
         match     "0006" to omlrnum
         if        equal
         move      c7 to form52
         goto      calce
         endif

         cmatch    yes to specl
         if        equal
         goto      calce
         endif

         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    *SAVE RESULT
         CLEAR     MM                *09apr98
         CLEAR     DD                *09apr98
         CLEAR     YY                *09apr98
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           *CONVERT TODAY'S    DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    *SAVE RESULT
         COMPARE   JUNDATE TO MDATE
         IF        NOT GREATER
         MOVE      C8 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF

         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         GOTO      CALCE
         ENDIF
         MOVE      C8 TO FORM52
CALCE
.Start patch #2.4 - replaced var
.         DIVIDE    THOUS INTO FORM72
.        MULTIPLY  FORM52 BY FORM72
.        MOVE      FORM72 TO UNBILAMT
         DIVIDE    THOUS INTO CMPT92
         MULTIPLY  FORM52 BY CMPT92
         MOVE      CMPT92 TO UNBILAMT
.end patch #2.4 - replaced var
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
.Start patch #2.4 - replaced var
.         MOVE      FORM72 TO UNBILINC
         MOVE      CMPT92 TO UNBILINC
.end patch #2.4 - replaced var
         GOTO      OK
         ENDIF
.
.Start patch #2.4 - replaced var
.RENTPART MOVE      C0 TO FORM72          SPLIT RENT/EXCHANGE
.         MOVE      C0 TO N7
.         MOVE      OQTY TO FORM72
.         MOVE      OEXQTY TO N7
.         SUBTRACT  N7 FROM FORM72           GET RENTAL PORTION
.         MULT      ".001" BY FORM72
RENTPART MOVE      C0 TO CMPT92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO CMPT92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM CMPT92           GET RENTAL PORTION
         MULT      ".001" BY CMPT92
.end patch #2.4 - replaced var
.oops should not be here         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         cmatch    yes to specl
         if        equal
         goto      calcsp2
         endif

         compare   c0 to card$
         if        equal
         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         else
         move      card$ to form52
         endif
.         CALL      GETCARD
.Start patch #2.4 - replaced var
.         MULT      FORM52 BY FORM72
         MULT      FORM52 BY CMPT92
.end patch #2.4 - replaced var
.
         match      "0192" to obrknum
         IF         EQUAL
.         display    *p1:24,*el,"EPSILON!!!!!!!!!"
.START PATCH 2.83 REPLACED LOGIC
.         match      " 20" to commper
.         if         equal
          if (COMMPER = "20")
.END PATCH 2.83 REPLACED LOGIC
.Start patch #2.4 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY CMPT92
.end patch #2.4 - replaced var
         goto      addamt
         ENDIF
.START PATCH 2.83 REPLACED LOGIC
.         match      " 30" to commper
.         if         equal
          if (COMMPER = "30")
.END PATCH 2.83 REPLACED LOGIC
.Start patch #2.4 - replaced var
.         MULT      ".2" BY FORM72
..         display    *p1:24,*el,"form72 ",form72,b1,commper,*w2
         MULT      ".2" BY CMPT92
.         display    *p1:24,*el,"CMPT92 ",CMPT92,b1,commper,*w2
.end patch #2.4 - replaced var
         goto      addamt
         ENDIF
.oddball use 10% commission
.Start patch #2.4 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY CMPT92
.end patch #2.4 - replaced var
         goto      addamt
         endif
.
        CMATCH    YES TO LSTMSW
         IF        EQUAL
.Start patch #2.4 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY CMPT92
.end patch #2.4 - replaced var
         ELSE
.START PATCH 2.83 REPLACED LOGIC
.         move       c0 to n32      .DLH USE Datacard info 09Jul98
.         move       commper to n32
.         mult       ".01" by n32
..Start patch #2.4 - replaced var
..         mult       n32,form72
...         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
.         mult       n32,CMPT92
.....................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,CMPT92
.END PATCH 2.83 REPLACED LOGIC
.         MULT      ".2" BY CMPT92            ESTIMATED LR INCOME.
.end patch #2.4 - replaced var
         ENDIF
.

addamt2
.Start patch #2.4 - replaced var
.         ADD       FORM72 TO UNBILAMT
         ADD       CMPT92 TO UNBILAMT
.end patch #2.4 - replaced var
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
calcsp2
.Start patch #2.4 - replaced var
.         MULT      FORM52 BY FORM72
.         ADD       FORM72 TO UNBILAMT
         MULT      FORM52 BY CMPT92
         ADD       CMPT92 TO UNBILAMT
.end patch #2.4 - replaced var
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
.
GETCARD  MOVE      OLNUM TO NDATFLD
.         DISPLAY   *P1:24,"GETCARD",*W2;
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
.START PATCH 2.7 REPLACED LOGIC
.         parse     textdata into text1 using " ~09",noskip,blankfill
.         SCAN      "EXCHANGE ONLY" IN TEXT1
.         RETURN    IF EQUAL                 NO USABLE $ RETURN
.         RESET     TEXT1
.         SCAN      "$" IN TEXT1
.         RETURN    IF NOT EQUAL        NO USABLE $ RETURN
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1
.         move      c0 to card$
.         MOVE      STR2 TO card$
..         DISPLAY   *P1:24,STR2,B1,card$,*W2;
.         SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
.         RETURN    IF NOT EQUAL            *YES.
.         CLEAR     STR2
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1       *NO, NOW WE DO!
.         move      c0 to card$
.         MOVE      STR2 TO card$
.....................
.begin patch 2.82
.START PATCH 2.83 REMOVED LOGIC
.               call           Trim using Commper
.END PATCH 2.83 REMOVED LOGIC
.end patch 2.82
.ARGH

 if (NDATCONV = "1")
.                   if (NDATEXCH <> "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"021XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
                                        if (NSELEXC <> "2")
                                                  move      C0,card$
                                                  move      NSELPRICE,card$
                                        endif
                              else
                                        goto DataCheckText
                              endif
.                   endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              move      NTXTTEXT,text1
                              SCAN      "EXCHANGE ONLY" IN TEXT1
                              RETURN    IF EQUAL                 NO USABLE $ RETURN
                              RESET     TEXT1
                              SCAN      "$" IN TEXT1
                              RETURN    IF NOT EQUAL        NO USABLE $ RETURN
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1
                              move      c0 to card$
                              MOVE      STR2 TO card$
                              SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
                              RETURN    IF NOT EQUAL            *YES.
                              CLEAR     STR2
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1       *NO, NOW WE DO!
                              move      c0 to card$
                              MOVE      STR2 TO card$
                    else
                              clear     text1
                    endif
          endif
.END PATCH 2.7 REPLACED LOGIC
.         DISPLAY   *P1:24,STR2,B1,card$,*W2;
         RETURN
RENT
         cmatch    yes to specl
         if        equal
         goto      ok
         endif

         CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE
.DLH 05Aug98 this section was missing Epsilon code
.and was using the variable form72 instead of unbilinc.   so rentals reflected AR instead of
.LR income on report details and totals.

        display     *p1:24,"brk",obrknum

         match      "0192" to obrknum
           IF         EQUAL
           display    *p1:24,*el,"EPSILON!!!!!!!!!"
.START PATCH 2.83 REMOVED LOGIC
.           match      " 20" to commper
.            if         equal
          if (COMMPER = "20")
.END PATCH 2.83 REMOVED LOGIC
            MULT      ".1" BY UNbilinc
            goto      ok
            ENDIF
.START PATCH 2.83 REMOVED LOGIC
.           match      " 30" to commper
.           if         equal
          if (COMMPER = "30")
.END PATCH 2.83 REMOVED LOGIC
           MULT      ".2" BY UNbilinc
.START PATCH 2.83 REMOVED LOGIC
.           display    *p1:24,*el,"Unbilinc ",Unbilinc,b1,commper
          move      COMMPER,str6
           display    *p1:24,*el,"Unbilinc ",Unbilinc,b1,str6
.END PATCH 2.83 REMOVED LOGIC
           goto      ok
           ENDIF
.oddball use 10% commission
          MULT      ".1" BY Unbilinc
          goto      ok
          endif

.START PATCH 2.83 REPLACED LOGIC
.         move       c0 to n32      .DLH USE Datacard info 09Jul98
.         move       commper to n32
.         mult       ".01" by n32
.         mult       n32,unbilinc
.................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,unbilinc
.END PATCH 2.83 REPLACED LOGIC
.         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
         ENDIF
.
OK       noreturn
.         display   *p1:24,*el,"OK ",olrn,unbilinc,b1,unbilamt,*w;
         COMPARE   SIX TO SALESNUM
         GOTO      MANAGE2 IF EQUAL
         COMPARE   "19" TO SALESNUM
         GOTO      MANAGE2 IF EQUAL
         COMPARE   C0 TO SALESNUM
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      "E" TO TIPE
         ELSE
         GOTO      MANAGE2
         ENDIF
         ENDIF
         MOVE      B TO SOURCE
         PACK      NBILFLD FROM omlrnum,z3,c0
         CALL      READBLTO
         move      sysmo to invdtem
         move      sysyr to invdtey
         GOTO      writerec
MANAGE2
         MOVE      M TO SOURCE
         CALL      OWNPREP
         CALL      READOWN
         move      sysmo to invdtem
         move      sysyr to invdtey
         goto      writerec
.
MLRREAD  MOVE      C1 TO NMLRPATH
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CMATCH    "B" TO MCODE        .BATCH BILL ?
         IF         NOT EQUAL         .NO
         cmatch    "A" to mcode
         IF         NOT EQUAL         .NO
         CLEAR      MCODE         .CLEAR VAR.
         ENDIF
         endif
         RETURN
.
.
READSHIP
         MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
         RETURN    IF OVER

.START PATCH 2.5 - REPLACED LOGIC
.         MOVE      C0 TO N7
.         MOVE      SQUANT TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO N9
         MOVE      SQUANT TO N9
         COMPARE   C0 TO N9
.END PATCH 2.5 - REPLACED LOGIC
         IF        NOT EQUAL
.START PATCH 2.5 - REPLACED LOGIC - CODE UN-REMMED
..Start Patch #2.4 - replaced logic until SQUANT is increased
         MOVE      SQUANT TO OQTY
.         PACK       OQTY,"00",SQUANT
..end Patch #2.4 - replaced logic until SQUANT is increased
.END PATCH 2.5 - REPLACED LOGIC - CODE UN-REMMED
         endif
         RETURN
READRTN  MATCH     ORTNNUM TO NRTNFLD
         IF        NOT EQUAL
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         ENDIF
         RETURN
READMRG
         MOVE      OLRN TO NMRGFLD
         rep       zfill in nmrgfld
         MOVE      C1 TO NMRGPATH
         CALL      NMRGKEY
         RETURN    IF OVER
         move      nmrgiqty to oqty
         RETURN
special
         move       no to specl                   .09Apr98 DLH default answer is no

.start patch 2.84
.         MATCH     "0020" TO OMLRNUM
.         IF         EQUAL
..         MOVE       C5 TO FORM52
.         move       yes to specl
.         return
.         ENDIF

.         MATCH     "0179" TO OMLRNUM
.         IF         EQUAL
.         MOVE       C5 TO FORM52
.         move       yes to specl
.         return
.         ENDIF
.end patch 2.84

         MATCH     "0188" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         match      "0171" to obrknum
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF
.
         return
.
*............................................................
*......................................................................
EOJ
         close    output
         compare  c3 to pass
         if       equal
.Patch2.81
         MOVE      "NINVLAST" TO GNXTFLD
           CALL      GNXTKEY
           MOVE      GNXTNUM TO lastinv
         MOVE     "NINCLAST" TO GNXTFLD
            CALL     GNXTKEY
            MOVE     lastinv TO GNXTNUM
            CALL     GNXTUPD
.Patch2.81
         STOP
         endif
         goto    unbilled
.
OWNPREP
         REP       ZFILL IN NOWNFLD
         READ      DUPEOWN,NOWNFLD;NOWNFLD,DUPE1,NEWOLON
         IF        NOT OVER
         MOVE      NEWOLON TO NOWNFLD
         ELSE
         MOVE      OLON TO NOWNFLD
         ENDIF
         RETURN

debugger
          return

.begin patch 2.5
         INCLUDE   COMPUTE.inc
         include   nacdio.inc
.end patch 2.5
         include   nshpio.inc
         include   ndatio.inc
         include   nrtnio.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NORDIO.inc
.Patch2.8
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch2.8
         INclude        NInvAcdio.inc
         INCLUDE   NINVIO.inc
.begin patch 2.6
         INCLUDE   NJSTIO.inc
.end patch 2.6
         INCLUDE   NBILIO.inc
         INCLUDE   GNXTIO.INC
         INCLUDE   NDAT3IO.INC
         include   nmrgio.inc
.Patch2.8
.         include   nbrkio.inc
.Patch2.8
.START PATCH 2.7 REPLACED LOGIC
          INCLUDE   NSELIO.INC
          INCLUDE   NTXTIO.INC
          include   nsel2io.inc
          include   nmodio.inc
.END PATCH 2.7 REPLACED LOGIC
   INCLUDE   COMLOGIC.inc

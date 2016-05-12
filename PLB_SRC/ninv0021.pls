*                             ====== ==== ======
*                       ***   SOURCE CODE LISTING   ***
*                             ====== ==== =======
*
*                    - Nin Cal.  AUTO RUNNING CHARGE ORDER/INVOICE PROGRAM -
*
*******************************************************************************
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC         
.  
Release   Init      "1.76"     DH More use of the data manager
Reldate   INIt      "31 May 2012"

.Release   Init      "1.75"     DH use date from dsprog.dat to get EOM date
.Reldate   INIt      "23 January 2010"
.Release   Init      "1.74"     DH Internal INDex
.Reldate   INIt      "23 APril 2008"
.Release       Init           "1.73"      30Apr2007 JD write COMPID from Company var read not last order
.release         init               "1.74"   27FEB2007 JD New Running charge list
.release         init               "1.73"   21JUN2006 DMS      Fulfillment Conversion
.release        init           "1.72"   AUG312006 JD new added new fulfill lists.
.release        init           "1.71"   28NOV2005 JD PLB 9.0 new index
.release        init           "1.7"        DMB 18JUN2005 Changed IP address of FIle manager
.release        init           "1.6"        ASH   18MAY2005 LISTMLR Conversion
.release        init           "1.5"        DLH   8March2005          Invoice Conversion  
.release  init      "1.42"        DMB   26MAY2004 Mailer Conversion  
.Release    init      "1.41"          JD26MAY2004  Added hpreset to header.
.Release    init      "1.40"          JD  16MAY2003      added new fulfill list.
.Release    init      "1.36"         ASH 04FEB2002          NINFUL CONVERSION
.Release    init      "1.35"         jd20NOV2001 tightened up code on date sub
.Release    init      "1.32"         jd28SEP2001 added ninvoice to FM.
.Release    init      "1.31"         jd  23MAR2001 CLEARING MORE ORD VARS.
.Release    init      "1.3"         ASH 02OCT2000 NEW SERVER ADDED
.Release    init      "1.2"         ASH 08Sep99 EXCHARGE File Conversion
.Release    init      "1.1"         DLH 5May99 NININV Y2K
.Release    init      "1.0"         JD 01FEB99 new
         include   xMGtdd.inc
.Patch1.42
                              include   compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.INC
.Patch1.42
         INCLUDE   NORDDD.INC
         INCLUDE   NOWNDD.INC
         INCLUDE   NXRFDD.INC             
         INCLUDE   NMTXDD.INC
.Patch1.42
.         include   nbrkdd.inc
.Patch1.42

.begin patch 1.5
.               include   ninvdd.inc
               include        ninvdd.inc
               include        NInvacddd.inc
.end patch 1.5
         include   gnxtdd.inc
         include   ndatdd.inc
         inc       hp.inc
.START PATCH 1.72 REMOVED LOGIC
..START PATCH 1.36 ADDED LOGIC
.         include   nfuldd.inc
..END PATCH 1.36 ADDED LOGIC
.END PATCH 1.72 REMOVED LOGIC
.ORDPRINT IFILE     KEYLEN=6,FIXED=582   .Order Print file
.ORDPRINT IFILE     KEYLEN=6,FIXED=696   .Order Print file
.>Patch 1.7 Begin
.ORDPRINT IFILE     KEYLEN=6,FIXED=696,Name="NINPRINT.isi|20.20.30.103:502"
ORDPRINT IFILE     KEYLEN=6,FIXED=696,Name="NINPRINT.isi|NINS1:502"
.end patch 1.7
EXPRINT  IFILE     KEYLEN=20,STATIC=5
.begin patch 1.1
.'NVOICE IFILE     KEYLEN=6,FIX=403     .iNVOICE pRINT FILE
.begin patch 1.5
.PINVOICE IFILE     KEYLEN=6,FIX=403,Name="NINVOICE.isi|20.20.30.103:502"     
.end patch 1.5
.PINVOICE IFILE     KEYLEN=6,FIX=305     .iNVOICE pRINT FILE
.end patch 1.1
.tEMP     FILE                           .TEMP WORK FILE
.input    file                           .additional handle for master file
REcsin   form      5                    .Records in
recsslct form      5                    .Records Selected
billed   form      5
daternge form      5                    .Date of records wanted Julian
.START PATCH 1.2 - INCREASED VAR
.form62   form      6.2
.form74   form       7.4
form92A  form      6.2
form94A  form      9.4
.END PATCH 1.2 - INCREASED VAR
form92   form      9.2
permask  init      "$$9.99"
EXAR     dim       10
ARMask   init      "$$$,$$9.99"
TOtar    form      6.2
totqty   form      7
qtymask  init      "Z,ZZZ,Z99"
qtyout   dim       9
GTOtar   form      7.2
EXGAR     dim       12
ARGMask   init      "$,$$$,$$9.99"
Gtotqty  form      8
Gqtymask  init      "ZZ,ZZZ,Z99"
Gqtyout   dim       10
SPCL7    DIM       2
SPCL8    DIM       2
SPCL9    DIM       2
DESC0L1  DIM       47                    .'SPEC INSTRUC-DESCRIP'
DESC0L2  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC991  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC992  DIM       47                    .SPEC INSTRUC-DESCRIP
DESC981  DIM       47                    .SPEC INSTRUCTION DESc
DESC982  DIM       47                    .SPEC INSTRUCTION DESC
Hown     dim       4
Holdlist dim       6
holdlr   dim       6
holdrate form      3.2
Month    dim        9
.
NOFILE   DIM       8
DISPLR   INIT      "(1) Lr "
DASH6    INIT      "______"
DISPM    INIT      "(2) Mailer "
DASH4    INIT      "____"
DISPL    INIT      "    List # "
DISPOWN  INIT      "(3) Owner # "
DISPTI   INIT      "(4) TRIPLEX INV # "
DASH5    INIT      "_____"
DISPQ    INIT      "(5) Qty. Shipped "
DASH7    INIT      "_______"
DISPAR   INIT      "(6) AR$ "
DASH10   INIT      "___________"
DISPAP   INIT      "(7) AP$ "
DISPIN   INIT      "(8) LR IN  "
TLINE    INIT      "========================="
TDISP    INIT      " Total __________"
DASH3    INIT      "___"
FILL38   INIT      "                                     "
C501     DIM           6            .PRINTS 501C STATUS
. ...........
* Excharge File
LR       DIM       6          KEY= LR Position 1-6
MLR      DIM       4
MLRCNT   DIM       3          MAILER/CONTACT NUMBER.
.START PATCH 1.2 - INCREASED VAR
.QTY      FORM      7
QTY      FORM      9
.END PATCH 1.2 - INCREASED VAR
APrun    FORM      8.2
AT1      FORM      8.2
ARrun    FORM      8.2
AP1run   FORM      8.2
.START PATCH 1.2 - ADDED VAR
CE       DIM       2
.END PATCH 1.2 - ADDED VAR
. .......
form122  form      12.2
TDMCAMT  FORM       8.2
...
BRANCH     FORM        1
.BLANK2   DIM       2
.MLRKEY   DIM       7
.BLANK34  DIM       34
.BLANK52  DIM       52
.BLANK75  DIM       73
.BLANK6   DIM       6
.BLANK9   DIM       9
LN       DIM       6          LIST NUMBER
HLN      DIM       6          HOLD LIST NUMBER
CONTACT  DIM       3
OWNER    DIM       4
HOWNER   DIM       4
TPI      DIM       6          invoice #
LDESC    DIM       35
.BLANK    DIM       1
YR       DIM       2
DATE     DIM       8
MO       DIM       2
PMO      DIM       2
PYR      DIM       2
PDD      DIM       2
.START PATCH 1.2 - ADDED VAR
PCE      DIM       2
.END PATCH 1.2 - ADDED VAR
RKEY      DIM       22
KEY      DIM       6
acharge  dim       1
.START PATCH 1.2 - INCREASED VAR
.REPAR    FORM      8.2
REPAR    FORM      9.2
.END PATCH 1.2 - INCREASED VAR
LAR      FORM      8.2         LIST TOTALS
.START PATCH 1.2 - INCREASED VAR
.LSH      FORM      7           LIST TOTALS
LSH      FORM      9           LIST TOTALS
.END PATCH 1.2 - INCREASED VAR
LAP      FORM      8.2         LIST TOTALS
LINC     FORM      8.2         LIST TOTALS
LrAR      FORM      8.2         LIST rental TOTALS
.START PATCH 1.2 - INCREASED VAR
.LrSH      FORM      8           LIST rental TOTALS
LrSH      FORM      9           LIST rental TOTALS
.END PATCH 1.2 - INCREASED VAR
LrAP      FORM      8.2         LIST rental TOTALS
LrINC     FORM      8.2         LIST rental TOTALS
LeAR      FORM      8.2         LIST exchange TOTALS
.START PATCH 1.2 - INCREASED VAR
.LeSH      FORM      8           LIST exchange TOTALS
LeSH      FORM      9           LIST exchange TOTALS
.END PATCH 1.2 - INCREASED VAR
LeAP      FORM      8.2         LIST exchange TOTALS
LeINC     FORM      8.2         LIST exchange TOTALS
LeARold   FORM      8.2         LIST exchange TOTALS order date pre 98
.START PATCH 1.2 - INCREASED VAR
.LeSHold   FORM      8           LIST exchange TOTALS order date pre 98
LeSHold   FORM      9           LIST exchange TOTALS order date pre 98
.END PATCH 1.2 - INCREASED VAR
LeAPold   FORM      8.2         LIST exchange TOTALS order date pre 98
LeINCold  FORM      8.2         LIST exchange TOTALS order date pre 98
powner    dim       4
exchrent dim       4
exchflag form      1
OAR      FORM      8.2         OWNER TOTALS
.START PATCH 1.2 - INCREASED VAR
.OSH      FORM      8           OWNER TOTALS
OSH      FORM      9           OWNER TOTALS
.END PATCH 1.2 - INCREASED VAR
OAP      FORM      8.2         OWNER TOTALS
OINC     FORM      8.2         OWNER TOTALS
GAR      FORM      8.2         GRAND TOTALS
.START PATCH 1.2 - INCREASED VAR
.GSH      FORM      8           GRAND TOTALS
GSH      FORM      9           GRAND TOTALS
.END PATCH 1.2 - INCREASED VAR
GAP      FORM      8.2         GRAND TOTALS
GINC     FORM      8.2         GRAND TOTALS
LKEY     DIM       6           LIST KEY
ONAME    DIM       25
.BLANK31  DIM       31
LINES    FORM      "00"
.BLANK25  DIM       25
ROLLBR   FORM      1          HOLDS ONE IF ROLLOUT OCCURRED.
ROLLFILE INIT      "EXCHARGE/ROLL"
form94   form     9.4
form104  form     10.4
strtdate form     5               .starting date for management Run fees
qtyship  form     7
PerM     form      3.2
................................................................................................
.Main
.begin patch 2.75
.         move      "NINV0021" to program
         MOVE      "Running CHARGES" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         MOVE      C1 TO NDATPATH         .SET ACCESS TO ISAM KEY List number
         MOVE      C1 TO NORDPATH         .SET ACCESS TO ISAM KEY
         move      c2 to ninvpath         .SET ACCESS TO ISAM KEY INVoice number
.set up input file, verify date being run
.         clock     date to today
.         unpack    today into mm,str1,dd,str1,yy
..begin patch xxx
.         unpack    today into Pmo,str1,Pdd,str1,Pyr
.         move       cc,PCe
..end patch xxx
         
.begin patch 2.75
         clock     timestamp to timestamp
         unpack    timestamp into cc
          match     "NINV0021",PROGRAM
          if        equal
          unpack    Today into MM,str1,dd,str1,yy
          unpack    today into Pmo,str1,Pdd,str1,Pyr
          Else
         move      "NINV0021" to program
         clock     date to today
         unpack    today into mm,str1,dd,str1,yy
          unpack    today into Pmo,str1,Pdd,str1,Pyr
          endif
         move       cc,PCe
.end patch 2.51

         call      paint
         call      funcdisp 
.Temporary - open fake main files
.        Order, Invoice, Order Print, INvoice Print, Gnxt
      

.begin patch 1.5
.        OPEN       PINVOICE,"NINVOICE"          .Print file for NINca invoices
.end patch 1.5
         open       ordprint,"ninprint|NINS1:502"          .print file form nin orders
.START PATCH 1.3 REPLACED LOGIC
.         splopen    "g:\data\runFee.lst"
         PACK      STR35,NTWKPATH1,"RUNFee.lst"
         splopen   STR35
.END PATCH 1.3 REPLACED LOGIC
.         prepare   temp,"g:\data\exchTemp.dat"

................................................................................................
.MainLoop - read file till done             

...............................................................................................
.Detail: print Details
         move       c1 to nordpath
         MOVE      "EXPRINT",NOFILE
.
.* 59-60= YR            2
.* 61-62 = MO           2
.* 20-23 = OWNER        4
.* 14-19 = LIST NO      6
.* 7-10 = MLR            4
.* 1-6    lr             6   = 20   mlr not used anymore 2/98 DLH
.         DISPLAY   *P35:12,*EL,"ROLLOUT FINISHED.....THANK YOU",*B,*W2:
.START PATCH #1.2 - REPLACED LOGIC
.          pack      taskname from "f:\netutils\sunIDXNT g:\DATA\TEXT\EXCHARGE.DAT":
.                       ",g:\DATA\EXPRINT ":
.                   "-59-60,61-62,20-23,14-19,1-6"
.START PATCH 1.3 REPLACED LOGIC
.          pack      taskname from "f:\netutils\sunIDXNT g:\DATA\TEXT\EXCHARGE.DAT":
.                       ",g:\DATA\EXPRINT ":
.                   "-61-64,65-66,20-23,14-19,1-6"
.          pack      taskname from NTWKPATH2,"sunIDXNT ",NTWKPATH1,"TEXT\EXCHARGE.DAT,":
.begin patch 1.74
.begin patch 1.71
.          pack      taskname from "\\nts0\c\apps\plb\code\sunindex ",NTWKPATH1,"TEXT\EXCHARGE.DAT,":
.end patch 1.71
          pack      taskname from NTWKPATH1,"TEXT\EXCHARGE.DAT,":
                    NTWKPATH1,"EXPRINT ":
                   "-61-64,65-66,20-23,14-19,1-6"
.END PATCH 1.3 REPLACED LOGIC
.END PATCH #1.2 - REPLACED LOGIC
          INDEX     taskname
.          execute   taskname
.end patch 1.74
.         MOVE      C1 TO ROLLBR
.         GOTO      START1
.
*  PRINT DELIMITER PMO,PYR
.
SPOOL
.START PATCH 1.3 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\runFEE.LST","Q"
         PACK      STR35,NTWKPATH1,"RUNFee.lst"
         SPLOPEN   STR35,"Q"
.begin patch xxx
         goto       Date1
.end patch xxx
.END PATCH 1.3 REPLACED LOGIC
DATE
         MOVE      "0",LINES
.START PATCH 1.2 - ADDED VAR
.         KEYIN     *P01:22,"LAST WORKING DAY OF PREVIOUS....MM/DD/YY":
.                   *P33:22,*JR,*ZF,PMO,*P36:22,*JR,*ZF,Pdd,*P39:22,*JR,*ZF,Pyr;
.         DISPLAY   *P33:22,PMO,*P36:22,PDD,*P39:22,PYR
         KEYIN     *P01:22,"LAST WORKING DAY OF THE MONTH....MM/DD/YYYY":
                   *P34:22,*JR,*ZF,PMO,*P37:22,*JR,*ZF,Pdd,*P40:22,*JR,*ZF,*+,PCE,*JR,*ZF,*-,Pyr;
.begin patch xxx
Date1         DISPLAY   *P34:22,PMO,*P37:22,PDD,*P40:22,PCE,PYR
.end patch xxx
.END PATCH 1.2 - ADDED VAR
         KEYIN     *P76:22,"OK?",STR1;
         cmatch    "*" to str1
         goto      eoj2 if equal
.begin patch 1.35
         cmatch    b1 to str1
         goto      date if eos
         cmatch    yes to str1
         goto      date if not equal
.end patch 1.35
.         CMATCH    "Y",STR1
.         GOTO      DATE IF NOT EQUAL
INTERNAL
         DISPLAY   *P01:22,*EL,"THANK YOU .......";
         REPLACE   ZFILL,PMO
         REPLACE   ZFILL,PYR
         REPLACE   ZFILL,PDD
.START PATCH 1.2 - ADDED VAR
         REPLACE   ZFILL,PCE
.END PATCH 1.2 - ADDED VAR
.         PACK      KEY FROM PYR,PMO,FILL38
.          filepi    1;exprint
.START PATCH 1.3 REPLACED LOGIC
.         OPEN      EXPRINT,"g:\data\EXPRINT"
.begin patch 1.76
         PACK      STR55,NTWKPATH1,"ExPRINT|NINS1:502"
         OPEN      EXPRINT,STR55
.end patch 1.76
.END PATCH 1.3 REPLACED LOGIC
.>         READ      EXPRINT,KEY;;
...............................................................................
PRT1     filepi    1;exprint
.START PATCH #1.2 - REPLACED LOGIC
.         READKS    EXPRINT;LR,MLR,MLRCNT,LN,OWNER,TPI,QTY,ARrun,APrun,YR,MO
         READKS    EXPRINT;LR,MLR,MLRCNT,LN,OWNER,TPI,QTY,ARrun,APrun,CE,YR,MO
.END PATCH #1.2 - REPLACED LOGIC
         GOTO      EOJ2 IF OVER
         ADD       C1 TO N5
         if         (lr = "731547")
         call       debug
         endif
         DISPLAY   *P10:12,"RECORDS IN ",N5
.updated logic 2016 March 1 DLH
         if        (pyr <> yr | PMO <> MO | PCE <> CE)
         goto         prt1
         endif
.         MATCH     PYR,YR
.         GOTO      PRT1 IF NOT EQUAL
.         MATCH     PMO,MO
.         GOTO      PRT1 IF NOT EQUAL
..START PATCH 1.2 - ADDED VAR
.         MATCH     PCE,CE
.         GOTO      PRT1 IF NOT EQUAL
.updated logic
.END PATCH 1.2 - ADDED VAR
goodone
          add       c1 to recsslct
         display   *p10:13,"Records Selected : ",recsslct,b2,xmgtlr
         compare    c1 to recsslct           .1st good record?
         if         equal            .yes not a break,just set up the goodies. 
         call       OH
         call       LDESC
         CALL      LHEAD
         CALL      DETAIL
           goto     prt1
         endif
         compare   ".00" to arrun
         goto      prt1 if equal
        MATCH     OWNER,HOWNER
         GOTO      BREAK1 IF NOT EQUAL
         MATCH     LN,HLN
         GOTO      BREAK2 IF NOT EQUAL
         CALL      DETAIL
         GOTO      PRT1
. ............................................................................
BREAK1   COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
* SUBROUTINE OWNER TOTALS COMMENTED OUT APRIL 4 1982 AS PER S.A.
.        CALL      OTOT
.        COMPARE   "50",LINES
.        CALL      NEWPAGE IF NOT LESS
.
         CALL      OH
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LDESC
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LHEAD
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      DETAIL
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         GOTO      PRT1
.
. ............................................................................
BREAK2
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LTOT
.
         CALL      OH
         CALL      LDESC
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LHEAD
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      DETAIL
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         GOTO      PRT1
. ............................................................................
* LIST TOTALS
LTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.START PATCH 1.2 - REPLACED LOGIC
.         PRINT *L,*L,*L,*3,"LIST *":
.               *L,*13,"--------",*65,"-------",*95,"---------":
.                   *115,"--------":
.               *L,*1,"Exchange*",*10,LeAR,*64,LeSH,*93,LeAP,*112,LeINC:
.               *L,*13,"--------",*65,"-------",*95,"---------":
.                   *115,"--------":
.               *L,*1,Hpbon,"Exchange*",*10,LeAR,*64,LeSH,*93,LeAP,*112,LeINC:
.               *L,*13,"--------",*65,"-------",*95,"---------":
.                   *115,"--------",hpboff:
.               *L,*2,"Rental*",*10,LrAR,*64,LrSH,*93,LrAP,*112,LrINC:
.               *L,*13,"--------",*65,"-------",*95,"---------":
.               *L,*13,"--------",*65,"-------",*95,"---------":
.                   *115,"--------":
.               *L,*3,"TOTAL*",*10,LAR,*64,LSH,*93,LAP,*112,LINC
         PRINT *L,*L,*L,*3,"LIST *":
               *L,*13,"--------",*67,"-------",*99,"---------":
                   *115,"--------":
               *L,*1,"Exchange*",*10,LeAR,*65,LeSH,*97,LeAP,*112,LeINC:
               *L,*13,"--------",*67,"-------",*99,"---------":
                   *115,"--------":
               *L,*1,Hpbon,"Exchange*",*10,LeAR,*65,LeSH,*97,LeAP,*112,LeINC:
               *L,*13,"--------",*67,"-------",*99,"---------":
                   *115,"--------",hpboff:
               *L,*2,"Rental*",*10,LrAR,*65,LrSH,*97,LrAP,*112,LrINC:
               *L,*13,"--------",*67,"-------",*99,"---------":
               *L,*13,"--------",*67,"-------",*99,"---------":
                   *115,"--------":
               *L,*3,"TOTAL*",*10,LAR,*65,LSH,*97,LAP,*112,LINC
.END PATCH 1.2 - REPLACED LOGIC
.START PATCH 1.36 REPLACED LOGIC
.         RESET     OWNCTN
.        SCAN      "TDMC" IN OWNCTN          *TRIPLEX CCTO?
.        if        equal
.        move     "005051" to olnum
.        move     "1883" to olon
.        move     "TDMC Running Charges for Exchanges" to o1des
.        else
.        move     "016909" to olnum
.        move     "4600" to olon
.        move     "The Fide Group Running Charges/Exch" to o1des
.        endif
........
          call      Trim using OWNCTN
.START PATCH 1.72 REMOVED LOGIC
.         if (OWNCTN <> "")
.                   pack      NFULFLD,OWNCTN
.                   rep       zfill,NFULFLD
.                   move      C1,NFULPATH
.                   move      "LTOT-NFULKEY",Location
.                   pack      KeyLocation,NFULFLD
.                   call      NFULKEY
.         else
.                   clear     NFULFLD
.                   clear     NFULCOMP
.         endif
..Begin patch 1.40
.         if (NFULFLD = "0032")
.                   move      "021302",olnum
.                   move      "5654",olon
.                   move      "Data Mngmt Inc Running Charges/Exch",o1des
.                            goto          fulfxit
.              endif
..end patch 1.40
.         if (NFULFLD = "0026")
.                   move      "005051",olnum
.                   move      "1883",olon
.                   move      "TDMC Running Charges for Exchanges",o1des
.         else
.                   scan      "TDMC",NFULCOMP
.;

.         if (OWNCTN <> "")
.                   pack      COMPFLD6,OWNCTN
.                   rep       zfill,COMPFLD6
.                   move      C1,COMPPATH
.                   move      "LTOT-COMPKEY6",Location
.                   pack      KeyLocation,COMPFLD6
.                   call      COMPKEY6
.                   if over
.                             clear     COMPFLD6
.                             clear     COMPCOMP
.                   else
.                             if (COMPSVBFLG <> "T")
.                                       clear     COMPFLD6
.                                       clear     COMPCOMP
.                             endif
.                   endif
.         else      // OWNCTN = ""
.                   clear     COMPFLD6
.                   clear     COMPCOMP
.         endif
.;
        call    trim using datful
                    if (DATFUL <> "")
                              pack      COMPFLD,DATFUL
                              rep       zfill, COMPFLD
                              move           C1,COMPPATH
                              move      "Verify-COMPKEY",Location
                              pack      KeyLocation,COMPFLD
                              call      COMPKEY
                              if over
                              clear     COMPCOMP
                              else
                                        if (COMPSVBFLG <> "T")
                                        clear     COMPCOMP
                                        endif
                              endif
                    else      // datful = ""
                              clear     COMPCOMP
                    endif

.Begin patch 1.72
.         if (COMPFLD6 = "0034")
.                   move      "022189",olnum
.                   move      "5882",olon
.                   move      "PIDI/Virginia Running Charges/Exch",o1des
.                            goto          fulfxit
.              endif
.         if (COMPFLD6 = "0048")
.                   move      "022190",olnum
.                   move      "5883",olon
.                   move      "MMI Direct/Running Charges/Exch",o1des
.                            goto          fulfxit
.              endif
.         if (COMPFLD6 = "0030")
.                   move      "022191",olnum
.                   move      "5884",olon
.                   move      "Frontline Data Group-Running Charges/Exch",o1des
.                            goto          fulfxit
.              endif


          if (COMPNUM = "009429")
                    move      "022189",olnum
                    move      "5882",olon
                    move      "PIDI/Virginia Running Charges/Exch",o1des
                            goto          fulfxit
              endif
          if (COMPNUM = "009428")
                    move      "022190",olnum
                    move      "5883",olon
                    move      "MMI Direct/Running Charges/Exch",o1des
                            goto          fulfxit
              endif
          if (COMPNUM = "009410")
                    move      "022191",olnum
                    move      "5884",olon
                    move      "Frontline Data Group-Running Charges/Exch",o1des
                            goto          fulfxit
              endif
.Start patch 1.74
          if (COMPNUM = "009407")
                    move      "023069",olnum
                    move      "5922",olon
                    move      "MKTG Services-Running Charges/Exch",o1des
                            goto          fulfxit
              endif
.End patch 1.74
.e.end patch 1.73   
          
          if (COMPNUM = "009412")
                    move      "021302",olnum
                    move      "5654",olon
                    move      "Data Mngmt Inc Running Charges/Exch",o1des
                            goto          fulfxit
              endif
          if (COMPNUM = "009406")
                    move      "005051",olnum
                    move      "1883",olon
                    move      "TDMC Running Charges for Exchanges",o1des
          else
                    scan      "TDMC",COMPCOMP
.PATCH 1.73 REMOVED LOGIC
                    if equal 
                              move      "005051",olnum
                              move      "1883",olon
                              move      "TDMC Running Charges for Exchanges",o1des
                    else
                              move      "016909",olnum
                              move      "4600",olon
                              move      "The Fide Group Running Charges/Exch",o1des
                    endif
          endif
.END PATCH 1.36 REPLACED LOGIC
fulfxit
.START PATCH 1.6 REPLACED LOGIC
.         move     NXRFMLR to omlrnum
          move      NXRFMLR,COMPFLD
          move      "fulfxit-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          move      COMPOLDMLR,omlrnum
.END PATCH 1.6 REPLACED LOGIC
         clear    mkey
         pack     mkey from omlrnum,z3
         clear    mbrknum
         move       c0 to osales10
         move       c0 to osales
         clear     obildrct
         call     nmlrkey
.Begin patch 1.2
         match    yes to mbildrct
         if       equal
         move     yes to obildrct
         endif
.End patch 1.2
         UNPACK    MSLSPER INTO OSALES10,OSALES         .DLH 29sep98
         match    "0000" to mbrknum
         goto     skipbrk if equal
         type     mbrknum
         if       equal
         pack     nbrkfld from mbrknum
         call     nbrkkey
         UNPACK    BRSALES INTO OSALES10,OSALES        .IF EXISTS USE IT.
         else
         UNPACK    MSLSPER INTO OSALES10,OSALES
         endif         
skipbrk
         move     z3 to ocobn
         clear    OMLRPON
         move     "S" to orcode
         move     "B" to ostat
.Start patch #1.1 - increase string to fill increased OQTY
.         move      "      0" to oqty
         move      "        0" to oqty
.End patch #1.1 - increase string to fill increased OQTY
         clear     OMLRKY
         move      "   00" to oppm
.Start patch #1.1 - increase string to fill increased VAR
.         move      b1 to ofocode
         move      b2 to ofocode
.End patch #1.1 - increase string to fill increased VAR
         clear     ortndtem
         clear     ortndted
         clear     ortndtey
.Start patch #1.1 - clear new var
         clear     ortndtec
         move      "00",omdtec
         clear     oodtecoc
.End patch #1.1 - clear new var
         clear     ortndtem
         clear     ortndted
         clear     ortndtey
.Start patch #1.1 - clear new var
         clear     ortndtec
.START PATCH 1.2 - REPLACED LOGIC
.         move      cc,omdtec
         move      PCE,omdtec
.END PATCH 1.2 - REPLACED LOGIC
         clear     oodtecoc
.End patch #1.1 - clear new var
         move      PMo,omdtem
         move      PDD,omdted
         move      PYR,omdtey
         clear     otocode
         clear     osotcode
         clear     occode
         clear     olrnco
         clear     oodtecom
         clear     oodtecod
         clear     oodtecoy
         clear     oqtyco
         clear     ospi
         clear     obrkguar
         clear     oelcode
         clear     oodnum
         clear     ohist
         clear     oclrinit
         clear     orent
         clear     OBRKRPT
         clear     OCLRDTEC
         clear     OCLRDTEY
         clear     OCLRDTEM
         clear     OCLRDTED
.Start patch #1.31 - clear new varS
         CLEAR     OCAMP
         CLEAR     OCLRINIT
         CLEAR     OCLRSTAT
.End patch #1.31 - clear new varS
.offer ????what to do
         append    omlrnum to oodnum
         append    "001" to oodnum
         reset      oodnum
         clear      oodes        .offer desc
         move      "0269" to ortnnum
         clear      otaperet
         clear      ouqty
.Start patch #1.1 - increase string to clear increased var
.         move       b1 to ococode
         move       b2 to ococode
.End patch #1.1 - increase string to clear increased var
         unpack     today into oodtem,str1,oodted,str1,oodtey
.Start Patch #1.1 - pack century
         move       cc,oodtec
.End Patch #1.1 - pack century
         clear      oscode
         clear      ocomslct
         clear      oshp
         clear      o2des
         clear      oreuse
         move       "ARB" to odowj
         clear      oexqty
         clear      guarcode
         clear      obrknum
         clear      obrkcnt
         unpack     mbrknum into obrknum,obrkcnt
         clear      osamcde
         clear      onetper
         move       c0 to onetrc
         clear      onetfm
         move       c0 to onetmin
         clear      spcl7
         clear      spcl8
         clear      spcl9
         clear      DESC0L1
         clear      DESC0L2
         clear      desc991
         clear      desc992
         clear      desc981
         clear      desc982

.Start patch #1.73
          clear       OcompID
          Move      CompExcl,OCompID
.End patch #1.73
         MOVE     "NORDNXT" TO GNXTFLD
         CALL     GNXTKEY
         MOVE     GNXTNUM TO N6
addord   ADD      C1,N6
         MOVE      N6 TO GNXTNUM
         REP       ZFILL IN GNXTNUM
         CALL      GNXTUPD
        move      n6 to nordfld
         move      n6 to olrn
         call      nordtst
         goto      addord if not over
JD1
.make sure we pass data for all keys!!!!!

         call      nordwrt
         FILEPI    1;ORDPRINT
         WRITE     ORDPRINT,olrn;ordvars:
                   SPCL7,DESC0L1,DESC0L2:
                   SPCL8,DESC991,DESC992,SPCL9,DESC981,DESC982
         move      "F" to CODE
         move      c0 to STATB   
         move      omlrnum to MLRN    
         move      olrn to LRN     
         move      c0 to BILLTN  
         move      c0 to PAYTN   
         CLEAR     LOINVN  
         MOVE      C0 TO AP1     
         MOVE      C0 TO COMMPCT 
         MOVE      C4 TO PAYCODE                 .all to lr income
.begin patch 1.1
.         move      lsh to qtyshp
         move      lsh to qtyin
         move      lsh to qtybild
.end patch 1.1
         move      c0 to repar
.START PATCH 1.2 - INCREASED VAR
.         move      c0 to form62
.         move       c0 to form74
.         move       lsh to form74
.         mult      "3.90" by form74
.         divide     "1000" into form74
.         add       form74 to form62
.         move      lar to repar
.         sub       form62 from repar
....
         move      c0 to form92A
         move       c0 to form94A
         move       lsh to form94A
         mult      "3.90" by form94A
         divide     "1000" into form94A
         add       form94A to form92A
         move      lar to repar
         sub       form92A from repar
.END PATCH 1.2 - INCREASED VAR
         compare   c0 to repar
         if        not equal
         move      yes to acharge
         else
         move      no to acharge
         endif
         move      "003.90" to ppm
         MOVE      "000" TO COBN                            
         clear     INCC    
         move      olon to LON
         CLEAR      CHKN2   
         clear      WSJPC   
         CLEAR      ADJC    
         CLEAR      CHKN1   
.begin patch 1.1
.         CLEAR      CHKDTEM
.         CLEAR      CHKDTED
.         cLEAR      CHKDTEY
         CLEAR      CHKN2
         CLEAR      CHKN3
         CLEAR      CHK1DTEM
         CLEAR      CHK1DTED
         cLEAR      CHK1DTEY
         CLEAR      CHK2DTEM
         CLEAR      CHK2DTED
         cLEAR      CHK2DTEY
         CLEAR      CHK3DTEM
         CLEAR      CHK3DTED
         cLEAR      CHK3DTEY
.end patch 1.1
         CLEAR      LET90D  
         CLEAR      MLRPAYR 
         CLEAR      MLRPAYD 
         CLEAR      INVNUM  
         unpack     today into invdtem,str1,invdted,str1,invdtey
         move       cc to invdtec
         move       c0 to form92
.begin patch 1.1
.         add        lar to form92
.         MULT       "100" BY form92
.         move       c0 to n9
.         add        form92 to n9
.         MOVE       n9 TO AR
         add        lar to ar
.end patch 1.1
.         cmatch     yes to acharge
.         if         equal
.         move       c0 to form92
.         add        repar to form92
.begin patch 1.5
. .        mult       "100" by form92
.         move       c0 to n7
.         move        repar to n7
.         move       form92 to n7
.         CLEAR      ADDCHG1
.               Clear          STR15
.begin patch 1.1
.         pack       addchg1 from "88",n7," 100 "
.         pack       addchg1 from "088",n7," 100 "
.               pack           str15 from "088",n7," 100 "
.               UNpack         str15 into NinvAcdCode,str7,str1,str3,str1a           ;cheating could just move vars to new code
.               pack           Ninvacdnum from c0,C0,C1
.               Move           LSH to NINvAcdqty
.               MOve           "3.90" to NInvAcdrate
.               MOve           repar to NInvAcdrate
.               MOve           Invnum to NinvAcdINV
.               rep            Zfill in Ninvacdnum
.               rep            Zfill in NinvacdCode
.               rep            Zfill in NinvacdInv
.               packkey        NinvAcdFld from NinvAcdINv,NinvAcdNum
.               call           NinvAcdTst
.               IF             not over
.         pack      taskname,"Already have add chrg record for LR ## ",NINVFLD
.         alert     caution,taskname,result
.               endif
.               MOVE           C1 TO NinvAcdPErc
.               pack           Ninvacdanincd from b1,b1,b1
.               Move           "088" to NinvAcdCode
.               Move           NinvAcdCode to N3
.               Move           "m" to NINVacdRateT
.               Move           "f" to NINVacdRateT
..               Call           NInvAcdwrt
..end patch 1.1
.         else
.         clear      addchg1
.         endif
.         CLEAR      ADDCHG2 
.         CLEAR      ADDCHG3 
.         CLEAR      ADDCHG4 
.         CLEAR      ADDCHG5 
.         CLEAR      ADDCHG6 
.         CLEAR      ADDCHG7 
.         CLEAR      ADDCHG8 
.         CLEAR      ADDCHG9 
.         CLEAR      ADDCHG10
.end patch 1.5               
         CLEAR      GUARPAY 
.begin patch 1.1
.         CLEAR      SALES
         CLEAR      invSALES
.end patch 1.1
         MOVE        C0 TO AP2     
         MOVE       OBRKNUM TO IBRKNUM 
         MOVE       OBRKCNT TO IBRKCNT 
         CLEAR      IMLRCHK 
         MOVE       C0 TO IRCQTY  
         MOVE       C0 TO IREXQTY 
         MOVE       C0 TO iexPPM  
         MOVE       C0 TO irnetper
         MOVE       C0 TO inetrc  
         MOVE      "NINVNXT" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO n6
NINADD   MOVE      N6,NINVFLD
         ADD       "1",N6
         MOVE      C2 TO NINVPATH
         CALL      NINVTST
         GOTO      NINADD IF NOT OVER
         MOVE     ninvfld TO GNXTNUM
         CALL     GNXTUPD
         MOVE      NINVFLD TO INVNUM
         MOVE      C1 TO NINVPATH
         MOVE      OLRN TO NINVFLD
         REP       ZFILL IN INVNUM
         REP       ZFILL IN NINVFLD
JD3

         CALL       NINVWRT
         Move     "ARB" to Inits
.begin patch 1.5
.         Filepi    1;PINVOICE
.         WRITE     PINVOICE,NINVFLD;invvars,inits
          Call           Pinvwrt
.end patch 1.5
         cmatch     yes to acharge
         if         equal
.         move       c0 to form92
.         add        repar to form92
.begin patch 1.5
. .        mult       "100" by form92
.         move       c0 to n7
.         move        repar to n7
.         move       form92 to n7
.         CLEAR      ADDCHG1
               Clear          STR15
.begin patch 1.1
.         pack       addchg1 from "88",n7," 100 "
.         pack       addchg1 from "088",n7," 100 "
               pack           str15 from "088",n7," 100 "
.               UNpack         str15 into NinvAcdCode,str7,str1,str3,str1a           ;cheating could just move vars to new code
               pack           Ninvacdnum from c0,C0,C1
.               Move           LSH to NINvAcdqty
.               MOve           "3.90" to NInvAcdrate
               MOve           repar to NInvAcdrate
               MOve           Invnum to NinvAcdINV
               rep            Zfill in Ninvacdnum
               rep            Zfill in NinvacdCode
               rep            Zfill in NinvacdInv
               packkey        NinvAcdFld from NinvAcdINv,NinvAcdNum
               call           NinvAcdTst
               IF             not over
                    pack      taskname,"Already have add chrg record for LR ## ",NINVFLD
                      alert   caution,taskname,result
               endif
               MOVE           C1 TO NinvAcdPErc
               pack           Ninvacdanincd from b1,b1,b1
               Move           "088" to NinvAcdCode
               Move           NinvAcdCode to N3
.               Move           "m" to NINVacdRateT
               Move           "f" to NINVacdRateT
               Call           NInvAcdwrt
.end patch 1.1
         else
.         clear      addchg1
         endif
         clear      ar
         MOVE      "0",LAR
         MOVE      "0",LSH
         MOVE      "0",LAP
         MOVE      "0",LINC
         MOVE      "0",LeAR
         MOVE      "0",LeSH
         MOVE      "0",LeAP
         MOVE      "0",LeINC
         MOVE      "0",LrAR
         MOVE      "0",LrSH
         MOVE      "0",LrAP
         MOVE      "0",LrINC
         ADD       "9",LINES
        RETURN
* OWNER TOTALS
OTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.         PRINT     *2," OWNER **":
.           *L,*2," TOTAL **",*19,"--------",*63,"-------",*95,"---------":
.                   *115,"--------",*L,*16,GAR,*62,GSH,*93,GAP,*112,GINC
.
. 04/29/82 DLH.
         MOVE      "0",OAR
         MOVE      "0",OSH
         MOVE      "0",OAP
         MOVE      "0",OINC
.         ADD       "2",LINES
        RETURN

* OWNER HEADING
OH

         MOVE      OWNER,HOWNER
         MOVE      HOWNER TO NOWNFLD
         rep       zfill in nownfld
         CALL      NOWNKEY
.START PATCH 1.41 - REPLACED LOGIC
         compare    c1 to recsslct           .1st good record?
         if         equal          
         PRINT     032,hpreset,hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
                              else
         PRINT    hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
                              endif
.END PATCH 1.41 - REPLACED LOGIC
         MOVE      "5",LINES
        RETURN
* GRAND TOTAL HEADING
GH
.START PATCH 1.2 - REPLACED LOGIC
.         PRINT     hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PYR:
.                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
.                             *L,*49,"=== ======== ======= ======":
.                   *L,*L,*44,"GRAND TOTALS: ":
.                   *L,*23,"AR",*63,"SHIPPED":
.                   *98,"AP",*116,"LR INC.":
.           *L,*2," TOTAL **",*19,"--------",*63,"-------",*95,"---------":
.                   *115,"--------",*L,*16,GAR,*62,GSH,*93,GAP,*112,GINC
         PRINT     hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"GRAND TOTALS: ":
                   *L,*23,"AR",*64,"SHIPPED":
                   *98,"AP",*116,"LR INC.":
           *L,*2," TOTAL **",*19,"--------",*64,"-------",*95,"---------":
                   *115,"--------",*L,*16,GAR,*62,GSH,*93,GAP,*112,GINC
.END PATCH 1.2 - REPLACED LOGIC
.
        RETURN
...............................................................................
LDESC    CLEAR     LKEY
         MOVE      LN,HLN
         MOVE      HLN TO LKEY
         MOVE      LKEY TO NDATFLD
         CALL      NDATKEY
           IF          OVER
           MOVE        "NO LIST FOUND!!!!!!!!" TO OLSTNAME
           ENDIF
           CLEAR     NXRFMLR             *CLEAR VARIABLE IN CASE OVER.
           MOVE        HLN TO NXRFFLD
           MOVE        C1 TO NXRFPATH
           CALL      NXRFKEY
           PRINT     *L,*1,"LIST: ",LKEY,"  ",OLSTNAME:
                       *L,*1,"X-REF MLR## ",NXRFMLR
         ADD       "2",LINES
         RETURN
...............................................................................
* DETAIL HEADING
LHEAD    
.START PATCH 1.2 - REPLACED LOGIC
.         PRINT     *16,"AR",*67,"QTY":
.                   *L,*1,"LR",*15,"AMT",*26,"MAILER",*65,"SHIPPED":
.                   *82,"INVOICE NO.",*100,"AP",*116,"LR INC.":
.                   *L,*1,"------",*13,"--------",*26,"------",*65,"-------":
.                   *82,"-----------",*97,"---------",*115,"--------"
         PRINT     *16,"AR",*69,"QTY":
                   *L,*1,"LR",*15,"AMT",*26,"MAILER",*67,"SHIPPED":
                   *84,"INVOICE NO.",*102,"AP",*116,"LR INC.":
                   *L,*1,"------",*13,"--------",*26,"------",*67,"-------":
                   *84,"-----------",*99,"---------",*115,"--------"
.END PATCH 1.2 - REPLACED LOGIC
         ADD       "3",LINES
         RETURN
...............................................................................
.Detail - In house
DETAIL
         PACK      MKEY FROM MLR,MLRCNT
         CALL      MLRREAD
         move      lr to nordfld
         rep       zfill in nordfld
         call      nordkey
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         call      cvtjul
.         
         move      c1 to exchflag
         clear     exchrent
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         if        equal
         move      c1 to exchflag                  .true
         move      "Exch" to ExchRent          
         else
         move     c2 to exchflag                   .false its rental
         move     "rent" to exchRent
.START PATCH 1.2 - MISSED THIS EARLIER!!! EXPANDED VAR.
.         MOVE     C0 TO N7
.         MOVE     OEXQTY TO N7
.         COMPARE  C0 TO N7
         MOVE     C0 TO N9
         MOVE     OEXQTY TO N9
         COMPARE  C0 TO N9
.END PATCH 1.2 - MISSED THIS EARLIER!!! EXPANDED VAR.
         IF       NOT EQUAL
         MOVE     "SPLT" TO EXCHRENT
         ENDIF
         endif
         move     c0 to form104
         move     qty to form104
         mult     ".001" by form104
         move     c0 to form94
         move     arrun to form94
         divide   form104 into form94
         move     c0 to perm
         add      form94 to perm 
         MOVE      ARRUN,AT1
         MOVE      APRUn,AP1
         SUB       AP1RUN,AT1
.START PATCH 1.2 - REPLACED LOGIC
.         if        (juldays < strtdate)
.         PRINT     *1,hpbon,LR,*10,ARRUN,*22,MCOMP,hpboff,*65,QTY,"@",perm,*83,TPI:
.                   *95,APRUn,*112,AT1,b4,ExchRent
.         else
.         PRINT     *1,LR,*10,ARRUN,*22,MCOMP,*48,*65,QTY,"@",perm,*83,TPI:
.                   *95,APRUN,*112,AT1,b4,ExchRent
.         endif          
         if        (juldays < strtdate)
         PRINT     *1,hpbon,LR,*10,ARRUN,*22,MCOMP,hpboff,*65,QTY,"@",perm,*85,TPI:
                   *97,APRUn,*112,AT1,b2,ExchRent
         else
         PRINT     *1,LR,*10,ARRUN,*22,MCOMP,*48,*65,QTY,"@",perm,*85,TPI:
                   *97,APRUN,*112,AT1,b2,ExchRent
         endif          
.END PATCH 1.2 - REPLACED LOGIC
         ADD       ARRUN,LAR
         ADD       QTY,LSH
         ADD       APRUN,LAP
         ADD       AT1,LINC
.         
         compare   c1 to exchflag
         if        equal
         if        (juldays < strtdate)
         ADD       ARRUN,LeARold
         ADD       QTY,LeSHold
         ADD       APRUN,LeAPold
         ADD       AT1,LeINCold
         else
         ADD       ARRUN,LeAR
         ADD       QTY,LeSH
         ADD       APRUN,LeAP
         ADD       AT1,LeINC
         endif
         endif
.
         compare   c2 to exchflag
         if        equal
         ADD       ARRUN,LrAR
         ADD       QTY,LrSH
         ADD       APRUN,LrAP
         ADD       AT1,LrINC
         endif
.
         ADD       ARRUN,OAR
         ADD       QTY,OSH
         ADD       APRUN,OAP
         ADD       AT1,OINC
.
         ADD       ARRUN,GAR
         ADD       QTY,GSH
         ADD       APRUN,GAP
         ADD       AT1,GINC
.
         ADD       C1,LINES
         RETURN
...............................................................................
...............................................................................
NOORD    BEEP
         DISPLAY   *P01:23,*EL,"THAT LR IS NOT ON THE ORDER FILE !!!",*W3;
         GOTO      date
...............................................................................
NOFILE   DISPLAY   *B,*P01:23,*EL,NOFILE," FILE IS NOT ON LINE NOTIFY YOUR ":
                   "PROGRAMER !!!!!",*W,*B,*W,*B,*W5;
         STOP
...............................................................................
NOLST    DISPLAY   *B,*P01:23,*EL,"I CAN'T FIND A LIST DESCRIPTION FOR ":
                   "LIST ## ",LN,*W,*B,*W,*B;
         DISPLAY   *P01:24,*EL,"NOTIFY YOUR PROGRAMER !!!!!!",*W,*B,*B;
         GOTO      date
...............................................................................
STOP
         STOP
...............................................................................
EOJ2
         CALL      LTOT
         CALL      OTOT
         CALL      GH
         PRINT     *F
         SPLCLOSE
         release
         BEEP
         display   *p1:24,*el,"submitting report to printer",*b,*w5
.         EXECUTE   "c:\command.com /c copy g:\DATA\runFEE.lst \\nts0\LASER2 "
.          erase      "g:\data\runFEE.lst"
         CLOSE     EXPRINT
          Shutdown    "cls"
         STOP
.         STOP
...............................................................................
* OWNER TOTALS
OTOT1
.         PRINT     *2,"** OWNER **":
.           *L,*2,"** TOTAL **",*19,"--------",*63,"-------":
.                   *L,*16,OAR,*62,OSH
.
.  TURNED OFF 04/29/82 BY DLH REQUESTED BY SA.
         MOVE      "0",OAR
         MOVE      "0",OSH
         MOVE      "0",OAP
         MOVE      "0",OINC
.         ADD       "2",LINES
        RETURN
.
* OWNER HEADING
OH1
         MOVE      OWNER,HOWNER
         MOVE      HOWNER TO NOWNFLD
         move      c1 to nownpath
         CALL      NOWNKEY
.START PATCH 1.2 - REPLACED LOGIC
.         PRINT     hp17ptch, *F,*1,"CONFIDENTIAL",*128,PMO,"/",PYR:
.                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
.                             *L,*49,"=== ======== ======= ======":
.                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
         PRINT     hp17ptch, *F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
.END PATCH 1.2 - REPLACED LOGIC
         MOVE      "5",LINES
        RETURN
...............................................................................
LDESC1    CLEAR     LKEY
         MOVE      LN TO HLN
         MOVE      HLN,LKEY
         MOVE      LKEY TO NDATFLD
         CALL      NDATKEY
         PRINT     *L,*L,*1,"LIST: ",LKEY,"  ",OLSTNAME
         ADD       "2",LINES
         RETURN
...............................................................................
* DETAIL HEADING
.DID NOT UPDATE WITH PATCH 1.2 AS THIS SUB-ROUTINE NEVERS SEEMS TO BE ACCESSED.  ASH 08SEP1999
LHEAD1   PRINT     *22,"AR",*65,"QTY":
                   *L,*6,"LR",*22,"AMT",*41,"MAILER",*66,"SHIPPED":
                   *78,"INVOICE NO.":
                   *L,*4,"------",*19,"--------",*41,"------",*66,"-------":
                   *78,"---------"
         MOVE      "3",LINES
         RETURN
...............................................................................
...............................................................................
NEWPAGE  PRINT     *F
         MOVE      "0",LINES
         RETURN
...............................................................................
MLRREAD
         CALL      NMLRKEY
         RETURN
*
.invoice goodies go here
*                   
.End of Subroutine Total
..............................................................................................
..............................................................................................
.End of Subroutine EOJ
..............................................................................................

         include   xmgtio.inc
         INCLUDE   NOWNIO.inc
.Patch1.42
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch1.42
         INCLUDE   NDATIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NXRFIO.inc
         INCLUDE   NMTXIO.inc
.begin patch 1.5
.         include   ninvio.inc
               include        ninvio.inc
               include        NInvacdio.inc
.end patch 1.5
         include   gnxtio.inc
.Patch1.42
.         include   nbrkio.inc
.Patch1.42
.START PATCH 1.72 REMOVED LOGIC
..START PATCH 1.36 ADDED LOGIC
.         include   nfulio.inc
..END PATCH 1.36 ADDED LOGIC
.START PATCH 1.72 REMOVED LOGIC
         INCLUDE   COMLOGIC.inc


*******************************************************************************
* RESPONSE  ANALISYS PROGRAM 1
*
*               ver. 1.2
*               version 1.3    print revision and delete option 06/06/83
*               version 2.0    major calculation corrections 06/14/83
*               version 2.1    allow keyin of owner number for report title
*                              08/17/83
*               version 2.2    allow reading of master datacard file for key
*                              description 09/09/86.
*               version 2.21   include our list number in rspkey file
*******************************************************************************
         INCLUDE   COMMON.INC
         include   cons.inc
         include   ndatdd.inc
         include   nowndd.inc
;Patch3.1
			include	compdd.inc
			include	cntdd.inc
.        include   nmlrdd.inc
;Patch3.1
         INCLUDE   HP.INC
release  init      "3.1"     DMB 26MAY2004	Mailer Conversion
.release  init      "3.01"     ASH 02OCT2000 NEW SERVER ADDED
.release  init      "3.00"     revive me with viverin
...............................................................................
. RSPDETAILS          KEY     SORT1    SORT2    SORT3
.
. MLRNO      1-4       1        1        1        1      MLRNO       1-4
. MLRDESC    5-39                                        MLRDESC     5-39
. KKEY      40-44      2        4        5        4      KKEY       40-44
. KDESC     45-99               2                        KDESC      45-99
. PKG      100-111     3        5        6        3      PKG       100-111
. PDESC    112-156              3                 2      PDESC     112-156
. QTYM     157-164                                       QTYM      157-164
. MM       165-166                       3(D)            MM        165-166
. DD       167-168                       4(D)            DD        167-168
. YY       169-170                       2(D)            YY        169-170
. RMO      171-172                                       RMO       171-172
. RDD      173-174                                       RDD       173-174
. RYY      175-176                                       RYY       175-176
. TOTAL$   177-186                                       TOTAL$    177-186
. COSTM    187-191                                       COSTM     187-191
. RTN      192-199                                       RTN       192-199
.
...............................................................................
.  FILES
.
.RSPMLR   IFILE                   MAILER FILE (KEY INDEXED 1-4)
.RSPKEY   IFILE                   KEY FILE    (KEY INDEXED 1-5)
.RSPPKG   IFILE                   PAKAGE FILE (KEY INDEXED 1-12)
.RSPDET   IFILE                   DETAIL FILE (KEY INDEXED 1-22)
.RSPOWN   IFILE                   OWNER FILE  (KEY INDEXED 3-6)
.NINDAT   IFILE                   MASTER DATACARD FILE
CHNFLE   FILE                    RMS PROCEDURE FILE
RSPSRT1  FILE                    SELECT AND SORT FILE FOR PRINT
RSPSRT1A FILE
RSPSRT2  FILE                    SELECT AND SORT FILE FOR PRINT
RSPSRT3  FILE                    SELECT AND SORT FILE FOR PRINT
ARCFILE  FILE
. RMS FILES.
. .............................
RSPMLR   IFILE     KEYLen=4
RSPKEY   IFILE     KEYLen=5
RSPPKG   IFILE     KEYLen=12
RSPDET   IFILE     KEYLen=21,STATIC=10
.RSPOWN   IFILE     KEYLen=4,FIXED=170

. .........
...............................................................................
.  DATA DEFINITION
.
ANS      DIM       1
COSTM    FORM      3.2
COSTM1   FORM      5.2
COSTM2   FORM      3
DETKEY   DIM       22
ITEM     FORM      2
KDESC    DIM       55
KKEY     DIM       5
MMO      DIM       2
MDD      DIM       2
MYY      DIM       2
MLRNO    DIM       4
MLRDESC  DIM       35
NOFILE   DIM       8
PDESC    DIM       45
PKG      DIM       12
QTYM     FORM      8
QTYM1    FORM      8.3
NANS     FORM      1
RMO      DIM       2
RDD      DIM       2
RYY      DIM       2
RTN      FORM      8
RTNM     FORM      8.5
RTNPC    FORM      3.2
RTNM1    DIM       13
RTNM2    FORM      8
RTNA     DIM       14
RTND     DIM       3
RTN$     FORM      7.2
TOTAL$   FORM      7.2
TOTAL$H  FORM      7.2
TOTAL$H1 FORM      7.4
.TOTAL$H2 FORM      .3
TOTAL$H2 FORM      7.2
TOTAL$H3 DIM       4
TOTAL$H4 FORM      4
TOTAL$H5 FORM      7.2
TOTAL$H6 FORM      7.4
GIFT     FORM      3.2
TWO      FORM      "11"
NINE     FORM      "9"
TEN      FORM      "10"
BLANKK   INIT      "          "
FIRST    FORM      "0"
.
KCTR     FORM      2
DCTR     FORM      2
VT       FORM      2
VT11     FORM      "11"
VT12     FORM      "12"
VT13     FORM      "13"
VT14     FORM      "14"
VT15     FORM      "15"
HZ       FORM      2
HZ1      FORM      "09"
HZ2      FORM      "49"
DZ       FORM      2
TLR      DIM       4
KLR      DIM       4
KLR1     DIM       4
KLR2     DIM       4
KLR3     DIM       4
KLR4     DIM       4
KLR5     DIM       4
KLR6     DIM       4
KLR7     DIM       4
KLR8     DIM       4
KLR9     DIM       4
KLR10    DIM       4
KLR11    INIT      "****"
KALERT   DIM       25
KALERT1  INIT      "#"ALL#" or MLR# or #"^#" "
KALERT2  INIT      "MLR# or #"*#" or #"^#"   "
KALERT3  INIT      "MLR# or #"^#"            "
PCTR     FORM      1
TCTR     FORM      2
MLRHOLD  DIM       4
RPTNO    DIM       1
LINES    FORM      2
PHOLD    DIM       12
PAGENO   FORM      2
.
DIVQ     FORM      10.5
QTYMASK  DIM       10
TQTY     FORM      10
GQTY     FORM      10
GQTYMASK DIM       13
.
RTNMASK  DIM       10
TRTN     FORM      10
GRTN     FORM      10
GRTNMASK DIM       13
.
TOTMASK  DIM       12
TTOT     FORM      9.2
GTOT     FORM      9.2
GTOTMASK DIM       14
.
TCOST    FORM      6.2
GCOST    FORM      6.2
COSTNUM  FORM      4      # OF TOTAL MAILINGS USED IN AVER TOTAL CALC'S
NUMPERM  FORM      4      # OF MAILINGS PER PACKAGE OR MAILING PERIOD
.                         USED ON SUMMARY CALC'S
.
MLRNAME  DIM       35
FPTR     FORM      5
LPTR     FORM      5
DESC45   DIM       45
ONETIME  DIM       1
RTN5     FORM      3.2
NET5     FORM      3.2
NET5MASK DIM       7
NET6     FORM      5.2
NET6MASK DIM       9
TDIV     FORM      10.5
SKIP     DIM       1
DATESUMM DIM       8
DATEHOLD DIM       8
DATERSP  DIM       8
THSND    FORM      "1000"
YRSEL    DIM       1           *SELECT ALL YEARS?
YR1      DIM       2
YR2      DIM       2
YR3      DIM       2
YR4      DIM       2
YR5      DIM       2
.
. OWNER FILE
LOWNCD   DIM       1
LOBLANK  DIM       1
LOWNUM   DIM       4       KEY
LONAME   DIM       25      CONTACT NAME
LOCOMP   DIM       25      COMPANY NAME
LOADDR   DIM       25      STREET ADDRESS
LOCITY   DIM       15      CITY
LOSTATE  DIM       2
LOZIP    DIM       10
LOCOPIES DIM       1        NUMBER OF EXTRA COPIES
LOCNAME  DIM       16       COPY TO NAME
LOTELE   DIM       10       OWNER PHONE
LOPASS   DIM       10
LODATE   DIM       6        REVISED DATE
LOBLANK4 DIM       4
.
TITLEDES DIM       25       TITLE DESCRIPTION
LKEY     DIM       6        USED FOR DATACARDS READ
LFILL71  DIM       71       USED FOR DATACARDS READS
DATE     DIM       6
ROLL     INIT      "RSPCALROLL/ROLL"
LSTOK    DIM       1
SORTBR   FORM      1
SAVEDESC DIM       55
DIM7     DIM       7
.TASKNAME DIM       256
...............................................................................
. MAINLINE FOLLOWS
.
SCREEN1 
          MOVE       "Names in the News Ca inc" to compnme
          move       "Response Analysis" to stitle
          move       "nrsp0001" to program
          clock      date to today
         call        paint
.         DISPLAY   *ES:
.                   *P18:06,"**** NAMES IN THE NEWS  RESPONSE ANALYSIS ****":
.                   *P18:07,"     ====================================":
.                   *P18:09,"       *** M A I L  A N A L Y S I S ***";
. ............
         TRAP      INTERROR IF INT
         TRAP      ABORT IF F1
FILESO   TRAP      NOFILE IF IO
         MOVE      "RSPMLR",NOFILE
         DISPLAY   *P01:23,"OPENING ",NOFILE;
         OPEN      RSPMLR,"RSPMLR"
         DISPLAY   *P01:23,"OPENED ";
.
         MOVE      "RSPKEY",NOFILE
         DISPLAY   *R,*P01:23,"OPENING ",NOFILE;
         OPEN      RSPKEY,"RSPKEY"
         DISPLAY   *P01:23,"OPENED ";
.
         MOVE      "RSPPKG",NOFILE
         DISPLAY   *R,*P01:23,"OPENING ",NOFILE;
         OPEN      RSPPKG,"RSPPKG"
         DISPLAY   *P01:23,"OPENED ";
.
         MOVE      "RSPDET",NOFILE
         DISPLAY   *R,*P01:23,"OPENING ",NOFILE;
         OPEN      RSPDET,"RSPDET"
         DISPLAY   *P01:23,"OPENED ";
.
.         MOVE      "RSPOWN",NOFILE
.         DISPLAY   *R,*P01:23,"OPENING ",NOFILE;
.         OPEN      RSPOWN,"NINOWN"
.         DISPLAY   *P01:23,"OPENED ";
.
.         MOVE      "NINDAT",NOFILE
.         DISPLAY   *R,*P01:23,"OPENING ",NOFILE;
.         OPEN      NINDAT,"NINDAT"
.         DISPLAY   *P01:23,"OPENED ";
.
         TRAPCLR   IO
. ..................
BEGIN    DISPLAY   *P01:19,*EF,*R,*P01:23,"FILES OPEN",*W1;
.
BEGIN1   DISPLAY   *P01:05,*EF:
                   *P32:06,"** MASTER  MENU **":
                   *P34:08,"1....UPDATE RESPONSE DATABASE":
                   *P34:09,"2....MAILER MODIFICATION":
                   *P34:10,"3....KEY MODIFICATION":
                   *P34:11,"4....PACKAGE MODIFICATION":
                   *P34:12,"5....SORT AND PRINT REPORTS":
                   *P34:13,"6....RETURN TO MASTER DATASHARE MENU";
BEGIN2
         KEYIN     *P27:15,"ENTER: _  (1,2,3,4,5,6)",*P34:15,NANS;
. ..................
                   BRANCH NANS OF UPDET,UPMAIL,UPKEY,UPPKG,INDEX,MASTER
                   BEEP
                   GOTO BEGIN2
.
MASTER
         CLOSE     RSPMLR
         CLOSE     RSPKEY
         CLOSE     RSPPKG
         CLOSE     RSPDET
.         CLOSE     RSPOWN
         STOP
. ..................
UPMAIL   KEYIN     *P01:04,*EF:
                   *P01:23,"ENTER 9999 TO END":
                   *P01:04,"ENTER RSP MAILER NUMBER: ____":
                   *P26:04,*JR,*ZF,*DE,MLRNO,*P26:04,*DV,MLRNO;
.
         TYPE      MLRNO
         GOTO      UPMAIL IF EOS
         MATCH     "0000",MLRNO
         GOTO      UPMAIL IF EQUAL
         MATCH     "9999",MLRNO
         GOTO      BEGIN IF EQUAL
.
         MOVE      MLRNO TO STR4    SAVE KEY
         READ      RSPMLR,MLRNO;MLRNO,MLRDESC
         GOTO      NEWMAIL IF OVER
.
MDOIT    KEYIN     *P01:22,"HIT ENTER OR REVISE MAILER DESCRIPTION":
                   *P01:23,"OR (*) TO DELETE":
                   *P01:06,*EL,"Mailer description : ",*P21:06,*DV,MLRDESC:
                   *P21:06,*RV,MLRDESC:
                   *P21:06,*EL:
                   *DV,*P21:06,MLRDESC;
.
         KEYIN     *P01:22,*EL,"OK ?  (Y) ",ANS;
         MATCH     "N",ANS
         GOTO      MDOIT IF EQUAL
         CMATCH    "*",MLRDESC
         GOTO      MDEL IF EQUAL
.
         UPDATE    RSPMLR;MLRNO,MLRDESC
         GOTO      UPMAIL
.
MDEL     READ      RSPMLR,MLRNO;MLRNO,MLRDESC
         DELETE    RSPMLR,MLRNO
         GOTO      UPMAIL
.
NEWMAIL  pack      mkey from STR4,z3
         rep       zfill in mkey
         MOVE      STR4 TO MLRNO
         move      c1 to nmlrpath
         call      nmlrkey
         move      mcomp to mlrdesc       
         KEYIN    *P01:05,"THAT MAILER IS NOT ON Resp FILE SHOULD I ADD IT ?":
                  *UC, ANS;
         MATCH     "Y",ANS
         GOTO      UPMAIL IF NOT EQUAL
         KEYIN     *P01:06,*EL,"ENTER MAILER DESC ",*dv,MLRDESC:
                   *p19:06,*edit,*rv,mlrdesc:
                   *p19:06,*dv,b12,*dv,b12,*dv,b10:
                   *p19:06,mlrdesc;
.
         WRITE     RSPMLR,MLRNO;MLRNO,MLRDESC
.
         DISPLAY   *P01:06,*EL:
                   *P01:05,*EL,"MAILER ADDED",*W1;
         GOTO      UPMAIL
...............................................................................
UPKEY
         KEYIN     *P01:04,*EF:
                   *P01:23,"ENTER 9999 TO END":
                   *P01:04,"ENTER KEY :_____",*P12:04,*JR,*ZF,KKEY:
                   *P12:04,*EL,*P12:04,*DV,KKEY;
.
         MATCH     "9999",KKEY
         GOTO      BEGIN IF EQUAL
.
         TYPE      KKEY
         GOTO      UPKEY IF EOS
         READ      RSPKEY,KKEY;KKEY,LKEY,KDESC
.
         GOTO      NEWKEY IF OVER
.
KDOIT    KEYIN     *P01:22,"HIT ENTER, OR REVISE KEY DESCRIPTION":
                   *P01:23,"OR (*) TO DELETE":
                   *P1:06,*EL,"ENTER NIN LIST ## OR ENTER :",*JR,*ZF,*RV,LKEY:
                   *P01:08,*EL,"KEY DESCRIPTION : ",*DV,KDESC:
                   *P19:08,*RV,KDESC,*P19:06,*EL:
                   *P19:08,*DV,KDESC;
.
         KEYIN     *P01:22,*EL,"OK ?  (Y) ",ANS;
         MATCH     "N",ANS
         GOTO      KDOIT IF EQUAL
         CMATCH    "*",KDESC
         GOTO      KDEL IF EQUAL
.
.
         UPDATE    RSPKEY;KKEY,LKEY,KDESC
         GOTO      UPKEY
.
KDEL     READ      RSPKEY,KKEY;;
         DELETE    RSPKEY,KKEY
         GOTO      UPKEY
.
NEWKEY
         KEYIN     *P01:05,"THAT LIST IS NOT ON FILE SHALL I ADD IT ?":
                   ANS;
         MATCH     "Y",ANS
         GOTO      UPKEY IF NOT EQUAL
         KEYIN     *P1:06,*EL,"ENTER NIN LIST ## OR ENTER :",*JR,*ZF,LKEY;
         CMATCH    " " TO LKEY
         GOTO      NEWKEY1 IF EOS
         REP       " 0" IN LKEY
         CALL      READLIST
         CMATCH    "*" TO LSTOK
         GOTO      DISLIST IF EQUAL
         GOTO      NEWKEY1 IF NOT EQUAL
.
DISLIST  DISPLAY   *P01:08,*EL,"ENTER KEY DESC. :",KDESC;
         GOTO      NEWKEY2
.
NEWKEY1  KEYIN     *P01:08,*EL,"ENTER KEY DESC. :",KDESC;
.
NEWKEY2  WRITE     RSPKEY,KKEY;KKEY,LKEY,KDESC
.
         DISPLAY   *P01:04,*EL:
                   *P01:05,*EL,"KEY ADDED",*W1;
         GOTO      UPKEY
.
         DISPLAY   *P01:05,*EL,"MAILER ADDED",*W1;
         GOTO      UPKEY
.
READLIST MOVE      "*" TO LSTOK
         MOVE      KDESC TO SAVEDESC
         move      lkey to ndatfld
         rep       zfill in ndatfld
         move      c1 to ndatpath
         call      ndatkey
         GOTO      NOLIST IF OVER
         move      mlstname to kdesc
.         READ      NINDAT,LKEY;LFILL71,KDESC
.         GOTO      NEWKEY1 IF OVER
         RETURN
NOLIST   MOVE      " " TO LSTOK
         MOVE      SAVEDESC TO KDESC
         RETURN
. ..........................................................................
UPPKG
         KEYIN     *P01:04,*EF:
                   *P01:23,"ENTER 9999 TO END":
                   *P01:04,"ENTER PKG :____________",*P12:04,PKG:
                   *P12:04,*EL,*P12:04,*DV,PKG;
.
         MATCH     "9999",PKG
         GOTO      BEGIN IF EQUAL
.
         TYPE      PKG
         GOTO      UPPKG IF EOS
         READ      RSPPKG,PKG;PKG,PDESC
         GOTO      NEWPKG IF OVER
.
PDOIT    KEYIN     *P01:22,"HIT ENTER, OR REVISE PACKAGE DESCRIPTION":
                   *P01:23,"OR (*) TO DELETE":
                   *P01:06,*EL,"PKG DESCRIPTION : ",*DV,PDESC:
                   *P19:06,*RV,PDESC:
                   *P19:06,*EL,*DV,PDESC;
.
         KEYIN     *P01:22,*EL,"OK ?  (Y) ",ANS;
         MATCH     "N",ANS
         GOTO      PDOIT IF EQUAL
         CMATCH    "*",PDESC
         GOTO      PDEL IF EQUAL
.
.
         UPDATE    RSPPKG;PKG,PDESC
         GOTO      UPPKG
.
PDEL     READ      RSPPKG,PKG;PKG,PDESC
         DELETE    RSPPKG,PKG
         GOTO      UPPKG
.
NEWPKG
         KEYIN     *P01:05,"THAT PACKAGE IS NOT ON FILE SHALL I ADD IT ?":
                   ANS;
         MATCH     "Y",ANS
         GOTO      UPPKG IF NOT EQUAL
         KEYIN     *P01:06,*EL,"ENTER PKG DESC :",PDESC;
.
         WRITE     RSPPKG,PKG;PKG,PDESC
.
         DISPLAY   *P01:05,*EL,"PKG ADDED",*W1;
         GOTO      UPPKG
. ..........................................................................
UPDET
MAILDET  KEYIN     *P01:03,*EF,"DETAIL UPDATE":
                   *P01:05,"MAILER: ____":
                   *P01:06,"KEY:    ______":
                   *P01:07,"PKG:    ____________":
                   *P09:05,*JR,*ZF,*DE,MLRNO,*P09:05,*EL,*DV,MLRNO;
         TYPE      MLRNO
         GOTO      MAILDET IF EOS
         MATCH     "9999",MLRNO
         GOTO      BEGIN IF EQUAL
.
         READ      RSPMLR,MLRNO;MLRNO,MLRDESC
.
         GOTO      NGM IF OVER
         GOTO      GOM
.
NGM      DISPLAY   *P13:05,*EL,*B,"THAT MAILER IS NOT ON FILE YET",*W2;
         GOTO      MAILDET
.
GOM      DISPLAY   *P13:05,*EL,"    ",MLRDESC;
         KEYIN     *P75:05,"OK ?",ANS,*P75:05,*EL;
         MATCH     "N" TO ANS
         GOTO      MAILDET IF EQUAL
. ..............
KEYDET   KEYIN    *P09:06,*JR,*ZF,KKEY,*P09:06,*EL,*DV,KKEY;
         TYPE      KKEY
         GOTO      KEYDET IF EOS
         MATCH     "999999",KKEY
         GOTO      BEGIN IF EQUAL
.
         READ      RSPKEY,KKEY;KKEY,LKEY,KDESC
.
         GOTO      NGK IF OVER
         GOTO      GOK
.
NGK      DISPLAY   *P14:06,*EL,*B,"THAT KEY IS NOT ON FILE YET",*W2;
         GOTO      UPDET
.
GOK      DISPLAY   *P14:06,*EL,"    ",KDESC;
         KEYIN     *P75:06,"OK ?",ANS,*P75:06,*EL;
                   MATCH "N",ANS
                   GOTO KEYDET IF EQUAL
. ..............
PKGDET   KEYIN    *P09:07,PKG,*P09:07,*EL,*DV,PKG;
         MATCH     "999999999999",PKG
         GOTO      BEGIN IF EQUAL
         TYPE      PKG
         GOTO      PKGDET IF EOS
.
         READ      RSPPKG,PKG;PKG,PDESC
         GOTO      NGPKG IF OVER
         GOTO      GOPKG
NGPKG
         DISPLAY   *P19:07,*EL,*B," THAT PKG IS NOT ON FILE YET !!",*W2;
         GOTO      UPDET
.
GOPKG
         KEYIN     *P19:07,*EL,"    ",*DV,PDESC,*P75:07,"OK ?",ANS,*P75:07,*EL;
         MATCH      "N",ANS
         GOTO       PKGDET IF EQUAL
. ..............
. ..............
EXIST    CLEAR     DETKEY
         PACK      DETKEY FROM MLRNO,KKEY,PKG
.
         READ      RSPDET,DETKEY;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                   QTYM,MMO,MDD,MYY,RMO:
                   RDD,RYY,TOTAL$,COSTM,RTN
         GOTO      ADD IF OVER
.
UPDATE   DISPLAY   *P02:12,*EF,"(1) QTY MAILED",*P19:12,"(2) DATE MAILED":
                   *P39:11,"DATE OF",*P35:12,"(3) 1ST RET":
                   *P48:12,"(4) TOTAL $",*P58:12,"(5) COST/M":
                   *P70:12,"(6) RETURNS":
                   *P05:16,"% RTN",*P19:16,"AVG. GIFT",*P35:16,"RTN/M",*P47:16:
                   "NET/M",*P05:13,QTYM,*P24:13,MMO,"/",MDD,"/",MYY:
                   *P39:13,RMO,"/",RDD,"/",RYY,*P48:13,TOTAL$,*P61:13,COSTM:
                   *P70:13,RTN;
.
CALC     CALL      CALCRET
         CALL      CALCAVG
         CALL      CALCRETM
         CALL      CALCNETM
.
.
CHANGE   KEYIN     *P01:24,*EL,"ENTER ITEM TO BE MODIFIED __  (99 to end)":
                   *P27:24,ITEM," (88) to delete record";
         COMPARE   "99",ITEM
         GOTO      FIX IF EQUAL
         COMPARE   "88",ITEM
         GOTO      DELETE IF EQUAL
         GOTO      BR1
.
FIX      DELETE   RSPDET,DETKEY
         WRITE     RSPDET,DETKEY;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                   QTYM,MMO,MDD,MYY,RMO,RDD,RYY,TOTAL$,COSTM,RTN
         GOTO      BEGIN
.
BR1      BRANCH    ITEM OF QTY,MDATE,RDATE,TOT,COST,RTN
         GOTO      CHANGE
.
DELETE
         DELETE    RSPDET,DETKEY
         GOTO      BEGIN
.
QTY
         DISPLAY   *P05:13,"        ";
         KEYIN       *P05:13,*RV,QTYM,*DV,*P05:13,QTYM;
         CALL      CALC
         GOTO      CHANGE
MDATE
         KEYIN     *P24:13,"  /  /  ":
                   *P24:13,*RV,MMO;
         REPLACE   " 0",MMO
         DISPLAY   *P24:13,MMO;
.
         KEYIN     *P27:13,*RV,MDD;
         REPLACE   " 0",MDD
         DISPLAY   *P27:13,MDD;
.
         KEYIN     *P30:13,*RV,MYY;
         REPLACE   " 0",MYY
         DISPLAY   *P30:13,MYY;
         GOTO      CHANGE
.
RDATE
         KEYIN     *P39:13,"  /  /  ":
                   *P39:13,*RV,RMO;
         REPLACE   " 0",RMO
         DISPLAY   *P39:13,RMO;
.
         KEYIN     *P42:13,*RV,RDD;
         REPLACE   " 0",RDD
         DISPLAY   *P42:13,RDD;
.
         KEYIN     *P45:13,*RV,RYY;
         REPLACE   " 0",RYY
         DISPLAY   *P45:13,RYY;
         GOTO      CHANGE
.
TOT
         KEYIN     *P48:13,"          ",*P48:13,*RV,*JR,TOTAL$:
                   *P48:13,*DV,TOTAL$;
         GOTO      CALC
.
COST
         KEYIN     *P62:13,"       ",*P62:13,*RV,*JR,COSTM,*P62:13,*DV,COSTM;
         GOTO      CALC
.
RTN
         KEYIN     *P73:13,"     ",*P73:13,*RV,*JR,RTN,*P73:13,*DV,RTN;
         GOTO      CALC
CALCRET
         MOVE      RTN,RTNM
         DIV       QTYM,RTNM
         ADD       ".00005",RTNM
         MULT      "100",RTNM
         MOVE      RTNA,RTND
         MOVE      RTNM TO RTNPC
         DISPLAY   *P04:17,RTNPC;
         RETURN
.
CALCAVG
         MOVE      TOTAL$,TOTAL$H
         MOVE      RTN,RTN$
         DIVIDE    RTN$,TOTAL$H
         DISPLAY   *P15:17,TOTAL$H;
         RETURN
CALCRETM
         MOVE      TOTAL$,TOTAL$H1
         DIVIDE    QTYM,TOTAL$H1
         MULT      THSND,TOTAL$H1
         MOVE      TOTAL$H1 TO TOTAL$H6            FOR CALCNETM
         ADD       ".0005",TOTAL$H1
         MOVE      TOTAL$H1,TOTAL$H2
         DISPLAY   *P32:17,TOTAL$H2;
         RETURN
CALCNETM
         MOVE      COSTM,COSTM1
.         MOVE      RTNM,RTNM2
..         MOVE      TOTAL$H2,TOTAL$H3
.         BUMP      TOTAL$H3,ONE
.         MOVE      TOTAL$H3,TOTAL$H4
.         SUB       COSTM1,TOTAL$H4
.         DISPLAY   *P44:17,TOTAL$H4;
         SUB       COSTM1,TOTAL$H6
         MOVE      TOTAL$H6,TOTAL$H5
         DISPLAY   *P44:17,TOTAL$H5;
         RETURN

. ................
ADD      DISPLAY   *P02:12,*EF,"(1) QTY MAILED",*P19:12,"(2) DATE MAILED":
                   *P39:11,"DATE OF",*P35:12,"(3) 1ST RET":
                   *P51:12,"TOTAL $",*P62:12,"COST/M":
                   *P74:12,"RETURNS":
                  *P05:16,"% RTN",*P19:16,"AVG. GIFT",*P31:16,"RTN/M",*P43:16:
                   "NET/M";
.
ADDQTY   KEYIN     *P05:13,QTYM;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDQTY IF EQUAL
.
ADDMMO   DISPLAY   *P24:13,"  /  /  ";
         KEYIN     *P24:13,*DE,*JR,*ZF,MMO;
         REPLACE   " 0",MMO
         DISPLAY   *P24:13,MMO;
.
ADDMDD   KEYIN     *P27:13,*DE,*JR,*ZF,MDD;
         REPLACE   " 0",MDD
         DISPLAY   *P27:13,MDD;
.
ADDMYY   KEYIN     *P30:13,*DE,*JR,*ZF,MYY;
         REPLACE   " 0",MYY
         DISPLAY   *P30:13,MYY;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDMMO IF EQUAL
. ...........
ADDRMO   DISPLAY   *P39:13,"  /  /  ";
         KEYIN     *P39:13,*DE,*JR,*ZF,RMO;
         REPLACE   " 0",RMO
         DISPLAY   *P39:13,RMO;
.
ADDRDD   KEYIN     *P42:13,*DE,*JR,*ZF,RDD;
         REPLACE   " 0",RDD
         DISPLAY   *P42:13,RDD;
.
ADDRYY   KEYIN     *P45:13,*DE,*JR,*ZF,RYY;
         REPLACE   " 0",RYY
         DISPLAY   *P45:13,RYY;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDRMO IF EQUAL
. .............
ADDTOT$  KEYIN     *P48:13,*EL,*P48:13,TOTAL$;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDTOT$ IF EQUAL
. .............
ADDCOST  KEYIN     *P62:13,*EL,*P62:13,COSTM;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDCOST IF EQUAL
. ............
ADDRTN   KEYIN     *P73:13,*EL,*P73:13,RTN;
         KEYIN     *P01:24,*EL,"OK ?",ANS,*P01:24,*EL;
         MATCH     "N",ANS
         GOTO      ADDRTN  IF EQUAL
. .............
         CALL     CALCRET
         CALL     CALCAVG
         CALL     CALCRETM
         CALL     CALCNETM
         KEYIN     *P01:23,*EL,"HIT ENTER TO CONTINUE",ANS;
. .............
         WRITE     RSPDET,DETKEY;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                   QTYM,MMO,MDD,MYY,RMO,RDD,RYY,TOTAL$,COSTM,RTN
.
         GOTO      UPDET
...............................................................................
INDEX
.         OPEN      ARCFILE,"ARCCLOCK"
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO MMO,STR1,MDD,STR1,MYY
.         READ      ARCFILE,SEQ;SKIP,MYY,SKIP,MMO,SKIP,MDD
         REPLACE   " 0" IN MMO
         REPLACE   " 0" IN MDD
.         CLEAR     TODAY
.         PACK      TODAY FROM MMO,SLASH,MDD,SLASH,MYY
         MOVE      c0 TO KCTR
         CLEAR     KLR
.
INDEXT
         KEYIN     *P01:06,*EF:
                   *P26:06,"** R E P O R T  M E N U **":
                   *P06:11,*JR,*ZF,"KEYIN OWNER NUMBER FOR TITLE ",LOWNUM;
         MATCH     "0000",LOWNUM
         GOTO      INDEXT IF EQUAL
         MATCH     "000*",LOWNUM
         GOTO      BEGIN1 IF EQUAL
         move      lownum to nownfld
         move      c1 to nownpath
         call      nownkey
.         READ      RSPOWN,LOWNUM;LOWNCD,LOBLANK,LOWNUM,LONAME,LOCOMP:
.                   LOADDR,LOCITY,LOSTATE,LOZIP,LOCOPIES,LOCNAME,LOTELE:
.                   LOPASS,LODATE,LOBLANK4
         GOTO      INDEXTO IF OVER
         MOVE      ownocpy TO TITLEDES
INDEXTD
         DISPLAY   *P06:12,*EL,TITLEDES;
         KEYIN     *P12:14,*EL,"OK? ",ANS;
         CMATCH    "Y",ANS
         GOTO      INDEXA IF EQUAL
         GOTO      INDEXT
INDEXTO
         MOVE      "NO OWNER DESC FOUND" TO TITLEDES
         DISPLAY   *P06:12,*EL,TITLEDES;
         PAUSE     "3"
         GOTO      INDEXT
INDEXA
         ADD       c1 TO KCTR
         COMPARE   "11" TO KCTR
         GOTO      INDEXB IF NOT LESS
         STORE     KLR INTO KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         GOTO      INDEXA
INDEXB
         DISPLAY   *P01:06,*EF:
                   *P26:06,"** R E P O R T  M E N U **":
                   *P27:08,"* MAILER  SELECTIONS *":
                   *P06:10,"MAILER##   DESCRIPTION":
                   *P46:10,"MAILER##   DESCRIPTION":
                   *P06:11,"1. ____   _________________________":
                   *P46:11,"2. ____   _________________________":
                   *P06:12,"3. ____   _________________________":
                   *P46:12,"4. ____   _________________________":
                   *P06:13,"5. ____   _________________________":
                   *P46:13,"6. ____   _________________________":
                   *P06:14,"7. ____   _________________________":
                   *P46:14,"8. ____   _________________________":
                   *P06:15,"9. ____   _________________________":
                   *P45:15,"10. ____   _________________________";
.
         MOVE      c0 TO KCTR
INDEX1
         ADD       c1 TO KCTR
         COMPARE   c1 TO KCTR
         GOTO      INDEX1A IF NOT EQUAL
         DISPLAY   *P01:20,*EL:
                   *P05:20,"#"ALL#" = COMPLETE FILE":
                   *P30:20,"#"MLR###" = MAILER NUMBER":
                   *P58:20,"#"^#" = BACK TO MAIN MENU";
INDEX1A
         COMPARE   "2" TO KCTR
         GOTO      INDEX1B IF NOT EQUAL
         DISPLAY   *P01:20,*EL:
                   *P05:20,"#"MLR###" = MAILER NUMBER":
                   *P31:20,"#"*#" = END OF SELECTIONS":
                   *P58:20,"#"^#" = BACK UP A LINE";
INDEX1B
         COMPARE   "11" TO KCTR
         GOTO      INDEX11 IF NOT LESS
.
         LOAD      VT FROM KCTR OF VT11,VT11,VT12,VT12,VT13:
                                   VT13,VT14,VT14,VT15,VT15
.
         LOAD      HZ FROM KCTR OF HZ1,HZ2,HZ1,HZ2,HZ1:
                                   HZ2,HZ1,HZ2,HZ1,HZ2
         MOVE      "7" TO DZ
         ADD       HZ TO DZ
.
         LOAD      KALERT FROM KCTR OF KALERT1,KALERT2,KALERT2,KALERT2,KALERT2:
                                       KALERT2,KALERT2,KALERT2,KALERT2,KALERT3
INDEX2
         KEYIN     *PDZ:VT,*DV,KALERT:
                   *PHZ:VT,*JR,*ZF,KLR;
.
         MATCH     "000^" TO KLR
         GOTO      INDEX3 IF EQUAL
         MATCH     "000*" TO KLR
         GOTO      INDEX4 IF EQUAL
         MATCH     "0ALL" TO KLR
         GOTO      INDEX5 IF EQUAL
         CMATCH    "0" TO KLR
         GOTO      INDEX8 IF EOS
         TYPE      KLR
         GOTO      INDEX9 IF EQUAL
         GOTO      INDEX6
.
INDEX3   COMPARE   c1 TO KCTR
         GOTO      BEGIN1 IF EQUAL
.
         LOAD      KLR FROM KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         MATCH     "****" TO KLR
         GOTO      INDEX3B IF NOT EQUAL
         DISPLAY   *PHZ:VT,"   *",*PDZ:VT,"END OF SELECTIONS!!      ";
         GOTO      INDEX3D
INDEX3B
         TYPE      KLR
         GOTO      INDEX3C IF NOT EQUAL
         GOTO      INDEX3C IF EOS
         DISPLAY   *PHZ:VT,KLR;
         DISPLAY   *PDZ:VT,"                         ";
         MOVE      KLR TO MLRNO
         READ      RSPMLR,MLRNO;MLRNO,MLRDESC
         MOVE      MLRDESC TO KALERT
         DISPLAY   *PDZ:VT,KALERT;
         GOTO      INDEX3D
INDEX3C
         DISPLAY   *PHZ:VT,"____";
         MOVE      "_________________________" TO KALERT
INDEX3D
         SUBTRACT  c1 FROM KCTR
         LOAD      VT FROM KCTR OF VT11,VT11,VT12,VT12,VT13:
                                   VT13,VT14,VT14,VT15,VT15
         LOAD      HZ FROM KCTR OF HZ1,HZ2,HZ1,HZ2,HZ1:
                                   HZ2,HZ1,HZ2,HZ1,HZ2
         MOVE      "7" TO DZ
         ADD       HZ TO DZ
         GOTO      INDEX2
.
INDEX4
         COMPARE   c1 TO KCTR
         GOTO      INDEX6 IF EQUAL
         MOVE      "****" TO KLR
         DISPLAY   *PHZ:VT,"   *",*PDZ:VT,"END OF SELECTIONS!   ";
         GOTO      INDEX10
INDEX5
         COMPARE   c1 TO KCTR
         GOTO      INDEX6 IF NOT EQUAL
         DISPLAY   *PHZ:VT," ALL":
                   *PDZ:VT,"PROCESS THE COMPLETE FILE";
         MOVE      "ALL " TO KLR
         GOTO      INDEX10
INDEX6
         DISPLAY   *PDZ:VT,*B,"INVALID!!                ",*W1:
                   *PDZ:VT,"_________________________":
                   *PHZ:VT,"____";
         GOTO      INDEX2
INDEX7
         DISPLAY   *PHZ:VT,"   *",*PDZ:VT,"END OF THE SELECTIONS!   ";
         MOVE      "****" TO KLR
         GOTO      INDEX10
INDEX8
         LOAD      KLR FROM KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         MATCH     "ALL " TO KLR
         GOTO      INDEX11 IF EQUAL
         CMATCH    "*" TO KLR
         GOTO      INDEX11 IF EQUAL
         TYPE      KLR
         GOTO      INDEX6 IF NOT EQUAL
         GOTO      INDEX1
.
INDEX9
         MOVE      c0 TO TCTR
INDEX9A
         ADD       c1 TO TCTR
         COMPARE   "11" TO TCTR
         GOTO      INDEX9C IF NOT LESS
         LOAD      TLR FROM TCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         MATCH     TLR TO KLR
         GOTO      INDEX9B IF EQUAL
         GOTO      INDEX9A
INDEX9B
         DISPLAY   *PDZ:VT,"DUPLICATE!!!             ",*W:
                   *PDZ:VT,"_________________________":
                   *PHZ:VT,"____";
         GOTO      INDEX2
INDEX9C
         DISPLAY   *PHZ:VT,KLR;
         DISPLAY   *PDZ:VT,"                         ";
         MOVE      KLR TO MLRNO
         READ      RSPMLR,MLRNO;MLRNO,MLRDESC
         GOTO      INDEX9D IF NOT OVER
         DISPLAY   *PHZ:VT,KLR:
                   *PDZ:VT,"NOT FOUND IN FILE!!      ",*B,*W1:
                   *PHZ:VT,"____";
         GOTO      INDEX2
INDEX9D
         STORE     KLR INTO KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         MOVE      MLRDESC TO KALERT
         DISPLAY   *PDZ:VT,KALERT;
         GOTO      INDEX1
INDEX10
         STORE     KLR INTO KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         MOVE      KCTR TO DCTR
INDEX10A
         ADD       c1 TO DCTR
         COMPARE   "11" TO DCTR
         GOTO      INDEX11 IF NOT LESS
         MOVE      "****" TO KLR
         STORE     KLR INTO DCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         LOAD      VT FROM DCTR OF VT11,VT11,VT12,VT12,VT13:
                                   VT13,VT14,VT14,VT15,VT15
         LOAD      HZ FROM DCTR OF HZ1,HZ2,HZ1,HZ2,HZ1:
                                   HZ2,HZ1,HZ2,HZ1,HZ2
         MOVE      "7" TO DZ
         ADD       HZ TO DZ
         DISPLAY   *PHZ:VT,"____",*PDZ:VT,"_________________________";
         GOTO      INDEX10A
INDEX11
         KEYIN     *P01:17,*EF:
                   *P32:17,"CORRECT? _  (Y,#"^#",N)":
                   *P11:20,"#"Y#" = RUN REPORTS":
                   *P31:20,"#"^#" = BACK UP TO LAST LINE":
                   *P61:20,"#"N#" = MAIN MENU":
                   *P41:17,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX20 IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      BEGIN1 IF EQUAL
         CMATCH    "^" TO ANS
         GOTO      INDEX12 IF NOT EQUAL
         LOAD      VT FROM KCTR OF VT11,VT11,VT12,VT12,VT13:
                                   VT13,VT14,VT14,VT15,VT15
         LOAD      HZ FROM KCTR OF HZ1,HZ2,HZ1,HZ2,HZ1:
                                   HZ2,HZ1,HZ2,HZ1,HZ2
         MOVE      "7" TO DZ
         ADD       HZ TO DZ
         MATCH     "ALL " TO KLR1
         GOTO      INDEX11A IF NOT EQUAL
         MOVE      "PROCESS THE COMPLETE FILE" TO KALERT
         GOTO      INDEX11B
INDEX11A
         MOVE      "END OF SELECTIONS!" TO KALERT
INDEX11B
         DISPLAY   *P01:17,*EF;
         GOTO      INDEX2
INDEX12
         BEEP
         GOTO      INDEX11
.
. INDEX20 - SELECT DESIRED REPORTS.
INDEX20
         MOVE      c0 TO PCTR
         KEYIN     *P1:17,*EF,*P32:17,"SELECT ALL YEARS ? ",ANS
         MOVE      "N" TO YRSEL
         CMATCH    "Y" TO ANS
         GOTO      INDEX20B IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      INDEX20A IF EQUAL
INDEX20A DISPLAY   *P32:19,"YEAR ONE   __":
                   *P32:20,"YEAR TWO   __":
                   *P32:21,"YEAR THREE __":
                   *P32:22,"YEAR FOUR  __":
                   *P32:23,"YEAR FIVE  __"
         MOVE      "Y" TO YRSEL
INDYR1   KEYIN     *P43:19,*JR,*ZF,YR1
         SCAN      "*" IN YR1
         GOTO      INDEX20B IF EQUAL
         SCAN      "^" IN YR1
         GOTO      INDEX20 IF EQUAL
         TYPE      YR1
         GOTO      INDEX20A IF NOT EQUAL
INDYR2   KEYIN     *P43:20,*JR,*ZF,YR2
         SCAN      "*" IN YR2
         GOTO      INDEX20B IF EQUAL
         SCAN      "^" IN YR2
         GOTO      INDYR1 IF EQUAL
         TYPE      YR2
         GOTO      INDYR2 IF NOT EQUAL
INDYR3   KEYIN     *P43:20,*JR,*ZF,YR3
         SCAN      "*" IN YR3
         GOTO      INDEX20B IF EQUAL
         SCAN      "^" IN YR3
         GOTO      INDYR2 IF EQUAL
         TYPE      YR3
         GOTO      INDYR3 IF NOT EQUAL
INDYR4   KEYIN     *P43:20,*JR,*ZF,YR2
         SCAN      "*" IN YR4
         GOTO      INDEX20B IF EQUAL
         SCAN      "^" IN YR4
         GOTO      INDYR3 IF EQUAL
         TYPE      YR4
         GOTO      INDYR4 IF NOT EQUAL
INDYR5   KEYIN     *P43:20,*JR,*ZF,YR5
         SCAN      "*" IN YR5
         GOTO      INDEX20B IF EQUAL
         SCAN      "^" IN YR5
         GOTO      INDYR5 IF EQUAL
         TYPE      YR5
         GOTO      INDYR5 IF NOT EQUAL
* *****************************************************************************
* DESCRIPTION OF PCTR VALUES:                                                 *
*                                                                             *
* 0 = PRINT ALL THREE REPORTS.                                                *
* 1 = PRINT STATISTICAL REPORT.                                               *
* 2 = PRINT SUMMARRY REPORT.                                                  *
* 3 = PRINT STATISTICAL & SUMMARY REPORTS.                                    *
* 4 = PRINT PACKAGE REPORT.                                                   *
* 5 = PRINT STATISTICAL & PACKAGE REPORTS.                                    *
* 6 = PRINT SUMMARRY & PACKAGE REPORTS.                                       *
* 7 + PRINT ALL THREE REPORTS.                                                *
*******************************************************************************
INDEX20B
         DISPLAY   *P01:17,*EF:
                   *P31:18,"* REPORT SELECTIONS *":
                   *P31:19,"    (Y,N) _  ALL REPORTS LISTED BELOW":
                   *P31:20,"1.  (Y,N) _  PROSPECTING STATISTICAL REPORT":
                   *P31:21,"2.  (Y,N) _  PROSPECTING SUMMARY REPORT":
                   *P31:22,"3.  (Y,N) _  PACKAGE REPORT":
                   *P31:23,"4.  (Y,N) _  PROS/STAT REPORT BY % RANK";
.
. INDEX21 - SELECT ALL REPORTS ?
INDEX21
         KEYIN     *P41:19,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX25 IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      INDEX22 IF EQUAL
         DISPLAY   *B,*P41:19,"_";
         GOTO      INDEX21
.
. INDEX22 - SELECT STATISTICAL REPORT ?
INDEX22
         KEYIN     *P41:20,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX26 IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      INDEX23 IF EQUAL
         DISPLAY   *B,*P41:20,"_";
         GOTO      INDEX22
.
. INDEX23 - SELECT SUMMARY REPORT ?
INDEX23
         KEYIN     *P41:21,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX27 IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      INDEX24 IF EQUAL
         DISPLAY   *B,*P41:21,"_";
         GOTO      INDEX23
.
. INDEX24 - SELECT PACKAGE REPORT ?
INDEX24
         KEYIN     *P41:22,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX28 IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      INDEX29 IF EQUAL
         DISPLAY   *B,*P41:22,"_";
         GOTO      INDEX24
.
.INDEX24A - SELECT STAT RANKED BY % RETN.
         KEYIN     *P41:23,ANS;
         CMATCH    "Y" TO ANS
         GOTO      INDEX29 IF NOT EQUAL
.         MOVE      ANS TO RANKSW
         GOTO      INDEX29
. INDEX25 - PRINT ALL REPORTS.
INDEX25
         MOVE      "7" TO PCTR
         GOTO      INDEX29
.
. INDEX26 - PRINT STATISTICAL REPORT.
INDEX26
         ADD       c1 TO PCTR
         GOTO      INDEX23
.
. INDEX27 - PRINT SUMMARY REPORT.
INDEX27
         ADD       "2" TO PCTR
         GOTO      INDEX24
.
. INDEX28 - PRINT PACKAGE REPORT.
INDEX28
         ADD       "4" TO PCTR
INDEX29
         COMPARE   c0 TO PCTR
.         GOTO      INDEX29A IF NOT EQUAL
         GOTO      SORTSEL
.
         DISPLAY   *P41:24,"NO REPORTS SELECTED!!!",*B,*W2;
         GOTO      INDEX11
.SORTSEL - SELECT SORT ON STAT REPORT.
SORTSEL
         DISPLAY   *P1:17,*EF:
                   *P31:18,"* SORT SELECTIONS *":
                   *P31:19,"1. STANDARD ":
                   *P31:20,"2. SORTED BY % (DESCENDING)":
                   *P31:21,"3. SORTED BY NET/M (DESCENDING)"
         KEYIN     *P41:23,"Choice : ",ANS
         CMATCH    " " TO ANS
         GOTO      SORTSEL IF EQUAL
         GOTO      SORTSEL IF EOS
         TYPE      ANS
         GOTO      SORTSEL IF NOT EQUAL
         MOVE      ANS TO SORTBR
         BRANCH    SORTBR OF INDEX29A,INDEX29A,INDEX29A
         GOTO      SORTSEL
.
INDEX29A
         DISPLAY   *ES:
                   *P18:24,"**** NAMES IN THE NEWS  RESPONSE ANALYSIS ****":
                   *R:
                   *P18:24,"     ====================================":
                   *R:
                   *P18:24,"       *** M A I L  A N A L Y S I S ***":
                   *R:
                   *P18:24,"           ** REPORT  GENERATION **":
                   *R:
                   *P24:24,"Tap the (F1) key to abort pick off";
.
         MOVE      c0 TO KCTR
. DID CLIENT SELECTION SPECIFY ALL CLIENTS?
         MATCH     "ALL " TO KLR1
         GOTO      INDEX40 IF EQUAL
.
.START PATCH 3.01 REPLACED LOGIC
.         PREP      RSPSRT1,"g:\data\plb\RSPSORT1"
.         PREP      RSPSRT1A,"g:\data\plb\RSPSRT1A"
.         PREP      RSPSRT2,"g:\data\plb\RSPSORT2"
.         PREP      RSPSRT3,"g:\data\plb\RSPSORT3"
.
        PACK    STR35,NTWKPATH1,"plb\RSPSORT1"
         PREP      RSPSRT1,STR35
        PACK    STR35,NTWKPATH1,"plb\RSPSRT1A"
         PREP      RSPSRT1A,STR35
        PACK    STR35,NTWKPATH1,"plb\RSPSORT2"
         PREP      RSPSRT2,STR35
        PACK    STR35,NTWKPATH1,"plb\RSPSORT3"
         PREP      RSPSRT3,STR35
.END PATCH 3.01 REPLACED LOGIC
.
         DISPLAY   *R;
         GOTO      INDEX30A
INDEX30
         DISPLAY   *HON:
                   *P18:24,"* #"S E L E C T I N G#"  TO  #"RSPSORTn#" FILES":
                   *HOFF:
                   *P18:24,"* #"S E L E C T I N G#"  TO  #"RSPSORTn#" FILES";
.
         READKS    RSPDET;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                          QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                          TOTAL$,COSTM,RTN
         GOTO      INDEX35 IF OVER
.
         MATCH     MLRNO TO KLR
         GOTO      INDEX30B IF EQUAL
.
INDEX30A
         ADD       c1 TO KCTR
         COMPARE   "12" TO KCTR
         GOTO      INDEX35 IF NOT LESS
.
         LOAD      KLR FROM KCTR OF KLR1,KLR2,KLR3,KLR4,KLR5:
                                    KLR6,KLR7,KLR8,KLR9,KLR10
         CMATCH    "*" TO KLR
         GOTO      INDEX35 IF EQUAL
         CLEAR     DETKEY
         APPEND    KLR TO DETKEY
         APPEND    "                 " TO DETKEY
         RESET     DETKEY
         READ      RSPDET,DETKEY;;
         GOTO      INDEX30
INDEX30B
         CMATCH    "Y" TO YRSEL
         GOTO      INDEX30C IF NOT EQUAL
         MATCH     YR1 TO MYY             *YR OK?
         GOTO      INDEX30C IF EQUAL      *YES
         MATCH     YR2 TO MYY             *YR OK?
         GOTO      INDEX30C IF EQUAL      *YES
         MATCH     YR3 TO MYY             *YR OK?
         GOTO      INDEX30C IF EQUAL      *YES
         MATCH     YR4 TO MYY             *YR OK?
         GOTO      INDEX30C IF EQUAL      *YES
         MATCH     YR5 TO MYY             *YR OK?
         GOTO      INDEX30C IF EQUAL      *YES
         GOTO      INDEX30
INDEX30C
.         GET CURRENT LIST INFO. (PARTIAL READ TO PREVENT ERASURE OF KDESC)
         READ      RSPKEY,KKEY;KKEY,LKEY
         CALL      READLIST IF NOT OVER
         BRANCH    PCTR TO INDEX31,INDEX32,INDEX31,INDEX33,INDEX31:
                           INDEX32,INDEX31
.
. INDEX31 - WRITE TO STATISTICAL REPORT OUTPUT FILE.
.
INDEX31
.CALC RETN %
         MOVE      RTN TO DIVQ
         DIVIDE    QTYM INTO DIVQ
         MULT      "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
.CALC  NET/M
         MOVE      TOTAL$ TO DIVQ
         DIV       QTYM INTO DIVQ
         MULT      "1000" BY DIVQ
         SUB       COSTM FROM DIVQ
         MOVE      DIVQ TO NET6
         MOVE      NET6 TO DIM7
         BRANCH    SORTBR OF INDEX31A,INDEX31A
         SCAN      "-" IN DIM7
         GOTO      NEG IF EQUAL
.
INDEX31A WRITE     RSPSRT1,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN,RTNPC,DIM7
.
         BRANCH    PCTR TO INDEX34,INDEX32,INDEX32,INDEX33,INDEX33:
                           INDEX32,INDEX32
NEG
         REP       "- " IN DIM7
         WRITE     RSPSRT1A,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN,RTNPC,DIM7
.
         BRANCH    PCTR TO INDEX34,INDEX32,INDEX32,INDEX33,INDEX33:
                           INDEX32,INDEX32
.
. INDEX32 - WRITE TO SUMMARY REPORT OUTPUT FILE.
.
INDEX32
         WRITE     RSPSRT2,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN
         FLUSH     RSPSRT2
.
         BRANCH    PCTR TO INDEX34,INDEX34,INDEX34,INDEX33,INDEX33:
                           INDEX33,INDEX33
.
. INDEX33 - WRITE TO PACKAGE REPORT OUTPUT FILE.
.
INDEX33
         WRITE     RSPSRT3,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN
.
. INDEX34 - GO GET NEXT DETAIL INPUT RECORD.
.
INDEX34
         DISPLAY   *P01:24,*EL;
         GOTO      INDEX30
.
. INDEX35 - PREPARE CHAIN FILE (CHNFLE).
.
INDEX35
         PREP      CHNFLE,"CHNFLE.bat"
.
         BRANCH    PCTR TO INDEX36,INDEX37,INDEX36,INDEX38:
                           INDEX36,INDEX37,INDEX36
.
. INDEX36 - WRITE END OF FILE STATISTICAL REPORT OUTPUT.
.           WRITE SORT FOR STAT REPORT TO CHNFLE/TXT.
.
INDEX36
         WEOF      RSPSRT1,SEQ
         WEOF      RSPSRT1A,SEQ
         FLUSH     RSPSRT1A
         CLOSE     RSPSRT1A
         FLUSH     RSPSRT1
         CLOSE     RSPSRT1
         BRANCH    SORTBR  OF INDEX36A,INDEX36B,INDEX36C
         DISPLAY   *P1:24,*EL,"BRANCH FAILED !!! STANDARD IS DEFAULT":
                   *B,*B,*W
.
.INDEX36A - STANDARD SORT
INDEX36A
.  WRITE     CHNFLE,SEQ;"SORT g:\data\plb\RSPSORT1.dat,g:\data\plb\RSPSORT1.srt;":
.                              "/s(1,4,c,a,45,44,c,a,112,44,c,a,40,5,c,a,100,12,c,a"
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT1.dat,g:\data\plb\RSPSORT1.NEW":
.                              " /s(1,4,c,a,45,44,c,a,112,44,c,a,40,5,c,a,100,12,c,a) VERBOSE W(C:)"
.         EXECUTE   TASKNAME
.         ERASE     "G:\DATA\PLB\RSPSORT1.DAT"
.         RENAME    "G:\DATA\PLB\RSPSORT1.NEW","G:\DATA\PLB\RSPSORT1.DAT"
.
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT1.dat,",NTWKPATH1,"plb\RSPSORT1.NEW":
                              " /s(1,4,c,a,45,44,c,a,112,44,c,a,40,5,c,a,100,12,c,a) VERBOSE W(C:)"
         EXECUTE   TASKNAME
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.DAT"
         ERASE     STR35
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.NEW"
         PACK   STR45,NTWKPATH1,"PLB\RSPSORT1.DAT"
         RENAME    STR35,STR45
.END PATCH 3.01 REPLACED LOGIC
.
         GOTO      INDEX36X
.INDEX36B - SORT BY RTN%
INDEX36B
. WRITE     CHNFLE,SEQ;"SORT RSPSORT1,RSPSORT1/NEW;":
.                   "D,201-206,A,1-4,45-49,112-156,"
.         WRITE     CHNFLE,SEQ;"40-44,100-111"
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT1.DAT"
.         WRITE     CHNFLE,SEQ;"RENAME RSPSORT1/NEW,.DAT"
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT1.dat g:\data\plb\RSPSORT1.NEW":
.                              " /s(201,6,c,D,1,4,c,a,44,5,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE "
.         EXECUTE   TASKNAME
.         ERASE     "G:\DATA\PLB\RSPSORT1.DAT"
.         RENAME    "G:\DATA\PLB\RSPSORT1.NEW","G:\DATA\PLB\RSPSORT1.DAT"
.
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT1.dat ",NTWKPATH1,"plb\RSPSORT1.NEW":
                              " /s(201,6,c,D,1,4,c,a,44,5,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE "
         EXECUTE   TASKNAME
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.DAT"
         ERASE     STR35
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.NEW"
         PACK   STR45,NTWKPATH1,"PLB\RSPSORT1.DAT"
         RENAME    STR35,STR45
.END PATCH 3.01 REPLACED LOGIC
         GOTO      INDEX36X
.INDEX36C - SORT BY NET/M
INDEX36C
. WRITE     CHNFLE,SEQ;"SORT RSPSORT1,RSPSORT1/NEW":
.                   ";D,207-213,A,1-4,45-49,112-156,40-44,100-111"
.         WRITE     CHNFLE,SEQ;"SORT RSPSRT1A,RSPSRT1A/NEW":
.                   ";207-213,A,1-4,45-49,112-156,40-44,100-111"
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT1.DAT"
.         WRITE     CHNFLE,SEQ;"BUILD RSPSORT1.DAT"
.         WRITE     CHNFLE,SEQ;"OPEN RSPSORT1/NEW"
.         WRITE     CHNFLE,SEQ;"COPY FIRST TO LAST"
.         WRITE     CHNFLE,SEQ;"OPEN RSPsrt1a/NEW"
.         WRITE     CHNFLE,SEQ;"COPY FIRST TO LAST"
.         WRITE     CHNFLE,SEQ;"END"
.         WRITE     CHNFLE,SEQ;"DELETE RSPSoRT1/NEW"
.         WRITE     CHNFLE,SEQ;"DELETE RSPsrt1a/NEW"
.         WRITE     CHNFLE,SEQ;"DELETE RSPsrt1a.DAT"
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT1.dat,g:\data\plb\RSPSORT1.NEW":
.                              " /s(207,7,c,D,1,4,c,a,45,5,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
.         EXECUTE   TASKNAME
.         ERASE     "G:\DATA\PLB\RSPSORT1.DAT"
.         RENAME    "G:\DATA\PLB\RSPSORT1.NEW","G:\DATA\PLB\RSPSORT1.DAT"
.
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT1.dat,",NTWKPATH1,"plb\RSPSORT1.NEW":
                              " /s(207,7,c,D,1,4,c,a,45,5,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
         EXECUTE   TASKNAME
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.DAT"
         ERASE     STR35
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.NEW"
         PACK   STR45,NTWKPATH1,"PLB\RSPSORT1.DAT"
         RENAME    STR35,STR45
.END PATCH 3.01 REPLACED LOGIC
.
INDEX36X BRANCH    PCTR TO INDEX50,INDEX37,INDEX37,INDEX38:
                           INDEX38,INDEX37,INDEX37
.
. INDEX37 - WRITE END OF FILE AND SORT FOR SUMMARY.
.
INDEX37
         WEOF      RSPSRT2,SEQ
         FLUSH     RSPSRT2
         CLOSE     RSPSRT2
.
.         WRITE     CHNFLE,SEQ;"SORT RSPSORT2,RSPSORT2/NEW":
.                              "1-4,D169-170,165-168,A40-44,100-111"
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT2.DAT"
.         WRITE     CHNFLE,SEQ;"RENAME RSPSORT2/NEW,.DAT"
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT2.dat,g:\data\plb\RSPSORT2.NEW":
.                              " /s(1,4,c,A,169,2,c,D,165,4,c,D,40,5,c,a,100,12,c,a) VERBOSE W(C:)"
.         EXECUTE   TASKNAME
.         ERASE     "G:\DATA\PLB\RSPSORT2.DAT"
.         RENAME    "G:\DATA\PLB\RSPSORT2.NEW","G:\DATA\PLB\RSPSORT2.DAT"
.
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT2.dat,",NTWKPATH1,"plb\RSPSORT2.NEW":
                              " /s(1,4,c,A,169,2,c,D,165,4,c,D,40,5,c,a,100,12,c,a) VERBOSE W(C:)"
         EXECUTE   TASKNAME
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT2.DAT"
         ERASE     STR35
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT2.NEW"
         PACK   STR45,NTWKPATH1,"PLB\RSPSORT2.DAT"
         RENAME    STR35,STR45
.END PATCH 3.01 REPLACED LOGIC
.
         BRANCH    PCTR TO INDEX50,INDEX50,INDEX50,INDEX38:
                           INDEX38,INDEX38,INDEX38
.
. INDEX38 - WRITE END OF FILE AND SORT FOR PACKAGE REPORT.
.
INDEX38
         WEOF      RSPSRT3,SEQ
         FLUSH     RSPSRT3
         CLOSE     RSPSRT3
INDEX39
.         WRITE     CHNFLE,SEQ;"SORT RSPSORT3,RSPSORT3/NEW;":
.                              "1-4,112-156,100-111,40-44"
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT3.DAT"
.         WRITE     CHNFLE,SEQ;"RENAME RSPSORT3/NEW,.DAT"
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT3.dat,g:\data\plb\RSPSORT3.NEW":
.                              " /s(1,4,c,a,100,12,c,a,40,5,C,A) VERBOSE W(C:)"
.         EXECUTE   TASKNAME
.         ERASE     "G:\DATA\PLB\RSPSORT3.DAT"
.         RENAME    "G:\DATA\PLB\RSPSORT3.NEW","G:\DATA\PLB\RSPSORT3.DAT"
.
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT3.dat,",NTWKPATH1,"plb\RSPSORT3.NEW":
                              " /s(1,4,c,a,100,12,c,a,40,5,C,A) VERBOSE W(C:)"
         EXECUTE   TASKNAME
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT3.DAT"
         ERASE     STR35
         PACK   STR35,NTWKPATH1,"PLB\RSPSORT3.NEW"
         PACK   STR45,NTWKPATH1,"PLB\RSPSORT3.DAT"
         RENAME    STR35,STR45
.END PATCH 3.01 REPLACED LOGIC
         GOTO      INDEX50
.
. .............................................................................
. THE INDEX40'S SUBROUTINES ARE USED WHEN ALL CLIENTS ON FILE ARE SELECTED.
. .............................................................................
.
INDEX40
.         PREP      CHNFLE,"CHNFLE.bat",CREATE
.
         BRANCH    PCTR TO INDEX41,INDEX42,INDEX41,INDEX43:
                           INDEX41,INDEX42,INDEX41
.
INDEX41
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\TEXT\plb\RSPSORT.dat,g:\data\plb\RSPSORT1.DAT;":
.                              " /s(1,4,c,a,45,54,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"TEXT\plb\RSPSORT.dat,",NTWKPATH1,"plb\RSPSORT1.DAT;":
                              " /s(1,4,c,a,45,54,c,a,112,44,c,a,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
.END PATCH 3.01 REPLACED LOGIC
         EXECUTE   TASKNAME
.         WRITE      CHNFLE,SEQ;"SORT RSPDET,RSPSORT1;":
.                              "1-4,45-99,112-156,40-44,100-111"
.
         BRANCH    PCTR TO INDEX50,INDEX42,INDEX42,INDEX43:
                           INDEX43,INDEX42,INDEX42
.
INDEX42
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT1.dat,g:\data\plb\RSPSORT2.DAT;":
.                              " /s(1,4,c,a,169,2,c,D,165,4,c,D,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT1.dat,",NTWKPATH1,"plb\RSPSORT2.DAT;":
                              " /s(1,4,c,a,169,2,c,D,165,4,c,D,40,5,C,A,100,12,c,a) VERBOSE W(C:)"
.END PATCH 3.01 REPLACED LOGIC
         EXECUTE   TASKNAME
.         WRITE     CHNFLE,SEQ;"SORT RSPDET,RSPSORT2;":
.                              "1-4,D169-170,165-168,A40-44,100-111"
.
         BRANCH    PCTR TO INDEX50,INDEX50,INDEX50,INDEX43:
                           INDEX43,INDEX43,INDEX43
.
INDEX43
         CLEAR     TASKNAME
.START PATCH 3.01 REPLACED LOGIC
.         PACK      TASKNAME FROM "SORT g:\data\plb\RSPSORT1.dat,g:\data\plb\RSPSORT3.DAT;":
.                              " /s(1,4,c,a,112,44,c,a,100,12,c,a,40,5,C,A) VERBOSE W(C:)"
         PACK      TASKNAME FROM "SORT ",NTWKPATH1,"plb\RSPSORT1.dat,",NTWKPATH1,"plb\RSPSORT3.DAT;":
                              " /s(1,4,c,a,112,44,c,a,100,12,c,a,40,5,C,A) VERBOSE W(C:)"
.END PATCH 3.01 REPLACED LOGIC
         EXECUTE   TASKNAME
.         WRITE     CHNFLE,SEQ;"SORT RSPDET,RSPSORT3;":
.                              "1-4,112-156,100-111,40-44"
.
* *****************************************************************************
* INDEX50'S - SET UP FOR PRINTING.
* ********************************
.
. INDEX50 - WRITE END OF FILE FOR CHNFLE, ROLLOUT AND PERFORM CHAIN.
.
INDEX50
.         WRITE     CHNFLE,SEQ;"DELETE CHNFLE.bat"
.         WRITE     CHNFLE,SEQ;"DATABUS ",ROLL,";ROLLBACK"
.                              "DATABUS;ROLLBACK"
.         WEOF      CHNFLE,SEQ
.
.         DISPLAY   *R:
.                   *P20:24,"* ROLLING OUT TO  SORT #"RSPSORTn#" FILES *";
.
.         ROLLOUT   "CHAIN CHNFLE.bat",ROLL
.
.         DISPLAY   *P1:1,*ES:
.                   *P18:23,"       * END OF THE  ROLL-OUT *":
.                   *R;
         MOVE      c0 TO KCTR
         MOVE      "    " TO KLR
INDEX51
         TRAP      INDEX52 IF IO
         OPEN      RSPSRT1,"RSPSORT1"
         TRAPCLR   IO
INDEX52
         TRAP      INDEX53 IF IO
         OPEN      RSPSRT2,"RSPSORT2"
         TRAPCLR   IO
INDEX53
         TRAP      INDEX54 IF IO
         OPEN      RSPSRT3,"RSPSORT3"
INDEX54
         TRAPCLR   IO
INDEX55
         DISPLAY   *P30:24,"SETTING UP TO PRINT";
.
         MOVE      c0 TO KCTR
INDEX56
         ADD       c1 TO KCTR
         COMPARE   "20" TO KCTR
         GOTO      INDEX57 IF NOT LESS
         PRINT     *N,"                                                 "
         GOTO      INDEX56
INDEX57
         BRANCH    PCTR TO REPORT1,REPORT2,REPORT1,REPORT3:
                           REPORT1,REPORT2,REPORT1
* *****************************************************************************
* READ SORTED FILES AND PRINT.
* ****************************
. REPORT1 - STATISTICAL REPORT. INCLUDES ALL INDEX60 SUBROUTINES.
. ...............................................................
REPORT1
         MOVE      c0 TO ONETIME
INDEX60
         READ      RSPSRT1,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN
         GOTO      INDEX60A IF NOT OVER
         CALL      INDEX68
         MOVE      c0 TO ONETIME
         GOTO      REPORT2
INDEX60A
         CMATCH    "1" TO ONETIME
         GOTO      INDEX60B IF EQUAL
         MOVE      c1 TO ONETIME
         MOVE      MLRDESC TO MLRNAME
         MOVE      MLRNO TO MLRHOLD
         CALL      HEAD11
INDEX60B
         DISPLAY   *P30:24,"* P R I N T I N G *";
         MATCH     MLRNO TO MLRHOLD
         GOTO      INDEX61 IF EQUAL
         CALL      INDEX68
         MOVE      MLRDESC TO MLRNAME
         MOVE      MLRNO TO MLRHOLD
         CALL      HEAD11
INDEX61
         COMPARE   "18" TO LINES
         CALL      HEAD11 IF NOT LESS
INDEX62
         MOVE      c0 TO KCTR
INDEX63
         ADD       c1 TO KCTR
         COMPARE   "5" TO KCTR
         GOTO      INDEX64 IF NOT LESS
         RESET     KKEY TO KCTR
         CMATCH    "0" TO KKEY
         GOTO      INDEX64 IF NOT EQUAL
         CMOVE     " " TO KKEY
         GOTO      INDEX63
INDEX64
. STATISTICAL REPORT
         RESET     KKEY
         MOVE      "ZZ,ZZZ,ZZ9" TO QTYMASK
         EDIT      QTYM TO QTYMASK
         MOVE      "ZZ,ZZZ,ZZ9" TO RTNMASK
         EDIT      RTN TO RTNMASK
         MOVE      "Z,ZZZ,ZZ9.99" TO TOTMASK
         EDIT      TOTAL$ TO TOTMASK
.
         MOVE      RTN TO DIVQ
         DIVIDE    QTYM INTO DIVQ
         MULTIPLY  "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
.
         MOVE      TOTAL$ TO DIVQ
         DIVIDE    RTN INTO DIVQ
         MOVE      DIVQ TO GIFT
.
         MOVE      TOTAL$ TO DIVQ
         DIVIDE    QTYM INTO DIVQ
         MULTIPLY  "1000" BY DIVQ
         MOVE      DIVQ TO RTN5
.
         SUBTRACT  COSTM FROM DIVQ
         MOVE      DIVQ TO NET6
         MOVE      "-$$$$$.Z9" TO NET6MASK
         EDIT      NET6 TO NET6MASK
.
         ADD       QTYM TO GQTY
         ADD       RTN TO GRTN
         ADD       TOTAL$ TO GTOT
         ADD       COSTM TO GCOST
         ADD       c1 TO COSTNUM
.
         PRINT     *01,KDESC:
                   *61,KKEY:
                   *65,QTYMASK:
                   *77,RMO,"/",RDD,"/",RYY:
                   *85,RTNPC:
                   *N:
                   *11,PDESC:
                   *55,PKG:
                   *67,MMO,"/",MDD,"/",MYY:
                   *75,RTNMASK:
                   *90,TOTMASK:
                   *102,GIFT:
                   *110,COSTM:
                   *117,RTN5:
                   *124,NET6MASK:
                   *N
.
         ADD       c1 TO LINES
         DISPLAY   *P30:24,*EL;
         GOTO      INDEX60
.
INDEX68
. STATISTICAL REPORT TOTALS
         CALL      INDEXTOT
         PRINT     *N:
                   *49,"TOTALS:":
                   *62,GQTYMASK:
                   *85,RTNPC:
                   *102,GIFT:
                   *N:
                   *72,GRTNMASK:
                   *88,GTOTMASK:
                   *107,GCOST:
                   *117,RTN5:
                   *124,NET6MASK:
                   *N
INDEX69
         MOVE      c0 TO GQTY
         MOVE      c0 TO GRTN
         MOVE      c0 TO GTOT
         MOVE      c0 TO GCOST
         MOVE      c0 TO COSTNUM
         RETURN
.
INDEXTOT
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GQTYMASK
         EDIT      GQTY TO GQTYMASK
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GRTNMASK
         EDIT      GRTN TO GRTNMASK
         MOVE      "ZZZ,ZZZ,ZZ9.99" TO GTOTMASK
         EDIT      GTOT TO GTOTMASK
         MOVE      GRTN TO DIVQ
         DIVIDE    GQTY INTO DIVQ
         MULTIPLY  "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
         MOVE      GTOT TO DIVQ
         DIVIDE    GRTN INTO DIVQ
         MOVE      DIVQ TO GIFT
         MOVE      GTOT TO DIVQ
         DIVIDE    GQTY INTO DIVQ
         MULTIPLY  "1000" BY DIVQ
         MOVE      DIVQ TO RTN5
         DIVIDE    COSTNUM INTO GCOST
         SUBTRACT  GCOST FROM DIVQ
         MOVE      DIVQ TO NET6
.         MOVE      "ZZZZ9-" TO NET5MASK
         MOVE      "-$$$$$.Z9" TO NET6MASK
         EDIT      NET6 TO NET6MASK
         RETURN
.
. ...................................................
. HEADER - MASTER REPORT HEADING USED ON ALL REPORTS.
. ...................................................
HEADER
         MOVE      "66" TO HZ
         MOVEFPTR  MLRNAME TO FPTR
         MOVELPTR  MLRNAME TO LPTR
         SUBTRACT  FPTR FROM LPTR
         ADD       c1 TO LPTR
         DIVIDE    "2" INTO LPTR
         SUBTRACT  LPTR FROM HZ
.
         ADD       c1 TO PAGENO
         COMPARE   C1 TO PAGENO
         IF        EQUAL
.START PATCH 3.01 REPLACED LOGIC
.         SPLOPEN   "G:\DATA\PLB\RSPDET.LST"
         PACK   STR35,NTWKPATH1,"PLB\RSPDET.LST"
         SPLOPEN   STR35
.end PATCH 3.01 REPLACED LOGIC
         PRINT     HP17PTCH,HPTOP
         ENDIF
         PRINT     *F:
                   *44,"**",TITLEDES,"  - RESPONSE ":
                       "ANALYSIS **":
                   *N,*10,"DATE: ",TODAY,*124,"PAGE",PAGENO
         RETURN
HEAD11
. STATISTICAL REPORT HEADER
         CALL      HEADER
         MOVE      c0 TO LINES
         MOVE      MLRNO TO MLRHOLD
         PRINT     *55,"* STATISTICAL  REPORT *":
                   *N,*N:
                   *HZ,MLRNAME:
                   *N,*N:
                   *01,"LIST NAME",*63,"KEY",*67,"QTY MAIL":
                   *76,"DATE FIRST    %":
                   *N:
                   *11,"PACKAGE DESCRIPTION",*57,"PKG",*69,"DATE":
                   *78,"RETURNS  RTNS":
                   *95,"TOTAL $":
                   *104,"GIFT",*109,"COST/M",*117,"RTN/M",*128,"NET/M":
                   *N,"---------------------------":
                      "----------------------------":
                   *57,"--------- -------- --------- -----":
                   *92,"---------- ----- ------  ------ ---------"
         RETURN
.
* ************************************************
. REPORT2 & INDEX70 SERIES - PRINT SUMMARY REPORT.
* ************************************************
REPORT2
         BRANCH    PCTR TO REPORT3,INDEX70,INDEX70,REPORT3:
                           REPORT3,INDEX70,INDEX70
INDEX70
. PROSPECTING REPORT SECTION INDEX70-79
         MOVE      c0 TO ONETIME
INDEX71
         DISPLAY   *P30:24,"* P R I N T I N G *";
         READ      RSPSRT2,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN
         GOTO      INDEX72 IF NOT OVER
         CALL      INDEX76
         CALL      INDEX78
         MOVE      c0 TO ONETIME
         GOTO      REPORT3
INDEX72
         CLEAR     DATESUMM
         PACK      DATESUMM FROM MMO,SLASH,MDD,SLASH,MYY
.
         CLEAR     DATERSP
         PACK      DATERSP FROM RMO,SLASH,RDD,SLASH,RYY
.
         CMATCH    "1" TO ONETIME
         GOTO      INDEX72A IF EQUAL
         MOVE      c1 TO ONETIME
         MOVE      MLRNO TO MLRHOLD
         MOVE      MLRDESC TO MLRNAME
         MOVE      DATESUMM TO DATEHOLD
         CALL      HEAD22
INDEX72A
         COMPARE   "51" TO LINES
         GOTO      INDEX73 IF LESS
         CALL      HEAD22
INDEX73
         MATCH     MLRNO TO MLRHOLD
         GOTO      INDEX74 IF EQUAL
         CALL      INDEX76
         CALL      INDEX78
         CALL      HEAD22
         MOVE      DATESUMM TO DATEHOLD
INDEX74
         MATCH     DATESUMM TO DATEHOLD
         CALL      INDEX76 IF NOT EQUAL
INDEX75
         ADD       QTYM TO TQTY
         ADD       QTYM TO GQTY
         ADD       RTN TO TRTN
         ADD       RTN TO GRTN
         ADD       TOTAL$ TO TTOT
         ADD       TOTAL$ TO GTOT
         ADD       COSTM TO TCOST
         ADD       COSTM TO GCOST
         ADD       c1 TO COSTNUM
         ADD       c1 TO NUMPERM
         DISPLAY   *P30:24,*EL;
         GOTO      INDEX71
INDEX76
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GQTYMASK
         EDIT      TQTY TO GQTYMASK
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GRTNMASK
         EDIT      TRTN TO GRTNMASK
.
         MOVE      TRTN TO DIVQ
         DIVIDE    TQTY INTO DIVQ
         MULTIPLY  "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
.
         MOVE      "ZZZ,ZZZ,ZZ9.99" TO GTOTMASK
         EDIT      TTOT TO GTOTMASK
.
         MOVE      TTOT TO DIVQ
         DIVIDE    TRTN INTO DIVQ
         MOVE      DIVQ TO GIFT
.
         MOVE      TTOT TO DIVQ
         DIVIDE    TQTY INTO DIVQ
         MULTIPLY  "1000" BY DIVQ
         MOVE      DIVQ TO RTN5
         DIVIDE    NUMPERM INTO TCOST    CALC AVER TOTAL COST/M
         SUBTRACT  TCOST FROM DIVQ
         MOVE      DIVQ TO NET6
.         MOVE      "ZZZZ9-" TO NET5MASK
         MOVE      "-$$$$$.Z9" TO NET6MASK
         EDIT      NET6 TO NET6MASK
.
         PRINT     *10,DATEHOLD," - ",DATERSP:
                   *37,GQTYMASK:
                   *51,GRTNMASK:
                   *65,RTNPC:
                   *73,GTOTMASK:
                   *88,GIFT:
                   *102,TCOST:
                   *112,RTN5:
                   *122,NET6MASK:
                   *N
         ADD       "2" TO LINES
         MOVE      c0 TO TQTY
         MOVE      c0 TO TRTN
         MOVE      c0 TO TTOT
         MOVE      c0 TO TCOST
         MOVE      "0000" TO NUMPERM
         MOVE      DATESUMM TO DATEHOLD
         RETURN
.
INDEX78
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GQTYMASK
         EDIT      GQTY TO GQTYMASK
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GRTNMASK
         EDIT      GRTN TO GRTNMASK
         MOVE      "ZZZ,ZZZ,ZZ9.99" TO GTOTMASK
         EDIT      GTOT TO GTOTMASK
         MOVE      GTOT TO DIVQ
         DIVIDE    GRTN INTO DIVQ
         MOVE      DIVQ TO GIFT
         MOVE      GRTN TO DIVQ
         DIVIDE    GQTY INTO DIVQ
         MULTIPLY  "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
.
         MOVE      GTOT TO DIVQ
         DIVIDE    GQTY INTO DIVQ
         MULTIPLY  "1000" BY DIVQ
         MOVE      DIVQ TO RTN5
         DIVIDE    COSTNUM INTO GCOST
         SUBTRACT  GCOST FROM DIVQ
         MOVE      DIVQ TO NET6
.         MOVE      "ZZZZ9-" TO NET5MASK
         MOVE      "-$$$$$.Z9",NET6MASK
         EDIT      NET6 TO NET6MASK
.
         PRINT     *N:
                   *22,"TOTALS:":
                   *37,GQTYMASK:
                   *51,GRTNMASK:
                   *65,RTNPC:
                   *73,GTOTMASK:
                   *88,GIFT:
                   *102,GCOST:
                   *112,RTN5:
                   *122,NET6MASK:
                   *N
.
         MOVE      c0 TO GQTY
         MOVE      c0 TO GRTN
         MOVE      c0 TO GTOT
         MOVE      c0 TO GCOST
         MOVE      c0 TO COSTNUM
         RETURN
.
HEAD22
         CALL      HEADER
         MOVE      c0 TO LINES
         PRINT     *55,"* PROSPECTING SUMMARY *":
                   *N,*N:
                   *HZ,MLRNAME:
                   *N,*N:
                   *12,"MAILING  PERIOD":
                   *40,"QTY MAILED":
                   *57,"RETURNS":
                   *64,"   -  %":
                   *80,"TOTAL $":
                   *90,"GIFT":
                   *105,"COST/M":
                   *113,"RTN/M":
                   *120,"INCOME/M":
                   *N:
                   *10,"-------------------":
                   *40,"----------       -------  -----":
                   *77,"----------  -----":
                   *105,"------":
                   *113,"-----":
                   *120,"--------"
.
         MOVE      MLRNO TO MLRHOLD
         MOVE      MLRDESC TO MLRNAME
         RETURN
* *****************************************************************************
* REPORT3 & INDEX80 SERIES - PRINT PACKAGE REPORT.
* *****************************************************************************
.
REPORT3
         BRANCH    PCTR TO INDEX90,INDEX90,INDEX90,INDEX80:
                           INDEX80,INDEX80,INDEX80
INDEX80
. INDEX80-89 PACKAGE SUMMARY REPORT
         MOVE      c0 TO ONETIME
INDEX81
         DISPLAY   *P30:24,"* P R I N T I N G *";
         READ      RSPSRT3,SEQ;MLRNO,MLRDESC,KKEY,KDESC,PKG,PDESC:
                               QTYM,MMO,MDD,MYY,RMO,RDD,RYY:
                               TOTAL$,COSTM,RTN
         GOTO      INDEX82 IF NOT OVER
         CALL      INDEX85
         CALL      INDEX88
         GOTO      INDEX90
INDEX82
         CMATCH    "1" TO ONETIME
         GOTO      INDEX83 IF EQUAL
         MOVE      c1 TO ONETIME
         MOVE      MLRNO TO MLRHOLD
         MOVE      PKG TO PHOLD
         MOVE      PDESC TO DESC45
         MOVE      MLRDESC TO MLRNAME
         CALL      HEAD33
INDEX83
         COMPARE   "50" TO LINES
         CALL      HEAD33 IF NOT LESS
         MATCH     MLRNO TO MLRHOLD
         GOTO      INDEX84 IF EQUAL
         CALL      INDEX85
         CALL      INDEX88
         CALL      HEAD33
INDEX84
         MATCH     PKG TO PHOLD
         CALL      INDEX85 IF NOT EQUAL
         ADD       QTYM TO TQTY
         ADD       QTYM TO GQTY
         ADD       RTN TO TRTN
         ADD       RTN TO GRTN
         ADD       TOTAL$ TO TTOT
         ADD       TOTAL$ TO GTOT
         ADD       COSTM TO TCOST
         ADD       COSTM TO GCOST
         ADD       c1 TO COSTNUM
         ADD       c1 TO NUMPERM
         DISPLAY   *P30:24,*EL;
         GOTO      INDEX81
INDEX85
. PACKAGE REPORT CALC & PRINT
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GQTYMASK
         EDIT      TQTY TO GQTYMASK
         MOVE      "Z,ZZZ,ZZZ,ZZ9" TO GRTNMASK
         EDIT      TRTN TO GRTNMASK
         MOVE      "ZZZ,ZZZ,ZZ9.99" TO GTOTMASK
         EDIT      TTOT TO GTOTMASK
.
         MOVE      TRTN TO DIVQ
         DIVIDE    TQTY INTO DIVQ
         MULTIPLY  "100" BY DIVQ
         MOVE      DIVQ TO RTNPC
.
         MOVE      TTOT TO DIVQ
         DIVIDE    TRTN INTO DIVQ
         MOVE      DIVQ TO GIFT
.
         MOVE      TTOT TO DIVQ
         DIVIDE    TQTY INTO DIVQ
         MULTIPLY  "1000" BY DIVQ
         MOVE      DIVQ TO RTN5
.
         DIVIDE    NUMPERM INTO TCOST
         SUBTRACT  TCOST FROM DIVQ            CALC NET/M
         MOVE      DIVQ TO NET6
.         MOVE      "ZZZZ9-" TO NET5MASK
         MOVE      "-$$$$$.Z9" TO NET6MASK
         EDIT      NET6 TO NET6MASK
.
         PRINT     *01,PHOLD:
                   *14,DESC45:
                   *53,GQTYMASK:
                   *63,GRTNMASK:
                   *77,RTNPC:
                   *84,GTOTMASK:
                   *98,GIFT:
                   *104,TCOST:
                   *118,RTN5:
                   *124,NET6MASK:
                   *N
.
         ADD       c1 TO LINES
         MOVE      c0 TO TQTY
         MOVE      c0 TO TRTN
         MOVE      c0 TO TTOT
         MOVE      c0 TO TCOST
         MOVE      "0000" TO NUMPERM
         MOVE      MLRNO TO MLRHOLD
         MOVE      MLRDESC TO MLRNAME
         MOVE      PKG TO PHOLD
         MOVE      PDESC TO DESC45
         RETURN
.
INDEX88
         CALL      INDEXTOT
         PRINT     *N:
                   *42,"TOTALS:":
                   *53,GQTYMASK:
                   *63,GRTNMASK:
                   *77,RTNPC:
                   *84,GTOTMASK:
                   *98,GIFT:
                   *104,GCOST:
                   *118,RTN5:
                   *124,NET6MASK:
                   *N
         MOVE      c0 TO GQTY
         MOVE      c0 TO GRTN
         MOVE      c0 TO GTOT
         MOVE      c0 TO GCOST
         MOVE      c0 TO COSTNUM
         RETURN
.
HEAD33
         CALL      HEADER
         MOVE      c0 TO LINES
         MOVE      MLRNO TO MLRHOLD
         PRINT     *57,"* PACKAGE REPORT *":
                   *N,*N:
                   *HZ,MLRNAME:
                   *N,*N:
                   *01,"PACKAGE      DESCRIPTION":
                   *59,"QUANITY":
                   *69,"RETURNS":
                   *79,"-  %":
                   *91,"TOTAL $":
                   *100,"GIFT":
                   *107,"COST/M":
                   *116,"RETURN/M":
                   *125,"INCOME/M":
                   *N:
                   *01,"------------ -----------------------------------":
                   *52,"    ---------- ---------  -----  -------------":
                   *99,"-----   ------   --------":
                   *125,"--------"
         RETURN
.
.
NOFILE   DISPLAY   *P01:24,*R,*EL,NOFILE,"  IS OFF LINE NOTIFY DP PERSONEL";
         BEEP
         KEYIN     *P79:23,ANS;
         NORETURN
         stop
INDEX90
         PRINT     *F
         DISPLAY   *R,*P30:24,"** END OF PRINTING **":
                   *R:
                   *P21:24,"* ROLLING OUT TO KILL  #"RSPSORTn#"  FILES *";
         RELEASE
.         PREP      CHNFLE,"CHNFLE.bat",CREATE
.         BRANCH    PCTR TO INDEX91,INDEX92,INDEX91,INDEX93:
.                           INDEX91,INDEX92,INDEX91
INDEX91
..         WRITE     CHNFLE,SEQ;"DELETE RSPSORT1.DAT"
.START PATCH 3.01 REPLACED LOGIC
.          ERASE     "G:\DATA\PLB\RSPSORT1.DAT"
        PACK   STR35,NTWKPATH1,"PLB\RSPSORT1.DAT"
          ERASE     STR35
.END PATCH 3.01 REPLACED LOGIC
.         BRANCH    PCTR TO INDEX92,INDEX92,INDEX92,INDEX93:
.                           INDEX93,INDEX92,INDEX92
INDEX92
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT2.DAT"
.START PATCH 3.01 REPLACED LOGIC
.          ERASE     "G:\DATA\PLB\RSPSORT2.DAT"
        PACK   STR35,NTWKPATH1,"PLB\RSPSORT2.DAT"
          ERASE     STR35
.END PATCH 3.01 REPLACED LOGIC
.         BRANCH    PCTR TO INDEX92,INDEX94,INDEX94,INDEX93:
.                           INDEX93,INDEX93,INDEX93
INDEX93
.         WRITE     CHNFLE,SEQ;"DELETE RSPSORT3.DAT"
.START PATCH 3.01 REPLACED LOGIC
.          ERASE     "G:\DATA\PLB\RSPSORT3.DAT"
        PACK   STR35,NTWKPATH1,"PLB\RSPSORT3.DAT"
          ERASE     STR35
.END PATCH 3.01 REPLACED LOGIC
INDEX94
.         WRITE     CHNFLE,SEQ;"DATABUS ",ROLL,";ROLLBACK"
.
.         WEOF      CHNFLE,SEQ
.
.         ROLLOUT   "CHNFLE.bat",ROLL
         STOP
.
. INTERROR - TRAP INT KEY.
INTERROR
         NORETURN
         TRAPCLR   INT
         TRAP      INTERROR IF INT
AGAIN
         DISPLAY   *P12:15,*EF,"**********************************":
                   *P12:16,*B,"*INT ERROR CALL COMPUTER PERSONEL*":
                   *P12:15,*B,"**********************************";
         KEYIN     *P1:24,*EOFF,ANS;
         CMATCH    "I" TO ANS
         GOTO      MASTER IF EQUAL
         GOTO      AGAIN
. ABORT - PROGRAM ESCAPE. ie - ABORT PRINTING SELECTION.
ABORT
         NORETURN
         TRAPCLR   F1
         TRAP      ABORT IF F1
         GOTO      MASTER
         include   comlogic.inc
;Patch3.1
			include	compio.inc
			include	cntio.inc
.        include   nmlrio.inc
;Patch3.1
         include   ndatio.inc
         include   nownio.inc
.
* * * * * * * * * * * * *  END OF THE PROGRAM LISTING  * * * * * * * * * * * *

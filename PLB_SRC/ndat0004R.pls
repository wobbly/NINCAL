 .........................................................................
.
.  NAMES IN THE NEWS & COMPUNAME, INC. DATA CARD RETRIEVAL PROGRAM
.......................................................................
.
. DATAPIC  RETRIEVES DATA CARD RECORDS FROM THE 'DATMST' FILES BASED
.          ON USER SELECTION CRITERIA.
.......................................................................
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
           INCLUDE   NORDDD.INC
           INCLUDE   NOWNDD.INC
.START PATCH 7.19 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
.         include   nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 7.19 REPLACED LOGIC
           INCLUDE   NDATDD.inc
           INCLUDE   NOFRDD.INC
           INCLUDE   NCATDD.INC
.START PATCH 7.17 ADDED LOGIC
          INCLUDE   NREFDD.INC
.END PATCH 7.17 ADDED LOGIC
         include   nusedd.inc
         include   hp.inc
         inc       logdata.inc
         include   ncntdd.inc
release   init    "7.42"            DLH cleanup menu & old code
reldate   Init      "2013 May 16"
.release   init    "7.41"            DLH Run Butil.exe locally
.reldate   Init      "2013 May 7"
.release   init    "7.4"            DLH add free float search of list name
.reldate   Init      "13 July 2012"
.release   init    "7.36"            DLH Change srv2008a\fax to nins2\fax
.reldate   Init      "11 January 2012"
.release   init    "7.35"            DLH adjust for 64 bit windows
.reldate   Init      "29 August 2011"
.release   init    "7.34"            DLH new fax server
.reldate   Init      " May 2010"
.release   init    "7.33"            DLH pick by SB
.reldate   Init      "20 July 2009"
.release   init    "7.32"            14Aug08 JD added more UNC
.release  init    "7.31"            13Nov2007 DLH Change all PRIN= to PA=
.release  init    "7.3"            25Jul2007 DLH PLI
.release init    "7.22"            30Aug2005 Nrh123 update for Ndat0005
.release init    "7.21"            07SEP2004 Logo Conversion
.release init    "7.20"            11AUG2004 Comment out code to update mailer/broker fax number
.release init    "7.19"            27MAY2004      ASH       MAILER CONVERSION
.release init    "7.18"            13APR2004      DMB       Added Option for rh with revision date.
.release init    "7.17"            26JAN2004  ASH  DATACARD CONVERSION
.release init    "7.16"            12NOV2003 DMB Added PDF & Print Options to RH
.release init    "7.15"            07JUL2003 DMB Added PDF & Print Options
.release init    "7.14"           12JUl2002 DLH Use GetWinVer
.release init    "7.13"           17Jan2002 DLH Suppress Banner if PDF  .tech center case 46
.release init    "7.12"           21Sep2001 JD packed revpick with dash to keep dates unique.
.release init    "7.1"           05May2001 DLH tries to Alphabetize
.release init    "7.02"           29OCT2000 ASH NEW SERVER ADDED
.release init    "7.01"           12Jul2000 JD quick fix branch move N to withdraw.
.release init    "7.0"           21Jun2000 DLH quick and dirty if from dsinit and comment = EXCL
.release init    "6.7"           04APR2000 ASH REPLACED PDF LOGIC
.release  init     "6.6"           16MAR2000 ASH REPLACED AREA CODE
.release  init     "6.5"           16jan00 DLH use new letter head subroutine
.release  init     "6.4a"           06jan00 jd clocking date for fax.
.release  init     "6.4"           28Spe99 DLH added PDF option
.release  init     "6.33"           09Jun99 DLH added NT copy (cmd.exe)
.release  init     "6.32"           27may99 JD added exclusive to prepare.
.release  init     "6.31"           18may99 JD send to print queue A.
.release  init     "6.3"           05Jan99 DLH Banner option
.release init      "6.2"           18Dec98 DLH cleanup testtele logic
.release  init       "6.1"           23Sep98  ASH NINMLR Y2K File expansion
.release  init       "6.0"           .03Sep98 DLH addes Sender to fax cover sheet embedded
.                                   codes so new facsys can ID sender
.release  init      "5.9"           20Jul98 ASH Y2K Changes due to increase in NINBRK.DAT fields:  BRCOMP,BRREVDAT,BRCNTCT
.release  init      "5.8"           05May98 DLH update fax number update
.release   init     "5.7"           6Nov97 DLH add trap codes at file prep
.release   init     "5.61"          10Apr97 DH tweak testtele
.release   init     "5.6"          21mar97 DH add more checking at testtele
.release  init      "5.5"          7Jan97 DH new record format.
.release  init      "5.4"          17oct96 DH check for failed executes.
.release  init      "5.3"          13SEP96 JD FIXED DELFILE SUB.
.release  init      "5.2"         11jul96 DLH run faxcards local, use port
.                                number as beginning Dataxx number.
.release  init      "5.1"         12mar96 new option to print/copy print file.
.release  init      "5.0"        04mar96 DLH run all the job locally facsys prob
.                               wrong user picked up if nprinted from batch
.release  init      "4.8"        20oct95 DLH fixed bug when user info not passed
.                               correctly & was not recalced correctly.         
.release  init      "4.7"         DLH  09Aug95 change checking for blank lines
.                                in faxcard section old code was throwing away
.                                lines of text after blank lines.
.Release  init      "4.6.2"       jd   07aug95 default yes to fax #.
.                                     can enter mailer/broker # for fax.opt
.Release  init      "4.6.1"      jd   06jun95 added check for blank filename.
.Release  init      "4.6"       DLH  17mar95 fixed f5 trap
.release  init      "4.5.1"     DLH  14mar95 display fax tele.
.release  init      "4.5"       DLH   make more friendly.
.release  init      "4.4"       DLH 07sep94   Add ability to keyin fax number & 
.                              update appropiate brk/mlr record, added list help.
.RELEASE  INIT      "4.3"       DLH 08JUL94   CHANGE OUTPUT FILE TO INDEX BY LIST
.                              NUMBER TO AVOID DUPES AND ALLOW removal of miskeyins at the end
.                              of pickoff by list number.
.RELEASE  INIT      "4.21"      DLH 06JUL94   CHANGED DEFAULT TO LASER CARD.
.RELEASE  INIT      "4.2"       DLH 27APR94   FIXED BUG IN CATEGORY PICK DISPLAY.
.
.RELEASE  INIT      "4.1"      DLH 14JUL93   DELFILE BUG FIXED.
.
.RELEASE  INIT      "4.0"      DLH 27MAY93   AFTER PICKOFF SUBMIT JOB STRAIGHT
.                                   BATCH SERVER FOR PRINT.
.RELEASE   INIT      "3.5"      DLH 27JAN93 ADD DEFAULT "Y" TO OK?
.                                   FIXED AIM KEY INIT'S.
.RELEASE   INIT        "3.4"      DLH 25SEP92 CHANGE ABORT TRAP TO VERIFY USER
.                                  WANTS TO EXIT AND DELETE FILE.
.RELEASE INIT          "3.3"      DLH 22SEP92   SUPPRESS RMS SORT FOR NON-DP USERS
.
.RELEASE  INIT     "3.2"      DLH 03SEP92    CONVERSION TO PC.
.
.RELEASE  INIT         "3.1"      DLH 02/03/92   NEW INCLUDES. USE CVTJUL.
.
.RELEASE  INIT      "R003"    E.W. LAKE    09/24/85
.                            - OUTPUT FILE INCREMENT STARTS WITH PORT#.
.                            - OPEN REF,OWN,ORD,MLR,OFR FILES WHEN NEEDED.
.                            - CONSOLIDATE DATE CHECKING.
.                            - USE SCAN INSTEAD OF BUMP & MOVE.
.                            - READ & WRITE COMPLETE RECORD (2811 RL).
.                            - USE READKS ON LIST# RANGE PICK-OFF.
.                            - ABILITY TO OMIT CARDS BY CATEGORY.
.                            - ABILITY TO OMIT CARDS USED BY A MLR/OFFER
.                            - ABILITY TO SELECT ON CREATION DATE D.L. HERRICK
.......................................................................
.
           IFNZ         PC
NEWMST   IFILE      keylen=6,VAR=3002,COMP       output file (DATA??)
           XIF
           IFZ        PC
NEWMST   IFILE     keylen=6,var=3002,comp,dup  
FORMFILE FILE      UNCOMP
           XIF
. ............................................................................
ROLLFILE INIT      "DATAPIC/ROLL:W"
CHNFLE   FILE
.......................................................................
.
.FILENAME DIM       12        file name being openned
NEWNAME  DIM       35        file name of output file
isiname  dim       35        "     "   "   "     isi file
SAVENAME DIM       35
recname  dim       55
A        INIT      "A"
R        INIT      "R"
CO       DIM       1         company N/C
PICK     DIM       1         keyed in pick-off choice
.begin patch 7.4
.NUMPICK  FORM      1         numeric version of above
NUMPICK  FORM      2         numeric version of above
searchstr dim       25        free form search
.end patch 7.4
WITHDRAW FORM      1         withdrawn branch flag, 0=don't include them
SORT     FORM      1         SORT BRANCH FLAG 0=DEFAULT ALPHA CARD STYLE,
.                            1=NO SORT, 2=RH style only, 3=RH & Card style.
.                                  4=BLANK STOCK SORTED, 5=BLANK NO SORT, 9-NO PRINT
ID       DIM       1
PORTNUM  DIM       3
CARD     INIT      ",DATA"
CHAIN    INIT      "CHAIN "
UTIL     INIT      "UTIL;"
IN       INIT      ",IN="
KILL     INIT      ",KILL=Y"
RH       INIT      ",DATARH"
PDF      init      ",PDF=Y"
.COMMA    INIT      ","
copy     dim       3        number of copies
H1       FORM      2         horizontal screen coordinate
H2       FORM      2                     "
H3       FORM      2                     "
H4       FORM      2                     "
H5       FORM      2                     "
FRLSNO   DIM       6         keyed in low list number range
TOLSNO   DIM       6         keyed in high list number range
CHECK1   FORM      6         numeric version of list number low range
CHECK2   FORM      6         numeric version of list number high number
FOUND    FORM      "    0"   records that matched criteria
TOTAL    FORM      6         total records read for a pick-off
ONAME    DIM       25        owner name from the owner file
OCOMP    DIM       25        owner company from the owner file
LISTOWN  DIM       70        chosen list owners seperated by a blank
LISTSB   DIM       90        chosen Service B's seperated by a blank
LISTBRCH FORM      1         used for coordinate and string changes
LIST     DIM       96        current list range string being worked on
LIST1ST  DIM       96        list number ranges
LIST2ND  DIM       96               " "
LIST3RD  DIM       96               " "
LIST4TH  DIM       96               " "
ANDOR    DIM       1         keyed in A or O for category pick-off
ANDORTXT DIM       3         for category pick-off contains 'and'/'or'
CATPICK  DIM       30        chosen categories
CATOMIT  DIM       15        chosen category omissions
OMITBRCH FORM      1         if category omits were requested,0=yes,1=no
CATMASK  INIT      "XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX"
.
CATS     DIM       39        above mask breaks up 'category' field for scan
DESC20   DIM       20        category description retrieved from catmst
.EXCLCODE DIM       1         exclusive type requested N/C/B
.begin patch 7.3
EXCLCODE DIM       1         exclusive type requested P/C/B
.end patch 7.3

REVPICK  DIM       112       chosen dates
REVPICK2 DIM       112       chosen dates only when mm/dd/yy is used
REVCOUNT FORM      2         screen line counter
YEAR     DIM       2         keyed in year and year from datmst
MO       DIM       2         keyed in month and month from datmst
MOYR     DIM       4         month & year from datmst
DAY      DIM       2         keyed in day and day from datmst
DATE     DIM       8         mm/dd/yy appended to chosen string
REVBRCH  FORM      1         used to determine if/how dates should
.                             be checked. 0=no,1=yy,2=mmyy,3=mmddyy
MLRBRCH  FORM      1         check mlr/offer omits,0=no,1=no,2=mlr,3=ofr
MLR      DIM       4         keyed in mailer number
MLROMIT  DIM       35        chosen mlr/offer omissions seperated by ' '
.                            maximum of seven mailers.
MLRPAGE  FORM      1         used during mlr omit, if more than 5 mlrs.
YR       DIM       2
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
ORDJUL   FORM      5         julian order date from ordmst
.
.
DATA     INIT      "DATA"
.T        INIT      "T"
.SLASH    INIT      "/"
.ZIP3     INIT      "000"
.ZERO     FORM      "0"
.ONE      FORM      "1"
.N1       FORM      1
time      init      "HH:MM:SS"
.
           IFNZ        PC
.DR       INIT      ":PRINT"         FORCE OUTPUT FILES TO BE BUILD
TXT      INIT      "/TEXT"
           XIF
           IFZ         PC
.START PATCH 7.02 REPLACED LOGIC
.DR       INIT      "g:\data\"         FORCE OUTPUT FILES TO BE BUILD
DR      DIM     45
.        MOVE    NTWKPATH,DR            FORCE OUTPUT FILES TO BE BUILD
.END PATCH 7.02 REPLACED LOGIC
TXT      INIT      ".DAT"
           XIF
DELNAME  DIM       20
.begin patch 7.42
.SHORTNME DIM       6
SHORTNME DIM       8
.end patch 7.42
OKEY1    INIT      "01R"
OKEY2    INIT      "02R"
WITHCODE DIM       1
NEWMFLAG FORM      1               0=OUTPUT FILE CLOSED 1=OUTPUT FILE OPEN
.Patch7.18
repflag  form      2               report type
.Patch7.18
rhflag   form      1               1=no rh 2=yes rh as secondary rep.
.begin patch 6.4
PDFFLag  form      1               0,1 = default, 2= produce a pdf file
.end patch 6.4
repstyle dim       25
.rep01    init      "Cardstock"
.rep02    init      "RH Style listing"
rep01    init      "RH Style listing"
rep02    init      "Blankstock"
Rep03     Init      "No Print"
.rep03    init      "Blankstock"
.rep04    init      "Fax Cards"
rep04    init      "Excel"
.rep05    init      "Excel"
rep05     init      "RH with Rev Date"
Rep06     Init      "  "
Rep07     Init      "  "
Rep08     Init      "  "
Rep09     Init      "  "
Rep10     Init      "  "
.rep06    init      "View Cards"
.rep07    init      "Attach Fax Cards"
.rep08    init      "No Print"
.rep09    init      "RH Flat File"
.rep10    init      "RH with Rev Date"
faxtele  dim       10
.Var increased to reflect change to NINBRK.DAT
.faxname  dim       25
faxname  dim       45
LONGDIST DIM       1
subject  dim       50
fline1   dim       50
fline2   dim       50
fline3   dim       50
fline4   dim       50
fline5   dim       50
fline6   dim       50
fline7   dim       50
fline8   dim       50
fline9   dim       50
fline10  dim       50
fline11  dim       50
fline12  dim       50
fline13  dim       50
fline14  dim       50
fline15  dim       50
fline16  dim       50
fline17  dim       50
fline18  dim       50
fline19  dim       50
fline20  dim       50
fline99  dim       50
blincnt form       1              blank line counter allow several in a row.
.PHONE VAR'S
telflag  form      1                  access number by 1=brk 2= mlr
ARCD     DIM       3                   AREA CODE
EXCH     DIM       3                   EXCHANGE
TELE     DIM       4                   TELEPHONE#
.for list help
wsw      dim       1              withdrawn flag
keycount form      2              number of keyed in characters for search
akey1    dim       3
ques     init      "??????"
formflag form      1              2=fax form file has been created
FILL4    DIM       4
scrn1    dim       15000         dlh- sunbelt 05mar96
banner   form      1
.begin patch 7.0
autoflag form      1             
.end patch 7.0
.START PATCH 7.17 ADDED LOGIC
.str75     dim       75
.END PATCH 7.17 ADDED LOGIC
.START PATCH 7.21 ADDED LOGIC
prfile    pfile
font1     font
font2     font
NINLogo   PICT
          move      "250",column
          move      "750",column1
          move      "2000",column2
          move      "2250",column3
          create    font1,"Helvetica",size=12,bold
          create    font2,"Arial",size=12 
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          call      GetWinVer
.END PATCH 7.21 ADDED LOGIC

                    call Get64OS
.
+
.START    KEYIN     *ES,*P11:1,"*** D A T A   C A R D   R E T R I E V A L        ":
.                   "P R O G R A M ***":
.begin patch 7.0
START      if       (program = "NDAT0004R")
           move     c1 to autoflag
           else
           MOVE       "NDAT0004" TO PROGRAM
           endif
.end patch 7.0
          if        (Company = c2)
           MOVE         "PLI" TO COMPNME
          else
.           MOVE         "NINCAL" TO COMPNME
           MOVE         "NIN" TO COMPNME
          Endif
           MOVE       "DATACARD RETRIEVAL" TO STITLE
           CALL         PAINT
           MOVE       "ABORT" TO PF5
           CALL         FUNCDISP
         TRAP      DELFILE IF F5
         TRAP      DELFILE IF INT
           MOVE       C1 TO NDATPATH
         move       c0 to newmflag
         move       c1 to formflag
         move      c1 to nusepath
         move      c1 to pdfflag
         move      c0 to nusefld
.begin patch 7.0
         if        (autoflag = c1)
         move      "099" to portn
         endif
         MOVE      PORTN TO NUSEFLD
         REP       ZFILL IN NUSEFLD
         CALL      NUSEKEY
         goto      userng if over
         scan      "INVALID" in nuseuser
         goto      userng if equal
         reset     nuseuser         
         move      c3 to ncntpath
.         call      findport
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
         if        over
         move      c2 to cntprint
         endif
.
         MOVE       NO TO CO
.begin patch 7.0
         if         (autoflag < c1)
           KEYIN      *P15:4,"(",*cyan,"N",*white,")ames In The News  or  ":
                    "(F)inished? ",*RV,*uc,CO,*lc,*it;
         endif
.end patch 7.0
         CMATCH    "F",CO
         GOTO      START IF EOS
         STOP      IF EQUAL
.         CMATCH    "C",CO
.         GOTO      COPEN IF EQUAL
         CMATCH    NO,CO
         GOTO      START IF NOT EQUAL
         MOVE      "NINDAT" TO FILENAME
         GOTO      OPENDAT
COPEN    MOVE      "CMPDAT/ISI" TO FILENAME
OPENDAT  TRAP      NOFILE IF IO
.         OPEN      DATMST,FILENAME,SHARE
         TRAPCLR   IO
.
CHOICE   CALL       PAINT
.begin patch 7.0
         if        (Autoflag < c1)
         MOVE       "L" TO PICK
         KEYIN     *P15:6,"How would you like to choose the lists you want?:":
                   *P27:9,"by (",*cyan,"L",*white,")ist Number?":
                   *P30:10,"(O)wner Number?":
                   *P30:11,"(C)ategory Code?":
                   *P30:12,"(E)xclusive Code?":
                   *P30:13,"(N)ew List Code?":
                   *P30:14,"(R)evise Date?":
                   *P30:15,"(W)ithdrawn Code?":
                   *P30:16,"(P)ut-Up Date?":
                   *P30:17,"(S)ervice Bureau?":
                   *P30:18,"(T)ext (list name) Search?":
                   *P30:19,"(F)inished?":
                   *P19:20,"Choose one: ",*DV,PICK,*P31:20,*RV,*uc,PICK,*lc;
         else
         move      "E" to pick
         endif
.end patch 7.0
         CMATCH    "F",PICK
         GOTO      CHOICE IF EOS
         STOP      IF EQUAL
.
         MOVE      PICK,STR1
.         REPLACE   "L1O2C3E4N5R6W7P8",STR1
.begin patch 7.4
          if        (str1 = "T")
          move      c10,numpick
          else
         REPLACE   "L1O2C3E4N5R6W7P8S9",STR1
         MOVE      STR1,NUMPICK
          endif
.end patch 7.4
         COMPARE   C0 TO NUMPICK
         GOTO      CHOICE IF EQUAL
.         COMPARE   "9" TO NUMPICK
.         GOTO      CHOICE IF NOT LESS
         MOVE      NO TO STR1
.begin patch 7.0
         if        (AUtoflag < C1)
         move      "N" to str1
         KEYIN     *P19:21,*EL,"Do you want WITHDRAWN's included? ":
                         *dV,*cyan,STR1,*white,*p54:21,*rv,*uc,str1,*lc;
         endif
.         move      "N" to str1
.end patch 7.0
         REPLACE   "Y1N0" IN STR1
         MOVE      STR1 TO WITHDRAW
          if        (numpick = c10)
repopt10
         KEYIN     *P15:8,*Ef,"Search For: ":
                   *P15:09,*uc,Searchstr;
          call      trim using searchstr
          count     n2,searchstr
          if        (n2 < 3)
          alert   caution,"You must enter at least 3 contiguous characters!",result
          goto      repopt10
          endif
          KEYIN     *P19:11,"Exclusives only ?":
                    *uc,str1;
          if        (str1 = "Y" or str1 = "y")                    
          KEYIN     *P19:12,"(C)alifornia   or   (P)acific Lists  or   (B)oth? ":
                    EXCLCODE;
          endif

.         branch    rhflag of sortopt,sortopt
.         move      c1 to rhflag
          endif


repopt   move      c5 to str2
         display   *p25:6,*ef
         call      funcdisp
.begin patch 7.0
         if        (AUtoflag < C1)
         KEYIN     *P15:6,"Do you want: ":
.begin patch 7.42
                   *p15:9,"(",*DION,"1",*dioff,*HOFF,*BLINKOFF:
.                   ")","Data Card(s) {InHouse} ":
                   ")","RH Style ":
                   *p15:10,"(",*DION,"2",*HOFF,")Blankstock":
..                       *P15:11,"(",*DION,"3",*HOFF,")Blankstock ":
..                       *p15:12,"(",*DION,"4",*HOFF,")Fax Cardstock ":
.                    *p15:11,"(",*DION,*cyan,*blinkon,"3",*HOFF:
.                    *blinkoff,*white,")Data Card(s) {Default}":
.                       *p15:14,"(",*DION,"6",*HOFF,")View Cards (Carol) ":
.                       *p15:15,"(",*DION,"7",*HOFF,")Attach Cardstock Fax ":
                   *P15:11,"(3) No print ":
.                   *p15:17,"(9)RH Flat File",*rv,*uc,STR1,*lc;
                   *p15:12,"(4)Excel File":
.patch7.18
                   *p15:13,"(5)RH w/ Rev Date",*rv,*uc,STR2,*lc;
.end patch 7.42
.patch7.18
         else
.patch7.18
.begin patch 7.42
         move      c3 to str2
.end patch 7.42
.patch7.18
.         move      c8 to str1
         endif
.end patch 7.0
         move      c0 to repflag
.patch7.18
         move      str2 to repflag
.         move      str1 to repflag
.patch7.8
.begin patch 7.4
.         branch     repflag of repopt1,sortopt,repopt1,repopt1,repopt1,sortopt,repopt1,sortopt,sortopt,sortopt
.begin patch 7.42
.         branch     repflag of repopt1,sortopt,repopt1,repopt1,repopt1,sortopt,repopt1,sortopt,sortopt,sortopt,sortopt
         branch     repflag of repopt1,sortopt,repopt1,sortopt,sortopt,sortopt
.end patch 7.42
.end patch 7.4
         display    *p1:24,*b,*el,*red,"Invalid choice, Please try again",*w3,*white
         goto       repopt
repopt1  move      no to str1              .default no rh as secondary
.         load      repstyle from repflag of rep01,rep02,rep03,rep04,rep05,rep06,rep07,rep08,rep09,REp10
.begin patch xxx
.         load      repstyle from repflag of rep01,rep02,rep03,rep08,rep09,REp10
         load      repstyle from repflag of rep01,rep02,rep03,rep04,rep05,REp06
.end patch xxx
         display   *p1:5,*ef
         call      funcdisp
.begin patch 7.0
         if        (Autoflag < c1)
         keyin     *p15:5,"You selected ",*dv,repstyle:
                   *p15:9,"RH style in addition ?(Y/",*cyan,"N",*white,")",*UC:
                   str1,*lc
         else
         move      "N" to str1
         endif
.begin patch 7.0
         rep      "Y2N1" in str1
         move     c0 to rhflag
         move      str1 to rhflag
.begin patch 6.4
         display   *p15:5,*el,"You selected ",repstyle
         if        (rhflag = 2)
         display  *p15:9,"With RH style in addition "
         else
         display  *p15:9,"Without RH style in addition "
         endif
         move     no to str1
.begin patch 7.0
.         if       (autoflag < c1)
.patch7.15
.         keyin    *p15:10,"Produce PDF file Y/",*cyan,"N ",*cyan,"(B)",*white,"oth  ",*uc,str1,*lc
.patch7.15
.         endif
.end patch 7.0
.
.patch7.15
.         rep      "B3Y2N1" in str1
.         rep      "Y2N1" in str1
.patch7.15
.         move     c1 to PDFflag
.         move      str1 to PDFflag
.end patch 6.4

SORTOPT  move      c1 to str1
         display   *p15:5,*ef,"You selected ",repstyle
         compare   c2 to rhflag
         if        equal
         display   *p15:6,"& an RH "
         endif
.begin patch 7.16
         if       (autoflag < c1)
         keyin    *p15:10,"Produce PDF file Y/",*cyan,"N ",*cyan,"(B)",*white,"oth  ",*uc,str1,*lc
         endif
         rep      "B3Y2N1" in str1
         move     c1 to PDFflag
         move      str1 to PDFflag
         move     c1 to str1
.patch7.16
         call      funcdisp
.begin patch 7.0
         if        (autoflag < c1)
         KEYIN     *P15:8,*Ef,"How Do you want Sorted: ":
                   *p15:9,"(",*DION,*cyan,*blinkon,"1",*HOFF,*BLINKOFF:
                       *white,")Alpha ":
                   *p15:10,"(",*DION,"2",*HOFF,")No Sort ":
                       *p15:11,"(",*DION,"3",*HOFF,")By LO## & list name":
                       *p15:12,"(",*DION,"4",*HOFF,")By list number":
                   *P15:13,*rv,STR1;
        endif
.begin patch 7.0
         MOVE      c0 TO SORT
         MOVE      STR1 TO SORT
          goto      copies
..................
COPIES   MOVE     C1 TO COPY
.begin patch 7.0
         IF       (AUTOFLAG < C1)
         KEYIN    *P15:22,*EL,*uc,*in,"Number of copies : ",*cyan,*DV,COPY,*white:
                 *P34:22,*RV,*de,COPY,*lc
         ENDIF
.enD patch 7.0
         move     c0 to n3
         move     copy to n3
         move     "26" to n4
         compare  n4 to n3
         if       not less
         display  *p1:24,*b,*b,*blinkon,"You can not print more that 25 copies!!":
                  *b,*b,*w20,*blinkoff
         goto     copies
         endif          
Banner   move     c2 to Banner
         move     Yes to str1
.begin patch 7.0
         if       (autoflag < c1)
                  IF        (pdfFlag = c1)
         KEYIN    *P15:22,*EL,*uc,*in,"Banner Page ",*cyan,"Y",*white,"/","N ",*white,*DV,str1:
                 *P34:22,*RV,STR1,*lc
                 else
                  move      c1 to banner
                  move      no to str1
         display *P15:22,*EL,*uc,*in,"NO Banner Page with PDF",*w1
                 endif
         endif
.end patch 7.0
         cmatch   no to str1
         if       equal
         move     c1 to Banner 
         endif

.  CREATE OUTPUT FILE NAME, APPENDS "01" TO 'DATA', ADDS 1
.    UNTIL IT FINDS A NON-EXISTENT FILE NAME.
.
VALIDNUM 
         move       c0 to newmflag
         move       c1 to formflag
.         TRAP      GOODFILE GIVING ERROR IF IO
         TRAP      GOODA GIVING ERROR IF IO
         MOVE      PORTN TO N3            .11jul96 use port number (again)
         move      c0 to n2
         add       n3 to n2
.         MOVE     C1 TO N2
NEXTFILE MOVE      N2   TO STR2
         REPLACE   ZFILL IN STR2
         TRAP      GOODA GIVING ERROR IF IO
.         PACK      NEWNAME WITH DR,DATA,STR2,TXT
.BEGIN PATCH 7.0
         IF        (AUTOFLAG < C1)
         PACK      NEWNAME WITH ntwkpath1,DATA,STR2
         PACK      SHORTNME FROM DATA,STR2
         ELSE  
.START PATCH 7.02 REPLACED LOGIC
.         MOVE      "G:\DATA\DATAEXCL.DAT" TO NEWNAME
        pack    NEWNAME,NTWKPATH1,"DATAEXCL.DAT"
.END PATCH 7.02 REPLACED LOGIC
         MOVE      "DATAEXCL" TO SHORTNME
         ENDIF
.END PATCH 7.0
         MOVE      NEWNAME TO SAVENAME
         display   *P31:24,*el,"Trying to create file ",NEWNAME;
         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST
ADDFILE  ADD      C1 TO N2
         GOTO      NEXTFILE
GOODA    TRAPCLR   IO
.         display   *P31:24,*el,"I'm at gooda",error,*b,*w4
         NORETURN
         SCAN      "0030-0031" IN ERROR        .datapoint rms
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR            .pcbus oops somebody elses file.
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I10" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I25" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           TRAP      GOODR GIVING ERROR IF IO
           IFNZ        PC
         PACK      NEWNAME WITH DATA,STR2,A,TXT,DR
           XIF
           IFZ         PC
.         PACK      NEWNAME WITH DR,DATA,STR2,A,TXT
         PACK      NEWNAME WITH DR,DATA,STR2,A
           XIF
.         display   *P31:24,*el,"Trying to create file ",NEWNAME,*w5;
         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST
         GOTO      ADDFILE
GOODR    TRAPCLR   IO
.         display   *P31:24,*el,"I'm at goodr",*b,*w
         NORETURN
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
.          SCAN      "I04" IN ERROR           .plb
.          GOTO      ADDFILE IF EQUAL
.          RESET     ERROR
.          SCAN      "I25" IN ERROR           .plb
.          GOTO      ADDFILE IF EQUAL
.          RESET     ERROR
         TRAP      GOODFILE GIVING ERROR IF IO
           IFNZ        PC
         PACK      NEWNAME WITH DATA,STR2,R,TXT,DR
           XIF
           IFZ         PC
.         PACK      NEWNAME WITH DR,DATA,STR2,r,TXT
.BEGIN PATCH 7.0
         IF        (AUTOFLAG < C1)
         PACK      NEWNAME WITH DR,DATA,STR2
         PACK      SHORTNME FROM DATA,STR2
         ELSE  
.START PATCH 7.02 REPLACED LOGIC
.         MOVE      "G:\DATA\DATAEXCL.DAT" TO NEWNAME
        PACK    NEWNAME,NTWKPATH1,"DATAEXCL.DAT"
.END PATCH 7.02 REPLACED LOGIC
         MOVE      "DATAEXCL" TO SHORTNME
         ENDIF
.END PATCH 7.0
           XIF
         display   *P31:24,*el,"Trying to create file ",NEWNAME;
         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST
         GOTO      ADDFILE
GOODFILE TRAPCLR   IO
.         display   *P31:24,*el,"I'm at goodfile",*b,*w
         NORETURN
         TRAP      GOODFILE GIVING ERROR IF IO
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
         MOVE      SAVENAME TO NEWNAME
         MOVE      B1 TO ERROR
.begin patch 7.0
         if        (autoflag < c1)
         PREPARE   NEWMST,NEWNAME,newname,"6","3002",EXCLUSIVE
         else
         move      "EXCL" to STR4
.         PACK      NEWNAME WITH DR,DATA,STR4
        pack    NEWNAME,NTWKPATH1,"DATAEXCL"
         PREPARE   NEWMST,NEWNAME,newname,"6","3002",EXCLUSIVE
         endif
.end patch 7.0
.         PREPARE   NEWMST,NEWNAME,CREATE
           MOVE      C1 TO NEWMFLAG              *FILE OPEN
         move      "ABORT" TO PF5
           CALL      FUNCDISP
         TRAP      DELFILE IF F5
         TRAP      DELFILE IF INT
.begin patch  xxx
.         compare   c4 to repflag              .fax option?
.         if        equal
.         display   *P31:24,*el,"I'm creating fax cover",*b,*w
.         CLEAR     RECNAME
..START PATCH 7.02 REPLACED LOGIC
..           APPEND    "g:\data\",RECNAME
.           APPEND    NTWKPATH1,RECNAME
..END PATCH 7.02 REPLACED LOGIC
.           APPEND    data,RECNAME
.         append    str2,recname
.         append    ".cvr",recname
.         RESET     RECNAME
.         MOVE      B1 TO ERROR
.         PREPARE   formfile,RECNAME
..         PREPARE   formfile,RECNAME,CREATE
.         move      c2 to formflag
.         close     formfile
.         endif
.end patch  xxx
.
. BRANCH TO APPROPRIATE STARTING POINT
.
.begin patch 7.4
         BRANCH    NUMPICK OF LISTPICK,OWNPICK,CATPICK,EXCLPICK,NEWPICK:
                              DATEPICK,WITHPICK,DATEPICK,SBPICK,srchpick
.                              DATEPICK,WITHPICK,DATEPICK,SBPICK
.end patch 7.4
.                              DATEPICK,WITHPICK,DATEPICK
*
.  LIST NUMBER RETRIEVAL SECTION
.    EITHER ONE AT A TIME (CAN ENTER AN UNLIMITED AMOUNT OF NUMBERS)
.        OR IN A RANGE (CAN ENTER UP TO 32 RANGES)
.
LISTPICK MOVE       "O" TO STR1
         KEYIN     *P1:4,*EF,*P17:4,"***L I S T   N U M B E R   P I C K - ":
                   "O F F***":
                   *P4:5,"How would you like to choose the lists:  (":
                       *DION,*cyan,"O",*dioff,*white,*HOFF,")ne at a":
                   " time or in a (R)ange? ",*RV,*uc,STR1,*lc;
         CMATCH    "O",STR1
         GOTO      LISTONE IF EQUAL
         GOTO      LISTPICK IF EOS
         CMATCH    "R",STR1
         GOTO      LISTPICK IF NOT EQUAL
.
. CHOOSE LIST NUMBERS BY RANGE...
.
         DISPLAY   *P1:23,"Please enter the list numbers you would like":
                   " to retrieve (enter '*' if finished)":
                   *P41:24,"Writing to ",NEWNAME,*P1:5,*EL;
.                   *P51:24,"Writing to ",NEWNAME,*P1:5,*EL;
         MOVE      "05",V
         MOVE      C0 TO LISTBRCH
NEXTLST  ADD      C1 TO LISTBRCH
         BRANCH    LISTBRCH TO LIST1,LIST2,LIST3,LIST4,ENDLIST
LIST1    CLEAR     LIST1ST
         MOVE      "01" TO H1
         MOVE      "21" TO H2
         MOVE      "32" TO H3
         MOVE      "33" TO H4
         GOTO      LISTRNG
LIST2    CLEAR     LIST2ND
         GOTO      LISTRNG
LIST3    CLEAR     LIST3RD
         MOVE      "44" TO H1
         MOVE      "64" TO H2
         MOVE      "75" TO H3
         MOVE      "76" TO H4
         GOTO      LISTRNG
LIST4    CLEAR     LIST4TH
         GOTO      LISTRNG
.
LISTRNG  KEYIN     *PH1:V,"From list#:",*JR,*ZF,FRLSNO;
         MATCH     "00000*",FRLSNO
         GOTO      ENDLIST IF EQUAL
         GOTO      LISTRNG IF EOS
         KEYIN     *PH2:V,"to:",*JR,*ZF,TOLSNO;
         MOVE      FRLSNO,CHECK1
         MOVE      TOLSNO,CHECK2
         COMPARE   CHECK1,CHECK2
         GOTO      RNGOK IF NOT LESS
         DISPLAY   *PH3:V,"Invalid.",*W2,*PH3:V,"        ";
         GOTO      LISTRNG
RNGOK      MOVE        YES TO STR1
           KEYIN     *PH4:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      LISTRNG IF EQUAL
         GOTO      RNGOK IF EOS
         CMATCH    YES,STR1
         GOTO      RNGOK IF NOT EQUAL
.
         ADD      C1 TO V
         BRANCH    LISTBRCH TO APP1,APP2,APP3,APP4
APP1     APPEND    FRLSNO,LIST1ST
         APPEND    TOLSNO,LIST1ST
         COMPARE   "13" TO V
         GOTO      LISTRNG IF NOT EQUAL
         GOTO      NEXTLST
APP2     APPEND    FRLSNO,LIST2ND
         APPEND    TOLSNO,LIST2ND
         COMPARE   "21" TO V
         GOTO      LISTRNG IF NOT EQUAL
         GOTO      NEXTLST
APP3     APPEND    FRLSNO,LIST3RD
         APPEND    TOLSNO,LIST3RD
         COMPARE   "13" TO V
         GOTO      LISTRNG IF NOT EQUAL
         GOTO      NEXTLST
APP4     APPEND    FRLSNO,LIST4TH
         APPEND    TOLSNO,LIST4TH
         COMPARE   "21" TO V
         GOTO      LISTRNG IF NOT EQUAL
         GOTO      NEXTLST
.
ENDLIST  DISPLAY   *P1:24,*EL,"The Data Card File is now being searched ":
                   "for the entered range(s) of list numbers...";
         RESET     LIST1ST
         RESET     LIST2ND
         RESET     LIST3RD
         RESET     LIST4TH
         MOVE      C0 TO LISTBRCH
         call      funcdisp
.
NXTLSET  ADD      C1 TO LISTBRCH
         BRANCH    LISTBRCH OF LST1,LST2,LST3,LST4,FINISH
LST1     MOVE      LIST1ST TO LIST
         GOTO      SRCHRNG
LST2     MOVE      LIST2ND TO LIST
         GOTO      SRCHRNG
LST3     MOVE      LIST3RD TO LIST
         GOTO      SRCHRNG
LST4     MOVE      LIST4TH TO LIST
.
. DO AN EXPLICIT READ ON THE LOW NUMBER,THEN READKS THRU THE HIGH NUMBER
.
SRCHRNG  MOVE      LIST TO STR12
         UNPACK    STR12 TO FRLSNO,TOLSNO        GET LOW & HIGH LIST#'S
         CMATCH    " " TO FRLSNO
         GOTO      FINISH IF EQUAL
         MOVE      FRLSNO TO CHECK1              MOVE TO NUMERIC
         MOVE      TOLSNO TO CHECK2
RNGREAD  MOVE      FRLSNO TO NDATFLD
           REP       ZFILL IN NDATFLD
         call      rotdial
           CALL      NDATKEY
         CALL      RNGWRITE IF NOT OVER
RNGNEXT  call      rotdial
         CALL          NDATKS
         GOTO      RNGBUMP IF OVER
         MOVE      LSTNUM TO CHECK1
         COMPARE   CHECK1 TO CHECK2              IS LAST READKS IN RNG?
         GOTO      RNGBUMP IF LESS               NO.
         CALL      RNGWRITE                      YES.
         GOTO      RNGNEXT
.
RNGBUMP  BUMP      LIST BY 12                    MORE IN STRING?
         GOTO      SRCHRNG IF NOT EOS            YES.
         GOTO      NXTLSET                       NO. GET NEXT STRING
.
RNGWRITE BRANCH    WITHDRAW TO RNGWRIT2          INCLUDE WITHDRAWNS?
         CMATCH    "W" TO STATUS                 NO.
         RETURN    IF EQUAL
         CMATCH    "T" TO STATUS                 NO.
         RETURN    IF EQUAL
           IFNZ        PC
RNGWRIT2 
.         WRITE     NEWMST,SEQ;DATVARS
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
         RETURN    IF NOT OVER              .DLH 08JUL94
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
         FLUSH     NEWMST
           XIF
           IFZ         PC
RNGWRIT2 
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
         RETURN    IF NOT OVER              .DLH 08JUL94
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
           XIF
         ADD      C1 TO FOUND
         RETURN
*
. CHOOSE LIST NUMBERS ONE AT A TIME...
.
LISTONE  DISPLAY   *P1:6,*EF:
                   *P1:23,"Please enter the list# you would like (enter":
                   " '*' when finished):":
                   *P41:24,"Writing to ",NEWNAME;
.                   *P51:24,"Writing to ",NEWNAME;
         MOVE      "6",V
LISTENT  KEYIN     *P1:V,*EL,"List## ",*ZF,*JR,LSTNUM;
         MATCH     "00000*",LSTNUM
.         GOTO      FiNISH IF EQUAL
         GOTO      cleanup IF EQUAL           .08jul94 DLH
         GOTO      LISTENT IF EOS
         match     "00000?" to lstnum
         if         equal
.         display    *scrnsave,1
         scrnsave    scrn1
         call       listhelp 
.         display    *scrnrst,1,*p1:v,"List## ",lstnum;
         scrnrest  scrn1
         display    *p1:v,"List## ",lstnum;
         MOVE       C1 TO NDATPATH
         cmatch     b1 to lstnum
         goto       listent if eos
         endif
.         FILEPI    1;DATMST
           MOVE      LSTNUM TO NDATFLD
         call      rotdial
           CALL      NDATKEY
         GOTO      LISTOK IF NOT OVER
         DISPLAY   *P14:V,"- List number not on file.",*W;
         GOTO      LISTENT
LISTOK   DISPLAY   *P14:V,"- ",MLSTNAME;
         BRANCH    WITHDRAW TO LISTOK1
         CMATCH    "W" TO STATUS
         GOTO      TW IF EQUAL
         CMATCH    "T" TO STATUS
         GOTO      TW IF EQUAL
         branch    repflag of offuse,listok1,listok1,offuse,offuse,listok1,offuse,listok1,listok1,listok1
.         branch    sort of listok1,listok1,offuse,listok1,listok1,offuse,offuse:
.                   offuse
offuse
         scan      "OFFICE USE" in mlstname
         goto       ou if equal
         reset      mlstname
         GOTO      LISTOK1
TW       DISPLAY   *P66:V,"Withdrawn...",*W2;
         GOTO      LISTENT
ou       DISPLAY   *P66:V,"Office Use...",*W2;
         GOTO      LISTENT
.
LISTOK1  MOVE      YES TO STR1
           KEYIN     *P73:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      LISTENT IF EQUAL
         GOTO      LISTOK IF EOS
         CMATCH    YES,STR1
         GOTO      LISTOK IF NOT EQUAL
           IFNZ        PC
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
         if        not over
         move      yes to str1
         keyin     *p1:24,*el,*b,"This is a duplicate, are you sure ? ",*t120,*rv,uc,str1,*lc:
                   *p1:24,*el
         cmatch    yes to str1
         goto      listent if not equal             .DLH 08JUL94
         endif
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1

         WRITE     NEWMST,lstnum;DATVARS
         FLUSH     NEWMST
           XIF
           IFZ         PC
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
         if        not over
         keyin     *p1:24,*el,*b,"This is a duplicate, are you sure ? ",*t120,*uc,str1,*lc:
                   *p1:24,*el
         cmatch    yes to str1
         goto      listent if not equal             .DLH 08JUL94
         endif
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
           XIF
         ADD      C1,FOUND
         ADD      C1,V
         COMPARE   "24",V
         GOTO      LISTENT IF NOT EQUAL
         GOTO      LISTONE
*
.  LIST OWNER SELECTION ROUTINE
.   CAN ENTER UP TO 14 LIST OWNER NUMBERS AT ONE TIME
.
OWNPICK  DISPLAY   *P1:2,*EF,*P17:2,"***L I S T   O W N E R   P I C K - O F F":
                   "***":
                   *P1:24,"Please enter the numbers you would like to":
                   " retrieve (enter '*' if finished)";
.
         MOVE      "5",V
         CLEAR     LISTOWN
OWN1     KEYIN     *P12:V,*EL,"List Owner##:",*ZF,*JR,NOWNFLD;
         MATCH     "000*",NOWNFLD
         GOTO      ENDOWN IF EQUAL
         GOTO      OWN1 IF EOS
           CALL      NOWNKEY
         GOTO      OWN1OK IF NOT OVER
         DISPLAY   *P30:V," -List Owner not on file.",*W2;
         GOTO      OWN1
OWN1OK   MOVE      YES TO STR1
           KEYIN     *P30:V,"-",*DV,ownocpy,*P67:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      OWN1 IF EQUAL
         GOTO      OWN1OK IF EOS
         CMATCH    YES,STR1
         GOTO      OWN1OK IF NOT EQUAL
         APPEND    NOWNFLD,LISTOWN
         APPEND    " " TO LISTOWN
         ADD      C1,V
         COMPARE   "20",V
         GOTO      OWN1 IF NOT EQUAL
ENDOWN   MOVEFPTR  LISTOWN TO N3
         COMPARE   C0 TO N3                    ANY OWNERS ENTERED?
         GOTO      CHOICE IF EQUAL               NO.
         GOTO      DATESEC                       YES. ASK FOR DATES.
.  SB SELECTION ROUTINE
.   CAN ENTER UP TO 14 SB NUMBERS AT ONE TIME
.
SBPICK  DISPLAY   *P1:2,*EF,*P17:2,"***L I S T   S e r v i c e B.  P I C K - O F F":
                   "***":
                   *P1:24,"Please enter the numbers you would like to":
                   " retrieve (enter '*' if finished)";
.
         MOVE      "5",V
SB1     KEYIN     *P12:V,*EL,"Service B##:",*ZF,*JR,COMPFLD;
         MATCH     "00000*",COMPFLD
         GOTO      ENDSB IF EQUAL
         GOTO      SB1 IF EOS
                    call                TRIM using Compfld
                    if (Compfld <> "")
                                        rep       zfill,COMPFLD
                                        move            C1,COMPPATH
                                        move                "D.Load-COMPKEY",Location
                                        pack                KeyLocation,COMPFLD
                                        call                COMPKEY
                                        if OVER
                                                  clear     COMPCOMP
                                        else
                                                  if (COMPSVBFLG <> "T")
                                                            clear COMPCOMP
                                                  endif
                                        Goto SB10OK          
                                        endif
                    else      // DATFUL = ""
                                        clear COMPCOMP
                    endif


         DISPLAY   *P30:V," -List SB not on file.",*W2;
         GOTO      Sb1
SB10OK   MOVE      YES TO STR1
           KEYIN     *P30:V,"-",*DV,compCOmp,*P67:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      Sb1 IF EQUAL
         GOTO      SB10OK IF EOS
         CMATCH    YES,STR1
         GOTO      SB10OK IF NOT EQUAL
         APPEND    CompFLD,LISTSB
         APPEND    " " TO LISTSB
         ADD      C1,V
         COMPARE   "20",V
         GOTO      SB1 IF NOT EQUAL
Endsb    MOVEFPTR  LISTSB TO N3
         COMPARE   C0 TO N3                    
         GOTO      CHOICE IF EQUAL               NO.
         GOTO      DATESEC                       YES. ASK FOR DATES.
*
.  CATEGORY SELECTION ROUTINE
.   CAN ENTER BY 'AND' (UP TO TEN)
.          OR BY 'OR' (UP TO FIFTEEN)
.
CATPICK  DISPLAY   *P1:2,*EF,*P20:2," C A T E G O R Y   P I C K - O F F ":
            *P1:23,"Please enter the category codes you would like to":
                   " choose (hit '*' if finished)":
                   *P41:24,"Writing to ",NEWNAME;
.                   *P51:24,"Writing to ",NEWNAME;
.
ANDOR    KEYIN     *P10:6,"Is this category pick-off an '(A)nd' or an '(O)r'":
                   " retrieval (A/O)? ",ANDOR;
         CMATCH    "A",ANDOR
         GOTO      OR IF NOT EQUAL
         GOTO      ANDOR IF EOS
         MOVE      "and",ANDORTXT
         GOTO      CAT
OR       CMATCH    "O",ANDOR
         GOTO      ANDOR IF NOT EQUAL
         MOVE      " or",ANDORTXT
CAT      CLEAR     CATPICK
         MOVE      "7",V
CAT1
.START PATCH 7.17 REPLACED LOGIC
.         KEYIN     *P21:V,*EL,"Category Code:",*JL,NCATFLD;
.         CMATCH    "*",NCATFLD
.         GOTO      CAT1 IF EOS
.         GOTO      FINCAT IF EQUAL
.           CALL      NCATKEY
.           MOVE      NCATDESC TO STR24
          KEYIN     *P21:V,*EL,"Category Code:",*JL,STR3;
          CMATCH    "*",STR3
          GOTO      CAT1 IF EOS
          GOTO      FINCAT IF EQUAL
          PACK      NREFFLD,"T",STR3
          CALL      NREFKEY
          MOVE      NREFDESC TO STR24
.END PATCH 7.17 REPLACED LOGIC
         GOTO      CAT1OK IF NOT OVER
         DISPLAY   *P40:V,"Category Code not on file.",*W2;
         GOTO      CAT1
CAT1OK   MOVE      YES TO STR1
           KEYIN     *P38:V,"- ",*DV,STR24,*P61:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      CAT1 IF EQUAL
         GOTO      CAT1OK IF EOS
         CMATCH    YES,STR1
         GOTO      CAT1OK IF NOT EQUAL
.START PATCH 7.17 REPLACED LOGIC
.         SETLPTR    NCATFLD TO 3
.         APPEND    NCATFLD,CATPICK
.         reset     ncatfld
          SETLPTR   STR3,3
          APPEND    STR3,CATPICK
          reset     STR3
.END PATCH 7.17 REPLACED LOGIC
CAT2     ADD      C1,V
CAT2B
.START PATCH 7.17 REPLACED LOGIC
.         KEYIN     *P31:V,*EL,*DV,ANDORTXT,":",*JL,NCATFLD
.         CMATCH    "*",NCATFLD
.         GOTO      FINCAT IF EQUAL
.         GOTO      CAT2B IF EOS
.         CALL          NCATKEY
          KEYIN     *P31:V,*EL,*DV,ANDORTXT,":",*JL,STR3
          CMATCH    "*",STR3
          GOTO FINCAT IF EQUAL
          GOTO CAT2B IF EOS
          PACK      NREFFLD,"T",STR3
          CALL      NREFKEY
.END PATCH 7.17 REPLACED LOGIC
         GOTO      CAT2OK IF NOT OVER
         DISPLAY   *P40:V,"Category Code not on file.",*W2;
         GOTO      CAT2B  
CAT2OK   MOVE      YES TO STR1
.START PATCH 7.17 REPLACED LOGIC
.           MOVE      NCATDESC TO STR24
          MOVE      NREFDESC TO STR24
.END PATCH 7.17 REPLACED LOGIC
           KEYIN     *P38:V,"- ",*DV,STR24,*P61:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      CAT2B IF EQUAL
         GOTO      CAT2OK IF EOS
         CMATCH    YES,STR1
         GOTO      CAT2OK IF NOT EQUAL
.START PATCH 7.17 REPLACED LOGIC
.         SETLPTR    NCATFLD TO 3
.         APPEND    NCATFLD,CATPICK
          SETLPTR   STR3,3
          APPEND    STR3,CATPICK
.END PATCH 7.17 REPLACED LOGIC
         CMATCH    "A",ANDOR
         GOTO      ORCHECK IF NOT EQUAL
         COMPARE   "16",V
         GOTO      FINCAT IF EQUAL
         GOTO      CAT2
ORCHECK  COMPARE   "21",V
         GOTO      CAT2 IF NOT EQUAL
.
FINCAT   MOVEFPTR  CATPICK TO N3
         COMPARE   C0 TO N3                    ANY CATEGORIES ENTRD?
         GOTO      CHOICE IF EQUAL               NO.
.
OMITCAT  MOVE     C1 TO OMITBRCH               DEFAULT TO NO OMITS.
         KEYIN     *P15:21,"Would you like to OMIT any ":
                           "categories? ",*uc,STR1,*lc;
         CMATCH    NO TO STR1
         GOTO      OMITCAT IF EOS
         GOTO      DATESEC IF EQUAL              NO OMISSIONS.
         CMATCH    YES TO STR1
         GOTO      OMITCAT IF NOT EQUAL
.
         CLEAR     CATOMIT
         DISPLAY   *P23:14,*EF,"Category Codes to be OMITTED:";
         MOVE      "15",V
OMITNEXT
.START PATCH 7.17 REPLACED LOGIC
.         KEYIN     *P21:V,*EL,"Category Code:",*JL,NCATFLD;
.         CMATCH    "*",NCATFLD
.         GOTO      OMITNEXT IF EOS
.         GOTO      FINOMIT IF EQUAL
.           CALL      NCATKEY
          KEYIN     *P21:V,*EL,"Category Code:",*JL,STR3;
          CMATCH    "*",STR3
          GOTO OMITNEXT IF EOS
          GOTO FINOMIT IF EQUAL
          PACK      NREFFLD,"T",STR3
          CALL      NREFKEY
.END PATCH 7.17 REPLACED LOGIC
         GOTO      OMITOK IF NOT OVER
         DISPLAY   *P40:V,"Category Code not on file.",*W2;
         GOTO      OMITNEXT
OMITOK   MOVE          YES TO STR1
.START PATCH 7.17 REPLACED LOGIC
.           MOVE      NCATDESC TO STR24
          MOVE      NREFDESC TO STR24
.END PATCH 7.17 REPLACED LOGIC
           KEYIN     *P38:V,"- ",*DV,STR24,*P61:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      OMITNEXT IF EQUAL
         GOTO      OMITOK IF EOS
         CMATCH    YES,STR1
         GOTO      OMITOK IF NOT EQUAL
.START PATCH 7.17 REPLACED LOGIC
.         SETLPTR    NCATFLD TO 3
.         APPEND    NCATFLD,CATOMIT
          SETLPTR   STR3,3
          APPEND    STR3,CATOMIT
.END PATCH 7.17 REPLACED LOGIC
         ADD      C1,V
         COMPARE   "19" TO V                     5 OMISSIONS ALLOWED.
         GOTO      OMITNEXT IF NOT EQUAL
FINOMIT  MOVEFPTR  CATOMIT TO N3                 ANY OMISSIONS?
         COMPARE   C0 TO N3
         GOTO      DATESEC IF EQUAL              NO.
         MOVE      C0 TO OMITBRCH              YES.
         GOTO      DATESEC
.................................................................................................
srchpick
         GOTO      DATESEC


*
.  EXCLUSIVE LIST SELECTION ROUTINE
.   CAN SELECT BY NY, CALIFORNIA, OR BOTH.
.
EXCLPICK DISPLAY   *P1:2,*EF,*P19:2,"***E X C L U S I V E   P I C K - ":
                   "O F F***";
         CMATCH    "C",CO                        CMP DOES NOT HAVE EXCL.
         GOTO      CHOICE IF EQUAL
.begin patch 7.0
EXCL2    if        (autoflag < c1)
         KEYIN     *P19:6,"(C)alifornia   or   (P)acific Lists  or   (B)oth? ":
                   EXCLCODE;
         else
.begin patch 7.3
.         move      "C" to exclcode
         move      "B" to exclcode
.end patch 7.3
         endif
.end patch 7.0
.begin patch 7.3
          if        (EXCLCODE = "B" or EXCLCODE = "C" or EXCLCODE = "P")
          goto      Datesec
          elseif    (EXCLCOde = "")
          goto      EXCL2
          endif
         CMATCH    NO TO EXCLCODE
         GOTO      EXCL2 IF EOS
.         GOTO      DATESEC IF EQUAL
.         CMATCH    "C" TO EXCLCODE
.         GOTO      DATESEC IF EQUAL
.         CMATCH    "B" TO EXCLCODE
.         GOTO      CHOICE IF NOT EQUAL
.         GOTO      DATESEC
          GOto      Choice
.end patch 7.3
*
.  NEW LIST SELECTION ROUTINE
.
NEWPICK  DISPLAY   *P1:2,*EF,*P21:2,"***N E W   L I S T   P I C K - O F F***";
         GOTO      DATESEC
*
.  WITHDRAWN LIST SELECTION ROUTINE
.
WITHPICK DISPLAY   *P1:2,*EF,*P19:3,"***W I T H D R A W N   P I C K - O F F***"
WITH2    KEYIN     *P19:6,"(W)ithdrawn   or   (T)empwith   or   (B)oth? ":
                   WITHCODE;
         MOVE      "1" TO WITHDRAW
         CMATCH    "W" TO WITHCODE
         GOTO      DATESEC IF EQUAL
         GOTO      WITH2 IF EOS
         CMATCH    "T" TO WITHCODE
         GOTO      DATESEC IF EQUAL
         CMATCH    "B" TO WITHCODE
         GOTO      CHOICE IF NOT EQUAL
         GOTO      DATESEC
*
.  REVISE DATE SELECTION ROUTINE
.   CAN SELECT BY YEAR, MONTH/YEAR OR MONTH/DAY/YEAR.
.   OTHER PICK-OFFS WILL GO TO DATESEC TO SEE IF THE USER WANTS A
.    SECONDARY DATE PICK-OFF.
.
DATEPICK DISPLAY   *P1:2,*EF,*P17:3,"***R E V I S E   D A T E   P I C K":
                   " - O F F***";
         GOTO      DATECHCE
DATESEC  MOVE      C0 TO REVBRCH
         move      no to str1
.begin patch 7.0
         if        (autoflag < c1)
         KEYIN    *P1:24,*EL,*P15:24,"Do you want a secondary search by ":
                   "REVISE DATE? ",*dv,*cyan,STR1,*white,*p73:24,*rv,*uc,str1,*lc;
         endif
.end patch 7.0
         CMATCH    YES,STR1
         GOTO      DATESEC IF EOS
         GOTO      DATESEC2 IF EQUAL
         CMATCH    NO,STR1
         GOTO      DATESEC IF NOT EQUAL
         GOTO      MLROMIT
DATESEC2 DISPLAY   *P8:3,"***S E C O N D A R Y   R E V I S E   D A T E   ":
                   "P I C K - O F F***";
DATECHCE KEYIN    *P1:11,*EF,"How would you like to choose the revise dates:":
                   *P49:11,"1. by YEAR?":
                   *P49:12,"2. by MONTH/YEAR?":
                   *P49:13,"3. by MONTH/DAY/YEAR?":
                   *P71:13,"Choose: ",REVBRCH;
         MOVE     C1 TO REVCOUNT
         MOVE      "14",V
         BRANCH    REVBRCH OF YRSEL,MOSEL,DYSEL
         GOTO      DATECHCE
.
. SELECT BY YEAR... 2 POSITION YEAR FIELDS ARE APPENDED TO REVPICK.
.
YRSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
YRROW    BRANCH    N1 OF YRROW1,YRROW2,YRROW3,YRROW4,ENDYR
YRROW1   MOVE      "03" TO H1
         MOVE      "16" TO H2
         GOTO      YRPICK
YRROW2   MOVE      "21" TO H1
         MOVE      "35" TO H2
         GOTO      YRPICK
YRROW3   MOVE      "40" TO H1
         MOVE      "54" TO H2
         GOTO      YRPICK
YRROW4   MOVE      "59" TO H1
         MOVE      "73" TO H2
YRPICK   KEYIN     *PH1:V,*DV,REVCOUNT,") 20",*ZF,*JR,YEAR;
         MATCH     "0*",YEAR
         GOTO      ENDYR IF EQUAL
         GOTO      YRPICK IF EOS
         TYPE      YEAR
         GOTO      YRPICK IF NOT EQUAL
YROK     MOVE          YES TO STR1
           KEYIN     *PH2:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      YRPICK IF EQUAL
         GOTO      YROK IF EOS
         CMATCH    YES,STR1
         GOTO      YROK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    YEAR,REVPICK
         append    dash,revpick
         ADD      C1,V
         COMPARE   "21",V
         GOTO      YRPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      YRROW
ENDYR    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      MLROMIT
.
. SELECT BY MONTH/YEAR... 4 POSITION MONTH/YEAR FIELDS ARE APPENDED
.                         TO REVPICK.
.
MOSEL    CLEAR     REVPICK
         MOVE     C1 TO N1
MOROW    BRANCH    N1 OF MOROW1,MOROW2,MOROW3,MOROW4,ENDMO
MOROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "16" TO H4
         GOTO      MOPICK
MOROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "35" TO H4
         GOTO      MOPICK
MOROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "47" TO H3
         MOVE      "54" TO H4
         GOTO      MOPICK
MOROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "73" TO H4
MOPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDMO IF EQUAL
         GOTO      MOPICK IF EOS
         TYPE      MO
         GOTO      MOPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      MOPICK IF EOS
         GOTO      MOPICK IF NOT EQUAL
MOOK     MOVE      YES TO STR1
           KEYIN     *PH4:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      MOPICK IF EQUAL
         GOTO      MOOK IF EOS
         CMATCH    YES,STR1
         GOTO      MOOK IF NOT EQUAL
         ADD      C1,REVCOUNT
         APPEND    MO,REVPICK
         APPEND    YEAR,REVPICK
         append    dash,revpick
         ADD      C1,V
         COMPARE   "21",V
         GOTO      MOPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      MOROW
ENDMO    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
         GOTO      MLROMIT
.
. SELECT BY MONTH/DAY/YEAR...  8 POSITION MO/DY/YR FIELDS ARE APPENDED
.                              TO REVPICK & REVPICK2 IF NEEDED.
.
DYSEL    CLEAR     REVPICK
         CLEAR     REVPICK2
         MOVE     C1 TO N1
DYROW    BRANCH    N1 OF DYROW1,DYROW2,DYROW3,DYROW4,ENDDY
DYROW1   MOVE      "03" TO H1
         MOVE      "07" TO H2
         MOVE      "10" TO H3
         MOVE      "13" TO H4
         MOVE      "16" TO H5
         GOTO      DYPICK
DYROW2   MOVE      "21" TO H1
         MOVE      "25" TO H2
         MOVE      "28" TO H3
         MOVE      "31" TO H4
         MOVE      "35" TO H5
         GOTO      DYPICK
DYROW3   MOVE      "40" TO H1
         MOVE      "44" TO H2
         MOVE      "48" TO H3
         MOVE      "50" TO H4
         MOVE      "54" TO H5
         GOTO      DYPICK
DYROW4   MOVE      "59" TO H1
         MOVE      "63" TO H2
         MOVE      "66" TO H3
         MOVE      "69" TO H4
         MOVE      "73" TO H5
DYPICK   KEYIN     *PH1:V,*DV,REVCOUNT,")   /  /":
                   *PH2:V,*+,*ZF,*JR,MO,*PH3:V,*ZF,*JR:
                   DAY,*PH4:V,*ZF,*JR,*-,YEAR;
         MATCH     "0*",MO
         GOTO      ENDDY IF EQUAL
         GOTO      DYPICK IF EOS
         TYPE      MO
         GOTO      DYPICK IF NOT EQUAL
         TYPE      DAY
         GOTO      DYPICK IF NOT EQUAL
         TYPE      YEAR
         GOTO      DYPICK IF NOT EQUAL
DYOK     MOVE          YES TO STR1
           KEYIN     *PH5:V,"OK?",*RV,*uc,STR1,*lc;
         CMATCH    NO,STR1
         GOTO      DYPICK IF EQUAL
         GOTO      DYOK IF EOS
         CMATCH    YES,STR1
         GOTO      DYOK IF NOT EQUAL
         PACK      DATE WITH MO,SLASH,DAY,SLASH,YEAR
         BRANCH    N1 TO DYAPP,DYAPP,DYAPP2,DYAPP2
DYAPP    APPEND    DATE TO REVPICK
         GOTO      DYCOUNT
DYAPP2   APPEND    DATE TO REVPICK2
DYCOUNT  ADD      C1,REVCOUNT
         ADD      C1,V
         COMPARE   "21",V
         GOTO      DYPICK IF NOT EQUAL
         MOVE      "14",V
         ADD      C1 TO N1
         GOTO      DYROW
ENDDY    MOVEFPTR  REVPICK TO N3
         COMPARE   C0 TO N3
         GOTO      CHOICE IF EQUAL
*
. MAILER OMISSION SELECTION ROUTINE...
. USE MAY ENTER 20 MLR#'S OR OFFER#'S.  PROGRAM WILL READ THE ORDER
. FILE TO CHECK FOR USAGE BY THESE MAILER/OFFERS AFTER CHECKING ALL
. OTHER CRITERIA.  IF THE LIST HAS BEEN USED BY ONE OF THESE
. MAILER/OFFERS, IT WILL NOT BE INCLUDED IN THE PICK-OFF.
.
MLROMIT  MOVE     C1 TO MLRBRCH                DEFAULT TO NO MLR'S
         MOVE      C0 TO MLRPAGE
         move      no to str1
.begin patch 7.0
         if        (autoflag < c1)
         KEYIN     *P1:24,*EL,"Do you want to omit cards already ":
                   "used by a client? ",*cyan,*dv,STR1,*white,*p55:24,*rv,*uc,str1,*lc;
         endif
.end patch 7.0
         CMATCH    NO TO STR1
         GOTO      MLROMIT IF EOS
         GOTO      RETRIEVE IF EQUAL             NO MLR'S
         CMATCH    YES TO STR1
         GOTO      MLROMIT IF NOT EQUAL
.
         MOVE      "2" TO MLRBRCH                OMIT BY MAILER#
         KEYIN     *P1:24,*EL,"Omit by mailer number or offer ":
                              "number? M/O: ",*uc,STR1,*lc;
         CMATCH    "M" TO STR1
         GOTO      MLROMIT IF EOS
         GOTO      KEYOMITS IF EQUAL
         CMATCH    "O" TO STR1
         GOTO      MLROMIT IF NOT EQUAL
         MOVE      "3" TO MLRBRCH                OMIT BY OFFER#
.
KEYOMITS DISPLAY   *P1:17,*EF,*P20:17,"***MAILER/OFFER OMISSION***";
         CLEAR     MLROMIT                       MLR/OFFER OMIT STRING
         BRANCH    MLRBRCH TO BYMLR,BYMLR,BYOFR
.
. OMIT BY MAILER NUMBER...
.
BYMLR    MOVE      "17" TO V
NEXTMLR  ADD      C1 TO V
KYMLR    KEYIN     *P7:V,*EL,"Mailer##:",*ZF,*JR,MLR;
         MATCH     "000*" TO MLR
         GOTO      kymlr IF EOS
         GOTO      ENDMLR IF EQUAL
         TYPE      MLR
         GOTO      kymlr IF NOT EQUAL
         PACK      MKEY WITH MLR,C0,C0,C0
           CALL      NMLRKEY
         GOTO      OKMLR IF NOT OVER
         DISPLAY   "- not on file.",*W2;
         GOTO      kymlr
OKMLR    MOVE          YES TO STR1
           KEYIN     "- ",*DV,MNAME,*DV,MCOMP," OK? ",*RV,*uc,STR1,*lc;
         CMATCH    NO TO STR1
         GOTO      OKMLR IF EOS
         GOTO      kymlr IF EQUAL
         CMATCH    YES TO STR1
         GOTO      OKMLR IF NOT EQUAL
         APPEND    MLR TO MLROMIT
         APPEND    " " TO MLROMIT
         COMPARE   "22" TO V
         GOTO      NEXTMLR IF NOT EQUAL
         ADD       "1" TO MLRPAGE
         COMPARE   "2" TO MLRPAGE
         GOTO      ENDMLR IF EQUAL
         GOTO      ENDMLR IF NOT LESS
         MOVE      "17" TO V
         DISPLAY   *P1:V,*EF,*P20:17,"***MAILER/OFFER OMISSION***";
         GOTO      NEXTMLR
ENDMLR   RESET     MLROMIT
         GOTO      ORDTRNG                       CHECK FOR ORDER DATES
.
. OMIT BY OFFER NUMBER...
.
BYOFR    MOVE      "17" TO V
NEXTOFR  ADD      C1 TO V
KEYOFR   KEYIN     *P7:V,*EL,"Offer##:",*ZF,*JR,NOFRFLD;
         MATCH     "000000*" TO NOFRFLD
         GOTO      KEYOFR IF EOS
         GOTO      ENDOFR IF EQUAL
         TYPE      NOFRFLD
         GOTO      KEYOFR IF NOT EQUAL
           CALL      NOFRKEY
         GOTO      OKOFR IF NOT OVER
         DISPLAY   "- not on file.",*W2;
         GOTO      KEYOFR
OKOFR    MOVE          YES TO STR1
           KEYIN     "- ",*DV,OFDESC," OK? ",*RV,*uc,STR1,*lc;
         CMATCH    NO TO STR1
         GOTO      OKOFR IF EOS
         GOTO      KEYOFR IF EQUAL
         CMATCH    YES TO STR1
         GOTO      OKOFR IF NOT EQUAL
         APPEND    NOFRFLD TO MLROMIT
         APPEND    " " TO MLROMIT
         COMPARE   "22" TO V
         GOTO      NEXTOFR IF NOT EQUAL
ENDOFR   GOTO      ORDTRNG                       CHECK FOR ORDER DATES
. 
ORDTRNG  DISPLAY   *P1:23,*EL,"Order Date Range:  Start date:__/__/__":
                              "  End Date:__/__/__";
         KEYIN     *P31:23,*+,MM,*P34:23,DD,*P37:23,*-,YY;
         CALL      CVTJUL
         MOVE      JULDAYS TO LODATE
         KEYIN     *P50:23,*+,MM,*P53:23,DD,*P56:23,*-,YY;
         CALL      CVTJUL
         MOVE      JULDAYS TO HIDATE
           MOVE      YES TO STR1
         KEYIN     *P65:23,"OK? ",*RV,*uc,STR1,*lc;
         CMATCH    YES TO STR1
         GOTO      ORDTRNG IF NOT EQUAL
.
+.......................................................................
.
. RETRIEVE RECORDS SECTION...
.
RETRIEVE DISPLAY   *P1:24,*EF,*P8:24,"Records searched,       ":
                    "of which match the parameters indicated.":
                   *P43:24,"Writing to ",NEWNAME;
.                   *P51:24,"Writing to ",NEWNAME;
.
         TABPAGE
          if        (numpick = c10)         
         PACK      NdatFLD2 FROM "02F",searchstr
         CLEAR     NdatFLD1
         CALL      NdatAIM
         GOTO      FINISH IF OVER
         BRANCH    WITHDRAW TO RETBRNCH          INCLUDE WITHDRWN LISTS?
         CMATCH    "W" TO STATUS                 NO. THIS ONE WITHDRWN?
         GOTO      READNEXT1 IF EQUAL                 YES.
         CMATCH    "T" TO STATUS
         GOTO      READNEXT1 IF EQUAL                 YES.
         move       "CPB",str3
          scan      exclcode in str3
          if        not equal
          goto      retbrnch
          endif
          if        (exclcode = "C" & Elstcde = "C")
          goto      retbrnch
          endif
          if        (exclcode = "P" & Elstcde = "P")
          goto      retbrnch
          endif
          if        (exclcode = "B" & (Elstcde = "P" or Elstcde = "C"))
          goto      retbrnch
          endif

READNEXT1 loop
         CALL      NdatKG
          until over
         BRANCH    WITHDRAW TO RETBRNCH          INCLUDE WITHDRWN LISTS?
         CMATCH    "W" TO STATUS                 NO. THIS ONE WITHDRWN?
         GOTO      READNEXT1 IF EQUAL                 YES.
         CMATCH    "T" TO STATUS
         GOTO      READNEXT1 IF EQUAL                 YES.
         move       "CPB",str3
          scan      exclcode in str3
          if        not equal
          goto      retbrnch
          endif
          if        (exclcode = "C" & Elstcde = "C")
          goto      retbrnch
          endif
          if        (exclcode = "P" & Elstcde = "P")
          goto      retbrnch
          endif
          if        (exclcode = "B" & (Elstcde = "P" or Elstcde = "C"))
          goto      retbrnch
          endif

         goto       readnext1


          repeat          
          goto      Finish
          endif
          
READNEXT call      rotdial
         CALL          NDATSEQ
         GOTO      FINISH IF OVER
         ADD      C1,TOTAL
         DISPLAY   *P1:24,TOTAL;

         BRANCH    WITHDRAW TO RETBRNCH          INCLUDE WITHDRWN LISTS?
         CMATCH    "W" TO STATUS                 NO. THIS ONE WITHDRWN?
         GOTO      READNEXT IF EQUAL                 YES.
         CMATCH    "T" TO STATUS
         GOTO      READNEXT IF EQUAL                 YES.
.
RETBRNCH REPLACE   ZFILL IN REVDATE               SET UP DATE FOR LATER.
.START PATCH 7.17 REPLACED LOGIC
.         UNPACK    REVDATE TO MO,STR1,DAY,STR1,cc,YEAR
          UNPACK    REVDATE,CC,YEAR,MO,DAY
.END PATCH 7.17 REPLACED LOGIC
.
         BRANCH    NUMPICK TO SETOWN,SETOWN,SETCAT,SETEXCL:
                              SETNEW,SETDATE,SETWITH,SETDAT1,SetSB,setsrch
setsrch
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MATCHED. DATE CHECK?
         GOTO      MLROMITS                               NO.

*
. OWNER RETRIEVAL...
.
SETOWN   RESET     LISTOWN                       CHOSEN OWNERS STRING
.START PATCH 7.17 REPLACED LOGIC
.         SCAN      OWNNUM IN LISTOWN               CHECK AGAINST RECORD.
          bump      OWNNUM,2
          SCAN      OWNNUM IN LISTOWN               CHECK AGAINST RECORD.
.END PATCH 7.17 REPLACED LOGIC
         GOTO      READNEXT IF NOT EQUAL         NO MATCH.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MATCHED. DATE CHECK?
         GOTO      MLROMITS                               NO.
*
. SB RETRIEVAL...
.
SETSB   RESET     LISTSB                       CHOSEN  STRING

          SCAN      DATFUL IN LISTSB               CHECK AGAINST RECORD.
.END PATCH 7.17 REPLACED LOGIC
         GOTO      READNEXT IF NOT EQUAL         NO MATCH.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MATCHED. DATE CHECK?
         GOTO      MLROMITS                               NO.
*
. CATEGORIES RETRIEVAL...
.
SETCAT   RESET     CATPICK                       CHOSEN CATEGORIES
         MOVE      CATMASK TO CATS               SET UP THE SCAN STRING
.START PATCH 7.17 REPLACED LOGIC
........................................................................................
.NOTE:  THIS IS A CHEAT.  EACH DATACARD IS ALLOWED HUNDREDS OF CATEGORIES, BUT I WILL ONLY
.       ALLOW 10, UNTIL WE REWRITE THIS PROGRAM.
........................................................................................
.           PACK      CATS FROM CATCDE1,DASH,CATCDE2,DASH,CATCDE3,DASH,CATCDE4:
.                       DASH,CATCDE5,DASH,CATCDE6,DASH,CATCDE7,DASH,CATCDE8,DASH:
.                       CATCDE9,DASH,CATCDE10
          packkey   taskname,taskname
          clear     taskname
          pack      NCATFLD1,"01X",LSTNUM
          move      "NCATAIM",Location
          pack      KeyLocation,"Key: ",NCATFLD1
          call      NCATAIM
          for result,"1","10"
                    if over
                              break
                    endif
                    if (result <> 1)
                              append    DASH,taskname
                    endif
                    append    NCATCODE,taskname
                    append    NCATNUM,taskname
                    move      "NCATKG",Location
                    call      NCATKG
          repeat
          reset     taskname
          call      Trim using taskname
          pack      CATS,taskname
.END PATCH 7.17 REPLACED LOGIC
.         EDIT      CATEGORY TO CATS              PUT '-' BETWEEN EACH CAT
         BRANCH    OMITBRCH TO CHKANDOR          CATEGORY OMISSIONS?
*
. CATEGORY OMISSIONS...
.
         RESET     CATOMIT                       CHOSEN CATEGORY OMITS
NEXTOMIT
.START PATCH 7.17 REPLACED LOGIC
.         MOVE      CATOMIT TO NCATFLD                MOVE TO 3 POS
.         SCAN      NCATFLD IN CATS                   IS IT IN THE RECORD?
         MOVE      CATOMIT TO str3                MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
.END PATCH 7.17 REPLACED LOGIC
         GOTO      READNEXT IF EQUAL             YES.
         BUMP      CATOMIT BY 3                  NO. MORE TO CHECK?
         GOTO      NEXTOMIT IF NOT EOS               YES.
.
CHKANDOR CMATCH    "A",ANDOR                         NO.
         GOTO      NEXTAND IF EQUAL
*
. 'OR'
.
NEXTOR
.START PATCH 7.17 REPLACED LOGIC
.         MOVE      CATPICK,NCATFLD                   MOVE TO 3 POS
.         SCAN      NCATFLD IN CATS                   IS IT IN THE RECORD?
         MOVE      CATPICK,str3                   MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
.END PATCH 7.17 REPLACED LOGIC
         GOTO      ENDCAT IF EQUAL               YES.
         BUMP      CATPICK BY 3                  NO. MORE TO CHECK?
         GOTO      READNEXT IF EOS                   NO.
         GOTO      NEXTOR                            YES.
*
.'AND'
.
NEXTAND
.START PATCH 7.17 REPLACED LOGIC
.         MOVE      CATPICK,NCATFLD                   MOVE TO 3 POS
.         SCAN      NCATFLD IN CATS                   IS IT IN THE RECORD?
         MOVE      CATPICK,str3                   MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
.END PATCH 7.17 REPLACED LOGIC
         GOTO      READNEXT IF NOT EQUAL         NO.
         RESET     CATS
         BUMP      CATPICK BY 3                  YES. MORE TO CHECK?
         GOTO      ENDCAT IF EOS                      NO.
         GOTO      NEXTAND                            YES.
.
ENDCAT   BRANCH    REVBRCH TO SETYR,SETMO,SETDY  DATE CHECK?
         GOTO      MLROMITS                      NO.
*
. WITHDRAWN RETRIEVAL...
.
SETWITH  CMATCH    "B" TO WITHCODE              ALL WITHDRAWN?
         GOTO      SETWITH1 IF NOT EQUAL         NO.
         CMATCH    "W" TO STATUS                 YES, WITHDRAWN?
         GOTO      SETWITHX IF EQUAL             YES.
         CMATCH    "T" TO STATUS                 NO,  TEMPWITH?
         GOTO      SETWITHX IF EQUAL             YES.
         GOTO      READNEXT                      NO.
SETWITH1 CMATCH    "W" TO WITHCODE              SELECT ONLY WITHDRAWS.
         GOTO      SETWITHW IF EQUAL             YES.
SETWITHT CMATCH    "T" TO WITHCODE               SELECT ONLY TEMPWITHS?
         GOTO      READNEXT IF NOT EQUAL         INVALID SELECT.
         CMATCH    "T" TO STATUS                 TEMPWITH?
         GOTO      READNEXT IF NOT EQUAL         NO.
         GOTO      SETWITHX                      YES.
SETWITHW CMATCH    "W",STATUS                    WITHDRAWN LIST?
         GOTO      SETWITHX IF EQUAL             YES.
         GOTO      READNEXT                      NO.
SETWITHX BRANCH    REVBRCH TO SETYR,SETMO,SETDY  YES. DATE CHECK?
         GOTO      MLROMITS                           NO.
*
. EXCLUSIVE RETRIEVAL...
.
SETEXCL  CMATCH    NO,EXCLCODE                  PICK NY EXCLUSIVES?
         GOTO      CALEX IF NOT EQUAL            NO.
         CMATCH    NO,ELSTCDE                      YES. IS THIS ONE?
         GOTO      READNEXT IF NOT EQUAL              NO.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY       YES. DATE CHECK?
         GOTO      MLROMITS                                NO.
CALEX    CMATCH    "C",EXCLCODE                  PICK CAL EXCLUSIVES?
.begin patch 7.3
.         GOTO      BOTHEX IF NOT EQUAL           NO.
         GOTO      PLIEX IF NOT EQUAL           NO.
.end patch 7.3
         CMATCH    "C",ELSTCDE                      YES. IS THIS ONE?
         GOTO      READNEXT IF NOT EQUAL              NO.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY       YES. DATE CHECK?
         GOTO      MLROMITS                                NO.
.begin patch 7.3
PLIEX    CMATCH    "P",EXCLCODE                  PICK PLI EXCLUSIVES?
         GOTO      BOTHEX IF NOT EQUAL           NO.
         CMATCH    "P",ELSTCDE                      YES. IS THIS ONE?
         GOTO      READNEXT IF NOT EQUAL              NO.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY       YES. DATE CHECK?
         GOTO      MLROMITS                                NO.
.end patch 7.3

BOTHEX   CMATCH    "B",EXCLCODE                  PICK ANY EXCLUSIVE?
         GOTO      READNEXT IF NOT EQUAL         NO. ENTRY IS NG.
.begin patch 7.3
.         CMATCH    " ",ELSTCDE                      YES. IS THIS ONE?
.         GOTO      READNEXT IF EQUAL                  NO.
          IF        (Elstcde = "C" or Elstcde = "P")
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY       YES. DATE CHECK?       
         GOTO      MLROMITS                                NO.
          Else
          goto      Readnext
          endif
.end branch 7.3
*         
. NEW LIST RETRIEVAL...
.
SETNEW   CMATCH    YES,NLSTCDE                       NEW LIST RECORD?
         GOTO      READNEXT IF NOT EQUAL         NO.
         BRANCH    REVBRCH TO SETYR,SETMO,SETDY  YES. DATE CHECK?
         GOTO      MLROMITS                           NO.
*
. BY DATE RETRIEVAL...
.
SETDATE  BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MAJOR PICK-OFF BY DATE.
*
. BY YEAR RETRIEVAL...
.
SETYR    RESET     REVPICK                       GET FP READY FOR SCAN.
         SCAN      YEAR IN REVPICK               IS IT IN THE STRING?
         GOTO      MLROMITS IF EQUAL             YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. BY MONTH RETRIEVAL...
.
SETMO    RESET     REVPICK                       GET FP READY FOR SCAN.
         PACK      MOYR WITH MO,YEAR             SET UP SEARCH STRING.
         SCAN      MOYR IN REVPICK               IS IT IN THE STRING?
         GOTO      MLROMITS IF EQUAL             YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. BY DAY RETRIEVAL...
.
SETDY    RESET     REVPICK                       GET FP READY FOR SCAN.
         RESET     REVPICK2                               "  "
.START PATCH 7.17 REPLACED LOGIC
.         SCAN      REVDATE IN REVPICK            IS IT IN 1ST STRING?
.         GOTO      MLROMITS IF EQUAL             YES.
.         SCAN      REVDATE IN REVPICK2           NO. HOWBOUT 2ND STRING?
          unpack    REVDATE,str4,MM,DD
          pack      str10,MM,DD,str4
          SCAN      str10 IN REVPICK            IS IT IN 1ST STRING?
          GOTO      MLROMITS IF EQUAL             YES.
          SCAN      str10 IN REVPICK2           NO. HOWBOUT 2ND STRING?
.END PATCH 7.17 REPLACED LOGIC
         GOTO      MLROMITS IF EQUAL                 YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. BY PUT UP DATE RETRIEVAL...
.
SETDAT1  clear     mo
         clear     year
         clear     day
.START PATCH 7.17 REPLACED LOGIC
.         UNPACK    NEWDATE INTO MO,DAY,cc,YEAR
         UNPACK    NEWDATE INTO CC,YEAR,MO,DAY
.END PATCH 7.17 REPLACED LOGIC
         REP       ZFILL IN MO
         REP       ZFILL IN DAY
         REP       ZFILL IN YEAR
.patch for weirdstuff
         move      c0 to n2
         move      mo to n2
         if        (n2 < 1 or n2 > 12)
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
         endif
         move      c0 to n2
         move      Day to n2
         if        (n2 < 1 or n2 > 31)
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
         endif
.         move      c0 to n2
.         move      CC to n2
.         if        (n2 <> 19 or n2 <> 20)
.         goto      readnext
.         endif
.end patch for weirdstuff
         BRANCH    REVBRCH TO SETYR1,SETMO1,SETDY1  MAJOR PICK-OFF BY DATE.
*
. BY YEAR RETRIEVAL...
.
SETYR1   RESET     REVPICK                       GET FP READY FOR SCAN.
         SCAN      YEAR IN REVPICK               IS IT IN THE STRING?
         GOTO      MLROMITS IF EQUAL             YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. BY MONTH RETRIEVAL...
.
SETMO1   RESET     REVPICK                       GET FP READY FOR SCAN.
         clear     moyr

         PACK      MOYR WITH MO,YEAR             SET UP SEARCH STRING.
         SCAN      MOYR IN REVPICK               IS IT IN THE STRING?
         GOTO      MLROMITS IF EQUAL             YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. BY DAY RETRIEVAL...
.
SETDY1   RESET     REVPICK                       GET FP READY FOR SCAN.
         RESET     REVPICK2                               ."
.START PATCH 7.17 REPLACED LOGIC
.         SCAN      NEWDATE IN REVPICK            IS IT IN 1ST STRING?
.         GOTO      MLROMITS IF EQUAL             YES.
.         SCAN      NEWDATE IN REVPICK2           NO. HOWBOUT 2ND STRING?
          unpack    NEWDATE,str4,MM,DD
          pack      str10,MM,DD,str4
          SCAN      str10 IN REVPICK            IS IT IN 1ST STRING?
          GOTO      MLROMITS IF EQUAL             YES.
          SCAN      str10 IN REVPICK2           NO. HOWBOUT 2ND STRING?
.END PATCH 7.17 REPLACED LOGIC
         GOTO      MLROMITS IF EQUAL                 YES.
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      NO.
          endif
*
. OMIT RECORD BASED ON MLR/OFFER OMISSION...
.
MLROMITS BRANCH    MLRBRCH TO WRITEREC           ANY MLR OMISSIONS?
.
CHKORDRS CLEAR     NORDFLD1
         MOVE      MLROMIT TO MLR
         PACK      NORDFLD1 FROM OKEY1,MLR
         REP       ZFILL IN LSTNUM
         PACK      NORDFLD2 FROM OKEY2,LSTNUM
           CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         DISPLAY   *P1:22,*EL,*HON,"FIRST ORDER READ FOR ##: ",LSTNUM:
                   "  KEYS= ",NORDFLD1,"  ",NORDFLD2,*W
           MOVE    C2 TO NORDPATH
           CALL    NORDAIM
.         READ    ORDMST,ORDKEY1,ORDKEY2;ORD2,ORDMLR,ORD9,ORDLIST,ORD84,ORDOFR:
.                          ORD54,ORDDATE
         GOTO      NOHIT IF OVER                 NO MATCH.
         GOTO      CKORDPAR                      MATCH CHECK OTHER PAR.
.CHKORDRS READ      ORDMST,LSTNUM;;                 SET POINTER
NEXTORDR DISPLAY   *P1:22,*EL,*HON,"READ ORDER FILE GENERIC FOR ##: ",LSTNUM;
           CALL      NORDKG
.         READKG    ORDMST;ORD2,ORDMLR,ORD9,ORDLIST,ORD84,ORDOFR:
.                          ORD54,ORDDATE
         GOTO      NOHIT IF OVER                 NO MATCH.
         GOTO      CKORDPAR                       MATCH, CHECK OTHER PAR
.         MATCH     LSTNUM TO ORDLIST               STILL IN LIST#?
.         GOTO      WRITEREC IF NOT EQUAL         NO. REC IS OK.
.         RESET     MLROMIT                       GET FP READY FOR SCAN
.         BRANCH    MLRBRCH TO CHKBYMLR,CHKBYMLR,CHKBYOFR
CKORDPAR  BRANCH    MLRBRCH TO WRITEREC,CKORDRNG,CHKBYOFR
.CHKBYMLR SCAN      ORDMLR IN MLROMIT             IS MLR ONE OF OMITS?
.         GOTO      CKORDRNG IF EQUAL             YES. CHECK THE ORD DT.
.         GOTO      NEXTORDR                      NO.  TRY NEXT ORDER.
.CHKBYOFR SCAN      OODNUM IN MLROMIT             IS OFR ONE OF OMITS?
.         GOTO      CKORDRNG IF EQUAL             YES. CHECK THE ORD DT.
.         GOTO      NEXTORDR                      NO. TRY NEXT ORDER.
CHKBYOFR DISPLAY   *P1:22,*EL,*HON,"CHECKING OFFER";
         SCAN      OODNUM IN MLROMIT             IS OFR ONE OF OMITS?
         GOTO      CKORDRNG IF EQUAL             YES. CHECK ORDER DATE.
         GOTO      NEXTORDR                      NO. TRY NEXT ORDER.
CKORDRNG DISPLAY   *P1:22,*EL,*HON,"CHECKING ORDER DATE";
           MOVE      OODTEM TO MM
           MOVE      OODTED TO DD
           MOVE      OODTEY TO YY
.         UNPACK    ORDDATE TO MM,DD,YY           SET UP ORDER DATE
         CALL      CVTJUL                        CONVERT TO JULIAN
         MOVE      JULDAYS TO ORDJUL
         COMPARE   LODATE TO ORDJUL              CHECK IF IN DTE RNG.
         GOTO      NEXTORDR IF LESS              NO. TRY NEXT ORDER.
         COMPARE   ORDJUL TO HIDATE
         GOTO      NEXTORDR IF LESS              NO. TRY NEXT ORDER.
         GOTO      OMITREC                       YES. OMIT THE RECORD.
. NOHIT
NOHIT    DISPLAY   *P1:22,*EL;
         BRANCH    MLRBRCH TO NOHIT1,NOHIT1,NOHIT2
. NOHIT1 OMIT BY MLR.
NOHIT1   BUMP      MLROMIT BY 5                  GET NEXT MLR TO CHECK.
         GOTO      NOHITW IF EOS                 NO MLR, WRITE RECORD.
         GOTO      NOHITW IF OVER                NO MLR, WRITE RECORD.
         GOTO      CHKORDRS                       MORE RECORDS TO CHECK.
. NOHIT2 OMIT BY MLR/OFFER.
NOHIT2
         BUMP      MLROMIT BY 8                  GET NEXT MLR/OFR TO CHK.
         GOTO      NOHITW IF EOS                 NO MLR, WRITE RECORD.
         GOTO      NOHITW IF OVER                NO MLR, WRITE RECORD.
         MOVE      MLROMIT TO NOFRFLD                PREP FOR OFFER MATCH.
         GOTO      CHKORDRS                       MORE RECORDS TO CHECK.
NOHITW   RESET     MLROMIT                       RESET FP FOR NEXT TIME.
         GOTO      WRITEREC                      GO WRITE REC.
OMITREC  RESET     MLROMIT                       RESET FP FOR NEXT TIME.
         DISPLAY   *P1:22,*EL;
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      YES. OMIT THE RECORD.
          endif
*
. WRITE THE OUTPUT RECORD...
.
WRITEREC
           IFNZ        PC
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
          if        (numpick = c10)
         GOTO      READNEXT1 IF NOT OVER              
          else
         GOTO      READNEXT IF NOT OVER              .DLH 08JUL94
          endif
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
         FLUSH     NEWMST
           XIF
           IFZ         PC
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
          if        (numpick = c10)
         GOTO      READNEXT1 IF NOT OVER              
          else
         GOTO      READNEXT IF NOT OVER              .DLH 08JUL94
          endif
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
           XIF
         ADD      C1 TO FOUND
         DISPLAY   *P26:24,FOUND;
          if        (numpick = c10)
         GOTO      READNEXT1 IF NOT OVER              
          else
         GOTO      READNEXT IF NOT OVER              .DLH 08JUL94
          endif
*
+.......................................................................
.
.CLEANUP - PICKOFF BY (O)ne IS COMPLETE - ALLOW REMOVAL OF MISTAKES.
CLEANUP
         KEYIN     *P1:24,*EL,"Any list you want removed from the pick ? ":
                   *t120,*uc,str1,*lc;
        cmatch    yes to str1
        goto      finish if not equal
        keyin     *p1:24,*el,"What Number ? ",*zf,*jr,str6
        scan      star in str6
        goto      cleanup if equal
        rep       zfill in str6
        read     newmst,str6;datvars
        if       over
        display  *p1:24,*el,"No record in file for ",lstnum,*b,*w2
        goto     cleanup
        endif
        keyin    *p1:24,*el,"Are you sure about ",*dv,olstname," ",*t120,*uc,str1,*lc;
        cmatch   yes to str1
        goto     cleanup if not equal
        delete   newmst,str6
        sub      c1 from found
        goto     cleanup
.FINISH - PICK-OFF IS COMPLETE...
.
FINISH   
.         WEOF      NEWMST,SEQ                    GET EOF IN THERE.
           IFNZ        PC
         FLUSH     NEWMST
           XIF
              if     (found <= c0)
              goto          None
              endif
.         COMPARE   C0 TO FOUND
.         GOTO      NONE IF EQUAL
         flush     newmst
         close     newmst
.         CLOSE     NEWMST,EOFSIZE                DE-ALLOCATE.
           MOVE      C0 TO NEWMFLAG                *FILE CLOSED
           call      funcdisp
         CMATCH    "L",PICK
         GOTO      FINISH2 IF NOT EQUAL
         DISPLAY   *B,*P1:21,*El,*P14:21,"Your Pick-off is completed !":
                   FOUND," Records were found.";
         GOTO      FINISH3
FINISH2  DISPLAY    *B,*P1:21,*El,"Your Pick-off is completed !",FOUND," Rec":
                   "ords were found out of a total file of",TOTAL;
FINISH3  
.         MOVE      YES TO STR1          *IF TIME OUT DEFAULT STR1 = 'Y'.
.begin patch 7.0
         IF        (AUTOFLAG < C1)
                    MOVE      YES TO STR1
*IF TIME OUT DEFAULT STR1 = 'Y'.
                    KEYIN     *p1:22,*el:
                    *P10:22,"The File Name with these records is: ",*DV,NEWNAME:
                   *EL,*p1:24,*el:
                   *P3:24,"Please record this name for future sorting and/or":
                   " printing... Then hit 'Y' ",*T180,*RV,*uc,STR1,*lc;
                    else
                    goto      Nother
         ENDIF
.enD patch 7.0
         CMATCH    YES,STR1
         GOTO      FINISH3 IF EOS
         GOTO      FINISH3 IF NOT EQUAL
           MOVE      C0 TO FOUND              28JUN93 DLH   CLEAR COUNTER
.         branch    repflag of card,rh,bstock,faxcard,lcard,cardview,attcard,nother,RHFLAT
         branch    repflag of rh,Lcard,nother,RHFLAT,RHREV
.         BRANCH    SORT OF nosort,RH,BOTH,BSTOCK,BSTOCKNS,faxcard,lCARD:
.                   Lboth,NOTHER
         GOTO      CARD
NOTHER     move       no to str1
.begin patch 7.0
         IF        (AUTOFLAG < C1)
         KEYIN     *P1:22,*EL,*W4,*P14:22,"Would you like to do another??",*uc,*rv,*T254,STR1
         ENDIF
.enD patch 7.0
         CMATCH     YES,STR1
         GOTO       CHOICE IF EQUAL
         cmatch     no to str1
         goto       nother if not equal
.         shutdown   "cls"
.begin patch 7.0
         IF         (AUTOFLAG < C1)
         STOP
         ELSE
         SHUTDOWN
         ENDIF
.enD patch 7.0
. CARD - pretty much all  options.
CARD     clear     taskname
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil ",taskname
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 \\nts0\c\apps\winbatch\butil ",taskname
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil ",taskname
.begin patch 7.41
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil ",taskname
.                    else
                    append    "\\Nins1\Winbatch\butil ",taskname
.                    endif
.end patch 7.41

         else
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil ",taskname
.begin patch 7.41
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil ",taskname
.                    else
                    append    "\\Nins1\Winbatch\butil ",taskname
.                    endif
.end patch 7.41
         endif
         perform   sort of ncards,ncardsa,ncardslo,ncardsn
           APPEND    SHORTNME TO TASKNAME
.           APPEND    " F=default C=",TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    copy,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31
         APPEND    cntprint TO TASKNAME
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
         APPEND    " kf=n",TASKNAME         .do not kill input file
         endif 
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
.START PATCH 6.7 REPLACED LOGIC
.         RESET     TASKNAME
.         compare   c2 to pdfflag             .PDF REquest?
.         if        equal                    .yes
.         APPEND    " PDF=Y",TASKNAME         .
.         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .Both Print and PDF Request?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          APPEND    " CO=",TASKNAME
          APPEND    COMPANY,TASKNAME
        RESET     TASKNAME
.END PATCH 6.7 REPLACED LOGIC
           EXECUTE   TASKNAME
         branch    rhflag of nother,rh
         goto      nother
ncards   append    "job=NCARDS INfile=",TASKNAME
         return
ncardsa  append    "job=NCARDSA INfile=",TASKNAME
         return
ncardSlo  append    "job=NCARDSLO INfile=",TASKNAME
         return
ncardsn  append    "job=NCARDSN INfile=",TASKNAME
         return
.RH - RH style report listing
RH 
         clear     taskname
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRH INfile=",TASKNAME
.begin patch 7.41
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil job=NCARDRH INfile=",TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil job=NCARDRH INfile=",TASKNAME
.                    endif
.end patch 7.41
         else
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRH INfile=",TASKNAME
.begin patch 7.41
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil job=NCARDRH INfile=",TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil job=NCARDRH INfile=",TASKNAME
.                    endif
.end patch 7.41
         endif
.          append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nins1\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRH INfile=",TASKNAME
           APPEND    SHORTNME TO TASKNAME
.           APPEND    " F=default C=",TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    COPY,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31

         APPEND    cntprint TO TASKNAME
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .Print & PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          APPEND    " CO=",TASKNAME
          APPEND    COMPANY,TASKNAME
         RESET     TASKNAME
         display   *p1:24,*el,"Submitting the Job, Please wait"
         pause     "5"
         EXECUTE   TASKNAME
         display   *p1:24,*el,"RH, Submitted"
         goto      nother
.patch7.18
RHREV 
         clear     taskname
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRHU1 INfile=",TASKNAME
           append    "\\Nins1\Winbatch\butil job=NCARDRHU1 INfile=",TASKNAME
         else
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRHU1 INfile=",TASKNAME
           append    "\\Nins1\Winbatch\butil job=NCARDRHU1 INfile=",TASKNAME
         endif
.          append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=NCARDRH INfile=",TASKNAME
           APPEND    SHORTNME TO TASKNAME
.           APPEND    " F=default C=",TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    COPY,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31

         APPEND    cntprint TO TASKNAME
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .Print & PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          append    " CO=",taskname
          append    Company,taskname
         RESET     TASKNAME
         display   *p1:24,*el,"Submitting the Job, Please wait"
         pause     "5"
         EXECUTE   TASKNAME
         display   *p1:24,*el,"RH, Submitted"
         goto      nother
.patch7.18
.RHflat - RH style flat file
RHflat 
         clear     taskname
.begin patch 7.22
.           append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=NRH123 INfile=",TASKNAME
.begin patch 7.32
.           append    "f:\apps\winbatch\butil job=NRH123 INfile=",TASKNAME
           append    "\\Nins1\Winbatch\butil job=NRH123 INfile=",TASKNAME
.end patch 7.32
.end patch 7.22
           APPEND    SHORTNME TO TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    COPY,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31
         APPEND    cntprint TO TASKNAME
          append    " CO=",taskname
          append    Company,taskname
         RESET     TASKNAME
         display   *p1:24,*el,"Submitting the Job, Please wait"
         pause     "5"
         EXECUTE   TASKNAME
         display   *p1:24,*el,"RH, Submitted"
         goto      nother
.CARDVIEW - VIEW BLANKSTOCKS /print RH Style report listing
CARDVIEW
         clear     taskname
.begin patch 7.32
.           append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=cardview INfile=",TASKNAME
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil job=cardview INfile=",TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil job=cardview INfile=",TASKNAME
.                    endif
           APPEND    SHORTNME TO TASKNAME
.end patch 7.32
.           APPEND    " F=default C=",TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    COPY,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
           APPEND    " kf=n",TASKNAME         .do not kill input file
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31

         APPEND    cntprint TO TASKNAME
         move      c2 to rhflag
.         compare   c2 to rhflag             .rh as secondary?
.         if        equal                    .yes
.          APPEND    " kf=n",TASKNAME         .do not kill input file
.         endif 
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .PDF and Print Request?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          APPEND    " CO=",TASKNAME
          APPEND    COMPANY,TASKNAME
         RESET     TASKNAME
           EXECUTE   TASKNAME
         branch    rhflag of nother,rh
         goto      nother

BSTOCK   clear     taskname
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil ",taskname
.                    else
                    append    "\\Nins1\Winbatch\butil ",taskname
.                    endif
         else
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil ",taskname
.                    else
                    append    "\\Nins1\Winbatch\butil ",taskname
.                    endif
         endif
         perform   sort of ncardswa,ncardsw,ncardswo,ncardswn
           APPEND    SHORTNME TO TASKNAME
.           APPEND    " F=default C=",TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    copy,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31

         APPEND    cntprint TO TASKNAME
.         display   *p1:24,*el,user,*w10
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
           APPEND    " kf=n",TASKNAME         .do not kill input file
         endif 
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          APPEND    " CO=",TASKNAME
          APPEND    COMPANY,TASKNAME
         RESET     TASKNAME
           EXECUTE   TASKNAME
         branch    rhflag of nother,rh
         goto      nother
ncardsWA append    "job=NCARDSWA INfile=",TASKNAME
         return
ncardsW  append    "job=NCARDSW INfile=",TASKNAME
         return
ncardsWo append    "job=NCARDSWO INfile=",TASKNAME
         return
ncardsWn append    "job=NCARDSWN INfile=",TASKNAME
         return

SortExcl  pack      taskname from "\\nins1\e\data\dataexcl.dat,\\nins1\e\data\dataexcl.srt;64-138"
          Sort      Taskname
                    sort      taskname,SUNDM="10.10.30.103:502"
                    If        Over
                    pack      str255 from "Sort to dataexcl.srt failed, ",CRLF,"Error: ",S$ERROR$,crlf:
                              "Yes = you fixed, No=try again, Canncel= abort"                 
                    ALERT    PLAIN,str255,RESULT
                               IF       (RESULT = 1)
                               ALERT    NOTE,"YES was pressed.",RESULT
                               ELSEIF   (RESULT = 2)
                               ALERT    NOTE,"NO was pressed.",RESULT
                               Goto     SortEXcl
                               ELSEIF  (RESULT = 3)
                               ALERT   NOTE,"CANCEL was pressed.",RESULT
                               Shutdown "Cls"
                               ENDIF
                     endif

.end patch 7.42
.         clear      taskname
.         append     "f:\public\flag g:\data\" TO TASKNAME
.         APPEND     SHORTNME TO TASKNAME
.         APPEND     ".SRT RWS" TO TASKNAME
.         reset      taskname
.         execute    taskname
         clear      taskname
         CLOCK      VERSION TO STR25
         SCAN       "PLBWIN" IN STR25
         IF          EQUAL
.Begin  patch 7.42 - was still pointing to "F:"
         append     "\\nins1\e\apps\plb\code\PLBWIN.exe \\nins1\e\apps\plb\code\DSINIT",taskname
         ELSE
         append     "\\nins1\e\apps\plb\code\plb386.exe \\nins1\e\apps\plb\code\DSINIT",taskname
         ENDIF
         reset      taskname
         execute    taskname 
         clear      taskname
         CLOCK      VERSION TO STR25
         SCAN       "PLBWIN" IN STR25
         IF          EQUAL
.begin patch 6.33
.begin patch 7.14
                    call                GetWinVer
.         path      exist,"c:\windows"
.;START PATCH 7.02 REPLACED LOGIC
.;         if        not over
.;         append     "c:\command.com /c COPY G:\data\",taskname
.;         else
.;         append     "c:\winnt\system32\cmd.exe /c COPY G:\data\",taskname
.;         endif
.;         if        not over
                    If                  (osflag = C3 | OSflag =C4)   .win9x
         append     "c:\command.com /c COPY ",taskname
         append     NTWKPATH1,taskname
.         else
                    Elseif               (Osflag = C1 | OSFlag = C5) NT or Windows 2000
         append     "c:\winnt\system32\cmd.exe /c COPY ",taskname
         append     NTWKPATH1,taskname
                    Elseif               (Osflag = C6)  Windows xp
         append     "!c:\windows\system32\cmd.exe /c COPY ",taskname
.end patch 7.14
         endif
.END PATCH 7.02 REPLACED LOGIC
.         append     "c:\command.com /c COPY G:\data\",taskname

.end patch 6.33

         else
.START PATCH 7.02 REPLACED LOGIC
.         append     "Y:command.com /c COPY G:\data\",taskname
         append     "Y:command.com /c COPY ",taskname
         append     NTWKPATH1,taskname
.END PATCH 7.02 REPLACED LOGIC
         endif
.patch7.18
         append     NTWKPATH1,taskname
.patch7.18
         append     shortnme to taskname
.START PATCH 7.02 REPLACED LOGIC
.         append     ".CVR/B+G:\data\",taskname
         append     ".CVR/B+",taskname
         append     NTWKPATH1,taskname
.END PATCH 7.02 REPLACED LOGIC
         append     shortnme to taskname
.START PATCH 7.02 REPLACED LOGIC
.         append     ".lst g:\data\",taskname
         append     ".lst ",taskname
         append     NTWKPATH1,taskname
.END PATCH 7.02 REPLACED LOGIC
         append     shortnme to taskname
         append     ".cvr",taskname
         reset      taskname
         execute    taskname
         if         over
         display    *p1:24,*el,"your fax job may have failed",*b,*b,*b,*w10
         endif
         clear      taskname
.begin patch 6.33
.begin patch 7.14
.         path      exist,"c:\windows"
.;START PATCH 7.02 REPLACED LOGIC
.;         if        not over
.;         append     "c:\command.com /c COPY /b G:\data\",taskname
.;         else
.;         append     "c:\winnt\system32\cmd.exe /c COPY /b G:\data\",taskname
.;         endif
.         if        not over
                    call                GetWinVer
                    If                  (Osflag = c3 or Osflag = C4) win9x
         append     "c:\command.com /c COPY /b ",taskname
         append     NTWKPATH1,taskname
.         else
                    Elseif               (osflag = c1 | osflag = C5)  nt or win2000
         append     "c:\winnt\system32\cmd.exe /c COPY /b ",taskname
         append     NTWKPATH1,taskname
                    Elseif               (Osflag = C6)  XP
         append     "c:\windows\system32\cmd.exe /c COPY /b ",taskname
         append     NTWKPATH1,taskname
.end patch 7.14
         endif
.END PATCH 7.02 REPLACED LOGIC
.         append     "c:\command.com /c copy g:\data\",taskname

.end patch 6.33
.         append     "f:\public\nprint g:\data\",taskname
         append     shortnme to taskname
.         append     ".CVR q=facsys s=ns1 nt f=0 ",taskname
.         append     ".CVR q=facsys s=ns1 nt ",taskname
.         append     ".CVR \\nts3\fax",taskname
         append     ".CVR \\NINS2\fax",taskname
         reset      taskname
         execute    taskname         
         if         over
         display    *p1:24,*el,"your fax job may have failed",*b,*b,*b,*w10
         endif
.          APPEND    SHORTNME TO TASKNAME
.          APPEND    " F=default C=",TASKNAME
.          APPEND    copy,TASKNAME
.          APPEND    " B=",TASKNAME
.         append    " " to taskname
.          APPEND    user TO TASKNAME
.         compare   c2 to rhflag             .rh as secondary?
.         if        equal                    .yes
.          APPEND    " N",TASKNAME         .do not kill input file
.         endif 
.         RESET     TASKNAME
.          EXECUTE   TASKN
         compare   c1 to rhflag
         if        equal
         clear     taskname
.START PATCH 7.02 REPLACED LOGIC
.         append    "g:\data\" to taskname
         append    NTWKPATH1 to taskname
.END PATCH 7.02 REPLACED LOGIC
         append    shortnme to taskname
         append    ".CVR" to taskname
         reset     taskname
         erase     taskname
         clear     taskname
.START PATCH 7.02 REPLACED LOGIC
.         append    "g:\data\" to taskname
         append    NTWKPATH1 to taskname
.END PATCH 7.02 REPLACED LOGIC
         append    shortnme to taskname
         append    ".dat" to taskname
         reset     taskname
         erase     taskname
         clear     taskname
.START PATCH 7.02 REPLACED LOGIC
.         append    "g:\data\" to taskname
         append    NTWKPATH1 to taskname
.END PATCH 7.02 REPLACED LOGIC
         append    shortnme to taskname
         append    ".lst" to taskname
         reset     taskname
         erase     taskname
         clear     taskname
.START PATCH 7.02 REPLACED LOGIC
.         append    "g:\data\" to taskname
         append    NTWKPATH1 to taskname
.END PATCH 7.02 REPLACED LOGIC
         append    shortnme to taskname
         append    ".srt" to taskname
         reset     taskname
         erase     taskname
         clear     taskname
.START PATCH 7.02 REPLACED LOGIC
.         append    "g:\data\" to taskname
         append    NTWKPATH1 to taskname
.END PATCH 7.02 REPLACED LOGIC
         append    shortnme to taskname
         append    ".isi" to taskname
         reset     taskname
         erase     taskname
         endif
         branch    rhflag of nother,rh
         goto      nother
.lcard - laser cards
LCARD    clear     taskname
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil.exe ", TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil.exe ", TASKNAME
.                    endif
.           append    "c:\progra~1\lanbatch\batch  #"BB.com butil ",taskname
         perform   sort of lcards,lcardsns,lcardslo,lcardsln
         append    " INfile=",TASKNAME
           APPEND    SHORTNME TO TASKNAME
           APPEND    " C=",TASKNAME
           APPEND    copy,TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
.begin patch 7.31
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch 7.31

         APPEND    cntprint TO TASKNAME
         compare   c2 to rhflag             .rh as secondary?
         if        equal                    .yes
           APPEND    " kf=n",TASKNAME         .do not kill input file
         endif 
.begin patch 7.13
.         compare   c2 to Banner             .Banner page?
              if            (BANNER = c2 and pdfFlag = c1)    .banner selected and not pdf
.         if        equal                    .yes
.end patch 7.13
         APPEND    " BP=Y",TASKNAME         .
         endif 
         compare   c2 to pdfflag             .PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=Y",TASKNAME         .
         endif 
.patch7.15
         compare   c3 to pdfflag             .Print & PDF REquest?
         if        equal                    .yes
         APPEND    " PDF=B",TASKNAME         .
         endif 
.patch7.15
          append    " CO=",taskname
          append    Company,taskname
         RESET     TASKNAME
           EXECUTE   TASKNAME
         if         over
         display    *p1:24,*el,"your Print job may have failed",*b,*b,*b,*w10
         endif
         branch    rhflag of nother,rh
         goto      nother
lcards   append    " job=lcards",taskname
         return                                                
lcardsns append    " job=lcardsns",taskname
         return
lcardslo append    " job=lcardslo",taskname
         return
lcardsln   append    " job=lcardsln",taskname
         return
           IFNZ        PC
PREPCHN  PREPARE   CHNFLE,"CHAINFILE/CHN:W"
         WRITE     CHNFLE,SEQ;"//OPTIONS #"-D-I-O-R#""
         WRITE     CHNFLE,SEQ;"//KEYIN #"SORTING YOUR CARD FILE#""
         WRITE     CHNFLE,SEQ;"ENV :CHAIN;R"
         WRITE     CHNFLE,SEQ;"ENV :CHAIN;I"
         WRITE     CHNFLE,SEQ;"RMSNET"
         WRITE     CHNFLE,SEQ;"FILEPROC"
         WRITE     CHNFLE,SEQ;"DISK00"
         WRITE     CHNFLE,SEQ;"CHAIN_LIB"
         WRITE     CHNFLE,SEQ;" "
         RETURN
           XIF
           IFZ         PC
PREPCHN  PREPARE   CHNFLE,"f:\APPS\BATCH\CHAINFLE.BAT"
         WRITE     CHNFLE,SEQ;"@ECHO OFF"
         WRITE     CHNFLE,SEQ;"ECHO      #"SORTING YOUR CARD FILE#""
         RETURN
           XIF
NOTOK    DISPLAY   *P1:24,*EL,"NOT SUCCESSFUL!!!!!!",*B,*W3,*P1:24,*EL;
         STOP
NONE     MOVE      YES TO STR1          *IF TIME OUT DEFAULT STR1 = 'Y'.
         KEYIN     *B,*P1:22,*EF,*P11:22,"Your Pick-off is completed !":
                   " There were NO records found !":
                  *P22:23,"Please hit enter when acknowledged...",*RV,*T180,*uc,STR1,*lc
         goto     delfile1
DELFILE  TRAP      DELFILE2 GIVING ERROR IF IO
           TRAPCLR   F5
           TRAP      DELFILE IF F5
           KEYIN     *P1:24,*EL,*B,"THIS OPTION WILL DELETE YOUR DATACARD ":
                       "FILE OK ? ",*uc,STR1,*lc,*P1:24,*EL;
           CMATCH    YES TO STR1
         if        equal
         noreturn
         goto      delfile1
         endif
         cmatch    no to str1
         return    if equal
           GOTO      DELFILE 
delfile1 branch    formflag of delfile2,delfile3
         stop
           BRANCH    NEWMFLAG TO DELFILE2
           GOTO      DELFILE3
DELFILE2 DISPLAY   *P1:24,*EL,"DELFILE - DELETING FILE - ",NEWNAME,*W;
          match    b4 to newname
          goto     close if eos
          CLOSE     NEWMST
.          ERASE     NEWNAME
           CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".DAT"
         erase      DELNAME
         DISPLAY   *P1:24,*EL,"FILE DELETED  - ",DELNAME,*W2;
          CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".ISI"
         erase      DELNAME
         DISPLAY   *P1:24,*EL,"FILE DELETED  - ",DELNAME,*W2;
         branch    formflag of close,delfile4
           GOTO      CLOSE
DELFILE3 TRAP      NOFILE IF IO
         MOVE      NEWNAME TO FILENAME
         DISPLAY   *P1:24,*EL,"DELFILE3 - DELETING FILE - ",NEWNAME,*W;
.         OPEN      NEWMST,NEWNAME,EXCLUSIVE
.         CLOSE     NEWMST,DELETE
          CLOSE     NEWMST
.          ERASE     NEWNAME
           CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".DAT"
         erase      DELNAME
         DISPLAY   *P1:24,*EL,"FILE DELETED      - ",DELNAME,*W2;
          CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".ISI"
         erase      DELNAME
         branch    formflag of close,delfile4
DELFILE4 DISPLAY   *P1:24,*EL,"DELFILE3 - DELETING FORMFILE - ",newname,*W;
         CLOSE     formfile,DELETE
           GOTO      CLOSE
CLOSE    STOP
         STOP
NOFILE   DISPLAY   *P1:24,*EL,*B,"The file: ",FILENAME," is not ":
                                 "on-line...",*W9;
         SCAN      "DATA" IN FILENAME
         GOTO      DELFILE IF NOT EQUAL
         STOP
userng   display   *p1:24,*el,*b,*b,*b,"I'm sorry I've lost track of who ":
                   "you are ",nuseuser,*b,*w,*r:
                   *p1:24,*el,"Please leave the program and try again, ":
                   "Don't forget to tell I.S.",*w5
.         CLOCK     PORTNO TO str3
.         move      str3 to Portn
.         CLOCK     PORT TO PORTnum
.         UNPACK    PORTINFO INTO TASK
.         UNPACK    TASK INTO str1,PROC,ID,str4,PORTNUM
.         MOVE      PORTNUM TO PORTN
        clock      port to str3           .plb note
.bytes 1-2 are 0-99
.byte  3   is the hundreds field
        unpack     str3 into str2,str1
        pack       str3 from str1,str2
        move       str3 to portN         
         goto      start                      .dh 
         stop
..........................................................................................................
.list name begins with 'A '
FixitA
          return    
        bump      Mlstname by 2
.START PATCH 7.17 REPLACED LOGIC
.        clear     str55
.        append    mlstname to str55
.        append    "|A" to str55
.        reset     str55
.        clear     Mlstname
.        move      str55 to mlstname
          clear     str75
          append    mlstname to str75
          append    "|A" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
.END PATCH 7.17 REPLACED LOGIC
        return
.
.list name begins with 'The '
FixitThe
          return
        bump      Mlstname by 4
.START PATCH 7.17 REPLACED LOGIC
.        clear     str55
.        append    mlstname to str55
.        append    "|The" to str55
.        reset     str55
.        clear     Mlstname
.        move      str55 to mlstname
          clear     str75
          append    mlstname to str75
          append    "|The" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
.END PATCH 7.17 REPLACED LOGIC
        return
..........................................................................................................

           INCLUDE   NORDIO.INC
           INCLUDE   NOWNIO.inc
.START PATCH 7.19 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
.         include   nbrkIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 7.19 REPLACED LOGIC
           INCLUDE   NDATIO.inc
           INCLUDE   NOFRIO.INC
           INCLUDE   NCATIO.INC
.START PATCH 7.17 ADDED LOGIC
          INCLUDE   NREFIO.INC
.END PATCH 7.17 ADDED LOGIC
         include   mlrhelp.inc
         include   listhelp.inc
         include   brkhelp.inc
         include   nuseio.inc
.begin patch 6.5
.         include    f:\library\develop\hpio.inc
         include    hpio.inc
.end patch 6.5
           INCLUDE   COMLOGIC.INC
         include   ncntio.inc

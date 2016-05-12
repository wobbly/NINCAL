..............................................................................
.
. CONS INCLUSION
.
. DEFINES ALL CONSTANT AND FREQUENTLY USED VARIABLES.
.
.Patch "9.41" DLH 2016 January 6 OSflag bumped to 2 bytes
.Patch "9.4" DLH April 2011 added vars for new comlogic routine Getpdfpath
.Patch "9.3" DLH added vars for new comlogic routine to find ir under client/server
.Patch "9.2" DLH added Time vars for new comlogic routine Waitin
.Patch "9.1" DLH added SysDrive   . maybe should add to common used by comlogic GetSDrive
.Patch "9.0" DLH smtp mail variables REMOVED must be includes seperately for programs that need
.        Include Smtp.PRI  (PRI - Profile Routine Interface)
.patch "8.87" JD   05Feb2008   added New list Exfeelst PL.  
.patch "8.86" DLH  03Oct2007   added New lists to runcodes MMI    & PIDI Virg  
.patch "8.85" JD  2Jul2007   added Doctors without Borders mlr 5023 TO discmlrs.
.patch "8.84" JD  27Feb2007   added New lists to runcodes Frontline,DataMgmt,Mrkt Services
.patch "8.83" DLH 14Aug2006   added New vars for mailsend
.patch "8.82"DMS 7Jun2006   added str20 for fields that grew in length during order conversion
.patch "8.81" JD 3Jul2003   added awf mlr 116 TO discmlrs.
.patch "8.8" DLH 9Jul2002   new vars for comlogic - GetWinVer
.patch "8.7" DLH 29Oct2001   new vars for comlogic
.patch "8.61" JD 03042001 removed wilderness/natl park trust from dismlrs.
.patch "8.6" ASH 03OCT2000 Added vars for Network paths
.patch "8.5" ASH corrected var
.patch "8.4" ASH added standard PRTPAGE fonts
.patch "8.3" ASH added COMMA
.patch 8.2   DLH add some frequent mask variables
.patch "8.1" ASH Added vars for Search.plf/Searchio.inc - Added 2 vars used for GUI programs
.Patch "8.0" DLH smtp mail variables added
. D. Herrick 09Sep98   moved Taskname here
. A. Harkins 09Jul98   Added VARIABLES for GUI Programs:
. J. Duenas  19Mar98   Added EXFEELST
. D. Herrick 17Nov95   Added True
. D. Herrick 10jul95   Added h & v screen positions.
. D. HERRICK 12/17/91  ADDED LWIN & RWIN SUBWINDOW VARS.
. D. HERRICK 08/16/91  ADDED OVER
. D. HERRICK 08/01/91  ADD SOME OF LAKE GROUP COMMON VARS.
. E.W. LAKE  03/13/89  ADD EXCODES LITERAL.
. E.W. LAKE  12/13/88  ADD Z3 VARIABLE.
. E.W. LAKE  08/25/88  ADD SEQEOF VARIABLE.
. E.W. LAKE  02/05/88  ADD TIMEFLAG,HH,MN,SS,C24,C60,C63
. E.W. LAKE  10/12/87  REMOVED B30,B36
. E.W. LAKE  05/28/87  INITIAL RELEASE
.
..............................................................................
.
.LAKE GROUP COMMON VARS. PARTIAL.
.EXIT     FORM      2
.PROGRAM  DIM       20
.COMPNME  DIM       12
.PRTNAME  DIM       25
.INPNAME  DIM       25
.COMMENT  DIM       30
.END
.
SEQ      FORM      "-1"
SEQEOF   FORM      "-3"
.
H        FORM      9
V        FORM      9
.
C0       FORM      "0"
C1       FORM      "1"
C2       FORM      "2"
C3       FORM      "3"
C4       FORM      "4"
C5       FORM      "5"
C6       FORM      "6"
C7       FORM      "7"
C8       FORM      "8"
C9       FORM      "9"
C10      FORM      "10"
C11      FORM      "11"
C13      FORM      "13"
C24      FORM      "24"
C28      FORM      "28"
C29      FORM      "29"
C30      FORM      "30"
C31      FORM      "31"
C45      FORM      "45"
C50      FORM      "50"
.begin patch 8.2
C51      FORM      "51"               .dlh 07/07/08 why value 50??
.C51      FORM      "50"
.end patch 8.2
C59      FORM      "59"
C60      FORM      "60"
C63      FORM      "63"
C80      FORM      "80"
c100     form      "100"
.
STR1     DIM       1
STR2     DIM       2
STR3     DIM       3
STR4     DIM       4
STR5     DIM       5
STR6     DIM       6
STR7     DIM       7
STR8     DIM       8
STR9     DIM       9
STR10    DIM       10
str11    dim       11
STR12    DIM       12
STR13    DIM       13
str14    DIM       14
str15    dIM       15
STR16    DIM       16
STR17    DIM       17
str18    DIM       18
str19    DIM       19
.START PATCH  8.82            ADDED LOGIC
str20          DIM               20
.END PATCH    8.82            ADDED LOGIC
STR24    DIM       24
str25    dim       25
STR29    DIM       29
STR30    DIM       30
STR32    DIM       32
str35    dim       35
str40    dim       40
str45    dim       45
str50    dim       50
str55    dim       55
str75    dim       75
str255    dim       255
.
N1       FORM      1
N2       FORM      2
N3       FORM      3
N4       FORM      4
N5       FORM      5
N6       FORM      6
N7       FORM      7
N8       FORM      8
N9       FORM      9
N10      FORM     10
N11      FORM     11
N32      FORM     3.2
IN9           Form            9                            ;added 18Oct2005 because compute uses N9
.
B1       INIT      " "
B2       INIT      "  "
B3       INIT      "   "
B4       INIT      "    "
B5       INIT      "     "
B6       INIT      "      "
B7       INIT      "       "
B8       INIT      "        "
B9       INIT      "         "
B10      INIT      "          "
B12      INIT      "            "
B15      INIT      "               "
b25      init      "                         "
b55      init      "                                                      "
.
STITLE   DIM       30                  SCREEN TITLE
PTITLE   DIM       36
PF1      DIM       12                  FUNCTION KEY DESCRIPTIONS
PF2      DIM       12
PF3      DIM       12
PF4      DIM       12
PF5      DIM       12
.
ZFILL    INIT      " 0"                ZERO-FILL
YES      INIT      "Y"
NO       INIT      "N"
PERIOD   INIT      "."
.START PATCH 8.3 - ADDED VAR
COMMA    INIT      ","
.END PATCH 8.3 - ADDED VAR
SLASH    INIT      "/"
STAR     INIT      "*"
COLON    INIT      ":"
EQUAL    INIT      "="
AT       INIT      "@"
PRC      INIT      "%"
DASH     INIT      "-"
YORN     INIT      "YN"
REPYN    INIT      "yYnN"
True     INIT      "T"
False    init      "F"
Dquote   init      "#""               ."
.
TOPWIND  FORM      2                   TOP SUB-WINDOWN COORDINATE
BOTWIND  FORM      2                   BOTTOM
LWIND    FORM      2                   LEFT
RWIND    FORM      2                   RIGHT
.
lhcontin form      1                   .list help continue
.
MM       DIM       2                   DATE VARIABLES
DD       DIM       2
YY       DIM       2
CC       form      "20"
NFEB     FORM      "28"
NDD      FORM      2
NMM      FORM      2
NYY      FORM      2
NCC      FORM      2          21oct96 DLH
holdcc   FORM      2          07Apr99 DLH
hold4    FORM      2          07Apr99 DLH
timestamp dim      16         'yyyymmddhhmmssss'      17oct96 DLH
JULDAYS  FORM      5
DATEFLAG FORM      "0"
HH       DIM       2                   TIME VARIABLES
MN       DIM       2
SS       DIM       2
.01 June 2012
Hours     Dim       2
Minutes   Dim       2
.01 June 2012
TIMEFLAG FORM      "0"
.see nordd.inc patch 5
.CANCODES INIT      "XQ"                POSSIBLE CANCEL STATUS CODES
CANCODES init      "XQx"               POSSIBLE CANCEL STATUS CODES
.ASH found problems including Cancelled LCR "z"
.CANCODES init      "XQxz"               POSSIBLE CANCEL STATUS CODES
EXCODES  INIT      "23"                POSSIBLE EXCHANGE CODES
ENTCODES INIT      "13"                POSSIBLE ENTIRE LIST CODES.
.START PATCH 8.87
.EXFEELST INIT      "018710"
.patch 10/21/08 DH
.EXFEELST INIT      "018710-024593"
EXFEELST INIT      "018710-024593-018055"
.end patch 10/21/08 DH
.End PATCH 8.87
.DiscMlrs INIT      "0179-0170-0173-0020-0188-4407"
.DiscMlrs INIT      "0179-0170-0173-0188"
.DiscMlrs INIT      "0179-0170-0173-0188-1282"
..................................................
.THESE WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
.updated 07/2006 DLH
.START PATCH 8.85 - ADDED VAR
.DiscMlrs IN/IT      "0179-0170-0173-0188-1282-0116"
.DiscMlrs INIT      "0170-0173-5023"
.added dccc 2015 july 22
DiscMlrs INIT      "0170-0173-5023-0193-7068"
.End PATCH 8.85 - ADDED VAR
.DiscMlrs INIT      "0170-0173"
DiscMlrsNot INIT      "0179-0188-0116"            .1282 may need to be put in with a different date
..................................................
..19Jan00 add mailer #4407 per GB/SA Dawson acct
..04may01 add mailer #1282 per Gs/SA Dawson acct
.begin patch 8.86
.Begin patch 2013 July 16  was including 017810, missing 017889
.RUNCODES INIT      "005051-009766-014477-016898-016909-018710-021302-022191-023069-022190-022189"      RUNNING CHARGE LIST #'S
RUNCODES INIT      "005051-009766-014477-016898-016909-021302-022191-023069-022190-022189-017889"      RUNNING CHARGE LIST #'S
.                   tdmc,  anacapa,JDM,  Antares,FIDE, DMI,   Frntln,MKTG,   MMI, PIDI, 
.end patch 2013 july 16
.RUNCODES INIT      "005051-009766-014477-016898-016909-018710-021302-022191-023069-022190-022189"      RUNNING CHARGE LIST #'S
.RUNCODES INIT      "005051-009766-014477-016898-016909-018710-021302-022191-023069"      RUNNING CHARGE LIST #'S
.end patch 8.86
.RUNCODES INIT      "005051-009766-014477-016898-016909-018710"      RUNNING CHARGE LIST #'S
.                   TRIPLEX, LPS, JDM.   antares, fide      exch management fee
.                                 15jul92 16mar94, 09/01/95       08may98
uplow    init      "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz"
lowup    init      "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
.WHITNEY  INIT      "3520-4020-4577"
..................................................
.THESE WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
WHITNEY  INIT      "3520-4020-4577"
..................................................
.THESE WILL NEED TO BE UPDATED WHEN CLIENT FILE IS FINISHED
perlowin init      "3317-223-4721-4722"
anacapa  init      "2177-2122-3508-2297"
.FILENAME DIM       12             CHANGED FROM 8 6MAY91 DLH
FILENAME DIM       250             CHANGED FROM 12 19Dec DLH
FileString          Dim       250
FileString1         Dim       250
FileString2         Dim       250
.START PATCH 8.5 REPLACED LOGIC
.pdrive   init      "g:\data\"
.START PATCH 8.6 REPLACED LOGIC
.pdrive   init      "\\NTS0\D\DATA\"
pdrive   init      "\\NinS1\E\DATA\"                                   ."
.END PATCH 8.6 REPLACED LOGIC
.END PATCH 8.5 REPLACED LOGIC
TESTFILE FILE
TEMPFILE FILE
.PRTFILE  DIM       30          .03March2010 changed DH
PRTFILE  DIM       75
PRT      INIT      ".LST"
Z3       INIT      "000"
Question INIT      "?"
OVER     DIM       1
position form      1          .used for rotating dial. 4/29/94 dlh
lstmsw   dim       1           .'Y'=yes list management order
.
UPLH     FORM      2
UPLV     FORM      2
LWRH     FORM      2
LWRV     FORM      2
I218     INIT      0332                   .CORNER TOP LEFT
I196     INIT      0304                   .LINE
I191     INIT      0277                   .CORNER TOP RIGHT
I186     INIT      0272                    .DBL BAR
I187     INIT      0273                   .DBL CORNER TOP RIGHT
I201     INIT      0311                    .DBL CORNER TOP LEFT
I188     INIT      0274                    .DBL CORNER BOTTOM RIFHT
I200     INIT      0310                   .DBL CORNER BOTTOM LEFT
I179     INIT      0263                   .BAR
I192     INIT      0300                   .CORNER BOTTOM LEFT
I217     INIT      0331                   .CORNER BOTTOM RIGHT
I197     INIT      0305                   .BOX LEFT RIGHT
I205     INIT      0315                   .DOUBLE LINE
I193     INIT      0301                   .
message  init      "message"       .used to invoke outlook and send a message
.DH 12 May 2009
.Taskname DIM       200
Taskname DIM       255
.DH 12 May 2009
.note neoy0003.pls used to require taskname of 250
splfile  pfile
..............................................................................
.dh goes bad with windows file menu
FileMenu        MENU
FileMenuData    INIT    "File;":
                        ")Printer Setup...;":
                        ")Print;":
                        "-;":
                        "E)xit"
MenuResult      FORM    1
.
psetupItem      FORM    "1"
printItem       FORM    "2"
PasteItem       FORM    "3"
CloseItem       FORM    "4"
QuitItem        FORM    "6"
.
.Done           INIT    "F"
IsOpen          FORM    "0"  Are we in the open state
..............................................
.GUI VARIABLES
..............................................
.Vars used for IOMssg in COMLOGIC.INC, ErrorMssg has StatText Boxes with
.maximum length OF 31 bytes (ah)- ErrMssg uses Error
Location dim    21
KeyLocation dim 25
LocMssg  dim     31
ErrMssg  dim     31
.Vars used for About Box
.maximum length for Vars in About Box = 50 bytes (ah)
WPrognme  dim   20
WFunction dim   44
WAuthor   dim   20
WRelease  dim   10
WReldate  dim   20
.Vars used for Printing
lncount  form   "    1"
.eightlpi form   "125"
twelvelpi form  "71"
tenlpi  form    "103"
eightlpi form   "135"
sixlpi  form    "167"
row     form    "        1"
prrow   form    "        1"
column  form    9
column1 form    9
column2 form    9
column3 form    9
column4 form    9
column5 form    9
column6 form    9
column7 form    9
column8 form    9
column9 form    9
column10 form    9
column11 form    9
column12 form    9
column13 form    9
column14 form    9
TempLPTR form   9
TempFPTR form   9
.
.begin patch 9.0
.begin patch 8.0
.        Include Smtp.PRI  (PRI - Profile Routine Interface)
.end patch 9.0
Mess    Dim     250
F1      Form    1
NewLine Init    0x7F
.end patch 8.0
CRLF         init        0x0D,0x0A

.BEGIN PATCH 8.1
.VARIABLES ASSOCIATED WITH SEARCH.PLF
SrchFlag form   1
Srch10  dim     10
Srch55  dim     55
SrchKey dim     45
Srchstr dim     110

howmany form    9
howmany2 form   9
howmany3 form   9
howmany4 form   9
result  form    9
INT1    INTEGER 1
.END PATCH 8.1
.begin patch 8.2
MASK32   INIT      "ZZZ.ZZ-"
MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
MASK102   INIT      "Z,ZZZ,ZZZ,ZZZ.ZZ-"
.end patch 8.2
HexZero  INIT   0X0
HexEight integer 4,"4294967295"
.begin patch "8.83"
MailTo    Dim       255       .if multiple comma delimit
MailFrom  Dim       255       .if multiple comma delimit
MailSubjct          Dim       200
mailBody    dim         %32768        .23Mar2007 made global
.MailBody Dim       5000         .size increased to match web logic and Elim dup var 03/15/2007  DH
MailServer          Dim       50                    .comlogic sets to NTS4
MailTimer Form      3
MailType  Form      1         .0=text,1=Html,2=Rich Text,3=text plain iso-8859-1
MailCC    Dim       255       .if multiple comma delimit
MailBcc   Dim       255       .if multiple comma delimit
MailAttach          Dim       500       .if multiple semicolon delimit
MailOkFlag          Dim       1                    .set to Y if ok
.05 aug 2008
MailTrace Dim       255       .if present trace communucation with server
.05 aug 2008
.17 Feb 2012
MailUser  Dim       50
MailPass  Dim       50
MailSSL   Dim       1
MailTTLS  Dim       1           .Dec 2015 28
mailreply dim       50
.17 Feb 2012
.end patch "8.83"
.START PATCH 8.4 ADDED VARS
.Header and Footer Vars
font10  font
font11  font
font12  font
font13  font
        create  font10,"Times New Roman",size=24,bold
        create  font11,"Times New Roman",size=24,italic
        create  font12,"Times New Roman",size=12
        create  font13,"Arial",size=9
.Standard PRTPAGE fonts
.font1   font
.font2   font
.font3   font
.font4   font
.font5   font
.font6   font
..Create fonts to be used
.        create  font1,"Arial",size=12,bold
.        create  font2,"Arial",size=10
.        create  font3,"Helvetica",size=9
.        create  font4,"Fixed",size=10
.        create  font5,"Arial",size=11
.        create  font6,"Arial",size=14
.END PATCH 8.4 ADDED VARS
.moved here 27Apr2007
Reset995Flag   Dim            1                             ;'Y' means we played with pdf99.ini and need to restore

.START PATCH 8.5
NTWKPATH1       INIT    "\\NinS1\E\DATA\"
NTWKPATH2       INIT    "\\NinS1\E\netutils\"                
NTWKPATH3       INIT    "\\NinS1\D\USERS\"        
NTWKPATH4       INIT    "\\NinS1\D\DATA\"
.
NTWKPATH5       INIT    "\\NinS1\E\DATA\"
NTWKPATH6       INIT    "\\NinS1\E\DATA\TEXT\"
NTWKPATH7       INIT    "\\NinS1\E\DATA\INDEX\"                       ."
.END PATCH 8.5
.begin patch 9.1
SysDrive            Dim       2               system drive letter and :    
.end patch 9.1
.begin patch 9.2
WaitTimer Timer
timestamp1 dim  16
time1   form    16
time2   form    16
time3   form    16
.end patch 9.2
.begin patch 8.7
ComlN2          Form    2
ComlN3          Form    3
ComlN4          Form    4
ComlN5          Form    5
.end patch 8.7
Bit64Flag           Dim       1          values - Y or N
.begin patch 8.8
OsFlag        Form            2
.end patch 8.8
PDFPATH             Dim       45
ClntServFlag        Form        1                 .=Yes if PLBclient No if PLBwin  use clock system via comlogic routine TestClient
GlobalVar1          Dim            %30
................................................................
SetPDFFlag          external "PDF995;PDFFlagSet"
SetPDFAuto          external "PDF995;PDFAutoSet"
SetPDFDefaults      external "PDF995;PDFDefaultsSEt"
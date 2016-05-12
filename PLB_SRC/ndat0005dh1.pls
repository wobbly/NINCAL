PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   nmdldd.inc
         INCLUDE   NDATDD.inc
           INCLUDE   NUSGDD.INC
         INCLUDE    HP.INC
         include   nmoddd.inc
          INCLUDE   NTXTDD.INC
          INCLUDE   NSELDD.INC
          Include Compdd.inc
          Include Cntdd.inc   
.begin patch 3.96
              include         NQRCDD.inc
NoteText  Dim       4000
.end patch 3.96
.begin patch 4.0
          INCLUDE   NTXT1DD.INC
.end patch 4.0
.START PATCH 4.01 ADDED LOGIC
          include   nrefdd.inc
          include   nadddd.inc
          include   narrdd.inc
          include   ncatdd.inc
          include   NSLTdd.inc
          include   nsrcdd.inc
.END PATCH 4.01 ADDED LOGIC

Release   Init      "4.01"   ASH   Code takeover from DLH, Patch 4.00
REldate   INit      "2013 July 02"
.Release   Init      "4.00"   DLH   Add excel option to create datacards within the excel file
.REldate   INit      "2013 June xx"
.Release   Init      "3.97"   DLH   Enhance looks of excel output
.REldate   INit      "2013 May 16"
.Release   Init      "3.96"   DLH   add website links to datacards,reco groups, and usage in the excel option
.REldate   INit      "2013 May 7"
.Release   Init      "3.95"   DLH   Website changes
.REldate   INit      "2013 April 29"
.Release   Init      "3.94"   DLH   Fix links to Client/testimonial pages
.REldate   INit      "29 January 2013"
.Release   Init      "3.93"   DLH   changes  on HTML to match new site
.REldate   INit      "09 July 2010"
.  text   #777777 now #2858a6
.         #748EB8 now #FFFFFF
.         #2759A6 Now #FFFFFF
.         #D9EF86 now #2858A6       
.         #E4F4AA now #2858A6       
.See previous release for other changes
.RELEASE  INIT      "1.0"           07/08/83   DLH & DSGEN
NAME     DIM       19 (FILE NAME)
INPUT    FILE      VAR=181 (INPUT  FILE)
OUTPUT   FILE
HTMLFILE FIle
REPLY    DIM       1
LINEs    FORM      2           LINE COUNTER
PAGE     FORM      3           PAGE NUMBER
LR       FORM      "0"         '1'=LAST RECORD PENDING
LRTOT    FORM      7          GRAND TOTAL
ANS      DIM       1
MO       DIM       2       MONTH
DAY      DIM       2
YR       DIM       2
DATE     DIM       8
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.......................
LINE1    DIM       150
LINE1A   DIM       5
LINE1B   DIM       10
LINE2    DIM       150
LINE2A   DIM       5
LINE2B   DIM       10
LINE3    DIM       150
LINE3A   DIM       5
LINE3B   DIM       10
LINE4    DIM       150
LINE4A   DIM       5
LINE4B   DIM       10
LINE5    DIM       150
LINE5A   DIM       5
LINE5B   DIM       10

FileCheck FIle
trapcount form      4

TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
PRSW     FORM      "0"
PSTATUS  DIM       6
BLANK127 DIM       127
ONE      FORM      "1"
COUNT    FORM      "00000"
EXSW     DIM       1           * "Y" = DO NOT PRINT WITHDRAWN CARDS.
SW1P     INIT      "Y"         * "Y" = 1ST PASS.
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
UPDFLAG  FORM      1
XLSXFlag FORM      1
DUPLFLAG FORM      1
LASRFLAG FORM      1
WITHFLAG FORM      1
NINPPATH FORM      1
TypEFlag  Form      1
htmlflag form      1
html30flag form      1
HTMLPTR  FORM      2
HTMLALPHA INIT     "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
PTRFLAG  FORM      1              0=IGNORE 1=WRITE BOOKMARK LINKS
markflag form      1
BOOKMARK DIM       1
NUMBER   DIM       10
PRICE    DIM       8
LDESC    DIM       40
BEGIN    FORM      2
LAST     FORM      2
DateJulian     form           5
MASK13    INIT      "Z,ZZZ,ZZZ,ZZZ"
DIM13a    DIM       13
dim9a     dim       13
str51a    DIM                  5
str52a    DIM                  5
str53a    DIM                  5
str54a    DIM                  5
str55a    DIM                  5
str31a    DIM                  5
str32a    DIM                  5
str33a    DIM                  5
str34a    DIM                  5
str35a    DIM                  5
TableOpen FORM 1

*............................................................
.
EXCL     DIM       4                    EXCLUSIVE PRINT FIELD
TOTUNIV  FORM      10
UNIMASK  INIT      "Z,ZZZ,ZZZ,ZZ9"
UNIVPRT  DIM       13
TEXT     DIM       46
LIN48     DIM       46
b46      init      "                                              "
startfp   form     4
endfp     form     4
sortvar   dim      300
HTMLINTRO DIM       2         1= ,2= All managed lists, 3= managed lists updated in the last 30 days 
.............................................................................................
.some excel goodies
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft                        integer 4,"0xffffefDD"
xlTop                         integer 4,"0xffffefc0"
.begin patch 3.97
AlignRight                    integer 4,"0xffffefc8"
.end patch 3.97
xlAlignCenter                 integer 4,"0xffffeff4"
xlBottom                      integer      4,"0xffffeff5"
XlLineStyleDBl                Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidthA variant
xlColumnWidthB variant
xlColumnWidthCats variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant
.begin patch 4.0
HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_I4                         EQU                  3           .4 byte integer
Zoom70                        VARIANT
ZoomRowMax            form    9      //Max row before a soft page break
ZoomRowMaxPage1   form        9      //Max row before a soft page break on page 1
CellRowCnt          FORM      "46"
CellRowCnt1         FORM      "54"
CurCellNum FORM 5
SheetIndex                              variant
sheetcount          integer   1
Index     Form      5
.hold2         dim             7500           .length of largest possible text record ---MIN
hold2         dim             20000           .length of largest possible text record ---MIN
.end patch 4.0

Row1          form            5
Row2          Form            5
DimRow1       Dim             5
DimRow2       Dim             5
ExRange1      Dim             7
ExRange2      Dim             7
ExRange3      Dim            16
.START PATCH 4.01 REPLACED LOGIC - RENAMED FOR EASIER SEARCHING...
.range1    automation
xlrange1    automation
.END PATCH 4.01 REPLACED LOGIC - RENAMED FOR EASIER SEARCHING...
NFULCOMP  DIM       55
.START PATCH 4.01 ADDED LOGIC
XLSCreateFlag       form      1
WorksheetName       DIM       31
Linefeed  init      0x0A
DimPtr              Dim       ^
DimPtr2             Dim       ^
RentFlag  Dim       1         .'Y' if anypart of list is rental
SelectListView      listview
Select2ListView     listview
#result   FORM      9
num9      FORM      9
SELNOTESFLAG        INIT      "N"
leftcol             form      9
rightcol  form      9
PreviousLink        dim       1000
NextLink  dim       1000
SelTextFlag         form      9
NINGreen  color
colornum  form      24
.END PATCH 4.01 ADDED LOGIC

+.........................................................................
.
. DEFAULT OPTIONS ARE : NO update info.
.                       NO WITHDRAWN CARDS
.
. COMMENT CAN MODIFY THE DEFAULTS:  UPDATE :  print updated date
.                                   WITHDRN:  PRINT WITHDRAWN CARDS.
.                                   LOTUS  :  OUTPUT TO FLAT FILE
.
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................

.
+............................................................
.
. FILE OPENING SEQUENCE
         TRAP      F5 giving error IF F5
         TRAP      io giving error IF io
.
.START PATCH 4.01 HARDCODING!!!
.         MOVE      "NDAT0005",PROGRAM
.         MOVE      "1", COMPANY
.         MOVE      "CREQUES",USER
.         MOVE      "DATA15.DAT",INPNAME
.         MOVE      "DATA15",PRTNAME
.         MOVE      "EXCEL",COMMENT
.         ALERT     NOTE,"Hard-coded vars!!",result
.END PATCH 4.01 HARDCODING!!!

.START PATCH 4.01 ADDED LOGIC
          CREATE    selectlistview=1:50:1:50,SORTHEADER=1,SORTORDER=1
          Selectlistview.INSERTCOLUMN  USING "Index",30,0
          Selectlistview.INSERTCOLUMN  USING "Select Num",30,1
          Selectlistview.INSERTCOLUMN  USING "ListNum",100,2
          ACTIVATE selectlistview
.
          CREATE    select2listview=1:50:50:200,SORTHEADER=1,SORTORDER=1
          Select2listview.INSERTCOLUMN  USING "Index",30,0
          Select2listview.INSERTCOLUMN  USING "Select Num",30,1
          Select2listview.INSERTCOLUMN  USING "ListNum",100,2
          ACTIVATE select2listview
          setprop   SelectListView,visible=c0
          setprop   Select2ListView,visible=c0
          create    NINGreen=222:234:162
.END PATCH 4.01 ADDED LOGIC

         rep       "nNdDaAtT" in program
         MATCH     "NDAT0005" TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        not equal                 .NO
         MOVE      "NDAT0005" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "LOCAL" TO PRTNAME
         CLEAR     COMMENT
         ENDIF

         MOVE      "RH STYLE DATACARD print" TO STITLE
         MOVE      "EXIT" TO PF5
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         MOVE      C0 TO UPDFLAG
         CALL      PAINT
         CALL      FUNCDISP

BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET   MOVE      C0 TO UPDFLAG
         MOVE      C1 TO XLSXFlag
         MOVE      C0 TO DUPLFLAG
         RESET     COMMENT
         KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                   *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE FLAT FILE":
                   *P20:13,"LASER  :  PRINT ON LASER":
                   *P20:14,"duplex :  PRINT ON LASER duplex":
                   *P20:15,"HTML   :  produce code for web":
                   *P20:16,"HTML30 :  upd exclusives for web":
                   *P20:17,"EXCEL  :  XLS SUMMARY & DETAIL":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO UPDFLAG
         MOVE      C0 TO WITHFLAG
         MOVE      C1 TO XLSXFlag
         MOVE      C0 TO LASRFLAG
         MOVE      C0 TO DUPLFLAG
         MOVE      C0 TO HTMLFLAG
         MOVE      C0 TO HTML30FLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P20:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "UPDATE" IN COMMENT
         CALL      OPTUPD IF EQUAL
         RESET     COMMENT
         SCAN      "WITHDRN" IN COMMENT
         CALL      OPTWITH IF EQUAL
         RESET     COMMENT
         SCAN      "LASER" IN COMMENT
         CALL      OPTLASER IF EQUAL
         RESET     COMMENT
         SCAN      "DUPLEX" IN COMMENT
         CALL      OPTdupl IF EQUAL
         RESET     COMMENT
         SCAN      "LOTUS" IN COMMENT
         CALL      OPTLOTUS IF EQUAL
         RESET     COMMENT
.begin patch 4.0
         SCAN      "EXCEL" IN COMMENT
         CALL      OPTXLS IF EQUAL
         RESET     COMMENT
.end patch 4.0
         SCAN      "HTML1" IN COMMENT
         CALL      OPTHTML IF EQUAL
         RESET     COMMENT
         SCAN      "HTML" IN COMMENT
         CALL      OPTHTML IF EQUAL
         RESET     COMMENT
         SCAN      "HTML30" IN COMMENT
         CALL      OPTHTML30 IF EQUAL
         RESET     COMMENT
         SCAN      "NOTYP" IN COMMENT
         CALL      OPTTYPE IF EQUAL

         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                   *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE Excel Summary":
                   *P20:13,"LASER  :  PRINT ON LASER":
                   *P20:14,"DUPLEX :  PRINT ON LASER Duplex":
                   *P20:15,"HTML   :  produce code for web":
.begin patch 3.2
                   *P20:16,"HTML30 :  upd exclusives for web":
.end patch 3.2
.begin patch 4.0
                   *P20:17,"EXCEL  :  Excel Summary with Datacarsd":
.end patch 4.0
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO UPDFLAG
         MOVE      C1 TO WITHFLAG
         MOVE      C1 TO XLSXFlag
         MOVE      C2 TO LASRFLAG
         MOVE      C1 TO DUPLFLAG
.begin patch 3.0
         MOVE      C1 TO HTMLFLAG
.end patch 3.0
         GOTO      INPGET
OPTUPD   MOVE      C2 TO UPDFLAG
         move      c2 to lasrflag
         move      c2 to duplflag
         move      c1 to XLSXFlag
         RETURN
OPTWITH  MOVE      C2 TO WITHFLAG
         RETURN
OPTLOTUS MOVE      C2 TO XLSXFlag
         RETURN
.begin patch 4.0
OPTXLS   MOVE      C3 TO XLSXFLAG
.START PATCH 4.01 ADDED LOGIC
          move      C1,FUNC
.END PATCH 4.01 ADDED LOGIC
         RETURN
.end patch 4.0
OPTLASER MOVE      C2 TO LASRFLAG
         RETURN
OPTdupl  MOVE      C2 TO duplFLAG
         RETURN
.begin patch 3.0
OPTHTML  MOVE      C2 TO HTMLFLAG
         MOVE      C1 TO HTMLPTR           .SET POINTER
.Patch 3.9 Logic Updated
                              move inits to HTMLINTRO
.Patch 3.9
         RETURN
.end patch 3.0
.begin patch 3.2
OPTHTML30      MOVE      C2 TO HTML30FLAG
               MOVE      C2 TO HTMLFLAG
               MOVE      C1 TO HTMLPTR           .SET POINTER
.Patch 3.9 Logic Updated
                                                  move inits to HTMLINTRO
.Patch 3.9
               RETURN
.end patch 3.2
OPTTYPE
          MOve      C2,TYPeFLAG
          return
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         TRAP      io giving error IF io
         CLOSE     TESTFILE
         MOVE      C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
.begin patch 4.0
          MOve            "11",Row1
          move            "12",row2
          MOve      c1,Index
.end patch 4.0

.begin patch 3.0
         MOVE      INPNAME TO NAME
         if        (htmlflag = 2)
         clear      name
         append     inpname to name
         append     ".srt",name
         reset      name
         clear     sortvar
         append     "c:\work\",sortvar
         append     inpname,sortvar
         append     ".dat",sortvar
         append     ",",sortvar
         append     "c:\work\",sortvar
         append     name,sortvar
         append     ";64-138",sortvar
.         append     ";76-130",sortvar
         reset      sortvar
         sort      sortvar
        IF OVER
                DISPLAY *N,"Sort ERROR ",S$ERROR$;
        ENDIF

         endif
.end patch 3.0

         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET
.begin patch 3.0
         if        (htmlflag <> 2)
         MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
.begin patch 4.0
.         BRANCH    XLSXFlag TO PRTOK,OUTPUT
         BRANCH    XLSXFlag TO PRTOK,OUTPUT,PreStart
.end patch 4.0
         endif
PRTOK
.begin patch 4.0
.         if        (htmlflag <> 2)
         if        (htmlflag <> 2 & xlsxflag < 2)
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
.         else
         elseif (xlsxflag < 2)
         clear     str25
         append    "c:\work\" to str25
         append    outname to str25
         append    ".html" to str25
         reset     str25
         prepare   htmlfile,str25
         if (HTMLINTRO = "1")
.beginning DH goes bad July 13th 2010
            write     htmlfile,seq;"<!DOCTYPE html PUBLIC #"-//W3C//DTD XHTML 1.0 Transitional//EN#" #"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd#">"
            write     htmlfile,seq;"<html xmlns=#"http://www.w3.org/1999/xhtml#" dir=#"ltr#" lang=#"en-US#">"
            write     htmlfile,seq;"<head profile=#"http://gmpg.org/xfn/11#">"
            write     htmlfile,seq;"<meta http-equiv=#"Content-Type#" content=#"text/html; charset=UTF-8#" />"
            write     htmlfile,seq;"<title>  Data Card Library - Names in the News</title>"
            write     htmlfile,seq;"<meta name=#"generator#" content=#"WordPress 1436#" />"
            write     htmlfile,seq;"<!-- leave this for stats -->"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News &raquo; Data Card Library Comments Feed#" href=#"http://www.nincal.com/index.php/list-management/data-card-library/feed/#" />"
            write     htmlfile,seq;"<link rel='stylesheet' id='A2A_SHARE_SAVE-css'  href='http://www.nincal.com/wp-content/plugins/add-to-any/addtoany.min.css?ver=1.0' type='text/css' media='' />"
            write     htmlfile,seq;"<link rel='stylesheet' id='contact-form-7-css'  href='http://www.nincal.com/wp-content/plugins/contact-form-7/styles.css?ver=2.3' type='text/css' media='all' />"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-includes/js/jquery/jquery.js?ver=1.3.2'></script>"
            write     htmlfile,seq;"<link rel=#"wlwmanifest#" type=#"application/wlwmanifest+xml#" href=#"http://www.nincal.com/wp-includes/wlwmanifest.xml#" />"
            write     htmlfile,seq;"<link rel='index' title='Names in the News' href='http://www.nincal.com' />"
            write     htmlfile,seq;"<link rel='up' title='List Management' href='http://www.nincal.com/index.php/list-management/' />"
            write     htmlfile,seq;"<link rel='canonical' href='http://www.nincal.com/index.php/list-management/data-card-library/' />"
            write     htmlfile,seq;"<!--[if IE]>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;"ul.addtoany_list a img{filter:alpha(opacity=70)}"
            write     htmlfile,seq;"ul.addtoany_list a:hover img,ul.addtoany_list a.addtoany_share_save img{filter:alpha(opacity=100)}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<![endif]-->"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/jquery-1.3.2.min.js#"></script>"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/slider.js#"></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/stylesheet.css#" type=#"text/css#" /><link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/calendar.css#" type=#"text/css#" />      <script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/mm-forms.js'></script>"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/calendar.js'></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/nav.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/plugins.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/template-style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/print.css#" type=#"text/css#" media=#"print#" />"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/js/dropdowns.js#"></script>"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News RSS Feed#" href=#"http://www.nincal.com/index.php/feed/#" />"
            write     htmlfile,seq;"<link rel=#"pingback#" href=#"http://www.nincal.com/xmlrpc.php#" />"
            write     htmlfile,seq;"</head>"
            write     htmlfile,seq;"<body>"
            write     htmlfile,seq;"<div id=#"page#" class=#"clearfloat#">"
            write     htmlfile,seq;"<div class=#"clearfloat#">"
            write     htmlfile,seq;"<div id=#"branding#" class=#"left#" onclick=#"location.href='http://www.nincal.com';#" style=#"cursor: pointer;#">"
            write     htmlfile,seq;"<div class=#"blogtitle#" ><a href=#"http://www.nincal.com/#">"
            write     htmlfile,seq;"</a></div>"
            write     htmlfile,seq;"<div class=#"description#">"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<div class=#"right#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META http-equiv=Content-Type content=#"text/html; charset=iso-8859-1#">"
            write     htmlfile,seq;"<LINK href=#"nin_styles.css#" type=text/css rel=stylesheet>"
            write     htmlfile,seq;"<SCRIPT language=JavaScript type=text/JavaScript>"
            write     htmlfile,seq;"<!--"
            write     htmlfile,seq;"function MM_preloadImages() { //v3.0"
            write     htmlfile,seq;"var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();"
            write     htmlfile,seq;"var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)"
            write     htmlfile,seq;"if (a[i].indexOf(#"###")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImgRestore() { //v3.0"
            write     htmlfile,seq;"var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_findObj(n, d) { //v4.01"
            write     htmlfile,seq;"var p,i,x;  if(!d) d=document; if((p=n.indexOf(#"?#"))>0&&parent.frames.length) {"
            write     htmlfile,seq;"d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}"
            write     htmlfile,seq;"if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];"
            write     htmlfile,seq;"for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);"
            write     htmlfile,seq;"if(!x && d.getElementById) x=d.getElementById(n); return x;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImage() { //v3.0"
            write     htmlfile,seq;"var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)"
            write     htmlfile,seq;"if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"//-->"
            write     htmlfile,seq;"</SCRIPT>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META content=#"Microsoft FrontPage 4.0#" name=GENERATOR>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;".style1 {"
            write     htmlfile,seq;"font-size: x-small;"
            write     htmlfile,seq;"font-weight: bold;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<meta name=#"verify-v1#" content=#"6BHMx2UPpCsHKyuB/beiUpzV9EMeXslsxUB+jo2et8A=#" >"
            write     htmlfile,seq;"</HEAD>"
            write     htmlfile,seq;"<Table cellSpacing=0 cellPadding=0 align=right border=0 id=#"table4#">"
            write     htmlfile,seq;"<Caption><b><font color=#"#000000#" face=#"Arial#" size=#"2#">Client Login:</font></b></Caption>"
            write     htmlfile,seq;"<form name='LoginForm' method='post' action='http://www.nincal.com/plb-bin/login.plc'>"
            write     htmlfile,seq;"<input type='hidden' name='function' value='1' />"
            write     htmlfile,seq;"<input type='hidden' name='SID' value='' />"
            write     htmlfile,seq;"<input type='hidden' name='cgibroker' value='' />"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td align=right width=#"150#">"
            write     htmlfile,seq;"<b><font color=#"#000000#" face=#"Arial#" size=#"2#">Username&nbsp;</font></b></td>"
            write     htmlfile,seq;"<td width=#"85#">"
            write     htmlfile,seq;"<input name='cgiusername' value='' size=#"11#" align=right>"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<td width=50>&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<TD align=right>"
            write     htmlfile,seq;"<b>"
            write     htmlfile,seq;"<font color=#"#000000#" face=#"Arial#" size=#"2#">Password&nbsp;</font></b></TD>"
            write     htmlfile,seq;"<TD width=#"85#">"
            write     htmlfile,seq;"<input type='password' name='cgipassword' size=#"11#"align=right value='' /> </TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td>"
            write     htmlfile,seq;"<a href=#"http://www.nincal.com/plb-bin/passrequest.plc#"><b><font size=#"1#" face=#"Arial#">Forgot"
            write     htmlfile,seq;"Your Password?</font></b></a>&nbsp&nbsp&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<TD width=#"85#" align=center valign=center>"
            write     htmlfile,seq;"<input type=image img border=#"0#" src=#"/images/btn_logon.gif#" width=#"38#" height=#"19#" name=#"I1#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"<TD>"
            write     htmlfile,seq;"<A href=#"http://www.nincal.com/plb-bin/registration.plc#"><font size=#"1#" face=#"Arial#">Register</font></a>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"</form>"
            write     htmlfile,seq;"</Table>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<ul id=#"nav#" class=#"clearfloat#">"
            write     htmlfile,seq;"<li><a href=#"http://www.nincal.com/#" class=#"on#">HOME</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-203#"><a href=#"http://www.nincal.com/index.php/about/#" title=#"About Us#">About Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-205#"><a href=#"http://www.nincal.com/index.php/about/team/#" title=#"Our Team#">Our Team</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-207#"><a href=#"http://www.nincal.com/index.php/list-brokerage/#" title=#"List Brokerage#">List Brokerage</a>"
            write     htmlfile,seq;"<ul>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-209#"><a href=#"http://www.nincal.com/index.php/list-brokerage/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-211#"><a href=#"http://www.nincal.com/index.php/list-brokerage/order-status/#" title=#"Order Status#">Order Status</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-213#"><a href=#"http://www.nincal.com/index.php/list-brokerage/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-6 current_page_ancestor current_page_parent#"><a href=#"http://www.nincal.com/index.php/Featured/#" title=#"List Management#">List Management</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-156 current_page_item#"><a href=#"http://www.nincal.COM/DATACARDS/ExcelLists.html#" title=#"Data Card Library#">Data Card Library</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-158#"><a href=#"http://www.nincal.com/index.php/list-management/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<li class=#"page_item page-item-168#"><a href=#"http://www.nincal.com/index.php/list-management/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"

.            write     htmlfile,seq;"<li class=#"page_item page-item-158#"><a href=#"http://www.nincal.com/index.php/list-brokerage/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-159#"><a href=#"http://www.nincal.com/index.php/category/featured/#" title=#"Featured Lists#">Featured Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-166#"><a href=#"http://www.nincal.com/index.php/list-brokerage/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-444#"><a href=#"http://www.nincal.COM/DATACARDS/animalwelfare.htm#" title=#"Animal Welfare#">Animal Welfare</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-447#"><a href=#"http://www.nincal.COM/DATACARDS/catalog.htm#" title=#"Catalog#">Catalog</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-449#"><a href=#"http://www.nincal.COM/DATACARDS/charitable.htm#" title=#"Charitable#">Charitable</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-452#"><a href=#"http://www.nincal.COM/DATACARDS/children.htm#" title=#"Children#">Children</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-454#"><a href=#"http://www.nincal.COM/DATACARDS/civilrights.htm#" title=#"Civil / Human Rights#">Civil / Human Rights</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-457#"><a href=#"http://www.nincal.COM/DATACARDS/cultural.htmm#" title=#"Culture / Arts#">Culture / Arts</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-460#"><a href=#"http://www.nincal.COM/DATACARDS/environmental.htm#" title=#"Environmental#">Environmental</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-462#"><a href=#"http://www.nincal.COM/DATACARDS/foodbanks-missions.htm#" title=#"Food Banks / Missions#">Food Banks / Missions</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-464#"><a href=#"http://www.nincal.COM/DATACARDS/health.htm#" title=#"Health / Disease#">Health / Disease</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-466#"><a href=#"http://www.nincal.COM/DATACARDS/jewish.htm#" title=#"Jewish#">Jewish</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-468#"><a href=#"http://www.nincal.COM/DATACARDS/nativeAmerican.htm#" title=#"Native American#">Native American</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-470#"><a href=#"http://www.nincal.COM/DATACARDS/outdoor.htm#" title=#"Outdoor Activities#">Outdoor Activities</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-472#"><a href=#"http://www.nincal.COM/DATACARDS/political.htm#" title=#"Political#">Political</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-476#"><a href=#"http://www.nincal.COM/DATACARDS/publictv.htm#" title=#"Public TV / Radio#">Public TV / Radio</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-474#"><a href=#"http://www.nincal.COM/DATACARDS/publications.htm#" title=#"Publications / Publishing#">Publications / Publishing</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-478#"><a href=#"http://www.nincal.COM/DATACARDS/womensRights.htm#" title=#"Women&##8217;s Rights#">Women&##8217;s Rights</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-264#"><a href=#"http://www.nincal.com/index.php/servicestools/#" title=#"Services/Tools#">Services/Tools</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-297#"><a href=#"http://www.nincal.com/index.php/servicestools/acquisition-advisor/#" title=#"Acquisition Advisor#">Acquisition Advisor</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-239#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs-advanced/#" title=#"FAQs &##8211; Advanced#">FAQs &##8211; Advanced</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-44#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs/#" title=#"FAQs &##8211; The Basics#">FAQs &##8211; The Basics</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
.2013 April 29 DLH
.            write     htmlfile,seq;"<li class=#"page_item page-item-272#"><a href=#"http://www.nincal.com/index.php/multi-channel-marketing/#" title=#"Multi-Channel Marketing#">Multi-Channel Marketing</a></li>"
.2013 April 29 DLH
.            write     htmlfile,seq;"<li class=#"page_item page-item-12#"><a href=#"http://www.nincal.com/index.php/clients/#" title=#"Clients/Testimonials#">Clients/Testimonials</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-274#"><a href=#"http://www.nincal.com/index.php/clientstestimonials/#" title=#"Clients/Testimonials#">Clients/Testimonials</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-276#"><a href=#"http://www.nincal.com/index.php/contact-us/#" title=#"Contact Us#">Contact Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-202#"><a href=#"http://www.nincal.com/index.php/contact-us/email-us/#" title=#"Email Us#">Email Us</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div id=#"content#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div class=#"post#" id=#"post-156#">"
            write     htmlfile,seq;"<h2>Data Card Library</h2>"
            write     htmlfile,seq;"<div class=#"entry#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<p>Below is an alphabetical listing of all our managed properties. You can     narrow your search by viewing <a href=#"http://www.ninlists.com/DATACARDS/exclupdates.html#">lists that have been     updated in the past 30 days</a>, or visiting our <a href=#"http://www.http://www.namesinthenews.com/index.php/list-management/recommended-lists/#">Quick     Recos</a> for recommendations by market.</p>"
            write     htmlfile,seq;"<p>A link to &##8220;Usage&##8221; will be displayed    for all organizations where 12-month continuation usage is available.</p>"
            write     htmlfile,seq;"<dl>"
            write     htmlfile,seq;"<dd>"
.begin Patch 3.95
.            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"525#">"
.end Patch 3.95
            write     htmlfile,seq;"<tbody>"
            write     htmlfile,seq;"<tr>"
            write     htmlfile,seq;"<td width=#"425#"><strong>List Name</strong></td>"
            write     htmlfile,seq;"</tr>"
         elseif (HTMLINTRO = "2")
.beginning DH goes bad July 13th 2010
            write     htmlfile,seq;"<!DOCTYPE html PUBLIC #"-//W3C//DTD XHTML 1.0 Transitional//EN#" #"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd#">"
            write     htmlfile,seq;"<html xmlns=#"http://www.w3.org/1999/xhtml#" dir=#"ltr#" lang=#"en-US#">"
            write     htmlfile,seq;"<head profile=#"http://gmpg.org/xfn/11#">"
            write     htmlfile,seq;"<meta http-equiv=#"Content-Type#" content=#"text/html; charset=UTF-8#" />"
            write     htmlfile,seq;"<title>  Data Card Library - Names in the News</title>"
            write     htmlfile,seq;"<meta name=#"generator#" content=#"WordPress 1436#" />"
            write     htmlfile,seq;"<!-- leave this for stats -->"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News &raquo; Data Card Library Comments Feed#" href=#"http://www.nincal.com/index.php/list-management/data-card-library/feed/#" />"
            write     htmlfile,seq;"<link rel='stylesheet' id='A2A_SHARE_SAVE-css'  href='http://www.nincal.com/wp-content/plugins/add-to-any/addtoany.min.css?ver=1.0' type='text/css' media='' />"
            write     htmlfile,seq;"<link rel='stylesheet' id='contact-form-7-css'  href='http://www.nincal.com/wp-content/plugins/contact-form-7/styles.css?ver=2.3' type='text/css' media='all' />"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-includes/js/jquery/jquery.js?ver=1.3.2'></script>"
            write     htmlfile,seq;"<link rel=#"wlwmanifest#" type=#"application/wlwmanifest+xml#" href=#"http://www.nincal.com/wp-includes/wlwmanifest.xml#" />"
            write     htmlfile,seq;"<link rel='index' title='Names in the News' href='http://www.nincal.com' />"
            write     htmlfile,seq;"<link rel='up' title='List Management' href='http://www.nincal.com/index.php/list-management/' />"
            write     htmlfile,seq;"<link rel='canonical' href='http://www.nincal.com/index.php/list-management/data-card-library/' />"
            write     htmlfile,seq;"<!--[if IE]>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;"ul.addtoany_list a img{filter:alpha(opacity=70)}"
            write     htmlfile,seq;"ul.addtoany_list a:hover img,ul.addtoany_list a.addtoany_share_save img{filter:alpha(opacity=100)}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<![endif]-->"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/jquery-1.3.2.min.js#"></script>"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/slider.js#"></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/stylesheet.css#" type=#"text/css#" /><link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/calendar.css#" type=#"text/css#" />      <script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/mm-forms.js'></script>"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/calendar.js'></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/nav.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/plugins.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/template-style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/print.css#" type=#"text/css#" media=#"print#" />"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/js/dropdowns.js#"></script>"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News RSS Feed#" href=#"http://www.nincal.com/index.php/feed/#" />"
            write     htmlfile,seq;"<link rel=#"pingback#" href=#"http://www.nincal.com/xmlrpc.php#" />"
            write     htmlfile,seq;"</head>"
            write     htmlfile,seq;"<body>"
            write     htmlfile,seq;"<div id=#"page#" class=#"clearfloat#">"
            write     htmlfile,seq;"<div class=#"clearfloat#">"
            write     htmlfile,seq;"<div id=#"branding#" class=#"left#" onclick=#"location.href='http://www.nincal.com';#" style=#"cursor: pointer;#">"
            write     htmlfile,seq;"<div class=#"blogtitle#" ><a href=#"http://www.nincal.com/#">"
            write     htmlfile,seq;"</a></div>"
            write     htmlfile,seq;"<div class=#"description#">"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<div class=#"right#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META http-equiv=Content-Type content=#"text/html; charset=iso-8859-1#">"
            write     htmlfile,seq;"<LINK href=#"nin_styles.css#" type=text/css rel=stylesheet>"
            write     htmlfile,seq;"<SCRIPT language=JavaScript type=text/JavaScript>"
            write     htmlfile,seq;"<!--"
            write     htmlfile,seq;"function MM_preloadImages() { //v3.0"
            write     htmlfile,seq;"var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();"
            write     htmlfile,seq;"var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)"
            write     htmlfile,seq;"if (a[i].indexOf(#"###")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImgRestore() { //v3.0"
            write     htmlfile,seq;"var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_findObj(n, d) { //v4.01"
            write     htmlfile,seq;"var p,i,x;  if(!d) d=document; if((p=n.indexOf(#"?#"))>0&&parent.frames.length) {"
            write     htmlfile,seq;"d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}"
            write     htmlfile,seq;"if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];"
            write     htmlfile,seq;"for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);"
            write     htmlfile,seq;"if(!x && d.getElementById) x=d.getElementById(n); return x;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImage() { //v3.0"
            write     htmlfile,seq;"var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)"
            write     htmlfile,seq;"if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"//-->"
            write     htmlfile,seq;"</SCRIPT>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META content=#"Microsoft FrontPage 4.0#" name=GENERATOR>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;".style1 {"
            write     htmlfile,seq;"font-size: x-small;"
            write     htmlfile,seq;"font-weight: bold;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<meta name=#"verify-v1#" content=#"6BHMx2UPpCsHKyuB/beiUpzV9EMeXslsxUB+jo2et8A=#" >"
            write     htmlfile,seq;"</HEAD>"
            write     htmlfile,seq;"<Table cellSpacing=0 cellPadding=0 align=right border=0 id=#"table4#">"
            write     htmlfile,seq;"<Caption><b><font color=#"#000000#" face=#"Arial#" size=#"2#">Client Login:</font></b></Caption>"
            write     htmlfile,seq;"<form name='LoginForm' method='post' action='http://www.nincal.com/plb-bin/login.plc'>"
            write     htmlfile,seq;"<input type='hidden' name='function' value='1' />"
            write     htmlfile,seq;"<input type='hidden' name='SID' value='' />"
            write     htmlfile,seq;"<input type='hidden' name='cgibroker' value='' />"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td align=right width=#"150#">"
            write     htmlfile,seq;"<b><font color=#"#000000#" face=#"Arial#" size=#"2#">Username&nbsp;</font></b></td>"
            write     htmlfile,seq;"<td width=#"85#">"
            write     htmlfile,seq;"<input name='cgiusername' value='' size=#"11#" align=right>"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<td width=50>&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<TD align=right>"
            write     htmlfile,seq;"<b>"
            write     htmlfile,seq;"<font color=#"#000000#" face=#"Arial#" size=#"2#">Password&nbsp;</font></b></TD>"
            write     htmlfile,seq;"<TD width=#"85#">"
            write     htmlfile,seq;"<input type='password' name='cgipassword' size=#"11#"align=right value='' /> </TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td>"
            write     htmlfile,seq;"<a href=#"http://www.nincal.com/plb-bin/passrequest.plc#"><b><font size=#"1#" face=#"Arial#">Forgot"
            write     htmlfile,seq;"Your Password?</font></b></a>&nbsp&nbsp&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<TD width=#"85#" align=center valign=center>"
            write     htmlfile,seq;"<input type=image img border=#"0#" src=#"/images/btn_logon.gif#" width=#"38#" height=#"19#" name=#"I1#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"<TD>"
            write     htmlfile,seq;"<A href=#"http://www.nincal.com/plb-bin/registration.plc#"><font size=#"1#" face=#"Arial#">Register</font></a>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"</form>"
            write     htmlfile,seq;"</Table>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<ul id=#"nav#" class=#"clearfloat#">"
            write     htmlfile,seq;"<li><a href=#"http://www.nincal.com/#" class=#"on#">HOME</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-203#"><a href=#"http://www.nincal.com/index.php/about/#" title=#"About Us#">About Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-205#"><a href=#"http://www.nincal.com/index.php/about/team/#" title=#"Our Team#">Our Team</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-207#"><a href=#"http://www.nincal.com/index.php/list-brokerage/#" title=#"List Brokerage#">List Brokerage</a>"
            write     htmlfile,seq;"<ul>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-209#"><a href=#"http://www.nincal.com/index.php/list-brokerage/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-211#"><a href=#"http://www.nincal.com/index.php/list-brokerage/order-status/#" title=#"Order Status#">Order Status</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-213#"><a href=#"http://www.nincal.com/index.php/list-brokerage/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-6 current_page_ancestor current_page_parent#"><a href=#"http://www.nincal.com/index.php/list-management/#" title=#"List Management#">List Management</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-156 current_page_item#"><a href=#"http://www.nincal.COM/DATACARDS/ExcelLists.html#" title=#"Data Card Library#">Data Card Library</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-158#"><a href=#"http://www.nincal.com/index.php/list-management/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<li class=#"page_item page-item-168#"><a href=#"http://www.nincal.com/index.php/list-management/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-159#"><a href=#"http://www.nincal.com/index.php/featured#" title=#"Featured Lists#">Featured Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-444#"><a href=#"http://www.nincal.COM/DATACARDS/animalwelfare.htm#" title=#"Animal Welfare#">Animal Welfare</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-447#"><a href=#"http://www.nincal.COM/DATACARDS/catalog.htm#" title=#"Catalog#">Catalog</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-449#"><a href=#"http://www.nincal.COM/DATACARDS/charitable.htm#" title=#"Charitable#">Charitable</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-452#"><a href=#"http://www.nincal.COM/DATACARDS/children.htm#" title=#"Children#">Children</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-454#"><a href=#"http://www.nincal.COM/DATACARDS/civilrights.htm#" title=#"Civil / Human Rights#">Civil / Human Rights</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-457#"><a href=#"http://www.nincal.COM/DATACARDS/cultural.htmm#" title=#"Culture / Arts#">Culture / Arts</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-460#"><a href=#"http://www.nincal.COM/DATACARDS/environmental.htm#" title=#"Environmental#">Environmental</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-462#"><a href=#"http://www.nincal.COM/DATACARDS/foodbanks-missions.htm#" title=#"Food Banks / Missions#">Food Banks / Missions</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-464#"><a href=#"http://www.nincal.COM/DATACARDS/health.htm#" title=#"Health / Disease#">Health / Disease</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-466#"><a href=#"http://www.nincal.COM/DATACARDS/jewish.htm#" title=#"Jewish#">Jewish</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-468#"><a href=#"http://www.nincal.COM/DATACARDS/nativeAmerican.htm#" title=#"Native American#">Native American</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-470#"><a href=#"http://www.nincal.COM/DATACARDS/outdoor.htm#" title=#"Outdoor Activities#">Outdoor Activities</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-472#"><a href=#"http://www.nincal.COM/DATACARDS/political.htm#" title=#"Political#">Political</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-476#"><a href=#"http://www.nincal.COM/DATACARDS/publictv.htm#" title=#"Public TV / Radio#">Public TV / Radio</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-474#"><a href=#"http://www.nincal.COM/DATACARDS/publications.htm#" title=#"Publications / Publishing#">Publications / Publishing</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-478#"><a href=#"http://www.nincal.COM/DATACARDS/womensRights.htm#" title=#"Women&##8217;s Rights#">Women&##8217;s Rights</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-8#"><a href=#"http://www.nincal.com/index.php/servicestools/#" title=#"Services/Tools#">Services/Tools</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-297#"><a href=#"http://www.nincal.com/index.php/servicestools/acquisition-advisor/#" title=#"Acquisition Advisor#">Acquisition Advisor</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-239#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs-advanced/#" title=#"FAQs &##8211; Advanced#">FAQs &##8211; Advanced</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-44#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs/#" title=#"FAQs &##8211; The Basics#">FAQs &##8211; The Basics</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
.2013 April 29 DLH
.            write     htmlfile,seq;"<li class=#"page_item page-item-272#"><a href=#"http://www.nincal.com/index.php/multi-channel-marketing/#" title=#"Multi-Channel Marketing#">Multi-Channel Marketing</a></li>"
.2013 April 29 DLH
            write     htmlfile,seq;"<li class=#"page_item page-item-274#"><a href=#"http://www.nincal.com/index.php/clientstestimonials/#" title=#"Clients/Testimonials#">Clients/Testimonials</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-276#"><a href=#"http://www.nincal.com/index.php/contact-us/#" title=#"Contact Us#">Contact Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-202#"><a href=#"http://www.nincal.com/index.php/contact-us/email-us/#" title=#"Email Us#">Email Us</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div id=#"content#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div class=#"post#" id=#"post-156#">"
            write     htmlfile,seq;"<h2>Data Card Library</h2>"
            write     htmlfile,seq;"<div class=#"entry#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<p>Below is an alphabetical listing of all our managed properties. You can     narrow your search by viewing <a href=#"http://www.ninlists.com/DATACARDS/exclupdates.html#">lists that have been     updated in the past 30 days</a>, or visiting our <a href=#"http://www.namesinthenews.com/index.php/list-management/recommended-lists/#">Quick     Recos</a> for recommendations by market.</p>"
            write     htmlfile,seq;"<p>A link to &##8220;Usage&##8221; will be displayed    for all organizations where 12-month continuation usage is available.</p>"
            write     htmlfile,seq;"<dl>"
            write     htmlfile,seq;"<dd>"
.begin Patch 3.95
.            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"525#">"
.end Patch 3.95
            write     htmlfile,seq;"<tbody>"
            write     htmlfile,seq;"<tr>"
            write     htmlfile,seq;"<td width=#"425#"><strong>List Name</strong></td>"
            write     htmlfile,seq;"</tr>"
         endif
         write     htmlfile,seq;""
..Patch 3.9 Logic Updated
         if (HTMLINTRO = "1")
.Patch 3.9
                   write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_dataCardLibrary.gif#" width=#"200#" height=#"16#"><br>"
         elseif (HTMLINTRO = "3")
.beginning DH goes bad July 13th 2010
            write     htmlfile,seq;"<!DOCTYPE html PUBLIC #"-//W3C//DTD XHTML 1.0 Transitional//EN#" #"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd#">"
            write     htmlfile,seq;"<html xmlns=#"http://www.w3.org/1999/xhtml#" dir=#"ltr#" lang=#"en-US#">"
            write     htmlfile,seq;"<head profile=#"http://gmpg.org/xfn/11#">"
            write     htmlfile,seq;"<meta http-equiv=#"Content-Type#" content=#"text/html; charset=UTF-8#" />"
            write     htmlfile,seq;"<title>  Data Card Library - Names in the News</title>"
            write     htmlfile,seq;"<meta name=#"generator#" content=#"WordPress 1436#" />"
            write     htmlfile,seq;"<!-- leave this for stats -->"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News &raquo; Data Card Library Comments Feed#" href=#"http://www.nincal.com/index.php/list-management/data-card-library/feed/#" />"
            write     htmlfile,seq;"<link rel='stylesheet' id='A2A_SHARE_SAVE-css'  href='http://www.nincal.com/wp-content/plugins/add-to-any/addtoany.min.css?ver=1.0' type='text/css' media='' />"
            write     htmlfile,seq;"<link rel='stylesheet' id='contact-form-7-css'  href='http://www.nincal.com/wp-content/plugins/contact-form-7/styles.css?ver=2.3' type='text/css' media='all' />"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-includes/js/jquery/jquery.js?ver=1.3.2'></script>"
            write     htmlfile,seq;"<link rel=#"wlwmanifest#" type=#"application/wlwmanifest+xml#" href=#"http://www.nincal.com/wp-includes/wlwmanifest.xml#" />"
            write     htmlfile,seq;"<link rel='index' title='Names in the News' href='http://www.nincal.com' />"
            write     htmlfile,seq;"<link rel='up' title='List Management' href='http://www.nincal.com/index.php/list-management/' />"
            write     htmlfile,seq;"<link rel='canonical' href='http://www.nincal.com/index.php/list-management/data-card-library/' />"
            write     htmlfile,seq;"<!--[if IE]>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;"ul.addtoany_list a img{filter:alpha(opacity=70)}"
            write     htmlfile,seq;"ul.addtoany_list a:hover img,ul.addtoany_list a.addtoany_share_save img{filter:alpha(opacity=100)}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<![endif]-->"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/jquery-1.3.2.min.js#"></script>"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/plugins/front-slider/scripts/slider.js#"></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/stylesheet.css#" type=#"text/css#" /><link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/plugins/mm-forms/calendar.css#" type=#"text/css#" />      <script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/mm-forms.js'></script>"
            write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/calendar.js'></script>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/nav.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/plugins.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/template-style.css#" type=#"text/css#" media=#"screen#" />"
            write     htmlfile,seq;"<link rel=#"stylesheet#" href=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/print.css#" type=#"text/css#" media=#"print#" />"
            write     htmlfile,seq;"<script type=#"text/javascript#" src=#"http://www.nincal.com/wp-content/themes/wyntonmagazine/js/dropdowns.js#"></script>"
            write     htmlfile,seq;"<link rel=#"alternate#" type=#"application/rss+xml#" title=#"Names in the News RSS Feed#" href=#"http://www.nincal.com/index.php/feed/#" />"
            write     htmlfile,seq;"<link rel=#"pingback#" href=#"http://www.nincal.com/xmlrpc.php#" />"
            write     htmlfile,seq;"</head>"
            write     htmlfile,seq;"<body>"
            write     htmlfile,seq;"<div id=#"page#" class=#"clearfloat#">"
            write     htmlfile,seq;"<div class=#"clearfloat#">"
            write     htmlfile,seq;"<div id=#"branding#" class=#"left#" onclick=#"location.href='http://www.nincal.com';#" style=#"cursor: pointer;#">"
            write     htmlfile,seq;"<div class=#"blogtitle#" ><a href=#"http://www.nincal.com/#">"
            write     htmlfile,seq;"</a></div>"
            write     htmlfile,seq;"<div class=#"description#">"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<div class=#"right#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META http-equiv=Content-Type content=#"text/html; charset=iso-8859-1#">"
            write     htmlfile,seq;"<LINK href=#"nin_styles.css#" type=text/css rel=stylesheet>"
            write     htmlfile,seq;"<SCRIPT language=JavaScript type=text/JavaScript>"
            write     htmlfile,seq;"<!--"
            write     htmlfile,seq;"function MM_preloadImages() { //v3.0"
            write     htmlfile,seq;"var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();"
            write     htmlfile,seq;"var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)"
            write     htmlfile,seq;"if (a[i].indexOf(#"###")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImgRestore() { //v3.0"
            write     htmlfile,seq;"var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_findObj(n, d) { //v4.01"
            write     htmlfile,seq;"var p,i,x;  if(!d) d=document; if((p=n.indexOf(#"?#"))>0&&parent.frames.length) {"
            write     htmlfile,seq;"d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}"
            write     htmlfile,seq;"if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];"
            write     htmlfile,seq;"for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);"
            write     htmlfile,seq;"if(!x && d.getElementById) x=d.getElementById(n); return x;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImage() { //v3.0"
            write     htmlfile,seq;"var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)"
            write     htmlfile,seq;"if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"//-->"
            write     htmlfile,seq;"</SCRIPT>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<META content=#"Microsoft FrontPage 4.0#" name=GENERATOR>"
            write     htmlfile,seq;"<style type=#"text/css#">"
            write     htmlfile,seq;".style1 {"
            write     htmlfile,seq;"font-size: x-small;"
            write     htmlfile,seq;"font-weight: bold;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"</style>"
            write     htmlfile,seq;"<meta name=#"verify-v1#" content=#"6BHMx2UPpCsHKyuB/beiUpzV9EMeXslsxUB+jo2et8A=#" >"
            write     htmlfile,seq;"</HEAD>"
            write     htmlfile,seq;"<Table cellSpacing=0 cellPadding=0 align=right border=0 id=#"table4#">"
            write     htmlfile,seq;"<Caption><b><font color=#"#000000#" face=#"Arial#" size=#"2#">Client Login:</font></b></Caption>"
            write     htmlfile,seq;"<form name='LoginForm' method='post' action='http://www.nincal.com/plb-bin/login.plc'>"
            write     htmlfile,seq;"<input type='hidden' name='function' value='1' />"
            write     htmlfile,seq;"<input type='hidden' name='SID' value='' />"
            write     htmlfile,seq;"<input type='hidden' name='cgibroker' value='' />"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td align=right width=#"150#">"
            write     htmlfile,seq;"<b><font color=#"#000000#" face=#"Arial#" size=#"2#">Username&nbsp;</font></b></td>"
            write     htmlfile,seq;"<td width=#"85#">"
            write     htmlfile,seq;"<input name='cgiusername' value='' size=#"11#" align=right>"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<td width=50>&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<TD align=right>"
            write     htmlfile,seq;"<b>"
            write     htmlfile,seq;"<font color=#"#000000#" face=#"Arial#" size=#"2#">Password&nbsp;</font></b></TD>"
            write     htmlfile,seq;"<TD width=#"85#">"
            write     htmlfile,seq;"<input type='password' name='cgipassword' size=#"11#"align=right value='' /> </TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"<TR>"
            write     htmlfile,seq;"<td>"
            write     htmlfile,seq;"<a href=#"http://www.nincal.com/plb-bin/passrequest.plc#"><b><font size=#"1#" face=#"Arial#">Forgot"
            write     htmlfile,seq;"Your Password?</font></b></a>&nbsp&nbsp&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<TD width=#"85#" align=center valign=center>"
            write     htmlfile,seq;"<input type=image img border=#"0#" src=#"/images/btn_logon.gif#" width=#"38#" height=#"19#" name=#"I1#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"<TD>"
            write     htmlfile,seq;"<A href=#"http://www.nincal.com/plb-bin/registration.plc#"><font size=#"1#" face=#"Arial#">Register</font></a>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;"</form>"
            write     htmlfile,seq;"</Table>"
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"</TR>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"</div>"
            write     htmlfile,seq;"<ul id=#"nav#" class=#"clearfloat#">"
            write     htmlfile,seq;"<li><a href=#"http://www.nincal.com/#" class=#"on#">HOME</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-203#"><a href=#"http://www.nincal.com/index.php/about/#" title=#"About Us#">About Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-205#"><a href=#"http://www.nincal.com/index.php/about/team/#" title=#"Our Team#">Our Team</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-207#"><a href=#"http://www.nincal.com/index.php/list-brokerage/#" title=#"List Brokerage#">List Brokerage</a>"
            write     htmlfile,seq;"<ul>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-209#"><a href=#"http://www.nincal.com/index.php/list-brokerage/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-211#"><a href=#"http://www.nincal.com/index.php/list-brokerage/order-status/#" title=#"Order Status#">Order Status</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-213#"><a href=#"http://www.nincal.com/index.php/list-brokerage/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-6 current_page_ancestor current_page_parent#"><a href=#"http://www.nincal.com/index.php/list-management/#" title=#"List Management#">List Management</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-156 current_page_item#"><a href=#"http://www.nincal.COM/DATACARDS/ExcelLists.html#" title=#"Data Card Library#">Data Card Library</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-158#"><a href=#"http://www.nincal.com/index.php/list-management/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<li class=#"page_item page-item-168#"><a href=#"http://www.nincal.com/index.php/list-management/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-159#"><a href=#"http://www.nincal.com/index.php/featured#" title=#"Featured Lists#">Featured Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-444#"><a href=#"http://www.nincal.COM/DATACARDS/animalwelfare.htm#" title=#"Animal Welfare#">Animal Welfare</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-447#"><a href=#"http://www.nincal.COM/DATACARDS/catalog.htm#" title=#"Catalog#">Catalog</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-449#"><a href=#"http://www.nincal.COM/DATACARDS/charitable.htm#" title=#"Charitable#">Charitable</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-452#"><a href=#"http://www.nincal.COM/DATACARDS/children.htm#" title=#"Children#">Children</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-454#"><a href=#"http://www.nincal.COM/DATACARDS/civilrights.htm#" title=#"Civil / Human Rights#">Civil / Human Rights</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-457#"><a href=#"http://www.nincal.COM/DATACARDS/cultural.htmm#" title=#"Culture / Arts#">Culture / Arts</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-460#"><a href=#"http://www.nincal.COM/DATACARDS/environmental.htm#" title=#"Environmental#">Environmental</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-462#"><a href=#"http://www.nincal.COM/DATACARDS/foodbanks-missions.htm#" title=#"Food Banks / Missions#">Food Banks / Missions</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-464#"><a href=#"http://www.nincal.COM/DATACARDS/health.htm#" title=#"Health / Disease#">Health / Disease</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-466#"><a href=#"http://www.nincal.COM/DATACARDS/jewish.htm#" title=#"Jewish#">Jewish</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-468#"><a href=#"http://www.nincal.COM/DATACARDS/nativeAmerican.htm#" title=#"Native American#">Native American</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-470#"><a href=#"http://www.nincal.COM/DATACARDS/outdoor.htm#" title=#"Outdoor Activities#">Outdoor Activities</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-472#"><a href=#"http://www.nincal.COM/DATACARDS/political.htm#" title=#"Political#">Political</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-476#"><a href=#"http://www.nincal.COM/DATACARDS/publictv.htm#" title=#"Public TV / Radio#">Public TV / Radio</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-474#"><a href=#"http://www.nincal.COM/DATACARDS/publications.htm#" title=#"Publications / Publishing#">Publications / Publishing</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-478#"><a href=#"http://www.nincal.COM/DATACARDS/womensRights.htm#" title=#"Women&##8217;s Rights#">Women&##8217;s Rights</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-8#"><a href=#"http://www.nincal.com/index.php/servicestools/#" title=#"Services/Tools#">Services/Tools</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-297#"><a href=#"http://www.nincal.com/index.php/servicestools/acquisition-advisor/#" title=#"Acquisition Advisor#">Acquisition Advisor</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-239#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs-advanced/#" title=#"FAQs &##8211; Advanced#">FAQs &##8211; Advanced</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-44#"><a href=#"http://www.nincal.com/index.php/servicestools/faqs/#" title=#"FAQs &##8211; The Basics#">FAQs &##8211; The Basics</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
.2013 April 29 DLH
.            write     htmlfile,seq;"<li class=#"page_item page-item-272#"><a href=#"http://www.nincal.com/index.php/multi-channel-marketing/#" title=#"Multi-Channel Marketing#">Multi-Channel Marketing</a></li>"
.2013 April 29 DLH
            write     htmlfile,seq;"<li class=#"page_item page-item-274#"><a href=#"http://www.nincal.com/index.php/clientstestimonials/#" title=#"Clients/Testimonials#">Clients/Testimonials</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-276#"><a href=#"http://www.nincal.com/index.php/contact-us/#" title=#"Contact Us#">Contact Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-202#"><a href=#"http://www.nincal.com/index.php/contact-us/email-us/#" title=#"Email Us#">Email Us</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div id=#"content#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;""
            write     htmlfile,seq;"<div class=#"post#" id=#"post-156#">"
            write     htmlfile,seq;"<h2>Data Card Library</h2>"
            write     htmlfile,seq;"<div class=#"entry#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"<p>These managed lists have been updated within the past 30 days. </p>"
            write     htmlfile,seq;"<p>A link to &##8220;Usage&##8221; will be displayed    for all organizations where 12-month continuation usage is available.</p>"
            write     htmlfile,seq;"<dl>"
            write     htmlfile,seq;"<dd>"
.begin Patch 3.95
.            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"525#">"
.end Patch 3.95
            write     htmlfile,seq;"<tbody>"
            write     htmlfile,seq;"<tr>"
            write     htmlfile,seq;"<td width=#"425#"><strong>List Name</strong></td>"
            write     htmlfile,seq;"</tr>"
         elseif (HTMLINTRO = "3")
                   write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_recentlyUpdated.gif#" width=#"200#" height=#"16#"><br>"
         endif
         write     htmlfile,seq;""
         write     htmlfile,seq;"<span class=#"arial10#"><br>Last update: ",today," </span></p>"
         write     htmlfile,seq;""
..Patch 3.9 Logic Updated
        if (HTMLINTRO = "1")
.Patch 3.9
         Elseif (HTMLINTRO = "2")
.DH gone bad
.                   write     htmlfile,seq;" <p>Below is an alphabetical listing of all our managed properties. You can"
.                write     htmlfile,seq;""
..>Patch 3.8.6
.             write     htmlfile,seq;"   narrow your search by viewing <a href=#"Exclupdates.html#">lists that have been"
..>Patch 3.8.6
.          write     htmlfile,seq;""
.                   write     htmlfile,seq;"   updated in the past 30 days</a>, or visiting our <a href=#"recos.htm#">Quick"
.                write     htmlfile,seq;""
.             write     htmlfile,seq;"   Recos</a> for recommendations by market.</p>"
.             write     htmlfile,seq;"   <p>A link to #"Usage#" will be displayed"
.             write     htmlfile,seq;"   for all organizations where 12-month continuation usage is available.</p>"
.
.DH gone bad
         elseif (HTMLINTRO = "3")
.DH gone bad
.            write     htmlfile,seq;"<p>These managed lists have been updated within the past 30 days. </p>"
..>Patch 3.8.7
.             write     htmlfile,seq;"   <p>A link to #"Usage#" will be displayed"
.             write     htmlfile,seq;"   for all organizations where 12-month continuation usage is available.</p>"
..>Patch 3.8.7
         endif
         write     htmlfile,seq;""
         write     htmlfile,seq;" <dl>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"   <dd>"
         write     htmlfile,seq;""
         write     htmlfile,seq;""
.DH gone bad
                              if (HTMLINTRO = "1")
                              elseif (HTMLINTRO = "2")
.>Patch 3.8.7
           move c1 to tableopen

.DH bad
.         write     htmlfile,seq;"                           <table border=#"0#" cellpadding=0 width=#"490#" cellspacing=0>"
.         write     htmlfile,seq;"                                     <tr>"
.         write     htmlfile,seq;"                                               <td width=#"425#">"
.         write     htmlfile,seq;"                                                         <b>List Name</b>"
.         write     htmlfile,seq;"                                               </td>"
.         write     htmlfile,seq;"                                     </tr>"
.DH bad


.>Patch 3.8.7
                              elseif (HTMLINTRO = "3")

.>Patch 3.8.7
           move c1 to tableopen

.DH bad
.         write     htmlfile,seq;"                           <table border=#"0#" cellpadding=0 width=#"490#" cellspacing=0>"
.         write     htmlfile,seq;"                                     <tr>"
.         write     htmlfile,seq;"                                               <td width=#"425#">"
.         write     htmlfile,seq;"                                                         <b>List Name</b>"
.         write     htmlfile,seq;"                                               </td>"
.         write     htmlfile,seq;"                                     </tr>"
.DH bad


.>Patch 3.8.7

                              endif
         endif
.Patch 3.9 Code Replaced
         GOTO      PRESTART
OUTPUT
.begin patch 4.0
.         if        (htmlflag <> 2)
         if        (htmlflag <> 2 & xlsxflag < 2)
.end patch 4.0
         SPLOPEN   PRTFILE
         DISPLAY   *P15:7,PRTNAME
         clear     outname
.START PATCH 3.02 REPLACED LOGIC
.         APPEND    "g:\DATA\" TO OUTNAME
         APPEND    NTWKPATH1 TO OUTNAME
.END PATCH 3.02 REPLACED LOGIC
         APPEND    PRTNAME TO OUTNAME
         APPEND    ".TMP" TO OUTNAME
         RESET     OUTNAME
.         PREPARE   OUTPUT,OUTNAME
         DISPLAY   *P01:07,"Output File :":
                   *P15:07,PRTNAME
         ENDIF
.end patch 3.0
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
PRESTART MOVE      C0 TO TOTUNIV
         KEYIN     *P1:24,*T5,STR1;
         CLEAR     PF4
         TRAPCLR   F4
         CALL      FUNCDISP
         TRAP      IO giving error IF IO
         OPEN      INPUT,NAME,EXCLUSIVE
         MOVE      "00000" TO COUNT
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MO,DAY,YR
         XIF
         IFZ       PC
         UNPACK    DATE INTO MO,STR1,DAY,STR1,YR
         XIF
         REP       zfill,DAY
         REP       zfill,MO
         CLEAR     TODAY
         PACK      TODAY FROM MO,SLASH,DAY,SLASH,YR
.begin patch 3.2
               if             (HTML30Flag = 2)
               UNPACK    DATE INTO Mm,STR1,Dd,STR1,Yy
               call           cvtjul
               move           juldays to dateJulian     ;system date used to compare against revdate
               endif
.               call           debug
.end patch 3.2
START
         CALL      HEADER
.
*............................................................
. READ A RECORD FROM THE FILE
.
.begin patch 3.0
READ     DISPLAY   *P1:23,*EL,*HON,"READING";
         if        (Htmlflag <> 2)
.START PATCH 3.5 REPLACED LOGIC
.          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,CATCDE1,CATCDE2:
.                   CATCDE3,CATCDE4,CATCDE5,CATCDE6,CATCDE7,CATCDE8,CATCDE9:
.                   CATCDE10,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                   REVDATE,PASSWORD,MLSTNAME,UNIVERSE,TEXT1
.START PATCH 3.8.5 REPLACED LOGIC
.         READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,COMMPER:
.                   HOTLINE,NEWDATE,REVDATE,PASSWORD,MLSTNAME,UNIVERSE,NDATCONV
          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3:
                    HOTLINE,NEWDATE,REVDATE,PASSWORD,MLSTNAME,UNIVERSE,NDATCONV
.END PATCH 3.8.5 REPLACED LOGIC
   if      over
                    call    setlr
                    goto    totcalc
          endif
.
          clear     TEXT1
          if (NDATCONV = "1")
                    move      C1,NDATPATH
                    pack      NDATFLD,LSTNUM
                    move      "NDATKEY",Location
                    pack      KeyLocation,NDATFLD
                    call      NDATKEY
.                   if (NDATEXCH <> "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"02XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
.Patch3.6
                                        clear dim13a
                                        clear n10
                                        move      mask13 to dim13a
                                        move      nselqty to n10
                   ADD   N10 TO TOTUNIV
                                        edit      n10 to dim13a
                                        call trim using dim13a
                                        call trim using nselsname
                                        clear str30
                                        packkey str30 with nselsname
                                        uppercase  str30
                                        move nselprice to dim9a
                                        call trim using dim9a
                                        CALL SelectLoadModifier
.                                       call trim using
                                        if (NSELEXC <> "2")
.patch3.6
                                                  pack      TEXT1,dim13a,B1,str30,"$",str25
.                                                 pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"$",NSELPRICE
                                        else
                                                  pack      TEXT1,dim13a,B1,str30,"EXCH ONLY"
.                                                 pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"EXCHANGE ONLY"
                                        endif
.patch3.6
                              else
                                        goto DataCheckText
                              endif
.                   endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              call      PARSITUP using text1,ntxttext,C1
.                             move      NTXTTEXT,text1
                    endif
          endif
                goto    ridon
         else
         read      input,seq;datvars
         goto      eoj if over
                        If    (HTML30FLag = 2)
.                        call                 debug
                        unpack               REVDATE      into CC,YY,MM,DD
                        call                 cvtjul
                                       if                   (juldays < (dateJulian-30))
                                       goto                 read
                                       endif
               endif
         endif
         MOVE      C0 TO PTRFLAG
test1
         IF        (HTMLPTR < 27)
         reset     htmlalpha
         bump      htmlalpha by htmlptr
         move      htmlalpha to bookmark
.lets make sure we are not skipping letters.
         clear     str1
.         move      c0 to skippedletr
         move      mlstname to str1
         scan      str1 in htmlalpha                    ;find letter we are on
         movefptr  htmlalpha to n2
         reset     htmlalpha
.begin patch 3.2
.         sub       htmlptr from n2
               if             ((n2-1) <> htmlptr)
.         if        (n2 > c1 & htmlptr > c1)            .we skipped a letter or more
.         call      debug
                         reset     htmlalpha
                         sub       c1 from n2
                         move      n2 to htmlptr
.                         add       c1 to htmlptr
                         bump      htmlalpha by htmlptr
                         clear     bookmark
                         move      htmlalpha to bookmark
.         move      c1 to skippedletr         .set flag
         endif
.end patch 3.2
.         LOAD      BOOKMARK FROM HTMLPTR OF HTMLALPHA

.         cmatch    "C" to mlstname
.         call      debug if equal
         CMATCH    BOOKMARK TO MLSTNAME
                 IF        EQUAL
                 move      c1 to markflag
                 else
                 move      c0 to markflag
                 endif
         if      (markflag = c1)
                MOVE      C1 TO PTRFLAG          .AT WRITE CREATE A BOOKMARK
                              if (HTMLINTRO = "1")
                              elseif (HTMLINTRO = "2")

.>Patch 3.8.7
          if (TableOpen = c1)
           move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7
..>Patch 3.8.7
           move        c1 to tableopen
.begin Patch 3.95
.         write     htmlfile,seq;"                           <table border=#"0#" width=#"490#" cellpadding=0 cellspacing=0>"
.            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"525#">"
.end Patch 3.95
.>Patch 3.8.7
                              elseif (HTMLINTRO = "3")
.>Patch 3.8.7
          if (TableOpen = c1)
           move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7
           move        c1 to tableopen
.begin Patch 3.95
.         write     htmlfile,seq;"                           <table border=#"0#" width=#"490#" cellpadding=0 cellspacing=0>"
.            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"525#">"
.end Patch 3.95
.>Patch 3.8.7
                              endif
.Patch 3.9 Code Replaced
                 ADD       C1 TO HTMLPTR
              ELSE
                 MOVE      C0 TO PTRFLAG
                 ENDIF
         ENDIF
packtext
         call      cleanprice
         goto      testeof
cleanprice
         SCAN      "EXCHANGE ONLY" IN TEXT1
         RETURN    IF EQUAL                 NO USABLE $ RETURN
         RESET     TEXT1

         SCAN      "Exchange only" IN TEXT1
         RETURN    IF EQUAL                 NO USABLE $ RETURN
         RESET     TEXT1

         SCAN      "EXCHANGE" IN TEXT1
         if        equal
         movefptr  text1 to n3
         sub       c2 from n3
         reset     text1
         SETLPTR   text1 to n3
         return
         endif

         SCAN      "Exchange" IN TEXT1
         if        equal
         movefptr  text1 to n3
         sub       c2 from n3
         reset     text1
         SETLPTR   text1 to n3
         return
         endif

         SCAN      "*SEE BELOW" IN TEXT1
         if        equal
         movefptr  text1 to n3
         sub       c2 from n3
         reset     text1
         SETLPTR   text1 to n3
         return
         endif

         SCAN      "*See below" IN TEXT1
         if        equal
         movefptr  text1 to n3
         sub       c2 from n3
         reset     text1
         SETLPTR   text1 to n3
         return
         endif

         RESET     TEXT1
         SCAN      "$" IN TEXT1
         if        not equal
         reset     text1          NO USABLE $ RETURN
         return
         endif
.
         movefptr  text1 to n3
         SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
         if        not equal            .think so lets truncate
         reset     text1
         sub       c2 from n3
         SETLPTR   text1 to n3
         else                          .guess above was not it
         movefptr  text1 to n3         .maybe here
         reset     text1
         sub       c2 from n3
         SETLPTR   text1 to n3
         CLEAR     STR2
         endif
         RETURN

*............................................................
. TEST FOR END OF FILE
. IF END OF FILE: TURN ON LR AND L1-L9
.
testeof
.end patch 3.0
         GOTO      RIDON IF NOT OVER
         DISPLAY   *P1:23,*EL;
         CALL      SETLR
         GOTO      TOTCALC
*............................................................
. TURN ON LEVEL-INDICATOR SWITCHES
.
SETLR    MOVE      ONE TO LR
         RETURN
*............................................................
.
RIDON
         ADD       ONE TO COUNT
         DISPLAY   *P15:12,*EL,"RECORDS READ = ",COUNT;
+............................................................
. TOTAL CALCULATIONS
.
TOTCALC  TRAPCLR   PARITY  (NOP)
TOTCLX   TRAPCLR   PARITY  (NOP)
         CALL      TOTOUT
*............................................................
. SEE IF LR INDICATOR IS ON
. IF SO: END JOB
.
TESTLR   BRANCH    LR OF EOJ
         GOTO      MOVEDATA  (NOP)
*............................................................
. MOVE DATA FROM INPUT AREA TO FIELDS
.
MOVEDATA
         MOVE      B4 TO EXCL        CLEAR EXCLUSIVE PRINT FIELD.
          IF        (Elstcde = "C" or ElstCde = "P")
          call      EXCL
          endif
.         CMATCH    "C" TO ELSTCDE        EXCLUSIVE?
.         CALL      EXCL IF EQUAL         YES
*............................................................
. DETAIL CALCULATIONS
.
DETCALC  MOVE      "        ",PSTATUS
         CMATCH    "W",STATUS
         CALL      WITHDRAW IF EQUAL
         CMATCH    "T" TO STATUS
         CALL      TEMPWITH IF EQUAL
         ADD       ONE,PRSW
         ADD       ONE TO LRTOT
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      C0 TO N9
.         MOVE      UNIVERSE TO N9
.         ADD       N9 TO TOTUNIV
         MOVE      C0 TO N10
         MOVE      UNIVERSE TO N10
         ADD       N10 TO TOTUNIV
.END PATCH 3.5 REPLACED LOGIC
         GOTO      DETLOAD
* ...........................................................
WITHDRAW
         BRANCH    WITHFLAG OF SKIP
         MOVE      "WTHDRN",PSTATUS
         RETURN
TEMPWITH MOVE      "TMPWDN" TO PSTATUS
         RETURN
EXCL
.          MOVE      "EXCL" TO EXCL
         RETURN
SKIP
         NORETURN
         GOTO      READ
*............................................................
DETLOAD
. LOAD PRINT LINE
         DISPLAY   *P1:23,*EL,*HON,"LOADING PRINT BUFFER";
         BRANCH    XLSXFlag OF LOAD,LTSOUT
LTSOUT
.START PATCH 3.5 REPLACED LOGIC
.         UNPACK    REVDATE INTO MM,STR1,DD,STR1,cc,YY

.         UNPACK    NDATUPDDATE,CC,YY,MM,DD
          UNPACK    REVDATE,CC,YY,MM,DD
.END PATCH 3.5 REPLACED LOGIC
          Type      REvdate
          if        equal
          CALL      CVTJULTS
          endif
.aug302006 skip this WHoHa
          goto      ltsout1
         CLEAR     NUMBER
         clear     price
         clear     ldesc
CMOVE    CMOVE     TEXT1 TO STR1
         TYPE      STR1
         IF        EQUAL
         APPEND    STR1 TO NUMBER
         ELSE
         CMATCH    "," TO STR1
         GOTO      BUMPIT IF EQUAL
         CMATCH    B1 TO STR1
         GOTO      NUMDONE IF EQUAL
         GOTO      NUMDONE IF EOS
         GOTO      BUMPIT
         ENDIF
BUMPIT   BUMP      TEXT1 BY 1
         GOTO      CMOVE
NUMDONE  RESET     NUMBER
         MOVEFPTR  TEXT1 TO BEGIN
         SCAN      "$" IN TEXT1
         IF        NOT EQUAL                   .NO PRICE
         CLEAR     PRICE
         RESET     TEXT1
         SETLPTR   TEXT1 TO BEGIN
         APPEND    TEXT1 TO LDESC
         RESET     TEXT1
         ELSE
         MOVEFPTR  TEXT1 TO LAST
         APPEND    TEXT1 TO PRICE
         SUB       C1 FROM LAST
         RESET     TEXT1
         SETLPTR   TEXT1 TO LAST
         BUMP      TEXT1 BY BEGIN
         APPEND    TEXT1 TO LDESC
         ENDIF
         rep       "#"'" in mlstname                                ."
         reset     text1
         unpack    text1 into str1,text
.START PATCH 3.5 REPLACED LOGIC
.         match     ownnum to nownfld
.         if        not equal
.         move      ownnum to nownfld
.         call      nownkey
.         endif
Ltsout1
          unpack    OWNNUM,str2,str4
          match     str4,nownfld
          if not equal
./Patch 3.8.9 Comment Out     
                    move      str4,nownfld
                    call      nownkey
          endif
./Patch 3.8.9 Comment Out               
./Patch 3.8.9
                    packkey   Ndatfld,lstnum
                    call      Ndatkey

.                    call      debug
                              
                    pack      COMPFLD,datFUl
                    rep       zfill,COMPFLD
                    move      C1,COMPPATH
                    move      "Driver-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              if (COMPSVBFLG <> "T")
                                        clear     COMPFLD
                                        clear     NFULCOMP
                              else
                                        move      COMPCOMP,NFULCOMP
                              endif
                    endif
./End Patch 3.8.9
                    
.         endif
.END PATCH 3.5 REPLACED LOGIC
          Packkey   Nmdlfld from lstnum
          rep       Zfill,Nmdlfld
         call      nmdlkey
         rep       loWUP in text
         CLEAR     STR24
         reset     text
         MOVE      "RENTAL" TO STR24
         scan      "EXCHANGE ONLY" IN TEXT
         IF         EQUAL
         MOVE       "EXCHANGE ONLY" TO STR24
         GOTO       WRITFLAT
         ENDIF
         RESET     TEXT
         scan      "EXCH ONLY" IN TEXT
         IF         EQUAL
         MOVE       "EXCHANGE ONLY" TO STR24
         GOTO       WRITFLAT
         ENDIF
         RESET     TEXT
         scan      "EXCH" IN TEXT
         IF         EQUAL
         MOVE       "EXCHANGE/RENT" TO STR24
         GOTO       WRITFLAT
         ENDIF
writflat
.begin patch 3.1
         scan   "|A" in mlstname
         call   undofixita if equal
         reset  Mlstname
         scan   "|T" in mlstname
         call   undofixitThe if equal
         reset  Mlstname
.end patch 3.1

.START PATCH 3.5 REPLACED LOGIC
.         WRITE     OUTPUT,seq;*cdfon,LSTNUM,mlstname,universe,text1:
.                 price,revdate:
.                   mdlplan,mdlcall,STR24 
.
.begin patch 4.0
          if        (XLSXFlag = c2)
.end patch 4.0
          Move            Row1,DimRow1
                    call            Trim using DimRow1
                    Move            Row2,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from "B",DimRow1
                    pack            Exrange2 from "B",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
          
                    setprop sheet.range(ExRange1,ExRange1),*Value=Lstnum,*NumberFormat="######0"
                    pack            Exrange1 from "C",DimRow1
                    pack            Exrange2 from "C",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2

.Begin patch 3.96
.begin patch 3.97
          call      trim using mlstname
.end patch 3.97
                              if        ((ELSTCDE = "P" | ELSTCDE = "C") & NDATWEB <> "1")
                              REP             "#"'" in  Mlstname     ."       Hyperlink does not like double quotes
                              Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/Datacards/Data",lstnum,".htm","#",#"",Mlstname,"#")"
                              setprop sheet.range(ExRange1,ExRange1),*Formula=taskname
                              else
                              setprop sheet.range(ExRange1,ExRange1),*Value=Mlstname
                              endif
.                              setprop sheet.range(ExRange1,ExRange1),*Value=Mlstname
                    pack            Exrange1 from "D",DimRow1
                    pack            Exrange2 from "D",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2


                    pack      NUSGFLD1,"01X",LSTNUM
                    move      "NUSGAIM",Location
                    pack      KeyLocation,"Key: ",NUSGFLD1
                    call      NUSGAIM
                              If Not over
                                        if (NDATLUSAGE <> "F")
                                        Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/usage/usg",lstnum,".pdf","#",#"","Click to see Usage","#")"
                                        setprop sheet.range(ExRange1,ExRange1),*Formula=taskname
                                        endif
                              endif
                    pack            Exrange1 from "T",DimRow1
                    pack            Exrange2 from "T",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
          Clear     NoteText
          Clear     NQRCDdesc
          packkey   NQRCFLD   From Lstnum
          rep       zfill in nqrcfld
          Call      NQRCKey
          if        not over  
          packkey   NQRCDFLD from NQRCNum
          rep       zfill in nqrcdfld
          Call      NqrcDkey

          append    NQRCDdesc,NoteText
.          append    Newline,NoteTExt
          Loop
          Call      NQRCKS
                    if        not over
                              if        (NQRCLIST <> Lstnum)
                              Break
                              endif
                    packkey   NQRCDFLD from NQRCNum
                    rep       zfill in nqrcdfld
                    Call      NqrcDkey
          append    NQRCDdesc,NoteText
.          append    Newline,NoteTExt
          else
          Break     
                    Endif
         repeat
         endif
          reset     NoteText
                    setprop   sheet.Range(ExRange1,ExRange1),*Value=NoteText,*WrapText=OTRUE             
                    Clear     notetext
.end patch 3.96


                    pack            Exrange1 from "F",DimRow1
                    pack            Exrange2 from "F",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=UNiverse,*NumberFormat="##,####0"
                    pack            Exrange1 from "g",DimRow1
                    pack            Exrange2 from "g",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=text1
                    pack            Exrange1 from "H",DimRow1
                    pack            Exrange2 from "H",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=PRice,*NumberFormat="$$,$$$$0.00"
                    pack            Exrange1 from "I",DimRow1
                    pack            Exrange2 from "I",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
.begin patch 3.97
.                    setprop sheet.range(ExRange1,ExRange1),*Value=Revdate
                    unpack    revdate into cc,yy,mm,dd
                    pack      str10 from mm,slash,dd,slash,cc,yy
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    pack            Exrange1 from "J",DimRow1
                    pack            Exrange2 from "J",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
.                    setprop sheet.range(ExRange1,ExRange1),*Value=NDATUPDDATE
                    unpack    NDATUPDDATE into cc,yy,mm,dd
                    pack      str10 from mm,slash,dd,slash,cc,yy
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
.end patch 3.97
                    pack            Exrange1 from "K",DimRow1
                    pack            Exrange2 from "K",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=MdlPlan
                    pack            Exrange1 from "L",DimRow1
                    pack            Exrange2 from "L",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Mdlcall
                    pack            Exrange1 from "m",DimRow1
                    pack            Exrange2 from "m",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=str24
                    pack            Exrange1 from "N",DimRow1
                    pack            Exrange2 from "N",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Ownnum
                    pack            Exrange1 from "o",DimRow1
                    pack            Exrange2 from "o",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=OWNOCPY
                    pack            Exrange1 from "p",DimRow1
                    pack            Exrange2 from "p",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=NFULCOMP
                    pack            Exrange1 from "Q",DimRow1
                    pack            Exrange2 from "Q",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Commper
                    pack            Exrange1 from "R",DimRow1
                    pack            Exrange2 from "R",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Pstatus
                    pack            Exrange1 from "S",DimRow1
                    pack            Exrange2 from "S",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Password
          Add       c1 to row1
          Add       c1 to row2
.begin patch 4.0
          Elseif        (XLSXFlag = c3)
                    Move            Row1,DimRow1
                    call            Trim using DimRow1
                    Move            Row2,DimRow2
                    call            Trim using DimRow2
.                   
.Create detail sheet for datacard
                    add       C1,index
                    if (index > C1)
                              getprop   sheets,*Count=N8
                              setprop   SheetIndex,VarValue=N8
                              getprop   sheets,*Item(SheetIndex)=sheet
                              sheets.Add.Move using *After=sheet
                              add       C1,index
                    endif
                    getprop   sheets,*Count=N8
                    setprop   SheetIndex,VarValue=N8
                    getprop   sheets,*Item(SheetIndex)=sheet
                    Sheets.item giving sheet using Sheetindex
                    Move c0 to CellRowCnt
                    call      trim using mlstname
                    REP       "#"'" in  Mlstname     ."       Hyperlink does not like double quotes
.START PATCH 4.01 REPLACED LOGIC
.Deal with limitations on Excel Worksheet names...
                    move      MLSTNAME,WorksheetName                  .Excel only allows worksheet names up to 31 characters
                    REP                 "\ " in WorksheetName
                    REP                 "/ " in WorksheetName
                    REP                 "? " in WorksheetName
                    REP                 "* " in WorksheetName
                    REP                 "[ " in WorksheetName
                    REP                 "] " in WorksheetName
                    REP                 ": " in WorksheetName
                    REP                 "' " in WorksheetName         .Hyperlinks.Add function cannot include locations with apostrophes...
                    call      Trim using WorksheetName
                    Setprop sheet,*Name=WorksheetName
                    pack      NextLink,"'",WorksheetName,"'","!A1"
.END PATCH 4.01 REPLACED LOGIC

.go produce details
                    Call SheetSetup
                    Move c1 to curcellnum
          
.back to summary goodies
.need to return to the summary sheet
                    Sheets.item giving sheet using 1
                    pack            Exrange1 from "B",DimRow1
                    pack            Exrange2 from "B",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1),*Value=Lstnum,*NumberFormat="######0"
                    pack            Exrange1 from "C",DimRow1
                    pack            Exrange2 from "C",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
.START PATCH 4.01 REPLACED LOGIC
.                   Setprop sheet.range(ExRange1,ExRange1),*Formula=taskname
.                   Pack      Taskname from "=Hyperlink(#"",WorksheetName,"!A1","#",#"",mlstname,"#")"
.Setting up the Link for the next Detail Page
.                   pack      PreviousLink from "=Hyperlink(#"",WorksheetName,"!A1","#",#"","Previous","#")"
.
                    pack      taskname,"'",WorksheetName,"'","!A1"
                    pack      PreviousLink,"'",WorksheetName,"'","!A1"
.What is happening in next line of code is a pulling of a range automation object to feed to the Hyperlinks.Add method
.We select the Range we are working with, and then select the range within that range.  This is why the following code is
.in place:  *range("A1").  If we used:  *range(ExRange1) we would have an offset of whatever is in ExRange1...
                    getprop   sheet.range(ExRange1),*range("A1")=xlrange1
                    clear     str1
                    sheet.Hyperlinks.Add using *Anchor=xlrange1, *Address=str1, *SubAddress=taskname, *TextToDisplay=mlstname
.END PATCH 4.01 REPLACED LOGIC
.
.START PATCH 4.01 REPLACED LOGIC
.                   pack            Exrange1 from "T",DimRow1
.                   Clear     NoteText
.                   Clear     NQRCDdesc
.                   packkey   NQRCFLD   From Lstnum
.                   rep       zfill in nqrcfld
.                   Call      NQRCKey
.                   if        not over  
.                             packkey   NQRCDFLD from NQRCNum
.                             rep       zfill in nqrcdfld
.                             Call      NqrcDkey
.                             append    NQRCDdesc,NoteText
.                             Loop
.                                       Call      NQRCKS
.                                       if        not over
.                                                 if        (NQRCLIST <> Lstnum)
.                                                           Break
.                                                 endif
.                                                 packkey   NQRCDFLD from NQRCNum
.                                                 rep       zfill in nqrcdfld
.                                                 Call      NqrcDkey
.                                                 append    NQRCDdesc,NoteText
.                                       else
.                                                 Break     
.                                       Endif
.                             repeat
.                   endif
.                   reset     NoteText
.                   setprop   sheet.Range(ExRange1),*Value=NoteText,*WrapText=OTRUE             
.                   Clear     notetext
.                   pack            Exrange1 from "E",DimRow1
.                   setprop sheet.range(ExRange1),*Value=text1
.                   pack            Exrange1 from "F",DimRow1
.                   setprop sheet.range(ExRange1),*Value=PRice,*NumberFormat="$$,$$$$0.00"
.                   pack            Exrange1 from "G",DimRow1
.                   unpack    revdate into cc,yy,mm,dd
.                   pack      str10 from mm,slash,dd,slash,cc,yy
.                   setprop sheet.range(ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
.                   pack            Exrange1 from "H",DimRow1
.                   unpack    NDATUPDDATE into cc,yy,mm,dd
.                   pack      str10 from mm,slash,dd,slash,cc,yy
.                   setprop sheet.range(ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
.                   pack            Exrange1 from "I",DimRow1
.                   setprop sheet.range(ExRange1),*Value=Ownnum
.                   pack            Exrange1 from "J",DimRow1
.                   setprop sheet.range(ExRange1),*Value=OWNOCPY
.                   pack            Exrange1 from "K",DimRow1
.                   setprop sheet.range(ExRange1),*Value=Commper
.                   pack            Exrange1 from "L",DimRow1
.                   if (PSTATUS = "WTHDRN")
.                             move      "Withdrawn",str25
.                   elseif (PSTATUS = "TMPWDN")
.                             move      "Temporarily Withdrawn",str25
.                   else
.                             clear     str25
.                   endif
.                   setprop sheet.range(ExRange1),*Value=str25
.                   pack            Exrange1 from "M",DimRow1
.                   setprop sheet.range(ExRange1),*Value=Password
.................................
                    pack            Exrange1 from "D",DimRow1
                    setprop sheet.range(ExRange1),*Value=UNiverse,*NumberFormat="##,####0"
                    pack            Exrange1 from "E",DimRow1
                    unpack    revdate into cc,yy,mm,dd
                    pack      str10 from mm,slash,dd,slash,cc,yy
                    setprop sheet.range(ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
.                    pack            Exrange1 from "F",DimRow1
.                    unpack    NDATUPDDATE into cc,yy,mm,dd
.                    pack      str10 from mm,slash,dd,slash,cc,yy
.                    setprop sheet.range(ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    pack            Exrange1 from "F",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=text1
.                    setprop sheet.range(ExRange1),*Value=Ownnum
                   pack            Exrange1 from "H",DimRow1
.                    pack            Exrange1 from "H",DimRow1
.                    setprop sheet.range(ExRange1),*Value=OWNOCPY
                    pack            Exrange1 from "G",DimRow1
                    setprop sheet.range(ExRange1),*Value=Commper
                    pack            Exrange1 from "H",DimRow1
                    if (STATUS = "W")
                              move      "Withdrawn",str25
                    elseif (STATUS = "T")
                              move      "Temporarily Withdrawn",str25
                    else
                              clear     str25
                    endif
                    setprop sheet.range(ExRange1),*Value=str25
.                    pack            Exrange1 from "K",DimRow1
.                    setprop sheet.range(ExRange1),*Value=Password
.END PATCH 4.01 REPLACED LOGIC
                    Add       c1 to row1
                    Add       c1 to row2

.START PATCH 4.01 ADDED LOGIC
                    getprop   sheets,*Count=N8
                    sub       C1,N8
                    if (N8 > 1)
                              setprop   SheetIndex,VarValue=N8
                              getprop   sheets,*Item(SheetIndex)=sheet
                              Sheets.item giving sheet using Sheetindex
.
                              pack      Exrange1 from "D1"
.What is happening in next line of code is a pulling of a range automation object to feed to the Hyperlinks.Add method
.We select the Range we are working with, and then select the range within that range.  This is why the following code is
.in place:  *range("A1").  If we used:  *range(ExRange1) we would have an offset of whatever is in ExRange1...
                              getprop   sheet.range(ExRange1),*range("A1")=xlrange1
                              clear     str1
                              sheet.Hyperlinks.Add using *Anchor=xlrange1, *Address=str1, *SubAddress=NextLink, *TextToDisplay="Next"
                              setprop sheet.range(Exrange1).Font,*Name="Times New Roman", *Size=8
                    endif
.END PATCH 4.01 ADDED LOGIC
          endif
.end patch 4.0



.                   sheet.range(EXRange3).Merge

.         WRITE     OUTPUT,seq;*cdfon,LSTNUM,mlstname,universe,text1:
.                   price,revdate:
.;                  price,NDATUPDDATE:
.                   mdlplan,mdlcall,STR24

.END PATCH 3.5 REPLACED LOGIC
.         ,NUMBER,",#"",LDESC:
.                   "#",",PRICE,",#"",PSTATUS,"#",",JULDAYS
LOAD
.begin patch 3.0
         if        (htmlflag <> 2)
         BRANCH    PRSW OF LINE1,LINE2,LINE3,LINE4,LINE5
         ENDIF
.begin patch 3.4
         Move      mlstname to str55
         rep       uplow in str55
         scan      "office use" in str55
         goto      read if equal
.end patch 3.4

         scan      "Office Use" in mlstname
         goto      read if equal
         Reset      Mlstname
         scan      "OFFICE USE" in mlstname
         goto      read if equal
         Reset      Mlstname
         scan      "Office use" in mlstname
         goto      read if equal
         Reset      Mlstname
.Patch3.7
                              goto                 read if (NDATOFF = "1")
.patch3.8
                              goto                 read if (NDATWEB = "1")
.patch3.8
.Patch3.7
.         WRITE     HTMLFILE,SEQ;"<a href=#"Datacards/data",LSTNUM,".htm#">",MLSTNAME:
.                   TEXT1,"</a><br>"
.begin patch 3.1
         scan   "|A" in mlstname
         call   undofixita if equal
         reset  Mlstname
         scan   "|T" in mlstname
         call   undofixitThe if equal
         reset  Mlstname
.end patch 3.1
.Patch 3.9
.         WRITE     HTMLFILE,SEQ;"<a href=#"Datacards/data",LSTNUM,".htm#">",MLSTNAME:
.                   "</a><br>"
.>Patch 3.8.7 Comment Out
.         WRITE     HTMLFILE,SEQ;"<a href=#"http://www.nincal.COM/DATACARDS/data",LSTNUM,".htm#">",MLSTNAME:
.                   "</a><br>"
.>Patch 3.8.7 Comment Out
.>Patch 3.8.7 Code Added Modified
          WRITE     HTMLFILE,SEQ;"                <tr>"
.begin Patch 3.95
.          WRITE     HTMLFILE,SEQ;"                          <td width=#"425#">"
          WRITE     HTMLFILE,SEQ;"                          <td width=#"525#">"
          WRITE     HTMLFILE,SEQ;"                <li>"
          WRITE     HTMLFILE,SEQ;"<class=#"MsoNormal#"><span style=#"font-size:11.0pt;color:navy#">"
          WRITE     HTMLFILE,SEQ;"<a style=#"text-decoration: underline; text-underline: single#"  href=#"/Datacards/data",LSTNUM,".htm#">",MLSTNAME:
                               "</a>"
.                               "</a>"</span></li>"


.          WRITE     HTMLFILE,SEQ;"                          <a target=#"_blank#"   <a href=#"/Datacards/data",LSTNUM,".htm#"><PRE>",MLSTNAME,"</PRE>":
.                                 "</a><br>"
.          WRITE     HTMLFILE,SEQ;"                          </td>"
.          WRITE     HTMLFILE,SEQ;"                                    <td align=#"center#" width=#"45#" valign=#"top#">"

.end Patch 3.95

          pack      NUSGFLD1,"01X",LSTNUM
          move      "NUSGAIM",Location
          pack      KeyLocation,"Key: ",NUSGFLD1
          call      NUSGAIM
          If Not over
                    if (NDATLUSAGE <> "F")
.begin Patch 3.95
                    WRITE     HTMLFILE,SEQ;"&nbsp;&nbsp;&nbsp;&nbsp;","                                              <a target=#"_blank#" title=#"Click to see Usage#" href=#"/usage/usg",lstnum,".pdf#">Usage</a></span></li>"
.                    WRITE     HTMLFILE,SEQ;"                                              <a target=#"_blank#" title=#"Click to see Usage#" href=#"/usage/usg",lstnum,".pdf#">Usage</a>"
..                   WRITE     HTMLFILE,SEQ;"                                              <img border=#"0#" src=#"/images/document.gif#" width=#"15#" height=#"13#"></a>"
                    Else
                    WRITE     HTMLFILE,SEQ;"</span></li>"
.end Patch 3.95
                    Endif
          Endif
          WRITE     HTMLFILE,SEQ;"                                    </td>"
          WRITE     HTMLFILE,SEQ;"                </tr>"
.>Patch 3.8.7 Code Added Modified
.Patch 3.9
         goto      read
.end patch 3.0
LINE1
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE1 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
          MOVE      MLSTNAME,STR55
.Patch3.6
                              pack str1,password
                              scan b1 in password
                              bump password
                              move str1 to str2
                              pack str1,password
                              pack str3,str2,str1
          if        (TypeFlag <> c2)
                              move str3,str31a
          endif
                              call trim using pstatus
                              call trim using EXCL
                              if (excl = "")
                                        move pstatus to str51a
                              else
                                        move excl to str51a
                              endif

         PACK      LINE1 FROM LSTNUM,B3,STR55,B5:
                   TEXT1
.                   TEXT1,B1,str3,PSTATUS,b1,EXCL
.patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE1A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE1B
.         CALL      TRIM USING NDATUPDDATE
          CALL      TRIM USING REVDATE
.         IF (NDATUPDDATE = "")
          IF (REVDATE = "")
                    CLEAR     STR10
          ELSE
                    UNPACK    REVDATE,CC,YY,MM,DD
.                   UNPACK    NDATUPDDATE,CC,YY,MM,DD
                    PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          ENDIF
          MOVE      STR10,LINE1B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
.
LINE2
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE2 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
          MOVE      MLSTNAME,STR55
.Patch3.6
                              pack str1,password
                              scan b1 in password
                              bump password
                              move str1 to str2
                              pack str1,password
                              pack str3,str2,str1
          if        (TypeFlag <> c2)
                              move str3,str32a
          Endif
                              call trim using pstatus
                              if (excl = "")
                                        move pstatus to str52a
                              else
                                        move excl to str52a
                              endif
         PACK      LINE2 FROM LSTNUM,B3,STR55,B5:
.                   TEXT1,B1,str3,PSTATUS,b3,EXCL
                   TEXT1
.;Patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE2A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE2B
.         CALL      TRIM USING NDATUPDDATE
          CALL      TRIM USING REVDATE
          IF (REVDATE = "")
.         IF (NDATUPDDATE = "")
                    CLEAR     STR10
          ELSE
.                   UNPACK    NDATUPDDATE,CC,YY,MM,DD
                    UNPACK    REVDATE,CC,YY,MM,DD
                    PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          ENDIF
          MOVE      STR10,LINE2B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
.
LINE3
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE3 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
          MOVE      MLSTNAME,STR55
.Patch3.6
                              pack str1,password
                              scan b1 in password
                              bump password
                              move str1 to str2
                              pack str1,password
                              pack str3,str2,str1
          if        (TypeFlag <> c2)
                              move str3,str33a
          endif
                              call trim using pstatus
                              call trim using EXCL
                              if (excl = "")
                                        move pstatus to str53a
                              else
                                        move excl to str53a
                              endif
         PACK      LINE3 FROM LSTNUM,B3,STR55,B5:
.                   TEXT1,B1,str3,PSTATUS,b3,EXCL
                   TEXT1
.Patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE3A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE3B
          CALL      TRIM USING REVDATE
          IF (REVDATE = "")
                    CLEAR     STR10
          ELSE
                    UNPACK    REVDATE,CC,YY,MM,DD
                    PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          ENDIF
          MOVE      STR10,LINE3B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
.
LINE4
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE4 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
          MOVE      MLSTNAME,STR55
.Patch3.6
                              pack str1,password
                              scan b1 in password
                              bump password
                              move str1 to str2
                              pack str1,password
                              pack str3,str2,str1
          if        (TypeFlag <> c2)
                              move str3,str34a
           endif
                              call trim using pstatus
                              call trim using EXCL
                              if (excl = "")
                                        move pstatus to str54a
                              else
                                        move excl to str54a
                              endif
         PACK      LINE4 FROM LSTNUM,B3,STR55,B5:
.                   TEXT1,B1,str3,PSTATUS,b3,EXCL
                   TEXT1
.patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE4A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE4B
          CALL      TRIM USING REVDATE
          IF (REVDATE = "")
                    CLEAR     STR10
          ELSE
                    UNPACK    REVDATE,CC,YY,MM,DD
                    PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          ENDIF
          MOVE      STR10,LINE4B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
.
LINE5
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE5 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
          MOVE      MLSTNAME,STR55
.Patch3.6
                              clear str1
                              clear str2
                              clear str3
                              pack str1,password
                              scan b1 in password
                              bump password
                              move str1 to str2
                              pack str1,password
                              pack str3,str2,str1
          if        (TypeFlag <> c2)
                              move str3,str35a
          endif
                              call trim using EXCL
                              call trim using pstatus
                              if (excl = "")
                                        move pstatus to str55a
                              else
                                        move excl to str55a
                              endif
         PACK      LINE5 FROM LSTNUM,B3,STR55,B5:
                   TEXT1
.                   TEXT1,B1,str3,PSTATUS,b3,EXCL,b1

.patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE5A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE5B
          CALL      TRIM USING REVDATE
          IF (REVDATE = "")
                    CLEAR     STR10
          ELSE
                    UNPACK    REVDATE,CC,YY,MM,DD
                    PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          ENDIF
          MOVE      STR10,LINE5B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
.
LOADEXIT
         MOVE      B4 TO EXCL
         DISPLAY   *P1:23,*EL;
         ADD       c1 TO lines
         COMPARE   "5" TO PRSW
         GOTO      DETOUT IF EQUAL
         GOTO      DETOUT IF NOT LESS
         GOTO      READ
* ...........................................................
. HEADING AND DETAIL OUTPUT
.
DETOUT   DISPLAY   *P1:23,*EL,*HON,"PRINTING";
         compare    c2 to updflag
         if        equal
         COMPARE   "52" TO lines
         CALL      HEADER IF NOT LESS
         else
         COMPARE   "57" TO lines
         CALL      HEADER IF NOT LESS
         endif
         MOVE      C0,PRSW
         BRANCH    UPDFLAG OF DT1,DT2
DT1      PRINT     *L,*1,LINE1,LINE1A:
                   *118,str31a:
                                                             *121,str51a
         PRINT     *1,LINE2,LINE2A:
                   *118,str32a:
                                                             *121,str52a
         PRINT     *1,LINE3,LINE3A:
                   *118,str33a:
                                                             *121,str53a
         PRINT     *1,LINE4,LINE4A:
                   *118,str34a:
                                                             *121,str54a
         PRINT     *1,LINE5,LINE5A:
                   *118,str35a:
                                                             *121,str55a;
         PRINT     *FLUSH;
         GOTO      BLANK
DT2      PRINT     *L,*1,LINE1,LINE1A,*134,LINE1B
         PRINT     *1,LINE2,LINE2A,*134,LINE2B
         PRINT     *1,LINE3,LINE3A,*134,LINE3B
         PRINT     *1,LINE4,LINE4A,*134,LINE4B
         PRINT     *1,LINE5,LINE5A,*134,LINE5B;
         PRINT     *FLUSH;
         GOTO      BLANK
.
BLANK    MOVE      BLANK127,LINE1
         MOVE      BLANK127,LINE2
         MOVE      BLANK127,LINE3
         MOVE      BLANK127,LINE4
         MOVE      BLANK127,LINE5
         MOVE      B5 TO LINE1A
         MOVE      B5 TO LINE2A
         MOVE      B5 TO LINE3A
         MOVE      B5 TO LINE4A
         MOVE      B5 TO LINE5A
         MOVE      B1 TO LINE1B
         MOVE      B1 TO LINE2B
         MOVE      B1 TO LINE3B
         MOVE      B1 TO LINE4B
         MOVE      B1 TO LINE5B
                              move b3 to                    str31a
                              move b3 to                    str32a
                              move b3 to                    str33a
                              move b3 to                    str34a
                              move b3 to                    str35a
                              move b5 to  str51a
                              move b5 to  str52a
                              move b5 to  str53a
                              move b5 to  str54a
                              move b5 to  str55a
         DISPLAY   *P1:23,*EL;
*............................................................
. TURN OFF R.I.D. & ALL L-INDICATORS
.
TURNOFF
         COMPARE   ONE TO LR
         GOTO      RETURN IF EQUAL
         NORETURN
*............................................................
. GO READ ANOTHER RECORD
.
         GOTO      READ
* ...........................................................
. GOTO PRINT TOTAL COUNT
RETURN
         RETURN
*............................................................
. PAGE HEADING ROUTINE
.
PAGE     ADD       ONE TO PAGE
.begin patch 3.0
         if        (htmlflag = 2 or xlsxflag < 2)
         return
         endif
.end patch 3.0
.start patch 4.0
           if (xlsxflag = 3)
           goto     hdxlsx
           endif
.end patch 4.o
         BRANCH    UPDFLAG OF PAGE1,PAGE2
PAGE1    BRANCH    LASRFLAG TO PAGE1B,PAGE1A
PAGE1A   COMPARE   C1 TO PAGE
         IF        EQUAL
         compare   c2 to duplflag
           if      equal
           compare  c2 to updflag
           if       equal
.begin patch "2.4"
.         PRINT     HPTMSR17,hpdupl,hptop,hpland
         PRINT     HPTMSR17,hpdupl,hptop,hpland,033,"&l4H"
           else
.         PRINT     HPTMSR17,hpdupl,hptop,*F
         PRINT     HPTMSR17,hpdupl,hptop,033,"&l4H",*F
.end patch "2.4"
           endif
           else
           compare  c2 to updflag
           if       equal
.begin patch "2.4"
         PRINT     HPTMSR17,hptop,hpland,033,"&l4H"
.         PRINT     HPTMSR17,hptop,hpland
           else
.         PRINT     HPTMSR17,hptop,*F
         PRINT     HPTMSR17,hptop,033,"&l4H",*F
.end patch "2.4"
         ENDIF
         endif
         endif
PAGE1B
         PRINT     *f,*C,*L:
                   *1,"CONFIDENTIAL":
                   *50,"*** NAMES IN THE NEWS MASTER LISTING ***":
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         MOVE      c5 TO lines
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN
PAGE2    BRANCH    LASRFLAG TO PAGE2B,PAGE2A
PAGE2A   COMPARE   C1 TO PAGE
         IF        EQUAL
         compare   c2 to duplflag
           if      equal
           compare   c2 to updflag
           if      equal
         PRINT     HPTMSR17,hpdupl,hptop,hpland,hplin8,*f
           else
         PRINT     HPTMSR17,hpdupl,hptop,*F
         endif
           else
           compare  c2 to updflag
           if       equal
         PRINT     HPTMSR17,hptop,hpland,hplin8,*f
           else
         PRINT     HPTMSR17,*F
         endif
         ENDIF
         endif
         GOTO      PAGE2X
PAGE2B   PRINT     *F,033,"M";
PAGE2X   PRINT     *f,*C,*L:
                   *1,"CONFIDENTIAL":
                   *50,"*** NAMES IN THE NEWS MASTER LISTING ***":
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         MOVE      c5 TO lines
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN
HEADER
.begin patch 3.0
         if        (htmlflag = 2)
         return
         endif
.end patch 3.0
         CALL       PAGE
         BRANCH    UPDFLAG OF HD1,HD2
HD1       if        (Typeflag <> c2)
          PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE":
                   *118,"Typist";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                   *120,"______":
                    *C,*L
          else
          PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                    *C,*L
          endif
.begin patch 4.0
.         branch    XLSXFlag of hdexit,hd3
         branch    XLSXFlag of hdexit,hd3,hdxlsx
.end patch 4.0
HD2      PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE",*134,"UPDATED";
.                   *107,"PRICE",*134,"REVISED";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                   *134,"_______":
                    *C,*L                                        ; ."
.              
.begin patch 4.0
.         branch    XLSXFlag of hdexit,hd3
         branch    XLSXFlag of hdexit,hd3,hdxlsx
.end patch 4.0
hdexit   return
HD3      COMPARE   C1 TO PAGE
         GOTO      HDEXIT IF NOT EQUAL
.begin patch 4.0
          MOve            "11",Row1
          move            "12",row2
.end patch 4.0
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        create   xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
        create   xlShiftUp,VarType=VT_R8,VarValue="-4162"
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8A,VarValue="0.46"
        create xlColumnWidthA,VarType=VT_R8,VarValue="64.00"
        create xlColumnWidthB,VarType=VT_R8,VarValue="12.00"
        create xlColumnWidthCats,VarType=VT_R8a,VarValue="24.00"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
.        setprop ex,*Visible="True"
              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45

              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin
              setprop sheet.range("b6","B6"),*Value="List ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b6:b6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b6:b6").Font,*Bold="True"
              setprop sheet.range("C6","C6"),*Value="Master List Name",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("C6:C6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("C6:C6").Font,*Bold="True"
              setprop sheet.range("F6","f6"),*Value="Quantity",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("f6:f6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("f6:f6").Font,*Bold="True"
              setprop sheet.range("g6","g6"),*Value="Description",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("g6:g6").Font,*Bold="True"
              setprop sheet.range("h6","h6"),*Value="Price",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h6:h6").Font,*Bold="True"
              setprop sheet.range("i6","i6"),*Value="Updated",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("i6:i6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("i6:i6").Font,*Bold="True"
              setprop sheet.range("j6","j6"),*Value="Revised",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("j6:j6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("j6:j6").Font,*Bold="True"

              setprop sheet.range("k6","k6"),*Value="Planner",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k6:k6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k6:k6").Font,*Bold="True"

              setprop sheet.range("l6","l6"),*Value="Caller",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l6:l6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("l6:l6").Font,*Bold="True"

              setprop sheet.range("n6","n6"),*Value="Owner ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("n6:n6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("n6:n6").Font,*Bold="True"

              setprop sheet.range("o6","o6"),*Value="Owner ",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("o6:o6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("o6:o6").Font,*Bold="True"

              setprop sheet.range("p6","p6"),*Value="Fulfillment",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("p6:p6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("p6:p6").Font,*Bold="True"

              setprop sheet.range("q6","q6"),*Value="Comm.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("q6:q6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("q6:q6").Font,*Bold="True"

              setprop sheet.range("r6","r6"),*Value="Status",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("r6:r6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("r6:r6").Font,*Bold="True"

              setprop sheet.range("s6","s6"),*Value="Typist",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("s6:s6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("s6:s6").Font,*Bold="True"

              setprop sheet.range("T5","T5"),*Value="Web Reco",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("T5:T5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("T5:T5").Font,*Bold="True"

              setprop sheet.range("T6","T6"),*Value="Categories",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("T6:T6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("T6:T6").Font,*Bold="True"

.        WRITE     OUTPUT,SEQ;*cdfon,"#"CONFIDENTIAL#"":
.                     "#"NAMES IN THE NEWS#"":
.                     "#"",TODAY,"#""
.         .WRITE      OUTPUT,SEQ;*cdfon,B2,"#"LIST###"":
.                   "#"MASTER LIST NAME#"":
.                   B6,B6,B6,"#"QUANTITY#"":
.                   B2,"#"DESCRIPTION#"":
.                   B6,"#"PRICE#"":
.                   B6,"#"UPDATED#""
.                   B6,"#"REVISED#""
        GOTO       HDEXIT
.begin patch 4.0
HDXLSX   COMPARE   C1 TO PAGE
         GOTO      HDEXIT IF NOT EQUAL
.START PATCH 4.01 ADDED LOGIC
          if (XLSCreateFlag = 1)
                    goto HDEXIT
          endif
.END PATCH 4.01 ADDED LOGIC
.begin patch 4.0
.START PATCH 4.01 REPLACED LOGIC
.          MOve            "11",Row1
.          move            "12",row2
          move      "8",Row1
          move      "9",row2
.END PATCH 4.01 REPLACED LOGIC
.end patch 4.0

.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.START PATCH 4.01 ADDED LOGIC
        setprop ex,*DisplayAlerts=OFALSE
.END PATCH 4.01 ADDED LOGIC
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        create   xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
        create   xlShiftUp,VarType=VT_R8,VarValue="-4162"
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
.START PATCH 4.01 REPLACED LOGIC
.        create xlColumnWidthA,VarType=VT_R8,VarValue="64.00"
.        create xlColumnWidthB,VarType=VT_R8,VarValue="12.00"
.        create xlColumnWidthCats,VarType=VT_R8a,VarValue="24.00"
        create xlColumnWidthA,VarType=VT_R8,VarValue="12.00"
        create xlColumnWidthB,VarType=VT_R8,VarValue="36.00"
        create xlColumnWidthCats,VarType=VT_R8a,VarValue="84.00"
.END PATCH 4.01 REPLACED LOGIC
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
        create  Zoom70,VarType=VT_I4,VarValue=70
          create    SheetIndex,VarType=VT_I4                          

              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45

              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin
.START PATCH 4.01 REPLACED LOGIC
.              setprop sheet.range("b6","B6"),*Value="List ##",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("b6:b6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("b6:b6").Font,*Bold="True"
.              setprop sheet.range("C6","C6"),*Value="Master List Name",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("C6:C6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("C6:C6").Font,*Bold="True"
.              setprop sheet.range("F6","f6"),*Value="Quantity",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("f6:f6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("f6:f6").Font,*Bold="True"
.              setprop sheet.range("g6","g6"),*Value="Description",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("g6:g6").Font,*Bold="True"
.              setprop sheet.range("h6","h6"),*Value="Price",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("h6:h6").Font,*Bold="True"
.              setprop sheet.range("i6","i6"),*Value="Updated",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("i6:i6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("i6:i6").Font,*Bold="True"
.              setprop sheet.range("j6","j6"),*Value="Revised",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("j6:j6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("j6:j6").Font,*Bold="True"
.
.              setprop sheet.range("k6","k6"),*Value="Comm.",*HorizontalAlignment=xlAlignCenter
.              setprop sheet.range("k6:k6").Font,*Name="Times New Roman", *Size=14
.              setprop sheet.range("k6:k6").Font,*Bold="True"
                    setprop sheet.range("b6","b6"),*Value="List ##"
                    setprop sheet.range("c6","c6"),*Value="Master List Name"
                    setprop sheet.range("d6","d6"),*Value="Quantity"
                    setprop sheet.range("e6","e6"),*Value="Updated"
.                    setprop sheet.range("f6","f6"),*Value="Revised"
.                    setprop sheet.range("g6","g6"),*Value="LO Num."
                    setprop sheet.range("F6","F6"),*Value="Description"
.                    setprop sheet.range("h6","h6"),*Value="List Owner"
                    setprop sheet.range("G6","G6"),*Value="Comm."
                    setprop sheet.range("H6","H6"),*Value="Status"
.                    setprop sheet.range("k6","k6"),*Value="Updated by"
. 
                    setprop sheet.range("b6","k6"),*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range("b6:k6").Font,*Name="Times New Roman", *Size=12, *Bold="True"
.
                    setprop sheet,*Name="SUMMARY"
                    pack      PreviousLink,"'SUMMARY'!A1"
.
                    move      C1,XLSCreateFlag
.END PATCH 4.01 REPLACED LOGIC

         GOTO       HDEXIT
.end patch 4.0
ZEROLINE MOVE      C0 TO lines
         MOVE      NO TO SW1P
         RETURN
*............................................................
. TOTAL OUTPUT
.
TOTOUT
.begin patch 3.0
.         if        (htmlflag = 2)
.begin patch 4.0
.         if        (htmlflag <> 2)
         if        (htmlflag <> 2 & XlsxFlag < 2)
.end patch 4.0
.         return
.         endif
         TRAPCLR   PARITY  (NOP)
         COMPARE   ONE TO LR
         RETURN    IF NOT EQUAL
         CALL      DETOUT
         COMPARE   "52" TO lines
         CALL      PAGE IF NOT LESS
         MOVE      UNIMASK TO UNIVPRT
         EDIT      TOTUNIV TO UNIVPRT
         DISPLAY   *P1:23,*EL,TOTUNIV,*W
         PRINT     *L,*35,"NUMBER OF RECORDS FOUND ",LRTOT,"****"
         PRINT     *L,*35,"TOTAL UNIVERSE",*61,UNIVPRT
         RETURN
         else
         return
         endif
.end patch 3.0
* ...........................................................
. F5 - JOB INTERUPTED.
.
F5
         TRAPCLR   F5
         TRAP      f5 IF F5
         DISPLAY   *P1:23,*EL,*B,"JOB ABORTED!!!!!",error,*B,*W2;
         NORETURN
         GOTO      EOJ
* ...........................................................
. int - JOB INTERUPTED.
.
int     TRAPCLR   INT
         TRAP      int giving error IF INT
         DISPLAY   *P1:23,*EL,*B,"JOB ABORTED!!!!!",error,*B,*W2;
         NORETURN
         GOTO      EOJ
* ...........................................................
. END OF JOB
EOJ
.begin patch 3.0
.begin patch 4.0
.        if        (htmlflag <> 2)
        if        (htmlflag <> 2 & xlsxflag < 2)
.enc patch 4.0
        BRANCH    UPDFLAG OF NOUPD,UPD
        else
        goto      close
.end patch 3.0
        endif
NOUPD
.begin patch 2.4
         print      *f,033,"&l1H"          .reset paper tray select
.      PRINT     *F
.end patch 2.4
         GOTO      CLOSE
.         CLOSE     INPUT
.begin patch 2.4
.UPD       PRINT     *F,033,"@"                  .RESET TO DEFAULTS
UPD       PRINT     *F,033,"@",033,"&l1H"                  .RESET TO DEFAULTS
.end patch 2.4
CLOSE
.begin patch 3.0
         if        (htmlflag <> 2)
         SPLCLOSE
         RELEASE
         endif
         if        (htmlflag <> 2)
.begin patch 4.0
.         BRANCH    XLSXFlag TO EOJ1,WEOF
         BRANCH    XLSXFlag TO EOJ1,WEOF,WEOF
.end patch 4.0
         else
                              if (HTMLINTRO = "1")
                              elseif (HTMLINTRO = "2")
.>Patch 3.8.7
          if (TableOpen = c1)
           move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
                              elseif (HTMLINTRO = "3")
.>Patch 3.8.7
          if (TableOpen = c1)
           move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7
                              endif
         write     htmlfile,seq;"</tbody>"
         write     htmlfile,seq;"</table>"
         write     htmlfile,seq;"</dd>"
         write     htmlfile,seq;"</dl>"
         write     htmlfile,seq;"<p><input id=#"gwProxy#" type=#"hidden#" /> <input id=#"jsProxy#" onclick=#"jsCall();#" type=#"hidden#" /> <input id=#"gwProxy#" type=#"hidden#" /> <input id=#"jsProxy#" onclick=#"jsCall();#" type=#"hidden#" /></p>"
         write     htmlfile,seq;"<div class=#"addtoany_share_save_container#"><ul class=#"addtoany_list#"><li><a class=#"a2a_dd addtoany_share_save#" href=#"http://www.addtoany.com/share_save?linkurl=http%3A%2F%2Fwww.nincal.com%2Findex.php%2Flist-management%2Fdata-card-library%2F&amp;linkname=Data%20Card%20Library#"><img src=#"http://www.nincal.com/wp-content/plugins/add-to-any/share_save_171_16.png#" width=#"171#" height=#"16#" alt=#"Share/Bookmark#"/></a></li></ul>"
         write     htmlfile,seq;"<script type=#"text/javascript#">"
         write     htmlfile,seq;"var a2a_config = a2a_config || {};"
         write     htmlfile,seq;"a2a_config.linkname=#"Data Card Library#";"
         write     htmlfile,seq;"a2a_config.linkurl=#"http://www.nincal.com/index.php/list-management/data-card-library/#";"
         write     htmlfile,seq;"</script><script type=#"text/javascript#" src=#"http://static.addtoany.com/menu/page.js#"></script>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"<p>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</p>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;"<div id=#"sidebar#">"
         write     htmlfile,seq;"<ul id=#"sidelist#">"
         write     htmlfile,seq;"<div class=#"sidebar_widget#"><form method=#"get#" id=#"searchform#" action=#"http://www.nincal.com/#">"
         write     htmlfile,seq;"<div><input type=#"text#" value=#"#" name=#"s#" id=#"s#" class=#"searchfield#" />"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"</form>"
         write     htmlfile,seq;"</div><div class=#"sidebar_widget#"><h3>Stay Informed:</h3>                       <div class=#"textwidget#"><a href=#"http://visitor.constantcontact.com/manage/optin/ea?v=001Qo1SzxA2oRQ-aZ5HD9z05Q%3D%3D#" target=#"_blank#" ><img src=#"http://64.71.27.110/images/newsletter.jpg#" height=#"45#" width=#"95#" style=#"border: 0pt none; margin: 0px;#"></a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"<a href=#"http://www.facebook.com/namesinthenews#"target=#"_blank#"><img src=#"http://64.71.27.110/images/facebook.jpg#"  height=#"45#" width=#"95#" style=#"border: 0pt none; margin: 0px;#"> </a><a href=#"http://www.twitter.com/#!/NamesintheNews#" target=#"_blank#">"
         write     htmlfile,seq;"<img src=#"http://64.71.27.110/images/twitter.jpg#"  height=#"45#" width=#"95#" style=#"border: 2pt; margin: 0px;#"> </a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;"<!-- BEGIN: Constant Contact Basic Opt-in Email List Form -->"
         write     htmlfile,seq;"<div align=#"center#">"
         write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"3#" bgcolor=#"#ffffff#" style=#"border:2px solid #ffffff;#">"
         write     htmlfile,seq;"<tr>"
         write     htmlfile,seq;"<td align=#"center#" style=#"font-weight: bold; font-family:Verdana; font-size:12px; color:2858a6;#">Join Our Mailing List</td>"
         write     htmlfile,seq;"</tr>"
         write     htmlfile,seq;"<tr>"
         write     htmlfile,seq;"<td align=#"center#" style=#"border-top:2px solid #ffffff#">"
         write     htmlfile,seq;"<form name=#"ccoptin#" action=#"http://visitor.constantcontact.com/d.jsp#" target=#"_blank#" method=#"post#" style=#"margin-bottom:2;#">"
         write     htmlfile,seq;"<input type=#"hidden#" name=#"m#" value=#"1101942235316#">"
         write     htmlfile,seq;"<input type=#"hidden#" name=#"p#" value=#"oi#">"
         write     htmlfile,seq;"<font style=#"font-weight: normal; font-family:Verdana; font-size:10px; color:2858a6;#">Email:</font> <input type=#"text#" name=#"ea#" size=#"20#" value=#"#" style=#"font-size:10pt; border:1px solid #999999;#">"
         write     htmlfile,seq;"<input type=#"submit#" name=#"go#" value=#"Go#" class=#"submit#" style=#"font-family:Verdana,Geneva,Arial,Helvetica,sans-serif; font-size:10pt;#">"
         write     htmlfile,seq;"</form>"
         write     htmlfile,seq;"</td>"
         write     htmlfile,seq;"</tr>"
         write     htmlfile,seq;"</table>"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"<!-- END: Constant Contact Basic Opt-in Email List Form -->"
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;""
         write     htmlfile,seq;"<a href=#"/index.php/clients/#" ><img src=#"http://64.71.27.110/images/clientlogos.gif#" style=#"border: 0pt none; margin: 0px;#"></a></div>"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"</ul>"
         write     htmlfile,seq;"<!--END SIDELIST-->"
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"<!--END SIDEBAR-->"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"<div id=#"footer#">"
         write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-includes/js/jquery/jquery.form.js?ver=2.02m'></script>"
         write     htmlfile,seq;"<script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/contact-form-7/scripts.js?ver=2.3'></script>"
         write     htmlfile,seq;"<div> &##169; 2013"
         write     htmlfile,seq;"Names in the News    | 180 Grand Avenue, Suite 1545, Oakland, CA 94612 | T: 415-989-3350 | F: 415-433-7796 |  <div></div>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</div>"
         write     htmlfile,seq;"</div>"
.begin patch 3.91
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"var gaJsHost = ((#"https:#" == document.location.protocol) ? #"https://ssl.#" : #"http://www.#");"
                              Write     htmlfile,seq;"document.write(unescape(#"%3Cscript src='#" + gaJsHost + #"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E#"))"
                              Write     htmlfile,seq;"</script>"
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"try {var pageTracker = _gat._getTracker(#"UA-6716303-4#");pageTracker._trackPageview();"
                              Write     htmlfile,seq;"} catch(err) {}</script>"
.end patch 3.91
         write     htmlfile,seq;"</body>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</html>"
         weof      htmlfile,seq
         CLOSE     htmlfile,EOFSIZE
         goto      eoj1
         endif
.end patch 3.0

WEOF
.         WEOF      OUTPUT,SEQ
.                   CLOSE     OUTPUT,EOFSIZE


          pack            Exrange1 from "B6"
          pack            Exrange2 from "S",DimRow2

          sheet.range(EXrange1,EXrange2).Columns.Autofit
         Setprop Sheet.Range("T1"),*ColumnWidth=xlColumnWidthCats

.START PATCH 4.01 ADDED LOGIC
          if (xlsxflag = 3)
.Put focus back on Summary sheet
                    Sheets.item giving sheet using 1
                    sheet.activate
                    pack      Exrange1,"B6"
                    pack      Exrange2,"M",DimRow2
                    sheet.range(EXrange1,EXrange2).Columns.Autofit
          endif
.END PATCH 4.01 ADDED LOGIC
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"\\nins1\e\data\",Prtname
./////
          pack      taskname,"c:\work\"                     ."
          setprop ex,*DefaultFilePath=taskname
          pack      taskname,"c:\work\",pRTNAME                       ."
.begin 3.92 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.end 3.92 get exel version info

............................................
SaveAsFileNameSelect
          setmode *mcursor=*arrow
.          ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.                              append  ".xls",taskname
              clear   taskname
                              if        (#ver = c1)
                              pack            Taskname,"c:\work\",Prtname,".xlsx"
                              else
                              pack            Taskname,"c:\work\",Prtname,".xls"
                              endif                  
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapSaveAsObject if Object
.                book.saveas giving N9 using *Filename=taskname
.                trapclr Object
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapSaveAsObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="FALSE"

..............................................................................................................
.CleanUp
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        Destroy xlRowHeight
        Destroy xlColumnWidth
        Destroy xlColumnWidthCats
        Destroy OTRUE
        Destroy OFALSE
        Destroy TopMargin
        Destroy BottomMargin
        Destroy LeftMargin
        Destroy RightMargin
              setprop ex,*DisplayAlerts=OTRUE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.

        ex.quit
        destroy ex
.Email new XLS to User
                              move    "Here is your List File in Excel",MailSubjct
                              pack    MailBody,"Input File:  ",INPNAME
                              pack      MailBOdy,"Input File:  ",INPNAME
          if        (func = "1")
                              Pack      MailTO from User,"@nincal.com"
          elseif    (func = "2")
                              Pack      MailTO from "SuzieMcGuire@nincal.com,PiaPayne@nincal.com"
                              move    "Here is your Weekly List File in Excel",MailSubjct
          endif
                              Pack      MailFrom from User,"@nincal.com"
                              Pack      MailAttach from taskname
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

.end patch 3.95
                              call      SendMail

EOJ1
         MOVE      "0.00",LRTOT
         MOVE      C0 TO TOTUNIV
         MOVE      "000",PAGE
         MOVE      C0,lines
         MOVE      C0,LR
         MOVE      YES TO STR1
         CLOSE     INPUT
         shutdown  "cls"
.         STOP
*...........................................................................
.list name begins with 'A '
UndoFixitA
        clear     str55
.
        bump      mlstname by -1
        lenset    mlstname
        reset     mlstname
        append    "A " to str55
        append    mlstname to str55
        reset     str55
        clear     Mlstname
        move      str55 to mlstname
        return
.
.list name begins with 'The '
UndoFixitThe
        clear     str55
        bump      mlstname by -1
        lenset    mlstname
        reset     mlstname
        append    "The " to str55
        append    mlstname to str55
        reset     str55
        clear     Mlstname
        move      str55 to mlstname
        return
*...........................................................................
. KILLFLE KILL PRINT FILE
KILLFLE
.         PREPARE   INPUT,NAME
.         CLOSE     INPUT,DELETE
          DISPLAY   *P10:23,*EL,NAME," Has been deleted",*B;
.         GOTO      DOMORE
         shutdown   "CLS"
         STOP
*............................................................
. IO ERROR
.
IO       DISPLAY   *P1:1,*ES,"IO error ",error,*B,*B ;
         trapclr   io
         trap      io giving error if io
         GOTO      INPNG

SelectLoadModifier
          pack      NMODFLD,NSELDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                    pack      str25,dim9a,NMODDESC
                    call trim using str25
          else
                    pack      str25,"+",dim9a,NMODDESC
                    call trim using str25
          endif
          return
TrapSaveAsObject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
.
.
        scan    "Cannot access",str55
        if equal
..Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
        endif
..Send them back to select another File name and try to Save again.
        goto SaveAsFileNameSelect
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Ndat0005 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
                    goto      checkfile
.begin patch 4.00
SheetSetup
.START PATCH 4.01 ADDED LOGIC
          clear     rentflag
.END PATCH 4.01 ADDED LOGIC
          Setprop sheet.PageSetup,*Zoom=Zoom70
          Move      "46" to ZoomRowMax
          Move      "54" to ZoomRowMaxPage1            
          Setprop sheet.PageSetup,*TopMargin=TopMargin
          Setprop sheet.PageSetup,*BottomMargin=BottomMargin
          Setprop sheet.PageSetup,*FooterMargin=TopMargin
          Setprop sheet.PageSetup,*LeftMargin=LeftMargin
          Setprop sheet.PageSetup,*RightMargin=LeftMargin     
./This is not important at this point - return pagebreak collection        
          Getprop sheet,*Hpagebreaks=HpageBreaks
./Sets these column widths to zero
.need to add more goodies
          Setprop Sheet.Range("A1"),*ColumnWidth=xlColumnWidthA
          Setprop Sheet.Range("B1"),*ColumnWidth=xlColumnWidthB
          Setprop Sheet.Range("C1"),*ColumnWidth=xlColumnWidthA
          Setprop Sheet.Range("D1"),*ColumnWidth=xlColumnWidthCats
          pack      Exrange1 from "D1"
          pack      Exrange2 from "D1"
          pack      Exrange3,Exrange1,":",Exrange2
.START PATCH 4.01 REPLACED LOGIC
.         setprop sheet.range(ExRange1,ExRange1),*Value="NEXT"
.         setprop sheet.range(Exrange3).Font,*Name="Times New Roman", *Size=14
          setprop sheet.range(Exrange3).Font,*Name="Times New Roman", *Size=8
          setprop sheet.range(ExRange1,ExRange1),*HorizontalAlignment=AlignRight
.
          pack      Exrange1 from "A1"
.What is happening in next line of code is a pulling of a range automation object to feed to the Hyperlinks.Add method
.We select the Range we are working with, and then select the range within that range.  This is why the following code is
.in place:  *range("A1").  If we used:  *range(ExRange1) we would have an offset of whatever is in ExRange1...
          getprop   sheet.range(ExRange1),*range("A1")=xlrange1
          clear     str1
          sheet.Hyperlinks.Add using *Anchor=xlrange1, *Address=str1, *SubAddress=PreviousLink, *TextToDisplay="Previous"
          setprop sheet.range(Exrange1).Font,*Name="Times New Roman", *Size=8
.END PATCH 4.01 REPLACED LOGIC
          setprop sheet.range(Exrange3).Font,*Bold="True"

          pack      Exrange1 from "A2"
          pack      Exrange2 from "D2"
          pack      Exrange3,Exrange1,":",Exrange2
          clear     taskname
          call      Trim using MLSTNAME
          append    MLSTNAME,taskname
          if (NDATOFF = "1")
                    append    " - Office Use Only",taskname
          endif
.WithDrawn / Temporary Withdrawn????
          if (STATUS = "W")
                    append    LineFeed,taskname
                    append    "**WITHDRAWN**",taskname
          elseif (STATUS = "T")
                    append    LineFeed,taskname
                    append    "**TEMPWITH**",taskname
          else
.No then is it New and/or exclusive?
                    if (NLSTCDE = YES)
                              if (ELSTCDE = "C" or Elstcde = "P")
                              else
                                        append    LineFeed,taskname
                                        append    "**NEW**",taskname
                              Endif
                    endif
          endif
          reset     taskname
          setprop sheet.range(ExRange1,ExRange1),*Value=taskname,*HorizontalAlignment=xlAlignCenter
          sheet.range(EXRange3).Merge
          setprop sheet.range(Exrange3).Font,*Name="Times New Roman", *Size=14
          setprop sheet.range(Exrange3).Font,*Bold="True"
          getitem   NINGreen,0,colornum
          setprop   sheet.range(Exrange3).Interior,*Color=colornum

.
.START PATCH 4.01 REMOVED LOGIC
.         pack      Exrange1 from "A4"
.         pack      Exrange2 from "D4"
.         pack      Exrange3,Exrange1,":",Exrange2
.         sheet.range(EXRange3).Merge
.         setprop sheet.range(ExRange1,ExRange1),*Value="Description",*HorizontalAlignment=xlAlignCenter
..                    sheet.range(EXRange3).Merge
.         setprop sheet.range(Exrange3).Font,*Name="Times New Roman", *Size=14
.         setprop sheet.range(Exrange3).Font,*Bold="True"
.END PATCH 4.01 REMOVED LOGIC
.START PATCH 4.01 ADDED LOGIC
          clear     hold2
          call      XLSLoadDetails      .Procedure might load some data into hold2
          if (SelTextFlag = C1)         .Put Base/Sec. Selects in their own cell, so that we can right-justify them
                    reset     hold2
                    call      Trim using hold2
                    pack      str2,NewLine,LineFeed
                    rep       str2,Hold2
.
                    pack      Exrange1,"D4"
                    setprop sheet.range(ExRange1),*Value=Hold2,*WrapText=OTRUE,*HorizontalAlignment=AlignRight
                    sheet.range(ExRange1).Rows.Autofit
                    setprop sheet.range(ExRange1),*VerticalAlignment=xlTop
                    setprop sheet.range(Exrange1).Font,*Name="Courier", *Size=9
          endif
          clear     hold2
.END PATCH 4.01 ADDED LOGIC

          clear     NTXTTEXT
          for N2,C1,"15"
                    pack      NTXTFLD,LSTNUM,N2
                    rep       Zfill,NTxtFLd
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    until over
                    rep       lowup in NTXTTEXT
                    append    NTXTTEXT,hold2
          repeat
          move      c2,Ntxt1path

          for N2,C1,"5"
                    pack      NTXT1FLD1,LSTNUM,N2
                    rep       zfill,ntxt1fld1
                    move      "D.Load-NTXT1KEY",Location
                    pack      KeyLocation,"Key: ",NTXT1FLD1
                    call      debug
                    call      NTXT1KEY
                    until over
                    if (lstnum <> NTXT1LIST)
                              Break    
                    endif
                    call      Padit
          repeat                  
          goto endtxt1

PADIT
          MOVELPTR  NTXT1TEXT,n3
          IF        (N3 < 50)
          PACK      nTXT1TEXT FROM NTXT1TEXT,B1
          GOTO PADIT
          ENDIF
          rep       lowup in ntxt1text
          append         ntxt1text,hold2

.          repeat
          return
endtxt1                 
.START PATCH 4.01 REPLACED LOGIC
.         if (hold2 <> "")
.                   APPEND    B55,HOLD2
.                   reset     hold2
.         ENDIF

.         pack            Exrange1 from "D4"
.         pack            Exrange2 from "D4"
.         pack            Exrange3,Exrange1,":",Exrange2
.         setprop sheet.range(ExRange1),*Value=Hold2,*WrapText=OTRUE




PrintCleanCode
.START PATCH 1.1.7 REPLACED LOGIC - MOVED TO DIFFERENT LOCATION
.         call trim using CLEANCDE
.         if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
.                   if (ROW   >= LASTLINE)
.                             Goto CheckPlacesLeft
.                   endif
.                   prtpage DataCardPrint;*p2150:ROW,*ALIGNMENT=*left,*font=CourierNew12,*ll,"UPDATED: ";
.                   if (CLEANCDE = "C002")
.                             prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,CLNINFO;
.                             add       singlespaced to row
.                   else
.                             Call      DataLoadRefClean
.                             prtpage DataCardPrint;*p3030:ROW,*font=CourierNew12,*ll,NREFDESC;
.                             add       singlespaced to row
.                   endif
.         endif
.         move "3" to CleanCdeFlag
.END PATCH 1.1.7 REPLACED LOGIC - MOVED TO DIFFERENT LOCATION
          call      AdditionalSelects
.
          call      trim using UNITDATA
          if (UNITDATA <> "")
                    rep       lowup in UNITDATA
                    append    UNITDATA,hold2
                    append    newline,hold2
                    append    newline,hold2
          endif
.
PrintNetName
          call trim using NETNAME
          if ((NETNAME <> "0000")&(NETNAME <> ""))
                    append    NewLine,hold2
                    append    NewLine,hold2
                    append    "NET NAMES:  ",hold2
                    if (NETNAME = "N002")
                              append    NETINFO,hold2
                    else
                              call      DataLoadRefNetName
                              append    NREFDESC,hold2
                    endif
          endif
PrintSample
          call trim using SAMPLE
          if ((SAMPLE <> "0000")&(SAMPLE <> ""))
                    append    NewLine,hold2
                    append    NewLine,hold2
                    append    "SAMPLE:  ",hold2
                    call      DataLoadRefSAMPLE
                    append    NREFDESC,hold2
          endif
PrintDelivery
          call trim using DELCODE
          if ((DELCODE <> "0000")&(DELCODE <> ""))
                    append    NewLine,hold2
                    append    NewLine,hold2
                    append    "DELIVERY:  ",hold2
                    call      DataLoadRefDelivery
                    append    NREFDESC,hold2
          endif
AddNotes
          if (SelNotesFlag = YES)
                    append    NewLine,hold2
                    append    NewLine,hold2
                    append    "*See list description for additional information regarding this select.",hold2
          endif
          clear     SELNOTESFLAG
          if (hold2 <> "")
                    reset     hold2
          ENDIF
.
          call      Trim using hold2
          pack      str2,NewLine,LineFeed
          rep       str2,Hold2
          call      Trim using hold2

.In order to get formatting to work we need to populate column 'D' using
.a row value one greater than the left columns.  Then we can apply Autofit,
.and then we can Merge.
          add       C1,leftcol
          move      leftcol,str9
          call      Trim using str9
          pack      Exrange1,"D",str9
          setprop sheet.range(ExRange1),*Value=Hold2,*WrapText=OTRUE
          sheet.range(ExRange1).Rows.Autofit
          setprop sheet.range(ExRange1),*VerticalAlignment=xlTop
.END PATCH 4.01 REPLACED LOGIC
.         sheet.range(EXRange3).Merge

.START PATCH 4.01 ADDED LOGIC
.Retain the number of LineFeeds....
          move      C0,rightcol
          movelptr hold2,N7
          loop
                    add       C1,rightcol
                    scan      LineFeed,hold2
                    if not equal
                              break
                    endif
                    BUMP      hold2,1
                    movefptr hold2,N6
                    UNTIL (N7 = N6)
          repeat
          add       C1,rightcol         .For good measure, and in case the last character was a LineFeed, which would be missed...
.Try to create some symmetry in the spreadsheet...
.         add       rightcol,N9
.         if (leftcol > N9)
.                   move      leftcol,N9
.         endif
          move      rightcol,N9
          if (leftcol > rightcol)
                    move      leftcol,N9
          endif
          move      N9,str9
          call      Trim using str9
          if (SelTextFlag = C1)         .Did we have Base/Sec. Selects sitting in topmost cell?
                    pack      Exrange1,"D5"
          else
                    pack      Exrange1,"D4"
          endif
          pack      Exrange2 from "D",str9
          pack      Exrange3,Exrange1,":",Exrange2
          sheet.range(EXRange3).Merge
.Placing an additional Autofit here WILL shrink the row height, but too much...
.         sheet.range(ExRange3).Rows.Autofit
          setprop sheet.range(Exrange1).Font,*Name="Courier", *Size=9
.
          if (rentFlag = Yes & (ELSTCDE = "C" or Elstcde = "P"))
                    add       C1,N9
                    move      N9,str9
                    call      Trim using str9
                    pack      Exrange1,"D",str9
                    clear     hold2
                    append    hold2,LineFeed
                    append    hold2,LineFeed
                    append    "*Any quantity below minimum is subject to a flat fee plus applicable base, select and additional",hold2
                    append    " charges. On quantities below 5,000 a $100/flat fee is imposed, Below 1,000 the fee is $200/flat.",hold2
                    reset     hold2
                    setprop sheet.range(ExRange1),*Value=Hold2,*WrapText=OTRUE
                    setprop sheet.range(Exrange1).Font, *Size=9
          endif
          add       C1,N9
          move      N9,str9
          call      Trim using str9
          pack      Exrange1,"D",str9
          clear     hold2
          append    hold2,LineFeed
          append    hold2,LineFeed
          append    "Full payment is required on orders not cancelled prior to the maildate. If exchange, status will",hold2
          append    " remain as ordered. Orders cancelled by mailer prior to mail date are subject to a $100.00/flat",hold2
          append    " cancellation fee plus running and shipping charges.",hold2
          append    hold2,LineFeed
          append    hold2,LineFeed
          reset     hold2     
          setprop sheet.range(ExRange1),*Value=Hold2,*WrapText=OTRUE
          setprop sheet.range(Exrange1).Font, *Size=9
.
          pack      ExRange2,"A2"
          sheet.range(ExRange2,ExRange1).BorderAround using *LineStyle=1,*Weight=2
.Footer
          add       C2,N9
          move      N9,str9
          call      Trim using str9
          pack      Exrange1,"D",str9
          sheet.range(Exrange1).Select
          sheet.Pictures.Insert("\\nins1\e\NETUTILS\logocolornotag.JPG").Select
.
          add       C2,N9
          move      N9,str9
          call      Trim using str9
          pack      Exrange1,"B",str9
          pack      taskname,"Direct",LineFeed,"Marketing",LineFeed,"Association"
          setprop sheet.range(ExRange1),*Value=taskname,*WrapText=OTRUE
          setprop sheet.range(Exrange1).Font, *Size=9
.
          sheet.range("A1").Select      .Put focus back on top of worksheet
.END PATCH 4.01 ADDED LOGIC

.Footer
..DMA Section
.          prtpage DataCardPrint;*p5:10010,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Direct",*boldoff;
.          prtpage DataCardPrint;*p5:10130,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Marketing",*boldoff;
.          prtpage DataCardPrint;*p5:10250,*ALIGNMENT=*LEFT,*font=CourierNew8,*ll,*boldon,"Association",*boldoff;
.
.
.          prtpage   DataCardPrint;*units=*HIENGLISH,*Pictrect=*off,*PICT=9910:10695:3000:8000:NINLogo
.





          return
.end patch 4.00
.START PATCH 4.01 ADDED LOGIC
XLSLoadDetails
          move      "4",leftcol
.
          move      leftcol,str9
          call      Trim using str9
          pack      Exrange1,"A",str9
          pack      Exrange2,"C",str9
          pack      Exrange3,Exrange1,":",Exrange2
          call      ConvertDate using revdate,taskname
          setprop sheet.range(ExRange1),*Value=taskname,*WrapText=OTRUE
          sheet.range(EXRange3).Merge
          sheet.range(EXRange3).Rows.Autofit
          setprop sheet.range(EXRange3),*VerticalAlignment=xlTop
.
          call trim using CLEANCDE
          if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
                    add       C1,leftcol
                    move      leftcol,str9
                    call      Trim using str9
                    pack      Exrange1,"A",str9
                    pack      Exrange2,"C",str9
                    pack      Exrange3,Exrange1,":",Exrange2
                    clear     taskname
                    append    "Updated:  ",taskname
                    if (CLEANCDE = "C002")
                              call      Trim using CLNINFO
.                             append    LineFeed,taskname
                              append    CLNINFO,taskname
                    else
                              Call      DataLoadRefClean
                              call      Trim using NREFDESC
.                             append    LineFeed,taskname
                              append    NREFDESC,taskname
                    endif
                    reset     taskname
                    setprop sheet.range(ExRange1),*Value=taskname,*WrapText=OTRUE
                    sheet.range(EXRange3).Merge
                    sheet.range(EXRange3).Rows.Autofit
          endif
.
          add       C1,leftcol
          move      leftcol,str9
          call      Trim using str9
          pack      Exrange1,"A",str9
          pack      Exrange2,"C",str9
          pack      Exrange3,Exrange1,":",Exrange2
          clear     taskname
          if (NdatFem > 0 or NdatMen > 0)
                    if (NdatFem > 0)
                              move      NDatFem,N3
                              move      N3,str3
                              append    "Women ",taskname
                              append    str3,taskname
                              append    "%",taskname
                              append    " ",taskname
                    endif
                    if        (NdatMen > 0)
                              move      NDatMen,N3
                              move      N3,str3
                              append    "Men ",taskname
                              append    str3,taskname
                              append    "%",taskname
                    endif
          else
                    append    "Gender:  ",taskname
                    call      Trim using Sex
                    append    Sex,taskname
          endif
          reset     taskname
          setprop sheet.range(ExRange1),*Value=taskname,*WrapText=OTRUE
          sheet.range(EXRange3).Merge
.
          add       C1,leftcol
          move      leftcol,str9
          call      Trim using str9
          pack      Exrange1,"A",str9
          pack      Exrange2,"C",str9
          pack      Exrange3,Exrange1,":",Exrange2
          clear     taskname
          call      Trim using min
          pack      taskname,"Minimum:  ",min
          setprop sheet.range(ExRange1),*Value=taskname,*WrapText=OTRUE
          sheet.range(EXRange3).Merge
.
.Arrangement
          pack      NARRFLD1,"01X",LSTNUM
          move      "D.Load-NARRAIM",Location
          pack      KeyLocation,"Key: ",NARRFLD1
          call      NARRAIM
          if not over
                    add       C1,leftcol
                    move      leftcol,str9
                    call      Trim using str9
                    pack      Exrange1,"A",str9
                    pack      Exrange2,"C",str9
                    pack      Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1),*Value="****ARRANGEMENT*****",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range(ExRange1).Font,*Bold="True"
                    sheet.range(EXRange3).Merge
          endif
          loop
                    until over
                    call      DataLoadRefArrangement
                    call      Trim using NREFDESC
                    if (NREFDESC <> "")
                              add       C1,leftcol
                              move      leftcol,str9
                              call      Trim using str9
                              pack      Exrange1,"A",str9
                              pack      Exrange2,"C",str9
                              pack      Exrange3,Exrange1,":",Exrange2
                              setprop sheet.range(ExRange1),*Value=NREFDESC,*WrapText=OTRUE
                              sheet.range(EXRange3).Merge
                    endif
                    move      "D.Load-NARRKG",Location
                    pack      KeyLocation,"Key: ",NARRFLD1
                    call      NARRKG
          repeat
.Addressing
          pack      NADDFLD1,"01X",LSTNUM
          move      "D.Load-NADDAIM",Location
          pack      KeyLocation,"Key: ",NADDFLD1
          call      NADDAIM
          if not over
                    add       C1,leftcol
                    move      leftcol,str9
                    call      Trim using str9
                    pack      Exrange1,"A",str9
                    pack      Exrange2,"C",str9
                    pack      Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1),*Value="****ADDRESSING*****",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range(ExRange1).Font,*Bold="True"
                    sheet.range(EXRange3).Merge
          endif
          loop
                    until over
                    call      DataLoadRefAddressing
                    call      Trim using NREFDESC
                    if (NREFDESC <> "")
                              add       C1,leftcol
                              move      leftcol,str9
                              call      Trim using str9
                              pack      Exrange1,"A",str9
                              pack      Exrange2,"B",str9
                              pack      Exrange3,Exrange1,":",Exrange2
                              call trim using NREFDESC
                              setprop sheet.range(ExRange1),*Value=NREFDESC,*WrapText=OTRUE
                              sheet.range(EXRange3).Merge
                              pack      Exrange2,"C",str9
                              setprop sheet.range(ExRange2),*Value=str25,*WrapText=OTRUE
                    endif
                    move      "D.Load-NADDKG",Location
                    pack      KeyLocation,"Key: ",NADDFLD1
                    call      NADDKG
          repeat
.SOURCE
          pack      NSRCFLD1,"01X",LSTNUM
          move      "D.Load-NSRCAIM",Location
          pack      KeyLocation,"Key: ",NSRCFLD1
          call      NSRCAIM
          if not over
                    add       C1,leftcol
                    move      leftcol,str9
                    call      Trim using str9
                    pack      Exrange1,"A",str9
                    pack      Exrange2,"C",str9
                    pack      Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1),*Value="****SOURCE*****",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range(ExRange1).Font,*Bold="True"
                    sheet.range(EXRange3).Merge
          endif
          loop
                    until over
                    call      DataLoadRefSource
                    call      Trim using NREFDESC
                    if (NREFDESC <> "")
                              add       C1,leftcol
                              move      leftcol,str9
                              call      Trim using str9
                              pack      Exrange1,"A",str9
                              pack      Exrange2,"C",str9
                              pack      Exrange3,Exrange1,":",Exrange2
                              setprop sheet.range(ExRange1),*Value=NREFDESC,*WrapText=OTRUE
                              sheet.range(EXRange3).Merge
                    endif
                    call      NSRCKG
          repeat
.SELECTIONS
          pack      NSLTFLD1,"01X",LSTNUM
          move      "D.Load-NSLTAIM",Location
          pack      KeyLocation,"Key: ",NSLTFLD1
          call      NSLTAIM
          if not over
                    add       C1,leftcol
                    move      leftcol,str9
                    call      Trim using str9
                    pack      Exrange1,"A",str9
                    pack      Exrange2,"C",str9
                    pack      Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1),*Value="****SELECTIONS*****",*WrapText=OTRUE,*HorizontalAlignment=xlAlignCenter
                    setprop sheet.range(ExRange1).Font,*Bold="True"
                    sheet.range(EXRange3).Merge
          endif
          loop
                    until over
                    call      DataLoadRefSelection
                    call      Trim using NREFDESC
                    if (NREFDESC <> "")
                              add       C1,leftcol
                              move      leftcol,str9
                              call      Trim using str9
                              pack      Exrange1,"A",str9
                              pack      Exrange2,"B",str9
                              pack      Exrange3,Exrange1,":",Exrange2
                              call trim using NREFDESC
                              setprop sheet.range(ExRange1),*Value=NREFDESC,*WrapText=OTRUE
                              sheet.range(EXRange3).Merge
                              pack      Exrange2,"C",str9
                              setprop sheet.range(ExRange2),*Value=str25,*WrapText=OTRUE
                    endif
                    move      "D.Load-NSLTKG",Location
                    pack      KeyLocation,"Key: ",NSLTFLD1
                    call      NSLTKG
          repeat
.Selects
          SelectListView.deleteallitems
          Select2ListView.deleteallitems
          move      C0,SelTextFlag
.
          if (NDATCONV <> "1")
                    return
          endif

          pack      NSELFLD1,"01X",LSTNUM
          clear     NSELFLD2
          move      "D.Load-NSELAIM",Location
          pack      KeyLocation,"Key: ",NSELFLD1
          call      NSELAIM
          loop
                    until over
                    if (NSELSTATUS = "1" | NSELSTATUS = "2")          .1 = Special, 2 = Office Use Only
                              Goto SkipSelect
                    endif
                    if (NSELINACTIVE = "1")
                              Goto SkipSelect
                    endif
                    if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                              selectlistview.InsertItem giving N8 using NSELINDEX
                              selectlistview.SetItemText giving N1 using N8,NSELNUM,1
                              selectlistview.SetItemText giving N1 using N8,NSELLIST,2
                    else
                              select2listview.InsertItem giving N8 using NSELINDEX
                              select2listview.SetItemText giving N1 using N8,NSELNUM,1
                              select2listview.SetItemText giving N1 using N8,NSELLIST,2
                    endif
                    if (NSELEXC <> "2")       .if the select is not exchange only
                              move      Yes,REntFlag           .at least one portion of this list is rentable
                    endif
SkipSelect
                    move      "D.Load-NSELKG",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELKG
          repeat
.
          Selectlistview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          if less
                    Goto SkipSelection
          else
.                   append    "Base:  ",hold2
                    move      C1,SelTextFlag
          endif
          for num9,"0",#result
                    Selectlistview.GetItemText giving str4 using num9,1
                    call      zfillit using str4,c0
                    rep       zfill,str4
                    pack      nselfld with lstnum,str4
                    call      nselkey
.
                    rep       lowup in NSELSNAME
.Cheating in order to get the Select name in the Excel cell without having to make the column super wide.
                    move      NSELSNAME,str45
                    call      trim using nselsname
                    if (NSELSNAME <> "")          .Skip it if there is no text
                              move      mask13 to dim13a
                              subtract N10,n10
                              move      nselqty to n10
                              edit      n10 to dim13a
.                             call      trim using dim13a
                              append    DIM13A,hold2
                              append    B1,hold2
.
                              clear     taskname
                              if (NSELNOTES <> "1")
                                        append    str45,taskname
                              else
                                        move YES to SELNOTESFLAG
                                        append    str45,taskname
                                        append    "*",taskname
                              endif
                              reset     taskname
                              append    taskname,hold2
                              append    B1,hold2
.
                              move      NSELPRICE to dim9a
                              call      SelectLoadModifier
                              If (NSELEXC= "1")
                                        pack      str55,"Exch/",str25
                                        move      str55,str25
                              elseif (NSELEXC= "2")                          .exchange only
                                        pack      str25 with "Exchange Only"
                              endif
                              append    str25,hold2
                              append    newline,hold2
                    endif
          repeat
SkipSelection
          return

AdditionalSelects
          clear     num9
          Select2listview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          if less
                    return
          endif
          append    newline,hold2
          for num9,"0",#result
                    append    newline,hold2
                    Select2listview.GetItemText giving str4 using num9,1
                    call      zfillit using str4,c0
                    rep       zfill,str4
                    pack      nselfld with lstnum,str4
                    call      nselkey
                    rep             lowup in NSELSNAME
.Cheating in order to get the Select name in the Excel cell without having to make the column super wide.
                    move      NSELSNAME,str45
                    call      trim using nselsname
                    if (NSELSNAME <> "")          .Skip it if there is no text
                              move      mask13 to dim13a
                              sub       n10,n10
                              move      nselqty to n10
                              edit      n10 to dim13a
.                             call      trim using dim13a
                              append    DIM13A,hold2
                              append    B1,hold2
                              if        (NSELNOTES <> "1")
                                        append    str45,hold2
                              else
                                        move YES to SELNOTESFLAG
                                        append    str45,hold2
                                        append    "*",hold2
                              endif
                              append    B1,hold2
                              IF (NSELPRICE <> C0)
                                        move nselprice to dim9a
                                        call trim using dim9a
                                        call SelectLoadModifier
                              ELSE
                                        Clear STR25
                              ENDIF
                              append    str25,hold2
                    endif
ContinueSelects2
          repeat
          append    newline,hold2
          append    newline,hold2
          return

.4.01 test logic....
drewtest
          move      str1,str1
          return
ConvertDate LRoutine DimPtr,DimPtr2
          clear     DimPtr2
          append    "Last Update:  ", DimPtr2
.
          call      Trim using Dimptr
          if (DimPtr <> "")
                    unpack    DimPtr into cc,yy,mm,dd
                    if (mm <> "")
                              if (dd <> "")
                                        rep       zfill,mm
                                        clear     str15
                                        if (mm = "01")
                                                  move      "January",str15
                                        elseif (mm = "02")
                                                  move      "February",str15
                                        elseif (mm = "03")
                                                  move      "March",str15
                                        elseif (mm = "04")
                                                  move      "April",str15
                                        elseif (mm = "05")
                                                  move      "May",str15
                                        elseif (mm = "06")
                                                  move      "June",str15
                                        elseif (mm = "07")
                                                  move      "July",str15
                                        elseif (mm = "08")
                                                  move      "August",str15
                                        elseif (mm = "09")
                                                  move      "September",str15
                                        elseif (mm = "10")
                                                  move      "October",str15
                                        elseif (mm = "11")
                                                  move      "November",str15
                                        elseif (mm = "12")
                                                  move      "December",str15
                                        endif
                                        if (str15 <> "")
                                                  move      C0,N2
                                                  move      dd,N2
                                                  if (N2 <> C0)
                                                            move      N2,str2
                                                            call      Trim using str2
.                                                           append    LineFeed,DimPtr2
                                                            append    str15,DimPtr2
                                                            append    " ",DimPtr2
                                                            append    str2,DimPtr2
                                                            append    ", ",DimPtr2
                                                            append    CC,DimPtr2
                                                            append    YY,DimPtr2
                                                  endif
                                        endif
                              endif
                    endif
          endif
          reset     DimPtr2
          return

DataLoadRefClean
          pack      NREFFLD,CLEANCDE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefAddressing
          pack      NREFFLD,"A",NADDNUM
          move      "D.Load1-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NADDPRICE = C0)
                    clear     str9
          else
                    unpack    NADDPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NADDDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          return
DataLoadRefArrangement
          pack      NREFFLD,"R",NARRNUM
          move      "D.Load4-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSource
          pack      NREFFLD,"S",NSRCNUM
          move      "D.Load5-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          move      C0,N3
          move      NSRCPER,N3
          if (N3 > C0)
                    move      N3,NSRCPER
                    call      Trim using NSRCPER
                    pack      str4,NSRCPER,PRC
          endif
          return
DataLoadRefSelection
          pack      taskname,NSLTVARS
          pack      str6,"LLL",NSLTNUM
          pack      NREFFLD,"L",NSLTNUM
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NSLTPRICE = C0)
                    clear     str9
          else
                    unpack    NSLTPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NSLTDESC
          rep       zfill,NMODFLD
          move      "D.Load3-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          call      FormatNumeric using NSLTQTY,str11
          PACK TASKNAME WITH NREFDESC,B8,STR25
          return

DataLoadRefNetName
          pack      taskname,NETNAME
          pack      NREFFLD,NETNAME
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSAMPLE
          pack      taskname,SAMPLE
          pack      NREFFLD,SAMPLE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefDelivery
          pack      taskname,DELCODE
          pack      NREFFLD,DELCODE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return

.END PATCH 4.01 ADDED LOGIC

DREWDEBUG
          MOVE      STR1,STR1
          RETURN
.begin patch 3.96
              include         NQRCIO.inc
.end patch 3.96

          include   nusgio.inc
         include   nownio.inc
         include   nmdlio.inc
          INCLUDE   NDATIO.INC
          INCLUDE   NTXTIO.INC
          INCLUDE   NSELIO.INC
          include   nmodio.inc
          Include Compio.inc
          Include Cntio.inc             
.begin patch 4.0
          INCLUDE   NTXT1IO.INC
.end patch 4.0
.START PATCH 4.01 ADDED LOGIC
          include   nrefio.inc
          include   naddio.inc
          include   narrio.inc
          include   ncatio.inc
          include   NSLTio.inc
          include   nsrcio.inc
.END PATCH 4.01 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
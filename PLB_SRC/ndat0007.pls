PC        EQU                 0
          INCLUDE             COMMON.inc
          INCLUDE             CONS.inc
          INCLUDE             NDATDD.inc
          INCLUDE             NUSGDD.INC
          INCLUDE             HP.INC
          Include   NQRCdd.inc          
Release   Init      "1.04"   DLH   .Update web ip from  64.71.27.110 to 207.141.154.44  
REldate   INit      "2014 October 21"
.Release   Init      "1.03"   DLH   change  HTML to match new site   IE remove  http://64.71.27.107 from  http://64.71.27.107/plb-bin ......
.REldate   INit      "02 July 2012"
.Release   Init      "1.02"   DLH   change  HTML to match new site
.REldate   INit      "09 July 2010"
.  text   #777777 now #33FF00
.         #748EB8 now #FFFFFF
.         #2759A6 Now #FFFFFF
.         #D9EF86 now #2858A6       
.         #E4F4AA now #2858A6       

.Release   Init      "1.01"   DLH     Add website Tracking code
.REldate   INit      "17 March 2009"
.Release   Init      "1.00"   DLH     New - Create Quick reco pages for the website
.REldate   INit      "19 June 2008"
NAME      DIM       19 (FILE NAME)

INPUT     FILE      
OUTPUT    FILE
HTMLFILE  FIle
REPLY     DIM       1
LINEs     FORM      2           LINE COUNTER
PAGE      FORM      3           PAGE NUMBER
LR        FORM      "0"         '1'=LAST RECORD PENDING
LRTOT     FORM      7          GRAND TOTAL
ANS       DIM       1
MO        DIM       2       MONTH
DAY       DIM       2
YR        DIM       2
DATE      DIM       8

ONE      FORM      "1"
COUNT    FORM      "00000"
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
NINPPATH FORM      1
htmlflag form      1
HTMLPTR  FORM      2
HTMLALPHA INIT     "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
markflag form      1
NUMBER   DIM       10
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
.............................................................................................
NFULCOMP  DIM       55
+.........................................................................
.
. DEFAULT OPTIONS ARE : 
.                       
.
. COMMENT CAN MODIFY THE DEFAULTS:  
.                                   
.                                   
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
         rep       LowUp in program
         MATCH     "NDAT0007" TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        not equal                 .NO
         MOVE      "NDAT0007" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "LOCAL" TO PRTNAME
         CLEAR     COMMENT
         ENDIF

         MOVE      "Quick Reco Webpages" TO STITLE
         MOVE      "EXIT" TO PF5
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         CALL      PAINT
         CALL      FUNCDISP

         CLOCK     DATE TO DATE
         UNPACK    DATE INTO MO,STR1,DAY,STR1,YR
         REP       zfill,DAY
         REP       zfill,MO
         CLEAR     TODAY
         PACK      TODAY FROM MO,SLASH,DAY,SLASH,YR
         

BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
.input name holds category IE  data10001 = category 1, data10002 = category 2
OPTGET   
         
         
         RESET     COMMENT
         KEYIN     *P20:15,"HTML   :  produce code for web":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION
         MOVE      C0 TO HTMLFLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P20:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         RESET     COMMENT
         SCAN      "HTML" IN COMMENT
         CALL      OPTHTML IF EQUAL
         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"HTML   :  produce code for web":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL;
         GOTO      OPTGET
OPTDEFLT 
         MOVE      C2 TO HTMLFLAG
         GOTO      INPGET
OPTHTML  MOVE      C2 TO HTMLFLAG
         MOVE      C1 TO HTMLPTR           .SET POINTER
         RETURN
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         TRAP      io giving error IF io
         CLOSE     TESTFILE
         MOVE      C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NAME
         if        (htmlflag = 2)
         clear      name
         append     inpname to name
         append     ".srt",name
         reset      name
         clear     sortvar
         append     "\\nins1\e\data\",sortvar                          ."
         append     inpname,sortvar
         append     ".dat",sortvar
         append     ",",sortvar
         append     "c:\work\",sortvar            ."
         append     name,sortvar
         append     ";64-138",sortvar
         reset      sortvar
         sort      sortvar
        IF OVER
                DISPLAY *N,"Sort ERROR ",S$ERROR$;
        ENDIF

         endif
          UNpack    INpName into str5,NQRCDFLD
          rep       Zfill,NQRCDFLD
          call      NQRCDKEY                      .get category
          Rep       LowUp,Nqrcddesc
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET

PRTOK
          clear     str55
            append    "c:\work\" to str55                               ."
          move      NQRCDFLD,n4
          if        (n4 = c1)
          append    "animalwelfare",str55
          elseif    (n4 = c2)
          append    "catalog",str55
          elseif    (n4 = c3)
          append    "charitable",str55
          elseif    (n4 = c4)
          append    "children",str55
          elseif    (n4 = c5)
          append    "civilRights",str55
          elseif    (n4 = c6)
          append    "Cultural",str55
          elseif    (n4 = c7)
          append    "Environmental",str55
          elseif    (n4 = c8)
          append    "Foodbanks-missions",str55
          elseif    (n4 = c9)
          append    "Health",str55
          elseif    (n4 = 10)
          append    "Jewish",str55
          elseif    (n4 = 11)
          append    "NativeAmerican",str55
          elseif    (n4 = 12)
          append    "Outdoor",str55
          elseif    (n4 = 13)
          append    "Political",str55
          elseif    (n4 = 14)
          append    "Publications",str55
          elseif    (n4 = 15)
          append    "PublicTV",str55
          elseif    (n4 = 16)
          append    "WomensRights",str55
          endif

          append    ".htm" to str55
          reset     str55
          prepare   htmlfile,str55

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
            write     htmlfile,seq;"<form name='LoginForm' method='post' action='/plb-bin/login.plc'>"
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
            write     htmlfile,seq;"<a href=#"/plb-bin/passrequest.plc#"><b><font size=#"1#" face=#"Arial#">Forgot"
            write     htmlfile,seq;"Your Password?</font></b></a>&nbsp&nbsp&nbsp"
            write     htmlfile,seq;"</td>"
            write     htmlfile,seq;"<TD width=#"85#" align=center valign=center>"
            write     htmlfile,seq;"<input type=image img border=#"0#" src=#"/images/btn_logon.gif#" width=#"38#" height=#"19#" name=#"I1#">"
            write     htmlfile,seq;""
            write     htmlfile,seq;"</TD>"
            write     htmlfile,seq;"<TD>"
            write     htmlfile,seq;"<A href=#"/plb-bin/registration.plc#"><font size=#"1#" face=#"Arial#">Register</font></a>"
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
            write     htmlfile,seq;"<li class=#"page_item page-item-2#"><a href=#"http://www.nincal.com/index.php/about/#" title=#"About Us#">About Us</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-21#"><a href=#"http://www.nincal.com/index.php/about/team/#" title=#"Our Team#">Our Team</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-3#"><a href=#"http://www.nincal.com/index.php/list-brokerage/#" title=#"List Brokerage#">List Brokerage</a>"
            write     htmlfile,seq;"<ul>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-144#"><a href=#"http://www.nincal.com/index.php/list-brokerage/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-142#"><a href=#"http://www.nincal.com/index.php/list-brokerage/order-status/#" title=#"Order Status#">Order Status</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-147#"><a href=#"http://www.nincal.com/index.php/list-brokerage/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
            write     htmlfile,seq;"</ul>"
            write     htmlfile,seq;"</li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-6 current_page_ancestor current_page_parent#"><a href=#"http://www.nincal.com/index.php/list-management/#" title=#"List Management#">List Management</a>"
            write     htmlfile,seq;"<ul>"
            write     htmlfile,seq;"<li class=#"page_item page-item-156 current_page_item#"><a href=#"http://www.nincal.COM/DATACARDS/ExcelLists.html#" title=#"Data Card Library#">Data Card Library</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-158#"><a href=#"http://www.nincal.com/index.php/list-management/invoice-request/#" title=#"Invoice Request#">Invoice Request</a></li>"
.            write     htmlfile,seq;"<li class=#"page_item page-item-159#"><a href=#"http://www.nincal.com/index.php/list-management/featured-lists#" title=#"Featured Lists#">Featured Lists</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-161#"><a href=#"http://www.nincal.COM/DATACARDS/exclupdates.html#" title=#"Recently Updated Lists#">Recently Updated Lists</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-164#"><a href=#"http://www.nincal.com/index.php/list-management/recommended-lists#" title=#"Recommended Lists#">Recommended Lists</a>"
            write     htmlfile,seq;"<li class=#"page_item page-item-168#"><a href=#"http://www.nincal.com/index.php/list-management/regional-count-request/#" title=#"Regional Count Request#">Regional Count Request</a></li>"
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
.         write     htmlfile,seq;"<li class=#"page_item page-item-10#"><a href=#"http://www.nincal.com/index.php/multi-channel-marketing/#" title=#"Multi-Channel Marketing#">Multi-Channel Marketing</a></li>"
.2013 April 29 DLH
            write     htmlfile,seq;"<li class=#"page_item page-item-12#"><a href=#"http://www.nincal.com/index.php/clientstestimonials/#" title=#"Clients#">Clients</a></li>"
            write     htmlfile,seq;"<li class=#"page_item page-item-14#"><a href=#"http://www.nincal.com/index.php/contact-us/#" title=#"Contact Us#">Contact Us</a>"
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
.            write     htmlfile,seq;"<h2>Data Card Library</h2>"
          if        (n4 = c1)
           Write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_animalWelfare.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c2)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Catalog.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c3)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Charitable.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c4)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Children.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c5)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_CivilRights.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c6)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Cultural.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c7)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Environmental.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c8)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Foodbanks.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = c9)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Health.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 10)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Jewish.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 11)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_NativeAmerican.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 12)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Outdoor.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 13)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Political.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 14)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_Publications.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 15)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_PublicTV.gif#" width=#"200#" height=#"16#"></p>"
          Elseif    (n4 = 16)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_WomensRights.gif#" width=#"200#" height=#"16#"></p>"
          endif

            write     htmlfile,seq;"<div class=#"entry#">"
            write     htmlfile,seq;""
          write     htmlfile,seq;"<p class=#"MsoNormal#"><span style=#"font-size:11.0pt;color:gray#">Based on "
          write     htmlfile,seq;"12-month continuation usage, these lists have proven to be the most "
          write     htmlfile,seq;"responsive to ",NQRCDDESC," mailers.</span></p><ul>"
            write     htmlfile,seq;"<dl>"
            write     htmlfile,seq;"<dd>"
            write     htmlfile,seq;"<table border=#"0#" cellspacing=#"0#" cellpadding=#"0#" width=#"490#">"
            write     htmlfile,seq;"<tbody>"

.          write     htmlfile,seq;"<p class=#"MsoNormal#"><span style=#"font-size:11.0pt;color:gray#">Based on "
.          write     htmlfile,seq;"12-month continuation usage, these lists have proven to be the most "
.          write     htmlfile,seq;"responsive to ",NQRCDDESC," mailers.</span></p><ul>"

         
          write     htmlfile,seq;""
          write     htmlfile,seq;"<span class=#"arial10#"><br>Last update: ",today," </span></p>"
          write     htmlfile,seq;""


          
.         write     htmlfile,seq;" <p>Based on 12-month continuation usage, these lists have proven to be the"
.         write     htmlfile,seq;"   most responsive to ",NQRCDDESC," mailers.</p>"
.                   write     htmlfile,seq;""


         GOTO      PRESTART
OUTPUT
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
PRESTART MOVE      C0 TO TOTUNIV
         CLEAR     PF4
         TRAPCLR   F4
         CALL      FUNCDISP
         TRAP      IO giving error IF IO
         clear      str35
         pack       str35 from "c:\work\",name                                  .."
         OPEN      INPUT,str35,EXCLUSIVE
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
START
.
*............................................................
. READ A RECORD FROM THE FILE
.
READ      DISPLAY   *P1:23,*EL,*HON,"READING";
          read      input,seq;datvars
          goto      eoj if over
          add       c1,count
          Display   *p15:09,*el,count
          Move      mlstname to str55
          rep       uplow in str55
          scan      "office use" in str55
          goto      read if equal

          scan      "Office Use" in mlstname
          goto      read if equal
          scan      "OFFICE USE" in mlstname
          goto      read if equal
          scan      "Office use" in mlstname
          goto      read if equal
          goto                 read if (NDATOFF = "1")
          goto                 read if (NDATWEB = "1")
          scan   "|A" in mlstname
          call   undofixita if equal
          reset  Mlstname
          scan   "|T" in mlstname
          call   undofixitThe if equal
          reset  Mlstname


          
          WRITE     HTMLFILE,SEQ;"                <li>"
          WRITE     HTMLFILE,SEQ;"<class=#"MsoNormal#"><span style=#"font-size:11.0pt;color:navy#">"
          WRITE     HTMLFILE,SEQ;"<a style=#"text-decoration: underline; text-underline: single#"  href=#"/Datacards/data",LSTNUM,".htm#">",MLSTNAME:
                               "</a></span></li>"


          pack      NUSGFLD1,"01X",LSTNUM
          move      "NUSGAIM",Location
          pack      KeyLocation,"Key: ",NUSGFLD1
          call      NUSGAIM
          If Not over
                    if (NDATLUSAGE <> "F")
.skipping for now
.                             WRITE     HTMLFILE,SEQ;"                                              <a target=#"_blank#" title=#"Click to see Usage#" href=#"/usage/usg",lstnum,".pdf#">Usage</a>"
                    Endif
          Endif
         goto      read
* ...........................................................
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
         write     htmlfile,seq;"</div><div class=#"sidebar_widget#"><h3>Stay Informed:</h3>                       <div class=#"textwidget#"><a href=#"http://visitor.constantcontact.com/manage/optin/ea?v=001Qo1SzxA2oRQ-aZ5HD9z05Q%3D%3D#" target=#"_blank#" ><img src=#"http://207.141.154.44/images/newsletter.jpg#" height=#"45#" width=#"95#" style=#"border: 0pt none; margin: 0px;#"></a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"<a href=#"http://www.facebook.com/namesinthenews#"target=#"_blank#"><img src=#"http://207.141.154.44/images/facebook.jpg#"  height=#"45#" width=#"95#" style=#"border: 0pt none; margin: 0px;#"> </a><a href=#"http://www.twitter.com/#!/NamesintheNews#" target=#"_blank#">"
         write     htmlfile,seq;"<img src=#"http://207.141.154.44/images/twitter.jpg#"  height=#"45#" width=#"95#" style=#"border: 2pt; margin: 0px;#"> </a>"
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
         write     htmlfile,seq;"<a href=#"/index.php/clients/#" ><img src=#"http://207.141.154.44/images/clientlogos.gif#" style=#"border: 0pt none; margin: 0px;#"></a></div>"
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
..*******<<<<<<<<<<<<<<<<annually
         write     htmlfile,seq;"<div> &##169; 2016"
         write     htmlfile,seq;"Names in the News    | 180 Grand Avenue, Suite 1365, Oakland, CA 94612 | T: 415-989-3350 | F: 415-433-7796 |  <div></div>"
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


EOJ1
         CLOSE     INPUT
         shutdown  "cls"
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
IO        DISPLAY   *P1:1,*ES,"IO error ",error,*B,*B ;
          trapclr   io
          trap      io giving error if io
          GOTO      INPNG

          include   nusgio.inc
          INCLUDE   NDATIO.INC
          Include   NQRCio.inc          

          INCLUDE   COMLOGIC.inc
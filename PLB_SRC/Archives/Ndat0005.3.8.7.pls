PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   nmdldd.inc
         INCLUDE   NDATDD.inc
.>Patch 3.8.7 Code Added
	 INCLUDE   NUSGDD.INC
.>Patch 3.8.7 Code Added
         INCLUDE    HP.INC
			include	nmoddd.inc
.START PATCH 3.5 ADDED LOGIC
	INCLUDE	NTXTDD.INC
	INCLUDE	NSELDD.INC
.END PATCH 3.5 ADDED LOGIC
Release  init      "3.8.7"      14MAR2006 ASH Added TOS tags
.Release  init      "3.8.6"      13JUN2005 DMB Changed Updated.htm to Exclupdates.html
.Release  init      "3.8.5"      07APR2005 ASH COMMPER CONVERSION
.Release  init      "3.8.4"      28FEB2005 DMB Added code for new newletter naming scheme
.Release  init      "3.8.3"      25OCT2004 DMB Added code for not indexing lists.html page
.Release  init      "3.8.2"      10AUG2004 DMB Log Conversion
.Release  init      "3.8.1"      21JUN2004 DMB Bug Fix
.Release  init      "3.8"      08JUN2004 DMB Added code to check for ndatweb - not allowed on web site
;Release  init      "3.7"      19APR2004 DMB Added code to check for converted office use only
;Release  init      "3.6"      12APR2004 DMB Added code to read for ndatconv and to include typist and correct printout to show whether list is excl
;Release  init      "3.5"      27JAN04  ASH  DATACARD CONVERSION
.Release  init      "3.4"      30September2003 DLH  More intensive checking for OFFICE USE ONLY CARDS
;Release  init      "3.3"      10March2003 DLH  skip DCCC & DNC if HTML --- temp
;release  init      "3.2"      24Feb03 DLH quick and dirty to produce HTML for exclusives updated in last
;                             30 days during webcards run.  Req BLO
;release  init      "3.1"      05May01 DLH Alphabetizing
;release  init      "3.02"      29oct00 ASH NEW SERVER ADDED
;release  init      "3.01"      ..11Sep00 DLH Fix bug cause by 3.0 symptom was missing detail
;                                and totals on a non HTML report.
;release  init      "3.0"        6June00 DLH add code to output an HTML file listing
;                               for the NIN web page
;                               Note: uses "HTML" as option & requires outname
;release  init      "2.4"        18Jan00 DLH add code for paper trays on new printer
;release  init      "2.3"        28APR99 DLH change in flat file option
;RELEASE  INIT      "2.2"        22JAN97 JD FONT CHANGE.
;RELEASE  INIT      "2.1"       07JUL94 DLH ADDED DUPLEX OPTION.
;RELEASE  INIT      "2.0"       31MAR94  DLH ADD OWNER READ, CHANGE FLAT FILE
;                               FORMAT TO MATCH THE (Y) REPORT.
;RELEASE  INIT      "1.8"       15MAR94  DLH LASER
;RELEASE  INIT      "1.7"       15SEP93  DLH ADD LOTUS FLAT FILE.
;
;RELEASE  INIT      "1.6"
;RELEASE  INIT     "1.5"       02/21/86 -- INCREASE EFFICIENCY OF READS ....
;RELEASE  INIT     "1.4"       01/16/86 -- CHANGE CATAGORY CODES TO 3 BYTES.
;RELEASE  INIT     "1.3"       05/07/85 -- ADD EXCLUDE WITHDRAWN OPTION.
;RELEASE  INIT      "1.2"       08/18/83  DLH   ADDED KILL INPUT FILE &
;                                              REPRINT OPTIONS.
;RELEASE  INIT      "1.1"          05/19/83   DLH INCREASE SPEED VIA ADDING
;                                            PRINT BUFFER, ALSO ADDED PRINT
;                                            OF WITHDRAWN.
;RELEASE  INIT      "1.0"           07/08/83   DLH & DSGEN
NAME     DIM       19 (FILE NAME)
INPUT    FILE      VAR=181 (INPUT  FILE)
OUTPUT   FILE
;begin patch 3.0
HTMLFILE FIle
;end patch 3.0
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
.START PATCH 2.3 REPLACED LOGIC
.LINE1    DIM       127
.LINE1A   DIM       5
.LINE1B   DIM       10
.LINE2    DIM       127
.LINE2A   DIM       5
.LINE2B   DIM       10
.LINE3    DIM       127
.LINE3A   DIM       5
.LINE3B   DIM       10
.LINE4    DIM       127
.LINE4A   DIM       5
.LINE4B   DIM       10
.LINE5    DIM       127
.LINE5A   DIM       5
.LINE5B   DIM       10
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

TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
.END PATCH 2.3 REPLACED LOGIC
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
LOTSFLAG FORM      1
DUPLFLAG FORM      1
LASRFLAG FORM      1
WITHFLAG FORM      1
NINPPATH FORM      1
;begin patch 3.0
htmlflag form      1
;begin patch 3.2
html30flag form      1
;end patch 3.2
HTMLPTR  FORM      2
HTMLALPHA INIT     "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
PTRFLAG  FORM      1              0=IGNORE 1=WRITE BOOKMARK LINKS
;skippedletr   form      1
markflag form      1
BOOKMARK DIM       1
;end patch 3.0
NUMBER   DIM       10
PRICE    DIM       8
LDESC    DIM       40
BEGIN    FORM      2
LAST     FORM      2
;begin patch 3.2
DateJulian     form           5
;end patch 3.2
;patch3.6
MASK13	INIT	"Z,ZZZ,ZZZ,ZZZ"
DIM13a	DIM	13
;CRLF	INTEGER	1,"0x07F"     Turned off 9/10/04  crashing with cons.inc var.  not used.
;patch3.6
dim9a	dim	13
str51a	DIM		 5
str52a	DIM		 5
str53a	DIM		 5
str54a	DIM		 5
str55a	DIM		 5
str31a	DIM		 5
str32a	DIM		 5
str33a	DIM		 5
str34a	DIM		 5
str35a	DIM		 5
.>Patch 3.8.7
TableOpen FORM 1
.>Patch 3.8.7

*............................................................
;
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
.Patch 3.9 Var Added
HTMLINTRO	DIM	2
.Patch 3.9 Var Added
+.........................................................................
;
; DEFAULT OPTIONS ARE : NO update info.
;                       NO WITHDRAWN CARDS
;
; COMMENT CAN MODIFY THE DEFAULTS:  UPDATE :  print updated date
;                                   WITHDRN:  PRINT WITHDRAWN CARDS.
;                                   LOTUS  :  OUTPUT TO FLAT FILE
;
; INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
; PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
;
; IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
;........................................................................

;
+............................................................
;
; FILE OPENING SEQUENCE
         TRAP      F5 giving error IF F5
         TRAP      int giving error IF INT
         TRAP      io giving error IF io
;
         rep       "nNdDaAtT" in program
         MATCH     "NDAT0005" TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        not equal                 .NO
         MOVE      "NDAT0005" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "LOCAL" TO PRTNAME
         CLEAR     COMMENT
         ENDIF

         MOVE      "RH STYLE DATACARD PRINT" TO STITLE
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
;
         GOTO      OPTION
OPTGET   MOVE      C0 TO UPDFLAG
         MOVE      C1 TO LOTSFLAG
         MOVE      C0 TO DUPLFLAG
         RESET     COMMENT
         KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                   *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE FLAT FILE":
                   *P20:13,"LASER  :  PRINT ON LASER":
                   *P20:14,"duplex :  PRINT ON LASER duplex":
;begin patch 3.0
                   *P20:15,"HTML   :  produce code for web":
;end patch 3.0
;begin patch 3.2
                   *P20:16,"HTML30 :  upd exclusives for web":
;end patch 3.2
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO UPDFLAG
         MOVE      C0 TO WITHFLAG
         MOVE      C1 TO LOTSFLAG
         MOVE      C0 TO LASRFLAG
         MOVE      C0 TO DUPLFLAG
;begin patch 3.0
         MOVE      C0 TO HTMLFLAG
;end patch 3.0
;begin patch 3.2
         MOVE      C0 TO HTML30FLAG
;end patch 3.2
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
.Patch 3.9 Code Added
         RESET     COMMENT
         SCAN      "HTML1" IN COMMENT
         CALL      OPTHTML IF EQUAL
;begin patch 3.0
         RESET     COMMENT
         SCAN      "HTML" IN COMMENT
         CALL      OPTHTML IF EQUAL
;end patch 3.0
.patch 3.9 Code Added
;begin patch 3.2
         RESET     COMMENT
         SCAN      "HTML30" IN COMMENT
         CALL      OPTHTML30 IF EQUAL
;end patch 3.2
         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                   *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE FLAT FILE":
                   *P20:13,"LASER  :  PRINT ON LASER":
                   *P20:14,"DUPLEX :  PRINT ON LASER Duplex":
                   *P20:15,"HTML   :  produce code for web":
;begin patch 3.2
                   *P20:16,"HTML30 :  upd exclusives for web":
;end patch 3.2
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO UPDFLAG
         MOVE      C1 TO WITHFLAG
         MOVE      C1 TO LOTSFLAG
         MOVE      C2 TO LASRFLAG
         MOVE      C1 TO DUPLFLAG
;begin patch 3.0
         MOVE      C1 TO HTMLFLAG
;end patch 3.0
         GOTO      INPGET
OPTUPD   MOVE      C2 TO UPDFLAG
         move      c2 to lasrflag
         move      c2 to duplflag
         move      c1 to lotsflag
         RETURN
OPTWITH  MOVE      C2 TO WITHFLAG
         RETURN
OPTLOTUS MOVE      C2 TO LOTSFLAG
         RETURN
OPTLASER MOVE      C2 TO LASRFLAG
         RETURN
OPTdupl  MOVE      C2 TO duplFLAG
         RETURN
;begin patch 3.0
OPTHTML  MOVE      C2 TO HTMLFLAG
         MOVE      C1 TO HTMLPTR           .SET POINTER
.Patch 3.9 Logic Updated
			move inits to HTMLINTRO
.Patch 3.9
         RETURN
;end patch 3.0
;begin patch 3.2
OPTHTML30      MOVE      C2 TO HTML30FLAG
               MOVE      C2 TO HTMLFLAG
               MOVE      C1 TO HTMLPTR           .SET POINTER
.Patch 3.9 Logic Updated
					move inits to HTMLINTRO
.Patch 3.9
               RETURN
;end patch 3.2
;
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         TRAP      io giving error IF io
         CLOSE     TESTFILE
         MOVE      C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
;begin patch 3.0
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
;         append     ";76-130",sortvar
         reset      sortvar
         sort      sortvar
        IF OVER
                DISPLAY *N,"Sort ERROR ",S$ERROR$;
        ENDIF

         endif
;end patch 3.0

         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
;
PRTGET
;begin patch 3.0
         if        (htmlflag <> 2)
         MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         BRANCH    LOTSFLAG TO PRTOK,OUTPUT
         endif
PRTOK
         if        (htmlflag <> 2)
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         else
         clear     str25
         append    "c:\work\" to str25
         append    outname to str25
         append    ".html" to str25
         reset     str25
         prepare   htmlfile,str25
.         write     htmlfile,seq;"<HTML>"
.Patch 3.8.2 Comment Out
.         write     htmlfile,seq;"<HEAD><TITLE>Names in the News, California, Inc.</TITLE></HEAD>"
.Patch 3.8.2 Comment Out
.Patch 3.9 Code Replaced
.         write     htmlfile,seq;"Html List of lists page(s)"
         if (HTMLINTRO = "1")
            write     htmlfile,seq;"<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#"> "
   	      write     htmlfile,seq;"<html>"
      	   write     htmlfile,seq;"<head>"
         	write     htmlfile,seq;"<title>Names in the News - List Library</title>"
.Patch 3.8.3
	         write     htmlfile,seq;"<meta name=#"robots#" content=#"noindex,nofollow#">"
	         write     htmlfile,seq;"<META name=#"description#" content=#"Names in the News provides mailing list brokerage, list management and analytical services to nonprofit organizations as part of their direct mail strategy to acquire new donors and members.#">"
	         write     htmlfile,seq;"<META name=#"keywords#" content=#"list, lists, mail, fundraising, fund raising, directmail, direct mail, list broker, list brokerage, list manager, list management, nonprofit,  non-profit, not-for-profit, charitable, environmental, political, consumer, recommendation, Democratic, progressive#">"
.End Patch 3.8.3
   	      write     htmlfile,seq;"<meta http-equiv=#"Content-Type#" content=#"text/html; charset=iso-8859-1#">"
      	   write     htmlfile,seq;"<link href=#"/nin_styles.css#" rel=#"stylesheet#" type=#"text/css#">"
	         write     htmlfile,seq;"<script language=#"JavaScript#" type=#"text/JavaScript#"> "
   	      write     htmlfile,seq;"<!-- "
      	   write     htmlfile,seq;"function MM_swapImgRestore() { //v3.0"
	         write     htmlfile,seq;"  var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;   "
   	      write     htmlfile,seq;"} "
      	   write     htmlfile,seq;"  "
	         write     htmlfile,seq;"function MM_preloadImages() { //v3.0"
   	      write     htmlfile,seq;"  var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array(); "
      	   write     htmlfile,seq;"    var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++) "
	         write     htmlfile,seq;"    if (a[i].indexOf(#"###")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}  "
   	      write     htmlfile,seq;"}"
      	   write     htmlfile,seq;" "
	         write     htmlfile,seq;"function MM_findObj(n, d) { //v4.01"
   	      write     htmlfile,seq;"  var p,i,x;  if(!d) d=document; if((p=n.indexOf(#"?#"))>0&&parent.frames.length) { "
      	   write     htmlfile,seq;"    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}"
            write     htmlfile,seq;"  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n]; "
            write     htmlfile,seq;"  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);"
            write     htmlfile,seq;"  if(!x && d.getElementById) x=d.getElementById(n); return x; "
            write     htmlfile,seq;"} "
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImage() { //v3.0 "
            write     htmlfile,seq;"  var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)"
            write     htmlfile,seq;"   if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"//-->"
            write     htmlfile,seq;"</script> "
            write     htmlfile,seq;"</head>"
            write     htmlfile,seq;"<body bgcolor=#"##FFFFFF#" text=#"##777777#" link=#"##2759A6#" vlink=#"##748EB8#" alink=#"##2759A6#" leftmargin=#"0#" topmargin=#"0#" marginwidth=#"0#" marginheight=#"0#" onLoad=#"MM_preloadImages('/images/navLeft_ON_02.gif','/images/navLeft_ON_03.gif','/images/navLeft_ON_04.gif','/images/navLeft_ON_01.gif')#">"
            write     htmlfile,seq;"<table width=#"740#" border=#"0#" cellspacing=#"5#" cellpadding=#"0#">"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"95#" align=#"center#" valign=#"middle#" bgcolor=#"##D9EF86#"><a href=#"index.html#"><img src=#"/images/logo.gif#" alt=#"Names in the News#" width=#"120#" height=#"64#" border=#"0#"></a></td>"
            write     htmlfile,seq;"    <td width=#"590#" height=#"95#" bgcolor=#"##D9EF86#" class=#"header#">&nbsp;</td>"
            write     htmlfile,seq;"  </tr>"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"335#" align=#"center#" valign=#"top#" bgcolor=#"##E4F4AA#"><form name=#"form2#" method=#"post#" action=#"#">"
            write     htmlfile,seq;"        <table id=#"Table_01#" width=#"130#" height=#"123#" border=#"0#" cellpadding=#"0#" cellspacing=#"0#"> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td height=#"30#" align=#"center#"><img src=#"/images/tagline.gif#" alt=#"List Brokerage &amp; Management #" width=#"126#" height=#"23#"></td>"
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td align=#"center#" bgcolor=#"##FFFFFF#"><img src=#"/images/5pixTrans.gif#" width=#"5#" height=#"5#"></td> "
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td height=#"45#" align=#"center#"><select name=#"select#" style=#"font-family:'Arial';font-size:9pt;#" onChange=#"location=this.options[this.selectedIndex].value;#">"
            write     htmlfile,seq;"                <option selected>Quick Links</option> "
            write     htmlfile,seq;"                <option value=#"index.htm#">Home</option> "
            write     htmlfile,seq;"                <option value=#"company.htm#">Company</option> "
            write     htmlfile,seq;"                <option value=#"clients.htm#">&middot;&middot;Clients</option>"
            write     htmlfile,seq;"                <option value=#"management.htm#">&middot;&middot;Management</option>"
            write     htmlfile,seq;"                <option value=#"services.htm#">Services</option>"
            write     htmlfile,seq;"                <option value=#"faqs.htm#">&middot;&middot;FAQ</option>"
            write     htmlfile,seq;"                <option value=#"news.htm#">News</option> "
            write     htmlfile,seq;"                <option value=#"news.htm#">&middot;&middot;Newsletter</option> "
            write     htmlfile,seq;"                <option value=#"contact.htm#">Contact</option>"
            write     htmlfile,seq;"                <option value=#"clientInfo.htm#">&middot;&middot;Client Info</option>"
            write     htmlfile,seq;"              </select> </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td><a href=#"company.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image5','','/images/navLeft_ON_01.gif',1)#"><img src=#"/images/navLeft_OFF_01.gif#" alt=#"Company#" name=#"Image5#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td> <a href=#"services.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image6','','/images/navLeft_ON_02.gif',1)#"><img src=#"/images/navLeft_OFF_02.gif#" alt=#"Services#" name=#"Image6#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td> <a href=#"news.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image7','','/images/navLeft_ON_03.gif',1)#"><img src=#"/images/navLeft_OFF_03.gif#" alt=#"News#" name=#"Image7#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td> <a href=#"contact.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image8','','/images/navLeft_ON_04.gif',1)#"><img src=#"/images/navLeft_OFF_04.gif#" alt=#"Contact#" name=#"Image8#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td> <img src=#"/images/navLeft_OFF_05.gif#" width=#"130#" height=#"3#" alt=#"#"></td> "
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
.Patch 3.8.4
            write     htmlfile,seq;"            <td height=#"70#" align=#"center#" valign=#"middle#"><a href=#"/NewsLetters/CurrentNewsletter.pdf#" target=#"_blank#"><img src=#"/images/btn_newsletter.gif#" alt=#"Newsletter#" width=#"104#" height=#"55#" border=#"0#"></a></td> "
.            write     htmlfile,seq;"            <td height=#"70#" align=#"center#" valign=#"middle#"><a href=#"http://www.nincal.com/NewsLetters/Sept%202004%20Newsletter.pdf#" target=#"_blank#"><img src=#"assets/btn_newsletter.gif#" alt=#"Newsletter#" width=#"104#" height=#"55#" border=#"0#"></a></td> "
.Patch 3.8.4
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td height=#"60#" align=#"center#" valign=#"middle#" class=#"cellDma#"><a href=#"http://www.the-dma.org/#" target=#"_blank#"><img src=#"/images/btn_DMAlogo.gif#" alt=#"The DMA#" width=#"67#" height=#"43#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
.START PATCH 3.8.7 ADDED LOGIC
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td align=#"center#"><a href='/tos.html';><font size=1 face='arial'>Terms of Service</font></a></td>"
            write     htmlfile,seq;"          </tr>"
.END PATCH 3.8.7 ADDED LOGIC
	    write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"          <td height=#"80#" align=#"center#" valign=#"middle#" class=#"cellNin#"><strong><span class=#"arialTitle#"><font color=#"##788146#">Mailers</font>:"
            write     htmlfile,seq;"           back to</span></strong><br> <a href=#"http://www.namesinthenews.com#" target=#"_blank#">NamesInTheNews.com</a> "
            write     htmlfile,seq;"          </td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"        </table>"
            write     htmlfile,seq;"      </form></td>"
         elseif (HTMLINTRO = "2" or HTMLINTRO = "3")
            write     htmlfile,seq;"<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">"
            write     htmlfile,seq;"<html>"
            write     htmlfile,seq;"<head>"
            write     htmlfile,seq;"<title>Names in the News</title>"
.Patch 3.8.3
	         write     htmlfile,seq;"<META name=#"description#" content=#"Names in the News provides mailing list brokerage, list management and analytical services to nonprofit organizations as part of their direct mail strategy to acquire new donors and members.#">"
	         write     htmlfile,seq;"<META name=#"keywords#" content=#"list, lists, mail, fundraising, fund raising, directmail, direct mail, list broker, list brokerage, list manager, list management, nonprofit,  non-profit, not-for-profit, charitable, environmental, political, consumer, recommendation, Democratic, progressive#">"
.End Patch 3.8.3
            write     htmlfile,seq;"<meta http-equiv=#"Content-Type#" content=#"text/html; charset=iso-8859-1#">"
            write     htmlfile,seq;"<link href=#"nin_styles.css#" rel=#"stylesheet#" type=#"text/css#">"
            write     htmlfile,seq;"<script language=#"JavaScript#" type=#"text/JavaScript#">"
            write     htmlfile,seq;"<!--"
            write     htmlfile,seq;"function MM_swapImgRestore() { //v3.0"
            write     htmlfile,seq;"  var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_preloadImages() { //v3.0"
            write     htmlfile,seq;"  var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();"
            write     htmlfile,seq;"    var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)"
            write     htmlfile,seq;"    if (a[i].indexOf(#"###")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_findObj(n, d) { //v4.01"
            write     htmlfile,seq;"  var p,i,x;  if(!d) d=document; if((p=n.indexOf(#"?#"))>0&&parent.frames.length) {"
            write     htmlfile,seq;"    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}"
            write     htmlfile,seq;"  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];"
            write     htmlfile,seq;"  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);"
            write     htmlfile,seq;"  if(!x && d.getElementById) x=d.getElementById(n); return x;"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;""
            write     htmlfile,seq;"function MM_swapImage() { //v3.0"
            write     htmlfile,seq;"  var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)"
            write     htmlfile,seq;"   if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}"
            write     htmlfile,seq;"}"
            write     htmlfile,seq;"//-->"
            write     htmlfile,seq;"</script>"
            write     htmlfile,seq;"</head>"
            write     htmlfile,seq;"<body bgcolor=#"##FFFFFF#" text=#"##777777#" link=#"##2759A6#" vlink=#"##748EB8#" alink=#"##2759A6#" leftmargin=#"0#" topmargin=#"0#" marginwidth=#"0#" marginheight=#"0#" onLoad=#"MM_preloadImages('/images/navLeft_ON_02.gif','/images/navLeft_ON_03.gif','/images/navLeft_ON_04.gif','/images/navLeft_ON_01.gif','/images/subnavLists_ON_01.gif','/images/subnavLists_ON_02.gif','/images/subnavLists_ON_03.gif')#">"
            write     htmlfile,seq;"<table width=#"740#" border=#"0#" cellspacing=#"5#" cellpadding=#"0#">"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"95#" align=#"center#" valign=#"middle#" bgcolor=#"##D9EF86#"><a href=#"index.htm#"><img src=#"/images/logo.gif#" alt=#"Names in the News#" width=#"120#" height=#"64#" border=#"0#"></a></td>"
            write     htmlfile,seq;"    <td width=#"590#" height=#"95#" bgcolor=#"##D9EF86#" class=#"header#">&nbsp;</td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"  </tr>"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"335#" align=#"center#" valign=#"top#" bgcolor=#"##E4F4AA#"><form name=#"form1#" method=#"post#" action=#"#">"
            write     htmlfile,seq;"        <table id=#"Table_01#" width=#"130#" height=#"123#" border=#"0#" cellpadding=#"0#" cellspacing=#"0#">"
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td height=#"30#" align=#"center#"><img src=#"/images/tagline.gif#" alt=#"List Brokerage &amp; Management #" width=#"126#" height=#"23#"></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td align=#"center#" bgcolor=#"##FFFFFF#"><img src=#"images/5pixTrans.gif#" width=#"5#" height=#"5#"></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td height=#"45#" align=#"center#"><select name=#"select#" style=#"font-family:'Arial';font-size:9pt;#" onChange=#"location=this.options[this.selectedIndex].value;#">"
            write     htmlfile,seq;"                <option selected>Quick Links</option>"
            write     htmlfile,seq;"                <option value=#"index.htm#">Home</option>"
            write     htmlfile,seq;"                <option value=#"Excellists.html#">Managed Lists</option>"
            write     htmlfile,seq;"                <option value=#"recos.htm#">&middot;&middot;Recos</option>"
            write     htmlfile,seq;"                <option value=#"Exclupdates.html#">&middot;&middot;Updated</option>"
	    write     htmlfile,seq;"                <option value=#"featuredLists.htm#">&middot;&middot;Featured</option>"
            write     htmlfile,seq;"                <option value=#"/plb-bin/ninlists/listsearch2.plc#">&middot;&middot;Search</option>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"                <option value=#"center.htm#">Broker Center</option>"
            write     htmlfile,seq;"                <option value=#"orderStatus.htm#">&middot;&middot;Order Status</option>"
            write     htmlfile,seq;"                <option value=#"countsRequest.htm#">&middot;&middot;Counts</option>"
            write     htmlfile,seq;"                <option value=#"invoiceRequest.htm#">&middot;&middot;Invoicing</option>"
            write     htmlfile,seq;"                <option value=#"company.htm#">Company</option>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"                <option value=#"contact.htm#">Contact</option>"
            write     htmlfile,seq;"              </select> </tr>"
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td><a href=#"Excellists.html#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image5','','/images/NnavLeft_ON_01.gif',1)#"><img src=#"/images/NnavLeft_OFF_01.gif#" alt=#"Managed Lists#" name=#"Image5#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td class=#"cellSubnav#"><table id=#"Table_01#" border=#"0#" cellpadding=#"0#" cellspacing=#"0#">"
            write     htmlfile,seq;"                <tr> "
            write     htmlfile,seq;"                  <td><a href=#"recos.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image3','','/images/subnavLists_ON_01.gif',1)#"><img src=#"/images/subnavLists_OFF_01.gif#" name=#"Image3#" width=#"90#" height=#"17#" border=#"0#"></a></td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"                </tr> "
            write     htmlfile,seq;"                <tr> "
            write     htmlfile,seq;"                  <td><a href=#"Exclupdates.html#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image4','','/images/subnavLists_ON_02.gif',1)#"><img src=#"/images/subnavLists_OFF_02.gif#" name=#"Image4#" width=#"90#" height=#"17#" border=#"0#"></a></td>"
            write     htmlfile,seq;"                </tr>"
            write     htmlfile,seq;"                <tr>"
            write     htmlfile,seq;"                  <td><a href=#"featuredLists.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image51','','/images/subnavLists_ON_03.gif',1)#"><img src=#"/images/subnavLists_OFF_03.gif#" name=#"Image51#" width=#"90#" height=#"17#" border=#"0#" id=#"Image51#"></a></td>"
            write     htmlfile,seq;"                </tr>"
            write     htmlfile,seq;"                <tr>"
            write     htmlfile,seq;"                  <td><a href=#"/plb-bin/ninlists/listsearch2.plc#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image52','','/images/subnavLists_ON_04.gif',1)#"><img src=#"/images/subnavLists_OFF_04.gif#" name=#"Image52#" width=#"90#" height=#"17#" border=#"0#"></a></td>"
            write     htmlfile,seq;"                </tr>"
            write     htmlfile,seq;"              </table></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td><a href=#"center.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image6','','/images/NnavLeft_ON_02.gif',1)#"><img src=#"/images/NnavLeft_OFF_02.gif#" alt=#"Broker Center#" name=#"Image6#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td><a href=#"company.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image7','','/images/NnavLeft_ON_03.gif',1)#"><img src=#"/images/NnavLeft_OFF_03.gif#" alt=#"Company#" name=#"Image7#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
            write     htmlfile,seq;"            <td> <a href=#"contact.htm#" onMouseOut=#"MM_swapImgRestore()#" onMouseOver=#"MM_swapImage('Image8','','/images/NnavLeft_ON_04.gif',1)#"><img src=#"/images/NnavLeft_OFF_04.gif#" alt=#"Contact#" name=#"Image8#" width=#"130#" height=#"30#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td> <img src=#"/images/navLeft_OFF_05.gif#" width=#"130#" height=#"3#" alt=#"#"></td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          </tr> "
            write     htmlfile,seq;"          <tr> "
.Patch 3.8.4
.            write     htmlfile,seq;"            <td height=#"80#" align=#"center#" valign=#"middle#"><a href=#"http://www.nincal.com/NewsLetters/Sept%202004%20Newsletter.pdf#" target=#"_blank#"><img src=#"assets/btn_newsletter.gif#" alt=#"Newsletter#" width=#"104#" height=#"55#" border=#"0#"></a></td>"
            write     htmlfile,seq;"            <td height=#"80#" align=#"center#" valign=#"middle#"><a href=#"/NewsLetters/CurrentNewsletter.pdf#" target=#"_blank#"><img src=#"/images/btn_newsletter.gif#" alt=#"Newsletter#" width=#"104#" height=#"55#" border=#"0#"></a></td>"
.Patch 3.8.4
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td height=#"80#" align=#"center#" valign=#"middle#" class=#"cellDma#"><a href=#"http://www.the-dma.org/#" target=#"_blank#"><img src=#"/images/btn_DMAlogo.gif#" alt=#"The DMA#" width=#"67#" height=#"43#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
.START PATCH 3.8.7 ADDED LOGIC
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td align=#"center#"><a href='/tos.html';><font size=1 face='arial'>Terms of Service</font></a></td>"
            write     htmlfile,seq;"          </tr>"
.END PATCH 3.8.7 ADDED LOGIC
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"          <td height=#"80#" align=#"center#" valign=#"middle#" class=#"cellNin#"><strong><span class=#"arialTitle#"><font color=#"##788146#">Mailers</font>:"
            write     htmlfile,seq;"           back to</span></strong><br> <a href=#"http://www.namesinthenews.com#" target=#"_blank#">NamesInTheNews.com</a> "
            write     htmlfile,seq;"          </td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"        </table>"
            write     htmlfile,seq;"      </form></td>"
         endif
.         endif
         write     htmlfile,seq;""
;.Patch 3.9 Logic Updated
         if (HTMLINTRO = "1")
.Patch 3.9
	         write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_dataCardLibrary.gif#" width=#"200#" height=#"16#"><br>"
         elseif (HTMLINTRO = "2")
   	      write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_dataCardLibrary.gif#" width=#"200#" height=#"16#"><br>"
         elseif (HTMLINTRO = "3")
	         write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_recentlyUpdated.gif#" width=#"200#" height=#"16#"><br>"
         endif
         write     htmlfile,seq;""
         write     htmlfile,seq;"<span class=#"arial10#"><br>Last update: ",today," </span></p>"
         write     htmlfile,seq;""
;.Patch 3.9 Logic Updated
.        if (HTMLINTRO = "1")
.Patch 3.9
         if (HTMLINTRO = "2")
	         write     htmlfile,seq;" <p>Below is an alphabetical listing of all our managed properties. You can"
   	      write     htmlfile,seq;""
.>Patch 3.8.6
.      	   write     htmlfile,seq;"   narrow your search by viewing <a href=#"updated.htm#">lists that have been"
      	   write     htmlfile,seq;"   narrow your search by viewing <a href=#"Exclupdates.html#">lists that have been"
.>Patch 3.8.6
         	write     htmlfile,seq;""
	         write     htmlfile,seq;"   updated in the past 30 days</a>, or visiting our <a href=#"recos.htm#">Quick"
   	      write     htmlfile,seq;""
      	   write     htmlfile,seq;"   Recos</a> for recommendations by market.</p>"
.>Patch 3.8.7
      	   write     htmlfile,seq;"   <p>A link to #"Usage#" will be displayed"
      	   write     htmlfile,seq;"   for all organizations where 12-month continuation usage is available.</p>"
.      	   write     htmlfile,seq;"   <p><img border=#"0#" src=#"/images/document.gif#" width=#"15#" height=#"13#">"
.      	   write     htmlfile,seq;"   Click to see Usage on list if available</p>"
.>Patch 3.8.7

         elseif (HTMLINTRO = "3")
            write     htmlfile,seq;"<p>These managed lists have been updated within the past 30 days. </p>"
.>Patch 3.8.7
      	   write     htmlfile,seq;"   <p>A link to #"Usage#" will be displayed"
      	   write     htmlfile,seq;"   for all organizations where 12-month continuation usage is available.</p>"
.>Patch 3.8.7
         endif
         write     htmlfile,seq;""
         write     htmlfile,seq;" <dl>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"   <dd>"
         write     htmlfile,seq;""
         write     htmlfile,seq;""
.         write     htmlfile,seq;"          <p align=#"left#">"
.         write     htmlfile,seq;""
			if (HTMLINTRO = "1")
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"##A#" NAME=#"A#">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##B#">B</a> <a href=#"##C#">C</a> <a href=#"##D#">D</a> <a href=#"##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##F#">F</a> <a href=#"##G#">G</a> <a href=#"##H#">H</a> <a href=#"##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##J#">J</a> <a href=#"##K#">K</a> <a href=#"##L#">L</a> <a href=#"##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##N#">N</a> <a href=#"##O#">O</a> <a href=#"##P#">P</a> <a href=#"##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##R#">R</a> <a href=#"##S#">S</a> <a href=#"##T#">T</a> <a href=#"##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##V#">V</a> <a href=#"##W#">W</a> <a href=#"##X#">X</a> <a href=#"##Y#">Y</a> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Z#">Z</a></p>"
			elseif (HTMLINTRO = "2")
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Excellists.html##A#" NAME=#"A#">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##B#">B</a> <a href=#"/Excellists.html##C#">C</a> <a href=#"/Excellists.html##D#">D</a> <a href=#"/Excellists.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##F#">F</a> <a href=#"/Excellists.html##G#">G</a> <a href=#"/Excellists.html##H#">H</a> <a href=#"/Excellists.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##J#">J</a> <a href=#"/Excellists.html##K#">K</a> <a href=#"/Excellists.html##L#">L</a> <a href=#"/Excellists.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##N#">N</a> <a href=#"/Excellists.html##O#">O</a> <a href=#"/Excellists.html##P#">P</a> <a href=#"/Excellists.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##R#">R</a> <a href=#"/Excellists.html##S#">S</a> <a href=#"/Excellists.html##T#">T</a> <a href=#"/Excellists.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##V#">V</a> <a href=#"/Excellists.html##W#">W</a> <a href=#"/Excellists.html##X#">X</a> <a href=#"/Excellists.html##Y#">Y</a> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Z#">Z</a></p>"
.>Patch 3.8.7
	 move c1 to tableopen

         write     htmlfile,seq;"			<table border=#"0#" cellpadding=0 width=#"490#" cellspacing=0>"
         write     htmlfile,seq;"				<tr>"
         write     htmlfile,seq;"					<td width=#"425#">"
         write     htmlfile,seq;"						<b>List Name</b>"
         write     htmlfile,seq;"					</td>"
.         write     htmlfile,seq;"					<td width=#"45#">"
.         write     htmlfile,seq;"						<p align=#"center#"><b>Usage</b>"
.         write     htmlfile,seq;"				        </td>"
         write     htmlfile,seq;"				</tr>"


.>Patch 3.8.7
			elseif (HTMLINTRO = "3")
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Exclupdates.html##A#" NAME=#"A#">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##B#">B</a> <a href=#"/Exclupdates.html##C#">C</a> <a href=#"/Exclupdates.html##D#">D</a> <a href=#"/Exclupdates.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##F#">F</a> <a href=#"/Exclupdates.html##G#">G</a> <a href=#"/Exclupdates.html##H#">H</a> <a href=#"/Exclupdates.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##J#">J</a> <a href=#"/Exclupdates.html##K#">K</a> <a href=#"/Exclupdates.html##L#">L</a> <a href=#"/Exclupdates.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##N#">N</a> <a href=#"/Exclupdates.html##O#">O</a> <a href=#"/Exclupdates.html##P#">P</a> <a href=#"/Exclupdates.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##R#">R</a> <a href=#"/Exclupdates.html##S#">S</a> <a href=#"/Exclupdates.html##T#">T</a> <a href=#"/Exclupdates.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##V#">V</a> <a href=#"/Exclupdates.html##W#">W</a> <a href=#"/Exclupdates.html##X#">X</a> <a href=#"/Exclupdates.html##Y#">Y</a> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Z#">Z</a></p>"

.>Patch 3.8.7
	 move c1 to tableopen

         write     htmlfile,seq;"			<table border=#"0#" cellpadding=0 width=#"490#" cellspacing=0>"
         write     htmlfile,seq;"				<tr>"
         write     htmlfile,seq;"					<td width=#"425#">"
         write     htmlfile,seq;"						<b>List Name</b>"
         write     htmlfile,seq;"					</td>"
.         write     htmlfile,seq;"					<td width=#"45#">"
.         write     htmlfile,seq;"						<p align=#"center#"><b>Usage</b>"
.         write     htmlfile,seq;"				        </td>"
         write     htmlfile,seq;"				</tr>"


.>Patch 3.8.7

			endif
         endif
.Patch 3.9 Code Replaced
         GOTO      PRESTART
OUTPUT
         if        (htmlflag <> 2)
         SPLOPEN   PRTFILE
         DISPLAY   *P15:7,PRTNAME
         clear     outname
;START PATCH 3.02 REPLACED LOGIC
;         APPEND    "g:\DATA\" TO OUTNAME
         APPEND    NTWKPATH1 TO OUTNAME
;END PATCH 3.02 REPLACED LOGIC
         APPEND    PRTNAME TO OUTNAME
         APPEND    ".TMP" TO OUTNAME
         RESET     OUTNAME
         PREPARE   OUTPUT,OUTNAME
         DISPLAY   *P01:07,"Output File :":
                   *P15:07,PRTNAME
         ENDIF
;end patch 3.0
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
;begin patch 3.2
               if             (HTML30Flag = 2)
               UNPACK    DATE INTO Mm,STR1,Dd,STR1,Yy
               call           cvtjul
               move           juldays to dateJulian     ;system date used to compare against revdate
               endif
;               call           debug
;end patch 3.2
START
         CALL      HEADER
;
*............................................................
; READ A RECORD FROM THE FILE
;
;begin patch 3.0
READ     DISPLAY   *P1:23,*EL,*HON,"READING";
         if        (Htmlflag <> 2)
.START PATCH 3.5 REPLACED LOGIC
.          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,CATCDE1,CATCDE2:
.                   CATCDE3,CATCDE4,CATCDE5,CATCDE6,CATCDE7,CATCDE8,CATCDE9:
.                   CATCDE10,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                   REVDATE,PASSWORD,MLSTNAME,UNIVERSE,TEXT1
.START PATCH 3.8.5 REPLACED LOGIC
.	READ	INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,COMMPER:
.		HOTLINE,NEWDATE,REVDATE,PASSWORD,MLSTNAME,UNIVERSE,NDATCONV
	READ	INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3:
		HOTLINE,NEWDATE,REVDATE,PASSWORD,MLSTNAME,UNIVERSE,NDATCONV
.END PATCH 3.8.5 REPLACED LOGIC
   if      over
		call    setlr
		goto    totcalc
	endif
.
	clear	TEXT1
	if (NDATCONV = "1")
		move	C1,NDATPATH
		pack	NDATFLD,LSTNUM
		move	"NDATKEY",Location
		pack	KeyLocation,NDATFLD
		call	NDATKEY
.		if (NDATEXCH <> "1")
			pack	NSELFLD1,"01X",LSTNUM
			pack	NSELFLD2,"02XBASE"
			move	"NSELAIM",Location
			pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
			call	NSELAIM
			if not over
;Patch3.6
				clear dim13a
				clear n10
				move	mask13 to dim13a
				move	nselqty to n10
	         ADD   N10 TO TOTUNIV
				edit	n10 to dim13a
				call trim using dim13a
				call trim using nselsname
				clear str30
				packkey str30 with nselsname
				uppercase  str30
				move nselprice to dim9a
				call trim using dim9a
				CALL SelectLoadModifier
;				call trim using
				if (NSELEXC <> "2")
;patch3.6
					pack	TEXT1,dim13a,B1,str30,"$",str25
.					pack	TEXT1,NSELQTY,B1,NSELSNAME,B1,"$",NSELPRICE
				else
					pack	TEXT1,dim13a,B1,str30,"EXCH ONLY"
.					pack	TEXT1,NSELQTY,B1,NSELSNAME,B1,"EXCHANGE ONLY"
				endif
;patch3.6
			else
				goto DataCheckText
			endif
.		endif
	else
DataCheckText
		pack	NTXTFLD,LSTNUM,"1"
		move	"NTXTKEY",Location
		pack	KeyLocation,"Key: ",NTXTFLD
		call	NTXTKEY
		if not over
			call	PARSITUP using text1,ntxttext,C1
.			move	NTXTTEXT,text1
		endif
	endif
.END PATCH 3.5 REPLACED LOGIC
;         GOTO      RIDON IF NOT OVER
;         GOTO      EOJ
;patch 3.8.1 Bug Fix
.                if      over
.                call    setlr
.                goto    totcalc
.                else
                goto    ridon
.                endif
.Patch3.8.1 Bug Fix
         else
         read      input,seq;datvars
         goto      eoj if over
;begin patch 3.3
;patch 3.3 revisited 23 June 2003 Per BLO
;               If             (HtmlFlag = 2 &( lstnum = "006374" | lstnum = "018080" | lstnum = "011947"))
;patch3.8
;               If             (HtmlFlag = 2 & lstnum = "006374")
;               goto           read
;               endif
;patch3.8
;end patch 3.3
;begin patch 3.2
                        If    (HTML30FLag = 2)
;                        call                 debug
.START PATCH 3.5 REPLACED LOGIC
.                        unpack               REVDATE      into mm,str1,dd,str3,yy
                        unpack               REVDATE      into CC,YY,MM,DD
;                    unpack               NDATUPDDATE      into CC,YY,MM,DD

.END PATCH 3.5 REPLACED LOGIC
                        call                 cvtjul
                                       if                   (juldays < (dateJulian-30))
                                       goto                 read
                                       endif
               endif
;end patch 3.2
         endif
;         call      debug
         MOVE      C0 TO PTRFLAG
test1
         IF        (HTMLPTR < 27)
         reset     htmlalpha
         bump      htmlalpha by htmlptr
         move      htmlalpha to bookmark
;lets make sure we are not skipping letters.
         clear     str1
;         move      c0 to skippedletr
         move      mlstname to str1
         scan      str1 in htmlalpha                    ;find letter we are on
         movefptr  htmlalpha to n2
         reset     htmlalpha
;begin patch 3.2
;         sub       htmlptr from n2
               if             ((n2-1) <> htmlptr)
;         if        (n2 > c1 & htmlptr > c1)            .we skipped a letter or more
;         call      debug
                         reset     htmlalpha
                         sub       c1 from n2
                         move      n2 to htmlptr
;                         add       c1 to htmlptr
                         bump      htmlalpha by htmlptr
                         clear     bookmark
                         move      htmlalpha to bookmark
;         move      c1 to skippedletr         .set flag
         endif
;end patch 3.2
;         LOAD      BOOKMARK FROM HTMLPTR OF HTMLALPHA

;         cmatch    "C" to mlstname
;         call      debug if equal
         CMATCH    BOOKMARK TO MLSTNAME
                 IF        EQUAL
                 move      c1 to markflag
                 else
                 move      c0 to markflag
                 endif
         if      (markflag = c1)
 ;        if      (markflag = c1 or skippedletr = c1)
                 MOVE      C1 TO PTRFLAG          .AT WRITE CREATE A BOOKMARK
.                 write     htmlfile,seq;"<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"##A#" Name=#"",Bookmark,"#" >A</a>"
;                         if        (c1 = skippedletr)         .this should handle a skipped letter
;                         reset     htmlalpha
;                         add       c1 to htmlptr
;                         bump      htmlalpha by htmlptr
;                         clear     bookmark
;                         move      htmlalpha to bookmark
;                         write     htmlfile,seq;"<a href=#"##B#" Name=#"",Bookmark,"#" >B</a>"
;                         else
.                         write     htmlfile,seq;"<a href=#"##B#">B</a>"
;                         endif
;end patch 3.2
.Patch 3.9 Code Replaced
			if (HTMLINTRO = "1")
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"##A#"","NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Z#">Z</a></p>"
			elseif (HTMLINTRO = "2")

.>Patch 3.8.7
	if (TableOpen = c1)
	 move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7

         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Excellists.html##A#""," NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Z#">Z</a></p>"
.>Patch 3.8.7
	 move 	   c1 to tableopen
         write     htmlfile,seq;"			<table border=#"0#" width=#"490#" cellpadding=0 cellspacing=0>"
.>Patch 3.8.7
			elseif (HTMLINTRO = "3")
.>Patch 3.8.7
	if (TableOpen = c1)
	 move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Exclupdates.html##A#""," NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"             <a href=#"/Exclupdates.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Z#">Z</a></p>"
.>Patch 3.8.7
	 move 	   c1 to tableopen
         write     htmlfile,seq;"			<table border=#"0#" width=#"490#" cellpadding=0 cellspacing=0>"
.>Patch 3.8.7
			endif
.Patch 3.9 Code Replaced
                 ADD       C1 TO HTMLPTR
              ELSE
                 MOVE      C0 TO PTRFLAG
                 ENDIF
         ENDIF
.START PATCH 3.5 REMOVED LOGIC
.         clear     lin48
.         movefptr  textdata into startfp
.         parse     textdata into lin48 using " ~09",noskip,blankfill,truncate
.
.         movefptr  textdata into endfp
.         add       c1 to n2
.         move      endfp to n5
.         sub       startfp from n5
.         compare   "2256" to startfp
.         goto      packtext if equal       .we have it all
.;
.         if        (n5 > 46)               .wordwrap, no new line char
.         move      startfp to endfp
.         add       "45" to endfp
.         reset     textdata to endfp
.         match     b46 to lin48
.                 if        equal
.                 bump      textdata,-1
.                 endif
.         cmatch    b1 to lin48
.                 if        eos
.                 move      b55 to lin48
.                 endif
.           movelptr  lin48 to n3           .18Mar98
.                   if        (n3 = 0)               .dlh if empty blank fill
.                   move      b55 to lin48
.                   endif
.          endif
.END PATCH 3.5 REMOVED LOGIC
packtext
.START PATCH 3.5 REMOVED LOGIC
.         move     lin48 to text1
.END PATCH 3.5 REMOVED LOGIC
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
;
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
; TEST FOR END OF FILE
; IF END OF FILE: TURN ON LR AND L1-L9
;
testeof
;end patch 3.0
         GOTO      RIDON IF NOT OVER
         DISPLAY   *P1:23,*EL;
         CALL      SETLR
         GOTO      TOTCALC
*............................................................
; TURN ON LEVEL-INDICATOR SWITCHES
;
SETLR    MOVE      ONE TO LR
         RETURN
*............................................................
;
RIDON
         ADD       ONE TO COUNT
         DISPLAY   *P15:12,*EL,"RECORDS READ = ",COUNT;
+............................................................
; TOTAL CALCULATIONS
;
TOTCALC  TRAPCLR   PARITY  (NOP)
TOTCLX   TRAPCLR   PARITY  (NOP)
         CALL      TOTOUT
*............................................................
; SEE IF LR INDICATOR IS ON
; IF SO: END JOB
;
TESTLR   BRANCH    LR OF EOJ
         GOTO      MOVEDATA  (NOP)
*............................................................
; MOVE DATA FROM INPUT AREA TO FIELDS
;
MOVEDATA
         MOVE      B4 TO EXCL        CLEAR EXCLUSIVE PRINT FIELD.
         CMATCH    "C" TO ELSTCDE        EXCLUSIVE?
         CALL      EXCL IF EQUAL         YES
*............................................................
; DETAIL CALCULATIONS
;
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
EXCL     MOVE      "EXCL" TO EXCL
         RETURN
SKIP
         NORETURN
         GOTO      READ
*............................................................
DETLOAD
; LOAD PRINT LINE
         DISPLAY   *P1:23,*EL,*HON,"LOADING PRINT BUFFER";
         BRANCH    LOTSFLAG OF LOAD,LTSOUT
LTSOUT
.START PATCH 3.5 REPLACED LOGIC
.         UNPACK    REVDATE INTO MM,STR1,DD,STR1,cc,YY

;	UNPACK	NDATUPDDATE,CC,YY,MM,DD
	UNPACK	REVDATE,CC,YY,MM,DD
.END PATCH 3.5 REPLACED LOGIC
         CALL      CVTJULTS
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
         rep       "#"'" in mlstname
         reset     text1
         unpack    text1 into str1,text
.START PATCH 3.5 REPLACED LOGIC
.         match     ownnum to nownfld
.         if        not equal
.         move      ownnum to nownfld
.         call      nownkey
.         endif
	unpack	OWNNUM,str2,str4
	match	str4,nownfld
	if not equal
		move	str4,nownfld
		call	nownkey
	endif
.END PATCH 3.5 REPLACED LOGIC
         move      lstnum to nmdlfld
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
;begin patch 3.1
         scan   "|A" in mlstname
         call   undofixita if equal
         reset  Mlstname
         scan   "|T" in mlstname
         call   undofixitThe if equal
         reset  Mlstname
;end patch 3.1

.START PATCH 3.5 REPLACED LOGIC
.         WRITE     OUTPUT,seq;*cdfon,LSTNUM,mlstname,universe,text1:
.                 price,revdate:
.                   mdlplan,mdlcall,STR24
	WRITE	OUTPUT,seq;*cdfon,LSTNUM,mlstname,universe,text1:
		price,revdate:
;		price,NDATUPDDATE:
		mdlplan,mdlcall,STR24

.END PATCH 3.5 REPLACED LOGIC
;         ,NUMBER,",#"",LDESC:
;                   "#",",PRICE,",#"",PSTATUS,"#",",JULDAYS
LOAD
;begin patch 3.0
         if        (htmlflag <> 2)
         BRANCH    PRSW OF LINE1,LINE2,LINE3,LINE4,LINE5
         ENDIF
;begin patch 3.4
         Move      mlstname to str55
         rep       uplow in str55
         scan      "office use" in str55
         goto      read if equal
;end patch 3.4

         scan      "Office Use" in mlstname
         goto      read if equal
         scan      "OFFICE USE" in mlstname
         goto      read if equal
         scan      "Office use" in mlstname
         goto      read if equal
;Patch3.7
			goto		 read if (NDATOFF = "1")
;patch3.8
			goto		 read if (NDATWEB = "1")
;patch3.8
;Patch3.7
;         WRITE     HTMLFILE,SEQ;"<a href=#"Datacards/data",LSTNUM,".htm#">",MLSTNAME:
;                   TEXT1,"</a><br>"
;begin patch 3.1
         scan   "|A" in mlstname
         call   undofixita if equal
         reset  Mlstname
         scan   "|T" in mlstname
         call   undofixitThe if equal
         reset  Mlstname
;end patch 3.1
.Patch 3.9
.         WRITE     HTMLFILE,SEQ;"<a href=#"Datacards/data",LSTNUM,".htm#">",MLSTNAME:
.                   "</a><br>"
.>Patch 3.8.7 Comment Out
.         WRITE     HTMLFILE,SEQ;"<a href=#"http://www.nincal.com/Datacards/data",LSTNUM,".htm#">",MLSTNAME:
.                   "</a><br>"
.>Patch 3.8.7 Comment Out
.>Patch 3.8.7 Code Added Modified
	WRITE     HTMLFILE,SEQ;"                <tr>"
	WRITE     HTMLFILE,SEQ;"			<td width=#"425#">"
        WRITE     HTMLFILE,SEQ;"				<a href=#"/Datacards/data",LSTNUM,".htm#">",MLSTNAME:
								"</a><br>"
	WRITE     HTMLFILE,SEQ;"			</td>"
	WRITE     HTMLFILE,SEQ;"				<td align=#"center#" width=#"45#" valign=#"top#">"


	pack	NUSGFLD1,"01X",LSTNUM
	move	"NUSGAIM",Location
	pack	KeyLocation,"Key: ",NUSGFLD1
	call	NUSGAIM
	If Not over
		if (NDATLUSAGE <> "F")
			WRITE     HTMLFILE,SEQ;"					<a target=#"_blank#" title=#"Click to see Usage#" href=#"/usage/usg",lstnum,".pdf#">Usage</a>"
.		WRITE     HTMLFILE,SEQ;"					<img border=#"0#" src=#"/images/document.gif#" width=#"15#" height=#"13#"></a>"
		Endif
	Endif
	WRITE     HTMLFILE,SEQ;"				</td>"
	WRITE     HTMLFILE,SEQ;"		</tr>"
.>Patch 3.8.7 Code Added Modified
.Patch 3.9
         goto      read
;end patch 3.0
LINE1
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE1 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
	MOVE	MLSTNAME,STR55
;Patch3.6
			pack str1,password
			scan b1 in password
			bump password
			move str1 to str2
			pack str1,password
			pack str3,str2,str1
			move str3,str31a

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
;patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE1A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE1B
;	CALL	TRIM USING NDATUPDDATE
	CALL	TRIM USING REVDATE
;	IF (NDATUPDDATE = "")
	IF (REVDATE = "")
		CLEAR	STR10
	ELSE
		UNPACK	REVDATE,CC,YY,MM,DD
;		UNPACK	NDATUPDDATE,CC,YY,MM,DD
		PACK	STR10,MM,SLASH,DD,SLASH,CC,YY
	ENDIF
	MOVE	STR10,LINE1B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
;
LINE2
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE2 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
	MOVE	MLSTNAME,STR55
;Patch3.6
			pack str1,password
			scan b1 in password
			bump password
			move str1 to str2
			pack str1,password
			pack str3,str2,str1
			move str3,str32a
			call trim using EXCL
			call trim using pstatus
			if (excl = "")
				move pstatus to str52a
			else
				move excl to str52a
			endif
         PACK      LINE2 FROM LSTNUM,B3,STR55,B5:
.                   TEXT1,B1,str3,PSTATUS,b3,EXCL
                   TEXT1
;;Patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE2A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE2B
;	CALL	TRIM USING NDATUPDDATE
	CALL	TRIM USING REVDATE
	IF (REVDATE = "")
;	IF (NDATUPDDATE = "")
		CLEAR	STR10
	ELSE
;		UNPACK	NDATUPDDATE,CC,YY,MM,DD
		UNPACK	REVDATE,CC,YY,MM,DD
		PACK	STR10,MM,SLASH,DD,SLASH,CC,YY
	ENDIF
	MOVE	STR10,LINE2B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
;
LINE3
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE3 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
	MOVE	MLSTNAME,STR55
;Patch3.6
			pack str1,password
			scan b1 in password
			bump password
			move str1 to str2
			pack str1,password
			pack str3,str2,str1
			move str3,str33a
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
;Patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE3A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE3B
	CALL	TRIM USING REVDATE
	IF (REVDATE = "")
		CLEAR	STR10
	ELSE
		UNPACK	REVDATE,CC,YY,MM,DD
		PACK	STR10,MM,SLASH,DD,SLASH,CC,YY
	ENDIF
	MOVE	STR10,LINE3B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
;
LINE4
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE4 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
	MOVE	MLSTNAME,STR55
;Patch3.6
			pack str1,password
			scan b1 in password
			bump password
			move str1 to str2
			pack str1,password
			pack str3,str2,str1
			move str3,str34a
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
;patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE4A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE4B
	CALL	TRIM USING REVDATE
	IF (REVDATE = "")
		CLEAR	STR10
	ELSE
		UNPACK	REVDATE,CC,YY,MM,DD
		PACK	STR10,MM,SLASH,DD,SLASH,CC,YY
	ENDIF
	MOVE	STR10,LINE4B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
;
LINE5
.START PATCH 3.5 REPLACED LOGIC
.         PACK      LINE5 FROM LSTNUM,B3,MLSTNAME,B5:
.                   TEXT1,B1,PSTATUS
	MOVE	MLSTNAME,STR55
;Patch3.6
			clear str1
			clear str2
			clear str3
			pack str1,password
			scan b1 in password
			bump password
			move str1 to str2
			pack str1,password
			pack str3,str2,str1
			move str3,str35a
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

;patch3.6
.END PATCH 3.5 REPLACED LOGIC
         PACK      LINE5A FROM B1,EXCL
.START PATCH 3.5 REPLACED LOGIC
.         MOVE      REVDATE TO LINE5B
	CALL	TRIM USING REVDATE
	IF (REVDATE = "")
		CLEAR	STR10
	ELSE
		UNPACK	REVDATE,CC,YY,MM,DD
		PACK	STR10,MM,SLASH,DD,SLASH,CC,YY
	ENDIF
	MOVE	STR10,LINE5B
.END PATCH 3.5 REPLACED LOGIC
         GOTO      LOADEXIT
;
LOADEXIT
         MOVE      B4 TO EXCL
         DISPLAY   *P1:23,*EL;
         ADD       c1 TO lines
         COMPARE   "5" TO PRSW
         GOTO      DETOUT IF EQUAL
         GOTO      DETOUT IF NOT LESS
         GOTO      READ
* ...........................................................
; HEADING AND DETAIL OUTPUT
;
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
;
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
			move b3 to 		str31a
			move b3 to 		str32a
			move b3 to 		str33a
			move b3 to 		str34a
			move b3 to 		str35a
			move b5 to  str51a
			move b5 to  str52a
			move b5 to  str53a
			move b5 to  str54a
			move b5 to  str55a
         DISPLAY   *P1:23,*EL;
*............................................................
; TURN OFF R.I.D. & ALL L-INDICATORS
;
TURNOFF
         COMPARE   ONE TO LR
         GOTO      RETURN IF EQUAL
         NORETURN
*............................................................
; GO READ ANOTHER RECORD
;
         GOTO      READ
* ...........................................................
; GOTO PRINT TOTAL COUNT
RETURN
         RETURN
*............................................................
; PAGE HEADING ROUTINE
;
PAGE     ADD       ONE TO PAGE
;begin patch 3.0
         if        (htmlflag = 2)
         return
         endif
;end patch 3.0
         BRANCH    UPDFLAG OF PAGE1,PAGE2
PAGE1    BRANCH    LASRFLAG TO PAGE1B,PAGE1A
PAGE1A   COMPARE   C1 TO PAGE
         IF        EQUAL
         compare   c2 to duplflag
           if      equal
           compare  c2 to updflag
           if       equal
;begin patch "2.4"
;         PRINT     HPTMSR17,hpdupl,hptop,hpland
         PRINT     HPTMSR17,hpdupl,hptop,hpland,033,"&l4H"
           else
;         PRINT     HPTMSR17,hpdupl,hptop,*F
         PRINT     HPTMSR17,hpdupl,hptop,033,"&l4H",*F
;end patch "2.4"
           endif
           else
           compare  c2 to updflag
           if       equal
;begin patch "2.4"
         PRINT     HPTMSR17,hptop,hpland,033,"&l4H"
;         PRINT     HPTMSR17,hptop,hpland
           else
;         PRINT     HPTMSR17,hptop,*F
         PRINT     HPTMSR17,hptop,033,"&l4H",*F
;end patch "2.4"
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
;begin patch 3.0
         if        (htmlflag = 2)
         return
         endif
;end patch 3.0
         CALL       PAGE
         BRANCH    UPDFLAG OF HD1,HD2
HD1      PRINT     *2,"LIST##":
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
         branch    lotsflag of hdexit,hd3
HD2      PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE",*134,"UPDATED";
;                   *107,"PRICE",*134,"REVISED";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                   *134,"_______":
                    *C,*L
         branch    lotsflag of hdexit,hd3
hdexit   return
HD3      COMPARE   C1 TO PAGE
         GOTO      HDEXIT IF NOT EQUAL
         WRITE     OUTPUT,SEQ;*cdfon,"#"CONFIDENTIAL#"":
                      "#"NAMES IN THE NEWS#"":
                      "#"",TODAY,"#""
         WRITE      OUTPUT,SEQ;*cdfon,B2,"#"LIST###"":
                   "#"MASTER LIST NAME#"":
                   B6,B6,B6,"#"QUANTITY#"":
                   B2,"#"DESCRIPTION#"":
                   B6,"#"PRICE#"":
                   B6,"#"UPDATED#""
;                   B6,"#"REVISED#""
        GOTO       HDEXIT
ZEROLINE MOVE      C0 TO lines
         MOVE      NO TO SW1P
         RETURN
*............................................................
; TOTAL OUTPUT
;
TOTOUT
;begin patch 3.0
;         if        (htmlflag = 2)
         if        (htmlflag <> 2)
;         return
;         endif
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
;end patch 3.0
* ...........................................................
; F5 - JOB INTERUPTED.
;
F5
         TRAPCLR   F5
         TRAP      f5 IF F5
         DISPLAY   *P1:23,*EL,*B,"JOB ABORTED!!!!!",error,*B,*W2;
         NORETURN
         GOTO      EOJ
* ...........................................................
; int - JOB INTERUPTED.
;
int     TRAPCLR   INT
         TRAP      int giving error IF INT
         DISPLAY   *P1:23,*EL,*B,"JOB ABORTED!!!!!",error,*B,*W2;
         NORETURN
         GOTO      EOJ
* ...........................................................
; END OF JOB
EOJ
;begin patch 3.0
        if        (htmlflag <> 2)
        BRANCH    UPDFLAG OF NOUPD,UPD
        else
        goto      close
;end patch 3.0
        endif
NOUPD
;begin patch 2.4
         print      *f,033,"&l1H"          .reset paper tray select
;      PRINT     *F
;end patch 2.4
         GOTO      CLOSE
;         CLOSE     INPUT
;begin patch 2.4
;UPD       PRINT     *F,033,"@"                  .RESET TO DEFAULTS
UPD       PRINT     *F,033,"@",033,"&l1H"                  .RESET TO DEFAULTS
;end patch 2.4
CLOSE
;begin patch 3.0
         if        (htmlflag <> 2)
         SPLCLOSE
         RELEASE
         endif
         if        (htmlflag <> 2)
         BRANCH    LOTSFLAG TO EOJ1,WEOF
         else
			if (HTMLINTRO = "1")
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"##A#"","NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"##Z#">Z</a></p>"
			elseif (HTMLINTRO = "2")
.>Patch 3.8.7
	if (TableOpen = c1)
	 move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Excellists.html##A#""," NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Excellists.html##Z#">Z</a></p>"
			elseif (HTMLINTRO = "3")
.>Patch 3.8.7
	if (TableOpen = c1)
	 move c0 to TableOpen
         write     htmlfile,seq;"            </table>"
         write     htmlfile,seq;"            <br>"
        endif
.>Patch 3.8.7

         write     htmlfile,seq;""
         write     htmlfile,seq;"          <p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <a href=#"/Exclupdates.html##A#""," NAME=",BOOKMARK,">A</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##B#">B</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##C#">C</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##D#">D</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##E#">E</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##F#">F</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##G#">G</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##H#">H</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"             <a href=#"/Exclupdates.html##I#">I</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##J#">J</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##K#">K</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##L#">L</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##M#">M</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##N#">N</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##O#">O</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##P#">P</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Q#">Q</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##R#">R</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##S#">S</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##T#">T</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##U#">U</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##V#">V</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##W#">W</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##X#">X</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Y#">Y</a>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"            <a href=#"/Exclupdates.html##Z#">Z</a></p>"
			endif
         write     htmlfile,seq;""
         write     htmlfile,seq;"      </dl>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"      <p><br>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"      </p></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"  </tr>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"  <tr bgcolor=#"##D9EF86#">"
         write     htmlfile,seq;""
         write     htmlfile,seq;"    <td height=#"35#" colspan=#"2#" align=#"center#" valign=#"middle#" class=#"footer#"><table width=#"98%#" border=#"0#" cellspacing=#"0#" cellpadding=#"0#">"
         write     htmlfile,seq;""
         write     htmlfile,seq;"        <tr class=#"arial10#"> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <td width=#"22%#"><font color=#"##788146#">&copy; 2005. All Rights Reserved.</font></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <td width=#"78%#" align=#"right#" class=#"cellFooterAddress#"><font color=#"##788146#">1300 "
         write     htmlfile,seq;""
         write     htmlfile,seq;"           Clay St, 11th Floor, Oakland, CA 94612-1429 <font color=#"##FFFFFF#">&middot;</font> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"           T: 415-989-3350 <font color=#"##FFFFFF#">&middot;</font> F: 415-433-7796"
         write     htmlfile,seq;""
         write     htmlfile,seq;"           </font></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"        </tr>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"    </table></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"  </tr>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</table>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"<p>&nbsp;</p>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</body>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"</html>"
         weof      htmlfile,seq
         CLOSE     htmlfile,EOFSIZE
         goto      eoj1
         endif
;end patch 3.0

WEOF     WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT,EOFSIZE

EOJ1
         MOVE      "0.00",LRTOT
         MOVE      C0 TO TOTUNIV
         MOVE      "000",PAGE
         MOVE      C0,lines
         MOVE      C0,LR
         MOVE      YES TO STR1
;         KEYIN     *B,*P10:23,*EL,"May the file you just printed ",*DV,NAME:
;                   " be deleted ? ",*RV,*T5,STR1;
;         CMATCH    YES,STR1
;         GOTO      KILLFLE IF EQUAL
         CLOSE     INPUT
         shutdown  "cls"
;         STOP
*...........................................................................
;list name begins with 'A '
UndoFixitA
        clear     str55
;
        bump      mlstname by -1
        lenset    mlstname
        reset     mlstname
        append    "A " to str55
        append    mlstname to str55
        reset     str55
        clear     Mlstname
        move      str55 to mlstname
        return
;
;list name begins with 'The '
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
; KILLFLE KILL PRINT FILE
KILLFLE
;         PREPARE   INPUT,NAME
;         CLOSE     INPUT,DELETE
          DISPLAY   *P10:23,*EL,NAME," Has been deleted",*B;
;         GOTO      DOMORE
         shutdown   "CLS"
         STOP
*............................................................
; IO ERROR
;
IO       DISPLAY   *P1:1,*ES,"IO error ",error,*B,*B ;
         trapclr   io
         trap      io giving error if io
         GOTO      INPNG

SelectLoadModifier
	pack	NMODFLD,NSELDESC
	rep	zfill,NMODFLD
	move	"D.Load2-NMODKEY",Location
	pack	KeyLocation,"Key: ",NMODFLD
	call	NMODKEY
	call	Trim using NMODDESC
	if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
		pack	str25,dim9a,NMODDESC
		call trim using str25
	else
		pack	str25,"+",dim9a,NMODDESC
		call trim using str25
	endif
	return
.>Patch 3.8.7 Code Added
	include	nusgio.inc
.>Patch 3.8.7 Code Added
         include   nownio.inc
         include   nmdlio.inc
.START PATCH 3.5 ADDED LOGIC
	INCLUDE	NDATIO.INC
	INCLUDE	NTXTIO.INC
	INCLUDE	NSELIO.INC
	include 	nmodio.inc
.END PATCH 3.5 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
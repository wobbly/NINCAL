PC        EQU                 0
          INCLUDE             COMMON.inc
          INCLUDE             CONS.inc
          INCLUDE             NDATDD.inc
          INCLUDE             NUSGDD.INC
          INCLUDE             HP.INC
          Include   NQRCdd.inc          
Release   Init      "1.02"   DLH   change colors on HTML to match new site
REldate   INit      "09 July 2010"
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

            write     htmlfile,seq;"<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">"
            write     htmlfile,seq;"<html>"
            write     htmlfile,seq;"<head>"
            write     htmlfile,seq;"<title>Names in the News</title>"
          write     htmlfile,seq;"<META name=#"description#" content=#"Names in the News provides mailing list brokerage, list management and analytical services to nonprofit organizations as part of their direct mail strategy to acquire new donors and members.#">"
          write     htmlfile,seq;"<META name=#"keywords#" content=#"list, lists, mail, fundraising, fund raising, directmail, direct mail, list broker, list brokerage, list manager, list management, nonprofit,  non-profit, not-for-profit, charitable, environmental, political, consumer, recommendation, Democratic, progressive, Animal Welfare#">"
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
            write     htmlfile,seq;"<body bgcolor=#"##FFFFFF#" text=#"##33FF00#" link=#"##FFFFFF#" vlink=#"##FFFFFF#" alink=#"##FFFFFF#" leftmargin=#"0#" topmargin=#"0#" marginwidth=#"0#" marginheight=#"0#" onLoad=#"MM_preloadImages('/images/navLeft_ON_02.gif','/images/navLeft_ON_03.gif','/images/navLeft_ON_04.gif','/images/navLeft_ON_01.gif','/images/subnavLists_ON_01.gif','/images/subnavLists_ON_02.gif','/images/subnavLists_ON_03.gif')#">"
            write     htmlfile,seq;"<table width=#"740#" border=#"0#" cellspacing=#"5#" cellpadding=#"0#">"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"95#" align=#"center#" valign=#"middle#" bgcolor=#"##2858A6#"><a href=#"index.htm#"><img src=#"/images/logo.gif#" alt=#"Names in the News#" width=#"120#" height=#"64#" border=#"0#"></a></td>"
            write     htmlfile,seq;"    <td width=#"590#" height=#"95#" bgcolor=#"##2858A6#" class=#"header#">&nbsp;</td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"  </tr>"
            write     htmlfile,seq;"  <tr> "
            write     htmlfile,seq;"    <td width=#"130#" height=#"335#" align=#"center#" valign=#"top#" bgcolor=#"##2858A6#"><form name=#"form1#" method=#"post#" action=#"#">"
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
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td height=#"80#" align=#"center#" valign=#"middle#" class=#"cellDma#"><a href=#"http://www.the-dma.org/#" target=#"_blank#"><img src=#"/images/btn_DMAlogo.gif#" alt=#"The DMA#" width=#"67#" height=#"43#" border=#"0#"></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"            <td align=#"center#"><a href='/tos.html';><font size=1 face='arial'>Terms of Service</font></a></td>"
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"          <tr>"
            write     htmlfile,seq;"          <td height=#"80#" align=#"center#" valign=#"middle#" class=#"cellNin#"><strong><span class=#"arialTitle#"><font color=#"##788146#">Mailers</font>:"
            write     htmlfile,seq;"           back to</span></strong><br> <a href=#"http://www.namesinthenews.com#" target=#"_blank#">NamesInTheNews.com</a> "
            write     htmlfile,seq;"          </td>"
            write     htmlfile,seq;""
            write     htmlfile,seq;"          </tr>"
            write     htmlfile,seq;"        </table>"
            write     htmlfile,seq;"      </form></td>"

          Write     htmlfile,seq;""

          if        (n4 = c1)
          write     htmlfile,seq;"<td width=#"590#" height=#"335#" valign=#"top#" class=#"cellBody#"> <p><img src=#"/images/title_animalWelfare.gif#" width=#"200#" height=#"16#"></p>"
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
          write     htmlfile,seq;"<p class=#"MsoNormal#"><span style=#"font-size:11.0pt;color:gray#">Based on "
          write     htmlfile,seq;"12-month continuation usage, these lists have proven to be the most "
          write     htmlfile,seq;"responsive to ",NQRCDDESC," mailers.</span></p><ul>"

         
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
         write     htmlfile,seq;"<class=#"MsoNormal#"></ul>"
         write     htmlfile,seq;"      </dl>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"      <p><br>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"      </p></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"  </tr>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"  <tr bgcolor=#"##2858A6#">"
         write     htmlfile,seq;""
         write     htmlfile,seq;"    <td height=#"35#" colspan=#"2#" align=#"center#" valign=#"middle#" class=#"footer#"><table width=#"98%#" border=#"0#" cellspacing=#"0#" cellpadding=#"0#">"
         write     htmlfile,seq;""
         write     htmlfile,seq;"        <tr class=#"arial10#"> "
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <td width=#"22%#"><span style=#"font-size:9.0pt;color=#"##788146#">&copy; 2008. All Rights Reserved.</font></td>"
.         write     htmlfile,seq;"          <td width=#"22%#"><font color=#"##788146#">&copy; 2008. All Rights Reserved.</font></td>"
         write     htmlfile,seq;""
         write     htmlfile,seq;"          <td width=#"78%#" align=#"right#" class=#"cellFooterAddress#"><span style=#"font-size:9.0pt;color=#"##788146#">180 "
         write     htmlfile,seq;""
         write     htmlfile,seq;"           Grand Ave Suite 1545, Oakland, CA 94612 <font color=#"##FFFFFF#">&middot;</font> "
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
.begin patch 1.01 --- enable googel tracking
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"var gaJsHost = ((#"https:#" == document.location.protocol) ? #"https://ssl.#" : #"http://www.#");"
                              Write     htmlfile,seq;"document.write(unescape(#"%3Cscript src='#" + gaJsHost + #"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E#"))"
                              Write     htmlfile,seq;"</script>"
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"try {var pageTracker = _gat._getTracker(#"UA-6716303-4#");pageTracker._trackPageview();"
                              Write     htmlfile,seq;"} catch(err) {}</script>"
.end patch 1.01
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
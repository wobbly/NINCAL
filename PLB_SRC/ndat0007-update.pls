PC        EQU                 0
          INCLUDE             COMMON.inc
          INCLUDE             CONS.inc
          INCLUDE             NDATDD.inc
          INCLUDE             NUSGDD.INC
          INCLUDE             HP.INC
          Include   NQRCdd.inc          
Release   Init      "1.03"   DLH   change  HTML to match new site   IE remove  http://64.71.27.107 from  http://64.71.27.107/plb-bin ......
REldate   INit      "02 July 2012"
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
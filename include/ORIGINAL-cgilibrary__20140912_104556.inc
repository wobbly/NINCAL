.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.//   PROGRAM:   CGILibary.inc                                                //
.//                                                                           //
.//    AUTHOR:   Brian Jackson (bjackson@adjacency.net)                       //
.//                                                                           //
.//      DATE:   11 APR 2001                                                  //
.//                                                                           //
.// COPYRIGHT:   2001-2004 Adjacency Consulting Group, Inc.                   //
.//              All rights reserved.                                         //
.//                                                                           //
.//  PURPOSE:    Common CGI routines for sending HTTP headers and parsing CGI //
.//                  data.                                                    //
.//                                                                           //
.// REVISION:                                                                 //
.//              VER18    RVW              Add search of S$CMDLIN to CGIParse //
.//              VER17    DLH              Modifications to correct rendering //
.//              VER16    BJJ              Modifications to correct rendering //
.//                                         in FF and IE9.                    //
.//              VER15    DLH              New Website                        //
.//              VER14    DLH              Added                              //
.//              VER13   27APR2006 ASH     Added Questions/Comments tag       //
.//              VER12   06FEB2006 DMB     Added a newline feed to            //
.//                                        displayfile routine                //
.//              VER11   11JAN2006 ASH     Added Terms of Service link        //
.//              VER10   29DEC2005 ASH     Added Website Tracking Logic       //
.//              VER09   19DEC2005       DMB     Added index html code        //
.//                                               for login/logout.pls        //
.//               VER08       14DEC2005 ASH       Added ToolTips              //
.//               VER07       13DEC2005 ASH       Added Consultant fil        //
.//              VER06   17 N0V 2005 ASH    Modified Navigation language      //
.//              VER05   27 SEP 2005 ASH    Modified Navigation menu          //
.//              VER04   19 AUG 2005 ASH    Added Client Number Loading       //
.//              VER03   28 JUL 2005 ASH    Exchange File Conversion          //
.//                                                                           //
.//              VER02   12 NOV 2004 BJACKSON    Added CGIDisplay routine.    //
.//                                               this routine reads an HTML  //
.//                                               file and writes out the     //
.//                                               contents of variables in    //
.//                                               place of ASP-style escape   //
.//                                               tags                        //
.//                                                                           //
.//              VER01   11 APR 2001 BJACKSON    Created                      //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////
errorMsg        dim     %1024
SID             dim     %36
.///////////////////////////////////////////////////////////////////////////////


.///////////////////////////////////////////////////////////////////////////////
#ASCII          dim     01
#buffer         dim     1024
#bufferSave     dim     ^
#CGIVariable    dim     256
#CGIValue       dim     ^
#cwk1           dim     1
#cwk2           dim     2
#cwk1024        dim     1024
#dim5           dim     05
#endTag         dim     40
#environment    dim     ^
#footerFileName dim     80
#genericVar     dim     260
#headerFileName dim     80
#highByte       dim     01
#htmlBuffer     dim     ^
#httBuffer      dim     ^
#httName        dim     260
#labelBuffer    dim     ^
#lowByte        dim     01
#osType         dim     02
#pBuffer        dim     ^
#percentSign    dim     01
#pGenericValue  dim     ^
#selectedCode   dim     260
#SID            dim     36
#sourceData     dim     36
#stateCode      dim     2
#stateName      dim     20
#typeDescription    dim     20
#typeCode       dim     1
#versionString  dim     17
#workString     dim     ^

newline2        dim     ^
STDIN_DATA      dim     ^
QUERY_STRING    dim     ^

#fFooter        file
#fHeader        file
#fHTT           file
#fLookup        file

#form5          form    05
#headersNotSent form    "1"
#seq            form    "-1"
//Begin Patch 09 VAR Added
errormsg2       dim     %1024
cgilogin  dim     %80
//End Patch 09 VAR Added

#CR             init    0x0D
#hexRep         init    "0",0x00:
                        "1",0x01:
                        "2",0x02:
                        "3",0x03:
                        "4",0x04:
                        "5",0x05:
                        "6",0x06:
                        "7",0x07:
                        "8",0x08:
                        "9",0x09:
                        "A",0x0A:
                        "B",0x0B:
                        "C",0x0C:
                        "D",0x0D:
                        "E",0x0E:
                        "F",0x0F
#LF             init    0x0A

#iASCII         integer 01
#iHighByte      integer 01
#iLowByte       integer 01
#length         integer 01

thisSession     record
id              dim     36  // 001-036
ipAddress       dim     15  // 037-051
lastActivity    form    14  // 052-065
userID          dim     36  // 066-101
userType        dim     1   // 102-102
userFName       dim     32  // 103-134
userCompany     dim     6   // 135-140
filler          dim     116 // 141-256
                recordend

#afterPos       integer 4
#lp             integer 4
#fp             integer 4
#len            integer 4

#fileManagerSettings    dim     260
.START PATCH 10 ADDED LOGIC
#DimPtr             dim       ^
#DimPtr1  dim       ^
#strParseRange      INIT      0x00,0xFF       .Allow all values
.END PATCH 10 ADDED LOGIC
.///////////////////////////////////////////////////////////////////////////////
  goto #end                                                                  //
.///////////////////////////////////////////////////////////////////////////////


.///////////////////////////////////////////////////////////////////////////////
.//       CGIInit, a routine to build an appropriate newline2 character        //
.///////////////////////////////////////////////////////////////////////////////
CGIInit routine

    pack PF1 from "HTML"
    smake #environment,256
    smake #labelBuffer,65535
    smake #htmlBuffer,65535

.    // get the os type (10 = Unix, any other, assume DOS/Windows)
    clock version in #versionString
    bump #versionString by 15
    move #versionString to #osType
    if (#osType = "10")
        smake newline2,1
        move #LF to newline2
    else
        smake newline2,2
        pack newline2 from #CR,#LF
    endif

    // get the STDIN data`
    clear #form5
    move "CONTENT_LENGTH" to #environment
    clock env into #environment
    bump #environment by 15
    move #environment to #dim5
    type #dim5
    if equal
        move #dim5 to #form5
        if (#form5 = 0)
            smake STDIN_DATA,1
            smake #workString,256
        else
            smake STDIN_DATA,#form5
            smake #workString,#form5
        endif
    else
        smake STDIN_DATA,1
        smake #workString,256
    endif

    if (#form5 > 0)
        stream *STDIN,STDIN_DATA
    endif

    // get the query string from the environment table
    smake QUERY_STRING,256
    move "QUERY_STRING" to #environment
    clock env,#environment
    bump #environment by 12
    move #environment to QUERY_STRING

    return
.///////////////////////////////////////////////////////////////////////////////


.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.// CGISendHeaders, a routine to send the HTTP, Content-type, and expires     //
.//  HTTP headers.                                                            //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////
CGISendHeaders routine

    stream *stdout,"HTTP/1.0 200 OK",newline2:
                    "Content-type: text/html",newline2:
                    "Expires: Tue 01 Jan 1980, 12:00:00 GMT",newline2,newline2

    clear #headersNotSent

    return
.///////////////////////////////////////////////////////////////////////////////


.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.// CGIPARSE, a routine to extract GET data from the                          //
.//  QUERY_STRING environment variable                                        //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////
CGIParse routine #CGIVariable,#CGIValue

.    // add an equal sign to the CGI Variable name (to ensure a match)
    count #length in #CGIVariable
    endset #CGIVariable
    append "=" to #CGIVariable
    add "1" to #length
    reset #CGIVariable

.   // Robb adds additional place to check S$CMDLIN so that we can pass stuff to it via PHP
    move S$CMDLIN to #workString
    scan #CGIVariable in #workString
    if not equal
.For testing
.	 stream *stdout,"I was looking for the word ",#CGIVariable," in ", #workString, " and I didn't find it. ",newline2,newline2
.        // look for the CGI Variable in the query string
        move STDIN_DATA to #workString
        scan #CGIVariable in #workString
        if not equal
.            // it wasn't in the POST data, so check the QUERY_STRING
            move QUERY_STRING to #workString
            scan #CGIVariable in #workString
            if not equal
.                // it's not in the QUERY_STRING, so check the environment table
                setlptr #CGIVariable to (#length - 1)
                move #CGIVariable to #workString
                clock env,#workString
                if over
.                    // it's not anywhere to be found...
                    setflag over
                    clear #CGIValue
                    return
                endif
            endif
        endif
    endif
.    // get the encoded value
    bump #workString by #length
    if eos
.        // there was no value given and we've run out of data
        clear #CGIValue
        setflag over
        return
    endif

    move #workString to #workString
    scan "&" in #workString
    if equal
        bump #workString by -1
        if eos
.            // there was no value given
            clear #CGIValue
            setflag over
            return
        endif
        lenset #workString
        reset #workString
        move #workString to #workString
    endif

.    // replace plus signs with spaces
    replace "+ " in #workString

.    // replace every HEX code with it's ASCII value
    loop
        scan "%" in #workString
        while equal


.        // move each byte of the hex string into a bucket
        unpack #workString into #percentSign,#highByte,#lowByte

.        // replace the hex string (0-F) with its numeric counterpart (0-15)
        replace #hexRep in #highByte
        replace #hexRep in #lowByte

.        // convert these variables to numbers
        move #highByte to #iHighByte
        move #lowByte to #iLowByte

.        // calculate the ASCII value of the buckets
        calc #iASCII = ((#iHighByte * 16) + #iLowByte)

.        // convert the ASCII code to a string
        move #iASCII to #ASCII

.        // splice the ASCII value into the query string in place of the hex code                                                             //
        splice #ASCII into #workString using 3
        reset #workString
    repeat

.    // move the result to the return value, and clear the over state
    move #workString to #CGIValue
    setflag not over

    return
.///////////////////////////////////////////////////////////////////////////////


.///////////////////////////////////////////////////////////////////////////////
CGIErrorHandler

  call CGISendHeaders if (#headersNotSent)

  stream *stdout,"<hr>",newline2:
                 "An unexpected error has occurred.<br>",newline2:
                 "Error code: '",S$ERROR$,"'<br>",newline2:
                 "<hr>",newline2
  stop
///////////////////////////////////////////////////////////////////////////////
CGIRenderHeader

.START PATCH 08 ADDED LOGIC
.....HERE
          stream *stdout:
     "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Transitional//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"">":
     "<html xmlns=""http://www.w3.org/1999/xhtml"" dir=""ltr"" lang=""en-US"">":
     "<head profile=""http://gmpg.org/xfn/11"">":
.//
.// VER16 BJACKSON --> force IE8 into compatibility mode to render things properly
.//
     newline2,"<meta http-equiv=""X-UA-Compatible"" content=""IE=EmulateIE7"" />",newline2:    
.//
.// VER16 BJACKSON --> end of changes
.//
     "<meta http-equiv=""Content-Type"" content=""text/html; charset=UTF-8"" />":
     "<title>  Data Card Library - Names in the News</title>":
     "<meta name=""generator"" content=""WordPress 1436"" />":
     "<!-- leave this for stats -->":
.    "<link rel=""alternate"" type=""application/rss+xml"" title=""Names in the News &raquo; Data Card Library Comments Feed"" href=""http://www.nincal.com/index.php/list-management/data-card-library/feed/"" />":
.     "<link rel='stylesheet' id='A2A_SHARE_SAVE-css'  href='http://www.nincal.com/wp-content/plugins/add-to-any/addtoany.min.css?ver=1.0' type='text/css' media='' />":
.     "<link rel='stylesheet' id='contact-form-7-css'  href='http://www.nincal.com/wp-content/plugins/contact-form-7/styles.css?ver=2.3' type='text/css' media='all' />":
.     "<script type='text/javascript' src='http://www.nincal.com/wp-includes/js/jquery/jquery.js?ver=1.3.2'></script>":
.     "<link rel=""wlwmanifest"" type=""application/wlwmanifest+xml"" href=""http://www.nincal.com/wp-includes/wlwmanifest.xml"" />":
.     "<link rel='index' title='Names in the News' href='http://www.nincal.com' />":
.     "<link rel='up' title='List Management' href='http://www.nincal.com/index.php/list-management/' />":
.     "<link rel='canonical' href='http://www.nincal.com/index.php/list-management/data-card-library/' />":
.     "<!--[if IE]>":
.     "<style type=""text/css"">":
.     "ul.addtoany_list a img{filter:alpha(opacity=70)}":
.     "ul.addtoany_list a:hover img,ul.addtoany_list a.addtoany_share_save img{filter:alpha(opacity=100)}":
     "</style>":
     "<![endif]-->":
     "<script type=""text/javascript"" src=""http://www.nincal.com/wp-content/plugins/front-slider/scripts/jquery-1.3.2.min.js""></script>":
     "<script type=""text/javascript"" src=""http://www.nincal.com/wp-content/plugins/front-slider/scripts/slider.js""></script>":
     "":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/plugins/mm-forms/stylesheet.css"" type=""text/css"" /><link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/plugins/mm-forms/calendar.css"" type=""text/css"" />      <script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/mm-forms.js'></script>":
     "<script type='text/javascript' src='http://www.nincal.com/wp-content/plugins/mm-forms/calendar.js'></script>":
     "":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/themes/wyntonmagazine/style.css"" type=""text/css"" media=""screen"" />":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/nav.css"" type=""text/css"" media=""screen"" />":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/plugins.css"" type=""text/css"" media=""screen"" />":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/template-style.css"" type=""text/css"" media=""screen"" />":
     "<link rel=""stylesheet"" href=""http://www.nincal.com/wp-content/themes/wyntonmagazine/styles/print.css"" type=""text/css"" media=""print"" />":
     "<script type=""text/javascript"" src=""http://www.nincal.com/wp-content/themes/wyntonmagazine/js/dropdowns.js""></script>":
     "<link rel=""alternate"" type=""application/rss+xml"" title=""Names in the News RSS Feed"" href=""http://www.nincal.com/index.php/feed/"" />":
     "<link rel=""pingback"" href=""http://www.nincal.com/xmlrpc.php"" />":
     "</head>":
     "<body>":
     "<div id=""page"" class=""clearfloat"">":
     "<div class=""clearfloat"">":
     "<div id=""branding"" class=""left"" onclick=""location.href='http://www.nincal.com';"" style=""cursor: pointer;"">":
     "<div class=""blogtitle"" ><a href=""http://www.nincal.com/"">":
     "</a></div>":
     "<div class=""description"">":
     "</div>":
     "</div>":
     "<div class=""right"">":
     "":
     "<META http-equiv=Content-Type content=""text/html; charset=iso-8859-1"">":
     "<LINK href=""nin_styles.css"" type=text/css rel=stylesheet>":
     "<SCRIPT language=JavaScript type=text/JavaScript>":
     "<!--":
     "function MM_preloadImages() { //v3.0":
     "var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();":
     "var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)":
     "if (a[i].indexOf(""##"")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}":
     "}":
     "":
     "function MM_swapImgRestore() { //v3.0":
     "var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;":
     "}":
     "":
     "function MM_findObj(n, d) { //v4.01":
     "var p,i,x;  if(!d) d=document; if((p=n.indexOf(""?""))>0&&parent.frames.length) {":
     "d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}":
     "if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];":
     "for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);":
     "if(!x && d.getElementById) x=d.getElementById(n); return x;":
     "}":
     "":
     "function MM_swapImage() { //v3.0":
     "var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)":
     "if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}":
     "}":
     "//-->":
     "</SCRIPT>":
     "":
     "<META content=""Microsoft FrontPage 4.0"" name=GENERATOR>":
     "<style type=""text/css"">":
     ".style1 {":
     "font-size: x-small;":
     "font-weight: bold;":
     "}":
//
// VER16 BJACKSON --> added #page1 style
//
    newline2:
    "##page {":
    "margin: 0px;":
    "margin-left: 12px;":
    "width:944px;}":
    newline2:              
//
// VER16 BJACKSON --> end of changes
//
     "</style>":
     "<meta name=""verify-v1"" content=""6BHMx2UPpCsHKyuB/beiUpzV9EMeXslsxUB+jo2et8A="" >":
     "</HEAD>":
     "</TD>":
     "</TR>":
     "":
     "</div>":
     "</div>":
     "<ul id=""nav"" class=""clearfloat"">":
     "<li><a href=""http://www.nincal.com/"" class=""on"">HOME</a></li>":
     "<li class=""page_item page-item-2""><a href=""http://www.nincal.com/index.php/about/"" title=""About Us"">About Us</a>":
     "<ul>":
     "<li class=""page_item page-item-21""><a href=""http://www.nincal.com/index.php/about/team/"" title=""Our Team"">Our Team</a></li>":
     "</ul>":
     "</li>":
     "<li class=""page_item page-item-3""><a href=""http://www.nincal.com/index.php/list-brokerage/"" title=""List Brokerage"">List Brokerage</a>":
     "<ul>":
     "<li class=""page_item page-item-144""><a href=""http://www.nincal.com/index.php/list-brokerage/invoice-request/"" title=""Invoice Request"">Invoice Request</a></li>":
     "<li class=""page_item page-item-142""><a href=""http://www.nincal.com/index.php/list-brokerage/order-status/"" title=""Order Status"">Order Status</a></li>":
     "<li class=""page_item page-item-147""><a href=""http://www.nincal.com/index.php/list-brokerage/regional-count-request/"" title=""Regional Count Request"">Regional Count Request</a></li>":
     "</ul>":
     "</li>":
     "<li class=""page_item page-item-6 current_page_ancestor current_page_parent""><a href=""http://www.nincal.com/index.php/list-management/"" title=""List Management"">List Management</a>":
     "<ul>":
     "<li class=""page_item page-item-156 current_page_item""><a href=""http://www.nincal.com/index.php/list-management/data-card-library/"" title=""Data Card Library"">Data Card Library</a></li>":
     "<li class=""page_item page-item-159""><a href=""http://www.nincal.com/index.php/list-management/featured-lists/"" title=""Featured Lists"">Featured Lists</a></li>":
     "<li class=""page_item page-item-161""><a href=""http://www.nincal.com/index.php/list-management/recently-updated-lists/"" title=""Recently Updated Lists"">Recently Updated Lists</a></li>":
     "<li class=""page_item page-item-164""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/"" title=""Recommended Lists"">Recommended Lists</a>":
     "<ul>":
     "<li class=""page_item page-item-444""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/animal-welfare/"" title=""Animal Welfare"">Animal Welfare</a></li>":
     "<li class=""page_item page-item-447""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/catalog/"" title=""Catalog"">Catalog</a></li>":
     "<li class=""page_item page-item-449""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/charitable/"" title=""Charitable"">Charitable</a></li>":
     "<li class=""page_item page-item-452""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/children/"" title=""Children"">Children</a></li>":
     "<li class=""page_item page-item-454""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/civil-rights-human-rights/"" title=""Civil / Human Rights"">Civil / Human Rights</a></li>":
     "<li class=""page_item page-item-457""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/culture-arts/"" title=""Culture / Arts"">Culture / Arts</a></li>":
     "<li class=""page_item page-item-460""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/environmental/"" title=""Environmental"">Environmental</a></li>":
     "<li class=""page_item page-item-462""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/food-banks-missions/"" title=""Food Banks / Missions"">Food Banks / Missions</a></li>":
     "<li class=""page_item page-item-464""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/health-disease/"" title=""Health / Disease"">Health / Disease</a></li>":
     "<li class=""page_item page-item-466""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/jewish/"" title=""Jewish"">Jewish</a></li>":
     "<li class=""page_item page-item-468""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/native-american/"" title=""Native American"">Native American</a></li>":
     "<li class=""page_item page-item-470""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/outdoor-activities/"" title=""Outdoor Activities"">Outdoor Activities</a></li>":
     "<li class=""page_item page-item-472""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/political/"" title=""Political"">Political</a></li>":
     "<li class=""page_item page-item-476""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/public-tv-radio/"" title=""Public TV / Radio"">Public TV / Radio</a></li>":
     "<li class=""page_item page-item-474""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/publications-publishing/"" title=""Publications / Publishing"">Publications / Publishing</a></li>":
     "<li class=""page_item page-item-478""><a href=""http://www.nincal.com/index.php/list-management/recommended-lists/womens-rights/"" title=""Women&#8217;s Rights"">Women&#8217;s Rights</a></li>":
     "</ul>":
     "</li>":
     "</ul>":
     "</li>":
     "<li class=""page_item page-item-8""><a href=""http://www.nincal.com/index.php/servicestools/"" title=""Services/Tools"">Services/Tools</a>":
     "<ul>":
     "<li class=""page_item page-item-297""><a href=""http://www.nincal.com/index.php/servicestools/acquisition-advisor/"" title=""Acquisition Advisor"">Acquisition Advisor</a></li>":
     "<li class=""page_item page-item-239""><a href=""http://www.nincal.com/index.php/servicestools/faqs-advanced/"" title=""FAQs &##8211; Advanced"">FAQs &##8211; Advanced</a></li>":
     "<li class=""page_item page-item-44""><a href=""http://www.nincal.com/index.php/servicestools/faqs/"" title=""FAQs &##8211; The Basics"">FAQs &##8211; The Basics</a></li>":
     "</ul>":
     "</li>":
     "<li class=""page_item page-item-10""><a href=""http://www.nincal.com/index.php/marketing/"" title=""Multi-Channel Marketing"">Multi-Channel Marketing</a></li>":
     "<li class=""page_item page-item-12""><a href=""http://www.nincal.com/index.php/clients/"" title=""Clients/Testimonials"">Clients/Testimonials</a></li>":
     "<li class=""page_item page-item-14""><a href=""http://www.nincal.com/index.php/contact-us/"" title=""Contact Us"">Contact Us</a>":
     "<ul>":
     "<li class=""page_item page-item-202""><a href=""http://www.nincal.com/index.php/contact-us/email-us/"" title=""Email Us"">Email Us</a></li>":
     "</ul>":
     "</li>":
     "</ul>":
     "</dd>":
     "</dl>":
     "<p><input id=""gwProxy"" type=""hidden"" /> <input id=""jsProxy"" onclick=""jsCall();"" type=""hidden"" /> <input id=""gwProxy"" type=""hidden"" /> <input id=""jsProxy"" onclick=""jsCall();"" type=""hidden"" /></p>":
.     "<div class=""addtoany_share_save_container""><ul class=""addtoany_list""><li><a class=""a2a_dd addtoany_share_save"" href=""http://www.addtoany.com/share_save?linkurl=http%3A%2F%2Fwww.nincal.com%2Findex.php%2Flist-management%2Fdata-card-library%2F&amp;linkname=Data%20Card%20Library""><img src=""http://www.nincal.com/wp-content/plugins/add-to-any/share_save_171_16.png"" width=""171"" height=""16"" alt=""Share/Bookmark""/></a></li></ul>":
     "<script type=""text/javascript"">":
     "var a2a_config = a2a_config || {};":
     "a2a_config.linkname=""Data Card Library"";":
     "a2a_config.linkurl=""http://www.nincal.com/index.php/list-management/data-card-library/"";":
     "</script><script type=""text/javascript"" src=""http://static.addtoany.com/menu/page.js""></script>":
     "":
.     "</div>":
     "":
     "<p>":
     "":
     "</p>":
     "":
     "</div>":
     "</div>":
     "</div>":
     "":
     "":
     "</body>":
     "</html>":
     ""


.END PATCH 08 ADDED LOGIC


    move "variables;CGIHeader" to #headerFileName
    clock ini,#headerFileName
.
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open #fHeader,#headerFileName,read
    setmode *openuseip=#fileManagerSettings
    loop
        read #fHeader,#seq;#cwk1024
        until over
        stream *STDOUT,#cwk1024,newline2
    repeat

    call cgiRenderMenu

    return
///////////////////////////////////////////////////////////////////////////////
CGIRenderFooter

    move "variables;CGIFooter" to #footerFileName
    clock ini,#footerFileName
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open #fFooter,#footerFileName,read
    setmode *openuseip=#fileManagerSettings
    loop
        read #fFooter,#seq;#cwk1024
        until over
        stream *STDOUT,#cwk1024,newline2
    repeat

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
CGIDisplayFile lroutine #httName

    smake #httBuffer,65535
    clear #httBuffer

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open #fHTT,#httName,read
    setmode *openuseip=#fileManagerSettings
    loop
        read #fHTT,#seq;*ll,#buffer
        until over
        append #buffer to #httBuffer
.//Ver12 Patch
        append newline2 to #httBuffer
.//Ver12 Patch End
    repeat

    reset #httBuffer
    call CGIDisplay using #httBuffer
    stream *stdout,#httBuffer

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
CGIDisplay lroutine #pBuffer

    loop
        scan "<%" in #pBuffer
        until not equal

        movelptr #pBuffer to #lp
        movefptr #pBuffer to #fp
        scan "%>" in #pBuffer
        if not equal
            // missing an end tag
            // TO DO: throw error!
        endif

        // make sure we include the '>' in our logical string
        bump #pBuffer by 1
        lenset #pBuffer
        reset #pBuffer to #fp
        move #pBuffer to #sourceData
        movelptr #sourceData to #len
        setlptr #pBuffer to #lp

.        // TO DO: validate for proper label format

.        // figure out what we're looking at
        unpack #sourceData into #cwk2,#cwk1
        if (#cwk1 = "=")
            // we're displaying the contents of a plb variable - step one
            //  is to find out what is between the tags
            bump #sourceData by 3
            move #sourceData to #genericVar
            endset #genericVar
            bump #genericVar by -2
            lenset #genericVar
            reset #genericVar
            uppercase #genericVar

            // now that we have a variable name, uppercase it because that's how
            //  PL/B handles globals
            moveptr 0,#pGenericValue
            movegaddr #genericVar to #pGenericValue
            if over
                // there was no PL/B variable that matched the one from the template
                splice "" into #pBuffer with #len
            else
                // #pGenericValue now contains the value of the requested variable
                //  so replace the whole escape tag with the value
                splice #pGenericValue into #pBuffer with #len
            endif
        else
            // this is a label name so, we'll try to call that label
            bump #sourceData by 2
            move #sourceData to #genericVar
            endset #genericVar
            bump #genericVar by -2
            lenset #genericVar
            reset #genericVar
            uppercase #genericVar

            move #pBuffer to #labelBuffer
            bump #labelBuffer by #len
            pack #labelBuffer from #labelBuffer
            pack #endTag from "<%/",#genericVar,"%>"
            scan #endTag in #labelBuffer
            if not equal
                // we have a fatal error - no closing tag
                stream *stdout,"<hr>The server encountered a fatal error.  A label reference was missing its closing tag.<br>":
                    "Label Name: ",#genericVar,"<br>":
                    "Error Code: ",s$error$,"<hr>",newline2
                shutdown
            else
                bump #labelBuffer by -1
                lenset #labelBuffer
                reset #labelBuffer
                packkey #labelBuffer from #labelBuffer
                chop #labelBuffer

                movelptr #labelBuffer to #afterPos
                movefptr #pBuffer to #fp
                movelptr #pBuffer to #lp
                add #afterPos to #fp
                add #len to #fp
                add #len to #fp
                add "1" to #fp

                bump #pBuffer by -1
                lenset #pBuffer
                reset #pBuffer
                stream *stdout,#pBuffer,newline2
                setlptr #pBuffer to #lp
                reset #pBuffer to #fp
                pack #htmlBuffer from #pBuffer
                moveptr #pBuffer to #bufferSave
                trap #noLabel if cfail
                    calls #genericVar using #labelBuffer
                trapclr cfail
                moveptr #bufferSave to #pBuffer
                move #pBuffer to #pBuffer
            endif
        endif
    repeat
    reset #pBuffer

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
.begin patch 14
#selectedmlrnumber dim 6
.end patch 14
#selectedListnumber dim 6
#compNumber         dim 6
#error              integer 4
#userType           dim 1
ilist               integer 4
#listLabel          dim 260
mailerList          dim 260
.START PATCH 03 REPLACED LOGIC
.mailer              dim 4
mailer              dim 6
.END PATCH 03 REPLACED LOGIC
mailerCount         integer 4
///////////////////////////////////////////////////////////////////////////////
CGIRenderListNumberInput lroutine #selectedListnumber,#compNumber,#userType,#error

    debug
    if (#userType = "A" or #userType = "B")
        // render an edit text
        stream *stdout,"<input type='text' name='cgilistnum' size=10 maxlength=6 />",newline2
    elseif (#userType = "C")
        // render a select box based on the company number for this client
        move #compNumber to compFld
        clear compOldMlr
.START PATCH 03 ADDED LOGIC
        clear COMPNUM
.END PATCH 03 ADDED LOGIC
        call compKey
        if (compOldMlr = "    ")
            stream *stdout,"<input type='text' name='cgilistnum' size=10 maxlength=6 />",newline2
        else
            stream *stdout,"<select name='cgilistnum'>",newline2
.START PATCH 03 REPLACED LOGIC
.            move COMPOLDMLR to NXRFFLD2
            move COMPNUM to NXRFFLD2
.END PATCH 03 REPLACED LOGIC
            move "2" to NXRFPATH
            call nxrfKey
            loop
                until over
.START PATCH 03 REPLACED LOGIC
.                while (nxrfMlr = compOldMlr)
                while (nxrfMlr = COMPNUM)
.END PATCH 03 REPLACED LOGIC
                packkey NDATFLD from nxrfList
                call nDatKey
                stream *stdout,"<option value='",nxrfList,"'>",nxrfList," - ",oLstName,newline2
                call nxrfks
            repeat
            stream *stdout,"</select>"
        endif
    elseif (#userType = "O")
        // render a select box based on the company number for this consultant
        clear compOldMlr
.START PATCH 07 REPLACED LOGIC
.        clear compfld12
.        pack compfld11 from "01X",#compNumber
.        call compaim2
.        loop
.            until over
.            // add Mailer to combo box
..START PATCH 03 REPLACED LOGIC
..            if (compoldmlr != "    ")
..                append compoldmlr to mailerList
.            if (COMPMLRFLG = "T")
.                append COMPNUM to mailerList
..END PATCH 03 REPLACED LOGIC
.            endif
.            call compkg2
.        repeat
..................................
          clear     NCLTFLD2
          pack      NCLTFLD1,"01X",#compNumber
          call      NCLTAIM
          loop
                    until over
                    // add Mailer to validMailers
                    pack      COMPFLD,NCLTCLIENT
                    call      COMPKEY
                    if not over
                              if (COMPMLRFLG = "T")
                              append    COMPNUM,mailerList
                              endif
                    endif
                    call      NCLTKG
          repeat
.END PATCH 07 REPLACED LOGIC
        reset mailerList

        count mailerCount in mailerList
        if not zero
            stream *stdout,"<select name='cgilistnum'>",newline2
            loop
                until (mailerCount = 0)
                move mailerList into mailer
                packkey nxrffld2 from mailer
                move "2" into nxrfpath
                call nxrfkey
                loop
                    until over
                    while (nxrfmlr = mailer)
                    packkey ndatfld from nxrflist
                    call ndatkey
                    stream *stdout,"<option value='",nxrfList,"'>",nxrfList," - ",oLstName,newline2
                    call nxrfks
                repeat
.START PATCH 03 REPLACED LOGIC
.                bump mailerList by 4
                bump mailerList by 6
.END PATCH 03 REPLACED LOGIC
                count mailerCount in mailerList
                if eos
                    break
                endif
            repeat
            debug
            stream *stdout,"</select>"
        else
            stream *STDOUT,"<td colspan='1' align='center' class='datarow1'>We're sorry, we can't locate any records<br>associated with your agency.<br>":
                    "If this is in error, please inform <br>your NIN contact so we can <br>trouble-shoot the problem.<br>Thank you.</td></tr>"
            return
        endif
    endif

    return
///////////////////////////////////////////////////////////////////////////////

.START PATCH 04 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
CGIRenderClientNumberInput lroutine #selectedListnumber,#compNumber,#userType,#error
          debug
          if (#userType = "A" or #userType = "B")
.Not yet in use!!
.// render an edit text
.begin patch 2.3 === lite it up
        stream *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=6 />",newline2
.end patch 2.3 === lite it up
.                   stream    *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=6 />",newline2
          elseif (#userType = "C")
.Not yet in use!!
.begin patch 2.3 === lite it up
        move #compNumber to compnum
        if (compnum = "      ")
            stream *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=6 />",newline2
        else
            stream *stdout,"<select name='cgimailernum'>",newline2
                stream *stdout,"<option value='",compnum,"'>",compnum," - ",compcomp,newline2
            stream *stdout,"</select>"
        endif
.end patch 2.3 === lite it up
.// render a select box based on the company number for this client
          elseif (#userType = "O")
.// render a select box based on the company number for this consultant
                    move      C0,N3
.START PATCH 07 REPLACED LOGIC
.                   clear     compfld12
.                   pack      compfld11,"01X",#compNumber
.                   call      compaim2
.                   loop
.                             until over
..// add Mailer to combo box
.                             if (COMPMLRFLG = "T")
.                                       add       C1,N3
.                                       if (N3 = C1)
.                                                 stream *stdout,"<select name='cgimailernum'>",newline2
.                                       endif
.                                       stream    *stdout,"<option value='",COMPNUM,"'>",COMPNUM," - ",COMPCOMP,newline2
.                             endif
.                             call compkg2
.                   repeat
..................................
                    clear     NCLTFLD2
                    pack      NCLTFLD1,"01X",#compNumber
                    call      NCLTAIM
                    loop
                              until over
                              pack      COMPFLD,NCLTCLIENT
                              call      COMPKEY
                              if not over
.// add Mailer to combo box
                                        if (COMPMLRFLG = "T")
                                                  add       C1,N3
                                                  if (N3 = C1)
                                                            stream *stdout,"<select name='cgimailernum'>",newline2
                                                  endif
                                                  stream    *stdout,"<option value='",COMPNUM,"'>",COMPNUM," - ",COMPCOMP,newline2
                                        endif
                              endif
                              call      NCLTKG
                    repeat
.END PATCH 07 REPLACED LOGIC
                    if (N3 = C0)
                              stream *STDOUT,"<td colspan='1' align='center' class='datarow1'>We're sorry, we can't locate any records<br>associated with your agency.<br>":
                              "If this is in error, please inform <br>your NIN contact so we can <br>trouble-shoot the problem.<br>Thank you.</td></tr>"
                              return
                    else
                              stream *stdout,"</select>"
                    endif
          endif
          return
///////////////////////////////////////////////////////////////////////////////
.END PATCH 04 ADDED LOGIC
.begin patch 14
///////////////////////////////////////////////////////////////////////////////
CGIRenderMLRNumberViewOrderInput lroutine #selectedMLRnumber,#compNumber,#userType

    debug
    if (#userType = "A" or #userType = "B")
        // render an edit text
        stream *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=4 />",newline2
    elseif (#userType = "C")
        // render a select box based on the company number for this client
        move #compNumber to compFld
        clear compOldMlr
        clear COMPNUM
        clear COMPcomp
        call compKey
        if (compOldMlr = "    ")
            stream *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=4 />",newline2
        else
            stream *stdout,"<select name='cgimailernum'>",newline2
                stream *stdout,"<option value='",compoldmlr,"'>",compoldmlr," - ",compcomp,newline2
            stream *stdout,"</select>"
        endif
    elseif (#userType = "O")
        // render a select box based on the company number for this consultant
        clear compOldMlr
          clear     NCLTFLD2
          pack      NCLTFLD1,"01X",#compNumber
          call      NCLTAIM
          loop
                    until over
                    // add Mailer to validMailers
                    pack      COMPFLD,NCLTCLIENT
                    call      COMPKEY
                    if not over
                              if (COMPMLRFLG = "T")
                              append    COMPNUM,mailerList
                              endif
                    endif
                    call      NCLTKG
          repeat
        reset mailerList

        count mailerCount in mailerList
        if not zero
            stream *stdout,"<select name='cgimailernum'>",newline2
            loop
                until (mailerCount = 0)
                move mailerList into mailer
          Packkey compFld from mailer
          clear compOldMlr
          clear COMPNUM
          clear COMPcomp
          call compKey
                    stream *stdout,"<option value='",Compoldmlr,"'>",Compoldmlr," - ",compComp,newline2
                bump mailerList by 6
                count mailerCount in mailerList
                if eos
                    break
                endif
            repeat
            debug
            stream *stdout,"</select>"
        else
            stream *stdout,"<input type='text' name='cgimailernum' size=10 maxlength=4 />",newline2
        endif
    endif

    return
///////////////////////////////////////////////////////////////////////////////
.end patch 14

///////////////////////////////////////////////////////////////////////////////
CGIRenderListNumberViewOrderInput lroutine #selectedListnumber,#compNumber,#userType

    debug
    if (#userType = "A" or #userType = "B")
        // render an edit text
        stream *stdout,"<input type='text' name='cgilistnum' size=10 maxlength=6 />",newline2
    elseif (#userType = "C")
        // render a select box based on the company number for this client
        move #compNumber to compFld
        clear compOldMlr
.START PATCH 03 ADDED LOGIC
        clear COMPNUM
.END PATCH 03 ADDED LOGIC
        call compKey
        if (compOldMlr = "    ")
            stream *stdout,"<input type='text' name='cgilistnum' size=10 maxlength=6 />",newline2
        else
            stream *stdout,"<select name='cgilistnum'>",newline2
.START PATCH 03 REPLACED LOGIC
.            move COMPOLDMLR to NXRFFLD2
            move COMPNUM to NXRFFLD2
.END PATCH 03 REPLACED LOGIC
            move "2" to NXRFPATH
            call nxrfKey
            loop
                until over
.START PATCH 03 REPLACED LOGIC
.                while (nxrfMlr = compOldMlr)
                while (nxrfMlr = COMPNUM)
.END PATCH 03 REPLACED LOGIC
                packkey NDATFLD from nxrfList
                call nDatKey
                stream *stdout,"<option value='",nxrfList,"'>",nxrfList," - ",oLstName,newline2
                call nxrfks
            repeat
            stream *stdout,"</select>"
        endif
    elseif (#userType = "O")
        // render a select box based on the company number for this consultant
        clear compOldMlr
.START PATCH 07 REPLACED LOGIC
.        clear compfld12
.        pack compfld11 from "01X",#compNumber
.        call compaim2
.        loop
.            until over
.            // add Mailer to combo box
..START PATCH 03 REPLACED LOGIC
..            if (compoldmlr != "    ")
..                append compoldmlr to mailerList
.            if (COMPMLRFLG = "T")
.                append COMPNUM to mailerList
..END PATCH 03 REPLACED LOGIC
.            endif
.            call compkg2
.        repeat
..................................
          clear     NCLTFLD2
          pack      NCLTFLD1,"01X",#compNumber
          call      NCLTAIM
          loop
                    until over
                    // add Mailer to validMailers
                    pack      COMPFLD,NCLTCLIENT
                    call      COMPKEY
                    if not over
                              if (COMPMLRFLG = "T")
                              append    COMPNUM,mailerList
                              endif
                    endif
                    call      NCLTKG
          repeat
.END PATCH 07 REPLACED LOGIC
        reset mailerList

        count mailerCount in mailerList
        if not zero
            stream *stdout,"<select name='cgilistnum'>",newline2
            loop
                until (mailerCount = 0)
                move mailerList into mailer
                packkey nxrffld2 from mailer
                move "2" into nxrfpath
                call nxrfkey
                loop
                    until over
                    while (nxrfmlr = mailer)
                    packkey ndatfld from nxrflist
                    call ndatkey
                    stream *stdout,"<option value='",nxrfList,"'>",nxrfList," - ",oLstName,newline2
                    call nxrfks
                repeat
.START PATCH 03 REPLACED LOGIC
.                bump mailerList by 4
                bump mailerList by 6
.END PATCH 03 REPLACED LOGIC
                count mailerCount in mailerList
                if eos
                    break
                endif
            repeat
            debug
            stream *stdout,"</select>"
        else
            stream *stdout,"<input type='text' name='cgilistnum' size=10 maxlength=6 />",newline2
        endif
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#selectedOrdernumber dim 6
//#compNumber dim 6
//#userType dim 1
CGIRenderOrderNumberInput lroutine #selectedordernumber,#compNumber,#userType


    move #compNumber to compFld
    clear compOldBrk
    clear compOldMlr
    call compKey
    if (#userType = "O")
        // render a select box based on the company number for this consultant
        clear compOldMlr
.START PATCH 07 REPLACED LOGIC
.        clear compfld12
.        pack compfld11 from "01X",#compNumber
.        call compaim2
.        loop
.            until over
.            // add Mailer to combo box
..START PATCH 03 REPLACED LOGIC
..            if (compoldmlr != "    ")
..                append compoldmlr to mailerList
.            if (COMPMLRFLG = "T")
.                append COMPNUM to mailerList
..END PATCH 03 REPLACED LOGIC
.            endif
.            call compkg2
.        repeat
..................................
          clear     NCLTFLD2
          pack      NCLTFLD1,"01X",#compNumber
          call      NCLTAIM
          loop
                    until over
                    // add Mailer to validMailers
                    pack      COMPFLD,NCLTCLIENT
                    call      COMPKEY
                    if not over
                              if (COMPMLRFLG = "T")
                              append    COMPNUM,mailerList
                              endif
                    endif
                    call      NCLTKG
          repeat
.END PATCH 07 REPLACED LOGIC
        reset mailerList

        count mailerCount in mailerList
        if not zero
            stream *stdout,"<select name='cgiordernumber'>",newline2
            stream *stdout,"<option value='",oLRN,"'>","  ",newline2
            loop
                until (mailerCount = 0)
                move mailerList into mailer
.START PATCH 03 REPLACED TEMPORARY LOGIC
.                packkey nordfld1 from "01X",mailer
                    move      mailer,COMPFLD
                    call      COMPKEY
                    call      Trim COMPOLDMLR
                packkey nordfld1 from "01X",COMPOLDMLR
.END PATCH 03 REPLACED TEMPORARY LOGIC
                call nordaim
                loop
                    until over
.START PATCH 03 REPLACED TEMPORARY LOGIC
.                    while (oMlrNum = mailer)
                    while (oMlrNum = COMPOLDMLR)
.END PATCH 03 REPLACED TEMPORARY LOGIC
                    stream *stdout,"<option value='",oLRN,"'>",oLRN,newline2
                    call NORDKG
                repeat
.START PATCH 03 REPLACED TEMPORARY LOGIC
.                bump mailerList by 4
                bump mailerList by 6
.END PATCH 03 REPLACED TEMPORARY LOGIC
                count mailerCount in mailerList
                if eos
                    break
                endif
            repeat
            debug
            stream *stdout,"</select>"
        else
            stream *stdout,"<input type='text' name='cgiordernumber' size=10 maxlength=6 />",newline2
        endif
    else
        stream *stdout,"<input type='text' name='cgiordernumber' size=10 maxlength=6 />",newline2
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CGISelectState lroutine #selectedCode

    // CGISelectState generates an HTML select containing state codes
    //  for all the states contained in LUSTATES.DAT.  If a value
    //  is passed into the calling routine (#selectedCode), then that
    //  value will be highlighted and selected when the SELECT is rendered
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open #fLookup,"LUSTATES.DAT"
    setmode *openuseip=#fileManagerSettings
    stream *stdout,"<select name='cgistate'>",newline2
    loop
        read #fLookup,#seq;*ll,#stateCode,#stateName
        until over
        if (#selectedCode = #stateCode)
            stream *stdout,"<option value='",#stateCode,"' selected>"
        else
            stream *stdout,"<option value='",#stateCode,"'>"
        endif
        stream *stdout,#stateCode," - ",#stateName,"</option>",newline2
    repeat
    stream *stdout,"</select>",newline2
    close #fLookup

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CGISelectUserType lroutine #selectedCode

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open #fLookup,"LUUSERTYPES.DAT"
    setmode *openuseip=#fileManagerSettings
    stream *stdout,"<select name='cgiusertype'>",newline2
    loop
        read #fLookup,#seq;*ll,#typeCode,#typeDescription
        until over
        if (#selectedCode = #typeCode)
            stream *stdout,"<option value='",#typeCode,"' selected>"
        else
            stream *stdout,"<option value='",#typeCode,"'>"
        endif
        stream *stdout,#typeCode," - ",#typeDescription,"</option>",newline2
    repeat
    stream *stdout,"</select>",newline2
    close #fLookup

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CGISelectInvoiceFlag lroutine #selectedCode

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""

    stream *stdout,"<select name='cgiinvoiceflag'>",newline2
    stream *stdout,"<option value='Y' "
    if (#selectedCode = "Y")
        stream *stdout,"selected"
    endif
    stream *stdout,">Y - Approved",newline2

    stream *stdout,"<option value='P' "
    if (#selectedCode = "P")
        stream *stdout,"selected"
    endif
    stream *stdout,">P - Pending",newline2

    stream *stdout,"<option value='N' "
    if (#selectedCode != "Y" and #selectedCode != "P")
        stream *stdout,"selected"
    endif
    stream *stdout,">N - No Access",newline2

    stream *stdout,"</select>",newline2

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CGISelectCompanyNumber lroutine #selectedCode

    trap compGone if IO
        open compFList,read
    trapclr IO
    move c1 to compFlag

    stream *stdout,"<select name='cgicompanynumber'>",newline2:
        "<option value='      '></option>"
    loop
        call compKS
        until over
        if (#selectedCode = compNum)
            stream *stdout,"<option value='",compNum,"' selected>"
        else
            stream *stdout,"<option value='",compNum,"'>"
        endif
        chop compComp
        stream *stdout,compNum," - ",compComp,"</option>"
    repeat
    stream *stdout,"</select>",newline2
    close compFList

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CGIRenderMenu
	stream *stdout:
	        "<div id=#"container#" style=#"background: ##FFFFFF; margin: 0 auto; border: 1px solid ##000000; text-align: left;#">",newline2:
    		"<div id=#"sidebar1#" style=#"float: left; width: 12em; background: ##EBEBEB; padding: 15px 0;#">",newline2:
		"<b><center>Welcome, ",thisSession.userFName,"</center></b>",newline2

		if (thisSession.userType = "C")
	        // render the client menu
	
	        stream *stdout:
	        "<form name=usermaintForm method='POST' action='/plb-bin/usermaint.plc'>",newline2:
		"<input type=hidden name='SID' value='",thisSession.id,"' />",newline2:
		"<input type=hidden name='cgiuserid' value='",thisSession.id,"' />",newline2:
                "</form>",newline2:
	        "<a class='menuHdrText'>Client Menu</a><br />",newline2:
	        "<hr />":
	        "<a href=""/index.php/exchangereport"" onClick='document.myForm.submit();return false'>text link</A>":
.	        "<a class='menuText' href='javascript:doMenuItem(""/index.php/exchangereport"");' onMouseOver='this._tip=""View current exchange balances"";' onMouseOut='setStatus(""""); return true;'>EXCHANGE REPORT</a><br />",newline2:
.		"<a href=""/index.php/exchangereport"">EXCHANGE REPORT</a><br />",newline2:
		"<a href=""/index.php/owneractivityrpt.plc"">OWNER ACTIVITY REPORT</a><br />",newline2:
		"<a href=""/index.php/statusrpt.plc"">VIEW ORDER</a><br />",newline2:
		"<a href=""/index.php/onlineinvoicing.plc"">VIEW INVOICE</a><br />",newline2:
		"<a href=""/index.php/listsearch.plc"">DATACARDS &amp; SAMPLES</a><br />",newline2:
		"<a href=""/index.php/usermaint.plc"">EDIT PROFILE</a><br />",newline2:
		"<a href=""/index.php/logout.plc"">LOG OUT</a><br />",newline2:
		"<a href='/index.php/terms-of-service/';><font size=1 face='arial'>Terms of Service</font></a>",newline2:
	        "<a href='/plb-bin/comments.plc';><font size=1 face='arial'>Questions/Comments?</font></a>",newline2:
	    	"<hr />":
		"<a href='../htdocs/tos.html';><font size=1 face='arial'>Terms of Service</font></a>",newline2:
		"<a href='/plb-bin/comments.plc';><font size=1 face='arial'>Questions/Comments?</font></a>",newline2		
	//
	// 2010-09-13 BJACKSON --> removed width from TD
	//
	// 2012-11-2 DLH ---> restore width to TD
	//            "<td width='1000' valign='top' class='body'>",newline2
	//            "<td valign='top' class='body'>",newline2
	//
	// 2010-09-13 BJACKSON --> end of changes
	//            
	    elseif (thisSession.userType = "O")
	
	        // render the consultant menu
	
	        stream *stdout:
		"<a class='menuHdrText'>Consultant Menu</a><br />",newline2:
	    	"<hr />",newline2:
	    	"<a href=""/index.php/exchangereport"">EXCHANGE REPORT</a><br />",newline2:
	    	"<a href=""/index.php/statusrpt.plc"">VIEW ORDER</a><br />",newline2:
	    	"<a href=""/index.php/onlineinvoicing.plc"">VIEW INVOICE</a><br />",newline2:
	    	"<a href=""/index.php/listsearch.plc"">DATACARDS &amp; SAMPLES</a><br />",newline2:
	    	"<a href=""/index.php/usermaint.plc"">EDIT PROFILE</a><br />",newline2:
	    	"<a href=""/index.php/logout.plc"">LOG OUT</a><br />",newline2:
		"<a href='/index.php/terms-of-service/';><font size=1 face='arial'>Terms of Service</font></a>",newline2:
	        "<a href='/plb-bin/comments.plc';><font size=1 face='arial'>Questions/Comments?</font></a>",newline2:
	        "<hr />"
	//
	// 2010-09-13 BJACKSON --> removed width from TD
	//
	//            "<td width='600' valign='top' class='body'>",newline2
	// 2012-11-2 DLH ---> restore width to TD and increased from 600
	//            "<td width='1000' valign='top' class='body'>",newline2
	//            "<td valign='top' class='body'>",newline2
	//
	// 2010-09-13 BJACKSON --> end of changes
	//            
	    elseif (thisSession.userType = "B")
	
	        // render the broker menu
	
	        stream *stdout,"<a class='menuHdrText'>Brokerage Menu</a><br />",newline2:
	    		"<hr />":
	    		"<a href=""/index.php/"">EXCHANGE REPORT</a><br />",newline2:
	    		"<a href=""/index.php/statusrpt.plc"">VIEW ORDER</a><br />",newline2:
	    		"<a href=""/index.php/onlineinvoicing.plc"">VIEW INVOICE</a><br />",newline2:
	    		"<a href=""/index.php/listsearch.plc"">DATACARDS &amp; SAMPLES</a><br />",newline2:
	    		"<a href=""/index.php/usermaint.plc"">EDIT PROFILE</a><br />",newline2:
	    		"<a href=""/index.php/logout.plc"">LOG OUT</a><br />",newline2:
			"<a href='/index.php/terms-of-service/';><font size=1 face='arial'>Terms of Service</font></a>",newline2:
	        	"<a href='/plb-bin/comments.plc';><font size=1 face='arial'>Questions/Comments?</font></a>",newline2:
	        	"<hr />"
	//
	// 2010-09-13 BJACKSON --> removed width from TD
	//
	// 2012-11-2 DLH ---> restore width to TD
	//            "<td width='1000' valign='top' class='body'>",newline2
	//            "<td valign='top' class='body'>",newline2
	//
	// 2010-09-13 BJACKSON --> end of changes
	//            
.END PATCH 08 REPLACED LOGIC
	    elseif (thisSession.userType = "A")
	        // render the admin menu
	        stream *stdout:
	    	    "Administrator Menu<br />":
			"<hr />":
		    "<form name=exchangereport method='POST' action='/index.php/exchangereport'>":
			  "<input type=hidden name='SID' value='",SID,"' />":
			  "<a href='/index.php/exchangereport' onClick='document.exchangereport.submit();return false'>EXCHANGE REPORT</A>":
		    "</form>":
		    "<form name=owneractivityrpt method='POST' action='/index.php/owneractivityrpt'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/owneractivityrpt' onClick='document.owneractivityrpt.submit();return false'>OWNER ACTIVITY REPORT</a>":
		    "</form>":
		    "<form name=statusrpt method='POST' action='/index.php/statusrpt'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
			  "<a href='/index.php/statusrpt' onClick='document.statusrpt.submit();return false'>VIEW ORDER</a>":
		    "</form>":
		    "<form name=onlineinvoicing method='POST' action='/index.php/onlineinvoicing'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/onlineinvoicing' onClick='document.onlineinvoicing.submit();return false'>VIEW INVOICE</a>":
		    "</form>":
		    "<form name=listsearch method='POST' action='/index.php/listsearch'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/listsearch' onClick='document.listsearch.submit();return false'>DATACARDS &amp; SAMPLEST</a>":
		    "</form>":
		    "<form name=listsearch method='POST' action='/index.php/listsearch'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/listsearch' onClick='document.listsearch.submit();return false'>EDIT PROFILE</a>":
		    "</form>":
		    "<form name=logout method='POST' action='/index.php/logout'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/logout' onClick='document.logout.submit();return false'>LOG OUT</a>":
		    "</form>":
			"<a href='/index.php/terms-of-service/';><font size=1 face='arial'>Terms of Service</font></a>":
		    "<form name=comments method='POST' action='/index.php/comments'>":
		 	  "<input type=hidden name='SID' value='",SID,"' />":
		    "<a href='/index.php/comments'><font size=1 face='arial'>Questions/Comments?</font></a>":
		    "</form>":
	"<hr />"
	//
	// 2010-09-13 BJACKSON --> removed width from TD
	//
	// 2012-11-2 DLH ---> restore width to TD
	//            "<td width='1000' valign='top' class='body'>",newline2
	//          "<td valign='top' class='body'>",newline2
	//
	// 2010-09-13 BJACKSON --> end of changes
	//            
	    else
.	        // we don't have a user type, so render an empty menu.  We use a transparent image so that
.	        //  we can make sure that the empty area is 120 pixels wide
	//      stream *stdout,"<img src='/images/transparent.png' width='120'>",newline2:
	//          "</td>",newline2:
	//
	// 2010-09-13 BJACKSON --> removed width from TD
	//
	// 2012-11-2 DLH ---> restore width to TD
	//          "<td width='1000' valign='top' class='body'>",newline2
	//            "<td valign='top' class='body'>",newline2
	//
	// 2010-09-13 BJACKSON --> end of changes
	//            
	    endif
	stream *stdout:
		"</div>":
		"<div id='mainContent' width='1000' valign='top' class='body'>",newline2
	return
	
CGIRenderIndexPage
        stream *stdout:
        	"<p align='center'>Welcome</p>",newline2:
		"<p align='center'>You are successfully logged in to Names in the News online. </p>",newline2:
		"<p align='center'>Please select an option from the menu on the left.</p>",newline2
	return

.START PATCH 10 ADDED LOGIC
.///////////////////////////////////////////////////////////////////////////////
CGITrack Routine #DimPtr,#DimPtr1
.#DimPtr  = Session ID
.#DimPtr1 = Process ID
          //A local Trim - cannot Trim a Local Variable in a different file!
          chop      #DimPtr,#DimPtr                                             .get rid of trailing spaces
          parse     #DimPtr INTO #DimPtr USING #strParseRange         .get rid of leading spaces
          if (#DimPtr <> "")
                    pack      NWEBFLD1,"01X",#DimPtr
                    call      NWEBAIMT
                    if not over
.                             //Refresh some values
.                             //This should happen with any process OTHER than Login
                              call      NWEBAIM
                    else      //Should only happen with Login
                              move      #DimPtr,NWEBRECORD.NWEBSESSION
                              move      fUserIO.userID,NWEBRECORD.NWEBID
                              move      fUserIO.Email,NWEBRECORD.NWEBUSEREMAIL
                              move      fUserIO.username,NWEBRECORD.NWEBUSERNAME
                              move      fUserIO.userType,NWEBRECORD.NWEBUSERTYPE
                              move      fUserIO.companyNumber,NWEBRECORD.NWEBCOMP
                    endif
.
          elseif (#DimPtr1 = "0002")
.                   //Registration
                    move      fUserIO.userID,NWEBRECORD.NWEBID
                    move      fUserIO.Email,NWEBRECORD.NWEBUSEREMAIL
                    move      fUserIO.username,NWEBRECORD.NWEBUSERNAME
                    move      fUserIO.userType,NWEBRECORD.NWEBUSERTYPE
                    move      fUserIO.companyNumber,NWEBRECORD.NWEBCOMP
          elseif (#DimPtr1 = "0003")
.                   //Password Request
                    move      fUserIO.Email,NWEBRECORD.NWEBUSEREMAIL
          endif
          clock     timestamp,NWEBRECORD.NWEBTIME
          move      #DimPtr1,NWEBRECORD.NWEBPROCESS
          pack      NWEBFLD,#DimPtr,NWEBRECORD.NWEBPROCESS,NWEBRECORD.NWEBTIME
.
          call      NWEBWRT
          return
.///////////////////////////////////////////////////////////////////////////////
.END PATCH 10 ADDED LOGIC

.///////////////////////////////////////////////////////////////////////////////

#noLabel

        stream *stdout,"<hr>The server encountered a fatal error.  A label reference could not be located.<br>":
            "Label Name: ",#genericVar,"<br>":
            "Error Code: ",s$error$,"<hr>",newline2
        shutdown

        return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
#end
.///////////////////////////////////////////////////////////////////////////////

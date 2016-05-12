///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   orderrpt.pls                                                 //
//                                                                           //
//    AUTHOR:   bjackson@adjacency.net                                       //
//                                                                           //
//      DATE:   16 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    NIN order report online                                      //
//                                                                           //
// REVISION:    VER01   16 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include CGILibrary.inc
    include user.io
    include sessionmgmt.inc
///////////////////////////////////////////////////////////////////////////////
cgiFunction     dim     %1
cginumber       dim     %99
cginame         dim     %99
cgicompany      dim     %99
cgidue          dim     %99
cgipaid         dim     %99
cgidocument     dim     %99

buffer          dim     32768

httName         init    "orderrpt.htt"
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    // TO DO: insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    call CGIParse using "cginumber",cgiNumber

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "function",cgiFunction
    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the form results
        call actionProcess
    default
        // display a form
        call actionDisplay
    endswitch

    // this will send the static bottom part of the HTML document
    call CGIRenderFooter

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

    // this will display the template (HTT) document we've created for this
    //  page
    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // add logic to process the data that we've parsed from the CGI process

    call CGIDisplayFile using "orderrptresponse.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    stream *stdout,"<tr><td colspan=9 align=center class='datarow'><br>No results for list number '":
        cgiNumber,"'<br><br></td></tr>",newline

    return
///////////////////////////////////////////////////////////////////////////////

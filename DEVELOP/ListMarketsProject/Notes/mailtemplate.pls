///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   mailtemplate.pls                                             //
//                                                                           //
//    AUTHOR:   bjackson@adjacency.net                                       //
//                                                                           //
//      DATE:   05 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI template application with email support                  //
//                                                                           //
// REVISION:    VER01   05 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include CGILibrary.inc
    include user.io
    include sessionmgmt.inc
///////////////////////////////////////////////////////////////////////////////
// TO DO: INSERT UDA HERE
objMailer       automation

cgiFunction     dim         1
cgiNumber       dim         10
mailBody        dim         32768

httName         init        "mailtemplate.htt"
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
//    call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    // TO DO: insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "number",cgiNumber

    call CGIParse using "SID",SID
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif

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
    call CGIDisplay using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // TO DO: add logic to process the data that we've parsed from the
    //  CGI process

    // TO DO: append your data onto the email body
    clear mailBody
    append "Thank you for using mailtemplate.plc.  Your form data was as follows:" to mailBody
    append newline to mailBody
    append "number='" to mailBody
    append cgiNumber to mailBody
    append "'" to mailBody
    append newline to mailBody
    append "Sincerely, the Webmaster." to mailBody
    reset mailBody

    // create the mailer object
    trap noMailer if object
        create objMailer,class="Dundas.Mailer"
    trapclr object

    // TO DO: change the properties of the mailer to use the correct
    //  email settings
    objMailer.TOs.Add using "jbrown@adjacency.net"
    objMailer.TOs.Add using "bjackson@adjacency.net"
    setprop objMailer,*FromAddress="bjackson@adjacency.net"
    setprop objMailer,*Subject="FROM MAILTEMPLATE.PLC"
    setprop objMailer,*Body=mailBody

    // send the email
    trap noSend if object
        objMailer.SendMail
    trapclr object

    destroy objMailer

    stream *stdout,"<br><br>An email has been sent.":
            "<br><br>"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noMailer

    // the mailer object has not been installed properly
    stream *stdout,"<hr><h3>An error has occurred while accessing this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noSend

    // the mailer object couldn't send the message
    stream *stdout,"<hr><h3>An error has occurred while connecting to this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
///////////////////////////////////////////////////////////////////////////////

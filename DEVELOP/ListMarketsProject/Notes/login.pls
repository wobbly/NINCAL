///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   login.pls                                                    //
//                                                                           //
//    AUTHOR:   bjackson@adjacency.net                                       //
//                                                                           //
//      DATE:   04 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    NIN login processor - built from template.pls                //
//                                                                           //
// REVISION:    VER02   15 NOV 2004 BJACKSON    Modified to work with        //
//                                               enhanced CGILIBRARY         //
//              VER01   04 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
cgiFunction     dim     %1
cgiPassword     dim     %80
cgiUsername     dim     %80
cgiBroker       dim     %1
cgiTitle        dim     %20
fromAddress     dim     260

httName         init    "login.htt"
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // we don't session validate because we're not logged in yet

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    call CGIParse using "cgiusername",cgiUsername
    call CGIParse using "cgipassword",cgiPassword
    call CGIParse using "cgibroker",cgiBroker
    if (cgiBroker="1")
        move "Broker " to cgiTitle
    else
        move "Client " to cgiTitle
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

    // this will display the login (HTT) document we've created for this page
    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    if (cgiUsername = "")
        move "Username is required!" to errorMsg
        call actionDisplay
        return
    endif

    if (cgiPassword = "")
        move "Password is required!" to errorMsg
        call actionDisplay
        return
    endif

    // open user file and read for username/password match
    call fUserOpenReadOnly
    packkey fUser2IKey from cgiUsername,""
    call fUser2IRead
    call fUser2IReadKS

    match cgiUsername to fUserIO.username
    if not equal
        // we couldn't find the username, so the login failed
        move "Invalid username or password<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
        call actionDisplay
        return
    else
        // decrypt fUserIO.Password
        if (cgiPassword != fUserIO.Password)
            // the password does not match
            move "Invalid username or password<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
            call actionDisplay
            return
        endif
    endif

    // if the user is inactive or pending, we need to display an error message for them - they can't
    //  log in!

    // Get from address out of plbwin.ini
    pack fromAddress from "nin;ADMIN_EMAIL"
    clock ini into fromAddress
    if over
        move "Unable to send an email message.  Please contact the webmaster." to errorMsg
        call actionDisplay
        return
    endif
    if (fUserIO.userType = "P")
        // pending account
        pack errorMsg from "<center>Your user account has not yet been activated.<br>Please be patient while the administrator ":
            "processes your registration. <br>If you believe this message is in error, please contact the ":
            "webmaster at ",fromAddress,".</center>"
        call actionDisplay
        return
    elseif (fUserIO.userType = "S")
        // suspended account
        pack errorMsg from "There is a problem with your user account.  Please contact the Names in the News ":
            "webmaster at ",fromAddress,"."
        call actionDisplay
        return
    endif

    // if we made it this far, the login was successful, so assign a session ID and
    //  redirect to the main menu
    call sessionLogin using SID

    stream *STDOUT:
        "<html>",newline:
        "<body>",newline

    stream *STDOUT,"<form name='loginForm' method='post' action='mainmenu.plc'>",newline:
        "<input type='hidden' name='SID' value='",SID,"'><br>",newline:
        "<script language=javascript>",newline:
        "document.loginForm.submit();",newline:
        "</script>",newline:
        "</body>",newline:
        "</html>",newline

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

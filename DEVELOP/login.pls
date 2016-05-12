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
// REVISION:                                                                         //
//                  VER07   12 JAN 2006 ASH         Added Pre-Reg Filters        //
//                  VER06   29 DEC 2005 ASH         Added Tracking Logic         //
//                  VER05   28 DEC 2005 ASH         Added Contact links          //
//                  VER04   23 DEC 2005 ASH         Updated error message        //
//                  VER03   16 DEC 2005 DBACA       call index.htm for login verification//
//                  VER02   15 NOV 2004 BJACKSON    Modified to work with        //
//                                               enhanced CGILIBRARY         //
//              VER01   04 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
cgiFunction     dim     %1
cgiPassword     dim     %80
//Patch 03 Comment Out Var
//cgiUsername     dim     %80
//Patch 03 Comment Out Var
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
//Begin Patch 03 Comment Out
//    call CGIRenderHeader
//End Patch 03 EndComment Out
//Begin Patch 03 Modified Code
    call CGIParse using "cgiusername",cgilogin
//    call CGIParse using "cgiusername",cgiUsername
//End Patch 03 Modified Code
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
//Begin Patch 03 Code Added
        stop
//End Patch 03 Code Added
    default
        // display a form
        call actionDisplay
    endswitch

    // this will send the static bottom part of the HTML document
//Begin Patch 03 Code Modified
//    call CGIRenderFooter
//End Patch 03 Code Modified
    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

    // this will display the login (HTT) document we've created for this page
//Begin Patch 03 Comment Out
    call CGIDisplayFile using httName
//End Patch 03 Comment Out
//Begin Patch 03 Replace Logic
//          call CGIRenderIndexPage
//End Patch 03 Replace Logic
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess
//Begin Patch 03  Code Modification
//    if (cgiUsername = "")
      if (cgilogin = "")
//End Patch 03  Code Modification
        move "Username is required!" to errorMsg
        call actionDisplay
        return
    endif

    if (cgiPassword = "")
//Begin Patch 03 Code Modified
        move "Password is required!" to errorMsg
//        move "Password is required!" to errorMsg
//End Patch 03 Code Modified
        call actionDisplay
        return
    endif

    // open user file and read for username/password match
    call fUserOpenReadOnly
//Begin Patch 03  Code Modification
//    packkey fUser2IKey from cgiUsername,""
    packkey fUser2IKey from cgiLogin,""
//End Patch 03 Code Modification
    call fUser2IRead
    call fUser2IReadKS

//BeginPatch 03 Code Modification
//    match cgiUsername to fUserIO.username
    match cgiLogin to fUserIO.username
//EndPatch 03 Code Modification
    if not equal
        // we couldn't find the username, so the login failed
.START PATCH 04 REPLACED LOGIC
.        move "Invalid username or password<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
        move "Invalid username or password<br>Username & password are case-sensitive<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
.END PATCH 04 REPLACED LOGIC
        call actionDisplay
        return
    else
        // decrypt fUserIO.Password
        if (cgiPassword != fUserIO.Password)
            // the password does not match
.START PATCH 04 REPLACED LOGIC
.            move "Invalid username or password<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
            move "Invalid username or password<br>Username & password are case-sensitive<br><a class='errorMsg' href='/plb-bin/passrequest.plc'>Click here if you've forgotten your password.</a>" to errorMsg
.END PATCH 04 REPLACED LOGIC
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
.START PATCH 05 REPLACED LOGIC
.        move "Unable to send an email message.  Please contact the webmaster." to errorMsg
        move "Unable to send an email message.  Please contact the <a href=#"#/index.php/contact-us/#";#"> website administrator</a>." to errorMsg
.END PATCH 05 REPLACED LOGIC
        call actionDisplay
        return
    endif
    if (fUserIO.userType = "P")
        // pending account
.START PATCH 05 REPLACED LOGIC
.        pack errorMsg from "<center>Your user account has not yet been activated.<br>Please be patient while the administrator ":
.            "processes your registration. <br>If you believe this message is in error, please contact the ":
.            "webmaster at ",fromAddress,".</center>"
        pack errorMsg from "<center>Your user account has not yet been activated.<br>Please be patient while the administrator ":
            "processes your registration. <br>If you believe this message is in error, please contact the <a href=#"#/index.php/contact-us/#";#"> website administrator</a>.</center>"
.END PATCH 05 REPLACED LOGIC
        call actionDisplay
        return
    elseif (fUserIO.userType = "S")
        // suspended account
.START PATCH 05 REPLACED LOGIC
.        pack errorMsg from "There is a problem with your user account.  Please contact the Names in the News ":
.            "webmaster at ",fromAddress,"."
        pack errorMsg from "There is a problem with your user account.  Please contact the Names in the News <a href=#"#/index.php/contact-us/#";#"> website administrator</a>."
.END PATCH 05 REPLACED LOGIC
        call actionDisplay
        return
    endif

.START PATCH 07 ADDED LOGIC
          if (fUserIO.tos = "1")
          //They were pre-registered and need to affirm Terms of Service
                    stream *STDOUT:
                              "<html>",newline2:
                              "<body>",newline2

                    stream *STDOUT,"<form name='loginForm' method='post' action='tosprereg.plc'>",newline2:
                              "<input type='hidden' name='cgiusername' value='",cgilogin,"'><br>",newline2:
                              "<script language=javascript>",newline2:
                              "document.tosprereg.submit();",newline2:
                              "</script>",newline2:
                              "</body>",newline2:
                              "</html>",newline2
          endif
.END PATCH 07 ADDED LOGIC

    // if we made it this far, the login was successful, so assign a session ID and
    // redirect to the main menu
    call sessionLogin using SID

.START PATCH 06 ADDED LOGIC
          call      CGITrack using SID,"0001"
.END PATCH 06 ADDED LOGIC

    stream *STDOUT:
        "<html>",newline2:
        "<body>",newline2
.TEMP
    stream *STDOUT:
        fUserIO.username,newline2

.TEMP

    stream *STDOUT,"<form name='loginForm' method='post' action='/plb-bin/mainmenu.plc'>",newline2:
        "<input type='hidden' name='SID' value='",SID,"'><br>",newline2:
        "<script language=javascript>",newline2:
        "document.loginForm.submit();",newline2:
        "</script>",newline2:
        "</body>",newline2:
        "</html>",newline2

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

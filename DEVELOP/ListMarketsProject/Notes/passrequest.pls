///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   passrequest.pls                                              //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   08 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI password request application                             //
//                                                                           //
// REVISION:    VER01   08 NOV 2004 JBROWN    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
objMailer       automation

cgiFunction     dim     %1
cgiEmail        dim     %60
cgiServerName   dim     %260
cgipassword     dim     %12
fromAddress     dim     %260
mailBody        dim     %32768
cwk260          dim     260

httName         init    "passwordrequest.htt"

i               integer 4
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "function",cgiFunction
    call CGIParse using "cgiemail",cgiEmail

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

    // this will display the (HTT) document we've created for this
    //  page
    reset errorMsg
    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    //  CGI process
    call cgiParse using "cgiemail",cgiEmail
    if over
        append "Email is required!<br>" to errorMsg
        call actionDisplay
        return
    endif

    count i in cgiEmail
    move i to cgiServerName
    if zero
        move "Email is required!" to errorMsg
        call actionDisplay
        return
    endif

    call cgiParse using "SERVER_NAME",cgiServerName

    uppercase cgiemail
    call fUserOpenReadOnly
    packkey fUser3Ikey with cgiemail
    call fUser3IRead

    clear fUserIO
    call fUser3IReadKS
    uppercase fUserIO.email
    if (fUserIO.Email = cgiemail)

        // Get from address out of plbwin.ini
        pack fromAddress from "nin;ADMIN_EMAIL"
        clock ini into fromAddress
        if over
            move "Unable to send an email message.  Please contact the webmaster." to errorMsg
            call actionDisplay
            return
        endif

        // build the email message
        clear mailBody
        append "Here is your log in information" to mailBody
        append newline to mailBody
        append newline to mailBody
        append "Username: " to mailBody
        append fUserIO.userName to mailBody
        append newline to mailBody
        append "Password: " to mailBody
        append fUserIO.password to mailBody
        append newline to mailBody
        append newline to mailBody
        append "You can log in to your account by going to http://" to mailBody
        append cgiServerName to mailBody
        append "/plb-bin/login.plc" to mailBody
        append newline to mailBody
        append newline to mailBody
        append "If you have any questions, you can contact us at " to mailBody
        append fromAddress to mailBody
        append "." to mailBody
        append newline to mailBody
        append "Thank you for using Names in the News." to mailBody
        append newline to mailBody
        reset mailBody

        // create the mailer object
        trap noMailer if object
            create objMailer,class="Dundas.Mailer"
        trapclr object

        //  email settings
        objMailer.TOs.Add using cgiemail
        setprop objMailer,*FromAddress=fromAddress
        setprop objMailer,*Subject="Names in the News login information"
        setprop objMailer,*Body=mailBody
        setprop objMailer,*TimeOutConnect=15

        // get the name of the SMTP we'll be using from the INI file
        pack cwk260 from "nin;SMTPSERVER"
        clock ini into cwk260
        if over
            move "Error reading SMTP configuration from .ini file!" to errorMsg
            call actionDisplay
            return
        endif
        objMailer.SMTPRelayServers.Add using cwk260

        // send the email
        trap noSend if object
            objMailer.SendMail
        trapclr object

        destroy objMailer

        // do a lowercase to make the email address look pretty
        lowercase cgiemail

        stream *stdout,"<center>",newline:
            "<br><br><br><br>Log in information has been sent to ",cgiEmail,".<br><br>",newline:
            "Click <a href='/plb-bin/login.plc'>here</a> to return to the login screen.":
            "</center>",newline
    else
        pack errorMsg from "The email address you provided is invalid or could not be found in our database.<br>":
            "Please try again, or click <a href='/plb-bin/registration.plc'>here</a> if you need to register as a new user."
        call actionDisplay
    endif

    return
///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

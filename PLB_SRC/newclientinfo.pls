///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   newclientinfo.pls                                            //
//                                                                           //
//    AUTHOR:   bjackson@adjacency.net                                       //
//                                                                           //
//      DATE:   04 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI template application                                     //
//                                                                           //
//                1.1    13Nov2012  DLH replace Dundas.mailer                //
// REVISION:    VER01   04 NOV 2004 BJACKSON    Created                      //
//                                                                           //
//        release   1.1       03FEB2006 ASH Modified email logic so that they go to Front Desk instead of Administrators
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
.begin patch 1.05
.objMailer           automation
.end patch 1.05

cgiaddress          dim     %80
cgiarea             dim     %32
cgiFunction         dim     %1
cgicity             dim     %32
cgicompfunction     dim     %32
cgiemail            dim     %60
cgiexchange         dim     %4
cgiexchangecont     dim     %4
cgiexchangeformat   dim     %10
cgiexchangeqty      dim     %10
cgiexchangetype     dim     %80
cgifax              dim     %10
cgimailer           dim     %4
cgimailerarea       dim     %32
cginame             dim     %32
cgiorganization     dim     %32
cgipassword         dim     %12
cgiphone            dim     %10
cgipreferredtime    dim     %32
cgiquantity         dim     %10
cgiquestions        dim     %260
cgisource           dim     %32
cgistate            dim     %2
cgitimeframe        dim     %10
cgiusername         dim     %12
cgizip              dim     %10

cwk60               dim     60
cwk260              dim     260
fromAddress         dim     60
.mailBody           dim     32768

nwk1                form    1

i                   integer 4

httName             init    "newclientinfo.htt"
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    // call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    //  insert all of our CGI parse statements here.  If any variable is
    //      supposed to filled out but you get an over, empty string, etc.,
    //      append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    call CGIParse using "cgiaddress",cgiaddress
    call CGIParse using "cgiarea",cgiarea
    call CGIParse using "cgicity",cgicity
    call CGIParse using "cgicompfunction",cgicompfunction
    call CGIParse using "cgiemail",cgiemail
    call CGIParse using "cgiexchange",cgiexchange
    call CGIParse using "cgiexchangecont",cgiexchangecont
    call CGIParse using "cgiexchangeformat",cgiexchangeformat
    call CGIParse using "cgiexchangeqty",cgiexchangeqty
    call CGIParse using "cgiexchangetype",cgiexchangetype
    call CGIParse using "cgifax",cgifax
    call CGIParse using "cginame",cginame
    call CGIParse using "cgimailer",cgimailer
    call CGIParse using "cgimailerarea",cgimailerarea
    call CGIParse using "cgiorganization",cgiorganization
    call CGIParse using "cgipassword",cgipassword
    call CGIParse using "cgiphone",cgiphone
    call CGIParse using "cgipreferredtime",cgipreferredtime
    call CGIParse using "cgiquanity",cgiquantity
    call CGIParse using "cgiquestions",cgiquestions
    call CGIParse using "cgisource",cgisource
    call CGIParse using "cgistate",cgistate
    call CGIParse using "cgitimeframe",cgitimeframe
    call CGIParse using "cgiusername",cgiusername
    call CGIParse using "cgizip",cgizip

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
    reset errorMsg
    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // add logic to process the data that we've parsed from the CGI process

    // Validation logic
    count i in cgiphone
    if (i != 0)
        if (i = 10)
            type cgiphone
            if not equal
                move "Please enter a valid phone number with no dashes or spaces." to errorMsg
                call actionDisplay
                return
            endif
        else
            move "Please enter phone number with area code and no dashes or spaces." to errorMsg
            call actionDisplay
            return
        endif
    endif

    count i in cgifax
    if (i != 0)
        if (i = 10)
            type cgifax
            if not equal
                move "Please enter a valid fax with no dashes or spaces." to errorMsg
                call actionDisplay
                return
            endif
        else
            move "Please enter fax number with area code and no dashes or spaces." to errorMsg
            call actionDisplay
            return
        endif
    endif

    move cgiemail to cwk60
    scan "@" in cwk60
    if not equal
        move "Please enter a valid email address." to errorMsg
        call actionDisplay
        return
    endif

    // Email the Admin
    clear mailBody
    append "Name: " to mailBody
    append cginame to mailBody
    append newline2 to mailBody
    append "Organization: " to mailBody
    append cgiorganization to mailBody
    append newline2 to mailBody
    append "Address: " to mailBody
    append cgiaddress to mailBody
    append newline2 to mailBody
    append "City: " to mailBody
    append cgicity to mailBody
    append newline2 to mailBody
    append "State: " to mailBody
    append cgistate to mailBody
    append newline2 to mailBody
    append "Zip: " to mailBody
    append cgizip to mailBody
    append newline2 to mailBody
    append "Phone: " to mailBody
    append cgiphone to mailBody
    append newline2 to mailBody
    append "Fax: " to mailBody
    append cgifax to mailBody
    append newline2 to mailBody
    append "Email: " to mailBody
    append cgiemail to mailBody
    append newline2 to mailBody
    append "Preferred Time: " to mailBody
    append cgipreferredtime to mailBody
    append newline2 to mailBody
    append "Source: " to mailBody
    append cgisource to mailBody
    append newline2 to mailBody
    append "Company Function: " to mailBody
    append cgicompfunction to mailBody
    append newline2 to mailBody
    append "Mailer: " to mailBody
    append cgimailer to mailBody
    append newline2 to mailBody
    append "Mailer Area: " to mailBody
    append cgimailerarea to mailBody
    append newline2 to mailBody
    append "Quantity: " to mailBody
    append cgiquantity to mailBody
    append newline2 to mailBody
    append "Timeframe: " to mailBody
    append cgitimeframe to mailBody
    append newline2 to mailBody
    append "Exchange: " to mailBody
    append cgiexchange to mailBody
    append newline2 to mailBody
    append "Exchange Quantity: " to mailBody
    append cgiexchangeqty to mailBody
    append newline2 to mailBody
    append "Exchange Type: " to mailBody
    append cgiexchangetype to mailBody
    append newline2 to mailBody
    append "Exchange Format: " to mailBody
    append cgiexchangeformat to mailBody
    append newline2 to mailBody
    append "Exchange Contributors: " to mailBody
    append cgiexchangecont to mailBody
    append newline2 to mailBody
    append "Questions: " to mailBody
    append cgiquestions to mailBody
.START PATCH 1.1 REMOVED LOGIC
.    append newline2 to mailBody
.    append "Username: " to mailBody
.    append cgiusername to mailBody
.    append newline2 to mailBody
.    append "User has been added with a pending status.  Please approve." to mailBody
.END PATCH 1.1 REMOVED LOGIC
    reset mailBody

.begin patch 1.05
.    // create the mailer object
.    trap noMailer if object
.        create objMailer,class="Dundas.Mailer"
.    trapclr object
.end patch 1.05

.START PATCH 1.1 REPLACED LOGIC
.    // get Admin email addresses
.    call fUserOpen
.    packkey fUser4IKey from "A"
.    call fUser4IRead
.    loop
.        call fUser4IReadKS
.        until over
.        while (fUserIO.userType = "A")
.        objMailer.TOs.Add using fUserIO.email
.    repeat
.
.    // Get from address - plbwin.ini
.    pack cwk260 from "nin;ADMIN_EMAIL"
.    clock ini into cwk260
.    if over
.        move "Error reading email address from .ini file!" to errorMsg
.        call actionDisplay
.        return
.    endif
.    move cwk260 into fromAddress
........................................................
          // Get from address - plbwin.ini
          pack cwk260 from "nin;ADMIN2_EMAIL"
          clock ini into cwk260
          if over
                    move "Error reading email address from .ini file!" to errorMsg
                    call actionDisplay
                    return
          endif
          move cwk260 into fromAddress
.begin patch 1.05
.          objMailer.TOs.Add using fromAddress
          pack      mailfrom,"creques@nincal.com"
          pack      mailto,fromaddress
.end patch 1.05
.END PATCH 1.1 REPLACED LOGIC

    // Get the preferred DNS servers from the plbwin.ini file.  This makes
    //  mail sending MUCH faster.
    for nwk1 from "0" to "9"
        pack cwk260 from "nin;DNSSERVER",nwk1
        clock ini into cwk260
        if not over
.begin patch 1.05
.            objMailer.DNSServers.Add using cwk260
.end patch 1.05
        endif
    repeat

    // set our email properties - sender, subject, and body
.begin patch 1.05
.    setprop objMailer,*FromAddress=fromAddress
.    setprop objMailer,*Subject="NEW CLIENT INFORMATION"
.    setprop objMailer,*Body=mailBody
          pack      mailsubjct,"NEW CLIENT INFORMATION"
.end patch 1.05

    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
.begin patch 1.05
.    objMailer.SMTPRelayServers.Add using cwk260
          pack      mailserver,cwk260
.end patch 1.05

    // send the email
.begin patch 1.05
.    trap noSend if object
.        objMailer.SendMail
.    trapclr object
.
.    destroy objMailer
          Move      "Y",Mailtrace

          Call      SendMail

.end patch 1.05

    call CGIDisplayFile using "newclientinforesponse.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doStateCodes routine

    call CGISelectState using "  "

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

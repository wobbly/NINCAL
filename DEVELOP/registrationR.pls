///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   registration.pls                                             //
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
// REVISION:    VER01   04 NOV 2004 BJACKSON    Created                      //
//                      1.2  29DEC2005  ASH  Added Tracking Logic            //
//                      1.1  19OCT2005  ASH  Removed Fax Number              //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
objMailer           automation

cgiaddress          dim     %80
cgiarea             dim     %32
cgiBroker           dim     %1
cgicity             dim     %32
cgicompfunction     dim     %32
cgiemail            dim     %60
.START PATCH 1.1 REMOVED LOGIC
.cgifax              dim     %10
.END PATCH 1.1 REMOVED LOGIC
cgifirstname        dim     %32
cgilastname         dim     %32
cgiorganization     dim     %32
cgipassword         dim     %20
cgipassword2        dim     %20
cgiphone            dim     %10
cgistate            dim     %2
cgiTitle            dim     %20
cgiusername         dim     %20
cgizip              dim     %10
cgiFunction         dim     %1
cgiServerName       dim     %260
cgiTOS              dim     %1

cwk60               dim     60
cwk260              dim     260
fromAddress         dim     60
.mailBody            dim     32768       .in cons.inc DLH March 15 2007

i                   integer 4

httName             init    "registration.htt"
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
.    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    // TO DO: insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    call cgiParse using "SERVER_NAME",cgiServerName
    call CGIParse using "cgifirstname",cgifirstname
    call CGIParse using "cgilastname",cgilastname
    call CGIParse using "cgiorganization",cgiorganization
    call CGIParse using "cgiaddress",cgiaddress
    call CGIParse using "cgicity",cgicity
    call CGIParse using "cgistate",cgistate
    call CGIParse using "cgizip",cgizip
    call CGIParse using "cgiphone",cgiphone
.START PATCH 1.1 REMOVED LOGIC
.    call CGIParse using "cgifax",cgifax
.END PATCH 1.1 REMOVED LOGIC
    call CGIParse using "cgicompfunction",cgicompfunction
    call CGIParse using "cgiarea",cgiarea
    call CGIParse using "cgiusername",cgiusername
    call CGIParse using "cgipassword",cgipassword
    call CGIParse using "cgipassword2",cgipassword2
    call CGIParse using "cgiemail",cgiemail
    call CGIParse using "cgitos",cgitos
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
.    call CGIRenderFooter

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

    // logic to process the data that we've parsed from the CGI process
    call fUserOpen

    //Validation
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

.START PATCH 1.1 REMOVED LOGIC
.    count i in cgifax
.    if (i != 0)
.        if (i = 10)
.            type cgifax
.            if not equal
.                move "Please enter a valid fax with no dashes or spaces." to errorMsg
.                call actionDisplay
.                return
.            endif
.        else
.            move "Please enter fax number with area code and no dashes or spaces." to errorMsg
.            call actionDisplay
.            return
.        endif
.    endif
.END PATCH 1.1 REMOVED LOGIC

    move cgiemail to cwk60
    scan "@" in cwk60
    if not equal
        move "Please enter a valid email address." to errorMsg
        call actionDisplay
        return
    endif

    count i in cgiusername
    if (i < 5)
        move "Your username must be at least 5 characters." to errorMsg
        call actionDisplay
        return
    elseif (i > 12)
        move "Your username can not be more than 12 characters." to errorMsg
        call actionDisplay
        return
    endif


    count i in cgipassword
    if (i < 6)
        move "Your password must be at least 6 characters." to errorMsg
        call actionDisplay
        return
    elseif (i > 12)
        move "Your password can not be more than 12 characters." to errorMsg
        call actionDisplay
        return
    endif

    if (cgipassword2 != cgipassword)
        move "Your passwords do not match." to errorMsg
        call actionDisplay
        return
    endif

    if (cgitos != "1")
        move "You must agree to the Terms of Service." to errorMsg
        call actionDisplay
        return
    endif


    // Check for an existing record
    packkey fUser3IKey with cgiemail,""
    call fUser3IRead
    call fUser3IReadKS
    if not over
        if (cgiemail = fUserIO.email)
            // display message here
            pack errorMsg from "This email address is already registered.  Click ":
                "<a class='errorMsg' href='/index.php'>here</a> to log in.</a>"
            call actionDisplay
            return
        endif
    endif

    packkey fUser2IKey with cgiusername,""
    call fUser2IRead
    call fUser2IReadKS
    if not over
        if (cgiusername = fUserIO.username)
            // display message here
            pack errorMsg from "This username is already registered.  Click ":
                "<a class='errorMsg' href='/index.php'>here</a> to log in.</a>"
            call actionDisplay
            return
        endif
    endif

    // Get new user info
    clear fUserIO
    move cgifirstname into fUserIO.fName
    move cgilastname into fUserIO.lName
    move cgiorganization into fUserIO.organization
    move cgiaddress into fUserIO.address
    move cgicity into fUserIO.city
    move cgistate into fUserIO.state
    move cgizip into fUserIO.zip
    move cgiphone into fUserIO.telephone
.START PATCH 1.1 REPLACED LOGIC
.    move cgifax into fUserIO.fax
    clear fUserIO.fax
.END PATCH 1.1 REPLACED LOGIC
    move cgicompfunction into fUserIO.compFunction
    move cgiarea into fUserIO.area
    move cgiusername into fUserIO.username
    move cgipassword into fUserIO.password
    move cgiemail into fUserIO.email
    clear fUserIO.companyNumber
    move "P" into fUserIO.userType
    clock timestamp into fUserIO.datetime

    // write new user to file as pending until approved
    call fUserWrite

.START PATCH 1.2 ADDED LOGIC
           call       CGITrack using SID,"0002"
.END PATCH 1.2 ADDED LOGIC

    // Email registration info to the admin
    call emailAdmin
    call emailUser

    call cgiDisplayFile using "registrationresponse.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
emailAdmin

    // Email the Admin
    clear mailBody
    append "First Name: " to mailBody
    append cgifirstname to mailBody
    append newline2 to mailBody
    append "Last Name: " to mailBody
    append cgilastname to mailBody
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
.START PATCH 1.1 REMOVED LOGIC
.    append "Fax: " to mailBody
.    append cgifax to mailBody
.    append newline2 to mailBody
.END PATCH 1.1 REMOVED LOGIC
    append "Email: " to mailBody
    append cgiemail to mailBody
    append newline2 to mailBody
    append "Company Function: " to mailBody
    append cgicompfunction to mailBody
    append newline2 to mailBody
    append "Area: " to mailBody
    append cgiarea to mailBody
    append newline2 to mailBody
    append "Username: " to mailBody
    append cgiusername to mailBody
    append newline2 to mailBody
    append "User has been added with a pending status.  Please approve." to mailBody
    reset mailBody

    // create the mailer object
    trap noMailer if object
        create objMailer,class="Dundas.Mailer"
    trapclr object

    // get Admin email addresses
    packkey fUser4IKey from "A"
    call fUser4IRead
    loop
        call fUser4IReadKS
        until over
        while (fUserIO.userType = "A")
        objMailer.TOs.Add using fUserIO.email
    repeat

    // Get from address - plbwin.ini
    pack cwk260 from "nin;ADMIN_EMAIL"
    clock ini into cwk260
    if over
        move "Error reading email address from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    move cwk260 into fromAddress

    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    objMailer.SMTPRelayServers.Add using cwk260

    //  email settings
    setprop objMailer,*FromAddress=fromAddress
    setprop objMailer,*Subject="NEW CLIENT REGISTRATION"
    setprop objMailer,*Body=mailBody

    // send the email
    trap noSend if object
        objMailer.SendMail
    trapclr object

    destroy objMailer

    // finished sending Admin email
    return
//////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
emailUser

    // Email the user

    // Get from address - plbwin.ini
    pack cwk260 from "nin;ADMIN_EMAIL"
    clock ini into cwk260
    if over
        move "Error reading email address from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    move cwk260 into fromAddress

    clear mailBody
    append cgifirstname to mailBody
    append "," to mailBody
    append newline2 to mailBody
    append "Thank you for registering with Names in the News." to mailBody
    append " Your registration is currently being reviewed by the administrator." to mailBody
    append " When the administrator approves your account, your username and" to mailBody
    append " password will be activated and you will be able to log in to the Names" to mailBody
    append " in the News website at http://" to mailBody
    append cgiServerName to mailBody
    append "/plb-bin/login.plc." to mailBody
    append newline2 to mailBody
    append newline2 to mailBody
    append "If you have any questions, you can contact us at " to mailBody
    append fromAddress to mailBody
    append "." to mailBody
    append newline2 to mailBody
    append "Thank you for using Names in the News!" to mailBody
    reset mailBody

    // create the mailer object
    trap noMailer if object
        create objMailer,class="Dundas.Mailer"
    trapclr object

    objMailer.TOs.Add using cgiemail

    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    objMailer.SMTPRelayServers.Add using cwk260

    //  email settings
    setprop objMailer,*FromAddress=fromAddress
    setprop objMailer,*Subject="Your Names in the News Account Status"
    setprop objMailer,*Body=mailBody

    // send the email
    trap noSend if object
        objMailer.SendMail
    trapclr object

    destroy objMailer

    // finished emailing the user
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doStateCodes routine

    count i in cgistate
    if zero
        call CGISelectState using "  "
    else
        call CGISelectState using cgistate
    endif

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

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

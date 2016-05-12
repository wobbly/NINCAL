.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.//   PROGRAM:   usermaint.pls                                                //
.//                                                                           //
.//    AUTHOR:   jbrown@adjacency.net                                         //
.//                                                                           //
.//      DATE:   23 NOV 2004                                                  //
.//                                                                           //
.// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
.//              All rights reserved.                                         //
.//                                                                           //
.//  PURPOSE:    CGI application                                              //
.//                                                                           //
.// REVISION:    VER01   23 NOV 2004 JBROWN    Created                        //
.//                                                                           //
.//                1.7    13Nov2012  DLH replace Dundas.mailer                //
.//           Release 1.6  15March2007 DLH  cgi servername MIA
.//              Release 1.5  12JAN2006  ASH  ADDED LOGIC FOR PREREG USERS    //
.//              Release 1.4  29DEC2005  ASH  ADDED LINKS TO CONTACT PAGE     //
.//                                               Added Tracking Logic                 //
.//              Release 1.3  17NOV2005  ASH  MODIFIED DIPSLAY OF PASSWORD    //
.//              Release 1.2  19OCT2005  ASH  Removed Fax Number              //
.//                 Release 1.1         8/18/2005 ASH  Small patches                 //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
.///////////////////////////////////////////////////////////////////////////////
.begin patch 1.7
.objMailer           automation
.end patch 1.7

cwk1                dim     1
cwk60               dim     %60
cgiFunction         dim     %1
cgiaddress          dim     %80
cgiarea             dim     %32
cgicity             dim     %32
cgicompfunction     dim     %32
cgicompanynumber    dim     %6
cgiemail            dim     %60
.START PATCH 1.2 REMOVED LOGIC
.cgifax              dim     %10
.END PATCH 1.2 REMOVED LOGIC
cgifname            dim     %32
cgiinvoiceflag      dim     %10
cgilname            dim     %32
cgiorganization     dim     %32
cgipassword         dim     %20
cgiphone            dim     %10
cgiServerName       dim     %260
cgistate            dim     %2
cgiusername         dim     %20
cgizip              dim     %10
cgiusertype         dim     %9
.START PATCH 1.1 ADDED LOGIC
cgicompanyname      dim     %75
FrmPtr              form    ^
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.5 ADDED LOGIC
cgitos                  DIM     %200
.END PATCH 1.5 ADDED LOGIC

cgiUserID           dim     %36
cgiDeleteButton     dim     %80
cwk260              dim     260
fromAddress         dim     60
.mailBody            dim     32768      in cons

oldusertype         dim     1

thisMailer          dim     4

httName             init    "usermaint.htt"

i                   integer 4

nwk6                form    6
.///////////////////////////////////////////////////////////////////////////////
start

.    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

.    // if the session isn't valid for some reason, this will send us to an
.    //  error page -  if this page requires admin access, change the call
.    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate using "Admin Only"

.    // this will send the static top part of the HTML document
    call CGIRenderHeader

.    ///////////////////////////////////////////////////////////////////////////
.    //Insert all of your CGI parse statements here.  If any variable is
.    //         supposed to filled out but you get an over, empty string, etc.,
.    //         append an error string (in HTML) to the variable errorMsg
.    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif
.Begin patch  1.6
    call cgiParse using "SERVER_NAME",cgiServerName
.End patch  1.6
    call CGIParse using "cgifname",cgifname
    call CGIParse using "cgilname",cgilname
    call CGIParse using "cgiorganization",cgiorganization
    call CGIParse using "cgiaddress",cgiaddress
    call CGIParse using "cgicity",cgicity
    call CGIParse using "cgistate",cgistate
    call CGIParse using "cgizip",cgizip
    call CGIParse using "cgiphone",cgiphone
.START PATCH 1.2 REMOVED LOGIC
.    call CGIParse using "cgifax",cgifax
.END PATCH 1.2 REMOVED LOGIC
    call CGIParse using "cgicompfunction",cgicompfunction
    call CGIParse using "cgiarea",cgiarea
    call CGIParse using "cgiusername",cgiusername
    call CGIParse using "cgipassword",cgipassword
    call CGIParse using "cgiemail",cgiemail
    call CGIParse using "cgiusertype",cgiusertype
    call CGIParse using "cgiuserid",cgiUserID
    call CGIParse using "cgicompanynumber",cgicompanynumber
    call CGIParse using "cgiinvoiceflag",cgiinvoiceflag

.START PATCH 1.5 ASH ADDED LOGIC
    call CGIParse using "cgitos",cgitos
.END PATCH 1.5 ASH ADDED LOGIC

.    // based upon the "function" value, we need to decide whether to render the form or
.    //  process it's data

    call CGIParse using "cgifunction",cgiFunction


.//    debug
.//    move "1B2AF726-0CA4-3C4E-8004-FC3B5C844B69" into cgiuserid
.//    move "1" into cgifunction

    switch cgiFunction
    case "0"
.        // display a form
        call actionDisplay
    case "1"
.        // process the form results
        call actionProcess
    case "2"
.        // process the form results
        call actionDelete
.START PATCH 1.5 ADDED LOGIC
    case "3"
.        // process the form results
        call actionSearch
.END PATCH 1.5 ADDED LOGIC
    default
.        // display a form
        call actionDisplay
    endswitch

.    // this will send the static bottom part of the HTML document
    call CGIRenderFooter

    stop
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
actionDisplay

.    // this will display the (HTT) document we've created for this page
.    // check for userID
    count i in cgiUserID
    if (i = 36)
.        // there's a user ID, so get the record (userID implies that we're doing
.        //  an edit)
        call fUserOpenReadOnly
        packkey fUserIKey from cgiUserID
        call fUserIRead
        if over
            move "User Record Not Located" to errorMsg
            clear cgiUserID
        endif

        move fUserIO.fname to cgiFName
        move fUserIO.lname to cgiLName
        move fUserIO.organization to cgiOrganization
        move fUserIO.address to cgiAddress
        move fUserIO.city to cgiCity
        move fUserIO.state to cgiState
        move fUserIO.zip to cgiZip
        move fUserIO.telephone to cgiPhone
.START PATCH 1.2 REMOVED LOGIC
.        move fUserIO.fax to cgiFax
.END PATCH 1.2 REMOVED LOGIC
        move fUserIO.email to cgiEmail
        move fUserIO.compfunction to cgiCompfunction
        move fUserIO.area to cgiArea
        move fUserIO.username to cgiUsername
.START PATCH 1.3 REPLACED LOGIC
.        clear cgiPassword
          move fUserIO.password to cgiPassword
.END PATCH 1.3 REPLACED LOGIC
        move fUserIO.invoiceFlag to cgiinvoiceflag
        move fUserIO.usertype to cgiUsertype
        move fUserIO.companyNumber to cgiCompanynumber
.START PATCH 1.5 ADDED LOGIC
          if (fUserIO.tos = "1")
                    pack      cgitos,"<input type='checkbox' name='cgitos' value='Y' checked> TOS Approval Required"
          else
                    pack      cgitos,"<input type='checkbox' name='cgitos' value='Y'> Force Re-Approval of TOS"
          endif
.END PATCH 1.5 ADDED LOGIC

.START PATCH 1.1 ADDED LOGIC
        move fUserIO.companyNumber,COMPFLD
          call      DisplayLoadCompany
.END PATCH 1.1 ADDED LOGIC

        if (thisSession.userType = "A")
.            // this user is an administrator - they can do whatever they
.            //  want
            pack cgiDeleteButton from "<input type=button value='Delete' ":
               "onClick='doDelete();'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
            call CGIDisplayFile using "usermaint.htt"
        elseif (thisSession.userID != cgiUserID)
.            // the user is trying to edit somebody else's record and they
.            //  are not an administrator
            stream *STDOUT,"Access denied - you do not have permission to edit this record.<br>"
        else
            call CGIDisplayFile using "usermaintclient.htt"
        endif
    elseif (thisSession.userType = "A")
        call CGIDisplayFile using "usermaint.htt"
    else
.        // make sure a client can't do an add...
        stream *STDOUT,"Access denied - you do not have permission to modify records.<br>"
    endif
.START PATCH 1.4 ADDED LOGIC
          call      CGITrack using SID,"0025"
.END PATCH 1.4 ADDED LOGIC

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
actionProcess

.    // logic to process the data that we've parsed from the CGI process
.    //  add logic to process the data that we've parsed from the CGI process
.    // check for userID
    call fUserOpen
    count i in cgiUserID
    if (i = 36)
.        // there's a user ID, so get the record (userID implies that we're doing
.        //  an edit)
        packkey fUserIKey from cgiUserID

        filepi 99;fUserFiles
            call fUserIRead
            if over
                move "No userID" to errorMsg
                filepi 0
                call CGIDisplayFile using "usermaint.htt"
            endif

            move fUserIO.usertype into oldusertype

.            // Check for duplicate email
            packkey fUser3IKey with cgiemail,""
            call fUser3IRead
            call fUser3IReadKS
            if not over
                if (cgiemail = fUserIO.email and cgiuserid != fUserIO.userID)
.                    // display message here
                    pack errorMsg from "This email address is already registered."
                    call actionDisplay
                    return
                endif
            endif

.            // check for duplicate username
            packkey fUser2IKey with cgiusername,""
            call fUser2IRead
            call fUser2IReadKS
            if not over
                if (cgiusername = fUserIO.username and cgiuserID != fUserIO.userID)
.                    // display message here
                    pack errorMsg from "Please choose a different username."
                    call actionDisplay
                    return
                endif
            endif

.            // reset User record for update
            packkey fUserIKey from cgiuserid
            call fUserIRead
            if over
                pack errorMsg from "Record not located."
                call actionDisplay
                return
            endif

.START PATCH 1.1 ADDED LOGIC
          if (thisSession.userType = "A")
                    call      VerifyCompany using N1
                    if (N1 <> C0)
                              if (N1 = C1)                  .Invalid Company Record
                                        pack errorMsg from "Valid Company Number Required."
                              elseif (N1 = C2)    .Status should be Client
                                        pack errorMsg from "Company is actually a Client."
                              elseif (N1 = C3)    .Status should be Consultant
                                        pack errorMsg from "Company is actually a Consultant."
                              elseif (N1 = C4)    .Status should be Broker
                                        pack errorMsg from "Company is actually a Broker."
                              endif
                              call actionDisplay
                              return
                    endif
          endif
.END PATCH 1.1 ADDED LOGIC

            move cgiFName to fUserIO.fname
            move cgiLName to fUserIO.lname
            move cgiAddress to fUserIO.address
            move cgiCity to fUserIO.city
            move cgiState to fUserIO.state
            move cgiZip to fUserIO.zip
            move cgiPhone to fUserIO.telephone
.START PATCH 1.2 REMOVED LOGIC
.            move cgiFax to fUserIO.fax
            clear fUserIO.fax
.END PATCH 1.2 REMOVED LOGIC
            move cgiEmail to fUserIO.email
            move cgiUsername to fUserIO.username

            if (thisSession.userType = "A")
                move cgiOrganization to fUserIO.organization
                move cgiCompfunction to fUserIO.compfunction
                move cgiArea to fUserIO.area
                move cgiinvoiceflag to fUserIO.invoiceflag
                move cgiUsertype to fUserIO.usertype
                count i in cgiCompanynumber
                if (i < 6)
                    move cgiCompanynumber into nwk6
                    move nwk6 into cgiCompanynumber
                    replace " 0" in cgiCompanynumber
                endif
                move cgiCompanynumber to fUserIO.companyNumber
            endif

.            // if there is a cgiPassword, we will update that
            count i in cgiPassword
            if not zero
                move cgiPassword to fUserIO.password
            endif
.START PATCH 1.5 ADDED LOGIC
            if (cgitos = "Y")
                    move      "1",fUserIO.tos
            else
                    clear     fUserIO.tos
            endif
.END PATCH 1.5 ADDED LOGIC

            call fUserUpdate

.            // if we just approved a user, send an email
            if (oldusertype = "P")
                call emailAdmin
                call emailUser
            endif

        filepi 0
    else
.        // Check for an existing record
        packkey fUser3IKey with cgiemail,""
        call fUser3IRead
        call fUser3IReadKS
        if not over
            if (cgiemail = fUserIO.email and cgiuserid != fUserIO.userid)
                // display message here
                pack errorMsg from "This email address is already registered."
                call actionDisplay
                return
            endif
        endif

        packkey fUser2IKey with cgiusername,""
        call fUser2IRead
        call fUser2IReadKS
        if not over
            if (cgiusername = fUserIO.username and cgiuserid != fUserIO.userid)
                // display message here
                pack errorMsg from "Please choose a different username."
                call actionDisplay
                return
            endif
        endif

        move cgiFName to fUserIO.fname
        if (cgiFName ="")
            move "Please enter your first name." to errorMsg
            call actionDisplay
            return
        endif

        move cgiLName to fUserIO.lname
        if (cgiLName ="")
            move "Please enter a last name." to errorMsg
            call actionDisplay
            return
        endif

        move cgiOrganization to fUserIO.organization
        if (cgiOrganization ="")
            move "Please enter your organization." to errorMsg
            call actionDisplay
            return
        endif

        move cgiAddress to fUserIO.address
        if (cgiAddress ="")
            move "Please enter your address." to errorMsg
            call actionDisplay
            return
        endif

        move cgiCity to fUserIO.city
        if (cgiCity ="")
            move "Please enter a city." to errorMsg
            call actionDisplay
            return
        endif

        move cgiState to fUserIO.state
        if (cgiState ="")
            move "Please enter a state." to errorMsg
            call actionDisplay
            return
        endif

        move cgiZip to fUserIO.zip
        if (cgiZip ="")
            move "You must enter a zip code!" to errorMsg
            call actionDisplay
            return
        endif

.        //Validation
        if (cgiphone = "")
            move "Please enter a valid phone number with no dashes or spaces." to errorMsg
            call actionDisplay
            return
        endif

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

.START PATCH 1.2 REMOVED LOGIC
.        count i in cgifax
.        if (i != 0)
.            if (i = 10)
.                type cgifax
.                if not equal
.                    move "Please enter a valid fax with no dashes or spaces." to errorMsg
.                    call actionDisplay
.                    return
.                endif
.            else
.                move "Please enter fax number with area code and no dashes or spaces." to errorMsg
.                call actionDisplay
.                return
.            endif
.        endif
.END PATCH 1.2 REMOVED LOGIC

        move cgiemail to cwk60
        scan "@" in cwk60
        if not equal
            move "Please enter a valid email address." to errorMsg
            call actionDisplay
            return
        endif

        move cgiUsername to fUserIO.username
        if (cgiUsername ="")
            move "Please enter a username" to errorMsg
            call actionDisplay
            return
        endif

        move cgiPassword to fUserIO.password
        if (cgiPassword ="")
            move "Please enter a password" to errorMsg
            call actionDisplay
            return
        endif

        move cgiUsertype to fUserIO.usertype
        if (cgiUsertype ="")
            move "Please enter user type" to errorMsg
            call actionDisplay
            return
        endif

        move cgiCompanynumber to fUserIO.companyNumber
        if (cgiCompanynumber ="")
            move "Please enter company number." to errorMsg
            call actionDisplay
            return
        else
.START PATCH 1.1 REPLACED LOGIC
.            // read file for a valid company number
.            pack mkey from cgicompanynumber
.            call nmlrkey
.            if over
.                move "Please enter a valid company number" to errorMsg
.                call actionDisplay
.                return
.            endif
                    // read file for a valid company number
                    if (thisSession.userType = "A")
                              call      VerifyCompany using N1
                              if (N1 <> C0)
                                        if (N1 = C1)                  .Invalid Company Record
                                                  pack errorMsg from "Valid Company Number Required."
                                        elseif (N1 = C2)    .Status should be Client
                                                  pack errorMsg from "Company is actually a Client."
                                        elseif (N1 = C3)    .Status should be Consultant
                                                  pack errorMsg from "Company is actually a Consultant."
                                        elseif (N1 = C4)    .Status should be Broker
                                                  pack errorMsg from "Company is actually a Broker."
                                        endif
                                        call actionDisplay
                                        return
                              endif
                    endif
.END PATCH 1.1 REPLACED LOGIC
        endif
        clear fUserIO
        move cgiFName to fUserIO.fname
        move cgiLName to fUserIO.lname
        move cgiAddress to fUserIO.address
        move cgiCity to fUserIO.city
        move cgiState to fUserIO.state
        move cgiZip to fUserIO.zip
        move cgiPhone to fUserIO.telephone
.START PATCH 1.2 REMOVED LOGIC
.        move cgiFax to fUserIO.fax
.END PATCH 1.2 REMOVED LOGIC
        move cgiEmail to fUserIO.email
        move cgiusername to fUserIO.username
        move cgipassword to fUserIO.password
        move cgiOrganization to fUserIO.organization
        move cgiCompfunction to fUserIO.compfunction
        move cgiArea to fUserIO.area
        move cgiInvoiceFlag to fUserIO.invoiceFlag
        move cgiUsertype to fUserIO.usertype
        count i in cgiCompanynumber
        if (i < 6)
            move cgiCompanynumber into nwk6
            move nwk6 into cgiCompanynumber
            replace " 0" in cgiCompanynumber
        endif
        move cgiCompanynumber to fUserIO.companyNumber
.START PATCH 1.5 ADDED LOGIC
.         if (cgitos = "Y")
                    move      "1",fUserIO.tos
.         else
.                   clear     fUserIO.tos
.         endif
.END PATCH 1.5 ADDED LOGIC
        call fUserWrite
    endif

    move fUserIO.userID into cgiuserid

    call cgiDisplayFile using "usermaintresp.htt"
.START PATCH 1.4 ADDED LOGIC
          call      CGITrack using SID,"0026"
.END PATCH 1.4 ADDED LOGIC

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
doStateCodes routine

    call CGISelectState using fUserIO.state

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
doUserTypeCodes routine

    call CGISelectUserType using fUserIO.userType

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
doInvoiceCodes routine

    call CGISelectInvoiceFlag using fUserIO.invoiceFlag

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
doCompanyNumbers routine

    call CGISelectCompanyNumber using fUserIO.companyNumber

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
actionDelete

    call fUserOpen
.    // there's a user ID, so get the record (userID implies that we're doing
.    //  an edit)
    packkey fUserIKey from cgiUserID
    filepi 99;fUserFiles
        call fUserIRead
        if over
.            // error!
.START PATCH 1.4 REPLACED LOGIC
.            move "Unable to delete user account.  Please contact the webmaster." to errorMsg
            move "Unable to delete user account.  Please contact the <a href=#"#/index.php/contact-us/#";#"> website administrator</a>." to errorMsg
.END PATCH 1.4 REPLACED LOGIC
            call cgiDisplayFile using "usermaint.htt"
            filepi 0
            return
        endif
        call fUserDelete
    filepi 0

    move fUserIO.fname to cgifname
    move fUserIO.lname to cgilname

    call cgiDisplayFile using "usermaintresp2.htt"
.START PATCH 1.4 ADDED LOGIC
          call      CGITrack using SID,"0027"
.END PATCH 1.4 ADDED LOGIC

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
emailAdmin

.    // Email the Admin
    clear mailBody
    append "First Name: " to mailBody
    append cgifname to mailBody
    append newline2 to mailBody
    append "Last Name: " to mailBody
    append cgilname to mailBody
    append newline2 to mailBody
    append "Organization: " to mailBody
    append cgiorganization to mailBody
    append newline2 to mailBody
    append "Email: " to mailBody
    append cgiemail to mailBody
    append newline2 to mailBody
    append "Username: " to mailBody
    append cgiusername to mailBody
    append newline2 to mailBody
    append "User approval email has been sent." to mailBody
    reset mailBody

.    // create the mailer object
.begin patch 1.7
.          trap noMailer if object
.        create objMailer,class="Dundas.Mailer"
.    trapclr object
.end patch 1.7

.    // get Admin email addresses
    packkey fUser4IKey from "A"
    call fUser4IRead
          clear     mailto
    loop
        call fUser4IReadKS
        until over
        while (fUserIO.userType = "A")
.begin patch 1.7
.        objMailer.TOs.Add using fUserIO.email
          append    fUserIO.email,mailto
          append    ",",mailto
    repeat
          reset     mailto
.end patch 1.7

.    // Get from address - plbwin.ini
    pack cwk260 from "nin;ADMIN_EMAIL"
    clock ini into cwk260
    if over
        move "Error reading email address from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    move cwk260 into fromAddress
.begin patch 1.7
          pack mailfrom,fromaddress
.end patch 1.7

.    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
.begin patch 1.7
.    objMailer.SMTPRelayServers.Add using cwk260
          pack      mailserver,cwk260
.end patch 1.7

.    //  email settings
.begin patch 1.7
.    setprop objMailer,*FromAddress=fromAddress
.    setprop objMailer,*Subject="NEW CLIENT APPROVAL"
.    setprop objMailer,*Body=mailBody
          pack      mailsubjct,"NEW CLIENT APPROVAL"

.    // send the email
.    trap noSend if object
.        objMailer.SendMail
.    trapclr object
.
.    destroy objMailer
          Move      "Y",Mailtrace

          Call      SendMail

.end patch 1.7
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
.begin patch 1.7
    move cwk260 into fromAddress
          pack      mailfrom,fromaddress
.end patch 1.7

    clear mailBody
    append cgifname to mailBody
    append "," to mailBody
    append newline2 to mailBody
    append "Thank you for registering with Names in the News." to mailBody
    append " Your registration has been approved by the administrator." to mailBody
    append " You will be able to log in to the Names" to mailBody
    append " in the News website at http://www.NamesintheNews.com" to mailBody
.begin patch 1.7
.    append cgiServerName to mailBody
.    append "/plb-bin/login.plc." to mailBody
.end patch 1.7
    append newline2 to mailBody
    append newline2 to mailBody
    append "If you have any questions, you can contact us at " to mailBody
    append fromAddress to mailBody
    append "." to mailBody
    append newline2 to mailBody
    append "Thank you for using Names in the News!" to mailBody
    reset mailBody

.begin patch 1.7
    // create the mailer object
.    trap noMailer if object
.        create objMailer,class="Dundas.Mailer"
.    trapclr object

.    objMailer.TOs.Add using cgiemail
          pack      Mailto,cgiemail
.begin patch 1.7

    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
.begin patch 1.7
.    objMailer.SMTPRelayServers.Add using cwk260
          pack      Mailserver,cwk260
.end patch 1.7

    //  email settings
.begin patch 1.7
.    setprop objMailer,*FromAddress=fromAddress
.    setprop objMailer,*Subject="Your Names in the News Account Status"
.    setprop objMailer,*Body=mailBody
          pack      mailsubjct,"Your Names in the News Account Status"

    // send the email
.    trap noSend if object
.        objMailer.SendMail
.    trapclr object
.
.    destroy objMailer

          Move      "Y",Mailtrace

          Call      SendMail
.end patch 1.7
    // finished emailing the user
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

.START PATCH 1.1 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
DisplayLoadCompany
          move      C3,COMPLOCK
        call        Trim using COMPFLD
        if (COMPFLD <> "")
          call      COMPKEY
          if not over
                    move      COMPCOMP,cgicompanyname
          endif
        endif
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
VerifyCompany Routine FrmPtr
.FrmPtr = Return Value: '0'-Successful, '1'-Over
.                             '2'-Status should be Mailer
.                             '3'-Status should be Consultant
.                             '4'-Status should be Broker
.
          move      C0,FrmPtr .Initialize
          move      cgiCompanynumber,COMPFLD
          move      C3,COMPLOCK
          call      ZFillIt using COMPFLD,C0
          call      COMPKEY
          if over
                    move      C1,N1
          else
                    move      COMPCOMP,cgicompanyname
.         //We need to allow entering of other stati:  Admin, Suspended, Pending, etc.
                    if (COMPMLRFLG = "T" & (cgiUsertype = "O" | cgiUsertype = "B"))                 .NOT "C"
                              move      C2,N1
                    elseif (COMPCLRFLG = "T" & (cgiUsertype = "C" | cgiUsertype = "B"))   .NOT "O"
                              move      C3,N1
                    elseif (COMPBRKFLG = "T" & (cgiUsertype = "O" | cgiUsertype = "C"))   .NOT "B"
                              move      C4,N1
                    endif
          endif
          return
///////////////////////////////////////////////////////////////////////////////
.END PATCH 1.1 ADDED LOGIC

.START PATCH 1.5 ADDED LOGIC
actionSearch
          move      cgiCompanynumber,COMPFLD
          move      C3,COMPLOCK
          call      ZFillIt using COMPFLD,C0
          call      COMPKEY
          if over
                    move      "Company does not exist!",cgicompanyname
          else

                    count i in cgiUserID
                    if (i = 36)
                              // there's a user ID, so get the record (userID implies that we're doing
                              //  an edit)
                              call fUserOpenReadOnly
                              packkey fUserIKey from cgiUserID
                              call fUserIRead
                              if over
                                  move "User Record Not Located" to errorMsg
                                  clear cgiUserID
                              endif
                              if (fUserIO.tos = "1")
                                        pack      cgitos,"<input type='checkbox' name='cgitos' value='Y' checked> TOS Approval Required"
                              else
                                        pack      cgitos,"<input type='checkbox' name='cgitos' value='Y'> Force Re-Approval of TOS"
                              endif
                    endif
                    move      COMPCOMP,cgicompanyname
                    move      COMPCOMP,cgiOrganization
                    move      COMPADDR,cgiAddress
                    move      COMPCITY,cgiCity
                    call      Trim using COMPSTATE
                    uppercase COMPSTATE
                    move      COMPSTATE,cgiState
                    move      cgiState,fUserIO.state
                    move      COMPZIP,cgiZip
                    move      COMPPHONE,cgiPhone
                    if (COMPMLRFLG = "T")
                              move      "C",cgiUsertype
                    elseif (COMPCLRFLG = "T")
                              move      "O",cgiUsertype
                    elseif (COMPBRKFLG = "T")
                              move      "B",cgiUsertype
                    endif
                    move      cgiUsertype,fUserIO.userType
          endif
          call CGIDisplayFile using "usermaint.htt"
          return
.END PATCH 1.5 ADDED LOGIC

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

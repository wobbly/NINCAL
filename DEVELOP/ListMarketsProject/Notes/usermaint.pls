///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   usermaint.pls                                                //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   23 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI application                                              //
//                                                                           //
// REVISION:    VER01   23 NOV 2004 JBROWN    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
cwk1                dim     1
cwk60               dim     %60
cgiFunction         dim     %1
cgiaddress          dim     %80
cgiarea             dim     %32
cgicity             dim     %32
cgicompfunction     dim     %32
cgicompanynumber    dim     %6
cgiemail            dim     %60
cgifax              dim     %10
cgifname            dim     %32
cgilname            dim     %32
cgiorganization     dim     %32
cgipassword         dim     %20
cgiphone            dim     %10
cgistate            dim     %2
cgiusername         dim     %20
cgizip              dim     %10
cgiusertype         dim     %9
cgiUserID           dim     %36
cgiDeleteButton     dim     %80

httName             init    "usermaint.htt"

i                   integer 4
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate using "Admin Only"

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    //Insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif
    call CGIParse using "cgifname",cgifname
    call CGIParse using "cgilname",cgilname
    call CGIParse using "cgiorganization",cgiorganization
    call CGIParse using "cgiaddress",cgiaddress
    call CGIParse using "cgicity",cgicity
    call CGIParse using "cgistate",cgistate
    call CGIParse using "cgizip",cgizip
    call CGIParse using "cgiphone",cgiphone
    call CGIParse using "cgifax",cgifax
    call CGIParse using "cgicompfunction",cgicompfunction
    call CGIParse using "cgiarea",cgiarea
    call CGIParse using "cgiusername",cgiusername
    call CGIParse using "cgipassword",cgipassword
    call CGIParse using "cgiemail",cgiemail
    call CGIParse using "cgiusertype",cgiusertype
    call CGIParse using "cgiuserid",cgiUserID
    call CGIParse using "cgicompanynumber",cgicompanynumber


    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data

    call CGIParse using "cgifunction",cgiFunction
    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the form results
        call actionProcess
    case "2"
        // process the form results
        call actionDelete
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

    // this will display the (HTT) document we've created for this page
     debug
    // check for userID
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

        move fUserIO.fname to cgiFName
        move fUserIO.lname to cgiLName
        move fUserIO.organization to cgiOrganization
        move fUserIO.address to cgiAddress
        move fUserIO.city to cgiCity
        move fUserIO.state to cgiState
        move fUserIO.zip to cgiZip
        move fUserIO.telephone to cgiPhone
        move fUserIO.fax to cgiFax
        move fUserIO.email to cgiEmail
        move fUserIO.compfunction to cgiCompfunction
        move fUserIO.area to cgiArea
        move fUserIO.username to cgiUsername
        clear cgiPassword
        move fUserIO.usertype to cgiUsertype
        move fUserIO.companyNumber to cgiCompanynumber

        if (thisSession.userType = "A")
            // this user is an administrator - they can do whatever they
            //  want
            pack cgiDeleteButton from "<input type=button value='Delete' ":
               "onClick='doDelete();'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
            call CGIDisplayFile using "usermaint.htt"
        elseif (thisSession.userID != cgiUserID)
            // the user is trying to edit somebody else's record and they
            //  are not an administrator
            stream *STDOUT,"Access denied - you do not have permission to edit this record.<br>"
        else
            call CGIDisplayFile using "usermaintclient.htt"
        endif
    elseif (thisSession.userType = "A")
        call CGIDisplayFile using "usermaint.htt"
    else
        // make sure a client can't do an add...
        stream *STDOUT,"Access denied - you do not have permission to modify records.<br>"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // logic to process the data that we've parsed from the CGI process

    //  add logic to process the data that we've parsed from the CGI process
    // check for userID
    call fUserOpen
    count i in cgiUserID
    if (i = 36)
        // there's a user ID, so get the record (userID implies that we're doing
        //  an edit)
        packkey fUserIKey from cgiUserID

        filepi 99;fUserFiles
            call fUserIRead
            if over
                move "No userID" to errorMsg
                filepi 0
                call CGIDisplayFile using "usermaint.htt"
            endif

            // Check for duplicate email
            packkey fUser3IKey with cgiemail,""
            call fUser3IRead
            call fUser3IReadKS
            if not over
                if (cgiemail = fUserIO.email and cgiuserid != fUserIO.userID)
                    // display message here
                    pack errorMsg from "This email address is already registered."
                    call actionDisplay
                    return
                endif
            endif

            // check for duplicate username
            packkey fUser2IKey with cgiusername,""
            call fUser2IRead
            call fUser2IReadKS
            if not over
                if (cgiusername = fUserIO.username and cgiuserID != fUserIO.userID)
                    // display message here
                    pack errorMsg from "Please choose a different username."
                    call actionDisplay
                    return
                endif
            endif

            // reset User record for update
            packkey fUserIKey from cgiuserid
            call fUserIRead
            if over
                pack errorMsg from "Record not located."
                call actionDisplay
                return
            endif
            move cgiFName to fUserIO.fname
            move cgiLName to fUserIO.lname
            move cgiAddress to fUserIO.address
            move cgiCity to fUserIO.city
            move cgiState to fUserIO.state
            move cgiZip to fUserIO.zip
            move cgiPhone to fUserIO.telephone
            move cgiFax to fUserIO.fax
            move cgiEmail to fUserIO.email
            move cgiUsername to fUserIO.username

            if (thisSession.userType = "A")
                move cgiOrganization to fUserIO.organization
                move cgiCompfunction to fUserIO.compfunction
                move cgiArea to fUserIO.area
                move cgiUsertype to fUserIO.usertype
                move cgiCompanynumber to fUserIO.companyNumber
            endif

            // if there is a cgiPassword, we will update that
            count i in cgiPassword
            if not zero
                move cgiPassword to fUserIO.password
            endif

            call fUserUpdate
        filepi 0
    else
        // Check for an existing record
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

        //Validation
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
            // read file for a valid company number
            pack mkey from cgicompanynumber
            call nmlrkey
            if over
                move "Please enter a valid company number" to errorMsg
                call actionDisplay
                return
            endif
        endif
        clear fUserIO
        move cgiFName to fUserIO.fname
        move cgiLName to fUserIO.lname
        move cgiAddress to fUserIO.address
        move cgiCity to fUserIO.city
        move cgiState to fUserIO.state
        move cgiZip to fUserIO.zip
        move cgiPhone to fUserIO.telephone
        move cgiFax to fUserIO.fax
        move cgiEmail to fUserIO.email
        move cgiusername to fUserIO.username
        move cgipassword to fUserIO.password
        move cgiOrganization to fUserIO.organization
        move cgiCompfunction to fUserIO.compfunction
        move cgiArea to fUserIO.area
        move cgiUsertype to fUserIO.usertype
        move cgiCompanynumber to fUserIO.companyNumber
        call fUserWrite
    endif

    move fUserIO.userID into cgiuserid

    call cgiDisplayFile using "usermaintresp.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doStateCodes routine

    call CGISelectState using fUserIO.state

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doUserTypeCodes routine

    call CGISelectUserType using fUserIO.userType

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doInvoiceCodes routine

    call CGISelectInvoiceFlag using fUserIO.invoiceFlag

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doCompanyNumbers routine

    call CGISelectCompanyNumber using fUserIO.companyNumber

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDelete

    call fUserOpen
    // there's a user ID, so get the record (userID implies that we're doing
    //  an edit)
    packkey fUserIKey from cgiUserID
    filepi 99;fUserFiles
        call fUserIRead
        if over
            // error!
            move "Unable to delete user account.  Please contact the webmaster." to errorMsg
            call cgiDisplayFile using "usermaint.htt"
            filepi 0
            return
        endif
        call fUserDelete
    filepi 0

    move fUserIO.fname to cgifname
    move fUserIO.lname to cgilname

    call cgiDisplayFile using "usermaintresp2.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

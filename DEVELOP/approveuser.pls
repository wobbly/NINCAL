///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   approveuser.pls                                              //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   24 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI application                                              //
//                                                                           //
// REVISION:    VER01   24 NOV 2004 JBROWN    Created                        //
//                                                                           //
//         release  1.2  27FEB2015  RVW  Separate Header, Menu and CSS       //
//         release  1.1  29DEC2005  ASH  Added tracking Logic                //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////

buffer              dim     65535
cgiColor            dim     %1
cgiemail            dim     %60
cgiFunction         dim     %1
cgiFname            dim     %32
cgiLname            dim     %32
cgiOrganization     dim     %32
cgiUsertype         dim     %1
cgioldusertype      dim     %9
cgiZip              dim     %10
cgiUserID           dim     %36
cgiusername         dim     %20
cgiServerName       dim     %260
htmlTemplate        dim     65535
cwk60               dim     60
cwk260              dim     260
fromAddress         dim     60
.mailBody            dim     32768            .in cons   03/15/2007 DLH

foundRecord         form    1

httName             init    "approveuserresults.htt"
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
. release 1.2 - begin comment out
.    call CGIRenderHeader
. release 1.2 - end comment out

. release 1.2 - begin new logic
    call CGIRenderUniversalCSS
    call CGIRenderMenu
. release 1.2 - end comment out


    call CGIParse using "cgiuserid",cgiuserid
    call CGIParse using "SID",SID
    call CGIParse using "errorMsg",errorMsg

    ///////////////////////////////////////////////////////////////////////////
    // TO DO: insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "cgifunction",cgiFunction

//    debug
//    move "DED54E88-E25E-3141-9673-9F93B849ECCC" into cgiuserid
//    move "1" into cgifunction

    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the form results
        call actionProcess
    case "2"
        //process the form results
        call actionDelete
    case "3"
        call actionProcessInvoice
    case "4"
        call actionProcessInvoice
    default
        // display a form
        call actionDisplay
    endswitch

    // this will send the static bottom part of the HTML document
. release 1.2 - begin comment out
.    call CGIRenderFooter
. release 1.2 - end comment out

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

    call CGIDisplayFile using "approveuserresults.htt"
.START PATCH 1.1 ADDED LOGIC
           call       CGITrack using SID,"0029"
.END PATCH 1.1 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // Add logic to process the data that we've parsed from the
    //  CGI process
    call fUserOpen
    packkey fUserIKey from cgiUserID
    call fUserIRead
    if over
        // No record found
        move "USER NOT UPDATED" to errorMsg
    else
        // get cgi variables from file
        move fUserIO.fname into cgifname
        move fUserIO.lname into cgilname
        move fUserIO.organization into cgiorganization
        move fUserIO.Email into cgiemail
        move fUserIO.username into cgiusername

        // update user type
        move "C" to fUserIO.userType
        call fUserUpdate
    endif

    call CGIDisplayFile using "approveuserresults.htt"
.START PATCH 1.1 ADDED LOGIC
           call       CGITrack using SID,"0030"
.END PATCH 1.1 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcessInvoice

    // Add logic to process the data that we've parsed from the
    //  CGI process
    call fUserOpen
    packkey fUserIKey from cgiUserID
    call fUserIRead
    if over
        // No record found
        move "USER NOT UPDATED" to errorMsg
    else
        move "N" to fUserIO.invoiceFlag
        if (cgiFunction = "3")
            move "Y" to fUserIO.invoiceFlag
        endif
        call fUserUpdate
    endif
    call CGIDisplayFile using "approveuserresults.htt"

.START PATCH 1.1 ADDED LOGIC
           if (cgiFunction = "3")
                      call       CGITrack using SID,"0032"
           else
                      call       CGITrack using SID,"0033"
           endif
.END PATCH 1.1 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
displayResults routine htmlTemplate

    // this is where we display the results - it gets called when the
    //  template gets processed.  Everything between <%DISPLAYRESULTS%> and
    //  <%/DISPLAYRESULTS%> is passed into htmlTemplate and program control
    //  passes here.

    clear foundRecord
    call fUserOpenReadOnly
    packkey fUser4IKey from "P"
    call fUser4IRead

    loop
        call fUser4IReadKS
        until over
        set foundRecord
        while (fUserIO.userType = "P")
        if (cgiColor = "1")
            clear cgiColor
        else
            move "1" to cgiColor
        endif

        move fUserIO.fname to cgiFname
        move fUserIO.lname to cgiLname
        move fUserIO.organization to cgiOrganization
        move fUserIO.zip to cgiZip
        move fUserIO.usertype to cgiUsertype
        move fUserIO.userID to cgiUserID
        move htmlTemplate to buffer

        // now display the template information and insert our data into it
        call cgiDisplay using buffer
        stream *stdout,buffer

    repeat

    if (foundRecord = 0)
        stream *stdout,"<tr><td align=center colspan=5 class=datarow1>No member records awaiting approval</td></tr>",newline2
    endif


    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
displayResults2 routine htmlTemplate

    // this is where we display the results - it gets called when the
    //  template gets processed.  Everything between <%DISPLAYRESULTS%> and
    //  <%/DISPLAYRESULTS%> is passed into htmlTemplate and program control
    //  passes here.

    clear foundRecord
    call fUserOpenReadOnly
    clear fUserIKey
    call fUserIRead
    loop
        call fUserIReadKS
        until over
        continue if (fUserIO.invoiceFlag != "P")
        if (cgiColor = "1")
            clear cgiColor
        else
            move "1" to cgiColor
        endif

        move fUserIO.fname to cgiFname
        move fUserIO.lname to cgiLname
        move fUserIO.organization to cgiOrganization
        move fUserIO.zip to cgiZip
        move fUserIO.usertype to cgiUsertype
        move fUserIO.userID to cgiUserID
        move htmlTemplate to buffer

        // now display the template information and insert our data into it
        set foundRecord
        call cgiDisplay using buffer
        stream *stdout,buffer
    repeat

    if (not foundRecord)
        stream *stdout,"<tr><td align=center colspan=5 class=datarow1>No member records awaiting invoice access</td></tr>",newline2
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDelete
    // Add logic to process the data that we've parsed from the
    //  CGI process
    call fUserOpen
    packkey fUserIKey from cgiUserID
    call fUserIRead
    if over
        // no record found
        move "USER NOT DELETED" to errorMsg
    else
        call fUserDelete
    endif

    call CGIDisplayFile using "approveuserresults.htt"

.START PATCH 1.1 ADDED LOGIC
           call       CGITrack using SID,"0031"
.END PATCH 1.1 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

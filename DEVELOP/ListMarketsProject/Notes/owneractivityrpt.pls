///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   owneractivityrpt.pls                                         //
//                                                                           //
//    AUTHOR:   bjackson@adjacency.net                                       //
//                                                                           //
//      DATE:   15 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    NIN owner activity report online                             //
//                                                                           //
//              The owner activity report requests a list number, and if     //
//               the user has permission to view the list owner's data, will //
//               search for an activity report in PDF format which is named: //
//               OOOOOOLLLLLLYYMM.PDF, where OOOOOO is the 6 digit owner #,  //
//               and LLLLLL is the 6 digit list #.                           //
//                                                                           //
// REVISION:    VER01   15 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
//    include ndatdd.inc
    include nowndd.inc
//    include nxrfdd.inc
//    include compdd.inc
///////////////////////////////////////////////////////////////////////////////
cgiFunction     dim     %1
cginumber       dim     %6
cginame         dim     %99
cgicompany      dim     %99
cgidue          dim     %99
cgipaid         dim     %99
cgiLink         dim     %99
cgiColor        dim     %1
cgiCompanyNo    dim     %6
cgiDate         dim     %20
cgilistnum      dim     %6

buffer          dim     32768
cwk2            dim     2
cwk4            dim     4
cwk32768        dim     32768
documentName    dim     260
monthNames      dim     9(12),("January"):
                              ("February"):
                              ("March"):
                              ("April"):
                              ("May"):
                              ("June"):
                              ("July"):
                              ("August"):
                              ("September"):
                              ("October"):
                              ("November"):
                              ("December")
#fileManagerSettings    dim     260
#MM             dim     2
#YY             dim     2
#YYYYMM         dim     6
#CC             dim     2

fTest           file

nwk2            form    2
nwk2a           form    2
nwk4            form    4
nwk6            form    6

httName         init    "owneractivityrpt.htt"

foundFile       integer 4
i               integer 4
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
//    call CGIParse using "cginumber",cgiNumber
    call CGIParse using "cgilistnum",cgilistnum

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

    // get the list info from nindat
    type cgilistnum
    if not equal
        move "0" to cgilistnum
    endif

    // right justify and zero-fill the input list number
    move cgilistnum to nwk6
    move nwk6 to cgilistnum
    replace " 0" in cgilistnum

    // We need to get the list information.  We don't worry about the OVER
    //  flag here, because we want to render the response page no matter what.
    //  Only in DISPLAYRESULTS do we deal with the missing data, because the
    //  rest of the page should be rendered even if we don't have any data
    packkey nDatFld from cgilistnum
    call NDATKEY
    move MLSTNAME to cgiName
    pack nxrffld from lstnum
    call nxrfkey
    packkey compfld3 from nxrfmlr
    call compkey3
    if over
        // there are not results
        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
        return
    endif

    // Enforce security.  If our company number does not match the list owner,
    //  then we can't display the results (unless we're an administrator)
//    if (thisSession.userCompany != compnum and thisSession.userType != "A")
//        move "You do not have permission to display reports for this list.<br><br>" to errorMsg
//        call CGIDisplayFile using "owneractivityrpt.htt"
//    else
        call CGIDisplayFile using "owneractivityrptresponse.htt"
//    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    // Counting the length of LSTNUM is essentially the same as checking for
    //  an OVER condition.  If we can't find the number for our list from the
    //  file IO variables, then we can assume an OVER exists, and we have
    //  no results.
    count i in LSTNUM
    if zero
        stream *stdout,"<tr><td colspan=6 align=center class='datarow'><br>No results for list number '":
            cgilistnum,"'<br><br></td></tr>",newline
    else
        // we use CGIColor to alternate the output of table rows between to options
        if (cgiColor = "1")
            clear cgiColor
        else
            move "1" to cgiColor
        endif

        unpack OWNNUM into cwk2,cwk4
        pack NOWNFLD from cwk4
        call NOWNKEY

        // pack all of our CGI output variables for display
        move LSTNUM to cgilistnum
        move MLSTNAME to cgiName
        move OWNOCPY to cgiCompany
        move "n/a" to cgiPaid
        move "n/a" to cgiDue

        // Logic to find the most recent statement - we are going to look for a file
        //  name matching ownernumber+listnumber+currentyear+currentmonth.pdf.  If
        //  we find it, then we store that filename for use in the template.  If we
        //  don't find it, we start working backward one month at a time, until we
        //  hit JAN 2004.  If we still don't find any matching PDF files, we clear
        //  out our filename.
        clock timestamp into #YYYYMM
        unpack #YYYYMM into #CC,#YY,#MM
        loop
            pack filename from "..\data\weboars\",cwk4,LSTNUM,#CC,#YY,#MM,".PDF"
            getmode *openuseip=#fileManagerSettings
            setmode *openuseip=""
            set foundFile
            trap fileNotFound if io
                open fTest,filename,read
                close fTest
            trapclr io
            setmode *openuseip=#fileManagerSettings

            if (foundFile)
                // we found the file
                unpack OWNNUM into cwk2,cwk4
                pack documentName from cwk4,LSTNUM,#CC,#YY,#MM,".PDF"
                move #MM to nwk2
                pack cgiDate from monthNames(nwk2)," 20",#YY
                break
            endif

            move #YY to nwk2a
            move #MM to nwk2
            decr nwk2
            if zero
                move "12" to nwk2
                decr nwk2a
                if (nwk2a < 4)
                    // no pdf's found at all
                    clear cgiDate
                    clear documentName
                    break
                endif
            endif
            move nwk2 to #MM
            move nwk2a to #YY
            replace " 0" in #MM
            replace " 0" in #YY
        repeat

        // If the document name has data in it, we're going to display a link to that file.  We
        //  do this by packing up a short HTML string to send through the template.  If there
        //  is no document to link to, we just pack an "n/a" string instead.
        count i in documentName
        if zero
            pack cgiLink from "Nothing found."
        else
            pack cgiLink from "<a href='/statements/",documentName,"' target='_blank'><img src=""/images/pdf.gif"" border=0></a>"
        endif

        move buffer to cwk32768
        call CGIDisplay using cwk32768
        stream *stdout,cwk32768
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DOLISTNUMBER routine

    call CGIRenderListNumberInput using "",sessionIO.userCompany,sessionIO.userType

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
fileNotFound

    clear foundFile
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
//    include ndatio.inc
    include nownio.inc
//    include nxrfio.inc
//    include compio.inc
///////////////////////////////////////////////////////////////////////////////

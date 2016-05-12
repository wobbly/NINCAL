///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   memberdir.pls                                                //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   04 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI application                                              //
//                                                                           //
// REVISION:    VER01   04 NOV 2004 JBROWN    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
cgiColor        dim     %1
cgiDataRow      dim     %1
cgiDataSource   dim     %260
cgiFunction     dim     %1
cgiFname        dim     %32
cgiLname        dim     %32
cgiOrganization dim     %32
cgiReadPos      dim     %10
cgiZip          dim     %10
cgiUserID       dim     %36
cgiLinks        dim     %1024

action          dim     6
buffer          dim     32768
cmdString       dim     260
cwk1            dim     1
cwk10           dim     10
cwk260          dim     260
cwk32768        dim     32768
dataSource      dim     60
htmlTemplate    dim     65535
sortFileName    dim     260
tempFileName    dim     260

nwk10           form    10
readPosition    form    10
readToPosition  form    10

sortFile        file

i               integer 4

httName         init    "memberdirclient.htt"

#fileManagerSettings dim 260
///////////////////////////////////////////////////////////////////////////////
start

    // Set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    call CGIParse using "cgifname",cgiFname
    call CGIParse using "cgilname",cgiLname
    call CGIParse using "cgiorganization",cgiOrganization
    call CGIParse using "cgizip",cgiZip
    call CGIParse using "SID",SID

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
        // process the form results for the entire member directory
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
    // display appropriate HTT depending on who is logged in
    packkey sessionKey from SID
    call sessionRead
    if not over
        if (sessionIO.usertype = "A")
            move "memberdir.htt" into httName
        endif
    endif

    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

// 20 JAN 2005 MLYONS --> add logic to decide whether we are showing all records or not
//                        what button as clicked?

    //  CGI process
    clear errorMsg

    call cgiParse using "cgidatasource",cgiDatasource
    count i in cgiDatasource
    if (i = 0 and cgiFunction = "1")
        move "search" into action
        // Validate AAM key 1
        move cgiFname to cwk260
        replace "? " in cwk260
        squeeze cwk260 into cwk260
        count i in cwk260
        if (i > 0 and i < 3)
            append "Please enter at least 3 non-blank characters for first name!<br>" to errorMsg
        endif
        count i in cgifname
        if (i<>0)
            pack fUserAKey1 from "01L",cgifname
        endif

        // Validate AAM key 2
        move cgiLname to cwk260
        replace "? " in cwk260
        squeeze cwk260 into cwk260
        count i in cwk260
        if (i > 0 and i < 3)
            append "Please enter at least 3 non-blank characters for last name!<br>" to errorMsg
        endif
        count i in cgiLname
        if (i<>0)
            pack fUserAKey2 from "02L",cgiLname
        endif

        // Validate AAM key 3
        move cgiOrganization to cwk260
        replace "? " in cwk260
        squeeze cwk260 into cwk260
        count i in cwk260
        if (i > 0 and i < 3)
            append "Please enter at least 3 non-blank characters for the organization!<br>" to errorMsg
        endif
        count i in cgiOrganization
        if (i<>0)
            pack fUserAKey3 from "03L",cgiOrganization
        endif

        // Validate AAM key 4
        move cgiZip to cwk260
        squeeze cwk260 into cwk260
        replace "? " in cwk260
        count i in cwk260
        if (i > 0 and i < 3)
            append "Please enter at least 3 non-blank numbers for the zip code!<br>" to errorMsg
        endif
        count i in cgiZip
        if (i<>0)
            pack fUserAKey4 from "04L",cgiZip
        endif

        if (fUserAKey1 = "" and fUserAKey2 = "" and fUserAKey3 = "" and fUserAKey4 = "")
            append "Please fill in one or more fields to search by.<br>" to errorMsg
        endif

        //Msg if no info is given any any of the fields
        reset errorMsg
        count i in errorMsg
        if not zero
            call actionDisplay
            return
        endif
    endif

    //Check to see if user is a "admin" or not
    if (thisSession.userType = "A")
        call CGIDisplayFile using "memberdiradminresults.htt"
    else
        call CGIDisplayFile using "memberdirresults.htt"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
displayResults routine buffer

    // this is where we display the results - it gets called when the
    //  template gets processed.  Everything between <%DISPLAYRESULTS%> and
    //  <%/DISPLAYRESULTS%> is passed into htmlTemplate and program control
    //  passes here.

// 20 JAN 2005 MLYONS --> add logic for paging if we are showing all records
    clear cgiDatasource
    call cgiParse using "cgidatasource",cgiDatasource
    if over
        call createDataSource
    else
        move cgiDataSource to sortFileName
    endif

    trap noDataSource if io
        getmode *openuseip=#fileManagerSettings
        setmode *openuseip=""
        open sortFile,sortFileName,exclusive
        setmode *openuseip=#fileManagerSettings
    trapclr io
    debug
    // if readPos > 1 then we need to skip to that position in the file
    call cgiParse using "cgireadpos",cgiReadPos
    if over
        move "1" to cgiReadPos
    endif

    type cgiReadPos
    if not equal
        move "1" to cgiReadPos
    endif
    move cgiReadPos to readPosition

    if (readPosition < 1)
        move "1" to readPosition
    endif

    // skip over records to get to the proper position in our file.  We use
    //  readPosition-1 because we are already pointing at record one by default.
    for i from "1" to (readPosition-1)
        read sortFile,seq;cgiFname,cgiLname,cgiOrganization,cgiZip,cgiuserid
        until over
    repeat

    pack cwk260 from "nin;RECSPERPAGE"
    clock ini,cwk260
    if over
        move "20" to cwk260
    else
        bump cwk260 by 11
    endif

    type cwk260
    if not equal
        move "20" to cwk260
    endif

    move cwk260 to readToPosition

    // sort the temp file
    for i from "1" to readToPosition
        // read the temp file
        read sortFile,seq;cgiFname,cgiLname,cgiOrganization,cgiZip,cgiuserid
        until over
        // display the temp file
        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768
        if (cgiDataRow = "1")
            move "" to cgiDataRow
        else
            move "1" to cgiDataRow
        endif
    repeat

    clear cgiLinks
    if (readPosition > 1)
        // do a previous link
        append "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
        calc nwk10 = (readPosition - readToPosition)
        if (nwk10 < 1)
            move "1" to nwk10
        endif
        move nwk10 to cwk10
        squeeze cwk10 into cwk10
        append cwk10 to cgiLinks
        append "');#">&lt; &lt; Previous</a>" to cgiLinks
    endif

    append "&nbsp;&nbsp;&nbsp;&nbsp;" to cgiLinks

    read sortFile,seq;cwk1
    if not over
        // do a next link
        append "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
        calc nwk10 = (readPosition + readToPosition)
        move nwk10 to cwk10
        squeeze cwk10 into cwk10
        append cwk10 to cgiLinks
        append "');#">Next &gt; &gt;</a>" to cgiLinks
    endif

    reset cgiLinks

    return

// 20 JAN 2005 MLYONS --> end of changes
///////////////////////////////////////////////////////////////////////////////

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
createDataSource
    // create temp file
    debug
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"
    prep tempFile,tempFileName,exclusive

    call fUserOpenReadOnly

    if (action = "search")
        // do the AAM read
        call fUserARead
        if over
            // there are not results
            stream *stdout,"<table align='center'>",newline
            stream *stdout,"<tr><td colspan=4 align=center>No results found</td></tr>",newline
        else
            loop

                // we have results, so put the results into the cgiVariables that the
                //  template wants populated
                if (cgiColor = "1")
                    clear cgiColor
                else
                    move "1" to cgiColor
                endif

                write tempFile,seq;fUserIO.fName,fUserIO.lName,fUserIO.Organization,fUserIO.Zip,fUserIO.userID

//                move fUserIO.fname to cgiFname
//                move fUserIO.lname to cgiLname
//                move fUserIO.organization to cgiOrganization
//                move fUserIO.zip to cgiZip
//                move fUserIO.userID to cgiUserID
//                move htmlTemplate to buffer

                // now display the template information and insert our data into it
//                call cgiDisplay using buffer
//                stream *stdout,buffer

                call fUserAReadKG
            repeat until over
        endif
    else
        // set filepointer to eof
        loop
            read tempFile,seq;cwk1
            until over
        repeat
        packkey fUserIKey from "                                     "
        call fUserIRead
        loop
            call fUserIReadKS
            until over
            write tempFile,seq;fUserIO.fName,fUserIO.lName,fUserIO.Organization,fUserIO.Zip,fUserIO.userID
        repeat
    endif

    // restore these variables to their input state for hiding in the submit form
    call CGIParse using "cgifname",cgiFname
    call CGIParse using "cgilname",cgiLname
    call CGIParse using "cgiorganization",cgiOrganization
    call CGIParse using "cgizip",cgiZip

    close tempFile

    pack cmdString with tempFileName,",",sortFileName," -u,1-30"
    sort cmdString
    if over
        // no results
        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
        return
    endif

    move sortFileName to cgiDatasource

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

    noreturn
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

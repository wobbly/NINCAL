///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   onlineinvoicing.pls                                          //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   30 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    NIN invoice retrieval module                                 //
//                                                                           //
// REVISION:    VER01   30 NOV 2004 JBROWN    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include norddd.inc
    include ninvdd.inc
///////////////////////////////////////////////////////////////////////////////
cgiAgree                dim     %10
cgiCompanyName          dim     %55
cgiSysDate              dim     %24
cgiInvoice              dim     %99
cgiAmount               dim     %99
cgiLink                 dim     %99
cgiFunction             dim     %1
cgiUserID               dim     %36
cgiOrderNumber          dim     %6
cgiCampaignNumber       dim     %6
cgiPONumber             dim     %12
cgiMailerDate           dim     %10
cgiDataSource           dim     %260
cgiReadPos              dim     %10
cgiDataRow              dim     %1
cgiLinks                dim     %260
cgiLRN                  dim     %10
cgiOrderField           dim     %500

#fileManagerSettings    dim     ^
buffer                  dim     ^
cmdString               dim     ^
cwk1                    dim     1
cwk10                   dim     10
cwk260                  dim     ^
cwk32768                dim     ^
dataSource              dim     ^
outputFileName          dim     ^
pdfData                 dim     ^
sortFileName            dim     ^
tempFileName            dim     ^
yyyy                    dim     4

fInvoice                file
fOutput                 file
sortFile                file

nwk10                   form    10
readPosition            form    10
readToPosition          form    10

httName                 init    "onlineinvoicing.htt"

i                       integer 4
fileReady               integer 4
///////////////////////////////////////////////////////////////////////////////
start

    // smake our big variables
    smake #fileManagerSettings,260
    smake buffer,32768
    smake cmdString,260
    smake cwk260,260
    smake cwk32768,32768
    smake pdfData,260
    smake sortFileName,260
    smake tempFileName,260
    smake dataSource,260
    smake outputFileName,260

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader
    call CGIParse using "SID",SID
    call CGIParse using "cgiponumber",cgiPONumber
    call CGIParse using "cgicampaignnumber",cgiCampaignNumber
    call CGIParse using "cgiordernumber",cgiOrderNumber

    // this processes our company information
    // for non admin users
    if (thisSession.usertype != "A")
        call compOpen
        packkey compFld from thisSession.userCompany
        call compKey
        if over
            // no results
            stream *STDOUT,"No company set up in your user account.  Please contact support!"
            call CGIRenderFooter
            stop
        else
            count i in COMPOLDMLR
            if zero
                stream *STDOUT,"The company assigned to you in your user account appears to have ":
                    "the following problem: <br><br>",newline:
                    "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline:
                    "Please contact the website administrator to correct this problem."
                call CGIRenderFooter
                stop
            endif
        endif

        move compComp to cgiCompanyName
        chop cgiCompanyName
    endif

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "cgifunction",cgiFunction

    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the signup form
        call actionProcess
    case "2"
        // process the search form and display results
        call actionSearch
    case "3"
        // display an invoice
        call actionDisplayInvoice
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

    count i in thisSession.userID
    if (i != 36)
        move "User ID Not Located" to errorMsg
        return
    endif

    packkey fUserIKey from thisSession.userID
    call fUserOpenReadOnly
    call fUserIRead
    if over
        stream *stdout,"<span class='errorMsg'>":
            "Unable to locate user record.  Please contact MIS.":
            "</span>"
        return
    endif

    // Show invoice if flag is set to yes
    if (fUserIO.invoiceFlag = "Y")
        call CGIDisplayFile using "onlineinvoicingsearch.htt"
    else (fUserIO.invoiceFlag = "P")
        call CGIDisplayFile using "onlineinvoicingpending.htt"
//    else
//        call CGIDisplayFile using "onlineinvoicingsignup.htt"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // process the signup form submission
    call fUserOpen
    packkey fUserIKey from thisSession.UserID
    call fUserIRead
    if over
        stream *stdout,"<span class='errorMsg'>":
            "Unable to locate user record.  Please contact MIS.":
            "</span>"
        return
    endif

    // the company name is in compComp, so we move it to a CGI variable
    move compCOMP to cgiCompanyName
    clock sysdate into cgiSysDate

    call cgiParse using "cgiagree",cgiAgree
    if not over
        move "P" to fUserIO.invoiceFlag
        call fUserUpdate
        call CGIDisplayFile using "onlineinvoicingpending.htt"
//    else
        // display a message asking user to check that they have read and agree
//        move "Please check the box below to agree to these terms and conditions." to errorMsg
//        call CGIDisplayFile using "onlineinvoicingsignup.htt"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionSearch

    // find LR numbers that match our search criteria and display them
    call CGIDisplayFile using "onlineinvoicingresults.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYSEARCHRESULTS routine buffer

    // see if there is already a working file for the search results
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
        read sortFile,seq;cgiOrderNumber,cgiCampaignNumber,cgiPONumber,cgiMailerDate
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
        read sortFile,seq;cgiOrderNumber,cgiCampaignNumber,cgiPONumber,cgiMailerDate
        until over

        unpack cgiMailerDate into yyyy,mm,dd
        pack cgiMailerDate from mm,"/",dd,"/",yyyy

        count nwk10 in cgiCampaignNumber
        if zero
            move "n/a" to cgiCampaignNumber
        endif

        count nwk10 in cgiPoNumber
        if zero
            move "n/a" to cgiPoNumber
        endif

        move "1" to nInvPath
        packkey nInvFld from cgiOrderNumber
        call nInvKey
        if over
            pack cgiOrderField from cgiOrderNumber
        else
            pack cgiOrderField from "<a href='javascript:viewinvoice(""",cgiOrderNumber,""");'>",cgiOrderNumber,"</a>"
        endif

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
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
CREATEDATASOURCE
 debug
    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"
    prep tempFile,tempFileName,exclusive

    // loop through the NORD file displaying all the matches for our criteria
    if (thisSession.userType != "A")
        move COMPOLDMLR to MKEY
        call NMLRKEY
        if over
            stream *STDOUT,"The company assigned to you in your user account appears to have ":
                "the following problem: <br><br>",newline:
                "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline:
                "Please contact the website administrator to correct this problem."
            call CGIRenderFooter
            noreturn
            return
        else
            pack NORDFLD1 from "01X",mNum
        endif
    endif

    // pack up optional filters based on the search parameters
    count i in cgiPONumber
    if not zero
        pack nOrdFld3 from "03X",cgiPONumber
    endif

    clear error
    call NORDAIM

    loop
        until over
        if (cgiOrderNumber != "" and cgiOrderNumber != oLrn)
            // if we're searching by order number and it does not match
            match cgiOrderNumber to oLrn with "?"
            if not equal
                call nOrdKG
                continue
            endif
        endif

        if (cgiCampaignNumber != "" and cgiCampaignNumber != oCamp)
            // if we're searching by campaign number and it does not match
            match cgiCampaignNumber to oCamp with "?"
            if not equal
                call nOrdKG
                continue
            endif
        endif

        packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
        write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
        call NORDKG
    repeat
    close tempFile

    pack cmdString with tempFileName,",",sortFileName," -u,d,25-32"
    sort cmdString
    if over
        // no results
        if (cgiPONumber != "")
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for PO Number: ",cgiPONumber," <br><br></td></tr>",newline
            return
        elseif (cgiOrderNumber != "")
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for Order Number: ",cgiOrderNumber," <br><br></td></tr>",newline
            return
        elseif (cgiCampaignNumber != "")
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for PO Number: ",cgiCampaignNumber," <br><br></td></tr>",newline
            return
        else
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found." <br><br></td></tr>",newline
            return
        endif
    endif

    move sortFileName to cgiDatasource
    return

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplayInvoice

    call CGIDisplayFile using "onlineinvoicing.htt"
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    // displays the invoice file (if any) in an IFRAME object
    move "1" to nInvPath
    call cgiParse using "cgilrn",cgiLRN
    packkey nInvFld from cgiLRN
    call nInvKey
    if over
        stream *STDOUT,"<tr><td class='datarow1' colspan='3' align=center>There are no invoices ":
            "available at this time</td></tr>"
        return
    else
        calls "ninv002l;webGenerate" using cgiLRN
    endif

    pause "5"
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""

    pack filename from "c:\work\pdf\",cgiLRN,"PWEB.PDF"
    loop
        set fileReady
        trap notYet if io
            open fInvoice,filename,exclusive
        trapclr io
    repeat until (fileReady)
    setmode *openuseip=#fileManagerSettings

    pack filename from "/invoices/",cgiLRN,"PWEB.PDF"
    stream *STDOUT,"<center><iframe src='",filename,"' width='100%' height='600'></iframe>"
    stream *STDOUT,"<br><br>If your invoice does not appear above, or you wish to view it in full screen mode, ":
        "please click <a href='",filename,"' target='_blank'>here</a></center>"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
notYet

    // the file we're looking for isn't ready for us to get exclusive access
    //  to it yet.
    clear fileReady
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

    // the datasource referred to by the CGI variables could not be found
    noreturn
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
    include nordio.inc
    include ninvio.inc
///////////////////////////////////////////////////////////////////////////////

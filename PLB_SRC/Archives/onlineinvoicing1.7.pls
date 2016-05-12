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
//        Release   1.7       05JAN2006 ASH       Major overhaul - condensed Mlr PO and Mlr Key searches, removed Camp. Num search
//        Release   1.6       29DEC2005 ASH       Added Tracking Logic         //
//        Release   1.5       13DEC2005 ASH       Added Consultant file        //
//                                                          Modified TotPage logic       //
//                                                          Added link to Contacts page  //
//        Release   1.4       01DEC2005 ASH       Added logic paging/aam checking//
//        Release   1.3       23SEP2005 ASH       Limited Searches to 1 year   //
//                                                          Small bug fix                      //
//        Release   1.2       19AUG2005 ASH       Bug Fixes                    //
//        Release   1.1       15JUL2005 ASH       Exchange File Conversion     //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
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
.START PATCH 1.7 REPLACED LOGIC
.cgiCampaignNumber       dim     %6
cgiOther            dim       %12
cgiMailerKey                  dim       %12
.END PATCH 1.7 REPLACED LOGIC
cgiPONumber             dim     %12
cgiMailerDate           dim     %50
cgiDataSource           dim     %260
cgiReadPos              dim     %10
cgiDataRow              dim     %1
cgiLinks                dim     %260
cgiLRN                  dim     %10
cgiOrderField           dim     %500

.START PATCH 1.4 ADDED LOGIC
cgiPageNum          dim     %10
cgiTotPage          dim     %10
cgiRecNum           dim     %13
.START PATCH 1.5 ADDED LOGIC
N92                 form    9.2
.END PATCH 1.5 ADDED LOGIC
.END PATCH 1.4 ADDED LOGIC

#fileManagerSettings    dim     ^
buffer                  dim     ^
brokerFlag              dim     1
clientFlag              dim     1
consultantFlag          dim     1
cmdString               dim     ^
consultantNum           dim     6
cwk1                    dim     1
cwk10                   dim     10
cwk260                  dim     ^
cwk32768                dim     ^
dataSource              dim     ^
oldConsultant           dim     6
outputFileName          dim     ^
pdfData                 dim     ^
readFlag                dim     5
sortFileName            dim     ^
tempFileName            dim     ^
thisMailer              dim     4
.START PATCH 1.1 ADDED LOGIC
.I can get rid of this when ALL files have updated Mailer Numbers!!
thisMailer2             dim     6
thisBroker              dim     6
.END PATCH 1.1 ADDED LOGIC
validLists              dim     260
validMailers            dim     260
yyyy                    dim     4

fInvoice                file
fOutput                 file
sortFile                file

nwk6                    form    6
nwk10                   form    10
readPosition            form    10
readToPosition          form    10

httName                 init    "onlineinvoicing.htt"

i                       integer 4
fileReady               integer 4
.START PATCH 1.3 ADDED LOGIC
JULDAYS2  form      5
.END PATCH 1.3 ADDED LOGIC
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
.START PATCH 1.7 REPLACED LOGIC
.    call CGIParse using "cgicampaignnumber",cgiCampaignNumber
    call CGIParse using "cgiother",cgiother
    call CGIParse using "cgimailerkey",cgimailerkey
.END PATCH 1.7 REPLACED LOGIC
    call CGIParse using "cgiordernumber",cgiOrderNumber
    call CGIParse using "cgifunction",cgiFunction

//    move "C" into thisSession.userType
//    move "C" into sessionIO.userType
//    move "001082" into thisSession.userCompany
//    move "001082" into sessionIO.userCompany
//    move "559254" into cgiordernumber
//
//    move "2" into cgifunction

    // this processes our company information
    // for non admin users
.START PATCH 1.2 REPLACED LOGIC
.    if (thisSession.usertype = "A")
.         // we can skip it if we're an administrator since security doesn't apply
.    else
.        // this user is not an administrator
.        call compOpen
.        packkey compFld from thisSession.userCompany
.        call compKey
.        if over
.            // no results
.            stream *STDOUT,"No company set up in your user account.  Please contact support!"
.            call CGIRenderFooter
.            stop
.        else
.            // Use new aam file to determine
.            // is this a client, broker or consultant?
.            packkey COMPFLD11 from "01X",sessionIO.userCompany
.            call compaim2
.            if not over
.                // this is a consultant
.                move "1" into consultantFlag
.                count i in compOldMlr
.//                if zero
.                if not zero
.//                    move compOldBrk into thisMailer
.//                else
.                    move compOldMlr into thisMailer
.                endif
.                move compOldBrk into oldConsultant
..START PATCH 1.1 ADDED LOGIC
.                move compnum into thisMailer2
..END PATCH 1.1 REPLACED LOGIC
.            else
.                packkey COMPFLD12 from "02X",sessionIO.userCompany
.                call compaim2
.                if not over
.                    // this is a broker
.                    move "1" into brokerFlag
.                    count i in compOldMlr
.                    if zero
.                        move compOldBrk into thisMailer
.                    else
.                        move compOldMlr into thisMailer
.                    endif
..START PATCH 1.1 ADDED LOGIC
.                    move compnum into thisMailer2
..END PATCH 1.1 REPLACED LOGIC
.                else
.                    // this is a client
..START PATCH 1.1 ADDED LOGIC
.                    move compnum into thisMailer2
..END PATCH 1.1 REPLACED LOGIC
.                    move "1" into clientFlag
.                    count i in COMPOLDMLR
.                    if zero
.                        stream *STDOUT,"The company assigned to you in your user account appears to have ":
.                            "the following problem: <br><br>",newline2:
.                            "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
.                            "Please contact the website administrator to correct this problem."
.                        call CGIRenderFooter
.                        stop
.                    endif
.                    move compoldmlr into thisMailer
.                endif
.            endif
.
.            // populate dropdown list for clients and consultants
.            count i in thisMailer
.            if not zero
.                clear validLists
.                // get all the list numbers for this client
.                if (sessionIO.usertype = "C")
..START PATCH 1.1 REPLACED LOGIC
..                    move thisMailer to NXRFFLD2
.                    move thisMailer2 to NXRFFLD2
..END PATCH 1.1 REPLACED LOGIC
.                    move "2" to NXRFPATH
.                    call nxrfKey
.                    loop
..START PATCH 1.1 REPLACED LOGIC
..                        while (nxrfMlr = compOldMlr)
.                        while (nxrfMlr = compnum)
..END PATCH 1.1 REPLACED LOGIC
.                        append nxrflist to validLists
.                        append ";" to validLists
.                        call nxrfks
.                    repeat until over
.                    reset validLists
.                elseif (sessionIO.userType = "O")
.                    clear compOldMlr
.                    clear compfld12
.                    pack compfld11 from "01X",sessionIO.userCompany
.                    call compaim2
.                    loop
.                        until over
.                        // add Mailer to validMailers
.                        if (compoldmlr != "    ")
.                            append compoldmlr to validMailers
.                            append ";" to validMailers
.                        endif
.                        call compkg2
.                    repeat
.                    reset validMailers
.                endif
.            endif
.        endif
.        move compComp to cgiCompanyName
.        chop cgiCompanyName
.    endif
.............................................
.         // this processes our company information
          if (thisSession.userType = "A")
.         // we can skip it if we're an administrator since security doesn't apply
          else
.         // this user is not an administrator
                    call      compOpen
                    packkey   compFld,thisSession.userCompany
                    call      COMPKEY
                    if over
.                             // no results
.START PATCH 1.5 REPLACED LOGIC
.                             stream    *STDOUT,"No company set up in your user account.  Please contact support!"
                              stream    *STDOUT,"No company set up in your user account.  Please contact <a href=#"#/contact.htm#";#"> support</a>!"
.END PATCH 1.5 REPLACED LOGIC
                              call      CGIRenderFooter
                              stop
                    else
.START PATCH 1.2 MOVED LOGIC TO
                              move      compCOMP,cgiCompanyName
.END PATCH 1.2 MOVED LOGIC TO
                              move      COMPNUM,thisMailer2
                              if (thisSession.userType = "O" | thisSession.userType = "B")
.                                       //Reference for future Conversion of NINORD
.                                       move      COMPNUM,thisBroker
                                        move      COMPOLDBRK,thisBroker
                                        if (thisSession.userType = "O")
                                                  move      "1",consultantFlag
                                        elseif (thisSession.userType = "B")
                                                  move      "1",brokerFlag
                                        endif
                              elseif (thisSession.userType = "C")
                                        move      "1",clientFlag
                                        call      Trim using COMPOLDMLR
                                        if (COMPOLDMLR = "")
.START PATCH 1.5 REPLACED LOGIC
.                                                 stream    *STDOUT,"The company assigned to you in your user account appears to have ":
.                                                           "the following problem: <br><br>",newline2:
.                                                           "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
.                                                           "Please contact the website administrator to correct this problem."
                                                  stream    *STDOUT,"The company assigned to you in your user account appears to have ":
                                                            "the following problem: <br><br>",newline2:
                                                            "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
                                                            "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."
.END PATCH 1.5 REPLACED LOGIC
                                                  call      CGIRenderFooter
                                                  stop
                                        endif
                                        move      compoldmlr,thisMailer
                              else
.                             //This should never really happen.  Welcome Page should not even give statusrpt option if not:  Client, Consultant, Broker
.START PATCH 1.5 REPLACED LOGIC
.                                       stream    *STDOUT,"You have not been assigned a Status that allows you ":
.                                                 "to View Orders. <br><br>",newline2:
.                                                 "Please contact the website administrator to correct this problem."
                                        stream    *STDOUT,"You have not been assigned a Status that allows you ":
                                                  "to View Orders. <br><br>",newline2:
                                                  "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a>website administrator to correct this problem."
.END PATCH 1.5 REPLACED LOGIC
                                        call      CGIRenderFooter
                                        stop
                              endif
                              if (thisSession.userType = "C" | thisSession.userType = "O")
.                                       // populate dropdown list for clients and consultants
                                        clear     validLists
.                                       // get all the list numbers for this client
                                        if (sessionIO.usertype = "C")
.                                                move       thisMailer to NXRFFLD2
                                                  move      thisMailer2,NXRFFLD2
                                                  move      "2",NXRFPATH
                                                  call      NXRFKEY
                                                  loop
                                                            until over
                                                            while (NXRFMLR = COMPNUM)
                                                            append    NXRFLIST,validLists
                                                            append    ";",validLists
                                                            call      NXRFKS
                                                  repeat
                                                  reset     validLists
                                        elseif (sessionIO.userType = "O")
                                                  clear     compOldMlr
.START PATCH 1.5 REPLACED LOGIC
.                                                 clear     COMPFLD12
.                                                 pack      COMPFLD11,"01X",sessionIO.userCompany
.                                                 call      COMPAIM2
.                                                 loop
.                                                           until over
.                                                           // add Mailer to validMailers
.                                                           if (compoldmlr != "    ")
.                                                                     append    compoldmlr,validMailers
.                                                                     append    ";",validMailers
.                                                           endif
.                                                           move      COMPNUM,NXRFFLD2
.                                                           move      "2",NXRFPATH
.                                                           call      NXRFKEY
.                                                           loop
.                                                                     until over
.                                                                     while (NXRFMLR = COMPNUM)
.                                                                     append    NXRFLIST,validLists
.                                                                     append    ";",validLists
.                                                                     call      NXRFKS
.                                                           repeat
.                                                           call      COMPKG2
.                                                 repeat
.........................................................
                                                  clear     NCLTFLD2
                                                  pack      NCLTFLD1,"01X",sessionIO.userCompany
                                                  call      NCLTAIM
                                                  loop
                                                            until over
                                                            // add Mailer to validMailers
                                                            pack      COMPFLD,NCLTCLIENT
                                                            call      COMPKEY
                                                            if not over
                                                                      if (compoldmlr != "    ")
                                                                                append    compoldmlr,validMailers
                                                                                append    ";",validMailers
                                                                      endif
                                                                      move      NCLTCLIENT,NXRFFLD2
                                                                      move      "2",NXRFPATH
                                                                      call      NXRFKEY
                                                                      loop
                                                                                until over
                                                                                while (NXRFMLR = NCLTCLIENT)
                                                                                append    NXRFLIST,validLists
                                                                                append    ";",validLists
                                                                                call      NXRFKS
                                                                      repeat
                                                            endif
                                                            call      NCLTKG
                                                  repeat
.END PATCH 1.5 REPLACED LOGIC
                                                  reset     validMailers
                                                  reset     validLists
                                        endif
                              endif
                    endif
          endif
.END PATCH 1.2 REPLACED LOGIC

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
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
.START PATCH 1.5 REPLACED LOGIC
.        stream *stdout,"<span class='errorMsg'>":
.            "Unable to locate user record.  Please contact MIS.":
.            "</span>"
        stream *stdout,"<span class='errorMsg'>":
            "Unable to locate user record.  Please contact <a href=#"#/contact.htm#";#"> support</a>.":
            "</span>"
.END PATCH 1.5 REPLACED LOGIC
        return
    endif

    // Show invoice if flag is set to yes
    if (fUserIO.invoiceFlag = "Y")
        call CGIDisplayFile using "onlineinvoicingsearch.htt"
    else
        call CGIDisplayFile using "onlineinvoicingpending.htt"
//    else
//        call CGIDisplayFile using "onlineinvoicingsignup.htt"
    endif
.START PATCH 1.6 ADDED LOGIC
          call      CGITrack using SID,"0016"
.END PATCH 1.6 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

    debug

    // process the signup form submission
    call fUserOpen
    packkey fUserIKey from thisSession.UserID
    call fUserIRead
    if over
.START PATCH 1.5 REPLACED LOGIC
.        stream *stdout,"<span class='errorMsg'>":
.            "Unable to locate user record.  Please contact MIS.":
.            "</span>"
        stream *stdout,"<span class='errorMsg'>":
            "Unable to locate user record.  Please contact <a href=#"#/contact.htm#";#"> support</a>.":
            "</span>"
.END PATCH 1.5 REPLACED LOGIC
        return
    endif
    // the company name is in compComp, so we move it to a CGI variable
.START PATCH 1.2 MOVED LOGIC FROM
.    move compCOMP to cgiCompanyName
.END PATCH 1.2 MOVED LOGIC FROM
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
.START PATCH 1.6 ADDED LOGIC
          call      CGITrack using SID,"0019"
.END PATCH 1.6 ADDED LOGIC

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
.START PATCH 1.4 ADDED LOGIC
          call      cgiParse using "cgiTotPage",cgiTotPage
          call      cgiParse using "cgiRecNum",cgiRecNum
.END PATCH 1.4 ADDED LOGIC
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

.START PATCH 1.4 ADDED LOGIC
          move      C1,N10
          move      cgiReadPos,N10
          calc      howmany=N10/20
          add       C1,howmany
          move      howmany,cgiPageNum  .Must always assume at least 1 page!!
          call      Trim using cgiPageNum
.END PATCH 1.4 ADDED LOGIC

    if (readPosition < 1)
        move "1" to readPosition
    endif

    // skip over records to get to the proper position in our file.  We use
    //  readPosition-1 because we are already pointing at record one by default.
    for i from "1" to (readPosition-1)
.START PATCH 1.7 REPLACED LOGIC
.        read sortFile,seq;cgiOrderNumber,cgiCampaignNumber,cgiPONumber,cgiMailerDate
        read sortFile,seq;cgiOrderNumber,cgiMailerKey,cgiPONumber,cgiMailerDate
.END PATCH 1.7 REPLACED LOGIC
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
.START PATCH 1.7 REPLACED LOGIC
.        read sortFile,seq;cgiOrderNumber,cgiCampaignNumber,cgiPONumber,cgiMailerDate
        read sortFile,seq;cgiOrderNumber,cgiMailerKey,cgiPONumber,cgiMailerDate
.END PATCH 1.7 REPLACED LOGIC
        until over

        unpack cgiMailerDate into yyyy,mm,dd
        pack cgiMailerDate from mm,"/",dd,"/",yyyy
        if (cgimailerdate = "11/11/1111")
            pack cgimailerdate with "See Special Instructions"
        elseif (cgimailerdate = "00/00/0000")
            pack cgimailerdate with "As Soon As Possible"
        endif

.START PATCH 1.7 REMOVED LOGIC
.        count nwk10 in cgiCampaignNumber
.        if zero
.            move "n/a" to cgiCampaignNumber
.        endif
.        count nwk10 in cgiPoNumber
.        if zero
.            move "n/a" to cgiPoNumber
.        endif
.END PATCH 1.7 REMOVED LOGIC

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

.START PATCH 1.4 ADDED LOGIC
          move      C0,N10
          move      C0,cgiPageNum
          move      C0,cgiTotPage
          move      C0,cgiRecNum
.END PATCH 1.4 ADDED LOGIC

    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    prep tempFile,tempFileName,exclusive
    setmode *openuseip=#fileManagerSettings

    // if we have an order number or a campaign number, we can do an ISAM read
    clear error
.START PATCH 1.2 REPLACED LOGIC
.    move "1" to nOrdPath
.
.    if (cgiOrderNumber != "")
.        // do an ISAM read by order number (there should be only 1 match for this read)
.        move "ISAM1" to readFlag
.        packkey nOrdFld from cgiOrderNumber
.        call nOrdKey
.    elseif (cgiCampaignNumber != "")
.        // do an ISAM read by campaign number (there could be many hits for this read/readks)
.        move "ISAMC" to readFlag
.        move cgiCampaignNumber into nwk6
.        move nwk6 into cgiCampaignNumber
.        replace " 0" in cgiCampaignNumber
.        packkey nOrdFldC from cgiCampaignNumber
.        call nord1e
.    elseif (cgiPONumber != "")
.        // do an AAM read by PO number
.        move "AAM" to readFlag
.        // try to use the mailer key to narrow down our AAM results
.        //  (note that this only applies to NON administrators!)
.        if (thisSession.userType = "B" or thisSession.userType = "C")
.            move COMPOLDMLR to MKEY
.            call NMLRKEY
.            if over
.                stream *STDOUT,"The company assigned to you in your user account appears to have ":
.                    "the following problem: <br><br>",newline2:
.                    "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
.                    "Please contact the website administrator to correct this problem."
.                call CGIRenderFooter
.                noreturn
.                return
.            else
.                pack NORDFLD1 from "01X",mNum
.            endif
.        elseif (thisSession.userType = "O")
.            packkey nordfld1 from "01X",compOldMlr
.        endif
.
.        // pack up optional filters based on the search parameters
.        count i in cgiPONumber
.        if not zero
.            pack nOrdFld3 from "03X",cgiPONumber
.        endif
.        call NORDAIM
.    endif
.
.    loop
.        until over
.        if (readFlag = "AAM")
.            // we need to filter our hits against any order number or campaign number supplied
.            if (cgiOrderNumber != "" and cgiOrderNumber != oLrn)
.                // if we're searching by order number and it does not match
.                match cgiOrderNumber to oLrn with "?"
.                if not equal
.                    call nOrdKG
.                    continue
.                endif
.            endif
.
.            if (cgiCampaignNumber != "" and cgiCampaignNumber != oCamp)
.                // if we're searching by campaign number and it does not match
.                match cgiCampaignNumber to oCamp with "?"
.                if not equal
.                    call nOrdKG
.                    continue
.                endif
.            endif
.
.            // if this is a consultant or a client
.            if (thisSession.userType = "O" or thisSession.userType = "C")
.                if (oMlrNum = thisMailer)
.                    packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                    write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                    call nOrdKG
.                else
.                    scan olnum in validlists
.                    if equal
.                        packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                        write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                        call nOrdKG
.                    endif
.                endif
.            else
.                packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                call nOrdKG
.            endif
.
.        else
.            // we're doing an ISAM read, so make sure that we haven't read past our
.            //  search criteria
.            break if (readFlag = "ISAM1" and oLRN > nOrdFld)
.            break if (readFlag = "ISAMC" and oCamp > nOrdFldC)
.
.            // we need to filter against our mailer number
.            if (thisSession.userType = "A")
.//                if (OMLRNUM = mNum)
.                    // now if and ONLY if we have both a campaign and order number, we need to
.                    //  make sure that both match
.                    if (readFlag = "ISAM1" and (cgiCampaignNumber != "" and cgiCampaignNumber != oCamp))
.                        // do nothing, the record doesn't match
.                    else
.                        packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                        write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                    endif
.//                endif
.            elseif (thisSession.userType = "O" or thisSession.userType = "C")
.                // now if and ONLY if we have both a campaign and order number, we need to
.                //  make sure that both match
.                if (readFlag = "ISAM1" and (cgiCampaignNumber != "" and cgiCampaignNumber != oCamp))
.                    // do nothing, the record doesn't match
.                else
.                    if (oMlrNum = thisMailer)
.                        packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                        write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                    else
.                        scan olnum in validlists
.                        if equal
.                            packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                            write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                        endif
.                    endif
.                endif
.            else
.                if (readFlag = "ISAM1" and (cgiCampaignNumber != "" and cgiCampaignNumber != oCamp))
.                    // do nothing, the record doesn't match
.                else
.                    packkey cgimailerdate from OMDTEC,OMDTEY,OMDTEM,OMDTED
.                    write tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                endif
.            endif
.
.            if (readFlag = "ISAM1")
.                call nOrdKS
.            else
.                call nord3FL
.            endif
.        endif
.    repeat
..................................................
.START PATCH 1.7 REPLACED LOGIC
.         move      C1,NORDPATH         .Default Value
.         if (cgiOrderNumber != "")
..        // do an ISAM read by order number (there should be only 1 match for this read)
.                   move      "ISAM1",readFlag
.                   call      ZFillIt using cgiOrderNumber
.                   packkey   NORDFLD,cgiOrderNumber
.
.         elseif (cgiCampaignNumber != "")
..        // do an ISAM read by campaign number (there could be many hits for this read/readks)
.                   move      "ISAMC",readFlag
.                   call      ZFillIt using cgiCampaignNumber
.                   packkey   NORDFLDC,cgiCampaignNumber
.                   move      C4,NORDPATH         .Change Default!!!
.         elseif (cgiPONumber != "")
..START PATCH 1.4 REPLACED LOGIC
..Make sure there is enough to search on!!
.                   move      cgiPONumber,taskname
.                   call      Trim using taskname
.                   count     howmany,taskname
.                   if (howmany < 3)
.                             // no results
.                             stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You must enter at least 3 characters to search!<br><br></td></tr>",newline2
.                             return
.                   else
.                             move      C0,N1
.                             for result,C1,howmany
.                                       move      taskname,str1
.                                       if (str1 = B1 | str1 = QUESTION)
.                                                 move      C0,N1
.                                       else
.                                                 add       C1,N1
.                                                 if (N1 >= 3)
.                                                           break
.                                                 endif
.                                       endif
.                                       bump      taskname
.                             repeat
.                             reset     taskname
.                             if (N1 < 3)
.                                       stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>To Search you must enter at least 3 sequential non-blank, non '?' characters!<br><br></td></tr>",newline2
.                                       return
.                             endif
.                   endif
...END PATCH 1.4 REPLACED LOGIC
..        // do an AAM read by PO number
.                   move      "AAM",readFlag
..        // try to use the mailer/broker key to narrow down our AAM results
..        //  (note that this only applies to NON administrators!)
.                   if (thisSession.userType = "C")
.                             call      Trim using thisMailer
.                             if (thisMailer <> "")
.                                       pack      NORDFLD1,"01X",thisMailer
.                             endif
..START PATCH 1.5 REPLACED LOGIC
..                  elseif (thisSession.userType = "O" | thisSession.userType = "B")
..                            call      Trim using thisBroker
..                            if (thisBroker <> "")
..                                      packkey   NORDFLD4,"04X",thisBroker
..                            endif
.                   elseif (thisSession.userType = "B")
.                             call      Trim using thisBroker
.                             if (thisBroker <> "")
.                                       packkey   NORDFLD4,"04X",thisBroker
.                             endif
.                   elseif (thisSession.userType = "O")
.                             call      Trim using thisBroker
.                             if (thisBroker <> "")
.                                       if (NCLTTYPE <> "1")
.                                       //NCLTTYPE = "1" implies the Consultant may not have their name on the Orders!!
.                                                 packkey   NORDFLD4,"04X",thisBroker
.                                       endif
.                             endif
..END PATCH 1.5 REPLACED LOGIC
.                   endif
..        // pack up optional filters based on the search parameters
.                   pack      NORDFLD3,"03L",cgiPONumber
.         else
..Should never happen, but just in case
.                   stream *STDOUT,"Please enter an Order, Campaign, or P.O. Number."
.                   call CGIRenderFooter
.                   noreturn
.                   return
.         endif
..START PATCH 1.3 ADDED LOGIC
.         clock     timestamp,timestamp
.         unpack    timestamp,CC,YY,MM,DD
.         call      CVTJUL
.         move      JULDAYS,JULDAYS2
..END PATCH 1.3 ADDED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         if (thisSession.userType = "O")                   .Consultant Mode
.                   //Must refresh Consultant file record
.                   pack      NCLTFLD1,"01X",sessionIO.UserCompany
.                   pack      NCLTFLD2,"02X",COMPNUM
.                   call      NCLTAIM
.                   if not over
.                             loop
.                                       //Save variables from latest read
.                                       pack      taskname,NCLTVARS
.                                       //Position to the last entry - the most recent!
.                                       call      NCLTKG
.                                       until over
.                             repeat
.                             unpack    taskname,NCLTVARS   //Refresh variables!
.                             call      Trim using NCLTSDATE
.                             call      Trim using NCLTEDATE
.                   else
..START PATCH 1.5 REPLACED LOGIC
..                            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You are not currently set up to view Invoices.  Please contact your NIN representative. <br><br></td></tr>",newline2        ."
.                             stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You are not currently set up to view Invoices.  Please contact your <a href=#"#/contact.htm#";#"> NIN representative</a>. <br><br></td></tr>",newline2    ."
..END PATCH 1.5 REPLACED LOGIC
.                             return
.                   endif
..Following is a safety measure!!
.                   call      Trim using NCLTCONSULT        //if empty, then we know we got an over - this is used later on!!
.         endif
..END PATCH 1.5 ADDED LOGIC
..Do actual reads
.         if (readFlag = "AAM")
.                   call      NORDAIM
.         else
.                   call      NORDKEY
.         endif
.         loop
.                   until over
..START PATCH 1.3 ADDED LOGIC
.                   move      OMDTEC,CC
.                   move      OMDTEY,YY
.                   move      OMDTEM,MM
.                   move      OMDTED,DD
.                   call      CVTJUL
.                   if (JULDAYS < JULDAYS2)
.                             sub       JULDAYS,JULDAYS2,N5
..                            //Only display records within past year.  Only search for records within past 2 years.
..                            //This allows finding older records with newer Mail Dates.
.                             if (N5 > 730)
.                                       break
.                             elseif (N5 > 365)
.                                       goto EndofLoop
.                             endif
.                   endif
..END PATCH 1.3 ADDED LOGIC
.                   if (readFlag = "ISAM1")
..This should never actually happen, but what the hay.
.                             if (OLRN <> NORDFLD)
.                                       break
.                             endif
.                   elseif (readFlag = "ISAMC")
.                             if (OCAMP <> NORDFLDC)
.                                       break
.                             endif
.                   endif
.                   call      Trim using OMLRPON
.                   if (cgiOrderNumber <> "" & cgiOrderNumber <> OLRN)
.                             goto EndOfLoop
.                   elseif (cgiCampaignNumber <> "" & cgiCampaignNumber <> OCAMP)
.                             goto EndOfLoop
.                   elseif (cgiPONumber <> "" & cgiPONumber <> OMLRPON)
.                             goto EndOfLoop
.                   endif
.                   if (thisSession.userType = "A")                             .Admin Mode
.                             packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                             write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
..START PATCH 1.4 ADDED LOGIC
.                             add       C1,N10
..END PATCH 1.4 ADDED LOGIC
.                   elseif (thisSession.userType = "O")               .Consultant Mode
.                             reset     validMailers
.                             scan      OMLRNUM,validMailers
.                             if equal
..START PATCH 1.5 REPLACED LOGIC
..                                      if (OBRKNUM = thisBroker)
..                                                packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
..                                                write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
...START PATCH 1.4 ADDED LOGIC
..                                                add       C1,N10
...END PATCH 1.4 ADDED LOGIC
...                                     else
...                                               goto CheckList
..                                      endif
...                           else
...CheckList
...                                     reset     validLists
...                                     scan      OLNUM,validLists
...                                     if equal
...                                               packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
...                                               write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
...                                     endif
..                            endif
..................................................................
.                                       call      Trim using obrknum
.                                       if (obrknum = thisBroker | (obrknum = "" & NCLTTYPE = "1"))
.                                       //NCLTTYPE is used to determine if Consultant can view records where their name is NOT on them
.                                                 if (NCLTCONSULT <> "")        //blank field implies an over!!  Double-Check.
.                                                           if (NCLTSDATE <> "" | NCLTEDATE <> "")
.                                                                     pack      str8,OODTEC,OODTEY,OODTEM,OODTED
.                                                                     call      Trim using str8
.                                                                     if (str8 <> "")
.                                                                               if (NCLTSDATE = "")
.                                                                                         move      "00000000",NCLTSDATE
.                                                                               endif
.                                                                               if (NCLTEDATE = "")
.                                                                                         move      "99999999",NCLTEDATE
.                                                                               endif
.                                                                               if (str8 >= NCLTSDATE & str8 <= NCLTEDATE)
.                                                                                         packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                                                                         write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                                                                                         add       C1,N10
.                                                                               endif
.                                                                     else
.                                                                               packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                                                               write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                                                                               add       C1,N10
.                                                                     endif
.                                                           else
.                                                                     packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                                                     write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
.                                                                     add       C1,N10
.                                                           endif
.                                                 endif
..                                      else
..                                                goto CheckList
.                                       endif
..                            else
..CheckList
..                                      reset     validLists
..                                      scan      OLNUM,validLists
..                                      if equal
..                                                packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
..                                                write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
..                                      endif
.                             endif
..END PATCH 1.5 REPLACED LOGIC
.                   elseif (thisSession.userType = "C")     .Client Mode
.                             if (OMLRNUM = thisMailer)
.                                       packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                       write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
..START PATCH 1.4 ADDED LOGIC
.                                       add       C1,N10
..END PATCH 1.4 ADDED LOGIC
.                             else
.                                       reset     validLists
.                                       scan      OLNUM,validLists
.                                       if equal
.                                                 packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                                 write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
..START PATCH 1.4 ADDED LOGIC
.                                                 add       C1,N10
..END PATCH 1.4 ADDED LOGIC
.                                       endif
.                             endif
.                   elseif (thisSession.userType = "B")     .Broker Mode
.                             if (OBRKNUM = thisBroker)
.                                       packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                       write     tempfile,seq;OLRN,OCAMP,OMLRPON,cgiMailerDate
..START PATCH 1.4 ADDED LOGIC
.                                       add       C1,N10
..END PATCH 1.4 ADDED LOGIC
.                             endif
.                   endif
.EndofLoop
.                   if (readFlag = "AAM")
.                             call      NORDKG
.                   elseif (readFlag = "ISAM1")
.                             break
.                   elseif (readFlag = "ISAMC")
.                             call      NORDKS
.                   endif
.         repeat
..END PATCH 1.2 REPLACED LOGIC
.    close tempFile
.
.    pack cmdString with tempFileName,",",sortFileName," -u,d,25-32"
.    sort cmdString
.    if over
.        // no results
.        if (cgiPONumber != "")
.            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for PO Number: ",cgiPONumber," <br><br></td></tr>",newline2
.            return
.        elseif (cgiOrderNumber != "")
.            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for Order Number: ",cgiOrderNumber," <br><br></td></tr>",newline2
.            return
.        elseif (cgiCampaignNumber != "")
.            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for PO Number: ",cgiCampaignNumber," <br><br></td></tr>",newline2
.            return
.        else
.            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found." <br><br></td></tr>",newline2    ."
.            return
.        endif
.    endif
.........................................
          move      C1,NORDPATH         .Default Value
          if (cgiOrderNumber != "")
.         // do an ISAM read by order number (there should be only 1 match for this read)
                    move      "O",readFlag
                    call      ZFillIt,cgiOrderNumber
                    packkey   NORDFLD,cgiOrderNumber
          elseif (cgiOther != "")
.         // do an ISAM read by PO/Mlr Key (there could be many hits for this read/readks)
.         //First verify data format
                    move      cgiOther,taskname
                    call      Trim using taskname
                    count     howmany,taskname
                    if (howmany < 3)
                              // no results
                              stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You must enter at least 3 characters to search!<br><br></td></tr>"
                              return
                    else
                              move      C0,N1
                              for result,C1,howmany
                                        move      taskname,str1
                                        if (str1 = B1 | str1 = QUESTION)
                                                  move      C0,N1
                                        else
                                                  add       C1,N1
                                                  if (N1 >= 3)
                                                            break
                                                  endif
                                        endif
                                        bump      taskname
                              repeat
                              reset     taskname
                              if (N1 < 3)
                                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>To Search you must enter at least 3 sequential non-blank, non '?' characters!<br><br></td></tr>"
                                        return
                              endif
                    endif
.
                    move      "-",readFlag
                  packkey NORDFLD7 from "06X",cgiOther
                  clear       NORDFLD1
                  clear       NORDFLD2
                  clear       NORDFLD3
                  clear       NORDFLD4
                  clear       NORDFLD5
                  clear       NORDFLD6
                  clear       NORDFLD8
.         // try to use the mailer/broker key to narrow down our AAM results
.         //  (note that this only applies to NON administrators!)
                    if (thisSession.userType = "C")
                              call      Trim using thisMailer
                              if (thisMailer <> "")
                                        pack      NORDFLD1,"01X",thisMailer
                              endif
                    elseif (thisSession.userType = "B")
                              call      Trim using thisBroker
                              if (thisBroker <> "")
                                        packkey   NORDFLD4,"04X",thisBroker
                              endif
                    elseif (thisSession.userType = "O")
                              call      Trim using thisBroker
                              if (thisBroker <> "")
                                        if (NCLTTYPE <> "1")
                                        //NCLTTYPE = "1" implies the Consultant may not have their name on the Orders!!
                                                  packkey   NORDFLD4,"04X",thisBroker
                                        endif
                              endif
                    endif
          else
.Should never happen, but just in case
                    stream *STDOUT,"Please enter an Order or Reference Number."
                    call CGIRenderFooter
                    noreturn
                    return
          endif
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          move      JULDAYS,JULDAYS2
          if (thisSession.userType = "O")                   .Consultant Mode
                    //Must refresh Consultant file record
                    pack      NCLTFLD1,"01X",sessionIO.UserCompany
                    pack      NCLTFLD2,"02X",COMPNUM
                    call      NCLTAIM
                    if not over
                              loop
                                        //Save variables from latest read
                                        pack      taskname,NCLTVARS
                                        //Position to the last entry - the most recent!
                                        call      NCLTKG
                                        until over
                              repeat
                              unpack    taskname,NCLTVARS   //Refresh variables!
                              call      Trim using NCLTSDATE
                              call      Trim using NCLTEDATE
                    else
                              stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You are not currently set up to view Invoices.  Please contact your <a href=#"#/contact.htm#";#"> NIN representative</a>. <br><br></td></tr>",newline2    ."
                              return
                    endif
.Following is a safety measure!!
                    call      Trim using NCLTCONSULT        //if empty, then we know we got an over - this is used later on!!
          endif
.Do actual reads
FirstRead
          if (readFlag = "O")
                    call      NORDKEY
          else
                    call      NORDLAST
          endif
          loop
                    if over
                              if (readFlag = "-")
                                        move      cgiother,str12
                                        uppercase str12
                                        packkey NORDFLD3,"03X",str12
                                        clear     NORDFLD7
                                        move      "+",readFlag
                                        goto FirstRead
                              else
                                        break
                              endif
                    endif
                    move      OMDTEC,CC
                    move      OMDTEY,YY
                    move      OMDTEM,MM
                    move      OMDTED,DD
                    call      CVTJUL
                    if (JULDAYS < JULDAYS2)
                              sub       JULDAYS,JULDAYS2,N5
.                             //Only display records within past year.  Only search for records within past 2 years.
.                             //This allows finding older records with newer Mail Dates.
                              if (N5 > 730)
                                        if (readFlag <> "-")
                                                  break
                                        else
                                                  move      cgiother,str12
                                                  uppercase str12
                                                  packkey NORDFLD3,"03X",str12
                                                  clear     NORDFLD7
                                                  move      "+",readFlag
                                                  goto FirstRead
                                        endif
                              elseif (N5 > 365)
                                        goto EndofLoop
                              endif
                    endif
                    if (readFlag = "O")
.This should never actually happen, but what the hay.
                              if (OLRN <> NORDFLD)
                                        break
                              endif
                    endif
                    if (thisSession.userType = "A")                             .Admin Mode
                              packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                              write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                              add       C1,N10
                    elseif (thisSession.userType = "O")               .Consultant Mode
                              reset     validMailers
                              scan      OMLRNUM,validMailers
                              if equal
                                        call      Trim using obrknum
                                        if (obrknum = thisBroker | (obrknum = "" & NCLTTYPE = "1"))
                                        //NCLTTYPE is used to determine if Consultant can view records where their name is NOT on them
                                                  if (NCLTCONSULT <> "")        //blank field implies an over!!  Double-Check.
                                                            if (NCLTSDATE <> "" | NCLTEDATE <> "")
                                                                      pack      str8,OODTEC,OODTEY,OODTEM,OODTED
                                                                      call      Trim using str8
                                                                      if (str8 <> "")
                                                                                if (NCLTSDATE = "")
                                                                                          move      "00000000",NCLTSDATE
                                                                                endif
                                                                                if (NCLTEDATE = "")
                                                                                          move      "99999999",NCLTEDATE
                                                                                endif
                                                                                if (str8 >= NCLTSDATE & str8 <= NCLTEDATE)
                                                                                          packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                                                                          write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                                                                          add       C1,N10
                                                                                endif
                                                                      else
                                                                                packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                                                                write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                                                                add       C1,N10
                                                                      endif
                                                            else
                                                                      packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                                                      write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                                                      add       C1,N10
                                                            endif
                                                  endif
.                                       else
.                                                 goto CheckList
                                        endif
.                             else
.CheckList
.                                       reset     validLists
.                                       scan      OLNUM,validLists
.                                       if equal
.                                                 packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
.                                                 write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
.                                       endif
                              endif
                    elseif (thisSession.userType = "C")     .Client Mode
                              if (OMLRNUM = thisMailer)
                                        packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                        write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                        add       C1,N10
                              else
                                        reset     validLists
                                        scan      OLNUM,validLists
                                        if equal
                                                  packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                                  write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                                  add       C1,N10
                                        endif
                              endif
                    elseif (thisSession.userType = "B")     .Broker Mode
                              if (OBRKNUM = thisBroker)
                                        packkey   cgimailerdate,OMDTEC,OMDTEY,OMDTEM,OMDTED
                                        write     tempfile,seq;OLRN,OMLRKY,OMLRPON,cgiMailerDate
                                        add       C1,N10
                              endif
                    endif
EndofLoop
                    if (readFlag = "O")
                              break
                    else
                              call      NORDKGP
                    endif
          repeat
    close tempFile

    pack cmdString with tempFileName,",",sortFileName," -u,d,31-38"
    sort cmdString
    if over
        // no results
        if (cgiOrderNumber != "")
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for Order Number: ",cgiOrderNumber," <br><br></td></tr>",newline2
            return
        elseif (cgiOther != "")
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found for Reference Number: ",cgiOther," <br><br></td></tr>",newline2
            return
        else
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found." <br><br></td></tr>",newline2     ."
            return
        endif
    endif
.END PATCH 1.7 REPLACED LOGIC

    move sortFileName to cgiDatasource

.START PATCH 1.4 ADDED LOGIC
          move      N10,str10
          call      FormatNumeric using str10,cgiRecNum
.
.START PATCH 1.5 REPLACED LOGIC
.         calc      howmany=N10/20
.         add       C1,howmany          .Must always assume at least 1 page!!
.         move      howmany,cgiTotPage
.         call      Trim using cgiTotPage
...............................
          move      N10,N92
.
          calc      N92=N92/20
          move      N92,howmany
          sub       howmany,N92
          if (N92 > 0)
                    add       "1",howmany
          endif
          move      howmany,cgiTotPage
          call      Trim using cgiTotPage
.END PATCH 1.5 REPLACED LOGIC
.END PATCH 1.4 ADDED LOGIC

.START PATCH 1.6 ADDED LOGIC
          if (readFlag = "O")
                    call      CGITrack using SID,"0017"
          else
                    call      CGITrack using SID,"0018"
          endif
.END PATCH 1.6 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplayInvoice

    call CGIDisplayFile using "onlineinvoicing.htt"
.START PATCH 1.6 ADDED LOGIC
          call      CGITrack using SID,"0020"
.END PATCH 1.6 ADDED LOGIC
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
    //stream *stdout,"<h1>'testing'</h1>"

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
.    stream *STDOUT,"<br><br>If your invoice does not appear above, or you wish to view it in full screen mode, ":
.        "please click <a href='",filename,"' target='_blank'>here</a></center>"
.    stream *STDOUT,"<center><iframe src='",filename,"' width='100%' height='600'></iframe>"

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
    include ninvio.inc
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DOORDERNUMBER routine

    call CGIRenderOrderNumberInput using "",sessionIO.userCompany,sessionIO.userType

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   statusrpt.pls                                                //
//                                                                           //
//    AUTHOR:   mlyons@adjacency.net                                         //
//                                                                           //
//      DATE:   19 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI template application                                     //
//                                                                           //
// REVISION:    VER01   04 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
//    include nxrfdd.inc
//    include ndatdd.inc
    include ncmpdd.inc
    include norddd.inc
    include NSEL2DD.INC
    include nshpdd.inc
    include nspidd.inc
    include nofrdd.inc
    include nspedd.inc
///////////////////////////////////////////////////////////////////////////////
cgiBasePrice            dim     %10
cgiFunction             dim     %1
cgiOrderNum             dim     %6
cgiCampaignNum          dim     %6
cgiPONum                dim     %12
cgiMailerKey            dim     %12
cgiMailerNum            dim     %4
cgiMailerName           dim     %50
cgiMethod               dim     %50
cgiListNumber           dim     %10
cgilistnum              dim     %6
cgiListName             dim     %50
cgiCampaignName         dim     %50
cgiMailerDate           dim     %12
cgiOffer                dim     %30
cgiOrderDate            dim     %12
cgiOrderName            dim     %260
cgiOrderQty             dim     %36
cgiQuantity             dim     %36
cgiSelect               dim     %30
cgiShipDate             dim     %15
cgiSpecInstructions     dim     %650
cgiStatus               dim     %20
cgiTrackingNum          dim     %30
cgiDataRow              dim     %1
cgiLiveOrder            dim     %1
cgiBilledOrder          dim     %1
cgiCancelOrder          dim     %1
cgiPendOrder            dim     %1
cgiCancelBilledOrder    dim     %1
cgiCancelPendOrder      dim     %1
cgiLcrOrder             dim     %1
cgiCancelLcrOrder       dim     %1
cgiFromDate             dim     %12
cgiToDate               dim     %12
cgiDataSource           dim     %260
cgiLinks                dim     %260
dataSource              dim     260
sortFileName            dim     260
tempFileName            dim     260
selectiveSort           dim     260
cmdString               dim     260
cgiOtherUnknown         dim     %1
cgiReadPos              dim     %10
cwk3                    dim     3
cwk4                    dim     4
cwk10                   dim     10
cwk260                  dim     260
omDteY4                 dim     4
fromTimestamp           dim     8
sp1                     dim     2
sp2                     dim     2
sp3                     dim     2
sp4                     dim     2
sp5                     dim     2
sp6                     dim     2
toTimestamp             dim     8
thisMailer              dim     4
thisTimestamp           dim     8
validLists              dim     260

#fileManagerSettings    dim     260

sortedOutput            varlist cgiOrderName:           //001-260
                                cgilistnum:             //261-266
                                cgimailernum:           //267-270
                                cgimailername:          //271-320
                                cgicampaignnum:         //321-326
                                cgicampaignname:        //327-376
                                cgiponum:               //377-388
                                cgimailerkey:           //389-400
                                cgimailerdate:          //401-412
                                cgistatus               //417-432

buffer              dim     32768
brokerFlag          dim     1
clientFlag          dim     1
cwk1                dim     1
cwk2                dim     2
cwk32768            dim     32768
day                 dim     2
fromdate            dim     8
fromdd              dim     2
frommm              dim     2
fromyy              dim     4
month               dim     2
readKey             dim     12
readMethod          dim     1   // O,C,P,M
todate              dim     8
todd                dim     2
tomm                dim     2
toyy                dim     4
validRange          dim     1
yyyy                dim     4
thisShortMailerNo   dim     4

i                   integer 4

httName             init    "statusrpt.htt"

sortFile            file

baseprice           form    5
nwk4                form    4
nwk6                form    6
nwk10               form    10
readPosition        form    10
readToPosition      form    10
qty                 form    10
securityFlag        form    1
shipqty             form    10
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
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "cgifunction",cgiFunction
    call CGIParse using "cgiordernum",cgiOrderNum
    call CGIParse using "cgicampaignnum",cgiCampaignNum
    call CGIParse using "cgiponum",cgiPONum
    call CGIParse using "cgimailerkey",cgiMailerKey
    call CGIParse using "cgilistnum",cgilistnum
    call CGIParse using "cgilistname",cgiListName
    call CGIParse using "cgicampaignname",cgiCampaignName
    call CGIParse using "cgimailerdate",cgiMailerDate
    call CGIParse using "cgifromdate",cgifromdate
    call CGIParse using "cgitodate",cgitodate
    call CGIParse using "cgistatus",cgiStatus
    call CGIParse using "cgiliveorder",cgiliveOrder
    call CGIParse using "cgibilledorder",cgibilledorder
    call CGIParse using "cgicancelorder",cgicancelorder
    call CGIParse using "cgicancelbilledorder",cgicancelbilledorder
    call CGIParse using "cgipendorder",cgipendorder
    call CGIParse using "cgicancelpendorder",cgicancelpendorder
    call CGIParse using "cgilcrorder",cgilcrorder
    call CGIParse using "cgicancellcrorder",cgicancellcrorder
    call CGIParse using "cgiotherunknown",cgiotherunknown
    call cgiParse using "cgiordername",cgiordername
    call cgiParse using "cgimailername",cgimailername
    call cgiParse using "cgimailernum",cgimailernum
    call cgiParse using "cgidatasource",cgiDatasource
    call cgiParse using "cgimethod",cgiMethod
    call cgiParse using "cgishipdate",cgiShipDate
    call cgiParse using "cgiquantity",cgiQuantity
    call cgiParse using "cgitrackingnum",cgiTrackingNum
    call cgiParse using "cgioffer",cgiOffer
    call cgiParse using "cgiselect",cgiselect
    call cgiParse using "cgiorderqty",cgiOrderqty
    call cgiParse using "cgibaseprice",cgiBasePrice
    call cgiParse using "cgispecinstructions",cgiSpecInstructions
    call cgiParse using "cgiorderdate",cgiOrderDate


    // open all our files in read-only mode
    open compFList,read
    move c1 to compFlag

    load ndatName using ndatPath from ndatNme1,ndatNme2
    open ndatFList,read
    move c1 to nDatFlag

    move nxrfNam1 to nxrfName
    open nxrfFLst,read
    move c1 to nxrfFlag
    move c1 to nxrfFlg2

    move "1" to ncmpPath
    load ncmpName using ncmpPath from ncmpNme1,ncmpNme2
    open ncmpFile,ncmpName,read
    move ncmpPath to ncmpFlag

    move "1" to nordPath
    load nordName using nordPath from nordNme1,nordNme2,nordNme3
    open nordFile,nordName,read
    move nordPath to nordFlag

    open nshpFile,nshpName,read
    move c1 to nshpFlag

    open nspiFile,nspiName,read
    move c1 to nspiFlag

    debug
    // this processes our company information
    if (thisSession.userType = "A")
         // we can skip it if we're an administrator since security doesn't apply
    else
        call compOpen
        packkey compFld from thisSession.userCompany
        call compKey
        if over
            // no results
            stream *STDOUT,"No company set up in your user account.  Please contact support!"
            call CGIRenderFooter
            stop
        else
            debug
            // is this a client or a broker?
            if (compbrkflg = "T")
                // this is a broker
                move "1" into brokerFlag
                count i in compbroker
                if zero
                    count i in compconsult
                    if not zero
                        move compconsult into thisMailer
                    endif
                    move compoldmlr into thisMailer
                else
                    move compbroker into thisMailer
                endif
            elseif (compmlrflg = "T")
                // this is a client
                move "1" into clientFlag
                count i in COMPOLDMLR
                if zero
                    stream *STDOUT,"The company assigned to you in your user account appears to have ":
                        "the following problem: <br><br>",newline:
                        "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline:
                        "Please contact the website administrator to correct this problem."
                    call CGIRenderFooter
                    stop
                endif
                move compoldmlr into thisMailer
            endif

            count i in thisMailer
            if not zero
                // populate dropdown list
                clear validLists
                // get all the list numbers for this company
                move thisMailer to NXRFFLD2
                move "2" to NXRFPATH
                call nxrfKey
                loop
                    while (nxrfMlr = compOldMlr)
                    append nxrflist to validLists
                    append ";" to validLists
                    call nxrfks
                repeat until over
                reset validLists
            endif
        endif
    endif

    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the form results
        call actionProcess
    case "2"
        // display the exchange detail
        call getDetail
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

    // logic to process the data that we've parsed from the CGI environment
    if (cgiDatasource != "")
        // We already have a datasource and we're paging forward or back.  We
        //  don't need to validate anything.
        call CGIDisplayFile using "statusrptresponse.htt"
        return
    endif

    // We are going to have to build up a data file, so we have to validate
    //  the user inputs first.
    clear readKey,readMethod
    if (cgiListNum <> "")
        move cgiListNum into nwk6
        move nwk6 into cgiListnum
        replace " 0" in cgiListnum
        move "L" to readMethod
        move cgiListNum to readKey
    elseif (cgiOrderNum <> "")
        move "O" to readMethod
        move cgiOrderNum to readKey
    elseif (cgiCampaignNum <> "")
        move "C" to readMethod
        move cgiCampaignNum to readKey
    elseif (cgiPONum <> "")
        move "P" to readMethod
        move cgiPONum to readKey
    elseif (cgiMailerKey <> "")
        move "M" to readMethod
        move cgiMailerKey to readKey
    endif

    debug

    // validate our date fields
    count i in cgiFromDate
    if not zero
        if (i != 10)
            move "Please enter a valid 'From' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif

        unpack cgiFromDate into frommm,cwk1,fromdd,cwk1,yyyy
        // validate month
        if (fromMM = "00" or fromMM > "12")
            move "Please enter a valid 'From' date range: month is invalid." into errorMsg
            call actionDisplay
            return
        // months with 31 days
        elseif (fromMM = "1" or fromMM = "3" or fromMM = "5" or fromMM = "7" or fromMM = "8" or fromMM = "10" or fromMM = "12")
            if (fromDD < "01" or fromDD > "31")
                move "Please enter a valid 'From' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        // months with 30 days
        elseif (fromMM = "4" or fromMM = "6" or fromMM = "9" or fromMM = "11")
            if (fromDD < "01" or fromDD > "30")
                move "Please enter a valid 'From' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        // February
        elseif (fromMM = "2")
            if (fromDD < "01" or fromDD > "29")
                move "Please enter a valid 'From' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        endif


        pack fromTimestamp from yyyy,fromMM,fromDD
        type fromTimestamp
        if not equal
            //  invalid character found in time string
            move "Please enter a valid 'From' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif
        move "Y" to validRange
    endif

    count i in cgiToDate
    if not zero
        if (i != 10)
            move "Please enter a valid 'To' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif

        unpack cgiToDate into tomm,cwk1,todd,cwk1,yyyy

        // validate month
        if (toMM = "00" or toMM > "12")
            pack errorMsg with "Please enter a valid 'To' date range: month is invalid."
            call actionDisplay
            return
        // months with 31 days
        elseif (toMM = "01" or toMM = "03" or toMM = "05" or toMM = "07" or toMM = "08" or toMM = "10" or toMM = "12")
            if (toDD < "01" or toDD > "31")
                move "Please enter a valid 'To' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        // months with 30 days
        elseif (toMM = "04" or toMM = "06" or toMM = "09" or toMM = "11")
            if (toDD < "01" or toDD > "30")
                move "Please enter a valid 'To' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        // February
        elseif (toMM = "02")
            if (toDD < "01" or toDD > "29")
                move "Please enter a valid 'To' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
        endif


        pack toTimestamp from yyyy,toMM,toDD
        type toTimestamp
        if not equal
            //  invalid character found in time string
            move "Please enter a valid 'To' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif
        move "Y" to validRange
    endif

    if (validRange = "Y")
        // we have a valid date search, so make sure we have defaults in
        //  our to/from timestamps if they are empty
        count i in fromTimestamp
        if zero
            move "00000000" to fromTimestamp
        endif

        count i in toTimestamp
        if zero
            move "99999999" to toTimestamp
        endif
    endif

    if (readMethod = "")
        // nothing provided
        move "Please enter an order, campaign, purchase order or mailer key. " to errorMsg
        call actionDisplay
        return
    endif

// implement different display files here
    if (readMethod = "L")
        call CGIDisplayFile using "statusrptlistnumberresp.htt"
    elseif (readMethod = "O")
        call CGIDisplayFile using "statusrptordernumberresp.htt"
    elseif (readMethod = "C")
        call CGIDisplayFile using "statusrptcampnumberresp.htt"
    elseif (readMethod = "P")
        call CGIDisplayFile using "statusrptpurchorderresp.htt"
    elseif (readMethod = "M")
        call CGIDisplayFile using "statusrptmlrkeyresp.htt"
    else
        call CGIDisplayFile using "statusrptresponse.htt"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    debug
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
        read sortFile,seq;sortedOutput
        until over
    repeat

    // find out how many records to display on the page
    pack cwk260 from "nin;RECSPERPAGE"
    clock ini,cwk260
    if over
        move "20" to cwk260
    endif

    type cwk260
    if not equal
        move "20" to cwk260
    endif

    move cwk260 to readToPosition

    // read and display the appropriate number of records
    //  based upon the settings in RECSPERPAGE
    for i from "1" to readToPosition
        // read the temp file
        read sortFile,seq;sortedOutput
        until over
        chop cgistatus
        if (cgistatus = "B")
            move "Billed" into cgistatus
        elseif (cgistatus = "0")
            move "Live" into cgistatus
        elseif (cgistatus = "Q")
            move "Billed Cancelled" into cgistatus
        elseif (cgistatus = "X")
            move "Cancelled" into cgistatus
        elseif (cgistatus = "p")
            move "Pending" into cgistatus
        elseif (cgistatus = "x")
            move "Pending Cancelled" into cgistatus
        elseif (cgistatus = "l")
            move "LCR" into cgistatus
        elseif (cgistatus = "z")
            move "LCR Cancelled" into cgistatus
        else
            move "Unknown" into cgistatus
        endif
        // format the mailer date
        unpack cgiMailerDate into yyyy,mm,dd
        pack cgiMailerDate from mm,"/",dd,"/",yyyy

        // display the temp file record according to the template format
        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768

        if (cgiDataRow = "1")
            move "" to cgiDataRow
        else
            move "1" to cgiDataRow
        endif
    repeat

    // decide if we should display a previous page link
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

    // decide if we should display a next page link
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
createDataSource

    debug

    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"

    getmode *prepuseip=#fileManagerSettings
    setmode *prepuseip=""
    prep tempFile,tempFileName,exclusive
    setmode *prepuseip=#fileManagerSettings

    switch readMethod
    case "L"
        packkey nOrdFld2 from "02X",readkey
        call nOrdAIM
    case "O"
        move readKey to nOrdFld
        move "1" to nOrdPath
        call nOrdKey
    case "C"
        move readKey to nOrdFldC
        move "4" to nOrdPath
        call nOrdKey
    case "P"
        uppercase readKey
        packkey nOrdFld3 from "03X",readKey
        call nOrdAIM
    case "M"
        packkey nOrdFld7 from "06X",readKey
        call nOrdAIM
    endswitch

    debug

    if over
        // no results found
        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
        noreturn
        return
    else
        debug
        // loop through the order file
        loop
            // get campaign info
            move oCamp to nCmpFld
            move "1" to nCmpPath
            call nCmpKey
            if over
                move "n/a" to nCmpCName
            endif
            move oMlrNum to nXrfFld2
            move "2" to nXrfPath
            clear nXrfList
            call nXrfKey
                packkey nDatFld from nXrfList
                call nDatKey
                if over
                    // no list information
                    move "n/a" to lstNum
                    move "n/a" to oLstName
                endif

                move oLRN to cgiOrderNum
                move olNum to cgiListNum
                packkey NXRFFLD from lstNum
                call nxrfkey
                debug
                if not over
                    packkey mkey from NXRFMLR
                    call nmlrkey
                    if over
                        // check security
                        if (thisSession.usertype != "A")
                            if (clientFlag = "1")
                                scan oLNum in validLists
                                if not equal
                                    // don't populate cgi variables
                                else
                                    // populate cgivariables here
                                    call populateCGIVariables
                                endif
                            elseif (clientFlag = "1" and brokerFlag = "1")
                                scan oLNum in validLists
                                if not equal
                                    // don't populate cgi variables
                                else
                                    // populate cgivariables here
                                    call populateCGIVariables
                                endif
                            endif
                        else
                            call populateCGIVariables
                        endif
                    else
                        // check security
                        if (thisSession.usertype != "A")
                            if (omlrnum != thisMailer)
                                scan oLNum in validLists
                                if not equal
                                    // don't populate cgi variables
                                    move "1" to securityFlag
                                else
                                    // populate cgivariables here
                                    call populateCGIVAriables
                                endif
                                // don't populate cgi variables
                            else
                                call populateCGIVariables
                            endif
                        else
                            call populateCGIVariables
                        endif
                    endif

                    // check date if we have a valid date range
                    if (validRange = "Y")
                        pack thisTimestamp from omDteY4,omDteM,omDteD
                        if (thisTimestamp >= fromTimestamp and thisTimestamp <= toTimestamp)
                            pack cgiMailerDate from omDteY4,omDteM,omDteD
                            debug
                            // apply status filters here
                            if (cgiStatus = "0" and cgiLiveOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "p" and cgiPendOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "l" and cgiLCROrder != "Y")
                                // do nothing
                            elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
                                // do nothing
                            else
                                // no filter for this status, so write to file
                                write tempFile,seq;sortedOutput
                            endif
                        else
                            // do nothing - we just ignore this record, and fall through
                            //  to the endif, which will take us to a readks/kg
                        endif
                    else
                        pack cgiMailerDate from omDteY4,omDteM,omDteD
                        // apply status filters here
                        debug
                        if (cgiStatus = "0" and cgiLiveOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "p" and cgiPendOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "l" and cgiLCROrder != "Y")
                            // do nothing
                        elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
                            // do nothing
                        else
                            // no filter for this status, so write to file
                            write tempFile,seq;sortedOutput
                        endif
                    endif
//                    write tempFile,seq;sortedOutput
                endif

            switch readMethod
            case "L"
                // nothing to do because AAM will never
                //  give us a non-matching key
                call nOrdKG
                break if over
            case "O"
                call nOrdKS
                break if  over
                break if (readKey != oLRN)
            case "C"
                call nOrdKS
                break if over
                break if (readKey != oCamp)
            case "P"
                // nothing to do because AAM will never
                //  give us a non-matching key
                call nOrdKG
                break if over
            case "M"
                // nothing to do because AAM will never
                //  give us a non-matching key
                call nOrdKG
                break if over
            endswitch
        repeat
    endif

    debug
    close tempFile
    clear selectiveSort

// removed selective sort

//    count i in selectiveSort
//    if zero
        pack cmdString with tempFileName,",",sortFileName," -U,D,401-412"
//    else
//        reset selectiveSort to 2
//        pack cmdString with tempFileName,",",sortFileName," -U,D,405-416,S=""",selectiveSort,""""
//        pack cmdString with tempFileName,",",sortFileName," -U,D,401-412"
//    endif

    sort cmdString
    if over
        // nothing in the file
        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
        noreturn
        return
    else
        if (securityFlag = "1")
            stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
            noreturn
            return
        endif

        clear securityFlag

    endif

    move sortFileName to cgiDatasource

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

    move "An error has occurred: data source not located." to errorMsg
    noreturn
    noreturn
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
getDetail

    debug

    // Get order confirmation information
    set nordpath
    packkey nordfld from cgiordernum
    call nordkey
    if over
        move "No order detail is available at this time." to errorMsg
        call CGIDisplayFile using "statusrptshipdetailerr.htt"
        return
    else
        move oMlrPON into cgiponum
        move oQty into qty
        move qty into cgiorderqty
        move "(Z,ZZZ,ZZZ,ZZ9)" to cgiorderqty
        edit qty into cgiorderqty

        move oPPM into baseprice
        move "(ZZ9.99)" to cgibaseprice
        edit baseprice into cgibaseprice

        move oMlrKy into cgimailerkey
        move oSotCode into cgiselect
        pack cgimailerdate from omdtem,"/",omdted,"/",omdtey
        pack cgiorderdate from oodtem,"/",oodted,"/",oodtey

        // mailer name
        count i in cgimailername
        if zero
            move mcomp into cgimailername
        endif

//        // get Offer Name here
        unpack oodnum into cwk4,cwk3
        packkey nofrfld from omlrnum,cwk3
        call nofrkey
        if over
            move "Not Available" into cgioffer
        else
            move ofDesc into cgioffer
        endif

        // get Select here
        packkey nsel2fld from "1",olrn
        call nsel2key
        if over
            move o2des into cgiselect
            move oppm into cgibaseprice
        else
            move nsel2name into cgiselect
            add nsel2sprice to nsel2price giving nwk10
            move nwk10 into cgibaseprice
        endif

        debug
        // get special instructions here
        packkey nspefld from olrn
        call nspekey
        if over
            move "NONE" into cgispecinstructions
        else
            append desc001 to cgispecinstructions
            append "<br>" to cgispecinstructions
            append desc002 to cgispecinstructions
            reset cgispecinstructions
        endif

        packkey ndatfld from olnum
        call ndatkey
        if not over
            move oLstName into cgilistname
        else
            move "Not Available" into cgilistname
        endif

    endif

    // Get shipping information
    packkey nshpfld from cgiordernum
    call nshpkey
    if over
        call CGIDisplayFile using "statusrptorderdetail.htt"
        return
    elseif (cgiordernum = slrnum)
        move sInfo into cgimethod
        unpack sDate into yyyy,month,day
        pack cgishipdate from month,"/",day,"/",yyyy

        move sQuant into shipqty
        move shipqty into cgiquantity
        move "(Z,ZZZ,ZZZ,ZZ9)" to cgiquantity
        edit shipqty into cgiquantity

        count i in sTrack
        if zero
            move "Not Available" into cgitrackingnum
        else
            move sTrack into cgitrackingnum
        endif
    else
        call CGIDisplayFile using "statusrptorderdetail.htt"
        return
    endif

    call CGIDisplayFile using "statusrptshipdetail.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYDETAIL routine buffer

    move buffer to cwk32768
    call cgiDisplay using cwk32768
    stream *STDOUT,cwk32768
    if (cgiDataRow = "1")
        move "" to cgiDataRow
    else
        move "1" to cgiDataRow
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DOLISTNUMBER routine

    call CGIRenderListNumberInput using "",sessionIO.userCompany,sessionIO.userType

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
populateCGIVariables

    move oStat to cgistatus
    move oLstName to cgiListName
    move oLnum to cgiListNum
    move oCamp to cgiCampaignNum
    move nCmpCName to cgiCampaignName
    move oMlrPon to cgiPONum
    move oMlrNum to cgiMailerNum
    move oMlrKy to cgiMailerKey
    move mcomp to cgiMailerName
    clear cgiOrderName
    append "<a href=#"javascript:shipSub('" to cgiOrderName
    append oLRN to cgiOrderName
    append "','" to cgiOrderName
    append cgiMailerName to cgiOrderName
    append "');#">" to cgiOrderName
    append o1Des to cgiOrderName
    append "</a>" to cgiOrderName
    reset cgiOrderName

    debug

    move omDteY to nwk4
    if  (nwk4 >= 100)
        // do nothing, we assume this is a valid year if there
        //  is anything in the century column
    elseif (nwk4 < 80)
        add "2000" to nwk4
    else
        add "1900" to nwk4
    endif
    move nwk4 to omDteY4

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
//    include nxrfio.inc
//    include ndatio.inc
    include ncmpio.inc
    include nordio.inc
    INCLUDE NSEL2IO.INC
    include nshpio.inc
    include nspiio.inc
    include nofrio.inc
    include nspeio.inc
///////////////////////////////////////////////////////////////////////////////

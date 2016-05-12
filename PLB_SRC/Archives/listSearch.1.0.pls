///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   listSearch.pls                                                //
//                                                                           //
//    AUTHOR:   jbrown@adjacency.net                                         //
//                                                                           //
//      DATE:   17 DEC 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI application                                              //
//                                                                           //
// REVISION:    VER01   17 DEC 2004 JBROWN    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include nseldd.inc
    include ntxtdd.inc
///////////////////////////////////////////////////////////////////////////////
cgiColor            dim     %1
cgiFunction         dim     %1
cgiListname         dim     %75
cgiMinquantity      dim     %10
cgilisttype         dim     %1
cgikeyword          dim     %10
cgiUserID           dim     %36
cgiDataRow          dim     %1
cgiDataSource       dim     %260
cgiLinks            dim     %260
dataSource          dim     260
sortFileName        dim     260
tempFileName        dim     260
cgiReadPos          dim     %10
cgiQuantity         dim     %15
cgiListTypeDesc     dim     %15
cgiListDescription  dim     %500
cwk1                dim     1
cwk10               dim     10
cwk16               dim     16
cwk32768            dim     32768
holdingFileName     dim     260
cgiListNumber       dim     %6
buffer              dim     65535
cwk6                dim     6
cwk260              dim     260
htmlTemplate        dim     65535
cmdString           dim     260
cwk40               dim     40

nwk10               form    10
readPosition        form    10
readToPosition      form    10
quantityToMatch     form    10
listQty             form    10
displayedRecord     form    1

sortFile            file

holdingFile         ifile

ETCRLF              init    0x7f

i                   integer 4

httName             init    "listsearch.htt"

sortedOutput        varlist cgiListNumber:
                            cgiListName:
                            cgiMinQuantity:
                            cgiListType:
                            cgiListDescription

#fileManagerSettings    dim     260

datacardFile            file

missingFlag             integer 4
.///////////////////////////////////////////////////////////////////////////////
start

.    // Set up our CGI environment
    call CGIInit
    call CGISendHeaders

.    // if the session isn't valid for some reason, this will send us to an
.    //  error page -  if this page requires admin access, change the call
.    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

.    // this will send the static top part of the HTML document
    call CGIRenderHeader

    call CGIParse using "cgifunction",cgiFunction
    call CGIParse using "cgilistname",cgilistname
    call CGIParse using "cgiMinquantity",cgiMinquantity
    call CGIParse using "cgilisttype",cgilisttype
    call CGIParse using "cgikeyword",cgikeyword
    call CGIParse using "SID",SID

    chop cgiMinQuantity
    type cgiMinQuantity
    if equal
        move cgiMinQuantity to quantityToMatch
    else
        move "0" to quantityToMatch
    endif

.    // based upon the "function" value, we need to decide whether to render the form or
.    //  process it's data
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

    // this will display the (HTT) document we've created for this
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
        call CGIDisplayFile using "listsearchresults.htt"
        return
    endif

    call CGIDisplayFile using "listsearchresults.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
displayResults routine htmlTemplate

    // this is where we display the results - it gets called when the
    //  template gets processed.  Everything between <%DISPLAYRESULTS%> and
    //  <%/DISPLAYRESULTS%> is passed into htmlTemplate and program control
    //  passes here.
    move htmlTemplate to buffer
    call cgiParse using "cgidatasource",cgiDatasource
    if over
        call createDataSource
    else
        move cgiDataSource to sortFileName
    endif

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    trap noDataSource if io
        open sortFile,sortFileName,exclusive
    trapclr io
    setmode *openuseip=#fileManagerSettings

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

.    // read and display the appropriate number of records
.    //  based upon the settings in RECSPERPAGE
    clear displayedRecord
    for i from "1" to readToPosition
        // read the temp file
        read sortFile,seq;sortedOutput
        until over

        switch cgiListType
        case "1"
            move "Rental/Exchange" to cgiListTypeDesc
        case "2"
            move "Exchange" to cgiListTypeDesc
        case "3"
            move "Rental" to cgiListTypeDesc
        default
            move "Rental/Exchange" to cgiListTypeDesc
        endswitch

        scan ETCRLF in cgiListDescription
        if equal
            bump cgiListDescription by -1
            lenset cgiListDescription
            append "...<br><div align=right><a target='_blank' href='/datacards/data" to cgiListDescription
            append cgiListNumber to cgiListDescription
            append ".htm'>more...</a></div>" to cgiListDescription
            reset cgiListDescription
        endif

        move cgiMinQuantity to nwk10
        move "(Z,ZZZ,ZZZ,ZZ9)" to cgiQuantity
        edit nwk10 into cgiQuantity

.        // display the temp file record according to the template format
        set displayedRecord
        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768

        if (cgiDataRow = "1")
            move "" to cgiDataRow
        else
            move "1" to cgiDataRow
        endif
    repeat

.    // decide if we should display a previous page link
    clear cgiLinks
    if (readPosition > 1)
.        // do a previous link
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

    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"

    unpack dataSource into cwk40
    unpack dataSource into cwk16
    pack holdingFileName from cwk16
    prep tempFile,tempFileName,exclusive
    prep holdingFile,holdingFileName,holdingfileName,"6","582",exclusive

    // if we have a list name, then we can do an AAM read for all matching lists
    //  in NINDAT
    if (cgiListName != "")
        // we have a list name, so use that as our AAM key read
        pack NDATFLD2 from "02F",cgiListName
        call NDATAIM
        if not over
            loop
                pack NSELFLD1 from "01X",lstNum
                pack NSELFLD2 from "02XBASE"
                call NSELAIM
                if over
                    move "1" to NSELEXC
                endif
                pack NTXTFLD1 from "01X",lstNum
                call NTXTAIM
                call filterResults
                if equal
                    write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
                endif
                call NDATKG
            repeat until over
        endif
    endif

    // if we have a keyword then we can do an AAM read for all matching list
    //  descriptions (NINTXT)
    if (cgiKeyword != "")
        // we have a list name, so use that as our AAM key read
        clear NTXTFLD1
        clear NSELFLD1
        clear NSELFLD2
        clear NDATFLD1
        clear NDATFLD2
        pack NTXTFLD2 from "02F",cgiKeyword
        call NTXTAIM
        if not over
            loop
                pack NSELFLD1 from "01X",NTXTLIST
                pack NSELFLD2 from "02XBASE"
                call NSELAIM
                if over
                    move "1" to NSELEXC
                endif
                pack NDATFLD2 from "01X",NTXTLIST
                call NDATAIM
                call filterResults
                if equal
                    write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
                endif
                call NTXTKG
            repeat until over
        endif
    endif

    pack cwk6 from ""
    read holdingFile,cwk6;cwk1
    loop
        readks holdingFile;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
        until over
        write tempFile,seq;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
    repeat
    close tempFile
    close holdingFile
    pack cmdString with tempFileName,",",sortFileName," -U,7-81"
    sort cmdString
    if over
        // no results
        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
        return
    endif

    pack cgiDataSource from sortFileName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

    move "An error has occurred: data source not located." to errorMsg
    stream *stdout,"<hr>",errorMsg:
        "<hr>SortFileName: ",sortFileName:
        "<hr>Error Code: ",s$error$

    noreturn
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
filterResults

    // check to see if the list is already in our result list
    read holdingFile,LSTNUM;cwk1
    if not over
        // list is already in the result list.  Set NOT EQUAL so that we know
        //  not to write it again
        setflag not equal
        return
    endif

    // check to see if the record passes our quantity filter
    squeeze universe into cwk10
    type cwk10
    if equal
        move cwk10 to listqty
    else
        move "0" to listqty
    endif
    if (listQty < quantityToMatch)
        // there aren't enough names on the list to match our search
        //  criteria
        setflag not equal
        return
    endif

    // check to see if there is a datacard for the list
    pack filename from "datacards/data",lstNum,".htm"
    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    clear missingFlag
    trap noDatacard if io
    open datacardFile,filename,read
    close datacardFile
    trapclr io
    setmode *openuseip=#fileManagerSettings

    // if missingFlag is set then we don't have a datacard for the given list
    //  number
    if (missingFlag)
// 22 FEB 2005 BJACKSON --> commented out the following two lines for testing so
//  that we see all matches for a search, regardless of whether we have a datacard
//  or not (TEMPORARY FOR TESTING ONLY!)
//        setflag not equal
//        return
//
// 22 FEB 2005 BJACKSON --> end of changes
//
    endif

    setflag equal
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatacard

    set missingFlag
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
    include nselio.inc
    include ntxtio.inc
///////////////////////////////////////////////////////////////////////////////
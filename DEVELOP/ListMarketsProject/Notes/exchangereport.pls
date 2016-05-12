///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   exchangereport.pls                                           //
//                                                                           //
//    AUTHOR:   mlyons@adjacency.net                                         //
//                                                                           //
//      DATE:   13 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI template application                                     //
//                                                                           //
// REVISION:    VER01   04 NOV 2004 MLYONS      Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include nxngdd.inc
    include nxchdd.inc
    include nsel2dd.inc
    include nsmpdd.inc
///////////////////////////////////////////////////////////////////////////////
cgiexchangeto       dim     %15
cgiexchangefrom     dim     %15
cgiexchangestatus   dim     %30
cgibalance          dim     %36
cgibalanceto        dim     %15
cgibalancedue       dim     %36
cgiDataRow          dim     %1
cgiDataSource       dim     %260
cgidate             dim     %12
cgiexchangename     dim     %50
cgiFunction         dim     %1
cgimailer           dim     %6
cginame             dim     %50
cgilistnum          dim     %6
cginxchkey          dim     %13
cgisampledescrip    dim     %30
cgisamplelink       dim     %50
cgisamplenumber     dim     %3
cgitradeco          dim     %260
cgiReadPos          dim     %10
cgiLinks            dim     %1024

buffer              dim     32768
cmdString           dim     260
compNo              dim     4
cwk1                dim     1
cwk2                dim     2
cwk4                dim     4
cwk4a               dim     4
cwk5                dim     5
cwk10               dim     10
cwk15               dim     15
cwk45               dim     45
cwk260              dim     260
cwk32768            dim     32768
dataSource          dim     60
destTiff            dim     260
farMailer           dim     4
mailers             dim     20
mailer1             dim     4
mailer2             dim     4
nearMailer          dim     4
owner               dim     6
sortFileName        dim     260
sourceTiff          dim     260
tempFileName        dim     260
tiffData            dim     32768
yyyy                dim     4

fTIFFIn             file
fTIFFOut            file
sortFile            file

balance             form    10
nwk4                form    4
nwk6                form    6
nwk10               form    10
nwk15               form    15
readPosition        form    10
readToPosition      form    10

counter             integer 4
i                   integer 4
tiffFound              integer 4
readPos             integer 4

httName             init    "exchangereport.htt"

#fileManagerSettings dim 260
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
    // insert all of your CGI parse statements here.  If any variable is
    //  supposed to filled out but you get an over, empty string, etc.,
    //  append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    call CGIParse using "cgifunction",cgiFunction
    call CGIParse using "cgilistnum",cgilistnum
    call CGIParse using "cginame",cginame
    call CGIParse using "cgitradeco",cgitradeco
    call CGIParse using "cgiexchangeto",cgiexchangeto
    call CGIParse using "cgiexchangefrom",cgiexchangefrom
    call CGIParse using "cgibalanceto",cgibalanceto
    call CGIParse using "cgibalancedue",cgibalancedue
    call CGIParse using "cginxchkey",cginxchkey
    call CGIParse using "cgimailer",cgimailer
    call CGIParse using "cgidate",cgidate
    call CGIParse using "cgiexchangename",cgiexchangename
    call CGIParse using "cgibalance",cgibalance
    call CGIParse using "cgiexchangestatus",cgiexchangestatus
    call CGIParse using "cgisamplenumber",cgisamplenumber
    call CGIParse using "cgisampledescrip",cgisampledescrip
    call cgiParse using "cgidatasource",cgiDatasource
    call cgiParse using "cgireadpos",cgiReadPos

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

    // TO DO: add logic to process the data that we've parsed from the
    //  CGI process

    // validation
 debug
    move cgilistnum into nwk6
    move nwk6 into cgilistnum
    replace " 0" in cgilistnum
    count i in cgilistnum
    if (i > 6 or i < 1)
        move "Please enter a valid list number." to ErrorMsg
        call actionDisplay
        return
    endif

    // read for file name
    clear datVars
    packkey ndatfld with cgilistnum

    // open file
    call #ndatopen
    call ndatkey
    move lstnum into cgilistnum
    move olstname into cginame

    call CGIDisplayFile using "exchangereportresp.htt"

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

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
        read sortFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
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
        read sortFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
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
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
createDataSource

    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"
    prep tempFile,tempFileName,exclusive

    packkey compfld from ownnum

    // open file
    call #compopen
    call compkey
    if over
            // read company file for tradecompany
            // there are not results
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1>No results found<br><br></td></tr>",newline
            return
        else
        packkey nxrffld from lstnum

        call #nxrfOpen
        call nxrfkey
        if over
            // there are not results
            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
            return
        else
            packkey nxngfld1 from "01X",nxrfmlr
            move nxrfmlr into cgimailer
            // read aam file to get all instance of mlr1
            call #nxngOpen
            call NXNGAIM
            loop
                while (lstnum = cgilistnum)
                if (flag != "I")
                    move entry into cwk5
                    replace " 0" in cwk5
                    packkey nxchfld1 with acckey,cwk5
                    move nxchfld1 to cginxchkey
                    call #nxchOpen
                    call nxchkey
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    endif
                    sub usage2 from usage1 giving balance
                    move usage1 to cgiExchangeTo
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeTo
                    edit usage1 into cgiExchangeTo
                    move usage2 to cgiExchangeFrom
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeFrom
                    edit usage2 into cgiExchangeFrom
                    move balance into cgibalancedue
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalancedue
                    edit balance into cgibalancedue
                    move cgibalancedue into cwk15
                    if (balance < 0)
                        pack cgibalancedue with "<font color=red>",cwk15,"</font>"
                    endif
                    // take the exchange mailer # and read the
                    //  mailer file to extract a company
                    unpack acckey into nearMailer,farMailer
                    packkey compFld3 from nearMailer
// 19 JAN 2005 MLYONS -->  Using wrong routine to read for records for users company
//                    call compkey
                    call compkey3
// 19 JAN 2005 MLYONS -->  end of changes
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    elseif (thisSession.userType != "A")
                        if (thisSession.userCompany != compnum)
                            call NXNGKG
                            break if over
                            continue
                        endif
                    endif

                    packkey mkey from farMailer
                    call NMLRKEY
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    endif

                    move mcomp into cwk45

                    // the apostrophe cause problems in javascript with errors
                    // so we remove them
                    Loop
                        scan "'" with cwk45
                        until not equal
                        splice "\þ",cwk45 with 1
                    repeat
                    reset cwk45
                    replace "þ'" in cwk45
                    chop cwk45
                    clear cgitradeco
                    append "<a href=#"javascript:detailSub('" to cgitradeco
                    append cginxchkey to cgitradeco
                    append "','" to cgitradeco
                    append farmailer to cgitradeco
                    append "','" to cgitradeco
                    append cwk45 to cgitradeco
                    append "');#">" to cgitradeco
                    chop mcomp
                    append mcomp to cgitradeco
                    append "</a>" to cgitradeco
                    loop
                        read tempFile,seq;cwk1
                        until over
                    repeat

                    write tempFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
                else
                    // the exchange is inactive - don't do anything
                endif

                call NXNGKG
                until over
            repeat

            clear nxngfld1
            clear cgibalanceto
            packkey nxngfld2 from "02X",nxrfmlr
            // read aam file to get all instance of mlr1
            call NXNGAIM
            loop
                while (lstnum = cgilistnum)
                if (flag != "I")
                    move entry into cwk5
                    replace " 0" in cwk5
                    packkey nxchfld1 with acckey,cwk5
                    move nxchfld1 to cginxchkey
                    call nxchkey
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    endif
// 18 JAN 2005 MLYONS --> Displaying the balances in reverse
//                    sub usage2 from usage1 giving balance
//                    move usage1 to cgiExchangeTo
                    sub usage1 from usage2 giving balance
                    move usage2 to cgiExchangeTo
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeTo
//                    edit usage1 into cgiExchangeTo
                    edit usage2 into cgiExchangeTo
//                    move usage2 to cgiExchangeFrom
                    move usage1 to cgiExchangeFrom
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeFrom
//                    edit usage2 into cgiExchangeFrom
                    edit usage1 into cgiExchangeFrom
                    move balance to cgibalancedue
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalancedue
                    edit balance into cgibalancedue
                    move cgibalancedue into cwk15
                    if (balance < 0)
                        pack cgibalancedue with "<font color=red>",cwk15,"</font>"
                    endif
// 18 JAN 2005 MLYONS --> end of changes
                    // take the exhchange mailer # and read the
                    //  mailer file to extract a company
                    unpack acckey into farMailer,nearMailer
                    packkey compFld3 from nearMailer
// 19 JAN 2005 MLYONS -->  Using wrong routine to read for users company
//                    call compkey
                    call compkey3
// 19 JAN 2005 MLYONS -->  end of changes
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    elseif (thisSession.userType != "A")
                        if (thisSession.userCompany != compnum)
                            call NXNGKG
                            break if over
                            continue
                        endif
                    endif
                    packkey mkey from farMailer
                    call NMLRKEY
                    if over
                        // there are not results
                        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline
                        return
                    endif
                    move mcomp into cwk45
                    // the apostrophe cause problems in javascript with errors
                    // so we remove them
                    Loop
                        scan "'" with cwk45
                        until not equal
                        splice "\þ",cwk45 with 1
                    repeat
                    reset cwk45
                    replace "þ'" in cwk45
                    chop cwk45
                    clear cgitradeco
                    append "<a href=#"javascript:detailSub('" to cgitradeco
                    append cginxchkey to cgitradeco
                    append "','" to cgitradeco
                    append farmailer to cgitradeco
                    append "','" to cgitradeco
                    append cwk45 to cgitradeco
                    append "');#">" to cgitradeco
                    chop mcomp
                    append mcomp to cgitradeco
                    append "</a>" to cgitradeco
                    loop
                        read tempFile,seq;cwk1
                        until over
                    repeat
                    write tempFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
                else
                    // the exchange is inactive - don't do anything
                endif
                call NXNGKG
                until over
            repeat
        endif
    endif

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
getDetail

    packkey nxchfld1 from cginxchkey
    call nxchkey
    if over
        move "No exchange details are available at this time." to errorMsg
        return
    else
        if (stat = "R")
            move "Rental" into cgiexchangestatus
        elseif (stat = "C")
            move "Cancelled Adjusted" into cgiexchangestatus
        elseif (stat = "X")
            move "Cancelled Not Adjusted" into cgiexchangestatus
        else
            move "Unavailable" into cgiexchangestatus
        endif

        unpack dat into yyyy,mm,dd
        pack cgidate from mm,"/",dd,"/",yyyy

        sub usage2 from usage1 giving balance
        move balance to cgibalance
        move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalance
        edit balance into cgibalance
        move cgibalance into cwk15
        if (balance < 0)
            pack cgibalance with "<font color=red>",cwk15,"</font>"
        endif

        call CGIDisplayFile using "exchangedetail.htt"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYDETAIL routine buffer

    // ensure that our mailer number is a 6-byte, zero padded field
    move cgimailer to nwk4
    move nwk4 to cgimailer
    replace " 0" in cgiMailer

    pack compFld3 from cgiMailer
    call compKey3
    if over
        move "" to compNum
    endif

    packkey nsmpfld from compNum,""
    call nsmpkey
    loop
        call nsmpks
        until over
        while (compNum = nsmpmlr)
        add "1" to counter

        // write the tiff file out as a sample document in the TIFF directory
        pack sourceTiff from "s",nsmpmlr,nsmpNum,".tif"
        set tiffFound
        trap tiffNotFound if IO
            open fTIFFIn,sourceTiff,read
        trapclr IO

        if (tiffFound)
            getmode *prepuseip=#fileManagerSettings
            setmode *prepuseip=""
            pack destTiff from "..\data\tif\",sourceTiff
            erase destTiff
            prep fTIFFOut,destTiff,exclusive
            setmode *prepuseip=#fileManagerSettings
            loop
                clear tiffData
                read fTIFFIn,seq;*ABSON,tiffData;
                until over
                while (tiffData != "")
                write fTIFFOut,seq;*ABSON,tiffData;
            repeat
            close fTIFFIn
            close fTIFFOut

            pack cgisamplelink from "<a href='/tif/",sourceTiff,"'>View Sample</a>"
        else
            pack cgiSampleLink from "not found (",sourceTiff,")"
        endif
        move nsmpNum into cgisamplenumber
        move nsmpDes1 to cgisampledescrip

        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768
        if (cgiDataRow = "1")
            move "" to cgiDataRow
        else
            move "1" to cgiDataRow
        endif
    repeat

    if (counter = 0)
        stream *STDOUT,"<tr><td colspan='4' align='center' class=datarow1><br>No results found.<br><br</td></tr><br>"
    endif

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
tiffNotFound

    clear tiffFound
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
    include nxngio.inc
    include nxchio.inc
    include nsel2io.inc
    include nsmpio.inc
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#ndatOpen

    load ndatName using ndatPath from ndatNme1,ndatNme2
    open ndatFList,read
    move c1 to nDatFlag
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#compOpen

    open compFList,read
    move c1 to compFlag
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#nxrfOpen

    move nxrfNam1 to nxrfName
    open nxrfFLst,read
    move c1 to nxrfFlag
    move c1 to nxrfFlg2
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#nxngOpen

    open nxngFile,nxngName,read
    move c1 to nxngFlag
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#nxchOpen
    load nxchName FROM nxchPath OF nxchNme1,nxchNme2
    open nxchFile,nxchName,read
    move c1 to nxchFlg1
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DOLISTNUMBER routine

    call CGIRenderListNumberInput using "",sessionIO.userCompany,sessionIO.userType

    return
///////////////////////////////////////////////////////////////////////////////

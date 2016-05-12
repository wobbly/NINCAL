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
//                                                                           //
.this does not appear to be the current version :-(
//      Release   2.2       25JAN2013 DLH       Cleaned up data size mismatch//
//      Release   2.0       29APR2006 ASH       Added Exclusivity Indicator  //
//      Release   1.9       17MAR2006 ASH       Added Sample Inactive filter //
//      Release   1.8       29DEC2005 ASH       Added Tracking Logic         //
//      Release   1.7       27DEC2005 ASH       Added No Web Display Option  //
//      Release   1.6       14DEC2005 ASH       Modified TotPage logic       //
// patch 1.5   22NOV2005  ASH Added language for search results              //
// patch 1.4   18NOV2005  ASH Changed search page                            //
// patch 1.3   11OCT2005  ASH Added patch to create Sample tags              //
// patch 1.2   27SEP2005  ASH Added patch to create HTML files on the fly    //
// patch 1.1   15SEP2005  ASH Small fixes to shore up Search criteria        //
// REVISION:    VER01   17 DEC 2004 JBROWN    Created                        //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include nseldd.inc
    include ntxtdd.inc
.START PATCH 1.3 ADDED LOGIC
          include   nsmpdd.inc
.END PATCH 1.3 ADDED LOGIC

.START PATCH 1.2 ADDED LOGIC
.EXTERNAL ROUTINES FROM       NDAT002W.PLC
CreateWebCardW external "NDAT002W;CreateWebCardW"
.END PATCH 1.2 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
cgiColor            dim     %1
cgiFunction         dim     %1
.START PATCH 2.0 REPLACED LOGIC
.cgiListname         dim     %75
.must match written variable Mlstname2
cgiListname         dim     %85
.cgiListname         dim     %100
MLSTNAME2     dim       85
.END PATCH 2.0 REPLACED LOGIC
cgiMinquantity      dim     %10
cgilisttype         dim     %1
.START PATCH 1.5 ADDED LOGIC
cgiPageNum          dim     %10
cgiTotPage          dim     %10
cgiRecNum           dim     %13
.START PATCH 1.6 ADDED LOGIC
N92                 form    9.2
.END PATCH 1.6 ADDED LOGIC
.END PATCH 1.5 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.cgikeyword          dim     %10
cgikeyword          dim     %75
cgisearchtype       dim     %50
.END PATCH 1.4 REPLACED LOGIC
cgiUserID           dim     %36
cgiDataRow          dim     %1
cgiDataSource       dim     %260
cgiLinks            dim     %1024
dataSource          dim     260
sortFileName        dim     260
tempFileName        dim     260
cgiReadPos          dim     %10
cgiQuantity         dim     %15
cgiListTypeDesc     dim     %15
cgiListDescription  dim     %500
.START PATCH 1.3 ADDED LOGIC
cgiListSample       dim     %100
cgiMailer           dim     %6
cgiMlrName          dim     %50
cgisampledescrip    dim     %30
cgisamplelink       dim     %50
cgisamplenumber     dim     %3
cgisampledate       dim     %10
destTiff            dim     260
sourceTiff          dim     260
tiffData            dim     32768
fTIFFIn             file
fTIFFOut            file
tiffFound           integer 4
str100                  dim     100
.END PATCH 1.3 ADDED LOGIC
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

emptyFile           integer 4
i                   integer 4

httName             init    "listsearch.htt"

.START PATCH 1.3 REPLACED LOGIC
.sortedOutput        varlist cgiListNumber:
.                            cgiListName:
.                            cgiMinQuantity:
.                            cgiListType:
.                            cgiListDescription
sortedOutput        varlist cgiListNumber:
                            cgiListName:
                            cgiMinQuantity:
                            cgiListType:
                            cgiListSample:
                            cgiListDescription
.END PATCH 1.3 REPLACED LOGIC

#fileManagerSettings    dim     260

datacardFile            file

missingFlag             integer 4
.START PATCH 1.4 ADDED LOGIC
TrapTest  form      1
.END PATCH 1.4 ADDED LOGIC
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

    call CGIParse using "cgifunction",cgiFunction
    call CGIParse using "cgilistname",cgilistname
    call CGIParse using "cgiMinquantity",cgiMinquantity
.START PATCH 1.4 REPLACED LOGIC
    call CGIParse using "cgilisttype",cgilisttype
.END PATCH 1.4 REPLACED LOGIC
    call CGIParse using "cgikeyword",cgikeyword
.START PATCH 1.3 ADDED LOGIC
    call CGIParse using "cgimailer",cgimailer
    call CGIParse using "cgimlrname",cgimlrname
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
    call CGIParse using "cgisearchtype",cgisearchtype
.END PATCH 1.4 ADDED LOGIC
    call CGIParse using "SID",SID

    chop cgiMinQuantity
    type cgiMinQuantity
    if equal
        move cgiMinQuantity to quantityToMatch
    else
        move "0" to quantityToMatch
    endif

    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
    switch cgiFunction
    case "0"
        // display a form
        call actionDisplay
    case "1"
        // process the form results
        call actionProcess
.START PATCH 1.3 ADDED LOGIC
    case "2"
        // retrieve Samples
        call actionSamples
.END PATCH 1.3 ADDED LOGIC
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
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0021"
.END PATCH 1.8 ADDED LOGIC
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


    count i in cgilistname
    if not zero
        call CGIDisplayFile using "listsearchresultslistname.htt"
.START PATCH 1.5 REMOVED LOGIC
.One form to rule them all, one form to bind them.
.One form to rule them all, and with the search engine find them.
.    else
.        count i in cgikeyword
.        if not zero
.            call CGIDisplayFile using "listsearchresultskeyword.htt"
.        endif
.END PATCH 1.5 REMOVED LOGIC
    endif

//    call CGIDisplayFile using "listsearchresults.htt"

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
.START PATCH 1.5 ADDED LOGIC
          call      cgiParse using "cgiTotPage",cgiTotPage
          call      cgiParse using "cgiRecNum",cgiRecNum
.END PATCH 1.5 ADDED LOGIC
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

.START PATCH 1.5 ADDED LOGIC
          move      C1,N10
          move      cgiReadPos,N10
          calc      howmany=N10/20
          add       C1,howmany
          move      howmany,cgiPageNum  .Must always assume at least 1 page!!
          call      Trim using cgiPageNum
.END PATCH 1.5 ADDED LOGIC

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
        // display the temp file record according to the template format
        set displayedRecord
        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768
.START PATCH 1.2 ADDED LOGIC
.Check to see if there is a datacard for the list
          pack       filename from "E:\NINSERV\NamesintheNews\wordpress\datacards\data",cgiListNumber,".htm"
.          pack      filename from "datacards/data",cgiListNumber,".htm"
          getmode   *openuseip=#fileManagerSettings
          setmode   *openuseip=""
          clear     missingFlag
          trap      noDatacard if io
          open      datacardFile,filename,read
          close     datacardFile
          trapclr   io
          if (missingflag)
.Create Datacard on the fly
                    call      CreateWebCardW using cgiListNumber
          endif
          setmode   *openuseip=#fileManagerSettings
.END PATCH 1.2 ADDED LOGIC
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

.START PATCH 1.4 ADDED LOGIC
I10Error
          noreturn  .Pull implied return to trap location off of the call stack
          add       C1,TrapTest
          //Increment one second to try and find a unique timestamp
          pause     "1"
          goto createDataSource
.END PATCH 1.4 ADDED LOGIC

///////////////////////////////////////////////////////////////////////////////
createDataSource

.START PATCH 1.5 ADDED LOGIC
          move      C0,cgiPageNum
          move      C0,cgiTotPage
          move      C0,cgiRecNum
.Make sure there is enough to search on!!
          move      cgilistname,taskname
          call      Trim using taskname
          count     howmany,taskname
          if (howmany < 3)
                    // no results
                    stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>You must enter at least 3 characters to search!<br><br></td></tr>",newline2
                    noreturn
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
                              stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>To Search you must enter at least 3 sequential non-blank, non '?' characters!<br><br></td></tr>",newline2
                              noreturn
                              return
                    endif
          endif
.END PATCH 1.5 ADDED LOGIC
    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"

    unpack dataSource into cwk40
    unpack dataSource into cwk16
    pack holdingFileName from cwk16
.START PATCH 1.4 REPLACED LOGIC
.    prep tempFile,tempFileName,exclusive
..START PATCH 1.3 REPLACED LOGIC
..    prep holdingFile,holdingFileName,holdingfileName,"6","582",exclusive
.    prep holdingFile,holdingFileName,holdingfileName,"6","682",exclusive
..END PATCH 1.3 REPLACED LOGIC
.............................................
.Logic added to circumvent the instance where 2 people may be hitting the website at same time.
.I10 errors can occur if they attempt to prep a file with the same name.
          if (TrapTest <= 5)
          //Only do trap for 5 iterations, otherwise, let it bomb
                    trap      I10Error if IO
          endif
          prep      tempFile,tempFileName,exclusive
          prep      holdingFile,holdingFileName,holdingfileName,"6","682",exclusive
          if (TrapTest <= 5)
                    trapclr   IO
          endif
.END PATCH 1.4 REPLACED LOGIC

    clear emptyFile

.START PATCH 1.4 ADDED LOGIC
          call      Trim using cgisearchtype
          if (cgisearchtype = "Text/Description")
                    move      cgilistname,cgikeyword
                    clear     cgilistname
          endif
.END PATCH 1.4 ADDED LOGIC

    // if we have a list name, then we can do an AAM read for all matching lists
    //  in NINDAT
    if (cgiListName != "")
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0022"
.END PATCH 1.8 ADDED LOGIC
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
// 29 JUN 2005 MLYONS --> use ISAM key here per AH
//                pack NTXTFLD1 from "01X",lstNum
//                call NTXTAIM
                pack NTXTFLD,LSTNUM,"1"
                call NTXTKEY
// 29 JUN 2005 MLYONS --> end of changes
                call filterResults
                if equal
.START PATCH 1.1 REPLACED LOGIC
.                    if (thissession.usertype = "B")
.                        // 27 JUL 2005 MLYONS --> Filter for Brokers per DH
.                        // only show lists that are exclusive and not withdrawn
.                        if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1")
.                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.                        endif
.                        // 27 JUL 2005 MLYONS --> end of changes
.                    else
.                        if (status != "W" and ndatoff != "1")
.                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.                        endif
.                    endif
.........................
                              clock     timestamp,timestamp
                              unpack    timestamp,CC,YY,MM,DD
                              call      CVTJUL
                              move      JULDAYS,N5
                              unpack    REVDATE,CC,YY,MM,DD
                              call      CVTJUL
                              sub       JULDAYS,N5
                              if (N5 <= 730)      .24 Months
.START PATCH 2.0 ADDED LOGIC
                                        if (ELSTCDE = "C")
                                                  pack      MLSTNAME2,"<b>",MLSTNAME,"</b>"
                                        else
                                                  pack      MLSTNAME2,"   ",MLSTNAME
                                        endif
.END PATCH 2.0 ADDED LOGIC
                                        if (thissession.usertype = "B")
                                                  // 27 JUL 2005 MLYONS --> Filter for Brokers per DH
                                                  // only show lists that are exclusive and not withdrawn
.START PATCH 1.7 REPLACED LOGIC
.                                                 if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1")
                                                  if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1" and NDATWEB != "1")
.END PATCH 1.7 REPLACED LOGIC
.START PATCH 1.3 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.START PATCH 2.0 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
                                                  endif
                                                  // 27 JUL 2005 MLYONS --> end of changes
                                        else
.START PATCH 1.7 REPLACED LOGIC
.                                                 if (status != "W" and status != "T" and ndatoff != "1")
                                                  if (status != "W" and status != "T" and ndatoff != "1" and NDATWEB != "1")
.END PATCH 1.7 REPLACED LOGIC
.START PATCH 1.3 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.START PATCH 2.0 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
                                                  endif
                                        endif
                              endif
.END PATCH 1.1 REPLACED LOGIC
                endif
                call NDATKG
            repeat until over
        endif
.START PATCH 1.3 REPLACED LOGIC
.    endif
.
.    // if we have a keyword then we can do an AAM read for all matching list
.    //  descriptions (NINTXT)
.    if (cgiKeyword != "")
    elseif (cgiKeyword != "")
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0023"
.END PATCH 1.8 ADDED LOGIC
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
.START PATCH 1.3 REPLACED LOGIC
.                pack NDATFLD2 from "01X",NTXTLIST
.                call NDATAIM
                pack NDATFLD,NTXTLIST
                call NDATKEY
                pack NTXTFLD,LSTNUM,"1"
                call NTXTKEY
.END PATCH 1.3 REPLACED LOGIC
                call filterResults
                if equal
.START PATCH 1.1 REPLACED LOGIC
.                    if (thissession.usertype = "B")
.                        // 27 JUL 2005 MLYONS --> Filter for Brokers per DH
.                        // only show lists that are exclusive and not withdrawn
.                        if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1")
.                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.                        endif
.                        // 27 JUL 2005 MLYONS --> end of changes
.                    else
.                        if (status != "W" and ndatoff != "1")
.                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.                        endif
.                    endif
                              clock     timestamp,timestamp
                              unpack    timestamp,CC,YY,MM,DD
                              call      CVTJUL
                              move      JULDAYS,N5
                              unpack    REVDATE,CC,YY,MM,DD
                              call      CVTJUL
                              sub       JULDAYS,N5
                              if (N5 <= 730)      .24 Months
.START PATCH 2.0 ADDED LOGIC
                                        if (ELSTCDE = "C")
                                                  pack      MLSTNAME2,"<b>",MLSTNAME,"</b>"
                                        else
                                                  pack      MLSTNAME2,"   ",MLSTNAME
                                        endif
.END PATCH 2.0 ADDED LOGIC
                                        if (thissession.usertype = "B")
                                                  // 27 JUL 2005 MLYONS --> Filter for Brokers per DH
                                                  // only show lists that are exclusive and not withdrawn
.START PATCH 1.7 REPLACED LOGIC
.                                                 if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1")
                                                  if (status != "W" and status != "T" and elstcde = "C" and ndatoff != "1" and NDATWEB != "1")
.END PATCH 1.7 REPLACED LOGIC
.START PATCH 1.3 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.START PATCH 2.0 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
                                                  endif
                                                  // 27 JUL 2005 MLYONS --> end of changes
                                        else
.START PATCH 1.7 REPLACED LOGIC
.                                                 if (status != "W" and status != "T" and ndatoff != "1")
                                                  if (status != "W" and status != "T" and ndatoff != "1" and NDATWEB != "1")
.END PATCH 1.7 REPLACED LOGIC
.START PATCH 1.3 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.START PATCH 2.0 REPLACED LOGIC
.                                                           write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                                            write holdingFile,LSTNUM;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
                                                  endif
                                        endif
                              endif
.END PATCH 1.1 REPLACED LOGIC
                endif
                call NTXTKG
            repeat until over
        endif
    endif

.START PATCH 1.5 ADDED LOGIC
          move      C0,N10
.END PATCH 1.5 ADDED LOGIC

    pack cwk6 from ""
    read holdingFile,cwk6;cwk1
    loop
.START PATCH 1.3 REPLACED LOGIC
.        readks holdingFile;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.        until over
.        write tempFile,seq;LSTNUM,MLSTNAME,listQty,NSELEXC,NTXTTEXT
.START PATCH 2.0 REPLACED LOGIC
.        readks holdingFile;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
.        until over
.        write tempFile,seq;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
        readks holdingFile;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
        until over
        write tempFile,seq;LSTNUM,MLSTNAME2,listQty,NSELEXC,str100,NTXTTEXT
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.5 ADDED LOGIC
          add       C1,N10
.END PATCH 1.5 ADDED LOGIC
    repeat
    close tempFile
    close holdingFile
.START PATCH 2.0 REPLACED LOGIC
.    pack cmdString with tempFileName,",",sortFileName," -U,7-81"
    pack cmdString with tempFileName,",",sortFileName," -U,10-84"
.END PATCH 2.0 REPLACED LOGIC
    sort cmdString
    if over
        // no results
        stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
        noreturn
        return
    endif

    pack cgiDataSource from sortFileName
.START PATCH 1.5 ADDED LOGIC
          move      N10,str10
          call      FormatNumeric using str10,cgiRecNum
.
.START PATCH 1.6 REPLACED LOGIC
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
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.5 ADDED LOGIC
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
.START PATCH 1.3 ADDED LOGIC
.Must be initialized before any returns
          clear     str100
          pack      NXRFFLD,lstnum
.START PATCH 1.4 REMOVED LOGIC
.         if (NSELEXC <> "1" & cgilisttype <> "1")
.                   if (cgilisttype <> NSELEXC)
.                           setflag not equal
.                           return
.                   endif
.         endif
.END PATCH 1.4 REMOVED LOGIC
          //Following code will obliterate lstnum, so we must use it early!!
.END PATCH 1.3 ADDED LOGIC
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
.START PATCH 1.3 ADDED LOGIC
          move      C1,NXRFPATH
          //NXRFFLD had to be set at beginning of routine
          call      NXRFKEY
          if not over
                    pack      COMPFLD,NXRFMLR
                    call      COMPKEY
                    if not over
                              pack      NSMPFLD1,"01X",NXRFMLR
                              call      NSMPAIM
.START PATCH 1.9 REPLACED LOGIC
.                             if not over
..For Testing purposes!!!     if (NXRFFLD <> "021752")
.
.                                       move      COMPCOMP,str55
.                                       // the apostrophe cause problems in javascript with errors
.                                       // so we remove them
.                                       Loop
.                                                 scan      "'",str55
.                                                 until not equal
.                                                 splice    "\þ",str55,1
.                                       repeat
.                                       reset     str55
.                                       replace   "þ'",str55
.                                       chop      str55
.                                       clear     str100
.                                       append    "<a href=#"javascript:detailSub('",str100         ."
.                                       append    NSMPMLR,str100
.                                       append    "','",str100
.                                       append    str55,str100
.                                       append    "');#">",str100                         ."
.                                       append    "Samples</a>",str100
.                                       reset     str100
..                                      endif
.                             endif
................................
                              loop
                                        until over
                                        if (NSMPINACTIVE <> "1")
                                                  move      COMPCOMP,str55
                                                  // the apostrophe cause problems in javascript with errors
                                                  // so we remove them
                                                  Loop
                                                            scan      "'",str55
                                                            until not equal
                                                            splice    "\þ",str55,1
                                                  repeat
                                                  reset     str55
                                                  replace   "þ'",str55
                                                  chop      str55
                                                  clear     str100
                                                  append    "<a href=#"javascript:detailSub('",str100         ."
                                                  append    NSMPMLR,str100
                                                  append    "','",str100
                                                  append    str55,str100
                                                  append    "');#">",str100                         ."
                                                  append    "Samples</a>",str100
                                                  reset     str100
                                                  break
                                        else
                                                  call      NSMPKG
                                        endif
                              repeat
.END PATCH 1.9 REPLACED LOGIC
                    endif
          endif
.END PATCH 1.3 ADDED LOGIC
    setflag equal
    return
///////////////////////////////////////////////////////////////////////////////
.START PATCH 1.3 ADDED LOGIC
actionSamples
          pack      NSMPFLD1,"01X",cgimailer
          call      NSMPAIM
          if over
.Should never really happen!!  Sample link should never have been included!
                    move      "No Samples Found!" to errorMsg
                    return
          endif
          call CGIDisplayFile using "sampledisplay.htt"
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0024"
.END PATCH 1.8 ADDED LOGIC
          return
///////////////////////////////////////////////////////////////////////////////
DISPLAYSAMPLES routine buffer
          // ensure that our mailer number is a 6-byte, zero padded field
          move      C0,result
          move      C0,N6
          move      cgimailer,N6
          move      N6,cgimailer
          rep       zfill,cgiMailer
          pack      compFld,cgiMailer
          call      compKey
          if over
                    move      "", compNum
          endif
          packkey   nsmpfld1,"01X",compNum
          call      nsmpaim
          loop
                    until over
                    while (compNum = nsmpmlr)
.START PATCH 1.9 ADDED LOGIC
                    if (NSMPINACTIVE <> "1")
.END PATCH 1.9 ADDED LOGIC
                              add       "1",result
                              // write the tiff file out as a sample document in the TIFF directory
.don't need to write it out it is on the website
.                              pack      sourceTiff, "s",nsmpmlr,nsmpNum,".tif"
                              pack      sourceTiff, "s",nsmpmlr,nsmpNum,".pdf"
.                              set       tiffFound
.                              trap      tiffNotFound if IO
.                              open      fTIFFIn,sourceTiff,read
.                              trapclr   IO
.                              if (tiffFound)
.                                        getmode   *prepuseip=#fileManagerSettings
.                                        setmode   *prepuseip=""
.                                        pack      destTiff,"..\data\tif\",sourceTiff      ."
.                                        erase     destTiff
.                                        prep      fTIFFOut,destTiff,exclusive
.                                        setmode   *prepuseip=#fileManagerSettings
.                                        loop
.                                                  clear     tiffData
.                                                  read      fTIFFIn,seq;*ABSON,tiffData;
.                                                  until over
.                                                  while (tiffData != "")
.                                                  write     fTIFFOut,seq;*ABSON,tiffData;
.                                        repeat
.                                        close     fTIFFIn
.                                        close     fTIFFOut
                                        pack      cgisamplelink,"<a href='/samples/",sourceTiff,"'>View Sample</a>"
.                              else
.                                        pack      cgiSampleLink, "not found (",sourceTiff,")"
.                              endif
                              move      nsmpNum, cgisamplenumber
                              move      nsmpDes1,cgisampledescrip
                              call      Trim using nsmpdte
                              if (nsmpdte <> "")
                                        unpack    nsmpdte,str4,MM,DD
                                        pack      cgisampledate,MM,SLASH,DD,SLASH,str4
                              else
                                        clear     cgisampledate
                              endif
                              move      buffer,cwk32768
                              call      cgiDisplay using cwk32768
                              stream    *STDOUT,cwk32768
                              if (cgiDataRow = "1")
                                        move      "",cgiDataRow
                              else
                                        move      "1",cgiDataRow
                              endif
.START PATCH 1.9 ADDED LOGIC
                    endif
.END PATCH 1.9 ADDED LOGIC
                    call      nsmpkg
          repeat
          if (result = 0)
                    stream *STDOUT,"<tr><td colspan='4' align='center' class=datarow1><br>No results found.<br><br</td></tr><br>"
          endif
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
tiffNotFound
          clear     tiffFound
          return
///////////////////////////////////////////////////////////////////////////////
.END PATCH 1.3 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
noDatacard

    set missingFlag
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include \\nins1\e\library\include\RobbSaves\appbottom.inc
    include nselio.inc
    include ntxtio.inc
.START PATCH 1.3 ADDED LOGIC
          include   nsmpio.inc
.END PATCH 1.3 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
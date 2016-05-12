///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   listSearch2.pls                                              //
//                                                                           //
//    AUTHOR:   Andrew Harkins                                               //
//                                                                           //
//      DATE:   7 FEB 2005                                                   //
//                                                                           //
// COPYRIGHT:   2002-2004 Names in the News                                  //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI application                                              //
//                                                                           //
// REVISION:    1.0   17 FEB 2005 ASH    Created                             //
//                                                                           //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
          include   apptop.inc
          include   nseldd.inc
          include   ntxtdd.inc
          include   ndatwdd.inc

.EXTERNAL ROUTINES FROM       NDAT002W.PLC
CreateWebCardW      external  "NDAT002W;CreateWebCardW"
///////////////////////////////////////////////////////////////////////////////
cgiColor  dim       %1
cgiFunction         dim       %1
cgiListname         dim       %75
cgiMinquantity      dim       %10
cgilisttype         dim       %1
cgiPageNum          dim       %10
cgiTotPage          dim       %10
cgiRecNum dim       %13
N92                 form      9.2
cgikeyword          dim       %75
cgisearchtype       dim       %50
cgiUserID dim       %36
cgiDataRow          dim       %1
cgiDataSource       dim       %260
cgiLinks  dim       %1024
dataSource          dim       260
sortFileName        dim       260
tempFileName        dim       260
cgiReadPos          dim       %10
cgiQuantity         dim       %15
cgiListTypeDesc     dim       %15
cgiListDescription dim        %500
cgiListSample       dim       %100
cgiMailer dim       %6
cgiMlrName          dim       %50
cgisampledescrip dim          %30
cgisamplelink       dim       %50
cgisamplenumber     dim       %3
cgisampledate       dim       %10
destTiff  dim       260
sourceTiff          dim       260
tiffData  dim       32768
fTIFFIn             file
fTIFFOut  file
tiffFound integer   4
str100              dim       100
cwk1                dim       1
cwk10               dim       10
cwk16               dim       16
cwk32768  dim       32768
holdingFileName     dim       260
cgiListNumber       dim       %6
buffer              dim       65535
cwk6                dim       6
cwk260              dim       260
htmlTemplate        dim       65535
cmdString dim       260
cwk40               dim       40

nwk10               form      10
readPosition        form      10
readToPosition      form      10
quantityToMatch     form      10
listQty             form      10
displayedRecord     form      1

sortFile  file

holdingFile         ifile

ETCRLF              init      0x7f

emptyFile integer   4
i                   integer   4

httName             init      "searchlists.html"

sortedOutput        varlist cgiListNumber:
                    cgiListName:
                    cgiMinQuantity:
                    cgiListType:
                    cgiListSample:
                    cgiListDescription

#fileManagerSettings dim      260

datacardFile        file

missingFlag         integer   4
TrapTest  form      1
///////////////////////////////////////////////////////////////////////////////
start

          // Set up our CGI environment
          call      CGIInit
          call      CGISendHeaders

          // if the session isn't valid for some reason, this will send us to an
          //  error page -  if this page requires admin access, change the call
          //  to this: call sessionValidate using "ADMIN"
          //call    sessionValidate

          // this will send the static top part of the HTML document
          //call    CGIRenderHeader

          call      CGIParse using "cgifunction",cgiFunction
          call      CGIParse using "cgilistname",cgilistname
          call      CGIParse using "cgiMinquantity",cgiMinquantity
          call      CGIParse using "cgilisttype",cgilisttype
          call      CGIParse using "cgikeyword",cgikeyword
          call      CGIParse using "cgimailer",cgimailer
          call      CGIParse using "cgimlrname",cgimlrname
          call      CGIParse using "cgisearchtype",cgisearchtype
          call      CGIParse using "SID",SID

          chop      cgiMinQuantity
          type      cgiMinQuantity
          if equal
                    move      cgiMinQuantity,quantityToMatch
          else
                    move      "0",quantityToMatch
          endif

          // based upon the "function" value, we need to decide whether to render the form or
          //  process it's data
          switch    cgiFunction
          case "0"
                    // display a form
                    call      actionDisplay
          case "1"
                    // process the form results
                    call      actionProcess
          default
                    // display a form
                    call      actionDisplay
          endswitch

          // this will send the static bottom part of the HTML document
          //call    CGIRenderFooter
          stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay
          // this will display the (HTT) document we've created for this
          //  page
          reset errorMsg
          call      CGIDisplayFile using httName
          //call    CGITrack using SID,"0021"
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess
          // logic to process the data that we've parsed from the CGI environment
          if (cgiDatasource != "")
                    // We already have a datasource and we're paging forward or back.  We
                    //  don't need to validate anything.
                    call      CGIDisplayFile using "searchlistresults.html"
                    return
          endif
          count i in cgilistname
          if not zero
                    call      CGIDisplayFile using "searchlistresultslistname.html"
          endif
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
displayResults routine htmlTemplate

          // this is where we display the results - it gets called when the
          //  template gets processed.  Everything between <%DISPLAYRESULTS%> and
          //  <%/DISPLAYRESULTS%> is passed into htmlTemplate and program control
          //  passes here.
          move      htmlTemplate to buffer
          call      cgiParse using "cgidatasource",cgiDatasource
          if over
                    call      createDataSource
          else
                    move      cgiDataSource to sortFileName
                    call      cgiParse using "cgiTotPage",cgiTotPage
                    call      cgiParse using "cgiRecNum",cgiRecNum
          endif

          getmode   *openuseip=#fileManagerSettings
          setmode   *openuseip=""
          trap      noDataSource if io
          open      sortFile,sortFileName,exclusive
          trapclr   io
          setmode   *openuseip=#fileManagerSettings

          // if readPos > 1 then we need to skip to that position in the file
          call      cgiParse using "cgireadpos",cgiReadPos
          if over
                    move      "1",cgiReadPos
          endif

          type      cgiReadPos
          if not equal
                    move      "1",cgiReadPos
          endif
          move      cgiReadPos,readPosition

          move      C1,N10
          move      cgiReadPos,N10
          calc      howmany=N10/20
          add       C1,howmany
          move      howmany,cgiPageNum  .Must always assume at least 1 page!!
          call      Trim using cgiPageNum

          if (readPosition < 1)
                    move      "1",readPosition
          endif

          // skip over records to get to the proper position in our file.  We use
          //  readPosition-1 because we are already pointing at record one by default.
          for i from "1" to (readPosition-1)
                    read      sortFile,seq;sortedOutput
                    until over
          repeat

          // find out how many records to display on the page
          pack      cwk260,"nin;RECSPERPAGE"
          clock     ini,cwk260
          if over
                    move      "20",cwk260
          endif

          type      cwk260
          if not equal
                    move      "20",cwk260
          endif

          move      cwk260,readToPosition
          // read and display the appropriate number of records
          //  based upon the settings in RECSPERPAGE
          clear     displayedRecord
          for i from "1" to readToPosition
          // read the temp file
                    read      sortFile,seq;sortedOutput
                    until over
                    switch    cgiListType
                    case "1"
                              move      "Rental/Exchange",cgiListTypeDesc
                    case "2"
                              move      "Exchange",cgiListTypeDesc
                    case "3"
                              move      "Rental",cgiListTypeDesc
                    default
                              move      "Rental/Exchange",cgiListTypeDesc
                    endswitch

                    scan      ETCRLF,cgiListDescription
                    if equal
                              bump      cgiListDescription,-1
                              lenset    cgiListDescription
                              append    "...<br><div align=right><a target='_blank' href='/datacards/data" to cgiListDescription
                              append    cgiListNumber to cgiListDescription
                              append    ".htm'>more...</a></div>" to cgiListDescription
                              reset     cgiListDescription
                    endif
                    move      cgiMinQuantity,nwk10
                    move      "(Z,ZZZ,ZZZ,ZZ9)",cgiQuantity
                    edit      nwk10,cgiQuantity
                    // display the temp file record according to the template format
                    set       displayedRecord
                    move      buffer,cwk32768
                    call      cgiDisplay using cwk32768
                    stream    *STDOUT,cwk32768
.Check to see if there is a datacard for the list
                    pack      filename from "datacards/data",cgiListNumber,".htm"
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
                    if (cgiDataRow = "1")
                              move      "",cgiDataRow
                    else
                              move      "1",cgiDataRow
                    endif
          repeat

          // decide if we should display a previous page link
          clear     cgiLinks
          if (readPosition > 1)
          // do a previous link
                    append    "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
                    calc      nwk10 = (readPosition - readToPosition)
                    if (nwk10 < 1)
                              move      "1",nwk10
                    endif
                    move      nwk10,cwk10
                    squeeze   cwk10,cwk10
                    append    cwk10,cgiLinks
                    append    "');#">&lt; &lt; Previous</a>" to cgiLinks
          endif

          append    "&nbsp;&nbsp;&nbsp;&nbsp;" to cgiLinks

          // decide if we should display a next page link
          read      sortFile,seq;cwk1
          if not over
                    // do a next link
                    append    "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
                    calc      nwk10 = (readPosition + readToPosition)
                    move      nwk10,cwk10
                    squeeze   cwk10,cwk10
                    append    cwk10,cgiLinks
                    append    "');#">Next &gt; &gt;</a>" to cgiLinks
          endif

          reset     cgiLinks
          return
///////////////////////////////////////////////////////////////////////////////

I10Error
          noreturn  .Pull implied return to trap location off of the call stack
          add       C1,TrapTest
          //Increment one second to try and find a unique timestamp
          pause     "1"
          goto createDataSource

///////////////////////////////////////////////////////////////////////////////
createDataSource
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
    // create temp file
          clock     timestamp,timestamp
          pack      dataSource,timestamp,thisSession.ID
          pack      tempFileName,dataSource,".tmp"
          pack      sortFileName,dataSource,".srt"

          unpack    dataSource,cwk40
          unpack    dataSource,cwk16
          pack      holdingFileName,cwk16
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

          clear     emptyFile

          call      Trim using cgisearchtype
          if (cgisearchtype = "Text/Description")
                    move      cgilistname,cgikeyword
                    clear     cgilistname
          endif

    // if we have a list name, then we can do an AAM read for all matching lists
    //  in NINDAT
          if (cgiListName != "")
                    //call    CGITrack using SID,"0022"
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
                                        pack NTXTFLD,LSTNUM,"1"
                                        call NTXTKEY
                                        call filterResults
                                        if equal
                                                  write holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                        endif
                                        call NDATKG
                              repeat until over
                    endif
          elseif (cgiKeyword != "")
                    //call    CGITrack using SID,"0023"
                    // we have a list name, so use that as our AAM key read
                    clear     NTXTFLD1
                    clear     NSELFLD1
                    clear     NSELFLD2
                    clear     NDATFLD1
                    clear     NDATFLD2
                    pack      NTXTFLD2 from "02F",cgiKeyword
                    call      NTXTAIM
                    if not over
                              loop
                                        pack      NSELFLD1 from "01X",NTXTLIST
                                        pack      NSELFLD2 from "02XBASE"
                                        call      NSELAIM
                                        if over
                                                  move      "1" to NSELEXC
                                        endif
                                        pack      NDATFLD,NTXTLIST
                                        call      NDATKEY
                                        pack      NTXTFLD,LSTNUM,"1"
                                        call      NTXTKEY
                                        call      filterResults
                                        if equal
                                                  write     holdingFile,LSTNUM;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                                        endif
                                        call      NTXTKG
                              repeat until over
                    endif
          endif

          move      C0,N10
          close     NDATWFLIST
          pack      cwk6,""
          read      holdingFile,cwk6;cwk1
          loop
                    readks    holdingFile;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                    until over
                    write     tempFile,seq;LSTNUM,MLSTNAME,listQty,NSELEXC,str100,NTXTTEXT
                    add       C1,N10
          repeat
          close     tempFile
          close     holdingFile
          pack      cmdString with tempFileName,",",sortFileName," -U,7-81"
          sort      cmdString
          if over
          // no results
                    stream    *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                    noreturn
                    return
          endif

          pack      cgiDataSource,sortFileName
          move      N10,str10
          call      FormatNumeric using str10,cgiRecNum
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
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

          move      "An error has occurred: data source not located." to errorMsg
          stream    *stdout,"<hr>",errorMsg:
                    "<hr>SortFileName: ",sortFileName:
                    "<hr>Error Code: ",s$error$
          noreturn
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
filterResults
.Test to see if uploaded in weekly batch
          move      C1,NDATWPATH
          pack      NDATWFLD,lstnum
          call      NDATWTST
          if over
                    setflag not equal
                    return
          endif
.Must be initialized before any returns
          clear     str100
.         //Following code will obliterate lstnum, so we must use it early!!
.         // check to see if the list is already in our result list
          read      holdingFile,LSTNUM;cwk1
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
                    move      cwk10,listqty
          else
                    move      "0",listqty
          endif

          // check to see if there is a Usage for the list
          pack      filename from "Usage/usg",lstnum,".pdf"
          getmode *openuseip=#fileManagerSettings
          setmode *openuseip=""
          clear missingFlag
          trap noDatacard if io
          open datacardFile,filename,read
          close datacardFile
          trapclr io
          setmode *openuseip=#fileManagerSettings

          // if missingFlag is set then we don't have usage for the given list
          //  number


          if (missingFlag)
          else
                    move      olstname,str55
.// the apostrophe cause problems in javascript with errors
.// so we remove them
                    Loop
                              scan      "'",str55
                              until not equal
                              splice    "\þ",str55,1
                    repeat
                    reset     str55
                    replace   "þ'",str55
                    chop      str55
                    clear     str100
                    append    "<a target='_blank' href='/usage/",str100
                    append    "usg",str100
                    append    lstnum,str100
                    append    "#.pdf'",str100
                    append    ">Usage</a>",str100
                    reset     str100
          endif
          setflag equal
          return
///////////////////////////////////////////////////////////////////////////////
noDatacard
          set missingFlag
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
          include   appbottom.inc
          include   nselio.inc
          include   ntxtio.inc
          include   ndatwio.inc
///////////////////////////////////////////////////////////////////////////////
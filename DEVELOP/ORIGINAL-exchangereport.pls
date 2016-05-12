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
//                                                                           //
//        Release   1.8       02Nov2012 DLH       Cleanup of cgilibrary &    //
//                                                .htt files                 //
//        Release   1.7       29DEC2005 ASH       Added Tracking Logic       //
//        Release   1.6       23DEC2005 ASH      Added Options               //
//                                              Access to Datacards          //
//        Release   1.5       14DEC2005 ASH      odified TotPage logic       //
//        Release   1.4       19OCT2005 ASH     Added Page Numbers to form   //
//                      22NOV2005       ASH     Continuation of Above patch  //
//        Release   1.3       04OCT2005 ASH       Redesigned                 // 
//                                                exchangereportresp.htt     //
//        Release   1.2       19AUG2005 ASH       Bug Fixes -                //
//                                              Modified code so that is now //
//                                              Searches by Mailer Number    //
//        Release   1.1     15JUL2005 ASH       Exchange File Conversion     //
// REVISION:    VER01   04 NOV 2004 MLYONS      Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include nxngdd.inc
    include nxchdd.inc
    include nsel2dd.inc
    include nsmpdd.inc
.START PATCH 1.6 ADDED LOGIC
    include nseldd.inc
    include ntxtdd.inc
.END PATCH 1.6 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
.START PATCH 1.6 ADDED LOGIC
.EXTERNAL ROUTINES FROM       NDAT002W.PLC
CreateWebCardW external "NDAT002W;CreateWebCardW"
.END PATCH 1.6 ADDED LOGIC
.START PATCH 1.3 REMOVED LOGIC
.cgiexchangeto       dim     %15
.cgiexchangefrom     dim     %15
.END PATCH 1.3 REMOVED LOGIC
cgiexchangestatus   dim     %30
cgibalance          dim     %36
cgibalanceto        dim     %15
.START PATCH 1.3 REMOVED LOGIC
.cgibalancedue       dim     %36
.END PATCH 1.3 REMOVED LOGIC
cgiDataRow          dim     %1
cgiDataSource       dim     %260
cgidate             dim     %12
cgiexchangename     dim     %50
cgiFunction         dim     %1
cgimailer           dim     %6
cgimailername       dim     %50
cgimailernum        dim     %6
cginame             dim     %50
cgilistnum          dim     %6
.START PATCH 1.1 REPLACED LOGIC
.cginxchkey          dim     %13
cginxchkey          dim     %17
.END PATCH 1.1 REPLACED LOGIC
cgisampledescrip    dim     %30
cgisamplelink       dim     %50
cgisamplenumber     dim     %3
cgitradeco          dim     %260
cgiReadPos          dim     %10
cgiLinks            dim     %1024
.START PATCH 1.3 ADDED LOGIC
cgiexchangeowes     dim     %15
cgiexchangeisowed   dim     %15
cgiexchangeiseven   dim     %1
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
cgiPageNum          dim     %10
cgiTotPage          dim     %10
cgiRecNum           dim     %13
.START PATCH 1.5 ADDED LOGIC
N92                 form    9.2
.END PATCH 1.5 ADDED LOGIC
.END PATCH 1.4 ADDED LOGIC
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
dropDown            dim     260
.START PATCH 1.1 REPLACED LOGIC
.farMailer           dim     4
farMailer           dim     6
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
ListLink      dim       1
str100                  dim     100
readPosition2       form    10
readToPosition2     form    10
cgiDataSource2      dim     %260
cgiListTypeDesc     dim     %15
cgiQuantity         dim     %15
.
cgiListNumber       dim     %6
cgiListName         dim     %75
cgiMinQuantity      dim     %10
cgiListType         dim     %1
cgiListSample       dim     %100
cgiListDescription  dim     %500
.
cgiReadPos2         dim     %10
cgiPageNum2         dim     %10
cgiTotPage2         dim     %10
cgiRecNum2          dim     %13
.
ETCRLF              init    0x7f
missingFlag         integer 4
.
fileReady           integer 4
fReport                       file
.END PATCH 1.6 REPLACED LOGIC
listLabel           dim     260
mailers             dim     20
.START PATCH 1.1 REPLACED LOGIC
.mailer1             dim     4
.mailer2             dim     4
.nearMailer          dim     4
mailer1             dim     6
mailer2             dim     6
nearMailer          dim     6
.END PATCH 1.1 REPLACED LOGIC
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
.    call CGISendHeaders
    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

    // this will send the static top part of the HTML document
.    call CGIRenderHeader
    call cgiRenderMenu
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
.    stream *stdout,"<h1>'",cgilistnum,"'</h1>"
    call CGIParse using "cginame",cginame
    call CGIParse using "cgimailername",cgimailername
    call CGIParse using "cgimailernum",cgimailernum
.    stream *stdout,"<h1>'",cgimailernum,"'</h1>"
    call CGIParse using "cgitradeco",cgitradeco
.START PATCH 1.3 REPLACED LOGIC
.    call CGIParse using "cgiexchangeto",cgiexchangeto
.    call CGIParse using "cgiexchangefrom",cgiexchangefrom
.    call CGIParse using "cgibalancedue",cgibalancedue
    call CGIParse using "cgiexchangeowes",cgiexchangeowes
    call CGIParse using "cgiexchangeisowed",cgiexchangeisowed
    call CGIParse using "cgiexchangeiseven",cgiexchangeiseven
.END PATCH 1.3 REPLACED LOGIC
    call CGIParse using "cgibalanceto",cgibalanceto
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
.START PATCH 1.4 ADDED LOGIC
    call cgiParse using "cgiPageNum",cgiPageNum
    call cgiParse using "cgiTotPage",cgiTotPage
    call cgiParse using "cgiRecNum",cgiRecNum
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.6 REPLACED LOGIC
    call cgiParse using "cgiDataSource2",cgiDataSource2
    call cgiParse using "cgiListTypeDesc",cgiListTypeDesc
    call cgiParse using "cgiQuantity",cgiQuantity
    call cgiParse using "cgiListNumber",cgiListNumber
    call cgiParse using "cgiListName",cgiListName
    call cgiParse using "cgiMinQuantity",cgiMinQuantity
    call cgiParse using "cgiListType",cgiListType
    call cgiParse using "cgiListSample",cgiListSample
    call cgiParse using "cgiListDescription",cgiListDescription
    call cgiParse using "cgiReadPos2",cgiReadPos2
    call cgiParse using "cgiPageNum2",cgiPageNum2
    call cgiParse using "cgiTotPage2",cgiTotPage2
    call cgiParse using "cgiRecNum2",cgiRecNum2
.END PATCH 1.6 REPLACED LOGIC

 debug
//    move "002700" into cgilistnum
//    move "A" into thisSession.userType
//    move "A" into sessionIO.userType
//    move "001082" into thisSession.userCompany
//    move "001082" into sessionIO.userCompany
//    move "1" into cgifunction

    switch cgiFunction
    case "0"
        // display a form
.START PATCH 1.2 REPLACED LOGIC
.        call actionDisplay
          if (thisSession.userType = "C")         .Go directly to Process
                    move      "1",cgiFunction
                    call      actionProcess
          else
                    call      actionDisplay
          endif
.END PATCH 1.2 REPLACED LOGIC
    case "1"
        // process the form results
        call actionProcess
.START PATCH 1.3 REMOVED LOGIC - NO LONGER ACCESSING DETAIL
.    case "2"
.        // display the exchange detail
.        call getDetail
.END PATCH 1.3 REMOVED LOGIC - NO LONGER ACCESSING DETAIL
.START PATCH 1.6 ADDED LOGIC
    case "3"
        // link to listSearch
        call        actionListSearch
    case "4"
        // View Exchange Report in PDF format
        call        actionViewPDF
.END PATCH 1.6 ADDED LOGIC
    default
        // display a form
.START PATCH 1.2 REPLACED LOGIC
.        call actionDisplay
          if (thisSession.userType = "C")         .Go directly to Process
                    move      "1",cgiFunction
                    call      actionProcess
          else
                    call      actionDisplay
          endif
.END PATCH 1.2 REPLACED LOGIC
    endswitch

    // this will send the static bottom part of the HTML document
.    call CGIRenderFooter
    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

    // this will display the template (HTT) document we've created for this
    //  page
    if (thisSession.userType = "A" or thisSession.userType = "B")
        call CGIDisplayFile using httName
    else
        call CGIDisplayFile using "exchangerptconsclient.htt"
    endif
.START PATCH 1.7 ADDED LOGIC
          call      CGITrack using SID,"0004"
.END PATCH 1.7 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

.START PATCH 1.6 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
actionListSearch
          move      C0,N10
          close     tempfile
          clock     timestamp,timestamp
          pack      taskname,timestamp,thisSession.ID
          pack      tempFileName,taskname,".tmp"
          pack      sortFileName,taskname,".srt"
          prep      tempFile,tempFileName,exclusive
          packkey   NXRFFLD2,cgimailer
          clear     NXRFFLD
          move      C2,NXRFPATH
          call      NXRFKEY
          loop
                    until over
                    pack      NDATFLD,NXRFLIST
                    call      NDATKEY
                    if not over
                              pack NSELFLD1 from "01X",lstNum
                              pack NSELFLD2 from "02XBASE"
                              call NSELAIM
                              if over
                                        move "1" to NSELEXC
                              endif
                              pack NTXTFLD,LSTNUM,"1"
                              call NTXTKEY
.Samples
                              pack      NSMPFLD1,"01X",NXRFMLR
                              call      NSMPAIM
                              if not over
.For Testing purposes!!!      if (NXRFFLD <> "021752")
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
                              endif
.
                              unpack    timestamp,CC,YY,MM,DD
                              call      CVTJUL
                              move      JULDAYS,N5
                              unpack    REVDATE,CC,YY,MM,DD
                              call      CVTJUL
                              sub       JULDAYS,N5
                              if (N5 <= 730)      .24 Months
                                        if (status != "W" and status != "T" and ndatoff != "1" and NDATWEB != "1")
                                                  write tempFile,SEQ;LSTNUM,MLSTNAME,str10,NSELEXC,str100,NTXTTEXT
                                                  add       C1,N10
                                        endif
                              endif
                    endif
                    call      NXRFKS
                    until (NXRFMLR <> NXRFFLD2)
          repeat
          close     tempFile
          pack      taskname with tempFileName,",",sortFileName," -U,7-81"
          sort      taskname
          if not over
                    move      N10,str10
                    call      FormatNumeric using str10,cgiRecNum2
.
                    move      N10,N92
                    calc      N92=N92/20
                    move      N92,howmany
                    sub       howmany,N92
                    if (N92 > 0)
                    add       "1",howmany
                    endif
                    move      howmany,cgiTotPage2
                    call      Trim using cgiTotPage2
.
                    move      sortFileName,cgiDataSource2
                    call      CGIDisplayFile using "listsearchresultexchange.htt"
          endif
.START PATCH 1.7 ADDED LOGIC
          call      CGITrack using SID,"0006"
.END PATCH 1.7 ADDED LOGIC
          RETURN
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionViewPDF
          call      CGIDisplayFile using "exchangereportpdf.htt"
.START PATCH 1.7 ADDED LOGIC
          call      CGITrack using SID,"0007"
.END PATCH 1.7 ADDED LOGIC
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYPDF routine buffer
          // displays the Order Record (if any) in an IFRAME object
          call      cgiParse using "cgimailernum",cgimailernum
          if over
                    stream    *STDOUT,"<tr><td class='datarow1' colspan='3' align=center>Unable to create PDF ":
                              "at this time</td></tr>"
                    return
          else
                    clock     timestamp,timestamp
                    pack      taskname,timestamp,cgimailernum
                    calls     "NXCH0002;PrintWebExchange" using taskname
                    if (taskname = "")  //calling Routine Failed!!
                              stream    *STDOUT,"<tr><td class='datarow1' colspan='3' align=center>Unable to create PDF ":
                                        "at this time</td></tr>"
                              return
                    endif
          endif
.
          pause     "15"
.
          getmode   *openuseip=#fileManagerSettings
          setmode   *openuseip=""
          pack      filename,"c:\work\pdf\",taskname,".PDF"
          loop
                    set       fileReady
                    trap      notYet if io
                    open      fReport,filename,exclusive
                    trapclr   io
          repeat until (fileReady)
          setmode   *openuseip=#fileManagerSettings
          pack      filename from "/invoices/",taskname,".PDF"
          stream    *STDOUT,"<center><iframe src='",filename,"' width='100%' height='600'></iframe>"
          stream    *STDOUT,"<br><br>If your Exchange Report does not appear above, or you wish to view it in full screen mode, ":
                    "please click <a href='",filename,"' target='_blank'>here</a></center>"                                        ."
          return
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
notYet
    // the file we're looking for isn't ready for us to get exclusive access
    //  to it yet.
    clear fileReady
    return
///////////////////////////////////////////////////////////////////////////////
.END PATCH 1.6 ADDED LOGIC

///////////////////////////////////////////////////////////////////////////////
actionProcess

    // TO DO: add logic to process the data that we've parsed from the
    //  CGI process

    // validation
 debug
.START PATCH 1.2 REPLACED LOGIC
.    move cgilistnum into nwk6
.    move nwk6 into cgilistnum
.    replace " 0" in cgilistnum
.    count i in cgilistnum
.    if (i > 6 or i < 1)
.        move "Please enter a valid list number." to ErrorMsg
.        call actionDisplay
.        return
.    endif
.
.    // read for file name
.    clear datVars
.    packkey ndatfld with cgilistnum
.
.    // open file
.    call #ndatopen
.    call ndatkey
.    move lstnum into cgilistnum
.    // Get the mailer name
.    debug
.    if (sessionIO.userType = "C")
.        packkey compfld from sessionIO.userCompany
.        call compkey
.        move compcomp into cginame
.    else
.        move olstname into cginame
.    endif
.
.    // Get header info
.    packkey nxrffld from cgilistnum
.    call nxrfkey
.    if not over
..START PATCH 1.1 REPLACED LOGIC
..        packkey mkey from nxrfmlr
..        call nmlrkey
..        move mcomp into cgimailername
..        packkey compfld3 from mnum
..        call compkey3
.        packkey compfld from nxrfmlr
.        call compkey
.        move compcomp into cgimailername
..END PATCH 1.1 REPLACED LOGIC
.        move compnum into cgimailernum
.    endif
.......................................
          if (thisSession.userType = "C")
                    move      sessionIO.userCompany,cgimailernum
          elseif (thisSession.userType = "A")
                    call      Trim using cgimailernum
                    if (cgimailernum = "")
                              move      cgilistnum,cgimailernum
                    endif
          endif
          call      Trim using cgimailernum
          if (cgimailernum = "")
                    move      "Please enter a valid Client number." to ErrorMsg
                    call      actionDisplay
                    return
          endif
          call      ZFillIt using cgimailernum

          packkey   COMPFLD,cgimailernum
          call      COMPKEY
          move      COMPCOMP,cginame
.// Set header info
          move COMPCOMP into cgimailername
          //move    COMPNUM,cgilistnum
.END PATCH 1.2 REPLACED LOGIC
          call CGIDisplayFile using "exchangereportresp.htt"
.START PATCH 1.7 ADDED LOGIC
          call      CGITrack using SID,"0005"
.END PATCH 1.7 ADDED LOGIC
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    debug

.START PATCH 1.6 REPLACED LOGIC
.    clear cgiDatasource
.    call cgiParse using "cgidatasource",cgiDatasource
.    if over
.        call createDataSource
.    else
.        move cgiDataSource to sortFileName
.    endif
.......................
          if (cgifunction = "3")        .Actually doing a call to display Lists!!
                    if (cgiDatasource2 = "")
                              stream    *stdout,"<tr><td colspan=4 align=center class=datarow1>No results found<br><br></td></tr>",newline2
                              return
                    else
                              move      cgiDataSource2 to sortFileName
                              call      DISPLAYRESULTSListSearch
                              return
                    endif
          else
.START PATCH 1.6 REPLACED LOGIC
.Following seems cleaner
.                   clear     cgiDatasource
.                   call      cgiParse using "cgidatasource",cgiDatasource
.                   if over
                    call      Trim using cgiDatasource
                    if (cgiDatasource = "")
.END PATCH 1.6 REPLACED LOGIC
                              call      createDataSource
                    else
                              move      cgiDataSource to sortFileName
                    endif
          endif
.END PATCH 1.6 REPLACED LOGIC
    debug

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
.START PATCH 1.3 REPLACED LOGIC
.        read sortFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
.START PATCH 1.6 REPLACED LOGIC
.        read sortFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer
        read sortFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer,ListLink
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
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
.START PATCH 1.3 REPLACED LOGIC
.        read sortFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
.START PATCH 1.6 REPLACED LOGIC
.        read sortFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer
        read sortFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer,ListLink
          if (ListLink = "1")
                    clear     taskname
                    append    "<a href=#"javascript:getLists('",taskname        ."
                    append    farMailer,taskname
                    append    "');#">",taskname                       ."
                    append    cgitradeco,taskname
                    append    "</a>",taskname
                    reset     taskname
                    move      taskname,cgitradeco
          endif
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
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
.START PATCH 1.6 ADDED LOGIC
DISPLAYRESULTSListSearch

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    trap noDataSource if io
        open sortFile,sortFileName,exclusive
    trapclr io
    setmode *openuseip=#fileManagerSettings

    // if readPos > 1 then we need to skip to that position in the file
    call cgiParse using "cgireadpos2",cgiReadPos2
    if over
        move "1" to cgiReadPos2
    endif

    type cgiReadPos2
    if not equal
        move "1" to cgiReadPos2
    endif
    move cgiReadPos2 to readPosition2
          move      C1,N10
          move      cgiReadPos2,N10
          calc      howmany=N10/20
          add       C1,howmany
          move      howmany,cgiPageNum2 .Must always assume at least 1 page!!
          call      Trim using cgiPageNum2
    if (readPosition2 < 1)
        move "1" to readPosition2
    endif

    // skip over records to get to the proper position in our file.  We use
    //  readPosition-1 because we are already pointing at record one by default.
    for i from "1" to (readPosition2-1)
        read sortFile,seq;;
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

    move cwk260 to readToPosition2
    // read and display the appropriate number of records
    //  based upon the settings in RECSPERPAGE
.    clear displayedRecord
    for i from "1" to readToPosition2
        // read the temp file
        read sortFile,seq;cgiListNumber,cgiListName,cgiMinQuantity,cgiListType,cgiListSample,cgiListDescription
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
.        set displayedRecord
        move buffer to cwk32768
        call cgiDisplay using cwk32768
        stream *STDOUT,cwk32768
.Check to see if there is a datacard for the list
          pack      filename from "datacards/data",cgiListNumber,".htm"
          getmode   *openuseip=#fileManagerSettings
          setmode   *openuseip=""
          clear     missingFlag
          trap      noDatacard if io
          open      tempfile,filename,read
          close     tempfile
          trapclr   io
          if (missingflag)
.Create Datacard on the fly
                    call      CreateWebCardW using cgiListNumber
          endif
          setmode   *openuseip=#fileManagerSettings
        if (cgiDataRow = "1")
            move "" to cgiDataRow
        else
            move "1" to cgiDataRow
        endif
    repeat

    // decide if we should display a previous page link
    clear cgiLinks
.Not going to allow this for now
    if (readPosition2 > 1)
        // do a previous link
        append "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
        calc nwk10 = (readPosition2 - readToPosition2)
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
        calc nwk10 = (readPosition2 + readToPosition2)
        move nwk10 to cwk10
        squeeze cwk10 into cwk10
        append cwk10 to cgiLinks
        append "');#">Next &gt; &gt;</a>" to cgiLinks
    endif

    reset cgiLinks

    return

noDatacard
    set missingFlag
    return

.END PATCH 1.6 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
createDataSource
.For some crazy reason, DisplayResults is being called twice, and so then this routine is.
.Add to this the fact that the second call is not pulling up records with Client Number as mlr1,
.and you have problems
          if (sortFileName <> "")
                    return
          endif

.START PATCH 1.4 ADDED LOGIC
          move      C0,N10
.END PATCH 1.4 ADDED LOGIC

          debug
          // create temp file
          clock     timestamp,timestamp
          pack      dataSource,timestamp,thisSession.ID
          pack      tempFileName,dataSource,".tmp"
          pack      sortFileName,dataSource,".srt"
          prep      tempFile,tempFileName,exclusive
.START PATCH 1.2 REPLACED LOGIC
.    packkey compfld from ownnum
          packkey   compfld,cgimailernum
.END PATCH 1.2 REPLACED LOGIC
          // open file
          call #compopen
          call compkey
          if over
                    // read company file for tradecompany
                    // there are no results - should never happen!!
                    stream    *stdout,"<tr><td colspan=4 align=center class=datarow1>No results found<br><br></td></tr>",newline2
                    return
          else
//        count i in compcomp
//        if not zero
//            move compcomp into cginame
//        else
//            move "Nothing in compcomp" into cginame
//        endif
.START PATCH 1.1 REPLACED LOGIC
.                   move mcomp into cgimailername
                    move      compcomp,cgimailername
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.                   packkey nxrffld from lstnum
                    packkey   nxrffld2,COMPNUM
                    clear     NXRFFLD
                    move      C2,NXRFPATH
.END PATCH 1.2 REPLACED LOGIC
                    call      #nxrfOpen
                    call      nxrfkey
                    if over
                              // there are not results
                              stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                              return
                    else
                              packkey   nxngfld1 from "01X",nxrfmlr
                              move      nxrfmlr into cgimailer
                              // read aam file to get all instance of mlr1
                              call      #nxngOpen
                              call      NXNGAIM
                              loop
.START PATCH 1.3 MOVED LOGIC
                                    until over
.END PATCH 1.3 MOVED LOGIC
.START PATCH 1.2 REMOVED LOGIC
.                                   while (lstnum = cgilistnum)
.END PATCH 1.2 REMOVED LOGIC
                                        if (flag != "I")
                                                  move      entry,cwk5
                                                  replace   " 0",cwk5
                                                  packkey   nxchfld1,acckey,cwk5
                                                  move      nxchfld1,cginxchkey
                                                  call      #nxchOpen
                                                  call      nxchkey
                                                  if over
                                                            // there are not results
                                                            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                                                            return
                                                  endif
.START PATCH 1.3 REPLACED LOGIC
.                    sub usage2 from usage1 giving balance
.                    move usage1 to cgiExchangeTo
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeTo
.                    edit usage1 into cgiExchangeTo
.                    move usage2 to cgiExchangeFrom
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeFrom
.                    edit usage2 into cgiExchangeFrom
.                    move balance into cgibalancedue
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalancedue
.                    edit balance into cgibalancedue
.                    move cgibalancedue into cwk15
.                    if (balance < 0)
.                        pack cgibalancedue with "<font color=red>",cwk15,"</font>"
.                    endif
                                                  sub       usage1,usage2,balance
.Initialize all the vars first off
                                                  clear     cgiexchangeowes
                                                  clear     cgiexchangeisowed
                                                  clear     cgiexchangeiseven
                                                  if (balance < 0)
                                                            move      "(Z,ZZZ,ZZZ,ZZ9)",cgiexchangeowes
                                                            edit      balance,cgiexchangeowes
                                                  elseif (balance > 0)
                                                            move      "(Z,ZZZ,ZZZ,ZZ9)",cgiexchangeisowed
                                                            edit      balance,cgiexchangeisowed
                                                  else
                                                            move      "X",cgiexchangeiseven
                                                  endif
.END PATCH 1.3 REPLACED LOGIC
                                                  // take the exchange mailer # and read the
                                                  //  mailer file to extract a company
                                                  unpack acckey into nearMailer,farMailer
.START PATCH 1.1 REPLACED LOGIC
.                    packkey compFld3 from nearMailer
.// 19 JAN 2005 MLYONS -->  Using wrong routine to read for records for users company
.//                    call compkey
.                    call compkey3
.// 19 JAN 2005 MLYONS -->  end of changes
...............................
                                                  packkey   COMPFLD,nearMailer
                                                  call      compkey
.END PATCH 1.1 REPLACED LOGIC
                                                  if over
                                                            // there are not results
                                                            stream    *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                                                            return
                                                  // not an admin
                                                  elseif (thisSession.userType != "A")
                                                  // not a consultant
                                                            if (thisSession.userType != "O")
                                                                      // check comp num
                                                                      if (thisSession.userCompany != compnum)
                                                                                call      NXNGKG
                                                                                break if over
                                                                                continue
                                                                      endif
                                                            endif
                                                  endif
.START PATCH 1.1 REPLACED LOGIC
.                    packkey mkey from farMailer
.                    call NMLRKEY
                                                  packkey   compfld from farMailer
                                                  call      COMPKEY
.END PATCH 1.1 REPLACED LOGIC
                                                  if over
                                                            // there are not results
                                                            stream    *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                                                            return
                                                  endif
.START PATCH 1.1 REPLACED LOGIC
.                    move mcomp into cwk45
                                                  move compcomp into cwk45
.END PATCH 1.1 REPLACED LOGIC
                    // the apostrophe cause problems in javascript with errors
                    // so we remove them
                                                  Loop
                                                            scan      "'",cwk45
                                                            until not equal
                                                            splice    "\þ",cwk45,1
                                                  repeat
                                                  reset     cwk45
                                                  replace   "þ'",cwk45
                                                  chop      cwk45
                                                  clear     cgitradeco
.START PATCH 1.3 REPLACED LOGIC
.                                                 append    "<a href=#"javascript:detailSub('",cgitradeco     ."
.                                                 append    cginxchkey,cgitradeco
.                                                 append    "','",cgitradeco
.                                                 append    farmailer,cgitradeco
.                                                 append    "','",cgitradeco
.                                                 append    cwk45,cgitradeco
.                                                 append    "');#">",cgitradeco ."
..START PATCH 1.1 REPLACED LOGIC
..                    chop mcomp
..                    append mcomp to cgitradeco
.                                                 chop      compcomp
.                                                 append    compcomp,cgitradeco
..END PATCH 1.1 REPLACED LOGIC
.                                                 append    "</a>",cgitradeco
                                                  chop      compcomp
                                                  move      compcomp,cgitradeco
.END PATCH 1.3 REPLACED LOGIC
                                                  loop
                                                            read      tempFile,seq;cwk1
                                                            until over
                                                  repeat
.START PATCH 1.3 REPLACED LOGIC
.                    write tempFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
.START PATCH 1.6 REPLACED LOGIC
.                                                 write     tempFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer
..........................................................
                                                  clear     ListLink
                                                  packkey   NXRFFLD2,COMPNUM
                                                  clear     NXRFFLD
                                                  call      NXRFKEY
                                                  loop
                                                            until over
                                                            pack      NDATFLD,NXRFLIST
                                                            call      NDATKEY
                                                            if not over
                                                                      unpack    timestamp,CC,YY,MM,DD
                                                                      call      CVTJUL
                                                                      move      JULDAYS,N5
                                                                      unpack    REVDATE,CC,YY,MM,DD
                                                                      call      CVTJUL
                                                                      sub       JULDAYS,N5
                                                                      if (N5 <= 730)      .24 Months
                                                                                if (status != "W" and status != "T" and ndatoff != "1" and NDATWEB != "1")
                                                                                          move      C1,ListLink         .There is at least 1 valid List to Display!!
                                                                                          break
                                                                                endif
                                                                      endif
                                                            endif
                                                            call      NXRFKS
                                                            until (NXRFMLR <> NXRFFLD2)
                                                  repeat
                                                  write     tempFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiExchangeiseven,farMailer,ListLink
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.4 ADDED LOGIC
                                                  add       C1,N10
.END PATCH 1.4 ADDED LOGIC
                                        else
                                                  // the exchange is inactive - don't do anything
                                        endif
                                        call      NXNGKG
.START PATCH 1.3 MOVED LOGIC
.                until over
.END PATCH 1.3 MOVED LOGIC
                              repeat
.
                              clear     nxngfld1
                              clear     cgibalanceto
.START PATCH 1.6 REPLACED LOGIC
.                             packkey   nxngfld2,"02X",nxrfmlr
                              packkey   nxngfld2,"02X",cgimailer
.END PATCH 1.6 REPLACED LOGIC
                              // read aam file to get all instance of mlr1
                              call      NXNGAIM
                              loop
.START PATCH 1.3 MOVED LOGIC
                                        until over
.END PATCH 1.3 MOVED LOGIC
.START PATCH 1.2 REMOVED LOGIC
.                while (lstnum = cgilistnum)
.END PATCH 1.2 REMOVED LOGIC
                                        if (flag != "I")
                                                  move      entry,cwk5
                                                  replace   " 0",cwk5
                                                  packkey   nxchfld1,acckey,cwk5
                                                  move      nxchfld1,cginxchkey
                                                  call      nxchkey
                                                  if over
                                                            // there are not results
                                                            stream    *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                                                            return
                                                  endif
// 18 JAN 2005 MLYONS --> Displaying the balances in reverse
//                    sub usage2 from usage1 giving balance
//                    move usage1 to cgiExchangeTo
.START PATCH 1.3 REPLACED LOGIC
.                    sub usage1 from usage2 giving balance
.                    move usage2 to cgiExchangeTo
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeTo
.//                    edit usage1 into cgiExchangeTo
.                    edit usage2 into cgiExchangeTo
.//                    move usage2 to cgiExchangeFrom
.                    move usage1 to cgiExchangeFrom
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiExchangeFrom
.//                    edit usage2 into cgiExchangeFrom
.                    edit usage1 into cgiExchangeFrom
.                    move balance to cgibalancedue
.                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalancedue
.                    edit balance into cgibalancedue
.                    move cgibalancedue into cwk15
.                    if (balance < 0)
.                        pack cgibalancedue with "<font color=red>",cwk15,"</font>"
.                    endif
                                                  sub       usage2,usage1,balance
                                                  clear     cgiexchangeowes
                                                  clear     cgiexchangeisowed
                                                  clear     cgiexchangeiseven
                                                  if (balance < 0)
                                                            move      "(Z,ZZZ,ZZZ,ZZ9)",cgiexchangeowes
                                                            edit      balance,cgiexchangeowes
                                                  elseif (balance > 0)
                                                            move      "(Z,ZZZ,ZZZ,ZZ9)",cgiexchangeisowed
                                                            edit      balance,cgiexchangeisowed
                                                  else
                                                            move      "X",cgiexchangeiseven
                                                  endif
.END PATCH 1.3 REPLACED LOGIC
// 18 JAN 2005 MLYONS --> end of changes
                    // take the exhchange mailer # and read the
                    //  mailer file to extract a company
                                                  unpack    acckey,farMailer,nearMailer
.START PATCH 1.1 REPLACED LOGIC
.                    packkey compFld3 from nearMailer
.// 19 JAN 2005 MLYONS -->  Using wrong routine to read for users company
.//                    call compkey
.                    call compkey3
.// 19 JAN 2005 MLYONS -->  end of changes
                                                  packkey   compFld,nearMailer
                                                  call      compkey
.END PATCH 1.1 REPLACED LOGIC
                                                  if over
                                                            // there are not results
                                                            stream *stdout,"<tr><td colspan=4 align=center class=datarow1><br>No results found<br><br></td></tr>",newline2
                                                            return
                                                  elseif (thisSession.userType != "A")
                                                            if (thisSession.userType != "O")
                                                                      if (thisSession.userCompany != compnum)
                                                                                call      NXNGKG
                                                                                break if over
                                                                                continue
                                                                      endif
                                                            endif
                                                  endif
.START PATCH 1.1 REPLACED LOGIC
.                    packkey mkey from farMailer
.                    call NMLRKEY
.                    if not over
.                        move mcomp into cwk45
                                                  packkey   compfld,farMailer
                                                  call      COMPKEY
                                                  if not over
                                                            move      compcomp,cwk45
.END PATCH 1.1 REPLACED LOGIC
                        // the apostrophe cause problems in javascript with errors
                        // so we remove them
                                                            Loop
                                                                      scan      "'",cwk45
                                                                      until not equal
                                                                      splice    "\þ",cwk45,1
                                                            repeat
                                                            reset     cwk45
                                                            replace   "þ'",cwk45
                                                            chop      cwk45
                                                            clear     cgitradeco
.START PATCH 1.3 REPLACED LOGIC
.                                                           append    "<a href=#"javascript:detailSub('",cgitradeco     ."
.                                                           append    cginxchkey,cgitradeco
.                                                           append    "','",cgitradeco
.                                                           append    farmailer,cgitradeco
.                                                           append    "','",cgitradeco
.                                                           append    cwk45,cgitradeco
.                                                           append    "');#">",cgitradeco ."
..START PATCH 1.1 REPLACED LOGIC
..                        chop mcomp
..                        append mcomp to cgitradeco
.                                                           chop      compcomp
.                                                           append    compcomp,cgitradeco
..END PATCH 1.1 REPLACED LOGIC
.                                                           append    "</a>",cgitradeco
                                                            chop      compcomp
                                                            move      compcomp,cgitradeco
.END PATCH 1.3 REPLACED LOGIC
                                                            loop
                                                                      read      tempFile,seq;cwk1
                                                                      until over
                                                            repeat
.START PATCH 1.3 REPLACED LOGIC
.                        write tempFile,seq;cwk45,cgitradeco,cgiExchangeTo,cgiExchangeFrom,cgibalancedue,farMailer
.START PATCH 1.6 REPLACED LOGIC
.                                                           write tempFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiexchangeiseven,farMailer
........................................................
                                                            clear     ListLink
                                                            packkey   NXRFFLD2,COMPNUM
                                                            clear     NXRFFLD
                                                            call      NXRFKEY
                                                            loop
                                                                      until over
                                                                      pack      NDATFLD,NXRFLIST
                                                                      call      NDATKEY
                                                                      if not over
                                                                                unpack    timestamp,CC,YY,MM,DD
                                                                                call      CVTJUL
                                                                                move      JULDAYS,N5
                                                                                unpack    REVDATE,CC,YY,MM,DD
                                                                                call      CVTJUL
                                                                                sub       JULDAYS,N5
                                                                                if (N5 <= 730)      .24 Months
                                                                                          if (status != "W" and status != "T" and ndatoff != "1" and NDATWEB != "1")
                                                                                                    move      C1,ListLink         .There is at least 1 valid List to Display!!
                                                                                                    break
                                                                                          endif
                                                                                endif
                                                                      endif
                                                                      call      NXRFKS
                                                                      until (NXRFMLR <> NXRFFLD2)
                                                            repeat
                                                            write tempFile,seq;cwk45,cgitradeco,cgiExchangeowes,cgiExchangeisowed,cgiexchangeiseven,farMailer,ListLink
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.4 ADDED LOGIC
                                                            add       C1,N10
.END PATCH 1.4 ADDED LOGIC
                                                  endif
                                        else
                                        // the exchange is inactive - don't do anything
                                        endif
                                        call NXNGKG
.START PATCH 1.3 MOVED LOGIC
.                until over
.END PATCH 1.3 MOVED LOGIC
                              repeat
                    endif
          endif
          close tempFile

          debug

          pack      cmdString,tempFileName,",",sortFileName," -u,1-30"
          sort      cmdString
          if over
                    // no results
                    stream    *stdout,"<tr><td colspan=4 align=center class=datarow1><br>590:No results found<br><br></td></tr>",newline2
                    return
          endif
          move      sortFileName,cgiDatasource
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
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noDatasource

    noreturn
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
.START PATCH 1.3 REMOVED LOGIC - NO LONGER ACCESSING DETAIL
.getDetail
.    packkey nxchfld1 from cginxchkey
.    call nxchkey
.    if over
.        move "No exchange details are available at this time." to errorMsg
.        return
.    else
.        if (stat = "R")
.            move "Rental" into cgiexchangestatus
.        elseif (stat = "C")
.            move "Cancelled Adjusted" into cgiexchangestatus
.        elseif (stat = "X")
.            move "Cancelled Not Adjusted" into cgiexchangestatus
.        else
.            clear cgiexchangestatus
.        endif
.
.        unpack dat into yyyy,mm,dd
.        pack cgidate from mm,"/",dd,"/",yyyy
.
.        sub usage2 from usage1 giving balance
.        move balance to cgibalance
.        move "(Z,ZZZ,ZZZ,ZZ9)" to cgibalance
.        edit balance into cgibalance
.        move cgibalance into cwk15
.        if (balance < 0)
.            pack cgibalance with "<font color=red>",cwk15,"</font>"
.        endif
.
.        call CGIDisplayFile using "exchangedetail.htt"
.    endif
.
.    return
.///////////////////////////////////////////////////////////////////////////////
.
.///////////////////////////////////////////////////////////////////////////////
.DISPLAYDETAIL routine buffer
.    // ensure that our mailer number is a 6-byte, zero padded field
..START PATCH 1.1 REPLACED LOGIC
..    move cgimailer to nwk4
..    move nwk4 to cgimailer
..    replace " 0" in cgiMailer
..
..    pack compFld3 from cgiMailer
..    call compKey3
...........................
.    move cgimailer to nwk6
.    move nwk6 to cgimailer
.    replace " 0" in cgiMailer
.
.    pack compFld from cgiMailer
.    call compKey
..END PATCH 1.1 REPLACED LOGIC
.    if over
.        move "" to compNum
.    endif
.
.    packkey nsmpfld from compNum,""
.    call nsmpkey
.    loop
.        call nsmpks
.        until over
.        while (compNum = nsmpmlr)
.        add "1" to counter
.
.        // write the tiff file out as a sample document in the TIFF directory
.        pack sourceTiff from "s",nsmpmlr,nsmpNum,".tif"
.        set tiffFound
.        trap tiffNotFound if IO
.            open fTIFFIn,sourceTiff,read
.        trapclr IO
.
.        if (tiffFound)
.            getmode *prepuseip=#fileManagerSettings
.            setmode *prepuseip=""
.            pack destTiff from "..\data\tif\",sourceTiff
.            erase destTiff
.            prep fTIFFOut,destTiff,exclusive
.            setmode *prepuseip=#fileManagerSettings
.            loop
.                clear tiffData
.                read fTIFFIn,seq;*ABSON,tiffData;
.                until over
.                while (tiffData != "")
.                write fTIFFOut,seq;*ABSON,tiffData;
.            repeat
.            close fTIFFIn
.            close fTIFFOut
.
.            pack cgisamplelink from "<a href='/tif/",sourceTiff,"'>View Sample</a>"
.        else
.            pack cgiSampleLink from "not found (",sourceTiff,")"
.        endif
.        move nsmpNum into cgisamplenumber
.        move nsmpDes1 to cgisampledescrip
.
.        move buffer to cwk32768
.        call cgiDisplay using cwk32768
.        stream *STDOUT,cwk32768
.        if (cgiDataRow = "1")
.            move "" to cgiDataRow
.        else
.            move "1" to cgiDataRow
.        endif
.    repeat
.
.    if (counter = 0)
.        stream *STDOUT,"<tr><td colspan='4' align='center' class=datarow1><br>No results found.<br><br</td></tr><br>"
.    endif
.
.    return
.///////////////////////////////////////////////////////////////////////////////
.
.///////////////////////////////////////////////////////////////////////////////
.tiffNotFound
.
.    clear tiffFound
.    return
..END PATCH 1.3 REMOVED LOGIC - NO LONGER ACCESSING DETAIL
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
    include nxngio.inc
    include nxchio.inc
    include nsel2io.inc
    include nsmpio.inc
.START PATCH 1.6 ADDED LOGIC
    include nselio.inc
    include ntxtio.inc
.END PATCH 1.6 ADDED LOGIC
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
.START PATCH 1.1 REPLACED LOGIC
.    load nxchName FROM nxchPath OF nxchNme1,nxchNme2
.    open nxchFile,nxchName,read
.    move c1 to nxchFlg1
    open NXCHFLIST,read
    move c1 to nxchFlag
.END PATCH 1.1 REPLACED LOGIC
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
.START PATCH 1.2 REPLACED LOGIC
.DOLISTNUMBER routine
.
.    if (thisSession.userType = "O" or thisSession.userType = "C")
.        call CGIRenderListNumberInput using "",sessionIO.userCompany,sessionIO.userType,counter
.
.//        if (counter > 0)
.//            stream *stdout,"<center>We're sorry, we can't locate any records associated with your agency.":
.//                "  If this is in error, please inform your NIN contact so we can trouble-shoot the problem.  Thank you.</center>"
.//        endif
.
.//
.//            call CGIDisplayFile using "exchangerpterror.htt"
.//            noreturn
.//            noreturn
.//            return
.//        endif
.    endif
.
.    return
DOCLIENTNUMBER routine
          if (thisSession.userType = "O")
                    call CGIRenderClientNumberInput using "",sessionIO.userCompany,sessionIO.userType,counter
          endif
          return
.END PATCH 1.2 REPLACED LOGIC
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
dropDownError
    return
///////////////////////////////////////////////////////////////////////////////
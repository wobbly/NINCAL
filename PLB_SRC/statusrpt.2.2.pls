.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.//   PROGRAM:   statusrpt.pls                                                //
.//                                                                           //
.//    AUTHOR:   mlyons@adjacency.net                                         //
.//                                                                           //
.//      DATE:   19 NOV 2004                                                  //
.//                                                                           //
.// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
.//              All rights reserved.                                         //
.//                                                                           //
.//  PURPOSE:    CGI template application                                     //
.//                                                                           //
.// REVISION:    VER01   04 NOV 2004 BJACKSON    Created                      //
.//                                                                           //
.//       Release   2.2       09DEC2008 DLH       Added logic to display More totals
.//       Release   2.1       27APR2006 ASH       Added logic to display Exc/Rent//
.//       Release   2.0       19JAN2006 ASH       Bug fixes after soft launch  //
.//       Release   1.9       04JAN2006 ASH       Major overhaul - condensed Mlr PO and Mlr Key searches,
.//                                               removed Camp. Num search
.//       Release   1.8       29DEC2005 ASH       Added Tracking Logic         //
.//       Release   1.7       13DEC2005 ASH       Added Consultant file        //
.//                                                         Modified TotPage logic       //
.//                                                         Added links to Contact page  //
.//       Release   1.6       01DEC2005 ASH       Added logic paging/aam checking//
.//       Release   1.5       21NOV2005 ASH       Added logic for LCRs         //
.//       Release   1.4       19OCT2005 ASH       Patches                      //
.//       Release   1.3       03SEP2005 ASH       Reworked statusrpt.htt       //
.//       Release   1.2       16SEP2005 ASH       SMALL PATCHES TO LIMIT ORDER SEARCHES TO Orders placed within past year
.//       Release   1.1       15JUL2005 ASH       Exchange File Conversion
.//
.///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
    include ncmpdd.inc
    include NSEL2DD.INC
    include nshpdd.inc
    include nspidd.inc
    include nofrdd.inc
    include nspedd.inc
.///////////////////////////////////////////////////////////////////////////////
cgiBasePrice            dim     %10
cgiFunction             dim     %1
cgiOrderNum             dim     %6
.START PATCH 1.9 REPLACED LOGIC
.cgiCampaignNum          dim     %6
cgiOther                dim     %12
.END PATCH 1.9 REPLACED LOGIC
cgiPONum                dim     %12
cgiMailerKey            dim     %12
cgiMailerNum            dim     %4
cgiMailerName           dim     %50
cgiMethod               dim     %50
cgiListNumber           dim     %10
cgilistnum              dim     %6
cgiListName             dim     %50
.START PATCH 1.9 REMOVED LOGIC
.cgiCampaignName         dim     %50
.END PATCH 1.9 REMOVED LOGIC
cgiMailerDate           dim     %25
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
cgiLcrOrder             dim     %1
.begin patch 2.2
cgiordcount                   Dim       %13           .Live order count global CGI var
cgilcrcount                   Dim       %13           .Live Lcr count global CGI var
cgibldcount                   Dim       %13           .Live billed count global CGI var
cgiordrent                    Dim       %36           .Live order rent qty global CGI var
cgiordexch                    Dim       %36           .Live order exch qty global CGI var
cgilcrrent                    Dim       %36           .Live order rent qyu global CGI var
cgilcrexch                    Dim       %36           .Live order exch qty global CGI var
BilledCOunt                   FOrm      10           .Live billed order count
OrderCount                    Form      10             .live order count
LcrCount            Form      10            .live Lcr count
OrdRent             Form      10        .live order rental qty
OrdExch             Form      10        .live order exch qty
LCRRent             Form      10        .live lcr rental qty
LCRExch             Form      10        .live lcr exch qty
.end patch 2.2

.START PATCH 1.3 REMOVED LOGIC
.cgiBilledOrder          dim     %1
.cgiCancelOrder          dim     %1
.cgiPendOrder            dim     %1
.cgiCancelBilledOrder    dim     %1
.cgiCancelPendOrder      dim     %1
.cgiCancelLcrOrder       dim     %1
.cgiOtherUnknown         dim     %1
.END PATCH 1.3 REMOVED LOGIC
cgiFromDate             dim     %12
cgiToDate               dim     %12
cgiDataSource           dim     %260
cgiLinks                dim     %260
dataSource              dim     260
sortFileName            dim     260
tempFileName            dim     260
selectiveSort           dim     260
cmdString               dim     260
cgiReadPos              dim     %10
.START PATCH 1.6 ADDED LOGIC
cgiPageNum          dim     %10
cgiTotPage          dim     %10
cgiRecNum           dim     %13
.START PATCH 1.7 ADDED LOGIC
.START PATCH 2.1 ADDED LOGIC
cgiExcRent                    dim       %10
.END PATCH 2.1 ADDED LOGIC
N92                 form    9.2
.END PATCH 1.7 ADDED LOGIC
.END PATCH 1.6 ADDED LOGIC
consultantNum           dim     6
cwk3                    dim     3
cwk4                    dim     4
cwk10                   dim     10
cwk260                  dim     260
oldConsultant           dim     6
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
.START PATCH 1.1 ADDED LOGIC
.I can get rid of this when ALL files have updated Mailer Numbers!!
thisMailer2             dim     6
thisBroker              dim     6
.END PATCH 1.1 ADDED LOGIC
thisTimestamp           dim     8
validLists              dim     260
validMailers            dim     260

#fileManagerSettings    dim     260

.START PATCH 1.9 REPLACED LOGIC
.sortedOutput            varlist cgiOrderName:           //001-260
.                                cgilistnum:             //261-266
.                                cgimailernum:           //267-270
.                                cgimailername:          //271-320
.                                cgicampaignnum:         //321-326
.                                cgicampaignname:        //327-376
.                                cgiponum:               //377-388
.                                cgimailerkey:           //389-400
.                                cgimailerdate:          //401-425
.                                cgistatus               //426-445
.START PATCH 2.1 REPLACED LOGIC
.sortedOutput            varlist cgiOrderName:           //001-260
.                                cgilistnum:             //261-266
.                                cgimailernum:           //267-270
.                                cgimailername:          //271-320
.                                cgiponum:               //321-332
.                                cgimailerkey:           //333-344
.                                cgimailerdate:          //345-369
.                                cgistatus               //370-389
sortedOutput            varlist cgiOrderName:           //001-260
                                cgilistnum:             //261-266
                                cgimailernum:           //267-270
                                cgimailername:          //271-320
                                cgiponum:               //321-332
                                cgimailerkey:           //333-344
                                cgimailerdate:          //345-369
                                cgistatus:              //370-389
                                cgiorderqty:                //390-425
                                cgiexcrent                  //426-435
.END PATCH 2.1 REPLACED LOGIC
.END PATCH 1.9 REPLACED LOGIC
buffer              dim     32768
brokerFlag          dim     1
clientFlag          dim     1
consultantFlag      dim     1
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
readMethod          dim     1   // O=order id,L=List, "-"=other
.readMethod          dim     1   // O,C,P,M
todate              dim     8
todd                dim     2
tomm                dim     2
toyy                dim     4
validRange          dim     1
yyyy                dim     4
thisShortMailerNo   dim     4

i                   integer 4

httName             init    "statusrpt.htt"
replf               init    0X7F,0X20

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
.START PATCH 1.2 ADDED LOGIC
JULDAYS2  form      5
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
fileReady               integer 4
fOrder                  file
.END PATCH 1.4 ADDED LOGIC
///////////////////////////////////////////////////////////////////////////////
start

.    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

.    // if the session isn't valid for some reason, this will send us to an
.    //  error page -  if this page requires admin access, change the call
.    //  to this: call sessionValidate using "ADMIN"
    call sessionValidate

.    // this will send the static top part of the HTML document
    call CGIRenderHeader

.    ///////////////////////////////////////////////////////////////////////////
.    // TO DO: insert all of your CGI parse statements here.  If any variable is
.    //         supposed to filled out but you get an over, empty string, etc.,
.    //         append an error string (in HTML) to the variable errorMsg
.    ///////////////////////////////////////////////////////////////////////////
    call CGIParse using "SID",SID
    if over
        append "Session ID is missing!<br>" to errorMsg
    endif

.    // based upon the "function" value, we need to decide whether to render the form or
.    //  process it's data
    call CGIParse using "cgifunction",cgiFunction
    call CGIParse using "cgiordernum",cgiOrderNum
.START PATCH 1.9 REPLACED LOGIC
.    call CGIParse using "cgicampaignnum",cgiCampaignNum
    call CGIParse using "cgiother",cgiOther
.END PATCH 1.9 REPLACED LOGIC
    call CGIParse using "cgiponum",cgiPONum
    call CGIParse using "cgimailerkey",cgiMailerKey
    call CGIParse using "cgilistnum",cgilistnum
    call CGIParse using "cgilistname",cgiListName
.START PATCH 1.9 REMOVED LOGIC
.    call CGIParse using "cgicampaignname",cgiCampaignName
.END PATCH 1.9 REMOVED LOGIC
    call CGIParse using "cgimailerdate",cgiMailerDate
    call CGIParse using "cgifromdate",cgifromdate
    call CGIParse using "cgitodate",cgitodate
    call CGIParse using "cgistatus",cgiStatus
    call CGIParse using "cgiliveorder",cgiliveOrder
    call CGIParse using "cgilcrorder",cgilcrorder
.START PATCH 1.3 REMOVED LOGIC
.    call CGIParse using "cgibilledorder",cgibilledorder
.    call CGIParse using "cgicancelorder",cgicancelorder
.    call CGIParse using "cgicancelbilledorder",cgicancelbilledorder
.    call CGIParse using "cgipendorder",cgipendorder
.    call CGIParse using "cgicancelpendorder",cgicancelpendorder
.    call CGIParse using "cgicancellcrorder",cgicancellcrorder
.    call CGIParse using "cgiotherunknown",cgiotherunknown
.END PATCH 1.3 REMOVED LOGIC
    call cgiParse using "cgiordername",cgiordername
    call cgiParse using "cgimailername",cgimailername
    call cgiParse using "cgimailernum",cgimailernum
    call cgiParse using "cgidatasource",cgiDatasource
.    stream *stdout,"<h1>'",cgiDatasource,"'</h1>"
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
.START PATCH 2.1 ADDED LOGIC
    call cgiParse using "cgiexcrent",cgiExcRent
   
.END PATCH 2.1 ADDED LOGIC
.//    move "524114" into cgiordernum
.//    move "010286" into cgilistnum
.//    move "001915" into cgiCampaignNum
.//    move "O" into thisSession.userType
.//    move "O" into sessionIO.userType
.//    move "000536" into thisSession.userCompany
.//    move "000536" into sessionIO.userCompany
.//    move "1" into cgifunction
.
.    // open all our files in read-only mode
    open compFList,read
    move c1 to compFlag

    load ndatName using ndatPath from ndatNme1,ndatNme2
    open ndatFList,read
    move c1 to nDatFlag

    move nxrfNam1 to nxrfName
    open nxrfFLst,read
    move c1 to nxrfFlag
    move c1 to nxrfFlg2

.START PATCH 1.9 REMOVED LOGIC
.    move "1" to ncmpPath
..START PATCH 1.1 REPLACED LOGIC
..    load ncmpName using ncmpPath from ncmpNme1,ncmpNme2
..    open ncmpFile,ncmpName,read
..    move ncmpPath to ncmpFlag
.    open NCMPFLIST,read
.    move ncmpPath to ncmpFlag
..END PATCH 1.1 REPLACED LOGIC
.END PATCH 1.9 REMOVED LOGIC

    move "1" to nordPath
    load nordName using nordPath from nordNme1,nordNme2,nordNme3
    open nordFile,nordName,read
    move nordPath to nordFlag

    open nshpFile,nshpName,read
    move c1 to nshpFlag

    open nspiFile,nspiName,read
    move c1 to nspiFlag

.START PATCH 1.1 REPLACED LOGIC
.    // this processes our company information
.    if (thisSession.userType = "A")
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
.                else
.                    // this is a client
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
.                    move thisMailer to NXRFFLD2
.                    move "2" to NXRFPATH
.                    call nxrfKey
.                    loop
.                        while (nxrfMlr = compOldMlr)
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
.    endif
.............................................................
.         // this processes our company information
          if (thisSession.userType = "A")
.         // we can skip it if we're an administrator since security doesn't apply
.START PATCH 1.4 REPLACED LOGIC
.         else
          elseif (cgiFunction <> "3")
.END PATCH 1.4 REPLACED LOGIC
.         // this user is not an administrator
                    call      compOpen
                    packkey   compFld,thisSession.userCompany
                    call      COMPKEY
                    if over
.                             // no results
.START PATCH 1.7 REPLACED LOGIC
.                             stream    *STDOUT,"No company set up in your user account.  Please contact support!"
                              stream    *STDOUT,"No company set up in your user account.  Please contact your <a href=#"#/contact.htm#";#"> NIN representative</a>!"
.END PATCH 1.7 REPLACED LOGIC
                              call      CGIRenderFooter
                              stop
                    else
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
.START PATCH 1.7 REPLACED LOGIC
.                                                 stream    *STDOUT,"The company assigned to you in your user account appears to have ":
.                                                           "the following problem: <br><br>",newline2:
.                                                           "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
.                                                           "Please contact the website administrator to correct this problem."
                                                  stream    *STDOUT,"The company assigned to you in your user account appears to have ":
                                                            "the following problem: <br><br>",newline2:
                                                            "<i>No COMPOLDMLR field has been assigned to this company</i><br><br>",newline2:
                                                            "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."
.END PATCH 1.7 REPLACED LOGIC
                                                  call      CGIRenderFooter
                                                  stop
                                        endif
                                        move      compoldmlr,thisMailer
                              else
.                             //This should never really happen.  Welcome Page should not even give statusrpt option if not:  Client, Consultant, Broker
.START PATCH 1.7 REPLACED LOGIC
.                                       stream    *STDOUT,"You have not been assigned a Status that allows you ":
.                                                 "to View Orders. <br><br>",newline2:
.                                                 "Please contact the website administrator to correct this problem."
                                        stream    *STDOUT,"You have not been assigned a Status that allows you ":
                                                  "to View Orders. <br><br>",newline2:
                                                  "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."
.END PATCH 1.7 REPLACED LOGIC
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
.START PATCH 1.7 REPLACED LOGIC
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
.END PATCH 1.7 REPLACED LOGIC
                                                  reset     validMailers
                                                  reset     validLists
                                        endif
                              endif
                    endif
          endif
.END PATCH 1.1 REPLACED LOGIC
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
.START PATCH 1.4 ADDED LOGIC
    case "3"
        // call up hard copy
        call getOrder
.END PATCH 1.4 ADDED LOGIC
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

.    // this will display the template (HTT) document we've created for this
.    //  page
    call CGIDisplayFile using httName
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0010"
.END PATCH 1.8 ADDED LOGIC

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

.    // logic to process the data that we've parsed from the CGI environment
    if (cgiDatasource != "")
.        // We already have a datasource and we're paging forward or back.  We
.        //  don't need to validate anything.
        call CGIDisplayFile using "statusrptresponse.htt"
        return
    endif

.    // We are going to have to build up a data file, so we have to validate
.    //  the user inputs first.
    clear readKey,readMethod
.START PATCH 1.2 REPLACED LOGIC
.    if (cgiListNum <> "")
.        move cgiListNum into nwk6
.        move nwk6 into cgiListnum
.        replace " 0" in cgiListnum
.        move "L" to readMethod
.        move cgiListNum to readKey
.    elseif (cgiOrderNum <> "")
.        move "O" to readMethod
.        move cgiOrderNum to readKey
.    elseif (cgiCampaignNum <> "")
.        move cgiCampaignNum into nwk6
.        move nwk6 into cgiCampaignNum
.        replace " 0" in cgiCampaignNum
.        move "C" to readMethod
.        move cgiCampaignNum to readKey
.    elseif (cgiPONum <> "")
.        move "P" to readMethod
.        move cgiPONum to readKey
.    elseif (cgiMailerKey <> "")
.        move "M" to readMethod
.        move cgiMailerKey to readKey
.    endif
.START PATCH 1.9 REPLACED LOGIC
.    if (cgiOrderNum <> "")
.        move "O" to readMethod
.        move cgiOrderNum to readKey
.    elseif (cgiCampaignNum <> "")
.        move cgiCampaignNum into nwk6
.        move nwk6 into cgiCampaignNum
.        replace " 0" in cgiCampaignNum
.        move "C" to readMethod
.        move cgiCampaignNum to readKey
.    elseif (cgiPONum <> "")
.        move "P" to readMethod
.        move cgiPONum to readKey
.    elseif (cgiMailerKey <> "")
.        move "M" to readMethod
.        move cgiMailerKey to readKey
.    elseif (cgiListNum <> "")
.        move cgiListNum into nwk6
.        move nwk6 into cgiListnum
.        replace " 0" in cgiListnum
.        move "L" to readMethod
.        move cgiListNum to readKey
.    endif
...................................
          if (cgiOrderNum <> "")
                    move      "O",readMethod
                    move      cgiOrderNum,readKey
          elseif (cgiListNum <> "")
                    move      cgiListNum,nwk6
                    move      nwk6,cgiListnum
                    replace   " 0",cgiListnum
                    move      "L",readMethod
                    move      cgiListNum,readKey
          elseif (cgiOther <> "")
                    move      "-",readMethod
                    move      cgiOther,readKey
          endif
.END PATCH 1.9 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.    // validate our date fields
    count i in cgiFromDate
    if not zero
        if (i != 10)
            move "Please enter a valid 'From' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif

        unpack cgiFromDate into frommm,cwk1,fromdd,cwk1,yyyy
.        // validate month
        if (fromMM = "00" or fromMM > "12")
            move "Please enter a valid 'From' date range: month is invalid." into errorMsg
            call actionDisplay
            return
.        // months with 31 days
        elseif (fromMM = "1" or fromMM = "3" or fromMM = "5" or fromMM = "7" or fromMM = "8" or fromMM = "10" or fromMM = "12")
            if (fromDD < "01" or fromDD > "31")
                move "Please enter a valid 'From' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
.        // months with 30 days
        elseif (fromMM = "4" or fromMM = "6" or fromMM = "9" or fromMM = "11")
            if (fromDD < "01" or fromDD > "30")
                move "Please enter a valid 'From' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
.        // February
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
.            //  invalid character found in time string
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

.        // validate month
        if (toMM = "00" or toMM > "12")
            pack errorMsg with "Please enter a valid 'To' date range: month is invalid."
            call actionDisplay
            return
.        // months with 31 days
        elseif (toMM = "01" or toMM = "03" or toMM = "05" or toMM = "07" or toMM = "08" or toMM = "10" or toMM = "12")
            if (toDD < "01" or toDD > "31")
                move "Please enter a valid 'To' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
.        // months with 30 days
        elseif (toMM = "04" or toMM = "06" or toMM = "09" or toMM = "11")
            if (toDD < "01" or toDD > "30")
                move "Please enter a valid 'To' date range: day is invalid." into errorMsg
                call actionDisplay
                return
            endif
.        // February
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
.            //  invalid character found in time string
            move "Please enter a valid 'To' date range: mm/dd/yyyy." into errorMsg
            call actionDisplay
            return
        endif
        move "Y" to validRange
    endif

    if (validRange = "Y")
.        // we have a valid date search, so make sure we have defaults in
.        //  our to/from timestamps if they are empty
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
.        // nothing provided
        move "Please enter an order, campaign, purchase order or mailer key. " to errorMsg
        call actionDisplay
        return
    endif

.// implement different display files here
    if (readMethod = "L")
        call CGIDisplayFile using "statusrptlistnumberresp.htt"
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0012"
.END PATCH 1.8 ADDED LOGIC
    elseif (readMethod = "O")
        call CGIDisplayFile using "statusrptordernumberresp.htt"
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0011"
.END PATCH 1.8 ADDED LOGIC
.START PATCH 1.9 REPLACED LOGIC
.    elseif (readMethod = "C")
.        call CGIDisplayFile using "statusrptcampnumberresp.htt"
..START PATCH 1.8 ADDED LOGIC
.         call      CGITrack using SID,"0013"
..END PATCH 1.8 ADDED LOGIC
.    elseif (readMethod = "P")
.        call CGIDisplayFile using "statusrptpurchorderresp.htt"
..START PATCH 1.8 ADDED LOGIC
.         call      CGITrack using SID,"0013"
..END PATCH 1.8 ADDED LOGIC
.    elseif (readMethod = "M")
.        call CGIDisplayFile using "statusrptmlrkeyresp.htt"
..START PATCH 1.8 ADDED LOGIC
.         call      CGITrack using SID,"0013"
..END PATCH 1.8 ADDED LOGIC
.    else
.        call CGIDisplayFile using "statusrptresponse.htt"
..START PATCH 1.8 ADDED LOGIC
.         call      CGITrack using SID,"0013"
..END PATCH 1.8 ADDED LOGIC
    elseif (readMethod = "-")
        call        CGIDisplayFile using "statusrptotherresp.htt"
          call      CGITrack using SID,"0013"
.END PATCH 1.9 REPLACED LOGIC
    endif

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
DISPLAYRESULTS routine buffer

    debug

    call cgiParse using "cgidatasource",cgiDatasource
    if over
        call createDataSource
    else
        move cgiDataSource to sortFileName
.START PATCH 1.6 ADDED LOGIC
          call      cgiParse using "cgiTotPage",cgiTotPage
          call      cgiParse using "cgiRecNum",cgiRecNum
.begin patch 2.2    
          call      cgiParse using "cgiordcount",cgiordcount
          call      cgiParse using "cgiordrent",cgiordrent
          call      cgiParse using "cgiordexch",cgiordexch
          call      cgiParse using "cgilcrcount",cgilcrcount
          call      cgiParse using "cgilcrrent",cgilcrrent
          call      cgiParse using "cgilcrexch",cgilcrexch
          call      cgiParse using "cgibldcount",cgibldcount
.end patch 2.2      
          
.END PATCH 1.6 ADDED LOGIC
    endif

    trap noDataSource if io
        getmode *openuseip=#fileManagerSettings
        setmode *openuseip=""
        open sortFile,sortFileName,exclusive
        setmode *openuseip=#fileManagerSettings
    trapclr io

.    // if readPos > 1 then we need to skip to that position in the file
    call cgiParse using "cgireadpos",cgiReadPos
    if over
        move "1" to cgiReadPos
    endif

    type cgiReadPos
    if not equal
        move "1" to cgiReadPos
    endif
    move cgiReadPos to readPosition

.START PATCH 1.6 ADDED LOGIC
          move      C1,N10
          move      cgiReadPos,N10
          calc      howmany=N10/20
          add       C1,howmany
          move      howmany,cgiPageNum  .Must always assume at least 1 page!!
          call      Trim using cgiPageNum

.END PATCH 1.6 ADDED LOGIC

    if (readPosition < 1)
        move "1" to readPosition
    endif

.    // skip over records to get to the proper position in our file.  We use
.    //  readPosition-1 because we are already pointing at record one by default.
    for i from "1" to (readPosition-1)
        read sortFile,seq;sortedOutput
        until over
    repeat

.    // find out how many records to display on the page
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

    debug
.    // read and display the appropriate number of records
.    //  based upon the settings in RECSPERPAGE
    for i from "1" to readToPosition
.        // read the temp file
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

.        // format the mailer date
        unpack cgiMailerDate into yyyy,mm,dd
        pack cgiMailerDate from mm,"/",dd,"/",yyyy
        if (cgiMailerDate = "11/11/2011")
            move "See Special Instructions" into cgiMailerDate
        elseif (cgiMailerDate = "00/00/2000")
            move "As Soon As Possible" into cgiMailerDate
        endif

.START PATCH 2.1 ADDED LOGIC
.         //Format Qty
          call      Trim using cgiorderqty
          call      RemoveChar using cgiorderqty,COMMA
          if (cgiorderqty <> "")
                    move      cgiorderqty,str45
                    call      FormatNumeric using str45,cgiorderqty
          endif

          //Format Exc/Rent
          //1=RENT/ENTIRE,2=EXCH,3=EXCHANGE/ENTIRE
          call      Trim using cgiexcrent
          if (cgiexcrent = "1")
                    move      "Rental",cgiexcrent
          elseif (cgiexcrent = "2")
                    move      "Exchange",cgiexcrent
          elseif (cgiexcrent = "3")
                    move      "Exc/Rent",cgiexcrent
          endif
.END PATCH 2.1 ADDED LOGIC

.        // display the temp file record according to the template format
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

.    // decide if we should display a next page link
    read sortFile,seq;cwk1
    if not over
.        // do a next link
        append "<a class='menuText' href=#"javascript:changepg('" to cgiLinks
        calc nwk10 = (readPosition + readToPosition)
        move nwk10 to cwk10
        squeeze cwk10 into cwk10
        append cwk10 to cgiLinks
        append "');#">Next &gt; &gt;</a>" to cgiLinks
    endif
    reset cgiLinks

    erase sortFileName
    erase tempFileName

    
    
    

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
createDataSource

    debug

.START PATCH 1.6 ADDED LOGIC
          move      C0,N10
          move      C0,cgiPageNum
          move      C0,cgiTotPage
          move      C0,cgiRecNum
          
.START PATCH 1.9 REPLACED LOGIC
.         if (readMethod = "P" | readMethod = "M")
          if (readMethod = "-")
.END PATCH 1.9 REPLACED LOGIC
.Make sure there is enough to search on!!
                    move      readKey,taskname
                    call      Trim using taskname
                    count     howmany,taskname
                    if (howmany < 3)
.                             // no results
                              stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>You must enter at least 3 characters to search!<br><br></td></tr>"
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
                                        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>To Search you must enter at least 3 sequential non-blank, non '?' characters!<br><br></td></tr>"
                                        noreturn
                                        return
                              endif
                    endif
          endif
.END PATCH 1.6 ADDED LOGIC

.    // create temp file
    clock timestamp into timestamp
    pack dataSource from timestamp,thisSession.ID
    pack tempFileName from dataSource,".tmp"
    pack sortFileName from dataSource,".srt"

    getmode *prepuseip=#fileManagerSettings
    setmode *prepuseip=""
    prep tempFile,tempFileName,exclusive
    setmode *prepuseip=#fileManagerSettings

.START PATCH 1.7 ADDED LOGIC
          if (thisSession.userType = "O")                   .Consultant Mode
.                   //Must refresh Consultant file record
                    pack      NCLTFLD1,"01X",sessionIO.UserCompany
                    pack      NCLTFLD2,"02X",COMPNUM
                    call      NCLTAIM
                    if not over
                              loop
.                                       //Save variables from latest read
                                        pack      taskname,NCLTVARS
.                                       //Position to the last entry - the most recent!
                                        call      NCLTKG
                                        until over
                              repeat
                              unpack    taskname,NCLTVARS   //Refresh variables!
                              call      Trim using NCLTSDATE
                              call      Trim using NCLTEDATE
                    else
.START PATCH 1.7 REPLACED LOGIC
.                             stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>You are not currently set up to view Order records.  Please contact your NIN representative.<br><br></td></tr>"
                              stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>You are not currently set up to view Order records.  Please contact your <a href=#"#/contact.htm#";#"> NIN representative</a>.<br><br></td></tr>"
.END PATCH 1.7 REPLACED LOGIC
                              noreturn
                              return
                    endif
.Following is a safety measure!!
                    call      Trim using NCLTCONSULT        //if empty, then we know we got an over - this is used later on!!
          endif
.END PATCH 1.7 ADDED LOGIC

.START PATCH 1.9 ADDED LOGIC
          move      C1,NORDPATH
FirstRead
.END PATCH 1.9 ADDED LOGIC

    switch readMethod
    case "L"
        packkey nOrdFld2 from "02X",readkey
.START PATCH 1.2 REPLACED LOGIC
.        call nOrdAIM
        call nOrdlast
.END PATCH 1.2 REPLACED LOGIC
    case "O"
        move readKey to nOrdFld
        move "1" to nOrdPath
        call nOrdKey
.START PATCH 1.9 REPLACED LOGIC
.    case "C"
.        move readKey to nOrdFldC
.        move "4" to nOrdPath
.        call nOrdKey
.    case "P"
.        uppercase readKey
.        packkey nOrdFld3 from "03X",readKey
..START PATCH 1.2 REPLACED LOGIC
..        call nOrdAIM
.        call nOrdlast
..END PATCH 1.2 REPLACED LOGIC
.    case "M"
.        packkey nOrdFld7 from "06X",readKey
..START PATCH 1.2 REPLACED LOGIC
..        call nOrdAIM
.        call nOrdlast
..END PATCH 1.2 REPLACED LOGIC
.
.The deal here is that we are using one value to read 2 different aam partitions
.HTT file passes us cgiother and we establish readkey value of "-".
.When we are finished reading the Mailer Key we change readkey to "+" in order to go
.back and read Mlr PO.
.We read Mailer Key first because that is not forced to Uppercase!!
    case "-"
        packkey NORDFLD7 from "06X",readKey
        clear       NORDFLD1
        clear       NORDFLD2
        clear       NORDFLD3
        clear       NORDFLD4
        clear       NORDFLD5
        clear       NORDFLD6
        clear       NORDFLD8
        call nOrdlast
    case "+"
        uppercase readKey
        packkey NORDFLD3 from "03X",readKey
        clear       NORDFLD1
        clear       NORDFLD2
        clear       NORDFLD4
        clear       NORDFLD5
        clear       NORDFLD6
        clear       NORDFLD7
        clear       NORDFLD8
        call nOrdlast
.END PATCH 1.9 REPLACED LOGIC
    endswitch

    if over
.START PATCH 1.9 REPLACED LOGIC
          if (readMethod = "-")
                    move      "+",readMethod
                    goto FirstRead
          elseif (readMethod = "+")
                    goto EndofLoop2
          endif
.END PATCH 1.9 REPLACED LOGIC
.        // no results found
        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
        noreturn
        return
    else
.        // loop through the order file
.START PATCH 1.2 ADDED LOGIC
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          call      CVTJUL
          move      JULDAYS,JULDAYS2
.END PATCH 1.2 ADDED LOGIC
        loop
.START PATCH 1.2 ADDED LOGIC
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
.START PATCH 1.9 REPLACED LOGIC
.                                       break
                                        if (readMethod <> "-")
                                                  break
                                        else
                                                  goto EndofLoop
                                        endif
.END PATCH 1.9 REPLACED LOGIC
                              elseif (N5 > 365)
                                        goto EndofLoop
                              endif
                    endif
.END PATCH 1.2 ADDED LOGIC
.                   // get broker number
                    move      OBRKNUM into consultantNum
.START PATCH 1.9 REMOVED LOGIC
.                   // get campaign info
.                   move      oCamp to nCmpFld
.                   move      "1" to nCmpPath
.                   call      nCmpKey
.                   if over
.                             move      "n/a" to nCmpCName
.                   endif
.END PATCH 1.9 REMOVED LOGIC

.START PATCH 1.1 REPLACED LOGIC
...START PATCH 1.1 REPLACED LOGIC
..            move oMlrNum to nXrfFld2
.             pack COMPFLD3,OMLRNUM
.             call COMPKEY3
.            move COMPNUM to nXrfFld2
..END PATCH 1.1 REPLACED LOGIC
.            move "2" to nXrfPath
.            clear nXrfList
.            call nXrfKey
.                packkey nDatFld from nXrfList
.                call nDatKey
.                if over
.                    // no list information
.                    move "n/a" to lstNum
.                    move "n/a" to oLstName
.                endif
.
.                move oLRN to cgiOrderNum
.                move olNum to cgiListNum
.                packkey NXRFFLD from lstNum
.                call nxrfkey
.                if not over
..START PATCH 1.1 REPLACED LOGIC
..                    packkey mkey from NXRFMLR
..                    call nmlrkey
.                    packkey COMPFLD from NXRFMLR
.                    call COMPKEY
..END PATCH 1.1 REPLACED LOGIC
.    debug
.                    if over
.                        // check security
.                        if (thisSession.usertype != "A")
.                            if (clientFlag = "1")
.                                scan oLNum in validLists
.                                if not equal
.                                    // clear cgi variables
.                                    call clearCGIVariables
.                                else
.                                    // populate cgivariables here
.                                    call populateCGIVariables
.                                endif
.                            elseif (clientFlag = "1" and brokerFlag = "1")
.                                scan oLNum in validLists
.                                if not equal
.                                    // don't populate cgi variables
.                                    call clearCGIVariables
.                                else
.                                    // populate cgivariables here
.                                    call populateCGIVariables
.                                endif
.                            elseif (consultantFlag = "1")
.                                scan omlrnum in validMailers
.                                if not equal
.                                    // don't populate cgi variables
.                                    call clearCGIVariables
.                                else
.                                    if (consultantNum = oldConsultant)
.                                        call populateCGIVariables
.                                    endif
.                                    // don't populate cgi variables
.                                    call clearCGIVariables
.                                endif
.                            endif
.                        else
.                            call populateCGIVariables
.                        endif
.                    else
.                        // check security
.                        if (thisSession.usertype != "A")
.                            if (thisSession.userType = "O")
.//                                if (consultantNum = oldConsultant)
.                                    call populateCGIVariables
.//                                else
.//                                    call clearCGIVariables
.//                                endif
.                            elseif (omlrnum != thisMailer)
.                                scan oLNum in validLists
.                                if not equal
.                                    // don't populate cgi variables
.                                    move "1" to securityFlag
.                                else
.                                    // populate cgivariables here
.                                    call populateCGIVAriables
.                                endif
.                                // don't populate cgi variables
.                            else
.                                call populateCGIVariables
.                            endif
.                        else
.                            call populateCGIVariables
.                        endif
.                    endif
.
.                    // check date if we have a valid date range
.                    if (validRange = "Y")
.                        pack thisTimestamp from omDteY4,omDteM,omDteD
.                        if (thisTimestamp >= fromTimestamp and thisTimestamp <= toTimestamp)
.                            pack cgiMailerDate from omDteY4,omDteM,omDteD
.                            // apply status filters here
.                            if (cgiStatus = "0" and cgiLiveOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "p" and cgiPendOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "l" and cgiLCROrder != "Y")
.                                // do nothing
.                            elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
.                                // do nothing
.                            else
.                                // no filter for this status, so write to file
.                                count i in cgilistnum
.                                if not zero
.                                    write tempFile,seq;sortedOutput
.                                endif
.                            endif
.                        else
.                            // fall through
.                            //  to the endif, which will take us to a readks/kg
.                        endif
.                    else
.                       pack cgiMailerDate from omDteY4,omDteM,omDteD
.                        // apply status filters here
.                        if (cgiStatus = "0" and cgiLiveOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "p" and cgiPendOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "l" and cgiLCROrder != "Y")
.                            // do nothing
.                        elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
.                            // do nothing
.                        else
.                            // no filter for this status, so write to file
.                            count i in cgilistnum
.                            if not zero
.                                write tempFile,seq;sortedOutput
.                            endif
.                        endif
.                    endif
.                endif

.....................................
.Retrieve Default Company Name value
                    pack COMPFLD3,OMLRNUM
                    call COMPKEY3
.
                    move      C0,N1
                    if (thisSession.usertype = "A")                             .Admin Mode
                              call      populateCGIVariables
                              move      C1,N1
                    else
                              if (thisSession.userType = "O")                   .Consultant Mode
                                        reset     validMailers
                                        scan      omlrnum,validMailers
                                        if equal
.START PATCH 1.7 REPLACED LOGIC
.                                                 if (obrknum = thisBroker)
..                                                                                      stream *stdout,"<h1>'broker'</h1>"
..                                                                                      noreturn
..                                                                                      return
.                                                           call      populateCGIVariables
.                                                           move      C1,N1
..                                                else
..                                                          goto CheckList
.                                                 endif
..                                      else
..CheckList
..                                                reset     validLists
..                                                scan      oLNum,validLists
..                                                if equal
..                                                          call      populateCGIVariables
..                                                          move      C1,N1
..                                                endif
.                                       endif
.................................................................
                                                  call      Trim using obrknum
                                                  if (obrknum = thisBroker | (obrknum = "" & NCLTTYPE = "1"))
.test
.                                                 if (nclttype = "1")
.                                                 //NCLTTYPE is used to determine if Consultant can view records where their name is NOT on them
                                                            if (NCLTCONSULT <> "")        //blank field implies an over!!  Double Check.
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
                                                                                                    call      populateCGIVariables
                                                                                                    move      C1,N1
                                                                                          endif
                                                                                else
                                                                                          call      populateCGIVariables
                                                                                          move      C1,N1
                                                                                endif
                                                                      else
                                                                                call      populateCGIVariables
                                                                                move      C1,N1
                                                                      endif
                                                            endif
..                                                else
..                                                          goto CheckList
                                                  endif
.retest
                                        else
.CheckList
                                                  reset     validLists
                                                  scan      oLNum,validLists
                                                  if equal
                                                            call      populateCGIVariables
                                                            move      C1,N1
                                                  endif
                                        endif
.END PATCH 1.7 REPLACED LOGIC
                              elseif (thisSession.userType = "C")     .Client Mode
                                        if (omlrnum = thisMailer)
                                                  call      populateCGIVariables
                                                  move      C1,N1
.START PATCH 1.9 REPLACED LOGIC
.                                       elseif (readMethod <> "M" & readMethod <> "P")
                                        elseif (readMethod <> "-" & readMethod <> "+")
.END PATCH 1.9 REPLACED LOGIC
                                                  reset     validLists
                                                  scan      oLNum,validLists
                                                  if equal
                                                            call      populateCGIVariables
                                                            move      C1,N1
                                                  endif
                                        endif
                              elseif (thisSession.userType = "B")     .Broker Mode
                                        if (obrknum = thisBroker)
                                                  call      populateCGIVariables
                                                  move      C1,N1
                                        endif
                              endif
                    endif
                    if (N1 = C1)
.                    // check date if we have a valid date range
                              if (validRange = "Y")
                                        pack thisTimestamp from omDteY4,omDteM,omDteD
                                        if (thisTimestamp >= fromTimestamp and thisTimestamp <= toTimestamp)
                                                  pack cgiMailerDate from omDteY4,omDteM,omDteD
.                                                 // apply status filters here
.START PATCH 1.3 REPLACED LOGIC
.                                                 if (cgiStatus = "0" and cgiLiveOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "p" and cgiPendOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "l" and cgiLCROrder != "Y")
..                                                          // do nothing
.                                                 elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
..                                                          // do nothing
.                                                 else
..................................
                                                  if (cgiStatus = "0" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "B" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "Q" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "X" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "p" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "x" and cgiLiveOrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "l" and cgiLCROrder != "Y")
.                                                           // do nothing
                                                  elseif (cgiStatus = "z" and cgiLCROrder != "Y")
.                                                           // do nothing
                                                  else
.END PATCH 1.3 REPLACED LOGIC
.                                                           // no filter for this status, so write to file
                                                            count i in cgilistnum
                                                            if not zero
                                                                      write tempFile,seq;sortedOutput
.START PATCH 1.6 ADDED LOGIC
                                                                      add       C1,N10
.begin patch 2.2
                                                                      Call      Getstat
.end patch 2.2
.END PATCH 1.6 ADDED LOGIC
                                                            endif
                                                  endif
                                        else
.                                       // fall through
.                                       //  to the endif, which will take us to a readks/kg
                                        endif
                              else
                                        pack cgiMailerDate from omDteY4,omDteM,omDteD
.                                       // apply status filters here
.START PATCH 1.3 REPLACED LOGIC
.                                       if (cgiStatus = "0" and cgiLiveOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "B" and cgiBilledOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "Q" and cgiCancelBilledOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "X" and cgiCancelOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "p" and cgiPendOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "x" and cgiCancelPendOrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "l" and cgiLCROrder != "Y")
..                                                // do nothing
.                                       elseif (cgiStatus = "z" and cgiCancelLCROrder != "Y")
..                                                // do nothing
.                                       else
......................................
                                        if (cgiStatus = "0" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "B" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "Q" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "X" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "p" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "x" and cgiLiveOrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "l" and cgiLCROrder != "Y")
.                                                 // do nothing
                                        elseif (cgiStatus = "z" and cgiLCROrder != "Y")
.                                                 // do nothing
                                        else
.END PATCH 1.3 REPLACED LOGIC
.                                                 // no filter for this status, so write to file
                                                  count i in cgilistnum
                                                  if not zero
                                                            write tempFile,seq;sortedOutput
.START PATCH 1.6 ADDED LOGIC
                                                            add       C1,N10
.END PATCH 1.6 ADDED LOGIC
.begin patch 2.2
                                                            Call      Getstat
.end patch 2.2
                                                  endif
                                        endif
                              endif
                    endif
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.2 ADDED LOGIC
EndofLoop
.END PATCH 1.2 ADDED LOGIC
            switch readMethod
            case "L"
.                // nothing to do because AAM will never
.                //  give us a non-matching key
.START PATCH 1.2 REPLACED LOGIC
.                call nOrdKG
                call nOrdKGP
.END PATCH 1.2 REPLACED LOGIC
                break if over
            case "O"
                call nOrdKS
                break if  over
                break if (readKey != oLRN)
.START PATCH 1.9 REPLACED LOGIC
.            case "C"
.                call nOrdKS
.                break if over
.                break if (readKey != oCamp)
.            case "P"
.                // nothing to do because AAM will never
.                //  give us a non-matching key
..START PATCH 1.2 REPLACED LOGIC
..                call nOrdKG
.                call nOrdKGP
..END PATCH 1.2 REPLACED LOGIC
.                break if over
.            case "M"
.                // nothing to do because AAM will never
.                //  give us a non-matching key
..START PATCH 1.2 REPLACED LOGIC
..                call nOrdKG
.                call nOrdKGP
..END PATCH 1.2 REPLACED LOGIC
.                break if over
            case "-"
                call NORDKGP
                if over
                    move      "+",readMethod
.                   //Go back and read Mailer PO AAM
                    goto FirstRead
                endif
            case "+"
                call NORDKGP
                break if over
.END PATCH 1.9 REPLACED LOGIC
            endswitch
        repeat
    endif
.START PATCH 1.2 ADDED LOGIC
EndofLoop2
.END PATCH 1.2 ADDED LOGIC
.                                     stream *stdout,"<h1>'done'</h1>"
.                                     noreturn
.                                     return

    close tempFile
    clear selectiveSort

    debug

.START PATCH 1.9 REPLACED LOGIC
.    pack cmdString with tempFileName,",",sortFileName," -U,D,401-425"
    pack cmdString with tempFileName,",",sortFileName," -U,D,345-369"
.END PATCH 1.9 REPLACED LOGIC

    sort cmdString
    if over
.        // nothing in the file
        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
        noreturn
        return
    elseif (securityFlag = "1")
        stream *stdout,"<tr><td colspan=10 align=center class=datarow1><br>No results found.<br><br></td></tr>"
        noreturn
        return
    else
        close tempFile
        clear securityFlag
    endif

    move sortFileName to cgiDatasource
.START PATCH 1.6 ADDED LOGIC
          move      N10,str10
          call      FormatNumeric using str10,cgiRecNum
.begin patch 2.2
          call      Newcounters
.end patch 2.2
.START PATCH 1.7 REPLACED LOGIC
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
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.6 ADDED LOGIC
......test

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

.START PATCH 2.1 ADDED LOGIC
          if (OELCODE = "1")
                    move      "Rental",cgiexcrent
          elseif (OELCODE     = "2")
                    if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                              if (ORENT = "1")  .LCR Rental
                                        move      "Exc/Rent",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    else
                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                              if (N9 > 0)
                                        move      "Split",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    endif
          elseif (OELCODE     = "3")
                    if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                              if (ORENT = "1")  .LCR Rental
                                        move      "Exc/Rent",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    else
                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                              if (N9 > 0)
                                        move      "Split",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    endif
          else      .OELCODE = B1
                    move      "Rental",cgiexcrent
          endif
.END PATCH 2.1 ADDED LOGIC

        move oPPM into baseprice
        move "(ZZ9.99)" to cgibaseprice
        edit baseprice into cgibaseprice

        move oMlrKy into cgimailerkey
        move oSotCode into cgiselect
        pack cgimailerdate from omdtem,"/",omdted,"/",omdtey
        if (cgiMailerDate = "11/11/11")
            move "See Special Instructions" into cgiMailerDate
        elseif (cgiMailerDate = "00/00/00")
            move "As Soon As Possible" into cgiMailerDate
        endif
        pack cgiorderdate from oodtem,"/",oodted,"/",oodtey

.        // mailer name
        count i in cgimailername
        if zero
            move mcomp into cgimailername
        endif

.//        // get Offer Name here
        unpack oodnum into cwk4,cwk3
        packkey nofrfld from omlrnum,cwk3
        call nofrkey
        if over
            move "Not Available" into cgioffer
        else
            move ofDesc into cgioffer
        endif

.        // get Select here
        packkey nsel2fld from "1",olrn
        call nsel2key
        if over
            move o2des into cgiselect
        else
            move nsel2name into cgiselect
            move nsel2price into baseprice
            move "(ZZ,ZZ9.99)" to cgibaseprice
            edit baseprice into cgibaseprice
        endif

.        // get special instructions here
        packkey nspefld from olrn
        call nspekey
        if over
            move "NONE" into cgispecinstructions
        else
            append desc001 to cgispecinstructions
            append "<br>" to cgispecinstructions
            append desc002 to cgispecinstructions
            reset cgispecinstructions
            replace replf in cgispecinstructions
        endif

        packkey ndatfld from olnum
        call ndatkey
        if not over
            move oLstName into cgilistname
        else
            move "Not Available" into cgilistname
        endif

    endif

.START PATCH 1.5 ADDED LOGIC
          if (OSTAT = "l"     OR OSTAT = "z" OR OSTAT       = "p" OR OSTAT = "x") .LCR/Pending
                    call CGIDisplayFile using "statusrptlcrdetail.htt"
                    return
          endif
.END PATCH 1.5 ADDED LOGIC

.    // Get shipping information
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
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0014"
.END PATCH 1.8 ADDED LOGIC

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
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
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
DOLISTNUMBER routine

    call CGIRenderListNumberViewOrderInput using "",sessionIO.userCompany,sessionIO.userType

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
populateCGIVariables

    move oStat to cgistatus
    move oLstName to cgiListName
    move oLnum to cgiListNum
.START PATCH 1.9 REMOVED LOGIC
.    move oCamp to cgiCampaignNum
.    move nCmpCName to cgiCampaignName
.END PATCH 1.9 REMOVED LOGIC
    move oMlrPon to cgiPONum
    move oMlrNum to cgiMailerNum
    move oMlrKy to cgiMailerKey
.START PATCH 1.1 REPLACED LOGIC
.    move mcomp to cgiMailerName
    move compcomp to cgiMailerName
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.    clear cgiOrderName
.    append "<a href=#"javascript:shipSub('" to cgiOrderName
.    append oLRN to cgiOrderName
.    append "','" to cgiOrderName
.    append cgiMailerName to cgiOrderName
.    append "');#">" to cgiOrderName
.    append o1Des to cgiOrderName
.    append "</a>" to cgiOrderName
.    reset cgiOrderName
.......................................
          move      cgiMailerName,str55
.         // the apostrophe cause problems in javascript with errors
.         // so we remove them
          Loop
                    scan      "'",str55
                    until not equal
                    splice    "\þ",str55,1
          repeat
          reset     str55
          replace   "þ'",str55
          chop      str55
.
          clear     cgiOrderName
          append    "<a href=#"javascript:shipSub('",cgiOrderName     ."
          append    oLRN,cgiOrderName
          append    "','",cgiOrderName
          append    str55,cgiOrderName
          append    "');#">",cgiOrderName                   ."
          append    o1Des,cgiOrderName
          append    "</a>",cgiOrderName
          reset     cgiOrderName
.END PATCH 1.4 REPLACED LOGIC

    move omDteY to nwk4
    if  (nwk4 >= 100)
.        // do nothing, we assume this is a valid year if there
.        //  is anything in the century column
    elseif (nwk4 < 80)
        add "2000" to nwk4
    else
        add "1900" to nwk4
    endif
    move nwk4 to omDteY4

.START PATCH 2.1 ADDED LOGIC
          move      OQTY,cgiorderqty
          if (OELCODE = "1")
                    move      "Rental",cgiexcrent
          elseif (OELCODE     = "2")
                    if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                              if (ORENT = "1")  .LCR Rental
                                        move      "Exc/Rent",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    else
                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                              if (N9 > 0)
                                        move      "Split",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    endif
          elseif (OELCODE     = "3")
                    if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
                              if (ORENT = "1")  .LCR Rental
                                        move      "Exc/Rent",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    else
                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                              if (N9 > 0)
                                        move      "Split",cgiexcrent
                              else
                                        move      "Exchange",cgiexcrent
                              endif
                    endif
          else      .OELCODE = B1
                    move      "Rental",cgiexcrent
          endif
.END PATCH 2.1 ADDED LOGIC
    return
.///////////////////////////////////////////////////////////////////////////////

.START PATCH 1.4 ADDED LOGIC
.///////////////////////////////////////////////////////////////////////////////
getOrder
    call CGIDisplayFile using "statusrptviewlr.htt"
.START PATCH 1.8 ADDED LOGIC
          call      CGITrack using SID,"0015"
.END PATCH 1.8 ADDED LOGIC
return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
DISPLAYORDER routine buffer
.START PATCH 2.0 ADDED LOGIC
          if (cgiordernum = "done")
                    return
          endif
.END PATCH 2.0 ADDED LOGIC
.         // displays the Order Record (if any) in an IFRAME object
          move      "1",NORDPATH
          call      cgiParse using "cgiordernum",cgiordernum
          packkey   NORDFLD,cgiordernum
          call      NORDKEY
          if over
                    stream    *STDOUT,"<tr><td class='datarow1' colspan='3' align=center>Unable to create Order ":
                              "at this time</td></tr>"
                    return
          else
                    calls     "NORD002L;webGenerate" using cgiordernum
          endif
          pause     "5"
          getmode   *openuseip=#fileManagerSettings
          setmode   *openuseip=""

          pack      filename,"c:\work\pdf\",cgiordernum,"MWEB.PDF"
          loop
                    set       fileReady
                    trap      notYet if io
                    open      fOrder,filename,exclusive
                    trapclr   io
          repeat until (fileReady)
          setmode   *openuseip=#fileManagerSettings
          pack      filename from "/invoices/",cgiordernum,"MWEB.PDF"
          stream    *STDOUT,"<center><iframe src='",filename,"' width='100%' height='600'></iframe>"
          stream    *STDOUT,"<br><br>If your Order does not appear above, or you wish to view it in full screen mode, ":
                    "please click <a href='",filename,"' target='_blank'>here</a></center>"
.START PATCH 2.0 ADDED LOGIC
          move      "done",cgiordernum
.END PATCH 2.0 ADDED LOGIC
          return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
notYet
.    // the file we're looking for isn't ready for us to get exclusive access
.    //  to it yet.
    clear fileReady
    return
.///////////////////////////////////////////////////////////////////////////////
.END PATCH 1.4 ADDED LOGIC
.Begin patch 2.2
GetStat
.         stream    *STDOUT,"getstat Ostat =  ",Ostat," Oqty = ",oqty," OELcode = ",oelcode:
.                   ". <br><br>",newline2:
.                   "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."

          move oQty,qty
          
          if (OELCODE = "1")
                    if (OSTAT = "l" | OSTAT = "z")
                              add       qty,Lcrrent
                              add       c1,LcrCount                   
                    elseif    (ostat = "0")
                              add       qty,Ordrent
                              add       c1,OrderCount                 
                    elseif    (ostat = "B")
                              add       qty,Ordrent
                              add       c1,OrderCount                 
                              add       c1,BilledCOunt
                    endif
          elseif (OELCODE     = "2")
                    if (OSTAT = "l" | OSTAT = "z")
                              if (ORENT = "1")  .LCR Rental
                                        add       Qty,Lcrrent
                                        add       c1,LcrCount                   
                              else
                                        add       Qty,LcrExch
                                        add       c1,LcrCount                   
                              endif
                    else
                              if        (Ostat = "B" | Ostat = "0")
                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                                        if (N9 > 0)
                                                  add       Qty,Ordrent
                                                  Sub       n9,ordRent
                                                  Add       N9,ordExch
                                                  add       c1,OrderCount                 
                                        else
                                                  add       c1,OrderCount                 
                                                  add       Qty,OrdExch
                                        endif
                              endif     
                              if        (ostat = "B")
                              add       c1,BilledCount
                              endif
                    endif
          elseif (OELCODE     = "3")
                    if (OSTAT = "l" | OSTAT = "z")
                              if (ORENT = "1")  .LCR Rental
                                        add       Qty,Lcrrent
                                        add       c1,LcrCount                   
                              else
                                        add       Qty,LcrExch
                                        add       c1,LcrCount                   
                              endif
                    else
                              if        (Ostat = "B" | Ostat = "0")

                              call      Trim using OEXQTY
                              move      C0,N9
                              move      OEXQTY,N9
                              if (N9 > 0)
                                        add       Qty,Ordrent
                                        Sub       N9,ordRent
                                        Add       N9,ordExch
                                        add       c1,OrderCount                 
                              else
                                        add       c1,OrderCount                 
                                        add       Qty,OrdExch
                              endif
                              endif

                              if        (ostat = "B")
                              add       c1,BilledCount
                              endif
                    endif
          else      .OELCODE = B1
                              if        (Ostat = "B" | Ostat = "0")
                                        add       Qty,Ordrent
                                        add       c1,OrderCount                 
                              endif     
                              if        (ostat = "B")
                              add       c1,BilledCount
                              endif
          endif

.         move      BilledCount,str10
.         stream    *STDOUT,"getstat Ostat =  ",Ostat," Oqty = ",oqty," OELcode = ",oelcode:
.                   "BilledCount= ",str10,". <br><br>",newline2:
.                   "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."

.         stream    *STDOUT,"I am at Getstat =  ",str10:
.                   ". <br><br>",newline2:
.                   "Please contact the <a href=#"#/contact.htm#";#"> website administrator</a> to correct this problem."

          Return
.end patch 2.2

.///////////////////////////////////////////////////////////////////////////////
clearCGIVariables

    clear cgistatus
    clear cgiListName
    clear cgiListNum
.START PATCH 1.9 REPLACED LOGIC
.    clear cgiCampaignNum
.    clear cgiCampaignName
    clear cgiOther
.END PATCH 1.9 REPLACED LOGIC
    clear cgiPONum
    clear cgiMailerNum
    clear cgiMailerKey
    clear cgiMailerName
    clear cgiOrderName
    clear cgiMailerDate
    clear cgistatus

    clear sortedOutput

    return
.///////////////////////////////////////////////////////////////////////////////
.begin patch 2.2    
newcounters
.         //Format rental Qty
          Clear     CgiOrdRent
                    move "(Z,ZZZ,ZZZ,ZZ9)" to cgiordRent
          if (OrdRent > c0)
                    edit      Ordrent into cgiordrent
.                   move      ordrent,str45
.                   call      FormatNumeric using str45,cgiordrent
                    else
                    Move      C0,Ordrent
                    edit      Ordrent into cgiordrent
.                   move      c0,str45
.                   call      FormatNumeric using str45,cgiordrent
          endif
.         //Format Exchange Qty
          Clear     CgiOrdExch
          if (OrdExch > c0)
                    move      ordexch,str45
                    call      FormatNumeric using str45,cgiordexch
                    else
                    move      c0,str45
                    call      FormatNumeric using str45,cgiordexch
          call      Trim using cgiordexch
          endif

          move      c0,str10
          move      BIlledCount,str10
          call      FormatNumeric using str10,Cgibldcount

          if        (orderCount > c0)
          move      OrderCount,str10
          call      FormatNumeric using str10,CgiOrdCount
          Else
          Move      C0,str10
          call      FormatNumeric using str10,CgiOrdCount
          endif     
          
.         //Format rental Qty
          Clear     CgiLcrRent
          if (LcrRent > c0)
                    move      Lcrrent,str45
                    call      FormatNumeric using str45,cgiLcrrent
                    else
                    move      c0,str45
                    call      FormatNumeric using str45,cgiLcrrent
          call      Trim using cgiLcrrent         

          endif
.         //Format Exchange Qty
          Clear     CgiLcrExch
          if (LcrExch > c0)
                    move      Lcrexch,str45
                    call      FormatNumeric using str45,cgiLcrexch
                    else
                    move      c0,str45
                    call      FormatNumeric using str45,cgiLcrexch
          call      Trim using cgiLcrrent         
          endif
          move      LcrCount,str10
          call      FormatNumeric using str10,CgiLcrCount
          call      Trim using cgiLcrCount
          return
.end patch 2.2      

.///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
.//    include nxrfio.inc
.//    include ndatio.inc
    include ncmpio.inc
    INCLUDE NSEL2IO.INC
    include nshpio.inc
    include nspiio.inc
    include nofrio.inc
    include nspeio.inc
.///////////////////////////////////////////////////////////////////////////////

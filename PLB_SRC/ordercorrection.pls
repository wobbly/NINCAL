.///////////////////////////////////////////////////////////////////////////////
.//                                                                           //
.//   PROGRAM:   ordercorrection.pls                                                //
.//                                                                           //
.//    AUTHOR:                                          //
.//                                                                           //
.//      DATE:   19 NOV 2004                                                  //
.//                                                                           //
.// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
.//              All rights reserved.                                         //
.//                                                                           //
.//  PURPOSE:    CGI template application                                     //
.//                                                                           //
.//                1.1    13Nov2012  DLH replace Dundas.mailer                //
.// REVISION:    VER01   04 NOV 2004 BJACKSON    Created                      //
.//                                                                           //
.///////////////////////////////////////////////////////////////////////////////

    include apptop.inc
    include ncmpdd.inc
    include NSEL2DD.INC
    include nshpdd.inc
    include nspidd.inc
    include nofrdd.inc
    include nspedd.inc
.    include CGILibrary.inc
.    include sessionmgmt.inc
.///////////////////////////////////////////////////////////////////////////////
.mailBody                               dim 32768
fromAddress dim %260
.// TO DO: INSERT UDA HERE
cgifunction dim %1
cgiselectform DIM %20
.begin patch 1.1
.objMailer       automation
.end patch 1.1
cgidatarow          dim %1
cgiordernum             dim     %10
cgibilledorder          dim     %1
cgibilledordertext      dim     %15
cgitype                 dim     %10
cgimailernum            dim     %50
cgilistnum              dim     %50
cgiquantity             dim     %12
cgiselections           dim     %12
cgimaildate             dim     %10
cginet                  dim     %10
cgiformat               dim     %50
cgilistownerdisc        dim     %50
cgilistowner            dim     %50
cgimailercontact        dim     %50
cgiservicebureau        dim     %50
cgibase                 dim     %50
cgiselect               dim     %150
cgireturndate           dim     %30
cgirunningcharge        dim     %12
cgiexstatadj            dim     %10
cgilsitowner            dim     %36
cgipo                   dim     %36
cgistatus               dim     %30
cgishipto               dim     %30
cgispecial              dim     %60
cginotes                dim     %250
cgiemailaddress1        dim     %250
cgiDataSource           dim     %260
cgiLinks                dim     %260

.//cancellation form
cgicharges                    dim       %250
cgiwhopays                    dim       %50
cgidiscussedwith    dim       %50
cgiReason           dim       %250
cgiyesnocharge          dim     %10
dataSource              dim     260
sortFileName            dim     260
tempFileName            dim     260
selectiveSort           dim     260
cmdString               dim     260
cgiOtherUnknown         dim     %1
cgiReadPos              dim     %10
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
thisTimestamp           dim     8
validLists              dim     260
validMailers            dim     260


#fileManagerSettings    dim     260

.//sortedOutput            varlist cgiOrderName:           //001-260
.//                                cgilistnum:             //261-266
.//                                cgimailernum:           //267-270
.//                                cgimailername:          //271-320
.//                                cgicampaignnum:         //321-326
.//                                cgicampaignname:        //327-376
.//                                cgiponum:               //377-388
.//                                cgimailerkey:           //389-400
.//                                cgimailerdate:          //401-425
.//                                cgistatus               //426-445
.//
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
readMethod          dim     1   // O,C,P,M
todate              dim     8
todd                dim     2
tomm                dim     2
toyy                dim     4
validRange          dim     1
yyyy                dim     4
thisShortMailerNo   dim     4

i                   integer 4

httName             dim     45
htttemplate         dim     45
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

EXSPL               form    9
excode                  init    "23"
cgistatustext       dim      %60
.///////////////////////////////////////////////////////////////////////////////
start

.    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

.    // if the session isn't valid for some reason, this will send us to an
.    //  error page -  if this page requires admin access, change the call
.    //  to this: call sessionValidate using "ADMIN"
.    //call sessionValidate

.    // this will send the static top part of the HTML document
    call CGIRenderHeader

.    ///////////////////////////////////////////////////////////////////////////
.    // TO DO: insert all of your CGI parse statements here.  If any variable is
.    //         supposed to filled out but you get an over, empty string, etc.,
.    //         append an error string (in HTML) to the variable errorMsg
.    ///////////////////////////////////////////////////////////////////////////
.    //call CGIParse using "SID",SID
.    //if over
.    //    append "Session ID is missing!<br>" to errorMsg
.    //endif    
    call CGIParse using "cgiemailaddress1",cgiemailaddress1       
.    //   if over
.    //    append "NO Email address!<br>" to errorMsg
.    //.  call      ActionDisplay
.    //.  endif    
    call CGIParse using "cgifunction",cgiFunction
    call cgiParse using "cgiselectform",cgiselectform    
    call CGIParse using "cgiordernum",cgiOrderNum
    call CGIParse using "cgibilledorder",cgibilledorder 
    call CGIParse using "cgibilledordertext",cgibilledordertext     
    call CGIParse using "cgitype",cgitype
    call CGIParse using "cgimailernum",cgimailernum
    call CGIParse using "cgilistnum",cgilistnum 
    call CGIParse using "cgiquantity",cgiquantity 
    call CGIParse using "cgiselections",cgiselections
    call CGIParse using "cgimaildate",cgimaildate 
    call CGIParse using "cginet",cginet  
    call CGIParse using "cgiformat",cgiformat      
    call CGIParse using "cgilistownerdisc",cgilistownerdisc 
    call CGIParse using "cgilistowner",cgilistowner
    call CGIParse using "cgimailercontact",cgimailercontact
    call CGIParse using "cgiservicebureau",cgiservicebureau
    call CGIParse using "cgibase",cgibase  
    call CGIParse using "cgiselect",cgiselect
    call CGIParse using "cgireturndate",cgireturndate     
    call CGIParse using "cgirunningcharge",cgirunningcharge 
    call CGIParse using "cgiexstatadj",cgiexstatadj 
    call CGIParse using "cgilsitowner",cgilsitowner
    call CGIParse using "cgipo",cgipo 
    call CGIParse using "cgistatus",cgistatus  
    call CGIParse using "cgistatustext",cgistatustext      
    call CGIParse using "cgishipto",cgishipto  
    call cgiParse using "cgispecial",cgispecial 
    call cgiParse using "cginotes",cginotes
    call cgiParse using "cgicharges",cgicharges
    call cgiParse using "cgiyesnocharge",cgiyesnocharge    
    call cgiParse using "cgiwhopays",cgiwhopays
    call cgiParse using "cgidiscussedwith",cgidiscussedwith
    call cgiParse using "cgireason",cgireason
.    // based upon the "function" value, we need to decide whether to render the form or
.    //  process it's data
  

    switch cgiFunction
    case "0"
.        // display a form
        move "ordercorrectionlr.htt" to httName
        call actionDisplay
    case "1"
    
.        // process the form results
        move "ordercorrectionlr.htt" to httName        
        call actionProcess1
        
    case "2"
.        // Process a form
        move "ordercorrection.htt" to httName
.        call CGIDisplayFile using httName
.        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,cgistatustext,"<br><br></td></tr>"
.        stream *stdout,cgiemailaddress1 
.        stop
.//        call getdata
        call actionProcess
    case "3"
.        // display a form
.//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,"<br><br></td></tr>"
.//        stream *stdout,cgiemailaddress1        
.//        move "ordercorrectionresponse.htt" to httName
        move "ordercancellation.htt" to httName
        call actionProcess      
    default
.        // display a form
        move "ordercorrectionlr.htt" to httName        
        call actionDisplay
    endswitch
.    // this will send the static bottom part of the HTML document
    call CGIRenderFooter

    stop
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
actionDisplay

.    // this will display the template (HTT) document we've created for this
.    //  page
    call CGIDisplayFile using httName

    return
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
actionProcess

.    // logic to process the data that we've parsed from the CGI environment
.//    if (cgiDatasource != "")
.//        // We already have a datasource and we're paging forward or back.  We
.//        //  don't need to validate anything.
.//        move "ordercorrectionresponse.htt" to httName        
.//        call CGIDisplayFile using httName
.//        return
.//    endif
    call emailAdmin    

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
.////////////////////////////////////////////////////////////////////////////////
.///////////////////////////////////////////////////////////////////////////////
ActionProcess1

.    // logic to process the data that we've parsed from the CGI environment
.//    if (cgiDatasource != "")
.        // We already have a datasource and we're paging forward or back.  We
.        //  don't need to validate anything.
.//        call CGIDisplayFile using httname
.//        return
.//    endif

.    // We are going to have to build up a data file, so we have to validate
.    //  the user inputs first.
    clear readKey,readMethod
    if (cgiOrderNum = "")
        move "Please enter an order number. " to errorMsg
        call actionDisplay
        return      
.//    else
    elseif (cgiemailaddress1="")
        move "Please enter your email address. " to errorMsg
        call actionDisplay
        return          
    endif
.//    squeeze cgiemailaddress1,cwk260
.//    clear cgiemailaddress1
.//    pack cgiemailaddress1 with cwk260,"@nincal.com"
.//     "@nincal.com" to cgiemailaddress1
.//    reset cgiemailaddress1
.//    scan "@nincal.com" in cgiemailaddress1
.//    if not equal
.//        move "Please enter your full email address.  Address must be within the nincal.com domain. " to errorMsg
.//        call actionDisplay
.//        return              
.//    else
.//       reset cgiemailaddress1    
.//    endif
    call datasource
.// implement different display files here
    if (cgiselectform = "Correction Form")
              move "ordercorrection.htt" to htttemplate        
    else
              move "ordercancellation.htt" to htttemplate            
    endif
    call CGIDisplayFile using htttemplate
.//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,cgistatustext,"<br><br></td></tr>"
.//        stream *stdout,cgiemailaddress1       
    
    move buffer to cwk32768
    call cgiDisplay using cwk32768
    stream *STDOUT,cwk32768
.//    if (cgiDataRow = "1")
.//        move "" to cgiDataRow
.//    else
.//        move "1" to cgiDataRow
.//    endif 
    return
.///////////////////////////////////////////////////////////////////////////////
DataSource
.///////////////////////////////////////////////////////////////////////////////

.    // open all our files in read-only mode
    open compFList,read
    move c1 to compFlag

    load ndatName using ndatPath from ndatNme1,ndatNme2
    open ndatFList,read
    move c1 to nDatFlag

    move "1" to nordPath
    load nordName using nordPath from nordNme1,nordNme2,nordNme3
    open nordFile,nordName,read
    move nordPath to nordFlag
.///////////////////////////////////////////////////////////////////////////////  

.//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,"<br><br></td></tr>"
.//        stream *stdout,cgiemailaddress1   
.//        return       
    getmode *prepuseip=#fileManagerSettings
    setmode *prepuseip=""
.//    prep tempFile,tempFileName,exclusive
    setmode *prepuseip=#fileManagerSettings
    move cgiOrderNum to readkey
    move readKey to nOrdFld
    move "1" to nOrdPath
    call nOrdKey
    if over
.        // no results found
        move "Order Number does not Exist." to errorMsg
        call actionDisplay
        noreturn
        return             
    else
        if (ostat <> "B" and ostat <> "0")
                  move "Please enter a valid order number." to errorMsg
                  call actionDisplay
                  noreturn
                  return              
        endif    
.//        move olNum to cgiListNum
        squeeze cgiemailaddress1,cwk260
        clear   cgiemailaddress1
        uppercase   cwk260,cwk260
        scan "@NINCAL.COM",cwk260
        if equal
          reset cwk260
                  move cwk260,cgiemailaddress1                 
        else
                  pack cgiemailaddress1 with cwk260,"@NINCAL.COM"        
        endif
        clear cgilistnum
        packkey ndatfld from olnum
        call ndatkey
        if not over
                    append oLnum to cgiListNum
                    append " - " to cgiListNum
                    replace "#' " in oLstName                         
                    append oLstName to cgiListNum
                    reset cgiListNum        
.//               move oLstName into cgilistnum
        else
                  move "Not Available" into cgilistnum
        endif                
....................................
.Retrieve Default Company Name value
        pack COMPFLD3,OMLRNUM
          call COMPKEY3
          move C0,N1
          call populateCGIVariables
    endif
    return          
.//////////////////////////////////////////////////////////////////////////////
emailAdmin

.    // Email the Admin
    clear MailBody
    if (cgiordernum = "")
              move "*LR Required" to errorMsg
              call actionDisplay
              return    
    endif
    append "LR:                           " to MailBody
    append cgiOrderNum to MailBody
    append newline2 to MailBody
    append newline2 to MailBody    
   
    if (cgiFunction    = "2")
          if (cgimailernum = "")
                    move "*Mailer Info Required" to errorMsg
                    call actionDisplay
                    return    
          endif    
          append "Mailer:                       " to MailBody
          append cgimailernum to MailBody
          append newline2 to MailBody
          append newline2 to MailBody        
          if (cgilistnum = "")
                    move "*List Number Required" to errorMsg
                    call actionDisplay
                    return    
          endif        
          append "List:                         " to MailBody
          append cgilistnum to MailBody
          append newline2 to MailBody
          append newline2 to MailBody        
          if (cgiquantity <> "")    
                    append "Quantity:                     " to MailBody
                    append cgiquantity  to MailBody
                    append newline2 to MailBody
              append newline2 to MailBody         
          endif
          if (cgiselections <> "")        
                    append "Selections:                   " to MailBody
                    append cgiselections to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif    
          if (cgimaildate <> "")            
                    append "Mail Date:                    " to MailBody
              append cgimaildate  to MailBody
              append newline2 to MailBody
              append newline2 to MailBody            
          endif    
          if (cginet <> "")                
                    append "Net:                          " to MailBody
                    append cginet to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
.         if (cgiformat <> "")                    
.                   append "Format/Pricing:               " to MailBody
.                   append cgiformat to MailBody
.                   append newline2 to MailBody
.                   append newline2 to MailBody 
.         else
.                   move "*Format/Pricing Field is required" to errorMsg
.                   call actionDisplay
.                   return              
.         endif    
          if (cgilistownerdisc = "")
                    move "*Discussed with List Owner Info Required" to errorMsg
                    call actionDisplay
                    return    
          endif     
          append "Discussed with List Owner:    " to MailBody
        append cgilistownerdisc  to MailBody
        append newline2 to MailBody
        append newline2 to MailBody        
        if (cgimailercontact = "")
                    move "*Mailer Contact Info Required" to errorMsg
                    call actionDisplay
                  return    
          endif     
          append "Mailer Contact:               " to MailBody
          append cgimailercontact to MailBody
          append newline2 to MailBody
          append newline2 to MailBody        
          if (cgiservicebureau <> "")    
                    append "Service Bureau:               " to MailBody
                    append cgiservicebureau to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody                       
          endif
          if (cgibase <> "")        
                    append "Base Price:                   " to MailBody
                    append cgibase  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif    
          if (cgiselect     <> "")            
                    append "Select Price:                 " to MailBody
                    append cgiselect to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif    
          if (cgireturndate <> "")                
                    append "Return Date:                  " to MailBody
                    append cgireturndate   to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
        if (cgirunningcharge <> "")                    
                    append "Running Charge:               " to MailBody
                    append cgirunningcharge  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgiexstatadj <> "")                        
                    append "Adj Exstat:                   " to MailBody
                    append cgiexstatadj  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgilistowner <> "")                            
                    append "List Owner:                   " to MailBody
                    append cgilistowner to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgipo <> "")                                
                    append "P.O.:                         " to MailBody
                    append cgipo  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgishipto <> "")                                    
                    append "Ship To:                      " to MailBody
                    append cgishipto    to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgispecial <> "")                                    
                    append "Special Instructions:         " to MailBody
                    append cgispecial  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cginotes <> "")                                        
                    append "Notes to Order Correction:          " to MailBody
                    append cginotes to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif


          if (cgistatus = "No Change")            
                    move "*Exchange/Rental Status has not been set.  Please set appropriate value." to errorMsg
                    call actionDisplay
                    return              
          else
                    append "Change Status to:             " to MailBody         
                    append cgistatus to MailBody
                    append newline2 to MailBody    
                    append newline2 to MailBody 
          endif
.//    if (cgibilledorder = "")
.//           move "*Billing Status Required" to errorMsg
.//           call actionDisplay
.//           return    
.//    endif    
        append "Billed:                       " to MailBody
        if (cgibilledordertext = "Billed")
.//      append cgibilledordertext  to MailBody
                    append "Y"  to MailBody
          else
                    append "N"  to MailBody       
        endif
          append newline2 to MailBody    
          append newline2 to MailBody             

          if (cgitype = "No Action")
                    move "*Please set whether this Correction is a System or Reprint." to errorMsg
                    call actionDisplay
                    return                        
          else    
                    append "Action:                       " to MailBody         
                    append cgitype to MailBody
                    append newline2 to MailBody        
                    append newline2 to MailBody        
          endif
                    
    else
        append "Billed:                       " to MailBody
        if (cgibilledordertext = "Billed")
                    append "Y"  to MailBody
        else
                    append "N"  to MailBody       
        endif
        append newline2 to MailBody    
        append newline2 to MailBody      
          if (cgimailernum = "")
                    move "*Mailer Info Required" to errorMsg
                    call actionDisplay
                    return    
          endif    
          append "Mailer:                       " to MailBody
          append cgimailernum to MailBody
          append newline2 to MailBody
          append newline2 to MailBody        
          if (cgilistnum = "")
                    move "*List Number Required" to errorMsg
                    call actionDisplay
                    return    
          endif        
          append "List:                         " to MailBody
          append cgilistnum to MailBody
          append newline2 to MailBody
          append newline2 to MailBody        
          if (cgiquantity <> "")    
                    append "Quantity:                     " to MailBody
                    append cgiquantity  to MailBody
                    append newline2 to MailBody
              append newline2 to MailBody         
          endif
          if (cgimaildate <> "")            
                    append "Mail Date:                    " to MailBody
                    append cgimaildate  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody            
          endif   
          if ((cgiyesnocharge = "Yes") and (cgicharges = ""))
                    move "*Please specify the charges or select No if there are none." to errorMsg
                    call actionDisplay
                    return 
          else
                    if (cgicharges <> "")
                              append "Charges:                      " to MailBody
                              append cgicharges to MailBody
                              append newline2 to MailBody
                              append newline2 to MailBody             
                              if (cgiwhopays = "")
                                        move "*Please specify Who Pays or select No Charges if there are none." to errorMsg
                                        call actionDisplay
                                        return                        
                              else
                                        append "Who Pays:                      " to MailBody
                                        append cgiwhopays to MailBody
                                        append newline2 to MailBody
                                        append newline2 to MailBody                                           
                              endif
                    else
                              if (cgiyesnocharge = "No")
                                        append "Charges:                      " to MailBody
                                        append "No Charges" to MailBody                                       
                                        append newline2 to MailBody
                                        append newline2 to MailBody                                 
                              endif
                    endif     
          endif
          if (cgidiscussedwith <> "")                                        
                    append "Discussed with:               " to MailBody
                    append cgidiscussedwith to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif      
          if (cgireason <> "")                                        
                    append "Reason:                       " to MailBody
                    append cgireason to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif
          if (cgiexstatadj <> "")                        
                    append "Adj Exstat:                   " to MailBody
                    append cgiexstatadj  to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif     
          if (cginotes <> "")                                        
                    append "Notes to Order Correction:          " to MailBody
                    append cginotes to MailBody
                    append newline2 to MailBody
                    append newline2 to MailBody             
          endif     
    endif
          reset MailBody
.    // create the mailer object
.begin patch 1.1
.    trap noMailer if object
.        create objMailer,class="Dundas.Mailer"
.    trapclr object
.end patch 1.1

.    // get Admin email addresses
.;    packkey fUser4IKey from "A"
.;    call fUser4IRead
.;    loop
.;        call fUser4IReadKS
.;        until over
.;        while (fUserIO.userType = "A")
.;        objMailer.TOs.Add using fUserIO.email
.;    repeat
.//    objMailer.TOs.Add using "dherric@nincal.com"
.    objMailer.TOs.Add using "dataentry@nincal.com"    
.begin patch 1.1
          pack      Mailto from "OrderCorrection@nincal.com,",cgiemailaddress1
.    objMailer.TOs.Add using "OrderCorrection@nincal.com"    

.    objMailer.TOs.Add using cgiemailaddress1
.end patch 1.1
    if (cgiFunction    = "2")    
              if (cgibilledordertext = "Billed")
.begin patch 1.1
                        pack  mailcc,"gspranz@nincal.com"    
.                        objMailer.TOs.Add using "gspranz@nincal.com"    
.end patch 1.1

.                       objMailer.TOs.Add using "gbarlaa@nincal.com"                
              endif
    else
              if ((cgicharges <> "") or (cgibilledordertext = "Billed")) 
.begin patch 1.1
                        pack  mailcc,"gspranz@nincal.com"    
.                        objMailer.TOs.Add using "gspranz@nincal.com"    
.end patch 1.1
.                       objMailer.TOs.Add using "gbarlaa@nincal.com"                
              endif    
    endif
.    // Get from address - plbwin.ini
.//    pack cwk260 from "nin;ADMIN_EMAIL"
.//    clock ini into cwk260
.//    if over
.//        move "Error reading email address from .ini file!" to errorMsg
.//        call actionDisplay
.//        return
.//    endif
.    //move cwk260 into fromAddress
.begin patch 1.1
          pack      mailfrom,"creques@nincal.com"
.          move cgiemailaddress1 into fromAddress    
.end patch 1.1

.    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
.begin patch 1.1
.    objMailer.SMTPRelayServers.Add using cwk260
          pack      mailserver from cwk260
.end patch 1.1

.    //  email settings
.begin patch 1.1
.    setprop objMailer,*FromAddress=fromAddress
    if (cgiFunction    = "2")    
               pack mailsubjct,"CORRECTION"     
.              setprop objMailer,*Subject="CORRECTION"
    else
               pack mailsubjct,"CANCELLATION"
.          setprop objMailer,*Subject="CANCELLATION"
    endif
.    setprop objMailer,*Body=MailBody
.end patch 1.1

.    // send the email
.begin patch 1.1
.    trap noSend if object
.        objMailer.SendMail
.    trapclr object
.
.    destroy objMailer
          Move      "Y",Mailtrace

          Call      SendMail
.end patch 1.1

.    // finished sending Admin email
    call cgiDisplayFile using "ordercorrectionresponse.htt"    
    return
.//////////////////////////////////////////////////////////////////////////////
populateCGIVariables


    if (ostat = "B")
.//       move "Y" to cgibilledorder              
          move "Billed" to cgibilledordertext     
    elseif (ostat = "0")
          move "Live Order" to cgibilledordertext
.//       move "N" to cgibilledorder                        
    else
          move "Not an Order" to cgibilledordertext    
    endif
    scan oelcode in excode
    if  equal
        move "Exchange" to cgiStatus
          move OEXQTY to EXSPL
          if (EXSPL > 0)
          move "Split" to cgiStatus
        endif
    else
        move "Rental" to cgiStatus    
    endif
    move cgistatus to cgistatustext
.//    append oLnum to cgiListNum
.//    append " - " to cgiListNum    
.//    append oLstName to cgiListNum
.//    reset cgiListNum
    append oMlrNum to cgiMailerNum
    append " - " to cgiMailerNum
    replace "#' " in compcomp    
    append Compcomp to cgiMailerNum   
    reset cgiMailerNum    
    move oLRN to cgiOrderNum
    return
.///////////////////////////////////////////////////////////////////////////////
noMailer

.    // the mailer object has not been installed properly
    stream *stdout,"<hr><h3>An error has occurred while accessing this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
.///////////////////////////////////////////////////////////////////////////////

.///////////////////////////////////////////////////////////////////////////////
noSend

.    // the mailer object couldn't send the message
    stream *stdout,"<hr><h3>An error has occurred while connecting to this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
.///////////////////////////////////////////////////////////////////////////////


    include appbottom.inc
.    include nxrfio.inc
.    include ndatio.inc
    include ncmpio.inc
    INCLUDE NSEL2IO.INC
    include nshpio.inc
    include nspiio.inc
    include nofrio.inc
    include nspeio.inc
.///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   ordercancel.pls                                                //
//                                                                           //
//    AUTHOR:                                          //
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
//    include ncmpdd.inc
//    include NSEL2DD.INC
//    include nshpdd.inc
//    include nspidd.inc
//    include nofrdd.inc
//    include nspedd.inc
      //include CGILibrary.inc
//      include sessionmgmt.inc
///////////////////////////////////////////////////////////////////////////////
mailbody				dim 32768
fromAddress dim %260
// TO DO: INSERT UDA HERE
cgifunction dim %1
cgiselectform DIM %20
objMailer       automation
cgidatarow	dim %1
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
cgiselect               dim     %25
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

//cancellation form
cgicharges		dim	%250
cgiwhopays		dim	%50
cgidiscussedwith	dim	%50
cgiReason		dim	%50
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

//sortedOutput            varlist cgiOrderName:           //001-260
//                                cgilistnum:             //261-266
//                                cgimailernum:           //267-270
//                                cgimailername:          //271-320
//                                cgicampaignnum:         //321-326
//                                cgicampaignname:        //327-376
//                                cgiponum:               //377-388
//                                cgimailerkey:           //389-400
//                                cgimailerdate:          //401-425
//                                cgistatus               //426-445
//
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
excode		    init    "23"
cgistatustext       dim      %60
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // if the session isn't valid for some reason, this will send us to an
    //  error page -  if this page requires admin access, change the call
    //  to this: call sessionValidate using "ADMIN"
    //call sessionValidate

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    ///////////////////////////////////////////////////////////////////////////
    // TO DO: insert all of your CGI parse statements here.  If any variable is
    //         supposed to filled out but you get an over, empty string, etc.,
    //         append an error string (in HTML) to the variable errorMsg
    ///////////////////////////////////////////////////////////////////////////
    //call CGIParse using "SID",SID
    //if over
    //    append "Session ID is missing!<br>" to errorMsg
    //endif    
    call CGIParse using "cgiemailaddress1",cgiemailaddress1       
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
    // based upon the "function" value, we need to decide whether to render the form or
    //  process it's data
  

    switch cgiFunction
    case "0"
        // display a form
        move "ordercorrectionlr.htt" to httName
        call actionDisplay
    case "1"
    
        // process the form results
        move "ordercorrectionlr.htt" to httName        
        call actionProcess1
        
    case "2"
        // Process a form
        move "ordercorrection.htt" to httName
//        call CGIDisplayFile using httName
//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,cgistatustext,"<br><br></td></tr>"
//        stream *stdout,cgiemailaddress1 
//        stop
//        call getdata
        call actionProcess
    case "3"
        // display a form
//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,"<br><br></td></tr>"
//        stream *stdout,cgiemailaddress1        
//        move "ordercorrectionresponse.htt" to httName
        move "ordercancellation.htt" to httName
        call actionProcess      
    default
        // display a form
        move "ordercorrectionlr.htt" to httName        
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
//    if (cgiDatasource != "")
//        // We already have a datasource and we're paging forward or back.  We
//        //  don't need to validate anything.
//        move "ordercorrectionresponse.htt" to httName        
//        call CGIDisplayFile using httName
//        return
//    endif
    call emailAdmin    

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
////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
ActionProcess1

    // logic to process the data that we've parsed from the CGI environment
//    if (cgiDatasource != "")
        // We already have a datasource and we're paging forward or back.  We
        //  don't need to validate anything.
//        call CGIDisplayFile using httname
//        return
//    endif

    // We are going to have to build up a data file, so we have to validate
    //  the user inputs first.
    clear readKey,readMethod
    if (cgiOrderNum = "")
        move "Please enter an order number. " to errorMsg
        call actionDisplay
        return      
//    else
    elseif (cgiemailaddress1="")
        move "Please enter your email address. " to errorMsg
        call actionDisplay
        return          
    endif
//    squeeze cgiemailaddress1,cwk260
//    clear cgiemailaddress1
//    pack cgiemailaddress1 with cwk260,"@nincal.com"
//     "@nincal.com" to cgiemailaddress1
//    reset cgiemailaddress1
//    scan "@nincal.com" in cgiemailaddress1
//    if not equal
//        move "Please enter your full email address.  Address must be within the nincal.com domain. " to errorMsg
//        call actionDisplay
//        return              
//    else
//    	reset cgiemailaddress1    
//    endif
    call datasource
// implement different display files here
    if (cgiselectform = "Correction Form")
	    move "ordercorrection.htt" to htttemplate        
    else
	    move "ordercancellation.htt" to htttemplate            
    endif
    call CGIDisplayFile using htttemplate
//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,cgistatustext,"<br><br></td></tr>"
//        stream *stdout,cgiemailaddress1       
    
    move buffer to cwk32768
    call cgiDisplay using cwk32768
    stream *STDOUT,cwk32768
//    if (cgiDataRow = "1")
//        move "" to cgiDataRow
//    else
//        move "1" to cgiDataRow
//    endif 
    return
///////////////////////////////////////////////////////////////////////////////
DataSource
///////////////////////////////////////////////////////////////////////////////

    // open all our files in read-only mode
    open compFList,read
    move c1 to compFlag

    load ndatName using ndatPath from ndatNme1,ndatNme2
    open ndatFList,read
    move c1 to nDatFlag

    move "1" to nordPath
    load nordName using nordPath from nordNme1,nordNme2,nordNme3
    open nordFile,nordName,read
    move nordPath to nordFlag
///////////////////////////////////////////////////////////////////////////////  

//        pack cgiemailaddress1 with "<tr><td colspan=10 align=center class=datarow1><br>",cgifunction,"<br><br></td></tr>"
//        stream *stdout,cgiemailaddress1   
//        return       
    getmode *prepuseip=#fileManagerSettings
    setmode *prepuseip=""
//    prep tempFile,tempFileName,exclusive
    setmode *prepuseip=#fileManagerSettings
    move cgiOrderNum to readkey
    move readKey to nOrdFld
    move "1" to nOrdPath
    call nOrdKey
    if over
        // no results found
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
//        move olNum to cgiListNum
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
//	        move oLstName into cgilistnum
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
//////////////////////////////////////////////////////////////////////////////
emailAdmin

    // Email the Admin
    clear mailBody
    if (cgiordernum = "")
	    move "*LR Required" to errorMsg
	    call actionDisplay
	    return    
    endif
    append "LR:                           " to mailBody
    append cgiOrderNum to mailBody
    append newline2 to mailBody
    append newline2 to mailBody    
   
    if (cgiFunction    = "2")
    	if (cgimailernum = "")
		move "*Mailer Info Required" to errorMsg
		call actionDisplay
		return    
    	endif    
    	append "Mailer:                       " to mailBody
    	append cgimailernum to mailBody
    	append newline2 to mailBody
    	append newline2 to mailBody        
    	if (cgilistnum = "")
		move "*List Number Required" to errorMsg
		call actionDisplay
		return    
    	endif        
    	append "List:                         " to mailBody
    	append cgilistnum to mailBody
    	append newline2 to mailBody
    	append newline2 to mailBody        
    	if (cgiquantity <> "")    
		append "Quantity:                     " to mailBody
		append cgiquantity  to mailBody
		append newline2 to mailBody
    	    append newline2 to mailBody    	
    	endif
    	if (cgiselections <> "")        
    		append "Selections:                   " to mailBody
    		append cgiselections to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif    
    	if (cgimaildate <> "")            
		append "Mail Date:                    " to mailBody
    	    append cgimaildate  to mailBody
    	    append newline2 to mailBody
    	    append newline2 to mailBody            
    	endif    
    	if (cginet <> "")                
    		append "Net:                          " to mailBody
    		append cginet to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgiformat <> "")                    
		append "Format/Pricing:               " to mailBody
		append cgiformat to mailBody
		append newline2 to mailBody
    		append newline2 to mailBody 
    	else
		move "*Format/Pricing Field is required" to errorMsg
		call actionDisplay
		return     	
    	endif    
    	if (cgilistownerdisc = "")
		move "*Discussed with List Owner Info Required" to errorMsg
		call actionDisplay
		return    
	endif     
	append "Discussed with List Owner:    " to mailBody
        append cgilistownerdisc  to mailBody
        append newline2 to mailBody
        append newline2 to mailBody        
        if (cgimailercontact = "")
		move "*Mailer Contact Info Required" to errorMsg
 		call actionDisplay
    	        return    
	endif     
	append "Mailer Contact:               " to mailBody
	append cgimailercontact to mailBody
	append newline2 to mailBody
	append newline2 to mailBody        
	if (cgiservicebureau <> "")    
		append "Service Bureau:               " to mailBody
	    	append cgiservicebureau to mailBody
	    	append newline2 to mailBody
		append newline2 to mailBody        	    	
	endif
	if (cgibase <> "")        
		append "Base Price:                   " to mailBody
	    	append cgibase  to mailBody
	    	append newline2 to mailBody
	    	append newline2 to mailBody        	
	endif    
	if (cgiselect     <> "")            
	    	append "Select Price:                 " to mailBody
	    	append cgiselect to mailBody
	    	append newline2 to mailBody
	    	append newline2 to mailBody        	
	endif    
	if (cgireturndate <> "")                
	    	append "Return Date:                  " to mailBody
	    	append cgireturndate   to mailBody
	    	append newline2 to mailBody
	    	append newline2 to mailBody        	
	endif
        if (cgirunningcharge <> "")                    
    		append "Running Charge:               " to mailBody
    		append cgirunningcharge  to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgiexstatadj <> "")                        
    		append "Adj Exstat:                   " to mailBody
    		append cgiexstatadj  to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgilistowner <> "")                            
    		append "List Owner:                   " to mailBody
    		append cgilistowner to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgipo <> "")                                
    		append "P.O.:                         " to mailBody
    		append cgipo  to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgishipto <> "")                                    
    		append "Ship To:                      " to mailBody
    		append cgishipto    to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgispecial <> "")                                    
    		append "Special Instructions:         " to mailBody
    		append cgispecial  to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cginotes <> "")                                        
    		append "Notes to Data Entry:          " to mailBody
    		append cginotes to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif


    	if (cgistatus = "No Change")    	
		move "*Exchange/Rental Status has not been set.  Please set appropriate value." to errorMsg
		call actionDisplay
		return     	
    	else
    		append "Change Status to:             " to mailBody    	
    		append cgistatus to mailBody
    		append newline2 to mailBody    
    		append newline2 to mailBody 
    	endif
//    if (cgibilledorder = "")
//	    move "*Billing Status Required" to errorMsg
//	    call actionDisplay
//	    return    
//    endif    
        append "Billed:                       " to mailBody
        if (cgibilledordertext = "Billed")
//      append cgibilledordertext  to mailBody
		append "Y"  to mailBody
	else
		append "N"  to mailBody	
        endif
	append newline2 to mailBody    
	append newline2 to mailBody      	

    	if (cgitype = "No Action")
		move "*Please set whether this Correction is a System or Reprint." to errorMsg
		call actionDisplay
		return		    	
	else    
	    	append "Action:                       " to mailBody	
    		append cgitype to mailBody
    		append newline2 to mailBody        
    		append newline2 to mailBody        
    	endif
           	
    else
        append "Billed:                       " to mailBody
        if (cgibilledordertext = "Billed")
		append "Y"  to mailBody
        else
		append "N"  to mailBody	
        endif
        append newline2 to mailBody    
        append newline2 to mailBody      
    	if (cgimailernum = "")
		move "*Mailer Info Required" to errorMsg
		call actionDisplay
		return    
    	endif    
    	append "Mailer:                       " to mailBody
    	append cgimailernum to mailBody
    	append newline2 to mailBody
    	append newline2 to mailBody        
    	if (cgilistnum = "")
		move "*List Number Required" to errorMsg
		call actionDisplay
		return    
    	endif        
    	append "List:                         " to mailBody
    	append cgilistnum to mailBody
    	append newline2 to mailBody
    	append newline2 to mailBody        
    	if (cgiquantity <> "")    
		append "Quantity:                     " to mailBody
		append cgiquantity  to mailBody
		append newline2 to mailBody
    	    append newline2 to mailBody    	
    	endif
    	if (cgimaildate <> "")            
		append "Mail Date:                    " to mailBody
    	    	append cgimaildate  to mailBody
    	    	append newline2 to mailBody
    	    	append newline2 to mailBody            
    	endif   
    	if ((cgiyesnocharge = "Yes") and (cgicharges = ""))
		move "*Please specify the charges or select No if there are none." to errorMsg
		call actionDisplay
		return 
	else
	    	if (cgicharges <> "")
			append "Charges:                      " to mailBody
		    	append cgicharges to mailBody
		    	append newline2 to mailBody
		    	append newline2 to mailBody        	
		    	if (cgiwhopays = "")
				move "*Please specify Who Pays or select No Charges if there are none." to errorMsg
				call actionDisplay
				return 		    	
			else
				append "Who Pays:                      " to mailBody
			    	append cgiwhopays to mailBody
			    	append newline2 to mailBody
			    	append newline2 to mailBody        				
		    	endif
		else
		    	if (cgiyesnocharge = "No")
		    		append "Charges:                      " to mailBody
		    		append "No Charges" to mailBody		    		
			    	append newline2 to mailBody
			    	append newline2 to mailBody		    		
	    		endif
	    	endif	
    	endif
    	if (cgidiscussedwith <> "")                                        
    		append "Discussed with:               " to mailBody
    		append cgidiscussedwith to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif      
    	if (cgireason <> "")                                        
    		append "Reason:                       " to mailBody
    		append cgireason to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif
    	if (cgiexstatadj <> "")                        
    		append "Adj Exstat:                   " to mailBody
    		append cgiexstatadj  to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif    	
    	if (cginotes <> "")                                        
    		append "Notes to Data Entry:          " to mailBody
    		append cginotes to mailBody
    		append newline2 to mailBody
    		append newline2 to mailBody        	
    	endif    	
    endif
    	reset mailBody
    // create the mailer object
    trap noMailer if object
        create objMailer,class="Dundas.Mailer"
    trapclr object

    // get Admin email addresses
;    packkey fUser4IKey from "A"
;    call fUser4IRead
;    loop
;        call fUser4IReadKS
;        until over
;        while (fUserIO.userType = "A")
;        objMailer.TOs.Add using fUserIO.email
;    repeat
//    objMailer.TOs.Add using "dbaca@nincal.com"
    objMailer.TOs.Add using "dataentry@nincal.com"    
    objMailer.TOs.Add using cgiemailaddress1
    if (cgiFunction    = "2")    
	    if (cgibilledordertext = "Billed")
		    objMailer.TOs.Add using "gspranz@nincal.com"    
		    objMailer.TOs.Add using "gbarlaa@nincal.com"    	    
	    endif
    else
	    if ((cgicharges <> "") or (cgibilledordertext = "Billed")) 
		    objMailer.TOs.Add using "gspranz@nincal.com"    
		    objMailer.TOs.Add using "gbarlaa@nincal.com"    	    
	    endif    
    endif
    // Get from address - plbwin.ini
//    pack cwk260 from "nin;ADMIN_EMAIL"
//    clock ini into cwk260
//    if over
//        move "Error reading email address from .ini file!" to errorMsg
//        call actionDisplay
//        return
//    endif
    //move cwk260 into fromAddress
	move cgiemailaddress1 into fromAddress    

    // get the name of the SMTP we'll be using from the INI file
    pack cwk260 from "nin;SMTPSERVER"
    clock ini into cwk260
    if over
        move "Error reading SMTP configuration from .ini file!" to errorMsg
        call actionDisplay
        return
    endif
    objMailer.SMTPRelayServers.Add using cwk260

    //  email settings
    setprop objMailer,*FromAddress=fromAddress
    if (cgiFunction    = "2")    
	    setprop objMailer,*Subject="CORRECTION"
    else
    	setprop objMailer,*Subject="CANCELLATION"
    endif
    setprop objMailer,*Body=mailBody

    // send the email
    trap noSend if object
        objMailer.SendMail
    trapclr object

    destroy objMailer

    // finished sending Admin email
    call cgiDisplayFile using "ordercorrectionresponse.htt"    
    return
//////////////////////////////////////////////////////////////////////////////
populateCGIVariables


    if (ostat = "B")
//    	move "Y" to cgibilledorder    	
	move "Billed" to cgibilledordertext    	
    elseif (ostat = "0")
    	move "Live Order" to cgibilledordertext
//    	move "N" to cgibilledorder    	    	
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
//    append oLnum to cgiListNum
//    append " - " to cgiListNum    
//    append oLstName to cgiListNum
//    reset cgiListNum
    append oMlrNum to cgiMailerNum
    append " - " to cgiMailerNum
    replace "#' " in compcomp    
    append Compcomp to cgiMailerNum   
    reset cgiMailerNum    
    move oLRN to cgiOrderNum
    return
///////////////////////////////////////////////////////////////////////////////
noMailer

    // the mailer object has not been installed properly
    stream *stdout,"<hr><h3>An error has occurred while accessing this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noSend

    // the mailer object couldn't send the message
    stream *stdout,"<hr><h3>An error has occurred while connecting to this page.  Please inform ":
        "the webmaster about the problem.</h3><hr>"
    stop
///////////////////////////////////////////////////////////////////////////////


    include appbottom.inc
//    include nxrfio.inc
//    include ndatio.inc
//    include ncmpio.inc
//    INCLUDE NSEL2IO.INC
//    include nshpio.inc
//    include nspiio.inc
//    include nofrio.inc
//    include nspeio.inc
///////////////////////////////////////////////////////////////////////////////

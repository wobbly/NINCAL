///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   tosprereg.pls                                                //
//                                                                           //
//    AUTHOR:   Andrew Harkins                                               //
//                                                                           //
//      DATE:   January 12, 2006                                             //
//                                                                           //
//                                                                           //
// REVISION:                                                                 //
//   1.0  12JAN2006 ASH       Initial Release                              //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
objMailer           automation

cgicompfunction     dim     %32
cgiemail            dim     %60
cgiusername         dim     %20
cgiFunction         dim     %1
cgiServerName       dim     %260
cgiTOS              dim     %1


i                   integer 4

httName             init    "tosprereg.htt"
///////////////////////////////////////////////////////////////////////////////
start
          // we do this to set up our CGI environment
          call      CGIInit
          call      CGISendHeaders

          // if the session isn't valid for some reason, this will send us to an
          //  error page -  if this page requires admin access, change the call
          //  to this: call sessionValidate using "ADMIN"
          // call sessionValidate

          // this will send the static top part of the HTML document
          call      CGIRenderHeader

          ///////////////////////////////////////////////////////////////////////////
          // TO DO: insert all of your CGI parse statements here.  If any variable is
          //         supposed to filled out but you get an over, empty string, etc.,
          //         append an error string (in HTML) to the variable errorMsg
          ///////////////////////////////////////////////////////////////////////////
          call      CGIParse using "SID",SID
          call      cgiParse using "SERVER_NAME",cgiServerName
          call      CGIParse using "cgicompfunction",cgicompfunction
          call      CGIParse using "cgiusername",cgiusername
          call      CGIParse using "cgiemail",cgiemail
          call      CGIParse using "cgitos",cgitos

          // based upon the "function" value, we need to decide whether to render the form or
          //  process it's data
          call      CGIParse using "cgifunction",cgiFunction
          switch    cgiFunction
                    case      "0"
                              // display a form
                              call      actionDisplay
                    case      "1"
                              // process the form results
                              call      actionProcess
                    default
                              // display a form
                              call      actionDisplay
          endswitch

          // this will send the static bottom part of the HTML document
          call      CGIRenderFooter
          stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay
          // this will display the template (HTT) document we've created for this
          //  page
          call      CGIDisplayFile using httName
          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess
          // logic to process the data that we've parsed from the CGI process
          call      fUserOpen

          if (cgitos != "1")
                    move "You must agree to the Terms of Service." to errorMsg
                    call actionDisplay
                    return
          endif

          call      Trim using cgiusername
          if (cgiusername = "")
                    pack      errorMsg from "I have lost track of who you are.  Please contact your <a href=#"#/index.php/contact-us/#";#"> NIN representative</a>!"
                    call      actionDisplay
                    return
          endif

          packkey   fUser2IKey with cgiusername,""
          call      fUser2IRead
          call      fUser2IReadKS
          if over
                    if (cgiusername = fUserIO.username)
                              // display message here
                              pack      errorMsg from "I have lost track of who you are.  Please contact your <a href=#"#/index.php/contact-us/#";#"> NIN representative</a>!"
                              call      actionDisplay
                              return
                    endif
          endif
          // Update user info
          clear     fUserIO.tos
          call    fUserUpdate
          call    CGITrack using SID,"0037"

          // if we made it this far, the login was successful, so assign a session ID and
          // redirect to the main menu
          call sessionLogin using SID

          stream *STDOUT:
          "<html>",newline2:
          "<body>",newline2

          stream *STDOUT,"<form name='tosprereg' method='post' action='mainmenu.plc'>",newline2:
          "<input type='hidden' name='SID' value='",SID,"'><br>",newline2:
          "<script language=javascript>",newline2:
          "document.tosprereg.submit();",newline2:
          "</script>",newline2:
          "</body>",newline2:
          "</html>",newline2

          return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

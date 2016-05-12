///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   tos.pls                                                      //
//                                                                           //
//    AUTHOR:   Robb Whiting (rwhiting@nincal.com)                           //
//                                                                           //
//      DATE:   08 OCT 2014                                                  //
//                                                                           //
//   PURPOSE:   Log out from NIN                                             //
//                                                                           //
//  REVISION:   VER01   08 OCT 2014 MLYONS                                   //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
   
userType            dim     1
username            dim     80
cgimailernum        dim     %6

///////////////////////////////////////////////////////////////////////////////
start
    
   // we do this to set up our CGI environment
       call CGIInit
       call CGISendHeaders

   // if the session isn't valid for some reason, this will send us to an
   //  error page -  if this page requires admin access, change the call
   //  to this: call sessionValidate using "ADMIN"
       call sessionValidate
   
   // Get the SID passwed by the last page   
       call CGIParse using "SID",SID   
       
   // print the CSS we need.
       
       call CGIRenderUniversalCSS
   // print the menu

       call CGIRenderMenu 
    
    call CGIDisplayFile using "tos.htt"

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

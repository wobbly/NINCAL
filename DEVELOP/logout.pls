///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   logout.pls                                                   //
//                                                                           //
//    AUTHOR:   Mandie Lyons (mlyons@adjacency.net)                          //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//      DATE:   02 NOV 2004                                                  //
//                                                                           //
//   PURPOSE:   Log out from NIN                                             //
//                                                                           //
//  REVISION:   VER01   02 NOV 2004 MLYONS                                   //
//  REVISION:   VER02   23 FEB 2015 RWHITIN                                  //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
   
///////////////////////////////////////////////////////////////////////////////
username        dim     80
userType        dim     1
logout          form    1
    move c0 to logout
///////////////////////////////////////////////////////////////////////////////
start
           
   // we do this to set up our CGI environment
       call CGIInit
       call CGISendHeaders

   // if the session isn't valid for some reason, this will send us to an
   //  error page -  if this page requires admin access, change the call
   //  to this: call sessionValidate using "ADMIN"
       call sessionValidate
       call sessionLogout
   
   // Get the SID passwed by the last page   
.       call CGIParse using "SID",SID   
       
   stream *stdout:
                "<h2><b>Logged out</b></h2>",newline2:
                "Click <a href='/index.php' target='_top'>here</a> to return to the main page.",newline2
    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

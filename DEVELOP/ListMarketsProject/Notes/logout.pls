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
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
username        dim     80
userType        dim     1
///////////////////////////////////////////////////////////////////////////////
start

    call CGIInit
    call CGISendHeaders

    // this is where all the CGI data is retrieved using CGIParse
    call sessionLogout

    // perform the session management functions
    call CGIDisplayFile using "logout.htt"

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

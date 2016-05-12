OBSOLETE - NO LONGER IN USE!!
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   newslettersearch.pls                                         //
//                                                                           //
//    AUTHOR:   mlyons@adjacency.net                                         //
//                                                                           //
//      DATE:   07 APR 2005                                                  //
//                                                                           //
// COPYRIGHT:   2002-2005 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    NIN newsletter search                                        //
//                                                                           //
// REVISION:    VER02   15 NOV 2004 BJACKSON    Modified to work with        //
//                                               enhanced CGILIBRARY         //
//              VER01   04 NOV 2004 BJACKSON    Created                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////

httName         init    "newslettersearch.htt"
///////////////////////////////////////////////////////////////////////////////
start

    // we do this to set up our CGI environment
    call CGIInit
    call CGISendHeaders

    // we don't session validate because we're not logged in yet

    // this will send the static top part of the HTML document
    call CGIRenderHeader

    call actionDisplay

    // this will send the static bottom part of the HTML document
    call CGIRenderFooter

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

    // this will display the login (HTT) document we've created for this page
    call CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

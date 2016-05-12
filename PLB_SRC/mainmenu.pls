///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   mainmenu.pls                                                 //
//                                                                           //
//    AUTHOR:   Mandie Lyons (mlyons@adjacency.net)                          //
//                                                                           //
//      DATE:   03 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    main menu for NIN                                            //
//                                                                           //
// REVISION:    VER01   03 NOV 2004 MLYONS    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
    include apptop.inc
///////////////////////////////////////////////////////////////////////////////
sessionFlag integer 1
userType    dim     1
username    dim     80
cgimailernum        dim     %6
//////////////////////////////////////////////////////////////////////////////
start

    call CGIInit
    call CGISendHeaders
    call sessionValidate

    // now that we're validated, render the page
    call CGIParse using "SID",SID
.temp
.    stream *STDOUT:
.        username,newline2
.          Pause     "5"
.temp
    
    call CGIParse using "Username",username
    call CGIRenderHeader

    call CGIDisplayFile using "mainmenu.htt"

    call CGIRenderFooter

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
    include appbottom.inc
///////////////////////////////////////////////////////////////////////////////

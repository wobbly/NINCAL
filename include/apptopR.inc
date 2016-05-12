///////////////////////////////////////////////////////////////////////////////
//
//   Program:   apptop.inc
//
//    Author:   bjackson@adjacency.net
//
// Copyright:   © 2004 Adjacency Consulting Group, Inc.  All rights reserved.
//
//      Date:   3 December 2004
//
//   Purpose:   Encapsulate required top-of-program inclusions
//
//  Revision:
//              ver03   29 DEC 2005     ASH - ADDED WEBSITE TRACKING FILE
//              ver02   13 DEC 2005     ASH - ADDED CONSULTANT FILE
//              ver01   03 DEC 2004     BJACKSOn
//
///////////////////////////////////////////////////////////////////////////////
    include common.inc
    include cons.inc
    include cntdd.inc
    include compdd.inc
    include ndatdd.inc
    include nxrfdd.inc
    include norddd.inc
.START PATCH 02 ADDED LOGIC
    include ncltdd.inc
.END PATCH 02 ADDED LOGIC
.START PATCH 03 ADDED LOGIC
    include nwebdd.inc
.END PATCH 03 ADDED LOGIC
    include user.io
    include CGILibraryR.inc
    include sessionmgmt.inc
///////////////////////////////////////////////////////////////////////////////
RELEASE INIT "PHASE II"
///////////////////////////////////////////////////////////////////////////////

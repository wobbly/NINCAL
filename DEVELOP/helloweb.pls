///////////////////////////////////////////////////////////////////////////////
//
//   Program:   helloweb.pls
//
//    Author:   Brian J. Jackson
//
// Copyright:   © 2004 Adjacency Consulting Group, Inc.  All rights reserved.
//
//      Date:   01 JUL 1998
//
//   Purpose:   Demonstrate using STREAM to send HTML to a browser
//
//  Revision:   v1.0    01 JUL 1998     bjackson
//
///////////////////////////////////////////////////////////////////////////////
newline init 0x0D,0x0A
///////////////////////////////////////////////////////////////////////////////
start

    // The helloweb.plc program needs to get called by your web server
    //  (APACHE/IIS) in response to a browser's request for the document.
    //  Setting up the web server to associate the PLC with the runtime is
    //  not too difficult, and you can find documentation on how to do this
    //  online.

    // standard HTTP headers used in EVERY CGI script
    stream *STDOUT,"HTTP/1.1 200 OK",newline:
                   "Content-type: text/html",newline:
                   "Expires: Tue 01 Jan 1980, 12:00:00 GMT",newline,newline

    // custom HTML content, any HTML we want to send
    stream *STDOUT,"<html>",newline:
                   "<head>",newline:
                   "<title>Hello!</title>",newline:
                   "</head>",newline:
                   "<body>",newline:
                   "<h1>Hello Web!</h1>",newline:
                   "</body>",newline:
                   "</html>",newline

    stop
///////////////////////////////////////////////////////////////////////////////

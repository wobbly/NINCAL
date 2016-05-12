///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   comments.pls                                                 //
//                                                                           //
//    AUTHOR:   aharkin@nincal.com                                           //
//                                                                           //
//      DATE:   27 APR 2006                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Names in the News                                  //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    CGI comments submission application                          //
//                                                                           //
// REVISION:                                                                 //
//                                                                           //
//                							     //
//                1.06    27FEB2015  RVW replace logic to use Office365      //                                   
//                1.05    13Nov2012  DLH replace Dundas.mailer               //
//.               1.04    19February2010 DLH   remove Divisions              //
//.               1.03    15September009 DLH   New Divisions                 // 
//.                        this code must now be compiled 2X once for        //
//                            nincal.com and once for innocativeacq.com      //
//.                         setting flag iWebFlag'   N or I   default N      //
//.               1.02    09June2009 DLH   some more trap & deny             //
//.               1.01    23March2007 DLH   basic html trap & deny           //
//              VER01   27 APR 2006 ASH    Created                           //
//                                                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
pc        equ       0
          include     apptop.inc
///////////////////////////////////////////////////////////////////////////////
.begin patch 1.05
.objMailer       automation
.end patch 1.05

cgiFunction     dim     %1
cgiText         dim     %500
cgiServerName   dim     %260
cgipassword     dim     %12
fromAddress     dim     %260
.mailBody        dim     %32768               23March2007 in cons.inc
cwk260          dim     260
.begin patch        1.03
.WebFlag   Init      "N"
.end patch        1.03

.begin patch 1.03
httName         init    "comments.htt"
.httName   Dim       15
.end patch 1.03

i               integer 4
///////////////////////////////////////////////////////////////////////////////
start
..begin patch 1.03
.          if        (WEbFlag = "I")
.          move      "commentsAI.htt",Httname
.          ElseIF    (WEbFlag = "N")
.          move      "commentsAI.htt",Httname
.          endif
..end patch 1.03
          // we do this to set up our CGI environment
          call      CGIInit
          call      CGISendHeaders

          // this will send the static top part of the HTML document
.begin patch 1.07 comment out
.          call      CGIRenderHeader
.end patch 1.07 comment out

          call      CGIRenderUniversalCSS
          call      CGIRenderMenu

          // based upon the "function" value, we need to decide whether to render the form or
          //  process it's data
          call      CGIParse using "function",cgiFunction
          call      CGIParse using "cgitext",cgiText

          switch    cgiFunction
          case "0"
          // display a form
                    call      actionDisplay
          case "1"
          // process the form results
                    call      actionProcess
          default
          // display a form
                    call      actionDisplay
          endswitch

          // this will send the static bottom part of the HTML document
.begin patch 1.07 comment out
.          call      CGIRenderFooter
.end patch 1.07 comment out
          stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionDisplay

          // this will display the (HTT) document we've created for this
          //  page
          reset     errorMsg
          call      CGIDisplayFile using httName

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
actionProcess

          // logic to process the data that we've parsed from the CGI process
          call fUserOpen

          //  CGI process
          call      cgiParse using "cgitext",cgiText
          if over
                    append    "Text is required!<br>" to errorMsg
                    call      actionDisplay
                    return
          endif

          count     i in cgiText
          move      i to cgiServerName
          if zero
                    move      "Text is required!" to errorMsg
                    call      actionDisplay
                    return
          endif
          move      cgiText,mailBody
.Begin patch 1.02
          scan      "map.txt",mailbody
          if        equal
                    move      "Unallowed Text in message!" to errorMsg
                    call      actionDisplay
                    return
          endif
.end patch 1.02

.Begin patch 1.01
          reset     mailbody
          scan      "http",mailbody
          if        equal
                    move      "Unallowed Text in message!" to errorMsg
                    call      actionDisplay
                    return
          endif
          reset     mailbody

          scan      "[url]",mailbody
          if        equal
                    move      "Unallowed Text in message!" to errorMsg
                    call      actionDisplay
                    return
          endif
          reset     mailbody
          scan      "href",mailbody
          if        equal
                    move      "Unallowed Text in message!" to errorMsg
                    call      actionDisplay
                    return
          endif
          reset     mailbody
.Begin patch 1.01
          call      CGITrack using SID,"0038"

.patch 1.06 -- implement emailAdmin function that we used in registration.pls -- comment the other code out.
.begin comment out

.begin patch 1.05
.          // create the mailer object
          trap      noMailer if object
          create    objMailer,class="Dundas.Mailer"
          trapclr   object
.end patch 1.05

          // get Admin email addresses
          packkey   fUser4IKey from "A"
          call      fUser4IRead
          loop
.begin patch 1.05
                    clear     mailto
                    call      fUser4IReadKS
                    until over
                    while (fUserIO.userType = "A")
                    objMailer.TOs.Add using fUserIO.email
                    append      fUserIO.email,Mailto
                    append    ",",mailto
          repeat
                    reset     mailto
.end patch 1.05


          // Get from address - plbwin.ini
          pack      cwk260 from "nin;ADMIN_EMAIL"
          clock     ini into cwk260
          if over
                    move      "Error reading email address from .ini file!" to errorMsg
                    call      actionDisplay
                    return
          endif
          move      cwk260 into fromAddress

          // get the name of the SMTP we'll be using from the INI file
          pack      cwk260 from "nin;SMTPSERVER"
          clock     ini into cwk260
          if over
                    move      "Error reading SMTP configuration from .ini file!" to errorMsg
                    call      actionDisplay
                    return
          endif
.begin patch 1.05
       objMailer.SMTPRelayServers.Add using cwk260
.end patch 1.05

.          //  email settings
.begin patch 1.05
          setprop   objMailer,*FromAddress=fromAddress
          
.begin patch        1.03
          setprop   objMailer,*Subject="Questions/Comments about NIN website"
          pack      mailsubjct,"Questions/Comments about NIN website"
          If        (WebFlag = "N")
          setprop   objMailer,*Subject="Questions/Comments about NIN website"
          ElseIf    (WebFlag = "I")
          setprop   objMailer,*Subject="Questions/Comments about Innovative Acquisition website"
          endif
..end patch        1.03
       setprop   objMailer,*Body=mailBody

.          // send the email
          trap      noSend if object
          objMailer.SendMail
          trapclr   object

          destroy   objMailer
          pack      MailServer,cwk260
          Move      "Y",Mailtrace
          Call      SendMail

.end patch 1.05

.end comment out -- patch 1.06
          // finished sending Admin email
        stream *stdout,"<center>",newline2:
            "<br><br><br><br>Your thoughts have been submitted.  Thanks for the suggestions.<br><br>",newline2:
            "Click <a href='javascript:history.go(-2)'>here</a> to return to the calling Screen.":
            "</center>",newline2
          return
///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
noMailer

          // the mailer object has not been installed properly
          stream    *stdout,"<hr><h3>An error has occurred while accessing this page.  Please inform ":
                    "the webmaster about the problem.</h3><hr>"
          stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
noSend

          // the mailer object couldn't send the message
          stream    *stdout,"<hr><h3>An error has occurred while connecting to this page.  Please inform ":
                    "the webmaster about the problem.</h3><hr>"
          stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
          include   appbottom.inc
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////


    return
//////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
//
//   Program:   taunting.pls
//
//    Author:   Brian Jackson
//
// Copyright:   © 2004 Adjacency Consulting Group, Inc.  All rights reserved.
//
//      Date:   29 APR 2004
//
//   Purpose:   sample CGI processor - it is a cross between a Mad-Lib and
//               the French taunting scene from The Holy Grail.  Use at your
//               own risk!
//
//  Revision:   v1.0    29 APR 2004     BJACKSON
//
//   License:   GPL
//
//              This program is free software; you can redistribute it and/or
//              modify it under the terms of the GNU General Public License
//              as published by the Free Software Foundation; either version 2
//              of the License, or (at your option) any later version.
//
//              This program is distributed in the hope that it will be useful,
//              but WITHOUT ANY WARRANTY; without even the implied warranty of
//              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//              GNU General Public License for more details.
//
//              You should have received a copy of the GNU General Public License
//              along with this program; if not, write to the Free Software
//              Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
///////////////////////////////////////////////////////////////////////////////
animal          dim         260
cgiInput        dim         ^
countString     dim         5
cwk5            dim         5
environment     dim         260
food            dim         260
newCount        dim         5
ordinal         dim         260
relative1       dim         260
relative2       dim         260
varValue        dim         260

count           form        5
nwk5            form        5

newline         init        0x0D,0x0A

i               integer     4
str20	dim	20
cgiFunction         dim     %1
str1	dim	1
taskname	dim	500
///////////////////////////////////////////////////////////////////////////////


start

    move	"SERVER_SOFTWARE",taskname
    clock env into taskname
    // find out how big the CGI input is
    move "CONTENT_LENGTH" to environment
    clock env into environment
    bump environment by 15
    move environment to cwk5
    type cwk5
    if equal
    move	"CONTENT_LENGTH",str20
        // If there is an environment variable called content length, then
        //  we have data in STDIN
        move cwk5 to nwk5
        smake cgiInput,nwk5
        stream *STDIN,cgiInput
    else
    move	"QUERY_STRING",str20
        // all our data is in the QUERY_STRING environment variable
        smake cgiInput,260
        move "QUERY_STRING" to environment
        clock env into environment
        if over
            clear cgiInput
        else
            bump environment by 13
            move environment to cgiInput
        endif
    endif

    scan "cgifunction=" in cgiInput
    if equal
        bump cgiInput by 12
.        move cgiInput to varValue
.        scan "&" in varValue
.        if equal
.            bump varValue by -1
.            if eos
.                clear varValue
.            else
.                lenset varValue
.                reset varValue
.            endif
.        endif
.	move	VarValue,str1
	move cgiInput to str1
	if (str1 = "2")
		goto  Drew
	endif
    endif
    reset cgiInput


    // extract the relative1 variable from the CGI input
    scan "relative1=" in cgiInput
    if equal
        bump cgiInput by 10
        move cgiInput to varValue
        scan "&" in varValue
        if equal
            bump varValue by -1
            if eos
                clear varValue
            else
                lenset varValue
                reset varValue
            endif
        endif
        move varValue to relative1
    endif
    reset cgiInput
    count i in relative1
    call doError if zero

    // extract the relative2 variable from the CGI input
    scan "relative2=" in cgiInput
    if equal
        bump cgiInput by 10
        move cgiInput to varValue
        scan "&" in varValue
        if equal
            bump varValue by -1
            if eos
                clear varValue
            else
                lenset varValue
                reset varValue
            endif
        endif
        move varValue to relative2
    endif
    reset cgiInput
    count i in relative2
    call doError if zero

    // extract the animal variable from the CGI input
    scan "animal=" in cgiInput
    if equal
        bump cgiInput by 7
        move cgiInput to varValue
        scan "&" in varValue
        if equal
            bump varValue by -1
            if eos
                clear varValue
            else
                lenset varValue
                reset varValue
            endif
        endif
        move varValue to animal
    endif
    reset cgiInput
    count i in animal
    call doError if zero

    // extract the food variable from the CGI input
    scan "food=" in cgiInput
    if equal
        bump cgiInput by 5
        move cgiInput to varValue
        scan "&" in varValue
        if equal
            bump varValue by -1
            if eos
                clear varValue
            else
                lenset varValue
                reset varValue
            endif
        endif
        move varValue to food
    endif
    reset cgiInput
    count i in food
    call doError if zero

    // extract the count variable from the CGI input
    scan "count=" in cgiInput
    if equal
        bump cgiInput by 6
        move cgiInput to varValue
        scan "&" in varValue
        if equal
            bump varValue by -1
            if eos
                clear varValue
            else
                lenset varValue
                reset varValue
            endif
        endif
        type varValue
        if equal
            move varValue to count
        else
            move "2" to count
        endif
    else
        move "2" to count
    endif

    // set a string so we can tell the poor user how many times they've been
    //  taunted
    switch count
        case "1"
            move "first" to ordinal
        case "2"
            move "second" to ordinal
        case "3"
            move "third" to ordinal
        case "4"
            move "fourth" to ordinal
        case "5"
            move "fifth" to ordinal
        case "6"
            move "sixth" to ordinal
        case "7"
            move "seventh" to ordinal
        case "8"
            move "eighth" to ordinal
        case "9"
            move "ninth" to ordinal
        default
            clear ordinal
    endswitch

    // increment the counter for our next taunting
    add "1" to count giving nwk5
    move nwk5 to newCount
    squeeze newCount into newCount

    // this is where we do business...
    call doHTML

    stop
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
doHTML

    // we ALWAYS have to output this HTTP header first
    stream *stdout,"HTTP/1.0 200 OK",newline:
                   "Content-type: text/html",newline:
                   "Expires: Tue 01 Jan 1980, 12:00:00 GMT",newline:
                   newline

    // output the basic html elements
    stream *stdout,"<html>"
    stream *stdout,"<body>"
    stream *stdout,"<center>"
    stream *stdout,"<font face=""Tahoma"" size=4>"
    stream *stdout,"Your ",relative1," was a ",animal,", <br>":
                       "and your ",relative2," smelled of ",food,"!<br><br>"

    // decide how to continue the taunting
    if (count > 9)
        move count to countString
        stream *stdout,"I have already taunted you ",countString," times.  Go away ":
                           "you silly code-monkey type!"
    else
        stream *stdout,"Now go away, or I shall taunt you a ",ordinal," time!",taskname
    endif

    // this is the same form we put into taunting.html - we want to regenerate it
    //  so the user can keep on getting taunted.
    stream *stdout,"</font>"
    stream *stdout,"<form method=""post"" action=""taunting.plc"">":
                   "<table border=0 align=center>":
                   "<tr>":
                   "<td align=right>Enter a relative (i.e. mother, father, etc.):</td>":
                   "<td align=left><input type=""text"" name=""relative1"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter a type of animal:</td>":
                   "<td align=left><input type=""text"" name=""animal"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter another relative:</td>":
                   "<td align=left><input type=""text"" name=""relative2"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter a type of food:</td>":
                   "<td align=left><input type=""text"" name=""food"" value="""" /></td>":
                   "</tr>":
			"<tr>":
			"<td align=center>":
			"<input type=submit value='Submit'>":
			"</td>":
			"<td align=center>":
			"<input type=Reset value='Reset'>":
			"</td>":
			"<td align=center>":
			"<input type='button' value='Pick Your Nose' onClick='pickNose();'>":
                   "<input type=""hidden"" name=""count"" value=""",newCount,""" /></td>":
			"</td>":
			"</tr>":
                   "</table>":
                   "</form>"

    stream *stdout,"</center>"
    stream *stdout,"</body>"
    stream *stdout,"</html>"
	move	"1",cgifunction
    return
///////////////////////////////////////////////////////////////////////////////

drew
    // we ALWAYS have to output this HTTP header first
    stream *stdout,"HTTP/1.0 200 OK",newline:
                   "Content-type: text/html",newline:
                   "Expires: Tue 01 Jan 1980, 12:00:00 GMT",newline:
                   newline

    // output the basic html elements
    stream *stdout,"<html>"
    stream *stdout,"<body>"
    stream *stdout,"<center>"
    stream *stdout,"<font face=""Tahoma"" size=4>"
    stream *stdout,"You Pick your own Nose, you filthy slug eating swamp sow!<br><br>"
.    stream *stdout,cgiInput," ",str1,",",varvalue
    // this is the same form we put into taunting.html - we want to regenerate it
    //  so the user can keep on getting taunted.
    stream *stdout,"</font>"
    stream *stdout,"<form method=""post"" action=""taunting.plc"">":
                   "<table border=0 align=center>":
                   "<tr>":
                   "<td align=right>Enter a relative (i.e. mother, father, etc.):</td>":
                   "<td align=left><input type=""text"" name=""relative1"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter a type of animal:</td>":
                   "<td align=left><input type=""text"" name=""animal"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter another relative:</td>":
                   "<td align=left><input type=""text"" name=""relative2"" value="""" /></td>":
                   "</tr>":
                   "<tr>":
                   "<td align=right>Enter a type of food:</td>":
                   "<td align=left><input type=""text"" name=""food"" value="""" /></td>":
                   "</tr>":
			"<tr>":
			"<td align=center>":
			"<input type=submit value='Submit'>":
			"</td>":
			"<td align=center>":
			"<input type=Reset value='Reset'>":
			"</td>":
			"<td align=center>":
			"<input type='button' value='Pick Your Nose' onClick='pickNose();'>":
                   "<input type=""hidden"" name=""count"" value=""",newCount,""" /></td>":
			"</td>":
			"</tr>":
                   "</table>":
                   "</form>"

    stream *stdout,"</center>"
    stream *stdout,"</body>"
    stream *stdout,"</html>"
	move	"1",cgifunction
    stop

///////////////////////////////////////////////////////////////////////////////
doError

    // we ALWAYS have to output this HTTP header first
    stream *stdout,"HTTP/1.0 200 OK",newline:
                   "Content-type: text/html",newline:
                   "Expires: Tue 01 Jan 1980, 12:00:00 GMT",newline:
                   newline

    stream *stdout,cgiInput,"<hr>"

    // output the basic html elements
    stream *stdout,"<html>"
    stream *stdout,"<body>"
    stream *stdout,"<center>"
    stream *stdout,"<font face=""Tahoma"" size=4>"
    stream *stdout,"You forgot to fill something in, you food-trough-waterer, ":
                   "you!  Now, go back and do it right this time!"
    stream *stdout,"</center>"
    stream *stdout,"</body>"
    stream *stdout,"</html>"
	move	"1",cgifunction
    stop
///////////////////////////////////////////////////////////////////////////////

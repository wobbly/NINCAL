CGI_Get_Input
;Exits with string CgiInput
;    // find out how big the CGI input is
    move "CONTENT_LENGTH" to environment
    clock env into environment
    bump environment by 15
    move environment to cwk5
    type cwk5
    if equal
;        // If there is an environment variable called content length, then
;        //  we have data in STDIN
        move cwk5 to nwk5
        smake cgiInput,nwk5
        stream *STDIN,cgiInput
    else
;        // all our data is in the QUERY_STRING environment variable
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
               Return
HTML_Head_Open
;              // output the basic html elements
;    // we ALWAYS have to output this HTTP header first
     stream *stdout,"<meta http-equiv=""Content-Type"" content=""text/html; charset=windows-1252"">",CRLF:
                   "Expires: Tue 01 Jan 1980, 12:00:00 GMT",CRLF:
                   CRLF
               stream *stdout,"<html>"
               Stream *stdout,"<head>"
               Stream *Stdout,"<script language=""JavaScript"">"
               Stream  *Stdout,"function preLoadImages(){":
                              "var t='<layer top=""-100"" left=""0"" visibility=""hide""><div style=""position:absolute; top:-100px; left:0px; visibility:hidden"">';":
                              "for(i=0;i<arguments.length;i++)t+='<img src=""'+arguments[i]+'"" height=""20"" width=""100"">';":
                              "t+='</div></layer>';":
                              "document.write(t);":
                              "}"
               Stream  *Stdout,"function swapImage(imgN,imgU){":
                              "if(document.images)document.images[imgN].src=imgU;":
                              "}"
               Return
HTML_Head_Close
               Stream *Stdout,"</script>"
               Stream *Stdout,"</head>"
               Return

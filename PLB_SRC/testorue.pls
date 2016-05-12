pc         equ        0
           include    common.inc
           include    cons.inc
dfile      file           
time       dim        8
release    init "no"           
                    open      dfile,"\\NinS1\e\data\ordersFax.srt"
                    Clear     MailBody
                    Append    Today,MailBody
                    Append    " -Company's Receiving Order's !!! ",MailBOdy
                    append    Time,mailbody
                    append    "<br>",MailBOdy                 
.begin patch 10.44
                     Clear       Mailto 
.end patch 10.44
                    loop
.begin patch 10.0
.                              read dfile,seq;str55
                      move       c0,n5
.                      call       debug
                              read dfile,seq;str45,str35
                              until over
                              append    STR45,mailbody
                              append    " - ",mailbody
                              append    str35,mailbody
.end patch 10.0
                              append    "<br>",MailBOdy                 
.begin patch 10.44
                              call          trim using str35
                              if            (str35 <> "")
                              scan          "Krsni" in str35
                              if            not equal
                                 reset         str35              
                                 scan          "Magee",str35
                                 if         not equal
                                 reset      str35
                                 scan          str35 in mailto
                                 if            Not Zero
                                            reset         mailto   
                                                       if            (n5 < 1)
                                                       pack          mailto using str35
                                                       add        c1,n5
                                                       else
                                                       pack          mailto using mailto,",",str35
                                                       add        c1,n5
                                                       endif
                                            else
                                            reset         mailto
                                            endif
                                 endif
                              endif   
                              endif
.end patch 10.44

                    repeat
           call       debug

           stop
           include    comlogic.inc
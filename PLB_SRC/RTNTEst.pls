          Include   Common.inc
          Include   Cons.inc
Release   Init      "temp"
Reldate   Init      "16 Feb 2011"
          INclude   Nrtndd.inc
File      Ifile     Keyl=4,"Name=OrderbyRtn|NINS1:502"          
Output    Ifile     Keyl=4,FIXED=227,UNCOMP,Name="UsedRTN.isi|NINS1:502"



          open      Output,"usedrtn.isi|NINS1:502",exclusive
          open      File,"OrderbyRtn.isi|NINS1:502"
          
Looper    Loop
          call      NrtnSeq 
          until     over
          packkey   str4,Rtnum
          read      File,str4;str1
          if        Over
          Else
          read      output,str4;str1
                    if        over
                    write     output,str4;rtnvars
                    endif
          endif
          repeat
          Stop
          Include   Nrtnio.inc
          include   comlogic.inc
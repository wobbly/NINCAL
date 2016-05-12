Pc        equ       0

          inc       common.inc         
          inc       cons.inc

INput     file
output    file
Line      dim       700
release   init      "1.0"    DLH
reldate   Init      "2010 July 27"
Amp       Init      "##"
.read html source code and convert to code that can be writtem by PLB to recreate pages on the fly

          Open      input,"\\nins1\e\library\plb_SRc\datacardlibrarysource.html"
          prepare   output,"c:\work\datacardlibrarysource.dat"
          
          loop      
                    Read      Input,seq;Line
                    until     over
                    Loop
                              Scan      "##" in line                     .find and replace # with ##
                              Until     Over
                              if        (GReater)
                              break
                              endif
                              if        equal
                              Sinsert   line,AMp
                              bump      Line,c1
                              endif
                              repeat
                              
                              Reset     Line                              
                    Loop
                              Scan      "#"" in line                     .find and replace " with #"         ."
                              Until     Over
                              if        (GReater)
                              break
                              endif
                              if        equal
                              Sinsert   line,AMp
                              bump      Line,c1
                              endif
                              repeat
                              
                    Reset     Line                              
                    call      Trim using Line
                   Write      Output,Seq;"#"",*ll,Line,"#""
                   repeat
                   weof       output,seq
                   close      output
                   stop
          Inc       Comlogic.inc                   
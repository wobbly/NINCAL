testFile file
seq form "-2"
result form 9

comspec DIM 100
dim500 dim 500

          CLEAR      comspec
          MOVE       "COMSPEC" TO comspec
          CLOCK      ENV,comspec
          ENDSET     comspec
          BUMP       comspec
          APPEND     " /C " TO comspec
          RESET      comspec
          SCAN       "=" IN comspec
          BUMP       comspec

          prepare testFile,"c:\work\testrun.bat"
          write testFile,seq;"dir >c:\work\testdir.txt"
          write testFile,seq;"pause"
          weof testFile,seq
          close testFile

          pack dim500 from comspec,"c:\work\testrun.bat"
          alert note,dim500,result
          execute dim500

          alert note,"c:\work\testrun.bat",result
          execute "c:\work\testrun.bat"

          prepare testFile,"c:\windows\testrun.bat"
          write testFile,seq;"dir"
          write testFile,seq;"pause"
          weof testFile,seq
          close testFile

          pack dim500 from comspec,"testrun.bat"
          alert note,dim500,result
          execute dim500

          alert note,"testrun.bat",result
          execute "testrun.bat"

          alert note,"",result
          execute ""

          alert note,"End of program",result

          stop

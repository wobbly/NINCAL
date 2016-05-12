////////////////////////////////////////////////////////////////////////////////////////////////////
newline       init          0x0d,0x0a
environment   dim           100
dim5          dim           5
workString    dim           ^
STDIN_DATA    dim           ^
form5         form          5
////////////////////////////////////////////////////////////////////////////////////////////////////
              move          "CONTENT_LENGTH" to environment
              clock         env into environment
              bump          environment by 15
              move          environment to dim5
              type          dim5
              if equal
                move          dim5 to form5
                if (form5 = 0)
                  smake         STDIN_DATA,1
                  smake         workString,256
                else
                  smake         STDIN_DATA,form5
                  smake         workString,form5
                endif
              else
                smake         STDIN_DATA,1
                smake         workString,256
              endif

              if (form5 > 0)
                stream        *STDIN,STDIN_DATA
              endif

              stream        *STDOUT,"HTTP/1.1 200 OK",newline
              stream        *STDOUT,"Content-type: text/plain",newline,newline
              stream        *STDOUT,STDIN_DATA
              stop
////////////////////////////////////////////////////////////////////////////////////////////////////

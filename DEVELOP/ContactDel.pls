pc equ     0
           include    common.inc
           include    cons.inc
           include    cntdd.inc


Release    Init       "PRE"
Reldate    Init       "2015 May 13"
           
key        keyin      *p10:10,"company ",*zf,*jr,str6
          clear     CNCTFLD3
          pack      CNCTFLD,"01X",str6,"000"
          move      "CNTKey",Location
          pack      KeyLocation,"Key: ",CNCTFLD
          loop

          call      CNCTKey
                    until over
           if         (str6 = CNCTCODE)         
           call       cnctdel
           endif
           add        c1,n3
           move       n3,str3
           packkey    cnctfld using str6,str3
           rep        zfill,cnctfld
           repeat
           
           stop
           
           
           
           include    cntio.inc
           include    comlogic.inc
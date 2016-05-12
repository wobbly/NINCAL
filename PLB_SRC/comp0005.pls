PC        EQU       0
          INC       COMMON.INC
          INCLUDE   CONS.INC
          include   oslspern.inc
          include compdd.inc
          include cntdd.inc
Release   Init      "Devo"
          
          
INput     FIle                                    .used to apply salesforce # to company fyle
.first pass entire file


          open      INput,"c:\work\Company.csv",exclusive
          
Loop      call      compks
          goto      eoj if over
         if        (COMPINACTIVE  = "F" or COMPINACTIVE  = " " or COMPINACTIVE  = "")
                    write     output,seq;*CDFon,compvars
                    endif
          endif
          goto      loop
          
EOJ       weof      output,seq
          stop

          include   compio.inc
          include   cntio.inc

          include   comlogic.inc
          

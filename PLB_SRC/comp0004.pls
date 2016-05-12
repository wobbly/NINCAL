PC        EQU       0
          INC       COMMON.INC
          INCLUDE   CONS.INC
          include   oslspern.inc
          include compdd.inc
          include cntdd.inc
Release   Init      "Devo"
.perverted to dump info for Suzie 2014 March 17

          
Output    FIle                                    .used to dump to salesforce
Output1    FIle                                    .used to dump to salesforce
.first pass entire file


          Prepare   output,"c:\work\Company.csv",exclusive
          Prepare   output1,"c:\work\Contact.csv",exclusive
          
Loop      call      compks
          goto      eoj if over
           goto       okok
.temporary for Suzie
           UpperCase  CompComp
           scan                  "INFOGROUP",CompComp
           goto       okok if equal
           reset      CompComp
           scan                  "INFOUSA",CompComp
           goto       okok if equal
           reset      CompComp
           scan                  "DIRECT MEDIA",CompComp
           goto       okok if equal
           reset      CompComp
           scan                  "MILLARD",CompComp
           goto       okok if equal
           UpperCase  CompEmail
           scan                  "INFOGROUP",CompEmail
           goto       okok if equal
           reset      CompEmail
           scan                  "INFOUSA",CompEmail
           goto       okok if equal
           goto Loop

             
okok       reset      CompComp
           reset      CompEmail

.          if        (COMPMLRFLG = "T" or COMPBRKFLG = "T" or COMPCLRFLG = "T")
.         if        (COMPINACTIVE  = "F" or COMPINACTIVE  = " " or COMPINACTIVE  = "")
                    write     output,seq;*CDFon,compvars

                    clear     str3
                    move      C0 TO N3
                    move      N3 to str3
                    rep       zfill,str3
                    pack      CNCTFLD,COMPNUM,str3
                    call      zfillit using CNCTFLD
                    loop
                              call      CNCTKey
                    until over
                    write     output1,seq;*CDFon,CNCTVARS
                              add       C1,N3
                              move      N3 to str3
                              rep       zfill,str3
                              pack      CNCTFLD,COMPNUM,str3
                    repeat

.          endif
          goto      loop
          
EOJ       weof      output,seq
           weof      output1,seq
          stop

          include   compio.inc
          include   cntio.inc

          include   comlogic.inc
          

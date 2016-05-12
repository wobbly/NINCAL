PC        EQU       0
          INC       COMMON.INC
          INCLUDE   CONS.INC
          include   oslspern.inc
          include compdd.inc
          include cntdd.inc
          include compsfdd.inc
Release   Init      "Devo"
.recently used to dump all broker emails to csv file          
          
Output    FIle                                    .used to dump to salesforce
.first pass entire file


          Prepare   output,"c:\work\Contacts.csv",exclusive
          MOVe      c1,COmpSFPath
          
Loop      call      CNCTKS
          goto      eoj if over
.         if        (CNCTINACTIVE  = "F" or CNCTINACTIVE  = " " or CNCTINACTIVE  = "")
.         CLear     CompSlsF
.         packkey   compsffld,CNCTCODE
.         call      CompsfKEy
.         if        over
.         CLear     CompSlsF
.         endif     
          call      Trim using CNCTEmail
          if        (CNCTEMAIL <> "")
          packkey   COMPFLD from CNCTCODE
          call      Compkey
.check for record type and output IE Broker Mailer, SB, ETC
          Clear     Str25
          if        (COMPMLRFLG = "T")
          move      "Mailer",str25
          Elseif    (COMPBRKFLG = "T")
          move      "Broker",str25
          Elseif    (COMPSVBFLG = "T")
          move      "Service B.",str25
          elseif    (COMPCLRFLG = "T")
          move      "Consultant",Str25
          endif



          write     output,seq;*CDFon,str25,COMPCOMP,CNCTVARS
          endif
          goto      loop
          
EOJ       weof      output,seq
          stop

          include   compio.inc
          include   cntio.inc
          include compsfio.inc

          include   comlogic.inc
          

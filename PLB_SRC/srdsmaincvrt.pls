PC        Equ       0          
          Include   Common.inc
          include   Cons.inc
          INclude   SrdsDD.inc
Release   Init      "Not"
reldate   Init      "Never"
File      File
.SRDSStatus          Dim       1
Dim75               Dim       75
          Prepare   File,"c:\work\sdrs_main_new.dat"
          
          Loop
          call      srdsseq
          until     over
          scan      "OTM-",SRDSMLSTNAME
          if        equal
          call      Debug
          move      "W",srdsStatus
          Reset     SRDSMLSTNAME
          Move      "OTM-",Str4
          call      REPLACEIT using SRDSMLSTNAME,STR4,B1
          call      Ltrim using SRDSMLSTNAME
          call      Debug
          else      
          Reset     SRDSMLSTNAME
          Clear     SrdsStatus
          endif
          write     File,Seq;SrdsVars,SRDSStatus
          repeat    
          WEof      File,seq
          CLose     File
          Stop





          include   Srdsio.inc
          include   comlogic.inc
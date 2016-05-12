PC        EQU       0
         Include             Common.inc
          Include             Cons.inc
         Include             Compdd.inc
          include   cntdd.inc          
          include   norddd.inc
.begin patch 1.1
          include     nxrfdd.inc
          include     Ndatdd.inc
.end patch 1.1
.Begin patch 1.2
          INCLUDE   NXNGDD.inc
.end patch 1.2
.Begin patch 1.21
           include    nsmpdd.inc
.end patch 1.21

Release    Init       "1.21"    .add revised date, list status, presence of samples
reldate   Init      "2015 April 23"
.Release   Init       "1.20"    .add exchange usage as well
.reldate   Init      "2014 September 18"
.Release   Init       "1.10"    .add List data
.reldate   Init      "2014 September 18"
.Release             Init                "1.00"    .add order data
.reldate   Init      "2013 October 24"
.Release             Init                "Pre"
KEYPAD1   INIT      "01L"
records   form      10
unused    file
used      file
.Begin patch 1.2
Exchange   Dim        1        .(Y)es if exchange history found
.end patch 1.2
.Begin patch 1.21
Samples    Dim        1
.end patch 1.21
          Prepare   Unused,"c:\work\UnusedMLr.csv",exclusive                                                       .mlr #'s not in master file
          Prepare   Used,"c:\work\UsedMLr.csv",exclusive                                                       .mlrs & ord history
.Begin patch 1.2
          write     Unused,seq;*cdfon,"Comp ##","MLR ##","Mailer Name"
          write     used,seq;*cdfon,"Comp ##","MLR ##","Mailer Name","Usage","List ##","List Name","Exchange History","MLR Last Updated","List Status","Samples","Last Order Date"
.end patch 1.2
          move      c3,nordlock
          move      c3,nmlrlock
             move         c3,NXNGLock
.begin patch 1.1
          move      c3,nDatlock
           move       c2,Nxrfpath
.end patch 1.1
           move       c3,nsmplock



          call      Paint
          move      c0,n4
          move      n4,str4
          pack      Mkey from str4,b3
          rep       Zfill in mkey

          loop
             display   *p10:10,"Checking ## :",str4
          call      Nmlrkey
          until     (n4 = "9999")
          if        over
          write unused,seq;*cdfon,compnum,mkey,CompComp
          else
.begin patch 1.1

               Packkey        NXRFFLD2,Compnum
               move           "NXRFKEY",Location
               pack           KeyLocation,"Key: ",NXRFFLD2
               call           NXRFKEY
               Packkey        Ndatfld,NXRFLIST
               move           "NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               call           NDATKEY
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal

         match     "THE " to mlstname
         call      fixitThe if equal

                Rep              ",;",mlstname         

.end patch 1.1
.Begin patch 1.2
           call       debug
           clear      Exchange
           clear     nxngfld1
           clear     nxngfld2
           packkey   nxngfld1 from "01X",cOMPNUM
           call      NXNGAIM
           if over

           clear      Exchange
           clear     nxngfld1
           clear     nxngfld2
           packkey   nxngfld2 from "02X",Compnum
           call      nxngaim
                      IF         OVER
                      clear      Exchange
                      else
                      move       yes,Exchange
                      endif
           else
           move       yes,Exchange
           endif
.end patch 1.2
.begin patch 1.21
             Clear    Samples
          pack      NSMPFLD1,"01X",Compnum
          move    "S.LoadSamples-NSMPAIM",Location
          pack    KeyLocation,"Key: ",NSMPFLD1
          call    NSMPAIM
             if       not over
             move     Yes,Samples
             endif

.end patch 1.21

.add loop to read order file and get usage count          write out data
          Clear NORDFLD1
          Clear NORDFLD2
          Clear NORDFLD3
          Clear NORDFLD4
          PACK  NORDFLD1 FROM KEYPAD1,mnum
          MOVE  C2 TO NORDPATH
          CALL  NORDAIM
          pack  str8 from OODTEc,OODTEy,OODTEM,OODTED
          rep         zfill,str8
          move        str8,n8
          if    over
.begin patch 1.1
.                    write     used,seq;*cdfon,mnum,compcomp,c0
                    write     used,seq;*cdfon,compnum,mnum,compcomp,c0,Ndatfld,Mlstname,Exchange,COMPRDTE,Status,samples,str8
.end patch 1.1
                      Clear      Exchange
                    goto      None
          endif
          add   c1 to RECORDS        
          loop          
          call      nordkg
          until over
          if        (omlrnum <> mnum)
          break
          endif
          add   c1 to RECORDS        
          pack  str9 from OODTEc,OODTEy,OODTEM,OODTED
          rep         zfill,str9
          move        str9,n9
           if         (N9 > n8)
          move        n9,n8
          move        n8,str8
          endif
          repeat
.begin patch 1.1
.          write     used,seq;*cdfon,mnum,compcomp,records
          write     used,seq;*cdfon,compnum,mnum,compcomp,records,ndatfld,mlstname,Exchange,COMPRDTE,Status,samples,str8
.end patch 1.1
          move      c0,records       
           Clear      Exchange
          endif
None
          add       c1,n4
          move      n4,str4
          pack      Mkey from str4,b3
          rep       Zfill in mkey
          repeat
          weof      unused,seq
          close     unused
          weof      used,seq
          close     used
stop


.


          stop
..........................................................................................................
.list name begins with 'A '
FixitA
        bump      Mlstname by 2
          clear     str75
          append    mlstname to str75
          append    "|A" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
        return
.
.list name begins with 'The '
FixitThe
        bump      Mlstname by 4
          clear     str75
          append    mlstname to str75
          append    "|The" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
        return
..........................................................................................................


          Include   Compio.inc
          include   cntio.inc          
          include   nordio.inc
.begin patch 1.1
          include     nxrfio.inc
          include     Ndatio.inc
.end patch 1.1
.Begin patch 1.2
          INCLUDE   NXNGIO.inc
.end patch 1.2
.Begin patch 1.21
           include    nsmpio.inc
.end patch 1.21
          include   comlogic.inc

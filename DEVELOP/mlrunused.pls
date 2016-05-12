PC        EQU       0
         Include             Common.inc
          Include             Cons.inc
         Include             Compdd.inc
          include   cntdd.inc          
          include   norddd.inc

Release             Init                "1.00"    .add order data
reldate   Init      "2013 October 24"
.Release             Init                "Pre"
KEYPAD1   INIT      "01L"
records   form      10
unused    file
used      file
          Prepare   Unused,"c:\work\UnusedMLr.dat",exclusive                                                       .mlr #'s not in master file
          Prepare   Used,"c:\work\UsedMLr.dat",exclusive                                                       .mlrs & ord history
          move      c3,nordlock
          move      c3,nmlrlock



          call      Paint
          move      c0,n4
          move      n4,str4
          pack      Mkey from str4,b3
          rep       Zfill in mkey

          loop
          call      Nmlrkey
          until     (n4 = "9999")
          if        over
          write unused,seq;mkey
          else
.add loop to read order file and get usage count          write out data
          Clear NORDFLD1
          Clear NORDFLD2
          Clear NORDFLD3
          Clear NORDFLD4
          PACK  NORDFLD1 FROM KEYPAD1,mnum
          MOVE  C2 TO NORDPATH
          CALL  NORDAIM
          if    over
                    write     used,seq;*cdfon,mnum,compcomp,c0
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
          repeat
          write     used,seq;*cdfon,mnum,compcomp,records
          move      c0,records       
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
          Include   Compio.inc
          include   cntio.inc          
          include   nordio.inc
          include   comlogic.inc

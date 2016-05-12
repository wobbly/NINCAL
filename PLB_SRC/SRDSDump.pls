PC EQU 0
          INCLUDE   common.inc
          INCLUDE   cons.inc
          include   ndatdd.inc
          include   nowndd.inc
RElease   INit      "zero"
reldate   Init      "Not"
Output    File

          Prepare   Output,"c:\work\SRDSTest.dat",exclusive
Loop      Loop
          call      Ndatseq
          until     over
          if        (status = "W")
          goto      loop
          endif
          Clear     Str4
          unpack    ownnum into str2,str4
          packkey   Nownfld,str4
          call      Nownkey
          if        Over
          Clear     OWNOCPY
          endif
          write     output,seq;*cdfon,lstnum,mlstname,OWNLON,OWNOCPY
          repeat
          Weof      Output,seq
          close     output
          shutdown
          stop




          include   ndatio.inc
          include   nownio.inc
          include   comlogic.inc
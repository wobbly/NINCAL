Pc        equ       0
          include   common.inc
          include   cons.inc
          include   ncntdd.inc
Release   Init      "Nope"
File      FIle          


          open      file,"\\nins1\e\data\text\nincnt.dat|NINS1:502",read
          move      c1,ncntpath
          loop
          read      file,seq;cntvars
          until     over
          if        (cntsales = "  ")
          packkey   ncntfld using CNTNUM
                    call      ncntkey
                    if        not over
                              if        (cntnum = "86")
                              move      "08",cntsales
                              else
                              move      "00",cntsales
                              endif
                    call      Ncntupd
                    endif
          endif
          repeat
          close     file
          stop
          include   ncntio.inc
          include   comlogic.inc          
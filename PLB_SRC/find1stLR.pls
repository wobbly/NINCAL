          include   common.inc
          include   cons.inc
          include   norddd.inc
PC        EQU       0
Release   Init      "0.00"
Reldate   Init      "not"
Two008    form      6
Two009    Form      6         


          MOve      "000000",nordfld
          move      c1,nordpath
          move      c3,nordlock
          move      "999999",two008
          move      "999999",two009
          display   *P1:1,*es
REad      Loop                
          Call      Nordseq
          until     Over
          if        (oodtey = "08" or oodtey = "09")
          call      goodone
          endif
          display   *P1:12,olrn
          repeat
          keyin     *p1:22,"2008 ",*DV,two008,"2009 ",*dv,Two009,str1
          stop
goodone
          if        (oodtey = "08")
          move      olrn,n6
                    if        (n6 < Two008)
                    move      n6,Two008
                    endif
          Elseif        (oodtey = "09")
          move      olrn,n6
                    if        (n6 < Two009)
                    move      n6,Two009
                    endif
          endif
          return
          include   nordio.inc
          include   comlogic.inc          

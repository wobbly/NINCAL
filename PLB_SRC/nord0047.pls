          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   ndatdd.inc

PC        EQU       0


.Calculates use by year of orders of managed vs outside lists
release   init      "Never"
output    file
ManYear1    form      5
ManYear2    form      5
ManYear3    form      5
ManYear4    form      5
ManYear5    form      5
OutYear1    form      5
OutYear2    form      5
OutYear3    form      5
OutYear4    form      5
OutYear5    form      5
          
          prepare   output,"c:\work\Nord0047.dat"
          move      C1,NORDPATH
          move      C1,NDATPATH
          call      Paint
          loop
                    call      NORDSEQ
                    until over
                    add       C1,N9
                    display   *p10:12,"records processed : ",n9
.                   if (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X")
                    if (OSTAT = "0" | OSTAT = "B")
                              pack      str8,oODTEC,OODTEY,OODTEM,OODTED
                              if (str8 > "20051231" AND str8 <= "20110101")
                                        pack      NDATFLD,OLNUM
                                        rep       zfill,NDATFLD
                                        call      NDATKEY
                                        if not over
                                                  if (ELSTCDE = "C" or ELSTCDE = "P")
                                                            call      Exclusive
                                                  else
                                                            call      Outside
                                                  endif
                                        endif
                              endif
                    endif
          repeat
          write     Output,seq;"2006"
          write     Output,seq;"      Exclusives: ",ManYear1
          write     Output,seq;"        Outsides: ",OutYear1
          write     Output,seq;
          write     Output,seq;"2007"
          write     Output,seq;"      Exclusives: ",ManYear2
          write     Output,seq;"        Outsides: ",OutYear2
          write     Output,seq;
          write     Output,seq;"2008"
          write     Output,seq;"      Exclusives: ",ManYear3
          write     Output,seq;"        Outsides: ",OutYear3
          write     Output,seq;
          write     Output,seq;"2009"
          write     Output,seq;"      Exclusives: ",ManYear4
          write     Output,seq;"        Outsides: ",OutYear4
          write     Output,seq;"2010"
          write     Output,seq;"      Exclusives: ",ManYear5
          write     Output,seq;"        Outsides: ",OutYear5
          stop

Exclusive
          if (str8 >= "20050101" AND str8 < "20060101")
                    add       C1,ManYear1
          elseif (str8 >= "20060101" AND str8 < "20070101")
                    add       C1,ManYear2
          elseif (str8 >= "20070101" AND str8 < "20080101")
                    add       C1,ManYear3
          elseif (str8 >= "20080101" AND str8 < "20090101")
                    add       C1,ManYear4
          else
                    add       C1,ManYear5
          endif
          return

Outside
          if (str8 >= "20050101" AND str8 < "20060101")
                    add       C1,OutYear1t
          elseif (str8 >= "20060101" AND str8 < "20070101")
                    add       C1,OutYear2
          elseif (str8 >= "20070101" AND str8 < "20080101")
                    add       C1,OutYear3
          elseif (str8 >= "20080101" AND str8 < "20090101")
                    add       C1,OutYear4
          Else
                    add       C1,OutYear6
          endif
          return

          include   nordio.inc
          include   ndatio.inc
          include   comlogic.inc

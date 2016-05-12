          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nrtndd.inc

PC        EQU       0

release   init      "temp"
output    file

OrderQty form       15.4(9999)
OrderNum form       9.4(9999)
NameArray dim       45(9999)
QtyTotal form       25.4
NumTotal form       15.4
PerTotal form       3.4
SEQEOF2   form      "-4"      

          prepare   output,"c:\work\datacheck8.dat"
          clock     timestamp,timestamp
          move      timestamp,str8
          move      str8,str9
          move      C1,NORDPATH
          move      C1,NRTNPATH
          move      C1,NORDLock
          move      C1,NRTNLock
          call      Paint
          CALL      NORDOPEN
          READ      NORDFILE,SEQEOF;ORDVARS
          loop
                    READ      NORDFILE,SEQEOF2;ORDVARS
                    until over
                    add       C1,N9
                    display   *p10:12,"records processed : ",N9
.                   if (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X")
                    if (OSTAT = "0" | OSTAT = "B")
                              pack      str2,OSALES10,OSALES
                              if (str2 <> "06")   .Sales Only!!
                                        reset     RUNCODES
                                        scan      OLNUM,RUNCODES
                                        if not equal
                                                  pack      str8,OODTEC,OODTEY,OODTEM,OODTED
                                                  if (str8 > "20111231" AND str8 <= str9)
                                                            call      Trim using ORTNNUM
                                                            if (ORTNNUM <> "" & ORTNNUM <> "0001")
                                                                      pack      NRTNFLD,ORTNNUM
                                                                      rep       zfill,NRTNFLD
                                                                      call      NRTNKEY
                                                                      if not over
                                                                                move      C0,N4
                                                                                move      RTNUM,N4
                                                                                if (N4 > 0)
                                                                                          add       C1,OrderNum(N4)
                                                                                          move      C0,howmany
                                                                                          move      OQTY,howmany
                                                                                          add       howmany,OrderQty(N4)
                                                                                          move      RTCOMP,NameArray(N4)
                                                                                          add       C1,NumTotal
                                                                                          add       howmany,QtyTotal
                                                                                endif
                                                                      endif
                                                            endif
                                                  elseif (str8 < "20111231")
                                                            break
                                                  endif
                                        endif
                              endif
                    endif
          repeat
andrew
          write     Output,seq;"Live/Billed Brokerage Orders from 1/1/2012 - Present, and their Return-To Companies (Running Charges Excluded)"
          write     Output,seq;
          move      NumTotal,str15
          call      FormatNumeric using str15,str25
          write     Output,seq;"     Total Number of Orders: ",str25
          write     Output,seq;
          move      QtyTotal,str25
          call      FormatNumeric using str25,str35
          write     Output,seq;"     Total Quantity:         ",str35
          write     Output,seq;
          for N4,"1","9999"
                    if (OrderNum(N4) > 0)
                              move      N4,str4
                              rep       zfill,str4
                              write     Output,seq;str4,"-",NameArray(N4)
                              move      OrderNum(N4),str9
                              call      FormatNumeric using str9,str11
                              call      Trim using str11
                              move      OrderQty(N4),str15
                              call      FormatNumeric using str15,str25
                              call      Trim using str25
                              write     Output,seq;"          Orders: ",str11
                              calc      PerTotal=((OrderNum(N4)/NumTotal)*100)
                              move      PerTotal,str8
                              call      Trim using str8
                              write     Output,seq;"               %: ",str8
                              write     Output,seq;"        Quantity: ",str25
                              calc      PerTotal=((OrderQty(N4)/QtyTotal)*100)
                              move      PerTotal,str8
                              call      Trim using str8
                              write     Output,seq;"               %: ",str8
                              write     Output,seq;
                    endif
          repeat
          stop

          include   nordio.inc
          include   nrtnio.inc
          include   comlogic.inc

          include   common.inc
          include   cons.inc
          include   norddd.inc
          include   nord5dd.inc
          include   nordpdd.inc

PC        EQU       0

release   init      "temp"
input     file
output    file
PRTLstat  Dim       1
          prepare   output,"c:\work\datacheck12.dat"
          move      C1,NORDPATH
          move      C1,NORD5PATH
          open      input,"e:\data\ninprintl.dat|10.10.30.103:502"
          write     Output,seq;"NINPRINTL Records which do not match OSTAT in NINORD"
          write     Output,seq;
          write     Output,seq;"  LR          LCR Status            Order Status"
          loop
                    READ      input,SEQ;ordvars
                    until over
                    add       C1,N9
                    move      OSTAT,prtLstat
                    display   *p10:12,"records processed : ",N9
                    pack      NORDFLD,OLRN
                    call      NORDKEY
                    if (OSTAT <> PrtLstat)
                              add       C1,N8
                              pack      nord5FLD,OLRN
                              call      NORD5KEY
                              if (nord5STAT = "01")
                                        pack      str25,"1st Request","                               "
                              elseif (nord5STAT = "02")
                                        pack      str25,"2nd Request","                               "
                              elseif (nord5STAT = "03")
                                        pack      str25,"Revised Request","                               "
                              elseif (nord5STAT = "04")
                                        pack      str25,"Approved","                               "
                              elseif (nord5STAT = "05")
                                        pack      str25,"Cancelled","                               "
                              elseif (nord5STAT = "06")
                                        pack      str25,"Pending","                               "
                              elseif (nord5STAT = "07")
                                        pack      str25,"Denied","                               "
                              elseif (nord5STAT = "08")
                                        pack      str25,"approved","                               "
                              else
                                        pack      str25,"Invalid Status-",NORD5STAT,"                               "
                              endif
                              if (OSTAT = "p")
                                        move      "Pending",STR55
                              elseif (OSTAT =     "x")
                                        move      "Cancelled Pending",STR55
                              elseif (OSTAT =     "0")
                                        pack      str55,"Live Order"
                              elseif (OSTAT =     "B")
                                        pack      str55,"Billed Order"
                              elseif (OSTAT =     "Q")
                                        pack      str55,"Cancelled-Billed"
                              elseif (OSTAT =     "X")
                                        pack      str55,"Cancelled Order"
                              elseif (OSTAT =     "e")
                                        pack      str55,"Live Order-est Inv"
                              elseif (OSTAT =     "l")
                                        pack      str55,"Clearance"
                              elseif (OSTAT =     "z")
                                        pack      str55,"Cancelled Clearance"
                              endif
..................................................


                              write     Output,seq;"  ",OLRN,"      ",str25,"    ",str55
                              If        (Ostat = "B" | Ostat = "Q" | Ostat = "X" | Ostat = "e" | Ostat = "x" | Ostat = "z")
                              Move      Ostat,str1
                              packkey   nordPFLD,Olrn
                              call      NordpKey
                              PACK      NORDPFLD1,"01X",OCO2CODE
                              PACK      NORDPFLD2,"02X",OLNUM
                              PACK      NORDPFLD3,"03X",OCOCODE
                              PACK      NORDPFLD4,"04X",OHIST

                              call      NordpDel
                              else
                              Move      Ostat,str1
                              packkey   nordPFLD,Olrn
                              call      NordpKey
                              move      str1,Ostat
                              call      NordpUpd
                              endif

                              MOve      Nord5stat,N1
                              If        (N1 < "1" or N1 > "8")              .invalid status
                                        If        (Ostat = "O" | Ostat = "B" | Ostat = "Q" | Ostat = "X" | Ostat = "e" )
                                        move      "08",Nord5stat
                                        call      Nord5upd
                                        elseif    (Ostat = "l")
                                        move      "01",Nord5stat
                                        call      Nord5upd
                                        elseif    (Ostat = "z")
                                        move      "05",Nord5stat
                                        call      Nord5upd
                                        elseif    (Ostat = "x")
                                        move      "05",Nord5stat
                                        call      Nord5upd
                                        elseif    (Ostat = "p")
                                        move      "06",Nord5stat
                                        call      Nord5upd
                                        else
                                        call      Nord5Del
                                        endif
                              endif
                              
                    endif
          repeat
andrew
          move      N9,str9
          call      FormatNumeric using str9,str11
          write     Output,seq;"Total Number of Records Found:      ",str11
          move      N8,str8
          call      FormatNumeric using str8,str11
          write     Output,seq;"Total Number of Incorrect Records:  ",str11
          write     Output,seq;
          write     Output,seq;"DONE"
          shutdown

          include   nordio.inc
          include   nord5io.inc
          include   nordpio.inc
          include   comlogic.inc

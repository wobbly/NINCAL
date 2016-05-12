.convert order files
.ninord
.ninprint
.ninprintL
Pc        Equ       0          
          Include   Common.inc          
          include   cons.inc
          INclude   norddd.inc
          include   Compdd.inc
          include   cntdd.inc
          
Release   init      "temp1"
Reldate   init      "28 Jan 2009"
.HAVE TO COMVERT the old broker contact to the new company contact
.pass one mater file
Output1    File      Fixed=408
Output2    File      Fixed=408
Output3    File      Fixed=408
OrdErr    FIle      Fixed=408
Input     FIle      
NewMlr    Dim       6
NewBrk    Dim       6
Newfill   dim       61        348-408     
          Prepare   Output1,"E:\data\text\ninord.conv|10.10.30.103:502",Exclusive
          OPen      INput,"ninord.dat|10.10.30.103:502"
          PRepare   OrdErr,"e:\data\text\orderErrors.conv|10.10.30.103:502",Exclusive
          Move      "T",CompDntCare
          
Order
          read      Input,seq;ORCODE:
                        OSTAT:
                        OMLRNUM:
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        OLON:   
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        OBRKNUM:
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    ofiller
                    
                    GOto      Endof1 if over
                    packkey      Mkey from Omlrnum,ocobn
                    call      Nmlrkey
                    If        Over
                              if        (omlrnum = "1966")               .warwick
                              move      "000193",compnum
                              Elseif     (omlrnum = "5604")               .OMP
                              move      "005198",compnum
                              Elseif     (omlrnum = "5881")               .Epsilon
                              move      "000181",compnum
                              Elseif     (omlrnum = "6779")               .Coplon
                              move      "000164",compnum
                              Elseif     (omlrnum = "8743")               .adv mrktn direct
                              move      "007619",compnum
                              Elseif     (omlrnum = "9548")               .WHRO
                              move      "001301",compnum
                              else
.                    alert     caution,"Mailer Read Failed! writing to error file",result
                              write     OrdErr,seq;ordvars
                              move      "0000",COmpnum
                              endif
                    endif
                    Move      Compnum,NewMLr
                    move      "000000",Newbrk
                    if        (obrknum <> "0000" & obrknum <> "" & obrknum <> "    " )
                              call      Trim using obrknum                      .some old orders are messed up
                                        count     n2,obrknum
                                        if        (n2 = 1)
                                        pack      str4 from "000",obrknum
                                        elseif    (n2 = 2)
                                        pack      str4 from "00",obrknum
                                        elseif    (n2 = 3)
                                        pack      str4 from "0",obrknum
                                        else
                                        move      obrknum,str4
                                        endif
                                        move      str4,obrknum
                                        rep       Zfill,obrknum
                                        Type      Obrknum
                                        if        not equal                   .at least one order with '000N'
                                        move      "0000",obrknum
                                        endif
                              packkey      Nbrkfld from Obrknum,Obrkcnt
                              rep       Zfill,nbrkfld
                              if        (obrknum <> "0000")
                                        call      Nbrkkey
                                        If        Over
                                                  if        (obrknum = "0269")
                                                  move      "000230",compnum
                                                  Elseif        (obrknum = "0256")          .does not exist
                                                  move      "000000",compnum
                                                  Elseif        (obrknum = "0357")          .heldref
                                                  move      "000309",compnum
                                                  Elseif        (obrknum = "0428")          .smithsonian rap
                                                  move      "000378",compnum
                                                  Elseif        (obrknum = "0440")          .Gull Rock
                                                  move      "000390",compnum
                                                  Elseif        (obrknum = "0451")          .Sierra Club
                                                  move      "000401",compnum
                                                  Elseif        (obrknum = "0642")          .Amer Farm Trust
                                                  move      "000590",compnum
                                                  Elseif        (obrknum = "5172")          .Unknown
                                                  move      "000000",compnum
                                                  Elseif        (obrknum = "5366")          .Unknown
                                                  move      "000000",compnum
                                                  Elseif        (obrknum = "0673")          .TNC
                                                  move      "000619",compnum
                                                  Elseif        (obrknum = "0747")          .Vt comp store
                                                  move      "000690",compnum
                                                  Elseif        (obrknum = "1361")          .Unknown
                                                  move      "000000",compnum
                                                  Elseif        (obrknum = "0808")          .Blackwell
                                                  move      "000749",compnum
                                                  Elseif        (obrknum = "0817")          .NY Rev books
                                                  move      "000758",compnum
                                                  Elseif        (obrknum = "0885")          .Real goods
                                                  move      "003095",compnum
                                                  Elseif        (obrknum = "0853")          .enlightennext
                                                  move      "008875",compnum
                                                  else
                                                  alert     caution,"Broker Read Failed!",result
                                                  stop
                                                  endif
                                        endif
                              endif          
                    Move      Compnum,NewBrk
                    endif
          unpack    oodnum into str4,str3
          pack      cnctfld4 from Obrknum,obrkcnt
          rep       Zfill,nbrkfld
          if        (obrknum <> "0000")       
          call      CNCTKEY2          
                    if        over
                    move      "000",obrkcnt
                    else
                    move      CNCTID,obrkcnt
                    endif
          endif                    

          Write      Output1,seq;ORCODE:
                        OSTAT:
                        NewMLR:                   *New
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        "00":                      *new filler for expanded owner      
                        OLON:   
                        "000":                    *New owner contact field      
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        NewMLr:
                        Str3:
.                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        "00":                     *new  for expanded return-to      
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        NewBrk:                   *New
                        OBRKCNT:                   *New
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    NewFIll
          Goto      Order
Endof1    Weof      Output1,seq
          Close      Output1
          weof      OrdErr,seq
          CLose     OrdErr
          Prepare   Output2,"E:\data\ninPRint.conv|10.10.30.103:502",Exclusive
          OPen      INput,"ninprint.dat|10.10.30.103:502"


Print          
          read      Input,seq;ORCODE:
                        OSTAT:
                        OMLRNUM:
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        OLON:   
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        OBRKNUM:
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    ofiller
                    GOto      Endof2 if over
                    packkey      Mkey from Omlrnum,ocobn
                    call      Nmlrkey
                    If        Over
                    alert     caution,"Mailer Read Failed!",result
                    stop
                    endif
                    Move      Compnum,NewMLr
                    move      "000000",Newbrk
                    if        (obrknum <> "0000" & obrknum <> "" & obrknum <> "    " )
                              packkey      Nbrkfld from Obrknum,Obrkcnt
                              call      Nbrkkey
                              If        Over
                    alert     caution,"Broker Read Failed!",result
                              stop
                              endif
                    Move      Compnum,NewBrk
                    endif

          unpack    oodnum into str4,str3
          pack      cnctfld4 from Obrknum,obrkcnt
          rep       Zfill,nbrkfld
          if        (obrknum <> "0000")       
          call      CNCTKEY2          
                    if        over
                    move      "000",obrkcnt
                    else
                    move      CNCTID,obrkcnt
                    endif
          endif                    

          Write      Output2,seq;ORCODE:
                        OSTAT:
                        NewMLR:                   *New
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        "00":                      *new filler for expanded owner      
                        OLON:   
                        "000":                    *New owner contact field      
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        NewMLr:
                        Str3:
.                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        "00":                     *new  for expanded return-to      
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        NewBrk:                   *New
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    NewFIll
          Goto      PRint
Endof2    Weof      Output2,seq
          Close      Output2
          
          Prepare   Output3,"E:\data\ninPRintl.conv|10.10.30.103:502",Exclusive
          OPen      INput,"ninprintl.dat|10.10.30.103:502"


PrintL          
          read      Input,seq;ORCODE:
                        OSTAT:
                        OMLRNUM:
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        OLON:   
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        OBRKNUM:
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    ofiller
                    GOto      Endof3 if over
                    packkey      Mkey from Omlrnum,ocobn
                    call      Nmlrkey
                    If        Over
                    alert     caution,"Mailer Read Failed!",result

                    stop
                    endif
                    Move      Compnum,NewMLr
                    move      "000000",Newbrk
                    if        (obrknum <> "0000" & obrknum <> "" & obrknum <> "    " )
                              packkey      Nbrkfld from Obrknum,Obrkcnt
                              call      Nbrkkey
                              If        Over
                    alert     caution,"Broker Read Failed!",result
                              stop
                              endif
                    Move      Compnum,NewBrk
                    endif

          unpack    oodnum into str4,str3
          pack      cnctfld4 from Obrknum,obrkcnt
          rep       Zfill,nbrkfld
          if        (obrknum <> "0000")       
          call      CNCTKEY2          
                    if        over
                    move      "000",obrkcnt
                    else
                    move      CNCTID,obrkcnt
                    endif
          endif                    

          Write      Output3,seq;ORCODE:
                        OSTAT:
                        NewMLR:                   *New
                        OLRN:   
                        OCOBN:  
                        OLNUM:  
                        "00":                      *new filler for expanded owner      
                        OLON:   
                        "000":                    *New owner contact field      
                        OMLRPON:
                        OQTY:   
                        OPPM:   
                        OMLRKY: 
                        OFOCODE:
                        ORTNDTEC:
                        ORTNDTEY:
                        ORTNDTEM:
                        ORTNDTED:
                        OMDTEC:
                        OMDTEY:
                        OMDTEM:  
                        OMDTED:    
                        OTOCODE: 
                        OSOTCODE:
                        OCCODE:  
                        OLRNCO:  
                        OODTECOC:
                        OODTECOY:
                        OODTECOM:
                        OODTECOD:
                        OQTYCO:  
                        OSPI:    
                        OBildrct:
                        OBRKGUAR:
                        OELCODE: 
                        NewMLr:
                        Str3:
.                        OODNUM:  
                        OODES:
                        ONETQTY:
                        OCAMP:
                        OCLRSTAT:
                        OCLRINIT:
                        OBRKRPT:
                        OCLRDTEC:
                        OCLRDTEY:
                        OCLRDTEM:
                        OCLRDTED:
                        ORENT:
                        OHIST:
                        OXPPM:   
                        "00":                     *new  for expanded return-to      
                        ORTNNUM: 
                        OTAPERET:
                        OUQTY:   
                        OSALES10:
                        osales:
                        OCOCODE:
                        OCO2CODE: 
                        OODTEC:
                        OODTEY:
                        OODTEM:  
                        OODTED:    
                        OSCODE:  
                        OCOMSLCT:
                        OSHP:    
                        O1DES:   
                        O2DES:   
                        OREUSE:  
                        ODOWJ:   
                        OEXQTY:  
                        GUARCODE:
                        NewBrk:                   *New
                        OBRKCNT:
                        OSAMCDE:
                        onetper:
                        onetrc:
                        onetfm:
                        onetmin:
                    OFullFil:
                    OCompID:
                    OCompID2:
                    NewFIll
          Goto      PRintL
Endof3    Weof      Output3,seq
          Close      Output3
          stop
          include   compio.inc
          include   cntio.inc
.          include   nmlrio.inc
.          include   nbrkio.inc
          include   comlogic.inc
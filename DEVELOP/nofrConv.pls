.convert offer file
.ninoffer
.ninprint
.ninprintL
Pc        Equ       0          
          Include   Common.inc          
          include   cons.inc
          include   nofrdd.inc
          include   Compdd.inc
          include   cntdd.inc
release   init      "not"
reldate   init      "26 Jan 09"
Output1    File      Fixed=70
Input     FIle      

NewMlr    Dim       6
          Prepare   Output1,"e:\data\text\ninofr.conv|NINS1:502",Exclusive
          OPen      INput,"ninofr.dat|NINS1:502"
          Move      "T",CompDntCare
          
Offer

          Read      INput,seq;ofrvars
          goto      Eoj if over
                    packkey      Mkey from OFMLR,"000"
                    call      Nmlrkey
                    If        Over
                              if        (OFMLR = "1966")               .warwick
                              move      "000193",compnum
                              Elseif     (OFMLR = "5604")               .OMP
                              move      "005198",compnum
                              Elseif     (OFMLR = "5881")               .Epsilon
                              move      "000181",compnum
                              Elseif     (OFMLR = "6779")               .Coplon
                              move      "000164",compnum
                              Elseif     (OFMLR = "8743")               .adv mrktn direct
                              move      "007619",compnum
                              Elseif     (OFMLR = "9548")               .WHRO
                              move      "001301",compnum
                              else
                    alert     caution,"Mailer Read Failed! writing to error file",result
                              move      "0000",COmpnum
                              endif
                    endif
                    Move      Compnum,NewMLr


          Write     Output1,seq;ofcode:
                    Newmlr:
                    ofnum:
                    ofdesc:
                    ofname:
                    OFDATE 
                    
          goto      Offer

EOJ       Weof      Output1,seq
             Close      Output1
          Stop      
          include   Compio.inc
          include   cntio.inc

          include   COmlogic.inc
          
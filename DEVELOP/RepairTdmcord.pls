pc        equ       0
          include common.inc
          include   cons.inc

release   init      "not"
reldate   init      "07 September 2011"
FIle      FIle 
tdmcord   Ifile     keyl=6
LR        Dim       6


          open      FIle,"\\nts1\e\data\diskin50.dat"
          open      Tdmcord,"tdmcord|NINS1:502"
          loop
          read      File,seq;str6,LR
          until     over
          write     TDmcord,lr;lr,"0"
          repeat
          stop
          include   Comlogic.inc          
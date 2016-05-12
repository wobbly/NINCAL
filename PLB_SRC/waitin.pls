pc        Equ       0
          include   common.inc
          include   cons.inc
Release   Init      "test"
          
          move      "1000",str4
          call      waitin using str4

          Display   *p1:20,time1," ",time2," ",time3," ",n10
          keyin     *p1:14,*el,str1
          shutdown          
          
          include   comlogic.inc
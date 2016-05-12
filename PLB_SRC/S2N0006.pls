Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          Include   SrdsDd.inc
Release   INit      "Devo"    DLH
REldate   Init      "29 August 2011"
Input    File
Input2    IFile      keyl=6
str6a     Dim       6
.Code: 
.s2n0005 --- celanup LO
 
.
 
          Open      Input,"e:\data\text\SRDS_MAIN.dat|NINS1:502",read
          Open      Input2,"SRDS_MAIN12|NINS1:502",read
          Move      c0 to N5
          Display   *p1:1,*el,"Processing  ",*w2

          Loop      
          read      Input,seq;SRDSVARS
          until     over
          call      Trim using SRDSOWNNUM
          if        (SRDSOWNNUM = "")
          call      debug
          read      Input2,SRDSLSTNUM;str6,SRDSOWNNUM
          call      Trim using SRDSOWNNUM
          if        (SRDSOWNNUM = "")
          else
                    packkey   SRDSFLD,SRDSLSTNUM
                    call      Srdstst
                    call      SRDSUPD
                    endif
          endif
          Repeat
          stop
          

          include   SrdsIO.inc                             .selects,addressing, etc
          include   Comlogic.inc





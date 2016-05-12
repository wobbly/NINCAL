Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          Include   Nrefdd.inc
          include   S2ndd.inc                        
          include   SRDSxSLTDD.inc
Release   INit      "Devo"    DLH
REldate   Init      "26 August 2011"
Input    File
str6a     Dim       6
.Code: 
.s2n0005 --- add matched lists to SRDS2NIN
 
.
 
          Open      Input,"c:\work\Tests2n.txt",read
          Move      c0 to N5
          Display   *p1:1,*el,"Processing Tests2n.txt ",*w2

          Loop      
          read      Input,seq;str6,b1,str6a
          until     over
          move      str6,S2NSRDS
          Clear     S2NStatus
          move      str6a,S2NNIN
          call      S2nWRT
          Repeat
          stop
          

          include   S2nio.inc                             .selects,addressing, etc
          include   Comlogic.inc





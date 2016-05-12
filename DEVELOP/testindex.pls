


work250   dim       250
path      dim       250       
path1      dim      250
compactoption       dim       10
fmadrs    dim       50


          move      "e:\data\text",path
          move      "e:\data\index",path1
          clear     fmadrs
          clear     compactoption
          
          pack  work250 with path,"\file.dat,",path1,"\file.i01,L230 -N",compactoption,",10-15,16-17,18-19"
          
          index work250



          pack  work250 with path,"\file.dat,",path1,"\file.i02,L230 -N",compactoption,",10-15,16-17,18-19"


          index work250,SUNDM=FMadrs 
          stop


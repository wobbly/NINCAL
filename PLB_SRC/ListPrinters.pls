PC        equ       0          
          include   common.inc
          include   cons.inc
Release   Init      "test"        
DLPrinter Datalist
output    File
DLresult  form 9
DLndx     form 9
dmFileName          dim 80


          create    DLPrinter=1:50:1:50,Visible=1

          getinfo printers,DLprinter
          DLPRINTER.GetCount giving DLresult     
           
          Prepare   output,"c:\work\printers.dat" 
          

         for DLndx from 0 to DLresult
         dlPrinter.GetText giving dmFileName using *Index=DLndx
          Write     output,seq;Dmfilename
         repeat
          weof      output,seq
          close     output


          Path      Exist,"c:\program files (x86)"              .=program files (x86)  64 bit os
          
          keyin     *p1:1,str1
          stop
          
          include   comlogic.inc          


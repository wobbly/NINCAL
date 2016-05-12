PC        Equ       0          
          Include   Common.inc
          include   Cons.inc
          INclude   Srdstxtdd.inc
          include   ntxt1dd.inc
          Include   S2Ndd.inc
Release   Init      "Not"
reldate   Init      "Never"
InFIle    File      
File      File
.SRDSStatus          Dim       1
Dim75               Dim       75
Str50a    dim       50
Str50b    dim       50
Str50c    dim       50
Str50d    dim       50
Str50e    dim       50
str250    dim       250

          Goto      Pass2

          Prepare   File,"c:\work\srds_txt_new.dat"
          open      Infile,"srds_Txt.save|NINS1:502"
          Loop
          read      Infile,seq;SRDSTXTLSTNUM,str250
.          call      srdstxtseq
          until     over
          call      trim using str250
          unpack    str250 into str50a,str50b,str50c,str50d,str50e          
          call      trim using str50a
          cmatch    b1,str50a
          if        not eos
          write     File,Seq;srdstxtlstnum,"01",str50a
          endif                    
          call      trim using str50b
          cmatch    b1,str50b
          if        not eos
          write     File,Seq;srdstxtlstnum,"02",str50b
          endif                    
          call      trim using str50c
          cmatch    b1,str50c
          if        not eos
          write     File,Seq;srdstxtlstnum,"03",str50c
          endif                    
          call      trim using str50d
          cmatch    b1,str50d
          if        not eos
          write     File,Seq;srdstxtlstnum,"04",str50d
          endif                    
          call      trim using str50e
          cmatch    b1,str50e
          if        not eos
          write     File,Seq;srdstxtlstnum,"05",str50e
          endif                    
          
          repeat    
          WEof      File,seq
          CLose     File
          Stop
Pass2
          Loop
loopy     call      srdstxtseq
          until     over
          if        (s2nfld <> SRDStxtLSTNUM)
          packkey   S2NFld,SRDStxtLSTNUM
          call      S2NKey
          goto      loopy if over
          endif
          if        (s2nnin = "" or S2nnin = "      ")
          goto      loopy
          endif
          move      S2NNIN,ntxt1list
          move      srdstxtnum,ntxt1num
          move      srdstxt,ntxt1text
          packkey   ntxt1fld from ntxt1list
          packkey   ntxt1fld1 from ntxt1list,ntxt1num
          call      ntxt1wrt
          repeat

          stop



          include   Srdstxtio.inc
          include   ntxt1io.inc
          Include   S2Nio.inc
          include   comlogic.inc
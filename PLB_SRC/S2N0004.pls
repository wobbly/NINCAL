Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          Include   Nrefdd.inc
          include   SRDSSLtdd.inc                             .selects,addressing, etc
          include   SRDSxSLTDD.inc
Release   INit      "Devo"    DLH
REldate   Init      "24 June 2011"
Output    File
.Code: 
.s2n0004 --- cleanup SRDS_slt file by adding nin codes  logic to be added to S2N0001
 
.
 
          Prepare   output,"c:\work\srds_slt.new"
          Move      c0 to N5
          Display   *p1:1,*el,"Processing SRDS_slt ",*w2

          TRAP      IOMssg Giving Error if IO
          move           "READ - srds_slt",Location
          pack           KeyLocation,"seq: ",seq
          move      c3,nrefpath
          Loop      
          call      SRDSSLTSEQ
          until     over
          call      Trim using SRDSSLTDES
          Packkey   SRDSxSLTfld,SRDSSLTDES
          call      SRDSxSLTKey
          if        Not over
          call      Trim using SRDSxSLTDESc
          packkey   Nreffld4,SRDSxSLTDESC
          else
          packkey   Nreffld4,SRDSSLTDES
          endif
          
          call      Nrefkey
          if        not over
          move      NREFCODE,SRDSSLTTYPE
          move      NREFNUM,SRDSSLTNUM
          endif
          write     Output,seq;SRDSSLTVARS
          Repeat
          weof      output,seq
          close     output
          stop
          
          Include   Nrefio.inc
          include   SRDSSLtio.inc                             .selects,addressing, etc
          include   SRDSxSLTIO.inc

          include   Comlogic.inc





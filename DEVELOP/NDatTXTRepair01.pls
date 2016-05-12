Pc         Equ        0
           Include    Common.inc
           include    Cons.inc
           include    NTXTdd.inc
           INclude    Ndatdd.inc
Release    INit       "0"
count1     Form       6
count2     Form       6
File       File
str59      DIm        59         
.          goto       Ndatconv
.          OPen       File,"NINTXT.dat|NINS1:502",read
           Prepare    File,"\\nins1\e\data\text\NINTXT.TEST|NINS1:502",exclusive
           For        N6 from c1 to "999999" using c1
           move       c1,N2
           pack       ntxtfld using N6,N2
           rep        zfill in ntxtfld
           call       Ntxtkey
           If         Not over
           write      File,Seq;NTXTVARS

                      loop
                      add        c1,n2
                      pack       ntxtfld using N6,N2
                      rep        zfill in ntxtfld
                      call       Ntxtkey
                      If         Not over
                      write      File,Seq;NTXTVARS
                      else
                      Break
                      endif
                      repeat
           endif

           repeat

.           add       c1,count1
.          display    *p10:12,Count1
.          cmatch     B1,Ntxtnum
.          if         equal
.          packkey    NtxtFld,NtxtList,Ntxtnum
.          call       NtxtTst
.                     IF         not over                 .should not be over
.                     rep        Zfill in NTxtnum
.                     call       NtxtUpd
.                     endif
.          else
.          endif
.          REpeat
           weof       File,seq   
           Close      File
.          goto       NdatConv
           Stop

           
           
           INclude    NTxtIO.inc
           INclude    NdatIO.inc
           Include    Comlogic.inc
           
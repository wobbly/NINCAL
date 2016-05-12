.Order Sample file Create
.2016 March 23 - read live LOL and Campaign files and create a secondary files with sample information
.2015 August 21 - read live order file and create a secondary file with sample information
pc         equ        0
           include    common.inc
           include    cons.inc
           include   norddd.inc
           include    Nordsdd.inc
           include    NCMPdd.inc
           include    NLOLdd.inc
           include    NCMPsdd.inc
           include    NLOLsdd.inc
release    Init       "develop"
           move       c1,Nordpath
           move       c3,Nordlock
           goto       looper2
Looper     loop       
          
           call       Nordseq
           until      over
           call       Trim using OSCODE
           if         (OSCODE = "")
           goto       Looper
           endif
           move       osamcde,NORDSNUM
           rep        zfill,nordsnum
           move       OLRN,NORDSLR
           move       OSCODE,NORDSCODE
           MOve       "00",NORDSREC           .values 00-99
           call       NordsWrt
           repeat
Looper1     loop       
           
           call       NCMpSeq
           until      over
           call       Trim using NCMPSample
           if         (NCMPSample = "")
           goto       Looper1
           endif
           move       NCMPSample,NCMPSNUM
           rep        zfill,NCMPSNUM
           move       NCMPNum,NCMPSLR
           MOve       "00",NCMPSREC           .values 00-99
           call       NcmpsWrt
           repeat
           move       c1,nlolpath
Looper2     loop       
           
           call       NLOLseq
           until      over
           call       Trim using NLOLSample
           if         (NLOLSample = "")
           goto       Looper2
           endif
           move       NLOLSample,NLOLSNUM
           rep        zfill,nLOLsnum
           move       NLOLLOL,NLOLSLOL
           MOve       "00",NLOLSREC           .values 00-99
           call       NLOLsWrt
           repeat

           shutdown   "cls"


           include   nordio.inc
           include Nordsio.inc
           include    NCMPio.inc
           include    NLOLio.inc
           include    NCMPsio.inc
           include    NLOLsio.inc
           include    comlogic.inc
Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          include   MDCmaindd.inc
          include   MDcSegdd.inc
          Include   MDCMSCDD.inc        
          include   mdc035dd.inc
          include   mdc090dd.inc
          include   mdctxtdd.inc
          include   mdc060dd.inc
Release   INit      "1.00"    DLH
REldate   Init      "07 September 2006"
.THis program is the "XX" run to auto update list information from MIN List system CSV format to  index/aimdex
.first pass kicks out "printed" exception reports.
.
.open and read mdc_main.csv file read Min2NIN xref and  to MDC_Main.dat and 
.supplemental files so they can be cross referenced and applied.
.
.         goto      loop6
          Move      c0 to N5
          Display   *p10:11,*el,"Processing MDC_Main "
          Trap      NoLoop1 Giving Error If IO
          Open      MinSfile,"c:\work\min\MDC_main.csv",exclusive
          TrapClr   IO
.         

.loop1 process Min_Main
Loop1     
          loop
          Read      Minsfile,seq;*cdfon,MinMainVARS1
          until     over
          add       c1,N5
          Display   *p35:11,N5
          mOVE      MinMainVARS1,MinMainVARS
          count     n2,DDCNOa
          if        (n2 = c1)
          pack      str6 from "00000",ddcnoa
          Elseif    (n2 = c2)
          pack      str6 from "0000",ddcnoa
          Elseif    (n2 = c3)
          pack      str6 from "000",ddcnoa
          Elseif    (n2 = c4)
          pack      str6 from "00",ddcnoa
          Elseif    (n2 = c5)
          pack      str6 from "0",ddcnoa
          Elseif    (n2 = c6)
          pack      str6 from ddcnoa
          endif
          
          packkey   Minfld,str6
          move      str6,ddcnoa
          call      MInTst
          if        over
          call      Minwrt
          Else
          call      MinUpd
          endif
          
          Repeat
          Close     MinSfile
          Close     Minfile
          Goto      Loop2
NoLoop1
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:11,"MDC_MAIN Not found - continuing"
          endif
.loop2 process Min_Seg
Loop2
          Move      c0 to N5
          Display   *p10:12,*el,"Processing MDC_SeG "
          Trap      NoLoop2 giving error if IO
          Open      MSegSfile,"c:\work\min\MDC_SEG.csv",exclusive
          TrapClr   IO
          Loop
          Read      MSegSfile,seq;*cdfon,MSegVARS1
          until     over
          add       c1,N5
          Display   *p35:12,N5
          mOVE      MSEgVARS1,MSegVARS
          count     n2,SGDCNO
          if        (n2 = c1)
          pack      str6 from "00000",SGDCNO
          Elseif    (n2 = c2)
          pack      str6 from "0000",SGDCNO
          Elseif    (n2 = c3)
          pack      str6 from "000",SGDCNO
          Elseif    (n2 = c4)
          pack      str6 from "00",SGDCNO
          Elseif    (n2 = c5)
          pack      str6 from "0",SGDCNO
          Elseif    (n2 = c6)
          pack      str6 from SGDCNO
          endif
          
          packkey   MSegfld,str6
          move      str6,SGDCNO
          call      MSegTst
          if        over
          call      MSegWrt
          Else
          MOVe      SGSQNO,N2
                    IF        (n2 = c0)
                    call      Loop2Del
                    call      MsegWRt
                    else
                    call      MsegWRt
                    endif
.         call      MSegUpd
          endif
          
          Repeat
          Close     MSEGSfile
          Close     MSEGfile
          Goto      Loop3
Loop2Del
          call      MSegDel
          call      Msegtst
          if        over
          return
          endif
          goto      Loop2Del
NoLoop2
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:12,"MDC_Seg Not found - continuing"
          endif

.loop3 process Min_MSC
Loop3
          Move      c0 to N5
          Display   *p10:13,*el,"Processing MDC_MSC "

          Trap      Noloop3 giving error if IO
          Open      MMSCSFILE,"c:\work\min\MDC_MSC.csv",exclusive
          TrapClr   IO
          Loop
          Read      MMSCSfile,seq;*cdfon,MMSCVARS
          until     over
          add       C1,N5
          Display   *p35:13,N5
          count     n2,DDCno
          if        (n2 = c1)
          pack      str6 from "00000",DDCno
          Elseif    (n2 = c2)
          pack      str6 from "0000",DDCno
          Elseif    (n2 = c3)
          pack      str6 from "000",DDCno
          Elseif    (n2 = c4)
          pack      str6 from "00",DDCno
          Elseif    (n2 = c5)
          pack      str6 from "0",DDCno
          Elseif    (n2 = c6)
          pack      str6 from DDCno
          endif
          
          packkey   MMSCFld,str6
          move      str6,DDCNO
          call      MMSCTst
          if        over
          call      MMSCWrt
          Else
          call      MMSCUpd
          endif
          
          Repeat
          Close     MMSCfile
          Close     MMSCfile
          Goto      Loop4
NoLoop3
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:13,"MDC_MSC not found - continuing"
          endif

.loop4 process Min_035
Loop4     
          Move      c0 to N5
          Display   *p10:14,*el,"Processing MDC_035 "
          Trap      Noloop4 giving error if IO
          Open      M035Sfile,"c:\work\min\MDC035.csv",exclusive      
          TrapClr   IO
          Loop
          Read      M035Sfile,seq;*cdfon,M035VARS1
          until     over
          Add       C1,n5
          Display   *p35:14,n5
          mOVE      M035VARS1,M035VARS
          count     n2,CADCNO
          if        (n2 = c1)
          pack      str6 from "00000",CADCNO
          Elseif    (n2 = c2)
          pack      str6 from "0000",CADCNO
          Elseif    (n2 = c3)
          pack      str6 from "000",CADCNO
          Elseif    (n2 = c4)
          pack      str6 from "00",CADCNO
          Elseif    (n2 = c5)
          pack      str6 from "0",CADCNO
          Elseif    (n2 = c6)
          pack      str6 from CADCNO
          endif
          
          packkey   M035Fld,str6
          move      str6,CADCNO
          call      M035Tst
          if        over
          call      M035Wrt
          Else
                    IF        (CasQNo = c1)
                    call      Loop4del
                    call      M035wrt
                    Else
                    call      M035wrt
                    endif
.         call      M035Upd
          endif
          
          Repeat
          Close     M035Sfile
          Close     M035file
          Goto      Loop5
Loop4Del
          Call      M035Del
          call      M035tst
          if        over
          return
          endif
          goto      Loop4del
NoLoop4
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:14,"MDC_035 not found - continuing"
          endif
......    
.loop5 process Min_090   .Owner
Loop5     
          Move      c0 to N5
          Display   *p10:15,*el,"Processing MDC_090 "
          Trap      Noloop5 giving error if IO
          Open      M090Sfile,"c:\work\min\MDC090.csv",exclusive      
          TrapClr   IO
          Loop
          Read      M090Sfile,seq;*cdfon,M090VARS
          until     over
          Add       C1,n5
          Display   *p35:15,N5
          count     n2,Gkey
          if        (n2 = c1)
          pack      str5 from "0000",GKey
          Elseif    (n2 = c2)
          pack      str5 from "000",GKey
          Elseif    (n2 = c3)
          pack      str5 from "00",GKey
          Elseif    (n2 = c4)
          pack      str5 from "0",GKey
          Elseif    (n2 = c5)
          pack      str5 from GKey
          endif
          
          packkey   M090Fld,str5
          move      str5,GKey
          call      M090Tst
          if        over
          call      M090Wrt
          Else
          call      M090Upd
          endif
          
          Repeat
          Close     M090Sfile
          Close     M090file
          goto      Loop6
NoLoop5
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:15,"MDC_090 not found - continuing"
          endif
.         
.Loop6 TXT file
Loop6     
          Move      c0 to N6
          Display   *p10:16,*el,"Processing MDC_TXT "
          Trap      NoLoop6 giving error if IO
          Open      MTxtSfile,"c:\work\min\MDC_TXT.csv",exclusive
          TrapClr   IO
          Loop
          Read      MTXTSfile,seq;*cdfon,MTXTVARS1
          until     over
          Add       c1,n6
          Display   *p34:16,N6
          mOVE      MTXTVARS1,MTXTVARS
          count     n2,TXDCNO
          if        (n2 = c1)
          pack      str6 from "00000",TXDCNO
          Elseif    (n2 = c2)
          pack      str6 from "0000",TXDCNO
          Elseif    (n2 = c3)
          pack      str6 from "000",TXDCNO
          Elseif    (n2 = c4)
          pack      str6 from "00",TXDCNO
          Elseif    (n2 = c5)
          pack      str6 from "0",TXDCNO
          Elseif    (n2 = c6)
          pack      str6 from TXDCNO
          endif
          
          packkey   MTXTFld,str6
          move      str6,TXDCNO
          call      MTXTTst
          if        over
          call      MTXTWrt
          Else
                    IF        (TGSQNO = c1)
                    call      Loop6del
                    call      MTXTwrt
                    Else
                    call      MTXTwrt
                    endif
          endif
          
          Repeat
          Close     MTXTSfile
          Close     MTXTfile
          Goto      Loop7
Loop6Del
          Call      MTXTDel
          call      MTXTtst
          if        over
          return
          endif
          goto      Loop6del
NoLoop6                       
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:16,"MDC_TXT not found - continuing"
          endif
.loop7 process Min_060   .Categories
Loop7     
          Move      c0 to N5
          Display   *p10:17,*el,"Processing MDC_060 "
          Trap      Noloop7 giving error if IO
          Open      M060Sfile,"c:\work\min\MDC060.csv",exclusive      
          TrapClr   IO
          Loop
          Read      M060Sfile,seq;*cdfon,M060VARS
          until     over
          Add       c1,n5
          Display   *p35:17,N5
          packkey   M060Fld,FC
          call      M060Tst
          if        over
          call      M060Wrt
          Else
          call      M060Upd
          endif
          
          Repeat
          Close     M060Sfile
          Close     M060file
          goto      EOJ
NoLoop7
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:17,"MDC_060 not found - continuing"
          endif

....      
EOJ       Shutdown
          stop
          INclude   MdcMainIO.inc
          Include   MDCSegIO.inc
          Include   MDCMSCIO.inc
          Include   MDC035IO.inc
          Include   MDC090IO.inc
          Include   MDCTXTIO.inc
          Include   MDC060IO.inc
          INclude   Comlogic.inc
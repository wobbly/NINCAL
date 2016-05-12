PC         EQU       0
          include   common.inc
          include   cons.inc
          include   nprjdd.inc
          include   compdd.inc
          include   cntdd.inc

release   init      "1.0"
Reldate   Init      "24 January 2011"

.Convert to A/R & A/P  read 4 import files (Brokerage AR & AP, List management AR & AP) and apply to ProjDolr file
.Data from projection Spreadsheets before import convert monthly projected dollars to montly Percentage (no decimal) and saved as comma delimited

Input     File
ErrorFile     FIle
Projclient     Dim  6                             Client or LIst number
.Name           Dim 45    
jan       dim       25
feb       dim       25
mar       dim       25
apr       dim       25
may       dim       25
jun       dim       25
jul       dim       25
aug       dim       25
sep       dim       25
oct       dim       25
nov       dim       25
dec       dim       25
ProjTot   Dim       11
form11    Form      11




          call      Paint
.
          erase     "c:\work\prjerror.dat"
.
          prepare   ErrorFile,"c:\work\prjerror.dat"
          OPEN      Input,"e:\DATA\BrkAR2011.CSV|10.10.30.103:502",read

Start
.First pass Brokerage AR
          Loop
          ReAD      Input,SEQ;*CDFON,PRoJCLIENT,STR45,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG:
                           SEP,OCT,NOV,DEC,ProjTot
          Until     Over
          Move      C0,Form11
          MOve      ProjTot,Form11
          call      debug
          if        (Form11 > 0)

                    move      "2011",PrjYr
                    move      "01",PrjKey
                    move      "R",PrjType
                    move      "B",PrjSrc
                    call      PrepCLient
                    packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                    move      "NPRJKey",Location
                    pack      KeyLocation,"Key: ",NPRJFLD
                    call      NPRJKey
                    If        Not Over
                              move      "NPRJUpd",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAr
                              call      NprjUpd
                    Else
                              move      "2011",PrjYr
                              move      "01",PrjKey
                              move      "R",PrjType
                              move      "B",PrjSrc
                              call      PrepCLient
                              packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                              move      "NPRJWrt",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAr
                              call      NPrjWrt
                    
                    endif
                    
                    call      VerfClient
          Endif
          repeat

.Second pass Brokerage AP
          Close     Input
          OPEN      Input,"e:\DATA\BrkAP2011.CSV|10.10.30.103:502",read

          Loop
          ReAD      Input,SEQ;*CDFON,PRoJCLIENT,STR45,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG:
                           SEP,OCT,NOV,DEC,ProjTot
          Until     Over
          Move      C0,Form11
          MOve      ProjTot,Form11
          if        (Form11 > 0)

                    move      "2011",PrjYr
                    move      "01",PrjKey
                    move      "R",PrjType
                    move      "B",PrjSrc
                    call      PrepCLient
         call       Debug
                    packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                    move      "NPRJKey",Location
                    pack      KeyLocation,"Key: ",NPRJFLD
                    call      NPRJKey
                    If        Not Over
                              move      "NPRJUpd",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAP
                              call      NprjUpd
                    Else
                              move      "2011",PrjYr
                              move      "01",PrjKey
                              move      "R",PrjType
                              move      "B",PrjSrc
                              call      PrepCLient
                              packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                              move      "NPRJWrt",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAP
                              call      NPrjWrt
                    
                    endif
                    
                    call      VerfClient
          Endif
          repeat
.Third pass Management AR
          Close     Input
          OPEN      Input,"e:\DATA\LMAR2011.CSV|10.10.30.103:502",read

          Loop
          ReAD      Input,SEQ;*CDFON,PRoJCLIENT,STR45,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG:
                           SEP,OCT,NOV,DEC,ProjTot
          Until     Over
          Move      C0,Form11
          MOve      ProjTot,Form11
          if        (Form11 > 0)
                    move      "2011",PrjYr
                    move      "01",PrjKey
                    move      " ",PrjType
                    move      "M",PrjSrc
                    call      PrepCLient
          
                    packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                    move      "NPRJKey",Location
                    pack      KeyLocation,"Key: ",NPRJFLD
                    call      NPRJKey
                    If        Not Over
                              move      "NPRJUpd",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAR
                              call      NprjUpd
                    Else
                              move      "2011",PrjYr
                              move      "01",PrjKey
                              move      " ",PrjType
                              move      "M",PrjSrc
                              call      PrepCLient
                              packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                              move      "NPRJWrt",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAr
                              call      NPrjWrt
                    
                    endif
          Endif
          repeat

.Forth pass Managment AP
          Close     Input
          OPEN      Input,"e:\DATA\LMAP2011.CSV|10.10.30.103:502",read

          Loop
          ReAD      Input,SEQ;*CDFON,PRoJCLIENT,STR45,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG:
                           SEP,OCT,NOV,DEC,ProjTot
          Until     Over
          Move      C0,Form11
          MOve      ProjTot,Form11
          if        (Form11 > 0)

                    move      "2011",PrjYr
                    move      "01",PrjKey
                    move      " ",PrjType
                    move      "M",PrjSrc
                    call      PrepCLient
          
                    packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                    move      "NPRJKey",Location
                    pack      KeyLocation,"Key: ",NPRJFLD
                    call      NPRJKey
                    If        Not Over
                              move      "NPRJUpd",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAP
                              call      NprjUpd
                    Else
                              move      "2011",PrjYr
                              move      "01",PrjKey
                              move      " ",PrjType
                              move      "M",PrjSrc
                              call      PrepCLient
                              packkey    NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYr,PrjKey
                              move      "NPRJWrt",Location
                              pack      KeyLocation,"Key: ",NPRJFLD
                              Call      LoadAP
                              call      NPrjWrt
                    
                    endif       
          Endif
          repeat

EOJ
          Weof      ErrorFile,seq
          shutdown
                    


LoadAR          
          MOve      "%",str1
          call      RemoveChar using Jan,str1
          call      RemoveChar using FEb,str1
          call      RemoveChar using Mar,str1
          call      RemoveChar using Apr,str1
          call      RemoveChar using May,str1
          call      RemoveChar using Jun,str1
          call      RemoveChar using Jul,str1
          call      RemoveChar using Aug,str1
          call      RemoveChar using Sep,str1
          call      RemoveChar using Oct,str1
          call      RemoveChar using Nov,str1
          call      RemoveChar using Dec,str1
          call      Trim using Jan
          move      Jan,PRJARJan
          call      Trim using Feb
          move      Feb,PRJARFEb
          call      Trim using Mar
          move      Mar,PRJARMar
          call      Trim using Apr
          move      Apr,PRJARApr
          call      Trim using May
          move      May,PRJARMay
          call      Trim using Jun
          move      Jun,PRJARJun
          call      Trim using Jul
          move      Jul,PRJARJul
          call      Trim using Aug
          move      Aug,PRJARAug
          call      Trim using Sep
          move      Sep,PRJARSep
          call      Trim using Oct
          move      Oct,PRJAROct
          call      Trim using Nov
          move      Nov,PRJARNov
          call      Trim using Dec
          move      Dec,PRJARDec
          call      Trim using ProjTot
          MOve      PRojTot,PRJAR
          Return
          
LoadAP          
          MOve      "%",str1
          call      RemoveChar using Jan,str1
          call      RemoveChar using FEb,str1
          call      RemoveChar using Mar,str1
          call      RemoveChar using Apr,str1
          call      RemoveChar using May,str1
          call      RemoveChar using Jun,str1
          call      RemoveChar using Jul,str1
          call      RemoveChar using Aug,str1
          call      RemoveChar using Sep,str1
          call      RemoveChar using Oct,str1
          call      RemoveChar using Nov,str1
          call      RemoveChar using Dec,str1
          call      Trim using Jan
          move      Jan,PRJAPJan
          call      Trim using Feb
          move      Feb,PRJAPFEb
          call      Trim using MAR
          move      MAR,PRJAPMAR
          call      Trim using Apr
          move      Apr,PRJAPApr
          call      Trim using May
          move      May,PRJAPMay
          call      Trim using Jun
          move      Jun,PRJAPJun
          call      Trim using Jul
          move      Jul,PRJAPJul
          call      Trim using Aug
          move      Aug,PRJAPAug
          call      Trim using Sep
          move      Sep,PRJAPSep
          call      Trim using Oct
          move      Oct,PRJAPOct
          call      Trim using Nov
          move      Nov,PRJAPNov
          call      Trim using Dec
          move      Dec,PRJAPDec
          call      Trim using ProjTot
          MOve      PRojTot,PRJAP
          Return

VerfClient
                    packkey   Compfld,prjclient
                    rep       zfill,COMPFLD
                    move      "CompOK-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if over
                                        pack      taskname,PrjClient," ",str45," does not exist in Company file!!       "
                                        call      WriteError
                              elseif (COMPMLRFLG <> "T")
                                        pack      taskname,PrjClient," ",str45," is not a valid Mailer in Company file!!"
                                        call      WriteError
                              else
                              endif
          Return
WriteError
          WRite     ErrorFile,seq;taskname
          return
PrepCLient
          Count     N2,Projclient
          if        (N2 =  6)
          move      Projclient,Prjclient       
          elseif    (N2 =  5)
          packkey   Prjclient from c0,projclient
          elseif    (N2 =  4)
          packkey   Prjclient from c0,c0,projclient
          elseif    (N2 =  3)
          packkey   Prjclient from c0,c0,c0,projclient
          elseif    (N2 =  2)
          packkey   Prjclient from c0,c0,c0,c0,projclient
          elseif    (N2 =  1)
          packkey   Prjclient from c0,c0,c0,c0,c0,projclient
          Else
          MOve      "000000",prjclient
          endif
          return
          include   nprjio.inc
          include   compio.inc
          include   cntio.inc
          include   comlogic.inc

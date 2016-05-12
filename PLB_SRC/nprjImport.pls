PC         EQU       0
          include   common.inc
          include   cons.inc
          include   nprjdd.inc
          include   Ndatdd.inc
          include   compdd.inc
          include   cntdd.inc

release   init      "1.31"     DH   New year
Reldate   Init      "2016 January 13"
.release   init      "1.3"     DH   New year
.Reldate   Init      "2015 January 5"
.release   init      "1.2"     DH   New year
.Reldate   Init      "2014 January 2"
.release   init      "1.1"     DH   New year
.Reldate   Init      "30 Jan 2013"
.release   init      "1.0"     DH
.Reldate   Init      "01 Feb 2013"
.Read muliple input files:
.Brokerage AR
.Brokerage AP
.Management AR
.Management Ap
. add current year info and write/update projection file
.input files created from master projections spreadsheet exported to CSV
.ID number needs leading zeros, month data converted from dollars to percentage of the year no decimals IE: 10 = ten percent
.needs total for the year

tempfile1 file

.Layout:
.PrjClient client/List number
.STR45      Name
.variables for monthly data will be used by all four input types
.export from spreadsheet as comma delimited, percentages for months, $ for total, all no decimal places percentage max = 100 in a month
INjan     dim   25           .percent for Jan
INfeb     dim   25
INmar     dim   25
INapr     dim   25
Inmay     dim   25
Injun     dim   25
Injul     dim   25
Inaug     dim   25
Insep     dim   25
Inoct    dim   25
Innov     dim   25
INdec     dim   25          .Percent for Dec

ProjInTot    dim 11         .total dollars projected for year
...........................................................................


percentage form     3.4
n111      FORM      1.11
n112  form  1.2
proj   dim 11
proj2  dim  12

PrjClient2  dim 6

INproj       form 11





          call      Paint
.         goto GetNextLoop
.
          erase     "c:\work\prjerror.dat"
.
          prepare   tempfile,"c:\work\prjerror.dat"
          OPEN      tempfile1,"C:\work\lmAR2016.CSV"
                    move      "NPRJSEQ",Location
                    pack      KeyLocation,"Key: SEQ"
LoopLMAR
       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopLMAP if over
                    Call      PRocess

                                        move      "2016",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      " ",PrjType
                                                  move      "M",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAr
                                        move   INJAN to PrjArjan
                                        move   INFeb to PrjArfeb
                                        move   INMar to PrjArmar
                                        move   INApr to PrjArapr
                                        move   INMay to PrjArmay
                                        move   INJun to PrjArjun
                                        move   INJul to PrjArjul
                                        move   INAug to PrjAraug
                                        move   INSep to PrjArsep
                                        move   INOct to PrjAroct
                                        move   INNov to PrjArnov
                                        move   INdec to PrjArdec

                                        move  yes to PrjMast
                                                  if        (PrjAr > 0)            .suppress if no projection
                                                  call      NPRJUPD
                                                  endif
                                        else
                                        
                                        move      "2016",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "M",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAr
                                        move   INJAN to PrjArjan
                                        move   INFeb to PrjArfeb
                                        move   INMar to PrjArmar
                                        move   INApr to PrjArapr
                                        move   INMay to PrjArmay
                                        move   INJun to PrjArjun
                                        move   INJul to PrjArjul
                                        move   INAug to PrjAraug
                                        move   INSep to PrjArsep
                                        move   INOct to PrjAroct
                                        move   INNov to PrjArnov
                                        move   INdec to PrjArdec

                                                  if        (PrjAr > 0)            .suppress if no projection
                                                  call      NPRJwrt
                                                  endif

                                        endif
                      goto    loopLMAR
.List Management AP

LoopLMAP
          OPEN      tempfile1,"C:\work\lmAP2016.CSV"
LoopLMAP1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopBAR if over
                    Call      PRocess

                                        move      "2016",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      " ",PrjType
                                                  move      "M",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAP
                                        move   INJAN to PrjAPjan
                                        move   INFeb to PrjAPfeb
                                        move   INMar to PrjAPmAr
                                        move   INApr to PrjAPapr
                                        move   INMay to PrjAPmay
                                        move   INJun to PrjAPjun
                                        move   INJul to PrjAPjul
                                        move   INAug to PrjAPaug
                                        move   INSep to PrjAPsep
                                        move   INOct to PrjAPoct
                                        move   INNov to PrjAPnov
                                        move   INdec to PrjAPdec

                                        move  yes to PrjMast
                                                  if        (PrjAp > 0)            .suppress if no projection
                                                  call      NPRJUPD
                                                  endif
                                        else
                                        move      "2016",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "M",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAP
                                        move   INJAN to PrjAPjan
                                        move   INFeb to PrjAPfeb
                                        move   INMar to PrjAPmAr
                                        move   INApr to PrjAPapr
                                        move   INMay to PrjAPmay
                                        move   INJun to PrjAPjun
                                        move   INJul to PrjAPjul
                                        move   INAug to PrjAPaug
                                        move   INSep to PrjAPsep
                                        move   INOct to PrjAPoct
                                        move   INNov to PrjAPnov
                                        move   INdec to PrjAPdec

                                                  if        (PrjAp > 0)            .suppress if no projection
                                                  call      NPRJwrt
                                                  endif

                                        endif
                      goto    loopLMAP1
.Brokerage AR

LoopBAR
          OPEN      tempfile1,"C:\work\BAR2016.CSV"
LoopBAR1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopBAP if over
                                                  move      "R",PrjType
                                                  move      "B",PrjSrc
                    Call      PRocess

                                        move      "2016",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      "R",PrjType
                                                  move      "B",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAR
                                        move   INJAN to PrjARjan
                                        move   INFeb to PrjARfeb
                                        move   INMar to PrjARmAr
                                        move   INApr to PrjARApr
                                        move   INMay to PrjARmay
                                        move   INJun to PrjARjun
                                        move   INJul to PrjARjul
                                        move   INAug to PrjARaug
                                        move   INSep to PrjARsep
                                        move   INOct to PrjARoct
                                        move   INNov to PrjARnov
                                        move   INdec to PrjARdec

                                        move  yes to PrjMast
                                                  if        (PrjAR > 0)            .suppress if no projection
                                                  call      NPRJUPD
                                                  endif
                                        else
                                        move      "2016",PrjYR
                                        move      "01",Prjkey
                                        move      "R",PrjType
                                        move      "B",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAR
                                        move   INJAN to PrjARjan
                                        move   INFeb to PrjARfeb
                                        move   INMar to PrjARmAr
                                        move   INApr to PrjARApR
                                        move   INMay to PrjARmay
                                        move   INJun to PrjARjun
                                        move   INJul to PrjARjul
                                        move   INAug to PrjARaug
                                        move   INSep to PrjARsep
                                        move   INOct to PrjARoct
                                        move   INNov to PrjARnov
                                        move   INdec to PrjARdec

                                                  if        (PrjAR > 0)            .suppress if no projection
                                                  call      NPRJwrt
                                                  endif

                                        endif
                      goto    loopBAR1
.Brokerage AP
LoopBAP
          OPEN      tempfile1,"C:\work\BAP2016.CSV"
LoopBAP1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    EOJ if over
                    Call      PRocess

                                        move      "2016",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      "R",PrjType
                                                  move      "B",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAP
                                        move   INJAN to PrjAPjan
                                        move   INFeb to PrjAPfeb
                                        move   INMar to PrjAPmAR
                                        move   INApr to PrjAPAPr
                                        move   INMay to PrjAPmay
                                        move   INJun to PrjAPjun
                                        move   INJul to PrjAPjul
                                        move   INAug to PrjAPaug
                                        move   INSep to PrjAPsep
                                        move   INOct to PrjAPoct
                                        move   INNov to PrjAPnov
                                        move   INdec to PrjAPdec

                                        move  yes to PrjMast
                                                  if        (PrjAp > 0)            .suppress if no projection
                                                  call      NPRJUPD
                                                  endif
                                        else
                                        move      "2016",PrjYR
                                        move      "01",Prjkey
                                        move      "R",PrjType
                                        move      "B",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAP
                                        move   INJAN to PrjAPjan
                                        move   INFeb to PrjAPfeb
                                        move   INMar to PrjAPmAR
                                        move   INApr to PrjAPAPr
                                        move   INMay to PrjAPmay
                                        move   INJun to PrjAPjun
                                        move   INJul to PrjAPjul
                                        move   INAug to PrjAPaug
                                        move   INSep to PrjAPsep
                                        move   INOct to PrjAPoct
                                        move   INNov to PrjAPnov
                                        move   INdec to PrjAPdec

                                                  if        (PrjAp > 0)            .suppress if no projection
                                                  call      NPRJwrt
                                                  endif

                                        endif
                      goto    loopBAP1
calcit
       move   proj to str25
                     return
.      move    c0 to n111
.                   move    c0 to n112
.                   move    lrsep to proj
                    scan   star in proj
.                   move    lrmay to proj
                    movefptr proj,n9
                    sub     c1,n9
                    setlptr  proj,n9
                    reset   proj
                    move    proj,proj2
                    reset    proj2
                    setlptr proj2,n9
                    move    proj2 to n111
                    move    n111 to n112
                    move    n112 to str25
          call    removeChar using str25,period
                    return


PRocess
          call      trim using PrjClient
          count     n2,PrjClient                    
          if        (n2=c6)
          move      PrjClient,compfld
          Elseif    (n2=c5)
          pack      compfld from c0,PrjClient
          Elseif    (n2=c4)
          pack      compfld from c0,c0,PrjClient
          Elseif    (n2=c3)
          pack      compfld from c0,c0,c0,PrjClient
          Elseif    (n2=c2)
          pack      compfld from c0,c0,c0,c0,PrjClient
          Elseif    (n2=c1)
          pack      compfld from c0,c0,c0,c0,c0,PrjClient
          else      move      "000000",PrjClient
          endif
          move      Compfld,PrjClient
          if        (prjsrc = "B")      .brokerage so its a "mailer" read
                    rep       zfill,COMPFLD
                    move      "CompOK-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
.                             pack      COMPFLD3,str4
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if over
                                        pack      str55,PrjClient," does not exist in Company file!!       "
                                       goto      WriteError
                              elseif (COMPMLRFLG <> "T")
                                        pack      str55,PrjClient," is not a valid Mailer in Company file!!"
                                       goto      WriteError
                              else
                                        move      COMPNUM,PrjClient
                              endif
          else
                    packkey   ndatfld from prjclient
                    move      "Process - NdatKEY",Location
                    pack      KeyLocation,"Key: ",ndatfld
                              pack      KeyLocation,"Key: ",Ndatfld
                              call      Ndatkey
                              if over
                                        pack      str55,PrjClient," does not exist in Datacard file!!       "
                                       goto      WriteError
                              endif
          endif
          REturn
WriteError
          write     tempfile,SEQ;str55
          return

EOJ
      shutdown

          include   nprjio.inc
          include   compio.inc
          include   cntio.inc
          include   ndatio.inc
          include   comlogic.inc

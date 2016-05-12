PC         EQU       0
          include   common.inc
          include   cons.inc
          include   \\nins1\e\library\include\nprjdd.inc
          include   compdd.inc
          include   cntdd.inc

release   init      "1.1"     DH   New year
Reldate   Init      "30 Jan 2013"
.release   init      "1.0"     DH
.Reldate   Init      "01 Feb 2012"
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
JD1    form "000"
.used by all
projINJant   dim 25
projINFebt   dim 25
projINMart   dim 25
projINAprt   dim 25
projINMayt   dim 25
projINJunt   dim 25
projINJult   dim 25
projINAugt   dim 25
projINSept   dim 25
projINOctt   dim 25
projINNovt   dim 25
projINDect   dim 25

PrjClient2  dim 6

INproj       form 11





          call      Paint
.         goto GetNextLoop
.
          erase     "c:\work\prjerror.dat"
.         erase     "c:\work\projdolr.dat"
.
          prepare   tempfile,"c:\work\prjerror.dat"
          OPEN      tempfile1,"C:\work\lmAR2013.CSV"
.         loop
                    move      "NPRJSEQ",Location
                    pack      KeyLocation,"Key: SEQ"
LoopLMAR
       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopLMAP if over
                    Call      PRocess

                                        move      "2013",PrjYR
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
                                        move   projINjant to PrjArjan
                                        move   projINfebt to PrjArfeb
                                        move   projINmart to PrjArmar
                                        move   projINaprt to PrjArapr
                                        move   projINmayt to PrjArmay
                                        move   projINjunt to PrjArjun
                                        move   projINjult to PrjArjul
                                        move   projINaugt to PrjAraug
                                        move   projINsept to PrjArsep
                                        move   projINoctt to PrjAroct
                                        move   projINnovt to PrjArnov
                                        move   projINdect to PrjArdec

                                        move  yes to PrjMast
                                        call      NPRJUPD
                                        else
                                        move      "2013",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "M",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAr
                                        move   projINjant to PrjArjan
                                        move   projINfebt to PrjArfeb
                                        move   projINmart to PrjArmar
                                        move   projINaprt to PrjArapr
                                        move   projINmayt to PrjArmay
                                        move   projINjunt to PrjArjun
                                        move   projINjult to PrjArjul
                                        move   projINaugt to PrjAraug
                                        move   projINsept to PrjArsep
                                        move   projINoctt to PrjAroct
                                        move   projINnovt to PrjArnov
                                        move   projINdect to PrjArdec

                                                  call  nprjwrt

                                        endif
                      goto    loopLMAR
.List Management AP

LoopLMAP
          OPEN      tempfile1,"C:\work\lmAP2013.CSV"
LoopLMAP1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopBAR if over
                    Call      PRocess

                                        move      "2013",PrjYR
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
                                        move   projINjant to PrjAPjan
                                        move   projINfebt to PrjAPfeb
                                        move   projINmArt to PrjAPmAr
                                        move   projINaprt to PrjAPapr
                                        move   projINmayt to PrjAPmay
                                        move   projINjunt to PrjAPjun
                                        move   projINjult to PrjAPjul
                                        move   projINaugt to PrjAPaug
                                        move   projINsept to PrjAPsep
                                        move   projINoctt to PrjAPoct
                                        move   projINnovt to PrjAPnov
                                        move   projINdect to PrjAPdec

                                        move  yes to PrjMast
                                        call      NPRJUPD
                                        else
                                        move      "2013",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "M",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAP
                                        move   projINjant to PrjAPjan
                                        move   projINfebt to PrjAPfeb
                                        move   projINmArt to PrjAPmAr
                                        move   projINaprt to PrjAPapr
                                        move   projINmayt to PrjAPmay
                                        move   projINjunt to PrjAPjun
                                        move   projINjult to PrjAPjul
                                        move   projINaugt to PrjAPaug
                                        move   projINsept to PrjAPsep
                                        move   projINoctt to PrjAPoct
                                        move   projINnovt to PrjAPnov
                                        move   projINdect to PrjAPdec

                                                  call  nprjwrt

                                        endif
                      goto    loopLMAP1
.Brokerage AR

LoopBAR
          OPEN      tempfile1,"C:\work\BAR2013.CSV"
LoopBAR1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    LoopBAP if over
                    Call      PRocess

                                        move      "2013",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      " ",PrjType
                                                  move      "B",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAR
                                        move   projINjant to PrjARjan
                                        move   projINfebt to PrjARfeb
                                        move   projINmArt to PrjARmAr
                                        move   projINARrt to PrjARARr
                                        move   projINmayt to PrjARmay
                                        move   projINjunt to PrjARjun
                                        move   projINjult to PrjARjul
                                        move   projINaugt to PrjARaug
                                        move   projINsept to PrjARsep
                                        move   projINoctt to PrjARoct
                                        move   projINnovt to PrjARnov
                                        move   projINdect to PrjARdec

                                        move  yes to PrjMast
                                        call      NPRJUPD
                                        else
                                        move      "2013",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "B",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAR
                                        move   projINjant to PrjARjan
                                        move   projINfebt to PrjARfeb
                                        move   projINmArt to PrjARmAr
                                        move   projINARrt to PrjARARr
                                        move   projINmayt to PrjARmay
                                        move   projINjunt to PrjARjun
                                        move   projINjult to PrjARjul
                                        move   projINaugt to PrjARaug
                                        move   projINsept to PrjARsep
                                        move   projINoctt to PrjARoct
                                        move   projINnovt to PrjARnov
                                        move   projINdect to PrjARdec

                                                  call  nprjwrt

                                        endif
                      goto    loopBAR1
.Brokerage AP
LoopBAP
          OPEN      tempfile1,"C:\work\BAP2013.CSV"
LoopBAP1

       ReAD   TEMPFILE1,SEQ;*CDFON,PrjClient,STR45,INJAN,INFEB,INMAR,INAPR,INMAY,INJUN,INJUL,INAUG:
                           INSEP,INOCT,INNOV,INDEC,ProjInTot
                    goto    EOJ if over
                    Call      PRocess

                                        move      "2013",PrjYR
                                        move      "01",Prjkey
.Brokerage LR Rental  "R" "B"
.LM " " "M"
                                                  move      " ",PrjType
                                                  move      "B",PrjSrc
                                        pack      NPRJFLD,PrjType,PrjSrc,PrjClient,PrjYR,Prjkey
                                        move      "NPRJKEY",Location
                                        pack      KeyLocation,"Key: ",NPRJFLD
                                        move  PrjClient to PrjClient2

                                        call      NPRJkey
                                        if not over
                                        move  ProjInTot to PrjAP
                                        move   projINjant to PrjAPjan
                                        move   projINfebt to PrjAPfeb
                                        move   projINmARt to PrjAPmAR
                                        move   projINAPrt to PrjAPAPr
                                        move   projINmayt to PrjAPmay
                                        move   projINjunt to PrjAPjun
                                        move   projINjult to PrjAPjul
                                        move   projINaugt to PrjAPaug
                                        move   projINsept to PrjAPsep
                                        move   projINoctt to PrjAPoct
                                        move   projINnovt to PrjAPnov
                                        move   projINdect to PrjAPdec

                                        move  yes to PrjMast
                                        call      NPRJUPD
                                        else
                                        move      "2013",PrjYR
                                        move      "01",Prjkey
                                        move      " ",PrjType
                                        move      "B",PrjSrc
                                        move  yes to PrjMast
                                        move  PrjClient2 to PrjClient
                                        move  ProjInTot to PrjAP
                                        move   projINjant to PrjAPjan
                                        move   projINfebt to PrjAPfeb
                                        move   projINmARt to PrjAPmAR
                                        move   projINAPrt to PrjAPAPr
                                        move   projINmayt to PrjAPmay
                                        move   projINjunt to PrjAPjun
                                        move   projINjult to PrjAPjul
                                        move   projINaugt to PrjAPaug
                                        move   projINsept to PrjAPsep
                                        move   projINoctt to PrjAPoct
                                        move   projINnovt to PrjAPnov
                                        move   projINdect to PrjARdec

                                                  call  nprjwrt

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
                    rep       zfill,COMPFLD
                    move      "CompOK-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
.                             pack      COMPFLD3,str4
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              if over
                                        pack      str55,PrjClient," does not exist in Company file!!       "
.                                       goto      WriteError
                              elseif (COMPMLRFLG <> "T")
                                        pack      str55,PrjClient," is not a valid Mailer in Company file!!"
.                                       goto      WriteError
                              else
                                        move      COMPNUM,PrjClient
                              endif
          REturn

EOJ
      shutdown

          include   nprjio.inc
          include   compio.inc
          include   cntio.inc
          include   comlogic.inc

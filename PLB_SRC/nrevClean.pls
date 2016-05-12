PC         EQU       0
           include common.inc
           include cons.inc
                                        include   compdd.inc
                                        include   cntdd.inc
           include ndatdd.inc
           include nrevdd.inc

*Sorted Revenue file
INFILE     FILE
newfile    file
.Print file
PRFILE     PFILE

ClintSrt   INIT  "3-8"                        .Sort by client #
SORTFLE    DIM    70                          .Var to pack file names of sort
PRTITLE    DIM    18                          .Title of Printjob
PRTNAME1   DIM    11                          .Name of printfile
PRTDIR     INIT    "C:\WORK\"                 .Printfile directory."
RELEASE   INIT      "1.1"          DH clean June 2014
Reldate   Init      "2014 July 8"
.RELEASE   INIT      "1.0"          DH 22 Jan 2011 
.Reldate   Init      "22 Jan 2011"
.===========================================================================
.======================================================================
..............................................................
        move    "NREVCLean.PLS",Wprognme
        move    "Actuals vs. Projected",Wfunction
        move    "David Herrick",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate
.=======================================================================
.==========================================================================
.==============================================================================
.==========================================================================
.===============================================================================

OpenIt
.          Prepare    Newfile,"e:\data\text\revenue.new|NINS1:502",exclusive
*.==========================================================================
         OPEN    INFILE,"e:\data\text\revenue.dat|NINS1:502",read
          Loop
          Read      Infile,seq;Nrevvars
          until over

          if        (yr0 = "2015")
          packkey   Nrevfld from TYPE,SRC,CID,YR0
          call      nREVKEY
.begin patch 1.1
.          SUB       aPRAR,RevAR                                .Totals for year
.          SUB       aPRAP,RevAP
          SUB       MayAR,RevAR                                .Totals for year
          SUB       MayAP,RevAP
.          SUB       AugAR,RevAR                                .Totals for year
.          SUB       AugAP,RevAP
.          mOVE      C0,aPRar
.          mOVE      C0,aPRap
.          mOVE      C0,aPRlr
.          mOVE      C0,aPRNIN
          mOVE      C0,Mayar
          mOVE      C0,Mayap
.          mOVE      C0,Jullr
.          mOVE      C0,JulNIN
.          mOVE      C0,Augar
.          mOVE      C0,Augap
.          mOVE      C0,Auglr
.          mOVE      C0,AugNIN
          
          call      Nrevupd
          endif
          
          repeat
          stop


        include nrevio.inc
        include ndatio.inc
                                        include   compio.inc
                                        include   cntio.inc
        include comlogic.inc

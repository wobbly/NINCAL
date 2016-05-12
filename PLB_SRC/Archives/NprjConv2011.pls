PC         EQU       0
           include common.inc
           include cons.inc
                                        include   compdd.inc
                                        include   cntdd.inc
           include ndatdd.inc
           include nprjdd.inc

*Sorted Revenue file
INFILE     FILE
newfile    file
.Print file
PRFILE     PFILE

INDAT      INIT  "projdolr.2010"   .File to be sorted
OUTSRT     INIT  "Projdolr.New"   . Output file
ClintSrt   INIT  "3-8"                        .Sort by client #
SORTFLE    DIM    70                          .Var to pack file names of sort
PRTITLE    DIM    18                          .Title of Printjob
PRTNAME1   DIM    11                          .Name of printfile
PRTDIR     INIT    "C:\WORK\"                 .Printfile directory
RELEASE   INIT      "1.0"          DH 22 Jan 2011 
Reldate   Init      "22 Jan 2011"
.===========================================================================
.======================================================================
..............................................................
        move    "NREVConv.PLS",Wprognme
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
          Prepare    Newfile,"e:\data\text\Projdolr.new|10.10.30.103:502",exclusive
*.==========================================================================
         OPEN    INFILE,"e:\data\text\projdolr.2010|10.10.30.103:502",read
          Loop
          Read      Infile,seq;PrjType,PrjSrc,PrjClient,PrjYr,PrjKey,PrjDate,PrjMod,PrjLR,PrjNin,PrjNotes:
                    PrjMast,prjLRJan,prjLRFeb,prjLRMar,prjLRApr,prjLRMay,prjLRJun,prjLRJul,prjLRAug,prjLRSep:
                    prjLROct,prjLRNov,prjLRDec,prjNINJan,prjNINFeb,prjNINMar,prjNINApr,prjNINMay,prjNINJun:
                    prjNINJul,prjNINAug,prjNINSep,prjNINOct,prjNINNov,prjNINDec,PRJAR,PRJAP    

          until over
          Write     Newfile,seq;Prjvars
          repeat
          weof      Newfile,seq
          stop


        include nPrjio.inc
                                        include   compio.inc
                                        include   cntio.inc
        include comlogic.inc

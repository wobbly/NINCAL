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

INDAT      INIT  "revenue.2010"   .File to be sorted
OUTSRT     INIT  "revenue.New"   . Output file
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
          Prepare    Newfile,"e:\data\text\revenue.new|NINS1:502",exclusive
*.==========================================================================
         OPEN    INFILE,"e:\data\text\revenue.2010|NINS1:502",read
          Loop
          Read      Infile,seq;TYPE,SRC,CID,YR0,CLIENT,JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
                SEPLR,OCTLR,NOVLR,DECLR,JANNIN,FEBNIN,MARNIN,APRNIN,MAYNIN,JUNNIN,JULNIN,AUGNIN:
                SEPNIN,OCTNIN,NOVNIN,DECNIN,REVunbld
          until over
          Write     Newfile,seq;Nrevvars
          repeat
          weof      Newfile,seq
          stop


        include nrevio.inc
        include ndatio.inc
                                        include   compio.inc
                                        include   cntio.inc
        include comlogic.inc

.Neom0033.pls
.produce Broker (outside) Guaranty  reports.
pc      Equ     0
              include   common.inc
              include   cons.inc
              include   norddd.inc
              include   npgedd.inc
.begin patch 1.4
.              include   ninvdd.inc
               Include        ninvdd.inc
               Include        NinvAcddd.inc
.end patch 1.4
.Patch1.2
                              include   compdd.inc
                              include   cntdd.inc
.              include   nbrkdd.inc
.              include   nmlrdd.inc
.Patch1.2
              include   nmoadd.inc
              include   nmobdd.inc
              include   nowndd.inc
              include   nadjdd.inc
              include   nmrgdd.inc
              include   ndatdd.inc
              include   ndat3dd.inc
              include   nshpdd.inc
              INCLUDE   OSLSPERN.inc
              INCLUDE   HP.INC
              include   nacddd.inc
              INCLUDE   CONSACCT.inc
.>Patch 1.4
        include winapi.inc
.>Patch 1.4        
guareom       file
FileCheck FIle
trapcount form      4
Release   Init      "1.52"     DLH use dsinit
Reldate   Init      "2016 May 2"
.Release   Init      "1.51"     DLH Turn back on add email results
..                             skip lcr's, pending, cancelled with charges, and List management orders that show a NIN guar
.Reldate   Init      "2013 October 31"
.release   init    "1.50"                DLH      Sunbelt PDF
.Reldate   Init      "2013 April 24"
.release   init    "1.42"                08Mar2007  DLH      Oslspern.inc expansion
.release  init      "1.41"         DMB  18AUG2005       Stopped print of Brkbyown
.release  init      "1.4"         DMB   03AUG2005       Conversion to printpage
.release   nit      "1.4"        DLH    08March2005         Invoice Conversion
.release  init      "1.3"        ASH    06AUG2004 Logo Conversion
.release  init      "1.2"        DMB    26MAY2004 Mailer Conversion 
.release       init       "1.1" JD 28JAN04
.release       init      "1.0"
countin       form      5
count         form      5
.str20    dim       20
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
SLS      DIM       8             USED TO PRINT SALESPERSON ON 60-90.
moaamt   dim        13
moa$     form       8.2
CHANGE   FORM      10.2         CHANGE TO BE APPLIED TO BALANCE.
DOLLAR   INIT      "$$,$$$,$$$.99"
DATEMASK INIT      "99/99/99"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
DIFFDAYS FORM      5
fARBRK    FORM      10.2
fApBRK    FORM      10.2
form52    form      5.2
C35      FORM      "35"
C36      FORM      "36"
C37      FORM      "37"
C38      FORM      "38"
c58      form      "58"
c57      form      "57"
NUMMASK  INIT      "Z,ZZZ,ZZZ,ZZZ.99-"
COL1     DIM       17
COL2     DIM       17
COL3     DIM       17
COL4     DIM       17
COL5     DIM       17
COL6     DIM       17
edittot  DIM       17
LNAME17  DIM       17
holdown  dim       4
holdlist dim       6
holdbrk  dim       4
holdmlr  dim       4
PAGE     FORM      5
PRTLINES FORM      2
DATE     DIM       8
GRAR     FORM      10.2
GRAP     FORM      10.2
GRAP1    FORM      10.2           NON-LIST OWNER A/P
GRAP2    FORM      10.2           advance pay to lo.
mrgsw    dim       1
shipsw   dim       1
guartype dim       4
DY00TO30 FORM      10.2            DETAIL
DY31TO60 FORM      10.2
DY61TO90 FORM      10.2
DYOVER90 FORM      10.2
BALDUE   FORM      10.2
CL00TO30 FORM      10.2            CLIENT TOTALS
CL31TO60 FORM      10.2
CL61TO90 FORM      10.2
CLOVER90 FORM      10.2
BR00TO30 FORM      10.2            BROKER/CONSULTANT TOTALS
BR31TO60 FORM      10.2
BR61TO90 FORM      10.2
BROVER90 FORM      10.2
GR00TO30 FORM      10.2            GRAND TOTALS
GR31TO60 FORM      10.2
GR61TO90 FORM      10.2
GROVER90 FORM      10.2
GR30to90 FORM      10.2
.
DYapTO30 FORM      10.2            DETAIL
DYapTO60 FORM      10.2
DYapTO90 FORM      10.2
DYapOR90 FORM      10.2
CLAPTO30 FORM      10.2            CLIENT TOTALS
CLAPTO60 FORM      10.2
CLAPTO90 FORM      10.2
CLApOR90 FORM      10.2
BRAPTO30 FORM      10.2            BROKER/CONSULTANT TOTALS
BRAPTO60 FORM      10.2
BRAPTO90 FORM      10.2
BRAPOR90 FORM      10.2
GRAPTO30 FORM      10.2            GRAND TOTALS
GRAPTO60 FORM      10.2
GRAPTO90 FORM      10.2
GRAPOR90 FORM      10.2
GRAP3t90 FORM      10.2
SYSDAYS  FORM      5
MON1     INIT      " January 20?? "
MON2     INIT      "February 20?? "
MON3     INIT      "  March 20??  "
MON4     INIT      "  April 20??  "
MON5     INIT      "   May 20??   "
MON6     INIT      "  June 20??   "
MON7     INIT      "  July 20??   "
MON8     INIT      " August 20??  "
MON9     INIT      "September 20??"
MON10    INIT      " October 20?? "
MON11    INIT      "Novenber 20?? "
MON12    INIT      "December 20?? "
MONTH    DIM       14
PRC30    FORM      10.2
PRC60    FORM      10.2
PRC90    FORM      10.2
PRCOV    FORM      10.2
.>Patch 3.3
PRFILE    pfile
.Column Defs
Header1   form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
Column11R form    9
Column12R form    9

.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
          move    "4250" to Header1
        move    "200",column
        move    "2500",column1
        move    "3100",column2
        move    "3700",column3
        move    "4300",column4R
        move    "4800",column5R
        move    "5300",column6R
        move    "5800",column7R
        move    "6300",column8R
        move    "6800",column9R
        move    "7400",column10R
        move    "7600",column11R        
        move    "8400",column12R        

.**********.305233 FIRST NIN LR OF 1998
.main
              move      c1 to nordpath
              move      c1 to ninvpath
              move      c3 to nordlock
                move      c2 to nmobpath

         MATCH     "NEOM0033" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         MOVE      TODAY TO DATE
         Else  
         move      "NEOM0033" to program
         CLOCK     DATE TO DATE
         ENDIF

         MOVE      DATE TO TODAY
         UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         call      paint

.
DATEGET  DISPLAY   *P15:06,MM,SLASH,DD,SLASH,YY
         KEYIN     *P1:24,*EL,"DATE OK ? ",*T05,STR1;
         CMATCH    NO TO STR1
         GOTO      DATEBAD IF EQUAL
         CALL      DATETEST
         BRANCH    DATEFLAG TO datebad
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSDAYS
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM MON1,MON2,MON3,MON4,MON5,MON6:
                   MON7,MON8,MON9,MON10,MON11,MON12
         SCAN      "??" IN MONTH
         GOTO      PREPIT IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
         GOTO      PREPIT
DATEBAD  KEYIN     *P01:24,*EL,"The ageing date is invalid.":
                   *P15:06,*DV,*HON,MM,*DV,SLASH,*DV,DD,*DV,SLASH,*DV,YY:
                   *P15:06,*RV,*+,*JR,MM:
                   *P18:06,*RV,*+,*JR,DD:
                   *P21:06,*RV,*-,*JR,YY,*HOFF;
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         GOTO      DATEGET
PREPIT
.TEMP
.              GOTO      PHASE3
                prepare   guareom,"\\nins1\e\data\brkguareom.dat",exclusive
.
.              move      "305233" to nordfld
.              move      "600000" to nordfld
              move      "775000" to nordfld
              call      nordkey
..............................................................................
.BEGIN = PHASE 1 GET THE RECORDS
begin         call      nordks
              goto      phase2 if over
              add       c1 to countin
              display   *p10:08,"Records reviewed : ",countin,b1,omdtem,"/",omdted,"/",omdtey

          scan      "B",olrn
          goto      begin if equal
          reset     olrn
          scan      "M",olrn
          goto      begin if equal
          if        (Ostat = "X" or Ostat = "l" or Ostat = "z" or Ostat = "t" or Ostat = "x")
          goto      begin
          endif

              cmatch    b1 to OBRKGUAR
              goto      begin if eos
              goto      begin if equal
                move      omdtem to mm
              move      omdted to dd
                move      omdtey to yy
              call      cvtjul
              MOVE      SYSDAYS TO DIFFDAYS
                   if         (OBRKGUAR = "1")
                   add        "30" to juldays
                   endif
                   if         (OBRKGUAR = "2")
                   add        "45" to juldays
                     endif
                   if         (OBRKGUAR = "3")
           add        "60" to juldays
                   endif
           if         (OBRKGUAR = "4")
                   add        "90" to juldays           .actually open ended.
           endif
              move      olrn to ninvfld
              call      ninvkey
              if        not over
              cmatch    "P" to statb
              goto      begin if equal
              else
                move      omdtem to mm
              move      omdted to dd
                move      omdtey to yy
                        goto       begin              .and not billed "PRESUME" that is ok
              endif
write         write     guareom,seq;ordvars
              add       c1 to count
              display   *p10:10,"Records output : ",count
                goto      begin
.
.PHASE 2 SORT THE RECORDS
phase2        weof      guareom,seq
              close     guareom
              clear     taskname
              move      "\\nins1\e\data\brkguareom.dat,\\nins1\e\data\brkguareom.srt;303-306,3-6",taskname
                SORT          taskname
              IF OVER
                DISPLAY       *N,"Sort ERROR ",S$ERROR$;
                keyin   str1
                ENDIF
              clear     taskname
              move      "\\nins1\e\data\brkguareom.dat,\\nins1\e\data\brkguareom2.srt;22-25,16-21",taskname
                SORT          taskname
              IF OVER
                DISPLAY       *N,"Sort ERROR ",S$ERROR$;
                keyin   str1
                ENDIF
phase3
.print by broker/consultant/mlr
              move      c0 to count
              open      guareom,"\\nins1\e\data\brkguareom.srt",exclusive
.>patch 1.5 Comment Out              
.              SPLOPEN   "\\nins1\E\DATA\BRKGUARBYMLR.LST"
.>patch 1.5 Comment Out              
.>Patch 1.4 Logic Addition for PDF Quality Control
.begin patch 1.50
.                    Call      GetPDFPATH
.          
.                    pack      str55 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str55:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.          result
.          if (result = C0)
..Prepare Flag file
.                    pack      str55 from PDFPATH,"\flag.dat"
.                    prep      tempfile,str55
.                    write     tempfile,SEQ;"flag set"
.                    close     tempfile
.          endif
.>Patch 1.4 Logic Addition for PDF Quality Control                              
           PRTOPEN prfile,"PDF:","c:\work\pdf\BRKGUARBYMLR.pdf"
.           PRTOPEN prfile,"PDF995","BRKGUARBYMLR.LST"
.end patch 1.50
           PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                               *Duplex=2;                                                 
.>patch 1.4 End Addition              
phase3in
                MOVE      C0 TO DY00TO30
              MOVE      C0 TO DY31TO60
                MOVE      C0 TO DY61TO90
                MOVE      C0 TO DYapTO30
                MOVE      C0 TO DYapTO60
                MOVE      C0 TO DYapTO90
              MOVE      C0 TO DYOVER90
                MOVE      C0 TO DYapOR90
                MOVE      B12 TO COL1
                MOVE      B12 TO COL2
                MOVE      B12 TO COL3
                MOVE      B12 TO COL4
              read      guareom,seq;ordvars
              goto      phase3eoj if over
              add       c1 to count
              display   *p10:10,"Records output : ",count
              compare   c1 to count
              if        equal
              move      omlrnum to holdmlr
              move      obrknum to holdbrk
              call      hd1                    .break header
              call      zeromlr1
.Patch 1.4 Comment Out          
.                  PRINT     *N,*01,MCOMP,B1,omlrnum,B2,"(",SLS,")":
.                            *N,*02,PERIOD
.Patch 1.4 Comment Out                           
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,MCOMP,B1,omlrnum,B2,"(",SLS,")";                 
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced
                   ADD       C3 TO PRTLINES
              endif
              match     obrknum to holdbrk
              call      bbreak if not equal
              match     omlrnum to holdmlr
              call      mbreak if not equal
              move      olrn to ninvfld
              rep       zfill in ninvfld
              call      ninvkey
              if        not over
                move      no to mrgsw
              move      no to shipsw
              move      olrn to nmrgfld
              CALL      NMRGKEY
                       if        not over
                         move      yes to mrgsw
                         endif
              MOVE      olrn to nshpfld
              REP       ZFILL IN NshpFLD
              CALL      NshpKEY
                       if        not over
                       move      yes to shipsw
                       endif
              call      wipecvars
              move      c1 to ndatpath
              move      olnum to ndatfld
              call      ndatkey
              call      compute
              move      ap to ap1
              move      olrn to nadjfld
              call      nadjkey
                        if        not over
                        add       asrecadj to ar
                        add       aslrinc to lrinc
                      add       aspayad1 to ap1
                        add       aspayad2 to ap2
                      endif
              else
              move    c0 to ar
              move    c0 to lrinc
              move    c0 to ap1
              move    c0 to ap2
              SUB       FORM92 FROM FORM92
              SUB       FORM52 FROM FORM52
              MOVE      OQTY TO FORM92
              DIV       THOUS INTO FORM92
              MOVE      OPPM TO FORM52         .use price from order
              DIV       HUND INTO FORM52
              mult      form52 by form92
              move      form92 to ar              .estimated
              endif
              call      age
                call      det1
              goto      phase3in

phase3eoj
.END OF PHASE 3 PRINT TOTALS ETC CLOSE FILES AND START FINAL PHASE
.EXCEPT FOR DEVELOPEMENT JUST STOP
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion
.>Patch 1.4 Comment Out     
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out              
         CALL      HD1 IF NOT LESS
         CALL      BALREAD
.Patch 1.4 Comment Out         
.         PRINT       *02,PERIOD:
.                   *N,*01,MCOMP:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT:
.                   *N
.Patch 1.4 Comment Out                   
.Patch 1.4 Logic Replaced
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,period;            
          add     eightlpi,row          
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,MCOMP;               
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                             
.         prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90,B5,"<MOA BALANCE> ",MOAAMT;            
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"<MOA BALANCE> ",MOAAMT;                         
          add     eightlpi,row                    
.Patch 1.4 Logic Replaced
         ADD       C3 TO PRTLINES
.
         pack      nbrkfld from holdbrk,z3
         call      nbrkkey
         MOVE      brcomp TO MCOMP
.>Patch 3.38 Comment Out                       
.         COMPARE   c57 TO PRTLINES
.>Patch 3.38 Comment Out                       
.>Patch 3.38      Code Added  
          COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion
         CALL      HD1 IF NOT LESS

         call      balread
.Patch 1.4 Comment Out
.         PRINT     *N;
.         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk:
.                      *22,BR00TO30:
.                      *32,BR31TO60:
.                      *42,BR61TO90:
.                      *52,BROVER90
.Patch 1.4 Comment Out                      
.Patch 1.4 Logic Replaced
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"<BROKER TOTAL> ##";           
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,BR00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,BR31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,BR61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,BROVER90;
          add     eightlpi,row 
.Patch 1.4 Logic Replaced
         ADD       CL00TO30 TO GR00TO30
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
.>Patch 3.38 Comment Out                       
.         COMPARE   C57 TO PRTLINES
.>Patch 3.38 Comment Out                       
.>Patch 3.38      Code Added  
          COMPARE  "9900" to ROW
.>Patch 3.38      End Code Addtion         
         CALL      Hd1 IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GR00TO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GR31TO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GR61TO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GROVER90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAR     TO COL5
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP     TO COL6
.
         MOVE      GR00TO30 TO PRC30
         DIVIDE    GRAR   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GR31TO60 TO PRC60
         DIVIDE    GRAR   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GR61TO90 TO PRC90
         DIVIDE    GRAR   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GROVER90 TO PRCOV
         DIVIDE    GRAR   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
.
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP2    TO COL6
.>Patch 1.4 Comment Out
.         PRINT     *N,"PROCESSED : ",count:
.                   *N:
.                   *N,"TOTALS:":
..                      *29,"0 - 30":
.                      *43,"31 - 60":
.                      *58,"61 - 90":
.                      *73,"OVER 90":
.                      *88,"TOTL AR":
.                      *103,"TOTL AP":
.                   *N,*26,"---------":
.                      *41,"---------":
..                      *56,"---------":
.                      *71,"---------":
.                      *86,"---------":
.                      *101,"---------":
.                   *N,*24,COL1:
.                      *39,COL2:
.                      *54,COL3:
.                      *69,COL4:
..                      *84,COL5:
.                      *99,COL6
.>Patch 1.4 Comment Out                      
.>Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Processed: ",count;         
          add eightlpi to row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Totals: ",count;              
          add eightlpi to row 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ULON,*ll,"0 - 30";                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,"31 - 60";                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"61 - 90";                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"OVER 90";                  
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,"TOTAL AR";                                     
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ULOFF,*ll,"TOTAL AP";
          add eightlpi to row           
          add eightlpi to row 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,COL1;                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,COL2;              
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,COL3;                       
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,COL4;             
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,COL5;                                 
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ll,COL6;  
          add eightlpi to row 
.>Patch 1.4 Replaced Logic
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP1    TO COL6
.>Patch 1.4 Comment Out                               
.         PRINT     *22,PRC30,PRC:
.                      *40,PRC60,PRC:
.                      *52,PRC90,PRC:
.                      *70,PRCOV,PRC:
.                      *99,COL6
.>Patch 1.4 Comment Out   
.>Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,PRC30,PRC;                             
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PRC60,PRC;                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PRC90,PRC;                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,PRCOV,PRC;                  
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ll,COL6;                                
          add eightlpi to row 
.>Patch 1.4 Replaced Logic
         ADD       C7 TO PRTLINES
.>Patch 1.4 Comment Out                    
.         COMPARE   C57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion

         CALL      Hd1 IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GRapTO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GRapTO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GRapTO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GRapOR90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAp2    TO COL5
.
         MOVE      GRapTO30 TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GRapTO60 TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GRapTO90 TO PRC90
         DIVIDE    GRAp2   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GRapOR90 TO PRCOV
         DIVIDE    GRAp2   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
         goto      skipper
.>Patch 1.4 Comment Out   this code never gets hit....         
.         print          *N:
.                   *N,"A/P TOTALS:":
.                      *29,"0 - 30":
.                      *43,"31 - 60":
.                      *58,"61 - 90":
.                      *73,"OVER 90":
.                      *88,"TOTL AP":
..                   *N,*26,"---------":
.                      *41,"---------":
.                      *56,"---------":
.                      *71,"---------":
.                      *86,"---------":
..                   *N,*24,COL1:
.                      *39,COL2:
.                      *54,COL3:
.                      *69,COL4:
.                      *84,COL5
.         PRINT     *22,PRC30,PRC:
.                      *40,PRC60,PRC:
.                      *52,PRC90,PRC:
.                      *70,PRCOV,PRC:
.                      *99,COL6
.>Patch 1.4 Comment Out   
                     
skipper
.>Patch 1.4 Comment Out
.              SPLCLOSE
.>Patch 1.4 Comment Out
.>Patch 1.4 Logic Replaced
          prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
.begin patch 1.50
.          call      GetPDfPAth
.          pack      str55 from PDFPATH,"\flag.dat"
.          pack      APIFileName,STR55,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
          pause     "2"
.          erase     STR55
.end patch 1.50
.>Patch 1.4 Logic Replaced
              RELEASE
        pack MailAttach,"c:\work\pdf\BRKGUARBYMLR.pdf"
        Move "creques@nincal.com,gspranz@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
        
        Move "NIN Broker Guaranty report.",MAILSubjct
        Move "",MAILBody
.........................
          Move      Yes,Mailtrace

        Call  SendMail                 

              MOVE      C0 TO PAGE
.>Patch 1.41 Program is no longer being viewed per eom3rd
.                stop
.>Patch 1.41 
                goto      phase4
mbreak
.print       total for mailer. new header & current detail
         call      balread
.>Patch 1.4 Comment Out          
.         PRINT        *02,HPBon,PERIOD:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,b1,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT:
.                   *N,hpboff
.>Patch 1.4 Comment Out 
.>Patch 1.4 Replaced Logic
          add eightlpi to row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*Boldon,*ll,PERIOD;                          
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                    
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                                       
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT,*Boldoff;                                    
          add eightlpi to row 
.>Patch 1.4 Replaced Logic
         ADD       C3 TO PRTLINES
              move      omlrnum to holdmlr
.              move      obrknum to holdbrk
                call      zeromlr1
              return
bbreak
.print       total for broker & mailer. new header & current detail
.>Patch 1.4 Comment Out                    
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion         
         CALL      HD1 IF NOT LESS
         call      balread
.Patch 1.4 Comment Out        
.         PRINT        *02,HPBon,PERIOD:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,b1,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT:
.                   *N,hpboff
.Patch 1.4 Comment Out                   
.>Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*Boldon,*ll,PERIOD;                          
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                    
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                                       
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"TOTAL ",edittot,b5,"<MOA BALANCE> ",MOAAMT,*Boldoff;                                    
          add eightlpi to row 
.>Patch 1.4 Replaced Logic                   
         ADD       C3 TO PRTLINES
.
           ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90

.         call      zeromlr1
.
         pack      nbrkfld from holdbrk,z3
         call      nbrkkey
         MOVE      brcomp TO MCOMP
          move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
.>Patch 1.4 Comment Out                             
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                  
         CALL      HD1 IF NOT LESS
         call      balread
.Patch 1.4 Comment Out
.         PRINT     *N;
.         PRINT     *N,*01,"<BROKER TOTAL> ##",holdbrk:
.                      *22,BR00TO30:
.                      *32,BR31TO60:
.                      *42,BR61TO90:
.                      *52,BROVER90,b1,"GRAND TOTAL ",COL5
.Patch 1.4 Comment Out
.         PRINT     *N,*01,*25,BR00TO30,*35,BR31TO60,*42,BR61TO90,*55,BROVER90
.>Patch 1.4 Replaced Logic  
          add eightlpi to row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"<BROKER TOTAL> ##",holdbrk;                       
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,BR00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,BR31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,BR61TO90;                    
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,BROVER90;                                       
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"GRAND TOTAL ",COL5;         
          add eightlpi to row 
.>Patch 1.4 Replaced Logic  
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
         ADD       C3 TO PRTLINES
              move      omlrnum to holdmlr
              move      obrknum to holdbrk
         pack      mkey from holdmlr,z3
         call      nmlrkey
         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
         if        not equal
         pack      nbrkfld from holdbrk,z3
         call      nbrkkey
                   if        not over
                   MOVE      mcomp TO MNAME
                   MOVE      brcomp TO MCOMP
           MOVE      braddr TO MADDR
                   MOVE      brcity TO MCITY
                   MOVE      brstate TO MSTATE
                   MOVE      BRZIP TO MZIP
                   endif
           endif
.>Patch 1.4 Comment Out                            
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion           
         CALL      HD1 IF NOT LESS

         MOVE       C0 TO N2
         clear      str2
         CLEAR      SLS
         pack       str2 from osales10,osales
         move       str2 to n2
         LOAD       SLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                    OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                    OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20:
                    OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.Patch 1.4 Comment Out                    
.         PRINT     *N,*01,MCOMP,B1,omlrnum,B2,"(",SLS,")":
.                   *N,*02,PERIOD
.Patch 1.4 Comment Out                   
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,MCOMP,B1,omlrnum,B2,"(",SLS,")";                 
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced                   
         ADD       C3 TO PRTLINES
         return
.
zeromlr1
           ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         pack      mkey from holdmlr,z3
         call      nmlrkey
         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
         if        not equal
         pack      nbrkfld from holdbrk,z3
         call      nbrkkey
                   if        not over
                   MOVE      mcomp TO MNAME
                   MOVE      brcomp TO MCOMP
           MOVE      braddr TO MADDR
                   MOVE      brcity TO MCITY
                   MOVE      brstate TO MSTATE
                   MOVE      BRZIP TO MZIP
                   endif
           endif
.>Patch 1.4 Comment Out                            
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion            
         CALL      HD1 IF NOT LESS
         MOVE       C0 TO N2
         clear      str2
         CLEAR      SLS
         pack       str2 from osales10,osales
         move       str2 to n2
         LOAD       SLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                    OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                    OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20:
                    OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
         return
.................................................................
MLR2     
.>Patch 1.4 Comment Out                    
.         COMPARE   C57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                  
         CALL      HD1 IF NOT LESS
.Patch 1.4 Comment Out         
.         PRINT     *N,*01,MCOMP,B1,omlrnum:
.                   *N,*02,PERIOD
.Patch 1.4 Comment Out                   
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,MCOMP,B1,omlrnum;            
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced
         ADD       C3 TO PRTLINES
.         CALL      PAGES
              move      omlrnum to holdmlr
              move      obrknum to holdbrk
              return
age
.age the record by "GUARANTY DATE"
         MATCH     "00" TO omdtec
         IF        EQUAL
         move      invdted to dd
         move      invdtem to mm
         move      invdtey to yy
         ELSE
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         ENDIF


AGE1     CALL      CVTJUL
         move       b4 to guartype
         if         (OBRKGUAR = "1")
         add        "30" to juldays
         move       "30" to guartype
         endif
         if         (OBRKGUAR = "2")
         add        "45" to juldays
         move       "45" to guartype
         endif
         if         (OBRKGUAR = "3")
         add        "60" to juldays
         move       "60" to guartype
         endif
         if         (OBRKGUAR = "4")
         add        "90" to juldays           .actually open ended.
         move       "Open" to guartype
         endif
         MOVE      SYSDAYS TO DIFFDAYS
         SUBTRACT  JULDAYS FROM DIFFDAYS
CHK90    COMPARE   "90" TO DIFFDAYS
         GOTO      CHK60 IF LESS
         GOTO      CHK60 IF EQUAL
         MOVE      AR TO DYOVER90
         GOTO      ageexit
CHK60    COMPARE   "60" TO DIFFDAYS
         GOTO      CHK30 IF LESS
         GOTO      CHK30 IF EQUAL
         MOVE      AR TO DY61TO90
         GOTO      ageexit
CHK30    COMPARE   "30" TO DIFFDAYS
         GOTO      CHK00 IF LESS
         GOTO      CHK00 IF EQUAL
         MOVE      AR TO DY31TO60
         GOTO      ageexit
CHK00    MOVE      AR TO DY00TO30
AgeExit
         return
..............................................................................
det1
.det1 - print Mailer detail line.
         MOVE      DATEMASK TO DATEPRT1
         CLEAR     STR6
         pack       str6 from omdtem,omdted,omdtey
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         CLEAR     STR6
         pack      str6 from INVDTEm,INVDTEd,INVDTEY
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT2
         MOVE      o1des TO LNAME17
.
         ADD       DY00TO30 TO CL00TO30     CLIENT TOTALS
         ADD       DY31TO60 TO CL31TO60
         ADD       DY61TO90 TO CL61TO90
         ADD       DYOVER90 TO CLOVER90

         
         ADD       DY00TO30 TO BR00TO30     BROKER TOTALS
         ADD       DY31TO60 TO BR31TO60
         ADD       DY61TO90 TO BR61TO90
         ADD       DYOVER90 TO BROVER90
.
         ADD       DYapTO30 TO CLapTO30     CLIENT TOTALS
         ADD       DYapTO60 TO CLapTO60
         ADD       DYapTO90 TO CLapTO90
         ADD       DYapOR90 TO CLapOR90

         
         ADD       DYapTO30 TO BRapTO30     BROKER TOTALS
         ADD       DYapTO60 TO BRapTO60
         ADD       DYapTO90 TO BRapTO90
         ADD       DYapOR90 TO BRapOR90
.
         ADD       AR       TO GRAR
         ADD       AP1      TO GRAP
         ADD       AP1      TO GRAP1
.>Patch 1.4 Comment Out                    .
.         COMPARE   c59 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                    
         CALL      HD1 IF NOT LESS
         move      mname to str20
.Patch 1.4 Comment Out         
.         PRINT        *02,PERIOD:
.                      *06,str20:
.                      *22,DY00TO30:
.                      *32,DY31TO60:
.                      *42,DY61TO90:
.                      *52,DYOVER90:
.                      *66,DATEPRT1:
.                      *75,oLRn:
.                      *84,guartype:
.                      *90,INVnum:
.                      *97,DATEPRT2:
.                      *102,AP1:
.                      *116,LNAME17
.Patch 1.4 Comment Out                      
.Patch 1.4 Replaced Logic Prtpage
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,period,b5,str20;             
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,DY00TO30;                              
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,DY31TO60;                    
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,DY61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,DYOVER90;                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,DATEPRT1;                                       
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,oLRn;             
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,guartype;                             
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,INVnum;         
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,DATEPRT2;                   
          prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ll,AP1;                       
          prtpage prfile;*pColumn11R:row,*ALIGNMENT=*Left,*ll,LNAME17;                    
        add     eightlpi,row            
.Patch 1.4 Replaced Logic Prtpage
.
         ADD       C1 TO PRTLINES
              return
..............................................................................
det2
.det2 - print list detail line.
         MOVE      DATEMASK TO DATEPRT1
         CLEAR     STR6
         pack      str6 from omdtem,omdted,omdtey
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         CLEAR     STR6
         pack      str6 from INVDTEm,INVDTEd,INVDTEY
         RESET     STR6                         .MMDDYY
         EDIT      STR6 TO DATEPRT2
         pack      mkey from omlrnum,z3
         call      nmlrkey
         MOVE      mcomp TO LNAME17
.
         ADD       DY00TO30 TO CL00TO30     CLIENT TOTALS
         ADD       DY31TO60 TO CL31TO60
         ADD       DY61TO90 TO CL61TO90
         ADD       DYOVER90 TO CLOVER90

         
         ADD       DY00TO30 TO BR00TO30     BROKER TOTALS
         ADD       DY31TO60 TO BR31TO60
         ADD       DY61TO90 TO BR61TO90
         ADD       DYOVER90 TO BROVER90
.
         ADD       DYapTO30 TO CLapTO30     CLIENT TOTALS
         ADD       DYapTO60 TO CLapTO60
         ADD       DYapTO90 TO CLapTO90
         ADD       DYapOR90 TO CLapOR90

         
         ADD       DYapTO30 TO BRapTO30     BROKER TOTALS
         ADD       DYapTO60 TO BRapTO60
         ADD       DYapTO90 TO BRapTO90
         ADD       DYapOR90 TO BRapOR90
.
         ADD       AR       TO GRAR
         ADD       AP1      TO GRAP
         ADD       AP1      TO GRAP1
.
.>Patch 1.4 Comment Out                    
.         COMPARE   c59 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                    
         CALL      HD2 IF NOT LESS
         move      o1des to str20
.Patch 1.4 Comment Out         
.         PRINT        *02,PERIOD:
.                      *06,str20:
.                      *22,DY00TO30:
.                      *32,DY31TO60:
.                      *42,DY61TO90:
.                      *52,DYOVER90:
.                      *66,DATEPRT1:
.                      *75,oLRn:
.                      *84,guartype:
.                      *90,INVnum:
.                      *97,DATEPRT2:
.                      *102,AP1:
.                      *116,LNAME17
.Patch 1.4 Comment Out
.Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,period,b5,str20;             
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,DY00TO30;                              
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,DY31TO60;                    
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,DY61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,DYOVER90;                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,DATEPRT1;                                       
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,oLRn;             
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,guartype;                             
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,INVnum;         
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,DATEPRT2;
          prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ll,AP1;                       
          prtpage prfile;*pColumn11R:row,*ALIGNMENT=*Left,*ll,LNAME17;                              
        add     eightlpi,row  
.Patch 1.4 Replaced Logic
.
         ADD       C1 TO PRTLINES
              return
hd1
         ADD       C1 TO PAGE
         compare    c1 to page
.>Patch 1.4 Comment
.         if         equal
..         print      hp17ptch,hpdups,hptop,*f
.         print      hp17ptch,hpdupl,hptop,*f
.         endif
.>Patch 1.4 Comment Out         
.>Patch 1.4 
          if not equal
                    PRTPAGE prfile;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                *ORIENT=*PORTRAIT:
                    *Duplex=2  
          endif                         
.>Patch 1.4
.START PATCH 1.3 REPLACED LOGIC
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Broker GUARANTY REPORT":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                    *N,*29,"0 - 30":
.                      *38,"31 - 60":
.                      *48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
.                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
.                      *112,"A/P":
.                      *116,"LIST NAME":
.                   *N,*26,"---------":
.                      *36,"---------":
.                      *46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
.>Patch 1.4 Comment Out
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Broker GUARANTY REPORT":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                    *N,*29,"0 - 30":
.                      *38,"31 - 60":
.                      *48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
..                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
..                      *112,"A/P":
.                      *116,"LIST NAME":
.                   *N,*26,"---------":
.                      *36,"---------":
.                      *46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
.>Patch 1.4 Comment Out                     
.>Patch 1.4 Prtpage Header
          clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date: ";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date: ",str10;        
.        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Names in the News";                  
        add     eightlpi,row        
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,"Broker Guaranty Report";           
        add     eightlpi,row        
        add     eightlpi,row                
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,MONTH,*boldoff;           
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
.         prtpage prfile;*pColumn:row,*ALIGNMENT=*Right,*ULON,*ll,"";         
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ULON,*ll,"0 - 30";            
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,"31 - 60";                   
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,"61 - 90";                             
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,"Over 90";                  
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"Maildate";                                     
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,"LR##";           
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"Mlr PO";                             
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,"INV##";                  
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,"INV Date";                 
          prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ll,"A/P";                     
          prtpage prfile;*pColumn11R:row,*ALIGNMENT=*Left,*ll,"List Name",*ULOFF,*boldoff;                    
        add     eightlpi,row   
        add     eightlpi,row       
.>Patch 1.4 Prtpage Header End
.END PATCH 1.3 REPLACED LOGIC
        MOVE      C8 TO PRTLINES
         RETURN
hd2

         ADD       C1 TO PAGE
         compare    c1 to page
.>Patch 1.4 Comment Out         
.         if         equal
..         print      hp17ptch,hpdups,hptop,*f
.         print      hp17ptch,hpdupl,hptop,*f
.         endif
.>Patch 1.4 Comment Out         
.>Patch 1.4 
          if not equal
                    PRTPAGE prfile;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                *ORIENT=*PORTRAIT:
                    *Duplex=2  
          endif                         
.>Patch 1.4         
.START PATCH 1.3 REPLACED LOGIC
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS, INC.":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Broker GUARANTY REPORT":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                    *N,*29,"0 - 30":
.                      *38,"31 - 60":
.                      *48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
.                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
.                      *112,"A/P":
.                      *116,"Mailer NAME":
.                   *N,*26,"---------":
.                      *36,"---------":
.                      *46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
.Patch 1.4 Comment Out
.         PRINT     *F,*n,*n,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Broker GUARANTY REPORT":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                    *N,*29,"0 - 30":
.                      *38,"31 - 60":
.                      *48,"61 - 90":
.                      *58,"OVER 90":
.                      *66,"MAILDATE":
.                      *78,"LR##":
.                      *83,"MLR PO":
.                      *92,"INV##":
.                      *97,"INV.DATE":
.                      *112,"A/P":
..                      *116,"Mailer NAME":
.                   *N,*26,"---------":
.                      *36,"---------":
.                      *46,"---------":
.                      *56,"---------":
.                      *66,"--------":
.                      *75,"------":
.                      *82,"-------":
.                      *90,"------":
.                      *97,"--------":
.                      *106,"---------":
.                      *116,"-----------------"
.Patch 1.4 Comment Out                      
.END PATCH 1.3 REPLACED LOGIC
.>Patch 1.4 Prtpage Header
          clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date: ";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date: ",str10;        
.        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Names in the News";                  
        add     eightlpi,row        
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,"Broker Guaranty Report";           
        add     eightlpi,row        
        add     eightlpi,row                
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,MONTH;          
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
.         prtpage prfile;*pColumn:row,*ALIGNMENT=*Right,*ULON,*ll,"";         
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ULON,*ll,"0 - 30";            
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,"31 - 60";                   
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,"61 - 90";                             
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,"Over 90";                  
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"Maildate";                                     
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,"LR##";           
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"Mlr PO";                             
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,"INV##";                  
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,"INV DATE";                 
          prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Right,*ll,"A/P";                     
          prtpage prfile;*pColumn11R:row,*ALIGNMENT=*Left,*ll,"Mailer Name",*ULOFF,*boldoff;                  
        add     eightlpi,row   
        add     eightlpi,row       
.>Patch 1.4 Prtpage Header End
        MOVE      C8 TO PRTLINES
         RETURN
phase4
.print by Manager/List
              move      c0 to count
              open      guareom,"\\nins1\e\data\BRKguareom2.srt",exclusive
.>Patch 1.4 Comment Out              
.              SPLOPEN   "\\nins1\E\DATA\BRKGUARBYOWN.LST"
.>Patch 1.4 Comment Out              
.>Patch 1.4 Logic Addition for PDF Quality Control
.begin patch 1.50

.                    Call      GetPDFPATH
.          
.                    pack      str55 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str55:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.          result
.          if (result = C0)
..Prepare Flag file
.                    pack      str55 from PDFPATH,"\flag.dat"
.                    prep      tempfile,str55
.                    write     tempfile,SEQ;"flag set"
.                    close     tempfile
.          endif
.>Patch 1.4 Logic Addition for PDF Quality Control                              
           PRTOPEN prfile,"PDF:","c:\work\pdf\BRKGUARBYOWN.PDF"
.           PRTOPEN prfile,"PDF995","BRKGUARBYOWN.LST"
.end patch 1.50
           PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                               *Duplex=2;                                                 
.>patch 1.4 End Addition              
              
phase4in
                MOVE      C0 TO DY00TO30
              MOVE      C0 TO DY31TO60
                MOVE      C0 TO DY61TO90
                MOVE      C0 TO DYapTO30
                MOVE      C0 TO DYapTO60
                MOVE      C0 TO DYapTO90
              MOVE      C0 TO DYOVER90
                MOVE      C0 TO DYapOR90
                MOVE      B12 TO COL1
                MOVE      B12 TO COL2
                MOVE      B12 TO COL3
                MOVE      B12 TO COL4
              read      guareom,seq;ordvars
              goto      phase4eoj if over
              add       c1 to count
              display   *p10:10,"Records output : ",count
              compare   c1 to count
                        if        equal
                           move      olon to holdown
                           move      olnum to holdlist
                        call      hd2                    .break header
                        pack      nownfld from holdown
                        call      nownkey
.>Patch 1.4 Comment Out                                         
.                       COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                                          
                        CALL      HD2 IF NOT LESS
.Patch 1.4 Comment Out
.                       PRINT     *N,*01,Hpbon,ownocpy,hpboff,B1,olon,B2:
.                                 *N,*02,PERIOD
.Patch 1.4 Comment Out
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,ownocpy,*boldoff;
          prtpage prfile;B1,olon;       
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced                             
                        ADD       C3 TO PRTLINES
                          endif
              match     olon to holdown
              call      obreak if not equal
              match     olnum to holdlist
              call      Lbreak if not equal
              move      olrn to ninvfld
              rep       zfill in ninvfld
              call      ninvkey
              if        not over
                move      no to mrgsw
              move      no to shipsw
              move      olrn to nmrgfld
              CALL      NMRGKEY
                       if        not over
                         move      yes to mrgsw
                         endif
              MOVE      olrn to nshpfld
              REP       ZFILL IN NshpFLD
              CALL      NshpKEY
                       if        not over
                       move      yes to shipsw
                       endif
              call      wipecvars
              move      c1 to ndatpath
              move      olnum to ndatfld
              call      ndatkey
              call      compute
              move       ap to ap1
              move      olrn to nadjfld
              call      nadjkey
                        if        not over
                        add       asrecadj to ar
                        add       aslrinc to lrinc
                      add       aspayad1 to ap1
                        add       aspayad2 to ap2
                      endif
              else
              move    c0 to ar
              move    c0 to lrinc
              move    c0 to ap1
              move    c0 to ap2
              SUB       FORM92 FROM FORM92
              SUB       FORM52 FROM FORM52
              MOVE      OQTY TO FORM92
              DIV       THOUS INTO FORM92
              MOVE      OPPM TO FORM52         .use price from order
              DIV       HUND INTO FORM52
              mult      form52 by form92
              move      form92 to ar              .estimated
              endif
              call      age
                call      det2
              goto      phase4in

phase4eoj
.END OF PHASE 4 PRINT TOTALS ETC CLOSE FILES AND stop
.EXCEPT FOR DEVELOPEMENT JUST STOP
         pack      nownfld from holdown
         call      nownkey
.>Patch 1.4 Comment Out                             
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                                                   
         CALL      HD2 IF NOT LESS
.>Patch 1.4 Comment Out          
.         PRINT     *N;
.         PRINT     *N,*01,HPbon,"<Manager TOTAL> ##",holdown,hpboff:
.                      *22,BR00TO30:
.                      *32,BR31TO60:
.                      *42,BR61TO90:
.                      *52,BROVER90
.>Patch 1.4 Comment Out                      
.>Patch 1.4 Logic Replaced  
          add     eightlpi,row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"<Manager TOTAL> ##",holdown;          
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,BR00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,BR31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,BR61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,BROVER90;                             
          add     eightlpi,row                    
.>Patch 1.4 Logic Replaced                        
         ADD       C3 TO PRTLINES

         ADD       CL00TO30 TO GR00TO30
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
.>Patch 1.4 Comment Out       
.         COMPARE   C57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                                                            
         CALL      Hd2 IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GR00TO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GR31TO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GR61TO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GROVER90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAR     TO COL5
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP     TO COL6
.
         MOVE      GR00TO30 TO PRC30
         DIVIDE    GRAR   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GR31TO60 TO PRC60
         DIVIDE    GRAR   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GR61TO90 TO PRC90
         DIVIDE    GRAR   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GROVER90 TO PRCOV
         DIVIDE    GRAR   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
.
         MOVE      NUMMASK  TO COL6
         EDIT      GRAP2    TO COL6
.Patch 1.4 Comment Out
.         PRINT     *N,"PROCESSED : ",count:
.                   *N:
.                   *N,"TOTALS:":
.                      *29,"0 - 30":
.                      *43,"31 - 60":
.                      *58,"61 - 90":
.                      *73,"OVER 90":
.                      *88,"TOTL AR":
.                      *103,"TOTL AP":
.                   *N,*26,"---------":
.                      *41,"---------":
.                      *56,"---------":
.                      *71,"---------":
.                      *86,"---------":
.                      *101,"---------":
.                   *N,*24,COL1:
.                      *39,COL2:
.                      *54,COL3:
.                      *69,COL4:
.                      *84,COL5:
.                      *99,COL6
.>Patch 1.4 Comment Out                      

.>Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Processed: ",count;         
          add eightlpi to row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Totals: ",count;              
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ULON,*ll,"0 - 30";                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,"31 - 60";                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"61 - 90";                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"OVER 90";                  
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,"TOTAL AR";                                     
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ULOFF,*ll,"TOTAL AP";
          add eightlpi to row           
          add eightlpi to row 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,COL1;                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,COL2;              
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,COL3;                       
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,COL4;             
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,COL5;                                 
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ll,COL6;  
          add eightlpi to row 
.>Patch 1.4 Replaced Logic

         MOVE      NUMMASK  TO COL6
         EDIT      GRAP1    TO COL6
.>Patch 1.4 Comment Out            
.         PRINT     *22,PRC30,PRC:
.                      *40,PRC60,PRC:
.                      *52,PRC90,PRC:
.                      *70,PRCOV,PRC:
.                      *99,COL6
.>Patch 1.4 Comment Out   
.>Patch 1.4 Replaced Logic
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,PRC30,PRC;                             
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PRC60,PRC;                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PRC90,PRC;                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,PRCOV,PRC;                  
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ll,COL6;                                
          add eightlpi to row 
.>Patch 1.4 Replaced Logic                      
         ADD       C7 TO PRTLINES
.>Patch 1.4 Comment Out                             
.         COMPARE   C57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion           
         CALL      Hd2 IF NOT LESS
         MOVE      NUMMASK  TO COL1
         EDIT      GRapTO30 TO COL1
         MOVE      NUMMASK  TO COL2
         EDIT      GRapTO60 TO COL2
         MOVE      NUMMASK  TO COL3
         EDIT      GRapTO90 TO COL3
         MOVE      NUMMASK  TO COL4
         EDIT      GRapOR90 TO COL4
         MOVE      NUMMASK  TO COL5
         EDIT      GRAp2    TO COL5
.
         MOVE      GRapTO30 TO PRC30
         DIVIDE    GRAp2   INTO PRC30
         MULTIPLY  "-1"     BY PRC30
         MULTIPLY  "100"    BY PRC30
         MULTIPLY  "-1"     BY PRC30
         MOVE      GRapTO60 TO PRC60
         DIVIDE    GRap2   INTO PRC60
         MULTIPLY  "-1"     BY PRC60
         MULTIPLY  "100"    BY PRC60
         MULTIPLY  "-1"     BY PRC60
         MOVE      GRapTO90 TO PRC90
         DIVIDE    GRAp2   INTO PRC90
         MULTIPLY  "-1"     BY PRC90
         MULTIPLY  "100"    BY PRC90
         MULTIPLY  "-1"     BY PRC90
         MOVE      GRapOR90 TO PRCOV
         DIVIDE    GRAp2   INTO PRCOV
         MULTIPLY  "-1"     BY PRCOV
         MULTIPLY  "100"    BY PRCOV
         MULTIPLY  "-1"     BY PRCOV
.>Patch 1.4 Comment Out 
.         print          *N:
.                   *N,"A/P TOTALS:":
.                      *29,"0 - 30":
.                      *43,"31 - 60":
.                      *58,"61 - 90":
.                      *73,"OVER 90":
.                      *88,"TOTL AP":
.                   *N,*26,"---------":
.                      *41,"---------":
.                      *56,"---------":
.                      *71,"---------":
.                      *86,"---------":
.                   *N,*24,COL1:
.                      *39,COL2:
.                      *54,COL3:
.                      *69,COL4:
.                      *84,COL5
.         PRINT     *22,PRC30,PRC:
.                      *40,PRC60,PRC:
.                      *52,PRC90,PRC:
.                      *70,PRCOV,PRC:
.                      *99,COL6
.>Patch 1.4 Comment Out                      
.>Patch 1.4 Replaced Logic
          add eightlpi to row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"A/P Totals: ",count;                    
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ULON,*ll,"0 - 30";                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,"31 - 60";                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"61 - 90";                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"OVER 90";                  
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,"TOTAL AR";                                     
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ULOFF,*ll,"TOTAL AP";
          add eightlpi to row           
          add eightlpi to row 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,COL1;                        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,COL2;              
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,COL3;                       
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,COL4;             
          prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,COL5;                                 
          add eightlpi to row 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,PRC30,PRC;                             
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PRC60,PRC;                   
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,PRC90,PRC;                            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,PRCOV,PRC;                  
          prtpage prfile;*pColumn12R:row,*ALIGNMENT=*Right,*ll,COL6;                                
          add eightlpi to row 
.>Patch 1.4 Replaced Logic    
.>Patch 1.4 Comment Out
.              SPLCLOSE
.>Patch 1.4 Comment Out              
.>Patch 1.4 Logic Replaced
          prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
.begin patch 1.50
.          call      GetPDfPAth
.          pack      str55 from PDFPATH,"\flag.dat"
.          pack      APIFileName,STR55,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
          pause     "2"
.          erase     STR55
.end patch 1.50
.>Patch 1.4 Logic Replaced              
              RELEASE
        pack MailAttach,"c:\work\pdf\BRKGUARBYOWN.PDF"
        Move "creques@nincal.com,gspranz@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
        
        Move "NIN Broker Guaranty report.",MAILSubjct
        Move "",MAILBody
.........................
          Move      Yes,Mailtrace

        Call  SendMail                 
              STOP
Lbreak
.print       total for LIST new header & current detail
.Patch 1.4 Comment Out
.         PRINT        *02,HPBon,PERIOD:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,b1,"TOTAL ",edittot,b5:
.                   *N,hpboff
.>Patch 1.4 Comment Out          
.>Patch 1.4 Logic Replaced
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,period;            
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                             
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"TOTAL ",edittot,b5;                             
          add     eightlpi,row                    
.>Patch 1.4 Logic Replaced

         ADD       C3 TO PRTLINES
         move      olnum to holdlist
           ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
              return
Obreak
.print       total for Owner & List. new header & current detail
.>Patch 1.4 Comment Out                    
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                    
         CALL      HD1 IF NOT LESS
.>Patch 1.4 Comment Out                   
.         PRINT        *02,PERIOD:
.                   *N,*01,ownocpy:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,*N
.>Patch 1.4 Comment Out          
.>Patch 1.4 Logic Replaced
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,period;            
          add     eightlpi,row                              
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,ownocpy;             
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                             
          add     eightlpi,row                    
.>Patch 1.4 Logic Replaced                      
         ADD       C3 TO PRTLINES
           ADD       CL00TO30 TO GR00TO30     GRAND TOTALS
         ADD       CL31TO60 TO GR31TO60
         ADD       CL61TO90 TO GR61TO90
         ADD       CLOVER90 TO GROVER90
.
.>Patch 1.4 Comment Out
.         PRINT        *02,HPBon,PERIOD:
.                      *22,CL00TO30:
.                      *32,CL31TO60:
.                      *42,CL61TO90:
.                      *52,CLOVER90,b1,"TOTAL ",edittot,b5:
.                   *N,hpboff
.>Patch 1.4 Comment Out     
.>Patch 1.4 Logic Replaced
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,period;              
          add     eightlpi,row                              
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,ownocpy;             
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,CL00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,CL31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,CL61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,CLOVER90;                             
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Left,*ll,"TOTAL ",edittot,b5,*boldoff;                                        
          add     eightlpi,row                    
.>Patch 1.4 Logic Replaced 
         ADD       C3 TO PRTLINES
.
         MOVE      C0 TO CL00TO30
         MOVE      C0 TO CL31TO60
         MOVE      C0 TO CL61TO90
         MOVE      C0 TO CLOVER90
         pack      nownfld from holdown
         call      nownkey
         move      c0 to farBRK
         add       br00tO30 to farBRK
         add       br31to60 to farBRK
         add       br61tO90 to farBRK
         add       brover90 to farBRK
         MOVE      NUMMASK TO COL5
         EDIT      fARBRK TO COL5
.>Patch 1.4 Comment Out                             
.         COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                    
         CALL      HD2 IF NOT LESS
.>Patch 1.4 Comment Out         
.         PRINT     *N;
.         PRINT     *N,*01,"<Manager TOTAL> ##",holdown:
.                      *22,BR00TO30:
.                      *32,BR31TO60:
.                      *42,BR61TO90:
.                      *52,BROVER90,b1,"GRAND TOTAL ",COL5
.>Patch 1.4 Comment Out
.>Patch 1.4 Logic Replaced  
          add     eightlpi,row
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"<Manager TOTAL> ##",holdown;          
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Right,*ll,BR00TO30;                    
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Right,*ll,BR31TO60;                              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,BR61TO90;                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,BROVER90,b1,"GRAND TOTAL ",COL5;                           
          add     eightlpi,row                    
.>Patch 1.4 Logic Replaced                      
         MOVE      C0 TO BR00TO30
         MOVE      C0 TO BR31TO60
         MOVE      C0 TO BR61TO90
         MOVE      C0 TO BROVER90
         move      c0 to farBRK
         ADD       C3 TO PRTLINES
              move      olnum to holdlist
              move      olon to holdown
                        pack      nownfld from holdown
                        call      nownkey
.>Patch 1.4 Comment Out                                         
.                       COMPARE   c57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                                          
                        CALL      HD2 IF NOT LESS
.Patch 1.4 Comment Out                      
.                       PRINT     *N,*01,hpbon,ownocpy,hpboff,B1,olon,B2:
.                                 *N,*02,PERIOD
.Patch 1.4 Comment Out
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,ownocpy,*boldoff;
          prtpage prfile;B1,olon;       
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced
                        ADD       C3 TO PRTLINES
         return
.
.................................................................
list2     
.>Patch 1.4 Comment Out                    
.         COMPARE   C57 TO PRTLINES
.>Patch 1.4 Comment Out                    
.>Patch 1.4      Code Added   
          COMPARE  "9900" to ROW
.>Patch 1.4     End Code Addtion                                                   
         CALL      HD2 IF NOT LESS
.Patch 1.4 Comment Out         
.         PRINT     *N,*01,hpbon,ownocpy,hpboff,B1,olon:
.                   *N,*02,PERIOD
.Patch 1.4 Comment Out
.Patch 1.4 Logic Replaced
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,ownocpy,*boldoff;
          prtpage prfile;B1,olon;       
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,PERIOD;                    
        add     eightlpi,row            
.Patch 1.4 Logic Replaced                   
         ADD       C3 TO PRTLINES
              move      olon to holdown
              move      olnum to holdlist
              return
.
balread
         CLEAR     moaamt
         clear     nmoafld4
         PACK      NMOAFLD4 FROM holdbrk,HOLDmlr
         REP       ZFILL IN NMOAFLD4
         move      no to over
         CALL      NMOBKEY
         cmatch    "Y" to over
         if         equal
         move      "0.00" to balance
         endif
        MOVE      DOLLAR TO moaamt
         MULT      "-1" BY BALANCE
         MOVE      BALANCE TO moa$
         EDIT      moa$ TO moaamt
         return
              stop
.........................................................................................
.io includes
              include   nordio.inc
              include   npgeio.inc
.begin patch 1.4
.              include   ninvio.inc
.              include   compute.inc
               include        compute.inc
               include        ninvio.inc
               Include        NinvAcdio.inc
.end patch 1.4
.Patch1.2
                              include   compio.inc
                              include   cntio.inc
.              include   nbrkio.inc
.              include   nmlrio.inc
.Patch1.2
              include   nmoaio.inc
              include   nmobio.inc
              include   nownio.inc
              include   nadjio.inc
              include   nmrgio.inc
              include   nshpio.inc
              include   ndatio.inc
              include   ndat3io.inc
.              include   compute.inc
              include   nacdio.inc
              include   comlogic.inc


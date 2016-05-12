*******************************************************************************
.NORD013C     "UNBILLED Batch Summary REPORT"

PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NORD2DD.INC
.patch2.2
                                        include compnotesdd.inc
                              include   compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch2.2
         include   nmrgdd.inc
.patch2.2
.         include   nbrkdd.inc
.patch2.2
         include   oslspern.inc
         include   nrtndd.inc
         include   nmlddd.inc
         include   hp.inc
.
release   init    "2.4"                DLH     .use data manager
reldate   Init      "2 February 2011"
.release   init    "2.3"                DLH      cleanup LM vs Brokerage
.reldate   Init      "23April 2009"
.release  init    "2.23"                08Mar2007  DLH      Oslspern.inc expansion
.release  init      "2.22"        JD     17JAN2006   Use order sales person for printing.
.release  init      "2.21"        JD    03JUN2005   changed input name to NINORDU.SRT.
.release  init      "2.2"        DMB    26MAY2004 Mailer Conversion
.RELEASE  INIT      "2.1"      08Mar02    added read of new mail date revision file.
.RELEASE  INIT      "2.0"     02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "1.9"     30DEC98 ASH NINORD Y2K, File expansion
.Release  init      "1.8"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.release  init       "1.7"            DLH  10Jun98 turn off locks on mailer read
.release  init      "1.62"           DLH  mar98   turn off read only open of input file
.release  init      "1.61"            JD  21jun95 skip lstmgt on batch.
.release  init      "1.60"           JD   15mar95 merged summary by brokerage.
.release  init      "1.51"           DLH '14FEB95 batch & adjust code.
.release  init      "1.5"            DLH 11Jan95    flag if merge info online.
.RELEASE  INIT      "1.0"            DLH 28jun93    new
. .............................................................................
. WORK VARIABLES
.
PDATE    DIM       8
TOTALDOL FORM      7.2
AR       FORM      7.2
. 
.
UNBILINC FORM      7.2
mlrbrk   dim       4
mrgbrk   dim       12
brnum    dim       4
cntnum   dim       3
ordnum   form      4
cnthold  dim       3
.start patch #1.9 - increase var
.mdate    dim       8
mdate    dim       10
.end patch #1.9 - increase var
sls      dim       15
mrgflag  form      1
. 
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
+ *****************************************************************************
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "59"
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
SYSJDATE FORM      5
brflag   form       1
.LSTMSW   DIM       1                  LIST MANAGEMENT INDICATOR.
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NORD013C" TO PROGRAM
         MOVE      "UNBILLED Batch Summary REPORT" TO STITLE
          MOVE      "Names In The News" TO COMPNME
.
         move      b4 to mlrbrk
         IFZ       PC
.patch2.21
.         MOVE      "NINORD.SRT" TO NORDNAME
         MOVE      "NINORDU.SRT|10.10.30.103:502" TO NORDNAME
.patch2.21
         CLOCK     DATE TO DATE
         MOVE      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    PDATE INTO MM,str1,DD,str1,YY
         XIF
         CLOCK     TIME TO TIME
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      TRAPS
NOTHING  RETURN
.
TRAPS    TRAP      IO GIVING ERROR NORESET IF IO
         DISPLAY   *P1:24,*EL,"OPENING FILES";
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "input " TO FERROR
.        open      nordfile,nordname,read              .using read with filepi's seems to create
.                                                      .a hard lock DLH 30Mar98
         open      nordfile,nordname
         move      c1 to nordflag
         move      c3 to nmlrlock
         SCAN      "BROKER" IN COMMENT
         if        equal
         move      c2 to brflag
         else
         move      c1 to brflag
         endif
         branch     brflag of splone,spltwo
.START PATCH 2.0 REPLACED LOGIC
.splone  SPLOPEN   "g:\DATA\unbill1.LST"       
.        goto       spldone
.spltwo  SPLOPEN   "g:\DATA\unbill2.LST"       
splone  PACK      STR35,NTWKPATH1,"unbill1.LST"
        SPLOPEN   STR35
        goto       spldone
spltwo  PACK      STR35,NTWKPATH1,"unbill2.LST"
        SPLOPEN   STR35
.END PATCH 2.0 REPLACED LOGIC
.         OPEN      ACCOUNT1,"NINMOB",SHARE
spldone  MOVE      "                    " TO FERROR
         CALL      FUNCDISP
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         TRAP      ABORT IF F5
*****************
*for testing only*************************************************
*****************
.
START    MOVE      C0 TO LINES
         MOVE      C0 TO TOTALDOL
.
START1
.
GETREC   
.        DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
.
         call      rotdial
         CALL      NORD2SEQ
         GOTO      EXIT IF OVER
         ADD       C1 TO COUNTO
         DISPLAY   *P1:24,*EL,COUNTO;
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       *YES
.begin patch 1.8
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       lcr order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 1.8
         RESET     CANCODES               *RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       *CANCELLED?
         GOTO      GETREC IF EQUAL
         move      c1 to nmlrpath
         pack      mkey from omlrnum,ocobn
         rep       zfill in mkey
         call      nmlrkey
         MOVE      NO TO LSTMSW
          MOVE      C1 TO Nbrkpath
         PACK      nbrkfld FROM Obrknum,obrkcnt
         REP       ZFILL,nbrkfld
         clear     brsales
         CALL      nbrkkey
         match     "06" to brsales
         if        equal
         move       yes to lstmsw
         CLEAR      MCODE         .CLEAR VAR.
         endif
         branch    brflag of nonbrk,brkchk
nonbrk   cmatch    "B" to mcode         .batch bill?
         if        not equal
         cmatch    "A" to mcode
         goto      getrec if not equal
         endif
.         MOVE      C1 TO Nbrkpath
.         PACK      nbrkfld FROM Obrknum,obrkcnt
.         REP       ZFILL,nbrkfld
.         clear     brsales
.         CALL      nbrkkey
.         match     "06" to brsales
.         if        equal
.         goto      getrec
.        ENDIF
         goto      goodone
brkchk   cmatch    "B" to mcode         .batch bill?
         goto      getrec if equal
         cmatch    "A" to mcode         .batch bill?
         goto      getrec if equal
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
.begin patch 2.3         
          If        (str2 = "02" or Str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
.         MATCH     "06" TO STR2
.         goto      getrec if equal
          GOto      Getrec
          endif
.end patch 2.3         
goodone  move      olrn to nmrgfld
         match     omlrnum to mlrbrk          .same mlr?
         goto      contin if not equal
         match     omlrky to mrgbrk           .same merge?
         goto      contin if not equal
         add       c1 to ordnum
chkers
         MATCH     ORTNNUM TO NRTNFLD
         IF        NOT EQUAL
         clear      rtcomp
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         endif
         call      nmrgkey
         if        not over
         move      c1 to mrgflag
         endif
         goto      getrec

.
CONTIN   match     b4 to mlrbrk      .1st good record?
         if        equal             .yes
         add      c1 to ordnum
         move     omlrnum to mlrbrk
         move     ocobn to cnthold
         move     omlrky to mrgbrk
         move     obrknum to brnum
         move     obrkcnt to cntnum
.START PATCH 2.1
          pack      NMLDFLD1,"01X",OLRN
        clear   str8
          pack      str8,"99999999"
          call      NMLDAIM
          loop
                    until over
                    if (NMLDDATE < str8)
                              move      NMLDDATE,str8
                    endif
                    call      NMLDKG
          repeat
          if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
          else
.Use current Mail Date
          endif
.end PATCH 2.1
.start patch #1.9 - increase var
.         pack      mdate from omdtem,slash,omdted,slash,omdtey
         pack      mdate from omdtem,slash,omdted,slash,omdtec,omdtey
.end patch #1.9 - increase var
         goto     chkers
         endif
         CALL      DETAIL
         GOTO      chkers               *ADDITIONAL CRITERIA FAILED GET NEXT RE
.
DETAIL    pack      mkey from mlrbrk,cnthold
         call      nmlrkey
         call       print
         move       omlrnum to mlrbrk
         move      omlrky to  mrgbrk
         move     ocobn to cnthold
.start patch #1.9 - increase var
.         pack      mdate from omdtem,slash,omdted,slash,omdtey
         pack      mdate from omdtem,slash,omdted,slash,omdtec,omdtey
.end patch #1.9 - increase var
         move      c1 to  ordnum
         move      c0 to mrgflag
         return
.
*......................................................................
header   branch    brflag of header1,header2
HEADER1   ADD       C1 TO PAGE
         PRINT     *F,*L,*1,hpdtch12,"CONFIDENTIAL   N I N   U N B I L L":
                   " E D   B A T C H   O R D E R S",hpt650,"DATE:",PDATE:
                   *L,*1,PROGRAM,hpt650,"PAGE ## ",PAGE:
                   *L,*L,*1,hpdtch10,"MAILER ",hpt250,"MERGE":
                   hpt350,"MAILDATE",hpt450,"## Open Orders"
         MOVE      C5 TO LINES
         RETURN
HEADER2   ADD       C1 TO PAGE
         PRINT     *F,*L,*1,hpdtch12,"CONFIDENTIAL   N I N   U N B I L L":
                   " E D   B R O K E R A G E   O R D E R S",hpt650,"DATE:",PDATE:
                   *L,*1,PROGRAM,hpt650,"PAGE ## ",PAGE:
                   *L,*L,*1,hpdtch10,"MAILER ",hpt250,"MERGE":
                   hpt350,"MAILDATE",hpt450,"## Open Orders"
         MOVE      C5 TO LINES
         RETURN
*......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
         MOVE       C0 TO N2
         CLEAR      SLS
         match     b1 to brnum
         if        not equal
         pack      nbrkfld from brnum,cntnum
         call      nbrkkey
;         move      brsales to n2
.patch2.22
         pack       str2 from osales10,osales
                              move       str2 to n2
.patch2.22
         goto      loader
         endif
;         MOVE       MSLSPER TO N2
.patch2.22
         pack       str2 from osales10,osales
                              move       str2 to n2
.patch2.22
loader   LOAD       SLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                    OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                    OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20:
                    OSLS21,OSLS22,osls23,osls24,osls25:
                  osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.
         PRINT     *1,hpdtch85,mlrbrk:
                   b2,mcomp,hpt250,mrgbrk,hpt350,mdate:
                   hpt450,ordnum,b2,hpdtch06,sls,b1,rtcomp;
         compare   c1 to mrgflag
         if        equal
         print     b1,hpdtch85,hpt675,hpbon,"Merged",hpboff
         else
         print     b1,hpdtch85
         endif          
         ADD       C1 TO LINES
         move     obrknum to brnum
         move     obrkcnt to cntnum
         return
* ***************************************************************************
*  EXIT
* ****************************************************************************
.
EXIT

         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         BEEP
         SPLCLOSE
         release
         shutdown
ABORT    TRAPCLR   F5
         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT
* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - RANGE ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - FORMAT ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - PARITY ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      PARITY

         INCLUDE   NORD2io.INC
.patch2.2
                              include compnotesio.inc
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.         INCLUDE   NORD2IO.inc
.patch2.2
         include   nmrgio.inc
.patch2.2
.         include   nbrkio.inc
.patch2.2
         include   nrtnio.inc
         include   nmldio.inc
         INCLUDE   COMLOGIC.inc


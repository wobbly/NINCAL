.............................................................................
.NEOM0003
. THIS IS THE EXCHANGE STATUS PICK OFF PROGRAM
.............................................................................
.
* VARIABLES USED BY THE EXCHANGE STATUS PROGRAM.
.
.
PC       EQU       0
         listoff
         INCLUDE   COMMON.INC
         listoff
         INCLUDE   CONS.INC
         listoff
.START PATCH 3.16 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 3.16 REPLACED LOGIC
         listoff
         INCLUDE   NDATDD.INC
         listoff
.START PATCH 3.17.5 ADDED LOGIC
          include   NXNGDD.INC
          include   NXCHDD.INC
.END PATCH 3.17.5 ADDED LOGIC
..............................................................................
RELEASE  INIT      "3.24"           DLH need to move to the new automated reports screen program 34 and include comments - ie where salesperson needs to route reports
reldate   Init      "2014 February 28"
.RELEASE  INIT      "3.23"           DLH more cleanup use butil job :Nxchg instead of xchneom mess
..                                   remove or comment out some of the confusing unneeded code, more cleanup should be done
.reldate   Init      "2013 October 31"
.
.RELEASE  INIT      "3.22"           DLH Pull user from primary input file
.reldate   Init      "2013 September 30"
.RELEASE  INIT      "3.21"           DLH Run Butil locally
.reldate   Init      "2013 May 7"
.RELEASE  INIT      "3.20"           DLH 64bit and fixed file writes which have clearly been wrong a long time
.reldate   Init      "30 August 11"
.RELEASE  INIT      "3.19"           DLH Move batch server C
.reldate   Init      "06 April 09"
.RELEASE  INIT      "3.18"           18JUN2005  DMB         Changed IP address of File Manager
.RELEASE  INIT      "3.17.5"           14mar2005  ASH     Exchange File conversion
.RELEASE  INIT      "3.17"           20DEC2004  DMB         Emily's list add last transaction date
.;RELEASE  INIT      "3.16"           27MAY2004  ASH         MAILER CONVERSION
.RELEASE  INIT      "3.15"           24OCT2002 added two new option offered by nxch0003
.;RELEASE  INIT      "3.14"           11APR2001 ASH Added file to File Manager
.RELEASE  INIT      "3.13"           06NoV2000 JD Starting atexstat50 for auto pdfing.
.RELEASE  INIT      "3.12"          02OCT2000 ASH NEW SERVER ADDED
..30OCT2000 DLH for some reason "trap" for good file was always ok, so job kept creating
. exstat01 over and over again. fixed.
.RELEASE  INIT      "3.11"          28apr00   added prin location
.RELEASE  INIT      "3.1"          08JUN99   ASH NINXCHNG Y2K FILE EXPANSION
.release  init      "3.0"          24Sep98   ASH NINMLR Y2K File expansion
.Release   init      "2.9.1"       25Sep98 DLH suppress Inactive accounts
.release  init      "2.9"          30nov97   Jd use new batch.
.release  init      "2.8"         26nov96   DLH universe default ? = yes
..release  init      "R2.7"        22dec95   added mlrhelp mlr keyin #2
.release  init      "R2.6"       26OCT94   DLH add option for list universe
.RELEASE  init      "R2.5"       16sep94   DLH fixed problem with single (L)
.                               reports ie payable only & no records found.
.RELEASE  INIT      "R2.4"       14sep94   DLH add in house v1
.RELEASE  INIT      "R2.3"       12MAR92   DLH NDATXX
.RELEASE  INIT      "2.2"           17DEC91
.                               CONVERTED TO NMLRXX & MLRHELP INCLUDES.
.RELEASE  INIT      "2.1"          11OCT91 DLH
.                                 ADDITION OF COMBINED TOTALS REP.
.RELEASE  INIT      "2.0"
.                 09SEP87
.                 05/10/83
. MAJOR REVISIONS 12/17/82
. Written for Names in the News California By David Herrick                   *
*******************************************************************************
RECMST   FILE    FIXED=260         PICK OFF OUTPUT FILE
..........................
ACCOUNT1 AFILE      FIXED=26
XCHNG    IFILE     KEYLEN=17,FIXED=200,STATIC=3
EOMXCHNG FILE      fixed=85               MASTER INPUT FILE, (CLIENTS NEEDING EOM
.company number     1-6
.fill               7-10
.name              11-35
.Inits             36-38
.fill              39-39
.copy              40-40
.fill              41-60
.user              61-68 

PAYOUT   FILE     FIXED=260
RECOUT   FILE     FIXED=260
EVNOUT   FILE     FIXED=260

. .........................................
Rcount    form      2
+ .............................................................................
.
BLANK08  DIM       8              NOT USED 13-20
MLR1     DIM       6              MAILER#1
MLR2     DIM       6              MAILER # 2
.END PATCH 3.17.5 REPLACED LOGIC
.Start Patch #3.0 - var expanded to reflect NINMLR expansion
.MDES1    DIM       25
MDES1    DIM       45
.End Patch #3.0 - var expanded to reflect NINMLR expansion
.
+ .WORK FIELDS
.
.NO       FORM      2
FMESG    DIM       24
.START PATCH 3.1 - REPLACED LOGIC
.WORK09A  FORM      9
.WORK09B  FORM      9
WORK10A  FORM      10
WORK10B  FORM      10
.END PATCH 3.1 - REPLACED LOGIC
.START PATCH 3.17.5 REPLACED LOGIC
.MATCHKEY DIM       8
MATCHKEY DIM       12
.END PATCH 3.17.5 REPLACED LOGIC
SENTRY   DIM       5              *TEMP. FIELD FOR ENTRY NUMBER.
BR       FORM      2
NSW      DIM       1              SWITCH FOR NEW RECORD CONTAINS "N" IF
.                                 RECORD IS TO BE CREATED
PRTSW    DIM       1              PRINT SWITCH
COUNT    FORM      4              NUMBER OF RECORDS FOUND COUNTER
RECNAME  DIM       35             FIELD USED IN CREATION OF OUTPUT FILE.
FILENUM  form      2              NUMERIC WORK FIELD USED IN CREATION OF OUTPUT
NEWNAME  DIM       35              SEE RECNAME
LNAME    DIM       35             LIST DESC.
BLANK35  INIT      "                                   "
F2       DIM       2
.PRTFILE  DIM       9
BAL      DIM       1               BAL="Y" IF BALANCES <1000 TO BE SUPPRESSED.
FNUM     FORM      1
ALL      DIM       1               = Y IF ALL TOTALS OPTION CHOSEN
COUNT1   FORM      4
COUNT2   FORM      4
COUNT3   FORM      4
FILE1    DIM       9
FILE2    DIM       9
FILE3    DIM       9
PASS     FORM      1
KMAILER  DIM       4                  KEYIN FIELD FOR MAILER##;
KEYCOUNT FORM      2
CHKMLR   DIM       4
.
+ PRINT VARIABLES
. ...............
.
NOFILE   DIM       8
.START PATCH 3.17.5 REPLACED LOGIC
.TEMPMLR1 DIM       4
TEMPMLR1 DIM       6
.END PATCH 3.17.5 REPLACED LOGIC
PICSW    DIM       1                   HOLDS (1) IF PAYABLE PICK,(2) IF REC.
.                                      (3) IF EVEN (4) v (5) v1
PICNUM   FORM      1                         
STOPLOOP DIM       1
DONESW   DIM       1
NUM      FORM      1                  BRANCHING CONSTANT.
ALEFT    INIT      "01X"
ARIGHT   INIT      "02X"
.START PATCH 3.17.5 REPLACED LOGIC
.AKEY1    DIM       7
.AKEY2    DIM       7
AKEY1    DIM       9
AKEY2    DIM       9
.END PATCH 3.17.5 REPLACED LOGIC
QUES     INIT      "????"
copy     dim       2         number of copies
.TASKNAME DIM       160
duplex   form      1
univflag form      1               1=no, 2=yes, default 26Nov96
ANS      DIM       1
;patch3.15
str33     DIM       33
NUsedflag INIT      "Y"
TranFlag  Form      "1"             "2" = Print Last Transaction date option on summary report default = "1"
;patch3.15
         liston
+******************************************************************************
. PROGRAM
*******************************************************************************
.
PREP     MOVE      "NEOM0003" TO PROGRAM
         MOVE      "EXCHANGE PICK OFF" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         MOVE      "ABORT" TO PF1
         MOVE      "EXIT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
         DISPLAY   *P15:24,"FILES ARE BEING OPENED";
.
         MOVE      B1,STAT              CLEAR FIELD SO CORRECT STATUS WRITTEN
         MOVE      B1 TO DONESW
         TRAP      INT IF INT
         TRAP      ABORT IF F1
         TRAP      ERROR IF IO
         TRAP      EXIT IF F5
         MOVE      "EOMXCHNG" TO FMESG
         DISPLAY   *P1:23,*EL,"OPENING ",FMESG;
         IFZ      PC
         OPEN      EOMXCHNG,"EOMXCHNG.MLR"
         XIF
         MOVE      "EXCHANGE FILE ",FMESG
.START PATCH 3.14 REPLACED LOGIC
.         OPEN      XCHNG,"NINXCHNG",SHARE
.         MOVE      "NINLRXNG FILE ",FMESG
.         MOVE      "NINXNUM AFILE" TO FMESG
.         OPEN      ACCOUNT1,"NINXNUM"
.>Patch 3.18 Code Begin
.         OPEN      XCHNG,"NINXCHNG.ISI|20.20.30.103:502",SHARE
         OPEN      XCHNG,"NINXCHNG.ISI|NINS1:502",SHARE
         MOVE      "NINLRXNG FILE ",FMESG
         MOVE      "NINXNUM AFILE" TO FMESG
.         OPEN      ACCOUNT1,"NINXNUM.AAM|20.20.30.103:502"
         OPEN      ACCOUNT1,"NINXNUM.AAM|NINS1:502"
.>Patch 3.18 End
.END PATCH 3.14 REPLACED LOGIC
         TRAPCLR   IO
.         READ      EOMXCHNG,SEQ;TEMPMLR1,B4,STR25,STR3,B1,COPY
          Goto      New

         READ      EOMXCHNG,SEQ;TEMPMLR1,B4,STR25,STR3,B1,COPY,User
         GOTO      EXIT IF OVER
         MOVE      C0 TO PICSW
         MOVE      C0 TO NUM
.
START    MOVE      C1 TO FNUM
         move      "P" to str1
         DISPLAY    *P35:07,*cyan,"(P)",*white,"ick off":
                   *P35:09,"(F)inished";
         MOVE      B1,NSW
.
.         REP       "F1P2" IN str1
.         MOVE      str1 TO NUM
.         BRANCH    NUM OF EXIT,PRINT
.         GOTO      START
PRINT    MOVE      B1 TO ALL
         CALL      PAINT
         CALL      FUNCDISP
         move      c3 to br
         DISPLAY   *P30:12,"1) By Mailer ##/Mailer ## (detailed)":
                   *P30:13,"2) Totals By Mailer## (Current Balances(L))":
                   *P30:14,"3) Totals By Mailer## (Combined Report",*cyan:
                   "(",*dion,"V",*dioff,")",*white,") ":
                   *P30:15,"4) Totals By Mlr## (Cmbnd Rep for viewing)":
                   *P30:16,"5) Totals By Mailer## (Combined Inhouse":
                   "(V1) "
         MOVE      "0000",COUNT

COPIES

duplex   move      c2 to duplex
.START PATCH 3.17.5 REPLACED LOGIC
.                             if (TEMPMLR1 = "1746")
                              if (TEMPMLR1 = "001972")      .Emily's List
.END PATCH 3.17.5 REPLACED LOGIC
                                        move c2 to tranflag
                              else
                                        move c1 to tranflag
                              endif
.Patch 3.17
univques move      c2 to univflag
         GOTO      pic3
.
PICWRT   CMATCH    YES TO ALL           *ALL REPORTS SELECTED?
         GOTO      PICWRTA IF EQUAL     *YES, SKIP COMPARE ALL TYPES ACCEPTED.
         BRANCH    BR OF PICWRTA        *DETAIL REPORT, CUT CRAP AND WRITE.
         MOVE      PICSW TO PICNUM
         COMPARE   PICNUM TO NUM        *BALANCE TYPE REQUESTED?
         GOTO      PIC3AA IF NOT EQUAL    *NO.
.
PICWRTA  ADD       "1" TO COUNT
         DISPLAY   *P1:22,*EL,"NUMBER OF RECORDS FOUND EQUALS ",COUNT
.
         MOVE      LIST TO NDATFLD
         REP       ZFILL IN NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         GOTO      PICWRT1 IF OVER
         MOVE      OLSTNAME TO LNAME
         GOTO      PICWRT2
PICWRT1  MOVE      "LIST DESC. NOT FOUND ",LNAME
PICWRT2
         REP       ZFILL,LIST
picwrt3
          WRITE     RECMST,SEQ;nxchvars,Olstname,compCOMP,b1
          goto      pic3AA
WRITBR
WRITPAY
.START PATCH 3.17.5 REPLACED LOGIC
.         WRITE     PAYOUT,SEQ;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,EXCOMMNT,LNAME,MCOMP
          WRITE     Payout,SEQ;nxchvars,Olstname,compCOMP,b1
.         WRITE     PAYOUT,SEQ;EXKEY,LR2,LR,USAGE1,USAGE2:
.                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT,LNAME,COMPCOMP
.END PATCH 3.17.5 REPLACED LOGIC
         ADD       C1 TO COUNT1
         GOTO      PIC3AA
WRITREC
.START PATCH 3.17.5 REPLACED LOGIC
.         WRITE     RECOUT,SEQ;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,EXCOMMNT,LNAME,MCOMP
          WRITE     RECout,SEQ;nxchvars,Olstname,compCOMP,b1
.         WRITE     RECOUT,SEQ;EXKEY,LR2,LR,USAGE1,USAGE2:
.                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT,LNAME,COMPCOMP
.END PATCH 3.17.5 REPLACED LOGIC
         ADD       C1 TO COUNT2
         GOTO      PIC3AA
WRITEVN
.START PATCH 3.17.5 REPLACED LOGIC
.         WRITE     EVNOUT,SEQ;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,EXCOMMNT,LNAME,MCOMP
          WRITE     evnout,SEQ;nxchvars,Olstname,CompCOMP,b1
.         WRITE     EVNOUT,SEQ;EXKEY,LR2,LR,USAGE1,USAGE2:
.                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT,LNAME,COMPCOMP
.END PATCH 3.17.5 REPLACED LOGIC
         ADD       C1 TO COUNT3
         GOTO      PIC3AA
.
ALLDONE  DISPLAY  *P01:22,*EF,"ALL PICKS ARE DONE":
                   *P1:22,*R,FILE1," NUMBER OF RECORDS = ",COUNT1:
                   *P1:22,*R,FILE2," NUMBER OF RECORDS = ",COUNT2:
                   *P1:22,*R,FILE3," NUMBER OF RECORDS = ",COUNT3,*R;
         MOVE      C0 TO PICSW
.         GOTO      PICDON1
PICDONE  
.          BRANCH    BR OF PICDON1A
.         COMPARE   C2 TO PASS
.         GOTO      TOTPASS2 IF NOT EQUAL
.PICDON1A DISPLAY   *P01:22,*EL,"PICK OFF IS COMPLETE ",COUNT," RECORDS FOUND ":
.                   "WRITTEN TO ",*B,NEWNAME;
.         COMPARE   C3 TO BR
.         branch    br of weof,chkdone,weof,weof,weof
..         IF        NOT EQUAL
.chkdone  CMATCH    YES TO ALL
.         GOTO      CLOSEALL IF EQUAL
.         branch    num of weofpay,weofrec,weofevn
.weofpay  weof      payout,seq
.         DISPLAY   *P01:22,*EL,"PICK OFF IS COMPLETE ",COUNT1," RECORDS FOUND ":
.                   "WRITTEN TO ",*B,NEWNAME;
.         goto      picdon1
.weofrec  weof      recout,seq
.         DISPLAY   *P01:22,*EL,"PICK OFF IS COMPLETE ",COUNT2," RECORDS FOUND ":
.                   "WRITTEN TO ",*B,NEWNAME;
.         goto      picdon1
.weofevn  weof      evnout,seq
.         DISPLAY   *P01:22,*EL,"PICK OFF IS COMPLETE ",COUNT3," RECORDS FOUND ":
.                   "WRITTEN TO ",*B,NEWNAME;
.         goto      picdon2
.
..         ENDIF
weof     WEOF      RECMST,SEQ
chkcnt   call Get64OS
         CLOSE     RECMST,EOFSIZE
         COMPARE   "0000",COUNT
         GOTO      New IF EQUAL
.         goto      picdon1b
.         match     "00" to copy
.         goto      picdon1b if equal
.         clear     taskname
          clear     taskname
          append    "!\\nins1\winbatch\BUTIL job=nxchg INfile=",TASKNAME
          APPEND    NEWNAME TO TASKNAME
          APPEND    " B=",TASKNAME
          APPEND    user TO TASKNAME
          append    " PDF=Y " to taskname
         RESET     TASKNAME
         EXECUTE   TASKNAME




.         clear     taskname
.         append    "\\Nins1\Winbatch\butil job=nxchgeom",TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         RESET     TASKNAME
.         EXECUTE   TASKNAME
          goto      New
.         compare   c4 to br
.         goto      xview if equal
.         compare   c5 to br
.         goto      inhouse if equal
.         branch    duplex of side1,side2
.         
.side1    
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nins1\ServerC \\nins1\winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          else
.          append    "\\nins1\winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          endif
.         goto      picdon1c
.side2
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nins1\ServerC \\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          Else
.          append    "\\nins1\winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          endif
.         goto      picdon1c
.xview    branch    duplex of xview1,xview2
.xview2   
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=xchlstdp INfile=",TASKNAME
..          else
.          append    "\\nins1\winbatch\butil.exe job=xchlstdp INfile=",TASKNAME
..          endif
.          goto      picdon1c
.xview1
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=xchlst INfile=",TASKNAME
..          else
.          append    "\\Nins1\Winbatch\butil.exe job=xchlst INfile=",TASKNAME
..          endif
.         goto      picdon1c
.inhouse  branch    duplex of inhoss1,inhoss2
.inhoss2  
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=xchINHdp INfile=",TASKNAME
..          else
.          append    "\\Nins1\Winbatch\butil.exe job=xchINHdp INfile=",TASKNAME
.          endif
.         goto      picdon1c
.inhoss1  
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=xchINH INfile=",TASKNAME
..          else
.          append    "\\Nins1\Winbatch\butil.exe job=xchINH INfile=",TASKNAME
..          endif
.         goto      picdon1c
.picdon1c APPEND    NEWNAME TO TASKNAME
.         APPEND    " F=default C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         RESET     TASKNAME
.         display  *p1:24,*el,"PICDON1A ",file1,"banner ",user,*w
.         EXECUTE   TASKNAME
.         GOTO      PICDON1B
.PICDON2  CMATCH    YES TO ALL
.         GOTO      SAVE IF EQUAL
..         CLOSE     RECMST
.         GOTO      PICDON1
.CLOSEALL WEOF      PAYOUT,SEQ
.         WEOF      RECOUT,SEQ
.         WEOF      EVNOUT,SEQ
.         GOTO      ALLDONE
.PICDON1
.         branch    num of picdn1ok,picdon1b,picdon1b
.picdn1ok COMPARE   "0000" TO COUNT1
.         CALL      CLOSE1 IF NOT EQUAL
.         COMPARE   "0000" TO COUNT1
.         CALL      DEL1 if equal
.         branch    num of picdon1b,picdon1b,picdon1b
.         COMPARE   "0000" TO COUNT2
.         CALL      CLOSE2 IF NOT EQUAL
.         COMPARE   "0000" TO COUNT2
.         CALL      DEL2  if equal
.         branch    num of picdon1b,picdon1b,picdon1b
.         COMPARE   "0000" TO COUNT3
.         CALL      CLOSE3 IF NOT EQUAL
         COMPARE   "0000" TO COUNT3
.         CALL      DEL3 if equal
.         GOTO      PICDON1B
.CLOSE1
..         CLOSE     PAYOUT,EOFSIZE
.         match     "00" to copy
.         return    if equal
.         clear     taskname
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          else
.         append    "\\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          endif
.         APPEND    file1 TO TASKNAME
.         APPEND    " F=default C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         RESET     TASKNAME
.         display  *p1:24,*el,file1,*w4
.         EXECUTE   TASKNAME
.         RETURN
.CLOSE2
..         CLOSE     RECOUT,EOFSIZE
.         match     "00" to copy
.         return    if equal
.         clear     taskname
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          else
.         append    "\\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          endif
.         APPEND    file2 TO TASKNAME
.         APPEND    " F=default C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         RESET     TASKNAME
.         display  *p1:24,*el,file1,*w4
.         EXECUTE   TASKNAME
.         RETURN
.CLOSE3
..         CLOSE     EVNOUT,EOFSIZE
.         match     "00" to copy
.         return    if equal
.         clear     taskname
..          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
..         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          else
.         append    "\\Nins1\Winbatch\butil.exe job=nxchgDP INfile=",TASKNAME
..          endif
.         APPEND    file3 TO TASKNAME
.         APPEND    " F=default C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         RESET     TASKNAME
.         display  *p1:24,*el,file1,*w4
.         EXECUTE   TASKNAME
.         RETURN
.DEL1
.         CLOSE     PAYOUT,DELETE
.         RETURN
.DEL2
.         CLOSE     RECOUT,DELETE
.         RETURN
.DEL3
.         CLOSE     EVNOUT,DELETE
.         RETURN
.PICDON1B clear     taskname
..         append    "\\Nins1\Lanbatch\batch32 -X  -SC -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil job=nxchgeom INfile=",TASKNAME
..         append    "\\Nins1\Lanbatch\batch32 -X -SC -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil job=nxchgDP INfile=",TASKNAME
.         APPEND    NEWNAME TO TASKNAME
.         APPEND    " F=default C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    " B=",TASKNAME
..         APPEND    STR3 TO TASKNAME
.         APPEND    user TO TASKNAME
..begin patch NOv 13 2007
..         APPEND    " PRIN=",TASKNAME
.         APPEND    " PA=",TASKNAME
..end patch NOv 13 2007
.
.         APPEND    c1 TO TASKNAME
.         RESET     TASKNAME
.         EXECUTE   TASKNAME
.         close     account1
..START PATCH 3.14 REPLACED LOGIC
..         open      account1,"ninxnum"
..>Patch 3.18 Code Begin
.         open      account1,"ninxnum.AAM|10.10.30.103:502"
..>Patch 3.18 Code End
..END PATCH 3.14 REPLACED LOGIC
..
.         DISPLAY   *P1:24,*EL,*P10:24,*EL,*+,"HIT (Y) WHEN READY",YES;
.         GOTO      PIC3
.TOTPASS2 MOVE      C2 TO PASS
.         PACK      AKEY2 FROM ARIGHT,TEMPMLR1
.         REP       ZFILL IN AKEY2
.         CLEAR     AKEY1
.         GOTO      PIC3A
..
.PICDEL
.         compare   c2 to br                       .dlh
.         goto      picdon2 if equal               .09/16/94
.         CLOSE     RECMST,DELETE
.         BEEP
.         GOTO      PICDON2
..SAVE     MOVE      PICSW TO NUM
..         BRANCH    NUM OF SAVE1,SAVE2,SAVE3
..SAVE1    MOVE      NEWNAME TO FILE1
.         MOVE      COUNT TO COUNT1
.         MOVE      C0 TO COUNT
.         GOTO      PIC3
.SAVE2    MOVE      NEWNAME TO FILE2
.         MOVE      COUNT TO COUNT2
.         MOVE      C0 TO COUNT
.         GOTO      PIC3
.SAVE3    MOVE      NEWNAME TO FILE3
.         MOVE      COUNT TO COUNT3
.         MOVE      C0 TO COUNT
.         GOTO      PIC3
..
.PIC2     CALL      PAINT
.         CALL      FUNCDISP
.         DISPLAY   *P1:4,*P30:7,"EXCHANGE PICK OFF BY MAILER ##/MAILER ##":
.                   *P30:08,*EL,"---------------------------------------":
.                   *P30:12,"Enter Mailer ##1 ____ ":
.                   *P30:14,"Enter Mailer ##2 ____ (*)To exit";
.         KEYIN     *P46:12,*ZF,*JR,MLR1;
.         CALL      FUNCDISP
.         SCAN      STAR IN MLR1
.         GOTO      PRINT IF EQUAL
.         SCAN      "?" IN MLR1
.         IF        EQUAL
..START PATCH 3.17.5 REPLACED LOGIC
..         CALL      MLRHELP
..         MOVE      MNUM TO MLR1
.                    ALERT     NOTE,"Mailer Help is no longer an option!",result
.                    goto PIC2
..END PATCH 3.17.5 REPLACED LOGIC
.         ENDIF
.         MOVE      "N" TO ALL
.         MOVE      B1 TO DONESW
.         DISPLAY   *P46:12,MLR1;
..START PATCH 3.17.5 REPLACED LOGIC
..         CLEAR     MKEY
..         PACK      MKEY FROM MLR1,Z3
..         CALL      MLRREAD
..         MOVE      MCOMP TO MDES1
.          pack      COMPFLD,MLR1
.          call      MLRREAD
.          move      COMPCOMP,MDES1
..END PATCH 3.17.5 REPLACED LOGIC
.         DISPLAY   *P30:13,*EL,MDES1;
.         MOVE      YES TO str1
.         KEYIN     *P75:12,"OK?",*RV,*uc,str1;
.         CMATCH    YES,str1
.         GOTO      PIC2A IF EQUAL
.         GOTO      PIC2
.PIC2A    KEYIN     *P46:14,*ZF,*JR,MLR2;
..START PATCH 3.17.5 REPLACED LOGIC
..         MATCH     "000*",MLR2
.         MATCH     "00000*",MLR2
..END PATCH 3.17.5 REPLACED LOGIC
.         GOTO      PRINT IF EQUAL
.         SCAN      "?" IN MLR2
.         IF        EQUAL
..START PATCH 3.17.5 REPLACED LOGIC
..         CALL      MLRHELP
..         MOVE      MNUM TO MLR1
.                    ALERT     NOTE,"Mailer Help is no longer an option!",result
.                    goto PIC2A
..END PATCH 3.17.5 REPLACED LOGIC
.         ENDIF
.         DISPLAY   *P46:14,MLR2;
..START PATCH 3.17.5 REPLACED LOGIC
..         CLEAR     MKEY
..         PACK      MKEY FROM MLR2,Z3
..         CALL      MLRREAD
..         DISPLAY   *P30:15,*EL,MCOMP;
.          pack      COMPFLD,MLR2
.          call      MLRREAD
.          DISPLAY   *P30:15,*EL,COMPCOMP;
..END PATCH 3.17.5 REPLACED LOGIC
.         MOVE      YES TO str1
.         KEYIN     *P75:14,"OK?",*RV,*uc,str1;
.         CMATCH    YES,str1
.         GOTO      PIC2B IF EQUAL
.         GOTO      PIC2
..
PIC2B    CLEAR     EXKEY
         PACK      EXKEY FROM MLR1,MLR2,B5
         REP       ZFILL IN EXKEY
         PACK      MATCHKEY FROM MLR1,MLR2
         MOVE      B1,STOPLOOP
         MOVE      "2",PRTSW
         GOTO      NEWFILE
         GOTO      PIC2C
.
PIC2B1
.
         MATCH     YES,STOPLOOP
         GOTO      PICDONE IF EQUAL
         MOVE      YES,STOPLOOP
         CLEAR     EXKEY
         PACK      EXKEY FROM MLR2,MLR1,B5
         REP       ZFILL IN EXKEY
         PACK      MATCHKEY FROM MLR2,MLR1
         GOTO      PIC2C
.
PIC2C
         PACK      ACCKEY FROM EXKEY
         UNPACK    ACCKEY INTO MLR1,MLR2
         PACK      AKEY1 FROM ALEFT,MLR1
         PACK      AKEY2 FROM ARIGHT,MLR2
         REP       ZFILL IN AKEY1
         REP       ZFILL IN AKEY2
         FILEPI    3;ACCOUNT1,XCHNG
         READ      ACCOUNT1,AKEY1,AKEY2;ACCKEY,BLANK08,ENTRY,FLAG
         GOTO      PIC2B1 IF OVER
.begin patch 2.9.1"
         cmatch    "I" to flag
         goto      pic3aa if equal
.end patch 2.9.1"
.START PATCH 3.17.5 REPLACED LOGIC
.         READ      XCHNG,EXKEY;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,EXCOMMNT
         READ      XCHNG,EXKEY;EXKEY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
.END PATCH 3.17.5 REPLACED LOGIC
         if        over
         keyin     *p1:24,"no beg bal cont? ",*uc,str1
         cmatch    yes to str1
         goto      picdone if not equal
         endif                           .dlh 22mar93
.         GOTO      PICDONE IF OVER
         MOVE      "000000",LIST              (IN CASE OF GARBAGE IN FIELD)
         UNPACK    EXKEY INTO ACCKEY,SENTRY
         MOVE      SENTRY TO N8
         COMPARE   N8 TO ENTRY
         GOTO      PIC2D1 IF EQUAL
         GOTO      PICWRT
.
PIC2D
         CMATCH    STAR TO DONESW
         GOTO      PICDONE IF EQUAL
         MOVE      B1 TO DONESW
.START PATCH 3.17.5 REPLACED LOGIC
.         READKS    XCHNG;EXKEY,LR,USAGE1,USAGE2,QTY,LIST:
.                   DAT,STAT,MLRSW,TYPE,EXCOMMNT
         READKS    XCHNG;EXKEY,LR2,LR,USAGE1,USAGE2,QTYFILL,QTY,LIST:
                   DAT,STAT,MLRSW,TYPE,XCHCOMNT
.END PATCH 3.17.5 REPLACED LOGIC
         GOTO      PICDONE IF OVER
. MOVE MAILER#'S FROM EXKEY TO MAILER# FIELDS.
         UNPACK    EXKEY INTO ACCKEY,SENTRY
         MOVE      SENTRY TO N8
         COMPARE   N8 TO ENTRY
         GOTO      PIC2D1 IF EQUAL
         GOTO      PICWRT
.
PIC2D1
         MOVE      STAR TO DONESW
         GOTO      PICWRT
.
PIC3
         DISPLAY   *P1:3,*P30:07,"EXCHANGE PICK OFF BY MAILER":
                   *P30:08,*EL,"-------------------------":
                   *P20:24,*EL,"Enter (*) to exit, (?) for help";

NEW
.         READ      EOMXCHNG,SEQ;TEMPMLR1,B4,STR25,STR3,B1,COPY
         READ      EOMXCHNG,SEQ;TEMPMLR1,B4,STR25,STR3,B1,COPY,User
         GOTO      EXIT IF OVER
          add       c1,rcount
                              if (TEMPMLR1 = "001972")      .Emily's List
                                        move c2 to tranflag
                              else
                                        move c1 to tranflag
                              endif
         MOVE      C0 TO PICSW
         MOVE      PICSW TO NUM
          if        (rcount = 1)
         MOVE      "50" TO FILENUM
          else
          add       c1,filenum
          endif
          call      Newfile
         MOVE      C0 TO COUNT
         MOVE      C0 TO COUNT1
         MOVE      C0 TO COUNT2
         MOVE      C0 TO COUNT3


PIC3DIS
         DISPLAY   *P30:12,"ENTER MAILER## :",TEMPMLR1;
         MATCH     "00000*",TEMPMLR1
         GOTO      PRINT IF EQUAL
         DISPLAY   *P45:12,TEMPMLR1;
.............................
          pack      COMPFLD,TEMPMLR1
          move      "PIC3DIS-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          MOVE      YES TO ANS
          DISPLAY   *P30:13,*EL,COMPCOMP:
                    *P75:12,"OK?",ANS;
        goto       allbal5

MLRREAD
          move      "MLRREAD-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
         RETURN
.
          INCLUDE   COMPio.inc
          INCLUDE   CNTio.inc
         INC       MLRHELP.inc
.
.
NOMAILER
         MOVE      "MAILER DESC. NOT FOUND",COMPCOMP
         RETURN
.
LSTREAD
. PICALL - ALL OPTION.
PICALL
         MOVE      PICSW TO NUM
         ADD       C1 TO NUM
         MOVE      NUM TO PICSW
         COMPARE   c4 TO NUM
         GOTO      ALLDONE IF EQUAL
. ALLBAL CHECK IF BALANCES UNDER 1000 ARE TO BE PRINTED.
ALLBAL
         MOVE      C0 TO NUM        *ZERO OUT NUM.
         MOVE      PICSW TO NUM
         BRANCH    NUM OF ALLBAL1,ALLBAL2,ALLBAL3,ALLBAL4A,allbal4a
         GOTO      PIC3               *INVALID CHOICE
ALLBAL1  DISPLAY   *P1:14,*EF,*P30:15,"YOU SELECTED ",*HON,"PAYABLES",*HOFF;
         CALL      FUNCDISP
         GOTO      ALLBAL4
ALLBAL2  DISPLAY   *P1:14,*EF,*P30:15,"YOU SELECTED ",*HON,"RECEIVABLES",*HOFF;
         CALL      FUNCDISP
         GOTO      ALLBAL4
ALLBAL3  DISPLAY  *P1:14,*EF,*P30:15,"YOU SELECTED ",*HON,"EVEN BALANCES",*HOFF
         CALL      FUNCDISP
         GOTO      NEWFILE
ALLBAL4A MOVE      YES TO ALL
         MOVE      C1 TO PICSW
         GOTO      ALLBAL
. IF PICKING OFF ONLY EVEN ACCOUNTS DON'T CHECK FOR FOLLOWING.
ALLBAL4
         CMATCH    YES TO ALL
         GOTO      ALLBAL4B IF EQUAL
         KEYIN     *P20:20,*EL,"PRINT BALANCES LESS THAN 1000 ? ",BAL;
         CMATCH    YES,BAL
         GOTO      ALLBAL IF EOS
         GOTO      NEWFILE IF EQUAL
         CMATCH    NO,BAL
         GOTO      NEWFILE IF EQUAL
         GOTO      ALLBAL4
ALLBAL4B DISPLAY   *P20:20,*EL,"PRINT BALANCES LESS THAN 1000 ? Y";
         DISPLAY   *P30:15,"YOU SELECTED ",*HON,"ALL REPORTS",*HOFF,*EL
         GOTO      NEWFILE
.
ALLBAL5  DISPLAY   *P30:15,"YOU SELECTED ",*HON,"EVERYTHING COMBINED",*HOFF
         MOVE      C4 TO PICSW
         MOVE      PICSW TO NUM
         MOVE      YES TO ALL
         CALL      FUNCDISP
.
.PIC3W - TOTALS REPORT WRITE.
PIC3W
pic3wa   
.         ENDIF
pic3wb
              WRITE RECMST,SEQ;TEMPMLR1,"H   ",PICSW,univflag,TRANFLAG,NUsedflag:
                    str35,str35,str35,str33

         GOTO      PICWA
.

PICWA    PACK      AKEY1 FROM ALEFT,TEMPMLR1
         CLEAR     AKEY2
         REP       ZFILL IN AKEY1
         MOVE      C1 TO PASS
         GOTO      PIC3A
.
PIC3A    FILEPI    1;ACCOUNT1
         READ      ACCOUNT1,AKEY1,AKEY2;ACCKEY,BLANK08,ENTRY,FLAG
.omg so wrong need to change the keys   DLH  2015Nov30
         IF           Over
                      if         (akey2 = "")
                      pack       akey2 from ARight,TEMPMLR1
                      REP       ZFILL IN AKEY2
                      clear      Akey1
                      goto       Pic3a
                      else
                      goto       Picdone
                      endif
         endif             
.         GOTO      PICDONE IF OVER
. MOVE MAILER#'S TO WORK FIELDS.
         UNPACK    ACCKEY INTO MLR1,MLR2
.
.begin patch 2.9.1"
         cmatch    "I" to flag
         goto      pic3aa if equal
.end patch 2.9.1"
         BRANCH    PASS OF PIC3B,PIC3C
         GOTO      PIC3C
.
PIC3AA   FILEPI    1;ACCOUNT1
         READKG    ACCOUNT1;ACCKEY,BLANK08,ENTRY,FLAG
.omg so wrong need to change the keys   DLH  2015Nov30
         IF           Over
                      if         (akey2 = "")
                      pack       akey2 from ARight,TEMPMLR1
                      REP       ZFILL IN AKEY2
                      clear      Akey1
                      goto       Pic3a
                      else
                      goto       Picdone
                      endif
         endif             
.begin patch 2.9.1"
         cmatch    "I" to flag
         goto      pic3aa if equal
.end patch 2.9.1"
         UNPACK    ACCKEY INTO MLR1,MLR2
         DISPLAY   *P1:12,*EL,MLR1,MLR2
         BRANCH    PASS OF PIC3B,PIC3C
         GOTO      PIC3C
.
PIC3B    CLEAR     EXKEY
         PACK      EXKEY FROM ACCKEY,ENTRY
         REP       ZFILL,EXKEY
.
         MOVE      "EXCHANGE RECORD",FMESG
         FILEPI    1;XCHNG
         READ      XCHNG,EXKEY;EXKEY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         GOTO      PIC3ERR IF OVER
         GOTO      GETMDESC
.
PIC3C    CLEAR     EXKEY
         PACK      EXKEY FROM ACCKEY,ENTRY
         REP       ZFILL,EXKEY
.
         MOVE      "EXCHANGE RECORD",FMESG
         FILEPI    1;XCHNG
         READ      XCHNG,EXKEY;EXKEY,LR2,LR,USAGE2,USAGE1:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         GOTO      PIC3ERR IF OVER
.
         UNPACK    EXKEY INTO MLR2,MLR1,SENTRY
         PACK      EXKEY FROM MLR1,MLR2,SENTRY
.
         GOTO      GETMDESC
.
.  MAILER AND USAGE FIELDS WERE REVERSED HERE SO ALL RECORD WILL BE WRITTEN IN
.  AN EASILY SORTABLE FORMAT FOR PRINTING.
.
GETMDESC
         MATCH     TEMPMLR1,MLR1
         GOTO      GETM2 IF EQUAL
         MATCH     TEMPMLR1,MLR2
         GOTO      GETM1 IF EQUAL
.
.START PATCH 3.17.5 REPLACED LOGIC
.GETM2
.         CLEAR     MKEY
.         PACK      MKEY FROM MLR2,Z3
.         GOTO      GETMREAD
.GETM1
.         CLEAR     MKEY
.         PACK      MKEY FROM MLR1,Z3
.         GOTO      GETMREAD
.GETMREAD
.         MOVE      NO TO OVER
..         CALL      NMLR1
.         CALL      NMLRKEY
.........................
GETM2
         CLEAR     COMPFLD
         PACK      COMPFLD FROM MLR2
         GOTO      GETMREAD
GETM1
         CLEAR     COMPFLD
         PACK      COMPFLD FROM MLR1
         GOTO      GETMREAD
GETMREAD
          MOVE      NO,OVER
          move      "GETMREAD-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          CALL      COMPKEY
.END PATCH 3.17.5 REPLACED LOGIC
         CMATCH    YES TO OVER
         CALL      PICMLRER IF EQUAL
         MOVE      C0 TO NUM          *ZERO OUT NUM.
.
PICPAY   MATCH     TEMPMLR1,MLR1
         GOTO      PICPAY1 IF EQUAL
         GOTO      PICPAY2
.
PICPAY1  COMPARE   USAGE1 TO USAGE2
         GOTO      PICREC IF EQUAL
         GOTO      PAY IF LESS
         GOTO      PICREC
.
PICPAY2  COMPARE   USAGE2 TO USAGE1
         GOTO      PICREC IF EQUAL
         GOTO      PAY IF LESS
         GOTO      PICREC
PAY      MOVE      C1 TO NUM
         GOTO      PICWRT
.
PICREC   MATCH     TEMPMLR1,MLR1
         GOTO      PICREC1 IF EQUAL
         GOTO      PICREC2
.
PICREC1  COMPARE   USAGE2 TO USAGE1
         GOTO      PICEVEN IF EQUAL
         GOTO      REC IF LESS
         GOTO      PICEVEN
.
PICREC2  COMPARE   USAGE1 TO USAGE2
         GOTO      PICEVEN IF EQUAL
         GOTO      REC IF LESS
         GOTO      PICEVEN
REC      MOVE      "2" TO NUM
         GOTO      PICWRT
.
PICEVEN
         MOVE      "3" TO NUM
         COMPARE   USAGE1 TO USAGE2
         GOTO      PICWRT IF EQUAL
         GOTO      PIC3AA
.
. CHKBAL IS "BAL" SET TO "N" IF SO COMPARE BALANCE TO 1000 IF LESS DON'T PRINT
CHKBAL
         CMATCH    NO,BAL
         GOTO      CHKBAL1 IF EQUAL
         GOTO      PICWRT
CHKBAL1
.START PATCH 3.1 - REPLACED LOGIC
.         MOVE      USAGE1 TO WORK09A
.         MOVE      USAGE2 TO WORK09B
.         SUB       WORK09B FROM WORK09A
.         SUB       "1000" FROM WORK09A
         MOVE      USAGE1 TO WORK10A
         MOVE      USAGE2 TO WORK10B
         SUB       WORK10B FROM WORK10A
         SUB       "1000" FROM WORK10A
.END PATCH 3.1 - REPLACED LOGIC
         GOTO       PICWRT IF ZERO
         GOTO      PIC3AA IF LESS
         GOTO      PICWRT
CHKBAL2
         CMATCH    NO,BAL
         GOTO      CHKBAL3 IF EQUAL
         GOTO      PICWRT
CHKBAL3
.START PATCH 3.1 - REPLACED LOGIC
.         MOVE      USAGE1 TO WORK09A
.         MOVE      USAGE2 TO WORK09B
.         SUB       WORK09A FROM WORK09B
.         SUB       "1000" FROM WORK09B
         MOVE      USAGE1 TO WORK10A
         MOVE      USAGE2 TO WORK10B
         SUB       WORK10A FROM WORK10B
         SUB       "1000" FROM WORK10B
.END PATCH 3.1 - REPLACED LOGIC
         GOTO       PICWRT IF ZERO
         GOTO      PIC3AA IF LESS
         GOTO      PICWRT
.
PICMLRER
.START PATCH 3.17.5 REPLACED LOGIC
.         MOVE      "MAILER DESC. NOT FOUND ",MCOMP
         MOVE      "MAILER DESC. NOT FOUND ",COMPCOMP
.END PATCH 3.17.5 REPLACED LOGIC
         RETURN
.
PIC3ERR
         DISPLAY   *P01:22,*R,*EL,"RECORD NUMBER ",EXKEY," COULD NOT BE ":
                   "FOUND I WILL CONTINUE WITH THE NEXT RECORD",*B,*B;
         PAUSE     "4"
         GOTO      PIC3AA
.
NEWFILE  
         
.
FILENAME CLEAR     NEWNAME
         APPEND    "EXSTAT",NEWNAME
         MOVE      FILENUM,F2
         REP       ZFILL,F2
         APPEND    F2,NEWNAME
         RESET     NEWNAME
         call      trim using newname
         clear     recname
         APPEND    NTWKPATH1 TO recname
         append    newname to recname
         reset     recname
         TRAP      GOODFILE GIVING ERROR IF IO
         OPEN      RECMST,recNAME
         CLOSE     RECMST
ADDFILE  ADD       "1" TO FILENUM
         GOTO      FILENAME
.
GOODFILE TRAPCLR   IO
         SCAN      "0030-0031" IN ERROR
.         DISPLAY    *P1:24,*EL,ERROR,*W4
         GOTO      ADDFILE IF EQUAL
         RESET     ERROR
         SCAN      "I * Y" IN ERROR         .fixed 3/22/93 dlh had I*Y
         GOTO      ADDFILE IF EQUAL
         reset     error
         SCAN      "I10" IN ERROR            .plb
         GOTO      ADDFILE IF EQUAL
         TRAP      START IF F5
         MOVE      B1 TO ERROR
         PREPARE   RECMST,RECNAME,exclusive
         DISPLAY   *P1:22,*EL,"The output file name for this search is: ":
                   NEWNAME,*P1:24,*EL;
          Noreturn                      .pop stack
          return                        .get out



         BRANCH    BR OF PIC2C,ALLYES,PIC3W,pic3w,pic3w
         MOVE      "PICSW" TO FMESG
         GOTO      ERROR
ALLYES
          CMATCH    YES TO ALL
.
TOTFILES BRANCH    FNUM OF PAYOUT,RECOUT,EVNOUT,PIC3W,pic3w
.
PAYOUT  ADD       C1 TO FNUM
         CLOSE     RECMST
         OPEN      PAYOUT,RECNAME,EXCLUSIVE
         MOVE      NEWNAME TO FILE1
         branch    num of pic3w,pic3w,pic3w,recout        .dh 09/16/94
         GOTO      ADDFILE
RECOUT  ADD       C1 TO FNUM
         CLOSE     RECMST
         OPEN      RECOUT,RECNAME,EXCLUSIVE
         MOVE      NEWNAME TO FILE2
         branch    num of pic3w,pic3w,pic3w,recout        .dh 09/16/94
         GOTO      ADDFILE
EVNOUT  ADD       C1 TO FNUM
         CLOSE     RECMST
         OPEN      EVNOUT,RECNAME,EXCLUSIVE
         MOVE      NEWNAME TO FILE3
         GOTO      PIC3W
...............................................................................
NOFILE   DISPLAY   *B,*P01:23,*EL,NOFILE," FILE IS NOT ON LINE NOTIFY YOUR ":
                   "PROGRAMER !!!!!",*W,*B,*W,*B,*W5;
         STOP
...............................................................................
.NOTES FROM PATCH 3.17.5 - THIS ROUTINE IS NOT CALLED!!!!, so I am commenting it out!
.NOMLR
.         DISPLAY   *B,*P01:24,*EL,"I CAN'T FIND A MLR DESCRIPTION FOR ":
.                   "MLR ## ",MKEY,*W,*B,*W,*B;
.         DISPLAY   *P01:24,*EL,"NOTIFY YOUR PROGRAMER !!!!!!",*W,*B,*B;
.         GOTO      START
. .............................................................................
.
EXIT
.         CLOSE     LRXNG
         CLOSE     XCHNG
         STOP
ERROR    DISPLAY   *P03:24,*EL,FMESG,"NOT ON LINE PLEASE INFORM COMPUTER ":
                   "PERSONEL";
         KEYIN     *P30:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      ERROR
.
INT
         TRAPCLR   INT
         NORETURN
         TRAP      INT IF INT
         TRAPCLR   F1
INT1
         DISPLAY   *P1:1,*ES,*B,*B,*B,*B,*HON:
                   *P8:4,"**********************************************":
                   *P8:5,"*   INT. ERROR CALL COMPUTER PERSONNEL.":    *
                   *P8:6,"**********************************************",*HOFF
         PAUSE     "1"
         BEEP
.         CONSOLE   *P14:1,"INT. ERROR"
         PAUSE     "1"
         BEEP
         KEYIN     *P30:24,*EOFF,str1;
         CMATCH    "I",str1
         GOTO      EXIT IF EQUAL
.         CONSOLE   *P14:1,"          "
         GOTO      INT1
ABORT
.
         TRAPCLR   F1
         NORETURN
         TRAP      ABORT IF F1
         TRAP      EXIT IF IO
         DISPLAY   *P1:24,*EL,"YOUR FILE(s) IS BEING DELETED!!!!",*B,*W3;
.         CONSOLE   *P14:1,"F1 DELETE"
.         PREPARE   RECMST,NEWNAME
         CMATCH    YES TO ALL
         GOTO      ABORT1 IF EQUAL
         CLOSE     RECMST,DELETE
         GOTO      EXIT
ABORT1   CLOSE     PAYOUT,DELETE
         CLOSE     RECOUT,DELETE
         CLOSE     EVNOUT,DELETE
         GOTO      EXIT
         listoff
         INCLUDE   NDATIO.inc
         listoff
         INCLUDE   COMLOGIC.inc


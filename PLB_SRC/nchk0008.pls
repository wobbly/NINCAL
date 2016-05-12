.PURPOSE - PRINT Transfer RECONCILIATION STATEMENT AFTER EACH CHECK RUN.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INC       CONS.inc
         inc       hp.inc
.
Release   init     "1.1"               DLH should not be anymore suppress if Zero records
Reldate   Init      "09 July 2010"
.Release   init     "1.0"               DLH New to track intercompany transfers
.Reldate   Init      "25 February 2009"
.
* *****************************************************************************
* NAMES IN THE NEWS MASTER Transfer FILE.
* *****************************************************************************
.
.    FILE:      Transfer
.  LENGTH:
...............................................................................
.
Transfer    FILE      
.
RCMLR    DIM       4       1-4          MAILER #
RCCNT    DIM       3       5-7          CONTACT #
RCLRNO   DIM       6       8-13         LR #
RCCHEK   DIM       6      14-19         ICHECK NUMBER (no physical check)
RCLONO   DIM       4      20-23         LIST OWNER NUMBER
RCACREC  FORM      10.2   24-36         ACCOUNTS RECEIVABLE
RCAPAY   FORM      10.2   34-49         ACCOUNTS PAYABLE
RCNNINC  FORM      9.2    44-61         NIN INCOME
RCLRINC  FORM      9.2    53-73         LR INCOME
RCSTAX   FORM      9.2    64-85         STATE TAX
RCCTAX   FORM      9.2    74-97         CITY TAX
RCPOST   FORM      3.2    84-103         POSTAGE
RCCNTLO  FORM      3      90-106         CONTROL NUMBER
RCDATE   DIM       6      92-112         CHECK DATE
rpayee   dim       25     99-137           payee
RCOmpId   Dim       1
RXNinc    FOrm      9.2            Xninc

. 
. 
TWO5     INIT      "                         "
TWO9     INIT      "                            "
DATE     DIM       6
SYSDATE  DIM       8
DETDATE  DIM       8
FORTY5  FORM      "45"
FOUR     FORM      "4"
ONETHOUS FORM      "1000"
DASH5    INIT      "-----"
DASH6    INIT      "------"
DASH7    INIT      "-------"
DASH8    INIT      "--------"
DASH9    INIT      "---------"
PAGECNTR FORM      "00"
LINECNTR FORM      2
PAGENUM  FORM      2
HEADATE  DIM       8
PAYMASK  DIM       18
TOTMASK  DIM       14
CHECKOUT DIM       6
RCPAYOUT FORM      9.2
DATEOUT  DIM       8
RLCRNOUT DIM       6
RCNTLOUT FORM      3
HEADISP  INIT      "NIN Transfer LOG  PROGRAM"
TOTALAP  FORM      9.2
PRTFLAG  DIM       1
LOCAL    INIT      "LOCAL"
rfile    dim       55
chkcnt   form      3
holdcheck dim      6
count     form      5
. 
+..............................................................................
          IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO DETDATE
         EDIT      DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
         CLOCK     DATE TO DETDATE
         MOVE      DETDATE TO TODAY
         UNPACK    DETDATE INTO MM,STR1,DD,STR1,YY
         XIF

         MOVE      "NCHK0004" TO PROGRAM
         MOVE      "Transfer RECONN PROGRAM" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         MOVE      "TRNSRECON" TO INPNAME
         CALL      PAINT
           MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
         reset     comment
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      Transfer,INPNAME
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET

. 
START
          Move      c0,count
.
LOOPER  READ      Transfer,SEQ;RCMLR,RCCNT:
                   RCLRNO:
                   RCCHEK:
                   RCLONO:
                   RCACREC:
                   RCAPAY:
                   RCNNINC:
                   RCLRINC:
                   RCSTAX:
                   RCCTAX:
                   RCPOST:
                   RCCNTLO:
                   RCDATE:
                   rpayee:
                 RCOmpId:
                 RXNinc
.
         GOTO      DONE IF OVER
          add       c1,count
         GOTO      PROCESS
. 
. 
. 
*   REPORT HEADINGS   *
. 
. 
. 
HEADINGS MOVE      C0 TO LINECNTR
         ADD       C1 TO PAGENUM
         compare    c1 to pagenum
         if         equal
;         print      hp17ptch,hptop,hpdupl,*f
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
          If        (Company = c2)
                    PRINT               *F,*n,*4,"CONFIDENTIAL",TWO9,"* * * PLI   Transfer   LOG":
                    *69,"  REPORT   * * * ",TWO5,"DATE ",TODAY
          else
                    PRINT               *F,*n,*4,"CONFIDENTIAL",TWO9,"* * * NIN   Transfer   LOG":
                    *69,"  REPORT   * * * ",TWO5,"DATE ",TODAY
          endif
         PRINT     *112,"PAGE ",PAGENUM
         PRINT     *N,*23,"ICHECK ##        DATE               $ A/P":
                   *75,"LR ##     CONTROL ##":
                   *L,*23,DASH6:
                   *38,DASH5:
                   *56,DASH8:
                   *75,DASH5:
                   *84,DASH9
         PRINT     *N,*FLUSH
         RETURN
. 
. 
. 
PROCESS

          if        (count = c1)
         CALL      HEADINGS
          endif

         UNPACK    RCDATE INTO MM,DD,YY
         REP       ZFILL,MM
         REP       ZFILL,DD
         REP       ZFILL,YY
         PACK      DETDATE FROM MM,SLASH,DD,SLASH,YY
         TYPE      RCCHEK
         GOTO      LOOPER IF NOT EQUAL
         add       c1 to chkcnt                 .number of checks    DLH 11June98
         MOVE      RCCHEK TO CHECKOUT
         MOVE      DETDATE TO DATEOUT
         MOVE      RCLRNO TO RLCRNOUT
         MOVE      RCCNTLO TO RCNTLOUT
         MOVE      "$,$$$,$$$,$$9.99-" TO PAYMASK
          if        (RCApay >= c0)
         ADD       RCAPAY TO TOTALAP
          endif
         EDIT      RCAPAY TO PAYMASK
         ADD       C1 TO LINECNTR
         match     RCChek to holdcheck
         if        equal
         sub       c1 from chkcnt                .record in file twice adjust count
         sub       rcapay from totalap           .record in file twice adjust AP total
         
         PRINT     Hpbon,*1,"Duplicate **":
                   *23,CHECKOUT:
                   *37,DATEOUT:
                   *53,PAYMASK:
                   *75,RLCRNOUT,"     "," ",RCNTLOUT,hpboff                 
         else
         move      RCChek to holdcheck
         PRINT     *23,CHECKOUT:
                   *37,DATEOUT:
                   *53,PAYMASK:
                   *75,RLCRNOUT,"     "," ",RCNTLOUT
         endif          
         COMPARE   LINECNTR TO FORTY5
         CALL      HEADINGS IF EQUAL
         GOTO      LOOPER
DONE      
          if        (count > c0)
         MOVE      "$$$,$$$,$$9.99" TO TOTMASK
         EDIT      TOTALAP TO TOTMASK
         PRINT     *L,*L,*23,chkcnt,b1,"Checks",*45,"TOTAL :",TOTMASK
         PRINT     hpreset
          endif
         GOTO      EOJ
. 
         INCLUDE   COMLOGIC.inc
. 
. 
EOJ

         CLOSE     transfer
         DISPLAY   *P1:1,*ES,*P10:12,"JOB DONE!!!!!!",*W2
         shutdown  "cls"
         STOP
. 
. 
ABORT      BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED VIA F4 KEY, OH NO!!!!",*W3
         GOTO      EOJ
         
IO       TRAPCLR   IO 
         NORETURN
         DISPLAY   *P12:15,"ERROR IS ",ERROR,*W5
         shutdown  "cls"
         STOP
. 
. 
